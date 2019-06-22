(cl:in-package #:huginn.machine.representation)


(define-constant +placeholder-array+
    (make-array 0 :element-type 'cell)
  :test 'vector=
  :documentation "Just an empty array of correct type, used as initial binding for CLAUSE slot.")


(deftype vector-representation ()
  `(simple-array cell (*)))


(deftype pointer ()
  'non-negative-fixnum)


(defstruct clause
  (body-to-head-mapping +placeholder-array+ :type vector-representation)
  (variable-values +placeholder-array+ :type simple-vector)
  (content +placeholder-array+ :type vector-representation)
  (body-pointer 0 :type fixnum))


(defstruct execution-state
  (clauses)
  ;; all clauses that can be selected for unfolding
  (objects-index 0 :type fixnum)
  ;; index of next mapped object
  (objects-mapping (make-hash-table :test 'eq) :type hash-table)
  ;; translates instance into unique index
  (variable-bindings (make-array 64) :type (simple-array t (*)))
  ;; reverse mapping, maps unique index to instance
  (heap +placeholder-array+ :type vector-representation)
  ;; heap, stores both data and code. Fresh clauses are appended at the back. It is a simple array, and fill-pointer is stored in the execution-stack-cell.
  (unification-stack (make-array 16 :element-type 'cl-ds.utils:index
                                    :adjustable t :fill-pointer 0)
   :type (cl-ds.utils:extendable-vector non-negative-fixnum))
  ;; shared unification stack. needs to be resetted before using.
  )


(defstruct execution-stack-cell
  (clause nil :type (or null clause))
  ;; clause used to construct this execution-stack-cell
  (goals '() :type list)
  ;; list of goals (pointers to the heap) for this cell. During unfolding top goal in this cell is selected for proving. If goal can't be proved stack cell will be popped. Therefore this stack represents already proven goals.
  (heap-pointer 0 :type fixnum)
  ;; location of representation of this cell on the heap
  (variable-bindings-trail '() :type (vector fixnum))
  ;; undo trail for variable bindings. This only contains variables not established in this stack cell
  (heap-cells-trail '() :type (vector fixnum))
  ;; undo trail for heap-cells. each cons cell in the list contains pair address.value
  (bindings-fill-pointer 0 :type cl-ds.utils:index)
  ;; bindings fill pointer (in the execution-state)
  (heap-fill-pointer 0 :type cl-ds.utils:index)
  ;; heap fill pointer (actual heap is represented as a simple-array
  (previous-cell nil :type (or null execution-stack-cell))
  ;; well, it is stack, what did you expect?
  )


(define-constant unbound 'unbound)


(defun unwind-heap-cells-trail (execution-state trail)
  (declare (type vector trail)
           (type execution-state execution-state))
  (let ((heap (execution-state-heap execution-state)))
    (iterate
      (declare (type fixnum address i)
               (type cell old-value))
      (for i from 0 by 2 below (length trail))
      (for address = (aref trail i))
      (for old-value = (aref trail (1+ i)))
      (setf (aref heap address) old-value))))


(defun unwind-variable-bindings-trail (execution-state trail)
  (declare (type vector trail)
           (type execution-state execution-state))
  (let ((bindings (execution-state-variable-bindings execution-state)))
    (map nil
         (lambda (elt)
           (setf (aref bindings elt) unbound))
         trail)))


(defun unbind-range (execution-state from below)
  (declare (type execution-state execution-state)
           (cl-ds.utils:index from below))
  (iterate
    (declare (type fixnum i))
    (with bindings = (execution-state-variable-bindings execution-state))
    (for i from from below below)
    (setf (aref bindings i) unbound)))


(defun pop-stack-cell (execution-state execution-stack-cell)
  "Pop top stack-cell, adjust execution-state by clearing variables."
  (declare (type execution-stack-cell execution-stack-cell)
           (type execution-state execution-state))
  (bind ((result (execution-stack-cell-previous-cell execution-stack-cell))
         (no-prev-cell (null result))
         (previous-bindings-fill-pointer
          (if no-prev-cell
              0
              (execution-stack-cell-heap-fill-pointer result)))
         (bindings-fill-pointer (execution-stack-cell-heap-fill-pointer
                                 execution-stack-cell)))
    (unbind-range execution-state
                  previous-bindings-fill-pointer
                  bindings-fill-pointer)
    (unwind-variable-bindings-trail
     execution-state
     (execution-stack-cell-variable-bindings-trail execution-stack-cell))
    (unwind-heap-cells-trail
     execution-state
     (execution-stack-cell-heap-cells-trail execution-stack-cell))
    result))


(-> clause-content-length (clause) cl-ds.utils:index)
(defun clause-content-length (clause)
  (declare (optimize (speed 3)))
  (~> clause clause-content length))


(defun clause-goals (clause pointer-offset &optional (initial-list '()))
  cl-ds.utils:todo)


(-> clause-body-length (clause) cl-ds.utils:index)
(defun clause-body-length (clause)
  (declare (optimize (speed 3)))
  (- (clause-content-length clause) (clause-body-pointer clause)))


;; this should be performed only after clause was already proven (and therefore copied to heap.
;; Code assumes that next stack cell is located DIRECTLY after the current one on the heap
(defun push-stack-cell (execution-stack-cell clause)
  "Constructs new stack-cell based on the clause, trail and execution-stack-cell assuming that this cell is constructed from the first goal of the execution-stack-cell after data was already placed to the heap."
  (declare (type clause clause))
  (let* ((fill-pointer (execution-stack-cell-heap-fill-pointer
                        execution-stack-cell))
         (new-fill-pointer (+ fill-pointer (clause-body-length clause))))
    (declare (type cl-ds.utils:index new-fill-pointer fill-pointer))
    (make-execution-stack-cell
     :previous-cell execution-stack-cell
     :heap-fill-pointer new-fill-pointer
     :goals (~>> execution-stack-cell
                 execution-stack-cell-goals
                 rest
                 (clause-goals clause fill-pointer))
     :clause clause
     :heap-pointer fill-pointer
     :bindings-fill-pointer (~> execution-stack-cell
                                execution-stack-cell-bindings-fill-pointer
                                (+ (~> clause
                                       clause-variable-values
                                       length))))))


(defun expand-state-heap (state desired-size)
  (declare (type execution-state state)
           (type fixnum desired-size)
           (optimize (speed 1) (safety 2)))
  (let* ((old-heap (execution-state-heap state))
         (old-heap-size (length old-heap))
         (new-heap-size
           (max old-heap-size
                (the fixnum (* 128 (1+ (truncate desired-size 64)))))))
    (declare (type fixnum old-heap-size new-heap-size))
    (unless (= old-heap-size new-heap-size)
      (let ((new-heap (make-heap new-heap-size)))
        (setf (execution-state-heap state) new-heap)
        (iterate
          (declare (type fixnum i))
          (for i from 0 below old-heap-size)
          (setf (aref new-heap i) (aref old-heap i)))))
    state))


;; (declaim (inline index-object))
(defun index-object (execution-state object)
  (declare (type execution-state execution-state))
  (bind ((object-index (execution-state-objects-index execution-state))
         (objects-mapping (execution-state-objects-mapping execution-state))
         (bindings (execution-state-variable-bindings execution-state))
         (lookup-result (ensure (gethash object objects-mapping) object-index)))
    (declare (type fixnum lookup-result)
             (optimize (speed 3)))
    (unless (eql lookup-result object-index)
      (return-from index-object  (values lookup-result nil)))
    (incf (execution-state-objects-index execution-state))
    (let ((bindings-length (length bindings)))
      (unless (< object-index bindings-length)
        (iterate
          (declare (type fixnum i))
          (with new-bindings = (make-array (ash (1+ bindings-length) 1)))
          (for i from 0 below bindings-length)
          (setf (aref new-bindings i) (aref bindings i))
          (finally
           (setf (execution-state-variable-bindings execution-state) new-bindings
                 bindings new-bindings))))
      (setf (aref bindings object-index) object)
      (values lookup-result t))))


(declaim (inline dereference-variable))
(defun dereference-variable (state cell)
  (declare (type execution-state state)
           (type cell cell)
           (optimize (speed 3)))
  (let ((word (detag cell)))
    (assert (> word 0))
    (aref (execution-state-variable-bindings state) (1- (detag cell)))))


(declaim (inline (setf dereference-variable)))
(defun (setf dereference-variable) (new-value state cell)
  (declare (type execution-state state)
           (type cell cell)
           (optimize (speed 3)))
  (setf (aref (execution-state-variable-bindings state) (detag cell))
        new-value))


(declaim (inline value-bound-p))
(defun value-bound-p (object)
  (not (value-unbound-p object)))

(declaim (inline value-unbound-p))
(defun value-unbound-p (object)
  (eq unbound object))


;; this can benefit from automaticly generated and compiled function for each clause, should be a little bit faster.
;; (declaim (inline clause-body-to-heap))
(defun clause-body-to-heap (execution-state execution-stack-cell clause)
  "Copies clause body to heap. Will extend variable bindings in the state (or fail and return nil if can't do so). Will return: new trail, new bindings-heap-pointer, and success-info. To unroll changes do the execution-state performed by this function it is required to both unwind-variable-bindings-trail and unbind-range"
  (declare (type execution-state execution-state)
           (type execution-stack-cell execution-stack-cell)
           (type clause clause))
  (let* ((body-length (clause-body-length clause))
         (content (clause-content clause))
         (body-pointer (clause-body-pointer clause))
         (variable-values (clause-variable-values clause))
         (fill-pointer
           (execution-stack-cell-heap-fill-pointer execution-stack-cell))
         (new-fill-pointer (+ fill-pointer body-length)))
    (declare (type fixnum new-fill-pointer))
    (expand-state-heap execution-state new-fill-pointer)
    (let* ((heap (execution-state-heap execution-state))
           (bindings-fill-pointer (execution-stack-cell-bindings-fill-pointer
                                   execution-stack-cell)))
      (iterate
        (declare (type fixnum i j z))
        (declare (type fixnum i j))
        (for i from fill-pointer below new-fill-pointer)
        (for j from body-pointer)
        (for z from 0)
        (for cell = (aref content j))
        (setf (aref heap i) cell)
        (for word = (detag cell))
        (tag-case (cell)
          :expression
          (setf (aref heap i) (tag +expression+ i))
          :reference
          (incf (aref heap i) fill-pointer)
          :variable
          (unless (zerop word)
            (bind ((object (aref variable-values word)))
              (assert (value-bound-p object))
              (bind (((:values index new) (index-object execution-state
                                                        object)))
                (setf (aref heap i) (tag +variable+ index))
                (when new
                  (incf bindings-fill-pointer)))))))
      (values bindings-fill-pointer t))))


(defun make-heap (size)
  (make-array size
              :element-type 'cell
              :initial-element 0))


(-> execution-state-heap-size (execution-state) fixnum)
(defun execution-state-heap-size (state)
  (declare (type execution-state state)
           (optimize (speed 3)))
  (~> state execution-state-heap length))


(-> clause-length (clause) fixnum)
(defun clause-length (clause)
  (declare (optimize (speed 3)))
  (~> clause clause-content length))


(defun clear-vector (vector)
  (setf (fill-pointer vector) 0))


(-> follow-reference (execution-state pointer &optional boolean) cell)
(defun follow-reference (execution-state pointer &optional recursive)
  (declare (type execution-state execution-state)
           (type pointer pointer)
           (optimize (speed 3)))
  (iterate
    (declare (type pointer prev-pointer))
    (with prev-pointer = pointer)
    (for heap-cell = (~> execution-state execution-state-heap (aref pointer)))
    (while (and recursive (reference-cell-p heap-cell)))
    (shiftf prev-pointer pointer (detag heap-cell))
    (finally (return prev-pointer))))


(-> dereference-heap-pointer (execution-state pointer &optional boolean) cell)
(defun dereference-heap-pointer (execution-state pointer &optional follow-references)
  (declare (type execution-state execution-state)
           (type pointer pointer)
           (optimize (speed 3)))
  (~> execution-state execution-state-heap
      (aref (follow-reference execution-state pointer follow-references))))


(defmacro with-unification-stack ((execution-state )
                                  &body body)
  (with-gensyms (!ustack !fill-pointer !state !start !block)
    `(let* ((,!state ,execution-state)
            (,!ustack (execution-state-unification-stack ,!state)))
       (declare (ignorable ,!ustack))
       (assert (adjustable-array-p ,!ustack))
       (assert (array-has-fill-pointer-p ,!ustack))
       (macrolet ((upush (stack-pointer goal-pointer)
                    `(progn
                       (vector-push-extend ,stack-pointer ,',!ustack)
                       (vector-push-extend ,goal-pointer ,',!ustack)))
                  (upop ((stack-pointer goal-pointer
                          &optional stack-cell goal-cell)
                         &body this-body)
                    `(let* ((,',!fill-pointer (fill-pointer ,',!ustack))
                            (,goal-pointer
                              (aref ,',!ustack
                                    (the fixnum (1- ,',!fill-pointer))))
                            (,stack-pointer
                              (aref ,',!ustack
                                    (- ,',!fill-pointer 2))))
                       (decf (fill-pointer ,',!ustack) 2)
                       ,@this-body))
                  (uclear ()
                    `(clear-vector ,',!ustack))
                  (uemptyp ()
                    `(zerop (fill-pointer ,',!ustack)))
                  (deref (pointer)
                    `(dereference-heap-pointer ,',!state
                                               ,pointer))
                  (next ()
                    `(go ,',!start))
                  (done (result)
                    `(return-from ,',!block ,result)))
         (block ,!block
           (tagbody
              ,!start
              ,@body))))))


(defun prepare-unification-stack (execution-state stack-cell goal-pointer)
  (declare (type execution-stack-cell stack-cell)
           (type execution-state execution-state)
           (type pointer goal-pointer))
  (with-unification-stack (execution-state)
    (uclear)
    (upush (execution-stack-cell-heap-pointer stack-cell)
           goal-pointer)))


(declaim (notinline select-clause))
(defun select-clause (predicate clauses)
  (declare (type list clauses))
  (iterate
    (for sub on clauses)
    (for first = (first sub))
    (when (funcall predicate first)
      (return-from select-clause (values first (rest sub))))
    (finally (return (values nil nil)))))


(defun more-clauses-p (clauses)
  (not (endp clauses)))


(declaim (notinline select-clause))
(defun clause-matches-goal-p (clause goal)
  cl-ds.utils:todo)


(defun combine-tags (first-cell second-cell)
  (declare (type cell first-cell second-cell)
           (optimize (speed 3)))
  (logior (ash (the fixnum (1- (tag-of first-cell)))
               (1+ +tag-size+))
          (1- (tag-of second-cell))))


(flet ((combine-tags (first-tag second-tag)
         (combine-tags (tag first-tag 0)
                       (tag second-tag 0))))
  (define-constant +var-var+ (combine-tags +variable+ +variable+))
  (define-constant +var-ref+ (combine-tags +variable+ +reference+))
  (define-constant +var-exp+ (combine-tags +variable+ +expression+))
  (define-constant +var-fixnum+ (combine-tags +variable+ +fixnum+))
  (define-constant +ref-var+ (combine-tags +reference+ +variable+))
  (define-constant +ref-ref+ (combine-tags +reference+ +reference+))
  (define-constant +ref-exp+ (combine-tags +reference+ +expression+))
  (define-constant +ref-fixnum+ (combine-tags +reference+ +fixnum+))
  (define-constant +exp-var+ (combine-tags +expression+ +variable+))
  (define-constant +exp-ref+ (combine-tags +expression+ +reference+))
  (define-constant +exp-exp+ (combine-tags +expression+ +expression+))
  (define-constant +exp-fixnum+ (combine-tags +expression+ +fixnum+))
  (define-constant +fixnum-var+ (combine-tags +fixnum+ +variable+))
  (define-constant +fixnum-ref+ (combine-tags +fixnum+ +reference+))
  (define-constant +fixnum-exp+ (combine-tags +fixnum+ +expression+))
  (define-constant +fixnum-fixnum+ (combine-tags +fixnum+ +fixnum+)))


(defun alter-cell (execution-state
                   execution-stack-cell
                   pointer
                   new-value)
  (declare (type execution-state execution-state)
           (type execution-stack-cell execution-stack-cell)
           (type pointer pointer)
           (type cell new-value))
  (let ((heap-trail (execution-stack-cell-heap-cells-trail
                     execution-stack-cell)))
    (vector-push-extend pointer heap-trail 2)
    (vector-push-extend (shiftf (aref (execution-state-heap execution-state) pointer)
                                new-value)
                        heap-trail)))


(defun unify-expressions (execution-state
                          execution-stack-cell
                          first-expression-pointer
                          second-expression-pointer)
  (with-unification-stack (execution-state)
    (let ((first-arity (deref (1+ first-expression-pointer)))
          (second-arity (deref (1+ second-expression-pointer))))
      (unless (eql first-arity second-arity)
        (done nil))
      (iterate
        (for i from 0 below first-arity)
        (upush (+ first-expression-pointer 2 i)
               (+ second-expression-pointer 2 i)))
      t)))


(defun unify-references (execution-state
                         execution-stack-cell
                         pointer1 pointer2
                         ref1 ref2)
  (unify-pair execution-state
              (follow-reference execution-state ref1 t)
              (follow-reference execution-state ref2 t)))


(defun unify-variable-fixnum (execution-state
                              execution-stack-cell
                              variable-pointer
                              fixnum-pointer
                              variable-cell
                              fixnum-cell)
  (declare (ignore fixnum-pointer))
  (unless (eql 0 (detag variable-cell))
    (return-from unify-variable-fixnum nil))
  (alter-cell execution-state
              execution-stack-cell
              variable-pointer
              fixnum-cell)
  t)


(defun unify-variables (execution-state
                        execution-stack-cell
                        pointer1
                        pointer2
                        cell1
                        cell2)
  (unless (< pointer1 pointer2)
    (rotatef pointer1 pointer2)
    (rotatef cell1 cell2))
  (let ((first-unbound (zerop (detag cell1)))
        (second-unbound (zerop (detag cell2))))
    (cond ((nor first-unbound second-unbound)
           nil)
          ((and first-unbound second-unbound)
           (alter-cell execution-state execution-stack-cell
                       pointer1 (tag +reference+ pointer2))
           t)
          (first-unbound
           (alter-cell execution-state execution-stack-cell
                       pointer1 cell2)
           t)
          (second-unbound
           (alter-cell execution-state execution-stack-cell
                       pointer2 cell1)
           t))))


(defun unify-variable-reference (execution-state
                                 execution-stack-cell
                                 variable-pointer
                                 reference-pointer
                                 variable-cell
                                 reference-cell)
  (declare (ignore variable-cell reference-pointer))
  (unify-pair execution-state
              execution-stack-cell
              variable-pointer
              (follow-reference execution-state
                                (detag reference-cell)
                                t)))


(defun unify-variable-expression (execution-state
                                  execution-stack-cell
                                  variable-pointer
                                  expression-pointer
                                  variable-cell
                                  expression-cell)
  (declare (ignore expression-cell))
  (unless (eql 0 (detag variable-cell))
    (return-from unify-variable-expression nil))
  (alter-cell execution-state
              execution-stack-cell
              variable-pointer
              (tag +reference+ expression-pointer))
  t)


(defun unify-pair (execution-state execution-stack-cell pointer1 pointer2)
  (when (eql pointer1 pointer2)
    (return-from unify-pair t))
  (let ((cell1 (dereference-heap-pointer execution-state pointer1))
        (cell2 (dereference-heap-pointer execution-state pointer2)))
    (when (eql cell1 cell2)
      (return-from unify-pair t))
    (let ((combine-tags (combine-tags cell1 cell2)))
      (cond
        ((eql combine-tags +var-var+)
         (unify-variables execution-state
                          execution-stack-cell
                          pointer1 pointer2
                          cell1 cell2))
        ((eql combine-tags +var-fixnum+)
         (unify-variable-fixnum execution-state
                                execution-stack-cell
                                pointer1 pointer2
                                cell1 cell2))
        ((eql combine-tags +fixnum-var+)
         (unify-variable-fixnum execution-state
                                execution-stack-cell
                                pointer2 pointer1
                                cell2 cell1))
        ((eql combine-tags +var-ref+)
         (unify-pair execution-state
                     execution-stack-cell
                     pointer1
                     (follow-reference execution-state pointer2 t)))
        ((eql combine-tags +ref-var+)
         (unify-pair execution-state
                     execution-stack-cell
                     (follow-reference execution-state pointer1 t)
                     pointer2))
        ((eql combine-tags +var-exp+)
         (unify-variable-expression execution-state execution-stack-cell
                                    pointer1 pointer2 cell1 cell2))
        ((eql combine-tags +exp-var+)
         (unify-variable-expression execution-state execution-stack-cell
                                    pointer2 pointer1 cell2 cell1))
        ((eql combine-tags +exp-exp+)
         (unify-expressions execution-state execution-stack-cell
                            pointer1 pointer2))
        ((eql combine-tags +fixnum-fixnum+)
         nil)
        ((eql combine-tags +ref-ref+)
         (unify-references execution-state
                           execution-stack-cell
                           pointer1 pointer2
                           cell1 cell2))))))


(defun unify (execution-state execution-stack-cell)
  (declare (type execution-stack-cell execution-stack-cell)
           (type execution-state execution-state))
  (with-unification-stack (execution-state)
    (when (uemptyp)
      (done t))
    (upop
     (stack-pointer goal-pointer)
     (let ((result (unify-pair execution-state execution-stack-cell
                               stack-pointer goal-pointer)))
       (unless result
         (done nil)))
     (next))))


(defun answer (execution-state stack-cell)
  cl-ds.utils:todo)


(defun unfold (execution-state stack-cell)
  (declare (type execution-stack-cell stack-cell)
           (type execution-state execution-state))
  (let* ((goals (execution-stack-cell-goals stack-cell))
         (clauses (execution-state-clauses execution-state))
         (goal (first goals))
         (goal-cell (dereference-heap-pointer execution-state
                                              goal)))
    (iterate
      (unless (more-clauses-p clauses)
        (leave (pop-stack-cell execution-state stack-cell)))
      (for (values clause more) =
           (select-clause (lambda (clause)
                            (clause-matches-goal-p clause goal-cell))
                          clauses))
      (when (null clause)
        (leave (pop-stack-cell execution-state stack-cell)))
      (for (values bindings-fill-pointer success) =
           (clause-body-to-heap execution-state stack-cell
                                clause goal))
      (unless success
        (unbind-range execution-state
                      (execution-stack-cell-bindings-fill-pointer stack-cell)
                      bindings-fill-pointer)
        (unwind-variable-bindings-trail execution-state trail))
      (for new-stack-cell = (push-stack-cell stack-cell clause))
      (prepare-unification-stack execution-state
                                 new-stack-cell
                                 goal)
      (for success-p = (unify execution-state new-stack-cell))
      (until success-p)
      (pop-stack-cell execution-state new-stack-cell)
      (setf clauses more)
      (finally (return new-stack-cell)))))
