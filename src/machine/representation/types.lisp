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
  (variable-bindings +placeholder-array+ :type vector-representation)
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
  (trail +placeholder-array+ :type vector-representation)
  ;; undo trail for variable bindings. This only contains variables not established in this stack cell
  (bindings-fill-pointer 0 :type cl-ds.utils:index)
  ;; bindings fill pointer (in the execution-state)
  (heap-fill-pointer 0 :type cl-ds.utils:index)
  ;; heap fill pointer (actual heap is represented as a simple-array
  (previous-cell nil :type (or null execution-stack-cell))
  ;; well, it is stack, what did you expect?
  )


(define-constant unbound 'unbound)


(defun unwind-trail (execution-state trail)
  (iterate
    (declare (type fixnum i)
             (type vector-representation trail))
    (with bindings = (execution-state-variable-bindings execution-state))
    (for i from 0 below (length trail))
    (for elt = (aref trail i))
    (while (variable-cell-p elt))
    (setf (aref bindings (detag elt)) unbound)))


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
                                 execution-stack-cell))
         (trail (execution-stack-cell-trail execution-stack-cell)))
    (unbind-range execution-state
                  previous-bindings-fill-pointer
                  bindings-fill-pointer)
    (unwind-trail execution-state trail)
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
(defun push-stack-cell (execution-stack-cell clause trail)
  "Constructs new stack-cell based on the clause, trail and execution-stack-cell assuming that this cell is constructed from the first goal of the execution-stack-cell after data was already placed to the heap."
  (declare (type vector-representation trail)
           (type clause clause))
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
     :trail trail
     :bindings-fill-pointer (~> execution-stack-cell
                                execution-stack-cell-bindings-fill-pointer
                                (+ (length trail))))))


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
    (declare (type fixnum lookup-result))
    (unless (eql lookup-result object-index)
      (return-from index-object  (values lookup-result nil)))
    (incf (execution-state-objects-index execution-state))
    (let ((bindings-length (length bindings)))
      (unless (< object-index bindings-length)
        (iterate
          (declare (type fixnum i))
          (with new-bindings = (make-array (ash bindings-length 1)))
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
  (assert (variable-cell-p cell))
  (aref (execution-state-variable-bindings state) (detag cell)))


(declaim (inline (setf dereference-variable)))
(defun (setf dereference-variable) (new-value state cell)
  (declare (type execution-state state)
           (type cell cell)
           (optimize (speed 3)))
  (setf (aref (execution-state-variable-bindings state) (detag cell))
        new-value))


(declaim (inline unbound-p))
(defun unbound-p (object)
  (eq unbound object))


;; this can benefit from automaticly generated and compiled function for each clause, should be a little bit faster.
;; (declaim (inline clause-body-to-heap))
(defun clause-body-to-heap (execution-state execution-stack-cell clause head-pointer)
  "Copies clause body to heap. Will extend variable bindings in the state (or fail and return nil if can't do so). Will return: new trail, new bindings-heap-pointer, and success-info. To unroll changes do the execution-state performed by this function it is required to both unwind-trail and unbind-range"
  (declare (type execution-state execution-state)
           (type execution-stack-cell execution-stack-cell)
           (type clause clause)
           (type fixnum head-pointer))
  (let* ((body-length (clause-body-length clause))
         (content (clause-content clause))
         (body-pointer (clause-body-pointer clause))
         (clause-variable-bindings
           (clause-variable-bindings clause))
         (body-to-head-mapping (clause-body-to-head-mapping clause))
         (trail-size 0)
         (fill-pointer
           (execution-stack-cell-heap-fill-pointer execution-stack-cell))
         (trail (make-array (iterate
                              (declare (type fixnum i))
                              (for i from 0 below body-pointer)
                              (counting (variable-cell-p (aref content i))))
                            :element-type 'fixnum))
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
          (bind ((object (aref clause-variable-bindings word))
                 (head-offset (aref body-to-head-mapping z))
                 ((:values index new) (index-object execution-state
                                                    object)))
            (declare (ignore new))
            (if (not (variable-cell-p head-offset))
                (setf (aref heap i) (tag +variable+ index))
                (let* ((head-index (+ head-pointer (detag head-offset)))
                       (head-cell (aref heap head-index))
                       (state-object (dereference-variable execution-state
                                                           head-cell)))
                  (assert (variable-cell-p head-cell))
                  (incf bindings-fill-pointer)
                  (unless (or (eq object state-object)
                              (unbound-p state-object))
                    (return-from clause-body-to-heap
                      (values trail bindings-fill-pointer nil)))
                  (when (unbound-p state-object)
                    (setf (dereference-variable execution-state head-cell) index
                          (aref trail trail-size) index
                          trail-size (1+ trail-size)))
                  (setf (aref heap i) head-cell
                        cell head-cell))))))
      (values trail bindings-fill-pointer t))))


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


(-> dereference-heap-pointer (execution-state pointer) cell)
(defun dereference-heap-pointer (execution-state pointer)
  (declare (type execution-state execution-state)
           (type pointer pointer)
           (optimize (speed 3)))
  (~> execution-state execution-state-heap (aref pointer)))


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


(defun unify-expressions (execution-state
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


(defun unify (execution-state execution-stack-cell)
  (declare (type execution-stack-cell execution-stack-cell)
           (type execution-state execution-state))
  (with-unification-stack (execution-state)
    (when (uemptyp)
      (done t))
    (upop
     (stack-pointer goal-pointer)
     (let* ((stack-cell (deref stack-pointer))
            (goal-cell (deref goal-pointer)))
       (when (eql stack-cell goal-cell)
         (next))
       (let ((combined-tag (combine-tags stack-cell goal-cell)))
         (cond
           ((eql combined-tag +var-var+)
            cl-ds.utils:todo)
           ((and
              (eql combined-tag +ref-ref+)
              (unify-expressions execution-state
                                 (detag stack-cell)
                                 (detag goal-cell)))
            nil)
           (t (done nil))))))
    (next)))


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
      (for (values trail bindings-fill-pointer success) =
           (clause-body-to-heap execution-state stack-cell
                                clause goal))
      (unless success
        (unbind-range execution-state
                      (execution-stack-cell-bindings-fill-pointer stack-cell)
                      bindings-fill-pointer)
        (unwind-trail execution-state trail))
      (for new-stack-cell = (push-stack-cell stack-cell clause trail))
      (prepare-unification-stack execution-state
                                 new-stack-cell
                                 goal)
      (for success-p = (unify execution-state new-stack-cell))
      (until success-p)
      (pop-stack-cell execution-state new-stack-cell)
      (setf clauses more)
      (finally (return new-stack-cell)))))
