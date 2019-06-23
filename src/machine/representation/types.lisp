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
  (objects-mapping (make-hash-table :test 'eq) :type hash-table)
  ;; translates instance into unique index
  (variable-bindings (make-array 64) :type (simple-array t (*)))
  ;; reverse mapping, maps unique index to instance
  (heap +placeholder-array+ :type vector-representation)
  ;; heap, stores both data and code. Fresh clauses are appended at the back. It is a simple array, and fill-pointer is stored in the execution-stack-cell.
  (unification-stack (make-array 16 :element-type 'fixnum
                                    :adjustable t
                                    :fill-pointer 0)
   :type (cl-ds.utils:extendable-vector fixnum))
  ;; shared unification stack. needs to be resetted before using.
  )


(defstruct execution-stack-cell
  (clause nil :type (or null clause))
  ;; clause used to construct this execution-stack-cell
  (goals '() :type list)
  ;; list of goals (pointers to the heap) for this cell. During unfolding top goal in this cell is selected for proving. If goal can't be proved stack cell will be popped. Therefore this stack represents already proven goals.
  (heap-pointer 0 :type fixnum)
  ;; location of representation of this cell on the heap
  (heap-cells-trail (make-array 16 :adjustable t :fill-pointer 0 :element-type 'fixnum)
   :type (cl-ds.utils:extendable-vector fixnum))
  ;; undo trail for heap-cells. each cons cell in the list contains pair address.value
  (bindings-fill-pointer 0 :type cl-ds.utils:index)
  ;; bindings fill pointer (in the execution-state)
  (heap-fill-pointer 0 :type cl-ds.utils:index)
  ;; heap fill pointer (actual heap is represented as a simple-array
  (previous-cell nil :type (or null execution-stack-cell))
  ;; well, it is stack, what did you expect?
  )


(define-constant unbound 'unbound)


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
(defun push-stack-cell (execution-stack-cell clause bindings-fill-pointer)
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
     :bindings-fill-pointer bindings-fill-pointer)))


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
(defun index-object (execution-state object object-index)
  (declare (type execution-state execution-state)
           (type fixnum object-index))
  (bind ((objects-mapping (execution-state-objects-mapping execution-state))
         (bindings (execution-state-variable-bindings execution-state))
         (lookup-result (ensure (gethash object objects-mapping) object-index)))
    (declare (type fixnum lookup-result)
             (optimize (speed 3)))
    (unless (eql lookup-result object-index)
      (return-from index-object  lookup-result))
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
      lookup-result)))


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
  (declare (optimize (speed 3))
           (type execution-state execution-state)
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
      (declare (type fixnum bindings-fill-pointer))
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
              (let* ((new-index (1+ bindings-fill-pointer))
                     (index (index-object execution-state
                                          object
                                          new-index)))
                (declare (type fixnum index new-index))
                (setf (aref heap i) (tag +variable+ index))
                (maxf bindings-fill-pointer index))))))
      bindings-fill-pointer)))


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
