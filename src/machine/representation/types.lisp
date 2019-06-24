(cl:in-package #:huginn.machine.representation)


(define-constant +placeholder-array+
    (make-array 0 :element-type 'cell)
  :test 'vector=
  :documentation "Just an empty array of correct type, used as initial binding for CLAUSE slot.")


(define-constant +placeholder-pointer-array+
    (make-array 0 :element-type 'pointer)
  :test 'vector= )


(deftype vector-representation ()
  `(simple-array cell (*)))


(deftype pointer ()
  'non-negative-fixnum)


(defstruct clause
  (goal-pointers +placeholder-pointer-array+ :type (simple-array pointer (*)))
  (variable-values +placeholder-array+ :type simple-vector)
  (content +placeholder-array+ :type vector-representation)
  (body-pointer 0 :type fixnum))


(def <empty-range-placeholder> (make 'cl-ds:empty-range))


(defstruct execution-stack-cell
  (clause nil :type (or null clause))
  ;; clause used to construct this execution-stack-cell
  (clauses <empty-range-placeholder> :type cl-ds:fundamental-forward-range)
  ;; range yielding a clauses matching to the first goal in this execution-stack-cell
  (goals '() :type list)
  ;; list of goals (pointers to the heap) for this cell. During unfolding top goal in this cell is selected for proving. If goal can't be proved stack cell will be popped. Therefore this stack represents already proven goals.
  (heap-pointer 0 :type fixnum)
  ;; location of representation of this cell on the heap
  (heap-cells-trail (make-array 16 :adjustable t
                                   :fill-pointer 0
                                   :element-type 'fixnum)
   :type (cl-ds.utils:extendable-vector fixnum))
  ;; undo trail for heap-cells. each cons cell in the list contains pair address.value
  (bindings-fill-pointer 0 :type cl-ds.utils:index)
  ;; bindings fill pointer (in the execution-state)
  (heap-fill-pointer 0 :type cl-ds.utils:index)
  ;; heap fill pointer (actual heap is represented as a simple-array
  (previous-cell nil :type (or null execution-stack-cell))
  ;; well, it is stack, what did you expect?
  )


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
  (stack nil :type (optional execution-stack-cell))
  )


(-> execution-stack-cell-more-goals-p (execution-stack-cell) boolean )
(defun execution-stack-cell-more-goals-p (execution-stack-cell)
  (declare (optimize (speed 3)))
  (~> execution-stack-cell execution-stack-cell-goals endp not))


(-> clause-content-length (clause) cl-ds.utils:index)
(defun clause-content-length (clause)
  (declare (optimize (speed 3)))
  (~> clause clause-content length))


(defun clause-goals (clause pointer-offset &optional (initial-list '()))
  (declare (type list initial-list)
           (type clause clause)
           (type pointer pointer-offset))
  (iterate
    (with goals = (clause-goal-pointers clause))
    (for i from (1- (length goals)) downto 0)
    (push (+ (aref goals i) pointer-offset) initial-list))
  initial-list)


(-> clause-body-length (clause) cl-ds.utils:index)
(defun clause-body-length (clause)
  (declare (optimize (speed 3)))
  (- (clause-content-length clause) (clause-body-pointer clause)))


(defun make-heap (size)
  (make-array size
              :element-type 'cell
              :initial-element 0))


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
      (setf (execution-state-heap state)
            (adjust-array old-heap new-heap-size)))
    state))


(declaim (inline dereference-variable))
(defun dereference-variable (state cell)
  (declare (type execution-state state)
           (type cell cell)
           (optimize (speed 3)))
  (let ((word (detag cell)))
    (assert (> word 0))
    (aref (execution-state-variable-bindings state) (1- (detag cell)))))


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


(declaim (inline follow-pointer))
(-> follow-pointer (execution-state pointer &optional boolean) pointer)
(defun follow-pointer (execution-state pointer &optional recursive)
  (declare (type execution-state execution-state)
           (type pointer pointer)
           (optimize (speed 3) (safety 0)))
  (iterate
    (declare (type pointer prev-pointer))
    (with prev-pointer = pointer)
    (for heap-cell = (~> execution-state execution-state-heap (aref pointer)))
    (while (and recursive (reference-cell-p heap-cell)))
    (shiftf prev-pointer pointer (detag heap-cell))
    (finally (return prev-pointer))))


(declaim (inline dereference-heap-pointer))
(-> dereference-heap-pointer (execution-state pointer &optional boolean) cell)
(defun dereference-heap-pointer (execution-state pointer &optional follow-references)
  (declare (type execution-state execution-state)
           (type pointer pointer)
           (optimize (speed 3) (safety 0)))
  (~> execution-state execution-state-heap
      (aref (follow-pointer execution-state pointer follow-references))))
