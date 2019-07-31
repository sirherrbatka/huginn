(cl:in-package #:huginn.machine.representation)

(def <empty-range-placeholder> (make 'cl-ds:empty-range))

(with-compilation-unit (:override nil)
  (deftype vector-representation ()
    `(simple-array cell (*)))


  (deftype pointer ()
    'non-negative-fixnum)


  (define-constant +placeholder-array+
      (make-array 0 :element-type 'cell)
    :test 'vector=
    :documentation "Just an empty array of correct type, used as initial binding for CLAUSE slot.")


  (define-constant +placeholder-pointer-array+
      (make-array 0 :element-type 'pointer)
    :test 'vector= )


  (defstruct clause
    (input)
    (goal-pointers +placeholder-pointer-array+ :type (simple-array pointer (*)))
    (variable-values +placeholder-array+ :type simple-vector)
    (content +placeholder-array+ :type vector-representation)
    (body-pointer 0 :type fixnum))


  (defstruct execution-stack-cell
    (clause nil :type (or null clause))
    ;; clause used to construct this execution-stack-cell
    (clauses <empty-range-placeholder> :type cl-ds:fundamental-forward-range)
    ;; range yielding a clauses matching to the first goal in this execution-stack-cell
    (goals '() :type list)
    ;; list of goals (pointers to the heap) for this cell. During unfolding top goal in this cell is selected for proving. If goal can't be proved stack cell will be popped. Therefore this stack represents already proven goals.
    (heap-pointer 0 :type fixnum)
    ;; location of representation of this cell on the heap
    (bindings-fill-pointer 0 :type cl-ds.utils:index)
    ;; bindings fill pointer (in the execution-state)
    (heap-fill-pointer 0 :type cl-ds.utils:index)
    ;; heap fill pointer (actual heap is represented as a simple-array
    (previous-cell nil :type (or null execution-stack-cell))
    ;; well, it is stack, what did you expect?
    (unwind-trail-pointer 0 :type fixnum)
    )


  (defun make-initial-execution-stack-cell (goal-pointers heap-fill-pointer
                                            bindings-fill-pointer
                                            clauses)
    (declare (type list goal-pointers)
             (type cl-ds.utils:index bindings-fill-pointer)
             (type pointer heap-fill-pointer))
    (assert (> heap-fill-pointer 0))
    (assert (>= bindings-fill-pointer 0))
    (make-execution-stack-cell
     :clauses clauses
     :heap-fill-pointer heap-fill-pointer
     :bindings-fill-pointer bindings-fill-pointer
     :goals goal-pointers))


  (defstruct execution-state
    (database)
    ;; all clauses that can be selected for unfolding
    (objects-mapping (make-hash-table :test 'eql) :type hash-table)
    ;; translates instance into unique index
    (variable-bindings (make-array 64) :type (simple-array t (*)))
    ;; reverse mapping, maps unique index to instance
    (heap +placeholder-array+ :type vector-representation)
    ;; heap, stores both data and code. Fresh clauses are appended at the back. It is a simple array, and fill-pointer is stored in the execution-stack-cell.
    (unification-stack (make-array 64 :element-type 'fixnum)
     :type (simple-array fixnum (*)))
    (unification-stack-fill-pointer 0 :type fixnum)
    ;; shared unification stack. needs to be resetted before using.
    (stack nil :type (or null execution-stack-cell))
    ;; Actual execution stack. If NULL after unfolding, no more answers can be found.
    (unwind-trail (make-array 64 :element-type 'fixnum)
     :type (simple-array fixnum (*)))
    )


  (-> execution-stack-cell-more-goals-p (execution-stack-cell) boolean)
  (declaim (inline execution-stack-cell-more-goals-p))
  (defun execution-stack-cell-more-goals-p (execution-stack-cell)
    (declare (optimize (speed 3) (safety 0)))
    (~> execution-stack-cell execution-stack-cell-goals endp not))


  (-> clause-content-length (clause) cl-ds.utils:index)
  (defun clause-content-length (clause)
    (declare (optimize (speed 3) (safety 0)))
    (~> clause clause-content length))


  (defun clause-goals (clause pointer-offset &optional (initial-list '()))
    (declare (type list initial-list)
             (type clause clause)
             (type pointer pointer-offset))
    (iterate
      (declare (type fixnum i))
      (with goals = (clause-goal-pointers clause))
      (for i from (1- (length goals)) downto 0)
      (push (the pointer (+ (aref goals i)
                            pointer-offset))
            initial-list))
    initial-list)


  (-> clause-body-length (clause) cl-ds.utils:index)
  (defun clause-body-length (clause)
    (declare (optimize (speed 3) (safety 0)))
    (- (clause-content-length clause) (clause-body-pointer clause)))


  (defun expand-state-heap (state desired-size)
    (declare (type execution-state state)
             (type fixnum desired-size)
             (optimize (speed 3) (safety 0)))
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
             (optimize (speed 3) (safety 1)))
    (assert (variable-cell-p cell))
    (let* ((word (detag cell))
           (index (1- word))
           (bindings (execution-state-variable-bindings state))
           (length (length bindings)))
      (declare (type fixnum index))
      (when (negative-fixnum-p index)
        (error 'variable-unbound-error))
      (unless (< index length)
        (error 'unknown-variable-error :cell cell))
      (aref bindings index)))


  (-> execution-state-heap-size (execution-state) fixnum)
  (defun execution-state-heap-size (state)
    (declare (type execution-state state)
             (optimize (speed 3) (safety 0)))
    (~> state execution-state-heap length))


  (-> clause-length (clause) fixnum)
  (defun clause-length (clause)
    (declare (optimize (speed 3) (safety 0)))
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
      ;; (with initial = pointer)
      (with heap = (execution-state-heap execution-state))
      (for heap-cell = (aref heap pointer))
      (if (and recursive (reference-cell-p heap-cell))
          (setf pointer (detag heap-cell))
          (finish))
      (finally (return pointer))))


  (declaim (inline dereference-heap-pointer))
  (-> dereference-heap-pointer (execution-state pointer &optional boolean) cell)
  (defun dereference-heap-pointer (execution-state pointer
                                   &optional follow-references)
    (declare (type execution-state execution-state)
             (type pointer pointer)
             (optimize (speed 3) (safety 0)))
    (~> execution-state execution-state-heap
        (aref (follow-pointer execution-state pointer follow-references))))


  (-> clone-execution-stack-cell ((or null execution-stack-cell))
      (or null execution-stack-cell))
  (defun clone-execution-stack-cell (execution-stack-cell)
    (when (null execution-stack-cell)
      (return-from clone-execution-stack-cell nil))
    (make-execution-stack-cell
     :clauses (~> execution-stack-cell
                  execution-stack-cell-clauses
                  cl-ds:clone)
     :clause (execution-stack-cell-clause execution-stack-cell)
     :goals (execution-stack-cell-goals execution-stack-cell)
     :heap-pointer (execution-stack-cell-heap-pointer execution-stack-cell)
     :unwind-trail-pointer (execution-stack-cell-unwind-trail-pointer
                            execution-stack-cell)
     :bindings-fill-pointer (execution-stack-cell-bindings-fill-pointer
                             execution-stack-cell)
     :heap-fill-pointer (execution-stack-cell-heap-fill-pointer
                         execution-stack-cell)
     :previous-cell (~> execution-stack-cell
                        execution-stack-cell-previous-cell
                        clone-execution-stack-cell)))


  (-> clone-execution-state (execution-state) execution-state)
  (defun clone-execution-state (execution-state)
    (make-execution-state
     :database (execution-state-database execution-state)
     :objects-mapping (~> execution-state
                          execution-state-objects-mapping
                          copy-hash-table)
     :heap (~> execution-state
               execution-state-heap
               copy-array)
     :variable-bindings (~> execution-state
                            execution-state-variable-bindings
                            copy-array)
     :unwind-trail (~> execution-state
                       execution-state-unwind-trail
                       copy-array)
     :stack (~> execution-state
                execution-state-stack
                clone-execution-stack-cell))))


(defmethod print-object ((object clause) stream)
  (print-unreadable-object (object stream :type t)
    (print-byte-code (clause-content object) stream)))


(defmethod print-object ((object execution-state) stream)
  (print-unreadable-object (object stream :type t)
    (print-byte-code (execution-state-heap object) stream
                     (let ((stack (execution-state-stack object)))
                       (if (null stack)
                           0
                           (execution-stack-cell-heap-fill-pointer stack))))))
