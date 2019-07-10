(cl:in-package #:huginn.machine.operations)


(with-compilation-unit (:override nil)
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))
  ;; this should be performed only after clause was already proven (and therefore copied to heap.
  ;; Code assumes that next stack cell is located DIRECTLY after the current one on the heap
  (defun push-stack-cell (execution-stack-cell clause
                          bindings-fill-pointer)
    "Constructs new stack-cell based on the clause, trail and execution-stack-cell assuming that this cell is constructed from the first goal of the execution-stack-cell after data was already placed to the heap."
    (declare (type huginn.m.r:clause clause)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type cl:fixnum bindings-fill-pointer))
    (let* ((fill-pointer (huginn.m.r:execution-stack-cell-heap-fill-pointer
                          execution-stack-cell))
           (new-fill-pointer (+ fill-pointer
                                (huginn.m.r:clause-body-pointer clause))))
      (declare (type huginn.m.r:pointer new-fill-pointer fill-pointer))
      (huginn.m.r:make-execution-stack-cell
       :previous-cell execution-stack-cell
       :heap-fill-pointer new-fill-pointer
       :clause clause
       :heap-pointer fill-pointer
       :bindings-fill-pointer bindings-fill-pointer)))


  ;; (declaim (inline index-object))
  (defun index-object (execution-state object bindings-fill-pointer)
    (declare (type huginn.m.r:execution-state execution-state)
             (type fixnum bindings-fill-pointer))
    (let* ((objects-mapping (huginn.m.r:execution-state-objects-mapping
                             execution-state))
           (bindings (huginn.m.r:execution-state-variable-bindings
                      execution-state))
           (lookup-result (ensure (gethash object objects-mapping)
                            bindings-fill-pointer))
           (new (eql lookup-result bindings-fill-pointer)))
      (declare (type fixnum lookup-result)
               (type simple-vector bindings))
      (when new
        (let ((bindings-length (length bindings)))
          (unless (< bindings-fill-pointer bindings-length)
            (setf bindings (adjust-array
                            bindings
                            (logand most-positive-fixnum
                                    (ash (1+ bindings-fill-pointer) 1)))
                  (huginn.m.r:execution-state-variable-bindings execution-state)
                  bindings))
          (setf (aref bindings bindings-fill-pointer) object)
          lookup-result))
      lookup-result))


  (defun relocate-cells (execution-state
                         clause
                         destination-start
                         source-start source-end
                         bindings-fill-pointer)
    (declare (type huginn.m.r:pointer
                   destination-start source-start source-end
                   bindings-fill-pointer)
             (type huginn.m.r:execution-state execution-state)
             (type huginn.m.r:clause clause))
    (huginn.m.r:expand-state-heap execution-state
                                  (the fixnum
                                       (+ destination-start
                                          (the fixnum (- source-end source-start)))))
    (iterate
      (declare (type fixnum i j))
      (with heap = (huginn.m.r:execution-state-heap execution-state))
      (with source = (huginn.m.r:clause-content clause))
      (with variable-values = (huginn.m.r:clause-variable-values
                               clause))
      (for i from destination-start)
      (for j from source-start below source-end)
      (for cell = (aref source j))
      (setf (aref heap i) cell)
      (when (and (not (first-iteration-p))
                 (~>> (1- i)
                      (aref heap)
                      huginn.m.r:expression-cell-p))
        (next-iteration))
      (for word = (huginn.m.r:detag cell))
      (huginn.m.r:tag-case (cell)
        :expression
        (setf (aref heap i) (huginn.m.r:tag huginn.m.r:+expression+ i))
        :reference
        (incf (aref heap i) source-start)
        :list-start
        (incf (aref heap i) source-start)
        :list-rest
        (unless (huginn.m.r:list-rest-unbound-p cell)
          (let* ((object (aref variable-values (1- word)))
                 (new-index bindings-fill-pointer)
                 (index (index-object execution-state
                                      object
                                      new-index)))
            (declare (type fixnum index new-index))
            (setf (aref heap i) (huginn.m.r:tag huginn.m.r:+list-rest+
                                                (1+ index)))
            (when (eql index new-index)
              (incf bindings-fill-pointer))))
        :variable
        (unless (huginn.m.r:variable-unbound-p cell)
          (let* ((object (aref variable-values (1- word)))
                 (new-index bindings-fill-pointer)
                 (index (index-object execution-state
                                      object
                                      new-index)))
            (declare (type fixnum index new-index))
            (setf (aref heap i) (huginn.m.r:tag huginn.m.r:+variable+
                                                (1+ index)))
            (when (eql index new-index)
              (incf bindings-fill-pointer))))))
    bindings-fill-pointer)


  (defun clause-head-to-heap (execution-state execution-stack-cell clause)
    (declare (optimize (speed 3))
             (type huginn.m.r:execution-state execution-state)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:clause clause))
    (let ((head-length (huginn.m.r:clause-body-pointer clause))
          (fill-pointer
            (huginn.m.r:execution-stack-cell-heap-fill-pointer
             execution-stack-cell))
          (bindings-fill-pointer (huginn.m.r:execution-stack-cell-bindings-fill-pointer
                                  execution-stack-cell)))
      (relocate-cells execution-state
                      clause
                      fill-pointer
                      0
                      head-length
                      bindings-fill-pointer)))


  (defun clause-body-to-heap (execution-state execution-stack-cell)
    "Copies clause body to heap. Will extend variable bindings in the state (or fail and return nil if can't do so). Will return: new trail, new bindings-heap-pointer, and success-info. To unroll changes do the execution-state performed by this function it is required to both unwind-variable-bindings-trail and unbind-range"
    (declare (type huginn.m.r:execution-state execution-state)
             (type huginn.m.r:execution-stack-cell execution-stack-cell))
    (let* ((clause (huginn.m.r:execution-stack-cell-clause
                    execution-stack-cell))
           (full-length (huginn.m.r:clause-length clause))
           (body-length (huginn.m.r:clause-body-length clause))
           (fill-pointer
             (huginn.m.r:execution-stack-cell-heap-fill-pointer
              execution-stack-cell))
           (previous-cell (huginn.m.r:execution-stack-cell-previous-cell
                           execution-stack-cell))
           (previous-fill-pointer
             (if (null previous-cell)
                 0
                 (huginn.m.r:execution-stack-cell-heap-fill-pointer
                  previous-cell)))
           (new-fill-pointer (+ fill-pointer body-length))
           (database (huginn.m.r:execution-state-database
                      execution-state))
           (body-pointer (huginn.m.r:clause-body-pointer clause))
           (bindings-fill-pointer
             (huginn.m.r:execution-stack-cell-bindings-fill-pointer
              execution-stack-cell))
           (new-bindings-fill-pointer (relocate-cells execution-state
                                                      clause
                                                      fill-pointer
                                                      body-pointer
                                                      full-length
                                                      bindings-fill-pointer))
           (goals (~>> execution-stack-cell
                       huginn.m.r:execution-stack-cell-goals
                       rest
                       (huginn.m.r:clause-goals clause
                                                previous-fill-pointer))))
      (setf (huginn.m.r:execution-stack-cell-bindings-fill-pointer
             execution-stack-cell)
            new-bindings-fill-pointer

            (huginn.m.r:execution-stack-cell-goals execution-stack-cell)
            goals

            (huginn.m.r:execution-stack-cell-heap-fill-pointer
             execution-stack-cell)
            new-fill-pointer)
      (unless (endp goals)
        (setf (huginn.m.r:execution-stack-cell-clauses execution-stack-cell)
              (huginn.m.d:matching-clauses database execution-state
                                           (first goals))))
      nil))
  )
