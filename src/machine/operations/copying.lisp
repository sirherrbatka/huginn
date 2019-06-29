(cl:in-package #:huginn.machine.operations)


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
         (new-fill-pointer (+ fill-pointer (huginn.m.r:clause-body-length
                                            clause))))
    (declare (type huginn.m.r:pointer new-fill-pointer fill-pointer))
    (huginn.m.r:make-execution-stack-cell
     :previous-cell execution-stack-cell
     :heap-fill-pointer new-fill-pointer
     :clause clause
     :heap-pointer fill-pointer
     :bindings-fill-pointer bindings-fill-pointer)))


;; (declaim (inline index-object))
(defun index-object (execution-state object object-index)
  (declare (type huginn.m.r:execution-state execution-state)
           (type fixnum object-index))
  (bind ((objects-mapping (huginn.m.r:execution-state-objects-mapping
                           execution-state))
         (bindings (huginn.m.r:execution-state-variable-bindings
                    execution-state))
         (lookup-result (ensure (gethash object objects-mapping) object-index)))
    (declare (type fixnum lookup-result)
             (optimize (speed 3)))
    (unless (eql lookup-result object-index)
      (return-from index-object lookup-result))
    (let ((bindings-length (length bindings)))
      (unless (< object-index bindings-length)
        (setf (huginn.m.r:execution-state-variable-bindings
               execution-state)
              (adjust-array bindings (logand most-positive-fixnum
                                             (ash (1+ bindings-length) 1)))))
      (setf (aref bindings object-index) object)
      lookup-result)))


(defun clause-head-to-heap (execution-state execution-stack-cell clause)
  (declare (optimize (speed 3))
           (type huginn.m.r:execution-state execution-state)
           (type huginn.m.r:execution-stack-cell execution-stack-cell)
           (type huginn.m.r:clause clause))
  (let* ((content (huginn.m.r:clause-content clause))
         (head-length (huginn.m.r:clause-body-pointer clause))
         (variable-values (huginn.m.r:clause-variable-values clause))
         (fill-pointer
           (huginn.m.r:execution-stack-cell-heap-fill-pointer
            execution-stack-cell))
         (new-fill-pointer (+ fill-pointer head-length)))
    (declare (type fixnum new-fill-pointer))
    (huginn.m.r:expand-state-heap execution-state new-fill-pointer)
    (let* ((heap (huginn.m.r:execution-state-heap execution-state))
           (bindings-fill-pointer
             (huginn.m.r:execution-stack-cell-bindings-fill-pointer
              execution-stack-cell)))
      (declare (type fixnum bindings-fill-pointer))
      (iterate
        (declare (type fixnum i j z))
        (declare (type fixnum i j))
        (for i from fill-pointer below new-fill-pointer)
        (for j from 0)
        (for z from 0)
        (for cell = (aref content j))
        (setf (aref heap i) cell)
        (for word = (huginn.m.r:detag cell))
        (huginn.m.r:tag-case (cell)
          :expression
          (setf (aref heap i) (huginn.m.r:tag huginn.m.r:+expression+ i))
          :reference
          (incf (aref heap i) fill-pointer)
          :variable
          (unless (huginn.m.r:variable-unbound-p cell)
            (let* ((object (aref variable-values word))
                   (new-index (1+ bindings-fill-pointer))
                   (index (index-object execution-state
                                        object
                                        new-index)))
              (declare (type fixnum index new-index))
              (setf (aref heap i) (huginn.m.r:tag huginn.m.r:+variable+
                                                  index))
              (maxf bindings-fill-pointer index)))))
      bindings-fill-pointer)))


(defun clause-body-to-heap (execution-state execution-stack-cell)
  "Copies clause body to heap. Will extend variable bindings in the state (or fail and return nil if can't do so). Will return: new trail, new bindings-heap-pointer, and success-info. To unroll changes do the execution-state performed by this function it is required to both unwind-variable-bindings-trail and unbind-range"
  (declare (optimize (speed 3))
           (type huginn.m.r:execution-state execution-state)
           (type huginn.m.r:execution-stack-cell execution-stack-cell))
  (let* ((clause (huginn.m.r:execution-stack-cell-clause
                  execution-stack-cell))
         (body-length (huginn.m.r:clause-body-length clause))
         (content (huginn.m.r:clause-content clause))
         (variable-values (huginn.m.r:clause-variable-values clause))
         (fill-pointer
           (huginn.m.r:execution-stack-cell-heap-fill-pointer
            execution-stack-cell))
         (new-fill-pointer (+ fill-pointer body-length)))
    (declare (type fixnum new-fill-pointer))
    (huginn.m.r:expand-state-heap execution-state new-fill-pointer)
    (let* ((heap (huginn.m.r:execution-state-heap execution-state))
           (body-pointer (~> execution-stack-cell
                             huginn.m.r:execution-stack-cell-clause
                             huginn.m.r:clause-body-pointer))
           (bindings-fill-pointer
             (huginn.m.r:execution-stack-cell-bindings-fill-pointer
              execution-stack-cell))
           (goals (~>> execution-stack-cell
                       huginn.m.r:execution-stack-cell-goals
                       rest
                       (huginn.m.r:clause-goals clause fill-pointer))))
      (declare (type fixnum bindings-fill-pointer))
      (iterate
        (declare (type fixnum i j z))
        (declare (type fixnum i j))
        (for i from fill-pointer below new-fill-pointer)
        (for j from body-pointer)
        (for z from 0)
        (for cell = (aref content j))
        (setf (aref heap i) cell)
        (for word = (huginn.m.r:detag cell))
        (huginn.m.r:tag-case (cell)
          :expression
          (setf (aref heap i) (huginn.m.r:tag huginn.m.r:+expression+ i))
          :reference
          (incf (aref heap i) fill-pointer)
          :variable
          (unless (huginn.m.r:variable-unbound-p cell)
            (let* ((object (aref variable-values word))
                   (new-index (1+ bindings-fill-pointer))
                   (index (index-object execution-state
                                        object
                                        new-index)))
              (declare (type fixnum index new-index))
              (setf (aref heap i) (huginn.m.r:tag huginn.m.r:+variable+
                                                  index))
              (maxf bindings-fill-pointer index)))))
      (let ((database (ensure (huginn.m.r:execution-state-database
                               execution-state)
                        (huginn.m.d:database))))
        (setf (huginn.m.r:execution-stack-cell-bindings-fill-pointer
               execution-stack-cell)
              bindings-fill-pointer

              (huginn.m.r:execution-stack-cell-goals execution-stack-cell)
              goals

              (huginn.m.r:execution-stack-cell-clauses execution-stack-cell)
              (huginn.m.d:matching-clauses* database execution-state
                                            (first goals))

              (huginn.m.r:execution-stack-cell-heap-fill-pointer
               execution-stack-cell)
              new-fill-pointer))
      nil)))
