(cl:in-package #:huginn.machine.representation)


(defun matching-clauses (execution-state goal-pointer)
  cl-ds.utils:todo)


;; this should be performed only after clause was already proven (and therefore copied to heap.
;; Code assumes that next stack cell is located DIRECTLY after the current one on the heap
(defun push-stack-cell (execution-stack-cell clause
                        bindings-fill-pointer execution-state)
  "Constructs new stack-cell based on the clause, trail and execution-stack-cell assuming that this cell is constructed from the first goal of the execution-stack-cell after data was already placed to the heap."
  (declare (type clause clause)
           (type execution-stack-cell execution-stack-cell)
           (type fixnum bindings-fill-pointer))
  (let* ((fill-pointer (execution-stack-cell-heap-fill-pointer
                        execution-stack-cell))
         (new-fill-pointer (+ fill-pointer (clause-body-length clause)))
         (goals (~>> execution-stack-cell
                     execution-stack-cell-goals
                     rest
                     (clause-goals clause fill-pointer))))
    (declare (type cl-ds.utils:index new-fill-pointer fill-pointer))
    (make-execution-stack-cell
     :previous-cell execution-stack-cell
     :heap-fill-pointer new-fill-pointer
     :goals goals
     :clauses (matching-clauses execution-state
                                (first goals))
     :clause clause
     :heap-pointer fill-pointer
     :bindings-fill-pointer bindings-fill-pointer)))


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


(defun clause-head-to-heap (execution-state execution-stack-cell clause)
  (declare (optimize (speed 3))
           (type execution-state execution-state)
           (type execution-stack-cell execution-stack-cell)
           (type clause clause))
  (let* ((content (clause-content clause))
         (head-length (clause-body-pointer clause))
         (variable-values (clause-variable-values clause))
         (fill-pointer
           (execution-stack-cell-heap-fill-pointer execution-stack-cell))
         (new-fill-pointer (+ fill-pointer head-length)))
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
        (for j from 0)
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
          (unless (variable-unbound-p cell)
            (bind ((object (aref variable-values word)))
              (assert (value-bound-p object))
              (let* ((new-index (1+ bindings-fill-pointer))
                     (index (index-object execution-state
                                          object
                                          new-index)))
                (declare (type fixnum index new-index))
                (setf (aref heap i) (tag +variable+ index))
                (maxf bindings-fill-pointer index))))))
      bindings-fill-pointer))))


(defun clause-body-to-heap (execution-state execution-stack-cell)
  "Copies clause body to heap. Will extend variable bindings in the state (or fail and return nil if can't do so). Will return: new trail, new bindings-heap-pointer, and success-info. To unroll changes do the execution-state performed by this function it is required to both unwind-variable-bindings-trail and unbind-range"
  (declare (optimize (speed 3))
           (type execution-state execution-state)
           (type execution-stack-cell execution-stack-cell))
  (let* ((clause (execution-stack-cell-clause execution-stack-cell))
         (body-length (clause-body-length clause))
         (content (clause-content clause))
         (variable-values (clause-variable-values clause))
         (fill-pointer
           (execution-stack-cell-heap-fill-pointer execution-stack-cell))
         (new-fill-pointer (+ fill-pointer body-length)))
    (declare (type fixnum new-fill-pointer))
    (expand-state-heap execution-state new-fill-pointer)
    (let* ((heap (execution-state-heap execution-state))
           (body-pointer (~> execution-stack-cell
                             execution-stack-cell-clause
                             clause-body-pointer))
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
          (unless (variable-unbound-p cell)
            (bind ((object (aref variable-values word)))
              (assert (value-bound-p object))
              (let* ((new-index (1+ bindings-fill-pointer))
                     (index (index-object execution-state
                                          object
                                          new-index)))
                (declare (type fixnum index new-index))
                (setf (aref heap i) (tag +variable+ index))
                (maxf bindings-fill-pointer index))))))
      (setf (execution-stack-cell-bindings-fill-pointer execution-stack-cell)
            bindings-fill-pointer
            (execution-stack-cell-heap-fill-pointer execution-stack-cell)
            new-fill-pointer)
      nil)))
