(cl:in-package #:huginn.machine.operations)


(defun unwind-heap-cells-trail (execution-state trail)
  (declare (type (cl-ds.utils:extendable-vector fixnum) trail)
           (type huginn.m.r:execution-state execution-state)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (let ((heap (huginn.m.r:execution-state-heap execution-state)))
    (iterate
      (declare (type fixnum address i)
               (type huginn.m.r:cell old-value))
      (for i from 0 by 2 below (length trail))
      (for address = (aref trail i))
      (for old-value = (aref trail (1+ i)))
      (setf (aref heap address) old-value))))


(defun unbind-range (execution-state from below)
  (declare (type huginn.m.r:execution-state execution-state)
           (cl-ds.utils:index from below))
  (iterate
    (declare (type fixnum i))
    (with objects-mapping =
          (huginn.m.r:execution-state-objects-mapping execution-state))
    (with bindings =
          (huginn.m.r:execution-state-variable-bindings execution-state))
    (for i from from below below)
    (remhash (aref bindings i) objects-mapping)))


(defun pop-stack-cell (execution-state execution-stack-cell)
  "Pop top stack-cell, adjust execution-state by clearing objects."
  (declare (type huginn.m.r:execution-stack-cell execution-stack-cell)
           (type huginn.m.r:execution-state execution-state))
  (~>> execution-stack-cell
       huginn.m.r:execution-stack-cell-heap-cells-trail
       (unwind-heap-cells-trail execution-state))
  (unbind-range execution-state
                (~> execution-stack-cell
                    huginn.m.r:execution-stack-cell-previous-cell
                    huginn.m.r:execution-stack-cell-bindings-fill-pointer)
                (~> execution-stack-cell
                    huginn.m.r:execution-stack-cell-bindings-fill-pointer))
  (huginn.m.r:execution-stack-cell-previous-cell execution-stack-cell))
