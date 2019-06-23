(cl:in-package #:huginn.machine.representation)


(defun unwind-heap-cells-trail (execution-state trail)
  (declare (type (cl-ds.utils:extendable-vector fixnum) trail)
           (type execution-state execution-state)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (let ((heap (execution-state-heap execution-state)))
    (iterate
      (declare (type fixnum address i)
               (type cell old-value))
      (for i from 0 by 2 below (length trail))
      (for address = (aref trail i))
      (for old-value = (aref trail (1+ i)))
      (setf (aref heap address) old-value))))


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
  (~>> execution-stack-cell
       execution-stack-cell-heap-cells-trail
       (unwind-heap-cells-trail execution-state))
  (execution-stack-cell-previous-cell execution-stack-cell))
