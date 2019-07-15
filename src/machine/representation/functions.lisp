(cl:in-package #:huginn.machine.representation)


(defun scan-heap-list (function execution-state pointer)
  (iterate
    (with heap = (execution-state-heap execution-state))
    (for cell = (aref heap pointer))
    (tag-case (cell)
      :list-rest (progn (setf pointer (detag cell))
                        (next-iteration))
      :list-end (finish))
    (funcall function pointer cell)
    (incf pointer)
    (finally (return execution-state))))
