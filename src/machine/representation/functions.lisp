(cl:in-package #:huginn.machine.representation)


(defun scan-heap-list (function execution-state pointer)
  (iterate
    (for cell = (dereference-heap-pointer execution-state pointer t))
    (tag-case (cell)
      :list-rest
      (let ((new-pointer (detag cell)))
        (if (zerop new-pointer)
            (finish)
            (progn (setf pointer new-pointer)
                   (next-iteration))))
      :list-end (finish))
    (funcall function pointer cell)
    (incf pointer)
    (finally (return execution-state))))
