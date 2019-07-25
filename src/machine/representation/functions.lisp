(cl:in-package #:huginn.machine.representation)


(defun scan-heap-list (function execution-state pointer)
  (iterate
    (with heap = (execution-state-heap execution-state))
    (for actual-pointer = (follow-pointer execution-state pointer t))
    (for cell = (aref heap actual-pointer))
    (tag-case (cell)
      :list-rest
      (let ((new-pointer (detag cell)))
        (if (zerop new-pointer)
            (finish)
            (progn (setf pointer new-pointer)
                   (next-iteration))))
      :list-end (finish))
    (funcall function actual-pointer cell)
    (incf pointer)
    (finally (return execution-state))))
