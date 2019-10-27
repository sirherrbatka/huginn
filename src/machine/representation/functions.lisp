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


(defun save-head-state (cell)
  "Saves the current state of the state. This should be done after unification of head, but before copying the body. Needed only for the cells originating from the recursive clauses."
  (declare (type execution-stack-cell cell))
  (when (recursive-execution-stack-cell-p cell)
    (setf (recursive-execution-stack-cell-head-unwind-trail-pointer cell)
          (recursive-execution-stack-cell-unwind-trail-pointer cell)

          (recursive-execution-stack-cell-head-heap-fill-pointer cell)
          (recursive-execution-stack-cell-heap-fill-pointer cell)))
  cell)
