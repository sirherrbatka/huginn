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


(-> clause-recursive-p (clause) boolean)
(defun clause-recursive-p (clause)
  (declare (optimize (speed 3)))
  (~> clause clause-recursive-call-position (> 0)))


(-> execution-stack-cell-recursive-call-position
    (execution-stack-cell)
    pointer)
(defun execution-stack-cell-recursive-call-position (cell)
  (declare (optimize (speed 3)))
  (~> cell
      execution-stack-cell-clause
      clause-recursive-call-position
      (+ (recursive-execution-stack-cell-heap-pointer cell))))


(-> recursive-execution-stack-cell-p
    (t)
    boolean)
(defun recursive-execution-stack-cell-p (cell)
  (and (execution-stack-cell-p cell)
       (~> cell
           execution-stack-cell-clause
           clause-recursive-p)))
