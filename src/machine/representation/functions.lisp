(cl:in-package #:huginn.machine.representation)


(defun scan-heap-list (function execution-state pointer)
  (declare (optimize (speed 3) (safety 0)))
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
  (declare (optimize (speed 3) (safety 0)))
  (~> clause clause-recursive-goal-pointer (> 0)))


(-> execution-stack-cell-recursive-goal-pointer
    (execution-stack-cell)
    pointer)
(defun execution-stack-cell-recursive-goal-pointer (cell)
  (declare (optimize (speed 3) (safety 0)))
  (~> cell
      execution-stack-cell-clause
      clause-recursive-goal-pointer
      (+ (execution-stack-cell-heap-pointer cell))))


(-> recursive-execution-stack-cell-p
    (t)
    boolean)
(defun recursive-execution-stack-cell-p (cell)
  (declare (optimize (speed 3) (safety 0)))
  (and (execution-stack-cell-p cell)
       (~> cell
           execution-stack-cell-clause
           clause-recursive-p)))


(-> execution-stack-cell-same-clause-p
    (execution-stack-cell clause)
    boolean)
(defun execution-stack-cell-same-clause-p (stack-cell clause)
  (declare (optimize (speed 3) (safety 0)))
  (eq (execution-stack-cell-clause stack-cell)
      clause))
