(cl:in-package #:huginn.machine.representation)


(declaim (notinline select-clause))
(defun select-clause (predicate clauses)
  (declare (type list clauses))
  (iterate
    (for sub on clauses)
    (for first = (first sub))
    (when (funcall predicate first)
      (return-from select-clause (values first (rest sub))))
    (finally (return (values nil nil)))))


(defun more-clauses-p (clauses)
  (not (endp clauses)))


(declaim (notinline select-clause))
(defun clause-matches-goal-p (clause goal)
  cl-ds.utils:todo)


(defun answer (execution-state stack-cell)
  cl-ds.utils:todo)


(defun unfold (execution-state stack-cell)
  (declare (type execution-stack-cell stack-cell)
           (type execution-state execution-state))
  (let* ((goals (execution-stack-cell-goals stack-cell))
         (clauses (execution-state-clauses execution-state))
         (goal (first goals))
         (goal-cell (dereference-heap-pointer execution-state
                                              goal)))
    (iterate
      (unless (more-clauses-p clauses)
        (leave (pop-stack-cell execution-state stack-cell)))
      (for (values clause more) =
           (select-clause (lambda (clause)
                            (clause-matches-goal-p clause goal-cell))
                          clauses))
      (when (null clause)
        (leave (pop-stack-cell execution-state stack-cell)))
      (for bindings-fill-pointer = (clause-to-heap execution-state
                                                   stack-cell
                                                   clause))
      (for new-stack-cell = (push-stack-cell stack-cell clause
                                             bindings-fill-pointer))
      (prepare-unification-stack execution-state
                                 new-stack-cell
                                 goal)
      (for success-p = (unify execution-state new-stack-cell))
      (until success-p)
      (pop-stack-cell execution-state new-stack-cell)
      (setf clauses more)
      (finally (return new-stack-cell)))))
