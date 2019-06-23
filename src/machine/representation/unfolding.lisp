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
         (clauses (execution-stack-cell-clauses stack-cell))
         (goal-pointer (first goals)))
    (iterate
      (for (values clause more) = (cl-ds:consume-front clauses))
      (unless more
        (leave (pop-stack-cell execution-state stack-cell)))
      (for bindings-fill-pointer = (clause-head-to-heap execution-state
                                                        stack-cell
                                                        clause))
      (for new-stack-cell = (push-stack-cell stack-cell clause
                                             bindings-fill-pointer))
      (prepare-unification-stack execution-state
                                 new-stack-cell
                                 goal-pointer)
      (for head-unified-p = (unify execution-state new-stack-cell))
      (when (and head-unified-p
              (let ((new-body-pointer
                      (execution-state-body-pointer new-stack-cell)))
                (declare (type pointer new-body-pointer))
                (clause-body-to-heap execution-state new-stack-cell)
                (prepare-unification-stack execution-state
                                           new-stack-cell
                                           new-body-pointer)
                (unify execution-state new-stack-cell)))
       (finish))
      (finally (return new-stack-cell)))))
