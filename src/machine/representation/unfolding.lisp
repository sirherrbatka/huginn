(cl:in-package #:huginn.machine.representation)


(defun unfold (execution-state stack-cell)
  (declare (type execution-stack-cell stack-cell)
           (type execution-state execution-state))
  (let* ((goals (execution-stack-cell-goals stack-cell))
         (clauses (execution-stack-cell-clauses stack-cell))
         (goal-pointer (first goals)))
    (iterate
      (for (values clause more) = (cl-ds:consume-front clauses))
      (unless more ; no more clauses matching current goal
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
      (unless head-unified-p
        (pop-stack-cell execution-state new-stack-cell)
        (next-iteration))
      (clause-body-to-heap execution-state new-stack-cell)
      (finish)
      (finally (return new-stack-cell)))))


(defun unfold-all (execution-state)
  (iterate
    (with stack = (execution-state-stack execution-state))
    (while (and (not (null stack))
                (execution-stack-cell-more-goals-p stack)))
    (setf stack (unfold execution-stack stack))
    (finally (return stack))))


(defun find-answer (execution-state)
  (if (~> execution-state execution-state-stack null)
      nil
      (let ((new-stack (unfold-all execution-state)))
        (setf (execution-stack-cell execution-state) new-stack)
        t)))
