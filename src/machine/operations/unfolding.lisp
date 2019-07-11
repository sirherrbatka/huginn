(cl:in-package #:huginn.machine.operations)


(defun unfold (execution-state stack-cell)
  (declare (type huginn.m.r:execution-stack-cell stack-cell)
           (type huginn.m.r:execution-state execution-state)
           (optimize (debug 3)))
  (let* ((goals (huginn.m.r:execution-stack-cell-goals stack-cell))
         (clauses (huginn.m.r:execution-stack-cell-clauses stack-cell))
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
      (for head-unified-p = (unify execution-state
                                   new-stack-cell
                                   goal-pointer))
      (unless head-unified-p
        (pop-stack-cell execution-state new-stack-cell)
        (next-iteration))
      (clause-body-to-heap execution-state new-stack-cell)
      (finish)
      (finally (return new-stack-cell)))))


(defun unfold-all (execution-state)
  (iterate
    (with stack = (huginn.m.r:execution-state-stack execution-state))
    (while (and (not (null stack))
                (huginn.m.r:execution-stack-cell-more-goals-p stack)))
    (setf stack (unfold execution-state stack)
          (huginn.m.r:execution-state-stack execution-state) stack)
    (finally (return stack))))


(defun find-answer (execution-state)
  (if (~> execution-state huginn.m.r:execution-state-stack null)
      nil
      (not (null (unfold-all execution-state)))))
