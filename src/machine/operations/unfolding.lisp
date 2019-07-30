(cl:in-package #:huginn.machine.operations)


(with-compilation-unit ()
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0) (space 0)))

  (defun unfold (execution-state stack-cell)
    (declare (type huginn.m.r:execution-stack-cell stack-cell)
             (type huginn.m.r:execution-state execution-state))
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
    (declare (type huginn.m.r:execution-state execution-state))
    (iterate
      (with stack = (huginn.m.r:execution-state-stack execution-state))
      (setf stack (unfold execution-state stack)
            (huginn.m.r:execution-state-stack execution-state) stack)
      (while (and (not (null stack))
                  (huginn.m.r:execution-stack-cell-more-goals-p stack)))
      (finally (return stack))))


  (defun find-answer (execution-state)
    (declare (type huginn.m.r:execution-state execution-state))
    (if (~> execution-state huginn.m.r:execution-state-stack null)
        nil
        (~> execution-state unfold-all null not))))
