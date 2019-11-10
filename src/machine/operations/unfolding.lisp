(cl:in-package #:huginn.machine.operations)


(locally
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0) (space 0)))

  (declaim (inline unfold))
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

  #|
  When unfolding recursive stack-cell, recursive goal is not present
  in the goals list. Instead such goal needs to be matched before other goals.
  |#
  (declaim (inline recursive-unfold))
  (defun recursive-unfold (execution-state stack-cell)
    (declare (type huginn.m.r:execution-stack-cell stack-cell)
             (type huginn.m.r:execution-state execution-state))
    (when (huginn.m.r:recursive-execution-stack-cell-p stack-cell)
      cl-ds.utils:todo)
    (unfold execution-state stack-cell))


  (declaim (notinline unfold-all))
  (defun unfold-all (execution-state)
    (declare (type huginn.m.r:execution-state execution-state))
    (iterate
      (with stack = (huginn.m.r:execution-state-stack execution-state))
      (setf stack (recursive-unfold execution-state stack)
            (huginn.m.r:execution-state-stack execution-state) stack)
      (while (and (not (null stack))
                  (huginn.m.r:execution-stack-cell-more-goals-p stack)))
      (finally (return stack))))


  (declaim (notinline find-answer))
  (defun find-answer (execution-state)
    (declare (type huginn.m.r:execution-state execution-state))
    (if (~> execution-state huginn.m.r:execution-state-stack null)
        nil
        (~> execution-state unfold-all null not))))
