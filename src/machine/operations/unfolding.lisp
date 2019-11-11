(cl:in-package #:huginn.machine.operations)


(locally
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0) (space 0)))

  (-> recursive-goal-p (huginn.m.r:execution-stack-cell
                        huginn.m.r:pointer
                        huginn.m.r:clause)
      boolean)
  (defun recursive-goal-p (stack-cell goal-pointer clause)
    (let ((stack-clause (huginn.m.r:execution-stack-cell-clause
                         stack-cell)))
      (and stack-clause
           (eql goal-pointer
                (huginn.m.r:clause-recursive-call-position stack-clause))
           (eql stack-clause clause))))


  (declaim (inline unfold))
  (defun unfold (execution-state stack-cell)
    (declare (type huginn.m.r:execution-stack-cell stack-cell)
             (type huginn.m.r:execution-state execution-state))
    (let* ((goals (huginn.m.r:execution-stack-cell-goals stack-cell))
           (clauses (huginn.m.r:execution-stack-cell-clauses stack-cell))
           (goal-pointer (first goals)))
      (declare (type huginn.m.r:pointer goal-pointer)
               (type list goals))
      (iterate
        (for (values clause more) = (cl-ds:consume-front clauses))
        (unless more ; no more clauses matching current goal
          (leave (pop-stack-cell execution-state stack-cell)))
        (for bindings-fill-pointer = (clause-head-to-heap execution-state
                                                          stack-cell
                                                          clause))
        (if (recursive-goal-p stack-cell
                              goal-pointer
                              clause)
            (progn
              ;; because in the case of the success clauses range will be reset, recursive clause MUST be the one yielded for check
              (assert (not (nth-value 1 (cl-ds:peek-front clauses))))
              ;; this function also performs cleanup if unification fails
              (dereference-body execution-state stack-cell)
              (~> stack-cell
                  huginn.m.r:execution-stack-cell-previous-cell
                  (clause-head-to-heap execution-state _ clause))
              (when (unify-head-with-recursive-goal execution-state
                                                    stack-cell)
                (dereference-head execution-state stack-cell)
                (reset-pointers-to-head execution-state stack-cell)
                (let ((unified-p
                        (~>> stack-cell
                             huginn.m.r:execution-stack-cell-goal-pointer
                             (unify execution-state stack-cell))))
                  (assert unified-p))
                (clause-body-to-heap execution-state stack-cell)
                (cl-ds:reset! clauses)
                ;; since no new stack frame was actually allocated, return the current one
                (leave stack-cell))
              (next-iteration))
            (let* ((new-stack-cell (push-stack-cell stack-cell clause
                                                    bindings-fill-pointer))
                   (head-unified-p (unify execution-state
                                          new-stack-cell
                                          goal-pointer)))
              (unless head-unified-p
                (pop-stack-cell execution-state new-stack-cell)
                (next-iteration))
              (clause-body-to-heap execution-state new-stack-cell)
              (leave new-stack-cell))))))


  (declaim (notinline unfold-all))
  (defun unfold-all (execution-state)
    (declare (type huginn.m.r:execution-state execution-state))
    (iterate
      (with stack = (huginn.m.r:execution-state-stack execution-state))
      (setf stack (unfold execution-state stack)
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
