(cl:in-package #:huginn.machine.operations)


(locally
  (declare (optimize (speed 3) (debug 0) (safety 0)
                     (compilation-speed 0) (space 0)))

  (-> recursive-goal-p (huginn.m.r:execution-stack-cell
                        huginn.m.r:pointer
                        huginn.m.r:clause)
      boolean)
  (defun recursive-goal-p (stack-cell goal-pointer clause)
    (and
     (huginn.m.r:clause-recursive-p clause)
     (let ((stack-clause (huginn.m.r:execution-stack-cell-clause
                          stack-cell)))
       (and (eq stack-clause clause)
            (eql goal-pointer
                 (huginn.m.r:execution-stack-cell-recursive-goal-pointer stack-cell))))))


  (-> update-after-recursive-goal-satisfaction (huginn.m.r:execution-stack-cell)
      t)
  (defun update-after-recursive-goal-satisfaction (stack-cell)
    cl-ds.utils:todo)


  (-> handle-recursive-goal
      (huginn.m.r:execution-state huginn.m.r:execution-stack-cell)
      boolean)
  (defun handle-recursive-goal (execution-state stack-cell)
    (copy-recursive-head execution-state stack-cell)
    (if (unify-recursive-goal execution-state
                              stack-cell)
      (let* ((clause (huginn.m.r:execution-stack-cell-clause stack-cell))
             (clause-head-length (huginn.m.r:clause-body-pointer clause))
             (clause-body-length (huginn.m.r:clause-body-length clause))
             (pointer (huginn.m.r:execution-stack-cell-heap-pointer
                       stack-cell))
             (clause-end-pointer (+ clause-head-length
                                    clause-body-length
                                    pointer))
             (body-start (+ clause-head-length pointer))
             (body-end (+ body-start clause-body-length)))
        (declare (type huginn.m.r:pointer clause-end-pointer body-start body-end))
        (realize-heap-cells execution-state
                            body-start
                            body-end
                            clause-end-pointer
                            (the huginn.m.r:pointer
                                 (+ clause-end-pointer clause-head-length)))
        (copy-recursive-body execution-state stack-cell)
        (unify-head-with-recursive-head execution-state
                                        stack-cell)
        t)
      (progn
        ;; cleanup goes here
        nil)))


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
              cl-ds.utils:todo
              (assert (not (nth-value 1 (cl-ds:peek-front clauses))))
              ;; this function also performs cleanup if unification fails
              ;; returns the status of unification (T if success NIL if failure)
              (when (handle-recursive-goal execution-state
                                           stack-cell)
                ;; This will select next goal as current (if there is a next goal)
                ;; it will also replace the clauses object with new range, matching said goal
                (update-after-recursive-goal-satisfaction stack-cell)
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
