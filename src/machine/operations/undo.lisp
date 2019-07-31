(cl:in-package #:huginn.machine.operations)


(defun unwind-heap-cells-trail (execution-state execution-stack-cell)
  (declare (type huginn.m.r:execution-stack-cell execution-stack-cell)
           (type huginn.m.r:execution-state execution-state)
           (optimize (speed 3) (safety 0) (space 0) (debug 0)))
  (let ((heap (huginn.m.r:execution-state-heap execution-state))
        (trail (huginn.m.r:execution-state-unwind-trail execution-state))
        (trail-start
          (if-let ((prev (huginn.m.r:execution-stack-cell-previous-cell
                          execution-stack-cell)))
            (huginn.m.r:execution-stack-cell-unwind-trail-pointer prev)
            0))
        (trail-end (huginn.m.r:execution-stack-cell-unwind-trail-pointer
                    execution-stack-cell)))
    (iterate
      (declare (type fixnum address i)
               (type huginn.m.r:cell old-value))
      (for i from trail-start by 2 below trail-end)
      (for address = (aref trail i))
      (for old-value = (aref trail (1+ i)))
      (setf (aref heap address) old-value))))


(defun unbind-range (execution-state from below)
  (declare (type huginn.m.r:execution-state execution-state)
           (cl-ds.utils:index from below))
  (iterate
    (declare (type fixnum i))
    (with objects-mapping =
          (huginn.m.r:execution-state-objects-mapping execution-state))
    (with bindings =
          (huginn.m.r:execution-state-variable-bindings execution-state))
    (for i from from below below)
    (remhash (aref bindings i) objects-mapping)))


(defun pop-stack-cell (execution-state execution-stack-cell)
  "Pop top stack-cell, adjust execution-state by clearing changes."
  (declare (type huginn.m.r:execution-stack-cell execution-stack-cell)
           (type huginn.m.r:execution-state execution-state)
           (optimize (speed 3) (safety 0)))
  (unwind-heap-cells-trail execution-state execution-stack-cell)
  (let* ((prev-cell (huginn.m.r:execution-stack-cell-previous-cell execution-stack-cell))
         (lower-bound (if (null prev-cell)
                          0
                          (huginn.m.r:execution-stack-cell-bindings-fill-pointer
                           prev-cell))))
    (unbind-range execution-state
                  lower-bound
                  (~> execution-stack-cell
                      huginn.m.r:execution-stack-cell-bindings-fill-pointer)))
  (huginn.m.r:execution-stack-cell-previous-cell execution-stack-cell))


(defun pop-stack-cells-until-goal (execution-state)
  (iterate
    (for stack = (huginn.m.r:execution-state-stack execution-state))
    (while (and (not (null stack))
                (not (huginn.m.r:execution-stack-cell-more-goals-p stack))))
    (setf (huginn.m.r:execution-state-stack execution-state)
          (pop-stack-cell execution-state stack))
    (finally (return execution-state))))


(defun possible-answer (stack)
  (nth-value (~>> stack huginn.m.r:execution-stack-cell-clauses
                  cl-ds:peek-front)
             1))


(defun pop-stack-cells-until-possible-answer (execution-state)
  (iterate
    (with stack = (huginn.m.r:execution-state-stack execution-state))
    (until (possible-answer stack))
    (setf stack (pop-stack-cell execution-state stack)
          (huginn.m.r:execution-state-stack execution-state) stack)
    (while (not (null stack)))
    (finally (return execution-state))))
