(cl:in-package #:huginn.machine.operations)


(defun matching-clauses (execution-state goal-pointer)
  (declare (type huginn.m.r:execution-state execution-state)
           (type huginn.m.r:pointer goal-pointer))
  (let ((heap (huginn.m.r:execution-state-heap execution-state)))
    (macrolet ((deref (i)
                 `(aref heap ,i)))
      (~> execution-state
          huginn.m.r:execution-state-clauses
          cl-ds:whole-range
          (cl-ds.alg:only
           (lambda (clause)
             (let ((content (huginn.m.r:clause-content clause)))
               (and (eql (aref content 1)
                         (deref (1+ goal-pointer))) ; same arity
                    (let ((clause-predicate (aref content 2))
                          (goal-predicate (deref (+ 2 goal-pointer))))
                      (declare (ignore clause-predicate goal-predicate))
                      ;; I need another tag type: predicate and index those indepenendtly from everything else
                      t)))))))))
