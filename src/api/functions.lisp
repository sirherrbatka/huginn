(cl:in-package #:huginn)


(defun compiler ()
  *compiler*)


(defun database ()
  *database*)


(defun (setf database) (database)
  (setf *database* database))


(defun (setf compiler) (compiler)
  (setf *compiler* compiler))


(defun add-clause (clause)
  (huginn.m.d:add-clause (database) clause)
  t)


(defun clauses ()
  (huginn.m.d:clauses (database)))


(defun clear ()
  (huginn.m.d:clear (database))
  t)


(defun make-database (class &optional default &rest more-options)
  (lret ((result (apply #'huginn.m.d:make-database class more-options)))
    (when default
      (setf *database* result))))


(defun make-compilation-state (content)
  (huginn.c:make-compilation-state (compiler) content))


(defun <- (head &rest goals)
  (let* ((compilation (make-compilation-state (list* head goals)))
         (content (huginn.c:content compilation (database)))
         (body-pointer (huginn.c:body-pointer compilation))
         (variable-bindings (huginn.c:variable-bindings compilation))
         (copy-head-function (huginn.c:optimized-relocate-cells-function
                              compilation
                              (database)
                              0
                              body-pointer))
         (copy-body-function (huginn.c:optimized-relocate-cells-function
                              compilation
                              (database)
                              body-pointer
                              (length content))))
    (~> (huginn.m.r:make-clause
         :copy-head-function copy-head-function
         :copy-body-function copy-body-function
         :body-pointer body-pointer
         :variable-values variable-bindings
         :input (list* head goals)
         :content content
         :goal-pointers (map '(vector huginn.m.r:pointer)
                             (curry #'huginn.c:pointer-for-expression
                                    compilation)
                             goals))
        add-clause)))


(defun ?- (&rest goals)
  (bind ((compilation (make-compilation-state (list* nil goals)))
         (content (huginn.c:content compilation (database)))
         (total-size (length content))
         (variable-bindings (huginn.c:variable-bindings compilation))
         (bindings-fill-pointer (length variable-bindings))
         (objects-mapping (iterate
                            (with table = (make-hash-table :test 'eql))
                            (for i from 0)
                            (for v in-vector variable-bindings)
                            (setf (gethash v table) i)
                            (finally (return table))))
         (goal-pointers
          (map 'list
               (curry #'huginn.c:pointer-for-expression
                      compilation)
               goals))
         (database (database))
         (execution-state
           (huginn.m.r:make-execution-state
            :database (database)
            :variable-bindings variable-bindings
            :heap content
            :objects-mapping objects-mapping))
         (clauses (huginn.m.d:matching-clauses database
                                               execution-state
                                               (first goal-pointers)))
         (stack (huginn.m.r:make-initial-execution-stack-cell
                 goal-pointers total-size bindings-fill-pointer clauses)))
    (setf (huginn.m.r:execution-state-stack execution-state) stack)
    (wrap-into-answers-range execution-state
                             compilation)))


(defun li (content)
  (check-type content list)
  (huginn.c:list-input content))
