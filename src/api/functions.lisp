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
  (declare (optimize (debug 3)))
  (let* ((compilation (make-compilation-state (list* head goals)))
         (content (huginn.c:content compilation (database)))
         (body-pointer (huginn.c:body-pointer compilation))
         (variable-bindings (huginn.c:variable-bindings compilation))
         (database (database))
         (unify-head-function (when *compile*
                                (huginn.c:optimized-unify-head-function
                                 compilation
                                 database)))
         (copy-head-function (when *compile*
                               (huginn.c:optimized-relocate-cells-function
                                compilation
                                database
                                0
                                body-pointer)))
         (copy-body-function (when *compile*
                               (huginn.c:optimized-relocate-cells-function
                                compilation
                                database
                                body-pointer
                                (length content))))
         (recursive-goal-pointer (huginn.c:pointer-for-recursive-goal
                                  compilation))
         (goal-pointers (map '(vector huginn.m.r:pointer)
                             (curry #'huginn.c:pointer-for-expression
                                    compilation)
                             goals)))
    (~> (huginn.m.r:make-clause
         :copy-head-function copy-head-function
         :copy-body-function copy-body-function
         :body-pointer body-pointer
         :unify-head-function unify-head-function
         :variable-values variable-bindings
         :input (list* head goals)
         :content content
         ;; recursive goal can't show up as a head (because it is a goal) s' ot can't have value equal to 0. Therefore 0 is used as indicator for lack of the recursive goal
         :recursive-goal-pointer (or recursive-goal-pointer 0)
         :goal-pointers goal-pointers)
        add-clause)))


(defun ?- (&rest goals)
  (bind ((compilation (make-compilation-state (list* nil goals)))
         (resources (or *shared-resources* (make-shared-resources)))
         (total-size (huginn.c:cells-count compilation))
         (content (huginn.c:content compilation (database)
                                    (shared-resources-heap resources)))
         (variable-bindings (huginn.c:variable-bindings compilation))
         (bindings-fill-pointer (length variable-bindings))
         (objects-mapping (iterate
                            (with table = (make-hash-table :test 'eq))
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
            :unification-stack (shared-resources-unification-stack resources)
            :unwind-trail (shared-resources-unwind-trail resources)
            :objects-database (huginn.machine.database:objects-database database)
            :objects-mapping objects-mapping))
         (clauses (huginn.m.d:matching-clauses database
                                               execution-state
                                               (first goal-pointers)
                                               nil))
         (stack (huginn.m.r:make-initial-execution-stack-cell
                 goal-pointers total-size bindings-fill-pointer clauses)))
    (setf (huginn.m.r:execution-state-stack execution-state) stack)
    (wrap-into-answers-range execution-state
                             compilation)))


(defun li (content)
  (check-type content list)
  (huginn.c:list-input content))


(defun recur (content)
  (check-type content list)
  (huginn.c:recursive-call content))


(defun register-object (object)
  (huginn.machine.database:register-object (database) object))
