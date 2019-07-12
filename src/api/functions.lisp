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
         (total-size (length content))
         (body-pointer (huginn.c:body-pointer compilation))
         (variable-bindings (huginn.c:variable-bindings compilation)))
    (~> (huginn.m.r:make-clause
         :body-pointer body-pointer
         :variable-values variable-bindings
         :input (list* head goals)
         :content content
         :goal-pointers (~> compilation
                            (huginn.c:expressions body-pointer total-size)
                            (map '(vector huginn.m.r:pointer)
                                 (curry #'huginn.c:pointer-for-expression
                                        compilation)
                                 _)))
        add-clause)))


(defun ?- (&rest goals)
  (bind ((compilation (make-compilation-state (list* nil goals)))
         (content (huginn.c:content compilation (database)))
         (total-size (length content))
         (variable-bindings (huginn.c:variable-bindings compilation))
         (bindings-fill-pointer (length variable-bindings))
         (objects-mapping (iterate
                            (with table = (make-hash-table :test 'eql))
                            (break)
                            (for i from 0)
                            (for v in-vector variable-bindings)
                            (setf (gethash v table) i)
                            (finally (return table))))
         (expressions (huginn.c:expressions compilation
                                            0 total-size))
         (goals
          (map 'list
               (curry #'huginn.c:pointer-for-expression
                      compilation)
               expressions))
         (database (database))
         (execution-state
           (huginn.m.r:make-execution-state
            :database (database)
            :variable-bindings variable-bindings
            :heap content
            :objects-mapping objects-mapping))
         (clauses (huginn.m.d:matching-clauses database
                                               execution-state
                                               (first goals)))
         (stack (huginn.m.r:make-initial-execution-stack-cell goals
                                                              total-size
                                                              bindings-fill-pointer
                                                              clauses)))
    (setf (huginn.m.r:execution-state-stack execution-state) stack)
    (wrap-into-answers-range execution-state
                             compilation)))


(defun li (content)
  (check-type content list)
  (huginn.c:list-input content))
