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


(defun <- (&rest clause)
  (let* ((compilation (make-compilation-state clause))
         (content (huginn.c:content compilation))
         (total-size (length content))
         (body-pointer (huginn.c:body-pointer compilation))
         (variable-bindings (huginn.c:variable-bindings compilation)))
    (~> (huginn.m.r:make-clause
         :body-pointer body-pointer
         :variable-values variable-bindings
         :content content
         :goal-pointers (~> compilation
                         (huginn.c:expressions body-pointer total-size)
                         (map '(vector huginn.m.r:pointer)
                              (curry #'huginn.c:pointer-for-expression
                                     compilation)
                              _)))
        add-clause)))
