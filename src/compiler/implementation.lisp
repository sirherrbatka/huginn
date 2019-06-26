(cl:in-package #:huginn.compiler)


(defun anonymus-variable-p (variable)
  (and (symbolp variable)
       (string= (symbol-name variable) "?")))


(defun variable-p (variable)
  (and (symbolp variable)
       (char= #\? (aref (symbol-name variable) 0))))


(defun expression-p (element)
  (consp element))


(defun gather-all-sublists (list &key
                                   (index 0)
                                   (result (make-hash-table :test 'eq)))
  (labels ((impl (sublist)
             (when (expression-p sublist)
               (let ((new-index (ensure (gethash sublist result)
                                  index)))
                 (when (eql new-index index)
                   (incf index (+ 2 (length sublist))))
                 (map nil #'impl sublist)))))
    (impl list)
    (values result index)))


(defun gather-all-variable-bindings
    (list &key
            (index 0)
            (result (make-hash-table :test 'eql)))
  (labels ((impl (x)
             (cond ((or (variable-p x)
                        (typep x 'huginn.m.r:word))
                    nil)
                   ((expression-p x)
                    (map nil #'impl x))
                   (t (let ((new-index (ensure (gethash x result)
                                         index)))
                        (when (eql new-index index)
                          (incf index)))))))
    (values result index)))


(defclass compilation-state (fundamental-compilation-state)
  ((%expressions-table :initarg :forms-table
                       :reader read-expressions-table)
   (%variables-table :initarg :variables-table
                     :reader read-variables-table)
   (%head :initarg :head
          :reader read-head)
   (%body :initarg :body
          :reader read-body))
  (:default-initargs
   :forms-table (make-hash-table :test 'eq)))


(defmethod expressions ((state compilation-state))
  (~> state
      read-expressions-table
      cl-ds:whole-range
      (cl-ds.alg:on-each #'car)))


(defmethod pointer-for-expression ((state compilation-state)
                                   expression)
  (check-type expression list)
  (~>> state
       read-expressions-table
       (gethash expression)))
