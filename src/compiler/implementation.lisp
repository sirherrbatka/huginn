(cl:in-package #:huginn.compiler)


(defun anonymus-variable-p (variable)
  (and (symbolp variable)
       (string= (symbol-name variable) "?")))


(defun variablep (variable)
  (and (symbolp variable)
       (char= #\? (aref (symbol-name variable) 0))))


(defun expressionp (element)
  (consp element))


(defun inlined-fixnum-p (element)
  (typep element 'huginn.m.r:word))


(defun valuep (element)
  (nor (expressionp element)
       (variablep element)
       (inlined-fixnump element)))


(defun walk-expressions (expression
                         &key
                           (on-expression #'identity)
                           (on-value #'identity)
                           (on-variable #'identity)
                           (on-inlined-fixnum #'identity))
  (labels ((impl (elt parent)
             (cond ((variablep elt)
                    (funcall on-variable elt parent))
                   ((valuep elt)
                    (funcall on-value elt parent))
                   ((inlined-fixnum-p elt)
                    (funcall on-inlined-fixnum elt parent))
                   ((expression-p elt)
                    (funcall on-expression elt parent)
                    (iterate
                      (for sub in elt)
                      (impl elt sub))))))
    (impl expression)))


(defun gather-all-expressions (list &key
                                      (index 0)
                                      (result (make-hash-table :test 'eq)))
  (walk-expressions
   list
   :on-expression (lambda (sublist parent)
                    (declare (ignore parent))
                    (let ((new-index (ensure (gethash sublist result)
                                       index)))
                      (when (eql new-index index)
                        (incf index (+ 2 (length sublist))))
                      (map nil #'impl sublist))))
  (values result index))


(defun gather-all-values
    (list &key
            (index 0)
            (result (make-hash-table :test 'eql)))
  (walk-expressions
   list
   :on-value (lambda (x parent)
               (declare (ignore parent))
               (let ((new-index (ensure (gethash x result)
                                  index)))
                 (when (eql new-index index)
                   (incf index)))))
  (values result index))


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
