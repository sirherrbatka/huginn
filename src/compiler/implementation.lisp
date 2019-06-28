(cl:in-package #:huginn.compiler)


#|
Clause can contain the below:
- variable (anonymus or not)
- some lisp object (value)
- other expression
- fixnum possible to inline (that is: word)
|#

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
       (inlined-fixnum-p element)))


(deftype value ()
  `(satisfies valuep))


(deftype expression ()
  `(satisfies expression))


(deftype variable ()
  `(satisfies variablep))


(deftype anonymus-variable ()
  `(satisfies anonymus-variable-p))


(defun walk-expression (expression
                         &key
                           (on-expression #'identity)
                           (on-value #'identity)
                           (on-variable #'identity)
                           (on-inlined-fixnum #'identity)
                           (on-anonymus-variable on-variable))
  "Scan expression, execute callbacks on expression elements. Needed for gather functions."
  (labels ((impl (elt parent)
             (cond ((anonymus-variable-p elt)
                    (funcall on-anonymus-variable elt parent))
                   ((variablep elt)
                    (funcall on-variable elt parent))
                   ((valuep elt)
                    (funcall on-value elt parent))
                   ((inlined-fixnum-p elt)
                    (funcall on-inlined-fixnum elt parent))
                   ((expressionp elt)
                    (funcall on-expression elt parent)
                    (iterate
                      (for sub in elt)
                      (impl elt sub))))))
    (impl expression nil)))


(defun gather-all-expressions (list &key
                                      (index 0)
                                      (result (make-hash-table :test 'eq)))
  (check-type list list)
  (check-type index huginn.m.r:pointer)
  (check-type result hash-table)
  (walk-expression
   list
   :on-expression (lambda (sublist parent)
                    (declare (ignore parent))
                    (let ((new-index (ensure (gethash sublist result) index)))
                      (when (eql new-index index)
                        (incf index (+ 2 (length sublist)))))))
  (values result index))


(defun gather-all-values
    (list &key
            (index 0)
            (result (make-hash-table :test 'eql)))
  (check-type list list)
  (check-type index huginn.m.r:pointer)
  (check-type result hash-table)
  (walk-expression
   list
   :on-value (lambda (x parent)
               (declare (ignore parent))
               (let ((new-index (ensure (gethash x result) index)))
                 (when (eql new-index index)
                   (incf index)))))
  (values result index))


(defun gather-all-variables
    (list expressions-table &key (result (make-hash-table :test 'eq)))
  (check-type list list)
  (check-type expressions-table hash-table)
  (flet ((callback (variable parent)
           (assert (not (null parent)))
           (let ((expression-pointer (gethash parent expressions-table)))
             (ensure (gethash variable result)
               (+ 2 (position variable parent)
                  expression-pointer)))))
    (walk-expression
     list
     :on-anonymus-variable #'identity
     :on-variable #'callback))
  result)


(defclass compilation-state (fundamental-compilation-state)
  ((%expressions-table :initarg :forms-table
                       :reader read-expressions-table)
   (%values-table :initarg :values-table
                  :reader read-values-table)
   (%variables-table :initarg :variables-table
                     :reader read-variables-table)
   (%content-length :initarg :content-length
                    :reader read-content-length
                    :reader cells-count)
   (%body-pointer :initarg :body-pointer
                  :reader body-pointer
                  :reader read-body-pointer)
   (%head :initarg :head
          :reader head
          :reader read-head)
   (%body :initarg :body
          :reader body
          :reader read-body))
  (:default-initargs
   :forms-table (make-hash-table :test 'eq)))

#|
This representation is pretty much the same as one used by norvig in the PAIP.
|#

(deftype predicate ()
  '(and (not variable) symbol))


(deftype clause ()
  'list)


(defun clause-head (clause)
  (check-type clause clause)
  (car clause))


(defun clause-body (clause)
  (check-type clause clause)
  (cdr clause))


(defun clause-head-predicate (head)
  (check-type head clause)
  (first head))


(defun clause-predicate (clause)
  (check-type clause clause)
  (~> clause clause-head clause-head-predicate))


(defun unique-index (&key (test 'eq))
  (let ((table (make-hash-table :test test))
        (index 0))
    (values
     (lambda (elt)
       (let* ((fresh-index (ensure (gethash elt table) index))
              (new (eql fresh-index index)))
         (when new (incf index))
         (values index new)))
     table)))


(defmethod content ((state compilation-state))
  (bind ((result (make-array (cells-count state)
                             :element-type 'huginn.m.r:cell))
         ((:slots %head %body) state)
         (expressions-table (make-hash-table :test 'eq))
         (variables-table (make-hash-table :test 'eq))
         (variables-index 0)
         (values-table (unique-index :test 'eql))
         (end 0)
         ((:flet add (index item))
          (setf (aref result index) item))
         ((:labels scan (elt pointer))
          (cond ((expressionp elt)
                 (let ((length (length elt))
                       (expression-pointer
                         (ensure (gethash elt expressions-table)
                           end))
                       (old-end end))
                   (when (= old-end expression-pointer)
                     (incf end (+ 2 length))
                     (add expression-pointer
                          (huginn.m.r:tag huginn.m.r:+expression+
                                          expression-pointer))
                     (add (1+ expression-pointer) length)
                     (iterate
                       (for sub in elt)
                       (for i from (+ expression-pointer 2))
                       (add i (scan sub i))))
                   (huginn.m.r:make-reference expression-pointer)))
                ((anonymus-variable-p elt)
                 (huginn.m.r:tag huginn.m.r:+variable+ 0))
                ((variablep elt)
                 (bind (((new-pointer new-index)
                         (ensure (gethash variables-table elt)
                           (list* pointer variables-index))))
                   (declare (ignore new-index))
                   (if (= new-pointer pointer)
                     (prog1
                       (huginn.m.r:tag huginn.m.r:+variable+
                                       variables-index)
                       (incf variables-index))
                     (huginn.m.r:make-reference new-pointer))))
                ((valuep elt)
                 (huginn.m.r:tag huginn.m.r:+variable+
                                 (funcall values-table elt)))
                ((inlined-fixnum-p elt)
                 (huginn.m.r:tag huginn.m.r:+fixnum+
                                 elt)))))
    (scan %head 0)
    (scan %body end)
    result))


(defmethod make-compilation-state ((class (eql 'compilation-state))
                                   clause)
  (check-type clause clause)
  (let* ((head (clause-head clause))
         (body (clause-body clause))
         (predicate (clause-predicate clause))
         (expressions-table (make-hash-table :test 'eq))
         (variables-table (make-hash-table :test 'eq))
         (values-table (make-hash-table :test 'eql))
         (body-pointer 0)
         (content-length 0))
    (check-type predicate predicate)
    (check-type head clause)
    (check-type body clause)
    (setf body-pointer (nth-value 1 (gather-all-expressions
                                     head :result expressions-table))
          content-length (nth-value 1 (gather-all-expressions
                                       body
                                       :result expressions-table
                                       :index body-pointer)))
    (gather-all-values head :result values-table)
    (gather-all-values body
                       :result values-table
                       :index (hash-table-count values-table))
    (gather-all-variables head expressions-table
                          :result variables-table)
    (gather-all-variables body expressions-table
                          :result variables-table)
    (make 'compilation-state
          :head head
          :body body
          :variables-table variables-table
          :values-table values-table
          :expressions-table expressions-table
          :content-length content-length
          :body-pointer body-pointer)))


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


(defmethod predicate ((state compilation-state))
  (~> state head clause-head-predicate))
