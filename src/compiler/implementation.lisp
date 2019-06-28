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
  ((%values-table :initarg :values-table
                  :reader read-values-table)
   (%flat-representation :initarg :flat-representation
                         :reader read-flat-representation
                         :reader expressions)
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
     (lambda (elt &rest all)
       (declare (ignore all))
       (let* ((fresh-index (ensure (gethash elt table) index))
              (new (eql fresh-index index)))
         (when new (incf index))
         (values index new)))
     table)))


(defmethod content ((state compilation-state))
  (bind ((result (make-array (cells-count state)
                             :element-type 'huginn.m.r:cell))
         ((:slots %flat-representation) state)
         (index 0)
         ((:flet add (item))
          (setf (aref result index) item)
          (incf index)))
    (iterate
      (for elt in-vector %flat-representation)
      (cond ((anonymus-variable-p elt)
             (add (huginn.m.r:tag huginn.m.r:+variable+ 0)))
            ((variablep elt)
             (let ((pointer (pointer-for-variable state elt)))
               (assert (not (null pointer)))
               (if (eql pointer index)
                   (add (huginn.m.r:tag huginn.m.r:+variable+ 0))
                   (add (huginn.m.r:tag huginn.m.r:+reference+ pointer)))))
            ((inlined-fixnum-p elt)
             (add (huginn.m.r:tag huginn.m.r:+fixnum+ elt)))
            ((expressionp elt)
             (let ((pointer (pointer-for-expression state elt)))
               (assert (not (null pointer)))
               (if (eql pointer index)
                   (progn
                     (add (huginn.m.r:tag huginn.m.r:+expression+ index))
                     (add (length elt)))
                   (add (huginn.m.r:tag huginn.m.r:+reference+ pointer)))))))
    result))


(defun flat-representation (expression &optional (result (vect)))
  (labels ((impl (exp)
             (when (expressionp exp)
               (iterate
                 (for e in exp)
                 (vector-push-extend e result))
               (iterate
                 (for e in exp)
                 (impl exp)))))
    (impl expression)
    result))


(defun flat-representation-cells-count (flat-form &key (end (length flat-form)))
  (* end 2 (count-if #'expressionp flat-form :end end)))


(defmethod cells-count ((state compilation-state))
  (~> state read-flat-representation flat-representation-cells-count))


(defmethod make-compilation-state ((class (eql 'compilation-state))
                                   clause)
  (check-type clause clause)
  (let* ((head (clause-head clause))
         (body (clause-body clause))
         (predicate (clause-head-predicate clause))
         (flat-form (vect))
         (body-pointer 0))
    (check-type predicate predicate)
    (check-type head clause)
    (check-type body clause)
    (flat-representation head flat-form)
    (setf body-pointer (flat-representation-cells-count flat-form))
    (flat-representation head flat-form)
    (make 'compilation-state
          :head head
          :body body
          :flat-representation flat-form
          :body-pointer body-pointer)))


(defmethod pointer-for-expression ((state compilation-state)
                                   expression)
  (check-type expression list)
  (let* ((flat-form (expressions state))
         (position (position expression flat-form :test 'eq)))
    (when (null position)
      (return-from pointer-for-expression nil))
    (flat-representation-cells-count flat-form :end (1+ position))))


(defmethod pointer-for-variable ((state compilation-state)
                                 variable)
  (check-type variable variable)
  (let* ((flat-form (expressions state))
         (position (position variable flat-form :test 'eq)))
    (when (null position)
      (return-from pointer-for-variable nil))
    (flat-representation-cells-count flat-form :end (1+ position))))


(defmethod predicate ((state compilation-state))
  (~> state head clause-head-predicate))
