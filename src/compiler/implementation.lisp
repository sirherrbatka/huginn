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
  `(satisfies expressionp))


(deftype variable ()
  `(satisfies variablep))


(deftype anonymus-variable ()
  `(satisfies anonymus-variable-p))


(defclass compilation-state (fundamental-compilation-state)
  ((%values-table :initarg :values-table
                  :reader read-values-table)
   (%flat-representation :initarg :flat-representation
                         :reader read-flat-representation)
   (%body-pointer :initarg :body-pointer
                  :reader body-pointer
                  :reader read-body-pointer)
   (%head :initarg :head
          :reader head
          :reader read-head)
   (%body :initarg :body
          :reader body
          :reader read-body)))

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


(defun unique-index (&key (test 'eq) (start 0))
  (let ((table (make-hash-table :test test))
        (index start))
    (values
     (lambda (elt &rest all)
       (declare (ignore all))
       (let* ((fresh-index (ensure (gethash elt table) index))
              (new (eql fresh-index index)))
         (when new (incf index))
         (values fresh-index new)))
     table)))


(defstruct expression-marker (content))

(defun flat-representation (expression &optional (result (vect)))
  (declare (optimize (debug 3)))
  (unless (endp expression)
    (check-type expression expression)
    (labels ((impl (exp)
               (when (and (expressionp exp)
                          (not (find-if (lambda (x)
                                          (and (expression-marker-p x)
                                               (~> x expression-marker-content
                                                   (eq exp))))
                                        result)))
                 (vector-push-extend (make-expression-marker :content exp)
                                     result)
                 (iterate
                   (for e in exp)
                   (vector-push-extend e result))
                 (iterate
                   (for e in exp)
                   (impl e)))))
      (impl expression)))
  result)


(defmethod content ((state compilation-state))
  (declare (optimize (debug 3)))
  (bind ((result (make-array (cells-count state)
                             :element-type 'huginn.m.r:cell))
         ((:slots %flat-representation) state)
         (index 0)
         (value-index-function (unique-index :test 'eql :start 1))
         ((:flet add (item))
          (setf (aref result index) item)
          (incf index)))
    (iterate
      (for elt in-vector %flat-representation)
      (econd
        ((anonymus-variable-p elt)
         (add (huginn.m.r:tag huginn.m.r:+variable+ 0)))
        ((variablep elt)
         (let ((pointer (pointer-for-variable state elt)))
           (assert (not (null pointer)))
           (if (eql pointer index)
               (add (huginn.m.r:tag huginn.m.r:+variable+ 0))
               (add (huginn.m.r:tag huginn.m.r:+reference+ pointer)))))
        ((inlined-fixnum-p elt)
         (add (huginn.m.r:tag huginn.m.r:+fixnum+ elt)))
        ((expression-marker-p elt)
         (let ((pointer (~>> elt expression-marker-content
                             (pointer-for-expression state))))
           (assert (not (null pointer)))
           (if (eql pointer index)
               (progn
                 (add (huginn.m.r:tag huginn.m.r:+expression+ index))
                 (add (~> elt expression-marker-content length)))
               (add (huginn.m.r:tag huginn.m.r:+reference+ pointer)))))
        ((valuep elt)
         (~>> elt
              (funcall value-index-function)
              (huginn.m.r:tag huginn.m.r:+variable+)
              add))))
    result))


(defun flat-representation-cells-count (flat-form
                                        &key
                                          (end (length flat-form))
                                          (start 0))
  (+ (- end start)
     (count-if #'expression-marker-p flat-form :end end
                                               :start start)))


(defmethod cells-count ((state compilation-state))
  (~> state read-flat-representation flat-representation-cells-count))


(defmethod make-compilation-state ((class (eql 'compilation-state))
                                   clause)
  (check-type clause clause)
  (let* ((head (clause-head clause))
         (body (clause-body clause))
         (predicate (clause-head-predicate head))
         (flat-form (vect))
         (body-pointer 0))
    (check-type head clause)
    (check-type body clause)
    (check-type predicate predicate)
    (flat-representation head flat-form)
    (setf body-pointer (flat-representation-cells-count flat-form))
    (iterate
      (for b in body)
      (flat-representation b flat-form))
    (make 'compilation-state
          :head head
          :body body
          :flat-representation flat-form
          :body-pointer body-pointer)))


(defmethod pointer-for (flat-form predicate
                        &key
                          (start 0)
                          (end (length flat-form)))
  (iterate
    (for i from start below end)
    (for elt in-vector flat-form)
    (finding pointer such-that (funcall predicate elt))
    (sum (if (expression-marker-p elt) 2 1)
         into pointer)))


(defmethod pointer-for-expression ((state compilation-state)
                                   expression)
  (check-type expression expression)
  (pointer-for (read-flat-representation state)
               (lambda (elt)
                 (and (expression-marker-p elt)
                      (eq (expression-marker-content elt)
                          expression)))))


(defmethod expressions ((compilation-state compilation-state) start end)
  (check-type start non-negative-fixnum)
  (check-type end non-negative-fixnum)
  (assert (<= start end))
  (iterate
    (with flat-form = (read-flat-representation compilation-state))
    (for elt in-vector flat-form)
    (when (and (>= sum start) (expression-marker-p elt))
      (collect (expression-marker-content elt)))
    (sum (if (expression-marker-p elt) 2 1) into sum)
    (while (< sum end))))


(defmethod variables ((compilation-state compilation-state) start end)
  (check-type start non-negative-fixnum)
  (check-type end non-negative-fixnum)
  (assert (<= start end))
  (iterate
    (with flat-form = (read-flat-representation compilation-state))
    (for elt in-vector flat-form)
    (when (and (>= sum start) (variablep elt))
      (collect elt))
    (sum (if (expression-marker-p elt) 2 1) into sum)
    (while (< sum end))))

(defmethod pointer-for-variable ((state compilation-state)
                                 variable)
  (check-type variable variable)
  (pointer-for (read-flat-representation state)
               (lambda (elt) (eq elt variable))))


(defmethod predicate ((state compilation-state))
  (~> state head clause-head-predicate))


(defmethod print-object ((state fundamental-compilation-state) stream)
  (print-unreadable-object (state stream :type nil)
    (format stream "<- ~a ~a" (head state) (body state))))


(defmethod compile-clause ((compilation-state fundamental-compilation-state))
  (huginn.m.r:make-clause))


(defmethod variable-bindings ((compilation-state compilation-state))
  (~>> compilation-state read-flat-representation
       (remove-if (cl-ds.utils:or* #'variablep
                                   #'inlined-fixnum-p
                                   #'expression-marker-p))
       delete-duplicates))
