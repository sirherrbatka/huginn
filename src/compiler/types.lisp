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


(defclass fundamental-marker ()
  ())


(defgeneric marker-size (marker))


(defmethod marker-size ((marker fundamental-marker))
  1)


(defclass referencable-mixin (fundamental-marker)
  ((%object-position :initarg :object-position
                     :accessor access-object-position)))


(defclass pointer-mixin (fundamental-marker)
  ((%destination :initarg :destination
                 :accessor access-destination)))


(defclass complex-mixin (content-marker)
  ())


(defclass potentially-unbound-mixin (fundamental-marker)
  ((%bound :initarg :bound
           :accessor access-bound))
  (:default-initargs :bound nil))


(defclass content-mixin (fundamental-marker)
  ((%content :initarg :content
             :reader read-content)))


(defclass list-rest-marker (referencable-mixin
                            content-mixin
                            fundamental-marker)
  ())


(defclass fixnum-marker (content-mixin)
  ())


(defclass variable-marker (referencable-mixin
                           potentially-unbound-mixin
                           content-mixin
                           fundamental-marker)
  ())


(defclass expression-marker (complex-mixin
                             fundamental-marker)
  ((%arity :initarg :arity
           :reader read-arity)))


(defclass predicate-marker (content-mixin
                            fundamental-marker)
  ())


(defclass list-end-marker (fundamental-marker)
  ())


(defclass list-marker (pointer-mixin
                       complex-mixin
                       fundamental-marker)
  ())
