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


(defclass flattening ()
  ((%queue :initarg :queue
           :reader read-queue)
   (%markers :initarg :markers
             :reader read-markers)
   (%pointer :initarg :pointer
             :accessor access-pointer)
   (%variable-index :initarg :variable-index
                    :accessor access-variable-index))
  (:default-initargs
   :queue (make 'flexichain:standard-flexichain)
   :pointer 0
   :variable-index 0
   :markers (make-hash-table :test 'eq)))


(defclass referencable-mixin (fundamental-marker)
  ((%object-position :initarg :object-position
                     :accessor access-object-position)
   (%pinned :initarg :pinned
            :accessor access-pinned))
  (:default-initargs
   :object-position nil
   :pinned nil))


(defclass pointer-mixin (fundamental-marker)
  ((%destination :initarg :destination
                 :accessor access-destination))
  (:default-initargs
   :destination nil))


(defclass complex-mixin (content-mixin)
  ())


(defclass potentially-unbound-mixin (fundamental-marker)
  ((%bound :initarg :bound
           :accessor access-bound))
  (:default-initargs :bound nil))


(defclass content-mixin (fundamental-marker)
  ((%content :initarg :content
             :reader read-content)))


(defclass indexed-mixin (fundamental-marker)
  ((%variable-index :initarg :variable-index
                    :accessor access-variable-index))
  (:default-initargs :variable-index 0))


(defclass list-rest-marker (referencable-mixin
                            content-mixin
                            indexed-mixin
                            fundamental-marker)
  ())


(defclass fixnum-marker (content-mixin)
  ())


(defclass variable-marker (referencable-mixin
                           potentially-unbound-mixin
                           content-mixin
                           indexed-mixin
                           fundamental-marker)
  ())


(defclass expression-marker (complex-mixin
                             pointer-mixin
                             fundamental-marker)
  ((%arity :initarg :arity
           :reader read-arity)))


(defmethod initialize-instance :after ((marker expression-marker)
                                       &key &allow-other-keys)
  (setf (slot-value marker '%arity) (~> marker read-content length)))


(defclass predicate-marker (content-mixin
                            fundamental-marker)
  ())


(defclass list-end-marker (fundamental-marker)
  ())


(defclass list-marker (pointer-mixin
                       complex-mixin
                       fundamental-marker)
  ())


(defclass fundamental-operation ()
  ())


(defclass set-position-operation (fundamental-operation)
  ((%marker :initarg :marker
            :reader read-marker)
   (%pin :initarg :pin
         :reader read-pin))
  (:default-initargs
   :pin nil))


(defmethod execute ((flattening flattening)
                    (operation set-position-operation))
  (let* ((marker (read-marker operation))
         (pin (read-pin operation))
         (marker-pinned (access-pinned marker))
         (position (access-pointer flattening)))
    (unless marker-pinned
      (setf (access-object-position marker) position)
      (when pin
        (setf (access-object-position marker) position
              (access-pinned marker) t)))))


(defclass set-destination-operation (fundamental-operation)
  ((%marker :initarg :marker
            :reader read-marker)))


(defmethod execute ((flattening flattening)
                    (operation set-destination-operation))
  (let* ((marker (read-marker operation))
         (position (access-pointer flattening)))
    (setf (access-destination marker) position)))


(defstruct list-input
  (content))


(defun list-input (content)
  (make-list-input :content content))


(defgeneric marker-size (marker)
  (:method ((marker fundamental-marker))
    1))

(defgeneric ensure-object-position (object position))
(defgeneric execute (flattening operation))
(defgeneric queue-size (flattening))
(defgeneric next-object (flattening))
(defgeneric marker-for (flattening exp class))
(defgeneric enqueue-expression/variable/list/fixnum (flattening exp
                                                     direction))
(defgeneric marker->cell (marker position database))
(defgeneric enqueue-front (flattening exp)
  (:method ((flattening flattening) exp)
    (~> flattening read-queue (flexichain:push-start exp))
    flattening))
(defgeneric enqueue-back (flattening exp)
  (:method ((flattening flattening) exp)
    (~> flattening read-queue (flexichain:push-end exp))
    flattening))
(defgeneric enqueue-markers-content (flattening marker)
  (:method ((flattening flattening) (marker fundamental-marker))
    nil))
(defgeneric flat-representation (flattening &optional result))
(defgeneric ensure-object-position (marker pointer)
  (:method ((marker fundamental-marker) pointer)
    nil))
