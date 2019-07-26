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
             :accessor access-pointer))
  (:default-initargs
   :queue (cl-ds.queues.2-3-tree:make-mutable-2-3-queue)
   :pointer 0
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


(defclass fundamental-operation ()
  ())


(defclass set-position-operation (fundamental-operation)
  ((%marker :initarg :marker
            :reader read-marker)
   (%pin :initarg :pin
         :reader read-pin))
  (:default-initargs
   :pin nil))


(defclass set-destination-operation (fundamental-operation)
  ((%marker :initarg :marker
            :reader read-marker)))


(defstruct list-input
  (content))


(defun list-input (content)
  (make-list-input :content content))


(defgeneric marker-size (marker)
  (:method ((marker fundamental-marker))
    1))

(defgeneric ensure-object-position ())
(defgeneric execute (flattening operation))
(defgeneric queue-size (flattening))
(defgeneric next-object (flattening))
(defgeneric markers-for (flattening exp class))
(defgeneric enque-expression/variable/list/fixnum (flattening exp))
(defgeneric enqueue (flattening exp)
  (:method ((flattening flattening) exp)
    (~> flattening read-queue (cl-ds:put! exp))
    flattening))
(defgeneric enqueue-markers-content (flattening marker)
  (:method ((flattening flattening) (marker fundamental-marker))
    nil))
(defgeneric flat-representation (flattening &optional result))
(defgeneric ensure-object-position (marker pointer)
  (:method ((marker fundamental-marker) pointer)
    nil))


(defmethod marker-size ((marker expression-marker))
  2)


(defmethod enqueue-expression/variable/list/fixnum ((flattening flattening)
                                                    exp)
  (cond
    ((expressionp exp) cl-ds.utils:todo)
    ((variablep exp) cl-ds.utils:todo)
    ((list-input-p exp) cl-ds.utils:todo)
    ((huginn.m.r:fixnum-cell-p exp) cl-ds.utils:todo)
    (t cl-ds.utils:todo)))


(defmethod ensure-object-position ((marker referencable-mixin) pointer)
  (ensure (access-object-position marker) pointer))


(defmethod next-object ((flattening flattening))
  (~> flattening read-queue cl-ds:take-out!))


(defgeneric markerp (object)
  (:method ((object fundamental-marker))
    t)
  (:method ((object t))
    nil))


(defmethod flat-representation ((flattening flattening)
                                &optional (result (vect)))
  (iterate
    (until (zerop (queue-size flattening)))
    (for object = (next-object flattening))
    (etypecase object
      (fundamental-marker
       (ensure-object-position object (access-pointer flattening))
       (vector-push-extend object result)
       (enqueue-markers-content flattening object)
       (incf (access-pointer flattening)
             (marker-size object)))
      (fundamental-operation
       (execute flattening object))
      (t (enque-expression/variable/list/fixnum flattening object)))
    (finally (return result))))


(defmethod enqueue-markers-content ((flattening flattening) (marker expression-marker))
  (let ((content (read-content marker)))
    (enqueue flattening
             (first (markers-for flattening
                                 (first content)
                                 'predicate-marker)))
    (iterate
      (for c in (rest content))
      (enque-expression/variable/list/fixnum flattening c)))
  flattening)


(defmethod enqueue-markers-content ((flattening flattening) (marker list-marker))
  (iterate
    (with sub = (read-content marker))
    (until (null sub))
    (if (consp sub)
        (enque-expression/variable/list/fixnum flattening (first sub))
        (let ((markers (markers-for flattening sub 'list-rest-marker)))
          (iterate
            (for m in markers)
            (etypecase m
              (variable-marker (~>> (make 'set-pointer-operation
                                          :marker m :pin t)
                                    (enqueue flattening)))
              (list-rest-marker (enqueue flattening m))))
          (leave)))
    (pop sub)
    (finally (enqueue flattening (make 'list-end-marker)))))


(defmethod queue-size ((flattening flattening))
  (~> flattening read-queue cl-ds:size))
