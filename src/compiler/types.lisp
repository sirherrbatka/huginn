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
         (marker-pinned (access-pinned operation))
         (position (access-object-position flattening)))
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
(defgeneric markers-for (flattening exp class))
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


(defmethod markers-for ((flattening flattening) exp class)
  (bind ((markers (read-markers flattening))
         (list (gethash exp markers))
         (result (find-if (rcurry #'typep class)
                          list)))
    (when (null result)
      (setf result (make-instance class :content exp)
            list (cons result list)
            (gethash exp markers) list)
      (when (typep result 'indexed-mixin)
        (if (variablep exp)
            0
            (setf (access-variable-index result)
                  (incf (access-variable-index flattening))))))
    list))


(defmethod enqueue-expression/variable/list/fixnum ((flattening flattening)
                                                    exp
                                                    direction)
  (cond
    ((expressionp exp) (~>> (markers-for flattening exp 'expression-marker)
                            first
                            (funcall direction flattening)))
    ((list-input-p exp) (~>> (markers-for flattening
                                          (list-input-content exp)
                                          'list-marker)
                             first
                             (funcall direction flattening)))
    ((variablep exp)
     (~>> (markers-for flattening exp 'variable-marker)
          first
          (funcall direction flattening)))
    ((inlined-fixnum-p exp)
     (~>> (make 'fixnum-marker :content exp)
          (funcall direction flattening)))
    (t (~>> (markers-for flattening exp 'variable-marker)
            first
            (funcall direction flattening)))))


(defmethod ensure-object-position ((marker referencable-mixin) pointer)
  (ensure (access-object-position marker) pointer))


(defmethod next-object ((flattening flattening))
  (~> flattening read-queue flexichain:pop-start))


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
      (t (enqueue-expression/variable/list/fixnum flattening object
                                                  #'enqueue-back)))
    (finally (return result))))


(defmethod enqueue-markers-content ((flattening flattening)
                                    (marker expression-marker))
  (let ((content (read-content marker)))
    (~>> (make 'set-destination-operation
               :marker marker)
         (enqueue-back flattening))
    (~>> (make-instance 'fixnum-marker
                        :content (read-arity marker))
         (enqueue-back flattening))
    (~>> (markers-for flattening
                      (first content)
                      'predicate-marker)
         first
         (enqueue-back flattening))
    (iterate
      (for c in (rest content))
      (enqueue-expression/variable/list/fixnum flattening c
                                               #'enqueue-back)))
  flattening)


(defmethod enqueue-markers-content ((flattening flattening)
                                    (marker list-marker))
  (declare (optimize (debug 3)))
  (~>> (make 'set-destination-operation
             :marker marker)
       (enqueue-back flattening))
  (iterate
    (with sub = (read-content marker))
    (until (null sub))
    (if (consp sub)
        (enqueue-expression/variable/list/fixnum flattening
                                                 (first sub)
                                                 #'enqueue-back)
        (let ((markers (markers-for flattening sub 'list-rest-marker)))
          (iterate
            (for m in markers)
            (typecase m
              (variable-marker
               (~>> (make 'set-position-operation :marker m
                                                  :pin t)
                    (enqueue-back flattening))
               (change-class m 'list-rest-marker)
               (enqueue-back flattening m))
              (list-rest-marker
               (enqueue-back flattening m))))
          (leave)))
    (pop sub)
    (finally (enqueue-back flattening (make 'list-end-marker)))))


(defmethod queue-size ((flattening flattening))
  (~> flattening read-queue flexichain:nb-elements))


(defmethod marker->cell ((marker list-end-marker) position database)
  (huginn.m.r:tag huginn.m.r:+list-end+ 0))


(defgeneric marker-tag (marker)
  (:method ((marker list-marker))
    huginn.m.r:+list-start+)
  (:method ((marker expression-marker))
    huginn.m.r:+expression+)
  (:method ((marker list-rest-marker))
    huginn.m.r:+list-rest+)
  (:method ((marker variable-marker))
    huginn.m.r:+variable+))


(defmethod marker->cell ((marker pointer-mixin) position database)
  (huginn.m.r:tag (marker-tag marker)
                  (access-destination marker)))


(defmethod marker->cell ((marker fixnum-marker) position database)
  (huginn.m.r:tag huginn.m.r:+fixnum+
                  (read-content marker)))


(defmethod marker->cell :around ((marker referencable-mixin)
                                 position database)
  (let ((object-position (access-object-position marker)))
    (if (= position object-position)
        (call-next-method)
        (huginn.m.r:tag huginn.m.r:+reference+ object-position))))


(defmethod marker->cell ((marker variable-marker) position database)
  (huginn.m.r:tag huginn.m.r:+variable+
                  (access-variable-index marker)))


(defmethod marker->cell ((marker list-rest-marker) position database)
  (huginn.m.r:tag huginn.m.r:+list-rest+
                  (access-variable-index marker)))


(defmethod marker->cell ((marker predicate-marker) position database)
  (huginn.m.r:tag huginn.m.r:+predicate+
                  (if (~> marker read-content variablep)
                      0
                      (~>> marker read-content
                           (huginn.m.d:index-predicate database)))))
