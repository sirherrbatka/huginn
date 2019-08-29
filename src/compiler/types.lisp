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
  ((%unification-function-symbol :reader read-unification-function-symbol)
   (%object-position :initarg :object-position
                     :accessor access-object-position)
   (%pinned :initarg :pinned
            :accessor access-pinned))
  (:default-initargs
   :object-position nil
   :pinned nil))


(defmethod initialize-instance :after ((marker fundamental-marker)
                                       &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value marker '%unification-function-symbol)
        (~> marker class-of class-name symbol-name gensym)))


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


(defclass abstract-value-mixin (fundamental-marker)
  ((%value-symbol :initarg :value-symbol
                  :reader read-value-symbol))
  (:default-initargs :value-symbol (gensym)))


(defclass lazy-value-mixin (abstract-value-mixin)
  ())


(defclass eager-value-mixin (abstract-value-mixin)
  ())


(defclass referencable-mixin (fundamental-marker)
  ())


(defclass immutable-cell-mixin (fundamental-marker)
  ())


(defclass mutable-cell-mixin (fundamental-marker)
  ())


(defclass pointer-mixin (fundamental-marker)
  ((%destination :initarg :destination
                 :accessor access-destination))
  (:default-initargs
   :destination nil))


(defclass complex-mixin (content-mixin)
  ())


(defclass content-mixin (fundamental-marker)
  ((%content :initarg :content
             :reader read-content)))


(defclass indexed-mixin (fundamental-marker)
  ((%variable-index :initarg :variable-index
                    :accessor access-variable-index))
  (:default-initargs :variable-index 0))


(defclass list-rest-marker (referencable-mixin
                            content-mixin
                            indexed-mixin)
  ())


(defclass fixnum-marker (immutable-cell-mixin
                         content-mixin
                         eager-value-mixin)
  ())


(defclass variable-marker (content-mixin
                           indexed-mixin)
  ())


(defclass unbound-variable-marker (mutable-cell-mixin
                                   referencable-mixin
                                   eager-value-mixin
                                   variable-marker)
  ())


(defclass bound-variable-marker (immutable-cell-mixin
                                 lazy-value-mixin
                                 variable-marker)
  ())


(defclass expression-marker (immutable-cell-mixin
                             complex-mixin
                             pointer-mixin
                             eager-value-mixin)
  ((%arity :initarg :arity
           :reader read-arity)))


(defmethod initialize-instance :after ((marker expression-marker)
                                       &key &allow-other-keys)
  (setf (slot-value marker '%arity) (~> marker read-content length)))


(defclass predicate-marker (content-mixin
                            eager-value-mixin)
  ())


(defclass unbound-predicate-marker (mutable-cell-mixin
                                    predicate-marker)
  ())


(defclass bound-predicate-marker (immutable-cell-mixin
                                  predicate-marker)
  ())


(defclass list-end-marker (immutable-cell-mixin)
  ())


(defclass list-marker (immutable-cell-mixin
                       pointer-mixin
                       complex-mixin
                       eager-value-mixin)
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


(defclass cell-copy-form-arguments ()
  ((%heap-symbol :initarg :heap-symbol
                 :reader read-heap-symbol)
   (%execution-state-symbol :initarg :execution-state-symbol
                            :reader read-execution-state-symbol)
   (%heap-pointer-symbol :initarg :heap-pointer-symbol
                         :reader read-heap-pointer-symbol)
   (%execution-stack-cell-symbol :reader read-execution-stack-cell-symbol
                                 :initarg :execution-stack-cell-symbol)
   (%clause-symbol :initarg :clause-symbol
                   :reader read-clause-symbol)
   (%position :initarg :position
              :reader read-position)
   (%bindings-fill-pointer :initarg :bindings-fill-pointer-symbol
                           :reader read-bindings-fill-pointer-symbol)
   (%database :initarg :database
              :reader read-database)))


(cl-ds.utils:define-list-of-slots cell-copy-form-arguments ()
  (heap-symbol read-heap-symbol)
  (clause-symbol read-clause-symbol)
  (position read-position)
  (execution-state-symbol read-execution-state-symbol)
  (execution-stack-cell-symbol read-execution-stack-cell-symbol)
  (heap-pointer-symbol read-heap-pointer-symbol)
  (bindings-fill-pointer-symbol read-bindings-fill-pointer-symbol)
  (database read-database))


(defstruct list-input
  (content))


(defun list-input (content)
  (make-list-input :content content))


(defclass unification-form-arguments ()
  ((%ponter-symbol :accessor access-pointer-symbol
                   :initarg :pointer-symbol)
   (%goal-pointer-symbol :accessor access-goal-pointer-symbol
                         :initarg :goal-pointer-symbol)
   (%execution-state-symbol :reader read-execution-state-symbol
                            :initarg :execution-state-symbol)
   (%execution-stack-cell-symbol :reader read-execution-stack-cell-symbol
                                 :initarg :execution-stack-cell-symbol)
   (%heap-symbol :reader read-heap-symbol
                 :initarg :heap-symbol)
   (%fail-symbol :reader read-fail-symbol
                 :initarg :fail-symbol)
   (%all-markers :reader read-all-markers
                 :initarg :all-markers)
   (%function-symbol :reader read-function-symbol
                     :initarg :function-symbol)
   (%database :reader read-database
              :initarg :database))
  (:default-initargs
   :function-symbol (gensym "FUNCTION")
   :pointer-symbol (gensym "POINTER")
   :goal-pointer-symbol (gensym "GOAL-POINTER")
   :execution-state-symbol (gensym "EXECUTION-STATE")
   :execution-stack-cell-symbol (gensym "EXECUTION-STACK-CELL")
   :heap-symbol (gensym "HEAP")
   :fail-symbol (gensym "FAIL")))


(defun make-unification-form-arguments (all-markers database)
  (make-instance 'unification-form-arguments
                 :all-markers all-markers
                 :database database))


(cl-ds.utils:define-list-of-slots unification-form-arguments ()
  (pointer-symbol access-pointer-symbol)
  (goal-pointer-symbol access-goal-pointer-symbol)
  (execution-stack-cell-symbol read-execution-stack-cell-symbol)
  (heap-symbol read-heap-symbol)
  (database read-database)
  (function-symbol read-function-symbol)
  (all-markers read-all-markers)
  (fail-symbol read-fail-symbol)
  (execution-state-symbol read-execution-state-symbol))


(defgeneric cell-value-form (marker arguments)) ; needs to be implemented just for the abstract-value-mixin (this excludes list-end)
(defgeneric cell-unification-form (marker arguments) ; different for each marker
  (:method ((marker fundamental-marker) arguments)
    (cl-ds.utils:with-slots-for (arguments unification-form-arguments)
      `(huginn.m.o:unify-pair ,execution-state-symbol ,execution-stack-cell-symbol
                              (the huginn.m.r:pointer (+ ,(access-object-position marker)
                                                         ,pointer-symbol))
                              (huginn.m.r:follow-pointer ,execution-state-symbol
                                                         ,goal-pointer-symbol
                                                         t)
                              ,(read-value-symbol marker))))
  (:method ((marker list-rest-marker) arguments)
    (cl-ds.utils:with-slots-for (arguments unification-form-arguments)
      (with-gensyms (!this-pointer !result)
        `(let ((,!this-pointer (+ ,(access-object-position marker)
                                  ,pointer-symbol)))
           (declare (type huginn.m.r:pointer ,!this-pointer))
           (let ((,!result
                   (huginn.machine.operations:unify-inner-list-rest
                    ,execution-state-symbol
                    ,execution-stack-cell-symbol
                    ,!this-pointer
                    ,goal-pointer-symbol)))
             (when (null ,!result)
               ,(cell-fail-form marker arguments))
             t)))))
  (:method ((marker expression-marker) arguments)
    (cl-ds.utils:with-slots-for (arguments unification-form-arguments)
      (with-gensyms (!other-expression)
        `(let ((,!other-expression (~> ,heap-symbol
                                       (aref ,goal-pointer-symbol)
                                       huginn.m.r:detag)))
          ,@(iterate
              (with range = (content-for-unification marker arguments))
              (for (values content more) = (cl-ds:consume-front range))
              (for i from 0)
              (while more)
              (collect (list (read-unification-function-symbol content)
                             `(+ ,!other-expression ,i)))))))))
(defgeneric unify-each-form (range arguments)) ; used for expressions and lists alike
(defgeneric content-for-unification (marker arguments)) ; needs implementation for lists and expressions, returns range that should yield markers
(defgeneric cell-fail-form (marker arguments) ; the same for every marker
  (:method ((marker fundamental-marker) arguments)
    (list (read-fail-symbol arguments))))
(defgeneric cell-store-form (marker arguments)
  (:method ((marker immutable-cell-mixin) arguments)
    (with-gensyms (!result)
      `(let ((,!result ,(cell-unification-form marker arguments)))
         (when (null ,!result) ,(cell-fail-form marker arguments))
         t)))
  (:method ((marker list-marker) arguments)
    (cl-ds.utils:with-slots-for (arguments unification-form-arguments)
      (with-gensyms (!other-cell !word !start !walk !sub)
        `(tagbody ,!start
            (let* ((,!other-cell (aref ,heap-symbol ,goal-pointer-symbol))
                   (,!word (huginn.m.r:detag ,!other-cell)))
              (flet ((,!walk ()
                       (setf ,goal-pointer-symbol ,!word)
                       ,@(iterate
                           (with range = (content-for-unification marker arguments))
                           (for (values content more) = (cl-ds:consume-front range))
                           (for i from 0)
                           (while more)
                           (collect `(tagbody ,!sub
                                        (let ((,!other-cell (aref ,heap-symbol ,goal-pointer-symbol)))
                                          (when (huginn.m.r:list-rest-cell-p ,!other-cell)
                                            (when (huginn.m.r:list-rest-unbound-p ,!other-cell)
                                              (huginn.m.o:alter-cell
                                               ,execution-state-symbol
                                               ,execution-stack-cell-symbol
                                               ,goal-pointer-symbol
                                               (huginn.m.r:tag huginn.m.r:+list-rest+
                                                               (+ ,pointer-symbol
                                                                  ,i
                                                                  ,(access-destination marker))))
                                              (return-from ,!start t))
                                            (setf ,goal-pointer-symbol (huginn.m.r:detag ,!other-cell))
                                            (go ,!sub)))
                                        (,(read-unification-function-symbol content)
                                         ,goal-pointer-symbol)
                                        (incf ,goal-pointer-symbol))))))
                (cond ((huginn.m.r:reference-cell-p ,!other-cell)
                       (setf ,goal-pointer-symbol ,!word)
                       (go ,!start))
                      ((huginn.m.r:variable-cell-p ,!other-cell)
                       (if (huginn.m.r:variable-unbound-p ,!other-cell)
                           (huginn.m.o:alter-cell ,execution-state-symbol
                                                  ,execution-stack-cell-symbol
                                                  ,goal-pointer-symbol
                                                  ,(read-value-symbol marker))
                           ,(cell-fail-form marker arguments)))
                      ((huginn.m.r:list-rest-cell-p ,!other-cell)
                       (if (huginn.m.r:list-rest-unbound-p ,!other-cell)
                           (huginn.m.o:alter-cell
                            ,execution-state-symbol
                            ,execution-stack-cell-symbol
                            ,goal-pointer-symbol
                            (huginn.m.r:retag huginn.m.r:+list-rest+
                                              ,(read-value-symbol marker)))
                           (,!walk)))
                      ((huginn.m.r:list-start-cell-p ,!other-cell)
                       (,!walk))
                      (t ,(cell-fail-form marker arguments)))))))))
  (:method ((marker expression-marker) arguments)
    (cell-unification-form marker arguments))
  (:method ((marker list-rest-marker) arguments)
    (cell-unification-form marker arguments))
  (:method ((marker mutable-cell-mixin) arguments)
    (cl-ds.utils:with-slots-for (arguments unification-form-arguments)
      `(progn
         (setf ,(read-value-symbol marker)
               ,(cell-unification-form marker arguments))
         (when (null ,(read-value-symbol marker))
           ,(cell-fail-form marker arguments))
         (when (huginn.m.r:reference-cell-p
                ,(read-value-symbol marker))
           (setf ,(read-value-symbol marker)
                 (huginn.m.r:dereference-heap-pointer
                  ,execution-state-symbol
                  (huginn.m.r:detag ,(read-value-symbol marker))
                  t)))
         t))))
(defgeneric ensure-object-position (object position))
(defgeneric execute (flattening operation))
(defgeneric queue-size (flattening))
(defgeneric next-object (flattening))
(defgeneric marker-for (flattening exp class &key enforce-class))
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
(defgeneric ensure-object-position (marker pointer))
(defgeneric cell-copy-form (marker arguments))
