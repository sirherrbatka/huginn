(cl:in-package #:huginn.compiler)


(defmethod marker-for ((flattening flattening) exp class
                       &key (enforce-class nil))
  (if (anonymus-variable-p exp)
      (make-instance class :content exp)
      (bind ((markers (read-markers flattening))
             (result-list (gethash exp markers))
             (result (find class result-list :key #'type-of)))
        (when (and (not (null class))
                   (and (null result)
                        (or (endp result-list)
                            enforce-class)))
          (setf result (make-instance class :content exp)
                result-list (cons result result-list)
                (gethash exp markers) result-list)
          (when (and (typep result 'indexed-mixin)
                     (not (variablep exp)))
            (setf (access-variable-index result)
                  (incf (access-variable-index flattening)))))
        (first result-list))))


(defmethod enqueue-expression/variable/list/fixnum ((flattening flattening)
                                                    exp
                                                    direction)
  (cond
    ((expressionp exp) (~>> (marker-for flattening exp 'expression-marker)
                            (funcall direction flattening)))
    ((list-input-p exp) (~>> (marker-for flattening
                                          (list-input-content exp)
                                          'list-marker)
                             (funcall direction flattening)))
    ((variablep exp)
     (~>> (marker-for flattening exp 'unbound-variable-marker)
          (funcall direction flattening)))
    ((inlined-fixnum-p exp)
     (~>> (make 'fixnum-marker :content exp)
          (funcall direction flattening)))
    (t (~>> (marker-for flattening exp
                        'bound-variable-marker
                        :enforce-class t)
            (funcall direction flattening)))))


(defmethod ensure-object-position ((marker fundamental-marker) pointer)
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
       (incf (access-pointer flattening)))
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
    (~>> (marker-for flattening
                      (first content)
                      (if (variablep (first content))
                          'unbound-predicate-marker
                          'bound-predicate-marker)
                      :enforce-class t)
         (enqueue-back flattening))
    (iterate
      (for c in (rest content))
      (enqueue-expression/variable/list/fixnum flattening c
                                               #'enqueue-back)))
  flattening)


(defmethod enqueue-markers-content ((flattening flattening)
                                    (marker list-marker))
  (declare (optimize (speed 3)))
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
        (let ((marker (marker-for flattening sub 'list-rest-marker)))
          (etypecase marker
            (variable-marker
             (~>> (make 'set-position-operation :marker marker
                                                :pin t)
                  (enqueue-back flattening))
             (change-class marker 'list-rest-marker)
             (enqueue-back flattening marker))
            (list-rest-marker
             (enqueue-back flattening marker)))
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
  (:method ((marker predicate-marker))
    huginn.m.r:+predicate+)
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


(defmethod content ((state compilation-state)
                    (database huginn.m.d:database)
                    &optional output)
  (declare (optimize (debug 3)))
  (bind ((cells-count (cells-count state))
         (result (or (and output
                          (if (< cells-count (length output))
                              output
                              (adjust-array output cells-count)))
                     (make-array (cells-count state)
                                 :element-type 'huginn.m.r:cell)))
         ((:slots %flat-representation) state)
         (index 0)
         ((:flet add (item))
          (setf (aref result index) item)
          (incf index)))
    (iterate
      (for elt in-vector %flat-representation)
      (~> elt (marker->cell index database) add))
    (iterate
      (for i from 0 below cells-count)
      (for elt in-vector result)
      (huginn.m.r:tag-case (elt)
        :expression (let ((pointer (huginn.m.r:detag elt)))
                      (assert (~> result (aref pointer)
                                  huginn.m.r:fixnum-cell-p))
                      (assert (~> result (aref pointer)
                                  huginn.m.r:detag
                                  (> 1)))
                      (assert (~> result (aref (1+ pointer))
                                  huginn.m.r:predicate-cell-p)))))
    result))


(defun flat-representation-cells-count (flat-form
                                        &key
                                          (end (length flat-form))
                                          (start 0))
  (declare (optimize (speed 1)))
  (- end start))


(defmethod cells-count ((state compilation-state))
  (~> state read-flat-representation flat-representation-cells-count))


(defmethod make-compilation-state ((class (eql 'compilation-state))
                                   clause)
  (declare (optimize (debug 3)))
  (check-type clause clause)
  (let* ((head (clause-head clause))
         (body (clause-body clause))
         (predicate (clause-head-predicate head))
         (flat-form (vect))
         (body-pointer 0))
    (check-type head clause)
    (check-type body clause)
    (check-type predicate predicate)
   (let ((flattening (make-instance 'flattening)))
     (unless (endp head)
       (enqueue-back flattening head)
       (flat-representation flattening flat-form)
       (setf body-pointer (flat-representation-cells-count flat-form)))
     (iterate
       (for b in body)
       (unless (goalp b)
         (error 'invalid-goal :form b))
       (enqueue-back flattening b))
     (flat-representation flattening flat-form))
    (make 'compilation-state
          :head head
          :body body
          :flat-representation flat-form
          :body-pointer body-pointer)))


(defun pointer-for (flat-form predicate
                    &key
                      (class t)
                      (start 0)
                      (end (length flat-form)))
  (iterate
    (for i from start below end)
    (for elt in-vector flat-form)
    (finding i such-that (and (typep elt class)
                              (if (typep elt 'referencable-mixin)
                                  (= (access-object-position elt) i)
                                  t)
                              (funcall predicate elt)))))


(defmethod pointer-for-list ((state compilation-state)
                             (list list-input))
  (pointer-for (read-flat-representation state)
               (lambda (elt)
                 (eq (read-content elt)
                     (list-input-content list)))
               :class 'list-marker))


(defmethod pointer-for-expression ((state compilation-state)
                                   expression)
  (check-type expression expression)
  (pointer-for (read-flat-representation state)
               (lambda (elt)
                 (eq (read-content elt)
                     expression))
               :class 'expression-marker))


(defun collect-range (compilation-state start end
                      &key
                        (key #'identity)
                        (predicate (constantly t))
                        (class t))
  (check-type start non-negative-fixnum)
  (check-type end non-negative-fixnum)
  (assert (<= start end))
  (iterate
    (with flat-form = (read-flat-representation compilation-state))
    (for i from start below end)
    (for elt = (aref flat-form i))
    (when (and (typep elt class) (funcall predicate elt))
      (collect (funcall key elt)))))


(defmethod expressions ((compilation-state compilation-state) start end)
  (~> (collect-range compilation-state start end
                  :key #'read-content
                  :class 'expression-marker)
      (remove-duplicates :from-end t)))


(defmethod variables ((compilation-state compilation-state) start end)
  (~>> (collect-range compilation-state start end
                      :predicate (cl-ds.utils:or*
                                  (rcurry #'typep 'variable-marker)
                                  (rcurry #'typep 'list-rest-marker))
                      :key #'read-content)
       (remove-if (lambda (name)
                    (or (string= name "?")
                        (not (eql (first-elt name) #\?))))
                  _
                  :key #'symbol-name)
       (remove-duplicates _)))


(defmethod pointer-for-variable ((state compilation-state)
                                 variable)
  (check-type variable variable)
  (flet ((matches (elt)
           (eq (read-content elt)
               variable)))
    (or (pointer-for (read-flat-representation state)
                     #'matches
                     :class 'list-rest-marker)
        (pointer-for (read-flat-representation state)
                     #'matches
                     :class 'variable-marker))))


(defmethod pointer-for-predicate ((state compilation-state)
                                  predicate)
  (check-type predicate predicate)
  (pointer-for (read-flat-representation state)
               (lambda (elt) (eq (read-content elt)
                                 predicate))
               :class 'predicate-marker))


(defmethod predicates ((state compilation-state)
                       start end)
  (~> (collect-range state start end
                     :class 'predicate-marker
                     :key #'read-content)
      (remove-duplicates :from-end t)))


(defmethod predicate ((state compilation-state))
  (~> state head clause-head-predicate))


(defmethod print-object ((state fundamental-compilation-state) stream)
  (print-unreadable-object (state stream :type nil)
    (format stream "<- ~a ~a" (head state) (body state))))


(defmethod variable-bindings ((compilation-state compilation-state))
  (~>> compilation-state read-flat-representation
       (remove-if-not (lambda (x)
                        (and (typep x 'variable-marker)
                             (~> x read-content variablep not))))
       (delete-duplicates _ :from-end t)
       (cl-ds.utils:transform #'read-content)))


(defmethod cell-copy-form :around ((marker referencable-mixin) arguments)
  (cl-ds.utils:with-slots-for (arguments cell-copy-form-arguments)
    (let ((object-position (access-object-position marker)))
      (if (= position object-position)
          (call-next-method)
          `(setf (aref ,heap-symbol
                       (the huginn.m.r:pointer (+ ,position
                                                  ,heap-pointer-symbol)))
                 (huginn.m.r:make-reference
                  (the huginn.m.r:pointer (+ ,object-position
                                             ,heap-pointer-symbol))))))))


(defmethod cell-copy-form ((marker list-end-marker) arguments)
  (cl-ds.utils:with-slots-for (arguments cell-copy-form-arguments)
    `(setf (aref ,heap-symbol
                 (the huginn.m.r:pointer (+ ,position
                                            ,heap-pointer-symbol)))
           ,(huginn.m.r:tag huginn.m.r:+list-end+ 0))))


(defmethod cell-copy-form ((marker fixnum-marker) arguments)
  (cl-ds.utils:with-slots-for (arguments cell-copy-form-arguments)
    `(setf (aref ,heap-symbol (the huginn.m.r:pointer
                                   (+ ,position
                                      ,heap-pointer-symbol)))
           ,(huginn.m.r:tag huginn.m.r:+fixnum+ (read-content marker)))))


(defmethod cell-copy-form ((marker pointer-mixin) arguments)
  (cl-ds.utils:with-slots-for (arguments cell-copy-form-arguments)
    `(setf (aref ,heap-symbol (the huginn.m.r:pointer
                                   (+ ,position
                                      ,heap-pointer-symbol)))
           (huginn.m.r:tag ,(marker-tag marker)
                           (the fixnum (+ ,heap-pointer-symbol
                                          ,(access-destination marker)))))))


(defmethod cell-copy-form ((marker variable-marker) arguments)
  (cl-ds.utils:with-slots-for (arguments cell-copy-form-arguments)
    (if (~> marker read-content variablep)
        `(setf (aref ,heap-symbol (the huginn.m.r:pointer
                                       (+ ,position
                                          ,heap-pointer-symbol)))
               ,(huginn.m.r:tag huginn.m.r:+variable+ 0))
        `(let* ((object (~> ,clause-symbol
                            huginn.m.r:clause-variable-values
                            (aref ,(1- (access-variable-index marker)))))
                (new-index ,bindings-fill-pointer-symbol)
                (index (huginn.machine.operations:index-object
                        ,execution-state-symbol
                        object
                        new-index)))
           (declare (type fixnum index new-index))
           (when (eql index new-index)
             (the fixnum (incf ,bindings-fill-pointer-symbol)))
           (setf (aref ,heap-symbol
                       (the huginn.m.r:pointer
                            (+ ,position
                               ,heap-pointer-symbol)))
                 (huginn.m.r:tag huginn.m.r:+variable+
                                 (the fixnum (1+ index))))))))


(defmethod cell-copy-form ((marker list-rest-marker) arguments)
  (cl-ds.utils:with-slots-for (arguments cell-copy-form-arguments)
    `(setf (aref ,heap-symbol (the huginn.m.r:pointer
                                   (+ ,position
                                      ,heap-pointer-symbol)))
           ,(huginn.m.r:tag huginn.m.r:+list-rest+
                            (access-variable-index marker)))))


(defmethod cell-copy-form ((marker predicate-marker) arguments)
  (cl-ds.utils:with-slots-for (arguments cell-copy-form-arguments)
    `(setf (aref ,heap-symbol (the huginn.m.r:pointer
                                   (+ ,position
                                      ,heap-pointer-symbol)))
           ,(huginn.m.r:tag
             huginn.m.r:+predicate+
             (if (~> marker read-content variablep)
                 0
                 (~>> marker read-content
                      (huginn.m.d:index-predicate database)))))))


(defun generate-copying-lambda-form (compilation-state database start end)
  (with-gensyms (!execution-state
                 !heap-pointer !heap !clause
                 !bindings-fill-pointer)
    (if (zerop end)
        `(lambda (a b c) (declare (ignore b a)) c)
        `(lambda (,!execution-state ,!heap-pointer ,!bindings-fill-pointer
                  ,!clause)
           (declare  (type huginn.m.r:execution-state ,!execution-state)
                     (ignorable ,!execution-state
                                ,!heap-pointer
                                ,!clause
                                ,!bindings-fill-pointer)
                     (type huginn.m.r:pointer ,!heap-pointer
                           ,!bindings-fill-pointer)
                     (optimize (speed 3) (debug 0) (safety 0)
                               (compilation-speed 0) (space 0)))
           (huginn.m.r:expand-state-heap ,!execution-state
                                         (the fixnum
                                              (+ ,!heap-pointer
                                                 ,end)))
           (let ((,!heap (huginn.m.r:execution-state-heap ,!execution-state)))
             (declare (type (simple-array huginn.m.r:cell (*)) ,!heap)
                      (ignorable ,!heap))
             ,@(iterate
                 (with flat = (read-flat-representation compilation-state))
                 (for i from start below end)
                 (for marker = (aref flat i))
                 (for arguments =
                      (make-instance
                       'cell-copy-form-arguments
                       :heap-symbol !heap
                       :clause-symbol !clause
                       :execution-state-symbol !execution-state
                       :bindings-fill-pointer-symbol !bindings-fill-pointer
                       :heap-pointer-symbol !heap-pointer
                       :database database
                       :position i))
                 (collect (cell-copy-form marker arguments))))
           ,!bindings-fill-pointer))))


(defmethod optimized-relocate-cells-function ((compilation-state
                                               compilation-state)
                                              database
                                              start end)
  (check-type start huginn.m.r:pointer)
  (check-type end huginn.m.r:pointer)
  (compile nil (generate-copying-lambda-form compilation-state
                                             database
                                             start end)))


(defmethod cell-value-form ((marker lazy-value-mixin) arguments)
  (cl-ds.utils:with-slots-for (arguments unification-form-arguments)
    `(aref ,heap-symbol
           (the fixnum (+ ,pointer-symbol
                          ,(access-object-position marker))))))


(defmethod content-for-unification ((marker list-marker) arguments)
  (cl-ds.utils:with-slots-for (arguments unification-form-arguments)
    (cl-ds:xpr (:i (access-destination marker))
      (let ((result (aref all-markers i)))
        (if (or (typep result 'list-end-marker)
                (typep result 'list-rest-marker))
            (cl-ds:send-finish result)
            (cl-ds:send-recur result :i (1+ i)))))))


(defmethod content-for-unification ((marker expression-marker) arguments)
  (cl-ds.utils:with-slots-for (arguments unification-form-arguments)
    (let ((length (read-arity marker))
          (destination (access-destination marker)))
      (cl-ds:xpr (:i 0)
        (when (<= i length)
          (cl-ds:send-recur (aref all-markers (+ i destination))
                            :i (1+ i)))))))


(defmethod cell-value-form ((marker pointer-mixin) arguments)
  (cl-ds.utils:with-slots-for (arguments unification-form-arguments)
    `(huginn.m.r:tag ,(marker-tag marker)
                     (the huginn.m.r:word
                          (+ ,pointer-symbol ,(access-destination marker))))))


(defmethod cell-value-form ((marker fixnum-marker) arguments)
  (cl-ds.utils:with-slots-for (arguments unification-form-arguments)
    (marker->cell marker (access-object-position marker) database)))


(defmethod cell-value-form ((marker predicate-marker) arguments)
  (cl-ds.utils:with-slots-for (arguments unification-form-arguments)
    (marker->cell marker (access-object-position marker) database)))


(defmethod cell-value-form ((marker list-rest-marker) arguments)
  (cl-ds.utils:with-slots-for (arguments unification-form-arguments)
    (marker->cell marker (access-object-position marker) database)))


(defmethod cell-value-form ((marker unbound-variable-marker) arguments)
  (cl-ds.utils:with-slots-for (arguments unification-form-arguments)
    (marker->cell marker (access-object-position marker) database)))


(defmethod cell-value-form ((marker list-end-marker) arguments)
  (cl-ds.utils:with-slots-for (arguments unification-form-arguments)
    (marker->cell marker (access-object-position marker) database)))


(defun optimized-unify-head-function-form (compilation-state database)
  (let* ((filtered-markers
           (~> compilation-state
               read-flat-representation
               (take (read-body-pointer compilation-state) _)
               (remove-duplicates :test #'eq :from-end t)))
         (!stop-on-failure (gensym "STOP-ON-FAILURE"))
         (eager-markers (~>> filtered-markers
                             (remove-if-not (rcurry #'typep  'eager-value-mixin))))
         (arguments (make-unification-form-arguments
                    (read-flat-representation compilation-state)
                    database)))
    (cl-ds.utils:with-slots-for (arguments unification-form-arguments)
      (if (emptyp filtered-markers)
          `(lambda (,execution-state-symbol ,execution-stack-cell-symbol
                    ,goal-pointer-symbol ,!stop-on-failure)
             (declare (ignore ,execution-stack-cell-symbol
                              ,execution-stack-cell-symbol
                              ,goal-pointer-symbol)
                      (optimize (speed 3) (safety 0) (debug 0) (space 0)))
             t)
          `(lambda (,execution-state-symbol
                    ,execution-stack-cell-symbol
                    ,goal-pointer-symbol
                    ,!stop-on-failure)
             (declare (optimize (speed 3) (debug 0) (safety 0)
                                (space 0) (compilation-speed 0))
                      (type huginn.m.r:execution-stack-cell ,execution-stack-cell-symbol)
                      (type huginn.m.r:execution-state ,execution-state-symbol)
                      (type boolean ,!stop-on-failure)
                      (type huginn.m.r:pointer ,goal-pointer-symbol))
             (block ,function-symbol
               (let* ((,heap-symbol (huginn.m.r:execution-state-heap
                                     ,execution-state-symbol))
                      (,pointer-symbol (huginn.m.r:execution-stack-cell-heap-pointer
                                        ,execution-stack-cell-symbol))
                      ,@(map 'list
                             (lambda (marker)
                               (list (read-value-symbol marker)
                                     (cell-value-form marker arguments)))
                             eager-markers))
                 (declare (ignorable ,heap-symbol ,pointer-symbol
                                     ,@(map 'list #'read-value-symbol
                                            eager-markers))
                          (type (or null huginn.m.r:cell)
                                ,@(map 'list #'read-value-symbol
                                       eager-markers)))
                 (cl-ds.utils:lazy-let
                     ,(~>> filtered-markers
                           (remove-if-not (rcurry #'typep 'lazy-value-mixin))
                           (map 'list
                                (lambda (marker)
                                  (list (read-value-symbol marker)
                                        (cell-value-form marker arguments)))))
                   (labels ((,fail-symbol ()
                              (when ,!stop-on-failure
                                (return-from ,function-symbol nil)))
                            ,@(map 'list
                                (lambda (marker)
                                  (list (read-unification-function-symbol marker)
                                        (list goal-pointer-symbol)
                                        `(declare (type huginn.m.r:pointer ,goal-pointer-symbol))
                                        (cell-store-form marker arguments)))
                                filtered-markers))
                     (declare (inline ,@(map 'list
                                             #'read-unification-function-symbol
                                             filtered-markers)
                                      ,fail-symbol)
                              (ignorable ,@(map 'list
                                                (compose
                                                 (curry #'list 'function)
                                                 #'read-unification-function-symbol)
                                                filtered-markers)))
                     (,(~> filtered-markers first-elt read-unification-function-symbol)
                      ,goal-pointer-symbol)
                     t)))))))))


(defmethod optimized-unify-head-function ((compilation-state compilation-state)
                                          database)
  (let ((*error-output* (make-broadcast-stream)))
    (~>> (optimized-unify-head-function-form compilation-state
                                             database)
         (compile nil))))
