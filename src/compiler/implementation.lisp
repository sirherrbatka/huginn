(cl:in-package #:huginn.compiler)


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


(defstruct expression-marker (content))


(defstruct predicate-marker (content))


(defun list-needs-rest-marker-p (list)
  (check-type list list)
  (~> list last rest null not))


(defun list-input-end (input)
  (~> input list-input-content last rest))


(defvar *list-end-marker* (make-list-end-marker))


(defun flat-representation (expression &optional (result (vect)))
  (declare (optimize (speed 1) (debug 3)))
  (unless (endp expression)
    (check-type expression expression)
    (bind (((:labels impl (exp))
            (cond ((and (expressionp exp)
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
                     (impl e)))
                  ((and (list-input-p exp)
                        (not (find-if (lambda (x)
                                        (and (list-marker-p x)
                                             (~> x list-marker-content
                                                 (eq (list-input-content exp)))))
                                      result)))
                   (vector-push-extend (make-list-marker
                                        :content (list-input-content exp))
                                       result)
                   (let ((content (list-input-content exp))
                         (old-size (length result)))
                     (iterate
                       (for e on content)
                       (while (consp e))
                       (vector-push-extend (first e) result))
                     (if (list-needs-rest-marker-p content)
                         (vector-push-extend (make-list-rest-marker
                                              :content (list-input-end exp))
                                             result)
                         (vector-push-extend *list-end-marker* result))
                     (iterate
                       (for i from old-size below (length result))
                       (for e = (aref result i))
                       (impl e))))))
           (initial-size (length result)))
      (impl expression)
      (iterate
        (for i from (1+ initial-size) below (length result))
        (when (~>> (1- i) (aref result) expression-marker-p)
          (setf #1=(aref result i) (make-predicate-marker :content #1#))))))
  result)


(defmethod content ((state compilation-state)
                    (database huginn.m.d:database))
  (declare (optimize (speed 1) (debug 3)))
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
        ((predicate-marker-p elt)
         (let ((predicate (predicate-marker-content elt)))
           (cond ((anonymus-variable-p predicate)
                  (add (huginn.m.r:tag huginn.m.r:+predicate+ 0)))
                 ((variablep predicate)
                  (let ((pointer (pointer-for-predicate state predicate)))
                    (assert (not (null pointer)))
                    (if (eql pointer index)
                        (add (huginn.m.r:tag huginn.m.r:+predicate+ 0))
                        (add (huginn.m.r:tag huginn.m.r:+reference+ pointer)))))
                 (t (~>> predicate
                         (huginn.m.d:index-predicate database)
                         (huginn.m.r:tag huginn.m.r:+predicate+)
                         add)))))
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
        ((list-marker-p elt)
         ;; This will not be placed into the byte code.
         nil)
        ((list-rest-marker-p elt)
         (let ((pointer (~>> elt list-rest-marker-content
                             (pointer-for-variable state))))
           (assert (not (null pointer)))
           (if (eql pointer index)
               (add (huginn.m.r:tag huginn.m.r:+list-rest+ 0))
               (add (huginn.m.r:tag huginn.m.r:+reference+ pointer)))))
        ((list-end-marker-p elt)
         (add (huginn.m.r:tag huginn.m.r:+list-end+ 0)))
        ((list-input-p elt)
         (let ((pointer (pointer-for-list state elt)))
           (assert (not (null pointer)))
           (assert (not (eql pointer index)))
           (add (huginn.m.r:tag huginn.m.r:+list-start+ pointer))))
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
  (declare (optimize (speed 1)))
  (+ (- end start)
     (count-if #'expression-marker-p flat-form :end end
                                               :start start)
     (- (count-if #'list-marker-p flat-form :end end
                                            :start start))))


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
      (unless (goalp b)
        (error 'invalid-goal :form b))
      (flat-representation b flat-form))
    (make 'compilation-state
          :head head
          :body body
          :flat-representation flat-form
          :body-pointer body-pointer)))


(defun pointer-for (flat-form predicate
                        &key
                          (start 0)
                          (end (length flat-form)))
  (iterate
    (for i from start below end)
    (for elt in-vector flat-form)
    (finding pointer such-that (funcall predicate elt))
    (sum (cond ((expression-marker-p elt) 2)
               ((list-marker-p elt) 0)
               (t 1))
         into pointer)))


(defmethod pointer-for-list ((state compilation-state)
                             (list list-input))
  (pointer-for (read-flat-representation state)
               (lambda (elt)
                 (and (list-marker-p elt)
                      (eq (list-marker-content elt)
                          (list-input-content list))))))


(defmethod pointer-for-expression ((state compilation-state)
                                   expression)
  (check-type expression expression)
  (pointer-for (read-flat-representation state)
               (lambda (elt)
                 (and (expression-marker-p elt)
                      (eq (expression-marker-content elt)
                          expression)))))


(defun collect-range (compilation-state start end &key (key #'identity) (predicate (constantly t)))
  (check-type start non-negative-fixnum)
  (check-type end non-negative-fixnum)
  (assert (<= start end))
  (iterate
    (with flat-form = (read-flat-representation compilation-state))
    (for elt in-vector flat-form)
    (when (and (>= sum start) (funcall predicate elt))
      (collect (funcall key elt)))
    (sum (if (expression-marker-p elt) 2 1) into sum)
    (while (< sum end))))


(defmethod expressions ((compilation-state compilation-state) start end)
  (collect-range compilation-state start end
                 :key #'expression-marker-content
                 :predicate #'expression-marker-p))


(defmethod variables ((compilation-state compilation-state) start end)
  (~> (collect-range compilation-state start end
                     :predicate (cl-ds.utils:or* #'variablep
                                                 #'list-rest-marker-p)
                     :key (lambda (x)
                            (if (list-rest-marker-p x)
                                (list-rest-marker-content x)
                                x)))
      remove-duplicates))


(defmethod pointer-for-variable ((state compilation-state)
                                 variable)
  (check-type variable variable)
  (or (pointer-for (read-flat-representation state)
                   (lambda (elt)
                     (and (list-rest-marker-p elt)
                          (eq (list-rest-marker-content elt)
                              variable))))
      (pointer-for (read-flat-representation state)
                   (lambda (elt)
                     (eq elt variable)))))


(defmethod pointer-for-predicate ((state compilation-state)
                                  predicate)
  (check-type predicate predicate)
  (pointer-for (read-flat-representation state)
               (lambda (elt) (and (predicate-marker-p elt)
                                  (eq (predicate-marker-content elt)
                                      predicate)))))


(defmethod predicates ((state compilation-state)
                       start end)
  (collect-range compilation-state start end
                 :predicate #'predicate-marker-p
                 :key #'predicate-marker-content))


(defmethod predicate ((state compilation-state))
  (~> state head clause-head-predicate))


(defmethod print-object ((state fundamental-compilation-state) stream)
  (print-unreadable-object (state stream :type nil)
    (format stream "<- ~a ~a" (head state) (body state))))


(defmethod variable-bindings ((compilation-state compilation-state))
  (~>> compilation-state read-flat-representation
       (remove-if (cl-ds.utils:or* #'variablep
                                   #'inlined-fixnum-p
                                   #'expression-marker-p
                                   #'predicate-marker-p))
       delete-duplicates))
