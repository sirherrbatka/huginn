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


(defmethod content ((state compilation-state)
                    (database huginn.m.d:database))
  (declare (optimize (speed 1) (debug 3)))
  (bind ((result (make-array (cells-count state)
                             :element-type 'huginn.m.r:cell))
         ((:slots %flat-representation) state)
         (index 0)
         ((:flet add (item))
          (setf (aref result index) item)
          (incf index)))
    (iterate
      (for elt in-vector %flat-representation)
      (~> elt (marker->cell index database) add))
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
