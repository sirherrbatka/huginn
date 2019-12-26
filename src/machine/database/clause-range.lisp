(cl:in-package #:huginn.machine.database)


(defclass clauses-range (cl-ds:fundamental-forward-range)
  ((%recursive-clause-position :initarg :recursive-clause-position
                               :reader read-recursive-clause-position)
   (%reached-end :initarg :reached-end
                 :accessor access-reached-end)
   (%initial-reached-end :reader read-initial-reached-end
                         :initarg :reached-end)
   (%current-clause :initarg :current-clause
                    :accessor access-current-clause)
   (%initial-clause :initarg :current-clause
                    :reader read-initial-clause)
   (%clauses :initarg :clauses
             :reader read-clauses))
  (:default-initargs
   :reached-end nil
   :current-clause 0))


(defun make-clauses-range (clause clauses)
  (declare (type (array t (*)) clauses)
           (optimize (speed 3)))
  (assert (array-has-fill-pointer-p clauses))
  (make
   'clauses-range
   :recursive-clause-position
   (if-let ((recursive (huginn.m.r:clause-recursive-p clause))
            (recur (position clause clauses
                             :test 'eq)))
     recur
     -1)
   :clauses clauses))


(cl-ds.utils:define-list-of-slots clauses-range ()
  (recursive-clause-position read-recursive-clause-position)
  (reached-end access-reached-end)
  (initial-reached-end read-initial-reached-end)
  (current-clause access-current-clause)
  (initial-clause read-initial-clause)
  (clauses read-clauses))


(defmethod cl-ds.utils:cloning-information append (clauses-range)
  '((:current-clause access-current-clause)
    (:clauses read-clauses)
    (:reached-end access-reached-end)
    (:recursive-clause-position read-recursive-clause-position)))


(defmethod cl-ds:reset! ((range clauses-range))
  (cl-ds.utils:with-slots-for (range clauses-range)
    (setf current-clause initial-clause
          reached-end initial-reached-end))
  range)


(defmethod cl-ds:clone ((range clauses-range))
  (cl-ds.utils:with-slots-for (range clauses-range)
    (make (class-of range)
          :clauses clauses
          :current-clause current-clause
          :reached-end reached-end
          :recursive-clause-position recursive-clause-position)))


(defmethod cl-ds:consume-front ((range clauses-range))
  (declare (optimize (speed 3)))
  (cl-ds.utils:with-slots-for (range clauses-range)
    (let* ((vector clauses)
           (length (length vector))
           (position current-clause)
           (end reached-end)
           (recursive-clause recursive-clause-position))
      (declare (type (array t (*)) vector)
               (type fixnum position recursive-clause))
      (assert (array-has-fill-pointer-p vector))
      (when end
        (return-from cl-ds:consume-front (values nil nil)))
      (when (eql position recursive-clause)
        (the fixnum (incf position)))
      (if (< position length)
          (let ((result (aref vector position)))
            (setf current-clause (the fixnum (1+ position)))
            (values result t))
          (if (and (not end)
                   (> recursive-clause -1))
              (progn
                (setf reached-end t)
                (values (aref vector recursive-clause) t))
              (values nil nil))))))


(defmethod cl-ds:peek-front ((range clauses-range))
  (declare (optimize (speed 3)))
  (cl-ds.utils:with-slots-for (range clauses-range)
    (let* ((vector clauses)
           (length (length vector))
           (position current-clause)
           (end reached-end)
           (recursive-clause recursive-clause-position))
      (declare (type (array t (*)) vector)
               (type fixnum position recursive-clause))
      (assert (array-has-fill-pointer-p vector))
      (when end
        (return-from cl-ds:peek-front (values nil nil)))
      (when (eql position recursive-clause)
        (the fixnum (incf position)))
      (if (< position length)
          (let ((result (aref vector position)))
            (values result t))
          (if (and (not end)
                   (> recursive-clause -1))
              (values (aref vector recursive-clause) t)
              (values nil nil))))))


(defmethod cl-ds:traverse ((range clauses-range) function)
  (ensure-functionf function)
  (iterate
    (for (values data more) = (cl-ds:consume-front range))
    (while more)
    (funcall function data)
    (finally (return range))))


(defmethod cl-ds:across ((range clauses-range) function)
  (cl-ds:traverse (cl-ds:clone range) function)
  range)
