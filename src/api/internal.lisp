(cl:in-package #:huginn)


(defclass answers-stream (cl-ds:fundamental-forward-range)
  ((%variables :initarg :variables
               :type list
               :reader read-variables)
   (%pointers :initarg :pointers
              :type list
              :reader read-pointers)
   (%execution-state :initarg :execution-state
                     :type huginn.m.r:execution-state
                     :accessor access-execution-state)
   (%initial-execution-state :initarg :execution-state
                             :type huginn.m.r:execution-state
                             :reader read-initial-execution-state)))


(defmethod cl-ds:clone ((range answers-stream))
  (let* ((execution-state (access-execution-state range))
         (execution-state-clone-1 #1=(huginn.m.r:clone-execution-state
                                      execution-state))
         (execution-state-clone-2 #1#)
         (result (make (class-of range)
                       :variables (read-variables range)
                       :pointers (read-pointers range)
                       :execution-state execution-state-clone-1)))
    (setf (access-execution-state range) execution-state-clone-2)
    result))


(defun dereference-variable-pointer (execution-state pointer)
  (declare (optimize (debug 3)))
  (let ((result (huginn.m.r:dereference-heap-pointer
                 execution-state
                 pointer t)))
    (cond ((huginn.m.r:expression-cell-p result)
           :todo)
          ((huginn.m.r:variable-cell-p result)
           (huginn.m.r:dereference-variable execution-state
                                            result))
          ((huginn.m.r:fixnum-cell-p result)
           (huginn.m.r:detag result))
          (t (assert nil)))))


(defun extrack-variable-bindings (execution-state variables pointers)
  (mapcar (lambda (variable pointer)
            (~>> pointer
                 (dereference-variable-pointer execution-state)
                 (list* variable)))
          variables
          pointers))


(defmethod cl-ds:consume-front ((range answers-stream))
  (let* ((execution-state (access-execution-state range))
         (variables (read-variables range))
         (pointers (read-pointers range))
         (answer-found-p (huginn.m.o:find-answer execution-state)))
    (unless answer-found-p
      (return-from cl-ds:consume-front (values nil nil)))
    (values (extrack-variable-bindings execution-state
                                       variables
                                       pointers))))


(defun wrap-into-answers-range (execution-state compilation-state)
  (let* ((variables (~>> compilation-state
                         huginn.c:cells-count
                         (huginn.c:variables compilation-state 0)))
         (variable-pointers (mapcar (curry #'huginn.c:pointer-for-variable
                                           compilation-state)
                                    variables))
         (result (make 'answers-stream
                       :pointers variable-pointers
                       :execution-state execution-state
                       :variables variables)))
    (setf (access-execution-state result)
          (huginn.m.r:clone-execution-state execution-state))
    result))