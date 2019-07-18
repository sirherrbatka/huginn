(cl:in-package #:huginn)


(defclass answers-stream (cl-ds:chunking-mixin
                          cl-ds:fundamental-forward-range)
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


(defmethod cl-ds:peek-front ((range answers-stream))
  (~> range cl-ds:clone cl-ds:consume-front))


(defmethod cl-ds:traverse ((range answers-stream) function)
  (ensure-functionf function)
  (iterate
    (for (values data more) = (cl-ds:consume-front range))
    (while more)
    (funcall function data))
  range)


(defmethod cl-ds:across ((range answers-stream) function)
  (cl-ds:traverse (cl-ds:clone range) function)
  range)


(defun expression-from-heap (execution-state pointer)
  (iterate
    (with arity = (huginn.m.r:dereference-heap-pointer execution-state
                                                       (1+ pointer)))
    (with result = `(,(huginn.m.d:predicate-from-cell/word
                       (huginn.m.r:execution-state-database execution-state)
                       (huginn.m.r:dereference-heap-pointer execution-state
                                                            pointer))))
    (for p from (+ pointer 2))
    (repeat (1- arity))
    (push (dereference-pointer execution-state p)
          result)
    (finally (return (nreversef result)))))


(defun handle-cell (execution-state pointer cell)
  (huginn.m.r:tag-case (cell)
    :expression (expression-from-heap execution-state pointer)
    :variable (if (huginn.m.r:variable-unbound-p cell)
                  (error 'variable-binding-failed)
                  (huginn.m.r:dereference-variable execution-state
                                                   cell))
    :reference (dereference-pointer execution-state
                                    (huginn.m.r:follow-pointer
                                     execution-state
                                     pointer t))
    :predicate (huginn.m.d:predicate-from-cell/word
                (huginn.m.r:execution-state-database execution-state)
                cell)
    :fixnum (huginn.m.r:detag cell)
    :list-start (list-from-heap execution-state pointer)
    ))


(more-conditions:define-condition-translating-method
    dereference-pointer-with-condition-translation
    (execution-state pointer variable-symbol)
  ((huginn.m.r:variable-dereference-error cant-bind-variable-error)
   :variable-symbol variable-symbol)
  ((huginn.m.d:predicate-dereference-error cant-bind-predicate-error)))


(defun dereference-pointer (execution-state pointer)
  (~>> (huginn.m.r:dereference-heap-pointer execution-state pointer t)
       (handle-cell execution-state pointer)))


(defmethod dereference-pointer-with-condition-translation
    (execution-state pointer variable-symbol)
  (dereference-pointer execution-state pointer))


(defun list-from-heap (execution-state pointer)
  (let ((result '()))
    (huginn.m.r:scan-heap-list (lambda (pointer cell)
                                 (push (handle-cell execution-state
                                                    pointer cell)
                                       result))
                               execution-state
                               (~> (huginn.m.r:dereference-heap-pointer
                                    execution-state pointer)
                                   huginn.m.r:detag))
    (nreversef result)))


(defun extract-variable-bindings (execution-state variables pointers)
  (mapcar (lambda (variable pointer)
            (~>> (restart-case
                     (dereference-pointer-with-condition-translation
                      execution-state pointer variable)
                   (self-bind () variable)
                   (use-value (new-value)
                     :interactive read
                     new-value))
                 (list* variable)))
          variables
          pointers))


(defmethod cl-ds:consume-front ((range answers-stream))
  (let* ((execution-state (access-execution-state range))
         (variables (read-variables range))
         (pointers (read-pointers range)))
    (iterate
      (restart-case
          (let ((answer-found-p (huginn.m.o:find-answer execution-state)))
            (unless answer-found-p
              (return-from cl-ds:consume-front (values nil nil)))
            (let ((result (extract-variable-bindings execution-state
                                                     variables
                                                     pointers)))
              (huginn.m.o:pop-stack-cells-until-goal execution-state)
              (return-from cl-ds:consume-front (values result t))))
        (go-to-next-answer ()
          (huginn.m.o:pop-stack-cells-until-goal execution-state))))))


(defmethod cl-ds:reset! ((range answers-stream))
  (setf (access-execution-state range)
        (~> range read-initial-execution-state
            huginn.m.r:clone-execution-state))
  range)


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
