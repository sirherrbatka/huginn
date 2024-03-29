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
  (~> range cl-ds:clone next-answer))


(defmethod cl-ds:traverse ((range answers-stream) function)
  (ensure-functionf function)
  (iterate
    (for (values data more) = (next-answer range))
    (while more)
    (funcall function (print data)))
  range)


(defmethod cl-ds:across ((range answers-stream) function)
  (cl-ds:traverse (cl-ds:clone range) function)
  range)


(defun expression-from-heap (execution-state pointer)
  (let* ((expression-cell (huginn.m.r:dereference-heap-pointer execution-state
                                                               pointer nil))
         (expression-pointer (huginn.m.r:detag expression-cell))
         (arity (~> (huginn.m.r:dereference-heap-pointer execution-state
                                                         expression-pointer)
                    huginn.m.r:detag))
         (predicate-cell (huginn.m.r:dereference-heap-pointer
                          execution-state
                          (+ 1 expression-pointer))))
    (assert (> arity 0))
    (assert (huginn.m.r:expression-cell-p expression-cell))
    (assert (huginn.m.r:predicate-cell-p predicate-cell))
    (iterate
      (with predicate = (huginn.m.d:predicate-from-cell/word
                         (huginn.m.r:execution-state-database execution-state)
                         predicate-cell))
      (with result = (list predicate))
      (for p from (+ expression-pointer 2))
      (repeat (1- arity))
      (push (dereference-pointer execution-state p)
            result)
      (finally (return (nreversef result))))))


(defun handle-cell (execution-state pointer cell)
  (huginn.m.r:tag-case (cell)
    :expression (expression-from-heap execution-state pointer)
    :variable (if (huginn.m.r:variable-unbound-p cell)
                  :?
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
    :list-rest (list-from-heap execution-state pointer)))


(more-conditions:define-condition-translating-method
    dereference-pointer-with-condition-translation
    (execution-state pointer variable-symbol)
  ((huginn.m.r:variable-dereference-error cant-bind-variable-error)
   :variable-symbol variable-symbol)
  ((huginn.m.d:predicate-dereference-error cant-bind-predicate-error)))


(defun dereference-pointer (execution-state pointer)
  (let* ((pointer (huginn.m.r:follow-pointer execution-state pointer t))
         (cell (huginn.m.r:dereference-heap-pointer execution-state pointer nil)))
    (handle-cell execution-state pointer cell)))


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


(defun store-shared-resources (shared-resources execution-state)
  (declare (type (or null shared-resources) shared-resources)
           (type huginn.m.r:execution-state execution-state)
           (optimize (speed 3)))
  (unless (null shared-resources)
    (setf (shared-resources-heap shared-resources)
          (huginn.m.r:execution-state-heap execution-state)

          (shared-resources-unification-stack shared-resources)
          (huginn.m.r:execution-state-unification-stack execution-state)

          (shared-resources-unwind-trail shared-resources)
          (huginn.m.r:execution-state-unwind-trail execution-state))))


(defun next-answer (range)
  (declare (type answers-stream range)
           (optimize (debug 0) (speed 3)))
  (let* ((execution-state (access-execution-state range))
         (variables (read-variables range))
         (pointers (read-pointers range)))
    (iterate
      (restart-case
          (let ((answer-found-p (huginn.m.o:find-answer execution-state)))
            (store-shared-resources *shared-resources* execution-state)
            (unless answer-found-p
              (return-from next-answer (values nil nil)))
            (let ((result (extract-variable-bindings execution-state
                                                     variables
                                                     pointers)))
              (huginn.m.o:pop-stack-cells-until-goal execution-state)
              (return-from next-answer (values result t))))
        (go-to-next-answer ()
          (huginn.m.o:pop-stack-cells-until-goal execution-state))
        (end ()
          (return-from next-answer (values nil nil)))))))


(defmethod cl-ds:consume-front ((range answers-stream))
  (next-answer range))


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
