(cl:in-package #:huginn.machine.operations)


(defmacro with-unification-stack ((execution-state)
                                    &body body)
    (with-gensyms (!ustack !fill-pointer !length !state !start !block)
      `(let* ((,!state ,execution-state)
              (,!ustack (huginn.m.r:execution-state-unification-stack ,!state)))
         (declare (ignorable ,!ustack)
                  (type (simple-array fixnum (*)) ,!ustack))
         (macrolet ((upush (stack-pointer goal-pointer)
                      `(let ((,',!length (length ,',!ustack))
                             (,',!fill-pointer
                               (huginn.m.r:execution-state-unification-stack-fill-pointer
                                ,',!state)))
                         (unless (< (the fixnum (+ 2 ,',!fill-pointer))
                                    ,',!length)
                           (setf ,',!ustack (adjust-array ,',!ustack (ash ,',!length 2))
                                 (huginn.m.r:execution-state-unification-stack ,',!state) ,',!ustack))
                         (setf (aref ,',!ustack ,',!fill-pointer) ,goal-pointer)
                         (setf (aref ,',!ustack (1+ ,',!fill-pointer)) ,stack-pointer)
                         (the fixnum (incf (huginn.m.r:execution-state-unification-stack-fill-pointer
                                            ,',!state) 2))))
                    (upop ((stack-pointer goal-pointer)
                           &body this-body)
                      `(let* ((,',!fill-pointer
                                (huginn.m.r:execution-state-unification-stack-fill-pointer
                                 ,',!state))
                              (,goal-pointer
                                (aref ,',!ustack
                                      (the fixnum (1- ,',!fill-pointer))))
                              (,stack-pointer
                                (aref ,',!ustack
                                      (- ,',!fill-pointer 2))))
                         (the fixnum
                              (decf (huginn.m.r:execution-state-unification-stack-fill-pointer
                                     ,',!state) 2))
                         ,@this-body))
                    (uclear ()
                      `(setf (huginn.m.r:execution-state-unification-stack-fill-pointer
                              ,',!state)
                             0))
                    (uemptyp ()
                      `(zerop (huginn.m.r:execution-state-unification-stack-fill-pointer
                               ,',!state)))
                    (deref (pointer)
                      `(huginn.m.r:dereference-heap-pointer ,',!state
                                                            ,pointer))
                    (next ()
                      `(go ,',!start))
                    (done (result)
                      `(return-from ,',!block ,result)))
           (block ,!block
             (tagbody
                ,!start
                ,@body))))))
