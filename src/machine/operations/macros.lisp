(cl:in-package #:huginn.machine.operations)


(defmacro with-unification-stack ((execution-state)
                                    &body body)
    (with-gensyms (!ustack !fill-pointer !state !start !block)
      `(let* ((,!state ,execution-state)
              (,!ustack (huginn.m.r:execution-state-unification-stack ,!state)))
         (declare (ignorable ,!ustack)
                  (type (cl-ds.utils:extendable-vector fixnum) ,!ustack))
         (assert (array-has-fill-pointer-p ,!ustack))
         (macrolet ((upush (stack-pointer goal-pointer)
                      `(progn
                         (vector-push-extend ,stack-pointer ,',!ustack)
                         (vector-push-extend ,goal-pointer ,',!ustack)))
                    (upop ((stack-pointer goal-pointer)
                           &body this-body)
                      `(let* ((,',!fill-pointer (fill-pointer ,',!ustack))
                              (,goal-pointer
                                (aref ,',!ustack
                                      (the fixnum (1- ,',!fill-pointer))))
                              (,stack-pointer
                                (aref ,',!ustack
                                      (- ,',!fill-pointer 2))))
                         (decf (fill-pointer ,',!ustack) 2)
                         ,@this-body))
                    (uclear ()
                      `(setf (fill-pointer ,',!ustack) 0))
                    (uemptyp ()
                      `(zerop (fill-pointer ,',!ustack)))
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
