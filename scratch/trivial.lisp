(cl:in-package #:huginn-user)

(defparameter *data* (make-database 'huginn.m.d:database t))
(clear)

(<- '(lubi zuzia ?cos) '(jest ?cos kot))
(<- '(jest sansa kot))

(defparameter *answer* (?- '(lubi zuzia ?cos)))

(print (cl-ds:consume-front *answer*))

(<- `(member ?item ,(li '(?item . ?rest))))

(<- `(member ?item ,(li '(? . ?rest)))
    '(member ?item ?rest))
