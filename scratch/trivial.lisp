(cl:in-package #:huginn-user)

(defparameter *data* (make-database 'huginn.m.d:database t))

(progn
  (clear)

  (<- '(lubi zuzia ?cos) '(jest ?cos kot))
  (<- '(jest sansa kot))

  (defparameter *answer* (?- '(lubi zuzia ?cos)))

  (print (cl-ds:consume-front *answer*)))

(progn
  (clear)

  (<- `(member ?item ,(li '(?item . ?rest))))
  (<- `(member ?item ,(li '(? . ?rest)))
      '(member ?item ?rest))
  (<- '(= ?item ?item))

  (defparameter *answer* (?- `(member ?zuzia ,(li `(nie-zuzia zuzia)))
                             '(= ?zuzia zuzia)))

  (print (cl-ds:consume-front *answer*))
  (print (cl-ds:consume-front *answer*)))

(progn
  (clear)
  (<- `(rest ?rest ,(li '(? . ?rest))))
  (defparameter *answer* (?- `(rest ?rest ,(li `(nie-zuzia zuzia)))))
  (print (cl-ds:consume-front *answer*))
  )
