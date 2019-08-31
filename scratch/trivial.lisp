(cl:in-package #:huginn-user)

(defparameter *data* (make-database 'huginn.m.d:database t))

(progn
  (clear)

  (<- '(lubi zuzia ?cos) '(jest ?cos kot))
  (<- '(jest sansa kot))

  (defparameter *answer* (?- '(lubi zuzia ?cos)))

  (print (cl-ds:consume-front *answer*))
  (print (cl-ds:consume-front *answer*))
  )

(progn
  (clear)

  (<- `(member ?item ,(li '(?item . ?rest))))
  (<- `(member ?item ,(li '(? . ?rest)))
      '(member ?item ?rest))
  (<- ' (= ?item ?item))

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

(progn
  (clear)

  (<- `(= ?item ?item))
  (<- `(member ?item ,(li '(?item . ?rest))))
  (<- `(member ?item ,(li '(? . ?rest)))
      '(member ?item ?rest))
  (<- `(s ?item))

  (defparameter *answer* (?- `(= ?list ,(li `(? ? ?)))
                             `(member (s a) ?list)
                             `(member (s b) ?list)))
  (cl-ds:traverse *answer*
                  (lambda (x)
                    (print x))))

(progn
  (clear)

  (<- `(iright ?left ?right ,(li `(?left ?right . ?))))
  (<- `(iright ?left ?right ,(li `(? . ?rest)))
      `(iright ?left ?right ?rest))
  (<- `(nextto ?x ?y ?list)
      `(iright ?x ?y ?list))
  (<- `(nextto ?x ?y ?list)
      `(iright ?y ?x ?list))

  (cl-ds:traverse (?- `(iright ?left ?right
                               ,(li '(a b c d e f g))))
                  #'print)
  (cl-ds:traverse (?- `(nextto ?x ?y
                               ,(li '(a b c d e f g))))
                  #'print))

(progn
  (clear)

  (<- `(= ?item ?item))
  (<- `(member ?item ,(li '(?item . ?rest))))
  (<- `(member ?item ,(li '(? . ?rest)))
      '(member ?item ?rest))
  (<- `(h ? ? ?))

  (defparameter *answer* (?- `(= ?list ,(li `(? ? ?)))
                             `(member (h a ? ?) ?list)
                             `(member (h ? b ?) ?list)
                             `(member (h ? ? c) ?list)))
  (cl-ds:traverse *answer*
                  (lambda (x)
                    (print x))))

(progn
  (clear)

  (<- `(= ?item ?item))
  (<- `(member ?item ,(li '(?item . ?rest))))
  (<- `(member ?item ,(li '(? . ?rest)))
      '(member ?item ?rest))
  (<- `(h ? ?))

  (defparameter *answer* (?- `(= ?list ,(li `(?a ? ?)))
                             `(member (h a ?) ?list)
                             `(member (h ? b) ?list)
                             `(member (h ? c) ?list)))
  (cl-ds:traverse *answer*
                  (lambda (x)
                    (print x))))

(progn
  (clear)

  (<- `(iright ?left ?right ,(li `(?left ?right . ?))))
  (<- `(iright ?left ?right ,(li `(? . ?rest)))
      `(iright ?left ?right ?rest))
  (<- `(nextto ?x ?y ?list)
      `(iright ?x ?y ?list))
  (<- `(nextto ?x ?y ?list)
      `(iright ?y ?x ?list))

  (cl-ds:traverse (?- `(iright ?left ?right
                               ,(li '(a b c d e f g))))
                  #'print)
  (cl-ds:traverse (?- `(nextto ?x ?y
                               ,(li '(a b c d e f g))))
                  #'print))
(progn
  (clear)

  (<- `(house ? ? ? ? ?))
  (<- `(= ?item ?item))
  (<- `(member ?item ,(li '(?item . ?rest))))
  (<- `(member ?item ,(li '(? . ?rest)))
      '(member ?item ?rest))

  (<- `(zebra ?houses)
      `(= ?houses ,(li '((house norwegian ? ? ? ?) ? (house ? ? ? milk ?) ? ?)))
      `(member (house ?zebra-owner zebra ? ? ?) ?houses)
      `(member (house ?water-drinker ? ? water ?) ?houses)
      `(member (house ? snails old-gold ? ?) ?houses)
      `(member (house ? ? kools ? yellow) ?houses)
      `(member (house spaniard dog ? ? ?) ?houses)
      `(member (house ? ? ? coffe green) ?houses)
      `(member (house ukrainian ? ? tea ?) ?houses)
  )

  (defparameter *answer* (?- '(zebra ?houses)))
  (print (cl-ds:consume-front *answer*))
  )
