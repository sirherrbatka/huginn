(cl:in-package #:huginn-user)

(defparameter *data* (make-database 'huginn.m.d:database t))

(progn
  (clear)

  (<- `(house ? ? ? ? ?))
  (<- `(= ?item ?item))
  (<- `(member ?item ,(li '(?item . ?rest))))
  (<- `(member ?item ,(li '(? . ?rest)))
      '(member ?item ?rest))
  (<- `(iright ?left ?right ,(li `(?left ?right . ?))))
  (<- `(iright ?left ?right ,(li `(? . ?rest)))
      `(iright ?left ?right ?rest))
  (<- `(nextto ?x ?y ?list)
      `(iright ?x ?y ?list))
  (<- `(nextto ?x ?y ?list)
      `(iright ?y ?x ?list))

  (<- `(zebra ?houses)
      `(= ?houses ,(li '((house norwegian ? ? ? ?)
                         ?
                         (house ? ? ? milk ?)
                         ?
                         ?)))
      `(member (house englishman ? ? ? red) ?houses)
      `(member (house spaniard dog ? ? ?) ?houses)
      `(member (house ? ? ? coffe green) ?houses)
      `(member (house ukrainian ? ? tea ?) ?houses)
      `(iright (house ? ? ? ? ivory)
               (house ? ? ? ? green)
               ?houses)
      `(member (house ? snails old-gold ? ?) ?houses)
      `(member (house ? ? kools ? yellow) ?houses)
      `(nextto (house ? ? chesterfield ? ?)
               (house ? fox ? ? ?)
               ?houses)
      `(nextto (house ? ? kools ? ?)
               (house ? horse ? ? ?)
               ?houses)
      `(member (house ? ? luckystrike orange-juice ?)
               ?houses)
      `(member (house japanese ? parliments ? ?)
               ?houses)
      `(nextto (house norwegian ? ? ? ?)
               (house ? ? ? ? blue)
               ?houses))

  (defparameter *answer* (?- '(zebra ?houses)))
  (print (cl-ds:consume-front *answer*)))
