(cl:in-package #:huginn-user)

(defparameter *data* (make-database 'huginn.m.d:database t))

(defmacro house (nation pet smokes drinks house-color)
  ``(house ,',nation ,',pet ,',smokes ,',drinks ,',house-color))

(progn
  (clear)

  (<- `(= ?item ?item))
  (<- `(member ?item ,(li '(?item . ?rest))))
  (<- `(member ?item ,(li '(? . ?rest)))
      '(member ?item ?rest))

  (<- `(zebra ?houses ?water-drinker ?zebra-owner)
      `(= ?houses ,(li '((house norway ? ? ? ?) . ?)))
      `(member (house england ? ? ? red) ?houses)
      `(member (house spain dog ? ? ?) ?houses)
      `(member (house ? ? ? coffe green) ?houses)
      `(member (house ukrain ? ? tea ?) ?houses)
      `(iright (house ? ? ? ? ivory)
               (house ? ? ? ? green)
               ?houses)
      `(member (house ? snails winston ? ?) ?houses)
      `(member (house ? ? kools ? yellow) ?houses)
      `(nextto (house ? ? chesterfield ? ?)
               (house ? fox ? ? ?)
               ?houses)
      `(nextto (house ? ? kools ? ?)
               (house ? horse ? ? ?)
               ?houses)
      `(member (house ? ? luckystrike orange-juice ?)
               ?houses)
      `(member (house japan ? parliments ? ?)
               ?houses)
      `(nextto (house norway ? ? ? ?)
               (house ? ? ? ? blue)
               ?houses))

  (defparameter *answer* (?- '(zebra ?houses ?water-drinker ?zebra-owner)))
  (print (cl-ds:consume-front *answer*)))
