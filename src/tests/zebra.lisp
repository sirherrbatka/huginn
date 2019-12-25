(cl:in-package #:huginn-tests)


(prove:plan 4)

(with/without-compilation
  (with-options (:database (make-database 'huginn.m.d:database))
    (<- `(house ? ? ? ? ?))
    (<- `(= ?item ?item))
    (<- `(member ?item ,(li '(?item . ?rest))))
    (<- `(member ?item ,(li '(? . ?rest)))
        (recur '(member ?item ?rest)))
    (<- `(iright ?left ?right ,(li `(?left ?right . ?))))
    (<- `(iright ?left ?right ,(li `(? . ?rest)))
        (recur `(iright ?left ?right ?rest)))
    (<- `(nextto ?x ?y ?list)
        `(iright ?x ?y ?list))
    (<- `(nextto ?x ?y ?list)
        `(iright ?y ?x ?list))

    (<- `(zebra ?houses ?water-drinker ?zebra-owner)
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
                 ?houses)
        `(member (house ?water-drinker ? ? water ?) ?houses)
        `(member (house ?zebra-owner zebra ? ? ?) ?houses))

    (let ((answer (?- '(zebra ?houses ?water-drinker ?zebra-owner))))
      (prove:is (cl-ds:consume-front answer)
                '((?HOUSES (HOUSE NORWEGIAN FOX KOOLS WATER YELLOW)
                   (HOUSE UKRAINIAN HORSE CHESTERFIELD TEA BLUE)
                   (HOUSE ENGLISHMAN SNAILS OLD-GOLD MILK RED)
                   (HOUSE SPANIARD DOG LUCKYSTRIKE ORANGE-JUICE IVORY)
                   (HOUSE JAPANESE ZEBRA PARLIMENTS COFFE GREEN))
                  (?WATER-DRINKER . NORWEGIAN) (?ZEBRA-OWNER . JAPANESE)))
      (prove:is (cl-ds:consume-front answer) nil))))

(prove:finalize)
