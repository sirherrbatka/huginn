(cl:in-package #:huginn-user)


(defmacro with/without-compilation (&body body)
  `(flet ((impl ()
           ,@body))
    (with-options (:compile nil)
      (impl))
    (with-options (:compile t)
      (impl))))

(prove:plan 30)

(with/without-compilation
  (with-options (:database (make-database 'huginn.m.d:database))
    (<- '(lubi zuzia ?cos) '(jest ?cos kot))
    (<- '(jest sansa kot))

    (let ((answer (?- '(lubi zuzia ?cos))))
      (prove:is (cl-ds:consume-front answer) '((?cos . sansa)))
      (prove:is (cl-ds:consume-front answer) nil)))

  (with-options (:database (make-database 'huginn.m.d:database))
    (<- `(member ?item ,(li '(?item . ?rest))))
    (<- `(member ?item ,(li '(? . ?rest)))
        '(member ?item ?rest))
    (<- '(= ?item ?item))

    (let ((answer (?- `(member ?zuzia ,(li `(nie-zuzia zuzia)))
                      '(= ?zuzia zuzia))))
      (prove:is (cl-ds:consume-front answer) '((?zuzia . zuzia)))
      (prove:is (cl-ds:consume-front answer) nil)))

  (with-options (:database (make-database 'huginn.m.d:database))
    (<- `(rest ?rest ,(li '(? . ?rest))))
    (let ((answer (?- `(rest ?rest ,(li `(nie-zuzia zuzia))))))
      (prove:is (cl-ds:consume-front answer) '((?rest . (zuzia))))
      (prove:is (cl-ds:consume-front answer) nil)))

  (with-options (:database (make-database 'huginn.m.d:database))
    (<- `(= ?item ?item))
    (<- `(member ?item ,(li '(?item . ?rest))))
    (<- `(member ?item ,(li '(? . ?rest)))
        '(member ?item ?rest))
    (<- `(s ?item))

    (prove:is (cl-ds.alg:to-list (?- `(= ?list ,(li `(? ? ?)))
                                     `(member (s a) ?list)
                                     `(member (s b) ?list)))
              '(((?LIST :? (S B) (S A)))
                ((?LIST (S B) :? (S A)))
                ((?LIST :? (S A) (S B)))
                ((?LIST (S B) (S A) :?))
                ((?LIST (S A) :? (S B)))
                ((?LIST (S A) (S B) :?)))))

  (with-options (:database (make-database 'huginn.m.d:database))
    (<- `(iright ?left ?right ,(li `(?left ?right . ?))))
    (<- `(iright ?left ?right ,(li `(? . ?rest)))
        `(iright ?left ?right ?rest))
    (<- `(nextto ?x ?y ?list)
        `(iright ?x ?y ?list))
    (<- `(nextto ?x ?y ?list)
        `(iright ?y ?x ?list))

    (prove:is (cl-ds.alg:to-list
               (?- `(iright ?left ?right ,(li '(a b c d e f g)))))
              '(((?LEFT . F) (?RIGHT . G))
                ((?LEFT . E) (?RIGHT . F))
                ((?LEFT . D) (?RIGHT . E))
                ((?LEFT . C) (?RIGHT . D))
                ((?LEFT . B) (?RIGHT . C))
                ((?LEFT . A) (?RIGHT . B))))
    (prove:is (cl-ds.alg:to-list
               (?- `(nextto ?x ?y
                            ,(li '(a b c d e f g)))))
              '(((?X . G) (?Y . F))
                ((?X . F) (?Y . E))
                ((?X . E) (?Y . D))
                ((?X . D) (?Y . C))
                ((?X . C) (?Y . B))
                ((?X . B) (?Y . A))
                ((?X . F) (?Y . G))
                ((?X . E) (?Y . F))
                ((?X . D) (?Y . E))
                ((?X . C) (?Y . D))
                ((?X . B) (?Y . C))
                ((?X . A) (?Y . B)))))

  (with-options (:database (make-database 'huginn.m.d:database))
    (<- `(= ?item ?item))
    (<- `(member ?item ,(li '(?item . ?rest))))
    (<- `(member ?item ,(li '(? . ?rest)))
        '(member ?item ?rest))
    (<- `(h ? ? ?))

    (let ((answer (?- `(= ?list ,(li `(? ? ?)))
                      `(member (h a ? ?) ?list)
                      `(member (h ? b ?) ?list)
                      `(member (h ? ? c) ?list))))
      (prove:is (cl-ds.alg:to-list answer)
                '(((?LIST :? :? (H A B C)))
                  ((?LIST :? (H :? :? C) (H A B :?)))
                  ((?LIST (H :? :? C) :? (H A B :?)))
                  ((?LIST :? (H :? B :?) (H A :? C)))
                  ((?LIST :? (H :? B C) (H A :? :?)))
                  ((?LIST (H :? :? C) (H :? B :?) (H A :? :?)))
                  ((?LIST (H :? B :?) :? (H A :? C)))
                  ((?LIST (H :? B :?) (H :? :? C) (H A :? :?)))
                  ((?LIST (H :? B C) :? (H A :? :?)))
                  ((?LIST :? (H A :? :?) (H :? B C)))
                  ((?LIST :? (H A :? C) (H :? B :?)))
                  ((?LIST (H :? :? C) (H A :? :?) (H :? B :?)))
                  ((?LIST :? (H A B :?) (H :? :? C)))
                  ((?LIST :? (H A B C) :?))
                  ((?LIST (H :? :? C) (H A B :?) :?))
                  ((?LIST (H :? B :?) (H A :? :?) (H :? :? C)))
                  ((?LIST (H :? B :?) (H A :? C) :?))
                  ((?LIST (H :? B C) (H A :? :?) :?))
                  ((?LIST (H A :? :?) :? (H :? B C)))
                  ((?LIST (H A :? :?) (H :? :? C) (H :? B :?)))
                  ((?LIST (H A :? C) :? (H :? B :?)))
                  ((?LIST (H A :? :?) (H :? B :?) (H :? :? C)))
                  ((?LIST (H A :? :?) (H :? B C) :?))
                  ((?LIST (H A :? C) (H :? B :?) :?))
                  ((?LIST (H A B :?) :? (H :? :? C)))
                  ((?LIST (H A B :?) (H :? :? C) :?))
                  ((?LIST (H A B C) :? :?))))))

  (with-options (:database (make-database 'huginn.m.d:database))
    (<- `(= ?item ?item))
    (<- `(member ?item ,(li '(?item . ?rest))))
    (<- `(member ?item ,(li '(? . ?rest)))
        '(member ?item ?rest))
    (<- `(h ? ?))

    (let ((answer (?- `(= ?list ,(li `(?a ? ?)))
                      `(member (h a ?) ?list)
                      `(member (h ? b) ?list)
                      `(member (h ? c) ?list))))
      (prove:is (cl-ds.alg:to-list answer)
                '(((?LIST :? (H :? C) (H A B)) (?A . :?))
                  ((?LIST (H :? C) :? (H A B)) (?A H :? C))
                  ((?LIST :? (H :? B) (H A C)) (?A . :?))
                  ((?LIST (H :? C) (H :? B) (H A :?)) (?A H :? C))
                  ((?LIST (H :? B) :? (H A C)) (?A H :? B))
                  ((?LIST (H :? B) (H :? C) (H A :?)) (?A H :? B))
                  ((?LIST :? (H A C) (H :? B)) (?A . :?))
                  ((?LIST (H :? C) (H A :?) (H :? B)) (?A H :? C))
                  ((?LIST :? (H A B) (H :? C)) (?A . :?))
                  ((?LIST (H :? C) (H A B) :?) (?A H :? C))
                  ((?LIST (H :? B) (H A :?) (H :? C)) (?A H :? B))
                  ((?LIST (H :? B) (H A C) :?) (?A H :? B))
                  ((?LIST (H A :?) (H :? C) (H :? B)) (?A H A :?))
                  ((?LIST (H A C) :? (H :? B)) (?A H A C))
                  ((?LIST (H A :?) (H :? B) (H :? C)) (?A H A :?))
                  ((?LIST (H A C) (H :? B) :?) (?A H A C))
                  ((?LIST (H A B) :? (H :? C)) (?A H A B))
                  ((?LIST (H A B) (H :? C) :?) (?A H A B))))))

  (with-options (:database (make-database 'huginn.m.d:database))
    (<- `(iright ?left ?right ,(li `(?left ?right . ?))))
    (<- `(iright ?left ?right ,(li `(? . ?rest)))
        `(iright ?left ?right ?rest))
    (<- `(nextto ?x ?y ?list)
        `(iright ?x ?y ?list))
    (<- `(nextto ?x ?y ?list)
        `(iright ?y ?x ?list))

    (prove:is (cl-ds.alg:to-list (?- `(iright ?left ?right
                                              ,(li '(a b c d e f g)))))
              '(((?LEFT . F) (?RIGHT . G))
                ((?LEFT . E) (?RIGHT . F))
                ((?LEFT . D) (?RIGHT . E))
                ((?LEFT . C) (?RIGHT . D))
                ((?LEFT . B) (?RIGHT . C))
                ((?LEFT . A) (?RIGHT . B))))


    (prove:is (cl-ds.alg:to-list (?- `(iright ?left ?right ,(li '(a b c d e f g)))))
              '(((?LEFT . F) (?RIGHT . G))
                ((?LEFT . E) (?RIGHT . F))
                ((?LEFT . D) (?RIGHT . E))
                ((?LEFT . C) (?RIGHT . D))
                ((?LEFT . B) (?RIGHT . C))
                ((?LEFT . A) (?RIGHT . B))))

    (prove:is (cl-ds.alg:to-list (?- `(nextto ?x ?y ,(li '(a b c d e f g)))))
              '(((?X . G) (?Y . F))
                ((?X . F) (?Y . E))
                ((?X . E) (?Y . D))
                ((?X . D) (?Y . C))
                ((?X . C) (?Y . B))
                ((?X . B) (?Y . A))
                ((?X . F) (?Y . G))
                ((?X . E) (?Y . F))
                ((?X . D) (?Y . E))
                ((?X . C) (?Y . D))
                ((?X . B) (?Y . C))
                ((?X . A) (?Y . B)))))

  (with-options (:database (make-database 'huginn.m.d:database))
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
        `(member (house ukrainian ? ? tea ?) ?houses))

    (prove:is (cl-ds:consume-front (?- '(zebra ?houses)))
              '((?HOUSES (HOUSE NORWEGIAN :? :? :? :?) (HOUSE UKRAINIAN :? :? TEA :?)
                 (HOUSE SPANIARD DOG :? MILK :?) (HOUSE :? SNAILS OLD-GOLD COFFE GREEN)
                 (HOUSE :? ZEBRA KOOLS WATER YELLOW))))))

(prove:finalize)
