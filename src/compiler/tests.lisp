(cl:in-package #:huginn.compiler)

(prove:plan 15)

(let* ((head '(a ?b))
       (compilation-state (make-compilation-state 'compilation-state
                                                  `(,head . (c ?b)))))
  (prove:is (body-pointer compilation-state)
            4)
  (prove:is (predicate compilation-state)
            'a)
  (let ((expressions (expressions compilation-state 0 4)))
    (prove:is (length expressions) 1)
    (prove:is (first expressions)
              head))
  (prove:ok (~> compilation-state
                read-flat-representation
                (aref 0)
                expression-marker-p))
  (prove:ok (~> compilation-state
                read-flat-representation
                (aref 3)
                expression-marker-p))
  (prove:is (cells-count compilation-state)
            8)
  (let ((content (content compilation-state))
        (all-expressions (expressions compilation-state 0 8)))
    (prove:is (length all-expressions) 2)
    (prove:is (length content) 8)
    (iterate
      (for expression in all-expressions)
      (for pointer = (pointer-for-expression compilation-state expression))
      (prove:is (huginn.m.r:tag-of (aref content pointer))
                huginn.m.r:+expression+))))

(let ((compilation-state (make-compilation-state 'compilation-state
                                                 '((a ?b) . (c (d ?b))))))
  (prove:is (body-pointer compilation-state)
            4)
  (prove:ok (~> compilation-state
                read-flat-representation
                (aref 0)
                expression-marker-p))
  (prove:ok (~> compilation-state
                read-flat-representation
                (aref 3)
                expression-marker-p))
  (prove:is (cells-count compilation-state)
            12))

(prove:finalize)
