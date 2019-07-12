(cl:in-package #:huginn.machine.operations)


(defmacro define-tag-combinations (&body input)
  `(flet ((combine-tags (first-tag second-tag)
            (combine-tags (huginn.m.r:tag first-tag 0)
                          (huginn.m.r:tag second-tag 0))))
     ,@(iterate outer
         (for a on input)
         (iterate
           (for b on input)
           (for combination = (list (first a) (first b)))
           (in outer
               (collect
                   `(define-constant
                        ,(intern (format nil "+~a/~a+"
                                         (~>> combination
                                              first
                                              symbol-name
                                              (remove #\+ ))
                                         (~>> combination
                                              second
                                              symbol-name
                                              (remove #\+ ))))
                        (combine-tags ,(first combination)
                                      ,(second combination)))))))))


(-> combine-tags (huginn.m.r:cell huginn.m.r:cell) fixnum)
(defun combine-tags (first-cell second-cell)
  (declare (type huginn.m.r:cell first-cell second-cell))
  (logior (ash (the fixnum (1- (huginn.m.r:tag-of first-cell)))
               (1+ huginn.m.r:+tag-size+))
          (the fixnum (1- (huginn.m.r:tag-of second-cell)))))


(define-tag-combinations
  huginn.m.r:+expression+
  huginn.m.r:+fixnum+
  huginn.m.r:+list-end+
  huginn.m.r:+list-rest+
  huginn.m.r:+list-start+
  huginn.m.r:+predicate+
  huginn.m.r:+reference+
  huginn.m.r:+variable+)


(with-compilation-unit (:override nil)
  (declare (optimize (speed 0) (debug 3) (safety 3) (space 2)))

  (-> prepare-unification-stack
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer)
      t)
  (defun prepare-unification-stack (execution-state stack-cell goal-pointer)
    (declare (type huginn.m.r:execution-stack-cell stack-cell)
             (type huginn.m.r:execution-state execution-state)
             (type huginn.m.r:pointer goal-pointer))
    (with-unification-stack (execution-state)
      (uclear)
      (upush (huginn.m.r:execution-stack-cell-heap-pointer stack-cell)
             goal-pointer)))


  (declaim (notinline alter-cell))
  (-> alter-cell
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:cell)
      t)
  (defun alter-cell (execution-state
                     execution-stack-cell
                     pointer
                     new-value)
    (declare (type huginn.m.r:execution-state execution-state)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:pointer pointer)
             (type huginn.m.r:cell new-value))
    (when (= pointer 5)
      (break))
    (let ((heap-trail (huginn.m.r:execution-stack-cell-heap-cells-trail
                       execution-stack-cell)))
      (vector-push-extend pointer heap-trail 2)
      (vector-push-extend
       (shiftf (aref (huginn.m.r:execution-state-heap execution-state)
                     pointer)
               new-value)
       heap-trail)))


  (declaim (notinline unify-variable/list-start))
  (-> unify-variable/list-start
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      boolean)
  (defun unify-variable/list-start (execution-state
                                    execution-stack-cell
                                    variable-pointer
                                    list-start-pointer
                                    variable-cell
                                    list-start-cell)
    (declare (type huginn.m.r:cell variable-cell list-start-cell)
             (type huginn.m.r:pointer
                   list-start-pointer
                   variable-pointer)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state)
             (ignore list-start-pointer))
    (unless (huginn.m.r:variable-unbound-p variable-cell)
      (return-from unify-variable/list-start nil))
    (alter-cell execution-state execution-stack-cell
                variable-pointer list-start-cell)
    t)


  (declaim (notinline unify-variable/list-start))
  (-> unify-list-rest/variable
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      boolean)
  (defun unify-list-rest/variable (execution-state
                                   execution-stack-cell
                                   list-rest-pointer
                                   variable-pointer
                                   list-rest-cell
                                   variable-cell)
    (declare (type huginn.m.r:cell variable-cell list-rest-cell)
             (type huginn.m.r:pointer
                   list-rest-pointer
                   variable-pointer)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state))
    (unless (huginn.m.r:variable-unbound-p variable-cell)
      (return-from unify-list-rest/variable nil))
    (alter-cell execution-state execution-stack-cell variable-pointer
                (if (huginn.m.r:list-rest-unbound-p list-rest-cell)
                    (huginn.m.r:make-reference list-rest-pointer)
                    (huginn.m.r:tag huginn.m.r:+list-start+ list-rest-cell)))
    t)


  (declaim (notinline unify-list-rests))
  (-> unify-list-rests
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer
       huginn.m.r:pointer
       huginn.m.r:cell
       huginn.m.r:cell)
      boolean)
  (defun unify-list-rests (execution-state execution-stack-cell
                           pointer1 pointer2
                           cell1 cell2)
    (declare (type huginn.m.r:cell cell1 cell2)
             (type huginn.m.r:pointer pointer1 pointer2)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state))
    (when (= pointer1 pointer2)
      (return-from unify-list-rests t))
    (let ((first-unbound (huginn.m.r:list-rest-unbound-p cell1))
          (second-unbound (huginn.m.r:list-rest-unbound-p cell2)))
      (cond ((nor first-unbound second-unbound)
             (if (huginn.m.r:same-cells-p cell1 cell2)
                 t
                 (unify-lists execution-state execution-stack-cell
                              (huginn.m.r:detag cell1)
                              (huginn.m.r:detag cell2))))
            ((and first-unbound second-unbound)
             (alter-cell execution-state execution-stack-cell
                         pointer1
                         (huginn.m.r:make-reference pointer2))
             t)
            (first-unbound
             (alter-cell execution-state execution-stack-cell
                         pointer1 cell2)
             t)
            (second-unbound
             (alter-cell execution-state execution-stack-cell
                         pointer2 cell1)
             t))))


  (declaim (notinline unify-lists))
  (-> unify-lists
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer
       huginn.m.r:pointer)
      boolean)
  (defun unify-lists (execution-state execution-stack-cell
                      first-pointer second-pointer)
    (declare (type huginn.m.r:pointer first-pointer second-pointer)
             (type huginn.m.r:execution-state execution-state)
             (type huginn.m.r:execution-stack-cell execution-stack-cell))
    (unless (< first-pointer second-pointer)
      (rotatef first-pointer second-pointer))
    (iterate
      (declare (type huginn.m.r:pointer p1 p2)
               (type huginn.m.r:cell cell1 cell2))
      (with p1 = first-pointer)
      (with p2 = second-pointer)
      (when (= p1 p2)
        (leave t))
      (for cell1 = (huginn.m.r:dereference-heap-pointer execution-state
                                                        p1 t))
      (for cell2 = (huginn.m.r:dereference-heap-pointer execution-state
                                                        p2 t))
      (until (and (huginn.m.r:list-end-cell-p cell1)
                  (huginn.m.r:list-end-cell-p cell2)))
      (for cell1-rest-p = (huginn.m.r:list-rest-cell-p cell1))
      (for cell2-rest-p = (huginn.m.r:list-rest-cell-p cell2))
      (when (and cell1-rest-p cell2-rest-p)
        (let ((unbound1 (huginn.m.r:list-rest-unbound-p cell1))
              (unbound2 (huginn.m.r:list-rest-unbound-p cell2)))
          (cond ((nor unbound1 unbound2)
                 (setf p1 (huginn.m.r:detag cell1)
                       p2 (huginn.m.r:detag cell2))
                 (next-iteration))
                (unbound2
                 (alter-cell execution-state execution-stack-cell
                             p2 (huginn.m.r:make-reference p1))
                 (leave t))
                (unbound1
                 (alter-cell execution-state execution-stack-cell
                             p1 (huginn.m.r:make-reference p2))
                 (leave t)))))
      (when cell2-rest-p
        (when (huginn.m.r:list-rest-unbound-p cell2)
          (alter-cell execution-state execution-stack-cell
                      p2 (huginn.m.r:tag huginn.m.r:+list-rest+ p1))
          (leave t))
        (setf p2 (huginn.m.r:follow-pointer execution-state
                                            (huginn.m.r:detag cell2)
                                            t))
        (next-iteration))
      (when cell1-rest-p
        (when (huginn.m.r:list-rest-unbound-p cell1)
          (alter-cell execution-state execution-stack-cell
                      p1 (huginn.m.r:tag huginn.m.r:+list-rest+ p2))
          (leave t))
        (setf p1 (huginn.m.r:follow-pointer execution-state
                                            (huginn.m.r:detag cell1)
                                            t))
        (next-iteration))
      (for unified-p = (unify-pair execution-state execution-stack-cell
                                   p1 p2))
      (break)
      (always unified-p)
      (incf p1)
      (incf p2)))


  (declaim (notinline unify-list-rest/list-start))
  (-> unify-list-rest/list-start
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      boolean)
  (defun unify-list-rest/list-start (execution-state
                                     execution-stack-cell
                                     list-rest-pointer
                                     list-start-pointer
                                     list-rest-cell
                                     list-start-cell)
    (declare (type huginn.m.r:cell
                   list-start-cell list-rest-cell)
             (type huginn.m.r:pointer
                   list-rest-pointer list-start-pointer)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state)
             (ignore list-start-pointer))
    (when (huginn.m.r:list-rest-unbound-p list-rest-cell)
      (alter-cell execution-state execution-stack-cell list-rest-pointer
                  (huginn.m.r:tag huginn.m.r:+list-rest+ list-start-cell))
      (return-from unify-list-rest/list-start t))
    (unify-lists execution-state execution-stack-cell
                 (huginn.m.r:detag list-start-cell)
                 (huginn.m.r:detag list-rest-cell)))


  (declaim (notinline unify-expressions))
  (-> unify-expressions
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      boolean)
  (defun unify-expressions (execution-state
                            execution-stack-cell
                            first-expression-pointer
                            second-expression-pointer
                            first-cell
                            second-cell)
    (declare (type huginn.m.r:cell first-cell second-cell)
             (type huginn.m.r:pointer
                   first-expression-pointer
                   second-expression-pointer)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state))
    (assert (huginn.m.r:expression-cell-p first-cell))
    (assert (huginn.m.r:expression-cell-p second-cell))
    (or (huginn.m.r:same-cells-p first-cell second-cell) ; first cell contains unique ID for the expression. If those matches it is the same expression so unification will succeed.
        (with-unification-stack (execution-state)
          (let ((first-arity (deref (1+ first-expression-pointer)))
                (second-arity (deref (1+ second-expression-pointer))))
            (declare (type fixnum first-arity second-arity))
            (unless (huginn.m.r:same-cells-p first-arity second-arity)
              (done nil))
            (iterate
              (declare (type fixnum i j))
              (for j from 0 below first-arity)
              (for i from 2)
              (unless
                  (unify-pair execution-state execution-stack-cell
                              (the huginn.m.r:pointer
                                   (+ first-expression-pointer i))
                              (the huginn.m.r:pointer
                                   (+ second-expression-pointer i)))
                (done nil))))
          (done t))))


  (declaim (notinline unify-references))
  (-> unify-references
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      boolean)
  (defun unify-references (execution-state
                           execution-stack-cell
                           pointer1 pointer2
                           ref1 ref2)
    (declare (type huginn.m.r:pointer pointer1 pointer2)
             (type huginn.m.r:cell ref1 ref2)
             (type huginn.m.r:execution-state execution-state)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (ignore pointer1 pointer2 execution-stack-cell))
    (if (huginn.m.r:same-cells-p ref1 ref2) ; same reference, unification succesfull
        t
        (with-unification-stack (execution-state)
          (upush (huginn.m.r:follow-pointer execution-state
                                            (huginn.m.r:detag ref1)
                                            t)
                 (huginn.m.r:follow-pointer execution-state
                                            (huginn.m.r:detag ref2)
                                            t))
          (done t))))


  (declaim (notinline unify-variable/fixnum))
  (-> unify-variable/fixnum
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      boolean)
  (defun unify-variable/fixnum (execution-state
                                execution-stack-cell
                                variable-pointer
                                fixnum-pointer
                                variable-cell
                                fixnum-cell)
    (declare (ignore fixnum-pointer)
             (type huginn.m.r:execution-state execution-state)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:pointer variable-pointer fixnum-pointer)
             (type huginn.m.r:cell fixnum-cell variable-cell))
    (unless (huginn.m.r:variable-unbound-p variable-cell)
      (return-from unify-variable/fixnum nil))
    (alter-cell execution-state
                execution-stack-cell
                variable-pointer
                fixnum-cell)
    t)


  (declaim (notinline unify-variables))
  (-> unify-variables
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      boolean)
  (defun unify-variables (execution-state
                          execution-stack-cell
                          pointer1
                          pointer2
                          cell1
                          cell2)
    (declare (type huginn.m.r:pointer pointer1 pointer2)
             (type huginn.m.r:cell cell1 cell2))
    (assert (huginn.m.r:variable-cell-p cell1))
    (assert (huginn.m.r:variable-cell-p cell2))
    (break)
    (let ((first-unbound (huginn.m.r:variable-unbound-p cell1))
          (second-unbound (huginn.m.r:variable-unbound-p cell2)))
      (cond ((nor first-unbound second-unbound)
             (huginn.m.r:same-cells-p cell1 cell2))
            ((and first-unbound second-unbound)
             (alter-cell execution-state execution-stack-cell
                         pointer2 (huginn.m.r:make-reference pointer1))
             t)
            (first-unbound
             (alter-cell execution-state execution-stack-cell
                         pointer1 cell2)
             t)
            (second-unbound
             (alter-cell execution-state execution-stack-cell
                         pointer2 cell1)
             t))))


  (declaim (notinline unify-variable/reference))
  (-> unify-variable/reference
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      boolean)
  (defun unify-variable/reference (execution-state
                                   execution-stack-cell
                                   variable-pointer
                                   reference-pointer
                                   variable-cell
                                   reference-cell)
    (declare (type huginn.m.r:pointer variable-pointer reference-pointer)
             (ignore reference-pointer))
    (when-let ((new-pointer (huginn.m.r:follow-pointer
                             execution-state
                             (huginn.m.r:detag reference-cell)
                             t)))
      (unify-pair execution-state
                  execution-stack-cell
                  variable-pointer
                  new-pointer
                  variable-cell)))


  (declaim (notinline unify-predicates))
  (-> unify-predicates
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      boolean)
  (defun unify-predicates (execution-state
                           execution-stack-cell
                           pointer1
                           pointer2
                           cell1
                           cell2)
    (let ((first-unbound (huginn.m.r:predicate-unbound-p cell1))
          (second-unbound (huginn.m.r:predicate-unbound-p cell2)))
      (cond ((nor first-unbound second-unbound)
             (huginn.m.r:same-cells-p cell1 cell2))
            ((and first-unbound second-unbound)
             (alter-cell execution-state execution-stack-cell
                         pointer1 (huginn.m.r:make-reference pointer2))
             t)
            (first-unbound
             (alter-cell execution-state execution-stack-cell
                         pointer1 cell2)
             t)
            (second-unbound
             (alter-cell execution-state execution-stack-cell
                         pointer2 cell1)
             t))))


  (declaim (notinline unify-variable/expression))
  (-> unify-variable/expression
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      boolean)
  (defun unify-variable/expression (execution-state
                                    execution-stack-cell
                                    variable-pointer
                                    expression-pointer
                                    variable-cell
                                    expression-cell)
    (declare (ignore expression-cell))
    (unless (huginn.m.r:variable-unbound-p variable-cell) ; can't change bound variable
      (return-from unify-variable/expression nil))
    (alter-cell execution-state
                execution-stack-cell
                variable-pointer
                (huginn.m.r:make-reference expression-pointer))
    t)


  (-> unify-pair
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       &optional
       (or null huginn.m.r:cell)
       (or null huginn.m.r:cell))
      boolean)
  (declaim (notinline unify-pair))
  (defun unify-pair (execution-state execution-stack-cell pointer1 pointer2
                     &optional cell1 cell2)
    (declare (type huginn.m.r:pointer pointer1 pointer2)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state))
    (break)
    (when (eql pointer1 pointer2)
      (return-from unify-pair t))
    (bind ((cell1 (or cell1
                      (huginn.m.r:dereference-heap-pointer execution-state
                                                           pointer1)))
           (cell2 (or cell2
                      (huginn.m.r:dereference-heap-pointer execution-state
                                                           pointer2)))
           ((:dflet follow-pointer (pointer))
            (declare (type huginn.m.r:pointer pointer))
            (lret ((result (huginn.m.r:follow-pointer execution-state
                                                      pointer
                                                      t)))
              (when (null result)
                (return-from unify-pair nil)))))
      (declare (type huginn.m.r:cell cell1 cell2))
      (switch ((combine-tags cell1 cell2) :test 'eql)
        (+predicate/predicate+
         (unify-predicates execution-state
                           execution-stack-cell
                           pointer1 pointer2
                           cell1 cell2))
        (+predicate/reference+
         (unify-pair execution-state
                     execution-stack-cell
                     pointer1
                     (follow-pointer (huginn.m.r:detag cell2))
                     cell1))
        (+reference/predicate+
         (unify-pair execution-state
                     execution-stack-cell
                     pointer2
                     (follow-pointer (huginn.m.r:detag cell1))
                     cell2))
        (+variable/variable+
         (unify-variables execution-state
                          execution-stack-cell
                          pointer1 pointer2
                          cell1 cell2))
        (+variable/fixnum+
         (unify-variable/fixnum execution-state
                                execution-stack-cell
                                pointer1 pointer2
                                cell1 cell2))
        (+fixnum/variable+
         (unify-variable/fixnum execution-state
                                execution-stack-cell
                                pointer2 pointer1
                                cell2 cell1))
        (+variable/reference+
         (unify-pair execution-state
                     execution-stack-cell
                     pointer1
                     (follow-pointer (huginn.m.r:detag cell2))
                     cell1))
        (+reference/fixnum+
         (unify-pair execution-state
                     execution-stack-cell
                     (follow-pointer (huginn.m.r:detag cell1))
                     pointer2
                     nil
                     cell2))
        (+fixnum/reference+
         (unify-pair execution-state
                     execution-stack-cell
                     pointer1
                     (follow-pointer (huginn.m.r:detag cell2))
                     cell1))
        (+reference/variable+
         (unify-pair execution-state
                     execution-stack-cell
                     (follow-pointer (huginn.m.r:detag cell1))
                     pointer2))
        (+variable/expression+
         (unify-variable/expression execution-state execution-stack-cell
                                    pointer1 pointer2 cell1 cell2))
        (+expression/variable+
         (unify-variable/expression execution-state execution-stack-cell
                                    pointer2 pointer1 cell2 cell1))
        (+expression/expression+
         (unify-expressions execution-state
                            execution-stack-cell
                            pointer1 pointer2
                            cell1 cell2))
        (+reference/reference+
         (unify-references execution-state
                           execution-stack-cell
                           pointer1 pointer2
                           cell1 cell2))
        (+fixnum/fixnum+
         (huginn.m.r:same-cells-p cell1 cell2))
        (+list-end/list-end+
         t)
        (+variable/list-start+
         (unify-variable/list-start execution-state
                                    execution-stack-cell
                                    pointer1 pointer2
                                    cell1 cell2))
        (+list-start/variable+
         (unify-variable/list-start execution-state
                                    execution-stack-cell
                                    pointer2 pointer1
                                    cell2 cell1))
        (+reference/list-start+
         (unify-pair execution-state
                     execution-stack-cell
                     pointer2
                     (follow-pointer (huginn.m.r:detag cell1))
                     cell2))
        (+list-start/reference+
         (unify-pair execution-state
                     execution-stack-cell
                     pointer1
                     (follow-pointer (huginn.m.r:detag cell2))
                     cell1))
        (+list-start/list-start+
         (unify-lists execution-state execution-stack-cell
                      (huginn.m.r:detag cell1)
                      (huginn.m.r:detag cell2)))
        (+list-rest/variable+
         (unify-list-rest/variable execution-state
                                   execution-stack-cell
                                   pointer1 pointer2
                                   cell1 cell2))
        (+variable/list-rest+
         (unify-list-rest/variable execution-state
                                   execution-stack-cell
                                   pointer2 pointer1
                                   cell2 cell1))
        (+list-rest/reference+
         (unify-pair execution-state
                     execution-stack-cell
                     (follow-pointer pointer1)
                     pointer2
                     cell1))
        (+reference/list-rest+
         (unify-pair execution-state
                     execution-stack-cell
                     pointer2
                     (follow-pointer pointer1)
                     cell2))
        (+list-rest/list-rest+
         (unify-list-rests execution-state
                           execution-stack-cell
                           pointer1 pointer2
                           cell1 cell2)))))


  (-> unify (huginn.m.r:execution-state
             huginn.m.r:execution-stack-cell
             huginn.m.r:pointer)
      boolean)
  (defun unify (execution-state execution-stack-cell goal-pointer)
    (declare (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state)
             (type huginn.m.r:pointer goal-pointer))
    (prepare-unification-stack execution-state
                               execution-stack-cell
                               goal-pointer)
    (with-unification-stack (execution-state)
      (when (uemptyp)
        (done t))
      (upop
       (first-pointer second-pointer)
       (let ((result (unify-pair execution-state execution-stack-cell
                                 first-pointer second-pointer)))
         (unless result
           (done nil)))
       (next))))
  )
