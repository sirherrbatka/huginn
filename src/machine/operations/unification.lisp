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
  (declare (optimize (speed 3) (debug 0) (safety 0) (space 0)))

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


  (declaim (inline alter-cell))
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
    (let ((heap-trail (huginn.m.r:execution-stack-cell-heap-cells-trail
                       execution-stack-cell)))
      (vector-push-extend pointer heap-trail 2)
      (vector-push-extend
       (shiftf (aref (huginn.m.r:execution-state-heap execution-state)
                     pointer)
               new-value)
       heap-trail)))


  (declaim (inline unify-variable/list-start))
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
                   variable-cell)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state)
             (ignore list-start-pointer))
    (unless (huginn.m.r:variable-unbound-p variable-cell)
      (return-from unify-variable/list-start nil))
    (alter-cell execution-state execution-stack-cell
                variable-pointer list-start-cell)
    t)


  (declaim (inline unify-variable/list-start))
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


  (declaim (inline unify-variable/list-start))
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
             (type huginn.m.r:execution-state execution-state)
             (ignore list-rest-pointer))
    (unless (huginn.m.r:variable-unbound-p variable-cell)
      (return-from unify-list-rest/variable nil))
    (alter-cell execution-state execution-stack-cell variable-pointer
                (huginn.m.r:tag huginn.m.r:+list-start+ list-rest-cell))
    t)


  (declaim (inline unify-lists))
  (-> unify-lists
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer
       huginn.m.r:pointer)
      boolean)
  (defun unify-lists (execution-state execution-stack-cell
                      first-pointer second-pointer)
    cl-ds.utils:todo)


  (declaim (inline unify-list-rest/list-start))
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


  (declaim (inline unify-expressions))
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
    (declare (ignore execution-stack-cell))
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
              (declare (type fixnum i))
              (for i from 0 below first-arity)
              (upush
               (the huginn.m.r:pointer
                    (+ first-expression-pointer 2 i))
               (the huginn.m.r:pointer
                    (+ second-expression-pointer 2 i)))))
          (done t))))


  (declaim (inline unify-references))
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


  (declaim (inline unify-variable/fixnum))
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


  (declaim (inline unify-variables))
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
    (let ((first-unbound (huginn.m.r:variable-unbound-p cell1))
          (second-unbound (huginn.m.r:variable-unbound-p cell2)))
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


  (declaim (inline unify-variable/reference))
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


  (declaim (inline unify-predicates))
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


  (declaim (inline unify-variable/expression))
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
  (defun unify-pair (execution-state execution-stack-cell pointer1 pointer2
                     &optional cell1 cell2)
    (declare (type huginn.m.r:pointer pointer1 pointer2)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state))
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
        (+var/var+
         (unify-variables execution-state
                          execution-stack-cell
                          pointer1 pointer2
                          cell1 cell2))
        (+var/fixnum+
         (unify-variable/fixnum execution-state
                                execution-stack-cell
                                pointer1 pointer2
                                cell1 cell2))
        (+fixnum/var+
         (unify-variable/fixnum execution-state
                                execution-stack-cell
                                pointer2 pointer1
                                cell2 cell1))
        (+var/ref+
         (unify-pair execution-state
                     execution-stack-cell
                     pointer1
                     (follow-pointer (huginn.m.r:detag cell2))
                     cell1))
        (+ref/fixnum+
         (unify-pair execution-state
                     execution-stack-cell
                     (follow-pointer (huginn.m.r:detag cell1))
                     pointer2
                     nil
                     cell2))
        (+fixnum/ref+
         (unify-pair execution-state
                     execution-stack-cell
                     pointer1
                     (follow-pointer (huginn.m.r:detag cell2))
                     cell1))
        (+ref/var+
         (unify-pair execution-state
                     execution-stack-cell
                     (follow-pointer (huginn.m.r:detag cell1))
                     pointer2
                     nil
                     cell2))
        (+var/exp+
         (unify-variable/expression execution-state execution-stack-cell
                                    pointer1 pointer2 cell1 cell2))
        (+exp/var+
         (unify-variable/expression execution-state execution-stack-cell
                                    pointer2 pointer1 cell2 cell1))
        (+exp/exp+
         (unify-expressions execution-state
                            execution-stack-cell
                            pointer1 pointer2
                            cell1 cell2))
        (+ref/ref+
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
         cl-ds.utils:todo)
        (+reference/list-rest+
         cl-ds.utils:todo)
        (+list-rest/list-rest+
         cl-ds.utils:todo)
        )))


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
       (next)))))
