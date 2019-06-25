(cl:in-package #:huginn.machine.operations)


(with-compilation-unit ()
  (defun combine-tags (first-cell second-cell)
    (declare (type huginn.m.r:cell first-cell second-cell)
             (optimize (speed 3)))
    (logior (ash (the fixnum (1- (huginn.m.r:tag-of first-cell)))
                 (1+ huginn.m.r:+tag-size+))
            (the fixnum (1- (huginn.m.r:tag-of second-cell)))))


  (flet ((combine-tags (first-tag second-tag)
           (combine-tags (huginn.m.r:tag first-tag 0)
                         (huginn.m.r:tag second-tag 0))))
    (define-constant +var-var+ (combine-tags huginn.m.r:+variable+
                                             huginn.m.r:+variable+))
    (define-constant +var-ref+ (combine-tags huginn.m.r:+variable+
                                             huginn.m.r:+reference+))
    (define-constant +var-exp+ (combine-tags huginn.m.r:+variable+
                                             huginn.m.r:+expression+))
    (define-constant +var-fixnum+ (combine-tags huginn.m.r:+variable+
                                                huginn.m.r:+fixnum+))
    (define-constant +ref-var+ (combine-tags huginn.m.r:+reference+
                                             huginn.m.r:+variable+))
    (define-constant +ref-ref+ (combine-tags huginn.m.r:+reference+
                                             huginn.m.r:+reference+))
    (define-constant +ref-exp+ (combine-tags huginn.m.r:+reference+
                                             huginn.m.r:+expression+))
    (define-constant +ref-fixnum+ (combine-tags huginn.m.r:+reference+
                                                huginn.m.r:+fixnum+))
    (define-constant +exp-var+ (combine-tags huginn.m.r:+expression+
                                             huginn.m.r:+variable+))
    (define-constant +exp-ref+ (combine-tags huginn.m.r:+expression+
                                             huginn.m.r:+reference+))
    (define-constant +exp-exp+ (combine-tags huginn.m.r:+expression+
                                             huginn.m.r:+expression+))
    (define-constant +exp-fixnum+ (combine-tags huginn.m.r:+expression+
                                                huginn.m.r:+fixnum+))
    (define-constant +fixnum-var+ (combine-tags huginn.m.r:+fixnum+
                                                huginn.m.r:+variable+))
    (define-constant +fixnum-ref+ (combine-tags huginn.m.r:+fixnum+
                                                huginn.m.r:+reference+))
    (define-constant +fixnum-exp+ (combine-tags huginn.m.r:+fixnum+
                                                huginn.m.r:+expression+))
    (define-constant +fixnum-fixnum+ (combine-tags huginn.m.r:+fixnum+
                                                   huginn.m.r:+fixnum+)))


  (defmacro with-unification-stack ((execution-state)
                                    &body body)
    (with-gensyms (!ustack !fill-pointer !state !start !block)
      `(let* ((,!state ,execution-state)
              (,!ustack (huginn.m.r:execution-state-unification-stack ,!state)))
         (declare (ignorable ,!ustack)
                  (type (cl-ds.utils:extendable-vector fixnum) ,!ustack))
         (assert (array-has-fill-pointer-p ,!ustack))
         (macrolet ((upush (stack-pointer goal-pointer)
                      `(progn
                         (vector-push-extend ,stack-pointer ,',!ustack)
                         (vector-push-extend ,goal-pointer ,',!ustack)))
                    (upop ((stack-pointer goal-pointer)
                           &body this-body)
                      `(let* ((,',!fill-pointer (fill-pointer ,',!ustack))
                              (,goal-pointer
                                (aref ,',!ustack
                                      (the fixnum (1- ,',!fill-pointer))))
                              (,stack-pointer
                                (aref ,',!ustack
                                      (- ,',!fill-pointer 2))))
                         (decf (fill-pointer ,',!ustack) 2)
                         ,@this-body))
                    (uclear ()
                      `(setf (fill-pointer ,',!ustack) 0))
                    (uemptyp ()
                      `(zerop (fill-pointer ,',!ustack)))
                    (deref (pointer)
                      `(huginn.m.r:dereference-heap-pointer ,',!state
                                                            ,pointer))
                    (next ()
                      `(go ,',!start))
                    (done (result)
                      `(return-from ,',!block ,result)))
           (block ,!block
             (tagbody
                ,!start
                ,@body))))))


  (-> prepare-unification-stack
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer)
      t)
  (defun prepare-unification-stack (execution-state stack-cell goal-pointer)
    (declare (type huginn.m.r:execution-stack-cell stack-cell)
             (type huginn.m.r:execution-state execution-state)
             (type huginn.m.r:pointer goal-pointer)
             (optimize (speed 3)))
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
    (declare (optimize (speed 3))
             (type huginn.m.r:execution-state execution-state)
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
    (declare (optimize (speed 3))
             (type huginn.m.r:cell first-cell second-cell)
             (type huginn.m.r:pointer first-expression-pointer
                   second-expression-pointer)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state))
    (declare (ignore execution-stack-cell))
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
              (upush (the huginn.m.r:pointer
                          (+ first-expression-pointer 2 i))
                     (the huginn.m.r:pointer
                          (+ second-expression-pointer 2 i))))
            t))))


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
             (optimize (speed 3))
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
          t)))


  (declaim (notinline unify-variable-fixnum))
  (-> unify-variable-fixnum
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      boolean)
  (defun unify-variable-fixnum (execution-state
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
      (return-from unify-variable-fixnum nil))
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
    (declare (optimize (speed 3))
             (type huginn.m.r:pointer pointer1 pointer2)
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


  (declaim (notinline unify-variable-reference))
  (-> unify-variable-reference
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      boolean)
  (defun unify-variable-reference (execution-state
                                   execution-stack-cell
                                   variable-pointer
                                   reference-pointer
                                   variable-cell
                                   reference-cell)
    (declare (type huginn.m.r:pointer variable-pointer reference-pointer)
             (ignore reference-pointer)
             (optimize (speed 3)))
    (unify-pair execution-state
                execution-stack-cell
                variable-pointer
                (huginn.m.r:follow-pointer execution-state
                                           (huginn.m.r:detag reference-cell)
                                           t)
                variable-cell))


  (declaim (notinline unify-variable-expression))
  (-> unify-variable-expression
      (huginn.m.r:execution-state
       huginn.m.r:execution-stack-cell
       huginn.m.r:pointer huginn.m.r:pointer
       huginn.m.r:cell huginn.m.r:cell)
      boolean)
  (defun unify-variable-expression (execution-state
                                    execution-stack-cell
                                    variable-pointer
                                    expression-pointer
                                    variable-cell
                                    expression-cell)
    (declare (ignore expression-cell)
             (optimize (speed 3)))
    (unless (huginn.m.r:variable-unbound-p variable-cell) ; can't change bound variable
      (return-from unify-variable-expression nil))
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
    (declare (optimize (speed 3))
             (type huginn.m.r:pointer pointer1 pointer2)
             (type huginn.m.r:execution-stack-cell execution-stack-cell)
             (type huginn.m.r:execution-state execution-state))
    (when (eql pointer1 pointer2)
      (return-from unify-pair t))
    (let ((cell1 (or cell1
                     (huginn.m.r:dereference-heap-pointer execution-state
                                                          pointer1)))
          (cell2 (or cell2
                     (huginn.m.r:dereference-heap-pointer execution-state
                                                          pointer2))))
      (declare (type huginn.m.r:cell cell1 cell2))
      (switch ((combine-tags cell1 cell2) :test 'eql)
        (+var-var+
         (unify-variables execution-state
                          execution-stack-cell
                          pointer1 pointer2
                          cell1 cell2))
        (+var-fixnum+
         (unify-variable-fixnum execution-state
                                execution-stack-cell
                                pointer1 pointer2
                                cell1 cell2))
        (+fixnum-var+
         (unify-variable-fixnum execution-state
                                execution-stack-cell
                                pointer2 pointer1
                                cell2 cell1))
        (+var-ref+
         (unify-pair execution-state
                     execution-stack-cell
                     pointer1
                     (huginn.m.r:follow-pointer execution-state pointer2 t)
                     cell1))
        (+ref-fixnum+
         (unify-pair execution-state
                     execution-stack-cell
                     (huginn.m.r:follow-pointer execution-state pointer1 t)
                     pointer2
                     nil
                     cell2))
        (+fixnum-ref+
         (unify-pair execution-state
                     execution-stack-cell
                     pointer1
                     (huginn.m.r:follow-pointer execution-state pointer2 t)
                     cell1))
        (+ref-var+
         (unify-pair execution-state
                     execution-stack-cell
                     (huginn.m.r:follow-pointer execution-state pointer1 t)
                     pointer2
                     nil
                     cell2))
        (+var-exp+
         (unify-variable-expression execution-state execution-stack-cell
                                    pointer1 pointer2 cell1 cell2))
        (+exp-var+
         (unify-variable-expression execution-state execution-stack-cell
                                    pointer2 pointer1 cell2 cell1))
        (+exp-exp+
         (unify-expressions execution-state execution-stack-cell
                            pointer1 pointer2
                            cell1 cell2))
        (+fixnum-fixnum+
         (huginn.m.r:same-cells-p cell1 cell2))
        (+ref-ref+
         (unify-references execution-state
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
             (type huginn.m.r:pointer goal-pointer)
             (optimize (speed 3)))
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