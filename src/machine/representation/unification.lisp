(cl:in-package #:huginn.machine.representation)


(defmacro with-unification-stack ((execution-state )
                                  &body body)
  (with-gensyms (!ustack !fill-pointer !state !start !block)
    `(let* ((,!state ,execution-state)
            (,!ustack (execution-state-unification-stack ,!state)))
       (declare (ignorable ,!ustack)
                (type (cl-ds.utils:extendable-vector fixnum) ,!ustack))
       (assert (array-has-fill-pointer-p ,!ustack))
       (macrolet ((upush (stack-pointer goal-pointer)
                    `(progn
                       (vector-push-extend ,stack-pointer ,',!ustack)
                       (vector-push-extend ,goal-pointer ,',!ustack)))
                  (upop ((stack-pointer goal-pointer
                          &optional stack-cell goal-cell)
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
                    `(clear-vector ,',!ustack))
                  (uemptyp ()
                    `(zerop (fill-pointer ,',!ustack)))
                  (deref (pointer)
                    `(dereference-heap-pointer ,',!state
                                               ,pointer))
                  (next ()
                    `(go ,',!start))
                  (done (result)
                    `(return-from ,',!block ,result)))
         (block ,!block
           (tagbody
              ,!start
              ,@body))))))


(with-compilation-unit ()
  (-> prepare-unification-stack
      (execution-state execution-stack-cell pointer)
      t)
  (defun prepare-unification-stack (execution-state stack-cell goal-pointer)
    (declare (type execution-stack-cell stack-cell)
             (type execution-state execution-state)
             (type pointer goal-pointer)
             (optimize (speed 3)))
    (with-unification-stack (execution-state)
      (uclear)
      (upush (execution-stack-cell-heap-pointer stack-cell)
             goal-pointer)))


  (declaim (notinline alter-cell))
  (-> alter-cell
      (execution-state execution-stack-cell pointer cell)
      t)
  (defun alter-cell (execution-state
                     execution-stack-cell
                     pointer
                     new-value)
    (declare (optimize (speed 3))
             (type execution-state execution-state)
             (type execution-stack-cell execution-stack-cell)
             (type pointer pointer)
             (type cell new-value))
    (let ((heap-trail (execution-stack-cell-heap-cells-trail
                       execution-stack-cell)))
      (vector-push-extend pointer heap-trail 2)
      (vector-push-extend (shiftf (aref (execution-state-heap execution-state) pointer)
                                  new-value)
                          heap-trail)))


  (declaim (notinline unify-expressions))
  (-> unify-expressions
      (execution-state
       execution-stack-cell
       pointer pointer)
      boolean)
  (defun unify-expressions (execution-state
                            execution-stack-cell
                            first-expression-pointer
                            second-expression-pointer)
    (declare (optimize (speed 3))
             (type pointer first-expression-pointer second-expression-pointer)
             (type execution-stack-cell execution-stack-cell)
             (type execution-state execution-state))
    (declare (ignore execution-stack-cell))
    (with-unification-stack (execution-state)
      (let ((first-arity (deref (1+ first-expression-pointer)))
            (second-arity (deref (1+ second-expression-pointer))))
        (declare (type fixnum first-arity second-arity))
        (unless (eql first-arity second-arity)
          (done nil))
        (iterate
          (declare (type fixnum i))
          (for i from 0 below first-arity)
          (upush (the fixnum (+ first-expression-pointer 2 i))
                 (the fixnum (+ second-expression-pointer 2 i))))
        t)))


  (declaim (notinline unify-references))
  (-> unify-references
      (execution-state
       execution-stack-cell
       pointer pointer
       cell cell)
      boolean)
  (defun unify-references (execution-state
                           execution-stack-cell
                           pointer1 pointer2
                           ref1 ref2)
    (declare (type pointer pointer1 pointer2)
             (type cell ref1 ref2)
             (type execution-state execution-state)
             (type execution-stack-cell execution-stack-cell)
             (optimize (speed 3))
             (ignore pointer1 pointer2))
    (unify-pair execution-state
                execution-stack-cell
                (follow-reference execution-state (detag ref1) t)
                (follow-reference execution-state (detag ref2) t)))


  (declaim (notinline unify-variable-fixnum))
  (-> unify-variable-fixnum
      (execution-state
       execution-stack-cell
       pointer pointer
       cell cell)
      boolean)
  (defun unify-variable-fixnum (execution-state
                                execution-stack-cell
                                variable-pointer
                                fixnum-pointer
                                variable-cell
                                fixnum-cell)
    (declare (ignore fixnum-pointer)
             (type execution-state execution-state)
             (type execution-stack-cell execution-stack-cell)
             (type pointer variable-pointer fixnum-pointer)
             (type cell fixnum-cell variable-cell))
    (unless (eql 0 (detag variable-cell))
      (return-from unify-variable-fixnum nil))
    (alter-cell execution-state
                execution-stack-cell
                variable-pointer
                fixnum-cell)
    t)


  (declaim (notinline unify-variables))
  (-> unify-variables
      (execution-state
       execution-stack-cell
       pointer pointer
       cell cell)
      boolean)
  (defun unify-variables (execution-state
                          execution-stack-cell
                          pointer1
                          pointer2
                          cell1
                          cell2)
    (declare (optimize (speed 3))
             (type pointer pointer1 pointer2)
             (type cell cell1 cell2))
    (unless (< pointer1 pointer2)
      (rotatef pointer1 pointer2)
      (rotatef cell1 cell2))
    (let ((first-unbound (variable-unbound-p cell1))
          (second-unbound (variable-unbound-p cell2)))
      (cond ((nor first-unbound second-unbound)
             nil)
            ((and first-unbound second-unbound)
             (alter-cell execution-state execution-stack-cell
                         pointer1 (make-reference pointer2))
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
      (execution-state execution-stack-cell pointer pointer
                       cell cell)
      boolean)
  (defun unify-variable-reference (execution-state
                                   execution-stack-cell
                                   variable-pointer
                                   reference-pointer
                                   variable-cell
                                   reference-cell)
    (declare (type pointer variable-pointer reference-pointer)
             (ignore variable-cell reference-pointer)
             (optimize (speed 3)))
    (unify-pair execution-state
                execution-stack-cell
                variable-pointer
                (follow-reference execution-state
                                  (detag reference-cell)
                                  t)))


  (declaim (notinline unify-variable-expression))
  (-> unify-variable-expression
      (execution-state
       execution-stack-cell
       pointer pointer
       cell cell)
      boolean)
  (defun unify-variable-expression (execution-state
                                    execution-stack-cell
                                    variable-pointer
                                    expression-pointer
                                    variable-cell
                                    expression-cell)
    (declare (ignore expression-cell)
             (optimize (speed 3)))
    (unless (eql 0 (detag variable-cell))
      (return-from unify-variable-expression nil))
    (alter-cell execution-state
                execution-stack-cell
                variable-pointer
                (make-reference expression-pointer))
    t)


  (-> unify-pair
      (execution-state
       execution-stack-cell
       pointer pointer)
      boolean)
  (defun unify-pair (execution-state execution-stack-cell pointer1 pointer2)
    (declare (optimize (speed 3))
             (type pointer pointer1 pointer2)
             (type execution-stack-cell execution-stack-cell)
             (type execution-state execution-state))
    (when (eql pointer1 pointer2)
      (return-from unify-pair t))
    (let ((cell1 (dereference-heap-pointer execution-state pointer1))
          (cell2 (dereference-heap-pointer execution-state pointer2)))
      (when (eql cell1 cell2)
        (return-from unify-pair t))
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
                     (follow-reference execution-state pointer2 t)))
        (+ref-var+
         (unify-pair execution-state
                     execution-stack-cell
                     (follow-reference execution-state pointer1 t)
                     pointer2))
        (+var-exp+
         (unify-variable-expression execution-state execution-stack-cell
                                    pointer1 pointer2 cell1 cell2))
        (+exp-var+
         (unify-variable-expression execution-state execution-stack-cell
                                    pointer2 pointer1 cell2 cell1))
        (+exp-exp+
         (unify-expressions execution-state execution-stack-cell
                            pointer1 pointer2))
        (+fixnum-fixnum+
         nil)
        (+ref-ref+
         (unify-references execution-state
                           execution-stack-cell
                           pointer1 pointer2
                           cell1 cell2)))))


  (-> unify (execution-state execution-stack-cell) boolean)
  (defun unify (execution-state execution-stack-cell)
    (declare (type execution-stack-cell execution-stack-cell)
             (type execution-state execution-state)
             (optimize (speed 3)))
    (with-unification-stack (execution-state)
      (when (uemptyp)
        (done t))
      (upop
       (stack-pointer goal-pointer)
       (let ((result (unify-pair execution-state execution-stack-cell
                                 stack-pointer goal-pointer)))
         (unless result
           (done nil)))
       (next)))))
