;;;; compile.scm
;;;;
;;;; Code used to compile scheme forms into instruction sets.

(define (default-arithmetic-val op)
  (case op
    [(add) 0]
    [(sub) 0]
    [else 1]))

(define (compile-arg arg)
  (if (self-evaluating? arg)
      `((push ,arg))
      (compile-form arg)))

;; Compiles the pushes for arithemtic args.  This will, at most, push two
;; arguments onto the stack.  If arguments are lacking, depending on the
;; op, this will push an null-operator (0 for add, 1 for mul).
(define (compile-arithmetic op args)
  (case (length args)
    [(0) `((push ,(default-arithmetic-val op))
           (push ,(default-arithmetic-val op))
           (,op))]
    [(1)  `(,@(compile-arg (car args))
            (push ,(default-arithmetic-val op))
            (,op))]
    [(2) `(,@(compile-arg (car args))
           ,@(compile-arg (cadr args))
           (,op))]
    [else (append (compile-arithmetic op (take args 2))
                  (append-map (lambda (x) `(,@(compile-arg x) (,op)))
                              (drop args 2)))]))

;; Takes in a Scheme form for input and returns a list of instructions.
(define (compile-form input)
  (if (self-evaluating? input)
      input
      (let ((form-name (car input))
            (args      (cdr input)))
        (case form-name
          [(+) (compile-arithmetic 'add args)]
          [(-) (compile-arithmetic 'sub args)]
          [(*) (compile-arithmetic 'mul args)]
          [(/) (compile-arithmetic 'div args)]
          [else (list (car input))]))))
