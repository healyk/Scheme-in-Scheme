;;;; env.scm
;;;;
;;;; Environment management.

;; Defines a lisp environment.
(define-record-type lisp-env
  (make-lisp-env defs stack)
  lisp-env?
  (defs          lisp-env/get-defs)
  (stack         lisp-env/get-stack lisp-env/set-stack!))

;; Makes a new environment
(define (make-env)
  (make-lisp-env (make-hash-table) '()))

;; Adds a new symbol to the environment with the given form.
(define (env/add-def! env symbol form)
  (hash-table-set! (lisp-env/get-defs env) symbol form)
  env)

(define (env/get-def env symbol)
  (hash-table-ref/default (lisp-env/get-defs env) symbol 'error))

;;;
;;; Stack functions
;;;
;;; These will push/pop objects off the environment stack.
;;;
(define (env/pop! env)
  (let ((element (car (lisp-env/get-stack env))))
    (lisp-env/set-stack! env (cdr (lisp-env/get-stack env)))
    element))

(define (env/push! env val)
  (lisp-env/set-stack! env (append (list val) (lisp-env/get-stack env))))

(define (env/stack-empty? env)
  (null? (lisp-env/get-stack env)))

;; Checks a list to see if the first symbol is define
(define (define-form? input)
  (and (not (null? input))
       (equal? (car input) (string->symbol "define"))
       (>= (length input) 2)))

;; Takes in a define and adds it to the environment
(define (define-form input env)
  (let ((form (if (> (length input) 2)
                  (vm-eval (caddr input) env)
                  '())))
    (if (not (equal? form 'error))
        (values input (env/add-def! env (cadr input) form))
        (values 'error env))))
