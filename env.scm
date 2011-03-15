;;;; env.scm
;;;;
;;;; Environment management.

(use srfi-1)
(use srfi-9)
(use srfi-69)

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

;;; Stack functions
(define (env/pop! env)
  (let ((element (car (lisp-env/get-stack env))))
    (lisp-env/set-stack! env (cdr (lisp-env/get-stack env)))
    element))

(define (env/push! env val)
  (lisp-env/set-stack! env (append (list val) (lisp-env/get-stack env))))

