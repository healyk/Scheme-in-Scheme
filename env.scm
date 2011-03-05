;;;; env.scm
;;;;
;;;; Environment management.

(use srfi-1)
(use srfi-9)
(use srfi-69)

;; Defines a lisp environment.
(define-record-type lisp-env
  (make-lisp-env defs local-vars)
  lis-env?
  (defs         lisp-env/get-defs)
  (local-vars   lisp-env/get-local-vars))

;; Replaces the value with key in list l
(define (replace l key value)
  (map (lambda (x)
	 (if (equal? (car x) key)
             (list key value)
	     x))
       l))

;; Makes a new environment
(define (make-env)
  (make-lisp-env (make-hash-table) '()))

;; Adds a new symbol to the environment with the given form.
(define (env/add-def! env symbol form)
  (hash-table-set! (lisp-env/get-defs env) symbol form)
  env)

(define (env/get-def env symbol)
  (hash-table-ref/default (lisp-env/get-defs env) symbol 'error))
