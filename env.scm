;;;; env.scm
;;;;
;;;; Environment management.

(use srfi-1)

;; Replaces the value with key in list l
(define (replace l key value)
  (map (lambda (x)
	 (if (equal? (car x) key)
             (list key value)
	     x))
       l))

;; Makes a new environment
(define (make-env)
  '((defs ((placeholder 123)))))

;; Adds a new symbol to the environment with the given form.
(define (env-add-def env symbol form)
  (replace env 'defs
	   (append 
	    (cdr (assq 'defs env))
	    (list (list symbol form)))))

;; Gets the define for symbol in environment env
(define (env-get-def env symbol)
  (let ((form (assq symbol (cadr (assq 'defs env)))))
    (if form
        (cadr form)
        'error)))
