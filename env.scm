;;;; env.scm
;;;;
;;;; Environment management.

;; Replaces the value with key in list l
(define (replace l key value)
  (map (lambda (x)
	 (if (equal? (car x) key) 
	     (list key value)
	     x))
       l))

;; Returns a list of values from l which func returns true
(define (filter func l)
  (if (null? l)
      '()
      (append 
       (if (func (car l))
	   (list (car l))
	   '())
       (filter func (cdr l)))))

;; Makes a new environment
(define (make-env)
  '((defs )))

;; Adds a new symbol to the environment with the given form.
(define (env-add-def env symbol form)
  (replace env 'defs 
	   (append 
	    (cdr (assq 'defs env))
	    (list (list symbol form)))))

;; Gets the define for symbol in environment env
(define (env-get-def env symbol)
  (cadr
   (assq symbol 
	 (cadr (assq 'defs env)))))