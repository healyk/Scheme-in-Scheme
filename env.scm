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

(define (make-env)
  '((defs )))

;; Adds a new symbol to the environment with the given form.
(define (env-add-def env symbol form)
  (replace env 'defs 
	   (append 
	    (cdr (assq 'defs env))
	    (list (list symbol form)))))

#;
(define (env-get-def env symbol)
  (filter (lambda (x)
	    (equal? (car x) symbol))
	  (assq 'defs env)))