;;;; repl.scm
;;;;
;;;; Runs a read evaluate print loop function in Scheme for Scheme.

(load "env.scm")

;;; Constants
(define prompt "-: ")
(define doublequote-char (string #\"))

;; Reads input from the user
(define (read-input)
  (read))

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
        (values input (env-add-def env (cadr input) form))
        (values 'error env))))

;; Evaluates an input string
(define (vm-eval input env)
  (cond
   [(string? input)  (values input env)]
   [(number? input)  (values input env)]
   [(null? input)    (values input env)]
   [(char? input)    (values input env)]
   [(boolean? input) (values input env)]
   [(symbol? input)  (values (env-get-def env input) env)]
   
   ; Process define forms
   [(and (list? input)
         (define-form? input))
    (define-form input env)]
   [else 'error]))

;; Prints objects out to stdout
(define (print-obj obj)
  (cond
   [(string? obj) (display
                  (string-append doublequote-char obj doublequote-char))]
   [(char? obj)   (display (string-append "#\\" (string obj)))]
   [(equal? obj 'error)  (display "ERROR!")]
   [else          (display obj)])
  (newline))

;; Entry point into the repl.  This will currently loop infinitely.
(define (repl)
  (define (repl-inner env)
    (display prompt)
    (let-values (((result new-env) (vm-eval (read-input) env)))
      (print-obj result)
      (repl-inner new-env)))
  (repl-inner (make-env)))
