;;;; repl.scm
;;;;
;;;; Runs a read evaluate print loop function in Scheme for Scheme.

(use srfi-1)
(load "env.scm")
(load "vm.scm")

;;; Constants
(define prompt "-: ")
(define doublequote-char (string #\"))

;; Reads input from the user
(define (read-input)
  (read))

;; Executes a form.  Input is expected to be a list with the executable symbol
;; being the first element.  All other elements in input are evaluated and
;; placed on the environment stack.
(define (exec-form input env)
  (for-each (lambda (x) (exec-instruction! env 'push (vm-eval x env)))
	    (cdr input))
  (exec-instruction! env (car input))
  (exec-instruction! env 'pop))

;; Stolen from: http://pointlessprogramming.wordpress.com/2011/02/24/lispy-in-chicken-self-evaluating-values/
(define (self-evaulating? expr)
  (or (string? expr)
      (number? expr)
      (null? expr)
      (char? expr)
      (boolean? expr)))

;; Evaluates an input string
(define (vm-eval input env)
  (cond
   ; Semi-hack used to exit the repl
   [(and (symbol? input)
	 (equal? input 'quit)) (values 'quit env)]   
   [(self-evaulating? input) (values input env)]
   [(symbol? input)  (values (env/get-def env input) env)]
   
   ; Process define forms
   [(and (list? input)
         (define-form? input))
    (define-form input env)]

   ; Process execution of lists
   [(and (list? input)
	 (instruction? (car input))) 
    (values (exec-form input env) env)]
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
      ; TODO: Seperate repl command logic from eval logic.  The repl should
      ; be catching this before passing to vm-eval.
      (if (not (equal? result 'quit))
	  (repl-inner new-env))))
  (repl-inner (make-env)))
