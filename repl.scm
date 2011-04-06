;;;; repl.scm
;;;;
;;;; Runs a read evaluate print loop function in Scheme for Scheme.

(use srfi-1)
(use srfi-9)
(use srfi-69)

(load "env.scm")
(load "vm.scm")

;;; Constants
(define prompt "-: ")
(define doublequote-char (string #\"))

;; Reads input from the user
(define (read-input)
  (read))

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
	  (repl-inner new-env)
          '())))
  (repl-inner (make-env)))
