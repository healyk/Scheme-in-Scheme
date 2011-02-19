;;;; repl.scm
;;;;
;;;; Runs a read evaluate print loop function in Scheme for Scheme.

;;; Constants
(define prompt "-: ")
(define doublequote-char (string #\"))

;; Reads input from the user
(define (read-input)
  (read))

;; Evaluates an input string
(define (vm-eval input)
  (cond
   [(string? input)  input]
   [(number? input)  input]
   [(null? input)    input]
   [(char? input)    input]
   [(boolean? input) input]
   [else "Error!"]))

;; Prints objects out to stdout
(define (print-obj obj)
  (cond
   [(string? obj) (display
                   (string-append doublequote-char obj doublequote-char))]
   [(char? obj)   (display (string-append "#\\" (string obj)))]
   [else          (display obj)])
  (newline))

;; Entry point into the repl.  This will currently loop infinitely.
(define (repl)
  (display prompt)
  (let* [(input     (read-input))
         (evaluated (vm-eval input))]
    (print-obj evaluated)
    (repl)))
