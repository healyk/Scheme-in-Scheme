;;;; vm.scm
;;;;
;;;; Contains virtual machine constructs and instructions.
;;;; Author: Kris Healy

;; Executes a form.  Input is expected to be a list with the executable symbol
;; being the first element.  All other elements in input are evaluated and
;; placed on the environment stack.
(define (exec-form input env)
  (for-each (lambda (x) (exec-instruction! env 'push (vm-eval x env)))
	    (reverse (cdr input)))
  (exec-instruction! env 'push (length (cdr input))) ; Length of arguments
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

   ; Process execution of built-in instructions
   [(and (list? input)
	 (instruction? (car input))) 
    (values (exec-form input env) env)]
   
   [else 'error]))

;;; All instructs take the environment as the first argument and an optional
;;; second argument.  All return 1 argument, except pop which will return a
;;; result as well as the environment.

(define instruction-symbols
  '(+ push pop - * / call))

(define (instruction? sym)
  (any (lambda (x) (equal? x sym))
       instruction-symbols))

(define (exec-instruction! env instruction . arg)
  (case instruction
    [(+)    (instruction/arithmetic env instruction)]
    [(-)    (instruction/arithmetic env instruction)]
    [(*)    (instruction/arithmetic env instruction)]
    [(/)    (instruction/arithmetic env instruction)]
    [(push) (instruction/push env (car arg))]
    [(pop)  (instruction/pop env)]
    [else 'error]))

(define (instruction/push env x)
  (env/push! env x)
  env)

;; Returns 2 values - the pop'ed value and then the environment
(define (instruction/pop env)
  (values (env/pop! env) env))

(define (instruction/arithmetic env op)
  (let ((arg-length (env/pop! env)))
    (let* ((num1 (if (> arg-length 1) (env/pop! env) 0))
           (num2 (if (> arg-length 0) (env/pop! env) 0)))
      (env/push! env
                 (case op
                   [(+) (+ num1 num2)]
                   [(-) (- num1 num2)]
                   [(*) (* num1 num2)]
                   [(/) (/ num1 num2)]))
      ; Now the result is on the stack.  If we still have arguments
      ; to add then re-iterate the entire process
      (if (> arg-length 2)
          (begin
            (env/push! env (- arg-length 1))
            (instruction/arithmetic env op))
          ; No more arguments - just ignore this case
          #f))))