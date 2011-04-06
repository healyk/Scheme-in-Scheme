;;;; vm.scm
;;;;
;;;; Contains virtual machine constructs and instructions.
;;;; Author: Kris Healy

(load "compile.scm")

;; Stolen from: http://pointlessprogramming.wordpress.com/2011/02/24/lispy-in-chicken-self-evaluating-values/
(define (self-evaluating? expr)
  (or (string? expr)
      (number? expr)
      (null? expr)
      (char? expr)
      (boolean? expr)))

;; Executes a compiled set of instructions.
(define (exec-compiled-form compiled-form env)
  (for-each (lambda (instruction)
              (if (equal? (length instruction) 1)
                  (exec-instruction! env (car instruction))
                  (exec-instruction! env (car instruction)
                                         (cadr instruction))))
            compiled-form)
  (if (not (env/stack-empty? env))
      (exec-instruction! env 'pop)
      'nothing))

;; Evaluates an input string
(define (vm-eval input env)
  (cond
   ; Semi-hack used to exit the repl
   [(and (symbol? input)
	 (equal? input 'quit)) (values 'quit env)]   
   [(self-evaluating? input) (values input env)]
   [(symbol? input)  (values (env/get-def env input) env)]
   
   ; Process define forms
   [(and (list? input)
         (define-form? input))
    (define-form input env)]

   ; Process execution of built-in instructions
   [(and (list? input)
	 (instruction? (car input)))
    (values (exec-compiled-form (compile-form input) env) env)]
   
   [else 'error]))

;;; All instructs take the environment as the first argument and an optional
;;; second argument.  All return 1 argument, except pop which will return a
;;; result as well as the environment.

(define instruction-symbols
  '(+ push pop - * /))

(define (instruction? sym)
  (any (lambda (x) (equal? x sym))
       instruction-symbols))

(define (exec-instruction! env instruction . arg)
  (case instruction
    [(add)  (instruction/arithmetic env instruction)]
    [(sub)  (instruction/arithmetic env instruction)]
    [(mul)  (instruction/arithmetic env instruction)]
    [(div)  (instruction/arithmetic env instruction)]
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
  (let* ((num2   (env/pop! env))
         (num1   (env/pop! env))
         (result (case op
                   [(add) (+ num1 num2)]
                   [(sub) (- num1 num2)]
                   [(mul) (* num1 num2)]
                   [(div) (/ num1 num2)])))
    (env/push! env result))
  env)
