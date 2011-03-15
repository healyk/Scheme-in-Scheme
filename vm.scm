;;;; vm.scm
;;;;
;;;; Contains virtual machine constructs and instructions.
;;;; Author: Kris Healy

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
  ; Pop the numbers off the stack in reverse order.  This is important
  ; for subtraction and division because they aren't communitive!
  (let* ((num2 (env/pop! env))
 	 (num1 (env/pop! env)))
    (env/push! env
	       (case op
		 [(+) (+ num1 num2)]
		 [(-) (- num1 num2)]
		 [(*) (* num1 num2)]
		 [(/) (/ num1 num2)]))))

