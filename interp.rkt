#lang eopl

;; interpreter for the LET language.  The \commentboxes are the
;; latex code for inserting the rules into the code in the book.
;; These are too complicated to put here, see the text, sorry.

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
;; Page: 71
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
;; Page: 71
(define value-of
  (lambda (exp env)
    (cases expression exp
      
      (const-exp (num) (num-val num))

      (var-exp (var) (apply-env env var))
      
      (zero?-exp (exp1)
        ;; -----------------------
        ;; INSERT YOUR CODE HERE 
        ;; -----------------------
                (cases expression exp1
                  (rational-exp (rat1 rat2)
                   (if (zero? (car (expval->rat (value-of exp1 env))))
                       (bool-val #t)
                       (bool-val #f)))
                  (const-exp (num)
                    (if (zero? (expval->num (value-of exp1 env)))
                       (bool-val #t)
                       (bool-val #f)))
                  (op-exp (ex1 ex2 num)
                          (cases expression ex1
                            (const-exp (number1)
                                       (cases expression ex2
                                         (const-exp (number2)
                                                        (if (zero? (expval->num (value-of exp1 env)))
                                                            (bool-val #t)
                                                            (bool-val #f)))
                                         (rational-exp (rat1 rat2)
                                                      (if (zero? (car (expval->rat (value-of exp1 env))))
                                                          (bool-val #t)
                                                          (bool-val #f)))
                                         (else  (eopl:error 'invalid-input "invalid-input"))))
                            (rational-exp (rat1 rat2)
                                           (if (zero? (car (expval->rat (value-of exp1 env))))
                                                          (bool-val #t)
                                                          (bool-val #f)))
                            (else  (eopl:error 'invalid-input "invalid-input"))))
                  (else  (eopl:error 'invalid-input "invalid-input")))
                  
                                
        ;; -----------------------
      )
      
      (let-exp (var exp1 body)       
               (let ((val1 (value-of exp1 env)))
                 (value-of body
                           (extend-env var val1 env))))

      ;; -----------------------
      ;; INSERT YOUR CODE HERE 
      ;; -----------------------
      (str-exp (str) (str-val str))

      (rational-exp (num1 num2)
                     (if (zero? num2)
                        (eopl:error 'zero-denominator "zero-denominator")
                        (rational-val (cons num1 num2))))

      (op-exp (exp1 exp2 num)
           (cases expression exp1
             (const-exp (num1)
                   (let ((val1 (value-of exp1 env)))
                         (let ((num1 (expval->num val1)))
                               (cases expression exp2
                                 (const-exp (num2)
                                            (let ((val2 (value-of exp2 env)))
                                                  (let ((num2 (expval->num val2)))
                                                       (num-val
                                                        (cond ((eq? num 1) (+ num1 num2))
                                                              ((eq? num 2) (* num1 num2))
                                                              ((eq? num 3) (/ num1 num2))
                                                              (else (- num1 num2)))))))
                                 (rational-exp (rat1 rat2)
                                                 (let ((val3 (value-of exp2 env)))
                                                    (let ((rat (expval->rat val3)))
                                                      (let ((rat1 (car rat))
                                                            (rat2 (cdr rat)))
                                                        (rational-val
                                                         (cond ((eq? num 1) (cons (+ (* num1 rat2) rat1 ) rat2))
                                                              ((eq? num 2) (cons (* num1 rat1) rat2))
                                                              ((eq? num 3) (cons (* num1 rat2) rat1))
                                                              (else (cons (- (* num1 rat2) rat1 ) rat2))))))))
                                  (op-exp (ex1 ex2 number)
                                           (value-of exp2 env))
                                 (var-exp (var)
                                          (value-of exp2 env))
                                 (else (eopl:error 'not-a-num-or-rat "not-a-num-or-rat"))))))
             
              (rational-exp (rat1 rat2)
                            (let ((val1 (value-of exp1 env)))
                              (let ((rat-first (expval->rat val1)))
                                  (let ((rat1 (car rat-first))
                                        (rat2 (cdr rat-first)))
                                         (cases expression exp2
                                           (const-exp (num1)
                                                      (let ((val2 (value-of exp2 env)))
                                                        (let ((num1 (expval->num val2)))
                                                          (rational-val
                                                           (cond ((eq? num 1) (cons (+ (* num1 rat2) rat1 ) rat2))
                                                                 ((eq? num 2) (cons (* num1 rat1) rat2))
                                                                 ((eq? num 3) (cons rat1 (* num1 rat2)))
                                                                 (else (cons (- rat1 (* num1 rat2)) rat2)))))))
                                           (rational-exp (rat3 rat4)
                                                 (let ((val3 (value-of exp2 env)))
                                                    (let ((rat-second (expval->rat val3)))
                                                      (let ((rat3 (car rat-second))
                                                            (rat4 (cdr rat-second)))
                                                        (rational-val
                                                         (cond ((eq? num 1) (cons (+ (* rat1 rat4) (* rat3 rat2)) (* rat2 rat4)))
                                                              ((eq? num 2) (cons (* rat1 rat3) (* rat2 rat4)))
                                                              ((eq? num 3) (cons (* rat1 rat4) (* rat2 rat3)))
                                                              (else (cons (- (* rat1 rat4) (* rat3 rat2)) (* rat2 rat4)))))))))
                                            (op-exp (ex1 ex2 number)
                                                    (value-of exp2 env))
                                            (var-exp (var)
                                                        (value-of exp2 env))
                                 (else (eopl:error 'not-a-num-or-rat "not-a-num-or-rat")))))))
             (op-exp (ex1 ex2 number)
                     (value-of exp1 env))
             
             (var-exp (var)
                    (value-of exp1 env))
             
             (else (eopl:error 'not-a-num-or-rat "not-a-num-or-rat"))))


      (if-exp (exp1 exp2 conds exps exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env);;exp1 is true, return exp2
                    (if-exp-helper conds exps (value-of exp3 env) env))))
                       
              
      ;; -----------------------

      )))

(define if-exp-helper
  (lambda (conds exps val env)
    (if (null? conds)
        val ;;conds are not true, return exp3
        (if (expval->bool (value-of (car conds) env))
            (value-of (car exps) env) ;;one of the conds is true
            (if-exp-helper (cdr conds) (cdr exps) val env)))))
