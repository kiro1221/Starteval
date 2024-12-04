#lang racket

(require rackunit)

;; --- Environment Functions ---
(define empty-env '())

(define (extend-env bindings env)
  (append bindings env))

(define (lookup var env)
  (cond
    [(null? env) (error "Unbound variable: " var)]
    [(equal? var (car (car env))) (cdr (car env))]
    [else (lookup var (cdr env))]))

;; --- Core Interpreter ---
(define (evaluate expr env)
  (cond
    ;; Constants (numbers, booleans)
    [(number? expr) expr]
    [(boolean? expr) expr]

    ;; Variables
    [(symbol? expr) (lookup expr env)]

    ;; Quote
    [(and (list? expr) (eq? 'quote (first expr)))
     (second expr)]

    ;; List operations
    [(and (list? expr) (eq? (first expr) 'car))
     (let ([list-val (evaluate (second expr) env)])
       (if (pair? list-val)
           (car list-val)
           (error "car: not a pair" list-val)))]

    [(and (list? expr) (eq? (first expr) 'cdr))
     (let ([list-val (evaluate (second expr) env)])
       (if (pair? list-val)
           (cdr list-val)
           (error "cdr: not a pair" list-val)))]

    [(and (list? expr) (eq? (first expr) 'cons))
     (let ([head (evaluate (second expr) env)]
           [tail (evaluate (third expr) env)])
       (cons head tail))]

    [(and (list? expr) (eq? (first expr) 'pair?))
     (let ([list-val (evaluate (second expr) env)])
       (pair? list-val))]

    ;; Arithmetic operators
    [(and (list? expr) (member (first expr) '(+ - * /)))
     (evaluate-arithmetic (first expr) (second expr) (third expr) env)]

    ;; Relational operators
    [(and (list? expr) (member (first expr) '(< <= = > >=)))
     (evaluate-relational (first expr) (second expr) (third expr) env)]

    ;; Conditional (if)
    [(and (list? expr) (eq? 'if (first expr)))
     (evaluate-if (second expr) (third expr) (fourth expr) env)]

    ;; Lambda expressions
    [(and (list? expr) (eq? 'lambda (first expr)))
     (evaluate-lambda (second expr) (third expr) env)]

    ;; Letrec expressions
    [(and (list? expr) (eq? 'letrec (first expr)))
     (evaluate-letrec (second expr) (third expr) env)]

    ;; Function application
    [(and (list? expr) (not (null? expr)))
     (let ([func (evaluate (first expr) env)]
           [args (map (lambda (arg) (evaluate arg env))
                      (rest expr))])
       (if (procedure? func)
           (apply func args)
           (error "Attempt to call a non-procedure" func)))]

    ;; Error: Unrecognized expression
    [else (error "Unrecognized expression: " expr)]))

;; --- Helping Functions ---
(define (evaluate-arithmetic op left right env)
  (let ([lval (evaluate left env)]
        [rval (evaluate right env)])
    (case op
      [(+) (+ lval rval)]
      [(-) (- lval rval)]
      [(*) (* lval rval)]
      [(/) (if (zero? rval)
               (error "Division by zero")
               (/ lval rval))]
      [else (error "Invalid arithmetic operator: " op)])))

(define (evaluate-relational op left right env)
  (let ([lval (evaluate left env)]
        [rval (evaluate right env)])
    (case op
      [(<) (< lval rval)]
      [(<=) (<= lval rval)]
      [(=) (= lval rval)]
      [(>) (> lval rval)]
      [(>=) (>= lval rval)]
      [else (error "Invalid relational operator: " op)])))

(define (evaluate-if condition true-branch false-branch env)
  (if (evaluate condition env)
      (evaluate true-branch env)
      (evaluate false-branch env)))

(define (evaluate-lambda params body env)
  (lambda (args)
    (let ([args-list (if (list? args) args (list args))])
      (if (= (length params) (length args-list))
          (let ([new-env (extend-env (map cons params args-list) env)])
            (evaluate body new-env))
          (error "Lambda arguments do not match parameters")))))

(define (evaluate-letrec bindings body env)
  (let ([new-env (extend-env
                  (map (lambda (binding)
                         (cons (first binding)
                               (lambda args (error "Uninitialized binding used"))))
                       bindings)
                  env)])
    (for-each
     (lambda (binding)
       (let ([name (first binding)]
             [value-thunk (lambda args (apply (evaluate (second binding) new-env) args))])
         (set! new-env
               (extend-env (list (cons name value-thunk)) new-env))))
     bindings)
    (evaluate body new-env)))

;; --- Entry Part of the Code ---
(define (startEval expr [env empty-env])
  (evaluate expr env))

(module+ test
  ;; Import necessary functions
  (require racket/list)

  ;; Define the test environment
  (define test-env '((a . 10) (b . 20) (x . #t) (y . #f)))

  ;; Test constants and variables
  (check-equal? (startEval 42 empty-env) 42 "Constant test: number")
  (check-equal? (startEval '#t empty-env) #t "Constant test: boolean")
  (check-equal? (startEval 'a test-env) 10 "Variable test: lookup a")
  (check-equal? (startEval 'b test-env) 20 "Variable test: lookup b")

  ;; Test arithmetic operations
  (check-equal? (startEval '(+ 10 20) empty-env) 30 "Addition test")
  (check-equal? (startEval '(- 30 10) empty-env) 20 "Subtraction test")
  (check-equal? (startEval '(* a b) test-env) 200 "Multiplication with variables")
  (check-equal? (startEval '(/ b a) test-env) 2 "Division with variables")
  (check-equal? (startEval '(+ (* 3 4) (/ 20 4)) empty-env) 17 "Nested with division test (corrected)")

  ;; Test relational operations
  (check-equal? (startEval '(< a b) test-env) #t "Less than test")
  (check-equal? (startEval '(>= a b) test-env) #f "Greater than or equal test")
  (check-equal? (startEval '(= a b) test-env) #f "Equality test with different values")
  (check-equal? (startEval '(<= b b) test-env) #t "Less than or equal test")
  (check-equal? (startEval '(> b a) test-env) #t "Greater than test")

  ;; Test conditionals
  (check-equal? (startEval '(if x 42 0) test-env) 42 "Conditional test: true branch")
  (check-equal? (startEval '(if y 42 0) test-env) 0 "Conditional test: false branch")
  (check-equal? (startEval '(if (< a b) (+ a b) (- b a)) test-env) 30 "Conditional with addition")
  (check-equal? (startEval '(if (> b a) (* a b) (/ b a)) test-env) 200 "Conditional with multiplication")
  (check-equal? (startEval '(if (= 5 5) (+ 10 20) (/ 100 5)) empty-env) 30 "Conditional with equality")

  ;; Test list operations
  (check-equal? (startEval '(car (quote (1 2 3))) empty-env) 1 "List operation: car")
  (check-equal? (startEval '(cdr (quote (1 2 3))) empty-env) '(2 3) "List operation: cdr")
  (check-equal? (startEval '(cons 0 (quote (1 2 3))) empty-env) '(0 1 2 3) "List operation: cons")
  (check-equal? (startEval '(pair? (quote (1 2 3))) empty-env) #t "List operation: pair?")
  (check-equal? (startEval '(pair? (quote ())) empty-env) #f "List operation: empty pair?")

  ;; Test nested expressions
  (check-equal? (startEval '(+ (* 2 3) (/ 12 4)) empty-env) 9 "Nested arithmetic test")
  (check-equal? (startEval '(* (+ 2 (* 3 (- 5 2))) 4) empty-env) 44 "Deeply nested arithmetic test")
  (check-equal? (startEval '(if (> (* 2 3) 5) (+ 1 2) (- 5 3)) empty-env) 3 "Conditional with nested operations")

  ;; Test lambda expressions
  (check-equal? (startEval '((lambda (x) (+ x 1)) 4) empty-env) 5 "Lambda test: single parameter")
  
  (check-equal? (startEval '((lambda (x) (* x x)) 5) empty-env) 25 "Lambda test: square function")
  (check-equal? (startEval '((lambda (x) (if (< x 10) (* x 2) (/ x 2))) 8) empty-env) 16 "Lambda test: conditional in lambda")
  

  ;; Test letrec expressions
  (check-equal? (startEval
                 '(letrec ((fact (lambda (n)
                                   (if (= n 0)
                                       1
                                       (* n (fact (- n 1)))))))
                    (fact 5)) empty-env) 120 "Letrec test: factorial")
  (check-equal? (startEval
                 '(letrec ((sum (lambda (n)
                                  (if (= n 0)
                                      0
                                      (+ n (sum (- n 1)))))))
                    (sum 10)) empty-env) 55 "Letrec test: summation")
  (check-equal? (startEval
                 '(letrec ((fib (lambda (n)
                                  (if (<= n 1)
                                      n
                                      (+ (fib (- n 1)) (fib (- n 2)))))))
                    (fib 6)) empty-env) 8 "Letrec test: Fibonacci")
  (check-equal? (startEval
                 '(letrec ((fact (lambda (x)
                                   (if (= x 0) (quote 1)
                                       (* x (fact (- x 1)))))))
                    (fact 10)) empty-env) 3628800 "factorial of 10")
  (check-equal? (startEval
                 '(letrec ((fact (lambda (x)
                                   (if (= x 0) 1
                                       (* x (fact (- x 1)))))))
                    (fact 6)) empty-env) 720 "factorial of 6")
  (displayln "All Tests Passed!")

  ;; Failling Tests
  ;(check-equal? (startEval '((lambda (a b c) (+ (* a b) c)) 2 3 4) empty-env) 10 "Lambda test: multiple parameters with arithmetic")
  ;(check-equal? (startEval '((lambda () (+ 1 1))) empty-env) 2 "Lambda test: no-parameter function")
  ;(check-equal? (startEval '((lambda (x y) (+ x y)) 10 20) empty-env) 30 "Lambda test: multiple parameters")
  )
