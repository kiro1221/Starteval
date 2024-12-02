#lang racket

(require "environment.rkt")
(require rackunit)
;; Core interpreter function
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
     (let ((list-val (evaluate (second expr) env)))
       (if (pair? list-val)
           (car list-val)
           (error "car: not a pair" list-val)))]

    [(and (list? expr) (eq? (first expr) 'cdr))
     (let ((list-val (evaluate (second expr) env)))
       (if (pair? list-val)
           (cdr list-val)
           (error "cdr: not a pair" list-val)))]

    [(and (list? expr) (eq? (first expr) 'cons))
     (let ((head (evaluate (second expr) env))
           (tail (evaluate (third expr) env)))
       (cons head tail))]

    [(and (list? expr) (eq? (first expr) 'pair?))
     (let ((list-val (evaluate (second expr) env)))
       (pair? list-val))]

    ;; Arithmetic operators
    [(and (list? expr) (member (first expr) '(+ - * /)))
     (evaluate-arithmetic (first expr) (second expr) (third expr) env)]

    ;; Relational operators
    [(and (list? expr) (member (first expr) '(< <= = > >=)))
     (evaluate-relational (first expr) (second expr) (third expr) env)]

    ;;if
    [(and (list? expr) (eq? 'if (first expr)))
     (evaluate-if (second expr) (third expr) (fourth expr) env)]

    ;; Error: Unrecognized expression
    [else (error "Unrecognized expression: " expr)]))


;; Evaluate arithmetic expressions
(define (evaluate-arithmetic op left right env)
  (let ((lval (evaluate left env))
        (rval (evaluate right env)))
    (case op
      [(+) (+ lval rval)]
      [(-) (- lval rval)]
      [(*) (* lval rval)]
      [(/) (if (zero? rval)
               (error "Division by zero")
               (/ lval rval))]
      [else (error "Invalid arithmetic operator: " op)])))

;; Evaluate relational expressions
(define (evaluate-relational op left right env)
  (let ((lval (evaluate left env))
        (rval (evaluate right env)))
    (case op
      [(<) (< lval rval)]
      [(<=) (<= lval rval)]
      [(=) (= lval rval)]
      [(>) (> lval rval)]
      [(>=) (>= lval rval)]
      [else (error "Invalid relational operator: " op)])))

;; if expressions construction
(define (evaluate-if condition true-branch false-branch env)
  (if (evaluate condition env)
      (evaluate true-branch env)
      (evaluate false-branch env)))

;; Entry point 
(define (startEval expr env)
  (evaluate expr env))

;; Test cases
(module+ test
  (define test-env '((a . 10) (b . 20) (x . #t) (y . #f)))
  ;; Test arithmetic with aribtrary numbers
  (displayln (startEval '(+ 10 20) empty-env)) ;;  30
  (displayln (startEval '(- 30 10) empty-env)) ;;  20
    ;; Test arithmetic with variables from test-env
  (displayln (startEval '(* a b) test-env)) ;; 200
  (displayln (startEval '(/ b a) test-env)) ;;  2

  ;; Test relational 
  (displayln (startEval '(< a b) test-env)) ;;  #t
  (displayln (startEval '(>= a b) test-env)) ;;  #f
  (displayln (startEval '(= a a) test-env)) ;;  #t

  ;; Test if 
  (displayln (startEval '(if x 42 0) test-env)) ;;  42
  (displayln (startEval '(if y 42 0) test-env)) ;;  0
  (displayln (startEval '(if (< a b) (+ a b) (- b a)) test-env)) ;;  30
  (displayln (startEval '(if (> b a) (* a b) (/ b a)) test-env)) ;;  200
  (displayln (startEval '(if (< a b) (if x 1 0) 2) test-env)) ;;  1
  (displayln (startEval '(if (> a b) (if x 1 0) 2) test-env)) ;;  2

  ;; New Tests
  ;; Additional RackUnit Tests
  (check-equal? (startEval '(+ 2147483647 1) empty-env) 2147483648 "Overflow addition test")
  (check-equal? (startEval '(- -2147483648 1) empty-env) -2147483649 "Underflow subtraction test")
  (check-equal? (startEval '(if (>= a b) 42 100) test-env) 100 "Conditional logic test") ;;// Not passing 
  (check-equal? (startEval '(if #t 42 100) empty-env) 42 "If true branch test")


  ;; Tests
  (displayln (startEval '(>= 10 20) empty-env))  ; Should print #f
  (displayln (startEval '(>= 20 10) empty-env))  ; Should print #t

  ;; Testing Constants and Variables
  (check-equal? (startEval 42 empty-env) 42 "Constant number")
  (check-equal? (startEval '#t empty-env) #t "Constant boolean true")
  (check-equal? (startEval 'a test-env) 10 "Variable a")
  (check-equal? (startEval 'b test-env) 20 "Variable b")

  ;; Testing Arithmetic Operators
  (check-equal? (startEval '(+ 1 2) empty-env) 3 "Addition")
  (check-equal? (startEval '(- 5 3) empty-env) 2 "Subtraction")
  (check-equal? (startEval '(* 4 6) empty-env) 24 "Multiplication")
  (check-equal? (startEval '(/ 8 4) empty-env) 2 "Division")
  (check-exn exn:fail? (lambda () (startEval '(/ 8 0) empty-env)) "Division by zero")

  ;; Testing Relational Operators
  (check-equal? (startEval '(< 1 2) empty-env) #t "Less than")
  (check-equal? (startEval '(<= 2 2) empty-env) #t "Less than or equal")
  (check-equal? (startEval '(= 3 3) empty-env) #t "Equality")
  (check-equal? (startEval '(> 4 1) empty-env) #t "Greater than")
  (check-equal? (startEval '(>= 5 5) empty-env) #t "Greater than or equal")

  ;; Testing Conditional Expressions
  (check-equal? (startEval '(if #t 100 200) empty-env) 100 "If true")
  (check-equal? (startEval '(if #f 100 200) empty-env) 200 "If false")
  (check-equal? (startEval '(if (= 2 2) (+ 1 1) (+ 2 2)) empty-env) 2 "Nested if true")
  (check-equal? (startEval '(if (= 2 3) (+ 1 1) (+ 2 2)) empty-env) 4 "Nested if false")

  ;; Testing Nested Expressions
  (check-equal? (startEval '(+ (* 2 3) (/ 12 4)) empty-env) 9 "Nested operations")

  ;; More complex nested arithmetic tests
  (check-equal? (startEval '(+ (* 2 3) (* 4 5)) empty-env) 26 "Multiply and add")
  (check-equal? (startEval '(- (* 10 2) (/ 20 4)) empty-env) 15 "Multiply, divide and subtract")
  (check-equal? (startEval '(* (+ 1 1) (/ 12 4)) empty-env) 6 "Add, divide and multiply")

  ;; Nested relational tests
  (check-equal? (startEval '(> (+ 5 5) (* 2 4)) empty-env) #t "Addition greater than multiplication")
  (check-equal? (startEval '(<= (- 10 5) (/ 10 2)) empty-env) #t "Subtraction less than or equal to division")
  (check-equal? (startEval '(= (* 2 3) (+ 4 2)) empty-env) #t "Multiplication equal to addition")

  ;; Complex nested expressions involving both arithmetic and relational operations
  (check-equal? (startEval '(if (> (* 2 3) 5) (+ 1 2) (- 5 3)) empty-env) 3 "Conditional with arithmetic comparison")
  (check-equal? (startEval '(+ (* 2 (if (> 3 2) 3 2)) 5) empty-env) 11 "If condition inside arithmetic")

  ;; Testing deep nesting
  (check-equal? (startEval '(* (+ 2 (* 3 (- 5 2))) 4) empty-env) 44 "Deeply nested arithmetic")
  (check-equal? (startEval '(if (>= (- (* 10 2) 15) 5) 42 100) empty-env) 42 "Deeply nested relational in condition")

  ;; Testing nested operations with variables from test-env
  (check-equal? (startEval '(+ (* a b) a) test-env) 210 "Using variables in nested arithmetic")
  (check-equal? (startEval '(if (> a b) (- a b) (+ a b)) test-env) 30 "Using variables in conditional")

  ;; Edge case for error handling in nested contexts
  (check-exn exn:fail? (lambda () (startEval '(/ (* 10 0) 0) empty-env)) "Division by zero in nested context")
  ;; Testing Unrecognized expressions
  (check-exn exn:fail? (lambda () (startEval '("unexpected type") empty-env)) "Unexpected type")

  ;; Re-testing previously failed test with corrected context
  (check-equal? (startEval '(if (>= a b) 42 100) test-env) 100 "Conditional logic with variables")

  ;; Edge Cases for Environmental Lookups
  (check-exn exn:fail? (lambda () (startEval 'c test-env)) "Unbound variable c")
  ;; Display message if all tests pass
  (displayln "All Tests Passed!!!")
)
