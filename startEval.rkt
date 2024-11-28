#lang racket

(require "environment.rkt")

;; Core interpreter function
(define (evaluate expr env)
  (cond
    ;; Case: Constants (numbers, booleans)
    [(number? expr) expr]
    [(boolean? expr) expr]

    ;; Case: Variables
    [(symbol? expr) (lookup expr env)]

    ;; Case: Arithmetic operators
    [(and (list? expr) (member (first expr) '(+ - * /)))
     (evaluate-arithmetic (first expr) (second expr) (third expr) env)]

    ;; Case: Relational operators
    [(and (list? expr) (member (first expr) '(< <= = > >=)))
     (evaluate-relational (first expr) (second expr) (third expr) env)]

    ;; Case: Conditional (if)
    [(and (list? expr) (eq? 'if (first expr)))
     (evaluate-if (second expr) (third expr) (fourth expr) env)]

    ;; Error]]unrecognized expressions
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
)
