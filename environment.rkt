#lang racket

(provide lookup extend-env empty-env)

(define empty-env '()) ;; empty env

(define (lookup var env) ;; Lookup variable 
  (cond
    [(null? env) (error "Unbound variable: " var)]
    [(equal? var (car (car env))) (cdr (car env))]
    [else (lookup var (cdr env))]))

;; Extend the environment with new bindings
;; bindings: list of (var . value) pairs
(define (extend-env bindings env)
  (append bindings env))
