#lang racket

(provide empty-env lookup extend-env)

(define empty-env '()) ;; Empty environment

(define (lookup var env)
  (cond
    [(null? env) (error "Unbound variable: " var)]
    [(equal? var (car (car env))) (cdr (car env))]
    [else (lookup var (cdr env))]))

(define (extend-env bindings env)
  (append bindings env)) ;; Add new bindings
