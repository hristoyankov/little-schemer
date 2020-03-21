#lang racket
(require "preface.rkt")
(require "chapter4.rkt")

(define rember-f1
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? a (car l)) (cdr l))
      (else (cons (car l) (rember-f1 test? a (cdr l)))))))

(equal? '(6 2 3) (rember-f1 o= 5 '(6 2 5 3)))
(equal? '(beans are good) (rember-f1 eq? 'jelly '(jelly beans are good)))
(equal? '(lemonade and (cake)) (rember-f1 equal? '(pop corn) '(lemonade (pop corn) and (cake))))

(define rember
  (lambda (a l)
    (rember-f1 equal? a l)))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? a x))))

(define eq?-salad
  (eq?-c 'salad))

(eq? #t (eq?-salad 'salad))
(eq? #f (eq?-salad 'tuna))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define rember-eq?
  (rember-f eq?))

(equal? '(salad is good) (rember-eq? 'tuna '(tuna salad is good)))
