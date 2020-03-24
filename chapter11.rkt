#lang racket
(require "preface.rkt")
(require "chapter4.rkt")

(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) preceding)
                (two-in-a-row-b? (car lat) (cdr lat)))))))

(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat) (cdr lat))))))

(eq? #f (two-in-a-row? '(Italian sardines spaghetti parsley)))
(eq? #t (two-in-a-row? '(Italian sardines sardines spaghetti parsley)))
(eq? #f (two-in-a-row? '(Italian sardines more sardines spaghetti)))

(define sum-of-prefixes-b
  (lambda (sonssf tup)
    (cond
      ((null? tup) '())
      (else (cons (+ sonssf (car tup))
                  (sum-of-prefixes-b (+ sonssf (car tup))
                                     (cdr tup)))))))

(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

(equal? '(2 3 12 29 29) (sum-of-prefixes '(2 1 9 17 0)))

(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) '())
      (else (cons (pick (car tup) (cons (car tup) rev-pre))
                  (scramble-b (cdr tup) (cons (car tup) rev-pre)))))))

(define scramble
  (lambda (tup)
    (scramble-b tup '())))

(equal? '(1 1 1 1 1 4 1 1 1 9)
        (scramble '(1 1 1 3 4 2 1 1 9 2)))
(equal? '(1 1 1 1 1 1 1 1 1 )
        (scramble '(1 2 3 4 5 6 7 8 9)))
(equal? '(1 1 1 1 1 1 1 1 2 8 2)
        (scramble '(1 2 3 1 2 3 4 1 8 2 10)))
