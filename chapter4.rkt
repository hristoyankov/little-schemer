#lang racket
(require "preface.rkt")

(define add1
  (lambda (n)
    (+ n 1)))

(eq? (add1 67) 68)

(define sub1
  (lambda (n)
    (- n 1)))

(eq? (sub1 5) 4)

(zero? 0)
(not (zero? 1492))

(define o+
  (lambda (n m)
    (cond
      ((zero? n) m)
      (else (add1 (o+ (sub1 n) m))))))

(eq? (o+ 46 12) 58)

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

(eq? (o- 14 3) 11)
(eq? (o- 17 9) 8)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))

(eq? (addtup '(3 5 2 8)) 18)
(eq? (addtup '(15 6 7 12 3)) 43)

(define X
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (X n (sub1 m)))))))

(eq? (X 3 5) 15)
(eq? (X 13 4) 52)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((and (null? tup1) (null? tup2)) '())
      (else (cons (o+ (car tup1)
                      (car tup2))
                  (tup+ (cdr tup1)
                        (cdr tup2)))))))

(equal? (tup+ '(3 6) '(2 3)) '(5 9))
(equal? (tup+ '(8 4 5) '(3 7 6)) '(11 11 11))
