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
