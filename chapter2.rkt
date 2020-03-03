#lang racket
(require "preface.rkt")

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(eq? #t (lat? '(Jack Sprat could eat no chicken fat)))
(eq? #f (lat? '((Jack) Sprat could eat no chicken fat)))
(eq? #f (lat? '(Jack (Sprat) could eat no chicken fat)))
(eq? #t (lat? '()))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (member? a (cdr lat)))))))

(eq? #t (member? 'tea '(coffee tea or milk)))
(eq? #f (member? 'poached '(fried eggs and scrambled eggs)))


