#lang racket
(require "chapter2.rkt")

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))

(equal? (rember 'mint '(lamb chops and mint jelly))
        '(lamb chops and jelly))

(equal? (rember 'toast '(bacon lettuce and tomato))
        '(bacon lettuce and tomato))

(equal? (rember 'cup '(coffee cup tea cup and hick cup))
        '(coffee tea cup and hick cup))
