#lang racket
(require "preface.rkt")

(define x
  (cons 'chicago
        (cons 'pizza
              '())))

(set! x 'gone)
(set! x 'skins)

(define gourmet
  (lambda (food)
    (cons food
          (cons x '()))))

;;(gourmet 'onion)
(set! x 'rings)

(define gourmand
  (lambda (food)
    (set! x food)
    (cons food (cons x '()))))

(define dinerR
  (lambda (food)
    (set! x food)
    (cons 'milkshake
          (cons food '()))))

(define omnivore
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

(define food 'none)

(define chez-nous
  (let ((y 'cabbage))
    (lambda ()
      (set! y food)
      (set! food x)
      (set! x y))))

(chez-nous)
