#lang racket

(define last 'angelfood)

(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food
          (cons 'cake '()))))

(define ingredients '())

(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (set! last food)
    (cons food
          (cons 'cake '()))))


