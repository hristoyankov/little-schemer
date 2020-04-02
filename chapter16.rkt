#lang racket
(require "preface.rkt")

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

(define deep
  (lambda (m)
    (cond
      ((zero? m) 'pizza)
      (else (cons (deep (sub1 m)) '())))))

(equal? '(((pizza))) (deep 3))
(equal? '(((((((pizza))))))) (deep 7))
(equal? 'pizza (deep 0))

(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond
                ((null? ns) #f)
                ((eq? (car ns) n) (car rs))
                (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

(define deepM
  (let ((Ns '())
        (Rs '()))
    (lambda (m)
      (let ((exists (find m Ns Rs)))
        (if (atom? exists)
            (let ((result (deep m)))
              (set! Ns (cons m Ns))
              (set! Rs (cons result Rs))
              result)
            exists)))))

(define L
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))

(define Y-bang
  (lambda (L)
    (letrec ((h (L (lambda (arg) (h arg)))))
      h)))

(define biz
  (let ((x 0))
    (lambda (f)
      (set! x (add1 x))
      (lambda (a)
        (if (= a x)
            0
            (f a))))))
