#lang racket
(require "preface.rkt")

(define deep
  (lambda (m)
    (cond
      ((zero? m) 'pizza)
      (else (cons (deep (sub1 m)) '())))))

(define toppings '())

(define deepB
  (lambda (m)
    (cond
      ((zero? m)
       (let/cc jump
         (set! toppings jump)
         'pizza))
      (else (cons (deepB (sub1 m)) '())))))

(define deep&co
  (lambda (m k)
    (cond
      ((zero? m)
       (let ()
         (set! toppings k)
         (k 'pizza)))
      (else (deep&co (sub1 m) (lambda (x)
                                (k (cons x '()))))))))

(define two-in-a-row?
  (letrec
      ((W (lambda (a lat)
            (cond
              ((null? lat) #f)
              (else
               (let ((nxt (car lat)))
                 (or (eq? nxt a)
                     (W nxt (cdr lat)))))))))
    (lambda (lat)
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))

(define leave '())

(define walk
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (let ()
         (leave (car l))
         (walk (cdr l))))
      (else (let ()
              (walk (car l))
              (walk (cdr l)))))))

(define start-it
  (lambda (l)
    (let/cc here
      (set! leave here)
      (walk l))))

(define fill '())

(define waddle
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (let ()
         (let/cc rest
           (set! fill rest)
           (leave (car l)))
         (waddle (cdr l))))
      (else (let ()
              (waddle (car l))
              (waddle (cdr l)))))))

(define start-it2
  (lambda (l)
    (let/cc here
      (set! leave here)
      (waddle l))))

(define get-first
  (lambda (l)
    (let/cc here
      (set! leave here)
      (waddle l)
      (leave '()))))

(define get-next
  (lambda (x)
    (let/cc home-again
      (set! leave home-again)
      (fill 'go))))


(define two-in-a-row-b*?
  (lambda (a)
    (let ((n (get-next 'go)))
      (if (atom? n)
          (or (eq? n a)
              (two-in-a-row-b*? n))
          #f))))

(define two-in-a-row*?
  (lambda (l)
    (let ((fst (get-first l)))
      (if (atom? fst)
          (two-in-a-row-b*? fst)
          #f))))

(eq? #f (two-in-a-row*? '((mozzarella) (cake) mozzarella)))
(eq? #t (two-in-a-row*? '((potato) chips ((with) fish) (fish))))
(eq? #f (two-in-a-row*? '((potato) chips ((with) fish) (chips))))
(eq? #t (two-in-a-row*? '((potato) chips ((chips) with) (fish))))
