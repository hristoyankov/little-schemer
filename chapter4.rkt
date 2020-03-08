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
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1)
                      (car tup2))
                  (tup+ (cdr tup1)
                        (cdr tup2)))))))

(equal? (tup+ '(3 6) '(2 3)) '(5 9))
(equal? (tup+ '(8 4 5) '(3 7 6)) '(11 11 11))
(equal? (tup+ '(4 6 8 1) '(3 7)) '(7 13 8 1))
(equal? (tup+ '(3 7) '(4 6 8 1)) '(7 13 8 1))

(define >
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (> (sub1 n) (sub1 m))))))

(eq? #f (> 12 133))
(eq? #t (> 120 11))

(define <
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (< (sub1 n) (sub1 m))))))

(eq? #t (< 4 6))
(eq? #f (< 8 3))
(eq? #f (< 6 6))

(define o=
  (lambda (n m)
    (cond
      ((or (> n m) (< n m)) #f)
      (else #t))))

(eq? #t (o= 3 3))
(eq? #f (o= 4 3))

(define ^
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (X n (^ n (sub1 m)))))))

(= 1 (^ 1 1))
(= 8 (^ 2 3))
(= 125 (^ 5 3))

(define %
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (% (o- n m) m))))))

(= 3 (% 15 4))

(define len
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (len (cdr lat)))))))

(= 6 (len '(hotdogs with mustard sauerkraut and pickels)))
(= 5 (len '(ham and cheese on rye)))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(eq? (pick 4 '(lasagna spaghetti ravioli macaroni meatball)) 'macaroni)

(define rempick-old
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat)
                  (rempick-old (sub1 n) (cdr lat)))))))

(equal? (rempick-old 3 '(hotdogs with hot mustard)) '(hotdogs with mustard))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat)
                  (no-nums (cdr lat)))))))

(equal? (no-nums '(5 pears 6 prunes 9 dates)) '(pears prunes dates))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (cons (car lat)
                                 (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(equal? (all-nums '(5 pears 6 prunes 9 dates)) '(5 6 9))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (o= a1 a2))
      ((or (number? a1) (number? a2)) #f)
      (else (eq? a1 a2)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eq? (car lat) a) (add1 (occur (cdr lat))))
      (else (occur (cdr lat))))))

(define one?
  (lambda (n)
    (o= n 1)))

(eq? #t (one? 1))
(eq? #f (one? 11))

(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat)
                  (rempick (sub1 n) (cdr lat)))))))

(equal? (rempick 3 '(hotdogs with hot mustard)) '(hotdogs with mustard))
