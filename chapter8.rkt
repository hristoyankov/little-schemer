#lang racket
(require "preface.rkt")
(require "chapter4.rkt")
(require "chapter6.rkt")

(define rember-f1
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? a (car l)) (cdr l))
      (else (cons (car l) (rember-f1 test? a (cdr l)))))))

(equal? '(6 2 3) (rember-f1 o= 5 '(6 2 5 3)))
(equal? '(beans are good) (rember-f1 eq? 'jelly '(jelly beans are good)))
(equal? '(lemonade and (cake)) (rember-f1 equal? '(pop corn) '(lemonade (pop corn) and (cake))))

(define rember
  (lambda (a l)
    (rember-f1 equal? a l)))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? a x))))

(define eq?-salad
  (eq?-c 'salad))

(eq? #t (eq?-salad 'salad))
(eq? #f (eq?-salad 'tuna))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? a (car l)) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define rember-eq?
  (rember-f eq?))

(equal? '(salad is good) (rember-eq? 'tuna '(tuna salad is good)))


(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old) (cons new l))
        (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old) (cons old
                                 (cons new (cdr l))))
        (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((equal? (car l) old) (seq new old (cdr l)))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

(define insertL (insert-g (lambda (new old l)
                            (cons new (cons old l)))))
(define insertR (insert-g (lambda (new old l)
                            (cons old (cons new l)))))

(define subst (insert-g (lambda (new old l)
                          (cons new l))))

(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x 'o+) o+)
      ((eq? x 'X) X)
      (else ^))))

(define value
  (lambda (nexp)
    (cond
      ((number? nexp) nexp)
      (else ((atom-to-function (operator nexp))
             (value (1st-sub-exp nexp))
             (value (2nd-sub-exp nexp)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a) ((multirember-f test?) a (cdr l)))
        (else (cons (car l) ((multirember-f test?) a (cdr l))))))))

(equal? '(shrimp salad salad and)
        ((multirember-f eq?) 'tuna '(shrimp salad tuna salad and tuna)))

(define mutlirember-eq?
  (multirember-f eq?))

(define eq?-tuna
  (lambda (x)
    (eq? x 'tuna)))

(define multiremberT
  (lambda (test? lat)
    (cond
      ((null? lat) '())
      ((test? (car lat)) (multiremberT test? (cdr lat)))
      (else (cons (car lat) (multiremberT test? (cdr lat)))))))

(equal? '(shrimp salad salad and)
        (multiremberT eq?-tuna '(shrimp salad tuna salad and tuna)))

(define multiinsertLR
  (lambda (new oldL oldR l)
    (cond
      ((null? l) '())
      ((eq? (car l) oldL) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr l)))))
      ((eq? (car l) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr l)))))
      (else (cons (car l) (multiinsertLR new oldL oldR (cdr l)))))))

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
      ((null? lat) (col '() 0 0))
      ((eq? (car lat) oldL)
       (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat L R)
                                                   (col (cons new (cons oldL newlat))
                                                        (add1 L)
                                                        R))))
      ((eq? (car lat) oldR)
       (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat L R)
                                                   (col (cons oldR (cons new newlat))
                                                        L
                                                        (add1 R)))))
      (else (multiinsertLR&co new oldL oldR (cdr lat) (lambda (newlat L R)
                                                        (col (cons (car lat) newlat)
                                                             L
                                                             R)))))))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((and (number? (car l))
            (even? (car l)))
       (cons (car l) (evens-only* (cdr l))))
      ((number? (car l)) (evens-only* (cdr l)))
      (else (cons (evens-only* (car l))
                  (evens-only* (cdr l)))))))

(equal? '((2 8) 10 (() 6) 2)
        (evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2)))

(define evens-only*&co
  (lambda (l col)
    (cond
      ((null? l) (col '() 1 0))
      ((and (number? (car l))
            (even? (car l)))
       (evens-only*&co (cdr l) (lambda (newl p s)
                                 (col (cons (car l) newl)
                                      (X (car l) p)
                                      s))))
      ((number? (car l)) (evens-only*&co (cdr l) (lambda (newl p s)
                                                   (col newl
                                                        p
                                                        (o+ (car l) s)))))
      (else (evens-only*&co (car l) (lambda (al ap as)
                                      (evens-only*&co (cdr l) (lambda (dl dp ds)
                                                                (col (cons al dl)
                                                                     (X ap dp)
                                                                     (o+ as ds))))))))))


(equal? '(1920 38 (2 8) 10 (() 6) 2)
        (evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) (lambda (l p s)
                                                          (cons p (cons s l)))))

