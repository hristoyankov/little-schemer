#lang racket
(require "preface.rkt")
(require "chapter4.rkt")

(define leftmost
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else
       (let ((a (leftmost (car l))))
         (cond
           ((atom? a) a)
           (else (leftmost (cdr l)))))))))

(eq? 'a (leftmost '(((a) b) (c d))))
(eq? 'a (leftmost '(((a) ()) () (e))))
(eq? 'a (leftmost '(((() a) ()))))

(define rember1*
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) '())
                ((atom? (car l))
                 (cond
                   ((eq? (car l) a) (cdr l))
                   (else (cons (car l) (R (cdr l))))))
                (else
                 (let ((av (R (car l))))
                   (cond
                     ((equal? (car l) av)
                      (cons (car l) (R (cdr l))))
                     (else (cons av (cdr l))))))))))
      (R l))))

(equal? '((Swedish rye)
          (French (mustard turkey))
          salad)
        (rember1* 'salad '((Swedish rye)
                           (French (mustard salad turkey))
                           salad)))
(equal? '((Swedish rye)
          (French (mustard turkey)))
        (rember1* 'salad '((Swedish rye)
                           (French (mustard turkey))
                           salad)))
(equal? '((pasta)
          pasta
          (noodles meat sauce)
          meat tomatoes)
        (rember1* 'meat '((pasta meat)
                         pasta
                         (noodles meat sauce)
                         meat tomatoes)))

(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth* (cdr l)))
      (else
       (letrec ((max (lambda (n m)
                       (if (> n m) n m))))
         (max (add1 (depth* (car l))) (depth* (cdr l))))))))

(eq? 2 (depth* '((pickled) peppers (peppers pickled))))
(eq? 4 (depth* '(margarine
                 ((bitter butter)
                  (makes)
                  (batter (bitter)))
                 butter)))
(eq? 3 (depth* '(c (b (a b) a) a)))

(define scramble
  (letrec
      ((P (lambda (tup prefix)
            (cond
              ((null? tup) '())
              (else
               (let ((rp (cons (car tup) prefix)))
                 (cons (pick (car tup) rp)
                       (P (cdr tup) rp))))))))
    (lambda (tup)
      (P tup '()))))

(define leftmost2
  (let/cc skip
    (letrec ((lm (lambda (l)
                   (cond
                     ((null? l) '())
                     ((atom? (car l)) (skip (car l)))
                     (else (let ()
                             (lm (car l))
                             (lm (cdr l))))))))
      (lambda (l)
        (lm l)))))

(define-syntax try
  (syntax-rules ()
    ((try var a . b)
     (let/cc success
            (let/cc var (success a)) . b))))

(define rember1*2
  (lambda (a l)
    (letrec ((rm (lambda (l oh)
                   (cond
                     ((null? l) (oh 'no))
                     ((atom? (car l))
                      (if (eq? (car l) a)
                          (cdr l)
                          (cons (car l) (rm (cdr l) oh))))
                     (else
                      (try oh2
                           (cons (rm (car l) oh2)
                                 (cdr l))
                           (cons (car l)
                                 (rm (cdr l) oh))))))))
      (try oh (rm l oh) l))))

(equal? '((Swedish rye)
          (French (mustard turkey))
          salad)
        (rember1*2 'salad '((Swedish rye)
                            (French (mustard salad turkey))
                            salad)))
