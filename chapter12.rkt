#lang racket
(require "preface.rkt")
(require "chapter4.rkt")

(define multirember
  (lambda (a lat)
    (letrec
        ((mr (lambda (lat)
               (cond
                 ((null? lat) '())
                 ((eq? (car lat) a) (mr (cdr lat)))
                 (else (cons (car lat) (mr (cdr lat))))))))
      (mr lat))))

(define multirember-f
  (lambda (test?)
    (letrec
        ((m-f (lambda (a lat)
                (cond
                  ((null? lat) '())
                  ((test? (car lat) a) (m-f (cdr lat)))
                  (else (cons (car lat) (m-f (cdr lat))))))))
      m-f)))

(define union
  (lambda (set1 set2)
    (letrec
        ((U (lambda (set1)
              (cond
                ((null? set1) set2)
                ((M? (car set1) set2) (U (cdr set1) set2))
                (else (cons (car set1) (U (cdr set1) set2))))))
         (M? (lambda (a lat)
               (letrec
                   ((N? (lambda (lat)
                          (cond
                            ((null? lat) #f)
                            ((eq? (car lat) a) #t)
                            (else (N? (cdr lat)))))))
                 (N? lat)))))
      (U set1))))

(define two-in-a-row?
  (letrec
      ((T? (lambda (previous lat)
             (cond
               ((null? lat) #f)
               ((eq? (car lat) previous) #t)
               (else (T? (car lat) (cdr lat)))))))
    (lambda (lat)
      (cond
        ((null? lat) #f)
        (else (T? (car lat) (cdr lat)))))))

(define sum-of-prefixes
  (letrec
      ((S (lambda (sonssf tup)
            (cond
              ((null? tup) '())
              (else (cons (+ sonssf (car tup))
                          (S (+ sonssf (car tup)) (cdr tup))))))))
    (lambda (tup)
      (S 0 tup))))

(define scramble
  (letrec
      ((P (lambda (tup prefix)
            (cond
              ((null? tup) '())
              (else (cons (pick (car tup) (cons (car tup) prefix))
                          (P (cdr tup) (cons (car tup) prefix))))))))
    (lambda (tup)
      (P tup '()))))
