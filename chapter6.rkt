#lang racket
(require "chapter4.rkt")

(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else (and (numbered? (car aexp))
                 (numbered? (car (cdr (cdr aexp)))))))))

(eq? #t (numbered? 1))
(eq? #t (numbered? '(3 o+ (4 X 5))))
(eq? #f (numbered? '(2 X sausage)))

(define value1
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? 'o+ (car (cdr nexp))) (o+ (value1 (car nexp))
                                      (value1 (car (cdr (cdr nexp))))))
      ((eq? 'X (car (cdr nexp))) (X (value1 (car nexp))
                                    (value1 (car (cdr (cdr nexp))))))
      (else (^ (value1 (car nexp))
               (value1 (car (cdr (cdr nexp)))))))))

(eq? 13 (value1 13))
(eq? 4 (value1 '(1 o+ 3)))
(eq? 82 (value1 '(1 o+ (3 ^ 4))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? 'o+ (operator nexp)) (o+ (value (1st-sub-exp nexp))
                                     (value (2nd-sub-exp nexp))))
      ((eq? 'X (operator nexp)) (X (value (1st-sub-exp nexp))
                                    (value (2nd-sub-exp nexp))))
      (else (^ (value (1st-sub-exp nexp))
               (value (2nd-sub-exp nexp)))))))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define z+
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (z+ n (zub1 m)))))))

(equal? '(() () ()) (z+ '(() ()) '(())))
