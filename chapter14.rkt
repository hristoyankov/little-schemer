#lang racket
(require "preface.rkt")

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
       (let ((D1 (add1 (depth* (car l))))
             (D2 (depth* (cdr l))))
         (cond
           ((> D1 D2) D1)
           (else D2)))))))

(eq? 2 (depth* '((pickled) peppers (peppers pickled))))
(eq? 4 (depth* '(margarine
                 ((bitter butter)
                  (makes)
                  (batter (bitter)))
                 butter)))
(eq? 3 (depth* '(c (b (a b) a) a)))


