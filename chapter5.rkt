#lang racket
(require "chapter4.rkt")

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l))
                  (rember* a (cdr l)))))))

(equal? (rember* 'cup '((coffee) cup ((tea cup)) (and (hick)) cup))
        '((coffee) ((tea)) (and (hick))))

(equal? (rember* 'sauce '(((tomato sauce))
                          ((bean) sauce)
                          (and ((flying)) sauce)))
        '(((tomato))
          ((bean))
          (and ((flying)))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l))
                  (insertR* new old (cdr l)))))))

(equal? (insertR* 'roast 'chuck '((how much (wood))
                                  could
                                  ((a (wood) chuck))
                                  (((chuck)))
                                  (if (a) ((wood chuck)))
                                  could chuck wood))
        '((how much (wood))
          could
          ((a (wood) chuck roast))
          (((chuck roast)))
          (if (a) ((wood chuck roast)))
          could chuck roast wood))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a)
          (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (o+ (occur* a (car l))
                (occur* a (cdr l)))))))

(= 5 (occur* 'banana '((banana)
                       (split ((((banana ice)))
                               (cream (banana))
                               sherbet))
                       (banana)
                       (bread)
                       (banana brandy))))
