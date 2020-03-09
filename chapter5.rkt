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

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else
       (cons (subst* new old (car l))
             (subst* new old (cdr l)))))))

(equal? (subst* 'orange 'banana '((banana)
                                  (split ((((banana ice)))
                                          (cream (banana))
                                          sherbet))
                                  (banana)
                                  (bread)
                                  (banana brandy)))
        '((orange)
          (split ((((orange ice)))
                  (cream (orange))
                  sherbet))
          (orange)
          (bread)
          (orange brandy)))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else
       (cons (insertL* new old (car l))
             (insertL* new old (cdr l)))))))

(equal? (insertL* 'pecker 'chuck '((how much (wood))
                                  could
                                  ((a (wood) chuck))
                                  (((chuck)))
                                  (if (a) ((wood chuck)))
                                  could chuck wood))
        '((how much (wood))
          could
          ((a (wood) pecker chuck))
          (((pecker chuck)))
          (if (a) ((wood pecker chuck)))
          could pecker chuck wood))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a)
           (member* a (cdr l))))
      (else
       (or (member* a (car l))
           (member* a (cdr l)))))))

(eq? #t (member* 'chips '((potato) (chips ((with) fish) (chips)))))

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l))
       (car l))
      (else (leftmost (car l))))))

(eq? (leftmost '((potato) (chips ((with) fish) (chips))))
     'potato)
(eq? (leftmost '(((hot) (tuna (and))) cheese))
     'hot)

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      ((and (atom? (car l1))
            (atom? (car l2)))
       (and (eqan? (car l1) (car l2))
            (eqlist? (cdr l1) (cdr l2))))
      ((or (atom? (car l1))
           (atom? (car l2)))
       #f)
      (else (and (and (eqlist? (car l1) (car l2)))
                 (and (eqlist? (cdr l1) (cdr l2))))))))

(eq? #t (eqlist? '(strawberry ice cream)
                 '(strawberry ice cream)))
(eq? #f (eqlist? '(strawberry ice cream)
                 '(strawberry cream ice)))

(define oequal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1) (atom? s2))
       #f)
      (else (eqlist? s1 s2)))))

(define rember
  (lambda (s l)
    (cond
      ((null? l) '())
      ((oequal? (car l) s) (cdr l))
      (else (cons (car l)
                  (rember (cdr l)))))))
