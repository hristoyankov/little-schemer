#lang racket
(require "chapter2.rkt")

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))

(equal? (rember 'mint '(lamb chops and mint jelly))
        '(lamb chops and jelly))

(equal? (rember 'toast '(bacon lettuce and tomato))
        '(bacon lettuce and tomato))

(equal? (rember 'cup '(coffee cup tea cup and hick cup))
        '(coffee tea cup and hick cup))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

(equal? (firsts '((apple peach pumpkin)
                  (plum pear cherry)
                  (grape raisin pea)
                  (bean carrot eggplant)))
        '(apple plum grape bean))

(equal? (firsts '((a b)
                  (c d)
                  (e f)))
        '(a c e))

(equal? (firsts '())
        '())

(equal? (firsts '((five plums)
                  (four)
                  (eleven green oranges)))
        '(five four eleven))

(equal? (firsts '(((five plums) four)
                  (eleven green oranges)
                  ((no) more)))
        '((five plums) eleven (no)))


(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old
                                 (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))

(equal? (insertR 'topping 'fudge '(ice cream with fudge for desert))
        '(ice cream with fudge topping for desert))

(equal? (insertR 'jalapeno 'and '(tacos tamales and salsa))
        '(tacos tamales and jalapeno salsa))

(equal? (insertR 'e 'd '(a b c d f g d h))
        '(a b c d e f g d h))


(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))

(equal? (insertL 'topping 'fudge '(ice cream with fudge for desert))
        '(ice cream with topping fudge for desert))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))

(equal? (subst 'topping 'fudge '(ice cream with fudge for desert))
        '(ice cream with topping for desert))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1)
           (eq? (car lat) o2)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))

(equal? (subst2 'vanilla 'chocolate 'banana '(banana ice cream with chocolate topping for desert))
        '(vanilla ice cream with chocolate topping for desert))


(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))

(equal? (multirember 'cup '(coffee cup tea cup and hick cup))
        '(coffee tea and hick))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old
                                 (cons new
                                       (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (mutliinsertL new old (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))
