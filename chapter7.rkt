#lang racket
(require "preface.rkt")
(require "chapter2.rkt")
(require "chapter3.rkt")

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(eq? #f (set? '(apple peaches apple plum)))
(eq? #t (set? '(apple peaches pears plum)))
(eq? #t (set? '()))
(eq? #f (set? '(apple 3 pear 4 apple 3)))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else (cons (car lat)
                  (makeset (multirember (car lat) (cdr lat))))))))

(equal? '(apple peach pear plum lemon)
        (makeset '(apple peach pear peach plum apple lemon peach)))


(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2)
                 (subset? (cdr set1) set2))))))

(eq? #t (subset? '(5 chicken wings)
                 '(5 hamburgers 2 pieces fried chicken and light duckling wings)))
(eq? #f (subset? '(4 pounds of horseradish)
                 '(four pounds chicken and 5 ounces horseradish)))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(eq? #t (eqset? '(6 large chickens with wings)
                '(6 chickens with large wings)))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set2) #f)
      (else (or (member? (car set2) set1)
                (intersect? set1 (cdr set2)))))))

(eq? #t (intersect? '(stewed tomatoes and macaroni)
                    '(macaroni and cheese)))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(equal? '(and macaroni) (intersect '(stewed tomatoes and macaroni)
                                   '(macaroni and cheese)))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) (union (cdr set1) set2))
      (else (cons (car set1) (union (cdr set1) set2))))))

(equal? '(stewed tomatoes casserole macaroni and cheese)
        (union '(stewed tomatoes and macaroni casserole)
               '(macaroni and cheese)))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1)
                  (difference (cdr set1) set2))))))

(define intersectall
  (lambda (lset)
    (cond
      ((null? (cdr lset)) (car lset))
      (else (intersect (car lset)
                       (intersectall (cdr lset)))))))

(equal? '(a) (intersectall '((a b c) (c a d e) (e f g h a b))))
(equal? '(6 and) (intersectall '((6 pears and)
                                 (3 peaches and 6 peppers)
                                 (8 pears and 6 plums)
                                 (and 6 prunes with some apples))))


(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(eq? #t (a-pair? '(pear pear)))
(eq? #t (a-pair? '(3 6)))
(eq? #t (a-pair? '((2) (pair))))
(eq? #t (a-pair? '(full (house))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (p)
    (car (cdr (cdr p)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(eq? #t (fun? '((8 3) (4 2) (7 6) (6 2) (3 4))))
(eq? #f (fun? '((d 4) (b 0) (b 9) (e 5) (g 4))))

(define revrel1
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (build (second (car rel))
                         (first (car rel)))
                  (revrel1 (cdr rel)))))))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel))
                  (revrel (cdr rel)))))))

(equal? '((a 8) (pie pumpkin) (sick got))
        (revrel '((8 a) (pumpkin pie) (got sick))))

(define seconds
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (second (car rel))
                  (seconds (cdr rel)))))))

(define fullfun?
  (lambda (fun)
    (set? (seconds fun))))

(eq? #f (fullfun? '((8 3) (4 2) (7 6) (6 2) (3 4))))
(eq? #t (fullfun? '((8 3) (4 8) (7 6) (6 2) (3 4))))
(eq? #f (fullfun? '((grape raisin)
                    (plum prune)
                    (stewed prune))))
(eq? #t (fullfun? '((grape raisin)
                    (plum prune)
                    (stewed grape))))
