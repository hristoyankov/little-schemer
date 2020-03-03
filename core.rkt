#lang racket

; Preface
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

; Chapter 2: Do It, Do It Again, and Again, ...
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? (car lat) a) #t)
      (else (member? a (cdr lat))))))

; Chapter 3: Cons the Magnificent
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat)
                  (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l))
                  (firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons old
             (cons new (cdr lat))))
      (else (cons (car lat)
                  (insertR new old
                           (cdr lat)))))))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new lat))
      (else (cons (car lat)
                  (insertL new old
                           (cdr lat)))))))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new (cdr lat)))
      (else (cons (car lat)
                  (subst new old
                         (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1)
           (eq? (car lat) o2))
       (cons new (cdr lat)))
      (else (cons (car lat)
                  (subst2 new o1 o2
                          (cdr lat)))))))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a)
       (multirember a (cdr lat)))
      (else (cons (car lat)
                  (multirember a (cdr lat)))))))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons old
             (cons new
                   (multiinsertR new old
                                 (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertR new old
                                (cdr lat)))))))

(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new
             (cons old
                   (multiinsertL new old
                                 (cdr lat)))))
      (else (cons (car lat)
                  (multiinsertL new old
                                (cdr lat)))))))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old)
       (cons new
             (multisubst new old
                         (cdr lat))))
      (else (cons (car lat)
                  (multisubst new old
                              (cdr lat)))))))

; Chapter 4: Numbers Games
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (sub1 (o- n (sub1 m)))))))

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup)
                (addtup (cdr tup)))))))

(define x
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (x n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1)
                      (car tup2))
                  (tup+ (cdr tup1)
                        (cdr tup2)))))))

(define o>
  (lambda (n m)
    (cond
      ((zero? n) #f)
      ((zero? m) #t)
      (else (o> (sub1 n) (sub1 m))))))

(define o<
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (o< (sub1 n) (sub1 m))))))

(define o=
  (lambda (n m)
    (cond
      ((or (o< n m) (o> n m)) #f)
      (else #t))))

(define pow
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (x n (pow n (sub1 m)))))))

(define quot
  (lambda (n m)
    (cond
      ((< n m) 0)
      (else (add1 (quot (o- n m) m))))))

(define length
  (lambda (lat)
    (cond
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

(define rempick-obsolete
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (cdr lat))
      (else (cons (car lat)
             (rempick-obsolete (sub1 n)
                      (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat))
       (no-nums (cdr lat)))
      (else (cons (car lat)
                  (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat))
       (cons (car lat)
             (all-nums (cdr lat))))
      (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (x y)
    (cond
      ((and (number? x) (number? y))
       (o= x y))
      ((or (number? x) (number? y))
       #f)
      (else (eq? x y)))))

(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? (car lat) a)
       (add1 (occur a (cdr lat))))
      (else
       (occur a (cdr lat))))))

(define one?
  (lambda (a)
    (= a 1)))

(define rempick
  (lambda (n lat)
    (cond
      ((one? n) (cdr lat))
      (else (cons (car lat)
             (rempick (sub1 n)
                      (cdr lat)))))))

; Chapter 5: Oh My Gawd *: It's Full of Stars
(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (first l))
       (cond
         ((eq? (car l) a) (rember* a (cdr l)))
         (else (cons (car l) (rember* a (cdr l))))))
      (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons old (cons new (insertR* new old (cdr l)))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else
       (cons (insertR* new old (car l))
             (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else
       (o+ (occur* a (car l))
           (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (subst* new old (cdr l))))
         (else
          (cons (car l) (subst* new old (cdr l))))))
      (else
       (cons (subst* new old (car l))
             (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old)
          (cons new (cons old (insertL* new old (cdr l)))))
         (else
          (cons (car l) (insertL* new old (cdr l))))))
      (else
       (cons (insertL* new old (car l))
             (insertL* new old (cdr l)))))))

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

(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
    (cond
      ((and (null? l1) (null? l2)) #t)
      ((or (null? l1) (null? l2)) #f)
      (else (and (equal? (car l1) (car l2))
                 (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2))
       (eqan? s1 s2))
      ((or (atom? s1) (atom? s2))
       #f)
      (else (eqlist? s1 s2)))))

(define rember2
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (cdr l))
      (else (cons (car l)
                  (rember s (cdr l)))))))

; Chapter 6: Shadows
(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
       (and (numbered? (car aexp))
            (numbered? (car (cdr (cdr aexp)))))))))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) '+)
       (o+ (value (1st-sub-exp nexp))
           (value (2nd-sub-exp nexp))))
      ((eq? (operator nexp) 'x)
       (x (value (1st-sub-exp nexp))
          (value (2nd-sub-exp nexp))))
      (else (pow (value (1st-sub-exp nexp))
                 (value (2nd-sub-exp nexp)))))))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp)
    (car aexp)))

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define ol+
  (lambda (n m)
    (cond
      ((sero? m) n)
      (else (edd1 (ol+ n (zub1 m)))))))

; Chapter 7: Friends and Relations
(define member2?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((equal? (car lat) a) #t)
      (else (member2? a (cdr lat))))))

(define set?
  (lambda (lat)
    (cond
      ((null? lat) #t)
      ((member2? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))

(define mkset
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((member? (car lat) (cdr lat))
       (mkset (cdr lat)))
      (else
       (cons (car lat) (mkset (cdr lat)))))))

(define makeset
  (lambda (lat)
    (cond
      ((null? lat) '())
      (else
       (cons (car lat)
             (makeset (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
      ((null? set1) #t)
      (else (and (member? (car set1) set2)
                 (subset? (cdr set1) set2))))))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2)
         (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
    (cond
      ((null? set1) #f)
      (else (or (member? (car set1) set2)
                (intersect? (cdr set1) set2))))))

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2)))
      (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2)
       (union (cdr set1) set2))
      (else (cons (car set1)
                  (union (cdr set1) set2))))))

(define difference
  (lambda (set1 set2)
    (cond
      ((null? set1) '())
      ((member? (car set1) set2)
       (difference (cdr set1) set2))
      (else (cons (car set1)
                  (difference (cdr set1) set2))))))

(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set)
             (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cdr (cdr x))) #t)
      (else #f))))

(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (car (cdr p))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (revpair (car rel))
                  (revrel (cdr rel)))))))

(define revpair
  (lambda (p)
    (build (second p) (first p))))

(define fullfun?
  (lambda (rel)
    (set? (seconds rel))))

(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (second (car l))
                  (seconds (cdr l)))))))

(define one-to-one
  (lambda (rel)
    (fun? (revrel rel))))

; Chapter 8: Lambda the Ultimate
(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
        ((null? l) '())
        ((test? (car l) a) (cdr l))
        (else (cons (car l) ((rember-f test?) a (cdr l))))))))

(define eq?-c
  (lambda (a)
    (lambda (x)
      (eq? x a))))

(define eq?-salad (eq?-c 'salad))

(define rember-eq? (rember-f eq?))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old)
         (cons new (cons old (cdr l))))
        (else (cons (car l)
                    ((insertL-f test?) new old (cdr l))))))))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((test? (car l) old)
         (cons old (cons new (cdr l))))
        (else (cons (car l)
                    ((insertR-f test?) new old l)))))))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old)
         (seq new old (cdr l)))
        (else (cons (car l)
                    ((insert-g seq) new old (cdr l))))))))

(define insertL2
  (insert-g
   (lambda (new old l)
     (cons new (cons old l)))))

(define insertR2
  (insert-g
   (lambda (new old l)
     (cons old (cons new l)))))

(define subst3
  (insert-g
   (lambda (new old l)
     (cons new l))))

(define atom-to-function
  (lambda (a)
    (cond
      ((eq? a '+) o+)
      ((eq? a 'x) x)
      (else pow))))

(define value2
  (lambda (x)
    (cond
      ((atom? x) x)
      (else
       ((atom-to-function (operator x))
        (value (1st-sub-exp x))
        (value (2nd-sub-exp x)))))))

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a)
         ((multirember-f test?) a (cdr lat)))
        (else
         (cons (car lat)
               ((multirember-f test?) a (cdr lat))))))))

(define multirember-eq?
  (multirember-f eq?))
