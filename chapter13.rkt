#lang racket
(require "preface.rkt")

(define intersect
  (lambda (set1 set2)
    (letrec
        ((I (lambda (set)
              (cond
                ((null? set) '())
                ((M? (car set) set2) (cons (car set) (I (cdr set))))
                (else (I (cdr set))))))
         (M? (lambda (a lat)
               (letrec
                   ((N? (lambda (lat)
                          (cond
                            ((null? lat) #f)
                            ((eq? (car lat) a) #t)
                            (else (N? (cdr lat)))))))
                 (N? lat)))))
      (I set1))))

(define intersectall
  (lambda (lset)
    (let/cc hop
           (letrec
               ((A (lambda (lset)
                     (cond
                       ((null? (car lset)) (hop '()))
                       ((null? (cdr lset)) (car lset))
                       (else (I (car lset) (A (cdr lset)))))))
                (I (lambda (s1 s2)
                     (letrec
                         ((J (lambda (s1)
                               (cond
                                 ((null? s1) '())
                                 ((M? (car s1) s2) (cons (car s1) (I (cdr s1) s2)))
                                 (else (I (cdr s1) s2)))))
                          (M? (lambda (a lat)
                                (letrec
                                    ((N? (lambda (lat)
                                           (cond
                                             ((null? lat) #f)
                                             ((eq? (car lat) a) #t)
                                             (else (N? (cdr lat)))))))
                                  (N? lat)))))
                       (cond
                         ((null? s2) (hop '()))
                         (else (J s1)))))))
             (cond
               ((null? lset) '())
               (else (A lset)))))))

(equal? '(3) (intersectall '((3 mangos and)
                             (3 kiwis and)
                             (3 hamburgers))))
(equal? '() (intersectall '((3 steaks and)
                            (no food and)
                            (three bakes potatoes)
                            (3 diet hamburgers))))
(equal? '() (intersectall '((3 mangoes and)
                            ()
                            (3 hamburgers))))


(define rember
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? (car lat) a) (cdr lat))
                (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))

(define rember-beyond-first
  (lambda (a lat)
    (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? (car lat) a) '())
                (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))

(equal? '(noodles spaghetti spatzle bean-thread)
        (rember-beyond-first 'roots '(noodles spaghetti spatzle bean-thread roots potatoes yam others rice)))
(equal? '(noodles spaghetti spatzle bean-thread roots potatoes yam)
        (rember-beyond-first 'others '(noodles spaghetti spatzle bean-thread roots potatoes yam others rice)))
(equal? '(noodles spaghetti spatzle bean-thread roots potatoes yam others rice)
        (rember-beyond-first 'sweeties '(noodles spaghetti spatzle bean-thread roots potatoes yam others rice)))
(equal? '(cookies chocolate mints
                  caramel delight ginger snaps)
        (rember-beyond-first 'deserts '(cookies chocolate mints
                                                caramel delight ginger snaps
                                                deserts chocolate mousse
                                                vanilla ice cream
                                                German chocolate cake
                                                more desserts
                                                gingerbreadman chocolate
                                                chip brownies)))

(define rember-upto-last
  (lambda (a lat)
    (let/cc skip
      (letrec
          ((R (lambda (lat)
                (cond
                  ((null? lat) '())
                  ((eq? (car lat) a) (skip (R (cdr lat))))
                  (else (cons (car lat) (R (cdr lat))))))))
        (R lat)))))

(equal? '(potatoes yam others rice)
        (rember-upto-last 'roots '(noodles spaghetti spatzle bean-thread roots potatoes yam others rice)))
(equal? '(noodles spaghetti spatzle bean-thread roots potatoes yam others rice)
        (rember-upto-last 'sweeties '(noodles spaghetti spatzle bean-thread roots potatoes yam others rice)))
(equal? '(gingerbreadman chocolate chip brownies)
        (rember-upto-last 'cookies '(cookies
                                     chocolate mints
                                     caramel delight ginger snaps
                                     deserts
                                     chocolate mousse
                                     vanilla ice cream
                                     German chocolate cake
                                     more cookies
                                     gingerbreadman chocolate
                                     chip brownies)))
