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


