#lang racket
;(require rnrs/mutable-pairs-6)
(require "preface.rkt")

(define kounter '())
(define set-kounter '())

(define bons
  (lambda (kar)
    (let ((kdr '()))
      (lambda (selector)
        (selector
         (lambda (x) (set! kdr x))
         kar
         kdr)))))

(define kar
  (lambda (c)
    (c (lambda (s a d) a))))

(define kdr
  (lambda (c)
    (c (lambda (s a d) d))))

(define set-kdr
  (lambda (c x)
    ((c (lambda (s a d) s)) x)))

(define kons
  (lambda (kar kdr)
    (let ((c (bons kar)))
      (set-kdr c kdr)
      c)))

(define konsC
  (let ((N 0))
    (set! kounter
          (lambda () N))
    (set! set-kounter
          (lambda (x)
            (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (kons x y))))

;; (define kons mcons)
;; (define kdr mcdr)
;; (define set-kdr set-mcdr!)
;; (define kar mcar)

(define lots
  (lambda (m)
    (cond
      ((zero? m) '())
      (else (kons 'egg (lots (sub1 m)))))))

(define lenkth
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (lenkth (kdr l)))))))

(define add-at-end
  (lambda (l)
    (cond
      ((null? (kdr l))
       (konsC (kar l)
              (kons 'egg '())))
      (else (konsC (kar l)
                   (add-at-end (kdr l)))))))

(define add-at-end-too
  (lambda (l)
    (letrec
        ((A (lambda (ls)
              (cond
                ((null? (kdr ls))
                 (set-kdr ls (kons 'egg '())))
                (else (A (kdr ls)))))))
      (A l)
      l)))

(kounter)
(define dozen (lots 12))
(define bakers-dozen (add-at-end dozen))
(define bakers-dozen-too (add-at-end-too dozen))
(define bakers-dozen-again (add-at-end dozen))

(define eklist?
  (lambda (l1 l2)
    (cond
      ((null? l1) (null? l2))
      ((null? l2) #f)
      (else (and (eq? (kar l1) (kar l2))
                 (eklist? (kdr l1) (kdr l2)))))))

(define same?
  (lambda (c1 c2)
    (let ((t1 (kdr c1))
          (t2 (kdr c2)))
      (set-kdr c1 1)
      (set-kdr c2 2)
      (let ((v (= (kdr c1) (kdr c2))))
        (set-kdr c1 t1)
        (set-kdr c2 t2)
        v))))

(define last-kons
  (lambda (ls)
    (cond
      ((null? (kdr ls)) ls)
      (else (last-kons (kdr ls))))))

(define long (lots 12))
