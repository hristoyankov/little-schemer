#lang racket
(require "preface.rkt")
(require "chapter4.rkt")
(require "chapter7.rkt")

(define pick
  (lambda (n lat)
    (car lat)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (eq? a sorn)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

;;(eq? #t (looking 'caviar '(6 2 4 caviar 5 7 3)))
;;(eq? #f (looking 'caviar '(6 2 grit caviar 5 7 3)))

(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair)) (second pair)))))

(equal? '(a (b c)) (shift '((a b) c)))
(equal? '(a (b (c d))) (shift '((a b) (c d))))

(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (align (shift pora)))
      (else (build (first pora)
                   (align (second pora)))))))

(define length*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (o+ (length* (first pora))
                (length* (second pora)))))))

(define weight*
  (lambda (pora)
    (cond
      ((atom? pora) 1)
      (else (o+ (X (weight* (first pora)) 2)
                (weight* (second pora)))))))

(eq? 7 (weight* '((a b) c)))
(eq? 5 (weight* '(a (b c))))

(define shuffle
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora)) (shuffle (revpair pora)))
      (else (build (first pora)
                   (shuffle (second pora)))))))

(equal? '(a (b c)) (shuffle '(a (b c))))
(equal? '(a b) (shuffle '(a b)))
