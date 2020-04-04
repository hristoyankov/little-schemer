#lang racket
(require "preface.rkt")

(provide counter)
(provide set-counter)
(provide consC)

(define counter '())
(define set-counter '())

(define consC
  (let ((N 0))
    (set! counter
          (lambda () N))
    (set! set-counter
          (lambda (x)
            (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(define find
  (lambda (n Ns Rs)
    (letrec
        ((A (lambda (ns rs)
              (cond
                ((null? ns) #f)
                ((eq? (car ns) n) (car rs))
                (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (if (zero? n)
                              'pizza
                              (consC (deepM (sub1 n)) '()))))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))

(define deep
  (lambda (n)
    (if (zero? n)
        'pizza
        (consC (deep (sub1 n)) '()))))

(define supercounter
  (lambda (f)
    (letrec
        ((S (lambda (n)
              (if (zero? n)
                  (f n)
                  (let ()
                    (f n)
                    (S (sub1 n)))))))
      (S 1000)
      (counter))))

(define-syntax try
  (syntax-rules ()
    ((try var a . b)
     (let/cc success
       (let/cc var (success a)) . b))))

(define rember1*C
  (lambda (a l)
    (letrec ((rm (lambda (l oh)
                   (cond
                     ((null? l) (oh 'no))
                     ((atom? (car l))
                      (if (eq? (car l) a)
                          (cdr l)
                          (consC (car l) (rm (cdr l) oh))))
                     (else
                      (try oh2
                           (consC (rm (car l) oh2)
                                  (cdr l))
                           (consC (car l)
                                  (rm (cdr l) oh))))))))
      (try oh (rm l oh) l))))

(define rember1*C2
  (lambda (a l)
    (letrec
        ((R (lambda (l)
              (cond
                ((null? l) '())
                ((atom? (car l))
                 (cond
                   ((eq? (car l) a) (cdr l))
                   (else (consC (car l) (R (cdr l))))))
                (else
                 (let ((av (R (car l))))
                   (cond
                     ((equal? (car l) av)
                      (consC (car l) (R (cdr l))))
                     (else (consC av (cdr l))))))))))
      (R l))))

