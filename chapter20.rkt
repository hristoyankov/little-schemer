#lang racket
(require "preface.rkt")



(define the-empty-table
  (lambda (name)
    ))

(define lookup
  (lambda (table name)
    (table name)))

(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (cond
        ((eq? name1 name2) value)
        (else (table name2))))))

(define define?
  (lambda (e)
    (cond
      ((atom? e) #f)
      ((atom? (car e))
       (eq? (car e) 'define))
      (else #f))))

(define global-table
  ... the-empty-table ...)

(define box
  (lambda (it)
    (lambda (sel)
      (sel it (lambda (new)
                (set! it new))))))

(define setbox
  (lambda (box new)
    (box (lambda (it set) (set new)))))

(define unbox
  (lambda (box)
    (box (lambda (it set) it))))

(define *define
  (lambda (e)
    (set! global-table
          (extend (name-of e)
                  (box (the-meaning (right-side-of e)))
                  global-table))))

(define value
  (lambda (e)
    ...
    (cond
      ((define? e) (*define e))
      (else (the-meaning e)))
    ...))
