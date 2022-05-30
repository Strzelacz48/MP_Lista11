#lang plait

;; my-and i my-or

(define-syntax my-and
  (syntax-rules ()
    [(my-and) #t]
    [(my-and x1 x2 ...)
     (if (equal? x1 #f)
         #f
         (my-and x2 ...))]))

(define-syntax my-or
  (syntax-rules ()
    [(my-or) #f]
    [(my-or x1 x2 ...)
     (if (equal? x1 #t)
         #t
         (my-or x2 ...))]))

;; my-let i my-let*

(define-syntax my-let
  (syntax-rules ()
    [(my-let (var ...) exp)
     (let [var ...] exp)]))

(define-syntax my-let*
  (syntax-rules ()
    [(my-let* (var ...) exp)
     (let* [var ...] exp)]))
