#lang racket

(struct stack (ptr len) #:mutable #:transparent)

(define (stack-init)
  (stack empty 0))

(define (stack-empty? s)
  (empty? (stack-ptr s)))

(define (stack-push s x)
  (let ((s-s (stack-ptr s))
        (s-len (stack-len s)))
    (set-stack-ptr! s (cons x s-s))
    (set-stack-len! s (add1 s-len))))

(define (stack-pop s)
  (if (not (stack-empty? s))
      (let* ((s-s (stack-ptr s))
             (e (car s-s))
             (s-len (stack-len s)))
        (set-stack-ptr! s (cdr s-s))
        (set-stack-len! s (sub1 s-len))
        e)
      (error "empty stack")))

(define (stack-print s)
  (eprintf "the stack is ~a;\nthe length is ~a\n" (stack-ptr s) (stack-len s)))

(define (test)
  (let ((s (stack-init))
        (e 0))
    (stack-push s 1)
    (stack-push s 2)
    (stack-print s)
    (stack-pop s)
    (stack-print s)
    (stack-pop s)
    (stack-print s)
    ))

(test)