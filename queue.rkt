#lang racket

(struct queue (front-ptr rear-ptr len)  #:mutable #:transparent)

(define (make-queue)
  (queue empty empty 0))

(define (queue-empty? q)
  (= (queue-len q) 0))

(define (queue-front q)
  (if (queue-empty? q)
      (error "FRONT called with empty queue" q)
      (mcar (queue-front-ptr q))))

(define (queue-en q x)
  (let ((tail (mcons x empty)))
    
    (if (queue-empty? q)
        (begin
          (set-queue-front-ptr! q tail)
          (set-queue-rear-ptr! q tail)
          (set-queue-len! q (add1 (queue-len q))))
        (begin
          (set-mcdr! (queue-rear-ptr q) tail)
          (set-queue-rear-ptr! q tail)
          (set-queue-len! q (add1 (queue-len q)))))))

(define (queue-delete q)
  (if (queue-empty? q)
      (error "DELETE called with empty queue" q)
      (begin0
        (mcar (queue-front-ptr q))
        (set-queue-len! q (sub1 (queue-len q)))
        (set-queue-front-ptr! q (mcdr (queue-front-ptr q))))))

(define (queue-print q)
  (eprintf "the queue is ~a;\nthe length is ~a\n" (queue-front-ptr q) (queue-len q)))


(define (test)
  (let ((q (make-queue)))
    (queue-en q 1)
    (queue-en q 2)
    (queue-print q)
    (queue-delete q)
    (queue-print q)
        (queue-delete q)
    (queue-print q)
    ))
(test)