#lang racket

(define (random-i s e)
  (random s (add1 e)))

(define (exchange v x y)
  (let* ((tmp (vector-ref v x)))
    (vector-set! v x (vector-ref v y))
    (vector-set! v y tmp)))

(define (quick-sort v p r)
  (when (< p r)
    (let ((q (random-partition v p r)))
      (quick-sort v p (- q 1))
      (quick-sort v (+ q 1) r))))

(define (random-partition v p r)
  (let ((t (random-i p r)))
    (exchange v t r)
    (partition v p r)))

(define (partition v p r)
  (let* ((x (vector-ref v r))
         (i (- p 1)))
    (define (iter j)
      (when (< j r)
        (when (< (vector-ref v j) x)
          (set! i (add1 i))
          (exchange v i j))
        (iter (add1 j))))
    (iter p)
    (exchange v (add1 i) r)
    (add1 i)))

(define v (vector 4 1 3 2 16 9 10 14 8 7))
(quick-sort v 0 9)