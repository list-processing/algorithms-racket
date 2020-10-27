#lang racket

;; sorting the number in 0 to (k-1)
(define k 11)

(define (counting-sort v)
  (let* ((c (make-vector k 0))
         (length (vector-length v))
         (b (make-vector length 0)))

    (for ([n v])
      (vector-set! c n (+ 1 (vector-ref c n))))

    (for-each (lambda (i)
                (vector-set! c i (+
                                  (vector-ref c i)
                                  (vector-ref c (- i 1)))))
              (range 1 k))

    (for ([n v])
                  (vector-set! b (- (vector-ref c n) 1) n)
                  (vector-set! c n (- (vector-ref c n) 1)))
    b
    ))

(define v (vector 1 3 2 4 6 5 3 7 6 5 4 3 9 10))
(counting-sort v)