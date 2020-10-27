#lang racket

(define (minimum v)
  (let ((min (vector-ref v 0)))
    (for-each (lambda (i)
                (when (< (vector-ref v i) min)
                  (set! min (vector-ref v i))))
              (range 1 (vector-length v)))
    min))

(define (min-and-max v)
  (define (init)
    (if (odd? (vector-length v))
        (values (vector-ref v 0)
                (vector-ref v 0)
                1)
        (let-values (((a b) (compare 0)))
          (values a
                  b
                  2))))
  (define (compare i)
    (let ((a (vector-ref v i))
          (b (vector-ref v (+ 1 i))))
      (if (< a b)
          (values a b)
          (values b a))))
  
  (let-values (((min max start) (init))
               ((len) (vector-length v)))
    (for-each (lambda (i)
                (let-values (((a b) (compare i)))
                  (when (< a min)
                    (set! min a))
                  (when (> b max)
                    (set! max b))))
              (range start len 2))
    (values min max)))


(define (exchange v x y)
  (let* ((tmp (vector-ref v x)))
    (vector-set! v x (vector-ref v y))
    (vector-set! v y tmp)))


(define (random-i s e)
  (random s (add1 e)))

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

(define (randomized-select v p r i)
  (if (= p r)
      (vector-ref v p)
      (let* ((q (random-partition v p r))
             (k (+ 1 (- q p))))
        (cond
          ((= k i)
           (vector-ref v q))
          ((< i k)
           (randomized-select v p (- q 1) i))
          (true
           (randomized-select v (+ q 1) r (- i k)))))))

(define (insert-sort v l r)
  (define (iter i)
    (when (and (> i l)
               (> (vector-ref v (- i 1))
                  (vector-ref v i)))
      (exchange v i (- i 1))
      (iter (- i 1))))

  (for-each (lambda (i)
              (iter i))
            (range (+ l 1) (+ r 1)))
  )

(define (select-mid v l r)
  (if (= l r)
      (values l (vector-ref v l))
      (select-mid-1 v l r)))

(define (select-mid-1 v l r)
  (let ((len (+ 1 (- r l)))
        (n -1))
    (for-each (lambda (i)
                (insert-sort v i (+ i 4))
                (set! n (add1 n))
                (exchange v (+ i 2) (+ l n))
                )
              (range l (- r 4) 5))

    (when (< (* 5 (+ n 1)) len)
      (insert-sort v (+ l (* (+ n 1) 5)) r)
      (set! n (add1 n))
      (exchange v (quotient (+ r (* 5 n) l) 2) (+ l n)))

    (select-mid v l (+ l n))
    )
  )

(define (select v p r i)
  (if (= p r)
      (vector-ref v p)
      (let*-values (((vi va) (select-mid v p r))
                    ((q) (begin
                           (exchange v vi r)
                           (partition v p r)))
                    ((k) (+ 1 (- q p))))
        (cond
          ((= k i)
           (vector-ref v q))
          ((< i k)
           (select v p (- q 1) i))
          (true
           (select v (+ q 1) r (- i k)))))))

(define v (vector 1 2 3 4 5 6 7 8 9 0))

(for-each (lambda (x)
            (define v (vector 1 2 3 4 5 6 7 8 9 0))
            (displayln (list x (select v 0 9 x))))
          (range 1 11))