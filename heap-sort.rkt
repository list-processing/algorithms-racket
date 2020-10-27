#lang racket

(struct heap (v
              length
              size) #:mutable)

(define (left-child x)
  (+ (* 2 x) 1))

(define (right-child x)
  (+ (* 2 x) 2))

(define (parient x)
  (quotient (- x 1) 2))

(define (exchange h x y)
  (let* ((v (heap-v h))
         (tmp (vector-ref v x)))
    (vector-set! v x (vector-ref v y))
    (vector-set! v y tmp)))

(define (max-index h i)
  (let ((left (left-child i))
        (right (right-child i))
        (max i)
        (v (heap-v h))
        (size (heap-size h)))
    (when (and (< left size)
               (> (vector-ref v left) (vector-ref v max)))
      (set! max left))
    (when (and (< right size)
               (> (vector-ref v right) (vector-ref v max)))
      (set! max right))
    max))

(define (max-heapify h i)
  (let ((max (max-index h i)))
    (when (not (= max i))
      (exchange h max i)
      (max-heapify h max))))

(define (build-max-heap h)
  (let ((length (heap-length h)))
  (for-each (lambda (i)
              (max-heapify h i))
            (range (quotient (- length 1) 2) -1 -1))))

(define (build-heap v)
  (heap v (vector-length v) (vector-length v)))

(define (heap-sort v)
  (let ((h (build-heap v)))
    (build-max-heap h)
    (for-each (lambda (x)
                (exchange h x 0)
                (set-heap-size! h (- (heap-size h) 1))
                (max-heapify h 0))
              (range (- (vector-length v) 1) 0 -1))))

(define (heap-maximum h)
  (vector-ref (heap-v h) 0))
  
(define (heap-extract-max h)
  (exchange h 0 (- (heap-size h) 1))
  (set-heap-size! h (- (heap-size h) 1))
  (max-heapify h 0)
  (vector-ref (heap-v h) (heap-size h)))

(define (heap-increase-key h i key)
  (when (< key (vector-ref (heap-v h) i))
      (error "key is smaller than current key!"))

  (vector-set! (heap-v h) i key)

  (define (adjust-heap i)
    (when (and (> i 0)
               (< (vector-ref (heap-v h) (parient i))
                  (vector-ref (heap-v h) i)))
      (exchange h i (parient i))
      (adjust-heap (parient i))))

  (adjust-heap i))

(define (max-heap-insert h key)
  (when (>= (heap-size h) (heap-length h))
    (error "heap is full"))

  (vector-set! (heap-v h) (heap-size h) -inf.0)
  (set-heap-size! h (+ (heap-size h) 1))
  (heap-increase-key h (- (heap-size h) 1) key))

(define v (vector 4 1 3 2 16 9 10 14 8 7))
(define h (heap v (vector-length v) (vector-length v)))
(build-max-heap h)
(heap-extract-max h)
(heap-v h)
(max-heap-insert h 16)
(heap-v h)