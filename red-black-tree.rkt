#lang racket

(require racket/undefined)

(struct tree (root len) #:mutable #:transparent)
(struct node (value parent left right color) #:mutable #:transparent)

(define (tree-init)
  (tree empty 0))

(define (node-init x p)
  (node x p empty empty 'red))

(define (node-grandparent n)
  (node-parent (node-parent n)))

(define (node-uncle n)
  (if (eq? (node-parent n) (node-left (node-grandparent n)))
      (node-right (node-grandparent n))
      (node-left (node-grandparent n))))

(define (tree-insert t x)
  (define (iter-insert n p child)
    (cond ((empty? n) 
           (let ((nd (node-init x p)))
             (cond ((eq? child undefined) void)
                   ((eq? child 'left) (set-node-left! p nd))
                   (else (set-node-right! p nd)))
             nd
             ))
          ((< x (node-value n)) (iter-insert (node-left n) n 'left))
          (else (iter-insert (node-right n) n 'right))))

  (let ((n (iter-insert (tree-root t) empty undefined)))
    (when (= (tree-len t) 0)
      (set-tree-root! t n))

    (adjust-tree-1 t n)
    (set-tree-len! t (add1 (tree-len t)))))

(define (adjust-tree-1 t n)
  (if (empty? (node-parent n))
      (set-node-color! n 'black)
      (adjust-tree-2 t n)))

(define (adjust-tree-2 t n)
  (unless (equal? (node-color (node-parent n)) 'black)
    (adjust-tree-3 t n)))

(define (adjust-tree-3 t n)
  (if (and (not (empty? (node-uncle n)))
           (equal? (node-color (node-uncle n)) 'red))
      (begin
        (set-node-color! (node-parent n) 'black)
        (set-node-color! (node-uncle n) 'black)
        (set-node-color! (node-grandparent n) 'red)
        (adjust-tree-1 t (node-grandparent n)))
      (adjust-tree-4 t n)))

(define (adjust-tree-4 t n)
  (cond ((and
          (eq? n (node-right (node-parent n)))
          (eq? (node-parent n) (node-left (node-grandparent n))))
         (rotate-left t n)
         (adjust-tree-5 t (node-left n)))
        ((and
          (eq? n (node-right (node-parent n)))
          (eq? (node-parent n) (node-left (node-grandparent n))))
         (rotate-right t n)
         (adjust-tree-5 t n)
         )
        (else
         (adjust-tree-5 t n))))

(define (adjust-tree-5 t n)
  (set-node-color! (node-parent n) 'black)
  (set-node-color! (node-grandparent n) 'red)
  (if (and (eq? n (node-left (node-parent n)))
           (eq? (node-parent n) (node-left (node-grandparent n))))
      (rotate-right t (node-parent n))
      (rotate-left t (node-parent n))))

(define (rotate-left t n)
  (let ((p (node-parent n))
        (g (node-grandparent n)))
    (if (empty? g)
        (set-tree-root! t n)
        (unless (empty? g)
          (if (eq? p (node-left g))
              (set-node-left! g n)
              (set-node-right! g n))))
    
    (set-node-parent! n g)
    (set-node-right! p (node-left n))
    (set-node-parent! p n)
    (unless (empty? (node-left n))
      (set-node-parent! (node-left n) p))
    (set-node-left! n p)
    ))

(define (rotate-right t n)
  (let ((p (node-parent n))
        (g (node-grandparent n)))
    (if (empty? g)
        (set-tree-root! t n)
        (if (eq? p (node-left g))
            (set-node-left! g n)
            (set-node-right! g n)))
    
    (set-node-parent! n g)
    (set-node-left! p (node-right n))
    (set-node-parent! p n)
    (unless (empty? (node-right n))
      (set-node-parent! (node-right n) p))
    (set-node-right! n p)
    ))
        


(define t (tree-init))
(tree-insert t 5)
(tree-insert t 3)
(tree-insert t 1)
(tree-insert t 2)
(tree-insert t 4)
(tree-insert t 8)
(tree-insert t 6)
(tree-insert t 7)
(tree-insert t 9)
;                 5 b
;                 |
;        -------------------
;        |                 |
;        3 r               7 r
;        |                 |
;   ----------        -----------
;   |        |        |         |
;   1 b      4 b      6  b      8 b
;   |                 |         |
;   ---                         ---
;     |                           |
;     2  r                        9 r

