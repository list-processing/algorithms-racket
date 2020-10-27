#lang racket

(struct tree (root len) #:mutable #:transparent)
(struct node (value parent left right) #:mutable #:transparent)

(define (tree-init)
  (tree empty 0))


(define (node-init x p)
  (node x p empty empty))

(define (tree-insert t x)
  (define (iter-insert n p)
    (cond ((empty? n) (node-init x p))
          ((< x (node-value n))
           (set-node-left! n (iter-insert (node-left n) n))
           n)
          (else
           (set-node-right! n (iter-insert (node-right n) n))
           n)))

  (set-tree-root! t (iter-insert (tree-root t) empty))
  (set-tree-len! t (add1 (tree-len t))))

(define (tree-inorder-walk t)
  (define (iter n)
    (when (not (empty? n))
      (iter (node-left n))
      (eprintf "value ~a~n" (node-value n))
      (iter (node-right n))))

  (iter (tree-root t)))

(define (tree-preorder-walk t)
  (define (iter n)
    (when (not (empty? n))
      (eprintf "value ~a~n" (node-value n))
      (iter (node-left n))
      (iter (node-right n))))

  (iter (tree-root t)))

(define (tree-postorder-walk t)
  (define (iter n)
    (when (not (empty? n))
      (iter (node-left n))
      (iter (node-right n))
      (eprintf "value ~a~n" (node-value n))))

  (iter (tree-root t)))

(define (tree-search t x)
  (define (iter n)
    (cond ((= (node-value n) x)
           n)
          ((< x (node-value n))
           (iter (node-left n)))
          (else
           (iter (node-right n)))))
  (iter (tree-root t)))

(define (node-maxmum n)
  (if (empty? (node-right n))
      n
      (node-maxmum (node-right n))))

(define (node-minimum n)
  (if (empty? (node-left n))
      n
      (node-minimum (node-left n))))

(define (tree-maxmum t)
  (node-maxmum (tree-root t)))

(define (tree-minimum t)
  (node-minimum (tree-root t)))

(define (tree-node-successor n)
  (define (iter n)
    (let ((p (node-parent n)))
      (if (and (not (empty? p)) (eq? n (node-right p)))
          (iter p)
          p)))
  (if (empty? (node-right n))
      (iter n)
      (node-minimum (node-right n))))

(define (tree-node-predecessor n)
  (define (iter n)
    (let ((p (node-parent n)))
      (if (and (not (empty? p)) (eq? n (node-left p)))
          (iter p)
          p)))
  (if (empty? (node-left n))
      (iter n)
      (node-maxmum (node-left n))))

(define (tree-transplant t u v)
  (cond ((empty? (node-parent u))
         (set-tree-root! t v))
        ((eq? u (node-left (node-parent u)))
         (set-node-left! (node-parent u) v))
        (else
         (set-node-right! (node-parent u) v)))
  (unless (empty? v)
    (set-node-parent! v (node-parent u))))

(define (tree-delete t n)
  (cond ((empty? (node-left n))
         (tree-transplant t n (node-right n)))
        ((empty? (node-right n))
         (tree-transplant t n (node-left n)))
        (else
         (let ((s (tree-node-successor n)))
           (tree-delete-1 t n s)))))

(define (tree-delete-1 t n s)
  (when (not (eq? s (node-right n)))
    (tree-transplant t s (node-right s))
    (set-node-right! s (node-right n))
    (set-node-parent! (node-right n) s))

  (tree-transplant t n s)
  (set-node-left! s (node-left n))
  (set-node-parent! (node-left n) s))
  

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
;                 5
;                 |
;        -------------------
;        |                 |
;        3                 8
;        |                 |
;   ----------        -----------
;   |        |        |         |
;   1        4        6         9
;   |                 |
;   ---               ---
;     |                  |
;     2                  7

(displayln "inorder walk")
(tree-inorder-walk t)

(displayln "preorder walk")
(tree-preorder-walk t)

(displayln "postorder walk")
(tree-postorder-walk t)

(displayln "tree-search")
(displayln (tree-search t 3))


;                 6
;                 |
;        -------------------
;        |                 |
;        3                 8
;        |                 |
;   ----------        -----------
;   |        |        |         |
;   1        4        7         9
;   |
;   ---
;     |
;     2
(displayln "delete node 5")
(tree-delete t (tree-search t 5))
(display t)