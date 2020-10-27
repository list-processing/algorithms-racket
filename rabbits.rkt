#lang racket

;(\_/)
;(*_*)
;/ >(\_/)
;   (*_*)
;   / >(\_/)
;      (*_*)
;      / >(\_/)
;         (*_*)
;         / >(\_/)
;            (*_*)
;            / >(\_/)
;               (*_*)
;               / >(\_/)
;                  (*_*)
;                  / >(\_/)
;                     (*_*)
;                     / >(\_/)
;                        (*_*)
;                        / >(\_/)
;                           (*_*)
;                           / >(\_/)
;                              (*_*)
;                              / >(\_/)
;                                 (*_*)
;                                 / >(\_/)
;                                    (*_*)
;                                    / >(\_/)
;                                       (*_*)
;                                       / >(\_/)
;                                          ......

(define (f n)
  (lambda (s)
    (lambda ()
      (begin0
        (string-append (make-string n #\space) s (make-string n #\space))
        (set! n (+ 3 n)))
      )))

(define (\_/) "(\\_/)\n")

(define *_*
  ((f 0) "(*_*)\n"))

(define / "/ ")

(define > ">")

(define (rabbits)
  (display
   (string-append
    (\_/)
    (*_*)
    / >))
  (rabbits))

(rabbits)