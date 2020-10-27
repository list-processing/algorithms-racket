#lang racket

(require 2htdp/image)
(require lang/posn)

(define right-black-semi-circle (crop 50 0 50 100
 (circle 50 "solid" "black")
 ))

(define left-white-semi-circle (crop 0 0 50 100
 (circle 50 "solid" "white")
 ))

(define right-big-white-semi-circle (crop 100 0 100 200
 (circle 100 "solid" "white")
 ))

(define left-big-black-semi-circle (crop 0 0 100 200
 (circle 100 "solid" "black")
 ))

(define big-circle-outline
  (circle 100 "outline" "black"))

(define t-white (text/font "λ" 100 "white"
              "Gill Sans" 'system 'slant 'bold #f))

(define t-black (text/font "λ" 100 "black"
              "Gill Sans" 'system 'slant 'bold #f))

(define big-circle (overlay/offset
 left-big-black-semi-circle
 100 0
 right-big-white-semi-circle))

(define big-circle1 (overlay/offset
 right-black-semi-circle
 -24 50
 big-circle))

(define big-circle-2 (overlay/offset
 left-white-semi-circle
 24 -50
 big-circle1))

(define big-circle-3 (overlay/offset
 big-circle-2
 0 0
 big-circle-outline))

(define big-circle-4 (overlay/offset
 (rotate 180 t-white)
 0 50
 big-circle-3))

(define r (overlay/offset
 t-black
 0 -50
 big-circle-4))

(define rr (rotate 45 (freeze r)))

(save-svg-image r "a.svg")

(rotate 45 r)
