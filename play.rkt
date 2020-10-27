#lang racket
(require 2htdp/image)
(require lang/posn)
;
;
(let test ([n 3])
  (if (zero? n)
      ;(square 10 'solid 'red)
      (text/font "赢" 24 "olive"
                     "Gill Sans" 'swiss 'normal 'bold #f)
      (let ([t (test (- n 1))])
        (freeze (above t (beside t t))))))
;
;(define item1
;  (text/font "吃" 24 "olive"
;             "Gill Sans" 'swiss 'normal 'bold #f))
;
;(define item2
;  (above (text/font "赞" 24 "olive"
;                    "Gill Sans" 'swiss 'normal 'bold #f)
;         (beside
;          (text/font "赞" 24 "olive"
;                     "Gill Sans" 'swiss 'normal 'bold #f)
;          (text/font "赞" 24 "olive"
;                     "Gill Sans" 'swiss 'normal 'bold #f))))
;(define item3
;  (above (beside
;          (text/font "赞" 24 "olive"
;                     "Gill Sans" 'swiss 'normal 'bold #f)
;          (text/font "赞" 24 "olive"
;                     "Gill Sans" 'swiss 'normal 'bold #f))
;         (text/font "赞" 24 "olive"
;                    "Gill Sans" 'swiss 'normal 'bold #f)
;         ))
;
;(define a (let sierpinski ([n 2])
;            (if (zero? n)
;                ;;(triangle 10 'solid 'red)
;                item2
;                (let ([t (sierpinski (- n 1))])
;                  (freeze (above t (beside t t)))))))
;(define b (let sierpinski ([n 2])
;            (if (zero? n)
;                ;;(triangle 10 'solid 'red)
;                item3
;                (let ([t (sierpinski (- n 1))])
;                  (freeze (above (beside t t) t ))))))
;
;(let sierpinski ([n 0])
;  (if (zero? n)
;      ;;(triangle 10 'solid 'red)
;      (freeze (above
;               (beside a b)
;               (beside b a)))
;      (let ([t (sierpinski (- n 1))])
;        (freeze (above t (beside t t))))))

;(define im
;  (overlay
;   (text/font "刘♫帆" 24 "Gold"
;              "Gill Sans" 'system 'slant 'bold #f)
;   (rectangle 200 60 "solid" "Maroon")
;   )
;  )
;im
;
;(save-svg-image im "a.svg")

