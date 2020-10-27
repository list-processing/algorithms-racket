#lang racket

(require opencv/highgui)

(define image (imread "1.png" CV_LOAD_IMAGE_COLOR))
(imshow "Display window" image)