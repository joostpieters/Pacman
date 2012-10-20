;#lang racket
(require graphics/graphics)
(open-graphics)
(define home-viewport (open-viewport "Pacman" 405 405))


((draw-pixmap home-viewport) "Home-screen.jpg" (make-posn 0 0))
(define (begin-play)
  (let* ([click (cordie)]
        [x-y (mouse-click-posn click)]
        [refer (newlist (get-xy x-y ))])
        
    (cond [(equal? refer (list 1 1)) (begin 
                                       (send frame show #t)
                                       (close-viewport home-viewport ))]
                                            
         ; [(equal? refer (list 1 2)) "masala"]
          [(equal? refer (list 1 3)); (open-viewport about-viewport)]
           (define about-viewport (open-viewport "about" 900 500))
          ; (open-viewport about-viewport)
           ((draw-pixmap about-viewport) "hiray.jpg" (make-posn 0 0))]
          [else (begin-play)])))

(define (cordie) (get-mouse-click home-viewport))

(define (get-x a) (posn-x a))
(define (get-y a) (posn-y a))



;(128 301) (264 317)
;(144 338) (250 353)
;(159 372) (227 389)
(define (get-xy a) (cons (get-x a) (get-y a)))

(define (newlist lst)
  (let ([y (cdr lst)])
    (cond [(and (<= y 317) (>= y 301)) (list 1 1)]
          [(and (<= y 338) (>= y 353)) (list 1 2)]
          [(and (<= y 360) (>= y 395)) (list 1 3)])))
        
;(begin-play)
  