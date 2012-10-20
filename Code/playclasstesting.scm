#lang racket/gui

(require racket/draw)

(include "grid-vector.scm")
(include "pacman_class_version1_9.scm")
(include "target.rkt")
(include "playfunctiontesting.scm")
(include "crossing.scm")


;; Global Macros 
(define sleepaft 2)
(define sleepgo 1)
(define counter 0)
(define img 0)


;; Reading of various images
(define ig (read-bitmap "bg2.png"))
(define bg (read-bitmap "rg2.png"))
(define cg (read-bitmap "yg2.png"))
(define pg (read-bitmap "pg2.png"))
(define bg1 (read-bitmap "rg1.png"))
(define pg1 (read-bitmap "pg1.png"))
(define ig1 (read-bitmap "bg1.png"))
(define cg1 (read-bitmap "yg1.png"))
(define pr (read-bitmap "pacmanr.png"))
(define pl (read-bitmap "pacmanl.png"))
(define p (read-bitmap "pacman.png"))
(define pu (read-bitmap "pacmanu.png"))
(define pd (read-bitmap "pacmand.png"))
(define gameover (read-bitmap "gameover1.jpeg"))
(define gameover2 (read-bitmap "gameover.jpg")) 
(define press 'up)

(define (show x) 
  (let ([a x])
    (begin (display a)
           x)))
(define frame (new frame% [label "Pacman"]
                          [width 1000]
                          [height 768]))

(define frame1 (new frame% [label "Game Over"]
                          [width 600]
                          [height 400]))







(include "home.scm")


(define msg
  (new message% [parent frame]
       [label "playing :P :P"]))


(define score (new button% [parent frame1]
                   [label "Left"]
                   [callback (lambda (button event)
                               (send msg set-label "Left click"))]
                   [min-width 100]
                   [min-height 50]))

(define (paint1 dc)
 ; (sleep 0.5)
  (send dc draw-bitmap gameover2 50 50)
  ;(send canvas1 on-paint)
  ;(send dc draw-bitmap gameover2 350 350))
)

(define canvas1
  (new canvas% [parent frame1]
      ; [paint-callback (lambda (canvas1 dc) (paint1 dc ))]
       ))


   
(define (scorecard points)
  (* 10  (- 234 foodcounter)))

(define my-canvas%
  (class canvas%
    (define/override (on-char event)
      (define pressed (send event get-key-code))
     (cond[(not (equal? pressed 'release)) 
                                              
                                                  (cond [(= pacman-life total-life)
                                                         (send msg set-label "Game-over")
                                                         (sleep 1)
                                                         (send frame show #f)
                                                         (send frame1 show #t)
                                                         (send score set-label (number->string (scorecard foodcounter)))
                                                        ; (send (send canvas1 get-dc) draw-bitmap gameover 200 50)
                                                         ;(sleep 3)
                                                         (send (send canvas1 get-dc) draw-bitmap gameover2 50 30)
                                                         ;(send canvas1 on-paint)
                                                         (sleep sleepaft)
                                                         ;(send frame1 show #f)
                                                         (begin (set! press 'null) (animated press))
                                                         ]
                                                        [(> pacman-life total-life) (set! press 'null)]
                                                        [else (if (equal? press pressed) (void)
                                                                  (begin (set! press pressed)
                                                                         (set! counter 0)))]
                                                        ;(animated press)
                                                        )]
          [else (begin (set! counter (+ counter 1))
                        (cond [(= counter 1) (void)]
                              [(< pacman-life total-life)  (animated press)]
                              [(>= pacman-life total-life)
                               (send msg set-label "Game-over")
                               (sleep sleepgo)
                                          (send frame show #f)
                                          (send frame1 show #t)
                                          (send score set-label (number->string (scorecard foodcounter)))
                                          ;(send (send canvas1 get-dc) draw-bitmap gameover 200 50)
                                          ;(sleep 3)
                                          (send (send canvas1 get-dc) draw-bitmap gameover2 50 30)
                                         ; (send canvas1 on-paint)
                                          (sleep sleepaft)
                                          ;(send frame1 show #f)
                                          (begin (set! press 'null) (animated press))
                                          ]
                              ;[else (se]
                              
                                         ; (send (send canvas1 get-dc)  draw-bitmap gameover 350 350)                                          ]
                              [else (animated press)]
                              ))]))
    (define/public (get-image type pos)     ;gives the image coordinates in pixels wrt the given cooridinates in grid
      (if (equal? type 'pacman) (cons (+ 300 (* 25 (car pos))) (+ 10 (* 25 (cdr pos))))
          (cons (+ 292.5 (* 25 (car pos))) (+ 2.5 (* 25 (cdr pos))))))
    
    (define/public (get-image-x type pos) (car (get-image type pos)))
    (define/public (get-image-y type pos) (cdr (get-image type pos)))

    (define/public (animated dir)              ;;Most imp:: It handles the complete calling of all the functions and setting of images
      (cond[(equal? dir 'up) (sleep/yield sleep1)
                             (send pac% set-direction 3)
                             (begin (cond [(= foodcounter indicator1) (send pac% set-mode 2) (send msg set-label "mode changed")]
                                          [(= foodcounter indicator2) (send pac% set-mode 1) (send msg set-label "mode changed")])
                                          
                             (cond [(= (send g% get-bg (send pac% get-x) (- (send pac% get-y) 1)) 2) 
                                    (begin (send canvas on-paint)
                                           (crossing1?)
                                          ; (display counter1)
                                          ; (newline)
                                           (cond [(=(send pac% get-mode) 1) (black-tile-mode1 pu)]
                                                 [(=(send pac% get-mode) 2) (black-tile-mode2 pu)]))]
                                   [(or (=(send g% get-bg (send pac% get-x) (send pac% get-y)) 9)
                                        (=(send g% get-bg (send pac% get-x) (send pac% get-y)) 3)
                                        (=(send g% get-bg (send pac% get-x) (send pac% get-y)) 5))
                                   
                                    (begin (if (= (send g% get-bg (send pac% get-x) (send pac% get-y)) 9) (set! foodcounter (- foodcounter 1))
                                               (void))
                                          ; (display "food eaten")
                                          ; (newline)
                                           ;(display foodcounter)
                                    (send g% set-bg (send pac% get-x) (send pac% get-y) 3)
                                    (send canvas on-paint)
                                    (crossing1?)
                                          ; (display counter1)
                                          ; (newline)
                                           
                                    (send pac% set-position (cons (send pac% get-x) (- (send pac% get-y) 1)))
                                    (cond [(=(send pac% get-mode) 1) (black-tile-mode1 pu)]
                                          [(=(send pac% get-mode) 2) (black-tile-mode2 pu)]))]))]
           [(equal? dir 'down) (sleep/yield sleep1)
                               (send pac% set-direction 4)
                               (begin (cond [(= foodcounter indicator1) (send pac% set-mode 2) (send msg set-label "Mode Changed")]                                                  
                                            [(= foodcounter indicator2) (send pac% set-mode 1)])
                                            
                                      (cond [(= (send g% get-bg (send pac% get-x) (+ (send pac% get-y) 1)) 2) 
                                             (send canvas on-paint)
                                             (begin (crossing1?)
                                                    (cond [(=(send pac% get-mode) 1) (black-tile-mode1 pd)]
                                                          [(=(send pac% get-mode) 2) (black-tile-mode2 pd)]))]
                                            [(or (=(send g% get-bg (send pac% get-x) (send pac% get-y)) 9)
                                                 (=(send g% get-bg (send pac% get-x) (send pac% get-y)) 3)
                                                 (=(send g% get-bg (send pac% get-x) (send pac% get-y)) 5))
                                             
                                             (begin(if (= (send g% get-bg (send pac% get-x) (send pac% get-y)) 9) (set! foodcounter (- foodcounter 1))
                                                       (void))
                                                 ;  (display "food eaten")
                                                  ; (newline)
                                                  ; (display foodcounter)
                                                   
                                    (send g% set-bg (send pac% get-x) (send pac% get-y) 3)
                                    (send canvas on-paint)
                                    (crossing1?)
                                    ; (display counter1) 
                                    ; (newline)
                                    (send pac% set-position (cons (send pac% get-x) (+ (send pac% get-y) 1)))
                                    (cond [(=(send pac% get-mode) 1) (black-tile-mode1 pd)]
                                          [(=(send pac% get-mode) 2) (black-tile-mode2 pd)]))]))]
[(equal? dir 'left) (sleep/yield sleep1)
                    (send pac% set-direction 2)
                    (begin (cond [(= foodcounter indicator1) (begin (send pac% set-mode 2) (send msg set-label "mode changed"))]
                                       [(= foodcounter indicator2) (begin (send pac% set-mode 1) (send msg set-label "mode changed"))])
                                 
                             (cond [(= (send g% get-bg (-(send pac% get-x) 1) (send pac% get-y)) 2)  
                                    (send canvas on-paint)
                                    (begin (crossing1?) 
                                         ;  (display counter1)
                                          ; (newline)
                                            (cond [(=(send pac% get-mode) 1) (black-tile-mode1 pl)]
                                                  [(=(send pac% get-mode) 2) (black-tile-mode2 pl)]))]
                                   [(or (=(send g% get-bg (send pac% get-x) (send pac% get-y)) 9)
                                        (=(send g% get-bg (send pac% get-x) (send pac% get-y)) 3)
                                        (=(send g% get-bg (send pac% get-x) (send pac% get-y)) 5))
                                    
                                    (begin (if (= (send g% get-bg (send pac% get-x) (send pac% get-y)) 9) (set! foodcounter (- foodcounter 1))
                                               (void))
                                        ;   (display "food eaten")
                                         ;  (newline)
                                          ; (display foodcounter)
                                    (send g% set-bg (send pac% get-x) (send pac% get-y) 3)
                                    (send canvas on-paint)
                                    (crossing1?)
                                          ; (display counter1)
                                          ; (newline)
                                           (send pac% set-position (cons (-(send pac% get-x)1) (send pac% get-y)))
                                           (cond [(=(send pac% get-mode) 1) (black-tile-mode1 pl)]
                                                 [(=(send pac% get-mode) 2) (black-tile-mode2 pl)]))]))]
           [(equal? dir 'right) (sleep/yield sleep1)
                                 (send pac% set-direction 1)
                                 (begin (cond [(= foodcounter indicator1) (begin (send pac% set-mode 2) (send msg set-label "Mode Changed"))]
                                                     [(= foodcounter indicator2) (begin (send pac% set-mode 1)(send msg set-label "Mode Changed"))])
                                               
                             (cond [(= (send g% get-bg (+(send pac% get-x) 1) (send pac% get-y)) 2)  
                                    (send canvas on-paint)
                                    (begin (crossing1?)
                                          ; (display counter1)
                                           ;(newline)
                                           (cond [(=(send pac% get-mode) 1) (black-tile-mode1 pr)]
                                                 [(=(send pac% get-mode) 2) (black-tile-mode2 pr)]))]
                                   [(or (=(send g% get-bg (send pac% get-x) (send pac% get-y)) 9)
                                        (=(send g% get-bg (send pac% get-x) (send pac% get-y)) 3)
                                        (=(send g% get-bg (send pac% get-x) (send pac% get-y)) 5))
                                    
                                    (begin (if (= (send g% get-bg (send pac% get-x) (send pac% get-y)) 9) (set! foodcounter (- foodcounter 1))
                                               (void))
                                        ;   (display "food eaten")
                                         ;  (newline)
                                          ; (display foodcounter)
                                    (send g% set-bg (send pac% get-x) (send pac% get-y) 3)
                                    (send canvas on-paint)
                                    (crossing1?)
                                         ;  (display counter1)
                                        ;   (newline)
                                           (send pac% set-position (cons (+(send pac% get-x)1) (send pac% get-y)))
                                           (cond [(=(send pac% get-mode) 1) (black-tile-mode1 pr)]
                                                 [(=(send pac% get-mode) 2) (black-tile-mode2 pr)]))]))]
           [else (void)]))
    (super-new)))

(define canvas
  (new my-canvas% [parent frame]
               [paint-callback
                (lambda (canvas dc) (paint dc))]))

(define (paint dc) (make-grid grid-vec 29 28 dc ))

(define bg2 (read-bitmap "i2.png"))
(define bg9 (read-bitmap "i9.png"))
(define bg3 (read-bitmap "i3.png"))
(define bg5 (read-bitmap "i5.png"))

(define (make-grid vec r c dc)
  (define (helper i j)
    
      (cond  [(= i c) (helper 0 (+ j 1))]
             [(and (= j (- r 1)) (= i (- c 1))) (cond [(equal? (2d-vector-ref vec j i) 2) (send dc draw-bitmap bg2 (+ (* 25 i) 300) (+ (* 25 j) 10))]
                                                      [(equal? (2d-vector-ref vec j i) 3) (send dc draw-bitmap bg3 (+ (* 25 i) 300) (+ (* 25 j) 10))]
                                                      [(equal? (2d-vector-ref vec j i) 9) (send dc draw-bitmap bg9 (+ (* 25 i) 300) (+ (* 25 j) 10))]
                                                      [(equal? (2d-vector-ref vec j i) 5) (send dc draw-bitmap bg5 (+ (* 25 i) 300) (+ (* 25 j) 10))])]
           
            
            [else (cond [(equal? (2d-vector-ref vec j i) 2)
                                 (begin (send dc draw-bitmap bg2 (+ (* 25 i) 300) (+ (* 25 j) 10))
                                        (helper (+ i 1) j))]
                        [(equal? (2d-vector-ref vec j i) 9)
                         (begin (send dc draw-bitmap bg9 (+ (* 25 i) 300) (+ (* 25 j) 10)) 
                                (helper (+ i 1) j))]
                        [(equal? (2d-vector-ref vec j i) 3)
                         (begin (send dc draw-bitmap bg3 (+ (* 25 i) 300) (+ (* 25 j) 10)) 
                                (helper (+ i 1) j))]
                        [(equal? (2d-vector-ref vec j i) 5)
                         (begin (send dc draw-bitmap bg5 (+ (* 25 i) 300) (+ (* 25 j) 10))
                                (helper (+ i 1) j))])]))
            
   
         
  (helper 0 0)
         ;(draw-life (- total-life pacman-life) dc)))
  )

   
    

(define (draw-life alpha dc)
 (define (helper i)
  (if (< i alpha ) (begin (send dc draw-bitmap pr (* 10 i) 20) (helper (+ i 1)))
      (void)))
  (helper 1))

;(time (send frame show  #t))
;(send g% shortest-path (cons 12 1) (cons 19  1) 3)

(begin-play)