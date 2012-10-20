;#lang racket

(define (black-tile-mode1 pacchar)              ;;responsible for setting of positions of all ghosts and pacman and their directions
  (let* ([alpha (send g% shortest-path (send blinky% get-position) (target (send pac% get-position) (send pac% get-direction) 'blinky) (send blinky% get-direction))]
         [beta (send g% shortest-path (send inky% get-position) (target (send pac% get-position) (send pac% get-direction) 'inky) (send inky% get-direction))]
         [gamma (send g% shortest-path (send pinky% get-position) (target (send pac% get-position) (send pac% get-direction) 'pinky) (send pinky% get-direction))]
         [delta (send g% shortest-path (send clyde% get-position) (target (send pac% get-position) (send pac% get-direction) 'clyde) (send clyde% get-direction))]
         [la (length alpha)]
         [lb (length beta)]
         [lc (length gamma)]
         [ld (length delta)]
         [cur (send pac% get-position)]
         )
         
         
    
           (cond [(not (or 
                 (= la 1)
                 (= lb 1)
                 (= lc 1)
                 (= ld 1)
                 )
           ) 
           (send blinky% set-direction (send g% update-direction alpha 'blinky))
           (send blinky% set-position (cadr alpha))
           (send inky% set-direction (send g% update-direction beta 'inky))
           (send inky% set-position (cadr beta))
           (send pinky% set-direction (send g% update-direction gamma 'pinky))
           (send pinky% set-position (cadr gamma))
           (send clyde% set-direction (send g% update-direction delta 'clyde))
           (send clyde% set-position (cadr delta))
           
            (begin (crossing2?)
                   ;(display "crossing2")
                   ;(display counter2)
                 ;  (newline)
    
          (cond [(and (not 
                  (or 
                   (equal? (cadr alpha) cur)
                   (equal? (cadr beta) cur) 
                 (equal? (cadr gamma) cur)
                  (equal? (cadr delta) cur)
                 )
                 )  (or (= counter1 1) (= counter2 1))) 
                          (cond [(= img 1) (send (send canvas get-dc) draw-bitmap p (send canvas get-image-x 'pacman (send pac% get-position)) (send canvas get-image-y 'pacman (send pac% get-position)))
                            (send (send canvas get-dc) draw-bitmap bg1(send canvas get-image-x 'ghost (send blinky% get-position)) (send canvas get-image-y 'ghost (send blinky% get-position)))
                            (send (send canvas get-dc) draw-bitmap ig1(send canvas get-image-x 'ghost (send inky% get-position)) (send canvas get-image-y 'ghost (send inky% get-position)))
                            (send (send canvas get-dc) draw-bitmap pg1(send canvas get-image-x 'ghost (send pinky% get-position)) (send canvas get-image-y 'ghost (send pinky% get-position)))
                            (send (send canvas get-dc) draw-bitmap cg1(send canvas get-image-x 'ghost (send clyde% get-position)) (send canvas get-image-y 'ghost (send clyde% get-position)))
                            (set! img 0)
                           ; (display (send pac% get-position))
                           ; (newline)
                            (sleep/yield sleep1)
                            (send canvas animated press)]
                                [else (send (send canvas get-dc) draw-bitmap pacchar (send canvas get-image-x 'pacman (send pac% get-position)) (send canvas get-image-y 'pacman (send pac% get-position)))
                                      (send (send canvas get-dc) draw-bitmap bg(send canvas get-image-x 'ghost (send blinky% get-position)) (send canvas get-image-y 'ghost (send blinky% get-position)))
                                      (send (send canvas get-dc) draw-bitmap ig(send canvas get-image-x 'ghost (send inky% get-position)) (send canvas get-image-y 'ghost (send inky% get-position)))
                                      (send (send canvas get-dc) draw-bitmap pg(send canvas get-image-x 'ghost (send pinky% get-position)) (send canvas get-image-y 'ghost (send pinky% get-position)))
                                      (send (send canvas get-dc) draw-bitmap cg(send canvas get-image-x 'ghost (send clyde% get-position)) (send canvas get-image-y 'ghost (send clyde% get-position)))
                                      (set! img 1)
                                      (sleep/yield sleep1)
                                     ; (display (send pac% get-position))
                                    ;  (newline)
                                      (send canvas animated press)])]
                [else (send inky% set-direction dir)
                      (send clyde% set-direction dir)
                      (send blinky% set-direction dir)
                      (send pinky% set-direction dir)
                      (send inky% set-position posinky)
                      (send blinky% set-position posblinky)
                      (send pinky% set-position pospinky)
                      (send clyde% set-position posclyde)
                      ;(send pac% set-position (cons 13 19))
                      (send msg set-label "Dead ")
                      (sleep 3)
                      (send msg set-label "playing")
                      ;(display "Im dead sob sob :'( ")
                      ;(newline)
                      (send pac% set-life (+ (send pac% get-life) 1))
                     ; (display "life")
                     ; (display pacman-life)
                      (send canvas animated press)
                      ]))]
          
           [else (length-1-mode pacchar)])))



(define (length-1-mode pacchar)
  (let* ([posi (send inky% get-position)]
        [posp (send pinky% get-position)]
        [posb (send blinky% get-position)]
        [posc (send clyde% get-position)]
        [diri (send inky% get-direction)]
        [dirp (send pinky% get-direction)]
        [dirb (send blinky% get-direction)]
        [dirc (send clyde% get-direction)]
        [posi2 (send g% pos-squares posi diri)]
        [posb2 (send g% pos-squares posb dirb)]
        [posp2 (send g% pos-squares posp dirp)]
        [posc2 (send g% pos-squares posc dirc)]
        [iy (append (list posi) posi2)]
        [by (append(list  posb) posb2)]
        [cy (append (list posc) posc2)]
        [py (append (list posp) posp2)]
        [curr (send pac% get-position)])
    (send blinky% set-direction (send g% update-direction by 'blinky))
    (send blinky% set-position (cadr by))
    (send inky% set-direction (send g% update-direction iy 'inky))
    (send inky% set-position (cadr iy))
    (send pinky% set-direction (send g% update-direction py 'pinky))
    (send pinky% set-position (cadr py))
    (send clyde% set-direction (send g% update-direction cy 'clyde))
    (send clyde% set-position (cadr cy))
    
    (begin (crossing2?)
                  ; (display "crossing2")
                  ; (display counter2)
                  ; (newline)
                   (cond [(and (not 
                                (or 
                                 (equal? (cadr iy) curr)
                                 (equal? (cadr by) curr) 
                                 (equal? (cadr cy) curr)
                                 (equal? (cadr py) curr)
                                 )
                                )  (or (= counter1 1) (= counter2 1))) 
                          
                          (cond [(= img 1) (send (send canvas get-dc) draw-bitmap p (send canvas get-image-x 'pacman (send pac% get-position)) (send canvas get-image-y 'pacman (send pac% get-position)))
                                           (send (send canvas get-dc) draw-bitmap bg1(send canvas get-image-x 'ghost (send blinky% get-position)) (send canvas get-image-y 'ghost (send blinky% get-position)))
                                           (send (send canvas get-dc) draw-bitmap ig1(send canvas get-image-x 'ghost (send inky% get-position)) (send canvas get-image-y 'ghost (send inky% get-position)))
                                           (send (send canvas get-dc) draw-bitmap pg1(send canvas get-image-x 'ghost (send pinky% get-position)) (send canvas get-image-y 'ghost (send pinky% get-position)))
                                           (send (send canvas get-dc) draw-bitmap cg1(send canvas get-image-x 'ghost (send clyde% get-position)) (send canvas get-image-y 'ghost (send clyde% get-position)))
                                           (set! img 0)
                                           (sleep/yield sleep1)
                                          ; (display (send pac% get-position))
                                          ; (newline)
                                           (send canvas animated press)]
                                [else (send (send canvas get-dc) draw-bitmap pacchar (send canvas get-image-x 'pacman (send pac% get-position)) (send canvas get-image-y 'pacman (send pac% get-position)))
                                      (send (send canvas get-dc) draw-bitmap bg(send canvas get-image-x 'ghost (send blinky% get-position)) (send canvas get-image-y 'ghost (send blinky% get-position)))
                                      (send (send canvas get-dc) draw-bitmap ig(send canvas get-image-x 'ghost (send inky% get-position)) (send canvas get-image-y 'ghost (send inky% get-position)))
                                      (send (send canvas get-dc) draw-bitmap pg(send canvas get-image-x 'ghost (send pinky% get-position)) (send canvas get-image-y 'ghost (send pinky% get-position)))
                                      (send (send canvas get-dc) draw-bitmap cg(send canvas get-image-x 'ghost (send clyde% get-position)) (send canvas get-image-y 'ghost (send clyde% get-position)))
                                      (set! img 1)
                                      (sleep/yield sleep1)
                                     ; (display (send pac% get-position))
                                     ; (newline)
                                     ; (send canvas animated press)
                                      ])]
                        [else (send inky% set-direction dir)
                              (send clyde% set-direction dir)
                              (send blinky% set-direction dir)
                              (send pinky% set-direction dir)
                              (send inky% set-position posinky)
                              (send blinky% set-position posblinky)
                              (send pinky% set-position pospinky)
                              (send clyde% set-position posclyde)
                              ;(send pac% set-position (cons 13 19))
                              (send msg set-label "Dead ")
                              (sleep 3)
                              (send msg set-label "Playing")
                             ; (display "IM dead")
                             ; (newline)
                              ;(newline)
                              (send pac% set-life (+ (send pac% get-life) 1))
                             ; (display pacman-life)
                              ;(set! life (+ life 1))
                             ; (display "life")
                              ;(display life)
                             ; (send canvas animated press)
                              ]))))
    
    
    
        
        
(define (black-tile-mode2 pacchar)
  (let ([alpha (send g% scatter (send blinky% get-position) (send blinky% get-direction) 'blinky)]
        [beta (send g% scatter (send inky% get-position) (send inky% get-direction) 'inky)]
        [gamma (send g% scatter (send pinky% get-position) (send blinky% get-direction) 'pinky)]
        [delta (send g% scatter (send clyde% get-position) (send clyde% get-direction) 'clyde)]
        [curr (send pac% get-position)])
    (send blinky% set-direction (send g% update-direction alpha 'blinky))
    (send blinky% set-position (cadr alpha))
    (send inky% set-direction (send g% update-direction beta 'inky))
    (send inky% set-position (cadr beta))
    (send pinky% set-direction (send g% update-direction gamma 'pinky))
    (send pinky% set-position (cadr gamma))
    (send clyde% set-direction (send g% update-direction delta 'clyde))
    (send clyde% set-position (cadr delta))
    
  
  (begin (crossing2?)
         ;(display "crossing2")
         ;(display counter2)
        ; (newline)
    
   (cond [(and (not  (or 
                      (equal? (cadr alpha) curr)
                      (equal? (cadr beta) curr) 
                      (equal? (cadr gamma) curr)
                      (equal? (cadr delta) curr)
                      )
                     )  (or (= counter1 1) (= counter2 1)))  
          (cond [(= img 1) (send (send canvas get-dc) draw-bitmap p (send canvas get-image-x 'pacman (send pac% get-position)) (send canvas get-image-y 'pacman (send pac% get-position)))
                     (send (send canvas get-dc) draw-bitmap bg1(send canvas get-image-x 'ghost (send blinky% get-position)) (send canvas get-image-y 'ghost (send blinky% get-position)))
                     (send (send canvas get-dc) draw-bitmap ig1(send canvas get-image-x 'ghost (send inky% get-position)) (send canvas get-image-y 'ghost (send inky% get-position)))
                     (send (send canvas get-dc) draw-bitmap pg1(send canvas get-image-x 'ghost (send pinky% get-position)) (send canvas get-image-y 'ghost (send pinky% get-position)))
                     (send (send canvas get-dc) draw-bitmap cg1(send canvas get-image-x 'ghost (send clyde% get-position)) (send canvas get-image-y 'ghost (send clyde% get-position)))
                     (set! img 0)
                     (sleep/yield sleep1)
                     (send canvas animated press)]
                [else (send (send canvas get-dc) draw-bitmap pacchar (send canvas get-image-x 'pacman (send pac% get-position)) (send canvas get-image-y 'pacman (send pac% get-position)))
                      (send (send canvas get-dc) draw-bitmap bg(send canvas get-image-x 'ghost (send blinky% get-position)) (send canvas get-image-y 'ghost (send blinky% get-position)))
                      (send (send canvas get-dc) draw-bitmap ig(send canvas get-image-x 'ghost (send inky% get-position)) (send canvas get-image-y 'ghost (send inky% get-position)))
                      (send (send canvas get-dc) draw-bitmap pg(send canvas get-image-x 'ghost (send pinky% get-position)) (send canvas get-image-y 'ghost (send pinky% get-position)))
                      (send (send canvas get-dc) draw-bitmap cg(send canvas get-image-x 'ghost (send clyde% get-position)) (send canvas get-image-y 'ghost (send clyde% get-position)))
                      (set! img 1)
                      (sleep/yield sleep1)
                      (send canvas animated press)])]
         [else (send inky% set-direction dir)
               (send clyde% set-direction dir)
               (send blinky% set-direction dir)
               (send pinky% set-direction dir)
               (send inky% set-position posinky)
               (send blinky% set-position posblinky)
               (send pinky% set-position pospinky)
               (send clyde% set-position posclyde)
               ;(set! life (+ life 1))
                (send pac% set-life (+ (send pac% get-life) 1))
               ; (display (send pac% get-life))
               ; (send pac% set-position (cons 13 19))
               (send msg set-label "Pacman dead ")
               (sleep 3)
               (send msg set-label "playing :P :P")
              ; (display "IM dead")
               
               ;(newline)
               ] ))))


  
                                           

