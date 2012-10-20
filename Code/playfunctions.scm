;#lang racket

(define (black-tile-mode1 pacchar)
  (let* ([alpha (send g% shortest-path (send blinky% get-position) (target (send pac% get-position) (send pac% get-direction) 'blinky) (send blinky% get-direction))]
         [beta (send g% shortest-path (send inky% get-position) (target (send pac% get-position) (send pac% get-direction) 'inky) (send inky% get-direction))]
         [gamma (send g% shortest-path (send pinky% get-position) (target (send pac% get-position) (send pac% get-direction) 'pinky) (send pinky% get-direction))]
         [delta (send tpha)]
         [lb (length beta)]
         [lc (length gamma)]
         [ld (length delta)])
  ;  (cond [(not (or (= la 1) (= lb 1) (= lc 1) (= ld 1))) 
    (send blinky% set-direction (send g% update-direction alpha))
    (send blinky% set-position (cadr alpha))
    (send inky% set-direction (send g% update-direction beta))
    (send inky% set-position (cadr beta))
    (send pinky% set-direction (send g% update-direction gamma))
    (send pinky% set-position (cadr gamma))
    (send clyde% set-direction (send g% update-direction delta))
    (send clyde% set-position (cadr delta))
    
  (cond [(= img 1) (send (send canvas get-dc) draw-bitmap p (send canvas get-image-x 'pacman (send pac% get-position)) (send canvas get-image-y 'pacman (send pac% get-position)))
                   (send (send canvas get-dc) draw-bitmap bg1(send canvas get-image-x 'ghost (send blinky% get-position)) (send canvas get-image-y 'ghost (send blinky% get-position)))
                   (send (send canvas get-dc) draw-bitmap ig1(send canvas get-image-x 'ghost (send inky% get-position)) (send canvas get-image-y 'ghost (send inky% get-position)))
                   (send (send canvas get-dc) draw-bitmap pg1(send canvas get-image-x 'ghost (send pinky% get-position)) (send canvas get-image-y 'ghost (send pinky% get-position)))
                   (send (send canvas get-dc) draw-bitmap cg1(send canvas get-image-x 'ghost (send clyde% get-position)) (send canvas get-image-y 'ghost (send clyde% get-position)))
                   (set! img 0)
                   (sleep/yield 0.3)
                   (send canvas animated press)]
        [else (send (send canvas get-dc) draw-bitmap pacchar (send canvas get-image-x 'pacman (send pac% get-position)) (send canvas get-image-y 'pacman (send pac% get-position)))
              (send (send canvas get-dc) draw-bitmap bg(send canvas get-image-x 'ghost (send blinky% get-position)) (send canvas get-image-y 'ghost (send blinky% get-position)))
              (send (send canvas get-dc) draw-bitmap ig(send canvas get-image-x 'ghost (send inky% get-position)) (send canvas get-image-y 'ghost (send inky% get-position)))
              (send (send canvas get-dc) draw-bitmap pg(send canvas get-image-x 'ghost (send pinky% get-position)) (send canvas get-image-y 'ghost (send pinky% get-position)))
              (send (send canvas get-dc) draw-bitmap cg(send canvas get-image-x 'ghost (send clyde% get-position)) (send canvas get-image-y 'ghost (send clyde% get-position)))
              (set! img 1)
              (sleep/yield 0.3)
              (send canvas animated press)]))


(define (black-tile-mode2 pacchar)
  (let ([alpha (send g% scatter (send blinky% get-position) (send blinky% get-direction) 'blinky)]
        [beta (send g% scatter (send inky% get-position) (send inky% get-direction) 'inky)]
        [gamma (send g% scatter (send pinky% get-position) (send blinky% get-direction) 'pinky)]
        [delta (send g% scatter (send clyde% get-position) (send clyde% get-direction) 'clyde)]
        )
    (send blinky% set-direction (send g% update-direction alpha))
    (send blinky% set-position (cadr alpha))
    (send inky% set-direction (send g% update-direction beta))
    (send inky% set-position (cadr beta))
    (send pinky% set-direction (send g% update-direction gamma))
    (send pinky% set-position (cadr gamma))
    (send clyde% set-direction (send g% update-direction delta))
    (send clyde% set-position (cadr delta))
    )
    
    (cond [(= img 1) (send (send canvas get-dc) draw-bitmap p (send canvas get-image-x 'pacman (send pac% get-position)) (send canvas get-image-y 'pacman (send pac% get-position)))
                     (send (send canvas get-dc) draw-bitmap bg1(send canvas get-image-x 'ghost (send blinky% get-position)) (send canvas get-image-y 'ghost (send blinky% get-position)))
                     (send (send canvas get-dc) draw-bitmap ig1(send canvas get-image-x 'ghost (send inky% get-position)) (send canvas get-image-y 'ghost (send inky% get-position)))
                     (send (send canvas get-dc) draw-bitmap pg1(send canvas get-image-x 'ghost (send pinky% get-position)) (send canvas get-image-y 'ghost (send pinky% get-position)))
                     (send (send canvas get-dc) draw-bitmap cg1(send canvas get-image-x 'ghost (send clyde% get-position)) (send canvas get-image-y 'ghost (send clyde% get-position)))
                     (set! img 0)
                     (sleep/yield 0.3)
                     (send canvas animated press)]
          [else (send (send canvas get-dc) draw-bitmap pacchar (send canvas get-image-x 'pacman (send pac% get-position)) (send canvas get-image-y 'pacman (send pac% get-position)))
                (send (send canvas get-dc) draw-bitmap bg(send canvas get-image-x 'ghost (send blinky% get-position)) (send canvas get-image-y 'ghost (send blinky% get-position)))
                (send (send canvas get-dc) draw-bitmap ig(send canvas get-image-x 'ghost (send inky% get-position)) (send canvas get-image-y 'ghost (send inky% get-position)))
                (send (send canvas get-dc) draw-bitmap pg(send canvas get-image-x 'ghost (send pinky% get-position)) (send canvas get-image-y 'ghost (send pinky% get-position)))
                (send (send canvas get-dc) draw-bitmap cg(send canvas get-image-x 'ghost (send clyde% get-position)) (send canvas get-image-y 'ghost (send clyde% get-position)))
                (set! img 1)
                (sleep/yield 0.3)
                (send canvas animated press)]))


  
                                           

