;#lang racket
(define c1 1)
(define c2 1)
(define counter1 c1)
(define counter2 c2)
(define (crossing1?)                               ;;checks the position of pacman and all the ghost
  (define (opposite? lst)
  (let ([dir (list-ref lst 1)])
    (cond [(equal? dir 1) (list (cons (+ 1 (caar lst)) (cdar lst)) 2)]
          [(equal? dir 2) (list (cons (- (caar lst) 1) (cdar lst)) 1)]
          [(equal? dir 3) (list (cons (caar lst) (- (cdar lst) 1)) 4)]
          [(equal? dir 4) (list (cons (caar lst) (+ (cdar lst) 1)) 3)]
          [else dir])))
  
  (let* ([pacpos (send pac% get-position)]
        [pacdir (send pac% get-direction)]
        [bpos (send blinky% get-position)]
        [plist (list pacpos pacdir)]  
        [ipos (send inky% get-position)]
        [ppos (send pinky% get-position)]
        [cpos (send clyde% get-position)]
        [bdir (send blinky% get-direction)]
        [cdir (send clyde% get-direction)]
        [idir (send inky% get-direction)]
        [pdir (send pinky% get-direction)])
    (cond [(equal? (opposite? (list bpos bdir)) plist) (set! counter1 2)]
          [(equal? (opposite? (list ipos idir)) plist) (set! counter1 2)]
          [(equal? (opposite? (list ppos pdir)) plist) (set! counter1 2)]
          [(equal? (opposite? (list cpos cdir)) plist) (set! counter1 2)]
          [else (set! counter1 1)])))

(define (crossing2?)
  (define (opposite? lst)
  (let ([dir (list-ref lst 1)])
    (cond [(equal? dir 1) (list (cons (-  (caar lst) 1) (cdar lst)) 2)]
          [(equal? dir 2) (list (cons (+ (caar lst) 1) (cdar lst)) 1)]
          [(equal? dir 3) (list (cons (caar lst) (+ (cdar lst) 1)) 4)]
          [(equal? dir 4) (list (cons (caar lst) (- (cdar lst) 1)) 3)]
          [else dir])))
  
  (let* ([pacpos (send pac% get-position)]
        [pacdir (send pac% get-direction)]
        [bpos (send blinky% get-position)]
        [plist (list pacpos pacdir)]  
        [ipos (send inky% get-position)]
        [ppos (send pinky% get-position)]
        [cpos (send clyde% get-position)]
        [bdir (send blinky% get-direction)]
        [cdir (send clyde% get-direction)]
        [idir (send inky% get-direction)]
        [pdir (send pinky% get-direction)])
    (cond [(equal? (opposite? (list bpos bdir)) plist) (set! counter2 2)]
          [(equal? (opposite? (list ipos idir)) plist) (set! counter2 2)]
          [(equal? (opposite? (list ppos pdir)) plist) (set! counter2 2)]
          [(equal? (opposite? (list cpos cdir)) plist) (set! counter2 2)]
          [else (set! counter2 1)])))
