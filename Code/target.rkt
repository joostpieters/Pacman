;#lang racket
;(define (2d-vector-ref vec r c)
;  (vector-ref (vector-ref vec r) c))
;
;(define (2d-vector-set! vec r c val)
;  (vector-set! (vector-ref vec r c val)))
;
;(define grid-vec #(#( 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)
;                   #( 2 9 9 9 9 9 9 9 9 9 9 9 9 2 2 9 9 9 9 9 9 9 9 9 9 9 9 2)
;                   #( 2 9 2 2 2 2 9 2 2 2 2 2 9 2 2 9 2 2 2 2 2 9 2 2 2 2 9 2)
;                   #( 2 5 2 2 2 2 9 2 2 2 2 2 9 2 2 9 2 2 2 2 2 9 2 2 2 2 5 2)
;                   #( 2 9 2 2 2 2 9 2 2 2 2 2 9 2 2 9 2 2 2 2 2 9 2 2 2 2 9 2)
;                   #( 2 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 9 2)
;                   #( 2 9 2 2 2 2 9 2 2 9 2 2 2 2 2 2 2 2 9 2 2 9 2 2 2 2 9 2)
;                   #( 2 9 2 2 2 2 9 2 2 9 2 2 2 2 2 2 2 2 9 2 2 9 2 2 2 2 9 2)
;                   #( 2 9 9 9 9 9 9 2 2 9 9 9 9 2 2 9 9 9 9 2 2 9 9 9 9 9 9 2)
;                   #( 2 2 2 2 2 2 9 2 2 2 2 2 3 2 2 3 2 2 2 2 2 9 2 2 2 2 2 2)
;                   #( 2 2 2 2 2 2 9 2 2 2 2 2 3 2 2 3 2 2 2 2 2 9 2 2 2 2 2 2)
;                   #( 2 2 2 2 2 2 9 2 2 3 3 3 3 3 3 3 3 3 3 2 2 9 2 2 2 2 2 2)
;                   #( 2 2 2 2 2 2 9 2 2 3 2 2 2 2 2 2 2 2 3 2 2 9 2 2 2 2 2 2)
;                   #( 2 2 2 2 2 2 9 2 2 3 2 2 2 2 2 2 2 2 3 2 2 9 2 2 2 2 2 2)
;                   #( 3 3 3 3 3 3 9 3 3 3 2 2 2 2 2 2 2 2 3 3 3 9 3 3 3 3 3 3)
;                   #( 2 2 2 2 2 2 9 2 2 3 2 2 2 2 2 2 2 2 3 2 2 9 2 2 2 2 2 2)
;                   #( 2 2 2 2 2 2 9 2 2 3 2 2 2 2 2 2 2 2 3 2 2 9 2 2 2 2 2 2)
;                   #( 2 2 2 2 2 2 9 2 2 3 3 3 3 3 3 3 3 3 3 2 2 9 2 2 2 2 2 2)
;                   #( 2 2 2 2 2 2 9 2 2 3 2 2 2 2 2 2 2 2 3 2 2 9 2 2 2 2 2 2)
;                   #( 2 9 9 9 9 9 9 9 9 9 9 9 9 2 2 9 9 9 9 9 9 9 9 9 9 9 9 2)
;                   #( 2 9 2 2 2 2 9 2 2 2 2 2 9 2 2 9 2 2 2 2 2 9 2 2 2 2 9 2)
;                   #( 2 9 2 2 2 2 9 2 2 2 2 2 9 2 2 9 2 2 2 2 2 9 2 2 2 2 9 2)
;                   #( 2 5 9 9 2 2 9 9 9 9 9 9 9 3 3 9 9 9 9 9 9 9 2 2 9 9 5 2)
;                   #( 2 2 2 9 2 2 9 2 2 9 2 2 2 2 2 2 2 2 9 2 2 9 2 2 9 2 2 2)
;                   #( 2 2 2 9 2 2 9 2 2 9 2 2 2 2 2 2 2 2 9 2 2 9 2 2 9 2 2 2)
;                   #( 2 9 9 9 9 9 9 2 2 9 9 9 9 2 2 9 9 9 9 2 2 9 9 9 9 9 9 2)
;                   #( 2 9 2 2 2 2 2 2 2 2 2 2 9 2 2 9 2 2 2 2 2 2 2 2 2 2 9 2)
;                   #( 2 9 9 9 9 9 9 9 9 9 9 9 9 2 2 9 9 9 9 9 9 9 9 9 9 9 9 2)
;                   #( 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2)))
;
;
(define pac-pos (send pac% get-position))
(define b-pos (send blinky% get-position))
(define p-pos (send pinky% get-position))
(define i-pos (send inky% get-position))
(define c-pos (send clyde% get-position))
(define c-s-pos (cons 12 25))
(define mood-i 'normal)  ;;;;mood-i (normal :- simple) (vagg :- blinky) (agg :- pinky)

(define (target pac-pos p-dir ghost)
  (define (offset pos i j) (cons (- (car pos) i) (- (cdr pos) j)))
  (define (+2block)
    (cond [(equal? p-dir 3) (offset pac-pos 2 2)]
          [(equal? p-dir 4) (offset pac-pos 0 -2)]
          [(equal? p-dir 2) (offset pac-pos 2 0)]
          [(equal? p-dir 1) (offset pac-pos -2 0)]))
  (define (check pos)
    (define x (car pos)) (define y (cdr pos))
    (define (helper1)
      (let ([x1 (< x 1)] [x2 (> x 26)] [y1 (< y 1)] [y2 (> y 27)])
      (cond [(and x1 y1) (begin (set! y 1) (set! x 1) (helper2 1))]
             [(and x2 y2) (begin (set! y 27) (set! x 26) (helper2 1))]
             [(and x1 y2) (begin (set! x 1) (set! y 27) (helper2 1))]
             [(and x2 y1) (begin (set! y 1) (set! x 26) (helper2 1))]
             [y1 (begin (set! y 1) (helper2 1))]
             [y2 (begin (set! y 27) (helper2 1))]
             [x1 (begin (set! x 1) (helper2 1))]
             [x2 (begin (set! x 26) (helper2 1))]
             [else (helper2 1)])))
    (define (helper2 i)
      (cond [(not (= (2d-vector-ref grid-vec y x) 2)) (cons x y)]
            [(and (or (= x 1) (= x 2)) (and (>= y 10) (<= y 18))) (begin (set! x (+ x 2)) (helper2 i))]
            [(and (or (= x 25) (= x 26)) (and (>= y 10) (<= y 18))) (begin (set! x (- x 2)) (helper2 i))]
            [(not (= (2d-vector-ref grid-vec y (+ x i)) 2)) (cons (+ x i) y)]
            [(not (= (2d-vector-ref grid-vec y (- x i)) 2)) (cons (- x i) y)]
            [(not (= (2d-vector-ref grid-vec (- y i) x) 2)) (cons x (- y i))]
            [(not (= (2d-vector-ref grid-vec (+ y i) x) 2)) (cons x (+ y i))]
            [else (helper2 (+ 1 i))]))
    
    (helper1))

          
  (define (target-b) pac-pos)
  
  (define (target-p)
    (let ([alpha (cond [(equal? p-dir 3) (check (offset pac-pos 4 4))]
                       [(equal? p-dir 4) (check (offset pac-pos 0 -4))]
                       [(equal? p-dir 2) (check (offset pac-pos 4 0))]
                       [(equal? p-dir 1) (check (offset pac-pos -4 0))])])
      (if (equal? alpha (send pinky% get-position)) (car (send g% pos-squares alpha (send pinky% get-direction))) alpha)))
  
  (define (target-c)
    (if (>= (+ (sqr (- (car pac-pos) (car c-pos))) (sqr (- (cdr pac-pos) (cdr c-pos)))) 64) (target-b) c-s-pos))
  
  (define (target-i)
    (let ([alpha (cond [(equal? mood-i 'vagg) (target-b)]
                       [(equal? mood-i 'agg) (target-p)]
                       [(equal? mood-i 'normal) (let ([alpha1 (+2block)])
                                                  (check (cons (- (* 2 (car alpha1)) (car b-pos)) (- (* 2 (cdr alpha1)) (cdr b-pos)))))])])
      (if (equal? alpha (send inky% get-position)) (car (send g% pos-squares alpha (send inky% get-direction))) alpha)))
  
  (cond [(equal? ghost 'blinky ) (target-b)]
        [(equal? ghost 'pinky ) (target-p)]
        [(equal? ghost 'inky ) (target-i)]
        [(equal? ghost 'clyde ) (target-c)]
        [else "invalid input"]))
;(target pac-pos 'left 'inky)