
(define (get-image-pos char r c)
  (define (helper i j)
    (cond [(and (= j (- r 1)) (= i (- c 1))) (if (= (send g% get-bg i j) char) (cons i j)
                                                 (display "bitch please"))]
          [(= j r) (helper (+ i 1) 0 )]
          [(equal? (send g% get-bg i j) char) (cons i j)]
          [else (helper  i (+ j 1))]))
  (helper 0 0))


