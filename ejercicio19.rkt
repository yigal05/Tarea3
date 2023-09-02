#lang racket
(define (colocar i n m)
     (if (= i 9)
      ( if (not (= n m))
        (begin
          (newline)
          (colocar 0 (+ n 1) (- m 1))
        );else
        (void)
      )
      ;else
      (if (or (= i n) (= i m))
              (begin
                (printf "a" )
                (colocar (+ i 1) n m))
              ;else
              (begin
                (printf " " )
                (colocar (+ i 1) n m))
       )
   )
)

(colocar 0 0 8)