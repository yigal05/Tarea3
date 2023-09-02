#lang racket
(define (pareja n m)
  (if (< n 9)
      (if (= (remainder n 2) 0)         
          (begin
            (printf "~a ~a \n" n m)
            (pareja (+ n 1) m)
            )
          (begin
            (printf "~a ~a \n" n m)
            (pareja (+ n 1) (+ m 1))
            )
       )
      (printf "~a ~a \n" n m)
      
   )
 )

(pareja 0 1)