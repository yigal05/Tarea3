#lang racket
(define (pareja n m c)
  (if (< n 9)
      (if (= (remainder n 3) 0)
          (begin
            (printf "~a ~a ~a\n" n m c)
            (pareja (+ n 1) (+ m 1) 1)
            )
          (begin
            (printf "~a ~a ~a\n" n m c)
            (pareja (+ n 1) m (+ c 1) )
            )
         
       )
      (printf "~a ~a ~a \n" n m c)
      
   )
 )

(pareja 1 1 1)