#lang racket

(define (HallarFibonacci n)
  (if (= n 0)
      0
      (if (= n 1)
          1
          (+ (HallarFibonacci (- n 1)) (HallarFibonacci (- n 2)))
       )
   )
)

(define (SumarFibonacci si n)
  (if (> (HallarFibonacci n) 100)
      (printf "y su suma es: :~a " si )
      ;else
      (begin
        (printf "~a, " (HallarFibonacci n) )
        (SumarFibonacci (+ si (HallarFibonacci n)) (+ n 1))
        
        )
      
   )
  
)
  

(SumarFibonacci 0 0)
