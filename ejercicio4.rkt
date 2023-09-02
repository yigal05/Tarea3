#lang racket
(define (HallarFibonacci n)
  
  (if (= n 0)
      0
      (if (= n 1)
          1
          (+ (* 2 (HallarFibonacci (- n 1))) (HallarFibonacci (- n 2)))
       )
   )
)


(define numero (read))
(define (LlamarFb i)
  (if (<= i numero)
      (begin
        (printf "~a " (HallarFibonacci i))
         (LlamarFb (+ i 1)))
      (void))
  )
(LlamarFb 0)
