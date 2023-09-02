#lang racket
(define (HallarFibonacci n)
  
  (if (= n 0)
      1
      (if (= n 1)
          0
          (if (= n 2)
              0
              (+ (HallarFibonacci (- n 2)) (HallarFibonacci (- n 3)))
            )
       )
   )
)

(define numero (read))
(define (LlamarFb i)
  (if (< i numero)
      (begin
        (printf "~a " (HallarFibonacci i))
         (LlamarFb (+ i 1)))
      (void))
  )
(LlamarFb 0)