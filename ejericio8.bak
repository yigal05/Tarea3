#lang racket

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))
      ))

(define (HallarFibonacci n)
  
  (if (= n 0)
      1
      (/ (factorial (* 2 n)) (* (factorial n) (factorial (+ n 1))))
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