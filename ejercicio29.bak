#lang racket
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))
      ))

(define (sumatoria i x t m)
  (if (= i t)
      (print (exact->inexact m))
      (sumatoria (add1 i) x t (+ m (/ (expt x i) (factorial i))))
   )
  )

(define valor (read))
(define nterminos (read))

(sumatoria 0 valor nterminos 0)