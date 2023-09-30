#lang racket
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))
      ))

(define (sumatoria i x t m)
  (if (= i t)
      (print (add1 (exact->inexact m)))
      (sumatoria (add1 i) x t (+ m (/ (expt x (* (add1 i) 2)) (factorial (* (add1 i) 2)))))
   )
  )

;(define valor (read))
;(define nterminos (read))

(sumatoria 0 1 10 0)