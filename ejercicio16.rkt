#lang racket
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))
      ))


(define (sumatoria n si)
(if (not (= n 0))
    (sumatoria (- n 1) (+ (factorial n) si))
    si
 )
 )

(sumatoria 4 1)