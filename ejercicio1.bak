#lang racket

(define (Hallarfb n)
  (if (= n 0)
      0
      (if (= n 1)
          1
          (+ (Hallarfb (- n 1)) (Hallarfb (- n 2)))
       )
   )
)

(define (llegar i)
  (if(> ( Hallarfb i) 10000 )
     (void)
     (begin
      (printf "~a " (Hallarfb i))
      (llegar (+ i 1)))
     ))

(llegar 0)