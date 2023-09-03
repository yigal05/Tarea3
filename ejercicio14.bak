#lang racket

(define (tabla numero n)
   (if (< n 11)
       (begin
         (printf "~a x ~a = ~a \n" numero n (* numero n) )
         (tabla numero (+ n 1)))
        (newline)
   )
 
)


(define (llamarTablas n)
(if (= n 10)
    (tabla n 1)
    (begin
      (tabla n 1)
      (llamarTablas (+ n 1))
      )
 )
  )



(llamarTablas 1)