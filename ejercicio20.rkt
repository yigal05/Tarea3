#lang racket

(define (imprimir n)
  (if (= n 80)
      (printf "~a" (string-append (make-string (- 80 n) #\space) (make-string n #\a) ))
      (begin
        (printf "~a\n" (string-append (make-string (- 80 n) #\space) (make-string n #\a) ) )
        (imprimir (+ n 1))
       )
   )
 )

(imprimir 1)