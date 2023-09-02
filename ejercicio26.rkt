#lang racket
(define (imprimir n)
  (if (= n 10)
      (printf "~a\n" (string-append (make-string 9 #\space) "Z") ) 
      (begin
        (printf "~a\n" (string-append (make-string (- n 1) #\space) "Z"  (make-string (- 19 (* 2 n)) #\space) "Z" ) ) 
        (imprimir (+ n 1))
       )
   )
 )

(imprimir 1)