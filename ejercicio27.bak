#lang racket
(define (imprimir n)
  (if (= n 10)
      (printf "~a\n" (string-append  "Z"  (make-string 17 #\space) "Z" ) )
      (if (= n 1)
          (begin
            (printf "~a\n" (string-append (make-string 9 #\space) "Z"  ) ) 
            (imprimir (+ n 1) )
           )
          (begin
            (printf "~a\n" (string-append (make-string (- 10 n ) #\space) "Z"  (make-string (- (* 2 n ) 3) #\space) "Z" ) ) 
            (imprimir (+ n 1))
           )
      )
   )
 )

(imprimir 1)