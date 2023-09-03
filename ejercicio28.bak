#lang racket
(define (imprimir n)
  (if (= n 12)
      (void)
      (if (< n 6)
          (begin
            (printf "~a\n" (string-append (make-string (- 40 n) #\space) (string-append (make-string (- n 1) #\a) "a"  )  ) )
            (imprimir (+ n 1))
          )
          (begin
            (printf "~a\n" (string-append (make-string (- 36 (- 8 n) ) #\space) (string-append (make-string (- 11 n) #\a) "a" )  ) )
            (imprimir (+ n 1))
           )
   )
   )
 )

(imprimir 1)