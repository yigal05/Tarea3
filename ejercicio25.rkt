#lang racket
(define (imprimir n)
  (if (= n 8)
      (void)
      (if (< n 4)
          (begin
            (printf "~a\n" (string-append (make-string (- 11 n) #\space) (string-append (make-string (- n 1) #\a) "a" (make-string (- n 1) #\a) )  ) )
            (imprimir (+ n 1))
          )
          (begin
            (printf "~a\n" (string-append (make-string (- 11 (- 8 n) ) #\space) (string-append (make-string (- 7 n) #\a) "a" (make-string (- 7 n) #\a) )  ) )
            (imprimir (+ n 1))
           )
   )
   )
 )

(imprimir 1)