#lang racket

(define (imprimir n)
  (if (= n 7)
      (void)
      (begin
        (printf "~a\n" (string-append (make-string n #\space) (make-string (- 13 (* 2 n)) #\P) (make-string n #\space)))
        (imprimir (+ n 1))
       )
   )
  )

(imprimir 0)