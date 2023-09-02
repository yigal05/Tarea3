#lang racket

(define (imprimir n)
  (if (= n 8)
      (void)
      (if (= n 4)
          (begin
            (printf "~a\n" (string-append (make-string 27 #\space)
                                      (string-append  (make-string (- n 1) #\A) (make-string 0 #\space) (make-string n #\A) )
                                      (make-string (+ 27 n) #\space)))
            (imprimir (+ n 1))
           )
          (if (> n 4)
              (begin
                (printf "~a\n" (string-append (make-string 27 #\space)
                                      (string-append  (make-string (- 8 n) #\A) (make-string (- (* n 2) 9) #\space) (make-string (- 8 n) #\A) )
                                      (make-string (+ 27 n) #\space)))
                (imprimir (+ n 1))
                )
              (begin
                (printf "~a\n" (string-append (make-string 27 #\space)
                                      (string-append  (make-string n #\A) (make-string (- 7 (* n 2)) #\space) (make-string n #\A) )
                                      (make-string (+ 27 n) #\space)))
                (imprimir (+ n 1))
               )
            )
          )
   )
  )

(imprimir 1)