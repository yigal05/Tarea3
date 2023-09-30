#lang racket
(define conter "+&@*+@*&")
(define ty "12345678")
(define zz "12345678")
(define conteroculto (string-copy zz))

(define (verificar)
  (if (equal? zz conter)
      (printf "Ganaste")
      (primero (read))
      ))

(define (segundo x y)

(string-set!  conteroculto y (string-ref conter y ) )  
(printf conteroculto)
(if (equal? (string-ref conteroculto x) (string-ref conteroculto y) ) 
    (void)
    (begin
      (string-set! conteroculto x (string-ref ty x) )  
      (string-set! conteroculto y (string-ref ty y) )  
     )
)
(newline)
(printf conteroculto)
(verificar)
)

(define (primero x)

(string-set! conteroculto (- x 1) (string-ref conter (- x 1) ) )
(newline)
(printf conteroculto)

(segundo (- x 1) (- (read) 1) ) 

)

(primero (read))