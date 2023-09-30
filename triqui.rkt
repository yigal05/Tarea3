#lang racket

(define we "123456789")
(define wa (string-copy we))

(define (verificar)
(if (or
     (= (char->integer (string-ref wa 0) ) (char->integer (string-ref wa 1) ) (char->integer (string-ref wa 2) ) )
     (= (char->integer (string-ref wa 3) ) (char->integer (string-ref wa 4) ) (char->integer (string-ref wa 5) ) )
     (= (char->integer (string-ref wa 6) ) (char->integer (string-ref wa 7) ) (char->integer (string-ref wa 8) ) )
     (= (char->integer (string-ref wa 0) ) (char->integer (string-ref wa 3) ) (char->integer (string-ref wa 6) ) )
     (= (char->integer (string-ref wa 1) ) (char->integer (string-ref wa 4) ) (char->integer (string-ref wa 7) ) )
     (= (char->integer (string-ref wa 2) ) (char->integer (string-ref wa 5) ) (char->integer (string-ref wa 8) ) )
     (= (char->integer (string-ref wa 0) ) (char->integer (string-ref wa 4) ) (char->integer (string-ref wa 8) ) )
     (= (char->integer (string-ref wa 6) ) (char->integer (string-ref wa 4) ) (char->integer (string-ref wa 2) ) ))
     "Ganaste"
     (void)
 )

)

(define (Display )
(printf "\n\n\n\n\n\n\n|~a|~a|~a|\n|~a|~a|~a|\n|~a|~a|~a|\n"
 (string-ref wa 0)
 (string-ref wa 1)
 (string-ref wa 2)
 (string-ref wa 3)
 (string-ref wa 4)
 (string-ref wa 5)
 (string-ref wa 6)
 (string-ref wa 7)
 (string-ref wa 8)
 )
 )

(define (preguntar casilla turno)
  (if (or (equal? (string-ref wa casilla) #\X) (equal? (string-ref wa casilla) #\O)  )
      (begin
        (printf "Esta casilla ya esta ocupada ingrese una nueva:\n")
        (preguntar (- (read) 1) turno)
       )
      (string-set! wa casilla turno)
   )
 )

(define (juego turno juegos)
  (if (= juegos 9)
      (printf "EMPATE")
      (begin
          (if (equal? (verificar) "Ganaste")
              (begin
                (Display)
                (printf "Gano ~a" (if (equal? turno #\X) "O" "X")))
              (begin
                (Display)
                (printf "turno de ~a, ingrese su casilla:\n" turno )
                (preguntar (- (read) 1) turno)
                (juego (if (equal? turno #\X) #\O #\X) (add1 juegos))
                )
              )
        )
      )
)


(juego #\X 0)

