#lang racket
(define letras "0@@++**##")
(define caracter (string-copy "0zzzzzzzz"))
(define cadenaJugador (string-copy "012345678"))

(define ( mezcladorLetras i randomz)
  (if (= i (string-length letras))
      (void)
      (if (equal? (string-ref caracter randomz) #\z)
          (begin
            (string-set! caracter randomz (string-ref letras i))
            (mezcladorLetras (add1 i) (random 1 9))
            )
            (mezcladorLetras i (random 1 9))    
       )
 )
)
(mezcladorLetras 1 (random 1 9))



(printf "~aJUEGO DEL MEMORAMA~a\n" (make-string 50 #\*)(make-string 50 #\*))
(printf "INGRESA TU NOMBRE JUGADOR 1: ")
(define primerJugador (read))
(printf "INGRESA TU NOMBRE JUGADOR 2: ")
(define segundoJugador (read))

(define (imprimir score1 score2 )
  (printf "\n\n\n\n\n\n\n")
  (printf "        JUGADORES            SCORE\n")
  (printf "        ~a               ~a\n" primerJugador score1)
  (printf "        ~a               ~a\n" segundoJugador score2)
  (printf "            ___ ___ ___ ___ ___ ___ ___ ___\n")
  (printf "           |_~a_|_~a_|_~a_|_~a_|_~a_|_~a_|_~a_|_~a_|\n\n\n\n\n" (string-ref cadenaJugador 1) (string-ref cadenaJugador 2) (string-ref cadenaJugador 3) (string-ref cadenaJugador 4) (string-ref cadenaJugador 5) (string-ref cadenaJugador 6) (string-ref cadenaJugador 7) (string-ref cadenaJugador 8) )
 )

(define (verificar x y turno score1 score2)
  (if (equal? caracter cadenaJugador)
      (if (> (- score1 score2) 0 ) (printf "Gano ~a" primerJugador ) (printf "Gano ~a" segundoJugador) )
      (if (equal? (string-ref cadenaJugador x) (string-ref cadenaJugador y))
          (begin
            (if (equal? turno primerJugador)
                (juego primerJugador (add1 score1) score2)
                (juego segundoJugador score1 (add1 score2))
            )
           )
          (begin
            (string-set! cadenaJugador y  (integer->char (+ y 48)))
            (string-set! cadenaJugador x  (integer->char (+ x 48)))
            (if (equal? turno primerJugador)
                (juego segundoJugador score1 score2)
                (juego primerJugador score1 score2)
            )
           )

      )
  )
)

(define (modificarC2  x y turno score1 score2)
  (if (= x y)
      (begin
        (printf "Haz ingresdo la misma casilla sigues de pendejo y te saco:")
        (modificarC2 x (read))
       )
      (begin
        (string-set! cadenaJugador y  (string-ref caracter y))
        (imprimir score1 score2)
        (sleep 1.5)
        (verificar x y turno score1 score2)
       )
   )
)

(define (modificarC1  x turno score1 score2)
  (string-set! cadenaJugador x  (string-ref caracter x))
  (imprimir score1 score2)
  (printf "~a ingresa tu segunda casilla!: " turno)
  (modificarC2   x (read) turno score1 score2 )
)


(define (juego turno score1 score2 )
  (printf "\n\n\n\n\n\n\n\n\n\n                   Turno de :~a PUNTAJE: JUGADOR 1:~a \n" turno score1 )
  (printf "                                               JUGADOR 2:~a \n" score2)
  (imprimir score1 score2)
  (printf "~a ingresa tu primer casilla!: " turno)
  (modificarC1 (read) turno score1 score2)
)

(juego primerJugador 0 0)