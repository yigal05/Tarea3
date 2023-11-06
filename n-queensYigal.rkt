#lang racket

(require graphics/graphics)
(open-graphics)

;(printf "ingrese n para n-queens: ")
;(define n (read))
(define n 6)

(define ventana (open-viewport "ajedrez" (* 80 n) (* 80 n) ))
((draw-solid-rectangle ventana) (make-posn 0 0 ) (* 80 n) (* 80 n) "black")

(define colorCasilla (string-copy  (make-string (add1 (sqr n)) #\B ) ))

(define tablero (string-copy  (make-string (add1 (sqr n)) #\space ) ))

(define (organizarColores i char)
  (if (= i (add1 (sqr n)) )
      (void)
      (if  ( = (remainder i n) 0)
          (begin
            (string-set! colorCasilla i (if (char=? char #\B ) #\B  #\N)  )
            (organizarColores (add1 i ) (if (char=? char #\B ) #\B #\N) )
           )
          (begin
            (string-set! colorCasilla i (if (char=? char #\B ) #\B  #\N)  )
            (organizarColores (add1 i ) (if (char=? char #\B ) #\N #\B) )
           )
         
       )
  )

)

(organizarColores 1 #\B)

(define (dibujarCuadros i  x y )
  (if (= i (add1 (sqr n)) )
      (void)
      (if  ( = (remainder i n) 0)
          (begin
            (  (draw-solid-rectangle ventana) (make-posn x y) 80 80  (if (char=? (string-ref colorCasilla i) #\B) "beige" "brown") )
            (dibujarCuadros (add1 i )  0 (+ y 80) )
           )
          (begin
            (  (draw-solid-rectangle ventana) (make-posn x y) 80 80  (if (char=? (string-ref colorCasilla i) #\B) "beige" "brown") )      
            (dibujarCuadros (add1 i)  (+ x 80) y )
           )
         
       )
  )

)

(define (conocerFicha x)
  ( +  (add1 ( quotient (posn-x (mouse-click-posn x)) 80 ) )   (* ( quotient (posn-y (mouse-click-posn x)) 80 ) n)  )
)

(define (colocarReina i obj x y )
  (if (= i obj )
      (begin
        ( ( (draw-pixmap-posn "reina negro.png" ) ventana) (make-posn x y ) )
        (string-set! tablero obj #\X )
       )
      (if  ( = (remainder i n) 0)
          (begin    
            (colocarReina (add1 i ) obj  0 (+ y 80) )
           )
          (begin    
            (colocarReina (add1 i) obj  (+ x 80) y )
           )         
       )
  )
)


(define (verticales x signo)
  (if ( (integer-in 1 (sqr n) ) x )
      (if (char=? (string-ref tablero x ) #\X )
          #f
          (verticales (signo x n) signo)
       )
       #t 
      
   )

)

(define (horizontales i posRevisar signo vecesBucle cantidad)
 ( if (= i vecesBucle)
      #t
      (if ( not (char=? (string-ref tablero posRevisar ) #\X)  )
          (horizontales (add1 i) (signo posRevisar cantidad) signo vecesBucle cantidad )
          #f
      )
 )
  
)



(define (encontrarRangos i x y cantidad)
  (if (and  (>= i x) (<= i y )    )
      (and (horizontales 0  (- i 1) - (- i x) cantidad ) (horizontales 0  (+ i 1) + (- y i) cantidad )  )    ;(- i x) (- y i)
      (encontrarRangos i (+ x n) (+ y n) cantidad )
   )

)

(define (verificar x)
  (and (verticales x +)
  (verticales x -) (encontrarRangos x 1 n 1)   )

)

(define (empezarBack i reinasColocadas )
 (if (= i (add1 (sqr n)))
     (printf "reinas colocadas : ~a\n" reinasColocadas)
     (if (= reinasColocadas n)
     (printf "ya se han colocado \n")
     (if (and (char=? (string-ref tablero i) #\space) (verificar i))
          (begin
            (colocarReina 1 i 0 0 )
            (empezarBack (add1 i) (add1 reinasColocadas) )
           )
          (empezarBack (add1 i) reinasColocadas )
      )
  )
  ) 
)

(define (juego)
  (colocarReina 1 (conocerFicha (get-mouse-click ventana)   ) 0 0 )
  (empezarBack 1 1)
)

(dibujarCuadros 1 0 0 )
(juego)