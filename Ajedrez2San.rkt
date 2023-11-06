#lang racket
(require graphics/graphics)
(open-graphics)

(define ventana (open-viewport "ajedrez" 1000 640 ))
((draw-solid-rectangle ventana) (make-posn 0 0 ) 1000 640 "black")

(define colorCasilla "XBNBNBNBNNBNBNBNBBNBNBNBNNBNBNBNBBNBNBNBNNBNBNBNBBNBNBNBNNBNBNBNB")

(define tablero (string-copy (string-append "!CBDFEDBCAAAAAAAA"(make-string 32 #\space)"1111111132465423" )))

(define (vincularFicha x)
  (cond
    [ ( char=? (string-ref tablero x ) #\C ) "Torre negro.png" ] [ ( char=? (string-ref tablero x ) #\B ) "Caballo negro.png" ]
    [ ( char=? (string-ref tablero x ) #\D ) "Alfil negro.png" ] [ ( char=? (string-ref tablero x ) #\F ) "Reina negro.png" ]
    [ ( char=? (string-ref tablero x ) #\E ) "Rey negro.png" ]   [ ( char=? (string-ref tablero x ) #\A ) "Peon negro.png"]
    [ ( char=? (string-ref tablero x ) #\1 ) "Peon blanco.png"] [ ( char=? (string-ref tablero x ) #\2 ) "Caballo blanco.png"]
    [ ( char=? (string-ref tablero x ) #\3 ) "Torre blanco.png" ]  [ ( char=? (string-ref tablero x ) #\4 ) "Alfil blanco.png"]
    [ ( char=? (string-ref tablero x ) #\5 ) "Rey blanco.png"]  [ ( char=? (string-ref tablero x ) #\6 ) "Reina blanco.png"]
    )
)

(define (dibujarCuadros i x y )
  (if (= i 65)
      (void)
      (if  ( = (remainder i 8) 0)
          (begin
            (  (draw-solid-rectangle ventana) (make-posn x y) 80 80  (if (char=? (string-ref colorCasilla i) #\B) "beige" "brown") )
            (if (char=? (string-ref tablero i) #\space ) (void ) ( ( (draw-pixmap-posn (vincularFicha i) ) ventana) (make-posn x y ) )   )
            (dibujarCuadros (add1 i) 0 (+ y 80) )
           )
          (begin
            (  (draw-solid-rectangle ventana) (make-posn x y) 80 80  (if (char=? (string-ref colorCasilla i) #\B) "beige" "brown") )
            (if (char=? (string-ref tablero i) #\space ) (void ) ( ( (draw-pixmap-posn (vincularFicha i) ) ventana) (make-posn x y ) )   )
            (dibujarCuadros (add1 i) (+ x 80) y )
           )         
       )
  )
)

(define (colocarFicha i casillaADibujar x y  posInicial )
  (if (= i casillaADibujar)
      (begin
        (string-set! tablero casillaADibujar (string-ref tablero posInicial))
        ( ( (draw-pixmap-posn (vincularFicha posInicial ) ) ventana) (make-posn x y ) )
       )
      (if  ( = (remainder i 8) 0)
          (colocarFicha (add1 i) casillaADibujar 0 (+ y 80) posInicial )
          (colocarFicha (add1 i) casillaADibujar (+ x 80) y  posInicial)         
       )
  )
)

(define (colocarTapa i casillaADibujar x y )
  (if (= i casillaADibujar)
      (begin
        (string-set! tablero casillaADibujar #\space)
        ( (draw-solid-rectangle ventana) (make-posn x y) 80 80  (if (char=? (string-ref colorCasilla i) #\B) "beige" "brown")  )
       )  
      (if  ( = (remainder i 8) 0)
          (colocarTapa (add1 i) casillaADibujar 0 (+ y 80) )
          (colocarTapa (add1 i) casillaADibujar (+ x 80) y )    
       )
  )
)

(dibujarCuadros 1 0 0 )

(define (conocerFicha x)
  ( +  (add1 ( quotient (posn-x (mouse-click-posn x)) 80 ) )   (* ( quotient (posn-y (mouse-click-posn x)) 80 ) 8)  )
)

(define (detectarBordes x)
 (cond
   [  ( =   (remainder (- x 1) 8) 0  ) 1 ]
   [  ( =   (remainder (- x 2) 8) 0  ) 2 ]
   [  ( =   (remainder (+ x 1) 8) 0  ) 3 ]
   [  ( =   (remainder x 8) 0  ) 4 ]
   (else 0)
 ) 
)

(define (peon posInicial  signo)
  (if (equal? signo "suma")
      (peon2 (conocerFicha posInicial) (conocerFicha (get-mouse-click ventana)) +  (if (= ( quotient (posn-y (mouse-click-posn posInicial )) 80 ) 1) #t #f ) #\A )
      (peon2 (conocerFicha posInicial) (conocerFicha (get-mouse-click ventana)) -  (if (= ( quotient (posn-y (mouse-click-posn posInicial )) 80 ) 6) #t #f ) #\1 )
   )
)

(define fBlancas "123456E " )
(define fNegras "ABCDEF5 ")

(define (PuedoComer? fInical fEnemiga)
  ( if (string-contains? fBlancas (~a (string-ref tablero fInical ) ))
       (if  (string-contains? fBlancas (~a (string-ref tablero fEnemiga ) )) #f #t   )
       (if  (string-contains? fBlancas (~a (string-ref tablero fEnemiga ) )) #t #f   )
   )

)

(define (peon2 posInicial posFinal signo fTime ficha )
  [cond
    [ (and fTime (=  posFinal (signo posInicial 16) ) (char=? (string-ref tablero (signo posInicial 8) ) #\space ) (char=? (string-ref tablero posFinal) #\space ) ) (colocarFicha 1 posFinal 0 0 posInicial ) (colocarTapa 1 posInicial 0 0 )  ]
    [ (and (=  posFinal (signo posInicial 8) ) (char=? (string-ref tablero posFinal) #\space ) ) (colocarFicha 1 posFinal 0 0 posInicial) (colocarTapa 1 posInicial 0 0 )  ]
    [  (and  (PuedoComer? posInicial posFinal) (char=? ficha #\1)  (= (detectarBordes posInicial ) 1) (=  posFinal (signo posInicial 7) )  ) (colocarTapa 1 posFinal 0 0 ) (colocarFicha 1 posFinal 0 0 posInicial) (colocarTapa 1 posInicial 0 0 )  ]
    [  (and  (PuedoComer? posInicial posFinal) (char=? ficha #\1) (= (detectarBordes posInicial ) 4) (=  posFinal (signo posInicial 9) )  ) (colocarTapa 1 posFinal 0 0 ) (colocarFicha 1 posFinal 0 0 posInicial) (colocarTapa 1 posInicial 0 0 )  ]    
    [  (and  (PuedoComer? posInicial posFinal) (char=? ficha #\A) (= (detectarBordes posInicial ) 4) (=  posFinal (signo posInicial 7) )  ) (colocarTapa 1 posFinal 0 0 ) (colocarFicha 1 posFinal 0 0 posInicial) (colocarTapa 1 posInicial 0 0 )  ]
    [  (and  (PuedoComer? posInicial posFinal) (char=? ficha #\A) (= (detectarBordes posInicial ) 1) (=  posFinal (signo posInicial 9) )  ) (colocarTapa 1 posFinal 0 0 ) (colocarFicha 1 posFinal 0 0 posInicial) (colocarTapa 1 posInicial 0 0 )  ]
    [  (and (PuedoComer? posInicial posFinal) (and (not (= (detectarBordes posInicial ) 1)) (not (= (detectarBordes posInicial ) 4)) ) (=  posFinal (signo posInicial 9) ) ) (colocarTapa 1 posFinal 0 0 ) (colocarFicha 1 posFinal 0 0 posInicial) (colocarTapa 1 posInicial 0 0 ) ]
    [  (and (PuedoComer? posInicial posFinal) (and (not (= (detectarBordes posInicial ) 1)) (not (= (detectarBordes posInicial ) 4)) ) (=  posFinal (signo posInicial 7) ) ) (colocarTapa 1 posFinal 0 0 ) (colocarFicha 1 posFinal 0 0 posInicial) (colocarTapa 1 posInicial 0 0 ) ]
    (else (displayln  "movimiento invalido" ))
  ]
)


(define (torre posInicial )
  (torre2 posInicial (get-mouse-click ventana) )
)

(define (validarTorre posInicial posArranque posFinal signo relevo)
  (cond
    [ (not ( (integer-in 1 64)  posArranque ) )  #f ]
    [ (and  (= posArranque posFinal )  (or (char=? (string-ref tablero posFinal) #\space)  (PuedoComer? posInicial posFinal) ) )   #t  ]
    [ ( char=?  (string-ref tablero posArranque) #\space  )  (validarTorre posInicial (signo posArranque relevo) posFinal signo relevo)     ]
    [ else #f]
   )
)

(define (moverFicha posFinal posInicial)
  (colocarFicha 1 (conocerFicha posFinal) 0 0 (conocerFicha posInicial)) (colocarTapa 1 (conocerFicha posInicial) 0 0 )
)

(define (compararFilas posInicial posFinal) (= (quotient (posn-y (mouse-click-posn posInicial)) 80) (quotient (posn-y (mouse-click-posn posFinal)) 80) ) )

(define (torre2 posInicial posFinal )
  (cond
    [ (and  (> (conocerFicha posInicial) (conocerFicha posFinal) ) (validarTorre (conocerFicha posInicial) (- (conocerFicha posInicial) 8) (conocerFicha posFinal) - 8)) (moverFicha posFinal posInicial )   ]
    [ (and  (< (conocerFicha posInicial) (conocerFicha posFinal) ) (validarTorre (conocerFicha posInicial) (+ (conocerFicha posInicial) 8) (conocerFicha posFinal) + 8))  (moverFicha posFinal posInicial  )   ]
    [ (and  (> (conocerFicha posInicial) (conocerFicha posFinal) ) (compararFilas posInicial posFinal) (validarTorre (conocerFicha posInicial) (- (conocerFicha posInicial) 1) (conocerFicha posFinal) - 1)  ) (moverFicha posFinal posInicial ) ]
    [ (and  (< (conocerFicha posInicial) (conocerFicha posFinal) ) (compararFilas posInicial posFinal) (validarTorre (conocerFicha posInicial) (+ (conocerFicha posInicial) 1) (conocerFicha posFinal) + 1)  ) (moverFicha posFinal posInicial )]
    [ (= (conocerFicha posInicial) (conocerFicha posFinal) ) (printf "perdio turno" ) ]
   )
)

(define (darMovimiento posicion )
  (cond
    [ (char=? (string-ref tablero (conocerFicha posicion) ) #\1) (peon posicion  "resta")  ]
    [ (char=? (string-ref tablero (conocerFicha posicion)) #\A) (peon posicion  "suma") ]
    [ (or (char=? (string-ref tablero (conocerFicha posicion)) #\C) (char=? (string-ref tablero (conocerFicha posicion)) #\3)) (torre posicion )  ]
    (else (printf "no es identificado \n"))
   )
)

(define (juego)
  (darMovimiento  (get-mouse-click ventana)  )
  (juego)
)

(juego)