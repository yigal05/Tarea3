#lang racket
(require graphics/graphics)
(open-graphics)
(define ventana (open-viewport "ajedrez" 880 640  ))
((draw-solid-rectangle ventana) (make-posn 0 0 ) 920 640 "black")
(define colorCasilla "XBNBNBNBNNBNBNBNBBNBNBNBNNBNBNBNBBNBNBNBNNBNBNBNBBNBNBNBNNBNBNBNB")
(define tablero (string-copy (string-append "!TCADRACTPPPPPPPP"(make-string 32 #\space)"pppppppptcadract" )))

(define (vincularFicha x)
  (cond
    [ ( char=? (string-ref tablero x ) #\T ) "Torre negro.png" ] [ ( char=? (string-ref tablero x ) #\C ) "Caballo negro.png" ]
    [ ( char=? (string-ref tablero x ) #\A ) "Alfil negro.png" ] [ ( char=? (string-ref tablero x ) #\D ) "Reina negro.png" ]
    [ ( char=? (string-ref tablero x ) #\R ) "Rey negro.png" ]   [ ( char=? (string-ref tablero x ) #\P ) "Peon negro.png"]
    [ ( char=? (string-ref tablero x ) #\p ) "Peon blanco.png"] [ ( char=? (string-ref tablero x ) #\c ) "Caballo blanco.png"]
    [ ( char=? (string-ref tablero x ) #\t ) "Torre blanco.png" ]  [ ( char=? (string-ref tablero x ) #\a ) "Alfil blanco.png"]
    [ ( char=? (string-ref tablero x ) #\r ) "Rey blanco.png"]  [ ( char=? (string-ref tablero x ) #\d ) "Reina blanco.png"]
    )
)

(define (vincularCoronacion x str)
  (cond
    [ ( char=? (string-ref str x ) #\T ) "Torre negro.png" ] [ ( char=? (string-ref str x ) #\C ) "Caballo negro.png" ] [ ( char=? (string-ref str x ) #\A ) "Alfil negro.png" ]
    [ ( char=? (string-ref str x ) #\D ) "Reina negro.png" ] [ ( char=? (string-ref str x ) #\c ) "Caballo blanco.png"] [ ( char=? (string-ref str x ) #\t ) "Torre blanco.png" ]
    [ ( char=? (string-ref str x ) #\a ) "Alfil blanco.png"] [ ( char=? (string-ref str x ) #\d ) "Reina blanco.png"]
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

(define (coronarFicha i casillaADibujar x y  indicador str)
  (if (= i casillaADibujar)
      (begin
        (string-set! tablero casillaADibujar (string-ref str indicador))
        ( ( (draw-pixmap-posn (vincularCoronacion indicador str) ) ventana) (make-posn x y ) )
       )
      (if  ( = (remainder i 8) 0)
          (coronarFicha (add1 i) casillaADibujar 0 (+ y 80) indicador str)
          (coronarFicha (add1 i) casillaADibujar (+ x 80) y  indicador str)         
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
   [  ( =   (remainder    x    8) 0  ) 4 ]
   (else 0)
 ) 
)

(define fBlancas "pcatdrR " )
(define fNegras "PCATDRr ")

(define (PuedoComer? fInical fEnemiga)
  ( if (and (string-contains? fBlancas (~a (string-ref tablero fInical ) ))  (not (char=? (string-ref tablero fInical ) #\R))  )
       (if  (string-contains? fBlancas (~a (string-ref tablero fEnemiga ) )) #f #t   )
       (if  (string-contains? fNegras (~a (string-ref tablero fEnemiga ) )) #f #t   )
   )

)

(define (tapar)
  ( (draw-solid-rectangle ventana) (make-posn 680 240) 160 220  "black" )
)


(define (posCoronacion fichaElegida str posFinal)
  (cond
    [ (and ( (integer-in 680 760 ) (posn-x fichaElegida) )  ( (integer-in 240 320 ) (posn-y fichaElegida) ) ) (colocarTapa 1 posFinal 0 0 ) (coronarFicha 1 posFinal 0 0  1 str) (tapar)]
    [ (and ( (integer-in 761 840 ) (posn-x fichaElegida) )  ( (integer-in 240 320)  (posn-y fichaElegida) ) ) (colocarTapa 1 posFinal 0 0 ) (coronarFicha 1 posFinal 0 0  2 str) (tapar)]
    [ (and ( (integer-in 680 760 ) (posn-x fichaElegida) )  ( (integer-in 321 400)  (posn-y fichaElegida) ) ) (colocarTapa 1 posFinal 0 0 ) (coronarFicha 1 posFinal 0 0  3 str) (tapar)]
    [ (and ( (integer-in 761 840 ) (posn-x fichaElegida) )  ( (integer-in 321 400)  (posn-y fichaElegida) ) ) (colocarTapa 1 posFinal 0 0 ) (coronarFicha 1 posFinal 0 0  4 str) (tapar)]
    [ (and ( (integer-in 680 840 ) (posn-x fichaElegida) )  ( (integer-in 420 460)  (posn-y fichaElegida) ) ) (tapar) ]
    ( else (posCoronacion (mouse-click-posn (get-mouse-click ventana)) str posFinal ) )
    )
)

(define (crearCoronacion i x y xI str posFinal)
  (if (= i 5)
      (begin ( (draw-solid-rectangle ventana) (make-posn x (+ y 20)) 160 40  "brown" ) (posCoronacion (mouse-click-posn (get-mouse-click ventana)) str posFinal) )
      (if  ( = (remainder i 2) 0)
          (begin
            ( (draw-solid-rectangle ventana) (make-posn x y) 80 80  "beige" )
            ( ( (draw-pixmap-posn (vincularCoronacion i str) ) ventana) (make-posn x y ) )
            (crearCoronacion (add1 i) xI (+ y 80) xI str posFinal )
           )
          (begin
            (  (draw-solid-rectangle ventana) (make-posn x y) 80 80  "beige" )
            ( ( (draw-pixmap-posn (vincularCoronacion i str) ) ventana) (make-posn x y ) )
            (crearCoronacion (add1 i) (+ x 80) y xI str posFinal)
           )         
       )
  )
)

(define (peon posInicial  signo)
  (if (equal? signo "suma")
      (peon2 (conocerFicha posInicial) (conocerFicha (get-mouse-click ventana)) +  (if (= ( quotient (posn-y (mouse-click-posn posInicial )) 80 ) 1) #t #f ) #\P (estoyDefendiendo? (conocerFicha posInicial) ) )
      (peon2 (conocerFicha posInicial) (conocerFicha (get-mouse-click ventana)) -  (if (= ( quotient (posn-y (mouse-click-posn posInicial )) 80 ) 6) #t #f ) #\p (estoyDefendiendo? (conocerFicha posInicial) ) )
   )
)

(define (peon2 posInicial posFinal signo fTime ficha restriccion)
  [cond
    [ (and fTime (=  posFinal (signo posInicial 16) ) (char=? (string-ref tablero (signo posInicial 8) ) #\space ) (char=? (string-ref tablero posFinal) #\space ) ) (colocarFicha 1 posFinal 0 0 posInicial ) (colocarTapa 1 posInicial 0 0 ) (coronacion? posFinal) ]
    [ (and (or (= restriccion 0) (= restriccion 1)) (= posFinal (signo posInicial 8) ) (char=? (string-ref tablero posFinal) #\space ) ) (colocarFicha 1 posFinal 0 0 posInicial) (colocarTapa 1 posInicial 0 0 ) (coronacion? posFinal) ]

    [  (and (= restriccion 0) (PuedoComer? posInicial posFinal) (char=? ficha #\p) (= (detectarBordes posInicial ) 1) (=  posFinal (signo posInicial 7) )  ) (colocarTapa 1 posFinal 0 0 ) (colocarFicha 1 posFinal 0 0 posInicial) (colocarTapa 1 posInicial 0 0 ) (coronacion? posFinal) ]
    [  (and (= restriccion 0) (PuedoComer? posInicial posFinal) (char=? ficha #\p) (= (detectarBordes posInicial ) 4) (=  posFinal (signo posInicial 9) )  ) (colocarTapa 1 posFinal 0 0 ) (colocarFicha 1 posFinal 0 0 posInicial) (colocarTapa 1 posInicial 0 0 ) (coronacion? posFinal) ]    
    [  (and (= restriccion 0) (PuedoComer? posInicial posFinal) (char=? ficha #\P) (= (detectarBordes posInicial ) 4) (=  posFinal (signo posInicial 7) )  ) (colocarTapa 1 posFinal 0 0 ) (colocarFicha 1 posFinal 0 0 posInicial) (colocarTapa 1 posInicial 0 0 ) (coronacion? posFinal) ]
    [  (and (= restriccion 0) (PuedoComer? posInicial posFinal) (char=? ficha #\P) (= (detectarBordes posInicial ) 1) (=  posFinal (signo posInicial 9) )  ) (colocarTapa 1 posFinal 0 0 ) (colocarFicha 1 posFinal 0 0 posInicial) (colocarTapa 1 posInicial 0 0 ) (coronacion? posFinal) ]
    [  (and (or (= restriccion 0) (= restriccion 4)) (PuedoComer? posInicial posFinal) (and (not (= (detectarBordes posInicial ) 1)) (not (= (detectarBordes posInicial ) 4)) ) (=  posFinal (signo posInicial 9) ) ) (colocarTapa 1 posFinal 0 0 ) (colocarFicha 1 posFinal 0 0 posInicial) (colocarTapa 1 posInicial 0 0 ) (coronacion? posFinal) ]
    [  (and (or (= restriccion 0) (= restriccion 3)) (PuedoComer? posInicial posFinal) (and (not (= (detectarBordes posInicial ) 1)) (not (= (detectarBordes posInicial ) 4)) ) (=  posFinal (signo posInicial 7) ) ) (colocarTapa 1 posFinal 0 0 ) (colocarFicha 1 posFinal 0 0 posInicial) (colocarTapa 1 posInicial 0 0 ) (coronacion? posFinal) ]
  ]
)

(define (coronacion? posFinal)
  (cond
    [ (and ( (integer-in 1 8) posFinal ) )   (crearCoronacion 1 680 240 680 "!tcad" posFinal) ] 
    [ (and ( (integer-in 57 64) posFinal ) )   (crearCoronacion 1 680 240 680 "!TCAD" posFinal )] 
    [ else (void)]
    )
)

(define (torre posInicial )
  (torre2 posInicial (get-mouse-click ventana) (estoyDefendiendo? (conocerFicha posInicial) ) )
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
 (colocarTapa 1 (conocerFicha posFinal) 0 0 )  (colocarFicha 1 (conocerFicha posFinal) 0 0 (conocerFicha posInicial)) (colocarTapa 1 (conocerFicha posInicial) 0 0 )
)

(define (compararFilas posInicial posFinal) (= (quotient (posn-y (mouse-click-posn posInicial)) 80) (quotient (posn-y (mouse-click-posn posFinal)) 80) ) )

(define (torre2 posInicial posFinal restriccion)
  (cond
    [ (and (or (= restriccion 0) (= restriccion 1))  (> (conocerFicha posInicial) (conocerFicha posFinal) ) (validarTorre (conocerFicha posInicial) (- (conocerFicha posInicial) 8) (conocerFicha posFinal) - 8)) (moverFicha posFinal posInicial )   ]
    [ (and (or (= restriccion 0) (= restriccion 1)) (< (conocerFicha posInicial) (conocerFicha posFinal) ) (validarTorre (conocerFicha posInicial) (+ (conocerFicha posInicial) 8) (conocerFicha posFinal) + 8))  (moverFicha posFinal posInicial  )   ]
    [ (and (or (= restriccion 0) (= restriccion 2)) (> (conocerFicha posInicial) (conocerFicha posFinal) ) (compararFilas posInicial posFinal) (validarTorre (conocerFicha posInicial) (- (conocerFicha posInicial) 1) (conocerFicha posFinal) - 1)  ) (moverFicha posFinal posInicial ) ]
    [ (and (or (= restriccion 0) (= restriccion 2)) (< (conocerFicha posInicial) (conocerFicha posFinal) ) (compararFilas posInicial posFinal) (validarTorre (conocerFicha posInicial) (+ (conocerFicha posInicial) 1) (conocerFicha posFinal) + 1)  ) (moverFicha posFinal posInicial )]
   )
)

(define (validarAlfil posInicial posArranque posFinal signo relevo)
  (cond
    [ (not ( (integer-in 1 64)  posArranque ) )  #f ]
    [ (and  (= posArranque posFinal )  (or (char=? (string-ref tablero posFinal) #\space)  (PuedoComer? posInicial posFinal) ) )   #t  ]
    [ (orilla? posArranque  ) #f]
    [ ( char=?  (string-ref tablero posArranque) #\space  )  (validarAlfil posInicial (signo posArranque relevo) posFinal signo relevo)     ]
    [ else #f]
   )
)


(define (orilla? x  )
  (if (or (= (detectarBordes x ) 1) (= (detectarBordes x) 4))  #t #f)
)

(define (alfil posInicial)
  
  (alfil2 posInicial  (get-mouse-click ventana) (estoyDefendiendo? (conocerFicha posInicial) ) )

)

(define (alfil2 posInicial posFinal restriccion )
  (cond
    [ (and (or (= restriccion 0) (= restriccion 4))  (not (= (detectarBordes (conocerFicha posInicial)  ) 4) ) (> (conocerFicha posInicial) (conocerFicha posFinal) ) (validarAlfil (conocerFicha posInicial) (- (conocerFicha posInicial) 7) (conocerFicha posFinal) - 7)) (moverFicha posFinal posInicial ) ]
    [ (and (or (= restriccion 0) (= restriccion 4))  (not (= (detectarBordes (conocerFicha posInicial) )  1) ) (< (conocerFicha posInicial) (conocerFicha posFinal) ) (validarAlfil (conocerFicha posInicial) (+ (conocerFicha posInicial) 7) (conocerFicha posFinal) + 7)) (moverFicha posFinal posInicial ) ]
    [ (and (or (= restriccion 0) (= restriccion 3))  (not (= (detectarBordes (conocerFicha posInicial) )  1) ) (> (conocerFicha posInicial) (conocerFicha posFinal) ) (validarAlfil (conocerFicha posInicial) (- (conocerFicha posInicial) 9) (conocerFicha posFinal) - 9)) (moverFicha posFinal posInicial ) ]
    [ (and (or (= restriccion 0) (= restriccion 3))  (not (= (detectarBordes (conocerFicha posInicial) )  4) ) (< (conocerFicha posInicial) (conocerFicha posFinal) ) (validarAlfil (conocerFicha posInicial) (+ (conocerFicha posInicial) 9) (conocerFicha posFinal) + 9)) (moverFicha posFinal posInicial ) ]
   )

)

(define (reina posInicial)
  (reina2 posInicial  (get-mouse-click ventana) (estoyDefendiendo? (conocerFicha posInicial) ) )

)

(define (reina2 posInicial posFinal restriccion)
  (cond
    [ (and (or (= restriccion 0) (= restriccion 4)) (not (= (detectarBordes (conocerFicha posInicial) ) 4) ) (> (conocerFicha posInicial) (conocerFicha posFinal) ) (validarAlfil (conocerFicha posInicial) (- (conocerFicha posInicial) 7) (conocerFicha posFinal) - 7)) (moverFicha posFinal posInicial )   ]
    [ (and (or (= restriccion 0) (= restriccion 4)) (not (= (detectarBordes (conocerFicha posInicial) ) 1) ) (< (conocerFicha posInicial) (conocerFicha posFinal) ) (validarAlfil (conocerFicha posInicial) (+ (conocerFicha posInicial) 7) (conocerFicha posFinal) + 7)) (moverFicha posFinal posInicial  )   ]
    [ (and (or (= restriccion 0) (= restriccion 3)) (not (= (detectarBordes (conocerFicha posInicial) ) 1) ) (> (conocerFicha posInicial) (conocerFicha posFinal) ) (validarAlfil (conocerFicha posInicial) (- (conocerFicha posInicial) 9) (conocerFicha posFinal) - 9)) (moverFicha posFinal posInicial ) ]
    [ (and (or (= restriccion 0) (= restriccion 3)) (not (= (detectarBordes (conocerFicha posInicial) ) 4) ) (< (conocerFicha posInicial) (conocerFicha posFinal) ) (validarAlfil (conocerFicha posInicial) (+ (conocerFicha posInicial) 9) (conocerFicha posFinal) + 9)) (moverFicha posFinal posInicial )]
    [ (and (or (= restriccion 0) (= restriccion 1)) (> (conocerFicha posInicial) (conocerFicha posFinal) ) (validarTorre (conocerFicha posInicial) (- (conocerFicha posInicial) 8) (conocerFicha posFinal) - 8)) (moverFicha posFinal posInicial )   ]
    [ (and (or (= restriccion 0) (= restriccion 1)) (< (conocerFicha posInicial) (conocerFicha posFinal) ) (validarTorre (conocerFicha posInicial) (+ (conocerFicha posInicial) 8) (conocerFicha posFinal) + 8))  (moverFicha posFinal posInicial  )   ]
    [ (and (or (= restriccion 0) (= restriccion 2)) (> (conocerFicha posInicial) (conocerFicha posFinal) ) (compararFilas posInicial posFinal) (validarTorre (conocerFicha posInicial) (- (conocerFicha posInicial) 1) (conocerFicha posFinal) - 1)  ) (moverFicha posFinal posInicial ) ]
    [ (and (or (= restriccion 0) (= restriccion 2)) (< (conocerFicha posInicial) (conocerFicha posFinal) ) (compararFilas posInicial posFinal) (validarTorre (conocerFicha posInicial) (+ (conocerFicha posInicial) 1) (conocerFicha posFinal) + 1)  ) (moverFicha posFinal posInicial )]
    )

)

(define (caballo posInicial)
  (caballo2 posInicial  (get-mouse-click ventana) (estoyDefendiendo? (conocerFicha posInicial) )  )

)

(define (validarMov posInicial posFinal)
  (or (char=? (string-ref tablero posFinal ) #\space) (PuedoComer? posInicial posFinal  ))
)

(define (moveCaballo borde posInicial posFinal )
 (cond
  [ (and (validarMov posInicial posFinal) (= borde 1 ) (or (= (- posInicial 15) posFinal) (= (+ posInicial 10) posFinal) (= (- posInicial 6) posFinal) (= (+ posInicial 17) posFinal) ) ) #t ]
  [ (and (validarMov posInicial posFinal)(= borde 2 ) (or (= (+ posInicial 10) posFinal) (= (- posInicial 6) posFinal)  (= (- posInicial 17) posFinal) (= (+ posInicial 17) posFinal) (= (+ posInicial 15) posFinal) (= (- posInicial 15) posFinal) ) ) #t]
  [ (and (validarMov posInicial posFinal) (= borde 3 ) (or (= (- posInicial 10) posFinal) (= (+ posInicial 6) posFinal)  (= (- posInicial 17) posFinal) (= (+ posInicial 17) posFinal) (= (+ posInicial 15) posFinal) (= (- posInicial 15) posFinal) ) )  #t]
  [ (and (validarMov posInicial posFinal) (= borde 4 ) (or (= (+ posInicial 15) posFinal) (= (- posInicial 10) posFinal) (= (+ posInicial 6) posFinal) (= (- posInicial 17) posFinal) ) ) #t]
  [ (and (validarMov posInicial posFinal) (= borde 0 ) (or (= (- posInicial 17) posFinal) (= (+ posInicial 17) posFinal) (= (+ posInicial 6) posFinal) (= (- posInicial 6) posFinal)
                                                   (= (- posInicial 15) posFinal) (= (+ posInicial 15) posFinal) (= (- posInicial 10) posFinal) (= (+ posInicial 10) posFinal)) ) #t ]
  (else #f)
  )
)

(define (caballo2 posInicial posFinal restriccion)
  (cond
    [ (and (= restriccion 0) (= (detectarBordes (conocerFicha posInicial) ) 1) (moveCaballo 1 (conocerFicha posInicial) (conocerFicha posFinal) )  ) (moverFicha posFinal posInicial  )  ]
    [ (and (= restriccion 0) (= (detectarBordes (conocerFicha posInicial) ) 2) (moveCaballo 2 (conocerFicha posInicial) (conocerFicha posFinal) ) )  (moverFicha posFinal posInicial  )   ]
    [ (and (= restriccion 0) (= (detectarBordes (conocerFicha posInicial) ) 3) (moveCaballo 3 (conocerFicha posInicial) (conocerFicha posFinal) ) )  (moverFicha posFinal posInicial ) ]
    [ (and (= restriccion 0) (= (detectarBordes (conocerFicha posInicial) ) 4) (moveCaballo 4 (conocerFicha posInicial) (conocerFicha posFinal) ) )  (moverFicha posFinal posInicial )]
    [ (and (= restriccion 0) (= (detectarBordes (conocerFicha posInicial) ) 0 )(moveCaballo 0 (conocerFicha posInicial) (conocerFicha posFinal) ) )  (moverFicha posFinal posInicial )   ]
    )

)

(define (rey posInicial)
  (rey2 posInicial (get-mouse-click ventana) )
  
)

(define (moveRey borde posInicial posFinal)
 (cond
  [ (and (revisarCasilla posFinal) (validarMov posInicial posFinal) (= borde 1) (or  (= (- posInicial 8) posFinal) (= (+ posInicial 8) posFinal) (= (+ posInicial 1) posFinal) (= (- posInicial 7) posFinal) (= (+ posInicial 9) posFinal) )) #t]
  [ (and (revisarCasilla posFinal) (validarMov posInicial posFinal) (= borde 4)) (or  (= (- posInicial 8) posFinal) (= (+ posInicial 8) posFinal) (= (+ posInicial 1) posFinal) (= (+ posInicial 7) posFinal) (= (- posInicial 9) posFinal) ) #t ]
  [ (and (revisarCasilla posFinal) (validarMov posInicial posFinal) (and (not (= (detectarBordes posInicial ) 1)) (not (= (detectarBordes posInicial ) 4)) )(or  (= (- posInicial 8) posFinal) (= (+ posInicial 8) posFinal) (= (+ posInicial 1) posFinal) (= (- posInicial 1) posFinal) (= (+ posInicial 7) posFinal) (= (- posInicial 9) posFinal) (= (- posInicial 7) posFinal) (= (+ posInicial 9) posFinal)) )  #t]
  [ else #f]
 )
)

(define (rey2 posInicial posFinal)
  (cond
    [ (and (= (detectarBordes (conocerFicha posInicial) ) 1) (moveRey 1 (conocerFicha posInicial) (conocerFicha posFinal)) )  (moverFicha posFinal posInicial  ) (actualizarPos  (~a (conocerFicha posFinal)))  ]
    [ (and (= (detectarBordes (conocerFicha posInicial) ) 4) (moveRey 4 (conocerFicha posInicial) (conocerFicha posFinal)) )  (moverFicha posFinal posInicial  ) (actualizarPos  (~a (conocerFicha posFinal)))  ]
    [ (and (moveRey (detectarBordes (conocerFicha posInicial) ) (conocerFicha posInicial) (conocerFicha posFinal)) )  (moverFicha posFinal posInicial  ) (actualizarPos  (~a (conocerFicha posFinal)))   ]
  )
)

(define (darMovimiento posicion )
  (cond
    [ (char=? (string-ref tablero (conocerFicha posicion) ) #\p) (peon posicion  "resta")  ]
    [ (char=? (string-ref tablero (conocerFicha posicion)) #\P) (peon posicion  "suma") ]
    [ (or (char=? (string-ref tablero (conocerFicha posicion)) #\T) (char=? (string-ref tablero (conocerFicha posicion)) #\t)) (torre posicion ) ]
    [ (or (char=? (string-ref tablero (conocerFicha posicion)) #\A) (char=? (string-ref tablero (conocerFicha posicion)) #\a)) (alfil posicion ) ]
    [ (or (char=? (string-ref tablero (conocerFicha posicion)) #\D) (char=? (string-ref tablero (conocerFicha posicion)) #\d)) (reina posicion) ]
    [ (or (char=? (string-ref tablero (conocerFicha posicion)) #\C) (char=? (string-ref tablero (conocerFicha posicion)) #\c)) (caballo posicion) ]
    [ (or (char=? (string-ref tablero (conocerFicha posicion)) #\R) (char=? (string-ref tablero (conocerFicha posicion)) #\r)) (rey posicion) ]
    (else (printf "no es identificado \n"))
   )
)

(define (imprimirTablero i x)
  (cond
    [  (= i 8) (printf "\n\n\n\n"  ) ]
    [ else (printf "|~a|\n" (substring tablero x (+ x 8)) ) (imprimirTablero (add1 i) (+ x 8))  ]
    )
)


(define posReyNegro (string-copy "05") )

(define (posRey x)
  (string->number x)
)

(define (actualizarPos  x)
  (if (= (string-length x) 2)
      (begin (string-set! posReyNegro 0 (string-ref x 0)) (string-set! posReyNegro 1 (string-ref x 1)))
      (begin (string-set! posReyNegro 0 #\0) (string-set! posReyNegro 1 (string-ref x 0)))
   )

)

(define (revisarReyBlanco )
  (if (or (verificacionVertical (posRey posReyNegro) - "TCADPpacr" "td" )    (verificacionVertical (posRey posReyNegro) + "TCADPpacr" "td" )
          (verificacionHorizontal (posRey posReyNegro) - "TCADPpacr" "td"  )   (verificacionHorizontal (posRey posReyNegro) + "TCADPpacr" "td" )
          (llamarDiagonales  (posRey posReyNegro)  "TCADPptcr" "ad" )   (verificarCaballo (posRey posReyNegro) )  (verificarPeon (posRey posReyNegro) )
      )  ((draw-string ventana ) (make-posn 650 40) "JAQUE" "red") ((draw-string ventana ) (make-posn 649 40) "█████████████" "black") )
)

(define (revisarCasilla posFinal)
  (not (or (verificacionVertical posFinal - "TCADPpacr" "td" )    (verificacionVertical posFinal + "TCADPpacr" "td" )
          (verificacionHorizontal posFinal - "TCADPpacr" "td"  )   (verificacionHorizontal posFinal + "TCADPpacr" "td" )
          (llamarDiagonales  posFinal  "TCADPptcr" "ad" )   (verificarCaballo posFinal )  (verificarPeon posFinal )
      ))
 )

(define (verificacionVertical posInicio signo  bloqueo Abuscar  )
 (cond
   [ (not ((integer-in 1 64) posInicio) ) #f ]
   [ (string-contains? bloqueo (~a (string-ref tablero posInicio) )) #f  ]
   [ (string-contains? Abuscar (~a (string-ref tablero posInicio) )) #t ]
   [ else (verificacionVertical (signo posInicio 8) signo  bloqueo Abuscar  ) ]
  )
)

(define (verificacionHorizontal posInicio signo  bloqueo Abuscar )
 (cond
   [ (not ((integer-in 1 64) posInicio) ) #f ]
   [ (string-contains? bloqueo (~a (string-ref tablero posInicio) )) #f  ]
   [ (string-contains? Abuscar (~a (string-ref tablero posInicio) )) #t ]
   [ (or (= (detectarBordes posInicio) 1) (= (detectarBordes posInicio) 4 )) #f ]
   [ else (verificacionHorizontal (signo posInicio 1) signo  bloqueo Abuscar  ) ]
  )
)

(define (llamarDiagonales  posInicio  bloqueo Abuscar )
  (cond
    [  (= (detectarBordes posInicio) 1) (or (verificacionDiagonal (- posInicio 7) - 7  bloqueo Abuscar )  (verificacionDiagonal (+ posInicio 9) + 9  bloqueo Abuscar )  ) ]
    [  (= (detectarBordes posInicio) 4) (or (verificacionDiagonal (+ posInicio 7) + 7  bloqueo Abuscar )  (verificacionDiagonal (- posInicio 9) - 9  bloqueo Abuscar )  ) ]
    [  (and (not (= (detectarBordes posInicio ) 1)) (not (= (detectarBordes posInicio ) 4)) ) (or (verificacionDiagonal (- posInicio 9) - 9  bloqueo Abuscar ) (verificacionDiagonal (+ posInicio 7) + 7  bloqueo Abuscar ) (verificacionDiagonal (+ posInicio 9) + 9  bloqueo Abuscar ) (verificacionDiagonal (- posInicio 7) - 7  bloqueo Abuscar ) )  ]
  )  
 
)

(define (verificacionDiagonal posInicio signo index bloqueo Abuscar )
 (cond
   [ (not ((integer-in 1 64) posInicio) ) #f ]
   [ (string-contains? bloqueo (~a (string-ref tablero posInicio) )) #f  ]
   [ (string-contains? Abuscar (~a (string-ref tablero posInicio) )) #t ]
   [ (or (= (detectarBordes posInicio) 1) (= (detectarBordes posInicio) 4 )) #f ]
   [ else (verificacionDiagonal (signo posInicio index) signo index  bloqueo Abuscar  ) ]
  )
)

(define (rvCaballo x)
  (if ( (integer-in 1 64) x) (if (char=?  (string-ref tablero x ) #\c)  #t #f )   #f)
)

(define (verificarCaballo posInicio)
  (cond
    [ (and (= (detectarBordes posInicio) 1 ) (or  (rvCaballo (+ posInicio 17) ) (rvCaballo (+ posInicio 10)) (rvCaballo  (- posInicio 6)) (rvCaballo (- posInicio 15)) )  ) #t ]
    [ (and (= (detectarBordes posInicio) 4 ) (or  (rvCaballo (- posInicio 17) ) (rvCaballo (- posInicio 10)) (rvCaballo  (+ posInicio 6)) (rvCaballo (+ posInicio 15)) )  ) #t ]
    [ (and (= (detectarBordes posInicio) 2 ) (or  (rvCaballo (- posInicio 17) ) (rvCaballo (- posInicio 15)) (rvCaballo  (- posInicio 6)) (rvCaballo (+ posInicio 15)) (rvCaballo (+ posInicio 17)) (rvCaballo (+ posInicio 10)) )  )  #t ]
    [ (and (= (detectarBordes posInicio) 3 ) (or  (rvCaballo (- posInicio 17) ) (rvCaballo (- posInicio 15)) (rvCaballo  (+ posInicio 6)) (rvCaballo (+ posInicio 15)) (rvCaballo (+ posInicio 17)) (rvCaballo (- posInicio 10)) )  ) #t ]
    [ (and (= (detectarBordes posInicio) 0 ) (or  (rvCaballo (- posInicio 17) ) (rvCaballo (- posInicio 15)) (rvCaballo  (- posInicio 6)) (rvCaballo  (+ posInicio 6)) (rvCaballo (+ posInicio 15)) (rvCaballo (+ posInicio 17)) (rvCaballo (+ posInicio 10)) (rvCaballo (- posInicio 10)) )  ) #t ]
    (else #f)
   )
)

(define (verificarPeon posInicio)
 (cond
   [(and ( (integer-in 1 64) (+ posInicio 9 )) (= (detectarBordes posInicio) 1 ) (char=? (string-ref tablero (+ posInicio 9 )) #\p) ) #t ]
   [(and ( (integer-in 1 64) (+ posInicio 7 )) (= (detectarBordes posInicio) 4 ) (char=? (string-ref tablero (+ posInicio 7 )) #\p) ) #t ]
   [(and ( (integer-in 1 64) (+ posInicio 7 )) ( (integer-in 1 64) (+ posInicio 9 )) (not (= (detectarBordes posInicio ) 1)) (not (= (detectarBordes posInicio ) 4))  (or (char=? (string-ref tablero (+ posInicio 9 )) #\p) (char=? (string-ref tablero (+ posInicio 7 )) #\p) )  ) #t ]
   (else #f)
  )
)

(define (estoyDefendiendo? posInicio )
 (cond
   [ (verificacionVertical   (- posInicio 8) -   "TtCcAaDdPpr" "R" )  (if (not (verificacionVertical (+ posInicio 8) +   "cCaATDPpr" "dt" )) 0 1 ) ]
   [ (verificacionVertical   (+ posInicio 8) +   "TtCcAaDdPpr" "R" )  (if (not (verificacionVertical (- posInicio 8) -   "cCaATDPpr" "dt" )) 0 1 ) ]
   [ (verificacionHorizontal (- posInicio 1) -   "TtCcAaDdPpr" "R" )  (if (not (verificacionHorizontal (+ posInicio 1) + "cCaATDPpr" "dt" )) 0 2 ) ]
   [ (verificacionHorizontal (+ posInicio 1) +   "TtCcAaDdPpr" "R" )  (if (not (verificacionHorizontal (- posInicio 1) - "cCaATDPpr" "dt" )) 0 2 ) ]
   [ (verificacionDiagonal   (- posInicio 9) - 9 "TtCcAaDdPpr" "R" )  (if (not (verificacionDiagonal (+ posInicio 9) + 9 "cCATtDPpr" "da" )) 0 3 ) ]
   [ (verificacionDiagonal   (+ posInicio 9) + 9 "TtCcAaDdPpr" "R" )  (if (not (verificacionDiagonal (- posInicio 9) - 9 "cCATtDPpr" "da" )) 0 3 ) ]
   [ (verificacionDiagonal   (- posInicio 7) - 7 "TtCcAaDdPpr" "R" )  (if (not (verificacionDiagonal (+ posInicio 7) + 7 "cCATtDPpr" "da" )) 0 4 ) ]
   [ (verificacionDiagonal   (+ posInicio 7) + 7 "TtCcAaDdPpr" "R" )  (if (not (verificacionDiagonal (- posInicio 7) - 7 "cCATtDPpr" "da" )) 0 4 ) ]
   (else 0)
  )
)

(define (juego)
  (revisarReyBlanco )
  (darMovimiento  (get-mouse-click ventana)  )
  (juego)
)

(juego)