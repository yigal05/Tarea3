#lang racket

#|
la llama dentro de revisarnegron con  (verificarPeonJaque (posRey posReyNegro) #\p +)
(define (verificarPeonJaque posInicio pA signo) ;pA = peonABuscar
 (cond
   [(and ( (integer-in 1 64) (signo posInicio 9 )) (= (detectarBordes posInicio) 1 ) (char=? (string-ref tablero (+ posInicio 9 )) pA) ) (jCaballo (signo posInicio 9 ) ) ]
   [(and ( (integer-in 1 64) (signo posInicio 7 )) (= (detectarBordes posInicio) 4 ) (char=? (string-ref tablero (+ posInicio 7 )) pA) ) (jCaballo (signo posInicio 9 ) ) ]
   [(and ( (integer-in 1 64) (signo posInicio 7 )) ( (integer-in 1 64) (signo posInicio 9 )) (not (= (detectarBordes posInicio ) 1)) (not (= (detectarBordes posInicio ) 4))  (char=? (string-ref tablero (signo posInicio 9 )) pA)  ) (jCaballo (signo posInicio 9 ) ) ]
   [(and ( (integer-in 1 64) (signo posInicio 7 )) ( (integer-in 1 64) (signo posInicio 9 )) (not (= (detectarBordes posInicio ) 1)) (not (= (detectarBordes posInicio ) 4))  (char=? (string-ref tablero (signo posInicio 7 )) pA)  ) (jCaballo (signo posInicio 7 ) ) ]
   (else 0)
  )
)

|#


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
      (peon2 (conocerFicha posInicial) (conocerFicha (get-mouse-click ventana)) +  (if (= ( quotient (posn-y (mouse-click-posn posInicial )) 80 ) 1) #t #f ) #\P (estoyDefendiendo? (conocerFicha posInicial) "TtCcAaDdPpr" "R" "cCaATDPpr" "dt" "da") )
      (peon2 (conocerFicha posInicial) (conocerFicha (get-mouse-click ventana)) -  (if (= ( quotient (posn-y (mouse-click-posn posInicial )) 80 ) 6) #t #f ) #\p (estoyDefendiendo? (conocerFicha posInicial) "TtCcAaDdPpR" "r" "cCaAtdPpr" "DT" "DA") )
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

  (   if (char-upper-case? (string-ref tablero (conocerFicha posInicial)))
         (torre2 posInicial  (get-mouse-click ventana) (estoyDefendiendo? (conocerFicha posInicial) "TtCcAaDdPpr" "R" "cCaATDPpr" "dt" "da") )
         (torre2 posInicial  (get-mouse-click ventana) (estoyDefendiendo? (conocerFicha posInicial) "TtCcAaDdPpR" "r" "cCaAtdPpr" "DT" "DA") ))
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
  
  (   if (char-upper-case? (string-ref tablero (conocerFicha posInicial)))
         (alfil2 posInicial  (get-mouse-click ventana) (estoyDefendiendo? (conocerFicha posInicial) "TtCcAaDdPpr" "R" "cCATDPpr" "dt" "da") )
         (alfil2 posInicial  (get-mouse-click ventana) (estoyDefendiendo? (conocerFicha posInicial) "TtCcAaDdPpR" "r" "cCatdPpr" "DT" "DA") ))

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
  (   if (char-upper-case? (string-ref tablero (conocerFicha posInicial)))
         (reina2 posInicial  (get-mouse-click ventana) (estoyDefendiendo? (conocerFicha posInicial) "TtCcAaDdPpr" "R" "cCaATDPpr" "dt" "da") )
         (reina2 posInicial  (get-mouse-click ventana) (estoyDefendiendo? (conocerFicha posInicial) "TtCcAaDdPpR" "r" "cCaAtdPpr" "DT" "DA") ))

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
  (   if (char-upper-case? (string-ref tablero (conocerFicha posInicial)))
         (caballo2 posInicial  (get-mouse-click ventana) (estoyDefendiendo? (conocerFicha posInicial) "TtCcAaDdPpr" "R" "cCaATDPpr" "dt" "da") )
         (caballo2 posInicial  (get-mouse-click ventana) (estoyDefendiendo? (conocerFicha posInicial) "TtCcAaDdPpR" "r" "cCaAtdPpr" "DT" "DA") ))

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
  [ (and (estoyPeligro? posFinal  "cCATDPpr" "dt" "da") (validarMov posInicial posFinal) (= borde 1) (or  (= (- posInicial 8) posFinal) (= (+ posInicial 8) posFinal) (= (+ posInicial 1) posFinal) (= (- posInicial 7) posFinal) (= (+ posInicial 9) posFinal) )) #t]
  [ (and (estoyPeligro? posFinal  "cCATDPpr" "dt" "da") (validarMov posInicial posFinal) (= borde 4)) (or  (= (- posInicial 8) posFinal) (= (+ posInicial 8) posFinal) (= (+ posInicial 1) posFinal) (= (+ posInicial 7) posFinal) (= (- posInicial 9) posFinal) ) #t ]
  [ (and (estoyPeligro? posFinal  "cCATDPpr" "dt" "da") (validarMov posInicial posFinal) (and (not (= (detectarBordes posInicial ) 1)) (not (= (detectarBordes posInicial ) 4)) )(or  (= (- posInicial 8) posFinal) (= (+ posInicial 8) posFinal) (= (+ posInicial 1) posFinal) (= (- posInicial 1) posFinal) (= (+ posInicial 7) posFinal) (= (- posInicial 9) posFinal) (= (- posInicial 7) posFinal) (= (+ posInicial 9) posFinal)) )  #t]
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

(define (sumaJaque x)
(cond
  [ (>= x 100) (if (reyAutosuficiente (string->number posReyNegro) "cCaATDPpr" "dt" "da" ) (begin ((draw-string ventana ) (make-posn 650 40) "JAQUE MATE" "RED")) (begin  ((draw-string ventana ) (make-posn 650 40) "█████████████████████" "black") ((draw-string ventana ) (make-posn 650 40) "JAQUE" "RED")) )]
  [ (= x 0) ((draw-string ventana ) (make-posn 650 40) "█████████████████████" "black")]
  [ else  ((draw-string ventana ) (make-posn 650 40) "█████████████████████" "black") ((draw-string ventana ) (make-posn 650 40) "JAQUE" "RED") ]
)
 )



(define (reyAutosuficiente posRey bloqueo2 ataca1 ataca2)
  (cond
    [ (and (> (- posRey 8) 0) (string-contains? "TCADP" (~a (string-ref tablero (- posRey 8) )) ) (estoyPeligro? (- posRey 8) bloqueo2 ataca1 ataca2)  ) #f ]
    [ (and (<= (- posRey 8) 64) (string-contains? "TCADP" (~a (string-ref tablero (+ posRey 8) )) ) (estoyPeligro? (+ posRey 8) bloqueo2 ataca1 ataca2)  ) #f ]
    
    [ (and (= (detectarBordes posRey) 1)  (not (string-contains? "TCADP" (~a (string-ref tablero (+ posRey 1) )) )) (estoyPeligro? (+ posRey 1) bloqueo2 ataca1 ataca2) )  #f]
    [ (and (= (detectarBordes posRey) 4)  (not (string-contains? "TCADP" (~a (string-ref tablero (- posRey 1) )) )) (estoyPeligro? (- posRey 1) bloqueo2 ataca1 ataca2) )  #f]
    
    [ (and (not (or (= (detectarBordes posRey) 1) (= (detectarBordes posRey) 4))  ) (not (string-contains? "TCADP" (~a (string-ref tablero (+ posRey 1) ) ))  ) (estoyPeligro? (+ posRey 1) bloqueo2 ataca1 ataca2) ) #f]
    [ (and (not (or (= (detectarBordes posRey) 1) (= (detectarBordes posRey) 4))  ) (not (string-contains? "TCADP" (~a (string-ref tablero (- posRey 1) ) ))  ) (estoyPeligro? (- posRey 1) bloqueo2 ataca1 ataca2) ) #f]
    
    [ (and (= posRey 1) (not (string-contains? "TCADP" (~a (string-ref tablero (+ posRey 9) )) ))  (estoyPeligro? (+ posRey 9) bloqueo2 ataca1 ataca2) ) #f]
    [ (and (= posRey 8) (not (string-contains? "TCADP" (~a (string-ref tablero (+ posRey 7) )) ))  (estoyPeligro? (+ posRey 7) bloqueo2 ataca1 ataca2) ) #f]
    [ (and (= posRey 57) (not (string-contains? "TCADP" (~a (string-ref tablero (- posRey 7) )) )) (estoyPeligro? (+ posRey 7) bloqueo2 ataca1 ataca2) ) #f]
    [ (and (= posRey 64) (not (string-contains? "TCADP" (~a (string-ref tablero (- posRey 9) )) )) (estoyPeligro? (+ posRey 9) bloqueo2 ataca1 ataca2) ) #f]

    [ (and (= (detectarBordes posRey) 1) (not (or (= posRey 57) (= posRey 1)))  (not (string-contains? "TCADP" (~a (string-ref tablero (+ posRey 9) ) ))  ) (estoyPeligro? (+ posRey 9) bloqueo2 ataca1 ataca2) ) #f]
    [ (and (= (detectarBordes posRey) 1) (not (or (= posRey 57) (= posRey 1)))  (not (string-contains? "TCADP" (~a (string-ref tablero (- posRey 7) ) ))  ) (estoyPeligro? (- posRey 7) bloqueo2 ataca1 ataca2)) #f]

    [ (and (= (detectarBordes posRey) 4) (not (or (= posRey 64) (= posRey 8)))  (not (string-contains? "TCADP" (~a (string-ref tablero (- posRey 9) ) ))  ) (estoyPeligro? (- posRey 9) bloqueo2 ataca1 ataca2) ) #f]
    [ (and (= (detectarBordes posRey) 4) (not (or (= posRey 64) (= posRey 8)))  (not (string-contains? "TCADP" (~a (string-ref tablero (+ posRey 7) ) ))  ) (estoyPeligro? (+ posRey 7) bloqueo2 ataca1 ataca2) ) #f ]

    [ (and ((integer-in 2 7 ) posRey) (not (string-contains? "TCADP" (~a (string-ref tablero (+ posRey 7) ) ))  ) (estoyPeligro? (+ posRey 7) bloqueo2 ataca1 ataca2)) #f]
    [ (and ((integer-in 2 7 ) posRey) (not (string-contains? "TCADP" (~a (string-ref tablero (+ posRey 9) ) ))  ) (estoyPeligro? (+ posRey 9) bloqueo2 ataca1 ataca2)) #f]

    [ (and ((integer-in 58 63) posRey) (not (string-contains? "TCADP" (~a (string-ref tablero (- posRey 7) ) ))  ) (estoyPeligro? (- posRey 7) bloqueo2 ataca1 ataca2) ) #f]
    [ (and ((integer-in 58 63) posRey) (not (string-contains? "TCADP" (~a (string-ref tablero (- posRey 9) ) ))  ) (estoyPeligro? (- posRey 9) bloqueo2 ataca1 ataca2) ) #f]

    [ (and  (not (or ((integer-in 2 7 ) posRey) ((integer-in 58 63) posRey))) (not (or (= (detectarBordes posRey) 1) (= (detectarBordes posRey) 4) )) (not (string-contains? "TCADP" (~a (string-ref tablero (+ posRey 7) ) ))  ) (estoyPeligro? (+ posRey 7) bloqueo2 ataca1 ataca2)) #f]
    [ (and  (not (or ((integer-in 2 7 ) posRey) ((integer-in 58 63) posRey))) (not (or (= (detectarBordes posRey) 1) (= (detectarBordes posRey) 4) )) (not (string-contains? "TCADP" (~a (string-ref tablero (- posRey 7) ) ))  ) (estoyPeligro? (- posRey 7) bloqueo2 ataca1 ataca2)) #f]
    [ (and  (not (or ((integer-in 2 7 ) posRey) ((integer-in 58 63) posRey))) (not (or (= (detectarBordes posRey) 1) (= (detectarBordes posRey) 4) )) (not (string-contains? "TCADP" (~a (string-ref tablero (- posRey 9) ) ))  ) (estoyPeligro? (- posRey 9) bloqueo2 ataca1 ataca2)) #f]
    [ (and  (not (or ((integer-in 2 7 ) posRey) ((integer-in 58 63) posRey))) (not (or (= (detectarBordes posRey) 1) (= (detectarBordes posRey) 4) )) (not (string-contains? "TCADP" (~a (string-ref tablero (+ posRey 9) ) ))  ) (estoyPeligro? (+ posRey 9) bloqueo2 ataca1 ataca2)) #f]


    )
)


(define (revisarReyBlanco )
  (sumaJaque (+ (llamarVerticales  (posRey posReyNegro)  "TCADPpacr" "td" 1)
          (llamarHorizontales  (posRey posReyNegro)  "TCADPpacr" "td" 1) (llamarDiagonales  (posRey posReyNegro)  "TCADPptcr" "ad" 1 )
          (verificarCaballoJaque (posRey posReyNegro) #\c )  (verificarPeonJaque (posRey posReyNegro) #\p +)
      )  )
)


(define (llamarVerticales  posInicio  bloqueo Abuscar modo )
  (cond
    [  ( (integer-in 1 8) posInicio) (verificacionVertical (+ (posRey posReyNegro) 8) + bloqueo Abuscar "" modo )  ]
    [  ( (integer-in 57 64) posInicio) (verificacionVertical (- (posRey posReyNegro) 8) - bloqueo Abuscar "" modo ) ]
    [  else (if (= modo 1 ) (+  (verificacionVertical (+ (posRey posReyNegro) 8) + bloqueo Abuscar "" modo )  (verificacionVertical (- (posRey posReyNegro) 8) - bloqueo Abuscar "" modo) ) (or  (verificacionVertical (+ (posRey posReyNegro) 8) + bloqueo Abuscar "" modo )  (verificacionVertical (- (posRey posReyNegro) 8) - bloqueo Abuscar "" modo) )  )  ]
    
  )  
 
)

(define (verificacionVertical posInicio signo  bloqueo Abuscar recorrido modo )
 (cond
   [ (not ((integer-in 1 64) posInicio) ) (if (= modo 1) 0 #f) ]
   [ (string-contains? bloqueo (~a (string-ref tablero posInicio) )) (if (= modo 1) 0 #f)  ]
   [ (string-contains? Abuscar (~a (string-ref tablero posInicio) )) (if (= modo 1) (if (>= (string-length recorrido ) 2)  (puedeInterponerse? 0 (string-append recorrido (~a posInicio)) ) (if (buscarDefensa  posInicio "cCatdPpr" "DT" "DA" #t ) 1 100) ) (if (= modo 2) posInicio #t)) ]
   [ else (verificacionVertical (signo posInicio 8) signo  bloqueo Abuscar (string-append recorrido (reguex (~a posInicio )) ) modo ) ]
  )
)

(define (reguex x)
(cond
  [ (= (string-length x ) 2) x ]
  [ else (string-append "0" x)]
)
)

(define (llamarHorizontales  posInicio  bloqueo Abuscar modo )
  (cond
    [  (= (detectarBordes posInicio) 1) (verificacionHorizontal (+ (posRey posReyNegro) 1) + bloqueo Abuscar "" modo )  ]
    [  (= (detectarBordes posInicio) 4) (verificacionHorizontal (- (posRey posReyNegro) 1) - bloqueo Abuscar "" modo ) ]
    [  (and (not (= (detectarBordes posInicio ) 1)) (not (= (detectarBordes posInicio ) 4)) ) (if (= modo 1 ) (+  (verificacionHorizontal (+ (posRey posReyNegro) 1) + bloqueo Abuscar "" modo )  (verificacionHorizontal (- (posRey posReyNegro) 1) - bloqueo Abuscar "" modo) ) (or  (verificacionHorizontal (+ (posRey posReyNegro) 1) + bloqueo Abuscar "" modo )  (verificacionHorizontal (- (posRey posReyNegro) 1) - bloqueo Abuscar "" modo) ))  ]
  )  
 
)

(define (verificacionHorizontal posInicio signo  bloqueo Abuscar recorrido modo)
 (cond
   [ (not ((integer-in 1 64) posInicio) ) (if (= modo 1) 0 #f) ]
   [ (string-contains? bloqueo (~a (string-ref tablero posInicio) )) (if (= modo 1) 0 #f)  ]
   [ (string-contains? Abuscar (~a (string-ref tablero posInicio) ))  (if (= modo 1) (if (>= (string-length recorrido ) 2)  (puedeInterponerse? 0 (string-append recorrido (~a posInicio)) ) (if (buscarDefensa  posInicio "cCatdPpr" "DT" "DA" #t ) 1 100) ) (if (= modo 2) posInicio #t))  ]
   [ (or (= (detectarBordes posInicio) 1) (= (detectarBordes posInicio) 4 )) (if (= modo 1) 0 #f) ]
   [ else (verificacionHorizontal (signo posInicio 1) signo  bloqueo Abuscar (string-append recorrido (reguex (~a posInicio )) ) modo ) ]
  )
)


(define (llamarDiagonales  posInicio  bloqueo Abuscar modo )
  (cond
    [  (= (detectarBordes posInicio) 1) ( if (= modo 1 ) (+ (verificacionDiagonal (- posInicio 7) - 7  bloqueo Abuscar "" modo)  (verificacionDiagonal (+ posInicio 9) + 9  bloqueo Abuscar "" modo)  ) (or (verificacionDiagonal (- posInicio 7) - 7  bloqueo Abuscar "" modo)  (verificacionDiagonal (+ posInicio 9) + 9  bloqueo Abuscar "" modo)  ) ) ]
    [  (= (detectarBordes posInicio) 4) (if (= modo  1 ) (+ (verificacionDiagonal (+ posInicio 7) + 7  bloqueo Abuscar "" modo)  (verificacionDiagonal (- posInicio 9) - 9  bloqueo Abuscar "" modo)   ) (or (verificacionDiagonal (+ posInicio 7) + 7  bloqueo Abuscar "" modo)  (verificacionDiagonal (- posInicio 9) - 9  bloqueo Abuscar "" modo)   )) ]
    [  (and (not (= (detectarBordes posInicio ) 1)) (not (= (detectarBordes posInicio ) 4)) ) (if (= modo 1 ) (+ (verificacionDiagonal (- posInicio 9) - 9  bloqueo Abuscar "" modo ) (verificacionDiagonal (+ posInicio 7) + 7  bloqueo Abuscar "" modo) (verificacionDiagonal (+ posInicio 9) + 9  bloqueo Abuscar "" modo) (verificacionDiagonal (- posInicio 7) - 7  bloqueo Abuscar "" modo ) ) (or (verificacionDiagonal (- posInicio 9) - 9  bloqueo Abuscar "" modo ) (verificacionDiagonal (+ posInicio 7) + 7  bloqueo Abuscar "" modo) (verificacionDiagonal (+ posInicio 9) + 9  bloqueo Abuscar "" modo) (verificacionDiagonal (- posInicio 7) - 7  bloqueo Abuscar "" modo ) ))  ]
  )  
 
)

(define (verificacionDiagonal posInicio signo index bloqueo Abuscar recorrido modo)
 (cond
   [ (not ((integer-in 1 64) posInicio) ) (if (= modo 1) 0 #f) ]
   [ (string-contains? bloqueo (~a (string-ref tablero posInicio) )) (if (= modo 1) 0 #f)  ]
   [ (string-contains? Abuscar (~a (string-ref tablero posInicio) )) (if (= modo 1) (if (>= (string-length recorrido ) 2)  (puedeInterponerse? 0 (string-append recorrido (~a posInicio)) ) (if (buscarDefensa  posInicio "cCatdPpr" "DT" "DA" #t ) 1 100) ) (if (= modo 2) posInicio #t)) ]
   [ (or (= (detectarBordes posInicio) 1) (= (detectarBordes posInicio) 4 )) (if (= modo 1) 0 #f) ]
   [ else (verificacionDiagonal (signo posInicio index) signo index  bloqueo Abuscar (string-append recorrido (reguex (~a posInicio )) ) modo ) ]
  )
)

(define (rvCaballo x caballoABuscar )
  (if ( (integer-in 1 64) x) (if (char=?  (string-ref tablero x ) caballoABuscar )  #t #f )   #f)
)

(define (jCaballo casilla ) ; jCaballo= jaque caballo
  (if (buscarDefensa casilla "cCatdPpr" "DT" "DA" #t ) 1 100 )
 
)

(define (verificarCaballoJaque posInicio cA ) ;cA = caballoABuscar
  (cond
    [ (and (= (detectarBordes posInicio) 1 ) (rvCaballo (+ posInicio 17) cA)  ) (jCaballo (+ posInicio 17) )  ]
    [ (and (= (detectarBordes posInicio) 1 ) (rvCaballo (+ posInicio 10) cA)  ) (jCaballo (+ posInicio 10) ) ]
    [ (and (= (detectarBordes posInicio) 1 ) (rvCaballo  (- posInicio 6) cA)  ) (jCaballo (- posInicio 6) ) ]
    [ (and (= (detectarBordes posInicio) 1 ) (rvCaballo (- posInicio 15) cA)  ) (jCaballo (- posInicio 15) ) ]

    [ (and (= (detectarBordes posInicio) 4 ) (rvCaballo (- posInicio 17) cA)  ) (jCaballo (- posInicio 17) ) ]
    [ (and (= (detectarBordes posInicio) 4 ) (rvCaballo (- posInicio 10) cA)  ) (jCaballo (- posInicio 10) ) ]
    [ (and (= (detectarBordes posInicio) 4 ) (rvCaballo  (+ posInicio 6) cA)  ) (jCaballo (+ posInicio 6) ) ]
    [ (and (= (detectarBordes posInicio) 4 ) (rvCaballo (+ posInicio 15) cA)  ) (jCaballo (+ posInicio 15) ) ]

    [ (and (= (detectarBordes posInicio) 2 ) (rvCaballo (- posInicio 17) cA)  )  (jCaballo (- posInicio 17) ) ]
    [ (and (= (detectarBordes posInicio) 2 ) (rvCaballo (- posInicio 15) cA)  )  (jCaballo (- posInicio 15) ) ]
    [ (and (= (detectarBordes posInicio) 2 ) (rvCaballo  (- posInicio 6) cA)  )  (jCaballo (- posInicio 6) ) ]
    [ (and (= (detectarBordes posInicio) 2 ) (rvCaballo (+ posInicio 15) cA)  )  (jCaballo (+ posInicio 15) ) ]
    [ (and (= (detectarBordes posInicio) 2 ) (rvCaballo (+ posInicio 17) cA)  )  (jCaballo (+ posInicio 17) ) ]
    [ (and (= (detectarBordes posInicio) 2 ) (rvCaballo (+ posInicio 10) cA)  )  (jCaballo (+ posInicio 10) ) ]

    [ (and (= (detectarBordes posInicio) 3 ) (rvCaballo (- posInicio 17) cA)  )  (jCaballo (- posInicio 17) ) ]
    [ (and (= (detectarBordes posInicio) 3 ) (rvCaballo (- posInicio 15) cA)  )  (jCaballo (- posInicio 15) ) ]
    [ (and (= (detectarBordes posInicio) 3 ) (rvCaballo  (+ posInicio 6) cA)  )  (jCaballo (+ posInicio 6) ) ]
    [ (and (= (detectarBordes posInicio) 3 ) (rvCaballo (+ posInicio 15) cA)  )  (jCaballo (+ posInicio 15) ) ]
    [ (and (= (detectarBordes posInicio) 3 ) (rvCaballo (+ posInicio 17) cA)  )  (jCaballo (+ posInicio 17) ) ]
    [ (and (= (detectarBordes posInicio) 3 ) (rvCaballo (- posInicio 10) cA)  )  (jCaballo (- posInicio 10) ) ]

    [ (and (= (detectarBordes posInicio) 0 ) (rvCaballo (- posInicio 17) cA)  )  (jCaballo (- posInicio 17) ) ]
    [ (and (= (detectarBordes posInicio) 0 ) (rvCaballo (- posInicio 15) cA)  )  (jCaballo (- posInicio 15) ) ]
    [ (and (= (detectarBordes posInicio) 0 ) (rvCaballo  (+ posInicio 6) cA)  )  (jCaballo (+ posInicio 6) ) ]
    [ (and (= (detectarBordes posInicio) 0 ) (rvCaballo (+ posInicio 15) cA)  )  (jCaballo (+ posInicio 15) ) ]
    [ (and (= (detectarBordes posInicio) 0 ) (rvCaballo (+ posInicio 17) cA)  )  (jCaballo (+ posInicio 17) ) ]
    [ (and (= (detectarBordes posInicio) 0 ) (rvCaballo (- posInicio 10) cA)  )  (jCaballo (- posInicio 10) ) ]
    [ (and (= (detectarBordes posInicio) 0 ) (rvCaballo (- posInicio 6) cA)  )  (jCaballo (- posInicio 6) ) ]
    [ (and (= (detectarBordes posInicio) 0 ) (rvCaballo (+ posInicio 10) cA)  )  (jCaballo (+ posInicio 10) ) ]
    
    (else 0)
   )
)

(define (verificarPeonJaque posInicio pA signo) ;pA = peonABuscar
 (cond
   [(and ( (integer-in 1 64) (signo posInicio 9 )) (= (detectarBordes posInicio) 1 ) (char=? (string-ref tablero (+ posInicio 9 )) pA) ) (jCaballo (signo posInicio 9 ) ) ]
   [(and ( (integer-in 1 64) (signo posInicio 7 )) (= (detectarBordes posInicio) 4 ) (char=? (string-ref tablero (+ posInicio 7 )) pA) ) (jCaballo (signo posInicio 9 ) ) ]
   [(and ( (integer-in 1 64) (signo posInicio 7 )) ( (integer-in 1 64) (signo posInicio 9 )) (not (= (detectarBordes posInicio ) 1)) (not (= (detectarBordes posInicio ) 4))  (char=? (string-ref tablero (signo posInicio 9 )) pA)  ) (jCaballo (signo posInicio 9 ) ) ]
   [(and ( (integer-in 1 64) (signo posInicio 7 )) ( (integer-in 1 64) (signo posInicio 9 )) (not (= (detectarBordes posInicio ) 1)) (not (= (detectarBordes posInicio ) 4))  (char=? (string-ref tablero (signo posInicio 7 )) pA)  ) (jCaballo (signo posInicio 7 ) ) ]
   (else 0)
  )
)


(define (verificarCaballo posInicio cA ) ;cA = caballoABuscar
  (cond
    [ (and (= (detectarBordes posInicio) 1 ) (or  (rvCaballo (+ posInicio 17) cA) (rvCaballo (+ posInicio 10) cA) (rvCaballo  (- posInicio 6) cA) (rvCaballo (- posInicio 15) cA) )  ) #t ]
    [ (and (= (detectarBordes posInicio) 4 ) (or  (rvCaballo (- posInicio 17) cA) (rvCaballo (- posInicio 10) cA) (rvCaballo  (+ posInicio 6) cA) (rvCaballo (+ posInicio 15) cA) )  ) #t ]
    [ (and (= (detectarBordes posInicio) 2 ) (or  (rvCaballo (- posInicio 17) cA) (rvCaballo (- posInicio 15) cA) (rvCaballo  (- posInicio 6) cA) (rvCaballo (+ posInicio 15) cA) (rvCaballo (+ posInicio 17) cA) (rvCaballo (+ posInicio 10) cA) )  )  #t ]
    [ (and (= (detectarBordes posInicio) 3 ) (or  (rvCaballo (- posInicio 17) cA) (rvCaballo (- posInicio 15) cA) (rvCaballo  (+ posInicio 6) cA) (rvCaballo (+ posInicio 15) cA) (rvCaballo (+ posInicio 17) cA) (rvCaballo (- posInicio 10) cA)  )  ) #t ]
    [ (and (= (detectarBordes posInicio) 0 ) (or  (rvCaballo (- posInicio 17) cA) (rvCaballo (- posInicio 15) cA) (rvCaballo  (- posInicio 6) cA) (rvCaballo  (+ posInicio 6) cA) (rvCaballo (+ posInicio 15) cA) (rvCaballo (+ posInicio 17) cA) (rvCaballo (+ posInicio 10) cA) (rvCaballo (- posInicio 10) cA) )  ) #t ]
    (else #f)
   )
)


(define (verificarPeon posInicio pA signo) ;pA = peonABuscar
 (cond
   [(and ( (integer-in 1 64) (signo posInicio 9 )) (= (detectarBordes posInicio) 1 ) (char=? (string-ref tablero (+ posInicio 9 )) pA) ) #t ]
   [(and ( (integer-in 1 64) (signo posInicio 7 )) (= (detectarBordes posInicio) 4 ) (char=? (string-ref tablero (+ posInicio 7 )) pA) ) #t ]
   [(and ( (integer-in 1 64) (signo posInicio 7 )) ( (integer-in 1 64) (signo posInicio 9 )) (not (= (detectarBordes posInicio ) 1)) (not (= (detectarBordes posInicio ) 4))  (or (char=? (string-ref tablero (signo posInicio 9 )) pA) (char=? (string-ref tablero (signo posInicio 7 )) pA) )  ) #t ]
   (else #f)
  )
)


(define (estoyDefendiendo? posInicio bloqueo1 rey bloqueo2 ataca1 ataca2 )
 (cond
   [ (verificacionVertical   (- posInicio 8) -   bloqueo1 rey "" 0)  (if (not (verificacionVertical (+ posInicio 8) +   bloqueo2 ataca1 "" 0)) 0 1 ) ]
   [ (verificacionVertical   (+ posInicio 8) +   bloqueo1 rey "" 0)  (if (not (verificacionVertical (- posInicio 8) -   bloqueo2 ataca1 "" 0)) 0 1 ) ]
   [ (verificacionHorizontal (- posInicio 1) -   bloqueo1 rey "" 0)  (if (not (verificacionHorizontal (+ posInicio 1) + bloqueo2 ataca1 "" 0)) 0 2 ) ]
   [ (verificacionHorizontal (+ posInicio 1) +   bloqueo1 rey "" 0)  (if (not (verificacionHorizontal (- posInicio 1) - bloqueo2 ataca1 "" 0)) 0 2 ) ]
   [ (verificacionDiagonal   (- posInicio 9) - 9 bloqueo1 rey "" 0)  (if (not (verificacionDiagonal (+ posInicio 9) + 9 bloqueo2 ataca2 "" 0)) 0 3 ) ]
   [ (verificacionDiagonal   (+ posInicio 9) + 9 bloqueo1 rey "" 0)  (if (not (verificacionDiagonal (- posInicio 9) - 9 bloqueo2 ataca2 "" 0)) 0 3 ) ]
   [ (verificacionDiagonal   (- posInicio 7) - 7 bloqueo1 rey "" 0)  (if (not (verificacionDiagonal (+ posInicio 7) + 7 bloqueo2 ataca2 "" 0)) 0 4 ) ]
   [ (verificacionDiagonal   (+ posInicio 7) + 7 bloqueo1 rey "" 0)  (if (not (verificacionDiagonal (- posInicio 7) - 7 bloqueo2 ataca2 "" 0)) 0 4 ) ]
   (else 0)
  )
)

(define (estoyPeligro? posInicio bloqueo2 ataca1 ataca2)
  (cond
    [ (verificacionVertical (+ posInicio 8) +   bloqueo2 ataca1 "" 0) #f ]
    [ (verificacionVertical (- posInicio 8) -   bloqueo2 ataca1 "" 0) #f ]
    [ (verificacionHorizontal (+ posInicio 1) + bloqueo2 ataca1 "" 0) #f ]
    [ (verificacionHorizontal (- posInicio 1) - bloqueo2 ataca1 "" 0) #f ]
    [ (verificacionDiagonal (+ posInicio 9) + 9 bloqueo2 ataca2 "" 0) #f ]
    [ (verificacionDiagonal (- posInicio 9) - 9 bloqueo2 ataca2 "" 0) #f ]
    [ (verificacionDiagonal (+ posInicio 7) + 7 bloqueo2 ataca2 "" 0) #f ]
    [ (verificacionDiagonal (- posInicio 7) - 7 bloqueo2 ataca2 "" 0) #f ]
    ( (not (= (verificarPeonJaque posInicio #\p +)  0) ) #f)
    (else #t)

  )


)

(define (juego)
  (revisarReyBlanco )
  (imprimirTablero 0 1)
  (darMovimiento  (get-mouse-click ventana)  )
  (juego)
)


(define (puedeInterponerse? i recorrido )
  ;(printf "~a\n" recorrido)
  (if (>= i (string-length recorrido) )
      100
      (if (= i (- (string-length recorrido) 2))
          (if (buscarDefensa  (string->number (substring recorrido i (+ i 2))) "cCatdPpr" "DT" "DA" #t )
              1
              (puedeInterponerse? (+ i 2) recorrido )
              )
          (if (buscarDefensa  (string->number (substring recorrido i (+ i 2))) "cCatdPpr" "DT" "DA" #f )
              1
              (puedeInterponerse? (+ i 2) recorrido )
              )
          )
      )
  )

(define (comparX x)
  (cond
    [ (not (= x 0) ) #f ]
    [else #t]
   )
)

(define (buscarDefensa  posInicio bloqueo2 ataca1 ataca2 atacante )
 (cond
   [ (and (verificacionVertical   (+ posInicio 8) +  bloqueo2 ataca1   "" 0) (comparX (estoyDefendiendo? (verificacionVertical   (+ posInicio 8) +  bloqueo2 ataca1   "" 2)  "TtCcAaDdPpr" "R" "cCaATDPpr" "dt" "da" )) )  #t]
   [ (and (verificacionVertical   (- posInicio 8) -  bloqueo2 ataca1   "" 0) (comparX (estoyDefendiendo? (verificacionVertical   (- posInicio 8) -  bloqueo2 ataca1   "" 2)  "TtCcAaDdPpr" "R" "cCaATDPpr" "dt" "da" )) )  #t]
   [ (and (verificacionHorizontal (+ posInicio 1) +  bloqueo2 ataca1   "" 0) (comparX (estoyDefendiendo? (verificacionHorizontal (+ posInicio 1) +  bloqueo2 ataca1   "" 2)  "TtCcAaDdPpr" "R" "cCaATDPpr" "dt" "da" )) )  #t]
   [ (and (verificacionHorizontal (- posInicio 1) -  bloqueo2 ataca1   "" 0) (comparX (estoyDefendiendo? (verificacionHorizontal (- posInicio 1) -  bloqueo2 ataca1   "" 2)  "TtCcAaDdPpr" "R" "cCaATDPpr" "dt" "da" )) )  #t]
   [ (and (verificacionDiagonal   (+ posInicio 9) +  9 bloqueo2 ataca2 "" 0) (comparX (estoyDefendiendo? (verificacionDiagonal   (+ posInicio 9) +  9 bloqueo2 ataca2 "" 2)  "TtCcAaDdPpr" "R" "cCaATDPpr" "dt" "da" )) )  #t]
   [ (and (verificacionDiagonal   (- posInicio 9) -  9 bloqueo2 ataca2 "" 0) (comparX (estoyDefendiendo? (verificacionDiagonal   (- posInicio 9) -  9 bloqueo2 ataca2 "" 2)  "TtCcAaDdPpr" "R" "cCaATDPpr" "dt" "da" )) )  #t]
   [ (and (verificacionDiagonal   (+ posInicio 7) +  7 bloqueo2 ataca2 "" 0) (comparX (estoyDefendiendo? (verificacionDiagonal   (+ posInicio 7) +  7 bloqueo2 ataca2 "" 2)  "TtCcAaDdPpr" "R" "cCaATDPpr" "dt" "da" )) )  #t]
   [ (and (verificacionDiagonal   (- posInicio 7) -  7 bloqueo2 ataca2 "" 0) (comparX (estoyDefendiendo? (verificacionDiagonal   (- posInicio 7) -  7 bloqueo2 ataca2 "" 2)  "TtCcAaDdPpr" "R" "cCaATDPpr" "dt" "da" )) )  #t]
   [ (verificarCaballo posInicio #\C ) #t ]
   [ (and atacante (verificarPeon posInicio #\P -)  ) #t ]
   [ (and (not atacante) (char=? (string-ref tablero (- posInicio 8) ) #\P  ) ) #t]
   (else #f)
  )
)

(juego)
