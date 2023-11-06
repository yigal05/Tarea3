#lang racket
(require graphics/graphics)
(open-graphics)
;----------------------------------------------------

(define ventana (open-viewport "ajedrez" 1000 640 ))
((draw-solid-rectangle ventana) (make-posn 0 0 ) 1000 640 "black")

(define tamañoAjedrez 80)

(define colorCasilla "XBNBNBNBNNBNBNBNBBNBNBNBNNBNBNBNBBNBNBNBNNBNBNBNBBNBNBNBNNBNBNBNB")

(define (crearBase)
  ((draw-solid-rectangle ventana) (make-posn 0 0 ) (* 8 tamañoAjedrez) (* 8 tamañoAjedrez) "brown")
  (crearTablero  0 0 1 tamañoAjedrez)
)

(define hayJaque? (string-copy "N" ))

(define fBlancas "165423E ")
(define fNegras  "EDBCA5 ")


(define fQPuedenDefenderReyBlanco "DFBCA" )

(define tableroBase (string-copy (string-append "!CBDFEDBCAAAAAAAA"(make-string 32 #\space)"1111111132465423" )))


(define (EstoyLimite x)
  (+   (if ( = 0 (remainder (- x 1) 8))  1 0 )  (if ( = 0 (remainder (- x 2) 8))  2 0 ) (if ( = 0 (remainder x 8))  4 0 ) (if ( = 0 (remainder (+ x 1) 8))  3 0 )    )
)



(define posReyB (string-copy "61" ))
(define posReyN (string-copy "05" ))



(define (actualizarPos reyActulizar x)
  (if (= (string-length x) 2)
      (begin (string-set! reyActulizar 0 (string-ref x 0)) (string-set! reyActulizar 1 (string-ref x 1)))
      (begin (string-set! reyActulizar 0 #\0) (string-set! reyActulizar 1 (string-ref x 0)))
   )

)

(define (regguearDato dato)
(if (= (string-length dato) 2)
      dato
      (format "0~a" dato)
   )
)

(define (vacio? n)
  (if ( (integer-in 1 64) n)  (if (and (equal? (string-ref tableroBase n) #\space) (not (equal? (string-ref tableroBase n) #\5)) (not (equal? (string-ref tableroBase n) #\E)) ) #t #f ) #f)
  
)

(define (quienSoy? t )
       (cond
       [ (equal? t  #\C) torreNegro] [ (equal? t  #\B) caballoNegro] [ (equal? t  #\D) alfilNegro] [ (equal? t  #\E) reyNegro]
       [ (equal? t  #\F) reinaNegro] [ (equal? t  #\A) peonNegro] [ (equal? t #\3) torreBlanca] [ (equal? t #\2) caballoBlanco]
       [ (equal? t #\4) alfilBlanco] [ (equal? t #\5) reyBlanco] [ (equal? t #\6) reinaBlanca]  [ (equal? t #\1) peonBlanco]
      )
)

(define (reyAutosuficiente? )
  (cond
    ;[ (and  (vacio? (- (string->number posReyN) 1) ) (comprobarEstadoZ (string->number posReyN) (- (string->number posReyN) 1) peonNegro (string-ref tableroBase (string->number posReyN)))) #t ]
    ;[ (and  (vacio? (+ (string->number posReyN) 1) ) (comprobarEstadoZ (string->number posReyN) (- (string->number posReyN) 1) peonNegro (string-ref tableroBase (string->number posReyN)))) #t ]
    ;[ (and  (vacio? (- (string->number posReyN) 8) ) (comprobarEstadoZ (string->number posReyN) (- (string->number posReyN) 1) peonNegro (string-ref tableroBase (string->number posReyN)))) #t ]
    [ (and  (vacio? (+ (string->number posReyN) 8) ) (comprobarEstadoAuto  (+ (string->number posReyN) 8)  (string->number posReyN)  peonNegro (string-ref tableroBase (string->number posReyN)))) #t ]
    ;[ (and  (vacio? (- (- (string->number posReyN) 8) 1) ) (comprobarEstadoZ (string->number posReyN) (- (string->number posReyN) 1) peonNegro (string-ref tableroBase (string->number posReyN)))) #t ]
    ;[ (and  (vacio? (+ (- (string->number posReyN) 8) 1) ) (comprobarEstadoZ (string->number posReyN) (- (string->number posReyN) 1) peonNegro (string-ref tableroBase (string->number posReyN)))) #t ]
    ;[ (and  (vacio? (- (+ (string->number posReyN) 8) 1) ) (comprobarEstadoZ (string->number posReyN) (- (string->number posReyN) 1) peonNegro (string-ref tableroBase (string->number posReyN)))) #t ]
    ;[ (and  (vacio? (+ (+ (string->number posReyN) 8) 1) ) (comprobarEstadoZ (string->number posReyN) (- (string->number posReyN) 1) peonNegro (string-ref tableroBase (string->number posReyN)))) #t ]
    (else #f)
   )
 
)



(define (encontrarRangos i x restar vecesBucle string casillaQueHacenElJaque modo fichasQuePuedenDefenderAlRey casillaDesdeVerificacion)
  (if (= i vecesBucle)
      0
      (if ((integer-in 1 64  ) x )
          (begin
            (if (equal? modo "rey")
                (printf "en la posicion ~a hay ~a \n" x (~a (string-ref tableroBase x) ))
                (void)
             )
            (if (string-contains? fichasQuePuedenDefenderAlRey (~a (string-ref tableroBase x) ) )
                0
                (if  (string-contains? string (~a (string-ref tableroBase x) ) )
                     (if (equal? modo "rey")
                         (if (detectorCasillas 0 casillaQueHacenElJaque)
                               1
                               100                     
                          )
                         (if (comprobarEstadoZ x casillaDesdeVerificacion  (string-ref tableroBase x)) 1 0)
                      )
                     (encontrarRangos (add1 i) (+ x restar) restar vecesBucle string (string-append casillaQueHacenElJaque (regguearDato (~a x)) ) modo fichasQuePuedenDefenderAlRey casillaDesdeVerificacion)
                 )
             )  
            )
          0
       )
   )
)

(define (detectorCasillas x str)
  (printf str)
  (if (or (=  (string-length str) x ) (=  (string-length str) 0 ) )
      #f
      (if (revisarCasilla (substring str x (+ x 2)) )
          #t
          (begin
           (detectorCasillas (+ x 2) str)
          )
      )
  )

)

(define (revisarCasilla casilla )
  (printf "\nDebo revisar a ~a\n" casilla )
  (if (arreglo1 (+ (Diagonales 0 (string->number casilla) 1 8 7 "DF" "asda" "1A3C2B6")
          (Laterales 0 (string->number casilla) 1 8 1 "CF" "asd" "1A3C2B6")
          (Diagonales 0 (string->number casilla) 1 8 9 "DF" "asd" "1A3C2B6")
          (encontrarRangos 0 (+ (string->number casilla) 8) 8 8 "CF" "" "asd" "1A3C2B6" (+ (string->number casilla) 8))
          (encontrarRangos 0 (- (string->number casilla) 8) -8 8 "CF" "" "1as3" "1A3C2B6" (- (string->number casilla) 8))
          ))
      #t
      #f
      )
  )

(define (Diagonales i x z y DequeForma str modo fichasQuePuedenDefenderAlRey)
  (if (= i 8)
      #f
      (if (and (>= x z) (<= x y) )
          (+  (encontrarRangos 0 (+ x DequeForma) DequeForma (- x z) str "" modo fichasQuePuedenDefenderAlRey x)  (encontrarRangos 0 (- x DequeForma) (* -1 DequeForma) (- y x ) str "" modo fichasQuePuedenDefenderAlRey x)  )
          (Diagonales (add1 i) x (+ z 8) (+ y 8) DequeForma str modo fichasQuePuedenDefenderAlRey )
       )
   )
)

(define (Laterales i x z y DequeForma str modo fichasQuePuedenDefenderAlRey)
  (if (= i 8)
      #f
      (if (and (>= x z) (<= x y) )
          (+  (encontrarRangos 0 (+ x DequeForma) DequeForma (- y x ) str "" modo fichasQuePuedenDefenderAlRey x)  (encontrarRangos 0 (- x DequeForma) (* -1 DequeForma) (- x z) str "" modo fichasQuePuedenDefenderAlRey x) )
          (Laterales (add1 i) x (+ z 8) (+ y 8) DequeForma str modo fichasQuePuedenDefenderAlRey )
       )
   )
)

(define (x!? c)
  (if (and ( (integer-in 1 64) c ) (char=? #\2 (string-ref tableroBase c) ) ) #t #f  )
)

(define (revisarCaballos x )
 (cond
   
   [ (and  (= (EstoyLimite x ) 1) (or  (x!? (+ (+ x 16) 1))  (x!? (+ (- x 16) 1))  (x!? (+ (+ x 2) 8)) (x!? (- (+ x 2) 8))  )  ) 100  ]
   [ (and  (= (EstoyLimite x ) 2) (or (x!? (- (- x 16) 1)) (x!? (+ (+ x 16) 1)) (x!? (- (+ x 16) 1)) (x!? (+ (- x 16) 1))  (x!? (+ (+ x 2) 8)) (x!? (- (+ x 2) 8))  )  ) 100  ]
   [ (and  (= (EstoyLimite x ) 3) (or (x!? (- (- x 16) 1)) (x!? (+ (+ x 16) 1)) (x!? (- (+ x 16) 1)) (x!? (+ (- x 16) 1))  (x!? (- (- x 2) 8)) (x!? (+ (- x 2) 8)) )  ) 100  ]
   [ (and  (= (EstoyLimite x ) 4) (or (x!? (- (- x 16) 1)) (x!? (- (+ x 16) 1))  (x!? (- (- x 2) 8)) (x!? (+ (- x 2) 8)) )  ) 100  ]
   [ (and  (= (EstoyLimite x ) 0) (or (x!? (- (- x 16) 1)) (x!? (+ (+ x 16) 1)) (x!? (- (+ x 16) 1)) (x!? (+ (- x 16) 1)) (x!? (- (- x 2) 8)) (x!? (+ (+ x 2) 8)) (x!? (- (+ x 2) 8)) (x!? (+ (- x 2) 8)) )  ) 100  ]
   
   ( else 0  )
  )
  

)

(define (arreglo1 x )
  (displayln x)
  (cond
    [ ( (integer-in 1 4) x) (begin ((draw-string ventana ) (make-posn 650 40) "JAQUE" "purple") #t) ]
    [ ( >= x 100) (begin ((draw-string ventana ) (make-posn 650 40) "JAQUE MATE" "red") #t) ] ;aqui se hacen mas verificacion se puede matar la causante?
                                                                                              ;el rey puede escapar?
    [ else (begin ((draw-string ventana ) (make-posn 650 40) "█████████████" "black") #f) ]
   )
)

(define (revisarReyN )
  (arreglo1 (+ (Diagonales 0 (string->number posReyN) 1 8 7 "4" "rey" fQPuedenDefenderReyBlanco )
          (Laterales 0 (string->number posReyN) 1 8 1 "3" "rey" fQPuedenDefenderReyBlanco)
          (Diagonales 0 (string->number posReyN) 1 8 9 "4" "rey" fQPuedenDefenderReyBlanco)
          (encontrarRangos 0 (+ (string->number posReyN) 8) 8 8 "3" "" "rey" fQPuedenDefenderReyBlanco (+ (string->number posReyN) 8))
          (encontrarRangos 0 (- (string->number posReyN) 8) -8 8 "3" "" "rey" fQPuedenDefenderReyBlanco (- (string->number posReyN) 8))
          (revisarCaballos (string->number posReyN) )
          ))
  
)




#|----------------------------------- MOVER FICHAS ------------------------------------|#

(define (puedoComer x n)

  (if (string-contains? fBlancas x)
      (if (not (string-contains? fBlancas (~a (string-ref tableroBase n)))) #t #f)
      (if (not (string-contains? fNegras (~a (string-ref tableroBase n)))) #t #f)
   )
  
)



;--------------------------- Peon -------------------------------------------

(define (peon t x )
  (peon2 t x (CasillaTocadaX (get-mouse-click ventana))  )
)

(define (peon2 t x n)
 (displayln (EstoyLimite t))
 (cond
 [ (and (equal? x "1") (= n (- t 8)) (vacio? n) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 peonBlanco #\1) (ir) )  ]
 [ (and (equal? x "A") (= n (+ t 8)) (vacio? n) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 peonNegro #\A) (ir) )  ]
 

[ (and (equal? x "1") (= (EstoyLimite t) 1) (= n (+ (- t 8) 1)) (puedoComer x n) ) (begin (taparBloque 1 t 0 0 )  (taparBloque 1 n 0 0 ) (colocarFicha 1 n 0 0 peonBlanco #\1) (ir) )  ]
[ (and (equal? x "1") (= (EstoyLimite t) 4) (= n (- (- t 8) 1)) (puedoComer x n) ) (begin (taparBloque 1 t 0 0 )  (taparBloque 1 n 0 0 ) (colocarFicha 1 n 0 0 peonBlanco #\1) (ir) )  ]
[ (and (equal? x "1") (or (= (EstoyLimite t) 0) (= (EstoyLimite t) 2) (= (EstoyLimite t) 4) ) (or (= n (+ (- t 8) 1) ) (= n (- (- t 8) 1)) ) (puedoComer x n) ) (begin (taparBloque 1 t 0 0 )  (taparBloque 1 n 0 0 ) (colocarFicha 1 n 0 0 peonBlanco #\1) (ir) )  ]

[ (and (equal? x "A") (= (EstoyLimite t) 1) (= n (+ (+ t 8) 1)) (puedoComer x n) ) (begin (taparBloque 1 t 0 0 )  (taparBloque 1 n 0 0 ) (colocarFicha 1 n 0 0 peonNegro #\A) (ir) )  ]
[ (and (equal? x "A") (= (EstoyLimite t) 4) (= n (- (+ t 8) 1)) (puedoComer x n) ) (begin (taparBloque 1 t 0 0 )  (taparBloque 1 n 0 0 ) (colocarFicha 1 n 0 0 peonNegro #\A) (ir) )  ]
[ (and (equal? x "A") (or (= (EstoyLimite t) 0) (= (EstoyLimite t) 2) (= (EstoyLimite t) 3) ) (or (= n (+ (+ t 8) 1) ) (= n (- (+ t 8) 1)) ) (puedoComer x n) ) (begin (taparBloque 1 t 0 0 )  (taparBloque 1 n 0 0 ) (colocarFicha 1 n 0 0 peonNegro #\A) (ir) )  ]


( else (ir)  )
 )
)

;----------------------- mover torres horizontalmente ----------------------------------------------------------------------
(define (puedeTorre t n i m res vecesbucle)
  (if (= i vecesbucle)
      #f
      (if (and (= (- t m) n)  (not (string-contains? "5E" (~a (string-ref tableroBase (- t m) ))  ))    )
          (begin
            #t
           )
          (if (vacio? (- t  m ))
                  (puedeTorre t n (add1 i) (+ m res) res vecesbucle)
                   #f
           )
        )
    )
)

(define (encontrarRango i t n z y)
  (if (= i 8)
      (void)
      (if (and (>= t z) (<= t y) )
          (if (or  (puedeTorre t n 0 1 1 (- t z ) ) (puedeTorre t n 0 -1 -1 (- y t))  )
              #t
              #f
           )
          (encontrarRango (add1 i) t n (+ z 8) (+ y 8))
       )
   )
)
;--------------------- Comprobar si el moviemto que se quiere hacer es permitido -------------------------------------------

(define (puedeAlfil? t n i m res)
  (if (= i 7)
      #f
      (if (or (<= (- t m) 0) (>= (- t m) 65) )
          #f
          (if (and  (= (- t m) n) (not (string-contains? "5E" (~a (string-ref tableroBase (- t m) ))  ) ) )
              (begin
                (taparBloque 1 n 0 0 )
                #t
               )
              (if (vacio? (- t  m ))
                  (puedeAlfil? t n (add1 i) (+ m res) res)
                   #f
               )
           )  
        )
    )
)
;--------------------------- Alfil -------------------------------------------
(define (alfil t x )
  (alfil2 t x (CasillaTocadaX (get-mouse-click ventana))  )
)

(define (alfil2 t x n)
 (cond
   [ (and (equal? x "4") (puedeAlfil? t n 0 9 9)  ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 alfilBlanco #\4) (ir) )  ]
   [ (and (equal? x "4") (puedeAlfil? t n 0 7 7)  ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 alfilBlanco #\4) (ir) )  ]
   [ (and (equal? x "4") (puedeAlfil? t n 0 -9 -9)  ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 alfilBlanco #\4) (ir) )  ]
   [ (and (equal? x "4") (puedeAlfil? t n 0 -7 -7)  ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 alfilBlanco #\4) (ir) )  ]

   [ (and (equal? x "D") (puedeAlfil? t n 0 9 9)  ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 alfilNegro #\D) (comprobarEstado n t #\D) )  ]
   [ (and (equal? x "D") (puedeAlfil? t n 0 7 7)  ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 alfilNegro #\D) (comprobarEstado n t #\D) )  ]
   [ (and (equal? x "D") (puedeAlfil? t n 0 -9 -9)  ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 alfilNegro #\D) (comprobarEstado n t #\D) )  ]
   [ (and (equal? x "D") (puedeAlfil? t n 0 -7 -7)  ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 alfilNegro #\D) (comprobarEstado n t #\D) )  ]
   ( else (ir)  )
 )
)

(define (comprobarEstado n t ficha)
  (if (not (revisarReyN) )
      (ir)
      (begin
       ((draw-string ventana ) (make-posn 650 40) "Movimiento invalido" "RED")
       (taparBloque 1 n 0 0 )
       (colocarFicha 1 t 0 0 (quienSoy? ficha ) ficha)
       (ir)
      )
   )
)

(define (comprobarEstadoZ n t   ficha)
     (displayln "es que me estan llamando")
       (taparBloque 1 n 0 0 )
       (colocarFicha 1 t 0 0 (quienSoy? ficha ) ficha)
      (if (revisarReyN)
          (begin
            (taparBloque 1 t 0 0 )
            (colocarFicha 1 n 0 0 (quienSoy? ficha ) ficha)
            #f
           )
          (begin
            (taparBloque 1 t 0 0 )
            (colocarFicha 1 n 0 0 (quienSoy? ficha ) ficha)
            #t
           )
       )

)

(define (comprobarEstadoAuto n t   ficha)
     (displayln "zz")
       (taparBloque 1 n 0 0 )
       (colocarFicha 1 t 0 0 (quienSoy? t ) ficha)
      (if (or (Diagonales 0 (string->number posReyN) 1 8 7 "4" "asd" fQPuedenDefenderReyBlanco )
          (Laterales 0 (string->number posReyN) 1 8 1 "3" "asd" fQPuedenDefenderReyBlanco)
          (Diagonales 0 (string->number posReyN) 1 8 9 "4" "asd" fQPuedenDefenderReyBlanco)
          (encontrarRangos 0 (+ (string->number posReyN) 8) 8 8 "3" "" "asd" fQPuedenDefenderReyBlanco (+ (string->number posReyN) 8))
          (encontrarRangos 0 (- (string->number posReyN) 8) -8 8 "3" "" "asd" fQPuedenDefenderReyBlanco (- (string->number posReyN) 8))
          (revisarCaballos (string->number posReyN) )
          )
          (begin
            (taparBloque 1 t 0 0 )
            (colocarFicha 1 n 0 0 (quienSoy? n ) ficha)
            #f
           )
          (begin
            (taparBloque 1 t 0 0 )
            (colocarFicha 1 n 0 0 (quienSoy? n ) ficha)
            #t
           )
       )

)



;--------------------------- Caballo -------------------------------------------
(define (caballo t x )
  (caballo2 t x (CasillaTocadaX (get-mouse-click ventana))  )
)

(define (caballo2 t x n)
  
 (cond
   
   [ (and (equal? x "2") (= (EstoyLimite t ) 1) (or  (= n (+ (+ t 16) 1)) (= n (+ (- t 16) 1))  (= n (+ (+ t 2) 8)) (= n (- (+ t 2) 8))  ) (or (vacio? n) (puedoComer x n)) ) (begin (taparBloque 1 t 0 0 ) (taparBloque 1 n 0 0 )(colocarFicha 1 n 0 0 caballoBlanco #\2) (ir) )  ]
   [ (and (equal? x "2") (= (EstoyLimite t ) 2) (or (= n (- (- t 16) 1)) (= n (+ (+ t 16) 1)) (= n (- (+ t 16) 1)) (= n (+ (- t 16) 1))  (= n (+ (+ t 2) 8)) (= n (- (+ t 2) 8))  ) (or (vacio? n) (puedoComer x n)) ) (begin (taparBloque 1 t 0 0 ) (taparBloque 1 n 0 0 )(colocarFicha 1 n 0 0 caballoBlanco #\2) (ir) )  ]
   [ (and (equal? x "2") (= (EstoyLimite t ) 3) (or (= n (- (- t 16) 1)) (= n (+ (+ t 16) 1)) (= n (- (+ t 16) 1)) (= n (+ (- t 16) 1))  (= n (- (- t 2) 8)) (= n (+ (- t 2) 8)) ) (or (vacio? n) (puedoComer x n)) ) (begin (taparBloque 1 t 0 0 ) (taparBloque 1 n 0 0 )(colocarFicha 1 n 0 0 caballoBlanco #\2) (ir) )  ]
   [ (and (equal? x "2") (= (EstoyLimite t ) 4) (or (= n (- (- t 16) 1)) (= n (- (+ t 16) 1))  (= n (- (- t 2) 8)) (= n (+ (- t 2) 8)) ) (or (vacio? n) (puedoComer x n)) ) (begin (taparBloque 1 t 0 0 ) (taparBloque 1 n 0 0 )(colocarFicha 1 n 0 0 caballoBlanco #\2) (ir) )  ]
   [ (and (equal? x "2") (= (EstoyLimite t ) 0) (or (= n (- (- t 16) 1)) (= n (+ (+ t 16) 1)) (= n (- (+ t 16) 1)) (= n (+ (- t 16) 1)) (= n (- (- t 2) 8)) (= n (+ (+ t 2) 8)) (= n (- (+ t 2) 8)) (= n (+ (- t 2) 8)) ) (or (vacio? n) (puedoComer x n)) ) (begin (taparBloque 1 t 0 0 ) (taparBloque 1 n 0 0 )(colocarFicha 1 n 0 0 caballoBlanco #\2) (ir) )  ]
   
   ( else (ir)  )
  )

)
;--------------------------- torre -------------------------------------------

(define (torre t x )

  (torre2 t x (CasillaTocadaX (get-mouse-click ventana))  )
)

(define (torre2 t x n)

 (cond
   [ (and (equal? x "3") (puedeAlfil? t n 0 8 8)  ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 torreBlanca #\3) (ir) )  ]
   [ (and (equal? x "3") (encontrarRango 0 t n 1 8)  ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 torreBlanca #\3) (ir) )  ]
   [ (and (equal? x "3") (puedeAlfil? t n 0 -8 -8)  ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 torreBlanca #\3) (ir) )  ]

   [ (and (equal? x "C") (puedeAlfil? t n 0 8 8)  ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 torreNegro #\C) (comprobarEstado n t torreNegro #\C) )  ]
   [ (and (equal? x "C") (encontrarRango 0 t n 1 8)  ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 torreNegro #\C) (comprobarEstado n t torreNegro #\C) )  ]
   [ (and (equal? x "C") (puedeAlfil? t n 0 -8 -8)  ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 torreNegro #\C) (comprobarEstado n t torreNegro #\C) )  ]

   ( else (ir)  )

  )

)

;--------------------------- rey -------------------------------------------

(define (rey t x )
  (rey2 t x (CasillaTocadaX (get-mouse-click ventana))  )
)

(define (rey2 t x n)
 (cond
 [ (and (equal? x "5") (or (= n (+ t 8)) (= n (- t 8))  (= n (+ t 1)) (= n (- t 1))  (= n (+ t 9)) (= n (- t 9))  (= n (+ t 7)) (= n (- t 7)) ) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 reyBlanco #\5) (actualizarPos posReyB (number->string n)) (ir) )  ]
 [ (and (equal? x "E") (or (= n (+ t 8)) (= n (- t 8))  (= n (+ t 1)) (= n (- t 1))  (= n (+ t 9)) (= n (- t 9))  (= n (+ t 7)) (= n (- t 7)) ) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 reyNegro #\E) (actualizarPos posReyN (number->string n)) (ir) )  ]
 ( else (ir)  )
 )

)
;--------------------------- reina -------------------------------------------

(define (reina t x )
  (reina2 t x (CasillaTocadaX (get-mouse-click ventana))  )
)

(define (reina2 t x n)
 (cond
 [ (and (equal? x "6") (or (puedeAlfil? t n 0 -9 -9) (puedeAlfil? t n 0 9 9)  (puedeAlfil? t n 0 7 7) (puedeAlfil? t n 0 -7 -7) (puedeAlfil? t n 0 8 8) (puedeAlfil? t n 0 -8 -8) (encontrarRango 0 t n 1 8)   ) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 reinaBlanca #\6) (ir) )  ]
 [ (and (equal? x "F") (or (puedeAlfil? t n 0 -9 -9) (puedeAlfil? t n 0 9 9)  (puedeAlfil? t n 0 7 7) (puedeAlfil? t n 0 -7 -7) (puedeAlfil? t n 0 8 8) (puedeAlfil? t n 0 -8 -8) (encontrarRango 0 t n 1 8)   ) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 reinaNegro #\F) (ir) )  ]
 ( else (ir)  )
 )

)
;------------------------------------------------------------------------------------------------------------
;-------------------------------- cargar imagenes --------------------------------------------

(define peonBlanco "Peon blanco.png")
(define torreBlanca "Torre blanco.png")
(define caballoBlanco "Caballo blanco.png")
(define alfilBlanco "Alfil blanco.png")
(define reyBlanco "Rey blanco.png")
(define reinaBlanca "Reina blanco.png")
(define peonNegro "Peon negro.png")
(define torreNegro "Torre negro.png")
(define caballoNegro "Caballo negro.png")
(define alfilNegro "Alfil negro.png")
(define reyNegro "Rey negro.png")
(define reinaNegro "Reina negro.png")

;-------------------------------------------------------------------------------------------------

(define (CasillaTocadaX d) 
  (+ (add1 (quotient (posn-x (mouse-click-posn d)) 80))
    (*  (quotient (posn-y (mouse-click-posn d)) 80) 8)   )
)

(define (taparBloque i obj x y )
  (cond
    [ (= i obj) (begin ((draw-solid-rectangle ventana) (make-posn x y ) 80 80 (if (equal? (string-ref colorCasilla obj)  #\B) "beige" "brown")) (string-set! tableroBase obj #\space))]
    [ (and (= (remainder i 8) 0) ( not (= i 0))) (taparBloque (add1 i) obj 0 (+ y 80) ) ]
    [ else (taparBloque (add1 i) obj (+ x 80) y  ) ]
    
  )
)

(define (colocarFicha i obj x y ficha caracter)
  (cond
    [ (= i obj)  (begin ( ( (draw-pixmap-posn ficha ) ventana) (make-posn x y ) ) (string-set! tableroBase obj  caracter ) ) ]
    [ (and (= (remainder i 8) 0) ( not (= i 0))) (colocarFicha (add1 i) obj 0 (+ y 80) ficha caracter) ]
    [ else (colocarFicha (add1 i) obj (+ x 80) y  ficha caracter) ]
  )
)


(define (hazAccedido t )
       (cond
       [ (equal? (string-ref tableroBase t )  #\C) (torre   t "C")]
       [ (equal? (string-ref tableroBase t )  #\B) (caballo t "B")]
       [ (equal? (string-ref tableroBase t )  #\D) (alfil   t "D")]
       [ (equal? (string-ref tableroBase t )  #\E) (rey     t "E")]
       [ (equal? (string-ref tableroBase t )  #\F) (reina   t "F")]
       [ (equal? (string-ref tableroBase t )  #\A) (peon    t "A")]
       
       [ (equal? (string-ref tableroBase t ) #\3) (torre   t "3")]
       [ (equal? (string-ref tableroBase t ) #\2) (caballo t "2")]
       [ (equal? (string-ref tableroBase t ) #\4) (alfil   t "4")]
       [ (equal? (string-ref tableroBase t ) #\5) (rey     t "5")]
       [ (equal? (string-ref tableroBase t ) #\6) (reina   t "6")]
       [ (equal? (string-ref tableroBase t ) #\1) (peon    t "1")]
       ( else (ir)  )
      )
)

(define (colocarFichas i x y)
 (if (= i 65)
     (ir)
     (begin
       (cond
       [ (equal? (string-ref tableroBase i) #\C) ( ( (draw-pixmap-posn torreNegro ) ventana) (make-posn x y ) )]
       [ (equal? (string-ref tableroBase i) #\B) ( ( (draw-pixmap-posn caballoNegro  ) ventana) (make-posn x y ) )]
       [ (equal? (string-ref tableroBase i) #\D) ( ( (draw-pixmap-posn alfilNegro  ) ventana) (make-posn x y ) )]
       [ (equal? (string-ref tableroBase i) #\E) ( ( (draw-pixmap-posn reyNegro  ) ventana) (make-posn x y ) )]
       [ (equal? (string-ref tableroBase i) #\F) ( ( (draw-pixmap-posn reinaNegro  ) ventana) (make-posn x y ) )]
       [ (equal? (string-ref tableroBase i) #\A) ( ( (draw-pixmap-posn peonNegro ) ventana) (make-posn x y ) )]
       
       [ (equal? (string-ref tableroBase i) #\3) ( ( (draw-pixmap-posn torreBlanca ) ventana) (make-posn x y ) )]
       [ (equal? (string-ref tableroBase i) #\2) ( ( (draw-pixmap-posn caballoBlanco ) ventana) (make-posn x y ) )]
       [ (equal? (string-ref tableroBase i) #\4) ( ( (draw-pixmap-posn alfilBlanco  ) ventana) (make-posn x y ) )]
       [ (equal? (string-ref tableroBase i) #\5) ( ( (draw-pixmap-posn reyBlanco  ) ventana) (make-posn x y ) )]
       [ (equal? (string-ref tableroBase i) #\6) ( ( (draw-pixmap-posn reinaBlanca  ) ventana) (make-posn x y ) )]
       [ (equal? (string-ref tableroBase i) #\1) ( ( (draw-pixmap-posn peonBlanco ) ventana) (make-posn x y ) )]
      )
      (cond
        [ (= (remainder i 8) 0) (colocarFichas (add1 i) 0 (+ y 80)) ]
        [ else (colocarFichas (add1 i) (+ x 80) y)]
       )
     )
     
     )

)
(define (crearTablero x y i tz)
 (if (= i 33)
     (colocarFichas 1 0 0)
     (begin
       (if (= (remainder i 4) 0)
           (begin
             ((draw-solid-rectangle ventana) (make-posn x y ) tz tz  "beige")
             (crearTablero (if (= x (* tz 7)) 0 tz) (+ y tz) (add1 i) tz) 
            )
           (begin
             ((draw-solid-rectangle ventana) (make-posn x y ) tz tz "beige")
             (crearTablero (+ x (* 2 tz)) y (add1 i) tz) 
            )
           )
        )
      )
 )

(define (ir)
  (revisarReyN)
  (if (equal? hayJaque? "S")
      ((draw-string ventana ) (make-posn 650 40) "JAQUE MATE" "RED")
      (begin
        (hazAccedido (CasillaTocadaX (get-mouse-click ventana)) )
        
      )  
   )
)

(crearBase)

