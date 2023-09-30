#lang racket
(require graphics/graphics)
(open-graphics)
;----------------------------------------------------

(define ventana (open-viewport "ajedrez" 800 800 ))
((draw-solid-rectangle ventana) (make-posn 0 0 ) 800 800 "black")
(define tamañoAjedrez 80)
(define colorCasilla "XBNBNBNBNNBNBNBNBBNBNBNBNNBNBNBNBBNBNBNBNNBNBNBNBBNBNBNBNNBNBNBNB")
(define (crearBase)
  ((draw-solid-rectangle ventana) (make-posn 0 0 ) (* 8 tamañoAjedrez) (* 8 tamañoAjedrez) "brown")
  (crearTablero  0 0 1 tamañoAjedrez)
)

(define tableroBase (string-copy (string-append "1CBDEFDBCAAAAAAAA"(make-string 32 #\space)"1111111132456423" )))

#|----------------------------------- MOVER FICHAS ------------------------------------|#
(define (torre x) 1)
(define (rey x) 1)
(define (reina x) 1)

;--------------------------- Peon -------------------------------------------

(define (peon t x )
  (peon2 t x (CasillaTocadaX (get-mouse-click ventana))  )
)

(define (peon2 t x n)

 (cond
 [ (and (equal? x "1") (= n (- t 8)) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 peonBlanco #\1) (ir) )  ]
 [ (and (equal? x "A") (= n (+ t 8)) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 peonNegro #\A) (ir) )  ]
 [ (and (equal? x "1") (or (= n (+ (- t 8) 1) ) (= n (- (- t 8) 1)) ) (not (equal? (string-ref tableroBase n) #\space)) ) (begin (taparBloque 1 t 0 0 ) (taparBloque 1 n 0 0 )  (colocarFicha 1 n 0 0 peonBlanco #\1) (ir) )  ]
 [ (and (equal? x "A") (or (= n (+ (+ t 8) 1) ) (= n (- (+ t 8) 1)) ) (not (equal? (string-ref tableroBase n) #\space)) ) (begin (taparBloque 1 t 0 0 )  (taparBloque 1 n 0 0 ) (colocarFicha 1 n 0 0 peonNegro #\A) (ir) )  ]
  )

)

;--------------------------- Alfil -------------------------------------------

(define (puedeAlfil? t n i m res)
  (print (- t m))
  (newline)
  (if (= i 8)
      #f
      (if (= (- t m) n)
          #t
          (if (equal? (string-ref tableroBase (- t m) ) #\space )
              (puedeAlfil? t n (add1 i) (+ m res) res)
              #f
            )
       )

    )

)

(define (alfil t x )
  (alfil2 t x (CasillaTocadaX (get-mouse-click ventana))  )
)

(define (alfil2 t x n)
 (cond
 [ (and (equal? x "4") (puedeAlfil? t n 0 9 9) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 alfilBlanco #\4) (ir) )  ]
 [ (and (equal? x "4") (puedeAlfil? t n 0 7 7) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 alfilBlanco #\4) (ir) )  ]
 [ (and (equal? x "4") (puedeAlfil? t n 0 -9 -9) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 alfilBlanco #\4) (ir) )  ]
 [ (and (equal? x "4") (puedeAlfil? t n 0 -7 -7) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 alfilBlanco #\4) (ir) )  ]
  )

)
;--------------------------- Caballo -------------------------------------------
(define (caballo t x )
  (caballo2 t x (CasillaTocadaX (get-mouse-click ventana))  )
)

(define (caballo2 t x n)
 (cond
   [ (and (equal? x "2") (= n (- (- t 16) 1)) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 caballoBlanco #\2) (ir) )  ]
   [ (and (equal? x "2") (= n (+ (+ t 16) 1)) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 caballoBlanco #\2) (ir) )  ]
   [ (and (equal? x "2") (= n (- (+ t 16) 1)) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 caballoBlanco #\2) (ir) )  ]
   [ (and (equal? x "2") (= n (+ (- t 16) 1)) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 caballoBlanco #\2) (ir) )  ]
   [ (and (equal? x "2") (= n (- (- t 2) 8)) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 caballoBlanco #\2) (ir) )  ]
   [ (and (equal? x "2") (= n (+ (+ t 2) 8)) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 caballoBlanco #\2) (ir) )  ]
   [ (and (equal? x "2") (= n (- (+ t 2) 8)) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 caballoBlanco #\2) (ir) )  ]
   [ (and (equal? x "2") (= n (+ (- t 2) 8)) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 caballoBlanco #\2) (ir) )  ]

   [ (and (equal? x "B") (= n (- (- t 16) 1)) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 caballoNegro #\B) (ir) )  ]
   [ (and (equal? x "B") (= n (+ (+ t 16) 1)) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 caballoNegro #\B) (ir) )  ]
   [ (and (equal? x "B") (= n (- (+ t 16) 1)) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 caballoNegro #\B) (ir) )  ]
   [ (and (equal? x "B") (= n (+ (- t 16) 1)) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 caballoNegro #\B) (ir) )  ]
   [ (and (equal? x "B") (= n (- (- t 2) 8)) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 caballoNegro #\B) (ir) )  ]
   [ (and (equal? x "B") (= n (+ (+ t 2) 8)) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 caballoNegro #\B) (ir) )  ]
   [ (and (equal? x "B") (= n (- (+ t 2) 8)) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 caballoNegro #\B) (ir) )  ]
   [ (and (equal? x "B") (= n (+ (- t 2) 8)) (equal? (string-ref tableroBase n) #\space) ) (begin (taparBloque 1 t 0 0 ) (colocarFicha 1 n 0 0 caballoNegro #\B) (ir) )  ]

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
  
  (+ (cond
    [ (<= (posn-x (mouse-click-posn d) ) 80) 1]
    [ (and (< (posn-x (mouse-click-posn d)) 160) (>= (posn-x (mouse-click-posn d)) 81) ) 2 ]
    [ (and (< (posn-x (mouse-click-posn d)) 240) (>= (posn-x (mouse-click-posn d)) 161) ) 3 ]
    [ (and (< (posn-x (mouse-click-posn d)) 320) (>= (posn-x (mouse-click-posn d)) 241)  ) 4 ]
    [ (and (< (posn-x (mouse-click-posn d)) 400) (>= (posn-x (mouse-click-posn d)) 321)  ) 5]
    [ (and (< (posn-x (mouse-click-posn d)) 480) (>= (posn-x (mouse-click-posn d)) 401)  ) 6]
    [ (and (< (posn-x (mouse-click-posn d)) 560) (>= (posn-x (mouse-click-posn d)) 481)  ) 7]
    [ (and (< (posn-x (mouse-click-posn d)) 690) (>= (posn-x (mouse-click-posn d)) 561) )  8 ])
    (* (cond
    [ (<= (posn-y (mouse-click-posn d) ) 80) 0]
    [ (and (< (posn-y (mouse-click-posn d)) 160) (>= (posn-y (mouse-click-posn d)) 81) )  1  ]
    [ (and (< (posn-y (mouse-click-posn d)) 240) (>= (posn-y (mouse-click-posn d)) 161) )  2 ]
    [ (and (< (posn-y (mouse-click-posn d)) 320) (>= (posn-y (mouse-click-posn d)) 241)  ) 3 ]
    [ (and (< (posn-y (mouse-click-posn d)) 400) (>= (posn-y (mouse-click-posn d)) 321)  ) 4]
    [ (and (< (posn-y (mouse-click-posn d)) 480) (>= (posn-y (mouse-click-posn d)) 401)  ) 5]
    [ (and (< (posn-y (mouse-click-posn d)) 560) (>= (posn-y (mouse-click-posn d)) 481)  ) 6]
    [ (and (< (posn-y (mouse-click-posn d)) 690) (>= (posn-y (mouse-click-posn d)) 561) )  7 ]
   ) 8 )  )

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

(define (ComerFicha i obj x y ficha caracter)
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
  (hazAccedido (CasillaTocadaX (get-mouse-click ventana)) )
)
(crearBase)









