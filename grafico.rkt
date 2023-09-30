#lang racket
(require graphics/graphics)
(open-graphics)
(define ventana (open-viewport "ajedrez" 800 800 ))
((draw-solid-rectangle ventana) (make-posn 0 0 ) 800 800 "black")
(define tamañoAjedrez 80)

(define (crearBase)
  ((draw-solid-rectangle ventana) (make-posn 0 0 ) (* 8 tamañoAjedrez) (* 8 tamañoAjedrez) "brown")
  (crearTablero  0 0 1 tamañoAjedrez)
)


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

(define tableroBase (string-append "1CBDEFDBCAAAAAAAA"(make-string 32 #\space)"1111111132456423" ))




(define (CasillaTocadaX d)
  
  (hazAccedido (cond
    [ (<= (posn-x (mouse-click-posn d) ) 80) 1]
    [ (and (< (posn-x (mouse-click-posn d)) 160) (>= (posn-x (mouse-click-posn d)) 81) ) 2 ]
    [ (and (< (posn-x (mouse-click-posn d)) 240) (>= (posn-x (mouse-click-posn d)) 161) ) 3 ]
    [ (and (< (posn-x (mouse-click-posn d)) 320) (>= (posn-x (mouse-click-posn d)) 241)  ) 4 ]
    [ (and (< (posn-x (mouse-click-posn d)) 400) (>= (posn-x (mouse-click-posn d)) 321)  ) 5]
    [ (and (< (posn-x (mouse-click-posn d)) 480) (>= (posn-x (mouse-click-posn d)) 401)  ) 6]
    [ (and (< (posn-x (mouse-click-posn d)) 560) (>= (posn-x (mouse-click-posn d)) 481)  ) 7]
    [ (and (< (posn-x (mouse-click-posn d)) 690) (>= (posn-x (mouse-click-posn d)) 561) )  8 ])
    (cond
    [ (<= (posn-y (mouse-click-posn d) ) 80) 0]
    [ (and (< (posn-y (mouse-click-posn d)) 160) (>= (posn-y (mouse-click-posn d)) 81) )  1  ]
    [ (and (< (posn-y (mouse-click-posn d)) 240) (>= (posn-y (mouse-click-posn d)) 161) )  2 ]
    [ (and (< (posn-y (mouse-click-posn d)) 320) (>= (posn-y (mouse-click-posn d)) 241)  ) 3 ]
    [ (and (< (posn-y (mouse-click-posn d)) 400) (>= (posn-y (mouse-click-posn d)) 321)  ) 4]
    [ (and (< (posn-y (mouse-click-posn d)) 480) (>= (posn-y (mouse-click-posn d)) 401)  ) 5]
    [ (and (< (posn-y (mouse-click-posn d)) 560) (>= (posn-y (mouse-click-posn d)) 481)  ) 6]
    [ (and (< (posn-y (mouse-click-posn d)) 690) (>= (posn-y (mouse-click-posn d)) 561) )  7 ]
   )  )

)

(define (taparBloque i obj x y color1 color2)
  (cond
    [ (= i obj) ((draw-solid-rectangle ventana) (make-posn x y ) 80 80 (if (= (remainder (add1 color1) 2) 0) (if  (= (remainder (add1 color2) 2) 0) "brown" "beige") (if  (= (remainder (add1 color2) 2) 0) "beige" "brown")))]
    [ (and (= (remainder i 8) 0) ( not (= i 0))) (taparBloque (add1 i) obj 0 (+ y 80) color1 color2) ]
    [ else (taparBloque (add1 i) obj (+ x 80) y color1 color2) ]
    

  )
)


(define (hazAccedido x y)
       (cond
       [ (equal? (string-ref tableroBase (+ x (* y 8) ) ) #\C) (torre (+ x (* y 8) ) "C")]
       [ (equal? (string-ref tableroBase (+ x (* y 8) ))  #\B) (caballo (+ x (* y 8) ) "B")]
       [ (equal? (string-ref tableroBase (+ x (* y 8) ))  #\D) (alfil (+ x (* y 8) ) "D")]
       [ (equal? (string-ref tableroBase (+ x (* y 8) ))  #\E) (rey (+ x (* y 8) ) "E")]
       [ (equal? (string-ref tableroBase (+ x (* y 8) ))  #\F) (reina (+ x (* y 8) ) "F")]
       [ (equal? (string-ref tableroBase (+ x (* y 8) ))  #\A) (peon (+ x (* y 8) ) "A")]
       
       [ (equal? (string-ref tableroBase (+ x (* y 8) )) #\3) (torre (+ x (* y 8) ) "3")]
       [ (equal? (string-ref tableroBase (+ x (* y 8) )) #\2) (caballo (+ x (* y 8) ) "2")]
       [ (equal? (string-ref tableroBase (+ x (* y 8) )) #\4) (alfil (+ x (* y 8) ) "4")]
       [ (equal? (string-ref tableroBase (+ x (* y 8) )) #\5) (rey (+ x (* y 8) ) "5")]
       [ (equal? (string-ref tableroBase (+ x (* y 8) )) #\6) (reina (+ x (* y 8) ) "6")]
       [ (equal? (string-ref tableroBase (+ x (* y 8) )) #\1) (peon (+ x (* y 8) ) "1")]
      )
  
  ;(taparBloque 1 (+ x (* y 8) ) 0 0 y x)

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
  (CasillaTocadaX (get-mouse-click ventana))
)
(crearBase)


#|----------------------------------- MOVER FICHAS ------------------------------------|#
(define (torre x) 1)
(define (caballo x) 1)
(define (alfil x) 1)
(define (rey x) 1)
(define (reina x) 1)


(define (peon n x)
  (peonIr n x ()

)
(define (peonIr n x)


)



