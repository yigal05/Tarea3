#lang racket
(require graphics/graphics)
(open-graphics)

(define urano "urano.png")
(define neptuno "neptuno.png")
(define jupiter "jupiter.png")
(define saturno "saturno.jpg")
(define marte "marte.jpg")
(define tierra "tierra.jpg")
(define venus "venus.jpg")
(define mercurio "mercurio.png")


(define ventana (open-viewport "Memorama" 800 800 ))
((draw-solid-rectangle ventana) (make-posn 0 0 ) 800 800 "black")

(define orden "uunnjjssmmttvvMM")


(define (devolver x)
  (if (equal? x #\u)
      urano
      (if (equal? x #\n)
          neptuno
          (if (equal? x #\j)
              jupiter
              (if (equal? x #\s)
                  saturno
                  (if (equal? x #\m)
                      marte
                      (if (equal? x #\t)
                          tierra
                          (if (equal? x #\v)
                              venus
                              (if (equal? x #\M)
                                  mercurio
                                  (void)
                                  )
                              )
                          )
                      )
                  )
              )
          )
      )



)


(define (imprimirFotos i x y)

  (if (= i 17)
      (void)
      (if (= (remainder i 4) 0)
          (begin
            (((draw-pixmap-posn (devolver (string-ref orden i)) ) ventana) (make-posn x y) )
            (imprimirFotos (add1 i) 0 (+ y 200))
           )
           (begin
             (((draw-pixmap-posn (devolver (string-ref orden i)) ) ventana) (make-posn x y) )
             (imprimirFotos (add1 i) (+ x 200) y)
           )
       )
   )
  
)
(imprimirFotos 1 0 0)