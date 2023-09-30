#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 6:22
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa imprime cierto patron de una letras en pantalla.
|#
(define (Colocar i n m )
     ( if (= i 8)
          
      ( if (not (= n 3)) 
        (begin
          (newline)
          (Colocar 0 (+ n 1) (- m 1))
         )
        ;else
        (void)
       );endif
      
      ;else
      ( if (or (= i n) (= i m))
              (begin
                (printf "a" )
                (Colocar (+ i 1) n m))
              ;else
              (begin
                (printf " " )
                (Colocar (+ i 1) n m))
       )
   )
)

(Colocar 0 0 7)