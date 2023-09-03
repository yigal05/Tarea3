#lang racket
#|
- Fecha de publicación: 
- Hora de publicación:
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa recibe un numero del uno (1) al cinco (5) y devuelve si el numero es primo o no.
|#
(define (colocar i n m)
     (if (= i 9)
      ( if (not (= n m))
        (begin
          (newline)
          (colocar 0 (+ n 1) (- m 1))
        );else
        (void)
      )
      ;else
      (if (or (= i n) (= i m))
              (begin
                (printf "a" )
                (colocar (+ i 1) n m))
              ;else
              (begin
                (printf " " )
                (colocar (+ i 1) n m))
       )
   )
)

(colocar 0 0 8)