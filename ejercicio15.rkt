#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 4:22
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa recibe un numero y devuelve el factorial de ese numero.
|#
(printf "Este programa devuelve el factorial de un numero.
Salvedad: no se garantiza resultados para numeros negativos
Ingrese un numero:\n")

(define (factorial n)
  (if (= n 1)
      1
      ;else
      (* n (factorial (- n 1)))
   );endif
)

(factorial (read))