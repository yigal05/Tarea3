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
(define (imprimir n)
  (if (= n 7)
      (void)
      (begin
        (printf "~a\n" (string-append (make-string n #\space) (make-string (- 13 (* 2 n)) #\P) (make-string n #\space)))
        (imprimir (+ n 1))
       )
   )
  )

(imprimir 0)