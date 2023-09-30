#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 7:08
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa imprime cierto patron de unas letras en pantalla.
|#
(define (Imprimir n)
  (if (= n 80)
      (printf "~a" (string-append (make-string (- 80 n) #\space) (make-string n #\a) ))
      (begin
        (printf "~a\n" (string-append (make-string (- 80 n) #\space) (make-string n #\a) ) )
        (Imprimir (+ n 1))
       )
   )
 )

(Imprimir 1)