#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 7:18
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa imprime un patron de unas letras en pantalla.
|#
(define letras "PNLJHFD") ;este identificador guarda las letras que se usaran para crear el patron.

(define (imprimir n)
  (if (= n 7)
      (void)
      ;else
      (begin
        (printf "~a\n" (string-append (make-string n #\space) (make-string (- 13 (* 2 n)) (string-ref letras n)) (make-string n #\space))) ;se ocupan 13 carácteres
        ;entonces cada vez se le resta 2n ya que es la cantidad de espacios que hay en cada linea.
        (imprimir (+ n 1))
       )
   );endif
  )

(imprimir 0)