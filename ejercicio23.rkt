#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 10:44
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa imprime cierto patron con la letra P.
|#
(define (Imprimir n)
  (if (= n 7)
      (void)
      ;else
      (begin
        (printf "~a\n" (string-append (make-string n #\space) (make-string (- 13 (* 2 n)) #\P) (make-string n #\space)))
        (Imprimir (+ n 1))
       )
   );endif
  )

(Imprimir 0)