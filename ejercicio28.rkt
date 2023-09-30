#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 12:04
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa imprime una figura conformada por letras A.
|#
(define (Imprimir n)
  (if (= n 12)
      (void)
      ;else
      (if (< n 6)
          (begin
            (printf "~a\n" (string-append (make-string (- 40 n) #\space) (string-append (make-string (- n 1) #\A) "A"  )  ) )
            (Imprimir (+ n 1))
          )
          ;else
          (begin
            (printf "~a\n" (string-append (make-string (- 36 (- 8 n) ) #\space) (string-append (make-string (- 11 n) #\A) "A" )  ) )
            (Imprimir (+ n 1))
           )
       );endif
   );endif
 )

(Imprimir 1)