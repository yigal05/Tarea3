#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 11:13
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa imprime cierto patron con la letra Z.
|#
(define (Imprimir n)
  (if (= n 8)
      (void)
      ;else
      (if (< n 4)
          (begin
            (printf "~a\n" (string-append (make-string (- 10 n) #\space) (string-append (make-string (- n 1) #\Z) "Z" (make-string (- n 1) #\Z) )  ) )
            (Imprimir (+ n 1))
          )
          ;else
          (begin
            (printf "~a\n" (string-append (make-string (- 10 (- 8 n) ) #\space) (string-append (make-string (- 7 n) #\Z) "Z" (make-string (- 7 n) #\Z) )  ) )
            (Imprimir (+ n 1))
           )
       );endif
   );endif
)

(Imprimir 1)