#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 4:15
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa imprime las tablas del 1 al 10.
|#
(define (tabla numero n)
   (if (< n 11)
       (begin
         (printf "~a x ~a = ~a \n" numero n (* numero n) )
         (tabla numero (+ n 1)))
       ;else
        (newline)
   );endif
 
)

(define (llamarTablas n)
  (if (= n 10)
      (tabla n 1)
      ;else
      (begin
        (tabla n 1)
        (llamarTablas (+ n 1))
        )
    );endif
)


(llamarTablas 1)