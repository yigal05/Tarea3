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
(define numero (~a(read)))
(define tamaño (- (string-length numero) 1))
(define (volterar n)
  (if (= n 0)
      (printf "~a" (string-ref numero 0))
      (begin
        (printf "~a" (string-ref numero n))
        (volterar (- n 1))
        )
    )
)

(volterar tamaño)