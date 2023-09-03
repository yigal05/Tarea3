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
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))
      ))


(define (sumatoria n si)
(if (not (= n 0))
    (sumatoria (- n 1) (+ (factorial n) si))
    si
 )
 )

(sumatoria 4 1)