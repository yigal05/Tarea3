#lang racket
#|
- Fecha de publicación: 04/09/2023
- Hora de publicación: 1:28
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa calcula Cosh con la serie de taylor, pide el valor x y el numero determinos.
|#
(define (Sumatoria i x terminosDeseados si)
  (if (= i  terminosDeseados )
      (print (- (- x 1) (exact->inexact si)))
      ;else
      (if (= (remainder i 2) 0)
          (Sumatoria (add1 i) x terminosDeseados (+ si (/ (expt (- x 1) i ) i )))
          ;else
          (Sumatoria (add1 i) x terminosDeseados (- si (/ (expt (- x 1) i ) i )))
       )
   )
  )

(printf "Ingrese el valor de X:\n")
(define valor (read)) ;este identificador guarda el valor que el usuario quiere que x tome
(printf "Ingrese la cantidad de terminos:\n")
(define nterminos (read)) ;este identificador guarda la cantidad de terminos que el usuario quiere en el proceso de la operación

(Sumatoria 2 valor (+ nterminos 2) 0)