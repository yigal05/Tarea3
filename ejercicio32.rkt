#lang racket
#|
- Fecha de publicación: 04/09/2023
- Hora de publicación: 1:17
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
(printf "Calculador de Cosh para X\n")
(define (factorial n)
  (if (= n 0)
      1
      ;else
      (* n (factorial (- n 1)))
      );endif
  )
(define (Sumatoria i x terminosDeseados si)
  (if (= i terminosDeseados)
      (print (add1 (exact->inexact si)))
      ;else
      (Sumatoria (add1 i) x terminosDeseados (+ si (/ (expt x (* (add1 i) 2)) (factorial (* (add1 i) 2)))))
   );endif
 )

(printf "Ingrese el valor de X:\n")
(define valor (read)) ;este identificador guarda el valor que el usuario quiere que x tome
(printf "Ingrese la cantidad de terminos:\n")
(define nterminos (read)) ;este identificador guarda la cantidad de terminos que el usuario quiere en el proceso de la operación

(Sumatoria 0 valor nterminos 0)