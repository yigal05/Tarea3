#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 1:01
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa suma los numero del 0 100 que pertenecen a la serie de fibonacci.
|#

(printf "Este programa presenta la suma de los elementos de la serie de Fibonacci entre 0 y 100.
Los números a sumar son:\n")

(define (HallarFibonacci n)
  (if (= n 0)
      0
      ;else
      (if (= n 1)
          1
          ;else
          (+ (HallarFibonacci (- n 1)) (HallarFibonacci (- n 2)))
       );endif
   );endif
)

(define (SumarFibonacci sumatoria n)
  (if (> (HallarFibonacci n) 100)
      (printf "y su suma es: ~a " sumatoria )
      ;else
      (begin
        (printf "~a, " (HallarFibonacci n) )
        (SumarFibonacci (+ sumatoria (HallarFibonacci n)) (+ n 1))       
      )  
   );endif
)
  
(SumarFibonacci 0 0)
