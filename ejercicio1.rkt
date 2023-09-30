#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 12:28
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa imprime por pantalla los numero de la serie fibonacci menores a 10000.
|#

(printf "Este programa presenta la serie de Fibonacci como la serie que comienza con los dígitos 1 y 0 y va
sumando progresivamente los dos últimos elementos de la serie, así: 0 1 1 2 3 5 8 13 21 34.......
Para este programa, se presentará la serie de Fibonacci hasta llegar sin sobrepasar el número 10,000.\n")

(define (Hallarfb n)
  (if (= n 0)
      0
      ;else
      (if (= n 1)
          1
          ;else
          (+ (Hallarfb (- n 1)) (Hallarfb (- n 2)))
       );endif
   );endif
)

(define (MantenerBucle i)
  (if(> ( Hallarfb i) 10000 )
     (void)
     ;else
     (begin
      (printf "~a " (Hallarfb i))
      (MantenerBucle (+ i 1)))
     );endif
)

(MantenerBucle 0)