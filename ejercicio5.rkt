#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 1:50
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa imprime tantos numeros de la serie de Perrin como el usuario desee.
|#
(printf "Entre el número de terminos que desea de la serie Perrin:\n")
(define (Perrin n)
  (if (= n 0)
      3
      ;else
      (if (= n 1)
          0
          ;else
          (if (= n 2)
              2
              ;else
              (+ (Perrin (- n 2)) (Perrin (- n 3)))
           );endif
       );endif
   );endif
)

(define (MantenerBucle i t)
  (if(= i t)
     (void)
     ;else
     (begin
      (printf "~a " (Perrin i))
      (MantenerBucle (+ i 1) t))
   );endif
)

(MantenerBucle 0 (read))