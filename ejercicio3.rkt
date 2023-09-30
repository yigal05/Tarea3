#lang racket
#|
- Fecha de publicación: 1:28
- Hora de publicación: 03/09/2023
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa imprime tantos numeros de la serie de Lucas como el usuario desee.
|#
(printf "Entre el número de terminos que desea de la serie Lucas:\n")

(define (Lucas n)
  (if (= n 0)
      2
      ;else
      (if (= n 1)
          1
          ;else
          (+ (Lucas (- n 1)) (Lucas (- n 2)))
       );endif
   );endif
)

(define (MantenerBucle i t)
  (if(= i t)
     (void)
     ;else
     (begin
      (printf "~a " (Lucas i))
      (MantenerBucle (+ i 1) t))
     );endif
)

(MantenerBucle 0 (read))