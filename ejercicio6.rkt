#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 1:55
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa imprime tantos numeros de la serie de Padovan como el usuario desee.
|#
(printf "Entre el número de terminos que desea de la serie de Padovan:\n")
(define (Padovan n)
  (if (= n 0)
      1
      ;else
      (if (or (= n 1) (= n 2))
          0
          ;else
          (+ (Padovan (- n 2)) (Padovan (- n 3)))
       );endif
   );endif
)

(define (MantenerBucle i terminosDeseados)
  (if(= i terminosDeseados)
     (void)
     ;else
     (begin
      (printf "~a " (Padovan i))
      (MantenerBucle (+ i 1) terminosDeseados))
   );endif
)

(MantenerBucle 0 (read))