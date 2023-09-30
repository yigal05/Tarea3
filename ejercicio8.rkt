#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 2:30
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa imprime tantos numeros de la serie de Catalan como el usuario desee.
|#
(printf "Entre el número de terminos que desea de la serie de Catalan\n")
(define (factorial n)
  (if (= n 1)
      1
      ;else
      (* n (factorial (- n 1)))
   );endif
)

(define (Catalan n)
  (if (= n 0)
      1
      ;else
      (/ (factorial (* 2 n)) (* (factorial n) (factorial (+ n 1))))
   );endif
)

(define (MantenerBucle i terminosDeseados)
  (if(= i terminosDeseados)
     (void)
     ;else
     (begin
      (printf "~a " (Catalan i))
      (MantenerBucle (+ i 1) terminosDeseados))
   );endif
)

(MantenerBucle 0 (read))