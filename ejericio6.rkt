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
(define (HallarFibonacci n)
  
  (if (= n 0)
      1
      (if (= n 1)
          0
          (if (= n 2)
              0
              (+ (HallarFibonacci (- n 2)) (HallarFibonacci (- n 3)))
            )
       )
   )
)

(define numero (read))
(define (LlamarFb i)
  (if (< i numero)
      (begin
        (printf "~a " (HallarFibonacci i))
         (LlamarFb (+ i 1)))
      (void))
  )
(LlamarFb 0)