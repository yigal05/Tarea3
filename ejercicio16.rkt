#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 4:41
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa recibe un numero y devuelve la suma de los factoriales de ese numero.
|#
(printf "Este programa devuelve la suma de los factoriales de un numero.
Salvedad: Solo garantiza resultados para numeros enteros positivos.
Ingrese un numero:\n")

(define (factorial n)
  (if (= n 1)
      1
      ;else
      (* n (factorial (- n 1)))
   );endif
)

(define (sumatoria n si)
  (if (not (= n 0))
      (sumatoria (- n 1) (+ (factorial n) si))
      ;else
      (print si)
   );endif
)

(define (Llamarsumatoria n si)
  (if (and (integer? n) (> n 0) )
      (sumatoria n 1)
      ;else
      (begin
        (printf "Valor no valido ingrese un nuevo numero:\n")
        (Llamarsumatoria (read) 0)
       )
   );endif
 )

(Llamarsumatoria (read) 0)