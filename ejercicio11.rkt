#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 3:19
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa imprime tantos numeros de la serie Triangular como el usuario desee.
|#
(printf "Entre el número de terminos que desea de la serie Triangular:\n")
(define (Triangular i sumatoria numerodeterminos)
  (if(= i numerodeterminos)
     (void)
     ;else
     (if (= i 1)
         (begin
           (printf "1 ~a "   (+ sumatoria (add1 i) ) )
           (Triangular (add1 i) (+ sumatoria (add1 i) ) numerodeterminos)
         )
         ;else
         (begin
           (printf "~a "   (+ sumatoria (add1 i) ) )
           (Triangular (add1 i) (+ sumatoria (add1 i) ) numerodeterminos)
         );endif
      )
   );endif
)

(Triangular 1 1 (read))

