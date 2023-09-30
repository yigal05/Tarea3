#lang racket
#|
- Fecha de publicación: 04/09/2023
- Hora de publicación: 2:23
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa imprime cierto patron con la letra A.
|#
(define (Imprimir n)
  (if (= n 8)
      (void)
      (if (= n 4)
          (begin
            (printf "~a\n" (string-append (make-string 27 #\space)
                                      (string-append  (make-string (- n 1) #\A) (make-string 0 #\space) (make-string n #\A) )
                                      (make-string (+ 27 n) #\space)))
            (Imprimir (+ n 1))
           )
          (if (> n 4)
              (begin
                (printf "~a\n" (string-append (make-string 27 #\space)
                                      (string-append  (make-string (- 8 n) #\A) (make-string (- (* n 2) 9) #\space) (make-string (- 8 n) #\A) )
                                      (make-string (+ 27 n) #\space)))
                (Imprimir (+ n 1))
                )
              (begin
                (printf "~a\n" (string-append (make-string 27 #\space)
                                      (string-append  (make-string n #\A) (make-string (- 7 (* n 2)) #\space) (make-string n #\A) )
                                      (make-string (+ 27 n) #\space)))
                (Imprimir (+ n 1))
               )
            )
          )
   )
  )

(Imprimir 1)