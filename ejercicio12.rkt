#lang racket
#|
- Fecha de publicación: 03/09/2023
- Hora de publicación: 3:24
- Versión de su código: 1.0
- Autor. Ing(c) : Yigal Fabricio Rojas Acevedo
- Nombre del lenguaje utilizado: Racket
- Versión del lenguaje utilizado : 8.10
- Presentado a: Doctor Ricardo Moreno Laverde
- Universidad Tecnológica de Pereira
- Programa de Ingeniería de Sistemas y Computación
- Asignatura IS105 Programación I
- El programa toma un numero y lo devuelve al reves.
|#
(printf "Este programa lee desde el teclado un número
entero y lo imprime al revés.
Entre el número:\n")

(define numero ( ~a ( read ) ) ) ;este identificador se usa para guardar el numero que se ha ingresado en formato string
(define tamaño ( - (string-length numero) 1)) ;este identificador se usa para guardar el tamaño de numero - 1 para que se pueda usar como index en un ref
(define ( voltear n )
  (if ( = n 0 )
      ( printf "~a" ( string-ref numero 0 ))
      ;else
      ( begin
        (printf "~a" ( string-ref numero n ))
        (voltear ( - n 1 ))
       )
    );endif
)

(voltear tamaño)