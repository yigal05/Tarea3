#lang racket

(define (main n numeroleido cantidadmayor150 mayor menor cantidadnegativos sumatoria)
  (if (not (= numeroleido 0))
      (if (not (= n 75))
          (begin
            (printf "~a- Ingrese un numero que no sea 0:\n" (+ n 1))
            (main
             (+ n 1)
             (read)
             (if (> numeroleido 150)(+ cantidadmayor150 1) cantidadmayor150)
             (if(> numeroleido mayor) numeroleido mayor)
             (if(< numeroleido menor) numeroleido menor)
             (if (< numeroleido 0) (+ cantidadnegativos 1) cantidadnegativos)
             (if (> numeroleido 0) (+ numeroleido sumatoria) sumatoria)
             )
            )
           ;else
      (printf "La cantidad de numeros mayores a 150 es:~a\n \nEl numero mayor es: ~a\nEl numero menor es: ~a\nLa cantidad de negativos es: ~a
El promedio de los numeros positvos es ~a" cantidadmayor150 mayor menor  (if (< numeroleido 0) (+ cantidadnegativos 1) cantidadnegativos) (exact->inexact (/ (if (> numeroleido 0) (+ numeroleido sumatoria) sumatoria) (- 75 cantidadnegativos))))
   )
      ;else
      (begin
        (printf "~a: 0 no es un valor aceptado ingrese un valor nuevo:\n" n)
        (main n (read) cantidadmayor150 mayor menor cantidadnegativos sumatoria)
        )
   )

)

(printf "1- Ingrese un numero que no sea 0:\n")
(define primerNumero (read))
(main  1 primerNumero 0 primerNumero primerNumero 0 0)
