#lang racket

(define tableroBase "!CBDFEDBCAAAAAAAA                                1111111132465423")

(define (puedeTorre t n i m res vecesbucle)
  (print vecesbucle)
  (newline)
  (if (= i vecesbucle)
      #f
      (if (= (- t m) n)
          #t
          (if (equal? (string-ref tableroBase (- t  m ) ) #\space )
                  (puedeTorre t n (add1 i) (+ m res) res vecesbucle)
                   #f
           )
        )

    )
)

(define (encontrarRango i t n z y)
  (if (= i 8)
      (void)
      (if (and (>= t z) (<= t y) )
          (if (or  (puedeTorre t n 0 1 1 (- t z ) ) (puedeTorre t n 0 -1 -1 (- y t))  ) #t #f )
          (encontrarRango (add1 i) t n (+ z 8) (+ y 8))
       )
   )
)

(encontrarRango 0 45 40 1 8)