#lang racket



(define (solucion tamano)
  (define (solucionar-tour camino)
    (cond ((= (length camino) (* tamano tamano)) camino)
          (else (encontrar-solucion camino (mover (car camino) tamano)))))
  
  (define (mover posicion tamano)
    (define (check-pos pos) ;x=card pos, y=cdr pos
      (and (>= (car pos) 0)
           (< (car pos) tamano)
           (>= (cdr pos) 0)
           (< (cdr pos) tamano)
           (not (member pos path))))
    (posicion-valida check-pos (list (cons (+ (car posicion) 1) (+ (cdr posicion) 2))
                                     (cons (+ (car posicion) 2) (+ (cdr posicion) 1))
                                     (cons (+ (car posicion) 2) (- (cdr posicion) 1))
                                     (cons (+ (car posicion) 1) (- (cdr posicion) 2))
                                     (cons (- (car posicion) 1) (- (cdr posicion) 2))
                                     (cons (- (car posicion) 2) (- (cdr posicion) 1))
                                     (cons (- (car posicion) 2) (+ (cdr posicion) 1))
                                     (cons (- (car posicion) 1) (+ (cdr posicion) 2)))))

   (define (posicion-valida pred lista)
  (cond ((null? lista) lista)
        ((and (pred (car lista)) (posicion-valida pred (cdr lista))))
        (else (cons (car lista) (posicion-valida pred (cdr lista))))))

  (define (encontrar-solucion camino posiciones)
    (cond ((null? posiciones) posiciones)
          ((solucionar-tour (cons (car posiciones) camino)))
          (else (encontrar-solucion camino (cdr posiciones))))))


