#lang racket

;Returns just a solution to the problem
;Parameters: the number of boxes and a list with the starting position
;Output: a list with all the boxes visited
(define (PDC-Sol n start)

  (define (valid-position? x y path)
    (and (<= 0 x (- n 1))
         (<= 0 y (- n 1))
         (not (member (cons x y) path))))
  
  (define (next-moves x y path)
    (filter (lambda (pos) (valid-position? (car pos) (cdr pos) path))
            (list (cons (+ x 1) (+ y 2))
                  (cons (+ x 2) (+ y 1))
                  (cons (+ x 2) (- y 1))
                  (cons (+ x 1) (- y 2))
                  (cons (- x 1) (- y 2))
                  (cons (- x 2) (- y 1))
                  (cons (- x 2) (+ y 1))
                  (cons (- x 1) (+ y 2)))))
  
  (define (warnsdorff x y path)
  (cond ((null? (sort (next-moves x y path) (lambda (a b)
                                              (<= (length (next-moves (car a) (cdr a) path))
                                                  (length (next-moves (car b) (cdr b) path)))))) path)
                                                      
        (else (warnsdorff (car (car (sort (next-moves x y path) (lambda (a b)
                                                                     (<= (length (next-moves (car a) (cdr a) path))
                                                                         (length (next-moves (car b) (cdr b) path)))))))

                          (cdr (car (sort (next-moves x y path) (lambda (a b)
                                                                     (<= (length (next-moves (car a) (cdr a) path))
                                                                         (length (next-moves (car b) (cdr b) path)))))))

                          (cons (car (sort (next-moves x y path) (lambda (a b)
                                                                  (<= (length (next-moves (car a) (cdr a) path))
                                                                      (length (next-moves (car b) (cdr b) path)))))) path)))))

  (reverse (warnsdorff (car start) (cdr start) (list start))))



(PDC-Sol 3 '(0 . 0))





