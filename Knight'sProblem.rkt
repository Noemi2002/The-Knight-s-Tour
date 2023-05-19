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



;(PDC-Sol 8 '(1 . 0))

; Returns 5 solutions to the problem
; Parameters: the number of boxes and a list with the starting position
; Output: a list with all the boxes visited
(define (PDC-Todas n initial-position)
  (define (PDC-Sol-Wrapper position)
    (reverse (cdr (PDC-Sol n position))))

  (define (generate-solutions i current-position solutions)
    (cond
      ((or (= i 5) (null? current-position))
       solutions)
      (else
        (define solution (PDC-Sol-Wrapper current-position))
        (generate-solutions (+ i 1) (list-ref solution 0) (cons solution solutions)))))
  
  (define solutions (generate-solutions 0 initial-position '()))


  (define unique-solutions (remove-duplicates solutions))
  (cond
    ((>= (length unique-solutions) 5) (take unique-solutions 5))
    (else unique-solutions)))

(PDC-Todas 3 '(0 . 0))





