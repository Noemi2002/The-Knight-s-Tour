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


;Verifies if a given structure is a valid solution
;Parameters: the number of boxes and a list-matrix with the solution
;Output: a boolean
(define (PDC-Test n sol)
  (define (valido? x y sol)
    (and (>= x 0) (< x n) (>= y 0) (< y n) (not (member (cons x y) sol))))

  (define (adyacentes x y sol)
    (filter (lambda (p) (valido? (car p) (cdr p) sol))
            `((,(+ x 2) . ,(if (< y 6) +1 -1))
              (,(+ x 2) . ,(if (< y 6) -1 +1))
              (,(+ x 1) . ,(if (< y 7) +2 -2))
              (,(+ x 1) . ,(if (< y 7) -2 +2))
              (,(- x 2) . ,(if (< y 2) +1 -1))
              (,(- x 2) . ,(if (< y 2) -1 +1))
              (,(- x 1) . ,(if (< y 1) +2 -2))
              (,(- x 1) . ,(if (< y 1) -2 +2)))))

  (define (backtrack x y visited)
    (cond
      ((valido? x y visited)
       (or (backtrack (car (car (adyacentes x y visited))) (cdr (car (adyacentes x y visited))) (cons (car (adyacentes x y visited)) visited))
           (backtrack x y (cdr visited))))
      ((= (length visited) (* n n)) #t)  ; Se visitaron todas las casillas
      (else #f)))

  (backtrack (caar sol) (cdar sol) (cdr sol)))


#|(define solu '((0 . 0) (2 . 1) (0 . 2) (2 . 3) (0 . 4)
  (2 . 5) (0 . 6) (2 . 7) (1 . 1) (3 . 0)
  (1 . 3) (3 . 2) (1 . 5) (3 . 4) (1 . 7)
  (3 . 6) (2 . 2) (0 . 1) (2 . 0) (0 . 3)
  (2 . 4) (0 . 7) (2 . 6) (1 . 2) (3 . 1)
  (1 . 4) (3 . 3) (1 . 6) (3 . 5) (2 . 3)
  (0 . 6) (2 . 5) (0 . 4) (2 . 7) (0 . 0)
  (2 . 1) (0 . 2) (2 . 3) (0 . 6) (2 . 5)
  (0 . 4) (2 . 7) (0 . 0) (2 . 1) (0 . 2)
  (2 . 3) (1 . 1) (3 . 0) (1 . 3) (3 . 2)
  (1 . 5) (3 . 4) (1 . 7) (3 . 6) (2 . 2)
  (0 . 1) (2 . 0) (0 . 3) (2 . 4) (0 . 7)
  (2 . 6) (1 . 2) (3 . 1) (1 . 4) (3 . 3)
  (1 . 6) (3 . 5) (2 . 3) (0 . 5))) 

(PDC-Test 8 solu)|#


;(PDC-Todas 16 '(0 . 0))
;(PDC-Todas 3 '(0 . 0))






