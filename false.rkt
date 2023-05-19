#lang racket

(define (caballo? n sol)
  (define (valido? x y)
    (and (>= x 0) (< x n) (>= y 0) (< y n) (not (member (cons x y) sol))))
  
  (define (adyacentes x y)
    (filter (lambda (p) (valido? (car p) (cdr p)))
            `((,(+ x 2) . ,(if (< y 6) +1 -1))
              (,(+ x 2) . ,(if (< y 6) -1 +1))
              (,(+ x 1) . ,(if (< y 7) +2 -2))
              (,(+ x 1) . ,(if (< y 7) -2 +2))
              (,(- x 2) . ,(if (< y 2) +1 -1))
              (,(- x 2) . ,(if (< y 2) -1 +1))
              (,(- x 1) . ,(if (< y 1) +2 -2))
              (,(- x 1) . ,(if (< y 1) -2 +2)))))
  
  (define (dfs x y)
    (let loop ((stack (list (cons x y))))
      (cond
        ((null? stack) #t)
        ((valido? x y)
         (let ((next (adyacentes x y)))
           (set! sol (cons (cons x y) sol))
           (or (dfs (car (car next)) (cdr (car next)))
               (loop (cdr next)))))
        (else #f))))
  (dfs 0 0))






(define solu '((0 . 0) (2 . 1) (0 . 2) (2 . 3) (0 . 4)
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

(caballo? 8 solu)



