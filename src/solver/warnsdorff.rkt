#lang racket


(define (knight-tour n)
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

    ; remove variable
    ; store sorted moves in a variable
    (let* ((moves (next-moves x y path))
           (sorted-moves (sort moves (lambda (a b) (<= (length (next-moves (car a) (cdr a) path))
                                                       (length (next-moves (car b) (cdr b) path)))))))
      
      ; shoud be a cond
      (if (null? sorted-moves)
          path
          (let ((next-move (car sorted-moves)))
            (warnsdorff (car next-move) (cdr next-move) (cons next-move path))))))

  ; store start-x and start-y in variables
  
  (let* ((start-x 0)
         (start-y 0)
         (path (list (cons start-x start-y))))
    (reverse (warnsdorff start-x start-y path))))


(knight-tour 8)