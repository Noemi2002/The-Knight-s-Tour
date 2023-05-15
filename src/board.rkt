#lang racket/gui

(define board-width 8)
(define board-height 8)
(define cell-size 50)

(define (draw-board canvas)
  (let loop ((x 0) (y 0) (color #f))
    (cond ((>= x board-width))
          ((>= y board-height)
           (loop (add1 x) 0 (not color)))
          (else
           (send canvas set-brush (if color "tan" "white") 'solid)
           (send canvas draw-rectangle (* x cell-size) (* y cell-size) cell-size cell-size #t)
           (loop x (add1 y) (not color))))))

(define frame (new frame% [label "Chess Board"] [width (* board-width cell-size)] [height (* board-height cell-size)]))
(define canvas (new canvas% [parent frame]
                            [style '(border)]
                            [min-width (* board-width cell-size)]
                            [min-height (* board-height cell-size)]
                            [paint-callback (lambda (canvas dc) (draw-board canvas))]))

(send frame show #t)

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
    (let* ((moves (next-moves x y path))
           (sorted-moves (sort moves (lambda (a b) (<= (length (next-moves (car a) (cdr a) path))
                                                       (length (next-moves (car b) (cdr b) path)))))))
      (if (null? sorted-moves)
          path
          (let ((next-move (car sorted-moves)))
            (warnsdorff (car next-move) (cdr next-move) (cons next-move path))))))

  (let* ((start-x 0)
         (start-y 0)
         (path (list (cons start-x start-y))))
    (reverse (warnsdorff start-x start-y path))))


(knight-tour 8)