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
