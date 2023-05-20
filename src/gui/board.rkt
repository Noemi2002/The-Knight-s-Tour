#lang racket/gui


(define board-size 16)

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



; The pasteboard% class that will hold and manage the knight's tour
; Parameters: none
; Output: a pasteboard% object
(define chess-board%
  (class pasteboard%
    (super-new)
    (define/override (on-paint before? dc . other)
      (when before?
        (define solved-matrix (PDC-Sol board-size '(1 . 0)))
        (PDC-Paint dc solved-matrix)))))


; Draws the chess board with the steps from the solution
; Parameters: the device context and the solution
; Output: the chess board with the steps from the solution
(define (PDC-Paint dc matrix-solution)
  (define brush (send the-brush-list find-or-create-brush "orange" 'solid))
  (define pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
  (define font (send the-font-list find-or-create-font 8 'default 'normal 'normal))
  (define-values (dc-width dc-height) (send dc get-size))
  (define cell-width (/ dc-width board-size))
  (define cell-height (/ dc-height board-size))
  

  (send dc clear)
  (send dc set-brush brush)
  (send dc set-pen pen)
  (send dc set-font font)
  
  (for* ([row (in-range board-size)] [col (in-range board-size)]
         #:when (or (and (odd? row) (even? col))
                    (and (even? row) (odd? col))))
    (define-values [x y] (values (* col cell-width) (* row cell-height)))
    (send dc draw-rectangle x y cell-width cell-height))

  (define counter 1)
  (for ([pair matrix-solution]
          [i (in-naturals)]
          [j (in-naturals)])
      (display pair)
      (send dc draw-text (number->string counter) (* (car pair) cell-width) (* (cdr pair) cell-height)
      (set! counter (+ counter 1)))))



; Define the tile size
(define tile-size 60)

; The pasteboard% that will hold and manage the chess pieces
(define board (new chess-board%))

; Toplevel window for our application
(define toplevel (new frame% [label "Chess Board"] [width (*  tile-size board-size)] [height (* tile-size board-size)]))

; The canvas which will display the pasteboard contents
(define canvas (new editor-canvas%
                    [parent toplevel]
                    [style '(no-hscroll no-vscroll)]
                    [horizontal-inset 0]
                    [vertical-inset 0]
                    [editor board]))

; Show the toplevel window
(send toplevel show #t)

