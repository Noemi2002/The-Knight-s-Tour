#lang racket/gui


(define chess-board%
  (class pasteboard%
    (super-new)
    (define/override (on-paint before? dc . other)
      (when before?
        (draw-chess-board dc)))))

(define board-size 16)
(define (draw-chess-board dc)
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
    (send dc draw-rectangle x y cell-width cell-height)))



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
