#lang racket/gui

(define chess-piece-snip-class
  (make-object
   (class snip-class%
     (super-new)
     (send this set-classname "chess-piece-snip"))))

(send (get-the-snip-class-list) add chess-piece-snip-class)

(define chess-piece%
  (class snip%
    (init-field glyph font size)
    (super-new)
    (send this set-snipclass chess-piece-snip-class)

    (define/override (get-extent dc x y width height descent space lspace rspace)
      (when width (set-box! width size))
      (when height (set-box! height size))
      (when descent (set-box! descent 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    (define/override (draw dc x y . other)
      (send dc set-font font)
      (send dc set-text-foreground "black")
      (define-values (glyph-width glyph-height baseline extra-space)
        (send dc get-text-extent glyph font #t))
      (let ((ox (/ (- size glyph-width) 2))
            (oy (/ (- size glyph-height 2))))
        (send dc draw-text glyph (+ x ox) (+ y oy))))
    ))

(define chess-piece-data
  (hash
   "K" #\u2654 "Q" #\u2655 "R" #\u2656 "B" #\u2657 "N" #\u2658 "P" #\u2659
   "k" #\u265A "q" #\u265B "r" #\u265C "b" #\u265D "n" #\u265E "p" #\u265F))

(define (make-chess-piece id)
  (define glyph (hash-ref chess-piece-data id))
  (define font (send the-font-list find-or-create-font 20 'default 'normal 'normal))
  (new chess-piece% [glyph (string glyph)] [font font] [size 35]))

(define chess-board%
  (class pasteboard%
    (super-new)

    (define/override (on-paint before? dc . other)
      (when before?
        (draw-chess-board dc)))

    ))

(define board-size 16)
(define (draw-chess-board dc)
  (define brush (send the-brush-list find-or-create-brush "gray" 'solid))
  (define pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
  (define font (send the-font-list find-or-create-font 8 'default 'normal 'normal))
  (define-values (dc-width dc-height) (send dc get-size))
  (define cell-width (/ dc-width board-size))
  (define cell-height (/ dc-height board-size))
  (define margin 3)
    
  (send dc clear)
  (send dc set-brush brush)
  (send dc set-pen pen)
  (send dc set-font font)
  
  (for* ([row (in-range board-size)] [col (in-range board-size)]
         #:when (or (and (odd? row) (even? col))
                    (and (even? row) (odd? col))))
    (define-values [x y] (values (* col cell-width) (* row cell-height)))
    (send dc draw-rectangle x y cell-width cell-height))

  (for ([(rank index) (in-indexed '("8" "7" "6" "5" "4" "3" "2" "1"))])
    (define-values [_0 h _1 _2] (send dc get-text-extent rank font #t))
    (define y (+ (* index cell-height) (- (/ cell-height 2) (/ h 2))))
    (send dc draw-text rank margin y))
  
  (for ([(file index) (in-indexed '("a" "b" "c" "d" "e" "f" "g" "h"))])
    (define-values [w h _1 _2] (send dc get-text-extent file font #t))
    (define x (+ (* index cell-width) (- (/ cell-width 2) (/ w 2))))
    (send dc draw-text file x (- dc-height h margin))))


; Define the tile size
(define tile-size 65)
;; The pasteboard% that will hold and manage the chess pieces
(define board (new chess-board%))
;; Toplevel window for our application
(define toplevel (new frame% [label "Chess Board"] [width (*  tile-size board-size)] [height (* tile-size board-size)]))
;; The canvas which will display the pasteboard contents
(define canvas (new editor-canvas%
                    [parent toplevel]
                    [style '(no-hscroll no-vscroll)]
                    [horizontal-inset 0]
                    [vertical-inset 0]
                    [editor board]))

(send toplevel show #t)