#lang racket


; Travese a Matrix and print each row on a new line
; Input: Matrix
; Output: None
; Example: (traverse-matrix (chess-board))
(define (traverse-matrix matrix)
    (cond
        ; Base case, empty matrix
        [(empty? matrix) (newline)]
        (else 
            [begin (displayln (car matrix)) 
                (traverse-matrix (cdr matrix))])))


; 8x8 chess board
(define (chess-board) (list (list 1 0 1 0 1 0 1 0)
                            (list 0 1 0 1 0 1 0 1)
                            (list 1 0 1 0 1 0 1 0)
                            (list 0 1 0 1 0 1 0 1)
                            (list 1 0 1 0 1 0 1 0)
                            (list 0 1 0 1 0 1 0 1)
                            (list 1 0 1 0 1 0 1 0)
                            (list 0 1 0 1 0 1 0 1)))


; Call the function and print the result
(traverse-matrix (chess-board))

