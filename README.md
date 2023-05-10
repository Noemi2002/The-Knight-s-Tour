# The Knight's Tour

The Knight's Tour is a chess puzzle in which the objective is to find a sequence of moves by a knight on a chessboard such that all squares are visited exactly once. The knight's tour problem is the mathematical problem of finding a knight's tour. Creating a program to find a knight's tour is a common problem given to computer science students.

The knight is placed on the first block of an empty board and, moving according to the rules of chess, must visit each square exactly once.

## Approach

The knight's tour problem can be viewed as a graph traversal problem, where each square is a node and each valid move is an edge. The problem is to find a path that visits every node exactly once. This is called a Hamiltonian path problem. There are several algorithms available to solve this problem, including Warnsdorff's algorithm and the Warnsdorff's algorithm with backtracking.

### Algorithm

The Warnsdorff's algorithm is a heuristic for finding a knight's tour. The knight is moved so that it always proceeds to the square from which the knight will have the fewest onward moves. When calculating the number of onward moves for each candidate square, we do not count moves that revisit any square already visited. It is, of course, possible to have two or more choices for which the number of onward moves is equal. There are several ways of dealing with this. One way is to use backtracking: if a dead end is reached, the algorithm goes back to the last point in the path that had more than one option to try a different option.

### Implementation in Racket

```racket

#lang racket

(display "Hello World\n")
```

---

# How to create an executable

`$ raco exe -o hello hello.rkt`

This will create an executabe hello or hello.exe depending on your platform.

For help:

`$ ./hello -h or hello.exe -h`