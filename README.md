## A lisp program to solve the sudoku puzzle

This program solves a 9 * 9 sudoku puzzle using a full tree search until
a solution is found.  It does not attempt to find all solutions.

### Solution Method

The program searches the grid from top left to bottom right for the first
empty grid location.  It then computes all possible values that can be entered
at the location.  Using this list it calls itself to attempt to solve the next
empty grid position.  If it fails then it uses the next number in the list
until it either produces a solution or fails because the list is empty.