# Welcome to the Sudoku solver documentation site.



### Running the Sudoku Solver

To run the solver for a particular puzzle you need to create a text file
which defines the initial population of the puzzle. For details of this file
see the data file format section.

To obtain a printout of the solution, run the sudoku program from a command line terminal with the definition file as a an argument.
```
sudoku <definition file>
```

The solution will be printed out on the termial.

### Sudoku Inital State File

The content of the file is defined by a simple sexpression format.  Each line of the sudoku is represented by a bracketed set of nine integers with a set of
brackets enclosing the nine lines.  A blank position is represented by the
digit 0 whereas the digits 1 to nine represent themselves. An example format
is shown as follows:-

```
(
(0 3 6 0 4 0 7 0 8)
(7 0 0 0 0 1 0 5 0)
(0 4 0 8 3 0 9 6 0)
(3 0 0 1 0 2 0 8 4)
(0 0 0 0 0 0 0 0 0)
(8 7 0 6 0 4 0 0 9)
(0 6 2 0 1 5 0 4 0)
(0 1 0 7 0 0 0 0 6)
(5 0 7 0 9 0 3 2 0))
```
#### Building the Sudoku Program

The sudoku program is written in the common Lisp language and requires an SBCL
compiler/development environment as well as the buildapp application wich produces
a binary application of the Lisp code.  A Rakefile is provided to carry out
the build.  Issue the command:

```
rake sudoku
```
to build the binary image file.