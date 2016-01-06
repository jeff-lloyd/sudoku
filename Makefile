all: sbcl-sudoku
sbcl-sudoku: sudoku.lisp
	buildapp --entry sudoku:main --output sbcl-sudoku --load sudoku.lisp


.PHONY: clean

clean:
	-rm sbcl-sudoku *.fasl *~ TAGS
