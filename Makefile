all: sbcl-sudoku ccl-sudoku
sbcl-sudoku: sudoku.lisp
#	buildapp --entry sudoku:main --output sbcl-sudoku --load sudoku.lisp
	sbcl --no-userinit --load sudoku.lisp --eval "(sudoku::produce-executable)"

ccl-sudoku: sudoku.lisp
	ccl --no-init --load sudoku.lisp --eval "(sudoku::produce-executable)"


.PHONY: clean

clean:
	-rm sbcl-sudoku ccl-sudoku *.lx32fsl *.fasl *~ TAGS
