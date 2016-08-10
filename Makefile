all: sbcl-sudoku ccl-sudoku
sbcl-sudoku: sudoku.lisp
	sbcl --no-userinit --load sudoku.lisp --eval "(produce-executable)"

ccl-sudoku: sudoku.lisp
	ccl --no-init --load sudoku.lisp --eval "(produce-executable)"


.PHONY: clean

clean:
	-rm -f sbcl-sudoku ccl-sudoku *.lx32fsl *.fasl *~ TAGS
