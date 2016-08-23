all: sbcl-sudoku ccl-sudoku
sbcl-sudoku: sudoku.lisp
	buildapp --eval '(require :sb-posix)' --asdf-path ~/common-lisp/systems --load-system sudoku --entry sudoku:main --output sbcl-sudoku

ccl-sudoku: sudoku.lisp
	ccl --no-init --load sudoku.lisp --eval "(produce-executable)"


.PHONY: clean

clean:
	-rm -f sbcl-sudoku ccl-sudoku *.lx32fsl *.fasl *~ TAGS
