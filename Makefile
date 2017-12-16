all: sudoku sudoku-vector
sudoku: sudoku.lisp
	buildapp --eval '(require :sb-posix)' --asdf-path ~/common-lisp/systems --load-system sudoku --entry main --output sudoku

sudoku-vector: sudoku-vector.lisp
	buildapp --eval '(require :sb-posix)' --asdf-path ~/common-lisp/systems --load-system sudoku-vector --entry main --output sudoku-vector

ccl-sudoku: sudoku.lisp
	ccl-buildapp --asdf-path ~/common-lisp/systems --load-system sudoku --entry main --output ccl-sudoku

ccl-sudoku-vector: sudoku-vector.lisp
	ccl-buildapp --eval --asdf-path ~/common-lisp/systems --load-system sudoku-vector --entry main --output ccl-sudoku-vector


.PHONY: clean

clean:
	-rm -f sudoku *.fasl ccl-sudoku ccl-sudoku-vector sudoku-vector
