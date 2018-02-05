all: sudoku sudoku-vector ccl-sudoku ccl-sudoku-vector
switches = --asdf-path ~/common-lisp/systems --manifest-file manifest-file.txt
sudoku: sudoku.lisp
	buildapp --eval '(require :sb-posix)'\
	 $(switches)\
	 --load-system $@ --entry main --output $@

sudoku-vector: sudoku-vector.lisp
	buildapp --eval '(require :sb-posix)'\
	 $(switches)\
	 --load-system $@ --entry main --output $@

ccl-sudoku: sudoku.lisp
	ccl-buildapp\
	 $(switches)\
	 --load-system sudoku --entry main --output $@

ccl-sudoku-vector: sudoku-vector.lisp
	ccl-buildapp\
	 $(switches)\
	 --load-system sudoku-vector --entry main --output $@


.PHONY: clean

clean:
	-rm -f sudoku *.fasl ccl-sudoku ccl-sudoku-vector sudoku-vector
