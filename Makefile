switches :=--asdf-tree ~/quicklisp
all: sudoku-vector sudoku

sudoku-vector: sudoku-vector.lisp
	buildapp --eval '(require :sb-posix)'\
        $(switches)\
        --load-system $@ --entry main --output $@

sudoku: sudoku.lisp auxfns.lisp
	buildapp --eval '(require :sb-posix)'\
        $(switches)\
        --load-system $@ --entry main --output $@

.PHONY: clean

clean:
	-rm -f sudoku *.fasl  sudoku-vector
