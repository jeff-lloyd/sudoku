switches :=--asdf-tree ~/quicklisp/dists/quicklisp/software
all: sudoku-vector sudoku

sudoku-vector: sudoku-vector.lisp
	buildapp \
        $(switches) \
        --load-system $@ --entry main --output $@

sudoku: sudoku.lisp auxfns.lisp
	buildapp \
        $(switches) \
        --load-system $@ --entry main --output $@

.PHONY: clean

clean:
	-rm -f sudoku *.fasl  sudoku-vector
