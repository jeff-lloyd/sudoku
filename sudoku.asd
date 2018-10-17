;;;; sudoku.asd

(defsystem #:sudoku
  :description "solve a sudoku puzzle"
  :author "Jeff Lloyd"
  :license "BSD"

  :depends-on (:alexandria :lispbuilder-sdl)
  :components (
	       (:file "package")
               (:file "sudoku" :depends-on ("package"))
	       ))
