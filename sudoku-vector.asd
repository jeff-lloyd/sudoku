;;;; sudoku.asd

(asdf:defsystem #:sudoku-vector
  :description "solve a sudoku puzzle"
  :author "Jeff Lloyd"
  :license "BSD"

  :depends-on (:alexandria)
  :components (
	       (:file "package")
               (:file "sudoku-vector" :depends-on ("package"))))
