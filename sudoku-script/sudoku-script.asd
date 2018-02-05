;;;; sudoku.asd

(asdf:defsystem #:sudoku-script
  :description "solve a sudoku puzzle"
  :author "Jeff Lloyd"
  :license "BSD"

  :depends-on (:alexandria)
  :components (
               (:file "sudoku" :depends-on ("auxfns"))
	       (:file "auxfns")))
