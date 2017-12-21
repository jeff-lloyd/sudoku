;;;; sudoku.asd

(asdf:defsystem #:sudoku-vector
  :description "solve a sudoku puzzle"
  :author "Jeff Lloyd"
  :license "BSD"

  :components (
	       (:file "auxfns")
               (:file "sudoku-vector" :depends-on ("auxfns"))))
