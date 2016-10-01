;;;; sudoku.asd

(asdf:defsystem #:sudoku
  :description "solve a sudoku puzzle"
  :author "Jeff Lloyd"
  :license "BSD"
  :serial t
  :depends-on (:iterate :ltk)
  :components (
               (:file "sudoku")))
