;;;; sudoku.asd

(asdf:defsystem #:sudoku-vector
  :description "solve a sudoku puzzle"
  :author "Jeff Lloyd"
  :license "BSD"
  :serial t
  :components (
               (:file "sudoku-vector")))
