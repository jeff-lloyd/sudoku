;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Sudoku -*-

;;; auxiliary functions for use by any application
;;;
(defun iota (low high)
  "generate a list of integers from low to high exclusive"
  (if (>= low high)
       '()
       (cons low (iota (+ low 1) high))))

(defun mklist (x)
  "Return x if it is a list, otherwise (x)."
  (if (listp x)
      x
      (list x)))

;;; Utility functions for mapping
(defun map0-n (fn n)
  (mapa-b fn 0 n))

(defun map1-n (fn n)
  (mapa-b fn 1 n))

(defun mapa-b (fn a b &optional (step 1))
  (do ((i a (+ i step))
       (result nil))
      ((> i b) (nreverse result))
    (push (funcall fn i) result)))

(defun map-> (fn start test-fn succ-fn)
  (do ((i start (funcall succ-fn i))
       (result nil))
      ((funcall test-fn i) (nreverse result))
    (push (funcall fn i) result)))

(defun flatten (x)
  "flatten a multi level list"
  (labels ((rec (x acc)
	     (cond ((null x) acc)
		   ((atom x) (cons x acc))
		   (t (rec (car x) (rec (cdr x) acc))))))
    (rec x nil)))
