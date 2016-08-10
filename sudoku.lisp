;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Sudoku -*-
;(defpackage :sudoku
;   (:use :cl)
;   (:export main))

;(in-package :sudoku)


(defconstant blank 0)

(defun flatten (the-list)
  "Append together elements (or lists) in the list."
  (mappend #'mklist the-list))

(defun mklist (x)
  "Return x if it is a list, otherwise (x)."
  (if (listp x)
      x
      (list x)))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the results."
  (apply #'append (mapcar fn the-list)))

(defun group-by-2 (ls)
  (flatten (mapcar #'(lambda (x)
		       (mapcar #'(lambda(y)
				   (list x y)) ls)) ls)))

(defun group-by-3 (ls)
  (flatten (mapcar #'(lambda(y)
		       (mapcar #'(lambda (x)
				   (cons y x)) (group-by-2 ls))) ls)))

(defun filter (predicate sequence)
  "Return a sequence with only items that satisfy predicate."
  (cond ((null sequence) nil)
	((apply predicate (list (car sequence)))
	 (cons (car sequence)
	       (filter predicate (cdr sequence))))
	(t
	 (filter predicate (cdr sequence)))))

;(proclaim '(inline make-posn posn-x posn-y index))
(defun make-posn (x y)
  (cons x y))

(defun posn-x (pos)
  (car pos))

(defun posn-y (pos)
  (cdr pos))


(defun iota (low high)
  (if (>= low high)
       '()
       (cons low (iota (+ low 1) high))))

(defvar row-indices (iota 0 9))
(defvar col-indices (iota 0 9))

(defun index (x y)
  (declare (integer x y))
  (the integer (+ (* x 9) y)))

(defun inverse-index (i)
  (make-posn (floor i 9) (mod i 9)))


 (defun make-sudoku (s)
   "Make a sudoku structure using a list of lists as initial values."
   (make-array 81 :element-type 'integer :initial-contents (flatten s)))

(defun copy-sudoku(s)
  "Make a copy of the sudoku s"
  (copy-seq s))

 (defvar simple-sudoku
   (make-sudoku 
    '((0 3 6 0 4 0 7 0 8)
      (7 0 0 0 0 1 0 5 0)
      (0 4 0 8 3 0 9 6 0)
      (3 0 0 1 0 2 0 8 4)
      (0 0 0 0 0 0 0 0 0)
      (8 7 0 6 0 4 0 0 9)
      (0 6 2 0 1 5 0 4 0)
      (0 1 0 7 0 0 0 0 6)
      (5 0 7 0 9 0 3 2 0))))

 (defvar *empty-sudoku*
   (make-sudoku 
    '((0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0)
      (0 0 0 0 0 0 0 0 0))))

; first-blank-location :: Sudoku -> Posn
       
(defun first-blank-location (s)
  "Find the first blank location in sudoku but use fact s is vector"
    (dotimes (i 81)
      (if (= blank (svref s i))
	    (return-from first-blank-location (inverse-index i))
	    nil)))

(defun solved? (s)
  (not (first-blank-location s)))

; get-element :: Sudoku -> Posn  -> Int
(defun get-element (s x y)
  "Get a single element from the matrix"
  (declare (integer x y))
  (svref s (index x y)))

; get-row :: Sudoku -> Int -> [Int]
(defun get-row (s n)
  (mapcar (lambda (col)
	 (get-element s  n col)) col-indices))

; get-row-elements :: Sudoku -> Int -> [Int]
(defun get-row-elements (s row)
  "Retrieve the non-blank elements of the row"
  (remove-if #'zerop (get-row s row)))

; get-column :: Sudoku -> Int -> [Int]
(defun get-column (s n)
  (mapcar (lambda (row)
	 (get-element s row n)) row-indices))

; get-column-elements :: Sudoku -> Int -> [Int]
(defun get-column-elements (s col)
  (remove-if #'zerop (get-column s col)))

(defun set-element! (s x y v)
    (setf (aref s (index x y)) v))

; delete-from-set :: [Int] -> [Int] -> [Int]
(defun delete-from-set (set1 set2)
  "Delete items in set1 from set2"
  (if (not set1)
      set2
      (delete-from-set (cdr set1) (remove-if (lambda (x)
					       (= x (car set1)))
					 set2))))

; empty-element? :: Sudoku -> Int -> Int -> Boolean
(defun empty-element? (s
                       x y)
  "Check if element is blank."
  (= blank (get-element s x y)))

; get-elements-of-3*3 :: Sudoku -> Posn -> [Int]
(defun get-elements-of-3*3 (s x y &optional trace)
  (let* ((r (* 3 (floor (/ x 3)))) (c (* 3 (floor (/ y 3))))
	 (coords (mapcar #'(lambda (x)
			(list (+ r (car x))
			      (+ c (cadr x))))
		      '((0 0) (0 1) (0 2)
			(1 0) (1 1) (1 2)
			(2 0) (2 1) (2 2)))))
    (when  trace
      (format t "get-elements-of-3*3: coords: ~s~%" coords))
    (remove-if #'zerop (mapcar (lambda (x)
			     (get-element s
					  
                                           (car x) (cadr x))) coords))))

; get-valid-cell-elements :: Sudoku -> Posn -> {->Bool} -> [Int]
(defun get-valid-cell-elements (s x y &optional mtrace)
  "Retrive the valid values for the specified cell."
  (when mtrace 
    (format t "get-valid-cell-elements: i= ~d j=~d mtrace =~s~%" 
            x y mtrace))
  (if (not (empty-element? s x y))
      '()
      (delete-from-set (get-elements-of-3*3 s x y)
                       (delete-from-set 
                        (get-column-elements s y) 
                        (delete-from-set 
                         (get-row-elements s x)
                         '(1 2 3 4 5 6 7 8 9))))))



;;; solve :: Sudoku -> {False|Sudoku}
(defun solve (s)
  (let* ((loc (first-blank-location s))
	(elements (get-valid-cell-elements s (posn-x loc) (posn-y loc))))
    (try s loc elements 0)))

(defun try (s pos vlist level)
  (cond ((solved? s)
	 s)
	((null vlist) ;test if no more values can be used in pos
	 nil)	      ;this search has failed
	(t	      ;More values can be tried at this position
;;	   (printf "Trying location ~s ~s with elements ~s\n" (posn-x pos) (posn-y pos) vlist)
	 (let ((scopy (copy-sudoku s)))
	   (set-element! scopy (posn-x pos) (posn-y pos) (first vlist)) ;set a value for the current position
	   (cond ((solved? scopy)
		  scopy)
		 (t
		  (let* ((new-loc (first-blank-location scopy))
			 (elements (get-valid-cell-elements scopy (posn-x new-loc) (posn-y new-loc)))
			 (result (try scopy new-loc elements (1+ level))))
		    (if result
			result
			(try s pos (rest vlist) level)))))))))
		  
		 

;;; print-sudoku :: Sudoku -> Bool
(defun print-sudoku (s)
  (map nil (lambda(row)	     
	      (format t  "~{~2d~}~%" (get-row s row)))
	    '(0 1 2 3 4 5 6 7 8)))

;;; read-sudoku :: String -> Sudoku
(defun read-sudoku (file)
  (make-sudoku (with-open-file (p file :if-does-not-exist :error)
    (read p))))

;;;zip :: List -> List -> List
(defun zip (l1 l2)
  (cond ((or (null l1) (null l2)) (list))
        (t
         (cons (list (car l1) (car l2))
                    (zip (rest l1) (rest l2))))))

(defun test0 ()
  (time (print-sudoku (solve (read-sudoku "simple.txt")))))

(defun test1 ()
  (print-sudoku (solve (read-sudoku "simple.tx1"))))

(defun main (args)
  (if (not (= (length args) 2))
      (format t "Program requires a sudoku file name~%")
      (handler-case
	  (time (print-sudoku (solve (read-sudoku (car (rest args))))))
	(simple-error (err) (format t "~a~%" err))
	#+sbcl (sb-int:simple-file-error (err)
				

(in-package :sudoku)


(defconstant blank 0)

(defun flatten (the-list)
  "Append together elements (or lists) in the list."
  (mappend #'mklist the-list))

(defun mklist (x)
  "Return x if it is a list, otherwise (x)."
  (if (listp x)
      x
      (list x)))	       (format t "~a~%" err))
	#+ccl (ccl::simple-file-error (err)
					   (format t "~a~%" err))
	)))
  
(defun start ()
  #+sbcl  (main SB-EXT:*POSIX-ARGV*)
  #+ccl   (main CCL:*COMMAND-LINE-ARGUMENT-LIST*)
  )

(defun produce-executable ()
  #+sbcl (sb-ext:save-lisp-and-die "sbcl-sudoku" :toplevel #'start :executable t)
  #+ccl (ccl:save-application  "ccl-sudoku" :toplevel-function #'start :prepend-kernel t)
  )

;;; Killer sudoku routines
;;;

(defun kentry-sum (e)
  (car e))

(defun kentry-coords (e)
  (second e))

(defun killer-list->posn (l)
  "Convert a list of pairs to positions."
  (mapcar #'pair->posn l))

(defun pair->posn (l)
  (make-posn (first l) (second l)))

; read-killer-sudoku :: String -> [(Int [Cells])]
(defun read-killer-sudoku (file)
  (mapcar (lambda (x)
	    (list (first x) (killer-list->posn (second x))))
	  (with-open-file (p file :if-does-not-exist :error)
	    (read p))))

(defun sudoku-bases (s)
 (mapcar #'(lambda(x)
	     (list (kentry-sum x) (length (kentry-coords x))
		   (kentry-coords x))) s))

(defun sum-equal-p (value numeric-list)
  (= value (apply #'+ numeric-list)))

(defun all-possibilities (tuple-size value)
  (filter #'(lambda(x)
	      (sum-equal-p value x))
	  (case tuple-size
	    (2 (group-by-2 '(1 2 3 4 5 6 7 8 9)))
	    (3 (group-by-3 '(1 2 3 4 5 6 7 8 9)))
	    (4 (error  "Need group by 4"))
	    (5 (error  "Need group by 5")))))

(defun try-block (grid block-list solutions coords)
  "Try to place a solution at the specified coordinate list"
  ;;; each solution  needs to be placed at the specified coordinates
  ;;; in the sudoku grid
  (declare (notinline try-block))
  (cond ((null solutions) nil)
	(t
	 (let* ((grid-copy (copy-sudoku grid))
	       (result (place-solution grid-copy (first solutions) coords)))
	   (cond ((null result)
		  (clear-values-at-coords grid-copy coords)
		  (try-block grid-copy block-list (rest solutions) coords))
		 (t
		  (let ((result2 (try-killer result (rest block-list))))
		    (if result2
			result2
			(progn
			  (try-block grid block-list (rest solutions) coords))))))))))

(defun clear-values-at-coords (sudoku coords)
  (dolist (i coords)
    (set-element! sudoku (posn-x i) (posn-y i) 0)))

(defun place-solution (grid solution coords)
  "Place the individual values from solution at the specified coordinates"
  (cond ((null solution) grid)
	(t
	 (let* ((value (first solution))
	       (posn (first coords))
	       (valid-cell-values (get-valid-cell-elements grid (posn-x posn)
							   (posn-y posn))))
	   (cond ((member value valid-cell-values)
		  (set-element! grid (posn-x posn) (posn-y posn) value)
		  (place-solution grid (rest solution) (rest coords)))
		 (t
		  nil))))))

(defun ktest0 ()
  (let ((killer (read-killer-sudoku "killer.cons")))
    (sudoku-bases killer)))    

(defun ktest1 ()
  (let* ((killer (read-killer-sudoku "killer.cons"))
	 (block (first killer))
	 (solutions (all-possibilities (length (kentry-coords block))
				       (kentry-sum block)))
	 (s (try-block *empty-sudoku* killer solutions (kentry-coords block))))
    (when s
      (print-sudoku s))))

(defun ktest2 ()
  (let ((killer (read-killer-sudoku "killer.cons")))
    (let ((s (try-killer *empty-sudoku* killer)))
      (when s
	(print-sudoku s)))))
	  
(defun try-killer (sudoku block-list)
  "Try setting up a killer sudoku"
  (cond ((null block-list)
	 sudoku)
	(t
	 (let* ((copy-sud (copy-sudoku sudoku))
		(block (first block-list))
		(solutions (all-possibilities (length (kentry-coords block))
					      (kentry-sum block)))
		(result (try-block copy-sud block-list solutions (kentry-coords block))))
	   (cond ((null result)
		  nil)
		 (t
		  (try-killer result (rest block-list))))))))

(defun fact (n)
  (if (= n 1)
      1
      (* n (fact (1- n)))))
