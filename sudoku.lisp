;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Sudoku -*-
(defpackage :sudoku
   (:use :cl)
   (:export main))

(in-package :sudoku)
  
(defconstant blank 0)


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

(defun index (pos)
  (+ (* (posn-x pos) 9) (posn-y pos)))

(defun inverse-index (i)
  (make-posn (floor i 9) (mod i 9)))


 (defun make-sudoku (s)
   (make-array '(9 9) :initial-contents s))


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
 

; first-blank-location :: Sudoku -> Posn
       
(defun first-blank-location (s)
    (dotimes (i 9)
      (dotimes (j 9)
;	(format t "i= ~d, j= ~d~%" i j)
	(if (empty-element? s (make-posn i j))
	    (return-from first-blank-location (make-posn i j))
	    nil))))

(defun solved? (s)
  (not (first-blank-location s)))

; get-element :: Sudoku -> Posn  -> Int
(defun get-element (s pos)
  "Get a single element from the matrix"
  (aref s (posn-x pos) (posn-y pos)))

; get-row :: Sudoku -> Int -> [Int]
(defun get-row (s n)
  (mapcar (lambda (col)
	 (get-element s (make-posn n col))) col-indices))

; get-row-elements :: Sudoku -> Int -> [Int]
(defun get-row-elements (s row)
  "Retrieve the non-blank elements of the row"
  (remove-if #'zerop (get-row s row)))

; get-column :: Sudoku -> Int -> [Int]
(defun get-column (s n)
  (mapcar (lambda (row)
	 (get-element s (make-posn row n))) row-indices))

; get-column-elements :: Sudoku -> Int -> [Int]
(defun get-column-elements (s col)
  (remove-if #'zerop (get-column s col)))

(defun set-element! (s posn v)
    (setf (aref s (posn-x posn) (posn-y posn)) v))

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
                       posn)
  "Check if element is blank."
  (= blank (get-element s posn)))

; get-elements-of-3*3 :: Sudoku -> Posn -> [Int]
(defun get-elements-of-3*3 (s pos &optional trace)
  (let* ((r (* 3 (floor (/ (posn-x pos) 3)))) (c (* 3 (floor (/ (posn-y pos) 3))))
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
					  (make-posn
                                           (car x) (cadr x)))) coords))))

; get-valid-cell-elements :: Sudoku -> Posn -> {->Bool} -> [Int]
(defun get-valid-cell-elements (s pos &optional mtrace)
  "Retrive the valid values for the specified cell."
  (when mtrace 
    (format t "get-valid-cell-elements: i= ~d j=~d mtrace =~s~%" 
            (posn-x pos) (posn-y pos) mtrace))
  (if (not (empty-element? s pos))
      '()
      (delete-from-set (get-elements-of-3*3 s pos)
                       (delete-from-set 
                        (get-column-elements s (posn-y pos)) 
                        (delete-from-set 
                         (get-row-elements s (posn-x pos))
                         '(1 2 3 4 5 6 7 8 9))))))



;;; solve :: Sudoku -> {False|Sudoku}
(defun solve (s)
  (let* ((loc (first-blank-location s))
	(elements (get-valid-cell-elements s loc)))
    (try s loc elements 0)))

(defun try (s pos vlist level)
  (cond ((solved? s)
	 (print-sudoku s)
	 nil)
	((null vlist) 
	 NIL)		;test if no more values can be used in pos
	(t
;;	   (printf "Trying location ~s ~s with elements ~s\n" (posn-x pos) (posn-y pos) vlist)
	 (set-element! s pos (first vlist)) ;set a value for the current position
	 (cond ((solved? s)
		(print-sudoku s))
		(t			;go on to try the next location
		 (cond ((try s (first-blank-location s)
			     (get-valid-cell-elements s (first-blank-location s))
			     level)
			s)
		       (t		;Failed on the next location 
			(set-element! s pos blank) ; set the current location to blank
		       (try s pos (cdr vlist) (+ 1 level))))))))) ;try the next value for this location


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
  (time (solve (read-sudoku "simple.txt"))))

(defun test1 ()
  (solve (read-sudoku "simple.tx1")))

(defun main (args)
  (if (not (= (length args) 2))
      (format t "Program requires a sudoku file name~%")
      (handler-case
	  (time (solve (read-sudoku (car (rest args)))))
	(simple-error (err) (format t "~a~%" err))
	#+sbcl (sb-int:simple-file-error (err)
					       (format t "~a~%" err))
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
