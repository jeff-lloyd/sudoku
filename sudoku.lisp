;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: Sudoku -*-

(defconstant blank 0)

(proclaim '(inline make-posn posn-x posn-y index))


(defun mklist (x)
  "Return x if it is a list, otherwise (x)."
  (if (listp x)
      x
      (list x)))


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
  (declare ((unsigned-byte 8) x y) (optimize (safety 0) (speed 3)))
  (the fixnum (+ (* x 9) y)))

(defun inverse-index (i)
;  (declare ((unsigned-byte 32) i) (optimize (safety 0) (speed 3)))
  (make-posn (floor i 9) (mod i 9)))

(deftype tile () `((unsigned-byte 8) 0 9))
(deftype board () `(simple-array tile (81)))

 (defun make-sudoku (s)
   "Make a sudoku structure using a list of lists as initial values."
   (make-array '(9 9) :element-type 'unsigned-byte :initial-contents s))

(defun copy-sudoku(s)
  "Make a copy of the sudoku s"
  (let ((ns (make-array '(9 9) :element-type 'unsigned-byte)))
    (dotimes (r 9)
      (dotimes (c 9)
	(setf (aref ns r c) (aref s r c))))
    ns))

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
  "Find the first blank location in sudoku but use fact s is vector"
  (dotimes (r 9)
    (dotimes (c 9)
      (if (= blank (aref s r c))
	  (return-from first-blank-location (make-posn r c))
	    nil))))

(defun solved? (s)
  (not (first-blank-location s)))

; get-element :: Sudoku -> Posn  -> Int
(defun get-element (s x y)
  "Get a single element from the matrix"
;  (declare (integer x y))
  (aref s x y))

; get-row :: Sudoku -> Int -> [Int]
(defun get-row (s n)
  (mapcar (lambda (col)
	 (aref s  n col)) col-indices))

; get-row-elements :: Sudoku -> Int -> [Int]
(defun get-row-elements (s row)
  "Retrieve the non-blank elements of the row"
  (remove-if #'zerop (get-row s row)))

; get-column :: Sudoku -> Int -> [Int]
(defun get-column (s n)
  (mapcar (lambda (row)
	 (aref s row n)) row-indices))

; get-column-elements :: Sudoku -> Int -> [Int]
(defun get-column-elements (s col)
  (remove-if #'zerop (get-column s col)))

(defun set-element! (s x y v)
    (setf (aref s x y) v))

; delete-from-set :: [Int] -> [Int] -> [Int]
;; (defun delete-from-set (set1 set2)
;;   "Delete items in set1 from set2"
;;   (if (not set1)
;;       set2
;;       (delete-from-set (cdr set1) (remove-if (lambda (x)
;; 					       (= x (car set1)))
;; 					 set2))))

(defun delete-from-set (set1 set2)
  (set-difference set2 set1))

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
  (mapc (lambda(row)	     
	      (format t  "~{~2d~}~%" (get-row s row)))
	    '(0 1 2 3 4 5 6 7 8)))

;;; read-sudoku :: String -> Sudoku
(defun read-sudoku (file)
  (make-sudoku (with-open-file (p file :if-does-not-exist :error)
    (read p))))

(defun main (args)
  (cond ((not (= (length args) 2))
	 (format t "Program requires a sudoku file name~%")
	 nil)
	(t
	 (let ((file (second args)))
	   (if (probe-file file)
	       (progn
		 (handler-case
		     (time (print-sudoku (solve (read-sudoku file))))
		   (type-error (v)
		     (format t "Error data format in file: ~a~%" v))
		   (end-of-file () (format t "End of file received probably closing bracket missing~%"))
		   (simple-error (err) (format t "~a~%" err))
		   #+sbcl (sb-int:simple-file-error (err) (format t "File error ~a~%" err))))
	     (format t "No such file ~a~%" file))))))

(defun main-ccl ()
  (main *command-line-argument-list*))

(defun make-image ()
  (save-application "ccl-sudoku" :toplevel-function #'main-ccl :prepend-kernel t))

(defun test1 ()
  (time (main '("sudoku" "super-fiendish8404.txt"))))

(defun test2 ()
  (time (main '("sudoku" "fiend5402.txt"))))


(defun test0 ()
  (time (print-sudoku (solve (read-sudoku "simple.txt")))))

(defun test11 ()
  (time (print-sudoku (solve (read-sudoku "super-fiendish8404.txt")))))

