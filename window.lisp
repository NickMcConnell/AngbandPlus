;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: window.lisp - code for the window system
Copyright (c) 2002-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

(defun establish-data-in-window (win)
  "Makes the datastructures internally."
  (when win
    (let ((width (window.width win))
	  (height (window.height win)))
      
      (setf (window.data win) (make-array (list +num-gfx-layers+ width height)
					  :element-type 'u32b :initial-element 0))
      (setf (window.flagmap win) (make-array (list width height)
					     :element-type 'u32b :initial-element 0)) 
      win)))

(defun create-window (id num-id width height)
  (assert (plusp width))
  (assert (plusp height))
  (let ((win (make-instance 'window :id id :num-id num-id :width width :height height)))

    (establish-data-in-window win)
 
    win))


(defun (setf window-coord) (value win layer x y)
  (setf (aref (window.data win) layer x y) value)
  (setf (aref (window.flagmap win) x y) +coord-updated+))

(defun window-coord (win layer x y)
  (aref (window.data win) layer x y))

(defun paint-coord (win x y &optional (flag 0))
  "This function paints a coord to the window.. it has some hackish parts to be able
to do both ascii info, ascii maps and graphics."
  ;; assume sdl-only now
  (let ((win-num (window.num-id win))
	(bg (window-coord win +background+ x y)))

    ;; graphic tiles are simple.. 
    (cond ((window.gfx-tiles? win)
	   (c-full-blit win-num x y bg flag)
	   (dotimes (i #.(1- +num-gfx-layers+))
	     (setf bg (window-coord win (1+ i) x y))
	     (when (> bg 0)
	       (c-transparent-blit win-num x y bg flag))))
	  ;; characters are nasty, only the top one should be done
	  (t
	   ;; first do background
	   (c-full-blit win-num x y bg flag)
	   ;; then do only the top-most
	   (loop named paint
		 for i from #.(1- +num-gfx-layers+) downto 1
		 for bg = (window-coord win i x y)
		 do
		 (when (> bg 0)
		   ;; ultra-ugly hack!! nasty nasty
		   (if (= win-num *map-frame*)
		       (c-full-blit win-num x y bg flag)
		       (c-transparent-blit win-num x y bg flag))
		       
		   (return-from paint nil)))
	   ))
  
    (setf (aref (window.flagmap win) x y) 0)))

(defun paint-window (win)
  (when (integerp win)
    (setf win (aref *windows* win)))

  (let ((wid (window.width win))
	(hgt (window.height win))
	(flag +winflag-delay-paint+))
 
    (loop for x from 0 below wid
	  do
	  (loop for y from 0 below hgt
		do
		(paint-coord win x y flag)))
    
    (c-flush-coords! (window.num-id win) 0 0 wid hgt)
    ))


(defun refresh-window (&optional (the-win nil) (flag 0))
  "Repaints any updated parts of the window."
  (let* ((win (etypecase the-win
		(window the-win)
		(number (if (< the-win 0)
			    *cur-win*
			    (aref *windows* the-win)))))
	 (wid (window.width win))
	 (hgt (window.height win))
	 (anything-painted nil))
    
    (loop for x from 0 below wid
	  do
	  (loop for y from 0 below hgt
		do
		(when (plusp (aref (window.flagmap win) x y))
		  (setf anything-painted t)
		  (paint-coord win x y flag) ;; implicitly resets flagmap
		  )))
    (when (and anything-painted (bit-flag-set? flag +winflag-delay-paint+))
      (c-flush-coords! (window.num-id win) 0 0 (1- wid) (1- hgt)))
    ))

(defun flush-coords (win x y w h)
  (c-flush-coords! (window.num-id win) x y w h))

(defun clear-coord (win x y)
  "Clears the coord of any values.  Does not force repaint"
  (let ((coord-table (window.data win))
	(flag-table (window.flagmap win)))
    (setf (aref flag-table x y) 1)
    (loop for j from 0 below +num-gfx-layers+
	  do
	  (setf (aref coord-table j x y) 0))))

(defun clear-row (win x y &optional (len 256))
  "x,y are start coordinates for clearn, when LEN is given it refers
to length cleared from x,y  LEN will be chopped if it exceeds boundaries."

  (when (integerp win)
    (setf win (aref *windows* win)))
  
  (when (minusp x)
    (setf x 0))
    
  (let ((num-to-del (min len (- (window.width win) x 1)))
	(coord-table (window.data win))
	(flag-table (window.flagmap win)))

    ;;(warn "Del ~s" num-to-del)
    (when (plusp num-to-del)
      (loop for i from 0 below num-to-del
	    do
	    (loop for j from 0 below +num-gfx-layers+
		  do
		  (setf (aref coord-table j (+ x i) y) 0)
		  (setf (aref flag-table (+ x i) y) 0)))
      
      (org.langband.ffi:c-clear-coords! (window.num-id win) x y num-to-del 1)
      nil)))

(defun clear-window-from (win row)
  "Clears window from given row and downwards.  can be made more efficient."
  (when (integerp win)
    (setf win (aref *windows* win)))

  (loop for i from row below (window.height win)
	do
	(clear-row win 0 i))
  nil)

(defun clear-window (the-win)
  
  (let* ((win (cond ((typep the-win 'window)
		     the-win)
		    ((< the-win 0)
		     *cur-win*)
		    (t
		     (aref *windows* the-win))))
	 (coord-table (window.data win))
	 (flag-table (window.flagmap win))
	 (wid (window.width win))
	 (hgt (window.height win)))
    
    (loop for x from 0 below wid
	  do
	  (loop for y from 0 below hgt
		do
		(loop for j from 0 below +num-gfx-layers+
		      do
		      (setf (aref coord-table j x y) 0
			    (aref flag-table  x y) 0))))
    
    (org.langband.ffi:c-clear-coords! (window.num-id win) 0 0 (window.width win) (window.height win))
    nil))

(defun activate-window (win)
  (let ((the-win (if (integerp win)
		     (aref *windows* win)
		     win)))
    (when the-win
      (setf (window.visible? the-win) t))
    t))

(defun deactivate-window (win)
  (let ((the-win (if (integerp win)
		     (aref *windows* win)
		     win)))
    (when the-win
      (setf (window.visible? the-win) nil))
    nil))

(defvar *win/colour* +term-white+)

(defun win/write-char (win x y val)
  (let* ((colour *win/colour*)
	 (pval (if (< colour 256)
		   (text-paint-value colour val)
		   (logior colour (if (characterp val) (char-code val) val)))))
    (setf (window-coord win +foreground+ x y) pval)
    (paint-coord win x y +winflag-delay-paint+))
  1)

(defun win/write-str (win x y str)
  (loop for chr across str
	for i from x
	do
	(win/write-char win i y chr))
  (length str))

;; should be enough
(defvar *write-int-dummy* "                                        ")

(defun win/write-int (win x y int padding)
  "Borrowed and modified from CMUCL."
  (let ((count 0)
	(quotient int)
	(remainder nil)
	(minus-sign nil))

    ;;(warn "num is ~s, pad is ~s" quotient padding)
    (when (minusp quotient)
      (setf quotient (- quotient)
	    minus-sign t)
      (when (integerp padding)
	(decf padding)))

    ;;(warn ">num is ~s, pad is ~s" quotient padding)
    
    ;; print number in reverse
    (loop named do-numbers
	  for i from 0
	  do
	  (progn
	    (multiple-value-setq (quotient remainder)
	      (truncate quotient *print-base*))

	    (let ((val (code-char (+ (char-code #\0) remainder))))
	      ;;(warn "assigning val ~s to ~s" val i)
	      (setf (schar *write-int-dummy* i) val)
	      (incf count))

	    (when (zerop quotient)
	      (return-from do-numbers t))))

    ;;(warn "padding is ~s and count is ~s" padding count)

    (when (integerp padding)
      (when (> padding count)
	(dotimes (i (- padding count))
	  (win/write-char win (+ i x) y #\Space))
	(incf x (- padding count))))

    (when minus-sign
      (win/write-char win x y #\-)
      (incf x))
    
    (loop for j from 0
	  for i from (1- count) downto 0
	  for col = (+ x j)
	  do
	  (let ((val (schar *write-int-dummy* i))) 
	    ;;(warn "writing ~s from ~s to ~s" val i col)
	    (win/write-char win col y val)))

    count))


(defun win/format (win x y colour format-str &rest args)
  "A non-consing format that writes directly to a window.  It understands the format
directives ~a ~d ~~ ~% and ~v  (~v is similar to ~vd in CL:FORMAT and takes two
arguments)."
  
  (let ((*win/colour* colour)
	(col x)
	(row y))
    
    (flet ((output-int (arg padding)
	     (let ((*print-base* 10))
	       (cond ((< arg 0)
		      (when (integerp padding)
			(decf padding))
		      (incf col (win/write-int win col row arg padding)))
		     (t					  
		      (incf col (win/write-int win col row arg padding))))
	       )))

	 
      (let ((last-char #\a)
	    (arg-iter args))
	(loop for chr across format-str
	      do
	    (cond ((eql last-char #\~)
		   (ecase chr
		     (#\a (let ((arg (car arg-iter)))
			    (etypecase arg
			      (symbol
			       (when (keywordp arg)
				 (incf col (win/write-char win col row #\:)))
			       (incf col (win/write-str win col y (symbol-name arg))))
			      
			      (integer
			       (output-int arg nil))
				    
			      (string
			       (incf col (win/write-str win col y arg)))
			      
			      )
			    (setf arg-iter (cdr arg-iter))))
			  
		     (#\d (let ((arg (car arg-iter)))
			    (etypecase arg
			      (integer
			       (output-int arg nil)))
			    (setf arg-iter (cdr arg-iter))))

		     (#\v (let ((arg (car arg-iter)))
			    (etypecase arg
			      (integer
			       (output-int (cadr arg-iter) arg)))
			    (setf arg-iter (cddr arg-iter))))
		     
		     ;;(#\% (win/write-char win col row #\Newline))
		     (#\~ (incf col (win/write-char win col row #\~))))
		   (setf last-char #\a)	;; dummy
		   )
		  (t
		   (if (eql chr #\~)
		       nil
		       (incf col (win/write-char win col row chr)))
		   (setf last-char chr)))
	    )))

    (flush-coords win x y 100 1)
    t))
