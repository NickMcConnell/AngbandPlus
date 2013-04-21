;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: lib/compat/floor.lisp - reads standard vanilla floors/features
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.


|#

(in-package :langband)

(defun compat-read-feature-file& (fname)
  "Reads floor from 2.9.0"
  
  (with-open-file (in-str (pathname fname)
			  :direction :input)

    (let ((cur-feature nil))

      (loop for l = (read-line in-str nil 'eof)
	    until (eq l 'eof)
	    do
	    (let ((first-char (if (> (length l) 0)
				  (schar l 0)
				  nil)))
		
	      (case first-char
		((#\# nil #\Space) nil)
		;; Version
		((#\V #\d);;(format t "Version: ~a~%" l)
		 )

		;; Name
		((#\N #\n)
		 (let ((res (split-seq-on l #\:)))
		   (when cur-feature
		     (setf (get-feature (feature.id cur-feature)) cur-feature)
		     (setq cur-feature nil))
		 
		   (setq cur-feature (make-instance 'feature-type))
		   ;; the first should be N
		   (assert (string-equal (car res) "n"))
		   ;; the second should be the numeric id
		   (setf (feature.id cur-feature) (parse-integer (second res)))
		   ;; the third is the name
		   (setf (feature.name cur-feature) (third res))
		   ;;		   (warn "Doing ~a" cur-feature) 
		   ))
		 
		;; graphics
		((#\G #\g)
		 
		 (when cur-feature
		   ;; the : shows up here, so no ordinary tokenising :(
		   ;; no assert
		   ;; the second should be the symbol to paint
		   (setf (feature.x-char cur-feature) (schar l 2))
		   ;; the third should be the colour
		   (setf (feature.x-attr cur-feature) (get-colour-code-from-letter (schar l 4)))
		   ))
		   
		
		;; Mimic
		((#\M #\m)
		 (when cur-feature
		   (let ((res (split-seq-on l #\:)))
		     ;; the first should be m
		     (assert (string-equal (car res) "m"))
		     ;; the second is what is mimicked
		     (setf (feature.mimic cur-feature) (parse-integer (second res))))))
		(t
		 (format t "Unhandled [~s]: ~a~%" first-char l)))
	      ))
      
      (when cur-feature
	(setf (get-feature (feature.id cur-feature)) cur-feature)
	(setq cur-feature nil))
      )))



