;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: modules/compat/floor.lisp - reads standard vanilla floors
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.


|#

(in-package :org.langband.engine)

(defun compat-read-floor-file& (fname)
  "Reads floor from 2.9.0"
  
  (with-open-file (in-str (pathname fname)
			  :direction :input)

    (let ((cur-floor nil))

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
		   (when cur-floor
		     (setf (get-floor-type (floor.id cur-floor)) cur-floor)
		     (setq cur-floor nil))
		 
		   (setq cur-floor (make-instance 'floor-type))
		   ;; the first should be N
		   (assert (string-equal (car res) "n"))
		   ;; the second should be the numeric id
		   (setf (floor.id cur-floor) (parse-integer (second res)))
		   ;; the third is the name
		   (setf (floor.name cur-floor) (third res))
		   ;;		   (warn "Doing ~a" cur-floor) 
		   ))
		 
		;; graphics
		((#\G #\g)
		 
		 (when cur-floor
		   ;; the : shows up here, so no ordinary tokenising :(
		   ;; no assert
		   ;; the second should be the symbol to paint
		   (setf (x-char cur-floor) (schar l 2))
		   ;; the third should be the colour
		   (setf (x-attr cur-floor) (convert-obj (schar l 4) :colour-code))
		   ))
		   
		
		;; Mimic
		((#\M #\m)
		 (when cur-floor
		   (let ((res (split-seq-on l #\:)))
		     ;; the first should be m
		     (assert (string-equal (car res) "m"))
		     ;; the second is what is mimicked
		     (setf (floor.mimic cur-floor) (parse-integer (second res))))))
		(t
		 (format t "Unhandled [~s]: ~a~%" first-char l)))
	      ))
      
      (when cur-floor
	(setf (get-floor-type (floor.id cur-floor)) cur-floor)
	(setq cur-floor nil))
      )))


(pushnew :compatibility-floors cl:*features*)
