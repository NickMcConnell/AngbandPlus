;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: modules/compat/spells.lisp - reads standard vanilla spell-info
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.


|#
;; reads lines like B:42:35:35:85:34 from p_class.txt

(in-package :org-langband.engine)


(defun compat-read-class-spells (offset file)
  
  (let ((spell-table (variant.spells *variant*))
	 (spell-arr (make-array 200 :initial-element nil))
	 (results '()))
    
    (loop for v being the hash-values of spell-table
	  do
	  (setf (aref spell-arr (spell.numeric-id v)) v))

    (with-open-file (s file
		       :direction :input)
      (loop for l = (read-line s nil 'eof)
	    until (eq l 'eof)
	    do
	    (let ((first-char (if (> (length l) 0)
				  (schar l 0)
				  nil)))
	      
	      (case first-char
		((#\B #\b)
		 (let* ((res (split-seq-on l #\:))
			(idx (+ offset (parse-integer (second res))))
			(level (parse-integer (third res))))
		   
		   (when (and (< level 60)
			      (aref spell-arr idx))
		     (push (list :id (spell.id (aref spell-arr idx))
				 :level level
				 :mana (parse-integer (fourth res))
				 :fail (parse-integer (fifth res))
				 :xp (parse-integer (sixth res)))
			   results))
		   )))
	      )))
    
    (let ((*print-case* :downcase))
      (with-open-file (ffile (pathname "spells.txt")
			     :direction :output
			     :if-exists :supersede
			     :if-does-not-exist :create)
	(loop for x in (reverse results)
	      do
	      (print x ffile))))
    
    t))
