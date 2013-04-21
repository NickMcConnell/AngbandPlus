;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: USER -*-

#|

DESC: filedesc.lisp - generates a file with file-descriptions
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

ADD_DESC: Simple code to access the dungeon object(s) 

|#

(in-package :user)

(defun describe-files (file-list out-format out-file)
  "Takes a list of filenames and an out-format, currently only
accepts :html as a format, and the file to write to."

  (let ((real-list (stable-sort file-list #'string-lessp))
	(cur-desc nil))

    (with-open-file (out-str (pathname out-file)
			     :direction :output
			     :if-exists :new-version
			     :if-does-not-exist :create)
      
    
      (dolist (i real-list)
	(setq cur-desc nil)
	(with-open-file (in-file (pathname i)
				 :direction :input)
	  (loop named reading
		for x = (read-line in-file nil :eof)
		until (eq x :eof) 
		do
		(let ((len (length x)))
		  (when (> len 6)
		    ;;(warn "Checking ~a" (subseq x 0 5))
		    (when (string-equal "DESC:" (subseq x 0 5))
		      ;; we have a description
		      (setq cur-desc (subseq x 6)))))
		))

	(if cur-desc
	    (format out-str "~&~a~%" cur-desc)
	    (format out-str "~&~a~%" (file-namestring i)))
	)


      (terpri out-str)
      (finish-output out-str)

      )))

(defun xy ()
  (let ((files (mapcar #'namestring (directory "./*.lisp"))))
;;    (format t "~a~%" files)
    (describe-files files :html "outfile.txt")))

	