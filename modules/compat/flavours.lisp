;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: modules/compat/flavours.lisp - reads standard vanilla floors
Copyright (c) 2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.


|#

(in-package :org.langband.engine)

(defstruct flav
  name
  num
  tval
  char
  colour)

(defstruct gfx-flav
  num
  attr
  char
  flav)

(defvar *flavs* (make-hash-table :test #'equal))
(defvar *gfx-flavs* (make-hash-table :test #'equal))


(defun compat-read-flavour-file& (fname)
  "Reads flavour from 3.x"
  
  (with-open-file (in-str (pathname fname)
			  :direction :input)

    (let ((cur-flavour nil))

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
		   (when cur-flavour
		     ;;(warn "register flavour ~s" cur-flavour) 
		     (setf (gethash (flav-num cur-flavour) *flavs*) cur-flavour)
		     (setq cur-flavour nil))
		 
		   (setq cur-flavour (make-flav))
		   ;; the first should be N
		   (assert (string-equal (car res) "n"))
		   ;; the second should be the numeric id
		   (setf (flav-num cur-flavour) (parse-integer (second res)))
		   ;; the third we ignore
		   (setf (flav-tval cur-flavour) (parse-integer (third res)))
		   ))
		 
		((#\F #\f)
		 (when cur-flavour
		   (let ((res (split-seq-on l #\:)))
		     ;; the first should be f
		     (assert (string-equal (car res) "f"))
		     ;; the second is char
		     (setf (flav-char cur-flavour) (char (second res) 0))
		     ;; third is colour
		     (setf (flav-colour cur-flavour) (convert-obj (char (third res) 0) :colour-code))
		     ;; fourth is name
		     (setf (flav-name cur-flavour) (string-downcase (fourth res)))
		     )))

		(t
		 (format t "Unhandled [~s]: ~a~%" first-char l)))
	      ))

      (when cur-flavour
	(setf (gethash (flav-num cur-flavour) *flavs*) cur-flavour)
	(setq cur-flavour nil))
	)))



(defun compat-read-gfx-flavour-file& (fname)
  "Reads flavour from 3.x"
  
  (with-open-file (in-str (pathname fname)
			  :direction :input)

    (let ((cur-flavour nil))

      (loop for l = (read-line in-str nil 'eof)
	    until (eq l 'eof)
	    do
	    (let ((first-char (if (> (length l) 0)
				  (schar l 0)
				  nil)))
		
	      (case first-char
		((#\# nil #\Space) nil)

		;; Name
		((#\L #\l)
		 (let ((res (split-seq-on l #\:)))
		   (when cur-flavour
		     ;;(warn "register flavour ~s" cur-flavour) 
		     (setf (gethash (gfx-flav-num cur-flavour) *gfx-flavs*) cur-flavour)
		     (setq cur-flavour nil))
		 
		   (setq cur-flavour (make-gfx-flav))
		   ;; the first should be N
		   (assert (string-equal (car res) "l"))
		   ;; the second should be the numeric id
		   (setf (gfx-flav-num cur-flavour) (parse-integer (second res)))
		   ;; the next is attr
		   (let ((num (third res)))
		     (when (eql (schar num 0) #\0)
		       (setf (schar num 0) #\#))
		     (setf (gfx-flav-attr cur-flavour) (read-from-string num)))
		   (let ((num (fourth res)))
		     (when (eql (schar num 0) #\0)
		       (setf (schar num 0) #\#))
		     ;; the next is char
		     (setf (gfx-flav-char cur-flavour) (read-from-string num)))
		   ))
		 
		(t
		 (format t "Unhandled [~s]: ~a~%" first-char l)))
	      ))

      (when cur-flavour
	(setf (gethash (gfx-flav-num cur-flavour) *gfx-flavs*) cur-flavour)
	(setq cur-flavour nil))
	))

  (loop for x being the hash-values of *gfx-flavs*
	do
	(let* ((num (gfx-flav-num x))
	       (y (gethash num *flavs*)))
	  (if y
	      (setf (gfx-flav-flav x) y)
	      (warn "Unable to find flavour ~s" num))))
  )


(defun spit-out-basics ()
  (flet ((get-sym-for-num (num)
	   (ecase num
	     (45 '<ring>)
	     (40 '<amulet>)
	     (55 '<staff>)
	     (65 '<wand>)
	     (66 '<rod>)
	     (70 '<scroll>)
	     (75 '<potion>)
	     (80 '<mushroom>))))
    
    (let ((*print-case* :downcase)
	  (*print-right-margin* 120))
      (with-open-file (ffile (pathname "bas-flavours.lisp")
			     :direction :output
			     :if-exists :supersede
                           :if-does-not-exist :create)

      (loop for gfx being the hash-values of *gfx-flavs*
	    for flav = (gfx-flav-flav gfx)
	    do
	    (pprint `(define-basic-flavour ',(get-sym-for-num (flav-tval flav)) ,(flav-name flav)
		      :x-attr ,(convert-obj (flav-colour flav) :letter) :x-char ,(flav-char flav))
		    ffile))
	    
      
      (loop for gfx being the hash-values of *gfx-flavs*
	    for flav = (gfx-flav-flav gfx)
	    for attr = (- (gfx-flav-attr gfx) 128)
	    for char = (- (gfx-flav-char gfx) 128)
	    do
	    (pprint `(update-flavour-display ',(get-sym-for-num (flav-tval flav)) ,(flav-name flav)
		      :x-attr (+ +graphics-start+ ,attr)
		      :x-char (+ +graphics-start+ ,char))
		    ffile))
      ))
    ))


(pushnew :compatibility-flavours cl:*features*)
