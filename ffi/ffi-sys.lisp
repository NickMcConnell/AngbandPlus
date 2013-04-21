;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.ffi -*-

#|

DESC: ffi/ffi-sys.lisp - basic code for ffi
Copyright (c) 2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.ffi)

(eval-when (:compile-toplevel :load-toplevel :execute)

#+allegro
(defconstant +c-null-value+ 0)
#-allegro
(defconstant +c-null-value+ nil)

;; the char-format
(deftype loc-char-format ()
  #+allegro
  '(unsigned-byte 8)
  #-allegro
  '(signed-byte 8)
  )
)

;; optimisation hacks..
(defvar *field-hack* (make-array 1024 :element-type 'loc-char-format :initial-element 0))
(defvar *field-cnt* 0)
(defvar *foreign-str-target* (make-array 1024 :element-type 'base-char :fill-pointer t :adjustable t))

;;(defmacro with-foreign-str ((str-var orig) &body body)
;;  `(funcall #'(lambda (,str-var) ,@body) ,orig))

(defun %get-fresh-str ()
  (let ((tg *foreign-str-target*))
    (setf (fill-pointer tg) 0)
    tg))


(defun str-to-arr (str)
  "Returns a pointer to something the ffi can eat."

;;  (declare (type simple-string str))

;;  (describe *field-hack*)
  
  (let ((len (if str (length str) 0))
        (arr *field-hack*))
;;    (declare (type (simple-array loc-char-format (*)) arr)
;;             (type u-16b len))
    
;;    (incf *field-cnt* len)
    (loop for x across str
          for i from 0
          do
          (setf (aref arr i) (char-code x)))
    
    (setf (aref arr len) 0)
    
    arr))

#||
FFI-Matrix:
             NULL        ""       "text"     foreign    field
CMU-txt       ok         ok         ok       ->field    conses 
CMU-arr       ok         ok         ok       ->field     ok  
ACL-txt       ok         ?          ?
ACL-arr       ok         ok         ok
CLISP-t
CLISP-a
||#

(defun to-arr (arg)
  "Tries to convert arg to an array for a C-function."
  (cond ((eq arg *foreign-str-target*)
	 #+(or cmu sbcl)
	 (to-arr (str-to-arr arg))
	 #+allegro
	 (to-arr (str-to-arr arg))
	 #+clisp
;;	 (to-arr (str-to-arr arg))
	 arg
	 #-(or cmu allegro clisp sbcl)
	 (error "foreign to arr"))
	((eq arg *field-hack*)
	 #+cmu
	 (system:vector-sap arg)
	 #+sbcl
	 (sb-sys:vector-sap arg)
	 #+allegro
	 arg
	 #+clisp
	 arg
	 #-(or sbcl cmu allegro clisp)
	 (error "field to arr"))
	((eql arg +c-null-value+)
	 #+cmu
	 (system:int-sap 0)
	 #+sbcl
	 (sb-sys:int-sap 0)
	 #+allegro
	 ;;(%str-to-arr nil)
	 +c-null-value+
	 #+clisp
	 +c-null-value+
	 #-(or sbcl cmu allegro clisp)
	 (error "null to arr")
	 )
	
	((typep arg 'simple-base-string)
	 #+cmu
	 (system:vector-sap arg)
	 #+sbcl
	 (sb-sys:vector-sap arg)
	 #+allegro
	 (str-to-arr arg)
	 #+clisp
	 arg
	 #-(or cmu allegro sbcl clisp)
	 (error "simple to arr"))
	((stringp arg)
	 (error "ord. str to arr"))
	(t
	 (error "fell through when making ffi-arr"))))

;;(trace to-arr)


(defun to-str (arg)
  "Tries to convert arg to something the ffi-str argument can eat."

  (cond ((eq arg *foreign-str-target*)
	 #+(or sbcl cmu)
	 (to-str (str-to-arr arg))
	 #+allegro
	 arg
	 #-(or cmu allegro sbcl)
	 (error "foreign to txt"))
	((eq arg *field-hack*)
	 #+(or sbcl cmu)
	 ;; this must be made non-consing
	 (let* ((len (length arg))
		(foo (make-string len)))
	   (dotimes (i len)
	     (setf (schar foo i) (code-char (aref arg i))))
	   foo)
	 #-(or sbcl cmu)
	 (error "field to txt"))
	((eql arg +c-null-value+)
	 +c-null-value+)
	((typep arg 'simple-base-string)
	 arg)
	((stringp arg)
	 ;; gamble
	 arg)
	(t
	 (error "fell through when making ffi-string"))))
