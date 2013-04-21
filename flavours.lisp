;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: flavours.lisp - basic code for flavour
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: This file contains the code which is visible to the
ADD_DESC: flavouring of variants

|#

(in-package :langband)

;; no known flavour-types initially
(defvar *flavour-types* nil "should be a list")

(defun define-flavour-type (symbol &optional generator-fn)
  "Defines a flavour-type"
  (setf (get symbol 'flavour-table) (make-hash-table))
  (push symbol *flavour-types*)
  (when generator-fn
    (setf (get symbol 'flavour-generator) generator-fn))
  (values))

  
(defun establish-flavour& (table name colour)
  (setf (gethash name table) (list name colour)))

(defun define-basic-flavour (type name colour)
  (let ((table (get type 'flavour-table)))
    (if table
	(establish-flavour& table name colour)
	(warn "Unknown flavour type ~a" type)))
  (values))


(defun use-flavour-table (used-table used-by)
  "a handy way to re-use a flavour-table for another kind
of objects."
  (let ((table (get used-table 'flavour-table)))
    (if table
	(setf (get used-by 'flavour-table) table)
	(warn "Unable to find a flavour table on ~a" used-table)))
  (values))
	      
