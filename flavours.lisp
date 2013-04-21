;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

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

(in-package :org.langband.engine)

;; no known flavour-types initially
;;(defvar *flavour-types* nil "should be a list")

(defstruct (flavour-type (:conc-name flavour-type.))
  (symbol nil)
  (table (make-hash-table)) ;; this will be changed to a vector later
  (generator-fn nil))


(defun define-flavour-type (symbol &optional generator-fn)
  "Defines a flavour-type"
  (let ((ft-obj (make-flavour-type :symbol symbol
				   :generator-fn generator-fn)))
    (push ft-obj (variant.flavour-types *variant*))
    ft-obj))

  
(defun establish-flavour& (table name colour)
  (setf (gethash name table) (list name colour)))

(defun find-flavour-type (variant-obj type)
  "Tries to find given flavour-type in given variant-obj."
  (find type (variant.flavour-types variant-obj) :key #'flavour-type.symbol))

(defun define-basic-flavour (type name colour)
  "Defines a basic flavour.."
  
  (let ((ft-obj (find-flavour-type *variant* type)))
    (unless ft-obj
      (warn "Unable to find flavour-type ~s" type)
      (return-from define-basic-flavour nil))
    (let ((table (flavour-type.table ft-obj)))
      (establish-flavour& table name colour))))
	


(defun use-flavour-table (flavour-to-use used-by)
  "a handy way to re-use a flavour-table for another kind
of objects.  all entries are copied, not shared."
  
  (let* ((var-obj *variant*)
	 (used-by-type (find-flavour-type var-obj used-by))
	 (type-to-use (find-flavour-type var-obj flavour-to-use))
	 (old-table (flavour-type.table type-to-use))
	 (new-table (flavour-type.table used-by-type)))
	 
    
    (maphash #'(lambda (key val)
		 (setf (gethash key new-table) val))
	     old-table)
    
    ;;(warn "~s will use ~s" used-by-type type-to-use)
    
    used-by-type))


;; move me later
(defmethod activate-object ((obj active-object) &key)
  "Ensures that the kind in question is flavoured."
  
  (block flavouring
    (let ((the-kind (aobj.kind obj)))
      ;; hack
      (when (and the-kind (eq (object.flavour the-kind) nil))
	(let ((flavour-type-list (variant.flavour-types *variant*)))
	  (dolist (i flavour-type-list) 
	    ;;(warn "Checking ~s vs ~s" (object.obj-type the-kind) (flavour-type.symbol i)) 
	    (when (obj-is? the-kind (flavour-type.symbol i))
	      (flavour-obj-kind! the-kind i)
	      (return-from flavouring obj))))))
    
    obj))
	

(defun flavour-obj-kind! (kind f-type)
  "Tries to flavour a given object-kind with a given flavour-type object."
  
  (let ((gen-fn (flavour-type.generator-fn f-type))
	(table (flavour-type.table f-type)))
    (cond (gen-fn
	   (setf (object.flavour kind) (funcall gen-fn kind)))
	  ((and table (typep table 'array))
	   (setf (object.flavour kind) (aref table (fill-pointer table)))
	   (incf (fill-pointer table)))
	  (t
	   (warn "Unable to flavour object kind ~a with ~s" kind f-type)
	   (describe f-type)
	   ))
    kind))

;;(trace flavour-obj-kind!)
