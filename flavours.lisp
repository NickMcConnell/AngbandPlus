;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: flavours.lisp - basic code for flavour
Copyright (c) 2000-2002 - Stig Erik Sandø

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
  (generator-fn nil) ;; generator-function should return a cons
  )


(defun define-flavour-type (symbol &optional generator-fn)
  "Defines a flavour-type"
  (let* ((var-obj *variant*)
	 (ft-obj (make-flavour-type :symbol symbol
				    :generator-fn generator-fn))
	 (table (variant.flavour-types var-obj)))
    (setf (gethash symbol table) ft-obj)
    ft-obj))

(defun legal-flavour-obj? (flav)
  (and (consp flav)
       (stringp (car flav))
       (atom (car flav)) ;; integer 0..16 ?
       ))
  
(defun establish-flavour& (table name colour)
  (setf (gethash name table) (cons name colour)))

(defun find-flavour-type (variant-obj type)
  "Tries to find given flavour-type in given variant-obj."
  (gethash type (variant.flavour-types variant-obj)))

;;(trace find-flavour-type)

(defun define-basic-flavour (type name colour)
  "Defines a basic flavour.."
  
  (let ((ft-obj (find-flavour-type *variant* type)))
    (unless ft-obj
      (warn "Unable to find flavour-type ~s" type)
      (return-from define-basic-flavour nil))
    (let ((table (flavour-type.table ft-obj)))
      (establish-flavour& table name colour))))
	


(defun use-flavour-table (flavour-to-use used-by &key (variant *variant*))
  "a handy way to re-use a flavour-table for another kind
of objects.  all entries are copied, not shared."
  
  (let* ((var-obj variant)
	 (used-by-type (find-flavour-type var-obj used-by))
	 (type-to-use (find-flavour-type var-obj flavour-to-use))
	 (old-table (flavour-type.table type-to-use))
	 (new-table (flavour-type.table used-by-type)))
	 
    
    (maphash #'(lambda (key val)
		 (setf (gethash key new-table) val))
	     old-table)
    
    ;;(warn "~s will use ~s" used-by-type type-to-use)
    
    used-by-type))

(defmethod flavour-object! ((variant variant) (obj object-kind))
  ;; do nothing
  (warn "Not added flavouring to ~a" obj)
  nil)

(defun %flavour-obj-kind! (obj)
  "Flavours the given object OBJ."
  (let* ((kind obj) ;; hack
	 (var-obj *variant*)
	 (f-type (gethash (object.the-kind obj) (variant.flavour-types var-obj))))
    (when f-type
      (let ((gen-fn (flavour-type.generator-fn f-type))
	    (table (flavour-type.table f-type)))
	(cond (gen-fn
	       (setf (object.flavour kind) (funcall gen-fn kind)))
	      ((and table (typep table 'array))
	       (let ((next-flavour (aref table (fill-pointer table))))
		 (setf (object.flavour kind) next-flavour)
		 (incf (fill-pointer table))))
	      (t
	       (describe f-type)
	       (error "Unable to flavour object kind ~a with ~s" kind (flavour-type.symbol f-type))
	       ))
    
	(assert (legal-flavour-obj? (object.flavour kind)))))
    obj))



