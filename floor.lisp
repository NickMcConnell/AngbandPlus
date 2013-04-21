;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: floor.lisp - deals with floor/tile/feature code
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)


(defclass feature-type ()
    ((id     :accessor feature.id     :initform nil :initarg :id)
     (name   :accessor feature.name   :initform nil :initarg :name)
     (x-attr :accessor feature.x-attr :initform nil :initarg :x-attr)
     (x-char :accessor feature.x-char :initform nil :initarg :x-char)
     (mimic  :accessor feature.mimic  :initform nil :initarg :mimic)
     ))

(defmethod print-object ((inst feature-type) stream)
  (print-unreadable-object
   (inst stream :identity t)
   (format stream "~:(~S~) [~S]" (class-name (class-of inst)) 
	   (feature.name inst)))
  inst)

(defun get-feature (id)
  "Returns an object of type FEATURE-TYPE or NIL."
  (let ((table (variant.floor-features *variant*)))
    (gethash id table)))

(defun (setf get-feature) (feature id)
  "Adds a feature with given id to the appropriate table."
  (let ((table (variant.floor-features *variant*)))
    (setf (gethash id table) feature)))

(defun define-feature-type (id name x-attr x-char &key mimic)
  "Defines a feature/floor-type and registers it.  The floor/feature
is returned."
  (let ((ftype (make-instance 'feature-type :id id
			      :name name
			      :x-attr (etypecase x-attr
					(number (charify-number x-attr)))
			      :x-char x-char
			      :mimic mimic)))
    (setf (get-feature id) ftype)
    ftype))


(defun dump-features (out-file &optional feature-list)
  (let* ((features (if feature-list
		       feature-list
		       (loop for x being the hash-values of (variant.floor-features *variant*)
			     collecting x)))
	 (sorted-features (sort (copy-list features) #'< :key #'feature.id)))

    (let ((*print-case* :downcase))
      (with-open-file (ffile (pathname out-file)
			     :direction :output
			     :if-exists :supersede
			     :if-does-not-exist :create)
	(loop for x in sorted-features
	      do
	      (pprint `(define-feature-type ,(feature.id x)
			,(feature.name x)
			,(feature.x-attr x)
			,(feature.x-char x)
			:mimic ,(feature.mimic x))
		      ffile))))))
