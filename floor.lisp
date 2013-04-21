;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: floor.lisp - deals with floor/tile/feature code
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)


(eval-when (:compile-toplevel :load-toplevel :execute)
 
  (defclass feature-type ()
    ((id     :accessor feature.id     :initform nil)
     (name   :accessor feature.name   :initform nil)
     (x-attr :accessor feature.x-attr :initform nil)
     (x-char :accessor feature.x-char :initform nil)
     (mimic  :accessor feature.mimic  :initform nil))))

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
