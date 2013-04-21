;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/creatures.lisp - code dealing with non-player creatures
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.contraband)

(defmethod produce-monster-kind ((variant contraband) id name &key the-kind)
  (declare (ignore the-kind))
  
  (assert (stringp id))
  (assert (stringp name))

  (let ((retval (make-instance 'con/monster-kind :id id :name name)))

    ;; add stuff here?

    retval))

  
  
(defmethod initialise-monster-kind! ((var-obj contraband) (m-obj con/monster-kind) keyword-args)

  (call-next-method)

  ;; add stuff here
  (when-bind (pic (getf keyword-args :picture))
    (setf (monster.picture m-obj) pic))

  m-obj)


(defmethod initialise-monsters& ((var-obj contraband) &key old-file (file "monsters"))
  "old-file is ignored in contraband."
  (declare (ignore old-file))
  
  (cond (file
	 (let ((*load-verbose* nil))
	   (load-variant-data& var-obj file)))
	(t
	 (error "No file specified for monster-init.")))
    
  ;; initialise all tables
  (let ((object-tables (variant.monsters-by-level var-obj)))
    (maphash #'(lambda (key obj)
		 (con/update-gobj-table! var-obj key obj
					 #'create-alloc-table-monsters))
	     object-tables)))
