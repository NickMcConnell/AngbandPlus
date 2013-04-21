;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.contraband -*-

#|

DESC: variants/contraband/objects.lisp - code dealing with physical objects
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.contraband)

(defmethod initialise-objects& ((var-obj contraband) &key old-file (file "objects"))

  (declare (ignore old-file))
  (cond
    (file
     (let ((*load-verbose* nil))
       (load-variant-data& var-obj file)))
    (t
     (error "No file specified for floor-init.")))


  ;; initialise all tables
;;    (warn "Mapping ~a" object-tables)
  ;; let us find some behaviour

  (let ((object-tables (variant.objects-by-level var-obj)))
    
    (maphash #'(lambda (key obj)
		 (con/update-gobj-table! var-obj key obj
					 #'create-alloc-table-objects))
	     object-tables))


  
;;  #+langband-debug
;;  (%output-kinds-to-file "dumps/obj.lisp")
  )

(defmethod write-obj-description ((variant contraband) (obj active-object) stream
				  &key (store nil) (verbosity 1) (numeric-prefix t))

  (declare (ignore verbosity))
  (let* (;;(o-type (aobj.kind obj))
	 (name (object.name obj))
	 ;;(flavour (if store nil (object.flavour o-type)))
	 (known-type (or store (is-object-known? obj)))
	 (number (aobj.number obj))
	 ;;(plural-string nil)
	 )


    (let ((str (plural-name number name nil known-type nil :numeric-prefix numeric-prefix)))
      (write-string str stream))

    ))
