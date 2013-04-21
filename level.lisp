;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: level.lisp - describing a level
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)


(eval-when (:compile-toplevel :load-toplevel :execute)
 
  (defclass level ()
    (
     (dungeon :accessor level.dungeon :initarg :dungeon :initform nil)
     (rating  :accessor level.rating  :initarg :rating  :initform nil)
     (depth   :accessor level.depth   :initarg :depth   :initform nil)
     ))


  (defclass random-level (level)
    ())


  (defclass themed-level (level)
    (
     (depth :accessor level.depth :initarg :depth :initform nil)
     )))

(defgeneric generate-level! (level player dungeon)
  (:documentation "Returns the level-object.  If the
dungeon object is valid and can be reused, it will be reused."))


(defmethod generate-level! (level player dungeon)
  (declare (ignore level player dungeon))
  (error "The basic GENERATE-LEVEL is not implemented, please pass
a proper LEVEL object."))

;; see generate.lisp and variants


(defgeneric create-appropriate-level (variant old-level player depth)
  (:documentation "Returns an appropriate level for the given
variant and player."))

(defmethod create-appropriate-level (variant old-level player depth)
  (declare (ignore old-level player depth))
  (error "CREATE-APPROPRIATE-LEVEL not implemented for variant ~a"
	 (type-of variant)))

(defgeneric level-ready? (level)
  (:documentation "Returns T if the level is ready for use, returns NIL otherwise."))

(defmethod level-ready? (level)
  (declare (ignore level))
  (error "pass a proper level to LEVEL-READY?"))


;;; random levels (see also generate.lisp)


(defun make-random-level-obj (depth)
  (make-instance 'random-level :depth depth))

(defmethod level-ready? ((level random-level))
  (when (level.dungeon level)
    t))



(defmethod post-initialise ((obj level) &key leave-method)

  (warn "post-init of level, ~a" leave-method)
  
  obj)

