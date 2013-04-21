;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: lib/common/objects.lisp - common objects
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

|#

(in-package :langband)

(defconstant +common-backpack-size+ 23)

(defun common-creating-backpack (state dungeon player aobj)
  "Assigns a container to aobj.contains."
  
  (declare (ignore player dungeon state))
  
  (let ((container (make-container +common-backpack-size+)))
    (setf (aobj.contains aobj) container)
    t))

;;(trace common-creating-backpack)
