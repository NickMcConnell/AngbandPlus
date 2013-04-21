;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: variants/vanilla/base.lisp - the base variant class for Vanilla
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)


(defclass vanilla-variant (variant)
  ((dawn-time     :initarg :dawntime   :initform 0    :accessor variant.dawn)
   (twilight-time :initarg :twilight   :initform 6000 :accessor variant.twilight)
   ))

(defun van-make-variant-obj ()
  (make-instance 'vanilla-variant
		 :id 'langband-vanilla
		 :name "Vanilla"

		 ;; only used by development
		 :sys-file "./variants/vanilla/langband-vanilla.system"
		 :config-path
		 #+langband-development
		 "./variants/vanilla/config"
		 #-langband-development
		 "/var/lib/games/langband/vanilla"))


(register-variant& (van-make-variant-obj))
	   
