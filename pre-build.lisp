;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: cl-user -*-

#|

DESC: pre-build.lisp - settings that must be set before build
Copyright (c) 2001-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :cl-user)

#+clisp
(progn
  (format t "~&Removing some clisp-warnings.. we hope~%")
  ;;(push (pathname "@lisppath@/") *load-paths*)        
  (setq 
   clos::*gf-warn-on-removing-all-methods* nil
   clos::*warn-if-gf-already-called* nil
   clos::*gf-warn-on-replacing-method* nil
   system::*source-file-types* '(".lisp" ".lsp")))

#+cormanlisp
(setq cl::*support-eql-specializers* t)
  
#+ecl
(setq sys:*gc-verbose* nil)

  
#+cmu
(progn
  (setq ext:*gc-verbose* nil
	ext:*byte-compile-default* nil
	*compile-print* nil)
  #+pcl
  (pushnew 'compile pcl::*defclass-times*))
  
#+allegro
(progn
  (setf *load-local-names-info* t
	;;(sys:gsgc-switch :print) t
	))
 
