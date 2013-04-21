;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: sys.lisp - Various system-related code
Copyright (c) 2001-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

;; ripped from clocc/port
(defun lbsys/getenv (var)
  "Return the value of the environment variable."
  #+allegro (sys::getenv (string var))
  #+clisp (sys::getenv (string var))
  #+cmu (cdr (assoc (string var) ext:*environment-list* :test #'equalp
                    :key #'string))
  #+gcl (si:getenv (string var))
  #+lispworks (lw:environment-variable (string var))
  #+lucid (lcl:environment-variable (string var))
  #+sbcl (sb-ext:posix-getenv var)
  #-(or allegro clisp cmu gcl lispworks lucid sbcl)
  (error 'not-implemented :proc (list 'getenv var)))


(defun lbsys/make-sure-dirs-exist& (dir &key (verbose nil))
  "mostly a call to ENSURE-DIRECTORIES-EXIST,
but stops errors from floating out.. returns NIL instead."
  (let ((the-path (merge-pathnames (etypecase dir
                                     (pathname dir)
                                     (string (pathname dir))))))
    (handler-case
        (ensure-directories-exist the-path 
                                  :verbose verbose)
      (file-error (co)
        (warn "Something went wrong [~a] during directory creation [~s], returning NIL."
              co the-path)
        nil))))

(defun lbsys/ensure-dir-name (str)
  "Makes sure the str has a / suffix"
  (if (and str (not (eq (char str (1- (length str))) #\/)))
      (concatenate 'string str "/")
      str))

(defun home-langband-path ()
  "Returns the path (as a string, not pathname) to the langband-dir in the home-dir."
  #-win32
  (ignore-errors
    (let ((home-dir (lbsys/getenv "HOME")))
      (when (and home-dir (length home-dir))
        (setq home-dir (lbsys/ensure-dir-name home-dir))
        ;;      (print home-dir)
        (concatenate 'string home-dir ".angband/langband/"))))
  #+win32
  "c:/")
