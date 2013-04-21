;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: lib/foreign/cmu.lisp - deals with foreign code for CMUCL
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: This file contains the code needed to access foreign functions
ADD_DESC: in C from CMUCL.

|#

(in-package :langband)

#||
(defmacro export-alien-routine (function &rest body)
  (let ((lisp-function (read-from-string function))
        (c-function (substitute #\_ #\- function)))
    `(progn
      (export '(,lisp-function))
      (proclaim '(inline ,lisp-function))
            (def-alien-routine ,c-function ,@body))))
||#

(use-package :c-call)

(eval-when (:compile-toplevel :load-toplevel :execute)
;;  (format t "Loading ~a~%" +shared-zterm-lib+)
  #-:already-loaded
  (alien:load-foreign +shared-zterm-lib+)
  (pushnew :already-loaded *features*))

(alien:def-alien-type byte c-call:unsigned-char)
(alien:def-alien-type cptr c-call:c-string)
(alien:def-alien-type errr c-call:int)

(alien:def-alien-routine ("z_quit" c-quit!)
    void
  (msg cptr))

(alien:def-alien-routine ("bell" c-bell)
    void
  (msg cptr))

(alien:def-alien-routine ("pause_line" c-pause-line)
    void
  (row int :in))

(alien:def-alien-routine ("clear_from" c-clear-from)
    void
  (row int :in))

(alien:def-alien-routine ("prt" c-prt)
    void
  (a cptr)
  (row int :in)
  (col int :in))

(alien:def-alien-routine ("msg_print" c-print-message)
    void
  (a cptr))


(alien:def-alien-routine ("put_str" c-put-str)
    void
  (a cptr)
  (row int :in)
  (col int :in))

(alien:def-alien-routine ("c_put_str" c-col-put-str)
    void
  (colour byte)
  (a cptr)
  (row int :in)
  (col int :in))


(alien:def-alien-routine ("Term_putstr" c-term-putstr)
    errr
  (row int :in)
  (col int :in)
  (c int :in)
  (d byte)
  (e cptr))


(alien:def-alien-routine ("Term_queue_char" c-term-queue-char)
    void
  (row int :in)
  (col int :in)
  (d byte)
  (e char))


(alien:def-alien-routine ("Term_gotoxy" c-term-gotoxy)
    void
  (row int :in)
  (col int :in))

(alien:def-alien-routine ("Term_set_cursor" c-set-cursor&)
    errr
  (col int :in))


(alien:def-alien-routine ("Term_clear" c-term-clear)
    errr)

(alien:def-alien-routine ("Term_fresh" c-term-clear)
    errr)

(alien:def-alien-routine ("Term_save" c-term-save)
    errr)

(alien:def-alien-routine ("Term_load" c-term-load)
    errr)


(alien:def-alien-routine ("Term_inkey" c-term-inkey&)
    errr
  (a cptr)
  (row int :in)
  (col int :in))

;;(alien:def-alien-routine ("read_some_key" c-read-some-key&)
;;    char
;;  (row int :in)
;;  (col int :in))

(alien:def-alien-routine ("inkey" c-inkey)
    char)

(alien:def-alien-routine ("init_gui" c-init-gui!)
    errr
  (argc int :in)
  (a (* cptr)))

(alien:def-alien-routine ("init_angband" c-init-angband!)
    void)

(alien:def-alien-routine ("macro_add" c-macro-add&)
    void
  (a cptr)
  (b cptr))


