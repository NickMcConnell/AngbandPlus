;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: lib/foreign/acl.lisp - deals with foreign code for ACL
Copyright (c) 2000 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: This file contains the code needed to access foreign functions
ADD_DESC: in C from ACL.

|#

(in-package :langband)

(load +shared-zterm-lib+)

(ff:def-foreign-type byte :unsigned-char)
(ff:def-foreign-type cptr (* :char))
(ff:def-foreign-type errr :int)

(ff:def-foreign-variable (c-inkey-flag "inkey_flag"))

(ff:def-foreign-call (c-quit! "z_quit") ((a (* :char)))
  :returning :void)

(ff:def-foreign-call (c-bell "bell") ((a (* :char)))
  :returning :void)


(ff:def-foreign-call (c-pause-line "pause_line") ((a :int fixnum))
  :returning :void)

(ff:def-foreign-call (c-clear-from "clear_from") ((a :int fixnum))
  :returning :void)

;;(ff:def-foreign-call (c-run-stuff "run_stuff") nil :returning :void)

(ff:def-foreign-call (c-term-putstr "Term_putstr") ((a :int fixnum)
						    (b :int fixnum)
						    (c :int fixnum)
						    (d byte)
						    (e cptr))
  :returning errr)

(ff:def-foreign-call (c-term-queue-char "Term_queue_char") ((a :int fixnum)
							    (b :int fixnum)
							    (d byte)
							    (e :char))
  :returning :void)

(ff:def-foreign-call (c-term-gotoxy "Term_gotoxy") ((a :int fixnum)
						    (b :int fixnum))

  :returning :void)


(ff:def-foreign-call (c-put-str "put_str") ((a cptr)
					    (row :int fixnum)
					    (col :int fixnum))
  :returning :void)

(ff:def-foreign-call (c-col-put-str "c_put_str") ((colour byte)
						  (a cptr)
						  (row :int fixnum)
						  (col :int fixnum))
  :returning :void)


(ff:def-foreign-call (c-term-clear "Term_clear") nil :returning errr)
(ff:def-foreign-call (c-term-fresh "Term_fresh") nil :returning errr)
(ff:def-foreign-call (c-term-save "Term_save") nil :returning errr)
(ff:def-foreign-call (c-term-load "Term_load") nil :returning errr)

(ff:def-foreign-call (c-init-angband! "init_angband") nil :returning :void)

(ff:def-foreign-call (c-inkey "inkey") nil :returning :char)


(ff:def-foreign-call (c-init-gui! "init_gui") ((a :int fixnum)
					       (b (* (* :char))))
  :returning errr)


(ff:def-foreign-call (c-prt "prt") ((a cptr)
				    (b :int fixnum)
				    (c :int fixnum))
  :returning :void)

(ff:def-foreign-call (c-print-message "msg_print") ((a cptr))
  :returning :void)

(ff:def-foreign-call (c-set-cursor& "Term_set_cursor") ((a :int fixnum))
  :returning errr)

(ff:def-foreign-call (c-term-inkey& "Term_inkey") ((b cptr)
						   (a :int fixnum)
						   (c :int fixnum))
  :returning errr)

;;(ff:def-foreign-call (c-read-some-key& "read_some_key") ((a :int fixnum)
;;							 (c :int fixnum))
;;  :returning :char)


(ff:def-foreign-call (c-macro-add& "macro_add") ((b cptr)
						 (a cptr))
  :returning :void)
