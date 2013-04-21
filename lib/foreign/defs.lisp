#|

DESC: lib/foreign/defs.lisp - the foreign declarations that [L] uses

|#

(in-package :langband-ffi)

(def-foreign-type byte uchar8)
(def-foreign-type cptr c-string8)
(def-foreign-type errr int32)

(def-foreign-function ("z_quit" c-quit!)
    :returns 'void
    :args '((cptr msg)))

(def-foreign-function ("bell" c-bell!)
    :returns 'void
    :args '((cptr msg)))

(def-foreign-function ("pause_line" c-pause-line!)
    :returns 'void
    :args '((int row)))

(def-foreign-function ("clear_from" c-clear-from!)
    :returns 'void
    :args '((int row)))

(def-foreign-function ("prt" c-prt!)
    :returns 'void
    :args '((cptr text)
	    (int row)
	    (int col)
	    ))

(def-foreign-function ("put_str" c-put-str!)
    :returns 'void
    :args '((cptr text)
	    (int row)
	    (int col)
	    ))

(def-foreign-function ("c_put_str" c-col-put-str!)
    :returns 'void
    :args '((byte colour)
	    (cptr text)
	    (int row)
	    (int col)
	    ))

(def-foreign-function ("print_coloured_token" c-prt-token!)
    :returns 'void
    :args '((byte colour)
	    (int token)
	    (int row)
	    (int col)
	    ))

(def-foreign-function ("print_coloured_stat" c-prt-stat!)
    :returns 'void
    :args '((byte colour)
	    (int stat)
	    (int row)
	    (int col)
	    ))

(def-foreign-function ("print_coloured_number" c-prt-number!)
    :returns 'void
    :args '((byte colour)
	    (long number)
	    (int padding)
	    (int row)
	    (int col)
	    ))


(def-foreign-function ("msg_print" c-print-message!)
    :returns 'void
    :args '((cptr msg)))

(def-foreign-function ("Term_putstr" c-term-putstr!)
    :returns 'errr
    :args '(
	    (int row)
	    (int col)
	    (int something)
	    (byte colour)
	    (cptr text)
	    ))

(def-foreign-function ("Term_queue_char" c-term-queue-char!)
    :returns 'void
    :args '(
	    (int row)
	    (int col)
	    (byte colour)
	    (char text)
	    ))

(def-foreign-function ("Term_gotoxy" c-term-gotoxy!)
    :returns 'void
    :args '(
	    (int row)
	    (int col)
	    ))

(def-foreign-function ("Term_set_cursor" c-set-cursor&)
    :returns 'errr
    :args '(
	    (int col)
	    ))

(def-foreign-function ("Term_clear" c-term-clear!)
    :returns 'errr)

(def-foreign-function ("Term_fresh" c-term-fresh!)
    :returns 'errr)

(def-foreign-function ("Term_save" c-term-save!)
    :returns 'errr)

(def-foreign-function ("Term_load" c-term-load!)
    :returns 'errr)

(def-foreign-function ("Term_xtra" c-term-xtra&)
    :returns 'errr
    :args '(
	    (int msg)
	    (int arg)
	    ))

(def-foreign-function ("Term_inkey" c-term-inkey&)
    :returns 'errr
    :args '((cptr text)
	    (int row)
	    (int col)
	    ))

(def-foreign-function ("inkey" c-inkey!)
    :returns 'char)

(def-foreign-function ("init_gui" c-init-gui!)
    :returns 'errr
    :args '((int argc)
	    ((* cptr) argv)
	    ))

(def-foreign-function ("init_angband" c-init.angband!)
    :returns 'void)

(def-foreign-function ("macro_add" c-macro-add&)
    :returns 'void
    :args '((cptr key)
	    (cptr value)))

(def-foreign-function ("set_lisp_callback" set_lisp_callback!)
    :returns 'void
    :args '((ptr-type ptr)
	    )
    :only-when 'use-callback-from-c)

(def-foreign-function ("load_sound" c-load-sound&)
    :returns 'errr
    :args '((int msg)
	    (cptr fname))
    :only-when 'using-sound)

