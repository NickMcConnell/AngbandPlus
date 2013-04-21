#|

DESC: ffi/ffi-defs.lisp - the foreign declarations that [L] uses

|#

(in-package :org.langband.ffi)

(def-foreign-type angbyte uchar8)
(def-foreign-type cptr c-string8)
(def-foreign-type errr int32)

(def-foreign-function ("current_ui" c_current_ui)
    :returns 'int)

(def-foreign-function ("z_quit" c_quit!)
    :returns 'void
    :args '((char-arr msg)))

;; kill
(def-foreign-function ("bell" c_bell!)
    :returns 'void
    :args '((char-arr msg)))


;; kill
(def-foreign-function ("clear_from" c-clear-from!)
    :returns 'void
    :args '((int row)))

;; kill
(def-foreign-function ("prt" c_prt!)
    :returns 'void
    :args '(;;(cptr text)
	    (char-arr text)
	    (int row)
	    (int col)
	    ))


(def-foreign-function ("print_coloured_token" c-prt-token!)
    :returns 'void
    :args '((angbyte colour)
	    (int token)
	    (int row)
	    (int col)
	    ))

(def-foreign-function ("print_coloured_stat" c-prt-stat!)
    :returns 'void
    :args '((angbyte colour)
	    (int stat)
	    (int row)
	    (int col)
	    ))

(def-foreign-function ("print_coloured_number" c-prt-number!)
    :returns 'void
    :args '((angbyte colour)
	    (long number)
	    (int padding)
	    (int row)
	    (int col)
	    ))


(def-foreign-function ("msg_print" c_msg_print!)
    :returns 'void
    :args '((char-arr msg)))
;;    :args '((cptr msg)))

;; possible kill
(def-foreign-function ("Term_putstr" c_term_putstr!)
    :returns 'errr
    :args '(
	    (int col)
	    (int row)
	    (int something)
	    (angbyte colour)
	    (char-arr text)
	    ))

(def-foreign-function ("Term_queue_char" c-term-queue-char!)
    :returns 'void
    :args '(
	    (int row)
	    (int col)
	    (angbyte colour)
	    (char the-char)
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
    :args '((char-arr text)
	    (int row)
	    (int col)
	    ))

(def-foreign-function ("inkey" c-inkey!)
    :returns 'char)

(def-foreign-function ("init_c_side" init_c-side&)
    :returns 'errr
    :args '((cptr ui)
	    (cptr base-path)
	    (int debug-level)
	    ))

(def-foreign-function ("cleanup_c_side" cleanup-c-side&)
    :returns 'errr)


(def-foreign-function ("macro_add" c_macro_add&)
    :returns 'void
    :args '((cptr key)
	    (cptr value)))

(def-foreign-function ("set_lisp_system" c-set-lisp-system!)
    :returns 'void
    :args '((int type)))

(def-foreign-function ("set_lisp_callback" c-set-lisp-callback!)
    :returns 'void
    :args '((ptr-type ptr)
	    )
    :only-when 'use-callback-from-c)

(def-foreign-function ("setHINST" c-set-hinst!)
    :returns 'int
    :args '((long val))
    :only-when 'win32)

(def-foreign-function ("load_sound" c-load-sound&)
    :returns 'errr
    :args '((int msg)
	    (cptr fname))
    :only-when 'using-sound)


#||
(def-foreign-function ("roff" c_roff!)
    :returns 'void
    :args '((angbyte colour)
	    (cptr text)
	    ))

(def-foreign-function ("test_calling" c_test_calling!)
    :returns 'int
    :args '((char-arr msg)
	    (cptr alt)))

(def-foreign-function ("test_calling_2" c_test_calling!)
    :returns 'int
    :args '((char-arr msg)))

(def-foreign-function ("put_str" c-put-str!)
    :returns 'void
    :args '((cptr text)
	    (int row)
	    (int col)
	    ))

(def-foreign-function ("c_put_str" c_col_put_str!)
    :returns 'void
    :args '((angbyte colour)
	    (char-arr text)
	    (int row)
	    (int col)
	    ))

(def-foreign-function ("init_angband" c-init.angband!)
    :returns 'void)

;; kill
#+never
(def-foreign-function ("pause_line" c-pause-line!)
    :returns 'void
    :args '((int row)))

||#
