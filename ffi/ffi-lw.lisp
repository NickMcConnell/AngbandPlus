;;; Please do not edit this _GENERATED_ file.


(in-package :org.langband.ffi)
(fli:define-foreign-type angbyte () ':char)

(fli:define-foreign-type cptr () ':pointer)

(fli:define-foreign-type errr () ':int)


(fli:define-foreign-function (c_current_ui "current_ui") nil
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c_quit! "z_quit") ((msg :pointer))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-clear-from! "clear_from") ((row :int))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-prt-token! "print_coloured_token") ((colour
                                                                     angbyte)
                                                                    (token
                                                                     :int)
                                                                    (row :int)
                                                                    (col :int))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-prt-stat! "print_coloured_stat") ((colour
                                                                   angbyte)
                                                                  (stat :int)
                                                                  (row :int)
                                                                  (col :int))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-prt-number! "print_coloured_number") ((colour
                                                                       angbyte)
                                                                      (number
                                                                       :long)
                                                                      (padding
                                                                       :int)
                                                                      (row
                                                                       :int)
                                                                      (col
                                                                       :int))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c_term_putstr! "Term_putstr") ((col :int)
                                                             (row :int)
                                                             (something :int)
                                                             (colour angbyte)
                                                             (text :pointer))
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c_term_erase! "Term_erase") ((col :int)
                                                           (row :int)
                                                           (something :int))
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-term-queue-char! "Term_queue_char") ((row :int)
                                                                     (col :int)
                                                                     (colour
                                                                      angbyte)
                                                                     (the-char
                                                                      :char))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-term-gotoxy! "Term_gotoxy") ((row :int)
                                                             (col :int))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-set-cursor& "Term_set_cursor") ((col :int))
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-term-clear! "Term_clear") nil
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-term-fresh! "Term_fresh") nil
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-term-save! "Term_save") nil
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-term-load! "Term_load") nil
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-term-xtra& "Term_xtra") ((msg :int) (arg :int))
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-inkey! "inkey") nil
   :result-type :char :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (init_c-side& "init_c_side") ((ui :pointer)
                                                           (base-path :pointer)
                                                           (debug-level :int))
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (cleanup-c-side& "cleanup_c_side") nil
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c_macro_add& "macro_add") ((key :pointer)
                                                         (value :pointer))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)

(fli:define-foreign-function (c-set-lisp-system! "set_lisp_system") ((type
                                                                      :int))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)


#+use-callback-from-c
(fli:define-foreign-function (c-set-lisp-callback! "set_lisp_callback") ((name
                                                                          :pointer)
                                                                         (ptr
                                                                          ptr-type))
   :result-type :void :language :c :calling-convention :stdcall :module :lang-ffi)


#+win32
(fli:define-foreign-function (c-set-hinst! "setHINST") ((val :long))
   :result-type :int :language :c :calling-convention :stdcall :module :lang-ffi)


#+using-sound
(fli:define-foreign-function (c-load-sound& "load_sound") ((msg :int)
                                                           (fname :pointer))
   :result-type errr :language :c :calling-convention :stdcall :module :lang-ffi)


(eval-when (:execute :load-toplevel :compile-toplevel)
  (export
   '(c_current_ui c_quit! c-clear-from! c-prt-token! c-prt-stat! c-prt-number!
     c_term_putstr! c_term_erase! c-term-queue-char! c-term-gotoxy!
     c-set-cursor& c-term-clear! c-term-fresh! c-term-save! c-term-load!
     c-term-xtra& c-inkey! init_c-side& cleanup-c-side& c_macro_add&
     c-set-lisp-system! c-set-lisp-callback! c-set-hinst! c-load-sound&)))

;;; End of generated file.
