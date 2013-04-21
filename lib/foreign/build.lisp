#|

DESC: lib/foreign/build.lisp - batch lisp-code for building FFI-declarations

|#

(in-package :cl-user)

(setq ext:*gc-verbose* nil
      *compile-print* nil)

(load "../../package")

(in-package :langband-ffi)

(load "ffigen")
(load "defs")

(generate-for-type :cmucl "gen-cmu.lisp")
(generate-for-type :acl   "gen-acl.lisp")
(generate-for-type :clisp "gen-clisp.lisp")

#+cmu
(when ext:*batch-mode*
  (cl-user::quit))
