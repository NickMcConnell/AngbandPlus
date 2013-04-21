#|

DESC: tools/ffi-build.lisp - batch lisp-code for building FFI-declarations

|#

(in-package :cl-user)

(setq ext:*gc-verbose* nil
      *compile-print* nil)

(pushnew :building-ffi-defs *features*)

;;(print *features*)

(load "../package.lisp")

(in-package :org.langband.ffi)

(load "ffi-gen")
(load "../ffi/ffi-defs")

(generate-for-type :cmucl "../ffi/ffi-cmu.lisp")
(generate-for-type :acl   "../ffi/ffi-acl.lisp")
(generate-for-type :clisp "../ffi/ffi-clisp.lisp")
(generate-for-type :lispworks "../ffi/ffi-lw.lisp")
(generate-for-type :sbcl "../ffi/ffi-sbcl.lisp")

#+cmu
(when ext:*batch-mode*
  (cl-user::quit))
