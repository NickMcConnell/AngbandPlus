;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: config/sdl-keys.lisp - key-definitions for sdl-keys

|#

(in-package :org.langband.engine)


(define-key-macros #\0
    "^__100\\r")

(define-key-macros #\1
    "^__101\\r")

(define-key-macros #\2
    "^__112\\r"
    "^__102\\r")

(define-key-macros #\3
    "^__103\\r")

(define-key-macros #\4
    "^__114\\r"
    "^__104\\r")

(define-key-macros #\5
    "^__105\\r")

(define-key-macros #\6
    "^__113\\r"
    "^__106\\r")

(define-key-macros #\7
    "^__107\\r")

(define-key-macros #\8
    "^__111\\r"
    "^__108\\r")

(define-key-macros #\9
    "^__109\\r")

;; shift-versions
(define-key-macros ".0"
    "^_S_100\\r")

(define-key-macros ".1"
    "^_S_101\\r")

(define-key-macros ".2"
    "^_S_102\\r"
    "^_S_112\\r"
    )

(define-key-macros ".3"
    "^_S_103\\r")

(define-key-macros ".4"
    "^_S_104\\r"
    "^_S_114\\r"
    )

(define-key-macros ".5"
    "^_S_105\\r")

(define-key-macros ".6"
    "^_S_106\\r"
    "^_S_113\\r"
    )

(define-key-macros ".7"
    "^_S_107\\r")

(define-key-macros ".8"
    "^_S_108\\r"
    "^_S_111\\r"
  )

(define-key-macros ".9"
    "^_S_109\\r")

