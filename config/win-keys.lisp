;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: config/win-keys.lisp - key-definitions for windows keys

|#

(in-package :langband)

#||

 This file is used by Angband (when it was compiled using "main-ibm.c"
 or "main-dos.c" or "main-win.c") to specify various "user preferences",
 including "macros".

 This file defines some basic macros, which allow the use of the "keypad",
 alone, and with the shift and/or control modifier keys.  All "special"
 keys are translated by "main-ibm.c" (or "main-win.c") into special "macro
 triggers" of the encoded form "^_MMMxSS\r", where the "modifier" flags are
 stored in "MMM", and the two digit hexidecimal scan code of the keypress is
 stored in "SS".
||#


(define-key-macros #\<
    "^_x56\\r")

(define-key-macros #\>
    "^_Sx56\\r")

(define-key-macros #\|
    "^_Ax56\\r")


;; Keypad (/,*,7,8,9,-,4,5,6,+,1,2,3,0,.)

(define-key-macros #\/
    "^_x35\\r")

(define-key-macros #\*
    "^_x37\\r")

(define-key-macros #\7
    "^_x47\\r")

(define-key-macros #\8
    "^_x48\\r")

(define-key-macros #\9
    "^_x49\\r")

(define-key-macros #\-
    "^_x4A\\r")

(define-key-macros #\4
    "^_x4B\\r")

(define-key-macros #\5
    "^_x4C\\r")

(define-key-macros #\6
    "^_x4D\\r")

(define-key-macros #\+
    "^_x4E\\r")

(define-key-macros #\1
    "^_x4F\\r")

(define-key-macros #\2
    "^_x50\\r")

(define-key-macros #\3
    "^_x51\\r")

(define-key-macros #\0
    "^_x52\\r")

(define-key-macros #\.
    "^_x53\\r")

