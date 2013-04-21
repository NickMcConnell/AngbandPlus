;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: config/x11-keys.lisp - key-definitions for x11-keys

|#

(in-package :langband)

(define-key-macros #\0
    "^__FFB0\\r"
  "^__FF63\\r"
  "^__????\\r"
  "^__KP_Insert\\r"
  "^_M_KP_0\\r"
  "^__FF9E\\r")

(define-key-macros #\1
    "^__FFB1\\r"
  "^__FF57\\r"
  "^__FFDE\\r"
  "^__KP_End\\r"
  "^_M_KP_1\\r"
  "^__FF9C\\r")

(define-key-macros #\2
    "^__FFB2\\r"
  "^__FF54\\r"
  "^__Down\\r"
  "^__KP_Down\\r"
  "^_M_KP_2\\r"
  "^__FFDF\\r" 
  "^__FF99\\r")

(define-key-macros #\3
    "^__FFB3\\r"
  "^__FF56\\r"
  "^__FFE0\\r"
  "^__KP_Next\\r"
  "^_M_KP_3\\r"
  "^__FF9B\\r")

(define-key-macros #\4
"^__FFB4\\r"
"^__FF51\\r"
"^__FFDB\\r"
"^__Left\\r"
"^__KP_Left\\r"
"^_M_KP_4\\r"
"^__FF96\\r")

(define-key-macros #\5
"^__FFB5\\r"
"^__FF80\\r"
"^__FFDC\\r"
"^__KP_Begin\\r"
"^_M_KP_5\\r"
"^__FF9D\\r")

(define-key-macros #\6
"^__FFB6\\r"
"^__FF53\\r"
"^__FFDD\\r"
"^__Right\\r"
"^__KP_Right\\r"
"^_M_KP_6\\r"
"^__FF98\\r")

(define-key-macros #\7
"^__FFB7\\r"
"^__FF50\\r"
"^__FFD8\\r"
"^__KP_Home\\r"
"^_M_KP_7\\r"
"^__FF95\\r")

(define-key-macros #\8
"^__FFB8\\r"
"^__FF52\\r"
"^__FFD9\\r"
"^__Up\\r"
"^__KP_Up\\r"
"^_M_KP_8\\r"
"^__FF97\\r")

(define-key-macros #\9
"^__FFB9\\r"
"^__FF55\\r"
"^__FFDA\\r"
"^__KP_Prior\\r"
"^_M_KP_9\\r"
"^__FF9A\\r")
