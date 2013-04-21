;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: config/gcu-keys.lisp - key-definitions for gcu-keys

|#

(in-package :langband)

(define-key-macros #\.
    "\\e[3~")

(define-key-macros #\0
    "\\e[2~")

(define-key-macros #\1
    "\\e[4~"
  "\\e[F")

(define-key-macros #\2
    "\\e[B")

(define-key-macros #\3
    "\\e[6~")

(define-key-macros #\4
    "\\e[D")

(define-key-macros #\5
    "\\e[E"
  "\\e[G")

(define-key-macros #\6
    "\\e[C")

(define-key-macros #\7
    "\\e[1~"
  "\\e[H")

(define-key-macros #\8
    "\\e[A")

(define-key-macros #\9
    "\\e[5~")

