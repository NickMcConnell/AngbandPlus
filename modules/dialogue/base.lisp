;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.dialogue -*-

#|

DESC: modules/dialogue/base.lisp - base definitions for conversation code
Copyright (c) 2002-2003 - Knut Arild Erstad

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.dialogue)


(defstruct (conversation-node (:conc-name cnode.))
  (id nil)            ; global id
  (text "")           ; text or a function returning text
  (perform nil)       ; function to perform at entry
  (options '())       ; all possible replies (including hidden ones)
  (skip-test nil)     ; function to test for "skipping" this node
  (skip-dest nil)     ; destination to skip to: node or id string
  )

(defstruct (conversation-option (:conc-name copt.))
  (text "<undefined>")   ; text or a function returning text
  (test nil)             ; optional test closure; hide if it returns false
  (perform nil)          ; function to perform when chosen
  (dest nil)             ; destination node (conversation-node), id of node
			 ; (string), one of several special keywords
			 ; like :QUIT (?), or a function returning one of the
                         ; above
  )

(defstruct (conversation-parameters (:conc-name cparam.))
  (player nil)  ; player character
  (npc nil) ; non-player character
  )

;; hash table with all conversation nodes
(defparameter *conversations* (make-hash-table :test 'equal))

;; stack of conversation nodes in current conversation (with no duplicates)
;; the "bottom" of the stack of the stack is the starting node
(defvar *current-conversation-nodes*)

