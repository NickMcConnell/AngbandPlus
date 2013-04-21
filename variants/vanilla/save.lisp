;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/save.lisp - save/load related code
Copyright (c) 2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(defmethod save-object ((variant vanilla-variant) (player player) (stream l-binary-stream) indent)

  (call-next-method)
  (when (is-spellcaster? player)
    (let ((str (lang.stream stream))
	  (learnt-spells (class.learnt-spells (player.class player))))
      (bt:write-binary 'bt:u16 str (length learnt-spells))
      (loop for x across learnt-spells
	    do
	    (progn
	      ;;(warn "Saving spell ~s of ~s spells" x (length learnt-spells))
	      (save-binary-string x str)))))

  nil)

(defmethod save-object ((variant vanilla-variant) (player player) (stream l-readable-stream) indent)

  (call-next-method)
  
  (when (is-spellcaster? player)
    (let ((str (lang.stream stream))
	  (ind (get-indent-string indent)))
      
      (format str "~a (filed-player-data *variant* *player* :learnt-spells '~s)~%"
	      ind (loop for i across (class.learnt-spells (player.class player))
			collecting i))))
  
  nil)
  
(defmethod load-object ((variant vanilla-variant) (type (eql :player)) (stream l-binary-stream))
  (let ((player (call-next-method)))
    ;;(warn "Loading ~s vs ~s" (saveheader.engine-num-version *saveheader*) *engine-num-version*)
    (when (and (is-spellcaster? player)
	       (>= (saveheader.engine-num-version *saveheader*) 124))
      (let* ((str (lang.stream stream))
	     (len (bt:read-binary 'bt:u16 str))
	     (lspell-arr (class.learnt-spells (player.class player))))
	;;(warn "Spell-len ~s" len)
	(dotimes (i len)
	  (let ((id (load-binary-string str)))
	    ;;(warn "id is ~s" id)
	    (vector-push id lspell-arr)))

	;;(warn "Loaded spells ~s" lspell-arr)
	))

    player))


(defmethod filed-player-data ((variant vanilla-variant) (player player) &rest kwd-args &key  &allow-other-keys)

  (call-next-method)

  (when-bind (learnt-spells (getf kwd-args :learnt-spells))
    
    (cond ((not (is-spellcaster? player))
	   (warn "A non-spellcaster got learnt-spell info ~s" learnt-spells))

	  ((consp learnt-spells)
	   (let ((arr (class.learnt-spells (player.class player))))
	     (dolist (i learnt-spells)
	       (vector-push i arr))
	     ;;(warn "arr is ~s" arr)
	     ))
	  (t
	   (warn "Unknown format ~s for :learnt-spells." learnt-spells))))
  
  player)

(defmethod save-object ((variant vanilla-variant) (obj vanilla-variant) (stream l-binary-stream) indent)

  (call-next-method)

  (let ((str (lang.stream stream)))

    (bt:write-binary 'bt:u16 str 8) ;; number of houses
    
    ;; let's do homes and shops
    (dotimes (i 8)
      (let ((house (get-house (1+ i))))
	
	(let* ((items (house.items house))
	       (objs (items.objs items)))
	    
	  ;;(warn "Saving house ~s with ~s items" house (items.cur-size items))
	  
	  (bt:write-binary 'bt:u16 str (1+ i)) ;; house number
	  (bt:write-binary 'bt:u16 str (items.cur-size items))
	  (loop for x across objs
		when x
		do
		(save-object variant x stream indent))
	  
	  )))
    
    
    nil))


(defmethod save-object ((variant vanilla-variant) (obj vanilla-variant) (stream l-readable-stream) indent)

  (call-next-method)

  (let ((str (lang.stream stream))
	(ind (get-indent-string indent)))

    ;; let's do homes and shops
    (dotimes (i 8)
      (let ((house (get-house (1+ i))))
	
	(let* ((items (house.items house))
	       (objs (items.objs items)))
	    
	  ;;(warn "Saving house ~s with ~s items" house (items.cur-size items))
	  (format str "~a(filed-variant-data *variant* :house-number ~s :house-size ~s~%"
		  ind (1+ i) (items.cur-size items))
	  (format str "~a  :house-items (list " ind)
	  (loop for x across objs
		when x
		do
		(save-object variant x stream indent))
	  (format str "~a  ))~%" ind)
	  
	  )))
    
    
    nil))

(defmethod filed-variant-data ((variant vanilla-variant) &key house-number house-size house-items &allow-other-keys)

  (call-next-method)
  
  (when (and (integerp house-number) (integerp house-size) (listp house-items))
    (let ((house (get-house house-number variant)))
      (assert (= house-size (length house-items)))
      (unless (activated? house)
	(activate-object house))
      (dolist (i house-items)
	(item-table-add! (house.items house) i))))
  ;;(warn "Read house ~s of size ~s with ~s" house-number house-size house-items))
  
  variant)

(defmethod load-variant-object ((variant vanilla-variant) (stream l-binary-stream))
  (call-next-method)

  (when (>= (saveheader.engine-num-version *saveheader*) 125)
    (let* ((str (lang.stream stream)))
      (dotimes (i (bt:read-binary 'bt:u16 str))
	(let* ((house-num (bt:read-binary 'bt:u16 str))
	       (house-size (bt:read-binary 'bt:u16 str))
	       (items (loop for i from 0 below house-size
			    collecting (load-object variant :active-object stream))))
	  (filed-variant-data variant :house-number house-num
			      :house-size house-size
			      :house-items items)))))

  variant)

	  