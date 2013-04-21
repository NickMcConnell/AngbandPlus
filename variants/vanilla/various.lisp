;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: variants/vanilla/various.lisp - various helper-stuff that should be compiled
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :langband)


(defconstant +scroll-syllables+ #1A(
				    "a" "ab" "ag" "aks" "ala" "an" "ankh" "app"
				    "arg" "arze" "ash" "aus" "ban" "bar" "bat" "bek"
				    "bie" "bin" "bit" "bjor" "blu" "bot" "bu"
				    "byt" "comp" "con" "cos" "cre" "dalf" "dan"
				    "den" "der" "doe" "dok" "eep" "el" "eng" "er" "ere" "erk"
				    "esh" "evs" "fa" "fid" "flit" "for" "fri" "fu" "gan"
				    "gar" "glen" "gop" "gre" "ha" "he" "hyd" "i"
				    "ing" "ion" "ip" "ish" "it" "ite" "iv" "jo"
				    "kho" "kli" "klis" "la" "lech" "man" "mar"
				    "me" "mi" "mic" "mik" "mon" "mung" "mur" "nag" "nej"
				    "nelg" "nep" "ner" "nes" "nis" "nih" "nin" "o"
				    "od" "ood" "org" "orn" "ox" "oxy" "pay" "pet"
				    "ple" "plu" "po" "pot" "prok" "re" "rea" "rhov"
				    "ri" "ro" "rog" "rok" "rol" "sa" "san" "sat"
				    "see" "sef" "seh" "shu" "ski" "sna" "sne" "snik"
				    "sno" "so" "sol" "sri" "sta" "sun" "ta" "tab"
				    "tem" "ther" "ti" "tox" "trol" "tue" "turs" "u"
				    "ulk" "um" "un" "uni" "ur" "val" "viv" "vly"
				    "vom" "wah" "wed" "werg" "wex" "whon" "wun" "x"
				    "yerg" "yp" "zun" "tri" "blaa"))

(defvar *van-used-scroll-names* (make-hash-table :test #'equal)
  "a table with already created names of scrolls")

(defun van-make-scroll-name ()
  "Returns a string with the name of a scroll"
  (let ((scroll-name "")
	(syl-len (length +scroll-syllables+)))
    (loop
     (let ((syl-num (random 2)))
       (setq scroll-name (concatenate 'string
				      scroll-name
				      (svref +scroll-syllables+ (random syl-len))
				      ;; possible second syllable
				      (if (= syl-num 1)
					  (svref +scroll-syllables+ (random syl-len))
					  "")
				      " "))
       ;; if long enough, return
       (when (< 15 (length scroll-name))
	 (return-from van-make-scroll-name (string-right-trim '(#\Space #\Tab #\Newline) scroll-name)))))

    "<failure in scr name-gen>"))

	 

(defun van-generate-scroll-flavour (object)
  "returns the flavour for the given object"
  
  (declare (ignore object))
  
  ;; make a name for the scroll
  (loop named naming-loop
	for name = (van-make-scroll-name)
	for hash-val = (gethash name *van-used-scroll-names*)
	do
	(unless hash-val
	  (setf (gethash name *van-used-scroll-names*) t)
	  (return-from van-generate-scroll-flavour (list name +term-white+)))))


(defvar *van-object-effects* (make-hash-table :test #'equal))


(defmethod use-object! ((var vanilla-variant) dun pl the-object)
;;  (declare (ignore var))
  (assert (typep the-object 'active-object))
  
  (let* ((okind (aobj.kind the-object))
	 (effects (object.effects okind))
	 (use-effect (assoc :use effects)))

    (cond (use-effect
	   (let ((effect-fun (cdr use-effect)))
	     (assert (functionp effect-fun))
	     (funcall effect-fun dun pl the-object)))
	  (t
	   ;;(warn "Didn't find any effect for ~s ~s" (object.id okind) (object.obj-type okind))
	   :used)
	  )))

(defun %van-sort-obj-types (obj-types)
  (sort obj-types #'string< :key #'symbol-name))

(defun van-ensure-object-effect (obj-types fun)
  (let ((sorted-types (%van-sort-obj-types obj-types)))
    (setf (gethash sorted-types *van-object-effects*) fun)) 
  (values))

(defun van-combine-effects-with-objects! (objects)
  "Tries to hack things together."
  (assert (hash-table-p objects))
  (let ((htbl *van-object-effects*))
    (when (hash-table-p htbl)
      (loop for o-table being the hash-values of objects
	    for okind-table = (gobj-table.obj-table o-table)
	    do
	    (loop for obj being the hash-values of okind-table
		  do
		  (let* ((the-types (%van-sort-obj-types (copy-seq (object.obj-type obj))))
			 (effect (gethash the-types htbl)))
		  (cond ((and effect (functionp effect))
;;			 (warn "Found effect for ~s ~a" the-types (compiled-function-p effect))
			 (push (cons :use effect) (object.effects obj))
			 (remhash the-types htbl))
			((obj-is? obj '<potion>)
			 ;;(warn "Found no definition for potion: ~s" the-types)
			 ))
		)))
    
      (loop for x being the hash-keys of htbl
	    do
	    (warn "Unable to find matching object for ~s effect." x)))
    
    ;; try to remove it, a good idea??
    (setf *van-object-effects* nil)
    
    (values)))


(defmacro define-object-effect (obj-types args &body the-body)
  (assert (= 3 (length args)))
  `(van-ensure-object-effect ',obj-types #'(lambda ,args ,@the-body)))
