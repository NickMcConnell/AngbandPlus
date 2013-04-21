;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: LANGBAND -*-

#|

DESC: lib/vanilla/various.lisp - various helper-stuff that should be compiled
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
