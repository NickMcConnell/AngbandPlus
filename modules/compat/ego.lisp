;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: lib/compat/ego.lisp - reads standard vanilla ego-items
Copyright (c) 2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.


|#

(in-package :org.langband.engine)


(defun compat-read-ego-file& (fname)
  "Reads floor from 2.9.3"
  
  (with-open-file (in-str (pathname fname)
			  :direction :input)

    (let ((cur-ego nil))

      (loop for l = (read-line in-str nil 'eof)
	    until (eq l 'eof)
	    do
	    (let ((first-char (if (> (length l) 0)
				  (schar l 0)
				  nil)))
		
	      (case first-char
		((#\# nil #\Space) nil)
		;; Version
		((#\V #\d);;(format t "Version: ~a~%" l)
		 )

		;; Name
		((#\N #\n)
		 (let ((res (split-seq-on l #\:)))
		   (when cur-ego
;;		     (warn "Register ego ~s" cur-ego)
		     (setf (get-ego (ego.number cur-ego)) cur-ego)

		     (setq cur-ego nil))
		 
		   (setq cur-ego (make-instance 'ego-item))

		   ;; the first should be N
		   (assert (string-equal (car res) "n"))
		   ;; the second should be the numeric id
		   (setf (ego.number cur-ego) (parse-integer (second res)))
		   ;; the third is the name
		   (setf (ego.name cur-ego) (third res))

		   ))
		
		;; aux-info 
		((#\X #\x)
		 
		 (when cur-ego
		   (let ((res (split-seq-on l #\:)))
		     ;; the first should be X
		     (assert (string-equal (first res) "x"))
		     
		     ;; the second should be the inventory-slot
		     (setf (ego.inv-slot cur-ego) (parse-integer (second res)))
		     ;; the third should be the rating
		     (setf (ego.rating cur-ego) (parse-integer (third res)))
		     ;; the fourth should be some extra-slot
		     (setf (ego.xtra cur-ego) (parse-integer (fourth res)))
		     
		     )))

		;; combat
		((#\C #\c)
		 (when cur-ego
		   (let ((res (split-seq-on l #\:)))
		     ;; the first should be C
		     (assert (string-equal (first res) "c"))
		     
		     ;; the second should be the max-to-hit
		     (setf (ego.max-to-hit cur-ego) (parse-integer (second res)))
		     ;; the third should be the max-to-dmg
		     (setf (ego.max-to-dmg cur-ego) (parse-integer (third res)))
		     ;; the fourth should be the max-to-ac
		     (setf (ego.max-to-ac cur-ego) (parse-integer (fourth res)))
		     ;; the fifth should be the pval
		     (setf (ego.pval cur-ego) (parse-integer (fifth res)))
		   
		   )))

		;; other vital info
		((#\W #\w)
		 (when cur-ego
		   (let ((res (split-seq-on l #\:)))
		     ;; the first should be W
		     (assert (string-equal (first res) "w"))
		     
		     ;; the second should be the depth
		     (setf (ego.depth cur-ego) (parse-integer (second res)))
		     ;; the third should be the rarity
		     (setf (ego.rarity cur-ego) (parse-integer (third res)))
		     ;; the fourth should be the weight
		     (setf (ego.weight cur-ego) (parse-integer (fourth res)))
		     ;; the fifth should be the cost
		     (setf (ego.cost cur-ego) (parse-integer (fifth res)))
		   
		     )))

		;; type-info
		((#\T #\t)
		 (when cur-ego
		   (let ((res (split-seq-on l #\:)))
		     ;; the first should be T
		     (assert (string-equal (first res) "t"))
		     
		     ;; the second should be the tval
		     (setf (ego.tval cur-ego) (parse-integer (second res)))
		     ;; the third should be the min-sval
		     (setf (ego.min-sval cur-ego) (parse-integer (third res)))
		     ;; the fourth should be the max-sval
		     (setf (ego.max-sval cur-ego) (parse-integer (fourth res)))
		   
		   )))

		;; flags
		((#\f #\F)
                  (when cur-ego
                    (let* ((res (split-seq-on (subseq l 2) #\|))
                           (real-res (loop for i in res
					   collecting (string-trim '(#\Space #\Tab #\Newline) i))))
                     
                      (dolist (j real-res)
                        (when (> (length j) 0)
			  (cond ((string-equal j "INT")
				 (pushnew '<int> (ego.flags cur-ego)))
				((string-equal j "STR")
				 (pushnew '<str> (ego.flags cur-ego)))
				((string-equal j "WIS")
				 (pushnew '<wis> (ego.flags cur-ego)))
				((string-equal j "CON")
				 (pushnew '<con> (ego.flags cur-ego)))
				((string-equal j "CHR")
				 (pushnew '<chr> (ego.flags cur-ego)))
				((string-equal j "DEX")
				 (pushnew '<dex> (ego.flags cur-ego)))
				
				((string-equal j "SEE_INVIS")
				 (pushnew '<see-invisible> (ego.flags cur-ego)))
				((string-equal j "REGEN")
				 (pushnew '<regeneration> (ego.flags cur-ego)))
				((string-equal j "FREE_ACT")
				 (pushnew '<free-action> (ego.flags cur-ego)))
				((string-equal j "FEATHER")
				 (pushnew '<feather-fall> (ego.flags cur-ego)))
				((string-equal j "STEALTH")
				 (pushnew '<stealth> (ego.flags cur-ego)))
				((string-equal j "SEARCH")
				 (pushnew '<search> (ego.flags cur-ego)))
				((string-equal j "LITE")
				 (pushnew '<light-source> (ego.flags cur-ego)))
				((string-equal j "TUNNEL")
				 (pushnew '<tunnel> (ego.flags cur-ego)))
				((string-equal j "BLESSED")
				 (pushnew '<blessed-blade> (ego.flags cur-ego)))
				((string-equal j "BLOWS")
				 (pushnew '<extra-blows> (ego.flags cur-ego)))
				((string-equal j "SHOTS")
				 (pushnew '<extra-shots> (ego.flags cur-ego)))
				((string-equal j "SPEED")
				 (pushnew '<speed> (ego.flags cur-ego)))
				((string-equal j "MIGHT")
				 (pushnew '<extra-might> (ego.flags cur-ego)))
				((string-equal j "TELEPATHY")
				 (pushnew '<telepathy> (ego.flags cur-ego)))
				((string-equal j "INFRA")
				 (pushnew '<infravision> (ego.flags cur-ego)))
				((string-equal j "HOLD_LIFE")
				 (pushnew '<hold-life> (ego.flags cur-ego)))

				
				((string-equal j "SLAY_ORC")
				 (pushnew '(<slay> <orc>) (ego.flags cur-ego)))
				((string-equal j "SLAY_TROLL")
				 (pushnew '(<slay> <troll>) (ego.flags cur-ego)))
				((string-equal j "SLAY_DRAGON")
				 (pushnew '(<slay> <dragon>) (ego.flags cur-ego)))
				((string-equal j "SLAY_EVIL")
				 (pushnew '(<slay> <evil>) (ego.flags cur-ego)))
				((string-equal j "SLAY_DEMON")
				 (pushnew '(<slay> <demon>) (ego.flags cur-ego)))
				((string-equal j "SLAY_GIANT")
				 (pushnew '(<slay> <giant>) (ego.flags cur-ego)))
				((string-equal j "SLAY_ANIMAL")
				 (pushnew '(<slay> <animal>) (ego.flags cur-ego)))
				((string-equal j "SLAY_UNDEAD")
				 (pushnew '(<slay> <undead>) (ego.flags cur-ego)))
				((string-equal j "KILL_DRAGON")
				 (pushnew '(<execute> <dragon>) (ego.flags cur-ego)))
				
				((string-equal j "SUST_STR")
				 (pushnew '(<sustain> <str>) (ego.flags cur-ego)))
				((string-equal j "SUST_DEX")
				 (pushnew '(<sustain> <dex>) (ego.flags cur-ego)))
				((string-equal j "SUST_CON")
				 (pushnew '(<sustain> <con>) (ego.flags cur-ego)))
				((string-equal j "SUST_INT")
				 (pushnew '(<sustain> <int>) (ego.flags cur-ego)))
				((string-equal j "SUST_WIS")
				 (pushnew '(<sustain> <wis>) (ego.flags cur-ego)))
				((string-equal j "SUST_CHR")
				 (pushnew '(<sustain> <chr>) (ego.flags cur-ego)))
								
				((string-equal j "LIGHT_CURSE")
				 (pushnew '(<curse> <light>) (ego.flags cur-ego)))
				((string-equal j "HEAVY_CURSE")
				 (pushnew '(<curse> <heavy>) (ego.flags cur-ego)))
				((string-equal j "AGGRAVATE")
				 (pushnew '<aggravate> (ego.flags cur-ego)))
				((string-equal j "TELEPORT")
				 (pushnew '<random-teleport> (ego.flags cur-ego)))
				
				((string-equal j "BRAND_FIRE")
				 (pushnew '(<brand> <fire>) (ego.flags cur-ego)))
				((string-equal j "BRAND_COLD")
				 (pushnew '(<brand> <cold>) (ego.flags cur-ego)))
				((string-equal j "BRAND_ACID")
				 (pushnew '(<brand> <acid>) (ego.flags cur-ego)))
				((string-equal j "BRAND_ELEC")
				 (pushnew '(<brand> <electricity>) (ego.flags cur-ego)))
				
				((string-equal j "IGNORE_ACID")
				 (pushnew '(<ignore> <acid>) (ego.flags cur-ego)))
				((string-equal j "IGNORE_FIRE")
				 (pushnew '(<ignore> <fire>) (ego.flags cur-ego)))
				((string-equal j "IGNORE_COLD")
				 (pushnew '(<ignore> <cold>) (ego.flags cur-ego)))
				((string-equal j "IGNORE_ELEC")
				 (pushnew '(<ignore> <electricity>) (ego.flags cur-ego)))
				
				((string-equal j "RES_FIRE")
				 (pushnew '(<resist> <fire>) (ego.flags cur-ego)))
				((string-equal j "RES_COLD")
				 (pushnew '(<resist> <cold>) (ego.flags cur-ego)))
				((string-equal j "RES_ELEC")
				 (pushnew '(<resist> <electricity>) (ego.flags cur-ego)))
				((string-equal j "RES_ACID")
				 (pushnew '(<resist> <acid>) (ego.flags cur-ego)))
				((string-equal j "RES_LITE")
				 (pushnew '(<resist> <light>) (ego.flags cur-ego)))
				((string-equal j "RES_DARK")
				 (pushnew '(<resist> <darkness>) (ego.flags cur-ego)))
				((string-equal j "RES_BLIND")
				 (pushnew '(<resist> <blindness>) (ego.flags cur-ego)))

				((string-equal j "HIDE_TYPE")
				 (pushnew '<hide-type> (ego.flags cur-ego)))
				((string-equal j "SHOW_MODS")
				 (pushnew '<show-modifiers> (ego.flags cur-ego)))
				(t
				 (warn "Flag ~s fell through" j)))))
		      )))
				 
		
		(t
		 (format t "Unhandled [~s]: ~a~%" first-char l)))
	      ))
      
      (when cur-ego
;;	(warn "Handle ego ~s" cur-ego)
	(setf (get-ego (ego.number cur-ego)) cur-ego)
	(setq cur-ego nil))
      )))


(pushnew :compatibility-ego cl:*features*)
