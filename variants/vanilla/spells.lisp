;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: variants/vanilla/spells.lisp - spell-effects
Copyright (c) 2000-2001 - Stig Erik Sandø

This program is free software  ; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation	 ; either version 2 of the License, or
(at your option) any later version.

||#

(in-package :langband)

(defun light-room! (dungeon x y &key (type '<light>))
  "Lights the room."
  (let ((coords (make-queue)))
    (flet ((add-coord (bx by)
	     (let ((flag (cave-flags dungeon bx by)))
	       ;; no recursion
	       ;;(warn "flag at ~s,~s is ~s" bx by flag)
	       (when (or (bit-flag-set? flag +cave-temp+)
			 ;; don't leave the room
			 (not (bit-flag-set? flag +cave-room+)))
		 (return-from add-coord))

	       (bit-flag-add! (cave-flags dungeon bx by) +cave-temp+)
	       ;;(warn "adding ~s ~s" bx by)
	       (enqueue (cons bx by) coords))))

      ;; add first grid
      (add-coord x y)

      (dolist (i (queue-as-list coords))
	(let ((cx (car i))
	      (cy (cdr i)))
	  (when (cave-floor-bold? dungeon cx cy)
	    ;; next to
	    (add-coord (1+ cx) cy)
	    (add-coord (1- cx) cy)
	    (add-coord cx (1+ cy))
	    (add-coord cx (1- cy))

	    ;; diagonal
	    (add-coord (1+ cx) (1+ cy))
	    (add-coord (1- cx) (1- cy))
	    (add-coord (1+ cx) (1- cy))
	    (add-coord (1- cx) (1+ cy))
	    ))))

    ;;(warn "coords ~s" coords)
    
    (dolist (i (queue-as-list coords))
      (let ((flag (cave-flags dungeon (car i) (cdr i))))
	(bit-flag-remove! flag +cave-temp+)
	;;(warn "lighting ~s ~s" (car i) (cdr i))
	(ecase type
	  (<light>
	   (bit-flag-add! flag +cave-glow+))
	  (<darkness>
	   (bit-flag-remove! flag +cave-glow+)))
	(setf (cave-flags dungeon (car i) (cdr i)) flag)))

    ;; redraw things
    (bit-flag-add! *update* +forget-view+ +update-view+)
    (bit-flag-add! *redraw* +print-map+)
    
    t))


(defun light-area! (dungeon pl dmg radius &key (type '<light>))
  "Lights the area."
  (declare (ignore dmg radius))
  ;; unless blind
  (case type
    (<light>
     (c-print-message! "You are surrounded by a white light.")))
  ;; skip dmg
  (light-room! dungeon (location-x pl) (location-y pl) :type type)
  t)


(defun enchant-item! (dun pl &key (type '<weapon>) (bonus 1) (restrict nil))

  (flet ((%local-enchant (item)
	   (let ((gvals (object.game-values item)))
	     (warn "enchant ~s ~s ~s ~s" item type bonus restrict)
	     ;; improve later
	     (ecase type
	       (<weapon>
		(when (< (gval.tohit-bonus gvals) 10)
		  (incf (gval.tohit-bonus gvals) bonus)
		  (incf (gval.dmg-bonus gvals) bonus)
		  :used))
	       
	       (<armour>
		(when (< (gval.ac-bonus gvals) 10)
		  (incf (gval.ac-bonus gvals) bonus)
		  :used))))))
  
    (let ((retval :still-useful)
	  (selection (select-item dun pl '(:backpack :equip)
				  :prompt "Enchant item: "
				  :where :backpack)))

      (cond (selection
	     (let* ((the-table (get-item-table dun pl (car selection)))
		    (removed-obj (item-table-remove! the-table (cdr selection))))
	       (cond (removed-obj
		      (with-foreign-str (s)
			(lb-format s "~a ~a glow~a brightly."
				   "The" "[some-object, FIX]" "s")
			(c-print-message! s))
		      (setf retval (%local-enchant removed-obj))
		    
		      (item-table-add! the-table removed-obj))
		     (t
		      (warn "Did not find selected obj ~a" selection)))))
	    (t
	     (warn "Did not select anything.")))
    

      retval)))



;;(trace light-area! light-room!)
