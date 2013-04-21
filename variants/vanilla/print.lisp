;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#||

DESC: variants/vanilla/print.lisp - printout code for vanilla
Copyright (c) 2002 - Stig Erik Sandø

This program is free software  ; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation	 ; either version 2 of the License, or
(at your option) any later version.

||#

(in-package :org.langband.vanilla)

(defmethod print-depth ((level level) (setting bottom-row-locations))
  "prints current depth somewhere"
  (with-frame (+misc-frame+)
    (let ((column (- (get-frame-width +misc-frame+) 8))) ;;(slot-value setting 'depth)))
      (put-coloured-line! +term-l-blue+ (format nil "~d ft" (* 50 (level.depth level))) column 0))))


(defmethod print-depth ((level van-town-level) (setting bottom-row-locations))
  "prints current depth somewhere"
  (let ((column (- (get-frame-width +misc-frame+) 8)));; (slot-value setting 'depth)))
    (with-frame (+misc-frame+)
      (put-coloured-line! +term-l-blue+ "Town"
			 column
			 0))))

(defmethod print-cut ((variant vanilla-variant) (player player) 
                      (setting vanilla-basic-frame-locations))

  (let* ((cuts (get-attribute-value '<cut> (player.temp-attrs player)))
         (loc (slot-value setting 'cut))
         (cut-info (%get-cutlvl cuts)))

    ;;(warn "print cut ~s ~s ~s" cuts cut-info loc) 
    (with-frame (+charinfo-frame+)
      (put-coloured-str! (second cut-info) (third cut-info)
			 (cdr loc) (car loc)))
    ))


(defmethod print-stun ((variant vanilla-variant) (player player) 
                      (setting vanilla-basic-frame-locations))
  
  (let* ((stun (get-attribute-value '<stun> (player.temp-attrs player)))
         (loc (slot-value setting 'stun))
         (stun-info (%get-stunlvl stun)))

    (with-frame (+charinfo-frame+)
      (put-coloured-str! (second stun-info) (third stun-info)
			 (cdr loc) (car loc)))
    ))

(defmethod print-poisoned ((variant vanilla-variant) (player player) 
                           (setting vanilla-bottom-row-locations))
  (with-frame (+misc-frame+)
    (let ((column (slot-value setting 'poisoned))
	  (row 0))

      (if (get-attribute-value '<poisoned> (player.temp-attrs player))
	  (put-coloured-str! +term-orange+ "Poisoned" column row)
	  (put-coloured-str! +term-white+  "        " column row))
      )))

(defmethod print-state ((variant vanilla-variant) (player player) 
			(setting vanilla-bottom-row-locations))

  (with-frame (+misc-frame+)
    (let ((word nil)
	  (colour +term-white+)
	  (temp-attrs (player.temp-attrs player))
	  (column (slot-value setting 'state))
	  (row 0))

      (cond ((get-attribute-value '<paralysed> temp-attrs)
	     (setf word "Paralysed!"
		   colour +term-red+))
	    ;; turn it off
	    (t
	     (setf word "          ")
	     ))
    
      (put-coloured-str! colour word column row))
    
    t))

(defmethod print-afraid ((variant vanilla-variant) (player player) 
			 (setting vanilla-bottom-row-locations))

  (with-frame (+misc-frame+)
    (let ((column (slot-value setting 'afraid))
	  (row 0))
      (if (get-attribute-value '<fear> (player.temp-attrs player))
	  (put-coloured-str! +term-orange+ "Afraid" column row)
	  (put-coloured-str! +term-white+  "      " column row))
      )))

(defmethod print-confused ((variant vanilla-variant) (player player) 
			   (setting vanilla-bottom-row-locations))
  (with-frame (+misc-frame+)
    
    (let ((column (slot-value setting 'confused))
	  (row 0))
      (if (get-attribute-value '<confusion> (player.temp-attrs player))
	  (put-coloured-str! +term-orange+ "Confused" column row)
	  (put-coloured-str! +term-white+  "        " column row))
      )))

(defmethod print-can-study-more ((variant vanilla-variant) (player player) 
				 (setting vanilla-bottom-row-locations))
  
  (with-frame (+misc-frame+)
    (let ((column (slot-value setting 'study))
	  (row 0))
      (if (can-learn-more-spells? variant player)
	  (put-coloured-str! +term-l-green+ "Study" column row)
	  (put-coloured-str! +term-white+  "     " column row))
      )))


(defmethod print-extra-frame-content ((variant vanilla-variant) (dungeon dungeon) (player player))
  (let ((pr-set (get-setting variant :basic-frame-printing))
	(bot-set (get-setting variant :bottom-row-printing)))
	
    (print-cut variant player pr-set)
    (print-poisoned variant player bot-set)
    (print-afraid variant player bot-set)
    (print-confused variant player bot-set)
    ;; more
    (print-speed variant player bot-set)
    ;; more
    (print-can-study-more variant player bot-set)
    t))



(defmethod redraw-stuff ((variant vanilla-variant) (dungeon dungeon) (player player))
  
  (when (= 0 *redraw*) (return-from redraw-stuff nil))

  (let ((retval nil)
	(pr-set nil)
        (bot-set nil))

    (when (bit-flag-set? *redraw* +print-extra+)
      (bit-flag-remove! *redraw* +print-extra+)
      (bit-flag-remove! *redraw* +print-cut+)
      (bit-flag-remove! *redraw* +print-stun+)
      (bit-flag-remove! *redraw* +print-hunger+)
      (bit-flag-remove! *redraw* +print-blind+)
      (bit-flag-remove! *redraw* +print-confused+)
      (bit-flag-remove! *redraw* +print-afraid+)
      (bit-flag-remove! *redraw* +print-poisoned+)
      (bit-flag-remove! *redraw* +print-state+)
      (bit-flag-remove! *redraw* +print-speed+)
      (bit-flag-remove! *redraw* +print-study+)
;;      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))

      (print-extra-frame-content variant dungeon player)
      (setf retval t))

    
    (when (bit-flag-set? *redraw* +print-cut+)
      (bit-flag-remove! *redraw* +print-cut+)
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))
      (print-cut variant player pr-set)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-stun+)
      (bit-flag-remove! *redraw* +print-stun+)
      (unless pr-set (setf pr-set (get-setting variant :basic-frame-printing)))
      (print-stun variant player pr-set)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-hunger+)
      (bit-flag-remove! *redraw* +print-hunger+)
      ;; fix
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-blind+)
      (bit-flag-remove! *redraw* +print-blind+)
      ;; fix
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-confused+)
      (bit-flag-remove! *redraw* +print-confused+)
      (unless bot-set (setf bot-set (get-setting variant :bottom-row-printing)))
      (print-confused variant player bot-set)
      (setf retval t))

    (when (bit-flag-set? *redraw* +print-afraid+)
      (bit-flag-remove! *redraw* +print-afraid+)
      (unless bot-set (setf bot-set (get-setting variant :bottom-row-printing)))
      (print-afraid variant player bot-set)
      (setf retval t))
    

    (when (bit-flag-set? *redraw* +print-poisoned+)
      (bit-flag-remove! *redraw* +print-poisoned+)
      (unless bot-set (setf bot-set (get-setting variant :bottom-row-printing)))
      (print-poisoned variant player bot-set)
      (setf retval t))

    ;; state and speed in engine

    (when (bit-flag-set? *redraw* +print-study+)
      (bit-flag-remove! *redraw* +print-study+)
      (unless bot-set (setf bot-set (get-setting variant :bottom-row-printing)))
      (print-can-study-more variant player bot-set)
      (setf retval t))


    (if (call-next-method)
	t
	retval)
    ))

