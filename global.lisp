;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: global.lisp - globally available functions/classes
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: common langband-specific code of interest for larger
ADD_DESC: parts of the code.  Small classes, functions, et.al

||#

(in-package :org.langband.engine)


(defclass game-values ()
  ((base-ac       :accessor gval.base-ac
		  :initarg :base-ac
		  :initform 0)
   (ac-modifier   :accessor gval.ac-modifier
		  :initarg :ac-modifier
		  :initform 0)
   (base-dice     :accessor gval.base-dice
		  :initarg :base-dice
		  :initform 0)
   (num-dice      :accessor gval.num-dice
		  :initarg :num-dice
		  :initform 0)
   (tohit-modifier :accessor gval.tohit-modifier
		   :initarg :tohit-modifier
		   :initform 0)
   (dmg-modifier  :accessor gval.dmg-modifier
		  :initarg :dmg-modifier
		  :initform 0)
   (mana          :accessor gval.mana
		  :initarg :mana
		  :initform 0)
   (charges       :accessor gval.charges
		  :initarg :charges
		  :initform 0)
   (food-value    :accessor gval.food-value
		  :initarg :food-value
		  :initform 0)
   (light-radius  :accessor gval.light-radius
		  :initarg :light-radius
		  :initform 0)
   (tunnel        :accessor gval.tunnel
		  :initarg :tunnel
		  :initform 0)
   (speed         :accessor gval.speed
		  :initarg :speed
		  :initform 0)
   (skill-modifiers :accessor gval.skill-modifiers
		    :initarg :skill-modifiers
		    :initform '())
   (stat-modifiers :accessor gval.stat-modifiers
		   :initarg :stat-modifiers
		   :initform '())
   (ignores       :accessor gval.ignores
		  :initarg :ignores
		  :initform 0
		  :documentation "The value is tied to registered elements.")
   (resists       :accessor gval.resists
		  :initarg :resists
		  :initform 0
		  :documentation "The value is tied to registered elements.")
   (immunities    :accessor gval.immunities
		  :initarg :immunities
		  :initform 0
		  :documentation "The value is tied to registered elements.")
   (abilities     :accessor gval.abilities
		  :initarg :abilities
		  :initform '())
   (sustains      :accessor gval.sustains
		  :initarg :sustains
		  :initform '())
   (slays         :accessor gval.slays
		  :initarg :slays
		  :initform '())
   )
  
  (:documentation "necessary game-values for an object."))


(defclass attack ()
  ((kind     :accessor attack.kind
	     :initarg :kind
	     :initform nil)
   (dmg-type :accessor attack.dmg-type
	     :initarg :dmg-type
	     :initform nil)
   (damage   :accessor attack.damage
	     :initarg :damage
	     :initform nil))
  (:documentation "Representation for a monster-attack."))
   

(defclass active-object (activatable)
  ((kind        :accessor aobj.kind
		:initarg :obj
		:initform nil)
   (inscription :accessor aobj.inscr
		:initform "")
   (number      :accessor aobj.number
		:initarg :number
		:initform 1)
   (contains    :accessor aobj.contains
		:initarg :contains
		:initform nil)
   (game-values :accessor aobj.game-values
		:initarg :game-values
		:initform nil)
   (events      :accessor aobj.events
		:initarg :events
		:initform nil)
   (loc-x       :accessor location-x
		:initarg :loc-x
		:initform +illegal-loc-x+)
   (loc-y       :accessor location-y
		:initarg :loc-y
		:initform +illegal-loc-y+)
   (identify    :accessor aobj.identify
		:initarg :identify
		:initform 0
		:documentation "Bitfield that says how known the object is.")

   (marked :accessor aobj.marked
	   :initform nil
	   :documentation "boolean whether the object has been marked.")
   
   ))


(defclass active-monster (activatable)
  ((kind    :accessor amon.kind
	    :initarg :kind
	    :initform nil)
   (cur-hp  :accessor current-hp
	    :initarg :hp
	    :initform 0)
   (max-hp  :accessor maximum-hp
	    :initarg :max-hp
	    :initform 0)
   (speed   :accessor get-creature-speed
	    :initarg :speed
	    :initform 0)
   (energy  :accessor get-creature-energy
	    :initarg :energy
	    :initform 0)
   (mana    :accessor get-creature-mana
	    :initarg :mana
	    :initform 0)
       
   (loc-x   :accessor location-x      :initarg :loc-x :initform nil)
   (loc-y   :accessor location-y      :initarg :loc-y :initform nil)
   (alive?  :accessor creature-alive? :initarg :alive? :initform t)
       
   ))


(defstruct (alloc-entry (:conc-name alloc.))
  (obj nil)
  (index nil)
  (depth nil)
  (prob1 nil)
  (prob2 nil)
  (prob3 nil))


(defstruct (dun-data (:conc-name dun-data.))
  (room-centres nil)
  (doors nil)
  (walls nil)
  (tunnels nil)
  (row-rooms nil)
  (col-rooms nil)
  (room-map nil)
  (crowded nil))

(defclass treasure-drop ()
  ((chance  :initarg :chance  :initform 1       :accessor drop.chance)
   (amount  :initarg :amount  :initform 1       :accessor drop.amount) ;; either positive integer or (cons int int)
   (quality :initarg :quality :initform :normal :accessor drop.quality)
   (type    :initarg :type    :initform :any    :accessor drop.type)
   ))

(bt:define-binary-struct (hs-entry (:conc-name hs-entry.)) ()
    
    (version nil) ;; string
    (variant nil) ;; string
    
    (name nil)    ;; string
    (race nil)    ;; string-id
    (class nil)   ;; string-id
    (sex nil)     ;; string
    (cause-of-death nil) ;; string
    
    ;; directly savable
    (xp          0 :bt bt:u32)
    (max-xp      0 :bt bt:u32)
    (level       0 :bt bt:u16)
    (depth       0 :bt bt:u16)
    (max-depth   0 :bt bt:u16)
    (turn        0 :bt bt:u32)
    (gold        0 :bt bt:u32)
    (score       0 :bt bt:u32)  
    
    (date        0 :bt u64) ;; time of death
    )

;; this is a dummy for classes, not objects.. the player will have numbers
(defstruct (skill (:conc-name skill.))
  (name "")
  (base 0)
  (lvl-gain 0));; this is for 10 levels, to allow for fractions


;; move this to variants later
(defclass skills ()
  ((saving-throw :accessor skills.saving-throw  :initform 0)
   (stealth      :accessor skills.stealth       :initform 0)
   (fighting     :accessor skills.fighting      :initform 0)
   (shooting     :accessor skills.shooting      :initform 0)
   (disarming    :accessor skills.disarming     :initform 0)
   (device       :accessor skills.device        :initform 0)
   (perception   :accessor skills.perception    :initform 0)
   (searching    :accessor skills.searching     :initform 0))
  (:documentation "Various skills..")
  ;;    #+cmu
  ;;    (:metaclass pcl::standard-class)
  )


(defmethod produce-game-values-object ((variant variant))
  "Returns an object of type game-values."
  (make-instance 'game-values))

(defmethod copy-game-values ((variant variant) (obj game-values))
  "Copies the given OBJ and returns a new object that is equal."
  
  (let ((new-obj (produce-game-values-object variant)))
    (dolist (i '(base-ac ac-modifier base-dice num-dice tohit-modifier
		 dmg-modifier mana charges food-value light-radius tunnel
		 speed skill-modifiers stat-modifiers ignores resists
		 immunities abilities sustains slays))
      ;; doesn't handle shared-structures well
      (setf (slot-value new-obj i) (slot-value obj i)))
    new-obj))

(defun make-game-values (&key (base-dice :unspec)
			 (num-dice :unspec)
			 (base-ac :unspec)
			 (ac-modifier :unspec)
			 (tohit-modifier :unspec)
			 (dmg-modifier :unspec)
			 (ignores :unspec)
			 (resists :unspec)
			 (sustains :unspec)
			 (stat-modifiers :unspec)
			 (skill-modifiers :unspec)
			 (abilities :unspec)
			 (slays :unspec)
			 (food-value :unspec)
			 (charges :unspec)
			 (light-radius :unspec))
  "This one checks the incoming values exhaustively."
  (let* ((var-obj *variant*)
	 (gval (produce-game-values-object var-obj)))
    
    (cond ((eq ignores :unspec))
	  ((listp ignores)
	   (dolist (i ignores)
	     (bit-flag-add! (gval.ignores gval) (get-element-flag var-obj i))))
	  (t
	   (error "Value of ignores is odd: ~s" ignores)))
    
    (cond ((eq resists :unspec))
	  ((listp resists)
	   (dolist (i resists)
	     (bit-flag-add! (gval.resists gval) (get-element-flag var-obj i))))
	  (t
	   (error "Value of resists is odd: ~s" resists)))

    ;; improve this?
    (cond ((eq stat-modifiers :unspec))
	  ((listp stat-modifiers)
	   (setf (gval.stat-modifiers gval) stat-modifiers))
	  (t
	   (error "Value of stat-modifiers is odd: ~s" stat-modifiers)))
    
    ;; improve this?
    (cond ((eq skill-modifiers :unspec))
	  ((listp skill-modifiers)
	   (setf (gval.skill-modifiers gval) skill-modifiers))
	  (t
	   (error "Value of skill-modifiers is odd: ~s" skill-modifiers)))

    ;; improve this?
    (cond ((eq abilities :unspec))
	  ((listp abilities)
	   (setf (gval.abilities gval) abilities))
	  (t
	   (error "Value of abilities is odd: ~s" abilities)))

    ;; improve this?
    (cond ((eq sustains :unspec))
	  ((listp sustains)
	   (setf (gval.sustains gval) sustains))
	  (t
	   (error "Value of sustains is odd: ~s" sustains)))

    ;; improve this?
    (cond ((eq slays :unspec))
	  ((listp slays)
	   (setf (gval.slays gval) slays))
	  (t
	   (error "Value of slays is odd: ~s" slays)))

    
    (flet ((handle-value (constraint slot value)
	     (cond ((eq value :unspec))
		   ((funcall constraint value)
		    (setf (slot-value gval slot) value))
		   (t
		    (error "Value for ~s is quite odd: ~s" slot value))))
	   (pos-num? (x)
	     (and (integerp x) (>= x 0))))

      (handle-value #'pos-num? 'food-value   food-value)
      (handle-value #'pos-num? 'charges      charges)
      (handle-value #'pos-num? 'light-radius light-radius)
      (handle-value #'pos-num? 'base-dice    base-dice)
      (handle-value #'pos-num? 'num-dice     num-dice)
      (handle-value #'pos-num? 'base-ac      base-ac)
      (handle-value #'integerp 'ac-modifier     ac-modifier)
      (handle-value #'integerp 'dmg-modifier    dmg-modifier)
      (handle-value #'integerp 'tohit-modifier  tohit-modifier)

      )
    
    gval))
     

(defmethod produce-skills-object ((variant variant) &key (default-value 0))
  "Returns a skills object."
  (let ((obj (make-instance 'skills)))
    (unless (and (numberp default-value) (= 0 default-value))
      (let ((skill-list (variant.skill-translations variant)))
	(dolist (i skill-list)
	  (setf (slot-value obj (cdr i)) default-value))))
    obj))

(defmethod register-skill-translation& ((variant variant) translation)
  "Registers a translation (single cons) or a list
of translations."
  (flet ((add-single-translation (translation)
	   (pushnew translation (variant.skill-translations variant)
		    :test #'eql)))
    
    (when (consp translation) 
      (cond ((and (atom (car translation))
		  (atom (cdr translation)))
	     (warn "pushing a single one..")
	     (add-single-translation translation))
	  
	    (t
	     ;; we have a list
	     (dolist (i translation)
	       (assert (and (atom (car i))
			    (atom (cdr i))))
	       (add-single-translation i)))))
    
    translation))
  

(defmethod get-skill-translation ((variant variant) key)
  "Returns a symbol in the appropriate skills-class or nil."
  (let ((search (assoc key (variant.skill-translations variant))))
    (when search
      (cdr search))))


(defmethod build-skills-obj-from-list ((variant variant) skills)
  "Tries to build a skills-obj and include all possible
information from the list skills whose content depends on variant."
  
  (let ((skill-obj (produce-skills-object variant :default-value nil)))

      (when (listp skills)
	(dolist (i skills)
	  (if (not (consp i))
	      (warn "Skill argument ~s must be a list, like: (skill base-val lvl-val)"
		    i)
	      (let* ((skill-sym (first i))
		     (the-name (get-skill-translation variant skill-sym)))
		(if (eq the-name nil)
		    (warn "Unable to find skill-translation from ~s" skill-sym)
		    (setf (slot-value skill-obj the-name)
			  (make-skill :name (string-downcase (string the-name))
				      :base (let ((base-arg (second i)))
					      (if base-arg base-arg 0))
				      :lvl-gain (let ((lvl-arg (third i)))
						  (if lvl-arg lvl-arg 0))))
		    )))))
      skill-obj))

(defun get-armour-desc (variant number)
  "Returns a description of the armour-number."
  (declare (ignore variant))
  
  (cond ((<= number 10)
	 (cons "Unarmoured" +term-l-red+))
	((<= number 20)
	 (cons "Leather/hide armour" +term-white+))
	((<= number 30)
	 (cons "Light metal/bone armour" +term-orange+))
	((<= number 40)
	 (cons "Metal/bone armour" +term-yellow+))
	((<= number 50)
	 (cons "Heavy metal-armour" +term-violet+))
	((<= number 60)
	 (cons "Plated armour" +term-l-green+))
	((<= number 70)
	 (cons "Heavy plated armour" +term-l-red+))
	((<= number 80)
	 (cons "Dragon armour" +term-white+))
	((<= number 100)
	 (cons "Heavy dragon armour" +term-orange+))
	((<= number 130)
	 (cons "Enchanted dragon armour" +term-yellow+))
	((<= number 170)
	 (cons "Legendary magic armour" +term-violet+))
	(t
	 (cons "Mythical power-armour" +term-l-green+))
	))

(defun get-known-combat-skill (variant player)
  (declare (ignore variant))

  (let* ((skills (player.skills player))
	 (combat-skill (skills.fighting skills)))
    ;; fix
    combat-skill))


(defmethod convert-obj ((letter character) (to (eql :colour-code)) &key)
  ;; make this one into an array-access later
  "Returns a code which can be sent to C-functions as colour."
  
  (case letter
    (#\d +term-dark+)
    (#\w +term-white+)
    (#\s +term-slate+)
    (#\o +term-orange+)
    (#\r +term-red+)
    (#\g +term-green+)
    (#\b +term-blue+)
    (#\u +term-umber+)

    (#\D +term-l-dark+)
    (#\W +term-l-white+)
    (#\v +term-violet+)
    (#\y +term-yellow+)
    (#\R +term-l-red+)
    (#\G +term-l-green+)
    (#\B +term-l-blue+)
    (#\U +term-l-umber+)

    (otherwise
     (error "Fell through (CONVERT-OBJ ~s -> :colour-code)" letter)
     #-cmu
     +term-white+)))

(defmethod convert-obj (code (to (eql :letter)) &key)
  "Returns a char for the appropriate colour-code."
  #||
  (warn "compare ~s vs ~s, ~a vs ~a, ~a vs ~a, ~a vs ~a, ~a"
	code +term-dark+
	(type-of code) (type-of +term-dark+) (char-code code) (char-code +term-dark+)
	(eq code +term-dark+) (eql code +term-dark+) (case code (+term-dark+ t) (t nil)))
  ||#

  (cond  ((eql code +term-dark+)    #\d) 
	 ((eql code +term-white+)   #\w)
	 ((eql code +term-slate+)   #\s) 
	 ((eql code +term-orange+)  #\o) 
	 ((eql code +term-red+)     #\r) 
	 ((eql code +term-green+)   #\g) 
	 ((eql code +term-blue+)    #\b) 
	 ((eql code +term-umber+)   #\u) 
	 
	 ((eql code +term-l-dark+)  #\D) 
	 ((eql code +term-l-white+) #\W) 
	 ((eql code +term-violet+)  #\v) 
	 ((eql code +term-yellow+)  #\y) 
	 ((eql code +term-l-red+)   #\R) 
	 ((eql code +term-l-green+) #\G) 
	 ((eql code +term-l-blue+)  #\B) 
	 ((eql code +term-l-umber+) #\U) 

	 (t
	  (error "Fell through (CONVERT-OBJ ~s -> :letter)" (char-code code))
	  #-cmu
	  #\w)))

(let ((screen-lock nil))
  
  (defun screen-save ()
    
    (when screen-lock
      (error "Screen already locked, please fix execution-path."))

    ;; flush
    (c-print-message! +c-null-value+)
    (c-term-save!)
    (setf screen-lock t))
  
  (defun screen-load ()
    (unless screen-lock
      (error "Trying to load a screen, but none is locked."))
    ;; flush
    (c-print-message! +c-null-value+)
    (c-term-load!)
    (setf screen-lock nil)))


(defmacro with-new-screen (arg &body body)
  (declare (ignore arg))
  `(unwind-protect
    (prog2
	(screen-save)
	,@body)
    (screen-load)))

;; move later
(defun get-system-type ()
  (let ((num (c_current_ui)))
    (ecase num
      (0 'x11)
      (1 'gcu)
      (2 'gtk)
      (3 'win)

      )))

(defun define-key-macros (key &rest macros)
  (dolist (i macros)
    (let ((macro (text-to-ascii i)))

;;      (format t "~&~C: " key)
;;      (loop for x across macro do (format t "~a " (char-code x))
;;	    finally (format t "~%"))
;;      (warn "macro ~s" macro)
      #-lispworks
      (org.langband.ffi:c_macro_add& macro (string key))
      #+lispworks
      (fli:with-foreign-string (s a b)
	macro
	(fli:with-foreign-string (st aa bb)
	  (string key)
	  (org.langband.ffi:c_macro_add& s st)))
      ))
;;      (c-macro-add& macro (string key))))
  key)


#-compiler-that-inlines
(defmacro grid (x y)
  `(the fixnum (+ (the fixnum (* 256 ,y)) ,x)))

#+compiler-that-inlines
(defun grid (x y)
  (the fixnum (+ (* 256 y) x)))

#-compiler-that-inlines
(defmacro grid-y (g)
  `(the fixnum (int-/ ,g 256)))

#+compiler-that-inlines
(defun grid-y (g)
  (the fixnum (int-/ g 256)))

#-compiler-that-inlines
(defmacro grid-x (g)
  `(the fixnum (prog1 (mod ,g 256))))

#+compiler-that-inlines
(defun grid-x (g)
  (the fixnum (prog1 (mod g 256))))

(defun game-data-path (fname)
  "Returns a pathname for fname."
  (merge-pathnames (pathname fname) *engine-config-dir*))

(defun load-game-data (fname)
  "Tries to load the data-file fname."
  (load (game-data-path fname)))

;;(trace game-data-path)
;;(trace load-game-data)

(defun read-pref-file (fname)
  "Tries to read a named preference file."
  (load-game-data fname))

(defun loadable-val (val)
  (cond ((or (keywordp val) (stringp val) (characterp val) (numberp val) (arrayp val))
	 val)
	((eq val nil)
	 nil)
	((or (symbolp val) (consp val))
	 (list 'quote val))
	(t
	 (error "Unknown how to make val ~s of type ~s loadable" val (type-of val)))))

;; some wrappers for C-functions.

(defun c-print-message! (str)
;;  (warn "going fu on ~s" (type-of str))
  #-lispworks
  (org.langband.ffi:c_msg_print! (org.langband.ffi:to-arr str))
  #+lispworks
  (fli:with-foreign-string (base-ptr a b)
    str
    (org.langband.ffi:c_msg_print! base-ptr))
  (values))

(defun c-quit! (str)
  #-lispworks
  (org.langband.ffi:c_quit! (org.langband.ffi:to-arr str))
  #+lispworks
  (fli:with-foreign-string (base-ptr a b)
    str
    (org.langband.ffi:c_quit! base-ptr))
  (values))

(defun c-prt! (str row col)
  #-lispworks
  (org.langband.ffi:c_prt! (org.langband.ffi:to-arr str) row col)
  #+lispworks
  (fli:with-foreign-string (base-ptr a b)
    str
    (org.langband.ffi:c_prt! base-ptr row col))
  (values))

(defun c-term-putstr! (col row some colour text)
  #-lispworks
  (org.langband.ffi:c_term_putstr! col row some colour (org.langband.ffi:to-arr text))
  #+lispworks
  (fli:with-foreign-string (base-ptr a b)
    text
    (org.langband.ffi:c_term_putstr! col row some colour base-ptr))
  (values))

(defun c-col-put-str! (colour text row col)
  (c-term-putstr! col row -1 colour text)
  (values))

(defun c-put-str! (text row col)
  (c-term-putstr! col row -1 +term-white+ text))

(defun c-bell! (text)
  #-lispworks
  (org.langband.ffi:c_bell! (org.langband.ffi:to-arr text))
  #+lispworks
  (fli:with-foreign-string (base-ptr a b)
    text
    (org.langband.ffi:c_bell! base-ptr))
  (values))

(defun c-pause-line! (row)
  (c-prt! "" row 0)
  (c-term-putstr! 23 row -1 +term-white+ "[Press any key to continue]")
  (read-one-character)
  (c-prt! "" row 0))

(defun init-c-side& (ui base-path debug-level)
  #-lispworks
  (init_c-side& ui base-path debug-level)
  #+lispworks
  (fli:with-foreign-string (ui-ptr elm-count byte-count :external-format :ascii)
    ui
    (declare (ignore elm-count byte-count))
    (fli:with-foreign-string (base-ptr elem-count bbyte-count :external-format :ascii)
      base-path
      (declare (ignore elem-count bbyte-count))
;;      (warn "Going ahead with ~s ~s" ui-ptr base-ptr)
      (init_c-side& ui-ptr base-ptr debug-level))))

(defun c-print-text! (col row colour text &key (end-col 80))
  "Don't call this if you need non-consing or fast operation."
  
  (let ((startcol col)
;;	(startrow row)
	(cur-col col)
	(cur-row row)
;;	(end-col 80)
	(splitted (mapcar #'(lambda (x) (string-trim '(#\Space #\Tab #\Newline) x))
			  (split-seq-on text #\Space))))

;;    (warn "text ~s -> ~s" text splitted)
    
    (flet ((print-word (word)
	     (let ((word-len (length word)))
;;	     (warn "Printing word ~s at ~s,~s" word cur-col cur-row)
	       (c-col-put-str! colour word cur-row cur-col)
	       (incf cur-col word-len)
	       (c-col-put-str! colour " " cur-row cur-col)
	       (incf cur-col)
	       (1+ word-len))))

      (loop for cur-word in splitted
	    for i from 0
	    do
	    (when (plusp (length cur-word))
	      (cond ((< (+ cur-col (length cur-word) 1) end-col)
		     (print-word cur-word))
		    (t
		     (incf cur-row)
		     (setf cur-col startcol)
		     (print-word cur-word))))))
    ))

(defun quit-game& ()
  "Tries to clean up a few variables."
  (setf *variant* nil
	*level* nil
	*player* nil
	*dungeon* nil)
  (garbage-collect :global t)
  ;;#+boys-eating-their-vegetables
  (finish-output cl:*error-output*)
  (finish-output cl:*standard-output*)
  (finish-output cl:*trace-output*)
  (case (get-system-type)
    ((x11 gcu)
     (cleanup-c-side&)
     (signal (make-condition 'langband-quit)))
    (otherwise
     (format t "~&Thanks for helping to test Langband.~2%")
     (c-quit! +c-null-value+)))
  nil)
 
