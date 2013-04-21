;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: global.lisp - globally available functions/methods
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: common langband-specific code of interest for larger
ADD_DESC: parts of the code.  Small classes, functions, et.al

||#

(in-package :org.langband.engine)


(defmacro with-frame ((num) &body body)
  `(let ((*cur-win* (aref *windows* ,num)))
    ,@body))

(defmacro with-full-frame (() &body body)
  `(unwind-protect
    (progn 
      (switch-to-full-frame&)
      (with-frame (+full-frame+)
	,@body))
    (switch-to-regular-frameset&)))

;; very bulky
(defmacro with-dialogue (() &body body)
  ;; posibly find better solution later
  `(invoke-in-dialogue #'(lambda () ,@body)))

(defun fetch-event (event-obj only-poll)
  (let ((listen-arg (if only-poll 1 0))
	(read-obj nil))
    (loop
     (setf read-obj (c-listen-for-event listen-arg))
     (cond ((plusp read-obj)
	    ;; analyze
	    (cond ((bit-flag-set? read-obj #x01) ;; mouse-event
		   (let ((x (ldb (byte 12 6) read-obj))
			 (y (ldb (byte 12 18) read-obj))
			 (m-ev (input-event.mouseclick event-obj))
			 (button :left))
		     (when (bit-flag-set? read-obj #x02)
		       (setf button :left))
		     (when (bit-flag-set? read-obj #x04)
		       (setf button :right))
		     (when (bit-flag-set? read-obj #x08)
		       (setf button :middle))
		     
		     (setf (input-event.type event-obj) :mouse
			   (mouse-event.x m-ev) x
			   (mouse-event.y m-ev) y
			   (mouse-event.button m-ev) button)
		     (return-from fetch-event event-obj)))
		  
		  ;; keyboard-event
		  (t
		   (let ((key (ldb (byte 16 8) read-obj))
			 (k-ev (input-event.keypress event-obj)))
		     
		     (setf (kbd-event.key k-ev) nil
			   (kbd-event.ctrl k-ev) nil
			   (kbd-event.alt k-ev) nil
			   (kbd-event.shift k-ev) nil) ;; reset

		     
		     (when (bit-flag-set? read-obj #x02)
		       (setf (kbd-event.ctrl k-ev) t))
		     (when (bit-flag-set? read-obj #x04)
		       (setf (kbd-event.alt k-ev) t))
		     (when (bit-flag-set? read-obj #x08)
		       (setf (kbd-event.shift k-ev) t))

		     (when (< 0 key 126)
		       (setf (kbd-event.key k-ev) (code-char key)))
		     ;; hackish,
		     (when (< 272 key 282)
		       (setf (kbd-event.key k-ev) (ecase key
						    (273 #\8)
						    (274 #\2)
						    (275 #\6)
						    (276 #\4)
						    (277 #\0)
						    (278 #\7)
						    (279 #\1)
						    (280 #\9)
						    (281 #\3))))
	    
		     ;; hackish,
		     (when (< 255 key 266)
		       (setf (kbd-event.key k-ev) (ecase key
						    (256 #\0)
						    (257 #\1)
						    (258 #\2)
						    (259 #\3)
						    (260 #\4)
						    (261 #\5)
						    (262 #\6)
						    (263 #\7)
						    (264 #\8)
						    (265 #\9))))

		     (when (kbd-event.key k-ev)
		       (setf (input-event.type event-obj) :key)
		       (return-from fetch-event event-obj))

		     (when (plusp key)
		       (warn "Got back unhandled key ~s" key))
		     ))
		  ))
		  
	   (only-poll
	    (return-from fetch-event nil))
	   
	   (t nil))
     )))

(defun read-one-character ()
  "Reads one character from the C-side."

  (loop	;; we might get a mouse-event!
   (let ((ev (fetch-event *input-event* nil)))
     ;;(warn "got back ~s" ev)
     (when (and ev (eq (input-event.type ev) :key))
       ;; fix
       (return-from read-one-character (kbd-event.key (input-event.keypress ev)))
       ))))


(defmethod convert-obj (obj to &key)
  (error "Conversion from ~s to ~s not implemented." obj to)
  ;;(coerce obj to)
  )

(defmethod activate-object (obj &key)

  obj)

(defmethod activate-object :around ((obj activatable) &key)
   (unless (next-method-p)
     ;; this will never happen
     (lang-warn "Unable to find ACTIVATE-OBJECT for type ~a" (type-of obj))
     (return-from activate-object nil))

   ;; we pass along the same arguments.. 
   (let ((result (call-next-method)))
     ;; we only say that an object is activated if it returned the object
     (cond ((eq obj result)
	    (setf (slot-value obj 'activated) t)
	    obj)
	   
	   (t
	    (lang-warn "Activation of object ~a failed, return was ~a" obj result)
	    nil)
	   )))

(defun has-information? (key &key (variant *variant*))
  "Returns T if there is information for the given key."
  (assert (and (stringp key) (typep variant 'variant)))
  (when (gethash key (variant.information variant))
    t)) ;; a bit superfluous

(defun get-information (key &key (default nil) (variant *variant*))
  "Will just return one value, it's not like GETHASH but takes
same arguments."
  (assert (and (stringp key) (typep variant 'variant)))
  (multiple-value-bind (val f-p)
      (gethash key (variant.information variant) default)
    (if (not f-p)
	default
	val)))

(defun (setf get-information) (value key &key (variant *variant*))
  "Assigns value to an information key."
  (assert (and (stringp key) (typep variant 'variant)))
  (setf (gethash key (variant.information variant)) value)
  value)

(defun remove-information! (key &key (variant *variant*))
  "Returns T if the information existed, NIL if it didn't exist."
  (assert (and (stringp key) (typep variant 'variant)))
  (remhash key (variant.information variant)))

(defun register-information& (&rest args)
  "Registers a group of information on the format:
(register-information key1 info1 key2 info2 ...)"
  (unless (= 0 (mod (length args) 2))
    (warn "Uneven information registration (~s ...)" (car args))
    (return-from register-information& nil))

  (loop for (k v) on args by #'cddr
	do
	(setf (get-information k) v))

  t)

;; expanding function defined later in file.
(defmacro format-message! (format-str &rest args)
  `(print-message! (format nil ,format-str ,@args)))

(defmacro format-note! (format-str &rest args)
  `(print-note! (format nil ,format-str ,@args)))


(defun define-effect (symbol name &key number bit-flag)
  (let ((var-obj *variant*))
    (pushnew (make-instance 'effect :symbol symbol :name name
			    :number number :bit-flag bit-flag)
	     (variant.effects var-obj) :test #'eq :key #'effect.symbol)))

(defun is-legal-effect? (variant effect)
  "Returns T if the given effect is legal."
  (assert (and (symbolp effect) (not (eq nil effect))))
  (if (find effect (variant.effects variant) :test #'eq :key #'effect.symbol)
      t
      nil))


;; see variants/vanilla/config/defines.lisp for examples

(defun define-element (symbol name &key (bit-flag 0) (number 0))
  (let ((var-obj *variant*))
    (pushnew (make-instance 'element :symbol symbol :name name
			    :number number :bit-flag bit-flag)
	     (variant.elements var-obj) :test #'eq :key #'element.symbol)))



(defun is-legal-element? (variant element)
  "Returns T if the given element is legal."
  (assert (and (symbolp element) (not (eq nil element))))
  (if (find element (variant.elements variant) :test #'eq :key #'element.symbol)
      t
      nil))

(defun get-element-flag (variant element)
  "Returns the bit-flag for the given element."
  (check-type variant variant)
  (assert (and (symbolp element) (not (eq nil element))))
  (let ((elm (find element (variant.elements variant)
		   :test #'eq :key #'element.symbol)))
    (if (and elm (typep elm 'element))
	(element.bit-flag elm)
	(error "The element ~s is not registered for variant '~a'"
	       element (variant.name variant)))))

(defun get-element-number (variant element)
  "Returns the numeric index for the given element."
  (check-type variant variant)
  (assert (and (symbolp element) (not (eq nil element))))
  (let ((elm (find element (variant.elements variant)
		   :test #'eq :key #'element.symbol)))
    (if (and elm (typep elm 'element))
	(element.number elm)
	(error "The element ~s is not registered for variant '~a'"
	       element (variant.name variant)))))

(defun get-element-number-from-bit-flag (variant bit-flag)
  "Returns the numeric index for a given bit-flag."
  (check-type variant variant)
  (loop for x in (variant.elements variant)
	do
	(when (= (element.bit-flag x) bit-flag)
	  (return-from get-element-number-from-bit-flag (element.number x))))
  (error "Unable to find element with bit-flag ~s" bit-flag))

;;; == variant-related code

(defmethod initialise-monsters& (variant &key)
  (error "No INIT-MONSTERS for ~s" (type-of variant)))
  
(defmethod initialise-floors& (variant &key)
  (error "No INIT-FLOORS for ~s" (type-of variant)))

(defmethod initialise-objects& (variant &key)
  (error "No INIT-OBJECTS for ~s" (type-of variant)))

(defun is-variant? (obj)
  (and obj (typep obj 'variant)))

;; a small closure
(let ((registered-variants (make-hash-table :test #'equal)))
  
  (defun register-variant& (id var-constructor &key desc)
    "Registers a variant-object."
    
    (check-type var-constructor function)
    (setf (gethash id registered-variants) (list desc var-constructor)))

  (defun get-variant-info (id)
    "Returns variant-info that's tregistered for the given id."
    (gethash id registered-variants))

  (defun get-registered-variants ()
    "Returns a list of ids to registered variants."
    (loop for x being the hash-keys of registered-variants
	  collecting x))
  
  (defun load-variant& (id &key (verbose t))
    "Tries to load a variant."
    (declare (ignore verbose))
    (let ((var-data (gethash id registered-variants))
	  (var-obj nil))
      (cond ((and (consp var-data)
		  (functionp (second var-data)))
	     (format-note! "[Loading '~a' variant, please wait]" id)
	     (setf var-obj (funcall (second var-data))))
	    (t
	     (error "Unable to find variant ~s" id)))
      
      (when (and var-obj (typep var-obj 'variant))
	var-obj))))


;; uses id.. 
(defmethod variant-home-path ((variant variant))
  (variant-home-path (variant.id variant)))

(defmethod variant-home-path ((variant string))
  (concatenate 'string (home-langband-path) variant "/"))

(defmethod variant-data-fname ((var-obj variant) data-fname)
  "Returns a full pathname for data."
  (let ((file-path (variant.config-path var-obj)))
    (if file-path
	(concatenate 'string (lbsys/ensure-dir-name file-path) data-fname)
	data-fname)))

(defmethod variant-save-dir ((var-obj variant))
  (variant-save-dir (variant.id var-obj)))

(defmethod variant-save-dir ((variant string))
  (concatenate 'string (variant-home-path variant) "saves/"))

(defun load-variant-data& (var-obj data-file)
  "Loads variant-data from appropriate directory."

  (let ((fname (variant-data-fname var-obj data-file)))
    (load fname)))

(defmacro attack-effect (arguments &body body)
  (assert (= (length arguments) 4))
  (let ((def `(lambda ,arguments
	       (declare (ignorable ,@arguments))
	       ,@body)))
;;    (warn "Def is ~s" def)
    `(function ,def)))

(defun get-attack-type (key &optional (variant *variant*))
  "Returns a possible attack-type object registered with the given variant object."
  (gethash key (variant.attack-types variant)))


(defun execute-turn-events! (var-obj)
  "Executes any turn-events."
  (let* ((turn (variant.turn var-obj))
	 (turn-table (variant.turn-events var-obj))
	 (turn-ev (gethash turn turn-table)))

    (when turn-ev
      (warn "Executing events ~a" turn-ev)
      (remhash turn turn-table))))

(defun register-turn-event! (var-obj wanted-turn event)
  "Adds a turn-event."

  (push event (gethash wanted-turn (variant.turn-events var-obj))))


;;(defun get-monster-filters (type var-obj)
;;  (gethash type (variant.filters var-obj)))


(defun apply-filters-on-obj (type var-obj obj)
  (let ((filters (gethash type (variant.filters var-obj))))
    (dolist (i filters)
      (funcall (cdr i) var-obj obj))))


(defun get-level-builder (id &optional (var-obj *variant*))
  "Returns a level-builder or nil."
  (assert (or (symbolp id) (stringp id)))
  (let ((table (variant.level-builders var-obj))
	(key (if (symbolp id) (symbol-name id) id)))
    (gethash key table)))

(defun register-level-builder! (id builder &optional (var-obj *variant*))
  "Registers a level-builder which must be a function."
  (assert (or (symbolp id) (stringp id)))
  (assert (functionp builder))

  (let ((table (variant.level-builders var-obj))
	(key (if (symbolp id) (symbol-name id) id)))
    (setf (gethash key table) builder)))

(defun make-gender (&key id symbol name win-title)
  (make-instance 'gender :id id :symbol symbol
		 :name name :win-title win-title))

(defmethod get-gender ((variant variant) (key string))
  (find key (variant.genders variant) :key #'gender.id :test #'equal))

(defmethod get-gender ((variant variant) (key symbol))
  (find key (variant.genders variant) :key #'gender.symbol :test #'eq))

;; move this one somewhere else?

(defun make-birth-settings (&key allow-all-classes)
  "Returns a birth-settings object."
  (let ((settings (make-instance 'birth-settings :name "Birth settings")))
    (when allow-all-classes
      (setf (birth.allow-classes settings) t))
    settings))

;; these two needed?
(defmethod trigger-event ((obj settings) event arg-list)
  "trigger events registered for the settings."
  (apply-event event (setting.events obj) arg-list))

(defmethod register-object-event! ((obj settings) event)
  (push event (setting.events obj)))

(defmethod get-setting ((variant variant) key)
  (gethash key (variant.settings variant)))

(defmethod (setf get-setting) (setting (variant variant) key)
  (when setting
    (unless (keywordp key)
      (warn "Registered setting without a keyword as key [~s]"
	    key))
    (setf (gethash key (variant.settings variant)) setting)))

(defun setting-value (settings-obj slot default)
  "Returns a setting-value when setting and slot is found, otherwise
returns default value."
  (let ((retval default))

    (when (and settings-obj (typep settings-obj 'settings))
      (ignore-errors
	(setf retval (slot-value settings-obj slot))))

    retval))

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
	  ((and (integerp ignores) (<= 0 ignores))
	   (setf (gval.ignores gval) ignores))
	  ((listp ignores)
	   (dolist (i ignores)
	     (bit-flag-add! (gval.ignores gval) (get-element-flag var-obj i))))
	  (t
	   (error "Value of ignores is odd: ~s" ignores)))
    
    (cond ((eq resists :unspec))
	  ((and (integerp resists) (<= 0 resists))
	   (setf (gval.resists gval) resists))
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

    (unless (consp skills)
      (return-from build-skills-obj-from-list skill-obj))
    
    (dolist (i skills)
      (if (not (consp i))
	  (warn "Skill argument ~s must be a list: (skill base-val lvl-val)"
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
		))))
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


(defmethod convert-obj ((htbl hash-table) (to (eql :vector))
			&key sort-table-p sorted-by-key
			sorted-by-fun fill-pointer)
  "Takes a hash-table and returns a vector with the elements."
  
  (let* ((len (hash-table-count htbl))
	 (arr (if fill-pointer
		  (make-array len :initial-element nil :fill-pointer t)
		  (make-array len :initial-element nil))))
	 
    (declare (type u-fixnum len))
    
    (loop for i of-type u-fixnum from 0
	  for x being the hash-values of htbl
	  do
	  (setf (aref arr i) x))
    
    (when sort-table-p
      (let ((sort-args (list arr (if sorted-by-fun sorted-by-fun #'<))))
	(when sorted-by-key
	  (setq sort-args (append sort-args (list :key sorted-by-key))))
	(setq arr (apply #'sort sort-args))))
    
    arr))

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

(defmethod convert-obj (code (to (eql :text-attr)) &key)
  "Returns a usable attr-code for the given obj."
  (etypecase code
    (number code)
    (character (char-code code))))

(defmethod convert-obj (code (to (eql :x-attr)) &key)
  "Returns a usable attr-code for the given obj."
  (etypecase code
    (number code)
    (character (char-code code))))

(defmethod convert-obj (code (to (eql :text-char)) &key)
  "Returns a usable char-code for the given obj."
  (etypecase code
    (number code)
    (character (char-code code))))

(defmethod convert-obj (code (to (eql :x-char)) &key)
  "Returns a usable char-code for the given obj."
  (etypecase code
    (number code)
    (character (char-code code))))



;; move later
(defun get-system-type ()
  (let ((num (c_current_ui)))
    (ecase num
      (0 'x11)
      (1 'gcu)
      (2 'gtk)
      (3 'win)
      (4 'sdl)
      )))


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


(defun read-pref-file (fname)
  "Tries to read a named preference file."
  (load-game-data fname))

(defun loadable-val (val)
  (cond ((or (keywordp val) (stringp val) (characterp val)
	     (numberp val) (arrayp val))
	 val)
	((eq val nil)
	 nil)
	((or (symbolp val) (consp val))
	 (list 'quote val))
	(t
	 (error "Unknown how to make val ~s of type ~s loadable" val (type-of val)))))

;; some wrappers for C-functions.

(defun %flush-messages! (x-pos &optional (forced nil))
  "Do not use unless you know what you're doing."
  (with-frame (+message-frame+)
    (output-string! *cur-win* x-pos 0 +term-l-blue+ "-more-")
    
    (unless forced
      (block input-loop
	(loop
	 (read-one-character)
	 (refresh-window +message-frame+)
	 ;; see util.c for real code.. just skip now
	 (return-from input-loop t))))
    
    (clear-row *cur-win* 0 0)))


;; this code is simpler than in C, but does the same job basically
(let ((msg-col 0)
      (end-col 72)
      (messages '())) ;; make into a limited and circular sequence
  
  (defun print-message! (msg &optional (attr +term-white+))
    "If msg is nil, things are flushed."
    ;;  (warn "going fu on ~s" (type-of str))

    (let ((msg-len (if msg (length msg) 0)))
    
      (when (and (> msg-col 0)
		 (or (eq msg nil)
		     (> (+ msg-len msg-col) end-col)))
	(%flush-messages! msg-col)
	;; skip msg-flag
	(setf msg-col 0))

      (unless msg
	(return-from print-message! nil))

      (when (> msg-len 500) ;; panic
	(return-from print-message! nil))

      (push (make-message :text msg :attr attr)
	    messages)
      ;; skip msg-add
      (with-frame (+message-frame+)
	(output-string! *cur-win* msg-col 0 attr msg))
      (refresh-window +message-frame+)
      (incf msg-col (1+ msg-len))
      
      t))
  
  (defun flush-messages! (&optional (forced nil))
    ;; add reset-later
    (when (plusp msg-col)
      (%flush-messages! msg-col forced)
      ;; skip reset of msg-flag
      (setf msg-col 0))
    ;; we also fake a check of the length here
    (setf end-col (- (get-frame-width +message-frame+) 8)))

  (defun show-messages (&key (offset 0))
    (declare (ignore offset))
    (clear-window *cur-win*)
    (let ((last-line (get-last-console-line)))
      ;;(warn "last line is ~s" last-line)
      (loop for i from 0 to (- last-line 2)
	    for msg in messages
	    do
	    ;;(warn "Value ~s" msg)
	    (put-coloured-str! (message.attr msg) (message.text msg) 1 (1+ i))
	    ))
    (pause-last-line!))

  (defun get-messages ()
    messages)
  )

(defun print-note! (msg &optional (attr +term-white+) &key (row -1))
  "Prints a centred note on the last line."

  (let* ((win *cur-win*)
	 (row (if (or (< row 0) (< (window.height win) row))
		 (get-last-console-line)
		 row))
	;;(row 23) ;;(get-last-console-line))
	;; screws up somewhat when graphics is enabled
	(col (- (int-/ (get-last-console-column win) 2)
		(int-/ (length msg) 2))))

    (clear-row win 0 row)
    (put-coloured-str! attr msg col row)
    ;;(c-term-flush!)
    (refresh-window win)
    ;;(pause-last-line!)
    t))


(defun pause-at-line! (row &key msg attr)
  (unless msg
    (setf msg "[Press any key to continue]"))
  (unless attr
    (setf attr +term-white+))
  (print-note! msg attr :row row)
  
  (let ((read-char (read-one-character)))
    (clear-row *cur-win* 0 row)
    read-char))


(defun get-last-console-line (&optional (term -1))
  (1- (get-frame-height term)))

(defun get-last-console-column (&optional (term -1))
  (1- (get-frame-width term)))


(defun pause-last-line! (&key msg attr)
  (pause-at-line! (get-last-console-line) :msg msg :attr attr))


(defun paint-gfx-image& (fname x y)
  (let ((idx (find-image *variant* fname)))
    (unless idx ;; try to load then
      (setf idx (load-image& *variant* fname -1 0)))
    (when (and idx (<= 0 idx))
      (paint-gfx-image *cur-win* idx x y))))

(defun paint-gfx-image (window idx x y &optional (layer +foreground+))
;;  (warn "Paint ~s ~s to ~s" idx (aref (variant.images *variant*) idx) window)

  (let* ((tile-wid 8) ;; hack
	 (tile-hgt 16) ;; hack
	 (img-wid (lb-ffi:c-get-image-width idx))
	 (img-hgt (lb-ffi:c-get-image-height idx))
	 (wid (int-/ img-wid tile-wid))
	 (hgt (int-/ img-hgt tile-hgt)))

;;    (when (= 0 (mod img-wid tile-wid))
;;      (incf wid))

;;    (warn "Paint ~d (~d,~d,~d,~d)" idx x y (+ x wid) (+ y hgt))
    
    (loop for j from 0 below hgt
	  do
	  (loop for i from 0 below wid
		do
		(let ((val (tile-paint-value idx (+ i (* j wid)))))
		  ;;(warn "~d at (~d,~d)" (+ i (* j wid)) (+ i x) (+ y j))
		  (setf (window-coord window layer (+ i x) (+ y j)) val)
		  ;; paint to (x+i, y+j) and paint tile-num (i + j*wid)
		  ;;(incf cnt)
		  )
		))
    ;; fresh coord
    (refresh-window window +winflag-delay-paint+)
    ))

(defun fill-area (window img tile-num x1 y1 x2 y2)
  "Paints tile number TILE-NUM from given image IMG, to the rectangle (x1,y1,x2,y2)
in window WINDOW."
  
  (let ((val (tile-paint-value img tile-num)))
    (loop for x from x1 below x2
	  do
	  (loop for y from y1 below y2
		do
		(setf (window-coord window +foreground+ x y) val)))))

(defun load-scaled-image& (fname idx wid hgt)
  (declare (ignore idx wid hgt))
  (warn "Scale-load image ~s" fname))


(defun find-image (variant fname)
  "Tries to find index for an image, NIL when there is none."
  (when variant
    (loop for i from 0
	  for x across (variant.images variant)
	  do
	  (when (equal fname x)
	    (return-from find-image i))))
  nil)

(defun load-image& (variant fname idx transcolour)
  "Tries to load the named image in given idx spot."

  (flet ((load-op (fname idx tr)
           (org.langband.ffi:load-gfx-image& fname idx tr)))
               

    (block negative-idx
      (when (minusp idx)
	(check-type variant variant)
	(let ((image-table (variant.images variant)))
	  (loop for i from 20 below (length image-table)
		for val = (aref image-table i)
		do
		(when (eq val nil)
		  (setf idx i)
		  (return-from negative-idx i)))
	  (error "Unable to find available space for image in image-table."))))

    (let ((trans (if (plusp transcolour)
		     (1+ transcolour)
		     0)))
      (load-op (concatenate 'string *engine-graphics-dir* fname)
	       idx
	       trans))
    
    (when variant
      (register-image& variant fname idx))
    
    idx))

(defun delay (msecs)
  "Delays given amount of msecs."
  (sleep (/ msecs 1000.0)))

(defun print-text! (col row colour text &key (end-col 80))
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
	       (put-coloured-str! colour word cur-col cur-row)
	       (incf cur-col word-len)
	       (put-coloured-str! colour " " cur-col cur-row)
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
		     (print-word cur-word))))
	    finally (return cur-row)))))


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
    ((x11 gcu sdl)
     (cleanup-c-side&)
     (signal (make-condition 'langband-quit)))
    (otherwise
     (format t "~&Thanks for helping to test Langband.~2%")
     (signal (make-condition 'langband-quit))
     ))
  nil)
 
;;; === Code related to floors

(defun get-floor-type (id &key (variant *variant*))
  "Returns an object of type FLOOR-TYPE or NIL."
  ;; hack
  (when (typep id 'floor-type)
    (return-from get-floor-type id))
  
  (let ((table (variant.floor-types variant)))
    (gethash id table)))

(defun (setf get-floor-type) (floor id)
  "Adds a floor-type with given id to the appropriate table."
  (let ((table (variant.floor-types *variant*)))
    (setf (gethash id table) floor)))

(defun define-floor-type (id name x-attr x-char &key mimic num-idx text-attr text-char flags)
  "Defines a floor-type and registers it.  The floor is returned."

  (unless (or (verify-id id)
	      (integerp id)) ;; remove integer part later
    (warn "floor-id ~s not valid" id)
    (return-from define-floor-type nil))

  (let ((ftype (make-instance 'floor-type :id id
			      :name name
	
			      :mimic mimic)))

    (handle-gfx-visual ftype x-attr x-char)
    (handle-text-visual ftype text-attr text-char)

    (when (integerp num-idx)
      (setf (floor.num-idx ftype) num-idx))
    
    (when (integerp flags)
      (setf (floor.flags ftype) flags))
    
    (setf (get-floor-type id) ftype)
    
    (when (integerp (floor.num-idx ftype))
      (setf (get-floor-type (floor.num-idx ftype)) ftype))
    
    ftype))

(defun define-floor-type* (id name &key x-attr x-char mimic num-idx text-attr text-char flags)
  (define-floor-type id name x-attr x-char :mimic mimic :num-idx num-idx
		     :text-attr text-attr :text-char text-char :flags flags))

;;; === end floor code

;;; == Event related code

(defun is-event? (obj)
  (typep obj 'l-event))

(defun register-event& (id event &key (variant *variant*))
  "Registers an event-id and connects it to a function."
  (unless (equal id (event.id event))
    (warn "registration id ~s of event ~s aren't equal")) 
  (let ((key (if (symbolp id) (symbol-name id) id))
	(table (variant.event-types variant)))
    (setf (gethash key table) event)))

(defun find-event-for-key (id &key (variant *variant*))
  "Tries to find an event for the given id."
  (let ((key (if (symbolp id) (symbol-name id) id))
	(table (variant.event-types variant)))
    (gethash key table)))

(defun make-event (id type function &key (state nil) (return-action :remove-event))
  "Returns an event-object that can be used."
  (check-type return-action return-actions)
  (check-type type event-types)
  (make-instance 'l-event :id id :type type :function function :state state
		 :return return-action))

(defun make-coord-event (id function extra)
  (make-event id :step-on-coord function
	      :state (if (listp extra)
			 extra
			 (list extra))))
 

(defun define-normal-event (dummy-arg id type function &key (variant *variant*))
  "establishes an event basically."
  (declare (ignore dummy-arg))
  
  (let ((the-event (make-event id type function)))
    (register-event& id the-event :variant variant)
    the-event))

(defmethod trigger-event (obj event arg-list)
  (declare (ignore obj event arg-list))
  (values))

(defun apply-event (event-type event-list arg-list)
  "Iterates through event-list and funcalls any events
with given arg-list if any events match."
  (dolist (i event-list)
    (when (eq event-type (event.type i))
      (apply (event.function i) (event.state i) arg-list)
      )))

(defun get-legal-events (event-list)
  "Goes through the list and ensures that all events are
legal, and if they're not they will be replaced with a legal-event
or removed.  Conses up a new list."
  (let ((new-list nil))
    (dolist (i event-list)
      (cond ((typep i 'l-event)
	     (push i new-list))
	    ((or (symbolp i) (stringp i))
	     (let ((find-attempt (find-event-for-key i)))
	       (if (and find-attempt (typep find-attempt 'l-event))
		   (push find-attempt new-list)
		   (warn "Unable to find an event for key ~s" i))))

	    (t
	     (warn "Do not know how to handle possible event ~s" i))))
    (nreverse new-list)))

;;; End event-code

;;; == stat-related code

(defun define-character-stat (symbol name &key abbreviation
			      number data positive-desc negative-desc)
  "Defines and registers a stat with the current variant."

  (let ((the-stat (make-instance 'character-stat :symbol symbol :name name))
	(variant *variant*))
    
    (check-type variant variant)

    (cond ((and number (integerp number) (>= number 0))
	   (setf (stat.number the-stat) number))
	  (t
	   (error "Unknown number ~s for stat ~s" number symbol)))
    
    (when abbreviation
      (setf (stat.abbreviation the-stat) abbreviation))

    (when (stringp positive-desc)
      (setf (stat.positive-desc the-stat) positive-desc))
    
    (when (stringp negative-desc)
      (setf (stat.negative-desc the-stat) negative-desc))
    
    (when (consp data)
      (setf (stat.data the-stat) data)
      ;; let's hack things better

      (let ((field-list '()))
	(dolist (list data)
	  (push (make-stat-field :lower (car list) :upper (cadr list)
				 :data (loop for (first second) on (cddr list) by #'cddr
					     collect (cons first second)))
		field-list))
	(setf (stat.fields the-stat) (nreverse field-list))
;;	(warn "Fields: ~s" (stat.fields the-stat))
	))

      

    ;; now let's add it

    (pushnew the-stat (variant.stats variant) :test #'eq :key #'stat.symbol)

    (setf (variant.stats variant) (stable-sort (variant.stats variant) #'< :key #'stat.number)) 
    
    the-stat))

(defmethod make-stat-array ((variant variant))
  (make-array (variant.stat-length variant) :initial-element 0))

(defmethod is-stat-array? ((variant variant) obj)
  (and (arrayp obj)
       (= (length obj) (variant.stat-length variant))
       ;; add more?
       ))

;;; The stat-functions below should be checked and possible be improved
;;; now that there is a class/object and not just random tables

(defun get-stat-obj (variant key)
  (let ((stats (variant.stats variant)))
    (etypecase key
      (number (aref stats key))
      (symbol (find key stats :key #'stat.symbol))
      )))

(defun get-stat-name-from-num (num)
  "Improve later.."
  (let* ((variant *variant*)
	 (stat-obj (elt (variant.stats variant) num)))
    
    (check-type stat-obj character-stat)
    
    (stat.abbreviation stat-obj)))

(defun get-stat-name-from-sym (sym)
  "Improve later.."
  (let* ((variant *variant*)
	 (stat-obj (find sym (variant.stats variant) :key #'stat.symbol)))
    
    (check-type stat-obj character-stat)
    
    (stat.abbreviation stat-obj)))

(defun get-stat-num-from-sym (sym)
  "Improve later.."
  (let* ((variant *variant*)
	 (stat-obj (find sym (variant.stats variant) :key #'stat.symbol)))
    
    (check-type stat-obj character-stat)
    
    (stat.number stat-obj)))


(defun gsdfn (table num)
  (svref table num))

(defun build-stat-table-from-symlist (variant symlist)
;;  (warn "Building stat-table of ~s" symlist)
  (let ((table (make-stat-array variant)))
    (dolist (i symlist)
      (setf (svref table (get-stat-num-from-sym (car i)))
	    (cadr i)))
    table))

(defmethod x-attr ((obj active-trap))
  (x-attr (decor.type obj)))

(defmethod x-char ((obj active-trap))
  (x-char (decor.type obj)))

(defmethod gfx-sym ((obj active-trap))
  (gfx-sym (decor.type obj)))

(defmethod text-attr ((obj active-trap))
  (text-attr (decor.type obj)))

(defmethod text-char ((obj active-trap))
  (text-char (decor.type obj)))

(defmethod text-sym ((obj active-trap))
  (text-sym (decor.type obj)))

(defun define-trap-type (id name &key x-char x-attr text-char text-attr
			 effect min-depth max-depth rarity)
  "Defines and registers a trap-type."
  (unless (verify-id id)
    (warn "Trap-id ~s not valid" id)
    (return-from define-trap-type nil))
  
  (let ((trap-obj (make-instance 'trap-type :id id :name name
				 :min-depth min-depth :max-depth max-depth
				 :rarity rarity
				 ))
	(table (variant.traps *variant*)))

    (handle-gfx-visual trap-obj x-attr x-char)
    (handle-text-visual trap-obj text-attr text-char)

    
;;    (warn "Effect is ~s ~s ~s" effect (functionp effect) (compiled-function-p effect))
    (when (functionp effect)
      ;; we assume it is not compiled
      (setf effect (compile nil effect))
      (when (functionp effect)
	(setf (trap.effect trap-obj) effect)))
    
    (setf (gethash id table) trap-obj)
    
    trap-obj))

(defmacro trap-effect (arguments &body body)
  (assert (= (length arguments) 4))
  (let ((def `(lambda ,arguments
	       (declare (ignorable ,@arguments))
	       ,@body)))
;;    (warn "Def is ~s" def)
    `(function ,def)))

(defun c-has-frame? (key type)
  (if (= (org.langband.ffi:c-has_frame key type) 1)
      t
      nil))

(defun c-get-frame-gfx-tiles? (key type)
  (if (= (org.langband.ffi:c-get_frame-gfx-tiles key type) 1)
      t
      nil))


(defun update-term-sizes! ()
  (dotimes (i +predefined-frames+)
    (let ((var (aref *windows* i)))
      (unless var
	;; check if there should be one
	(when (c-has-frame? i +frametype-predefined+)
	  (warn "Must create a dummy window, lacking a themed version.")
	  (setf var (make-instance 'window :num-id i))))
      
      ;; ok, we have a frame we need more info about
      (when var
	;; we need to update our info
	(setf (window.width var)       (c-get-frame-columns i +frametype-predefined+)
	      (window.height var)      (c-get-frame-rows i +frametype-predefined+)
	      (window.tile-width var)  (c-get-frame-tile-width i +frametype-predefined+)
	      (window.tile-height var) (c-get-frame-tile-height i +frametype-predefined+)
	      (window.gfx-tiles? var)  (c-get-frame-gfx-tiles? i +frametype-predefined+))

	(assert (plusp (window.width var)))
	(assert (plusp (window.height var)))
	#+never
	(warn "window ~s has size [~d,~d,~d,~d] and gfx ~s" i
	      (window.width var) (window.height var)
	      (window.tile-width var) (window.tile-height var)
	      (window.gfx-tiles? var))

	(setf (aref *windows* i) var))
      )))


(defun get-frame-width (&optional (term -1))
  (cond ((typep term 'window)
	 (window.width term))
	((>= term 0)
	 (window.width (aref *windows* term)))
	(t
	 (window.width *cur-win*))))


(defun get-frame-height (&optional (term -1))
  (cond ((typep term 'window)
	 (window.width term))
	((>= term 0)
	 (window.height (aref *windows* term)))
	(t
	 (window.height *cur-win*))))

; bah!
(defun load-gfx-tiles? ()
  (eq (get-system-type) 'sdl))

;; hackish!
(defun use-gfx-tiles? (&optional (term -1))
  (if (>= term 0)
      (window.gfx-tiles? (aref *windows* term))
      ;; bad style
      (window.gfx-tiles? *cur-win*)))


(defun graphical-map? ()
  (use-gfx-tiles? *map-frame*))

(defun use-images? ()
  (if (eq (get-system-type) 'sdl)
      t
      nil))

(defun get-aim-direction ()
  "Interactive!"
  (flush-messages! t)
  (flet ((read-loop ()
	   (loop
	    (put-coloured-line! +term-white+ "Direction: " 0 0)
	    (let ((val (read-one-character)))
	      (cond ((or (eql val #\.)
			 (eql val #\0)
			 (eql val #\t))
		     (put-coloured-line! +term-white+ "" 0 0)
		     (return-from read-loop 5))
		    ((digit-char-p val)
		     (put-coloured-line! +term-white+ "" 0 0)
		     (return-from read-loop (digit-char-p val)))
		    ((eql val +escape+)
		     (put-coloured-line! +term-white+ "" 0 0)
		     (return-from read-loop nil))
		    (t
		     (put-coloured-line! +term-white+ "Unknown direction!" 0 0)))
	      ))))
    
  (with-frame (+query-frame+)
    (let ((retval (read-loop)))
      (put-coloured-line! +term-white+ "" 0 0)
      retval))))
  
;;; to have empty place-holders
#-image-support
(defun load-scaled-image& (fname idx wid hgt)
  (declare (ignore fname idx wid hgt))
  nil)

(defun image-exists? (name)
  (let ((fname (concatenate 'string *engine-graphics-dir* name)))
    (probe-file fname)))

(defun put-coloured-line! (colour text col row)
  "Erases rest of line and puts a string."
    (clear-row *cur-win* col row)
    (unless (or (eq text nil) (and (stringp text) (= (length text) 0)))
      (output-string! *cur-win* col row colour text)))

;; needs to print empty stuff?
(defun put-coloured-str! (colour text col row)
  (output-string! *cur-win* col row colour text)
  (values))

(defun output-string! (win col row colour text)
  ;;(warn "Writing text ~s to %d,%d in ~s" text col row (window.id win))
  ;; this is _slow_
  (assert (integerp colour))
  (let ((flag +winflag-delay-paint+))
    (loop for x across text
	  for i from col
	  do
	  (let ((pval (if (< colour 256)
			  (text-paint-value colour x)
			  (logior colour (if (characterp x) (char-code x) x))
			  )))
	    (setf (window-coord win +foreground+ i row) pval)
	    (paint-coord win i row flag)))
    ;;(warn "Flush ~s ~s ~s ~s" col row (length text) 1)
    (flush-coords win col row (length text) 1)
    ))


(defun texture-background! (win fname alpha)
  (declare (ignore alpha))
  (let ((idx nil)
	(var *variant*)
	(num-win (if (integerp win) win (window.num-id win)))
	(window (if (integerp win) (aref *windows* win) win)))
    
    (when (and (stringp fname) (plusp (length fname)))
      (setf idx (find-image var fname))
      (when (or (eq idx nil) (minusp idx))
	(setf idx (load-image& var fname -1 0))
	(when (minusp idx)
	  (setf idx nil))))

    ;;(warn "Assigning background ~s ~s to window ~s" fname idx (window.id window))
    (setf (window.background window) idx)
    ;; c-side needs negative value for bad values
    (lb-ffi::c-add-frame-bg! num-win (if (eq idx nil) -1 idx))))


(defun switch-to-full-frame& ()
  (loop for i from 1 below +max-frames+
	do
	(deactivate-window i))
  (activate-window +full-frame+)
  (paint-window +full-frame+))

;; fix to no-cons later
(defun switch-to-regular-frameset& ()
  (deactivate-window +full-frame+)
  (dolist (i (list +message-frame+ +charinfo-frame+
		   +misc-frame+ *map-frame* +inv-frame+))
    (activate-window i)
    (paint-window i)))

(defun invoke-in-dialogue (fun)
  (cond ((eql (window.num-id *cur-win*) +dialogue-frame+)
	 (funcall fun))
	(t
	 (unwind-protect
	      (progn
		(deactivate-window +charinfo-frame+)
		(deactivate-window *map-frame*)
		(activate-window +dialogue-frame+)
		(paint-window +dialogue-frame+)
		(with-frame (+dialogue-frame+)
		  (funcall fun)))

	   (progn
	     (clear-window +dialogue-frame+)
	     (deactivate-window +dialogue-frame+)
	     (activate-window +charinfo-frame+)
	     (activate-window *map-frame*)
	     (paint-window (aref *windows* +charinfo-frame+))
	     (paint-window (aref *windows* *map-frame*))
	     )))
	))

(defun handle-gfx-visual (obj x-attr x-char)
  "Handles/sets gfx-visuals for the given object.  Uses x-attr and x-char,
and if legal assigns their values to the object.  Also sets gfx-sym based
on these two."
  (cond ((and x-attr x-char)
	 ;; strict demands
	 (check-type x-attr integer)
	 (check-type x-char integer)
	 (assert (or (>= x-char (expt 2 8))
		     (= x-char 0)))
	 (assert (or (>= x-attr (expt 2 16))
		     (and (= x-char 0)
			  (= x-attr 0))))
	 
	 (setf (x-char obj) x-char
	       (x-attr obj) x-attr
	       (gfx-sym obj) (logior x-attr x-char)))
	  ((or x-attr x-char)
	   (error "Only one of x-char and x-char provided for visualisable object ~s, need both."
		  obj))
	  (t nil)))


(defun handle-text-visual (obj text-attr text-char)
  "Handles/sets text-visuals for the given object.  Uses text-attr and text-char,
and if legal assigns their values to the object.  Also sets text-sym based
on these two."
    (cond ((and text-attr text-char)
	   (assert (or (characterp text-attr)
		       (and (integerp text-attr)
			    (<= 0 text-attr +term-l-umber+))))
	   (check-type text-char character)

	   (setf (text-attr obj) (make-legal-attr (if (characterp text-attr)
						      (convert-obj text-attr :colour-code)
						      text-attr))
		 (text-char obj) (char-code text-char))
	   (check-type (text-attr obj) integer)
	   (check-type (text-char obj) integer)
	   
	   (assert (< (text-char obj) (expt 2 8)))
	   (assert (or (and (< (text-attr obj) (expt 2 16))
			    (>= (text-attr obj) (expt 2 8)))
		       (= (text-attr obj) 0)))
	   (setf (text-sym obj) (logior (text-attr obj) (text-char obj)))
	   
	   )
	  ((or text-attr text-char)
	   (error "Only one of text-char and text-attr provided for visualisable object ~s, need both."
		  obj))
	  (t nil)))


(defvar *key-operations* (make-hash-table :test #'equal))

(defun define-key-operation (key operation)
  "defines a key-operation which later can be bound."
  ;; to trigger warnings early
;;  (when (functionp operation)
;;    (setf operation (compile nil operation)))
  (setf (gethash key *key-operations*) operation))

(defun find-key-operation (key)
  "returns an operation or NIL."
  (gethash key *key-operations*))

(defun get-key-operations ()
  "Returns an alist of keys and their operations."

  (let ((collected nil))
    (maphash #'(lambda (k v)
		 (push (cons k v) collected))
	     *key-operations*)
    (nreverse collected)))

(defun define-key-table (name)
  "Returns a key-table."
  (declare (ignore name))
  (make-hash-table :test #'eql))

(defun make-inner-key-table ()
  (make-hash-table :test #'equal))


(defvar *current-key-table* nil)
(defvar *ang-keys* (define-key-table "angband"))

(defun define-keypress (key-table where key operation)
  "Defines a keypress and ties it to the appropriate
operation."

  (let ((table (gethash where key-table))
	(oper (find-key-operation operation)))

    (unless oper
      (warn "Unable to find operation '~a' in ~a for key '~a'"
	    operation (cons key-table where) key)
      #-cmu
      (return-from define-keypress nil))
    
    (unless table
      (setf table (make-inner-key-table))
      (setf (gethash where key-table) table))
    
    ;; would be faster to use oper directly, but when using operation it's easier
    ;; to update a key-operation without updating key-entries. 
    (setf (gethash key table) operation) 
    
    ))
    

(defun check-keypress (table key)
  "checks a keypress vs the given table"
  (let ((wanted-sym (gethash key table))
	(poss-fun nil))
    (check-type wanted-sym symbol)
    (setf poss-fun (find-key-operation wanted-sym))
    (assert (or (eq poss-fun nil) (typep poss-fun 'function)))
    poss-fun))



(defun register-image& (variant fname idx)
  "Registers an image with a variant."
  (setf (aref (variant.images variant) idx) fname))


(defun %print-imagelist ()
  (loop for i from 0
	for x across (variant.images *variant*)
	do
	(format t "~&~d. ~a~%" i x)))

(defun set-cursor-visibility (arg)
  "Should the cursor be visible?"
  arg)

(defun set-cursor-to (win cursor x y)
  "Sets the specified cursor to x,y in the given window.  The
cursor argument specifies the type of cursor.  Types are e.g
:input and :crosshair."
  ;;(warn "Setting ~s cursor to ~s,~s" cursor x y)
  (let ((painted nil))
    (when (integerp win)
      (setf win (aref *windows* win)))
    
    (case cursor
      (:bad-crosshair
       (setf (window-coord win +effect+ x y) (tile-paint-value 40 0)
	     painted t))
      (:legal-crosshair
       (setf (window-coord win +effect+ x y) (tile-paint-value 40 1)
	     painted t))
      (otherwise nil))

    (when painted
      (paint-coord win x y)
      (flush-coords win x y 1 1))
    
  
    cursor))

;; haven't I implemented this somewhere else????
(defun get-direction-from-diff (diff-x diff-y)
  (cond ((plusp diff-x)
	 (cond ((plusp diff-y) 3)
	       ((minusp diff-y) 9)
	       (t 6)))
	((minusp diff-x)
	 (cond ((plusp diff-y) 1)
	       ((minusp diff-y) 7)
	       (t 4)))
	(t
	 (cond ((plusp diff-y) 2)
	       ((minusp diff-y) 8)
	       (t 5)))))
		
(defun define-visual-projectile (id &key gfx-path text-path) ;; add rest later
  (assert (verify-id id))
  
  (let ((visual-effect (make-instance 'visual-projectile :id id)))

    (when (arrayp gfx-path)
      (setf (projectile.gfx-path visual-effect) gfx-path))
    (when (arrayp text-path)
      (setf (projectile.text-path visual-effect) text-path))

    (setf (gethash id (variant.visual-effects *variant*)) visual-effect)
    
    visual-effect))
    

;;; === Deprecated functions

;; add deprecated stuff here
