;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: global.lisp - globally available functions/methods
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

;; uses id.. 
(defmethod variant-home-path ((variant variant))
  (concatenate 'string (home-langband-path) (variant.id variant) "/"))

(defmethod initialise-monsters& (variant &key)
  (error "No INIT-MONSTERS for ~s" (type-of variant)))
  
(defmethod initialise-floors& (variant &key)
  (error "No INIT-FLOORS for ~s" (type-of variant)))

(defmethod initialise-objects& (variant &key)
  (error "No INIT-OBJECTS for ~s" (type-of variant)))


;; a small closure
(let ((registered-variants (make-hash-table :test #'equal)))
  
  (defun register-variant& (id var-constructor)
    "Registers a variant-object."
    
    (check-type var-constructor function)
    (setf (gethash id registered-variants) var-constructor))

  (defun load-variant& (id &key (verbose t))
    "Tries to load a variant."
    (declare (ignore verbose))
    (let ((var-constructor (gethash id registered-variants))
	  (var-obj nil))
      (cond ((functionp var-constructor)
	     (format-note! "[Loading '~a' variant, please wait]" id)
	     (setf var-obj (funcall var-constructor)))
	    (t
	     (error "Unable to find variant ~s" id)))
      
      (when (and var-obj (typep var-obj 'variant))
	var-obj))))


(defmethod variant-data-fname ((var-obj variant) data-fname)
  "Returns a full pathname for data."
  (let ((file-path (variant.config-path var-obj)))
    (if file-path
	(concatenate 'string (lbsys/ensure-dir-name file-path) data-fname)
	data-fname)))



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

#||
      (let ((sys-file (variant.sys-file var-obj)))
	(when verbose
	  (format t "~&Will try to load variant '~a' in file ~a~%" id sys-file))
	(compile-in-environment
	 #'(lambda ()
	     (load sys-file)
	     (mk:operate-on-system id 'compile :verbose nil)
	     (when verbose
	       (format t "~&Variant '~a' compiled and loaded.~%" id))))
	var-obj))))
||#
	     


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



(defmacro with-new-screen (arg &body body)
  (declare (ignore arg))
  `(unwind-protect
    (prog2
	(screen-save)
	,@body)
    (screen-load)))

(defmacro with-frame ((num) &body body)
  (let ((sym (gensym)))
    `(let ((,sym (c-get-cur-term)))
      (unwind-protect
	   (progn
	     (term-activate& ,num)
	     ,@body)
	(term-activate& ,sym)))))

(defmacro with-full-frame (() &body body)
  `(unwind-protect
    (progn 
      (switch-to-full-frame&)
      (with-frame (+full-frame+)
	,@body))
    (switch-to-regular-frameset&)))

(defun invoke-in-dialogue (fun)
  (let ((term (c-get-cur-term)))
    (cond ((= term +dialogue-frame+)
	   (funcall fun))
	  (t
	   (unwind-protect
		(progn
		  (c-deactivate-frame! +charinfo-frame+)
		  (c-deactivate-frame! *map-frame*)
		  (c-activate-frame! +dialogue-frame+)
		  (c-clean-frame! +dialogue-frame+)
		  (with-frame (+dialogue-frame+)
		    (funcall fun)))

	     (progn
	       (c-wipe-frame! +dialogue-frame+)
	       (c-deactivate-frame! +dialogue-frame+)
	       (c-activate-frame! +charinfo-frame+)
	       (c-activate-frame! *map-frame*)
	       (c-clean-frame! +charinfo-frame+)
	       (c-clean-frame! *map-frame*)
	       )))
	  )))

;; very bulky
(defmacro with-dialogue (() &body body)
  ;; posibly find better solution later
  `(invoke-in-dialogue #'(lambda () ,@body)))


;; hackish!
(defun queue-map-char! (x y col sym tcol tsym)
  "Queues to the map-frame."
  (with-frame (*map-frame*)
    (c-term-queue-char! x y col sym tcol tsym)))

;; ugly hack!
(defun c-term-fresh! (&optional (term -1))
  "If term is a negative number (default) it will fresh all terms, if not
it will fresh specified term."
  (cond ((>= term 0)
	 (with-frame (term)
	   (c-term_fresh!)))
	(t
	 ;;(warn "Fresh with negative argument!!!")
	 (dotimes (i +max-frames+)
	   (with-frame (i)
	     ;;(warn "Freshing term ~s" i)
	     (c-term_fresh!))))))


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
    (%term-putstr! x-pos 0 -1 +term-l-blue+ "-more-")
    
    (unless forced
      (block input-loop
	(loop
	 (let ((ch (read-one-character)))
	   (c-term-fresh! +message-frame+)
	   ;; see util.c for real code.. just skip now
	   (return-from input-loop t)))))
    (c_term_erase! 0 0 255)))

;; this code is simpler than in C, but does the same job basically
(let ((msg-col 0)
      (end-col 72)
      (messages '())) ;; make into a limited and circular sequence
  
  (defun print-message! (msg &optional (attr +term-white+))
    "If msg is nil, things are flushed."
    ;;  (warn "going fu on ~s" (type-of str))
    (when (equal msg +c-null-value+)
      (setq msg nil))

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
	(%term-putstr! msg-col 0 -1 attr msg))
      (c-term-fresh! +message-frame+)
      (incf msg-col (1+ msg-len))
      
      t))
  
  (defun flush-messages! (&optional (forced nil))
    ;; add reset-later
    (when (plusp msg-col)
      (%flush-messages! msg-col forced)
      ;; skip reset of msg-flag
      (setf msg-col 0))
    ;; we also fake a check of the length here
    (setf end-col (- (get-term-width +message-frame+) 8)))

  (defun show-messages (&key (offset 0))
    (declare (ignore offset))
    (c-clear-from! 0)
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


(let ((screen-lock nil))
  
  (defun screen-save ()
    
    (when screen-lock
      (error "Screen already locked, please fix execution-path."))

    ;; flush
    (print-message! nil)
    (c-term-save!)
    (setf screen-lock t))
  
  (defun screen-load ()
    (unless screen-lock
      (error "Trying to load a screen, but none is locked."))
    ;; flush
    (print-message! nil)
    (c-term-load!)
    (setf screen-lock nil)))

    
(defun c-quit! (str)
  #-lispworks
  (org.langband.ffi:c_quit! (org.langband.ffi:to-arr str))
  #+lispworks
  (fli:with-foreign-string (base-ptr a b)
    str
    (org.langband.ffi:c_quit! base-ptr))
  (values))

;; do not use outside global.lisp!
(defun %term-putstr! (col row some colour text)
;;  (warn "putting |~s|" text)
  #-lispworks
  (org.langband.ffi:c_term_putstr! col row some colour
				   (org.langband.ffi:to-arr text))
  #+lispworks
  (fli:with-foreign-string (base-ptr a b)
    text
    (org.langband.ffi:c_term_putstr! col row some colour base-ptr))
  (values))

(defun put-coloured-line! (colour text col row)
  "Erases rest of line and puts a string."
    (c_term_erase! col row 255)
    (unless (or (eq text nil) (and (stringp text) (= (length text) 0)))
      (%term-putstr! col row -1 colour text)))

;; needs to print empty stuff?
(defun put-coloured-str! (colour text col row)
  (%term-putstr! col row -1 colour text)
  (values))

;; deprecated!!
(defun c-prt! (str col row)
  (put-coloured-line! +term-white+ str col row))

(defun output-string! (term str col row &optional (attr +term-white+))
  (with-frame (term) ;; do this on the C-side later
    (%term-putstr! col row -1 attr str)))



(defun print-note! (msg &optional (attr +term-white+) &key (row -1))
  "Prints a centred note on the last line."

  (let ((row (if (or (< row 0) (< (get-term-height) row))
		 (get-last-console-line)
		 row))
	;;(row 23) ;;(get-last-console-line))
	;; screws up somewhat when graphics is enabled
	(col (- (int-/ (get-last-console-column) 2)
		(int-/ (length msg) 2))))

    (c_term_erase! 0 row 255)
    (put-coloured-str! attr msg col row)
    (c-term-flush!)
    ;;(c-term-fresh!)
    ;;(pause-last-line!)
    t))


(defun c-bell! (text)
  (declare (ignore text))
  ;; fix this later
  (c-term-fresh!)
  ;; skip msg-add
  ;; skip bell
  ;; skip flush
  )

(defun c-pause-line! (row &key msg attr)
  (unless msg
    (setf msg "[Press any key to continue]"))
  (unless attr
    (setf attr +term-white+))
  (print-note! msg attr :row row)
;;  (c_term_erase! 0 row 255)
;;  (put-coloured-str! attr msg 23 row)
  (let ((read-char (read-one-character)))
    (c_term_erase! 0 row 255)
    read-char))


(defun get-last-console-line (&optional (term -1))
  (1- (get-term-height term)))

(defun get-last-console-column (&optional (term -1))
  (1- (get-term-width term)))

;;(trace get-last-console-column)

(defun pause-last-line! (&key msg attr)
  (c-pause-line! (get-last-console-line) :msg msg :attr attr))

;; fix!
(defun init-c-side& (ui source-path config-path gfx-path debug-level)
  #-lispworks
  (org.langband.ffi:init_c-side& ui source-path config-path gfx-path debug-level)
  #+lispworks
  (fli:with-dynamic-foreign-objects ()
    (flet ((for-str (x)
	     (fli:convert-to-dynamic-foreign-string x)))
      (org.langband.ffi:init_c-side& (for-str ui) (for-str source-path)
				     (for-str config-path)
				     (for-str gfx-path) debug-level))))

(defun c-add-frame! (key name)
  #-lispworks
  (org.langband.ffi:c-add_frame! key name)
  #+lispworks
  (fli:with-foreign-string (s a b)
    name
    (org.langband.ffi:c-add_frame! key s)))

(defun c-add-frame-tileinfo! (key tw th font bg)
  #-lispworks
  (org.langband.ffi:c-add_frame-tileinfo! key tw th font bg)
  #+lispworks
  (fli:with-dynamic-foreign-objects ()
    (flet ((for-str (x)
	     (fli:convert-to-dynamic-foreign-string x)))
      (org.langband.ffi:c-add_frame-tileinfo! key tw th
					      (for-str font) (for-str bg)))))

(defun c-load-sound-effect& (name idx)
  #-lispworks
  (org.langband.ffi:c-load_sound-effect& name idx)
  #+lispworks
  (fli:with-foreign-string (s a b)
    name
    (org.langband.ffi:c-load_sound-effect& s idx)))

(defun c-texture-background! (term fname alpha)
  #-lispworks
  (org.langband.ffi:c-texture_background! term fname alpha)
  #+lispworks
  (fli:with-foreign-string (s a b)
    fname
    (org.langband.ffi:c-texture_background! term s alpha)))

(defun paint-gfx-image& (fname type x y)
  #-lispworks
  (org.langband.ffi:c-paint-gfx-image& fname type x y)
  #+lispworks
  (fli:with-dynamic-foreign-objects ()
    (flet ((for-str (x)
	     (fli:convert-to-dynamic-foreign-string x)))
      (org.langband.ffi:c-paint-gfx-image& (for-str fname) (for-str type)
					   x y))))

(defun %load-people-image (name wid height)
  (let ((pname (concatenate 'string "./graphics/people/" name)))
    (load-scaled-image& pname -1 wid height)))

(defun %paint-people-image (name x y)
  (paint-gfx-image& name "people" x y))

(defun %paint-other-image (name x y)
  (paint-gfx-image& name "other" x y))



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
		     (print-word cur-word))))))
    ))

(defsubst read-one-character ()
  "Reads one character from the C-side."
  (let ((read-obj (c-inkey!)))
    (etypecase read-obj
      (number (code-char read-obj))
      (character read-obj))))

  
(defsubst clear-the-screen! ()
  "Clears the screen on the C-side."
  (c-term-clear!)
  #+cmu
  (c-clear-from! 0))


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
 
;;; === Code related to floors

(defun get-floor-type (id &key (variant *variant*))
  "Returns an object of type FLOOR-TYPE or NIL."
  (let ((table (variant.floor-types variant)))
    (gethash id table)))

(defun (setf get-floor-type) (floor id)
  "Adds a floor-type with given id to the appropriate table."
  (let ((table (variant.floor-types *variant*)))
    (setf (gethash id table) floor)))

(defun define-floor-type (id name x-attr x-char &key mimic text-attr text-char)
  "Defines a floor-type and registers it.  The floor is returned."
  (let ((ftype (make-instance 'floor-type :id id
			      :name name
			      :x-attr (etypecase x-attr
					(number (charify-number x-attr)))  ;; right??
			      :x-char (etypecase x-char
					(character (char-code x-char))
					(number x-char))
			      :mimic mimic)))
    (cond (text-attr
	   (setf (text-attr ftype) text-attr))
	  (t
	   (setf (text-attr ftype) (x-attr ftype))))
    
    (cond (text-char
	   (setf (text-char ftype) text-char))
	  (t
	   (setf (text-char ftype) (x-char ftype))))
    
    (setf (get-floor-type id) ftype)
    ftype))

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
  (x-attr (trap.type obj)))

(defmethod x-char ((obj active-trap))
  (x-char (trap.type obj)))

(defmethod text-attr ((obj active-trap))
  (text-attr (trap.type obj)))

(defmethod text-char ((obj active-trap))
  (text-char (trap.type obj)))


(defun define-trap-type (id name &key x-char x-attr text-char text-attr
			 effect min-depth max-depth rarity)
  "Defines and registers a trap-type."
  (unless (verify-id id)
    (warn "Trap-id ~s not valid" id)
    (return-from define-trap-type nil))
  
  (let* ((the-char (etypecase x-char
		     (character (char-code x-char))
		     (number x-char)))
	 (trap-obj (make-instance 'trap-type :id id :name name
				 :x-char the-char :x-attr x-attr
				 :min-depth min-depth :max-depth max-depth
				 :rarity rarity
				 ))
	(table (variant.traps *variant*)))

    (cond (text-char
	   (setf (text-char trap-obj) text-char))
	  (t
	   (setf (text-char trap-obj) (x-char trap-obj))))
    
    (cond (text-attr
	   (setf (text-attr trap-obj) text-attr))
	  (t
	   (setf (text-attr trap-obj) (x-attr trap-obj))))
    
    
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
  (if (= (c-has_frame key type) 1)
      t
      nil))

(defun c-get-frame-gfx-tiles? (key type)
  (if (= (c-get_frame-gfx-tiles key type) 1)
      t
      nil))


(defun update-term-sizes! ()
  (dotimes (i +predefined-frames+)
    (let ((var (aref *predefined-frames* i)))
      (unless var
	;; check if there should be one
	(when (c-has-frame? i +frametype-predefined+)
	  (setf var (make-instance 'subwindow))))
      
      ;; ok, we have a frame we need more info about
      (when var
	;; we need to update our info
	(setf (subwindow.columns var)     (c-get-frame-columns i +frametype-predefined+)
	      (subwindow.rows var)        (c-get-frame-rows i +frametype-predefined+)
	      (subwindow.tile-width var)  (c-get-frame-tile-width i +frametype-predefined+)
	      (subwindow.tile-height var) (c-get-frame-tile-height i +frametype-predefined+)
	      (subwindow.gfx-tiles? var)  (c-get-frame-gfx-tiles? i +frametype-predefined+))

	(assert (plusp (subwindow.columns var)))
	(assert (plusp (subwindow.rows var)))
	(warn "window ~s has size [~d,~d,~d,~d] and gfx ~s" i
	      (subwindow.columns var) (subwindow.rows var)
	      (subwindow.tile-width var) (subwindow.tile-height var)
	      (subwindow.gfx-tiles? var))

	(setf (aref *predefined-frames* i) var))
      )))


(defun get-frame-width (&optional (term -1))
  (if (>= term 0)
      (subwindow.columns (aref *predefined-frames* term))
      (subwindow.columns (aref *predefined-frames* (c-get-cur-term)))))

;; deprecated
(defun get-term-width (&optional (term -1))
  (get-frame-width term))


(defun get-frame-height (&optional (term -1))
  (if (>= term 0)
      (subwindow.rows (aref *predefined-frames* term))
      (subwindow.rows (aref *predefined-frames* (c-get-cur-term)))))

;; deprecated
(defun get-term-height (&optional (term -1))
  (get-frame-height term))

(defun get-panel-width ()
  (get-term-width *map-frame*))

(defun get-panel-height ()
  (get-term-height *map-frame*))

;; bah!
(defun load-gfx-tiles? ()
  (eq (get-system-type) 'sdl))

;; hackish!
(defun use-gfx-tiles? (&optional (term -1))
  (if (>= term 0)
      (subwindow.gfx-tiles? (aref *predefined-frames* term))
      (subwindow.gfx-tiles? (aref *predefined-frames* (c-get-cur-term)))))

(defun graphical-map? ()
  (use-gfx-tiles? *map-frame*))

(defun use-images? ()
  (if (eq (get-system-type) 'sdl)
      t
      nil))

(defmacro tile-number (num)
  `(+ +graphics-start+ ,num))

;; also support names!
(defmacro tile-file (num)
  `(+ +graphics-start+ ,num))

(defun get-aim-direction ()
  "Interactive!"
  (flush-messages! t)
  (flet ((read-loop ()
	   (loop
	    (c-prt! "Direction: " 0 0)
	    (let ((val (read-one-character)))
	      (cond ((or (eql val #\.)
			 (eql val #\0)
			 (eql val #\t))
		     (c-prt! "" 0 0)
		     (return-from read-loop 5))
		    ((digit-char-p val)
		     (c-prt! "" 0 0)
		     (return-from read-loop (digit-char-p val)))
		    ((eql val +escape+)
		     (c-prt! "" 0 0)
		     (return-from read-loop nil))
		    (t
		     (c-prt! "Unknown direction!" 0 0)))
	      ))))
    
  (with-frame (+query-frame+)
    (let ((retval (read-loop)))
      (c-prt! "" 0 0)
      retval))))
  
;;; to have empty place-holders
#-image-support
(defun load-scaled-image& (fname idx wid hgt)
  (declare (ignore fname idx wid hgt))
  nil)

(defun image-exists? (type name)
  (let ((fname (concatenate 'string *engine-graphics-dir* (string-downcase (string type)) "/" name)))
    (probe-file fname)))

(defun switch-to-full-frame& ()
  (loop for i from 1 below +max-frames+
	do
	(c-deactivate-frame! i))
  (c-activate-frame! +full-frame+)
  (c-clean-frame! +full-frame+))

;; fix to no-cons later
(defun switch-to-regular-frameset& ()
  (c-deactivate-frame! +full-frame+)
  (dolist (i (list +message-frame+ +charinfo-frame+
		   +misc-frame+ *map-frame* +inv-frame+))
    (c-activate-frame! i)
    (c-clean-frame! i)))
