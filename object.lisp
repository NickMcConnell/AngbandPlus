;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#||

DESC: object.lisp - code for object-kinds
Copyright (c) 2000-2003 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

----

ADD_DESC: The code for object-kinds which is basic and should be widely
ADD_DESC: available in the game.

||#

(in-package :org.langband.engine)


(defmacro def-obj-type (name &key is key kind-slots aobj-slots)
  "Creates necessary objects and registers them."
  (let* ((ok-name (concat-pnames 'object-kind/ name))
	 (act-name (concat-pnames 'active-object/ name))
	 (ok-par-name (if is (concat-pnames 'object-kind/ is) 'object-kind))
	 (act-par-name (if is (concat-pnames 'active-object/ is) 'active-object))
	 (reg-call (when key `(setf (gethash ',key *obj-type-mappings*) (cons ',ok-name ',act-name)))))
    
    (let ((retval `(progn
		    (defclass ,ok-name (,ok-par-name) ,kind-slots)
		    (defclass ,act-name (,act-par-name) ,aobj-slots)
		    ,reg-call)))
      
;;      (warn "Ret: ~s" retval)
      
      retval)))


(defun ensure-game-values! (item)
  "Helper-function to make sure given item has game-values."
  (check-type item active-object)
  (unless (aobj.game-values item)
    (setf (aobj.game-values item) (make-game-values))))


(defmethod learn-about-object! (player object what)
  (error "Fell through learn with ~s ~s ~s" player object what))

(defmethod learn-about-object! (player (object active-object) (what (eql :aware)))
  (learn-about-object! player (aobj.kind object) what))

(defmethod learn-about-object! (player (object object-kind) (what (eql :aware)))
  (declare (ignore player))
  (setf (object.aware object) t))

(defmethod learn-about-object! (player (object active-object) (what (eql :tried)))
  (learn-about-object! player (aobj.kind object) what))

(defmethod learn-about-object! (player (object object-kind) (what (eql :tried)))
  (declare (ignore player))
  (setf (object.tried object) t))

(defmethod learn-about-object! (player (object active-object) (what (eql :known)))
  (declare (ignore player))
  (let ((flag (aobj.identify object)))
    (bit-flag-remove! flag +ident-sense+)
    (bit-flag-remove! flag +ident-empty+)
    (bit-flag-add! flag +ident-known+)
    (setf (aobj.identify object) flag)
    flag))

(defmethod learn-about-object! (player (object object-kind) (what (eql :known)))
  (declare (ignore player))
  (error "Learn :known must be called on an active-object.."))

(defmethod is-object-known? ((object active-object))
  (or (bit-flag-set? (aobj.identify object) +ident-known+)
      (and (object.easy-know (aobj.kind object))
	   (object.aware (aobj.kind object)))))

(defmethod get-text-colour ((obj active-object))
  (get-text-colour (aobj.kind obj)))

(defmethod get-text-colour ((kind object-kind))
  (let ((flavour (object.flavour kind)))
    (if flavour
	(text-attr flavour)
	(text-attr kind))))

(defmethod get-okind-table ((var-obj variant) (level level))
  
  (let* ((o-table (get-otype-table var-obj level))
	 (table (gobj-table.obj-table o-table)))
    table))

(defmethod get-okind-alloc-table ((var-obj variant) (level level))
  
  (let* ((o-table (get-otype-table var-obj level))
	 (table (gobj-table.alloc-table o-table)))
    table))

(defmethod get-object-kind ((variant variant) obj)
  "Returns the object-kind for the given obj id."
  (etypecase obj
    (string (gethash obj (variant.objects variant)))
    (symbol (gethash (symbol-name obj) (variant.objects variant)))
    (integer (block foo
	       (loop for x being the hash-values of (variant.objects variant)
		     do
		     (when (eql obj (object.numeric-id x))
		       (return-from foo x))
		     ))
	     )))

(defmethod object.name ((obj active-object))
  (object.name (aobj.kind obj)))


(defmethod object.game-values ((obj active-object))
  (let ((gvals (aobj.game-values obj)))
    (if gvals
	gvals
	(object.game-values (aobj.kind obj)))))

(defmethod x-attr ((obj active-object))
  (let* ((kind (aobj.kind obj))
	 (flavour (object.flavour kind)))
    (if flavour
	(x-attr flavour)
	(x-attr kind))))

(defmethod x-char ((obj active-object))
  (let* ((kind (aobj.kind obj))
	 (flavour (object.flavour kind)))
    (if flavour
	(x-char flavour)
	(x-char kind))))

(defmethod gfx-sym ((obj active-object))
  (let* ((kind (aobj.kind obj))
	 (flavour (object.flavour kind)))
    (if flavour
	(gfx-sym flavour)
	(gfx-sym kind))))

(defmethod text-attr ((obj active-object))
  (let* ((kind (aobj.kind obj))
	 (flavour (object.flavour kind)))
    (if flavour
	(text-attr flavour)
	(text-attr kind))))

(defmethod text-char ((obj active-object))
  (let* ((kind (aobj.kind obj))
	 (flavour (object.flavour kind)))
    (if flavour
	(text-char flavour)
	(text-char kind))))

(defmethod text-sym ((obj active-object))
  (let* ((kind (aobj.kind obj))
	 (flavour (object.flavour kind)))
    (if flavour
	(text-sym flavour)
	(text-sym kind))))

(defmethod object.weight ((obj active-object))
  (* (aobj.number obj) (object.weight (aobj.kind obj))))


(defun create-aobj-from-id (id &key (amount 1) (variant *variant*))
  "Creates an active object from object-kind identified by id.
Amount specifies how many objects the active-object is, e.g for arrows.
Uses *VARIANT*."
  (let* ((kind (get-object-kind variant id))) ;; fix later
    (unless kind
      (return-from create-aobj-from-id nil))
    (create-aobj-from-kind kind :amount amount :variant variant)))


(defun create-aobj-from-kind-num (num &key (amount 1) (variant *variant*))
  "This is a hackish function which is backward compatible
with k-info.txt numbers. NUM is the numeric id."
  (create-aobj-from-id num :amount amount :variant variant))

(defun create-aobj-from-kind (kind &key (amount 1) (variant *variant*))
  "Creates an aobj from a given kind.  Uses *VARIANT*."
  (let ((obj (produce-active-object variant kind)))
    ;; assume dice
    (setf (aobj.number obj)
	  (cond ((and (stringp amount) (position #\d amount))
		 (parse-and-roll-dice amount))
		((stringp amount)
		 (parse-integer amount))
		((and (numberp amount) (plusp amount))
		 amount)
		(t
		 (warn "Invalid amount ~s for object ~s, assuming 1 instead."
		       amount (object.name kind))
		 1)))

    (when-bind (create-effect (get-object-effect variant obj :create))
      
      (assert (and (effect-entry-p create-effect)
                   (functionp (effect-entry-fun create-effect))))
      ;;  ignore retval so far
      (funcall (effect-entry-fun create-effect) obj))

;;    (trigger-event obj :on-create (list nil nil))
    (activate-object obj)
    obj))

(defmethod trigger-event ((obj active-object) event arg-list)
  "trigger any kind events first (add this obj as last argument).. then active object"
  (trigger-event (aobj.kind obj) event (append arg-list (list obj)))
  (apply-event event (aobj.events obj) arg-list))

(defmethod trigger-event ((obj object-kind) event arg-list)
  "trigger events registered for the kind."
  (apply-event event (object.events obj) arg-list))
  

(defun write-pluralised-string (stream plural-string number &key (flavour nil) (ident nil) (actual-name nil)
				(numeric-prefix t))
  (declare (type u16b number)
	   (type simple-base-string plural-string))

  (assert (or (eq nil flavour) (typep flavour 'flavour)))
  
  (let ((plural (> number 1))
	(counter 0))
    (declare (type u16b counter))

    ;; fix this to jump right whatever happens!
    (let ((article? (eql (schar plural-string counter) #\&)))
      (when article?
	(incf counter 2)) ;; skip ampersand plus space
      (cond ((eq numeric-prefix nil)
	     nil) ;; nothing
	    ((<= number 0)
	     (write-string "no more " stream))
	    ((> number 1)
	     (format stream "~d " number))
	    
	    ;; did we ask for a/an ?
	    (article?
	     ;; should take into account flavour! should also check length
	     (let ((next-char (schar plural-string counter)))
	       (when (and (eql next-char #\#) flavour) ;; use flavour if we have a flavour sign
		 (setf next-char (schar (flavour.name flavour) 0)))
	       ;;(format t "~&Checking ~s for vowel in '~a'~%" next-char plural-string)
	       (cond ((find next-char '(#\a #\e #\i #\o #\u #\y))
		      (write-string "an " stream))
		     (t
		      (write-string "a " stream)))
	       ))
	    
	    (t nil)))
	   
    
    (loop for i of-type u16b from counter below (length plural-string)
	  for x = (schar plural-string i)
	  do
	  (case x
	    (#\~ (when plural
		   ;; hackish
		   (when (find (schar plural-string (1- i)) '(#\h #\s))
		     (write-char #\e stream))
		   (write-char #\s stream)))
	    #||
	    (#\& (if numeric-prefix
		     (if plural
			 (write-string (format nil "~a" number) stream)
			 (if (find (schar plural-string (+ i 2)) '(#\a #\e #\i #\o #\u #\y))
			     (write-string "an" stream)
			     (write-char #\a stream)))
		     ;; wah!
		     ))
	    ||#	    
	    (#\# (when flavour
		   (write-string (flavour.name flavour) stream)
		   (write-char #\Space stream)
		   ))
	    
	    (#\@ (when ident
		   (write-string " of " stream)
		   (write-string actual-name stream)))
	    
	    (otherwise
	     (write-char x stream))))
    ))

;;(trace write-pluralised-string)

(defun plural-name (number name flavour ident actual-name &key numeric-prefix)
  "Returns a name with plurality fixed as in normal Angband.  FIX ME"
  (with-output-to-string (s)
    (write-pluralised-string s name number :flavour flavour :ident ident :actual-name actual-name
			     :numeric-prefix numeric-prefix)))




(defmethod is-eatable? ((player player) (obj active-object))
  nil)


(defmethod is-magical? ((obj active-object))
  nil)

(defmethod is-artifact? ((obj active-object))
  nil)

(defmethod is-cursed? ((obj active-object))
  (bit-flag-set? (aobj.identify obj) +ident-cursed+))

(defmethod is-broken? ((obj active-object))
  (bit-flag-set? (aobj.identify obj) +ident-broken+))


(defun get-object-list (&key (var-obj *variant*) (level *level*))
  "returns a fresh list.  Remove me!"
  (let ((table (get-okind-table var-obj level)))
    (stable-sort (loop for v being each hash-value of table
		       collecting v)
		 #'<
		 :key #'object.numeric-id)))


(defmethod copy-active-object ((variant variant) (obj active-object))
  "Copies the given OBJ and returns a new object that is equal."
  
  (let ((new-obj (make-instance (class-of obj))))
;;    (warn "Old ~s and new ~s" (class-of obj) (class-of new-obj))
    ;; needs improvement
    (dolist (i '(kind inscription number loc-x loc-y identify marked))
      ;; doesn't handle shared-structures well
      (setf (slot-value new-obj i) (slot-value obj i)))
    (when-bind (gvals (aobj.game-values obj))
      (setf (aobj.game-values new-obj) (copy-game-values variant gvals)))
    ;; skip contains
    ;; skip events
    new-obj))

(defun is-object-effect? (arg)
  (functionp arg))

;; hack
(defmacro object-effect (arguments &body body)
  (assert (= (length arguments) 3))
  (let ((def `(lambda ,arguments
	       (declare (ignorable ,@arguments))
	       ,@body)))
;;    (warn "Def is ~s" def)
    `(function ,def)))

(defmacro magic-add (arguments &body body)
  (assert (= (length arguments) 3))
  (let ((def `(lambda ,arguments
	       (declare (ignorable ,@arguments))
	       ,@body)))
;;    (warn "Def is ~s" def)
    `(function ,def)))

(defmethod get-object-effect ((var variant) (the-object active-object) effect)
  (find effect (object.effects (aobj.kind the-object)) :key #'effect-entry-type))

(defmethod get-object-effect ((var variant) (the-object object-kind) effect)
  (find effect (object.effects the-object) :key #'effect-entry-type))

(defun update-kind-display (kind &key x-attr x-char text-attr text-char)

  (let ((m-obj (if (typep kind 'object-kind)
		   kind
		   (get-object-kind *variant* kind))))

    (unless m-obj
      (warn "unable to find object-kind ~s" kind)
      (return-from update-kind-display nil))

    (check-type m-obj object-kind)

    ;;(unless (and x-attr x-char)
    ;;  (warn "No x-char x-attr found for ~s" kind))
    
    (handle-gfx-visual m-obj x-attr x-char)
    (handle-text-visual m-obj text-attr text-char)

    m-obj))

(defmethod initialise-object-kind! ((var-obj variant) (new-obj object-kind) keyword-args)
  
  ;; hackish, gradually move variant-specific stuff to variant. 

  (let* ((id (object.id new-obj))
	 ;;(name (object.name new-obj))
	 (key (if (symbolp id)
		  (string-downcase (symbol-name id))
		  id))
	 )

    
  (destructuring-bind (&key numeric-id x-attr x-char depth rarity
			    chance locale weight cost sort-value
			    events game-values flags flavour desc the-kind
			    multiplier
			    (text-attr :unspec) (text-char :unspec)
			    (on-quaff :unspec)
			    (on-read :unspec) (on-eat :unspec)
			    (on-create :unspec) (on-add-magic :unspec)
			    (on-wear :unspec) (on-drop :unspec)
			    (on-takeoff :unspec) (on-destroy :unspec)
			    (on-zap :unspec) (on-hit :unspec) (on-miss :unspec)
			    (on-calculate :unspec)
			    &allow-other-keys)
      keyword-args
    (declare (ignore flavour desc the-kind multiplier))

    (when flags
      (when (find '<easy-know> flags)
	(setf (object.easy-know new-obj) t)
	(setf flags (remove '<easy-know> flags)))
      (setf (object.flags new-obj) flags))

;;    (when (and depth level)
;;      (error "Object ~s given both level and depth." key))

;;    (when level
;;      (setf depth level))

    (cond ((and depth (typep depth '(integer 0 *)))
	   (setf (object.depth new-obj) depth))
	  (t
	   (lang-warn "Given illegal depth-value ~s for object ~s" depth key)
	   (setf (object.depth new-obj) 1))) ;; hack


    (setf (object.numeric-id new-obj) (if numeric-id
					  numeric-id
					  key)
	  ;;(x-attr new-obj) (convert-obj x-attr :x-attr)
	  ;;(x-char new-obj) (convert-obj x-char :x-char)
	  (object.rarity new-obj) rarity
	  (object.chance new-obj) chance
	  (object.locale new-obj) locale
	  (object.weight new-obj) weight
	  (object.cost new-obj) cost
	  (object.sort-value new-obj) (if (numberp sort-value)
					  sort-value
					  0) ;; hack
	  (object.events new-obj) (get-legal-events events)
	  (object.game-values new-obj) game-values)

    (update-kind-display new-obj :x-attr x-attr :x-char x-char :text-attr text-attr :text-char text-char)

    
    (flet ((possible-add-effect (effect var &optional (energy +energy-normal-action+))
	     (cond ((eq :unspec var))
		   ((is-object-effect? var)
		    ;;(warn "Compiling ~s for ~s" effect id)
		    (let ((entry (make-effect-entry :type effect
						    ;;;; somehow allegro trips up when passed a compiled function
						    ;;:fun #+allegro var
						    ;;#-allegro (compile nil var)
						    :fun var
						    :energy-use energy)))
		      (pushnew entry (object.effects new-obj) :key #'effect-entry-type)))
		   (t
		    (error "Unknown value ~s for ~s for ~s" var effect key)))))
      
      (possible-add-effect :quaff on-quaff)
      (possible-add-effect :read on-read)
      (possible-add-effect :eat on-eat)
      (possible-add-effect :create on-create)
      (possible-add-effect :add-magic on-add-magic)
      (possible-add-effect :wear on-wear)
      (possible-add-effect :drop on-drop)
      (possible-add-effect :takeoff on-takeoff)
      (possible-add-effect :destroy on-destroy)
      (possible-add-effect :zap on-zap)
      (possible-add-effect :hit on-hit)
      (possible-add-effect :miss on-miss)
      (possible-add-effect :calculate on-calculate)
      )


    new-obj)))
  

;; must be fixed!!
(defun define-object-kind (id name &rest keyword-args
			   &key the-kind &allow-other-keys) ;; list should be checked thoroughly!
  "creates and establishes an object corresponding to parameters.  It uses
the *VARIANT* object so it has to be properly initialised."

  (let* ((var-obj *variant*)
	 (new-obj (produce-object-kind var-obj id name :the-kind the-kind))
	 )

    (when (symbolp id)
      (warn "Deprecated id for object ~s" id))

    (initialise-object-kind! var-obj new-obj keyword-args)
    
    ;; hackish addition to big object-table
    (let ((main-obj-table (variant.objects var-obj))
	  (obj-id (object.id new-obj)))
      (multiple-value-bind (val found-p)
	  (gethash obj-id main-obj-table)
	(declare (ignore val))
	(when found-p
	  (warn "Replacing object with id ~s" obj-id))
	(setf (gethash obj-id main-obj-table) new-obj)))
    
    ;; apply object-filters on the new object.    
    (apply-filters-on-obj :objects var-obj new-obj)
    
    new-obj))


(defmethod produce-active-object ((variant variant) (okind object-kind))
  "Returns an active-object based on the given okind."

  
  (let* ((wanted-kind (object.the-kind okind))
	 (mapping (when wanted-kind
		    (gethash wanted-kind *obj-type-mappings*)))
	 (gvals (when (object.game-values okind)
		  (copy-game-values variant (object.game-values okind))))
	 )

    (let ((new-obj (cond ((and mapping (consp mapping))
			  (make-instance (cdr mapping) :obj okind :game-values gvals))
			 (t 
			  (make-instance 'active-object :obj okind :game-values gvals)))))


      (when (find '<curse> (object.flags okind))
	(bit-flag-add! (aobj.identify new-obj) (logior +ident-cursed+ +ident-broken+)))

      new-obj)))


;; this is a hack
(defmethod produce-active-object ((variant variant) (id string))
  (let ((okind (get-object-kind variant id)))
    (cond ((typep okind 'object-kind)
	   (produce-active-object variant okind))
	  (t
	   (warn "Unable to find object with id ~s" id)
	   nil))))


(defmethod produce-object-kind ((variant variant) id name &key the-kind)
  "Produces a suitable object of type object-kind"

  (assert (or (stringp id) (symbolp id)))

  (let ((key (if (symbolp id) (symbol-name id) id))
	(has-mapping (gethash the-kind *obj-type-mappings*)))
    
    (assert (verify-id key))

    (cond ((consp has-mapping)
	   (make-instance (car has-mapping) :id key :name name
			  :the-kind the-kind))
	  (t
	   (make-instance 'object-kind :id key :name name))
	  )))


(defun define-flavour-type (symbol &key generator-fn x-char text-char)
  "Defines a flavour-type"
  (let* ((var-obj *variant*)
	 (ft-obj (make-instance 'flavour-type
				:symbol symbol
				:x-char x-char
				:text-char text-char
				:generator-fn generator-fn))
	 (table (variant.flavour-types var-obj)))
    (setf (gethash symbol table) ft-obj)
    ft-obj))

(defun legal-flavour-obj? (flav)
  (typep flav 'flavour))

#||
  (and (consp flav)
       (stringp (car flav))
       (atom (car flav)) ;; integer 0..16 ?
       ))
||#
  
(defun establish-flavour& (f-type name &key x-attr x-char text-attr text-char)
    
  (let ((flav (make-instance 'flavour :name name));; :x-attr x-attr :text-attr text-attr))
	(table (flavour-type.table f-type)))

    (handle-gfx-visual flav x-attr x-char)
    (handle-text-visual flav text-attr text-char)

    (push flav (flavour-type.unused-flavours f-type))
    (setf (gethash name table) flav)
    
    flav))

(defun find-flavour-type (variant-obj type)
  "Tries to find given flavour-type in given variant-obj."
  (gethash type (variant.flavour-types variant-obj)))

;;(trace find-flavour-type)

(defun define-basic-flavour (type name &key x-char x-attr text-char text-attr)
  "Defines a basic flavour.."
  
  (let ((ft-obj (find-flavour-type *variant* type)))
    (unless ft-obj
      (warn "Unable to find flavour-type ~s" type)
      (return-from define-basic-flavour nil))

    (establish-flavour& ft-obj name
			:x-attr x-attr
			:text-attr text-attr
			:x-char (if x-char x-char (flavour-type.x-char ft-obj))
			:text-char (if text-char text-char (flavour-type.text-char ft-obj))
			)))
	

#||
(defun use-flavour-table (flavour-to-use used-by &key (variant *variant*))
  "a handy way to re-use a flavour-table for another kind
of objects.  all entries are copied, not shared."
  
  (let* ((var-obj variant)
	 (used-by-type (find-flavour-type var-obj used-by))
	 (type-to-use (find-flavour-type var-obj flavour-to-use))
	 (old-table (flavour-type.table type-to-use))
	 (new-table (flavour-type.table used-by-type)))
	 
    
    (maphash #'(lambda (key val)
		 (setf (gethash key new-table) val))
	     old-table)
    
    ;;(warn "~s will use ~s" used-by-type type-to-use)
    
    used-by-type))
||#

(defmethod flavour-object! ((variant variant) (obj object-kind))
  ;; do nothing
  (warn "Not added flavouring to ~a" obj)
  nil)

(defun %flavour-obj-kind! (kind)
  "Flavours the given object OBJ."
  (let* ((var-obj *variant*)
	 (f-type (gethash (object.the-kind kind) (variant.flavour-types var-obj))))
    (when f-type
      (cond ((flavour-type.generator-fn f-type)
	     (setf (object.flavour kind) (funcall (flavour-type.generator-fn f-type)
						  var-obj kind)))
	    
	    ((consp (flavour-type.unused-flavours f-type))
	     (setf (object.flavour kind) (pop (flavour-type.unused-flavours f-type))))
	    
	    ((null (flavour-type.unused-flavours f-type))
	     (warn "No more unused ~s flavours for ~s" (object.the-kind kind)
		   (object.id kind)))
	    (t
	     (describe f-type)
	     (error "Unable to flavour object kind ~a with ~s" kind (flavour-type.symbol f-type))
	     )))
    
    (assert (legal-flavour-obj? (object.flavour kind)))
    
    kind))


(defmethod get-visual-projectile (obj)
  (declare (ignore obj))
  nil)

