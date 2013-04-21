;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: load.lisp - loading of various parts of the game
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

(defun %distribute-monsters! (dungeon objs)
  (dolist (i objs)
    (let ((its-x (location-x i))
	  (its-y (location-y i)))

      (unless (legal-coord? dungeon its-x its-y)
	(error "Trying to set coord [~a,~a] when size is [~a,~a] on mon ~s"
	       its-x its-y (dungeon.width dungeon) (dungeon.height dungeon) i))
      
    (let ((coord (aref (dungeon.table dungeon) its-x its-y)))
      
      (if (coord.monsters coord)
	  (error "REPORT: Already added mon to (~a,~a)" (location-x i) (location-y i))
	  (push i (coord.monsters coord))))))
  nil)

(defun %distribute-objects! (dungeon objs)
  (dolist (i (reverse objs))
    (let ((its-x (location-x i))
	  (its-y (location-y i)))

      (unless (legal-coord? dungeon its-x its-y)
	(error "Trying to set coord [~a,~a] when size is [~a,~a] on objs ~s"
	       its-x its-y (dungeon.width dungeon) (dungeon.height dungeon) i))
      
      (let* ((coord (aref (dungeon.table dungeon) its-x its-y))
	     (container (coord.objects coord)))
	(unless container
	  (setf container (make-floor-container dungeon its-x its-y)))
	
	(item-table-add! container i)
	(setf (coord.objects coord) container))
      ))
      
  nil)

(defun %distribute-decor! (dungeon decor)

  (dolist (i (reverse decor))
    (let ((its-x (location-x i))
	  (its-y (location-y i)))

      (unless (legal-coord? dungeon its-x its-y)
	(error "Trying to set coord [~a,~a] when size is [~a,~a] on objs ~s"
	       its-x its-y (dungeon.width dungeon) (dungeon.height dungeon) i))

      (typecase i
	(active-trap
	 (place-trap! *variant* dungeon its-x its-y i))
	(active-door
	 (place-door! *variant* dungeon its-x its-y i))
	(t
	 nil))

      )))


(defun %filed-variant (&key engine-num-version id variant-num-version turn information)

  (unless engine-num-version
    (signal (make-condition 'savefile-problem
			    :desc "Save-game too old (no engine version specified). Please remove old save-files.")))

  (unless variant-num-version
    (signal (make-condition 'savefile-problem
			    :desc "Save-game too old (no variant version specified). Please remove old save-files.")))
  
  (unless (and id (stringp id))
    (signal (make-condition 'savefile-problem
			    :desc "The saved variant-object does not have a legal id, it has ~s" id)))

  (let ((var-obj (load-variant& id)))
    (if var-obj
	(setf *variant* var-obj)
	(error "Unable to find variant with id ~s" id)))
  
  (let ((var-obj *variant*)) ;; hackish
   
    (unless (equal (string id) (string (variant.id var-obj)))
      (warn "Id of loaded variant (~s) doesn't match running variant (~s)"
	    id (variant.id var-obj)))

    (setf (variant.turn var-obj) turn)

    (when (consp information)
      (dolist (i information)
	(setf (get-information (car i) :variant var-obj) (cdr i))))
    
    ;; hackish
    (activate-object var-obj)
    
    var-obj))

(defun %filed-object-kind (&key id (aware :unspec) (flavour :unspec))

  ;; kind of hackish
  (assert (stringp id))
  (let* ((var-obj *variant*)
	 (the-kind (gethash id (variant.objects var-obj))))
    (unless the-kind
      (warn "Unable to find object-kind with id ~s" id)
      (return-from %filed-object-kind nil))

    (cond ((eq aware :unspec))
	  ((or (eq aware t) (eq aware nil))
	   (setf (object.aware the-kind) aware))
	  (t
	   (error "Unknown aware-value ~s for kind ~s" aware id)))
    
    (cond ((eq flavour :unspec))
	  ((eq flavour nil)
	   (setf (object.flavour the-kind) nil))
	  ((stringp flavour)
	   ;; find flavour
	   (let ((f-type (gethash (object.the-kind the-kind) (variant.flavour-types var-obj)))
		 (wanted-flavour nil))
	     (cond ((and f-type (flavour-type.generator-fn f-type))
		    (setf wanted-flavour (funcall (flavour-type.generator-fn f-type)
						  var-obj the-kind))
		    ;; scroll hack! (not portable)
		    (setf (flavour.name wanted-flavour) flavour)
		    )
		   (f-type
		    (let ((lookup (gethash flavour (flavour-type.table f-type))))
		      (when (legal-flavour-obj? lookup)
			(setf wanted-flavour lookup)
			(setf (flavour-type.unused-flavours f-type)
			      (delete lookup (flavour-type.unused-flavours f-type)))
			)))
		   (t
		    ;; do nothing
		    ))

	     (unless wanted-flavour
	       (warn "Unable to find flavour ~s for ~s" flavour id))
	     (setf (object.flavour the-kind) wanted-flavour)
	     ))
	       
	  (t
	   (error "Unknown flavour-value ~s for kind ~s" flavour id)))
    t))

(defun %filed-monster-kind (&key id (already-dead :unspec))

  ;; kind of hackish
  (assert (stringp id))
  (let* ((var-obj *variant*)
	 (the-kind (gethash id (variant.monsters var-obj))))
    
    (unless the-kind
      (warn "Unable to find monster-kind with id ~s" id)
      (return-from %filed-monster-kind nil))

    (cond ((eq already-dead :unspec))
	  ((and (typep the-kind 'unique-monster)
		(or (eq already-dead t) (eq already-dead nil)))
	   (setf (monster.already-dead the-kind) already-dead))
	  (t
	   (error "Unknown 'already-dead'-value ~s for kind ~s" already-dead id)))
;;    (warn "Processed ~s with ~a" id already-dead)
    t))

  
(defun %filed-level (&key id rating depth dungeon)

  (let ((constructor (get-level-builder id))
	(the-level nil))

    (if constructor
	(setf the-level (funcall constructor))
	(error "Unable to find level-builder ~s" id))

    (setf (level.rating the-level) rating
	  (level.depth the-level) depth
	  (level.dungeon the-level) dungeon)

    ;; the lvl.id should be preset
    (assert (equal (string id) (string (level.id the-level))))
    
    the-level))


(defun %filed-dungeon (&key width height depth table monsters objects rooms decor)
  "returns a dungeon-object or nil."
  
  (let ((dungeon (create-dungeon width height :its-depth depth)))
    (with-dungeon (dungeon (coord x y))
      (let ((old-cons (aref table x y)))
	(setf (coord.flags coord) (cdr old-cons)) ;; flag first.
	(setf (coord-floor coord) (car old-cons)) ;; then floor
	))

	      
    (when monsters
      (%distribute-monsters! dungeon monsters)
      (setf (dungeon.monsters dungeon) monsters))

    (when objects
      (%distribute-objects! dungeon objects) ;; it is added to dungeon.objects here
      ;;(setf (dungeon.objects dungeon) objects)
      )

    (when rooms
      ;; need more?
      (setf (dungeon.rooms dungeon) rooms))

    (when decor
      (%distribute-decor! dungeon decor))
    
    dungeon))


(defun %filed-monster (&key kind cur-hp max-hp speed energy mana loc-x loc-y alive?)
  "returns an active-monster object or nil."
  
  (let* ((var-obj *variant*)
	 ;;(ret-monster nil)
	 (amon (produce-active-monster var-obj kind)))
    
    (setf (current-hp amon) cur-hp
	  (maximum-hp amon) max-hp
	  (get-creature-speed amon) speed
	  (get-creature-energy amon) energy
	  (get-creature-mana amon) mana
	  (location-x amon) loc-x
	  (location-y amon) loc-y
	  (creature-alive? amon) alive?)
    
    amon))


(defun %filed-object (&key kind inscr number (identify :unspec)
		      (game-values :unspec)
		      contains events loc-x loc-y)
  "returns an active-object or nil."

  ;; FIX: simplify, and let factory deal with kind id
  (let* ((var-obj *variant*)
	 (the-kind (get-object-kind var-obj kind))
	 (ret-obj nil))
    
    (if (not kind)
	(error "Unable to find object-kind ~s" the-kind) ;; make this a warning later
	
	(let ((aobj (produce-active-object var-obj the-kind)))

	  (setf (location-x aobj) loc-x
		(location-y aobj) loc-y
		(aobj.number aobj) number
		(aobj.inscr aobj) inscr
		(aobj.contains aobj) contains
		(aobj.events aobj) events)

	  (cond ((eq identify :unspec))
		((and (integerp identify)
		      (>= identify 0))
		 (setf (aobj.identify aobj) identify))
		(t 
		 (warn "Identify argument ~s to filed-object does not follow invariant."
		       identify)))

	  (cond ((eq game-values :unspec))
		((or (eq game-values nil)
		     (typep game-values 'game-values))
		 (setf (aobj.game-values aobj) game-values))
		(t
		 (warn "Game-values argument to FILED-OBJECT does not follow invariant.")))
		 
	  (setf ret-obj aobj)))
    
    ret-obj))

(defun %filed-room (&key type loc-x loc-y)
  "returns an active-room or nil."
  
  (let ((the-kind (get-room type))
	(ret-obj nil))
    (if (or (not the-kind) (not (functionp the-kind)))
	(error "Unable to find room ~s" type)
	(let ((aobj (make-instance 'active-room :type (funcall the-kind))))

	  (setf (location-x aobj) loc-x
		(location-y aobj) loc-y)
	  
	  (setq ret-obj aobj)))
    
    ret-obj))

(defun %filed-trap (&key type loc-x loc-y)
  "returns an active-trap or nil."
  (let* ((var-obj *variant*)
	 (table (variant.traps var-obj))
	 (type-obj (gethash type table)))

    (cond ((typep type-obj 'trap-type)
	   (create-simple-trap type-obj loc-x loc-y))
	  (t
	   (warn "Unable to find trap-type ~s" type)
	   nil))
    ))

(defun %filed-door (&key type loc-x loc-y flags)
  "returns an active-door or nil."
  (let ((door (get-door *variant* type)))

    (unless (typep door 'active-door)
      (warn "Unable to find door-type ~s" type)
      (return-from %filed-door nil))

    (setf (location-x door) loc-x
	  (location-y door) loc-y)

    (when (integerp flags)
      (setf (decor.visible? door) (if (bit-flag-set? flags #x01) t nil))
      (setf (door.closed? door)   (if (bit-flag-set? flags #x02) t nil))
      (setf (door.broken? door)   (if (bit-flag-set? flags #x04) t nil))
      )

    door))

(defun %filed-player-misc (&key age status height weight)
  (let ((misc (make-instance 'misc-player-info)))
    (when age
      (setf (playermisc.age misc) age))
    (when status
      (setf (playermisc.status misc) status))
    (when height
      (setf (playermisc.height misc) height))
    (when weight
      (setf (playermisc.weight misc) weight))
    misc))
    

(defun %filed-player-info (pl-obj &key name class race gender base-stats cur-statmods
			   hp-table equipment variant temp-attrs)
  "modifies the PL-OBJ with the extra info and returns the modified PL-OBJ."

  (let* ((var-obj (if (is-variant? variant) variant *variant*))
	 (the-class (get-char-class class :variant var-obj))
	 (the-race (get-char-race race :variant var-obj))
	 (the-gender (get-gender var-obj gender)))

    (setf (player.name pl-obj) name)
    
    (if the-class
	(setf (player.class pl-obj) the-class)
	(error "Unable to find class ~s" class))
    (if the-race
	(setf (player.race pl-obj) the-race)
	(error "Unable to find race ~s" race))
    (if the-gender
	(setf (player.gender pl-obj) the-gender)
	(progn
	  (lang-warn "Unable to find gender ~s, assume male" gender)
	  (setf (player.gender pl-obj) '<male>)))

    (setf (player.base-stats pl-obj) base-stats)
    (when (and cur-statmods (arrayp cur-statmods))
      (setf (player.cur-statmods pl-obj) cur-statmods))

    (unless equipment
      (error "When constructing a player, the equipment-list _must_ be kosher."))

    (when (consp temp-attrs)
      (let ((attr-table (player.temp-attrs pl-obj)))
	(dolist (i temp-attrs)
	  (assert (eq (first i) :attr))
	  (assert (eq (third i) :value))
	  (assert (eq (fifth i) :duration))
	  (let ((attr-name (second i))
		(attr-val (fourth i))
		(attr-dur (sixth i)))
	    (loop named name-search
		  for attr being the hash-values of attr-table
		  do
		  (when (string-equal attr-name (attr.name attr))
		    (ecase (attr.value-type attr)
		      (boolean (when (numberp attr-val)
				 (if (= 0 attr-val)
				     (setf attr-val nil)
				     (setf attr-val t))))
		      (integer
		       (assert (integerp attr-val))))
		    (setf (attr.value attr) attr-val
			  (attr.duration attr) attr-dur)
		    (return-from name-search t)))
	    ))
	))
	
    (setf (player.hp-table pl-obj) hp-table
	  (player.equipment pl-obj) equipment
	  ;; calculated
	  (player.inventory pl-obj) (item-table-find equipment 'eq.backpack))

    ;; then we need to recalculate the xp, as we now have race and class
    (update-xp-table! var-obj pl-obj) ;; hack
    (update-max-hp! var-obj pl-obj) ;; hack
    
    pl-obj))
  
(defun %filed-player (&key name race class gender
		      base-stats cur-statmods hp-table equipment
		      temp-attrs
		      loc-x loc-y view-x view-y
		      depth max-depth max-xp cur-xp fraction-xp
		      cur-hp fraction-hp cur-mana fraction-mana
		      gold food energy)
  
  "Returns a player object or nil."
  
  (let* ((var-obj *variant*)
	 (pl-obj (produce-player-object var-obj)))
    
    (setf (location-x pl-obj) loc-x
	  (location-y pl-obj) loc-y
	  (player.view-x pl-obj) view-x
	  (player.view-y pl-obj) view-y
	  (player.depth pl-obj) depth
	  (player.max-depth pl-obj) max-depth
	  (player.max-xp pl-obj) max-xp
	  (player.cur-xp pl-obj) cur-xp
	  (player.fraction-xp pl-obj) fraction-xp

	  (current-hp pl-obj) cur-hp
	  (player.fraction-hp pl-obj) fraction-hp
	  (current-mana pl-obj) cur-mana
	  (player.fraction-mana pl-obj) fraction-mana
	  (player.gold pl-obj) gold
	  (player.food pl-obj) food
	  (player.energy pl-obj) energy)
    


    (%filed-player-info pl-obj :name name :race race :class class :gender gender
			:base-stats base-stats :cur-statmods cur-statmods
			:temp-attrs temp-attrs
			:hp-table hp-table :equipment equipment :variant var-obj)

    (calculate-creature-bonuses! var-obj pl-obj)
    
    pl-obj))

(defun %filed-contained-items (&key cur-size max-size objs)
  "returns a container for items or nil."
  (let ((container (make-container max-size)))
    (loop for i in objs
	  for cnt from 0
	  do
	  (item-table-add! container i))
    (assert (= cur-size (items.cur-size container)))
;;    (describe container)
    container))

;;(trace %filed-contained-items)

(defun %filed-worn-items (&key objs)
  "returns a worn-items object or nil."
  (let* ((var-obj *variant*)
	 (eq (make-equipment-slots var-obj)))
    (loop for i in objs
	  for cnt from 0
	  do
	  (when i
	    (item-table-add! eq i cnt)))
    
    eq))


(defmethod load-object ((variant variant) (type (eql :dungeon)) (stream l-binary-stream))

  (let* ((str (lang.stream stream))
	 (depth (read-binary 'bt:s16 str))
	 (width (read-binary 'bt:u16 str))
	 (height (read-binary 'bt:u16 str)))

    (let ((dungeon (create-dungeon width height :its-depth depth)))

      (with-dungeon (dungeon (coord x y))
	(declare (ignore x y))
	(let* ((feat (read-binary 'bt:u16 str))
	       (flag (read-binary 'bt:u16 str)))
	  (setf (coord.flags coord) flag ;; flag first
		(coord-floor coord) feat ;; then floor
		)))

      (let* ((mon-len (bt:read-binary 'bt:u32 str))
	     (monsters (loop for i from 1 to mon-len
			     collecting (load-object variant :active-monster stream))))
	(when monsters
	  (setf (dungeon.monsters dungeon) monsters)
	  (%distribute-monsters! dungeon monsters)))

      (let* ((obj-len (bt:read-binary 'bt:u32 str))
	     (objs (loop for i from 1 to obj-len
			 collecting (load-object variant :active-object stream))))
	(when objs
	  ;;(setf (dungeon.objects dungeon) objs)
	  (%distribute-objects! dungeon objs)))

      (let* ((room-len (bt:read-binary 'bt:u32 str))
	     (objs (loop for i from 1 to room-len
			 collecting (load-object variant :active-room stream))))
	(when objs
	  (setf (dungeon.rooms dungeon) objs)))
      
      (let* ((dec-len (bt:read-binary 'bt:u32 str))
	     (objs (loop for i from 1 to dec-len
			 collecting (load-object variant :decor stream))))
	(when objs
	  (%distribute-decor! dungeon objs)))
      
      dungeon)))

(defmethod load-object ((variant variant) (type (eql :active-monster)) (stream l-binary-stream))

  (let* ((str (lang.stream stream))
	 (kind-id (%bin-read-string str)))
    (%filed-monster :kind kind-id :cur-hp (bt:read-binary 'bt:u16 str)
		    :max-hp (bt:read-binary 'bt:u16 str) :speed (bt:read-binary 'bt:u16 str)
		    :energy (bt:read-binary 'bt:u16 str) :mana (bt:read-binary 'bt:u16 str)
		    :loc-x (bt:read-binary 'bt:u16 str) :loc-y (bt:read-binary 'bt:u16 str)
		    :alive? (if (= 0 (bt:read-binary 'bt:u16 str))
				nil
				t))
    ))

(defmethod load-object ((variant variant) (type (eql :active-object)) (stream l-binary-stream))

  (let* ((str (lang.stream stream))
	 (kind-id (%bin-read-string str)))

    (%filed-object :kind kind-id :inscr (%bin-read-string str)
		   :number (bt:read-binary 'bt:u16 str)
		   :identify (bt:read-binary 'bt:u32 str)
		   :loc-x (bt:read-binary 'bt:u16 str)
		   :loc-y (bt:read-binary 'bt:u16 str)
		   :contains (let ((any-containment? (if (= 1 (bt:read-binary 'bt:u16 str))
							 t nil)))
			       (if any-containment?
				   (load-object variant :items-in-container stream)
				   nil))
				   
		   ;; skip events
		   )
    ))
		   

(defmethod load-object ((variant variant) (type (eql :active-room)) (stream l-binary-stream))
  (let* ((str (lang.stream stream))
	 (id (%bin-read-string str)))

    (%filed-room :type id :loc-x (bt:read-binary 'bt:u16 str)
		 :loc-y (bt:read-binary 'bt:u16 str))))



(defmethod load-object ((variant variant) (type (eql :decor)) (stream l-binary-stream))
  (let* ((str (lang.stream stream))
	 (decortype (%bin-read-string str)))

    (cond ((string-equal decortype "trap")
	   (load-object variant :active-trap stream))
	  ((string-equal decortype "door")
	   (load-object variant :active-door stream))
	  (t
	   (warn "Unknown decortype ~s found." decortype)
	   nil))
    ))

(defmethod load-object ((variant variant) (type (eql :active-door)) (stream l-binary-stream))
  (let* ((str (lang.stream stream))
	 (id (%bin-read-string str)))

    (%filed-door :type id :loc-x (bt:read-binary 'bt:u16 str)
		 :loc-y (bt:read-binary 'bt:u16 str)
		 :flags (bt:read-binary 'bt:u16 str))
    ))
  
(defmethod load-object ((variant variant) (type (eql :active-trap)) (stream l-binary-stream))
  (let* ((str (lang.stream stream))
	 (id (%bin-read-string str)))

    (%filed-trap :type id :loc-x (bt:read-binary 'bt:u16 str)
		 :loc-y (bt:read-binary 'bt:u16 str))))


(defmethod load-object ((variant variant) (type (eql :temp-creature-attribute)) (stream l-binary-stream))
  (let ((str (lang.stream stream)))
    (list :attr (%bin-read-string str) :value (bt:read-binary 'bt:s32 str)
	  :duration (bt:read-binary 'bt:u16 str))))


(defmethod load-object ((variant variant) (type (eql :player)) (stream l-binary-stream))
  (let* ((str (lang.stream stream))
	 (pl-obj (produce-player-object variant))
	 (stat-len (variant.stat-length variant)))

    ;; get basic values in
    (setf (location-x pl-obj) (read-binary 'bt:u16 str)
	  (location-y pl-obj) (read-binary 'bt:u16 str)
	  (player.view-x pl-obj) (read-binary 'bt:u16 str)
	  (player.view-y pl-obj) (read-binary 'bt:u16 str)
	  (player.depth pl-obj) (read-binary 'bt:s16 str)
	  (player.max-depth pl-obj) (read-binary 'bt:s16 str)
	  (player.max-xp pl-obj) (read-binary 'bt:u32 str)
	  (player.cur-xp pl-obj) (read-binary 'bt:u32 str)
	  (player.fraction-xp pl-obj) (read-binary 'bt:u32 str)

	  (current-hp pl-obj) (read-binary 'bt:u32 str)
	  (player.fraction-hp pl-obj) (read-binary 'bt:u32 str)
	  (current-mana pl-obj) (read-binary 'bt:u32 str)
	  (player.fraction-mana pl-obj) (read-binary 'bt:u32 str)
	  (player.gold pl-obj) (read-binary 'bt:u32 str)
	  (player.food pl-obj) (read-binary 'bt:u32 str)
	  (player.energy pl-obj) (read-binary 'bt:u16 str))


    (setf (player.misc pl-obj) (%filed-player-misc :age (read-binary 'bt:u16 str)
						   :status (read-binary 'bt:u16 str)
						   :height (read-binary 'bt:u16 str)
						   :weight (read-binary 'bt:u16 str)))
	 
    (%filed-player-info pl-obj
			:name (%bin-read-string str)
			:race  (%bin-read-string str)
			:class  (%bin-read-string str)
			:gender  (%bin-read-string str)
			:base-stats (%bin-read-array stat-len 'bt:u16 str)
			:cur-statmods (%bin-read-array stat-len 'bt:u16 str)
			:hp-table (%bin-read-array (variant.max-charlevel *variant*) 'bt:u16 str)
			:equipment (load-object variant :items-worn stream)
			:temp-attrs (loop for i from 1 to (bt:read-binary 'bt:u16 str)
					  collecting
					  (load-object variant :temp-creature-attribute stream))
			)

    
    ;; recalculate rest
    (calculate-creature-bonuses! variant pl-obj)
    ;;(warn "player loaded")
    pl-obj))

(defmethod load-object ((variant variant) (type (eql :items-in-container)) (stream l-binary-stream))
  (let* ((str (lang.stream stream))
	 (cur-size (bt:read-binary 'bt:u16 str))
	 (max-size (bt:read-binary 'bt:u16 str))
	 (objs (loop for i from 1 to cur-size
		     collecting (load-object variant :active-object stream))))
;;    (lang-warn "making bin container ~s ~s ~s" cur-size max-size objs)
    (%filed-contained-items :cur-size cur-size :max-size max-size :objs objs)))

(defmethod load-object ((variant variant) (type (eql :items-worn)) (stream l-binary-stream))
  (let* ((str (lang.stream stream))
	 (cur-size (bt:read-binary 'bt:u16 str))
	 (objs (loop for i from 1 to cur-size
		     collecting
		     (let ((is-there (bt:read-binary 'bt:u16 str)))
		       (if (= is-there 1)
			   (load-object variant :active-object stream)
			   nil)))))
    (%filed-worn-items :objs objs)))

(defmethod load-object ((variant variant) (type (eql :level)) (stream l-binary-stream))
  (let* ((str (lang.stream stream))
	 (the-id (%bin-read-string str))
	 (builder (get-level-builder the-id)))
    (unless builder
      (error "Unable to find builder for level ~s" the-id))
    (let ((*level* (funcall builder))) ;; evil hack
      (%filed-level :id the-id
		    :rating (bt:read-binary 'bt:u16 str)
		    :depth (bt:read-binary 'bt:u16 str)
		    :dungeon (load-object variant :dungeon stream))
      )))

(defmethod load-object (variant (type (eql :variant)) (stream l-binary-stream))
  (assert (eq variant nil))
  (let* ((str (lang.stream stream))
	 (var-obj (%filed-variant :id (%bin-read-string str)
				  :turn (bt:read-binary 'bt:u32 str)
				  :variant-num-version (bt:read-binary 'bt:u16 str)
				  :engine-num-version (bt:read-binary 'bt:u16 str))))

    ;; something went bad
    (unless var-obj
      (warn "Unable to load usable variant object.")
      (return-from load-object nil))
    
    (check-type *variant* variant)

    (let ((info-len (bt:read-binary 'bt:u32 str)))
      (dotimes (i info-len)
	(let* ((key (%bin-read-string str))
	       (val-type (bt:read-binary 'bt:u16 str))
	       (val (%bin-read-string str))
	       (val-obj (ecase val-type
			  (1 val)
			  (2 (read-from-string val))
			  (3 (char val 0))
			  (4 nil)
			  (5 t)
			  )))
	  (setf (get-information key :variant var-obj) val-obj))))
    

    (let ((obj-len  (bt:read-binary 'bt:u32 str)))
      (dotimes (i obj-len)
	(load-object var-obj :object-kind  stream)))
    
    (let ((mon-len  (bt:read-binary 'bt:u32 str)))
      (dotimes (i mon-len)
	(load-object var-obj :monster-kind  stream)))
    
    var-obj))

(defmethod load-object ((variant variant) (type (eql :object-kind)) (stream l-binary-stream))
  (let* ((str (lang.stream stream))
	 (id (%bin-read-string str))
	 (aware-info (bt:read-binary 'bt:s16 str))
	 (flavoured (bt:read-binary 'bt:s16 str))
	 (aware (when (= aware-info 1) t))
	 (flavour nil))

    (when (= flavoured 1)
      (setf flavour (%bin-read-string str)))
    
    (%filed-object-kind :id id :aware aware :flavour flavour)
    ))

(defmethod load-object ((variant variant) (type (eql :monster-kind)) (stream l-binary-stream))
  (let* ((str (lang.stream stream))
	 (id (%bin-read-string str))
	 (wiped-flag (bt:read-binary 'bt:s16 str)))
    
    (%filed-monster-kind :id id
			 :already-dead (cond ((= wiped-flag 0) nil)
					     ((= wiped-flag 1) t)
					     ((= wiped-flag 2) :unspec)
					     (t
					      (error "Unknown 'already-dead'-flag ~s" wiped-flag))))
    ))

(defun %read-bin-fixed-str (len str)
  (coerce (loop for i below len
		collecting (code-char (bt:read-binary 'bt:u8 str)))
	  'string))

(defmethod load-object (variant (type (eql :saveheader)) (stream l-binary-stream))
  (declare (ignore variant))
  (let* ((str (lang.stream stream))
	 (obj (make-saveheader)))

    (setf (saveheader.major obj) (bt:read-binary 'bt:u8 str)
	  (saveheader.minor obj) (bt:read-binary 'bt:u8 str)
	  (saveheader.patch obj) (bt:read-binary 'bt:u8 str)
	  (saveheader.extra obj) (bt:read-binary 'bt:u8 str)
	  (saveheader.engine-num-version obj) (bt:read-binary 'bt:u16 str)
	  (saveheader.variant-num-version obj) (bt:read-binary 'bt:u16 str)
	  (saveheader.variant-id obj) (string-right-trim '(#\Space #\Tab #\Newline) (%read-bin-fixed-str 24 str))
	  (saveheader.status obj)  (bt:read-binary 'bt:u16 str)
	  (saveheader.desc obj) (string-right-trim '(#\Space #\Tab #\Newline) (%read-bin-fixed-str 64 str))
	  (saveheader.block-num obj)(bt:read-binary 'bt:u16 str)
	  )
    obj))

(defmethod load-object (variant (type (eql :saveblock)) (stream l-binary-stream))
  (declare (ignore variant))
  (let* ((str (lang.stream stream))
	 (obj (make-saveblock)))
    (setf (saveblock.vendor-tag obj) (bt:read-binary 'bt:u32 str)
	  (saveblock.type obj) (bt:read-binary 'bt:u16 str)
	  (saveblock.version obj) (bt:read-binary 'bt:u16 str)
	  (saveblock.len obj) (bt:read-binary 'bt:u32 str)
	  (saveblock.checksum obj) (bt:read-binary 'u128 str))
    (setf (saveblock.data obj) (loop for i from 0 below (saveblock.len obj)
				     collecting (bt:read-binary 'bt:u8 str)))
    obj))

(defun load-saveheader (fname)
  "Tries to load a saveheader for the given file."
  (let ((pname (pathname fname)))
    (when (probe-file pname)
      (handler-case
	  (bt:with-binary-file (s pname :direction :input)
	    (let* ((bt:*endian* :little-endian)
		   (the-lang-stream (make-instance 'l-binary-stream :stream s))
		   (header (load-object nil :saveheader the-lang-stream)))

	      (when header
		(return-from load-saveheader header))))
	(end-of-file (co)
	  (declare (ignore co))
	  (return-from load-saveheader nil)))
      nil)))

	      
