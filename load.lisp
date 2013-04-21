;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: load.lisp - loading of various parts of the game
Copyright (c) 2000-2001 - Stig Erik Sandø

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

(defun %filed-variant (&key id turn)

  (unless (and id (stringp id))
    (error "The saved variant-object does not have a legal id, it has ~s" id))

  (let ((var-obj (load-variant& id)))
    (if var-obj
	(setf *variant* var-obj)
	(error "Unable to find variant with id ~s" id)))
  
  (let ((var-obj *variant*)) ;; hackish
    (setf (variant.turn var-obj) turn)
    (unless (equal (string id) (string (variant.id var-obj)))
      (warn "Id of loaded variant (~s) doesn't match running variant (~s)"
	    id (variant.id var-obj)))
    var-obj))

  
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
  
(defun %filed-dungeon (&key width height depth table monsters objects rooms)
  "returns a dungeon-object or nil."
  
  (let ((dungeon (create-dungeon width height :its-depth depth)))
    (with-dungeon (dungeon (coord x y))
      (let ((old-cons (aref table x y)))
	(setf (coord.flags coord) (cdr old-cons)) ;; flag first.
	(setf (coord-feature coord) (car old-cons)) ;; then feature
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


(defun %filed-object (&key kind inscr number contains events loc-x loc-y)
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

(defun %filed-player-info (pl-obj &key name class race sex base-stats curbase-stats
			   hp-table equipment)
  "modifies the PL-OBJ with the extra info and returns the modified PL-OBJ."
  
  (let ((the-class (get-char-class class))
	(the-race (get-char-race race))
	(the-sex (find-symbol sex (find-package :langband))))

    (setf (player.name pl-obj) name)
    
    (if the-class
	(setf (player.class pl-obj) the-class)
	(error "Unable to find class ~s" class))
    (if the-race
	(setf (player.race pl-obj) the-race)
	(error "Unable to find race ~s" race))
    (if the-sex
	(setf (player.sex pl-obj) the-sex)
	(progn
	  (lang-warn "Unable to find sex ~s, assume male" sex)
	  (setf (player.sex pl-obj) '<male>)))

    (setf (player.base-stats pl-obj) base-stats
	  (player.curbase-stats pl-obj) curbase-stats)

    (unless equipment
      (error "When constructing a player, the equipment-list _must_ be kosher."))
    
    (setf (player.hp-table pl-obj) hp-table
	  (player.equipment pl-obj) equipment
	  ;; calculated
	  (player.inventory pl-obj) (item-table-find equipment 'eq.backpack))
	  
    pl-obj))
  
(defun %filed-player (&key name race class sex
		      base-stats curbase-stats hp-table equipment
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
	  (player.cur-mana pl-obj) cur-mana
	  (player.fraction-mana pl-obj) fraction-mana
	  (player.gold pl-obj) gold
	  (player.food pl-obj) food
	  (player.energy pl-obj) energy)
    


    (%filed-player-info pl-obj :name name :race race :class class :sex sex
			:base-stats base-stats :curbase-stats curbase-stats
			:hp-table hp-table :equipment equipment)
    
    (update-player! var-obj pl-obj)
    
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
  (let ((eq (make-equipment-slots)))
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
		(coord-feature coord) feat ;; then feature
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

(defmethod load-object ((variant variant) (type (eql :player)) (stream l-binary-stream))
  (let* ((str (lang.stream stream))
	 (pl-obj (produce-player-object variant)))

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
	  (player.cur-mana pl-obj) (read-binary 'bt:u32 str)
	  (player.fraction-mana pl-obj) (read-binary 'bt:u32 str)
	  (player.gold pl-obj) (read-binary 'bt:u32 str)
	  (player.food pl-obj) (read-binary 'bt:u32 str)
	  (player.energy pl-obj) (read-binary 'bt:u16 str))


	 
    (%filed-player-info pl-obj
			:name (%bin-read-string str)
			:race  (%bin-read-string str)
			:class  (%bin-read-string str)
			:sex  (%bin-read-string str)
			:base-stats (%bin-read-array +stat-length+ 'bt:u16 str)
			:curbase-stats (%bin-read-array +stat-length+ 'bt:u16 str)
			:hp-table (%bin-read-array (variant.max-charlevel *variant*) 'bt:u16 str)
			:equipment (load-object variant :items-worn stream) 
			)

    
    ;; recalculate rest
    (update-player! variant pl-obj)		
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
  (let* ((str (lang.stream stream)))
    (%filed-variant :id (%bin-read-string str)
		    :turn (bt:read-binary 'bt:u32 str))
    ))
