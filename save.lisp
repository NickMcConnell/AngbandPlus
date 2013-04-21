;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.engine -*-

#|

DESC: save.lisp - saving of various parts of the game
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.engine)

(defun %bin-save-string (obj stream)
  "Tries to write a var-length string."
  (let ((len (length obj)))
    (bt:write-binary 'bt:u32 stream len)
    (dotimes (i len)
      (funcall bt:*binary-write-byte* (char-code (aref obj i)) stream))
    len))
 
(defun %bin-read-string (stream)
  "Tries to read a string from the given stream."
  (let* ((len (bt:read-binary 'bt:u32 stream))
	 (str (if (= len 0) "" (bt:read-binary-string stream :size len))))
    (values str len)))


(defun %bin-read-array (len type str)
  "Tries to read an array from the stream STR."
  (let ((arr (make-array len)))
    (dotimes (i len)
      (setf (aref arr i) (bt:read-binary type str)))
    arr))

(defun %bin-write-array (arr type str)
  "Tries to write the array ARR to the stream STR."
  (dotimes (i (length arr))
;;    (warn "writing ~s as ~s" (aref arr i) type)
    (bt:write-binary type str (aref arr i))))

(defvar +long-space-word+ "                                ")
;;(defvar *save-hanger* nil)

(defun %get-indent-str (indent)
  (subseq +long-space-word+ 0 indent))

(defmethod save-object ((variant variant) (obj variant) (stream l-readable-stream) indent)

  (let ((str (lang.stream stream))
	(ind (%get-indent-str indent)))
    
    ;; do turn-events later
    (format str "~a(setf *variant* (%filed-variant :id ~s :turn ~s))~%"
	    ind (string (variant.id obj)) (variant.turn obj))

    (loop for x being the hash-values of (variant.objects obj)
	  do (save-object variant x stream indent))
    (loop for x being the hash-values of (variant.monsters obj)
	  do (save-object variant x stream indent))
    
    nil))

(defmethod save-object ((variant variant) (obj variant) (stream l-binary-stream) indent)
  (let* ((str (lang.stream stream))
	 (obj-table (variant.objects obj))
	 (obj-len (hash-table-count obj-table))
	 (mon-table (variant.monsters obj))
	 (mon-len (hash-table-count mon-table)))

    (%bin-save-string (string (variant.id obj)) str)
    (write-binary 'u32 str (variant.turn obj))

    ;; write out all object-kinds
    (write-binary 'u32 str obj-len)
    (loop for x being the hash-values of obj-table
	  do (progn
	       #+langband-extra-checks
	       (assert (ok-object? x :context :in-game :warn-on-failure t))
	       (save-object variant x stream indent)))

    (write-binary 'u32 str mon-len)
    (loop for x being the hash-values of mon-table
	  do (progn
	       #+langband-extra-checks
	       (assert (ok-object? x :context :in-game :warn-on-failure t))
	       (save-object variant x stream indent)))
   
    
    
    
    nil))

(defmethod save-object ((variant variant) (obj object-kind) (stream l-readable-stream) indent)
  (let* ((str (lang.stream stream))
	 (ind (%get-indent-str indent))
	 (flavour-arg (object.flavour obj))
	 (flav-to-print (if flavour-arg
			    `(cons ,(car flavour-arg) ,(convert-obj (cdr flavour-arg) :letter))
			    nil)))
	    
    #+langband-extra-checks
    (assert (ok-object? obj :context :in-game :warn-on-failure t))
    (format str "~a(%filed-object-kind :id ~s :flavour ~s :aware ~s)~%"
	    ind (object.id obj) flav-to-print (object.aware obj))
    nil))
	 
	 
(defmethod save-object ((variant variant) (obj object-kind) (stream l-binary-stream) indent)
  (declare (ignore indent))
  (let* ((str (lang.stream stream))
	 (aware-num (if (object.aware obj) 1 0))
	 (flavour (object.flavour obj))
	 (flav-num (if flavour 1 0))
	 )
    
    (%bin-save-string (string (object.id obj)) str)
    (write-binary 's16 str aware-num)
    (write-binary 's16 str flav-num)
    (when flavour
      (assert (legal-flavour-obj? flavour))
      (%bin-save-string (car flavour) str)
      (write-binary 's16 str (char-code (convert-obj (cdr flavour) :letter))))


    nil))

(defmethod save-object ((variant variant) (obj monster-kind) (stream l-readable-stream) indent)
  (declare (ignore indent))
  ;; only save stuff for uniques
  nil)

(defmethod save-object ((variant variant) (obj unique-monster) (stream l-readable-stream) indent)
  ;; only save stuff for uniques
  (let* ((str (lang.stream stream))
	 (ind (%get-indent-str indent)))

    #+langband-extra-checks
    (assert (ok-object? obj :context :in-game :warn-on-failure t))
    (format str "~a(%filed-monster-kind :id ~s :already-dead ~s)~%"
	    ind (monster.id obj) (monster.already-dead obj))
    
    nil))

(defmethod save-object ((variant variant) (obj monster-kind) (stream l-binary-stream) indent)
  (declare (ignore indent))
  (let* ((str (lang.stream stream))
	 (wiped-flag (cond ((typep obj 'unique-monster)
			    (if (monster.already-dead obj) 1 0))
			   (t 2)))
	 )
    
    (%bin-save-string (string (monster.id obj)) str)
    (write-binary 's16 str wiped-flag)
    nil))

 

(defmethod save-object ((variant variant) (object dungeon) (stream l-readable-stream) indent)
  ;;(print object stream)

  (let* ((width (dungeon.width object))
	 (height (dungeon.height object))
;;	 (old-table (dungeon.table object))
	 (str (lang.stream stream))
	 (ind (%get-indent-str indent))
	 (new-arr (make-array (list width height))))
    
    (with-dungeon (object (coord x y))
      (setf (aref new-arr x y) (cons (coord.floor coord)
				     (logand (coord.flags coord) +saved-cave-flags+)
				     )))
    

    (format str "~a(%filed-dungeon :height ~a :width ~a :depth ~a ~%"
	    ind height width (dungeon.depth object))
    
    (format str "~a :table ~s ~%" ind new-arr)
    
    (let ((monsters (dungeon.monsters object)))
      (when monsters
	(format str "~a :monsters (list ~%" ind)
	(dolist (i monsters)
	  (save-object variant i stream (+ 2 indent)))
	(format str "~a )~%" ind)))

    (let ((objs (dungeon.objects object)))
      (when objs
	(format str "~a :objects (list ~%" ind)
	(dolist (i objs)
	  (save-object variant i stream (+ 2 indent)))
	(format str "~a )~%" ind)))

    (let ((rooms (dungeon.rooms object)))
      (when rooms
	(format str "~a :rooms (list ~%" ind)
	(dolist (i rooms)
	  (save-object variant i stream (+ 2 indent)))
	(format str "~a )~%" ind)))
    
    (format str "~a) ;; end dng~%" ind)
    ))


(defmethod save-object ((variant variant) (object dungeon) (stream l-binary-stream) indent)
  (let ((str (lang.stream stream)))

    (write-binary 's16 str (dungeon.depth  object))
    (write-binary 'u16 str (dungeon.width  object))
    (write-binary 'u16 str (dungeon.height object))

    (with-dungeon (object (coord x y))
      (declare (ignore x y))
      (write-binary 'u16 str (coord.floor coord))
      (write-binary 'u16 str (logand (coord.flags coord)
				     +saved-cave-flags+)))

    ;; write monsters
    (let* ((monsters (dungeon.monsters object))
	   (mon-len (length monsters)))

      (bt:write-binary 'bt:u32 str mon-len)
      (dolist (i monsters)
	(save-object variant i stream indent)))
    
    ;; write objects
    (let* ((objects (dungeon.objects object))
	   (obj-len (length objects)))
      
      (bt:write-binary 'bt:u32 str obj-len)
      (dolist (i objects)
	(save-object variant i stream indent)))

    ;; write rooms
    (let* ((rooms (dungeon.rooms object))
	   (room-len (length rooms)))
      
      (bt:write-binary 'bt:u32 str room-len)
      (dolist (i rooms)
	(save-object variant i stream indent)))

    
    ))

(defmethod save-object ((variant variant) (monster active-monster) (stream l-readable-stream) indent)
  (let ((str (lang.stream stream))
	(object monster)
	(ind (%get-indent-str indent)))
    
    (format str "~a(%filed-monster :kind ~s :cur-hp ~a :max-hp ~a :speed ~a ~%"
	    ind (monster.id (amon.kind monster))
	    (current-hp monster)
	    (maximum-hp monster)
	    (get-creature-speed monster))
    
    (format str "~a :energy ~a :mana ~a :loc-x ~a :loc-y ~a :alive? ~a) ;; end mon~%"
	    ind (get-creature-energy object)
	    (get-creature-mana object)
	    (location-x object)
	    (location-y object)
	    (creature-alive? object))
    nil))

  
(defmethod save-object ((variant variant) (object active-monster) (stream l-binary-stream) indent)
  (declare (ignore indent))
  
  (let ((str (lang.stream stream))
	(the-kind-id (monster.id (amon.kind object))))

    (assert (stringp the-kind-id))
    (%bin-save-string the-kind-id str)
    (bt:write-binary 's16 str (current-hp object))
    (bt:write-binary 'u16 str (maximum-hp object))
    (bt:write-binary 'u16 str (get-creature-speed object))
    (bt:write-binary 'u16 str (get-creature-energy object))
    (bt:write-binary 'u16 str (get-creature-mana object))
    (bt:write-binary 'u16 str (location-x object))
    (bt:write-binary 'u16 str (location-y object))
    (bt:write-binary 'u16 str (if (creature-alive? object) 1 0))
			 
    nil))

(defmethod save-object ((variant variant) (object active-object) (stream l-readable-stream) indent)

  (assert (ok-object? object :context :in-game :warn-on-failure t))
;;  (warn "saving r-object ~a" object)
  (let ((str (lang.stream stream))
	(ind (%get-indent-str indent))
	(the-kind-id (object.id (aobj.kind object)))
	(inscription (aobj.inscr object))
	(number (aobj.number object))
	(contains (aobj.contains object))
	(events (aobj.events object))
	(gvals (aobj.game-values object))
	(identify (aobj.identify object))
	(loc-x (location-x object))
	(loc-y (location-y object)))

    (check-type number integer)   
    (check-type loc-x integer)
    (check-type loc-y integer)
    (check-type identify integer)

    (assert (and (plusp number) (plusp loc-x) (plusp loc-y)))
    (assert (>= identify 0))
	    
    (format str "~a(%filed-object :kind ~s :inscr ~s :number ~s~%"
	    ind the-kind-id inscription number)

    (when contains 
      (format str "~a:contains~%" ind)
      (save-object variant contains stream (+ 2 indent)))

    (when events
      (format str "~a:events ~s" ind events))

    (when gvals
      (format str "~a:game-values ~s" ind (get-loadable-form variant gvals)))
    
    (format str "~a :identify ~s :loc-x ~s :loc-y ~s) ;; end obj~%"
	    ind identify loc-x loc-y)
    
    ))

(defmethod save-object ((variant variant) (object active-object) (stream l-binary-stream) indent)

  (assert (ok-object? object :context :in-game :warn-on-failure t))
	
;;  (warn "saving b-object ~a" object)
  (let ((str (lang.stream stream))
	(the-kind-id (object.id (aobj.kind object)))
	(inscription (aobj.inscr object))
	(number (aobj.number object))
	(contains (aobj.contains object))
;;	(events (aobj.events object))
	(identify (aobj.identify object))
	(loc-x (location-x object))
	(loc-y (location-y object)))

    (check-type number integer)
    (check-type loc-x integer)
    (check-type loc-y integer)
    (check-type identify integer)
    
    (assert (and (plusp number) (plusp loc-x) (plusp loc-y)))
    (assert (>= identify 0))
    
    (%bin-save-string the-kind-id str)

    (%bin-save-string inscription str)


    (bt:write-binary 'u16 str number)
    (bt:write-binary 'u32 str identify)
    (bt:write-binary 'u16 str loc-x)
    (bt:write-binary 'u16 str loc-y)

    
    ;; dump out a digit if we have containment
    (bt:write-binary 'u16 str (if contains 1 0))
    (when contains
      (save-object variant contains stream indent))
    
    ;; skip events


    nil))

(defmethod save-object ((variant variant) (obj active-room) (stream l-readable-stream) indent)
  
  (assert (ok-object? obj :context :in-game :warn-on-failure t))

  (let ((str (lang.stream stream))
	(ind (%get-indent-str indent)))

    (format str "~a(%filed-room :type ~s :loc-x ~s :loc-y ~s)~%"
	    ind (room-type.id (room.type obj))
	    (location-x obj) (location-y obj))
    
    nil))

(defmethod save-object ((variant variant) (obj active-room) (stream l-binary-stream) indent)
  (declare (ignore indent))
  (assert (ok-object? obj :context :in-game :warn-on-failure t))

  (let ((str (lang.stream stream))
	(loc-x (location-x obj))
	(loc-y (location-y obj))
	)

    (%bin-save-string (room-type.id (room.type obj)) str)
    (bt:write-binary 'u16 str loc-x)
    (bt:write-binary 'u16 str loc-y)
     
    nil))

(defmethod save-object ((variant variant) (obj player) (stream l-readable-stream) indent)
  (assert (ok-object? obj :context :in-game :warn-on-failure t))
  (let ((str (lang.stream stream))
	(ind (%get-indent-str indent)))
	
    (format str "~a(setf *player* (%filed-player ~%" ind)
    (format str "~a  :name ~s :race ~s :class ~s :gender ~s ~%"
	    ind (player.name obj)
	    (string (race.id (player.race obj)))
	    (string (class.id (player.class obj)))
	    (string (gender.id (player.gender obj)))
	    )
    
    (format str "~a  :base-stats ~s :cur-statmods ~s ~%"
	    ind (player.base-stats obj) (player.cur-statmods obj))
    
    (format str "~a  :hp-table ~s ~%"
	    ind (player.hp-table obj))
    
    (format str "~a  :loc-x ~s :loc-y ~s :view-x ~s :view-y ~s ~%"
	    ind (location-x obj) (location-y obj)
	    (player.view-x obj) (player.view-y obj))
    
    (format str "~a  :depth ~s :max-depth ~s :max-xp ~s :cur-xp ~s :fraction-xp ~s ~%"
	    ind (player.depth obj) (player.max-depth obj)
	    (player.max-xp obj) (player.cur-xp obj)
	    (player.fraction-xp obj))
    
    (format str "~a  :cur-hp ~s :fraction-hp ~s :cur-mana ~s :fraction-mana ~s ~%"
	    ind (current-hp obj) (player.fraction-hp obj)
	    (current-mana obj) (player.fraction-mana obj))

    (format str "~a  :gold ~s :food ~s :energy ~s ~%"
	    ind (player.gold obj) (player.food obj) (player.energy obj))
    
    
    (format str "~a  :equipment ~%" ind)
    (save-object variant (player.equipment obj) stream (+ 2 indent))
  
    (format str " ~a)) ;; end player ~%" ind)

    nil))

(defmethod save-object ((variant variant) (obj player) (stream l-binary-stream) indent)
  (let ((str (lang.stream stream)))

    ;; do numbers first:
    (bt:write-binary 'u16 str (location-x obj))
    (bt:write-binary 'u16 str (location-y obj))
    (bt:write-binary 'u16 str (player.view-x obj))
    (bt:write-binary 'u16 str (player.view-y obj))
    (bt:write-binary 's16 str (player.depth obj))
    (bt:write-binary 's16 str (player.max-depth obj))
    (bt:write-binary 'u32 str (player.max-xp obj))
    (bt:write-binary 'u32 str (player.cur-xp obj))
    (bt:write-binary 'u32 str (player.fraction-xp obj))
    (bt:write-binary 'u32 str (current-hp obj))
    (bt:write-binary 'u32 str (player.fraction-hp obj))
    (bt:write-binary 'u32 str (current-mana obj))
    (bt:write-binary 'u32 str (player.fraction-mana obj))
    (bt:write-binary 'u32 str (player.gold obj))
    (bt:write-binary 'u32 str (player.food obj))
    (bt:write-binary 'u16 str (player.energy obj))


    
;;    (bt:write-binary 'player str obj)
    ;; then do the four first ones
    (%bin-save-string (player.name obj) str)
    (%bin-save-string (string (race.id (player.race obj))) str)
    (%bin-save-string (string (class.id (player.class obj))) str)
    (%bin-save-string (string (gender.id (player.gender obj))) str)

    (%bin-write-array (player.base-stats obj) 'bt:u16 str)
    (%bin-write-array (player.cur-statmods obj) 'bt:u16 str)
    (%bin-write-array (player.hp-table obj) 'bt:u16 str)

    (save-object variant (player.equipment obj) stream indent)
    
    nil))


(defmethod save-object ((variant variant) (obj items-worn) (stream l-binary-stream) indent)
  (let ((str (lang.stream stream)))

    (bt:write-binary 'u16 str (items.cur-size obj))

    (item-table-iterate! obj #'(lambda (tbl num loc-obj)
				 (declare (ignore tbl num))
				 (if (not loc-obj)
				     (bt:write-binary 'u16 str 0)
				     (progn
				       (bt:write-binary 'u16 str 1)
				       (save-object variant loc-obj stream indent)))))
    nil))


(defmethod save-object ((variant variant) (obj items-worn) (stream l-readable-stream) indent)
  
  (let ((str (lang.stream stream))
	(ind (%get-indent-str indent)))
    
    (format str "~a(%filed-worn-items :objs (list " ind)

;;    (format str "nil")
    (flet ((save-objs (tbl num loc-obj)
	     (declare (ignore tbl num))
	     (if (not loc-obj)
		 (format str " nil ")
		 (save-object variant loc-obj stream (+ 2 indent)))))
      
      (item-table-iterate! obj #'save-objs))

    (format str "~a )) ;; end worn ~%" ind)
    
    nil))


(defmethod save-object ((variant variant) (obj items-in-container) (stream l-readable-stream) indent)
  (let ((str (lang.stream stream))
	(ind (%get-indent-str indent)))
    
    (format str "~a(%filed-contained-items :cur-size ~s :max-size ~s :objs (list "
	    ind (items.cur-size obj) (items.max-size obj))
    
    (flet ((save-objs (tbl num loc-obj)
	     (declare (ignore tbl num))
	     (if (not loc-obj)
		 (format str " nil ")
		 (save-object variant loc-obj stream (+ 2 indent)))))
      
      (item-table-iterate! obj #'save-objs))
    
    (format str "~a )) ;; end cont ~%" ind)
    
    nil))


(defmethod save-object ((variant variant) (obj items-in-container) (stream l-binary-stream) indent)
  (let ((str (lang.stream stream))
	(cur-size (items.cur-size obj))
	(max-size (items.max-size obj)))
    
    (bt:write-binary 'u16 str cur-size)
    (bt:write-binary 'u16 str max-size)

    (flet ((save-objs (tbl num loc-obj)
	     (declare (ignore tbl num))
	     (if (not loc-obj)
		 (error "NIL in the middle of a container")
		 (save-object variant loc-obj stream indent))))
      
      (item-table-iterate! obj #'save-objs))
				     
    nil))

(defmethod save-object ((variant variant) (obj level) (stream l-readable-stream) indent)
  (let ((str (lang.stream stream))
	(ind (%get-indent-str indent))
	(the-id (string (level.id obj))))

    ;; hackish
    (format str "~a(setf *level* (let* ((builder (get-level-builder ~s))~%"
	    ind the-id)
    (format str "~a       (*level* (funcall builder))) ;; evil hack~%"
	    ind)

    (format str "  ~a(%filed-level :id ~s :rating ~s :depth ~s ~%"
	    ind  the-id (level.rating obj) (level.depth obj))

    (format str "  ~a :dungeon ~%" ind)
    
    (save-object variant (level.dungeon obj) stream (+ 4 indent))
    
    (format str "  ~a))) ;; end lvl~%" ind)
    
    nil))

(defmethod save-object ((variant variant) (obj level) (stream l-binary-stream) indent)
  (let ((str (lang.stream stream)))

    (%bin-save-string (string (level.id obj)) str)
    (bt:write-binary 'u16 str (level.rating obj))
    (bt:write-binary 'u16 str (level.depth obj))
    (save-object variant (level.dungeon obj) stream indent)
    
    nil))
  

;;; === Move the ones below somewhere else later ===


(defmethod do-save ((variant variant) fname obj-list (style (eql :readable)))
  (with-open-file (s (pathname fname)
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (let ((the-lang-stream (make-instance 'l-readable-stream :stream s))
	  (*print-case* :downcase)
	  (objs (if (listp obj-list) obj-list (list obj-list))))

           
      (format s "(in-package :langband)~2%")

;;      (format s "(setf *save-hanger* nil)~2%")
      (dolist (i objs)
;;	(format s "(push ~%")
	(save-object variant i the-lang-stream 0)
	;;(format s " *save-hanger*)~2%")
	)
      )))

(defmethod do-save ((variant variant) fname obj-list (style (eql :binary)))
;;  #-cormanlisp
  (bt:with-binary-file (s (pathname fname)
			  :direction :output
			  :if-exists :supersede
			  :if-does-not-exist :create)
    
    (let ((bt:*endian* :little-endian)
	  (the-lang-stream (make-instance 'l-binary-stream :stream s))
	  (objs (if (listp obj-list) obj-list (list obj-list))))

      
      ;;      (warn "saving binary with endian ~a" bt:*endian*)
      (dolist (i objs)
	(save-object variant i the-lang-stream 0))
      
      )))

(defun %save-obj (obj fname)
  (with-open-file (s (pathname fname)
		     :direction :output
		     :if-exists :supersede
		     :if-does-not-exist :create)
    (print obj s)))

(defmethod load-a-saved-game (variant fname (style (eql :readable)))
  (declare (ignore variant))

;;  (warn "Doing load.. ")
  (let ((*variant* nil)
	(*player* nil)
	(*dungeon* nil)
	(*level* (make-instance 'level)) ;; ugly hack
	)
    
    (load fname)
    (list *variant* *player* *level*)))


(defmethod load-a-saved-game (variant fname (style (eql :binary)))
;;  #-cormanlisp
  (declare (ignore variant))
  (bt:with-binary-file (s (pathname fname)
			  :direction :input)
    
    (let ((*variant* nil)
	  (*dungeon* nil)
	  (*player* nil)
	  (*level* (make-instance 'level)) ;; hack
	  (bt:*endian* :little-endian)
	  (the-lang-stream (make-instance 'l-binary-stream :stream s))
;;	  (objs (if (listp obj-type-list) obj-type-list (list obj-type-list)))
;;	  (retval '())
	  )

      ;; first we need the variant object
      (let ((ret-obj (load-object nil :variant the-lang-stream)))
	(cond ((and ret-obj (typep ret-obj 'variant))
	       (setf *variant* ret-obj))
	      (t
	       (error "Unable to read variant-object from file ~a" fname))))

      ;; then the player comes
      (let ((ret-obj (load-object *variant* :player the-lang-stream)))
	(cond ((and ret-obj (is-player? ret-obj))
	       (setf *player* ret-obj))
	      (t
	       (error "Unable to read player-object from file ~a" fname))))
      
      ;; then we want the current level
      (let ((ret-obj (load-object *variant* :level the-lang-stream)))
	(cond ((and ret-obj (typep ret-obj 'level))
	       (setf *level* ret-obj))
	      (t
	       (error "Unable to read level-object from file ~a" fname))))
      
      
      (list *variant* *player* *level*)
      )))


(defun save-the-game (var-obj player level &key (fname *readable-save-file*) (format :readable))
  "Tries to save the game."

  (do-save var-obj fname (list var-obj player level) format)
  t)
