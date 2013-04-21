;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/various.lisp - various helper-stuff that should be compiled
Copyright (c) 2000-2002 - Stig Erik Sandø

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

|#

(in-package :org.langband.vanilla)

(defvar *van-dwarf-syllables*
  '(("B" "D" "F" "G" "Gl" "H" "K" "L" "M" "N" "R" "S" "T" "Th" "V")
    ("a" "e" "i" "o" "oi" "u")
    ("bur" "fur" "gan" "gnus" "gnar" "li" "lin" "lir" "mli" "nar"
     "nus" "rin" "ran" "sin" "sil" "sur")))

(defvar *van-elf-syllables*
  '(("Al" "An" "Bal" "Bel" "Cal" "Cel" "El" "Elr" "Elv" "Eow" "Ear"
     "F" "Fal" "Fel" "Fin" "G" "Gal" "Gel" "Gl" "Is" "Lan" "Leg" "Lom" 
     "N" "Nal" "Nel"  "S" "Sal" "Sel" "T" "Tal" "Tel" "Thr" "Tin")
    ("a" "adrie" "ara" "e" "ebri" "ele" "ere" "i" "io" "ithra" "ilma"
     "il-Ga" "ili" "o" "orfi" "u" "y")
    ("l" "las" "lad" "ldor" "ldur" "linde" "lith" "mir" "n" "nd" "ndel"
     "ndil" "ndir" "nduil" "ng" "mbor" "r" "rith" "ril" "riand" "rion"
     "s" "thien" "viel" "wen" "wyn")))

(defvar *van-gnome-syllables*
  '(("Aar" "An" "Ar" "As" "C" "H" "Han" "Har" "Hel" "Iir" "J" "Jan"
     "Jar" "K" "L" "M" "Mar" "N" "Nik" "Os" "Ol" "P" "R" "S" "Sam"
     "San" "T" "Ter" "Tom" "Ul" "V" "W" "Y")
    ("a" "aa"  "ai" "e" "ei" "i" "o" "uo" "u" "uu")
    ("ron" "re" "la" "ki" "kseli" "ksi" "ku" "ja" "ta" "na" "namari"
     "neli" "nika" "nikki" "nu" "nukka" "ka" "ko" "li" "kki" "rik" "po"
     "to" "pekka" "rjaana" "rjatta" "rjukka" "la" "lla" "lli" "mo" "nni")))

(defvar *van-hobbit-syllables*
  '(("B" "Ber" "Br" "D" "Der" "Dr" "F" "Fr" "G" "H" "L" "Ler" "M"
     "Mer" "N" "P" "Pr" "Per" "R" "S" "T" "W")
    ("a" "e" "i" "ia" "o" "oi" "u")
    ("bo" "ck" "decan" "degar" "do" "doc" "go" "grin" "lba" "lbo"
     "lda" "ldo" "lla" "ll" "lo" "m" "mwise" "nac" "noc" "nwise"
     "p" "ppin" "pper" "tho" "to")))

(defvar *van-human-syllables*
  '(("Ab" "Ac" "Ad" "Af" "Agr" "Ast" "As" "Al" "Adw" "Adr"
     "Ar" "B" "Br" "C" "Cr" "Ch" "Cad" "D" "Dr" "Dw" "Ed"
     "Eth" "Et" "Er" "El" "Eow" "F" "Fr" "G" "Gr" "Gw" "Gal"
     "Gl" "H" "Ha" "Ib" "Jer" "K" "Ka" "Ked" "L" "Loth" "Lar"
     "Leg" "M" "Mir" "N" "Nyd" "Ol" "Oc" "On" "P" "Pr" "R" "Rh"
     "S" "Sev" "T" "Tr" "Th" "V" "Y" "Z" "W" "Wic")
    ("a" "ae" "au" "ao" "are" "ale" "ali" "ay" "ardo" "e" "ei"
     "ea" "eri" "era" "ela" "eli" "enda" "erra" "i" "ia" "ie"
     "ire" "ira" "ila" "ili" "ira" "igo" "o" "oa" "oi" "oe"
     "ore" "u" "y")
    ("a" "and" "b" "bwyn" "baen" "bard" "c" "ctred" "cred" "ch"
     "can" "d" "dan" "don" "der" "dric" "dfrid" "dus" "f" "g"
     "gord" "gan" "l" "li" "lgrin" "lin" "lith" "lath" "loth"
     "ld" "ldric" "ldan" "m" "mas" "mos" "mar" "mond" "n" "nydd"
     "nidd" "nnon" "nwan" "nyth" "nad" "nn" "nnor" "nd" "p" "r"
     "ron" "rd" "s" "sh" "seth" "sean" "t" "th" "tha" "tlan"
     "trem" "tram" "v" "vudd" "w" "wan" "win" "wyn" "wyr" "wyr" "wyth")))

(defvar *van-orc-syllables*
  '(("B" "Er" "G" "Gr" "H" "P" "Pr" "R" "V" "Vr" "T" "Tr" "M" "Dr")
    ("a" "i" "o" "oo" "u" "ui")
    ("dash" "dish" "dush" "gar" "gor" "gdush" "lo" "gdish" "k" "lg"
     "nak" "rag" "rbag" "rg" "rk" "ng" "nk" "rt" "ol" "urk" "shnak" "mog"
     "mak" "rak")))


(defvar *van-scroll-syllables* #1A(
				    "a" "ab" "ag" "aks" "ala" "an" "ankh" "app"
				    "arg" "arze" "ash" "aus" "ban" "bar" "bat" "bek"
				    "bie" "bin" "bit" "bjor" "blu" "bot" "bu"
				    "byt" "comp" "con" "cos" "cre" "dalf" "dan"
				    "den" "der" "doe" "dok" "eep" "el" "eng" "er" "ere" "erk"
				    "esh" "evs" "fa" "fid" "flit" "for" "fri" "fu" "gan"
				    "gar" "glen" "gop" "gre" "ha" "he" "hyd" "i"
				    "ing" "ion" "ip" "ish" "it" "ite" "iv" "jo"
				    "kho" "kli" "klis" "la" "lech" "man" "mar"
				    "me" "mi" "mic" "mik" "mon" "mung" "mur" "nag" "nej"
				    "nelg" "nep" "ner" "nes" "nis" "nih" "nin" "o"
				    "od" "ood" "org" "orn" "ox" "oxy" "pay" "pet"
				    "ple" "plu" "po" "pot" "prok" "re" "rea" "rhov"
				    "ri" "ro" "rog" "rok" "rol" "sa" "san" "sat"
				    "see" "sef" "seh" "shu" "ski" "sna" "sne" "snik"
				    "sno" "so" "sol" "sri" "sta" "sun" "ta" "tab"
				    "tem" "ther" "ti" "tox" "trol" "tue" "turs" "u"
				    "ulk" "um" "un" "uni" "ur" "val" "viv" "vly"
				    "vom" "wah" "wed" "werg" "wex" "whon" "wun" "x"
				    "yerg" "yp" "zun" "tri" "blaa"))


(defun van-make-scroll-name (&key (max-length 15))
  "Returns a string with the name of a scroll"
  (let* ((scroll-name "")
	 (scroll-syllables *van-scroll-syllables*)
	 (syl-len (length scroll-syllables)))
    (loop
     (let ((syl-num (random 2)))
       (setq scroll-name (concatenate 'string
				      scroll-name
				      (svref scroll-syllables (random syl-len))
				      ;; possible second syllable
				      (if (= syl-num 1)
					  (svref scroll-syllables (random syl-len))
					  "")
				      " "))
       ;; if long enough, return
       (when (< max-length (length scroll-name))
	 (return-from van-make-scroll-name (string-right-trim '(#\Space #\Tab #\Newline) scroll-name)))))

    ;;"<failure in scr name-gen>"
    ))

	 

(defun van-generate-scroll-flavour (variant object)
  "returns the flavour for the given object"
  
  (declare (ignore object))
  
  ;; make a name for the scroll
  (loop named naming-loop
	for name = (van-make-scroll-name)
	for hash-val = (gethash name (variant.used-scroll-names variant))
	do
	(unless hash-val
	  (setf (gethash name (variant.used-scroll-names variant)) t)
	  (return-from van-generate-scroll-flavour (cons name +term-white+)))))

(defmethod create-gold ((variant vanilla-variant) (dungeon dungeon))
 
  (let* ((gold-table (variant.gold-table variant))
	 (gold-len (length gold-table))
	 (obj-level (dungeon.depth dungeon))
	 (which-gold (- (int-/ (+ (randint (+ 2 obj-level)) 2)
			   2)
			1)))
    (when (>= which-gold gold-len)
      (setf which-gold (1- gold-len)))
    
    (let* ((gold-kind (aref gold-table which-gold))
	   (base-amount (object.cost gold-kind))
	   (amount (+ base-amount (* 8 (randint base-amount)) (randint 8))))
      
;;      (warn "Making ~s gold (~a) of kind ~s" amount (object.name gold-kind) which-gold)
      
      (create-aobj-from-kind gold-kind :amount amount :variant variant))))


(defmethod use-object! ((var vanilla-variant) dungeon player the-object &key (which-use :use))
;;  (declare (ignore var))
  (check-type the-object active-object)
  
  (let* ((okind (aobj.kind the-object))
	 (effects (object.effects okind))
	 (the-effect (find which-use effects :key #'effect-entry-type))
	 (retval :not-used))

;;    (warn "Found use-effect ~s" use-effect)

    (unless the-effect 
      (warn "Didn't find any effect for ~s" (object.id okind))
      (return-from use-object! retval))
	    
    
    (when the-effect
      (assert (and (effect-entry-p the-effect)
		   (functionp (effect-entry-fun the-effect))))
;;      (unless (compiled-function-p (effect-entry-fun the-effect))
;;	(warn "not compiled"))
      
      (setf retval (funcall (effect-entry-fun the-effect) dungeon player the-object))
      
      (ecase retval
	(:used
	 (incf (player.energy-use player) (effect-entry-energy-use the-effect)))
	(:still-useful
	 (incf (player.energy-use player) (effect-entry-energy-use the-effect)))
	;; do nothing
	(:not-used)
	(nil
	 (warn "Object-effect ~s for object ~s returned nil, fix?"
	       (effect-entry-type the-effect) the-object)
	 nil)
	))

    retval))


(defmethod need-flavour? ((var-obj vanilla-variant) (obj object-kind))          nil)

(defmethod need-flavour? ((var-obj vanilla-variant) (obj object-kind/potion))   t)
(defmethod need-flavour? ((var-obj vanilla-variant) (obj object-kind/scroll))   t)
(defmethod need-flavour? ((var-obj vanilla-variant) (obj object-kind/wand))     t)
(defmethod need-flavour? ((var-obj vanilla-variant) (obj object-kind/rod))      t)
(defmethod need-flavour? ((var-obj vanilla-variant) (obj object-kind/staff))    t)
(defmethod need-flavour? ((var-obj vanilla-variant) (obj object-kind/ring))     t)
(defmethod need-flavour? ((var-obj vanilla-variant) (obj object-kind/amulet))   t)
(defmethod need-flavour? ((var-obj vanilla-variant) (obj object-kind/mushroom)) t)

(defmethod flavour-object! ((var-obj vanilla-variant) (obj object-kind/potion))
  (%flavour-obj-kind! obj))
(defmethod flavour-object! ((var-obj vanilla-variant) (obj object-kind/mushroom))
  (%flavour-obj-kind! obj))
(defmethod flavour-object! ((var-obj vanilla-variant) (obj object-kind/scroll))
  (%flavour-obj-kind! obj))
(defmethod flavour-object! ((var-obj vanilla-variant) (obj object-kind/ring))
  (%flavour-obj-kind! obj))
(defmethod flavour-object! ((var-obj vanilla-variant) (obj object-kind/wand))
  (%flavour-obj-kind! obj))
(defmethod flavour-object! ((var-obj vanilla-variant) (obj object-kind/staff))
  (%flavour-obj-kind! obj))
(defmethod flavour-object! ((var-obj vanilla-variant) (obj object-kind/rod))
  (%flavour-obj-kind! obj))
(defmethod flavour-object! ((var-obj vanilla-variant) (obj object-kind/amulet))
  (%flavour-obj-kind! obj))

(defmethod distribute-flavours! ((var-obj vanilla-variant))
  "Allocates flavours for objects that need it."
  (let ((objects (variant.objects var-obj)))
    (loop for obj being the hash-values of objects
	  do
	  (if (need-flavour? var-obj obj)
	      (flavour-object! var-obj obj)
	      ;; add aware-bit, seems like vanilla does that
	      (setf (object.aware obj) t)))
    var-obj))



(defmethod get-price ((object active-object) (store black-market))
  #+cmu
  (declare (optimize (ext:inhibit-warnings 3)))
  #+sbcl
  (declare (optimize (sb-ext:inhibit-warnings 3)))
  (* 3 (call-next-method)))

(defmethod get-offer ((object active-object) (store black-market))
  (int-/ (get-price object store) 4)) ;; decent value, eh?

(defmethod store-generate-object ((variant vanilla-variant) (the-store black-market))
  (let* ((object-depth (+ 25 (randint 25)))
	 (level *level*)
	 (some-obj (get-active-object-by-level variant level
					       :depth object-depth))
;;	 (o-type (when some-obj (aobj.kind some-obj)))
	 )
    
    ;; possibly add magic
    (apply-magic! variant some-obj object-depth :allow-artifact nil)
    
    (when (and some-obj (plusp (get-price some-obj the-store))
	       (not (typep some-obj 'active-object/chest))) ;; hack
      some-obj)))

			     

(defmethod generate-random-name ((variant vanilla-variant) creature race)
  (declare (ignore creature))
  (let* ((the-race (cond ((symbolp race)
			  race)
			 ((typep race 'character-race)
			  (race.symbol race))
			 (t
			  (error "Unknown race-object ~s" race))))
	 (ptr (ecase the-race
	       ((<half-elf> <elf> <high-elf>) *van-elf-syllables*)
	       ((<human> <dunedan>) *van-human-syllables*)
	       (<hobbit> *van-hobbit-syllables*)
	       (<dwarf> *van-dwarf-syllables*)
	       ((<half-orc> <half-troll>) *van-orc-syllables*)
	       (<gnome> *van-gnome-syllables*))))
    

    (when (consp ptr)
      (concatenate 'string (rand-elm (first ptr)) (rand-elm (second ptr)) (rand-elm (third ptr))))
    ))

(defun van-group-chance (id mon-depth lvl-depth)
  (declare (ignore id))
  (let* ((diff (- lvl-depth mon-depth))
	 (chance (if (plusp diff)
		     (* 10 diff)
		     0)))
      
      (when (> chance 60)
	(setq chance 60))

;;      (warn "Group chance for ~a (~a) at depth ~a is ~a%" id mon-depth lvl-depth chance)
      
      (if (plusp chance)
	  (< (random 100) chance)
	  nil)))

;; seems to be original depth + 4 which is the basis for when groups appear
(defun van-novice-appears-in-group? (level mon)

  (when (typep mon 'active-monster)
    (setq mon (amon.kind mon)))

  (unless (typep mon 'monster-kind)
    (error "Unknown object ~s given to grouping-function, should be a monster."
	   mon))
  
  (when (typep mon 'unique-monster)
    (error "A unique-monster ~s should not have a grouping-function."
	   (monster.id mon)))

  (let ((mon-depth (monster.depth mon)) ;; a bit more tricky than vanilla, but gets increasingly worse
	(lvl-depth (level.depth level)))
    (van-group-chance (monster.id mon)
		      mon-depth
		      lvl-depth
		      )))


(defmethod apply-projection-effect-to-target! ((variant vanilla-variant) source (target active-monster)
					       &key
					       (x 0) (y 0) (damage 0) (effect nil) (distance 0))
;;  (declare (ignore x y damage effect distance source))

  (let* ((dungeon *dungeon*)
	 (the-monster target)
	 (the-kind (amon.kind the-monster))
	 (balanced-damage (int-/ (+ damage distance) (1+ distance)))
	 (cur-hp (current-hp the-monster))
	 (meff (make-instance 'vanilla-monster-effect :seen (amon.seen-by-player? the-monster)
			      :damage balanced-damage
			      :note nil :dying-note " dies.")))

    (let ((type (monster.type the-kind)))
      (when (or (eq type '<demon>)
		(eq type '<undead>)
		(eq type '<stupid>)) ;; fix
	(setf (meff.dying-note meff) " is destroyed.")))
    
    
	(cond ((functionp effect)
;;	       (warn "Function-effect not implemented for project-monster")
	       (let ((retval (funcall effect variant source the-monster :x x :y y :damage balanced-damage
				      :state-object meff)))
		 (when (typep retval 'vanilla-monster-effect)
		   (setf meff retval))))
	      (t
;;	       (warn "Hit monster ~s at (~s,~s) from ~s at (~s,~s) [~s]" (monster.name the-monster) loc-x loc-y
;;		     (if (typep source 'player) "player" "someone")
;;		     (location-x source) (location-y source) distance)
	       ))

	;; add skip!
	
	;; we simplify greatly here!

	(setf balanced-damage (meff.damage meff))
	
	;; uniques only killed by player
	(when (and (is-unique-monster? the-monster)
		   (is-player? source) 
		   (< cur-hp balanced-damage))
	  (setf balanced-damage cur-hp))


	(cond ((> balanced-damage cur-hp)
	       (setf (meff.note meff) (meff.dying-note meff)))
	      ;; skip polymorph
	      ;; skip teleport
	      ;; skip stun
	      ;; skip confusion
	      (t))
	;; skip fear

	(cond ((is-monster? source)
	       (warn "Monster attacked.."))
	      ((is-player? source)
	       (let ((is-dead? (deliver-damage! variant source target balanced-damage
						:dying-note (meff.dying-note meff))))
		 (unless is-dead? ;; he died
		   ;; improve message later
		   (print-message! (format nil "~a was hurt." (monster.name target)))
		   ;; skip fear
		   ;; skip sleep
		   )))
	      (t
	       (warn "Who was source?? ~s" source)))


	      
	(update-monster! variant the-monster nil)
	(light-spot! dungeon x y)

	
	;; skip window

	;, return if the object was obviously seen
	(meff.obvious meff)))


(defmethod apply-projection-effect-to-target! ((variant vanilla-variant) source (target active-object)
					       &key
					       (x 0) (y 0) (damage 0) (effect nil) (distance 0))
  (declare (ignore distance))
;;  (warn "VAN-OBJ: Applying effect ~s to ~s" effect target)
  (when (and effect (functionp effect))
    (funcall effect variant source target :x x :y y :damage damage)))



(defmethod damaged-by-element? ((variant vanilla-variant) (object active-monster) element)
  (declare (ignore element))
  t)

(defmethod damaged-by-element? ((variant vanilla-variant) (object active-object) element)
  ;; must be improved by looking not only at general type, but also at ignores and resists!
  (let* ((okind (aobj.kind object))
	 (otype (object.the-kind okind)))

    (assert (symbolp otype))
;;    (warn "Checking if ~s resists ~s" otype element)
    
    (case element
      (<cold> (member otype '(<potion> ))) ;; flask bottle
      (<electricity> (member otype '(<ring> <wand>)))
      (<fire> (member otype '(<bow> <cloak> <gloves> <boots> <book> <light-source> <staff> <scroll>))) ;; add more
      (<acid> t)
      )))

(defmethod is-spellcaster? (obj)
  (declare (ignore obj))
  nil)

(defmethod is-spellcaster? ((obj player))
  (is-spellcaster? (player.class obj)))

(defmethod is-spellcaster? ((obj spellcasting-class))
  t)

(defmethod produce-character-class ((variant vanilla-variant) id name &key spells magic-abilities &allow-other-keys)
  ;; we only do stuff if we get magic-abiltiies info, otherwise we assume he is no spell-caster
  (cond ((consp magic-abilities)
	 (let ((class-obj (make-instance 'spellcasting-class :id id :name name)))

	   (setf (class.learnt-spells class-obj) (make-array 10 :fill-pointer 0 :initial-element nil))
	   
	   ;; handle basic magic-info
	   (destructuring-bind (&key spell-stat spells-at-level) magic-abilities
	     (when (and spell-stat (symbolp spell-stat))
	       (setf (class.spell-stat class-obj) spell-stat))
	     (when (and spells-at-level (integerp spells-at-level) (plusp spells-at-level))
	       (setf (class.spells-at-level class-obj) spells-at-level)))
	   
	   ;; handle spells
	   (when (consp spells)
	     (let ((collected-spells '()))
	       (dolist (spell spells)
		 (destructuring-bind (&key id level mana fail xp) spell
		   (let ((spell-obj (make-instance 'spell-classdata)))
		     (if (and id (verify-id id))
			 (setf (spell.id spell-obj) id)
			 (error "Unable to understand spell-info for ~s for class ~s" spell id))
		     (when (and level (integerp level) (plusp level))
		       (setf (spell.level spell-obj) level))
		     (when (and mana (integerp mana) (plusp mana))
		       (setf (spell.mana spell-obj) mana))
		     (when (and fail (integerp fail) (<= 0 fail))
		       (setf (spell.failure spell-obj) fail))
		     (when (and xp (integerp xp) (<= 0 xp))
		       (setf (spell.xp spell-obj) xp))

		     ;; check if something matches the def
		     (unless (gethash id (variant.spells variant))
		       (warn "Can't find reference to spell-id ~s for class ~s" id name))
		     		     
		     (push spell-obj collected-spells))))
	       
	       (assert (plusp (length collected-spells)))
	       (let ((spell-array (make-array (length collected-spells))))
		 (loop for i from 0
		       for spell in (nreverse collected-spells)
		       do
		       (setf (aref spell-array i) spell))
		 (setf (class.spells class-obj) spell-array))))
		     
		 
	   class-obj))

	(t
	 (call-next-method))))


(defun boost-stats! (item amount)
  (let ((gvals (aobj.game-values item))
	(variant *variant*))
    (setf (gval.stat-modifiers gvals)
	  (build-stat-table-from-symlist variant
					 (loop for i in (gval.stat-modifiers gvals)
					       collecting (list i amount))))))


;; move somewhere else later
(defmethod shoot-a-missile ((dungeon dungeon) (player player)
			   (missile-weapon active-object/bow)
			   (arrow active-object/ammo))
  
;;  (declare (ignore missile-weapon))
  (block missile-shooting
    (when-bind (dir (%read-direction))
      (assert (and (numberp dir) (< dir 10)))
;;      (check-type arrow active-object)
      
;;      (warn "dir is ~s with ~s + ~s" dir missile-weapon arrow)
      (let* ((pvx (location-x player))
	     (pvy (location-y player))
	     (ddx *ddx*)
	     (ddy *ddy*)
	     (tx (+ pvx (* 99 (aref ddx dir))))
	     (ty (+ pvy (* 99 (aref ddy dir))))
	     (max-range (+ 10 (* 5 (object.multiplier (aobj.kind missile-weapon)))))
	     (path-arr (make-array (1+ max-range) :fill-pointer 0))
	     (path-len (project-path dungeon max-range path-arr pvx pvy tx ty 0))
	     (miss-attr (object.x-attr arrow))
	     (miss-char (object.x-char arrow))
	     (cur-x pvx)
	     (cur-y pvy)
	     )

	(declare (ignore path-len))

	
	(loop named follow-path
	      for g across path-arr
	      do
	      (let ((x (grid-x g))
		    (y (grid-y g)))
		(setq cur-x x
		      cur-y y)
		(unless (cave-floor-bold? dungeon x y)
		  (return-from follow-path nil))
	      
		(display-moving-object dungeon x y miss-char miss-attr)
	      
		(when-bind (monsters (cave-monsters dungeon x y))
		  (let* ((fmon (if (consp monsters) (car monsters) monsters))
			 (mon-name (get-creature-name fmon)))
		    (when (missile-hit-creature? player fmon missile-weapon arrow)
		      
		      (print-message! (format nil "The ~a was hit." mon-name))
		      (missile-inflict-damage! player fmon missile-weapon arrow)
		      (when (< (current-hp fmon) 0)
			(print-message! (format nil "The ~a died." mon-name))
			(let ((target-xp (get-xp-value fmon)))
			  (alter-xp! player (if target-xp target-xp 0)))
			(kill-target! dungeon player fmon x y)
			;; repaint spot
			(light-spot! dungeon x y))

		      (return-from follow-path nil))))
		))

	;; if it crashes in a wall, your arrow is gone.
	(when (cave-floor-bold? dungeon cur-x cur-y)
	  (item-table-add! (get-item-table dungeon player :floor :x cur-x :y cur-y)
			   arrow))

	))))

(defmethod process-world& ((variant vanilla-variant) (dungeon dungeon) (player player))
  "tries to process important world-stuff every 10 turns."

  (let ((the-turn (variant.turn variant))
	(temp-attrs (player.temp-attrs player)))

    (unless (= 0 (mod the-turn 10)) ;; every 10 turns only
      (return-from process-world& nil))

   
    ;; if in town fix lightning

    (when (plusp (dungeon.depth dungeon)) ;; in dungeon
      ;; shuffle stores
      nil)

    ;; possibly allocate new monster
    ;; possible monster-regeration

    ;; is the player poisoned?
    (when-bind (poison-level (get-attribute-value '<poisoned> temp-attrs))
      ;; damage player 1 from poison!
      (deliver-damage! variant "poison" player 1)
      nil)

    ;; has the player got any cuts?
    (let ((cuts (get-attribute-value '<cut> temp-attrs)))
      (when (plusp cuts)
	(let ((dmg (cond ((> cuts 200) 3)
			 ((> cuts 100) 2)
			 (t 1))))
	  (deliver-damage! variant "fatal wound" player dmg)
	  )))
    

    ;; check food
    (decf (player.food player))

    (let ((con-based-regen-rate (get-stat-info-value variant player '<con> :regeneration)))

    
    ;; possible regenerate
    (let ((regen-amount 197))
	  
      ;; affect regen by food
      ;; affect regen by abilities and items

      (when (< (current-mana player)
	       (maximum-mana player))
	(regenerate-mana! player regen-amount))

      ;; affected by condition

      (when (< (current-hp player)
	       (maximum-hp player))
	(regenerate-hp! player regen-amount)))
      

    ;; do timeout'ing of effects
    (loop for x being the hash-values of temp-attrs
	  do
	  (let ((old-duration (attr.duration x)))
	    (cond ((= old-duration 1)
		   (modify-creature-state! player (attr.key x) :new-value 0))
		  ((plusp old-duration)
		   (decf (attr.duration x)))
		  )
	    ))

    ;; some are modified by stats and equipment
    (let ((poison (get-attribute-value '<poisoned> temp-attrs)))
      (when poison
	(modify-creature-state! player '<poisoned>
				:subtract con-based-regen-rate)))

    (let ((cuts (get-attribute-value '<cut> temp-attrs)))
      (when (plusp cuts)
	;; add more here
	(modify-creature-state! player '<cut>
				:subtract con-based-regen-rate)))

    (let ((stun (get-attribute-value '<stun> temp-attrs)))
      (when (plusp stun)
	(modify-creature-state! player '<stun>
				:subtract con-based-regen-rate)))

    
    ;; burn fuel when needed
    (when-bind (l-s (get-light-source player))
      (unless (is-artifact? l-s)
	(let ((gvals (aobj.game-values l-s)))
	  (decf (gval.charges gvals))
	  (when (< (gval.charges gvals) 1)
	    (setf (gval.light-radius gvals) 0)))))
		 
    
    ;; drain xp
    
    ;; check for timeouts on equipment
    
    ;; recharge rods

    ;; recharge things on the ground
    
    ;; random teleport/WoR

    )))

(defun interactive-take-off-item! (dungeon player)
  (let (;;(var-obj *variant*)
        (selection (with-new-screen ()
                     (select-item dungeon player '(:equip)
                                  :prompt "Take off item: "
                                  :where :equip))))
    (cond (selection
           (let* ((the-table (get-item-table dungeon player (car selection)))
                  (removed-obj (item-table-remove! the-table (cdr selection))))
             (cond ((typep removed-obj 'active-object)
		    ;; an object was returned
		    (%put-obj-in-cnt dungeon player :backpack removed-obj)
		    (bit-flag-add! *update* +pl-upd-bonuses+ +pl-upd-mana+ +pl-upd-torch+))
		   
                   (t
                    (print-message! (format nil "Did not find selected obj ~a" selection)
				    )))))
          (t
           ;;(warn "Did not select anything.")
           ))
    ))
