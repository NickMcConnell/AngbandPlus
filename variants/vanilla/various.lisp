;;; -*- Mode: Lisp; Syntax: Common-Lisp; Package: org.langband.vanilla -*-

#|

DESC: variants/vanilla/various.lisp - various helper-stuff that should be compiled
Copyright (c) 2000-2003 - Stig Erik Sandø

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


(defvar *van-scroll-syllables*
  #1A("a" "ab" "ag" "aks" "ala" "an" "ankh" "app" "arg" "arze" "ash" "aus" "ban" "bar" "bat" "bek"
	  "bie" "bin" "bit" "bjor" "blu" "bot" "bu" "byt" "comp" "con" "cos" "cre" "dalf" "dan"
	  "den" "der" "doe" "dok" "eep" "el" "eng" "er" "ere" "erk" "esh" "evs" "fa" "fid"
	  "flit" "for" "fri" "fu" "gan" "gar" "glen" "gop" "gre" "ha" "he" "hyd" "i" "ing"
	  "ion" "ip" "ish" "it" "ite" "iv" "jo" "kho" "kli" "klis" "la" "lech" "man" "mar"
	  "me" "mi" "mic" "mik" "mon" "mung" "mur" "nag" "nej" "nelg" "nep" "ner" "nes" "nis"
	  "nih" "nin" "o" "od" "ood" "org" "orn" "ox" "oxy" "pay" "pet" "ple" "plu" "po" "pot"
	  "prok" "re" "rea" "rhov" "ri" "ro" "rog" "rok" "rol" "sa" "san" "sat" "see" "sef"
	  "seh" "shu" "ski" "sna" "sne" "snik" "sno" "so" "sol" "sri" "sta" "sun" "ta" "tab"
	  "tem" "ther" "ti" "tox" "trol" "tue" "turs" "u" "ulk" "um" "un" "uni" "ur" "val"
	  "viv" "vly" "vom" "wah" "wed" "werg" "wex" "whon" "wun" "x" "yerg" "yp" "zun" "tri" "blaa"))


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
	  (let ((flav (make-instance 'flavour
				     :name name))
		(x-attr 0)
		(x-char 0)
		(text-attr +term-white+)
		(text-char #\?))
	    
	    ;; ultra-hack
	    (when (load-gfx-tiles?)
	      (setf x-attr (tile-file 10)
		    x-char (tile-number (+ 18 (random 4)))))
	    
	    (handle-gfx-visual flav x-attr x-char)
	    (handle-text-visual flav text-attr text-char)
	    
	    (return-from van-generate-scroll-flavour flav)
	    ))))

(defmethod create-gold ((variant vanilla-variant) (dungeon dungeon) &key originator)

  (declare (ignore originator)) ;; fix for creeping coins
  
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

    ;;(warn "Found use-effect ~s for ~s" the-effect the-object)

    (unless the-effect 
      (warn "Didn't find ~s effect for ~s" which-use (object.id okind))
      (return-from use-object! retval))
	    
    
    (when the-effect
      (assert (and (effect-entry-p the-effect)
		   (functionp (effect-entry-fun the-effect))))
;;      (unless (compiled-function-p (effect-entry-fun the-effect))
;;	(warn "not compiled"))
      
      (setf retval (funcall (effect-entry-fun the-effect) dungeon player the-object))

      (cond ((eq retval :used)
	     (incf (player.energy-use player) (effect-entry-energy-use the-effect)))
	    ((eq retval :still-useful)
	     (incf (player.energy-use player) (effect-entry-energy-use the-effect)))
	    ;; do nothing
	    ((eq retval :not-used) nil)
	    ((eq retval nil)
	     (warn "Object-effect ~s for object ~s returned nil, fix?"
		   (effect-entry-type the-effect) the-object))
	    (t
	     (error "Unknown return-value from effect: ~s" retval))))
      
    
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

(defmethod store-buys-item? ((object active-object) (store store))
  ;; hackish
  (let ((buy-value (store.buys store)))
    (cond ((eq nil buy-value)
	   buy-value)
	  
	  ((eq t buy-value)
	   buy-value)
	  
	  ((functionp buy-value)
	   (funcall buy-value object store))
	  
	  ((consp buy-value)
	   (let ((kind-type (object.the-kind (aobj.kind object))))
	     (unless (symbolp kind-type)
	       (error "Unknown the-kind ~s for object ~s" kind-type object))
	     (when (find kind-type buy-value)
	       t)))
	  
	  (t
	   (error "Uknown buy-information in store ~s" buy-value)))
    ))

(defmethod store-buys-item? ((object active-object) (store black-market))
  ;; buy everything!
  t)


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

  (let ((mon-depth (monster.power-lvl mon)) ;; a bit more tricky than vanilla, but gets increasingly worse
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
		   (format-message! "~a was hurt." (monster.name target))
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



(defun boost-stats! (item amount)
  "Boosts stat-modifiers by AMOUNT is the modifiers are not equal to 0."
  (let* ((gvals (aobj.game-values item))
	 ;;(variant *variant*)
	 (stat-table (gval.stat-modifiers gvals)))
    
    (loop for i from 0
	  for x across stat-table
	  do
	  (when (/= 0 x)
	    (setf (aref stat-table i) (+ amount x))))
    
    ;;(warn "Stat-mod by ~s for ~s is ~s" amount item stat-table)
    
    (setf (gval.stat-modifiers gvals) stat-table)
    
    item))
  




(defmethod process-world& ((variant vanilla-variant) (dungeon dungeon) (player player))
  "tries to process important world-stuff every 10 turns."

  (let ((the-turn (variant.turn variant))
	(temp-attrs (player.temp-attrs player))
	(calc-attrs (player.calc-attrs player)))

    (unless (= 0 (mod the-turn 10)) ;; every 10 turns only
      (return-from process-world& nil))

    ;;(warn "Process world at ~s" the-turn)
    (cond ((= 0 (dungeon.depth dungeon)) ;; in town
	   (let ((time (mod the-turn +van/turns-in-24hours+)))
	     (cond ((= time (variant.dawn variant))
		    (print-message! "The sun has risen.")
		    (van/town-illuminate! dungeon player 'day))
		   ((= time (variant.twilight variant))
		    (print-message! "The sun has fallen.")
		    (van/town-illuminate! dungeon player 'night)))))

	  ;; in dungeon somewhere
	  ((plusp (dungeon.depth dungeon)) ;; in dungeon
	   ;; regular vanilla updates stores ten times a day, we do 8
	   (let ((time (mod the-turn (* 3 +van/turns-in-hour+))))
	     (when (= time 0) 
	       ;;(warn "Restock..")
	       (dotimes (i 7)
		 (let ((house (get-house (1+ i))))
		   (unless (activated? house)
		     (activate-object house))
		   (when (typep house 'store)
		     (store-maintenance! variant house))
		   ;; sometimes shuffle shopkeeper, postpone that
		   ))

	       ))
	   ))

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
    
    (cond ((< (player.food player) +food-max+) ;; normal amount of food
	   ;; only every 100th turn
	   (when (= 0 (mod the-turn 100))
	     (let ((amount (* 2 (energy-for-speed player))))
	       (when (plusp (get-attribute-value '<regenerate> calc-attrs)) ;; demands more energy
		 (incf amount 30))
	       (decf amount (get-attribute-value '<slow-digest> calc-attrs)) ;; slow eater
	       (when (< amount 1)
		 (setf amount 1)) ;; always digest some
	       ;;(warn "Digest ~s" amount)
	       (alter-food! player (- (player.food player) amount)))))
	  
	  ;; we've been overeating.. digest a lot
	  (t
	   (alter-food! player (- (player.food player) 100))))

    ;; add starvation damage


    ;; possible regenerate    
    (let ((con-based-regen-rate (get-stat-info-value variant player '<con> :regeneration))
	  (regen-amount 197)
	  (food-level (player.food player)))

      (when (< food-level +food-weak+)
	(cond ((< food-level +food-starving+)
	       (setf regen-amount 0))
	      ((< food-level +food-fainting+)
	       (setf regen-amount 33))
	      (t
	       (setf regen-amount 98)))
	;; skip fainting
	)

      (let ((regen-factor (get-attribute-value '<regenerate> calc-attrs)))
	(when (plusp regen-factor)
	  (incf regen-amount (* regen-factor regen-amount))))

      (when (is-resting? player) ;; skipping search
	(setf regen-amount (* 2 regen-amount)))


      (when (< (current-mana player)
	       (maximum-mana player))
	(regenerate-mana! player regen-amount))

      ;; affected by condition
      (when (or (get-attribute-value '<poisoned> temp-attrs)
		(plusp (get-attribute-value '<stun> temp-attrs))
		(plusp (get-attribute-value '<cut> temp-attrs))
		(get-attribute-value '<paralysed> temp-attrs))
	(setf regen-amount 0))

      
      (when (< (current-hp player)
	       (maximum-hp player))
	(regenerate-hp! player regen-amount))
      

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
    (when-bind (rand-tp (get-attribute-value '<random-teleport> calc-attrs))
      (when (= (random 100) 0) ;; 1/100 chance
	(teleport-creature! dungeon player player 40)))
    
    )))



;; we override to add our own stuff
(defmethod produce-active-monster ((variant vanilla-variant) mon-type)
  
  (let ((amon (call-next-method)))

    (flet ((install-attribute (&rest args)
	     (let ((attr (apply #'make-creature-attribute args)))
	       (unless (is-legal-effect? variant (attr.key attr))
		 (warn "The attribute ~s does not seem legal" attr))
	       (add-creature-attribute amon attr))))
      
      (install-attribute "stun" '<stun> :type :temporary ;; stun has special code
			 :value 0 :default-value 0 :value-type 'integer
			 :update-fun #'%modify-leveled-effect
			 :desc "number, stun-power")
            
      (install-attribute "hasted" '<hasted> :type :temporary
			 :value nil :default-value nil
			 :update-fun #'%modify-boolean-effect
			 :desc "boolean, in vanilla hasted means +10")
      
      (install-attribute "slowed" '<slowed> :type :temporary
			 :value nil :default-value nil
			 :update-fun #'%modify-boolean-effect
			 :desc "boolean, in vanilla slowed means -10")
      
      (install-attribute "sleeping" '<sleeping> :type :temporary
			 :value 0 :default-value 0 :value-type 'integer
			 :update-fun #'%modify-leveled-effect
			 :desc "integer, how sound asleep")

      (install-attribute "confusion" '<confusion> :type :temporary
			 :value 0 :default-value 0 :value-type 'integer
			 :update-fun #'%modify-leveled-effect
			 :desc "integer, how confused")

      (install-attribute "fear" '<fear> :type :temporary
			 :value 0 :default-value 0 :value-type 'integer
			 :update-fun #'%modify-boolean-effect
			 :desc "integer, how afraid")
      
	   )
      
      amon))

(defmethod attempt-multi-creation! ((variant vanilla-variant) (obj active-object) depth)
  (declare (ignore depth))
  ;; do nothing, assume 1 as default
  nil)

(defmethod attempt-multi-creation! ((variant vanilla-variant) (obj active-object/ammo) depth)
  (declare (ignore depth))
;;  (warn "Generating ammo")
  ;; ammo is always in groups
  (setf (aobj.number obj) (roll-dice 6 7)))

(defmethod handle-mouse-click ((variant vanilla-variant) window button x y)

  (let ((num-id (window.num-id window))
	(player *player*)
	(dungeon *dungeon*))
  
    (cond ((= num-id +inv-frame+)
	   (when (eq button :left)
	     (let ((wid (window.pixel-width window))
		   (tile-wid (window.tile-width window)))
	       ;; two button-sets
	       (cond ((> x (- wid tile-wid)) ;; last tile
		      (switch-inventory-view))
		     ((> x (- wid (* 2 tile-wid))) ;; second last tile 
		      (switch-map-mode *dungeon* *player*)))
	       )))
	  ((and (= num-id *map-frame*)
		(eq button :right))
	   ;; first get panel coords, then translate to real coords
	   (let* ((loc-x (int-/ x (window.tile-width window)))
		  (loc-y (int-/ y (window.tile-height window)))
		  (rx (+ loc-x (player.view-x player)))
		  (ry (+ loc-y (player.view-y player)))
		  (tgt (%get-target dungeon rx ry)))

	     (when (is-legal-target? dungeon tgt)
	       (when (player.target player)
		 (%remove-target (player.target player)))
	       (%highlight-target dungeon tgt)
	       (setf (player.target player) tgt))

	     
	     ;;(warn "right click in square ~s ~s" loc-x loc-y)
	     ))
	  
	  (t nil))
    
    t))

(defmethod update-stuff ((variant vanilla-variant) dungeon player)
  "Updates stuff according to *UPDATE*."
  
  (when (= 0 *update*) (return-from update-stuff nil))
  
  (let ((retval nil))

    (when (bit-flag-set? *update* +pl-upd-mana+)
      (bit-flag-remove! *update* +pl-upd-mana+)
      (calculate-creature-mana! variant player)
      (setf retval t))
    
    (when (bit-flag-set? *update* +pl-upd-spells+)
      (bit-flag-remove! *update* +pl-upd-spells+)
;;      (calculate-creature-hit-points! variant player)
      (setf retval t))

    (when (call-next-method)
      (setf retval t))

    retval))
    
(defun interactive-refill-item! (dungeon player)
  "Refill some item with another item"

  (let ((source (get-light-source player))
	(select-fun nil))
	  
	

    (cond ((eq nil source)
	   (print-message! "You're not carrying any light.")
	   (return-from interactive-refill-item! nil))
	  ((equal (object.id source) "lantern")
	   (setf select-fun #'(lambda (x)
				(or (equal (object.id x) "oil-flask")
				    (equal (object.id x) "lantern")))))
	  ((equal (object.id source) "torch")
	    (setf select-fun #'(lambda (x)
				 (equal (object.id x) "torch"))))
	  (t
	   (print-message! "Your light-source cannot be refilled.")
	   (return-from interactive-refill-item! nil)))

    ;; time to use the select-fun to find the obj

    (when-bind (selection (with-frame (+query-frame+)
			    (select-item dungeon player '(:backpack :floor)
					 :prompt "Refill with: "
					 :where :backpack
					 :selection-function select-fun)))

      (let* ((var-obj *variant*)
	     (the-table (get-item-table dungeon player (car selection)))
	     (removed-obj (item-table-remove! the-table (cdr selection))))
	
	(unless removed-obj
	  (format-message! "Error: Did not find selected obj ~a" selection)
	  (return-from interactive-refill-item! nil))
	
	(when (and (typep removed-obj 'active-object)
		   (is-cursed? removed-obj))
	  (print-message! "Hmmm, it seems to be cursed.")
	  (item-table-add! the-table removed-obj) ;; put back
	  (return-from interactive-refill-item! nil))

	(check-type removed-obj active-object)
	(let ((refill-amount 0))

	  (cond ((or (equal (object.id removed-obj) "lantern")
		     (equal (object.id removed-obj) "torch")
		     (equal (object.id removed-obj) "oil-flask"))    
		 (setf refill-amount (gval.charges (aobj.game-values removed-obj))))
		(t
		 (warn "Don't know how to handle refill obj ~s" removed-obj)))

	  (warn "Got selection ~s -> ~s" removed-obj refill-amount)
	
	  (cond ((plusp refill-amount)
		 (let* ((rem-charge (gval.charges (aobj.game-values removed-obj)))
			(src-charge (gval.charges (aobj.game-values source)))
			(max-fuel (object.max-fuel (aobj.kind source)))
			(sum (+ rem-charge src-charge)))

		   (cond ((> sum max-fuel)
			  (setf rem-charge (- rem-charge (- sum max-fuel))
				src-charge max-fuel))
			 (t
			  (setf src-charge sum
				rem-charge 0)))
		   
		   (setf (gval.charges (aobj.game-values source)) src-charge
			 (gval.charges (aobj.game-values removed-obj)) rem-charge)
		   
		   (cond ((equal (object.id source) "torch")
			  (print-message! "Your torch glows brighter."))
			 ((equal (object.id source) "lantern")
			  (print-message! "You fuel your lamp."))
			 (t
			  (error "Uknown light-source ~s" source)))
		   ))
		(t
		 (format-message! "Nothing to refuel from ~a." 
				  (with-output-to-string (s)
				    (write-obj-description var-obj removed-obj s)))
		 ))

	  ;; neat hack
	  (when (and (equal (object.id removed-obj) "oil-flask")
		     (= (gval.charges (aobj.game-values removed-obj)) 0))
	    (setf removed-obj (create-aobj-from-id "empty-bottle" :variant var-obj)))
	  
	  (item-table-add! the-table removed-obj) ;; put back
	  
	  )))
    nil))

(defmethod initialise-monster-kind! ((var-obj vanilla-variant) (m-obj monster-kind) keyword-args)

  (call-next-method)

  (let ((id (monster.id m-obj)))

    (when-bind (depth (getf keyword-args :depth))
      (assert (>= depth 0))
      (setf (monster.power-lvl m-obj) depth))

    (let ((depth (getf keyword-args :depth))
	  (rarity (getf keyword-args :rarity)))

      (cond ((and depth rarity)
	     (assert (>= depth 0))
	     (assert (>= rarity 0))
	     (push (cons depth rarity) (monster.locations m-obj)))
	    ((and (eq depth nil) (eq depth rarity)))
	    (t
	     (warn "Weird depth/rarity ~s/~s for monster ~s" depth rarity id))))
  
    m-obj))

(defmethod print-tomb ((variant vanilla-variant) (player player))
  "Prints a tombstone."
  (let* ((hs (produce-high-score-object variant player))
	 (title (get-title-for-level (player.class player) (player.level player)))
	 (class-name (class.name (player.class player)))
	 (name (player.name player))
	 (max-width 31))
    
    (when (eq (get-system-type) 'gcu)
      (flet ((dump-str (str y x)
	       (put-coloured-str! +term-white+ (%centred-string str max-width) x y)))
      
	(with-open-file (s (variant-data-fname variant "dead.txt")
			   :direction :input)
	  (loop for x = (read-line s nil 'eof)
		for i from 0
		until (eq x 'eof)
		do
		(put-coloured-str! +term-white+ x 0 i)))


	(dump-str name  6 11)
	(dump-str "the"  7 11)
	(dump-str title  8 11)
	(dump-str class-name 10 11)
	(dump-str (format nil "Level: ~a" (hs-entry.level hs))
		  11 11)
	(dump-str (format nil "Xp: ~a" (hs-entry.xp hs))
		  12 11)
	(dump-str (format nil "Au: ~a" (hs-entry.gold hs))
		  13 11)
	(dump-str (format nil "Killed on level: ~a" (hs-entry.depth hs))
		  14 11)
	(dump-str (format nil "by ~a" (hs-entry.cause-of-death hs))
		  15 11)
	;; add time
	;;      (dump-str (%pretty-date-line (hs-entry.date hs)) 17 11)
    
	nil))


    (when (eq (get-system-type) 'sdl)
      (let* ((is-male? (if (eq (gender.symbol (player.gender player)) '<male>)
			   t nil))
	     (pronoun (if is-male? "he" "she"))
	     (owning (if is-male? "his" "her"))
	     (text (format nil
			   #.(concatenate 'string "And so it has come to pass, ~a the ~a ~a has died.  "
					  "The might of Morgoth's armies has so far proven too strong "
					  "for ~a and ~a many ancestors. "
					  "Only ~a winters old, but already of rank ~a, ~a fought the "
					  "evils of Angband valiantly.  Ancestors mourn the loss of yet "
					  "another hapless adventurer.  ~a was killed "
					  "by a ~a.  The main claim to fame was ~a brutal killing of "
					  "innocent townspeople, and ~a will not be missed.")
			   name title class-name
			   name
			   owning
			   17 (hs-entry.level hs) pronoun
			   name
			   (hs-entry.cause-of-death hs) owning pronoun)))
	(print-text! 10 27 +term-white+ text
		     :end-col (- (get-frame-width +full-frame+) 10))))

    t))

(defun %input-one-key (str &key (x 0) (y 0))
  (flush-messages! t)
  (put-coloured-line! +term-white+ str x y)
  (let ((retval (read-one-character)))
    (if (eql retval #\Escape)
	nil
	retval)))

(defvar *van/ident-syms*
  #(
    (#\Space  "A dark grid" :floor) ;; #x20 #d32
    (#\! "A potion (or oil)" :object)
    (#\" "An amulet (or necklace)" :object)
    (#\# "A wall (or secret door)" :floor)
    (#\$ "Treasure (gold or gems)" :object)
    (#\% "A vein (magma or quartz)" :floor)
    (#\& "Pile of items" :object)
    (#\' "An open door" :decor)
    (#\( "Soft armor" :object)
    (#\) "A shield" :object)
    (#\* "A vein with treasure" :floor)
    (#\+ "A closed door" :decor)
    (#\, "Food (or mushroom patch)" :object)
    (#\- "A wand (or rod)" :object)
    (#\. "Floor" :floor)
    (#\/ "A polearm (Axe/Pike/etc)" :object)
    nil
    (#\1 "Entrance to General Store" :floor)
    (#\2 "Entrance to Armory" :floor)
    (#\3 "Entrance to Weaponsmith" :floor)
    (#\4 "Entrance to Temple" :floor)
    (#\5 "Entrance to Alchemy shop" :floor)
    (#\6 "Entrance to Magic store" :floor)
    (#\7 "Entrance to Black Market" :floor)
    (#\8 "Entrance to your home" :floor)
    nil
    (#\: "Rubble" :decor)
    (#\; "A glyph of warding" :decor)
    (#\< "An up staircase" :floor)
    (#\= "A ring" :object)
    (#\> "A down staircase" :floor)
    (#\? "A scroll" :object)
    (#\@ "You" :monster) 
    (#\A "Angel" :monster)
    (#\B "Bird" :monster)
    (#\C "Canine" :monster)
    (#\D "Ancient Dragon/Wyrm" :monster)
    (#\E "Elemental" :monster)
    (#\F "Dragon Fly" :monster)
    (#\G "Ghost" :monster)
    (#\H "Hybrid" :monster)
    (#\I "Insect" :monster)
    (#\J "Snake" :monster)
    (#\K "Killer Beetle" :monster)
    (#\L "Lich" :monster)
    (#\M "Multi-Headed Reptile" :monster)
    nil
    (#\O "Ogre" :monster)
    (#\P "Giant Humanoid" :monster)
    (#\Q "Quylthulg (Pulsing Flesh Mound)" :monster)
    (#\R "Reptile/Amphibian" :monster)
    (#\S "Spider/Scorpion/Tick" :monster)
    (#\T "Troll" :monster)
    (#\U "Major Demon" :monster)
    (#\V "Vampire" :monster)
    (#\W "Wight/Wraith/etc" :monster)
    (#\X "Xorn/Xaren/etc" :monster)
    (#\Y "Yeti" :monster)
    (#\Z "Zephyr Hound" :monster)
    (#\[ "Hard armor" :object)
    (#\\ "A hafted weapon (mace/whip/etc)" :object)
    (#\] "Misc. armor" :object)
    (#\^ "A trap" :decor)
    (#\_ "A staff" :object)
    nil
    (#\a "Ant" :monster)
    (#\b "Bat" :monster)
    (#\c "Centipede" :monster)
    (#\d "Dragon" :monster)
    (#\e "Floating Eye" :monster)
    (#\f "Feline" :monster)
    (#\g "Golem" :monster)
    (#\h "Hobbit/Elf/Dwarf" :monster)
    (#\i "Icky Thing" :monster)
    (#\j "Jelly" :monster)
    (#\k "Kobold" :monster)
    (#\l "Louse" :monster)
    (#\m "Mold" :monster)
    (#\n "Naga" :monster)
    (#\o "Orc" :monster)
    (#\p "Person/Human" :monster)
    (#\q "Quadruped" :monster)
    (#\r "Rodent" :monster)
    (#\s "Skeleton" :monster)
    (#\t "Townsperson" :monster)
    (#\u "Minor Demon" :monster)
    (#\v "Vortex" :monster)
    (#\w "Worm/Worm-Mass" :monster)
    nil
    (#\y "Yeek" :monster)
    (#\z "Zombie/Mummy" :monster)
    (#\{ "A missile (arrow/bolt/shot)" :object)
    (#\| "An edged weapon (sword/dagger/etc)" :object)
    (#\} "A launcher (bow/crossbow/sling)" :object)
    (#\~ "A tool (or miscellaneous item)" :object)))

(defun display-monster-recall (variant player mon)
  "Displays recall for given monster-knowledge to *cur-win* starting from row 1."
  (declare (ignore player))
  (let ((kind (get-monster-kind variant (monster.id mon)))
	(num (monster.killed mon)))
    (put-coloured-line! +term-yellow+ (monster.name kind) 2 1)
    (put-coloured-line! +term-l-blue+
			(cond ((> num 0)
			       (format nil "You have killed at least ~d of these creatures."
				       num))
			      (t
			       "No battles to death"))
			2 2)
  
    (clear-window-from *cur-win* 4)
    (when-bind (flags (monster.flags mon))
      (print-text! 2 4 +term-white+ (format nil "~a" flags)))
    ))
  
(defun display-object-recall (variant player obj)
  "Displays recall for given object-knowledge to *cur-win* starting from row 1."
  (declare (ignore player))
  (let ((kind (get-object-kind variant (object.id obj))))
    (put-coloured-line! +term-yellow+ (object.name kind) 2 1)
    
    (when-bind (flags (object.flags obj))
      (let ((row 3))
	(dolist (flag flags)
	  (case flag
	    (<free-action> (setf row (print-text! 2 row +term-white+ "Allows you to move freely")))
	    (t (setf row (print-text! 2 row +term-white+ (format nil "~s" flag))))))
	))))

(defun interactive-identify-symbol (variant player)

  (let ((cmd (%input-one-key "Enter character to be identified: "))
	(what nil)
	(mon-knowledge (player.monster-knowledge player)))

    (unless cmd
      (return-from interactive-identify-symbol nil))

    (let ((code (char-code cmd)))
      (when (and (>= code #.(char-code #\Space))
		 (<= code #.(char-code #\~)))
	(setf what (aref *van/ident-syms* (- code #.(char-code #\Space))))))

    (unless what
      (return-from interactive-identify-symbol nil))
    
    (put-coloured-line! +term-white+ (second what) 0 0)

    (unless (plusp (hash-table-count mon-knowledge))
      (pause-last-line!)
      (return-from interactive-identify-symbol nil))

    
    (put-coloured-str! +term-white+ "Recall details? (y/n): " 40 0)
    
    (let ((listing (read-one-character)))
      (unless (or (eql listing #\y)
		  (eql listing #\Y))
	(return-from interactive-identify-symbol nil)))
      
    (loop named check-monsters
	  for v being the hash-values of mon-knowledge
	  do
	  (progn
	    ;; add check here
	    (display-monster-recall variant player v)
	    (let ((read-val (read-one-character)))
	      (unless read-val
		(return-from interactive-identify-symbol t)))))
      
    t))

#-langband-release
(defmethod on-move-to-coord ((variant vanilla-variant) (player player) x y)

  (with-frame (+charinfo-frame+)
    (let ((row (- (window.height *cur-win*) 3)))
      
      (put-coloured-line! +term-l-blue+ (format nil "~3d,~3d" x y)
			  0 row)
      
      (put-coloured-line! +term-l-blue+ (format nil "~12d" (variant.turn variant)) 0 (1+ row))

      
      (let* ((time (mod (variant.turn variant) +van/turns-in-24hours+))
	     (hour (int-/ time +van/turns-in-hour+))
	     (minute (int-/ (- time (* hour +van/turns-in-hour+)) +van/turns-in-minute+))
	     (am (< time (/ +van/turns-in-24hours+ 2))))
	(put-coloured-line! +term-l-blue+ (format nil "~2,'0d:~2,'0d ~a" hour minute (if am "am" "pm"))
			    0 (+ 2 row)))
      
      ))
  
  player)
