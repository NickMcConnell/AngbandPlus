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


(defmethod use-object! ((var vanilla-variant) dun pl the-object &key (which-use :use))
;;  (declare (ignore var))
  (check-type the-object active-object)
  
  (let* ((okind (aobj.kind the-object))
	 (effects (object.effects okind))
	 (the-effect (find which-use effects :key #'effect-entry-type))
	 (retval :not-used))

;;    (warn "Found use-effect ~s" use-effect)

    (unless the-effect 
      (warn "Didn't find any effect for ~s ~s" (object.id okind) (object.obj-type okind)))
    
    (when the-effect
      (assert (and (effect-entry-p the-effect)
		   (functionp (effect-entry-fun the-effect))))
;;      (unless (compiled-function-p (effect-entry-fun the-effect))
;;	(warn "not compiled"))
      (setf retval (funcall (effect-entry-fun the-effect) dun pl the-object))
      (ecase retval
	(:used
	 (incf (player.energy-use pl) (effect-entry-energy-use the-effect)))
	(:still-useful
	 (incf (player.energy-use pl) (effect-entry-energy-use the-effect)))
	(:not-used
	 )))

    retval))

(defun %van-sort-obj-types (obj-types)
  (sort obj-types #'string< :key #'symbol-name))


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
  (* 3 (call-next-method)))

(defmethod get-offer ((object active-object) (store black-market))
  (int-/ (get-price object store) 4)) ;; decent value, eh?

(defmethod store-generate-object ((variant vanilla-variant) (the-store black-market))
    (let* (
	   (level *level*)
	   (some-obj (get-active-object-by-level variant level
						 :depth (+ 25 (randint 25))))
	   (o-type (when some-obj (aobj.kind some-obj))))

    (when (and some-obj (plusp (get-price some-obj the-store))
	       (not (obj-is? o-type '<chest>))) ;; hack
      some-obj)))

(defun teleport-creature! (dun pl creature range)
  (assert (numberp range))

  (let* ((minimum (floor range))
	 (cx (location-x creature))
	 (cy (location-y creature))
	 (tx cx)
	 (ty cy)
	 (cur-d range))
    (block find-grid
      (loop
       (when (> range 200)
	 (setf range 200))
       
       (block legal-dist
	 (dotimes (i 500)
	   (setf tx (rand-spread cx range)
		 ty (rand-spread cy range))
	   (setf cur-d (distance cx cy tx ty))
	   (when (and (>= cur-d minimum) (<= cur-d minimum))
	     (return-from legal-dist))))
       
       (when (and (in-bounds-fully? dun tx ty)
		  (cave-boldly-naked? dun tx ty)
		  (not (cave-icky? dun tx ty)))
	 (return-from find-grid))
       
       (setf range (* 2 range)
	     minimum (floor minimum 2))))

    ;; we found an ok spot!
    (assert (and (in-bounds-fully? dun tx ty)
		 (cave-boldly-naked? dun tx ty)
		 (not (cave-icky? dun tx ty))))

    ;; sound

    ;; swap monster
    (swap-monsters! dun pl cx cy tx ty)
#||    
    (warn "UPD: ~s (~s ~s ~a)  -> (~s ~s ~a), ~s"
	  *update* cx cy (multiple-value-bind (a b) (map-info dun cx cy) b)
	  (location-x pl) (location-y pl) (multiple-value-bind (a b) (map-info dun (location-x pl) (location-y pl)) b)
	  (distance cx cy tx ty))
    ||#
;;    (handle-stuff dun pl) ;; hack

;;    (print-map dun pl)
    ))

				     

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


#||
(defmethod ok-object? ((obj object-kind/staff) &key context warn-on-failure)

  (when (eq context :in-game)
    (%ok-check (legal-flavour-obj? (object.flavour obj))))
  
  (call-next-method))

(defmethod ok-object? ((obj object-kind/rod) &key context warn-on-failure)
  
  (when (eq context :in-game)
    (%ok-check (legal-flavour-obj? (object.flavour obj))))
  
  (call-next-method))

(defmethod ok-object? ((obj object-kind/wand) &key context warn-on-failure)
  
  (when (eq context :in-game)
    (%ok-check (legal-flavour-obj? (object.flavour obj))))
  
  (call-next-method))

(defmethod ok-object? ((obj object-kind/potion) &key context warn-on-failure)
  
  (when (eq context :in-game)
    (%ok-check (legal-flavour-obj? (object.flavour obj))))
  
  (call-next-method))

(defmethod ok-object? ((obj object-kind/mushroom) &key context warn-on-failure)
  
  (when (eq context :in-game)
    (%ok-check (legal-flavour-obj? (object.flavour obj))))
  (call-next-method))

(defmethod ok-object? ((obj object-kind/scroll) &key context warn-on-failure)
  
  (when (eq context :in-game)
    (%ok-check (legal-flavour-obj? (object.flavour obj))))

  (call-next-method))

(defmethod ok-object? ((obj object-kind/amulet) &key context warn-on-failure)
  
  (when (eq context :in-game)
    (%ok-check (legal-flavour-obj? (object.flavour obj))))

  (call-next-method))

(defmethod ok-object? ((obj object-kind/ring) &key context warn-on-failure)
  
  (when (eq context :in-game)
    (%ok-check (legal-flavour-obj? (object.flavour obj))))
  (call-next-method))
||#
