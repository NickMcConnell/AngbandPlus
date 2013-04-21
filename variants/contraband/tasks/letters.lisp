(in-package :org.langband.contraband)



;; crappy way to do things
(defquest deliver-letter-to-junifer ()
  :id "deliver-letter-to-junifer"
  :name "Deliver letter to Mereo Junifer"
  :desc "Deliver a sealed letter of introduction to Imperial Mereo Junifer in Mont Renuo."
  :steps 1
  ;; will be transformed into a working method
  :init (init-function (variant quest giver taker)
	   (add-to-inventory taker (get-new-object "sealed-letter-to-junifer"))
	   (setf (quest.state quest) :active)
	   quest)
  )

(defquest deliver-letter-to-ulydes ()
  :id "deliver-letter-to-ulydes"
  :name "Deliver letter to Mereo Ulydes"
  :desc "Deliver a sealed letter of introduction to Imperial Mereo Ulydes in Bartertown."
  :steps 1
  ;; will be transformed into a working method
  :init (init-function (variant quest giver taker)
	   (add-to-inventory taker (get-new-object "sealed-letter-to-ulydes"))
	   (setf (quest.state quest) :active)
	   quest)
  )

(defquest deliver-letter-to-tepesco ()
  :id "deliver-letter-to-tepesco"
  :name "Deliver letter to Consul Tepesco"
  :desc "Deliver a sealed letter of introduction to Atrocitas Consul Tepesco in Bartertown."
  :steps 1
  ;; will be transformed into a working method
  :init (init-function (variant quest giver taker)
	   (add-to-inventory taker (get-new-object "sealed-letter-to-tepesco"))
	   (setf (quest.state quest) :active)
	   quest)
  )


;; this one is a bit bigger so we'll use a full method
(defmethod finish-quest ((variant contraband) (quest deliver-letter-to-junifer) quest-taker)
  (remove-from-inventory quest-taker '(object "sealed-letter-to-junifer"))
  (remove-from-inventory quest-taker '(object "opened-letter-to-junifer"))

  (cond ((or (flag "annoyed-junifer") (flag "angry-junifer"))
	 (print-message! "Letter to Mereo Junifer removed from inventory.")
	 (alter-xp! quest-taker 30))
	(t
	 (print-message! "Letter to Mereo Junifer removed from inventory, you're given 15 florentins.")
	 (alter-xp! quest-taker 70)))

  (bit-flag-add! *update* +pl-upd-bonuses+)
  
  (setf (quest.state quest) :finished)
  (set-flag "delivered-letter-to-junifer")
  
  quest)

;; needs tweaking and dialogue-handling
(defmethod finish-quest ((variant contraband) (quest deliver-letter-to-ulydes) quest-taker)
  (remove-from-inventory quest-taker '(object "sealed-letter-to-ulydes"))
  (remove-from-inventory quest-taker '(object "opened-letter-to-ulydes"))

  (cond ((or (flag "annoyed-ulydes") (flag "angry-ulydes"))
	 (print-message! "Letter to Mereo Ulydes removed from inventory.")
	 (alter-xp! quest-taker 30))

	(t
	 (print-message! "Letter to Mereo Ulydes removed from inventory, you're given 5 florentins.")
	 (alter-xp! quest-taker 50)))


  (bit-flag-add! *update* +pl-upd-bonuses+)
  
  (setf (quest.state quest) :finished)
  (set-flag "delivered-letter-to-ulydes")
  
  quest)

(defmethod finish-quest ((variant contraband) (quest deliver-letter-to-tepesco) quest-taker)
  (remove-from-inventory quest-taker '(object "sealed-letter-to-tepesco"))
  (remove-from-inventory quest-taker '(object "opened-letter-to-tepesco"))

  (cond ((or (flag "annoyed-tepesco") (flag "angry-tepesco"))
	 (print-message! "Letter to Consul Tepesco removed from inventory.")
	 (alter-xp! quest-taker 30))

	(t
	 (print-message! "Letter to Consul Tepesco removed from inventory, you're given 15 florentins.")
	 (alter-xp! quest-taker 70)))


  (bit-flag-add! *update* +pl-upd-bonuses+)
  
  (setf (quest.state quest) :finished)
  (set-flag "delivered-letter-to-tepesco")
  
  quest)


#||
;; done with easier :init now, can also use method
(defmethod init-quest ((variant contraband) (quest deliver-letter-to-junifer) quest-giver quest-taker)
  (add-to-inventory quest-taker (get-new-object "sealed-letter-to-junifer"))
  (setf (quest.state quest) :active)
  quest)
||#

  #||
  ;; adding events
  (add-quest-event quest
		   '(on-move-to-coord 10 10) ;; when moved to 10,10 this should have effect
		   (quest-event (variant quest taker)
		     (when (has-object? taker "sealed-letter-to-junifer")
		       (finish-quest variant quest taker))))
  ||#
