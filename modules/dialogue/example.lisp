(in-package :org.langband.dialogue)

;; example
(define-conversation (player npc)
    (:id "generic-monster-info")
  (:text "")
  (:option (:text "What can you tell me about giant rats?")
	   (:node (:text "They're big, hairy and smelly.")
		  (:dest :back)))
  (:option (:text "What can you tell me about goblins?")
	   (:node (:text "Goblins? There haven't been goblins around here for hundreds of year.")
		  (:dest :back))))

(define-conversation (player npc)
    (:id "generic-human-info")
  (:text "")
  (:option (:text "It's good to see a fellow human being.")
	   (:node (:text "Is that a joke? I don't get it.")
		  (:dest :back))))

;; example, cont'd
(define-conversation (player npc)
    (:id "about-town")
  (:text "Oh, this is just another crossroad turned into a town. We get dozens of adventurers through here every day, selling loot, drinking mead and looking for work. Haven't you got anything more interesting to ask about?")
  (:dest :back "I guess I do.")
  (:quit-option "Not right now I don't. Bye"))

(define-conversation (player npc)
    (:id "alfred")
  ;; first time skip to this node
  (:skip-if
   (not (flag "met-alfred"))
   (:node (:text "[The man greets you with a grin.] Hello, stranger. I don't believe we've met. My name is Alfred.")
	  (:option (:text "[Shake his hand.] Pleased to meet you, I'm ~A." (player.name player))
		   (:perform (set-flag "met-alfred"))
		   (:dest :back))))
  (:text (if (flag "alfred-angry")
	     "[Alfred looks tense.] I'm not sure I want to talk to you."
	     (format nil "What can I do for you, ~A?" (player.name player))))
  ;; include generic options
  (:include "generic-human-info")
  ;; this option is only to make Alfred happy again
  (:option
   (:test (flag "alfred-angry"))
   (:text "I'm sorry if I offended you earlier.")
   (:node (:text "[He seems to relax a bit.] I guess that's okay.")
	  (:perform (unset-flag "alfred-angry"))
	  (:option (:text "Can you help me with something else?")
		   ;;(:dest "alfred"))
		   (:dest :start))
	  (:quit-option "See you later.")))
  ;; the rest of the options are always shown
  (:option
   (:text "What can you tell me about this place?")
   (:dest "about-town"))
  ;; more generic options
  (:include "generic-monster-info")
  
  (:option
   ;;(:test (flag "heard-of-killer-rabbit"))
   (:text "What can you tell me of a killer rabbit?")
   (:node (:text "My God, you don't actually believe those stories, do you? If you really want more information, try talking to Herman Wilkinson, he can't stop rambling about the bloody 'killer rabbit'. If you ask me, all that mead has done permanent damage to the poor guy. Now PLEASE let ut talk about something else.")
	  (:option (:text "OK then.") (:dest "alfred"))
	  (:option (:text "Can't you tell me what you have heard?")
		   (:node (:text "Aw, go away!")
			  (:perform (set-flag "alfred-angry"))
			  (:quit-option)))
	  (:quit-option "I will go see him right now, bye.")))
  (:option
   (:text "Do you know where I can find work?")
   (:node (:text "The mayor sometimes have work for adventurers.  Other than that I don't know.")
	  (:option (:text "Thank you.") (:dest "alfred"))
	  (:option (:text "Anything else I should know about this town?")
		   (:dest "about-town"))
	  (:option (:text "Where can I find the mayor?")
		   (:node (:text "Well, the blindingly obvious place would be the town hall, wouldn't it?")
			  (:option (:text "And where is the town hall?")
				   (:node (:text "Do I look like a bloody tourist guide to you? Do I? For god's sake, it's a small town, take a look around!")
					  (:perform (set-flag "alfred-angry"))
					  (:option (:text "Didn't mean to upset you. Can I ask about something else?")
						   (:dest "alfred"))
					  (:quit-option "Um... I think I'll leave you and your temper alone for a while.")))
			  (:option (:text "I suppose.") (:dest "alfred"))))))
  (:quit-option "I have to go now."))


(define-conversation (player npc)
  (:id "test1")
  (:text "Testing.")
  
  (:option (:text "Toggle flag test-flag. (~A)"
		  (if (flag "test-flag")
		      "currently set"
		    "currently unset"))
	   (:perform (set-flag "test-flag" (not (flag "test-flag"))))
	   (:dest "test1"))
  
  (:option (:text "Goto other node depending on flag")
	   (:cond ((flag "test-flag")
		   ;; normal nested node
		   (:text "Flag was set")
		   (:dest :back))
		  (:otherwise
		   ;; pointer to destination node
		   "test2")))
  )

(define-conversation (pc npc)
  (:id "test2")
  (:text "Flag was NOT set.")
  (:dest :back))
