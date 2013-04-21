(in-package :org.langband.contraband)

(define-conversation (player npc)
   (:id "letter-junifer")
   (:text "So you're yet another of Mangrevi's puppets, I'm glad your superiors have started to inform 
us about these things now.  Only last month we hanged a spy that had not identified himself 
correctly.  He was sneaking around our barracks.  Let me warn you now, if you're caught near military 
areas you're in so much trouble that you will beg for death.  Though, I have a few things that needs 
to be done by a sneaky little fellow like you, and it would be inappropriate to use one of my 
honourable soldiers for this.  Go tell Captain Perpetro who you are and who you work for, 
and he will inform you of your duties.")
   
   (:perform (let ((letter-quest (find-quest *variant* "deliver-letter-to-junifer")))
	       ;;(warn ">Found quest ~s" letter-quest)
	       (finish-quest *variant* letter-quest player)))
   (:quit-option "Please excuse me general, I have work to do."))
  

(define-conversation (player npc)
    (:id "mereo-junifer")

  (:skip-if (not (flag "met-junifer"))
	    (:node (:text "Who are you?")
		   (:option (:text "I am ~A, and I'm here to see Mereo Junifer." (player.name player))
			    (:perform (set-flag "annoyed-junifer")
				      (set-flag "met-junifer"))
			    (:dest "mereo-junifer"))
		   (:option (:text "I am ~A, and I'm here to see General Junifer." (player.name player))
			    (:perform (set-flag "met-junifer"))
			    (:dest "mereo-junifer"))))


  (:text "What is your business?")

  (:option (:test (has-object? player "sealed-letter-to-junifer"))
	   (:text "I have an urgent letter to Imperial Mereo Junifer." (player.name player))
	   (:perform (set-flag "annoyed-junifer"))
	   (:dest "letter-junifer"))
  (:option (:test (has-object? player "sealed-letter-to-junifer"))
	   (:text "I have a letter to General Junifer." (player.name player))
	   (:dest "letter-junifer"))
  (:option (:test (has-object? player "opened-letter-to-junifer"))
	   (:text "I have a letter to General Junifer." (player.name player))
	   (:perform (set-flag "angry-junifer"))
	   (:dest "letter-junifer"))
  (:option (:text "My name is ~A, you killed my father, prepare to die!" (player.name player))
	   (:perform (set-flag "annoyed-junifer"))
	   (:dest "mereo-junifer"))
  (:quit-option "Bye.")
  )

(define-conversation (player npc)
    (:id "captain-perpetro")
  
  (:skip-if (not (flag "met-perpetro"))
	    (:node (:text "Who are you?")
		   (:option (:text "I am Imperial Agent ~A, and General Junifer told me to talk to you." (player.name player))
			    (:test (flag "delivered-letter-to-junifer"))
			    (:perform (set-flag "work-for-perpetro")
				      (set-flag "met-perpetro"))
			    (:dest "captain-perpetro"))
		   (:option (:text "I am ~A, and I want information about Mont Renuo." (player.name player))
			    (:perform (set-flag "met-perpetro"))
			    (:dest "captain-perpetro"))))



  (:text "How can I help you?")

  (:option (:test (and (flag "delivered-letter-to-junifer") (not (flag "work-for-perpetro"))))
	   (:text "I am Imperial Agent ~A, and General Junifer told me to talk to you." (player.name player))
	   (:perform (set-flag "work-for-perpetro"))
	   (:dest "captain-perpetro"))
	   
  (:option (:test (and (flag "work-for-perpetro") (not (flag "buying-dress-for-junifer"))))
	   (:text "General Junifer told me you had a job your soldiers were unable to do.")
	   (:node (:text "Oh yes, there might be something you can do for the Empire.  We need you to travel 
to the southwest to the elven village, and meet with an elven tailour, Fossgard.  He is a sneaky bastard and 
hates the Empire, but he is a whiz with clothes and the general loves his dresses.  Buy a green dress of the 
finest silk for the General.  25 florentins to cover your expenses, and make sure you get high quality.")
		  (:perform (set-flag "buying-dress-for-junifer")
			    (let ((quest (find-quest *variant* "buy-dress-for-junifer")))
			      (init-quest *variant* quest npc player)))
		  (:quit-option "I will do that.")))
  
  (:option (:test (and (flag "buying-dress-for-junifer") (has-object? player "green-silk-dress")))
	   (:text "I got hold of the green silk dress, though it cost me 50 florentins.")
	   (:node (:text "Then you were robbed by that pointy-eared bastard.  Hand over the dress.")
		  (:option (:text "<hand over the dress, but look very unhappy>")
			   (:perform (let ((dress-quest (find-quest *variant* "buy-dress-for-junifer")))
				       (finish-quest *variant* dress-quest player)))
			   (:dest "captain-perpetro"))
		  (:quit-option "I'll keep the dress until you pay the remaining 25 florentins.")))
  
  (:option (:test (and (flag "buying-dress-for-junifer") (has-object? player "green-silk-dress")))
	   (:text "I had no problems getting the green silk dress from the elf.")
	   (:perform (let ((dress-quest (find-quest *variant* "buy-dress-for-junifer")))
		       (finish-quest *variant* dress-quest player)))
	   (:dest "captain-perpetro"))
  (:option (:text "Who is the Mereo in Mont Renuo?")
	   (:node (:text "The Imperial Mereo of Mont Renuo, or Lambda Rock as the locals call it, is General Junifer. 
She is also high commander of the Fourth and Fifth Imperial Legions.  With the high activity on the border now, she 
is very busy.")
		  (:dest "captain-perpetro")))
  (:quit-option "Thanks for your help Captain."))


(define-conversation (player npc)
    (:id "mereo-ulydes")
  (:text "How can I help you?")
  (:quit-option "I need a dead rabbit, will be back."))

(define-conversation (player npc)
    (:id "consul-tepesco")
  (:text "How can I help you?")
  (:quit-option "I need a dead mickey mouse, will be back."))

  

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

(define-conversation (player npc)
    (:id "alfred")
  (:include "generic-monster-info" "generic-human-info" "dummy")
  ;; first time skip to this node
  (:skip-if
   (not (flag "met-alfred"))
   (:node (:text "Hello, stranger. I don't believe we've met. My name is Alfred.")
	  (:option (:text "Pleased to meet you, I'm ~A." (player.name player))
		   (:perform (set-flag "met-alfred"))
		   (:dest :back))))
  (:text (if (flag "alfred-angry")
	     "I'm not sure I want to talk to you."
	     (format nil "What can I do for you, ~A?" (player.name player))))
  ;; this option is only to make Alfred happy again
  (:option
   (:test (flag "alfred-angry"))
   (:text "I'm sorry if I offended you earlier.")
   (:node (:text "I guess that's okay.")
	  (:perform (unset-flag "alfred-angry"))
	  (:option (:text "Can you help me with something else?")
		   ;;(:dest "alfred"))
		   (:dest :start))
	  (:quit-option "See you later.")))
  ;; the rest of the options are always shown
  (:option
   (:text "What can you tell me about this place?")
   (:dest "about-town"))
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

;; example, cont'd
(define-conversation (player npc)
    (:id "about-town")
  (:text "Oh, this is just another crossroad turned into a town. We get dozens of adventurers through here every day, selling loot, drinking mead and looking for work. Haven't you got anything more interesting to ask about?")
  (:dest :back "I guess I do.")
  (:quit-option "Not right now I don't. Bye"))
