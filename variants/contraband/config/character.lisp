

(in-package :org.langband.contraband)

;; all these are temporary, just to get things running!!

(define-character-race "atrocitan" "Atrocitan"
  :symbol '<atrocitan>
  :desc "You were born into a noble family in the Kingdom of Atrocitas, a mighty kingdom with 
a proud history.  The Kingdom of Atrocitas is one of the wealthiest in the world and her  
mounted knights are feared everywhere.  To the west lies the aggressive and ever-expanding 
Empire of Copia.  The Kingdom of Atrocitas has been able to stand up to the many 
invasion attempts by the Imperial Legions of Copia for centuries. 
You've been educated at the Royal Academy 
in the capital Atroburg, and will serve in the Foreign Office. 
Role difficulty: Hard."
  
  :xp-extra 0
  :hit-dice 10
  :base-age 18
  :mod-age '(1 . 10)
  :base-status 0

  ;; metric
  :m-height 170 :m-height-mod 15
  :f-height 160 :f-height-mod 15
  :m-weight 80  :m-weight-mod 20
  :f-weight 68  :f-weight-mod 15
  
  :classes t)

(define-character-race "copian" "Copian"
  :symbol '<copian>
  :desc "You're the son of a retired imperial legionary who was given a patch of land. 
Your family was not rich in any way, but you always had food on the table.  At the age of 
ten you received the great honour of attending an Imperial School for the gifted. 
After seven years in school you were recruited by the Imperial Agency for 
more training.  Joining the Imperial Agency is one of the highest honours in the Empire 
and you've worked hard to impress your teachers.  You're an imperial citizen and 
do your part to serve the Empire you owe everything.  The Empire has brought 
peace and civilisation to so many people and lands, and the might of the 
Imperial Legions is unquestioned.   Role difficulty: Moderate. " 
  :xp-extra 0
  :hit-dice 10
  :base-age 18
  :mod-age '(1 . 10)
  :base-status 0

  ;; metric
  :m-height 170 :m-height-mod 15
  :f-height 160 :f-height-mod 15
  :m-weight 80  :m-weight-mod 20
  :f-weight 68  :f-weight-mod 15
  
  :classes t)




(define-character-class "spy" "Spy"
  :symbol '<spy>
  :desc "You're a spy for your country and an expert on gathering information.  You're trained as 
a field agent and have basic combat abilities."
  
  :titles '("Newbie Spy" "Junior Spy" "Aspiring Spy"
	    "Spy" "Spy" "Spy"
	    "Spy" "Spy" "Spy" "James Bond")
  
  :starting-equipment '((obj :id "small-sword")
			(obj :id "soft-leather-armour")
			)

  ;; total points 150
  :skills '((<leatherwork> 5)
	    (<appraising> 10)
	    (<economy> 5)
	    (<conversation> 15)
	    (<monster-lore> 5)
	    (<object-lore> 5)
	    (<history> 5)
	    (<intuition> 5)
	    (<languages> 10)
	    (<pick-pocket> 15)
	    (<locksmithing> 10)
	    (<mechanical> 5)
	    (<unarmed> 5)
	    (<short-blades> 10)
	    (<bludgeoning> 5)
	    (<archery> 10)
	    (<light-armour> 10)
	    (<evasion> 15))
  )

(define-character-class "diplomat" "Diplomat"
  :symbol '<diplomat>
  :desc "You're a versatile diplomat for your country.  You're also an expert on 
gathering and analyzing information.  You have a good and broad education, making you
suitable for many tasks for the Foreign Office."
  
  :titles '("Briefcase boy" "Clerk" "Newbie Diplomat"
	    "Junior Diplomat" "Asst Diplomat" "Diplomat"
	    "Senior Diplomat" "Consul" "Ambassador" "Richelieu")
  
  :starting-equipment '((obj :id "dagger")
			(obj :id "pants-expensive")
			(obj :id "coat-expensive")
			(obj :id "shirt-expensive")
			)

  ;; total points 175
  :skills '((<appraising> 10)
	    (<economy> 15)
	    (<conversation> 20)
	    (<monster-lore> 10)
	    (<object-lore> 15)
	    (<history> 15)
	    (<intuition> 5)
	    (<languages> 20)
	    (<alchemy> 10)
	    (<magic-devices> 10)
	    (<pick-pocket> 5)
	    (<mechanical> 10)
	    (<unarmed> 5)
	    (<short-blades> 5)
	    (<long-blades> 10)
	    (<light-armour> 5)
	    (<evasion> 5))
  )

(define-character-stat '<str> "strength"
  :abbreviation "Str" :number 0 :positive-desc "strong" :negative-desc "weak")

(define-character-stat '<agi> "agility"
  :abbreviation "Agi" :number 1 :positive-desc "agile" :negative-desc "clumsy")

(define-character-stat '<con> "constitution"
  :abbreviation "Con" :number 2 :positive-desc "healthy" :negative-desc "sickly")

(define-character-stat '<dis> "discipline"
  :abbreviation "Dis" :number 3 :positive-desc "focused" :negative-desc "weak-willed")

(define-character-stat '<rea> "reasoning"
  :abbreviation "Rea" :number 4 :positive-desc "clever" :negative-desc "dumb")

(define-character-stat '<pre> "presence"
  :abbreviation "Pre" :number 5 :positive-desc "celebrity" :negative-desc "anonymous")
