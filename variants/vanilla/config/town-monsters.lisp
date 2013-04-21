(in-package :langband)

(define-monster-kind "urchin" "filthy street urchin"
  :desc "He looks squalid and thoroughly revolting."
  :symbol #\t
  :colour #\D
  :depth 0
  :rarity 2
  :hitpoints '(1 . 4)
  :armour 1
  :speed 110
  :abilities '(<open-door> <pick-up-item> (<random-mover> 1/4))
  :alertness 40
  :vision 4
  :attacks '((<touch> :type <eat-gold> :damage nil) (<beg> :type nil :damage nil))
  :sex '<male>) 

(define-monster-kind "squint-rogue" "squint eyed rogue"
  :desc "A hardy, street-wise crook that knows an easy catch when it sees one."
  :symbol #\t
  :colour #\b
  :alignment '<evil>
  :depth 0
  :rarity 1
  :hitpoints '(2 . 8)
  :armour 8
  :speed 110
  :abilities '(<bash-door> <open-door> <pick-up-item>)
  :alertness 99
  :vision 10
  :attacks '((<touch> :type <eat-item> :damage nil) (<hit> :type <hurt> :damage (1 . 6)))
  :treasures '((<drop-chance> 3/5))
  :sex '<male>) 

(define-monster-kind "singing-drunk" "singing, happy drunk"
  :desc "He makes you glad to be sober."
  :symbol #\t
  :colour #\y
  :depth 0
  :rarity 1
  :hitpoints '(2 . 3)
  :armour 1
  :speed 110
  :abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/2))
  :alertness 0
  :vision 10
  :attacks '((<beg> :type nil :damage nil))
  :treasures '((<drop-chance> 3/5) <only-drop-gold>)
  :sex '<male>) 

(define-monster-kind "aimless-merchant" "aimless looking merchant"
  :desc "The typical ponce around town, with purse jingling, and looking for more  amulets of adornment to buy."
  :symbol #\t
  :colour #\o
  :depth 0
  :rarity 1
  :hitpoints '(3 . 3)
  :armour 1
  :speed 110
  :abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/2))
  :alertness 255
  :vision 10
  :attacks '((<hit> :type <hurt> :damage (1 . 3)))
  :treasures '((<drop-chance> 3/5) <only-drop-gold>)
  :sex '<male>) 

(define-monster-kind "mean-mercenary" "mean looking mercenary"
  :desc "No job is too low for him."
  :symbol #\t
  :colour #\r
  :alignment '<evil>
  :depth 0
  :rarity 1
  :hitpoints '(5 . 8)
  :armour 20
  :speed 110
  :abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/2))
  :alertness 250
  :vision 10
  :attacks '((<hit> :type <hurt> :damage (1 . 10)))
  :treasures '((<drop-chance> 9/10))
  :sex '<male>) 

(define-monster-kind "scarred-veteran" "battle scarred veteran"
  :desc "He doesn't take to strangers kindly."
  :symbol #\t
  :colour #\B
  :depth 0
  :rarity 1
  :hitpoints '(7 . 8)
  :armour 30
  :speed 110
  :abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/2))
  :alertness 250
  :vision 10
  :attacks '((<hit> :type <hurt> :damage (2 . 6)))
  :treasures '((<drop-chance> 9/10))
  :sex '<male>) 

(define-monster-kind "scrawny-cat" "scrawny cat"
  :desc "A skinny little furball with sharp claws and a menacing look."
  :symbol #\f
  :colour #\U
  :type '(<animal>)
  :depth 0
  :rarity 3
  :hitpoints '(1 . 2)
  :armour 1
  :speed 110
  :abilities '((<random-mover> 1/4))
  :alertness 10
  :vision 30
  :attacks '((<claw> :type <hurt> :damage (1 . 1)))) 

(define-monster-kind "scruffy-dog" "scruffy little dog"
  :desc "A thin flea-ridden mutt, growling as you get close."
  :symbol #\C
  :colour #\U
  :type '(<animal>)
  :depth 0
  :rarity 3
  :hitpoints '(1 . 3)
  :armour 1
  :speed 110
  :abilities '((<random-mover> 1/4))
  :alertness 5
  :vision 20
  :attacks '((<bite> :type <hurt> :damage (1 . 1)))) 

(define-monster-kind "blub-idiot" "blubbering idiot"
  :desc "He tends to blubber a lot."
  :symbol #\t
  :colour #\W
  :depth 0
  :rarity 1
  :hitpoints '(1 . 2)
  :armour 1
  :speed 110
  :abilities '(<pick-up-item> (<random-mover> 1/4))
  :alertness 0
  :vision 6
  :attacks '((<drool> :type nil :damage nil))
  :sex '<male>) 

(define-monster-kind "boiled-wretch" "boil-covered wretch"
  :desc "Ugly doesn't begin to describe him."
  :symbol #\t
  :colour #\g
  :depth 0
  :rarity 1
  :hitpoints '(1 . 2)
  :armour 1
  :speed 110
  :abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/4))
  :alertness 0
  :vision 6
  :attacks '((<drool> :type nil :damage nil))
  :sex '<male>) 

(define-monster-kind "village-idiot" "village idiot"
  :desc "Drooling and comical, but then, what do you expect?"
  :symbol #\t
  :colour #\G
  :depth 0
  :rarity 1
  :hitpoints '(4 . 4)
  :armour 1
  :speed 120
  :abilities '(<pick-up-item> (<random-mover> 1/4))
  :alertness 0
  :vision 6
  :attacks '((<drool> :type nil :damage nil))
  :sex '<male>) 

(define-monster-kind "beggar" "pitiful looking beggar"
  :desc "You just can't help feeling sorry for him."
  :symbol #\t
  :colour #\U
  :depth 0
  :rarity 1
  :hitpoints '(1 . 4)
  :armour 1
  :speed 110
  :abilities '(<open-door> <pick-up-item> (<random-mover> 1/4))
  :alertness 40
  :vision 10
  :attacks '((<beg> :type nil :damage nil))
  :sex '<male>) 

(define-monster-kind "mangy-leper" "mangy looking leper"
  :desc "You feel it isn't safe to touch him."
  :symbol #\t
  :colour #\u
  :depth 0
  :rarity 1
  :hitpoints '(1 . 1)
  :armour 1
  :speed 110
  :abilities '(<open-door> <pick-up-item> (<random-mover> 1/4))
  :alertness 50
  :vision 10
  :attacks '((<beg> :type nil :damage nil))
  :sex '<male>) 
