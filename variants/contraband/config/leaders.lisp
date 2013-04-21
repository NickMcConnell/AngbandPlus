(in-package :org.langband.contraband)

(define-monster-kind "mereo-junifer" "Mereo Junifer"
  :numeric-id  1002
  :x-attr (tile-file 24)
  :x-char (tile-number 17)
  :desc "She's respected and loved by her soldiers."
  :text-char #\p
  :text-attr #\r
;;  :alignment '<good>
  :depth 0
  :rarity 1
  :hitpoints '(25 . 8)
  :armour 200
  :speed 120
  ;;:abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/4))
  :abilities '(<never-move> <never-attack>)
  :alertness 250
  :vision 15
  ;;:attacks '((<hit> :type <hurt> :damage (1 . 10)))
  ;;:treasures '((<drop-chance> 9/10))
  :picture '(variant-gfx "people/junifer.png")
  :gender '<female>) 

(define-monster-kind "captain-perpetro" "Captain Perpetro"
  :numeric-id  1003
  :x-attr (tile-file 14)
  :x-char (tile-number 10)
  :desc "He's in command of Renuo civil matters."
  :text-char #\p
  :text-attr #\r
;;  :alignment '<good>
  :depth 0
  :rarity 1
  :hitpoints '(25 . 8)
  :armour 200
  :speed 120
  ;;:abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/4))
  :abilities '(<never-move> <never-attack>)
  :alertness 250
  :vision 15
  ;;:attacks '((<hit> :type <hurt> :damage (1 . 10)))
  ;;:treasures '((<drop-chance> 9/10))
  :picture '(variant-gfx "people/perpetro.png")
  :gender '<male>) 

(define-monster-kind "mereo-ulydes" "Mereo Ulydes"
  :numeric-id  1002
  :x-attr (tile-file 24)
  :x-char (tile-number 2)
  :desc "He's the Imperial Mereo of Bartertown."
  :text-char #\p
  :text-attr #\r
;;  :alignment '<good>
  :depth 0
  :rarity 1
  :hitpoints '(25 . 8)
  :armour 200
  :speed 120
  ;;:abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/4))
  :abilities '(<never-move> <never-attack>)
  :alertness 250
  :vision 15
  ;;:attacks '((<hit> :type <hurt> :damage (1 . 10)))
  ;;:treasures '((<drop-chance> 9/10))
  :picture '(variant-gfx "people/ulydes.png")
  :gender '<male>) 

(define-monster-kind "consul-tepesco" "Consul Tepesco"
  :numeric-id  1002
  :x-attr (tile-file 6)
  :x-char (tile-number 38)
  :desc "He's the Atrocitan Consul of Bartertown."
  :text-char #\p
  :text-attr #\r
;;  :alignment '<good>
  :depth 0
  :rarity 1
  :hitpoints '(25 . 8)
  :armour 200
  :speed 120
  ;;:abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/4))
  :abilities '(<never-move> <never-attack>)
  :alertness 250
  :vision 15
  ;;:attacks '((<hit> :type <hurt> :damage (1 . 10)))
  ;;:treasures '((<drop-chance> 9/10))
  :picture '(variant-gfx "people/tepesco.png")
  :gender '<male>) 
