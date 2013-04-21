(in-package :org.langband.contraband)

(define-monster-kind "copian-guard" "Imperial Guard"
  :numeric-id  13
  :x-attr (tile-file 7)
  :x-char (tile-number 5)
  :desc "He is there to make you and other citizens safe."
  :text-char #\p
  :text-attr #\r
  :alignment '<good>
  :depth 0
  :rarity 1
  :hitpoints '(5 . 8)
  :armour 20
  :speed 110
  ;;:abilities '(<bash-door> <open-door> <pick-up-item> (<random-mover> 1/4))
  :abilities '(<never-move> <never-attack>)
  :alertness 250
  :vision 10
  ;;:attacks '((<hit> :type <hurt> :damage (1 . 10)))
  ;;:treasures '((<drop-chance> 9/10))
  :gender '<male>) 
