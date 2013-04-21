(in-package :org.langband.contraband)

(defconstant +common-backpack-size+ 23)

(define-object-kind 
    "backpack" "backpack" :numeric-id 750
    :text-attr #\w :text-char #\&
    :power-lvl 3 :weight nil
    :cost 1200 :the-kind '<container>
    :on-create #'(lambda (item)
		   (let ((container (make-container +common-backpack-size+)))
		     (setf (aobj.contains item) container)
		     t))
    )

(define-object-kind "sealed-letter-to-junifer" "sealed letter for Mereo Junifer"
  :numeric-id 173
  :text-attr #\w
  :text-char #\?
  :x-attr (tile-file 10)
  :x-char (tile-number 19)
  :weight 5
  :cost 125
  :sort-value 5017
  :on-read (object-effect (dungeon player item)
	     (warn "opened letter")
	     :still-useful)
  :the-kind '<letter>) 

(define-object-kind "sealed-letter-to-ulydes" "sealed letter for Mereo Ulydes"
  :numeric-id 174
  :text-attr #\w
  :text-char #\?
  :x-attr (tile-file 10)
  :x-char (tile-number 19)
  :weight 5
  :cost 125
  :sort-value 5018
  :on-read (object-effect (dungeon player item)
	     (warn "opened letter")
	     :still-useful)
  :the-kind '<letter>) 

(define-object-kind "sealed-letter-to-tepesco" "sealed letter for Consul Tepesco"
  :numeric-id 175
  :text-attr #\w
  :text-char #\?
  :x-attr (tile-file 10)
  :x-char (tile-number 19)
  :weight 5
  :cost 125
  :sort-value 5019
  :on-read (object-effect (dungeon player item)
	     (warn "opened letter")
	     :still-useful)
  :the-kind '<letter>) 


(define-object-kind "opened-letter-to-junifer" "letter for Mereo Junifer (seal broken)"
  :numeric-id 174
  :text-attr #\w
  :text-char #\?
  :x-attr (tile-file 10)
  :x-char (tile-number 90)
  :weight 5
  :cost 125
  :sort-value 5019
  :on-read (object-effect (dungeon player item)
	     (warn "read letter")
	     :still-useful)
  :the-kind '<letter>) 

(define-object-kind "measurements-junifer" "Junifer's physical measurements"
  :numeric-id 175
  :text-attr #\w
  :text-char #\?
  :x-attr (tile-file 10)
  :x-char (tile-number 86)
  :weight 5
  :cost 125
  :sort-value 5020
  :on-read (object-effect (dungeon player item)
	     (warn "read measurements")
	     :still-useful)
  :the-kind '<letter>) 

(define-object-kind "green-silk-dress" "& green dress~ of silk"
  :numeric-id 101
  :x-attr (tile-file 42)
  :x-char (tile-number 0)
  :text-attr #\g
  :text-char #\(
  :weight 5
  :cost 50
  :sort-value 3902
  :the-kind '<body-armour>
  :game-values (make-game-values :base-ac 1 :stat-modifiers '((<pre> +2))))

(define-object-kind "facts-machine" "& atrocitan facts-machine~"
  :numeric-id 102
  :x-attr (tile-file 43)
  :x-char (tile-number 0)
  :text-attr #\u
  :text-char #\o
  :weight 5
  :cost 5000
  :sort-value 3902
  :the-kind '<weird>)
