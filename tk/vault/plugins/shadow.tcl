proc ShadowAdd {oop} {

	set vaultId [Info $oop display,vaultId]

	ToUndo $oop

	scan [Info $oop select,bounds] "%d %d %d %d" y1 x1 y2 x2

	set y3 [expr {$y2 + 1}]
	set x3 [expr {$x2 + 1}]

	# Bottom
	ShadowAux $oop $vaultId "edging3 71" $y3 $x1
	for {set x [expr {$x1 + 1}]} {$x <= $x2} {incr x} {
		ShadowAux $oop $vaultId "edging3 70" $y3 $x
	}

	# Right
	ShadowAux $oop $vaultId "edging3 67" $y1 $x3
	for {set y [expr {$y1 + 1}]} {$y <= $y2} {incr y} {
		ShadowAux $oop $vaultId "edging3 68" $y $x3
	}

	# Bottom-right corner
	ShadowAux $oop $vaultId "edging3 69" $y3 $x3

	return
}

proc ShadowAux {oop vaultId icon y x} {

	set height [vault height $vaultId]
	set width [vault width $vaultId]

	if {$y >= $height || $x >= $width} return

	# Add the shadow to the first empty layer
	foreach layer {2 3 4} {
		set plane icon$layer
		if {[vault get $vaultId $plane $y $x] eq "none 0"} {
			vault put $vaultId $plane [list [list $icon]] $y $x
			break
		}
	}

	return
}

proc ShadowRemove {oop} {

	set vaultId [Info $oop display,vaultId]

	ToUndo $oop

	scan [Info $oop select,bounds] "%d %d %d %d" y1 x1 y2 x2

	for {set y $y1} {$y <= $y2} {incr y} {
		for {set x $x1} {$x <= $x2} {incr x} {
			foreach plane {icon2 icon3 icon4} {
				set icon [vault get $vaultId $plane $y $x]
				scan $icon "%s %d" type index
				if {$type eq "edging3" && $index >= 67 && $index <= 71} {
					vault put $vaultId $plane [list [list "none 0"]] $y $x
				}
			}
		}
	}

	return
}

