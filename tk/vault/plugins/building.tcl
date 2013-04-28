proc BldgRed {oop} {

	variable Bldg

	array unset Bldg
	set Bldg(nw) "edging2 27"
	set Bldg(n) "edging2 28"
	set Bldg(ne) "edging2 29"
	set Bldg(w) "edging2 35"
	set Bldg(e) "edging2 37"
	set Bldg(sw) "edging2 43"
	set Bldg(s) "edging2 44"
	set Bldg(se) "edging2 45"

	return
}

proc BldgThatch {oop} {

	variable Bldg

	array unset Bldg
	set Bldg(nw) "edging3 80"
	set Bldg(n) "edging3 81"
	set Bldg(ne) "edging3 82"
	set Bldg(w) "edging3 88"
	set Bldg(e) "edging3 90"
	set Bldg(sw) "edging3 96"
	set Bldg(s) "edging3 97"
	set Bldg(se) "edging3 98"

	return
}

proc BldgRect {oop type {fill 0}} {

	variable Bldg

	Bldg$type $oop
	set Bldg(fill) $fill
	if {$fill} {
		SetTool $oop RectFill
	} else {
		SetTool $oop RectOutline
	}
	Info $oop tool,command Feedback_BldgRect
	
	return
}

proc BldgRectPutIcon {oop icon y x} {

	set vaultId [Info $oop display,vaultId]
	set layer [Info $oop layer]

	scan $icon "%s %d" type index
	if {[icon transparent $type $index]} {
		if {$layer == 1} {
			set layer 2
		}
	} else {
		set layer 1
	}
	vault put $vaultId icon$layer [list [list $icon]] $y $x

	return
}

proc Feedback_BldgRect {oop y x} {

	variable Bldg

	if {[Info $oop track,first]} {
		ToUndo $oop
	}

	if {[Info $oop track,release]} {
		return
	}

	set vaultId [Info $oop display,vaultId]

	scan [Info $oop anchorPos] "%d %d" y2 x2

	if {$y < $y2} {
		set top $y
		set bottom $y2
	} else {
		set top $y2
		set bottom $y
	}

	if {$x < $x2} {
		set left $x
		set right $x2
	} else {
		set left $x2
		set right $x
	}

	FromUndo $oop

	# Left/Right
	for {set y $top} {$y <= $bottom} {incr y} {
		BldgRectPutIcon $oop $Bldg(w) $y $left
		BldgRectPutIcon $oop $Bldg(e) $y $right
	}
	
	# Top/Bottom
	for {set x $left} {$x <= $right} {incr x} {
		BldgRectPutIcon $oop $Bldg(n) $top $x
		BldgRectPutIcon $oop $Bldg(s) $bottom $x
	}

	# Corners
	BldgRectPutIcon $oop $Bldg(nw) $top $left
	BldgRectPutIcon $oop $Bldg(ne) $top $right
	BldgRectPutIcon $oop $Bldg(sw) $bottom $left
	BldgRectPutIcon $oop $Bldg(se) $bottom $right

	# Filled
	if {$Bldg(fill)} {
		incr top
		incr left
		for {set y $top} {$y < $bottom} {incr y} {
			for {set x $left} {$x < $right} {incr x} {
				vault put $vaultId icon1 [list [list $Edge(middle,$lgt)]] $y $x
			}
		}
	}

	return
}

