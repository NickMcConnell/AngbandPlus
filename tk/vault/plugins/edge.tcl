proc EdgeGrass {oop} {

	variable Edge

	array unset Edge
	set Edge(we,1) "edging2 97"
	set Edge(we,0) "edging2 122"
	set Edge(ew,1) "edging2 97"
	set Edge(ew,0) "edging2 122"
	set Edge(ns,1) "edging2 112"
	set Edge(ns,0) "edging2 107"
	set Edge(sn,1) "edging2 112"
	set Edge(sn,0) "edging2 107"
	set Edge(corner_nw,1) "edging2 96"
	set Edge(corner_nw,0) "edging2 105"
	set Edge(corner_ne,1) "edging2 106"
	set Edge(corner_ne,0) "edging2 99"
	set Edge(corner_sw,1) "edging2 120"
	set Edge(corner_sw,0) "edging2 113"
	set Edge(corner_se,1) "edging2 114"
	set Edge(corner_se,0) "edging2 123"

	set Edge(middle,1) "edging1 121"
	set Edge(middle,0) "edging1 121"
	
	return
}

proc EdgeWaterGrass {oop} {

	variable Edge

	array unset Edge
	set Edge(we,1) "edging1 5"
	set Edge(we,0) "edging1 29"
	set Edge(ew,1) "edging1 5"
	set Edge(ew,0) "edging1 29"
	set Edge(ns,1) "edging1 12"
	set Edge(ns,0) "edging1 15"
	set Edge(sn,1) "edging1 12"
	set Edge(sn,0) "edging1 15"
	set Edge(corner_nw,1) "edging1 4"
	set Edge(corner_nw,0) "edging1 13"
	set Edge(corner_ne,0) "edging1 7"
	set Edge(corner_ne,1) "edging1 14"
	set Edge(corner_sw,1) "edging1 28"
	set Edge(corner_sw,0) "edging1 21"
	set Edge(corner_se,0) "edging1 31"
	set Edge(corner_se,1) "edging1 22"

	set Edge(middle,1) "edging1 121"
	set Edge(middle,0) "edging1 114"

	return
}

proc EdgeWaterSandGrass {oop} {

	variable Edge

	array unset Edge
	set Edge(we,1) "edging1 1"
	set Edge(we,0) "edging1 25"
	set Edge(ew,1) "edging1 1"
	set Edge(ew,0) "edging1 25"
	set Edge(ns,1) "edging1 8"
	set Edge(ns,0) "edging1 11"
	set Edge(sn,1) "edging1 8"
	set Edge(sn,0) "edging1 11"
	set Edge(corner_nw,1) "edging1 0"
	set Edge(corner_nw,0) "edging1 9"
	set Edge(corner_ne,0) "edging1 3"
	set Edge(corner_ne,1) "edging1 10"
	set Edge(corner_sw,1) "edging1 24"
	set Edge(corner_sw,0) "edging1 17"
	set Edge(corner_se,0) "edging1 27"
	set Edge(corner_se,1) "edging1 18"

	set Edge(middle,1) "edging1 121"
	set Edge(middle,0) "edging1 113"

	return
}

proc EdgeSand {oop} {

	variable Edge

	array unset Edge
	set Edge(we,1) "edging2 101"
	set Edge(we,0) "edging2 126"
	set Edge(ew,1) "edging2 101"
	set Edge(ew,0) "edging2 126"
	set Edge(ns,1) "edging2 116"
	set Edge(ns,0) "edging2 111"
	set Edge(sn,1) "edging2 116"
	set Edge(sn,0) "edging2 111"
	set Edge(corner_nw,1) "edging2 100"
	set Edge(corner_nw,0) "edging2 109"
	set Edge(corner_ne,1) "edging2 110"
	set Edge(corner_ne,0) "edging2 103"
	set Edge(corner_sw,1) "edging2 124"
	set Edge(corner_sw,0) "edging2 117"
	set Edge(corner_se,1) "edging2 118"
	set Edge(corner_se,0) "edging2 127"

	set Edge(middle,1) "edging1 117"
	set Edge(middle,0) "edging1 117"

	return
}

proc EdgeWaterSand {oop} {

	variable Edge

	array unset Edge
	set Edge(we,1) "edging1 37"
	set Edge(we,0) "edging1 61"
	set Edge(ew,1) "edging1 37"
	set Edge(ew,0) "edging1 61"
	set Edge(ns,1) "edging1 44"
	set Edge(ns,0) "edging1 47"
	set Edge(sn,1) "edging1 44"
	set Edge(sn,0) "edging1 47"
	set Edge(corner_nw,1) "edging1 36"
	set Edge(corner_nw,0) "edging1 45"
	set Edge(corner_ne,0) "edging1 39"
	set Edge(corner_ne,1) "edging1 46"
	set Edge(corner_sw,1) "edging1 60"
	set Edge(corner_sw,0) "edging1 53"
	set Edge(corner_se,0) "edging1 63"
	set Edge(corner_se,1) "edging1 54"

	set Edge(middle,1) "edging1 117"
	set Edge(middle,0) "edging1 113"

	return
}

proc EdgeSandGrass {oop} {

	variable Edge

	array unset Edge
	set Edge(we,1) "edging1 33"
	set Edge(we,0) "edging1 57"
	set Edge(ew,1) "edging1 33"
	set Edge(ew,0) "edging1 57"
	set Edge(ns,1) "edging1 40"
	set Edge(ns,0) "edging1 43"
	set Edge(sn,1) "edging1 40"
	set Edge(sn,0) "edging1 43"
	set Edge(corner_nw,1) "edging1 32"
	set Edge(corner_nw,0) "edging1 41"
	set Edge(corner_ne,0) "edging1 35"
	set Edge(corner_ne,1) "edging1 42"
	set Edge(corner_sw,1) "edging1 56"
	set Edge(corner_sw,0) "edging1 49"
	set Edge(corner_se,0) "edging1 59"
	set Edge(corner_se,1) "edging1 50"

	set Edge(middle,1) "edging1 121"
	set Edge(middle,0) "edging1 117"

	return
}

proc EdgePencil {oop type} {

	Edge$type $oop
	SetTool $oop Pencil
	Info $oop tool,command Feedback_EdgePencil

	return
}

proc EdgeRect {oop type {fill 0}} {

	variable Edge

	Edge$type $oop
	set Edge(fill) $fill
	if {$fill} {
		SetTool $oop RectFill
	} else {
		SetTool $oop RectOutline
	}
	Info $oop tool,command Feedback_EdgeRect
	
	return
}

proc Feedback_EdgePencil {oop y x} {

	variable Edge

	if {[Info $oop track,first]} {
		set Edge(dir) ""
		set Edge(light) [expr {![Info $oop click,ctrl]}]
		ToUndo $oop
		return
	}

	if {[Info $oop track,release]} {
		Info $oop tool,command Feedback_Pencil
		return
	}

	set vaultId [Info $oop display,vaultId]
	scan [Info $oop prevPos] "%d %d" y2 x2
	set dy [Info $oop track,dy,prev]
	set dx [Info $oop track,dx,prev]
	set dir $Edge(dir)
	set light $Edge(light)
	set flip 0

	# Move left -> right
	if {!$dy && $dx > 0} {

		# Was moving top -> bottom
		if {$dir eq "ns"} {
			set shape corner_sw

		# Was moving bottom -> top
		} elseif {$dir eq "sn"} {
			set shape corner_nw

		# Repeat left -> right
		} else {
			set shape we
		}
		set dir we

	# Move right -> left
	} elseif {!$dy && $dx < 0} {
		if {$dir eq "ns"} {
			set shape corner_se
		} elseif {$dir eq "sn"} {
			set shape corner_ne
		} else {
			set shape we
		}
		set dir ew

	# Move top -> bottom
	} elseif {$dy > 0 && !$dx} {
		if {$dir eq "we"} {
			set shape corner_ne
			set flip 1
		} elseif {$dir eq "ew"} {
			set shape corner_nw
		} else {
			set shape ns
		}
		set dir ns

	# Move bottom -> top
	} elseif {$dy < 0 && !$dx} {

		# Was left -> right
		if {$dir eq "we"} {
			set shape corner_se

		# Was right -> left
		} elseif {$dir eq "ew"} {
			set shape corner_sw
			set flip 1
		} else {
			set shape ns
		}
		set dir sn
	} else {
		return
	}

	if {$flip} {
		set icon $Edge($shape,[expr {!$light}])
	} else {
		set icon $Edge($shape,$light)
	}
	set iconList [list [list $icon]]
	vault put $vaultId icon1 $iconList $y2 $x2

	# Light changes on corner_sw and corner_ne only
	if {$shape eq "corner_sw" || $shape eq "corner_ne"} {
		set light [expr {!$light}]
	}

	set Edge(dir) $dir
	set Edge(light) $light

	return
}

proc Feedback_EdgeRect {oop y x} {

	variable Edge

	if {[Info $oop track,first]} {
		set Edge(light) [expr {![Info $oop click,ctrl]}]
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

	set lgt $Edge(light)
	set drk [expr {!$lgt}]

	# Left/Right
	for {set y $top} {$y <= $bottom} {incr y} {
		vault put $vaultId icon1 [list [list $Edge(ns,$lgt)]] $y $left
		vault put $vaultId icon1 [list [list $Edge(ns,$drk)]] $y $right
	}
	
	# Top/Bottom
	for {set x $left} {$x <= $right} {incr x} {
		vault put $vaultId icon1 [list [list $Edge(we,$lgt)]] $top $x
		vault put $vaultId icon1 [list [list $Edge(we,$drk)]] $bottom $x
	}

	# Corners
	vault put $vaultId icon1 [list [list $Edge(corner_nw,$lgt)]] $top $left
	vault put $vaultId icon1 [list [list $Edge(corner_ne,$drk)]] $top $right
	vault put $vaultId icon1 [list [list $Edge(corner_sw,$lgt)]] $bottom $left
	vault put $vaultId icon1 [list [list $Edge(corner_se,$drk)]] $bottom $right

	# Filled
	if {$Edge(fill)} {
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

