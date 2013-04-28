# Draw water
proc PathWater {oop} {

	variable Path

	array unset Path
	set Path(we) "edging3 1"
	set Path(ns) "edging3 0"
	set Path(corner_nw) "edging3 6"
	set Path(corner_ne) "edging3 8"
	set Path(corner_sw) "edging3 12"
	set Path(corner_se) "edging3 14"
	set Path(quad) "edging3 10"

	# water sand grass
	lappend Path(merge,we) \
		"edging1 8" "edging1 16" \
		"edging1 11" "edging1 19"
	lappend Path(merge,ns) \
		"edging1 1" "edging1 2"  \
		"edging1 25" "edging1 26"

	# water grass
	lappend Path(merge,we) \
		"edging1 12" "edging1 20" \
		"edging1 15" "edging1 23"
	lappend Path(merge,ns) \
		"edging1 5" "edging1 6" \
		"edging1 29" "edging1 30"

	# sand grass
	lappend Path(merge,we) \
		"edging1 40" "edging1 48"  \
		"edging1 43" "edging1 51"
	lappend Path(merge,ns) \
		"edging1 33" "edging1 34"  \
		"edging1 57" "edging1 58"

	# water sand
	lappend Path(merge,we) \
		"edging1 44" "edging1 52"  \
		"edging1 47" "edging1 55"
	lappend Path(merge,ns) \
		"edging1 37" "edging1 38"  \
		"edging1 61" "edging1 62"

	return
}

# Draw sand
proc PathSand {oop} {

	variable Path

	array unset Path
	set Path(we) "edging3 31"
	set Path(ns) "edging3 30"
	set Path(corner_nw) "edging3 36"
	set Path(corner_ne) "edging3 38"
	set Path(corner_sw) "edging3 42"
	set Path(corner_se) "edging3 44"
	set Path(quad) "edging3 40"

	# grass
	lappend Path(merge,we) \
		"edging2 112" "edging2 104" \
		"edging2 107" "edging2 115"
	lappend Path(merge,ns) \
		"edging2 97" "edging2 98" \
		"edging2 122" "edging2 121"

	# sand
	lappend Path(merge,we) \
		"edging2 116" "edging2 108" \
		"edging2 111" "edging2 119"
	lappend Path(merge,ns) \
		"edging2 101" "edging2 102" \
		"edging2 126" "edging2 125"

	return
}

# Draw stone
proc PathStone {oop} {

	variable Path

	array unset Path
	set Path(we) "edging3 16"
	set Path(ns) "edging3 15"
	set Path(corner_nw) "edging3 21"
	set Path(corner_ne) "edging3 23"
	set Path(corner_sw) "edging3 27"
	set Path(corner_se) "edging3 29"
	set Path(quad) "edging3 25"

	# grass
	lappend Path(merge,we) \
		"edging2 112" "edging2 130" \
		"edging2 107" "edging2 131"
	lappend Path(merge,ns) \
		"edging2 97" "edging2 128" \
		"edging2 122" "edging2 129"

	# sand
	lappend Path(merge,we) \
		"edging2 116" "edging2 134" \
		"edging2 111" "edging2 135"
	lappend Path(merge,ns) \
		"edging2 101" "edging2 132" \
		"edging2 126" "edging2 133"

	return
}

proc PathPencil {oop type} {

	Path$type $oop
	SetTool $oop Pencil
	Info $oop tool,command Feedback_PathPencil

	return
}

proc Feedback_PathPencil {oop y x} {

	variable Path

	if {[Info $oop track,first]} {
		set Path(dir) ""
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
	set dir $Path(dir)

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
		} else {
			set shape ns
		}
		set dir sn
	} else {
		return
	}

	set Path(dir) $dir

	# Try replacing icon on layer 1
	set icon [vault get $vaultId icon1 $y2 $x2]
	if {[info exists Path(merge,$shape)]} {
		foreach {icon1 icon2} $Path(merge,$shape) {
			if {$icon eq $icon2} return
			if {$icon eq $icon1} {
				vault put $vaultId icon1 [list [list $icon2]] $y2 $x2
				return
			}
		}
	}

	# Try to merge with nearby path
	set icon [vault get $vaultId icon2 $y2 $x2]
	if {$icon eq $Path(ns) && $shape eq "we"} {
		set shape quad
	}
	if {$icon eq $Path(we) && $shape eq "ns"} {
		set shape quad
	}

set near(n) [vault get $vaultId icon2 [expr {$y2 - 1}] $x2]
set near(s) [vault get $vaultId icon2 [expr {$y2 + 1}] $x2]
set near(w) [vault get $vaultId icon2 $y2 [expr {$x2 - 1}]]
set near(e) [vault get $vaultId icon2 $y2 [expr {$x2 + 1}]]

	# Put the transparent path icon
	vault put $vaultId icon2 [list [list $Path($shape)]] $y2 $x2

	return
}
