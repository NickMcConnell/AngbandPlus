proc Bridge1 {oop} {

	variable Bridge

	array unset Bridge
	set Bridge(w) "edging3 52"
	set Bridge(e) "edging3 53"
	set Bridge(we) "edging3 51"
	set Bridge(n) "edging3 50"
	set Bridge(s) "edging3 49"
	set Bridge(ns) "edging3 48"

	return
}

proc Bridge2 {oop} {

	variable Bridge

	array unset Bridge
	set Bridge(w) "edging3 58"
	set Bridge(e) "edging3 59"
	set Bridge(we) "edging3 57"
	set Bridge(n) "edging3 56"
	set Bridge(s) "edging3 55"
	set Bridge(ns) "edging3 54"

	return
}

proc Bridge3 {oop} {

	variable Bridge

	array unset Bridge
	set Bridge(w) "edging3 64"
	set Bridge(e) "edging3 65"
	set Bridge(we) "edging3 63"
	set Bridge(n) "edging3 62"
	set Bridge(s) "edging3 61"
	set Bridge(ns) "edging3 60"

	return
}

proc BridgePencil {oop type} {

	Bridge$type $oop
	SetTool $oop Pencil
	Info $oop tool,command Feedback_BridgePencil

	return
}

proc Feedback_BridgePencil {oop y x} {

	variable Bridge

	set vaultId [Info $oop display,vaultId]

	if {[Info $oop track,first]} {
		ToUndo $oop
		return
	}

	if {[Info $oop track,release]} {
		scan [Info $oop anchorPos] "%d %d" ys xs
		set dy [Info $oop track,dy]
		set dx [Info $oop track,dx]
		if {!$dy && $dx >= 1} {
			vault put $vaultId icon2 [list [list $Bridge(w)]] $ys $xs
			for {set x2 [expr {$xs + 1}]} {$x2 < $x} {incr x2} {
				vault put $vaultId icon2 [list [list $Bridge(we)]] $y $x2
			}
			vault put $vaultId icon2 [list [list $Bridge(e)]] $y $x
		}
		if {$dy >= 1 && !$dx} {
			vault put $vaultId icon2 [list [list $Bridge(n)]] $ys $xs
			for {set y2 [expr {$ys + 1}]} {$y2 < $y} {incr y2} {
				vault put $vaultId icon2 [list [list $Bridge(ns)]] $y2 $x
			}
			vault put $vaultId icon2 [list [list $Bridge(s)]] $y $x
		}
		Info $oop tool,command Feedback_Pencil
		return
	}

	return
}

