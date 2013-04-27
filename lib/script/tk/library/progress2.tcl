# File: progress2.tcl

# Purpose: an MS-Windows-like progress bar using a canvas

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSProgress2 {

# namespace eval NSProgress2
}

# NSProgress2::NSProgress2 --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgress2::NSProgress2 {oop parent width height} {

	set canvas $parent.progress$oop
	canvas $canvas \
		-width $width -height $height -borderwidth 1 -relief sunken \
		-highlightthickness 0

	Info $oop frame $canvas
	Info $oop frac 0.0

	Configure $oop

	bind $canvas <Configure> "NSProgress2::Configure $oop"

	return
}

# NSProgress2::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgress2::Info {oop info args} {

	global NSProgress2

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSProgress2($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSProgress2($oop,$info)
			}
		}
	}

	return
}

# NSProgress2::SetDoneRatio --
#
#	Set the progress bar to frac% completion.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgress2::SetDoneRatio {oop frac} {

	set canvas [Info $oop frame]
	set numDots [Info $oop numDots]


	set dotNum [expr {int($frac * $numDots)}]
	for {set i [Info $oop curDot]} {$i < $dotNum} {incr i} {
		$canvas itemconfigure dot$i -fill $::SystemHighlight -outline $::SystemHighlight
	}
	for {set i $dotNum} {$i < [Info $oop curDot]} {incr i} {
		$canvas itemconfigure dot$i -fill {} -outline {}
	}

	Info $oop curDot $dotNum
	Info $oop frac $frac

	return
}

# NSProgress2::Zero --
#
#	Set the progress bar to 0% completion.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgress2::Zero {oop} {

	set canvas [Info $oop frame]
		
	$canvas itemconfigure dot -fill {} -outline {}
	Info $oop curDot 0
	Info $oop frac 0

	return
}

# NSProgress2::Configure --
#
#	Called when the canvas changes size.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgress2::Configure {oop} {

	set canvas [Info $oop frame]
	$canvas delete dot

	set width [winfo width $canvas]
	set height [winfo height $canvas]

	# Create a bunch of progress "dots", invisible for now
	set n [expr {$width / 5}]
	set x 2
	for {set i 0} {$i < $n} {incr i} {
		$canvas create rectangle $x 2 [expr {$x + 3}] [expr {$height - 1}] \
			-fill "" -outline "" -tags "dot dot$i"
		incr x 5
	}

	Info $oop numDots $n
	Info $oop curDot 0

	SetDoneRatio $oop [Info $oop frac]

	return
}

