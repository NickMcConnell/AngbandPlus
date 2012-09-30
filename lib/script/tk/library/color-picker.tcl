# File: color-picker.tcl

# Purpose: a color picker

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSColorPicker {

# namespace eval NSColorPicker
}

# NSColorPicker::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPicker::InitModule {} {
}

# NSColorPicker::NSColorPicker --
#
#	Object constructor called by NSObject::New(). Creates a new
#	color picker in the given parent. Afterwards you should call
#	SetColors() with a list of colors to display, and then set
#	NSColorPicker(OOP,command) to the command to call whenever
#	SetCursor() changes the cursor position. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPicker::NSColorPicker {oop parent cols rows size} {

	set canvas $parent.picker$oop

	canvas $canvas \
		-width [expr {$size * $cols + 1}] \
		-height [expr {$size * $rows + 1}] \
		-relief flat -borderwidth 0 -highlightthickness 0

	bind $canvas <ButtonPress-1> "
		NSColorPicker::SetCursor $oop \[NSColorPicker::GetIndex $oop %x %y] 1
	"
	bind $canvas <Button1-Motion> "
		NSColorPicker::SetCursor $oop \[NSColorPicker::GetIndex $oop %x %y] 1
	"

	Info $oop cols $cols
	Info $oop rows $rows
	Info $oop size $size
	Info $oop canvas $canvas
	Info $oop index 0
	Info $oop command ""

	bind $canvas <Destroy> "NSObject::Delete NSColorPicker $oop"

	return
}

# NSColorPicker::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPicker::Info {oop info args} {

	global NSColorPicker

	# Verify the object
	NSObject::CheckObject NSColorPicker $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSColorPicker($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSColorPicker($oop,$info)
			}
		}
	}

	return
}

# NSColorPicker::SetColors --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPicker::SetColors {oop colors} {

	set canvas [Info $oop canvas]
	set size [Info $oop size]
	set cols [Info $oop cols]
	set rows [Info $oop rows]

	set count [llength $colors]
	if {$count % $cols} {
		set rows [expr {$count / $cols + 1}]
		set fudgeCol [expr {$count % $cols}]
	} else {
		set rows [expr {$count / $cols}]
		set fudgeCol $cols
	}

	$canvas delete all

	set i 0
	foreach color $colors {
		set y [expr {$i / $cols}]
		set x [expr {$i % $cols}]
		set n 1
		if {($x == $cols - 1) || ($y == $rows - 1) ||
			(($y == $rows - 2) && ($x >= $fudgeCol))} {set n 0}
		$canvas create rectangle [expr {$x * $size}] \
			[expr {$y * $size}] [expr {($x + 1) * $size + $n}] \
			[expr {($y + 1) * $size + $n}] -fill $color \
			-tags $i
		incr i
	}

	$canvas create rectangle 0 0 $size \
		$size -outline White -tags cursor

	# Cursor starts out not visible
	ShowHideCursor $oop 0

	return
}

# NSColorPicker::SetCursor --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPicker::SetCursor {oop index callCmd} {

	if {![Info $oop active]} return
	if {$index < 0 || $index > 255} return
	if {$index == [Info $oop index]} return

	set canvas [Info $oop canvas]

	set size [Info $oop size]
	set cols [Info $oop cols]

	set bbox [$canvas bbox cursor]
	set oldX [lindex $bbox 0]
	set oldY [lindex $bbox 1]

	set row [expr {$index / $cols}]
	set col [expr {$index % $cols}]
	set x [expr {$col * $size - 1}]
	set y [expr {$row * $size - 1}]

	$canvas move cursor [expr {$x - $oldX}] [expr {$y - $oldY}]

	Info $oop index $index

	if {$callCmd} {
		set command [Info $oop command]
		if {[string length $command]} {
			uplevel #0 $command $oop $index
		}
	}

	return
}

# NSColorPicker::ShowHideCursor --
#
#	Makes the cursor visible or not visible, by changing its color.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPicker::ShowHideCursor {oop visible} {

	set canvas [Info $oop canvas]

	if {$visible} {
		$canvas itemconfigure cursor -outline White
	} else {
		$canvas itemconfigure cursor -outline Black
	}

	Info $oop active $visible

	return
}

# NSColorPicker::GetIndex --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSColorPicker::GetIndex {oop x y} {

	set canvas [Info $oop canvas]
	set itemId [$canvas find closest $x $y 1.0 cursor]
	if {$itemId == ""} {return -1}
	foreach tag [$canvas gettags $itemId] {
		if {$tag >= 0 && $tag < 256} {
			return $tag
		}
	}
	return -1
}

