# File: toplevel.tcl

# Purpose: toplevel-related geometry commands

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSToplevel {

	variable Priv

# namespace eval NSToplevel
}

# NSToplevel::MoveOffscreen --
#
#	Move a window offscreen.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToplevel::MoveOffscreen {toplevel} {

	variable Priv

	set Priv(save,focus) [focus]
	set Priv(save,state) [wm state $toplevel]
	set Priv(save,x) [winfo x $toplevel]
	set Priv(save,y) [winfo y $toplevel]

	wm withdraw $toplevel
	update idletasks

	wm geometry $toplevel +[winfo screenwidth $toplevel]+0
	update idletasks

#	wm deiconify $toplevel
wm state $toplevel normal
	update idletasks

	return
}

# NSToplevel::Restore --
#
#	Put a toplevel back to how it was before it was moved offscreen.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToplevel::Restore {toplevel} {

	variable Priv

	set state $Priv(save,state)

	if {$state == "withdrawn"} {
		wm withdraw $toplevel

	} elseif {$state == "iconic"} {
		wm iconify $toplevel
	}
	update idletasks

#	wm geometry $toplevel +$Priv(save,x)+$Priv(save,y)
#	update idletasks

	focus $Priv(save,focus)

	return
}

# NSToplevel::WidthLeft --
#
#	Return width of frame at left of a window. It is assumed that this is
#	also the width at the right and bottom of the frame.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToplevel::WidthLeft {toplevel} {
	return [expr {[ContentLeft $toplevel] - [FrameLeft $toplevel]}]
}

# NSToplevel::WidthTop --
#
#	Return the width of frame (and menubar) at the top of a window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToplevel::WidthTop {toplevel} {
	return [expr {[ContentTop $toplevel] - [FrameTop $toplevel]}]
}

# NSToplevel::FrameRight --
#
#	Return the pixel-coordinate of pixel to the right of the right edge of a
#	window's frame.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToplevel::FrameRight {toplevel} {
	return [expr {[ContentLeft $toplevel] + [winfo width $toplevel] + \
		[WidthLeft $toplevel]}]
}

# NSToplevel::FrameBottom --
#
#	Return the pixel-coordinate of pixel below the bottom edge of a
#	window's frame.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToplevel::FrameBottom {toplevel} {
	return [expr {[ContentTop $toplevel] + [winfo height $toplevel] + \
		[WidthLeft $toplevel]}]
}

# NSToplevel::ContentWidth --
#
#	Return the content width required so that a toplevel will be
#	exactly as wide as the given width, from left edge to right
#	edge.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToplevel::ContentWidth {toplevel width} {

	set frameLeft [WidthLeft $toplevel]
	return [expr {$width - $frameLeft * 2}]
}

# NSToplevel::ContentHeight --
#
#	Return the content height required so that a toplevel will be
#	exactly as tall as the given height, from top edge to bottom
#	edge.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToplevel::ContentHeight {toplevel height} {

	set frameTop [WidthTop $toplevel]
	set frameBottom [WidthLeft $toplevel]
	return [expr {$height - $frameTop - $frameBottom}]
}

# NSToplevel::TotalWidth --
#
#	Return the width of a toplevel from left to right.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToplevel::TotalWidth {toplevel} {

	set frameLeft [WidthLeft $toplevel]
	set width [winfo width $toplevel]
	set frameRight [WidthLeft $toplevel]
	return [expr {$frameLeft + $width + $frameRight}]
}

# NSToplevel::TotalHeight --
#
#	Return the height of a toplevel from top to bottom.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToplevel::TotalHeight {toplevel} {

	set frameTop [WidthTop $toplevel]
	set height [winfo height $toplevel]
	set frameBottom [WidthLeft $toplevel]
	return [expr {$frameTop + $height + $frameBottom}]
}

# NSToplevel::NaturalSize --
#
#	Withdraws the given toplevel, moves it offscreen, then shows it
#	again. At this point the geometry info is all correct, and the
#	caller can then confidently use the geometry info to set lists
#	etc.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToplevel::NaturalSize {toplevel command} {

if {[Platform unix]} {
	wm state $toplevel normal
	update idletasks
	wm geometry $toplevel +[winfo screenwidth $toplevel]+0
	update idletasks
} else {
	# Withdraw the toplevel
	wm withdraw $toplevel
	update idletasks

	# Move the toplevel offscreen
	wm geometry $toplevel +[winfo screenwidth $toplevel]+0
	update idletasks

	# Show the toplevel
	wm state $toplevel normal

	update idletasks
}

	# Now that the window is displayed, the geometry information
	# is all correct. Call the client command to fiddle with the
	# geometry of any widgets.
	if {[string length $command]} {
		uplevel #0 $command
	}

	# Just to club it over the head...
	update

	return
}

# NSToplevel::SetTotalWidth --
#
#	Take the current geometry, and replace width with the given width.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToplevel::SetTotalWidth {win newWidth} {

	set geometry [wm geometry $win]
	scan $geometry {%dx%d%[+-]%d%[+-]%d} width height xs x ys y
	wm geometry $win ${newWidth}x$height$xs$x$ys$y

	return
}

# NSToplevel::SetTotalHeight --
#
#	Take the current geometry, and replace height with the given height.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToplevel::SetTotalHeight {win newHeight} {

	set geometry [wm geometry $win]
	scan $geometry {%dx%d%[+-]%d%[+-]%d} width height xs x ys y
	wm geometry $win ${width}x${newHeight}$xs$x$ys$y

	return
}

# NSToplevel::FrameLeft --
#
#	Return the x coordinate of the toplevel's frame.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToplevel::FrameLeft {win} {

	switch -- [Platform] {
		windows {
			return [winfo x $win]
		}
		unix {
			set geometry [wm geometry $win]
			scan $geometry {%dx%d%[+-]%d%[+-]%d} width height xs x ys y
			return $x
		}
	}
}

# NSToplevel::FrameTop --
#
#	Return the y coordinate of the toplevel's frame.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToplevel::FrameTop {win} {

	switch -- [Platform] {
		windows {
			return [winfo y $win]
		}
		unix {
			set geometry [wm geometry $win]
			scan $geometry {%dx%d%[+-]%d%[+-]%d} width height xs x ys y
			return $y
		}
	}
}

# NSToplevel::ContentLeft --
#
#	Return the x coordinate of the toplevel's insides.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToplevel::ContentLeft {win} {

	switch -- [Platform] {
		windows {
			return [winfo rootx $win]
		}
		unix {
			return [winfo rootx $win]
		}
	}
}

# NSToplevel::ContentTop --
#
#	Return the y coordinate of the toplevel's insides.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToplevel::ContentTop {win} {

	switch -- [Platform] {
		windows {
			return [winfo rooty $win]
		}
		unix {
			return [winfo rooty $win]
		}
	}
}
