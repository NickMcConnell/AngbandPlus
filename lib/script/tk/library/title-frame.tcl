# File: title-frame.tcl

# Purpose: Grooved frame with a label/checkbutton title

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSTitleFrame {

# namespace eval NSTitleFrame
}

# NSTitleFrame::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTitleFrame::InitModule {} {

	return
}

# NSTitleFrame::NSTitleFrame --
#
#	Object constructor called by NSObject::New.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTitleFrame::NSTitleFrame {oop parent} {

	set frame $parent.titleFrame$oop
	frame $frame \
		-borderwidth 0

	frame $frame.pad \
		-borderwidth 0
	frame $frame.title \
		-borderwidth 0
	frame $frame.frameGroove \
		-borderwidth 2 -relief groove
	frame $frame.frameGroove.pad \
		-borderwidth 0

	pack $frame.pad \
		-side top -fill x
	pack $frame.frameGroove.pad \
		-side top -fill x
	pack $frame.frameGroove \
		-expand yes -fill both
	place $frame.title \
		-in $frame.frameGroove -x 6 -bordermode outside
	raise $frame.title

	bind $frame.title <Configure> \
		"NSTitleFrame::Configure $oop %h"

	Info $oop content $frame.frameGroove
	Info $oop frame $frame
	Info $oop title $frame.title
	Info $oop padY1 -1

	# Destroy the object along with the widget (later)
	NSUtils::DestroyObjectWithWidget NSTitleFrame $oop $frame

	return
}

# NSTitleFrame::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTitleFrame::Info {oop info args} {

	global NSTitleFrame

	# Verify the object
	NSObject::CheckObject NSTitleFrame $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSTitleFrame($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSTitleFrame($oop,$info)
			}
		}
	}

	return
}

# NSTitleFrame::Configure --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTitleFrame::Configure {oop height} {

	set frame [Info $oop frame]
	if {[Info $oop padY1] < 0} {
		$frame.pad configure -height [expr {$height / 2 - 1}]
	} else {
		$frame.pad configure -height [Info $oop padY1]
	}
	$frame.frameGroove.pad configure -height [expr {$height / 2 - 1}]
	place configure $frame.title \
		-y [expr {-$height / 2 + 1}]

	return
}
