# File: progress.tcl

# Purpose: a colorful progress bar using frames

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSProgress {

# namespace eval NSProgress
}

# NSProgress::NSProgress --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgress::NSProgress {oop parent width height color1 color2} {

	set frame $parent.progress$oop
	frame $frame -width $width -height $height -relief raised -borderwidth 1

	frame $frame.todo -relief sunken -borderwidth 1 -background $color2
	frame $frame.done -relief raised -borderwidth 1 -background $color1

	place $frame.todo -x 0 -y 0 -anchor nw -relwidth 1.0 -relheight 1.0
	place $frame.done -x 0 -y 0 -anchor nw -relwidth 0.5 -relheight 1.0

	Info $oop frame $frame

	return
}

# NSProgress::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgress::Info {oop info args} {

	global NSProgress

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSProgress($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSProgress($oop,$info)
			}
		}
	}

	return
}

# NSProgress::SetDoneRatio --
#
#	Set the progress bar to frac% completion.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgress::SetDoneRatio {oop frac} {

	set frame [Info $oop frame]
	place configure $frame.done -relwidth $frac

	return
}

