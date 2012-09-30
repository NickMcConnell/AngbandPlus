# File: tabbed-frame.tcl

# Purpose: Tabs + a frame w/ border

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSTabbedFrame {

# namespace eval NSTabbedFrame
}

# NSTabbedFrame::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTabbedFrame::InitModule {} {

#	NSModule::LoadIfNeeded NSTabs

	return
}

# NSTabbedFrame::NSTabbedFrame --
#
#	Object constructor called by NSObject::New.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTabbedFrame::NSTabbedFrame {oop parent} {

	set frame $parent.tabbedFrame$oop
	frame $frame \
		-borderwidth 0
	frame $frame.pad \
		-borderwidth 0
	frame $frame.frameOuter \
		-borderwidth 2 -relief raised
	frame $frame.frameOuter.frameInner \
		-borderwidth 0

	set tabsId [NSObject::New NSTabs $frame]
	set canvasTabs [NSTabs::Info $tabsId canvas]

	set height [winfo reqheight $canvasTabs]
	$frame.pad configure -height $height

	pack $frame.pad -side top -fill x
	pack $frame.frameOuter.frameInner -expand yes -fill both -padx 6 -pady 6
	pack $frame.frameOuter -expand yes -fill both
	place $canvasTabs -in $frame.frameOuter -bordermode outside \
		-x 0 -y [expr {0 - $height + 2}]
	raise [NSTabs::Info $tabsId canvas]

	Info $oop frame $frame
	Info $oop content $frame.frameOuter.frameInner
	Info $oop tabsId $tabsId

	# Destroy the object along with the widget (later)
	NSUtils::DestroyObjectWithWidget NSTabbedFrame $oop $frame

	return
}

# NSTabbedFrame::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTabbedFrame::Info {oop info args} {

	global NSTabbedFrame

	# Verify the object
	NSObject::CheckObject NSTabbedFrame $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSTabbedFrame($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSTabbedFrame($oop,$info)
			}
		}
	}

	return
}

