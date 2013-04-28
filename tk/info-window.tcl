# File: info-window.tcl

# Purpose: the Info Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSInfoWindow {

# namespace eval NSInfoWindow
}

# NSInfoWindow::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSInfoWindow::InitModule {} {

	NSObject::New NSInfoWindow
}

# NSInfoWindow::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInfoWindow::CloseModule {} {

	catch {
		destroy [Window info]
	}

	return
}

# NSInfoWindow::NSInfoWindow --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	oop					OOP ID.
#
# Results:
#	What happened.

proc NSInfoWindow::NSInfoWindow {oop} {

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow info $win \
		"GetDefaultGeometry $win main2 main" "" ""

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSInfoWindow $oop $win

	#
	# Global list of application windows
	#

	Global info,oop $oop
	Window info $win

	return
}

# NSInfoWindow::~NSInfoWindow --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInfoWindow::~NSInfoWindow {oop} {

	return
}

# NSInfoWindow::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInfoWindow::Info {oop info args} {

	global NSInfoWindow

	# Verify the object
	NSObject::CheckObject NSInfoWindow $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSInfoWindow($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSInfoWindow($oop,$info)
			}
		}
	}

	return
}

# NSInfoWindow::InitWindow --
#
#	Create the window associated with the object.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	.

proc NSInfoWindow::InitWindow {oop} {

	set win .info$oop
	toplevel $win
	wm title $win XXXXX

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSInfoWindow::Close $oop"

	# Set instance variables
	Info $oop win $win

	set frame $win.frameList
	set tree [NSList::New $frame]
	set font [Value font,knowledge]
	$tree configure -width [expr {60 * [font measure $font W]}] \
		-height [expr {([font metrics $font -linespace] + 8) * 10}]

	Info $oop tree $tree

	#
	# Geometry
	#

	pack $win.frameList \
		-expand yes -fill both

	#
	# Feed Term when keys pressed
	#

	bind $win <KeyPress> {
		angband keypress %A
	}

	return
}

# NSInfoWindow::Close --
#
#	Description. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	.

proc NSInfoWindow::Close {oop} {

	angband keypress \033

	return
}

# NSInfoWindow::SetList --
#
#	Display a list of items. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	.

proc NSInfoWindow::SetList {oop title info} {

	set win [Info $oop win]
	set tree [Info $oop tree]

	wm title $win $title

	NSList::Clear $tree

	foreach elem $info {
		set item [$tree item create]
		NSList::SetText $tree $item $elem
		$tree item lastchild root $item
	}

	return
}

