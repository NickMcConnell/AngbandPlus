# File: window-manager.tcl

# Purpose: toplevel management

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSWindowManager {

	variable Priv

# namespace eval NSWindowManager
}

# NSWindowManager::RegisterWindow --
#
#	Introduce a toplevel to the NSWindowManager.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWindowManager::RegisterWindow {winName window geomCmd setupCmd displayCmd} {

	variable Priv

#	if {[info exists Priv($winName,window)]} return
	set Priv($winName,win) $window
	set Priv($winName,geomCmd) $geomCmd
	set Priv($winName,displayCmd) $displayCmd
	set Priv($winName,setupCmd) $setupCmd
	if {![info exists Priv($winName,geomRequest)]} {
		set Priv($winName,geomRequest) ""
	}
	set Priv($winName,first) 1
	set Priv($winName,setup) 0

	return
}

# NSWindowManager::UnregisterWindow --
#
#	Forget about a toplevel registered with the NSWindowManager.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWindowManager::UnregisterWindow {winName} {

	variable Priv
	
	array unset Priv *,win

	return
}

# NSWindowManager::RequestGeometry --
#
#	Request geometry for a toplevel. The toplevel might not even exist
#	yet, and in any case nothing interesting happens. Only when
#	Setup() is called for the first time will the geometry passed to
#	this command be used.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWindowManager::RequestGeometry {winName geometry} {

	variable Priv

	set screenHeight [winfo screenheight .]
	set screenWidth [winfo screenwidth .]
	
	# Sanity check
	if {[scan $geometry {%dx%d%[+-]%d%[+-]%d} width height xs x ys y] != 6} return
	if {($width < 1) || ($height < 1)} return

	# Note:
	# +-N means N pixels to the left of the left side of the screen
	# -N means N pixels to the left of the right side of the screen
	
	if {[string equal $xs +-]} {
		set x -$x
	} elseif {[string equal $xs -]} {
		set x [expr {$screenWidth - $width - $x}]
	}
	if {[string equal $ys +-]} {
		set y -$y
	} elseif {[string equal $ys -]} {
		set y [expr {$screenHeight - $height - $y}]
	}

	# Insure part of the titlebar is visible
	if {($x + $width <= 0) || ($x >= $screenWidth)} return
	if {($y + 20 <= 0) || ($y >= $screenHeight)} return

	# Remember the requested geometry
	set Priv($winName,geomRequest) $geometry

	return
}

# NSWindowManager::Setup --
#
#	Display the toplevel offscreen. Then set the toplevel geometry
#	and withdraw the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWindowManager::Setup {winName} {

	variable Priv

	if {$Priv($winName,setup)} return

	set win $Priv($winName,win)
	
	NSToplevel::NaturalSize $win $Priv($winName,setupCmd)

	set geometry $Priv($winName,geomRequest)
	if {![string length $geometry]} {
		set geometry [uplevel #0 $Priv($winName,geomCmd)]
	}
	if {[scan $geometry {%dx%d%[+-]%d%[+-]%d} width height xs x ys y] != 6} {
		error "bad geometry \"$geometry\""
	}

	# If this window is not resizeable, then ignore the given
	# height or width and use the dimension requested for the
	# window.
	scan [wm resizable $win] "%s %s" resizeH resizeV
	if {!$resizeH} {set width [winfo reqwidth $win]}
	if {!$resizeV} {set height [winfo reqheight $win]}

	wm geometry $win ${width}x$height
	update

	set command $Priv($winName,setupCmd)
	if {[string length $command]} {
		uplevel #0 $command
	}

	wm geometry $win $xs$x$ys$y
	if {[Platform unix]} {
		update idletasks
	}
	wm withdraw $win
	update

	set Priv($winName,setup) 1

	return
}

# NSWindowManager::Arrange --
#
#	Set the desired geometry for the toplevel.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWindowManager::Arrange {winName} {

	variable Priv

	# Check for exists, because I want to clear any scheduled
	# geometry request below.
	if {[info exists Priv($winName,win)]} {
		set geometry [uplevel #0 $Priv($winName,geomCmd)]
		wm geometry $Priv($winName,win) $geometry
		update idletasks
	}
	
	# Hack -- Clear any geometry request
	set Priv($winName,geomRequest) ""

	return
}

# NSWindowManager::Arrange --
#
#	Set the desired/default geometry for each toplevel.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWindowManager::ArrangeAll {} {

	variable Priv

	foreach name [array names Priv *,geomRequest] {
		regexp "(.*),geomRequest" $name ignore winName
		Arrange $winName
	}

	return
}

# NSWindowManager::Display --
#
#	Call Setup() to update the toplevel geometry if needed. Call
#	the displayCmd if provided, show the toplevel, then call the
#	displayCmd again.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWindowManager::Display {winName args} {

	variable Priv

	Setup $winName

	set displayCmd $Priv($winName,displayCmd)

	if {[wm state $Priv($winName,win)] == "normal"} {

		if {[string length $displayCmd]} {
			uplevel #0 $displayCmd reDisplay 0 $args
		}
		
	} else {
	
		if {[string length $displayCmd]} {
			uplevel #0 $displayCmd preDisplay $Priv($winName,first) $args
		}
	
		WindowBringToFront $Priv($winName,win)
		update
	
		if {[string length $displayCmd]} {
			uplevel #0 $displayCmd postDisplay $Priv($winName,first) $args
		}
		
		set Priv($winName,first) 0
	}

	return
}

# NSWindowManager::Undisplay --
#
#	Remove a window from the screen.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWindowManager::Undisplay {winName args} {

	variable Priv

	set displayCmd $Priv($winName,displayCmd)
	if {[string length $displayCmd]} {
		uplevel #0 $displayCmd preWithdraw 0 $args
	}

	wm withdraw $Priv($winName,win)
	update
	
	if {[string length $displayCmd]} {
		uplevel #0 $displayCmd postWithdraw 0 $args
	}

	return
}

# NSWindowManager::GetWindowList --
#
#	Set the desired geometry for the toplevel.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWindowManager::GetWindowList {} {

	variable Priv

	set result {}
	foreach name [array names Priv "*,win"] {
		regexp "(.*),win" $name ignore window
		lappend result $window
	}
	return $result
}

