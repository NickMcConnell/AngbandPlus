# File: init-other.tcl

# Purpose: post-character-generation initialization script

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

# Toplevel headaches (Win32, Tk 8.3.3):
# If a window is withdrawn, then "wm geometry $w" returns the geometry
# from the last time the window was visible, *not* any requested geomtry.
# Also, "wm geometry $w $g" doesn't actually change the geometry until
# the window is displayed.

proc tryGeometry {win geometry} {
	if {1 || [string compare $geometry [wm geometry $win]]} {
		wm geometry $win $geometry
	}
	return
}

# Deiconify a window. If the window does not appear at the location
# we requested, then move it there.
proc wmDeiconify {win} {
	set geometry [wm geometry $win]
	wm deiconify $win
	update
	tryGeometry $win $geometry
}

# Show a window. If the window does not appear at the location
# we requested, then move it there.
proc wmStateNormal {win} {
	set geometry [wm geometry $win]
	wm state $win normal
	update
	tryGeometry $win $geometry
}

proc WMSetWindowGeometry {win geometry} {

	wm geometry $win $geometry

	return
}

# ReadGeometryFile --
#
#	Reads the "geometry" file which contains the desired geometry
#	of each of the game's windows. If the "geometry" file does not
#	exist, then the game uses the default window positions, as set
#	in HarcodeGeometry() below. To create the "geometry" file, the
#	user can choose "Save Window Positions" from the Other Menu.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc ReadGeometryFile {} {

	global Angband
	global Windows

	if {[catch {open [PathTk config geometry]} fileId]} {
		set msg "The following error occurred while attempting to open "
		append msg "the \"geometry\" file for reading:\n\n$fileId"
		tk_messageBox -title Oops -message $msg
		return
	}

	set buf [read $fileId]
	close $fileId

	# Check each line
	foreach geomInfo [split $buf \n] {

		# Skip blank lines
		if {![string length $geomInfo]} continue

		# Skip comments
		if {[string equal [string index $geomInfo 0] #]} continue

		# Split line into window keyword and geometry
		if {[scan $geomInfo "%s %s" window geometry] != 2} continue

		# Request geometry. The window may not exist yet.
		NSWindowManager::RequestGeometry $window $geometry

		# Sanity: Windows(win) exists?
		if {![info exists Windows($window)]} continue

		# Get the toplevel pathname
		set win [Window $window]

		if {[scan $geometry {%dx%d%[+-]%d%[+-]%d} width height xs x ys y] != 6} {
			continue
		}

		# If this window is not resizeable, then ignore the given
		# height or width and use the dimension requested for the
		# window.
		set resize [wm resizable $win]
		if {![lindex $resize 0]} {set width [winfo reqwidth $win]}
		if {![lindex $resize 1]} {set height [winfo reqheight $win]}

		# If this is a gridded window, convert from dimensions in
		# pixels to grid units.
		set grid [wm grid $win]
		if {[llength $grid]} {
			set width [expr {$width / [lindex $grid 2]}]
			set height [expr {$height / [lindex $grid 3]}]
		}

		# Set the window geometry
		set geometry ${width}x$height$xs$x$ys$y

WMSetWindowGeometry $win $geometry
continue

		# Get the toplevel state
		set state [wm state $win]

		# If this toplevel is showing, then hide it first
		# to make the geometry request work
		if {[string equal $state normal]} {
			wm withdraw $win
			update idletasks
		}

		# Set the toplevel geometry
		tryGeometry $win $geometry

		# Restore the window if it was hidden by us
		if {[string equal $state normal]} {
			update idletasks
			wmDeiconify $win
		}
	}

	return
}

# WriteGeometryFile --
#
#	Writes the "geometry" file with the current geometry of each of the
#	game's windows. The "geometry" file is created if it does not already
#	exist. To create the "geometry" file, the user can choose "Save
#	Window Positions" from the Other Menu.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	It seems a toplevel's geometry is not correct until is has been
#	displayed at least once. This routine brings any non-"normal"
#	windows to the front before getting its geometry. This looks bad.
#	It might be better to move the window offscreen first.
#	-> FixWindows() fixed this problem...?

proc WriteGeometryFile {} {

	global Angband
	global Windows

	set tempName [NSUtils::TempFileName [PathTk config]]
	if {[catch {openlf $tempName} fileId]} {
		set msg "The following error occurred while attempting to open "
		append msg "the \"geometry\" file for writing:\n\n$fileId"
		tk_messageBox -title Oops -message $msg
		return
	}

	set success 1

	if {[catch {

	puts $fileId "# Automatically generated. Do not edit.\n"

	# Be sure to keep any requested geometry for windows which may not
	# have been created.
	foreach name [array names NSWindowManager::Priv *,geomRequest] {
		regexp "(.*),geomRequest" $name ignore window
		set geometry $NSWindowManager::Priv($name)
		if {[scan $geometry {%dx%d%[+-]%d%[+-]%d} \
			width height xs x ys y] != 6} continue
		set geometryInfo($window) $geometry
	}

	# Get the current geometry for existing windows.
	foreach window [array names Windows] {
		set win [Window $window]

		# HighScore destroys itself after character death
		if {![winfo exists $win]} continue

		# Get the window geometry
		if {[scan [wm geometry $win] {%dx%d%[+-]%d%[+-]%d} \
			width height xs x ys y] != 6} continue

		# If this is a gridded window, the geometry information
		# is returned as {columns rows columnWidth rowWidth}. In
		# this case I save the total area of the grid in pixels.
		# This is needed because (1) the user can switch between
		# 16x16 and 32x32 icons at startup, and (2) the window
		# may not be gridded in the next version of the game.
		set grid [wm grid $win]
		if {[llength $grid]} {
			set width [expr {$width * [lindex $grid 2]}]
			set height [expr {$height * [lindex $grid 3]}]
		}

		set geometryInfo($window) ${width}x$height$xs$x$ys$y
	}

	# Write the geometry for each window
	foreach window [lsort -dictionary [array names geometryInfo]] {
		puts $fileId "$window $geometryInfo($window)"
	}

	# catch
	} result]} {
		set msg "The following error occurred while attempting to write "
		append msg "the \"geometry\" file:\n\n$result"
		tk_messageBox -title Oops -message $msg

		set success 0
	}
	
	close $fileId

	if {$success} {
		set fileName [NSUtils::ReadLink [PathTk config geometry]]
		if {[file exists $fileName]} {
			file rename -force -- $fileName $fileName.bak
		}
		file rename -- $tempName $fileName
	} else {
		file delete $tempName
	}

	return
}

# HardcodeGeometry --
#
#	Sets the position of all the game's windows to an appropriate
#	default position. This is done at startup if the "geometry" file does
#	not exist, and when the user chooses "Arrange Windows" from
#	the Other Menu.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc HardcodeGeometry {} {

	global Windows

	set spacing 0
	set offset [winfo screenwidth .]

	# Tk 8.3.0 bug
	set win [Window main]
	set visible(main) [winfo ismapped $win]
	wm geometry $win +$offset+0
	update idletasks
	wm deiconify $win
	
	# Move each window offscreen, and show it.
	foreach window [array names Windows] {

		# Tk 8.3.0 bug
		if {[string equal $window main]} continue

		set win [Window $window]
		set visible($window) [winfo ismapped $win]
		wm geometry $win +$offset+0
		update idletasks
		wm deiconify $win
	}

	# Must be update, not "update idletasks", or the Main Window geometry
	# is silly (too tall). Don't know why.
	update

	# Message Window
	set win1 [Window message]
	set x 0 ; incr x $offset
	wm geometry $win1 +$x+0
	update idletasks

	# Main Window
	set x 0 ; incr x $offset
	if {[Value message,float]} {
		set y [expr {[NSToplevel::FrameBottom $win1] + $spacing}]
	} else {
		set y 0
	}
	set win1 [Window main]
	wm geometry $win1 +$x+$y
	update idletasks

	# Misc Window
	set y [expr {[NSToplevel::FrameBottom $win3] + $spacing}]
	set win1 [Window misc]
	wm geometry $win1 +$x+$y
	update idletasks

	# Progress Window
	set y [expr {[NSToplevel::FrameBottom $win1] + $spacing}]
	set win1 [Window progress]
	wm geometry $win1 +$x+$y
	update idletasks

	# Message Window (width)
	set win2 [Window message]
	set x 0 ; incr x $offset
	set width [NSToplevel::ContentWidth $win2 \
		[expr {0 - $x}]]
	wm geometry $win2 ${width}x[winfo height $win2]+$x+0
	update idletasks

	# Tips Window (centered)
	if {[info exists Windows(tip)]} {
		set win [Window tip]
	    set x2 [expr {([winfo screenwidth $win] - [winfo reqwidth $win]) / 2 \
		    - [winfo vrootx $win] + $offset}]
	    set y2 [expr {([winfo screenheight $win] - [winfo reqheight $win]) / 3 \
		    - [winfo vrooty $win]}]
	    wm geometry $win +$x2+$y2
	}

	NSWindowManager::ArrangeAll

	# Iterate over each window. Move the mapped window from its offscreen
	# position onto the screen again. If it wasn't mapped previously, then
	# hide the window.
	foreach window [array names Windows] {
		set win [Window $window]

		scan [wm geometry $win] {%dx%d%[+-]%d%[+-]%d} width height xs x ys y
		incr x -$offset
		wm geometry $win ${width}x$height$xs$x$ys$y

		if {$visible($window)} continue

		# Tk 8.3.0 bug
		if {[string equal $window main]} continue

		wm withdraw $win
	}

	# Tk 8.3.0 bug
	if {!$visible(main)} {
		wm withdraw [Window main]
	}

	update

	return
}

if {[Platform unix]} {

proc HardcodeGeometry {} {

	global Windows

	set screenWidth [winfo screenwidth .]
	set spacing 0

	# Tk 8.3.0 bug
	set win [Window main]
	set visible(main) [winfo ismapped $win]
	if {!$visible(main)} {
		wm state $win normal
	}

	# Move each window offscreen, and show it.
	foreach window [array names Windows] {

		# Tk 8.3.0 bug
		if {[string equal $window main]} continue

		set win [Window $window]
		set visible($window) [winfo ismapped $win]
		if {!$visible($window)} {
			wm state $win normal
		}
	}

	# Must be update, not "update idletasks", or the Main Window geometry
	# is silly (too tall). Don't know why.
	update

	# Message Window
	set win1 [Window message]
	wm geometry $win1 +0+0
	update

	# Main Window
	set x 0
	if {[Value message,float]} {
		set y [expr {[NSToplevel::FrameBottom $win1] + $spacing}]
	} else {
		set y 0
	}
	set win1 [Window main]
	wm geometry $win1 +$x+$y
	update

	# Misc Window
	set y [expr {[NSToplevel::FrameBottom $win3] + $spacing}]
	set win1 [Window misc]
	wm geometry $win1 +$x+$y
	update

	# Progress Window
	set y [expr {[NSToplevel::FrameBottom $win1] + $spacing}]
	set win1 [Window progress]
	wm geometry $win1 +$x+$y
	update

	# Message Window (width)
	set win2 [Window message]
	set x 0
	set width [NSToplevel::ContentWidth $win2 \
		[expr {0 - $x}]]
	wm geometry $win2 ${width}x[winfo height $win2]+$x+0
	update

	# Tips Window (centered)
	if {[info exists Windows(tip)]} {
		set win [Window tip]
	    set x2 [expr {([winfo screenwidth $win] - [winfo reqwidth $win]) / 2 \
		    - [winfo vrootx $win]}]
	    set y2 [expr {([winfo screenheight $win] - [winfo reqheight $win]) / 3 \
		    - [winfo vrooty $win]}]
	    wm geometry $win +$x2+$y2
	}

	NSWindowManager::ArrangeAll

	# Iterate over each window. Move the mapped window from its offscreen
	# position onto the screen again. If it wasn't mapped previously, then
	# hide the window.
	foreach window [array names Windows] {
		set win [Window $window]

		scan [wm geometry $win] {%dx%d%[+-]%d%[+-]%d} width height xs x ys y

		if {!$visible($window)} {

			# Tk 8.3.0 bug
			if {![string equal $window main]} {
				wm withdraw $win
				update idletasks
			}
		}
	}

	# Tk 8.3.0 bug
	if {!$visible(main)} {
		wm withdraw [Window main]
	}

	update

	return
}

# unix
}

# MaximizeWindows --
#
#	Calculates the largest possible size for the Main Window, resizes
#	the Main Window, then positions all the other game windows accordingly.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc MaximizeWindows {} {

	set left 0
	set top 0
	set right [winfo screenwidth .]
	set bottom [winfo screenheight .]

	# Start with the Main Window as big as the desktop
	set width $right
	set height $bottom

	set winMessage [Window message]
	set winMain [Window main]
	set winMisc [Window misc]
	set winRecall [Window recall]

	# Subtract the width of the  Misc Window.
	set width [expr {$width - $widthMisc}]

	# Sutbtract the height of the Message Window
	if {[Value message,float]} {
		set height [expr {$height - [NSToplevel::TotalHeight $winMessage]}]
	}

	# Subtract the height of the Recall Window or Choice Window
	if {[Value recall,show]} {
		set height [expr {$height - [NSToplevel::TotalHeight $winRecall]}]
	} elseif {[Value choicewindow,show]} {
		set height [expr {$height - [NSToplevel::TotalHeight [Window choice]]}]
	}

	# Subtract the height of other subwindows of the Main Window.
	# This is done because I know main,widget is gridded, and
	# cannot therefore simply set the height of the window itself.
	set grid [wm grid $winMain]
	if {[llength $grid]} {
		set height2 [winfo height [Global main,widget]]
		set height [expr {$height - ([winfo height $winMain] - $height2)}]
	}

	# Calculate the required dimensions of the content area of
	# the Main Window
	set width [NSToplevel::ContentWidth $winMain $width]
	set height [NSToplevel::ContentHeight $winMain $height]
	
	# If the Main Window is a gridded window, convert from dimensions in
	# pixels to dimensions in grid units.
	set grid [wm grid $winMain]
	if {[llength $grid]} {
		set gridWidth [lindex $grid 2]
		set gridHeight [lindex $grid 3]
		set width [expr {$width / $gridWidth}]
		set height [expr {$height / $gridHeight}]
	}

	# Set the geometry of the Main Window
	wm geometry $winMain ${width}x$height
	update idletasks

	if {[Value recall,show]} {
		# Grow the Recall Window vertically
		set height [expr {$bottom - [NSToplevel::TotalHeight $winMessage] \
			- [NSToplevel::TotalHeight $winMain]}]
		set height [NSToplevel::ContentHeight $winRecall $height]
		wm geometry $winRecall [winfo width $winRecall]x$height
	}

	# Arrange the other windows relative to the Main Window
	HardcodeGeometry

	return
}

# FixWindows --
#
#	Here's a stinker for you. In "inventory.tcl" the command CalcLineLength()
#	calls "winfo width $canvas". Well, if the Inventory Window was never
#	shown, then "winfo width $canvas" returns a useless value. Ditto for the
#	Store Window. So to avoid this problem (and any other similar ones)
#	I call this command to move all the windows offscreen, show them, then
#	hide them again, once, during startup. This isn't a problem when
#	starting up with HardcodeGeometry(), only when setting window positions
#	with ReadGeometryFile().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc FixWindows {} {

	global Windows

	set offset [winfo screenwidth .]

	# Can anyone make this simpler? It is completely beyond me.

	foreach window [array names Windows] {
		set win [Window $window]

		# "winfo geometry" returns bad x,y values, as do both
		# "winfo x" and "winfo"y" at this point...
		set geomList [split [wm geometry $win] +x]
		set x [lindex $geomList 2]
		set y [lindex $geomList 3]

		wm geometry $win +$offset+0
		update idletasks

#		wm deiconify $win
wm state $win normal

		# "update idletasks" does NOT work here
		update

		wm withdraw $win
		update idletasks

		wm geometry $win +$x+$y
		update idletasks
	}

	return
}

# GetDefaultGeometry --
#
#	Calculates the default geometry for one of the secondary game windows,
#	such as the Inventory, Character or Knowledge Window. The top edge is
#	the same as the Main Window, the left edge zero, and the right edge
#	is 800 pixels, or the width of the screen, whichever is less.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc GetDefaultGeometry {win reqWidth reqHeight} {

	set winMain [Window main]
	set screenWidth [winfo screenwidth $winMain]

	# If this is being called in HardcodeGeometry(), then the window
	# should be positioned offscreen.
	if {[winfo x $winMain] >= $screenWidth} {
		set x $screenWidth
	} else {
		set x 0
	}

	# If the Message Window is embedded, then windows should be
	# below it.
	if {[Value message,float]} {
		set y [NSToplevel::FrameTop $winMain]
	} else {
		set y [winfo rooty [Window main].message]
		incr y [winfo height [Window main].message]
		incr y 2
	}

	# BUG: When a window is minimized (not withdrawn), Win32 tells
	# us that rootx and rooty are "3000".

	# Calculate the width
	switch -- $reqWidth {
		main {
			set width [NSToplevel::TotalWidth $winMain]
			if {$width > 600} {
				set width 600
			}
			set width [NSToplevel::ContentWidth $win $width]
		}
		main2 {
			if {$screenWidth < 800} {
				set width [NSToplevel::ContentWidth $win $screenWidth]
			} else {
				set width [NSToplevel::TotalWidth $winMain]
				if {$width > 600} {
					set width 600
				}
				set width [NSToplevel::ContentWidth $win $width]
			}
		}
		reqwidth {
			set width [winfo reqwidth $win]
		}
		screen {
			if {$screenWidth > 600} {
				set width 600
			} else {
				set width $screenWidth
			}
			set width [NSToplevel::ContentWidth $win $width]
		}
		default {
			set width [NSToplevel::ContentWidth $win $reqWidth]
		}
	}

	# Calculate the height
	switch -- $reqHeight {
		main {
			set dy [expr {$y - [NSToplevel::FrameTop $winMain]}]
			set height [expr {[NSToplevel::TotalHeight $winMain] - $dy}]
			if {$height > 400} {
				set height 400
			}
			set height [NSToplevel::ContentHeight $win $height]
		}
		reqheight {
			set height [winfo reqheight $win]
		} 
		default {
			set height [NSToplevel::ContentHeight $win $reqHeight]
		}
	}

	return ${width}x$height+$x+$y
}


# StripCommon --
#
#	Remove matching elements in path2 from path1.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc StripCommon {path1 path2} {

	if {[file exists $path1]} {
		set path1 [LongName $path1]
	}
	if {[file exists $path2]} {
		set path2 [LongName $path2]
	}

	set list1 [file split $path1]
	set list2 [file split $path2]
	set len [llength $list2]
	return [lrange $list1 $len end]
}

# IsFileInX --
#
#	Determine whether path1 is a child of path2.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc IsFileInX {path1 path2 {deep 1}} {

	if {[file exists $path1]} {
		set path1 [LongName $path1]
	}
	if {[file exists $path2]} {
		set path2 [LongName $path2]
	}

	foreach elem1 [file split $path1] elem2 [file split $path2] {
		if {[Platform windows]} {
			set elem1 [string tolower $elem1]
			set elem2 [string tolower $elem2]
		}
		if {![string length $elem1]} {
			return 0
		}
		if {![string length $elem2]} {
			if {$deep} {
				return 1
			}
			if {[llength [file split $path1]] == [llength [file split $path2]] + 1} {
				return 1
			}
			return 0
		}
		if {[string compare $elem1 $elem2]} {
			return 0
		}
	}

	return 1
}

# IsUserFile --
#
#	Determine whether the given file is inside the lib/user directory.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc IsUserFile {path} {

	return [IsFileInX $path [PathUser] 0]
}

# IsFileInPath --
#
#	Determine whether the given file is a child of the [Path] directory.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc IsFileInPath {path} {

	return [IsFileInX $path [PathTk]]
}


proc InitModules {} {

	NSModule::IndexLoad [PathTk moduleIndex.tcl]

	return
}

# InitOther --
#
#	The main initialization command.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc InitOther {} {

	global Angband
	
	InitLoadWindow
	
	angband_load progress 0.25
	angband_load note "Sourcing scripts..."
	
	Source keypress.tcl
	Source angband.tcl

	angband_load progress 0.45
	angband_load note "Initializing icons..."
	
	# Load the configuration files from the "current" set
	NSConfig::Load
	
	angband_load progress 0.75
	
	angband_load note "Initializing modules..."
	InitModules

	angband_load note "Initializing: Main Window"
	NSModule::LoadIfNeeded NSMainWindow
	angband_load progress 0.80
		
	angband_load note "Initializing: Aux Windows"
	NSModule::LoadIfNeeded NSMiscWindow
	angband_load progress 0.85

	angband_load note "Initializing: Recall"
	NSModule::LoadIfNeeded NSRecall
	
	angband_load note "Initializing: Info"
	NSObject::New NSInfoWindow

	# Synch windows with options
	if {[Value message,float]} {
		grid remove [Window main].message
	} else {
		Global message,message [Window main].message.message
	}
	if {[Value misc,float]} {
		grid remove [Window main].misc
	} else {
		Value misc,layout tall ; # Paranoia
		Global misc,canvas [Window main].misc.misc
	}

	if {[Platform unix]} {
		if {![file exists [PathTk config geometry]]} {
			wm geometry [Window main] +0+0
			wm geometry [Window message] +0+0
			wm geometry [Window recall] +0+0
			wm geometry [Window misc] +0+0
			wm geometry [Window progress] +0+0
			update idletasks
		
			wm deiconify [Window main]
			if {[Value message,float]} {
				wm state [Window message] normal
			}
			if {[Value recall,show]} {
#				wm state [Window recall] normal
				NSWindowManager::Display recall
			}
			if {[Value misc,float]} {
				wm state [Window misc] normal
				if {[Value misc,layout] == "wide"} {
					wm state [Window progress] normal
				}
			}
		}
	# unix
	}
	
	angband_load progress 0.95
	if {[file exists [PathTk config geometry]]} {
		angband_load note "Reading geometry file..."
		ReadGeometryFile
#		FixWindows
	} else {
		angband_load note "Setting default window positions..."
		HardcodeGeometry
	}
	update idletasks

#	if {$::DEBUG} {
#		NSModule::AddModule NSDebug [PathTk debug.tcl]
#		NSModule::LoadIfNeeded NSDebug
#	}

	# The load window is obscured below
	angband_load progress 1.0
	
	# Show windows which should be shown
	if {[Platform unix]} {
		if {[file exists [PathTk config geometry]]} {
			wmDeiconify [Window main]
			if {[Value recall,show]} {
#				wmStateNormal [Window recall]
				NSWindowManager::Display recall
			}
			if {[Value misc,float]} {
				wmStateNormal [Window misc]
				if {[Value misc,layout] == "wide"} {
					wmStateNormal [Window progress]
				}
			}
			if {[Value message,float]} {
				wmStateNormal [Window message]
			}
		}
	}
	if {[Platform windows]} {
		wm deiconify [Window main]
		update
		if {[Value recall,show]} {
			NSWindowManager::Display recall
#			wm state [Window recall] normal
			update
		}
		if {[Value misc,float]} {
			wm state [Window misc] normal
			update
			if {[Value misc,layout] == "wide"} {
				wm state [Window progress] normal
				update
			}
		}
		if {[Value message,float]} {
			wm state [Window message] normal
			update
		}
	}
	if {[Value choicewindow,show]} {
		NSModule::LoadIfNeeded NSChoiceWindow
		NSWindowManager::Display choice
	}
	if {[Value message2window,show]} {
		NSModule::LoadIfNeeded NSMessageWindow
		NSWindowManager::Display message2
	}
	
	update

	# Focus on Main Window
	# It seems important to do this before hiding the Load Window,
	# otherwise the application swaps into the background
	focus [Window main]
	
	# Done with "load" window
	angband_load kill

	# Show the Tips Window if desired
	if {[Value tip,show]} {
		NSModule::LoadIfNeeded NSTips
	}

	return
}

# Main initialization command
InitOther
