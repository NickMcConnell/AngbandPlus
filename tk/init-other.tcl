# File: init-other.tcl

# Purpose: post-character-generation initialization script

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

if 0 {
rename focus old_focus
proc focus {args} {
	dbwin "focus $args\n"
	return [eval old_focus $args]
}
}

# Toplevel headaches (Win32, Tk 8.3.3):
# If a window is withdrawn, then "wm geometry $w" returns the geometry
# from the last time the window was visible, *not* any requested geometry.
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
		if {[string index $geomInfo 0] eq "#"} continue

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
		if {$state eq "normal"} {
			wm withdraw $win
			update idletasks
		}

		# Set the toplevel geometry
		tryGeometry $win $geometry

		# Restore the window if it was hidden by us
		if {$state eq "normal"} {
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
		foreach name [array names ::NSWindowManager::Priv *,geomRequest] {
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

			# If the window was created but not displayed, don't save the
			# geometry. This doesn't affect the geomRequest stuff above.
			if {[info exists ::NSWindowManager::Priv($window,setup)]} {
				if {!$::NSWindowManager::Priv($window,setup)} continue
			}

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

Global HardcodeGeometry 0
Global WindowSpacing 0

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

	Global HardcodeGeometry 1

	set offset [OffscreenOffsetX]
	set spacing [Global WindowSpacing]

	scan [ScreenArea] "%d %d %d %d" waLeft waTop waRight waBottom
	set waHeight [expr {$waBottom - $waTop}]
	set waWidth [expr {$waRight - $waLeft}]

	# Tk 8.3.0 bug
	set win [Window main]
wm geom $win "" ; # Aug 18, 2004
	set visible(main) [winfo ismapped $win]
	wm geometry $win +$offset+0
	update idletasks
	wm deiconify $win

	# Move each window offscreen, and show it.
	foreach window [array names Windows] {

		# Tk 8.3.0 bug
		if {$window eq "main"} continue

		set win [Window $window]
wm geom $win "" ; # Aug 18, 2004
		set visible($window) [winfo ismapped $win]
		wm geometry $win +$offset+0
		update idletasks
		wm deiconify $win
	}

	# Must be update, not "update idletasks", or the Main Window geometry
	# is silly (too tall). Don't know why.
	update

	# Message Window
	NSWindowManager::Arrange message

	# Micro Map Window
	NSWindowManager::Arrange micromap

	# Main Window

	# Main Window width is the screen width minus largest of Micromap, Misc, and
	# Progress windows (if any are visible). The width of the Progress window
	# is the same as for Misc
	set subtract 0
	if {[Value micromap,float]} {
		set width [NSToplevel::TotalWidth [Window micromap]]
		if {$width > $subtract} {
			set subtract $width
		}
	}
	if {[Value misc,float]} {
		set width [NSToplevel::TotalWidth [Window misc]]
		if {$width > $subtract} {
			set subtract $width
		}
	}
	if {[Value progress,float]} {
		set width [NSToplevel::TotalWidth [Window progress]]
		if {$width > $subtract} {
			set subtract $width
		}
	}
	set width [NSToplevel::ContentWidth [Window main] [expr {$waWidth - $subtract}]]

	# Main Window height is the height of the screen minus height of recall/choice/message
	# if visible
	set subtract1 0
	set subtract2 0
	if {[Value message,float]} {
		set subtract1 [NSToplevel::TotalHeight [Window message]]
	}
	if {[Value recall,show]} {
		set height [NSToplevel::TotalHeight [Window recall]]
		if {$height > $subtract2} {
			set subtract2 $height
		}
	}
	if {[Value choicewindow,show]} {
		set height [NSToplevel::TotalHeight [Window choice]]
		if {$height > $subtract2} {
			set subtract2 $height
		}
	}
	set height [NSToplevel::ContentHeight [Window main] [expr {$waHeight - $subtract1 - $subtract2}]]
	set x $waLeft ; incr x $offset
	if {[Value message,float]} {
		set y [expr {[NSToplevel::FrameBottom [Window message]] + $spacing}]
	} else {
		set y $waTop
	}
	wm geometry [Window main] ${width}x$height+$x+$y
	update idletasks

	# Misc Window
	NSWindowManager::Arrange misc

	# Progress Window
	NSWindowManager::Arrange progress

	# Tips Window (centered)
	if {[info exists Windows(tip)]} {
		set win [Window tip]
		set x2 [expr {([ScreenWidth $win] - [winfo reqwidth $win]) / 2 \
			- [winfo vrootx $win] + $offset}]
		set y2 [expr {([ScreenHeight $win] - [winfo reqheight $win]) / 3 \
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
		if {$window eq "main"} continue

		wm withdraw $win
	}

	# Tk 8.3.0 bug
	if {!$visible(main)} {
		wm withdraw [Window main]
	}

	update

	Global HardcodeGeometry 0

	return
}

if {[Platform unix]} {

proc HardcodeGeometry {} {

	global Windows

	Global HardcodeGeometry 1

	set offset [OffscreenOffsetX]

	scan [ScreenArea] "%d %d %d %d" waLeft waTop waRight waBottom
	set waHeight [expr {$waBottom - $waTop}]
	set waWidth [expr {$waRight - $waLeft}]

	# Tk 8.3.0 bug
	set win [Window main]
wm geom $win "" ; # Aug 18, 2004
	set visible(main) [winfo ismapped $win]
	if {!$visible(main)} {
		wm state $win normal
	}

	# Move each window offscreen, and show it.
	foreach window [array names Windows] {

		# Tk 8.3.0 bug
		if {$window eq "main"} continue

		set win [Window $window]
wm geom $win "" ; # Aug 18, 2004
		set visible($window) [winfo ismapped $win]
		if {!$visible($window)} {
			wm state $win normal
		}
	}

	# Must be update, not "update idletasks", or the Main Window geometry
	# is silly (too tall). Don't know why.
	update

	# Message Window
	NSWindowManager::Arrange message

	# Micro Map Window
	NSWindowManager::Arrange micromap

	# Main Window

	# Main Window width is the screen width minus largest of Micromap, Misc, and
	# Progress windows (if any are visible). The width of the Progress window
	# is the same as for Misc
	set subtract 0
	if {[Value micromap,float]} {
		set width [NSToplevel::TotalWidth [Window micromap]]
		if {$width > $subtract} {
			set subtract $width
		}
	}
	if {[Value misc,float]} {
		set width [NSToplevel::TotalWidth [Window misc]]
		if {$width > $subtract} {
			set subtract $width
		}
	}
	if {[Value progress,float]} {
		set width [NSToplevel::TotalWidth [Window progress]]
		if {$width > $subtract} {
			set subtract $width
		}
	}
	set width [NSToplevel::ContentWidth [Window main] [expr {$waWidth - $subtract}]]

	# Main Window height is the height of the screen minus height of recall/choice/message
	# if visible
	set subtract1 0
	set subtract2 0
	if {[Value message,float]} {
		set subtract1 [NSToplevel::TotalHeight [Window message]]
	}
	if {[Value recall,show]} {
		set height [NSToplevel::TotalHeight [Window recall]]
		if {$height > $subtract2} {
			set subtract2 $height
		}
	}
	if {[Value choicewindow,show]} {
		set height [NSToplevel::TotalHeight [Window choice]]
		if {$height > $subtract2} {
			set subtract2 $height
		}
	}
	set height [NSToplevel::ContentHeight [Window main] [expr {$waHeight - $subtract1 - $subtract2}]]
	set x $waLeft ; incr x $offset
	if {[Value message,float]} {
		set y [NSToplevel::FrameBottom [Window message]]
	} else {
		set y $waTop
	}
	wm geometry [Window main] ${width}x$height+$x+$y
	update idletasks

	# Misc Window
	NSWindowManager::Arrange misc

	# Progress Window
	NSWindowManager::Arrange progress

	# Tips Window (centered)
	if {[info exists Windows(tip)]} {
		set win [Window tip]
	    set x2 [expr {([ScreenWidth $win] - [winfo reqwidth $win]) / 2 \
		    - [winfo vrootx $win]}]
	    set y2 [expr {([ScreenHeight $win] - [winfo reqheight $win]) / 3 \
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
			if {$window ne "main"} {
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

	Global HardcodeGeometry 0

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

	# Get the desktop area. On MS Windows this command takes into account
	# the screen area consumed by the Taskbar. On the Macintosh the menu
	# bar should be accounted for.
	scan [ScreenArea] "%d %d %d %d" left top right bottom

	# Start with the Main Window as big as the desktop
	set width $right
	set height $bottom

	set winMessage [Window message]
	set winMain [Window main]
	set winMicro [Window micromap]
	set winMisc [Window misc]
	set winRecall [Window recall]

	# Subtract the width of the Micro Map Window, or the Misc Window,
	# whichever is the greater.
	if {[Value micromap,float]} {
		set widthMicro [NSToplevel::TotalWidth $winMicro]
	} else {
		set widthMicro 0
	}
	if {[Value misc,float]} {
		set widthMisc [NSToplevel::TotalWidth $winMisc]
	} else {
		set widthMisc 0
	}
	if {$widthMicro > $widthMisc} {
		set width [expr {$width - $widthMicro}]
	} else {
		set width [expr {$width - $widthMisc}]
	}

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

	if {[Value micromap,float]} {
		# Grow the Micro Map Window
		set width [expr {$right - [NSToplevel::TotalWidth $winMain]}]
		set width [NSToplevel::ContentWidth $winMicro $width]
		set height [expr {$bottom - $top}]
		if {[Value message,float]} {
			incr height -[NSToplevel::TotalHeight $winMessage]
		}
		if {[Value misc,float]} {
			incr height -[NSToplevel::TotalHeight $winMisc]
			if {[Value misc,layout] eq "wide"} {
				incr height -[NSToplevel::TotalHeight [Window progress]]
			}
		}
		set height [NSToplevel::ContentHeight $winMicro $height]
		if {$height > [Value micromap,scale] * 66} {
			set height [expr {[Value micromap,scale] * 66}]
		}
		wm geometry $winMicro ${width}x$height
	}

	if {[Value recall,show]} {
		# Grow the Recall Window vertically
		set height [expr {$bottom - [NSToplevel::TotalHeight $winMain]}]
		if {[Value message,float]} {
			incr height -[NSToplevel::TotalHeight $winMessage]
		}
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

	set offset [ScreenWidth]

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
	set screenWidth [ScreenWidth $winMain]
	set offscreenX [OffscreenOffsetX]

	scan [ScreenArea] "%d %d %d %d" waLeft waTop waRight waBottom
	set waHeight [expr {$waBottom - $waTop}]
	set waWidth [expr {$waRight - $waLeft}]

	# If the Misc Window is embedded, then windows should be
	# to the right of it.
	if {[Value misc,float]} {
		set x 0
		set widthOfMisc 0
	} else {
if 1 { # Apr 26 2009
		set widthOfMisc [winfo width [Window main].misc]
		set x [expr {[winfo rootx [Window main].misc] + $widthOfMisc}]
		if {[Global HardcodeGeometry]} {
			incr x -$offscreenX
		}
} else {
		set x [expr {[winfo rootx [Window main].misc] - [winfo rootx [Window main]]}]
		set widthOfMisc [winfo width [Window main].misc]
		incr x $widthOfMisc
}
	}

	# If this is being called in HardcodeGeometry(), then the window
	# should be positioned offscreen.
	if {[Global HardcodeGeometry]} {
		incr x $offscreenX
	}
	incr x $waLeft

	# If the Message Window is embedded, then windows should be
	# below it.
	if {[Value message,float]} {
		set y [NSToplevel::FrameTop $winMain]
	} else {
		set y [winfo rooty [Window main].message]
		incr y [winfo height [Window main].message]
# Oct 26 2004
#		incr y 2
	}

	# Make room for the "ask" display
	incr y [NSAsk::ReqHeightOfAskWindow]

	# BUG: When a window is minimized (not withdrawn), Win32 tells
	# us that rootx and rooty are "3000".

	set maxWindowWidth 600
	if {$screenWidth > 1280} {
		set maxWindowWidth 800
	}

	# Calculate the width
	switch -- $reqWidth {
		main {
			set width [NSToplevel::TotalWidth $winMain]
			incr width -$widthOfMisc
			if {$width > $maxWindowWidth} {
				set width $maxWindowWidth
			}
			set width [NSToplevel::ContentWidth $win $width]
		}
		main2 {
			if {$screenWidth < 800} {
				set width [NSToplevel::ContentWidth $win $waWidth]
			} else {
				set width [NSToplevel::TotalWidth $winMain]
				incr width -$widthOfMisc
				if {$width > $maxWindowWidth} {
					set width $maxWindowWidth
				}
				set width [NSToplevel::ContentWidth $win $width]
			}
		}
		reqwidth {
			set width [winfo reqwidth $win]
		}
		screen {
			if {$waWidth > $maxWindowWidth} {
				set width $maxWindowWidth
			} else {
				set width $waWidth
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
# Nov 5 2004
		main2 {
			set height [expr {[winfo rooty [Window main].statusBar] - $y}]
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

# ProcessPrefFile --
#
#	Get a filename from the user then read in the given pref file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc ProcessPrefFile {parent} {

	global Angband

	set filename [tk_getOpenFile -initialdir [PathUser] \
		-parent $parent]
	if {![string length $filename]} {
		return 1
	}

	if {![IsUserFile $filename]} {
		tk_messageBox -title "Pref File Error" -icon info -message \
			"Pref files must be located in the lib/user directory."
		return 1
	}

	set filename [file tail $filename]
	if {[catch {angband game process_pref_file $filename} result]} {
		tk_messageBox -title "Pref File Error" -icon error -message $result
		return 1
	}

	# Success
	return 0
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
		if {$elem1 ne $elem2} {
			return 0
		}
	}

	return 1
}

# IsPrefFile --
#
#	Determine whether the given file is inside the lib/pref directory.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc IsPrefFile {path} {

	return [IsFileInX $path [Path lib pref] 0]
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

# IsFileInCPath --
#
#	Determine whether the given file is a child of the [CPath] directory.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc IsFileInCPath {path} {

	return [IsFileInX $path [CPath]]
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

	return [IsFileInX $path [Path]]
}

# GlobSoundDirectory --
#
#	Gets a list of *.wav files in a sound directory.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc GlobSoundDirectory {dir} {

	# Get the full path for this directory
	set path [angband sound dir cget $dir -path]

	# No such directory
	if {![file exists $path] || ![file isdirectory $path]} {

		# There are no sounds here
		Global sound,fileName,$dir {}
		Global sound,fileLower,$dir {}
		Global sound,globCount,$dir 0

		# Done
		return
	}

	# Get a list of sound files
	# FIXME: get list of file types per sound lib
	set fileList [glob -nocomplain -directory $path -tails *.wav]
	set fileList [lsort -dictionary $fileList]

	# Get a lower-case copy of each name
	set fileLower {}
	foreach file $fileList {
		lappend fileLower [string tolower $file]
	}

	Global sound,fileName,$dir $fileList
	Global sound,fileLower,$dir $fileLower

	# This lets other modules know that the list of sounds changed.
	if {[info exists ::Global(sound,globCount,$dir)]} {
		incr ::Global(sound,globCount,$dir)
	} else {
		Global sound,globCount,$dir 1
	}

	return
}

# CorrectSoundAssign --
#
#	Given a list of sound assignments, return the same list but
#	ensuring that each element matches the same case as the file names
#	in the global list of file names. This is done because Windows
#	file names are not case sensitive.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc CorrectSoundAssign {soundList} {

	# Filenames are case-sensitive on Unix
	if {[Platform unix]} {
		return $soundList
	}

	set result {}

	foreach {dir snd} $soundList {
		if {[string length $snd]} {
			# Only do this if we have read the list of filenames from
			# the sound directory.
			if {[info exists ::Global(sound,fileName,$dir)]} {
				set fileName [Global sound,fileName,$dir]
				set fileLower [Global sound,fileLower,$dir]
				if {[lsearch -exact $fileName $snd] == -1} {
					set index [lsearch -exact $fileLower [string tolower $snd]]
					if {$index != -1} {
						set snd [lindex $fileName $index]
					}
				}
			}
		}

		lappend result $dir $snd
	}

	return $result
}

# Playing sounds on Windoze is totally f*cked, in my opinion. Microsoft
# has managed to require you to have an active window or you cannot
# play a sound, and only one application can play a sound at once. Since
# my app has a lot of windows, I bind to the <Focus> event which
# activates and deactivates the sound system as window's claim and
# lose the input focus. This is not needed for DirectSound, which
# thankfully recognizes child windows.

proc SoundSuspend {} {

	Global sound,afterId ""
	if {[Global sound,suspended]} return
dbwin SoundSuspend\n

	angband sound stop

	angband sound activate no
	if {0 && [Value music,play]} {
		music suspend
	}
	Global sound,suspended 1

	if {[Global sound,ambient,afterId] ne ""} {
		after cancel [Global sound,ambient,afterId]
		Global sound,ambient,afterId ""
	}

	return
}

proc SoundResume {} {

	after cancel [Global sound,afterId]
	Global sound,afterId ""
	if {![Global sound,suspended]} return
dbwin SoundResume\n
	angband sound activate yes
	if {0 && [Value music,play]} {
		music resume
	}
	Global sound,suspended 0

	if {[Setting use_sound] && [Setting ambient_delay]} {
		AmbientTimerSchedule
	}

	return
}

# Initialize sound, telling the game which DLL the user wants to use.
# I still don't know how to detect if a computer actually has a
# sound card.
proc InitSound {} {

	global Angband

	if {[Platform windows]} {
		Global sound,afterId ""
		Global sound,suspended 1

		Global sound,ambient,afterId ""

		bind Toplevel <FocusOut> {+
			if {[Global sound,afterId] ne ""} {
				after cancel [Global sound,afterId]
			}
			Global sound,afterId [after 1000 SoundSuspend]
		}

		bind Toplevel <FocusIn> {+
			SoundResume
		}
	}

	set ext [info sharedlibextension]

	if {[Value sound,wants]} {
		set soundDll sound-[Value sound,dll]$ext

		if {![file exists [CPath lib $soundDll]]} {
			tk_messageBox -title "Sound Problem" -type warning \
				-message "Could not load the requested sound library.\nThe file \"$soundDll\" does not exist."
			set soundDll sound-nocard$ext
			Value sound,wants 0

			# Hack -- Disable useless processing
			Setting use_sound 0
			Value use_sound 0
		}

	# The user doesn't want any sound
	} else {
		set soundDll sound-nocard$ext

		# Hack -- Disable useless processing
		Setting use_sound 0
		Value use_sound 0
	}

	angband system load_hack [CPath lib $soundDll]
	angband sound init [CPath lib $soundDll]

	# Restore saved settings
	angband sound volume [Value sound,volume]

	# We want sound
	if {[Value sound,wants]} {

		# Read tk\config\sound
		if {[file exists [PathTk config sound]]} {
			Config::Sound::Source [PathTk config sound]
		}
	}

	return
}

proc AmbientTimerSchedule {} {
	if {[Global sound,ambient,afterId] ne ""} {
		after cancel [Global sound,ambient,afterId]
	}
	set ms [expr {[Setting ambient_delay] * 1000}]
	Global sound,ambient,afterId [after $ms AmbientTimer]
	return
}

proc AmbientTimer {} {

dbwin "AmbientTimer\n"

	Global sound,ambient,afterId ""

	# Sound (or ambient sound) is disabled
	if {![Setting use_sound] || ![Setting ambient_delay]} return

	if {![angband player depth]} {
		set day [angband cave day]
		if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
			angband sound play2 event town_$day 0x02
		}
		if {[variant ZANGBANDTK]} {
			angband sound play2 event town_$day 0x00
			if {![struct set player_type 0 town_num]} {
				angband sound play2 event wild_$day 0x00
			}
			angband sound flush
		}
	} else {
		angband sound play2 event dungeon 0x02
	}

	# Reschedule the timer
	AmbientTimerSchedule

	return
}

proc InitAmbientSound {} {

	if {[Platform unix]} {
		Global sound,ambient,afterId ""
	}

	if {[Setting use_sound] && [Setting ambient_delay]} {
		AmbientTimerSchedule
	}

	qebind Ambient <Setting-ambient_delay> {
		if {%c && ([Global sound,ambient,afterId] eq "")} {
			AmbientTimerSchedule
		}
	}
	qebind Ambient <Setting-use_sound> {
		if {%c && ([Global sound,ambient,afterId] eq "")} {
			AmbientTimerSchedule
		}
	}

	return
}

proc InitModules {} {

	NSModule::IndexLoad [CPathTk moduleIndex.tcl]

	set path [Path borg borg.tcl]
	if {[file exists $path]} {
		NSModule::AddModule NSBorg $path
	}

	set path [CPathTk vault vault-editor.tcl]
	if {[file exists $path]} {
		NSModule::AddModule NSVaultEditor $path
	}

	return
}

Source playlist.tcl
namespace import ::NSPlaylist::playlist

# Try to load the music package. Do this before creating the Main Window,
# so various commands are not made available to the user.
proc LoadMusic {} {

	# The user wants music
	if {[Value music,wants]} {

		# Load music-xxx.dll
		if {[catch {
			set musicDll music-[Value music,dll][info sharedlibextension]
			angband system load_hack [CPath lib $musicDll]
			load [CPath lib $musicDll] music
			Global music,avail 1

			# Restore saved settings
			music volume [Value music,volume]
			music mute [Value music,mute]

		# An error occured. Save the error and report it later
		} result]} {
			proc music {args} {}
			Global music,avail 0
			Global music,error $result
		}

	# The user doesn't want music
	} else {
		proc music {args} {}
		Global music,avail 0
	}

	return
}

proc PlaySong {song} {

	global Global

	# Is music desired?
#	if {![Value music,play]} return

	# Make sure the file exists
	if {![file exists $song]} return

	# Play the song, catching errors
	if {[catch {
		music play $song

		# Remember when it began playing
		Global music,time [clock seconds]

		if {[lsearch -exact [Global music,played] $song] == -1} {
			lappend Global(music,played) $song
		}

		qegenerate <Music-play>

	# Failure: inform the user
	} result]} {
		Global music,time 0
		tk_messageBox -parent [Window main] -title "Music Error" \
			-message "The song \"$song\" could not be played because\
				of the following error:\n\t$result"
	}

	return
}

proc QuestNumber {} {

	# FIXME: themed level for OAngband, Sauron/Morgoth for A/O

if {[variant KANGBANDTK ZANGBANDTK]} {

	if {[angband player inside_quest]} {
		return [angband player inside_quest]
	}

	set level [angband player depth]

	set q_match [struct find quest_type \
		-field status == QUEST_STATUS_TAKEN \
		-field type == QUEST_TYPE_KILL_LEVEL \
		-field flags !& QUEST_FLAG_PRESET \
		-field level == $level]
	if {[llength $q_match]} {
		return [lindex $q_match 0]
	}

	set q_match [struct find quest_type \
		-field status == QUEST_STATUS_TAKEN \
		-field type == QUEST_TYPE_RANDOM \
		-field level == $level]
	if {[llength $q_match]} {
		return [lindex $q_match 0]
	}
}

	return -1
}

proc PlayRandomSong {} {

	# Is music desired?
	if {![Value music,play]} return

	# Get a list of playlists for the current level
	if {[QuestNumber] != -1} {
		set where quest
	} elseif {![angband player depth]} {
		set where town
if {[variant ZANGBANDTK]} {
		if {![struct set player_type 0 town_num]} {
			set where wild
		}
}
	} else {
		set where dungeon
	}
if {[variant KANGBANDTK ZANGBANDTK]} {
	if {[angband player inside_arena]} {
		set where arena
	}
}
	set day [angband cave day]
	set level [angband player depth]
	set playlists [NSPlaylist::GetActivePlaylists $where $day $level]

	# No active playlists
	if {![llength $playlists]} return

	# Get a list of songs. Make sure the file exists
	set songList {}
	foreach playlistId $playlists {
		foreach song [playlist songs $playlistId] {
			if {![file exists $song]} continue
			lappend songList $song
		}
	}

	# No songs to play
	if {![llength $songList]} return

	set playedList [Global music,played]

	# Get a list of songs which haven't been played
	set notPlayedList {}
	foreach song $songList {
		if {[lsearch -exact $playedList $song] == -1} {
			lappend notPlayedList $song
		}
	}

	# All the songs were played
	if {![llength $notPlayedList]} {
		Global music,played {}
		PlayRandomSong
		return
	}

	# Pick a random song
	set songNumber [expr {int(rand() * [llength $notPlayedList])}]
	PlaySong [lindex $notPlayedList $songNumber]

	return
}

proc InitMusic {} {

	Global music,time 0
	Global music,played {}

	# The music package is not available
	if {![Global music,avail]} {

		# If an error occured during startup, then display a message
		if {[Value music,wants] && ![Value warning,music]} {
			tk_messageBox -parent [Window main] -title "Music Error" \
				-message "Music is not available because of the following\
				error:\n[Global music,error]"

			# Don't display this warning again
			Value warning,music 1
		}

		# Done
		return
	}

	# Create a new <Music> quasi-event to synchronize app with music
	qeinstall <Music-notify>
	qeinstall <Music-pause>
	qeinstall <Music-play>
	qeinstall <Music-resume>
	qeinstall <Music-stop>

	# Tell music.dll to notify app when a song finishes.
	# <Music-notify> is *not* generated when a song is stopped with [music stop]
	music notify {
		qegenerate <Music-notify>
	}

	# Call PlayRandomSong when a song finishes
	qebind MusicBindtag <Music-notify> PlayRandomSong

	# Start a new song when the dungeon level changes if no song is playing.
	# This is needed because there may be no songs for the previous level.
	qebind MusicBindtag <Dungeon-enter> {
		if {[Value music,play]} {
			if {![music issong]} {
				PlayRandomSong
			}
		}
	}

	# Read tk\config\music
	if {[file exists [PathTk config music]]} {
		Config::Music::Source [PathTk config music]
	}

	if {[Value music,play]} {
		PlayRandomSong
	}

	return
}

# InitBatFile --
#
#	On Win32, create a BAT file that will run the game with the current
#	variant and savefile. On Unix, a shell script
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc InitBatFile {} {

	switch -- [angband game variant] {
		ANGBANDTK { set name ang }
		KANGBANDTK { set name kang }
		OANGBANDTK { set name oang }
		ZANGBANDTK { set name zang }
	}
	set variant [file tail [Path]]

	if {[Platform unix]} {
		set path [CPath run$name.sh]
	}
	if {[Platform windows]} {
		set path [CPath run$name.bat]
	}

	# Note: Must be Win32 EOL
	if {[catch {open $path w} chan]} {
		return
	}

	if {[Platform unix]} {
		puts $chan "# Automatically generated."
		puts $chan "# Run the game using the most recent variant and savefile."
		puts $chan "./angband -variant $variant -savefile \"[angband game savefile]\""
	}
	if {[Platform windows]} {
		# "Echo off" seems needed to hide the DOS window.
		# I would like the DOS window never to appear. - not on XP?
		# On XP I add 'start' which lets the .bat file close itself
		puts $chan "@echo off"
		puts $chan "rem Automatically generated."
		puts $chan "rem Run the game using the most recent variant and savefile."
		puts $chan "start angband.exe -variant $variant -savefile \"[angband game savefile]\" %1 %2 %3 %4 %5 %6 %7 %8"
	}

	close $chan

	if {[Platform unix]} {
		file attributes $path -permissions rwxr--r--
	}

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

	angband_load progress 0.25
	angband_load note "Sourcing scripts..."

	Source keypress.tcl
	Source angband.tcl

	# Set some options here. This is not a good thing.
	angband setting set hilite_player no
	angband setting set view_yellow_lite yes
	angband setting set view_bright_lite yes
	angband setting set view_granite_lite yes
	angband setting set view_special_lite yes

	angband_load progress 0.45
	angband_load note "Initializing icons..."

	# Load the configuration files from the "current" set
	NSConfig::Load

	angband_load progress 0.75

	if {[file exists [PathTk config feature]]} {
		angband_load prompt "Reading feature file..."
		Config::Feature::Source [PathTk config feature] 
	}

	# g_feat_flag is not initialized until tk/config/feature is read in.
	# Any call to town_illuminate() in C will have failed.
	# In ZAngband some races start at night.
	if {[Global isNewGame] && ![angband player depth]} {
		angband cave town_illuminate
	}

	angband_load prompt "Initializing sound..."
	InitSound

	angband_load note "Initializing modules..."
	InitModules

	# XXX Hack -- If this is a new character, then read in the "default"
	# settings from the tk/config/setting file
	dbwin "isNewGame is [Global isNewGame]\n"
	if {[Global isNewGame] && [file exists [PathTk config setting]]} {
		Config::Setting::Source [PathTk config setting]
	}

	LoadMusic

	if {[file exists [PathTk config msgcolor]]} {
		Config::MsgColor::Source [PathTk config msgcolor]
	}

	angband_load prompt "Initializing: Main Window"
	NSModule::LoadIfNeeded NSMainWindow
	angband_load progress 0.80

	angband_load prompt "Initializing: Status"
	NSModule::LoadIfNeeded NSStatus

	angband_load prompt "Initializing: Aux Windows"
	NSModule::LoadIfNeeded NSMiscWindow
	NSModule::LoadIfNeeded NSAsk
	NSModule::LoadIfNeeded NSPopup
	angband_load progress 0.85

	angband_load prompt "Initializing: Recall"
	NSModule::LoadIfNeeded NSRecall

#	angband_load prompt "Initializing: Info"
#	NSObject::New NSInfoWindow

	# Synch windows with options
	if {[Value message,float]} {
		grid remove [Window main].message
	} else {
		Global message,message [Window main].message.message
	}
	if {[Value micromap,float]} {
		qeconfigure MicroMap <Position> -active yes
	} else {
		qeconfigure MicroMap <Position> -active no
	}
	if {[Value misc,float]} {
		grid remove [Window main].misc
	} else {
		grid [Window main].misc
	}
	if {[Value progress,float]} {
	} elseif {[Value misc,float]} {
		NSMiscWindow::AttachProgress [Global misc,oop]
	} else {
		NSMainWindow::AttachProgress [Global main,oop]
	}

	if {[Value choicewindow,show]} {
		NSModule::LoadIfNeeded NSChoiceWindow
	}

	# Oct 25 2004
	# Do this here *after* any town vault is loaded since the character
	# position may have changed.
	Global main,widget,center [WidgetCenter [Global main,widget]]
	WidgetCenter [Global micromap,widget]

	if {[Platform unix]} {
		if {![file exists [PathTk config geometry]]} {
			wm geometry [Window main] +0+0
			wm geometry [Window message] +0+0
			wm geometry [Window recall] +0+0
			wm geometry [Window micromap] +0+0
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
			if {[Value micromap,float]} {
				wm state [Window micromap] normal
			}
			if {[Value misc,float]} {
				wm state [Window misc] normal
				if {[Value misc,layout] eq "wide"} {
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

	# Nov 3 2004
	# Erase the widgets, and disable drawing until we enter the dungeon
	# Do this after the window geometry is set.
	foreach widget [list [Global main,widget] [Global micromap,widget]] {
		$widget configure -noupdate on -paintcolor black
		qebind $widget <Dungeon-enter> {
			%W configure -noupdate off -paintcolor ""
			qeunbind %W <Dungeon-enter>
		}
	}

	if {$::DEBUG} {
		NSModule::AddModule NSDebug [CPathTk debug.tcl]
		NSModule::AddModule NSDebugAlloc [CPathTk debug-alloc.tcl]
		NSModule::LoadIfNeeded NSDebug
		NSModule::LoadIfNeeded NSDebugAlloc
	}

	# The load window is obscured below
	angband_load progress 1.0

	# Show windows which should be shown
	if {[Platform unix]} {
		if {[file exists [PathTk config geometry]]} {
			wmDeiconify [Window main]
			if {[Value recall,show]} {
				NSWindowManager::Display recall
			}
			if {[Value micromap,float]} {
				NSWindowManager::Display micromap
			}
			if {[Value misc,float]} {
				NSWindowManager::Display misc
			}
			if {[Value progress,float]} {
				NSWindowManager::Display progress
			}
			if {[Value message,float]} {
				NSWindowManager::Display message
			}
		}
	}
	if {[Platform windows]} {
		wm deiconify [Window main]
		update
		if {[Value recall,show]} {
			NSWindowManager::Display recall
			update
		}
		if {[Value micromap,float]} {
			NSWindowManager::Display micromap
		}
		update
		if {[Value misc,float]} {
			NSWindowManager::Display misc
			update
		}
		if {[Value progress,float]} {
			NSWindowManager::Display progress
		}
		if {[Value message,float]} {
			NSWindowManager::Display message
		}
	}
	if {[Value choicewindow,show]} {
		NSWindowManager::Display choice
	}
	if {[Value message2window,show]} {
		NSModule::LoadIfNeeded NSMessagesWindow
		NSWindowManager::Display message2
	}

	update

	# Focus on Main Window
	# It seems important to do this before hiding the Load Window,
	# otherwise the application swaps into the background
	focus [Window main]

	# Done with "load" window
	angband_load kill

	# This call is only needed for DirectSound. It must appear after the
	# window was actually created or it bombs. It turns out that DirectSound
	# recognizes all the child windows of Main and plays sound when any
	# of child window is active. This call does nothing under WaveMix.
	angband sound window [Window main]

	# Show the Tips Window if desired
	if {[Value tip,show]} {
		NSModule::LoadIfNeeded NSTips
	}

	InitMusic

	### XXX Mega-Smegga-Hack -- A character record includes a "photo.txt"
	### entry, which describes what is seen by the character at the time
	### of death. The problem is that the monster and object lists are
	### wiped after dungeon(). So I create the photo.txt file whenever
	### leaving dungeon() and the character is dead.
	Global photoId 0
	Global photoText ""
	qebind PhotoFileHack <Dungeon-leave> {
		if {[angband player is_dead]} {
			NSModule::LoadIfNeeded NSPhoto
			Global photoText [NSUtils::TempFileName $Angband(dir)]
			Global photoId [NSObject::New NSPhoto]
			NSPhoto::ExamineWidget [Global photoId] [Global main,widget]
			NSPhoto::WritePhotoText [Global photoId] [Global photoText]
			NSObject::Delete NSPhoto [Global photoId]
		}
	}

	InitAmbientSound

	InitBatFile

	return
}

if {[catch {

	# Main initialization command
	InitOther

} error]} {
	HandleError $error
}



if {$DEBUG} {

# feature lighting -radius 1 -brightness -35 -contrast 0 -gamma 0.8
# feature lighting -radius 2 -brightness -80 -contrast 0 -gamma 0.8

# feature lighting -radius 1 -brightness -0 -contrast -40 -gamma 0.7
# feature lighting -radius 2 -brightness -0 -contrast -40 -gamma 0.4

proc AdjustBrightnessContrast {what value} {

	if {$what eq "-gamma"} {
		set value [expr {$value / 10.0}]
	}
	feature lighting -radius $::Radius $what $value
	[Global main,widget] wipe

	return
}

proc SetRadius {which} {

	set ::Radius $which
	scan [feature lighting -radius $::Radius] "%d %d %f" \
		::Brightness ::Contrast gamma
	set ::Gamma [expr {$gamma * 10.0}]

	return
}

proc InitBrightnessContrast {} {

	set win .brightnesscontrast
	toplevel $win
	wm withdraw $win

	scale $win.brightness \
		-orient horizontal -label "Brightness" -variable ::Brightness \
		-width 15 -sliderlength 20 -length 255 -from -127 -to 127 \
		-command "AdjustBrightnessContrast -brightness" -showvalue yes
	scale $win.contrast \
		-orient horizontal -label "Contrast"  -variable ::Contrast \
		-width 15 -sliderlength 20 -length 255 -from -127 -to 127 \
		-command "AdjustBrightnessContrast -contrast" -showvalue yes
	scale $win.gamma \
		-orient horizontal -label "Gamma"  -variable ::Gamma \
		-width 15 -sliderlength 20 -length 255 -from 0 -to 20 \
		-command "AdjustBrightnessContrast -gamma" -showvalue yes
	pack $win.brightness
	pack $win.contrast
	pack $win.gamma

	bind $win <KeyPress-1> {SetRadius 1}
	bind $win <KeyPress-2> {SetRadius 2}
	bind $win <KeyPress-3> {SetRadius 3}
}

SetRadius 1
InitBrightnessContrast

} elseif 1 {

	# Nothing

} else {

set darkenFile [CPathTk image darken.gif]
if {[file exists $darkenFile]} {
	image create photo Image_Darken -file $darkenFile
	for {set i 0} {$i < 3} {incr i} {
		set color {}
		for {set y 0} {$y < 16} {incr y} {
			for {set x 0} {$x < 16} {incr x} {
				set rgb [Image_Darken get [expr {$x + $i * 16}] $y]
				lappend color [eval format #%02x%02x%02x $rgb]
			}
		}
		angband tint $i $color
	}
	image delete Image_Darken
}

}

if 0 {

catch {
for {set y 0} {$y < 16} {incr y} {
	set row {}
	for {set x 0} {$x < 16} {incr x} {
		lappend row [palette set [expr {$x + $y * 16}]]
	}
	lappend data $row
}
image create photo Palette -height 16 -width 16 -palette 256/256/256
Palette put $data -to 0 0
toplevel .palette
label .palette.label -image Palette
pack .palette.label
} result
dbwin $result

}

# Dump a list of vaults to a text window. I found a bug in a vault
# by doing this (the height and width were reversed). We could
# easily print out the vaults sorted by size, by type (lesser/greater) and
# by rating!
proc vaultHack {} {

	toplevel .vault
	set textBox [text .vault.text -font {Courier 9}]
	pack $textBox
	update
	set max [debughook vault max]
	for {set i 0} {$i < $max} {incr i} {
		debughook vault info $i attrib
		set height $attrib(height)
		set width $attrib(width)
		set text $attrib(text)
		$textBox insert end "N:$i:$attrib(name)\n"
		$textBox insert end "X:$attrib(type):$attrib(rating):$height:$width\n"
		set offset 0
		for {set y 0} {$y < $height} {incr y} {
			$textBox insert end "D:[string range $text $offset \
				[expr {$offset + $width - 1}]]\n"
			incr offset $width
		}
		$textBox insert end \n
	}
}

