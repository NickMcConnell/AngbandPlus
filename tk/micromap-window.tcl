# File: micromap-window.tcl

# Purpose: the Micro Map Window

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSMicroMapWindow {

# namespace eval NSMicroMapWindow
}

# NSMicroMapWindow::InitModule -- 
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMicroMapWindow::InitModule {} {

	variable Priv

	MsgCatInit misc-win

	NSObject::New NSMicroMapWindow

	return
}

# NSMicroMapWindow::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMicroMapWindow::CloseModule {} {

	catch {
		destroy [Window micromap]
	}

	return
}

# NSMicroMapWindow::NSMicroMapWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMicroMapWindow::NSMicroMapWindow {oop} {

	InitWindow $oop

	BalloonInit $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow micromap $win \
		"NSMicroMapWindow::GeometryCmd $oop" "" \
		"NSMicroMapWindow::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSMicroMapWindow $oop $win

	#
	# Global list of application windows
	#

	Global micromap,oop $oop
	Window micromap $win

	return
}

# NSMicroMapWindow::~NSMicroMapWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMicroMapWindow::~NSMicroMapWindow {oop} {

	return
}

# NSMicroMapWindow::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMicroMapWindow::Info {oop info args} {

	global NSMicroMapWindow

	# Verify the object
	NSObject::CheckObject NSMicroMapWindow $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSMicroMapWindow($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSMicroMapWindow($oop,$info)
			}
		}
	}

	return
}

# NSMicroMapWindow::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMicroMapWindow::InitWindow {oop} {

	set win .micromap$oop
	toplevel $win
	wm title $win [mc "Micro Map"]
	wm resizable $win yes yes

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	wm protocol $win WM_DELETE_WINDOW {
		NSMainWindow::Info [Global main,oop] mapWindow 0
		NSMainWindow::MenuInvoke [Global main,oop] ignore E_WINDOW_MAP
	}
	Term_KeyPress_Bind $win

	Info $oop win $win

	set child [InitDisplay $win]
#	place $child -relx 0.5 -rely 0.5 -anchor center
	pack $child -expand yes -fill both

	return
}

# NSMicroMapWindow::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMicroMapWindow::DisplayCmd {oop message first} {

	switch -- $message {
		postDisplay {
			qeconfigure MicroMap <Position> -active yes
			Value micromap,float 1
		}
		postWithdraw {
			qeconfigure MicroMap <Position> -active no
			Value micromap,float 0
		}
	}

	return
}

# NSMicroMapWindow::GeometryCmd --
#
#	Called by NSWindowManager::Setup(). Returns the desired (default)
#	geometry for the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMicroMapWindow::GeometryCmd {oop} {

	set win [Info $oop win]
	set winMain [Window main]

	scan [ScreenArea] "%d %d %d %d" waLeft waTop waRight waBottom
	set waHeight [expr {$waBottom - $waTop}]
	set waWidth [expr {$waRight - $waLeft}]

	set spacing [Global WindowSpacing]

	if {[ScreenHeight] >= 1024} { # 1280x1024
		set height [expr {100 * 4}]
		set width [expr {100 * 4}]
	} elseif {[ScreenHeight] >= 768} { # 1024x768
		set height [expr {60 * 4}]
		set width [expr {60 * 4}]
	} else { # 800x600, 640x480
		# Make room for Misc window buttons on small monitors
		set height [expr {38 * 4}]
		# I want this window as wide as the Misc Window
		if {[Value misc,float] && ([Value misc,layout] eq "wide")} {
			set width [winfo reqwidth [NSMiscWindow::Info [Global misc,oop] canvas]]
		} else {
			set width $height
		}
	}

	set x [expr {$waRight - [NSToplevel::TotalWidth $win]}]

	# If this is being called in HardcodeGeometry(), then the window
	# should be positioned offscreen.
	if {[Global HardcodeGeometry]} {
		incr x [OffscreenOffsetX]
	}

	if {[Value message,float]} {
		set y [expr {[NSToplevel::FrameBottom [Window message]] + $spacing}]
	} else {
		set y $waTop
	}

	return ${width}x$height+$x+$y
}

# NSMicroMapWindow::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMicroMapWindow::Close {oop} {

	NSWindowManager::Undisplay message

	return
}

# NSMicroMapWindow::InitDisplay --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMicroMapWindow::InitDisplay {parent} {

	global PYPX

	set scale [Value micromap,scale]

	set width [set height 100]
	set widgetId [NSObject::New NSWidget $parent $width $height $scale $scale]
	set widget [NSWidget::Info $widgetId widget]

	NSWidget::Info $widgetId examineCmd \
		NSMicroMapWindow::ExamineLocation

	NSWidget::Info $widgetId leaveCmd \
		NSMicroMapWindow::Leave

	NSWidget::Info $widgetId scaleCmd \
		"Value micromap,scale \[NSWidget::Info $widgetId scale\]"
if 0 {
	$parent configure -width $width -height [NSWidget::Info $widgetId height]
}
	# Global access
	Global micromap,widgetId $widgetId
	Global micromap,widget $widget
if 0 {
	# Because the map supports different resolutions, and the
	# different resolutions result in slightly different-sized
	# maps, I make the background black so the edges don't show.
	$parent configure -background Black
}
	# Shift-click to center the Main Window Widget
	bind $widget <Shift-ButtonPress-1> {
		Global main,widget,center [NSWidget::PointToCave \
			[Global micromap,widgetId] %x %y]
	}

	# Don't display the Big Map after a Shift-click
	bind $widget <Shift-ButtonRelease-1> break

	# When the the left mouse button is released, and the mouse
	# did not move (ie, the user wasn't dragging the map), then
	# display the Big Map.
	bind $widget <ButtonRelease-1> {
		if {![NSWidget::Info [Global micromap,widgetId] track,mouseMoved]} {
			if {[angband inkey_flags] eq "INKEY_TARGET"} {
				scan [NSWidget::Info [Global micromap,widgetId] examined] "%%d %%d" y x
				angband keypress @$y\n$x\n
			} else {
				DoUnderlyingCommand M
			}
		}
	}
if 0 {
	# If the cursor was showing, recenter the Main Window Widget on the
	# cursor's location. If the cursor was not showing (meaning we displayed
	# it) then hide the cursor and recenter on the character's location.
	bind $widget <Leave> {+
		if {[Global cursor,visible]} {
			[Global main,widget] center [Global cursor,y] [Global cursor,x]
		} else {
			[Global main,widget] center {*}[Global main,widget,center]
			[Global main,widget] itemconfigure [Global cursor,itemId] \
				-visible no
		}
		NSMainWindow::StatusText [Global main,oop] {}
	}
}
	bind $widget <Shift-Leave> break

	# When the toplevel is resized, resize the Micro Map Widget
	bindtags $parent [concat [bindtags $parent] MicroMapBindTag]
	bind MicroMapBindTag <Configure> \
		"NSMicroMapWindow::Configure %w %h"

	# This Widget is used to display detail while examining the dungeon map
	set widgetId [NSObject::New NSWidget $parent $width $height \
		[icon width] [icon height]]
	set widget2 [NSWidget::Info $widgetId widget]

	# Cursor for detail widget
	set itemId [$widget2 create cursor -color yellow -linewidth 2]

	# Global access
	Global mapdetail,widgetId $widgetId
	Global mapdetail,widget $widget2
	Global mapdetail,cursor $itemId

	# Keep the Micro Map centered on the character
	qebind MicroMap <Position> {
		NSMicroMapWindow::PositionChanged %y %x
	}
	qeconfigure MicroMap <Position> -active no

	qebind $widget <Dungeon-enter> {
		WidgetCenter %W
	}

	return $widget
}

# NSMiscWindow::PositionChanged --
#
#	Called as a qebind <Position> script. Update the Micro Map Window
#	when the character's position changes. Handles the "scroll_follow"
#	option.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMicroMapWindow::PositionChanged {y x} {

	global PYPX

	set widget [Global micromap,widget]

	# Option: Keep character centered in the display
	if {[Value scroll_follow]} {
		$widget center $y $x

	# Scroll when character crosses the edges of the display
	} else {
		scan [$widget center] "%d %d" oy ox

		set ny $y
		set yscroll [ClipCenter ny $oy [angband cave height] [$widget cget -height]]

		set nx $x
		set xscroll [ClipCenter nx $ox [angband cave width] [$widget cget -width]]

		# Center the widget if needed
		if {$xscroll || $yscroll} {
			scan $PYPX "%d %d" opy opx
			if {abs($y - $opy) > 1 || abs($x - $opx) > 1} {
				set ny $y
				set nx $x
			}
		}
		$widget center $ny $nx
	}

	return
}

# NSMicroMapWindow::ExamineLocation --
#
#	Called when the mouse moves over the MicroMap. Displays
#	a description (if any) of what the character knows about
#	the given cave location. If the screen cursor is not
#	currently visible, it is displayed at the given location.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMicroMapWindow::ExamineLocation {widgetId y x modifiers} {

	set widget [Global main,widget]

	# Center the Main Window Widget at the location pointed to in the
	# Micro Map Window
#	NSWidget::SetCenter [Global main,widgetId] $y $x
	$widget center $y $x

	# If the cursor isn't already visible because of target/look, then
	# center the cursor at the given location.
	# Do this before NSMainWindow::ExamineLocation which may call update
	if {![Global cursor,visible]} {
		$widget itemconfigure [Global cursor,itemId] -y $y -x $x -visible yes
	}

	# Display information about the given location
	NSMainWindow::ExamineLocation [Global main,oop] micromap [Global main,widgetId] $y $x $modifiers

	return
}

# NSMicroMapWindow::Leave --
#
#	Handle the mouse leaving the Widget. Called as NSWidget(OOP,leaveCmd).
#
# Arguments:
#	widgetId			OOP ID NSWidget.
#
# Results:
#	What happened.

proc NSMicroMapWindow::Leave {widgetId} {

	# If the cursor was showing, recenter the Main Window Widget on the
	# cursor's location. If the cursor was not showing (meaning we displayed
	# it) then hide the cursor and recenter on the character's location.
	if {[Global cursor,visible]} {
		[Global main,widget] center [Global cursor,y] [Global cursor,x]
	} else {
		[Global main,widget] itemconfigure [Global cursor,itemId] \
			-visible no
		[Global main,widget] center {*}[Global main,widget,center]
	}
	NSMainWindow::BalloonHide [Global main,oop]
	NSMainWindow::StatusText [Global main,oop] {}

	return
}

# NSMicroMapWindow::Configure --
#
#	Called when the Micro Map Window is resized
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMicroMapWindow::Configure {width height} {

	set win [Window micromap]

	# Resize micromap,widget
	set widgetId [Global micromap,widgetId]
	if {[NSWidget::Resize $widgetId $width $height]} {
		# Nothing
	}

	# Resize mapdetail,widget
	set widgetId [Global mapdetail,widgetId]
	if {[NSWidget::Resize $widgetId $width $height]} {
		# Nothing
	}

	return
}

# NSMicroMapWindow::BalloonInit --
#
#	Init the show_cave_balloon tooltip window.
#
# Arguments:
#	oop					OOP ID of NSMicroMapWindow object.
#
# Results:
#	What happened.

proc NSMicroMapWindow::BalloonInit {oop} {

	set win [Global mapdetail,widget].balloon
	catch {destroy $win}
	toplevel $win -background black
	wm overrideredirect $win 1
	wm withdraw $win

	$win configure -borderwidth 1 -relief flat -background gray60
	text $win.text \
		-wrap none -font [Value font,autobar] \
		-borderwidth 0 -setgrid no -highlightthickness 0 \
		-padx 4 -pady 2 -background [Value listBG] -foreground White \
		-cursor ""
	pack $win.text -expand yes -fill both

	if {[Platform windows]} {
		wm attribute $win -alpha 0.85
	}

	Info $oop balloon,win $win

	return
}

# NSMicroMapWindow::BalloonShow --
#
#	Init the show_cave_balloon tooltip window.
#
# Arguments:
#	oop					OOP ID of NSMicroMapWindow object.
#	text				List of {text color} to display.
#	x, y				Screen coords to display it
#	anchor				position window relative to x,y
#
# Results:
#	What happened.

proc NSMicroMapWindow::BalloonShow {oop text x y {anchor nw}} {

	set win [Info $oop balloon,win]

	$win.text delete 1.0 end

	$win.text configure -font [Value font,choice]

	set chunk 0
	foreach {string color} $text {
		if {$color ne ""} {
			$win.text insert end $string CHUNK$chunk
			$win.text tag configure CHUNK$chunk -foreground $color
		} else {
			$win.text insert end $string
		}
		incr chunk
	}

	set width 0
	for {set i 1} {$i <= [$win.text count -lines 1.0 end]} {incr i} {
		set lineWidth [$win.text count -update -xpixels $i.0 "$i.0 lineend"]
		if {$lineWidth > $width} {
			set width $lineWidth
		}
	}
	incr width [expr {[$win.text cget -padx] * 2}]
	incr width 2
	set height [$win.text count -update -ypixels 1.0 end]
	incr height [expr {[$win.text cget -pady] * 2}]
	incr height 2

	switch -- $anchor {
		n {
			set x [expr {$x - $width / 2}]
		}
	}

	# Not too far left
	if {$x < 0} {set x 0}

	# Not too far right
	set screenWidth [ScreenWidth $win]
	if {$x + $width > $screenWidth } {
		set x [expr {$screenWidth - $width}]
	}

	wm geometry $win ${width}x${height}+$x+$y

	# Display and raise, don't change the focus
	wm deiconify $win

	update

	if {[Platform unix]} {
		raise $win
	}

    return
}

# NSMicroMapWindow::BalloonHide --
#
#	Hide the show_cave_balloon tooltip window.
#
# Arguments:
#	oop					OOP ID of NSMicroMapWindow object.
#
# Results:
#	What happened.

proc NSMicroMapWindow::BalloonHide {oop} {

	set win [Info $oop balloon,win]

	if {[winfo ismapped $win]} {
		wm withdraw $win
	}

	return
}
