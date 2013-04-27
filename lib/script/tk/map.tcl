# File: map.tcl

# Purpose: a Widget plus two scrollbars

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSMap {

# namespace eval NSMap
}

# NSMap::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMap::InitModule {} {

	NSModule::LoadIfNeeded NSWidget

	return
}

# NSMap::NSMap --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMap::NSMap {oop parent width height gwidth gheight} {

	set frame $parent.map$oop

	# Frame + widget + scrollbars
	frame $frame \
		-borderwidth 0

	set widgetId [NSObject::New NSWidget $frame $width $height \
		$gwidth $gheight]
	set widget [NSWidget::Info $widgetId widget]

	NSWidget::Info $widgetId scaleCmd "NSMap::ScaleCmd $oop"

	scrollbar $frame.xscroll \
		-orient horizontal -command "NSWidget::xview $widgetId"

	scrollbar $frame.yscroll \
		-orient vertical -command "NSWidget::yview $widgetId"

	# Synch the scrollbars when the Widget scrolls
	NSWidget::Info $widgetId xviewCmd "NSMap::SynchScrollBars $oop"
	NSWidget::Info $widgetId yviewCmd "NSMap::SynchScrollBars $oop"

	grid rowconfig $frame 0 -weight 1 -minsize 0
	grid rowconfig $frame 1 -weight 0 -minsize 0
	grid columnconfig $frame 0 -weight 1 -minsize 0
	grid columnconfig $frame 1 -weight 0 -minsize 0

	grid $widget \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $frame.xscroll \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $frame.yscroll \
		-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky ns

	# Resize the widget when it changes size
	bind $widget <Configure> \
		"NSMap::Configure $oop %w %h"

	# Set instance variables
	Info $oop frame $frame
	Info $oop widgetId $widgetId
	Info $oop widget $widget
	Info $oop scaleCmd ""
	Info $oop viewCmd ""

	# KeyPress bindings
	bind $widget <KeyPress-Left> \
		"NSWidget::xview $widgetId scroll -10 units"
	bind $widget <KeyPress-Right> \
		"NSWidget::xview $widgetId scroll 10 units"
	bind $widget <KeyPress-Up> \
		"NSWidget::yview $widgetId scroll -10 units"
	bind $widget <KeyPress-Down> \
		"NSWidget::yview $widgetId scroll 10 units"
	bind $widget <Control-KeyPress-Left> \
		"NSWidget::xview $widgetId moveto 0"
	bind $widget <Control-KeyPress-Right> \
		"NSWidget::xview $widgetId moveto 1"
	bind $widget <Control-KeyPress-Up> \
		"NSWidget::yview $widgetId moveto 0"
	bind $widget <Control-KeyPress-Down> \
		"NSWidget::yview $widgetId moveto 1"
	
	#
	# Synch the scrollbars when window is shown.
	#

	bind $frame <Map> "NSMap::SynchScrollBars $oop"

	return
}

# NSMap::Configure --
#
#	Called when the frameWidget changes size.
#
# Arguments:
#	oop					OOP ID of NSMainWindow object.
#
# Results:
#	What happened.

proc NSMap::Configure {oop width height} {

	set widgetId [Info $oop widgetId]
	set widget [Info $oop widget]
	if {[NSWidget::Resize $widgetId $width $height]} {
		eval SetView $oop [$widget center]
	}

	return
}

# NSMap::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMap::Info {oop info args} {

	global NSMap

	# Verify the object
	NSObject::CheckObject NSMap $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSMap($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSMap($oop,$info)
			}
		}
	}

	return
}

# NSMap::ScaleCmd --
#
#	Called as the scaleCmd of our NSWidget. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMap::ScaleCmd {oop} {

	set widget [Info $oop widget]
	eval SetView $oop [$widget center]

	set command [Info $oop scaleCmd]
	if {[string length $command]} {
		uplevel #0 $command
	}

	return
}

# NSMap::SetView --
#
#	Center the view on the given position. If this means the map
#	is scrolled "too far", then adjust accordingly. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMap::SetView {oop y x} {

	set widget [Info $oop widget]

	scan [$widget bounds] "%d %d %d %d" y_min x_min y_max x_max
	set height [expr {$y_max - $y_min + 1}]
	set width [expr {$x_max - $x_min + 1}]

	set dunHgt [angband cave height]
	set dunWid [angband cave width]

	if {$dunHgt > $height} { incr dunHgt 2 }
	if {$dunWid > $width} { incr dunWid 2 }

	set ny [ConstrainCenter $y $dunHgt $height]
	set nx [ConstrainCenter $x $dunWid $width]
if 0 {
	# Do nothing if position unchanged
	scan [$widget center] "%d %d" oy ox
	if {$oy == $ny && $ox == $nx} return
}
	# Center the widget at the given location
	$widget center $ny $nx

	# Update the scrollbars
	SynchScrollBars $oop

	return
}


# NSMap::SynchScrollBars --
#
#	Configures the scrollbars to reflect the current scroll position
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMap::SynchScrollBars {oop} {

	set frame [Info $oop frame]
	set widget [Info $oop widget]

	scan [$widget center] "%d %d" cy cx

	scan [$widget bounds] "%d %d %d %d" y_min x_min y_max x_max
	set height [expr {$y_max - $y_min + 1}]
	set width [expr {$x_max - $x_min + 1}]

	set dunHgt [angband cave height]
	set dunWid [angband cave width]
	
	
	if {$dunHgt > $height} {
		incr dunHgt 2
		incr cy 1
	}
	if {$dunWid > $width} {
		incr dunWid 2
		incr cx 1
	}
	
	set top [expr {$cy - $height / 2}]
	if {$top < 0} {set top 0}
	set bottom [expr {$top + $height}]
	if {$bottom > $dunHgt} {set bottom $dunHgt}
	$frame.yscroll set [expr {$top / double($dunHgt)}] \
		[expr {$bottom / double($dunHgt)}]

	set left [expr {$cx - $width / 2}]
	if {$left < 0} {set left 0}
	set right [expr {$left + $width}]
	if {$right > $dunWid} {set right $dunWid}
	$frame.xscroll set [expr {$left / double($dunWid)}] \
		[expr {$right / double($dunWid)}]

	set command [Info $oop viewCmd]
	if {[string length $command]} {
		uplevel #0 $command
	}

	return
}

