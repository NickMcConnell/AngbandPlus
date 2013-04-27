# File: toolbar.tcl

# Purpose: a row of NSWin98ToolbarButtons

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSToolbar {

# namespace eval NSToolbar
}

# NSToolbar::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToolbar::InitModule {} {

	NSModule::LoadIfNeeded NSWin98ToolbarButton

	return
}

# NSToolbar::NSToolbar --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToolbar::NSToolbar {oop size parent} {

	global NSToolbar

	set frame $parent.toolbar$oop
	Info $oop frame $frame
	Info $oop size $size
	Info $oop count 0

if 1 {
    frame $frame \
        -borderwidth 2 -relief groove

	set canvas $frame.canvas
	canvas $canvas \
		-borderwidth 0 -highlightthickness 0 -width 10 -height 16

	set x 2
	set y 2
	$canvas create line [expr {$x + 1}] $y $x $y $x 20 \
		-fill $::SystemButtonHighlight -tags topleft
	$canvas create line [expr {$x + 2}] $y [expr {$x + 2}] 20 \
		[expr {$x - 1}] 20 -fill $::SystemButtonShadow -tags bottomright

	bind $canvas <Configure> \
		"NSToolbar::Configure $oop %W %h %w"

	pack $canvas -side left -fill y

} else {
    frame $frame \
        -borderwidth 0 -relief flat
    frame $frame.divider1 \
        -borderwidth 1 -height 2 -relief groove

    pack $frame -expand yes -fill x -side top
    pack $frame.divider1 \
        -expand yes -fill x -side top
}

	NSUtils::DestroyObjectWithWidget NSToolbar $oop $frame

	return
}

# NSToolbar::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToolbar::Info {oop info args} {

	global NSToolbar

	# Verify the object
	NSObject::CheckObject NSToolbar $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSToolbar($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSToolbar($oop,$info)
			}
		}
	}

	return
}

# NSToolbar::AddTool --
#
#	Add a new "button" to the right of all others.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToolbar::AddTool {oop args} {

	global NSToolbar

	set frame [Info $oop frame]
	set size [Info $oop size]
	set count [incr NSToolbar($oop,count)]
	set button $frame.tool$count

if 1 {
	eval win98button $button $args
	pack $button -side left
} else {
    label $label \
        -anchor center -relief flat -borderwidth 2 -image $image -height $size -width $size

	NSButtonLabel::ButtonLabel $label $command

	pack $label \
		-anchor center -expand no -padx 2 -pady 2 -side left
}

	return $count
}

# NSToolbar::GetTool --
#
#	Return the full window pathname of the given "button".
#
# Arguments:
#	oop					OOP ID. See above.
#	index					1-based index of tool.
#
# Results:
#	What happened.

proc NSToolbar::GetTool {oop index} {

	global NSToolbar

	set frame $NSToolbar($oop,frame)
	return $frame.tool$index	
}

# NSToolbar::Configure --
#
#	Called when the toolbar changes size.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSToolbar::Configure {oop canvas height width} {

	foreach itemId [$canvas find withtag topleft] {
		scan [$canvas coords $itemId] "%s %s %s %s %s %s" x1 y1 x2 y2 x3 y3
		set y3 [expr {$height - 3}]
		eval $canvas coords $itemId $x1 $y1 $x2 $y2 $x3 $y3
	}
	
	foreach itemId [$canvas find withtag bottomright] {
		scan [$canvas coords $itemId] "%s %s %s %s %s %s" x1 y1 x2 y2 x3 y3
		set y2 [set y3 [expr {$height - 3}]]
		$canvas coords $itemId $x1 $y1 $x2 $y2 $x3 $y3
	}

	return
}

