# File: photo.tcl

# Purpose: a snapshot of a Widget

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSPhoto {

# namespace eval NSPhoto
}

# NSPhoto::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhoto::InitModule {} {

	return
}

# NSPhoto::NSPhoto --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSPhoto::NSPhoto {oop {parent ""}} {

	Info $oop parent $parent
	Info $oop exists 0
	Info $oop examined ""
	Info $oop examineCmd ""

	if {[string length $parent]} {
		set canvas $parent.photo$oop
		canvas $canvas \
			-highlightthickness 0 -scrollregion {0 0 0 0}
		$canvas create image 0 0 \
			-anchor nw -image Image_Empty -tags image
	
if {[icon style] ne "iso"} {
		$canvas bind image <Enter> \
			"NSPhoto::Motion $oop %x %y"
		$canvas bind image <Motion> \
			"NSPhoto::Motion $oop %x %y"
		$canvas bind image <Leave> \
			"NSPhoto::Info $oop examined {}"
}
		Info $oop canvas $canvas

		# Destroy the object along with the widget (later)
		NSUtils::DestroyObjectWithWidget NSPhoto $oop $canvas
	}

	return
}

# NSPhoto::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhoto::Info {oop info args} {

	global NSPhoto

	# Verify the object
	NSObject::CheckObject NSPhoto $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSPhoto($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSPhoto($oop,$info)
			}
		}
	}

	return
}

# NSPhoto::Forget --
#
#	Clear the memory about what is at each cave location.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhoto::Forget {oop} {

	global NSPhoto

	array unset NSPhoto examine,*
	array unset NSPhoto idx,*
	array unset NSPhoto what,*

	Info $oop exists 0

	return
}

# NSPhoto::SetImage --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhoto::SetImage {oop imageName} {

	set canvas [Info $oop canvas]

	$canvas itemconfigure image -image $imageName
	Forget $oop

	return
}

# NSPhoto::ExamineWidget --
#
#	Examine the area of the cave covered by the given widget.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhoto::ExamineWidget {oop widget} {

	scan [$widget bounds] "%d %d %d %d" y_min x_min y_max x_max
	
	for {set y $y_min} {$y <= $y_max} {incr y} {
		for {set x $x_min} {$x <= $x_max} {incr x} {

			# Require a valid location
			if {![angband cave in_bounds_fully $y $x]} continue

			# Describe what is seen
			set examine [angband cave examine $y $x]
			if {![string length $examine]} continue

			# Get info about the grid
			angband cave info $y $x attrib
			set m_idx $attrib(m_idx)
			set o_idx $attrib(o_idx)

			# Monster
			if {($m_idx > 0) && [angband m_list set $m_idx ml]} {
				set r_idx [angband m_list set $m_idx r_idx]
				Info $oop examine,$y,$x $examine
				Info $oop idx,$y,$x $r_idx
				Info $oop what,$y,$x monster

			# Pile
			} elseif {[string match "*a pile of * items*" $examine]} {
				Info $oop examine,$y,$x $examine
				Info $oop idx,$y,$x 0
				Info $oop what,$y,$x pile

			# Object
			} elseif {$o_idx && [angband o_list set $o_idx marked]} {
				set k_idx [angband o_list set $o_idx k_idx]
				Info $oop examine,$y,$x $examine
				Info $oop idx,$y,$x $k_idx
				Info $oop what,$y,$x object
			}
		}
	}

	Info $oop gwidth [$widget cget -gwidth]
	Info $oop gheight [$widget cget -gheight]
	Info $oop xmin $x_min
	Info $oop xmax $x_max
	Info $oop ymin $y_min
	Info $oop ymax $y_max

	Info $oop exists 1

	return
}

# NSPhoto::PointToCave --
#
#	Determine the cave y,x location based on the given
#	coordinates inside the given widget.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhoto::PointToCave {oop x y} {

	set gwidth [Info $oop gwidth]
	set gheight [Info $oop gheight]
	set xmin [Info $oop xmin]
	set ymin [Info $oop ymin]

if 1 { # Sep 14 2004
	set xmax [Info $oop xmax]
	set ymax [Info $oop ymax]
	set cols [expr {($xmax - $xmin) + 1}]
	set rows [expr {($ymax - $ymin) + 1}]
	set image [[Info $oop canvas] itemcget image -image]
	set w1 [expr {$cols * $gwidth}]
	set w2 [image width $image]
	set h1 [expr {$rows * $gheight}]
	set h2 [image height $image]
	set dx [expr {($w1 - $w2) / 2}]
	set dy [expr {($h1 - $h2) / 2}]

	set col [expr {($dx + $x) / $gwidth}]
	set row [expr {($dy + $y) / $gheight}]

	set x1 [expr {$xmin + $col}]
	set y1 [expr {$ymin + $row}]

} else {
	set col [expr {$x / $gwidth}]
	set row [expr {$y / $gheight}]

	set x1 [expr {$xmin + $col}]
	set y1 [expr {$ymin + $row}]
}
	return "$y1 $x1"
}

# NSPhoto::Motion --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhoto::Motion {oop x y} {

	set canvas [Info $oop canvas]

	if {![Info $oop exists]} return

	set x [expr {int([$canvas canvasx $x])}]
	set y [expr {int([$canvas canvasy $y])}]

	set pos [PointToCave $oop $x $y]
	if {[Info $oop examined] == $pos} return
	Info $oop examined $pos

	set command [Info $oop examineCmd]
	if {[string length $command]} {
		uplevel #0 $command $oop $pos
	}

	return
}

# NSPhoto::WritePhotoText --
#
#	Write photo.txt.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhoto::WritePhotoText {oop filePath} {

	global NSPhoto

	ASSERT {[Info $oop exists]} "NSPhoto info doesn't exist"
	
	if {[catch {openlf $filePath} fileId]} {
		set msg "The following error occurred while attempting write "
		append msg "the photo.txt file:\n\n$fileId"
		tk_messageBox -title Error -message $msg
		return 1
	}

	catch {

	set gwidth [Info $oop gwidth]	
	set gheight [Info $oop gheight]	
	set xmin [Info $oop xmin]
	set xmax [Info $oop xmax]
	set ymin [Info $oop ymin]
	set ymax [Info $oop ymax]

	puts $fileId "gwidth: $gwidth gheight: $gheight"
	puts $fileId "xmin: $xmin xmax: $xmax ymin: $ymin ymax: $ymax"
	
	for {set y $ymin} {$y <= $ymax} {incr y} {
		for {set x $xmin} {$x <= $xmax} {incr x} {
			if {![info exists NSPhoto($oop,examine,$y,$x)]} continue
			set examine [Info $oop examine,$y,$x]
			set idx [Info $oop idx,$y,$x]
			set what [Info $oop what,$y,$x]
			puts $fileId "$y $x $what $idx \"$examine\""
		}
	}

	# catch
	}
	
	close $fileId

	return 0
}

# NSPhoto::ReadPhotoText --
#
#	Read photo.txt.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhoto::ReadPhotoText {oop filePath} {

	global NSPhoto

	Forget $oop

	if {![file exists $filePath]} {
		return
	}
	set fileId [open $filePath]
	set buffer [read $fileId]
	close $fileId

	foreach string [split $buffer \n] {
		if {[string match "gwidth:*" $string]} {
			scan $string "gwidth: %d gheight: %d" \
				NSPhoto($oop,gwidth) NSPhoto($oop,gheight)
			continue
		}
		if {[string match "xmin:*" $string]} {
			scan $string "xmin: %d xmax: %d ymin: %d ymax: %d" \
				NSPhoto($oop,xmin) NSPhoto($oop,xmax) \
				NSPhoto($oop,ymin) NSPhoto($oop,ymax)
			continue
		}
		if {[regexp {([0-9]+) ([0-9]+) ([^ ]+) ([0-9]+) "(.*)"} $string \
			ignore y x what idx examine]} {
			Info $oop examine,$y,$x $examine
			Info $oop idx,$y,$x $idx
			Info $oop what,$y,$x $what
		}
	}

	Info $oop examined ""
	Info $oop exists 1

	return
}

