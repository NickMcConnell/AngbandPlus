# File: widget.tcl

# Purpose: commands for manipulating Widgets

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSWidget {

# namespace eval NSWidget
}

# NSWidget::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWidget::InitModule {} {
}

# NSWidget::NSWidget --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWidget::NSWidget {oop parent width height gwidth gheight} {

	set widget $parent.widget$oop

	if {$gwidth == [icon width]} {
		set style [icon style]
	} else {
		set style map
	}

	widget $widget -width $width -height $height \
		-gwidth $gwidth -gheight $gheight -style $style

	bind $widget <Enter> "NSWidget::Event $oop enter %x %y {}"
	bind $widget <Motion> "NSWidget::Event $oop motion %x %y {}"
	bind $widget <Leave> "NSWidget::Event $oop leave"

	bind $widget <Control-Enter> "NSWidget::Event $oop enter %x %y control"
	bind $widget <Control-Motion> "NSWidget::Event $oop enter %x %y control"

	bind $widget <Shift-Enter> "NSWidget::Event $oop enter %x %y shift"
	bind $widget <Shift-Motion> "NSWidget::Event $oop motion %x %y shift"

	# Shift-drag does nothing
	bind $widget <Shift-Button1-Motion> break

	bind $widget <ButtonPress-1> "NSWidget::TrackPress $oop %x %y"
	bind $widget <Button1-Motion> "NSWidget::TrackOnce $oop %x %y"
	
	# Disable tracking when dragging
	bind $widget <Button1-Enter> break
	bind $widget <Button1-Leave> break

if 0 {
	bind $widget <MouseWheel> {
		%W yview scroll [expr {- (%D / 120) * 4}] units
	}
}

	Info $oop widget $widget
	Info $oop examined ""
	Info $oop examineModifiers {}
	Info $oop examineTurn 0
	Info $oop examineCmd ""
	Info $oop leaveCmd ""
	Info $oop scaleCmd ""
	Info $oop xviewCmd ""
	Info $oop yviewCmd ""
	Info $oop track,mouseMoved 0
	Info $oop caveyx 0
	Info $oop centerCmd ""

	Info $oop event,afterId ""
	Info $oop event,gate 0

	# Micro-map Widgets get a popup menu to change the scale
	if {1 || ($gwidth != [icon width])} {
	
		set menu $widget.context
		menu $menu -tearoff 0
		foreach n $ConfigMap::MapSizes {
			$menu add radiobutton -label "${n}x$n" \
				-variable ::NSWidget($oop,scale) -value $n \
				-command "NSWidget::SetScale $oop $n"
		}
		menu $menu.detail -tearoff 0
		$menu.detail add radiobutton -label Low \
			-command "ConfigMap::Configure low" \
			-variable ::ConfigMap::Detail -value low
		$menu.detail add radiobutton -label Medium \
			-command "ConfigMap::Configure medium" \
			-variable ::ConfigMap::Detail -value medium
		$menu.detail add radiobutton -label High \
			-command "ConfigMap::Configure high" \
			-variable ::ConfigMap::Detail -value high
		$menu add separator
		$menu add cascade -label Detail -menu $menu.detail

		# Popup the menu when the right mouse button is clicked
		bind $widget <ButtonPress-3> "
			if {\$::NSWidget($oop,scale) != \[icon width]} {
				tk_popup $menu %X %Y
			}
		"

		# Set the checkmark for the current scale
		Info $oop scale $gwidth
	}

	if {$style ne "map"} {
		qebind $widget <IconCfg> {
#			%W configure -gwidth [icon width] -gheight [icon height] \
#				-style [icon style]
			%W center {*}[angband player position]
		}
	}

	# Support the [angband cave track] stuff
	qebind NSWidget$oop <Track-grid> \
		"NSWidget::TrackGrid $oop %y %x"

	return
}

# NSWidget::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWidget::Info {oop info args} {

	global NSWidget

	# Verify the object
	NSObject::CheckObject NSWidget $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSWidget($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSWidget($oop,$info)
			}
		}
	}

	return
}

# NSWidget::Motion --
#
#	Call the client's command when the mouse moves over a grid.
#
# Arguments:
#	oop					OOP ID.
#	x					x location in widget.
#	y					y location in widget.
#
# Results:
#	What happened.

proc NSWidget::Motion {oop x y modifiers} {

	set pos [PointToCave $oop $x $y]
	if {$pos eq ""} return

	if {[Info $oop examined] eq $pos &&
		[Info $oop examineTurn] == [angband player turn] &&
		[Info $oop examineModifiers] eq $modifiers} {
		return
	}
	Info $oop examined $pos
	Info $oop examineTurn [angband player turn]
	Info $oop examineModifiers $modifiers

	set command [Info $oop examineCmd]
	if {$command ne ""} {
		uplevel #0 $command $oop $pos [list $modifiers]
	}

	# The tells the game to generate a <Track-grid> event if lite_spot()
	# is called at this location.  If that happens then our TrackGrid
	# command is called.
	angband cave track {*}$pos

	return
}

# NSWidget::Leave --
#
#	Handle the <Leave> event.
#
# Arguments:
#	oop					OOP ID.
#
# Results:
#	What happened.

proc NSWidget::Leave {oop} {

	# Hack for Main Window show_cave_balloon balloon. Don't <Leave> the main
	# widget if the balloon pops up under the mouse.
	set widget [Info $oop widget]
	set pointerx [winfo pointerx .]
	set pointery [winfo pointery .]
	set w [winfo containing $pointerx $pointery]
	if {[string match "*balloon*" $w] &&
		[string match "${widget}*" $w]} return

	Info $oop examined ""

	set command [Info $oop leaveCmd]
	if {$command ne ""} {
		uplevel #0 $command $oop
	}

	angband cave track -1 -1

	return
}

# NSWidget::TrackGrid --
#
#	<Track-grid> callback.
#
# Arguments:
#	oop					OOP ID.
#	y					cave y
#	x					cave x
#
# Results:
#	What happened.

proc NSWidget::TrackGrid {oop y x} {

	if {"$y $x" eq [Info $oop examined]} {
if 0 {
		# Fake a motion event
		set widget [Info $oop widget]
		set pointerx [winfo pointerx $widget]
		set pointery [winfo pointery $widget]
		set x [expr {$pointerx - [winfo rootx $widget]}]
		set y [expr {$pointery - [winfo rooty $widget]}]
		set [Info $oop examineTurn] -1
		Event $oop motion $x $y [Info $oop examineModifiers]
} else {
		set command [Info $oop examineCmd]
		if {$command ne ""} {
			uplevel #0 $command $oop $y $x [list [Info $oop examineModifiers]]
		}
}
	}
	return
}

# NSWidget::Event --
#
#	Handle <Enter> <Motion> <Leave>.
#
# Arguments:
#	oop					OOP ID.
#	event				enter, motion, leave
#	args				event args
#
# Results:
#	What happened.

proc NSWidget::Event {oop event args} {

	switch -- $event {
		enter {
			lassign $args x y modifiers
			Info $oop event,modifiers $modifiers
		}
		motion {
			lassign $args x y modifiers
			Info $oop event,modifiers $modifiers
		}
		leave {
			lassign $args x y
		}
	}

	if {[Info $oop event,afterId] eq ""} {
		Info $oop event,afterId [after 10 "NSWidget::CheckPointer $oop"]
	}

	return
}

# NSWidget::CheckPointer --
#
#	Idle callback to handle motion events.
#
# Arguments:
#	oop					OOP ID.
#
# Results:
#	What happened.

proc NSWidget::CheckPointer {oop} {

	set widget [Info $oop widget]

	Info $oop event,afterId ""

	# Prevent recursion due to callbacks calling [update]
	if {[Info $oop event,gate]} {
		Info $oop event,afterId [after 5 "NSWidget::CheckPointer $oop"]
		return
	}
	Info $oop event,gate 1

	set pointerx [winfo pointerx $widget]
	set pointery [winfo pointery $widget]
	set containing [winfo containing $pointerx $pointery]
	if {$containing eq $widget} {
		set x [expr {$pointerx - [winfo rootx $widget]}]
		set y [expr {$pointery - [winfo rooty $widget]}]
		Motion $oop $x $y [Info $oop event,modifiers]
	} elseif {[Info $oop examined] ne ""} {
		Leave $oop
	}

	# Prevent recursion due to callbacks calling [update]
	Info $oop event,gate 0

	return
}

# NSWidget::PointToCave --
#
#	Determine the cave y,x location based on the given
#	coordinates inside the given widget.
#
# Arguments:
#	oop					OOP ID.
#	x					x coordinate in Widget.
#	y					y coordinate in Widget.
#
# Results:
#	Return "y x".

proc NSWidget::PointToCave {oop x y} {

	set widget [Info $oop widget]

	# Normally, we want to know which grid the point is over,
	# and for isometric view this requires accurate hittesting
	# of the actual icons near the point, instead of just the
	# floor tile.
	if {![Info $oop caveyx]} {
		set str [$widget hittest $x $y]
		if {[string length $str]} {
			scan $str "%d %d" cy cx
			set str "$cy $cx"
		}
		return $str
	}

	# Vault editor wants floor tile.
	return [$widget caveyx $x $y]
}

# NSWidget::SetScale --
#
#	Sets the resolution of the Widget, but doesn't let the Widget
#	get any larger than its original dimensions.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWidget::SetScale {oop scale} {

	set widget [Info $oop widget]

#	if {[string first $scale "45678"] == -1} return
	if {$scale == [$widget cget -gwidth]} return

	if {$scale == [icon width]} {
		set style [icon style]
	} else {
		set style map
	}

	$widget configure -gwidth $scale -gheight $scale -style $style

	# Context menu
	Info $oop scale $scale
	
	# Hack -- Fully update the widget
	$widget wipe
	$widget center {*}[$widget center]

	set command [Info $oop scaleCmd]
	if {[string length $command]} {
		uplevel #0 $command
	}

	return
}

# NSWidget::IncrScale --
#
#	Increments the scale of the Widget, wrapping if needed.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWidget::IncrScale {oop offset} {

	set widget [Info $oop widget]

	set scale [$widget cget -gwidth]
	if {$scale == [icon width]} return

	set index [lsearch -integer $ConfigMap::MapSizes $scale]
	incr index $offset
	set maxIndex [llength $ConfigMap::MapSizes]
	if {$index >= $maxIndex} {
		set index 0
	} elseif {$index < 0} {
		set index [expr {$maxIndex - 1}]
	}
	set scale [lindex $ConfigMap::MapSizes $index]

	SetScale $oop $scale

	return
}

# NSWidget::Resize --
#
#	Change the size of the widget.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWidget::Resize {oop width height} {

	set widget [Info $oop widget]

	if {($width == [$widget cget -width]) && \
		($height == [$widget cget -height])} {
		return 0
	}

	$widget configure -width $width -height $height

	# Hack -- Fully update the widget
	$widget wipe
	$widget center {*}[$widget center]

	return 1
}

proc NSWidget::Size {oop _height _width} {

	upvar $_height height
	upvar $_width width

	set widget [Info $oop widget]

	if {[$widget cget -style] eq "iso"} {
		scan [$widget isoinfo] "%d %d %d %d %d %d %d %d %d %d" \
			rc cc rTop rBottom cLeft cRight igTop igBot igLef igRig
		set height [expr {($rTop + $rBottom - $igTop - $igBot) * 2 + 1}]
		set width [expr {($cLeft + $cRight - $igLef - $igRig) * 2 + 1}]
	} else {
		scan [$widget bounds] "%d %d %d %d" y_min x_min y_max x_max
		set height [expr {$y_max - $y_min + 1}]
		set width [expr {$x_max - $x_min + 1}]
	}

	return
}

proc NSWidget::CaveSize {oop _height _width} {

	upvar $_height height
	upvar $_width width

	set widget [Info $oop widget]

	if {[$widget cget -vaultnum]} {
		set h [vault height [$widget cget -vaultnum]]
		set w [vault width [$widget cget -vaultnum]]
	} else {
		set h [angband cave height]
		set w [angband cave width]
	}

	Size $oop h2 w2
	if {$h > $h2} {
		incr h 2
	}
	if {$w > $w2} {
		incr w 2
	}

	if {[$widget cget -style] eq "iso"} {
		set height [expr {$h + $w - 1}]
		set width [expr {$h + $w - 1}]
	} else {
		set height $h
		set width $w
	}

	return
}

proc NSWidget::CaveToIso {oop _y _x} {

	upvar $_y y
	upvar $_x x

	set y1 $y
	set x1 $x
	set y [expr {$x1 + $y1}]
	set x [expr {$x1 - $y1}]

	incr x [IsoOffset $oop]

	return
}

proc NSWidget::IsoToCave {oop _y _x} {

	upvar $_y y
	upvar $_x x

	incr x -[IsoOffset $oop]

	set y1 $y
	set x1 $x
	set y [expr {($y1 - $x1) / 2}]
	set x [expr {($y1 + $x1) / 2}]

set y2 $y ; set x2 $x
CaveToIso $oop y2 x2
incr x1 [IsoOffset $oop]
if {$y2 != $y1 || $x2 != $x1} {
	dbwin "illegal iso coord $y1,$x1 ($y2,$x2)\n"
	IsoToCave $oop y2 x2
	set y $y2 ; set x $x2
}

	return
}

proc NSWidget::IsoOffset {oop} {

	set widget [Info $oop widget]

	if {[$widget cget -vaultnum]} {
		set h [vault height [$widget cget -vaultnum]]
		set w [vault width [$widget cget -vaultnum]]
	} else {
		set h [angband cave height]
		set w [angband cave width]
	}

	Size $oop h2 w2
	if {$h > $h2} {
		incr h 2
	}
	return [expr $h - 1]
}

# NSWidget::yview --
#
#	Typical yview command
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWidget::yview {oop cmd args} {

	set widget [Info $oop widget]

	if {[$widget cget -style] eq "iso"} {
		yview_iso $oop $cmd {*}$args
		return
	}

	scan [$widget center] "%d %d" oy ox

	scan [$widget bounds] "%d %d %d %d" y_min x_min y_max x_max
	set height [expr {$y_max - $y_min + 1}]

	if {[$widget cget -vaultnum]} {
		set caveHgt [vault height [$widget cget -vaultnum]]
	} else {
		set caveHgt [angband cave height]
	}

	if {$caveHgt > $height} {
		incr caveHgt 2
		set fiddle -1
	} else {
		set fiddle 0
	}

	switch $cmd {

		moveto {
			set fraction [lindex $args 0]
			if {$fraction > 1.0} {
				set fraction 1.0
			} elseif {$fraction < 0} {
				set fraction 0
			}
			set top [expr {int($fraction * double($caveHgt) + 0.5)}]
			incr top $fiddle
			set ny [expr {$top + $height / 2}]
		}

		scroll {

			set number [lindex $args 0]
			set what [lindex $args 1]

			switch $what {

				units {
					set ny [expr {$oy + $number}]
				}

				pages {
					set pageSize [expr {$height - 10}]
					set ny [expr {$oy + $number * $pageSize}]
				}
			}
		}
	}

	set ny [ConstrainCenter $ny $caveHgt $height]

	# Do nothing if position unchanged
	if {$oy == $ny} return

	SetCenter $oop $ny $ox

	set command [Info $oop yviewCmd]
	if {[string length $command]} {
		uplevel #0 $command
	}

	return
}

set DY 0

proc NSWidget::yview_iso {oop cmd args} {

	set widget [Info $oop widget]

	scan [$widget center] "%d %d" oy ox

	CaveToIso $oop oy ox

	scan [$widget isoinfo] "%d %d %d %d %d %d %d %d %d %d" \
		rc cc rTop rBottom cLeft cRight igTop igBot igLef igRig

	Size $oop height width
	CaveSize $oop caveHgt caveWid

	switch $cmd {

		moveto {
			set fraction [lindex $args 0]
			if {$fraction > 1.0} {
				set fraction 1.0
			} elseif {$fraction < 0} {
				set fraction 0
			}
			set top [expr {int($fraction * double($caveHgt) + 0.5)}]
			if {$caveHgt > $height} {
#				incr top -1
			}
			set ny [expr {$top + ($rTop - $igTop) * 2}]

			if {$height & 1} {
				if {$ny & 1} { incr ny -1 }
			} else {
				if {!($ny & 1)} { incr ny -1 }
			}
		}

		scroll {

			set number [lindex $args 0]
			set what [lindex $args 1]

			switch $what {

				units {
					set ny [expr {$oy + $number * 2}]
				}

				pages {
					set pageSize [expr {$height - 10}]
					set ny [expr {$oy + $number * $pageSize}]
				}
			}
		}
	}

#	set ny [ConstrainCenter $ny $caveHgt $height]

	# Do nothing if position unchanged
	if {$oy == $ny} return

	set y $ny
	set x $ox
	IsoToCave $oop y x
	
	SetCenter $oop $y $x

	set command [Info $oop yviewCmd]
	if {[string length $command]} {
		uplevel #0 $command
	}

	return
}

# NSWidget::xview --
#
#	Typical xview command
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWidget::xview {oop cmd args} {

	set widget [Info $oop widget]

	if {[$widget cget -style] eq "iso"} {
		xview_iso $oop $cmd {*}$args
		return
	}

	scan [$widget center] "%d %d" oy ox

	scan [$widget bounds] "%d %d %d %d" y_min x_min y_max x_max
	set width [expr {$x_max - $x_min + 1}]

	if {[$widget cget -vaultnum]} {
		set caveWid [vault width [$widget cget -vaultnum]]
	} else {
		set caveWid [angband cave width]
	}

	if {$caveWid > $width} {
		incr caveWid 2
		set fiddle -1
	} else {
		set fiddle 0
	}

	switch $cmd {

		moveto {
			set fraction [lindex $args 0]
			if {$fraction > 1.0} {
				set fraction 1.0
			} elseif {$fraction < 0} {
				set fraction 0
			}
			set left [expr {int($fraction * double($caveWid) + 0.5)}]
			incr left $fiddle
			set nx [expr {$left + $width / 2}]
		}

		scroll {

			set number [lindex $args 0]
			set what [lindex $args 1]

			switch $what {

				units {
					set nx [expr {$ox + $number}]
				}

				pages {
					set pageSize [expr {$width - 10}]
					set nx [expr {$ox + $number * $pageSize}]
				}
			}
		}
	}
	
	set nx [ConstrainCenter $nx $caveWid $width]

	# Do nothing if position unchanged
	if {$ox == $nx} return

	SetCenter $oop $oy $nx

	set command [Info $oop xviewCmd]
	if {[string length $command]} {
		uplevel #0 $command
	}

	return
}

set DX 0
set DX2 0

proc NSWidget::xview_iso {oop cmd args} {

	set widget [Info $oop widget]

	scan [$widget center] "%d %d" oy ox
	scan [$widget isoinfo] "%d %d %d %d %d %d %d %d %d %d" \
		rc cc rTop rBottom cLeft cRight igTop igBot igLef igRig
	Size $oop height width
	CaveSize $oop caveHgt caveWid

	CaveToIso $oop oy ox

	switch $cmd {

		moveto {
			set fraction [lindex $args 0]
			if {$fraction > 1.0} {
				set fraction 1.0
			} elseif {$fraction < 0} {
				set fraction 0
			}
			set left [expr {int($fraction * double($caveWid) + 0.5)}]
			if {$caveWid > $width} {
#				incr left -1
			}
			set nx [expr {$left + ($cLeft - $igLef) * 2}]

			# height is odd, x must be even
			# height is even, x must be odd
			if {$height & 1} {
				if {$nx & 1} { incr nx -1 }
			} else {
				if {!($nx & 1)} { incr nx -1 }
			}
		}

		scroll {

			set number [lindex $args 0]
			set what [lindex $args 1]

			switch $what {

				units {
					set nx [expr {$ox + $number * 2}]
				}

				pages {
					set pageSize [expr {$width - 10}]
					set nx [expr {$ox + $number * $pageSize}]
				}
			}
		}
	}
	
#	set nx [ConstrainCenter $nx $caveWid $width]

	# Do nothing if position unchanged
	if {$ox == $nx} return
	IsoToCave $oop oy nx
	SetCenter $oop $oy $nx

	set command [Info $oop xviewCmd]
	if {[string length $command]} {
		uplevel #0 $command
	}

	return
}

# NSWidget::TrackPress --
#
#	Handles <ButtonPress-1> events
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWidget::TrackPress {oop x y} {

	Info $oop track,x $x
	Info $oop track,y $y
	Info $oop track,mouseMoved 0

	return
}

# NSWidget::TrackOnce --
#
#	Handles <Button1-Motion> events
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWidget::TrackOnce {oop x y} {

	# Get the widget
	set widget [Info $oop widget]

	# Get the scale
	set scale [$widget cget -gwidth]
	
	# Calculate the distance the pointer moved
	set dx [expr {[Info $oop track,x] - $x}]
	set dy [expr {[Info $oop track,y] - $y}]

	# Require minimum movement
	if {abs($dx) < $scale} {
		set dx 0
	}
	if {abs($dy) < $scale} {
		set dy 0
	}

	# If the pointer didn't move, do nothing
	if {!$dx && !$dy} {
		return
	}

	# Remember the pointer moved
	Info $oop track,mouseMoved 1

	# Remember the current center
	scan [$widget center] "%d %d" oy ox

	# We should scroll horizontally
	if {$dx} {

		# Convert from pixels to grid size
		set dx [expr {$dx / $scale}]
		
		# Scroll the Widget
		xview $oop scroll $dx units
	}

	# We should scroll vertically
	if {$dy} {

		# Convert from pixels to grid size
		set dy [expr {$dy / $scale}]

		# Scroll the Widget
		yview $oop scroll $dy units
	}

	# Get the new center
	scan [$widget center] "%d %d" ny nx

	# Remember the current pointer position
	if {$nx != $ox} {
		Info $oop track,x $x
	}

	# Remember the current pointer position
	if {$ny != $oy} {
		Info $oop track,y $y
	}

	return
}

# NSWidget::SetCenter --
#
#	Call [$widget center] and do other stuff.  Any call to [$widget center]
#	should go through here.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWidget::SetCenter {oop y x} {

	# Get the widget
	set widget [Info $oop widget]

	# Get the current center
	scan [$widget center] "%d %d" oy ox

	# Update
	$widget center $y $x

	# Get the new center
	scan [$widget center] "%d %d" ny nx

	if {$oy != $ny || $ox != $nx} {
		set command [Info $oop centerCmd]
		if {$command ne ""} {
			uplevel #0 $command $oop $ny $nx
		}
	}

	return
}

# WidgetCenter --
#
#	When the character goes to a new level (or WOR back to a level) this
#	routine sets the center of the given widget. When scroll_follow is
#	FALSE and the dungeon is smaller horizontally than the widget is
#	wide, the dungeon is displayed centered within the widget.
#	Otherwise the widget is centered on the character position.
#
# Arguments:
#	widget					Widget to center 
#
# Results:
#	What happened.

proc WidgetCenter {widget} {

	scan [angband player position] "%d %d" y x

	if {![Value scroll_follow]} {

		scan [$widget bounds] "%d %d %d %d" y_min x_min y_max x_max
		set height [expr {$y_max - $y_min + 1}]
		set width [expr {$x_max - $x_min + 1}]

		set units [angband cave height]
		set units2 $height
		if {$units <= $units2} {
			set y [expr {($units - $units2) / 2 + $units2 / 2}]
		}
	
		set units [angband cave width]
		set units2 $width
		if {$units <= $units2} {
			set x [expr {($units - $units2) / 2 + $units2 / 2}]
		}
	}

	$widget center $y $x

	return "$y $x"
}

# ClipCenter --
#
#	Helper command used control scrolling of a widget when updating the
#	character's position.
#
# Arguments:
#	_coord					Name of variable holding 
#	center					Current widget center.
#	units					Cave height or width.
#	units2					Widget height or width.
#
# Results:
#	What happened.

proc ClipCenter {_coord center units units2} {

	upvar $_coord coord

	set min [expr {$center - $units2 / 2}]
	set max [expr {$min + $units2 - 1}]
	set bord [expr {$units2 / 8}]
	set pad [expr {$units2 / 4}]
	if {$coord < $min + $bord} {
		set coord [expr {($coord + $pad) - $units2 / 2}]
		if {$units2 % 2 == 0} {incr coord}
		set scroll 1
	} elseif {$coord > $max - $bord} {
		set coord [expr {($coord - $pad) + $units2 / 2}]
		set scroll 1
	} else {
		set coord $center
		set scroll 0
	}

	if {$scroll} {
		if {$units > $units2} {
			set centerMin [expr {$units2 / 2 - 1}]
			set centerMax [expr {$units - $units2 / 2 + 1}]
			if {$units2 & 1} {incr centerMax -1}
			if {$coord < $centerMin} {
				set coord $centerMin
			} elseif {$coord > $centerMax} {
				set coord $centerMax
			} elseif {$coord == $centerMin + 1} {
				set coord $centerMin
			} elseif {$coord == $centerMax - 1} {
				set coord $centerMax
			}
		} else {
			set coord [expr {($units - $units2) / 2 + $units2 / 2}]
		}
	}

	return $scroll
}

# ConstrainCenter --
#
#	Call this when you want to set the x/y center of a widget but do
#	not want the widget to scroll "too far". This calculation adds a
#	1-grid border around the edge of the cave.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc ConstrainCenter {coord units units2} {

	if {$units > $units2} {
		set centerMin [expr {$units2 / 2 - 1}]
		set centerMax [expr {$units - $units2 / 2 - 1}]
		if {$units2 & 1} {incr centerMax -1}
		if {$coord < $centerMin} {
			set coord $centerMin
		} elseif {$coord > $centerMax} {
			set coord $centerMax
		}
	} else {
		set coord [expr {($units - $units2) / 2 + $units2 / 2}]
	}

	return $coord
}

