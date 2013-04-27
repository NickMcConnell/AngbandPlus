# File: updowncontrol.tcl

# Purpose: MS Windows-like up-down control

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSUpDownControl {

	variable Priv
	variable Fill

	variable optionTable [list \
		"script" "-command" "command" "" doCommand \
		"string" "-state" "state" "normal" doState \
		"string" "-reliefup" "relief,up" "raised" doRelief \
		"string" "-reliefdown" "relief,down" "raised" doRelief \
		"pixels" "-width" "width" "16" doSize \
		"pixels" "-height" "height" "24" doSize \
		"pixels" "-widtharrow" "width,arrow" "5" doSize \
		"pixels" "-heightarrow" "height,arrow" "3" doSize \
		"orient" "-orient" "orient" "vertical" doOrient
	]
}

# NSUpDownControl::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownControl::InitModule {} {

	variable Priv
	variable Fill

	set Fill(raised,out,topleft) $::SystemButtonFace
	set Fill(raised,out,bottomright) $::System3dDarkShadow
	set Fill(raised,in,topleft) $::SystemButtonHighlight
	set Fill(raised,in,bottomright) $::SystemButtonShadow

	set Fill(sunken,out,topleft) $::SystemButtonShadow
	set Fill(sunken,out,bottomright) $::SystemButtonHighlight
	set Fill(sunken,in,topleft) $::System3dDarkShadow
	set Fill(sunken,in,bottomright) $::SystemButtonFace

	bind NSUpDownControlBindTag <ButtonPress-1> \
		"NSUpDownControl::ButtonPress %W %x %y"
	bind NSUpDownControlBindTag <Button1-Enter> \
		"NSUpDownControl::ButtonDown %W"
	bind NSUpDownControlBindTag <ButtonRelease-1> \
		"NSUpDownControl::ButtonUp %W %x %y"

	bind NSUpDownControlBindTag <Configure> \
		"NSUpDownControl::Resize %W %w %h"

	return
}

# NSUpDownControl::NSUpDownControl --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownControl::NSUpDownControl {oop path args} {

	variable optionTable
	variable Priv

	set canvas $path
	canvas $canvas -borderwidth 0 -highlightthickness 0 -scrollregion {0 0 0 0}
	set canvasCmd upDownControl$canvas
	rename $canvas $canvasCmd
	set Priv(oop,$path) $oop

	# This is the command seen by the outside
	set command $path
	interp alias {} ::$command {} ::NSUpDownControl::Command $oop
	Info $oop command $command

	# The border (up)
	$canvasCmd create line 0 1  0 0  1 0 -fill $::SystemButtonFace -tags out.topleft,up
	$canvasCmd create line 0 1  1 1  1 0 -fill $::System3dDarkShadow -tags out.bottomright,up
	$canvasCmd create line 0 1  0 0  1 0 -fill $::SystemButtonHighlight -tags in.topleft,up
	$canvasCmd create line 0 1  1 1  1 0 -fill $::SystemButtonShadow -tags in.bottomright,up

	# The border (down)
	$canvasCmd create line 0 1  0 0  1 0 -fill $::SystemButtonFace -tags out.topleft,down
	$canvasCmd create line 0 1  1 1  1 0 -fill $::System3dDarkShadow -tags out.bottomright,down
	$canvasCmd create line 0 1  0 0  1 0 -fill $::SystemButtonHighlight -tags in.topleft,down
	$canvasCmd create line 0 1  1 1  1 0 -fill $::SystemButtonShadow -tags in.bottomright,down

	# The arrow (up)
	$canvasCmd create polygon 0 0  2 0  1 1 -outline black -tags arrowbg,up
	$canvasCmd create polygon 0 0  2 0  1 1 -outline black -tags arrow,up

	# The arrow (down)
	$canvasCmd create polygon 0 0  2 0  1 1 -outline black -tags arrowbg,down
	$canvasCmd create polygon 0 0  2 0  1 1 -outline black -tags arrow,down

	NSUtils::DestroyObjectWithWidget NSUpDownControl $oop $canvas

	bindtags $canvas [concat [bindtags $canvas] NSUpDownControlBindTag]

	Info $oop canvas $canvas
	Info $oop canvasCmd $canvasCmd
	
	foreach {type arg info default flags} $optionTable {
		Info $oop $info $default
	}

	eval Configure $oop $args
		
	return
}

# NSUpDownControl::~NSUpDownControl --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownControl::~NSUpDownControl {oop} {

	return
}

# NSUpDownControl::Command --
#
#	This is the command called for an up-down control. The widget pathname
#	command is a wrapper around this.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownControl::Command {oop command args} {

	switch -- $command {
		canvas {
			return [eval [Info $oop canvasCmd] $args]
		}
		cget {
			set option [lindex $args 0]
			# The Tk tabbing engine calls this!
			if {[string equal $option -state]} {
				return [Info $oop state]
			} else {
				return [eval [Info $oop canvasCmd] cget $args]
			}
		}
		configure {
			return [eval Configure $oop $args]
		}
		info {
			return [eval Info $oop $args]
		}
		default {
			error "unknown command \"$command\""
		}
	}
	
	return
}

# NSUpDownControl::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownControl::Info {oop info args} {

	global NSUpDownControl

	# Verify the object
	NSObject::CheckObject NSUpDownControl $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSUpDownControl($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSUpDownControl($oop,$info)
			}
		}
	}

	return
}

# NSUpDownControl::Configure --
#
#	Change configuration options.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownControl::Configure {oop args} {

	variable optionTable

	if {[llength $args] & 1} {
		error "wrong number of arguments: must be \"[Info $oop command] configure ?option value?...\""
	}

	foreach {type arg info default flags} $optionTable {
		foreach flag $flags {
			set doFlag($flag) 0
		}
	}

	foreach error {0 1} {

		# First pass: Set options to new values
		if {!$error} {
			foreach {option value} $args {
				set match 0
				foreach {type arg info default flags} $optionTable {
					if {[string equal $option $arg]} {
						set savedOptions($info) [Info $oop $info]
						if {[string equal $type boolean]} {
							set value [NSUtils::GetBoolean $value]
						}
						Info $oop $info $value
						foreach flag $flags {
							set doFlag($flag) 1
						}
						set match 1
						break
					}
				}
				if {!$match} {
					error "unknown option \"$option\""
				}
			}

		# Second pass: restore options to old values.
		} else {
			foreach name [array names savedOptions] {
				Info $oop $name $savedOptions($name)
			}
		}

		# Success
		break
	}

	WorldChanged $oop

	if {$error} {
		error $errorString
	}

	return
}

# NSUpDownControl::WorldChanged --
#
#	Called when configuration options change.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownControl::WorldChanged {oop} {

	set canvas [Info $oop canvas]
	set canvasCmd [Info $oop canvasCmd]
	set state [Info $oop state]

	set isActive [string equal $state active]
	set isDisabled [string equal $state disabled]

	# Set the size of the canvas
	set width [Info $oop width]
	set height [Info $oop height]
	$canvasCmd configure -width $width -height $height

	# Configure the border (up)
if 1 {
	ConfigBorder $oop 0 0 $width [expr {$height / 2}] out up
	ConfigBorder $oop 1 1 [expr {$width - 2}] [expr {$height / 2 - 2}] in up

	set y [expr {$height / 2}]
	
	# If the control is an odd number of pixels tall, then move
	# the down button down 1 pixel
	set fiddle [expr {($height & 1) != 0}]

	incr y $fiddle

	ConfigBorder $oop 0 $y $width [expr {$height / 2}] out down
	ConfigBorder $oop 1 [expr {$y + 1}] [expr {$width - 2}] [expr {$height / 2 - 2}] in down
} else {

	set bumpUp 0
	if {[string equal [Info $oop relief,up] raised]} {
		set fillTopLeftOut $::SystemButtonFace
		set fillBottomRightOut $::System3dDarkShadow
		set fillTopLeftIn $::SystemButtonHighlight
		set fillBottomRightIn $::SystemButtonShadow
	} else {
		set fillTopLeftOut $::SystemButtonShadow
		set fillBottomRightOut $::SystemButtonHighlight
		set fillTopLeftIn $::System3dDarkShadow
		set fillBottomRightIn $::SystemButtonFace
		set bumpUp 1
	}
	set top 0
	set left 0
	set right [expr {$width - 1}]
	set bottom [expr {$height / 2 - 1}]

	$canvasCmd itemconfigure out.topleft,up -fill $fillTopLeftOut
	$canvasCmd itemconfigure out.bottomright,up -fill $fillBottomRightOut
	$canvasCmd coords out.topleft,up \
		0 $bottom 0 0 $right 0
	$canvasCmd coords out.bottomright,up \
		0 $bottom  $right $bottom  $right -1

	incr top
	incr left
	incr right -1
	incr bottom -1
	$canvasCmd itemconfigure in.topleft,up -fill $fillTopLeftIn
	$canvasCmd itemconfigure in.bottomright,up -fill $fillBottomRightIn
	$canvasCmd coords in.topleft,up \
		$left $bottom $left $top $right $left
	$canvasCmd coords in.bottomright,up \
		$left $bottom  $right $bottom  $right [expr {$top - 1}]

	# If the control is an odd number of pixels tall, then move
	# the down button down 1 pixel
	set fiddle [expr {($height & 1) != 0}]

	# Configure the border (down)
	set bumpDown 0
	if {[string equal [Info $oop relief,down] raised]} {
		set fillTopLeftOut $::SystemButtonFace
		set fillBottomRightOut $::System3dDarkShadow
		set fillTopLeftIn $::SystemButtonHighlight
		set fillBottomRightIn $::SystemButtonShadow
	} else {
		set fillTopLeftOut $::SystemButtonShadow
		set fillBottomRightOut $::SystemButtonHighlight
		set fillTopLeftIn $::System3dDarkShadow
		set fillBottomRightIn $::SystemButtonFace
		set bumpDown 1
	}
	set left 0
	set right [expr {$width - 1}]
	set top [expr {$fiddle + $height / 2}]
	set bottom [expr {$height - 1}]
	$canvasCmd itemconfigure out.topleft,down -fill $fillTopLeftOut
	$canvasCmd itemconfigure out.bottomright,down -fill $fillBottomRightOut
	$canvasCmd coords out.topleft,down \
		$left $bottom  $left $top  $right $top
	$canvasCmd coords out.bottomright,down \
		$left $bottom  $right $bottom  $right [expr {$top - 1}]

	incr top
	incr left
	incr right -1
	incr bottom -1
	$canvasCmd itemconfigure in.topleft,down -fill $fillTopLeftIn
	$canvasCmd itemconfigure in.bottomright,down -fill $fillBottomRightIn
	$canvasCmd coords in.topleft,down \
		$left $bottom $left $top $right $top
	$canvasCmd coords in.bottomright,down \
		$left $bottom  $right $bottom  $right [expr {$top - 1}]
}

	# The arrow (up)
	set arrowWidth [Info $oop width,arrow]
	set arrowHeight [Info $oop height,arrow]
	if {$isDisabled} {
		set fill $::SystemButtonShadow
		set fillbg $::SystemButtonHighlight
	} else {
		set fill Black
		set fillbg ""
	}
	set bumpUp [string equal [Info $oop relief,up] sunken]
	set x [expr {$bumpUp + ($width - $arrowWidth) / 2}]
	set y [expr {$bumpUp + ($height / 2 - $arrowHeight) / 2}]
	$canvasCmd itemconfigure arrow,up -fill $fill -outline $fill
	$canvasCmd itemconfigure arrowbg,up -fill $fillbg -outline $fillbg
	$canvasCmd coords arrow,up $x [expr {$y + $arrowHeight - 1}] \
		[expr {$x + $arrowWidth - 1}] [expr {$y + $arrowHeight - 1}] \
		[expr {$x + $arrowWidth / 2}] $y
	incr x
	incr y
	$canvasCmd coords arrowbg,up $x [expr {$y + $arrowHeight - 1}] \
		[expr {$x + $arrowWidth - 1}] [expr {$y + $arrowHeight - 1}] \
		[expr {$x + $arrowWidth / 2}] $y

	# The arrow (down)
	set bumpDown [string equal [Info $oop relief,down] sunken]
	set x [expr {$bumpDown + ($width - $arrowWidth) / 2}]
	set y [expr {$fiddle + $bumpDown + $height / 2 + ($height / 2 - $arrowHeight) / 2}]
	$canvasCmd itemconfigure arrow,down -fill $fill -outline $fill
	$canvasCmd itemconfigure arrowbg,down -fill $fillbg -outline $fillbg
	$canvasCmd coords arrow,down $x $y [expr {$x + $arrowWidth - 1}] $y \
		[expr {$x + $arrowWidth / 2}] [expr {$y + $arrowHeight - 1}]
	incr x
	incr y
	$canvasCmd coords arrowbg,down $x $y [expr {$x + $arrowWidth - 1}] $y \
		[expr {$x + $arrowWidth / 2}] [expr {$y + $arrowHeight - 1}]

	return
}

proc NSUpDownControl::ConfigBorder {oop x y wid hgt out up} {

	variable Fill

	set canvasCmd [Info $oop canvasCmd]

	set relief [Info $oop relief,$up]
	$canvasCmd itemconfigure $out.topleft,$up -fill $Fill($relief,$out,topleft)
	$canvasCmd itemconfigure $out.bottomright,$up -fill $Fill($relief,$out,bottomright)

	set left $x
	set top $y
	set right [expr {$left + $wid - 1}]
	set bottom [expr {$top + $hgt - 1}]
	$canvasCmd coords $out.topleft,$up \
		$left $bottom $left $top $right $top
	$canvasCmd coords $out.bottomright,$up \
		$left $bottom  $right $bottom  $right [expr {$top - 1}]

	return
}

# NSUpDownControl::ButtonPress --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownControl::ButtonPress {w x y} {

	variable Priv

	set oop $Priv(oop,$w)
	if {[string compare [Info $oop state] disabled]} {
		if {$y < [winfo height $w] / 2} {
			set press up
			set delta +1
		} else {
			set press down
			set delta -1
		}
		Configure $oop -relief$press sunken
		Info $oop press $press
		Invoke $oop $delta

		Info $oop afterId [after 500 NSUpDownControl::Auto $oop $delta]
	}

	return
}

proc NSUpDownControl::Auto {oop delta} {

	Invoke $oop $delta

	Info $oop afterId [after 200 NSUpDownControl::Auto $oop $delta]

	return
}

# NSUpDownControl::ButtonUp --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownControl::ButtonUp {w x y} {

	variable Priv

	set oop $Priv(oop,$w)
	if {[string compare [Info $oop state] disabled]} {
		Configure $oop -state active -relief[Info $oop press] raised
after cancel [Info $oop afterId]
Info $oop afterId ""
return
		set canvas [Info $oop canvas]
		set pointerx [winfo pointerx $canvas]
		set pointery [winfo pointery $canvas]
		if {[winfo containing $pointerx $pointery] == $canvas} {
			# nothing
		}
	}

	return
}

# NSUpDownControl::ButtonDown --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownControl::ButtonDown {w} {

return

	variable Priv

	set oop $Priv(oop,$w)
	if {[string compare [Info $oop state] disabled]} {
		Configure $oop -relief[Info $oop press] sunken
	}

	return
}

# NSUpDownControl::Invoke --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownControl::Invoke {oop delta} {

	set command [Info $oop command]
	if {[string length $command]} {
		uplevel #0 $command $delta
	}

	return
}

# NSUpDownControl::Resize --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSUpDownControl::Resize {w width height} {

	variable Priv

	set oop $Priv(oop,$w)

	Configure $oop -width $width -height $height

	return
}

# updowncontrol --
#
#	Call this to create a new up-down control.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc updowncontrol {args} {

	set oop [eval NSObject::New NSUpDownControl $args]
	return [NSUpDownControl::Info $oop command]
}

