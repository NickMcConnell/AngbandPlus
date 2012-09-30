# File: win98ToolbarButton.tcl

# Purpose: Win98-like toolbar button

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSWin98ToolbarButton {

	variable Priv

	# Win98 Explorer buttons are 54x36 or 54x36+13 pixels

	variable optionTable [list \
		"script" "-command" "invokeCmd" "" doCommand \
		"string" "-state" "state" "normal" doState \
		"string" "-relief" "relief" "flat" doRelief \
		"image" "-image" "image" "" doImage \
		"image" "-imageactive" "imageActive" "" doImage \
		"image" "-imagedisabled" "imageDisabled" "" doImage \
		"image" "-imagenormal" "imageNormal" "" doImage \
		"pixels" "-width" "width" "48" doSize \
		"pixels" "-height" "height" "30" doSize \
		"pixels" "-widthimage" "widthImage" "16" doSize \
		"pixels" "-heightimage" "heightImage" "16" doSize \
		"pixels" "-widthlabel" "widthLabel" "48" doSize \
		"pixels" "-heightlabel" "heightLabel" "13" doSize \
		"boolean" "-showimage" "showImage" "1" doButton \
		"string" "-label" "label" "" doLabel \
		"boolean" "-showlabel" "showLabel" "1" doLabel \
		"boolean" "-hasmenu" "hasMenu" "0" doMenu \
		"boolean" "-onlymenu" "onlyMenu" "0" doMenu \
		"string" "-reliefmenu" "reliefMenu" "flat" doRelief \
		"script" "-menucommand" "menuCmd" "" doCommand \
		"pixels" "-padx" "padX" "3" doSize \
		"pixels" "-pady" "padY" "3" doSize \
		"variable" "-labelvariable" "labelVar" "" doLabel
	]
}

# NSWin98ToolbarButton::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWin98ToolbarButton::InitModule {} {

	variable Priv

if 0 {
	catch {image delete Image_ComboBox}

	set dot Black
	set image Image_ComboBox
	image create photo $image
	$image put [list [list $dot $dot $dot $dot $dot]]  -to 0 0
	$image put      [list [list $dot $dot $dot]]       -to 1 1
	$image put           [list [list $dot]]            -to 2 2
}

	bind NSWin98ToolbarButtonBindTag <Enter> \
		"NSWin98ToolbarButton::ButtonEnter %W"
	bind NSWin98ToolbarButtonBindTag <Leave> \
		"NSWin98ToolbarButton::ButtonLeave %W"
	bind NSWin98ToolbarButtonBindTag <ButtonPress-1> \
		"NSWin98ToolbarButton::ButtonPress %W %x %y"
	bind NSWin98ToolbarButtonBindTag <Button1-Enter> \
		"NSWin98ToolbarButton::ButtonDown %W"
	bind NSWin98ToolbarButtonBindTag <ButtonRelease-1> \
		"NSWin98ToolbarButton::ButtonUp %W %x %y"

	bind NSWin98ToolbarButtonBindTag <FocusOut> \
		"NSWin98ToolbarButton::FocusOut %W"

	set Priv(focus) ""
	set Priv(posted) 0

	return
}

# NSWin98ToolbarButton::NSWin98ToolbarButton --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWin98ToolbarButton::NSWin98ToolbarButton {oop path args} {

	variable optionTable
	variable Priv

	set canvas $path
	canvas $canvas -borderwidth 0 -highlightthickness 0 -scrollregion {0 0 0 0}
	set canvasCmd win98Canvas$canvas
	rename $canvas $canvasCmd
	set Priv(oop,$canvas) $oop

	# This is the command seen by the outside
	set command $path
if 1 {
	interp alias {} ::$command {} ::NSWin98ToolbarButton::Command $oop
} else {
	proc ::$command args \
		"eval NSWin98ToolbarButton::Command $oop \$args"
}
	Info $oop command $command

	# The border
	$canvasCmd create line 0 1  0 0  1 0 -fill $::SystemButtonHighlight -tags topleft
	$canvasCmd create line 0 1  1 1  1 0 -fill $::SystemButtonShadow -tags bottomright

	# The menu-button
	$canvasCmd create line 0 1  0 0  1 0 -fill $::SystemButtonHighlight -tags topleftmenu
	$canvasCmd create line 0 1  1 1  1 0 -fill $::SystemButtonShadow -tags bottomrightmenu
	$canvasCmd create polygon 0 0  4 0  2 0 -outline black -tags combobg
	$canvasCmd create polygon 0 0  4 0  2 0 -outline black -tags combo

	# The image
	$canvasCmd create image 0 0 -image Image_Empty -anchor n -tags image

	# The label
	$canvasCmd create text 0 0 -text "" -tags labelbg -anchor s
	$canvasCmd create text 0 0 -text "" -tags label -anchor s

	NSUtils::DestroyObjectWithWidget NSWin98ToolbarButton $oop $canvas

	bindtags $canvas [concat [bindtags $canvas] NSWin98ToolbarButtonBindTag]

	Info $oop canvas $canvas
	Info $oop canvasCmd $canvasCmd
	Info $oop menuVisible 0
	Info $oop inFocusOut 0
	Info $oop oldLabelVar ""
	
	foreach {type arg info default flags} $optionTable {
		Info $oop $info $default
	}

	eval Configure $oop $args
		
	return
}

# NSWin98ToolbarButton::~NSWin98ToolbarButton --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWin98ToolbarButton::~NSWin98ToolbarButton {oop} {

	variable Priv

	catch {
		set labelVar [Info $oop labelVar]
		if {[string length $labelVar]} {
			trace vdelete $labelVar w \
				"NSWin98ToolbarButton::LabelVarChanged $oop"
		}
	}
	catch {
		set canvas [Info $oop canvas]
		unset Priv(oop,$canvas)
		if {$Priv(posted) == $oop} {
			set Priv(posted) 0
		}
	}

	return
}

# NSWin98ToolbarButton::Command --
#
#	This is the command called for a toolbar button. The widget pathname
#	command is a wrapper around this.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWin98ToolbarButton::Command {oop command args} {

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
		hidemenu {
			return [eval HideMenu $oop $args]
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

# NSWin98ToolbarButton::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWin98ToolbarButton::Info {oop info args} {

	global NSWin98ToolbarButton

	# Verify the object
	NSObject::CheckObject NSWin98ToolbarButton $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSWin98ToolbarButton($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSWin98ToolbarButton($oop,$info)
			}
		}
	}

	return
}

# NSWin98ToolbarButton::Configure --
#
#	Change configuration options.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWin98ToolbarButton::Configure {oop args} {

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

#		if {([Info $oop image] == "") && ([Info $oop label] == "")} {
#			set errorString "must specify image and/or label"
#			continue
#		}

		set oldLabelVar [Info $oop oldLabelVar]
		set labelVar [Info $oop labelVar]
		if {[string compare $oldLabelVar $labelVar]} {
			if {[string length $oldLabelVar]} {
				trace vdelete $oldLabelVar w \
					"NSWin98ToolbarButton::LabelVarChanged $oop"
			}
#			global $labelVar
			trace variable $labelVar w \
				"NSWin98ToolbarButton::LabelVarChanged $oop"
			Info $oop oldLabelVar $labelVar

			LabelVarChanged $oop
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

proc NSWin98ToolbarButton::Max {a b} {

	return [expr {$a > $b ? $a : $b}]
}

# NSWin98ToolbarButton::WorldChanged --
#
#	Called when configuration options change.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWin98ToolbarButton::WorldChanged {oop} {

	set canvas [Info $oop canvas]
	set canvasCmd [Info $oop canvasCmd]
	set hasMenu [Info $oop hasMenu]
	set onlyMenu [Info $oop onlyMenu]
	set showImage [Info $oop showImage]
	set showLabel [Info $oop showLabel]
	set state [Info $oop state]

	set isActive [string equal $state active]
	set isDisabled [string equal $state disabled]

	set width 0
	set height 0
	set minWidth 0
	set minHeight 0

	set padX [Info $oop padX]
	set padY [Info $oop padY]

	if {$showLabel} {
		set width [Max $width [Info $oop widthLabel]]
		incr height [Info $oop heightLabel]
		set font [$canvasCmd itemcget label -font]
		set minWidth [Max $minWidth [font measure $font [Info $oop label]]]
		set minHeight [Max $minHeight [font metrics $font -linespace]]
	}
	if {$showImage} {
		set width [Max $width [Info $oop widthImage]]
		incr height [Info $oop heightImage]
#		if {!$showLabel && $hasMenu && $onlyMenu} {
#			incr width [expr {$padX + 5}]
#		}
		set image [Info $oop imageNormal]
		if {![string length $image]} {
			set image [Info $oop image]
		}
		if {![string length $image]} {
			set image Image_Empty
		}
		set minWidth [Max $minWidth [image width $image]]
		set minHeight [Max $minHeight [image height $image]]
	}
	if {$showImage && $showLabel} {
		set width [Max $width [Info $oop width]]
		set height [Max $height [Info $oop height]]
	}
if {$hasMenu} {
	if {$onlyMenu} {
#		set width [Max $width [expr {$width + $padX + 5}]]
		set minWidth [Max $minWidth [expr {$minWidth + $padX + 5}]]
	} else {
#		set width [Max $width [expr {$width + 13}]]
#		set minWidth [Max $minWidth [expr {$minWidth + 13}]]
	}
}
#puts "minWidth $minWidth width $width  minHeight $minHeight height $height"
incr minWidth [expr {$padX * 2}]
incr width [expr {$padX * 2}]
if {$width < $minWidth} {
	set width $minWidth
}
incr minHeight [expr {$padY * 2}]
incr height [expr {$padY * 2}]
if {$height < $minHeight} {
	set height $minHeight
}
	set canvasWidth $width
	if {$hasMenu && !$onlyMenu} {
		incr canvasWidth 13
	}
	$canvasCmd configure -width $canvasWidth -height $height

	# Configure the border
	set bump 0
	if {$isActive} {
		if {[Info $oop relief] == "raised"} {
			set fillTopLeft $::SystemButtonHighlight
			set fillBottomRight $::SystemButtonShadow
		} else {
			set fillTopLeft $::SystemButtonShadow
			set fillBottomRight $::SystemButtonHighlight
			set bump 1
		}
	} else {
		set fillTopLeft [$canvasCmd cget -background]
		set fillBottomRight [$canvasCmd cget -background]
	}
	set widLessOne [expr {$width - 1}]
	set hgtLessOne [expr {$height - 1}]
	$canvasCmd itemconfigure topleft -fill $fillTopLeft
	$canvasCmd itemconfigure bottomright -fill $fillBottomRight
	$canvasCmd coords topleft 0 $hgtLessOne 0 0 $widLessOne 0
	$canvasCmd coords bottomright 0 $hgtLessOne  $widLessOne $hgtLessOne  $widLessOne -1

	# The menu-button
	if {$hasMenu && !$onlyMenu} {
		set menuLeft [expr {$canvasWidth - 13}]
		set widLessOne [expr {$canvasWidth - 1}]
		set hgtLessOne [expr {$height - 1}]
		set oldBump $bump
		if {[Info $oop reliefMenu] != [Info $oop relief]} {
			if {[Info $oop reliefMenu] == "raised"} {
				set fillTopLeft $::SystemButtonHighlight
				set fillBottomRight $::SystemButtonShadow
			} elseif {[Info $oop reliefMenu] == "sunken"} {
				set fillTopLeft $::SystemButtonShadow
				set fillBottomRight $::SystemButtonHighlight
				set bump 1
			} else {
				set fillTopLeft [$canvasCmd cget -background]
				set fillBottomRight [$canvasCmd cget -background]
			}
		}
		$canvasCmd itemconfigure topleftmenu -fill $fillTopLeft
		$canvasCmd itemconfigure bottomrightmenu -fill $fillBottomRight
		$canvasCmd coords topleftmenu $menuLeft $hgtLessOne $menuLeft 0 $widLessOne 0
		$canvasCmd coords bottomrightmenu $menuLeft $hgtLessOne $widLessOne \
			$hgtLessOne $widLessOne 0
		set iWid 5
		set iHgt 3
		set x [expr {$menuLeft + (13 - $iWid) / 2 + $bump}]
		set y [expr {($height - $iHgt) / 2 + $bump}]
		if {[string equal $state disabled]} {
			set fill $::SystemButtonShadow
			set fillbg $::SystemButtonHighlight
		} else {
			set fill Black
			set fillbg ""
		}
		$canvasCmd itemconfigure combo -fill $fill -outline $fill
		$canvasCmd itemconfigure combobg -fill $fillbg -outline $fillbg
		$canvasCmd coords combo $x $y [expr {$x + 4}] $y [expr {$x + 2}] [expr {$y + 2}]
		incr x
		incr y
		$canvasCmd coords combobg $x $y [expr {$x + 4}] $y [expr {$x + 2}] [expr {$y + 2}]
		set bump $oldBump
	} elseif {$hasMenu} {
		set iWid 5
		set iHgt 3
		if {$showImage} {
			set state [string totitle $state]
			set image [Info $oop image$state]
			if {![string length $image]} {
				set image [Info $oop image]
			}
			if {![string length $image]} {
				set image Image_Empty
			}
			set xWid [image width $image]
		} else {
			set xWid [font measure [$canvasCmd itemcget label -font] \
				[Info $oop label]]
		}
		set x [expr {$width - ($width - ($xWid + $padX + $iWid)) / 2 - $iWid + $bump}]
		if {$showLabel && $showImage} {
			set y [expr {$padY + ([Info $oop heightImage] - $iHgt) / 2 + $bump}]
		} else {
			set y [expr {($height - $iHgt) / 2 + $bump}]
		}
		if {$isDisabled} {
			set fill $::SystemButtonShadow
			set fillbg $::SystemButtonHighlight
		} else {
			set fill Black
			set fillbg ""
		}
		$canvasCmd itemconfigure combo -fill $fill -outline $fill
		$canvasCmd itemconfigure combobg -fill $fillbg -outline $fillbg
		$canvasCmd coords combo $x $y [expr {$x + 4}] $y [expr {$x + 2}] [expr {$y + 2}]
		incr x
		incr y
		$canvasCmd coords combobg $x $y [expr {$x + 4}] $y [expr {$x + 2}] [expr {$y + 2}]

		$canvasCmd itemconfigure topleftmenu -fill ""
		$canvasCmd itemconfigure bottomrightmenu -fill ""
	} else {
		$canvasCmd itemconfigure combo -fill "" -outline ""
		$canvasCmd itemconfigure combobg -fill "" -outline ""
		$canvasCmd itemconfigure topleftmenu -fill ""
		$canvasCmd itemconfigure bottomrightmenu -fill ""
	}

	# Configure the image
	if {$showImage} {
		set state [string totitle $state]
		set image [Info $oop image$state]
		if {![string length $image]} {
			set image [Info $oop image]
		}
		if {![string length $image]} {
			set image Image_Empty
		}
		set iWid [image width $image]
		set iHgt [image height $image]
		if {$hasMenu && $onlyMenu} {
			set x [expr {$width / 2 - 4 + $bump}]
		} else {
			set x [expr {$width / 2 + $bump}]
		}
		if {$showLabel} {
			set y [expr {$padY + [Info $oop heightImage] / 2 + $bump}]
			set anchor center
		} else {
			set y [expr {$height / 2 + $bump}]
			set anchor center
		}
		$canvasCmd itemconfigure image -image $image -anchor $anchor
		$canvasCmd coords image $x $y
	} else {
		$canvasCmd itemconfigure image -image Image_Empty
	}
	
	# Configure the label
	if {$showLabel} {
		set text [Info $oop label]
		if {$isDisabled} {
			set fill $::SystemButtonShadow
			set fillbg $::SystemButtonHighlight
		} else {
			set fill Black
			set fillbg ""
		}
		if {!$showImage && $hasMenu && $onlyMenu} {
			set x [expr {($width - ($padX + 5)) / 2 + $bump}]
		} else {
			set x [expr {$width / 2 + $bump}]
		}
		if {$showImage} {
			set y [expr {$height - $padY + $bump}]
			set anchor s
		} else {
			set y [expr {$height / 2 + $bump}]
			set anchor center
		}
		$canvasCmd itemconfigure label -text $text -fill $fill -anchor $anchor
		$canvasCmd itemconfigure labelbg -text $text -fill $fillbg -anchor $anchor
		$canvasCmd coords label $x $y
		$canvasCmd coords labelbg [incr x] [incr y]
	} else {
		$canvasCmd itemconfigure label -fill ""
		$canvasCmd itemconfigure labelbg -fill ""
	}

	return
}

# NSWin98ToolbarButton::Invoke --
#
#	Call the client command.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWin98ToolbarButton::Invoke {oop} {

	set command [Info $oop invokeCmd]
	if {[string length $command]} {
		uplevel #0 $command
	}

	return
}

# NSWin98ToolbarButton::ShowMenu --
#
#	Call the client command to show the menu.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWin98ToolbarButton::ShowMenu {oop} {

	variable Priv

	set canvas [Info $oop canvas]

	set command [Info $oop menuCmd]
	if {![string length $command]} {
		return
	}

	# This is like the Tk menubutton code
	if {$Priv(posted)} {
		HideMenu $Priv(posted)
	}
	set Priv(focus) [focus]
	if {[string length $Priv(focus)]} {
		if {[string compare [winfo toplevel $Priv(focus)] \
			[winfo toplevel $canvas]]} {
			set Priv(focus) [focus -lastfor [winfo toplevel $canvas]]
		}
	}
	set Priv(posted) $oop

	Info $oop menuVisible 1
#	focus $canvas
	uplevel #0 $command [Info $oop command]

	return
}

# NSWin98ToolbarButton::HideMenu --
#
#	Called when the menu is unposted.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWin98ToolbarButton::HideMenu {oop} {

	variable Priv

	if {[catch {NSObject::CheckObject NSWin98ToolbarButton $oop}]} {
		return
	}

	if {![Info $oop menuVisible]} {
		return
	}

#	catch {focus $Priv(focus)}
	set Priv(focus) ""
	set Priv(posted) 0

	Info $oop menuVisible 0

	set canvas [Info $oop canvas]
	set pointerx [winfo pointerx $canvas]
	set pointery [winfo pointery $canvas]
	if {[string compare [winfo containing $pointerx $pointery] $canvas]} {
		ButtonLeave $canvas
	} else {
		Configure $oop -state active -relief raised -reliefmenu raised
	}
	
	return
}

# NSWin98ToolbarButton::ButtonEnter --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWin98ToolbarButton::ButtonEnter {w} {

	variable Priv

	set oop $Priv(oop,$w)
	if {([Info $oop state] != "disabled") && ![Info $oop menuVisible]} {
		Configure $oop -state active -relief raised -reliefmenu raised
	}

	return
}

# NSWin98ToolbarButton::ButtonLeave --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWin98ToolbarButton::ButtonLeave {w} {

	variable Priv

	set oop $Priv(oop,$w)
	if {[string compare [Info $oop state] disabled] && ![Info $oop menuVisible]} {
		Configure $oop -state normal -relief flat -reliefmenu flat
	}

	return
}

# NSWin98ToolbarButton::ButtonPress --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWin98ToolbarButton::ButtonPress {w x y} {

	variable Priv

	set oop $Priv(oop,$w)
	if {[string compare [Info $oop state] disabled]} {
		if {[Info $oop menuVisible]} {
			Configure $oop -state active -reliefmenu raised
			HideMenu $oop
		} elseif {[Info $oop hasMenu] && [Info $oop onlyMenu]} {
			Configure $oop -state active -relief sunken
			ShowMenu $oop
		} elseif {[Info $oop hasMenu] && ($x >= ([winfo width $w] - 13))} {
			Configure $oop -state active -reliefmenu sunken
			ShowMenu $oop
		} else {
			Configure $oop -state active -relief sunken \
				-reliefmenu sunken
		}
	}

	return
}

# NSWin98ToolbarButton::ButtonUp --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWin98ToolbarButton::ButtonUp {w x y} {

	variable Priv

	set oop $Priv(oop,$w)
	if {([Info $oop state] != "disabled") && ![Info $oop menuVisible]} {
		Configure $oop -state active -relief raised -reliefmenu raised
		set canvas [Info $oop canvas]
		set pointerx [winfo pointerx $canvas]
		set pointery [winfo pointery $canvas]
		if {[winfo containing $pointerx $pointery] == $canvas} {
			if {![Info $oop hasMenu] || (![Info $oop onlyMenu] && \
				($x < [winfo width $w] - 13))} {
				Invoke $oop
			}
		}
	}

	return
}

# NSWin98ToolbarButton::ButtonDown --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWin98ToolbarButton::ButtonDown {w} {

	variable Priv

	set oop $Priv(oop,$w)
	if {[Info $oop state] != "disabled"} {
		if {[Info $oop menuVisible]} {
		} else {
			Configure $oop -state active -relief sunken -reliefmenu sunken
		}
	}

	return
}

# NSWin98ToolbarButton::LabelVarChanged --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWin98ToolbarButton::LabelVarChanged {oop args} {

	set labelVar [Info $oop labelVar]
	global $labelVar
	Configure $oop -label [set $labelVar]

	return
}

# NSWin98ToolbarButton::FocusOut --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWin98ToolbarButton::FocusOut {w} {

#####
return
#####

	variable Priv

	set oop $Priv(oop,$w)
	if {[Info $oop menuVisible]} {
		Info $oop inFocusOut 1
		HideMenu $oop
		Info $oop inFocusOut 0
	}

	return
}

# win98button --
#
#	Call this to create a new button.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc win98button {args} {

	set oop [eval NSObject::New NSWin98ToolbarButton $args]
	return [NSWin98ToolbarButton::Info $oop command]
}

