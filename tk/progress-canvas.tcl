# File: progress-canvas.tcl

# Purpose: canvas used in the Progress Window and Main Window

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSProgressCanvas {

# namespace eval NSProgressCanvas
}

# NSProgressCanvas::InitModule -- 
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressCanvas::InitModule {} {

	variable Priv

	MsgCatInit misc-win

	return
}

# NSProgressCanvas::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressCanvas::CloseModule {} {

	return
}

# NSProgressCanvas::NSProgressCanvas --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressCanvas::NSProgressCanvas {oop args} {

	array set opts {
		-parent ""
		-layout wide
	}
	array set opts $args

	Info $oop parent $opts(-parent)
	Info $oop layout $opts(-layout)

	set c [InitDisplay $oop]
	Info $oop canvas $c

	# Destroy the object along with the canvas (later)
	NSUtils::DestroyObjectWithWidget NSProgressCanvas $oop $c

	return
}

# NSProgressCanvas::~NSProgressCanvas --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressCanvas::~NSProgressCanvas {oop} {

	return
}

# NSProgressCanvas::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressCanvas::Info {oop info args} {

	global NSProgressCanvas

	# Verify the object
	NSObject::CheckObject NSProgressCanvas $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSProgressCanvas($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSProgressCanvas($oop,$info)
			}
		}
	}

	return
}

proc NSProgressCanvas::Update_HP_SP_FD {oop which cur max frac} {

	set canvas [Info $oop canvas]

	if {[Value progress,showNumbers]} {
		if {$which ne "food"} {
			set hitpoint_warn [angband setting set hitpoint_warn]
			if {$cur >= $max} {
				set fill [Value TERM_L_GREEN]
				if {$which eq "mana"} {
					scan [angband player mana_cumber] "%d %d" glove armor
					if {$glove > 0 || $armor > 0} {
						set fill [Value TERM_ORANGE]
					}
				}
			} elseif {$cur > ($max * $hitpoint_warn) / 10} {
				set fill [Value TERM_YELLOW]
			} else {
				if {$cur < 0} {set cur 0}
				set fill [Value TERM_L_RED]
			}
		} else {
			if {$cur < 500} {
				set fill [Value TERM_L_RED]
			} elseif {$cur < 1000} {
				set fill [Value TERM_ORANGE]
			} elseif {$cur < 2000} {
				set fill [Value TERM_YELLOW]
			} elseif {$cur < 15000} {
				set fill [Value TERM_L_GREEN]
			} else {
				set fill [Value TERM_GREEN]
			}
		}
		if {[Value progress,showMax]} {
			append cur /
			$canvas itemconfigure $which-max -fill $fill -text $max
		}
		$canvas itemconfigure $which-cur -fill $fill -text $cur
	}

	if {[Value progress,showBars]} {
		NSProgress::SetDoneRatio [Info $oop progId,$which] $frac
	}

	return
}

#+EN
proc NSProgressCanvas::Update_WT {oop} {

	set canvas [Info $oop canvas]

	set cur [angband player total_weight]
	set max [angband inventory weight_limit]
	set frac [expr {double($cur) / $max}]

	if {[Value progress,showNumbers]} {
		set capacity [expr {$max / 2 + $max / 10}]
		if {$cur > $max} {
			set fill [Value TERM_L_RED]
		} elseif {$cur >= $capacity} {
			set fill [Value TERM_YELLOW]
		} else {
			set fill [Value TERM_L_GREEN]
		}
		set cur [fmt_wgt $cur]
		set max [fmt_wgt $max]
		if {[Value progress,showMax]} {
			append cur /
			$canvas itemconfigure weight-max -fill $fill -text $max
		}
		$canvas itemconfigure weight-cur -fill $fill -text $cur
	}

	if {[Value progress,showBars]} {
		NSProgress::SetDoneRatio [Info $oop progId,weight] $frac
	}

	return
}
#-EN

proc NSProgressCanvas::InitDisplay {oop} {

	global NSProgress

	set parent [Info $oop parent]

	set font [Value font,misc]

	set c $parent.progress$oop
	canvas $c \
		-scrollregion "0 0 0 0" -relief flat -highlightthickness 0 \
		-background Black

	# Display help string in Main Window status bar
	CanvasFeedback::Init $c

	# Border around each bar
	foreach attrib {hitpoints mana food weight} {
		$c create line 0 0 0 0 -fill #666699 \
			-tags [list border border1-$attrib]
		$c create line 0 0 0 0 -fill #333366 \
			-tags [list border border2-$attrib]
	}

	# Bar titles
	foreach title {HP SP FD WT} attrib {hitpoints mana food weight} {
		$c create text 0 0 -text [mc $title] -font $font \
			-fill White -anchor sw -tags "font title-$attrib"
		CanvasFeedback::Add $c title-$attrib \
			"NSProgressCanvas::CanvasFeedbackCmd $oop $attrib"
	}

	# Bars and cur/max
	foreach attrib {hitpoints mana food weight} color {Red Blue Brown #808000} {
		set progId [NSObject::New NSProgress $c 100 6 $color gray60]
		Info $oop progId,$attrib $progId

		NSStatusText::StatusCommand $NSProgress($progId,frame) \
			[Global main,statusBar] \
			"NSProgressCanvas::CanvasFeedbackCmd $oop $attrib enter"

		$c create text 0 0 -text 99999 -font $font \
			-fill [Value TERM_L_GREEN] -anchor se \
			-tags [list font $attrib $attrib-cur]
		$c create text 0 0 -font $font -anchor se \
			-tags [list font $attrib $attrib-max]
		CanvasFeedback::Add $c $attrib \
			"NSProgressCanvas::CanvasFeedbackCmd $oop $attrib"
	}

	foreach attrib {hitpoints mana food} {
		qebind $c <Py-$attrib> \
			"NSProgressCanvas::Update_HP_SP_FD $oop $attrib %c %m %f"
		qeconfigure $c <Py-$attrib> -active no
	}

	qebind $c <Track-inventory> \
		"NSProgressCanvas::Update_WT $oop"
	qebind $c <Stat-strength> \
		"if {\[qeconfigure %W <Track-inventory> -active]} {
			NSProgressCanvas::Update_WT $oop
		}"
	if {[variant OANGBANDTK]} {
		qebind $c <Setting-use_metric> \
			"if {\[qeconfigure %W <Track-inventory> -active]} {
				NSProgressCanvas::Update_WT $oop
			}"
	}
	qeconfigure $c <Track-inventory> -active no

	# Update ourself when the font for the Misc Window changes
	qebind $c <Value-font,misc> "NSProgressCanvas::Arrange $oop"

	bind $c <Configure> \
		"NSProgressCanvas::Configure $oop %w %h"

	#
	# Context Menu
	#

	set menu $c.context
	menu $menu -tearoff 0
	bind $c <ButtonPress-3> \
		"NSProgressCanvas::ContextMenu $oop $menu %X %Y"
	foreach attrib {hitpoints mana food weight} {
		set progId [Info $oop progId,$attrib]
		set w $NSProgress($progId,frame)
		bind $w <ButtonPress-3> \
			"NSProgressCanvas::ContextMenu $oop $menu %X %Y"
		bind $w.done <ButtonPress-3> \
			"NSProgressCanvas::ContextMenu $oop $menu %X %Y"
		bind $w.todo <ButtonPress-3> \
			"NSProgressCanvas::ContextMenu $oop $menu %X %Y"
	}

	return $c
}

# NSProgressCanvas::ArrangeWide --
#
#	Arranges stuff in the Progress Window depending on the current
#	configuration options.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressCanvas::ArrangeWide {oop} {

	set canvas [Info $oop canvas]

	set width [$canvas cget -width]

	set showBars [Value progress,showBars]
	set showNumbers [Value progress,showNumbers]
	set showMax [Value progress,showMax]

	# Get the desired font
	set font [Value font,misc]

	# Calculate the height of a line
	set fontHeight [font metrics $font -linespace]

	# Set the font of items with the "font" tag
	$canvas itemconfigure font -font $font

	if {[Value progress,float]} {
		set padY 4
	} else {
		set padY 0
	}

	# Count visible rows
	set count 0
	foreach title {HP SP FD WT} {
		set show($title) [Value progress,show$title]
		if {$show($title)} {
			incr count
		}
	}

	# Set the height of the canvas
	$canvas configure -height [expr {$fontHeight * $count + $padY * 2}]

	# Calculate the width of the titles
	set titlePadX 2
	set titleWidth 0
	foreach title {HP SP FD WT} {
		if {!$show($title)} continue
		set twidth [font measure $font [mc $title]]
		if {$twidth > $titleWidth} {
			set titleWidth $twidth
		}
	}

	# Calculate width of numbers
	set numberWidth 0
	if {$showNumbers} {
		incr numberWidth 8 ; # pad
		incr numberWidth [font measure $font 99999]
		if {$showMax} {
			incr numberWidth [font measure $font /]
			set numberWidthOfMax [font measure $font 99999]
			incr numberWidth $numberWidthOfMax
			set stateMax normal
		} else {
			set stateMax hidden
		}
		set stateCur normal
	} else {
		set stateCur hidden
		set stateMax hidden
	}
	foreach attrib {hitpoints mana food weight} title {HP SP FD WT} {
		if {$show($title)} {
			$canvas itemconfigure $attrib-cur -state $stateCur
			$canvas itemconfigure $attrib-max -state $stateMax
			set state normal
		} else {
			$canvas itemconfigure $attrib-cur -state hidden
			$canvas itemconfigure $attrib-max -state hidden
			set state hidden
		}
		$canvas itemconfigure title-$attrib -state $state
		$canvas itemconfigure border1-$attrib -state $state
		$canvas itemconfigure border2-$attrib -state $state
	}

	# Space at ends of each bar
	set barPadX 6
	incr barPadX 1 ; # for border

	# Get the height of each progress bar
	set barHeight [winfo reqheight [NSProgress::Info [Info $oop progId,food] frame]]

	# Width of each bar. This is the width of the window minus the space
	# at each end of a bar, minus the bar title "HP" etc, minus the
	# numbers "12345" or "12345/12345" if numbers are showing.
	set barWidth [expr {$width - $titleWidth - $titlePadX - $numberWidth - $barPadX * 2}]

	set barLeft [expr {$width - $barPadX - $barWidth}]
	set barRight [expr {$width - $barPadX}]

	# Titles
	set y2 $padY
	foreach attrib {hitpoints mana food weight} title {HP SP FD WT} {
		if {!$show($title)} continue
		$canvas coords title-$attrib $titlePadX [incr y2 $fontHeight]
	}

	# Numbers: yes, Bars: yes
	if {$showNumbers && $showBars} {
		set x [expr {$barLeft - $barPadX}] 
		set y2 $padY
		foreach attrib {hitpoints mana food weight} title {HP SP FD WT} {
			if {!$show($title)} continue
			if {$showMax} {
				$canvas coords $attrib-max $x [incr y2 $fontHeight]
				$canvas coords $attrib-cur [expr {$x - $numberWidthOfMax}] $y2
			} else {
				$canvas coords $attrib-cur $x [incr y2 $fontHeight]
			}
		}

	# Numbers: yes, Bars: no
	} elseif {$showNumbers} {
		set x [expr {$width - $titlePadX}] 
		set y2 $padY
		foreach attrib {hitpoints mana food weight} title {HP SP FD WT} {
			if {!$show($title)} continue
			if {$showMax} {
				$canvas coords $attrib-max $x [incr y2 $fontHeight]
				$canvas coords $attrib-cur [expr {$x - $numberWidthOfMax}] $y2
			} else {
				$canvas coords $attrib-cur $x [incr y2 $fontHeight]
			}
		}

	# Don't show numbers
	} else {
	}

	# Option: Show bars
	if {$showBars} {

		# Border around each bar
		set x [expr {$barLeft - 1}]
		set y1 [expr {$padY + $fontHeight / 2 - $barHeight / 2}]
		set y2 [expr {$y1 + $barHeight}]
		foreach attrib {hitpoints mana food weight} title {HP SP FD WT} {
			if {!$show($title)} continue
			$canvas coords border1-$attrib $x $y2 $x $y1 $barRight $y1
			$canvas coords border2-$attrib $barRight $y1 $barRight $y2 $x $y2
			incr y1 $fontHeight
			incr y2 $fontHeight
		}

		# Bars
		set y [expr {$padY + $fontHeight / 2 - $barHeight / 2 + 1}]
		foreach attrib {hitpoints mana food weight} title {HP SP FD WT} {
			set progId [Info $oop progId,$attrib]
			set frame [NSProgress::Info $progId frame]
			if {!$show($title)} {
				place forget $frame
				continue
			}
			place $frame \
				-x $barLeft -y $y -anchor nw
			$frame configure -width $barWidth
			incr y $fontHeight
		}

	# Don't show bars
	} else {
		$canvas move border $width 0
		foreach bar [winfo children $canvas] {
			place forget $bar
		}
	}

	return
}

# NSProgressCanvas::ArrangeTall --
#
#	Arranges stuff in the Progress Window depending on the current
#	configuration options.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressCanvas::ArrangeTall {oop} {

	global NSProgress

	set canvas [Info $oop canvas]

	set width [$canvas cget -width]

	set showBars [Value progress,showBars]
	set showNumbers [Value progress,showNumbers]
	set showMax [Value progress,showMax]

	# Get the desired font
	set font [Value font,misc]

	# Calculate the height of a line
	set fontHeight [font metrics $font -linespace]

	# Set the font of items with the "font" tag
	$canvas itemconfigure font -font $font

	if {[Value progress,float]} {
		set padY 4
	} else {
		set padY 0
	}

	# Count visible rows
	set count 0
	foreach title {HP SP FD WT} {
		set show($title) [Value progress,show$title]
		if {$show($title)} {
			incr count
		}
	}

	# Set the height of the canvas
	if {$showBars && $showNumbers} {
		$canvas configure -height [expr {$fontHeight * $count * 2 + $padY * 2}]
	} else {
		$canvas configure -height [expr {$fontHeight * $count + $padY * 2}]
	}

	# Calculate the width of the titles
	set titlePadX 2
	set titleWidth 0
	foreach title {HP SP FD WT} {
		if {!$show($title)} continue
		set twidth [font measure $font [mc $title]]
		if {$twidth > $titleWidth} {
			set titleWidth $twidth
		}
	}

	# Calculate width of numbers
	set numberWidth 0
	if {$showNumbers} {
		incr numberWidth 8 ; # pad
		incr numberWidth [font measure $font 99999]
		if {$showMax} {
			incr numberWidth [font measure $font /]
			set numberWidthOfMax [font measure $font 99999]
			incr numberWidth $numberWidthOfMax
			set stateMax normal
		} else {
			set stateMax hidden
		}
		set stateCur normal
	} else {
		set stateCur hidden
		set stateMax hidden
	}
	foreach attrib {hitpoints mana food weight} title {HP SP FD WT} {
		if {$show($title)} {
			$canvas itemconfigure $attrib-cur -state $stateCur
			$canvas itemconfigure $attrib-max -state $stateMax
			set state normal
		} else {
			$canvas itemconfigure $attrib-cur -state hidden
			$canvas itemconfigure $attrib-max -state hidden
			set state hidden
		}
		$canvas itemconfigure title-$attrib -state $state
		$canvas itemconfigure border1-$attrib -state $state
		$canvas itemconfigure border2-$attrib -state $state
	}

	# Space at ends of each bar
	if {$showNumbers} {
		set barPadX $titlePadX
	} else {
		set barPadX 6
	}
	incr barPadX 1 ; # for border

	# Get the height of each progress bar
	set barHeight [winfo reqheight [NSProgress::Info [Info $oop progId,food] frame]]

	# Width of each bar. This is the width of the window minus the space
	# at each end of a bar, minus the bar title "HP" etc, minus the
	# numbers "12345" if numbers are showing.
	if {$showBars && $showNumbers} {
		set barWidth [expr {$width - $barPadX * 2}]
	} else {
		set barWidth [expr {$width - $titleWidth - $titlePadX - $numberWidth - $barPadX * 2}]
	}
	set barLeft [expr {$width - $barPadX - $barWidth}]
	set barRight [expr {$width - $barPadX}]

	# Titles
	set y2 $padY
	foreach attrib {hitpoints mana food weight} title {HP SP FD WT} {
		if {!$show($title)} continue
		$canvas coords title-$attrib $titlePadX [incr y2 $fontHeight]
		if {$showBars && $showNumbers} {
			incr y2 $fontHeight
		}
	}

	# Numbers: yes, Bars: yes
	if {$showNumbers && $showBars} {
		set x [expr {$width - $titlePadX}]
		set y2 $padY
		foreach attrib {hitpoints mana food weight} title {HP SP FD WT} {
			if {!$show($title)} continue
			if {$showMax} {
				$canvas coords $attrib-max $x [incr y2 $fontHeight]
				$canvas coords $attrib-cur [expr {$x - $numberWidthOfMax}] $y2
			} else {
				$canvas coords $attrib-cur $x [incr y2 $fontHeight]
			}
			incr y2 $fontHeight
		}

	# Numbers: yes, Bars: no
	} elseif {$showNumbers} {
		set x [expr {$width - $titlePadX}] 
		set y2 $padY
		foreach attrib {hitpoints mana food weight} title {HP SP FD WT} {
			if {!$show($title)} continue
			if {$showMax} {
				$canvas coords $attrib-max $x [incr y2 $fontHeight]
				$canvas coords $attrib-cur [expr {$x - $numberWidthOfMax}] $y2
			} else {
				$canvas coords $attrib-cur $x [incr y2 $fontHeight]
			}
		}

	# Don't show numbers
	} else {
	}

	# Option: Show bars
	if {$showBars} {

		# Border around each bar
		set x [expr {$barLeft - 1}]
		set y1 [expr {$padY + $fontHeight / 2 - $barHeight / 2}]
		if {$showNumbers} {
			incr y1 $fontHeight
		}
		set y2 [expr {$y1 + $barHeight}]
		foreach attrib {hitpoints mana food weight} title {HP SP FD WT} {
			if {!$show($title)} continue
			$canvas coords border1-$attrib $x $y2 $x $y1 $barRight $y1
			$canvas coords border2-$attrib $barRight $y1 $barRight $y2 $x $y2
			incr y1 $fontHeight
			incr y2 $fontHeight
			if {$showNumbers} {
				incr y1 $fontHeight
				incr y2 $fontHeight
			}
		}

		# Bars
		set y [expr {$padY + $fontHeight / 2 - $barHeight / 2 + 1}]
		if {$showNumbers} {
			incr y $fontHeight
		}
		foreach attrib {hitpoints mana food weight} title {HP SP FD WT} {
			set progId [Info $oop progId,$attrib]
			set frame [NSProgress::Info $progId frame]
			if {!$show($title)} {
				place forget $frame
				continue
			}
			place $frame \
				-x $barLeft -y $y -anchor nw
			$frame configure -width $barWidth
			incr y $fontHeight
			if {$showNumbers} {
				incr y $fontHeight
			}
		}

	# Don't show bars
	} else {
		$canvas move border $width 0
		foreach bar [winfo children $canvas] {
			place forget $bar
		}
	}

	return
}

# NSProgressCanvas::Arrange --
#
#	Arranges stuff in the Progress Window depending on the current
#	configuration options.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressCanvas::Arrange {oop} {

	set layout [Info $oop layout]

	switch -- $layout {
		wide { ArrangeWide $oop }
		tall { ArrangeTall $oop }
	}

	Set $oop

	return
}

# NSProgressCanvas::NeededWidth --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressCanvas::NeededWidth {oop} {

	set layout [Info $oop layout]

	# Get the desired font
	set font [Value font,misc]

	set showBars [Value progress,showBars]
	set showNumbers [Value progress,showNumbers]
	set showMax [Value progress,showMax]

	switch -- $layout {

		wide {
		}

		tall {
			# Calculate the width of the titles
			set titlePadX 2
			set titleWidth 0
			foreach title {HP SP FD WT} {
				if {![Value progress,show$title]} continue
				set twidth [font measure $font [mc $title]]
				if {$twidth > $titleWidth} {
					set titleWidth $twidth
				}
			}

			# Calculate width of numbers
			set numberWidth 0
			if {$showNumbers} {
				incr numberWidth 8 ; # pad
				incr numberWidth [font measure $font 99999]
				if {$showMax} {
					incr numberWidth [font measure $font /]
					set numberWidthOfMax [font measure $font 99999]
					incr numberWidth $numberWidthOfMax
				}
			}

			return [expr {$titlePadX + $titleWidth + $numberWidth + $titlePadX}]
		}
	}

	return
}

# NSProgressCanvas::Set --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressCanvas::Set {oop} {

	foreach attrib {hitpoints mana food} {
		Update_HP_SP_FD $oop $attrib {*}[angband player $attrib]
	}

	Update_WT $oop

	return
}

# NSProgressCanvas::ContextMenu --
#
#	Pop up a context menu in the Progress Window to configure it's
# 	appearance.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressCanvas::ContextMenu {oop menu x y} {

	$menu delete 0 end
	destroy {*}[winfo children $menu]

	set showBars [Value progress,showBars]
	set showNumbers [Value progress,showNumbers]
	set showMax [Value progress,showMax]

	if {[Value progress,float]} {
		if {[Value misc,float]} {
			set label [mc "Attach To Misc"]
		} else {
			set label [mc "Attach To Main"]
		}
		set value 0
	} else {
		set label [mc Float]
		set value 1
	}
	$menu add command -label $label \
		-command "NSMainWindow::Info [Global main,oop] progress,float $value
		NSMainWindow::MenuInvoke [Global main,oop] ignore E_WINDOW_PROGRESS"

	set state normal
	if {$showBars} {
		set label [mc "Hide Bars"]
		if {!$showNumbers} {
			set state disabled
		}
	} else {
		set label [mc "Show Bars"]
	}
	$menu add command -label $label -state $state \
		-command "NSProgressCanvas::Toggle $oop showBars"

	set state normal
	if {$showNumbers} {
		set label [mc "Hide Numbers"]
		if {!$showBars} {
			set state disabled
		}
	} else {
		set label [mc "Show Numbers"]
	}
	$menu add command -label $label -state $state \
		-command "NSProgressCanvas::Toggle $oop showNumbers"

	set state normal
	if {$showMax} {
		set label [mc "Hide Max"]
	} else {
		set label [mc "Show Max"]
	}
	if {!$showNumbers} {
		set state disabled
	}
	$menu add command -label $label -state $state \
		-command "NSProgressCanvas::Toggle $oop showMax"

	if {[winfo ismapped [Window progress]]} {
		if {[Info $oop layout] eq "wide"} {
			set label [mc "Tall Layout"]
			set value tall
		} else {
			set label [mc "Wide Layout"]
			set value wide
		}
		$menu add command -label $label \
			-command "NSProgressCanvas::Info $oop layout $value
			Value progress,layout $value
			NSProgressCanvas::Arrange $oop
			NSProgressCanvas::Set $oop"
	}

	set mShow [menu $menu.mShow -tearoff 0]
	foreach what {HP SP FD WT} {
		Info $oop show$what [Value progress,show$what]
		$mShow add checkbutton -label [mc show$what] \
			-variable ::NSProgressCanvas($oop,show$what) \
			-command "NSProgressCanvas::Toggle $oop show$what"
	}
	$menu add cascade -label [mc "Show"] -menu $mShow

	$menu add separator
	$menu add command -label [mc Cancel]

	tk_popup $menu $x $y

	return
}

# NSProgressCanvas::Toggle --
#
#	Hide or show the bars or numbers in the Progress Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressCanvas::Toggle {oop which} {

	set showIt [Value progress,$which]
	Value progress,$which [expr {!$showIt}]
if 1 {
	# Yuck! Make sure the Misc canvas gets updated in case this canvas
	# changes size.
	qegenerate <Value-font,misc>
} else {
	Arrange $oop
}

	return
}

proc NSProgressCanvas::CanvasFeedbackCmd {oop info action} {

	if {$action eq "leave"} {
		NSMainWindow::StatusText [Global main,oop] {}
		return
	}

	switch -- $info {
		food -
		hitpoints -
		mana {
			set desc(food) "food level"
			set desc(hitpoints) "hit points"
			set desc(mana) "spell points"
			set message [format [mc "Your character's %s"] [mc $desc($info)]]
			set showNumbers [Value progress,showNumbers]
			set showMax [Value progress,showMax]
			set showBars [Value progress,showBars]
			scan [angband player $info] "%d %d %f" cur max frac
			if {!$showNumbers} {
				append message " ($cur/$max)"
			} elseif {$showBars && !$showMax} {
				if {$cur < $max} {
					append message [format [mc " (max %s)"] $max]
				}
			}
			if {$info eq "mana"} {
				set extra ""
				scan [angband player mana_cumber] "%d %d" glove armor
				if {$glove > 0} {
					append extra [format [mc "-%d gloves"] $glove]
				}
				if {$armor > 0} {
					if {$extra ne ""} {
						append extra ", "
					}
					append extra [format [mc "-%d armor"] $armor]
				}
				if {$extra ne ""} {
					append message " \[" $extra "\]"
				}
			}
			append message [mc ". Right-Click for options."]
		}
		weight {
			set message [format [mc "Your character's %s"] [mc "weight"]]
			set showNumbers [Value progress,showNumbers]
			set showBars [Value progress,showBars]
			set cur [angband player total_weight]
			set max [angband inventory weight_limit]
			set capacity [expr {$max / 2 + $max / 10}]
			set cur [fmt_wgt $cur]
			set capacity [fmt_wgt $capacity]
			set max [fmt_wgt $max 1]
			append message " ($cur/$capacity/$max)"
			append message [mc ". Right-Click for options."]
		}
	}

	NSMainWindow::StatusText [Global main,oop] $message

	return
}

# NSProgressCanvas::Configure --
#
#	Called when the canvas is resized
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressCanvas::Configure {oop width height} {

	[Info $oop canvas] configure -width $width

	Arrange $oop

	return
}
