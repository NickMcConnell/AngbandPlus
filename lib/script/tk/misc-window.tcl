# File: misc-window.tcl

# Purpose: Message, Misc and Progress Windows

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSMiscWindow {

	variable Priv

# namespace eval NSMiscWindow
}

# NSMiscWindow::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::InitModule {} {

	NSModule::LoadIfNeeded NSBalloon

	InitAuxWindows

	return
}

# NSMiscWindow::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::Info {info args} {

	variable Priv

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set Priv($info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $Priv($info)
			}
		}
	}

	return
}

# NSMiscWindow::InitAuxWindows --
#
#	Initialize other windows.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::InitAuxWindows {} {

	# Get the Main Window
	set winMain [Window main]

	# Big screens get the Tool Window
	if {0 && [winfo screenwidth .] >= 800} {
	
		#
		# Toolbar
		#
	
		set win .tools
		toplevel $win
		wm title $win "Tool"
		wm transient $win $winMain
		wm resizable $win no no
		Term_KeyPress_Bind $win
	
		# Start out withdrawn (hidden)
		wm withdraw $win
	
		Window tool $win

		set child [InitDisplay_Toolbar $win]
		pack $child \
			-side top -expand yes -fill x
	}

	#
	# Message line
	#

	set win .message
	toplevel $win
	wm title $win "Message"
	wm transient $win $winMain
	wm resizable $win yes no
	wm protocol $win WM_DELETE_WINDOW {
		NSMainWindow::Info [Global main,oop] messageWindow 0
		NSMainWindow::MenuInvoke [Global main,oop] ignore E_WINDOW_MESSAGE
	}
	Term_KeyPress_Bind $win

	# Start out withdrawn (hidden)
	wm withdraw $win

	Window message $win

set child [InitDisplay_Message [Window main].message]
pack $child -side top -expand yes -fill x
	set child [InitDisplay_Message $win]
	pack $child \
		-side top -expand yes -fill x

	#
	# Misc info window
	#

	set win .misc
	toplevel $win
	wm title $win "Misc"
	wm transient $win [Window main]
	wm resizable $win no no
	wm protocol $win WM_DELETE_WINDOW {
		NSMainWindow::Info [Global main,oop] miscWindow 0
		NSMainWindow::MenuInvoke [Global main,oop] ignore E_WINDOW_MISC
	}
	Term_KeyPress_Bind $win

	# Start out withdrawn (hidden)
	wm withdraw $win

	Window misc $win

set child [InitDisplay_Misc [Window main].misc]
pack $child -expand yes -fill y
	set child [InitDisplay_Misc $win]
	pack $child \
		-expand no
	

	#
	# Progress window
	#

	set win .progress
	toplevel $win
	wm title $win "Progress"
	wm transient $win $winMain
	wm resizable $win no no
	wm protocol $win WM_DELETE_WINDOW "NSMiscWindow::HideProgressWindow"
	Term_KeyPress_Bind $win

	# Start out withdrawn (hidden)
	wm withdraw $win

	Window progress $win

	set child [InitDisplay_Progress $win]
	pack $child \
		-expand no

	return
}

proc NSMiscWindow::InitDisplay_Toolbar {parent} {

	set canvas $parent.tools
	canvas $canvas -width [expr {(16 + 4) * 9}] -height 22 \
		-scrollregion {0 0 0 0} -highlightthickness 0 -background Black

	Global misc,toolbar $canvas

	NSModule::LoadIfNeeded NSMiscPopup

	return $canvas
}

proc NSMiscWindow::InitDisplay_Message {parent} {

	set font [Value font,message]

	set text $parent.message
	text $text \
		-width 80 -height 1 -font $font -background Black \
		-foreground White -cursor {} -borderwidth 0 -highlightthickness 0
	bindtags $text [list $text $parent all]

	# When the Message Window is clicked:
	#	- see the previous message, or
	#	- toggle the display of inventory, spells, etc, or
	#	- bypass the -more- prompt
	bind $text <ButtonPress-1> {
		switch -- [angband inkey_flags] {
			INKEY_CMD {
				DoUnderlyingCommand ^o
			}
			INKEY_ITEM -
			INKEY_SPELL {
				angband keypress *
			}
			INKEY_MORE {
				angband keypress " "
			}
		}
		switch -- [angband inkey_flags] {
			INKEY_MINDCRAFT -
			INKEY_POWER {
				angband keypress *
			}
		}
	}	
	bind $text <Double-ButtonPress-1> {
		DoUnderlyingCommand ^p
	}	

	NSStatusText::StatusText $text \
		[Global main,statusBar] \
		"Click for last message. Double-click for message history."

	# Update ourself when the font for the Message Window changes
	NSValueManager::AddClient font,message \
		NSMiscWindow::ValueChanged_font_message

	if {0 && [llength [info commands tk_chooseFont]]} {
		bind $text <ButtonPress-3> {
			SetMessageFont [tk_chooseFont -parent %W -style]
		}
	}

	pack $text \
		-side top -expand yes -fill x

	Global message,message $text

	# Support for colored messages
	foreach attr [Global term_attr] {
		$text tag configure $attr -foreground [Value $attr]
	}

	# Pop up a menu when the window is clicked
	set menu $parent.popup
	menu $menu -tearoff 0
	bind $text <ButtonPress-3> \
		"NSMiscWindow::ContextMenu_Message $menu %X %Y"

	return $text
}

# NSMiscWindow::ContextMenu_Message --
#
#	Pop up a context menu in the Message Window to configure it's
# 	appearance.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::ContextMenu_Message {menu x y} {

	$menu delete 0 end

	$menu add command -label "Set Font" \
		-command "NSModule::LoadIfNeeded NSFont ; NSWindowManager::Display font message"
	if {[winfo ismapped [Window message]]} {
		set label "Attach To Main"
		set value 0
	} else {
		set label Float
		set value 1
	}
	$menu add command -label $label \
		-command "
		NSMainWindow::Info [Global main,oop] messageWindow $value
		NSMainWindow::MenuInvoke [Global main,oop] ignore E_WINDOW_MESSAGE
		"
	$menu add separator
	$menu add command -label "Cancel"

	tk_popup $menu $x $y

	return
}


proc NSMiscWindow::InitDisplay_Misc {parent} {

	#
	# Font
	#

	set font [Value font,misc]
	set fontHeight [font metrics $font -linespace]

	#
	# Pics
	#

	InitImageIfNeeded Image_MiscAC ac.gif
	InitImageIfNeeded Image_MiscAU au.gif
	InitImageIfNeeded Image_MiscCHR cha.gif
	InitImageIfNeeded Image_MiscCON const.gif
	InitImageIfNeeded Image_MiscDEX dex.gif
	InitImageIfNeeded Image_MiscEXP exp.gif
	InitImageIfNeeded Image_MiscINT int.gif
	InitImageIfNeeded Image_MiscLEVEL level.gif
	InitImageIfNeeded Image_MiscLOOK look.gif
	InitImageIfNeeded Image_MiscSTR str.gif
	InitImageIfNeeded Image_MiscWIS wis.gif

	#
	# Main canvas
	#

	set c $parent.misc
	canvas $c \
		-scrollregion "0 0 100 100" -width 100 -height 100 \
		-relief flat -highlightthickness 0 -background Black

	# Display help string in Main Window status bar
	CanvasFeedbackInit $c

if 0 {

	# Character image (center at 22,22)
	$c create widget 0 0 -anchor center -tags icon

	# Click the image to see character info
	$c bind icon <Enter> {
		%W itemconfigure focus -outline gray60
	}
	$c bind icon <Leave> {
		%W itemconfigure focus -outline Black
	}

	$c bind icon <ButtonPress-1> "
		$c move icon 1 1
		set CanvasButtonDown 1
	"
	$c bind icon <Button1-Enter> "
		$c move icon 1 1
		set CanvasButtonDown 1
	"
	$c bind icon <Button1-Leave> "
		$c move icon -1 -1
		set CanvasButtonDown 0
	"
	$c bind icon <ButtonRelease-1> "
		if {\$CanvasButtonDown} {
			$c move icon -1 -1
			update idletasks
			DoCommandIfAllowed C
		}
	"

	CanvasFeedbackAdd $c icon "NSMiscWindow::CanvasFeedbackCmd_MiscWindow icon"
}
	
	# Focus ring around character image (since user can click it)
	$c create rectangle 0 0 1 1 -outline Black -tags focus

	# Character name
	$c create text 0 0 -font $font -justify left \
		-anchor w -fill [Value TERM_L_BLUE] -tags {font name}
	CanvasFeedbackAdd $c name "NSMiscWindow::CanvasFeedbackCmd_MiscWindow name"

	# Click name to change it
	$c bind name <Enter> {
		%W itemconfigure name -fill gray60
	}
	$c bind name <Leave> {
		%W itemconfigure name -fill [Value TERM_L_BLUE]
	}

	# Title
	$c create text 0 0 -font $font -justify left \
		-anchor w -fill [Value TERM_L_BLUE] -tags {font title}
	CanvasFeedbackAdd $c title "NSMiscWindow::CanvasFeedbackCmd_MiscWindow title"
if 0 {
	# Stat titles
	foreach title {STR INT WIS DEX CON CHR} stat [angband info stat_name] {
		$c create image 0 0 -image Image_Misc$title -anchor nw \
			-tags "$title pic,$title"
		$c create text 0 0 -text $title -font $font \
			-justify left -anchor nw -fill White \
			-tags "font $title txt,$title"
		CanvasFeedbackAdd $c $title \
			"NSMiscWindow::CanvasFeedbackCmd_MiscWindow $stat"
	}

	# Stats
	foreach stat [angband info stat_name] {
		$c create text 0 0 -font $font -justify right \
			-anchor ne -fill [Value TERM_L_GREEN] -tags "font $stat"
		CanvasFeedbackAdd $c $stat \
			"NSMiscWindow::CanvasFeedbackCmd_MiscWindow $stat"
	}
}

	# Race and Class
	foreach info {race class} {
		$c create text 0 0 -font $font -justify left \
			-anchor nw -fill [Value TERM_L_BLUE] -tags "font $info"
		CanvasFeedbackAdd $c $info \
			"NSMiscWindow::CanvasFeedbackCmd_MiscWindow $info"
	}

	# Level, Experience, Gold and Armor Class titles
	foreach title {LEVEL EXP AU AC} {
		$c create image 0 0 -image Image_Misc$title -anchor nw \
			-tags "$title pic,$title"
		$c create text 0 0 -text $title -font $font \
			-justify left -anchor nw -fill White \
			-tags "font $title txt,$title"
		CanvasFeedbackAdd $c $title \
			"NSMiscWindow::CanvasFeedbackCmd_MiscWindow $title"
	}

	# Level, Experience, Gold and Armor Class
	foreach info {level exp gold armor_class} title {LEVEL EXP AU AC} {
		$c create text 0 0 -font $font -justify right \
			-anchor ne -fill [Value TERM_L_GREEN] -tags "font $info"
		if {[string equal $title EXP]} {set title exp}
		if {[string equal $title AC]} {set title ac}
		CanvasFeedbackAdd $c $info \
			"NSMiscWindow::CanvasFeedbackCmd_MiscWindow $title"
	}
	
	foreach title {HP SP FD} option {hitpoints mana food} {
		$c create text 0 0 -text $title -font $font \
			-fill White -anchor nw -tags "font $title"
		$c create text 0 0 -text 99999 -font $font \
			-fill [Value TERM_L_GREEN] -anchor ne -tags "font $option"
		CanvasFeedbackAdd $c $title \
			"NSMiscWindow::CanvasFeedbackCmd_MiscWindow $option"
		CanvasFeedbackAdd $c $option \
			"NSMiscWindow::CanvasFeedbackCmd_MiscWindow $option"
	}

	# Global access
	Global misc,canvas $c

	# Set some initial values.
	MiscSet

	# When the mouse enters the "EXP" text, it toggles to ADV and shows
	# the points needed to reach the next level.
	$c bind EXP <Enter> {
		%W itemconfigure txt,EXP -fill gray60
	}
	$c bind EXP <Leave> {
		%W itemconfigure txt,EXP -fill White
	}
	$c bind EXP <ButtonPress-1> {
		Value misc,mode,exp [expr {![Value misc,mode,exp]}]
		namespace eval NSMiscWindow {
			if {[Value misc,mode,exp]} {
				%W itemconfigure txt,EXP -text "EXP"
			} else {
				%W itemconfigure txt,EXP -text "ADV"
			}
			eval bind_Py_exp [angband player exp]
			CanvasFeedbackCmd_MiscWindow EXP enter
		}
	}

	$c bind AC <Enter> {
		%W itemconfigure txt,AC -fill gray60
	}
	$c bind AC <Leave> {
		%W itemconfigure txt,AC -fill White
	}
	$c bind AC <ButtonPress-1> {
		Value misc,mode,armor_class [expr {![Value misc,mode,armor_class]}]
		namespace eval NSMiscWindow {
			eval bind_Py_ac [angband player armor_class]
			CanvasFeedbackCmd_MiscWindow AC enter
		}
	}
	
if {$parent == [Window misc]} {
	$c create line 0 0 1 1 -fill #333366 -tags divider
	set tools [InitDisplay_Toolbar $c]
	pack $tools -side top -anchor n
	pack propagate $c no
}

	# Arrange all the items
	ValueChanged_font_misc

	# Update ourself when the font for the Misc Window changes
	NSValueManager::AddClient font,misc \
		NSMiscWindow::ValueChanged_font_misc

	# Pop up a menu when the window is clicked
	set menu $parent.popup
	menu $menu -tearoff 0
	bind $c <ButtonPress-3> \
		"NSMiscWindow::ContextMenu_Misc $menu %X %Y"

	return $c
}

# NSMiscWindow::MiscSet --
#
#	Set text of Misc Window items.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::MiscSet {} {

	set canvas [Global misc,canvas]

	if {[Value misc,mode,exp]} {
		$canvas itemconfigure txt,EXP -text "EXP"
	} else {
		$canvas itemconfigure txt,EXP -text "ADV"
	}

	eval bind_Py_exp [angband player exp]

	eval bind_Py_ac [angband player armor_class]

if 0 {
	foreach stat [angband info stat_name] {
		bind_Stat $stat
	}

	foreach tag [list gold level name race class title] {
		bind_Py_value $tag [angband player $tag]
	}
}
	foreach tag {hitpoints mana food} {
		scan [angband player $tag] "%d %d" cur max
		UpdateHP_SP_FD $tag $cur $max
	}

	return
}

# NSMiscWindow::ContextMenu_Misc --
#
#	Pop up a context menu in the Misc Window to configure it's
# 	appearance.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::ContextMenu_Misc {menu x y} {

	$menu delete 0 end

	$menu add command -label "Set Font" \
		-command "NSModule::LoadIfNeeded NSFont ; NSWindowManager::Display font misc"
	if {[winfo ismapped [Window misc]]} {
		set label "Attach To Main"
		set value 0
	} else {
		set label Float
		set value 1
	}
	$menu add command -label $label \
		-command "
		NSMainWindow::Info [Global main,oop] miscWindow $value
		NSMainWindow::MenuInvoke [Global main,oop] ignore E_WINDOW_MISC
		"

	if {[Value misc,textLabels]} {
		set label "Picture Labels"
		set value 0
	} else {
		set label "Text Labels"
		set value 1
	}
	$menu add command -label $label \
		-command "Value misc,textLabels $value ;
			NSMiscWindow::ValueChanged_font_misc"
	if {[winfo ismapped [Window misc]]} {
		if {[Value misc,layout] == "wide"} {
			set label "Tall Layout"
			set value tall
		} else {
			set label "Wide Layout"
			set value wide
		}
		$menu add command -label $label \
			-command "
				Value misc,layout $value
				NSMiscWindow::ValueChanged_font_misc
			"
	}
	$menu add separator
	$menu add command -label "Cancel"

	tk_popup $menu $x $y

	return
}

proc NSMiscWindow::UpdateHP_SP_FD {which cur max} {

	set canvas [Global misc,canvas]

	if {[string compare $which food]} {

		if {$cur >= $max} {
			set fill [Value TERM_L_GREEN]
		} elseif {$cur > ($max * 5) / 10} {
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

	if {[Value misc,layout] == "tall"} {
		$canvas itemconfigure $which -fill $fill -text $cur
		return
	}

	# Creation
	if {![info exists ::Global(progress,canvas)]} return

	set canvas [Global progress,canvas]

	if {[Value progress,showNumbers]} {
		if {[Value progress,showBars]} {
			$canvas itemconfigure $which -fill $fill -text $cur
		} else {
			$canvas itemconfigure $which -fill $fill -text $cur/$max
		}
	}
if 0 {
	if {[Value progress,showBars]} {
		NSProgress::SetDoneRatio [NSMiscWindow::Info prog$which] $frac
	}
}
	return
}

proc NSMiscWindow::InitDisplay_Progress {parent} {

	global NSProgress

	# I want this window as wide as the Misc Window
	set width [winfo reqwidth [Window misc].misc]

	set font [Global font,fixed,small]
	set fontWidth [font measure $font "W"]

	# Space at ends of each bar
	set barSpace 8

	# Width of each bar
	set progWidth [expr {$width - $fontWidth * 8 - $barSpace * 2 - 2}]

	set barLeft [expr {$width - $barSpace - $progWidth}]
	set barRight [expr {$width - $barSpace}]

	set c $parent.progress
	canvas $c \
		-scrollregion "0 0 $width 60" \
		-width $width -height 60 \
		-relief flat -highlightthickness 0 -background Black

	# Display help string in Main Window status bar
	CanvasFeedbackInit $c

	# Border around first bar
	set x [expr {$barLeft - 1}]
	set y1 12
	set y2 19
	$c create line $x $y2 $x $y1 $barRight $y1 -fill #666699 \
		-tags {borderHP1 border}
	$c create line $barRight $y1 $barRight $y2 $x $y2 -fill #333366 \
		-tags {borderHP2 border}

	# Border around second bar
	incr y1 15
	incr y2 15
	$c create line $x $y2 $x $y1 $barRight $y1 -fill #666699 \
		-tags {borderSP1 border}
	$c create line $barRight $y1 $barRight $y2 $x $y2 -fill #333366 \
		-tags {borderSP2 border}

	# Border around third bar
	incr y1 15
	incr y2 15
	$c create line $x $y2 $x $y1 $barRight $y1 -fill #666699 \
		-tags {borderFD1 border}
	$c create line $barRight $y1 $barRight $y2 $x $y2 -fill #333366 \
		-tags {borderFD2 border}

	# Bar titles
	set y 7
	foreach title {HP SP FD} option {hitpoints mana food} {
		$c create text 2 [incr y 15] -text $title -font $font \
			-fill White -anchor sw -tags "font $title"
		CanvasFeedbackAdd $c $title \
			"NSMiscWindow::CanvasFeedbackCmd_Progress $option"
	}

	# Hit Points
	set y 13
	set progHP [NSObject::New NSProgress $c $progWidth 6 "Red" gray60]
	place $NSProgress($progHP,frame) \
		-x $barLeft -y $y -anchor nw
	Info proghitpoints $progHP

	NSStatusText::StatusCommand $NSProgress($progHP,frame) \
		[Global main,statusBar] \
		{NSMiscWindow::CanvasFeedbackCmd_Progress hitpoints enter}

	set x [expr {$barLeft - $barSpace}]
	set y2 7
	$c create text $x [incr y2 15] -text 99999 -font $font \
		-fill [Value TERM_L_GREEN] -anchor se -tags {font hitpoints}
	CanvasFeedbackAdd $c hitpoints \
		"NSMiscWindow::CanvasFeedbackCmd_Progress hitpoints"

	# Spell Points
	set progSP [NSObject::New NSProgress $c $progWidth 6 "Blue" gray60]
	place $NSProgress($progSP,frame) \
		-x $barLeft -y [incr y 15] -anchor nw
	Info progmana $progSP

	NSStatusText::StatusCommand $NSProgress($progSP,frame) \
		[Global main,statusBar] \
		{NSMiscWindow::CanvasFeedbackCmd_Progress mana enter}

	$c create text $x [incr y2 15] -text 99999 -font $font \
		-fill [Value TERM_L_GREEN] -anchor se -tags {font mana}
	CanvasFeedbackAdd $c mana \
		"NSMiscWindow::CanvasFeedbackCmd_Progress mana"

	# Food
	set progFD [NSObject::New NSProgress $c $progWidth 6 "Brown" gray60]
	place $NSProgress($progFD,frame) \
		-x $barLeft -y [incr y 15] -anchor nw
	Info progfood $progFD

	NSStatusText::StatusCommand $NSProgress($progFD,frame) \
		[Global main,statusBar] \
		{NSMiscWindow::CanvasFeedbackCmd_Progress food enter}

	$c create text $x [incr y2 15] -text 99999 -font $font \
		-fill [Value TERM_L_GREEN] -anchor se -tags {font food}
	CanvasFeedbackAdd $c food \
		"NSMiscWindow::CanvasFeedbackCmd_Progress food"

	#
	# Context Menu
	#
	
	set menu $parent.context
	menu $menu -tearoff 0
	bind $parent <ButtonPress-3> \
		"NSMiscWindow::ContextMenu_Progress $menu %X %Y"

	# Global access
	Global progress,canvas $c

	# Update ourself when the font for the Misc Window changes
#	NSValueManager::AddClient font,misc \
#		NSMiscWindow::ConfigProgress

	# Hack -- Synch with options
	ConfigProgress

	return $c
}

proc NSMiscWindow::ShowProgressWindow {} {

	wm deiconify [Window progress]

	set canvas [Global progress,canvas]

	return
}

proc NSMiscWindow::HideProgressWindow {} {

####
bell
return
####

	wm withdraw [Window progress]

	set canvas [Global progress,canvas]

	return
}

# NSMiscWindow::ConfigProgress --
#
#	Arranges stuff in the Progress Window depending on the current
#	configuration options.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::ConfigProgress {} {

	global NSProgress

	# Get the Progress Window canvas
	set canvas [Global progress,canvas]

	set showBars [Value progress,showBars]
	set showNumbers [Value progress,showNumbers]

	# Get the desired font
	set font [Value font,misc]

	# Calculate the height of a line
	set fontHeight [font metrics $font -linespace]

	# Set the font of items with the "font" tag
	$canvas itemconfigure font -font $font

	# Get the width of the Misc Window canvas
	set width [winfo reqwidth [Global misc,canvas]]

	# Keep the Progress Window as wide as the Misc Window
	$canvas configure -width $width

	# Set the height of the canvas
	$canvas configure -height [expr {$fontHeight * 3 + 8}]

	# Calculate the width of the text
	set textWidth 0
	foreach label [list HP SP FD] {
		set twidth [font measure $font $label]
		if {$twidth > $textWidth} {
			set textWidth $twidth
		}
	}
	if {$showNumbers} {
		incr textWidth 8 ; # pad
		incr textWidth [font measure $font 99999]
	}
	if {!$showBars} {
		incr textWidth [font measure $font /99999]
	}
	incr textWidth 2 ; # left pad

	# Space at ends of each bar
	set barSpace 8

	# Get the height of each progress bar
	set barHeight [winfo reqheight $NSProgress([Info progfood],frame)]
	
	# Width of each bar. This is the width of the window minus the space
	# at each end of a bar, minus the bar title "HP " etc, minus the
	# numbers "12345" if numbers are showing.
	set barWidth [expr {$width - $textWidth - $barSpace * 2}]

	set barLeft [expr {$width - $barSpace - $barWidth}]
	set barRight [expr {$width - $barSpace}]

	set y2 4
	foreach title {HP SP FD} {
		$canvas coords $title 2 [incr y2 $fontHeight]
	}

	# Option: Display numbers
	if {$showNumbers} {
		set x [expr {$barLeft - $barSpace}]
		set y2 4
		$canvas coords hitpoints $x [incr y2 $fontHeight]

		set x [expr {$barLeft - $barSpace}]
		$canvas coords mana $x [incr y2 $fontHeight]

		set x [expr {$barLeft - $barSpace}]
		$canvas coords food $x [incr y2 $fontHeight]

	# Don't show numbers
	} else {
		$canvas move hitpoints $width 0
		$canvas move mana $width 0
		$canvas move food $width 0
	}

	# Option: Show bars
	if {$showBars} {

		# Border around first bar
		set x [expr {$barLeft - 1}]
		set y1 [expr {4 + $fontHeight / 2 - $barHeight / 2}]
		set y2 [expr {$y1 + $barHeight}]
		$canvas coords borderHP1 $x $y2 $x $y1 $barRight $y1
		$canvas coords borderHP2 $barRight $y1 $barRight $y2 $x $y2
	
		# Border around second bar
		incr y1 $fontHeight
		incr y2 $fontHeight
		$canvas coords borderSP1 $x $y2 $x $y1 $barRight $y1
		$canvas coords borderSP2 $barRight $y1 $barRight $y2 $x $y2
	
		# Border around third bar
		incr y1 $fontHeight
		incr y2 $fontHeight
		$canvas coords borderFD1 $x $y2 $x $y1 $barRight $y1
		$canvas coords borderFD2 $barRight $y1 $barRight $y2 $x $y2
	
		# Hit Points
		set y [expr {4 + $fontHeight / 2 - $barHeight / 2 + 1}]
		set progHP [Info proghitpoints]
		place $NSProgress($progHP,frame) \
			-x $barLeft -y $y -anchor nw
		$NSProgress($progHP,frame) configure -width $barWidth
		
		# Spell Points
		set progSP [Info progmana]
		place $NSProgress($progSP,frame) \
			-x $barLeft -y [incr y $fontHeight] -anchor nw
		$NSProgress($progSP,frame) configure -width $barWidth
	
		# Food
		set progFD [Info progfood]
		place $NSProgress($progFD,frame) \
			-x $barLeft -y [incr y $fontHeight] -anchor nw
		$NSProgress($progFD,frame) configure -width $barWidth

	# Don't show bars
	} else {
		$canvas move border $width 0
		foreach bar [winfo children $canvas] {
			place forget $bar
		}
	}

	# Cancel geometry
	wm geometry [Window progress] ""

	return
}

# NSMiscWindow::ContextMenu_Progress --
#
#	Pop up a context menu in the Progress Window to configure it's
# 	appearance.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::ContextMenu_Progress {menu x y} {

	$menu delete 0 end

	set showBars [Value progress,showBars]
	set showNumbers [Value progress,showNumbers]

	set state normal
	set action Show
	if {$showBars} {
		set action Hide
		if {!$showNumbers} {
			set state disabled
		}
	}
	$menu add command -label "$action Bars" -state $state \
		-command "NSMiscWindow::ToggleProgress showBars"

	set state normal
	set action Show
	if {$showNumbers} {
		set action Hide
		if {!$showBars} {
			set state disabled
		}
	}
	$menu add command -label "$action Numbers" -state $state \
		-command "NSMiscWindow::ToggleProgress showNumbers"
	$menu add separator
	$menu add command -label "Cancel"

	tk_popup $menu $x $y

	return
}

# NSMiscWindow::ToggleProgress --
#
#	Hide or show the bars or numbers in the Progress Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::ToggleProgress {which} {

	set showIt [Value progress,$which]
	Value progress,$which [expr {!$showIt}]

	ConfigProgress

	foreach option {hitpoints mana food} {
		eval Update_HP_SP_FD hitpoints [angband player $option]
	}

	return
}


# NSMiscWindow::CanvasAddTextItem --
#
#	Create a new canvas text item at the given "row" and "column".
#	Item positioning is determined by the size of the given font.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::CanvasAddTextItem {canvas font col row width fill text justify tags} {

	set fontWidth [font measure $font "W"]
	set fontHeight [font metrics $font -linespace]

	if {[string equal $justify right]} {
		incr col $width
		set anchor ne
	} else {
		set anchor nw
	}
	set x [expr {$col * $fontWidth}]
	set y [expr {$row * $fontHeight}]
	set width [expr {$width * $fontWidth}]
	return [$canvas create text $x $y -fill $fill -text $text \
		-font $font -justify $justify \
		-anchor $anchor -tags $tags]
}

# NSMiscWindow::bind_Stat --
#
#	Called in response to a <Stat> event. Updates the Misc Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::bind_Stat {stat} {

if 0 {

	set canvas [Global misc,canvas]

	angband player stat $stat statInfo
	set max $statInfo(use)
	set top $statInfo(top)
	if {$max < $top} {
		set fill [Value TERM_YELLOW]
	} else {
		set fill [Value TERM_L_GREEN]
	}
	set value [cnv_stat_disp $max]
	set text [$canvas itemcget $stat -text]
	set fillCur [FlashCanvasTextFill $canvas $stat]
	if {[string compare $text $value] || [string compare $fill $fillCur]} {
		$canvas itemconfigure $stat -text $value
		FlashCanvasText $canvas $stat White $fill 4
	}

}

	return
}

# NSMiscWindow::bind_Py --
#
#	Called in response to a <Py-armor_class> event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::bind_Py_ac {dis_ac dis_to_a} {

	set canvas [Global misc,canvas]

	if {[Value misc,mode,armor_class]} {
		set ac [expr {$dis_ac + $dis_to_a}]
	} else {
		set ac [format "%d,%+d" $dis_ac $dis_to_a]
	}
	set fill [Value TERM_L_GREEN]
	set text [$canvas itemcget armor_class -text]
	if {[string compare $text $ac]} {
		$canvas itemconfigure armor_class -text $ac
		FlashCanvasText $canvas armor_class White $fill 4
	}

	return
}

# NSMiscWindow::bind_Py_exp --
#
#	Called in response to a <Py-exp> event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::bind_Py_exp {cur max adv} {

	set canvas [Global misc,canvas]

	if {$cur < $max} {
		set fill [Value TERM_YELLOW]
	} else {
		set fill [Value TERM_L_GREEN]
	}
	if {![Value misc,mode,exp]} {
		if {$adv == 999999999} {
			set cur *****
		} else {
			set cur [expr {$adv - $cur}]
		}
	}
	set text [$canvas itemcget exp -text]
	set fillCur [FlashCanvasTextFill $canvas exp]
	if {[string compare $text $cur] || [string compare $fill $fillCur]} {
		$canvas itemconfigure exp -text $cur
		FlashCanvasText $canvas exp White $fill 4
	}

	return
}

# NSMiscWindow::bind_Py_gold --
#
#	Called in response to a <Py-> event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::bind_Py_value {tag value} {

	set canvas [Global misc,canvas]

	set text [$canvas itemcget $tag -text]
	if {[string compare $text $value]} {
		set fill [FlashCanvasTextFill $canvas $tag]
		$canvas itemconfigure $tag -text $value
		FlashCanvasText $canvas $tag White $fill 4
	}

	return
}

# NSMiscWindow::bind_Py_level --
#
#	Called in response to a <Py-level> event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::bind_Py_level {cur max} {

	set canvas [Global misc,canvas]

	if {$cur < $max} {
		set fill [Value TERM_YELLOW]
	} else {
		set fill [Value TERM_L_GREEN]
	}
	set text [$canvas itemcget level -text]
	if {$text != $cur} {
		$canvas itemconfigure level -text $cur
		FlashCanvasText $canvas level White $fill 4
	}

	# Hack -- Display experience as well
	eval bind_Py_exp [angband player exp]

	return
}


# NSMiscWindow::CanvasFeedbackInit --
#
#	Call a command whenever the mouse enters or leaves a canvas item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::CanvasFeedbackInit {canvas} {

	$canvas bind StatusTextCanvasTag <Enter> {
		if {[info exists CanvasFeedback(%W,command,[%W find withtag current])]} {
			eval $CanvasFeedback(%W,command,[%W find withtag current]) enter
		}
	}

	$canvas bind StatusTextCanvasTag <ButtonPress-1> {
		if {[info exists CanvasFeedback(%W,command,[%W find withtag current])]} {
			eval $CanvasFeedback(%W,command,[%W find withtag current]) press1
		}
	}

	$canvas bind StatusTextCanvasTag <Leave> {
		if {[info exists CanvasFeedback(%W,command,[%W find withtag current])]} {
			eval $CanvasFeedback(%W,command,[%W find withtag current]) leave
		}
	}

	return
}

proc NSMiscWindow::CanvasFeedbackAdd {canvas tagOrId command} {

	global CanvasFeedback

	foreach itemId [$canvas find withtag $tagOrId] {
		$canvas addtag StatusTextCanvasTag withtag $itemId
		set CanvasFeedback($canvas,command,$itemId) $command
		set CanvasFeedback($canvas,tag,$itemId) $tagOrId
	}

#####
return
#####

	set itemId [lindex [$canvas find withtag $tagOrId] 0]

	set tagList [$canvas itemcget $itemId -tags]
	lappend tagList StatusTextCanvasTag
	$canvas itemconfigure $itemId -tags $tagList

	set CanvasFeedback($canvas,command,$itemId) $command
	set CanvasFeedback($canvas,tag,$itemId) $tagOrId

	return
}

proc NSMiscWindow::CanvasFeedbackCmd_MiscWindow {info action} {

	if {![string equal $action enter]} {
		NSMainWindow::StatusText [Global main,oop] {}
		return
	}

	switch -- $info {
		strength -
		intelligence -
		wisdom -
		dexterity -
		constitution -
		charisma {
			angband player stat $info statInfo
			set max $statInfo(use)
			set top $statInfo(top)
			set message [format "Your character's %s" $info]
			if {$max < $top} {
				append message [format " (max %s)" [cnv_stat_disp $top]]
			}
		}
		icon {
#			set message "Click for a menu of actions."
			set message "Click to display the Character Window."
		}
		class -
		name -
		race -
		title {
			set message [format "Your character's %s" $info]
			if {[string equal $info name]} {
				append message ". Click to change."
			}
		}
		ac -
		AC {
			set message "Your character's armor class "
			if {[Value misc,mode,armor_class]} {
				append message "(total)"
			} else {
				append message "(base,bonus)"
			}
			if {[string equal $info AC]} {
				append message ". Click to toggle."
			}
		}
		AU {
			set message "Your character's gold"
		}
		exp -
		EXP {
			scan [angband player exp] "%d %d %d" cur max adv
			if {[Value misc,mode,exp]} {
				set message "Current experience points"
				if {$cur < $max} {
					append message [format " (max %s)" $max]
				}
			} else {
				set message "Experience points for next level"
				if {($cur < $max) && ($adv != 999999999)} {
					if {$max < $adv} {
						set max [expr {$adv - $max}]
						append message [format " (min %d)" $max]
					}
				}
			}
			if {[string equal $info EXP]} {
				append message ". Click to toggle."
			}
		}
		LEVEL {
			set message "Your character's experience level"
			set max [angband player max_lev]
			if {[angband player lev] < $max} {
				append message [format " (max %s)" $max]
			}
		}
		food -
		hitpoints -
		mana {
			set desc(food) "food level"
			set desc(hitpoints) "hit points"
			set desc(mana) "spell points"
			set message [format "Your character's %s" $desc($info)]
			scan [angband player $info] "%d %d" cur max
			if {$cur < $max} {
				append message [format " (max %s)" $max]
			}
		}
	}

	NSMainWindow::StatusText [Global main,oop] $message

	return
}

proc NSMiscWindow::CanvasFeedbackCmd_Progress {info action} {

	if {[string equal $action leave]} {
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
			set message [format "Your character's %s" $desc($info)]
			set showNumbers [Value progress,showNumbers]
			set showBars [Value progress,showBars]
			scan [angband player $info] "%d %d" cur max
			if {!$showNumbers} {
				append message " ($cur/$max)"
			} elseif {$showBars} {
				if {$cur < $max} {
					append message [format " (max %s)" $max]
				}
			}
			append message ". Right-Click for options."
		}
	}

	NSMainWindow::StatusText [Global main,oop] $message

	return
}

# NSMiscWindow::MenuSelect_InventoryPopup --
#
#	When a menu entry is highlighted in the Misc Window popup, display
#	the object memory in the Recall Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::MenuSelect_InventoryPopup {menu} {

	variable InventoryPopup

	set invOrEquip $InventoryPopup(invOrEquip)
	set itemList $InventoryPopup(itemList)
	
	# Get the index of the active menu entry
	set index [$menu index active]

	# No entry is selected
	if {[string equal $index none]} {
		set item -1
		
	# An entry is selected
	} else {
		if {$index < [llength $itemList]} {
			set item [lindex $itemList $index]
		} else {
			set item -1
		}
	}
	
	return
}

# NSMiscWindow::InventoryPopup --
#
#	Pop up a menu of equipment/inventory choices.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::InventoryPopup {menu canvas invOrEquip cmdChar args} {

	variable InventoryPopup

	if {[lsearch -exact [angband inkey_flags] INKEY_CMD] == -1} return

	# When an entry is highlighted, show object memory in the Recall Window
	set InventoryPopup(invOrEquip) $invOrEquip
	set InventoryPopup(itemList) {}
	bind $menu <<MenuSelect>> "NSMiscWindow::MenuSelect_InventoryPopup %W"

	$menu delete 0 end

	# Can't call DoUnderlyingCommand because INKEY_CMD is not set
	# while looking at inventory! Can't simply prepend a slash to
	# get the underlying command because request_command() isn't
	# being called. So I have to find a keymap for the given
	# underlying command.
	set cmdChar [angband keymap find $cmdChar]

	set num 0
	foreach item [eval angband $invOrEquip find $args] {
		angband $invOrEquip info $item attrib
		$menu add command -label "$attrib(char) $attrib(name)" \
			-command "angband keypress $cmdChar$attrib(char)" \
			-underline 0
		lappend InventoryPopup(itemList) $item
		incr num
	}

	if {$num} {
		$menu add separator
	}
	$menu add command -label "Cancel"

	set x [winfo rootx [winfo toplevel $canvas]]
	set y [expr {[winfo rooty $canvas] + [winfo height $canvas]}]

	# Calculate the size of the menu. If it will go offscreen, then
	# move it to the left by some appropriate amount.
	update idletasks
	set menuWidth [winfo reqwidth $menu]
	
	# It seems that [winfo reqwidth $menu] does not return a large
	# enoug value.
	incr menuWidth 35
	
	set screenWidth [winfo screenwidth .]
	if {$x + $menuWidth > $screenWidth} {
		incr x [expr {$screenWidth - ($x + $menuWidth)}]
	}

	tk_popup $menu $x $y

	if {[Platform unix]} {
		tkwait variable ::tkPriv(popup)

		# Because we never get a <ButtonRelease> event
		variable Priv
		$canvas move $Priv(button) -1 -1
	}

	# The menu may be used for other things...
	after idle bind $menu <<MenuSelect>> {}

	return
}


# NSMiscWindow::ValueChanged_font_message --
#
#	Called when the font,message value changes.
#	Updates the Message Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::ValueChanged_font_message {} {

	set win [Window message]
	set text [Global message,message]

	$text configure -font [Value font,message]
	update idletasks
	wm geometry $win [winfo reqwidth $win]x[winfo reqheight $win]

	return
}

# NSMiscWindow::ValueChanged_font_misc --
#
#	Called when the font,misc value changes.
#	Updates the Misc Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::MiscArrangeWide {} {

	# Get the Misc Window canvas
	set canvas [Global misc,canvas]

	# Get the desired font
	set font [Value font,misc]

	# Text or pic labels
	set useText [Value misc,textLabels]

	# Calculate the height of a line
	set fontHeight [font metrics $font -linespace]

	set rowHeight $fontHeight
	if {!$useText} {
		set picHeight 18
		if {$picHeight > $fontHeight} {
			set rowHeight $picHeight
		}
	}

	# Set the font of items with "font" tag
	$canvas itemconfigure font -font $font

	$canvas itemconfigure icon -state normal
	$canvas itemconfigure name -anchor w
	$canvas itemconfigure title -anchor w

	foreach tag {HP SP FD hitpoints mana food} {
		$canvas itemconfigure $tag -state hidden
	}

	# Position the character image
	$canvas coords icon 22 22

	# Focus ring
	set inset [expr {22 - [icon size] / 2}]
	set left [expr {$inset - 2}]
	set top [expr {$inset - 2}]
	set right [expr {$left + [icon size] + 2 + 1}]
	set bottom [expr {$top + [icon size] + 2 + 1}]
	$canvas coords focus $left $top $right $bottom
	set topSpace 42

	# Character name
	$canvas coords name 45 [expr {20 - $fontHeight / 2}]

	# Title
	$canvas coords title 45 [expr {20 + $fontHeight / 2}]

	# Stat titles
	if {$useText} {
		set maxWidth 0
	} else {
		set maxWidth 18
	}
	set y [expr {$topSpace - $rowHeight}]
	foreach title {STR INT WIS DEX CON CHR} {
		if {$useText} {
			$canvas coords txt,$title 2 [incr y $rowHeight]
			set width [font measure $font $title]
			if {$width > $maxWidth} {
				set maxWidth $width
			}
			$canvas itemconfigure txt,$title -state normal
			$canvas itemconfigure pic,$title -state hidden
		} else {
			$canvas coords pic,$title 2 [incr y $rowHeight]
			$canvas itemconfigure pic,$title -state normal
			$canvas itemconfigure txt,$title -state hidden
		}
	}

	# Stats
	set pad 10
	set offset [expr {$maxWidth + $pad + [font measure $font 18/999]}]
	set y [expr {$topSpace - $rowHeight}]
if 0 {
	foreach stat [angband info stat_name] {
		$canvas coords $stat $offset [incr y $rowHeight]
	}
}
	set reqWidthLeft $offset

	# Race and Class
	incr offset 15
	set y [expr {$topSpace - $rowHeight}]
	set dy $rowHeight
	foreach info {race class} {
		$canvas coords $info $offset [incr y $dy]
	}

	# Level, Experience, Gold and Armor Class titles
	foreach title {LEVEL EXP AU AC} {
		if {$useText} {
			$canvas coords txt,$title $offset [incr y $rowHeight]
			$canvas itemconfigure txt,$title -state normal
			$canvas itemconfigure pic,$title -state hidden
		} else {
			$canvas coords pic,$title $offset [incr y $rowHeight]
			$canvas itemconfigure pic,$title -state normal
			$canvas itemconfigure txt,$title -state hidden
		}
	}

	# Level, Experience, Gold and Armor Class
	if {$useText} {
		set w 0
		foreach title {EXP ADV AU} {
			set w2 [expr {[font measure $font $title] + [font measure $font 99999999]}]
			if {$w2 > $w} {
				set w $w2
			}
		}
		incr offset [expr {$pad + $w}]
	} else {
		incr offset [expr {18 + $pad + [font measure $font 999999999]}]
	}

	set y [expr {$topSpace + $rowHeight * 2 - $rowHeight}]
	foreach info {level exp gold armor_class} {
		$canvas coords $info $offset [incr y $rowHeight]
	}
	set reqWidthRight [expr {$offset - $reqWidthLeft}]

	pack $canvas.tools -side top -anchor n
	pack propagate $canvas.tools no
	
	# Make room for the toolbar
	$canvas move all 0 20
		
	scan [$canvas bbox armor_class] "%s %s %s %s" x1 y1 x2 y2
	if {!$useText} {
		scan [$canvas bbox pic,AC] "%s %s %s %s" x2 y2 x4 y4
		if {$y4 > $y2} {
			set y2 $y4
		}
	}

	# 1 or 2 pixel border on right and bottom edges
	if {$useText} {
		incr x2 1
		incr y2 1
	} else {
		incr x2 2
		incr y2 2
	}

	if {$x2 < [winfo reqwidth $canvas.tools]} {
		set x2 [winfo reqwidth $canvas.tools]
	}

	# Resize the canvas, set the scroll region
	$canvas configure -width $x2 -height $y2 -scrollregion "0 0 $x2 $y2"

	# Set the width of the divider
	$canvas itemconfigure divider -state normal
	$canvas coords divider 2 22 [expr {$x2 - 2}] 22

	set d [expr {$x2 - $reqWidthLeft - $reqWidthRight - 2}]

if 0 {
	foreach stat [angband info stat_name] {
		$canvas move $stat $d 0
	}
}
	foreach info {race class} {
		$canvas move $info $d 0
	}
	foreach title {LEVEL EXP AU AC} {
		$canvas move $title $d 0
	}
	foreach info {level exp gold armor_class} {
		$canvas move $info $d 0
	}

	# Update items
	MiscSet

	# Cancel geometry
	wm geometry [Window misc] ""

	# Update the Progress Window
	if {[info exists ::Windows(progress)]} {
		wm deiconify [Window progress]
		foreach tag {hitpoints mana food} {
			scan [angband player $tag] "%d %d" cur max
			UpdateHP_SP_FD $tag $cur $max
		}
		ConfigProgress
	}

	return
}

proc NSMiscWindow::MiscArrangeTall {} {

	# Get the Misc Window canvas
	set canvas [Global misc,canvas]

	# Get the desired font
	set font [Value font,misc]

	# Text or pic labels
	set useText [Value misc,textLabels]

	# Calculate the height of a line
	set fontHeight [font metrics $font -linespace]

	set rowHeight $fontHeight
	if {!$useText} {
		set picHeight 18
		if {$picHeight > $fontHeight} {
			set rowHeight $picHeight
		}
	}

	# Set the font of items with "font" tag
	$canvas itemconfigure font -font $font

	$canvas itemconfigure icon -state hidden
	$canvas itemconfigure name -anchor nw
	$canvas itemconfigure title -anchor nw

	foreach tag {HP SP FD hitpoints mana food} {
		$canvas itemconfigure $tag -state normal
	}

	# Name, Race, Class, Title
	set y 2
	foreach tag {name race class title} {
		$canvas coords $tag 2 $y
		incr y $fontHeight
	}

	# Level, Experience, Gold titles
	set top1 [expr {2 + $fontHeight * 4}]
	set y $top1
	foreach title {LEVEL EXP AU} {
		if {$useText} {
			$canvas coords txt,$title 2 $y
			$canvas itemconfigure txt,$title -state normal
			$canvas itemconfigure pic,$title -state hidden
		} else {
			$canvas coords pic,$title 2 $y
			$canvas itemconfigure pic,$title -state normal
			$canvas itemconfigure txt,$title -state hidden
		}
		incr y $rowHeight
	}

	# Stat titles
	set top2 [expr {$top1 + $rowHeight * 3}]
	set y $top2
	foreach title {STR INT WIS DEX CON CHR} {
		if {$useText} {
			$canvas coords txt,$title 2 $y
			$canvas itemconfigure txt,$title -state normal
			$canvas itemconfigure pic,$title -state hidden
		} else {
			$canvas coords pic,$title 2 $y
			$canvas itemconfigure pic,$title -state normal
			$canvas itemconfigure txt,$title -state hidden
		}
		incr y $rowHeight
	}

	# AC title
	set top3 [expr {$top2 + $rowHeight * 6 + $fontHeight}]
	set y $top3
	if {$useText} {
		$canvas coords txt,AC 2 $y
		$canvas itemconfigure txt,AC -state normal
		$canvas itemconfigure pic,AC -state hidden
	} else {
		$canvas coords pic,AC 2 $y
		$canvas itemconfigure pic,AC -state normal
		$canvas itemconfigure txt,AC -state hidden
	}

	# HP SP FD
	set top4 [expr {$top3 + $rowHeight}]
	set y $top4
	foreach title {HP SP FD} {
		$canvas coords $title 2 $y
		incr y $fontHeight
	}

	# Level, Experience, Gold
	set pad 10
	if {$useText} {
		set maxWidth 0
		foreach title {EXP ADV AU} {
			set width [font measure $font $title]
			if {$width > $maxWidth} {
				set maxWidth $width
			}
		}
		set minRight [expr {2 + $maxWidth + $pad}]
	} else {
		set minRight 18
	}
	incr minRight [font measure $font 99999999]

	# Stat
	if {$useText} {
		set maxWidth 0
		foreach title {STR INT WIS DEX CON CHR} {
			set width [font measure $font $title]
			if {$width > $maxWidth} {
				set maxWidth $width
			}
		}
		incr maxWidth [expr {2 + $pad}]
	} else {
		set maxWidth 18
	}
	set right [expr {$maxWidth + [font measure $font 18/999]}]
	if {$right > $minRight} {
		set minRight $right
	}

	if {$useText} {
		set right [expr {2 + [font measure $font "AC"] + $pad}]
	} else {
		set right 18
	}
	incr right [font measure $font 999,+999]
	if {$right > $minRight} {
		set minRight $right
	}

	# Level, Experience, Gold values
	set y $top1
	foreach tag {level exp gold} {
		$canvas coords $tag $minRight $y
		incr y $rowHeight
	}
if 0 {
	# Stat values
	set y $top2
	foreach stat [angband info stat_name] {
		$canvas coords $stat $minRight $y
		incr y $rowHeight
	}
}

	# AC value
	set y $top3
	$canvas coords armor_class $minRight $y

	# HP SP FD
	set y $top4
	foreach option {hitpoints mana food} {
		$canvas coords $option $minRight $y
		incr y $fontHeight
	}

	if {[winfo exists $canvas.tools]} {
		pack forget $canvas.tools
		$canvas itemconfigure divider -state hidden
	}

	scan [$canvas bbox food] "%s %s %s %s" x1 y1 x2 y2

	# 1 or 2 pixel border on right and bottom edges
	if {$useText} {
		incr x2 1
		incr y2 1
	} else {
		incr x2 2
		incr y2 2
	}

	# Resize the canvas, set the scroll region
	$canvas configure -width $x2 -height $y2 -scrollregion "0 0 $x2 $y2"

	# Set the width of the divider
	$canvas coords divider 2 22 [expr {$x2 - 2}] 22

	# Update items
	MiscSet

	# Cancel geometry
	wm geometry [Window misc] ""

	if {[info exists ::Windows(progress)]} {
		wm withdraw [Window progress]
	}

	return
}

proc NSMiscWindow::ValueChanged_font_misc {} {

	set layout [Value misc,layout]
	if {![winfo exists [Global misc,canvas].tools]} {
		set layout tall
	}
	
	switch -- $layout {
		wide { MiscArrangeWide }
		tall { MiscArrangeTall }
	}

	return
}

