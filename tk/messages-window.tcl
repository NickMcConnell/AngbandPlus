# File: messages-window.tcl

# Purpose: the Messages Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSMessagesWindow {

	variable Priv

# namespace eval NSMessagesWindow
}

# NSMessagesWindow::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessagesWindow::InitModule {} {

	MsgCatInit message

	NSObject::New NSMessagesWindow

	return
}

# NSMessagesWindow::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessagesWindow::CloseModule {} {

	catch {
		destroy [Window message2]
	}

	return
}

# NSMessagesWindow::NSMessagesWindow --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMessagesWindow::NSMessagesWindow {oop} {

	Info $oop afterId ""

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow message2 $win \
		"NSMessagesWindow::GeometryCmd $oop" "" \
		"NSMessagesWindow::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSMessagesWindow $oop $win

	# Update ourself when the font,messages value changes
	qebind NSMessagesWindow <Value-font,messages> \
		"NSMessagesWindow::ValueChanged_Font_Messages $oop"

	qebind NSMessagesWindow <Value-listBG> \
		"ListBackgroundChanged [Info $oop text]"

	qebind NSMessagesWindow <Track-message> \
		"NSMessagesWindow::TrackMessage $oop"
	qeconfigure NSMessagesWindow <Track-message> -active no

	#
	# Global list of application windows
	#

	Global message2,oop $oop
	Window message2 $win

	return
}

# NSMessagesWindow::~NSMessagesWindow --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessagesWindow::~NSMessagesWindow {oop} {

	return
}

# NSMessagesWindow::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessagesWindow::Info {oop info args} {

	global NSMessagesWindow

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSMessagesWindow($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSMessagesWindow($oop,$info)
			}
		}
	}

	return
}

# NSMessagesWindow::InitWindow --
#
#	Create the window.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMessagesWindow::InitWindow {oop} {

	set win .message$oop
	toplevel $win
	wm title $win [mc Messages]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Feed the Term when keys are pressed
	Term_KeyPress_Bind $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSMessagesWindow::Close $oop"

	# Set instance variables
	Info $oop win $win

	set frame $win.frame
	frame $frame \
		-borderwidth 0

	set font [Value font,messages]
	set width 60
	set height 5
if 1 {
	set tree $frame.tree
	treectrl $tree -usetheme yes -background [Value listBG] \
		-showroot no -showlines no -showbuttons no -showheader no \
		-highlightthickness 0 -borderwidth 0 -font $font

	set itemHeight [expr {[font metrics $font -linespace] + 2}]
	$tree configure -itemheight $itemHeight \
		-height [expr {$itemHeight * $height}] \
		-width [expr {[font measure $font W] * $width}]
$tree configure -itemheight 0 -yscrollincrement 1; # so item wrapping works

	#
	# Item style
	#
	$tree element create eSel rect
	$tree element create eTxt text -fill White -lmargin2 24

	$tree style create STYLE
	$tree style elements STYLE {eSel eTxt}
	$tree style layout STYLE eSel -detach yes -iexpand xy
	$tree style layout STYLE eTxt -padx {4 4} -pady 1 -squeeze x

	$tree column create -itemstyle STYLE -squeeze yes -tags COL

	bindtags $tree [list $tree $win all]

	set text $tree
} else {
	set text $frame.text
	text $text \
		-borderwidth 0 -highlightthickness 0 -font $font -width 50 -height 5 \
		-background [Value listBG] -foreground White -wrap none \
		-cursor {}
	bindtags $text [list $text $win all]
}
	pack $text -expand yes -fill both
	pack $frame -expand yes -fill both

	Info $oop text $text

	bind $text <Configure> \
		"NSMessagesWindow::Configure $oop %h %w"

	bind $text <Double-ButtonPress-1> {
		DoUnderlyingCommand ^p
	}

	# Pop up a menu when the window is clicked
	set menu $text.popup
	menu $menu -tearoff 0
	bind $text <ButtonPress-3> \
		"NSMessagesWindow::ContextMenu $oop $menu %X %Y"
if 0 {
	# Support for colored messages
	foreach attr [angband info term_attr] {
		$text tag configure $attr -foreground [Value $attr]
	}
}
	return
}

# NSMessagesWindow::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessagesWindow::DisplayCmd {oop message first args} {

	switch -- $message {
		preDisplay {
			qeconfigure NSMessagesWindow <Track-message> -active yes
		}
		postDisplay {
			Value message2window,show 1
		}
		postWithdraw {
			qeconfigure NSMessagesWindow <Track-message> -active no
			Value message2window,show 0
		}
	}

	return
}

# NSMessagesWindow::GeometryCmd --
#
#	Called by NSWindowManager::Setup(). Returns the desired (default)
#	geometry for the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessagesWindow::GeometryCmd {oop} {

	set win [Info $oop win]

	scan [ScreenArea] "%d %d %d %d" waLeft waTop waRight waBottom
	set waHeight [expr {$waBottom - $waTop}]
	set waWidth [expr {$waRight - $waLeft}]

	set spacing [Global WindowSpacing]

	set right $waRight
	if {[Global HardcodeGeometry]} {
		incr right [OffscreenOffsetX]
	}

	if {1 || [ScreenHeight] >= 1024} { # 1280x1024
		set x [NSToplevel::FrameRight [Window main]]
		if {[Value progress,float]} {
			set y [NSToplevel::FrameBottom [Window progress]]
		} elseif {[Value misc,float]} {
			set y [NSToplevel::FrameBottom [Window misc]]
		} elseif {[Value micromap,float]} {
			set y [NSToplevel::FrameBottom [Window micromap]]
		} elseif {[Value message,float]} {
			set y [NSToplevel::FrameBottom [Window message]]
		} else {
			set y $waTop
		}
		incr y $spacing
		set width [NSToplevel::ContentWidth $win [expr {$right - $x}]]
		set height [NSToplevel::ContentHeight $win [expr {$waBottom - $y}]]
	} else { # 800x600, 640x480
		set width [winfo width $win]
		set height [winfo height $win]
		set x [expr {$waRight - [NSToplevel::TotalWidth $win]}]
		if {[Global HardcodeGeometry]} {
			incr x [OffscreenOffsetX]
		}
		set y $waTop
	}

	return ${width}x$height+$x+$y
}

# NSMessagesWindow::Close --
#
#	Description. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMessagesWindow::Close {oop} {

	NSWindowManager::Undisplay message2

	return
}

# NSMessagesWindow::ContextMenu --
#
#	Pop up a context menu in the Messages Window to configure it's
# 	appearance.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessagesWindow::ContextMenu {oop menu x y} {

	$menu delete 0 end

	$menu add command -label [mc "Set Font"] \
		-command "NSModule::LoadIfNeeded NSFont ; NSWindowManager::Display font messages"
	$menu add separator
	$menu add command -label [mc Cancel]

	tk_popup $menu $x $y

	return
}

# NSMessagesWindow::TrackMessage --
#
#	Description. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMessagesWindow::TrackMessage {oop} {
	if {[Info $oop afterId] ne ""} return
	set afterId [after 50 "NSMessagesWindow::TrackMessageAux $oop"]
	Info $oop afterId $afterId
	return
}

proc NSMessagesWindow::TrackMessageAux {oop} {

	Info $oop afterId ""

	set tree [Info $oop text]

	$tree item delete all

	set max [angband message count]

	# Limit to number of displayed messages
	if {$max > [Info $oop numRows]} {
		set max [Info $oop numRows]
	}

	# Option: Combine identical messages
	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		set combine [Value messages,combine]
	}
	if {[variant ZANGBANDTK]} {
		set combine 0
	}

	# Option: Combine identical messages (ex "You hit it. (x3)")
	if {$combine} {

		set curMsg ""
		set curSound ""
		set count 0
		set total 0
		for {set i 0} {$i < [angband message count]} {incr i} {
			set nextMsg [angband message get $i]
			set nextSound [angband message sound $i]
			if {$curMsg ne $nextMsg} {
				if {[string length $curMsg]} {
					if {$count > 1} {
						append curMsg " (x$count)"
					}
					lappend textList $curMsg
					lappend colorList [angband message color $curAge]
					lappend soundList $curSound
					incr total
					if {$total + 1 >= [Info $oop numRows]} {
						set curMsg $nextMsg
						set curSound $nextSound
						set curAge $i
						set count 1
						break
					}
				}
				set curMsg $nextMsg
				set curSound $nextSound
				set curAge $i
				set count 1
			} else {
				incr count
			}
		}

		if {$count > 1} {
			append curMsg " (x$count)"
		}
		lappend textList $curMsg
		lappend colorList [angband message color $curAge]
		lappend soundList $curSound
		incr total

	# Don't combine identical messages
	} else {
		for {set i [expr {$max - 1}]} {$i >= 0} {incr i -1} {
			set curMsg [angband message get $i]
			set curSound [angband message sound $i]
			lappend textList $curMsg
			lappend colorList [angband message color $i]
			lappend soundList $curSound
			lappend age $i
		}
	}

	set max [llength $textList]

	set textList [lrange $textList end-$max end]
	set colorList [lrange $colorList end-$max end]
	set soundList [lrange $soundList end-$max end]

	foreach text $textList color $colorList sound $soundList {
		set item [$tree item create]
		if {$sound ne ""} {
		}
		$tree item text $item COL $text
		if {$color ne "TERM_WHITE"} {
			$tree item element configure $item COL eTxt -fill [Value $color]
		}
		if {$combine} {
			$tree item firstchild root $item
		} else {
			$tree item lastchild root $item
		}
	}

	$tree see end

if 0 {
	set text [Info $oop text]
	$text delete 1.0 end

	# Get the number of messages
	set max [angband message count]

	# Limit to number of displayed messages
	if {$max > [Info $oop numRows]} {
		set max [Info $oop numRows]
	}
	incr max -1

	# Collect each message
	for {set i $max} {$i >= 0} {incr i -1} {
		lappend info [angband message get $i] [angband message color $i] \n {}
	}

	# Display
	$text insert end {*}$info
}
	return
}

# NSMessagesWindow::Configure --
#
#	Description. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMessagesWindow::Configure {oop height width} {

	set text [Info $oop text]
	set height [winfo height $text]
if 1 {
	set rowHeight [$text cget -itemheight]
	set rowHeight [expr {[font metrics [Value font,messages] -linespace] + 2}]
} else {
	incr height [expr {[$text cget -pady] * -2}]
	set rowHeight [font metrics [Value font,messages] -linespace]
}
	set numRows [expr {$height / $rowHeight}]
	Info $oop numRows $numRows

	TrackMessage $oop

	return
}

# NSMessagesWindow::ValueChanged_Font_Messages --
#
#	Called when the font,messages value changes.
#	Updates the Message Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMessagesWindow::ValueChanged_Font_Messages {oop} {

	set text [Info $oop text]
	$text configure -font [Value font,messages]
if 1 {
	set itemHeight [expr {[font metrics [Value font,messages] -linespace] + 2}]
set itemHeight 0 ; # so item wrapping works
	$text configure -itemheight $itemHeight
}
	Configure $oop [winfo height $text] [winfo width $text]

	return
}

