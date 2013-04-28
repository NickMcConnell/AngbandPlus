# File: progress-window.tcl

# Purpose: the Progress Window

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSProgressWindow {

# namespace eval NSProgressWindow
}

# NSProgressWindow::InitModule -- 
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressWindow::InitModule {} {

	variable Priv

	MsgCatInit misc-win

	NSModule::LoadIfNeeded NSProgressCanvas

	NSObject::New NSProgressWindow

	return
}

# NSProgressWindow::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressWindow::CloseModule {} {

	catch {
		destroy [Window progress]
	}

	return
}

# NSProgressWindow::NSProgressWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressWindow::NSProgressWindow {oop} {

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow progress $win \
		"NSProgressWindow::GeometryCmd $oop" "" \
		"NSProgressWindow::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSProgressWindow $oop $win

	#
	# Global list of application windows
	#

	Global progress,oop $oop
	Window progress $win

	return
}

# NSProgressWindow::~NSProgressWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressWindow::~NSProgressWindow {oop} {

	return
}

# NSProgressWindow::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressWindow::Info {oop info args} {

	global NSProgressWindow

	# Verify the object
	NSObject::CheckObject NSProgressWindow $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSProgressWindow($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSProgressWindow($oop,$info)
			}
		}
	}

	return
}

# NSProgressWindow::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressWindow::InitWindow {oop} {

	set win .progress$oop
	toplevel $win
	wm title $win [mc "Progress"]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	wm resizable $win yes no
	wm protocol $win WM_DELETE_WINDOW "NSProgressWindow::Close $oop"
	Term_KeyPress_Bind $win

	Info $oop win $win

	set progressId [NSObject::New NSProgressCanvas -parent $win -layout wide]
	set canvas [NSProgressCanvas::Info $progressId canvas]
	Info $oop canvasId $progressId
	Info $oop canvas $canvas

	pack $canvas \
		-expand yes -fill both
if 0 {
	#
	# Context Menu
	#

	set menu $canvas.context
	menu $menu -tearoff 0
	bind $canvas <ButtonPress-3> \
		"NSProgressWindow::ContextMenu $oop $menu %X %Y"
}
	return
}

# NSProgressWindow::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressWindow::DisplayCmd {oop message first} {

	switch -- $message {
		preDisplay {
			Value progress,float 1
			if {[Value misc,float]} {
				set width [[NSMiscWindow::Info [Global misc,oop] canvas] cget -width]
			} else {
				set width [[NSMainWindow::Info [Global main,oop] misc,canvas] cget -width]
			}
			[Info $oop canvas] configure -width $width
			NSProgressCanvas::Info [Info $oop canvasId] layout [Value progress,layout]
			NSProgressCanvas::Arrange [Info $oop canvasId]

			foreach attrib {hitpoints mana food} {
				qeconfigure [Info $oop canvas] <Py-$attrib> -active yes
			}
			qeconfigure [Info $oop canvas] <Track-inventory> -active yes

			wm geometry [Info $oop win] "" ; update idletasks
		}
		postWithdraw {
			Value progress,float 0
			foreach attrib {hitpoints mana food} {
				qeconfigure [Info $oop canvas] <Py-$attrib> -active no
			}
			qeconfigure [Info $oop canvas] <Track-inventory> -active no
		}
	}

	return
}

# NSProgressWindow::GeometryCmd --
#
#	Called by NSWindowManager::Setup(). Returns the desired (default)
#	geometry for the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressWindow::GeometryCmd {oop} {

	set win [Info $oop win]
	set winMain [Window main]

	scan [ScreenArea] "%d %d %d %d" waLeft waTop waRight waBottom
	set waHeight [expr {$waBottom - $waTop}]
	set waWidth [expr {$waRight - $waLeft}]

	set spacing [Global WindowSpacing]

	set x [expr {[NSToplevel::FrameRight $winMain] + $spacing}]

	set right $waRight
	if {[Global HardcodeGeometry]} {
		incr right [OffscreenOffsetX]
	}

	if {$x + [NSToplevel::TotalWidth $win] > $right} {
		set x [expr {$right - [NSToplevel::TotalWidth $win]}]
	}
	if {[Value misc,float]} {
		set y [expr {[NSToplevel::FrameBottom [Window misc]] + $spacing}]
	} elseif {[Value micromap,float]} {
		set y [expr {[NSToplevel::FrameBottom [Window micromap]] + $spacing}]
	} elseif {[Value message,float]} {
		set y [expr {[NSToplevel::FrameBottom [Window message]] + $spacing}]
	} else {
		set y $waTop
	}

	set width [winfo reqwidth $win]
	set height [winfo reqheight $win]

	return ${width}x$height+$x+$y
}

# NSProgressWindow::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressWindow::Close {oop} {

	NSMainWindow::Info [Global main,oop] progress,float 0
	NSMainWindow::MenuInvoke [Global main,oop] ignore E_WINDOW_PROGRESS

	return
}

# NSProgressWindow::ContextMenu --
#
#	Pop up a context menu in the Progress Window to configure it's
# 	appearance.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.
# NOT USED
proc NSProgressWindow::ContextMenu {oop menu x y} {

	$menu delete 0 end

	set showBars [Value progress,showBars]
	set showNumbers [Value progress,showNumbers]

	if {[Value progress,float]} {
		set label [mc "Attach To Misc"]
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
		-command "NSProgressWindow::Toggle $oop showBars"

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
		-command "NSProgressWindow::Toggle $oop showNumbers"

	$menu add separator
	$menu add command -label [mc Cancel]

	tk_popup $menu $x $y

	return
}

# NSProgressWindow::Toggle --
#
#	Hide or show the bars or numbers in the Progress Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSProgressWindow::Toggle {oop which} {

	set showIt [Value progress,$which]
	Value progress,$which [expr {!$showIt}]

	Arrange $oop

	foreach attrib {hitpoints mana food} {
		Update_HP_SP_FD $oop $attrib {*}[angband player $attrib]
	}

	return
}

