# File: misc-window.tcl

# Purpose: Misc Window

#
# Copyright (c) 1997-2009 Tim Baker
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

	MsgCatInit inven misc-win

	NSModule::LoadIfNeeded NSMiscCanvas

	NSObject::New NSMiscWindow

	return
}

# NSMiscWindow::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::CloseModule {} {

	catch {
		destroy [Window misc]
	}

	return
}

# NSMiscWindow::NSMiscWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::NSMiscWindow {oop} {

	NSModule::LoadIfNeeded NSMessageWindow

	#
	# Misc info window
	#

	set win .misc$oop
	toplevel $win
	wm title $win [mc "Misc"]
	wm resizable $win no no

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	wm protocol $win WM_DELETE_WINDOW {
		NSMainWindow::Info [Global main,oop] misc,float 0
		NSMainWindow::MenuInvoke [Global main,oop] ignore E_WINDOW_MISC
	}
	Term_KeyPress_Bind $win

	NSWindowManager::RegisterWindow misc $win \
		"NSMiscWindow::GeometryCmd $oop" "" \
		"NSMiscWindow::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSMiscWindow $oop $win

	Info $oop win $win
	Global misc,oop $oop
	Window misc $win


	#
	# Progress info when Progress Window is closed
	#

	set progressId [NSObject::New NSProgressCanvas -parent $win]
	Info $oop progress,oop $progressId
	Info $oop progress,canvas [NSProgressCanvas::Info $progressId canvas]
	[Info $oop progress,canvas] create line 0 0 1 1 -fill #333366 -tags divider

	#
	# Misc info
	#
	set canvasId [NSObject::New NSMiscCanvas -parent [Window misc] \
		-layout wide -toolbar yes -progress $progressId]
	set c [NSMiscCanvas::Info $canvasId canvas]
	Info $oop canvasId $canvasId
	Info $oop canvas $c
	Info $oop toolbar [NSMiscCanvas::Info $canvasId toolbar,canvas]
	pack [NSMiscCanvas::Info $canvasId frame] \
		-expand no
if 0 {
	# Pop up a menu when the window is clicked
	set menu $c.popup
	menu $menu -tearoff 0
	bind $c <ButtonPress-3> \
		"NSMiscWindow::ContextMenu $oop $menu %X %Y"
}

	#
	# Micro-map window
	#

	NSModule::LoadIfNeeded NSMicroMapWindow

	#
	# Progress window
	#

	NSModule::LoadIfNeeded NSProgressWindow

	return
}

# NSMiscWindow::~NSMiscWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::~NSMiscWindow {oop} {

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

proc NSMiscWindow::Info {oop info args} {

	global NSMiscWindow

	# Verify the object
	NSObject::CheckObject NSMiscWindow $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSMiscWindow($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSMiscWindow($oop,$info)
			}
		}
	}

	return
}

# NSMiscWindow::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::DisplayCmd {oop message first} {

	switch -- $message {
		preDisplay {
			Value misc,float 1
			NSMiscCanvas::Arrange [Info $oop canvasId]
			if {![Value progress,float]} {
				set width [[Info $oop canvas] cget -width]
				[Info $oop progress,canvas] configure -width $width
				pack [Info $oop progress,canvas] -side bottom
				NSProgressCanvas::Arrange [Info $oop progress,oop]
			}

			qeconfigure [Info $oop canvas] <Stat> -active yes
			foreach detail {armor_class exp gold level} {
				qeconfigure [Info $oop canvas] <Py-$detail> -active yes
			}

			wm geometry [Info $oop win] "" ; update idletasks
		}
		postWithdraw {
			Value misc,float 0
			if {![Value progress,float]} {
				pack forget [Info $oop progress,canvas]
			}
			qeconfigure [Info $oop canvas] <Stat> -active no
			foreach detail {armor_class exp gold level} {
				qeconfigure [Info $oop canvas] <Py-$detail> -active no
			}
		}
	}

	return
}

# NSMessageWindow::NSMiscWindow --
#
#	Called by NSWindowManager::Setup(). Returns the desired (default)
#	geometry for the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::GeometryCmd {oop} {

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
	set y [expr {[NSToplevel::FrameBottom [Window micromap]] + $spacing}]

	set width [winfo reqwidth $win]
	set height [winfo reqheight $win]

	return ${width}x$height+$x+$y
}

# NSMiscWindow::AttachProgress --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::AttachProgress {oop} {

	set width [[Info $oop canvas] cget -width]
	[Info $oop progress,canvas] configure -width $width
	pack [Info $oop progress,canvas] -side bottom
	Value progress,layout [NSMiscCanvas::Info [Info $oop canvasId] layout]
	NSProgressCanvas::Info [Info $oop progress,oop] layout [Value progress,layout]
	NSProgressCanvas::Arrange [Info $oop progress,oop]

	[Info $oop progress,canvas] coords divider 0 0 $width 0

	foreach attrib {hitpoints mana food} {
		qeconfigure [Info $oop progress,canvas] <Py-$attrib> -active yes
	}
	qeconfigure [Info $oop progress,canvas] <Track-inventory> -active yes

	wm geometry [Info $oop win] ""

	return
}

# NSMiscWindow::DetachProgress --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::DetachProgress {oop} {

	foreach attrib {hitpoints mana food} {
		qeconfigure [Info $oop progress,canvas] <Py-$attrib> -active no
	}
	qeconfigure [Info $oop progress,canvas] <Track-inventory> -active no

	pack forget [Info $oop progress,canvas] -side bottom
	wm geometry [Info $oop win] ""

	return
}

# NSMiscWindow::SetLayout --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscWindow::SetLayout {oop args} {

	if {[llength $args]} {
		set layout [lindex $args 0]
	} else {
		set layout [NSMiscCanvas::Info [Info $oop canvasId] layout]
	}
	NSMiscCanvas::Info [Info $oop canvasId] layout $layout
	NSMiscCanvas::Arrange [Info $oop canvasId]
	Value misc,layout $layout
	if {![Value progress,float]} {
		set width [[Info $oop canvas] cget -width]
		[Info $oop progress,canvas] configure -width $width
		NSProgressCanvas::Info [Info $oop progress,oop] layout $layout
		NSProgressCanvas::Arrange [Info $oop progress,oop]
		Value progress,layout $layout
	}
	wm geometry [Info $oop win] ""

	return
}

