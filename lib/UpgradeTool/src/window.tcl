# File: window.tcl

# Purpose: main window for UpgradeTool

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSWindow {

# namespace eval NSWindow
}

# NSWindow::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWindow::InitModule {} {

	NSObject::New NSWindow

	return
}

# NSWindow::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWindow::CloseModule {} {

	return
}

# NSWindow::NSWindow --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWindow::NSWindow {oop} {

	InitWindow $oop
	set win [Info $oop win]

	# Defaults when testing
	if {$::DRY_RUN || $::BUILD} {
		if {$::BUILD} {
			set path [Path recent-build]
		} else {
			set path [Path recent]
		}
		if {[file exists $path]} {
			source $path
			$win.frameSrc.entrySrc insert 0 [Global dir,Src]
			$win.frameDst.entryDst insert 0 [Global dir,Dst]
		}
	}

	WindowPosition $win 2 3
	focus $win.frameSrc.entrySrc

	Global win $win
	Global win,oop $oop

	return
}

# NSWindow::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWindow::Info {oop info args} {

	global NSWindow

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSWindow($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSWindow($oop,$info)
			}
		}
	}

	return
}

# NSWindow::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWindow::InitWindow {oop} {

	set win .upgrade$oop
	toplevel $win
	wm title $win [mc UpgradeTool]
	wm resizable $win no no

	wm protocol $win WM_DELETE_WINDOW Exit

	Info $oop win $win
	
	frame $win.frameSrc \
		-borderwidth 0
	frame $win.frameDst \
		-borderwidth 0

	set frame $win.frameSrc
	label $frame.labelSrc \
		-text "Directory of old version:" -anchor w
	entry $frame.entrySrc \
		-width 60
	button $frame.buttonSrc \
		-text "Browse..." -command "NSWindow::ChooseDir $oop Src"

	set frame $win.frameDst
	label $frame.labelDst \
		-text "Directory of new version:" -anchor w
	entry $frame.entryDst \
		-width 60
	button $frame.buttonDst \
		-text "Browse..." -command "NSWindow::ChooseDir $oop Dst"

	set frame $win.frameSrc
	pack $frame \
		-side top -padx 5 -pady 5
	pack $frame.labelSrc \
		-in $frame -side top -padx 5 -anchor w
	pack $frame.entrySrc \
		-in $frame -side left -padx 5
	pack $frame.buttonSrc \
		-in $frame -side left -padx 5

	set frame $win.frameDst
	pack $frame \
		-side top -padx 5 -pady 5
	pack $frame.labelDst \
		-in $frame -side top -padx 5 -anchor w
	pack $frame.entryDst \
		-in $frame -side left -padx 5
	pack $frame.buttonDst \
		-in $frame -side left -padx 5

	label $win.labelStatus \
		-text "Status:" -anchor w
	set frame $win.frameStatus
	frame $frame \
		-borderwidth 2 -relief sunken
	listbox $frame.status \
		-height 5 -width 40 -background White -borderwidth 0 \
		-highlightthickness 0 \
		-yscrollcommand "$frame.yscroll set"
	scrollbar $frame.yscroll \
		-orient vertical -command "$frame.status yview"
	pack $win.labelStatus \
		-side top -anchor w -padx 10
	pack $frame \
		-side top -padx 10 -fill x
	pack $frame.status \
		-side left -fill x -expand yes
	pack $frame.yscroll \
		-side left -fill y
		
	frame $win.divider \
		-borderwidth 1 -height 2 -relief sunken
	pack $win.divider \
		-side top -fill x -padx 10 -pady 5

	set frame $win.frameButtons
	frame $frame \
		-borderwidth 0
	button $frame.begin \
		-text Start -width 9 -command "NSWindow::Start $oop"
	button $frame.cancel \
		-text Quit -width 9 -command "Exit"

	pack $frame \
		-side top -padx 5 -pady 0 -anchor e
	pack $frame.cancel \
		-side right -padx 5 -pady 5
	pack $frame.begin \
		-side right -padx 5 -pady 5
	
	return
}

# NSWindow:: --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWindow::Status {oop string} {

	set win [Info $oop win]
	
	$win.frameStatus.status insert end $string
	$win.frameStatus.status see end
	update

	return
}

# NSWindow::ChooseDir --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWindow::ChooseDir {oop which} {

	set win [Info $oop win]

	set initialDir [$win.frame$which.entry$which get]
	if {![file exists $initialDir] || ![file isdirectory $initialDir]} {
		if {[string equal $which Src]} {
			set which2 Dst
		} else {
			set which2 Src
		}
		set initialDir [$win.frame$which2.entry$which2 get]
		if {[file exists $initialDir] && [file isdirectory $initialDir]} {
			set initialDir [file dirname $initialDir]
		}
	}
	if {![file exists $initialDir] || ![file isdirectory $initialDir]} {
		set initialDir [pwd]
	}	
	set filePath [tk_chooseDirectory -initialdir $initialDir -parent $win \
		-mustexist yes]

	if {![string length $filePath]} return
	set filePath [file nativename [LongName $filePath]]
	
	set win [Info $oop win]
	$win.frame$which.entry$which delete 0 end
	$win.frame$which.entry$which insert 0 $filePath

	return
}

# NSWindow::Start --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSWindow::Start {oop} {

	set win [Info $oop win]

	set pathSrc [$win.frameSrc.entrySrc get]
	set pathDst [$win.frameDst.entryDst get]
		
	if {![file exists $pathSrc] || ![file isdirectory $pathSrc]} {
		MsgBox -message [format [mc err-invalid-dir] $pathSrc]
		return
	}

	if {![file exists $pathDst] || ![file isdirectory $pathDst]} {
		MsgBox -message [format [mc err-invalid-dir] $pathDst]
		return
	}

	set pathSrc [LongName $pathSrc]
	set pathDst [LongName $pathDst]

	if {[string equal $pathSrc $pathDst]} {
		MsgBox -message [mc err-same-dir]
		return
	}

	Global dir,Src $pathSrc
	Global dir,Dst $pathDst

	if {[ExtractVersion Src]} return
	if {[ExtractVersion Dst]} return

	set nameSrc [Global Src,gameName]
	set verSrc [Global Src,versionNumber]
	set relSrc [Global Src,releaseNumber]

	set nameDst [Global Dst,gameName]
	set verDst [Global Dst,versionNumber]
	set relDst [Global Dst,releaseNumber]

	if {[string compare $nameSrc $nameDst]} {
		MsgBox -message [format [mc err-diff] $nameSrc $nameDst]
		return
	}
	if {($verSrc > $verDst) || ($verSrc == $verDst) && ($relSrc > $relDst)} {
		MsgBox -message [mc err-swap]
		return
	}

	# A disabled entry still changes the cursor and, if it had
	# the focus, still responds to mouse clicks.
	focus $win
	
	# Disable the "Start" button, etc
	$win.frameButtons.begin configure -state disabled
	$win.frameSrc.buttonSrc configure -state disabled
	$win.frameDst.buttonDst configure -state disabled
	$win.frameSrc.entrySrc configure -state disabled
	$win.frameDst.entryDst configure -state disabled

	if {$::BUILD} {
		NSModule::LoadIfNeeded NSBuild
		NSBuild::Build
	} else {
		Upgrade
	}

	return
}

proc Status {string} {
	return [NSWindow::Status [Global win,oop] $string]
}
