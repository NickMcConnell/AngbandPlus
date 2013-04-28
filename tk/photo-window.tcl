# File: photo-window.tcl

# Purpose: take a snapshot of a Widget

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSPhotoWindow {

	variable Priv

# namespace eval NSPhotoWindow
}

# NSPhotoWindow::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhotoWindow::InitModule {} {

	variable Priv

	MsgCatInit

	set Priv(count) 0
	
	return
}

# NSPhotoWindow::NSPhotoWindow --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSPhotoWindow::NSPhotoWindow {oop} {

	variable Priv

	incr Priv(count)

	Info $oop index $Priv(count)

	InitWindow $oop
	
	return
}

# NSPhotoWindow::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhotoWindow::Info {oop info args} {

	global NSPhotoWindow

	# Verify the object
	NSObject::CheckObject NSPhotoWindow $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSPhotoWindow($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSPhotoWindow($oop,$info)
			}
		}
	}

	return
}

# NSPhotoWindow::InitWindow --
#
#	Description.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSPhotoWindow::InitWindow {oop} {

	global Angband
	variable Priv

	set win .photo$oop
	toplevel $win
	wm title $win "Photo #$Priv(count) - $Angband(name)"
	wm resizable $win no no
 
	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSPhotoWindow::Close $oop"

	wm withdraw $win
	
	# Set instance variables
	Info $oop win $win

	# Create menus
	InitMenus $oop

	# Create the (empty) image
	set image Image_Photo$Priv(count)
	image create photo $image
	Info $oop image $image

	# Photo canvas
	NSModule::LoadIfNeeded NSPhoto
	set photoId [NSObject::New NSPhoto $win]
	NSPhoto::Info $photoId examineCmd \
		"NSPhotoWindow::ExamineCmd $oop"
	set canvas [NSPhoto::Info $photoId canvas]
	$canvas configure -borderwidth 1 -relief sunken

	Info $oop canvas $canvas
	Info $oop photoId $photoId

	# "Please Wait" label
	label $win.wait \
		-font "Times 18 bold" -text "Please Wait..."

	# StatusBar
	MakeStatusBar $win.statusBar 20

	# Geometry
	pack $canvas
	place $win.wait \
		-in $canvas -relx 0.5 -rely 0.4 -anchor center
	pack $win.statusBar -side top -fill x

	return
}

# NSPhotoWindow::InitMenus --
#
#	Description.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSPhotoWindow::InitMenus {oop} {

	global NSMenu

	# Default accelerator modifier
	set mod "Ctrl"

	set win [Info $oop win]

	#
	# Menu bar
	#

	Info $oop mbarId [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSPhotoWindow::SetupMenus $oop" -identifier MENUBAR]
	set mbar [Info $oop mbarId]

	#
	# Photo Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_PHOTO
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_PHOTO -label "Photo" -underline 0 -identifier M_PHOTO

	lappend entries [list -type command -label "Save As..." \
		-command "NSPhotoWindow::SaveAs $oop" \
		-underline 5 -identifier E_SAVEAS]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-command "NSPhotoWindow::Close $oop" -underline 0 \
		-accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_PHOTO $entries

	NSMenu::BindAccels $mbar $win

	return
}

# NSPhotoWindow::SetupMenus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhotoWindow::SetupMenus {oop mbarId} {

	global Angband

	lappend identList E_SAVEAS E_CLOSE

	NSMenu::MenuEnable $mbarId $identList

	return
}

# NSPhotoWindow::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhotoWindow::Close {oop} {

	image delete [Info $oop image]
	destroy [Info $oop win]
	NSObject::Delete NSPhotoWindow $oop

	return
}

# NSPhotoWindow::StatusBar --
#
#	Display text in the status bar, perhaps clearing it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhotoWindow::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label [$win.statusBar itemcget t1 -label]
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

# NSPhotoWindow::SetImage --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhotoWindow::SetImage {oop} {

	set image [Info $oop image]
	set widget [Info $oop widget]
	$widget photo $image

	# Gamma correction. Looks okay on my monitor anyways.
	if {[Platform windows]} {
		$image configure -gamma 0.9
	}

	NSPhoto::SetImage [Info $oop photoId] $image

	return
}

# NSPhotoWindow::New --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhotoWindow::New {oop widget} {

	set win [Info $oop win]
	set canvas [Info $oop canvas]

	Info $oop widget $widget

	set width [winfo width $widget]
	set height [winfo height $widget]
	$canvas configure -width $width -height $height
	
	# Position and display the window
	NSToplevel::NaturalSize $win ""
#	wm resizable $win no no
    set x2 [expr {([ScreenWidth $win] - [winfo reqwidth $win]) / 2 \
	    - [winfo vrootx $win]}]
    set y2 [expr {([ScreenHeight $win] - [winfo reqheight $win]) / 3 \
	    - [winfo vrooty $win]}]
	wm withdraw $win
	update idletasks
    wm geometry $win +$x2+$y2
	update idletasks
	WindowBringToFront $win
	update
	focus $canvas

	# Set the image data
	SetImage $oop

if {[icon style] ne "iso"} {
	# Extract information about what is seen
	NSPhoto::ExamineWidget [Info $oop photoId] $widget
}

	# Remove "Please Wait..." label
	destroy $win.wait

	return
}

# NSPhotoWindow::Open --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhotoWindow::Open {oop} {

	global Angband

	set win [Info $oop win]
	set canvas [Info $oop canvas]
	set image [Info $oop image]

	# Position and display the window
	NSToplevel::NaturalSize $win ""
#	wm resizable $win no no
	set x2 [expr {([ScreenWidth $win] - [winfo reqwidth $win]) / 2 \
		- [winfo vrootx $win]}]
	set y2 [expr {([ScreenHeight $win] - [winfo reqheight $win]) / 3 \
		- [winfo vrooty $win]}]
	wm withdraw $win
	update idletasks
	wm geometry $win +$x2+$y2
	update idletasks
	WindowBringToFront $win
	update

	set types {
		{{CompuServe GIF} {.gif}}
		{{JPEG} {.jpg .jpeg}}
		{{PiNG} {.png}}
		{{All files} {*}}
	}
	set filePath [tk_getOpenFile -filetypes $types -parent $win \
		-initialdir [PathUser]]
	if {![string length $filePath]} {
		Close $oop
		return
	}

	wm title $win "[file tail $filePath] - $Angband(name)"
	StatusBar $oop "Reading image file..." 1
	update
	if {[catch {
		$image configure -file $filePath -gamma 0.9
	} result]} {
		tk_messageBox -title "Photo Error" -message $result
		Close $oop
		return
	}

	# Set the image data
	NSPhoto::SetImage [Info $oop photoId] $image

	NSToplevel::MoveOffscreen $win

	set width [image width $image]
	set height [image height $image]
	$canvas configure -width $width -height $height

	WindowPosition $win 2 3
#	WindowBringToFront $win
	focus $canvas
	
	# Extract information about what is seen
	set photoText [file rootname $filePath].txt
	if {[file exists $photoText]} {
		NSPhoto::ReadPhotoText [Info $oop photoId] $photoText
	}

	# Remove "Please Wait..." label
	destroy $win.wait

	return
}

# NSPhotoWindow::SaveAs --
#
#	Write the image to a file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhotoWindow::SaveAs {oop} {

	global Angband

	set image [Info $oop image]
	set win [Info $oop win]

	if {[info tclversion] >= 8.3} {
		set types {
			{{CompuServe GIF} {.gif}}
			{{PiNG} {.png}}
			{{All files} {*}}
		}
	} else {
		if {[lsearch -exact [package names] Img] == -1} {
			tk_messageBox -icon info -title "Missing Img Package" -parent $win \
				-message "The Img package is required for saving photo images.\nSee\
				the file ReadMe_Img for more information."
			return
		}
		package require Img

		### TIFF format seems busted (infinite loop) ###
		set types {
			{{CompuServe GIF} {.gif}}
			{{JPEG} {.jpg .jpeg}}
			{{PiNG} {.png}}
			{{All files} {*}}
		}
	}
	set fileName [tk_getSaveFile -initialfile photo[Info $oop index].gif \
		-initialdir [PathUser] -filetypes $types \
		-parent $win]
	if {![string length $fileName]} return

	switch -- [file extension $fileName] {
		.gif {set format "gif gif89a"}
		.jpg -
		.jpeg {set format "jpeg"}
		.png {set format "png color"}
		default {
			tk_messageBox -icon info -message "Unrecognized file type." \
				-title "Save Photo Error" -parent $win
			return
		}
	}

	# If I was a real keener, I'd allow the user to configure the
	# file format more exactly (JPEG quality, etc)
	StatusBar $oop "Writing image file..." 0
	$image write $fileName -format $format

if {0 && [icon style] ne "iso"} {
	# We should probably make sure the file doesn't exist.
	StatusBar $oop "Writing information file..." 0
	set photoId [Info $oop photoId]
	NSPhoto::WritePhotoText $photoId [file rootname $fileName].txt
}
	StatusBar $oop "Done." 1

	return
}

# NSPhotoWindow::ExamineCmd --
#
#	Describe what is at the given cave location.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPhotoWindow::ExamineCmd {oop photoId y x} {

	global NSPhoto
	
	if {[info exists NSPhoto($photoId,examine,$y,$x)]} {
		StatusBar $oop [NSPhoto::Info $photoId examine,$y,$x] 0
		switch -- [NSPhoto::Info $photoId what,$y,$x] {
			monster {
				NSRecall::RecallMonster [NSPhoto::Info $photoId idx,$y,$x]
			}
			object {
				NSRecall::RecallObjectKind [NSPhoto::Info $photoId idx,$y,$x]
			}
		}
	} else {
		StatusBar $oop "" 0
	}

	return
}

