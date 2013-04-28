# File: tomb.tcl

# Purpose: the Kingly Window, Tomb Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSTomb {

# namespace eval NSTomb
}

# NSTomb::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTomb::InitModule {} {

	MsgCatInit tomb

	return
}

# NSTomb::KinglyWindow --
#
#	Display the window when the user has won the game.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTomb::KinglyWindow {} {

	global AngbandPriv
	global Windows

	#
	# Toplevel
	#

	set win .kingly
	toplevel $win
	wm title $win [mc Winner!]
	wm resizable $win no no

	#
	# Font
	#

	set font [Value font,fixed]
	set fontWidth [font measure $font "W"]
	set fontHeight [font metrics $font -linespace]

	#
	# Main canvas
	#

	set c $win.canvas
	set wid [expr {$fontWidth * 33}]
	set hgt [expr {$fontHeight * 20}]
	canvas $c \
		-scrollregion "0 0 0 0" \
		-width $wid -height $hgt \
		-relief flat -highlightthickness 0 -background Black

	#
	# Canvas items
	#

	set center [expr {$wid / 2}]
	set y 0

	append kingly    "            #            \n"
	append kingly    "          #####          \n"
	append kingly    "            #            \n"
	append kingly    "      ,,,  $$$  ,,,      \n"
	append kingly    "  ,,=$   \"$$$$$\"   $=,,  \n"
	append kingly    ",$$        $$$        $$,\n"
	append kingly    "*>         <*>         <*\n"
	append kingly    "$$         $$$         $$\n"
	append kingly    "\"$$        $$$        $$\"\n"
	append kingly    " \"$$       $$$       $$\" \n"
	append kingly    "  *#########*#########*  \n"
	append kingly    "  *#########*#########*  \n"

	$c create text \
		$center [incr y $fontHeight] -fill White -font $font \
		-anchor n -justify center -text $kingly -fill Gold

    append message [mc "Veni, Vidi, Vici!\n"]
    append message [mc "I came, I saw, I conquered!\n"]
	if {[angband player sex] eq "Male"} {
		append message [mc "All Hail the Mighty King!"]
	} else {
		append message [mc "All Hail the Mighty Queen!"]
	}

	incr y [expr {$fontHeight * 14}]
	$c create text \
		$center [incr y $fontHeight] -text $message -fill White \
		-font $font -justify center

	button $win.continue \
		-text [mc Continue] -width 11 -default active \
		-command "set AngbandPriv(result) 1"
	NSUtils::CheckButtonWidth $win.continue

	#
	# Geometry
	#

	pack $c \
		-expand no
	pack $win.continue \
		-side bottom -anchor center -pady 10

	# Position the window
	WindowPosition $win 2 3

	# Make sure the window is active
	focus $win

	# DirectSound sees child windows as the parent window, and
	# will play sound when either the parent or one of its children
	# is active. But the Kingly Window is not a child window, so we
	# must mark it as a "sound window".
#	angband sound window $win

	# If you close ALL the windows, the application swaps into
	# the background. So we delay closing the other windows until here.
	CloseGameWindows

	# Hack -- Quit without saving
	bind $win <KeyPress-a> {AbortGame}

	# Quit with save if window is closed
	bind $win <Destroy> {set AngbandPriv(result) 1}

	# Set a grab on the window and claim the focus
	NSUtils::GrabSave $win
	focus $win.continue

	vwait AngbandPriv(result)

	# Release grab
	NSUtils::GrabRelease $win

	catch {
		bind $win <Destroy> {}
#		destroy $win
	}

	return
}

# NSTomb::TombWindow --
#
#	Display the window when the character has died (or retired).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTomb::TombWindow {} {

	global AngbandPriv
	global Windows

	#
	# Toplevel
	#

	set win .tomb
	toplevel $win
	wm title $win [mc Tomb]
	wm resizable $win no no

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "set AngbandPriv(result) 1"

	#
	# Font
	#

	set font [Value font,fixed]
	set fontWidth [font measure $font "W"]
	set fontHeight [font metrics $font -linespace]

	# Split long "died_from" string
	set doSplit 0
	set died_from [format [mc "by %s"] [angband player died_from]]
	if {[string length $died_from] > 38} {
		set index [string wordstart $died_from 37]
		set died_from2 [string range $died_from $index end]
		incr index -1
		set died_from [string range $died_from 0 $index]\\
		set doSplit 1
	}

	#
	# Main canvas
	#

	frame $win.bg -borderwidth 0 -background Black

	set c $win.canvas
	set wid [expr {$fontWidth * 40}]
	set hgt [expr {$fontHeight * ($doSplit ? 16 : 15) + 12 + [icon height] + 12}]
	canvas $c \
		-scrollregion "0 0 0 0" \
		-width $wid -height $hgt \
		-relief flat -highlightthickness 0 -background Black

	#
	# Canvas items
	#

	set center [expr {$wid / 2}]
	set y [expr {$fontHeight * 1}]
	set y 0

	set args [list -fill White -font $font -anchor n -justify center]

	$c create text \
		$center [incr y $fontHeight] -text [mc RIP] {*}$args
	incr y $fontHeight

	$c create text \
		$center [incr y $fontHeight] -text [angband player name] {*}$args
	$c create text \
		$center [incr y $fontHeight] -text [mc the] {*}$args
	$c create text \
		$center [incr y $fontHeight] -text [angband player title] {*}$args
	incr y $fontHeight

	$c create text \
		$center [incr y $fontHeight] \
		-text [angband player class] \
		{*}$args
	$c create text \
		$center [incr y $fontHeight] \
		-text [format [mc "Level: %s"] [angband player level]] \
		{*}$args
	$c create text \
		$center [incr y $fontHeight] \
		-text [format [mc "Exp: %s"] [lindex [angband player exp] 0]] \
		{*}$args
	$c create text \
		$center [incr y $fontHeight] \
		-text [format [mc "AU: %s"] [angband player gold]] \
		{*}$args
	$c create text \
		$center [incr y $fontHeight] \
		-text [format [mc "Killed on Level %s"] [angband player depth]] \
		{*}$args
	$c create text \
		$center [incr y $fontHeight] -text $died_from \
		{*}$args
	if {$doSplit} {
		$c create text \
			$center [incr y $fontHeight] -text $died_from2 \
			{*}$args
	}
	incr y $fontHeight

	$c create text \
		$center [incr y $fontHeight] -text [clock format [clock seconds] \
		-format "%a %b %d %H:%M:%S %Y"] {*}$args

	#
	# Buttons
	#

	set frame $win.frameButton
	frame $frame \
		-borderwidth 0
	button $frame.messages \
		-text [mc Messages] -width 9 -command "MessageDump $win"
	button $frame.file \
		-text [mc File] -width 9 -command "FileCharacter $win"
	button $frame.continue \
		-text [mc Continue] -width 9 -default active \
		-command "set AngbandPriv(result) 1" -state disabled

	NSUtils::CheckButtonWidth $frame.messages
	NSUtils::CheckButtonWidth $frame.file
	NSUtils::CheckButtonWidth $frame.continue

	#
	# Geometry
	#

	place $win.bg \
		-x 0 -y 0 -relwidth 1.0 -height [winfo reqheight $c]
	pack $c \
		-expand no
	pack $win.frameButton \
		-side bottom -anchor center -padx 10 -pady 10
	pack $win.frameButton.continue \
		-side right
	pack $win.frameButton.file \
		-side right -padx 10
	pack $win.frameButton.messages \
		-side right

	# Character image
	incr y [expr {$fontHeight}]
	incr y [expr {([winfo reqheight $c] - $y) / 2}]
	$c create widget $center $y -anchor center -assign [angband player icon]

	NSUtils::SetDefaultButton $win $win.frameButton.continue

	bind $win <KeyPress-Return> \
		"NSUtils::InvokeDefaultButton $win"

	# Position the window
	WindowPosition $win 2 3

	# Make sure the window is active
	focus $win

	# DirectSound sees child windows as the parent window, and
	# will play sound when either the parent or one of its children
	# is active. But the Tomb Window is not a child window, so we
	# must mark it as a "sound window".
#	angband sound window $win

	# If you close ALL the windows, the application swaps into
	# the background. So we delay closing the other windows until here.
	if {[winfo exists .kingly]} {
		wm withdraw .kingly
	}
	CloseGameWindows

	# Hack -- Quit without saving
	bind $win <KeyPress-a> {AbortGame}

bind $win <KeyPress-p> {
	NSModule::LoadIfNeeded NSPhotoWindow
	set photoId [NSObject::New NSPhotoWindow]
	NSPhotoWindow::New $photoId [Global main,widget]
}

	# Set a grab on the window and claim the focus
	NSUtils::GrabSave $win
	focus $win.frameButton.continue

	# Don't let in-game keypress close the window too soon
	after 3000 [list $win.frameButton.continue configure -state normal]

	vwait AngbandPriv(result)

	# Release grab
	NSUtils::GrabRelease $win

	catch {
#		destroy $win
	}

	# Allow the user to create a new character record.
	CharacterRecord

	return
}

# NSTomb::CharacterRecord --
#
#	Allow the user to specify options for a new character record.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTomb::CharacterRecord {} {

	global Angband
	global AngbandPriv
	global CharacterRecord

	set win .record
	toplevel $win
	wm title $win [mc "Character Record"]
	wm resizable $win no no

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "set AngbandPriv(result) 0"

	set frame $win.framePrompt
	frame $frame \
		-borderwidth 0

	# Note: Here we are using the "icon photo" command to set a
	# photo image with the character icon. This is because the
	# Widget-type canvas item does not handle transparency
	# correctly.
	set image Image_RecordNNNN ; # TempImageName() ?
	image create photo $image -width [icon width] -height [icon height]
	set icon [assign toicon [angband player icon]]
	set count [scan $icon "%s %d %d" iconType iconIndex ascii]
	if {$count == 3} {
		icon photo $image -type $iconType -index $iconIndex -ascii $ascii
	} else {
		icon photo $image -type $iconType -index $iconIndex
	}
	label $frame.image -image $image
	set imagePad [expr {(32 - [icon width]) / 2}]

	set message $frame.prompt
	message $message \
		-anchor w -width 250 -borderwidth 0 -text [mc *prompt]

	incr imagePad 4
	pack $frame.image -side left -anchor w -padx $imagePad -pady $imagePad
	pack $frame.prompt -side left -anchor w

	# Restore the user's settings
	set CharacterRecord(dump) [Value record,dump]
	set CharacterRecord(message) [Value record,message]
	set CharacterRecord(photo) [Value record,photo]
	
	set frame $win.frameCheck
	frame $frame \
		-borderwidth 2 -relief groove
	checkbutton $frame.checkDump \
		-text [mc "Character dump"] -variable CharacterRecord(dump)
	checkbutton $frame.checkMessage \
		-text [mc "Message log"] -variable CharacterRecord(message)
	checkbutton $frame.checkPhoto \
		-text [mc Photo] -variable CharacterRecord(photo)

#	pack [frame $frame.pad1 -borderwidth 0 -height 6] -side top -fill x
	pack $frame.checkDump -side top -anchor w -padx 6 -pady {6 0}
	pack $frame.checkMessage -side top -anchor w -padx 6
	pack $frame.checkPhoto -side top -anchor w -padx 6 -pady {0 6}
#	pack [frame $frame.pad2 -borderwidth 0 -height 6] -side top -fill x

	label $win.label \
		-text [mc Options]

	set frame $win.frameButton
	frame $frame \
		-borderwidth 0
	button $frame.buttonOK \
		-text [mc OK] -width 11 -command "set AngbandPriv(result) 1"
	button $frame.buttonCancel \
		-text [mc Cancel] -width 11 -command "set AngbandPriv(result) 0"

	pack $frame.buttonCancel -side right -padx 6
	pack $frame.buttonOK -side right

	pack $win.framePrompt -side top -anchor w -padx 6 -pady 6
	pack $win.frameCheck -side top -anchor w -fill x -padx 6 -pady 6
	pack $win.frameButton -side top -anchor e
	pack [frame $win.pad1 -borderwidth 0 -height 6] -side top -fill x

	NSUtils::SetDefaultButton $win $win.frameButton.buttonOK

	bind $win <KeyPress-Return> \
		"NSUtils::InvokeDefaultButton $win"
	bind $win <KeyPress-Escape> \
		"NSUtils::InvokeButton $win.frameButton.buttonCancel"

	if {[info tclversion] < 8.3} {
		# Photo isn't possible without the Img extension
		if {[catch {
			package require Img
		}]} {
			$win.frameCheck.checkPhoto configure -state disabled \
				-text [mc "Photo (Img extension missing)"]
		}
	}

	# The zip extension is required
	if {[catch {
		package require TclZip
	}]} {
		$win.frameButton.buttonOK configure -state disabled
	}

	# Position the window
	WindowPosition $win 2 3

	# Make sure the window is active
	focus $win

	destroy .tomb

	# Needed to get geometry correct before placing the label below
	update

	set height [winfo reqheight $win.label]
	place $win.label -in $win.frameCheck -x 6 -y [expr {-$height / 2 + 1}] -bordermode outside

	set AngbandPriv(result) -1

	bind $win <KeyPress-a> {AbortGame}

	# Set a grab on the window and claim the focus
	NSUtils::GrabSave $win
	focus $win.frameButton.buttonOK

	vwait AngbandPriv(result)

	# Release grab
	NSUtils::GrabRelease $win

	if {$AngbandPriv(result) == 1} {

		# Save the user's settings
		Value record,dump $CharacterRecord(dump)
		Value record,message $CharacterRecord(message)
		Value record,photo $CharacterRecord(photo)

		if {[catch {
			CharacterRecordDump
		} result]} {
			HandleError $result
		}
	}
	
#	destroy $win

	return
}

# NSTomb::DumpScoreFile --
#
#	Each character record has a score.txt file which is used to
#	identify the character the record is for. We want to save enough
#	information so we can uniquely identify a character when reading
#	the scores.raw file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTomb::DumpScoreFile {filePath} {

	if {[catch {openlf $filePath} fileId]} {
		set msg "The following error occurred while attempting write "
		append msg "the score.txt file:\n\n$fileId"
		tk_messageBox -title Error -message $msg
		return 1
	}

	puts $fileId ".what [angband game version]"
	puts $fileId ".gold [angband player gold]"
	puts $fileId ".turns [angband player turn]"
	puts $fileId ".who \"[angband player name]\""
	puts $fileId ".sex [angband player sex]"
	puts $fileId ".race \"[angband player race]\""
	puts $fileId ".class \"[angband player class]\""
	puts $fileId ".cur_lev [angband player level]"
	puts $fileId ".cur_dun [angband player depth]"
	puts $fileId ".max_lev [angband player max_lev]"
	puts $fileId ".max_dun [angband player max_depth]"
	puts $fileId ".how \"[angband player died_from]\""
	
	close $fileId

	return 0
}

# NSTomb::CharacterRecordDump --
#
#	Create a new character record based on the user's preferences.
#	The new record will be saved as lib/user/dumpNNNN.zip.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTomb::CharacterRecordDump {} {

	global Angband

	set makeRecord 0
	set tempFileList {}

	set dumpFile ""
	if {[Value record,dump]} {
		set dumpFile [NSUtils::TempFileName $Angband(dir)]
		if {[catch {angband game file_character $dumpFile} result]} {
			set msg "The following error occurred while attempting write "
			append msg "the character dump:\n\n$result"
			tk_messageBox -title Error -message $msg
			set dumpFile ""
		} else {
			lappend tempFileList $dumpFile
			set makeRecord 1
		}
	}

	set messageFile ""
	if {[Value record,message]} {
		set messageFile [NSUtils::TempFileName $Angband(dir)]
		if {[MessageDumpAux $messageFile]} {
			set messageFile ""
		} else {
			lappend tempFileList $messageFile
			set makeRecord 1
		}
	}

	set photoFile ""
	set photoText ""
	set doPhoto [Value record,photo]
	if {[info tclversion] < 8.3} {
		set doPhoto [expr {$doPhoto && ![catch {package require Img}]}]
	}
	if {$doPhoto} {
		set photoFile [NSUtils::TempFileName $Angband(dir)]
		set widget [Global main,widget]
		set image Image_Record
		set width [winfo width $widget]
		set height [winfo height $widget]
		image create photo $image -width $width -height $height
		$widget photo $image

		if {[catch {
		
			# ALWAYS A GIF!!! Allow the user to choose.
			$image write $photoFile -format "gif gif89a"
		
			lappend tempFileList $photoFile
			set makeRecord 1
	
			set photoText [Global photoText]
			if {[string length $photoText]} {
				lappend tempFileList $photoText
			}
			
		} result]} {
			set msg "The following error occurred while attempting write "
			append msg "the photo:\n\n$result"
			tk_messageBox -title Error -message $msg
			set photoFile ""
		}
	}

	if {$makeRecord} {
	
		# Create a record to identify the character. If this doesn't
		# work, then don't write a record
		set scoreFile [NSUtils::TempFileName $Angband(dir)]
		if {[DumpScoreFile $scoreFile]} {
			set makeRecord 0
		} else {
			lappend tempFileList $scoreFile
		}
	}

	if {$makeRecord} {

		# Pick a unique name for this character record, of the
		# form dumpNNNN.zip, where NNNN is some number.
		set dir [PathUser]
		set index 0
		set archive [file join $dir [format dump%04d $index].zip]
		while {[file exists $archive]} {
			incr index
			set archive [file join $dir [format dump%04d $index].zip]
		}

		# Create the zip archive
		set zipCmd [zip recordZipCmd]
		$zipCmd add $scoreFile score.txt
		if {[string length $dumpFile]} {
			$zipCmd add $dumpFile dump.txt
		}
		if {[string length $messageFile]} {
			$zipCmd add $messageFile message.txt
		}
		if {[string length $photoFile]} {
			$zipCmd add $photoFile photo.gif
		}
		if {[string length $photoText]} {
			$zipCmd add $photoText photo.txt
		}
		$zipCmd write $archive
		rename $zipCmd ""
	}

	# Delete the temporary files
	if {[llength $tempFileList]} {
		file delete {*}$tempFileList
	}

	return
}

# NSTomb::CloseGameWindows --
#
#	Close all the game windows.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSTomb::CloseGameWindows {} {

	global Windows

	# Tk 8.3.0 bug: If you withdraw a transient but the master is
	# already withdrawn, nothing happens. So withdraw the Main Window
	# after all the others.

	foreach name [array names Windows] {
		if {$name eq "main"} continue
		wm withdraw $Windows($name)
	}

	wm withdraw [Window main]

	return
}
