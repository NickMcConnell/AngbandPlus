# File: tomb.tcl

# Purpose: the Kingly Window, Tomb Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
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
	wm title $win "Winner!"
	wm resizable $win no no

	#
	# Font
	#

	if {[Platform unix]} {
		set font [Global font,fixed,normal]
	}
	if {[Platform windows]} {
		set font [Global font,fixed,small]
	}
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

    append message "Veni, Vidi, Vici!\n"
    append message "I came, I saw, I conquered!\n"
	if {[string equal [angband player sex] Male]} {
		append message "All Hail the Mighty King!"
	} else {
		append message "All Hail the Mighty Queen!"
	}

	incr y [expr {$fontHeight * 14}]
	$c create text \
		$center [incr y $fontHeight] -text $message -fill White \
		-font $font -justify center

	button $win.continue \
		-text "Continue" -width 11 -default active \
		-command "set AngbandPriv(result) 1"

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
	wm title $win "Tomb"
	wm resizable $win no no

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "set AngbandPriv(result) 1"

	#
	# Font
	#

	if {[Platform unix]} {
		set font [Global font,fixed,normal]
	}
	if {[Platform windows]} {
		set font [Global font,fixed,small]
	}
	set fontWidth [font measure $font "W"]
	set fontHeight [font metrics $font -linespace]

	# Split long "died_from" string
	set doSplit 0
	set died_from [format "by %s" [angband player died_from]]
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

	set c $win.canvas
	set wid [expr {$fontWidth * 40}]
	set hgt [expr {$fontHeight * ($doSplit ? 21 : 20)}]
	canvas $c \
		-scrollregion "0 0 0 0" \
		-width $wid -height $hgt \
		-relief flat -highlightthickness 0 -background Black

	#
	# Canvas items
	#

	set center [expr {$wid / 2}]
	set y [expr {$fontHeight * 2}]

	$c create text \
		$center [incr y $fontHeight] -text "RIP" -fill White -font $font \
		-justify center
	incr y $fontHeight

	$c create text \
		$center [incr y $fontHeight] -text [angband player name] -fill White \
		-font $font -justify center
	$c create text \
		$center [incr y $fontHeight] -text "the" -fill White -font $font \
		-justify center
	$c create text \
		$center [incr y $fontHeight] -text [angband player title] -fill White \
		-font $font -justify center
	incr y $fontHeight

	$c create text \
		$center [incr y $fontHeight] \
		-text [angband player class] \
		-fill White -font $font -justify center
	$c create text \
		$center [incr y $fontHeight] \
		-text [format "Level: %s" [angband player level]] \
		-fill White -font $font -justify center
	$c create text \
		$center [incr y $fontHeight] \
		-text [format "Exp: %s" [lindex [angband player exp] 0]] \
		-fill White -font $font -justify center
	$c create text \
		$center [incr y $fontHeight] \
		-text [format "AU: %s" [angband player gold]] \
		-fill White -font $font -justify center
	$c create text \
		$center [incr y $fontHeight] \
		-text [format "Killed on Level %s" [angband player depth]] \
		-fill White -font $font -justify center
	$c create text \
		$center [incr y $fontHeight] -text $died_from \
		-fill White -font $font -justify center
	if {$doSplit} {
		$c create text \
			$center [incr y $fontHeight] -text $died_from2 \
			-fill White -font $font -justify center
	}
	incr y $fontHeight

	$c create text \
		$center [incr y $fontHeight] -text [clock format [clock seconds] \
		-format "%a %b %d %H:%M:%S %Y"] -fill White -font $font \
		-justify center

	#
	# Buttons
	#

	button $win.messages \
		-text "Messages" -width 9 -command "MessageDump $win"
	button $win.continue \
		-text "Continue" -width 11  -default active \
		-command "set AngbandPriv(result) 1"

	#
	# Geometry
	#

	pack $c \
		-expand no
	# Not enough room on Unix
	if {[Platform unix]} {
		pack $win.continue \
			-side bottom -anchor center -pady 10
	}
	if {[Platform windows]} {
		pack $win.continue \
			-side bottom -anchor e -pady 10 -padx 10
		place $win.messages \
			-in $win.file -bordermode outside -relx 0.0 -x -10 -anchor ne
	}

	# Character image
	incr y [expr {$fontHeight + 16}]
	set size [icon size]
	$c create widget $center $y -anchor center -assign [angband player icon]

	NSUtils::SetDefaultButton $win $win.continue

	bind $win <KeyPress-Return> \
		"NSUtils::InvokeDefaultButton $win"

	# Position the window
	WindowPosition $win 2 3

	# Make sure the window is active
	focus $win

	# If you close ALL the windows, the application swaps into
	# the background. So we delay closing the other windows until here.
	if {[winfo exists .kingly]} {
		wm withdraw .kingly
	}
	CloseGameWindows

	# Hack -- Quit without saving
	bind $win <KeyPress-a> {AbortGame}

	# Set a grab on the window and claim the focus
	NSUtils::GrabSave $win
	focus $win.continue

	vwait AngbandPriv(result)

	# Release grab
	NSUtils::GrabRelease $win

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
		if {[string equal $name main]} continue
		wm withdraw $Windows($name)
	}

	wm withdraw [Window main]

	return
}
