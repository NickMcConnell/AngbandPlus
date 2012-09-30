# File: init-startup.tcl

# Purpose: the Startup Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

#	This is the first file sourced during program initialization. It is 
#	sourced before init_angband() is called.


# Global --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Global {info args} {

	global Global

	# Set info
	if {[llength $args]} {
		set Global($info) [lindex $args 0]

	# Get info
	} else {
		return $Global($info)
	}

	return
}

# Platform --
#
#	Return 1 if we are running on any of the given platforms.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Platform {args} {

	global Angband

	if {![llength $args]} {
		return $Angband(platform)
	}

	foreach name $args {
		if {[string equal $name $Angband(platform)]} {
			return 1
		}
	}
	return 0
}

# Window --
#
#	Global window info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Window {info args} {

	global Windows

	# Set info
	if {[llength $args]} {
		set Windows($info) [lindex $args 0]

	# Get info
	} else {
		return $Windows($info)
	}

	return
}

# Source --
#
#	Source a .tcl file in the tk directory or a subdirectory of it.
#	Post a warning if the file is not found.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Source {args} {

	global Angband

	set path [eval file join [list $Angband(dirTk)] $args]
	if {![file exists $path]} {
		error "file not found:\n $args, $path"
	}
	uplevel #0 source $path

	return
}

# LongName --
#
#	Under Windows, return -longname of the given file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc LongName {name} {

	if {[string equal $::tcl_platform(platform) windows]} {
		return [file attributes $name -longname]
	}
	return $name
}


# AboutApplication --
#
#	Display program information.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc AboutApplication {} {

	NSModule::LoadIfNeeded NSAbout
	NSAbout::About

	return
}


# QuitNoSave --
#
#	Quit the game without saving. If the game is not asking for
#	a command, then call "game abort". Otherwise do a clean exit.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc QuitNoSave {} {

	global Angband

	# Check if game is waiting for a command
	if {[string compare [angband inkey_flags] INKEY_CMD]} {
		angband game abort
		return
	}

	# Ask the user to confirm quit with save
	set answer [tk_messageBox -icon question -type yesno \
		-title "Quit ZAngband" -message "Do you really want to\
		quit without saving?"]
	if {[string equal $answer no]} return

	# Quit without saving
	DoUnderlyingCommand ^Ey

	return
}

namespace eval NSInitStartup {
}


# NSInitStartup::InitStartupScreen --
#
#	Initialize the startup window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInitStartup::InitStartupScreen {} {

	global Angband

	wm title . "ZAngband"
	wm resizable . no no

	# Quit application if the user closes the window
	wm protocol . WM_DELETE_WINDOW "exit"

	# Load the "Tcl Powered Logo"
	image create photo Image_PwrdLogo175 \
		-file [PathTk image pwrdLogo175.gif]

	# Program name
	if {[Platform unix]} {
		set font {Times 24 bold}
	}
	if {[Platform windows]} {
		set font {Times 18 bold}
	}
	label .title \
		-text "ZAngband" -font $font

	# Program info
	set info "ZAngband\nhttp://www.zangband.org\n"
	label .info \
		-text $info -justify left

	# Tcl Powered Logo
	label .logo \
		-image Image_PwrdLogo175

	# Startup progress
	listbox .prompt \
		-width 15 -height 10 -borderwidth 0 -highlightthickness 0
	.prompt insert end "Initializing arrays..."

	# Geometry
	pack .title \
		-side top -expand no -anchor w
	pack .info \
		-side top -expand no -anchor w
	pack .logo \
		-side left -expand no
	pack [frame .filler -borderwidth 0 -height 10] -side top -anchor w
	pack .prompt \
		-side top -expand no -pady 00 -padx 20 -anchor w

	# Position
	WindowPosition . 2 3

	# When the listbox is unpacked, the window may shrink horizontally.
	# So set the desired window geometry to what it is now (with the list).
	wm geometry . [wm geometry .]

	update

	return
}


# angband_startup --
#
#	Called by Angband (and below) to display status messages
#	during program initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc angband_startup {what} {

	switch -- $what {

		init_misc {
			set prompt "    misc"
		}
		init_script {
			set prompt "    script"
		}
		init_wilderness {
			set prompt "    wilderness"
		}
		init_quests {
			set prompt "    quests"
		}
		init_plots {
			set prompt "    plots"
		}
		init_other {
			set prompt "    other"
		}
		init_alloc {
			set prompt "    alloc"
		}

		default {
			set prompt $what
		}
	}

	.prompt insert end $prompt
	.prompt see end

	update

	return
}

# angband_initialized --
#
#	Called by Angband when program initialization is complete.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc angband_initialized {} {

	.prompt delete 0 end
	.prompt insert end "Sourcing scripts..."

	# Source library files
	angband_startup "    object.tcl"
	Source library object.tcl
	angband_startup "    buttonlabel.tcl"
	Source library buttonlabel.tcl
	angband_startup "    canvist.tcl"
	Source library canvist.tcl
	angband_startup "    menu.tcl"
	Source library menu.tcl
	angband_startup "    module.tcl"
	Source library module.tcl
	angband_startup "    status-text.tcl"
	Source library status-text.tcl
	angband_startup "    progress.tcl"
	Source library progress1.tcl
	Source library progress2.tcl
	Source library progress-window.tcl
	angband_startup "    tabs.tcl"
	Source library tabs.tcl
	angband_startup "    texist.tcl"
	Source library texist.tcl
	angband_startup "    toplevel.tcl"
	Source library toplevel.tcl
	angband_startup "    window-manager.tcl"
	Source library window-manager.tcl

	NSModule::IndexLoad [PathTk library moduleIndex.tcl]
	
	destroy .filler
	destroy .prompt

	Source config.tcl
	NSConfig::InitModule

	return
}



# Because init-other.tcl isn't called before Angband starts calling
# "angband_xxx", I must set a dummy proc's here.

proc angband_display {args} {
}


# InitLoadWindow --
#
#	Creates and displays the Load Window, which is used to display
#	progress during savefile loading and subsequent program initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc InitLoadWindow {} {

	global Angband
	global AngbandPriv

	# If there are no front windows, the application is swapped
	# into the background.
	if {[winfo exists .load]} return
	
	set win .load
	toplevel $win -borderwidth 4 -relief ridge
	wm overrideredirect $win yes

	# Busy
	$win configure -cursor watch

	set width 350
	set height 250
	set height2 40

	set fg Black
	set bg White
	
	set canvas $win.canvas
	canvas $canvas \
		-borderwidth 0 -highlightthickness 0 \
		-width $width -height $height -background $bg

	set progId [NSObject::New NSProgress2 $win.canvas 150 8]

###
	
	if {[Platform unix]} {
		set font {Times 24 bold}
		set font2 {Times 12}
	}
	if {[Platform windows]} {
		set font {Times 18 bold}
		set font2 {Times 10}
	}
	set anchor nw
	set x 11
	set y 11
	set lineSpace [font metrics $font -linespace]

	# Create a "shadow" for the text below
	$canvas create text $x $y -font $font -anchor $anchor -fill gray \
		-text "Zangband"
	$canvas create text $x [expr {$y + $lineSpace}] -font $font2 \
		-text "Copyright (c) 1997-2001 Tim Baker and ZAngband Dev Team" -anchor $anchor -fill gray

	# Draw text over the shadow created above
	incr x -1
	incr y -1
	$canvas create text $x $y -text "ZAngband" \
		-font $font -anchor $anchor -fill $fg
	$canvas create text $x [expr {$y + $lineSpace}] -font $font2 \
		-text "Copyright (c) 1997-2001 Tim Baker" -anchor $anchor -fill $fg \
		-tags copyright

	# Scrolling text
	scan [$canvas bbox copyright] "%s %s %s %s" left top right bottom
	set height3 [expr {$height - ($height2 + 8) - ($bottom + 8)}]
	$canvas create rectangle 0 [expr {$bottom + 8}] $width \
		[expr {$bottom + 8 + $height3}] -fill gray90 -outline {}

	# Rectangle at bottom
	$canvas create rectangle 0 [expr {$height - $height2}] $width $height \
		-fill $fg
	$canvas create text [expr {$width / 2}] [expr {$height - $height2 + 4}] \
		-text "" -fill $bg -anchor n -tags message

###

	set font [Global font,sys,normal]
	set rowHeight [font metrics $font -linespace]
	set rowCount [expr {($height3 - 6) / $rowHeight}]
	set AngbandPriv(load,rowCount) $rowCount
	set y [expr {$height - $height2 - 8 - ($height3 - ($rowHeight * $rowCount)) / 2}]
	for {set row 1} {$row <= $rowCount} {incr row} {
		$canvas create text [expr {$width / 2}] \
			[expr {$y - $row * $rowHeight}] \
			-fill Black -font $font -anchor n -tags row$row
	}

	pack $canvas -padx 1 -pady 1
	place [NSProgress2::Info $progId frame] -x [expr {$width / 2}] \
		-y [expr {$height - 8}] -anchor s

###

	# Position the window
	WindowPosition $win 2 3

	# This inocuous call insures an *active* front window exists
	focus $win

	# Hide the startup screen
	wm withdraw .

	# Cleanup the startup screen
	foreach window [winfo children .] {
		if {[string compare [winfo class $window] Toplevel]} {
			destroy $window
		}
	}
	image delete Image_PwrdLogo175
	foreach tag [bind .] {
		bind . $tag {}
	}

	set AngbandPriv(load,win) $win
	set AngbandPriv(load,prog) $progId
	set AngbandPriv(load,message) {}

	return
}

# LoadNote --
#
#	Inserts the given message to the head of the message queue, then
#	displays all the messages following the message in the Load Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc LoadNote {message} {

	global AngbandPriv

	set win .load
	set canvas $win.canvas

	set AngbandPriv(load,message) \
		[linsert $AngbandPriv(load,message) 0 $message]
	set AngbandPriv(load,message) \
		[lrange $AngbandPriv(load,message) 0 $AngbandPriv(load,rowCount)]
	
	set row 1
	foreach message [lrange $AngbandPriv(load,message) 1 end] {
		$canvas itemconfigure row$row -text $message
		incr row
	}

	return
}

# angband_load --
#
#	Called by Angband to display info during savefile loading.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc angband_load {action {message ""}} {

	global AngbandPriv
	
	switch -- $action {
		kill {
			set win $AngbandPriv(load,win)
			destroy $win
		}
		note {
			set canvas $AngbandPriv(load,win).canvas
			$canvas itemconfigure message -text $message
			LoadNote $message
		}
		progress {
			NSProgress2::SetDoneRatio $AngbandPriv(load,prog) $message
		}
	}

	update

	return
}

# NSInitStartup::InitStartup --
#
#	The main initialization command.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInitStartup::InitStartup {} {

	global auto_path
	global Angband
	global DEBUG
			
	# The tk directory
	set Angband(dirTk) [angband game tkdir]
	set Angband(dirTk) [LongName $Angband(dirTk)]

	# The msgs directory (message catalog)
	set Angband(dirTk,msgs) [list [file join $Angband(dirTk) msgs]]
	
	# Remember the platform
	switch -- $::tcl_platform(platform) {
		macintosh -
		unix -
		windows {
			set Angband(platform) $::tcl_platform(platform)
		}
		default {
			error "unknown platform \"$::tcl_platform(platform)\""
		}
	}
	
	# This call makes sure Tcl reads in info about all available packages.
	# Without this call, the "package names" command returns an empty list.
	# Also, "package require Tk" doesn't scan external packages.
	catch {package require no-such-package}
	
	if {$Angband(platform) == "windows"} {
		# Set the default window icon
		wm iconbitmap . -default [file join $Angband(dirTk) image angbandtk.ico]
	}

	# Development debug support
	set DEBUG 1
	
	Source errorInfo.tcl
	
	proc ::ASSERT {condition message} {
		if {![uplevel expr $condition]} {
			error $message
		}
		return
	}
	
	# Error handling
	Source bgerror.tcl
			
	# Global copyright blurb
	set Angband(copy) "Based on ZAngband (c) the ZAngband Dev Team"

	# Hack -- Require WindowPosition() command
	Source library utils.tcl
	
	# Various game-related commands
	Source misc.tcl
	
	# Create a universal "empty" image
	image create photo Image_Empty

	# Poor-Man's Font Management
	set screenWidth [winfo screenwidth .]
	if {[Platform windows]} {
		set fontSys "{MS Sans Serif}"
		set fontFixed "Courier"
		if {$screenWidth > 800} {
			Global font,sys,small "$fontSys 8"
			Global font,sys,normal "$fontSys 8"
			Global font,sys,large "$fontSys 10"
			Global font,fixed,small "$fontFixed 9"
			Global font,fixed,normal "$fontFixed 10"
		} elseif {$screenWidth > 640} {
			Global font,sys,small "$fontSys 8"
			Global font,sys,normal "$fontSys 8"
			Global font,sys,large "$fontSys 8"
			Global font,fixed,small "$fontFixed 8"
			Global font,fixed,normal "$fontFixed 9"
		} else {
			Global font,sys,small "$fontSys 8"
			Global font,sys,normal "$fontSys 8"
			Global font,sys,large "$fontSys 8"
			Global font,fixed,small "$fontFixed 8"
			Global font,fixed,normal "$fontFixed 8"
		}
	# Platform win
	}
	if {[Platform unix]} {
		Global font,sys,small {Helvetica 10}
		Global font,sys,normal {Helvetica 12}
		Global font,sys,large {Helvetica 12}
		Global font,fixed,small {Courier 10}
		Global font,fixed,normal {Courier 12}
	}
	
	# The MS Windows version of Tk supports platform-specific color names
	# all beginning with "System". Each of these colors is set here for
	# each platform.

	# Get these from library/utils.tcl
	Global SystemButtonFace $::SystemButtonFace
	Global SystemButtonHighlight $::SystemButtonHighlight
	Global SystemButtonShadow $::SystemButtonShadow
	Global SystemHighlight $::SystemHighlight
	Global SystemHighlightText $::SystemHighlightText
	Global System3dLight $::System3dLight
	Global System3dDarkShadow $::System3dDarkShadow
	Global SystemInfoBackground $::SystemInfoBackground

	if {[Platform unix]} {
		option add *Button.default normal widgetDefault
		option add *Button.font {Helvetica 12} widgetDefault
		option add *Checkbutton.font {Helvetica 12} widgetDefault
		option add *Dialog.msg.font {Helvetica 12}
		option add *Entry.background White widgetDefault
		option add *Label.font {Helvetica 12} widgetDefault
		option add *Listbox.font {Helvetica 12} widgetDefault
		option add *Menu.font {Helvetica 12} widgetDefault
		option add *Message.font {Helvetica 12} widgetDefault
		option add *Radiobutton.font {Helvetica 12} widgetDefault
		option add *Scale.font {Helvetica 12} widgetDefault
		option add *selectForeground [Global SystemHighlightText] 100
		option add *selectBackground [Global SystemHighlight] 
	}
		
	# Get term colours
	Global term_attr {"TERM_DARK" \
					"TERM_WHITE" \
					"TERM_SLATE" \
					"TERM_ORANGE" \
					"TERM_RED" \
					"TERM_GREEN" \
					"TERM_BLUE" \
					"TERM_UMBER" \
					"TERM_L_DARK" \
					"TERM_L_WHITE" \
					"TERM_VIOLET" \
					"TERM_YELLOW" \
					"TERM_L_RED" \
					"TERM_L_GREEN" \
					"TERM_L_BLUE" \
					"TERM_L_UMBER"}

	# If a new character is created, this is set to 1
	Global isNewGame 0

	# Value Manager (needed for Birth Options Window)
	Source value-manager.tcl
	
	NSValueManager::InitModule
	
	InitStartupScreen
	
	return
}

# Begin
NSInitStartup::InitStartup
