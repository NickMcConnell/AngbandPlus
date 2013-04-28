# File: main.tcl

# Purpose: entry point

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

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

# /home/./tnb/foo/../bar ==> /home/tnb/foo/bar
proc CleanPath {path} {

	set result {}
	foreach elem [file split $path] {
		if {[string equal $elem .]} continue
		if {[string equal $elem ..]} {
			set result [lrange $result 0 end-1]
			continue
		}
		lappend result $elem
	}

	return [eval file join $result]
}

# ScreenWidth --
#
#	Return width of screen.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc ScreenWidth {{win .}} {

	return [winfo screenwidth $win]
}

# ScreenHeight --
#
#	Return height of screen.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc ScreenHeight {{win .}} {

	return [winfo screenheight $win]
}

# Source --
#
#	Source a .tcl file.
#	Post a warning if the file is not found.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Source {args} {

	set path [file join [Global dir,root] {*}$args]
	if {![file exists $path]} {
		error "file not found:\n$path"
	}
	uplevel #0 source $path

	return
}

# Path --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Path {args} {

	return [file join [Global dir,root] {*}$args]
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

	global tcl_platform

	if {![llength $args]} {
		return $tcl_platform(platform)
	}

	foreach name $args {
		if {$::DEBUG} {
			set platformList [list macintosh unix windows]
			if {[lsearch -exact $platformList $name] == -1} {
				tk_messageBox -message "unknown platform \"$name\""
			}
		}
		if {[string equal $name $tcl_platform(platform)]} {
			return 1
		}
	}
	return 0
}

# LongName --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc LongName {path} {

	if {[string equal $::tcl_platform(platform) windows]} {
		return [file attributes $path -longname]
	}
	return $path
}

# ldelete --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc ldelete {_listName args} {

	upvar $_listName list

	foreach elem $args {
		while {[set index [lsearch -exact $list $elem]] != -1} {
			set list [lreplace $list $index $index]
		}
	}

	return
}

# MsgBox --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc MsgBox {args} {

	array set opt [list \
		-icon info \
		-message "???" \
		-parent [Global win] \
		-title [mc UpgradeTool] \
	]
	array set opt $args

	return [tk_messageBox {*}[array get opt]]
}

# Exit --
#
#	Quit.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Exit {} {

	if {$::DRY_RUN || $::BUILD} {
		if {$::BUILD} {
			set path [Path recent-build]
		} else {
			set path [Path recent]
		}
		catch {
			set chan [open $path [list CREAT WRONLY TRUNC]]
			append buf [list Global dir,Src [Global dir,Src]] \n
			append buf [list Global dir,Dst [Global dir,Dst]]
			puts -nonewline $chan $buf
			close $chan
		}
	}

	if {!$::DRY_RUN} {
		if {[Global logFileId] != ""} {
			close [Global logFileId]
		}
	}

	exit

	return
}

# Main --
#
#	Program entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Main {} {

	global auto_path

	# Get the complete pathname of our directory
	set path [file dirname [info script]]
	if {[string equal [file pathtype $path] relative]} {
		set path [file join [pwd] $path]
	}
	set path [CleanPath $path]
	Global dir,root $path

	set auto_path [concat [list [Path .. lib]] $auto_path]

	if {$::DEBUG} {
		package require dbwin
	} else {
		proc ::dbwin {string} {
			puts $string
		}
	}

	# The Tk console!
	if {[llength [info commands console]]} {
		console hide
		console title "Console - UpgradeTool"
		bind all <Control-KeyPress-C> {console show}

		# Hack -- Change the font
		console eval ".console configure -font {Courier 9}"

		# Hack -- Show initial prompt (picky me)
		console eval tk::ConsolePrompt

		# Hack -- Add a "Clear" menu item
		console eval {.menubar.edit add command -label "Clear" -underline 4 \
			-command {.console delete 1.0 end ; tkConsolePrompt}}

		proc ::evalclip {} {
			uplevel #0 [selection get -selection CLIPBOARD]
		}
	}

	# Work around a bug on Windows 95. Note that this also includes Win98, even
	# though Win98 doesn't have the bug.
	if {[string equal $::tcl_platform(os) "Windows 95"]} {
		load [Path .. tk_chooseDirectory tk_chooseDirectory.dll]
	}

	# Internationalization
	package require msgcat
	::msgcat::mclocale en
	namespace import ::msgcat::*
	::msgcat::mcload [Path src msgs]

	# Grab some files from OmnibandTk
	Source .. .. tk library utils.tcl
	Source .. .. tk library object.tcl
	Source .. .. tk library module.tcl

Global tclCompiler 0

	NSModule::IndexLoad [Path src moduleIndex.tcl]
	NSModule::IndexLoad [Path src convert moduleIndex.tcl]

	NSModule::LoadIfNeeded NSUpgrade

	if {$::DRY_RUN || $::BUILD} {
		InitResultWindow
		LogMessage "DRY_RUN TESTING MODE! NOTHING WILL HAPPEN"
		LogMessage "IF YOU SEE THIS MESSAGE CHECK THE WEB SITE FOR AN UPDATE"
		LogMessage "CONTACT THE AUTHOR IF NO UPDATE IS AVAILABLE"
	}

	update

	NSModule::LoadIfNeeded NSWindow

	global argv
	foreach {option value} $argv {
		switch -- $option {
			-dstdir {
				Global dir,Dst $value
				set win [Global win]
				$win.frameDst.entryDst delete 0 end
				$win.frameDst.entryDst insert 0 [file nativename $value]
				# FIXME: 8.4 doesn't need this
				$win.frameDst.entryDst configure -state disabled \
					-background SystemButtonFace
				$win.frameDst.buttonDst configure -state disabled
			}
		}
	}

	Global logFileId ""

	return
}

namespace eval CvtValue {
	proc Value v {
		return C:/Foo/Bar
	}
}

# Set to 1 ala UpgradeBuilder
set BUILD 0

# Misc debug
set DEBUG 1

# Set to 1 for no-write-files
set DRY_RUN 1

# Set to 1 for testsuite
set TEST 1

wm withdraw .

Main

# Run the testsuite
if {$TEST} {
	NSModule::LoadIfNeeded NSTest
	NSTest::RunTests
}
