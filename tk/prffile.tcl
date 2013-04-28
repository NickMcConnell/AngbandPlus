# File: prffile.tcl

# Purpose: .prf file manipulator

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#


if 0 {
set prefObj [NSObject::New NSPrfFile]
NSPrfFile::File $prefObj $path
NSPrfFile::Read $prefObj
NSPrfFile::Write $prefObj
NSPrfFile::MacroList $prefObj
NSPrfFile::MacroNew $prefObj $macroIndex
NSPrfFile::MacroDelete $prefObj $macroIndex
}


namespace eval NSPrfFile {
}

# NSPrfFile::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::InitModule {} {

	return
}

# NSPrfFile::NSPrfFile --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::NSPrfFile {oop} {

	Info $oop file ""
	Info $oop contents {}
	Info $oop macro,lines {}
	Info $oop keymap,lines,0 {}
	Info $oop keymap,lines,1 {}
	Info $oop modified 0
	Info $oop editor,init 0

	return
}

# NSPrfFile::!NSPrfFile --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::!NSPrfFile {oop} {

	if {[Info $oop editor,init]} {
		destroy [Info $oop editor,win]
	}

	return
}

# NSPrfFile::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::Info {oop info args} {

	global NSPrfFile

	# Verify the object
	NSObject::CheckObject NSPrfFile $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSPrfFile($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSPrfFile($oop,$info)
			}
		}
	}

	return
}

# NSPrfFile::File --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::File {oop args} {

	if {![llength $args]} {
		return [Info $oop file]
	}
	set newFile [lindex $args 0]
	set oldFile [Info $oop file]

	Info $oop file $newFile
	Info $oop contents {}
	Info $oop macro,lines {}
	Info $oop keymap,lines,0 {}
	Info $oop keymap,lines,1 {}
	Info $oop modified 0

	return
}

# NSPrfFile::Read --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::Read {oop} {

	set file [Info $oop file]

	if {$file eq ""} {
		error "no file"
	}

	set chan [open $file]
	set contents [split [read $chan] \n]
	close $chan

	Info $oop contents $contents
	ScanMacros $oop
	ScanKeymaps $oop 0
	ScanKeymaps $oop 1
	Info $oop modified 0

	return
}

# NSPrfFile::Write --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::Write {oop} {

	set file [Info $oop file]
	if {$file eq ""} {
		error "no file"
	}

	set tempName [NSUtils::TempFileName [file dirname $file]]
	if {[catch {openlf $tempName} chan]} {
		set msg "The following error occurred while attempting to open\n"
		append msg "$file\nfile for writing:\n\n$chan"
		tk_messageBox -title Oops -message $msg
		return
	}

	puts $chan [join [Info $oop contents] \n]

	close $chan

	set fileName [NSUtils::ReadLink $file]
	if {[file exists $fileName]} {
		file rename -force -- $fileName $fileName.bak
	}
	file rename -- $tempName $fileName


	Info $oop modified 0

	return
}

# NSPrfFile::ScanMacros --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::ScanMacros {oop} {

	set macroLines {}

	set lineA -1
	set line -1
	foreach string [Info $oop contents] {
		incr line
		if {$string eq ""} continue
		if {[string match #* $string]} continue
		switch -- [string range $string 0 1] {
			A: {
				set lineA $line
			}
			P: {
				# Require A: line to immediately preceed P: line
				if {$lineA == $line - 1} {
					lappend macroLines $lineA
				}
			}
		}
	}

	Info $oop macro,lines $macroLines

	return
}

# NSPrfFile::ScanKeymaps --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::ScanKeymaps {oop mode} {

	set keymapLines {}
	set lineA -1
	set line -1
	foreach string [Info $oop contents] {
		incr line
		if {$string eq ""} continue
		if {[string match #* $string]} continue
		switch -glob -- $string {
			A:* {
				set lineA $line
			}
			C:[0-9]:* {
				if {[string index $string 2] eq $mode} {
					# Require A: line to immediately preceed C: line
					if {$lineA == $line - 1} {
						lappend keymapLines $lineA
					}
				}
			}
		}
	}

	Info $oop keymap,lines,$mode $keymapLines

	return
}

# NSPrfFile::DeleteLines --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::DeleteLines {oop first last} {

	set contents [Info $oop contents]

	for {set i $first} {$i <= $last} {incr i} {
#		puts "NSPrfFile::DeleteLines: deleting line \"[lindex $contents $i]\""
	}
	set contents [lreplace $contents $first $last]

	set count [expr {$last - $first + 1}]

	set i 0
	set delete {}
	foreach lineA [Info $oop macro,lines] {
		if {$lineA >= $first && $lineA <= $last} {
			lappend delete $i
		}
		if {$lineA > $first} {
			lset ::NSPrfFile($oop,macro,lines) $i [expr {$lineA - $count}]
		}
		incr i
	}
	foreach i [lsort -integer -decreasing $delete] {
		Info $oop macro,lines [lreplace [Info $oop macro,lines] $i $i]
	}

	foreach mode {0 1} {
		set i 0
		set delete {}
		foreach lineA [Info $oop keymap,lines,$mode] {
			if {$lineA >= $first && $lineA <= $last} {
				lappend delete $i
			}
			if {$lineA > $first} {
				lset ::NSPrfFile($oop,keymap,lines,$mode) $i [expr {$lineA - $count}]
			}
			incr i
		}
		foreach i [lsort -integer -decreasing $delete] {
			Info $oop keymap,lines,$mode [lreplace [Info $oop keymap,lines,$mode] $i $i]
		}
	}

	Info $oop contents $contents
	Info $oop modified 1

	return
}

# NSPrfFile::MacroList --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::MacroList {oop} {

	set contents [Info $oop contents]

	set macroList {}
	foreach lineA [Info $oop macro,lines] {
		set lineP [expr {$lineA + 1}]
		set action [string range [lindex $contents $lineA] 2 end]
		set keypress [string range [lindex $contents $lineP] 2 end]
		lappend macroList $keypress $action
	}
	return $macroList
}

# NSPrfFile::MacroLines --
#
#	Return the line numbers of the A: and P: for a macro.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::MacroLines {oop index} {

	if {$index < 0 || $index >= [llength [Info $oop macro,lines]]} {
		error "no such macro \"$index\""
	}

	set lineA [lindex [Info $oop macro,lines] $index]
	return [list $lineA [expr {$lineA + 1}]]
}

# NSPrfFile::MacroGet --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::MacroGet {oop index} {

	set lines [MacroLines $oop $index]
	set lineA [lindex $lines 0]
	set lineP [lindex $lines 1]

	set action [lindex [Info $oop contents] $lineA]
	set keypress [lindex [Info $oop contents] $lineP]

	set action [string range $action 2 end]
	set keypress [string range $keypress 2 end]

	return [list $keypress $action]
}

# NSPrfFile::MacroDelete --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::MacroDelete {oop index} {

	set lines [MacroLines $oop $index]
	set lineA [lindex $lines 0]
	set lineP [lindex $lines 1]

	DeleteLines $oop $lineA $lineP

	return
}

# NSPrfFile::MacroModify --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::MacroModify {oop index keypress action} {

	set lines [MacroLines $oop $index]
	set lineA [lindex $lines 0]
	set lineP [lindex $lines 1]

	set contents [Info $oop contents]

	lset contents $lineA A:$action
	lset contents $lineP P:$keypress

	Info $oop contents $contents
	Info $oop modified 1

	return
}

# NSPrfFile::MacroKeypress --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::MacroKeypress {oop index args} {

	set lines [MacroLines $oop $index]
	set lineP [lindex $lines 1]

	set contents [Info $oop contents]
	if {[llength $args]} {
		set keypress [lindex $args 0]
		lset contents $lineP P:$keypress
		Info $oop contents $contents
		Info $oop modified 1
	}

	return [string range [lindex $contents $lineP] 2 end]
}

# NSPrfFile::MacroAction --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::MacroAction {oop index args} {

	set lines [MacroLines $oop $index]
	set lineA [lindex $lines 0]

	set contents [Info $oop contents]
	if {[llength $args]} {
		set keypress [lindex $args 0]
		lset contents $lineA A:$keypress
		Info $oop contents $contents
		Info $oop modified 1
	}

	return [string range [lindex $contents $lineA] 2 end]
}

# NSPrfFile::MacroNew --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::MacroNew {oop args} {

	set contents [Info $oop contents]

	if {[llength $args] == 2} {
		set keypress [lindex $args 0]
		set action [lindex $args 1]
	} else {
		set keypress {^_???\r}
		set action {\m}
	}
	lappend contents {} A:$action P:$keypress

	Info $oop contents $contents
	lappend ::NSPrfFile($oop,macro,lines) [expr {[llength $contents] - 2}]
	Info $oop modified 1

	return [expr {[llength [Info $oop macro,lines]]  - 1}]
}

# NSPrfFile::KeymapList --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::KeymapList {oop} {

	if {[Setting rogue_like_commands]} {
		set mode 1
	} else {
		set mode 0
	}

	set contents [Info $oop contents]

	set keymapList {}
	foreach lineA [Info $oop keymap,lines,$mode] {
		set lineC [expr {$lineA + 1}]
		set action [string range [lindex $contents $lineA] 2 end]
		set keypress [string range [lindex $contents $lineC] 4 end]
		lappend keymapList $keypress $action
	}
	return $keymapList
}

# NSPrfFile::KeymapLines --
#
#	Return the line numbers of the A: and C: for a keymap.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::KeymapLines {oop index} {

	if {[Setting rogue_like_commands]} {
		set mode 1
	} else {
		set mode 0
	}

	if {$index < 0 || $index >= [llength [Info $oop keymap,lines,$mode]]} {
		error "no such keymap \"$index\""
	}

	set lineA [lindex [Info $oop keymap,lines,$mode] $index]
	return [list $lineA [expr {$lineA + 1}]]
}

# NSPrfFile::KeymapDelete --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::KeymapDelete {oop index} {

	set lines [KeymapLines $oop $index]
	set lineA [lindex $lines 0]
	set lineC [lindex $lines 1]

	DeleteLines $oop $lineA $lineC

	return
}

# NSPrfFile::KeymapKeypress --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::KeymapKeypress {oop index args} {

	if {[Setting rogue_like_commands]} {
		set mode 1
	} else {
		set mode 0
	}

	set lines [KeymapLines $oop $index]
	set lineC [lindex $lines 1]

	set contents [Info $oop contents]
	if {[llength $args]} {
		set keypress [lindex $args 0]
		lset contents $lineC C:$mode:$keypress
		Info $oop contents $contents
		Info $oop modified 1
	}

	return [string range [lindex $contents $lineC] 4 end]
}

# NSPrfFile::KeymapAction --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::KeymapAction {oop index args} {

	set lines [KeymapLines $oop $index]
	set lineA [lindex $lines 0]

	set contents [Info $oop contents]
	if {[llength $args]} {
		set keypress [lindex $args 0]
		lset contents $lineA A:$keypress
		Info $oop contents $contents
		Info $oop modified 1
	}

	return [string range [lindex $contents $lineA] 2 end]
}

# NSPrfFile::KeymapNew --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::KeymapNew {oop args} {

	if {[Setting rogue_like_commands]} {
		set mode 1
	} else {
		set mode 0
	}

	set contents [Info $oop contents]

	if {[llength $args] == 2} {
		set keypress [lindex $args 0]
		set action [lindex $args 1]
	} else {
		set keypress ?
		set action {\m}
	}
	lappend contents {} A:$action C:$mode:$keypress

	Info $oop contents $contents
	lappend ::NSPrfFile($oop,keymap,lines,$mode) [expr {[llength $contents] - 2}]
	Info $oop modified 1

	return [expr {[llength [Info $oop keymap,lines,$mode]]  - 1}]
}







# NSPrfFile::ScanIncludes --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::ScanIncludes {file parent} {

	lappend includes $file $parent

	if {[file exists [Path lib pref $file]]} {
		set path [Path lib pref $file]
	} elseif {[file exists [Path lib user $file]]} {
		set path [Path lib user $file]
	} else {
		return $includes
	}

	set chan [open $path]
	set contents [split [read $chan] \n]
	close $chan

	namespace eval Expr {
		variable CLASS [angband player class]
		variable RACE [angband player race]
		switch -- [Platform] {
			unix {
				variable SYS tk-x11
			}
			windows {
				variable SYS tk-win
			}
		} 
	}

	set bypass 0
	foreach string $contents {
		if {$string eq ""} continue
		if {[string match #* $string]} continue
		switch -- [string range $string 0 1] {
			?: {
				set expr [namespace eval Expr "set x [string range $string 2 end]"]
				set bypass [expr {!$expr}]
			}
			%: {
				if {$bypass} continue
				set file2 [string range $string 2 end]
				set includes [concat $includes [ScanIncludes $file2 $file]]
			}
		}
	}

	return $includes
}

namespace eval NSPrfFile::Expr {
proc IOR {args} {
	foreach arg $args {
		if {$arg} {
			return 1
		}
	}
	return 0
}
proc AND {args} {
	foreach arg $args {
		if {!$arg} {
			return 0
		}
	}
	return 1
}
proc NOT {args} {
	foreach arg $args {
		if {$arg} {
			return 0
		}
	}
	return 1
}
proc EQU {args} {
	set t [lindex $args 0]
	foreach arg [lrange $args 1 end] {
		set p $t
		set t $arg
		if {$p ne $t} {
			return 0
		}
	}
	return 1
}
proc LEQ {args} {
	set t [lindex $args 0]
	foreach arg [lrange $args 1 end] {
		set p $t
		set t $arg
		if {[string compare $p $t] >= 0} {
			return 0
		}
	}
	return 1
}
proc GEQ {args} {
	set t [lindex $args 0]
	foreach arg [lrange $args 1 end] {
		set p $t
		set t $arg
		if {[string compare $p $t] <= 0} {
			return 0
		}
	}
	return 1
}

}

# NSPrfFile::EditorInit --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::EditorInit {oop} {

	set win .prfEditor$oop
	catch {destroy $win}
	toplevel $win
#	wm title $win [mc title-editor]
	wm withdraw $win

	set frame $win.frame
	frame $frame \
		-borderwidth 0
	text $frame.text -font [Value font,fixed] \
		-width 80 -height 24 \
		-xscrollcommand "$frame.xscroll set" \
		-yscrollcommand "$frame.yscroll set"
	scrollbar $frame.xscroll -command "$frame.text xview" -orient horizontal
	scrollbar $frame.yscroll -command "$frame.text yview" -orient vertical

	grid columnconfigure $frame 0 -weight 1
	grid columnconfigure $frame 1 -weight 0
	grid rowconfigure $frame 0 -weight 1
	grid rowconfigure $frame 1 -weight 0
	grid $frame.text -column 0 -row 0 -sticky news
	grid $frame.xscroll -column 0 -row 1 -sticky ew
	grid $frame.yscroll -column 1 -row 0 -sticky ns

	pack $frame -expand yes -fill both

	Info $oop editor,wText $frame.text
	Info $oop editor,win $win
	Info $oop editor,init 1

	return
}

# NSPrfFile::EditorEdit --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfFile::EditorEdit {oop parent} {

	if {![Info $oop editor,init]} {
		EditorInit $oop
	}

	set win [Info $oop editor,win]
	TransientToWin $win $parent

	set wText [Info $oop editor,wText]
	$wText delete 1.0 end
	$wText insert end [join [Info $oop contents] \n]

	WindowPosition $win 2 3

	return
}
