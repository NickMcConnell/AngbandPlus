# File: errorInfo.tcl

# Purpose: errorInfo display for debugging

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval ErrorInfo {
	variable Priv
}

proc ErrorInfo::TraceCmd {text name1 name2 op} {

	global errorInfo errorCode
	variable Priv

	set errorString $errorInfo
	set appendString $errorString
	if {![winfo exists $text]} {
		trace remove variable errorInfo write "ErrorInfo::tracecmd $text"
		return
	}

	if {$errorString eq $Priv(info)} {
		set Priv(info) ""
	}
	set newline "\n\n"
	if {$Priv(info) ne ""} {
		set length [string length $Priv(info)]
		set string [string range $errorString 0 [expr {$length - 1}]]
		if {$Priv(info) eq $string} {
			set appendString [string range $errorString [expr {$length + 0}] end]
			set newline ""
		}
	}
	if {$errorString eq $Priv(info)} {
#		return
	}
	set Priv(info) $errorString

	append Priv(string) "$newline$appendString"
	if {$Priv(afterId) eq ""} {
		set Priv(afterId) [after 50 ErrorInfo::Flush $text]
	}

	return
}

proc ErrorInfo::Flush {text} {

	variable Priv

	$text insert end $Priv(string)
	scan [$text index end] %d.%d line char
	if {$line > 1000} {
		$text delete 1.0 [expr {$line - 1000}].0
	}
	$text see "end - 1 lines linestart"

	set Priv(afterId) ""
	set Priv(string) ""

	return
}

proc ErrorInfo::InitWindow {} {

	variable Priv

	set win .errors
	toplevel $win
	wm title $win errorInfo

	set frame $win.textFrame
	frame $frame \
		-relief sunken -borderwidth 1

	scrollbar $frame.yscroll \
		-orient vertical -command [list $frame.text yview] \
		-takefocus 1

	scrollbar $frame.xscroll \
		-orient horizontal -command [list $frame.text xview] \
		-takefocus 1

	SynchScrollBar $frame.text $frame.yscroll 0
	SynchScrollBar $frame.text $frame.xscroll 0

	if {[Platform unix]} {
		set font {Courier 12}
	}
	if {[Platform windows]} {
		set font {Courier 9}
	}

	text $frame.text \
		-yscrollcommand [list $frame.yscroll set] -wrap none \
		-xscrollcommand [list $frame.xscroll set] \
		-width 82 -height 30 -font $font -borderwidth 0 \
		-setgrid no -highlightthickness 0 -padx 4 -pady 2 \
		-background Black -foreground White

	pack $win.textFrame \
		-expand yes -fill both

	grid rowconfig $win.textFrame 0 -weight 1 -minsize 0
	grid columnconfig $win.textFrame 0 -weight 1 -minsize 0

	grid $frame.text \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $frame.yscroll \
		-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky news
	grid $frame.xscroll \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news

	set Priv(win) $win
	set Priv(wText) $frame.text

	return
}

proc ErrorInfo::Init {} {

	global errorInfo
	variable Priv

	InitWindow

	set win $Priv(win)
	set text $Priv(wText)
	trace add variable errorInfo write "ErrorInfo::TraceCmd $text"
	set Priv(afterId) ""
	set Priv(info) ""
	set Priv(string) ""

	$text insert end "# IGNORE errors about tclIndex. It is okay.\n"
	$text insert end "# (If it is not okay, then ignore this message. ;-\})\n\n"

	return
}

proc Debug {what} {

	if {![winfo exists .errors]} return

	set wText $ErrorInfo::Priv(wText)
	if {[$wText get "end - 2 chars"] ne "\n"} {
		$wText insert end \n
	}
	$wText insert end $what\n
	$wText see "end - 1 lines linestart"

	return
}

if 0 {

set errorInfo {}

rename angband angband_old
proc angband {args} {
	Debug "angband $args"
	eval angband_old $args
}

}

# Copied from NSUtils::SynchScrollBar
proc ErrorInfo::SynchScrollBar {window scrollbar {moveto 0}} {

	if {$::tcl_platform(platform) ne "windows"} return

	set tag SynchScrlBarBindTag$scrollbar
	bindtags $scrollbar [concat [bindtags $scrollbar] $tag]

	switch -- [$scrollbar cget -orient] {
		horizontal {
			set view xview
		}
		vertical {
			set view yview
		}
	}

	bind $tag <Map> "
		$scrollbar set {*}\[$window $view]
		if {$moveto} {
			$window $view moveto 0.0
		}
	"

	bind $tag <Destroy> "
		bind $tag <Map> {}
		bind $tag <Destroy {}
	"

	return
}

# Forget this... use the console
proc InitCommandWindow {} {

	global CommandHistory

	set win .command

	toplevel $win
	wm title $win Command

	entry $win.entry -width 60

	bind $win.entry <Return> {
		lappend CommandHistory(line) [%W get]
		if {[incr CommandHistory(count)] > 20} {
			set CommandHistory(line) [lrange $CommandHistory(line) \
				[expr {$CommandHistory(count) - 20}] end]
			set CommandHistory(count) [llength $CommandHistory(line)]
		}
		set CommandHistory(index) $CommandHistory(count)
		catch {eval [%W get]} result
		%W delete 0 end
		%W insert 0 $result
	}

	bind $win.entry <KeyPress-Up> {
		if {$CommandHistory(index) > 0} {
			incr CommandHistory(index) -1
		}
		%W delete 0 end
		%W insert 0 [lindex $CommandHistory(line) $CommandHistory(index)]
	}

	bind $win.entry <KeyPress-Down> {
		if {$CommandHistory(index) < $CommandHistory(count) - 1} {
			incr CommandHistory(index)
		}
		%W delete 0 end
		%W insert 0 [lindex $CommandHistory(line) $CommandHistory(index)]
	}

	pack $win.entry -side top -expand yes -fill x

	set CommandHistory(line) {}
	set CommandHistory(count) 0
	set CommandHistory(index) 0

	focus $win.entry

	return
}

ErrorInfo::Init
# InitCommandWindow

