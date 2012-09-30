# File: errorInfo.tcl

# Purpose: errorInfo display for debugging

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

proc tracecmd {text name1 name2 op} {

	global traceId
	global errorInfo
	global traceInfo
	global traceText

	set errorString $errorInfo
	if {![winfo exists $text]} {
		trace vdelete errorInfo w "tracecmd $text"
		return
	}
	
	# Hack - ignore the warnings about not finding the
	# file tclIndex.tcl  They are harmless.
	if [string match "*tclIndex*" $errorString] {
		return
	}
	
	set length [string length $traceInfo]
	set string [string range $errorString 0 [expr {$length - 1}]]
	set newline 1
	if {[string equal $traceInfo $string]} {
		set errorString [string range $errorString $length end]
		set newline 0
	}
	if {[string equal $errorString $traceInfo]} return
	set traceInfo $errorString

	if {$newline} {
		append traceText "\n\n$errorString"
	} else {
		append traceText $errorString
	}
	if {![string length $traceId]} {
		set traceId [after idle traceflush $text]
	}

	return
}

proc traceflush {text} {

	global traceId
	global traceText
	global ErrorText
	
	append ErrorText $traceText

	$text insert end $traceText
	scan [$text index end] %d.%d line char
	if {$line > 1000} {
		$text delete 1.0 [expr {$line - 1000}].0
	}
	$text see "end linestart"
	
	set traceId ""
	set traceText ""

	return
}

proc tracewindow {} {

	set win .errors
	toplevel $win
	wm title $win errorInfo

	frame $win.textFrame \
		-relief sunken -borderwidth 1

	scrollbar $win.yscroll \
		-orient vertical -command [list $win.text yview] \
		-takefocus 1

	scrollbar $win.xscroll \
		-orient horizontal -command [list $win.text xview] \
		-takefocus 1

	if {[Platform unix]} {
		set font {Courier 12}
	}
	if {[Platform windows]} {
		set font {Courier 9}
	}

	text $win.text \
		-yscrollcommand [list $win.yscroll set] -wrap none \
		-xscrollcommand [list $win.xscroll set] \
		-width 82 -height 30 -font $font -borderwidth 0 \
		-setgrid no -highlightthickness 0 -padx 4 -pady 2 \
		-background Black -foreground White

	pack $win.textFrame \
		-expand yes -fill both


	grid rowconfig $win.textFrame 0 -weight 1 -minsize 0
	grid columnconfig $win.textFrame 0 -weight 1 -minsize 0

	grid $win.text -in $win.textFrame \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.yscroll -in $win.textFrame \
		-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky news
	grid $win.xscroll -in $win.textFrame \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news

	return
}

proc tracesetup {} {

	global traceId
	global errorInfo
	global traceInfo
	global traceText
	global ErrorText

	tracewindow

	set win .errors
	set text $win.text

	trace variable errorInfo w "tracecmd $text"
	set traceId ""
	set traceInfo ""
	set traceText ""
	set ErrorText ""

	$text insert end "# Error Messages:\n\n"

	return
}

proc Debug {what} {

	if {![winfo exists .errors]} return

	if {![string equal [.errors.text get "end - 2 chars"] "\n"]} {
		.errors.text insert end \n
	}
	.errors.text insert end $what\n
	.errors.text see end

	return
}

tracesetup

wm iconify .errors
