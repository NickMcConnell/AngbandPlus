# File: font.tcl

# Purpose: the Font Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSFont {

	variable Priv

# namespace eval NSFont
}

# NSFont::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSFont::InitModule {} {

	variable Priv

	set Priv(items) {}
	lappend Priv(items) autobar
	lappend Priv(items) choice
	lappend Priv(items) inventory
	lappend Priv(items) knowledge
	lappend Priv(items) magic
	lappend Priv(items) message
	lappend Priv(items) messages
	lappend Priv(items) misc
	lappend Priv(items) miscPopup
	lappend Priv(items) monster
	lappend Priv(items) options
	lappend Priv(items) recall
	lappend Priv(items) status
	lappend Priv(items) statusBar
	lappend Priv(items) store

	set oop [NSObject::New NSFont]

	# Select the first item
	[Info $oop win].frameList.list selection set 0
	ItemSelectionChanged $oop

	return
}

# NSFont::NSFont --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSFont::NSFont {oop} {

	# Create the toplevel
	InitWindow $oop

	# Get the toplevel
	set win [Info $oop win]

	# Register toplevel with NSWindowManager
	NSWindowManager::RegisterWindow font $win \
		"GetDefaultGeometry $win reqwidth reqheight" "" \
		"NSFont::DisplayCmd $oop"

	#
	# Global list of application windows
	#

	Global font,oop $oop
	Window font $win

	return
}

# NSFont::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSFont::Info {oop info args} {

	global NSFont

	# Verify the object
	NSObject::CheckObject NSFont $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSFont($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSFont($oop,$info)
			}
		}
	}

	return
}

# NSFont::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSFont::InitWindow {oop} {

	variable Priv
	
	set win .font$oop
	toplevel $win
	wm title $win "Font"

	wm resizable $win no no
	wm transient $win [Window main]

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSFont::Close $oop"

	# Remember the toplevel
	Info $oop win $win

	# Create the menus
	InitMenus $oop

	#
	# Divider
	#

	MakeDivider $win.divider2 x

	#
	# List of items
	#

	set frame $win.frameList
	frame $frame \
		-borderwidth 1 -relief sunken
	listbox $frame.list \
		-height 5 -width 35 -background White -borderwidth 0 \
		-yscrollcommand "$frame.yscroll set" -highlightthickness 0 \
		-exportselection no -selectmode extended
	scrollbar $frame.yscroll \
		-command "$frame.list yview"

	grid rowconfigure $frame 0 -weight 1
	grid columnconfigure $frame 0 -weight 1
	grid columnconfigure $frame 1 -weight 0
	grid $frame.list \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $frame.yscroll \
		-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky ns

	foreach item $Priv(items) {
		$frame.list insert end $item
	}

	# Configure the display when the item changes
	bind $frame.list <<ListboxSelect>> \
		"NSFont::ItemSelectionChanged $oop"

	#
	# Font face, style, and size
	#

	frame $win.frameFont \
		-borderwidth 0
	InitListPlusLabel $oop $win.frameFont.face "Font:" 20
	InitListPlusLabel $oop $win.frameFont.size "Size:" 4

	pack $win.frameFont.face -side left -padx 4
	pack $win.frameFont.size -side left -padx 4

	foreach family [lsort -dictionary [font families]] {
		$win.frameFont.face.frameList.list insert end $family
	}
	foreach size [list 8 9 10 11 12 14 16 18 20 22 24 26 28 36 48 72] {
		$win.frameFont.size.frameList.list insert end $size
	}

	#
	# Style checkbuttons
	#

	set frame $win.frameFont.frameStyle
	frame $frame \
		-borderwidth 0
	label $frame.label \
		-text "Style:" -anchor w
	frame $frame.frameCheck \
		-borderwidth 2 -relief groove
	checkbutton $frame.frameCheck.weight \
		-text "Bold" -variable NSFont($oop,weight) \
		-onvalue bold -offvalue normal \
		-command "NSFont::SynchFont $oop"
	checkbutton $frame.frameCheck.slant \
		-text "Italic" -variable NSFont($oop,slant) \
		-onvalue italic -offvalue roman \
		-command "NSFont::SynchFont $oop"

	pack $frame.label -side top -anchor w
	pack $frame.frameCheck.weight -side top -anchor w
	pack $frame.frameCheck.slant -side top -anchor w
	pack $frame.frameCheck -side top -anchor nw
	pack $frame -side left -anchor nw -padx 4
	
	#
	# Sample text
	#

	frame $win.frameSample \
		-borderwidth 2 -relief sunken -width 200 -height 60
	text $win.frameSample.text \
		-background [Global SystemButtonFace] -borderwidth 0 -wrap word

	pack propagate $win.frameSample no
	pack $win.frameSample.text

	$win.frameSample.text tag configure FontTag -justify center
	$win.frameSample.text insert end "The quick brown fox jumped over the lazy dog." FontTag
	$win.frameSample.text configure -state disabled

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar
#	frame $win.statusBar -relief flat -borderwidth 0
#	label $win.statusBar.label -anchor w -relief sunken -padx 2
#	pack $win.statusBar.label -side left -expand yes -fill both

	#
	# Geometry
	#

	grid rowconfig $win 0 -weight 0
	grid rowconfig $win 1 -weight 1
	grid rowconfig $win 2 -weight 0
	grid rowconfig $win 3 -weight 0
	grid rowconfig $win 4 -weight 0
	grid columnconfig $win 0 -weight 1
 
	if {[Platform windows]} {
		grid $win.divider2 \
			-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	}
	grid $win.frameList \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news -padx 2 -pady 2
	grid $win.frameFont \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.frameSample \
		-row 3 -column 0 -rowspan 1 -columnspan 1 -sticky ew -padx 10 -pady 4
	grid $win.statusBar \
		-row 4 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	#
	# KeyPress bindings
	#

	bind $win <KeyPress-Escape> "NSFont::Close $oop"
	bind $win <Control-KeyPress-w> "NSFont::Close $oop"

	return
}

# NSFont::InitMenus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSFont::InitMenus {oop} {

	# Get the toplevel
	set win [Info $oop win]

	# Default accelerator modifier
	set mod "Ctrl"

	#
	# Menu bar
	#

	Info $oop mbar [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSFont::SetupMenus $oop" -identifier MENUBAR]
	set mbar [Info $oop mbar]

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSFont::MenuInvoke $oop"

	#
	# Font Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_FONT
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_FONT -label "Font" -underline 0 -identifier M_FONT

	set entries {}
	lappend entries [list -type command -label "Close" \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_FONT $entries

	return
}

# NSFont::SetupMenus --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSFont::SetupMenus {oop mbarId} {

	lappend identList E_CLOSE

	NSMenu::MenuEnable $mbarId $identList

	return
}

# NSFont::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSFont::MenuInvoke {oop menuId ident} {

	switch -glob -- $ident {
		E_CLOSE {Close $oop}
	}

	return
}

# NSFont::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSFont::DisplayCmd {oop message first args} {

	variable Priv

	switch -- $message {
		preDisplay -
		reDisplay {
			if {[llength $args]} {
				set listbox [Info $oop win].frameList.list
				set row [lsearch -exact $Priv(items) [lindex $args 0]]
				$listbox selection clear 0 end
				$listbox selection set $row
				$listbox see $row
				ItemSelectionChanged $oop
			}
		}
		postDisplay {
		}
	}

	return
}

# NSFont::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSFont::Close {oop} {

	NSWindowManager::Undisplay font

	return
}

# NSFont::InitListPlusLabel --
#
#	Creates a frame with a label, listbox and scrollbar.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSFont::InitListPlusLabel {oop parent title width} {

	frame $parent \
		-borderwidth 0
		
	label $parent.title \
		-text $title

	set frame $parent.frameList
	frame $frame \
		-borderwidth 1 -relief sunken
	listbox $frame.list \
		-height 7 -width $width -background White -borderwidth 0 \
		-yscrollcommand "$frame.yscroll set" -highlightthickness 0 \
		-exportselection no
	scrollbar $frame.yscroll \
		-command "$frame.list yview"

	#
	# List geometry
	#
	
	grid rowconfigure $frame 0 -weight 1
	grid columnconfigure $frame 0 -weight 1
	grid columnconfigure $frame 1 -weight 0
	grid $frame.list \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $frame.yscroll \
		-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky ns

	#
	# Geometry
	#
	
	grid rowconfigure $parent 0 -weight 0
	grid rowconfigure $parent 1 -weight 1
	grid columnconfigure $frame 0 -weight 1
	grid $parent.title \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky w
	grid $parent.frameList \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky w

	# Update the font when the selection changes
	bind $parent.frameList.list <<ListboxSelect>> \
		"NSFont::SynchFont $oop"

	return
}

# NSFont::ListSelectValue --
#
#	Looks for a given value in a listbox, selects it, and scrolls to it.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSFont::ListSelectValue {oop parent value} {

	set listbox $parent.frameList.list

	set index 0
	foreach item [$listbox get 0 end] {
		if {[string equal $item $value]} {
			$parent.frameList.list selection clear 0 end
			$parent.frameList.list selection set $index
			$parent.frameList.list see $index
			break
		}
		incr index
	}

	return
}

# NSFont::SynchFont --
#
#	Called when the font face, size, or style changes. Updates the
#	sample text with the new font, and sets the "item" font.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSFont::SynchFont {oop} {

	variable Priv
	
	set win [Info $oop win]

	set selection [$win.frameList.list curselection]
	set count [llength $selection]
	if {$count == 0} return
	
	set row [$win.frameFont.face.frameList.list curselection]
	if {![string length $row]} return
	set family [$win.frameFont.face.frameList.list get $row]
	set row [$win.frameFont.size.frameList.list curselection]
	if {![string length $row]} return
	set size [$win.frameFont.size.frameList.list get $row]

	set weight [Info $oop weight]
	set slant [Info $oop slant]

	# Set the font description
	set font [list -family $family -size $size -weight $weight -slant $slant]

	# Update the sample text
	$win.frameSample.text tag configure FontTag -font $font

	# Update each "item" font, if asked
	if {![Info $oop noconfigure]} {
		foreach row $selection {
			set item [lindex $Priv(items) $row]
			Value font,$item $font
		}
	}

	return
}

# NSFont::ItemSelectionChanged --
#
#	Sets the font controls and sample text with the item's font.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSFont::ItemSelectionChanged {oop} {

	variable Priv

	set win [Info $oop win]

	set selection [$win.frameList.list curselection]
	set count [llength $selection]

	set synchFont 1
	
	$win.frameSample.text configure -state normal
	$win.frameSample.text delete 1.0 end

	if {$count >= 1} {
		set row [lindex $selection 0]
		set item [lindex $Priv(items) $row]
		set font [fontdesc [Value font,$item]]
		foreach row [lrange $selection 1 end] {
			set item [lindex $Priv(items) $row]
			set font2 [fontdesc [Value font,$item]]
			if {[string compare $font2 $font]} {
				set synchFont 0
				break
			}
		}
		
	} else {
		set synchFont 0

		$win.frameFont.face.frameList.list selection clear 0 end
		$win.frameFont.size.frameList.list selection clear 0 end
	}

	if {$synchFont} {
		array set attrib [fontdesc $font]
		ListSelectValue $oop $win.frameFont.size $attrib(-size)
		ListSelectValue $oop $win.frameFont.face $attrib(-family)
		Info $oop weight $attrib(-weight)
		Info $oop slant $attrib(-slant)

		$win.frameSample.text insert end "The quick brown fox jumped over the lazy dog." FontTag

		Info $oop noconfigure 1
		SynchFont $oop
		Info $oop noconfigure 0
		
	} else {
		$win.frameFont.face.frameList.list selection clear 0 end
		$win.frameFont.size.frameList.list selection clear 0 end
	}
	
	$win.frameSample.text configure -state disabled

	if {$count == 1} {
		set string "1 item selected"
	} else {
		set string [format "%d items selected" $count]
	}
	$win.statusBar itemconfigure t1 -text $string

	return
}

