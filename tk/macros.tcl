# File: macros.tcl

# Purpose: the Macros Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSMacros {

	variable MenuString
	variable Priv

# namespace eval NSMacros
}

# NSMacros::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::InitModule {} {

	variable Priv

	MsgCatInit macro

	InitImageIfNeeded Image_Open open.gif
	InitImageIfNeeded Image_Save save.gif

	NSModule::LoadIfNeeded NSToolbar

	set Priv(oop) [NSObject::New NSMacros]
	set Priv(selectedMacro) -1

	return
}

# NSMacros::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::CloseModule {} {

	catch {
		destroy [Window macros]
	}

	return
}

# NSMacros::NSMacros --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::NSMacros {oop} {

	InitWindow $oop

	set win [Info $oop win]
	
	NSWindowManager::RegisterWindow macros $win \
		"GetDefaultGeometry $win reqwidth main" "" \
		"NSMacros::DisplayCmd $oop"

	# Update ourself when the font,macros value changes
	qebind NSMacros <Value-font,macros> \
		"NSMacros::ValueChanged_font_macros $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSMacros $oop $win

	#
	# Global list of application windows
	#

	Global macros,oop $oop
	Window macros $win

	return
}

# NSMacros::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::Info {oop info args} {

	global NSMacros

	# Verify the object
	NSObject::CheckObject NSMacros $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSMacros($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSMacros($oop,$info)
			}
		}
	}

	return
}

# NSMacros::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::InitWindow {oop} {

	set win .macros$oop
	toplevel $win
	wm title $win [mc Macros]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSMacros::Close $oop"

	Info $oop win $win

	InitMenus $oop

	#
	# Toolbar
	#

	set toolId [NSObject::New NSToolbar 20 $win]
	NSToolbar::AddTool $toolId -image Image_Open -label [mc Open] \
		-showlabel yes -command "NSMacros::MacroLoad $oop"
	NSToolbar::AddTool $toolId -image Image_Save -label [mc Save] \
		-showlabel yes -command "NSMacros::MacroDump $oop"

	NSStatusText::StatusText [NSToolbar::GetTool $toolId 1] \
		$win.statusBar.label0 \
		"Read settings from an existing preferences file."
	NSStatusText::StatusText [NSToolbar::GetTool $toolId 2] \
		$win.statusBar.label0 \
		"Append macros to a new or existing preferences file."

	#
	# Divider
	#

#	frame $win.divider2 \
#		-borderwidth 1 -height 2 -relief groove

	#
	# List
	#

	set font [Value font,macros]
	set cw [font measure $font "W"]
	set width [expr {$cw * 40}]
	set rowHgt [font metrics [Value font,macros] -linespace]
	incr rowHgt 8

	frame $win.frame \
		-borderwidth 1 -relief sunken
	set canvistId [NSObject::New NSCanvist $win.frame $rowHgt $width $width \
		"NSMacros::NewItemCmd $oop" "NSMacros::HighlightItemCmd $oop"]
	set canvas [NSCanvist::Info $canvistId canvas]
	$canvas configure -background [Value listBG]
	$canvas configure -yscrollcommand "$win.frame.scroll set"
	scrollbar $win.frame.scroll \
		-command "$canvas yview" -orient vert

	bind $canvas <Configure> "
		NSMacros::Configure $oop $canvas
	"

	Info $oop canvistId $canvistId

	pack $win.frame.scroll -side right -fill y
	pack $canvas -side left -expand yes -fill both

	NSCanvist::Info $canvistId selectionCmd \
		"NSMacros::SelectionChanged $oop"

	# This call updates the list background color whenever the
	# global list background color changes
	qebind NSMacros <Value-listBG> "ListBackgroundChanged $canvas"

	# Trigger and Action fields
	set frame $win.frameFields
	frame $frame \
		-borderwidth 0
	label $frame.labelTrigger \
		-text [mc Trigger:]
	label $frame.entryTrigger \
		-width 30 -borderwidth 2 -relief sunken -takefocus 1
	label $frame.labelAction \
		-text [mc Action:]
	entry $frame.entryAction \
		-width 30
	pack $frame.labelTrigger \
		-side left -padx 2 -pady 5
	pack $frame.entryTrigger \
		-side left -padx 2 -pady 5
	pack $frame.labelAction \
		-side left -padx 2 -pady 5
	pack $frame.entryAction \
		-side left -padx 2 -pady 5

	# When the Trigger field has the focus, typing a key sequence
	# changes the trigger for the selected macro.
	set entry $frame.entryTrigger
	bind $entry <Any-Enter> {focus %W}
	bind $entry <Any-Leave> "focus $canvas"
	bind $entry <FocusIn> {%W configure -background White}
	bind $entry <FocusOut> {%W configure -background [[winfo toplevel %W] cget -background]}
	bindtags $entry [list $entry MacroTrigger_BindTag]

	set entry $frame.entryAction
	bind $entry <KeyPress-Return> "NSMacros::SetMacroAction $oop"

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 20

	#
	# Geometry
	#

	grid rowconfig $win 0 -weight 0 -minsize 0
	grid rowconfig $win 1 -weight 1 -minsize 0
	grid rowconfig $win 2 -weight 0 -minsize 0
	grid rowconfig $win 3 -weight 0 -minsize 0
#	grid rowconfig $win 4 -weight 0 -minsize 0
	grid columnconfig $win 0 -weight 1 -minsize 0
 
	pack forget [NSToolbar::Info $toolId frame]
	grid [NSToolbar::Info $toolId frame] -in $win \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
#	grid $win.divider2 -in $win \
#		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.frame -in $win \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.frameFields -in $win \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.statusBar -in $win \
		-row 3 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	#
	# Context Menu
	#
	
	set m $win.context
	menu $m -tearoff 0
	bind $canvas <Button-3> "::NSMacros::ContextMenu $oop $m %X %Y"

	#
	# KeyPress bindings
	#

	bind $win <KeyPress-Escape> "NSMacros::Close $oop"
	bind $win <Control-KeyPress-w> "NSMacros::Close $oop"
	bind $win <KeyPress-n> "NSMacros::CreateMacro $oop"

	#
	# Synch the scrollbars when window is shown.
	#

#	bind $win.frame.scroll <Map> "NSMacros::SynchScrollBars $oop"
	NSUtils::SynchScrollBar $canvas $win.frame.scroll 1

	NSStatusText::StatusText $win.frameFields.entryTrigger \
		$win.statusBar.label0 \
		"Type a new trigger for the selected macro."
	NSStatusText::StatusText $win.frameFields.entryAction \
		$win.statusBar.label0 \
		"Enter an encoded action and hit Enter."

	return
}

# NSMacros::InitMenus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::InitMenus {oop} {

	variable MenuString

	# Default accelerator modifier
	set mod "Ctrl"

	set win [Info $oop win]

	#
	# Menu bar
	#

	Info $oop mbarId [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSMacros::SetupMenus $oop" -identifier MENUBAR]
	set mbar [Info $oop mbarId]

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSMacros::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSMacros::MenuInvoke $oop"

	#
	# Macro Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_MACRO
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_MACRO -label [mc Macro] -underline 0 -identifier M_MACRO

	set entries {}
	lappend entries [list -type command -label [mc "New Empty Macro"] \
		-underline 0 -accelerator n -identifier E_MACRO_NEW]
	lappend entries [list -type command -label [mc "Dump Macros"] \
		-underline 0 -identifier E_MACRO_DUMP]
	lappend entries [list -type command -label [mc "Load Pref File"] \
		-underline 0 -identifier E_MACRO_LOAD]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_MACRO $entries

	set MenuString(M_MACRO) \
		"Contains commands for using macros."
	set MenuString(E_MACRO_NEW) \
		"Creates a new macro."
	set MenuString(E_MACRO_DUMP) \
		"Appends macros to a new or existing preferences file."
	set MenuString(E_MACRO_LOAD) \
		"Read settings from an existing preferences file."
	set MenuString(E_CLOSE) \
		"Closes the window."

	return
}

# NSMacros::SetupMenus --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::SetupMenus {oop mbarId} {

	set canvistId [Info $oop canvistId]

	lappend identList E_MACRO_NEW E_MACRO_DUMP E_MACRO_LOAD E_CLOSE

	if {[llength [NSCanvist::Selection $canvistId]]} {
		lappend identList E_MACRO_ACTION
	}

	NSMenu::MenuEnable $mbarId $identList

	return
}

# NSMacros::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::MenuSelect {oop menuId index ident} {

	variable MenuString

	switch -- $ident {
		{} {
			set desc {}
		}
		
		default {
			if {[info exists MenuString($ident)]} {
				set desc $MenuString($ident)
			} else {
				set menu [NSMenu::Info $menuId menu]
				set desc [$menu entrycget $index -label]
			}
		}
	}

	set statusBar [Info $oop win].statusBar
	$statusBar cover set $desc
	if {($desc eq "") && ($menuId == [Info $oop mbarId])} {
		if {[$statusBar cover visible]} {
			$statusBar cover hide
		} else {
			$statusBar cover show
		}
	}

	return
}

# NSMacros::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::MenuInvoke {oop menuId ident} {

	variable Priv

	switch -glob -- $ident {
		E_MACRO_NEW {CreateMacro $oop}
		E_MACRO_DUMP {MacroDump $oop}
		E_MACRO_LOAD {MacroLoad $oop}
		E_CLOSE {Close $oop}
	}

	return
}

# NSMacros::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::DisplayCmd {oop message first} {

	set canvistId [Info $oop canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]

	switch -- $message {
		preDisplay {
			SetList $oop
		}
		postDisplay {
			focus $canvas
		}
	}

	return
}

# NSMacros::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::Close {oop} {

	angband keypress \033

	return
}

# NSMacros::ContextMenu --
#
#	When an macros item is right-clicked, pop up a context
#	menu of actions.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::ContextMenu {oop menu x y} {
}

# NSMacros::SetList --
#
#	Add all the defined macros to the list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::SetList {oop} {

	variable Priv

	set win [Info $oop win]
	set canvistId [Info $oop canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]
	set index $Priv(selectedMacro)

	# Clear the list
	NSCanvist::DeleteAll $canvistId

	set Priv(width,keypress) 0

	# Get number of macros
	set max [angband macro max]

	# Iterate over macros
	for {set i 0} {$i < $max} {incr i} {

		# Get the keypress
		set keypress [angband macro keypress $i]

		# Strip leading "_^" and trailing "\r" from keypress
		regexp {\^_(.+)\\r} $keypress ignore keypress

		# Get the action
		set action [angband macro action $i]

		# Hack -- Remove weird formatting

		# Append to the list
		NSCanvist::Insert $canvistId end $keypress $action
	}

	set offset [expr {$Priv(width,keypress) + 40}]
	$canvas move action $offset 0

	# Display total number of macros
	$win.statusBar itemconfigure t2 -text [format [mc "%d macros"] $max]

	if {$index != -1} {
		NSCanvist::UpdateSelection $canvistId $index ""
	}

	return
}

# NSMacros::CreateMacro --
#
#	Create a new macro.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::CreateMacro {oop} {

	variable Priv

	# Create a new empty macro.
	set index [angband macro create ^_NEW\\r]
	set Priv(selectedMacro) $index

	# Update the list
	SetList $oop

	# See the new macro
	NSCanvist::See [Info $oop canvistId] $index

	return
}

# NSMacros::SetMacroAction --
#
#	Update the action for the selected macro. The new action is taken
#	from the Action Entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::SetMacroAction {oop} {

	variable Priv

	if {$Priv(selectedMacro) == -1} return
	set index $Priv(selectedMacro)

	set win [Info $oop win]
	set canvistId [Info $oop canvistId]
	set entry $win.frameFields.entryAction

	# Set the action for the selected macro
	angband macro action $index [$entry get]

	# Update the list
	SetList $oop

	# See the updated macro
	NSCanvist::See $canvistId $index

	focus [NSCanvist::Info $canvistId canvas]

	return
}

# NSMacros::NewItemCmd --
#
#	Called by NSCanvist::InsertItem() to create a list row.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::NewItemCmd {oop canvistId y keypress action} {

	variable Priv
	
	set c [NSCanvist::Info $canvistId canvas]
	set lineHeight [NSCanvist::Info $canvistId rowHgt]
	set font [Value font,macros]

	set text $keypress

	set fh [font metrics $font -linespace]
	set diff [expr {int([expr {($lineHeight - $fh) / 2}])}]

	# Selection rectangle inside row
	lappend itemIdList [$c create rectangle 2 [expr {$y + 2}] \
		[expr {([winfo width $c] - 1) - 2}] [expr {$y + $lineHeight - 2}] \
		-fill "" -outline "" -tags {enabled selrect} -width 2.0]

	#
	# Keypress
	#

	lappend itemIdList [$c create text 4 [expr {$y + $diff}] \
		-text $keypress -anchor nw -font $font -fill White -tags enabled]

	#
	# Action
	#

	lappend itemIdList [$c create text 0 [expr {$y + $diff}] \
		-text $action -anchor nw -font $font -fill White -tags action]

	# Maximum width of keypress
	set width [font measure $font $keypress]
	if {$width > $Priv(width,keypress)} {
		set Priv(width,keypress) $width
	}

	return $itemIdList
}

# NSMacros::HighlightItemCmd --
#
#	Called by NSCanvist::Select() to highlight a row.
#
# Arguments:
#	oop					OOP ID. See above.
#	canvistId					OOP ID of NSCanvist object.
#	state					1 or 0 highlight state.
#	args					List of canvas item ids
#
# Results:
#	What happened.

proc NSMacros::HighlightItemCmd {oop canvistId state args} {

	set canvas [NSCanvist::Info $canvistId canvas]
	set itemIdList $args

	set idRect [FindItemByTag $canvas $itemIdList selrect]

	if {[NSUtils::HasFocus $canvas]} {
		set fill [Value listHilite]
	} else {
		set fill [Value listInactive]
	}

	if {$state} {
		$canvas itemconfigure $idRect -outline $fill

	} else {
		$canvas itemconfigure $idRect -outline {}
	}

	return
}


# The following bindings are used to get a macro trigger keypress
# from the user. They should be almost identical to those found in
# keyboard.tcl. The differences are that (1) the keypress passed
# a local command, not as an argument to "angband keypress"; and (2)
# the "wrapper" chars \037 and \015 are not written.

bind MacroTrigger_BindTag <Control-Shift-KeyPress> {

	NSMacros::SetMacroTrigger Control-Shift-%K
}

bind MacroTrigger_BindTag <Control-KeyPress> {

	# Special Control-KeyPress (ex Control-F1)
	if {![string length %A] || [string length %K] > 1} {
		NSMacros::SetMacroTrigger Control-%K

	# Ascii Control-KeyPress
	} else {
		NSMacros::SetMacroTrigger %A
	}
}

bind MacroTrigger_BindTag <Shift-KeyPress> {

	# Special Shift-KeyPress (ex Shift-F1)
	if {![string length %A]} {
		NSMacros::SetMacroTrigger Shift-%K

	# Ascii Shift-KeyPress
	} else {
		NSMacros::SetMacroTrigger %A
	}
}

bind MacroTrigger_BindTag <Alt-KeyPress> {

	NSMacros::SetMacroTrigger Alt-%K
}

bind MacroTrigger_BindTag <KeyPress> {

	# Special KeyPress (ex F1)
	if {![string length %A]} {
		NSMacros::SetMacroTrigger %K

	# Normal keys with no modifiers
	} else {
		NSMacros::SetMacroTrigger %A
	}
}

bind MacroTrigger_BindTag <Escape> {

	bell
}

bind MacroTrigger_BindTag <Return> {

	bell
}

bind MacroTrigger_BindTag <Tab> {

	bell
}

bind MacroTrigger_BindTag <BackSpace> {

	bell
}

bind MacroTrigger_BindTag <Delete> {

	bell
}

# Ignore modifiers by themselves
foreach mod1 {Control Shift Alt} {
	bind MacroTrigger_BindTag <${mod1}_L> { ; }
	bind MacroTrigger_BindTag <${mod1}_R> { ; }
	foreach mod2 {Control Shift Alt} {
		if {$mod1 eq $mod2} continue
		bind MacroTrigger_BindTag <$mod1-${mod2}_L> { ; }
		bind MacroTrigger_BindTag <$mod1-${mod2}_R> { ; }
		bind MacroTrigger_BindTag <$mod2-${mod1}_L> { ; }
		bind MacroTrigger_BindTag <$mod2-${mod1}_R> { ; }
	}
}

# NSMacros::MacroDump --
#
#	Get a filename from the user then append all macros to the given
#	file (or create a new file). The file goes inside the lib/user
#	directory.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::MacroDump {oop} {

	global Angband

	set parent [Info $oop win]
	set filename [tk_getSaveFile -initialfile [angband player base_name].prf \
		-initialdir [PathUser] -parent $parent]
	if {![string length $filename]} return

	if {![IsUserFile $filename]} {
		tk_messageBox -title "Pref File Error" -icon info -message \
			"Pref files must be saved in the lib/user directory."
		return
	}

	set filename [file tail $filename]
	if {[catch {angband game macro_dump $filename} result]} {
		tk_messageBox -title "Pref File Error" -icon error -message $result
	}

	return
}

# NSMacros::MacroLoad --
#
#	Get a filename from the user then read in the given pref file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::MacroLoad {oop} {

	# Let the user choose a file, and read it
	if {[ProcessPrefFile [Info $oop win]]} return

	# Update the list
	SetList $oop

	return
}

# NSMacros::SetMacroTrigger --
#
#	When the Trigger Entry has the focus, and the user types a
#	key sequence, the trigger keypress for the selected macro
#	is updated.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::SetMacroTrigger {trigger} {

	variable Priv

	if {$Priv(selectedMacro) == -1} return

	set oop $Priv(oop)
	set win [Info $oop win]
	set canvistId [Info $oop canvistId]
	set index $Priv(selectedMacro)

	# Attempt to change the trigger for the selected macro. If another
	# macro is already assigned the same trigger, just tell the user
	# about it and return.
	set index2 [angband macro keypress $index ^_$trigger\\r]
	if {$index2 != $index} {
		bell
		$win.statusBar itemconfigure t1 \
			-text "That trigger is being used by macro #$index2."
		return
	}

	# Update the list
	SetList $oop

	# See the updated macro
	NSCanvist::See $canvistId $index

	# Warn the user about simple keypresses
	if {[string length $trigger] == 1} {
		tk_messageBox -parent $win -title "Macro Error" \
			-message "For simple triggers use the Keymap Window instead."
	}

	return
}

# NSMacros::SelectionChanged --
#
#	Called when the list selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::SelectionChanged {oop canvistId select deselect} {

	variable Priv

	set win [Info $oop win]
	set entryTrigger $win.frameFields.entryTrigger
	set entryAction $win.frameFields.entryAction

	if {[llength $select]} {
		set row [lindex $select 0]

		# Strip leading "_^" and trailing "\r" from keypress
		set keypress [angband macro keypress $row]
		regexp {\^_(.+)\\r} $keypress ignore keypress
		$entryTrigger configure -text $keypress

		$entryAction delete 0 end
		$entryAction insert 0 [angband macro action $row]

		set Priv(selectedMacro) $row

	} else {
		$entryTrigger configure -text ""
		$entryAction delete 0 end

		set Priv(selectedMacro) -1
	}

	return
}

# NSMacros::Configure --
#
#	Called as a <Configure> event script. Resizes the selection rectangles
#	so they fit properly.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::Configure {oop canvas} {

	# Get the width of the canvas
	set canvasWidth [winfo width $canvas]

	foreach itemId [$canvas find withtag selrect] {
		set coords [$canvas coords $itemId]
		set right [expr {($canvasWidth - 1) - 2}]
		lset coords 2 $right
		eval $canvas coords $itemId $coords
	}

	return
}

# ValueChanged_font_macros --
#
#	Called when the font,macros value changes.
#	Updates the Macros Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMacros::ValueChanged_font_macros {oop} {

	set canvistId [Info $oop canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]

	# Set the row height
	set rowHgt [font metrics [Value font,macros] -linespace]
	incr rowHgt 8
	NSCanvist::Info $canvistId rowHgt $rowHgt
	$canvas configure -yscrollincrement $rowHgt

	return
}

