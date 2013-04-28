# File: prf-window.tcl

# Purpose: the Pref File Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSPrfWindow {

	variable MenuString

# namespace eval NSPrfWindow
}

# NSPrfWindow::InitModule -- 
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::InitModule {} {

	variable Priv

	MsgCatInit prf

	NSModule::LoadIfNeeded NSPrfFile
	NSModule::LoadIfNeeded NSList

	set Priv(hook) {macro keymap}

	NSObject::New NSPrfWindow

	return
}

# NSPrfWindow::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::CloseModule {} {

	catch {
		destroy [Window prf]
	}

	return
}

# NSPrfWindow::NSPrfWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::NSPrfWindow {oop} {

	variable Priv

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow prf $win \
		"GetDefaultGeometry $win reqwidth main" "" \
		"NSPrfWindow::DisplayCmd $oop"

	# Update ourself when the font,macros value changes
	qebind NSPrfWindow <Value-font,macros> \
		"NSPrfWindow::ValueChanged_font_macros $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSPrfWindow $oop $win

	foreach hook $Priv(hook) {
		Info $oop hook $hook
		CallHook $oop init
	}

	Info $oop hook ""

	#
	# Global list of application windows
	#

	Global prf,oop $oop
	Window prf $win

	Loaded_Init $oop

	return
}

# NSPrfWindow::~NSPrfWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::~NSPrfWindow {oop} {

	foreach prfId [Info $oop prf,prfIds] {
		NSObject::Delete NSPrfFile $prfId
	}

	return
}

# NSPrfWindow::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::Info {oop info args} {

	global NSPrfWindow

	# Verify the object
	NSObject::CheckObject NSPrfWindow $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSPrfWindow($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSPrfWindow($oop,$info)
			}
		}
	}

	return
}

# NSPrfWindow::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::InitWindow {oop} {

	variable Priv

	set win .prf$oop
	toplevel $win
	wm title $win [mc title]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSPrfWindow::Close $oop"

	Info $oop win $win

	InitMenus $oop
if 0 {
	#
	# Toolbar
	#

	set toolId [NSObject::New NSToolbar 20 $win]
	NSToolbar::AddTool $toolId -image Image_Open -label [mc Open] \
		-showlabel yes -command "NSPrfWindow::MacroLoad $oop"
	NSToolbar::AddTool $toolId -image Image_Save -label [mc Save] \
		-showlabel yes -command "NSPrfWindow::MacroDump $oop"

	NSStatusText::StatusText [NSToolbar::GetTool $toolId 1] \
		$win.statusBar.label0 \
		"Read settings from an existing preferences file."
	NSStatusText::StatusText [NSToolbar::GetTool $toolId 2] \
		$win.statusBar.label0 \
		"Append macros to a new or existing preferences file."
}
	#
	# Divider
	#

	frame $win.divider2 \
		-borderwidth 1 -height 2 -relief groove

	#
	# Panedwindow
	#

	panedwindow $win.pw -orient horizontal -opaqueresize true -sashrelief sunken

	#
	# .prf file list
	#

	set font [Value font,macros]
	set cw [font measure $font "W"]
	set width [expr {$cw * 25}]

	set frame $win.framePrf
	set tree [NSList::New $frame -xscroll 1 -yscroll 1 -font macros]
	$tree configure -font $font -width $width

	$tree state define current
	$tree element configure eSel -fill [list gray30 current]

	$tree state define modified
	$tree element configure eTxt -fill [list [Value TERM_L_BLUE] modified White {}]

	NSList::OnSelection $tree \
		"NSPrfWindow::SelectionChanged_Prf $oop %T %c %S %D"

	Info $oop prf,tree $tree
	Info $oop prf,prfIds {}
	Info $oop prf,selection,prfId ""
	Info $oop prf,selection,items {}

	$win.pw add $frame

	#
	# Frame holding tabs and macro list
	#

	set frame $win.frame
	frame $frame \
		-borderwidth 0

	grid columnconfigure $frame 0 -weight 1
	grid rowconfigure $frame 0 -weight 0
	grid rowconfigure $frame 1 -weight 1

	$win.pw add $frame

	#
	# Tabs
	#

	set tabsId [NSObject::New NSTabs $win.frame]
	foreach hook $Priv(hook) {
		NSTabs::Add $tabsId [mc tab-$hook]
	}
	NSTabs::Info $tabsId invokeCmd "NSPrfWindow::InvokeTab $oop"
	NSTabs::Info $tabsId active 1
	Info $oop tabsId $tabsId

	#
	# Macro list
	#

	set width [expr {$cw * 40}]

	set frame $win.frame.frameMacro
	set tree [NSList::New $frame -xscroll 1 -yscroll 1 -columns 2 -font macros]
	$tree configure -font $font -width $width -showheader yes -selectmode extended

	$tree column configure 0 -text [mc Trigger]
	$tree column configure 1 -text [mc Action] -expand yes

	$tree state define duplicate
	$tree element configure eTxt -fill [list gray60 duplicate White {}]

	$tree notify bind $frame <Selection> \
		"NSPrfWindow::SelectionChanged_Macro $oop %T %c %S %D"

	Info $oop macro,tree $tree
	Info $oop macro,selection,row ""
	Info $oop macro,selection,rows {}

	#
	# Trigger and Action entries
	#

	set frame $win.frameFields
	frame $frame \
		-borderwidth 0
	button $frame.buttonNew \
		-text "+" -width 3 -command "NSPrfWindow::NewKeymapOrMacro $oop" \
		-state disabled
	button $frame.buttonDel \
		-text "-" -width 3 -command "NSPrfWindow::DeleteKeymapOrMacro $oop" \
		-state disabled
	label $frame.labelTrigger \
		-text [mc Trigger:]
	label $frame.entryTrigger \
		-width 10 -borderwidth 2 -relief sunken -takefocus 1
	label $frame.labelAction \
		-text [mc Action:]
	entry $frame.entryAction \
		-width 10
	pack $frame.buttonNew \
		-side left -padx {6 0}
	pack $frame.buttonDel \
		-side left -padx 6
	pack $frame.labelTrigger \
		-side left -padx 2 -pady 5
	pack $frame.entryTrigger \
		-side left -padx 2 -pady 5 -expand yes -fill x
	pack $frame.labelAction \
		-side left -padx 2 -pady 5
	pack $frame.entryAction \
		-side left -padx {2 6} -pady 5 -expand yes -fill x

	# When the Trigger field has the focus, typing a key sequence
	# changes the trigger for the selected macro/keymap.
	set entry $frame.entryTrigger
	bind $entry <Any-Enter> {
		if {[%W cget -state] ne "disabled"} {
			focus %W
		}
	}
	bind $entry <Any-Leave> "focus $tree"
	bind $entry <FocusIn> {%W configure -background White}
	bind $entry <FocusOut> {%W configure -background [[winfo toplevel %W] cget -background]}
	$entry configure -state disabled

	# The Action entry is enabled when a macro is selected
	set entry $frame.entryAction
	bind $entry <KeyPress-Return> "NSPrfWindow::CallHook $oop set_action"
	$entry configure -state disabled

	# Allow the user to type 'n' in the action field; normally 'n' creates
	# a new macro or keymap.
	set tags [bindtags $entry]
	set index [lsearch -exact $tags $win]
	bindtags $entry [lreplace $tags $index $index]

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 20

	#
	# Geometry
	#

	grid [NSTabs::Info $tabsId canvas] -column 0 -row 0 -sticky ew
	grid $win.frame.frameMacro -column 0 -row 1 -sticky news

	grid rowconfig $win 0 -weight 0 -minsize 0
	grid rowconfig $win 1 -weight 1 -minsize 0
	grid rowconfig $win 2 -weight 0 -minsize 0
	grid rowconfig $win 3 -weight 0 -minsize 0
	grid rowconfig $win 4 -weight 0 -minsize 0
	grid columnconfig $win 0 -weight 1 -minsize 0
if 0 {
	pack forget [NSToolbar::Info $toolId frame]
	grid [NSToolbar::Info $toolId frame] \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
}
	grid $win.divider2 -in $win \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.pw \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.frameFields \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.statusBar \
		-row 3 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	#
	# Context Menu
	#

	set m $win.context
	menu $m -tearoff 0
	bind $tree <Button-3> "::NSPrfWindow::ContextMenu $oop $m %X %Y"

	#
	# KeyPress bindings
	#

	bind $win <KeyPress-Escape> "NSPrfWindow::Close $oop"

	NSStatusText::StatusText $win.frameFields.buttonNew \
		$win.statusBar.label0 \
		[mc status-new]
	NSStatusText::StatusText $win.frameFields.buttonDel \
		$win.statusBar.label0 \
		[mc status-delete]
	NSStatusText::StatusText $win.frameFields.entryTrigger \
		$win.statusBar.label0 \
		[mc status-trigger]
	NSStatusText::StatusText $win.frameFields.entryAction \
		$win.statusBar.label0 \
		[mc status-action]

	return
}

# NSPrfWindow::InitMenus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::InitMenus {oop} {

	variable MenuString

	# Default accelerator modifier
	set mod "Ctrl"

	set win [Info $oop win]

	#
	# Menu bar
	#

	Info $oop mbarId [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSPrfWindow::SetupMenus $oop" -identifier MENUBAR]
	set mbar [Info $oop mbarId]

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSPrfWindow::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSPrfWindow::MenuInvoke $oop"

	#
	# Prf Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_PREF
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_PREF -label [mc "Prf"] -underline 0 -identifier M_PREF

	set entries {}
	lappend entries [list -type command -label [mc "New Prf File..."] \
		-underline 0 -identifier E_PRF_NEW]
	lappend entries [list -type command -label [mc "Save Selected File"] \
		-underline 0 -identifier E_PRF_SAVE]
	lappend entries [list -type command -label [mc "Save Modified Files"] \
		-underline 0 -identifier E_PRF_SAVE_MODIFIED]
	lappend entries [list -type command -label [mc "Revert Selected File"] \
		-underline 0 -identifier E_PRF_REVERT]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc "Rename Selected File"] \
		-underline 0 -identifier E_PRF_RENAME]
	lappend entries [list -type command -label [mc "Remove Selected File"] \
		-underline 0 -identifier E_PRF_REMOVE]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc "Display Load Order"] \
		-underline 0 -identifier E_PRF_LOAD_ORDER]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_PREF $entries

	#
	# Edit Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_EDIT
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_EDIT -label [mc "Edit"] -underline 0 -identifier M_EDIT

	set entries {}
	lappend entries [list -type command -label [mc "Cut"] \
		-underline 0 -accelerator $mod+X -identifier E_EDIT_CUT]
	lappend entries [list -type command -label [mc "Copy"] \
		-underline 0 -accelerator $mod+C -identifier E_EDIT_COPY]
	lappend entries [list -type command -label [mc "Paste"] \
		-underline 0 -accelerator $mod+V -identifier E_EDIT_PASTE]
	lappend entries [list -type command -label [mc "Delete"] \
		-underline 0 -accelerator Del -identifier E_EDIT_DELETE]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc "Select All"] \
		-underline 0 -identifier E_EDIT_SELECT_ALL]

	NSMenu::MenuInsertEntries $mbar -end MENU_EDIT $entries

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

	NSMenu::BindAccels $mbar $win

	return
}

# NSPrfWindow::SetupMenus --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::SetupMenus {oop mbarId} {

	set tree [Info $oop macro,tree]

	lappend identList E_CLOSE E_PRF_NEW E_PRF_LOAD_ORDER

	set prfId [Info $oop prf,selection,prfId]
	if {$prfId ne ""} {
		if {[NSPrfFile::Info $prfId modified]} {
			lappend identList E_PRF_REVERT E_PRF_SAVE
		}
	}
	foreach prfId [Info $oop prf,prfIds] {
		if {[NSPrfFile::Info $prfId modified]} {
			lappend identList E_PRF_SAVE_MODIFIED
			break
		}
	}

	if {[llength [Info $oop prf,selection,items]] == 1} {
		lappend identList E_PRF_RENAME
	}
	if {[llength [Info $oop prf,selection,items]]} {
		lappend identList E_PRF_REMOVE
	}

	if {[NSUtils::HasFocus $tree]} {
		if {[$tree selection count]} {
			lappend identList E_EDIT_CUT E_EDIT_COPY E_EDIT_DELETE
		}
		if {[$tree numitems] > 1} {
			lappend identList E_EDIT_SELECT_ALL
		}
	}

	set identList [concat $identList [CallHook $oop menu_setup]]

	NSMenu::MenuEnable $mbarId $identList

	return
}

# NSPrfWindow::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::MenuSelect {oop menuId index ident} {

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

# NSPrfWindow::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::MenuInvoke {oop menuId ident} {

	set win [Info $oop win]
	set prfId [Info $oop prf,selection,prfId]
	set tree [Info $oop macro,tree]

	switch -glob -- $ident {
		E_PRF_NEW {
			NewFile $oop
		}
		E_PRF_SAVE {
			NSPrfFile::Write $prfId
			PrfModified $oop $prfId
		}
		E_PRF_SAVE_MODIFIED {
			foreach prfId [Info $oop prf,prfIds] {
				if {![NSPrfFile::Info $prfId modified]} continue
				NSPrfFile::Write $prfId
				PrfModified $oop $prfId
			}
		}
		E_PRF_REVERT {
			set msg [format [mc msg-revert] [file tail [NSPrfFile::File $prfId]]]
			set ans [tk_messageBox -title [mc title-revert] \
				-message $msg -type yesno -icon question \
				-parent $win]
			if {$ans eq "no"} return
			NSPrfFile::Read $prfId
			PrfModified $oop $prfId
			CallHook $oop set_list
		}
		E_PRF_RENAME {
			set fileOld [NSPrfFile::File $prfId]
			set dirOld [file dirname $fileOld]
			set nameOld [file tail $fileOld]
			set nameNew [NSUtils::StringBox -title [mc title-rename] \
				-initial $nameOld -prompt [mc prompt-rename] \
				-buttons [list [mc Rename] [mc Cancel]] -parent $win]
			if {$nameNew eq ""} return
			set fileNew [file join $dirOld $nameNew]
			if {[file exists $fileNew]} {
				return
			}
			file rename $fileOld $fileNew
			NSPrfFile::Info $prfId file $fileNew
			# Set and sort list
			set item [Info $oop prf,selection,items]
			SetList_Prf $oop
			NSList::Activate [Info $oop prf,tree] $item
		}
		E_PRF_REMOVE {
			set msg [format [mc msg-remove] [file tail [NSPrfFile::File $prfId]]]
			set ans [tk_messageBox -title [mc title-remove] \
				-message $msg -type yesno -icon warning \
				-parent $win]
			if {$ans eq "no"} return
			foreach item [Info $oop prf,selection,items] {
				set prfId [Info $oop prf,item2prfId,$item]
				set file [NSPrfFile::File $prfId]
				file delete -force $file
				set i [lsearch -exact [Info $oop prf,prfIds] $prfId]
				Info $oop prf,prfIds [lreplace [Info $oop prf,prfIds] $i $i]
				NSObject::Delete NSPrfFile $prfId
			}
			SetList_Prf $oop
		}
		E_PRF_LOAD_ORDER {
			DisplayLoadOrder $oop
		}
		E_CLOSE {
			Close $oop
		}
		E_EDIT_SELECT_ALL {
			$tree selection modify [$tree item children root] {}
		}
		default {
			CallHook $oop menu_invoke $ident
		}
	}

	return
}

# NSPrfWindow::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::DisplayCmd {oop message first} {

	set tree [Info $oop macro,tree]

	switch -- $message {
		preDisplay {
			if {$first} {
				GlobPrf $oop
				SetList_Prf $oop

				SetHook $oop macro
			}
		}
		postDisplay {
			focus $tree
		}
	}

	return
}

# NSPrfWindow::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::Close {oop} {

	# Prompt to save modified files
	set modified 0
	foreach prfId [Info $oop prf,prfIds] {
		if {![NSPrfFile::Info $prfId modified]} continue
		set modified 1
		break
	}
	if {$modified} {
		set answer [tk_messageBox -title [mc title-modified] \
			-message [mc msg-modified] \
			-parent [Info $oop win] -type yesno]
		if {$answer eq "yes"} {
			foreach prfId [Info $oop prf,prfIds] {
				if {![NSPrfFile::Info $prfId modified]} continue
				NSPrfFile::Write $prfId
				PrfModified $oop $prfId
			}
		}
	}

	NSWindowManager::Undisplay prf

	return
}

# NSPrfWindow::ContextMenu --
#
#	When an macros item is right-clicked, pop up a context
#	menu of actions.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::ContextMenu {oop menu x y} {
}

# NSPrfWindow::GlobPrf --
#
#	Glob *.prf.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::GlobPrf {oop} {

	# Get list of .prf files
	set pref [glob -nocomplain -directory [Path lib pref] *.prf]
	set user [glob -nocomplain -directory [Path lib user] *.prf]
	set files [concat $pref $user]

	# Iterate over files
	set prfIds {}
	foreach path $files {
		set prfId [NSObject::New NSPrfFile]
		NSPrfFile::File $prfId $path
		NSPrfFile::Read $prfId
		lappend prfIds $prfId
	}

	Info $oop prf,prfIds $prfIds

	return
}

# NSPrfWindow::SetList_Prf --
#
#	Set the list of .prf files.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::SetList_Prf {oop} {

	set tree [Info $oop prf,tree]

	array unset ::NSPrfWindow $oop,prf,item2prfId,*
	array unset ::NSPrfWindow $oop,prf,prfId2item,*

	# Clear the list
	NSList::Clear $tree

	# Iterate over files
	foreach prfId [Info $oop prf,prfIds] {

		set file [NSPrfFile::File $prfId]

		# Append pref/* or user/* to the list
		set name [file join {*}[lrange [file split $file] end-1 end]]
		set name [file nativename $name]
		set item [$tree item create]
		NSList::SetText $tree $item $name
		if {[file tail $file] eq "[angband player base_name].prf"} {
			$tree item state set $item current
		}
		if {[NSPrfFile::Info $prfId modified]} {
			$tree item state set $item modified
		}
		$tree item lastchild root $item

		Info $oop prf,item2prfId,$item $prfId
		Info $oop prf,prfId2item,$prfId $item
	}

	# Sort
	$tree item sort root -dictionary

	return
}

# The following bindings are used to get a macro trigger keypress
# from the user. They should be almost identical to those found in
# keyboard.tcl. The differences are that (1) the keypress passed
# a local command, not as an argument to "angband keypress"; and (2)
# the "wrapper" chars \037 and \015 are not written.

bind MacroTrigger_BindTag <Control-Shift-KeyPress> {

	NSPrfWindow::CallHook [Global prf,oop] set_trigger Control-Shift-%K
}

bind MacroTrigger_BindTag <Control-KeyPress> {

	# Special Control-KeyPress (ex Control-F1)
	if {![string length %A] || [string length %K] > 1} {
		NSPrfWindow::CallHook [Global prf,oop] set_trigger Control-%K

	# Ascii Control-KeyPress
	} else {
		bell
	}
}

bind MacroTrigger_BindTag <Shift-KeyPress> {

	# Special Shift-KeyPress (ex Shift-F1)
	if {![string length %A]} {
		NSPrfWindow::CallHook [Global prf,oop] set_trigger Shift-%K

	# Ascii Shift-KeyPress
	} else {
		bell
	}
}

bind MacroTrigger_BindTag <Alt-KeyPress> {

	NSPrfWindow::CallHook [Global prf,oop] set_trigger Alt-%K
}

bind MacroTrigger_BindTag <KeyPress> {

	# Special KeyPress (ex F1)
	if {![string length %A]} {
		NSPrfWindow::CallHook [Global prf,oop] set_trigger %K

	# Normal keys with no modifiers
	} else {
		bell
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

# Same as above but for keymaps

bind KeymapTrigger_BindTag <Control-Shift-KeyPress> {

	bell
}

bind KeymapTrigger_BindTag <Control-KeyPress> {

	# Special Control-KeyPress (ex Control-F1)
	if {![string length %A] || [string length %K] > 1} {
		bell

	# Ascii Control-KeyPress
	} else {
		NSPrfWindow::CallHook [Global prf,oop] set_trigger ^[string toupper %K]
	}
}

bind KeymapTrigger_BindTag <Shift-KeyPress> {

	# Special Shift-KeyPress (ex Shift-F1)
	if {![string length %A]} {
		bell

	# Ascii Shift-KeyPress
	} else {
		NSPrfWindow::CallHook [Global prf,oop] set_trigger %A
	}
}

bind KeymapTrigger_BindTag <Alt-KeyPress> {

	bell
}

bind KeymapTrigger_BindTag <KeyPress> {

	# Special KeyPress (ex F1)
	if {![string length %A]} {
		bell

	# Normal keys with no modifiers
	} else {
		NSPrfWindow::CallHook [Global prf,oop] set_trigger %A
	}
}

bind KeymapTrigger_BindTag <Escape> {

	bell
}

bind KeymapTrigger_BindTag <Return> {

	bell
}

bind KeymapTrigger_BindTag <Tab> {

	bell
}

bind KeymapTrigger_BindTag <BackSpace> {

	bell
}

bind KeymapTrigger_BindTag <Delete> {

	bell
}

# Ignore modifiers by themselves
foreach mod1 {Control Shift Alt} {
	bind KeymapTrigger_BindTag <${mod1}_L> { ; }
	bind KeymapTrigger_BindTag <${mod1}_R> { ; }
	foreach mod2 {Control Shift Alt} {
		if {$mod1 eq $mod2} continue
		bind KeymapTrigger_BindTag <$mod1-${mod2}_L> { ; }
		bind KeymapTrigger_BindTag <$mod1-${mod2}_R> { ; }
		bind KeymapTrigger_BindTag <$mod2-${mod1}_L> { ; }
		bind KeymapTrigger_BindTag <$mod2-${mod1}_R> { ; }
	}
}

# NSPrfWindow::NewFile --
#
#	Create a new .prf file inside the lib/user directory.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::NewFile {oop} {

	set win [Info $oop win]

	set filename [tk_getSaveFile -initialfile [angband player base_name].prf \
		-initialdir [PathUser] -parent $win]
	if {$filename eq ""} return

	if {![IsPrefFile $filename] && ![IsUserFile $filename]} {
		tk_messageBox -parent $win -title [mc title-new] -icon info \
			-message [mc msg-new]
		return
	}
	if {[file extension $filename] ne ".prf"} {
		tk_messageBox -parent $win -title [mc title-extension] -icon info \
			-message [mc msg-extension]
		return
	}

	if {[file exists $filename]} {
		# Empty the file

		# Find the prfId
		foreach prfId [Info $oop prf,prfIds] {
			if {[NSPrfFile::File $prfId] eq $filename} break
		}

		# Read the empty file
		NSPrfFile::Read $prfId
	} else {
		# Create the file

		# Create prfId for file
		set prfId [NSObject::New NSPrfFile]
		NSPrfFile::File $prfId $filename

		# Add to list of prfIds
		lappend ::NSPrfWindow($oop,prf,prfIds) $prfId
	}

	# Add to list
	SetList_Prf $oop

	# Select new file
	set item [Info $oop prf,prfId2item,$prfId]
	set tree [Info $oop prf,tree]
	NSList::Activate $tree $item

	return
}

# NSPrfWindow::MacroLoad --
#
#	Get a filename from the user then read in the given pref file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::MacroLoad {oop} {

	# Let the user choose a file, and read it
	if {[ProcessPrefFile [Info $oop win]]} return

	# Update the list
	SetList_Macro $oop

	return
}

# NSPrfWindow::SelectionChanged_Prf --
#
#	Called when the .prf list selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::SelectionChanged_Prf {oop tree count select deselect} {

	set selection [$tree selection get]
	if {$count == 1} {
		set item [lindex $selection 0]
		set prfId [Info $oop prf,item2prfId,$item]
		Info $oop prf,selection,prfId $prfId

		[Info $oop win].frameFields.buttonNew configure -state enabled
	} else {
		Info $oop prf,selection,prfId ""

		[Info $oop win].frameFields.buttonNew configure -state disabled
	}
	Info $oop prf,selection,items $selection

	CallHook $oop set_list

	return
}

# NSPrfWindow::SelectionChanged_Macro --
#
#	Called when the macro list selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::SelectionChanged_Macro {oop tree count select deselect} {

	CallHook $oop selection_changed $select $deselect

	return
}

# NSPrfWindow::InvokeTab --
#
#	Called when a tab is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::InvokeTab {oop tabsId tabId} {

	variable Priv

	set index [lsearch -exact [NSTabs::Info $tabsId id] $tabId]
	SetHook $oop [lindex $Priv(hook) $index]

	return
}

# NSPrfWindow::PrfModified --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::PrfModified {oop prfId} {

	set tree [Info $oop prf,tree]
	set item [Info $oop prf,prfId2item,$prfId]
	if {[NSPrfFile::Info $prfId modified]} {
		set state modified
	} else {
		set state !modified
	}
	$tree item state set $item $state

	return
}

# ValueChanged_font_macros --
#
#	Called when the font,macros value changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::ValueChanged_font_macros {oop} {

	set tree [Info $oop macro,tree]
	$tree configure -font [Value font,macros]

	set tree [Info $oop prf,tree]
	$tree configure -font [Value font,macros]

	return
}

# NSPrfWindow::NewKeymapOrMacro --
#
#	Create a new macro or keymap.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::NewKeymapOrMacro {oop} {

	set mbarId [Info $oop mbarId]

	if {[Info $oop hook] eq "macro"} {
		NSMenu::TryInvoke $mbarId E_MACRO_NEW
	} else {
		NSMenu::TryInvoke $mbarId E_KEYMAP_NEW
	}

	return
}

# NSPrfWindow::DeleteKeymapOrMacro --
#
#	Delete the selected macro or keymap.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::DeleteKeymapOrMacro {oop} {

	set mbarId [Info $oop mbarId]

	focus [Info $oop macro,tree]
	NSMenu::TryInvoke $mbarId E_EDIT_DELETE

	return
}

# NSPrfWindow::CallHook --
#
#	Call the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::CallHook {oop message args} {

	return [uplevel #0 NSPrfWindow::hook_[Info $oop hook] $oop $message $args]
}

# NSPrfWindow::SetHook --
#
#	Set the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::SetHook {oop hook} {

	variable Priv

	if {$hook ne [Info $oop hook]} {
		set again 0
	} else {
		set again 1
	}

	if {!$again} {
#		StateRemember $oop
		if {[Info $oop hook] ne ""} {
			CallHook $oop close
		}
	}

	# Remember the hook
	Info $oop hook $hook

	set tabsId [Info $oop tabsId]
	set current [NSTabs::Info $tabsId current]
	set tabId [NSTabs::GetNthId $tabsId [lsearch -exact $Priv(hook) $hook]]
	if {$tabId != $current} {
		NSTabs::Smaller $tabsId $current
		NSTabs::Bigger $tabsId $tabId
		NSTabs::Info $tabsId current $tabId
	}

	CallHook $oop open
	CallHook $oop set_list

	return
}

proc NSPrfWindow::LoadMacro {trigger action} {

	# Get number of macros
	set max [angband macro max]

	# Iterate over macros
	for {set i 0} {$i < $max} {incr i} {

		# Get the keypress
		set keypress [angband macro keypress $i]

		# Check for macro using this trigger
		if {$keypress eq $trigger} {
			angband macro action $i $action
			return
		}
	}

	# Create a new macro
	set i [angband macro create $trigger]
	angband macro action $i $action

	return
}

proc NSPrfWindow::hook_macro {oop message args} {

	set tree [Info $oop macro,tree]
	set prfId [Info $oop prf,selection,prfId]

	switch -- $message {

		init {
			set mbarId [Info $oop mbarId]

			# Not saved
			Info $oop macro,autoload 1

			#
			# Macro Menu
			#

			NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_MACRO

			set entries {}
			lappend entries [list -type command -label [mc "New Macro"] \
				-underline 0 -accelerator n -identifier E_MACRO_NEW]
			lappend entries [list -type command -label [mc "Remove Duplicates"] \
				-underline 0 -identifier E_MACRO_REMOVE_DUPS]
			lappend entries [list -type separator]
			lappend entries [list -type command -label [mc "Load Selected Macros"] \
				-underline 0 -identifier E_MACRO_LOAD_SEL]
			lappend entries [list -type command -label [mc "Load All Macros"] \
				-underline 0 -identifier E_MACRO_LOAD_ALL]
			lappend entries [list -type command -label [mc "Display Loaded Macros"] \
				-underline 0 -identifier E_MACRO_DISPLAY]
			lappend entries [list -type checkbutton -label [mc E_MACRO_AUTOLOAD] \
				-variable ::NSPrfWindow($oop,macro,autoload) \
				-underline 0 -identifier E_MACRO_AUTOLOAD]

			NSMenu::MenuInsertEntries $mbarId -end MENU_MACRO $entries

			Info $oop macro,clip {}
		}

		open {
			set mbarId [Info $oop mbarId]
			NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
				-menu MENU_MACRO -label [mc Macro] -underline 0 -identifier M_MACRO

			set win [Info $oop win]
#			bind $win <KeyPress-n> "NSMenu::TryInvoke $mbarId E_MACRO_NEW"
			bind $win <KeyPress-n> "NSUtils::InvokeButton $win.frameFields.buttonNew"

			set entry $win.frameFields.entryTrigger
			bindtags $entry [list $entry MacroTrigger_BindTag StatusText_BindTag]
		}

		close {
			set mbarId [Info $oop mbarId]
			NSMenu::MenuDeleteEntry $mbarId M_MACRO
		}

		menu_setup {
			set identList {}
			lappend identList E_MACRO_DISPLAY E_MACRO_AUTOLOAD
			if {$prfId ne ""} {
				lappend identList E_MACRO_NEW
			}
			if {[$tree selection count]} {
				lappend identList E_MACRO_REMOVE E_MACRO_LOAD_SEL
			} 
			if {[$tree numitems] > 1} {
				lappend identList E_MACRO_LOAD_ALL
			}
			if {[Info $oop macro,hasDups]} {
				lappend identList E_MACRO_REMOVE_DUPS
			}
			if {[llength [Info $oop macro,clip]]} {
				lappend identList E_EDIT_PASTE
			}
			return $identList
		}

		menu_invoke {
			switch -- [lindex $args 0] {
				E_EDIT_CUT {
					hook_macro $oop menu_invoke E_EDIT_COPY
					hook_macro $oop menu_invoke E_EDIT_DELETE
				}
				E_EDIT_COPY {
					set clip {}
					foreach row [Info $oop macro,selection,rows] {
						lappend clip [NSPrfFile::MacroKeypress $prfId $row] \
							[NSPrfFile::MacroAction $prfId $row]
					}
					Info $oop macro,clip $clip
				}
				E_EDIT_PASTE {
					foreach {keypress action} [Info $oop macro,clip] {
						set index [NSPrfFile::MacroNew $prfId]
						NSPrfFile::MacroKeypress $prfId $index $keypress
						NSPrfFile::MacroAction $prfId $index $action
					}
					PrfModified $oop $prfId
					hook_macro $oop set_list
				}
				E_EDIT_DELETE {
					foreach row [lsort -integer -decreasing [Info $oop macro,selection,rows]] {
						NSPrfFile::MacroDelete $prfId $row
					}
					PrfModified $oop $prfId
					hook_macro $oop set_list
				}
				E_MACRO_NEW {

					# Create a new empty macro.
					set index [NSPrfFile::MacroNew $prfId]

					# Update the list
					hook_macro $oop set_list

					# See the new macro
					NSList::Activate $tree "root child $index"

					# Indicate .prf file is modified
					PrfModified $oop $prfId
				}
				E_MACRO_REMOVE_DUPS {
					set row 0
					set delete {}
					foreach item [$tree item children root] {
						if {[$tree item state get $item duplicate]} {
							lappend delete $row
						}
						incr row
					}
					if {![llength $delete]} return
					foreach row [lsort -integer -decreasing $delete] {
						NSPrfFile::MacroDelete $prfId $row
					}
					hook_macro $oop set_list
					PrfModified $oop $prfId
				}
				E_MACRO_LOAD_SEL {
					foreach row [Info $oop macro,selection,rows] {
						set keypress [NSPrfFile::MacroKeypress $prfId $row]
						set action [NSPrfFile::MacroAction $prfId $row]
						LoadMacro $keypress $action
					}
					Loaded_SetList $oop
				}
				E_MACRO_LOAD_ALL {
					foreach {keypress action} [NSPrfFile::MacroList $prfId] {
						LoadMacro $keypress $action
					}
					Loaded_SetList $oop
				}
				E_MACRO_DISPLAY {
					Loaded_SetList $oop macro
					WindowPosition [Info $oop loaded,win] 2 3
				}
			}
		}

		set_list {

			set win [Info $oop win]

			# Clear the list
			NSList::Clear $tree

			Info $oop macro,hasDups 0

			# No .prf file is selected
			if {$prfId eq ""} return

			set max 0

			# Iterate over macros
			set macroList [NSPrfFile::MacroList $prfId]
			foreach {keypress action} $macroList {

				# See if this keypress is used by a following macro
				set isDup 0
				foreach {keypress2 action2} [lrange $macroList [expr {($max + 1) * 2}] end] {
					if {$keypress eq $keypress2} {
						set isDup 1
						Info $oop macro,hasDups 1
						break
					}
				}

				# Strip leading "_^" and trailing "\r" from keypress
				regexp {\^_(.+)\\r} $keypress ignore keypress

				set keypress [string map [list Control- [mc Ctrl+] Shift- [mc Shift+] Alt- [mc Alt+]] $keypress]

				# Append to the list
				set item [$tree item create]
				$tree item text $item 0 $keypress 1 $action
				if {$isDup} {
					$tree item state set $item duplicate
				}
				$tree item lastchild root $item

				incr max
			}

			# Display total number of macros
			if {$max == 1} {
				set text [mc "1 macro"]
			} else {
				set text [format [mc "%d macros"] $max]
			}
			$win.statusBar itemconfigure t2 -text $text
		}

		selection_changed {

			set win [Info $oop win]
			set entryTrigger $win.frameFields.entryTrigger
			set entryAction $win.frameFields.entryAction

			set selection [$tree selection get]
			if {[llength $selection] == 1} {
				set item [lindex $selection 0]
				set row [NSList::Item2Row $tree $item]

				# Strip leading "_^" and trailing "\r" from keypress
				set keypress [NSPrfFile::MacroKeypress $prfId $row]
				regexp {\^_(.+)\\r} $keypress ignore keypress
				$entryTrigger configure -state normal -text $keypress

				set action [NSPrfFile::MacroAction $prfId $row]
				$entryAction configure -state normal
				$entryAction delete 0 end
				$entryAction insert 0 $action

				Info $oop macro,selection,row $row

			} else {
				$entryTrigger configure -state disabled -text ""
				$entryAction delete 0 end
				$entryAction configure -state disabled

				Info $oop macro,selection,row -1
			}
			set rows {}
			foreach item $selection {
				lappend rows [NSList::Item2Row $tree $item]
			}
			Info $oop macro,selection,rows $rows

			if {[llength $rows]} {
				$win.frameFields.buttonDel configure -state enabled
			} else {
				$win.frameFields.buttonDel configure -state disabled
			}
		}

		set_trigger {
			set index [Info $oop macro,selection,row]
			if {$index == -1} return

			set trigger [lindex $args 0]
			NSPrfFile::MacroKeypress $prfId $index ^_$trigger\\r

			# Update the list
			hook_macro $oop set_list

			# See the updated macro
			NSList::Activate $tree "root child $index"

			# Indicate .prf file is modified
			PrfModified $oop $prfId

			if {[Info $oop macro,autoload]} {
				set action [NSPrfFile::MacroAction $prfId $index]
				LoadMacro ^_$trigger\\r $action
				Loaded_SetList $oop
			}
		}

		set_action {
			set index [Info $oop macro,selection,row]
			if {$index == -1} return

			set win [Info $oop win]
			set entry $win.frameFields.entryAction

			# Set the action for the selected macro
			set action [$entry get]
			NSPrfFile::MacroAction $prfId $index $action

			# Update the list
			hook_macro $oop set_list

			# See the updated macro
			NSList::Activate $tree "root child $index"

			focus $tree

			# Indicate .prf file is modified
			PrfModified $oop $prfId

			if {[Info $oop macro,autoload]} {
				set keypress [NSPrfFile::MacroKeypress $prfId $index]
				LoadMacro $keypress $action
				Loaded_SetList $oop
			}
		}
	}

	return
}

proc NSPrfWindow::hook_keymap {oop message args} {

	set tree [Info $oop macro,tree]
	set prfId [Info $oop prf,selection,prfId]

	switch -- $message {

		init {

			Info $oop keymap,clip {}

			# checkbutton menu entry
			Info $oop rogue_like [Setting rogue_like_commands]

			# Not saved
			Info $oop keymap,autoload 1

			set mbarId [Info $oop mbarId]

			#
			# Keymap Menu
			#

			NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_KEYMAP

			set entries {}
			lappend entries [list -type command -label [mc "New Keymap"] \
				-underline 0 -accelerator n -identifier E_KEYMAP_NEW]
			lappend entries [list -type separator]
			lappend entries [list -type command -label [mc "Load Selected Keymaps"] \
				-underline 0 -identifier E_KEYMAP_LOAD_SEL]
			lappend entries [list -type command -label [mc "Load All Keymaps"] \
				-underline 0 -identifier E_KEYMAP_LOAD_ALL]
			lappend entries [list -type command -label [mc "Display Loaded Keymaps"] \
				-underline 0 -identifier E_KEYMAP_DISPLAY]
			lappend entries [list -type separator]
			lappend entries [list -type checkbutton -label [mc "Rogue-like Keyset"] \
				-variable NSPrfWindow($oop,rogue_like) -identifier E_KEYMAP_ROGUE]
			lappend entries [list -type checkbutton -label [mc E_KEYMAP_AUTOLOAD] \
				-variable ::NSPrfWindow($oop,keymap,autoload) \
				-underline 0 -identifier E_KEYMAP_AUTOLOAD]

			NSMenu::MenuInsertEntries $mbarId -end MENU_KEYMAP $entries

			qebind NSPrfWindow <Setting-rogue_like_commands> \
				"NSPrfWindow::CallHook $oop set_list ;
				set NSPrfWindow($oop,rogue_like) %c"
			qeconfigure NSPrfWindow <Setting-rogue_like_commands> -active no
		}

		open {

			set mbarId [Info $oop mbarId]
			NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
				-menu MENU_KEYMAP -label [mc Keymap] -underline 0 -identifier M_KEYMAP

			set win [Info $oop win]
#			bind $win <KeyPress-n> "NSMenu::TryInvoke $mbarId E_KEYMAP_NEW"
			bind $win <KeyPress-n> "NSUtils::InvokeButton $win.frameFields.buttonNew"

			set entry $win.frameFields.entryTrigger
			bindtags $entry [list $entry KeymapTrigger_BindTag StatusText_BindTag]

			qeconfigure NSPrfWindow <Setting-rogue_like_commands> -active yes
		}

		close {
			set mbarId [Info $oop mbarId]
			NSMenu::MenuDeleteEntry $mbarId M_KEYMAP

			qeconfigure NSPrfWindow <Setting-rogue_like_commands> -active no
		}

		menu_setup {
			set identList {}
			lappend identList E_KEYMAP_ROGUE E_KEYMAP_DISPLAY E_KEYMAP_AUTOLOAD
			if {$prfId ne ""} {
				lappend identList E_KEYMAP_NEW
			}
			if {[$tree selection count]} {
				lappend identList E_KEYMAP_REMOVE E_KEYMAP_LOAD_SEL
			}
			if {[$tree numitems] > 1} {
				lappend identList E_KEYMAP_LOAD_ALL
			}
			if {[llength [Info $oop keymap,clip]]} {
				lappend identList E_EDIT_PASTE
			}
			return $identList
		}

		menu_invoke {
			switch -- [lindex $args 0] {
				E_EDIT_CUT {
					hook_keymap $oop menu_invoke E_EDIT_COPY
					hook_keymap $oop menu_invoke E_EDIT_DELETE
				}
				E_EDIT_COPY {
					set clip {}
					foreach row [Info $oop macro,selection,rows] {
						lappend clip [NSPrfFile::KeymapKeypress $prfId $row] \
							[NSPrfFile::KeymapAction $prfId $row]
					}
					Info $oop keymap,clip $clip
				}
				E_EDIT_PASTE {
					foreach {keypress action} [Info $oop keymap,clip] {
						set index [NSPrfFile::KeymapNew $prfId]
						NSPrfFile::KeymapKeypress $prfId $index $keypress
						NSPrfFile::KeymapAction $prfId $index $action
					}
					PrfModified $oop $prfId
					hook_keymap $oop set_list
				}
				E_EDIT_DELETE {
					foreach row [lsort -integer -decreasing [Info $oop macro,selection,rows]] {
						NSPrfFile::KeymapDelete $prfId $row
					}
					PrfModified $oop $prfId
					hook_keymap $oop set_list
				}
				E_KEYMAP_NEW {

					# Create a new empty keymap.
					set index [NSPrfFile::KeymapNew $prfId]

					# Update the list
					hook_keymap $oop set_list

					# See the new keymap
					NSList::Activate $tree "root child $index"

					# Indicate .prf file is modified
					PrfModified $oop $prfId
				}
				E_KEYMAP_ROGUE {
					Setting rogue_like_commands [Info $oop rogue_like]
				}
				E_KEYMAP_LOAD_SEL {
					foreach row [Info $oop macro,selection,rows] {
						set keypress [NSPrfFile::KeymapKeypress $prfId $row]
						set action [NSPrfFile::KeymapAction $prfId $row]
						angband keymap action $keypress $action
					}
					Loaded_SetList $oop
				}
				E_KEYMAP_LOAD_ALL {
					foreach {keypress action} [NSPrfFile::KeymapList $prfId] {
						angband keymap action $keypress $action
					}
					Loaded_SetList $oop
				}
				E_KEYMAP_DISPLAY {
					Loaded_SetList $oop keymap
					WindowPosition [Info $oop loaded,win] 2 3
				}
			}
		}

		set_list {

			set win [Info $oop win]

			# Clear the list
			NSList::Clear $tree

			# No .prf file is selected
			if {$prfId eq ""} return

			# Iterate over keymaps
			foreach {keypress action} [NSPrfFile::KeymapList $prfId] {

				if {[string length $keypress] == 2} {
					set keypress [mc Ctrl+][string index $keypress 1]
				}

				# Append to the list
				set item [$tree item create]
				$tree item text $item 0 $keypress 1 $action
				$tree item lastchild root $item
			}

			# Display total number of keymaps
			set n [$tree item numchildren root]
			if {$n == 1} {
				set text [mc "1 keymap"]
			} else {
				set text [format [mc "%d keymaps"] $n]
			}
			$win.statusBar itemconfigure t2 -text $text
		}

		selection_changed {

			set win [Info $oop win]
			set entryTrigger $win.frameFields.entryTrigger
			set entryAction $win.frameFields.entryAction

			set selection [$tree selection get]
			if {[llength $selection] == 1} {
				set item [lindex $selection 0]
				set row [NSList::Item2Row $tree $item]

				set keypress [NSPrfFile::KeymapKeypress $prfId $row]
				$entryTrigger configure -state normal -text $keypress

				set action [NSPrfFile::KeymapAction $prfId $row]
				$entryAction configure -state normal
				$entryAction delete 0 end
				$entryAction insert 0 $action

				Info $oop macro,selection,row $row

			} else {
				$entryTrigger configure -state disabled -text ""
				$entryAction delete 0 end
				$entryAction configure -state disabled

				Info $oop macro,selection,row -1
			}
			set rows {}
			foreach item $selection {
				lappend rows [NSList::Item2Row $tree $item]
			}
			Info $oop macro,selection,rows $rows

			if {[llength $rows]} {
				$win.frameFields.buttonDel configure -state enabled
			} else {
				$win.frameFields.buttonDel configure -state disabled
			}
		}

		set_trigger {
			set index [Info $oop macro,selection,row]
			if {$index == -1} return

			set trigger [lindex $args 0]
			NSPrfFile::KeymapKeypress $prfId $index $trigger

			# Update the list
			hook_keymap $oop set_list

			# See the updated keymap
			NSList::Activate $tree "root child $index"

			# Indicate .prf file is modified
			PrfModified $oop $prfId

			if {[Info $oop keymap,autoload]} {
				set action [NSPrfFile::KeymapAction $prfId $index]
				angband keymap action $trigger $action
				Loaded_SetList $oop
			}
		}

		set_action {
			set index [Info $oop macro,selection,row]
			if {$index == -1} return

			set win [Info $oop win]
			set entry $win.frameFields.entryAction

			# Set the action for the selected keymap
			set action [$entry get]
			NSPrfFile::KeymapAction $prfId $index $action

			# Update the list
			hook_keymap $oop set_list

			# See the updated keymap
			NSList::Activate $tree "root child $index"

			focus $tree

			# Indicate .prf file is modified
			PrfModified $oop $prfId

			if {[Info $oop keymap,autoload]} {
				set keypress [NSPrfFile::KeymapKeypress $prfId $index]
				angband keymap action $keypress $action
				Loaded_SetList $oop
			}
		}
	}

	return
}

# NSPrfWindow::DisplayLoadOrder --
#
#	Put up a window showing the order .prf files are loaded in.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::DisplayLoadOrder {oop} {

	set win [Info $oop win].loadorder
	set tree $win.tree
	if {![winfo exists $win]} {
		toplevel $win
		wm title $win [mc title-order]
		wm withdraw $win
		TransientToWin $win [Info $oop win]

		message $win.msg -width 250 -text [mc msg-order]

		treectrl $tree -showheader no -highlightthickness 0 \
			-showroot no -font [Value font,macros]

		$tree column create

		$tree element create eTxt text
		$tree style create s0
		$tree style elements s0 eTxt
		$tree style layout s0 eTxt -padx {2 0}

		$tree configure -defaultstyle s0 -treecolumn 0

		pack $win.msg
		pack $win.tree -expand yes -fill both -padx 4 -pady 4

		wm protocol $win WM_DELETE_WINDOW "wm withdraw $win"
		bind $win <KeyPress-Escape> \
			"wm withdraw $win"
	}

	if {[variant ANGBANDTK KANGBANDTK]} {
		set order [list \
			pref.prf \
			user.prf \
			[angband player base_name].prf \
		]
	}
	if {[variant OANGBANDTK]} {
		set order [list \
			pref.prf \
			user.prf \
			[angband player race].prf \
			[angband player class].prf \
			[angband player base_name].prf \
		]
	}
	if {[variant ZANGBANDTK]} {
		set order [list \
			pref.prf \
			user.prf \
			[angband player race].prf \
			[angband player class].prf \
			[angband player base_name].prf \
		]
		foreach attrib {realm1 realm2} {
			set realm [angband player $attrib]
			if {$realm ne "no magic"} {
				lappend order $realm.prf
			}
		}
	}
	$tree item delete all
	foreach prf $order {
		array unset items
		set items(root) root
		foreach {file parent} [NSPrfFile::ScanIncludes $prf ""] {
			set item [$tree item create -button auto]
			$tree item text $item 0 $file
			if {$parent eq ""} {
				set parent root
			}
			$tree item lastchild $items($parent) $item
			set items($file) $item
		}
	}

	WindowPosition $win 2 3

	return
}

# NSPrfWindow::Loaded_Init --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::Loaded_Init {oop} {

# BUG in Tk
if 0 {
create .prf
withdraw .prf

create .prf.loaded
withdraw .prf.loaded
transient .prf.loaded to .prf
-> wm deiconify .prf.loaded fails (says state is "normal" but it isn't shown)
-> window was created while .prf was not mapped
-> wm deiconify .prf.loadorder works because .prf is mapped when .prf.loadorder is created

so an extra "raise $w" is needed
}

	set win [Info $oop win].loaded
	toplevel $win
	wm title $win [mc title-loaded]
	wm withdraw $win
	TransientToWin $win [Info $oop win]

	wm protocol $win WM_DELETE_WINDOW "wm withdraw $win"

	Info $oop loaded,win $win

	#
	# Frame holding tabs and macro list
	#

	set frame $win.frame
	frame $frame \
		-borderwidth 0

	grid columnconfigure $frame 0 -weight 1
	grid rowconfigure $frame 0 -weight 0
	grid rowconfigure $frame 1 -weight 1

	#
	# Tabs
	#

	set tabsId [NSObject::New NSTabs $win.frame]
	foreach hook {macro keymap} {
		NSTabs::Add $tabsId [mc tab-$hook]
	}
	NSTabs::Info $tabsId invokeCmd "NSPrfWindow::Loaded_InvokeTab $oop"
	NSTabs::Info $tabsId active 1
	Info $oop loaded,which macro
	Info $oop loaded,tabsId $tabsId

	#
	# Macro/Keymaps list
	#

	set font [Value font,macros]
	set cw [font measure $font "W"]
	set width [expr {$cw * 40}]

	set frame $win.frame.frameMacro
	set tree [NSList::New $frame -xscroll 1 -yscroll 1 -columns 2]
	$tree configure -font [Value font,macros] -width $width -showheader yes \
		-selectmode extended

	$tree column configure 0 -text [mc Trigger]
	$tree column configure 1 -text [mc Action] -expand yes

	NSList::OnSelection $tree \
		"NSPrfWindow::Loaded_SelectionChanged $oop %c %S %D"

	Info $oop loaded,tree $tree

	grid [NSTabs::Info $tabsId canvas] -column 0 -row 0 -sticky ew
	grid $win.frame.frameMacro -column 0 -row 1 -sticky news

	pack $win.frame -expand yes -fill both

	#
	# Unload button
	#
	button $win.unload -text [mc Unload] -state disabled \
		-command "NSPrfWindow::Loaded_Unload $oop"
	pack $win.unload -side bottom -anchor s -pady 8

	bind $win <KeyPress-Escape> \
		"wm withdraw $win"

	return
}

# NSPrfWindow::Loaded_SetList --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::Loaded_SetList {oop args} {

	set tree [Info $oop loaded,tree]

	if {[llength $args]} {
		set which [lindex $args 0]
	} else {
		set which [Info $oop loaded,which]
	}

	NSList::Clear $tree

	switch -- $which {
		macro {
			for {set i 0} {$i < [angband macro max]} {incr i} {
				set keypress [angband macro keypress $i]
				regexp {\^_(.+)\\r} $keypress ignore keypress
				set keypress [string map [list Control- [mc Ctrl+] Shift- [mc Shift+] Alt- [mc Alt+]] $keypress]
				set action [angband macro action $i]
				set item [$tree item create]
				$tree item text $item 0 $keypress 1 $action
				$tree item lastchild root $item
			}
		}
		keymap {
			foreach {keypress action} [angband keymap find] {
				if {[string length $keypress] == 2} {
					set keypress [mc Ctrl+][string index $keypress 1]
				}
				set item [$tree item create]
				$tree item text $item 0 $keypress 1 $action
				$tree item lastchild root $item
			}
		}
	}

	Info $oop loaded,which $which

	set tabsId [Info $oop loaded,tabsId]
	set current [NSTabs::Info $tabsId current]
	set tabId [NSTabs::GetNthId $tabsId [lsearch -exact {macro keymap} $which]]
	if {$tabId != $current} {
		NSTabs::Smaller $tabsId $current
		NSTabs::Bigger $tabsId $tabId
		NSTabs::Info $tabsId current $tabId
	}

	return
}

# NSPrfWindow::Loaded_InvokeTab --
#
#	Called when a tab is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::Loaded_InvokeTab {oop tabsId tabId} {

	set index [lsearch -exact [NSTabs::Info $tabsId id] $tabId]
	Loaded_SetList $oop [lindex {macro keymap} $index]

	return
}

# NSPrfWindow::Loaded_SelectionChanged --
#
#	Called when the "loaded" list selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::Loaded_SelectionChanged {oop count select deselect} {

	set win [Info $oop loaded,win]
	if {$count} {
		$win.unload configure -state normal
	} else {
		$win.unload configure -state disabled
	}

	return
}

# NSPrfWindow::Loaded_Unload --
#
#	Called when the "loaded" list selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPrfWindow::Loaded_Unload {oop} {

	set tree [Info $oop loaded,tree]
	set which [Info $oop loaded,which]

	while {[$tree selection count]} {
		set row [$tree item order "last state selected"]
		incr row -1
		if {$which eq "macro"} {
			angband macro delete $row
		} else {
			set trigger [lindex [angband keymap find] [expr {$row * 2}]]
			angband keymap action $trigger ""
		}
		$tree item delete "last state selected"
	}

	return
}
