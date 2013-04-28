# File: borg.tcl

# Purpose: the Borg Window and related commands

#
# Copyright (c) 1997-2000 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSBorg {

	variable MenuString
	variable Priv

# Help text for options handled by this module. Author-specific Borg
# options are specified by the author-specific borg.tcl files.

set vSettingHelp(delay_factor) \
"The higher the value, the slower the Borg goes."

set vSettingHelp(detach_interface) \
"If you want to watch everthing the Borg does, turn this option off.\
If you want to allow the Borg to run as fast as possible, turn this\
option on."

# namespace eval NSBorg
}

# NSBorg::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::InitModule {} {

	global Angband
	variable Priv

	# This is a private command, not from msgcat.tcl
	MsgCatInit

	NSModule::LoadIfNeeded NSList

	set Priv(page) {}
	lappend Priv(page) Author
	lappend Priv(page) Options
	lappend Priv(page) Other

	# Hack -- Reload borg-specific stuff during debugging
	if {[Global borg,initialized]} {
		Initialize
	}

	# Default values for options
	Global borg,delay_factor 0
	Global borg,detach_interface 0

	# Hack -- Verify that all displayed options are valid
	if {$::DEBUG} {
		foreach page $Priv(page) {
			foreach option [GetPage $page] {
				BorgSetting $option
			}
		}
	}

	# Read each borg.info found in subdirectories of borg/
	variable Prefix
	foreach dir [glob -types d -directory [Path borg] *] {
		set prefix [file tail $dir]
		set path [Path borg $prefix borg.info]
		if {[file exists $path]} {
			NSBorg::Info::Source $prefix $path
			lappend Prefix $prefix
		}
	}
	set Prefix [lsort -dictionary $Prefix]

	# Read borg/setting
	set path [Path borg setting]
	if {[file exists $path]} {
		NSBorg::Setting::Source $path
	}

	set Priv(find,string) ""
	set Priv(find,fromStart) 1

	# Create the Borg Window
	NSObject::New NSBorg

	return
}

# NSBorg::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::CloseModule {} {

	catch {
		destroy [Window borg]
		namespace delete ::Borg
	}

	return
}

# NSBorg::NSBorg --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::NSBorg {oop} {

	global Angband
global NSBorg
	variable Priv

	# Update ourself when the font,options value changes
	qebind NSBorg <Value-font,options> \
		"NSBorg::ValueChanged_font_options $oop"

	Info $oop ignoreChange 0
	Info $oop current -1
	Info $oop current2 -1

	trace variable NSBorg($oop,scale,value) w \
		"NSBorg::EntryTextVarProc $oop"

	InitWindow $oop

	set win [Info $oop win]
	
	Info $oop page none
	Info $oop tempWidget {}

	# Set the list
	Info $oop page [lindex $Priv(page) 0]

	NSWindowManager::RegisterWindow borg $win \
		"GetDefaultGeometry $win reqwidth main" "" \
		"NSBorg::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSBorg $oop $win

	#
	# Global list of application windows
	#

	Global borg,oop $oop
	Window borg $win

	return
}

# NSBorg::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::Info {oop info args} {

	global NSBorg

	# Verify the object
	NSObject::CheckObject NSBorg $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSBorg($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSBorg($oop,$info)
			}
		}
	}

	return
}

# NSBorg::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::InitWindow {oop} {

	variable Priv

	set win .borg$oop
	toplevel $win
	wm title $win [mc Borg]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSBorg::Close $oop"

	Info $oop win $win

	InitMenus $oop

	#
	# Tabs
	#

	set tabsId [NSObject::New NSTabs $win]
	foreach page $Priv(page) {
		NSTabs::Add $tabsId [mc $page]
	}
	NSTabs::Info $tabsId invokeCmd "NSBorg::InvokeTab $oop"
	NSTabs::Info $tabsId active 1
	Info $oop tabsId $tabsId

	#
	# List
	#

	set frame $win.frame
	set tree [NSList::New $frame -checkbutton yes -font options]
	$tree configure -width 400 -height 200

	NSList::OnSelection $tree \
		"NSBorg::SelectionChanged $oop %T %c %S %D"

	NSList::OnToggle $tree \
		"NSBorg::Toggle $oop %T %I %r"

	Info $oop tree $tree

	#
	# Help + divider
	#

	MakeDivider $win.divider2 x
	frame $win.frameHelp \
		-borderwidth 1 -relief sunken
	text $win.frameHelp.text \
		-borderwidth 0 \
		-width 40 -height 5 -background [Value listBG] \
		-foreground White -cursor {} -wrap word \
		-font [Value font,options] -takefocus 0 \
		-yscrollcommand "$win.frameHelp.yscroll set"
	scrollbar $win.frameHelp.yscroll \
		-orient vertical -command "$win.frameHelp.text yview"
	bindtags $win.frameHelp.text [list $win.frameHelp.text $win all]

	# This call updates the list background color whenever the
	# global list background color changes
	qebind NSBorg <Value-listBG> "$win.frameHelp.text configure -background %c"

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 20

	#
	# Geometry
	#

	grid rowconfigure $win.frameHelp 0 -weight 0
	grid rowconfigure $win.frameHelp 1 -weight 1
	grid columnconfigure $win.frameHelp 0 -weight 1
	grid columnconfigure $win.frameHelp 1 -weight 0

	grid $win.frameHelp.text \
		-row 1 -column 0 -rowspan 2 -columnspan 1 -sticky news
	grid $win.frameHelp.yscroll \
		-row 1 -column 1 -rowspan 2 -columnspan 1 -sticky ns

	grid rowconfigure $win 0 -weight 0 -minsize 0
	grid rowconfigure $win 1 -weight 0 -minsize 0
	grid rowconfigure $win 2 -weight 2 -minsize 0
	grid rowconfigure $win 3 -weight 0 -minsize 0
	grid rowconfigure $win 4 -weight 1 -minsize 0
	grid rowconfigure $win 5 -weight 0 -minsize 0
	grid columnconfigure $win 0 -weight 1 -minsize 0

	if {[Platform windows]} {
		grid [MakeDivider $win.divider1 x] \
			-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	}
	grid [NSTabs::Info $tabsId canvas] \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.frame \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.divider2 \
		-row 3 -column 0 -rowspan 1 -columnspan 2 -sticky ew -pady 2
	grid $win.frameHelp \
		-row 4 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.statusBar \
		-row 5 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	bind $win <KeyPress-Escape> "NSBorg::Close $oop"
	bind $win <Control-KeyPress-w> "NSBorg::Close $oop"
	bind $win <KeyPress-f> "NSBorg::Find $oop 0"
	bind $win <KeyPress-g> "NSBorg::Find $oop 1"

	return
}

# NSBorg::InitMenus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::InitMenus {oop} {

	global NSBorg
	variable MenuString
	variable Priv

	set win [Info $oop win]
	set mod "Ctrl"

	#
	# Menu bar
	#

	set mbar [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSBorg::SetupMenus $oop" -identifier MENUBAR]
	Info $oop mbarId $mbar

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSBorg::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSBorg::MenuInvoke $oop"

	#
	# Borg Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_BORG
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_BORG -label [mc Borg] -underline 0 \
		-identifier M_BORG

	set entries {}

	set i 1
	foreach page $Priv(page) {
		lappend entries [list -type radiobutton -label [mc $page] \
			-variable NSBorg($oop,radio) -value $page \
			-accelerator $i -identifier E_PAGE_$i]
		bind $win <KeyPress-$i> "NSBorg::SetOptions $oop $page"
		incr i
	}
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Find...] \
		-underline 0 -accelerator f -identifier E_FIND]
	lappend entries [list -type command -label [mc "Find Again"] \
		-underline 6 -accelerator g -identifier E_FIND_AGAIN]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc "Save As Default"] \
		-underline 0 -identifier E_WRITE_DEFAULT]
	lappend entries [list -type command -label [mc "Load Default Settings"] \
		-underline 0 -identifier E_READ_DEFAULT]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_BORG $entries

	set MenuString(M_BORG) \
		"Contains commands for using game settings."
	set MenuString(E_FIND) \
		"Searches for an option by keyword."
	set MenuString(E_FIND_AGAIN) \
		"Repeats the previous search."
	set MenuString(E_WRITE_DEFAULT) \
		"Makes the current settings the default."
	set MenuString(E_READ_DEFAULT) \
		"Reads the saved settings, if any."
	set MenuString(E_CLOSE) \
		"Closes the window."

	return
}

# NSBorg::SetupMenus --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::SetupMenus {oop mbarID} {

	variable Priv

	set i 0
	foreach page $Priv(page) {
		lappend identList E_PAGE_[incr i]
	}

	lappend identList E_FIND E_FIND_AGAIN
	lappend identList E_WRITE_DEFAULT E_READ_DEFAULT E_SHOW_UNUSED E_CLOSE

	NSMenu::MenuEnable $mbarID $identList

	[Info $oop win].statusBar cover show

	return
}

# NSBorg::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::MenuSelect {oop menuId index ident} {

	variable MenuString
	variable Priv

	switch -glob -- $ident {
		{} {
			set desc {}
		}

		E_PAGE_* {
			set desc "Displays this page."
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

	[Info $oop win].statusBar cover set $desc
	if {![string length $desc]} {
		if {$menuId == [Info $oop mbarId]} {
			[Info $oop win].statusBar cover hide
		}
	}

	return
}

# NSBorg::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::MenuInvoke {oop menuId ident} {

	variable Priv

	switch -glob -- $ident {
		E_PAGE_* {
			scan $ident "E_PAGE_%d" pageNum
			SetOptions $oop [lindex $Priv(page) [expr {$pageNum - 1}]]
		}
		E_FIND {Find $oop 0}
		E_FIND_AGAIN {Find $oop 1}
		E_WRITE_DEFAULT {
			WriteSettings $oop 0
			if {[Global borg,initialized]} {
				WriteSettings $oop 1
			}
		}
		E_READ_DEFAULT {
			ReadSettings $oop 0
			if {[Global borg,initialized]} {
				ReadSettings $oop 1
			}
		}
		E_CLOSE {Close $oop}
	}

	return
}

# NSBorg::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::DisplayCmd {oop message first} {

	set tree [Info $oop tree]

	switch -- $message {
		preDisplay {			
			SetOptions $oop [Info $oop page]

			# Hack -- Restore the selection
			set current [Info $oop current2]
			if {$current != -1} {
				NSList::Activate $tree "root child $current"
			}
		}
		postDisplay {
		}
		postWithdraw {
			Info $oop current2 [Info $oop current]
		}
	}

	return
}

# NSBorg::Close --
#
#	Called by WM_DELETE_WINDOW protocol handler.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	Instead of destroying the window, just withdraw (hide) it.

proc NSBorg::Close {oop} {

	NSWindowManager::Undisplay borg

	return
}

# NSBorg::StatusBar --
#
#	Display text in the status bar, perhaps clearing it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label [$win.statusBar itemcget t1 -label]
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

# NSBorg::Toggle --
#
#	Toggles the value of an option.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSBorg::Toggle {oop tree item row} {

	set option [lindex [Info $oop settings] $row]
	set setting [expr {[BorgSetting $option] ? 0 : 1}]
	NSList::SetCheck $tree $item $setting
	BorgSetting $option $setting

	return
}

# NSBorg::SetOptions --
#
#	Display options on given "page". Usually this is list of
#	on/off options handled by our NSCanvist list. But sometimes
#	the page does not use a list, instead using slider controls.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::SetOptions {oop page} {

	global NSBorg
	variable Priv

	set win [Info $oop win]
	set tree [Info $oop tree]

	foreach widget [Info $oop tempWidget] {
		destroy $widget
	}
	Info $oop tempWidget {}

	eval grid forget [winfo children $win.frame]

	# Clear the help text and keyword
	$win.frameHelp.text delete 1.0 end
	$win.statusBar itemconfigure t2 -text ""	

	switch -- $page {

		Author {
			set message $win.frame.info
			message $message \
				-width 325 -text "Choose the author of the Borg you want\
				to use. This must be done before using the Borg command\
				(Control-Z). Otherwise changes will not take affect until\
				you restart the game."
			pack $message -side top -anchor w -padx 10 -pady 10

			set frameList $win.frame.frameList
			frame $frameList \
				-borderwidth 1 -relief sunken
			listbox $frameList.list \
				-height 5 -width 35 -background White -borderwidth 0 \
				-yscrollcommand "$frameList.yscroll set" -highlightthickness 0
			scrollbar $frameList.yscroll \
				-command "$frameList.list yview"

			bind $frameList.list <<ListboxSelect>> \
				"NSBorg::SelectionChanged_Author $oop \[%W curselection]"

			pack $frameList.list \
				-side left -fill both
			pack $frameList.yscroll \
				-side right -fill y
			pack $frameList \
				-side top

			lappend NSBorg($oop,tempWidget) $message
			lappend NSBorg($oop,tempWidget) $frameList

			# Set the list of borg authors
			variable Prefix
			foreach prefix $Prefix {
				$frameList.list insert end $NSBorg::Info::Author($prefix)
			}

			# Select the current author. This might be different from
			# the currently-loaded DLL.
			set prefix [Value borg,prefix]
			if {[string length $prefix]} {
				set index [lsearch -exact $Prefix $prefix]
				if {$index != -1} {
					$frameList.list selection clear 0 end
					$frameList.list selection set $index
					$frameList.list see $index

					SelectionChanged_Author $oop [list $index]
				}
			}
		}

		Options {
			grid $tree \
				-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
			grid $win.frame.yscroll \
				-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky ns
			NSList::Config $tree -columns 1 -checkbutton yes
			SetList $oop $page
		}

		Other {
			grid $tree \
				-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
			grid $win.frame.yscroll \
				-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky ns

			set data [list \
				delay_factor 0 9 1 \
			]

			NSList::Clear $tree
			NSList::Config $tree -columns 2 -checkbutton no
			$tree column configure "order 1" -justify right

			set settings {}
			set scaleInfo {}
			set itemList {}
			foreach {setting from to resolution} $data {
				set label [BorgSettingDesc $setting]
				set value [BorgSetting $setting]

				set item [$tree item create]
				NSList::SetText $tree $item $label
				NSList::SetTextEx $tree $item "order 1" $value
				$tree item lastchild root $item

				lappend settings $setting
				lappend scaleInfo [list $from $to $resolution]
			}

			Info $oop settings $settings
			Info $oop scaleInfo $scaleInfo

			set width [expr {[winfo width $win.frame] - 60}]

			set frame $win.frame.scale
			frame $frame \
				-borderwidth 0
			scale $frame.scale \
				-orient horizontal -label "(Nothing selected)" \
				-width 8 -sliderlength 12 -length $width -from 1 -to 100 \
				-showvalue no -command "" -state disabled
			entry $frame.entry \
				-width 5 -state disabled -textvariable NSBorg($oop,scale,value)
			bind $frame.entry <FocusIn> {%W selection range 0 end}
			bind $frame.entry <KeyPress-Escape> "NSBorg::Close $oop"
			bindtags $frame.entry [list $frame.entry Entry all]
			$frame.scale set 50
			pack $frame.scale -side left
			pack $frame.entry -side left
			grid $frame -row 1 -column 0 -rowspan 1 -columnspan 2
			lappend NSBorg($oop,tempWidget) $frame
		}
	}

	wm title $win "Borg ($page)"

	Info $oop page $page
	Info $oop radio $page

	set tabsId [Info $oop tabsId]
	set current [NSTabs::Info $tabsId current]
	set tabId [NSTabs::GetNthId $tabsId [lsearch -exact $Priv(page) $page]]
	if {$tabId != $current} {
		NSTabs::Smaller $tabsId $current
		NSTabs::Bigger $tabsId $tabId
		NSTabs::Info $tabsId current $tabId
	}

	return
}

# NSBorg::SetList --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::SetList {oop page} {

	set tree [Info $oop tree]

	NSList::Clear $tree

	set settings {}
	set itemList {}
	foreach keyword [GetPage $page] {
		set setting [BorgSetting $keyword]
		if {$setting} {
			set image Image_Checked
		} else {
			set image Image_Unchecked
		}
		set desc [BorgSettingDesc $keyword]
		set color White

		set item [$tree item create]
		NSList::SetCheck $tree $item $setting
		NSList::SetText $tree $item $desc
		$tree item lastchild root $item

		lappend settings $keyword
	}

	Info $oop settings $settings

	return
}

# NSBorg::GetPage --
#
#	Return a list of option-variable keywords.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::GetPage {page} {

	switch -- $page {

		Author {
		}

		Options {
			lappend optionList \
				detach_interface
			if {[Global borg,initialized]} {
				set optionList [concat $optionList $Borg::vSetting]
			}
			return $optionList
		}

		Other {
			lappend optionList \
				delay_factor
			return $optionList
		}
	}

	return
}

# NSBorg::SelectionChanged --
#
#	Called when the list selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::SelectionChanged {oop tree count select deselect} {

	set win [Info $oop win]

	Info $oop ignoreChange 1

$win.frameHelp.text delete 1.0 end

	if {[llength $select]} {
		set item [lindex $select 0]
		set row [NSList::Item2Row $tree $item]
		Info $oop current $row

		set keyword [lindex [Info $oop settings] $row] 
		$win.statusBar itemconfigure t2 -text $keyword

DisplaySettingHelp $oop $keyword

		if {[winfo exists $win.frame.scale]} {
			$win.frame.scale.entry configure -state normal
			set scaleInfo [lindex [Info $oop scaleInfo] $row]
			$win.frame.scale.scale configure \
				-from [lindex $scaleInfo 0] -to [lindex $scaleInfo 1] \
				-resolution [lindex $scaleInfo 2] \
				-label [BorgSettingDesc $keyword] -state normal \
				-command "NSBorg::ScaleCmd $oop $keyword"
			Info $oop scale,value [BorgSetting $keyword]
			$win.frame.scale.scale set [BorgSetting $keyword]
		}

	} else {
		Info $oop current -1

		$win.statusBar itemconfigure t2 -text ""

		if {[winfo exists $win.frame.scale]} {
			$win.frame.scale.entry configure -state disabled
			Info $oop scale,value 1
			$win.frame.scale.scale set 1
			$win.frame.scale.scale configure \
				-from 1 -to 100 \
				-label "(Nothing selected)" -state disabled \
				-command ""
		}
	}

	# Must update now, because scale's command is called at
	# idle time!
	update idletasks

	Info $oop ignoreChange 0

	return
}

# NSBorg::InvokeTab --
#
#	Called when a tab is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::InvokeTab {oop tabsId tabId} {

	variable Priv

	set index [lsearch -exact [NSTabs::Info $tabsId id] $tabId]
	SetOptions $oop [lindex $Priv(page) $index]

	return
}

# NSBorg::ScaleCmd --
#
#	Called when the value of a scale changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::ScaleCmd {oop keyword value} {

	if {[Info $oop ignoreChange]} return

	# Update the game setting
	BorgSetting $keyword $value

	# Update the list
	UpdateList $oop

	# Update the entry
	Info $oop scale,value $value

	return
}

# NSStore::EntryTextVarProc --
#
#	Trace variable callback on NSBorg($oop,scale,value).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::EntryTextVarProc {oop name1 name2 op} {

	if {[Info $oop ignoreChange]} return

	set value [Info $oop scale,value]
	set scaleInfo [lindex [Info $oop scaleInfo] [Info $oop current]]
	set from [lindex $scaleInfo 0]
	set to [lindex $scaleInfo 1]

	regsub -all \[^0-9] $value "" value
	if {[string length $value]} {
		if {$value < $from} {set value $from}
		if {$value > $to} {set value $to}
		[Info $oop win].frame.scale.scale set $value
	}

	Info $oop scale,value $value

	return
}

# NSBorg::SelectionChanged_Author --
#
#	Called when the selected author changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::SelectionChanged_Author {oop selection} {

	variable Prefix

	if {[llength $selection]} {
		set row [lindex $selection 0]
		set prefix [lindex $Prefix $row]
		Value borg,prefix $prefix

		set text [Info $oop win].frameHelp.text
		$text delete 1.0 end
		$text insert end $NSBorg::Info::Author($prefix)\n
		$text insert end $NSBorg::Info::Desc($prefix) tag1
		$text tag configure tag1 \
			-lmargin1 19 -lmargin2 19 -rmargin 0
	}

	return
}

# NSBorg::UpdateList --
#
#	When the scale value changes, we want to update the list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::UpdateList {oop} {

	set tree [Info $oop tree]
	set row [Info $oop current]

	set keyword [lindex [Info $oop settings] $row]
	NSList::SetTextEx $tree "root child $row" 1 [BorgSetting $keyword]

	return
}

# NSBorg::DisplaySettingHelp --
#
#	Display the help text for the given setting keyword.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::DisplaySettingHelp {oop keyword} {

	set text [Info $oop win].frameHelp.text

	set desc [BorgSettingDesc $keyword]
	set help [GetSettingHelp $oop $keyword]

	$text insert end $desc\n
	$text insert end $help tag1
	$text tag configure tag1 \
		-lmargin1 19 -lmargin2 19 -rmargin 0

	ForceTextWidgetViewUpdate $text

	return
}

# NSBorg::GetSettingHelp --
#
#	Return help-text for the given setting.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::GetSettingHelp {oop keyword} {

	variable vSettingHelp

	if {[info exists vSettingHelp($keyword)]} {
		return $vSettingHelp($keyword)
	}
	return $Borg::vSettingHelp($keyword)

	return $result
}

# NSBorg::WriteSettings --
#
#	Save the current settings to the tk/config/setting file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::WriteSettings {oop borgSpecific} {

	variable Priv

	if {!$borgSpecific} {
		set answer [tk_messageBox -title "Write Settings" -icon question \
			-parent [Info $oop win] \
			-type yesno -message "Really make these the default settings?"]
		if {[string equal $answer no]} return
	}

	set tempName [NSUtils::TempFileName [Path borg]]
	if {[catch {openlf $tempName} fileId]} {
		set msg "The following error occurred while attempting to open "
		append msg "the \"setting\" file for writing:\n\n$fileId"
		tk_messageBox -title Oops -parent [Info $oop win] -message $msg
		return
	}

	StatusBar $oop "Writing settings..." 0

	if {$borgSpecific} {
		set data $Borg::vSetting
	} else {
		foreach page $Priv(page) {
			if {[string equal $page Cheating]} continue
			foreach keyword [GetPage $page] {
				if {[Global borg,initialized]} {
					if {[lsearch -exact $Borg::vSetting $keyword] != -1} continue
				}
				lappend data $keyword
			}
		}
	}

	puts $fileId "# Automatically generated. Do not edit.\n"

	foreach keyword [lsort $data] {
		set value [BorgSetting $keyword]
		puts $fileId "One $keyword \"$value\""
	}

	close $fileId

	if {$borgSpecific} {
		set fileName [Path borg [Global borg,prefix] setting]
	} else {
		set fileName [Path borg setting]
	}
	if {[file exists $fileName]} {
		file rename -force -- $fileName $fileName.bak
	}
	file rename -- $tempName $fileName

	StatusBar $oop "Done." 1

	return
}

# NSBorg::ReadSettings --
#
#	Override current settings with those in the tk/config/setting file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::ReadSettings {oop borgSpecific} {

	if {$borgSpecific} {
		set fileName [Path borg [Global borg,prefix] setting]
	} else {
		set fileName [Path borg setting]
	}
	if {![file exists $fileName]} {
		tk_messageBox -title "Read Settings" -icon info -type ok \
			-parent [Info $oop win] \
			-message "There are no default settings to restore."
		return
	}

	if {!$borgSpecific} {
		set answer [tk_messageBox -title "Read Settings" -icon question \
			-type yesno -parent [Info $oop win] \
			-message "Really read the saved settings?"]
		if {[string equal $answer no]} return
	}

	NSBorg::Setting::Source $fileName

	SetOptions $oop [Info $oop page]

	StatusBar $oop "Done." 1

	return
}

# ValueChanged_font_options --
#
#	Called when the font,options value changes.
#	Updates the Options Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::ValueChanged_font_options {oop} {

	# Set the help-text font
	set text [Info $oop win].frameHelp.text
	$text configure -font [Value font,options]

	return
}

# NSBorg::Find --
#
#	Simple search routine to look for an option by name.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::Find {oop again} {

	variable Priv

	# Repeat the last search
	if {$again && [string length $Priv(find,string)]} {
		set string $Priv(find,string)

	# Enter a string to find, start from the beginning
	} else {

		# Ask the user for a name
		set string [NSUtils::StringBox -title [mc Find] \
			-initial $Priv(find,string) \
			-prompt Keyword -buttons [list [mc OK] [mc Cancel]] \
			-parent [Info $oop win]]
		if {![string length $string]} return

		# Clean up after the dialog, give message
		StatusBar $oop "Searching..." 1
		update

		# Reset search parameters
		set Priv(find,string) $string
	}

	# Get a list of pages
	set pageList $Priv(page)

	# Default to searching from the beginning
	set page 0
	set member 0

	# Don't search from the start
	if {!$Priv(find,fromStart)} {

		# Search in current page
		set page [lsearch -exact $pageList [Info $oop page]]

		# An option is selected
		if {[Info $oop current] != -1} {

			# Search after the selected option
			set member [expr {[Info $oop current] + 1}]
		}
	}

	# Compare lowercase
	set string [string tolower $string]

	# Check each page
	for {set i $page} {$i < [llength $pageList]} {incr i} {

		set keywordList {}

		# Check each option on this page
		foreach keyword [GetPage [lindex $pageList $i]] {

			# Remember option
			lappend keywordList $keyword
		}

		# Check each keyword
		foreach keyword [lrange $keywordList $member end] {

			# Compare lowercase
			set keyword [string tolower $keyword]

			# Found a match
			if {[string first $string $keyword] != -1} {

				# The option is not on the displayed page
				if {$i != [lsearch -exact $pageList [Info $oop page]]} {

					# Display the new page
					SetOptions $oop [lindex $pageList $i]
				}

				# Get the list
				set tree [Info $oop tree]
	
				# View the option
				focus $tree
				NSList::Activate $tree "root child $member"

				# Don't search from start next time
				set Priv(find,fromStart) 0

				# Clear "Searching..." message
				StatusBar $oop "" 0

				# Done
				return
			}

			# Next member
			incr member
		}

		# First member in the next group
		set member 0
	}

	# If we didn't search from the start, then wrap around
	if {!$Priv(find,fromStart)} {

		# Search from the start
		set Priv(find,fromStart) 1

		# Search again
		Find $oop 1

		# Done
		return
	}

	# Not found
	StatusBar $oop "No match for \"$string\"." 1

	return
}

# NSBorg::MsgCatInit --
#
#	Sets up msgcat support for this module.
#	This is nearly the same as MsgCatInit() in msgcat.tcl, but
#	we aren't reading from tk/msgs.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::MsgCatInit {} {

	# Import all the msgcat commands into the calling namespace, so we
	# can write "mc Foo" instead of "::msgcat::mc Foo".
	uplevel {namespace import ::msgcat::*}

	# XXX Hack -- Always include the "common" translations
	uplevel ::msgcat::mcload [list [file join $::Angband(dir,msgs) common]]

	# Load the message file from the given directory
	uplevel ::msgcat::mcload [list [Path borg msgs]]

	return
}

# NSBorg::BorgSetting --
#
#	Get or set the value of a setting. This one routine consolidates
#	all the different setting-related commands.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::BorgSetting {keyword args} {

	# Set
	if {[llength $args]} {
		set value [lindex $args 0]
		switch -- $keyword {
			delay_factor {
				Global borg,$keyword $value
				if {[Global borg,initialized]} {
					Borg::SettingSet $keyword $value
				}
			}
			detach_interface {
				Global borg,$keyword $value
			}
			default {
				Borg::SettingSet $keyword $value
			}
		}

	# Get
	} else {
		switch -- $keyword {
			delay_factor -
			detach_interface {
				return [Global borg,$keyword]
			}
			default {
				return [Borg::SettingGet $keyword]
			}
		}
	}

	return
}

# NSBorg::BorgSettingDesc --
#
#	Return the human-readable description for a setting.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::BorgSettingDesc {keyword} {

	switch -- $keyword {
		delay_factor {
			return "Borg delay factor"
		}
		detach_interface {
			return "Don't update the display while the Borg is active"
		}
		default {
			return [Borg::SettingDesc $keyword]
		}
	}
}

# NSBorg::Initialize --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBorg::Initialize {} {

	# Source the borg-specific script
	uplevel #0 source [list [Path borg [Global borg,prefix] borg.tcl]]

	# Load the borg-specific settings if present
	set path [Path borg [Global borg,prefix] setting]
	if {[file exists $path]} {
		NSBorg::Setting::Source $path
	}

	return
}

# NSBorg::Setting --
#
#	A namespace with commands called when the borg/setting file
#	is sourced.
#

namespace eval NSBorg::Setting {

# namespace eval NSBorg::Setting
}

proc NSBorg::Setting::Source {path} {

	source $path

	return
}

proc NSBorg::Setting::One {keyword value} {

	# Ignore errors, such as outdated settings
	catch {
		NSBorg::BorgSetting $keyword $value
	}

	return
}

# NSBorg::Info --
#
#	A namespace with commands called when a borg.info file
#	is sourced.
#

namespace eval NSBorg::Info {

	variable Prefix
	variable Author
	variable Desc

# namespace eval NSBorg::Info
}

proc NSBorg::Info::Source {prefix path} {

	variable Prefix

	set Prefix $prefix	
	source $path

	return
}

proc NSBorg::Info::Author {author} {

	variable Author
	variable Prefix

	set Author($Prefix) $author

	return
}

proc NSBorg::Info::Description {desc} {

	variable Desc
	variable Prefix

	set Desc($Prefix) $desc

	return
}
