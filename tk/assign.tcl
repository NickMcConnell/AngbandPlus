# File: assign.tcl

# Purpose: the Assign Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSAssign {

	variable MenuString
	variable Priv

# namespace eval NSAssign
}

# NSAssign::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::InitModule {} {

	variable Priv

	MsgCatInit know assign

	InitImageIfNeeded Image_Sound sound.gif
	InitImageIfNeeded Image_ArrowLeft arrow-left.gif
	InitImageIfNeeded Image_ArrowRight arrow-right.gif

	NSModule::LoadIfNeeded NSIconBrowser

	set Priv(hook) {}
	lappend Priv(hook) character
	lappend Priv(hook) effect
	lappend Priv(hook) feature
	lappend Priv(hook) monster
	lappend Priv(hook) object

	set Priv(find,string) ""
	set Priv(find,fromStart) 1

	set Priv(didAssign) 0
	qebind NSAssign <Assign> {
		set NSAssign::Priv(didAssign) 1
	}

	# Create the Assign Window
	NSObject::New NSAssign

	return
}

# NSAssign::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::CloseModule {} {

	catch {
		destroy [Window assign]
	}

	return
}

# NSAssign::NSAssign --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::NSAssign {oop} {

	variable Priv

	InitWindow $oop

	Info $oop hook monster
	Info $oop group,current -1
	Info $oop member,current -1
	Info $oop display icon
	Info $oop display,ignoreSel 0
	Info $oop radio,display icon
	Info $oop soundAssign,ignoreSel 0
	Info $oop sound,match {}
	Info $oop sound,dir [lindex [angband sound dir listof] 0]
	Info $oop sound,config [lindex [angband sound config listof] 0]

	# Info about selected group & member for each hook
	foreach hook $Priv(hook) {
		Info $oop group,$hook 0
		Info $oop member,$hook -1
	}

	set win [Info $oop win]

	NSWindowManager::RegisterWindow assign $win \
		"GetDefaultGeometry $win reqwidth main2" "NSAssign::SetupCmd $oop" \
		"NSAssign::DisplayCmd $oop"

	# Update ourself when the font,knowledge value changes
	qebind NSAssign <Value-font,knowledge> \
		"NSAssign::ValueChanged_font_knowledge $oop"

	qebind NSAssign <IconCfg> \
		"NSAssign::IconCfg $oop"

	bind $win <KeyPress-Escape> "NSAssign::Close $oop"
	bind $win <Control-KeyPress-w> "NSAssign::Close $oop"
	bind $win <KeyPress-f> "NSAssign::Find $oop 0"
	bind $win <KeyPress-g> "NSAssign::Find $oop 1"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSAssign $oop $win

	#
	# Global access
	#

	Window assign $win
	Global assign,oop $oop

	return
}

# NSAssign::~NSAssign --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::~NSAssign {oop} {

	return
}

# NSAssign::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::Info {oop info args} {

	global NSAssign

	# Verify the object
	NSObject::CheckObject NSAssign $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSAssign($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSAssign($oop,$info)
			}
		}
	}

	return
}

# NSAssign::InitWindow --
#
#	Create the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::InitWindow {oop} {

	variable Priv

	set win .assign$oop
	toplevel $win
	wm title $win [mc Assign]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSAssign::Close $oop"

	Info $oop win $win

	InitMenus $oop

	#
	# Tabs for groups of things to assign to
	#

	set tabsId [NSObject::New NSTabs $win]
	foreach hook $Priv(hook) {
		NSTabs::Add $tabsId [mc $hook]
	}
	NSTabs::Info $tabsId invokeCmd "NSAssign::InvokeTab $oop"
	NSTabs::Info $tabsId active 1
	Info $oop tabsId $tabsId

	#
	# Group List
	#

	set width [CalcGroupListWidth $oop]

	set frame $win.frameGroup
	NSList::New $frame -icon 1
	set tree $frame.tree
	$tree configure -width $width -height [expr {40 * 5}]

	# Do something when a group item is selected
	NSList::OnSelection $tree \
		"NSAssign::SelectionChanged_Group $oop %T %c %S %D"

	Info $oop group,tree $tree

	#
	# Member List
	#

	set width 300

	set frame $win.frameMember
	NSList::New $frame -icon 1 -xscrollbar 0 -columns 2
	set tree $frame.tree
	$tree configure -width $width -height [expr {40 * 5}] -xscrollincrement 1

	# Show Image_Sound when a sound is assigned
	$tree state define hasSound
	$tree element create eImg image -image {Image_Sound hasSound}
	set S [$tree style create sKM]
	$tree style elements $S {eSel.w eImg}
	$tree style layout $S eSel.w -padx {0 1} -ipadx {0 3} -pady 1 -ipady 3 -iexpand news -union eImg
	$tree style layout $S eImg -expand ns

	# Features show 2 icons
	$tree element create eAss2 assign
	set S [$tree style create sKF]
	$tree style elements $S {eSel eAss eAss2 eTxt}
	$tree style layout $S eSel -padx 1 -pady 1 -ipadx 3 -ipady 3 -iexpand ew -union {eAss eAss2 eTxt}
	$tree style layout $S eAss -expand ns
	$tree style layout $S eAss2 -expand ns -padx 4
	$tree style layout $S eTxt -expand ns

	# Do something when a member item is selected.
	NSList::OnSelection $tree \
		"NSAssign::SelectionChanged_Member $oop %T %c %S %D"

	# Do something when a selected member is clicked.
	NSList::OnClick $tree \
		"NSAssign::Click_Member $oop %r"
	NSList::OnInvoke $tree \
		"NSAssign::Click_Member $oop %r"

	Info $oop member,tree $tree

	MakeDivider $win.divider1 x

	#
	# Tabs for groups of things to assign
	#

	set tabs2Id [NSObject::New NSTabs $win]
	foreach what {Alternate Icon Sound Sprite} {
		NSTabs::Add $tabs2Id [mc $what]
	}
	NSTabs::Info $tabs2Id invokeCmd "NSAssign::InvokeTab2 $oop"
	NSTabs::Info $tabs2Id active 1
	Info $oop tabs2Id $tabs2Id

	#
	# An NSIconBrowser lets the user examine all icon types
	#

	set browserId [NSObject::New NSIconBrowser $win]
	set tree [NSIconBrowser::Info $browserId member,tree]
	NSList::OnSelection $tree \
		"NSAssign::SelectionChanged_Icon $oop %T %c %S %D"
	set tree [NSIconBrowser::Info $browserId group,tree]
	$tree configure -height [expr {40 * 3}]
	set tree [NSIconBrowser::Info $browserId member,tree]
	$tree configure -height [expr {40 * 3}]

	# Display progress while listing an icon type
	NSIconBrowser::Info $browserId clientCmd \
		"NSAssign::BrowserCmd $oop"

	NSIconBrowser::Info $browserId group,motionCmd \
		"NSAssign::BrowserMotionCmd $oop group"
	NSIconBrowser::Info $browserId group,leaveCmd \
		"NSAssign::BrowserMotionCmd $oop group"

	NSIconBrowser::Info $browserId member,motionCmd \
		"NSAssign::BrowserMotionCmd $oop member"
	NSIconBrowser::Info $browserId member,leaveCmd \
		"NSAssign::BrowserMotionCmd $oop member"

	# Remember the icon browser
	Info $oop browserId $browserId

	#
	# Sound List + Buttons + List of Assigned Sounds
	#

	frame $win.frameSound \
		-borderwidth 0

	#
	# Sound List
	#

	set frame $win.frameSound.frameSound
	NSList::New $frame
	set tree $frame.tree
	$tree configure -height [expr {40 * 3}] -showheader yes -selectmode extended
	$tree column configure 0 -text [mc "Sound Files"]

	NSList::OnSelection $tree \
		"NSAssign::SelectionChanged_Sound $oop %T %c %S %D"

	# Do something when a selected sound is clicked.
	NSList::OnClick $tree \
		"NSAssign::Click_Sound $oop %r"
	NSList::OnInvoke $tree \
		"NSAssign::Click_Sound $oop %r"

	Info $oop sound,tree $tree

	#
	# Buttons
	#

	set frame $win.frameSound.frameButton
	frame $frame \
		-borderwidth 0
	tk::button $frame.buttonAdd -image Image_ArrowRight -width 18 -height 20 \
		-state disabled -command "NSAssign::InsertSound $oop"
	tk::button $frame.buttonRemove -image Image_ArrowLeft -width 18 -height 20 \
		-state disabled -command "NSAssign::DeleteSound $oop"
	pack $frame.buttonAdd -side top -pady {0 5}
	pack $frame.buttonRemove -side top

	Info $oop sound,button,add $frame.buttonAdd
	Info $oop sound,button,remove $frame.buttonRemove

	#
	# Sound-Assign List
	#

	set frame $win.frameSound.frameAssign
	NSList::New $frame
	set tree $frame.tree
	$tree configure -height [expr {40 * 3}] -showheader yes -selectmode extended
	$tree column configure 0 -text [mc "Assigned Sounds"]

	NSList::OnSelection $tree \
		"NSAssign::SelectionChanged_SoundAssign $oop %T %c %S %D"

	# Do something when a selected sound assignment is clicked.
	NSList::OnClick $tree \
		"NSAssign::Click_SoundAssign $oop %r"
	NSList::OnInvoke $tree \
		"NSAssign::Click_SoundAssign $oop %r"

	Info $oop soundAssign,tree $tree

	set frame $win.frameSound
	grid rowconfigure $frame 0 -weight 1
	grid columnconfigure $frame 0 -weight 1
	grid columnconfigure $frame 1 -weight 0
	grid columnconfigure $frame 2 -weight 1
	grid $frame.frameSound \
		-column 0 -row 0 -columnspan 1 -rowspan 1 -sticky news
	grid $frame.frameButton \
		-column 1 -row 0 -columnspan 1 -rowspan 1 -padx 4
	grid $frame.frameAssign \
		-column 2 -row 0 -columnspan 1 -rowspan 1 -sticky news

	#
	# The list of sprites
	#

	set frameSprite $win.frameSprite
	set tree [NSList::New $frameSprite -icon 1 -text 0 -wrap 1]
	$tree configure -width [expr {40 * 10}] -height [expr {40 * 3}]

	NSList::OnSelection $tree \
		"NSAssign::SelectionChanged_Sprite $oop %T %c %S %D"

	Info $oop sprite,tree $tree

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 20

	# Progress bar used to display progress of listing icons
	set label [$win.statusBar itemcget t1 -label]
	set progId [NSObject::New NSProgress2 $label 225 10]
	[NSProgress2::Info $progId frame] configure -borderwidth 0
	Info $oop progId $progId

	# Clicking in label2 displays the icon assigned to the selected
	# member.
	bind [$win.statusBar itemcget t2 -label] <ButtonPress-1> \
		"NSAssign::DisplayIcon $oop"

	#
	# Geometry
	#

	grid rowconfig $win 0 -weight 0 -minsize 0
	grid rowconfig $win 1 -weight 0 -minsize 0
	grid rowconfig $win 2 -weight 1 -minsize 0
	grid rowconfig $win 3 -weight 0 -minsize 0
	grid rowconfig $win 4 -weight 0 -minsize 0
	grid rowconfig $win 5 -weight 1 -minsize 0
	grid rowconfig $win 6 -weight 0 -minsize 0
	grid columnconfig $win 0 -weight 0 -minsize 0
	grid columnconfig $win 1 -weight 1 -minsize 0
 
	if {[Platform windows]} {
		grid [MakeDivider $win.divider2 x] \
			-row 0 -column 0 -rowspan 1 -columnspan 2 -sticky ew -pady 2
	}
	grid [NSTabs::Info $tabsId canvas] \
		-row 1 -column 0 -rowspan 1 -columnspan 2 -sticky ew
	grid $win.frameGroup -in $win \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky ns
	grid $win.frameMember -in $win \
		-row 2 -column 1 -rowspan 1 -columnspan 1 -sticky news
	grid $win.divider1 -in $win \
		-row 3 -column 0 -rowspan 1 -columnspan 2 -sticky ew \
		-pady 2
	grid [NSTabs::Info $tabs2Id canvas] \
		-row 4 -column 0 -rowspan 1 -columnspan 2 -sticky ew
	grid [NSIconBrowser::Info $browserId frame] \
		-row 5 -column 0 -rowspan 1 -columnspan 2 -sticky news
	grid $win.statusBar -in $win \
		-row 6 -column 0 -rowspan 1 -columnspan 2 -sticky ew

	return
}

# NSAssign::InitMenus --
#
#	Create the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::InitMenus {oop} {

	variable MenuString
	variable Priv

	set win [Info $oop win]
	set mod "Ctrl"

	#
	# Menu bar
	#

	set mbar [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSAssign::MenuSetup $oop" -identifier MENUBAR]
	Info $oop mbarId $mbar

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSAssign::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSAssign::MenuInvoke $oop"

	#
	# Assign To
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_ASSIGN_TO
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_ASSIGN_TO -label [mc "Assign To"] -underline 7 \
		-identifier M_ASSIGN_TO

	set entries {}
	set i 1
	foreach hook $Priv(hook) {
		lappend entries [list -type radiobutton -label [mc $hook] \
			-variable NSAssign($oop,radio,hook) -value $hook \
			-accelerator $i -identifier E_HOOK_$hook]
		bind $win <KeyPress-$i> "NSAssign::SetHook $oop $hook"
		incr i
	}
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Find...] \
		-accelerator f -underline 0 -identifier E_FIND]
	lappend entries [list -type command -label [mc "Find Again"] \
		-accelerator g -underline 6 -identifier E_FIND_AGAIN]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_ASSIGN_TO $entries

	#
	# Assign What Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_ASSIGN_WHAT
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_ASSIGN_WHAT -label [mc "Assign What"] -underline 7 \
		-identifier M_ASSIGN_WHAT

	set entries {}
	lappend entries [list -type radiobutton -label [mc Alternate] \
		-variable NSAssign($oop,radio,display) -value alternate \
		-identifier E_ASSIGN_ALTERNATE]
	lappend entries [list -type radiobutton -label [mc Icon] \
		-variable NSAssign($oop,radio,display) -value icon \
		-identifier E_ASSIGN_ICON]
	lappend entries [list -type radiobutton -label [mc Sound] \
		-variable NSAssign($oop,radio,display) -value sound \
		-identifier E_ASSIGN_SOUND]
	lappend entries [list -type radiobutton -label [mc Sprite] \
		-variable NSAssign($oop,radio,display) -value sprite \
		-identifier E_ASSIGN_SPRITE]

	NSMenu::MenuInsertEntries $mbar -end MENU_ASSIGN_WHAT $entries

	#
	# Sound
	# This menu is only displayed when assigning sounds.
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_SOUND

	set id [NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_SOUND_CONFIG]
	Info $oop menu,MENU_SOUND_CONFIG [NSMenu::Info $id menu]
	set id [NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_SOUND_DIR]
	Info $oop menu,MENU_SOUND_DIR [NSMenu::Info $id menu]

	set entries {}
	lappend entries [list -type cascade -label [mc "Configuration"] \
		-menu MENU_SOUND_CONFIG -underline 0 -identifier M_SOUND_CONFIG]
	lappend entries [list -type cascade -label [mc "Directory"] \
		-menu MENU_SOUND_DIR -underline 0 -identifier M_SOUND_DIR]

	NSMenu::MenuInsertEntries $mbar -end MENU_SOUND $entries

	#
	# Hook Menu
	#

	set m [NSMenu::Info $mbar menu].hook
	menu $m -tearoff 0
	bind $m <<MenuSelect>> \
		"NSAssign::MenuSelect_Hook $oop hook %W"
	Info $oop hookMenu,menu $m
	Info $oop hookMenu,inserted 0

	set MenuString(M_ASSIGN_TO) \
		"Contains commands for displaying and searching groups."
	set MenuString(E_FIND) \
		"Searches for a member by name."
	set MenuString(E_FIND_AGAIN) \
		"Repeats the previous search."
	set MenuString(E_CLOSE) \
		"Closes the window."

	set MenuString(M_ASSIGN_WHAT) \
		"Contains commands for displaying things to assign."
	set MenuString(E_ASSIGN_ALTERNATE) \
		"Displays a list of alterates to assign."
	set MenuString(E_ASSIGN_ICON) \
		"Displays a list of icon types to assign."
	set MenuString(E_ASSIGN_SOUND) \
		"Displays a list of sounds to assign."
	set MenuString(E_ASSIGN_SPRITE) \
		"Displays a list of sprites to assign."

	set MenuString(M_SOUND) \
		"Contains commands for using sounds."

	return
}

# NSAssign::MenuSetup --
#
#	Prepare to post the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::MenuSetup {oop mbarId} {

	variable Priv

	lappend identList E_FIND E_FIND_AGAIN E_CLOSE

	foreach hook $Priv(hook) {
		lappend identList E_HOOK_$hook
	}

	lappend identList E_ASSIGN_ALTERNATE E_ASSIGN_ICON E_ASSIGN_SPRITE \
		E_ASSIGN_SOUND

	if {[Info $oop display] eq "sound"} {

		# Configurations
		set cfgList [angband sound config listof]
		if {[llength $cfgList]} {
			lappend identList M_SOUND_CONFIG
			set menu [Info $oop menu,MENU_SOUND_CONFIG]
			$menu delete 0 end
			foreach config $cfgList {
				set title [angband sound config cget $config -title]
				$menu add radiobutton -label $title \
					-variable ::NSAssign($oop,sound,config) -value $config \
					-command "NSAssign::SoundConfigChanged $oop"
			}
		}

		# Directories
		set dirList [angband sound dir listof]
		if {[llength $dirList]} {
			lappend identList M_SOUND_DIR
			set menu [Info $oop menu,MENU_SOUND_DIR]
			$menu delete 0 end
			foreach dir $dirList {
				set path [angband sound dir cget $dir -path]
				$menu add radiobutton -label $path \
					-variable ::NSAssign($oop,sound,dir) \
					-value $dir -command "NSAssign::SetList_Sound $oop"
			}
		}
	}

	if {[Info $oop hookMenu,inserted]} {
		set menu [Info $oop hookMenu,menu]
		set last [$menu index end]
		for {set i 0} {$i <= $last} {incr i} {
			if {[$menu type $i] eq "separator"} continue
			menuentrystate $menu $i disabled
		}
		CallHook $oop menu_setup
	}

	NSMenu::MenuEnable $mbarId $identList

	return
}

# NSAssign::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::MenuSelect {oop menuId index ident} {

	variable MenuString

	switch -glob -- $ident {
		{} {
			# Hack -- If there is no identifier but the index isn't "none",
			# it means the hook menu is highlighted.
			if {$index ne "none"} {
				set desc "Contains commands specific to this group."
			} else {
				set desc {}
			}
		}

		E_HOOK_* {
			set desc "Displays this group."
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

# NSAssign::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::MenuInvoke {oop menuId ident} {

	variable Priv

	switch -glob -- $ident {
		E_HOOK_* {
			scan $ident "E_HOOK_%s" hook
			SetHook $oop $hook
		}
		E_FIND {Find $oop 0}
		E_FIND_AGAIN {Find $oop 1}
		E_CLOSE {Close $oop}

		E_ASSIGN_* {
			scan $ident "E_ASSIGN_%s" display
			set display [string tolower $display]
			SetDisplay $oop $display
		}
	}

	return
}

# NSAssign::MenuSelect_Hook --
#
#	Displays a help string associated with a menu entry in the
#	hook menu or a submenu of the hook menu.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::MenuSelect_Hook {oop which menu} {

	# Get the index of the active menu entry
	set index [$menu index active]

	# No entry is selected
	if {$index eq "none"} {
		set string ""

	# An entry is selected
	} else {
		set ident [lindex [Info $oop hookMenu,ident,$which] $index]
		set string [CallHook $oop menu_string $which $ident]
	}

	StatusBar $oop $string 0

	return
}

# NSAssign::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::DisplayCmd {oop message first args} {

	variable Priv

	switch -- $message {
		preDisplay {

			# Sound config & dir may change
			set i [lsearch -exact [angband sound config listof] [Info $oop sound,config]]
			if {$i == -1} {
				Info $oop sound,config ""
			}
			set i [lsearch -exact [angband sound dir listof] [Info $oop sound,dir]]
			if {$i == -1} {
				Info $oop sound,dir ""
			}

			if {[llength $args]} {
				set hook [lindex $args 0]
				Info $oop hook $hook
				if {[llength $args] > 1} {
					set index [lindex $args 1]
					DisplayMember $oop hook_$hook $index
				}
			}
			set Priv(didAssign) 0

			SetDisplay $oop [Info $oop display]
			SetHook $oop [Info $oop hook]
		}
		postDisplay {
			if {$first} {
				set browserId [Info $oop browserId]
				set tree [NSIconBrowser::Info $browserId group,tree]
				NSIconBrowser::SetList_Group $browserId
				$tree selection add "first visible"
				SetList_Sound $oop
			}

			# If the list of sounds changed, set the list again
			set dir [Info $oop sound,dir]
			if {[info exists ::Global(sound,globCount,$dir)]} {
				if {[Info $oop sound,globCount,$dir] != [Global sound,globCount,$dir]} {
					SetList_Sound $oop
				}
			}
		}
		postWithdraw {
			StateRemember $oop
			NSList::Clear [Info $oop group,tree]
			NSList::Clear [Info $oop member,tree]
			NSList::Clear [Info $oop sprite,tree]
		}
	}

	return
}

# NSAssign::SetupCmd --
#
#	Called by NSWindowManager::Setup().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::SetupCmd {oop} {

	set win [Info $oop win]

	grid $win.frameSound \
		-row 4 -column 0 -rowspan 1 -columnspan 2 -sticky news
	update idletasks
	grid forget $win.frameSound

	grid $win.frameSprite \
		-row 4 -column 0 -rowspan 1 -columnspan 2 -sticky news
	update idletasks
	grid forget $win.frameSprite

	return
}

# NSAssign::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::Close {oop} {

	variable Priv

	if {$::DEBUG} {
		if {$Priv(didAssign)} {
			set answer [tk_messageBox -icon question -type yesno \
				-parent [Info $oop win] -message "WriteAssignFile() Now?"]
			if {$answer eq "yes"} {
				Config::Assign::Write
			}
			set Priv(didAssign) 0
		}
	}

	NSWindowManager::Undisplay assign

	return
}

# NSAssign::StateRemember --
#
#	Remember the group/member selections.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::StateRemember {oop} {

	if {!$NSWindowManager::Priv(assign,setup)} return

	set hook [Info $oop hook]

	# Because we are clearing the lists here, and don't want to
	# upset the user, I save the selected group/member so it can
	# be restored in StateRestore() below.
	Info $oop group,$hook [Info $oop group,current]
	Info $oop member,$hook [Info $oop member,current]

	return
}

# NSAssign::StateRestore --
#
#	Restore the group/member selections.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::StateRestore {oop} {

	set hook [Info $oop hook]

	# Restore the selected group
	set current [Info $oop group,$hook]
	set tree [Info $oop group,tree]
	if {$current != -1} {
		NSList::Activate $tree "root child $current"
	}

	# Restore the selected member
	set current [Info $oop member,$hook]
	set tree [Info $oop member,tree]
	if {$current != -1} {
		NSList::Activate $tree "root child $current"
	}

	return
}

# NSAssign::CallHook --
#
#	Call the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::CallHook {oop message args} {

	return [uplevel #0 NSAssign::hook_[Info $oop hook] $oop $message $args]
}

# NSAssign::SetHook --
#
#	Set the hook. The hook controls what is displayed and what is
#	done when icons/sprites are selected.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::SetHook {oop hook} {

	variable Priv

	if {$hook ne [Info $oop hook]} {
		set again 0
	} else {
		set again 1
	}

	if {!$again} {
		StateRemember $oop
	}

	# Remember the hook
	Info $oop hook $hook

	# Clear the hook-specific menu
	set hookMenu [Info $oop hookMenu,menu]
	$hookMenu delete 0 end
	destroy {*}[winfo children $hookMenu]

	# Set the group list
	SetList_Group $oop

	# If the hook-menu is empty, remove it, otherwise insert it
	set mbarId [Info $oop mbarId]
	set menu [NSMenu::Info $mbarId menu]
	if {[$hookMenu index end] eq "none"} {
		if {[Info $oop hookMenu,inserted]} {
			$menu delete end
			Info $oop hookMenu,inserted 0
		}
	} else {
		if {![Info $oop hookMenu,inserted]} {
			$menu add cascade -menu $hookMenu
			Info $oop hookMenu,inserted 1
		}
		$menu entryconfigure end -label [mc $hook]
	}

	# Radiobutton menu entries
	Info $oop radio,hook $hook

	set tabsId [Info $oop tabsId]
	set current [NSTabs::Info $tabsId current]
	set tabId [NSTabs::GetNthId $tabsId [lsearch -exact $Priv(hook) $hook]]
	if {$tabId != $current} {
		NSTabs::Smaller $tabsId $current
		NSTabs::Bigger $tabsId $tabId
		NSTabs::Info $tabsId current $tabId
	}

	StateRestore $oop

	return
}

# NSAssign::AssignStuff --
#
#	Call the hook to assign stuff, and display the number of members
#	the thing was assigned to.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::AssignStuff {oop what args} {

	set count [CallHook $oop assign $what {*}$args]
	if {$count} {
		StatusBar $oop "Assigned $what to $count things." 1
	}

	return
}

# NSAssign::Click_Member --
#
#	Do something when a selected member is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::Click_Member {oop row} {

	if {[Info $oop sound,config] ne ""} {
		set soundList [CallHook $oop get_sound $row]
		if {[llength $soundList]} {
			set dir1 [lindex $soundList 0]
			set snd1 [lindex $soundList 1]

			# Stop other sounds
			angband sound stop

			angband sound play $dir1 $snd1
		}
	}

	return
}

# NSAssign::Click_Sound --
#
#	Do something when a selected sound is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::Click_Sound {oop row} {

	# Skip the "None" sound
	incr row -1

	set dir [Info $oop sound,dir]

	# Get the sound name
	set sound [lindex [Info $oop sound,match] $row]

	# Stop other sounds
	angband sound stop

	# Play the sound
	angband sound play $dir $sound

	return
}

# NSAssign::Click_SoundAssign --
#
#	Do something when a selected sound assignment is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::Click_SoundAssign {oop row} {

	# Get a list of sounds assigned to the selected member
	set soundList [CallHook $oop get_sound [Info $oop member,current]]

	set dir1 [lindex $soundList [expr {$row * 2 + 0}]]
	set snd1 [lindex $soundList [expr {$row * 2 + 1}]]

	if {$snd1 eq ""} return

	# Stop other sounds
	angband sound stop

	# Play the sound
	angband sound play $dir1 $snd1

	return
}

# NSAssign::SelectionChanged_Group --
#
#	When a "group" list item is selected, display members in the
#	"member" list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::SelectionChanged_Group {oop tree count select deselect} {

	# If nothing was selected, clear the member list
	if {![llength $select]} {
		set tree [Info $oop member,tree]
		NSList::Clear $tree
		Info $oop group,current -1
		return
	}

	# Get the (first) row
	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]
	Info $oop group,current $row

	# Display members in that group
	SetList_Member $oop $row

	return
}

# NSAssign::SelectionChanged_Member --
#
#	Do something when a "member" list item is selected.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::SelectionChanged_Member {oop tree count select deselect} {

	set win [Info $oop win]

	# Require unique selection.
	if {$count != 1} {
		Info $oop member,current -1
		$win.statusBar itemconfigure t2 -text ""
		switch -- [Info $oop display] {
			sound {
				SoundSynchButtons $oop
				SetList_SoundAssign $oop
			}
		}
		return
	}

	# Get the row
	set item [lindex [$tree selection get] 0]
	set row [NSList::Item2Row $tree $item]
	Info $oop member,current $row

	# Get the icon assigned to the selected member
	set icon [CallHook $oop get_icon $row]
	scan $icon %s assignType

	# Display it in the status line
	set member [lindex [CallHook $oop member_list [Info $oop group,current]] $row]
	$win.statusBar itemconfigure t2 -text "#$member: $icon"

	Info $oop display,ignoreSel 1

	set playSound 1

	switch -- [Info $oop display] {

		alternate {
			if {$assignType eq "alternate"} {
				set tree [Info $oop sprite,tree]
				scan $icon "%s %d" assignType index
				NSList::Activate $tree "root child $index"
			}
		}

		icon {
			if {$assignType eq "icon"} {
				set browserId [Info $oop browserId]
				set browserType [NSIconBrowser::Info $browserId iconType]
				scan $icon "%s %s %d" assignType iconType index
				if {$browserType eq $iconType} {
					NSIconBrowser::SeeIcon $browserId $iconType $index
				}
			}
		}

		sound {
			set tree [Info $oop soundAssign,tree]
			SetList_SoundAssign $oop
			if {[$tree numitems] > 1} {
				$tree selection add "first visible"
			}
			set playSound 0
		}

		sprite {
			if {$assignType eq "sprite"} {
				set tree [Info $oop sprite,tree]
				scan $icon "%s %d" assignType index
				NSList::Activate $tree "root child $index"
			}
		}
	} 

	Info $oop display,ignoreSel 0

	# Call the hook to do other stuff (recall, etc)
	CallHook $oop select_member $row

	if {$playSound && ([Info $oop sound,config] ne "")} {
		set soundList [CallHook $oop get_sound $row]
		if {[llength $soundList]} {
			set dir1 [lindex $soundList 0]
			set snd1 [lindex $soundList 1]
			# Stop other sounds
			angband sound stop
			angband sound play $dir1 $snd1
		}
	}

	SoundSynchButtons $oop

	return
}

# NSAssign::SelectionChanged_Icon --
#
#	Do something when an icon is selected in the NSIconBrowser.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::SelectionChanged_Icon {oop tree count select deselect} {

	# Don't assign stuff during feedback
	if {[Info $oop display,ignoreSel]} return

	# Do nothing if no new icon was selected
	if {![llength $select]} return

	# Do nothing if no member is selected
	if {![[Info $oop member,tree] selection count]} return

	# Get the (first) index
	set item [lindex $select 0]
	set index [NSList::Item2Row $tree $item]

	# Get the type of icon
	set browserId [Info $oop browserId]
	set type [NSIconBrowser::Info $browserId iconType]

	# Call hook to assign icons to selected members
	AssignStuff $oop icon $type $index

	# Update the assigned icon in the status bar
	set row [Info $oop member,current]
	set member [lindex [CallHook $oop member_list [Info $oop group,current]] $row]
	[Info $oop win].statusBar itemconfigure t2 -text "#$member: $type $index"

	return
}

# NSAssign::SoundSynchButtons --
#
#	Enable/disable buttons for assigning sounds.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::SoundSynchButtons {oop} {

	set tree [Info $oop sound,tree]
	if {[$tree selection count] &&
		([Info $oop member,current] != -1) &&
		[CallHook $oop wants_sound]} {
		[Info $oop sound,button,add] configure -state normal
	} else {
		[Info $oop sound,button,add] configure -state disabled
	}

	set tree [Info $oop soundAssign,tree]
	if {[$tree selection count]} {
		[Info $oop sound,button,remove] configure -state normal
	} else {
		[Info $oop sound,button,remove] configure -state disabled
	}

	return
}

# NSAssign::SelectionChanged_Sound --
#
#	Called when the sound selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::SelectionChanged_Sound {oop tree count select deselect} {

	SoundSynchButtons $oop

	# Do nothing if no new row was selected
	if {![llength $select] || ($count > 1)} return

	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]
	if {$row} {

		# Strip off the "None" sound
		incr row -1

		# Get the sound name
		set sound [lindex [Info $oop sound,match] $row]

		# Stop other sounds
		angband sound stop

		# Play the sound
		angband sound play [Info $oop sound,dir] $sound
	}

	return
}

# NSAssign::SelectionChanged_SoundAssign --
#
#	Called when the sound-assign selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::SelectionChanged_SoundAssign {oop tree count select deselect} {

	SoundSynchButtons $oop

	if {($count != 1) || ![llength $select]} return

	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]

	if {[Info $oop soundAssign,ignoreSel]} return

	set soundList [CallHook $oop get_sound [Info $oop member,current]]
	set dir1 [lindex $soundList [expr {$row * 2}]]
	set snd1 [lindex $soundList [expr {$row * 2 + 1}]]

	if {[string compare $dir1 [Info $oop sound,dir]]} {
		# Stop other sounds
		angband sound stop
		angband sound play $dir1 $snd1
		return
	}

	if {$snd1 eq ""} {
		set row 0
	} else {
		set row [lsearch -exact [Info $oop sound,match] $snd1]
		if {$row == -1} return
		incr row
	}

	# If the sound is already selected in the Sound List, then
	# play the sound here, since updating the selection below
	# will have no effect.
	set tree [Info $oop sound,tree]
	if {[$tree selection includes "root child $row"]} {
		# Stop other sounds
		angband sound stop
		angband sound play $dir1 $snd1
	} else {
		set ignoreSel [Info $oop display,ignoreSel]
		Info $oop display,ignoreSel 1
		$tree activate "root child $row"
		$tree selection modify active all
		Info $oop display,ignoreSel $ignoreSel
	}

	$tree see "root child $row"

	return
}

# NSAssign::SelectionChanged_Sprite --
#
#	Do something when an icon is selected in the sprite list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::SelectionChanged_Sprite {oop tree count select deselect} {

	# Don't assign stuff during feedback
	if {[Info $oop display,ignoreSel]} return

	# Do nothing if no new row was selected
	if {![llength $select]} return

	# Do nothing if no member is selected
	if {![[Info $oop member,tree] selection count]} return

	# Get the (first) index
	set item [lindex $select 0]
	set index [NSList::Item2Row $tree $item]

	# Call hook to assign sprite to selected members
	set other [Info $oop display]
	AssignStuff $oop $other $index

	return
}

# NSAssign::SetList_Group --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::SetList_Group {oop} {

	set win [Info $oop win]
	set treeG [Info $oop group,tree]

	# Feedback
	StatusBar $oop "Displaying..." 0
	update idletasks

	# Clear the list
	NSList::Clear $treeG

	# Call hook to set the group list
	CallHook $oop set_list_group

	Info $oop group,current -1

	# Config the "member" list
	set treeM [Info $oop member,tree] 
	NSList::Config $treeM -columns 1

	# Hack -- Set the new-item command
	switch -- [Info $oop hook] {
		"feature" {
			$treeM configure -defaultstyle sKF
		}
		"monster" {
			NSList::Config $treeM -columns 2
			$treeM configure -defaultstyle {s0 sKM}
		}
		default {
#			$treeM configure -defaultstyle s0
		}
	}

	# Feedback
	StatusBar $oop "Done." 1

	return
}

# NSAssign::SetList_Member --
#
#	.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::SetList_Member {oop group} {

	set tree [Info $oop member,tree]

	# Clear the list
	NSList::Clear $tree

	Info $oop member,current -1

	# Call hook to set the member list
	CallHook $oop set_list_member $group

	return
}

# NSAssign::SetList_Other --
#
#	Set the other list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::SetList_Other {oop other} {

	set tree [Info $oop sprite,tree]

	# Clear the list
	NSList::Clear $tree

	# Get the number of alternates/sprites */
	set max [$other count]

	# Add each alternate/sprite to the list
	for {set i 0} {$i < $max} {incr i} {

		# Append match to the list
		set item [$tree item create]
		NSList::SetIcon $tree $item "$other $i"
		$tree item lastchild root $item
	}

	return
}

# NSAssign::SetList_Sound --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::SetList_Sound {oop} {

	set win [Info $oop win]
	set tree [Info $oop sound,tree]
	set dir [Info $oop sound,dir]

	# Clear the list
	NSList::Clear $tree

	if {$dir eq ""} return

	Progress $oop open

	# Read the list of sound files if needed
	if {![info exists ::Global(sound,globCount,$dir)] ||
		![info exists ::NSAssign($oop,sound,globCount,$dir)] ||
		![Info $oop sound,globCount,$dir]} {
		GlobSoundDirectory $dir
	}

	set fileList [Global sound,fileName,$dir]
	set max [llength $fileList]

	# Row zero is "None", for "no sound assigned"	
	NewItem_Sound $tree [mc sound-none]

	set i 0
	foreach file $fileList {
		NewItem_Sound $tree $file
		Progress $oop update $i $max
		incr i
	}

	Progress $oop close

	Info $oop sound,match $fileList
	Info $oop sound,globCount,$dir [Global sound,globCount,$dir]

	return
}

# NSAssign::SetList_SoundAssign --
#
#	Display the sounds assigned to the selected member(s).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::SetList_SoundAssign {oop} {

	set tree [Info $oop soundAssign,tree]

	# Clear the list
	NSList::Clear $tree

	# Get the selected member
	set row [Info $oop member,current]
	if {$row == -1} return

	if {[Info $oop sound,config] eq ""} return

	foreach {dir1 snd1} [CallHook $oop get_sound $row] {
		if {![string length $snd1]} {set snd1 [mc sound-none]}
		NewItem_Sound $tree $snd1
	}

	return
}

# NSAssign::SetDisplay --
#
#	Displays the list of what is assigned (icons or sprites).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::SetDisplay {oop display} {

	set win [Info $oop win]
	set browserId [Info $oop browserId]

	# Remove the Sound Menu if present
	set mbarId [Info $oop mbarId]
	NSMenu::MenuDeleteEntry $mbarId M_SOUND

	switch -- $display {
		alternate {
			set frame $win.frameSprite
			SetList_Other $oop alternate
		}
		icon {
			set frame [NSIconBrowser::Info $browserId frame]
		}
		sound {
			set frame $win.frameSound

			# If the list of sounds changed, set the list again
			set dir [Info $oop sound,dir]
			if {[info exists ::Global(sound,globCount,$dir)]} {
				if {[Info $oop sound,globCount,$dir] != [Global sound,globCount,$dir]} {
					SetList_Sound $oop
				}
			}

			NSMenu::MenuInsertEntry $mbarId -after M_ASSIGN_WHAT \
				-type cascade -menu MENU_SOUND -label [mc Sound] -underline 0 \
				-identifier M_SOUND
		}
		sprite {
			set frame $win.frameSprite
			SetList_Other $oop sprite
		}
	}

	grid forget [grid slaves $win -row 5]
	grid $frame \
		-row 5 -column 0 -rowspan 1 -columnspan 2 -sticky news

	Info $oop display $display

	# Radiobutton menu entries
	Info $oop radio,display $display

	set tabsId [Info $oop tabs2Id]
	set current [NSTabs::Info $tabsId current]
	set tabId [NSTabs::GetNthId $tabsId [lsearch -exact {alternate icon sound sprite} $display]]
	if {$tabId != $current} {
		NSTabs::Smaller $tabsId $current
		NSTabs::Bigger $tabsId $tabId
		NSTabs::Info $tabsId current $tabId
	}

	return
}

# NSAssign::UpdateList --
#
#	Configure an element on the row of a list.
#	When the user assigns an icon to a member, we want to update
#	the member list, and sometimes the group list too.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::UpdateList {oop which row elem args} {

	set tree [Info $oop $which,tree]

	set item [$tree index "root child $row"]
	$tree item element configure $item 0 $elem {*}$args

	return
}

# NSAssign::DisplayMember --
#
#	Find the group the given member is in, and display it.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::DisplayMember {oop hook member} {

	foreach group [CallHook $oop group_list] {
		set memberList [CallHook $oop member_list $group]
		set index [lsearch -exact $memberList $member]
		if {$index != -1} {
			Info $oop group,$hook $group
			Info $oop member,$hook $index
			break
		}
	}

	return
}

# NSAssign::Find --
#
#	Simple search routine to look for a member by name.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::Find {oop again} {

	variable Priv

	# Repeat the last search
	if {$again && [string length $Priv(find,string)]} {
		set string $Priv(find,string)

	# Enter a string to find, start from the beginning
	} else {

		# Ask the user for a name
		set string [NSUtils::StringBox -title [mc Find] \
			-initial $Priv(find,string) -prompt [mc find-prompt] \
			-buttons [list [mc Find] [mc Cancel]] -parent [Info $oop win]]

		# User cancelled
		if {![string length $string]} return

		# Clean up after the dialog, give message
		StatusBar $oop "Searching..." 1
		update

		# Reset search parameters
		set Priv(find,string) $string
	}

	# Default to searching from the beginning
	set group 0
	set member 0

	# Search in selected group, if any
	if {!$Priv(find,fromStart)} {
		set groupCurr [Info $oop group,current]
		set memberCurr [Info $oop member,current]
		if {$groupCurr != -1} {
			set group $groupCurr
		}
		if {$memberCurr != -1} {
			set member [expr {$memberCurr + 1}]
		}
	}

	set max [expr {[[Info $oop group,tree] numitems] - 1}]

	# Compare lowercase
	set string [string tolower $string]

	# Check each group
	for {set i $group} {$i < $max} {incr i} {

		# Get a list of member indexes
		set match [lrange [CallHook $oop member_list $i] $member end]

		# Check each member index
		foreach index $match {

			# Get the member name
			set name2 [CallHook $oop member_name $i $index]

			# Compare lowercase
			set name2 [string tolower $name2]

			# Found a match
			if {[string first $string $name2] != -1} {

				set treeG [Info $oop group,tree]

				# The new group is not displayed
				if {![$treeG selection includes "root child $i"]} {

					# Select the new group. As a side effect, the
					# SetList_Member() command is called to display
					# the monsters in the group.
					$treeG activate "root child $i"
					$treeG selection modify active all
					$treeG see active

				# The new group is already selected
				} else {
				}

				set treeM [Info $oop member,tree]

				# Select the matching member, deselecting all others
				$treeM activate "root child $member"
				$treeM selection modify active all
				$treeM see active

				# Don't search from start next time
				set Priv(find,fromStart) 0

				# Clear "Searching..." message
				StatusBar $oop "" 0

				# Done
				return
			}

			# Next row
			incr member
		}

		# First row
		set member 0
	}

	# If we didn't search from the start, then wrap around
	if {!$Priv(find,fromStart)} {
		set Priv(find,fromStart) 1
		Find $oop 1
		return
	}

	StatusBar $oop "No match for \"$string\"." 1

	return
}

# NSAssign::StatusBar --
#
#	Display text in the status bar, perhaps clearing it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label [$win.statusBar itemcget t1 -label]
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

proc NSAssign::NewItem {tree icon text {hasSound 0}} {

	set item [$tree item create]
	NSList::SetIcon $tree $item $icon
	NSList::SetText $tree $item $text
	if {$hasSound} {
		$tree item state set $item hasSound
	}
	$tree item lastchild root $item

	return
}

proc NSAssign::NewItem_Feature {tree icon iconBG text} {

	set item [$tree item create]
	NSList::SetIcon $tree $item $icon
	$tree item element configure $item 0 eAss2 -fg $iconBG
	NSList::SetText $tree $item $text
	$tree item lastchild root $item

	return
}

proc NSAssign::NewItem_Sound {tree text} {

	set item [$tree item create]
	NSList::SetText $tree $item $text
	$tree item lastchild root $item

	return
}

# NSAssign::SynchronizeSoundAssignments --
#
#	When more than one member is selected, and a new sound assignment
#	is inserted or deleted, this command makes sure that the sounds
#	assigned to group+keyword2 match those in group+keyword1.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::SynchronizeSoundAssignments {oop group keyword1 keyword2} {

	set config [Info $oop sound,config]
	set soundList1 [angband sound assign $config $group $keyword1]
	set soundList2 [angband sound assign $config $group $keyword2]

	set match 1
	set sameLength [expr {[llength $soundList1] == [llength $soundList2]}]

	# The lists are the same length
	if {$sameLength} {

		# Compare the lists of sounds
		foreach {dir1 snd1} $soundList1 {dir2 snd2} $soundList2 {
			if {$dir1 ne $dir2 ||
				$snd1 ne $snd2} {
				set match 0
				break
			}
		}

	# The lists have differing lengths
	} else {
		set match 0
	}

	# Both group+keywords have the same list of sounds
	if {$match} return

	# The lists are the same length, so just overwrite each assignment
	# in keyword2 with those in keyword1.
	if {$sameLength} {
		set soundIndex 0
		foreach {dir1 snd1} $soundList1 {
			angband sound assign $config $group $keyword2 $soundIndex $dir1 $snd1
			incr soundIndex
		}

	# The lists have differing lengths
	} else {

		# Delete all assignments from keyword2
		while {[angband sound config count $config $group $keyword2]} {
			angband sound delete $config $group $keyword2 0
		}

		# Copy keyword1 assignments to keyword2
		foreach {dir1 snd1} $soundList1 {
			angband sound insert $group $keyword2 1000 $dir1 $snd1
		}
	}

	return
}

# NSAssign::InsertSound --
#
#	Add each selected sound to the list of sounds assigned to each selected
#	member.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::InsertSound {oop} {

	if {[Info $oop sound,config] eq ""} return
	if {[Info $oop sound,dir] eq ""} return
	if {![CallHook $oop wants_sound]} return

	set treeA [Info $oop soundAssign,tree]

	# Do nothing if no member is selected
	set member [Info $oop member,current]
	if {$member == -1} return
	set treeM [Info $oop member,tree]

	# Do nothing if no sound is selected
	set treeS [Info $oop sound,tree]
	if {![$treeS selection count]} return

	foreach item [$treeS selection get] {
		lappend rows [NSList::Item2Row $treeS $item]
	}
	foreach row [lsort -integer $rows] {
		if {$row == 0} {
			set sound ""
		} else {
			incr row -1
			set sound [lindex [Info $oop sound,match] $row]
		}

		if {![CallHook $oop insert_sound 1000 $sound]} return

		if {[llength [CallHook $oop get_sound $member]] == 2} {
			$treeM item state set "root child $member" hasSound
		}
	}

	# Redisplay the sound-assign list
	SetList_SoundAssign $oop

	# Select the newly inserted sound
	Info $oop soundAssign,ignoreSel 1
	$treeA selection add last
	Info $oop soundAssign,ignoreSel 0
	$treeA see last

	return
}

# NSAssign::DeleteSound --
#
#	Delete the selected sound from the list of sounds assigned to
#	the selected member(s).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::DeleteSound {oop} {

	if {![CallHook $oop wants_sound]} return

	# Do nothing if no sound-assignment is selected
	set treeA [Info $oop soundAssign,tree]
	if {![$treeA selection count]} return

	set member [Info $oop member,current]
	set treeM [Info $oop member,tree]

	# Delete each selected assignment
	foreach item [$treeA selection get] {
		lappend rows [NSList::Item2Row $treeA $item]
	}
	foreach index [lsort -integer -decreasing $rows] {
		# Delete the sound from each selected member
		if {![CallHook $oop delete_sound $index]} return
	}

	if {![llength [CallHook $oop get_sound $member]]} {
		$treeM item state set "root child $member" !hasSound
	}

	# Redisplay the list of assigned sounds
	SetList_SoundAssign $oop

	set count [expr {[$treeA numitems] - 1}]
	if {$count} {
		if {$index >= $count} {
			set index [expr {$count - 1}]
		}
		Info $oop soundAssign,ignoreSel 1
		$treeA selection add "root child $index"
		Info $oop soundAssign,ignoreSel 0
		$treeA see "root child $index"
	}

	return
}

# NSAssign::InvokeTab --
#
#	Called when a tab is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::InvokeTab {oop tabsId tabId} {

	variable Priv

	set index [lsearch -exact [NSTabs::Info $tabsId id] $tabId]
	SetHook $oop [lindex $Priv(hook) $index]

	return
}

# NSAssign::InvokeTab2 --
#
#	Called when a tab is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::InvokeTab2 {oop tabsId tabId} {

	set index [lsearch -exact [NSTabs::Info $tabsId id] $tabId]
	SetDisplay $oop [lindex {alternate icon sound sprite} $index]

	return
}

# NSAssign::Progress --
#
#	Show, update, and hide the progress bar.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::Progress {oop action args} {

	set win [Info $oop win]
	set progId [Info $oop progId]

	switch -- $action {
		open {
			NSProgress2::Zero $progId
			pack [NSProgress2::Info $progId frame] -pady 1 -fill x
			update idletasks
			Info $oop oldLabel2 [$win.statusBar itemcget t2 -text]
		}
		update {
			set cur [lindex $args 0]
			set max [lindex $args 1]
			set bump [expr {(($max / 20) > 40) ? ($max / 20) : 40}]
			if {$cur && ($cur % $bump) == 0} {
				NSProgress2::SetDoneRatio $progId [expr {$cur / double($max)}]
				$win.statusBar itemconfigure t2 -text "$cur/$max"
				update idletasks
			}
		}
		close {
			NSProgress2::SetDoneRatio $progId 1.0
			update idletasks
			pack forget [NSProgress2::Info $progId frame]
			$win.statusBar itemconfigure t2 -text [Info $oop oldLabel2]
		}
	}

	return
}

# NSAssign::BrowserCmd --
#
#	Called by NSIconBrowser when displaying an icon type. Display
#	the progress of listing the icons. Note that this can
#	actually slow down listing the icons.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::BrowserCmd {oop action args} {

	set win [Info $oop win]
	set progId [Info $oop progId]

	switch -- $action {
		open {
			StatusBar $oop {} 0
			NSProgress2::Zero $progId
			pack [NSProgress2::Info $progId frame] -pady 1 -fill x
			update idletasks
			Info $oop oldLabel2 [$win.statusBar itemcget t2 -text]
		}
		update {
			set cur [lindex $args 0]
			set max [lindex $args 1]
			set bump [expr {(($max / 20) > 40) ? ($max / 20) : 40}]
set bump [expr {$max / 20}]
			if {$cur && ($cur % $bump) == 0} {
				NSProgress2::SetDoneRatio $progId [expr {$cur / double($max)}]
				$win.statusBar itemconfigure t2 -text "$cur/$max"
				update idletasks
			}
		}
		close {
			NSProgress2::SetDoneRatio $progId 1.0
			update idletasks
			pack forget [NSProgress2::Info $progId frame]
			$win.statusBar itemconfigure t2 -text [Info $oop oldLabel2]
		}
	}

	return
}

proc NSAssign::hook_character {oop message args} {

	switch -- $message {

		set_list_group {

			set tree [Info $oop group,tree]
			NewItem $tree [angband player icon] [angband player name]

			set ident {}
			set hookMenu [Info $oop hookMenu,menu]
			$hookMenu add command -label [mc "Auto Assign..."] \
				-command "NSAssign::CallHook $oop menu_cmd auto_assign"
			lappend ident auto_assign
			Info $oop hookMenu,ident,hook $ident
		}

		set_list_member {
			set tree [Info $oop member,tree]
			if {[variant ANGBANDTK KANGBANDTK ZANGBANDTK]} {
				set icon [angband player icon]
				NewItem $tree $icon [angband player name]
				Info $oop member,match [list 0]
			}
			if {[variant OANGBANDTK]} {
				# OAngbandTk allows an icon for each shape
				set index 0
				set match {}
				foreach name [angband info shape_name] {
					set icon [assign set character $index]
					NewItem $tree $icon $name
					lappend match $index
					incr index
				}
				Info $oop member,match $match
			# OANGBANDTK
			}
		}

		assign {
			set count 0
			set what [lindex $args 0]
			switch -- $what {
				alternate {
					tk_messageBox -title "Assign" -icon info \
						-message "You can only assign alternates to\
						features and objects" -parent [Info $oop win]
				}
				icon -
				sprite {
					# Build the assignment, such as "sprite 2"
					set assign $what
					foreach arg [lrange $args 1 end] {
						append assign " $arg"
					}
					set treeM [Info $oop member,tree]
					set count 0
					foreach item [$treeM selection get] {
						set row [NSList::Item2Row $treeM $item]
						if {!$row && [Global autoAssign]} {
							set msg "The icon for the character was automatically chosen based\n"
							append msg "on race and class.\n\n"
							append msg "Do you really want to override the automatic icon assignment?"
							set answer [tk_messageBox -title "Character Icon" \
								-message $msg \
								-type yesno -parent [Info $oop win]]
							if {$answer eq "no"} continue
							Global autoAssign 0
						}
						assign set character $row $assign
						if {!$row} {
							set treeG [Info $oop group,tree]
							NSList::SetIcon $treeG "root child 0" $assign
						}
						NSList::SetIcon $treeM "root child $row" $assign
						incr count
					}
				}
			}
			return $count
		}

		get_icon {
			set row [lindex $args 0]
			return [assign set character $row]
		}

		member_has_sound {
			return 0
		}

		get_sound {
			return {}
		}

		insert_sound {
			return 0
		}

		delete_sound {
			return 0
		}

		wants_sound {
			return 0
		}

		select_member {
		}

		group_names {
			return [angband player name]
		}

		member_name {
			set group [lindex $args 0]
			set index [lindex $args 1]
			if {[variant ANGBANDTK KANGBANDTK ZANGBANDTK]} {
				return [angband player name]
			}			
			if {[variant OANGBANDTK]} {
				return [lindex [angband info shape_name] $index]
			}
		}

		member_list {
			if {[variant ANGBANDTK KANGBANDTK ZANGBANDTK]} {
				return [list 0]
			}
			if {[variant OANGBANDTK]} {
				set index -1
				set result {}
				foreach name [angband info shape_name] {
					lappend match [incr index]
				}
				return $result
			}
		}

		menu_setup {
			set hookMenu [Info $oop hookMenu,menu]
			if {![Global autoAssign] && \
				[llength [info commands ::IconCfg::AutoAssignCharacterIcon]]} {
				menuentrystate $hookMenu 0 normal
			}
		}

		menu_cmd {
			switch -- [lindex $args 0] {
				auto_assign {
					set msg "Do you want $::Angband(name) to automatically choose\n"
					append msg "a character icon based on race and class from now on?"
					set answer [tk_messageBox -title "Character Icon" \
						-message $msg -type yesno -icon question -parent [Info $oop win]]
					if {$answer eq "no"} return
					IconCfg::AutoAssignCharacterIcon
					Config::CharIcon::AutoAssign
					set treeM [Info $oop member,tree]
					if {[$treeM numitems] > 1} {
						NSList::SetIcon $treeM "root child 0" [angband player icon]
					}
					set treeG [Info $oop group,tree]
					NSList::SetIcon $treeG "root child 0" [angband player icon]
				}
			}
		}

		menu_string {
			switch -- [lindex $args 1] {
				auto_assign {
					return "Assigns a character icon based on race and class."
				}
				default {
					return ""
				}
			}
		}
	}

	return
}

proc NSAssign::hook_effect {oop message args} {

	switch -- $message {

		set_list_group {

			set tree [Info $oop group,tree]

			set assign [effect assign ball fire]
			NewItem $tree $assign [mc Ball]

			set assign [effect assign bolt fire]
			NewItem $tree $assign [mc Bolt]

			set assign [effect assign ammo arrow]
			NewItem $tree $assign [mc Ammunition]
		}

		set_list_member {
			set group [lindex $args 0]
			set effectType [lindex [effect groups] $group]

			set tree [Info $oop member,tree]
			foreach effect [effect names $effectType] {
				set assign [effect assign $effectType $effect]
				NewItem $tree $assign $effect
				lappend match $effect
			}

			# Keep a list of matching effect names
			Info $oop member,match $match
		}

		assign {
			set count 0
			set what [lindex $args 0]
			set group [Info $oop group,current]
			set effectType [lindex [effect groups] $group]
			switch -- $what {
				alternate {
					tk_messageBox -title "Assign" -icon info \
						-message "You can only assign alternates to\
						objects" -parent [Info $oop win]
				}
				icon {
					set iconType [lindex $args 1]
					set iconIndex [lindex $args 2]
					set assign "icon $iconType $iconIndex"
					set treeM [Info $oop member,tree]
					foreach item [$treeM selection get] {
						set row [NSList::Item2Row $treeM $item]
						set effectName [lindex [Info $oop member,match] $row]
						effect assign $effectType $effectName \
							-type $iconType -index $iconIndex
						NSList::SetIcon $treeM "root child $row" $assign
						incr count
					}
					set treeG [Info $oop group,tree]
					if {$effectType ne "ammo" && $effectName eq "fire"} {
						set row [Info $oop group,current]
						NSList::SetIcon $treeG "root child $row" $assign
					}
					if {$effectType eq "ammo" && $effectName eq "arrow"} {
						set row [Info $oop group,current]
						NSList::SetIcon $treeG "root child $row" $assign
					}
				}
			}
			return $count
		}

		get_icon {
			set row [lindex $args 0]
			set effectName [lindex [Info $oop member,match] $row]
			set group [Info $oop group,current]
			set effectType [lindex [effect groups] $group]
			return [effect assign $effectType $effectName]
		}

		member_has_sound {
			return 0
		}

		get_sound {
			return {}
		}

		insert_sound {
			return 0
		}

		delete_sound {
			return 0
		}

		wants_sound {
			return 0
		}

		select_member {
		}

		group_names {
			return [list Ball Bolt Ammo]
		}

		member_name {
			set group [lindex $args 0]
			set index [lindex $args 1]
			set effectType [lindex [effect groups] $group]
			return [lindex [effect names $effectType] $index]
		}

		member_list {
			set group [lindex $args 0]
			set effectType [lindex [effect groups] $group]
			set index -1
			foreach effect [effect names $effectType] {
				lappend match [incr index]
			}
			return $match
		}
	}

	return
}

proc NSAssign::hook_feature {oop message args} {

	global NSAssign

	if {[variant ANGBANDTK]} {
		set feat_name [list \
			"nothing" "open floor" "invisible trap" "glyph of warding" \
			"open door" "broken door" "up staircase" "down staircase" \
			"General Store" "Armoury" "Weapon Smiths" "Temple" "Alchemy Shop" \
			"Magic Shop" "Black Market" "Home" "trap door" "open pit" \
			"spiked pit" "poisoned pit" "strange rune (summon)" \
			"strange rune (teleport)" "discolored spot (fire)" \
			"discolored spot (acid)" "dart trap (slow)" "dart trap (strength)" \
			"dart trap (dexterity)" "dart trap (constitution)" \
			"gas trap (blind)" "gas trap (confuse)" "gas trap (poison)" \
			"gas trap (sleep)" "locked door" "locked door" "locked door" \
			"locked door" "locked door" "locked door" "locked door" \
			"locked door" "jammed door" "jammed door" "jammed door" \
			"jammed door" "jammed door" "jammed door" "jammed door" \
			"jammed door" "secret door" "pile of rubble" "magma vein" \
			"quartz vein" "magma vein + hidden treasure" "quartz vein + hidden treasure" \
			"magma vein + treasure" "quartz vein + treasure" \
			"granite wall (basic)" "granite wall (inner)" \
			"granite wall (outer)" "granite wall (solid)" \
			"permanent wall (basic)" "permanent wall (inner)" \
			"permanent wall (outer)" "permanent wall (solid)" \
		]
	# ANGBANDTK
	}
	if {[variant KANGBANDTK]} {
		set feat_name [list \
			"nothing" "open floor" "invisible trap" "glyph of warding" \
			"open door" "broken door" "up staircase" "down staircase" \
			"General Store" "Armoury" "Weapon Smiths" "Temple" "Alchemy Shop" \
			"Magic Shop" "Black Market" "Home" "Bookstore" "trap door" "open pit" \
			"spiked pit" "poisoned pit" "strange rune (summon)" \
			"strange rune (teleport)" "discolored spot (fire)" \
			"discolored spot (acid)" "dart trap (slow)" "dart trap (strength)" \
			"dart trap (dexterity)" "dart trap (constitution)" \
			"gas trap (blind)" "gas trap (confuse)" "gas trap (poison)" \
			"gas trap (sleep)" "closed door" "locked door" "locked door" \
			"locked door" "locked door" "locked door" "locked door" \
			"locked door" "jammed door" "jammed door" "jammed door" \
			"jammed door" "jammed door" "jammed door" "jammed door" \
			"secret door" "pile of rubble" "magma vein" \
			"quartz vein" "magma vein + hidden treasure" "quartz vein + hidden treasure" \
			"magma vein + treasure" "quartz vein + treasure" \
			"granite wall (basic)" "granite wall (inner)" \
			"granite wall (outer)" "granite wall (solid)" \
			"permanent wall (basic)" "permanent wall (inner)" \
			"permanent wall (outer)" "permanent wall (solid)" \
			"Building" "Building" "Building" "Building" "Building" \
			"Building" "Building" "Building" "Building" "Building" \
			"Building" "Building" "Building" "Building" "Building" \
			"Building" "Building" "Building" \
			"" "" "" "" "" "" "" "" "" "" "" "" "" "" \
			"tree" "dense fog" "" "" "mountain" "" "" "patch of grass" \
			"pool of deep water" "pool of shallow water" "pool of deep lava" \
			"stream of shallow lava" "chasm" "patch of dirt" "" "" \
			"quest entrance" "quest exit" "quest down level" "quest up level" \
		]
	# KANGBANDTK
	}
	if {[variant OANGBANDTK]} {
		set feat_name [list \
			"nothing" "open floor" "invisible trap" "glyph of warding" \
			"open door" "broken door" "up staircase" "down staircase" \
			"lava" "water" "tree" "" "" "" "" "" \
			"trap door" "pit" "dart trap" "discolored spot (acid)" \
			"gas trap" "strange rune (summon)" "strange rune (dungeon)" \
			"hex" "shimmering portal" "murder hole" \
			"" "" "" "" "" "monster trap" \
			"closed door" "locked door" "locked door" \
			"locked door" "locked door" "locked door" "locked door" \
			"locked door" "jammed door" "jammed door" "jammed door" \
			"jammed door" "jammed door" "jammed door" "jammed door" \
			"jammed door" "secret door" "pile of rubble" "magma vein" \
			"quartz vein" "magma vein + hidden treasure" "quartz vein + hidden treasure" \
			"magma vein with treasure" "quartz vein with treasure" \
			"granite wall (basic)" "granite wall (inner)" \
			"granite wall (outer)" "granite wall (solid)" \
			"permanent wall (basic)" "permanent wall (inner)" \
			"permanent wall (outer)" "permanent wall (solid)" \
			"General Store" "Armoury" "Weapon Smiths" "Temple" "Alchemy Shop" \
			"Magic Shop" "Black Market" "Home" "Bookstore" \
		]
	# OANGBANDTK
	}
	if {[variant ZANGBANDTK]} {
		set feat_name [list \
			"nothing" "open floor" "invisible trap" "glyph of warding" \
			"open door" "broken door" "up staircase" "down staircase" \
			"quest entrance" "quest exit" "quest down level" "quest up level" \
			"town exit" "" "" "" "trap door" "open pit" \
			"spiked pit" "poisoned pit" "strange rune (summon)" \
			"strange rune (teleport)" "discolored spot (fire)" \
			"discolored spot (acid)" "dart trap (slow)" "dart trap (strength)" \
			"dart trap (dexterity)" "dart trap (constitution)" \
			"gas trap (blind)" "gas trap (confuse)" "gas trap (poison)" \
			"gas trap (sleep)" "locked door" "locked door" "locked door" \
			"locked door" "locked door" "locked door" "locked door" \
			"locked door" "jammed door" "jammed door" "jammed door" \
			"jammed door" "jammed door" "jammed door" "jammed door" \
			"jammed door" "secret door" "pile of rubble" "magma vein" \
			"quartz vein" "magma vein + hidden treasure" "quartz vein + hidden treasure" \
			"magma vein + treasure" "quartz vein + treasure" \
			"granite wall (basic)" "granite wall (inner)" \
			"granite wall (outer)" "granite wall (solid)" \
			"permanent wall (basic)" "permanent wall (inner)" \
			"permanent wall (outer)" "permanent wall (solid)" \
			"explosive rune" "Pattern startpoint" \
			"section of the Pattern" "section of the Pattern" \
			"section of the Pattern" "section of the Pattern" \
			"section of the Pattern" "section of the Pattern (discharged)" \
			"Pattern exit" "corrupted section of the Pattern" \
			"General Store" "Armoury" "Weapon Smiths" "Temple" "Alchemy Shop" \
			"Magic Shop" "Black Market" "Home" "Bookstore" \
			"a pool of deep water" "stream of shallow water" \
			"a pool of deep lava" "a stream of shallow lava" \
			"dark pit" "dirt" "patch of grass" \
			"compact rune" "invisible wall" "" "" "" "" \
			"tree" "mountain chain" \
			"" "" "" "" "" "" "" "" "" "" "" "" "" "" \
			"" "" "" "" "" "" "" "" "" "" "" "" "" "" "" "" \
			Building Building Building Building Building \
			Building Building Building Building Building \
			Building Building Building Building Building \
			Building Building Building Building Building \
			Building Building Building Building Building \
			Building Building Building Building Building \
			Building Building \
		]
	# ZANGBANDTK
	}

	set showAll 0

	switch -- $message {

		set_list_group {

			set tree [Info $oop group,tree]

			# Use down staircase as the group icon
			set icon [assign set feature [const FEAT_MORE]]
			NewItem $tree $icon [mc feature]

			set hookMenu [Info $oop hookMenu,menu]

			# Build a list of entry identifiers
			set ident {}
if 0 {
			# Lighting
			set menu $hookMenu.light
			menu $menu -tearoff 0
			bind $menu <<MenuSelect>> \
				"NSAssign::MenuSelect_Hook $oop light %W"
			$hookMenu add cascade -menu $menu -label [mc Lighting]
			lappend ident m_light
			$menu add radiobutton -label [mc Icon] \
				-variable NSAssign($oop,hookMenu,light) -value icon \
				-command "NSAssign::CallHook $oop menu_cmd light"
			$menu add radiobutton -label [mc None] \
				-variable NSAssign($oop,hookMenu,light) -value none \
				-command "NSAssign::CallHook $oop menu_cmd light"
			$menu add radiobutton -label [mc Tint] \
				-variable NSAssign($oop,hookMenu,light) -value tint \
				-command "NSAssign::CallHook $oop menu_cmd light"
			Info $oop hookMenu,ident,light [list icon none tint]
}
			# Background
			$hookMenu add command -label [mc Background...] \
				-command "NSAssign::CallHook $oop menu_cmd background"
			lappend ident background

			# Optional memorize
			$hookMenu add checkbutton -label [mc Memorize] \
				-variable NSAssign($oop,hookMenu,boring) \
				-onvalue 0 -offvalue 1 \
				-command "NSAssign::CallHook $oop menu_cmd boring"
			lappend ident boring

			# Force lighting in town at night
			$hookMenu add checkbutton -label [mc "Town Light"] \
				-variable NSAssign($oop,hookMenu,town) \
				-onvalue 1 -offvalue 0 \
				-command "NSAssign::CallHook $oop menu_cmd town"
			lappend ident town

			Info $oop hookMenu,ident,hook $ident
		}

		set_list_member {
			set tree [Info $oop member,tree]
			set f_idx 0

			# Check each feature name
			foreach name $feat_name {

				# This is a real feature
				if {$showAll || ([string length $name] &&
					($f_idx == [struct set feature_type $f_idx mimic]))} {

					# Get the icon assigned to that feature
					set icon [assign set feature $f_idx]

					# ... and the background icon
					set bg_idx [feature configure $f_idx -background]
					set iconBG [assign set feature $bg_idx]

					# Make sure binary and feat_name agree
					if {$::DEBUG} {
						set name2 [angband f_info info $f_idx name]
						append name " \[$name2\]"
if 0 {
						append name " <[feature configure $f_idx -light]>"
}
					}

					# Collect info for each row
					NewItem_Feature $tree $icon $iconBG $name

					# Keep a list of matchin indexes
					lappend match $f_idx

				} else {
					set name2 [angband f_info info $f_idx name]
					if {[string length $name2]} {
						dbwin "Warning: Unhandled feature $name2\n"
					}
				}

				# Next feature
				incr f_idx
			}

			# Keep a list of matching indexes
			Info $oop member,match $match
		}

		assign {

			# Count number of assignments actually made
			set count 0

			# What is being assigned?
			set what [lindex $args 0]

			switch -- $what {
				alternate -
				icon -
				sprite {
					# Build the assignment, such as "sprite 2"
					set assign [join $args]
					set treeM [Info $oop member,tree]
					set match [Info $oop member,match]

					# Check each selected member
					foreach item [$treeM selection get] {

						set row [NSList::Item2Row $treeM $item]

						# Get the feature index
						set f_idx [lindex $match $row]

						# Assign to the feature
						assign set feature $f_idx $assign

						# Update the member list
						NSList::SetIcon $treeM "root child $row" $assign

						# Count assignments
						incr count

						# Hack -- Down staircase is used as group icon
						if {$f_idx == [const FEAT_MORE]} {
							set row 0
							set treeG [Info $oop group,tree]
							NSList::SetIcon $treeG "root child $row" $assign
						}

						# Hack -- Update list for features using this
						# feature as the background
						set row2 0
						foreach f2_idx $match {
							set bg_idx [feature configure $f2_idx -background]
							if {$bg_idx == $f_idx} {
								UpdateList $oop member $row2 eAss2 -fg $assign
							}
							incr row2
						}
					}
				}
			}

			# Return the number of assignments made
			return $count
		}

		get_icon {
			set row [lindex $args 0]
			set f_idx [lindex [Info $oop member,match] $row]
			return [assign set feature $f_idx]
		}

		member_has_sound {
			return 0
		}

		get_sound {
			return {}
		}

		insert_sound {
			return 0
		}

		delete_sound {
			return 0
		}

		wants_sound {
			return 0
		}

		select_member {
			set row [lindex $args 0]
			set f_idx [lindex [Info $oop member,match] $row]
if 0 {
			Info $oop hookMenu,light [feature configure $f_idx -light]
}
			Info $oop hookMenu,boring [feature configure $f_idx -boring]
			Info $oop hookMenu,town [feature configure $f_idx -town]
		}

		group_names {
			return Feature
		}

		member_name {
			set group [lindex $args 0]
			set index [lindex $args 1]
			return [lindex $feat_name $index]
		}

		member_list {
			set f_idx 0
			foreach name $feat_name {
				if {$showAll || ([string length $name] &&
					($f_idx == [struct set feature_type $f_idx mimic]))} {
					lappend match $f_idx
				}
				incr f_idx
			}
			return $match
		}

		menu_setup {
			set hookMenu [Info $oop hookMenu,menu]
			if {[Info $oop member,current] != -1} {
				menuentrystate $hookMenu 0 normal
				menuentrystate $hookMenu 1 normal
				menuentrystate $hookMenu 2 normal
				menuentrystate $hookMenu 3 normal
			}
		}

		menu_cmd {
			set treeM [Info $oop member,tree]
			set entry [lindex $args 0]
			switch -- $entry {
				background {
					set row [Info $oop member,current]
					set f_idx [lindex [Info $oop member,match] $row]
					set initial [feature configure $f_idx -background]
					set bg [NSUtils::StringBox -title "Set Feature Background" \
						-initial $initial -prompt "Feature Index" \
						-buttons [list [mc OK] [mc Cancel]] \
						-parent [Info $oop win]]
					if {![string length $bg]} return
					foreach item [$treeM selection get] {
						set row [NSList::Item2Row $treeM $item]
						set f_idx [lindex [Info $oop member,match] $row]
						feature configure $f_idx -background $bg
						set assign [assign set feature $bg]
						UpdateList $oop member $row eAss2 -fg $assign
					}
					StatusBar $oop "Set feature background to \"$bg\"." 1
				}
				light {
if 0 {
					set light [Info $oop hookMenu,light]
					foreach item [$treeM selection get] {
						set row [NSList::Item2Row $treeM $item]
						set f_idx [lindex [Info $oop member,match] $row]
						feature configure $f_idx -light $light
					}
					StatusBar $oop "Set feature lighting to \"$light\"." 1
}
				}
				boring -
				town {
					set value [Info $oop hookMenu,$entry]
					foreach item [$treeM selection get] {
						set row [NSList::Item2Row $treeM $item]
						set f_idx [lindex [Info $oop member,match] $row]
						feature configure $f_idx -$entry $value
					}
					StatusBar $oop "Set \"-$entry\" to \"$value\"." 1
				}
			}
		}

		menu_string {
			switch -- [lindex $args 1] {
				m_light {
if 0 {
					return "Contains options which control lighting."
}
				}
				background {
					return "Sets the feature index to use as the background for masked icons."
				}
				boring {
					return "The feature is memorized."
				}
				town {
					return "The feature is always brightest on the town level."
				}
				icon {
if 0 {
					return "The feature uses a sequence of icons for lighting."
}
				}
				none {
if 0 {
					return "The feature is always brightest."
}
				}
				tint {
if 0 {
					return "The feature uses a tint table for lighting."
}
				}
				default {
					return ""
				}
			}
		}
	}

	return
}

proc NSAssign::hook_monster {oop message args} {

	switch -- $message {

		set_list_group {

			set tree [Info $oop group,tree]

			foreach {title findSpec} [Global groups,r_info] {

				# Find the last monster in the group
				set match [angband r_info find -unique no -limit 1 \
					-backwards {*}$findSpec]

				if {![llength $match]} {
					tk_messageBox -message "Monster group \"$title\" is empty!"
					continue
				}

				# Get the icon
				set icon [angband r_info info [lindex $match 0] icon]

				# Add this group to the list
				NewItem $tree $icon [mc $title]
			}
		}

		set_list_member {

			set config [Info $oop sound,config]
			set tree [Info $oop member,tree]

			set group [lindex $args 0]
			set findSpec [lindex [Global groups,r_info] [expr {$group * 2 + 1}]]

			# Get a list of monsters in the group
			set match [angband r_info find -unique no {*}$findSpec]

			# Add each match to the list
			foreach index $match {

				# Get the icon and name
				set icon [angband r_info info $index icon]
				set name [angband r_info info $index name]

				# Are sounds assigned?
				if {$config eq ""} {
					set hasSound 0
				} else {
					set hasSound [angband sound config count $config monster mon$index]
				}

				# Collect info for each row
				NewItem $tree $icon $name $hasSound
			}

			# Keep a list of matching indexes
			Info $oop member,match $match
		}

		assign {
			set count 0
			set treeM [Info $oop member,tree]
			set what [lindex $args 0]
			switch -- $what {
				alternate {
					tk_messageBox -title "Assign" -icon info \
						-message "You can only assign alternates to\
						features and objects" -parent [Info $oop win]
				}
				icon -
				sprite {
					# Build the assignment, such as "sprite 2"
					set assign [join $args]
					set match [Info $oop member,match]
					foreach item [$treeM selection get] {
						set row [NSList::Item2Row $treeM $item]
						set index [lindex $match $row]
						assign set monster $index $assign
						NSList::SetIcon $treeM "root child $row" $assign
						incr count
					}
					if {$item == [$treeM index last]} {
						set row [Info $oop group,current]
						set treeG [Info $oop group,tree]
						NSList::SetIcon $treeG "root child $row" $assign
					}
				}
				sound {
return
					set config [Info $oop sound,config]
					set match [Info $oop member,match]
					set dir [Info $oop sound,dir]
					set snd [lindex $args 1]
					set soundIndex [Info $oop soundAssign,current]
					set mcRow [Info $oop member,current]
					set mcIndex [lindex $match $mcRow]
					eval angband sound assign $config monster mon$mcIndex $soundIndex $dir $snd
					foreach item [$tree selection get] {
						set row [NSList::Item2Row $tree $item]
						if {$row == $mcRow} continue
						set index [lindex $match $row]
						SynchronizeSoundAssignments $oop monster mon$mcIndex mon$index
					}
				}
			}
			return $count
		}

		get_icon {
			set row [lindex $args 0]
			set index [lindex [Info $oop member,match] $row]			
			return [angband r_info info $index icon]
		}

		member_has_sound {
			set row [lindex $args 0]
			set index [lindex [Info $oop member,match] $row]
			set config [Info $oop sound,config]
			return [angband sound config count $config monster mon$index]
		}

		get_sound {
			set row [lindex $args 0]
			set index [lindex [Info $oop member,match] $row]
			set config [Info $oop sound,config]
			return [angband sound assign $config monster mon$index]
		}

		insert_sound {
			set config [Info $oop sound,config]
			set dir [Info $oop sound,dir]
			set tree [Info $oop member,tree]
			set soundIndex [lindex $args 0]
			set sound [lindex $args 1]
			set match [Info $oop member,match]
			set mcRow [Info $oop member,current]
			set mcIndex [lindex $match $mcRow]
			angband sound insert $config monster mon$mcIndex $soundIndex $dir $sound
			foreach item [$tree selection get] {
				set row [NSList::Item2Row $tree $item]
				if {$row == $mcRow} continue
				set index [lindex $match $row]
				SynchronizeSoundAssignments $oop monster mon$mcIndex mon$index
			}
			return 1
		}

		delete_sound {
			set config [Info $oop sound,config]
			set tree [Info $oop member,tree]
			set soundIndex [lindex $args 0]
			set match [Info $oop member,match]
			set mcRow [Info $oop member,current]
			set mcIndex [lindex $match $mcRow]
			angband sound delete $config monster mon$mcIndex $soundIndex
			foreach item [$tree selection get] {
				set row [NSList::Item2Row $tree $item]
				if {$row == $mcRow} continue
				set index [lindex $match $row]
				SynchronizeSoundAssignments $oop monster mon$mcIndex mon$index
			}
			return 1
		}

		wants_sound {
			return 1
		}

		select_member {
			set row [lindex $args 0]
			set index [lindex [Info $oop member,match] $row]
			NSRecall::RecallMonster $index
		}

		group_names {
			set names {}
			foreach {title findSpec} [Global groups,r_info] {
				lappend names [mc $title]
			}
			return $names
		}

		group_list {
			set index -1
			set result {}
			foreach {title findSpec} [Global groups,r_info] {
				lappend result [incr index]
			}
			return $result
		}

		member_name {
			set group [lindex $args 0]
			set index [lindex $args 1]
			return [angband r_info info $index name]
		}

		member_list {
			set group [lindex $args 0]
			set findSpec [lindex [Global groups,r_info] [expr {$group * 2 + 1}]]
			return [angband r_info find -unique no {*}$findSpec]
		}
	}	

	return
}

proc NSAssign::hook_object {oop message args} {

	switch -- $message {

		set_list_group {

			set treeG [Info $oop group,tree]

			foreach {title findSpec} [Global groups,k_info] {

				# Find the last monster in the group
				set match [angband k_info find -limit 1 \
					-backwards {*}$findSpec]

				# Get the assignment. This resolves alternates.
				set icon [angband k_info info [lindex $match 0] icon]

				# Add this group to the list
				NewItem $treeG $icon [mc $title]
			}
		}

		set_list_member {

			set treeM [Info $oop member,tree]

			set group [lindex $args 0]
			set findSpec [lindex [Global groups,k_info] [expr {$group * 2 + 1}]]

			# Get a list of monsters in the group
			set match [angband k_info find {*}$findSpec]

			# Add each match to the list
			foreach index $match {

				# Get the assignment. This resolves alternates.
				set icon [angband k_info info $index icon]

				# Hack -- object_desc
				set name [angband k_info info $index object_desc]

				# Collect info for each row
				NewItem $treeM $icon $name
			}

			# Keep a list of matching indexes
			Info $oop member,match $match
		}

		assign {
			set count 0
			set treeM [Info $oop member,tree]
			set what [lindex $args 0]
			switch -- $what {
				alternate -
				icon -
				sprite {
					# Build the assignment, such as "sprite 2"
					set assign [join $args]
					set match [Info $oop member,match]
					foreach item [$treeM selection get] {
						set row [NSList::Item2Row $treeM $item]
						set index [lindex $match $row]
						assign set object $index $assign
						set assign [angband k_info info $index icon]
						NSList::SetIcon $treeM "root child $row" $assign
						incr count
					}
					if {$item == [$treeM index last]} {
						set row [Info $oop group,current]
						set treeG [Info $oop group,tree]
						NSList::SetIcon $treeG "root child $row" $assign
					}
				}
			}
			return $count
		}

		get_icon {
			set row [lindex $args 0]
			set index [lindex [Info $oop member,match] $row]

			# Return the assignment. Does not resolve alternates.
#			return [angband k_info info $index icon]
			return [assign set object $index]
		}

		member_has_sound {
			return 0
		}

		get_sound {
			return {}
		}

		insert_sound {
			return 0
		}

		delete_sound {
			return 0
		}

		wants_sound {
			return 0
		}

		select_member {
			set row [lindex $args 0]
			set index [lindex [Info $oop member,match] $row]
			NSRecall::RecallObjectKind $index
		}

		group_names {
			set names {}
			foreach {title findSpec} [Global groups,k_info] {
				lappend names [mc $title]
			}
			return $names
		}

		group_list {
			set index -1
			set result {}
			foreach {title findSpec} [Global groups,k_info] {
				lappend result [incr index]
			}
			return $result
		}

		member_name {
			set group [lindex $args 0]
			set index [lindex $args 1]
			return [angband k_info info $index object_desc]
		}

		member_list {
			set group [lindex $args 0]
			set findSpec [lindex [Global groups,k_info] [expr {$group * 2 + 1}]]
			return [angband k_info find {*}$findSpec]
		}
	}

	return
}

proc NSAssign::hook_XXX {oop message args} {

	switch -- $message {

		set_list_group {
		}

		set_list_member {
		}

		assign {
		}

		get_icon {
		}

		member_has_sound {
			return 0
		}

		get_sound {
		}

		select_member {
		}

		member_name {
		}

		member_list {
		}
	}

	return
}

# NSAssign::ValueChanged_font_knowledge --
#
#	Called when the font,knowledge value changes.
#	Updates the Assign Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::ValueChanged_font_knowledge {oop} {

	# Get the desired font
	set font [Value font,knowledge]

	# Set the width of the group list
	set tree [Info $oop group,tree]
	set oldWidth [winfo width $tree]
	set newWidth [CalcGroupListWidth $oop]
	$tree configure -width $newWidth

	# Hack -- Resize the toplevel so the member list is not resized
	set diff [expr {$newWidth - $oldWidth}]
	if {$diff} {
		set win [Info $oop win]
		set newWidth [expr {[winfo width $win] + $diff}]
		NSToplevel::SetTotalWidth $win $newWidth
	}

	return
}

# NSAssign::CalcGroupListWidth --
#
#	Returns the desired width of the group list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::CalcGroupListWidth {oop} {

	variable Priv

	# Get the desired font
	set font [Value font,knowledge]

	# Minimum desired width of the group list
	set maxWidth 100

	# Check each hook
	foreach hook $Priv(hook) {

		# Check each name
		foreach name [hook_$hook $oop group_names] {

			# Calculate the width in pixels
			set width [font measure $font $name]

			# Remember the maximum width
			if {$width > $maxWidth} {
				set maxWidth $width
			}
		}
	}

	return [expr {[icon width] + 8 + $maxWidth + 20 + 4}]
}

# NSAssign::BrowserMotionCmd --
#
#	Called by NSIconBrowser.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::BrowserMotionCmd {oop which iconIndex} {

	set browserId [Info $oop browserId]

	# No icon was hit
	if {$iconIndex == -1} {
		StatusBar $oop {} 0
		return
	}

	# The mouse moved over the group list (ie, icon types)
	if {$which eq "group"} {
		set iconType [lindex [NSIconBrowser::Info $browserId iconTypes] $iconIndex]
		StatusBar $oop [format [mc browser-group] $iconType] 0
		return
	}

	set iconType [NSIconBrowser::Info $browserId iconType]
	set assign "icon $iconType $iconIndex"

	# Check each feature
	set max [angband f_info max]
	for {set index 0} {$index < $max} {incr index} {
		set assign2 [assign set feature $index]
		if {$assign eq $assign2} {
			set name [angband f_info info $index name]
			StatusBar $oop "($iconType $iconIndex) [mc Feature] #$index: \"$name\"" 0
			return
		}
	}

	# Check each object
	set max [angband k_info max]
	for {set index 0} {$index < $max} {incr index} {
		set assign2 [assign set object $index]
		if {$assign eq $assign2} {
			set name [angband k_info info $index object_desc]
			StatusBar $oop "($iconType $iconIndex) [mc Object] #$index: \"$name\"" 0
			return
		}
	}

	# Check each monster
	set max [angband r_info max]
	for {set index 0} {$index < $max} {incr index} {
		set assign2 [assign set monster $index]
		if {$assign eq $assign2} {
			set name [angband r_info info $index name]
			StatusBar $oop "($iconType $iconIndex) [mc Monster] #$index: \"$name\"" 0
			return
		}
	}

	StatusBar $oop "($iconType $iconIndex) [mc {Not assigned.}]" 0

	return
}

# NSAssign::DisplayIcon --
#
#	When statusBar.label2 is clicked, we display the icon of the
#	selected member, if any.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::DisplayIcon {oop} {

	set browserId [Info $oop browserId]
	set row [Info $oop member,current]
	if {$row == -1} return

	set icon [CallHook $oop get_icon $row]
	scan $icon "%s" assignType
	if {$assignType ne "icon"} return

	if {[Info $oop display] ne "icon"} {
		SetDisplay $oop icon
	}

	scan $icon "%s %s %d" assignType iconType iconIndex
	if {[lsearch -exact [NSIconBrowser::Info $browserId iconTypes] $iconType] != -1} {
		Info $oop display,ignoreSel 1
		NSIconBrowser::SeeIcon $browserId $iconType $iconIndex
		Info $oop display,ignoreSel 0
	}

	return
}

# NSAssign::SoundConfigChanged --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::SoundConfigChanged {oop} {

	set config [Info $oop sound,config]

	if {![CallHook $oop wants_sound]} return

	set tree [Info $oop member,tree]
	set max [expr {[$tree numitems] - 1}]
	for {set row 0} {$row < $max} {incr row} {
		if {[CallHook $oop member_has_sound $row]} {
			$tree item state set "root child $row" hasSound
		} else {
			$tree item state set "root child $row" !hasSound
		}
	}

	SetList_SoundAssign $oop

	return
}

# NSAssign::IconCfg --
#
#	Called when the game's icon configuration changes
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAssign::IconCfg {oop} {

	ValueChanged_font_knowledge $oop

	if {[winfo ismapped [Info $oop win]]} {
		SetList_Group $oop
		if {[Info $oop display] eq "alternate"} {
			SetList_Other $oop [Info $oop display]
		}
		if {[Info $oop display] eq "sprite"} {
			SetList_Other $oop [Info $oop display]
		}
	}

	return
}

