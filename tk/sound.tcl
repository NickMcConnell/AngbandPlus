# File: sound.tcl

# Purpose: the Sound Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSSound {

	variable Priv

# namespace eval NSSound
}

# NSSound::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::InitModule {} {

	variable Priv

	MsgCatInit sound

	NSModule::LoadIfNeeded NSList

	InitImageIfNeeded Image_ArrowLeft arrow-left.gif
	InitImageIfNeeded Image_ArrowRight arrow-right.gif

	set Priv(hook) {}
	lappend Priv(hook) event
	lappend Priv(hook) monster_attack
	lappend Priv(hook) monster_spell
	if {[variant OANGBANDTK ZANGBANDTK]} {
		lappend Priv(hook) martial_art
	# OANGBANDTK ZANGBANDTK
	}

	NSObject::New NSSound

	return
}

# NSSound::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::CloseModule {} {

	catch {
		destroy [Window sound]
	}

	return
}

# NSSound::NSSound --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::NSSound {oop} {

	InitWindow $oop

	set winMain [Window main]
	set win [Info $oop win]

	Info $oop config ""
	Info $oop config,current -1
	Info $oop dir ""
	Info $oop dir,current -1
	Info $oop sound,ignoreSel 0
	Info $oop group,current -1
	Info $oop group,keyword ""
	Info $oop sound,match {}
	Info $oop assign,ignoreSel 0

	NSWindowManager::RegisterWindow sound $win \
		"GetDefaultGeometry $win reqwidth main2" "" \
		"NSSound::DisplayCmd $oop"

	# Update ourself when the font,knowledge value changes
	qebind NSSound <Value-font,knowledge> \
		"NSSound::ValueChanged_font_knowledge $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSSound $oop $win

	#
	# Global list of application windows
	#

	Global sound,oop $oop
	Window sound $win

	return
}

# NSSound::~NSSound --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::~NSSound {oop} {

	return
}

# NSSound::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::Info {oop info args} {

	global NSSound

	# Verify the object
	NSObject::CheckObject NSSound $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSSound($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSSound($oop,$info)
			}
		}
	}

	return
}

# NSSound::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::InitWindow {oop} {

	global NSSound
	variable Priv

	set win .sound$oop
	toplevel $win
	wm title $win [mc Sound]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSSound::Close $oop"

	Info $oop win $win

	InitMenus $oop

	#
	# Divider
	#

	MakeDivider $win.divider1 x

	#
	# Tabs
	#

	set tabsId [NSObject::New NSTabs $win]
	foreach hook $Priv(hook) {
		NSTabs::Add $tabsId [mc $hook]
	}
	NSTabs::Info $tabsId invokeCmd "NSSound::InvokeTab $oop"
	NSTabs::Info $tabsId active 1
	Info $oop tabsId $tabsId

	set font [Value font,knowledge]

	#
	# Config List
	#

	set rowHgt [font metrics $font -linespace]
	incr rowHgt 8
	set height [expr {$rowHgt * 4}]

	set frame $win.frameConfig
	NSList::New $frame -checkbutton 1
	set tree $frame.tree
	$tree column configure 0 -text [mc "Configuration"]
	$tree configure -showheader 1 -width 100 -height $height

	NSList::OnSelection $tree \
		"NSSound::SelectionChanged_Config $oop %T %c %S %D"

	NSList::OnClick $tree \
		"NSSound::Click_Config $oop %r"

	NSList::OnToggle $tree \
		"NSSound::Toggle $oop %T %I %r"

	NSList::OnInvoke $tree \
		"NSSound::Invoke_Config $oop %r"

	Info $oop config,tree $tree

	#
	# Event List
	#

	set rowHgt [font metrics $font -linespace]
	if {[image height Image_Checked] > $rowHgt} {
		set rowHgt [image height Image_Checked]
	}
	incr rowHgt 8
	set width [CalcGroupListWidth $oop]

	set frame $win.frameGroup
	NSList::New $frame -checkbutton 1
	set tree $frame.tree
#	$tree column configure 0 -text [mc "Events"]
	$tree configure -showheader 1 -width $width -height 100

# +SND_MSG_COLORS
	$tree element create eColor rect -width 48
	$tree style elements s0 {eSel eChk eTxt eColor}
	$tree style layout s0 eColor -expand w -iexpand y
	$tree style layout s0 eSel -union {eChk eTxt eColor}

	#
	# Color Menu
	#

	set menu $win.colorMenu
	menu $menu -tearoff 0
	foreach attr [angband info term_attr] {
		# No black-on-black messages allowed
		if {$attr eq "TERM_DARK"} continue
		$menu add command -label $attr -background [Value $attr] \
			-command "NSSound::ClickColorInvoke $oop $attr"
	}

	bind SoundColorBindTag <ButtonPress-1> \
		"NSSound::ClickColor $oop $menu $tree %x %y"
	bindtags $tree [concat SoundColorBindTag [bindtags $tree]]

# -SND_MSG_COLORS

	NSList::OnSelection $tree \
		"NSSound::SelectionChanged_Group $oop %T %c %S %D"

	# Do something when a selected event is clicked.
	NSList::OnClick $tree \
		"NSSound::Click_Group $oop"
	NSList::OnInvoke $tree \
		"NSSound::Click_Group $oop"

	Info $oop group,tree $tree

	#
	# Directory List
	#

	set rowHgt [font metrics $font -linespace]
	incr rowHgt 8
	set height [expr {$rowHgt * 3}]

	set frame $win.frameDir
	NSList::New $frame
	set tree $frame.tree
	$tree column configure 0 -text [mc "Directory"]
	$tree configure -showheader 1 -width 100 -height $height

	NSList::OnSelection $tree \
		"NSSound::SelectionChanged_Dir $oop %T %c %S %D"

	NSList::OnClick $tree \
		"NSSound::Click_Dir $oop %r"

	NSList::OnInvoke $tree \
		"NSSound::Invoke_Dir $oop %r"

	Info $oop dir,tree $tree

	#
	# Sound List + Buttons + List of Assigned Sounds
	#

	frame $win.frameSound \
		-borderwidth 0

	#
	# Sound List
	#

	set rowHgt [font metrics $font -linespace]
	incr rowHgt 8
	set width 240

	set frame $win.frameSound.frameSound
	NSList::New $frame
	set tree $frame.tree
	$tree column configure 0 -text [mc "Sound Files"]
	$tree configure -showheader 1 -height 100 -selectmode extended

	NSList::OnSelection $tree \
		"NSSound::SelectionChanged_Sound $oop %T %c %S %D"

	# Do something when a selected sound is clicked.
	NSList::OnClick $tree \
		"NSSound::Click_Sound $oop %r"
	NSList::OnInvoke $tree \
		"NSSound::Click_Sound $oop %r"

	Info $oop sound,tree $tree

	#
	# Divider
	#

	MakeDivider $win.divider2 y

	#
	# Buttons
	#

	set frame $win.frameSound.frameButton
	frame $frame \
		-borderwidth 0
	tk::button $frame.buttonAdd -image Image_ArrowRight -width 18 -height 20 \
		-state disabled -command "NSSound::InsertSound $oop"
	tk::button $frame.buttonRemove -image Image_ArrowLeft -width 18 -height 20 \
		-state disabled -command "NSSound::DeleteSound $oop"
	pack $frame.buttonAdd -side top -pady {0 5}
	pack $frame.buttonRemove -side top

	Info $oop sound,button,add $frame.buttonAdd
	Info $oop sound,button,remove $frame.buttonRemove

	#
	# Assign List
	#

	set frame $win.frameSound.frameAssign
	NSList::New $frame
	set tree $frame.tree
	$tree column configure 0 -text [mc "Assigned Sounds"]
	$tree configure -showheader 1 -height [expr {$rowHgt * 5}] -selectmode extended

	NSList::OnSelection $tree \
		"NSSound::SelectionChanged_Assign $oop %T %c %S %D"

	# Do something when a selected sound assignment is clicked.
	NSList::OnClick $tree \
		"NSSound::Click_SoundAssign $oop %r"
	NSList::OnInvoke $tree \
		"NSSound::Click_SoundAssign $oop %r"

	Info $oop assign,tree $tree

	#
	# Splitter between bottom 2 areas
	#
	if {$::UseTile} {
		ttk::panedwindow $win.splitterV -orient vertical
	} else {
		panedwindow $win.splitterV -orient vertical -opaqueresize true -sashrelief sunken
	}
	raise $win.frameGroup ; raise $win.frameSound
	$win.splitterV add $win.frameGroup -weight 1
	$win.splitterV add $win.frameSound -weight 1

	#
	# Geometry of Sound + Buttons + Assign
	#

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
	set label2 [$win.statusBar itemcget t2 -label]
	bind $label2 <ButtonPress-1> \
		"NSSound::SynchView $oop"
	NSStatusText::StatusText $label2 $label \
		"Click to view highlighted rows."

	#
	# Geometry
	#

	grid rowconfig $win 0 -weight 0 -minsize 0
	grid rowconfig $win 1 -weight 0 -minsize 0
	grid rowconfig $win 2 -weight 0 -minsize 0
	grid rowconfig $win 3 -weight 1 -minsize 0
	grid rowconfig $win 4 -weight 1 -minsize 0
	grid rowconfig $win 5 -weight 0 -minsize 0
	grid columnconfig $win 0 -weight 1 -minsize 0
	grid columnconfig $win 1 -weight 0 -minsize 0
	grid columnconfig $win 2 -weight 1 -minsize 0
 
	grid $win.divider1 \
		-row 0 -column 0 -rowspan 1 -columnspan 3 -sticky ew
	grid [NSTabs::Info $tabsId canvas] \
		-row 1 -column 0 -rowspan 1 -columnspan 3 -sticky ew
	grid $win.frameConfig \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.frameDir \
		-row 2 -column 2 -rowspan 1 -columnspan 1 -sticky news
	grid $win.divider2 \
		-row 2 -column 1 -rowspan 1 -columnspan 1 -sticky ns -padx 2
if 1 {
	grid $win.splitterV \
		-row 3 -column 0 -rowspan 2 -columnspan 3 -sticky news
} else {
	grid $win.frameGroup \
		-row 3 -column 0 -rowspan 1 -columnspan 3 -sticky news
	grid $win.frameSound \
		-row 4 -column 0 -rowspan 1 -columnspan 3 -sticky news
}
	grid $win.statusBar \
		-row 5 -column 0 -rowspan 1 -columnspan 3 -sticky ew

	#
	# KeyPress bindings
	#

	bind $win <KeyPress-Escape> "NSSound::Close $oop"
	bind $win <Control-KeyPress-w> "NSSound::Close $oop"

	return
}

# NSSound::InitMenus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::InitMenus {oop} {

	variable MenuString
	variable Priv

	# Default accelerator modifier
	set mod "Ctrl"

	set win [Info $oop win]

	#
	# Menu bar
	#

	Info $oop mbarId [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSSound::MenuSetup $oop" -identifier MENUBAR]
	set mbar [Info $oop mbarId]

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSSound::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSSound::MenuInvoke $oop"

	#
	# Sound Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_SOUND
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_SOUND -label [mc Sound] -underline 0 -identifier M_SOUND

	set entries {}
	set i 1
	foreach hook $Priv(hook) {
		lappend entries [list -type radiobutton -label [mc $hook] \
			-variable NSSound($oop,radio,hook) -value $hook \
			-accelerator $i -identifier E_HOOK_$i]
		bind $win <KeyPress-$i> "NSSound::SetList_Group $oop $hook"
		incr i
	}
	lappend entries [list -type separator]
	lappend entries [list -type checkbutton -label [mc "Use Sound"] \
		-variable ::NSMainWindow([Global main,oop],useSound) \
		-identifier E_USE_SOUND]
	lappend entries [list -type command -label [mc "Volume..."] \
		-identifier E_VOLUME]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc "History"] \
		-identifier E_HISTORY]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_SOUND $entries

	#
	# Config Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_CONFIG
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_CONFIG -label [mc Config] -underline 0 -identifier M_CONFIG

	set entries {}
	lappend entries [list -type command -label [mc "New Configuration..."] \
		-underline 0 -identifier E_CONFIG_NEW]
	lappend entries [list -type command -label [mc "Add Configuration..."] \
		-underline 0 -identifier E_CONFIG_ADD]
	lappend entries [list -type command -label [mc "Edit Configuration..."] \
		-underline 0 -identifier E_CONFIG_EDIT]
	lappend entries [list -type command -label [mc "Remove Configuration"] \
		-underline 0 -identifier E_CONFIG_REMOVE]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc "Save"] \
		-underline 0 -identifier E_CONFIG_SAVE]
	lappend entries [list -type command -label [mc "Revert"] \
		-underline 0 -identifier E_CONFIG_REVERT]

	NSMenu::MenuInsertEntries $mbar -end MENU_CONFIG $entries

	#
	# Directory Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_DIR
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_DIR -label [mc Directory] -underline 0 -identifier M_DIR

	set entries {}
	lappend entries [list -type command -label [mc "Add Directory..."] \
		-underline 0 -identifier E_DIR_ADD]
	lappend entries [list -type command -label [mc "Edit Directory..."] \
		-underline 0 -identifier E_DIR_EDIT]
	lappend entries [list -type command -label [mc "Remove Directory"] \
		-underline 0 -identifier E_DIR_REMOVE]

	NSMenu::MenuInsertEntries $mbar -end MENU_DIR $entries

	set MenuString(M_SOUND) \
		"Contains commands for using sounds."
	set MenuString(E_HISTORY) \
		"Displays list of recently-played sounds."
	set MenuString(E_CLOSE) \
		"Closes the window."

	return
}

# NSSound::MenuSetup --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::MenuSetup {oop mbarId} {

	variable Priv

	set i 0
	foreach hook $Priv(hook) {
		lappend identList E_HOOK_[incr i]
	}

	lappend identList E_USE_SOUND E_VOLUME E_HISTORY E_CLOSE

	lappend identList E_CONFIG_NEW E_CONFIG_ADD
	if {[Info $oop config,current] != -1} {
		lappend identList E_CONFIG_EDIT E_CONFIG_REMOVE E_CONFIG_SAVE
	}

	lappend identList E_DIR_ADD
	if {[Info $oop dir,current] != -1} {
		lappend identList E_DIR_EDIT E_DIR_REMOVE
	}

	NSMenu::MenuEnable $mbarId $identList

	return
}

# NSSound::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::MenuSelect {oop menuId index ident} {

	variable MenuString

	switch -glob -- $ident {
		{} {
			set desc {}
		}

		E_HOOK_* {
			set desc "Displays this page."
		}

		E_USE_SOUND {
			if {[Value use_sound]} {
				set desc "Turns off sounds."
			} else {
				set desc "Turns on sounds."
			}
		}

		E_VOLUME {
			set desc "Change sound volume."
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

# NSSound::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::MenuInvoke {oop menuId ident} {

	variable Priv

	switch -glob -- $ident {
		E_HOOK_* {
			scan $ident "E_HOOK_%d" hookNum
			SetList_Group $oop [lindex $Priv(hook) [expr {$hookNum - 1}]]
		}
		E_USE_SOUND {NSMainWindow::ToggleSound [Global main,oop]}
		E_HISTORY {History $oop}
		E_VOLUME {
			ChangeSoundVolume $oop
		}
		E_CLOSE {Close $oop}

		E_CONFIG_NEW {Config $oop new}
		E_CONFIG_ADD {
			set types {
				{{Sound Config File} {.snd}}
				{{All files} {*}}
			}
			set path [tk_getOpenFile -filetypes $types -parent [Info $oop win] \
				-initialdir [PathTk config]]
			if {$path eq ""} return
			if {[file extension $path] ne ".snd"} {
				set answer [tk_messageBox -message [mc config-msg-notsnd] \
					-title [mc config-title-notsnd] -icon question \
					-parent [Info $oop win] -type yesno]
				if {$answer eq "no"} return
			}
			set row [llength [angband sound config listof]]
			Config::Sound::ConfigHack $path 1
			SetList_Config $oop
			set tree [Info $oop config,tree]
			$tree selection add "root child $row"
			SetList_Dir $oop
		}
		E_CONFIG_EDIT {Config $oop edit}
		E_CONFIG_REMOVE {
			set title [angband sound config cget [Info $oop config] -title]
			set msg [format [mc config-msg-remove] $title]
			set answer [tk_messageBox -title [mc config-title-remove] \
				-message $msg -type yesno -icon warning -parent [Info $oop win]]
			if {$answer eq "no"} return
			angband sound config delete [Info $oop config]
			SetList_Config $oop
		}
		E_CONFIG_SAVE {
			set config [Info $oop config]
			Config::Sound::WriteConfig $config
		}
		E_CONFIG_REVERT {
			set msg [format [mc config-msg-revert] $title]
			set answer [tk_messageBox -title [mc config-title-revert] \
				-message $msg -type yesno -icon warning -parent [Info $oop win]]
			if {$answer eq "no"} return
		}

		E_DIR_ADD {Directory $oop add}
		E_DIR_EDIT {Directory $oop edit}
		E_DIR_REMOVE {
			set path [angband sound dir cget [Info $oop dir] -path]
			set msg [format [mc dir-msg-remove] $path]
			set answer [tk_messageBox -title [mc dir-title-remove] \
				-message $msg -type yesno \
				-icon warning -parent [Info $oop win]]
			if {$answer eq "no"} return
			angband sound dir delete [Info $oop dir]
			SetList_Dir $oop
		}
	}

	return
}

# NSSound::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::DisplayCmd {oop message first} {

	variable Priv

	switch -- $message {
		preDisplay {
		}
		postDisplay {
			if {$first} {

				SetList_Config $oop
				SetList_Group $oop event
				SetList_Dir $oop
#				SetList_Sound $oop

				# Select first configuration, update event list
				set cfgList [angband sound config listof]
				if {[llength $cfgList]} {
					set tree [Info $oop config,tree]
					$tree selection add "root firstchild"
				}

				# Select first directory, set sound list
				set dirList [angband sound dir listof]
				if {[llength $dirList]} {
					set tree [Info $oop dir,tree]
					$tree selection add "root firstchild"
				}
			}

			# If the list of sounds changed, set the list again
			set dir [Info $oop dir]
			if {[info exists ::Global(sound,globCount,$dir)]} {
				if {[Info $oop globCount,$dir] != [Global sound,globCount,$dir]} {
					SetList_Sound $oop
				}
			}

			Info $oop msgcolor,changed 0
		}
		postWithdraw {
			if {[Info $oop msgcolor,changed]} {
				Config::MsgColor::Write
			}
		}
	}

	return
}

# NSSound::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::Close {oop} {

	NSWindowManager::Undisplay sound

	return
}

# NSSound::SetList_Config --
#
#	Set the config list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::SetList_Config {oop} {

	set tree [Info $oop config,tree]

	# Clear the list
	NSList::Clear $tree

	set configList [angband sound config listof]

	foreach config $configList {
		set active [angband sound config cget $config -active]
		set title [angband sound config cget $config -title]

		set item [$tree item create]
		NSList::SetText $tree $item $title
		$tree item lastchild root $item
		if {$active} {
			$tree item state set $item checked
		}
	}

	return
}

# NSSound::SetList_Group --
#
#	Set the group list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::SetList_Group {oop group} {

	variable Priv

	set tree [Info $oop group,tree]

	$tree column configure 0 -text [mc $group]

	set config [Info $oop config]
	set dir [Info $oop dir]
	Info $oop group $group

	Progress $oop open

	# Clear the list
	NSList::Clear $tree

	# Map rows to sound indexes
	set match {}

	set max [angband sound count $group]
	for {set i 0} {$i < $max} {incr i} {

		if {![angband sound exists $group $i]} continue

		set keyword [angband sound keyword $group $i]
		set desc [mc $group,$keyword]

		set checked 0
		if {$config ne ""} {
			if {[angband sound config count $config $group $keyword]} {
				set checked 1

				if {$::DEBUG} {
					if {[llength [Info $oop sound,match]]} {
						set sndList [angband sound assign $config $group $keyword]
						foreach {dir2 snd2} $sndList {
							if {$dir ne $dir2} continue
							if {[lsearch -exact [Info $oop sound,match] $snd2] == -1} {
								dbwin "Can't find sound \"$snd2\" for $keyword\n"
							}
						}
					}
				}
			}
		}

		set item [$tree item create]
		NSList::SetText $tree $item $desc
		$tree item lastchild root $item
		if {$checked} {
			$tree item state set $item checked
		}

# +SND_MSG_COLORS
		set color [angband sound color $group $keyword]
		$tree item element configure $item 0 eColor -fill [Value $color]
# -SND_MSG_COLORS

		# Map rows to sound indexes
		lappend match $i

		Progress $oop update $i $max
	}

	# Map rows to sound indexes
	Info $oop group,match $match

	# Radiobutton menu entries
	Info $oop radio,hook $group

	set tabsId [Info $oop tabsId]
	set current [NSTabs::Info $tabsId current]
	set tabId [NSTabs::GetNthId $tabsId [lsearch -exact $Priv(hook) $group]]
	if {$tabId != $current} {
		NSTabs::Smaller $tabsId $current
		NSTabs::Bigger $tabsId $tabId
		NSTabs::Info $tabsId current $tabId
	}

	Progress $oop close

	return
}

# NSSound::SetList_Dir --
#
#	Set the dir list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::SetList_Dir {oop} {

	set tree [Info $oop dir,tree]

	# Clear the list
	NSList::Clear $tree

	set dirList [angband sound dir listof]

	set i 0
	foreach dir $dirList {
		set path [angband sound dir cget $dir -path]

		set item [$tree item create]
		NSList::SetText $tree $item "$i: $path"
		$tree item lastchild root $item

		incr i
	}

	return
}

# NSSound::SetList_Sound --
#
#	Set the sound list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::SetList_Sound {oop} {

	set tree [Info $oop sound,tree]

	# Read the list of sound files if needed
	set dir [Info $oop dir]
	if {![info exists ::Global(sound,globCount,$dir)] ||
		![info exists ::NSSound($oop,globCount,$dir)] ||
		![Info $oop globCount,$dir]} {
		GlobSoundDirectory $dir
	}

	Progress $oop open

	# Clear the list
	NSList::Clear $tree

	set fileList [Global sound,fileName,$dir]
	set max [llength $fileList]

	# Row zero is "None", for "no sound assigned"
	set item [$tree item create]
	NSList::SetText $tree $item [mc sound-none]
	$tree item lastchild root $item

	set i 0
	foreach file $fileList {

		set item [$tree item create]
		NSList::SetText $tree $item $file
		$tree item lastchild root $item

		Progress $oop update $i $max
		incr i
	}

	Info $oop sound,match $fileList
	Info $oop globCount,$dir [Global sound,globCount,$dir]

	Progress $oop close

	return
}

# NSSound::SetList_Assign --
#
#	Display the sounds assigned to the selected keyword(s).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::SetList_Assign {oop} {

	set tree [Info $oop assign,tree]

	set config [Info $oop config]
	set dir [Info $oop dir]
	set group [Info $oop group]
	set keyword [Info $oop group,keyword]

	# Clear the list
	NSList::Clear $tree

	set soundIndex 0
	set soundList [angband sound assign $config $group $keyword]
	set soundList2 [CorrectSoundAssign $soundList]
	foreach {dir2 snd2} $soundList {dir3 snd3} $soundList2 {

		# Hack -- If the case changed, re-assign it
		if {$snd2 ne $snd3} {
			angband sound assign $config $group $keyword $soundIndex $dir2 $snd3
			set snd2 $snd3
		}
		if {![string length $snd2]} {
			set snd2 [mc sound-none]
		}
		set i [lsearch -exact [angband sound dir listof] $dir2]

		set item [$tree item create]
		NSList::SetText $tree $item "$i: $snd2"
		$tree item lastchild root $item

		incr soundIndex
	}

	return
}

# NSSound::Click_Config --
#
#	Do something when a selected config is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::Click_Config {oop row} {

	return
}

# NSSound::Click_Group --
#
#	Do something when a selected group is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::Click_Group {oop} {

	set config [Info $oop config]
	set group [Info $oop group]
	set keyword [Info $oop group,keyword]

	# There are no sounds assigned
	if {![angband sound config count $config $group $keyword]} return

	set treeA [Info $oop assign,tree]
	set soundIndex 0
	if {[$treeA selection count] == 1} {
		set item [$treeA selection get]
		set soundIndex [NSList::Item2Row $treeA $item]
	}

	# Get the n'th sound
	set list [angband sound assign $config $group $keyword $soundIndex]
	set dir [lindex $list 0]
	set snd [lindex $list 1]

	# Stop other sounds
	angband sound stop

	# Play the sound
	angband sound play $dir $snd

	return
}

# NSSound::ClickColor --
#
#	Pop up a color-picker when the message-color box associated with an
#	event is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::ClickColor {oop menu tree  x y} {

	set ident [$tree identify $x $y]
	if {[lindex $ident 0] ne "item"} return
	if {[llength $ident] != 6 || [lindex $ident 5] ne "eColor"} return
	set item [lindex $ident 1]
	set E [lindex $ident 5]
	set row [NSList::Item2Row $tree $item]

	scan [$tree item bbox $item first eColor] "%d %d %d %d" left top right bottom

	set x [expr {[winfo rootx $tree] + $left}]
	set y [expr {[winfo rooty $tree] + $bottom}]

	Info $oop clickcolor,item $item
	Info $oop clickcolor,row $row

	tk_popup $menu $x $y

	return -code break
}

# NSSound::ClickColorInvoke --
#
#	Change the message color for an event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::ClickColorInvoke {oop attr} {

	set row [Info $oop clickcolor,row]
	set group [Info $oop group]
	set index [lindex [Info $oop group,match] $row]
	set keyword [angband sound keyword $group $index]
	angband sound color $group $keyword $attr

	set tree [Info $oop group,tree]
	set item [Info $oop clickcolor,item]
	$tree item element configure $item first eColor -fill [Value $attr]

	Info $oop msgcolor,changed 1

	qegenerate <Track-message>

	return
}


# NSSound::Click_Dir --
#
#	Do something when a selected directory is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::Click_Dir {oop row} {
}

# NSSound::Click_Sound --
#
#	Do something when a selected sound is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::Click_Sound {oop row} {

	set dir [Info $oop dir]

	# Skip the "None" sound
	incr row -1

	# Get the sound name
	set sound [lindex [Info $oop sound,match] $row]

	# Stop other sounds
	angband sound stop

	# Play the sound
	angband sound play $dir $sound

	return
}

# NSSound::Click_SoundAssign --
#
#	Do something when a selected sound assignment is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::Click_SoundAssign {oop row} {

	set config [Info $oop config]
	set dir [Info $oop dir]
	set group [Info $oop group]
	set keyword [Info $oop group,keyword]

	set list [angband sound assign $config $group $keyword $row]
	set dir2 [lindex $list 0]
	set snd [lindex $list 1]

	# Stop other sounds
	angband sound stop

	# Play the sound
	angband sound play $dir2 $snd

	return
}

# NSSound::Invoke_Config --
#
#	Do something when a configuration is double-clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::Invoke_Config {oop row} {

	Config $oop edit

	return
}

# NSSound::Invoke_Dir --
#
#	Do something when a directory is double-clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::Invoke_Dir {oop row} {

	Directory $oop edit

	return
}

# NSSound::SelectionChanged_Config --
#
#	Called when the config selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::SelectionChanged_Config {oop tree count select deselect} {

	set tree [Info $oop group,tree]

	# Clear selection in event list (and clear assign list)
	set treeG [Info $oop group,tree]
	$treeG selection clear

	if {![llength $select]} {
		Info $oop config,current -1
		Info $oop config ""

		# Clear checkboxes in event list
		foreach item [$treeG item children root] {
			$treeG item state set $item !checked
		}

		return
	}

	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]
	Info $oop config,current $row
	Info $oop config [lindex [angband sound config listof] $row]

	set config [Info $oop config]
	set group [Info $oop group]

	# Update the event list to indicate sound assignments
	foreach event [Info $oop group,match] item [$treeG item children root] {
		set keyword [angband sound keyword $group $event]
		if {[angband sound config count $config $group $keyword]} {
			$treeG item state set $item checked
		} else {
			$treeG item state set $item !checked
		}
	}

	return
}

# NSSound::SelectionChanged_Group --
#
#	Called when the group selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::SelectionChanged_Group {oop tree count select deselect} {

	SoundSynchButtons $oop

	set win [Info $oop win]
	set selection [$tree selection get]

	# Nothing is selected. Clear the assign list.
	if {!$count} {
		Info $oop group,current -1
		Info $oop group,keyword ""
		NSList::Clear [Info $oop assign,tree]
		$win.statusBar itemconfigure t2 -text ""
		Info $oop oldLabel2 ""
		return
	}

	set config [Info $oop config]
	if {$config eq ""} return

	set group [Info $oop group]
	set match [Info $oop group,match]

	# Only one keyword is selected. Display assigned sounds in the
	# assign list.
	if {$count == 1} {
		set item [lindex $selection 0]
		set row [NSList::Item2Row $tree $item]
		Info $oop group,current $row

		set index [lindex $match $row]
		Info $oop group,keyword [angband sound keyword $group $index]

		SetList_Assign $oop
		set treeA [Info $oop assign,tree]
		if {[$treeA numitems] > 1} {
			$treeA selection add "root firstchild"
		}

		$win.statusBar itemconfigure t2 -text [Info $oop group,keyword]

	# More than one item is now selected. The sounds assigned to the
	# group,keyword are assigned to the newly selected keywords.
	# This allows the user to assign the same sounds to a number of
	# keywords just by selecting the new keywords.
	} else {

		set keyword [Info $oop group,keyword]
		set soundList [angband sound assign $config $group $keyword]
		foreach row $select {
			set index [lindex $match $row]
			set keyword [angband sound keyword $group $index]
			foreach {dir snd} [angband sound assign $config $group $keyword] {
				angband sound delete $config $group $keyword 0
			}
			foreach {dir snd} $soundList {
				angband sound insert $config $group $keyword 1000 $dir $snd
			}
			UpdateList_Group $oop $row
		}

		$win.statusBar itemconfigure t2 -text "$count selected"
	}

	return
}

# NSSound::SelectionChanged_Dir --
#
#	Called when the directory selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::SelectionChanged_Dir {oop tree count select deselect} {

	if {![llength $select]} {
		Info $oop dir,current -1
		Info $oop dir ""

		# Clear sound list
		set tree [Info $oop sound,tree]
		NSList::Clear $tree
		return
	}

	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]
	Info $oop dir,current $row
	Info $oop dir [lindex [angband sound dir listof] $row]
	SetList_Sound $oop

	return
}

# NSSound::SoundSynchButtons --
#
#	Enable/disable buttons for assigning sounds.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::SoundSynchButtons {oop} {

	set treeG [Info $oop group,tree]
	set treeS [Info $oop sound,tree]
	set treeA [Info $oop assign,tree]

	if {[$treeS selection count] &&
		([$treeG selection count] == 1)} {
		[Info $oop sound,button,add] configure -state normal
	} else {
		[Info $oop sound,button,add] configure -state disabled
	}

	if {[$treeA selection count]} {
		[Info $oop sound,button,remove] configure -state normal
	} else {
		[Info $oop sound,button,remove] configure -state disabled
	}

	return
}

# NSSound::SelectionChanged_Sound --
#
#	Called when the sound selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::SelectionChanged_Sound {oop tree count select deselect} {

	SoundSynchButtons $oop

	if {($count != 1) || ![llength $select]} return

	set item [$tree selection get]
	set row [NSList::Item2Row $tree $item]

	if {$row} {
		incr row -1
		set sound [lindex [Info $oop sound,match] $row]
		set dir [Info $oop dir]

		# Stop other sounds
		angband sound stop

		angband sound play $dir $sound
	}

	return
}

# NSSound::SelectionChanged_Assign --
#
#	Called when the assign selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::SelectionChanged_Assign {oop tree count select deselect} {

	SoundSynchButtons $oop

	if {($count != 1) || ![llength $select]} return

	set item [$tree selection get]
	set row [NSList::Item2Row $tree $item]

	set config [Info $oop config]
	set dir [Info $oop dir]
	set group [Info $oop group]
	set keyword [Info $oop group,keyword]

	set list [angband sound assign $config $group $keyword $row]
	set dir2 [lindex $list 0]
	set snd [lindex $list 1]

	if {$snd eq ""} return

	# Selected sound is in current directory
	if {$dir eq $dir2} {

		set row [lsearch -exact [Info $oop sound,match] $snd]
		if {$row == -1} return
		incr row

		set treeS [Info $oop sound,tree]
		if {[$treeS selection includes "root child $row"]} {
			if {![Info $oop assign,ignoreSel]} {
				# Stop other sounds
				angband sound stop
				angband sound play $dir2 $snd
			}
		} else {

			# Highlight the sound (and play it if it wasn't highlighted)
			Info $oop sound,ignoreSel 1
			$treeS selection modify [list "root child $row"] all
			Info $oop sound,ignoreSel 0
		}

		# Scroll the sound into view
		$treeS see "root child $row"

	# Play the sound
	} else {
		if {![Info $oop assign,ignoreSel]} {
			# Stop other sounds
			angband sound stop
			angband sound play $dir2 $snd
		}
	}

	return
}

# NSSound::Toggle --
#
#	Toggle a checkbox in a list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::Toggle {oop tree item row} {

	set config [lindex [angband sound config listof] $row]
	set active [angband sound config cget $config -active]

	set active [expr {!$active}]
	$tree item state set $item ~checked
	angband sound config configure $config -active $active

	return
}

# NSSound::StatusBar --
#
#	Display text in the status bar, perhaps clearing it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label [$win.statusBar itemcget t1 -label]
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

# NSSound::UpdateList_Group --
#
#	When a sound is assigned/unassigned, the event list must
#	be updated.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::UpdateList_Group {oop row} {

	set tree [Info $oop group,tree]
	set config [Info $oop config]
	set group [Info $oop group]

	set evt [lindex [Info $oop group,match] $row]
	set keyword [angband sound keyword $group $evt]

	set item [$tree index "root child $row"]

	if {[angband sound config count $config $group $keyword]} {
		$tree item state set $item checked
	} else {
		$tree item state set $item !checked
	}

	return
}

# NSSound::UpdateList_Assign --
#
#	Configure the Widget canvas item on the row of a list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::UpdateList_Assign {oop row} {

	set tree [Info $oop assign,tree]
	set config [Info $oop config]
	set group [Info $oop group]
	set keyword [Info $oop group,keyword]

	set list [angband sound assign $config $group $keyword $row]
	set dir2 [lindex $list 0]
	set snd2 [lindex $list 1]
	set i [lsearch -exact [angband sound dir listof] $dir2]
	if {![string length $snd2]} {set snd2 [mc sound-none]}

	NSList::SetText $tree "root child $row" "$i: $snd2"

	return
}

# NSSound::InvokeTab --
#
#	Called when a tab is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::InvokeTab {oop tabsId tabId} {

	variable Priv
	set index [lsearch -exact [NSTabs::Info $tabsId id] $tabId]
	SetList_Group $oop [lindex $Priv(hook) $index]

	return
}

# NSSound::InsertSound --
#
#	Insert a None sound to the selected keyword.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::InsertSound {oop} {

	if {[Info $oop group,current] == -1} return

	set config [Info $oop config]
	set dir [Info $oop dir]
	set group [Info $oop group]
	set keyword [Info $oop group,keyword]

	# Remember how many sounds were assigned
	set count [angband sound config count $config $group $keyword]

	# Append every selected sound to the selected event
	set treeS [Info $oop sound,tree]
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
		angband sound insert $config $group $keyword 1000 $dir $sound
	}

	# Redisplay the list of assignments
	SetList_Assign $oop

	# Highlight and view the new assignments
	set treeA [Info $oop assign,tree]
	Info $oop assign,ignoreSel 1
	$treeA selection add "root child $count" last
	Info $oop assign,ignoreSel 0
	$treeA see "root child $count"

	# If there were no assignments, then give a visual indication
	# that the keyword now has an assignment.
	if {!$count} {
		UpdateList_Group $oop [Info $oop group,current]
	}

	return
}

# NSSound::DeleteSound --
#
#	Delete a frame from the selected keyword.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::DeleteSound {oop} {

	set treeA [Info $oop assign,tree]

	if {![$treeA selection count]} return

	set config [Info $oop config]
	set group [Info $oop group]
	set keyword [Info $oop group,keyword]

	# Delete each selected assignment
	foreach item [$treeA selection get] {
		lappend rows [NSList::Item2Row $treeA $item]
	}
	foreach index [lsort -integer -decreasing $rows] {
		angband sound delete $config $group $keyword $index
	}

	SetList_Assign $oop

	set count [angband sound config count $config $group $keyword]
	if {$count} {
		if {$index >= $count} {
			set index [expr {$count - 1}]
		}
		Info $oop assign,ignoreSel 1
		$treeA selection add "root child $index"
		Info $oop assign,ignoreSel 0
		$treeA see "root child $index"

	} else {
		UpdateList_Group $oop [Info $oop group,current]
	}

	return
}

# NSSound::Progress --
#
#	Show, update, and hide the progress bar.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::Progress {oop action args} {

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

# NSSound::Config --
#
#	Puts up a dialog to allow the user to create/edit a configuration.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::Config {oop action} {

	set win [Info $oop win].config
	toplevel $win
	wm title $win [mc config-title-$action]
	wm withdraw $win
	TransientToWin $win [Info $oop win]

	message $win.prompt \
		-text [mc config-prompt-$action] -width 350
	pack $win.prompt \
		-side top -padx 5 -pady 5 -anchor w

	#
	# Title
	#

	set frame $win.frameTitle
	frame $frame \
		-borderwidth 0
	label $frame.label \
		-text [mc Title:]
	entry $frame.entry \
		-width 40

	pack $frame \
		-side top -anchor w -padx 5 -pady 5
	pack $frame.label \
		-side top -padx 5 -anchor w
	pack $frame.entry \
		-side left -padx 5

	#
	# File
	#

	set frame $win.frameFile
	frame $frame \
		-borderwidth 0
	label $frame.label \
		-text [mc File:]
	entry $frame.entry \
		-width 20

	pack $frame \
		-side top -anchor w -padx 5 -pady 5
	pack $frame.label \
		-side top -padx 5 -anchor w
	pack $frame.entry \
		-side left -padx 5

	MakeDivider $win.divider x
	pack $win.divider \
		-side top -fill x -padx 10 -pady 5

	#
	# Buttons
	#

	set frame $win.frameButtons
	frame $frame \
		-borderwidth 0
	button $frame.begin \
		-text [mc OK] -width 9 -default active \
		-command "NSSound::Config_Accept $oop"
	button $frame.cancel \
		-text [mc Cancel] -width 9 -command "destroy $win"

	pack $frame \
		-side top -padx 5 -pady 0 -anchor e
	pack $frame.cancel \
		-side right -padx 5 -pady 5
	pack $frame.begin \
		-side right -padx 5 -pady 5

	NSUtils::SetDefaultButton $win $win.frameButtons.begin

	bind $win <KeyPress-Return> \
		"NSUtils::InvokeDefaultButton $win"
	bind $win <KeyPress-Escape> \
		"NSUtils::InvokeButton $frame.cancel"

	Info $oop setcfg,win $win
	Info $oop setcfg,action $action
	Info $oop setcfg,config [Info $oop config]

	if {$action eq "edit"} {
		set config [Info $oop config]
		set title [angband sound config cget $config -title]
		$win.frameTitle.entry insert 0 $title
		set file [angband sound config cget $config -file]
		$win.frameFile.entry insert 0 $file
	}
	if {$action eq "new"} {
		$win.frameTitle.entry insert 0 Untitled
		$win.frameFile.entry insert 0 untitled.snd
	}
	$win.frameTitle.entry selection range 0 end
	focus $win.frameTitle.entry

	WindowPosition $win 2 3

	return
}

# NSSound::Config_Accept --
#
#	Accept changes made to a configuration.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::Config_Accept {oop} {

	set treeC [Info $oop config,tree]
	set config [Info $oop setcfg,config]
	set win [Info $oop setcfg,win]
	set title [$win.frameTitle.entry get]
	set file [$win.frameFile.entry get]

	# Checks for "" as well
	if {[file extension $file] ne ".snd"} {
		tk_messageBox -message [mc config-msg-ext] \
			-title [mc config-title-ext]
		return
	}

	switch -- [Info $oop setcfg,action] {
		edit {
			destroy $win
			angband sound config configure $config -title $title -file $file
			set row [lsearch -exact [angband sound config listof] $config]
			NSList::SetText $treeC "root child $row" $title
		}
		new {
			# Warn if there is already a file with that name
			if {[file exists [PathTk config $file]]} {
				set msg [format [mc config-msg-exist] $file]
				set answer [tk_messageBox -message $msg \
					-title [mc config-title-exist] -type yesno \
					-parent $win -icon question]
				if {$answer eq "no"} return
			}
			destroy $win
			set row [llength [angband sound config listof]]
			set config [angband sound config new -title $title -file $file]
			SetList_Config $oop
			$treeC activate "root child $row"
			$treeC selection add active
			$treeC see active
		}
	}

	return
}

# NSSound::Directory --
#
#	Puts up a dialog to allow the user to add/edit a directory.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::Directory {oop action} {

	set win .setsounddir
	toplevel $win
	wm title $win [mc dir-title-$action]
	wm withdraw $win
	TransientToWin $win [Info $oop win]

	frame $win.frameSrc \
		-borderwidth 0

	set frame $win.frameSrc
	message $frame.labelSrc \
		-text [mc dir-prompt-$action] -width 350
	entry $frame.entrySrc \
		-width 60
	button $frame.buttonSrc \
		-text [mc Browse...] \
		-command "NSSound::Directory_ChooseDir $oop $win $frame.entrySrc"

	set frame $win.frameSrc
	pack $frame \
		-side top -padx 5 -pady 5
	pack $frame.labelSrc \
		-in $frame -side top -padx 5 -anchor w
	pack $frame.entrySrc \
		-in $frame -side left -padx 5
	pack $frame.buttonSrc \
		-in $frame -side left -padx 5

	MakeDivider $win.divider x
	pack $win.divider \
		-side top -fill x -padx 10 -pady 5

	set frame $win.frameButtons
	frame $frame \
		-borderwidth 0
	button $frame.begin \
		-text [mc OK] -width 9 -default active \
		-command "NSSound::Directory_Accept $oop $win $win.frameSrc.entrySrc"
	button $frame.cancel \
		-text [mc Cancel] -width 9 -command "destroy $win"

	pack $frame \
		-side top -padx 5 -pady 0 -anchor e
	pack $frame.cancel \
		-side right -padx 5 -pady 5
	pack $frame.begin \
		-side right -padx 5 -pady 5

	NSUtils::SetDefaultButton $win $win.frameButtons.begin

	bind $win <KeyPress-Return> \
		"NSUtils::InvokeDefaultButton $win"
	bind $win <KeyPress-Escape> \
		"NSUtils::InvokeButton $frame.cancel"

	Info $oop setdir,action $action
	Info $oop setdir,dir [Info $oop dir]

	if {$action eq "edit"} {
		set dir [Info $oop dir]
		set path [angband sound dir cget $dir -path]
		$win.frameSrc.entrySrc insert 0 $path
	}
	$win.frameSrc.entrySrc selection range 0 end
	focus $win.frameSrc.entrySrc

	WindowPosition $win 2 3

	return
}

# NSSound::Directory_ChooseDir --
#
#	Puts up a dialog to allow the user to change the current
#	sound directory.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::Directory_ChooseDir {oop parent entry} {

	set initialDir [$entry get]
	if {![file exists $initialDir] || ![file isdirectory $initialDir]} {
		set initialDir [CPath]
	}

	set newDir [tk_chooseDirectory -mustexist yes -parent $parent \
		-initialdir $initialDir]

	if {[string length $newDir]} {
		$entry delete 0 end
		$entry insert 0 [file nativename [LongName $newDir]]
	}

	return
}

# NSSound::Directory_Accept --
#
#	Tell the binary the location of the sound directory, if it is
#	valid.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::Directory_Accept {oop win entry} {

	set path [$entry get]
	if {[file exists $path]} {
		set path [file nativename [LongName $path]]
	}

	# Is it a real directory?
	if {![file exists $path] || ![file isdirectory $path]} {
		set answer [tk_messageBox -parent $win -title "No Such Directory" \
			-message "No such directory\n\n    \"$path\"\n\n\
			Use it anyways?" -type yesno]
		if {$answer eq "no"} return
	}

	destroy $win
	update

	set treeD [Info $oop dir,tree]

	switch -- [Info $oop setdir,action] {
		add {
			set row [llength [angband sound dir listof]]
			angband sound dir new -path $path
			SetList_Dir $oop
			$treeD activate "root child $row"
			$treeD selection add active
			$treeD see active
		}
		edit {
			set dir [Info $oop setdir,dir]
			angband sound dir configure $dir -path $path
			Global sound,globCount,$dir 0
			Info $oop globCount,$dir 0

			set row [lsearch -exact [angband sound dir listof] $dir]
			NSList::SetText $treeD "root child $row" "$row: $path"
			SetList_Sound $oop
		}
	}

	return
}

# NSSound::ValueChanged_font_knowledge --
#
#	Called when the font,knowledge value changes.
#	Updates the Assign Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::ValueChanged_font_knowledge {oop} {

	# Set the width of the group list
	set treeG [Info $oop group,tree]
	$treeG configure -width [CalcGroupListWidth $oop]

	return
}

# NSSound::CalcGroupListWidth --
#
#	Returns the desired width of the group list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::CalcGroupListWidth {oop} {

	variable Priv

	# Get the desired font
	set font [Value font,knowledge]

	# Minimum desired width of the group list
	set maxWidth 100

	# Calculate the maximum width of the sound names in each group
	foreach group $Priv(hook) {

		# Count the sounds in this group
		set max [angband sound count $group]

		# Check each sound
		for {set i 0} {$i < $max} {incr i} {

			# Require real sound
			if {![angband sound exists $group $i]} continue

			# Get the keyword
			set keyword [angband sound keyword $group $i]

			# Get the description
			set desc [mc $group,$keyword]

			# Calculate the width in pixels
			set width [font measure $font $desc]

			# Remember the maximum width
			if {$width > $maxWidth} {
				set maxWidth $width
			}
		}
	}

	return [expr {$maxWidth + [image width Image_Checked] + 20 + 8}]
}

# NSSound::SynchView --
#
#	Scrolls the selected group, sound, and sound assignment into view.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::SynchView {oop} {

	set treeG [Info $oop group,tree]
	if {[$treeG selection count] == 1} {
		$treeG see [$treeG selection get]
	}

	set treeS [Info $oop sound,tree]
	if {[$treeS selection count] == 1} {
		$treeS see [$treeS selection get]
	}

	set treeA [Info $oop assign,tree]
	if {[$treeA selection count] == 1} {
		$treeA see [$treeA selection get]
	}

	return
}

# NSSound::History --
#
#	Display list of recent sound events.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::History {oop} {

	variable Priv

	set win [Info $oop win].history
	toplevel $win
	wm title $win [mc "Sound History"]

	wm withdraw $win
	TransientToWin $win [Info $oop win]

	set frame $win.frame
	set tree [NSList::New $frame -columns 5]
	$frame configure -borderwidth 0
	$tree configure -width 400 -height 300 -showheader yes
	for {set C 0} {$C < 5} {incr C} {
		$tree style layout s$C eTxt -padx 6
	}
	$tree column configure 0 -text [mc Description] -tag desc
	$tree column configure 1 -text [mc "Sound File"] -tag file
	$tree column configure 2 -text [mc Group] -tag group
	$tree column configure 3 -text [mc Configuration] -tag config
	$tree column configure 4 -text [mc Directory] -tag dir

	NSList::OnSelection $tree \
		"NSSound::History_Select $oop %T %c %S %D"

	NSList::OnClick $tree \
		"NSSound::History_Click $oop %T %I %r"
	NSList::OnInvoke $tree \
		"NSSound::History_Click $oop %T %I %r"

	pack $frame -expand yes -fill both

	foreach group $Priv(hook) {
		set name($group) [mc $group]
	}
	set name(monster) [mc Monster]

	set dirList [angband sound dir listof]

	set history {}
	set count [angband sound history count]
	for {set i 0} {$i < $count} {incr i} {
		set list [angband sound history get $i]
		set dir [lindex $list 0]
		set snd [lindex $list 1]
		set config [lindex $list 2]
		set group [lindex $list 3]
		set keyword [lindex $list 4]
		if {$group eq "monster"} {
			scan $keyword mon%d r_idx
			set desc "\"[angband r_info info $r_idx name]\""
		} else {
			set desc [mc $group,$keyword]
		}

		set item [NSList::NewItem $tree]
		NSList::SetTextEx $tree $item file $snd
		NSList::SetTextEx $tree $item config [angband sound config cget $config -title]
		NSList::SetTextEx $tree $item desc $desc
		NSList::SetTextEx $tree $item group $name($group)
		NSList::SetTextEx $tree $item dir [lsearch -exact $dirList $dir]

		lappend history $list
	}

	bind $win <KeyPress-Escape> "destroy $win"

	Info $oop history,history $history
	Info $oop history,tree $tree

	set x [NSToplevel::FrameLeft [Info $oop win]]
	set y [NSToplevel::FrameTop [Info $oop win]]
	set width [winfo reqwidth $frame.yscroll]
	for {set C 0} {$C < [$tree numcolumns]} {incr C} {
		incr width [$tree column neededwidth $C]
	}
	wm geometry $win ${width}x[winfo reqheight [Info $oop win]]+$x+$y
	update idletasks

	WindowBringToFront $win
	update

	if {[$tree numitems] > 1} {
		$tree see "root lastchild"
	}

	return
}

# NSSound::History_Select --
#
#	Handle selection in History Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::History_Select {oop tree count select deselect} {

	if {![llength $select]} return

	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]

	History_Click $oop $tree $item $row

	return
}

# NSSound::History_Click --
#
#	Called when a selected event is clicked in the History Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::History_Click {oop tree item row} {

	set history [lindex [Info $oop history,history] $row]
	set dir [lindex $history 0]
	set snd [lindex $history 1]

	# Stop other sounds
	angband sound stop

	angband sound play $dir $snd

	return
}

# NSSound::ChangeSoundVolume --
#
#	Put up a window to allow the user to change the volume of sound playback.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::ChangeSoundVolume {oop} {

	set win [Info $oop win].volume
	catch {destroy $win}
	toplevel $win
	wm title $win [mc "Sound Volume"]

	wm withdraw $win
	TransientToWin $win [Info $oop win]

	scale $win.volume \
		-orient horizontal \
		-length 200 -from 0 -to 100 \
		-showvalue yes -command "NSSound::SetVolume $oop"
	$win.volume set [angband sound volume]
	pack $win.volume -padx 10 -pady 8

	button $win.ok -text [mc OK] -width 11 -command "destroy $win"
	pack $win.ok -side top -pady {0 8}

	bind $win <KeyPress-Escape> "destroy $win"

	set x [NSToplevel::FrameLeft [Info $oop win]]
	set y [NSToplevel::FrameTop [Info $oop win]]
	incr x 100
	incr y 150
	wm geometry $win +$x+$y
	update idletasks

	WindowBringToFront $win
	update

	return
}

# NSSound::SetVolume --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSSound::SetVolume {oop volume} {

	angband sound volume [expr int($volume)]
	return
}

