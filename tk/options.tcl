# File: options.tcl

# Purpose: the Options Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSOptions {

	variable MenuString
	variable Priv

# namespace eval NSOptions
}

# NSOptions::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::InitModule {} {

	variable Priv

	MsgCatInit option

	NSModule::LoadIfNeeded NSList

	# If the tk/doc/options-index file does not exist, then create it.
	# Otherwise, recreate the file if it is older than tk/doc/options.txt.
	CheckOptionsIndex

	set Priv(page) {}
	lappend Priv(page) Interface
	lappend Priv(page) Command
	lappend Priv(page) Prompt
	lappend Priv(page) Dungeon
	lappend Priv(page) Monster
	lappend Priv(page) Object
	lappend Priv(page) Disturbance
	lappend Priv(page) Efficiency
	lappend Priv(page) Misc
	if {$::JAPANESE} {
		lappend Priv(page) Japanese
	}
	lappend Priv(page) Cheating
	lappend Priv(page) Other

	# Hack -- Verify that all displayed options are valid
	if {$::DEBUG} {
		foreach page $Priv(page) {
			foreach option [GetPage $page] {
				Setting $option
			}
		}
	}

	set Priv(find,string) ""
	set Priv(find,fromStart) 1

	# Create the Options Window
	NSObject::New NSOptions

	return
}

# NSOptions::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::CloseModule {} {

	catch {
		destroy [Window options]
	}

	return
}

# NSOptions::NSOptions --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::NSOptions {oop} {

	global Angband
	global NSOptions
	variable Priv

	Info $oop ignoreChange 0
	Info $oop option,current -1
	Info $oop option,current2 -1
#	Info $oop autosave [Value options,autosave]

	trace variable NSOptions($oop,scale,value) w \
		"NSOptions::EntryTextVarProc $oop"

	InitWindow $oop

	set win [Info $oop win]

	Info $oop page none
	Info $oop tempWidget {}

	# Set the list
	Info $oop page [lindex $Priv(page) 0]

	NSWindowManager::RegisterWindow options $win \
		"GetDefaultGeometry $win reqwidth main2" "" \
		"NSOptions::DisplayCmd $oop"

	# Update display when setting changes
	qebind NSOptions <Setting> \
		"NSOptions::SettingChanged $oop %d %c"
	qeconfigure NSOptions <Setting> -active no

	# Update ourself when the font,options value changes
	qebind NSOptions <Value-font,options> \
		"NSOptions::ValueChanged_font_options $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSOptions $oop $win

	#
	# Global list of application windows
	#

	Global options,oop $oop
	Window options $win

	return
}

# NSOptions::~NSOptions --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::~NSOptions {oop} {

	return
}

# NSOptions::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::Info {oop info args} {

	global NSOptions

	# Verify the object
	NSObject::CheckObject NSOptions $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSOptions($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSOptions($oop,$info)
			}
		}
	}

	return
}

# NSOptions::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::InitWindow {oop} {

	variable Priv

	set win .options$oop
	toplevel $win
	wm title $win [mc Options]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSOptions::Close $oop"

	Info $oop win $win

	InitMenus $oop

	#
	# Divider
	#

	MakeDivider $win.divider1 x

	#
	# Group list
	#

	set rowHgt [font metrics [Value font,options] -linespace]
	incr rowHgt 8

	set frameGroup $win.frameGroup
	set tree [NSList::New $frameGroup -font options]
	$tree configure -width 100 -height 200

	# Show options when a page is selected
	NSList::OnSelection $tree \
		"NSOptions::SelectionChanged_Group $oop %T %c %S %D"

	Info $oop group,tree $tree

	#
	# Options list
	#

	set frame $win.frame
	set tree [NSList::New $frame -font options]
	set width 300
	if {[ScreenWidth] > 800} {
		set width 400
	}
	$tree configure -width $width -height 200

	NSList::OnSelection $tree \
		"NSOptions::SelectionChanged $oop %T %c %S %D"

	NSList::OnToggle $tree \
		"NSOptions::Toggle $oop %T %I %r"

	Info $oop option,tree $tree

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
	qebind NSOptions <Value-listBG> \
		"$win.frameHelp.text configure -background \[Value listBG]"

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 20

	#
	# Geometry: Help
	#

	grid rowconfigure $win.frameHelp 0 -weight 0
	grid rowconfigure $win.frameHelp 1 -weight 1
	grid columnconfigure $win.frameHelp 0 -weight 1
	grid columnconfigure $win.frameHelp 1 -weight 0

	grid $win.frameHelp.text \
		-row 1 -column 0 -rowspan 2 -columnspan 1 -sticky news
	grid $win.frameHelp.yscroll \
		-row 1 -column 1 -rowspan 2 -columnspan 1 -sticky ns

	#
	# Geometry: Window
	#

	grid rowconfigure $win 0 -weight 0
	grid rowconfigure $win 1 -weight 2
	grid rowconfigure $win 2 -weight 0
	grid rowconfigure $win 3 -weight 1
	grid rowconfigure $win 4 -weight 0
	grid columnconfigure $win 0 -weight 0
	grid columnconfigure $win 1 -weight 1

	grid $win.divider1 \
		-row 0 -column 0 -rowspan 1 -columnspan 2 -sticky ew
	grid $frameGroup \
		-row 1 -column 0 -rowspan 3 -columnspan 1 -sticky news
	grid $win.frame \
		-row 1 -column 1 -rowspan 1 -columnspan 1 -sticky news
	grid $win.divider2 \
		-row 2 -column 1 -rowspan 1 -columnspan 1 -sticky ew -pady 2
	grid $win.frameHelp \
		-row 3 -column 1 -rowspan 1 -columnspan 1 -sticky news
	grid $win.statusBar \
		-row 4 -column 0 -rowspan 1 -columnspan 2 -sticky ew

	bind $win <KeyPress-Escape> "NSOptions::Close $oop"
	bind $win <Control-KeyPress-w> "NSOptions::Close $oop"
	bind $win <KeyPress-f> "NSOptions::Find $oop 0"
	bind $win <KeyPress-g> "NSOptions::Find $oop 1"

	return
}

# NSOptions::InitMenus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::InitMenus {oop} {

	global NSOptions
	variable MenuString
	variable Priv

	set win [Info $oop win]
	set mod "Ctrl"

	#
	# Menu bar
	#

	set mbar [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSOptions::SetupMenus $oop" -identifier MENUBAR]
	Info $oop mbarId $mbar

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSOptions::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSOptions::MenuInvoke $oop"

	#
	# Options Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_OPTIONS
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_OPTIONS -label [mc Options] -underline 0 \
		-identifier M_OPTIONS

	set entries {}

	lappend entries [list -type command -label [mc Find...] \
		-underline 0 -accelerator f -identifier E_FIND]
	lappend entries [list -type command -label [mc "Find Again"] \
		-underline 6 -accelerator g -identifier E_FIND_AGAIN]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc "Save As Default"] \
		-underline 0 -identifier E_WRITE_DEFAULT]
	lappend entries [list -type command -label [mc "Load Default Settings"] \
		-underline 0 -identifier E_READ_DEFAULT]
#	lappend entries [list -type checkbutton -label [mc "AutoSave Settings"] \
#		-underline 0 -variable ::NSOptions($oop,autosave) -identifier E_AUTOSAVE]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_OPTIONS $entries

	set MenuString(M_OPTIONS) \
		"Contains commands for using game settings."
	set MenuString(E_FIND) \
		"Searches for an option by keyword."
	set MenuString(E_FIND_AGAIN) \
		"Repeats the previous search."
	set MenuString(E_WRITE_DEFAULT) \
		"Makes the current settings the default when creating characters."
	set MenuString(E_READ_DEFAULT) \
		"Reads the saved settings, if any."
	set MenuString(E_CLOSE) \
		"Closes the window."

	return
}

# NSOptions::SetupMenus --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::SetupMenus {oop mbarID} {

	variable Priv

	lappend identList E_FIND E_FIND_AGAIN
	lappend identList E_WRITE_DEFAULT E_AUTOSAVE E_CLOSE

	if {[file exists [PathTk config setting]]} {
		lappend identList E_READ_DEFAULT
	}

	NSMenu::MenuEnable $mbarID $identList

	return
}

# NSOptions::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::MenuSelect {oop menuId index ident} {

	variable MenuString
	variable Priv

	switch -glob -- $ident {
		{} {
			set desc {}
		}
		E_AUTOSAVE {
			if {[Info $oop autosave]} {
				set desc "Do not save options as default when exiting."
			} else {
				set desc "Save options as default when exiting."
			}
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

# NSOptions::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::MenuInvoke {oop menuId ident} {

	variable Priv

	switch -glob -- $ident {
		E_FIND {Find $oop 0}
		E_FIND_AGAIN {Find $oop 1}
		E_WRITE_DEFAULT {WriteSettings $oop}
		E_READ_DEFAULT {ReadSettings $oop}
		E_AUTOSAVE {Value options,autosave [Info $oop autosave]}
		E_CLOSE {Close $oop}
	}

	return
}

# NSOptions::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::DisplayCmd {oop message first} {

	variable Priv

	set treeO [Info $oop option,tree]

	switch -- $message {
		preDisplay {
			qeconfigure NSOptions <Setting> -active yes

			CheckOptionsIndex
if 1 {
			set treeG [Info $oop group,tree]
			NSList::Clear $treeG
			foreach page $Priv(page) {
				set label [mc $page]
				set item [$treeG item create]
				NSList::SetText $treeG $item $label
				$treeG item lastchild root $item
			}
			set maxWidth [CalcGroupListWidth $oop]
} else {
			set font [Value font,options]
			set treeG [Info $oop group,tree]
			NSList::Clear $treeG
			set maxWidth 84
			foreach page $Priv(page) {
				set label [mc $page]
				set width [font measure $font $label]
				if {$width > $maxWidth} {
					set maxWidth $width
				}
				set item [$treeG item create]
				NSList::SetText $treeG $item $label
				$treeG item lastchild root $item
			}
			incr maxWidth 16
}
			$treeG configure -width $maxWidth
			update idletasks

			SetOptions $oop [Info $oop page]

			# Hack -- Restore the selection
			set current [Info $oop option,current2]
			if {$current != -1} {
				$treeO selection add "root child $current"
				$treeO see "root child $current"
			}
		}
		postDisplay {
		}
		postWithdraw {
			qeconfigure NSOptions <Setting> -active no
		}
	}

	return
}

# NSOptions::Close --
#
#	Called by WM_DELETE_WINDOW protocol handler.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	Instead of destroying the window, just withdraw (hide) it.

proc NSOptions::Close {oop} {

	Info $oop option,current2 [Info $oop option,current]
	angband keypress \033

	return
}

# NSOptions::StatusBar --
#
#	Display text in the status bar, perhaps clearing it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label [$win.statusBar itemcget t1 -label]
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

# NSOptions::Toggle --
#
#	Toggles the value of an option.
#
# Arguments:
#	oop						OOP ID. See above.
#	tagOrId					canvas id for checkbox image.
#
# Results:
#	What happened.

proc NSOptions::Toggle {oop tree item row} {

	set option [lindex [Info $oop settings] $row]
	set setting [Setting $option]

	# Hack -- If this is the first-ever setting of any cheating
	# option, including wizard mode or debug commands, then remind
	# the user about losing the high score.
	if {[string match cheat_* $option]} {
		angband player cheat cheat
		foreach {name boolean} [array get cheat] {
			if {$boolean} break
		}
		if {!$boolean} {
			set answer [tk_messageBox -icon warning -title "Cheat Warning" \
				-parent [Info $oop win] -message "Your character has never\
				cheated before!\n\nCheating means your character will not\
				get a\npermanent entry in the High Score list!\n\nAre you sure\
				you want to set this cheating option?" -type yesno]
			if {$answer eq "no"} return
		}
	}

	Setting $option [expr {$setting ? 0 : 1}]

	return
}

# NSOptions::SetOptions --
#
#	Display options on given "page". Usually this is list of
#	on/off options handled by our NSList. But sometimes
#	the page does not use a list, instead using slider controls.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::SetOptions {oop page} {

	global NSOptions
	variable Priv

	set win [Info $oop win]
	set tree [Info $oop option,tree]

	foreach widget [Info $oop tempWidget] {
		destroy $widget
	}
	Info $oop tempWidget {}

	grid forget {*}[winfo children $win.frame]
	grid $tree \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.frame.yscroll \
		-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky ns

	switch -- $page {

		Command -
		Dungeon -
		Monster -
		Object -
		Prompt -
		Interface -
		Disturbance -
		Efficiency -
		Misc -
		Cheating {
			NSList::Config $tree -columns 1 -checkbutton yes
			SetList $oop $page
		}

		Other {
			NSList::Config $tree -columns 2 -checkbutton no
			$tree column configure "order 1" -justify right
			grid $tree -rowspan 1
			grid $win.frame.yscroll -rowspan 1

			set data [list \
				delay_factor 0 9 1 \
				hitpoint_warn 0 9 1 \
			]
			if {[variant OANGBANDTK ZANGBANDTK]} {
				lappend data \
					autosave_freq 0 25000 50
			}
			lappend data \
				ambient_delay 0 500 5 \
				monster_delay 0 500 5 \
				mouse_repeat_delay 0 1000 5 \
				mouse_repeat_interval 0 1000 5 \
				mouse_repeat_town 0 1000 5 \
				autobar_button_size 18 32 2

			NSList::Clear $tree

			set settings {}
			set scaleInfo {}
			foreach {setting from to resolution} $data {
				set value [Setting $setting]
				set label [SettingDesc $setting]

				set item [$tree item create]
				NSList::SetText $tree $item $label
				$tree item text $item "order 1" $value
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
				-width 5 -state disabled -textvariable NSOptions($oop,scale,value)
			bind $frame.entry <FocusIn> {%W selection range 0 end}
			bind $frame.entry <KeyPress-Escape> "NSOptions::Close $oop"
			bindtags $frame.entry [list $frame.entry Entry all]
			$frame.scale set 50
			pack $frame.scale -side left
			pack $frame.entry -side left
			grid $frame -row 1 -column 0 -rowspan 1 -columnspan 2
			lappend NSOptions($oop,tempWidget) $frame
		}
	}

	if {$::JAPANESE} {
		switch -- $page {
			Japanese {
				NSList::Config $tree -columns 1 -checkbutton yes
				SetList $oop $page
			}
		}
	}

	wm title $win [format [mc "Options (%s)"] [mc $page]]

	Info $oop page $page
	Info $oop radio $page

	# Select group
	set treeG [Info $oop group,tree]
	set row [lsearch -exact $Priv(page) $page]
	$treeG selection modify [list "root child $row"] all

	return
}

# NSOptions::SetList --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::SetList {oop page} {

	set tree [Info $oop option,tree]

	NSList::Clear $tree

	set settings {}
	foreach keyword [GetPage $page] {
		set setting [Setting $keyword]
		set desc [SettingDesc $keyword]
		set color White

		set item [$tree item create]
		NSList::SetText $tree $item $desc
		if {$setting} {
			$tree item state set $item checked
		}
		$tree item lastchild root $item

		lappend settings $keyword
	}

	Info $oop settings $settings

	return
}

# NSOptions::GetPage --
#
#	Return a list of option-variable keywords.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::GetPage {page} {

	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		switch -- $page {

			Command {
				lappend optionList \
					rogue_like_commands \
					use_old_target \
					expand_look \
					expand_list \
					always_repeat
				if {[variant ANGBANDTK KANGBANDTK]} {
					lappend optionList \
						easy_floor
				}
				lappend optionList \
					easy_open \
					easy_alter
				return $optionList
			}

			Dungeon {
				lappend optionList \
					auto_scum \
					view_perma_grids \
					view_torch_grids
				if {[variant ANGBANDTK KANGBANDTK]} {
					lappend optionList \
						dungeon_align \
				}
				lappend optionList \
					dungeon_stair \
					remember_recall
				return $optionList
			}

			Monster {
				if {[variant ANGBANDTK]} {
					lappend optionList \
						flow_by_sound \
						flow_by_smell \
						smart_learn \
						smart_cheat \
						smart_monsters \
						smart_packs
				}
				if {[variant KANGBANDTK]} {
					lappend optionList \
						fast_flow
				}
				if {[variant OANGBANDTK]} {
					lappend optionList \
						smart_cheat \
						fast_flow
				}
				return $optionList
			}

			Object {
				lappend optionList \
					always_pickup \
					pickup_similar \
					stack_force_notes \
					stack_force_costs \
					easy_sense
				return $optionList
			}

			Prompt {
				lappend optionList \
					carry_query_flag \
					floor_query_flag
				if {[variant ANGBANDTK KANGBANDTK]} {
					lappend optionList \
						auto_more
				}
				lappend optionList \
					quick_messages \
					ring_bell \
					alert_hitpoint \
					alert_failure \
					verify_destroy \
					verify_destroy_junk \
					verify_special \
					verify_mana
				if {[variant ANGBANDTK KANGBANDTK]} {
					lappend optionList \
						allow_quantity
				}
				if {[variant ANGBANDTK]} {
					lappend optionList \
						easy_ring
				}
				return $optionList
			}

			Interface {
				lappend optionList \
					depth_in_feet \
					show_details \
					show_flavors \
					show_labels \
					show_weights \
					show_icons \
					scroll_follow \
					allow_animation \
					show_cave_balloon
				if {[variant OANGBANDTK]} {
					lappend optionList \
						use_metric
				}
				return $optionList
			}

			Disturbance {
				lappend optionList \
					run_ignore_stairs \
					run_ignore_doors \
					run_cut_corners \
					run_use_corners \
					disturb_move \
					disturb_near \
					disturb_panel \
					disturb_state \
					disturb_minor \
					disturb_other
				if {[variant OANGBANDTK]} {
					lappend optionList \
						show_detect \
						disturb_trap_detect
				}
				return $optionList
			}

			Efficiency {
				return {
					view_reduce_lite
					avoid_abort
					flush_failure
					flush_disturb
					fresh_before
				}
			}

			Cheating {
				return {
					cheat_peek
					cheat_hear
					cheat_room
					cheat_xtra
					cheat_know
					cheat_live
				}
			}

			Other {
				lappend optionList \
					delay_factor \
					hitpoint_warn
				if {[variant OANGBANDTK]} {
					lappend optionList \
						autosave_freq
				}
				lappend optionList \
					ambient_delay \
					monster_delay \
					mouse_repeat_delay \
					mouse_repeat_interval \
					mouse_repeat_town \
					autobar_button_size
				return $optionList
			}

			Misc {
				lappend optionList \
					auto_haggle \
					compress_savefile
				if {[variant OANGBANDTK]} {
					lappend optionList \
						autosave
				}
				return $optionList
			}
		}

	# ANGBANDTK, KANGBANDTK, OANGBANDTK
	}

	if {[variant ZANGBANDTK]} {
		switch -- $page {

			Command {
				lappend optionList \
					rogue_like_commands \
					use_old_target \
					expand_look \
					expand_list \
					always_repeat \
					easy_floor \
					easy_open \
					easy_alter \
					use_command
				return $optionList
			}

			Dungeon {
				lappend optionList \
					auto_scum \
					view_perma_grids \
					view_torch_grids \
					dungeon_align \
					dungeon_stair \
					small_levels \
					always_small_levels \
					empty_levels \
					pillar_tunnels \
					remember_recall
				return $optionList
			}

			Monster {
				lappend optionList \
					testing_carry \
					flow_by_sound \
					flow_by_smell \
					smart_learn \
					smart_cheat \
					stupid_monsters
				return $optionList
			}

			Object {
				lappend optionList \
					always_pickup \
					pickup_similar \
					stack_force_notes \
					stack_force_costs \
					stack_allow_items \
					testing_stack \
					easy_sense
				return $optionList
			}

			Prompt {
				lappend optionList \
					auto_destroy \
					carry_query_flag \
					confirm_wear \
					confirm_stairs \
					other_query_flag \
					quick_messages \
					ring_bell \
					alert_hitpoint \
					alert_failure \
					verify_mana
				return $optionList
			}

			Interface {
				return {
					depth_in_feet
					show_details
					plain_descriptions
					show_labels
					show_weights
					show_icons
					scroll_follow
					allow_animation
					show_cave_balloon
				}
			}

			Disturbance {
				return {
					find_ignore_stairs
					find_ignore_doors
					find_cut
					find_examine
					disturb_move
					disturb_near
					disturb_panel
					disturb_state
					disturb_minor
					disturb_other
					disturb_pets
				}
			}

			Efficiency {
				return {
					view_reduce_lite
					view_reduce_view
					avoid_abort
					flush_failure
					flush_disturb
					fresh_before
				}
			}

			Cheating {
				return {
					cheat_peek
					cheat_hear
					cheat_room
					cheat_xtra
					cheat_know
					cheat_live
				}
			}

			Other {
				return {
					delay_factor
					hitpoint_warn
					autosave_freq
					ambient_delay
					monster_delay
					mouse_repeat_delay
					mouse_repeat_interval
					mouse_repeat_town
					autobar_button_size
				}
			}

			Misc {
				return {
					auto_haggle
					compress_savefile
					last_words
					speak_unique
					autosave_l
					autosave_t
					take_notes
					auto_notes
				}
			}
		}
		if {$::JAPANESE} {
			switch -- $page {
				Japanese {
					return {
						always_show_list
						english_object
						english_monster
						powerup_home
						english_others
						old_way_of_kaz
						plain_savefile
						use_lb
					}
				}
			}
		}

	# ZANGBANDTK
	}

	return
}

# NSOptions::SelectionChanged_Group --
#
#	Called when the group selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::SelectionChanged_Group {oop tree count select deselect} {

	variable Priv

	if {![llength $select]} {
		Info $oop group,current -1
		return
	}

	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]
	Info $oop group,current $row

	set page [lindex $Priv(page) $row]
	SetOptions $oop $page

	return
}

# NSOptions::SelectionChanged --
#
#	Called when the list selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::SelectionChanged {oop tree count select deselect} {

	set win [Info $oop win]

	Info $oop ignoreChange 1

$win.frameHelp.text delete 1.0 end

	if {[llength $select]} {
		set item [lindex $select 0]
		set row [NSList::Item2Row $tree $item]
		Info $oop option,current $row

		set keyword [lindex [Info $oop settings] $row] 
		$win.statusBar itemconfigure t2 -text $keyword

DisplaySettingHelp $oop $keyword

		if {[winfo exists $win.frame.scale]} {
			$win.frame.scale.entry configure -state normal
			set scaleInfo [lindex [Info $oop scaleInfo] $row]
			$win.frame.scale.scale configure \
				-from [lindex $scaleInfo 0] -to [lindex $scaleInfo 1] \
				-resolution [lindex $scaleInfo 2] \
				-label [SettingDesc $keyword] -state normal \
				-command "NSOptions::ScaleCmd $oop $keyword"
			Info $oop scale,value [Setting $keyword]
			$win.frame.scale.scale set [Setting $keyword]
		}

	} else {
		Info $oop option,current -1

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

# NSOptions::ScaleCmd --
#
#	Called when the value of a scale changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::ScaleCmd {oop keyword value} {

	if {[Info $oop ignoreChange]} return

	# Update the game setting
	Setting $keyword $value
if 0 {
	# Update the list
	set row [Info $oop option,current]
	set keyword [lindex [Info $oop settings] $row]
	set tree [Info $oop option,tree]
	$tree item text "root child $row" "order 1" $value

	# Update the entry
	Info $oop scale,value $value
}

	return
}

# NSStore::EntryTextVarProc --
#
#	Trace variable callback on NSOptions($oop,scale,value).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::EntryTextVarProc {oop name1 name2 op} {

	if {[Info $oop ignoreChange]} return

	set value [Info $oop scale,value]
	set scaleInfo [lindex [Info $oop scaleInfo] [Info $oop option,current]]
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

# NSOptions::DisplaySettingHelp --
#
#	Display the help text for the given setting keyword.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::DisplaySettingHelp {oop keyword} {

	set text [Info $oop win].frameHelp.text

	set desc [SettingDesc $keyword]
	set help [GetSettingHelp $oop $keyword]

	$text insert end $desc\n
	$text insert end $help tag1
	$text tag configure tag1 \
		-lmargin1 19 -lmargin2 19 -rmargin 0

	ForceTextWidgetViewUpdate $text

	return
}

# NSOptions::GetSettingHelp --
#
#	Scan the options.txt file for the description of a setting.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::GetSettingHelp {oop keyword} {

	global Angband
	variable Priv

	if {[info exists Priv(optionsIndex,$keyword)]} {
		set offset $Priv(optionsIndex,$keyword)
	} else {
		return ""
	}

	# Open the tk/doc/options.txt file. Read and discard lines
	# up to and including the line determined above. Then read until
	# a blank line (or end-of-file).

	if {[catch {open [PathTk doc options.txt]} fileId]} {
		return ""
	}
	catch {
		set result ""
		seek $fileId $offset
		while 1 {
			set count [gets $fileId lineBuf]
			if {$count <= 0} break
			append result "[string trim $lineBuf] "
		}
	}
	close $fileId

	return $result
}

# NSOptions::GenOptionsIndex --
#
#	Reads the tk/doc/options.txt file, and for each keyword writes a
#	line to tk/doc/options-index. Each line consists of two elements:
#	the setting keyword and the offset to it.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::GenOptionsIndex {} {

	if {[catch {open [PathTk doc options.txt]} readId]} {
		return
	}
	if {[catch {openlf [PathTk doc options-index]} writeId]} {
		close $readId
		return
	}

	catch {
		set saw_blank 1
		while 1 {
			set count [gets $readId lineBuf]
			if {$count < 0} break
			set offset [tell $readId]
			if {$count == 0} {
				set saw_blank 1
			} else {
				if {$saw_blank} {
					set lineBuf [string trim $lineBuf]
					puts $writeId "$lineBuf $offset"
				}
				set saw_blank 0
			}
		}
	}

	close $writeId
	close $readId

	return
}

# NSOptions::ReadOptionsIndex --
#
#	Reads the tk/doc/options-index file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::ReadOptionsIndex {} {

	global Angband
	variable Priv

	set Priv(optionsIndex) {}

	if {[catch {open [PathTk doc options-index]} fileId]} {
		return
	}
	catch {
		set lineNo 0
		while 1 {
			set count [gets $fileId lineBuf]
			if {$count <= 0} break
			scan $lineBuf "%s %d" keyword offset
			set Priv(optionsIndex,$keyword) $offset
		}
	}
	close $fileId

	return
}

# NSOptions::CheckOptionsIndex --
#
#	Generate and read tk/doc/options-index file, if needed.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::CheckOptionsIndex {} {

	set file1 [PathTk doc options.txt]
	set file2 [PathTk doc options-index]
	if {![file exists $file2]} {
		GenOptionsIndex
	} else {
		set mtime1 [file mtime $file1]
		set mtime2 [file mtime $file2]
		if {$mtime1 > $mtime2} {
			GenOptionsIndex
		}
	}

	ReadOptionsIndex

	return
}

# NSOptions::WriteSettings --
#
#	Save the current settings to the tk/config/setting file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::WriteSettings {oop} {

	global Angband
	variable Priv

	set answer [tk_messageBox -title [mc title-write] -icon question \
		-parent [Info $oop win] -type yesno -message [mc msg-write]]
	if {$answer eq "no"} return

	set tempName [NSUtils::TempFileName [PathTk config]]
	if {[catch {openlf $tempName} fileId]} {
		set msg "The following error occurred while attempting to open "
		append msg "the \"setting\" file for writing:\n\n$fileId"
		tk_messageBox -title Oops -parent [Info $oop win] -message $msg
		return
	}

	StatusBar $oop "Writing settings..." 0

	foreach page $Priv(page) {
		if {$page eq "Cheating"} continue
		foreach keyword [GetPage $page] {
			lappend data $keyword
		}
	}

	puts $fileId "# Automatically generated. Do not edit.\n"

	foreach keyword [lsort $data] {
		set value [Setting $keyword]
		puts $fileId "One $keyword \"$value\""
	}

	close $fileId

	set fileName [NSUtils::ReadLink [PathTk config setting]]
	if {[file exists $fileName]} {
		file rename -force -- $fileName $fileName.bak
	}
	file rename -- $tempName $fileName

	StatusBar $oop "Done." 1

	return
}

# NSOptions::ReadSettings --
#
#	Override current settings with those in the tk/config/setting file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::ReadSettings {oop} {

	global Angband

	set fileName [PathTk config setting]
	if {![file exists $fileName]} {
		tk_messageBox -title [mc title-read] -icon info -type ok \
			-parent [Info $oop win] \
			-message "There are no default settings to restore."
		return
	}

	set answer [tk_messageBox -title [mc title-read] -icon question \
		-type yesno -parent [Info $oop win] \
		-message [mc msg-read]]
	if {$answer eq "no"} return

	Config::Setting::Source $fileName

	SetOptions $oop [Info $oop page]

	StatusBar $oop "Done." 1

	return
}

# SettingChanged --
#
#	Called when a <Setting> event is generated.
#	Updates the Options Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::SettingChanged {oop keyword value} {

	set page [Info $oop page]
	set keywordList [GetPage $page]
	if {[lsearch -exact $keywordList $keyword] == -1} return

	set tree [Info $oop option,tree]

	switch -- $page {

		Command -
		Dungeon -
		Monster -
		Object -
		Prompt -
		Interface -
		Disturbance -
		Efficiency -
		Misc -
		Cheating {
			set index [lsearch -exact [Info $oop settings] $keyword]
			if {$value} {
				set state checked
			} else {
				set state !checked
			}
			$tree item state set "root child $index" $state
		}

		Other {
			set index [lsearch -exact [Info $oop settings] $keyword]
			$tree item text "root child $index" "order 1" $value

			if {[Info $oop option,current] == -1} return

			# Update the entry and scale
			Info $oop scale,value $value
		}
	}

	return
}

# NSOptions::CalcGroupListWidth --
#
#	Returns required width of the group list
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::CalcGroupListWidth {oop} {

	variable Priv

	set font [Value font,options]

	set maxWidth 84
	foreach page $Priv(page) {
		set label [mc $page]
		set width [font measure $font $label]
		if {$width > $maxWidth} {
			set maxWidth $width
		}
	}
	incr maxWidth 16

	return $maxWidth
}

# NSOptions::ValueChanged_font_options --
#
#	Called when the font,options value changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::ValueChanged_font_options {oop} {

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

	# Set the help-text font
	set text [Info $oop win].frameHelp.text
	$text configure -font [Value font,options]

	return
}

# NSOptions::Find --
#
#	Simple search routine to look for an option by name.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSOptions::Find {oop again} {

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
		if {[Info $oop option,current] != -1} {

			# Search after the selected option
			set member [expr {[Info $oop option,current] + 1}]
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
				set tree [Info $oop option,tree]

				# Select and the option
				NSList::Activate $tree "root child $member"

				# View the option
				focus $tree

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

