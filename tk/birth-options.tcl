# File: birth-options.tcl

# Purpose: the Birth Options Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSBirthOptions {

	variable MenuString
	variable Priv

# namespace eval NSBirthOptions
}

# NSBirthOptions::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::InitModule {} {

	variable Priv

	MsgCatInit option

	NSModule::AddModuleIfNeeded NSList [CPathTk stdlist.tcl]
	NSModule::LoadIfNeeded NSList

	NSModule::LoadIfNeeded NSStatusBar

	# If the tk/doc/options-index file does not exist, then create it.
	# Otherwise, recreate the file if it is older than tk/doc/options.txt.
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

	set Priv(page) {}
	lappend Priv(page) Birth
	if {[variant ZANGBANDTK]} {
		lappend Priv(page) Ironman
		lappend Priv(page) Quests
		lappend Priv(page) Wilderness
	}

	set Priv(find,string) ""
	set Priv(find,fromStart) 1

	# Hack -- Changing the 'maximize' option affects the maximum stats
	# the user can enter on the auto-roller screen. If we change the
	# 'maximize' option and the auto-roller screen is displayed, we
	# will redisplay the screen in CloseModule().
	set Priv(maximize) [BirthObj Info maximize]

	# Create the Birth Options Window
	NSObject::New NSBirthOptions

	# Hack -- Verify we don't have any bad options
	if {$::DEBUG} {
		foreach page $Priv(page) {
			foreach option [GetPage $page] {
				BirthSetting $option
			}
		}
	}

	return
}

# NSBirthOptions::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::CloseModule {} {

	variable Priv

	set oopBirth $NSBirth::Priv(oop)
	set winBirth [BirthObj Info win]

	# Hack -- Changing the 'maximize' option affects the maximum stats
	# the user can enter on the auto-roller screen. If we change the
	# 'maximize' option and the auto-roller screen is displayed, we
	# will redisplay the screen
	if {$Priv(maximize) != [BirthObj Info maximize]} {
		if {[BirthObj Info screen] eq "AutoRoll"} {
#			eval destroy [winfo children $winBirth.content]
			NSBirth::InitScreen_AutoRoll $oopBirth
		}
	}

	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		BirthObj Info autoroll [angband setting set birth_auto_roller]
		BirthObj Info pointbased [angband setting set birth_point_based]
	}
	if {[variant ZANGBANDTK]} {
		BirthObj Info autoroll [angband setting set autoroller]
		BirthObj Info pointbased [angband setting set point_based]
	}

	WindowBringToFront $winBirth

	catch {
		wm withdraw [Info $Priv(oop) win]
		destroy [Info $Priv(oop) win]
	}

	return
}

# NSBirthOptions::NSBirthOptions --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::NSBirthOptions {oop} {

	variable Priv

	set Priv(oop) $oop

	Info $oop ignoreChange 0
	Info $oop current -1
	Info $oop current2 -1

	trace variable NSOptions($oop,scale,value) w \
		"NSBirthOptions::EntryTextVarProc $oop"

	InitWindow $oop

	set win [Info $oop win]

	# Set the initial page
	Info $oop page [lindex $Priv(page) 0]

	Info $oop tempWidget {}

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSBirthOptions $oop $win

	# Prep the window
	NSToplevel::NaturalSize $win ""

	# Display the options
	SetOptions $oop [Info $oop page]

#	update

	# Display the Birth Options Window
	wm geometry $win [winfo reqwidth $win]x[winfo reqheight $win]
	WindowPosition $win 2 3
	WindowBringToFront $win

	# Hide the Birth Window
	wm withdraw [BirthObj Info win]

	return
}

# NSBirthOptions::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::Info {oop info args} {

	global NSBirthOptions

	# Verify the object
	NSObject::CheckObject NSBirthOptions $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSBirthOptions($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSBirthOptions($oop,$info)
			}
		}
	}

	return
}

# NSBirthOptions::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::InitWindow {oop} {

	variable Priv

	set win .options$oop
	toplevel $win
	wm title $win [mc Options]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSBirthOptions::Close $oop"

	Info $oop win $win

	InitMenus $oop

	MakeDivider $win.divider1 x

	# Tabs!
	set tabsId [NSObject::New NSTabs $win]
	foreach page $Priv(page) {
		NSTabs::Add $tabsId [mc $page]
	}
	NSTabs::Info $tabsId invokeCmd "NSBirthOptions::InvokeTab $oop"
	NSTabs::Info $tabsId validateCmd "NSBirthOptions::Validate $oop"
	NSTabs::Info $tabsId active 1
	Info $oop tabsId $tabsId

	#
	# Options list
	#

	set frame $win.frame
	set tree [NSList::New $frame -checkbutton yes]
	$tree configure -width 400 -height 200

	NSList::OnSelection $tree \
		"NSBirthOptions::SelectionChanged $oop %T %c %S %D"

	NSList::OnToggle $tree \
		"NSBirthOptions::Toggle $oop %T %I %r"

	Info $oop tree $tree

	MakeDivider $win.divider2 x

	#
	# Help + divider
	#

	frame $win.frameHelp \
		-borderwidth 1 -relief sunken
	text $win.frameHelp.text \
		-width 40 -height 5 -background [Value listBG] \
		-foreground White -cursor {} -wrap word \
		-font [Value font,options] -takefocus 0 \
		-yscrollcommand "$win.frameHelp.yscroll set" \
		-borderwidth 0
	scrollbar $win.frameHelp.yscroll \
		-orient vertical -command "$win.frameHelp.text yview"
	bindtags $win.frameHelp.text [list $win.frameHelp.text $win all]

	#
	# Statusbar
	#

#	MakeStatusBar $win.statusBar 20 ; # Not defined!
	statusbar $win.statusBar -sizes {0 20} -weights {1 0} -tags {t1 t2}

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
	grid rowconfigure $win 2 -weight 1 -minsize 0
	grid rowconfigure $win 3 -weight 0 -minsize 0
	grid rowconfigure $win 4 -weight 0 -minsize 0
	grid rowconfigure $win 5 -weight 0 -minsize 0
	grid columnconfigure $win 0 -weight 1 -minsize 0

	grid $win.divider1 \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid [NSTabs::Info $tabsId canvas] \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.frame \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.divider2 \
		-row 3 -column 0 -rowspan 1 -columnspan 2 -sticky ew -pady 2
	grid $win.frameHelp \
		-row 4 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.statusBar \
		-row 5 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	bind $win <KeyPress-Escape> "NSBirthOptions::Close $oop"
	bind $win <Control-KeyPress-w> "NSBirthOptions::Close $oop"
	bind $win <KeyPress-f> "NSBirthOptions::Find $oop 0"
	bind $win <KeyPress-g> "NSBirthOptions::Find $oop 1"

	return
}

# NSBirthOptions::InitMenus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::InitMenus {oop} {

	variable MenuString
	variable Priv

	set win [Info $oop win]
	set mod "Ctrl"

	#
	# Menu bar
	#

	set mbar [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSBirthOptions::SetupMenus $oop" -identifier MENUBAR]
	Info $oop mbarId $mbar

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSBirthOptions::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSBirthOptions::MenuInvoke $oop"

	#
	# Options Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_OPTIONS
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_OPTIONS -label [mc Options] -underline 0 \
		-identifier M_OPTIONS

	set entries {}
	set i 1
	foreach page $Priv(page) {
		lappend entries [list -type radiobutton -label [mc $page] \
			-command "NSBirthOptions::SetOptions $oop $page" \
			-variable NSBirthOptions($oop,radio) -value $page \
			-accelerator $i -identifier E_PAGE_$i]
		bind $win <KeyPress-$i> "NSBirthOptions::SetOptions $oop $page"
		incr i
	}
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Find...] \
		-command "NSBirthOptions::Find $oop 0" -underline 0 \
		-accelerator f -identifier E_FIND]
	lappend entries [list -type command -label [mc "Find Again"] \
		-command "NSBirthOptions::Find $oop 1" -underline 6 \
		-accelerator g -identifier E_FIND_AGAIN]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-command "NSBirthOptions::Close $oop" -underline 0 \
		-accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_OPTIONS $entries

	set MenuString(M_OPTIONS) \
		"Contains commands for displaying and searching options."
	set MenuString(E_FIND) \
		"Searches for an option by keyword."
	set MenuString(E_FIND_AGAIN) \
		"Repeats the previous search."
	set MenuString(E_CLOSE) \
		"Closes the window."

	return
}

# NSBirthOptions::SetupMenus --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::SetupMenus {oop mbarID} {

	variable Priv

	set i 0
	foreach page $Priv(page) {
		lappend identList E_PAGE_[incr i]
	}

	lappend identList E_FIND E_FIND_AGAIN E_CLOSE

	NSMenu::MenuEnable $mbarID $identList

	return
}

# NSBirthOptions::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::MenuSelect {oop menuId index ident} {

	variable MenuString

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

# NSBirthOptions::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::MenuInvoke {oop menuId ident} {

	switch -glob -- $ident {
		E_CLOSE {Close $oop}
	}

	return
}

# NSBirthOptions::Close --
#
#	Called when closing the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::Close {oop} {

	if {[Validate $oop]} {
		return
	}
	NSModule::CloseModule NSBirthOptions

	return
}

# NSBirthOptions::StatusBar --
#
#	Display text in the status bar, perhaps clearing it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label [$win.statusBar itemcget t1 -label]
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

# NSBirthOptions::Toggle --
#
#	Toggles the value of an option when a checkbox is clicked.
#
# Arguments:
#	oop					OOP ID. See above.
#	tagOrId				canvas id for checkbox image.
#
# Results:
#	What happened.

proc NSBirthOptions::Toggle {oop tree item row} {

	set option [lindex [Info $oop settings] $row]
	set setting [BirthSetting $option]

	if {$setting} {
		BirthSetting $option 0
	} else {
		BirthSetting $option 1
	}

	$tree item state set $item ~checked

	return
}

# NSBirthOptions::Synch --
#
#	Turns a checkbox on or off to match an option.
#
# Arguments:
#	oop					OOP ID. See above.
#	tagOrId				canvas id for checkbox image.
#
# Results:
#	What happened.

proc NSBirthOptions::Synch {oop keyword} {

	set tree [Info $oop tree]

	set row [lsearch -exact [Info $oop settings] $keyword]
	if {$row == -1} return

	set item "root child $row"
	if {[BirthSetting $keyword]} {
		$tree item state set $item checked
	} else {
		$tree item state set $item !checked
	}

	return
}

# NSBirthOptions::SetOptions --
#
#	Display options on given "page". Usually this is list of
#	on/off options handled by our NSList list. But sometimes
#	the page does not use a list, instead using slider controls.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::SetOptions {oop page} {

	global NSBirthOptions
	variable Priv

	if {[Validate $oop]} {
		Info $oop radio [Info $oop page]
		return
	}

	set win [Info $oop win]
	set tree [Info $oop tree]

	foreach widget [Info $oop tempWidget] {
		destroy $widget
	}
	Info $oop tempWidget {}

	grid forget {*}[winfo children $win.frame]

	# Clear the keyword display
	$win.statusBar itemconfigure t2 -text ""

	# Clear the help text
	set text $win.frameHelp.text
	$text delete 1.0 end

	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		switch -- $page {

			Birth {
				grid $tree \
					-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
				grid $win.frame.yscroll \
					-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky ns
				SetList $oop $page
			}
		}
	# ANGBANDTK, KANGBANDTK, OANGBANDTK
	}
	if {[variant ZANGBANDTK]} {
		switch -- $page {

			Birth -
			Ironman {
				grid $tree \
					-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
				grid $win.frame.yscroll \
					-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky ns
				SetList $oop $page
			}

			Quests {
				set message $win.frame.info
				message $message \
					-width 325 -text [mc quest-prompt]
				pack $message -side top -anchor w -padx 10 -pady 10
				lappend NSBirthOptions($oop,tempWidget) $message

				set oopBirth $NSBirth::Priv(oop)
if 0 {
				if {[Platform unix]} {
					set font {Courier 12}
				}
				if {[Platform windows]} {
					set font {Courier 9}
				}
}
				set font [Value font,fixed]

				set frame $win.frame.quest
				frame $frame \
					-borderwidth 0
				entry $frame.num \
					-width 4 -font $font -textvariable ::NSBirth($oopBirth,max_quest)
				label $frame.max \
					-text [format [mc "(Max of %d)"] [birth info max_quest]] \
					-font $font

				bindtags $frame.num [list $frame.num Entry all]
				bind $frame.num <KeyPress> {
					if {[string match \[a-zA-Z\] "%A"]} {
						break
					}
				}
				bind $frame.num <KeyPress-Escape> "NSBirthOptions::Close $oop"
				bind $frame.num <Control-KeyPress-w> "NSBirthOptions::Close $oop"

				pack $frame \
					-side top -anchor w -padx 50
				pack $frame.num \
					-side left -expand no
				pack $frame.max \
					-side left -expand no

				lappend NSBirthOptions($oop,tempWidget) $frame

				$frame.num icursor end
				focus $frame.num	
			}

			Wilderness {
				set message $win.frame.info
				message $message \
					-width 325 -text [mc wild-prompt]
				pack $message -side top -anchor w -padx 10 -pady 10
				lappend NSBirthOptions($oop,tempWidget) $message

				set frame $win.frame.radio
				frame $frame \
					-borderwidth 0
				pack $frame -anchor w -padx {30 0}

				# XXX Hack -- Save 'wilderness_mode'
				set wilderness_mode [BirthSetting wilderness_mode]

				set settings {}
				foreach mode {normal lite none} {

					# XXX Hack -- The "setting" command description changes
					# depending on the value of the option
					BirthSetting wilderness_mode $mode

					set desc [BirthSettingDesc wilderness_mode]
					set radio $frame.radio$mode
					radiobutton $radio \
						-text $desc -variable NSBirthOptions($oop,optionRadio) \
						-value $mode -command "NSBirthOptions::RadiobuttonCmd $oop wilderness_mode,$mode $mode"
					pack $radio -side top -anchor w
					lappend settings wilderness_mode,$mode
				}

				# XXX Hack -- Restore 'wilderness_mode'
				BirthSetting wilderness_mode $wilderness_mode

				Info $oop settings $settings

				# Select the proper radiobutton (and display help text)
				Info $oop optionRadio $wilderness_mode
				DisplaySettingHelp $oop wilderness_mode,$wilderness_mode

				lappend NSBirthOptions($oop,tempWidget) $frame
			}
		}
	# ZANGBANDTK
	}

	wm title $win [format [mc "Options (%s)"] [mc $page]]

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

# NSBirthOptions::SetList --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::SetList {oop page} {

	set tree [Info $oop tree]

	NSList::Clear $tree

	set settings {}
	foreach keyword [GetPage $page] {
		set setting [BirthSetting $keyword]
		set desc [BirthSettingDesc $keyword]

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

# NSBirthOptions::GetPage --
#
#	Return a list of option-variable keywords.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::GetPage {page} {

	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		switch -- $page {

			Birth {
				lappend optionList \
					birth_point_based \
					birth_auto_roller \
					birth_maximize \
					birth_preserve
				if {[variant ANGBANDTK KANGBANDTK]} {
					lappend optionList \
						birth_ironman \
						birth_no_stores \
						birth_no_artifacts \
						birth_rand_artifacts
				}
				if {[variant KANGBANDTK]} {
					lappend optionList \
						birth_smart_cheat \
				}
				return $optionList
			}
		}
	}
	if {[variant ZANGBANDTK]} {
		switch -- $page {

			Birth {
				return {
					point_based
					autoroller
					maximize_mode
					preserve_mode
					terrain_streams
					munchkin_death
				}
			}

			Ironman {
				return {
					ironman_shops
					ironman_small_levels
					ironman_downward
					ironman_autoscum
					ironman_hard_quests
					ironman_empty_levels
					ironman_rooms
					ironman_nightmare
				}
			}

			Quests {
				return {
					max_quest
				}
			}

			Wilderness {
				return {
					lite_town
					vanilla_town
				}
			}
		}
	# ZANGBANDTK
	}

	return
}

# NSBirthOptions::SelectionChanged --
#
#	Called when the list selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::SelectionChanged {oop tree count select deselect} {

	set win [Info $oop win]

	Info $oop ignoreChange 1

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
				-label [BirthSettingDesc $keyword] -state normal \
				-command "NSBirthOptions::ScaleCmd $oop $keyword"
			Info $oop scale,value [BirthSetting $keyword]
			$win.frame.scale.scale set [BirthSetting $keyword]
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

		$win.frameHelp.text delete 1.0 end
	}

	# Must update now, because scale's command is called at
	# idle time!
	update idletasks

	Info $oop ignoreChange 0

	return
}

# NSBirthOptions::InvokeTab --
#
#	Called when a tab is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::InvokeTab {oop tabsId tabId} {

	variable Priv

	set index [lsearch -exact [NSTabs::Info $tabsId id] $tabId]
	SetOptions $oop [lindex $Priv(page) $index]

	return
}

# NSBirthOptions::ScaleCmd --
#
#	Called when the value of a scale changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::ScaleCmd {oop keyword value} {

	if {[Info $oop ignoreChange]} return

	# Update the game setting
	BirthSetting $keyword $value

	# Update the list
	UpdateList $oop

	# Update the entry
	Info $oop scale,value $value

	return
}

# NSStore::EntryTextVarProc --
#
#	Trace variable callback on NSBirthOptions($oop,scale,value).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::EntryTextVarProc {oop name1 name2 op} {

	if {[Info $oop ignoreChange]} return

	set value [Info $oop scale,value]
	set scaleInfo [lindex [Info $oop scaleInfo] [Info $oop current]]
	set from [lindex $scaleInfo 0]
	set to [lindex $scaleInfo 1]

	regsub -all {[^0-9]} $value "" value
	if {[string length $value]} {
		if {$value < $from} {set value $from}
		if {$value > $to} {set value $to}
		[Info $oop win].frame.scale.scale set $value
	}

	Info $oop scale,value $value

	return
}

# NSBirthOptions::UpdateList --
#
#	Configure the Widget canvas item on the row of the list.
#	When the scale value changes, we want to update the list.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::UpdateList {oop} {

	set tree [Info $oop tree]
	set row [Info $oop current]

	set keyword [lindex [Info $oop settings] $row]
	NSList::SetTextEx $tree "root child $row" 1 [BirthSetting $keyword]

	return
}

# NSBirthOptions::RadiobuttonCmd --
#
#	Called when a radiobutton is selected.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::RadiobuttonCmd {oop settingSpec value} {

	if {[Info $oop ignoreChange]} return

regexp {([^,]+),?(.*)} $settingSpec ignore keyword subval

	# Update the game setting
	BirthSetting $keyword $value

	# Display help
	DisplaySettingHelp $oop $settingSpec

	return
}

# NSBirthOptions::DisplaySettingHelp --
#
#	Display the help text for the given setting keyword.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::DisplaySettingHelp {oop settingSpec} {

	set text [Info $oop win].frameHelp.text

	$text delete 1.0 end

regexp {([^,]+),?(.*)} $settingSpec ignore keyword subval

	set desc [BirthSettingDesc $keyword]
	set help [GetSettingHelp $oop $settingSpec]

	$text insert end $desc\n
	$text insert end $help tag1
	$text tag configure tag1 \
		-lmargin1 19 -lmargin2 19 -rmargin 0

	ForceTextWidgetViewUpdate $text

	return
}

# NSBirthOptions::GetSettingHelp --
#
#	Scan the options.txt file for the description of a setting.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::GetSettingHelp {oop keyword} {

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

# NSBirthOptions::GenOptionsIndex --
#
#	Reads the tk/doc/options.txt file, and for each keyword writes a
#	line to tk/doc/options-index. Each line consists of two elements:
#	the setting keyword and the line it was found on.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::GenOptionsIndex {} {

	if {[catch {open [PathTk doc options.txt]} readId]} {
		return
	}
	if {[catch {openlf [PathTk doc options-index]} \
		writeId]} {
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

# NSBirthOptions::ReadOptionsIndex --
#
#	Reads the tk/doc/options-index file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::ReadOptionsIndex {} {

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

# NSBirthOptions::Find --
#
#	Simple search routine to look for an option by name.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::Find {oop again} {

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

				# Select and the option
				NSList::Activate $tree "root child $member"
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

# NSBirthOptions::BirthSetting --
#
#	A wrapper for the "angband setting" command. This handles the
#	pseudo-settings used during character creation.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::BirthSetting {keyword args} {

	variable Priv

	set oop $Priv(oop)

	# Set
	if {[llength $args]} {
		set value [lindex $args 0]
		if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
			angband setting set $keyword $value
			switch -- $keyword {
				birth_point_based {
					if {$value} {
						angband setting set birth_auto_roller 0
						Synch $oop birth_auto_roller
					}
				}
				birth_auto_roller {
					if {$value} {
						angband setting set birth_point_based 0
						Synch $oop birth_point_based
					}
				}
				birth_maximize {
					BirthObj Info maximize $value
				}
				birth_preserve {
					BirthObj Info preserve $value
				}
			}
		}
		if {[variant ZANGBANDTK]} {
			switch -- $keyword {
				point_based {
					angband setting set $keyword $value
					if {$value} {
						angband setting set autoroller 0
						Synch $oop autoroller
					}
				}
				autoroller {
					angband setting set $keyword $value
					if {$value} {
						angband setting set point_based 0
						Synch $oop point_based
					}
				}
				max_quest {
					BirthObj Info $keyword $value
					return
				}
				wilderness_mode {
					switch -- $value {
						none {
							angband setting set lite_town 0
							angband setting set vanilla_town 1
						}
						lite {
							angband setting set lite_town 1
							angband setting set vanilla_town 0
						}
						normal {
							angband setting set lite_town 0
							angband setting set vanilla_town 0
						}
					}
					return
				}
				default {
					angband setting set $keyword $value
				}
			}
		# ZANGBANDTK
		}

	# Get
	} else {
		if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
			return [angband setting set $keyword]
		}
		if {[variant ZANGBANDTK]} {
			switch -- $keyword {
				max_quest {
					return [BirthObj Info max_quest]
				}
				wilderness_mode {
					set vanilla_town [angband setting set vanilla_town]
					set lite_town [angband setting set lite_town]
					if {$vanilla_town} {
						return none
					} elseif {$lite_town} {
						return lite
					} else {
						return normal
					}
				}
				default {
					return [angband setting set $keyword]
				}
			}
		# ZANGBANDTK
		}
	}

	return
}

# NSBirthOptions::BirthSettingDesc --
#
#	Return the human-readable description for a setting.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::BirthSettingDesc {keyword} {

	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		return [angband setting desc $keyword]
	}
	if {[variant ZANGBANDTK]} {
		switch -- $keyword {
			max_quest {
				return "Max random quests"
			}
			wilderness_mode {
				set vanilla_town [angband setting set vanilla_town]
				set lite_town [angband setting set lite_town]
				if {$vanilla_town} {
					return "Use 'vanilla' town without quests and wilderness"
				} elseif {$lite_town} {
					return "Use 'lite' town with quests and small wilderness"
				} else {
					return "Use multiple towns with quests and large wilderness"
				}
			}
			default {
				return [angband setting desc $keyword]
			}
		}
	# ZANGBANDTK
	}

	return
}

if {[variant ZANGBANDTK]} {

# NSBirthOptions::Validate_Quest --
#
#	Make sure the user entered a valid number of quests.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::Validate_Quests {oop} {

	set win [Info $oop win]

	set number [BirthObj Info max_quest]

	if {[string length $number]} {
		if {[string is integer $number]} {
			if {($number >= 0) && ($number <= [birth info max_quest])} {

				# Success
				return 0
			}
		}

	# Empty string --> zero
	} else {
		BirthObj Info max_quest 0

		# Success
		return 0
	}

	# Failed
	set entry $win.frame.quest.num
	$entry selection range 0 end
	$entry icursor end
	focus $entry
	bell

	# Failure
	return 1
}

# ZANGBANDTK
}

# NSBirthOptions::Validate --
#
#	Verify the info entered by the user. Return 1 if there were errors.
#	This command is also called by the NSTabs module, in which case
#	"args" contains the OOP ID of the NSTabs object.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBirthOptions::Validate {oop args} {

	set page [Info $oop page]
	if {[llength [info commands Validate_$page]]} {
		return [Validate_$page $oop]
	}

	# Success
	return 0
}
