# File: controls.tcl

# Purpose: the Macros Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSControls {

	variable MenuString
	variable Priv

# namespace eval NSControls
}

# NSControls::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::InitModule {} {

	variable Priv

	# Get command text from "main"
	MsgCatInit main controls

	NSModule::LoadIfNeeded NSList

	set commands {
		"Walk commands" ""
		"walk-pickup" ;
		"walk-nopickup" -
		"Walk NW" ;7
		"Walk N" ;8
		"Walk NE" ;9
		"Walk W" ;4
		"Walk E" ;6
		"Walk SW" ;1
		"Walk S" ;2
		"Walk SE" ;3

		"Run commands" ""
		"Run" .
		"Run NW" .7
		"Run N" .8
		"Run NE" .9
		"Run W" .4
		"Run E" .6
		"Run SW" .1
		"Run S" .2
		"Run SE" .3

		"Alter commands" ""
		"Alter" +
		"Alter NW" +7
		"Alter N" +8
		"Alter NE" +9
		"Alter W" +4
		"Alter E" +6
		"Alter SW" +1
		"Alter S" +2
		"Alter SE" +3

		"Inventory commands" ""
		"Equipment" e
		"Inventory" i

		"Object commands" ""
		"Take Off" t
		"Wear/Wield" w
		"Destroy" k
		"Drop" d

		"Inspect" I
		"Inscribe" \{
		"Uninscribe" \}

		"Activate" A
		"Aim Wand" a
		"Drink Potion" q
		"Read Scroll" r
		"Use Staff" u
		"Zap Rod" z

		"Eat Food" E
		"Fire Missile" f
		"Fuel Light" F
		"Jam Spike" j
		"Throw" v

		"Magic commands" ""
		"Browse" b
		"Study" G
		"Cast Spell" m

		"Alter commands" ""
		"Bash" B
		"Close" c
		"Disarm" D
		"Open" o
		"Tunnel" T

		"Resting commands" ""
		"Rest" R
		"Rest for HP/SP" R*\\r
		"Rest as needed" R&\\r

		"Other movement commands" ""
		"Go Down" >
		"Go Up" <
		"stay-pickup" ,
		"stay-nopickup" g

		"Searching commands" ""
		"Search" s
		"Search Mode" S

		"Misc commands" ""
		"Note" :
		}
	if {[variant ANGBANDTK KANGBANDTK]} {
		append commands {
		"Repeat" ^V
		}
	}
	if {[variant OANGBANDTK ZANGBANDTK]} {
		append commands {
		"Repeat" n
		}
	}
		append commands {
		"Target" *
		}
	if {[variant KANGBANDTK]} {
		append commands {
		"Pets" $
		}
	}
	if {[variant OANGBANDTK]} {
		append commands {
		"End Shapechange" \]
		}
	}
	if {[variant ZANGBANDTK]} {
		append commands {
		"Pets" p
		"Use Power" U
		}
	}

		append commands {
		"Information commands" ""
		"Character Info" C
		"Feeling" ^F
		"Knowledge" ~
		"Locate" L
		"Look" l
		"Map" M
		"Message History" ^P
		}
	if {[variant KANGBANDTK OANGBANDTK ZANGBANDTK]} {
		append commands {
		"Time Of Day" ^T
		}
	}
		append commands {
		"Version" V
		"Help" ?

		"Preferences commands" ""
		"Options" =
		"Prf Files" @

		"Saving and quitting commands" ""
		"Suicide/Retire" Q
		"Save" ^S
		"Quit With Save" ^X

		"Cheating/Debugging commands" ""
		"Debugging Command" ^A
		"Wizard Mode" ^W
	}
	set Priv(commands) $commands

	set Priv(oop) [NSObject::New NSControls]
	set Priv(selectedMacro) -1

	return
}

# NSControls::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::CloseModule {} {

	catch {
		destroy [Window controls]
	}

	return
}

# NSControls::NSControls --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::NSControls {oop} {

	Info $oop selection ""

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow controls $win \
		"GetDefaultGeometry $win reqwidth main2" "" \
		"NSControls::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSControls $oop $win

	#
	# Global list of application windows
	#

	Global controls,oop $oop
	Window controls $win

	return
}

# NSControls::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::Info {oop info args} {

	global NSControls

	# Verify the object
	NSObject::CheckObject NSControls $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSControls($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSControls($oop,$info)
			}
		}
	}

	return
}

# NSControls::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::InitWindow {oop} {

	set win .controls$oop
	toplevel $win
	wm title $win [mc Controls]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSControls::Close $oop"

	Info $oop win $win

	InitMenus $oop

	#
	# Divider
	#

	frame $win.divider1 \
		-borderwidth 1 -height 2 -relief groove

	#
	# List
	#

	set font [Value font,macros]
	set cw [font measure $font "W"]
	set column1Width 80
	set width [expr {$cw * 40 + $column1Width}]
	set rowHgt [font metrics $font -linespace]
	incr rowHgt 2

	set frame $win.frame
	set tree [NSList::New $frame -xscroll 0 -yscroll 1 -font macros -columns 2]
	$tree configure -width $width -treecolumn first
	$tree column configure 1 -justify center -minwidth $column1Width

	$tree state define edit
	set outline [$tree element cget eSel.e -outline]
	$tree element configure eSel.e -outline [concat {White edit} $outline]
	$tree element configure eSel.w -outline [concat {White edit} $outline]

	$tree element create eRect rect -fill Orange -height 1

	set S [$tree style create styHeader]
	$tree style elements $S {eRect eTxt}
	$tree style layout $S eRect -detach yes -expand n -iexpand x \
		-visible {no open}
	$tree style layout $S eTxt -expand ns -pady 4 -padx 4

	bind $tree <ButtonPress-1> "
		NSControls::Button1 $oop %x %y
	"

	bind $tree <Motion> "
		NSControls::Motion $oop %y
	"

	bind $tree <Leave> "
		NSControls::Motion $oop 100000
	"

	# Typing a key selects the command
	bindtags $tree [list ControlFindTrigger_BindTag $tree TreeCtrl $win all]

	Info $oop tree $tree

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
	grid columnconfig $win 0 -weight 1 -minsize 0
 
	grid $win.divider1 -in $win \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.frame -in $win \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.statusBar -in $win \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	#
	# KeyPress bindings
	#

	bind $win <KeyPress-Escape> "NSControls::Close $oop"
#	bind $win <Control-KeyPress-w> "NSControls::Close $oop"

	return
}

# NSControls::InitMenus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::InitMenus {oop} {

	variable MenuString

	# Default accelerator modifier
	set mod "Ctrl"

	set win [Info $oop win]

	#
	# Menu bar
	#

	Info $oop mbarId [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSControls::SetupMenus $oop" -identifier MENUBAR]
	set mbar [Info $oop mbarId]

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSControls::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSControls::MenuInvoke $oop"

	#
	# Controls Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_CONTROLS
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_CONTROLS -label [mc Controls] -underline 0 -identifier M_CONTROLS

	set entries {}
	Info $oop rogue_like [Setting rogue_like_commands]
	lappend entries [list -type checkbutton -label [mc "Rogue-like Keyset"] \
		-variable NSControls($oop,rogue_like) -identifier E_ROGUELIKE]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-underline 0 -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbar -end MENU_CONTROLS $entries

	qebind NSControls <Setting-rogue_like_commands> \
		"NSControls::SetList $oop ;
		set NSControls($oop,rogue_like) %c"
	qeconfigure NSControls <Setting-rogue_like_commands> -active no

	set MenuString(E_CLOSE) \
		"Closes the window."

	return
}

# NSControls::SetupMenus --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::SetupMenus {oop mbarId} {

	lappend identList E_ROGUELIKE E_CLOSE

	NSMenu::MenuEnable $mbarId $identList

	return
}

# NSControls::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::MenuSelect {oop menuId index ident} {

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

# NSControls::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::MenuInvoke {oop menuId ident} {

	switch -glob -- $ident {
		E_ROGUELIKE {
			Setting rogue_like_commands [Info $oop rogue_like]
		}
		E_CLOSE {Close $oop}
	}

	return
}

# NSControls::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::DisplayCmd {oop message first} {

	set tree [Info $oop tree]

	switch -- $message {
		preDisplay {
			SetList $oop
		}
		postDisplay {
			Info $oop rogue_like [Setting rogue_like_commands]
			qeconfigure NSControls <Setting-rogue_like_commands> -active yes
			focus $tree
		}
		postWithdraw {
			qeconfigure NSControls <Setting-rogue_like_commands> -active no
		}
	}

	return
}

# NSControls::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::Close {oop} {

	NSWindowManager::Undisplay controls

	return
}

# NSControls::Title --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::Title {title} {

	switch -- $title {
		stay-pickup {
			if {![Setting always_pickup]} { set title stay-nopickup }
		}
		stay-nopickup {
			# In OAngband, 'g' always picks up, while ',' considers
			# always_pickup.
			if {[variant OANGBANDTK]} {
				set title stay-pickup
			} else {
				if {![Setting always_pickup]} { set title stay-pickup }
			}
		}
		walk-pickup {
			if {![Setting always_pickup]} { set title walk-nopickup }
		}
		walk-nopickup {
			if {![Setting always_pickup]} { set title walk-pickup }
		}
	}

	return [mc $title]
}

# NSControls::SetList --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::SetList {oop} {

	variable Priv

	set win [Info $oop win]
	set tree [Info $oop tree]

	# Save state
	set openItems [$tree item id "state open"]
	set yview [$tree yview]

	# Clear the list
	NSList::Clear $tree

	set commands $Priv(commands)

	# Remember action for each row
	set actionList {}

	foreach {title action} $commands {
		set item [NSList::NewItem $tree]

		# Group header
		if {$action eq ""} {
			$tree item style set $item 0 styHeader 1 ""
			$tree item span $item 0 2
			$tree item enabled $item no
			$tree item collapse $item
			$tree item configure $item -button yes
			$tree item tag add $item header
			NSList::SetTextFill $tree $item Orange

			set headerItem $item

		# Regular command
		} else {

			# The trigger
			set textList ""
			foreach keypress [FindTriggersForAction $action] {
				if {[string index $keypress 0] eq "^"} {
					set keypress [mc Ctrl+][string index $keypress 1]
				} else {
					set keypress [string map [list Control- [mc Ctrl+] Shift- [mc Shift+] Alt- [mc Alt+]] $keypress]
				}
				lappend textList $keypress
			}
			NSList::SetTextEx $tree $item 1 [join $textList " "]

			set fill White

			$tree item lastchild $headerItem $item
		}

		NSList::SetText $tree $item [Title $title]

		# Remember action for each row
		lappend actionList $item $action
	}

	# Restore state
	foreach item $openItems {
		$tree item expand $item
	}
	$tree yview moveto [lindex $yview 0]

	# Disable trigger bindtag
	bindtags $tree [list ControlFindTrigger_BindTag $tree TreeCtrl $win all] 

	Info $oop actionList $actionList
	Info $oop editRow ""
	Info $oop motionRow ""

	return
}

# NSControls::Button1 --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::Button1 {oop x y} {

	set win [Info $oop win]
	set tree [Info $oop tree]
	set row [Info $oop selection]

	if {[Info $oop editRow] ne ""} {
		$tree item state set [Info $oop editRow] !edit
	}
	if {$row eq ""} {

		# Disable trigger bindtag
		bindtags $tree [list ControlFindTrigger_BindTag $tree TreeCtrl $win all]

		# Expand/collapse header items
		set id [$tree identify $x $y]
		if {[lindex $id 0] eq "item"} {
			set item [lindex $id 1]
			if {[$tree item tag expr $item header]} {
				$tree item toggle $item
			}
		}
	} else {
		$tree selection clear
		Info $oop selection ""
		$tree item state set $row edit

		# Enable trigger bindtag
		bindtags $tree [list ControlSetTrigger_BindTag $tree TreeCtrl $win all] 
	}

	Info $oop editRow $row

	return
}

# NSControls::Motion --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::Motion {oop y} {

	set tree [Info $oop tree]

	set id [$tree identify 10 $y]
	if {[lindex $id 0] eq "item"} {
		set row [lindex $id 1]
	} else {
		set row ""
	}
	if {$row ne [Info $oop motionRow]} {
		if {[Info $oop motionRow] ne ""} {
			Leave $oop [Info $oop motionRow]
		}
		if {$row ne ""} {
			Enter $oop $row
		}
	}
	Info $oop motionRow $row

	return
}

# NSControls::Enter --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::Enter {oop row} {

	set tree [Info $oop tree]

	if {$row ne [Info $oop editRow] && [$tree item tag expr $row !header]} {
		$tree selection add $row
		Info $oop selection $row
	}

	return
}

# NSControls::Leave --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::Leave {oop row} {

	set tree [Info $oop tree]

	if {$row ne [Info $oop editRow] && [$tree item tag expr $row !header]} {
		$tree selection clear
		Info $oop selection ""
	}

	return
}

# The following bindings are used to get a keymap/macro trigger keypress
# from the user. They should be almost identical to those found in
# keyboard.tcl. The differences are that (1) the keypress passed
# a local command, not as an argument to "angband keypress"; and (2)
# the "wrapper" chars \037 and \015 are not written.

bind ControlSetTrigger_BindTag <Control-Shift-KeyPress> {

	NSControls::SetTrigger Control-Shift-%K
	break
}

bind ControlSetTrigger_BindTag <Control-KeyPress> {

	# Special Control-KeyPress (ex Control-F1)
	if {![string length %A] || [string length %K] > 1} {
		NSControls::SetTrigger Control-%K

	# Ascii Control-KeyPress
	} else {
		set ktrl [scan %A "%%c"]
		incr ktrl 64
		set char [format "%%c" $ktrl]

		NSControls::SetTrigger ^$char
	}
	break
}

bind ControlSetTrigger_BindTag <Shift-KeyPress> {

	# Special Shift-KeyPress (ex Shift-F1)
	if {![string length %A]} {
		NSControls::SetTrigger Shift-%K

	# Ascii Shift-KeyPress
	} else {
		NSControls::SetTrigger %A
	}
	break
}

bind ControlSetTrigger_BindTag <Alt-KeyPress> {

	NSControls::SetTrigger Alt-%K
	break
}

bind ControlSetTrigger_BindTag <KeyPress> {

	# Special KeyPress (ex F1)
	if {![string length %A]} {
		NSControls::SetTrigger %K

	# Normal keys with no modifiers
	} else {
		NSControls::SetTrigger %A
	}
	break
}

bind ControlSetTrigger_BindTag <Escape> {

	bell
	break
}

bind ControlSetTrigger_BindTag <Return> {

	bell
	break
}

bind ControlSetTrigger_BindTag <Tab> {

	bell
	break
}

bind ControlSetTrigger_BindTag <BackSpace> {

	NSControls::RemoveTrigger
	break
}

bind ControlSetTrigger_BindTag <Delete> {

	NSControls::RemoveTrigger
	break
}

# Ignore modifiers by themselves
foreach mod1 {Control Shift Alt} {
	bind ControlSetTrigger_BindTag <${mod1}_L> { break }
	bind ControlSetTrigger_BindTag <${mod1}_R> { break }
	foreach mod2 {Control Shift Alt} {
		if {$mod1 eq $mod2} continue
		bind ControlSetTrigger_BindTag <$mod1-${mod2}_L> { break }
		bind ControlSetTrigger_BindTag <$mod1-${mod2}_R> { break }
		bind ControlSetTrigger_BindTag <$mod2-${mod1}_L> { break }
		bind ControlSetTrigger_BindTag <$mod2-${mod1}_R> { break }
	}
}

# The following bindings are used to get a keymap/macro trigger keypress
# from the user. They should be almost identical to those found in
# keyboard.tcl. The differences are that (1) the keypress passed
# a local command, not as an argument to "angband keypress"; and (2)
# the "wrapper" chars \037 and \015 are not written.

bind ControlFindTrigger_BindTag <Control-Shift-KeyPress> {

	NSControls::FindTrigger Control-Shift-%K
	break
}

bind ControlFindTrigger_BindTag <Control-KeyPress> {

	# Special Control-KeyPress (ex Control-F1)
	if {![string length %A] || [string length %K] > 1} {
		NSControls::FindTrigger Control-%K

	# Ascii Control-KeyPress
	} else {
		set ktrl [scan %A "%%c"]
		incr ktrl 64
		set char [format "%%c" $ktrl]

		NSControls::FindTrigger ^$char
	}
	break
}

bind ControlFindTrigger_BindTag <Shift-KeyPress> {

	# Special Shift-KeyPress (ex Shift-F1)
	if {![string length %A]} {
		NSControls::FindTrigger Shift-%K

	# Ascii Shift-KeyPress
	} else {
		NSControls::FindTrigger %A
	}
	break
}

bind ControlFindTrigger_BindTag <Alt-KeyPress> {

	NSControls::FindTrigger Alt-%K
	break
}

bind ControlFindTrigger_BindTag <KeyPress> {

	# Special KeyPress (ex F1)
	if {![string length %A]} {
		NSControls::FindTrigger %K

	# Normal keys with no modifiers
	} else {
		NSControls::FindTrigger %A
	}
	break
}

bind ControlFindTrigger_BindTag <Escape> {
	# close the window
}

bind ControlFindTrigger_BindTag <Return> {

	bell
	break
}

bind ControlFindTrigger_BindTag <Tab> {

	bell
	break
}

bind ControlFindTrigger_BindTag <BackSpace> {

	bell
	break
}

bind ControlFindTrigger_BindTag <Delete> {

	bell
	break
}

# Ignore modifiers by themselves
foreach mod1 {Control Shift Alt} {
	bind ControlFindTrigger_BindTag <${mod1}_L> { break }
	bind ControlFindTrigger_BindTag <${mod1}_R> { break }
	foreach mod2 {Control Shift Alt} {
		if {$mod1 eq $mod2} continue
		bind ControlFindTrigger_BindTag <$mod1-${mod2}_L> { break }
		bind ControlFindTrigger_BindTag <$mod1-${mod2}_R> { break }
		bind ControlFindTrigger_BindTag <$mod2-${mod1}_L> { break }
		bind ControlFindTrigger_BindTag <$mod2-${mod1}_R> { break }
	}
}

# NSControls::VerifySetTrigger --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::VerifySetTrigger {oop trigger} {

	variable Priv

	set usedBy ""

	# Simple keypress or ASCII control-character -> keymap
	if {[string length $trigger] == 1 || [string index $trigger 0] eq "^"} {

		set action [angband keymap action $trigger]

		# If the trigger equals the action then there won't be a keymap.
		# See if one of the listed commands uses the trigger.
		if {$action eq ""} {
			foreach {title action1} $Priv(commands) {
				if {$trigger eq $action1} {
					set action $trigger
					break
				}
			}
		}
		if {$action ne ""} {
			set usedBy $action
		}

	# Complicated keypress -> macro
	} else {

		# Get number of macros
		set max [angband macro max]

		# Iterate over macros
		for {set i 0} {$i < $max} {incr i} {

			# Get the keypress
			set keypress [angband macro keypress $i]

			# Check for macro using this trigger
			if {$keypress eq "^_$trigger\\r"} {

				# Get the action
				set maction [angband macro action $i]

				# Strip out \e and \m
				set maction [string map {\\e "" \\m ""} $maction]

				# Strip out \\
				set maction [string trimleft $maction "\\\\"]

				set usedBy $maction
				break
			}
		}
	}

	# The trigger isn't used
	if {$usedBy eq ""} {
		return 1
	}

	set index 0
	foreach {item action} [Info $oop actionList] {
		if {$action eq $usedBy} {
			set item [Info $oop editRow]
			set row [[Info $oop tree] item order $item]

			# Allow the same key to be reassigned
			if {$row - 1 == $index} {return 1}

			# Format a prompt
			if {[string index $trigger 0] eq "^"} {
				set trigger [mc Ctrl+][string index $trigger 1]
			} else {
				set trigger [string map [list Control- [mc Ctrl+] Shift- [mc Shift+] Alt- [mc Alt+]] $trigger]
			}
			set command0 [lindex $Priv(commands) [expr {($row-1) * 2}]]
			set command1 [lindex $Priv(commands) [expr {$index * 2}]]
			set prompt [format [mc verify-prompt] $trigger [Title $command1] [Title $command0]]

			# Ask the user to confirm
			set answer [tk_messageBox -parent [Info $oop win] \
				-message $prompt -title [mc "Assign key"] \
				-type yesno -icon warning]

			return [expr {$answer eq "yes"}]
		}
		incr index
	}

	return 1
}

# NSControls::SetTrigger --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::SetTrigger {trigger} {

	set oop [Global controls,oop]
	set editRow [Info $oop editRow]

	if {![VerifySetTrigger $oop $trigger]} return

	# Get the action for the selected row
	array set actions [Info $oop actionList]
	set action $actions($editRow)

	# Ignore header item
	if {$action eq ""} return

	# FIXME: this calls SetList
	RemoveTrigger

	# Simple keypress -> keymap
	if {[string length $trigger] == 1} {

		# Trigger equals underlying command -> empty keymap
		if {$trigger eq $action} {
			angband keymap action $trigger ""
		} else {
			angband keymap action $trigger $action
		}

	# ASCII control-character -> keymap
	} elseif {[string index $trigger 0] eq "^"} {

		# Trigger equals underlying command -> empty keymap
		if {$trigger eq $action} {
			angband keymap action $trigger ""
		} else {
			angband keymap action $trigger $action
		}

	# Complicated keypress -> macro
	} else {

		set found 0
		set empty -1

		# Get number of macros
		set max [angband macro max]

		# Iterate over macros
		for {set i 0} {$i < $max} {incr i} {

			# Get the keypress
			set keypress [angband macro keypress $i]

			# Check for macro using this trigger
			if {$keypress eq "^_$trigger\\r"} {
				# Set the action, but prepend "keymap" backslash
				angband macro action $i \\m\\\\$action
				set found 1
			}

			# Check for empty macro we can reuse
			if {[angband macro action $i] eq ""} {
				set empty $i
			}
		}

		# Use an existing empty macro
		if {!$found && $empty != -1} {
			angband macro keypress $empty ^_$trigger\r
			angband macro action $empty \\m\\\\$action
			set found 1
		}

		# Create a new macro
		if {!$found} {
			set i [angband macro create ^_$trigger\r]
			angband macro action $i \\m\\\\$action
		}
	}

	[Info $oop tree] see $editRow

	# Update the list
	SetList $oop

	return
}

# NSControls::RemoveTrigger --
#
#	Cancels any keymaps or macros for the selected command.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::RemoveTrigger {} {

	set oop [Global controls,oop]
	set editRow [Info $oop editRow]

	# Get the action for the selected row
	array set actions [Info $oop actionList]
	set action $actions($editRow)

	# Clear any keymap for this action
	foreach keymap [angband keymap find $action] {
		angband keymap action $keymap ""
	}

	# Clear any macro for this action

	# Get number of macros
	set max [angband macro max]

	# Iterate over macros
	for {set i 0} {$i < $max} {incr i} {

		# Get the action
		set maction [angband macro action $i]

		# Strip out \e and \m
		set maction [string map {\\e "" \\m ""} $maction]

		if {$maction eq "\\\\$action"} {

			# Set empty action
			angband macro delete $i

			incr i -1
			incr max -1
		}
	}

	[Info $oop tree] see $editRow

	# Update the list
	SetList $oop

	return
}

# NSControls::FindTriggersForAction --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::FindTriggersForAction {action} {

	# Macros have priority over keymaps I think

	# Get number of macros
	set max [angband macro max]

	# Iterate over macros
	for {set i 0} {$i < $max} {incr i} {

		# Get the action
		set maction [angband macro action $i]

		# Strip out \e and \m
		set maction [string map {\\e "" \\m ""} $maction]

		if {$maction eq "\\\\$action"} {

			# Get the keypress
			set keypress [angband macro keypress $i]

			# Strip leading "_^" and trailing "\r" from keypress
			regexp {\^_(.+)\\r} $keypress ignore keypress

			return $keypress
		}
	}

	# See if any keys have this action assigned to it
	set keymaps [angband keymap find $action]
	if {[llength $keymaps]} {
		set trigger [lindex $keymaps 0]
		# If a key maps to itself, and another choice exists, use it
		if {($trigger eq $action) && ([llength $keymaps] > 1)} {
			set trigger [lindex $keymaps 1]
		}
		return $trigger
	}

	return ""
}

proc NSControls::FindTriggersForAction {action} {

	set result {}

	# Get number of macros
	set max [angband macro max]

	# Iterate over macros
	for {set i 0} {$i < $max} {incr i} {

		# Get the action
		set maction [angband macro action $i]

		# Strip out \e and \m
		set maction [string map {\\e "" \\m ""} $maction]

		if {$maction eq "\\\\$action"} {

			# Get the keypress
			set keypress [angband macro keypress $i]

			# Strip leading "_^" and trailing "\r" from keypress
			regexp {\^_(.+)\\r} $keypress ignore keypress

			lappend result $keypress
		}
	}

	# See if any keys have this action assigned to it
	foreach trigger [angband keymap find $action] {
		lappend result $trigger
	}

	return $result
}

# NSControls::FindTrigger --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::FindTrigger {trigger} {

	variable Priv

	set oop [Global controls,oop]
	set tree [Info $oop tree]

	set max [angband macro max]
	for {set i 0} {$i < $max} {incr i} {
		set mtrigger [angband macro keypress $i]
		if {$mtrigger eq "^_$trigger\\r"} {

			# Get the action
			set maction [angband macro action $i]

			# Strip out \e and \m
			set maction [string map {\\e "" \\m ""} $maction]

			# Strip out \\
			set maction [string trimleft $maction "\\\\"]

			set ls [lsearch -exact [Info $oop actionList] $maction]
			if {$ls != -1} {
				$tree item collapse "root children"
				set item [lindex [Info $oop actionList] [expr {$ls - 1}]]
				$tree item expand [$tree item parent $item]
				$tree selection modify $item all
				$tree see $item -center y
				return
			}
		}
	}

	foreach {ktrigger kaction} [angband keymap find] {
		if {$ktrigger eq $trigger} {
			set i [lsearch -exact [Info $oop actionList] $kaction]
			if {$i != -1} {
				$tree item collapse "root children"
				set item [lindex [Info $oop actionList] [expr {$i - 1}]]
				$tree item expand [$tree item parent $item]
				$tree selection modify $item all
				$tree see $item -center y
				return
			}
		}
	}

	# Simple keypress or ASCII control-character -> keymap
	if {[string length $trigger] == 1 || [string index $trigger 0] eq "^"} {

		set kaction [angband keymap action $trigger]

		# If the trigger equals the action then there won't be a keymap.
		# See if one of the listed commands uses the trigger.
		if {$kaction eq ""} {
			set i [lsearch -exact [Info $oop actionList] $trigger]
			if {$i != -1} {
				$tree item collapse "root children"
				set item [lindex [Info $oop actionList] [expr {$i - 1}]]
				$tree item expand [$tree item parent $item]
				$tree selection modify $item all
				$tree see $item -center y
				return
			}
		}
	}

	return
}

# NSControls::WriteControlsDotPrf --
#
#	Write keymaps and macros to lib/user/controls.prf.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSControls::WriteControlsDotPrf {} {

	variable Priv

	NSModule::LoadIfNeeded NSPrfFile

	set prfId [NSObject::New NSPrfFile]
	set path [Path lib user controls.prf]
	NSPrfFile::File $prfId $path

	set rogue_like [Setting rogue_like_commands]

	# Get number of macros
	set max [angband macro max]

	# Iterate over macros
	for {set i 0} {$i < $max} {incr i} {

		# Get the action
		set maction [angband macro action $i]

		# Strip out \e and \m
		set maction [string map {\\e "" \\m ""} $maction]

		set mactions($maction) $i
	}

	# Optimize "angband keymap find"
	foreach mode {0 1} {
		Setting rogue_like_commands $mode
		foreach {keypress action} [angband keymap find] {
			lappend kactions($mode,$action) $keypress
		}
	}

	foreach {title action} $Priv(commands) {

		# Group title
		if {$action eq ""} continue

		# See if a macro has this action assigned to it
		set maction "\\\\$action"
		if {[info exists mactions($maction)]} {
			set i $mactions($maction)
			set keypress [angband macro keypress $i]
			NSPrfFile::MacroNew $prfId $keypress [angband macro action $i]
			continue
		}

		# See if any keys have this action assigned to it
		foreach mode {0 1} {
			Setting rogue_like_commands $mode
			if {[info exists kactions($mode,$action)]} {
				foreach trigger $kactions($mode,$action) {
					NSPrfFile::KeymapNew $prfId $trigger $action
				}
			}
		}
	}

	Setting rogue_like_commands $rogue_like

	NSPrfFile::Write $prfId
	NSObject::Delete NSPrfFile $prfId

	return
}

