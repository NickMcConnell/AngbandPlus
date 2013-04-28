# File: inventory.tcl

# Purpose: the Inventory Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSInventory {

	variable Priv

# namespace eval NSInventory
}

# NSInventory::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::InitModule {} {

	MsgCatInit inven

	InitImageIfNeeded Image_ButtonOptions button-options.gif
	InitImageIfNeeded Image_ButtonHelp button-help.gif

	NSModule::LoadIfNeeded NSStatusBar
	NSModule::LoadIfNeeded NSToolbar
	NSModule::LoadIfNeeded NSList

	# Create the Inventory Window
	NSObject::New NSInventory

	return
}

# NSInventory::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::CloseModule {} {

	catch {
		destroy [Window inventory]
	}

	return
}

# NSInventory::NSInventory --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::NSInventory {oop} {

	Info $oop alwaysOnTop [Value inventory,alwaysOnTop]
	Info $oop browsing 0
	Info $oop choose,show 0
	Info $oop didChoose 0
	Info $oop both 0
	Info $oop current -1
	Info $oop toolbar,match {}
	Info $oop oldFocus ""
	Info $oop skipHide 0

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow inventory $win \
		"GetDefaultGeometry $win main2 main2" "" \
		"NSInventory::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSInventory $oop $win

	# Update the display when some settings change
	qebind NSInventory <Setting> {
		InventoryObj SettingChanged %d %c
	}
	qeconfigure NSInventory <Setting> -active no

	qebind NSInventory <Track> {
		InventoryObj Track %d
	}
	qeconfigure NSInventory <Track> -active no

	qebind NSInventory <Term-inkey> {
		InventoryObj TermInkey
	}
	qeconfigure NSInventory <Term-inkey> -active no

	qebind NSInventory <Choose-item> {
		InventoryObj ChooseItem %s %o
	}
	qeconfigure NSInventory <Choose-item> -active no

	qebind NSInventory2 <IconCfg> \
		"NSInventory::IconCfg $oop"

	bind $win <KeyPress-Escape> {
		InventoryObj Close
		break
	}

	#
	# Global list of application windows
	#

	Global inventory,oop $oop
	Window inventory $win

	return
}

# NSInventory::~NSInventory --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::~NSInventory {oop} {

	return
}

# NSInventory::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::Info {oop info args} {

	global NSInventory

	# Verify the object
	NSObject::CheckObject NSInventory $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSInventory($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSInventory($oop,$info)
			}
		}
	}

	return
}

# NSInventory::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::InitWindow {oop} {

	set win .inventory$oop
	toplevel $win
	wm title $win Inventory

	# Start out withdrawn (hidden)
	wm withdraw $win

	if {[Info $oop alwaysOnTop]} {
		# Do this *after* [wm withdraw] or it pops onscreen
		NSMainWindow::TransientToMain $win
	}

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSInventory::Close $oop"

	Info $oop win $win

	InitMenus $oop

	#
	# Toolbar
	#

	set toolId [NSObject::New NSToolbar 20 $win]
	NSToolbar::AddTool $toolId -image Image_ButtonOptions \
		-showlabel no -command "DoCommandIfAllowed =" -hasmenu yes \
		-menucommand "NSInventory::Win98MenuCmd_Options $oop"
	NSToolbar::AddTool $toolId -image Image_ButtonHelp \
		-showlabel no -command "DoCommandIfAllowed ?"

	set menuId [NSObject::New NSMenu *$win \
		-tearoff 0 -identifier MENU_TOOLBAR]
	NSMenu::Info $menuId menuSelectCmd "NSInventory::MenuSelect $oop"

	set menu [NSMenu::Info $menuId menu]
	if {[Platform unix]} {
		$menu configure -cursor arrow
	}
	Info $oop toolbar,menu $menu

	#
	# Entry for editing the inscription
	#

	set frame [NSToolbar::Info $toolId frame].frameInscription
	frame $frame \
		-borderwidth 0
	MakeDivider $frame.divider y
	label $frame.label \
		-text [mc Inscription:]
	set entry $frame.entry
	entry $entry -width 25

	bind $entry <KeyPress-Return> \
		"NSInventory::CommitInscription $oop"

	pack $frame.divider \
		-side left -fill y -padx 2
	pack $frame.label \
		-side left
	pack $entry \
		-side left
	pack $frame \
		-expand no -padx 2 -pady 2 -side left

	Info $oop inscription,entry $frame.entry

	#
	# Divider
	#

#	frame $win.divider2 \
#		-borderwidth 1 -height 2 -relief groove

	#
	# List
	#

	set cw [font measure [Value font,inventory] "W"]
	set width [expr {$cw * 81}]

	set frame $win.frame
	NSList::New $frame -columns 5 -font inventory
	set tree $frame.tree
	$tree configure -width $width -height 300

	$tree column configure 0 -tag char -expand no
	$tree column configure 1 -tag label
	$tree column configure 2 -tag semicolon
	$tree column configure 3 -tag desc -expand yes -squeeze yes -minwidth 100
	$tree column configure 4 -tag weight -justify right

	Info $oop tree $tree

	# When an item is selected, recall it
	NSList::OnSelection $tree \
		"NSInventory::SelectionChanged $oop %T %c %S %D"

	# Double-click to select item
	NSList::OnInvoke $tree \
		"NSInventory::Invoke $oop %r"

	bind $tree <ButtonPress-1> {
		if {[angband inkey_flags] eq "INKEY_MORE"} {
			angband keypress " "
			break
		}
	}

	#
	# Statusbar
	#
if 1 {
	statusbar $win.statusBar -sizes {18 14 18 15 12} -weights {1 1 1 1 1} \
		-tags {t1 t2 t3 t4 t5}
} else {
	statusbar $win.statusBar -sizes {18 14 18 15 12} -weights {0 0 0 1 0} \
		-tags {t1 t2 t3 t4 t5}
}

	#
	# Geometry
	#

	grid rowconfig $win 0 -weight 0 -minsize 0
	grid rowconfig $win 1 -weight 1 -minsize 0
	grid rowconfig $win 2 -weight 0 -minsize 0
#	grid rowconfig $win 3 -weight 0 -minsize 0
	grid columnconfig $win 0 -weight 1 -minsize 0
 
	pack forget [NSToolbar::Info $toolId frame]
	grid [NSToolbar::Info $toolId frame] -in $win \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
#	grid $win.divider2 -in $win \
#		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.frame -in $win \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.statusBar -in $win \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	#
	# Context Menu
	#

	set m $win.context
	menu $m -tearoff 0
	bind $tree <ButtonPress-3> "NSInventory::ContextMenu $oop $m %X %Y"

	#
	# Feed Term when keys pressed
	#

	Term_KeyPress_Bind $win
	Term_KeyPress_Bind $tree

	bind $win <FocusIn> "
		if {\[string equal %W $win]} {
			focus $tree
		}
	"

	return
}

# NSInventory::InitMenus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::InitMenus {oop} {
}

# NSInventory::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::MenuSelect {oop menuId index ident} {

	variable MenuString
	variable Priv

	switch -- $ident {
		{} {
			set desc {}
		}
		MENU_TOOLBAR {
			set menu [NSMenu::Info $menuId menu]
			switch -- [Info $oop toolbar,mode] {
				option {
					set desc [lindex [Info $oop toolbar,desc] $index].
				}
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

	[Info $oop win].statusBar cover set $desc
	if {![string length $desc]} {
		if {0 && $menuId == [Info $oop mbarId]} {
			[Info $oop win].statusBar cover hide
		}
	}

	return
}

# NSInventory::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::DisplayCmd {oop message first args} {

dbwin "NSInventory::DisplayCmd $message $first $args\n"

	switch -- $message {
		preDisplay {
			qeconfigure NSInventory <Setting> -active yes
			SetList $oop {*}$args
			if {([llength $args] == 3) && [lindex $args 2]} {
				Info $oop browsing 1
			}
			qeconfigure NSInventory <Track> -active yes
			qeconfigure NSInventory <Choose-item> -active yes
		}
		postDisplay {
		}
		reDisplay {
			if {[Info $oop browsing]} {
				if {![Info $oop alwaysOnTop]} {
					if {([llength $args] == 3) && [lindex $args 2]} {
						set focus [focus]
						if {[string length $focus] &&
							![string match [Info $oop win]* $focus]} {
							Info $oop oldFocus $focus
							Info $oop skipHide 1
						}
					}
				}
			}
			if {[Info $oop browsing]} {
				if {[llength $args] == 3} {
					set both [lindex $args 2]
				} else {
					set both 0
				}
				if {[string compare [lindex $args 0] [Info $oop invOrEquip]] ||
					($both != [Info $oop both])} {
					SetList $oop {*}$args
				}
			} else {
				SetList $oop {*}$args
			}
			if {![Info $oop alwaysOnTop]} {
				WindowBringToFront [Info $oop win]
			}
		}
		postWithdraw {
			qeconfigure NSInventory <Setting> -active no
			qeconfigure NSInventory <Track> -active no
			qeconfigure NSInventory <Choose-item> -active no
			Info $oop browsing 0
		}
	}

	return
}

# NSInventory::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::Close {oop} {

	if {[angband inkey_flags] eq "INKEY_ITEM"} {
dbwin "NSInventory::Close -> Escape\n"
		angband keypress \033
	} elseif {[Info $oop skipHide]} {
dbwin "NSInventory::Close -> Skip\n"
		set oldFocus [Info $oop oldFocus]
		if {[string length $oldFocus]} {
			catch {focus $oldFocus}
		}
		Info $oop skipHide 0
	} elseif {[Info $oop browsing]} {
dbwin "NSInventory::Close -> Undisplay\n"
		NSWindowManager::Undisplay inventory
	}

	return
}

# NSInventory::Win98MenuCmd_Options --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::Win98MenuCmd_Options {oop button} {

	set canvas [$button info canvas]
	set x [winfo rootx $canvas]
	set y [expr {[winfo rooty $canvas] + [winfo height $canvas]}]

	set menu [Info $oop toolbar,menu]
	$menu delete 0 end

	set keywordList {}
	set descList {}

	Info $oop setting,show_flavors [Setting show_flavors]
	$menu add checkbutton -label [mc "Show Flavors"] \
		-command "Setting show_flavors \$NSInventory($oop,setting,show_flavors)" \
		-variable ::NSInventory($oop,setting,show_flavors)
	lappend keywordList show_flavors
	lappend descList [SettingDesc show_flavors]

	Info $oop setting,show_icons [Setting show_icons]
	$menu add checkbutton -label [mc "Show Icons"] \
		-command "Setting show_icons \$NSInventory($oop,setting,show_icons)" \
		-variable ::NSInventory($oop,setting,show_icons)
	lappend keywordList show_icons
	lappend descList [SettingDesc show_icons]

	if {[Info $oop invOrEquip] eq "equipment"} {
		Info $oop setting,show_labels [Setting show_labels]
		$menu add checkbutton -label [mc "Show Labels"] \
			-command "Setting show_labels \$NSInventory($oop,setting,show_labels)" \
			-variable ::NSInventory($oop,setting,show_labels)
		lappend keywordList show_labels
		lappend descList [SettingDesc show_labels]
	}

	Info $oop setting,show_weights [Setting show_weights]
	$menu add checkbutton -label [mc "Show Weights"] \
		-command "Setting show_weights \$NSInventory($oop,setting,show_weights)" \
		-variable ::NSInventory($oop,setting,show_weights)
	lappend keywordList show_weights
	lappend descList [SettingDesc show_weights]

	if {[variant OANGBANDTK]} {
		Info $oop setting,use_metric [Setting use_metric]
		$menu add checkbutton -label [mc "Use Metric"] \
			-command "Setting use_metric \$NSInventory($oop,setting,use_metric)" \
			-variable ::NSInventory($oop,setting,use_metric)
		lappend keywordList use_metric
		lappend descList [SettingDesc use_metric]
	}

	$menu add separator
	lappend descList ""

	$menu add checkbutton -label [mc "Always On Top"] \
		-command {InventoryObj AlwaysOnTop} \
		-variable ::NSInventory($oop,alwaysOnTop)
	lappend descList "Keep the window on top of the Main Window"

	$menu add command -label [mc "Graphics Mode"] -command "InventoryObj Swap"
	lappend descList "Use the graphical window"

	$menu add separator
	lappend descList ""

	$menu add command -label [mc "Set Font"] -command {
		NSModule::LoadIfNeeded NSFont
		NSWindowManager::Display font inventory
	}
	lappend descList "Set the font"

	Info $oop toolbar,mode option
	Info $oop toolbar,match $keywordList
	Info $oop toolbar,desc $descList

	[Info $oop win].statusBar cover show

	tk_popup $menu $x $y

	if {[Platform unix]} {
		tkwait variable ::tk::Priv(popup)
	}

	after idle "$button hidemenu ; [Info $oop win].statusBar cover hide"

	return
}

# NSInventory::SettingChanged --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::SettingChanged {oop keyword value} {

	# Ignore settings which don't affect the display
	if {[lsearch -exact [Info $oop toolbar,match] $keyword] == -1} return

	# Update the button
	update idletasks

	SetList $oop "" "" [Info $oop both]

	# XXX Mega-Hack -- OAngband displays the current weight in the
	# message line. If the "use_metric" option changes, then we
	# change the prompt by hand to display the correct units.
	if {$keyword eq "use_metric"} {
		global Prompt
		set prompt $Prompt(new)
		if {[string match "*burden*" $prompt]} {
			set weight [angband player total_weight]
			if {[Setting use_metric]} {
				set old [format "%d.%d lb" [expr {$weight / 10}] [expr {$weight % 10}]]
				set weight [make_metric $weight]
				set new [format "%d.%d kg" [expr {$weight / 10}] [expr {$weight % 10}]]
			} else {
				set new [format "%d.%d lb" [expr {$weight / 10}] [expr {$weight % 10}]]
				set weight [make_metric $weight]
				set old [format "%d.%d kg" [expr {$weight / 10}] [expr {$weight % 10}]]
			}
			regsub -all $old $prompt $new Prompt(new)
			Fresh_Prompt
		}
	}

	return
}

# NSInventory::GetItemCommand --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::GetItemCommand {oop index} {

	set where [Info $oop invOrEquip]
	return [NSGlobal::GetItemCommand $where $index 0]
###
return

	upvar $_command command $_label label

	set command ""
	set label ""

	angband $where info $index attrib
	set charItem $attrib(char)

	if {$where eq "equipment"} {
		if {$attrib(known) && $attrib(activate)} {
			set label [mc Activate]
			set charCmd A
		} else {
			set label [mc Remove]
			set charCmd t
		}
		set command "DoKeymapCmd {} $charCmd $charItem"
		return
	}

	switch -glob -- $attrib(tval) {
		*_BOOK {

			# See if the character can read this book
			if {[lsearch -exact [angband player spell_book] $attrib(tval)] != -1} {

				set label [mc Browse]
				set charCmd b

				# Hack -- Browse shows all the books
				set command "DoKeymapCmd {} $charCmd {}"
				return
			}
		}
		TV_ARROW -
		TV_BOLT -
		TV_SHOT {
			# See if the character can fire this ammo
			if {[variant ZANGBANDTK]} {
				set field tval_ammo
			} else {
				set field ammo_tval
			}
			if {[struct set player_type 0 $field] eq $attrib(tval)} {
				set label [mc Fire]
				set charCmd f
			}
		}
		TV_FLASK {
			# See if light source is a lantern
			angband equipment info INVEN_LITE attrib
			if {$attrib(sval) == 1} {
				set label [mc Refuel]
				set charCmd F
			}
		}
		TV_FOOD {
			set label [mc Eat]
			set charCmd E
		}
		TV_POTION {
			set label [mc Drink]
			set charCmd q
		}
		TV_SCROLL {
			set label [mc Read]
			set charCmd r
		}
		TV_SPIKE {
			set label [mc Jam]
			set charCmd j
		}
		TV_STAFF {
			set label [mc Use]
			set charCmd u
		}
		TV_ROD {
			set label [mc Zap]
			set charCmd z
		}
		TV_WAND {
			set label [mc Aim]
			set charCmd a
		}
		TV_BOW -
		TV_DIGGING -
		TV_HAFTED -
		TV_POLEARM -
		TV_SWORD -
		TV_BOOTS -
		TV_GLOVES -
		TV_HELM -
		TV_CROWN -
		TV_SHIELD -
		TV_CLOAK -
		TV_SOFT_ARMOR -
		TV_HARD_ARMOR -
		TV_DRAG_ARMOR -
		TV_LITE -
		TV_AMULET -
		TV_RING {
			set label [mc Wield]
			set charCmd w
		}
	}

	if {[string length $label]} {
		set command "DoKeymapCmd {} $charCmd $charItem"
	}

	return
}

# NSInventory::Invoke --
#
#	When an inventory item is double-clicked, "angband keypress" to use it.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::Invoke {oop row} {

	variable Priv

	set char [lindex $Priv(char) $row]
	set index [lindex $Priv(index) $row]

	if {[angband inkey_flags] eq "INKEY_CMD"} {
		set list [GetItemCommand $oop $index]
		set command [lindex $list 1]
		if {$command ne ""} {
			eval $command
		}
		return
	}

	if {[angband inkey_flags] eq "INKEY_ITEM"} {
		angband keypress $char
	}

	return
}

# NSInventory::SelectionChanged --
#
#	Called when the list selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::SelectionChanged {oop tree count select deselect} {

	variable Priv

	set entry [Info $oop inscription,entry]
	set invOrEquip [Info $oop invOrEquip]

	# Disable the inscription entry by default. Only enable it if
	# a non-empty object is selected
	$entry delete 0 end
	$entry configure -state disabled

	# Nothing was selected
	if {![llength $select]} {
		$entry delete 0 end
		Info $oop current -1
		return
	}

	# Get the (first) row
	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]
	Info $oop current $row

	# Get the object index
	set index [lindex $Priv(index) $row]

	# Display memory for this object
	NSRecall::RecallObject $invOrEquip $index

	# Get object info
	angband $invOrEquip info $index attrib

	# Ignore non-objects in equipment
	if {$attrib(tval) ne "TV_NONE"} {

		# Display the inscription for possible editing
		$entry configure -state normal
		$entry delete 0 end
		$entry insert end [angband $invOrEquip inscription $index]
	}

	return
}

# NSInventory::ContextMenu --
#
#	When an inventory item is right-clicked, pop up a context
#	menu of actions.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::ContextMenu {oop menu x y} {

	variable Priv

	set tree [Info $oop tree]
	set where [Info $oop invOrEquip]

	set font [$menu cget -font]

	# Find the hit row
	set x1 [expr {$x - [winfo rootx $tree]}]
	set y1 [expr {$y - [winfo rooty $tree]}]
	set row [NSList::Point2Row $tree $x1 $y1]

	# Clear the menu
	$menu delete 0 end

	set askCmd 0
	set askItem 0
	if {[angband inkey_flags] eq "INKEY_CMD"} {
		set askCmd 1
	}
	if {[angband inkey_flags] eq "INKEY_ITEM"} {
		set askItem 1
	}
	if {$askCmd} {
		set closeCmd {InventoryObj Close}
		set cancelCmd {}
	} elseif {$askItem} {
		if {[Info $oop browsing]} {
			set closeCmd {
				angband keypress \033
				NSWindowManager::Undisplay inventory
			}
		} else {
			set closeCmd {angband keypress \033}
		}
		set cancelCmd {angband keypress \033}
	} else {
		set closeCmd {InventoryObj Close}
		set cancelCmd {}
	}

	# No row is hit
	if {$row == -1} {

		$menu add command -label [mc Close] -command $closeCmd
		$menu add separator
		$menu add command -label [mc Cancel] -command $cancelCmd

		# Pop up the menu
		tk_popup $menu $x $y

		# Done
		return
	}

	# Get the inventory index
	set index [lindex $Priv(index) $row]

	# Get information about this item
	angband $where info $index attrib

	# Get the item char. We can't use attrib(char) below because
	# it might be an item in a floor stack, in which case attrib(char)
	# is always 'a'.
	set itemKey [lindex $Priv(char) $row]

	# Get the tval
	set itemTval $attrib(tval)

	# Require a real item (ie, in equipment)
	if {$attrib(tval) eq "TV_NONE"} {

		$menu add command -label [mc Close] -command $closeCmd
		$menu add separator
		$menu add command -label [mc Cancel] -command $cancelCmd

		# Pop up the menu
		tk_popup $menu $x $y

		# Done
		return
	}

	# We are waiting for an item
	if {$askItem} {

		# Append a command to select the item
		set command "angband keypress $itemKey"
		$menu add command -label [mc "Select This Item"] -command $command \
			-font [BoldFont $font]
		$menu add separator
		$menu add command -label [mc Cancel] -command $cancelCmd

		# Pop up the menu
		tk_popup $menu $x $y

		# Done
		return
	}

	# We are not waiting for a command
	if {!$askCmd} return

	# Hack -- Looking at a floor stack
	if {$where eq "floor"} {

		# No commands are possible
		return
	}

	# Originally, we type 'e' to display equipment which sets command_see
	# to TRUE. Then type 'd' to drop selects from equipment. But now we
	# don't set command_see because the window can always be open. So the
	# toggle char must always be entered when acting on an equipment item.
	set command_see 0
	set toggleChar ""
	if {$command_see} {
		if {[Info $oop where] ne $where} {
			switch -- $where {
				equipment -
				inventory {
					set toggleChar /
				}
			}
		}
	} else {
		if {$where eq "equipment"} {
			if {[angband inventory count]} {
				set toggleChar /
			}
		}
	}

	# Get the default command for this item
	set list [GetItemCommand $oop $index]
	set label [lindex $list 0]
	set command [lindex $list 1]

	if {[string length $command]} {
		$menu add command -label $label -command $command -font [BoldFont $font]
	}
	if {![angband store shopping]} {
		if {$attrib(number) == 1} {
			$menu add command -label [mc "Drop"] \
				-command "DoKeymapCmd {} d $toggleChar$itemKey"
		} else {
			$menu add command -label [mc "Drop One"] \
				-command "DoKeymapCmd 01 d $toggleChar$itemKey"
			$menu add command -label [mc "Drop Some"] \
				-command "DoKeymapCmd {} d $toggleChar$itemKey"
			$menu add command -label [mc "Drop All"] \
				-command "DoKeymapCmd 0$attrib(number) d $toggleChar$itemKey"
		}
	}
	if {$where eq "inventory"} {
		$menu add command -label [mc Throw] \
			-command "DoKeymapCmd {} v $toggleChar$itemKey"
	}
	$menu add command -label [mc Inspect] \
		-command "DoKeymapCmd {} I $toggleChar$itemKey"
	$menu add command -label [mc Inscribe] \
		-command "DoKeymapCmd {} braceleft $toggleChar$itemKey"
	if {[string length [angband $where inscription $index]]} {
		$menu add command -label [mc Uninscribe] \
			-command "DoKeymapCmd {} braceright $toggleChar$itemKey"
	}

	# We are looking in the inventory
	if {$where eq "inventory"} {

		$menu add separator
		set prefix 0$attrib(number)
		set suffix $itemKey

		# Skip the y/n prompt if the user is asked to confirm
		# the destruction of worthless items.
		if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
			if {[Setting verify_destroy] &&
				([Setting verify_destroy_junk]
				|| ![angband inventory worthless $index])} {
				append suffix y
			}
		}
if 0 {  # no prompt given if quantity specified
		if {[variant ZANGBANDTK]} {
			if {!([Setting auto_destroy] && [angband inventory worthless $index])} {
				append suffix y
			}
		}
}
		$menu add command -label [mc *Destroy*] -command "DoKeymapCmd $prefix k $suffix"
	}

	$menu add separator
	$menu add command -label [mc Close] -command $closeCmd
	$menu add separator
	$menu add command -label [mc Cancel] -command $cancelCmd

	# Pop up the menu
	tk_popup $menu $x $y

	return
}

# NSInventory::SetList --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::SetList {oop invOrEquip tval {both 0}} {

	variable Priv

	set win [Info $oop win]
	set tree [Info $oop tree]

	if {[string length $invOrEquip]} {
		Info $oop invOrEquip $invOrEquip
		Info $oop tval $tval
	} else {
		set invOrEquip [Info $oop invOrEquip]
		set tval [Info $oop tval]
	}

	Info $oop both $both

	if {$both} {
		set items [angband $invOrEquip find]
	} elseif {[string length $tval]} {
		set items [angband $invOrEquip find -tester yes -tval $tval]
	} else {
		set items [angband $invOrEquip find -tester yes]
	}

	# Clear the list of char's
	set Priv(char) {}

	# Clear the list of indexes
	set Priv(index) {}

	# Clear the list
	NSList::Clear $tree

	# Total the weight
	set weightDisplayed 0

	# Options: Show icons, Show weights, Show labels (in equipment)
	set show_icons [Setting show_icons]
	set show_labels [Setting show_labels]
	set show_weights [Setting show_weights]

NSList::Config $tree -icon $show_icons
$tree style layout s[$tree column index desc] eTxt -squeeze x
NSList::ColumnShow $tree label $show_labels
NSList::ColumnShow $tree weight $show_weights

$tree style layout s4 eTxt -padx {20 0}

	# Add each item
	foreach index $items {

		set attrib(label) ""
		angband $invOrEquip info $index attrib

		# Get the (optional) icon
		set icon $attrib(icon)
		if {!$show_icons} {set icon ""}

		# Hack -- Set index for floor item
		if {$invOrEquip eq "floor"} {
			set attrib(char) [string index "abcdefghijklmnopqrstuvw" \
				[lsearch -integer $items $index]]
		}

		# Use the entire description. It is "truncated" with an opaque item
		set desc $attrib(name)

		# Get the (optional) label
		set label $attrib(label)
		if {!$show_labels} {set label ""}

		# Get the (optional) weight
		set weight [expr {$attrib(weight) * $attrib(number)}]
		if {!$show_weights} {set weight ""}

set item [$tree item create]
if {$icon ne ""} {
	NSList::SetIcon $tree $item $icon
}
NSList::SetText $tree $item "$attrib(char)\) "
if {$label ne ""} {
	NSList::SetTextEx $tree $item 1 $label
	NSList::SetTextEx $tree $item 2 " : "
}
NSList::SetTextEx $tree $item desc $desc
NSList::SetTextFillEx $tree $item desc [default_tval_to_attr $attrib(tval)]
if {$weight ne ""} {
	NSList::SetTextEx $tree $item weight [fmt_wgt $weight 1]
}
$tree item lastchild root $item

		# Total the weight
		incr weightDisplayed [expr {$attrib(weight) * $attrib(number)}]

		# Remember the char
		lappend Priv(char) $attrib(char)

		# Remember the index
		lappend Priv(index) $index
	}

	# Display weight of inventory items, % of capacity, and weight limit
	set weightTotal [angband inventory total_weight]
	set weightLimit [angband inventory weight_limit]
	set capacity [expr {$weightLimit / 2 + $weightLimit / 10}]

	$win.statusBar itemconfigure t1 -text "[mc Displayed] [fmt_wgt $weightDisplayed 1]"
	$win.statusBar itemconfigure t2 -text "[mc Total] [fmt_wgt $weightTotal 1]"
	$win.statusBar itemconfigure t3 -text "[mc Threshold] [fmt_wgt $capacity 1]"
	$win.statusBar itemconfigure t4 -text "[mc Limit] [fmt_wgt $weightLimit 1]"
	set numItems [llength $items]
	if {$numItems == 1} {
		$win.statusBar itemconfigure t5 -text [format [mc "%d item"] $numItems]
	} else {
		$win.statusBar itemconfigure t5 -text [format [mc "%d items"] $numItems]
	}

	# Set window title
	set title [string totitle $invOrEquip]
	wm title $win [mc $title]

	return
}

# NSInventory::CommitInscription --
#
#	Set the inscription of the selected item to the string in the
#	inscription Entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::CommitInscription {oop} {

	variable Priv

	set tree [Info $oop tree]
	set entry [Info $oop inscription,entry]
	set row [Info $oop current]

	# Get the object index
	set index [lindex $Priv(index) $row]

	# Hack -- Set the inscription.
	angband [Info $oop invOrEquip] inscription $index [$entry get]

	# Get object info
	angband [Info $oop invOrEquip] info $index attrib

	# Update the list. If there was another inventory window it would
	# also need to be updated!
	$tree item text "root child $row" desc $attrib(name)

	# Hack -- Focus on the list again
	focus $tree

	return
}

# NSInventory::Track --
#
#	Handle <Track> quasi-event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::Track {oop what} {

	switch -- $what {
		equipment -
		inventory {
dbwin "NSInventory::Track $what\n"
			if {[Info $oop invOrEquip] eq $what} {
				SetList $oop "" ""
			}
		}
	}

	return
}

# NSInventory::ChooseItem --
#
#	Handle <Choose-item> quasi-event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::ChooseItem {oop show other} {

dbwin "NSInventory::ChooseItem $show $other\n"

	Info $oop choose,show $show
	Info $oop choose,other $other
	if {$show} {
		Info $oop didChoose 0
	}

	qeconfigure NSInventory <Term-inkey> -active yes

	return
}

# NSInventory::TermInkey --
#
#	Handle <Term-inkey> quasi-event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::TermInkey {oop} {

dbwin "NSInventory::TermInkey\n"

	set win [Info $oop win]

	qeconfigure NSInventory <Term-inkey> -active no

	if {[Info $oop choose,show]} {
		SetList $oop [Info $oop choose,other] ""
		if {![Info $oop alwaysOnTop]} {
			if {![NSUtils::ToplevelHasFocus $win]} {
				Info $oop oldFocus [focus]
dbwin "oldFocus == [focus]\n"
				WindowBringToFront $win
			}
		}
		Info $oop didChoose 1
	} elseif {![Info $oop didChoose]} {
dbwin "NSInventory::TermInkey SKIPPED\n"
		# Nothing
	} else {
		if {![Info $oop alwaysOnTop]} {
			set oldFocus [Info $oop oldFocus]
			if {[string length $oldFocus] &&
				![string match $win* $oldFocus]} {
				catch {focus $oldFocus}
			}
			update
		}
		SetList $oop "" "" 1
	}

	return
}

# NSInventory::Swap --
#
#	Display the "new" Inventory Window
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::Swap {oop} {

	set browsing [Info $oop browsing]
	set where [Info $oop invOrEquip]
	set both [Info $oop both]

	NSWindowManager::Undisplay inventory

	NSModule::LoadIfNeeded NSInventory2
	NSWindowManager::Display inventory2 $where "" $both
	if {[Info $oop choose,show]} {
		Inventory2Obj Info browsing $browsing
		Inventory2Obj Info choose,show 1
		Inventory2Obj Info choose,other $where
		Inventory2Obj Info didChoose 1
		if {![string match [Info $oop win]* [Info $oop oldFocus]]} {
			Inventory2Obj Info oldFocus [Info $oop oldFocus]
		} else {
			Inventory2Obj Info oldFocus [Inventory2Obj Info win]
		}
	}

	Value inventory,style new

	return
}

# NSInventory::AlwaysOnTop --
#
#	Toggle the "Always On Top" option.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::AlwaysOnTop {oop} {

	set win [Info $oop win]
	set onTop [Info $oop alwaysOnTop]

	if {[Platform unix]} {
		wm state $win withdrawn
	}

	if {$onTop} {
		NSMainWindow::TransientToMain $win
	} else {
		wm transient $win ""
		raise $win

		if {[string compare [info patchlevel] 8.3.3] < 0} {
			# Hack -- Set the application icon again
			angband system windowicon $win
		}
	}

	if {[Platform unix]} {
		wm state $win normal
	}

	after idle focus $win

	Value inventory,alwaysOnTop $onTop

	# Synchronize the graphical inventory window
	if {[info exists ::Windows(inventory2)] && [winfo exists [Window inventory2]]} {
		if {$onTop} {
			wm transient [Window inventory2] [Window main]

			# Work around a Tk bug (can't remember actual version)
			if {[string compare [info patchlevel] 8.3.3] < 0} {
				wm withdraw [Window inventory]
			}
		} else {
			wm transient [Window inventory2] ""
		}
		Inventory2Obj Info alwaysOnTop $onTop
	}

	return
}

# NSInventory::IconCfg --
#
#	Called when the game's icon configuration changes
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::IconCfg {oop} {

	if {[winfo ismapped [Info $oop win]]} {
		SetList $oop "" "" [Info $oop both]
	}

	return
}

proc InventoryObj {command args} {
	return [NSInventory::$command [Global inventory,oop] {*}$args]
}
