# File: inventory.tcl

# Purpose: the Inventory Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
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

	InitImageIfNeeded Image_ButtonOptions button-options.gif
	InitImageIfNeeded Image_ButtonHelp button-help.gif

	NSModule::LoadIfNeeded NSStatusBar
	NSModule::LoadIfNeeded NSToolbar

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
	Info $oop skipHide 0

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow inventory $win \
		"GetDefaultGeometry $win main2 main" "" \
		"NSInventory::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSInventory $oop $win

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

	NSValueManager::RemoveClient font,inventory [Info $oop font,clientId]
	NSValueManager::RemoveClient listBG [Info $oop listBG,clientId]

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

	if {[Info $oop alwaysOnTop]} {
		wm transient $win [Window main]
	}

	# Start out withdrawn (hidden)
	wm withdraw $win

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
	# List
	#

	set cw [font measure [Value font,inventory] "W"]
	set width [expr {$cw * 81}]

	frame $win.frame \
		-borderwidth 1 -relief sunken
	set canvistId [NSObject::New NSCanvist $win.frame 40 $width 300 \
		"NSInventory::NewItemCmd $oop" "NSInventory::HighlightItemCmd $oop"]
	set canvas [NSCanvist::Info $canvistId canvas]
	$canvas configure -background [Value listBG]
	$canvas configure -yscrollcommand "$win.frame.scroll set"
	scrollbar $win.frame.scroll \
		-command "$canvas yview" -orient vert

	# When the window resizes, reposition the canvas items
	bind $canvas <Configure> \
		"NSInventory::PositionItems $oop"

	Info $oop canvistId $canvistId
	Info $oop canvas $canvas

	pack $win.frame.scroll -side right -fill y
	pack $canvas -side left -expand yes -fill both

	# Double-click to select item
	NSCanvist::Info $canvistId invokeCmd \
		"NSInventory::Invoke $oop"

	# Update ourself when the font,inventory value changes
	Info $oop font,clientId \
		[NSValueManager::AddClient font,inventory \
		"NSInventory::ValueChanged_font_inventory $oop"]

	# This call updates the list background color whenever the
	# global list background color changes
	Info $oop listBG,clientId \
		[NSValueManager::AddClient listBG \
		"NSInventory::ValueChanged_listBG $oop"]

	#
	# Statusbar
	#

	statusbar $win.statusBar -sizes {18 14 18 15 12} -weights {0 0 0 1 0} \
		-tags {t1 t2 t3 t4 t5}

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
	bind $canvas <ButtonPress-3> "NSInventory::ContextMenu $oop $m %X %Y"

	#
	# Feed Term when keys pressed
	#

	Term_KeyPress_Bind $win
	Term_KeyPress_Bind $canvas

	#
	# Synch the scrollbars when window is shown.
	#

	NSUtils::SynchScrollBar $canvas $win.frame.scroll 1

	bind $win <FocusIn> "
		if {\[string equal %W $win]} {
			focus $canvas
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

	switch -- $message {
		preDisplay {
			eval SetList $oop $args
			if {([llength $args] == 3) && [lindex $args 2]} {
				Info $oop browsing 1
			}
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
					eval SetList $oop $args
				}
			} else {
				eval SetList $oop $args
			}
			if {![Info $oop alwaysOnTop]} {
				WindowBringToFront [Info $oop win]
			}
		}
		postWithdraw {
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

	if {[string equal [angband inkey_flags] INKEY_ITEM]} {
		angband keypress \033
	} elseif {[Info $oop skipHide]} {
		set oldFocus [Info $oop oldFocus]
		if {[string length $oldFocus]} {
			catch {focus $oldFocus}
		}
		Info $oop skipHide 0
	} elseif {[Info $oop browsing]} {
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

	$menu add separator
	lappend descList ""

	$menu add checkbutton -label "Always On Top" \
		-command {InventoryObj AlwaysOnTop} \
		-variable ::NSInventory($oop,alwaysOnTop)
	lappend keywordList ""
	lappend descList "Keep the window on top of the Main Window"

	$menu add command -label "Graphics Mode" -command "InventoryObj Swap"
	lappend keywordList ""
	lappend descList "Use the graphical window"

	$menu add separator
	$menu add command -label "Set Font" -command {
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
		tkwait variable ::tkPriv(popup)
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

	SetList $oop {} {} [Info $oop both]

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

proc NSInventory::GetItemCommand {oop where index _command _label} {

	upvar $_command command $_label label

	set command ""
	set label ""

	angband $where info $index attrib
	set charItem $attrib(char)

	if {[string equal $where equipment]} {
		if {$attrib(known) && $attrib(activate)} {
			set label "Activate"
			set charCmd A
		} else {
			set label "Remove"
			set charCmd t
		}
		set command "DoKeymapCmd {} $charCmd $charItem"
		return
	}
	
	switch -glob -- $attrib(tval) {
		*_BOOK {
			set label "Browse"
			set charCmd b

			# Hack -- Browse shows all the books
			set command "DoKeymapCmd {} $charCmd {}"
			return
		}
		TV_ARROW -
		TV_BOLT -
		TV_SHOT {
			set label "Fire"
			set charCmd f
		}
		TV_FLASK {
			set label "Refuel"
			set charCmd F
		}
		TV_FOOD {
			set label "Eat"
			set charCmd E
		}
		TV_POTION {
			set label "Drink"
			set charCmd q
		}
		TV_SCROLL {
			set label "Read"
			set charCmd r
		}
		TV_SPIKE {
			set label "Jam"
			set charCmd j
		}
		TV_STAFF {
			set label "Use"
			set charCmd u
		}
		TV_ROD {
			set label "Zap"
			set charCmd z
		}
		TV_WAND {
			set label "Aim"
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
			set label "Wield"
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
#	When an inventory item is double-clicked, "angband keypress" the
#	char.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::Invoke {oop canvistId x y} {

	variable Priv

	set row [NSCanvist::PointToRow $canvistId $x $y]
	if {$row == -1} return

	set invOrEquip [Info $oop invOrEquip]
	set char [lindex $Priv(char) $row]
	set index [lindex $Priv(index) $row]

	if {[string equal [angband inkey_flags] INKEY_CMD]} {
		GetItemCommand $oop $invOrEquip $index command label
		if {[string length $command]} {
			eval $command
		}
		return
	}

	if {[string equal [angband inkey_flags] INKEY_ITEM]} {
		angband keypress $char
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

	set canvistId [Info $oop canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]
	set where [Info $oop invOrEquip]

	set font [$menu cget -font]

	# Find the hit row
	set x1 [expr {$x - [winfo rootx $canvas]}]
	set y1 [expr {$y - [winfo rooty $canvas]}]
	set row [NSCanvist::PointToRow $canvistId $x1 $y1]

	# Clear the menu
	$menu delete 0 end

	set askCmd 0
	set askItem 0
	if {[string equal [angband inkey_flags] INKEY_CMD]} {
		set askCmd 1
	}
	if {[string equal [angband inkey_flags] INKEY_ITEM]} {
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

		$menu add command -label "Close" -command $closeCmd
		$menu add separator
		$menu add command -label "Cancel" -command $cancelCmd

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
	if {[string equal $attrib(tval) TV_NONE]} {

		$menu add command -label "Close" -command $closeCmd
		$menu add separator
		$menu add command -label "Cancel" -command $cancelCmd

		# Pop up the menu
		tk_popup $menu $x $y

		# Done
		return
	}

	# We are waiting for an item
	if {$askItem} {

		# Append a command to select the item
		set command "angband keypress $itemKey"
		$menu add command -label "Select This Item" -command $command \
			-font [BoldFont $font]
		$menu add separator
		$menu add command -label "Cancel" -command $cancelCmd

		# Pop up the menu
		tk_popup $menu $x $y

		# Done
		return
	}

	# We are not waiting for a command
	if {!$askCmd} return

	# Hack -- Looking at a floor stack
	if {[string equal $where floor]} {

		# No commands are possible
		return
	}

	# Get the default command for this item
	GetItemCommand $oop $where $index command label

	if {[string length $command]} {
		$menu add command -label $label -command $command -font [BoldFont $font]
	}
	$menu add command -label "Drop" \
		-command "DoKeymapCmd {} d $itemKey"
	$menu add command -label "Inspect" \
		-command "DoKeymapCmd {} I $itemKey"
	$menu add command -label "Inscribe" \
		-command "DoKeymapCmd {} braceleft $itemKey"

	# We are looking in the inventory
	if {[string equal $where inventory]} {

		$menu add separator
		set command "angband keypress 0$attrib(number)[angband keymap find k]$itemKey"

		# Skip the y/n prompt if the user is asked to confirm
		# the destruction of worthless items.
		$menu add command -label "*Destroy*" -command $command
	}

	$menu add separator
	$menu add command -label "Close" -command $closeCmd
	$menu add separator
	$menu add command -label "Cancel" -command $cancelCmd

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
	set canvistId [Info $oop canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]

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
	} else {
		set items [angband $invOrEquip find -tester yes]
	}

	# Clear the list of char's
	set Priv(char) {}

	# Clear the list of indexes
	set Priv(index) {}

	# Clear the list
	NSCanvist::DeleteAll $canvistId
	
	# Total the weight
	set weightDisplayed 0

	# Calculate the row height.  This is done every time here since
	# the font may change.  The row height equals the linespace of
	# the font plus 8 pixels for the selection rectangle, or the
	# icon size plus 8, whichever is greater.
	set rowHgt [font metrics [Value font,inventory] -linespace]

	if {[icon size] > $rowHgt} {
		set rowHgt [icon size]
	}

	# Leave room for the selection rectangle on each line
	incr rowHgt 8
	
	# Set the row height	
	NSCanvist::Info $canvistId rowHgt $rowHgt
	$canvas configure -yscrollincrement $rowHgt

	# Options: Show weights, Show labels (in equipment)
	set show_labels [Setting show_labels]

	set Priv(width,char) 0
	set Priv(width,label) 0
	set Priv(width,desc) 0
	set Priv(width,weight) 0

	set itemList {}
	
	# Add each item
	foreach index $items {

		set attrib(label) ""
		angband $invOrEquip info $index attrib

		# Get the (optional) icon
		set icon $attrib(icon)

		# Hack -- Set index for floor item
		if {[string equal $invOrEquip floor]} {
			set attrib(char) [string index "abcdefghijklmnopqrstuvw" \
				[lsearch -exact $items $index]]
		}

		# Use the entire description. It is "truncated" with an opaque item
		set desc $attrib(name)

		# Get the (optional) label
		set label $attrib(label)
		if {!$show_labels} {set label ""}

		# Get the (optional) weight
		set weight [expr {$attrib(weight) * $attrib(number)}]

		lappend itemList [list $attrib(char) \
			$attrib(number) $desc $weight $label \
			$attrib(tval) $attrib(sval) $icon]
			
		# Total the weight
		incr weightDisplayed [expr {$attrib(weight) * $attrib(number)}]

		# Remember the char
		lappend Priv(char) $attrib(char)

		# Remember the index
		lappend Priv(index) $index
	}

	# Append the descriptions
	NSCanvist::InsertMany $canvistId end $itemList

	# Arrange all the items
	PositionItems $oop

	# Display weight of inventory items, % of capacity, and weight limit
	set units lb
	set weightTotal [angband inventory total_weight]
	set weightLimit [angband inventory weight_limit]
	set displayed [format "%d.%d $units" [expr {$weightDisplayed / 10}] [expr {$weightDisplayed % 10}]]
	set total [format "%d.%d $units" [expr {$weightTotal / 10}] [expr {$weightTotal % 10}]]
	set capacity [expr {$weightLimit / 2 + $weightLimit / 10}]
	set capacity [format "%d.%d $units" [expr {$capacity / 10}] [expr {$capacity % 10}]]
	set limit [format "%d.%d $units" [expr {$weightLimit / 10}] [expr {$weightLimit % 10}]]

	$win.statusBar itemconfigure t1 -text "Displayed $displayed"
	$win.statusBar itemconfigure t2 -text "Total $total"
	$win.statusBar itemconfigure t3 -text "Threshold $capacity"
	$win.statusBar itemconfigure t4 -text "Limit $limit"
	set numItems [llength $items]
	if {$numItems == 1} {
		$win.statusBar itemconfigure t5 -text [format "%d item" $numItems]
	} else {
		$win.statusBar itemconfigure t5 -text [format "%d items" $numItems]
	}

	# Set window title
	set title [string totitle $invOrEquip]
	wm title $win $title
	
	return
}

# NSInventory::NewItemCmd --
#
#	Called by NSCanvist::InsertItem() to create a list row.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

# The icon, char and weight are positioned exactly
# The label, : and name are positioned after all items are gathered

proc NSInventory::NewItemCmd {oop canvistId y char number text weight label tval sval icon} {

	variable Priv

	set canvas [NSCanvist::Info $canvistId canvas]
	set lineHeight [NSCanvist::Info $canvistId rowHgt]
	set font [Value font,inventory]

	set fh [font metrics $font -linespace]
	set diff [expr {int([expr {($lineHeight - $fh) / 2}])}]

	set offset [expr {[icon size] + 8}]

if 0 {

	# Image
	if {[string length $icon]} {
		set iw [icon size]
		set ih [icon size]
		set wid [expr {[icon size] + 8}]
		set xdiff [expr {int([expr {($wid - $iw) / 2}])}]
		set ydiff [expr {int([expr {($lineHeight - $ih) / 2}])}]
		lappend itemIdList [$canvas create widget $xdiff [expr {$y + $ydiff}] \
			 -assign $icon]
	}
}

	# Char
	lappend itemIdList [$canvas create text $offset [expr {$y + $diff}] \
		-text $char) -anchor nw -font $font -fill White -tags text]

	# Label and : (in equipment)
	if {[string length $label]} {

		# Label
		lappend itemIdList [$canvas create text 0 [expr {$y + $diff}] \
			-text $label -anchor nw -font $font -fill White -tags {text label}]

		# The : are all lined up
		lappend itemIdList [$canvas create text 0 [expr {$y + $diff}] \
			-text : -anchor nw -font $font -fill White -tags {text semicolon}]
	}

	# Description
	set fill [default_tval_to_attr $tval]
	lappend itemIdList [$canvas create text 0 [expr {$y + $diff}] \
		-text $text -anchor nw -font $font -fill $fill \
		-tags "text enabled hilite fill:$fill description"]

	# This item "truncates" long descriptions
	set listBG [Value listBG]
	lappend itemIdList [$canvas create rectangle 0 $y \
		1 [expr {$y + $lineHeight}] -fill $listBG -outline $listBG \
		-tags truncate]
	
	# Weight
	if {[string length $weight]} {
		set weight [format "%d.%d lb" [expr {$weight / 10}] [expr {$weight % 10}]]
		lappend itemIdList [$canvas create text 0 [expr {$y + $diff}] \
			-text $weight -anchor ne -justify right -font $font -fill White \
			-tags weight]
	}

	# Selection rectangle inside row
	lappend itemIdList [$canvas create rectangle 2 [expr {$y + 2}] \
		2 [expr {$y + $lineHeight - 2}] -fill "" -outline "" \
		-tags {enabled selrect} -width 2.0]

	# Maximum width of char
	set width [font measure $font "$char) "]
	if {$width > $Priv(width,char)} {
		set Priv(width,char) $width
	}

	# Maximum width of label
	if {[string length $label]} {
		set width [font measure $font "${label}A"]
		if {$width > $Priv(width,label)} {
			set Priv(width,label) $width
		}
	}

	# Maximum width of description
	set width [font measure $font $text]
	if {$width > $Priv(width,desc)} {
		set Priv(width,desc) $width
	}

	# Maximum width of weight
	if {[string length $weight]} {
		set width [font measure $font "AB$weight"]
		if {$width > $Priv(width,weight)} {
			set Priv(width,weight) $width
		}
	}

	return $itemIdList
}

# NSInventory::PositionItems --
#
#	Arranges all the canvas items in the list. This is called after all
#	the items are added, and when the <Configure> event indicates the
#	window has been resized. This is the routine that lets variable-width
#	fonts work.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::PositionItems {oop} {

	variable Priv

	set canvistId [Info $oop canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]

	# Nothing to do
	if {![NSCanvist::Info $canvistId count]} return

	# Get the width of the canvas
	set canvasWidth [winfo width $canvas]
	
	set offset [expr {[icon size] + 8}]

	incr offset $Priv(width,char)

	# Position each label and semicolon
	if {$Priv(width,label)} {

		# Position each label
		set coords [$canvas coords label]
		$canvas move label [expr {$offset - [lindex $coords 0]}] 0
		incr offset $Priv(width,label)

		# Position each semicolon
		set coords [$canvas coords semicolon]
		$canvas move semicolon [expr {$offset - [lindex $coords 0]}] 0
		incr offset [font measure [Value font,inventory] ":A"]
	}
	
	# Position each description
	set coords [$canvas coords description]
	$canvas move description [expr {$offset - [lindex $coords 0]}] 0

	# Truncate each description (by positioning each "truncate" item)
	set x0 [expr {($canvasWidth - 1) - $Priv(width,weight) - 4}]
	set x1 $canvasWidth
	foreach itemId [$canvas find withtag truncate] {
		scan [$canvas coords $itemId] "%s %s %s %s" c0 c1 c2 c3
		$canvas coords $itemId $x0 $c1 $x1 $c3
	}

	# Position each weight
	if {$Priv(width,weight)} {
		set offset [expr {($canvasWidth - 1) - 4}]
		set coords [$canvas coords weight]
		$canvas move weight [expr {$offset - [lindex $coords 0]}] 0
	}

	# Position each selection rectangle
	set x1 [expr {($canvasWidth - 1) - 2}]
	foreach itemId [$canvas find withtag selrect] {
		scan [$canvas coords $itemId] "%s %s %s %s" c0 c1 c2 c3
		$canvas coords $itemId $c0 $c1 $x1 $c3
	}

	# Set the scrollregion
	scan [$canvas cget -scrollregion] "%s %s %s %s" x1 y1 x2 y2
	$canvas configure -scrollregion "$x1 $y1 $canvasWidth $y2"

	return
}

# NSInventory::HighlightItemCmd --
#
#	Called by NSCanvist::Select() to highlight a row.
#
# Arguments:
#	oop						OOP ID. See above.
#	canvistId				OOP ID of NSCanvist object.
#	state					1 or 0 highlight state.
#	args					List of canvas item ids
#
# Results:
#	What happened.

proc NSInventory::HighlightItemCmd {oop canvistId state args} {

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
		$canvas itemconfigure $idRect -outline ""
	}

	return
}


# NSInventory::ValueChanged_font_inventory --
#
#	Called when the font,inventory value changes.
#	Updates the Inventory Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::ValueChanged_font_inventory {oop} {

	if {![winfo ismapped [Info $oop win]]} return
	SetList $oop "" ""
	
	return
}

# NSInventory::ValueChanged_listBG --
#
#	Called when the listBG value changes.
#	Updates the Inventory Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory::ValueChanged_listBG {oop} {

	set canvistId [Info $oop canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]

	set color [Value listBG]
	$canvas configure -background $color
	$canvas itemconfigure truncate -fill $color -outline $color

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
			if {[string equal [Info $oop invOrEquip] $what]} {
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

	Info $oop choose,show $show
	Info $oop choose,other $other
	if {$show} {
		Info $oop didChoose 0
	}

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

	set win [Info $oop win]

	if {[Info $oop choose,show]} {
		SetList $oop [Info $oop choose,other] ""
		if {![Info $oop alwaysOnTop]} {
			if {![NSUtils::ToplevelHasFocus $win]} {
				Info $oop oldFocus [focus]
				WindowBringToFront $win
			}
		}
		Info $oop didChoose 1
	} elseif {![Info $oop didChoose]} {
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
		wm transient $win [Window main]
	} else {
		wm transient $win ""
		raise $win
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
		} else {
			wm transient [Window inventory2] ""
		}
		Inventory2Obj Info alwaysOnTop $onTop
	}

	return
}

proc InventoryObj {command args} {
	return [eval NSInventory::$command [Global inventory,oop] $args]
}
