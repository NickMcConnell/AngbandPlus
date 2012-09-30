# File: store.tcl

# Purpose: the Store Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSStore {

	variable MenuString
	variable Priv

# namespace eval NSStore
}

# NSStore::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::InitModule {} {

	InitImageIfNeeded Image_ButtonOptions button-options.gif
	InitImageIfNeeded Image_ButtonHelp button-help.gif

	NSModule::LoadIfNeeded NSToolbar
	
	NSObject::New NSStore

	return
}

# NSStore::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::CloseModule {} {

	catch {
		destroy [Window store]
	}

	return
}

# NSStore::NSStore --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::NSStore {oop} {

	Info $oop current -1
	Info $oop toolbar,match {}

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow store $win \
		"GetDefaultGeometry $win main2 main" "NSStore::SetupCmd $oop" \
		"NSStore::DisplayCmd $oop"

	#
	# Global list of application windows
	#

	Global store,oop $oop
	Window store $win

	return
}

# NSStore::~NSStore --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::~NSStore {oop} {

	NSValueManager::RemoveClient listBG [Info $oop clientId,listBG]
	trace vdelete NSStore($oop,quantity) w "::NSStore::QuantityChanged $oop"

	return
}

# NSStore::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::Info {oop info args} {

	global NSStore

	# Verify the object
	NSObject::CheckObject NSStore $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSStore($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSStore($oop,$info)
			}
		}
	}

	return
}

# NSStore::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::InitWindow {oop} {

	global NSToolbar

	set win .store$oop
	toplevel $win
	wm title $win Store

	wm transient $win [Window main]

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSStore::Close $oop"

	# Start out withdrawn (hidden)
	wm withdraw $win

	Info $oop win $win

	InitMenus $oop

	#
	# Toolbar
	#

	set toolId [NSObject::New NSToolbar 20 $win]
	NSToolbar::AddTool $toolId -image Image_ButtonOptions \
		-showlabel no -command "DoCommandIfAllowed =" -hasmenu yes \
		-menucommand "NSStore::Win98MenuCmd_Options $oop"
	NSToolbar::AddTool $toolId -image Image_ButtonHelp \
		-showlabel no -command "DoCommandIfAllowed ?"

	MakeDivider [NSToolbar::Info $toolId frame].divider1 y
	pack [NSToolbar::Info $toolId frame].divider1 -side left -fill y -pady 2
	
	set id [NSToolbar::AddTool $toolId -image Image_ButtonHelp \
		-command "DoCommandIfAllowed p" -label "Buy" -showlabel yes \
		-showimage no -heightlabel 16 -hasmenu yes \
		-menucommand "NSStore::Win98MenuCmd_Buy $oop"]
	
	set id2 [NSToolbar::AddTool $toolId -image Image_ButtonHelp \
		-command "DoCommandIfAllowed s" -label "Sell" -showlabel yes \
		-showimage no -heightlabel 16 -hasmenu yes \
		-menucommand "NSStore::Win98MenuCmd_Sell $oop"]
	
	set menuId [NSObject::New NSMenu [Info $oop mbarId] \
		-tearoff 0 -identifier MENU_TOOLBAR -postcommand ";"]
	set menu [NSMenu::Info $menuId menu]
	if {[Platform unix]} {
		$menu configure -cursor arrow
	}
	Info $oop toolbar,menu $menu

	Info $oop toolbarId $toolId
	Info $oop toolbar,buyId $id
	Info $oop toolbar,sellId $id2

	#
	# Divider + Store Info
	#

	if {[Platform unix]} {
		set font {Courier 12}
	}
	if {[Platform windows]} {
		set font {Courier 9}
	}

	set frame $win.info
	frame $frame \
		-borderwidth 0
#	frame $frame.divider1 \
#		-borderwidth 1 -height 2 -relief groove
	label $frame.howMany \
		-font $font -text "Quantity:"
	entry $frame.quantity \
		-width 2 -state disabled -textvariable NSStore($oop,quantity)
	label $frame.howMuch \
		-font $font -text "Total Cost:"
	label $frame.totalCost \
		-font $font -text "" -anchor w
	label $frame.purse \
		-font $font
	label $frame.playerGold \
		-font $font -text "Gold Remaining:"
	label $frame.gold \
		-font $font -text [angband player gold]

	label $frame.price_character \
		-font $font -text "" -width 25 -anchor w
	label $frame.price_owner \
		-font $font -text "" -width 25 -anchor w

	global NSStore
	trace variable NSStore($oop,quantity) w "::NSStore::QuantityChanged $oop"

	# This stops keys being fed to the Term
#	bindtags $frame.quantity "$frame.quantity Entry all"

	MakeDivider $win.divider2 x

	#
	# List
	#

	set cw [font measure [Value font,store] "W"]
	set width [expr {$cw * 81}]
	set iconSize [expr {[icon size] + 8}]

	frame $win.frame \
		-borderwidth 1 -relief sunken
	set canvistId [NSObject::New NSCanvist $win.frame $iconSize $width 300 \
		"NSStore::NewItemCmd $oop" "NSStore::HighlightItemCmd $oop"]
	set canvas [NSCanvist::Info $canvistId canvas]
	$canvas configure -background [Value listBG]
	$canvas configure -yscrollcommand "$win.frame.scroll set"
	scrollbar $win.frame.scroll \
		-command "$canvas yview" -orient vert

	Info $oop clientId,listBG \
		[NSValueManager::AddClient listBG \
		"NSStore::ValueChanged_listBG $oop"]

	# When the window resizes, reposition the canvas items
	bind $canvas <Configure> \
		"NSStore::PositionItems $oop"

	Info $oop canvistId $canvistId

	pack $win.frame.scroll -side right -fill y
	pack $canvas -side left -expand yes -fill both

	# When an item is selected, recall it
	NSCanvist::Info $canvistId selectionCmd \
		"NSStore::SelectionChanged $oop"

	# Double-click to purchase an item
	NSCanvist::Info $canvistId invokeCmd \
		"NSStore::Invoke $oop"

	# Typing Enter in the Quantity Entry initiates a purchase
	bind $frame.quantity <KeyPress-Return> \
		"NSStore::InvokeByReturn $oop"

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 12

	#
	# Geometry
	#

	grid rowconfig $win 0 -weight 0 -minsize 0
	grid rowconfig $win 1 -weight 0 -minsize 0
	grid rowconfig $win 2 -weight 0 -minsize 0
	grid rowconfig $win 3 -weight 1 -minsize 0
	grid rowconfig $win 4 -weight 0 -minsize 0
	grid columnconfig $win 0 -weight 1 -minsize 0

	pack forget $NSToolbar($toolId,frame)
	grid $NSToolbar($toolId,frame) -in $win \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.info -in $win \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.divider2 -in $win \
		-row 2 -column 0 -rowspan 1 -columnspan 2 -sticky ew
	grid $win.frame -in $win \
		-row 3 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.statusBar -in $win \
		-row 4 -column 0 -rowspan 1 -columnspan 2 -sticky ew

	#
	# Context Menu
	#
	
	set m $win.context
	menu $m -tearoff 0
	bind $canvas <Button-3> \
		"NSStore::ContextMenu $oop $m %X %Y"

	#
	# Feed Term when keys pressed
	#

	Term_KeyPress_Bind $win
	Term_KeyPress_Bind $canvas

	# XXX Hack -- Don't feed Tab to the Term
	bind $canvas <KeyPress-Tab> {
		focus [tk_focusNext %W]
		break
	}
	
	#
	# Synch the scrollbars when window is shown.
	#

#	bind $win.frame.scroll <Map> "NSStore::SynchScrollBars $oop"
	NSUtils::SynchScrollBar $canvas $win.frame.scroll

	bind $win <FocusIn> "
		if {\[string equal %W $win]} {
			focus $canvas
		}
	"

	# Destroy the object along with the widget (later)
	NSUtils::DestroyObjectWithWidget NSStore $oop $win
	
	return
}

# NSStore::InitMenus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::InitMenus {oop} {

	variable MenuString

	set win [Info $oop win]

	#
	# Menu bar
	#

	set mbar [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSStore::SetupMenus $oop" -identifier MENUBAR]
	Info $oop mbarId $mbar

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSStore::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSStore::MenuInvoke $oop"

	#
	# Store Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_STORE
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_STORE -label "Store" -underline 0 -identifier M_STORE

	set entries {}
	lappend entries [list -type command -label "Buy" -identifier E_STORE_BUY]
	lappend entries [list -type command -label "Sell" -identifier E_STORE_SELL]
	lappend entries [list -type separator -identifier E_SEP_1]
	lappend entries [list -type separator]
	lappend entries [list -type command -label "Leave" \
		-identifier E_STORE_EXIT]

	NSMenu::MenuInsertEntries $mbar -end MENU_STORE $entries

	set MenuString(M_STORE) \
		"Contains store-related commands."
	set MenuString(E_STORE_BUY,home) \
		"Take an item from the Home."
	set MenuString(E_STORE_BUY,store) \
		"Purchase an item from the store."
	set MenuString(E_STORE_SELL,home) \
		"Drop an item in the Home."
	set MenuString(E_STORE_INSPECT) \
		"Examine the properties of an item."
	set MenuString(E_STORE_SELL,store) \
		"Sell an item to the store."

	set MenuString(E_DESTROY) \
		"Destroy an item in the Home."

	set MenuString(M_SELL,home) \
		"Contains a list of inventory items to drop."
	set MenuString(M_SELL,store) \
		"Contains a list of inventory items the store will buy."
	set MenuString(MENU_SELL,home) \
		"Drop this item."
	set MenuString(MENU_SELL,store) \
		"Sell this item."

	return
}

# NSStore::Synch --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::Synch {oop} {

	set toolbarId [Info $oop toolbarId]

	# Take/Buy button
	set state normal
	if {[angband store ishome]} {
		set label Take
		if {![angband store count]} {
			set state disabled
		}
	} else {
		set label Buy
	}
	set id [Info $oop toolbar,buyId]
	[NSToolbar::GetTool $toolbarId $id] configure -state $state \
		-label $label

	# Drop/Sell button
	if {[angband store ishome]} {
		set label Drop
	} else {
		set label Sell
	}
	set state disabled
	set items [angband inventory find -store_will_buy yes]
	if {[llength $items]} {
		set state normal
	}
	set id [Info $oop toolbar,sellId]
	[NSToolbar::GetTool $toolbarId $id] configure -state $state \
		-label $label

	return
}

# NSStore::SetupMenus --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::SetupMenus {oop mbarID} {

	variable Priv

	set identList {}

	if {[string equal [angband inkey_flags] INKEY_CMD]} {
		lappend identList E_STORE_BUY E_STORE_SELL E_STORE_EXIT
		if {[angband store ishome]} {
			lappend identList E_DESTROY
		}
	}

	NSMenu::MenuEnable $mbarID $identList

	[Info $oop win].statusBar cover show

	return
}

# NSStore::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::MenuSelect {oop menuId index ident} {

	variable MenuString
	variable Priv

	switch -- $ident {
		{} {
			set desc {}
		}
		E_STORE_EXIT {
			if {[angband store ishome]} {
				set desc "Leave the Home."
			} else {
				set desc "Leave the [angband store storename]."
			}
		}
		MENU_SELL {
			set menu [NSMenu::Info $menuId menu]
			if {$index == [$menu index end]} {
				set desc "Do nothing."
			} else {
				if {[angband store ishome]} {
					set desc $MenuString($ident,home)
				} else {
					set desc $MenuString($ident,store)
				}
				set index [lindex [Info $oop sellMenu,match] $index]
				NSRecall::RecallObject inventory $index
			}
		}
		MENU_TOOLBAR {
			set menu [NSMenu::Info $menuId menu]
			switch -- [Info $oop toolbar,mode] {
				buy -
				sell {
					if {$index == [$menu index end]} {
						set desc "Do nothing."
					} else {
						if {[angband store ishome]} {
							set desc $MenuString($ident,home)
						} else {
							set desc $MenuString($ident,store)
						}
						set index [lindex [Info $oop toolbar,match] $index]
						NSRecall::RecallObject [Info $oop toolbar,where] $index
					}
				}
				option {
					set desc [lindex [Info $oop toolbar,desc] $index].
				}
			}
		}
		default {
			if {[angband store ishome]} {
				set sym $ident,home
			} else {
				set sym $ident,store
			}
			if {[info exists MenuString($sym)]} {
				set desc $MenuString($sym)
			} elseif {[info exists MenuString($ident)]} {
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

# NSStore::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::MenuInvoke {oop menuId ident} {

	variable Priv

	switch -glob -- $ident {
		E_STORE_BUY {DoCommandIfAllowed p}
		E_STORE_SELL {DoCommandIfAllowed s}
		E_DESTROY {DoCommandIfAllowed K}
		E_STORE_EXIT {DoCommandIfAllowed \033}
	}

	return
}

# NSStore::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::DisplayCmd {oop message first} {

	set win [Info $oop win]
	
	switch -- $message {
		preDisplay {
			ConfigureWindow $oop

			SetList $oop
		}
		postDisplay {
		}
		reDisplay {
			# Preserve the scroll position and selection
			set canvistId [Info $oop canvistId]
			set canvas [NSCanvist::Info $canvistId canvas]
			set rowHeight [$canvas cget -yscrollincrement]
			set rowTop [expr {int([$canvas canvasy 0 $rowHeight] / $rowHeight)}]
			set current [Info $oop current]

			SetList $oop

			# Restore the scroll position and selection
			$canvas yview scroll $rowTop units
			if {$current != -1} {
				set count [NSCanvist::Info $canvistId count]
				if {$current >= $count} {
					set current [expr {$count - 1}]
				}
				if {$count} {
					NSCanvist::UpdateSelection $canvistId $current all
				}
			}
		}
		postWithdraw {

			# PositionItems() may be called when the window changes
			# size (for example, when the windows are arranged). Because
			# "angband store ishome" cannot be called when the character
			# isn't inside a store, I clear the list so PositionItems()
			# does nothing. The list should be cleared anyways, because
			# it might contain sprites.
			NSCanvist::DeleteAll [Info $oop canvistId]
		}
	}

	return
}

# NSStore::SetupCmd --
#
#	Called by NSWindowManager::Setup().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::SetupCmd {oop} {

	set win [Info $oop win]

	set frame $win.info

	# Like ConfigureWindow, but can't call that yet
	pack $frame.howMuch \
		-side left -expand no -padx 2
	pack $frame.totalCost \
		-side left -expand no -padx 2
	update idletasks

	$frame configure -height [winfo reqheight $frame]
	pack propagate $frame no

	return
}

# NSStore::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::Close {oop} {

	angband keypress \033

	return
}

# NSStore::Win98MenuCmd_Options --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::Win98MenuCmd_Options {oop button} {

	set canvas [$button info canvas]
	set x [winfo rootx $canvas]
	set y [expr {[winfo rooty $canvas] + [winfo height $canvas]}]

	set menu [Info $oop toolbar,menu]
	$menu delete 0 end

	set keywordList {}
	set descList {}

	$menu add separator
	lappend keywordList ""
	lappend descList ""

	$menu add command -label "Graphics Mode" -command {StoreObj Swap}
	lappend keywordList ""
	lappend descList "Use the graphical window"

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

# NSStore::SettingChanged --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::SettingChanged {oop keyword value} {

	# Ignore settings which don't affect the display
	if {[lsearch -exact [Info $oop toolbar,match] $keyword] == -1} return

	# Update the button
	update idletasks

	SetList $oop

	return
}

# NSStore::Win98MenuCmd_Buy --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::Win98MenuCmd_Buy {oop button} {

	variable MenuString

	set canvas [$button info canvas]
	set x [winfo rootx $canvas]
	set y [expr {[winfo rooty $canvas] + [winfo height $canvas]}]

	set menu [Info $oop toolbar,menu]

	$menu delete 0 end
	set doCmd 0
	set doItem 0
	set doMenu 0
	if {[string equal [angband inkey_flags] INKEY_CMD]} {
		set charCmd p
		set doMenu 1
		set doCmd 1
	} elseif {[string equal [angband inkey_flags] INKEY_ITEM_STORE]} {
		set charCmd ""
		set doMenu 1
		set doItem 1
	}
	set match {}
	if {$doMenu} {
		set count [angband store count]
		for {set index 0} {$index < $count} {incr index} {
			angband store info $index attrib
			set charItem $attrib(char)
			$menu add command -label "$charItem $attrib(name)" \
				-command "angband keypress $charCmd$charItem" \
				-underline 0
			lappend match $index
		}
	}
	if {[string compare [$menu index end] none]} {
		$menu add separator
	}
	if {$doItem} {
		$menu add command -label "Cancel" -command "angband keypress \033"
	} else {
		$menu add command -label "Cancel"
	}

	if {[angband store ishome]} {
		set MenuString(MENU_TOOLBAR,home) "Take this item."
	} else {
		set MenuString(MENU_TOOLBAR,store) "Buy this item."
	}

	Info $oop toolbar,mode buy
	Info $oop toolbar,match $match
	Info $oop toolbar,where store

	[Info $oop win].statusBar cover show

	tk_popup $menu $x $y

	if {[Platform unix]} {
		tkwait variable ::tkPriv(popup)
	}

	after idle "$button hidemenu ; [Info $oop win].statusBar cover hide"

	return
}

# NSStore::Win98MenuCmd_Sell --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::Win98MenuCmd_Sell {oop button} {

	variable MenuString

	set canvas [$button info canvas]
	set x [winfo rootx $canvas]
	set y [expr {[winfo rooty $canvas] + [winfo height $canvas]}]

	set menu [Info $oop toolbar,menu]

	$menu delete 0 end
	set doCmd 0
	set doItem 0
	set doMenu 0
	if {[string equal [angband inkey_flags] INKEY_CMD]} {
		set charCmd s
		set doMenu 1
		set doCmd 1
	} elseif {[string equal [angband inkey_flags] INKEY_ITEM] &&
		[string equal [angband inkey_other] inventory]} {
		set charCmd ""
		set doMenu 1
		set doItem 1
	}
	set match {}
	if {$doMenu} {
		foreach index [angband inventory find -store_will_buy yes] {
			angband inventory info $index attrib
			set charItem $attrib(char)
			$menu add command -label "$charItem $attrib(name)" \
				-command "angband keypress $charCmd$charItem" \
				-underline 0
			lappend match $index
		}
	}
	if {[string compare [$menu index end] none]} {
		$menu add separator
	}
	if {$doItem} {
		$menu add command -label "Cancel" -command "angband keypress \033"
	} else {
		$menu add command -label "Cancel"
	}

	if {[angband store ishome]} {
		set MenuString(MENU_TOOLBAR,home) "Drop this item."
	} else {
		set MenuString(MENU_TOOLBAR,store) "Sell this item."
	}

	Info $oop toolbar,mode sell
	Info $oop toolbar,match $match
	Info $oop toolbar,where inventory

	[Info $oop win].statusBar cover show

	tk_popup $menu $x $y

	if {[Platform unix]} {
		tkwait variable ::tkPriv(popup)
	}

	after idle "$button hidemenu ; [Info $oop win].statusBar cover hide"

	return
}

# NSStore::ConfigureWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::ConfigureWindow {oop} {

	set win [Info $oop win]
	set frame $win.info

	eval pack forget [winfo children $frame]

#	pack $frame.divider1 \
#		-side top -expand yes -fill x
	pack $frame.howMany \
		-side left -expand no -padx 2
	pack $frame.quantity \
		-side left -expand no -padx 2

	# Not in the Home
	if {![angband store ishome]} {

		pack $frame.howMuch \
			-side left -expand no -padx 2
		pack $frame.totalCost \
			-side left -expand no -padx 2
	}

	pack $frame.gold \
		-side right -expand no -padx 2
	pack $frame.playerGold \
		-side right -expand no -padx 2

	if {![angband store ishome]} {
		$frame.purse configure -text "Purse: [angband store purse]    "
		pack $frame.purse \
			-side right -expand no -padx 2
	}

	set mbarId [Info $oop mbarId]
	NSMenu::MenuDeleteEntry $mbarId E_DESTROY
	if {[angband store ishome]} {
		NSMenu::EntryConfigure $mbarId M_STORE -label "Home"
		NSMenu::EntryConfigure $mbarId M_SELL -label "Drop"
		NSMenu::EntryConfigure $mbarId E_STORE_BUY -label "Take"
		NSMenu::EntryConfigure $mbarId E_STORE_SELL -label "Drop"
		NSMenu::MenuInsertEntry $mbarId -before E_SEP_1 \
			-type command -label "Destroy" -identifier E_DESTROY

	} else {
		NSMenu::EntryConfigure $mbarId M_STORE -label "Store"
		NSMenu::EntryConfigure $mbarId M_SELL -label "Sell"
		NSMenu::EntryConfigure $mbarId E_STORE_BUY -label "Buy"
		NSMenu::EntryConfigure $mbarId E_STORE_SELL -label "Sell"
	}

	return
}

# NSStore::HaggleSetup --
#
#	Shows or hides display elements during the buy/sell process.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::HaggleSetup {oop action} {

	set win [Info $oop win]
	set frame $win.info

	switch -- $action {

		haggle_open {
			pack forget $frame.howMany $frame.quantity $frame.howMuch \
				$frame.totalCost
			pack $frame.price_owner \
				-side left -padx 2
			pack $frame.price_character \
				-side left -padx 2
		}

		haggle_close {
			pack forget $frame.price_owner $frame.price_character
			pack $frame.howMany \
				-side left -padx 2
			pack $frame.quantity \
				-side left -padx 2
			pack $frame.howMuch \
				-side left -padx 2
			pack $frame.totalCost \
				-side left -padx 2
		}
	}

	return
}

# NSStore::SetList --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::SetList {oop} {

	variable Priv

	# Get the window and canvas
	set win [Info $oop win]
	set canvistId [Info $oop canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]

	# Clear and disable the "quantity" entry
	Info $oop quantity ""
	$win.info.quantity configure -state disabled

	# Get the store name
	set storename [angband store storename]

	# In the Home
	if {[angband store ishome]} {

		# Set the window title
		wm title $win "Your Home"

	# Not in the Home
	} else {

		# Set the window title
		wm title $win [format "The %s owned by %s" \
			$storename [angband store ownername]]
	}

	# Get the number of items
	set count [angband store count]

	# Clear the list
	NSCanvist::DeleteAll $canvistId

	# Calculate the row height. This is done every time here since
	# the font may change. The row height equals the linespace of 
	# the font plus 8 pixels for the selection rectangle, or the
	# icon size plus 8, whichever is greater.
	set rowHgt [font metrics [Value font,store] -linespace]

	# Option: Show icons in lists
	if {[icon size] > $rowHgt} {
		set rowHgt [icon size]
	}

	# Leave room for the selection rectangle on each line
	incr rowHgt 8
	
	# Set the row height	
	NSCanvist::Info $canvistId rowHgt $rowHgt
	$canvas configure -yscrollincrement $rowHgt

	set Priv(width,char) 0
	set Priv(width,desc) 0
	set Priv(width,weight) 0
	set Priv(width,price) 0

	# Append each object
	for {set index 0} {$index < $count} {incr index} {

		# Get the object info
		angband store info $index attrib

		# Get the (optional) icon
		set icon $attrib(icon)

		# Get the (optional) weight
		set weight $attrib(weight)

		# No price in the Home
		if {[angband store ishome]} {
			set price ""
			set fixed ""
			
		# Price in the store
		} else {
		
			# Hack -- See if this is a fixed price
			set price $attrib(cost)
			regexp {([0-9]+)(.*)} $price ignore price fixed
		}
		
		# Append to the list
		NSCanvist::Insert $canvistId end $attrib(char) $attrib(number) \
			$attrib(name) $weight $attrib(tval) $icon $price $fixed
	}

	# Arrange all the items
	PositionItems $oop

	# Display number of objects
	if {$count == 1} {
		set string [format "%d item" $count]
	} else {
		set string [format "%d items" $count]
	}
	$win.statusBar itemconfigure t2 -text $string

	Synch $oop

	return
}

# NSStore::ContextMenu --
#
#	When the store list is right-clicked, pop up a context
#	menu of actions.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::ContextMenu {oop menu x y} {

	variable Priv

	set canvistId [Info $oop canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]

	set font [$menu cget -font]

	# Find the hit row
	set x1 [expr {$x - [winfo rootx $canvas]}]
	set y1 [expr {$y - [winfo rooty $canvas]}]
	set row [NSCanvist::PointToRow $canvistId $x1 $y1]

	# Clear the menu
	$menu delete 0 end

	set charBuy p
	set charSell s
	if {[angband store ishome]} {
		set stringBuy "Take"
		set stringSell "Drop"
	} else {
		set stringBuy "Buy"
		set stringSell "Sell"
	}

	# No row is hit
	if {$row == -1} {

		# We are waiting for a command
		if {[string equal [angband inkey_flags] INKEY_CMD]} {

			$menu add command -label "$stringBuy An Item" \
				-command "angband keypress $charBuy"
			$menu add command -label "$stringSell An Item" \
				-command "angband keypress $charSell"
			$menu add separator
			$menu add command -label Leave \
				-command "angband keypress \033"
			$menu add separator
			$menu add command -label Cancel
	
			# Pop up the menu
			tk_popup $menu $x $y
		}
		
		# Done
		return
	}

	set itemIndex $row

	# Get information about this item
	angband store info $itemIndex attrib

	# Get the item char
	set itemKey $attrib(char)

	# Get the tval
	set itemTval $attrib(tval)

	# Get the amount
	set itemAmount $attrib(number)

	# We are waiting for an item
	if {[string equal [angband inkey_flags] INKEY_ITEM_STORE]} {

		# Append a command to select the item
		set command "angband keypress $itemKey"
		$menu add command -label "Select This Item" -command $command \
			-font [BoldFont $font]
		$menu add separator
		$menu add command -label "Cancel" -command "angband keypress \033"

		# Pop up the menu
		tk_popup $menu $x $y

		# Done
		return
	}

	# We are not waiting for a command
	if {[string compare [angband inkey_flags] INKEY_CMD]} {
		return
	}

	if {$itemAmount == 1} {
		$menu add command -label "$stringBuy This Item" \
			-command "angband keypress $charBuy$itemKey" -font [BoldFont $font]
	} else {
		$menu add command -label "$stringBuy One" \
			-command "angband keypress 01$charBuy$itemKey" -font [BoldFont $font]
		if {$itemAmount > 5} {
			$menu add command -label "$stringBuy Five" \
				-command "angband keypress 05$charBuy$itemKey"
		}
		$menu add command -label "$stringBuy All" \
			-command "angband keypress 0$itemAmount$charBuy$itemKey"
	}
	$menu add separator
	$menu add command -label "$stringSell An Item" \
		-command "angband keypress $charSell"

	if {[angband store ishome]} {
		# Complex handling of Destroy command. The entire stack is
		# destroyed, and the user isn't asked to confirm.
		set command "angband keypress 0${itemAmount}K$itemKey"
		$menu add separator
		$menu add command -label "*Destroy*" -command $command
	}
	$menu add separator
	$menu add command -label "Leave" \
		-command "angband keypress \033"
	$menu add separator
	$menu add command -label "Cancel"

	# Pop up the menu
	tk_popup $menu $x $y

	return
}

# NSStore::SynchScrollBars --
#
#	There is a bug (my bug or in Tk?) which prevents the scroll bars
#	from synchronizing when the window is not mapped. So I bind to
#	the <Map> event and synch the scroll bars here.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::SynchScrollBars {oop} {

	set win [Info $oop win]
	set canvistId [Info $oop canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]

	eval $win.frame.scroll set [$canvas yview]
	$canvas yview moveto 0.0

	return
}

# NSStore::Invoke --
#
#	When a store item is double-clicked, "angband keypress" to
#	initiate a purchase. This is also called when Return is typed
#	while the Quantity Entry has the focus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::Invoke {oop canvistId x y} {

	set canvas [NSCanvist::Info $canvistId canvas]

	# Get the hit row
	set row [NSCanvist::PointToRow $canvistId $x $y]

	# No row was hit
	if {$row == -1} return

	# Invoke
	InvokeRow $oop $row

	return
}

# NSStore::InvokeByReturn --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::InvokeByReturn {oop} {

	set canvistId [Info $oop canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]
	
	# Hack -- Focus on the list
	focus $canvas

	# Get the selection
	set selection [NSCanvist::Selection $canvistId]
	if {![llength $selection]} return

	# Get the (first) selected row
	set row [lindex $selection 0]

	# Invoke
	InvokeRow $oop $row

	return
}

# NSStore::InvokeRow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::InvokeRow {oop row} {

	# See if we are waiting for a command
	set doCmd 0
	if {[string equal [angband inkey_flags] INKEY_CMD]} {
		set doCmd 1
	}

	# See if we are waiting for a store item
	set doItem 0
	if {[string equal [angband inkey_flags] INKEY_ITEM_STORE]} {
		set doItem 1
	}

	# Do nothing
	if {!$doCmd && !$doItem} return

	# Get the item info
	angband store info $row attrib

	# Append a quantity if waiting for a command
	if {$doCmd} {
		set keypress "0[Info $oop quantity]p"
	}

	# Append item char
	append keypress $attrib(char)

	# Feed the Term
	angband keypress $keypress

	return
}

# NSStore::SelectionChanged --
#
#	Called when the list selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::SelectionChanged {oop canvistId select deselect} {

	set win [Info $oop win]
	set entry $win.info.quantity

	# An item was selected
	if {[llength $select]} {

		$entry configure -state normal
		Info $oop quantity "1"

		# Get the (first) row
		set row [lindex $select 0]
		Info $oop current $row

		# Display memory for this object
		NSRecall::RecallObject store $row

	# An item was deselected (but not selected)
	} else {

		Info $oop quantity ""
		$entry configure -state disabled
		Info $oop current -1
	}

	return
}

# NSStore::QuantityChanged --
#
#	Trace variable callback on NSStore($oop,quantity). When the number
#	of items to buy changes, synch the "total price" field.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::QuantityChanged {oop name1 name2 op} {

	set win [Info $oop win]
	set canvistId [Info $oop canvistId]
	set quantity [Info $oop quantity]

	set selection [NSCanvist::Selection $canvistId]

	if {$selection == {} || $quantity == {}} {
		$win.info.totalCost configure -text {}
		return
	}

	set row [lindex $selection 0]

	angband store info $row attrib

	if {$quantity <= 0} {
		set quantity 1
	}
	set max $attrib(number)
	if {$quantity > $max} {
		set quantity $max
	}

	# Hack -- Handle "fixed" prices (ex "45 F")
	scan $attrib(cost) %d price
	
	# No prices are displayed in the Home
	if {![angband store ishome]} {
		$win.info.totalCost configure -text [expr {$quantity * $price}]
	}

	Info $oop quantity $quantity

	return
}

# NSStore::NewItemCmd --
#
#	Called by NSCanvist::InsertItem() to create a list row.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::NewItemCmd {oop canvistId y char number text weight tval icon price fixed} {

	variable Priv

	set canvas [NSCanvist::Info $canvistId canvas]
	set lineHeight [NSCanvist::Info $canvistId rowHgt]
	set font [Value font,store]

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
		-text $char) -anchor nw -font $font -fill White]

	# Description
	set fill [default_tval_to_attr $tval]
	lappend itemIdList [$canvas create text 0 \
		[expr {$y + $diff}] -text $text -anchor nw -font $font -fill $fill \
		-tags "enabled fill:$fill hilite description"]

	# This item "truncates" long descriptions
	set listBG [Value listBG]
	lappend itemIdList [$canvas create rectangle 0 $y \
		1 [expr {$y + $lineHeight}] -fill $listBG -outline $listBG \
		-tags truncate]

	# Weight
	set weight [format "%d.%d lb" [expr {$weight / 10}] [expr {$weight % 10}]]
	lappend itemIdList [$canvas create text 0 \
		[expr {$y + $diff}] -text $weight -anchor ne -justify right \
		-font $font -fill White -tags weight]

	# Price (except in Home)
	if {![angband store ishome]} {
	
		lappend itemIdList [$canvas create text 0 [expr {$y + $diff}] \
			-text $price -anchor ne -justify right -font $font -fill White \
			-tags price]

		lappend itemIdList [$canvas create text 0 [expr {$y + $diff}] \
			-text $fixed -anchor ne -justify right -font $font -fill White \
			-tags fixed]
	}

	# Selection rectangle around text
	lappend itemIdList [$canvas create rectangle 2 [expr {$y + 2}] \
		2 [expr {$y + $lineHeight - 2}] -fill "" -outline "" \
		-tags {enabled selrect} -width 2.0]

	# Maximum width of char
	set width [font measure $font "$char) "]
	if {$width > $Priv(width,char)} {
		set Priv(width,char) $width
	}

	# Maximum width of description
	set width [font measure $font $text]
	if {$width > $Priv(width,desc)} {
		set Priv(width,desc) $width
	}

	# Maximum width of weight
	if {[string length $weight]} {
		set width [font measure $font "ABC$weight"]
		if {$width > $Priv(width,weight)} {
			set Priv(width,weight) $width
		}
	}

	# Maximum width of price
	if {![angband store ishome]} {
		
		set width [font measure $font "ABC{$price}"]
		if {$width > $Priv(width,price)} {
			set Priv(width,price) $width
		}
	}

	return $itemIdList
}

# NSStore::PositionItems --
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

proc NSStore::PositionItems {oop} {

	variable Priv

	set canvistId [Info $oop canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]

	# Nothing to do
	if {![NSCanvist::Info $canvistId count]} return

	# Get the font
	set font [Value font,store]

	# Get the width of the canvas
	set canvasWidth [winfo width $canvas]
	
	set offset [expr {[icon size] + 8}]

	incr offset $Priv(width,char)
	
	# Position each description
	set coords [$canvas coords description]
	$canvas move description [expr {$offset - [lindex $coords 0]}] 0

	# Truncate each description (by positioning each "truncate" item)
	set x0 [expr {($canvasWidth - 1) - $Priv(width,weight) - \
		$Priv(width,price) - 4}]
	set x1 $canvasWidth
	foreach itemId [$canvas find withtag truncate] {
		scan [$canvas coords $itemId] "%s %s %s %s" c0 c1 c2 c3
		$canvas coords $itemId $x0 $c1 $x1 $c3
	}

	if {[angband store ishome]} {
		set Priv(width,fixed) 0
	} else {
		set Priv(width,fixed) [font measure $font " F"]
	}
	
	# Position each weight
	if {$Priv(width,weight)} {
		set offset [expr {($canvasWidth - 1) - $Priv(width,price) \
			- $Priv(width,fixed) - 4}]
		set coords [$canvas coords weight]
		$canvas move weight [expr {$offset - [lindex $coords 0]}] 0
	}

	if {![angband store ishome]} {
	
		# Position each price
		set offset [expr {($canvasWidth - 1) - $Priv(width,fixed) - 4}]
		set coords [$canvas coords price]
		$canvas move price [expr {$offset - [lindex $coords 0]}] 0

		# Position each fixed
		set offset [expr {($canvasWidth - 1) - 4}]
		set coords [$canvas coords fixed]
		$canvas move fixed [expr {$offset - [lindex $coords 0]}] 0
	}

	# Position each selection rectangle
	set x1 [expr {($canvasWidth - 1) - 2}]
	foreach itemId [$canvas find withtag selrect] {
		scan [$canvas coords $itemId] "%s %s %s %s" c0 c1 c2 c3
		$canvas coords $itemId $c0 $c1 $x1 $c3
	}

	# Set the scrollregion to prevent horizontal scrolling
	scan [$canvas cget -scrollregion] "%s %s %s %s" x1 y1 x2 y2
	$canvas configure -scrollregion "$x1 $y1 $canvasWidth $y2"

	return
}

# NSStore::HighlightItemCmd --
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

proc NSStore::HighlightItemCmd {oop canvistId state args} {

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

# NSStore::StatusBar --
#
#	Display text in the status bar, perhaps clearing it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label [$win.statusBar itemcget t1 -label]
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

# NSStore::ValueChanged_listBG --
#
#	Called when the listBG value changes.
#	Updates the Store Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::ValueChanged_listBG {oop} {

	set canvistId [Info $oop canvistId]
	set canvas [NSCanvist::Info $canvistId canvas]

	set color [Value listBG]
	$canvas configure -background $color
	$canvas itemconfigure truncate -fill $color -outline $color

	return
}

# NSStore::Swap --
#
#	Display the "old" Store Window
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::Swap {oop} {

	NSWindowManager::Undisplay store

	NSModule::LoadIfNeeded NSStore2
	NSWindowManager::Display store2

	Value store,style new

	return
}

proc StoreObj {command args} {
	return [eval NSStore::$command [Global store,oop] $args]
}
