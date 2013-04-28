# File: store.tcl

# Purpose: the Store Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
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

set QUANTITY_SCALE 0

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

	MsgCatInit inven store

	InitImageIfNeeded Image_ButtonOptions button-options.gif
	InitImageIfNeeded Image_ButtonHelp button-help.gif

	NSModule::LoadIfNeeded NSToolbar
	NSModule::LoadIfNeeded NSList

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
	PopupInit $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow store $win \
		"GetDefaultGeometry $win main2 main2" "NSStore::SetupCmd $oop" \
		"NSStore::DisplayCmd $oop"

	# Update the display when some settings change
	qebind NSStore <Setting> {
		StoreObj SettingChanged %d %c
	}
	qeconfigure NSStore <Setting> -active no

	qebind NSStore <Term-inkey> "NSStore::Synch $oop"
	qeconfigure NSStore <Term-inkey> -active no

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

	trace remove variable NSStore($oop,quantity) write "::NSStore::QuantityChanged $oop"

	# <Py-gold> binding is deleted with the window

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

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSStore::Close $oop"

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

	set id [NSToolbar::AddTool $toolId \
		-command "DoCommandIfAllowed p" -label "Buy" -showlabel yes \
		-showimage no -heightlabel 16 -hasmenu yes \
		-menucommand "NSStore::Win98MenuCmd_Buy $oop"]
[NSToolbar::GetTool $toolId $id] configure -hovermenu yes -onlymenu yes -menuhidecmd "NSStore::PopupHide $oop"

	set id2 [NSToolbar::AddTool $toolId \
		-command "DoCommandIfAllowed s" -label "Sell" -showlabel yes \
		-showimage no -heightlabel 16 -hasmenu yes \
		-menucommand "NSStore::Win98MenuCmd_Sell $oop"]
[NSToolbar::GetTool $toolId $id2] configure -hovermenu yes -onlymenu yes -menuhidecmd "NSStore::PopupHide $oop"

	set id3 [NSToolbar::AddTool $toolId \
		-command "DoCommandIfAllowed \033" -label [mc "Leave"] -showlabel yes \
		-showimage no -heightlabel 16]

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
	Info $oop toolbar,leaveId $id3

	#
	# Divider + Store Info
	#
if 0 {
	if {[Platform unix]} {
		set font {Courier 12}
	}
	if {[Platform windows]} {
		set font {Courier 9}
	}
}
	set font [Value font,fixed]

	set frame $win.info
	frame $frame \
		-borderwidth 0
#	frame $frame.divider1 \
#		-borderwidth 1 -height 2 -relief groove
if $::QUANTITY_SCALE {
	label $frame.howMany \
		-font $font -text "1" -width 2 -textvariable ::NSStore($oop,quantity)
	scale $frame.quantity -orient horizontal -length 130 -showvalue no \
		-highlightthickness 0 -state disabled \
		-command "NSStore::QuantityChanged $oop"
} else {
	label $frame.howMany \
		-font $font -text [mc "Quantity:"]
	entry $frame.quantity \
		-width 2 -state disabled -textvariable NSStore($oop,quantity)
}
	label $frame.howMuch \
		-font $font -text [mc "Total Cost:"]
	label $frame.totalCost \
		-font $font -text "" -anchor w
	label $frame.purse \
		-font $font
if $::QUANTITY_SCALE {
	label $frame.playerGold \
		-font $font -text [mc "Gold:"]
} else {
	label $frame.playerGold \
		-font $font -text [mc "Gold Remaining:"]
}
	label $frame.gold \
		-font $font -text [angband player gold]

	# Update the display when the character's gold changes
	qebind $frame.gold <Py-gold> {%W configure -text %c}
	qeconfigure $frame.gold <Py-gold> -active no

	label $frame.price_character \
		-font $font -text "" -width 25 -anchor w
	label $frame.price_owner \
		-font $font -text "" -width 25 -anchor w

	if !$::QUANTITY_SCALE {
	global NSStore
	trace add variable NSStore($oop,quantity) write "::NSStore::QuantityChanged $oop"

	# This stops keys being fed to the Term
#	bindtags $frame.quantity "$frame.quantity Entry all"

	# Typing Enter in the Quantity Entry initiates a purchase
	bind $frame.quantity <KeyPress-Return> \
		"NSStore::InvokeByReturn $oop"
	}

	MakeDivider $win.divider2 x

	#
	# List
	#

	set cw [font measure [Value font,store] "W"]
	set width [expr {$cw * 81}]

	set frame $win.frame
	set tree [NSList::New $frame -columns 5 -font store]
	$tree configure -width $width -height 300

	$tree column configure 0 -tag char -expand no
	$tree column configure 1 -tag desc -expand yes
	$tree column configure 2 -tag weight -justify right
	$tree column configure 3 -tag price -justify right
	$tree column configure 4 -tag fixed -justify right

	qebind NSStore <Value-font,store> \
		"NSStore::ValueChanged_font_store $oop"

	Info $oop tree $tree

	# When an item is selected, recall it
	NSList::OnSelection $tree \
		"NSStore::SelectionChanged $oop %T %c %S %D"

	# Double-click to purchase an item
	NSList::OnInvoke $tree \
		"NSStore::Invoke $oop %r"

	bind $tree <ButtonPress-1> {
		if {[angband inkey_flags] eq "INKEY_MORE"} {
			angband keypress " "
			break
		}
	}

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 24

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
	bind $tree <Button-3> \
		"NSStore::ContextMenu $oop $m %X %Y"

	#
	# Feed Term when keys pressed
	#

	Term_KeyPress_Bind $win
	Term_KeyPress_Bind $tree
if 0 {
	# XXX Hack -- Don't feed Tab to the Term
	bind $canvas <KeyPress-Tab> {
		focus [tk_focusNext %W]
		break
	}
}

	bind $win <FocusIn> "
		if {\[string equal %W $win]} {
			focus $tree
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
		-menu MENU_STORE -label [mc Store] -underline 0 -identifier M_STORE

	set entries {}
	lappend entries [list -type command -label [mc Buy] -identifier E_STORE_BUY]
	lappend entries [list -type command -label [mc Sell] -identifier E_STORE_SELL]
	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		lappend entries [list -type command -label [mc Inspect] \
			-identifier E_STORE_INSPECT]
	}
	lappend entries [list -type separator -identifier E_SEP_1]
	lappend entries [list -type checkbutton -label [mc Auto-Haggle] \
		-variable NSStore::Priv(radio) -identifier E_AUTO_HAGGLE]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Leave] \
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

	#
	# Take/Buy button
	#

	if {[angband store ishome]} {
		set label Take
	} else {
		set label Buy
	}
	set state disabled
	if {[angband inkey_flags] eq "INKEY_CMD" ||
		[angband inkey_flags] eq "INKEY_ITEM_STORE"} {
		if {[angband store count]} {
			set state normal
		}
	}
	set id [Info $oop toolbar,buyId]
	[NSToolbar::GetTool $toolbarId $id] configure -state $state \
		-label [mc $label]

	#
	# Drop/Sell button
	#

	if {[angband store ishome]} {
		set label Drop
	} else {
		set label Sell
	}
	set state disabled
	if {[angband inkey_flags] eq "INKEY_CMD" ||
		[angband inkey_flags] eq "INKEY_ITEM"} {
		set items [angband inventory find -store_will_buy yes]
		if {[llength $items]} {
			set state normal
		}
	}
	set id [Info $oop toolbar,sellId]
	[NSToolbar::GetTool $toolbarId $id] configure -state $state \
		-label [mc $label]

	#
	# Leave button
	#

	set id [Info $oop toolbar,leaveId]
	set state disabled
	if {[angband inkey_flags] eq "INKEY_CMD"} {
		set state normal
	}
	[NSToolbar::GetTool $toolbarId $id] configure -state $state

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

	if {[angband inkey_flags] eq "INKEY_CMD"} {
		lappend identList E_STORE_BUY E_STORE_SELL E_STORE_EXIT
		if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
			lappend identList E_STORE_INSPECT
		}
		set Priv(radio) [Setting auto_haggle]
		lappend identList E_AUTO_HAGGLE
		if {[angband store ishome]} {
			lappend identList E_DESTROY
		}
	}

	NSMenu::MenuEnable $mbarID $identList

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
		E_AUTO_HAGGLE {
			if {$Priv(radio)} {
				set desc "Disables automatic price negotiation."
			} else {
				set desc "Enables automatic price negotiation."
			}
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
		E_STORE_INSPECT {
			if {[variant ANGBANDTK KANGBANDTK]} {
				DoCommandIfAllowed l
			}
			if {[variant OANGBANDTK]} {
				DoCommandIfAllowed I
			}
		}
		E_DESTROY {DoCommandIfAllowed K}
		E_AUTO_HAGGLE {Setting auto_haggle $Priv(radio)}
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

			# Scroll to the top
			set tree [Info $oop tree]
			$tree yview moveto 0.0

			qeconfigure $win.info.gold <Py-gold> -active yes
			qeconfigure NSStore <Setting> -active yes
			qeconfigure NSStore <Term-inkey> -active yes
		}
		postDisplay {
		}
		reDisplay {
			# Preserve the scroll position and selection
			set tree [Info $oop tree]
			set item [$tree index "nearest 0 0"]
			set rowTop 0
			if {$item ne ""} {
				set rowTop [NSList::Item2Row $tree $item]
			}
			set current [Info $oop current]

			SetList $oop

			# Restore the scroll position and selection
			$tree yview scroll $rowTop units
			if {$current != -1} {
				set count [expr {[$tree numitems] - 1}]
				if {$current >= $count} {
					set current [expr {$count - 1}]
				}
				if {$count} {
					$tree selection add "root child $current"
				}
			}
		}
		postWithdraw {

if {[Info $oop popup,posted] ne ""} { [Info $oop popup,posted] hidemenu }

			# The list should be cleared because
			# it might contain sprites.
			NSList::Clear [Info $oop tree]

			qeconfigure $win.info.gold <Py-gold> -active no
			qeconfigure NSStore <Setting> -active no
			qeconfigure NSStore <Term-inkey> -active no
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

	Info $oop setting,show_icons [Setting show_icons]
	$menu add checkbutton -label [mc "Show Icons"] \
		-command {Setting show_icons [StoreObj Info setting,show_icons]} \
		-variable NSStore($oop,setting,show_icons)
	lappend keywordList show_icons
	lappend descList [SettingDesc show_icons]

	Info $oop setting,show_weights [Setting show_weights]
	$menu add checkbutton -label [mc "Show Weights"] \
		-command {Setting show_weights [StoreObj Info setting,show_weights]} \
		-variable NSStore($oop,setting,show_weights)
	lappend keywordList show_weights
	lappend descList [SettingDesc show_weights]

	if {[variant OANGBANDTK]} {
		Info $oop setting,use_metric [Setting use_metric]
		$menu add checkbutton -label [mc "Use Metric"] \
			-command {Setting use_metric [StoreObj Info setting,use_metric]} \
			-variable NSStore($oop,setting,use_metric)
		lappend keywordList use_metric
		lappend descList [SettingDesc use_metric]
	}

	$menu add separator
	lappend descList ""

	$menu add command -label [mc "Graphics Mode"] -command {StoreObj Swap}
	lappend descList "Use the graphical window"

	$menu add separator
	lappend descList ""

	$menu add command -label [mc "Set Font"] -command {
		NSModule::LoadIfNeeded NSFont
		NSWindowManager::Display font store
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
	if {[angband inkey_flags] eq "INKEY_CMD"} {
		set charCmd p
		set doMenu 1
		set doCmd 1
	} elseif {[angband inkey_flags] eq "INKEY_ITEM_STORE"} {
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
	if {[$menu index end] ne "none"} {
		$menu add separator
	}
	if {$doItem} {
		$menu add command -label [mc Cancel] -command "angband keypress \033"
	} else {
		$menu add command -label [mc Cancel]
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
		tkwait variable ::tk::Priv(popup)
	}

	after idle "$button hidemenu ; [Info $oop win].statusBar cover hide"

	return
}

proc NSStore::Win98MenuCmd_Buy {oop button} {

	variable MenuString

	set canvas [$button info canvas]
	set x [winfo rootx $canvas]
	set y [expr {[winfo rooty $canvas] + [winfo height $canvas]}]

	set tree [Info $oop popup,tree]
	$tree item delete all

	# Sync colors/fonts here
	[Info $oop popup,win].frame configure -background [Value listBG]
	$tree configure -background [Value listBG] -font [Value font,knowledge]
	set fill [list [Value listHilite] {selected focus} [Value listInactive] {selected}]
	$tree element configure eSel -fill $fill

	set doCmd 0
	set doItem 0
	set doMenu 0
	if {[angband inkey_flags] eq "INKEY_CMD"} {
		set charCmd p
		set doMenu 1
		set doCmd 1
	} elseif {[angband inkey_flags] eq "INKEY_ITEM_STORE"} {
		set charCmd ""
		set doMenu 1
		set doItem 1
	}
	if {$doMenu} {
		set count [angband store count]
		for {set index 0} {$index < $count} {incr index} {
			angband store info $index attrib
			set charItem $attrib(char)
			set color [default_tval_to_attr $attrib(tval)]
			set keypress $charCmd$charItem
			PopupNewItem $oop $charItem $attrib(name) $color \
				store $index $keypress
		}
	}

	# Add "Cancel" if choosing an item
	if {[angband inkey_flags] eq "INKEY_ITEM_STORE"} {
		if {[$tree item count visible]} {
			PopupNewSeparator $oop
		}
		set item [$tree item create -parent root]
		$tree item style set $item COL sCancel
		if {$doItem} {
			Info $oop popup,cancel 1
		} else {
			Info $oop popup,cancel 0
		}
	}

	if {[angband store ishome]} {
		set MenuString(MENU_TOOLBAR,home) "Take this item."
	} else {
		set MenuString(MENU_TOOLBAR,store) "Buy this item."
	}

	Info $oop toolbar,mode buy
	Info $oop toolbar,where store

	[Info $oop win].statusBar cover show

	# Prevent errors if the button should be disabled but hasn't been yet.
	if {![$tree item count visible]} {
		set item [$tree item create -parent root]
		$tree item style set $item COL sCancel
		Info $oop popup,cancel 0
	}

	# Set the size of the list to the size of all the items
	scan [$tree item bbox end] "%d %d %d %d" left top right bottom
	$tree configure -width $right -height $bottom
	update idletasks

	wm geometry [Info $oop popup,win] +$x+$y

	wm transient [Info $oop popup,win] [Info $oop win]
	wm deiconify [Info $oop popup,win]
	raise [Info $oop popup,win]

	Info $oop popup,posted $button

	# Set a grab on the Store Window and claim the focus
	NSUtils::GrabSave [Info $oop win]
	focus $tree

	return [Info $oop popup,win]
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
	if {[angband inkey_flags] eq "INKEY_CMD"} {
		set charCmd s
		set doMenu 1
		set doCmd 1
	} elseif {[angband inkey_flags] eq "INKEY_ITEM" &&
		[angband inkey_other] eq "inventory"} {
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
	if {[$menu index end] ne "none"} {
		$menu add separator
	}
	if {$doItem} {
		$menu add command -label [mc Cancel] -command "angband keypress \033"
	} else {
		$menu add command -label [mc Cancel]
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
		tkwait variable ::tk::Priv(popup)
	}

	after idle "$button hidemenu ; [Info $oop win].statusBar cover hide"

	return
}

proc NSStore::Win98MenuCmd_Sell {oop button} {

	variable MenuString

	set canvas [$button info canvas]
	set x [winfo rootx $canvas]
	set y [expr {[winfo rooty $canvas] + [winfo height $canvas]}]

	set tree [Info $oop popup,tree]
	$tree item delete all

	# Sync colors/fonts here
	[Info $oop popup,win].frame configure -background [Value listBG]
	$tree configure -background [Value listBG] -font [Value font,knowledge]
	set fill [list [Value listHilite] {selected focus} [Value listInactive] {selected}]
	$tree element configure eSel -fill $fill

	set doCmd 0
	set doItem 0
	set doMenu 0
	if {[angband inkey_flags] eq "INKEY_CMD"} {
		set charCmd s
		set doMenu 1
		set doCmd 1
	} elseif {[angband inkey_flags] eq "INKEY_ITEM" &&
		[angband inkey_other] eq "inventory"} {
		set charCmd ""
		set doMenu 1
		set doItem 1
	}
	if {$doMenu} {
		foreach index [angband inventory find -store_will_buy yes] {
			angband inventory info $index attrib
			set charItem $attrib(char)
			set color [default_tval_to_attr $attrib(tval)]
			set keypress $charCmd$charItem
			PopupNewItem $oop $charItem $attrib(name) $color \
				inventory $index $keypress
		}
	}

	# Add "Cancel" if choosing an item
	if {[angband inkey_flags] eq "INKEY_ITEM"} {
		if {[$tree item count visible]} {
			PopupNewSeparator $oop
		}
		set item [$tree item create -parent root]
		$tree item style set $item COL sCancel
		if {$doItem} {
			Info $oop popup,cancel 1
		} else {
			Info $oop popup,cancel 0
		}
	}

	if {[angband store ishome]} {
		set MenuString(MENU_TOOLBAR,home) "Drop this item."
	} else {
		set MenuString(MENU_TOOLBAR,store) "Sell this item."
	}

	Info $oop toolbar,mode sell
	Info $oop toolbar,where inventory

	[Info $oop win].statusBar cover show

	# Prevent errors if the button should be disabled but hasn't been yet.
	if {![$tree item count visible]} {
		set item [$tree item create -parent root]
		$tree item style set $item COL sCancel
		Info $oop popup,cancel 0
	}

	# Set the size of the list to the size of all the items
	scan [$tree item bbox end] "%d %d %d %d" left top right bottom
	$tree configure -width $right -height $bottom
	update idletasks

	wm geometry [Info $oop popup,win] +$x+$y

	wm transient [Info $oop popup,win] [Info $oop win]
	wm deiconify [Info $oop popup,win]
	raise [Info $oop popup,win]

	Info $oop popup,posted $button

	# Set a grab on the Store Window and claim the focus
	NSUtils::GrabSave [Info $oop win]
	focus $tree

	return [Info $oop popup,win]
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

	pack forget {*}[winfo children $frame]

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
		$frame.purse configure -text "[mc Purse:] [angband store purse]    "
		pack $frame.purse \
			-side right -expand no -padx 2
	}

	set mbarId [Info $oop mbarId]
	NSMenu::MenuDeleteEntry $mbarId E_DESTROY
	if {[angband store ishome]} {
		NSMenu::EntryConfigure $mbarId M_STORE -label [mc Home]
		NSMenu::EntryConfigure $mbarId M_SELL -label [mc Drop]
		NSMenu::EntryConfigure $mbarId E_STORE_BUY -label [mc Take]
		NSMenu::EntryConfigure $mbarId E_STORE_SELL -label [mc Drop]
		NSMenu::MenuInsertEntry $mbarId -before E_SEP_1 \
			-type command -label [mc Destroy] -identifier E_DESTROY

	} else {
		NSMenu::EntryConfigure $mbarId M_STORE -label [mc Store]
		NSMenu::EntryConfigure $mbarId M_SELL -label [mc Sell]
		NSMenu::EntryConfigure $mbarId E_STORE_BUY -label [mc Buy]
		NSMenu::EntryConfigure $mbarId E_STORE_SELL -label [mc Sell]
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

	# Get the window and list
	set win [Info $oop win]
	set tree [Info $oop tree]

	Info $oop quantity ""
	if $::QUANTITY_SCALE {
	$win.info.quantity configure -from 1 -to 1 -state disabled
	} else {
	# Clear and disable the "quantity" entry
	$win.info.quantity configure -state disabled
	}

	# Get the store name
	set storename [angband store storename]

	# In the Home
	if {[angband store ishome]} {

		# Set the window title
		wm title $win [mc home-title1]

	# Not in the Home
	} else {

		# Set the window title
		wm title $win [format [mc store-title1] \
			$storename [angband store ownername]]
	}

	# Get the number of items
	set count [angband store count]

	# Clear the list
	NSList::Clear $tree


	# Options: Show icons, Show weights
	set show_icons [Setting show_icons]
	set show_weights [Setting show_weights]

NSList::Config $tree -icon $show_icons
$tree column configure weight -visible $show_weights
$tree column configure price -visible [expr {![angband store ishome]}]

$tree style layout s2 eTxt -padx {20 0}
$tree style layout s3 eTxt -padx {20 0}

	# Append each object
	for {set index 0} {$index < $count} {incr index} {

		# Get the object info
		angband store info $index attrib

		# Get the (optional) icon
		set icon $attrib(icon)
		if {!$show_icons} {
			set icon ""
		}

		# Get the (optional) weight
		set weight $attrib(weight)
		if {!$show_weights} {
			set weight ""
		}

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
set item [$tree item create]
if {$icon ne ""} {
	NSList::SetIcon $tree $item $icon
}
NSList::SetText $tree $item "$attrib(char)\) "
NSList::SetTextEx $tree $item desc $attrib(name)
NSList::SetTextFillEx $tree $item desc [default_tval_to_attr $attrib(tval)]
if {$weight ne ""} {
	NSList::SetTextEx $tree $item weight [fmt_wgt $weight 1]
}
if {$price ne ""} {
	NSList::SetTextEx $tree $item price $price
	NSList::SetTextEx $tree $item fixed " $fixed"
}
$tree item lastchild root $item
	}

	# Display number of objects
	if {$count == 1} {
		set string [format [mc "%d item"] $count]
	} else {
		set string [format [mc "%d items"] $count]
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

	set tree [Info $oop tree]

	set font [$menu cget -font]

	# Find the hit row
	set x1 [expr {$x - [winfo rootx $tree]}]
	set y1 [expr {$y - [winfo rooty $tree]}]
	set row [NSList::Point2Row $tree $x1 $y1]

	# Clear the menu
	$menu delete 0 end

	# Waiting for an inventory item
	if {[angband inkey_flags] eq "INKEY_ITEM"} {

		# Hack -- PopupSelect_Item() changes the show_flavors option,
		# which calls SettingChaged(), which we don't want.
		Info $oop toolbar,match {}

		# Display equipment/inventory items
		NSRecall::PopupSelect_Item $menu $x $y

		# Done
		return
	}

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
		if {[angband inkey_flags] eq "INKEY_CMD"} {

			$menu add command -label [mc "$stringBuy An Item"] \
				-command "angband keypress $charBuy"
			$menu add command -label [mc "$stringSell An Item"] \
				-command "angband keypress $charSell"
			$menu add separator
			$menu add command -label [mc Leave] \
				-command "angband keypress \033"
			$menu add separator
			$menu add command -label [mc Cancel]

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
	if {[angband inkey_flags] eq "INKEY_ITEM_STORE"} {

		# Append a command to select the item
		set command "angband keypress $itemKey"
		$menu add command -label [mc "Select This Item"] -command $command \
			-font [BoldFont $font]
		$menu add separator
		$menu add command -label [mc Cancel] -command "angband keypress \033"

		# Pop up the menu
		tk_popup $menu $x $y

		# Done
		return
	}

	# We are not waiting for a command
	if {[angband inkey_flags] ne "INKEY_CMD"} {
		return
	}

	if {$itemAmount == 1} {
		$menu add command -label [mc "$stringBuy This Item"] \
			-command "angband keypress $charBuy$itemKey" -font [BoldFont $font]
	} else {
		$menu add command -label [mc "$stringBuy One"] \
			-command "angband keypress 01$charBuy$itemKey" -font [BoldFont $font]
		if {$itemAmount > 5} {
			$menu add command -label [mc "$stringBuy Five"] \
				-command "angband keypress 05$charBuy$itemKey"
		}
		if {$itemAmount > 1} {
			$menu add command -label [mc "$stringBuy Some"] \
				-command "angband keypress $charBuy$itemKey"
		}
		$menu add command -label [mc "$stringBuy All"] \
			-command "angband keypress 0$itemAmount$charBuy$itemKey"
	}
	$menu add separator
	$menu add command -label [mc "$stringSell An Item"] \
		-command "angband keypress $charSell"

	if {[variant ANGBANDTK KANGBANDTK]} {
		$menu add command -label [mc Inspect] \
			-command "angband keypress l$itemKey"
	}
	if {[variant OANGBANDTK]} {
		$menu add command -label [mc Inspect] \
			-command "angband keypress I$itemKey"
	}

	if {[angband store ishome]} {
		# Complex handling of Destroy command. The entire stack is
		# destroyed, and the user isn't asked to confirm.
		set command "angband keypress 0${itemAmount}K$itemKey"
		if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
			if {[Setting verify_destroy] &&
				([Setting verify_destroy_junk]
				|| ![angband store worthless $itemIndex])} {
				append command y
			}
		}
		if {[variant ZANGBANDTK]} {
			if {!([Setting auto_destroy] && [angband store worthless $itemIndex])} {
				append command y
			}
		}
		$menu add separator
		$menu add command -label [mc *Destroy*] -command $command
	}
	$menu add separator
	$menu add command -label [mc Leave] \
		-command "angband keypress \033"
	$menu add separator
	$menu add command -label [mc Cancel]

	# Pop up the menu
	tk_popup $menu $x $y

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

proc NSStore::Invoke {oop row} {

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

	set tree [Info $oop tree]

	# Hack -- Focus on the list
	focus $tree

	# Get the selected row
	set row [Info $oop current]
	if {$row == -1} return

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
	if {[angband inkey_flags] eq "INKEY_CMD"} {
		set doCmd 1
	}

	# See if we are waiting for a store item
	set doItem 0
	if {[angband inkey_flags] eq "INKEY_ITEM_STORE"} {
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

proc NSStore::SelectionChanged {oop tree count select deselect} {

	set win [Info $oop win]
	if $::QUANTITY_SCALE {
	set scale $win.info.quantity
	} else {
	set entry $win.info.quantity
	}

	# An item was selected
	if {[llength $select]} {

		# Get the (first) row
		set item [lindex $select 0]
		set row [NSList::Item2Row $tree $item]
		Info $oop current $row

		if $::QUANTITY_SCALE {
		angband store info $row attrib
		set max [expr {min($attrib(number), 20)}]
		$scale configure -state normal -from 1 -to $max
		$scale set 1
		QuantityChanged $oop 1
		} else {
		$entry configure -state normal
		Info $oop quantity "1"
		}

		# Display memory for this object
		NSRecall::RecallObject store $row

	# An item was deselected (but not selected)
	} else {

		if $::QUANTITY_SCALE {
		$scale configure -from 1 -to 1 -state disabled
		$win.info.totalCost configure -text ""
		} else {
		$entry configure -state disabled
		}
		Info $oop quantity ""
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

if $::QUANTITY_SCALE {

proc NSStore::QuantityChanged {oop quantity} {

	set win [Info $oop win]
	set tree [Info $oop tree]
	set current [Info $oop current]

	if {$current == -1 || $quantity eq ""} {
		$win.info.totalCost configure -text ""
		return
	}

	set row $current

	angband store info $row attrib

	# Hack -- Handle "fixed" prices (ex "45 F")
	scan $attrib(cost) %d price

	# No prices are displayed in the Home
	if {![angband store ishome]} {
		$win.info.totalCost configure -text [expr {$quantity * $price}]
	}

	Info $oop quantity $quantity

	return
}

} else {

proc NSStore::QuantityChanged {oop name1 name2 op} {

	set win [Info $oop win]
	set tree [Info $oop tree]
	set quantity [Info $oop quantity]
	set current [Info $oop current]

	if {$current == -1 || $quantity eq ""} {
		$win.info.totalCost configure -text ""
		return
	}

	set row $current

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

# NSInventory::ValueChanged_font_store --
#
#	Called when the font,inventory value changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::ValueChanged_font_store {oop} {

	if {![winfo ismapped [Info $oop win]]} return
	SetList $oop

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


# NSStore::PopupInit --
#
#	Init the treectrl-based menu.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::PopupInit {oop} {

	set win [Info $oop win].popup
	toplevel $win

	wm overrideredirect $win yes
	$win configure -borderwidth 1 -relief flat -background gray60
	wm resizable $win no no

	wm withdraw $win
	# Make it transient later

	set frame $win.frame
	frame $frame -borderwidth 0

	set tree $frame.tree
	treectrl $tree -usetheme yes \
		-showroot no -showlines no -showbuttons no -showheader no \
		-highlightthickness 0 -borderwidth 0

	#
	# Item style
	#
	$tree element create eSel rect
	$tree element create eChar text -fill White
	$tree element create eTxt text -fill {White selected}

	$tree style create STYLE
	$tree style elements STYLE {eSel eChar eTxt}
	$tree style layout STYLE eSel -detach yes -iexpand xy
	$tree style layout STYLE eChar -padx {4 0} -pady 1
	$tree style layout STYLE eTxt -padx {0 4} -pady 1

	#
	# Separator style
	#
	$tree element create eSeparator rect -fill gray20 -height 2
	$tree style create sSeparator
	$tree style elements sSeparator eSeparator
	$tree style layout sSeparator eSeparator -expand ns -iexpand x -padx 1

	#
	# Cancel style
	#
	$tree element create eCancel text -fill White -text [mc "Cancel"]
	$tree style create sCancel
	$tree style elements sCancel {eSel eCancel}
	$tree style layout sCancel eSel -detach yes -iexpand xy
	$tree style layout sCancel eCancel -expand we -pady 1

	$tree column create -itemstyle STYLE -tags COL

	$tree notify bind $tree <Selection> "NSStore::PopupSelectionChanged $oop"

	bind $tree <Motion> "NSStore::PopupMotion $oop %x %y"
	bind $win <Leave> "NSStore::PopupLeave $oop"

	bind $tree <Control-ButtonPress-1> { ; }
	bind $tree <Shift-ButtonPress-1> { ; }
#	bind $tree <ButtonPress-1> "NSStore::PopupButton1 $oop %x %y"
	bind $tree <ButtonRelease-1> "NSStore::PopupButton1 $oop %x %y"

	bind $win <KeyPress-Escape> "\[NSStore::Info $oop popup,posted] hidemenu"

	pack $tree -expand yes -fill both -padx 2 -pady 2
	pack $frame -expand yes -fill both

	Info $oop popup,win $win
	Info $oop popup,tree $tree
	Info $oop popup,posted ""
	Info $oop popup,afterId ""

	return
}

# NSStore::PopupSelectionChanged --
#
#	Handle <Selection>.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::PopupSelectionChanged {oop} {

	set tree [Info $oop popup,tree]

	if {[$tree selection count] == 1} {
		set item [$tree selection get]
		if {[$tree item tag expr $item hook]} {
			PopupCallHook $oop select {*}[Info $oop popup,match,$item]
		}
	}

	return
}

# NSStore::PopupMotion --
#
#	Handle <Motion>.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::PopupMotion {oop x y} {

	set tree [Info $oop popup,tree]

	set id [$tree identify $x $y]
	if {$id eq "" || [lindex $id 0] ne "item"} {
		$tree selection clear
		return
	}
	set item [lindex $id 1]
	$tree selection modify $item all

	return
}

# NSStore::PopupLeave --
#
#	Handle <Leave>.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::PopupLeave {oop} {

	if {[Info $oop popup,afterId] ne ""} {
		after cancel [Info $oop popup,afterId]
	}
	Info $oop popup,afterId [after 50 "NSStore::CheckWhoHasCursor $oop"]

	return
}

proc NSStore::CheckWhoHasCursor {oop} {

	Info $oop afterId ""

	if {[NSUtils::HasCursor [Info $oop popup,win] 1]} return

	set tree [Info $oop popup,tree]
	$tree selection clear

	if {[Info $oop popup,posted] ne ""} {
		if {![NSUtils::HasCursor [Info $oop popup,posted]]} {
			[Info $oop popup,posted] hidemenu
		}
	}

	return
}

# NSStore::PopupButton1 --
#
#	Handle <ButtonPress-1>.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::PopupButton1 {oop x y} {

	set tree [Info $oop popup,tree]

	set id [$tree identify $x $y]
	if {$id eq "" || [lindex $id 0] ne "item"} {
		return
	}
	set item [lindex $id 1]
	if {[$tree item tag expr $item hook]} {
		PopupCallHook $oop invoke {*}[Info $oop popup,match,$item]
		Info $oop result 1
		[Info $oop popup,posted] hidemenu
	} elseif {[$tree item enabled $item]} {
		if {[Info $oop popup,cancel]} {
			angband keypress \033
		}
		Info $oop result 1
		[Info $oop popup,posted] hidemenu
	}

	return
}

# NSStore::PopupCallHook --
#
#	Call the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::PopupCallHook {oop message args} {

	switch -- $message {
		select {
			lassign $args invOrEquip index
			NSRecall::RecallObject $invOrEquip $index
		}
		invoke {
			lassign $args invOrEquip index keypress
			angband keypress $keypress
		}
	}

	return
}

# NSStore::PopupNewItem --
#
#	Create a new item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::PopupNewItem {oop char string color args} {

	set tree [Info $oop popup,tree]
	set item [$tree item create -parent root -tags hook]
	$tree item element configure $item COL eChar -text "$char) "
	$tree item element configure $item COL eTxt -text $string -fill $color

	Info $oop popup,match,$item $args

	return $item
}

# NSStore::PopupNewSeparator --
#
#	Create a new separator item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::PopupNewSeparator {oop} {

	set tree [Info $oop popup,tree]
	set item [$tree item create -parent root -height 6]
	$tree item enabled $item no
	$tree item style set $item COL sSeparator

	return $item
}

# NSStore::PopupHide --
#
#	Hide the popup menu. Called by win98button.menuhidecmd.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore::PopupHide {oop} {

	if {[Info $oop popup,afterId] ne ""} {
		after cancel [Info $oop popup,afterId]
		Info $oop popup,afterId ""
	}
	wm withdraw [Info $oop popup,win]
	Info $oop popup,posted ""
	NSUtils::GrabRelease [Info $oop win]
	[Info $oop win].statusBar cover hide
#	set ::NSStore($oop,result) 1

	return
}

proc StoreObj {command args} {
	return [NSStore::$command [Global store,oop] {*}$args]
}
