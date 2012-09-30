# File: store2.tcl

# Purpose: the (new) Store Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSStore2 {

	variable MenuString
	variable Priv

# namespace eval NSStore2
}

# NSStore2::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::InitModule {} {

	InitImageIfNeeded Image_ButtonOptions button-options.gif
	InitImageIfNeeded Image_ButtonHelp button-help.gif
	InitImageIfNeeded Image_Binding dg_binding.gif
	InitImageIfNeeded Image_Shelf shelf.gif

#	NSModule::LoadIfNeeded NSBalloon
	NSModule::LoadIfNeeded NSToolbar

	NSObject::New NSStore2

	return
}

# NSStore2::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::CloseModule {} {

	catch {
		destroy [Window store2]
	}

	return
}

# NSStore2::NSStore2 --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::NSStore2 {oop} {

	Info $oop busy 0
	Info $oop dragging 0
	Info $oop highlight,where ""
	Info $oop highlight,index ""
	Info $oop select,where ""
	Info $oop select,index ""
	Info $oop toolbar,match {}
	Info $oop request {}
	Info $oop request,id ""
	Info $oop skipMore 0

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow store2 $win \
		"GetDefaultGeometry $win reqwidth reqheight" \
		"NSStore2::SetupCmd $oop" \
		"NSStore2::DisplayCmd $oop"

	# not true on X11
	if {[Platform windows]} {
	
		### Tk 8.3.0 BUG!!!
		### "wm resizable" always returns "1 1" if a toplevel has a menu.
		### Since the Store Window is not resizable, we must clear any
		### requested height/width for the window. See NSWindowManager::Setup().
	
		proc ::NSStore2::SetupCmd {oop} {
			set geometry $NSWindowManager::Priv(store2,geomRequest)
			if {[string length $geometry]} {
				if {[scan $geometry {%dx%d%[+-]%d%[+-]%d} width height xs x ys y] == 6} {
					set win [Info $oop win]
					set width [winfo reqwidth $win]
					set height [winfo reqheight $win]
					set NSWindowManager::Priv(store2,geomRequest) ${width}x$height$xs$x$ys$y
				}
			}
			return
		}
		set NSWindowManager::Priv(store2,setupCmd) "NSStore2::SetupCmd $oop"
	}

	# Update ourself when the font,statusBar value changes
	Info $oop clientId,font,statusBar \
		[NSValueManager::AddClient font,statusBar \
			"NSStore2::ValueChanged_font_statusBar $oop"]

	# Destroy the object along with the widget (later)
	NSUtils::DestroyObjectWithWidget NSStore2 $oop $win

	#
	# Global list of application windows
	#

	Global store2,oop $oop
	Window store2 $win

	return
}

# NSStore2::~NSStore2 --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::~NSStore2 {oop} {

	trace vdelete ::NSStore2($oop,quantity) w "::NSStore2::QuantityChanged $oop"
	NSValueManager::RemoveClient font,statusBar [Info $oop clientId,font,statusBar]

	return
}

# NSStore2::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::Info {oop info args} {

	global NSStore2

	# Verify the object
	NSObject::CheckObject NSStore2 $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSStore2($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSStore2($oop,$info)
			}
		}
	}

	return
}

# NSStore2::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::InitWindow {oop} {

	set win .store2_$oop
	toplevel $win
	wm title $win Store

	wm resizable $win no no
	wm transient $win [Window main]

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSStore2::Close $oop"

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
		-menucommand "NSStore2::Win98MenuCmd_Options $oop"
	NSToolbar::AddTool $toolId -image Image_ButtonHelp \
		-showlabel no -command "DoCommandIfAllowed ?"

	MakeDivider [NSToolbar::Info $toolId frame].divider1 y
	pack [NSToolbar::Info $toolId frame].divider1 -side left -fill y -pady 2
	
	set id [NSToolbar::AddTool $toolId -image Image_ButtonHelp \
		-command "DoCommandIfAllowed p" -label "Buy" -showlabel yes \
		-showimage no -heightlabel 16 -hasmenu yes \
		-menucommand "NSStore2::Win98MenuCmd_Buy $oop"]
	
	set id2 [NSToolbar::AddTool $toolId -image Image_ButtonHelp \
		-command "DoCommandIfAllowed s" -label "Sell" -showlabel yes \
		-showimage no -heightlabel 16 -hasmenu yes \
		-menucommand "NSStore2::Win98MenuCmd_Sell $oop"]
	
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
	label $frame.howMany \
		-font $font -text "Quantity:"
	entry $frame.quantity \
		-width 2 -state disabled -textvariable NSStore2($oop,quantity)
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

	# Pack these here so the initial geometry is correct. Otherwise
	# the requested height is wrong
	pack $frame.howMany \
		-side left -expand no -padx 2
	pack $frame.quantity \
		-side left -expand no -padx 2

	# Typing Enter in the Quantity Entry initiates a purchase
	bind $frame.quantity <KeyPress-Return> \
		"NSStore2::InvokeSelected $oop"

	label $frame.price_character \
		-font $font -text "" -anchor w
	label $frame.price_owner \
		-font $font -text "" -anchor w

	trace variable ::NSStore2($oop,quantity) w \
		"::NSStore2::QuantityChanged $oop"

	MakeDivider $win.divider2 x

	#
	# Canvas
	#

	set canvas $win.canvas
	canvas $canvas \
		-borderwidth 0 -highlightthickness 0 \
		-background [format #%02x%02x%02x 0 0 153]

	Info $oop canvas $canvas

	if {[Platform unix]} {
		set font {Helvetica 12 bold}
	}
	if {[Platform windows]} {
		set font {Helvetica 10 bold}
	}
	set fontHeight [font metrics $font -linespace]
	set heightHeader [expr {1 + $fontHeight + 1}]

	set heightBox [expr {[icon size] + 8}]
	set widthBox [expr {[icon size] + 8}]
	set heightBoxMax 40
	set widthBoxMax 40

	set heightImage [image height Image_Shelf]
	set heightRow [expr {6 + $heightBox + 2 + $heightImage}]
	set heightStore [expr {3 + ($heightHeader - 1) + 4 * $heightRow + 6 + 3}]

	set padRing 2
	set widthStore [expr {3 + 6 * (6 + $widthBoxMax) + 6 + 3 + $padRing}]

	# Store items
	MakeBorder $oop $canvas 0 0 $widthStore $heightStore
	MakeHeader $oop $canvas 0 0 $widthStore Store store,title

	set columns [expr {($widthStore - $padRing - 3 - 6 - 3) / ($widthBox + 6)}]
	set diff [expr {($widthStore - $padRing - 3 - 6 - 3) - $columns * ($widthBox + 6)}]

	set index 0
	for {set row 0} {$row < 4} {incr row} {
		for {set col 0} {$col < $columns} {incr col} {
			MakeBox $oop $canvas store $index

			set x [expr {$diff / 2 + 3 + 6 + $col * ($widthBox + 6) + $widthBox / 2}]
			set y [expr {3 + ($heightHeader - 1) + 6 + $row * $heightRow + $heightBox / 2}]
			Info $oop store,x,$index $x
			Info $oop store,y,$index $y

			# Shelf image
			set x [expr {3 + 6 - 1}]
			set y [expr {3 + ($heightHeader - 1) + 6 + $row * $heightRow + $widthBox + 2}]
			$canvas create image $x $y -image Image_Shelf -anchor nw

			if {[incr index] == 24} break
		}

		if {$index == 24} break
	}

	# Inventory
	set widthInven [expr {$padRing + 3 + 5 * ($widthBoxMax + 6) + 6 + 3}]

	set x [expr {$widthStore + 1}]
	MakeBorder $oop $canvas $x 0 $widthInven $heightStore
	MakeHeader $oop $canvas $x 0 $widthInven "Inventory" inventory,title

	set left [expr {$x + $padRing + 3 + 6}]
	set top [expr {3 + ($heightHeader - 1) + 6}]
	set columns [expr {($widthInven - $padRing - 3 - 6 - 3) / ($widthBox + 6)}]
	set diff [expr {($widthInven - $padRing - 3 - 6 - 3) - $columns * ($widthBox + 6)}]

	# 23 items
	set col 0
	set row 0
	for {set i 0} {$i <= 23} {incr i} {
		MakeBox $oop $canvas inventory $i

		set x [expr {$diff / 2 + $left + $col * ($widthBox + 6) + $widthBox / 2}]
		set y [expr {$top + $row * ($heightBox + 6) + $heightBox / 2}]
		Info $oop inventory,x,$i $x
		Info $oop inventory,y,$i $y
		if {[incr col] == $columns} {
			set col 0
			incr row
		}
	}

	bind $canvas <ButtonPress-1> \
		"NSStore2::Button1 $oop %x %y"
	bind $canvas <Button1-Motion> \
		"NSStore2::Motion1 $oop %x %y"
	bind $canvas <ButtonRelease-1> \
		"NSStore2::Release1 $oop %x %y"

	Info $oop width,store $widthStore

	#
	# Statusbar
	#

	# Store statusbar
	MakeStatus $oop $canvas 0 $heightStore $widthStore store

	# Inventory statusbar
	set x [expr {$widthStore + 1}]
	MakeStatus $oop $canvas $x $heightStore $widthInven inventory

	set font [Value font,statusBar]
	set fontHeight [font metrics $font -linespace]
	set heightStatus [expr {3 + $fontHeight + 3}]

	set widthTotal [expr {$widthStore + 1 + $widthInven}]
	set heightTotal [expr {$heightStore + $heightStatus}]
	$canvas configure -width $widthTotal -height $heightTotal

	# Divider filler
	set x $widthStore
	$canvas create line $x 0 $x $heightTotal -fill #282828

	# Hack -- ring bindings down the middle
	set deltaY 34
	set y [expr {3 + ($heightHeader - 1) + 6 + 1}]
	set max [expr {($heightStore - 6 - 6) / 34}]
	for {set i 0} {$i < $max} {incr i} {
		set itemId [$canvas create image $x $y -image Image_Binding -anchor n]
#		$canvas bind $itemId <ButtonPress-1> {dbwin "[%W coords current]\n"}
		$canvas bind $itemId <ButtonPress-1> {}
		incr y $deltaY
	}

	# This box is used for drag & drop
	MakeBox $oop $canvas drag 0
	HideBox $oop drag 0

	# Help-text statusbar (normally hidden)
	MakeStatus $oop $canvas 0 $heightStore $widthTotal statusBar
	$canvas itemconfigure statusBar,status -state hidden

	# Alternate balloon impl
	$canvas create rectangle 0 0 10 10 -fill White -state hidden \
		-tags {balloon balloon,rect}
	$canvas create text 0 0 -anchor n -state hidden -tags {balloon balloon,text}

	#
	# Geometry
	#

	grid rowconfig $win 0 -weight 0
	grid rowconfig $win 1 -weight 0
	grid rowconfig $win 2 -weight 0
	grid rowconfig $win 3 -weight 1
	grid columnconfig $win 0 -weight 1

	pack forget [NSToolbar::Info $toolId frame]
	grid [NSToolbar::Info $toolId frame] \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.info \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.divider2 \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.canvas \
		-row 3 -column 0 -rowspan 1 -columnspan 1 -sticky news

	#
	# Context Menu
	#
	
	set m $win.context
	menu $m -tearoff 0
	bind $canvas <Button-3> \
		"NSStore2::ContextMenu $oop $m %X %Y"

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
		
	return
}

# NSStore2::InitMenus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::InitMenus {oop} {

	variable MenuString

	set win [Info $oop win]

	#
	# Menu bar
	#

	set mbar [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSStore2::SetupMenus $oop" -identifier MENUBAR]
	Info $oop mbarId $mbar

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSStore2::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbar invokeCmd "NSStore2::MenuInvoke $oop"

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

# NSStore2::Synch --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::Synch {oop} {

	set toolbarId [Info $oop toolbarId]

	#
	# Take/Buy button
	#

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

	#
	# Drop/Sell button
	#

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

# NSStore2::SetupMenus --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::SetupMenus {oop mbarID} {

	variable Priv

	set identList {}

	if {[string equal [angband inkey_flags] INKEY_CMD]} {
		lappend identList E_STORE_BUY E_STORE_SELL E_STORE_EXIT
		if {[angband store ishome]} {
			lappend identList E_DESTROY
		}
	}

	NSMenu::MenuEnable $mbarID $identList

	[Info $oop canvas] itemconfigure statusBar,status -state ""

	return
}

# NSStore2::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::MenuSelect {oop menuId index ident} {

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

	[Info $oop canvas] itemconfigure statusBar,status,left -text $desc
	if {![string length $desc]} {
		if {$menuId == [Info $oop mbarId]} {
			[Info $oop canvas] itemconfigure statusBar,status -state hidden
		}
	}

	return
}

# NSStore2::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::MenuInvoke {oop menuId ident} {

	variable Priv

	switch -glob -- $ident {
		E_STORE_BUY {DoCommandIfAllowed p}
		E_STORE_SELL {DoCommandIfAllowed s}
		E_DESTROY {DoCommandIfAllowed K}
		E_STORE_EXIT {DoCommandIfAllowed \033}
	}

	return
}

# NSStore2::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::DisplayCmd {oop message first} {

	set win [Info $oop win]
	
	switch -- $message {
		preDisplay {
			ConfigureWindow $oop

			SetList $oop

			SetList_Inventory $oop
		}
		reDisplay {
			SetList $oop
		}
		postDisplay {
			if {0 && ![Value warning,store,window]} {
				tk_messageBox -parent [Info $oop win] -title "Game Change" \
					-message "This is the new graphical Store Window.\
					If you want the list instead, choose \"List Mode\" from\
					the options button in the toolbar."
				Value warning,store,window 1
			}
		}
		postWithdraw {
			Info $oop busy 0
		}
	}

	return
}

# NSStore2::SetupCmd --
#
#	Called by NSWindowManager::Setup().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::SetupCmd {oop} {

	set win [Info $oop win]

	set frame $win.info
	$frame configure -height [winfo reqheight $frame]
	pack propagate $frame no

	return
}

# NSStore2::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::Close {oop} {

	angband keypress \033

	return
}

# NSStore2::Win98MenuCmd_Options --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::Win98MenuCmd_Options {oop button} {

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

	$menu add command -label "List Mode" -command {Store2Obj Swap}
	lappend descList "Use the list window"

	Info $oop toolbar,mode option
	Info $oop toolbar,match $keywordList
	Info $oop toolbar,desc $descList

	[Info $oop canvas] itemconfigure statusBar,status -state ""

	tk_popup $menu $x $y

	if {[Platform unix]} {
		tkwait variable ::tkPriv(popup)
	}

	after idle "$button hidemenu ; [Info $oop canvas] itemconfigure statusBar,status -state hidden"

	return
}

# NSStore2::SettingChanged --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::SettingChanged {oop keyword value} {

	# Ignore settings which don't affect the display
	if {[lsearch -exact [Info $oop toolbar,match] $keyword] == -1} return

	# Update the button
	update idletasks

	SetList $oop
	SetList_Inventory $oop

	return
}

# NSStore2::Win98MenuCmd_Buy --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::Win98MenuCmd_Buy {oop button} {

	variable MenuString

	set canvas [$button info canvas]
	set x [winfo rootx $canvas]
	set y [expr {[winfo rooty $canvas] + [winfo height $canvas]}]

	set menu [Info $oop toolbar,menu]
	if {[Platform unix]} {
		set font [$menu cget -font]
		$menu configure -font {Helvetica 10}
	}

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

	[Info $oop canvas] itemconfigure statusBar,status -state ""

	tk_popup $menu $x $y

	if {[Platform unix]} {
		tkwait variable ::tkPriv(popup)
		$menu configure -font $font
	}

	after idle "$button hidemenu ; [Info $oop canvas] itemconfigure statusBar,status -state hidden"

	return
}

# NSStore2::Win98MenuCmd_Sell --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::Win98MenuCmd_Sell {oop button} {

	variable MenuString

	set canvas [$button info canvas]
	set x [winfo rootx $canvas]
	set y [expr {[winfo rooty $canvas] + [winfo height $canvas]}]

	set menu [Info $oop toolbar,menu]
	if {[Platform unix]} {
		set font [$menu cget -font]
		$menu configure -font {Helvetica 10}
	}

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

	[Info $oop canvas] itemconfigure statusBar,status -state ""

	tk_popup $menu $x $y

	if {[Platform unix]} {
		tkwait variable ::tkPriv(popup)
		$menu configure -font $font
	}

	after idle "$button hidemenu ; [Info $oop canvas] itemconfigure statusBar,status -state hidden"

	return
}

# NSStore2::ConfigureWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::ConfigureWindow {oop} {

	set win [Info $oop win]
	set frame $win.info

	eval pack forget [winfo children $frame]

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

# NSStore2::HaggleSetup --
#
#	Shows or hides display elements during the buy/sell process.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::HaggleSetup {oop action} {

	set win [Info $oop win]
	set frame $win.info

	switch -- $action {

		haggle_open {
			$frame.price_character configure -text ""
			pack forget $frame.howMany $frame.quantity $frame.howMuch \
				$frame.totalCost $frame.purse
			pack $frame.price_owner \
				-side left -padx 2
			pack $frame.price_character \
				-side left -padx 24
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
			pack $frame.purse \
				-side right -expand no -padx 2
		}
	}

	# Stop skipping -more- prompts
	SkipMoreMessages $oop 0

	return
}

# NSStore2::SetList --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::SetList {oop} {

	variable Priv

	set win [Info $oop win]
	set canvas [Info $oop canvas]

	# Clear and disable the "quantity" entry
	Info $oop quantity ""
	$win.info.quantity configure -state disabled

	# Get the store name
	set storename [angband store storename]

	# In the Home
	if {[angband store ishome]} {

		# Set the window title
		wm title $win "Your Home"
		$canvas itemconfigure store,title -text "Your Home"

	# Not in the Home
	} else {

		# Set the window title
		wm title $win [format "The %s owned by %s" \
			$storename [angband store ownername]]
		$canvas itemconfigure store,title -text "The $storename"
	}

	# Get the number of items
	set count [angband store count]

	# Clear the selection
	SelectionChanged $oop "" ""

	# Clear the list
	for {set i 0} {$i < 24} {incr i} {
		MoveBox $oop -20 -20 store $i
	}

	set index 0
	for {set item 0} {$item < $count} {incr item} {
		angband store info $item attrib
		$canvas itemconfigure icon,store,$item -assign $attrib(icon)

		set x [Info $oop store,x,$index]
		set y [Info $oop store,y,$index]
		MoveBox $oop $x $y store $item
		incr index

		Info $oop char,store,$item $attrib(char)
	}

	# Display number of objects
	if {$count == 1} {
		set string [format "%d item" $count]
	} else {
		set string [format "%d items" $count]
	}
	$canvas itemconfigure store,status,left -text $string

	Synch $oop

	return
}

# NSStore2::SetList_Inventory --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::SetList_Inventory {oop} {

	set canvas [Info $oop canvas]

	# Clear the selection
	SelectionChanged $oop "" ""

	# Clear the list
	for {set i 0} {$i <= 23} {incr i} {
		MoveBox $oop -20 -20 inventory $i
	}
	$canvas itemconfigure inventory,status,left -text ""
	$canvas itemconfigure inventory,status,right -text ""

	set itemList [angband inventory find -tester yes]

	# Display items the store will buy in a different color
	set buyList [angband inventory find -store_will_buy yes]

	set weight 0
	set index 0
	foreach item $itemList {
		angband inventory info $item attrib
		$canvas itemconfigure icon,inventory,$item -assign $attrib(icon)

		if {[lsearch -exact $buyList $item] != -1} {
			set rgb1 [format #%02x%02x%02x 0 124 0]
			set rgb2 [format #%02x%02x%02x 51 204 0]
		} else {
			set rgb1 [format #%02x%02x%02x 102 51 0]
			set rgb2 [format #%02x%02x%02x 255 153 0]
		}
		$canvas itemconfigure border,inventory,$item -outline $rgb1
		Info $oop color,inventory,$item $rgb2

		set x [Info $oop inventory,x,$index]
		set y [Info $oop inventory,y,$index]
		MoveBox $oop $x $y inventory $item
		incr index

		Info $oop char,inventory,$item $attrib(char)
		incr weight [expr {$attrib(weight) * $attrib(number)}]
	}

	Info $oop inventory,weight $weight

	$canvas itemconfigure inventory,status,right -text [fmt_wgt $weight 1]

	set numItems [llength $itemList]
	if {$numItems == 1} {
		set string [format "%d item" $numItems]
	} else {
		set string [format "%d items" $numItems]
	}
	$canvas itemconfigure inventory,status,left -text $string

	return
}

# NSStore2::ContextMenu --
#
#	When the store list is right-clicked, pop up a context
#	menu of actions.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::ContextMenu {oop menu x y} {

	set canvas [Info $oop canvas]

	set font [$menu cget -font]

	# Clear the menu
	$menu delete 0 end

	set itemId [$canvas find withtag current]
	if {[llength $itemId]} {
		set tags [$canvas gettags $itemId]
		set index [lsearch $tags icon,*,*]
		if {$index != -1} {
			scan [lindex $tags $index] {icon,%[^,],%s} where index

			scan [$canvas bbox sel,$where,$index] "%s %s %s %s" left top right bottom
			set x [expr {[winfo rootx $canvas] + ($right + 1) + 2}]
			set y [expr {[winfo rooty $canvas] + $top}]

			BalloonHide $oop
		} else {
			set where ""
			set index ""
		}
	} else {
		set where ""
		set index ""
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

	# No item is hit
	if {![string length $where]} {

		# We are waiting for a command
		if {[string equal [angband inkey_flags] INKEY_CMD]} {
		
			$menu add command -label "$stringBuy An Item" \
				-command "angband keypress $charBuy"
			$menu add command -label "$stringSell An Item" \
				-command "angband keypress $charSell"
			$menu add separator
			$menu add command -label "Leave" \
				-command "angband keypress \033"
			$menu add separator
			$menu add command -label "Cancel"
	
			# Pop up the menu
			tk_popup $menu $x $y
		}
		
		# Done
		return
	}

	# Get information about this item
	angband $where info $index attrib

	# Get the item char
	set itemKey $attrib(char)

	# Get the tval
	set itemTval $attrib(tval)

	# Get the amount
	set itemAmount $attrib(number)

	# An inventory item is hit
	if {[string compare $where store]} {

		# We are waiting for an inventory item
		if {[string equal [angband inkey_flags] INKEY_ITEM]} {
	
			# Append a command to select the item
			set command "angband keypress $itemKey"
	
			$menu add command -label "Select This Item" \
				-command $command -font [BoldFont $font]
			$menu add separator
			$menu add command -label "Cancel" \
				-command "angband keypress \033"
	
			# Pop up the menu
			tk_popup $menu $x $y
	
			# Done
			return
		}

		# We are not waiting for a command
		if {[string compare [angband inkey_flags] INKEY_CMD]} {
			return
		}

		# See if the store will accept this item
		set match [angband inventory find -store_will_buy yes]
		if {[lsearch -exact $match $index] != -1} {
			if {$itemAmount > 1} {
				$menu add command -label "$stringSell One" \
					-command "angband keypress 01$charSell$itemKey" \
					-font [BoldFont $font]
				if {$itemAmount > 5} {
					$menu add command -label "$stringSell Five" \
						-command "angband keypress 05$charSell$itemKey"
				}
				$menu add command -label "$stringSell All" \
					-command "angband keypress 0$itemAmount$charSell$itemKey"
			} else {
				$menu add command -label "$stringSell This Item" \
					-command "angband keypress $charSell$itemKey" \
					-font [BoldFont $font]
			}
			$menu add separator
			$menu add command -label "$stringBuy An Item" \
				-command "angband keypress $charBuy"
		} else {
			$menu add command -label "$stringBuy An Item" \
				-command "angband keypress $charBuy"
			$menu add command -label "$stringSell An Item" \
				-command "angband keypress $charSell"
		}
		$menu add separator
		$menu add command -label "Leave" \
			-command "angband keypress \033"
		$menu add separator
		$menu add command -label "Cancel"

		# Pop up the menu
		tk_popup $menu $x $y

		# Done
		return
	}

	# We are waiting for a store item
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
			-command "angband keypress $charBuy$itemKey" \
			-font [BoldFont $font]
	} else {
		$menu add command -label "$stringBuy One" \
			-command "angband keypress 01$charBuy$itemKey" \
			-font [BoldFont $font]
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

# NSStore2::Invoke_Store --
#
#	Called when a store item is double-clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::Invoke_Store {oop where index} {

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
	angband store info $index attrib

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

# NSStore2::Invoke_Inventory --
#
#	Called when an inventory item is double-clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::Invoke_Inventory {oop where index} {

	set char [Info $oop char,$where,$index]

#	angband $where info $index attrib

	if {[string equal [angband inkey_flags] INKEY_CMD]} {

		# If this item can be sold, set the default action to "Sell"
		set match [angband inventory find -store_will_buy yes]
		if {[lsearch -exact $match $index] != -1} {
			set cmdChar s
			angband keypress $cmdChar$char
		}
	}

	if {[string equal [angband inkey_flags] INKEY_ITEM]} {
		angband keypress $char
	}

	return
}

# NSStore2::InvokeByReturn --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::InvokeSelected {oop} {

	# Hack -- Focus on the list
	focus [Info $oop canvas]

	# Get the selection
	set where [Info $oop select,where]
	set index [Info $oop select,index]
	if {![string length $where]} return

	# Invoke
	Invoke $oop $where $index

	return
}

# NSStore2::Invoke --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::Invoke {oop where index} {

	switch -- $where {
		inventory {Invoke_Inventory $oop $where $index}
		store {Invoke_Store $oop $where $index}
	}

	return
}

# NSStore2::Select --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::Select {oop where index} {

	if {[string equal $where [Info $oop select,where]] &&
		[string equal $index [Info $oop select,index]]} {
#		SelectionChanged $oop "" ""
		return
	}
	SelectionChanged $oop $where $index

	return
}

# NSStore2::SelectionChanged --
#
#	Called when the list selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::SelectionChanged {oop where index} {

	if {[string length [Info $oop select,where]]} {
		Highlight $oop 0 [Info $oop select,where] [Info $oop select,index]
	}

	# Nothing was selected
	if {![string length $index]} {
		Info $oop select,where ""
		Info $oop select,index ""
		return
	}

	# Remember the selected item
	Info $oop select,where $where
	Info $oop select,index $index

	Highlight $oop 1 $where $index
	BalloonHide $oop

	return
}

# NSStore2::QuantityChanged --
#
#	Trace variable callback on NSStore2($oop,quantity). When the number
#	of items to buy changes, synch the "total price" field.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::QuantityChanged {oop name1 name2 op} {

	set win [Info $oop win]
	set quantity [Info $oop quantity]
	set where [Info $oop highlight,where]
	set index [Info $oop highlight,index]

	if {![string length $where] || [string compare $where store] || \
		![string length $quantity]} {
		$win.info.totalCost configure -text ""
		return
	}

	angband store info $index attrib

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

# NSStore2::Button1 --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::Button1 {oop x y} {

	set canvas [Info $oop canvas]

	focus $canvas

	set itemId [$canvas find withtag current]
	if {![llength $itemId] || [string equal [$canvas type current] image]} {
		if {[string length [Info $oop select,index]]} {
			SelectionChanged $oop "" ""
		}
		return	
	}

	Info $oop press,x $x
	Info $oop press,y $y

	return
}

# NSStore2::Motion1 --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::Motion1 {oop x y} {

	set canvas [Info $oop canvas]
	set dragging [Info $oop dragging]
	set where [Info $oop select,where]
	set index [Info $oop select,index]

	# No item is selected
	if {![string length $index]} return

	# Not dragging
	if {!$dragging} {
		set x2 [Info $oop press,x]
		set y2 [Info $oop press,y]
		if {(abs($x - $x2) < 10) && (abs($y - $y2) < 10)} return

		# Copy the image from the dragged item
		set assign [$canvas itemcget icon,$where,$index -assign]
		$canvas itemconfigure icon,drag,0 -assign $assign

		# Copy colors from the drag item
		set rgb1 [$canvas itemcget border,$where,$index -outline]
		$canvas itemconfigure border,drag,0 -outline $rgb1
		set rgb2 [Info $oop color,$where,$index]
		$canvas itemconfigure sel,drag,0 -outline $rgb2

		# Display the drag feedback
		ShowBox $oop drag 0

		# Drag in progress
		Info $oop dragging 1
	}

	if {$x - 20 < 0} {
		set x 20
	} elseif {$x + 20 > [winfo width $canvas]} {
		set x [expr {[winfo width $canvas] - 20}]
	}

	if {$y - 20 < 0} {
		set y 20
	} elseif {$y + 20 > [winfo height $canvas]} {
		set y [expr {[winfo height $canvas] - 20}]
	}

	MoveBox $oop $x $y drag 0

	return
}

# NSStore2::Release1 --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::Release1 {oop x y} {

	set canvas [Info $oop canvas]
	set where [Info $oop select,where]
	set index [Info $oop select,index]

	if {![Info $oop dragging]} return
	HideBox $oop drag 0
	Info $oop dragging 0

	set charItem [Info $oop char,$where,$index]
	if {$x < [Info $oop width,store]} {
		set dest store
	} else {
		set dest inventory
	}
	if {[string equal $where $dest]} return
	switch -- $where {
		inventory {
			DoUnderlyingCommand s$charItem
			SkipMoreMessages $oop 1
		}
		store {
			angband keypress "0[Info $oop quantity]p$charItem"
			SkipMoreMessages $oop 1
		}
	}

	return
}

# NSStore2::Highlight --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::Highlight {oop state where index} {

	if {[Info $oop busy]} return
	Info $oop busy 1

	set canvas [Info $oop canvas]
	set entry [Info $oop win].info.quantity

	if {$state} {

		# Highlight
		$canvas itemconfigure sel,$where,$index \
			-outline [Info $oop color,$where,$index]

		angband $where info $index attrib

		# Balloon
		scan [$canvas bbox sel,$where,$index] "%s %s %s %s" left top right bottom
		set x [expr {[winfo rootx $canvas] + $left + 3 + [icon size] / 2}]
		set y [expr {[winfo rooty $canvas] + ($bottom + 1) + 2}]
		set char [Info $oop char,$where,$index]
		set text "$char\) $attrib(name)"
		BalloonShow $oop $text $x $y

		# This must come before QuantityChanged()
		Info $oop highlight,where $where
		Info $oop highlight,index $index

		if {[string equal $where store]} {
			$entry configure -state normal
			Info $oop quantity "1"
		}

		if {[string equal $where store]} {
			set weight [fmt_wgt $attrib(weight) 1]
			$canvas itemconfigure $where,status,right -text $weight
		} else {
			set weight [expr {$attrib(weight) * $attrib(number)}]
			set weight [fmt_wgt $weight]
			set total [fmt_wgt [Info $oop $where,weight] 1]
			$canvas itemconfigure $where,status,right -text "$weight/$total"
		}

		NSRecall::RecallObject $where $index

	} else {

		# Un-highlight
		$canvas itemconfigure sel,$where,$index \
			-outline [Info $oop color2,$where,$index]

		BalloonHide $oop

		# This must come before QuantityChanged()
		Info $oop highlight,where ""
		Info $oop highlight,index ""

		if {[string equal $where store]} {
			Info $oop quantity ""
			$entry configure -state disabled
		}

		if {[string equal $where store]} {
			$canvas itemconfigure $where,status,right -text ""
		} else {
			$canvas itemconfigure $where,status,right \
				-text [fmt_wgt [Info $oop $where,weight] 1]
		}
	}

	Info $oop busy 0

	return	
}

# NSStore2::Enter --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::Enter {oop where index} {

	if {[string length [Info $oop select,where]]} return

	Highlight $oop 1 $where $index

	return
}

# NSStore2::Leave --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::Leave {oop where index} {

	if {![string length [Info $oop select,index]]} {
		Highlight $oop 0 $where $index
	}

	return
}

# NSStore2::Request --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::Request {oop type where index} {

	Info $oop request [list $type $where $index]
	if {![string length [Info $oop request,id]]} {
		Info $oop request,id [after 1 NSStore2::HandleRequest $oop]
	}

	return
}

# NSStore2::HandleRequest --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::HandleRequest {oop} {

	if {[Info $oop busy]} {
		Info $oop request,id [after 1 NSStore2::HandleRequest $oop]
		return
	}

	set request [Info $oop request]
	Info $oop request {}

	set type [lindex $request 0]
	set where [lindex $request 1]
	set index [lindex $request 2]

	if {[string length [Info $oop highlight,where]]} {
		Leave $oop [Info $oop highlight,where] [Info $oop highlight,index]
	}

	# Because this is called as an "after" script, it is possible that:
	#   a) the character has already left the store
	#   b) the item no longer exists
	# Both of these conditions can give an error, so we check for both.

	if {![angband store shopping]} {
		Info $oop request,id ""
		return
	}
	if {$index >= [angband $where count]} {
		Info $oop request,id ""
		return
	}

	if {[string equal $type enter]} {
		Enter $oop $where $index
	}

	# Clear this after above stuff. See Request()
	Info $oop request,id ""

	if {[llength [Info $oop request]]} {
		Info $oop request,id [after 1 NSStore2::HandleRequest $oop]
	}

	return
}

# NSStore2::BalloonShow --
#
#	Show the balloon.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::BalloonShow {oop text x y} {

	# XXX We used to use NSBalloon, but some users said the
	# balloon would disappear randomly.
	if {0} {
		NSBalloon::Show $text $x $y n
		return
	}

	set canvas [Info $oop canvas]

	incr x -[winfo rootx $canvas]
	incr y -[winfo rooty $canvas]

	set font [Global font,sys,normal]
	set textWidth [font measure $font $text]
	set textHeight [font metrics $font -linespace]

	set padx 4
	set pady 2

	set rectWidth [expr {$padx + $textWidth + $padx + 1}]
	set rectHeight [expr {$pady + $textHeight + $pady + 1}]

	# Not too far left
	if {$x - $rectWidth / 2 < 0} {
		incr x [expr {0 - ($x - $rectWidth / 2)}]

	# Not too far right
	} elseif {$x + $rectWidth / 2 > [winfo width $canvas]} {
		incr x [expr {[winfo width $canvas] - ($x + $rectWidth / 2)}]
	}
	$canvas coords balloon,rect [expr {$x - $rectWidth / 2}] $y \
		[expr {$x + $rectWidth / 2}] [expr {$y + $rectHeight}]
	$canvas coords balloon,text $x [expr {$pady + $y + 1}]
	$canvas itemconfigure balloon,text -text $text

	# Show the items
	$canvas itemconfigure balloon -state normal

	return
}

# NSStore2::BalloonHide --
#
#	Hide the balloon.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::BalloonHide {oop} {

	if {0} {
		NSBalloon::Hide
		return
	}

	set canvas [Info $oop canvas]

	# Hide the items
	$canvas itemconfigure balloon -state hidden

	return
}

# NSStore2::Track --
#
#	Handle <Track> quasi-event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::TrackInventory {oop} {

	SetList_Inventory $oop

	return
}

# NSStore2::ChooseItem --
#
#	Handle <Choose-item> quasi-event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::ChooseItem {oop show other} {

	if {[string compare $other inventory]} return

	Info $oop choose,show $show
	Info $oop choose,other $other
	if {$show} {
		Info $oop didChoose 0
	}

	return
}

# NSStore2::TermInkey --
#
#	Handle <Term-inkey> quasi-event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::TermInkey {oop} {

	if {[Info $oop choose,show]} {
		# Show the choices
		SetList_Inventory $oop
		
		Info $oop didChoose 1
	} elseif {![Info $oop didChoose]} {
		# Nothing
	} else {

		# Show all items
		SetList_Inventory $oop
	}

	return
}

# NSStore2::MakeBorder --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::MakeBorder {oop canvas x y width height} {

	set left [expr {$x + 1}]
	set top [expr {$y + 1}]
	set right [expr {$x + $width - 2}]
	set bottom [expr {$y + $height - 2}]
	
	# 3-pixel border
	$canvas create rectangle $left $top $right $bottom \
		-outline #282828 -fill {} -width 3.0

	# 1-pixel border
	$canvas create rectangle $left $top $right $bottom \
		-outline #0070FF -fill {}

	return
}

# NSStore2::MakeHeader --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::MakeHeader {oop canvas x y width text tags} {

	if {[Platform unix]} {
		set font {Helvetica 12 bold}
	}
	if {[Platform windows]} {
		set font {Helvetica 10 bold}
	}
	set fontHeight [font metrics $font -linespace]

	set left [expr {$x + 2}]
	set top [expr {$y + 2}]
	set right [expr {$x + $width - 3}]
	set bottom [expr {$top + 1 + $fontHeight + 1}]

	$canvas create rectangle $left $top $right $bottom \
		-outline #282828 -fill #0000D4
	$canvas create text [expr {$x + $width / 2}] [expr {$top + 1}] \
		-text $text -font $font -fill White -anchor n -tags $tags

	return
}

# NSStore2::MakeStatus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::MakeStatus {oop canvas x y width where} {

	set font [Value font,statusBar]
	set fontHeight [font metrics $font -linespace]
	set height [expr {3 + $fontHeight + 3}]

	# 3-pixel border + background
	$canvas create rectangle [expr {$x + 1}] [expr {$y + 1}] \
		[expr {$x + $width - 2}] [expr {$y + $height - 2}] \
		-outline #282828 -fill #0000D4 -width 3.0 \
		-tags [list statusBar,rect $where,status]

	# 1-pixel border
	$canvas create rectangle [expr {$x + 1}] [expr {$y + 1}] \
		[expr {$x + $width - 2}] [expr {$y + $height - 2}] \
		-outline #0070FF -fill {} \
		-tags [list statusBar,rect $where,status]

	# Left text
	$canvas create text [expr {$x + 4}] \
		[expr {$y + 3}] -anchor nw -fill gray -font $font \
		 -tags [list statusBar,text $where,status $where,status,left]

	# Right text
	$canvas create text [expr {$x + $width - 4}] \
		[expr {$y + 3}] -anchor ne -fill gray -font $font \
		 -tags [list statusBar,text $where,status $where,status,right]

	foreach side {left right} {
		$canvas bind $where,status,$side <Enter> \
			"NSStore2::BindStatus $oop enter $where $side"
		$canvas bind $where,status,$side <Leave> \
			"NSStore2::BindStatus $oop leave $where $side"
	}

	return
}

# NSStore2::BindStatus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::BindStatus {oop message where side} {

	if {$message == "leave"} {
		NSMainWindow::StatusText [Global main,oop] ""
		return
	}

	set string ""

	switch -- $where {
		inventory {
			switch -- $side {
				left {
					set string "Number of inventory items"
				}
				right {
					set string "Weight of inventory items"
				}
			}
		}
		store {
			switch -- $side {
				left {
					set string "Number of store items"
				}
				right {
				}
			}
		}
	}

	if {[string length $string]} {
		NSMainWindow::StatusText [Global main,oop] $string
	}

	return
}

# NSStore2::MakeBox --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::MakeBox {oop canvas where index {rgb1 ""} {rgb2 ""}} {

	set width [expr {[icon size] + 8}]
	set height [expr {[icon size] + 8}]

	if {![string length $rgb1]} {
		set rgb1 [format #%02x%02x%02x 102 51 0]
		set rgb2 [format #%02x%02x%02x 255 153 0]
	}

	# 3-pixel border
	set outline $rgb1
	set fill [format #%02x%02x%02x 68 68 68]
	$canvas create rectangle 2 2 \
		[expr {$width - 2}] [expr {$height - 2}] \
		-fill $fill -outline $outline -width 3.0 \
		-tags "$where,$index border,$where,$index"

	# 1-pixel border
	$canvas create rectangle 2 2 \
		[expr {$width - 2}] [expr {$height - 2}] \
		-fill {} -outline {} -tags "$where,$index sel,$where,$index"

	Info $oop color,$where,$index $rgb2
	Info $oop color2,$where,$index ""

if 0 {

	# Widget
	$canvas create widget [expr {$width / 2}] \
		[expr {$height / 2}] -assign {icon none 0} \
		-tags "icon $where,$index icon,$where,$index" -anchor center

	# No bindings on the drag box
	if {[string equal $where drag]} return

	$canvas bind icon,$where,$index <Enter> \
		"NSStore2::Request $oop enter $where $index"
	$canvas bind icon,$where,$index <Leave> \
		"NSStore2::Request $oop leave $where $index"
	$canvas bind icon,$where,$index <ButtonPress-1> \
		"NSStore2::Select $oop $where $index"
	$canvas bind icon,$where,$index <Double-ButtonPress-1> \
		"NSStore2::Invoke $oop $where $index"
}

	return
}

# NSStore2::MoveBox --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::MoveBox {oop x y where index} {

	set canvas [Info $oop canvas]

	scan [$canvas coords icon,$where,$index] "%s %s" cx cy
	$canvas move $where,$index [expr {$x - $cx}] [expr {$y - $cy}]

	return
}

# NSStore2::ShowBox --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::ShowBox {oop where index} {

	set canvas [Info $oop canvas]

	if {[info tclversion] >= 8.3} {
		$canvas itemconfigure $where,$index -state ""
	}

	return
}

# NSStore2::HideBox --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::HideBox {oop where index} {

	set canvas [Info $oop canvas]

	if {[info tclversion] >= 8.3} {
		$canvas itemconfigure $where,$index -state hidden
	} else {
		MoveBox $oop -20 -20 $where $index
	}

	return
}

# NSStore2::Swap --
#
#	Display the "old" Store Window
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::Swap {oop} {

	NSWindowManager::Undisplay store2

	NSModule::LoadIfNeeded NSStore
	NSWindowManager::Display store

	Value store,style old

	return
}

# NSStore2::ValueChanged_font_statusBar --
#
#	Called when the font,statusBar value changes.
#	Updates the Main Window statusbar.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::ValueChanged_font_statusBar {oop} {

	set win [Info $oop win]
	set canvas [Info $oop canvas]

	# Get the desired font
	set font [Value font,statusBar]

	# Update the font. Too bad there isn't a -fontvar font variable
	$canvas itemconfigure statusBar,text -font $font

	set fontHeight [font metrics $font -linespace]
	set height [expr {3 + $fontHeight + 3}]

	foreach itemId [$canvas find withtag statusBar,rect] {
		scan [$canvas coords $itemId] "%s %s %s %s" left top right bottom
		set bottom [expr {$top - 1 + $height - 2}]
		$canvas coords $itemId $left $top $right $bottom
	}

	scan [$canvas bbox statusBar,rect] "%d %d %d %d" left top right bottom
	$canvas configure -height $bottom

	wm geometry $win ""

	return
}

# NSStore2::SkipMoreMessages --
#
#	Skip -more- prompts during drag & drop,
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::SkipMoreMessages {oop skip} {

	if {$skip == [Info $oop skipMore]} {
		return
	}
	Info $oop skipMore $skip

	return
}

proc Store2Obj {command args} {
	return [eval NSStore2::$command [Global store2,oop] $args]
}
