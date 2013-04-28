# File: store2.tcl

# Purpose: the (new) Store Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
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

	MsgCatInit inven store

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
	PopupInit $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow store2 $win \
		"GetDefaultGeometry $win reqwidth reqheight" \
		"NSStore2::SetupCmd $oop" \
		"NSStore2::DisplayCmd $oop"

	# not true on X11
	if {0 && [Platform windows]} {

		### Tk 8.3.0 BUG!!!
		### "wm resizable" always returns "1 1" if a toplevel has a menu.
		### Since the Store Window is not resizable, we must clear any
		### requested height/width for the window. See NSWindowManager::Setup().

		### HOLY SHIT this bug was still there in 8.5.7... reported!

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

	# Update the display when some settings change
	qebind NSStore2 <Setting> {
		Store2Obj SettingChanged %d %c
	}
	qeconfigure NSStore2 <Setting> -active no

	qebind NSStore2 <Track-inventory> {
		Store2Obj TrackInventory
	}
	qeconfigure NSStore2 <Track-inventory> -active no

	qebind NSStore2 <Term-inkey> {
		Store2Obj TermInkey
	}
	qeconfigure NSStore2 <Term-inkey> -active no

	qebind NSStore2 <Choose-item> {
		Store2Obj ChooseItem %s %o
	}
	qeconfigure NSStore2 <Choose-item> -active no

	qebind NSStore2Synch <Term-inkey> "NSStore2::Synch $oop"
	qeconfigure NSStore2Synch <Term-inkey> -active no

	# Update ourself when the font,store value changes
	qebind NSStore2 <Value-font,store> \
		"NSStore2::ValueChanged_font_store $oop"

	# Update ourself when the font,statusBar value changes
	qebind NSStore2 <Value-font,statusBar> \
		"NSStore2::ValueChanged_font_statusBar $oop"

	qebind NSStore2 <IconCfg> \
		"NSStore2::IconCfg $oop"

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

	trace remove variable ::NSStore2($oop,quantity) write "::NSStore2::QuantityChanged $oop"

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

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSStore2::Close $oop"

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

	set id [NSToolbar::AddTool $toolId \
		-command "DoCommandIfAllowed p" -label [mc "Buy"] -showlabel yes \
		-showimage no -heightlabel 16 -hasmenu yes \
		-menucommand "NSStore2::Win98MenuCmd_Buy $oop"]
[NSToolbar::GetTool $toolId $id] configure -hovermenu yes -onlymenu yes -menuhidecmd "NSStore2::PopupHide $oop"

	set id2 [NSToolbar::AddTool $toolId \
		-command "DoCommandIfAllowed s" -label [mc "Sell"] -showlabel yes \
		-showimage no -heightlabel 16 -hasmenu yes \
		-menucommand "NSStore2::Win98MenuCmd_Sell $oop"]
[NSToolbar::GetTool $toolId $id2] configure -hovermenu yes -onlymenu yes -menuhidecmd "NSStore2::PopupHide $oop"

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
		-font $font -text [mc Quantity:]
	entry $frame.quantity \
		-width 2 -state disabled -textvariable NSStore2($oop,quantity)
	label $frame.howMuch \
		-font $font -text [mc "Total Cost:"]
	label $frame.totalCost \
		-font $font -text "" -anchor w
	label $frame.purse \
		-font $font
	label $frame.playerGold \
		-font $font -text [mc "Gold Remaining:"]
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

	# Update the display when the character's gold changes
	qebind $frame.gold <Py-gold> {%W configure -text %c}
	qeconfigure $frame.gold <Py-gold> -active no

	label $frame.price_character \
		-font $font -text "" -anchor w
	label $frame.price_owner \
		-font $font -text "" -anchor w

	trace add variable ::NSStore2($oop,quantity) write \
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

	SetCanvas $oop
if 0 {
	if {[Platform unix]} {
		set font {Helvetica 12 bold}
	}
	if {[Platform windows]} {
		set font {Helvetica 10 bold}
	}
	set fontHeight [font metrics $font -linespace]
	set heightHeader [expr {1 + $fontHeight + 1}]

	set bw 3
	set heightBox [expr {[icon height] + $bw * 2}]
	set widthBox [expr {[icon width] + $bw * 2}]
	set heightBoxMax 40
	set widthBoxMax 40

	set heightImage [image height Image_Shelf]
	set heightRow [expr {6 + $heightBox + 2 + $heightImage}]
	set heightStore [expr {3 + ($heightHeader - 1) + 4 * $heightRow + 6 + 3}]

	set padRing 2
	set widthStore [expr {3 + 6 * (6 + $widthBoxMax) + 6 + 3 + $padRing}]

	# Background image
	if {0 && [Global config,prefix] eq "dg32"} {
		if {![ImageExists Image_StoreBackground]} {
			image create photo Image_StoreBackground -width $widthStore \
				-height $heightStore
			set image Image_StoreTemp
			image create photo $image -width [icon width] -height [icon height]
			icon photo $image -type town1 -index 0
			Image_StoreBackground copy $image -to 0 0 $widthStore $heightStore
			image delete $image
		}
		$canvas create image 0 0 -image Image_StoreBackground -anchor nw
	}

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
	MakeHeader $oop $canvas $x 0 $widthInven [mc Inventory] inventory,title

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

	Info $oop width,store $widthStore
}
	bind $canvas <ButtonPress-1> \
		"NSStore2::Button1 $oop %x %y"
	bind $canvas <Button1-Motion> \
		"NSStore2::Motion1 $oop %x %y"
	bind $canvas <ButtonRelease-1> \
		"NSStore2::Release1 $oop %x %y"

	#
	# Statusbar
	#
if 0 {
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
		$canvas bind $itemId <ButtonPress-1> {dbwin "[%W coords current]\n"}
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
	$canvas create text 0 0 -anchor n -state hidden \
		-font [Value font,store] -tags {balloon balloon,text}
}
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
		-variable NSStore2::Priv(radio) -identifier E_AUTO_HAGGLE]
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

	[Info $oop canvas] itemconfigure statusBar,status,left -text $desc
	if {($desc eq "") && ($menuId == [Info $oop mbarId])} {
		if {[[Info $oop canvas] itemcget statusBar,status -state] eq ""} {
			[Info $oop canvas] itemconfigure statusBar,status -state hidden
		} else {
			[Info $oop canvas] itemconfigure statusBar,status -state ""
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
			qeconfigure $win.info.gold <Py-gold> -active yes
			qeconfigure NSStore2 <Setting> -active yes

			SetList_Inventory $oop
			qeconfigure NSStore2 <Track-inventory> -active yes
			qeconfigure NSStore2 <Choose-item> -active yes

			qeconfigure NSStore2Synch <Term-inkey> -active yes
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

if {[Info $oop popup,posted] ne ""} { [Info $oop popup,posted] hidemenu }

			# Clear both lists

			qeconfigure $win.info.gold <Py-gold> -active no
			qeconfigure NSStore2 <Setting> -active no

			qeconfigure NSStore2 <Track-inventory> -active no
			qeconfigure NSStore2 <Choose-item> -active no

			qeconfigure NSStore2Synch <Term-inkey> -active no

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

	Info $oop setting,show_flavors [Setting show_flavors]
	$menu add checkbutton -label [mc "Show Flavors"] \
		-command {Setting show_flavors [Store2Obj Info setting,show_flavors]} \
		-variable NSStore2($oop,setting,show_flavors)
	lappend keywordList show_flavors
	lappend descList [SettingDesc show_flavors]

	if {[variant OANGBANDTK]} {
		Info $oop setting,use_metric [Setting use_metric]
		$menu add checkbutton -label [mc "Use Metric"] \
			-command {Setting use_metric [Store2Obj Info setting,use_metric]} \
			-variable NSStore2($oop,setting,use_metric)
		lappend keywordList use_metric
		lappend descList [SettingDesc use_metric]
	}

	$menu add separator
	lappend descList ""

	$menu add command -label [mc "List Mode"] -command {Store2Obj Swap}
	lappend descList "Use the list window"

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

	[Info $oop canvas] itemconfigure statusBar,status -state ""

	tk_popup $menu $x $y

	if {[Platform unix]} {
		tkwait variable ::tk::Priv(popup)
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

	[Info $oop canvas] itemconfigure statusBar,status -state ""

	tk_popup $menu $x $y

	if {[Platform unix]} {
		tkwait variable ::tk::Priv(popup)
		$menu configure -font $font
	}

	after idle "$button hidemenu ; [Info $oop canvas] itemconfigure statusBar,status -state hidden"

	return
}

proc NSStore2::Win98MenuCmd_Buy {oop button} {

	variable MenuString

	set canvas [$button info canvas]
	set x [winfo rootx $canvas]
	set y [expr {[winfo rooty $canvas] + [winfo height $canvas] - 1}]

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

	[Info $oop canvas] itemconfigure statusBar,status -state ""

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

	[Info $oop canvas] itemconfigure statusBar,status -state ""

	tk_popup $menu $x $y

	if {[Platform unix]} {
		tkwait variable ::tk::Priv(popup)
		$menu configure -font $font
	}

	after idle "$button hidemenu ; [Info $oop canvas] itemconfigure statusBar,status -state hidden"

	return
}

proc NSStore2::Win98MenuCmd_Sell {oop button} {

	variable MenuString

	set canvas [$button info canvas]
	set x [winfo rootx $canvas]
	set y [expr {[winfo rooty $canvas] + [winfo height $canvas] - 1}]

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

	[Info $oop canvas] itemconfigure statusBar,status -state ""

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

	pack forget {*}[winfo children $frame]

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
		wm title $win [mc home-title1]
		$canvas itemconfigure store,title -text [mc home-title2]

	# Not in the Home
	} else {

		# Set the window title
		wm title $win [format [mc store-title1] \
			$storename [angband store ownername]]
		$canvas itemconfigure store,title \
			-text [format [mc store-title2] $storename]
	}

	# Get the number of items
	set count [angband store count]

	# Clear the selection
	SelectionChanged $oop "" ""

	# Clear the list
	for {set i 0} {$i < 24} {incr i} {
		MoveBox $oop -20 -20 store $i

		# Un-highlight
		$canvas itemconfigure sel,store,$i \
			-outline [Info $oop color2,store,$i]
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
		set string [format [mc "%d item"] $count]
	} else {
		set string [format [mc "%d items"] $count]
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

		# Un-highlight
		$canvas itemconfigure sel,inventory,$i \
			-outline [Info $oop color2,inventory,$i]
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

		if {[lsearch -integer $buyList $item] != -1} {
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
		set string [format [mc "%d item"] $numItems]
	} else {
		set string [format [mc "%d items"] $numItems]
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

	# Get information about this item
	angband $where info $index attrib

	# Get the item char
	set itemKey $attrib(char)

	# Get the tval
	set itemTval $attrib(tval)

	# Get the amount
	set itemAmount $attrib(number)

	# An inventory item is hit
	if {$where ne "store"} {

		# We are waiting for an inventory item
		if {[angband inkey_flags] eq "INKEY_ITEM"} {

			# Append a command to select the item
			set command "angband keypress $itemKey"

			$menu add command -label [mc "Select This Item"] \
				-command $command -font [BoldFont $font]
			$menu add separator
			$menu add command -label [mc Cancel] \
				-command "angband keypress \033"

			# Pop up the menu
			tk_popup $menu $x $y

			# Done
			return
		}

		# We are not waiting for a command
		if {[angband inkey_flags] ne "INKEY_CMD"} {
			return
		}

		# See if the store will accept this item
		set match [angband inventory find -store_will_buy yes]
		if {[lsearch -integer $match $index] != -1} {
			if {$itemAmount > 1} {
				$menu add command -label [mc "$stringSell One"] \
					-command "angband keypress 01$charSell$itemKey" \
					-font [BoldFont $font]
				if {$itemAmount > 5} {
					$menu add command -label [mc "$stringSell Five"] \
						-command "angband keypress 05$charSell$itemKey"
				}
				if {$itemAmount > 1} {
					$menu add command -label [mc "$stringSell Some"] \
						-command "angband keypress $charSell$itemKey"
				}
				$menu add command -label [mc "$stringSell All"] \
					-command "angband keypress 0$itemAmount$charSell$itemKey"
			} else {
				$menu add command -label [mc "$stringSell This Item"] \
					-command "angband keypress $charSell$itemKey" \
					-font [BoldFont $font]
			}
			$menu add separator
			$menu add command -label [mc "$stringBuy An Item"] \
				-command "angband keypress $charBuy"
		} else {
			$menu add command -label [mc "$stringBuy An Item"] \
				-command "angband keypress $charBuy"
			$menu add command -label [mc "$stringSell An Item"] \
				-command "angband keypress $charSell"
		}
		$menu add separator
		$menu add command -label [mc Leave] \
			-command "angband keypress \033"
		$menu add separator
		$menu add command -label [mc Cancel]

		# Pop up the menu
		tk_popup $menu $x $y

		# Done
		return
	}

	# We are waiting for a store item
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
			-command "angband keypress $charBuy$itemKey" \
			-font [BoldFont $font]
	} else {
		$menu add command -label [mc "$stringBuy One"] \
			-command "angband keypress 01$charBuy$itemKey" \
			-font [BoldFont $font]
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
				|| ![angband store worthless $index])} {
				append command y
			}
		}
		if {[variant ZANGBANDTK]} {
			if {!([Setting auto_destroy] && [angband store worthless $index])} {
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

	if {[angband inkey_flags] eq "INKEY_CMD"} {

		# If this item can be sold, set the default action to "Sell"
		set match [angband inventory find -store_will_buy yes]
		if {[lsearch -integer $match $index] != -1} {
			set cmdChar s
			angband keypress $cmdChar$char
		}
	}

	if {[angband inkey_flags] eq "INKEY_ITEM"} {
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

	if {($where eq [Info $oop select,where]) &&
		($index eq [Info $oop select,index])} {
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

	if {[Info $oop select,where] ne ""} {
		Highlight $oop {off selected} [Info $oop select,where] [Info $oop select,index]
	}

	# Nothing was selected
	if {$index eq ""} {
		Info $oop select,where ""
		Info $oop select,index ""
		return
	}

	# Remember the selected item
	Info $oop select,where $where
	Info $oop select,index $index

	Highlight $oop {on mouse selected} $where $index
#	BalloonHide $oop

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

	if {$where eq "" || $where ne "store" || $quantity eq ""} {
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

	Info $oop press,x $x
	Info $oop press,y $y

	if {[angband inkey_flags] eq "INKEY_MORE"} {
		angband keypress " "
		return
	}

	set canvas [Info $oop canvas]

	focus $canvas

	set itemId [$canvas find withtag current]
	if {![llength $itemId] || [$canvas type current] eq "image"} {
		if {[Info $oop select,index] ne ""} {
			SelectionChanged $oop "" ""
		}
		return
	}

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

	# In case we clicked the mouse while -more- was showing, just skip
	# past them while dragging.
	if {[angband inkey_flags] eq "INKEY_MORE"} {
		angband keypress " "
		return
	}

	if {[angband inkey_flags] ne "INKEY_CMD"} {
		return
	}

	set canvas [Info $oop canvas]
	set dragging [Info $oop dragging]
	set where [Info $oop select,where]
	set index [Info $oop select,index]

	# No item is selected
	if {$index eq ""} return

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
	if {$where eq $dest} return
	switch -- $where {
		inventory {
			Invoke_Inventory $oop $where $index
#			DoUnderlyingCommand s$charItem
			SkipMoreMessages $oop 1
		}
		store {
			Invoke_Store $oop $where $index
#			angband keypress "0[Info $oop quantity]p$charItem"
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

set isSelected [expr {$where eq [Info $oop select,where] &&
	$index eq [Info $oop select,index]}]

	if {"on" in $state} {

		# Highlight
		$canvas itemconfigure sel,$where,$index \
			-outline [Info $oop color,$where,$index]

		angband $where info $index attrib

if {"mouse" in $state} {
		# Balloon
		scan [$canvas bbox sel,$where,$index] "%s %s %s %s" left top right bottom
		set x [expr {[winfo rootx $canvas] + $left + 3 + [icon width] / 2}]
		set y [expr {[winfo rooty $canvas] + ($bottom + 1) + 2}]
		set char [Info $oop char,$where,$index]
		set text "$char\) $attrib(name)"
		BalloonShow $oop $text $x $y
}
		# This must come before QuantityChanged()
		Info $oop highlight,where $where
		Info $oop highlight,index $index

		if {$where eq "store"} {
			$entry configure -state normal
			Info $oop quantity "1"
		}

		if {$where eq "store"} {
			set weight [fmt_wgt $attrib(weight) 1]
			$canvas itemconfigure $where,status,right -text $weight
		} else {
			set weight [expr {$attrib(weight) * $attrib(number)}]
			set weight [fmt_wgt $weight]
			set total [fmt_wgt [Info $oop $where,weight]]
			$canvas itemconfigure $where,status,right \
				-text [format [mc weight2_[lb_or_kg]] $weight $total]
		}

		NSRecall::RecallObject $where $index

	} else {

if {"selected" in $state || !$isSelected} {
		# Un-highlight
		$canvas itemconfigure sel,$where,$index \
			-outline [Info $oop color2,$where,$index]
}
		BalloonHide $oop

		# This must come before QuantityChanged()
		Info $oop highlight,where ""
		Info $oop highlight,index ""

		if {$where eq "store"} {
			Info $oop quantity ""
			$entry configure -state disabled
		}

		if {$where eq "store"} {
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

#	if {[string length [Info $oop select,where]]} return

	Highlight $oop {on mouse} $where $index

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

	if {1 || ![string length [Info $oop select,index]]} {
		Highlight $oop {off} $where $index
	}

	# Re-enter the selected item, if any
	set where [Info $oop select,where]
	set index [Info $oop select,index]
	if {$where ne ""} {
		Highlight $oop {on} $where $index
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
	if {[Info $oop request,id] eq ""} {
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

	if {[Info $oop highlight,where] ne ""} {
		Leave $oop [Info $oop highlight,where] [Info $oop highlight,index]
	}

	if {$type eq "enter"} {
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

	set font [Value font,store]
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

dbwin "NSStore2::TrackInventory\n"
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

dbwin "NSStore2::ChooseItem $show $other\n"

	if {$other ne "inventory"} return

	Info $oop choose,show $show
	Info $oop choose,other $other
	if {$show} {
		Info $oop didChoose 0
	}

	qeconfigure NSStore2 <Term-inkey> -active yes

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

	qeconfigure NSStore2 <Term-inkey> -active no

	if {[Info $oop choose,show]} {
dbwin "NSStore2::TermInkey\n"

		# Show the choices
		SetList_Inventory $oop

		Info $oop didChoose 1
	} elseif {![Info $oop didChoose]} {
dbwin "NSStore2::TermInkey SKIPPED\n"
		# Nothing
	} else {

		# Show all items
		SetList_Inventory $oop
	}

	Synch $oop

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

proc NSStore2::SetCanvas {oop} {

	set canvas [Info $oop canvas]

	$canvas delete all

	if {[Platform unix]} {
		set font {Helvetica 12 bold}
	}
	if {[Platform windows]} {
		set font {Helvetica 10 bold}
	}
	set fontHeight [font metrics $font -linespace]
	set heightHeader [expr {1 + $fontHeight + 1}]

	set bw 3
	set heightBox [expr {[icon height] + $bw * 2}]
	set widthBox [expr {[icon width] + $bw * 2}]
	set heightBoxMax 40
	set widthBoxMax 40

	set heightImage [image height Image_Shelf]
	set heightRow [expr {6 + $heightBox + 2 + $heightImage}]
	set heightStore [expr {3 + ($heightHeader - 1) + 4 * $heightRow + 6 + 3}]

	set padRing 2
	set widthStore [expr {3 + 6 * (6 + $widthBoxMax) + 6 + 3 + $padRing}]

	# Background image
	if {0 && [Global config,prefix] eq "dg32"} {
		if {![ImageExists Image_StoreBackground]} {
			image create photo Image_StoreBackground -width $widthStore \
				-height $heightStore
			set image Image_StoreTemp
			image create photo $image -width [icon width] -height [icon height]
			icon photo $image -type town1 -index 0
			Image_StoreBackground copy $image -to 0 0 $widthStore $heightStore
			image delete $image
		}
		$canvas create image 0 0 -image Image_StoreBackground -anchor nw
	}

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
			set y [expr {3 + ($heightHeader - 1) + 6 + $row * $heightRow + $heightBox + 2}]
			$canvas create image $x $y -image Image_Shelf -anchor nw

			if {[incr index] == 24} break
		}

		if {$index == 24} break
	}

	# Inventory
	set widthInven [expr {$padRing + 3 + 5 * ($widthBoxMax + 6) + 6 + 3}]

	set x [expr {$widthStore + 1}]
	MakeBorder $oop $canvas $x 0 $widthInven $heightStore
	MakeHeader $oop $canvas $x 0 $widthInven [mc Inventory] inventory,title

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

	Info $oop width,store $widthStore

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
		$canvas bind $itemId <ButtonPress-1> {dbwin "[%W coords current]\n"}
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
	$canvas create text 0 0 -anchor n -state hidden \
		-font [Value font,store] -tags {balloon balloon,text}

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

	if {$message eq "leave"} {
		NSMainWindow::StatusText [Global main,oop] ""
		return
	}

	set string ""

	switch -- $where {
		inventory {
			switch -- $side {
				left {
					set string [mc status-number-inven]
				}
				right {
					set string [mc status-weight-inven]
				}
			}
		}
		store {
			switch -- $side {
				left {
					set string [mc status-number-store]
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

	set bw 3
	set width [expr {[icon width] + $bw * 2}]
	set height [expr {[icon height] + $bw * 2}]

	if {![string length $rgb1]} {
		set rgb1 [format #%02x%02x%02x 102 51 0]
		set rgb2 [format #%02x%02x%02x 255 153 0]
	}

	# 3-pixel border
	set outline $rgb1
	set fill [format #%02x%02x%02x 68 68 68]
	$canvas create rectangle [expr {$bw / 2}] [expr {$bw / 2}] \
		[expr {$width - $bw / 2 - 1}] [expr {$height - $bw / 2 - 1}] \
		-fill $fill -outline $outline -width 3.0 \
		-tags "$where,$index border,$where,$index"

	# 1-pixel border
	$canvas create rectangle [expr {$bw / 2}] [expr {$bw / 2}] \
		[expr {$width - $bw / 2 - 1}] [expr {$height - $bw / 2 - 1}] \
		-fill {} -outline {} -tags "$where,$index sel,$where,$index"

	Info $oop color,$where,$index $rgb2
	Info $oop color2,$where,$index ""

	# Widget
	$canvas create widget [expr {$width / 2}] \
		[expr {$height / 2}] -assign {icon none 0} \
		-tags "icon $where,$index icon,$where,$index" -anchor center

	# No bindings on the drag box
	if {$where eq "drag"} return

	$canvas bind icon,$where,$index <Enter> \
		"NSStore2::Request $oop enter $where $index"
	$canvas bind icon,$where,$index <Leave> \
		"NSStore2::Request $oop leave $where $index"
	$canvas bind icon,$where,$index <ButtonPress-1> \
		"NSStore2::Select $oop $where $index"
	$canvas bind icon,$where,$index <Double-ButtonPress-1> \
		"NSStore2::Invoke $oop $where $index"

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

# NSStore2::ValueChanged_font_store --
#
#	Called when the font,store value changes.
#	Updates the Store Window balloon.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::ValueChanged_font_store {oop} {

	set canvas [Info $oop canvas]

	$canvas itemconfigure balloon,text -font [Value font,store]

	return
}

# NSStore2::ValueChanged_font_statusBar --
#
#	Called when the font,statusBar value changes.
#	Updates the Store Window statusbar.
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

	if {$skip} {
		qebind NSStore2 <Inkey-more> {angband keypress \033}
		qebind NSStore2 <Inkey-cmd> {Store2Obj SkipMoreMessages 0}
	} else {
		qeunbind NSStore2 <Inkey-more>
		qeunbind NSStore2 <Inkey-cmd>
	}

	return
}

# NSStore2::IconCfg --
#
#	Called when the game's icon configuration changes
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::IconCfg {oop} {

	set canvas [Info $oop canvas]

	SetCanvas $oop
	wm geometry [Info $oop win] ""

	if {[winfo ismapped [Info $oop win]]} {
		SetList $oop
		SetList_Inventory $oop
	}

	return
}

# NSStore2::PopupInit --
#
#	Init the treectrl-based menu.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::PopupInit {oop} {

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

	$tree notify bind $tree <Selection> "NSStore2::PopupSelectionChanged $oop"

	bind $tree <Motion> "NSStore2::PopupMotion $oop %x %y"
	bind $win <Leave> "NSStore2::PopupLeave $oop"

	bind $tree <Control-ButtonPress-1> { ; }
	bind $tree <Shift-ButtonPress-1> { ; }
#	bind $tree <ButtonPress-1> "NSStore2::PopupButton1 $oop %x %y"
	bind $tree <ButtonRelease-1> "NSStore2::PopupButton1 $oop %x %y"

	bind $win <KeyPress-Escape> "\[NSStore2::Info $oop popup,posted] hidemenu"

	pack $tree -expand yes -fill both -padx 2 -pady 2
	pack $frame -expand yes -fill both

	Info $oop popup,win $win
	Info $oop popup,tree $tree
	Info $oop popup,posted ""
	Info $oop popup,afterId ""

	return
}

# NSStore2::PopupSelectionChanged --
#
#	Handle <Selection>.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::PopupSelectionChanged {oop} {

	set tree [Info $oop popup,tree]

	if {[$tree selection count] == 1} {
		set item [$tree selection get]
		if {[$tree item tag expr $item hook]} {
			PopupCallHook $oop select {*}[Info $oop popup,match,$item]
		}
	}

	return
}

# NSStore2::PopupMotion --
#
#	Handle <Motion>.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::PopupMotion {oop x y} {

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

# NSStore2::PopupLeave --
#
#	Handle <Leave>.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::PopupLeave {oop} {

	if {[Info $oop popup,afterId] ne ""} {
		after cancel [Info $oop popup,afterId]
	}
	Info $oop popup,afterId [after 50 "NSStore2::CheckWhoHasCursor $oop"]

	return
}

proc NSStore2::CheckWhoHasCursor {oop} {

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

# NSStore2::PopupButton1 --
#
#	Handle <ButtonPress-1>.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::PopupButton1 {oop x y} {

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

# NSStore2::PopupCallHook --
#
#	Call the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::PopupCallHook {oop message args} {

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

# NSStore2::PopupNewItem --
#
#	Create a new item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::PopupNewItem {oop char string color args} {

	set tree [Info $oop popup,tree]
	set item [$tree item create -parent root -tags hook]
	$tree item element configure $item COL eChar -text "$char) "
	$tree item element configure $item COL eTxt -text $string -fill $color

	Info $oop popup,match,$item $args

	return $item
}

# NSStore2::PopupNewSeparator --
#
#	Create a new separator item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::PopupNewSeparator {oop} {

	set tree [Info $oop popup,tree]
	set item [$tree item create -parent root -height 6]
	$tree item enabled $item no
	$tree item style set $item COL sSeparator

	return $item
}

# NSStore2::PopupHide --
#
#	Hide the popup menu. Called by win98button.menuhidecmd.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSStore2::PopupHide {oop} {

	if {[Info $oop popup,afterId] ne ""} {
		after cancel [Info $oop popup,afterId]
		Info $oop popup,afterId ""
	}
	wm withdraw [Info $oop popup,win]
	Info $oop popup,posted ""
	NSUtils::GrabRelease [Info $oop win]
	[Info $oop canvas] itemconfigure statusBar,status -state hidden
#	set ::NSStore2($oop,result) 1

	return
}

proc Store2Obj {command args} {
	return [NSStore2::$command [Global store2,oop] {*}$args]
}
