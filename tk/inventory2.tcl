# File: inventory2.tcl

# Purpose: the (new) Inventory Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSInventory2 {

# namespace eval NSInventory2
}

# NSInventory2::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::InitModule {} {

	MsgCatInit inven

	InitImageIfNeeded Image_ButtonOptions button-options.gif
	InitImageIfNeeded Image_ButtonHelp button-help.gif
	InitImageIfNeeded Image_Binding dg_binding.gif
	if {[InitImageIfNeeded Image_Equip dg_equip.gif]} {
		photomask Image_Equip
	}

#	NSModule::LoadIfNeeded NSBalloon
	NSModule::LoadIfNeeded NSStatusBar
	NSModule::LoadIfNeeded NSToolbar

	# Create the Inventory Window
	NSObject::New NSInventory2

	return
}

# NSInventory2::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::CloseModule {} {
	
	catch {
		destroy [Window inventory2]
	}

	return
}

# NSInventory2::NSInventory2 --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::NSInventory2 {oop} {

	Info $oop alwaysOnTop [Value inventory,alwaysOnTop]
	Info $oop browsing 0
	Info $oop busy 0
	Info $oop choose,show 0
	Info $oop didChoose 0
	Info $oop dragging 0
	Info $oop highlight,where ""
	Info $oop highlight,index ""
	Info $oop select,where ""
	Info $oop select,index ""
	Info $oop toolbar,match {}
	Info $oop request {}
	Info $oop request,id ""
	Info $oop oldFocus ""
	Info $oop skipHide 0

	Info $oop equipment,weight 0

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow inventory2 $win \
		"GetDefaultGeometry $win reqwidth reqheight" "" \
		"NSInventory2::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSInventory2 $oop $win

	# Update the display when some settings change
	qebind NSInventory2 <Setting> {
		Inventory2Obj SettingChanged %d %c
	}
	qeconfigure NSInventory2 <Setting> -active no

	qebind NSInventory2 <Track> {
		Inventory2Obj Track %d
	}
	qeconfigure NSInventory2 <Track> -active no

	qebind NSInventory2 <Term-inkey> {
		Inventory2Obj TermInkey
	}
	qeconfigure NSInventory2 <Term-inkey> -active no

	qebind NSInventory2 <Choose-item> {
		Inventory2Obj ChooseItem %s %o
	}
	qeconfigure NSInventory2 <Choose-item> -active no

	qebind NSInventory2 <IconCfg> \
		"NSInventory2::IconCfg $oop"

	bind $win <KeyPress-Escape> {
		Inventory2Obj Close
		break
	}

	# Update ourself when the font,inventory value changes
	qebind NSInventory2 <Value-font,inventory> \
		"NSInventory2::ValueChanged_font_inventory $oop"

	# Update ourself when the font,statusBar value changes
	qebind NSInventory2 <Value-font,statusBar> \
		"NSInventory2::ValueChanged_font_statusBar $oop"

	#
	# Global list of application windows
	#

	Global inventory2,oop $oop
	Window inventory2 $win

	return
}

# NSInventory2::~NSInventory2 --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::~NSInventory2 {oop} {

	return
}

# NSInventory2::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::Info {oop info args} {

	global NSInventory2

	# Verify the object
	NSObject::CheckObject NSInventory2 $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSInventory2($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSInventory2($oop,$info)
			}
		}
	}

	return
}

# NSInventory2::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::InitWindow {oop} {

	set win .inventory2_$oop
	toplevel $win
	wm title $win Inventory

	wm resizable $win no no

	# Start out withdrawn (hidden)
	wm withdraw $win

	if {[Info $oop alwaysOnTop]} {
		# Do this *after* [wm withdraw] or it pops onscreen
		NSMainWindow::TransientToMain $win
	}

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSInventory2::Close $oop"

	Info $oop win $win

	#
	# Toolbar
	#

	set toolId [NSObject::New NSToolbar 20 $win]
	NSToolbar::AddTool $toolId -image Image_ButtonOptions \
		-showlabel no -command "DoCommandIfAllowed =" -hasmenu yes \
		-menucommand "NSInventory2::Win98MenuCmd_Options $oop"
	NSToolbar::AddTool $toolId -image Image_ButtonHelp \
		-showlabel no -command "DoCommandIfAllowed ?"

	set menuId [NSObject::New NSMenu *$win \
		-tearoff 0 -identifier MENU_TOOLBAR]
	NSMenu::Info $menuId menuSelectCmd "NSInventory2::MenuSelect $oop"

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
		"NSInventory2::CommitInscription $oop"

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
	# Canvas
	#

	set canvas $win.canvas
	canvas $canvas \
		-borderwidth 0 -highlightthickness 0 \
		-background [format #%02x%02x%02x 0 0 153]
if 0 {
	$canvas create image 109 164 -image Image_Equip -anchor center -tags image
	if {[info tclversion] >= 8.3} {
		$canvas itemconfigure image -state disabled
	}
}
	bind $canvas <ButtonPress-1> \
		"NSInventory2::Button1 $oop %x %y"
	bind $canvas <Button1-Motion> \
		"NSInventory2::Motion1 $oop %x %y"
	bind $canvas <ButtonRelease-1> \
		"NSInventory2::Release1 $oop %x %y"

	Info $oop canvas $canvas
if 1 {
	SetCanvas $oop
} else {
	#
	# Equipment
	#

	if {[Platform unix]} {
		set font {Helvetica 12 bold}
	}
	if {[Platform windows]} {
		set font {Helvetica 10 bold}
	}
	set fontHeight [font metrics $font -linespace]
	set heightHeader [expr {1 + $fontHeight + 1}]

	set heightBox [expr {[icon height] + 8}]
	set widthBox [expr {[icon width] + 8}]
	set widthBoxMax 40

	set widthEquip 224
	set heightEquip 297

	set padRing 2

	MakeBorder $oop $canvas 0 0 $widthEquip $heightEquip
	MakeHeader $oop $canvas 0 0 $widthEquip [mc Equipment] equipment,title

	set slots [list \
		INVEN_WIELD 111 158 {119 0 0} {204 0 0} \
		INVEN_BOW 72 23 {119 0 0} {204 0 0} \
		INVEN_LEFT 160 171 {51 102 102} {102 204 204} \
		INVEN_RIGHT 164 7 {51 102 102} {102 204 204} \
		INVEN_NECK 51 148 {51 102 102} {102 204 204} \
		INVEN_LITE 26 31 {102 51 0} {255 153 0} \
		INVEN_BODY 95 89 {0 102 0} {51 204 0} \
		INVEN_OUTER 208 138 {0 102 0} {51 204 0} \
		INVEN_ARM 118 15 {0 102 0} {51 204 0} \
		INVEN_HEAD 39 89 {0 102 0} {51 204 0} \
		INVEN_HANDS 208 40 {0 102 0} {51 204 0} \
		INVEN_FEET 248 89 {0 102 0} {51 204 0} \
	]

	set lines [list \
		INVEN_WIELD {{147 167}} \
		INVEN_BOW {{82 90}} \
		INVEN_LEFT {{150 182}} \
		INVEN_RIGHT {{67 182}} \
		INVEN_NECK {{108 90}} \
		INVEN_ARM {{73 147}} \
		INVEN_HANDS {{70 175} {147 175}} \
	]
	array set data $lines

	foreach {slot y x rgb1 rgb2} $slots {
		set cy [expr {$y + 20}]
		set cx [expr {$x + 20}]

		if {[info exists data($slot)]} {
			set fill [format "#%02x%02x%02x" 153 153 153]
			foreach loc $data($slot) {
				scan $loc "%d %d" x2 y2
				$canvas create line $cx $cy $x2 $y2 -fill $fill \
					-tags "line line,$slot"
			}
		}

		set rgb1 [eval format "#%02x%02x%02x" $rgb1]
		set rgb2 [eval format "#%02x%02x%02x" $rgb2]
		MakeBox $oop equipment $slot $rgb1 $rgb2

		Info $oop equipment,y,$slot $cy
		Info $oop equipment,x,$slot $cx
		MoveBox $oop $cx $cy equipment $slot

		Info $oop visible,$slot 1
		Info $oop assign,$slot "icon none 0"

		lappend slot_names $slot
	}

	Info $oop slot_names $slot_names
	Info $oop width,equip $widthEquip

	#
	# Inventory
	#

	set widthInven [expr {$padRing + 3 + ($widthBoxMax + 6) * 5 + 6 + 3}]
	set heightInven $heightEquip

	set x [expr {$widthEquip + 1}]
	MakeBorder $oop $canvas $x 0 $widthInven $heightInven
	MakeHeader $oop $canvas $x 0 $widthInven [mc Inventory] inventory,title

	set left [expr {$x + $padRing + 3 + 6}]
	set top [expr {3 + ($heightHeader - 1) + 6}]
	set columns [expr {($widthInven - $padRing - 3 - 6 - 3) / ($widthBox + 6)}]
	set diff [expr {($widthInven - $padRing - 3 - 6 - 3) - $columns * ($widthBox + 6)}]

	# 23 items
	set col 0
	set row 0
	for {set i 0} {$i <= 23} {incr i} {
		MakeBox $oop inventory $i

		set x [expr {$diff / 2 + $left + $col * ($widthBox + 6) + $widthBox / 2}]
		set y [expr {$top + $row * ($heightBox + 6) + $heightBox / 2}]
		Info $oop inventory,x,$i $x
		Info $oop inventory,y,$i $y
		if {[incr col] == $columns} {
			set col 0
			incr row
		}

		if {[info tclversion] >= 8.3} {
			MoveBox $oop $x $y inventory $i
		}
	}

	#
	# Statusbar
	#

	set y $heightEquip
	if {[variant OANGBANDTK]} {
		incr y [expr {$heightBox + 8}]
	}
	set topStatus $y
	Info $oop top,status $y

	# Equipment statusbar
	MakeStatus $oop $canvas 0 $topStatus $widthEquip equipment

	# Inventory statusbar
	set x [expr {$widthEquip + 1}]
	MakeStatus $oop $canvas $x $topStatus $widthInven inventory

	set font [Value font,statusBar]
	set fontHeight [font metrics $font -linespace]
	set heightStatus [expr {3 + $fontHeight + 3}]

	set widthTotal [expr {$widthEquip + 1 + $widthInven}]
	set heightTotal [expr {$heightEquip + $heightStatus}]
	$canvas configure -width $widthTotal -height $heightTotal

	# Divider filler
	set x $widthEquip
	$canvas create line $x 0 $x $heightEquip -fill #282828

	# Hack -- ring bindings down the middle
	set y 34
	for {set i 0} {$i < 8} {incr i} {
		$canvas create image $x $y -image Image_Binding
		incr y 34
	}

	if {[variant OANGBANDTK]} {

		#
		# Quiver
		#

		set y $heightEquip
		MakeBorder $oop $canvas 0 $y $widthTotal [expr {$heightBox + 8}]
		$canvas configure -height [expr {$heightTotal + $heightBox + 8}]
		Info $oop top,quiver $y

		set x [expr {$widthTotal / 2}]
		set y [expr {2 + $heightEquip + $heightBox / 2}]
		$canvas create text $x $y -fill [format #%02x%02x%02x 0 0 200] \
			-text "Quiver" -font "Times -24 bold"

		set diff [expr {($widthTotal - ($widthBox * 10 + 6 * 9)) / 2}]
		for {set i 0} {$i < 10} {incr i} {

			set slot INVEN_Q$i

			MakeBox $oop equipment $slot
			set x [expr {$diff + $widthBox / 2 + $i * ($widthBox + 6)}]
			set y [expr {4 + $heightEquip + $heightBox / 2}]
			MoveBox $oop $x $y equipment $slot

			Info $oop assign,$slot "icon none 0"
			Info $oop visible,$slot 1
		}
	}

	# This box is used for drag & drop
	MakeBox $oop drag 0
	HideBox $oop drag 0

	# Help-text statusbar (normally hidden)
	MakeStatus $oop $canvas 0 $topStatus $widthTotal statusBar
	$canvas itemconfigure statusBar,status -state hidden

	# Alternate balloon impl
	$canvas create rectangle 0 0 10 10 -fill White -state hidden \
		-tags {balloon balloon,rect}
	$canvas create text 0 0 -anchor n -state hidden \
		-font [Value font,inventory] -tags {balloon balloon,text}
}
	#
	# Geometry
	#

	grid rowconfig $win 0 -weight 0
	grid rowconfig $win 1 -weight 1
	grid columnconfig $win 0 -weight 1
 
	pack forget [NSToolbar::Info $toolId frame]
	grid [NSToolbar::Info $toolId frame] -in $win \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.canvas -in $win \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news

	#
	# Context Menu
	#
	
	set m $win.context
	menu $m -tearoff 0
	bind $canvas <ButtonPress-3> \
		"NSInventory2::ContextMenu $oop $m %X %Y"

	#
	# Feed Term when keys pressed
	#

	Term_KeyPress_Bind $win
	Term_KeyPress_Bind $canvas

	return
}

# NSInventory2::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::MenuSelect {oop menuId index ident} {

	variable MenuString

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

	return
}

# NSInventory2::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::DisplayCmd {oop message first args} {

	switch -- $message {
		preDisplay {
			qeconfigure NSInventory2 <Setting> -active yes
			SetList $oop {*}$args
			if {([llength $args] == 3) && [lindex $args 2]} {
				Info $oop browsing 1
			}
			qeconfigure NSInventory2 <Track> -active yes
			qeconfigure NSInventory2 <Choose-item> -active yes
		}
		postDisplay {
			if {0 && ![Value warning,inventory,window]} {
				tk_messageBox -parent [Info $oop win] -title "Game Change" \
					-message "This is the new graphical Inventory Window.\
					If you want the list instead, choose \"List Mode\" from\
					the options button in the toolbar."
				Value warning,inventory,window 1
			}
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
			} else {
				SetList $oop {*}$args
			}
			if {![Info $oop alwaysOnTop]} {
				WindowBringToFront [Info $oop win]
			}
		}
		postWithdraw {

			# Clear the list
			set canvas [Info $oop canvas]
			$canvas itemconfigure icon -assign {icon none 0}

			foreach slot [Info $oop slot_names] {
				Info $oop assign,$slot "icon none 0"
			}

			if {[variant OANGBANDTK]} {
				for {set i 0} {$i < 10} {incr i} {
					Info $oop assign,INVEN_Q$i "icon none 0"
				}
			}

			qeconfigure NSInventory2 <Setting> -active no

			Info $oop browsing 0
			qeconfigure NSInventory2 <Track> -active no
			qeconfigure NSInventory2 <Choose-item> -active no

			# In case of errors, prevent paralysis
			Info $oop busy 0
		}
	}

	return
}

# NSInventory2::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::Close {oop} {

	if {[angband inkey_flags] eq "INKEY_ITEM"} {
dbwin "NSInventory2::Close -> Escape\n"
		angband keypress \033
	} elseif {[Info $oop skipHide]} {
dbwin "NSInventory2::Close -> Skip\n"
		set oldFocus [Info $oop oldFocus]
		if {[string length $oldFocus]} {
			catch {focus $oldFocus}
		}
		Info $oop skipHide 0
	} elseif {[Info $oop browsing]} {
dbwin "NSInventory2::Close -> Undisplay\n"
		NSWindowManager::Undisplay inventory2
	}

	return
}

# NSInventory2::Win98MenuCmd_Options --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::Win98MenuCmd_Options {oop button} {

	set canvas [$button info canvas]
	set x [winfo rootx $canvas]
	set y [expr {[winfo rooty $canvas] + [winfo height $canvas]}]

	set menu [Info $oop toolbar,menu]
	$menu delete 0 end

	set keywordList {}
	set descList {}

	Info $oop setting,show_flavors [Setting show_flavors]
	$menu add checkbutton -label [mc "Show Flavors"] \
		-command {Setting show_flavors [Inventory2Obj Info setting,show_flavors]} \
		-variable NSInventory2($oop,setting,show_flavors)
	lappend keywordList show_flavors
	lappend descList [SettingDesc show_flavors]

	if {[variant OANGBANDTK]} {
		Info $oop setting,use_metric [Setting use_metric]
		$menu add checkbutton -label [mc "Use Metric"] \
		-command {Setting use_metric [Inventory2Obj Info setting,use_metric]} \
			-variable NSInventory2($oop,setting,use_metric)
		lappend keywordList use_metric
		lappend descList [SettingDesc use_metric]
	}

	$menu add separator
	lappend descList ""

	$menu add checkbutton -label [mc "Always On Top"] \
		-command {Inventory2Obj AlwaysOnTop} \
		-variable NSInventory2($oop,alwaysOnTop)
	lappend descList "Keep the window on top of the Main Window"

	$menu add command -label [mc "List Mode"] -command {Inventory2Obj Swap}
	lappend descList "Use the list window"

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

	[Info $oop canvas] itemconfigure statusBar,status -state ""

	tk_popup $menu $x $y

	if {[Platform unix]} {
		tkwait variable ::tk::Priv(popup)
	}

	after idle "$button hidemenu ; [Info $oop canvas] itemconfigure statusBar,status -state hidden"

	return
}

# NSInventory2::SettingChanged --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::SettingChanged {oop keyword value} {

	# Ignore settings which don't affect the display
	if {[lsearch -exact [Info $oop toolbar,match] $keyword] == -1} return

	# Update the button
	update idletasks

#	SetList $oop {} {} [Info $oop both]
	SetList_Equipment $oop
	SetList_Inventory $oop

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

# NSInventory2::GetItemCommand --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::GetItemCommand {oop where index} {

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

# NSInventory2::Invoke --
#
#	Called when an item is double-clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::Invoke {oop where index} {

	set char [Info $oop char,$where,$index]
	set whereItem [Info $oop where,$where,$index]
	set item [Info $oop item,$where,$index]

	angband $whereItem info $item attrib

	# Ignore non-objects in equipment
	if {$attrib(tval) eq "TV_NONE"} return

	if {[angband inkey_flags] eq "INKEY_CMD"} {
		set list [GetItemCommand $oop $where $item]
		set command [lindex $list 1]
if 0 {
		# If this item can be sold, set the default action to "Sell"
		if {[angband store shopping] && $where eq "inventory"} {
			set match [angband inventory find -store_will_buy yes]
			if {[lsearch -integer $match $index] != -1} {
				set cmdChar s
			}
		}
}
		if {[string length $command]} {
			eval $command
		}
	}

	if {[angband inkey_flags] eq "INKEY_ITEM"} {
		angband keypress $char
	}

	return
}

# NSInventory2::Select --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::Select {oop where index} {

	if {$where eq [Info $oop select,where] &&
		$index eq [Info $oop select,index]} {
#		SelectionChanged $oop "" ""
		return
	}
	SelectionChanged $oop $where $index

	return
}

# NSInventory2::SelectionChanged --
#
#	Called when the list selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::SelectionChanged {oop where index} {

	if {[Info $oop select,where] ne ""} {
		Highlight $oop {off selected} [Info $oop select,where] [Info $oop select,index]
	}

	# Nothing was selected
	if {$where eq ""} {
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

# NSInventory2::Highlight --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::Highlight {oop state where index} {

	if {[Info $oop busy]} return
	Info $oop busy 1

	set canvas [Info $oop canvas]
	set entry [Info $oop inscription,entry]

set isSelected [expr {$where eq [Info $oop select,where] &&
	$index eq [Info $oop select,index]}]

	if {"on" in $state} {

		# Delayed action: box is hidden
		if {[$canvas itemcget $where,$index -state] eq "hidden"} {
			Info $oop busy 0
			return
		}
		 
		# Remember the highlighted box
		Info $oop highlight,where $where
		Info $oop highlight,index $index

		$canvas itemconfigure sel,$where,$index \
			-outline [Info $oop color,$where,$index]

		set whereItem [Info $oop where,$where,$index]
		set item [Info $oop item,$where,$index]

		# Get info about the item
		angband $whereItem info $item attrib

		# Ignore non-objects in equipment
		if {$attrib(tval) ne "TV_NONE"} {

			# Display the inscription for possible editing
			$entry configure -state normal
			$entry delete 0 end
			$entry insert end [angband $whereItem inscription $item]

			NSRecall::RecallObject $whereItem $item

			set weight [expr {$attrib(weight) * $attrib(number)}]
			set weight [fmt_wgt $weight]
			set total [fmt_wgt [Info $oop $where,weight]]
			set units [lb_or_kg]
			$canvas itemconfigure $where,status,right \
				-text [format [mc weight2_$units] $weight $total]

if {"mouse" in $state} {
			scan [$canvas bbox sel,$where,$index] "%s %s %s %s" left top right bottom
			set x [expr {[winfo rootx $canvas] + $left + 3 + [icon width] / 2}]
			set y [expr {[winfo rooty $canvas] + ($bottom + 1) + 2}]
			set char [Info $oop char,$where,$index]
			set text "$char\) $attrib(name)"
			BalloonShow $oop $text $x $y
}
		}

	} else {

		Info $oop highlight,where ""
		Info $oop highlight,index ""

if {"selected" in $state || !$isSelected} {
		$canvas itemconfigure sel,$where,$index \
			-outline [Info $oop color2,$where,$index]
}

		$entry delete 0 end
		$entry configure -state disabled

		$canvas itemconfigure $where,status,right \
			-text [fmt_wgt [Info $oop $where,weight] 1]

		BalloonHide $oop
	}

	Info $oop busy 0

	return
}

# NSInventory2::ContextMenu --
#
#	When an item is right-clicked, pop up a context menu of actions.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::ContextMenu {oop menu x y} {

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

	set askCmd 0
	set askItem 0
	if {[angband inkey_flags] eq "INKEY_CMD"} {
		set askCmd 1
	}
	if {[angband inkey_flags] eq "INKEY_ITEM"} {
		set askItem 1
	}
	if {$askCmd} {
		set closeCmd {Inventory2Obj Close}
		set cancelCmd {}
	} elseif {$askItem} {
		if {[Info $oop browsing]} {
			set closeCmd {
				angband keypress \033
				NSWindowManager::Undisplay inventory2
			}
		} else {
			set closeCmd {angband keypress \033}
		}
		set cancelCmd {angband keypress \033}
	} else {
		set closeCmd {Inventory2Obj Close}
		set cancelCmd {}
	}

	# No row is hit
	if {$where eq ""} {

		$menu add command -label [mc Close] -command $closeCmd
		$menu add separator
		$menu add command -label [mc Cancel] -command $cancelCmd

		# Pop up the menu
		tk_popup $menu $x $y

		# Done
		return
	}

	# Get information about this item
	angband $where info $index attrib

	# Get the item char. We can't use attrib(char) below because
	# it might be an item in a floor stack, in which case attrib(char)
	# is always 'a'.
	set itemKey [Info $oop char,$where,$index]

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
	set list [GetItemCommand $oop $where $index]
	set label [lindex $list 0]
	set command [lindex $list 1]
if 0 {
	# If this item can be sold, set the default action to "Sell"
	if {[angband store shopping] && $where eq "inventory"} {
		set match [angband inventory find -store_will_buy yes]
		if {[lsearch -exact $integer $index] != -1} {
			set cmdChar s
			if {[angband store ishome]} {
				set usageString [mc "Drop"]
			} else {
				set usageString [mc "Sell"]
			}
		}
	}
}
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

		### Don't use toggleChar: destroy only works on inventory
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

# NSInventory2::SetList --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::SetList {oop where tval {both 0}} {

	set win [Info $oop win]
	set canvas [Info $oop canvas]

	if {$where ne ""} {
		Info $oop where $where
		Info $oop tval $tval
	} else {
		set where [Info $oop where]
		set tval [Info $oop tval]
	}

	Info $oop both $both

	SetList_Equipment $oop
	SetList_Inventory $oop

	# Set window title
	if {$both} {
		wm title $win [mc Items]
	} else {
		wm title $win [mc [string totitle $where]]
	}
	
	return
}

# NSInventory2::SetList_Equipment --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::SetList_Equipment {oop} {

	set canvas [Info $oop canvas]

	# Clear the selection
	SelectionChanged $oop "" ""

	# Default to each slot not being displayed
	foreach slot [Info $oop slot_names] {
		set display($slot) 0
	}

	set doIt 1
	switch -- [Info $oop where] {
		floor {
			set doIt 0
		}
		inventory {
			if {![Info $oop both]} {
				set doIt 0
			}
		}
	}

	if {$doIt} {
		if {[Info $oop both]} {
			set itemList [angband equipment find]
		} else {
			set itemList [angband equipment find -tester yes]
		}
		set weight 0
		foreach item $itemList {

			# Get info about the item
			angband equipment info $item attrib

			if {[variant OANGBANDTK]} {

				# Fiddle with quiver items
				if {$item > 11} {

					# Sum the weights of displayed items
					incr weight [expr {$attrib(weight) * $attrib(number)}]

					continue
				}
			}
	
			set slot [lindex [Info $oop slot_names] $item]
			set display($slot) 1

			# Get the icon
			set assign $attrib(icon)

			# The assigment changed
			if {[string compare $assign [Info $oop assign,$slot]]} {
				$canvas itemconfigure icon,equipment,$slot -assign $assign
				Info $oop assign,$slot $assign
			}

			# The visibility changed
			if {![Info $oop visible,$slot]} {
				if {[info tclversion] >= 8.3} {
					$canvas itemconfigure equipment,$slot -state normal
					$canvas itemconfigure line,$slot -state normal
				} else {
					$canvas itemconfigure line,$slot -fill #989898
	
					set x [Info $oop equipment,x,$slot]
					set y [Info $oop equipment,y,$slot]
					MoveBox $oop $x $y equipment $slot
				}
				Info $oop visible,$slot 1
			}

			# Remember the char
			Info $oop char,equipment,$slot $attrib(char)

			# Remember the location
			Info $oop where,equipment,$slot equipment

			# Remember the item, != $index for floor
			Info $oop item,equipment,$slot $item

			# Sum the weights of displayed items
			incr weight [expr {$attrib(weight) * $attrib(number)}]
		}

		Info $oop equipment,weight $weight
		$canvas itemconfigure equipment,status,right -text [fmt_wgt $weight 1]

		set weightLimit [angband inventory weight_limit]
		set capacity [expr {$weightLimit / 2 + $weightLimit / 10}]
		set wgt1 [fmt_wgt [angband inventory total_weight]]
		set wgt2 [fmt_wgt $capacity]
		set wgt3 [fmt_wgt $weightLimit]
		set units [lb_or_kg]
		$canvas itemconfigure equipment,status,left -text \
			[format [mc weight3_$units] $wgt1 $wgt2 $wgt3]
	} else {
		$canvas itemconfigure equipment,status,left -text ""
		$canvas itemconfigure equipment,status,right -text ""
	}

	# Hide slots which aren't visible
	foreach slot [Info $oop slot_names] {
		if {!$display($slot) && [Info $oop visible,$slot]} {
			if {[info tclversion] >= 8.3} {
				$canvas itemconfigure equipment,$slot -state hidden
				$canvas itemconfigure line,$slot -state hidden
			} else {
				MoveBox $oop -20 -20 equipment $slot
				$canvas itemconfigure line,$slot -fill {}
			}
			Info $oop visible,$slot 0
		}
	}

	if {[variant OANGBANDTK]} {
		SetList_Quiver $oop
	}

	return
}

# NSInventory2::SetList_Inventory --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::SetList_Inventory {oop} {

	set canvas [Info $oop canvas]
	set where [Info $oop where]

	# Clear the selection
	SelectionChanged $oop "" ""

	# Clear the list
	for {set i 0} {$i <= 23} {incr i} {
		if {[info tclversion] >= 8.3} {
			$canvas itemconfigure inventory,$i -state hidden
		} else {
			MoveBox $oop -20 -20 inventory $i
		}
	}
	$canvas itemconfigure inventory,status,left -text ""
	$canvas itemconfigure inventory,status,right -text ""
	$canvas itemconfigure inventory,title -text [mc Inventory]

	switch -- $where {
		equipment {
			if {![Info $oop both]} return
			set itemList [angband inventory find]
			set whereItem inventory
		}
		floor {
			set itemList [angband floor find -tester yes]
			$canvas itemconfigure inventory,title -text [mc Floor]
			set whereItem floor
		}
		inventory {
			if {[Info $oop both]} {
				set itemList [angband inventory find]
			} else {
				set itemList [angband inventory find -tester yes]
			}
			set whereItem inventory
		}
	}

	set index 0
	set weight 0
	foreach item $itemList {

		set box $item
		if {$where eq "floor"} {
			set box $index
		}

		# Get info about the item
		angband $whereItem info $item attrib

		# Assign the icon
		$canvas itemconfigure icon,inventory,$box -assign $attrib(icon)

		# Display the box
		if {[info tclversion] >= 8.3} {
			$canvas itemconfigure inventory,$box -state normal
		} else {

			# Move the box into position
			set x [Info $oop inventory,x,$index]
			set y [Info $oop inventory,y,$index]
			MoveBox $oop $x $y inventory $box
		}

		# Hack -- Get char for floor item
		if {$where eq "floor"} {
			set attrib(char) [string index "abcdefghijklmnopqrstuvw" $index]
		}

		# Remember the char
		Info $oop char,inventory,$box $attrib(char)

		# Remember the location
		Info $oop where,inventory,$box $whereItem

		# Remember the item, != $index for floor
		Info $oop item,inventory,$box $item

		# Sum the weights of displayed items
		incr weight [expr {$attrib(weight) * $attrib(number)}]

		incr index
	}

	Info $oop inventory,weight $weight

	$canvas itemconfigure inventory,status,right -text [fmt_wgt $weight 1]

	set numItems [llength $itemList]
	if {$numItems == 1} {
		set string [format [mc "%d item"] $numItems]
	} else {
		set string [format [mc "%d items"] $numItems]
	}
	if {0 && [Info $oop where] ne "floor"} {
		set weightLimit [angband inventory weight_limit]
		set capacity [expr {$weightLimit / 2 + $weightLimit / 10}]
		append string "    [mc Threshold] [fmt_wgt $capacity 1]"
	}
	$canvas itemconfigure inventory,status,left -text $string

	return
}

if {[variant OANGBANDTK]} {

# NSInventory2::SetList_Quiver --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::SetList_Quiver {oop} {

	set canvas [Info $oop canvas]

	# Clear the selection
	SelectionChanged $oop "" ""

	for {set i 0} {$i < 10} {incr i} {
		lappend slot_names INVEN_Q$i
	}

	# Default to each slot not being displayed
	foreach slot $slot_names {
		set display($slot) 0
	}

	set doIt 1
	switch -- [Info $oop where] {
		floor {
			set doIt 0
		}
		inventory {
			if {![Info $oop both]} {
				set doIt 0
			}
		}
	}

	if {$doIt} {
		if {[Info $oop both]} {
			set itemList [angband equipment find]
		} else {
			set itemList [angband equipment find -tester yes]
		}
		set weight 0
		foreach item $itemList {

			if {$item < 13} continue
 
			set slot [lindex $slot_names [expr {$item - 13}]]
			set display($slot) 1

			# Get info about the item
			angband equipment info $slot attrib

			# Get the icon
			set assign $attrib(icon)

			# The assigment changed
			if {[string compare $assign [Info $oop assign,$slot]]} {
				$canvas itemconfigure icon,equipment,$slot -assign $assign
				Info $oop assign,$slot $assign
			}

			# The visibility changed
			if {![Info $oop visible,$slot]} {
				$canvas itemconfigure equipment,$slot -state normal
				Info $oop visible,$slot 1
			}

			# Remember the char
			Info $oop char,equipment,$slot $attrib(char)

			# Remember the location
			Info $oop where,equipment,$slot equipment

			# Remember the item, != $index for floor
			Info $oop item,equipment,$slot $item

			# Sum the weights of displayed items
			incr weight [expr {$attrib(weight) * $attrib(number)}]
		}
	}

	# Hide slots which aren't visible
	foreach slot $slot_names {
		if {!$display($slot) && [Info $oop visible,$slot]} {
			$canvas itemconfigure equipment,$slot -state hidden
			Info $oop visible,$slot 0
		}
	}

	return
}

# OANGBANDTK
}

# NSInventory2::MakeBorder --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::MakeBorder {oop canvas x y width height} {

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

# NSInventory2::MakeHeader --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::MakeHeader {oop canvas x y width text tags} {

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

# NSInventory2::MakeStatus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::MakeStatus {oop canvas x y width where} {

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
			"NSInventory2::BindStatus $oop enter $where $side"
		$canvas bind $where,status,$side <Leave> \
			"NSInventory2::BindStatus $oop leave $where $side"
	}

	return
}

# NSInventory2::BindStatus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::BindStatus {oop message where side} {

	if {$message eq "leave"} {
		NSMainWindow::StatusText [Global main,oop] ""
		return
	}

	set string ""

	switch -- $where {
		equipment {
			switch -- $side {
				left {
					set string [mc status-total-threshold-limit]
				}
				right {
					set string [mc status-weight-equip]
				}
			}
		}
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
	}

	if {[string length $string]} {
		NSMainWindow::StatusText [Global main,oop] $string
	}

	return
}

# NSInventory2::SetCanvas --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::SetCanvas {oop} {

	set canvas [Info $oop canvas]

	$canvas delete all

	$canvas create image 109 164 -image Image_Equip -anchor center -tags image
	if {[info tclversion] >= 8.3} {
		$canvas itemconfigure image -state disabled
	}

	#
	# Equipment
	#

	if {[Platform unix]} {
		set font {Helvetica 12 bold}
	}
	if {[Platform windows]} {
		set font {Helvetica 10 bold}
	}
	set fontHeight [font metrics $font -linespace]
	set heightHeader [expr {1 + $fontHeight + 1}]

	set bw 3
	set heightBox [expr {[icon height] + 6}]
	set widthBox [expr {[icon width] + 6}]
	set widthBoxMax 40

	set widthEquip 224
	set heightEquip 297

	set padRing 2

	MakeBorder $oop $canvas 0 0 $widthEquip $heightEquip
	MakeHeader $oop $canvas 0 0 $widthEquip [mc Equipment] equipment,title

	set slots [list \
		INVEN_WIELD 111 158 {119 0 0} {204 0 0} \
		INVEN_BOW 72 23 {119 0 0} {204 0 0} \
		INVEN_LEFT 160 171 {51 102 102} {102 204 204} \
		INVEN_RIGHT 164 7 {51 102 102} {102 204 204} \
		INVEN_NECK 51 148 {51 102 102} {102 204 204} \
		INVEN_LITE 26 31 {102 51 0} {255 153 0} \
		INVEN_BODY 95 89 {0 102 0} {51 204 0} \
		INVEN_OUTER 208 138 {0 102 0} {51 204 0} \
		INVEN_ARM 118 15 {0 102 0} {51 204 0} \
		INVEN_HEAD 39 89 {0 102 0} {51 204 0} \
		INVEN_HANDS 208 40 {0 102 0} {51 204 0} \
		INVEN_FEET 248 89 {0 102 0} {51 204 0} \
	]

	set lines [list \
		INVEN_WIELD {{147 167}} \
		INVEN_BOW {{82 90}} \
		INVEN_LEFT {{150 182}} \
		INVEN_RIGHT {{67 182}} \
		INVEN_NECK {{108 90}} \
		INVEN_ARM {{73 147}} \
		INVEN_HANDS {{70 175} {147 175}} \
	]
	array set data $lines

	foreach {slot y x rgb1 rgb2} $slots {
		set cy [expr {$y + 20}]
		set cx [expr {$x + 20}]

		if {[info exists data($slot)]} {
			set fill [format "#%02x%02x%02x" 153 153 153]
			foreach loc $data($slot) {
				scan $loc "%d %d" x2 y2
				$canvas create line $cx $cy $x2 $y2 -fill $fill \
					-tags "line line,$slot"
			}
		}

		set rgb1 [format "#%02x%02x%02x" {*}$rgb1]
		set rgb2 [format "#%02x%02x%02x" {*}$rgb2]
		MakeBox $oop equipment $slot $rgb1 $rgb2

		Info $oop equipment,y,$slot $cy
		Info $oop equipment,x,$slot $cx
		MoveBox $oop $cx $cy equipment $slot

		Info $oop visible,$slot 1
		Info $oop assign,$slot "icon none 0"

		lappend slot_names $slot
	}

	Info $oop slot_names $slot_names
	Info $oop width,equip $widthEquip

	#
	# Inventory
	#

	set widthInven [expr {$padRing + 3 + ($widthBoxMax + 6) * 5 + 6 + 3}]
	set heightInven $heightEquip

	set x [expr {$widthEquip + 1}]
	MakeBorder $oop $canvas $x 0 $widthInven $heightInven
	MakeHeader $oop $canvas $x 0 $widthInven [mc Inventory] inventory,title

	set left [expr {$x + $padRing + 3 + 6}]
	set top [expr {3 + ($heightHeader - 1) + 6}]
	set columns [expr {($widthInven - $padRing - 3 - 6 - 3) / ($widthBox + 6)}]
	set diff [expr {($widthInven - $padRing - 3 - 6 - 3) - $columns * ($widthBox + 6)}]

	# 23 items
	set col 0
	set row 0
	for {set i 0} {$i <= 23} {incr i} {
		MakeBox $oop inventory $i

		set x [expr {$diff / 2 + $left + $col * ($widthBox + 6) + $widthBox / 2}]
		set y [expr {$top + $row * ($heightBox + 6) + $heightBox / 2}]
		Info $oop inventory,x,$i $x
		Info $oop inventory,y,$i $y
		if {[incr col] == $columns} {
			set col 0
			incr row
		}

		if {[info tclversion] >= 8.3} {
			MoveBox $oop $x $y inventory $i
		}
	}

	#
	# Statusbar
	#

	set y $heightEquip
	if {[variant OANGBANDTK]} {
		incr y [expr {$heightBox + 8}]
	}
	set topStatus $y
	Info $oop top,status $y

	# Equipment statusbar
	MakeStatus $oop $canvas 0 $topStatus $widthEquip equipment

	# Inventory statusbar
	set x [expr {$widthEquip + 1}]
	MakeStatus $oop $canvas $x $topStatus $widthInven inventory

	set font [Value font,statusBar]
	set fontHeight [font metrics $font -linespace]
	set heightStatus [expr {3 + $fontHeight + 3}]

	set widthTotal [expr {$widthEquip + 1 + $widthInven}]
	set heightTotal [expr {$heightEquip + $heightStatus}]
	$canvas configure -width $widthTotal -height $heightTotal

	# Divider filler
	set x $widthEquip
	$canvas create line $x 0 $x $heightEquip -fill #282828

	# Hack -- ring bindings down the middle
	set y 34
	for {set i 0} {$i < 8} {incr i} {
		$canvas create image $x $y -image Image_Binding
		incr y 34
	}

	if {[variant OANGBANDTK]} {

		#
		# Quiver
		#

		set y $heightEquip
		MakeBorder $oop $canvas 0 $y $widthTotal [expr {$heightBox + 8}]
		$canvas configure -height [expr {$heightTotal + $heightBox + 8}]
		Info $oop top,quiver $y

		set x [expr {$widthTotal / 2}]
		set y [expr {2 + $heightEquip + $heightBox / 2}]
		$canvas create text $x $y -fill [format #%02x%02x%02x 0 0 200] \
			-text "Quiver" -font "Times -24 bold"

		set diff [expr {($widthTotal - ($widthBox * 10 + 6 * 9)) / 2}]
		for {set i 0} {$i < 10} {incr i} {

			set slot INVEN_Q$i

			MakeBox $oop equipment $slot
			set x [expr {$diff + $widthBox / 2 + $i * ($widthBox + 6)}]
			set y [expr {4 + $heightEquip + $heightBox / 2}]
			MoveBox $oop $x $y equipment $slot

			Info $oop assign,$slot "icon none 0"
			Info $oop visible,$slot 1
		}
	}

	# This box is used for drag & drop
	MakeBox $oop drag 0
	HideBox $oop drag 0

	# Help-text statusbar (normally hidden)
	MakeStatus $oop $canvas 0 $topStatus $widthTotal statusBar
	$canvas itemconfigure statusBar,status -state hidden

	# Alternate balloon impl
	$canvas create rectangle 0 0 10 10 -fill White -state hidden \
		-tags {balloon balloon,rect}
	$canvas create text 0 0 -anchor n -state hidden \
		-font [Value font,inventory] -tags {balloon balloon,text}

	return
}

# NSInventory2::MakeBox --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::MakeBox {oop where index {rgb1 ""} {rgb2 ""}} {

	set canvas [Info $oop canvas]

	set bw 3
	set width [expr {[icon width] + $bw * 2}]
	set height [expr {[icon height] + $bw * 2}]

	if {$rgb1 eq ""} {
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
		-fill {} -outline {} -tags "$where,$index sel,$where,$index" -width 1.0

	Info $oop color,$where,$index $rgb2
	Info $oop color2,$where,$index ""

	# Icon
	$canvas create widget [expr {$width / 2}] \
		[expr {$height / 2}] -assign {icon none 0} \
		-tags "icon $where,$index icon,$where,$index" -anchor center

	$canvas bind icon,$where,$index <Enter> \
		"NSInventory2::Request $oop enter $where $index"
	$canvas bind icon,$where,$index <Leave> \
		"NSInventory2::Request $oop leave $where $index"
	$canvas bind icon,$where,$index <ButtonPress-1> \
		"NSInventory2::Select $oop $where $index"
	$canvas bind icon,$where,$index <Double-ButtonPress-1> \
		"NSInventory2::Invoke $oop $where $index"

	return
}

# NSInventory2::MoveBox --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::MoveBox {oop x y where index} {

	set canvas [Info $oop canvas]

	scan [$canvas coords icon,$where,$index] "%s %s" cx cy
	$canvas move $where,$index [expr {$x - $cx}] [expr {$y - $cy}]

	return
}

# NSInventory2::ShowBox --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::ShowBox {oop where index} {

	set canvas [Info $oop canvas]

	if {[info tclversion] >= 8.3} {
		$canvas itemconfigure $where,$index -state ""
	}

	return
}

# NSInventory2::HideBox --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::HideBox {oop where index} {

	set canvas [Info $oop canvas]

	if {[info tclversion] >= 8.3} {
		$canvas itemconfigure $where,$index -state hidden
	} else {
		MoveBox $oop -20 -20 $where $index
	}

	return
}

# NSInventory2::CommitInscription --
#
#	Set the inscription of the selected item to the string in the
#	inscription Entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::CommitInscription {oop} {

	set entry [Info $oop inscription,entry]
	set where [Info $oop select,where]
	set index [Info $oop select,index]

	# Make sure something is selected
	if {$where eq ""} return

	# Hack -- Set the inscription.
	angband $where inscription $index [$entry get]

	# Hack
	focus [Info $oop canvas]

	return
}

# NSInventory2::Button1 --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::Button1 {oop x y} {

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

# NSInventory2::Motion1 --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::Motion1 {oop x y} {

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

# NSInventory2::Release1 --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::Release1 {oop x y} {

	set canvas [Info $oop canvas]
	set where [Info $oop select,where]
	set index [Info $oop select,index]

	# No drag in progress
	if {![Info $oop dragging]} return
	
	HideBox $oop drag 0
	Info $oop dragging 0

	set charItem [Info $oop char,$where,$index]

	# Drop onto the floor
	if {$y > [Info $oop top,status]} {
		set toggleChar ""
		set command_see 0
		if {$command_see} {
			if {[Info $oop where] ne $where} {
				set toggleChar /
			}
		} else {
			if {$where eq "equipment"} {
				if {[angband inventory count]} {
					set toggleChar /
				}
			}
		}
		DoUnderlyingCommand d$toggleChar$charItem
		SkipMoreMessages $oop 1

		# Done
		return
	}

	# Find out where the drag ended
	if {$x < [Info $oop width,equip]} {
		set dest equipment
	} else {
		set dest inventory
		if {[variant OANGBANDTK]} {
			if {$y > [Info $oop top,quiver]} {
				set dest equipment
			}
		}
	}

	# Drag onto source does nothing
	if {$where eq $dest} return

	switch -- $where {
		equipment {
			DoUnderlyingCommand t$charItem
			SkipMoreMessages $oop 1
		}
		inventory {
			DoUnderlyingCommand w$charItem
			SkipMoreMessages $oop 1
		}
	}

	return
}

# NSInventory2::Enter --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::Enter {oop where index} {

#	if {[Info $oop select,where] ne ""} return

	Highlight $oop {on mouse} $where $index

	if {$where eq "equipment"} {
		angband equipment info $index attrib
		NSMainWindow::StatusText [Global main,oop] $attrib(label)
	}

	return
}

# NSInventory2::Leave --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::Leave {oop where index} {

	if {1 || [Info $oop select,index] eq ""} {
		Highlight $oop {off} $where $index
	}

	if {$where eq "equipment"} {
		NSMainWindow::StatusText [Global main,oop] ""
	}

	# Re-enter the selected item, if any
	set where [Info $oop select,where]
	set index [Info $oop select,index]
	if {$where ne ""} {
		Highlight $oop {on} $where $index
		if {$where eq "equipment"} {
			angband equipment info $index attrib
			NSMainWindow::StatusText [Global main,oop] $attrib(label)
		}
	}

	return
}

# NSInventory2::Request --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::Request {oop type where index} {

	Info $oop request [list $type $where $index]
	if {[Info $oop request,id] eq ""} {
		Info $oop request,id [after 1 NSInventory2::HandleRequest $oop]
	}

	return
}

# NSInventory2::HandleRequest --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::HandleRequest {oop} {

	if {[Info $oop busy]} {
		Info $oop request,id [after 1 NSInventory2::HandleRequest $oop]
		return
	}

	set request [Info $oop request]
	Info $oop request {}

	set type [lindex $request 0]
	set where [lindex $request 1]
	set index [lindex $request 2]

	if {[Info $oop highlight,where] ne ""} {
		Leave $oop [Info $oop highlight,where] [Info $oop highlight,index]
	}

	# Double-check that the item exists
	if {$where eq "inventory"} {
		if {$index >= [angband inventory count]} {
			Info $oop request,id ""
			return
		}
	}

	if {$type eq "enter"} {
		Enter $oop $where $index
	}

	# Clear this after above stuff. See Request()
	Info $oop request,id ""

	if {[llength [Info $oop request]]} {
		Info $oop request,id [after 1 NSInventory2::HandleRequest $oop]
	}

	return
}

# NSInventory2::BalloonShow --
#
#	Show the balloon.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::BalloonShow {oop text x y} {

	# XXX We used to use NSBalloon, but some users said the
	# balloon would disappear randomly (in the Store, anyways).
	# UPDATE: It "disappeared" because it wasn't shown when one of the items
	# is selected, so NSBalloon should be fine.
	if {0} {
		NSBalloon::Show $text $x $y n
		return
	}

	set canvas [Info $oop canvas]

	incr x -[winfo rootx $canvas]
	incr y -[winfo rooty $canvas]

	set font [Value font,inventory]
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

	# If balloon would obscure the statusbar, then put it above
	if {$y + $rectHeight > [Info $oop top,status]} {
		incr y [expr {0 - ([icon height] + 8 + $rectHeight + 4)}]
	}

	$canvas coords balloon,rect [expr {$x - $rectWidth / 2}] $y \
		[expr {$x + $rectWidth / 2}] [expr {$y + $rectHeight}]
	$canvas coords balloon,text $x [expr {$pady + $y + 1}]
	$canvas itemconfigure balloon,text -text $text

	# Show the items
	$canvas itemconfigure balloon -state normal

	return
}

# NSInventory2::BalloonHide --
#
#	Hide the balloon.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::BalloonHide {oop} {

	if {0} {
		NSBalloon::Hide
		return
	}

	set canvas [Info $oop canvas]

	# Hide the items
	$canvas itemconfigure balloon -state hidden

	return
}

# NSInventory2::Track --
#
#	Handle <Track> quasi-event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::Track {oop what} {

	switch -- $what {
		equipment {
			SetList_Equipment $oop
		}
		inventory {
			SetList_Inventory $oop

			# Hack -- Do this so the statusbar weight is correct
			SetList_Equipment $oop
		}
	}

	return
}

# NSInventory2::ChooseItem --
#
#	Handle <Choose-item> quasi-event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::ChooseItem {oop show other} {

dbwin "NSInventory2::ChooseItem $show $other\n"

	Info $oop choose,show $show
	Info $oop choose,other $other
	if {$show} {
		Info $oop didChoose 0
	}

	qeconfigure NSInventory2 <Term-inkey> -active yes

	return
}

# NSInventory2::TermInkey --
#
#	Handle <Term-inkey> quasi-event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::TermInkey {oop} {

dbwin "NSInventory2::TermInkey\n"

	set win [Info $oop win]

	qeconfigure NSInventory2 <Term-inkey> -active no

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
dbwin "NSInventory2::TermInkey SKIPPED\n"
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
		SetList $oop inventory "" 1
	}

	return
}

# NSInventory2::Swap --
#
#	Display the "old" Inventory Window
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::Swap {oop} {

	set browsing [Info $oop browsing]
	set where [Info $oop where]
	set both [Info $oop both]

	NSWindowManager::Undisplay inventory2

	NSModule::LoadIfNeeded NSInventory
	NSWindowManager::Display inventory $where "" $both
	if {[Info $oop choose,show]} {
		InventoryObj Info browsing $browsing
		InventoryObj Info choose,show 1
		InventoryObj Info choose,other $where
		InventoryObj Info didChoose 1
		if {![string match [Info $oop win]* [Info $oop oldFocus]]} {
			InventoryObj Info oldFocus [Info $oop oldFocus]
		} else {
			InventoryObj Info oldFocus [InventoryObj Info win]
		}
	}

	Value inventory,style old

	return
}

# NSInventory2::AlwaysOnTop --
#
#	Toggle the "Always On Top" option.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::AlwaysOnTop {oop} {

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

	# Synchronize the non-graphical inventory window
	if {[info exists ::Windows(inventory)] && [winfo exists [Window inventory]]} {
		if {$onTop} {
			wm transient [Window inventory] [Window main]

			# Work around a Tk bug (can't remember actual version)
			if {[string compare [info patchlevel] 8.3.3] < 0} {
				wm withdraw [Window inventory]
			}
		} else {
			wm transient [Window inventory] ""
		}
		InventoryObj Info alwaysOnTop $onTop
	}

	return
}

# NSInventory2::ValueChanged_font_inventory --
#
#	Called when the font,inventory value changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::ValueChanged_font_inventory {oop} {

	set canvas [Info $oop canvas]

	$canvas itemconfigure balloon,text -font [Value font,inventory]

	return
}

# NSInventory2::ValueChanged_font_statusBar --
#
#	Called when the font,statusBar value changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::ValueChanged_font_statusBar {oop} {

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

# NSInventory2::SkipMoreMessages --
#
#	Skip -more- prompts during drag & drop,
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::SkipMoreMessages {oop skip} {

	if {$skip} {
		qebind NSInventory2 <Inkey-more> {angband keypress \033}
		qebind NSInventory2 <Inkey-cmd> {Inventory2Obj SkipMoreMessages 0}
	} else {
		qeunbind NSInventory2 <Inkey-more>
		qeunbind NSInventory2 <Inkey-cmd>
	}

	return
}

# NSInventory2::IconCfg --
#
#	Called when the game's icon configuration changes
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSInventory2::IconCfg {oop} {

	set canvas [Info $oop canvas]

	SetCanvas $oop
	wm geometry [Info $oop win] ""

	if {[winfo ismapped [Info $oop win]]} {
		SetList_Equipment $oop
		SetList_Inventory $oop
	}

	return
}

proc Inventory2Obj {command args} {
	return [NSInventory2::$command [Global inventory2,oop] {*}$args]
}

