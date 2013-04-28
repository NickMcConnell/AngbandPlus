# File: choice-window.tcl

# Purpose: the Choice Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSChoiceWindow {

	variable Priv

# namespace eval NSChoiceWindow
}

# Comment
proc comment {} {

Choose-spell 1
	SetHook spell
	Term-fresh -active yes
	display,what = bookNum
Term-fresh
	SetList
	Term-fresh -active no
Choose-spell 0
	SetHook default
	Term-fresh -active yes


Track-inventory
	SetList (if hook=inventory)

# Comment
}

# NSChoiceWindow::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::InitModule {} {

	MsgCatInit inven choice

	NSObject::New NSChoiceWindow

	return
}

# NSChoiceWindow::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::CloseModule {} {

	catch {
		set oop [Global choice,oop]
		set win [Window choice]
		qeunbind NSChoiceWindow
		destroy $win
	}

	return
}

# NSChoiceWindow::NSChoiceWindow --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSChoiceWindow::NSChoiceWindow {oop} {

	Info $oop expand [Value choicewindow,autoexpand]
	Info $oop showIcon [Value choicewindow,showicon]

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow choice $win \
		"NSChoiceWindow::GeometryCmd $oop" "" "NSChoiceWindow::DisplayCmd $oop"

	qebind NSChoiceWindow <Choose> \
		"NSChoiceWindow::Choose $oop %d %s %o"

	qebind NSChoiceWindow <Track> \
		"NSChoiceWindow::Track $oop %d"
	qeconfigure NSChoiceWindow <Track> -active no

	qebind NSChoiceWindow <Term-fresh> \
		"NSChoiceWindow::Fresh_Display $oop"
	qeconfigure NSChoiceWindow <Term-fresh> -active no

	qebind NSChoiceWindow <Setting-show_flavors> \
		"after idle NSChoiceWindow::Fresh_Display $oop"
	qeconfigure NSChoiceWindow <Setting-show_flavors> -active no

	qebind NSChoiceWindow <IconCfg> \
		"NSChoiceWindow::IconCfg $oop"

	# Update ourself when the list highlight color changes
	qebind NSChoiceWindow <Value-listHilite> \
		"$win.frame.text tag configure HOT \
		-background \[Value listHilite]"

	# Update ourself when the font changes
	qebind NSChoiceWindow <Value-font,choice> \
		"NSChoiceWindow::ValueChanged_font_choice $oop"

	Info $oop hook ""
	Info $oop current ""
	Info $oop choosing 0
	Info $oop choosing,what ""
	Info $oop default,hook hook_item
	Info $oop default,what inventory
	Info $oop display,what inventory
	Info $oop inConfigure 0
	Info $oop busy 0
	Info $oop expanded 0

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSChoiceWindow $oop $win

	#
	# Global list of application windows
	#

	Global choice,oop $oop
	Window choice $win

	return
}

# NSChoiceWindow::~NSChoiceWindow --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::~NSChoiceWindow {oop} {

	return
}

# NSChoiceWindow::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::Info {oop info args} {

	global NSChoiceWindow

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSChoiceWindow($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSChoiceWindow($oop,$info)
			}
		}
	}

	return
}

# NSChoiceWindow::InitWindow --
#
#	Create the window.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSChoiceWindow::InitWindow {oop} {

	set win .choice$oop
	toplevel $win
	wm title $win [mc title]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Feed the Term when keys are pressed
	Term_KeyPress_Bind $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSChoiceWindow::Close $oop"

	# Set instance variables
	Info $oop win $win

	set frame $win.frame
	frame $frame -relief sunken -borderwidth 1 -background Black

	# Canvas to display icon
	set iconWidth [expr {[icon width] + 8}]
	set iconHeight [expr {[icon height] + 8}]
	set canvas $frame.icon
	canvas $canvas \
		-borderwidth 0 -width $iconWidth -height $iconHeight -background Black \
		-highlightthickness 0
	$canvas create widget \
		6 6 -tags icon

	# Create an arrow which appears when there is content out of sight
	set x [expr {$iconWidth / 2}]
	$canvas create polygon [expr {$x - 3}] 46 [expr {$x + 3}] 46 \
		$x 49 -fill Red -outline Red -tags arrow

	# Keep this synched with NSRecall
	if {[ScreenHeight] >= 1024} { # 1280x1024
		set height 10
	} elseif {[ScreenHeight] >= 768} { # 1024x768
		set height 8
	} elseif {[ScreenHeight] >= 600} { # 800x600
		set height 4
	} else { # 640x480
		set height 4
	}

	text $frame.text \
		-wrap none -width 50 -height $height -font [Value font,choice] \
		-borderwidth 0 -setgrid no -highlightthickness 0 \
		-padx 4 -pady 2 -background Black -foreground White -cursor "" \
		-yscrollcommand "$frame.yscroll set" \
		-xscrollcommand "$frame.xscroll set"
	scrollbar $frame.yscroll \
		-orient vertical -command "$frame.text yview"
	scrollbar $frame.xscroll \
		-orient horizontal -command "$frame.text xview"

	# Bypass default Text bindings
	bindtags $frame.text [list $frame.text $win all]

	grid rowconfig $frame 0 -weight 1
	grid rowconfig $frame 1 -weight 0
	grid columnconfig $frame 0 -weight 0
	grid columnconfig $frame 1 -weight 1
	grid columnconfig $frame 2 -weight 0

	grid $frame.icon -in $frame \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ns
	grid $frame.text -in $frame \
		-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky news
	grid $frame.yscroll \
		-row 0 -column 2 -rowspan 1 -columnspan 1 -sticky ns
	grid $frame.xscroll \
		-row 1 -column 1 -rowspan 1 -columnspan 1 -sticky ew

	grid remove $frame.yscroll
	grid remove $frame.xscroll
	Info $oop scrollbar,vert 0
	Info $oop scrollbar,horz 0

	if {![Info $oop showIcon]} {
		grid remove $frame.icon
	}

	pack $frame \
		-expand yes -fill both

	# Set instance variables
	Info $oop icon $frame.icon
	Info $oop text $frame.text

	# Fiddle with the selection for list behaviour
	$frame.text tag configure HOT -foreground White \
		-background [Value listHilite]

#	$frame.text tag bind HOT <ButtonPress-1> \
#		"NSChoiceWindow::ButtonPress1 $oop %x %y"
	$frame.text tag bind HOT <Double-ButtonPress-1> \
		"NSChoiceWindow::DoubleClick $oop"
	$frame.text tag bind TEXT <Motion> \
		"NSChoiceWindow::Motion $oop \[$frame.text index {@%x,%y linestart}]"
	$frame.text tag bind HOT <Leave> \
		"NSChoiceWindow::Motion $oop {}"

	bind $win <Leave> \
		"NSChoiceWindow::Motion $oop {}"

#	bind $win <Enter> {raise %W}
#	bind $win <Leave> {raise [Window main]}

	bind $win <ButtonPress-1> \
		[list NSChoiceWindow::ButtonPress1 $oop %x %y]

	#
	# Context Menu
	#

	set menu $win.context
	menu $menu -tearoff 0
#	$frame.text tag bind TEXT <ButtonPress-3> \
#		"NSChoiceWindow::ContextMenu_Hook $oop $menu %X %Y"
	bind $frame.icon <ButtonPress-3> \
		"NSChoiceWindow::ContextMenu $oop $menu %X %Y"
	bind $frame.text <ButtonPress-3> \
		"NSChoiceWindow::ContextMenu_Hook $oop $menu %X %Y"

	bind $win <KeyPress-slash> \
		"NSChoiceWindow::Toggle $oop ; break"

	# Window expands and contracts as the mouse enters and leaves it
	bindtags $win [concat [bindtags $win] ChoiceWindowBindTag]
	bind ChoiceWindowBindTag <Enter> "NSChoiceWindow::Expand $oop"
	bind ChoiceWindowBindTag <Leave> "NSChoiceWindow::Contract $oop"

	if {[Platform unix]} {

		# When the inactive window is clicked, I get a <Leave> event
		# followed by an <Enter> event. The <Leave> Contract()'s the window
		# and removes the highlight (if any).

		bind ChoiceWindowBindTag <Leave> "
			if {!\[NSChoiceWindow::HasCursor $oop]} {
				NSChoiceWindow::Contract $oop
			}
		"
		$frame.text tag bind HOT <Leave> "
			if {!\[NSChoiceWindow::CursorHot $oop %x %y]} {
				NSChoiceWindow::Motion $oop {}
			}
		"
		bind $win <Leave> "
			if {!\[NSChoiceWindow::HasCursor $oop]} {
				NSChoiceWindow::Motion $oop {}
			}
		"

		proc CursorHot {oop x y} {
			set text [Info $oop text]
			if {![llength [$text tag ranges HOT]]} {return 0}
			set index [$text index @$x,$y]
			if {[$text compare $index < HOT.first] ||
				[$text compare $index > HOT.last]} {return 0}
			return 1
		}
	}

	# When the window changes size, reposition the indicator arrow
	bind $frame.text <Configure> \
		"NSChoiceWindow::Configure $oop"

	#####
	# Toggle button
	#####
	set canvas $frame.text.toggle
	canvas $canvas -borderwidth 0 -highlightthickness 0 -background Black \
		-width 20 -height 20
#	$canvas create text 23 0 -anchor ne -text E -fill White \
#		-font {Courier 12} -tags label
	$canvas create polygon {0 0 0 19 19 19} -width 1 -fill Black \
		-outline White -tags poly
	$canvas bind poly <Enter> "$canvas itemconfigure poly -fill gray20"
	$canvas bind poly <Leave> "$canvas itemconfigure poly -fill Black"
	bind $canvas <ButtonPress-1> "NSChoiceWindow::Toggle $oop"

	Info $oop toggle,win $canvas
	
	return
}

# NSChoiceWindow::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::DisplayCmd {oop message first args} {

	switch -- $message {
		preDisplay {
			if {[Info $oop choosing]} {
				set hook hook_[Info $oop choosing,what]
			} else {
				set hook hook_item
			}
			SetHook $oop $hook
			CallHook $oop fresh
			if {![Info $oop choosing]} {
				qeconfigure NSChoiceWindow <Term-fresh> -active no
			}
		}
		postDisplay {
Setting show_choices 0
			Value choicewindow,show 1
		}
		postWithdraw {
			SetHook $oop ""
			Value choicewindow,show 0
		}
	}

	return
}

# NSChoiceWindow::GeometryCmd --
#
#	Called by NSWindowManager::Setup(). Returns the desired (default)
#	geometry for the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::GeometryCmd {oop} {

	set win [Info $oop win]
	set winMain [Window main]
	set spacing [Global WindowSpacing]

	scan [ScreenArea] "%d %d %d %d" waLeft waTop waRight waBottom
	set waHeight [expr {$waBottom - $waTop}]
	set waWidth [expr {$waRight - $waLeft}]

	set x [expr {[NSToplevel::FrameLeft $winMain] +
		[NSToplevel::TotalWidth $winMain] / 2}]
	set width [NSToplevel::ContentWidth $win \
		[expr {[NSToplevel::TotalWidth $winMain] / 2}]]

	set y [expr {[NSToplevel::FrameBottom $winMain] + $spacing}]
	if {$waBottom - $y < 100} {
		set y [expr {$waBottom - 100}]
		set height [NSToplevel::ContentHeight $win 100]
	} elseif {($y + [NSToplevel::TotalHeight $win]) < $waBottom} {
		set height [winfo height $win]
	} else {
		set height [expr {$waBottom - [NSToplevel::FrameBottom $winMain]}]
		set height [NSToplevel::ContentHeight $win $height]
	}
	return ${width}x$height+$x+$y
}

# NSChoiceWindow::Close --
#
#	Description. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSChoiceWindow::Close {oop} {

	NSWindowManager::Undisplay choice

	return
}

# NSChoiceWindow::SetHook --
#
#	Set the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::SetHook {oop hook} {

	if {$hook ne ""} {
		Info $oop hook $hook
		CallHook $oop open
		qeconfigure NSChoiceWindow <Term-fresh> -active yes
		qeconfigure NSChoiceWindow <Track> -active yes
		qeconfigure NSChoiceWindow <Setting-show_flavors> -active yes
	} elseif {[Info $oop hook] ne ""} {
		Info $oop hook ""
		qeconfigure NSChoiceWindow <Track> -active no
		qeconfigure NSChoiceWindow <Setting-show_flavors> -active no
	}

	return
}

# NSChoiceWindow::CallHook --
#
#	Call the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::CallHook {oop message args} {

	return [uplevel #0 NSChoiceWindow::[Info $oop hook] $oop $message $args]
}

# NSChoiceWindow::Fresh_Display --
#
#	Calls the hook to set the list, if required. Called as a command
#	on the "Term-fresh" quasi-event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::Fresh_Display {oop} {

	CallHook $oop fresh

	qeconfigure NSChoiceWindow <Term-fresh> -active no

	return
}

# NSChoiceWindow::SetList --
#
#	Clears the text, sets the icon to "none 0" and calls the
#	hook to set the text.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::SetList {oop} {

	set win [Info $oop win]
	set icon [Info $oop icon]
	set textBox [Info $oop text]

	# Clear the text
	$textBox delete 1.0 end

	# Clear the icon
	$icon itemconfigure icon -assign "icon none 0"

	Info $oop match {}

	# Call the hook to set the list
	CallHook $oop set_list

	# Something is displayed
	Info $oop display something

	# No item is highlighted
	Info $oop current ""

	ExpandAgain $oop

	# Synch the scrollbars
	Configure $oop

	# Set the window title
	set title [CallHook $oop title]
	wm title [Info $oop win] [format [mc title] $title]

	return
}

# NSChoiceWindow::NewListItem --
#
#	Create a new list item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::NewListItem {oop char string color args} {

	set wText [Info $oop text]

	$wText insert end "$char\) " TEXT $string \
		[list TEXT_$char TEXT] "\n"
	$wText tag configure TEXT_$char -foreground $color

	lappend ::NSChoiceWindow($oop,match) $args

	return
}

# NSChoiceWindow::ButtonPress1 --
#
#	Called when the window is clicked. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::ButtonPress1 {oop x y} {

	switch -- [angband inkey_flags] {
		INKEY_MORE {
			angband keypress " "
			return
		}
	}

	if {[Info $oop current] ne ""} {
		set index [Info $oop current]
		set row [expr {[lindex [split $index .] 0] - 1}]
		CallHook $oop invoke {*}[lindex [Info $oop match] $row]
	}

	return
}

# NSChoiceWindow::Invoke --
#
#	Called when a list item is clicked. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::Invoke {oop index} {

	set row [expr {[lindex [split $index .] 0] - 1}]

	CallHook $oop invoke {*}[lindex [Info $oop match] $row]

	return
}

# NSChoiceWindow::DoubleClick --
#
#	Called when a list item is double-clicked. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::DoubleClick {oop} {

	set index [Info $oop current]
	set row [expr {[lindex [split $index .] 0] - 1}]

	CallHook $oop doubleclick {*}[lindex [Info $oop match] $row]

	return
}

# NSChoiceWindow::Motion --
#
#	Called when the mouse moves in a list item. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::Motion {oop index} {

	set textBox [Info $oop text]

	# If you invoke an item, hold down the mouse, and drag...
	if {[Info $oop hook] eq ""} return

	# No tracking while menu is up
	if {[Info $oop busy]} return

	# See if the item has changed
	if {$index eq [Info $oop current]} return

	# An item is highlighted
	if {[Info $oop current] ne ""} {

		# Remove highlighting
		UnhighlightItem $oop [Info $oop current]
	}

	if {![CallHook $oop highlight_ok]} {
		set index ""
	}

	# An item is under the pointer
	if {$index ne ""} {

		# Highlight the item
		HighlightItem $oop $index
	}

	# Remember which item is highlighted
	Info $oop current $index

	return
}

# NSChoiceWindow::HighlightItem --
#
#	Highlights a list item. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::HighlightItem {oop index} {

	set textBox [Info $oop text]
	set row [expr {[lindex [split $index .] 0] - 1}]

	# Highlight the item
	$textBox tag add HOT $index "$index lineend"
	$textBox tag raise HOT

	# Call the hook (to set the icon, for example)
	CallHook $oop highlight {*}[lindex [Info $oop match] $row]

	return
}

# NSChoiceWindow::UnhighlightItem --
#
#	Removes highlighting from a list item. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::UnhighlightItem {oop index} {

	set win [Info $oop win]
	set icon [Info $oop icon]
	set textBox [Info $oop text]

	# Unhighlight the item
	$textBox tag remove HOT 1.0 end

	# Clear the icon
	$icon itemconfigure icon -assign "icon none 0"

	return
}

# NSChoiceWindow::ShowItems --
#
#	Description. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::ShowItems {oop args} {

	set textBox [Info $oop text]

	$textBox delete 1.0 end

	# Get the list of matching item indexes
	set invOrEquip inventory
	set itemList [angband $invOrEquip find]

	# Process each item
	foreach index $itemList {

		# Get item info
		angband $invOrEquip info $index attrib

		if {$invOrEquip eq "floor"} {
			set attrib(char) [string index "abcdefghijklmnopqrstuvw" \
				[lsearch -integer $itemList $index]]
		}

		# Get the color
		set color [default_tval_to_attr $attrib(tval)]

		# Append the character and description
		$textBox insert end "$attrib(char) $attrib(name)" \
			ITEM_$index "\n"
		$textBox tag configure ITEM_$index -foreground $color
	}

	# Delete trailing newline
	$textBox delete "end - 1 chars"

	return
}

# NSChoiceWindow::GetItemCommand --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::GetItemCommand {oop index} {

	set where [Info $oop display,what]
	set shopping [angband store shopping]
	return [NSGlobal::GetItemCommand $where $index $shopping]
###
return

	upvar $_command command $_label label

	set where [Info $oop display,what]

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

	# If this item can be sold, set the default action to "Sell"
	if {[angband store shopping] && ($where eq "inventory")} {
		set match [angband inventory find -store_will_buy yes]
		if {[lsearch -integer $match $index] != -1} {
			set charCmd s
			if {[angband store ishome]} {
				set label [mc Drop]
			} else {
				set label [mc Sell]
			}
		}
	}

	if {[string length $label]} {
		set command "DoKeymapCmd {} $charCmd $charItem"
	}

	return
}

# NSChoiceWindow::HasCursor --
#
#	See if the cursor is over the window. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::HasCursor {oop} {

	return [NSUtils::HasCursor [Info $oop win] 1]
}

# NSChoiceWindow::Expand --
#
#	Resizes the Choice Window to display all of the information in it.
#	Does nothing if the window is already expanded. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::Expand {oop} {

	variable Priv

place [Info $oop toggle,win] -relx 1.0 -rely 0.0 -anchor ne

	# Option: expand/contract
	if {![Info $oop expand]} return

	if {[Info $oop busy]} return
	if {[Info $oop expanded]} return

	set win [Info $oop win]
	set textBox [Info $oop text]

	set textHeight [winfo height $textBox]
	set lineHeight [font metrics [Value font,choice] -linespace]
	scan [$textBox index end] %d.%d numRows char
	set height [expr {$lineHeight * ($numRows - 1) + 4 + 2}]

	set winHeight [winfo height $win]
	set winWidth [winfo width $win]

	if {$height <= $winHeight} return

	# If the window is closer to the top of the screen, then
	# expand downwards, otherwise expand upwards.
	set top [NSToplevel::FrameTop $win]
	set topDist $top
	if {$topDist < 0} {set topDist 0}
	set bottom [NSToplevel::FrameBottom $win]
	set bottomDist [expr {[ScreenHeight $win] - $bottom}]
	if {$bottomDist < 0} {set bottomDist 0}
	if {$topDist < $bottomDist} {
		set expandUp 0
	} else {
		set expandUp 1
	}

	# Save the current window geometry
	Info $oop geometry [wm geometry $win]

	Info $oop busy 1
	Info $oop expanded 1

	# Hack -- don't raise the window if a debug window is in front of
	# the Main window, otherwise the debug window disappears behind the
	# Main window and its transients.
	set raise 1
	if {[Platform windows] && [console eval {focus}] eq ".console"} {
		set raise 0
	} else {
		set stackOrder [wm stackorder .]
		set indexMain [lsearch -exact $stackOrder [Window main]]
		incr indexMain
		foreach w [lrange $stackOrder $indexMain end] {
			if {[wm transient $w] ne [Window main]} {
				set raise 0
				break
			}
		}
	}
	if {$raise} {
		raise $win
	}

	set x [NSToplevel::FrameLeft $win]
	if {$expandUp} {
		set y [expr {[NSToplevel::FrameTop $win] - ($height - $winHeight)}]
	} else {
		set y [NSToplevel::FrameTop $win]
	}
	wm geometry $win ${winWidth}x$height+$x+$y
	update

	Info $oop busy 0

	# If the cursor moved outside the Choice Window, collapse it
	if {![HasCursor $oop]} {
		Contract $oop
	}

	return
}

# NSChoiceWindow::Contract --
#
#	Restores the window geometry to the size it was before it was
#	expanded. Does nothing if the window is not expanded. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::Contract {oop} {

	if {[Info $oop busy]} return

place forget [Info $oop toggle,win]

	if {![Info $oop expanded]} return

	Info $oop busy 1
	Info $oop expanded 0

	set win [Info $oop win]
	wm geometry $win [Info $oop geometry]
	update

	Info $oop busy 0

	return
}

# NSChoiceWindow::ShowToggleButton --
#
#	Displays a button to toggle between inventory/equipment. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::ShowToggleButton {oop} {

	if {[Info $oop toggle,visible]} return

	set $toggle [Info $oop toggle,win]
	place $toggle

	Info $oop toggle,visible 1

	return
}

# NSChoiceWindow::ContextMenu_Hook --
#
#	When a list item is right-clicked, pop up a context
#	menu of actions.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::ContextMenu_Hook {oop menu x y} {

	set text [Info $oop text]

	# Find the hit row
	set x1 [expr {$x - [winfo rootx $text]}]
	set y1 [expr {$y - [winfo rooty $text]}]
	set index [$text index "@$x1,$y1"]
	if {[lsearch -exact [$text tag names $index] TEXT] == -1} {
		ContextMenu $oop $menu $x $y
		return
	}
	set row [expr {[lindex [split $index .] 0] - 1}]

	# Clear the menu
	$menu delete 0 end

	# Set the menu
	if {[CallHook $oop context_menu $menu $row {*}[lindex [Info $oop match] $row]]} {

		# Hack -- Try to prevent collapsing while popup is visible.
		# It would be nice if "winfo ismapped $menu" worked
		Info $oop busy 1

		# Pop up the menu
		tk_popup $menu $x $y

		if {[Platform unix]} {
			tkwait variable ::tk::Priv(popup)
		}

#		after idle NSChoiceWindow::Info $oop busy 0
		Info $oop busy 0

		set index ""
		if {[NSUtils::HasCursor $text]} {
			set x [expr {[winfo pointerx $text] - [winfo rootx $text]}]
			set y [expr {[winfo pointery $text] - [winfo rooty $text]}]
			set index2 [$text index @$x,$y]
			foreach tag [$text tag names $index2] {
				if {$tag eq "TEXT"} {
					set index "$index2 linestart"
					break
				}
			}
		}
		Motion $oop $index
	}

	return
}

# NSChoiceWindow::ContextMenu --
#
#	When the window is right-clicked, pop up a menu of options.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::ContextMenu {oop menu x y} {

	set text [Info $oop text]

	$menu delete 0 end

	if {[Info $oop display,what] eq "inventory"} {
		set label [mc "Show Equipment"] \
	} else {
		set label [mc "Show Inventory"]
	}
	$menu add command -label $label \
		-command "NSChoiceWindow::Toggle $oop"

	$menu add command -label [mc "Set Font"] \
		-command "NSModule::LoadIfNeeded NSFont ; NSWindowManager::Display font choice"
	$menu add checkbutton -label [mc "Auto Expand"] \
		-variable NSChoiceWindow($oop,expand) \
		-command "NSChoiceWindow::OptionChanged $oop expand autoexpand"

	Info $oop showFlavors [Setting show_flavors]
	$menu add checkbutton -label [mc "Show Flavors"] \
		-variable NSChoiceWindow($oop,showFlavors) \
		-command "Setting show_flavors \$NSChoiceWindow($oop,showFlavors)"

	$menu add checkbutton -label [mc "Show Icon"] \
		-variable NSChoiceWindow($oop,showIcon) \
		-command "NSChoiceWindow::OptionChanged $oop showIcon showicon"
	$menu add separator
	$menu add command -label [mc Cancel]

	# Hack -- Try to prevent collapsing while popup is visible.
	# It would be nice if "winfo ismapped $menu" worked
	Info $oop busy 1

	# Pop up the menu
	tk_popup $menu $x $y

	if {[Platform unix]} {
		tkwait variable ::tk::Priv(popup)
	}

#	after idle NSChoiceWindow::Info $oop busy 0
	Info $oop busy 0

	set index ""
	if {[NSUtils::HasCursor $text]} {
		set x [expr {[winfo pointerx $text] - [winfo rootx $text]}]
		set y [expr {[winfo pointery $text] - [winfo rooty $text]}]
		set index2 [$text index @$x,$y]
		foreach tag [$text tag names $index2] {
			if {$tag eq "TEXT"} {
				set index "$index2 linestart"
				break
			}
		}
	}
	Motion $oop $index

	return
}

if {[variant OANGBANDTK]} {

proc NSChoiceWindow::hook_ele_attack {oop message args} {

	switch -- $message {

		open {
		}

		fresh {
			SetList $oop
		}

		close {
		}

		title {
			return [mc title-ele_attack]
		}

		set_list {

			set textBox [Info $oop text]

			# These colors must match those below
			set data [list \
				a Fire [Value TERM_RED] \
				b Cold [Value TERM_L_WHITE] \
				c Acid [Value TERM_L_DARK] \
				d Electricity [Value TERM_L_BLUE] \
			]

			# This bit of code must match that in cmd5.c
			set num [expr {([angband player level] - 20) / 7}]
			if {$num <= 0} {
				set num 1
			}
			set data [lrange $data 0 [expr {$num * 3 - 1}]]

			# Process each attack
			foreach {char attack color} $data {

				# Append the character and description
				NewListItem $oop $char $attack $color \
					$char
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"
		}

		invoke {

			if {![Info $oop choosing]} return
			lassign $args char
			angband keypress $char
		}

		highlight_ok {
			return 1
		}

		highlight {
		}

		context_menu {

			lassign $args menu row char

			set font [$menu cget -font]

			# No row is hit
			if {$row == -1} {

				$menu add command -label [mc Cancel] \
					-command {angband keypress \033}
				return 1
			}

			# Append a command to select the item
			set command "angband keypress $char"
			$menu add command -label "Select This Attack" -command $command \
				-font [BoldFont $font]
			$menu add separator
			$menu add command -label [mc Cancel] -command {angband keypress \033}

			return 1
		}
	}

	return
}

# OANGBANDTK
}

proc NSChoiceWindow::hook_item {oop message args} {

	switch -- $message {

		open {
		}

		fresh {
			SetList $oop
		}

		close {
		}

		title {
			return [mc title-[Info $oop display,what]]
		}

		set_list {
			set textBox [Info $oop text]

			# Get the list of matching item indexes
			set invOrEquip [Info $oop display,what]
			set itemList [angband $invOrEquip find -tester yes]
			set itemListAll [angband $invOrEquip find]

			# Comment out this line to display all items
			set itemListAll $itemList

			# Process each item
			foreach index $itemListAll {

				# Get item info
				angband $invOrEquip info $index attrib

				if {$invOrEquip eq "floor"} {
					set attrib(char) [string index "abcdefghijklmnopqrstuvw" \
						[lsearch -integer $itemList $index]]
				}

				if {[lsearch -integer $itemList $index] == -1} {

					# Append the character and description
					NewListItem $oop $attrib(char) $attrib(name) gray40 \
						$index $attrib(char) 0

				} else {

					# Get the color
					set color [default_tval_to_attr $attrib(tval)]

					# Append the character and description
					NewListItem $oop $attrib(char) $attrib(name) $color \
						$index $attrib(char) 1
				}
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"
		}

		invoke {

			# Don't invoke when verifying a choice
			if {![Info $oop choosing] ||
				([angband inkey_flags] ne "INKEY_ITEM")} return

			lassign $args index char valid
			angband keypress $char
		}

		doubleclick {

			if {[angband inkey_flags] ne "INKEY_CMD"} return

			lassign $args index char valid
			set where [Info $oop display,what]
			if {$where eq "floor"} {
				return ; # Can't happen
			}
			set list [GetItemCommand $oop $index]
			set command [lindex $list 1]

			if {$command ne ""} {
				eval $command
			}
		}

		highlight_ok {
			if {![Info $oop choosing] && \
				([angband inkey_flags] ne "INKEY_CMD")} {
				return 0
			}
			return 1
		}

		highlight {

			if {![Info $oop choosing] && \
				[angband inkey_flags] ne "INKEY_CMD"} return

			lassign $args index char valid
			NSRecall::RecallObject [Info $oop display,what] $index
			angband [Info $oop display,what] info $index attrib
			[Info $oop icon] itemconfigure icon -assign $attrib(icon)
		}

		context_menu {

			lassign $args menu row index char valid

			set font [$menu cget -font]
			set where [Info $oop display,what]

			# No row is hit
			if {$row == -1} {

				$menu add command -label [mc Cancel]
				return 1
			}

			# Get information about this item
			angband $where info $index attrib

			# Get the item char
			set itemKey $char

			# Get the tval
			set itemTval $attrib(tval)

			# Require a real item (ie, in equipment)
			if {$attrib(tval) eq "TV_NONE"} {

				$menu add command -label [mc Cancel]
				return 1
			}

			# We are waiting for an item
			if {[angband inkey_flags] eq "INKEY_ITEM"} {

				if {$valid} {
					# Append a command to select the item
					set command "angband keypress $itemKey"
					$menu add command -label [mc "Select This Item"] \
						-command $command -font [BoldFont $font]
					$menu add separator
				}
				$menu add command -label [mc Cancel] -command {angband keypress \033}
				return 1
			}

			# We are not waiting for a command
			if {[angband inkey_flags] ne "INKEY_CMD"} {
				return 0
			}

			# Hack -- Looking at a floor stack
			if {$where eq "floor"} {

				# No commands are possible
				return 0
			}

			# If the game is waiting for a command, and the equipment is
			# displayed, then we need to know if the / toggle should be
			# fed to the Term to select an equipment item. This involves
			# seeing if the inventory is empty.
			set toggleChar ""
			if {$where eq "equipment"} {
				if {[angband inventory count]} {
					set toggleChar /
				}
			}

			# Get the default command for this item
			set list [GetItemCommand $oop $index]
			set label [lindex $list 0]
			set command [lindex $list 1]

			if {[string length $command]} {
				$menu add command -label $label \
					-command $command -font [BoldFont $font]
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
					if {!([Setting auto_destroy] && \
						[angband inventory worthless $index])} {
						append suffix y
					}
				}
}
				$menu add command -label [mc *Destroy*] -command "DoKeymapCmd $prefix k $suffix"
			}

			$menu add separator
			$menu add command -label [mc Cancel]

			return 1
		}
	}

	return
}

if {[variant KANGBANDTK ZANGBANDTK]} {

proc NSChoiceWindow::hook_cmd_pet {oop message args} {

	switch -- $message {

		open {
		}

		fresh {
			SetList $oop
		}

		close {
		}

		title {
			return [mc title-pet]
		}

		set_list {
			set textBox [Info $oop text]

			# Process each command
			foreach {char label} [NSRecall::PetCmdInfo mode] {

				if {$char eq $mode} {
					set color [Value TERM_L_BLUE]
				} else {
					set color White
				}

				# Append the character and description
				NewListItem $oop $char $label $color \
					$char
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"
		}

		invoke {
			if {![Info $oop choosing]} return
			lassign $args char
			angband keypress $char
		}

		highlight_ok {
			return 1
		}

		highlight {
		}
	}

	return
}

proc NSChoiceWindow::hook_mindcraft {oop message args} {

	switch -- $message {

		open {
		}

		fresh {
			SetList $oop
		}

		close {
		}

		title {
			return [mc title-mindcraft]
		}

		set_list {

			set textBox [Info $oop text]

			# Get the list of mindcraft powers
			set powerList [angband mindcraft get]

			# Process each power
			foreach power $powerList {

				angband mindcraft info $power attrib
				if {!$attrib(okay)} continue

				# Append the character and description
				NewListItem $oop $attrib(char) $attrib(name) $color \
					$power $char
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"
		}

		invoke {

			if {![Info $oop choosing]} return
			lassign $args power char
			angband keypress $char
		}

		highlight_ok {
			return 1
		}

		highlight {
			if {![Info $oop choosing] && \
				[angband inkey_flags] ne "INKEY_CMD"} return

			lassign $args power char
			NSRecall::RecallMindcraft $power
		}
	}

	return
}

proc NSChoiceWindow::hook_power {oop message args} {

	set powerChars "abcdefghijklmnopqrstuvwxyz0123456789"

	switch -- $message {

		open {
		}

		fresh {
			SetList $oop
		}

		close {
		}

		title {
			return [mc title-power]
		}

		set_list {

			set textBox [Info $oop text]

			# Get the list powers
			set powerList [angband power get]

			set i 0

			# Check each power
			foreach power $powerList {

				# Get information about this power
				angband power info $power attrib

				# Get the power char
				set attrib(char) [string index $powerChars $i]

				set fill White
				if {$attrib(chance) == 100} {
					set fill gray70
				}

				# Append the character and description
				NewListItem $oop $attrib(char) $attrib(name) $fill \
					$power $attrib(char)

				incr i
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"
		}

		invoke {

			if {![Info $oop choosing]} return
			lassign $args power char
			angband keypress $char
		}

		highlight_ok {
			return 1
		}

		highlight {
			if {![Info $oop choosing] && \
				[angband inkey_flags] ne "INKEY_CMD"} return

			lassign $args power char
			NSRecall::RecallPower $power
#			[Info $oop icon] itemconfigure icon -assign "icon none 0"
		}

		context_menu {

			lassign $args menu row power char

			set font [$menu cget -font]

			# No row is hit
			if {$row == -1} {

				$menu add command -label [mc Cancel] \
					-command {angband keypress \033}
				return 1
			}

			# Append a command to select the item
			set command "angband keypress $char"
			$menu add command -label "Select This Power" -command $command \
				-font [BoldFont $font]
			$menu add separator
			$menu add command -label [mc Cancel] \
				-command {angband keypress \033}

			return 1
		}
	}

	return
}

# ZANGBANDTK
}

proc NSChoiceWindow::hook_spell {oop message args} {

	switch -- $message {

		open {
		}

		fresh {
			SetList $oop
		}

		close {
		}

		title {
			return [mc title-[angband player prayer_or_spell]]
		}

		set_list {
			set textBox [Info $oop text]

			# Get player mana
			set pymana [lindex [angband player mana] 0]

			# Get the book number
			set bookNum [Info $oop display,what]

			# Get a list of legal spells
			set spellList [angband spell find $bookNum -tester yes]

			# Process each spell
			foreach spell $spellList {

				# Get information about this spell
				angband spell info $bookNum $spell attrib

				# Colorize the spell name (from OAngband)
				switch -- $attrib(info) {
					unknown {
						set fill gray70

						# The character can learn this spell
						if {[angband player new_spells] && 
							($attrib(level) <= [angband player level])} {
							set fill [Value TERM_L_GREEN]
						}
					}
					untried {
						set fill [Value TERM_L_BLUE]
					}
					default {
						set fill White

						# Not enought mana
						if {$attrib(mana) > $pymana} {
							set fill gray70
						}
					}
				}

				# Append the character and description
				NewListItem $oop $attrib(char) $attrib(name) $fill \
					$spell $attrib(char)
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"
		}

		invoke {

			if {![Info $oop choosing]} return
			lassign $args index char
			angband keypress $char
		}

		doubleclick {

			if {[angband inkey_flags] ne "INKEY_CMD"} return
			lassign $args spellNum char
			set bookNum [Info $oop display,what]
			set index [angband inventory find -limit 1 -k_idx $bookNum]
			if {![llength $index]} return
			angband inventory info $index attrib
			set charBook $attrib(char)
			angband spell info $bookNum $spellNum attrib
			set charSpell $attrib(char)
			GetMagicInfo charCmd spellType
			if {$attrib(known)} {
				DoUnderlyingCommand $charCmd$charBook$charSpell
			} else {
				if {[angband player new_spells] && ($attrib(level) <= \
					[angband player level])} {
					set command G
					if {[variant ZANGBANDTK]} {
						append command {\n}
					}
					append command $charBook
					if {$spellType eq "spell"} {
						append command $charSpell
					}
					DoUnderlyingCommand $command
				}
			}
		}

		highlight_ok {
			if {![Info $oop choosing] && \
				([angband inkey_flags] ne "INKEY_CMD")} {
				return 0
			}
			return 1
		}

		highlight {

			if {![Info $oop choosing] && \
				[angband inkey_flags] ne "INKEY_CMD"} return
			lassign $args index char
			set bookNum [Info $oop display,what]
			NSRecall::RecallSpell $bookNum $index
		}

		context_menu {

			lassign $args menu row index char

			set font [$menu cget -font]

			# No row is hit
			if {$row == -1} {

				$menu add command -label [mc Cancel] \
					-command {angband keypress \033}
				return 1
			}

			if {[angband inkey_flags] ne "INKEY_SPELL"} {
				return 0
			}

			set bookNum [Info $oop display,what]
			set spellNum $index
			angband spell info $bookNum $spellNum attrib

			GetMagicInfo charCmd spellType

			# Append a command to select the item
			set command "angband keypress $attrib(char)"
			$menu add command -label "Select This [string totitle $spellType]" \
				-command $command -font [BoldFont $font]
			$menu add separator
			$menu add command -label [mc Cancel] \
				-command {angband keypress \033}

			return 1
		}
	}

	return
}

# NSChoiceWindow::Choose --
#
#	Handle <Choose> quasi-event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::Choose {oop what show args} {

	if {[lsearch -exact [list cmd_pet ele_attack item mindcraft power spell] \
		$what] == -1} return

	Info $oop choosing $show
	Info $oop choosing,what $what

	if {!$show} {
		Info $oop display,what [Info $oop default,what]
		SetHook $oop [Info $oop default,hook]
		return
	}

	switch -- $what {
		cmd_pet {
			SetHook $oop hook_cmd_pet
		}
		ele_attack {
			SetHook $oop hook_ele_attack
		}
		item {
			Info $oop display,what [lindex $args 0]
			SetHook $oop hook_item
		}
		mindcraft {
			SetHook $oop hook_mindcraft
		}
		power {
			SetHook $oop hook_power
		}
		spell {
			Info $oop display,what [lindex $args 0]
			SetHook $oop hook_spell
		}
	}

	return
}

# NSChoiceWindow::Track --
#
#	Handle <Track> quasi-event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::Track {oop what} {

	if {[Info $oop choosing]} return

	switch -- $what {
		equipment -
		inventory {
			if {[Info $oop hook] eq "hook_item" && \
				[Info $oop display,what] eq $what} {
				qeconfigure NSChoiceWindow <Term-fresh> -active yes
			}
		}
	}

	return
}

# NSChoiceWindow::Configure --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::Configure {oop} {

	# Lottsa foolishness here
	if {[Info $oop inConfigure]} return
	Info $oop inConfigure 1

	set frame [Info $oop win].frame
	set canvas [Info $oop icon]
	set text [Info $oop text]

	# The "update idletasks" call below displays the arrow in the
	# wrong location if the window resized, so hide it
	if {[Info $oop showIcon]} {
		set bg [$canvas cget -background]
		$canvas itemconfigure arrow -fill $bg -outline $bg
	}

	set didVert 0
	set didHorz 0
	set done 0
	while {!$done} {

		set done 1
		set doVert 0
		set doHorz 0

		if {!$didVert} {
			if {![ContentVisible $text yview]} {
				set doVert 1
			}
			if {[Info $oop expanded] || [Info $oop expand]} {
				set doVert 0
			}
			if {$doVert != [Info $oop scrollbar,vert]} {
				if {$doVert} {
					grid $frame.yscroll
					set didVert 1
				} else {
					grid remove $frame.yscroll
				}
				Info $oop scrollbar,vert $doVert
				set done 0
			}
		}

		update idletasks

		if {!$didHorz} {
			if {![ContentVisible $text xview]} {
				set doHorz 1
			}
			if {[Info $oop expanded] || [Info $oop expand]} {
				set doHorz 0
			}
			if {$doHorz != [Info $oop scrollbar,horz]} {
				if {$doHorz} {
					grid $frame.xscroll
					set didHorz 1
				} else {
					grid remove $frame.xscroll
				}
				Info $oop scrollbar,horz $doHorz
				set done 0
			}
		}
	}

	if {[Info $oop showIcon]} {

		# Move the arrow into position
		scan [$canvas bbox arrow] "%s %s %s %s" left top right bottom
		set height [winfo height $text]
		$canvas move arrow 0 [expr {$height - $bottom - 4}]

		# Hide or show the arrow
		if {[Info $oop expand] && ![ContentVisible $text yview]} {
			set fill Red
			$canvas itemconfigure arrow -fill $fill -outline $fill
		}
	}

	Info $oop inConfigure 0

	return
}

# NSChoiceWindow::Toggle --
#
#	Swaps the default display between inventory and equipment.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::Toggle {oop} {

	if {[angband inkey_flags] eq "INKEY_ITEM"} {
		if {[angband inkey_other] eq "inventory"} {
			if {[angband equipment find -tester yes] eq ""} return
		} else {
			if {[angband inventory find -tester yes] eq ""} return
		}
		angband keypress "/"
		return
	}

	if {[angband inkey_flags] ne "INKEY_CMD"} return

	switch -- [Info $oop display,what] {
		"equipment" {
			Info $oop display,what inventory
			Info $oop default,what inventory
			Info $oop default,hook hook_item
		}
		"inventory" {
			Info $oop display,what equipment
			Info $oop default,what equipment
			Info $oop default,hook hook_item
		}
		"floor" {
			# TODO: display & invoke commands on items on floor
			Info $oop display,what equipment
			Info $oop default,what equipment
			Info $oop default,hook hook_item
		}
	}
	CallHook $oop fresh

	ExpandAgain $oop

	return -code break
}

# NSChoiceWindow::ExpandAgain --
#
#	Resize the already-expanded window if needed.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::ExpandAgain {oop} {

	# Resize the window if needed
	if {[Info $oop expanded]} {

		set win [Info $oop win]
		set textBox [Info $oop text]

		set textHeight [winfo height $textBox]
		set lineHeight [font metrics [Value font,choice] -linespace]
		scan [$textBox index end] %d.%d numRows char
		set height [expr {$lineHeight * ($numRows - 1) + 4 + 2}]

		# Don't make it too small
		scan [Info $oop geometry] {%dx%d%[+-]%d%[+-]%d} width2 height2 xs x ys y
		if {$height < $height2} {
			set height $height2
		}

		set winHeight [winfo height $win]
		set winWidth [winfo width $win]

		if {$height > $winHeight} {

			Info $oop busy 1

			set y [expr {[NSToplevel::FrameTop $win] - ($height - $winHeight)}]
			wm geometry $win ${winWidth}x$height+[NSToplevel::FrameLeft $win]+$y
			update

			Info $oop busy 0
		}
	} elseif {[HasCursor $oop]} {
		Expand $oop
	}

	return
}

# NSChoiceWindow::OptionChanged --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::OptionChanged {oop info keyword} {

	set setting [Info $oop $info]
	Value choicewindow,$keyword $setting
	switch -- $keyword {
		autoexpand {
			if {[Info $oop expanded]} {
				if {!$setting} {
					Contract $oop
				}
			} elseif {[HasCursor $oop]} {
				if {$setting} {
					Expand $oop
				}
			}
			Configure $oop
		}
		showicon {
			if {$setting} {
				grid [Info $oop icon]
			} else {
				grid remove [Info $oop icon]
			}
		}
	}

	return
}

# NSChoiceWindow::ValueChanged_font_choice --
#
#	Called when the font,choice value changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::ValueChanged_font_choice {oop} {

	[Info $oop text] configure -font [Value font,choice]

	return
}

# NSChoiceWindow::IconCfg --
#
#	Called when the game's icon configuration changes
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::IconCfg {oop} {

	set canvas [Info $oop icon]

	set iconWidth [expr {[icon width] + 8}]
	$canvas configure -width $iconWidth

	set x [expr {$iconWidth / 2}]
	$canvas coords arrow [expr {$x - 3}] 46 [expr {$x + 3}] 46 $x 49

	Configure $oop

	return
}
