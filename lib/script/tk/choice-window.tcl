# File: choice-window.tcl

# Purpose: the Choice Window and related commands

#
# Copyright (c) 1997-2001 Tim Baker
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

	# Update ourself when the list highlight color changes
	Info $oop clientId,listHilite \
		[NSValueManager::AddClient listHilite \
		"$win.frame.text tag configure HOT \
		-background \[Value listHilite]"]

	# Update ourself when the font changes
	Info $oop clientId,font,choice \
		[NSValueManager::AddClient font,choice \
			"NSChoiceWindow::ValueChanged_font_choice $oop"]

	Info $oop hook ""
	Info $oop current ""
	Info $oop choosing 0
	Info $oop choosing,what ""
#	Info $oop default,hook hook_item
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

	NSValueManager::RemoveClient font,choice [Info $oop clientId,font,choice]
	NSValueManager::RemoveClient listHilite [Info $oop clientId,listHilite]

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
	wm title $win "Choice"

	wm transient $win [Window main]

	# Feed the Term when keys are pressed
	Term_KeyPress_Bind $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSChoiceWindow::Close $oop"

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Set instance variables
	Info $oop win $win

	set frame $win.frame
	frame $frame -relief sunken -borderwidth 1 -background Black

	# Canvas to display icon
	set iconSize [expr {[icon size] + 8}]
	set canvas $frame.icon
	canvas $canvas \
		-borderwidth 0 -width $iconSize -height $iconSize -background Black \
		-highlightthickness 0
if 0 {
	$canvas create widget \
		6 6 -tags icon
}
	# Create an arrow which appears when there is content out of site
	set x [expr {$iconSize / 2}]
	$canvas create polygon [expr {$x - 3}] 46 [expr {$x + 3}] 46 \
		$x 49 -fill Red -outline Red -tags arrow

	text $frame.text \
		-wrap none -width 50 -height 10 -font [Value font,choice] \
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

	$frame.text tag bind HOT <ButtonPress-1> \
		"NSChoiceWindow::Invoke $oop \[$frame.text index {@%x,%y linestart}]"
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
		"NSChoiceWindow::Toggle $oop"

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
				#set hook hook_item
			}
			SetHook $oop $hook
			CallHook $oop fresh
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
	set spacing 0

	set left 0
	set top 0
	set right [winfo screenwidth .]
	set bottom [winfo screenheight .]

	set x [expr {[NSToplevel::FrameLeft $winMain] +
		[NSToplevel::TotalWidth $winMain] / 2}]
	set width [NSToplevel::ContentWidth $win \
		[expr {[NSToplevel::TotalWidth $winMain] / 2}]]

	set y [expr {[NSToplevel::FrameBottom $winMain] + $spacing}]
	if {$bottom - $y < 100} {
		set y [expr {$bottom - 100}]
		set height [NSToplevel::ContentHeight $win 100]
	} elseif {($y + [NSToplevel::TotalHeight $win]) < $bottom} {
		set height [winfo height $win]
	} else {
		set height [expr {$bottom - [NSToplevel::FrameBottom $winMain]}]
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

	if {[string length $hook]} {
		Info $oop hook $hook
		CallHook $oop open
	} elseif {[string length [Info $oop hook]]} {
		Info $oop hook ""
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

	# Call the hook to set the list
	CallHook $oop set_list

	# Something is displayed
	Info $oop display something

	# No item is highlighted
	Info $oop current ""	

	# Synch the scrollbars
	Configure $oop

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

	set textBox [Info $oop text]
set index [Info $oop current]
	set row [expr {[lindex [split $index .] 0] - 1}]

	CallHook $oop invoke $row

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

	CallHook $oop doubleclick $row

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
	if {![string length [Info $oop hook]]} return

	# No tracking while menu is up
	if {[Info $oop busy]} return

	# See if the item has changed
	if {$index == [Info $oop current]} return

	# An item is highlighted
	if {[string length [Info $oop current]]} {

		# Remove highlighting
		UnhighlightItem $oop [Info $oop current]
	}

	# An item is under the pointer
	if {[string length $index]} {

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
	CallHook $oop highlight $row

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


# NSChoiceWindow::GetItemCommand --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSChoiceWindow::GetItemCommand {oop index  _command _label} {

	upvar $_command command $_label label

	set where [Info $oop display,what]

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

	# If this item can be sold, set the default action to "Sell"
	if {[angband store shopping] && [string equal $where inventory]} {
		set match [angband inventory find -store_will_buy yes]
		if {[lsearch -exact $match $index] != -1} {
			set charCmd s
			if {[angband store ishome]} {
				set label "Drop"
			} else {
				set label "Sell"
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

	set pointerx [winfo pointerx .]
	set pointery [winfo pointery .]
	set window [winfo containing $pointerx $pointery]
	if {![string length $window]} {
		return 0
	}
	if {[string compare [winfo toplevel $window] [Info $oop win]]} {
		return 0
	}
	return 1
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
	set bottomDist [expr {[winfo screenheight $win] - $bottom}]
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

	raise $win
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
	if {![Info $oop expanded]} return

	Info $oop busy 1
	Info $oop expanded 0

	set win [Info $oop win]
	wm geometry $win [Info $oop geometry]
	update

	Info $oop busy 0

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
	if {[CallHook $oop context_menu $menu $row]} {

		# Hack -- Try to prevent collapsing while popup is visible.
		# It would be nice if "winfo ismapped $menu" worked
		Info $oop busy 1

		# Pop up the menu
		tk_popup $menu $x $y

		if {[Platform unix]} {
			tkwait variable ::tkPriv(popup)
		}

#		after idle NSChoiceWindow::Info $oop busy 0
		Info $oop busy 0

		set index ""
		if {[NSUtils::HasCursor $text]} {
			set x [expr {[winfo pointerx $text] - [winfo rootx $text]}]
			set y [expr {[winfo pointery $text] - [winfo rooty $text]}]
			set index2 [$text index @$x,$y]
			foreach tag [$text tag names $index2] {
				if {[string equal $tag TEXT]} {
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

	$menu add command -label "Set Font" \
		-command "NSModule::LoadIfNeeded NSFont ; NSWindowManager::Display font choice"
	$menu add checkbutton -label "Auto Expand" \
		-variable NSChoiceWindow($oop,expand) \
		-command "NSChoiceWindow::OptionChanged $oop expand autoexpand"
	$menu add checkbutton -label "Show Icon" \
		-variable NSChoiceWindow($oop,showIcon) \
		-command "NSChoiceWindow::OptionChanged $oop showIcon showicon"
	$menu add separator
	$menu add command -label "Cancel"

	# Hack -- Try to prevent collapsing while popup is visible.
	# It would be nice if "winfo ismapped $menu" worked
	Info $oop busy 1

	# Pop up the menu
	tk_popup $menu $x $y

	if {[Platform unix]} {
		tkwait variable ::tkPriv(popup)
	}

#	after idle NSChoiceWindow::Info $oop busy 0
	Info $oop busy 0

	set index ""
	if {[NSUtils::HasCursor $text]} {
		set x [expr {[winfo pointerx $text] - [winfo rootx $text]}]
		set y [expr {[winfo pointery $text] - [winfo rooty $text]}]
		set index2 [$text index @$x,$y]
		foreach tag [$text tag names $index2] {
			if {[string equal $tag TEXT]} {
				set index "$index2 linestart"
				break
			}
		}
	}
	Motion $oop $index

	return
}


proc NSChoiceWindow::hook_cmd_pet {oop message args} {

	switch -- $message {

		open {
		}

		fresh {
			SetList $oop
		}

		close {
		}

		set_list {
			set textBox [Info $oop text]

			# Keep a list of invoke chars
			set match {}
			
			# Process each command
			foreach {char label} [NSRecall::PetCmdInfo mode] {

				if {[string equal $char $mode]} {
					set color [Value TERM_L_BLUE]
				} else {
					set color White
				}

				# Append the character and description
				$textBox insert end "$char\) " TEXT $label \
					[list ITEM_$char TEXT] "\n"
				$textBox tag configure ITEM_$char -foreground $color

				# Keep a list of chars and colors
				lappend match $char
				lappend colors $color
			}
		
			# Delete trailing newline
			$textBox delete "end - 1 chars"
		
			# Keep a list of chars and colors
			Info $oop match $match
			Info $oop color $colors
		}

		get_color {
			set row [lindex $args 0]
			return [lindex [Info $oop color] $row]
		}

		invoke {
			if {![Info $oop choosing]} return
			set row [lindex $args 0]
			set char [lindex [Info $oop match] $row]
			angband keypress $char
		}

		highlight {
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

	if {[lsearch -exact [list cmd_pet ele_attack item] \
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
			if {[string compare [$text yview] "0 1"]} {
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
			if {[string compare [$text xview] "0 1"]} {
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
		if {[Info $oop expand] && [string compare [$text yview] "0 1"]} {
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

	if {[string compare [angband inkey_flags] INKEY_CMD]} return

	if {[string equal [Info $oop display,what] inventory]} {
		Info $oop display,what equipment
		Info $oop default,what equipment
		Info $oop default,hook hook_item
	} else {
		Info $oop display,what inventory
		Info $oop default,what inventory
		Info $oop default,hook hook_item
	}
	CallHook $oop fresh

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
	
		if {$height != $winHeight} {
	
			Info $oop busy 1
		
			set y [expr {[winfo y $win] - ($height - $winHeight)}]
			wm geometry $win ${winWidth}x$height+[winfo x $win]+$y
			update
		
			Info $oop busy 0
		}
	}

	return -code break
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

	set text [Info $oop text]
	$text configure -font [Value font,choice]

	return
}
