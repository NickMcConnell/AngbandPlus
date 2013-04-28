# File: misc-toolbar.tcl

# Purpose: the Misc Window toolbar

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSMiscToolbar {

	variable Priv

# namespace eval NSMiscToolbar
}

# NSMiscToolbar::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::InitModule {} {

	MsgCatInit inven misc-win

	InitImageIfNeeded Image_ButtonActivate button-activate.gif
	InitImageIfNeeded Image_ButtonFood button-food.gif
	InitImageIfNeeded Image_ButtonPotion button-potion.gif
	InitImageIfNeeded Image_ButtonScroll button-scroll.gif
	InitImageIfNeeded Image_ButtonRod button-rod.gif
	InitImageIfNeeded Image_ButtonWand button-wand.gif
	InitImageIfNeeded Image_ButtonStaff button-staff.gif
	InitImageIfNeeded Image_ButtonDown button-down.gif
	InitImageIfNeeded Image_ButtonUp button-up.gif

	return
}

# NSMiscToolbar::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::CloseModule {} {

	return
}

# NSMiscToolbar::NSMiscToolbar --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMiscToolbar::NSMiscToolbar {oop args} {

	array set opts {
		-parent ""
	}
	array set opts $args

	Info $oop hook ""
	Info $oop current ""
	Info $oop busy 0

	Info $oop win,visible 0
	Info $oop whoHasCursor ""
	Info $oop after ""

	Info $oop nextButton 0

	InitWindow $oop $opts(-parent)

	set win [Info $oop win]

	# Update ourself when the list highlight color changes
	qebind NSMiscToolbar <Value-listHilite> \
		"[Info $oop text] tag configure HOT -background \[Value listHilite]"

	# Update ourself when the font changes
	qebind NSMiscToolbar <Value-font,miscPopup> \
		"NSMiscToolbar::ValueChanged_font_miscPopup $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSMiscToolbar $oop $win

	return
}

# NSMiscToolbar::~NSMiscToolbar --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::~NSMiscToolbar {oop} {

	return
}

# NSMiscToolbar::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::Info {oop info args} {

	global NSMiscToolbar

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSMiscToolbar($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSMiscToolbar($oop,$info)
			}
		}
	}

	return
}

# NSMiscToolbar::InitWindow --
#
#	Create the window.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSMiscToolbar::InitWindow {oop parent} {

	set canvas $parent.miscPopupCanvas$oop
	canvas $canvas -width [expr {(16 + 4) * 9}] -height 22 \
		-scrollregion {0 0 0 0} -highlightthickness 0 -background Black

	Info $oop canvas $canvas

	#
	# Buttons
	#

	set x 1
	NewButton $oop -image Image_ButtonActivate -popup 1 -hook item \
		-args [list equipment A -activate yes] \
		-message [mc "Activate an equipment item"]
	NewButton $oop -image Image_ButtonFood -popup 1 -hook item \
		-args [list inventory E -tval TV_FOOD] \
		-message [mc "Eat an inventory item"]
	NewButton $oop -image Image_ButtonPotion -popup 1 -hook item \
		-args [list inventory q -tval TV_POTION] \
		-message [mc "Drink a potion in inventory"]
	NewButton $oop -image Image_ButtonScroll -popup 1 -hook item \
		-args [list inventory r -tval TV_SCROLL] \
		-message [mc "Read a scroll in inventory"]
	NewButton $oop -image Image_ButtonRod -popup 1 -hook item \
		-args [list inventory z -tval TV_ROD] \
		-message [mc "Zap a rod in inventory"]
	NewButton $oop -image Image_ButtonWand -popup 1 -hook item \
		-args [list inventory a -tval TV_WAND] \
		-message [mc "Aim a wand in inventory"]
	NewButton $oop -image Image_ButtonStaff -popup 1 -hook item \
		-args [list inventory u -tval TV_STAFF] \
		-message [mc "Use a staff in inventory"]

	NewButton $oop -image Image_ButtonUp -popup 0 \
		-command "DoUnderlyingCommand <" \
		-message [mc "Go up a dungeon level"]
	NewButton $oop -image Image_ButtonDown -popup 0 \
		-command "DoUnderlyingCommand >" \
		-message [mc "Go down a dungeon level"]

	#
	# Popup window of choices
	#

	set win $canvas.miscPopupWin$oop
	toplevel $win -borderwidth 1 -relief flat -background gray60

	# Start out withdrawn (hidden)
	wm withdraw $win

	wm overrideredirect $win yes
	wm transient $win [Window misc]

	if {[Platform unix]} {
		$win configure -cursor arrow
	}

	# Set instance variables
	Info $oop win $win

	set wText $win.text
	text $wText \
		-wrap none -font [Value font,miscPopup] \
		-borderwidth 0 -setgrid no -highlightthickness 0 \
		-padx 4 -pady 2 -background Black -foreground White -cursor ""

	# Bypass default Text bindings
	bindtags $wText [list $wText $win all]

	pack $wText \
		-expand yes -fill both

	Info $oop text $wText

	# Fiddle with the selection for list behaviour
	$wText tag configure HOT -foreground White \
		-background [Value listHilite]

	$wText tag bind HOT <ButtonPress-1> \
		"NSMiscToolbar::Invoke $oop"
	$wText tag bind TEXT <Motion> \
		"NSMiscToolbar::Motion $oop \[$wText index {@%x,%y linestart}]"
	$wText tag bind HOT <Leave> \
		"NSMiscToolbar::Motion $oop {}"

	bind $win <Enter> \
		"NSMiscToolbar::Event $oop enter-win"
	bind $win <Leave> "
		NSMiscToolbar::Motion $oop {}
		NSMiscToolbar::Event $oop leave-win
	"

	return
}

# NSMiscToolbar::NewButton --
#
#	Add a new button.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::NewButton {oop args} {

	set canvas [Info $oop canvas]

	# Get the next unique id for this button
	set num [incr ::NSMiscToolbar($oop,nextButton)]

	set config(-command) ""
	array set config $args

	set image $config(-image)
	set command $config(-command)
	set message $config(-message)

	# The buttons are positioned in ShowBar().
	set x [expr {1 + ($num - 1) * 20}]
	set y 2

	Info $oop button,x,$num $x

	# Focus rectangle
	$canvas create rectangle $x $y [expr {$x + 17}] [expr {$y + 17}] \
		-tags [list button button$num border$num]

	# Image
	$canvas create image [expr {$x + 1}] [expr {$y + 1}] -image $image \
		-anchor nw -tags "button button$num img$num"

	# Show popup on mouse-over
	if {![string length $command]} {

		$canvas bind img$num <Enter> "
			$canvas itemconfigure border$num -outline gray60
			NSMainWindow::StatusText $oop [list $message]
			NSMiscToolbar::EnterButton $oop $num
		"

		Info $oop button,hook,$num $config(-hook)
		Info $oop button,args,$num $config(-args)

	# Click to invoke command
	} else {
		$canvas bind img$num <Enter> "
			$canvas itemconfigure border$num -outline gray60
			NSMainWindow::StatusText $oop [list $message]
			NSMiscToolbar::Event $oop enter-button2
		"
		$canvas bind img$num <ButtonPress-1> "
			$canvas move button$num 1 1
			$command
		"
		$canvas bind img$num <ButtonRelease-1> \
			"$canvas move button$num -1 -1"

		Info $oop button,hook,$num ""
	}

	$canvas bind img$num <Leave> "
		$canvas itemconfigure border$num -outline Black
		NSMiscToolbar::Event $oop leave-button
		NSMainWindow::StatusText $oop {}
	"

	return
}

# NSMiscToolbar::SetHook --
#
#	Set the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::SetHook {oop hook} {

	if {[string length $hook]} {
		Info $oop hook $hook
		CallHook $oop open
	} elseif {[string length [Info $oop hook]]} {
		Info $oop hook ""
	}

	return
}

# NSMiscToolbar::CallHook --
#
#	Call the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::CallHook {oop message args} {

	return [uplevel #0 NSMiscToolbar::[Info $oop hook] $oop $message $args]
}

# NSMiscToolbar::NewListItem --
#
#	Create a new list item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::NewListItem {oop char string color args} {

	set wText [Info $oop text]

	$wText insert end "$char\) " TEXT $string \
		[list TEXT_$char TEXT] "\n" TEXT
	$wText tag configure TEXT_$char -foreground $color

	lappend ::NSMiscToolbar($oop,match) $args

	return
}

# NSMiscToolbar::EnterButton --
#
#	Display popup of choices.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::EnterButton {oop buttonNum} {

	set win [Info $oop win]
	set canvas [Info $oop canvas]
	set wText [Info $oop text]

	if {[lsearch -exact [angband inkey_flags] INKEY_CMD] == -1} return

	# Set the hook for this button
	set hookArgs [Info $oop button,args,$buttonNum]
	SetHook $oop hook_[Info $oop button,hook,$buttonNum]

	# See if there are any valid choices
	if {![CallHook $oop has_cmd {*}$hookArgs]} {
		HideWin $oop
		return
	}

	# Set the list
	$wText delete 1.0 end
	Info $oop match {}
	CallHook $oop set_list {*}$hookArgs

	set x [Info $oop button,x,$buttonNum]
	incr x [expr {[winfo rootx $canvas] + 9}]
	set y [expr {[winfo rooty $canvas] + [winfo height $canvas]}]

	set width 0
	for {set i 1} {$i <= [$wText count -lines 1.0 end]} {incr i} {
		set lineWidth [$wText count -update -xpixels $i.0 "$i.0 lineend"]
		if {$lineWidth > $width} {
			set width $lineWidth
		}
	}
	incr width [expr {[$wText cget -padx] * 2}]

	set height [$wText count -update -ypixels 1.0 end]
	incr height [expr {[$wText cget -pady] * 2}]

	incr width [expr {[$win cget -borderwidth] * 2}]
	incr height [expr {[$win cget -borderwidth] * 2}]

	# x is middle
	incr x [expr {0 - $width / 2}]
	if {$x < [winfo rootx $canvas]} {
		set x [winfo rootx $canvas]
	}

	set screenWidth [ScreenWidth]
	if {$x + $width > $screenWidth} {
		incr x [expr {$screenWidth - ($x + $width)}]
	}
	set screenHeight [ScreenHeight]
	if {$y + $height > $screenHeight} {
		incr y [expr {$screenHeight - ($y + $height)}]
	}

	wm geometry $win ${width}x${height}+${x}+$y

	# Perhaps show the window later
	Event $oop enter-button

	return
}

# NSMiscToolbar::Event --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::Event {oop event} {

	set who [Info $oop whoHasCursor]

	switch -- $event {
		enter-win {
			set who win
		}
		leave-win {
			set who ""
		}
		enter-button {
			set who button
		}
		enter-button2 {
			set who button2
		}
		leave-button {
			set who ""
		}
	}

	Info $oop whoHasCursor $who
	if {[string match enter-* $event]} {
		set delay 10
	} else {
		set delay 200
	}
	after cancel [Info $oop after]
	Info $oop after [after $delay NSMiscToolbar::CheckWhoHasCursor $oop]

	return
}

# NSMiscToolbar::CheckWhoHasCursor --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::CheckWhoHasCursor {oop} {

	set who [Info $oop whoHasCursor]

	switch -- $who {
		button {
			ShowWin $oop
		}
		button2 {
			HideWin $oop
		}
		win {
		}
		default {
			HideWin $oop
		}
	}

	Info $oop after ""

	return
}

# NSMiscToolbar::ShowWin --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::ShowWin {oop} {

	if {[Info $oop win,visible]} return

	set win [Info $oop win]

	wm deiconify $win
#	if {[Platform unix]} \{
		raise $win
#	\}

	Info $oop win,visible 1

	return
}

# NSMiscToolbar::HideWin --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::HideWin {oop} {

	if {![Info $oop win,visible]} return

	set win [Info $oop win]

	wm withdraw $win

	Info $oop win,visible 0

	return
}

# NSMiscToolbar::Invoke --
#
#	Called when a list item is clicked. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::Invoke {oop} {

	set textBox [Info $oop text]
	set index [Info $oop current]
	set row [expr {[lindex [split $index .] 0] - 1}]

	HideWin $oop

	CallHook $oop invoke {*}[lindex [Info $oop match] $row]

	return
}

# NSMiscToolbar::Motion --
#
#	Called when the mouse moves in a list item. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::Motion {oop index} {

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

# NSMiscToolbar::HighlightItem --
#
#	Highlights a list item. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::HighlightItem {oop index} {

	set textBox [Info $oop text]
	set row [expr {[lindex [split $index .] 0] - 1}]

	# Highlight the item
	$textBox tag add HOT $index "$index lineend + 1 chars"
	$textBox tag raise HOT

	# Call the hook (to set the icon, for example)
	CallHook $oop highlight {*}[lindex [Info $oop match] $row]

	return
}

# NSMiscToolbar::UnhighlightItem --
#
#	Removes highlighting from a list item. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::UnhighlightItem {oop index} {

	set win [Info $oop win]
	set textBox [Info $oop text]

	# Unhighlight the item
	$textBox tag remove HOT 1.0 end

	return
}

# NSMiscToolbar::HasCursor --
#
#	See if the cursor is over the window. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::HasCursor {oop} {

	return [NSUtils::HasCursor [Info $oop win] 1]
}

if {[variant OANGBANDTK]} {

proc NSMiscToolbar::hook_ele_attack {oop message args} {

	switch -- $message {

		open {
		}

		close {
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
			$textBox tag add TEXT "end - 1 chars"
		}

		invoke {

			if {![Info $oop choosing]} return
			lassign $args char
			angband keypress $char
		}

		highlight {
		}
	}

	return
}

# OANGBANDTK
}

proc NSMiscToolbar::hook_item {oop message args} {

	switch -- $message {

		open {
		}

		close {
		}

		has_cmd {
			set where [lindex $args 0]
			set char [lindex $args 1]
			set other [lrange $args 2 end]
			set itemList [angband $where find {*}$other]
			return [llength $itemList]
		}

		set_list {

			set where [lindex $args 0]
			set char [lindex $args 1]
			set other [lrange $args 2 end]

			Info $oop hook,where $where
			Info $oop hook,char $char

			set textBox [Info $oop text]

			# Get the list of matching item indexes
			set itemList [angband $where find {*}$other]

			# Process each item
			foreach index $itemList {

				# Get item info
				angband $where info $index attrib

				if {$where eq "floor"} {
					set attrib(char) [string index "abcdefghijklmnopqrstuvw" \
						[lsearch -integer $itemList $index]]
				}

				# Get the color
				set color [default_tval_to_attr $attrib(tval)]

				# Append the character and description
				NewListItem $oop $attrib(char) $attrib(name) $color \
					$index $attrib(char)
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"
			$textBox tag add TEXT "end - 1 chars"
		}

		invoke {
			lassign $args index char
			set where [Info $oop hook,where]
			set charCmd [Info $oop hook,char]
			DoUnderlyingCommand $charCmd$char
		}

		highlight {
			lassign $args index char
			NSRecall::RecallObject [Info $oop hook,where] $index
		}
	}

	return
}

if {[variant KANGBANDTK ZANGBANDTK]} {

proc NSMiscToolbar::hook_cmd_pet {oop message args} {

	switch -- $message {

		open {
		}

		close {
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
			$textBox tag add TEXT "end - 1 chars"
		}

		invoke {
			if {![Info $oop choosing]} return
			lassign $args char
			angband keypress $char
		}

		highlight {
		}
	}

	return
}

proc NSMiscToolbar::hook_mindcraft {oop message args} {

	switch -- $message {

		open {
		}

		close {
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
				NewListItem $oop $attrib(char) $attrib(name) White \
					$power $attrib(char)
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"
			$textBox tag add TEXT "end - 1 chars"
		}

		invoke {

			if {![Info $oop choosing]} return
			lassign $args power char
			angband keypress $char
		}

		highlight {
		}
	}

	return
}

proc NSMiscToolbar::hook_power {oop message args} {

	set powerChars "abcdefghijklmnopqrstuvwxyz0123456789"

	switch -- $message {

		open {
		}

		close {
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
			$textBox tag add TEXT "end - 1 chars"
		}

		invoke {

			if {![Info $oop choosing]} return
			lassign $args power char
			angband keypress $char
		}

		highlight {
		}
	}

	return
}

# ZANGBANDTK
}

proc NSMiscToolbar::hook_spell {oop message args} {

	switch -- $message {

		open {
		}

		close {
		}

		set_list {
			set textBox [Info $oop text]

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
					}
				}

				# Append the character and description
				NewListItem $oop $attrib(char) $attrib(name) $fill \
					$spell $attrib(char)
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"
			$textBox tag add TEXT "end - 1 chars"
		}

		invoke {

			if {![Info $oop choosing]} return
			lassign $args index char
			angband keypress $char
		}

		highlight {

			if {![Info $oop choosing] && \
				[angband inkey_flags] ne "INKEY_CMD"} return
			lassign $args index char
			set bookNum [Info $oop display,what]
			NSRecall::RecallSpell $bookNum $index
		}
	}

	return
}

# NSMiscToolbar::OptionChanged --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::OptionChanged {oop info keyword} {

	set setting [Info $oop $info]
	Value choicewindow,$keyword $setting
	switch -- $keyword {
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

# NSMiscToolbar::ValueChanged_font_miscPopup --
#
#	Called when the font,miscPopup value changes.
#	Updates the Recall Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscToolbar::ValueChanged_font_miscPopup {oop} {

	set text [Info $oop text]
	$text configure -font [Value font,miscPopup]

	return
}
