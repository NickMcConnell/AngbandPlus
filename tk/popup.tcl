# File: popup.tcl

# Purpose: the Popup Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSPopup {

	variable Priv

# namespace eval NSPopup
}

# NSPopup::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPopup::InitModule {} {

	MsgCatInit

	# Create the Popup Window
	NSObject::New NSPopup

	return
}

# NSPopup::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPopup::CloseModule {} {

	catch {
		destroy [Global popup,win]
	}

	return
}

# NSPopup::NSPopup --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPopup::NSPopup {oop} {

	InitWindow $oop

	set win [Info $oop win]

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSPopup $oop $win

	Global popup,oop $oop
	Global popup,win $win

	return
}

# NSPopup::~NSPopup --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPopup::~NSPopup {oop} {

	return
}

# NSPopup::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPopup::Info {oop info args} {

	global NSPopup

	# Verify the object
	NSObject::CheckObject NSPopup $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSPopup($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSPopup($oop,$info)
			}
		}
	}

	return
}

# NSPopup::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPopup::InitWindow {oop} {

	set win .popup$oop
	toplevel $win

	wm overrideredirect $win yes
	$win configure -borderwidth 1 -relief flat -background gray60
	wm resizable $win no no

	# Start out withdrawn (hidden)
	wm withdraw $win

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

	$tree notify bind $tree <Selection> "NSPopup::SelectionChanged $oop"

	bind $tree <Motion> "NSPopup::Motion $oop %x %y"
	bind $tree <Leave> "NSPopup::Leave $oop"

	bind $tree <Control-ButtonPress-1> { ; }
	bind $tree <Shift-ButtonPress-1> { ; }
	bind $tree <ButtonPress-1> "NSPopup::Button1 $oop %x %y"
	bind $tree <ButtonRelease-3> "NSPopup::Button1 $oop %x %y"

	bind $win <KeyPress-Escape> "angband keypress \033 ; set ::NSPopup($oop,result) 1"

	pack $tree -expand yes -fill both -padx 2 -pady 2
	pack $frame -expand yes -fill both

	Info $oop win $win
	Info $oop tree $tree

	return
}

# NSPopup::SelectionChanged --
#
#	Handle <Selection>.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPopup::SelectionChanged {oop} {

	set tree [Info $oop tree]

	if {[$tree selection count] == 1} {
		set item [$tree selection get]
		if {[$tree item tag expr $item hook]} {
			CallHook $oop select {*}[Info $oop match,$item]
		}
	}

	return
}

# NSPopup::Motion --
#
#	Handle <Motion>.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPopup::Motion {oop x y} {

	set tree [Info $oop tree]

	set id [$tree identify $x $y]
	if {$id eq "" || [lindex $id 0] ne "item"} {
		$tree selection clear
		return
	}
	set item [lindex $id 1]
	$tree selection modify $item all

	return
}

# NSPopup::Leave --
#
#	Handle <Leave>.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPopup::Leave {oop} {

	set tree [Info $oop tree]
	$tree selection clear

	return
}

# NSPopup::Button1 --
#
#	Handle <ButtonPress-1>.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPopup::Button1 {oop x y} {

	set tree [Info $oop tree]

	# Pressing and holding Button-3, popping up, then letting go selects 
	# an item, so wait a bit if it was a quick press-release
	if {[clock milliseconds] < [Info $oop invokeDelay]} return

	set id [$tree identify $x $y]
	if {$id eq "" || [lindex $id 0] ne "item"} {
		return
	}
	set item [lindex $id 1]
	if {[$tree item tag expr $item hook]} {
		CallHook $oop invoke {*}[Info $oop match,$item]
		set ::NSPopup($oop,result) 1
	} elseif {[$tree item enabled $item]} {
		angband keypress \033
		set ::NSPopup($oop,result) 1
	}

	return
}

# NSPopup::CallHook --
#
#	Call the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPopup::CallHook {oop message args} {

	return [uplevel #0 NSPopup::hook_[Info $oop hook] $oop $message $args]
}

# NSPopup::Select --
#
#	Display the window and wait for a choice.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPopup::Select {oop what parent x y} {

	set win [Info $oop win]
	set tree [Info $oop tree]

	$tree item delete all

	# Sync colors/fonts here
	$win.frame configure -background [Value listBG]
	$tree configure -background [Value listBG] -font [Value font,knowledge]

	set fill [list [Value listHilite] {selected focus} [Value listInactive] {selected}]
	$tree element configure eSel -fill $fill

	# Set the list then append separator and "Cancel"
	Info $oop hook $what
	CallHook $oop set_list
	NewSeparator $oop
	set item [$tree item create -parent root]
	$tree item style set $item COL sCancel

	# Set the size of the list to the size of all the items
	scan [$tree item bbox end] "%d %d %d %d" left top right bottom
	$tree configure -width $right -height $bottom
	update idletasks

	# Display with the hook-items centered under the given x,y
	scan [$tree item bbox "last tag hook"] "%d %d %d %d" left top right bottom
	incr x [expr {-[winfo reqwidth $win] / 2}]
	incr y [expr {-$bottom / 2}]
	wm geometry $win +$x+$y

	wm transient $win $parent
	wm deiconify $win
	raise $win

	# Select the item under the given x,y (hopefully the mouse pointer x,y)
	update idletasks
	set pointerx [winfo pointerx $win]
	set pointery [winfo pointery $win]
	set wx [expr {$pointerx - [winfo rootx $tree]}]
	set wy [expr {$pointery - [winfo rooty $tree]}]
	Motion $oop $wx $wy

	# Pressing and holding Button-3, popping up, then letting go selects 
	# an item, so wait a bit if it was a quick press-release
	Info $oop invokeDelay [expr {[clock milliseconds] + 200}]

	# Set a grab on the window and claim the focus
	NSUtils::GrabSave $win
	focus $tree

	KeepOnTop $oop
#	set afterId [after 20000 "set ::NSPopup($oop,result) 1 ; NSUtils::GrabRelease $win"] ; # Paranoia

	set ::NSPopup($oop,result) 0
	vwait ::NSPopup($oop,result)

	# Release grab
	NSUtils::GrabRelease $win

	after cancel [Info $oop keepOnTop]
#	after cancel $afterId
	
	wm withdraw $win

	return
}

# NSPopup::NewItem --
#
#	Create a new item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPopup::NewItem {oop char string color args} {

	set tree [Info $oop tree]
	set item [$tree item create -parent root -tags hook]
	$tree item element configure $item COL eChar -text "$char) "
	$tree item element configure $item COL eTxt -text $string -fill $color

	Info $oop match,$item $args

	return $item
}

# NSPopup::NewSeparator --
#
#	Create a new separator item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPopup::NewSeparator {oop} {

	set tree [Info $oop tree]
	set item [$tree item create -parent root -height 6]
	$tree item enabled $item no
	$tree item style set $item COL sSeparator

	return $item
}

# NSPopup::KeepOnTop --
#
#	[after] callback to keep the grabbed overrideredirect window on top.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSPopup::KeepOnTop {oop} {

	raise [Info $oop win]
	Info $oop keepOnTop [after 200 "NSPopup::KeepOnTop $oop"]

	return
}

proc NSPopup::hook_item {oop message args} {

	switch -- $message {

		set_list {

			# Hack -- Turn off show_flavors
			set setting [Setting show_flavors]
			Setting show_flavors no

			set invOrEquip [angband inkey_other]

			set n 0
			foreach item [angband $invOrEquip find -tester yes] {
				angband $invOrEquip info $item attrib
				if {$invOrEquip eq "floor"} {
					set attrib(char) [string index "abcdefghijklmnopqrstuvw" $n]
				}
				set char $attrib(char)
				set color [default_tval_to_attr $attrib(tval)]
				NSPopup::NewItem $oop $char $attrib(name) $color \
					$invOrEquip $item $char
				incr n
			}

			# Hack -- Restore show_flavors
			Setting show_flavors $setting
		}

		select {
			lassign $args invOrEquip index
			NSRecall::RecallObject $invOrEquip $index
		}

		invoke {
			lassign $args invOrEquip index char
			angband keypress $char
		}
	}

	return
}
proc NSPopup::SelectItem {parent x y} {
	NSModule::LoadIfNeeded NSPopup
	return [NSPopup::Select [Global popup,oop] item $parent $x $y]
}

proc NSPopup::hook_spell {oop message args} {

	switch -- $message {

		set_list {

			set bookNum [angband inkey_other]

			# Check each legal spell
			foreach spell [angband spell find $bookNum -tester yes] {

				# Get information about the spell
				angband spell info $bookNum $spell attrib
				
				# Get the spell char
				set char $attrib(char)

				NewItem $oop $char $attrib(name) White \
					$bookNum $spell $char
			}
		}

		select {
			lassign $args bookNum index
			NSRecall::RecallSpell $bookNum $index
		}

		invoke {
			lassign $args bookNum index char
			angband keypress $char
		}
	}

	return
}
proc NSPopup::SelectSpell {parent x y} {
	NSModule::LoadIfNeeded NSPopup
	return [NSPopup::Select [Global popup,oop] spell $parent $x $y]
}

proc NSPopup::hook_ele_attack {oop message args} {

	switch -- $message {

		set_list {

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
				NewItem $oop $char $attack $color \
					$char
			}
		}

		select {
		}

		invoke {
			lassign $args char
			angband keypress $char
		}
	}

	return
}
proc NSPopup::SelectEleAttack {parent x y} {
	NSModule::LoadIfNeeded NSPopup
	return [NSPopup::Select [Global popup,oop] ele_attack $parent $x $y]
}

proc NSPopup::hook_mindcraft {oop message args} {

	switch -- $message {

		set_list {

			# Process each power
			foreach power [angband mindcraft get] {

				# Get information about the power
				angband mindcraft info $power attrib
				if {!$attrib(okay)} continue

				NewItem $oop $attrib(char) $attrib(name) White \
					$power $attrib(char)

				incr num
			}
		}

		select {
			lassign $args power char
			NSRecall::RecallMindcraft $power
		}

		invoke {
			lassign $args power char
			angband keypress $char
		}
	}

	return
}
proc NSPopup::SelectMindcraft {parent x y} {
	NSModule::LoadIfNeeded NSPopup
	return [NSPopup::Select [Global popup,oop] mindcraft $parent $x $y]
}

proc NSPopup::hook_pet {oop message args} {

	switch -- $message {

		set_list {

			# Process each command
			foreach {char label} [NSRecall::PetCmdInfo mode] {

				if {$char eq $mode} {
					set color [Value TERM_L_BLUE]
				} else {
					set color White
				}
				NewItem $oop $char $label $color \
					$char
			}
		}

		select {
		}

		invoke {
			lassign $args char
			angband keypress $char
		}
	}

	return
}
proc NSPopup::SelectPetCommand {parent x y} {
	NSModule::LoadIfNeeded NSPopup
	return [NSPopup::Select [Global popup,oop] pet $parent $x $y]
}

proc NSPopup::hook_power {oop message args} {

	switch -- $message {

		set_list {
			set powerChars "abcdefghijklmnopqrstuvwxyz0123456789"

			set num 0
			foreach power [angband power get] {

				# Get information about the power
				angband power info $power attrib

				# Get the power char
				set attrib(char) [string index $powerChars $num]

				set fill White
				if {$attrib(chance) == 100} {
					set fill gray70
				}

				NewItem $oop $attrib(char) $attrib(name) $fill \
					$power $attrib(char)

				incr num
			}
		}

		select {
			lassign $args power char
			NSRecall::RecallPower $power
		}

		invoke {
			lassign $args power char
			angband keypress $char
		}
	}

	return
}
proc NSPopup::SelectPower {parent x y} {
	NSModule::LoadIfNeeded NSPopup
	return [NSPopup::Select [Global popup,oop] power $parent $x $y]
}

