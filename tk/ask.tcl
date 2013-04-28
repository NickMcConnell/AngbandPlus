# File: ask.tcl

# Purpose: The Ask display

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSAsk {

	variable Priv

# namespace eval NSAsk
}

# NSAsk::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::InitModule {} {

	MsgCatInit inven

	InitImageIfNeeded Image_ButtonOptions button-options.gif
	InitImageIfNeeded Image_Checked checked.gif
	InitImageIfNeeded Image_Unchecked unchecked.gif

	NSObject::New NSAsk

	return
}

# NSAsk::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::CloseModule {} {

	catch {
		destroy [Global ask,win]
	}
	return
}

# NSAsk::NSAsk --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::NSAsk {oop} {

	InitDisplay $oop
	PopupInit $oop

	set win [Info $oop win]

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSAsk $oop $win

	return
}

# NSAsk::~NSAsk --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::~NSAsk {oop} {

	return
}

# NSAsk::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::Info {oop info args} {

	global NSAsk

	# Verify the object
	NSObject::CheckObject NSAsk $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSAsk($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSAsk($oop,$info)
			}
		}
	}

	return
}

# NSAsk::InitDisplay --
#
#	The Ask Window pops when user is being asked something.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::InitDisplay {oop} {

	set top [Global main,widget].ask$oop
	frame $top -borderwidth 0

	Info $oop win $top
	Global ask,win $top

	#
	# quantity: horizontal slider control + all/ok/cancel buttons
	#
	set frame $top.frame_quantity
	frame $frame -borderwidth 0
	if {$::UseTile && 0} {
		ttk::scale $frame.scale -orient horizontal -length 200 \
			-command "NSAsk::QuantityChanged $oop $frame.scale"
	} else {
		scale $frame.scale -orient horizontal -length 200 -showvalue no \
			-highlightthickness 0 \
			-command "NSAsk::QuantityChanged $oop $frame.scale"
	}
	button $frame.buttonAll -text [mc All] -command {
		set top [Global ask,win]
		set count [$top.frame_quantity.scale get]
		angband keypress [string repeat \010 [string length $Prompt(new)]]
		angband keypress a\r
	}
	button $frame.buttonOK -text [mc OK] -command {
		set top [Global ask,win]
		set count [$top.frame_quantity.scale get]
		angband keypress [string repeat \010 [string length $Prompt(new)]]
		angband keypress $count\r
	}
	button $frame.buttonCancel -text [mc Cancel] -command "angband keypress \033"

	pack $frame.scale -side left -anchor w -padx 5 -pady 4
	pack $frame.buttonAll -side left -anchor w -padx {0 5} -pady 4
	pack $frame.buttonOK -side left -anchor w -padx {0 5} -pady 4
	pack $frame.buttonCancel -side left -anchor w -padx {0 5} -pady 4

	#
	# yes_no: yes/no buttons
	#
	set frame $top.frame_yes_no
	frame $frame -borderwidth 0
	button $frame.buttonYes -text [mc Yes] -command "angband keypress y" -width 5
	button $frame.buttonNo -text [mc No] -command "angband keypress n" -width 5

	NSUtils::CheckButtonWidth $frame.buttonYes
	NSUtils::CheckButtonWidth $frame.buttonNo

	pack $frame.buttonYes -side left -padx 5 -pady 4
	pack $frame.buttonNo -side left -padx {0 5} -pady 4

	#
	# haggle: accept/cancel buttons
	#
	set frame $top.frame_haggle
	frame $frame -borderwidth 0
	button $frame.buttonAccept -text [mc Accept] -command "NSAsk::HaggleAccept $oop"
	button $frame.buttonCancel -text [mc Cancel] -command {angband keypress \033}

	pack $frame.buttonAccept -side left -padx 5 -pady 4
	pack $frame.buttonCancel -side left -padx {0 5} -pady 4

	#
	# item: equip/inven/floor/cancel buttons
	#
	set frame $top.frame_item
	frame $frame -borderwidth 0

	set menuId [NSObject::New NSMenu *$top \
		-tearoff 0 -identifier MENU_POPUP]
	NSMenu::Info $menuId menuSelectCmd "NSAsk::MenuSelect $oop"
	set m [NSMenu::Info $menuId menu]

	NSModule::LoadIfNeeded NSWin98ToolbarButton

	win98button $frame.buttonE \
		-command "NSAsk::ButtonItem $oop equipment" \
		-label [mc Equipment] -showlabel yes \
		-showimage no -heightlabel 16 -hasmenu yes \
		-menucommand "NSAsk::Win98MenuCmd_Item $oop $m equipment"
$frame.buttonE configure -hovermenu yes -onlymenu yes -menuhidecmd "NSAsk::PopupHide $oop"

	win98button $frame.buttonI \
		-command "NSAsk::ButtonItem $oop inventory" \
		-label [mc Inventory] -showlabel yes \
		-showimage no -heightlabel 16 -hasmenu yes \
		-menucommand "NSAsk::Win98MenuCmd_Item $oop $m inventory"
$frame.buttonI configure -hovermenu yes -onlymenu yes -menuhidecmd "NSAsk::PopupHide $oop"

	win98button $frame.buttonF \
		-command "NSAsk::ButtonItem $oop floor" \
		-label [mc Floor] -showlabel yes \
		-showimage no -heightlabel 16 -hasmenu yes \
		-menucommand "NSAsk::Win98MenuCmd_Item $oop $m floor"
$frame.buttonF configure -hovermenu yes -onlymenu yes -menuhidecmd "NSAsk::PopupHide $oop"

	button $frame.buttonCancel -text [mc Cancel] -command {angband keypress \033}

	pack $frame.buttonE -side left -padx {5 5} -pady 4
	pack $frame.buttonI -side left -padx {0 5} -pady 4
	pack $frame.buttonF -side left -padx {0 5} -pady 4
	pack $frame.buttonCancel -side left -padx {0 5} -pady 4

	qebind Ask <Choose-item> "NSAsk::Bind_Ask $oop item %o %a %s"

	#
	# Options
	#
	win98button $top.options -image Image_ButtonOptions -showlabel no -hasmenu yes \
		-hovermenu yes -onlymenu yes -menuhidecmd "NSAsk::PopupHide $oop" \
		-menucommand "NSAsk::Win98MenuCmd_Option $oop"



	qebind Ask <Ask> "NSAsk::Bind_Ask $oop %d %m %M %s"

	# Don't display until we see a <Term-fresh> event
	qebind Ask <Term-fresh> "NSAsk::Bind_TermFresh $oop"
	qeconfigure Ask <Term-fresh> -active no

	Term_KeyPress_Bind $top

	return
}

# NSAsk::ReqHeightOfAskWindow --
#
#	Return the requested height of the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::ReqHeightOfAskWindow {} {

	set top [Global ask,win]

	set maxHeight 0
	foreach detail {haggle item quantity yes_no} {
		pack forget [pack slaves $top]
		pack $top.frame_$detail
		update idletasks
		if {[winfo reqheight $top] > $maxHeight} {
			set maxHeight [winfo reqheight $top]
		}
	}

	return $maxHeight
}

# NSAsk::Bind_Ask --
#
#	Handle <Ask> binding.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::Bind_Ask {oop detail min max show} {

	set top [Info $oop win]

if {[Info $oop popup,posted] ne ""} { [Info $oop popup,posted] hidemenu }

	# Show it
	if {$show} {

		Global ask,detail $detail
		Global ask,min $min
		Global ask,max $max
		Global ask,options [angband inkey_options]

		if {$detail eq "item"} {
			set options {}

			# Hack -- append universal options when choosing items
			if {[variant ANGBANDTK KANGBANDTK ZANGBANDTK]} {
				lappend options easy_floor
			}
			if {[variant OANGBANDTK]} {
			}
			if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
				lappend options floor_query_flag
			}
			if {[variant ZANGBANDTK]} {
				lappend options other_query_flag
			}
			lappend ::Global(ask,options) {*}$options
		}

		# Delay display until <Term-fresh> happens
		qeconfigure Ask <Term-fresh> -active yes

	} elseif {![winfo ismapped $top]} {
		qeconfigure Ask <Term-fresh> -active no

	# Hide it
	} else {

		# Don't allow mouse-related stuff while this is disappearing
		Global UI_Busy 1

		# Slide from view
		set height [winfo reqheight $top]
		foreach frac [list 0.7 0.4 0.1] {
			set dy [expr {$height * $frac}]
			place $top -x 0 -rely 0 -y $dy -anchor sw

			set ms [clock milliseconds]
			update
			set diff [expr {[clock milliseconds] - $ms}]
			if {$diff < 10} {
				after [expr {10 - $diff}]
			}
		}

		# Allow mouse-related stuff again
		Global UI_Busy 0

		place forget $top
		pack forget $top.frame_$detail
		if {[string match $top* [focus -lastfor [Window main]]]} {
			focus [Window main]
		}

		qeconfigure Ask <Term-fresh> -active no
	}

	return
}

# NSAsk::Bind_TermFresh --
#
#	Handle <Ask> binding.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::Bind_TermFresh {oop} {

	set top [Info $oop win]

	set detail [Global ask,detail]
	set min [Global ask,min]
	set max [Global ask,max]

	switch -- $detail {
		haggle {
		}
		item {
			foreach where {equipment inventory floor} b {E I F} {
				if {[lsearch -exact $max $where] != -1} {
					$top.frame_$detail.button$b configure -state normal
				} else {
					$top.frame_$detail.button$b configure -state disabled
				}
			}
		}
		quantity {
			$top.frame_quantity.scale configure -from $min -to $max
			$top.frame_quantity.scale set 1
		}
		yes_no {
		}
	}

	if {[winfo ismapped $top.frame_$detail]} {
		if {0 && [Global ask,options] ne "" && ![winfo ismapped $top.options]} {
			pack $top.options -side left -padx 6 -before $top.frame_$detail
		}
		if {0 && [Global ask,options] eq "" && [winfo ismapped $top.options]} {
			pack forget $top.options
		}
	} else {
		pack forget [pack slaves $top]

		if {0 && [Global ask,options] ne ""} {
			pack $top.options -side left -padx 6
		}
		pack $top.frame_$detail -side left

		# Update geometry
		update idletasks

		# Don't allow mouse-related stuff while this is displaying
		Global UI_Busy 1

		# Slide into view
		set height [winfo reqheight $top]
		foreach frac [list 0.1 0.2 0.3 0.5 0.7 1] {
			set dy [expr {$height * $frac}]
			place $top -x 0 -rely 0 -y $dy -anchor sw

			set ms [clock milliseconds]
			update
			set diff [expr {[clock milliseconds] - $ms}]
			if {$diff < 10} {
				after [expr {10 - $diff}]
			}
		}

		# Allow mouse-related stuff again
		Global UI_Busy 0
	}

	qeconfigure Ask <Term-fresh> -active no

	return
}

# NSAsk::QuantityChanged --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::QuantityChanged {oop scale value} {

	# Don't clear the intial yellow prompt else we can't type 'a' to
	# overwrite the initial value.
	set promptValue [lindex $::Prompt(new) end-1]
	if {$promptValue eq $value} return

	# Backspaces to clear current prompt
	angband keypress [string repeat \010 [string length $promptValue]]

	# New value
	angband keypress $value

	return
}

# NSAsk::Win98MenuCmd_Item --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::Win98MenuCmd_Item {oop menu where button} {

	set canvas [$button info canvas]
	set x [winfo rootx $canvas]
	set y [expr {[winfo rooty $canvas] + [winfo height $canvas]}]

	$menu delete 0 end

	set command_wrk [Global ask,min]
	set indexList [angband $where find -tester yes]
	set count [llength $indexList]

	# floor
	if {$where eq "floor"} {
		if {$where ne $command_wrk} {
			set toggleChar -
		} else {
			set toggleChar ""
		}

		# Two different options make floor selection complicated
		if {[variant ANGBANDTK KANGBANDTK ZANGBANDTK]} {
			set easy_floor [Setting easy_floor]
		}
		if {[variant OANGBANDTK]} {
			set easy_floor 1
		}
		if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
			set query_flag [Setting floor_query_flag]
		}
		if {[variant ZANGBANDTK]} {
			set query_flag [Setting other_query_flag]
		}

		if {$easy_floor} {
			# multiple items
			if {$count > 1} {
				set row 0
				foreach index $indexList {
					angband $where info $index attrib
					set char [string index "abcdefghijklmnopqrstuvw" $row]
					$menu add command -label "$char\) $attrib(name)" \
						-command "angband keypress $toggleChar$char"
					incr row
				}
			# one item
			} else {
				angband $where info [lindex $indexList 0] attrib
				if {$query_flag} {
					set char a
				} else {
					if {$command_wrk eq "floor"} {
						set toggleChar -
					}
					set char ""
				}
				$menu add command -label "a\) $attrib(name)" \
					-command "angband keypress $toggleChar$char"
			}

		# without easy_floor
		} else {
			# multiple items
			if {$count > 1} {
				if {$query_flag} {
					set row 0
					foreach index $indexList {
						angband $where info $index attrib
						set char [string index "abcdefghijklmnopqrstuvw" $row]
						set prompt [string repeat n $row]y
						$menu add command -label "$char\) $attrib(name)" \
							-command "angband keypress $toggleChar$prompt"
						incr row
					}
				# without query_flag only top item may be selected
				} else {
					angband $where info [lindex $indexList 0] attrib
					$menu add command -label "a\) $attrib(name)" \
						-command "angband keypress $toggleChar"
				}
			# one item
			} else {
				angband $where info [lindex $indexList 0] attrib
				if {$command_wrk eq "floor"} {
					set toggleChar -
				}
				if {$query_flag} {
					append toggleChar y
				}
				$menu add command -label "a\) $attrib(name)" \
					-command "angband keypress $toggleChar"
			}
		}

	# inventory or equipment
	} else {
		if {$where ne $command_wrk} {
			# Jump from floor to inven to equip
			if {($where eq "equipment") && ($command_wrk eq "floor")} {
				set toggleChar //
			# Jump from equip/floor to inven
			} else {
				set toggleChar /
			}
		} else {
			set toggleChar ""
		}

		foreach index $indexList {
			angband $where info $index attrib
			$menu add command -label "$attrib(char)\) $attrib(name)" \
				-command "angband keypress $toggleChar$attrib(char)"
		}
	}

	$menu add separator
	$menu add command -label [mc Cancel] \
		-command "angband keypress \033"

	Global ask,item,where $where

	tk_popup $menu $x $y

	if {[Platform unix]} {
		tkwait variable ::tk::Priv(popup)
	}

	after idle "$button hidemenu"

	return
}

proc NSAsk::Win98MenuCmd_Item {oop menu where button} {

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

	set command_wrk [Global ask,min]
	set indexList [angband $where find -tester yes]
	set count [llength $indexList]

	# floor
	if {$where eq "floor"} {
		if {$where ne $command_wrk} {
			set toggleChar -
		} else {
			set toggleChar ""
		}

		# Two different options make floor selection complicated
		if {[variant ANGBANDTK KANGBANDTK ZANGBANDTK]} {
			set easy_floor [Setting easy_floor]
		}
		if {[variant OANGBANDTK]} {
			set easy_floor 1
		}
		if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
			set query_flag [Setting floor_query_flag]
		}
		if {[variant ZANGBANDTK]} {
			set query_flag [Setting other_query_flag]
		}

		if {$easy_floor} {
			# multiple items
			if {$count > 1} {
				set row 0
				foreach index $indexList {
					angband $where info $index attrib
					set char [string index "abcdefghijklmnopqrstuvw" $row]
					set color [default_tval_to_attr $attrib(tval)]
					set keypress $toggleChar$char
					PopupNewItem $oop $char $attrib(name) $color \
						$where $index $keypress
					incr row
				}
			# one item
			} else {
				angband $where info [lindex $indexList 0] attrib
				if {$query_flag} {
					set char a
				} else {
					if {$command_wrk eq "floor"} {
						set toggleChar -
					}
					set char ""
				}
				set keypress $toggleChar$char
				set color [default_tval_to_attr $attrib(tval)]
				PopupNewItem $oop a $attrib(name) $color \
					$where [lindex $indexList 0] $keypress
			}

		# without easy_floor
		} else {
			# multiple items
			if {$count > 1} {
				if {$query_flag} {
					set row 0
					foreach index $indexList {
						angband $where info $index attrib
						set char [string index "abcdefghijklmnopqrstuvw" $row]
						set prompt [string repeat n $row]y
						set color [default_tval_to_attr $attrib(tval)]
						set keypress $toggleChar$prompt
						PopupNewItem $oop $char $attrib(name) $color \
							$where $index $keypress
						incr row
					}
				# without query_flag only top item may be selected
				} else {
					angband $where info [lindex $indexList 0] attrib
					set color [default_tval_to_attr $attrib(tval)]
					set keypress $toggleChar
					PopupNewItem $oop a $attrib(name) $color \
						$where [lindex $indexList 0] $keypress
				}
			# one item
			} else {
				angband $where info [lindex $indexList 0] attrib
				if {$command_wrk eq "floor"} {
					set toggleChar -
				}
				if {$query_flag} {
					append toggleChar y
				}
				set color [default_tval_to_attr $attrib(tval)]
				set keypress $toggleChar
				PopupNewItem $oop a $attrib(name) $color \
					$where [lindex $indexList 0] $keypress
			}
		}

	# inventory or equipment
	} else {
		if {$where ne $command_wrk} {
			# Jump from floor to inven to equip
			if {($where eq "equipment") && ($command_wrk eq "floor")} {
				set toggleChar //
			# Jump from equip/floor to inven
			} else {
				set toggleChar /
			}
		} else {
			set toggleChar ""
		}

		foreach index $indexList {
			angband $where info $index attrib
			set color [default_tval_to_attr $attrib(tval)]
			set keypress $toggleChar$attrib(char)
			PopupNewItem $oop $attrib(char) $attrib(name) $color \
				$where $index $keypress
		}
	}

	PopupNewSeparator $oop
	set item [$tree item create -parent root]
	$tree item style set $item COL sCancel

	Global ask,item,where $where

	# Set the size of the list to the size of all the items
	scan [$tree item bbox end] "%d %d %d %d" left top right bottom
	$tree configure -width $right -height $bottom
	update idletasks

	wm geometry [Info $oop popup,win] +$x+$y

	wm transient [Info $oop popup,win] [Info $oop win]
	wm deiconify [Info $oop popup,win]
	raise [Info $oop popup,win]

	Info $oop popup,posted $button
	Info $oop popup,hook item

if 1 {
	# Set a grab on the Ask Window and claim the focus
	NSUtils::GrabSave [Info $oop win]
	focus $tree
} else {
	# Set a grab on the window and claim the focus
	NSUtils::GrabSave [Info $oop win]
	focus $tree

	set ::NSAsk($oop,result) 0
	vwait ::NSAsk($oop,result)

	# Release grab
	NSUtils::GrabRelease [Info $oop win]

	after idle "$button hidemenu"
}
	return [Info $oop popup,win]
}

# NSAsk::Win98MenuCmd_Option --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::Win98MenuCmd_Option {oop button} {

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

	foreach setting [Global ask,options] {
		PopupNewItem $oop -[Setting $setting]- [SettingDesc $setting] White \
			$setting
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
	Info $oop popup,hook option

	# Set a grab on the Ask Window and claim the focus
	NSUtils::GrabSave [Info $oop win]
	focus $tree

	return [Info $oop popup,win]
}

# NSAsk::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::MenuSelect {oop menuId index ident} {

	set detail [Global ask,detail]
	set min [Global ask,min]
	set max [Global ask,max]

	if {$index eq "none"} return

	switch -- $detail {
		item {
			set where [Global ask,item,where]
			set indexList [angband $where find -tester yes]
			if {$index < [llength $indexList]} {
				NSRecall::RecallObject $where [lindex $indexList $index]
			}
		}
	}

	return
}

# NSAsk::ButtonItem --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::ButtonItem {oop where} {

	set command_wrk [Global ask,min]

	if {$where eq "floor"} {
		set indexList [angband $where find -tester yes]
		set count [llength $indexList]

		# Two different options make floor selection complicated
		if {[variant ANGBANDTK KANGBANDTK ZANGBANDTK]} {
			set easy_floor [Setting easy_floor]
		}
		if {[variant OANGBANDTK]} {
			set easy_floor 1
		}
		if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
			set query_flag [Setting floor_query_flag]
		}
		if {[variant ZANGBANDTK]} {
			set query_flag [Setting other_query_flag]
		}

		if {$easy_floor} {
			# multiple items
			if {$count > 1} {
				if {$where ne $command_wrk} {
					angband keypress -
				} else {
					angband keypress *
				}
			# one item
			} else {
				if {$query_flag} {
					if {$where ne $command_wrk} {
						angband keypress -
					} else {
						angband keypress *
					}
				} else {
					angband keypress -
				}
			}
		# without easy_floor
		} else {
			angband keypress -
		}

	# inventory or equipment
	} else {
		if {$where ne $command_wrk} {

			# Double // to jump from floor to inven to equip
			if {($where eq "equipment") && ($command_wrk eq "floor")} {
				angband keypress //
			} else {
				angband keypress /
			}

		} else {
			angband keypress *
		}
	}

	return
}

# NSAsk::HaggleAccept --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::HaggleAccept {oop} {

	# Backspaces to clear current prompt
	angband keypress [string repeat \010 [string length $::Prompt(new)]]

	set price [Global ask,min]
	angband keypress $price\r

	return
}

# NSAsk::PopupInit --
#
#	Init the popup "menu".
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::PopupInit {oop} {

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
	# Checkbutton style
	#
	$tree state define checked
	$tree element create eChk image -image {Image_Checked checked Image_Unchecked {}}

	$tree style create STYLE_CHECK
	$tree style elements STYLE_CHECK {eSel eChk eTxt}
	$tree style layout STYLE_CHECK eChk -expand ns -padx 4
	$tree style layout STYLE_CHECK eTxt -padx {0 4} -pady 1

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

	$tree notify bind $tree <Selection> "NSAsk::PopupSelectionChanged $oop"

	bind $tree <Motion> "NSAsk::PopupMotion $oop %x %y"
	bind $win <Leave> "NSAsk::PopupLeave $oop"

	bind $tree <Control-ButtonPress-1> { ; }
	bind $tree <Shift-ButtonPress-1> { ; }
#	bind $tree <ButtonPress-1> "NSAsk::PopupButton1 $oop %x %y"
	bind $tree <ButtonRelease-1> "NSAsk::PopupButton1 $oop %x %y"

	bind $win <KeyPress-Escape> "angband keypress \033 ; set ::NSAsk($oop,result) 1"

	pack $tree -expand yes -fill both -padx 2 -pady 2
	pack $frame -expand yes -fill both

	Info $oop popup,win $win
	Info $oop popup,tree $tree
	Info $oop popup,posted ""
	Info $oop popup,afterId ""
	Info $oop popup,hook ""

	return
}

# NSAsk::PopupSelectionChanged --
#
#	Handle <Selection>.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::PopupSelectionChanged {oop} {

	set tree [Info $oop popup,tree]

	if {[$tree selection count] == 1} {
		set item [$tree selection get]
		if {[$tree item tag expr $item hook]} {
			PopupCallHook $oop select {*}[Info $oop popup,match,$item]
		}
	}

	return
}

# NSAsk::PopupMotion --
#
#	Handle <Motion>.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::PopupMotion {oop x y} {

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

# NSAsk::PopupLeave --
#
#	Handle <Leave>.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::PopupLeave {oop} {

	if {[Info $oop popup,afterId] ne ""} {
		after cancel [Info $oop popup,afterId]
	}
	Info $oop popup,afterId [after 50 "NSAsk::CheckWhoHasCursor $oop"]

	return
}

proc NSAsk::CheckWhoHasCursor {oop} {

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

# NSAsk::PopupButton1 --
#
#	Handle <ButtonPress-1>.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::PopupButton1 {oop x y} {

	set tree [Info $oop popup,tree]

	set id [$tree identify $x $y]
	if {$id eq "" || [lindex $id 0] ne "item"} {
		return
	}
	set item [lindex $id 1]
	if {[$tree item tag expr $item hook]} {
		PopupCallHook $oop invoke {*}[Info $oop popup,match,$item]
		Info $oop result 1
	} elseif {[$tree item enabled $item]} {
		angband keypress \033
		Info $oop result 1
	}

	return
}

# NSAsk::PopupCallHook --
#
#	Call the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::PopupCallHook {oop message args} {

	switch -- [Info $oop popup,hook] {
		item {
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
		}
		option {
			switch -- $message {
				select {
					lassign $args setting
					
				}
				invoke {
					lassign $args setting
					Setting $setting [expr {![Setting $setting]}]

					set tree [Info $oop popup,tree]
					$tree item state set [$tree selection get] ~checked
				}
			}
		}
	}

	return
}

# NSAsk::PopupNewItem --
#
#	Create a new item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::PopupNewItem {oop char string color args} {

	set tree [Info $oop popup,tree]
	set item [$tree item create -parent root -tags hook]
	if {$char eq "-0-" || $char eq "-1-"} {
		$tree item style set $item COL STYLE_CHECK
		if {$char eq "-1-"} {
			$tree item state set $item checked
		}
	} else {
		$tree item element configure $item COL eChar -text "$char) "
	}
	$tree item element configure $item COL eTxt -text $string -fill $color

	Info $oop popup,match,$item $args

	return $item
}

# NSAsk::PopupNewSeparator --
#
#	Create a new separator item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::PopupNewSeparator {oop} {

	set tree [Info $oop popup,tree]
	set item [$tree item create -parent root -height 6]
	$tree item enabled $item no
	$tree item style set $item COL sSeparator

	return $item
}

# NSAsk::PopupHide --
#
#	Hide the popup menu. Called by win98button.menuhidecmd.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSAsk::PopupHide {oop} {

	if {[Info $oop popup,afterId] ne ""} {
		after cancel [Info $oop popup,afterId]
		Info $oop popup,afterId ""
	}
	wm withdraw [Info $oop popup,win]
	Info $oop popup,posted ""
	NSUtils::GrabRelease [Info $oop win]
#	set ::NSAsk($oop,result) 1

	return
}

