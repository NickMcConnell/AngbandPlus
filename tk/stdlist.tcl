# File: stdlist.tcl

# Purpose: Implements standard list used throughout the game

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSList {
}

# NSList::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#
# Results:
#	What happened.

proc NSList::InitModule {} {

	package require treectrl
	option add *TreeCtrl.UseTheme 1

	InitImageIfNeeded Image_Checked checked.gif
	InitImageIfNeeded Image_Unchecked unchecked.gif

	set tag NSListDestroyBindTag
	bind $tag <Destroy> "NSList::Destroy %W"

	bind NSList <Control-ButtonPress-1> { ; }
	bind NSList <Shift-ButtonPress-1> { ; }
	bind NSList <ButtonPress-1> {
		NSList::Click %W %x %y
	}

	bind NSList <Double-ButtonPress-1> {
		NSList::Invoke %W %x %y
	}

	return
}

# NSList::New --
#
#	Create a list in the style used throughout the game.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSList::New {frame args} {

	variable Priv

	array set cfg {
		-xscrollbar 0
		-yscrollbar 1
		-columns 1
		-text 1
		-icon 0
		-wrap 0
		-checkbutton 0
		-font knowledge
	}
	array set cfg $args

if 0 {
	# Always use scrollbars, just hide them when not needed
	set cfg(-xscrollbar) 1
	set cfg(-yscrollbar) 1
}
	frame $frame \
		-borderwidth 1 -relief sunken

	set tree $frame.tree
	treectrl $tree -background [Value listBG] -usetheme yes \
		-showroot no -showlines no -showbuttons no -showheader no \
		-highlightthickness 0 -borderwidth 0 -font [Value font,$cfg(-font)]
	if {$cfg(-wrap)} {
		$tree configure -wrap window -orient horizontal
	} else {
		$tree configure -xscrollincrement 1
	}

	if {$cfg(-xscrollbar)} {
		scrollbar $frame.xscroll \
			-command "$tree xview" -orient horizontal
if 0 {
		$tree notify bind $frame.xscroll <Scroll-x> \
			"NSList::SBSet $frame.xscroll %l %u"
} else {
		$tree notify bind $frame <Scroll-x> \
			"$frame.xscroll set %l %u"
}
		NSUtils::SynchScrollBar $tree $frame.xscroll 0
	}

	if {$cfg(-yscrollbar)} {
		scrollbar $frame.yscroll \
			-command "$tree yview" -orient vertical
if 0 {
		$tree notify bind $frame.yscroll <Scroll-y> \
			"NSList::SBSet $frame.yscroll %l %u"
} else {
		$tree notify bind $frame <Scroll-y> \
			"$frame.yscroll set %l %u"
}
		NSUtils::SynchScrollBar $tree $frame.yscroll 0
	}

	set outline [list [Value listHilite] {selected focus} [Value listInactive] {selected}]
	$tree element create eSel rect -outline $outline -outlinewidth 2
	$tree element create eSel.w rect -outline $outline -outlinewidth 2 -open w
	$tree element create eSel.we rect -outline $outline -outlinewidth 2 -open we
	$tree element create eSel.e rect -outline $outline -outlinewidth 2 -open e

	$tree state define checked
	$tree element create eChk image -image {Image_Checked checked Image_Unchecked {}}

	# Delay this, since NSBirthOptions calls this before "assign" type exists
#	$tree element create eAss assign

	$tree element create eTxt text -fill White

	set Priv(cfg,$tree) [list \
		-columns $cfg(-columns) \
		-checkbutton $cfg(-checkbutton) \
		-icon $cfg(-icon) \
		-text $cfg(-text) \
	]
	NSList::Config $tree

	grid columnconfigure $frame 0 -weight 1
	grid rowconfigure $frame 0 -weight 1
	grid $tree -column 0 -row 0 -sticky news
	if {$cfg(-xscrollbar)} {
		grid rowconfigure $frame 1 -weight 0
		grid $frame.xscroll -column 0 -row 1 -sticky ew
	}
	if {$cfg(-yscrollbar)} {
		grid columnconfigure $frame 1 -weight 0
		grid $frame.yscroll -column 1 -row 0 -sticky ns
	}

	$tree notify install <Click>
if 0 {
	bind $tree <Control-ButtonPress-1> { ; }
	bind $tree <Shift-ButtonPress-1> { ; }
	bind $tree <ButtonPress-1> {
		NSList::Click %W %x %y
	}
}
	$tree notify install <Invoke>
if 0 {
	bind $tree <Double-ButtonPress-1> {
		NSList::Invoke %W %x %y
	}
}
	$tree notify install <Toggle>

	# Update ourself when the list color values change
	foreach value {listBG listHilite listInactive} {
		qebind $tree <Value-$value> "NSList::ValueChanged $tree $value"
	}

	# Update ourself when the font,XXX value changes
	set Priv(font,$tree) $cfg(-font)
	qebind $tree <Value-font,$cfg(-font)> "NSList::ValueChanged $tree font"

	bindtags $tree [concat NSList [bindtags $tree] NSListDestroyBindTag]

	return $tree
}

proc NSList::Destroy {tree} {

	variable Priv

	# bindings are deleted with the tree

	array unset Priv cfg,$tree
	array unset Priv cfg,*,$tree
	array unset Priv font,$tree

	return
}

proc NSList::ValueChanged {tree value} {

	variable Priv

	switch -- $value {
		font {
			set value font,$Priv(font,$tree)
			$tree configure -font [Value $value]
		}
		listBG {
			$tree configure -background [Value listBG]
		}
		listHilite -
		listInactive {
			set outline [list [Value listHilite] {selected focus} [Value listInactive] {selected}]
			$tree element configure eSel -outline $outline
			$tree element configure eSel.w -outline $outline
			$tree element configure eSel.we -outline $outline
			$tree element configure eSel.e -outline $outline
		}
	}

	return
}

proc NSList::Config {tree args} {

	variable Priv

	array set cfg $Priv(cfg,$tree)
	array set cfg $args

	set Priv(cfg,$tree) [array get cfg]

	if {$cfg(-columns) != [$tree column count]} {

		# Delete extra columns
		set C $cfg(-columns)
		while {$cfg(-columns) < [$tree column count]} {
			$tree column delete last
			$tree style delete s$C
			array unset Priv cfg,$C,$tree
			incr C
		}

		# Create needed columns
		set C [$tree column count]
		while {[$tree column count] < $cfg(-columns)} {
			$tree column create -button no -font [Value font,system]
			if {$C == 0} {
				# By default, only column 0 has checkbutton and/or icon
				set Priv(cfg,$C,$tree) [list -checkbutton $cfg(-checkbutton) -icon $cfg(-icon) -text $cfg(-text)]
				$tree column configure "order $C" -expand yes
			} elseif {![info exists Priv(cfg,$C,$tree)]} {
				# By default, column 1+ has text only
				set Priv(cfg,$C,$tree) [list -checkbutton no -icon no -text yes]
				$tree column configure "order $C" -expand no
			}
			$tree style create s$C
			ConfigColumn $tree "order $C" {*}$Priv(cfg,$C,$tree)
			incr C
		}
	}

	set styles {}
	for {set C 0} {$C < [$tree column count]} {incr C} {
		lappend styles s$C
	}
	$tree configure -defaultstyle $styles

	# We must be configuring column 0
	ConfigColumn $tree "order 0" {*}$args

	return
}

proc NSList::ConfigColumn {tree column args} {

	variable Priv

	set C [$tree column order $column]

	array set cfg $Priv(cfg,$C,$tree)
	array set cfg $args

	set S s$C

	# Clear elements to get default layout below
	$tree style elements $S {}
if 1 {
	set eSel eSel ; # ColumnShow will choose correct selection rectangle
	set padx {}
} else {
	# Figure out which selection rectangle to use
	if {[$tree column count] == 1} {
		set eSel eSel
		set padx {-padx 1 -ipadx 3}
	} else {
		if {$C == 0} {
			set eSel eSel.e
			set padx {-padx {1 0} -ipadx {3 0}}
		} elseif {$C == [$tree column count] - 1} {
			set eSel eSel.w
			set padx {-padx {0 1} -ipadx {0 3}}
		} else {
			set eSel eSel.we
			set padx {-padx 0 -ipadx 0}
		}
	}
}
	set elements $eSel
	if {$cfg(-checkbutton)} {
		lappend elements eChk
	}
	if {$cfg(-icon)} {
		if {[lsearch -exact [$tree element names] eAss] == -1} {
			$tree element create eAss assign
		}
		lappend elements eAss
	}
	if {$cfg(-text)} {
		lappend elements eTxt
	}
	$tree style elements $S $elements
	$tree style layout $S $eSel {*}$padx -pady 1 -ipady 3 \
		-union [lrange $elements 1 end] -iexpand news
	if {$cfg(-checkbutton)} {
		$tree style layout $S eChk -expand ns
		if {$cfg(-text)} {
			$tree style layout $S eChk -padx {0 4}
		}
	}
	if {$cfg(-icon)} {
		$tree style layout $S eAss -expand ns
		if {$cfg(-text)} {
			$tree style layout $S eAss -padx {0 4}
		} else {
			$tree style layout $S eAss -expand news
		}
	}
	if {$cfg(-text)} {
		$tree style layout $S eTxt -expand ns
	}

	ColumnShow $tree "order $C" [$tree column cget "order $C" -visible]

	return
}

# Must update selection rectangles when a column is shown/hidden
proc NSList::ColumnShow {tree column args} {

	if {![llength $args]} {
		return [$tree column cget $column -visible]
	}

	# Show/hide the column
	set visible [lindex $args 0]
	$tree column configure $column -visible $visible

	# Get a list of visible columns
	set visible [$tree column list -visible]

	foreach C $visible {
		# Figure out which selection rectangle to use
		if {[llength $visible] == 1} {
			set eSel eSel
			set padx {-padx 1 -ipadx 3}
		} else {
			if {$C == [lindex $visible 0]} {
				set eSel eSel.e
				set padx {-padx {1 0} -ipadx {3 0}}
			} elseif {$C == [lindex $visible end]} {
				set eSel eSel.w
				set padx {-padx {0 1} -ipadx {0 3}}
			} else {
				set eSel eSel.we
				set padx {-padx 0 -ipadx 0}
			}
		}

		# Replace old eSel with new eSel
		set C [$tree column order $C]
		set S s$C
		set elements [$tree style elements $S]
		set i [lsearch -glob $elements eSel*]
		set _eSel [lindex $elements $i]
		set layout [$tree style layout $S $_eSel]
		lset elements $i $eSel
		$tree style elements $S $elements
		$tree style layout $S $eSel {*}$layout {*}$padx

# T style replace S -keeplayout E1 E2
	}

	return
}

proc NSList::Clear {tree} {
	$tree item delete all
	return
}

proc NSList::NewItem {tree} {
	set item [$tree item create]
	return [$tree item lastchild root $item]
}

proc NSList::SetCheck {tree item checked} {
	if {$checked} {
		$tree item state set $item checked
	} else {
		$tree item state set $item !checked
	}
	return
}

proc NSList::SetIcon {tree item icon} {
	$tree item element configure $item 0 eAss -fg $icon
	return
}

proc NSList::SetText {tree item text} {
	$tree item element configure $item 0 eTxt -text $text
	return
}

proc NSList::SetTextEx {tree item column text} {
	if {[string is integer $column]} {
		set column "order $column"
	}
	$tree item element configure $item $column eTxt -text $text
	return
}

proc NSList::SetTextFill {tree item color} {
	$tree item element configure $item 0 eTxt -fill $color
	return
}

proc NSList::SetTextFillEx {tree item column color} {
	if {[string is integer $column]} {
		set column "order $column"
	}
	$tree item element configure $item $column eTxt -fill $color
	return
}

proc NSList::GetText {tree item} {
	return [$tree item element cget $item 0 eTxt -text]
}

proc NSList::Point2Row {tree x y} {

	set ident [$tree identify $x $y]
	if {[lindex $ident 0] ne "item"} {
		return -1
	}
	set item [lindex $ident 1]
	return [NSList::Item2Row $tree $item]
}

# Duplicate behaviour of clickCmd of NSCanvist
proc NSList::Click {tree x y} {

	set ident [$tree identify $x $y]

	# Remove selection when clicking outside items/header
	if {$ident eq ""} {
		$tree selection clear
	}
	if {[lindex $ident 0] ne "item"} return

	set item [lindex $ident 1]
	set row [NSList::Item2Row $tree $item]

	# Click checkbutton image
	if {[lsearch -exact $ident eChk] != -1} {
		$tree notify generate <Toggle> [list I $item r $row] \
			"::TreeCtrl::PercentsCmd $tree"
		return
	}

	# Click and already-selected item
	if {![$tree selection includes $item]} return
	$tree notify generate <Click> [list I $item r $row] \
		"::TreeCtrl::PercentsCmd $tree"

	return
}

# Duplicate behaviour of invokeCmd of NSCanvist
proc NSList::Invoke {tree x y} {

	set ident [$tree identify $x $y]
	if {[lindex $ident 0] ne "item"} return

	set item [lindex $ident 1]
	set row [NSList::Item2Row $tree $item]

	# Click checkbutton image
	if {[lsearch -exact $ident eChk] != -1} {
		$tree notify generate <Toggle> [list I $item r $row] \
			"::TreeCtrl::PercentsCmd $tree"
		return
	}

	# Click and already-selected item
	if {![$tree selection includes $item]} return
	$tree notify generate <Invoke> [list I $item r $row] \
		"::TreeCtrl::PercentsCmd $tree"

	return
}

proc NSList::OnSelection {tree script} {
	$tree notify bind $tree <Selection> $script
	return
}

proc NSList::OnClick {tree script} {
	$tree notify bind $tree <Click> $script
	return
}

proc NSList::OnInvoke {tree script} {
	$tree notify bind $tree <Invoke> $script
	return
}

proc NSList::OnToggle {tree script} {
	$tree notify bind $tree <Toggle> $script
	return
}

proc NSList::Item2Row {tree item} {
	return [$tree item order $item -visible]
}

proc NSList::Row2Item {tree row} {
	return [$tree item id "root child $row visible"]
}

proc NSList::Activate {tree item} {
	$tree activate $item
	$tree selection modify active all
	$tree see active
	return
}

proc NSList::SelectAll {tree} {
	$tree selection add all
	return
}

# http://wiki.tcl.tk/950
proc NSList::SBSet {sb first last} {
	if {$first <= 0 && $last >= 1} {
		grid remove $sb
	} else {
		grid $sb
	}
	$sb set $first $last
	return
}

