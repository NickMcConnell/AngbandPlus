# File: recall.tcl

# Purpose: the Recall Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSRecall {

	variable Priv

# namespace eval NSRecall
}

# NSRecall::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::InitModule {} {

	variable Priv

	MsgCatInit recall

	set Priv(icon,valid) 0
	set Priv(icon,known) 0

	set oop [NSObject::New NSRecall]

	# Update ourself when the font for the Recall Window changes
	qebind NSRecall <Value-font,recall> \
		"NSRecall::ValueChanged_font_recall $oop"

	return
}

# NSRecall::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::CloseModule {} {

	catch {
		destroy [Window recall]
	}

	return
}

# NSRecall::NSRecall --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSRecall::NSRecall {oop} {

	Info $oop autoExpand [Value recall,autoexpand]
	Info $oop showIcon [Value recall,showicon]
	Info $oop newStyle [Value recall,newstyle]

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow recall $win \
		"NSRecall::GeometryCmd $oop" \
		"" \
		"NSRecall::DisplayCmd $oop"

	qebind NSRecall <Choose> \
		"NSRecall::Choose $oop %d %s %o"
	qeconfigure NSRecall <Choose> \
		-active no ; # [expr {![Value choicewindow,show]}]

	# If the Choice Window is displayed, we don't show choices in
	# the Recall Window.
	qebind NSRecall <Value-choicewindow,show> {
		NSRecall::SetHook [Global recall,oop] ""
		qeconfigure NSRecall <Choose> -active [expr {!%c}]
	}

	qebind NSRecall <Term-fresh> \
		"NSRecall::Fresh_Display $oop"
	qeconfigure NSRecall <Term-fresh> -active no
	if {$::DEBUG} {
		set ::debug_display 0
	}

	qebind NSRecall <Setting-depth_in_feet> \
		"NSRecall::SettingChanged_depth_in_feet $oop"

	if {[variant OANGBANDTK]} {
		qebind NSRecall <Setting-use_metric> \
			"NSRecall::SettingChanged_depth_in_feet $oop"
	}

	qebind NSRecall <Track-race> {
		NSRecall::RecallMonster %w
	}
	qeconfigure NSRecall <Track-race> -active no

	qebind NSRecall <IconCfg> \
		"NSRecall::IconCfg $oop"

	Info $oop hook ""
	Info $oop busy 0
	Info $oop expanded 0
	Info $oop current ""
	Info $oop inConfigure 0
	Info $oop afterId,configure ""

	Info $oop history,ignore 0
	Info $oop history,desc {}
	Info $oop history,args {}

	# Kind of information currently displayed
	Info $oop display ""

	Info $oop monsterMem ""

	#
	# Global list of application windows
	#

	Global recall,oop $oop
	Window recall $win

	return
}

# NSRecall::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::Info {oop info args} {

	global NSRecall

	# Verify the object
	NSObject::CheckObject NSRecall $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSRecall($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSRecall($oop,$info)
			}
		}
	}

	return
}

# NSRecall::InitWindow --
#
#	Create a recall window.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSRecall::InitWindow {oop} {

	set win .recall$oop
	toplevel $win
	wm title $win [mc Recall]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Feed the Term when keys are pressed
	Term_KeyPress_Bind $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSRecall::Close $oop"

	# Turn off geometry propagation for the window
#	pack propagate $win no

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
	$canvas create rectangle \
		4 4 [expr {6 + [icon width] + 1}] [expr {6 + [icon height] + 1}] \
		-outline Black -tags focus

	# Sometimes the highlight does not go away when the Knowledge Window appears
	$canvas bind icon <Enter> {
		%W itemconfigure focus -outline gray60
	}
	$canvas bind icon <Leave> {
		%W itemconfigure focus -outline Black
	}
	$canvas bind icon <ButtonPress-1> "
		$canvas move icon 1 1
		set CanvasButtonDown 1
	"
	$canvas bind icon <Button1-Enter> "
		$canvas move icon 1 1
		set CanvasButtonDown 1
	"
	$canvas bind icon <Button1-Leave> "
		$canvas move icon -1 -1
		set CanvasButtonDown 0
	"
	$canvas bind icon <ButtonRelease-1> "
		if {\$CanvasButtonDown} {
			$canvas move icon -1 -1
			update idletasks
			NSRecall::DisplayKnowledge $oop
		}
	"
	$canvas bind icon <Control-ButtonRelease-1> "
		if {\$CanvasButtonDown} {
			$canvas move icon -1 -1
			update idletasks
			NSRecall::DisplayAssign $oop
		}
	"

	# Create an arrow which appears when there is content out of sight
	set x [expr {$iconWidth / 2}]
	$canvas create polygon [expr {$x - 3}] 46 [expr {$x + 3}] 46 \
		$x 49 -fill Red -outline Red -tags arrow
if 0 {
	# Display the Assign Window when the icon is clicked
	$canvas bind icon <ButtonPress-1> \
		"NSRecall::DisplayAssign $oop"
}
	# If the icon of the displayed monster/object changes, we must
	# update the canvas Widget item. This binding does it.
	qebind $canvas <Assign> \
		"NSRecall::IconChanged $oop %d %I %a"

	set wrap word
	if {$::JAPANESE} {set wrap char}

	# Keep this synched with NSChoiceWindow
	if {[ScreenHeight] >= 1024} { # 1280x1024
		set height 10
	} elseif {[ScreenHeight] >= 768} { # 1024x768
		set height 8
	} elseif {[ScreenHeight] >= 600} { # 800x600
		set height 4
	} else { # 640x480
		set height 3
	}

	text $frame.text \
		-wrap $wrap -width 50 -height $height -font [Value font,recall] \
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

	# Window expands and contracts as the mouse enters and leaves it
	bindtags $win [concat [bindtags $win] RecallBindTag]
	bind RecallBindTag <Enter> "NSRecall::Expand $oop"
	bind RecallBindTag <Leave> "NSRecall::Contract $oop"

	# When the window changes size, reposition the indicator arrow
	bind $frame.text <Configure> \
		"NSRecall::Configure $oop"

	# Fiddle with the selection for list behaviour
	$frame.text tag configure HOT -foreground White \
		-background [Value listHilite]

	$frame.text tag bind HOT <ButtonPress-1> \
		"NSRecall::Invoke $oop \[$frame.text index {@%x,%y linestart}]"
	$frame.text tag bind TEXT <Motion> \
		"NSRecall::Motion $oop \[$frame.text index {@%x,%y linestart}]"
	$frame.text tag bind HOT <Leave> \
		"NSRecall::Motion $oop {}"

	if {[Platform unix]} {

		# When the inactive window is clicked, I get a <Leave> event
		# followed by an <Enter> event. The <Leave> Contract()'s the window
		# and removes the highlight (if any).

		bind RecallWindowBindTag <Leave> "
			if {!\[NSRecall::HasCursor $oop]} {
				NSRecall::Contract $oop
			}
		"
		$frame.text tag bind HOT <Leave> "
			if {!\[NSRecall::CursorHot $oop %x %y]} {
				NSRecall::Motion $oop {}
			}
		"
		bind $win <Leave> "
			if {!\[NSRecall::HasCursor $oop]} {
				NSRecall::Motion $oop {}
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

	#
	# Context Menu
	#

	set menu $win.context
	menu $menu -tearoff 0
	menu $menu.history -tearoff 0
	bind $menu.history <<MenuSelect>> \
		"NSRecall::MenuSelectHistory $oop $menu.history"
	bind $frame.icon <ButtonPress-3> \
		"NSRecall::ContextMenu $oop $menu %X %Y"
	bind $frame.text <ButtonPress-3> \
		"NSRecall::ContextMenu $oop $menu %X %Y"

	return
}

# NSRecall::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::DisplayCmd {oop message first args} {

	switch -- $message {
		preDisplay {
		}
		postDisplay {
			qeconfigure NSRecall <Choose> \
				-active [expr {![Value choicewindow,show]}]
			qeconfigure NSRecall <Track-race> -active yes
			Value recall,show 1
		}
		postWithdraw {
			qeconfigure NSRecall <Choose> -active no
			qeconfigure NSRecall <Track-race> -active no
			SetHook $oop ""
			Value recall,show 0
		}
	}

	return
}

# NSRecall::GeometryCmd --
#
#	Called by NSWindowManager::Setup(). Returns the desired (default)
#	geometry for the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::GeometryCmd {oop} {

	set win [Info $oop win]
	set winMain [Window main]
	set spacing [Global WindowSpacing]

	scan [ScreenArea] "%d %d %d %d" waLeft waTop waRight waBottom
	set waHeight [expr {$waBottom - $waTop}]
	set waWidth [expr {$waRight - $waLeft}]

	set x [NSToplevel::FrameLeft $winMain]
	if {[Value choicewindow,show]} {
		set width [NSToplevel::ContentWidth $win \
			[expr {[NSToplevel::TotalWidth $winMain] / 2}]]
	} else {
		set width [NSToplevel::TotalWidth $winMain]
		if {$width > 400} {
			set width 400
		}
		set width [NSToplevel::ContentWidth $win $width]
	}

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

# NSRecall::Close --
#
#	Description. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSRecall::Close {oop} {

	NSWindowManager::Undisplay recall

	return
}

# NSRecall::RecallArtifact --
#
#	Show info about an artifact.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::RecallArtifact {a_idx} {

	variable Priv

	if {![Value recall,show]} return

	# Hack -- Get the object id
	set oop [Global recall,oop]

	if {[string length [Info $oop hook]]} return

	# Get the icon
	set icon [angband a_info info $a_idx icon]

	# Get the name
	set name [angband a_info info $a_idx object_desc]
	set name [format [mc header-object] $name]

	# Get the memory
	set memory [angband a_info info $a_idx memory]

	# Get the color
	set k_idx [angband a_info info $a_idx k_idx]
	set tval [angband k_info set $k_idx tval]
	set color [default_tval_to_attr $tval]

	# Set the text
	SetText $oop $icon $color $name $memory

	set Priv(icon,to) artifact
	set Priv(icon,toindex) $a_idx
	set Priv(icon,valid) 1
	set Priv(icon,known) 0

	HistoryAdd $oop [angband a_info info $a_idx object_desc] RecallArtifact $a_idx

	return
}

if {[variant ZANGBANDTK]} {

# NSRecall::RecallMindcraft --
#
#	Show info about a mindcraft power.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::RecallMindcraft {index} {

	variable Priv

	if {![Value recall,show]} return

	# Hack -- Get the object id
	set oop [Global recall,oop]

	# If we are in "list mode", don't clobber the text
	if {[string length [Info $oop hook]]} return

	# Get information about the power
	angband mindcraft info $index attrib

	# Get the icon
	### Use the character icon?
	set icon {icon none 0}

	# Color
	set color White

	# Get the name
	set name [format [mc header-magic] $attrib(name)]

	# Get the memory
	set memory ""

	# Extra info
	if {[string length $memory]} {
		append memory \n
	}
	append memory "Level $attrib(level)  Mana $attrib(mana) \
		Fail $attrib(fail)%"
	if {[string length $attrib(comment)]} {
		append memory "\n$attrib(comment)"
	}

	# Set the text
	SetText $oop $icon $color $name $memory

	return
}

# NSRecall::RecallPower --
#
#	Show info about a racial/mutation power.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::RecallPower {index} {

	variable Priv

	if {![Value recall,show]} return

	# Hack -- Get the object id
	set oop [Global recall,oop]

	# If we are in "list mode", don't clobber the text
	if {[Info $oop hook] ne ""} return

	# Get information about the spell
	angband power info $index attrib

	# Get the power icon
	set icon "icon none 0"

	# Color
	set color White

	# Get the name
	set name [format [mc header-magic] $attrib(name)]

	# Get the memory
	set memory ""

	# Extra info
	if {$memory ne ""} {
		append memory \n
	}
	append memory [format [mc memory-power] $attrib(level) $attrib(cost) \
		$attrib(chance) $attrib(stat)]

	# Set the text
	SetText $oop $icon $color $name $memory

	HistoryAdd $oop $attrib(name) RecallPower $index

	return
}

# ZANGBANDTK
}

proc NSRecall::CLR {color text} {
	set color [Value $color]
	return "\$($color)$text\$(/)"
}

proc NSRecall::NewMonsterMemory {r_idx} {

	angband r_info info $r_idx memory2
	if {![info exists MonsterMemoryToArray]} return
	array set a [array get MonsterMemoryToArray]

	set result ""
	append result $a(desc)\n
	if {[info exists a(type)]} {
		append result "[CLR TERM_L_WHITE Type:] [join $a(type) ", "]\n"
	}
	# level shows only in town or if we killed one
	if {[info exists a(level)]} {
		set level "[CLR TERM_L_WHITE Level:] $a(level)  "
	} else {
		set level ""
	}
	# points shows only in town or if we killed one
	if {[info exists a(points)]} {
		set points "  [CLR TERM_L_WHITE Points:] $a(points)"
	} else {
		set points ""
	}
	if {$a(unique)} {
		if {[struct set monster_race $r_idx max_num] > 0} {
			set killed NO
		} else {
			set killed YES
		}
		append result "$level[CLR TERM_L_WHITE Deaths:] $a(deaths)  [CLR TERM_L_WHITE Killed:] $killed$points\n"
	} else {
		append result "$level[CLR TERM_L_WHITE Deaths:] $a(deaths)  [CLR TERM_L_WHITE Kills:] $a(kills)$points\n"
	}
	if {$a(speed) > 110} {
		set speed [format "Fast (+%d)" [expr {$a(speed) - 110}]]
	} elseif {$a(speed) < 110} {
		set speed [format "Slow (-%d)" [expr {110 - $a(speed)}]]
	} else {
		set speed Normal
	}
	if {[info exists a(armor)]} {
		set ac_health "  [CLR TERM_L_WHITE AC:] $a(armor)  [CLR TERM_L_WHITE Health:] $a(health)"
	} else {
		set ac_health ""
	}
	append result "[CLR TERM_L_WHITE Speed:] $speed$ac_health\n"
	if {[info exists a(attacks)]} {
		set string [join $a(attacks) "\n    "]
		append result "[CLR TERM_L_WHITE Attacks:]\n    $string\n"
		if {[info exists a(breath)]} {
			set string [join $a(breath) ", "]
			append result "    breathe $string\n"
		}
	}
	if {[info exists a(spells)]} {
		set string [join $a(spells) "\n    "]
		append result "[CLR TERM_L_WHITE Spells:]\n    $string\n"
	}
	if {[info exists a(frequency)]} {
		if {$a(frequency_known)} {
			set string "1 time in $a(frequency)"
		} else {
			set string "about 1 time in $a(frequency)"
		}
		append result "[CLR TERM_L_WHITE {Breath/Spell Frequency:}] $string\n"
	}
	if {[info exists a(special)]} {
		set string [join $a(special) "\n    "]
		append result "[CLR TERM_L_WHITE Abilities:]\n    $string\n"
	}
	if {[info exists a(immune)]} {
		set string [join $a(immune) "\n    "]
		append result "[CLR TERM_L_WHITE Immune:]\n    $string\n"
	}
	if {[info exists a(resist)]} {
		set string [join $a(resist) ", "]
		append result "[CLR TERM_L_WHITE Resist:] $string\n"
	}
	if {[info exists a(weakness)]} {
		set string [join $a(weakness) ", "]
		append result "[CLR TERM_L_WHITE Weakness:] $string\n"
	}
	if {[info exists a(drop)]} {
		append result "[CLR TERM_L_WHITE Drop:] $a(drop)\n"
	}
	if {[info exists a(random_move)]} {
		append result "[CLR TERM_L_WHITE Movement:] $a(random_move)% erratic\n"
	} elseif {$a(never_move)} {
		append result "[CLR TERM_L_WHITE Movement:] stationary\n"
	}
	return $result
}

# NSRecall::RecallMonster --
#
#	Show info about a monster race.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::RecallMonster {r_idx} {

	variable Priv

	if {![Value recall,show]} return

	# <Track-race> might pass us zero
	if {!$r_idx} return

	# Hack -- Get the object id
	set oop [Global recall,oop]

	# If we are in "list mode", don't clobber the text
	if {[string length [Info $oop hook]]} return

	# Get the memory
	if {[Value recall,newstyle]} {
		set memory [NewMonsterMemory $r_idx]
	} else {
		set memory [angband r_info info $r_idx memory]
	}

	# Don't set the text if it is unchanged... could be bad!
	if {$Priv(icon,valid) && $Priv(icon,to) eq "monster" && ($Priv(icon,toindex) == $r_idx)} {
		if {[string equal $memory [Info $oop monsterMem]]} {
			return
		}
		Info $oop monsterMem $memory
	}

	# Get the icon
	set icon [angband r_info info $r_idx icon]

	# Get the name
	set name [angband r_info info $r_idx name]
	if {[angband r_info info $r_idx unique]} {
		set name [format [mc header-unique] $name]
	} else {
		set name [format [mc header-monster] $name]
	}

	# Get the color
	set color [Value TERM_L_RED]

	# Set the text
	SetText $oop $icon $color $name $memory

	# Remember info about the displayed icon
	set Priv(icon,to) monster
	set Priv(icon,toindex) $r_idx
	set Priv(icon,valid) 1
	set Priv(icon,known) 1

	HistoryAdd $oop [angband r_info info $r_idx name] RecallMonster $r_idx

	return
}

# NSRecall::RecallObject --
#
#	Show info about an object.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::RecallObject {where index args} {

	variable Priv

	if {![Value recall,show]} return

	# Hack -- Get the object id
	set oop [Global recall,oop]

	# If we are in "list mode", don't clobber the text
	if {[string length [Info $oop hook]]} return

	switch -- $where {
		cave {
			set icon [angband o_list info $index icon]
			set tval [angband o_list set $index tval]
			set color [default_tval_to_attr $tval]
			set name [angband o_list info $index desc]
			set memory [angband o_list info $index memory]
			set attrib(k_idx) [angband o_list set $index k_idx]
			set attrib(known) [angband o_list info $index known]
			set attrib(tval) $tval
		}
		equipment -
		floor -
		inventory {
			angband $where info $index attrib

			# Handle non-objects in equipment
			if {$attrib(tval) eq "TV_NONE"} {
				set icon {icon none 0}
				set color White
				set name ""
				set memory ""
			} else {
				set icon $attrib(icon)
				set color [default_tval_to_attr $attrib(tval)]
				set name $attrib(name)
				set memory [angband $where memory $index]
			}
		}
		home {
			set town [lindex $args 0]
			angband home info $town $index attrib
			set icon $attrib(icon)
			set color [default_tval_to_attr $attrib(tval)]
			set name $attrib(name)
			set memory [angband home memory $town $index]
			set attrib(known) 0
		}
		store {
			angband store info $index attrib
			set icon $attrib(icon)
			set color [default_tval_to_attr $attrib(tval)]
			set name $attrib(name)
			set memory [angband store memory $index]
			set attrib(known) 0
		}
	}

	if {$name ne ""} {
		set name [format [mc header-object] $name]
	}

	SetText $oop $icon $color $name $memory

	# Hack -- Show spells in valid spellbooks
	if {[lsearch -exact [angband player spell_book] $attrib(tval)] != -1} {
		set text [Info $oop text]
		$text tag configure unknown -foreground gray70
		$text tag configure study -foreground [Value TERM_L_GREEN]
		$text tag configure untried -foreground [Value TERM_L_BLUE]
		if {$memory ne ""} {
			$text insert end \n
		}
		$text insert end \n
		foreach spell [angband spell find $attrib(k_idx)] {
			angband spell info $attrib(k_idx) $spell sattrib

			# Colorize the spell name (from OAngband)
			switch -- $sattrib(info) {
				unknown {
					set tag unknown

					# The character can learn this spell
					if {[angband player new_spells] && 
						($sattrib(level) <= [angband player level])} {
						set tag study
					}
				}
				untried {
					set tag untried
				}
				default {
					set tag ""
				}
			}
			$text insert end "$sattrib(char)\) " {} $sattrib(name) $tag \n {}
		}
		if {[$text get end-1chars] eq "\n"} {
			$text delete end-1chars
		}
	}

	set Priv(icon,to) object
	set Priv(icon,toindex) $attrib(k_idx)
	set Priv(icon,valid) 1
	set Priv(icon,known) $attrib(known)

	# Mar 7 2009 - Sync scrollbars/arrow
	Configure $oop

	return
}

# NSRecall::RecallObjectKind --
#
#	Show info about an object kind.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::RecallObjectKind {k_idx} {

	variable Priv

	if {![Value recall,show]} return

	# Hack -- Get the object id
	set oop [Global recall,oop]

	# If we are in "list mode", don't clobber the text
	if {[string length [Info $oop hook]]} return

	# Get the icon
	set icon [angband k_info info $k_idx icon]

	# Get the color
	set tval [angband k_info set $k_idx tval]
	set color [default_tval_to_attr $tval]

	# Hack -- Get the name
	set name [angband k_info info $k_idx object_desc_flavor]
	set name [format [mc header-object] $name]

	# Get the memory
	set memory [angband k_info info $k_idx memory]

	# Set the text
	SetText $oop $icon $color $name $memory

	# Hack -- Show spells in valid spellbooks
	if {[lsearch -exact [angband player spell_book] $tval] != -1} {
		set text [Info $oop text]
		$text tag configure unknown -foreground gray70
		$text tag configure study -foreground [Value TERM_L_GREEN]
		$text tag configure untried -foreground [Value TERM_L_BLUE]
		if {$memory ne ""} {
			$text insert end \n
		}
		$text insert end \n
		foreach spell [angband spell find $k_idx] {
			angband spell info $k_idx $spell sattrib

			# Colorize the spell name (from OAngband)
			switch -- $sattrib(info) {
				unknown {
					set tag unknown

					# The character can learn this spell
					if {[angband player new_spells] && 
						($sattrib(level) <= [angband player level])} {
						set tag study
					}
				}
				untried {
					set tag untried
				}
				default {
					set tag ""
				}
			}
			$text insert end "$sattrib(char)\) " {} $sattrib(name) $tag \n {}
		}
		if {[$text get end-1chars] eq "\n"} {
			$text delete end-1chars
		}
	}

	set Priv(icon,to) object
	set Priv(icon,toindex) $k_idx
	set Priv(icon,valid) 1
	set Priv(icon,known) 1

	HistoryAdd $oop [angband k_info info $k_idx object_desc_flavor] RecallObjectKind $k_idx

	# Mar 7 2009 - Sync scrollbars/arrow
	Configure $oop

	return
}

if {[variant KANGBANDTK ZANGBANDTK]} {

# NSRecall::RecallQuest --
#
#	Show info about a quest.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::RecallQuest {q_idx} {

	variable Priv

	if {![Value recall,show]} return

	# Hack -- Get the object id
	set oop [Global recall,oop]

	# If we are in "list mode", don't clobber the text
	if {[string length [Info $oop hook]]} return

	# Get information about this quest
	array set attrib [struct set quest_type $q_idx]

	# Default icon
	set icon "icon none 0"

	# Default color
	set color [Value TERM_YELLOW]

	# Build the name
	set name [format [mc header-quest] $attrib(name) $attrib(level)]

	# Use any monster icon
	if {$attrib(r_idx)} {
		set icon [assign set monster $attrib(r_idx)]
	}

	# Get the memory
	set memory [struct info quest_type $q_idx text]

	if {[variant KANGBANDTK]} {
		# KAngband has some useless "Kill <num> <monsters>" descriptions
		if {$attrib(type) eq "QUEST_TYPE_RANDOM"} {
			set memory ""
		}
	}

	# No memory (QUEST_TYPE_KILL_XXX)
	if {![string length $memory]} {
		if {$attrib(r_idx)} {
			RecallMonster $attrib(r_idx)
			return
		} else {
			set name "" ; # Random quests have no description
		}
	}

	# Set the text
	SetText $oop $icon $color $name $memory

	HistoryAdd $oop $attrib(name) RecallQuest $q_idx

	return
}

# KANGBANDTK, ZANGBANDTK
}

# NSRecall::RecallSpell --
#
#	Show info about a spell.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::RecallSpell {bookNum index} {

	variable Priv

	if {![Value recall,show]} return

	# Hack -- Get the object id
	set oop [Global recall,oop]

	# If we are in "list mode", don't clobber the text
	if {[string length [Info $oop hook]]} return

	# Get information about the spell
	angband spell info $bookNum $index attrib

	# Get the book icon
	set icon [angband k_info info $bookNum icon]

	# Color
	switch -- $attrib(info) {
		unknown {
			set color gray70

			# The character can learn this spell
			if {[angband player new_spells] && 
				($attrib(level) <= [angband player level])} {
				set color [Value TERM_L_GREEN]
			}
		}
		untried {
			set color [Value TERM_L_BLUE]
		}
		default {
			set color White
		}
	}

	# Get the name
	set name [format [mc header-magic] $attrib(name)]

	# Get the memory
	set memory [angband spell memory $bookNum $index]

	# Extra info
	if {[string length $memory]} {
		append memory \n
	}
	append memory [format [mc memory-spell] $attrib(level) $attrib(mana) $attrib(chance)]
	if {[string length $attrib(info)]} {
		append memory "\n$attrib(info)"
	}

	# Set the text
	SetText $oop $icon $color $name $memory

	HistoryAdd $oop $attrib(name) RecallSpell $bookNum $index

	return
}

# NSRecall::RecallStack --
#
#	Show info about a pile of items.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::RecallStack {y x} {

	variable Priv

	if {![Value recall,show]} return

	# Hack -- Get the object id
	set oop [Global recall,oop]

	set win [Info $oop win]
	set text [Info $oop text]

	# If we are in "list mode", don't clobber the text
	if {[string length [Info $oop hook]]} return

	# Get info about this cave location
	angband cave info $y $x attrib

	# Get the object index
	set o_idx $attrib(o_idx)

	# Sanity check
	if {!$o_idx} return

	# Clear the text
	$text delete 1.0 end

	# Set the icon to the pile image
	[Info $oop icon] itemconfigure icon -assign [assign set object 0]

	set index 0

	# Check each object in the stack
	while {$o_idx} {

		# Get info about this object
		array set attrib [angband o_list set $o_idx]

		# The object is marked
		if {$attrib(marked)} {

			# Get the char
			set char [string index "abcdefghijklmnopqrstuvw" $index]

			# Get the name
			set name [angband o_list info $o_idx desc]

			# Get the color
			set color [default_tval_to_attr $attrib(tval)]

			# Append the description
			$text insert end "$char) " {} $name item$char "\n"
			$text tag configure item$char -foreground $color

			incr index
		}

		# Get the next object in the stack
		set o_idx $attrib(next_o_idx)
	}

	if {$index} {
		$text insert 1.0 [format [mc header-pile] $index] TAG_STYLE "\n"
		$text tag configure TAG_STYLE -foreground White
	}

	# Delete any trailing newline
	$text delete "end - 1 chars"

	set Priv(icon,valid) 0

	# Synchronize the indicator arrow and scrollbars
	Configure $oop

	return
}

# NSRecall::SetText --
#
#	Description.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSRecall::SetText {oop icon color title text} {

	global NSRecall
	variable Priv

	set win [Info $oop win]
	set textBox [Info $oop text]

	# If we are in "list mode", then do not set the text. This may
	# happen if we are waiting for an object to be chosen, and the
	# user highlights an object in the Inventory Window, which would
	# ordinarily display the object memory.
	if {[Info $oop hook] ne ""} {
		return
	}

	# Display the icon
	[Info $oop icon] itemconfigure icon -assign $icon

	# Delete
	$textBox delete 1.0 end

	# Insert title if any
	if {[string length $title]} {

		# Title (color?)
		$textBox insert end $title\n
		$textBox tag add TAG_STYLE 1.0 {end -1 chars}
		$textBox tag configure TAG_STYLE -foreground $color
	}

	# Insert text if any
	set text [string trim $text]
if 1 {
	if {[string length $text]} {
		set color ""
		set chunk 0
		while {[regexp -indices {\$\(([a-zA-Z0-9_/#]*)\)} $text ignore subst]} {
			scan $subst "%d %d" first last
			set subst [string range $text $first $last]
			incr first -3
			incr last 2
			if {$subst eq "/"} {
				set substring [string range $text 0 $first]
				if {$color ne ""} {
					$textBox insert end $substring TAG$chunk
					$textBox tag configure TAG$chunk -foreground $color
					incr chunk
				} else {
					$textBox insert end $substring
				}
				set color ""
			} else {
				set substring [string range $text 0 $first]
				$textBox insert end $substring
				set color $subst
			}
			set text [string range $text $last end]
		}
		$textBox insert end $text
	}
} else {
	if {[string length $text]} {
		$textBox insert end $text
	}
}

	# Delete trailing newline
	if {[$textBox index end-1chars] eq "\n"} {
		$textBox delete end-1chars
	}

	set Priv(icon,valid) 0

	# Grow the window if the mouse pointer is in it
	ExpandAgain $oop

	# Synchronize the indicator arrow and scrollbars
	Configure $oop

	return
}

# NSRecall::IconChanged --
#
#	The icon of the recalled monster/object is displayed in the
#	Recall Window. If that monster or object is assigned a different
#	icon, we want to update the display. This is called as a
#	qebind command on the "Assign" quasi-event.
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSRecall::IconChanged {oop to toindex assign} {

	variable Priv

	if {!$Priv(icon,valid)} return
	if {$to eq $Priv(icon,to) && ($toindex == $Priv(icon,toindex))} {
		[Info $oop icon] itemconfigure icon -assign $assign
	}		

	return
}

# NSRecall::DisplayAssign --
#
#	Display the Assign Window for the displayed monster or object.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::DisplayAssign {oop} {

	variable Priv

	if {!$Priv(icon,valid)} return

	# This can't work when an unknown flavored object is displayed
	if {!$Priv(icon,known)} return

	if {$Priv(icon,to) ne "monster" &&
		$Priv(icon,to) ne "object"} {
		return
	}

	NSModule::LoadIfNeeded NSAssign
	NSWindowManager::Display assign $Priv(icon,to) $Priv(icon,toindex)

	return
}

# NSRecall::DisplayKnowledge --
#
#	Display the Knowledge Window for the displayed monster or object.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::DisplayKnowledge {oop} {

	variable Priv

	if {!$Priv(icon,valid)} return

	# This can't work when an unknown flavored object is displayed
	if {!$Priv(icon,known)} return

	if {$Priv(icon,to) ne "monster" &&
		$Priv(icon,to) ne "object"} {
		return
	}

	angband_display knowledge show $Priv(icon,to) $Priv(icon,toindex)

	return
}

# NSRecall::SettingChanged_depth_in_feet --
#
#	Called when the depth_in_feet option changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::SettingChanged_depth_in_feet {oop} {

	variable Priv

	# Hack -- Not a monster/artifact
	if {!$Priv(icon,valid)} return

	# Hack -- Not a monster
	if {$Priv(icon,to) ne "monster"} return

	# Hack -- Redisplay
	RecallMonster $Priv(icon,toindex)

	return
}

# NSRecall::SetHook --
#
#	Set the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::SetHook {oop hook} {

	if {[string length $hook]} {
		Info $oop hook NSRecall::$hook
		CallHook $oop open
		qeconfigure NSRecall <Term-fresh> -active yes
		if {$::DEBUG} {
			set ::debug_display 1
		}
	} elseif {[string length [Info $oop hook]]} {
		Info $oop hook ""
		Restore $oop
		qeconfigure NSRecall <Term-fresh> -active no
		if {$::DEBUG} {
			set ::debug_display 0
		}
	}

	return
}

# NSRecall::CallHook --
#
#	Call the hook.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::CallHook {oop message args} {

	return [uplevel #0 [Info $oop hook] $oop $message $args]
}

# NSRecall::Fresh_Display --
#
#	Calls the hook to set the list, if required. Called as a command
#	on the "Term-fresh" quasi-event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::Fresh_Display {oop} {

	ASSERT {$::debug_display == 1} \
		"Fresh_Display called with debug_display=0!"

	CallHook $oop fresh
if 0 {
	# If the cursor is inside the Recall Window, we will attempt to
	# expand it.
	set pointerx [winfo pointerx .]
	set pointery [winfo pointery .]
	set toplevel [winfo containing $pointerx $pointery]
	if {[string length $toplevel] && \
		[string equal [winfo toplevel $toplevel] [Info $oop win]]} {
		Expand $oop
	}
}
	return
}

# NSRecall::SetList --
#
#	Clears the recall text, sets the icon to "none 0" and calls the
#	hook to set the text.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::SetList {oop} {

	set win [Info $oop win]
	set textBox [Info $oop text]

	# Clear the text
	$textBox delete 1.0 end

	# Clear the icon
	[Info $oop icon] itemconfigure icon -assign {icon none 0}

	# Call the hook to set the list
	CallHook $oop set_list

	# Something is displayed
	Info $oop display something

	# No item is highlighted
	Info $oop current ""	

	# Synchronize the indicator arrow and scrollbars
	Configure $oop

	return
}

# NSRecall::Invoke --
#
#	Called when a list item is clicked. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::Invoke {oop index} {

	set textBox [Info $oop text]
set index [Info $oop current]
	set row [expr {[lindex [split $index .] 0] - 1}]

	CallHook $oop invoke $row

	return
}

# NSRecall::Motion --
#
#	Called when the mouse moves in a list item. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::Motion {oop index} {

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

# NSRecall::HighlightItem --
#
#	Highlights a list item. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::HighlightItem {oop index} {

	set textBox [Info $oop text]
	set row [expr {[lindex [split $index .] 0] - 1}]

	# Highlight the item
	$textBox tag add HOT $index "$index lineend"
	$textBox tag raise HOT

	# Call the hook (to set the icon, for example)
	CallHook $oop highlight $row

	return
}

# NSRecall::UnhighlightItem --
#
#	Removes highlighting from a list item. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::UnhighlightItem {oop index} {

	set win [Info $oop win]
	set textBox [Info $oop text]

	# Unhighlight the item
	$textBox tag remove HOT 1.0 end

	# Clear the icon
	[Info $oop icon] itemconfigure icon -assign {icon none 0}

	return
}

# NSRecall::HasCursor --
#
#	See if the cursor is over the window. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::HasCursor {oop} {

	return [NSUtils::HasCursor [Info $oop win] 1]
}

# NSRecall::Expand --
#
#	Resizes the Recall Window to display all of the information in it.
#	Does nothing if the window is already expanded. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::Expand {oop} {

	variable Priv

	# Option: expand/contract
	if {![Info $oop autoExpand]} return

#	if {![string length [Info $oop hook]]} return
	if {[Info $oop busy]} return
	if {[Info $oop expanded]} return

	set win [Info $oop win]
	set textBox [Info $oop text]

	set textHeight [winfo height $textBox]

if 1 {
	# Tk 8.5 goodness
	set height [$textBox count -update -ypixels 1.0 end]
	incr height [expr {[$textBox cget -pady] * 2}]
	incr height 2 ; # HACK -- needed but why?
} else {
	set lineHeight [font metrics [Value font,recall] -linespace]

	# Hack -- In order to find out how much space is taken up by the
	# text in the text widget, I create a canvas text item with the
	# proper attributes and calculate its size. The width is width-8
	# and height-4 because of the internal padding of the text
	# widget. I added 2 to each adjustment as a hack.
	set padx [$textBox cget -padx]
	set pady [$textBox cget -pady]
	set itemId [[Info $oop icon] create text 1 1 -font [Value font,recall] \
		-width [expr {[winfo width $textBox] - $padx * 2 - 1}] -anchor nw \
		-text [$textBox get 1.0 end]]
	set bbox [[Info $oop icon] bbox $itemId]
	set height [expr {[lindex $bbox 3] - [lindex $bbox 1] + $pady * 2 + 2}]

	# Hmmm... Is there a trailing newline, or what?
	incr height -$lineHeight

	# Delete the temp canvas item
	[Info $oop icon] delete $itemId
}
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

	Info $oop expanded 1
	Info $oop busy 0

	# If the cursor moved outside the Recall Window, collapse it
	if {![HasCursor $oop]} {
		Contract $oop
	}

	return
}

# NSRecall::Contract --
#
#	Restores the window geometry to the size it was before it was
#	expanded. Does nothing if the window is not expanded. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::Contract {oop} {

	if {[Info $oop busy]} return
	if {![Info $oop expanded]} return

	Info $oop busy 1

	set win [Info $oop win]
	wm geometry $win [Info $oop geometry]
	update

	Info $oop expanded 0
	Info $oop busy 0

	return
}

# NSRecall::Restore --
#
#	Description. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::Restore {oop} {

	if {![string length [Info $oop display]]} return
	SetText $oop {icon none 0} {} {} {}
	Contract $oop
	Info $oop display ""

	return
}

# NSRecall::ContextMenu --
#
#	When the window is right-clicked, pop up a menu of options.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::ContextMenu {oop menu x y} {

	set text [Info $oop text]

	$menu delete 0 end

	if {[llength [Info $oop history,desc]]} {
		$menu add cascade -label [mc History] -menu $menu.history
		$menu.history delete 0 end
		foreach desc [Info $oop history,desc] args [Info $oop history,args] {
			$menu.history add command -label $desc \
				-command "NSRecall::Info $oop history,ignore 1 ; NSRecall::$args ; NSRecall::Info $oop history,ignore 0"
		}
	}

	$menu add checkbutton -label [mc "Auto Expand"] \
		-variable NSRecall($oop,autoExpand) \
		-command "NSRecall::OptionChanged $oop autoExpand autoexpand"
	$menu add checkbutton -label [mc "Recall 2.0"] \
		-variable NSRecall($oop,newStyle) \
		-command "NSRecall::OptionChanged $oop newStyle newstyle"
	$menu add command -label [mc "Set Font"] \
		-command "NSModule::LoadIfNeeded NSFont ; NSWindowManager::Display font recall"
	$menu add checkbutton -label [mc "Show Icon"] \
		-variable ::NSRecall($oop,showIcon) \
		-command "NSRecall::OptionChanged $oop showIcon showicon"
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

# NSRecall::ExpandAgain --
#
#	Resize the already-expanded window if needed.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::ExpandAgain {oop} {

	# Resize the window if needed
	if {[Info $oop expanded]} {

		set win [Info $oop win]
		set textBox [Info $oop text]

		set textHeight [winfo height $textBox]

		# Tk 8.5 goodness
		set height [$textBox count -update -ypixels 1.0 end]
		incr height [expr {[$textBox cget -pady] * 2}]
		incr height 2 ; # HACK -- needed but why?

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

# NSRecall::OptionChanged --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::OptionChanged {oop info keyword} {

	variable Priv

	set setting [Info $oop $info]
	Value recall,$keyword $setting
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
		newstyle {
			if {$Priv(icon,to) eq "monster" && [Info $oop hook] eq ""} {
				RecallMonster $Priv(icon,toindex)
			}
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

# NSRecall::Configure --
#
#	Positions the indicator arrow (the one which tells us if there is
#	more information out of sight) near the bottom of the window.  Also
#	displays scrollbars as needed.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::Configure {oop} {

	if {[Info $oop inConfigure]} return
	if {[Info $oop afterId,configure] ne ""} return

	set afterId [after idle "NSRecall::ConfigureAux $oop"]
	Info $oop afterId,configure $afterId

	return
}

proc NSRecall::ConfigureAux {oop} {

	# Lottsa foolishness here
	if {[Info $oop inConfigure]} return
	Info $oop inConfigure 1

	Info $oop afterId,configure ""

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
			if {[Info $oop expanded] || [Info $oop autoExpand]} {
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
			if {[Info $oop expanded] || [Info $oop autoExpand]} {
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
		if {[Info $oop autoExpand] && ![ContentVisible $text yview]} {
			set fill Red
			$canvas itemconfigure arrow -fill $fill -outline $fill
		}
	}

	# July 6 2004
#	ContentChanged $oop

	Info $oop inConfigure 0

	return
}

# NSRecall::ContentChanged --
#
#	Called when the information displayed has changed. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.
# NOT USED - July 6 2004
proc NSRecall::ContentChanged {oop} {

	set win [Info $oop win]
	set canvas [Info $oop icon]
	set text [Info $oop text]

	scan [$text yview] "%f %f" top bottom
	if {$bottom < 1} {
		set fill Red
	} else {
		set fill [$canvas cget -background]
	}

	$canvas itemconfigure arrow -fill $fill -outline $fill

	return
}

# NSRecall::Choose --
#
#	Handle <Choose> quasi-event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::Choose {oop what show args} {

	if {[lsearch -exact [list cmd_pet ele_attack item mindcraft power spell] \
		$what] == -1} return

	if {!$show} {
		SetHook $oop {}
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

proc NSRecall::hook_item {oop message args} {

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

			# Get the list of matching item indexes
			set invOrEquip [Info $oop display,what]
			set itemList [angband $invOrEquip find -tester yes]

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
				$textBox insert end "$attrib(char)\) " TEXT \
					$attrib(name) [list ITEM_$index TEXT] "\n"
				$textBox tag configure ITEM_$index -foreground $color
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"

			# Keep a list of inventory indexes
			Info $oop match $itemList
		}

		invoke {

			# Don't invoke when verifying the choice
			if {[angband inkey_flags] ne "INKEY_ITEM"} return

			set row [lindex $args 0]
			set index [lindex [Info $oop match] $row]
			if {[Info $oop display,what] eq "floor"} {
				set attrib(char) [string index "abcdefghijklmnopqrstuvw" $row]
			} else {
				angband [Info $oop display,what] info $index attrib
			}
			angband keypress $attrib(char)
		}

		highlight {

			set row [lindex $args 0]
			set index [lindex [Info $oop match] $row]
# Weird bug reports from people
if {$index eq ""} return
			angband [Info $oop display,what] info $index attrib
			[Info $oop icon] itemconfigure icon -assign $attrib(icon)
		}

		menu_select {
			set menu [lindex $args 0]
			set index [lindex $args 1]
			set match $::Popup(match,$menu)
			set where $::Popup(where,$menu)
			if {$index < [llength $match]} {
				RecallObject $where [lindex $match $index]
			}
		}
	}

	return
}

proc NSRecall::MenuSelect {menu hook} {

	set index [$menu index active]
	eval $hook [Global recall,oop] menu_select $menu $index

	return
}

# NSRecall::PopupSelect_Item --
#
#	Show a pop-up menu of inventory/equipment/floor choices.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::PopupSelect_Item {menu x y} {

	global PopupResult

	set PopupResult 0

	# Hack -- Turn off show_flavors
	set setting [Setting show_flavors]
	Setting show_flavors no

	set invOrEquip [angband inkey_other]
	$menu delete 0 end
	set num 0
	set match {}
	foreach item [angband $invOrEquip find -tester yes] {
		angband $invOrEquip info $item attrib
		if {$invOrEquip eq "floor"} {
			set attrib(char) [string index "abcdefghijklmnopqrstuvw" $num]
		}
		set char $attrib(char)
		$menu add command -label "$char $attrib(name)" \
			-command "angband keypress $char ; set PopupResult 1" \
			-underline 0
		incr num

		lappend match $item
	}

	set ::Popup(match,$menu) $match
	set ::Popup(where,$menu) $invOrEquip

	if {$num} {$menu add separator}
	$menu add command -label [mc Cancel]

	# Hack -- Restore show_flavors
	Setting show_flavors $setting

	# Pressing and holding Button-3, popping up, then letting go selects 
	# an item, so wait a bit if it was a quick press-release
	after 100

	set script [bind $menu <<MenuSelect>>]
	bind $menu <<MenuSelect>> "NSRecall::MenuSelect $menu hook_item"

	tk_popup $menu $x $y [expr {$num / 2}]


	if {[Platform unix]} {
		tkwait variable ::tk::Priv(popup)
	}

	bind $menu <<MenuSelect>> $script

	# If the user unposts the menu without choosing an entry, then
	# I want to feed Escape into the Term. I tried binding to the <Unmap>
	# event but it isn't called on Windows(TM).
	after idle {
		if {!$PopupResult} {
			angband keypress \033
		}
	}

	return
}

# NSRecall::PopupSelect_Use --
#
#	Show a heirarchical pop-up menu of inventory items we can use.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::PopupSelect_Use {menu x y} {

	$menu delete 0 end
	destroy {*}[winfo children $menu]

	# Hack -- Turn off show_flavors
	set setting [Setting show_flavors]
	Setting show_flavors no

	set num 0

	set data {
		equipment "-activate yes" Activate A
		inventory "-tval TV_FOOD" Food E
		inventory "-tval TV_POTION" Potion q
		inventory "-tval TV_SCROLL" Scroll r
		inventory "-tval TV_ROD" Rod z
		inventory "-tval TV_WAND" Wand a
		inventory "-tval TV_STAFF" Staff u
	}

	foreach {invOrEquip criteria label cmdChar} $data {
		set itemList [angband $invOrEquip find {*}$criteria]
		if {[llength $itemList]} {
			set menu2 $menu.menu$num
			menu $menu2 -tearoff 0
			$menu add cascade -menu $menu2 -label $label
			foreach item $itemList {
				angband $invOrEquip info $item attrib
				$menu2 add command -label "$attrib(char)) $attrib(name)" \
					-command "DoUnderlyingCommand $cmdChar$attrib(char)"
			}
			incr num
bind $menu2 <<MenuSelect>> "NSRecall::MenuSelect $menu2 hook_item"
set ::Popup(match,$menu2) $itemList
set ::Popup(where,$menu2) $invOrEquip
		}
	}

	if {$num} {
		$menu add separator
	}
	$menu add command -label [mc Cancel]

	# Hack -- Restore show_flavors
	Setting show_flavors $setting

	# Pressing and holding Button-3, popping up, then letting go selects 
	# an item, so wait a bit if it was a quick press-release
	after 100

	tk_popup $menu $x $y [expr {$num / 2}]

	return
}

if {[variant KANGBANDTK ZANGBANDTK]} {

proc NSRecall::PetCmdInfo {_mode} {

	upvar $_mode mode

	set letters abcdefgh
	set index -1

	if {[llength [angband player pets]]} {
		set char [string index $letters [incr index]]
		lappend data $char [mc "Dismiss pets"]
	}

	set dist [struct set player_type 0 pet_follow_distance]
dbwin "dist is $dist\n"
set mode ""

	set char [string index $letters [incr index]]
	lappend data $char [mc "Stay close"]
	if {$dist == [const PET_CLOSE_DIST]} {
		set mode $char
	}
	set char [string index $letters [incr index]]
	lappend data $char [mc "Follow me"]
	if {$dist == [const PET_FOLLOW_DIST]} {
		set mode $char
	}
	set char [string index $letters [incr index]]
	lappend data $char [mc "Seek and destroy"]
	if {$dist == [const PET_DESTROY_DIST]} {
		set mode $char
	}
	set char [string index $letters [incr index]]
	lappend data $char [mc "Give me space"]
	if {$dist == [const PET_SPACE_DIST]} {
		set mode $char
	}
	set char [string index $letters [incr index]]
	lappend data $char [mc "Stay away"]
	if {$dist == [const PET_AWAY_DIST]} {
		set mode $char
	}

	set char [string index $letters [incr index]]
	if {[struct set player_type 0 pet_open_doors]} {
		lappend data $char [mc "Disallow open doors"]
	} else {
		lappend data $char [mc "Allow open doors"]
	}

	set char [string index $letters [incr index]]
	if {[struct set player_type 0 pet_pickup_items]} {
		lappend data $char [mc "Disallow pickup items"]
	} else {
		lappend data $char [mc "Allow pickup items"]
	}

	return $data
}

proc NSRecall::hook_cmd_pet {oop message args} {

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
			foreach {char label} [PetCmdInfo mode] {

				if {$char eq $mode} {
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

		invoke {
			set row [lindex $args 0]
			set char [lindex [Info $oop match] $row]
			angband keypress $char
		}

		highlight {
		}
	}

	return
}

# NSRecall::PopupSelect_CmdPet --
#
#	Show a pop-up menu of pet commands.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::PopupSelect_CmdPet {menu x y} {

	global PopupResult

	set PopupResult 0

	# Clear the menu
	$menu delete 0 end

	set num 0
	foreach {char name} [PetCmdInfo mode] {

		if {$char eq $mode} {
			set ::PopupCheck 1
			$menu add checkbutton -label "$char $name" \
				-command "angband keypress $char ; set PopupResult 1" \
				-underline 0 -variable ::PopupCheck
		} else {
			$menu add command -label "$char $name" \
				-command "angband keypress $char ; set PopupResult 1" \
				-underline 0
		}

		incr num
	}

	$menu add separator
	$menu add command -label [mc Cancel]

	# Pressing and holding Button-3, popping up, then letting go selects 
	# an item, so wait a bit if it was a quick press-release
	after 100

	tk_popup $menu $x $y [expr {$num / 2}]

	if {[Platform unix]} {
		tkwait variable ::tk::Priv(popup)
	}

	# If the user unposts the menu without choosing an entry, then
	# I want to feed Escape into the Term. I tried binding to the <Unmap>
	# event but it isn't called on Windows(TM).
	after idle {
		if {!$PopupResult} {
			angband keypress \033
		}
	}

	return
}

proc NSRecall::hook_mindcraft {oop message args} {

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

			# Get the list of mindcraft powers
			set powerList [angband mindcraft get]

			# Keep a list of chars
			set match {}

			# Process each power
			foreach power $powerList {

				angband mindcraft info $power attrib
				if {!$attrib(okay)} continue

				# Append the character and description
				$textBox insert end "$attrib(char)\) " TEXT \
					$attrib(name) [list POWER_$attrib(char) TEXT] "\n"
				$textBox tag configure POWER_$attrib(char) -foreground White

				# Keep a list of chars
				lappend match $attrib(char)
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"

			# Keep a list of chars
			Info $oop match $match
		}

		invoke {

			set row [lindex $args 0]
			set char [lindex [Info $oop match] $row]
			angband keypress $char
		}

		highlight {
		}
	}

	return
}

proc NSRecall::hook_power {oop message args} {

	set powerChars "abcdefghijklmnopqrstuvwxyz0123456789"

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
				$textBox insert end "$attrib(char)\) " TEXT \
					$attrib(name) [list POWER_$attrib(char) TEXT] "\n"
				$textBox tag configure POWER_$attrib(char) -foreground $fill

				incr i
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"

			# Keep a list of inventory indexes
			Info $oop match $powerList
		}

		invoke {

			set row [lindex $args 0]
			angband keypress [string index $powerChars $row]
		}

		highlight {
		}
	}

	return
}

# NSRecall::PopupSelect_Power --
#
#	Show a pop-up menu of power choices.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::PopupSelect_Power {menu x y} {

	global PopupResult

	set PopupResult 0

	# Clear the menu
	$menu delete 0 end

	set powerChars "abcdefghijklmnopqrstuvwxyz0123456789"

	set num 0
	foreach power [angband power get] {

		# Get information about the power
		angband power info $power attrib

		# Get the power char
		set attrib(char) [string index $powerChars $num]

		# Append menu entry
		$menu add command -label "$attrib(char) $attrib(name)" \
			-command "angband keypress $attrib(char) ; set PopupResult 1" \
			-underline 0

		# Count the number of powers added to the menu
		incr num
	}

	if {$num} {
		$menu add separator
	}
	$menu add command -label [mc Cancel]

	# Pressing and holding Button-3, popping up, then letting go selects 
	# an item, so wait a bit if it was a quick press-release
	after 100

	tk_popup $menu $x $y [expr {$num / 2}]

	if {[Platform unix]} {
		tkwait variable ::tk::Priv(popup)
	}

	# If the user unposts the menu without choosing an entry, then
	# I want to feed Escape into the Term. I tried binding to the <Unmap>
	# event but it isn't called on Windows(TM).
	after idle {
		if {!$PopupResult} {
			angband keypress \033
		}
	}

	return
}

# ZANGBANDTK
}

proc NSRecall::hook_spell {oop message args} {

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

			# Get the book number
			set bookNum [Info $oop display,what]

			# Get a list of legal spells
			set spellList [angband spell find $bookNum -tester yes]

			# Keep a list of spell chars
			set match {}

			# Process each spell
			foreach spell $spellList {

				# Get information about this spell
				angband spell info $bookNum $spell attrib

				# Append the character and description
				$textBox insert end "$attrib(char)\) " TEXT \
					$attrib(name) [list SPELL_$attrib(char) TEXT] "\n"
				$textBox tag configure SPELL_$attrib(char) -foreground White

				# Keep a list of spell chars
				lappend match $attrib(char)
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"

			# Keep a list of spell chars
			Info $oop match $match
		}

		invoke {

			set row [lindex $args 0]
			set char [lindex [Info $oop match] $row]
			angband keypress $char
		}

		highlight {
		}

		menu_select {
			set menu [lindex $args 0]
			set index [lindex $args 1]
			set bookNum $::Popup(book,$menu)
			set match $::Popup(match,$menu)
			if {$index < [llength $match]} {
				RecallSpell $bookNum [lindex $match $index]
			}
		}
	}

	return
}

if {[variant OANGBANDTK]} {

proc NSRecall::hook_ele_attack {oop message args} {

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
				$textBox insert end "$char\) " TEXT $attack \
					[list POWER_$char TEXT] "\n"
				$textBox tag configure POWER_$char -foreground $color

				# Keep a list of chars
				lappend match $char
			}

			# Delete trailing newline
			$textBox delete "end - 1 chars"

			# Keep a list of chars
			Info $oop match $match
		}

		invoke {

			set row [lindex $args 0]
			set char [lindex [Info $oop match] $row]
			angband keypress $char
		}

		highlight {
		}
	}

	return
}

# NSRecall::PopupSelect_EleAttack --
#
#	Show a pop-up menu of elemental attack choices.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::PopupSelect_EleAttack {menu x y} {

	global PopupResult

	set PopupResult 0

	# Clear the menu
	$menu delete 0 end

	set data {
		a Fire
		b Cold
		c Acid
		d Electricity
	}

	# This bit of code must match that in cmd5.c
	set num [expr {([angband player level] - 20) / 7}]
	if {$num <= 0} {
		set num 1
	}
	set data [lrange $data 0 [expr {$num * 2 - 1}]]

	set num 0
	foreach {char name} $data {

		# Append menu entry
		$menu add command -label "$char $name" \
			-command "angband keypress $char ; set PopupResult 1" \
			-underline 0

		# Count the number of powers added to the menu
		incr num
	}

	if {$num} {
		$menu add separator
	}
	$menu add command -label [mc Cancel]

	# Pressing and holding Button-3, popping up, then letting go selects 
	# an item, so wait a bit if it was a quick press-release
	after 100

	tk_popup $menu $x $y [expr {$num / 2}]

	if {[Platform unix]} {
		tkwait variable ::tk::Priv(popup)
	}

	# If the user unposts the menu without choosing an entry, then
	# I want to feed Escape into the Term. I tried binding to the <Unmap>
	# event but it isn't called on Windows(TM).
	after idle {
		if {!$PopupResult} {
			angband keypress \033
		}
	}

	return
}

# OANGBANDTK 040
}

proc NSRecall::hook_xxx {oop message args} {

	switch -- $message {

		set_list {
		}
	}

	return
}

# NSRecall::ValueChanged_font_recall --
#
#	Called when the font,recall value changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::ValueChanged_font_recall {oop} {

	[Info $oop text] configure -font [Value font,recall]

	return
}

# NSRecall::HistoryAdd --
#
#	Add to display history.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::HistoryAdd {oop desc args} {

	if {[Info $oop history,ignore]} return

	set descList {}
	set argsList {}
	foreach desc1 [Info $oop history,desc] args1 [Info $oop history,args] {
		if {($desc ne $desc1) || ($args ne $args1)} {
			lappend argsList $args1
			lappend descList $desc1
		}
	}

	lappend descList $desc
	lappend argsList $args

	Info $oop history,desc [lrange $descList end-19 end] 
	Info $oop history,args [lrange $argsList end-19 end] 

	return
}

# NSRecall::MenuSelectHistory --
#
#	Handle <<MenuSelect>> in the history context menu.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::MenuSelectHistory {oop menu} {

	set index [$menu index active]
	if {$index eq "none"} return

	NSRecall::Info $oop history,ignore 1
	eval [lindex [Info $oop history,args] $index]
	NSRecall::Info $oop history,ignore 0

	return
}

# NSRecall::IconCfg --
#
#	Called when the game's icon configuration changes
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSRecall::IconCfg {oop} {

	set canvas [Info $oop icon]

	set iconWidth [expr {[icon width] + 8}]
	$canvas configure -width $iconWidth

	$canvas coords focus 4 4 [expr {6 + [icon width] + 1}] \
		[expr {6 + [icon height] + 1}]

	set x [expr {$iconWidth / 2}]
	$canvas coords arrow [expr {$x - 3}] 46 [expr {$x + 3}] 46 $x 49

	Configure $oop

	return
}
