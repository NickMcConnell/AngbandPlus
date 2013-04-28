# File: charflags-canvas.tcl

# Purpose: the Character Flags canvas and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSCharFlagsCanvas {

	variable Priv

# namespace eval NSCharFlagsCanvas
}

# NSCharFlagsCanvas::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharFlagsCanvas::InitModule {} {

	variable Priv

	MsgCatInit charflags

	NSModule::LoadIfNeeded NSBalloon

	set Priv(slots) [list \
		INVEN_WIELD \
		INVEN_BOW \
		INVEN_LEFT \
		INVEN_RIGHT \
		INVEN_NECK \
		INVEN_LITE \
		INVEN_BODY \
		INVEN_OUTER \
		INVEN_ARM \
		INVEN_HEAD \
		INVEN_HANDS \
		INVEN_FEET \
	]

	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		set Priv(flags) [list \
			STR \
			INT \
			WIS \
			DEX \
			CON \
			CHR \
			SUST_STR \
			SUST_INT \
			SUST_WIS \
			SUST_DEX \
			SUST_CON \
			SUST_CHR \
			IM_ACID \
			IM_ELEC \
			IM_FIRE \
			IM_COLD \
			RES_ACID \
			RES_ELEC \
			RES_FIRE \
			RES_COLD \
			RES_POIS \
			RES_FEAR \
			RES_LITE \
			RES_DARK \
			RES_BLIND \
			RES_CONFU \
			RES_SOUND \
			RES_SHARD \
			RES_NEXUS \
			RES_NETHR \
			RES_CHAOS \
			RES_DISEN \
			SLOW_DIGEST \
			FEATHER \
			LITE \
			REGEN \
			TELEPATHY \
			SEE_INVIS \
			FREE_ACT \
			HOLD_LIFE \
			STEALTH \
			SEARCH \
			INFRA \
			TUNNEL \
			SPEED \
			BLOWS \
			SHOTS \
			MIGHT \
			SLAY_ANIMAL \
			SLAY_EVIL \
			SLAY_UNDEAD \
			SLAY_DEMON \
			SLAY_ORC \
			SLAY_TROLL \
			SLAY_GIANT \
			SLAY_DRAGON \
			KILL_DRAGON \
			BRAND_ACID \
			BRAND_ELEC \
			BRAND_FIRE \
			BRAND_COLD \
			IMPACT \
			TELEPORT \
			AGGRAVATE \
			DRAIN_EXP \
			BLESSED \
			LIGHT_CURSE \
			HEAVY_CURSE \
			PERMA_CURSE \
		]
	# ANGBANDTK KANGBANDTK
	}

	if {[variant ZANGBANDTK]} {
		set Priv(flags) [list \
			STR \
			INT \
			WIS \
			DEX \
			CON \
			CHR \
			SUST_STR \
			SUST_INT \
			SUST_WIS \
			SUST_DEX \
			SUST_CON \
			SUST_CHR \
			IM_ACID \
			IM_ELEC \
			IM_FIRE \
			IM_COLD \
			RACE_IM_CUT \
			RACE_IM_DARK \
			RACE_IM_STUN \
			RES_ACID \
			RES_ELEC \
			RES_FIRE \
			RES_COLD \
			RES_POIS \
			RES_FEAR \
			RES_LITE \
			RES_DARK \
			RES_BLIND \
			RES_CONF \
			RES_SOUND \
			RES_SHARDS \
			RES_NEXUS \
			RES_NETHER \
			RES_CHAOS \
			RES_DISEN \
			RACE_RES_SANITY \
			SLOW_DIGEST \
			FEATHER \
			LITE \
			REGEN \
			TELEPATHY \
			SEE_INVIS \
			REFLECT \
			FREE_ACT \
			HOLD_LIFE \
			RACE_EAT_NETHER \
			STEALTH \
			SEARCH \
			INFRA \
			TUNNEL \
			SPEED \
			BLOWS \
			XTRA_SHOTS \
			XTRA_MIGHT \
			SLAY_ANIMAL \
			SLAY_EVIL \
			SLAY_UNDEAD \
			SLAY_DEMON \
			SLAY_ORC \
			SLAY_TROLL \
			SLAY_GIANT \
			SLAY_DRAGON \
			KILL_DRAGON \
			BRAND_POIS \
			BRAND_ACID \
			BRAND_ELEC \
			BRAND_FIRE \
			BRAND_COLD \
			SH_FIRE \
			SH_ELEC \
			VORPAL \
			IMPACT \
			CHAOTIC \
			VAMPIRIC \
			WRAITH \
			NO_MAGIC \
			TELEPORT \
			NO_TELE \
			AGGRAVATE \
			DRAIN_EXP \
			BLESSED \
			CURSED \
			HEAVY_CURSE \
			PERMA_CURSE \
			TY_CURSE \
		]
	# ZANGBANDTK
	}

	set font [Value font,knowledge]
	set lineHgt [font metrics $font -linespace]

	set Priv(font,font) $font
	set Priv(font,height) $lineHgt

	return
}

# NSCharFlagsCanvas::NSCharFlagsCanvas --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharFlagsCanvas::NSCharFlagsCanvas {oop parent} {

	variable Priv

	set canvas $parent.header
	set width 100
	set height 40
	canvas $canvas \
		-scrollregion "0 0 $width $height" -width $width -height $height \
		-relief flat -highlightthickness 0 -background gray40

	Info $oop header,canvas $canvas

	set lineHgt $Priv(font,height)

	set canvas $parent.canvas
	set width 100
	set height [expr {$lineHgt * 22}]
	canvas $canvas \
		-scrollregion "0 0 $width [expr {($lineHgt + 3) * 32}]" \
		-width $width -height $height -yscrollincrement [expr {$lineHgt + 3}] \
		-highlightthickness 0 -background #000022 \
		-yscrollcommand "$parent.yscroll set" \
		-xscrollcommand "$parent.xscroll set"
	scrollbar $parent.yscroll \
		-orient vertical -command "$canvas yview"
	scrollbar $parent.xscroll \
		-orient horizontal -command "$canvas xview"

	bind $canvas <Configure> \
		"+NSCharFlagsCanvas::Configure $oop"
	bind $canvas <Map> \
		"+NSCharFlagsCanvas::Configure $oop"
#	bind $parent.yscroll <Map> \
#		"eval %W set \[$canvas yview]"

	bind $canvas <MouseWheel> {
		%W yview scroll [expr {- (%D / 120) * 4}] units
	}

	NSUtils::SynchScrollBar $canvas $parent.yscroll
		
	Info $oop canvas $canvas
	Info $oop frame $parent

	# So it is drawn first
	raise $parent.header

	grid rowconfigure $parent 0 -weight 0
	grid rowconfigure $parent 1 -weight 1
	grid rowconfigure $parent 2 -weight 0
	grid columnconfigure $parent 0 -weight 1
	grid columnconfigure $parent 1 -weight 0

	grid $parent.header \
		-row 0 -column 0 -rowspan 1 -columnspan 2 -sticky ew
	grid $parent.canvas \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $parent.yscroll \
		-row 1 -column 1 -rowspan 1 -columnspan 1 -sticky ns
	grid $parent.xscroll \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid remove $parent.yscroll
	grid remove $parent.xscroll
	Info $oop scrollbar,vert 0
	Info $oop scrollbar,horz 0

	qebind $canvas <Value-font,knowledge> \
		"NSCharFlagsCanvas::ValueChanged_font_knowledge $oop"

	NSUtils::DestroyObjectWithWidget NSCharFlagsCanvas $oop $canvas

	InitLayout $oop

	return
}

# NSCharFlagsCanvas::~NSCharFlagsCanvas --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharFlagsCanvas::~NSCharFlagsCanvas {oop} {

	# bindings are deleted with canvas

	return
}

# NSCharFlagsCanvas::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharFlagsCanvas::Info {oop info args} {

	global NSCharFlagsCanvas

	# Verify the object
	NSObject::CheckObject NSCharFlagsCanvas $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSCharFlagsCanvas($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSCharFlagsCanvas($oop,$info)
			}
		}
	}

	return
}

# NSCharFlagsCanvas::InitLayout --
#
#	Create all the canvas items.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharFlagsCanvas::InitLayout {oop} {

	variable Priv

	set canvas [Info $oop header,canvas]

	# Calculate the width of the row labels
	# Width = Text Width + 2 pixels + 2 * 3
	set labelWidth 0
	set labels {}
	foreach flag $Priv(flags) {
		set label [mc $flag]
		set width [font measure $Priv(font,font) $label]
		if {$width > $labelWidth} {
			set labelWidth $width
		}
		lappend labels $label
	}
	incr labelWidth [expr {2 + (2 * 3)}]
	if {$labelWidth < 100} {
		set labelWidth 100
	}

	# Calculate the width of the canvas
	set width [expr {$labelWidth + (32 + 2) * ([llength $Priv(slots)] + 1)}]
	$canvas configure -width [expr {$width + 16}]

	# One icon per equipment slot
	set x [expr {$labelWidth + 16}]
	set y 20
	foreach slot $Priv(slots) {
		$canvas create widget $x $y -anchor center \
			-tags $slot
		$canvas bind $slot <Enter> \
			"NSCharFlagsCanvas::StatusBar_Slot $oop $slot"
		$canvas bind $slot <Leave> \
			"\[NSCharFlagsCanvas::Info $oop statusBar] itemconfigure t1 -text {}"

if 0 {
		# Focus ring (really only one is needed)
		set x2 [expr {$x - [icon width] / 2 - 2}]
		set y2 [expr {$y - [icon height] / 2 - 2}]
		$canvas create rectangle $x2 $y2 [expr {$x2 + [icon width] + 4}] \
			[expr {$y2 + [icon height] + 4}] -outline Gray60 -fill "" \
			-tags $slot,focus
}

		incr x [expr {32 + 2}]
	}

	# Character icon
	$canvas create widget $x $y -anchor center -tags py
	$canvas bind py <Enter> \
		"NSCharFlagsCanvas::StatusBar_Player $oop"
	$canvas bind py <Leave> \
		"\[NSCharFlagsCanvas::Info $oop statusBar] itemconfigure t1 -text {}"

	NSBalloon::Delay $canvas 10

	set canvas [Info $oop canvas]
	$canvas configure -width $width

	set x 2 ; set y 0
	foreach flag $Priv(flags) label $labels {

		$canvas create rectangle $x $y [expr {$labelWidth - 2}] \
			[expr {$y + $Priv(font,height) + 2}] -fill gray40 -tags $flag
		$canvas create rectangle $labelWidth $y $width \
			[expr {$y + $Priv(font,height) + 2}] -fill "" -outline "" \
			-tags "rect $flag $flag,rect"
		$canvas create text [expr {$labelWidth / 2}] [expr {$y + 1}] -anchor n \
			-text $label -font $Priv(font,font) -fill White \
			-tags [list $flag $flag,text text]

		$canvas bind $flag <Enter> \
			"%W itemconfigure $flag,rect -fill #333366 -outline #333366"
		$canvas bind $flag <Leave> \
			"%W itemconfigure $flag,rect -fill {} -outline {}"

		# Dot for each slot
		set x2 [expr {$labelWidth + 16}]
		foreach slot $Priv(slots) {
			$canvas create oval [expr {$x2 - 2}] [expr {$y + 9 - 2}] \
				[expr {$x2 + 2}] [expr {$y + 9 + 2}] -fill "" -outline "" \
				-tags "$flag,$slot $flag $flag,dot dot"
			incr x2 34
		}

		# Dot for character
		$canvas create oval [expr {$x2 - 2}] [expr {$y + 9 - 2}] \
			[expr {$x2 + 2}] [expr {$y + 9 + 2}] -fill "" -outline "" \
			-tags "$flag,py $flag $flag,dot dot"
		
		incr y [expr {$Priv(font,height) + 3}]
	}

	return
}

# NSCharFlagsCanvas::SetInfo --
#
#	Set text of character-specific items.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharFlagsCanvas::SetInfo {oop} {

	variable Priv

	set header [Info $oop header,canvas]
	set canvas [Info $oop canvas]

	set rowHgt [expr {$Priv(font,height) + 3}]
	
	$canvas itemconfigure dot -fill "" -outline ""
	$canvas itemconfigure text -fill White
	foreach flag $Priv(flags) {
		set visible($flag) 0
	}
	set y [expr {$rowHgt * [llength $Priv(flags)]}]

	foreach slot $Priv(slots) {
		angband equipment info $slot attrib
		if {$attrib(k_idx)} {
			NSBalloon::Set_Canvas $header \
				$slot "$attrib(char)\) $attrib(name)"
		} else {
			set attrib(icon) "icon none 0"
			NSBalloon::Set_Canvas $header $slot ""
		}
		$header itemconfigure $slot -assign $attrib(icon)

		foreach flag [angband equipment flags $slot] {
			$canvas itemconfigure $flag,$slot -fill White -outline White
			set visible($flag) 1
		}
	}

	$header itemconfigure py -assign [angband player icon]
	foreach flag [angband player flags] {
		$canvas itemconfigure $flag,py -fill White -outline White
		set visible($flag) 1
	}
	set desc "[angband player name], the [angband player race]\
		[angband player class]"
	NSBalloon::Set_Canvas $header py $desc

	if {[variant ZANGBANDTK]} {
		# Hack -- Add special racial flag-like things
		switch [angband player race] {
			Golem {
				set visible(RACE_IM_CUT) 1
				set visible(RACE_IM_STUN) 1
				$canvas itemconfigure RACE_IM_CUT,py -fill White -outline White
				$canvas itemconfigure RACE_IM_STUN,py -fill White -outline White
			}
			Skeleton {
				set visible(RACE_IM_CUT) 1
				set visible(RACE_RES_SANITY) 1
				$canvas itemconfigure RACE_IM_CUT,py -fill White -outline White
				$canvas itemconfigure RACE_RES_SANITY,py -fill White -outline White
			}
			Spectre {
				set visible(RACE_IM_CUT) 1
				set visible(RACE_RES_SANITY) 1
				set visible(RACE_EAT_NETHER) 1
				$canvas itemconfigure RACE_IM_CUT,py -fill White -outline White
				$canvas itemconfigure RACE_RES_SANITY,py -fill White -outline White
				$canvas itemconfigure RACE_EAT_NETHER,py -fill White -outline White
			}
			Vampire {
				set visible(RACE_IM_DARK) 1
				set visible(RACE_RES_SANITY) 1
				$canvas itemconfigure RACE_IM_DARK,py -fill White -outline White
				$canvas itemconfigure RACE_RES_SANITY,py -fill White -outline White
			}
			Zombie {
				if {[angband player level] > 11} {
					set visible(RACE_IM_CUT) 1
					$canvas itemconfigure RACE_IM_CUT,py -fill White -outline White
				}
				set visible(RACE_RES_SANITY) 1
				$canvas itemconfigure RACE_RES_SANITY,py -fill White -outline White
			}
		}
	# ZANGBANDTK
	}

	foreach flag $Priv(flags) {
		if {!$visible($flag)} {
			$canvas itemconfigure $flag,text -fill gray60
		}
	}

	$canvas configure -scrollregion [list 0 0 [winfo reqwidth $canvas] $y]
	$canvas yview moveto 0

	Configure $oop 

	# Hack -- Fix scroll glitch
	set frame [Info $oop frame]
	set scrollHgt $y
	set winHgt [expr {[winfo height $frame] - [winfo y $canvas]}]
	if {$winHgt % $rowHgt} {
		incr scrollHgt [expr {$rowHgt - ($winHgt % $rowHgt) - 1}]
		$canvas configure -scrollregion [list 0 0 [winfo reqwidth $canvas] $scrollHgt]
	}

	return
}

# NSCharFlagsCanvas::StatusBar_Slot --
#
#	Put the name of the item in the given slot in the status bar.
#	Also displays item recall.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharFlagsCanvas::StatusBar_Slot {oop slot} {

	angband equipment info $slot attrib
	if {$attrib(k_idx)} {

		# Show name in statusbar
		[Info $oop statusBar] itemconfigure t1 -text $attrib(name)
	}

	NSRecall::RecallObject equipment $slot

	return
}

# NSCharFlagsCanvas::StatusBar_Player --
#
#	Display the character name and race in the status bar, and recall
#	the character history to the Recall Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharFlagsCanvas::StatusBar_Player {oop} {

	set desc [format [mc status-player] [angband player name] [angband player race] \
		[angband player class]]

	[Info $oop statusBar] itemconfigure t1 -text $desc

	set icon [angband player icon]
	set color [Value TERM_L_BLUE]
	set memory [angband player history]
	NSRecall::SetText [Global recall,oop] $icon $color $desc $memory

	# Hack -- Set things up so if the character icon changes, the
	# Recall Window will be updated. This is massive overkill.
	set NSRecall::Priv(icon,to) character
	if {[variant ANGBANDTK KANGBANDTK ZANGBANDTK]} {
		set NSRecall::Priv(icon,toindex) 0
	}
	if {[variant OANGBANDTK]} {
		set NSRecall::Priv(icon,toindex) [struct set player_type 0 schange]
	}
	set NSRecall::Priv(icon,valid) 1

	return
}

# NSCharFlagsCanvas::Configure --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharFlagsCanvas::Configure {oop} {

	set frame [Info $oop frame]
	set canvas [Info $oop canvas]

	set doVert 0
	set doHorz 0
	if {![ContentVisible $canvas yview]} {
		set doVert 1
	}
	if {![ContentVisible $canvas xview]} {
		set doHorz 1
	}
	if {$doVert != [Info $oop scrollbar,vert]} {
		if {$doVert} {
			grid $frame.yscroll
		} else {
			grid remove $frame.yscroll
		}
		Info $oop scrollbar,vert $doVert
	}
	if {$doHorz != [Info $oop scrollbar,horz]} {
		if {$doHorz} {
			grid $frame.xscroll
		} else {
			grid remove $frame.xscroll
		}
		Info $oop scrollbar,horz $doHorz
	}

	return
}

# NSCharFlagsCanvas::ValueChanged_font_knowledge --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharFlagsCanvas::ValueChanged_font_knowledge {oop} {

	variable Priv

	[Info $oop canvas] delete all
	[Info $oop header,canvas] delete all

	set font [Value font,knowledge]
	set lineHgt [font metrics $font -linespace]

	set Priv(font,font) $font
	set Priv(font,height) $lineHgt

	InitLayout $oop
	if {[winfo ismapped [Info $oop canvas]]} {
puts NSCharFlagsCanvas::SetInfo
		SetInfo $oop
	}

	return
}
