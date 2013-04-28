# File: charinfo-canvas.tcl

# Purpose: the Character Info canvas and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSCharInfoCanvas {

# namespace eval NSCharInfoCanvas
}

# NSCharInfoCanvas::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharInfoCanvas::InitModule {} {

	MsgCatInit player

	return
}

# NSCharInfoCanvas::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharInfoCanvas::CloseModule {} {

	return
}

# NSCharInfoCanvas::NSCharInfoCanvas --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharInfoCanvas::NSCharInfoCanvas {oop parent} {

	set font [Value font,fixed]
	set lineHgt [font metrics $font -linespace]

	set charWidth [font measure $font 9]
	if {$::JAPANESE} {
#		set charWidth [expr {$charWidth / 2}]
	}
	set width [expr {$charWidth * 80}]

	if {[variant ANGBANDTK KANGBANDTK]} {
		set height [expr {$lineHgt * 24}]
	}
	if {[variant OANGBANDTK ZANGBANDTK]} {
		set height [expr {$lineHgt * 25}]
	}

	set canvas $parent.charinfocanvas$oop
	canvas $canvas \
		-height $height -width $width \
		-relief flat -highlightthickness 0 -background #000022 \
		-yscrollcommand "$parent.yscroll set" \
		-xscrollcommand "$parent.xscroll set"
	scrollbar $parent.yscroll \
		-orient vertical -command "$canvas yview"
	scrollbar $parent.xscroll \
		-orient horizontal -command "$canvas xview"

	grid rowconfig $parent 0 -weight 1
	grid rowconfig $parent 1 -weight 0
	grid columnconfig $parent 0 -weight 1
	grid columnconfig $parent 1 -weight 0

	grid $canvas \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $parent.yscroll \
		-row 0 -column 1 -rowspan 1 -columnspan 1 -sticky ns
	grid $parent.xscroll \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid remove $parent.yscroll
	grid remove $parent.xscroll
	Info $oop scrollbar,vert 0
	Info $oop scrollbar,horz 0

	bind $canvas <Configure> \
		"+NSCharInfoCanvas::Configure $oop"
	bind $canvas <Map> \
		"+NSCharInfoCanvas::Configure $oop"

	qebind $canvas <Py-name> \
		{%W itemconfigure name -text "%c"}
	qebind $canvas <Value-font,fixed> \
		"NSCharInfoCanvas::ValueChanged_font_fixed $oop"

	NSUtils::DestroyObjectWithWidget NSCharInfoCanvas $oop $canvas

	Info $oop canvas $canvas
	Info $oop font,font $font
	Info $oop font,width $charWidth
	Info $oop font,height $lineHgt
	Info $oop frame $parent
	Info $oop topRow 0

	InitLayout $oop

	return
}

# NSCharInfoCanvas::~NSCharInfoCanvas --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharInfoCanvas::~NSCharInfoCanvas {oop} {

	# bindings are deleted with canvas

	return
}

# NSCharInfoCanvas::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharInfoCanvas::Info {oop info args} {

	global NSCharInfoCanvas

	# Verify the object
	NSObject::CheckObject NSCharInfoCanvas $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSCharInfoCanvas($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSCharInfoCanvas($oop,$info)
			}
		}
	}

	return
}

# NSCharInfoCanvas::InitLayout --
#
#	Create all the canvas items.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharInfoCanvas::InitLayoutAux {oop group row anchor labelList} {

	set canvas [Info $oop canvas]
	set font [Info $oop font,font]

	foreach label $labelList {
		if {$label ne "-"} {
			set label [mc $label]
			lappend idList [$canvas create text 0 0 -font $font -fill White \
				-anchor $anchor -text $label -tags $group]
		} else {
			lappend idList -
		}
	}

	Info $oop id,$group $idList
	Info $oop anchor,$group $anchor
	Info $oop row,$group $row
	Info $oop minx,$group 0

	return
}

proc NSCharInfoCanvas::InitLayoutAux2 {oop group row anchor tagList} {

	set canvas [Info $oop canvas]
	set font [Info $oop font,font]

	foreach tag $tagList {
		if {$tag ne "-"} {
			lappend idList [$canvas create text 0 0 -font $font \
				-anchor $anchor -fill White -tags [list $group $tag]]
		} else {
			lappend idList -
		}
	}

	if {[info exists ::NSCharInfoCanvas($oop,id,$group)]} {
		set idList [concat [Info $oop id,$group] $idList]
	}
	Info $oop id,$group $idList
	Info $oop anchor,$group $anchor
	Info $oop row,$group $row
	if {![info exists ::NSCharInfoCanvas($oop,minx,$group)]} {
		Info $oop minx,$group 0
	}

	return
}

proc NSCharInfoCanvas::InitLayout {oop} {

	set canvas [Info $oop canvas]

	set row1 0
	set rowX 1
	set row2 8
	if {[variant ZANGBANDTK]} {
		set rowX $row1
		incr row2
	}

	set labelList {
		"Name"
		"Sex"
		"Race"
		"Class"
		"Title"
	}
	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		lappend labelList \
			"HP" \
			"SP"
	}
	if {[variant ZANGBANDTK]} {
		lappend labelList \
			"Realm 1" \
			"Realm 2" \
			"Patron"
	}
	InitLayoutAux $oop tag1 0 nw $labelList

	set labelList {
		Age
		Height
		Weight
		Status
	}
	if {[variant ZANGBANDTK]} {
		lappend labelList \
			HP \
			SP
	}
	lappend labelList \
		Maximize \
		Preserve

	InitLayoutAux $oop tag2 $rowX nw $labelList
	Info $oop minx,tag2 25

	set labelList {
		STR:
		INT:
		WIS:
		DEX:
		CON:
		CHR:
	}
	InitLayoutAux $oop tag3 1 nw $labelList
	Info $oop minx,tag3 41

	set labelList {
		Self
		RB
		CB
		EB
		Best
	}
	foreach tag {max race class equip top} col {46 53 57 61 65} wid {6 3 3 3 6} label $labelList {
		InitLayoutAux $oop tag$tag 0 ne [list $label]
		Info $oop minx,tag$tag [expr {$col + $wid}]
	}

	set labelList {
		Level
		"Cur Exp"
		"Max Exp"
		"Adv Exp"
		-
		Gold
		-
		Burden
	}
	if {[variant OANGBANDTK ZANGBANDTK]} {
		set labelList [lreplace $labelList 6 6 - Armor]
	}
	InitLayoutAux $oop tag4 $row2 nw $labelList

	if {[variant ANGBANDTK KANGBANDTK]} {
		set labelList {
			"Armor"
			"Fight"
			"Melee"
			"Shoot"
			"Blows"
			"Shots"
			-
			"Infra"
		}
	}
	if {[variant OANGBANDTK ZANGBANDTK]} {
		set labelList {
			"Fighting"
			"+ to Skill"
			"Deadliness"
			"Blows/Round"
			-
			"Shooting"
			"+ to Skill"
			"Deadliness"
			"Shots/Round"
		}
	}
	InitLayoutAux $oop tag5 $row2 nw $labelList
	Info $oop minx,tag5 25

	set labelList {
		"Saving Throw"
		"Stealth"
		"Fighting"
		"Shooting"
		"Disarming"
		"Magic Device"
		"Perception"
		"Searching"
	}
	if {[variant OANGBANDTK ZANGBANDTK]} {
		lappend labelList Infravision
	}
	InitLayoutAux $oop tag6 $row2 nw $labelList
	Info $oop minx,tag6 48

	set tagList {
		name
		sex
		race
		class
		title
	}
	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		lappend tagList \
			*hp \
			*sp
	}
	if {[variant ZANGBANDTK]} {
		lappend tagList \
			realm1 \
			realm2 \
			patron
	}
	InitLayoutAux2 $oop tagA 0 nw $tagList
	Info $oop minx,tagA 9

	set tagList {
		age
		height
		weight
		social_class
	}
	if {[variant ZANGBANDTK]} {
		lappend tagList \
			*hp \
			*sp
	}
	lappend tagList \
		maximize \
		preserve

	InitLayoutAux2 $oop tagB $rowX ne $tagList
	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		set col 34
	}
	if {[variant ZANGBANDTK]} {
		set col 33
	}
	Info $oop minx,tagB [expr {$col + 4}]

	set tagList {
		strength.max
		intelligence.max
		wisdom.max
		dexterity.max
		constitution.max
		charisma.max
	}
	InitLayoutAux2 $oop tagmax 0 ne $tagList

	set tagList {
		strength.race
		intelligence.race
		wisdom.race
		dexterity.race
		constitution.race
		charisma.race
	}
	InitLayoutAux2 $oop tagrace 0 ne $tagList

	set tagList {
		strength.class
		intelligence.class
		wisdom.class
		dexterity.class
		constitution.class
		charisma.class
	}
	InitLayoutAux2 $oop tagclass 0 ne $tagList

	set tagList {
		strength.equip
		intelligence.equip
		wisdom.equip
		dexterity.equip
		constitution.equip
		charisma.equip
	}
	InitLayoutAux2 $oop tagequip 0 ne $tagList

	set tagList {
		strength.top
		intelligence.top
		wisdom.top
		dexterity.top
		constitution.top
		charisma.top
	}
	InitLayoutAux2 $oop tagtop 0 ne $tagList

	set tagList {
		strength.use
		intelligence.use
		wisdom.use
		dexterity.use
		constitution.use
		charisma.use
	}
	InitLayoutAux2 $oop taguse 1 ne $tagList

	set tagList {
		level
		exp
		expmax
		expadv
		-
		gold
		-
		*burden
	}
	if {[variant OANGBANDTK ZANGBANDTK]} {
		set tagList [lreplace $tagList 6 6 - *armor]
	}
	InitLayoutAux2 $oop tagC $row2 ne $tagList
	Info $oop minx,tagC [expr {8 + 10}]

	if {[variant ANGBANDTK KANGBANDTK]} {
		set tagList {
			*armor
			*fight
			*melee
			*shoot
			*blows
			*shots
			-
			*infra
		}
	}
	if {[variant OANGBANDTK ZANGBANDTK]} {
		set tagList {
			-
			*fskill
			*fdeadliness
			*blows
			-
			-
			*sskill
			*sdeadliness
			*shots
		}
	}
	InitLayoutAux2 $oop tagD $row2 ne $tagList
	Info $oop minx,tagD [expr {30 + 13}]

	set tagList {
		saving_throw
		stealth
		fighting
		bows_throw
		disarming
		magic_device
		perception
		searching
	}
	if {[variant OANGBANDTK ZANGBANDTK]} {
		lappend tagList *infra
	}
	InitLayoutAux2 $oop tagE $row2 ne $tagList
	Info $oop minx,tagE [expr {62 + 9}]

	# History
	$canvas create text 0 0 -fill White -text "" \
		-font [Info $oop font,font] -justify left -anchor nw -tags history

	return
}

#
# Returns a "rating" of x depending on y
#
proc NSCharInfoCanvas::likert {x y} {

	variable likert_color

	# Paranoia
	if {$y <= 0} {
		set y 1
	}

	if {[variant OANGBANDTK]} {

		# Negative value
		if {$x < 0} {
			set likert_color [palette set 129]
			return [mc "Awful"]
		}

		# Analyze the value
		switch [expr {$x / $y}] {

			0 {
				set likert_color [palette set 217]
				return [mc "Very Bad"]
			}
			1 {
				set likert_color [palette set 35]
				return [mc "Bad"]
			}
			2 {
				set likert_color [palette set 17]
				return [mc "Poor"]
			}
			3 {
				set likert_color [palette set 17]
				return [mc "Mediocre"]
			}
			4 {
				set likert_color [palette set 5]
				return [mc "Fair"]
			}
			5 {
				set likert_color [palette set 5]
				return [mc "Good"]
			}
			6 -
			7 {
				set likert_color [palette set 5]
				return [mc "Very Good"]
			}
			8 -
			9 -
			10 {
				set likert_color [palette set 185]
				return [mc "Excellent"]
			}
			11 -
			12 -
			13 {
				set likert_color [palette set 185]
				return [mc "Superb"]
			}
			14 -
			15 -
			16 -
			17 {
				set likert_color [palette set 204]
				return [mc "Heroic"]
			}
			default {
				set likert_color [palette set 204]
				return [mc "Legendary"]
			}
		}

		# Done
		return

	# OANGBANDTK
	}

	# Negative value
	if {$x < 0} {
		set likert_color [Value TERM_L_RED]
		return [mc "Very Bad"]
	}

	# Analyze the value
	switch [expr {$x / $y}] {

		0 -
		1 {
			set likert_color [Value TERM_L_RED]
			return [mc "Bad"]
		}
		2 {
			set likert_color [Value TERM_L_RED]
			return [mc "Poor"]
		}
		3 -
		4 {
			set likert_color [Value TERM_YELLOW]
			return [mc "Fair"]
		}
		5 {
			set likert_color [Value TERM_YELLOW]
			return [mc "Good"]
		}
		6 {
			set likert_color [Value TERM_YELLOW]
			return [mc "Very Good"]
		}
		7 -
		8 {
			set likert_color [Value TERM_L_GREEN]
			return [mc "Excellent"]
		}
		9 -
		10 -
		11 -
		12 -
		13 {
			set likert_color [Value TERM_L_GREEN]
			return [mc "Superb"]
		}
		14 -
		15 -
		16 -
		17 {
			if {[variant ANGBANDTK KANGBANDTK]} {
				set likert_color [Value TERM_L_GREEN]
				return [mc "Heroic"]
			}
			if {[variant ZANGBANDTK]} {
				set likert_color [Value TERM_L_BLUE]
				return [mc "Chaos Rank"]
			}
		}
		default {
			if {[variant ANGBANDTK KANGBANDTK]} {
				set likert_color [Value TERM_L_GREEN]
				return [mc "Legendary"]
			}
			if {[variant ZANGBANDTK]} {
				set likert_color Violet
				return [format [mc "Amber \[%d\]"] [expr {int((((($x / $y) - 17) * 5) / 2))}]]
			}
		}
	}

	return
}

# NSCharInfoCanvas::PositionItems --
#
#	Arrange all the canvas items.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharInfoCanvas::Layout {oop group x pad} {

	set canvas [Info $oop canvas]
	set rowHeight [Info $oop font,height]
	set charWidth [Info $oop font,width]

	scan [$canvas bbox $group] "%s %s %s %s" left top right bottom
	set width [expr {$right - $left}]

	set x2 $x
	if {[Info $oop anchor,$group] eq "ne"} {
		incr x2 $width
	}
	if {$x2 < $charWidth * [Info $oop minx,$group]} {
		set x2 [expr {$charWidth * [Info $oop minx,$group]}]
	}
	set y [expr {4 + ([Info $oop topRow] + [Info $oop row,$group]) * $rowHeight}]

	foreach id [Info $oop id,$group] {
		if {$id ne "-"} {
			$canvas coords $id $x2 $y
		}
		incr y $rowHeight
	}

	set pad [expr {$pad * $charWidth}]
	if {[Info $oop anchor,$group] eq "ne"} {
		return [expr {$x2 + $pad}]
	}
	return [expr {$x2 + $width + $pad}]
}

proc NSCharInfoCanvas::PositionItems {oop} {

	set canvas [Info $oop canvas]
	set topRow [Info $oop topRow]

	set x [Layout $oop tag1 4 1]
	set x [Layout $oop tagA $x 1]
	set x [Layout $oop tag2 $x 1]
	set x [Layout $oop tagB $x 3]
	set x [Layout $oop tag3 $x 1]
	foreach tag {max race class equip top use} {
		set x [Layout $oop tag$tag $x 1]
	}
	set x [Layout $oop tag4 4 2]
	set x [Layout $oop tagC $x 7]
	set x [Layout $oop tag5 $x 3]
	set x [Layout $oop tagD $x 5]
	set x [Layout $oop tag6 $x 1]
	set x [Layout $oop tagE $x 0]

	set x [expr {4 + 50}]
	if {[variant ANGBANDTK KANGBANDTK]} {
		set row 17
	}
	if {[variant OANGBANDTK]} {
		set row 18
	}
	if {[variant ZANGBANDTK]} {
		set row 19
	}
	set y [expr {($topRow + $row) * [Info $oop font,height]}]
	$canvas coords history $x $y

	return
}

# NSCharInfoCanvas::SetInfo --
#
#	Set text of character-specific items.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharInfoCanvas::SetInfo {oop} {

	variable likert_color

	set canvas [Info $oop canvas]

	set attribs {name sex race class title}
	foreach attrib $attribs {
		$canvas itemconfigure $attrib -text [angband player $attrib] \
			-fill [Value TERM_L_BLUE]
	}

	# Get hitpoint warning
	set hitpoint_warn [angband setting set hitpoint_warn]

	# Hit Points
	set info [angband player hitpoints]
	set cur [lindex $info 0]
	set max [lindex $info 1]
	if {$cur >= $max} {
		set fill [Value TERM_L_BLUE]
	} elseif {$cur > ($max * $hitpoint_warn) / 10} {
		set fill [Value TERM_YELLOW]
	} else {
		set fill [Value TERM_L_RED]
	}
	$canvas itemconfigure *hp -text "$cur/$max" -fill $fill

	# Spell Points
	set info [angband player mana]
	set cur [lindex $info 0]
	set max [lindex $info 1]
	if {$cur >= $max} {
		set fill [Value TERM_L_BLUE]
	} elseif {$cur > ($max * $hitpoint_warn) / 10} {
		set fill [Value TERM_YELLOW]
	} else {
		set fill [Value TERM_L_RED]
	}
	$canvas itemconfigure *sp -text "$cur/$max" -fill $fill

	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		set attribs {age height weight social_class}
	}
	if {[variant ZANGBANDTK]} {
		set attribs {age height weight social_class patron}
	}
	foreach attrib $attribs {
		set value [angband player $attrib]

		# Hack -- Convert some values to metric
		if {[variant OANGBANDTK]} {
			if {[Setting use_metric]} {
				switch -- $attrib {
					height {set value [expr {$value * 254 / 100}]}
					weight {set value [make_metric $value]} 
				}
			}
		}

		$canvas itemconfigure $attrib -text $value -fill [Value TERM_L_BLUE]
	}

	if {[variant ZANGBANDTK]} {
		foreach attrib {realm1 realm2} {
			set text [angband player $attrib]
			if {$text eq "no magic"} {
				set text "None"
			}
			$canvas itemconfigure $attrib -text $text -fill [Value TERM_L_BLUE]
		}
	# ZANGBANDTK
	}

	foreach attrib {maximize preserve} {
		set text [expr {[angband player $attrib] ? "Y" : "N"}]
		$canvas itemconfigure $attrib -text $text -fill [Value TERM_L_BLUE]
	}

	set attribs {level gold}
	foreach attrib $attribs {
		$canvas itemconfigure $attrib -text [angband player $attrib] \
			-fill [Value TERM_L_GREEN]
	}

	# Burden
	set weight [angband player total_weight]
	set units lb

	# Hack -- Convert burden to metric
	if {[variant OANGBANDTK]} {
		if {[Setting use_metric]} {
			set weight [make_metric $weight]
			set units kg
		}
	}

	set burden [format "%d.%d $units" [expr {$weight / 10}] [expr {$weight % 10}]]
	$canvas itemconfigure *burden -text $burden -fill [Value TERM_L_GREEN]

	# Stats
	set attribs [angband info stat_name]
	foreach attrib $attribs {
		angband player stat $attrib statInfo
		$canvas itemconfigure $attrib.max -text [cnv_stat_disp $statInfo(max)] \
			-fill [Value TERM_L_GREEN]
		foreach mod {race class equip} {
			if {$statInfo($mod) > 99} {
				$canvas itemconfigure $attrib.$mod \
					-text +++ -fill [Value TERM_L_BLUE]
			} else {
				$canvas itemconfigure $attrib.$mod \
					-text [format "%+d" $statInfo($mod)] -fill [Value TERM_L_BLUE]
			}
		}
		$canvas itemconfigure $attrib.top -text [cnv_stat_disp $statInfo(top)] \
			-fill [Value TERM_L_GREEN]
		if {$statInfo(use) < $statInfo(top)} {
			$canvas itemconfigure $attrib.use \
				-text [cnv_stat_disp $statInfo(use)] -fill [Value TERM_YELLOW]
		} else {
			$canvas itemconfigure $attrib.use -text ""
		}
	}

	# Experience
	set info [angband player exp]
	set cur [lindex $info 0]
	set max [lindex $info 1]
	set adv [lindex $info 2]
	if {$cur >= $max} {
		set fill [Value TERM_L_GREEN]
	} else {
		set fill [Value TERM_YELLOW]
	}
	if {$adv == 999999999} {
		set adv *****
	}
	$canvas itemconfigure exp -text $cur -fill $fill
	$canvas itemconfigure expmax -text $max -fill [Value TERM_L_GREEN]
	$canvas itemconfigure expadv -text $adv -fill [Value TERM_L_GREEN]

	# Armor
	set info [angband player armor_class]
	set base [lindex $info 0]
	set plus [format "%+d" [lindex $info 1]]
	$canvas itemconfigure *armor -text "\[$base,$plus]" -fill [Value TERM_L_BLUE]
	if {[variant OANGBANDTK ZANGBANDTK]} {
		$canvas itemconfigure *armor -fill [Value TERM_L_GREEN]
	}

	# Fight
	set hit [format "%+d" [angband player to_hit]]
	set dam [format "%+d" [angband player to_dam]]
	$canvas itemconfigure *fight -text "($hit,$dam)" -fill [Value TERM_L_BLUE]

	# Melee
	set hit [angband player to_hit]
	set dam [angband player to_dam]
	angband equipinfo INVEN_WIELD dump
	if {$dump(known)} {incr hit $dump(to_h)}
	if {$dump(known)} {incr dam $dump(to_d)}
	if {[variant ANGBANDTK KANGBANDTK]} {
		set hit [format "%+d" $hit]
		set dam [format "%+d" $dam]
		$canvas itemconfigure *melee -text "($hit,$dam)" -fill [Value TERM_L_BLUE]
	}
	if {[variant OANGBANDTK]} {
		$canvas itemconfigure *fskill -text $hit -fill [Value TERM_L_BLUE]
		$canvas itemconfigure *fdeadliness -text [angband player deadliness_conversion $dam] -fill [Value TERM_L_BLUE]
	}
	if {[variant ZANGBANDTK]} {
		set deadliness [angband player deadliness_conversion $dam]
		if {$dam > 0} {
			set percentdam [expr {100 + $deadliness}]
		} elseif {$dam > -31} {
			set percentdam [expr {100 - $deadliness}]
		} else {
			set percentdam 0
		}
		$canvas itemconfigure *fskill -text $hit -fill [Value TERM_L_BLUE]
		$canvas itemconfigure *fdeadliness -text $percentdam% -fill [Value TERM_L_BLUE]
	}

	# Shoot
	set hit [angband player to_hit]
	set dam 0
	angband equipinfo INVEN_BOW dump
	if {$dump(known)} {incr hit $dump(to_h)}
	if {$dump(known)} {incr dam $dump(to_d)}
	if {[variant ANGBANDTK KANGBANDTK]} {
		set hit [format "%+d" $hit]
		set dam [format "%+d" $dam]
		$canvas itemconfigure *shoot -text "($hit,$dam)" -fill [Value TERM_L_BLUE]
	}
	if {[variant OANGBANDTK]} {
		incr dam [angband player to_dam]
		$canvas itemconfigure *sskill -text $hit -fill [Value TERM_L_BLUE]
		$canvas itemconfigure *sdeadliness -text [angband player deadliness_conversion $dam] -fill [Value TERM_L_BLUE]
	}
	if {[variant ZANGBANDTK]} {
		incr dam [angband player to_dam]
		set deadliness [angband player deadliness_conversion $dam]
		if {$dam > 0} {
			set percentdam [expr {100 + $deadliness}]
		} elseif {$dam > -31} {
			set percentdam [expr {100 - $deadliness}]
		} else {
			set percentdam 0
		}
		$canvas itemconfigure *sskill -text $hit -fill [Value TERM_L_BLUE]
		$canvas itemconfigure *sdeadliness -text $percentdam% -fill [Value TERM_L_BLUE]
	}

	# Blows
	set blows [angband player blows_per_round]
	$canvas itemconfigure *blows -text $blows -fill [Value TERM_L_BLUE]

	# Shots
	set shots [angband player shots_per_round]
	$canvas itemconfigure *shots -text $shots -fill [Value TERM_L_BLUE]

	# Infravision
	set infra [angband player infravision]
	if {$infra == 1} {
		set units foot ; # Never this small
	} else {
		set units feet
	}
	set fill [Value TERM_L_BLUE]

	# Hack -- Convert infravision to metric
	if {[variant OANGBANDTK]} {
		if {[Setting use_metric]} {
			set infra [expr {$infra * 3 / 10}]
			if {$infra == 1} {
				set units metre
			} else {
				set units metres
			}
			set fill [Value TERM_L_GREEN]
		}
	}
	$canvas itemconfigure *infra -text [format [mc infra-fmt] $infra [mc $units]] \
		-fill $fill

	set attribs {fighting bows_throw saving_throw stealth}
	append attribs { perception searching disarming magic_device}
	foreach attrib $attribs {
		$canvas itemconfigure $attrib \
			-text [likert {*}[angband player ability $attrib]] \
			-fill $likert_color
	}

	$canvas itemconfigure history -text [angband player history]

	# Arrange the items
	PositionItems $oop

	scan [$canvas bbox all] "%s %s %s %s" left top right bottom
	$canvas configure -scrollregion "0 0 $right $bottom"

	return
}

# NSCharInfoCanvas::WipeInfo --
#
#	Clear text of character-specific items.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharInfoCanvas::WipeInfo {oop} {

	set canvas [Info $oop canvas]

	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
		lappend tags \
			name sex race class title *hp *sp \
			age height weight social_class maximize preserve
	}
	if {[variant OANGBANDTK ZANGBANDTK]} {
		lappend tags \
			*fskill *fdeadliness *sskill *sdeadliness
	}
	if {[variant ZANGBANDTK]} {
		lappend tags \
			name sex race class title realm1 realm2 patron \
			age height weight social_class *hp *sp maximize preserve
	}

	lappend tags \
		*armor *fight *melee *shoot *blows *shots *infra \
		level exp expmax expadv gold *burden \
		fighting bows_throw saving_throw stealth \
		perception searching disarming magic_device \
		history

	foreach tag $tags {
		$canvas itemconfigure $tag -text {}
	}

	foreach tag [angband info stat_name] {
		foreach mod {max race class equip top use} {
			$canvas itemconfigure $tag.$mod -text ""
		}
	}

	return
}

# NSCharInfoCanvas::AddTextItem --
#
#	Create a new canvas text item with given option values.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharInfoCanvas::AddTextItem {oop col row width fill text justify tags} {

	set canvas [Info $oop canvas]

	if {$justify eq "right"} {
		incr col $width
		set anchor ne
	} else {
		set anchor nw
	}
	set padleft 4
	set padtop 4
	set x [expr {$padleft + $col * [Info $oop font,width]}]
	set y [expr {$padtop + $row * [Info $oop font,height]}]
	set width [expr {$width * [Info $oop font,width]}]
	$canvas create text $x $y -fill $fill -text $text \
		-font [Info $oop font,font] -justify $justify -anchor $anchor \
		-tags $tags

	return
}

# NSCharInfoCanvas::Configure --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharInfoCanvas::Configure {oop} {

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

# NSCharInfoCanvas::ValueChanged_font_fixed --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSCharInfoCanvas::ValueChanged_font_fixed {oop} {

	set canvas [Info $oop canvas]
	set font [Value font,fixed]

	set lineHgt [font metrics $font -linespace]
	set charWidth [font measure $font 9]

	Info $oop font,font $font
	Info $oop font,width $charWidth
	Info $oop font,height $lineHgt

	# FIXME: this will break if some items are not text
	$canvas itemconfigure all -font $font

	PositionItems $oop

	scan [$canvas bbox all] "%s %s %s %s" left top right bottom
	$canvas configure -scrollregion "0 0 $right $bottom"

	Configure $oop

	return
}
