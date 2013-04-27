# File: charinfo-canvas.tcl

# Purpose: the Character Info canvas and related commands

#
# Copyright (c) 1997-2001 Tim Baker
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

	set font [Global font,fixed,normal]
	set lineHgt [font metrics $font -linespace]

	set charWidth [font measure $font 9]
	set width [expr {$charWidth * 80}]
	set height [expr {$lineHgt * 25}]

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
		if {[string compare $label -]} {
			set label $label
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
		if {[string compare $tag -]} {
			lappend idList [$canvas create text 0 0 -font $font \
				-anchor $anchor -fill White -tags [list  $group $tag]]
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
	set rowX $row1
	incr row2

	set labelList {
		"Name"
		"Sex"
		"Race"
		"Class"
		"Title"
	}
	lappend labelList \
		"Realm 1" \
		"Realm 2" \
		"Patron"
	InitLayoutAux $oop tag1 0 nw $labelList

	set labelList {
		Age
		Height
		Weight
		Status
	}
	lappend labelList \
		HP \
		SP \
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
	set labelList [lreplace $labelList 6 6 - Armor]
	InitLayoutAux $oop tag4 $row2 nw $labelList
	
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
	lappend labelList Infravision
	InitLayoutAux $oop tag6 $row2 nw $labelList
	Info $oop minx,tag6 48

	set tagList {
		name
		sex
		race
		class
		title
	}
	lappend tagList \
		realm1 \
		realm2 \
		patron
	InitLayoutAux2 $oop tagA 0 nw $tagList
	Info $oop minx,tagA 9

	set tagList {
		age
		height
		weight
		social_class
	}
	lappend tagList \
		*hp \
		*sp \
		maximize \
		preserve

	InitLayoutAux2 $oop tagB $rowX ne $tagList
	set col 33
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
	set tagList [lreplace $tagList 6 6 - *armor]

	InitLayoutAux2 $oop tagC $row2 ne $tagList
	Info $oop minx,tagC [expr {8 + 10}]
	
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
	lappend tagList *infra

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
	
	# Negative value
	if {$x < 0} {
		set likert_color [Value TERM_L_RED]
		return "Very Bad"
	}

	# Analyze the value
	switch [expr {$x / $y}] {

		0 -
		1 {
			set likert_color [Value TERM_L_RED]
			return "Bad"
		}
		2 {
			set likert_color [Value TERM_L_RED]
			return "Poor"
		}
		3 -
		4 {
			set likert_color [Value TERM_YELLOW]
			return "Fair"
		}
		5 {
			set likert_color [Value TERM_YELLOW]
			return "Good"
		}
		6 {
			set likert_color [Value TERM_YELLOW]
			return "Very Good"
		}
		7 -
		8 {
			set likert_color [Value TERM_L_GREEN]
			return "Excellent"
		}
		9 -
		10 -
		11 -
		12 -
		13 {
			set likert_color [Value TERM_L_GREEN]
			return "Superb"
		}
		14 -
		15 -
		16 -
		17 {
			set likert_color [Value TERM_L_BLUE]
			return "Chaos Rank"
		}
		default {
			set likert_color Violet
			return [format "Amber \[%d\]" [expr {int((((($x / $y) - 17) * 5) / 2))}]]
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
	if {[Info $oop anchor,$group] == "ne"} {
		incr x2 $width
	}
	if {$x2 < $charWidth * [Info $oop minx,$group]} {
		set x2 [expr {$charWidth * [Info $oop minx,$group]}]
	}
	set y [expr {4 + ([Info $oop topRow] + [Info $oop row,$group]) * $rowHeight}]
	
	foreach id [Info $oop id,$group] {
		if {[string compare $id -]} {
			$canvas coords $id $x2 $y
		}
		incr y $rowHeight
	}

	set pad [expr {$pad * $charWidth}]
	if {[Info $oop anchor,$group] == "ne"} {
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
	set row 19

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


	# Hit Points
	set info [angband player hitpoints]
	set cur [lindex $info 0]
	set max [lindex $info 1]
	if {$cur >= $max} {
		set fill [Value TERM_L_BLUE]
	} elseif {$cur > ($max * 5) / 10} {
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
	} elseif {$cur > ($max * 5) / 10} {
		set fill [Value TERM_YELLOW]
	} else {
		set fill [Value TERM_L_RED]
	}
	$canvas itemconfigure *sp -text "$cur/$max" -fill $fill

	set attribs {age height weight social_class patron}

	foreach attrib $attribs {
		set value [angband player $attrib]
		
		$canvas itemconfigure $attrib -text $value -fill [Value TERM_L_BLUE]
	}

	foreach attrib {realm1 realm2} {
		set text [angband player $attrib]
		if {[string equal $text "no magic"]} {
			set text "None"
		}
		$canvas itemconfigure $attrib -text $text -fill [Value TERM_L_BLUE]
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
	
	set burden [format "%d.%d $units" [expr {$weight / 10}] [expr {$weight % 10}]]
	$canvas itemconfigure *burden -text $burden -fill [Value TERM_L_GREEN]

if 0 {

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
	$canvas itemconfigure *armor -fill [Value TERM_L_GREEN]

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

	# Shoot
	set hit [angband player to_hit]
	set dam 0
	angband equipinfo INVEN_BOW dump
	if {$dump(known)} {incr hit $dump(to_h)}
	if {$dump(known)} {incr dam $dump(to_d)}
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
	
	# Blows
	set blows [angband player blows_per_round]
	$canvas itemconfigure *blows -text $blows -fill [Value TERM_L_BLUE]

	# Shots
	set shots [angband player shots_per_round]
	$canvas itemconfigure *shots -text $shots -fill [Value TERM_L_BLUE]

	# Infravision
	set infra [angband player infravision]
	set units feet
	
	$canvas itemconfigure *infra -text "$infra $units" -fill [Value TERM_L_BLUE]
	set attribs {fighting bows_throw saving_throw stealth}
	append attribs { perception searching disarming magic_device}
	foreach attrib $attribs {
		$canvas itemconfigure $attrib \
			-text [eval likert [angband player ability $attrib]] \
			-fill $likert_color
	}

	$canvas itemconfigure history -text [angband player history]

	# Arrange the items
	PositionItems $oop

	scan [$canvas bbox all] "%s %s %s %s" left top right bottom
	set left 0
	set top 0
	$canvas configure -scrollregion "$left $top $right $bottom"

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

	lappend tags \
		*fskill *fdeadliness *sskill *sdeadliness \
		name sex race class title realm1 realm2 patron \
		age height weight social_class *hp *sp maximize preserve \
		*armor *fight *melee *shoot *blows *shots *infra \
		level exp expmax expadv gold *burden \
		fighting bows_throw saving_throw stealth \
		perception searching disarming magic_device \
		history

	foreach tag $tags {
		$canvas itemconfigure $tag -text {}
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

	if {[string equal $justify right]} {
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
	if {[string compare [$canvas yview] "0 1"]} {
		set doVert 1
	}
	if {[string compare [$canvas xview] "0 1"]} {
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

