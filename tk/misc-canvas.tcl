# File: misc-canvas.tcl

# Purpose: canvas used in Misc Window and Main Window

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSMiscCanvas {

	variable Priv

# namespace eval NSMiscCanvas
}

# NSMiscCanvas::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscCanvas::InitModule {} {

	MsgCatInit misc-win

	InitImageIfNeeded Image_MiscAC ac.gif
	InitImageIfNeeded Image_MiscAU au.gif
	InitImageIfNeeded Image_MiscCHR cha.gif
	InitImageIfNeeded Image_MiscCON const.gif
	InitImageIfNeeded Image_MiscDEX dex.gif
	InitImageIfNeeded Image_MiscEXP exp.gif
	InitImageIfNeeded Image_MiscINT int.gif
	InitImageIfNeeded Image_MiscLEVEL level.gif
	InitImageIfNeeded Image_MiscLOOK look.gif
	InitImageIfNeeded Image_MiscSTR str.gif
	InitImageIfNeeded Image_MiscWIS wis.gif

	NSModule::LoadIfNeeded NSMiscToolbar

	return
}

# NSMiscCanvas::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscCanvas::CloseModule {} {

	return
}

# NSMiscCanvas::NSMiscCanvas --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscCanvas::NSMiscCanvas {oop args} {

	array set opts {
		-parent ""
		-toolbar yes
		-layout wide
		-progress ""
	}
	array set opts $args

	set parent $opts(-parent)

	# Frame to hold toolbar and canvas
	set frame $parent.frameMisc$oop
	frame $frame \
		-borderwidth 0

	Info $oop frame $frame
	Info $oop parent $parent
	Info $oop layout $opts(-layout)
	Info $oop progress,oop $opts(-progress)

	set c [InitCanvas $oop]

	if {$opts(-toolbar)} {
		$c create line 0 0 1 1 -fill #333366 -tags divider
		set tools [InitToolbar $oop]
		pack $tools -side top -anchor n
	} else {
		Info $oop toolbarId ""
	}
	pack $c -side top

	Arrange $oop

	# Destroy the object along with the canvas (later)
	NSUtils::DestroyObjectWithWidget NSMiscCanvas $oop $c

	return
}

# NSMiscCanvas::~NSMiscCanvas --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscCanvas::~NSMiscCanvas {oop} {

	# bindings are deleted with the canvas

	return
}

# NSMiscCanvas::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscCanvas::Info {oop info args} {

	global NSMiscCanvas

	# Verify the object
	NSObject::CheckObject NSMiscCanvas $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSMiscCanvas($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSMiscCanvas($oop,$info)
			}
		}
	}

	return
}

proc NSMiscCanvas::InitToolbar {oop} {

	set frame [Info $oop frame]
	set popupId [NSObject::New NSMiscToolbar -parent $frame]
	Info $oop toolbarId $popupId
	Info $oop toolbar,canvas [NSMiscToolbar::Info $popupId canvas]

	return [NSMiscToolbar::Info $popupId canvas]
}

proc NSMiscCanvas::InitCanvas {oop} {

	set parent [Info $oop parent]
	set frame [Info $oop frame]

	#
	# Font
	#

	set font [Value font,misc]
	set fontHeight [font metrics $font -linespace]

	#
	# Main canvas
	#

	set c $frame.canvas
	canvas $c \
		-scrollregion "0 0 100 100" -width 100 -height 100 \
		-relief flat -highlightthickness 0 -background Black

	# Display help string in Main Window status bar
	CanvasFeedback::Init $c

	# Character image (center at 22,22)
	$c create widget 0 0 -anchor center -tags icon

	# Click the image to see character info
	$c bind icon <Enter> {
		%W itemconfigure focus -outline gray60
	}
	$c bind icon <Leave> {
		%W itemconfigure focus -outline Black
	}

	$c bind icon <ButtonPress-1> "
		$c move icon 1 1
		set CanvasButtonDown 1
	"
	$c bind icon <Button1-Enter> "
		$c move icon 1 1
		set CanvasButtonDown 1
	"
	$c bind icon <Button1-Leave> "
		$c move icon -1 -1
		set CanvasButtonDown 0
	"
	$c bind icon <ButtonRelease-1> "
		if {\$CanvasButtonDown} {
			$c move icon -1 -1
			update idletasks
			DoCommandIfAllowed C
		}
	"
	$c bind icon <Control-ButtonRelease-1> "
		if {\$CanvasButtonDown} {
			$c move icon -1 -1
			update idletasks
			NSModule::LoadIfNeeded NSAssign
			NSWindowManager::Display assign character
		}
	"

	CanvasFeedback::Add $c icon "NSMiscCanvas::CanvasFeedbackCmd $oop icon"

	# Focus ring around character image (since user can click it)
	$c create rectangle 0 0 1 1 -outline Black -tags focus

	# When the character is assigned a new icon, I need to update the
	# canvas Widget item. This binding does the trick.
	if {[variant ANGBANDTK KANGBANDTK ZANGBANDTK]} {
		qebind $c <Assign-character> {
			%W itemconfigure icon -assign %a
		}
	}
	if {[variant OANGBANDTK]} {
		# OAngbandTk allows an icon for each shape

		# When the character is assigned an icon, update the canvas
		# if the icon was assigned to the character's current shape.
		qebind $c <Assign-character> {
			if {%I == [struct set player_type 0 schange]} {
				%W itemconfigure icon -assign %a
			}
		}

		# When the character's shape changes, update the canvas
		qebind $c <Status-shape> {
			%W itemconfigure icon -assign [angband player icon]
		}
	}

	qebind $c <IconCfg> "NSMiscCanvas::ArrangeFocusRing $oop 1"

	# Character name
	$c create text 0 0 -font $font -justify left \
		-anchor w -fill [Value TERM_L_BLUE] -tags {font name}
	CanvasFeedback::Add $c name "NSMiscCanvas::CanvasFeedbackCmd $oop name"

	# Click name to change it (wide layout) or display character info (tall)
	$c bind name <Enter> {
		%W itemconfigure name -fill gray60
	}
	$c bind name <Leave> {
		%W itemconfigure name -fill [Value TERM_L_BLUE]
	}
	$c bind name <ButtonPress-1> \
		"NSMiscCanvas::ClickName $oop"

	# Title
	$c create text 0 0 -font $font -justify left \
		-anchor w -fill [Value TERM_L_BLUE] -tags {font title}
	CanvasFeedback::Add $c title "NSMiscCanvas::CanvasFeedbackCmd $oop title"

	# Stat titles
	foreach title {STR INT WIS DEX CON CHR} stat [angband info stat_name] {
		$c create image 0 0 -image Image_Misc$title -anchor nw \
			-tags "$title pic,$title"
		$c create text 0 0 -text [mc $title] -font $font \
			-justify left -anchor nw -fill White \
			-tags "font $title txt,$title"
		CanvasFeedback::Add $c $title \
			"NSMiscCanvas::CanvasFeedbackCmd $oop $stat"
	}

	# Stats
	foreach stat [angband info stat_name] {
		$c create text 0 0 -font $font -justify right \
			-anchor ne -fill [Value TERM_L_GREEN] -tags "font $stat"
		CanvasFeedback::Add $c $stat \
			"NSMiscCanvas::CanvasFeedbackCmd $oop $stat"
	}

	# Race and Class
	foreach info {race class} {
		$c create text 0 0 -font $font -justify left \
			-anchor nw -fill [Value TERM_L_BLUE] -tags "font $info"
		CanvasFeedback::Add $c $info \
			"NSMiscCanvas::CanvasFeedbackCmd $oop $info"
	}

	# Level, Experience, Gold and Armor Class titles
	foreach title {LEVEL EXP AU AC} {
		$c create image 0 0 -image Image_Misc$title -anchor nw \
			-tags "$title pic,$title"
		$c create text 0 0 -text [mc $title] -font $font \
			-justify left -anchor nw -fill White \
			-tags "font $title txt,$title"
		CanvasFeedback::Add $c $title \
			"NSMiscCanvas::CanvasFeedbackCmd $oop $title"
	}

	# Level, Experience, Gold and Armor Class
	foreach info {level exp gold armor_class} title {LEVEL EXP AU AC} {
		$c create text 0 0 -font $font -justify right \
			-anchor ne -fill [Value TERM_L_GREEN] -tags "font $info"
		if {$title eq "EXP"} {set title exp}
		if {$title eq "AC"} {set title ac}
		CanvasFeedback::Add $c $info \
			"NSMiscCanvas::CanvasFeedbackCmd $oop $title"
	}

	# Equippy chars
	$c create rect 0 0 1 1 -tags highlight -fill gray20 -outline gray60 \
		-state hidden
	set slots [list \
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
	foreach slot $slots {
		$c create line 0 0 1 1 -tags timeout,$slot -state hidden \
			-fill Yellow
		$c create line 0 0 1 1 -tags curse,$slot -state hidden \
			-fill Red
		$c create text 0 0 -state hidden -font [Value font,equippy] \
			-fill White -anchor nw -tags "equippy $slot"
		CanvasFeedback::Add $c $slot \
			"NSMiscCanvas::CanvasFeedbackCmd $oop $slot"
	}
	qebind $c <Track-equipment> "NSMiscCanvas::Equippy $oop"

if 0 {
	foreach title {HP SP FD} attrib {hitpoints mana food} {
		$c create text 0 0 -text [mc $title] -font $font \
			-fill White -anchor nw -tags "font $title"
		$c create text 0 0 -text 99999 -font $font \
			-fill [Value TERM_L_GREEN] -anchor ne -tags "font $attrib"
		qebind $c <Py-$attrib> \
			"NSMiscCanvas::Update_HP_SP_FD $oop $attrib %c %m %f"
		CanvasFeedback::Add $c $title \
			"NSMiscCanvas::CanvasFeedbackCmd $oop $attrib"
		CanvasFeedback::Add $c $attrib \
			"NSMiscCanvas::CanvasFeedbackCmd $oop $attrib"
	}
}

	# When the mouse enters the "EXP" text, it toggles to ADV and shows
	# the points needed to reach the next level.
	$c bind EXP <Enter> {
		%W itemconfigure txt,EXP -fill gray60
	}
	$c bind EXP <Leave> {
		%W itemconfigure txt,EXP -fill White
	}
	$c bind EXP <ButtonPress-1> "NSMiscCanvas::ToggleEXP $oop"

	$c bind AC <Enter> {
		%W itemconfigure txt,AC -fill gray60
	}
	$c bind AC <Leave> {
		%W itemconfigure txt,AC -fill White
	}
	$c bind AC <ButtonPress-1> "NSMiscCanvas::ToggleAC $oop"

	qebind $c <Stat> "NSMiscCanvas::bind_Stat $oop %d"
	qebind $c <Py-armor_class> "NSMiscCanvas::bind_Py_ac $oop %c %t"
	qebind $c <Py-exp> "NSMiscCanvas::bind_Py_exp $oop %c %m %a"
	qebind $c <Py-gold> "NSMiscCanvas::bind_Py_value $oop %d %c"
	qebind $c <Py-level> "NSMiscCanvas::bind_Py_level $oop %c %m"
	qebind $c <Py-name> "NSMiscCanvas::bind_Py_value $oop %d %c"
	qebind $c <Py-title> "NSMiscCanvas::bind_Py_value $oop %d %c"

	if {[variant ZANGBANDTK]} {
		# Race can change in ZAngband!
		qebind $c <Py-race> "
			NSMiscCanvas::bind_Py_value $oop %d %c
			if {[Global autoAssign]} {
				IconCfg::AutoAssignCharacterIcon
			}
		"
	}

	# Update ourself when the fonts for the Misc Window changes
	qebind $c <Value-font,misc> "NSMiscCanvas::Arrange $oop"
	qebind $c <Value-font,equippy> "NSMiscCanvas::Arrange $oop"

	# Pop up a menu when the window is clicked
	set menu $c.popup
	menu $menu -tearoff 0
	bind $c <ButtonPress-3> \
		"NSMiscCanvas::ContextMenu $oop $menu %X %Y"

	Info $oop canvas $c

	return $c
}

# NSMiscCanvas::Set --
#
#	Set text of canvas items.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscCanvas::Set {oop} {

	set canvas [Info $oop canvas]

	$canvas itemconfigure icon -assign [angband player icon]

	if {[Value misc,mode,exp]} {
		$canvas itemconfigure txt,EXP -text [mc EXP]
	} else {
		$canvas itemconfigure txt,EXP -text [mc ADV]
	}
	bind_Py_exp $oop {*}[angband player exp]

	foreach stat [angband info stat_name] {
		bind_Stat $oop $stat
	}
	bind_Py_ac $oop {*}[angband player armor_class]

	foreach tag [list gold level name race class title] {
		bind_Py_value $oop $tag [angband player $tag]
	}
if 0 {
	foreach tag {hitpoints mana food} {
		Update_HP_SP_FD $tag {*}[angband player $tag]
	}
}
	Equippy $oop

	return
}

# NOT USED
if 0 {
proc NSMiscCanvas::Update_HP_SP_FD {oop which cur max frac} {

	set canvas [Info $oop canvas]

	if {$which ne "food"} {

		set hitpoint_warn [angband setting set hitpoint_warn]

		if {$cur >= $max} {
			set fill [Value TERM_L_GREEN]
		} elseif {$cur > ($max * $hitpoint_warn) / 10} {
			set fill [Value TERM_YELLOW]
		} else {
			if {$cur < 0} {set cur 0}
			set fill [Value TERM_L_RED]
		}

	} else {
		if {$cur < 500} {
			set fill [Value TERM_L_RED]
		} elseif {$cur < 1000} {
			set fill [Value TERM_ORANGE]
		} elseif {$cur < 2000} {
			set fill [Value TERM_YELLOW]
		} elseif {$cur < 15000} {
			set fill [Value TERM_L_GREEN]
		} else {
			set fill [Value TERM_GREEN]
		}
	}

	$canvas itemconfigure $which -fill $fill -text $cur

	return
}
}

# NSMiscCanvas::ContextMenu --
#
#	Pop up a context menu in the Misc Window to configure it's
# 	appearance.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscCanvas::ContextMenu {oop menu x y} {

	$menu delete 0 end

	$menu add command -label [mc "Set Font"] \
		-command "NSModule::LoadIfNeeded NSFont ; NSWindowManager::Display font misc"

	if {[winfo ismapped [Window misc]]} {
		set label [mc "Attach To Main"]
		set value 0
	} else {
		set label [mc Float]
		set value 1
	}
	$menu add command -label $label \
		-command "
		NSMainWindow::Info [Global main,oop] misc,float $value
		NSMainWindow::MenuInvoke [Global main,oop] ignore E_WINDOW_MISC
		"

	if {[Value misc,textLabels]} {
		set label [mc "Picture Labels"]
		set value 0
	} else {
		set label [mc "Text Labels"]
		set value 1
	}
	if {[Value misc,float]} {
		$menu add command -label $label \
			-command "Value misc,textLabels $value ;
				NSMiscWindow::SetLayout [Global misc,oop]"
	} else {
		$menu add command -label $label \
			-command "Value misc,textLabels $value
				NSMiscCanvas::Arrange $oop
				if {![Value progress,float]} {
					set width \[[Info $oop canvas] cget -width]
					[NSMainWindow::Info [Global main,oop] progress,canvas] configure -width \$width
					scan \[[Info $oop canvas] bbox all] {%d %d %d %d} left top right bottom
					place [NSMainWindow::Info [Global main,oop] progress,canvas] -x 0 -y \$bottom
				}"
	}

	if {[winfo ismapped [Window misc]]} {
		if {[Info $oop layout] eq "wide"} {
			set label [mc "Tall Layout"]
			set value tall
		} else {
			set label [mc "Wide Layout"]
			set value wide
		}
		$menu add command -label $label \
			-command "NSMiscWindow::SetLayout [Global misc,oop] $value"
	}

	$menu add separator
	$menu add command -label [mc Cancel]

	tk_popup $menu $x $y

	return
}

# NSMiscCanvas::bind_Stat --
#
#	Called in response to a <Stat> event. Updates the Misc Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscCanvas::bind_Stat {oop stat} {

	set canvas [Info $oop canvas]

	angband player stat $stat statInfo
	set max $statInfo(use)
	set top $statInfo(top)
	if {$max < $top} {
		set fill [Value TERM_YELLOW]
	} else {
		set fill [Value TERM_L_GREEN]
	}
	set value [cnv_stat_disp $max]
	set text [$canvas itemcget $stat -text]
	set fillCur [FlashCanvasTextFill $canvas $stat]
	if {$text ne $value || $fill ne $fillCur} {
		$canvas itemconfigure $stat -text $value
		FlashCanvasText $canvas $stat White $fill 4
	}

	return
}

# NSMiscCanvas::bind_Py --
#
#	Called in response to a <Py-armor_class> event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscCanvas::bind_Py_ac {oop dis_ac dis_to_a} {

	set canvas [Info $oop canvas]

	if {[Value misc,mode,armor_class]} {
		set ac [expr {$dis_ac + $dis_to_a}]
	} else {
		set ac [format "%d,%+d" $dis_ac $dis_to_a]
	}
	set fill [Value TERM_L_GREEN]
	set text [$canvas itemcget armor_class -text]
	if {$text ne $ac} {
		$canvas itemconfigure armor_class -text $ac
		FlashCanvasText $canvas armor_class White $fill 4
	}

	return
}

# NSMiscCanvas::bind_Py_exp --
#
#	Called in response to a <Py-exp> event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscCanvas::bind_Py_exp {oop cur max adv} {

	set canvas [Info $oop canvas]

	if {$cur < $max} {
		set fill [Value TERM_YELLOW]
	} else {
		set fill [Value TERM_L_GREEN]
	}
	if {![Value misc,mode,exp]} {
		if {$adv == 999999999} {
			set cur *****
		} else {
			set cur [expr {$adv - $cur}]
		}
	}
	set text [$canvas itemcget exp -text]
	set fillCur [FlashCanvasTextFill $canvas exp]
	if {$text ne $cur || $fill ne $fillCur} {
		$canvas itemconfigure exp -text $cur
		FlashCanvasText $canvas exp White $fill 4
	}

	return
}

# NSMiscCanvas::bind_Py_gold --
#
#	Called in response to a <Py-> event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscCanvas::bind_Py_value {oop tag value} {

	set canvas [Info $oop canvas]

	set text [$canvas itemcget $tag -text]
	if {$text ne $value} {
		set fill [FlashCanvasTextFill $canvas $tag]
		$canvas itemconfigure $tag -text $value
		FlashCanvasText $canvas $tag White $fill 4
	}

	return
}

# NSMiscCanvas::bind_Py_level --
#
#	Called in response to a <Py-level> event.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscCanvas::bind_Py_level {oop cur max} {

	set canvas [Info $oop canvas]

	if {$cur < $max} {
		set fill [Value TERM_YELLOW]
	} else {
		set fill [Value TERM_L_GREEN]
	}
	set text [$canvas itemcget level -text]
	if {$text != $cur} {
		$canvas itemconfigure level -text $cur
		FlashCanvasText $canvas level White $fill 4
	}

	# Hack -- Display experience as well
	bind_Py_exp $oop {*}[angband player exp]

	return
}

# NSMiscCanvas::ClickName --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscCanvas::ClickName {oop} {

	set c [Info $oop canvas]

	if {[Info $oop layout] eq "wide"} {
		NSGlobal::ChangeCharacterName [winfo toplevel $c]
	} else {
		DoCommandIfAllowed C
	}

	return
}

# NSMiscCanvas::ToggleAC --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscCanvas::ToggleAC {oop} {

	set c [Info $oop canvas]

	Value misc,mode,armor_class [expr {![Value misc,mode,armor_class]}]
	bind_Py_ac $oop {*}[angband player armor_class]
	after idle NSMiscCanvas::CanvasFeedbackCmd $oop AC enter

	return
}

# NSMiscCanvas::ToggleEXP --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscCanvas::ToggleEXP {oop} {

	set c [Info $oop canvas]

	Value misc,mode,exp [expr {![Value misc,mode,exp]}]
	if {[Value misc,mode,exp]} {
		$c itemconfigure txt,EXP -text [mc EXP]
	} else {
		$c itemconfigure txt,EXP -text [mc ADV]
	}
	bind_Py_exp $oop {*}[angband player exp]
	after idle NSMiscCanvas::CanvasFeedbackCmd $oop EXP enter

	return
}

namespace eval CanvasFeedback {
	variable Priv
}

# CanvasFeedback::Init --
#
#	Call a command whenever the mouse enters or leaves a canvas item.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc CanvasFeedback::Init {canvas} {

	$canvas bind StatusTextCanvasTag <Enter> {
		CanvasFeedback::Event %W enter
	}

	$canvas bind StatusTextCanvasTag <ButtonPress-1> {
		CanvasFeedback::Event %W press1
	}

	$canvas bind StatusTextCanvasTag <Leave> {
		CanvasFeedback::Event %W leave
	}

	return
}

proc CanvasFeedback::Event {canvas event} {

	variable Priv

	set item [$canvas find withtag current]
	if {[info exists Priv($canvas,command,$item)]} {
		eval $Priv($canvas,command,$item) $event
	}

	return
}

proc CanvasFeedback::Add {canvas tagOrId command} {

	variable Priv

	foreach itemId [$canvas find withtag $tagOrId] {
		$canvas addtag StatusTextCanvasTag withtag $itemId
		set Priv($canvas,command,$itemId) $command
		set Priv($canvas,tag,$itemId) $tagOrId
	}

#####
return
#####

	set itemId [lindex [$canvas find withtag $tagOrId] 0]

	set tagList [$canvas itemcget $itemId -tags]
	lappend tagList StatusTextCanvasTag
	$canvas itemconfigure $itemId -tags $tagList

	set Priv($canvas,command,$itemId) $command
	set Priv($canvas,tag,$itemId) $tagOrId

	return
}

proc NSMiscCanvas::CanvasFeedbackCmd {oop info action} {

	set canvas [Info $oop canvas]

	# Equippy
	if {[string match INVEN_* $info]} {
		switch -- $action {
			enter {
				angband equipment info $info attrib
				set string "$attrib(char)) $attrib(name)"
lassign [$canvas bbox $info] x1 y1 x2 y2
incr x1 -1 ; incr x2 1
$canvas coords highlight $x1 $y1 $x2 $y2
$canvas itemconfigure highlight -state normal -width 0
				if {$attrib(known) && $attrib(activate) && !$attrib(timeout)} {
					append string [mc ". Click to activate."]
$canvas itemconfigure highlight -width 1
				}
				NSMainWindow::StatusText [Global main,oop] $string
				NSRecall::RecallObject equipment $info
			}
			press1 {
				angband equipment info $info attrib
				if {$attrib(known) && $attrib(activate) && !$attrib(timeout)} {
					if {[angband inkey_flags] eq "INKEY_CMD"} {
						DoKeymapCmd {} A $attrib(char)
					}
				}
			}
			leave {
				NSMainWindow::StatusText [Global main,oop] {}
$canvas itemconfigure highlight -state hidden
			}
		}
		return
	}

	if {$action ne "enter"} {
		NSMainWindow::StatusText [Global main,oop] {}
		return
	}

	switch -- $info {
		strength -
		intelligence -
		wisdom -
		dexterity -
		constitution -
		charisma {
			angband player stat $info statInfo
			set max $statInfo(use)
			set top $statInfo(top)
			set message [format [mc "Your character's %s"] [mc $info]]
			if {$max < $top} {
				append message [format [mc " (max %s)"] [cnv_stat_disp $top]]
			}
		}
		icon {
#			set message [mc "Click for a menu of actions."]
			set message [mc "Click to display the Character Window."]
		}
		name {
			if {[Info $oop layout] eq "tall"} {
				set message [mc "Click to display the Character Window."]
			} else {
				set message [format [mc "Your character's %s"] [mc $info]]
				append message [mc ". Click to change."]
			}
		}
		class -
		race -
		title {
			set message [format [mc "Your character's %s"] [mc $info]]
		}
		ac -
		AC {
			set message [mc "Your character's armor class "]
			if {[Value misc,mode,armor_class]} {
				append message [mc "(total)"]
			} else {
				append message [mc "(base,bonus)"]
			}
			if {$info eq "AC"} {
				append message [mc ". Click to toggle."]
			}
		}
		AU {
			set message [mc "Your character's gold"]
		}
		exp -
		EXP {
			scan [angband player exp] "%d %d %d" cur max adv
			if {[Value misc,mode,exp]} {
				set message [mc "Current experience points"]
				if {$cur < $max} {
					append message [format [mc " (max %s)"] $max]
				}
			} else {
				set message [mc "Experience points for next level"]
				if {($cur < $max) && ($adv != 999999999)} {
					if {$max < $adv} {
						set max [expr {$adv - $max}]
						append message [format [mc " (min %d)"] $max]
					}
				}
			}
			if {$info eq "EXP"} {
				append message [mc ". Click to toggle."]
			}
		}
		LEVEL {
			set message [mc "Your character's experience level"]
			set max [angband player max_lev]
			if {[angband player lev] < $max} {
				append message [format [mc " (max %s)"] $max]
			}
		}
		food -
		hitpoints {
			set desc(food) "food level"
			set desc(hitpoints) "hit points"
			set message [format [mc "Your character's %s"] [mc $desc($info)]]
			scan [angband player $info] "%d %d %f" cur max frac
			if {$cur < $max} {
				append message [format [mc " (max %s)"] $max]
			}
		}
		mana {
			set message [format [mc "Your character's %s"] [mc "spell points"]]
			scan [angband player mana] "%d %d %f" cur max frac
			
			if {$cur < $max} {
				append message [format [mc " (max %s)"] $max]
			}
			scan [angband player mana_cumber "%d %d"] glove armor
			if {$glove > 0} {
				append message [format [mc " -%d gloves"] $glove]
			}
			if {$armor > 0} {
				append message [format [mc " -%d armor"] $armor]
			}
		}
	}

	NSMainWindow::StatusText [Global main,oop] $message

	return
}

proc NSMiscCanvas::ArrangeFocusRing {oop move} {

	set canvas [Info $oop canvas]

	set insetX [expr {22 - [icon width] / 2}]
	set insetY [expr {22 - [icon height] / 2}]
	set left [expr {$insetX - 2}]
	set top [expr {$insetY - 2}]
	set right [expr {$left + [icon width] + 2 + 1}]
	set bottom [expr {$top + [icon height] + 2 + 1}]
	$canvas coords focus $left $top $right $bottom

	# Make room for toolbar
	if {$move} {
		$canvas move focus 0 20
	}

	return
}

# NSMiscCanvas::ArrangeWide --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSMiscCanvas::ArrangeWide {oop} {

	set canvas [Info $oop canvas]

	# Get the desired font
	set font [Value font,misc]

	# Text or pic labels
	set useText [Value misc,textLabels]

	# Calculate the height of a line
	set fontHeight [font metrics $font -linespace]

	set rowHeight $fontHeight
	if {!$useText} {
		set picHeight 18
		if {$picHeight > $fontHeight} {
			set rowHeight $picHeight
		}
	}

	# Set the font of items with "font" tag
	$canvas itemconfigure font -font $font

	# Set the font of items with "equippy" tag
	$canvas itemconfigure equippy -font [Value font,equippy]

	$canvas itemconfigure icon -state normal
	$canvas itemconfigure name -anchor w
	$canvas itemconfigure title -anchor w

	foreach tag {equippy HP SP FD hitpoints mana food} {
		$canvas itemconfigure $tag -state hidden
	}

	# Position the character image
	$canvas coords icon 22 22

	# Focus ring
	ArrangeFocusRing $oop 0
	set topSpace 42

	# Character name
	$canvas coords name 45 [expr {20 - $fontHeight / 2}]

	# Title
	$canvas coords title 45 [expr {20 + $fontHeight / 2}]

	# Stat titles
	if {$useText} {
		set maxWidth 0
	} else {
		set maxWidth 18
	}
	set y [expr {$topSpace - $rowHeight}]
	foreach title {STR INT WIS DEX CON CHR} {
		if {$useText} {
			$canvas coords txt,$title 2 [incr y $rowHeight]
			set width [font measure $font [mc $title]]
			if {$width > $maxWidth} {
				set maxWidth $width
			}
			$canvas itemconfigure txt,$title -state normal
			$canvas itemconfigure pic,$title -state hidden
		} else {
			$canvas coords pic,$title 2 [incr y $rowHeight]
			$canvas itemconfigure pic,$title -state normal
			$canvas itemconfigure txt,$title -state hidden
		}
	}

	# Stats
	set pad 10
	set offset [expr {$maxWidth + $pad + [font measure $font 18/999]}]
	set y [expr {$topSpace - $rowHeight}]
	foreach stat [angband info stat_name] {
		$canvas coords $stat $offset [incr y $rowHeight]
	}
	set reqWidthLeft $offset

	# Race and Class
	incr offset 15
	set y [expr {$topSpace - $rowHeight}]
	set dy $rowHeight
	foreach info {race class} {
		$canvas coords $info $offset [incr y $dy]
	}

	# Level, Experience, Gold and Armor Class titles
	foreach title {LEVEL EXP AU AC} {
		if {$useText} {
			$canvas coords txt,$title $offset [incr y $rowHeight]
			$canvas itemconfigure txt,$title -state normal
			$canvas itemconfigure pic,$title -state hidden
		} else {
			$canvas coords pic,$title $offset [incr y $rowHeight]
			$canvas itemconfigure pic,$title -state normal
			$canvas itemconfigure txt,$title -state hidden
		}
	}

	# Level, Experience, Gold and Armor Class
	if {$useText} {
		set w 0
		foreach title {EXP ADV AU} {
			set w2 [expr {[font measure $font [mc $title]] + [font measure $font 99999999]}]
			if {$w2 > $w} {
				set w $w2
			}
		}
		incr offset [expr {$pad + $w}]
	} else {
		incr offset [expr {18 + $pad + [font measure $font 999999999]}]
	}

	set y [expr {$topSpace + $rowHeight * 2 - $rowHeight}]
	foreach info {level exp gold armor_class} {
		$canvas coords $info $offset [incr y $rowHeight]
	}
	set reqWidthRight [expr {$offset - $reqWidthLeft}]

	if {[Info $oop toolbarId] ne ""} {
		place [Info $oop toolbar,canvas] -relx 0.5 -y 0 -anchor n

		# Make room for the toolbar
		$canvas move all 0 20
	}

	scan [$canvas bbox armor_class] "%s %s %s %s" x1 y1 x2 y2
	if {!$useText} {
		scan [$canvas bbox pic,AC] "%s %s %s %s" x2 y2 x4 y4
		if {$y4 > $y2} {
			set y2 $y4
		}
	}

	# 1 or 2 pixel border on right and bottom edges
	if {$useText} {
		incr x2 1
		incr y2 1
	} else {
		incr x2 2
		incr y2 2
	}

	if {[Info $oop toolbarId] ne ""} {
		if {$x2 < [winfo reqwidth [Info $oop toolbar,canvas]]} {
			set x2 [winfo reqwidth [Info $oop toolbar,canvas]]
		}
	}

	# Resize the canvas, set the scroll region
	$canvas configure -width $x2 -height $y2 -scrollregion "0 0 $x2 $y2"

	if {[Info $oop toolbarId] ne ""} {
		# Set the width of the divider
		$canvas itemconfigure divider -state normal
		$canvas coords divider 2 22 [expr {$x2 - 2}] 22
	}

	set d [expr {$x2 - $reqWidthLeft - $reqWidthRight - 2}]
	foreach stat [angband info stat_name] {
		$canvas move $stat $d 0
	}
	foreach info {race class} {
		$canvas move $info $d 0
	}
	foreach title {LEVEL EXP AU AC} {
		$canvas move $title $d 0
	}
	foreach info {level exp gold armor_class} {
		$canvas move $info $d 0
	}

	return
}

proc NSMiscCanvas::ArrangeTall {oop} {

	set canvas [Info $oop canvas]

	# Get the desired font
	set font [Value font,misc]

	# Text or pic labels
	set useText [Value misc,textLabels]

	# Calculate the height of a line
	set fontHeight [font metrics $font -linespace]

	set rowHeight $fontHeight
	if {!$useText} {
		set picHeight 18
		if {$picHeight > $fontHeight} {
			set rowHeight $picHeight
		}
	}

	# Set the font of items with "font" tag
	$canvas itemconfigure font -font $font

	# Set the font of items with "equippy" tag
	$canvas itemconfigure equippy -font [Value font,equippy]

	$canvas itemconfigure icon -state hidden
	$canvas itemconfigure name -anchor nw
	$canvas itemconfigure title -anchor nw
if 0 {
	foreach tag {equippy HP SP FD hitpoints mana food} {
		$canvas itemconfigure $tag -state normal
	}
}
	# Name, Race, Class, Title
	set y 2
	foreach tag {name race class title} {
		$canvas coords $tag 2 $y
		incr y $fontHeight
	}

	# Level, Experience, Gold titles
	set top1 [expr {2 + $fontHeight * 4}]
	set y $top1
	foreach title {LEVEL EXP AU} {
		if {$useText} {
			$canvas coords txt,$title 2 $y
			$canvas itemconfigure txt,$title -state normal
			$canvas itemconfigure pic,$title -state hidden
		} else {
			$canvas coords pic,$title 2 $y
			$canvas itemconfigure pic,$title -state normal
			$canvas itemconfigure txt,$title -state hidden
		}
		incr y $rowHeight
	}

	set fontE [Value font,equippy]
	set fontHeightE [font metrics $fontE -linespace]
	set equippyHeight [expr {2 + $fontHeightE + 2}]

	# Stat titles
	set top2 [expr {$top1 + $rowHeight * 3 + $equippyHeight}]
	set y $top2
	foreach title {STR INT WIS DEX CON CHR} {
		if {$useText} {
			$canvas coords txt,$title 2 $y
			$canvas itemconfigure txt,$title -state normal
			$canvas itemconfigure pic,$title -state hidden
		} else {
			$canvas coords pic,$title 2 $y
			$canvas itemconfigure pic,$title -state normal
			$canvas itemconfigure txt,$title -state hidden
		}
		incr y $rowHeight
	}

	# AC title
	set top3 [expr {$top2 + $rowHeight * 6 + $fontHeight}]
	set y $top3
	if {$useText} {
		$canvas coords txt,AC 2 $y
		$canvas itemconfigure txt,AC -state normal
		$canvas itemconfigure pic,AC -state hidden
	} else {
		$canvas coords pic,AC 2 $y
		$canvas itemconfigure pic,AC -state normal
		$canvas itemconfigure txt,AC -state hidden
	}

	# HP SP FD
	set top4 [expr {$top3 + $rowHeight}]
	set y $top4
	foreach title {HP SP FD} {
		$canvas coords $title 2 $y
		incr y $fontHeight
	}

	# Level, Experience, Gold
	set pad 10
	if {$useText} {
		set maxWidth 0
		foreach title {EXP ADV AU} {
			set width [font measure $font [mc $title]]
			if {$width > $maxWidth} {
				set maxWidth $width
			}
		}
		set minRight [expr {2 + $maxWidth + $pad}]
	} else {
		set minRight 18
	}
	incr minRight [font measure $font 99999999]

	# Equippy
	set equippyWidth [font measure $fontE "012345678912"]
	set right [expr {2 + $equippyWidth}]
	if {$right > $minRight} {
		set minRight $right
	}

	# Stat
	if {$useText} {
		set maxWidth 0
		foreach title {STR INT WIS DEX CON CHR} {
			set width [font measure $font [mc $title]]
			if {$width > $maxWidth} {
				set maxWidth $width
			}
		}
		incr maxWidth [expr {2 + $pad}]
	} else {
		set maxWidth 18
	}
	set right [expr {$maxWidth + [font measure $font 18/999]}]
	if {$right > $minRight} {
		set minRight $right
	}

	if {$useText} {
		set right [expr {2 + [font measure $font [mc AC]] + $pad}]
	} else {
		set right 18
	}
	incr right [font measure $font 999,+999]
	if {$right > $minRight} {
		set minRight $right
	}

	# Name
	set width [font measure $font [angband player name]]
	if {$width > $minRight} {
		set minRight $width
	}

	# Make room for any attached NSProgressCanvas
	if {![Value progress,float]} {
		set progressOop [Info $oop progress,oop]
		set progressWidth [NSProgressCanvas::NeededWidth $progressOop]
		if {$progressWidth > $minRight} {
			set minRight $progressWidth
		}
	}

	# Level, Experience, Gold values
	set y $top1
	foreach tag {level exp gold} {
		$canvas coords $tag $minRight $y
		incr y $rowHeight
	}

	# Equippy
	set slots [list \
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
	set padx [expr {($minRight - $equippyWidth) / ([llength $slots] - 1)}]
	set x 2
	incr x [expr {($minRight - ($equippyWidth + $padx * ([llength $slots] - 1)))/2}]
	set y [expr {$top1 + $rowHeight * 3 + 2}]
	set slotW [font measure $fontE W]
	foreach slot $slots {
		$canvas coords timeout,$slot [expr {$x + 1}] [expr {$y + $fontHeightE}] \
			[expr {$x + $slotW}] [expr {$y + $fontHeightE}]
		$canvas coords curse,$slot [expr {$x + 1}] [expr {$y + $fontHeightE + 1}] \
			[expr {$x + $slotW}] [expr {$y + $fontHeightE + 1}]
		$canvas coords $slot $x $y
		incr x $slotW
		incr x $padx
	}

	# Stat values
	set y $top2
	foreach stat [angband info stat_name] {
		$canvas coords $stat $minRight $y
		incr y $rowHeight
	}

	# AC value
	set y $top3
	$canvas coords armor_class $minRight $y
if 0 {
	# HP SP FD
	set y $top4
	foreach option {hitpoints mana food} {
		$canvas coords $option $minRight $y
		incr y $fontHeight
	}
}
	if {[Info $oop toolbarId] ne ""} {
		place forget [Info $oop toolbar,canvas]
		$canvas itemconfigure divider -state hidden
	}
if 1 {
	scan [$canvas bbox all] "%s %s %s %s" x1 y1 x2 y2
} else {
	scan [$canvas bbox food] "%s %s %s %s" x1 y1 x2 y2
}
	# 1 or 2 pixel border on right and bottom edges
	if {$useText} {
		incr x2 1
		incr y2 1
	} else {
		incr x2 2
		incr y2 2
	}

	# Resize the canvas, set the scroll region
	$canvas configure -width $x2 -height $y2 -scrollregion "0 0 $x2 $y2"

	# Set the width of the divider
	$canvas coords divider 2 22 [expr {$x2 - 2}] 22

	# Needed when the progress canvas is in the Main Window because it
	# doesn't get a <Configure> when the Misc canvas is resized.
	if {![Value progress,float]} {
		[NSProgressCanvas::Info $progressOop canvas] configure -width $x2
	}

	return
}

proc NSMiscCanvas::Arrange {oop} {

	set layout [Info $oop layout]

	switch -- $layout {
		wide { ArrangeWide $oop }
		tall { ArrangeTall $oop }
	}

	Set $oop

	return
}

proc NSMiscCanvas::Equippy {oop} {

	if {[Info $oop layout] eq "wide"} return

	set canvas [Info $oop canvas]

	set slots [list \
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
	set index [const INVEN_WIELD]
	foreach slot $slots {
		if {[struct set inventory $index k_idx]} {
			set char [struct info inventory $index char]
			set attr [struct info inventory $index attr]
			set name [lindex [angband info term_attr] $attr]
			$canvas itemconfigure $slot -text $char -fill [Value $name] \
				-state normal

			# Recharging?
			angband equipment info $slot attrib
			if {$attrib(known) && $attrib(activate) && $attrib(timeout) > 0} {
				$canvas itemconfigure timeout,$slot -state normal
			} else {
				$canvas itemconfigure timeout,$slot -state hidden
			}

			# Cursed?
			set flags [angband equipment flags $slot]
			if {[lsearch -glob $flags *CURSE*] != -1} {
				$canvas itemconfigure curse,$slot -state normal
			} else {
				$canvas itemconfigure curse,$slot -state hidden
			}
		} else {
			$canvas itemconfigure timeout,$slot -state hidden
			$canvas itemconfigure curse,$slot -state hidden
			$canvas itemconfigure $slot -state hidden
		}
		incr index
	}

	return
}

