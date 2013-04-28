# File: misc.tcl

# Purpose: various commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

# cnv_stat --
#
#	Converts a raw stat value into a human-readable value. Values from
#	3 to 18 are returned unchanged.
#		ex. "118" becomes "18/100"
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc cnv_stat {val} {

	# Above 18
	if {$val > 18} {

		set bonus [expr {$val - 18}];
		if {$bonus >= 10} {
			return 18/$bonus
		}
		return 18/0$bonus

	# From 3 to 18
	} else {

		return $val
	}
}

# cnv_stat_disp --
#
#	Same as cnv_stat(), but any bonus greater than 220 is displayed as
#	"***".
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc cnv_stat_disp {val} {

	# Above 18
	if {$val > 18} {

		set bonus [expr {$val - 18}];
		if {$bonus >= 220} {
			return 18/***
		} elseif {$bonus >= 10} {
			return 18/$bonus
		}
		return 18/0$bonus

	# From 3 to 18
	} else {

		return $val
	}
}

# ImageExists --
#
#	Return true if an image with the given name exists.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc ImageExists {imageName} {

	set imageList [image names]
	return [expr {[lsearch -exact $imageList $imageName] != -1}]
}

# InitImageIfNeeded --
#
#	Creates a new photo image from the given file. If the image
#	already exists, nothing happens.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc InitImageIfNeeded {imageName fileName args} {

	global Angband

	if {[ImageExists $imageName]} {
		return 0
	}

	# Look in CommonTk/tk/image and subdirectories, then user-supplied tk
	# subdirectories
	foreach elem [concat [list {image} {image dg} {image dg misc-win}] $args] {
		set path [CPathTk {*}$elem $fileName]
		if {[file exists $path]} {
			image create photo $imageName -file $path
			return 1
		}
	}

	# Look in tk/image and subdirectories, then user-supplied tk
	# subdirectories
	foreach elem [concat [list {image} {image dg} {image dg misc-win}] $args] {
		set path [PathTk {*}$elem $fileName]
		if {[file exists $path]} {
			image create photo $imageName -file $path
			return 1
		}
	}

	error "can't find image file \"$fileName\""
	
	return 1
}

# openlf --
#
#	Open a file for writing, and set the translation mode to "lf" as well.
#	It seems that writing unix-style files is faster as well as smaller.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc openlf {fileName} {

	set id [open $fileName w]
	fconfigure $id -translation lf
	return $id
}

# SetWindowIcon --
#
#	When a toplevel is mapped for the first time, set the icon,
#	if the window is not transient.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc SetWindowIcon {win} {

	global WindowIcon

	if {![info exists WindowIcon($win)]} {
		if {![string length [wm transient $win]]} {
			angband system windowicon $win
		}
		set WindowIcon($win) 1
	}

	return
}

if {[string compare [info patchlevel] 8.3.3] < 0} {

bind Toplevel <Map> {+
	SetWindowIcon %W
}

# The pesky root window does not have the Toplevel bindtag!!!
bind . <Map> {+
	SetWindowIcon .
}

# < 8.3.3
}

# lb_or_kg --
#
#	Return lb or kg.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc lb_or_kg {} {
	set units lb
	if {[variant OANGBANDTK]} {
		if {[Setting use_metric]} {
			set units kg
		}
	}
	return $units
}

if {[variant OANGBANDTK]} {

# make_metric --
#
#	Convert lb to kg.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc make_metric {wgt} {

	set metric_wgt [expr {$wgt * 10 / 22}];
	if {($wgt * 10) % 22 > 10} {
		incr metric_wgt
	}
	return $metric_wgt
}

# OANGBANDTK
}

namespace eval NSGlobal {

	# inven			GetItemCommand, fmt_wgt
	# player		ChangeCharacterName
	MsgCatInit inven player

# namespace eval NSGlobal
}

# fmt_wgt --
#
#	Convert 10ths of lb to ib, or kg.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc fmt_wgt {wgt {showUnits 0}} {
	return [NSGlobal::fmt_wgt $wgt $showUnits]
}
proc NSGlobal::fmt_wgt {wgt {showUnits 0}} {

	set units [lb_or_kg]
	if {$units eq "kg"} {
		set wgt [make_metric $wgt]
	}

	set result [format "%d.%d" [expr {$wgt / 10}] [expr {$wgt % 10}]]
	if {$showUnits} {
		set result [format [mc weight1_$units] $result]
	}
	return $result
}

# NSGlobal::ChangeCharacterName --
#
#	Allow the user to change the name of the character.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSGlobal::ChangeCharacterName {parent} {

	# Ask until a valid name is entered, or cancel
	while 1 {

		# Ask for a new name
		set name [NSUtils::StringBox -title [mc pyname-title] \
			-message [mc pyname-msg] \
			-initial [angband player name] -prompt [mc pyname-prompt] \
			-buttons [list [mc OK] [mc Cancel]] -parent $parent \
			-entrywidth 20 -maxlen 15]

		# Validate the name
		if {[string length $name]} {
			if {[catch {angband player name $name} result]} {
				tk_messageBox -icon info -title [mc pyname-invalid-title] \
					-message [mc pyname-invalid-msg]
				continue
			}
		}
		break
	}
	
	return
}

# FileCharacter --
#
#	Allow the user to choose a location, and save a character dump.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc FileCharacter {parent} {

	global Angband

	set initial [angband player base_name].txt
	set filename [tk_getSaveFile -initialfile $initial \
		-initialdir [PathUser] -parent $parent]
	if {![string length $filename]} return

	if {[catch {angband game file_character $filename} result]} {
		tk_messageBox -icon error -message $result
	}

	return
}

# MessageDumpAux --
#
#	Dump a list of messages to the given file. Similar messages are
#	combined into a single line.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc MessageDumpAux {filePath} {

	set max [angband message count]

	set curMsg ""
	set count 0
	
	for {set i [expr {$max - 1}]} {$i >= 0} {incr i -1} {
		set nextMsg [angband message get $i]
		if {$curMsg ne $nextMsg} {
			if {[string length $curMsg]} {
				if {$count > 1} {
					append curMsg " (x$count)"
				}
				append buffer $curMsg\n
			}
			set curMsg $nextMsg
			set count 1
		} else {
			incr count
		}
	}
	if {$count > 1} {
		append curMsg " (x$count)"
	}
	append buffer $curMsg\n

	if {[catch {openlf $filePath} fileId]} {
		set msg "The following error occurred while attempting to open "
		append msg "the message log file for writing:\n\n$fileId"
		tk_messageBox -title Oops -message $msg
		return 1
	}

	puts $fileId "# Message Dump for [angband player name]\n"
	puts $fileId $buffer
	close $fileId

	return 0
}

# MessageDump --
#
#	Dump a list of messages to a file. 
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc MessageDump {parent} {

	global Angband

	set fileName [tk_getSaveFile -initialfile [angband player base_name].msg \
		-initialdir [PathUser] -parent $parent]
	if {![string length $fileName]} return

	MessageDumpAux $fileName

	return
}

# AbortGame --
#
#	Quit without saving.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc AbortGame {} {

	global Angband

	set answer [tk_messageBox -type yesno -icon question -title "Abort $Angband(name)" \
		-message "Write config files?"]
	if {$answer eq "yes"} {
		angband_close_game
	}

	# In any event, delete the temp photo.txt
	if {[info exists ::Global(photoText)]} {
		set tempFile [Global photoText]
		if {[string length $tempFile] && [file exists $tempFile]} {
			file delete $tempFile
		}
	}

	# Bye!
	angband game abort -noask
	
	return
}

# MakeStatusBar --
#
#	Creates a typical status bar.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc MakeStatusBar {statusBar args} {

	NSModule::LoadIfNeeded NSStatusBar

	lappend sizes 0
	lappend weights 1
	lappend tags t1
	
	set i 2
	foreach size $args {
		lappend sizes $size
		lappend weights 0
		lappend tags t$i
		incr i
	}
	
	statusbar $statusBar -sizes $sizes -weights $weights -tags $tags \
		-font [Value font,statusBar]

	return
}

# MakeDivider --
#
#	Creates a typical divider.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc MakeDivider {divider orient} {

	if {[Platform unix]} {
		set relief sunken
	}
	if {[Platform windows]} {
		set relief groove
	}
	switch -- $orient {
		x {set option -height}
		y {set option -width}
	}

	return [frame $divider -borderwidth 1 $option 2 -relief $relief]
}

# Path --
#
#	Create a path relative to Angband(dir)
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Path {args} {

	global Angband

	return [file join $Angband(dir) {*}$args]
}

# PathTk --
#
#	Create a path relative to Angband(dirTK)
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc PathTk {args} {

	global Angband

	return [file join $Angband(dirTK) {*}$args]
}

# CPath --
#
#	Create a path relative to Angband(dir,common)
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc CPath {args} {

	global Angband

	return [file join $Angband(dir,common) {*}$args]
}

# CPathTk --
#
#	Create a path relative to Angband(dir,common,tk)
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc CPathTk {args} {

	global Angband

	return [file join $Angband(dir,common,tk) {*}$args]
}

# PathUser --
#
#	Create a path relative to Angband(dir,user)
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc PathUser {args} {

	global Angband

	return [file join $Angband(dir,user) {*}$args]
}

# FontAddStyle --
#
#	Add one or more styles to the given font description if needed.
#	If you just say "$font bold" and $font is "Times 12 {bold italic}"
#	you get an error.
#	FIXME: The font family may change (ex from Times to {Times New Roman}).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc FontAddStyle {font args} {

	array set desc [fontdesc $font]
	set result "[list $desc(-family)] $desc(-size)"
	foreach style $args {
		switch -- $style {
			bold {
				set desc(-weight) bold
			}
			italic {
				set desc(-slant) italic
			}
			overstrike {
				set desc(-overstrike) 1
			}
			underline {
				set desc(-underline) 1
			}
		}
	}
	if {$desc(-weight) eq "bold"} {
		append result " bold"
	}
	if {$desc(-slant) eq "italic"} {
		append result " italic"
	}
	if {$desc(-underline)} {
		append result " underline"
	}
	if {$desc(-overstrike)} {
		append result " overstrike"
	}
	return $result
}

# BoldFont --
#
#	Add "bold" style to the given font description if needed.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc BoldFont {font} {

	return [FontAddStyle $font bold]
}

# FontResize --
#
#	Set the size of the given font description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc FontResize {font size} {

	array set desc [fontdesc $font]
	if {[string match {+[0-9]*} $size]} {
		incr desc(-size) $size
	} else {
		set desc(-size) $size
	}

	return [array get desc]
}

# GetItemCommand --
#
#	Examine an item in equipment or inventory and return a good default
#	command for using it.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSGlobal::GetItemCommand {where index shopping} {

	set command ""
	set label ""

	angband $where info $index attrib
	set charItem $attrib(char)

	# The item is in the equipment
	if {$where eq "equipment"} {
		if {$attrib(known) && $attrib(activate)} {
			set label [mc Activate]
			set charCmd A
		# Refuel lantern if a flask is in inventory
		} elseif {$attrib(tval) eq "TV_LITE" &&
				  $attrib(sval) == 1} {
			set match [angband inventory find -tval TV_FLASK]
			if {[llength $match]} {
				angband inventory info [lindex $match 0] attrib
				set label [mc Refuel]
				set charCmd F
				set charItem $attrib(char)
			} else {
				set label [mc Remove]
				set charCmd t
			}
		} else {
			set label [mc Remove]
			set charCmd t
		}
		set command "DoKeymapCmd {} $charCmd $charItem"
		return [list $label $command]
	}

	# If this item can be sold, set the default action to "Sell"
	if {$shopping} {
		set match [angband inventory find -store_will_buy yes]
		if {[lsearch -integer $match $index] != -1} {
			set charCmd s
			if {[angband store ishome]} {
				set label [mc Drop]
			} else {
				set label [mc Sell]
			}
			set command "DoKeymapCmd {} $charCmd $charItem"
			return [list $label $command]
		}
	}

	switch -glob -- $attrib(tval) {
		*_BOOK {

			# See if the character can read this book
			if {[lsearch -exact [angband player spell_book] $attrib(tval)] != -1} {

				set label [mc Browse]
				set charCmd b

				# Hack -- Browse shows all the books
				set command "DoKeymapCmd {} $charCmd {}"
				return [list $label $command]
			}
		}
		TV_ARROW -
		TV_BOLT -
		TV_SHOT {
			if {[variant OANGBANDTK] && ($where ne "equipment")} {
				set label [mc Wield]
				set charCmd w
			} else {
				# See if the character can fire this ammo
				if {[variant ZANGBANDTK]} {
					set field tval_ammo
				} else {
					set field ammo_tval
				}
				if {[struct set player_type 0 $field] eq $attrib(tval)} {
					set label [mc Fire]
					set charCmd f
				}
			}
		}
		TV_FLASK {
			# See if light source is a lantern
			angband equipment info INVEN_LITE attrib
			if {$attrib(sval) == 1} {
				set label [mc Refuel]
				set charCmd F
			}
		}
		TV_FOOD {
			set label [mc Eat]
			set charCmd E
		}
		TV_POTION {
			set label [mc Drink]
			set charCmd q
		}
		TV_SCROLL {
			set label [mc Read]
			set charCmd r
		}
		TV_SPIKE {
			set label [mc Jam]
			set charCmd j
		}
		TV_STAFF {
			set label [mc Use]
			set charCmd u
		}
		TV_ROD {
			set label [mc Zap]
			set charCmd z
		}
		TV_WAND {
			set label [mc Aim]
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
			set label [mc Wield]
			set charCmd w
		}
	}

	if {[string length $label]} {
		set command "DoKeymapCmd {} $charCmd $charItem"
	}

	return [list $label $command]
}

# TransientToWin --
#
#	Makes a window transient to another, and applies -toolwindow attribute
#	on Win32.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc TransientToWin {win1 win2} {

	wm transient $win1 $win2

	# Starting in 8.4b1 [wm transient] doesn't force "tool window" frame
	if {[Platform windows]} {
		wm attribute $win1 -toolwindow yes
	}

	return
}

# ContentVisible --
#
#	Return true if all the canvas/text content fits horizontally or
#	vertically. 
#
# Arguments:
#	view					xview or yview
#
# Results:
#	True or false.

proc ContentVisible {win view} {

	if {[winfo class $win] eq "Text"} {
		ForceTextWidgetViewUpdate $win
	}

	set view [$win $view]
	return [expr {[lindex $view 0] == 0 && [lindex $view 1] == 1}]
}

# ForceTextWidgetViewUpdate --
#
#	Fix a bug found in Tk 8.5.6 where the yview of a text widget is not
#	recalculated after inserting text.
#
# Arguments:
#	w					a Text widget
#
# Results:
#	What happened.

proc ForceTextWidgetViewUpdate {w} {
	set yview1 [$w yview]
	# Tk wierdness: -update must come before -ypixels!
	$w count -update -ypixels 1.0 end
	set yview2 [$w yview]
#	if {$yview1 ne $yview2} {
#		dbwin "ForceTextWidgetViewUpdate $w\n"
#	}

return

	# Tk wierdness: -update must come before -ypixels!
	set count [$w count -update -ypixels 1.0 end] ; puts 1:$count
	$w configure -wrap [$w cget -wrap]
	set count2 [$w count -update -ypixels 1.0 end] ; puts 2:$count2
	if {$count ne $count2} {
		puts "ForceTextWidgetViewUpdate $w"
	}
	return
}

