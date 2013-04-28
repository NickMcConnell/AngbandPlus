# File: building.tcl

# Purpose: the Building Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSBuilding {

	variable MenuString
	variable Priv

# namespace eval NSBuilding
}

# NSBuilding::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::InitModule {} {

	global Angband
	variable Image
	variable Action2Image

	MsgCatInit building

	# Symbolic names for images in the townactions.gif file
	set names [list \
		Sword Fruit1 Fruit2 CompareWeapons Fruit4 ResearchMonster Fruit6 Throne \
		Shield Fruit7 Fruit8 Fruit9 Fruit10 Fruit11 Fruit12 \
		Quest Armor Dice1 Dice2 Dice3 Dice4 Dice5 Dice6 King \
		Skull Card1 Card2 Card3 Card4 Card5 Card6 Exit \
		Wheel Rules Rest Food Rumors Research RaceLegends TownHistory \
		Lord Plaque EnchantWeapon EnchantArmor \
			MasterThief ShareOfGold Identify LearnSpell \
		Warden Healing Shrine Poster \
			EnterArena InBetween Craps DruidLord \
		MasterRanger Priest Wizard ShadowLord \
			Restoration EnchantBow EnchantArrow LearnPrayer \
		Wall Recharge Trees Spires \
			Slots Bust EnchantWeapon2 EnchantArmor2 \
	]

	InitImageIfNeeded Image_TownActions townactions.gif

	set columns [expr {[image width Image_TownActions] / 32}]

	# Calculate the location in townactions.gif of each sub-image.
	# The sub-images are only created when needed.
	set col 0
	set row 0
	foreach name $names {
		set Image($name,x) [expr {$col * 32}]
		set Image($name,y) [expr {$row * 32}]
		set Image($name,exists) 0
		if {[incr col] == $columns} {
			set col 0
			incr row
		}
	}

	# This command is called when we actually create a graphical
	# button for a building action. We do this on demand to speed
	# things up.
	proc GetImage {name} {

		variable Image

		# Set the Tk image name
		set image Image_Town$name

		# Has this image been created?
		if {!$Image($name,exists)} {

			# Create the new image
			image create photo $image -width 32 -height 32

			# Copy image data from townactions.gif
			set x $Image($name,x)
			set y $Image($name,y)
			$image copy Image_TownActions -from $x $y \
				[expr {$x + 32}] [expr {$y + 32}]

			# Create the mask by hand, because Tk is too slow
			photomask $image

			# The image was created
			set Image($name,exists) 1
		}

		# Return the Tk image name
		return $image
	}

	# Map images to building commands. In KAngband these are hard-coded
	if {[variant KANGBANDTK]} {
if 1 {

		set a 0
		set Action2Image([incr a]) Research
		set Action2Image([incr a]) TownHistory
		set Action2Image([incr a]) RaceLegends
		incr a ; # BACT_UNUSED_1
		set Action2Image([incr a]) King
		set Action2Image([incr a]) Quest
		set Action2Image([incr a]) ShareOfGold
		set Action2Image([incr a]) Poster
		set Action2Image([incr a]) Rules
		set Action2Image([incr a]) EnterArena
		set Action2Image([incr a]) Plaque ; # ArenaLegends
		set Action2Image([incr a]) InBetween
		set Action2Image([incr a]) Rules
		set Action2Image([incr a]) Craps
		set Action2Image([incr a]) Wheel
		set Action2Image([incr a]) Slots
		set Action2Image([incr a]) Rest
		set Action2Image([incr a]) Food
		incr a ; # BACT_UNUSED_2
		set Action2Image([incr a]) ResearchMonster
		set Action2Image([incr a]) CompareWeapons
		set Action2Image([incr a]) RaceLegends
		set Action2Image([incr a]) EnchantWeapon
		set Action2Image([incr a]) EnchantArmor
		set Action2Image([incr a]) Recharge
		set Action2Image([incr a]) Identify
		incr a ; # BACK_UNUSED_3
		set Action2Image([incr a]) Healing
		set Action2Image([incr a]) Restoration
		set Action2Image([incr a]) EnchantArrow
		set Action2Image([incr a]) EnchantBow

		# BACT_LEGENDS image varies by building
		set Action2Image(22,0) RaceLegends ; # Library
		set Action2Image(22,1) King ; # Castle
		set Action2Image(22,2) Plaque ; # Arena
		set Action2Image(22,7) Plaque ; # Fighter's Hall
		set Action2Image(22,8) Spires ; # Tower of Sorcery
		set Action2Image(22,9) Bust ; # Inner Temple
		set Action2Image(22,10) Wall ; # Thieves Guild
		set Action2Image(22,11) Plaque ; # Rangers Tavern
		set Action2Image(22,12) Shrine ; # Paladins Order
		set Action2Image(22,13) Spires ; # Tower of Illusion
		set Action2Image(22,14) Trees ; # Tower of Illusion

# not 1
} else {
		set name Arena
		set Action2Image($name,p) Poster
		set Action2Image($name,r) Rules
		set Action2Image($name,a) EnterArena
		set Action2Image($name,l) Plaque
	
		set name Beastmaster
		set Action2Image($name,r) ResearchMonster
	
		set name Castle
		set Action2Image($name,g) Throne
		set Action2Image($name,l) King
		set Action2Image($name,q) Quest
	
		set name "Fighter's Hall"
		set Action2Image($name,g) Lord
		set Action2Image($name,l) Plaque
		set Action2Image($name,w) EnchantWeapon2
		set Action2Image($name,a) EnchantArmor2
	
		set name "Gambling House"
		set Action2Image($name,b) InBetween
		set Action2Image($name,c) Craps
		set Action2Image($name,d) Slots
		set Action2Image($name,r) Rules
		set Action2Image($name,s) Wheel
	
		set name "Grove of the Druids"
		set Action2Image($name,g) DruidLord
		set Action2Image($name,l) Trees
		set Action2Image($name,h) Healing
		set Action2Image($name,r) Restoration
		set Action2Image($name,z) LearnPrayer
	
		set name "House of Thieves"
		set Action2Image($name,g) MasterThief
		set Action2Image($name,l) Wall
		set Action2Image($name,s) ShareOfGold
		set Action2Image($name,r) Rest
		set Action2Image($name,i) Identify
		set Action2Image($name,z) LearnSpell
	
		set name "Inn"
		set Action2Image($name,r) Rest
		set Action2Image($name,f) Food
		set Action2Image($name,u) Rumors
	
		set name "Inner Temple"
		set Action2Image($name,g) Priest
		set Action2Image($name,l) Bust
		set Action2Image($name,h) Healing
		set Action2Image($name,r) Restoration
		set Action2Image($name,z) LearnSpell
	
		set name "Library"
		set Action2Image($name,a) Research
		set Action2Image($name,h) TownHistory
		set Action2Image($name,l) RaceLegends
	
		set name "Order of Paladins"
		set Action2Image($name,g) Warden
		set Action2Image($name,l) Shrine
		set Action2Image($name,a) EnchantArmor
		set Action2Image($name,h) Healing
		set Action2Image($name,z) LearnSpell
	
		set name "Ranger's Tavern"
		set Action2Image($name,g) MasterRanger
		set Action2Image($name,l) Plaque
		set Action2Image($name,a) EnchantArrow
		set Action2Image($name,b) EnchantBow
		set Action2Image($name,z) LearnSpell
	
		set name "Tower of Illusion"
		set Action2Image($name,g) ShadowLord
		set Action2Image($name,l) Spires
		set Action2Image($name,r) Recharge
		set Action2Image($name,i) Identify
		set Action2Image($name,z) LearnSpell
	
		set name "Tower of Sorcery"
		set Action2Image($name,g) Wizard
		set Action2Image($name,l) Spires
		set Action2Image($name,r) Recharge
		set Action2Image($name,i) Identify
		set Action2Image($name,z) LearnSpell
	
		set name "Weaponsmaster"
		set Action2Image($name,c) CompareWeapons
# 0
}

	# KANGBANDTK
	}
	# Map images to building commands.
	if {[variant ZANGBANDTK]} {

		# Hack -- images for Recall and Teleport Level buttons use
		# the scroll images
		InitImageIfNeeded Image_DEG_Misc dg_misc32.gif

		image create photo Image_TownRecall -width 32 -height 32
		set x [expr {4 * 32}]
		set y [expr {13 * 32}]
		Image_TownRecall copy Image_DEG_Misc -from $x $y \
			[expr {$x + 32}] [expr {$y + 32}]
		photomask Image_TownRecall
		set Image(Recall,exists) 1

		image create photo Image_TownTeleportLevel -width 32 -height 32
		set x [expr {3 * 32}]
		set y [expr {12 * 32}]
		Image_TownTeleportLevel copy Image_DEG_Misc -from $x $y \
			[expr {$x + 32}] [expr {$y + 32}]
		photomask Image_TownTeleportLevel
		set Image(TeleportLevel,exists) 1

		image delete Image_DEG_Misc

		set a 0
		set Action2Image([incr a]) Research
		set Action2Image([incr a]) TownHistory
		set Action2Image([incr a]) RaceLegends
		set Action2Image([incr a]) Throne
		set Action2Image([incr a]) King
		set Action2Image([incr a]) Quest
		set Action2Image([incr a]) ShareOfGold
		set Action2Image([incr a]) Poster
		set Action2Image([incr a]) Rules
		set Action2Image([incr a]) EnterArena
		set Action2Image([incr a]) ArenaLegends
		set Action2Image([incr a]) InBetween
		set Action2Image([incr a]) Rules
		set Action2Image([incr a]) Craps
		set Action2Image([incr a]) Wheel
		set Action2Image([incr a]) Slots
		set Action2Image([incr a]) Rest
		set Action2Image([incr a]) Food
		set Action2Image([incr a]) Rumors
		set Action2Image([incr a]) ResearchMonster
		set Action2Image([incr a]) CompareWeapons
		set Action2Image([incr a]) RaceLegends
		set Action2Image([incr a]) EnchantWeapon
		set Action2Image([incr a]) EnchantArmor
		set Action2Image([incr a]) Recharge
		set Action2Image([incr a]) Identify
		set Action2Image([incr a]) LearnPrayer
		set Action2Image([incr a]) Healing
		set Action2Image([incr a]) Restoration
		set Action2Image([incr a]) EnchantArrow
		set Action2Image([incr a]) EnchantBow

		# BACT_GREET -> Ranger, Wizard, etc
		set Action2Image([incr a]) MasterRanger

		# BACT_RECALL
		set Action2Image([incr a]) Recall

		# BACT_TELEPORT_LEVEL
		set Action2Image([incr a]) TeleportLevel

		# BACT_CURE_MUTATION
		set Action2Image([incr a]) Restoration

	# ZANGBANDTK
	}

	NSObject::New NSBuilding

	return
}

# NSBuilding::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::CloseModule {} {

	catch {
		destroy [Window building]
	}

	return
}

# NSBuilding::NSBuilding --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::NSBuilding {oop} {

	InitWindow $oop

	set win [Info $oop win]
	
	NSWindowManager::RegisterWindow building [Info $oop win] \
		"GetDefaultGeometry $win main2 main" "" \
		"NSBuilding::DisplayCmd $oop"

	# Update ourself when the listBG value changes
	qebind NSBuilding <Value-listBG> \
		"NSBuilding::ValueChanged_listBG $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSBuilding $oop $win

	#
	# Global list of application windows
	#

	Global building,oop $oop
	Window building $win

	return
}

# NSBuilding::~NSBuilding --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::~NSBuilding {oop} {

	return
}

# NSBuilding::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::Info {oop info args} {

	global NSBuilding

	# Verify the object
	NSObject::CheckObject NSBuilding $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSBuilding($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSBuilding($oop,$info)
			}
		}
	}

	return
}

# NSBuilding::InitWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::InitWindow {oop} {

	set win .building$oop
	toplevel $win
#	wm title $win Building

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSBuilding::Close $oop"

	Info $oop win $win

	InitMenus $oop

	#
	# Divider + Building Info
	#

if 0 {
	if {[Platform unix]} {
		set font {Courier 12}
	}
	if {[Platform windows]} {
		set font {Courier 9}
	}
}
	set font [Value font,fixed]

	set frame $win.info
	frame $frame \
		-borderwidth 0
	MakeDivider $frame.divider1 x
	label $frame.playerGold \
		-font $font -text [mc "Gold Remaining:"]
	label $frame.gold \
		-font $font -text [angband player gold]

	# Update the display when the character's gold changes
	qebind $frame.gold <Py-gold> {%W configure -text %c}
	# binding is deleted with the label

	#
	# Canvas
	#

	frame $win.frame \
		-borderwidth 1 -relief sunken
	canvas $win.frame.canvas \
		-width 400 -height 300 -background [Value listBG] \
		-borderwidth 0 -highlightthickness 0

	Info $oop canvas $win.frame.canvas

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 12

	#
	# Geometry
	#

	pack $win.frame.canvas -expand yes -fill both

	grid rowconfig $win 0 -weight 0
	grid rowconfig $win 1 -weight 1
	grid rowconfig $win 2 -weight 0
	grid columnconfig $win 0 -weight 1

	grid $win.info \
		-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew
	grid $win.frame \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.statusBar \
		-row 2 -column 0 -rowspan 1 -columnspan 2 -sticky ew

	#
	# Context Menu
	#
	
	set m $win.context
	menu $m -tearoff 0
	bind [Info $oop canvas] <Button-3> \
		"NSBuilding::ContextMenu $oop $m %X %Y"

	#
	# Feed Term when keys pressed
	#

	Term_KeyPress_Bind $win
	Term_KeyPress_Bind [Info $oop canvas]

	bind $win.frame.canvas <ButtonPress-1> {
		if {[angband inkey_flags] eq "INKEY_MORE"} {
			angband keypress " "
			break
		}
	}

	return
}

# NSBuilding::InitMenus --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::InitMenus {oop} {

	variable MenuString

	set win [Info $oop win]

	#
	# Menu bar
	#

	set mbar [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSBuilding::SetupMenus $oop" -identifier MENUBAR]
	Info $oop mbarId $mbar

	# Context-sensitive help
	NSMenu::Info $mbar menuSelectCmd "NSBuilding::MenuSelect $oop"

	#
	# Building Menu
	#

	NSObject::New NSMenu $mbar -tearoff 0 -identifier MENU_BUILDING
	NSMenu::MenuInsertEntry $mbar -end MENUBAR -type cascade \
		-menu MENU_BUILDING -label [mc Building] -underline 0 \
		-identifier M_BUILDING

	set entries {}
	lappend entries [list -type command -label [mc Leave] \
		-command "DoCommandIfAllowed \033" -identifier E_EXIT]
	
	NSMenu::MenuInsertEntries $mbar -end MENU_BUILDING $entries

	return
}

# NSBuilding::SetupMenus --
#
#	Description
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::SetupMenus {oop mbarID} {

	set identList {}
	
	if {[angband inkey_flags] eq "INKEY_CMD"} {
		lappend identList E_EXIT
	}

	NSMenu::MenuEnable $mbarID $identList

	return
}

# NSBuilding::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::MenuSelect {oop menuId index ident} {

	variable MenuString

	switch -glob -- $ident {
		{} {
			set desc {}
		}				
		E_EXIT {
			set buildingname [angband building buildingname]
			set desc "Leave the $buildingname."
		}
		default {
			if {[info exists MenuString($ident)]} {
				set desc $MenuString($ident)
			} else {
				set menu [NSMenu::Info $menuId menu]
				set desc [$menu entrycget $index -label]
			}
		}
	}

	set statusBar [Info $oop win].statusBar
	$statusBar cover set $desc
	if {($desc eq "") && ($menuId == [Info $oop mbarId])} {
		if {[$statusBar cover visible]} {
			$statusBar cover hide
		} else {
			$statusBar cover show
		}
	}

	return
}

# NSBuilding::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::DisplayCmd {oop message first} {

	switch -- $message {
		preDisplay {
			set win [Info $oop win]
			set buildingname [angband building buildingname]
			set ownername [angband building ownername]
			set ownerrace [angband building ownerrace]
			wm title $win [format [mc "The %s run by %s the %s"] \
				$buildingname $ownername $ownerrace]

			ConfigureWindow $oop
			DisplayCommands $oop
		}
		reDisplay {
			DisplayCommands $oop
		}
		postDisplay {
		}
	}

	return
}

# NSBuilding::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::Close {oop} {

	angband keypress \033

	return
}

# NSBuilding::ConfigureWindow --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::ConfigureWindow {oop} {

	set win [Info $oop win]
	set frame $win.info

	pack forget {*}[winfo children $frame]

	pack $frame.divider1 \
		-side top -expand yes -fill x

	# Character's Gold
	pack $frame.gold \
		-side right -expand no -padx 2
	pack $frame.playerGold \
		-side right -expand no -padx 2

	return
}

# NSBuilding::DisplayCommands --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::DisplayCommands {oop} {

	variable Action2Image

	set canvas [Info $oop canvas]

	$canvas delete all

	set buildingname [angband building buildingname]
	set b_idx [angband building index]

	set font {Times 18 bold}
	set fh [font metrics $font -linespace]
	$canvas create text 10 10 -text [format [mc "The %s"] $buildingname] \
		-fill White -font $font -anchor nw

	# Get a list of building actions
	set actionList [angband building actions]

	set x 10
	set top [expr {10 + $fh + 6}]
	set y $top
	set row 0
	set highlightThickness 4
	set highlightPad 2
	set buttonPadX 8
	set buttonPadY 4

	# Calculate width of a button
	set font [Value font,system]
	set labelWidth 0
	foreach action $actionList {
		array unset data
		array set data $action

		# See if the action costs money or is not available
		if {[string match "*gp)" $data(info)]} {
			scan $data(info) " (%dgp)" cost
			set data(info) [format [mc "Cost: %d gp"] $cost]
		} elseif {[string match "*(closed)" $data(info)]} {
			set closed 1
			set data(info) [mc CLOSED]
		}

		set width [font measure $font $data(label)]
		incr width [font measure $font "$data(char)\) "]
		if {$width > $labelWidth} {
			set labelWidth $width
		}

		if {$data(info) ne ""} {
			set width [font measure $font $data(info)]
			incr width [font measure $font "$data(char)\) "]
			if {$width > $labelWidth} {
				set labelWidth $width
			}
		}
	}
	set width [expr {$highlightThickness + $highlightPad + 32 + $highlightPad + $labelWidth + $highlightPad + $highlightThickness}]
	if {$width < 200} {
		set width 200
	}

	# Calculate height of a button
	set height [expr {$highlightThickness + $highlightPad + $highlightPad + $highlightThickness}]
	if {[font metrics $font -linespace] * 2 > 32} {
		incr height [expr {[font metrics $font -linespace] * 2}]
	} else {
		incr height 32
	}

	# Append Exit action
	lappend actionList [list char ESC label [mc "Exit building"] info "" action xxx]

	# Create a new button for each action
	foreach action $actionList {

		# List -> Array -> Variables
		array unset data
		array set data $action
		set cmdChar $data(char)
		set desc $data(label)
		set extra $data(info)
		set actionNum $data(action)

		# Assume the action is available to the character
		set closed 0

		# See if the action costs money or is not available
		if {[string match "*gp)" $extra]} {
			scan $extra " (%dgp)" cost
			set extra [format [mc "Cost: %d gp"] $cost]
		} elseif {[string match "*(closed)" $extra]} {
			set closed 1
			set extra [mc CLOSED]
		}

		# Get an image for the action
		set image ""
		if {$cmdChar eq "ESC"} {
			set image [GetImage Exit]
		} elseif {[info exists Action2Image($actionNum,$b_idx)]} {
			set image [GetImage $Action2Image($actionNum,$b_idx)]
		} elseif {[info exists Action2Image($actionNum)]} {
			if {[string length $Action2Image($actionNum)]} {
				set image [GetImage $Action2Image($actionNum)]
			}
		}

		# Create a button
		CreateButton $canvas $x $y $width $height $cmdChar $image $desc $extra $closed

		# Next row/column
		incr y $height
		incr y $buttonPadY
		if {[incr row] == 4} {
			incr x $width
			incr x $buttonPadX
			set y $top
		}
	}

	return
}

# NSBuilding::ContextMenu --
#
#	If the game is waiting for the user to choose an item, then
#	pop up a menu of choices.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::ContextMenu {oop menu x y} {

	# Waiting for an inventory item
	if {[angband inkey_flags] eq "INKEY_ITEM"} {

		# Display equipment/inventory items
		NSPopup::SelectItem [Info $oop win] $x $y
#		NSRecall::PopupSelect_Item $menu $x $y

		# Done
		return
	}

	return	
}

# NSBuilding::FruitColor --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::FruitColor {slot} {

	set colors [list "" Yellow Orange Gray Gray Violet Red]
	return [lindex $colors $slot]
}

# NSBuilding::CreateButton --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::CreateButton {canvas x y width height char image text extra closed} {

	if {$closed} {
		set fill #804040
	} else {
		set fill Brown
	}
	$canvas create rectangle [expr {$x + 2}] [expr {$y + 2}] \
		[expr {$x + $width - 2}] \
		[expr {$y + $height - 2}] -outline $fill -fill $fill -width 4 \
		-tags "button$char button$char,rect"
	$canvas create image [expr {$x + 4 + 2 + 16}] \
		[expr {$y + $height / 2}] -image $image -anchor center \
		-tags button$char

	set font [Value font,system]

	# Char
	$canvas create text [expr {$x + 4 + 2 + 32 + 2}] \
		[expr {$y + $height / 2}] -text "$char)" -anchor w -fill White \
		-tags button$char -font $font
	set offset [font measure $font "$char) "]

	# Text + Extra
	if {[string length $extra]} {
		append text \n$extra
	}
	$canvas create text [expr {$x + 4 + 2 + 32 + 2 + $offset}] \
		[expr {$y + $height / 2}] -text $text -anchor w -fill White \
		-tags button$char -font $font

	if {$closed} {
		foreach sequence [$canvas bind button$char] {
			$canvas bind button$char $sequence ""
		}
	} else {
		if {$char eq "ESC"} {
			set string "\033"
		} else {
			set string $char
		}
		$canvas bind button$char <ButtonPress-1> \
			"DoCommandIfAllowed $string"
		$canvas bind button$char <Enter> \
			"%W itemconfigure button$char,rect -outline Gold"
		$canvas bind button$char <Leave> \
			"%W itemconfigure button$char,rect -outline Brown"
	}

	return
}

# NSBuilding::CreateCard --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::CreateCard {canvas x y face} {

	$canvas create image $x $y -image [GetImage Card$face] -anchor nw \
		-tags temp

	return
}

# NSBuilding::CreateDie --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::CreateDie {canvas x y face} {

	$canvas create image $x $y -image [GetImage Dice$face] -anchor nw \
		-tags temp

	return
}

# NSBuilding::CreateFruit --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::CreateFruit {canvas x y slot} {

	set font [Value font,fixed]

	set gif(Lemon) Fruit10
	set gif(Orange) Fruit11
	set gif(Sword) Sword
	set gif(Shield) Shield
	set gif(Grape) Fruit12
	set gif(Cherry) Fruit9

	set name [lindex [list "" Lemon Orange Sword Shield Grape Cherry] $slot]

	$canvas create image $x $y -image [GetImage $gif($name)] -anchor n \
		-tags temp
	$canvas create text $x [expr {$y + 36}] \
		-font $font -fill [FruitColor $slot] -text $name -tags temp -anchor n

	return
}

# NSBuilding::CanvasAddTextItem --
#
#	Create a new canvas text item at the given "row" and "column".
#	Item positioning is determined by the size of the given font.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::CanvasAddTextItem {canvas font col row width fill text justify tags} {

	set fontWidth [font measure $font "W"]
	set fontHeight [font metrics $font -linespace]

	if {$justify eq "right"} {
		incr col $width
		set anchor ne
	} else {
		set anchor nw
	}
	set x [expr {$col * $fontWidth}]
	set y [expr {$row * $fontHeight}]
	set width [expr {$width * $fontWidth}]
	return [$canvas create text $x $y -fill $fill -text $text \
		-font $font -justify $justify \
		-anchor $anchor -tags $tags]
}

# NSBuilding::HandleGamblingHouse --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::HandleGamblingHouse {oop message args} {

	set canvas [Info $oop canvas]

	switch -- $message {

		craps {
			$canvas delete temp
			set font [Value font,fixed]
			set fh [font metrics $font -linespace]
			set fw [font measure $font W]
			CanvasAddTextItem $canvas $font 2 1 -1 Green "Craps" left temp
			set size 32
			set x [expr {$fw * (54 / 2) - (($size + 8) * 3 + 8) / 2}]
			set y [expr {$fh * 6 - $size / 2}]
			$canvas create rectangle $x $y [expr {$x + ($size + 8) * 3 + 8}] [expr {$y + $size + 16}] \
				-fill Green -outline "" -tags temp
			incr x 8 ; incr y 8
			set faces [lindex $args 0]
			CreateDie $canvas $x $y [lindex $faces 0]
			CreateDie $canvas [incr x [expr {$size + 8}]] $y [lindex $faces 1]
			set font {Times 18}
			$canvas create text [incr x [expr {$size + 8 + $size / 2}]] [expr {$y + $size / 2}] \
				-text [lindex $faces 2] -font $font -fill Black -tags temp -anchor center
		}

		diceslots {
			$canvas delete temp
			set font [Value font,fixed]
			set fh [font metrics $font -linespace]
			set fw [font measure $font W]
			CanvasAddTextItem $canvas $font 2 1 -1 Green "Dice Slots" left temp
			set x [expr {$fw * (54 / 2 - 3 * 10 / 2 + 10 / 2)}]
			set y [expr {$fh * 6}]
			scan [lindex $args 0] "%d %d %d" slot1 slot2 slot3
			CreateFruit $canvas $x $y $slot1
			CreateFruit $canvas [incr x [expr {$fw * 10}]] $y $slot2
			CreateFruit $canvas [incr x [expr {$fw * 10}]] $y $slot3
		}

		inbetween {
			$canvas delete temp
			set font [Value font,fixed]
			set fh [font metrics $font -linespace]
			set fw [font measure $font W]
			CanvasAddTextItem $canvas $font 2 1 -1 Green "In Between" left temp
			set size 32
			set x [expr {$fw * (54 / 2) - (($size + 8) * 3 + 8) / 2}]
			set y [expr {$fh * 6 - $size / 2}]
			$canvas create rectangle $x $y [expr {$x + ($size + 8) * 3 + 8}] [expr {$y + $size + 16}] \
				-fill Green -outline "" -tags temp
			incr x 8 ; incr y 8
			set faces [lindex $args 0]
			CreateCard $canvas $x $y [lindex $faces 0]
			CreateCard $canvas [incr x [expr {$size + 8}]] $y [lindex $faces 1]
			CreateCard $canvas [incr x [expr {$size + 8}]] $y [lindex $faces 2]
		}

		wheel {
			$canvas delete temp
			set font [Value font,fixed]
			set fh [font metrics $font -linespace]
			set fw [font measure $font W]
			CanvasAddTextItem $canvas $font 2 1 -1 Green "Wheel" left temp
			set radius 48
			set x [expr {$fw * 25}]
			set y [expr {$fh * 6}]
			for {set i 1} {$i <= 10} {incr i} {
				if {$i & 1} {
					set fill Blue
					set tag pieTag1
				} else {
					set fill Green
					set tag pieTag2
				}
				$canvas create arc [expr {$x - $radius}] \
					[expr {$y - $radius}] [expr {$x + $radius}] \
					[expr {$y + $radius}] -extent 36 \
					-start [expr {($i - 1) * 36}] \
					-fill $fill -tags "temp $tag"
			}
			scan [lindex $args 0] "%d %d" choice wheel
			set font2 {Times 18}

			set x2 [expr {$x - ($radius + 48)}]
			$canvas create text $x2 $y \
				-text $choice -font $font2 -fill White -anchor center -tags "temp choice"
			set y2 [lindex [$canvas bbox choice] 1]
			$canvas create text $x2 [expr {$y2 - 4}] \
				-text "You bet on:" -font $font -fill White -anchor s -tags temp

			set x2 [expr {$x + ($radius + 48)}]
			$canvas create text $x2 $y \
				-text $wheel -font $font2 -fill White -anchor center -tags "temp wheel"
			$canvas create text $x2 [expr {$y2 - 4}] \
				-text "This spin:" -font $font -fill White -anchor s -tags temp

			# Animation
			set fill1 Green ; set fill2 Blue
			for {set i 1} {$i <= 4} {incr i} {
				$canvas itemconfigure pieTag1 -fill $fill1
				$canvas itemconfigure pieTag2 -fill $fill2
				set temp $fill1 ; set fill1 $fill2 ; set fill2 $temp
				$canvas itemconfigure wheel -text [string index " 4792" $i]
				update idletasks ; after 20
			}
			$canvas itemconfigure wheel -text $wheel
		}

		loser {
			set wager [lindex $args 0]
			set font [Value font,fixed]
			CanvasAddTextItem $canvas $font 37 13 -1 Red "You lost $wager gp" left temp
			CanvasAddTextItem $canvas $font 37 15 -1 White "Play again? (y/n)" left temp
		}

		wager {

			# This is the first thing called for any gambling screen
			# Clear display completely here
			$canvas delete all

			set wager [lindex $args 0]
			set font [Value font,fixed]
			CanvasAddTextItem $canvas $font 2 15 -1 White [format "Current Wager:    %9ld" $wager] left {}

			set gold [angband player gold]
			CanvasAddTextItem $canvas $font 2 14 -1 White [format "Gold before game: %9ld" $gold] left {}
		}

		winner {
			scan [lindex $args 0] "%d %d" wager odds
			set font [Value font,fixed]
			CanvasAddTextItem $canvas $font 37 13 -1 Gold "YOU WON $wager gp" left temp
			CanvasAddTextItem $canvas $font 37 14 -1 White "Payoff $odds-to-1" left temp
			CanvasAddTextItem $canvas $font 37 15 -1 White "Play again? (y/n)" left temp
		}
	}

	return
}

# NSBuilding::EnchantItem --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::EnchantItem {oop message args} {

	variable Priv

	set canvas [Info $oop canvas]

	set font [Value font,fixed]
	set fh [font metrics $font -linespace]
	set fw [font measure $font W]

	switch -- $message {
		init {
			$canvas delete all
			CanvasAddTextItem $canvas $font 2 1 -1 \
				Green "Enchant Item" left temp
			set prompt [lindex $args 0]
			CanvasAddTextItem $canvas $font 2 3 -1 \
				White $prompt left temp
		}
		init2 {
			set prompt [lindex $args 0]
			CanvasAddTextItem $canvas $font 2 5 -1 \
				White $prompt left temp
		}
	}

	return
}

if {[variant ZANGBANDTK]} {

# NSBuilding::RechargeItem --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::RechargeItem {oop message args} {

	variable Priv

	set canvas [Info $oop canvas]

	set font [Value font,fixed]
	set fh [font metrics $font -linespace]
	set fw [font measure $font W]

	switch -- $message {
		init {
			$canvas delete all
			CanvasAddTextItem $canvas $font 2 1 -1 \
				Green "Recharge Item" left temp
		}
		info {
			scan [lindex $args 0] "%d %d %d" k_idx cost charges
			set name [lindex $args 1]
			CanvasAddTextItem $canvas $font 2 3 -1 \
				White "Recharging $name." left temp
			set name [angband k_info info $k_idx object_desc]
			set pval [angband k_info set $k_idx pval]
			set string "Each $name can accept $pval charges."
			CanvasAddTextItem $canvas $font 2 5 -1 \
				White $string left temp
			set string "A single charge will cost $cost gold."
			CanvasAddTextItem $canvas $font 2 7 -1 \
				Gold $string left temp
			set max_cost [expr {$charges * $cost}]
			set string "A full recharge will cost $max_cost gold."
			CanvasAddTextItem $canvas $font 2 8 -1 \
				Gold $string left temp
		}
	}

	return
}

# ZANGBANDTK
}

# UNUSED
if {[variant KANGBANDTK]} {

# NSBuilding::FixItem --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::FixItem {oop message args} {

	variable Priv

	set canvas [Info $oop canvas]

	set font [Value font,fixed]
	set fh [font metrics $font -linespace]
	set fw [font measure $font W]

	switch -- $message {
		init {
			$canvas delete all
			CanvasAddTextItem $canvas $font 2 1 -1 Green "Enchant Item" left temp
			set prompt [lindex $args 0]
			CanvasAddTextItem $canvas $font 2 3 -1 White $prompt left temp
			set Priv(fixitem,count) 0
		}
		display_one {
			set row [expr {$Priv(fixitem,count) * 3 + 5}]
			set desc [lindex $args 0]
			set result [lindex $args 1]
			CanvasAddTextItem $canvas $font 4 $row -1 White $desc left temp
			CanvasAddTextItem $canvas $font 8 [incr row] -1 White $result left temp
			incr Priv(fixitem,count)
		}
	}

	return
}

# KANGBANDTK
}

# NSBuilding::Quest --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::Quest {oop message args} {

	set canvas [Info $oop canvas]

	set font [Value font,fixed]
	set fh [font metrics $font -linespace]
	set fw [font measure $font W]

	switch -- $message {
		info {
			$canvas delete all
			set qname [lindex $args 0]
			set qlevel [lindex $args 1]
			set qtext [lindex $args 2]
			CanvasAddTextItem $canvas $font 2 1 -1 Green "Quest information (Danger level: $qlevel)" left temp
			CanvasAddTextItem $canvas $font 2 3 -1 White $qname left temp
			CanvasAddTextItem $canvas $font 4 5 -1 Yellow $qtext left "temp qtext"
			set y2 [lindex [$canvas bbox qtext] 3]
			set row [expr {$y2 / $fh + 1}]
			if {$y2 % $fh} {incr row}

			# UNUSED
			if {0 && [variant KANGBANDTK]} {
				set prompt "Complete this quest and return for a reward."
				CanvasAddTextItem $canvas $font 2 $row -1 White $prompt left temp
			}
		}
	}

	return
}

# NSBuilding::FlagText --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::FlagText {oop flag} {

	switch -- $flag {
		SLAY_ANIMAL {return "vs Animals"}
		SLAY_EVIL {return "vs Evil"}
		SLAY_UNDEAD {return "vs Undead"}
		SLAY_DEMON {return "vs Demons"}
		SLAY_ORC {return "vs Orcs"}
		SLAY_TROLL {return "vs Trolls"}
		SLAY_GIANT {return "vs Giants"}
		SLAY_DRAGON {return "vs Dragons"}
		KILL_DRAGON {return "vs Dragons"}
		BRAND_ACID {return "Acid brand"}
		BRAND_ELEC {return "Lightning brand"}
		BRAND_FIRE {return "Fire brand"}
		BRAND_COLD {return "Frost brand"}
		BRAND_POIS {return "Poison brand"}
		default {
			return $flag
		}
	}

	return
}

# NSBuilding::CompareWeapons --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

if {[variant KANGBANDTK]} {

proc NSBuilding::CompareWeapons {oop message args} {

	variable Priv

	set canvas [Info $oop canvas]

	set font [Value font,fixed]
	set fh [font metrics $font -linespace]
	set fw [font measure $font W]

	switch -- $message {
		init {
			$canvas delete all
			set prompt [lindex $args 0]
			CanvasAddTextItem $canvas $font 2 1 -1 Green $prompt left temp
			set Priv(compare,count) 0
		}
		list_weapon {
			set desc [lindex $args 0]
			scan [lindex $args 1] "%d %d %d %d %d %d %d" Priv(compare,to_h) \
				Priv(compare,to_d) Priv(compare,dd) Priv(compare,ds) \
				Priv(compare,num_blow) Priv(compare,p_ptr_to_h) \
				Priv(compare,p_ptr_to_d)

			incr Priv(compare,count)
			if {$Priv(compare,count) == 1} {
				set col 2
			} else {
				set col 40
			}
			set row 2
			CanvasAddTextItem $canvas $font $col [incr row] -1 Yellow $desc left temp

			set string "To Hit: $Priv(compare,to_h)   To Damage: $Priv(compare,to_d)"
			CanvasAddTextItem $canvas $font $col [incr row] -1 White $string left temp

			set string "Dice: $Priv(compare,dd)   Sides: $Priv(compare,ds)"
			CanvasAddTextItem $canvas $font $col [incr row] -1 White $string left temp

			set string "Number of Blows: $Priv(compare,num_blow)"
			CanvasAddTextItem $canvas $font $col [incr row] -1 White $string left temp

			incr row
			set string "Base Damage:"
			CanvasAddTextItem $canvas $font $col [incr row] -1 Yellow $string left temp

			incr col
			set dam1 [expr {$Priv(compare,dd) + $Priv(compare,to_d) + $Priv(compare,p_ptr_to_d)}]
			set dam2 [expr {$Priv(compare,dd) * $Priv(compare,ds) + $Priv(compare,to_d) + $Priv(compare,p_ptr_to_d)}]
			set string "One Strike: $dam1-$dam2 damage"
			CanvasAddTextItem $canvas $font $col [incr row] -1 White $string left temp

			set dam1 [expr {$Priv(compare,num_blow) * $dam1}]
			set dam2 [expr {$Priv(compare,num_blow) * $dam2}]
			set string "One Attack: $dam1-$dam2 damage"
			CanvasAddTextItem $canvas $font $col [incr row] -1 White $string left temp

			set Priv(compare,flagCount) 0
		}
		flag {
			set flag [lindex $args 0]
			scan [lindex $args 1] "%d" mult

			if {$Priv(compare,count) == 1} {
				set col 2
			} else {
				set col 40
			}
			set row [expr {11 + [incr Priv(compare,flagCount)]}]

			if {$Priv(compare,flagCount) == 1} {
				set string "Damage Multipliers: "
				CanvasAddTextItem $canvas $font $col 12 -1 Yellow $string left temp
			}

			incr col
			set string "[FlagText $oop $flag] (x$mult): "
			CanvasAddTextItem $canvas $font $col [incr row] -1 White $string left temp

			incr col [string length $string]
			set dam1 [expr {$Priv(compare,num_blow) * ($mult * $Priv(compare,dd) + $Priv(compare,to_d) + $Priv(compare,p_ptr_to_d))}]
			set dam2 [expr {$Priv(compare,num_blow) * ($mult * $Priv(compare,dd) * $Priv(compare,ds) + $Priv(compare,to_d) + $Priv(compare,p_ptr_to_d))}]
			set string "$dam1-$dam2 damage"
			CanvasAddTextItem $canvas $font $col $row -1 White $string left temp
		}
	}

	return
}

# KANGBANDTK
}

if {[variant ZANGBANDTK]} {

proc NSBuilding::CompareWeapons {oop message args} {

	variable Priv

	set canvas [Info $oop canvas]

	set font [Value font,fixed]
	set fh [font metrics $font -linespace]
	set fw [font measure $font W]

	switch -- $message {
		init {
			$canvas delete all
			set prompt [lindex $args 0]
			CanvasAddTextItem $canvas $font 2 1 -1 Green $prompt left temp
			set Priv(compare,count) 0
		}
		list_weapon {
			set desc [lindex $args 0]
			scan [lindex $args 1] "%d %d %d %d %d %d %d" Priv(compare,to_h) \
				Priv(compare,to_d) Priv(compare,dd) Priv(compare,ds) \
				Priv(compare,num_blow) Priv(compare,min_dam) \
				Priv(compare,max_dam)

			incr Priv(compare,count)
			if {$Priv(compare,count) == 1} {
				set col 2
			} else {
				set col 40
			}
			set row 2
			CanvasAddTextItem $canvas $font $col [incr row] -1 Yellow $desc left temp

			set string "To Hit: $Priv(compare,to_h)   Deadliness: $Priv(compare,to_d)"
			CanvasAddTextItem $canvas $font $col [incr row] -1 White $string left temp

			set string "Dice: $Priv(compare,dd)   Sides: $Priv(compare,ds)"
			CanvasAddTextItem $canvas $font $col [incr row] -1 White $string left temp

			set string "Number of Blows: $Priv(compare,num_blow)"
			CanvasAddTextItem $canvas $font $col [incr row] -1 White $string left temp

			incr row
			set string "Possible Damage:"
			CanvasAddTextItem $canvas $font $col [incr row] -1 Yellow $string left temp

			incr col
			set dam1 [expr {$Priv(compare,min_dam) / 100}]
			set dam2 [expr {$Priv(compare,max_dam) / 100}]
			set string "One Strike: $dam1-$dam2 damage"
			CanvasAddTextItem $canvas $font $col [incr row] -1 White $string left temp

			set dam1 [expr {($Priv(compare,min_dam) * $Priv(compare,num_blow)) / 100}]
			set dam2 [expr {($Priv(compare,max_dam) * $Priv(compare,num_blow)) / 100}]
			set string "One Attack: $dam1-$dam2 damage"
			CanvasAddTextItem $canvas $font $col [incr row] -1 White $string left temp

			set Priv(compare,flagCount) 0
		}
		flag {
			set flag [lindex $args 0]
			scan [lindex $args 1] "%d %d" min_dam max_dam

			if {$Priv(compare,count) == 1} {
				set col 2
			} else {
				set col 40
			}
			set row [expr {11 + [incr Priv(compare,flagCount)]}]

			if {$Priv(compare,flagCount) == 1} {
				set string "Damage Multipliers: "
				CanvasAddTextItem $canvas $font $col 12 -1 Yellow $string left temp
			}

			incr col
set mult 1
			set string "[FlagText $oop $flag] (x$mult): "
set string "[FlagText $oop $flag]: "
			CanvasAddTextItem $canvas $font $col [incr row] -1 White $string left temp

			incr col [string length $string]
			set dam1 $min_dam
			set dam2 $max_dam
			set string "$dam1-$dam2 damage"
			CanvasAddTextItem $canvas $font $col $row -1 White $string left temp
		}
	}

	return
}

# ZANGBANDTK
}

# NSBuilding::ValueChanged_listBG --
#
#	Called when the listBG value changes.
#	Updates the Building Window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBuilding::ValueChanged_listBG {oop} {

	set color [Value listBG]
	[Info $oop canvas] configure -background $color

	return
}

# angband_building --
#
#	A bit of ugliness to handle building irregularites.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc angband_building {message args} {

	set oop [Global building,oop]
	set win [NSBuilding::Info $oop win]
	set canvas [NSBuilding::Info $oop canvas]

	set font [Value font,fixed]

	switch -- $message {
		compare_weapons {
			NSBuilding::CompareWeapons $oop {*}$args
			return
		}
		enchant_item {
			# ZANGBANDTK
			NSBuilding::EnchantItem $oop {*}$args
			return
		}
		fix_item {
			# KANGBANDTK
			NSBuilding::FixItem $oop {*}$args
			return
		}
		quest {
			NSBuilding::Quest $oop {*}$args
			return
		}
		recharge_item {
			# ZANGBANDTK
			NSBuilding::RechargeItem $oop {*}$args
			return
		}
		words {
			$canvas delete all
			set text [lindex $args 0]
			NSBuilding::CanvasAddTextItem $canvas $font 2 1 -1 White $text left temp
			return
		}
		craps -
		diceslots -
		inbetween -
		wheel -
		loser -
		wager -
		winner	{
			NSBuilding::HandleGamblingHouse $oop $message {*}$args
		}
	}

	return
}
