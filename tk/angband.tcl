# File: angband.tcl

# Purpose: script commands called by Angband

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

# angband_close_game --
#
#	Called by Angband when the game is about to exit.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc angband_close_game {} {

	global Angband

	if {[catch {

		if {0 && [Value options,autosave]} {

		}
		if {[Value window,autosave]} {
			WriteGeometryFile
		}

		if {[Global music,avail]} {

			# Remember the music volume
			Value music,volume [music volume]
			Value music,mute [music mute]
		}

		# Remember the sound volume
		Value sound,volume [angband sound volume]

		Config::Alternate::Write
		Config::Assign::Write
		Config::Feature::Write
		Config::Music::Write
		Config::Sound::Write
		Config::Sprite::Write
		Config::CharIcon::Write

		NSValueManager::CloseModule

		if {[info exists ::Global(photoText)]} {
			set tempFile [Global photoText]
			if {[string length $tempFile] && [file exists $tempFile]} {
				file delete $tempFile
			}
		}

		# Auto-save macros and keymaps to user/controls.prf. This is to
		# support the new Controls Window so the user doesn't have to manually
		# save the keyboard assignments.
		NSModule::LoadIfNeeded NSControls
		NSControls::WriteControlsDotPrf

	} result]} {
		set message "An error occured during shutdown:\n    $result"
		tk_messageBox -title "$Angband(name) Error" -message $message \
			-icon error
	}

	return
}

# angband_display --
#
#	Called by Angband to hide/show a window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc angband_display {window action args} {

	global Angband
	global Display
	global PYPX

	if {$action eq "hide"} {

		switch -- $window {

			floor -
			inven -
			equip {
				if {[Value inventory,style] eq "new"} {
					if {[Inventory2Obj Info browsing]} return
					set window inventory2
				} else {
					if {[InventoryObj Info browsing]} return
					set window inventory
				}
#eval RequestDisplay $window $action $args
#return
			}

			info {
				# Don't show the window when the Borg is *Identifying*
				if {[Global borg,active]} return
			}

			macros {
				set window prf
			}

			map {
				set mainId [Global main,oop]
				set mapId [Global bigmap,mapId]
				place forget [NSMap::Info $mapId frame]
				pack forget [Global mapdetail,widget]
if 1 {
				pack [Global micromap,widget] \
					-expand yes -fill both
} else {
				place [Global micromap,widget] \
					-relx 0.5 -rely 0.5 -anchor center
}
				# Do this in case window resized, or symbols changed
				[Global micromap,widget] center {*}$PYPX

				# Focus for KeyPress bindings
				focus [Window main]
				return
			}

			message {
				set window messages
			}

			player {
				set window character
			}

			store {
if {[Global borg,active]} return
				if {[Value store,style] eq "new"} {
					set window store2
				} else {
					set window store
				}
			}
		}

		# Undisplay the window
		if {[info exists NSWindowManager::Priv($window,win)]} {
			NSWindowManager::Undisplay $window {*}$args
		} else {
			wm withdraw [Window $window]
		}

		# Restore focus
		catch {focus $Display($window,oldFocus)}

		set Display(window) none

	} else {

		switch -- $window {

			kingly {
				if {[catch {
					NSModule::LoadIfNeeded NSTomb
					NSTomb::KinglyWindow
				} result]} {
					HandleError $result
				}
				return
			}

			tomb {
				if {[catch {
					NSModule::LoadIfNeeded NSTomb
					NSTomb::TombWindow
				} result]} {
					HandleError $result
				}
				return
			}

			book {
				NSModule::LoadIfNeeded NSBookWindow
#eval RequestDisplay $window $action $args
#return
			}

			building {
				# KANGBANDTK, ZANGBANDTK
				NSModule::LoadIfNeeded NSBuilding
			}

			choosemonster {
				NSModule::LoadIfNeeded NSChooseMonster
			}

			displayinfo {
				NSModule::LoadIfNeeded NSDisplayInfo
			}

			equip {
				if {[Value inventory,style] eq "new"} {
					NSModule::LoadIfNeeded NSInventory2
					set window inventory2
				} else {
					NSModule::LoadIfNeeded NSInventory
					set window inventory
				}
				set args [concat [list equipment ""] $args]
#eval RequestDisplay $window $action $args
#return
			}

			floor {
				if {[Value inventory,style] eq "new"} {
					NSModule::LoadIfNeeded NSInventory2
					set window inventory2
				} else {
					NSModule::LoadIfNeeded NSInventory
					set window inventory
				}
				set args [list floor ""]
#eval RequestDisplay $window $action $args
#return
			}	

			help {
				NSModule::LoadIfNeeded NSHelp
			}

			highscore {

				if {[catch {
					set loaded [NSModule::LoadIfNeeded NSHighScore]
				} result]} {
					HandleError $result
				}

				# Hack -- If the character-record option window is showing,
				# then it means the game is over, and we are about to
				# display the High Scores Window. To ensure there is
				# always an active frontmost window, we don't hide the
				# c-r'opt window until after the High Scores Window is
				# displayed.
				if {[angband player is_dead]} {

					if {[catch {

						# The High Scores Window was previously loaded
						if {!$loaded} {

							# No longer transient to the Main Window
							wm transient [Window highscore] ""
							if {[string compare [info patchlevel] 8.3.3] < 0} {
								unset ::WindowIcon([Window highscore])
							}
							wm geometry [Window highscore] ""

							# wm transient call above causes Character Record
							# window to appear in 8.4.7
							wm withdraw [Window record]

							# Make sure the dead-character record is read
							NSRecord::LoadRecords
						}

						# Important for positioning and yview'ing
						NSToplevel::NaturalSize [Window highscore] ""

						NSHighScore::SetList [Global highscore,oop]

						# Position the window
						WindowPosition [Window highscore] 2 3

						# Make sure the window is active
						focus [Window highscore]

						# Now destroy the Character Record Options Window
						destroy .record

						# XXX Hack -- The Character Record Window
						# is centered after character death.
						set NSWindowManager::Priv(record,setup) 0
						set NSWindowManager::Priv(record,geomRequest) ""
						wm geometry [Window record] ""

						focus [NSHighScore::Info [Global highscore,oop] tree]

						# Show the window, and wait
						tkwait window [Window highscore]

					} result]} {
						set prompt "The following error occured while trying to display the High Scores Window"
						HandleError $result $prompt
					}

					# Done
					return
				}
				if {[variant KANGBANDTK ZANGBANDTK]} {
					wm title [Window highscore] [lindex $args 0]
					set args {}
				}
			}

			info {
if {[Global borg,active]} return
				NSModule::LoadIfNeeded NSInfoWindow
				NSInfoWindow::SetList [Global info,oop] [lindex $args 0] \
					[lindex $args 1]
			}

			inven {
				if {[Value inventory,style] eq "new"} {
					NSModule::LoadIfNeeded NSInventory2
					set window inventory2
				} else {
					NSModule::LoadIfNeeded NSInventory
					set window inventory
				}
				set args [concat [list inventory ""] $args]
#eval RequestDisplay $window $action $args
#return
			}

			knowledge {
				NSModule::LoadIfNeeded NSKnowledge
			}

			map {
				set mainId [Global main,oop]
				set mapId [Global bigmap,mapId]
				place [NSMap::Info $mapId frame] -x 0 -y 0 \
					-relwidth 1.0 -relheight 1.0
if 1 {
				pack forget [Global micromap,widget]
} else {
				place forget [Global micromap,widget]
}
				pack [Global mapdetail,widget] \
					-expand yes -fill both
				update idletasks

				# We must map the widget so it may resize to fit its parent
				# before setting the view. Also, the scrollbars must be
				# mapped before they will update properly.
				NSMap::SetView $mapId {*}$PYPX
				[Global mapdetail,widget] center {*}$PYPX

				# Focus for KeyPress bindings
				focus [Global bigmap,widget]
				return
			}

			message {
				NSModule::LoadIfNeeded NSMessageHistory
				set window messages
			}

			macros {
				NSModule::LoadIfNeeded NSPrfWindow
				set window prf
			}

			mindcraft {
				# ZANGBANDTK
				NSModule::LoadIfNeeded NSMindcraftWindow
			}

			options {
				NSModule::LoadIfNeeded NSOptions
			}

			pets {
				NSModule::LoadIfNeeded NSPets
			}

			player {
				NSModule::LoadIfNeeded NSCharacterWindow
				set window character
			}

			playerflags {
				NSModule::LoadIfNeeded NSPlayerFlags
			}

			power {
				# ZANGBANDTK
				NSModule::LoadIfNeeded NSPower
			}

			store {
if {[Global borg,active]} return
				if {[Value store,style] eq "new"} {
					NSModule::LoadIfNeeded NSStore2
					set window store2
				} else {
					NSModule::LoadIfNeeded NSStore
					set window store
				}
			}

			default {
				error "unknown window \"$window\""
			}
		}

		# The window isn't already displayed
		if {$Display(window) != $window} {

			# Save current focus
			set Display($window,oldFocus) [focus]

			set Display(window) $window
		}

		# Display the window
		if {[info exists NSWindowManager::Priv($window,win)]} {
			NSWindowManager::Display $window {*}$args
		} else {
			WindowBringToFront [Window $window]
		}
	}

	# Update the windows now
	update

	return
}

if 0 {

set RequestDisplay(window) {}

qebind RequestDisplay <Term-inkey> {
	FreshDisplay
}
qeconfigure RequestDisplay <Term-inkey> -active no

proc FreshDisplay {} {

	global RequestDisplay

	while {[llength $RequestDisplay(window)]} {

		set window [lindex $RequestDisplay(window) 0]
		set action [lindex $RequestDisplay(action) 0]
		set args [lindex $RequestDisplay(args) 0]

		eval HandleRequest $window $action $args

		PopRequest
	}

	qeconfigure RequestDisplay <Term-inkey> -active no

	return
}

proc HandleRequest {window action args} {

	global Display

	if {$action eq "show"} {

		# The window isn't already displayed
		if {$Display(window) ne $window} {

			# Save current focus
			set Display($window,oldFocus) [focus]

			set Display(window) $window
		}

		# Display the window
		if {[info exists NSWindowManager::Priv($window,win)]} {
			eval NSWindowManager::Display $window $args
		} else {
			WindowBringToFront [Window $window]
		}

	# Hide the window
	} else {

		# Undisplay the window
		if {[info exists NSWindowManager::Priv($window,win)]} {
			eval NSWindowManager::Undisplay $window $args
		} else {
			wm withdraw [Window $window]
		}

		# Restore focus
		catch {focus $Display($window,oldFocus)}

		set Display(window) none
	}

	return
}

proc RequestDisplay {window action args} {

	global RequestDisplay

	if {[llength $RequestDisplay(window)]} {
		set window2 [lindex $RequestDisplay(window) end]
		if {$window eq $window2} {
			PopRequest
		}
	}

	eval PushRequest $window $action $args

	qeconfigure RequestDisplay <Term-inkey> -active yes

dbwin "RequestDisplay $window $action $args\n"

	return
}

proc PushRequest {window action args} {

	global RequestDisplay

	lappend RequestDisplay(window) $window
	lappend RequestDisplay(action) $action
	lappend RequestDisplay(args) $args

	return
}

proc PopRequest {} {

	global RequestDisplay

	set RequestDisplay(window) [lrange $RequestDisplay(window) 1 end]
	set RequestDisplay(action) [lrange $RequestDisplay(action) 1 end]
	set RequestDisplay(args) [lrange $RequestDisplay(args) 1 end]

	return
}

# 0
}

# angband_prompt --
#
#	Called by Angband to prompt the user. Rather complex interface to
#	handle multiple messages per line, and getting a string from the
#	user. This routine builds the string to be displayed in the
#	Message Window. It is not actually displayed until a Term-fresh
#	quasi-event occurs, at which point Fresh_Prompt() is called.
#	This behaviour prevents unwanted redraws during macro invocations.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

set Prompt(new) {}
set Prompt(old) {}

qebind PROMPT <Term-fresh> Fresh_Prompt
qeconfigure PROMPT <Term-fresh> -active no

proc Fresh_Prompt {} {

	global Prompt

	if {$Prompt(new) ne $Prompt(old)} {
		set wText [Global message,message]
		$wText delete 1.0 end
		if {[llength $Prompt(new)]} {
			$wText insert end {*}$Prompt(new)
		}
		set Prompt(old) $Prompt(new)
	}

	qeconfigure PROMPT <Term-fresh> -active no

	return
}

proc angband_prompt {action args} {

	global Prompt

	if {[Global borg,active]} return

	switch -- $action {

		open {
			set Prompt(new) $args
			set Prompt(prefix) $args
		}

		update {
			set Prompt(new) [concat $Prompt(prefix) $args]
		}

		set {
			set Prompt(new) $args
		}

		append {
			set Prompt(new) [concat $Prompt(new) $args]
		}

		wipe {
			set Prompt(new) {}
		}
	}

	qeconfigure PROMPT <Term-fresh> -active yes

	return
}

# NSModule::AddModule NSMessageLine [file join $Angband(dirTK) message-line.tcl]
# NSModule::LoadIfNeeded NSMessageLine

# angband_store --
#
#	A bit of ugliness to handle the haggle prompts in a store.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc angband_store {action {arg1 ""}} {

if {[Global borg,active]} return

	if {[Value store,style] eq "new"} {
		set command Store2Obj
		set window store2
	} else {
		set command StoreObj
		set window store
	}

	set win [Window $window]

	switch -- $action {

		haggle_open -
		haggle_close {
			$command HaggleSetup $action
		}

		price_character -
		price_owner {
			$win.info.$action configure -text $arg1
		}
	}

	return
}

# angband_borg --
#
#	C-to-Tcl Borg callback.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

Global borg,active 0
Global borg,initialized 0

proc angband_borg {command} {

	if {![NSModule::Exists NSBorg]} {
		tk_messageBox -title "No Borg" -icon error \
			-message "The Borg is not installed."
		return
	}

	switch -- $command {
		preinit {

			# Load (but don't display) the Borg Window. Always do this
			# so that the preference files will be loaded.
			NSModule::LoadIfNeeded NSBorg

			# Determine which DLL to use
			set prefix [Value borg,prefix]
			set shLib borg[info sharedlibextension]
			if {[string length $prefix] &&
				[file exists [Path borg $prefix $shLib]]} {

				# Tell the binary which directory to use
				borg path [Path borg $prefix]

				# Tell the binary which DLL to load
				borg dll [Path borg $prefix $shLib]

				# Remember the prefix. "Value borg,prefix" may be changed
				# by the user in the Borg Window, but "Global borg,prefix"
				# must not change once the borg is loaded.
				Global borg,prefix $prefix

			# No DLL is specified. Display the Borg Window so the user
			# can choose one.
			} else {
				NSWindowManager::Display borg
			}
		}
		postinit {
			NSBorg::Initialize
			Global borg,initialized 1
		}
		activate {
			Global borg,active 1

			set widget [Global main,widget]

			qeconfigure $widget <Cursor-hide> -active no
			qeconfigure $widget <Cursor-show> -active no
			qeconfigure $widget <Target-set> -active no
			qeconfigure $widget <Target-unset> -active no
			qeconfigure $widget <Target-visibility> -active no
			qeconfigure $widget <Track-grid> -active no

			if {[Global borg,detach_interface]} {

				# Disable Main Window
				$widget configure -noupdate yes
				qeconfigure $widget <Position> -active no
				qeconfigure $widget <Inkey> -active no
				qeconfigure $widget <Track-health> -active no

				# Disable status messages
				qeconfigure $widget <Status> -active no

				# Disable Micro Map
				set widget [Global micromap,widget]
				$widget configure -noupdate yes
				qeconfigure MicroMap <Position> -active no

				# Disable Misc Window
				if {[Value misc,float]} {
					set object [NSMiscWindow::Info [Global misc,oop] canvas]
				} else {
					set object [NSMainWindow::Info [Global main,oop] misc,canvas]
				}
				qeconfigure $object <Stat> -active no
				foreach detail {armor_class exp gold level} {
					qeconfigure $object <Py-$detail> -active no
				}

				# Disable Progress Window
				if {[Value progress,float]} {
					set object [NSProgressWindow::Info [Global progress,oop] canvas]
				} elseif {[Value misc,float]} {
					set object [NSMiscWindow::Info [Global misc,oop] progress,canvas]
				} else {
					set object [NSMainWindow::Info [Global main,oop] progress,canvas]
				}
				foreach detail {hitpoints mana food} {
					qeconfigure $object <Py-$detail> -active no
				}
				qeconfigure $object <Track-inventory> -active no

				# Disable monster recall
				if {[Value recall,show]} {
					qeconfigure NSRecall <Track-race> -active no
				}

				# Disable Choice Window
				if {[info exists ::Windows(choice)] && [winfo ismapped [Window choice]]} {
					qeconfigure NSChoiceWindow <Choose> -active no
					qeconfigure NSChoiceWindow <Track> -active no
				}

				# Disable Messages Window
				if {[info exists ::Windows(message2)] && [winfo ismapped [Window message2]]} {
					qeconfigure NSMessagesWindow <Track-message> -active no
				}

				# Zero delay factor
				Borg::SettingSet delay_factor 0
			}
		}
		deactivate {
			Global borg,active 0

			set widget [Global main,widget]

			qeconfigure $widget <Cursor-hide> -active yes
			qeconfigure $widget <Cursor-show> -active yes
			qeconfigure $widget <Target-set> -active yes
			qeconfigure $widget <Target-unset> -active yes
			qeconfigure $widget <Target-visibility> -active yes
			qeconfigure $widget <Track-grid> -active yes

			if {[Global borg,detach_interface]} {

				# Enable Main Window
				$widget configure -noupdate no
				qeconfigure $widget <Position> -active yes
				qeconfigure $widget <Inkey> -active yes
				qeconfigure $widget <Track-health> -active yes

				# Enable status messages
				qeconfigure $widget <Status> -active yes

				# Enable Micro Map
				set widget [Global micromap,widget]
				$widget configure -noupdate no
				qeconfigure MicroMap <Position> -active yes

				# Enable Misc Window
				if {[Value misc,float]} {
					set object [NSMiscWindow::Info [Global misc,oop] canvas]
				} else {
					set object [NSMainWindow::Info [Global main,oop] misc,canvas]
				}
				qeconfigure $object <Stat> -active yes
				foreach detail {armor_class exp gold level} {
					qeconfigure $object <Py-$detail> -active yes
				}

				# Enable Progress Window
				if {[Value progress,float]} {
					set object [NSProgressWindow::Info [Global progress,oop] canvas]
				} elseif {[Value misc,float]} {
					set object [NSMiscWindow::Info [Global misc,oop] progress,canvas]
				} else {
					set object [NSMainWindow::Info [Global main,oop] progress,canvas]
				}
				foreach detail {hitpoints mana food} {
					qeconfigure $object <Py-$detail> -active yes
				}
				qeconfigure $object <Track-inventory> -active yes

				# Enable monster recall
				if {[Value recall,show]} {
					qeconfigure NSRecall <Track-race> -active yes
				}

				# Enable Choice Window
				if {[info exists ::Windows(choice)] && [winfo ismapped [Window choice]]} {
					qeconfigure NSChoiceWindow <Choose> -active yes
					qeconfigure NSChoiceWindow <Track> -active yes
				}

				# Enable Messages Window
				if {[info exists ::Windows(message2)] && [winfo ismapped [Window message2]]} {
					qeconfigure NSMessagesWindow <Track-message> -active yes
				}

				# Enable delay factor
				Borg::SettingSet delay_factor [Global borg,delay_factor]
			}
		}
		rebirth {

			# Display race and class
			set canvas [Global misc,canvas]
			$canvas itemconfigure class -text [angband player class]
			$canvas itemconfigure race -text [angband player race]

			# Choose a new icon
			if {[Global autoAssign]} {
				IconCfg::AutoAssignCharacterIcon
			}

			# Clean up temp files
			if {[info exists ::Global(photoText)]} {
				set tempFile [Global photoText]
				if {[string length $tempFile] && [file exists $tempFile]} {
					file delete $tempFile
				}
			}
		}
	}

	return
}

# angband_generate --
#
#	Called by Angband after a level is generated. This is a debug command
#	which can be used to wait for a level to be created meeting certain
#	criteria. For example, it is possible to wait until a certain Artifact
#	or Unique is created.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

if {$DEBUG} {

# Set this to 1 to look for "interesting" levels
set angband_generate_flag 0

# Avoid infinited loops
set angband_generate_count 0

# Wait for an artifact
set angband_generate_artifact 0

# Set if a themed level is generated
set saw_themed_level 0

qebind GENERATE <Dungeon-enter> {
	set angband_generate_count 0
	set saw_themed_level 0

	# Autmatically skip -more- messages
	if {$angband_generate_flag} {
		qeconfigure GENERATE <Inkey-more> -active no
	}
}

# Autmatically skip -more- messages
qebind GENERATE <Inkey-more> {angband keypress \033}
qeconfigure GENERATE <Inkey-more> -active no

# Autmatically skip -more- messages
qebind GENERATE <Dungeon-leave> {
	if {$angband_generate_flag} {
		qeconfigure GENERATE <Inkey-more> -active yes
	}
}

proc angband_generate {} {

	global angband_generate_count
	global angband_generate_flag
	global angband_generate_artifact

	if {!$angband_generate_flag} {

		# Accept the level
		return 1
	}

	dbwin "$angband_generate_count..."

	# Prevent infinite loops
	if {$angband_generate_count > 500} {return 1}
	incr angband_generate_count

	# Always stop on themed levels
	if {$::saw_themed_level} {
		dbwin "okay! (themed level)\n"
		return 1
	}

	if {$angband_generate_artifact} {
		set match [angband o_list find -field name1 != 0]
		foreach o_idx $match {
			set desc [angband o_list info $o_idx name]
			dbwin "\n$desc"
		}
		if {[llength $match]} {
			dbwin "\nokay! (artifact)\n"
			return 1
		}
	}

	if {[variant OANGBANDTK]} {
		# Wait for any artifact wand
		foreach o_idx [angband o_list find -field k_idx != 0] {

			array set attrib [angband o_list set $o_idx]

			# Bug: Saw object 492 but name2 was zero!
			if {($attrib(k_idx) >= 492) && ($attrib(k_idx) <= 495)} {
				dbwin "okay\n"
				return 1
			}

			if {$attrib(name1) && $attrib(tval) eq "TV_WAND"} {
				dbwin "okay! (artifact wand)\n"
				return 1
			}
		}
	}

	dbwin "nope\n"

	return 0
}

# not DEBUG
} else {

proc angband_generate {} {

	# Accept the level
	return 1
}

# not DEBUG
}

proc angband_command {command} {

	global Display
	switch -- $command {
		e -
		i {
			return 0
		}
		default {
			if {$Display(window) eq "inventory"} {
				NSWindowManager::Undisplay inventory
				catch {focus $Display($window,oldFocus)}
				set Display(window) none
			}
			return 0
		}
	}
}
