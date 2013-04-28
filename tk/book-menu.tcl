# File: book-menu.tcl

# Purpose: manages a Book Menu

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSBookMenu {

	# Option: Bypass NSMenu::MenuEnable
	variable SetupFaster 0

	set Priv(hasStateCmd) \
		[expr {[llength [info command menuentrystate]] != 0}]

# namespace eval NSBookMenu
}

# NSBookMenu::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookMenu::InitModule {} {

	MsgCatInit book

	return
}

# NSBookMenu::NSBookMenu --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookMenu::NSBookMenu {oop mbarId} {

	variable SetupFaster

	# Remember the menubar we below to
	Info $oop mbar $mbarId

	# Create the menu of books
	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 \
		-postcommand "NSBookMenu::SetupMenus $oop" -identifier MENU_BOOK]
	NSMenu::Info $menuId setupCmd "NSBookMenu::SetupCmd $oop"
	Info $oop menuId,book $menuId
	Info $oop bookToMenu,book [NSMenu::Info $menuId menu]

	# Context-sensitive help
	NSMenu::Info $menuId menuSelectCmd \
		"NSBookMenu::MenuSelect $oop 0"

	# Append the menu to the menubar
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_BOOK -label [mc Book] -underline 0 -identifier M_BOOK

	# Keep a list of books with learnable spells
	set validBookList {}
	
	# Check each type of book the character can use
	foreach spell_book [angband player spell_book] {

		set entryIndex -1
		set bookIndex -1
		set highBookIndex -1

		# Find all books of this type
		set books [angband k_info find -tval $spell_book]

		# Check each book
		foreach bookNum $books {

			if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {

				# This is the first powerful book
				if {[incr bookIndex] == 4} {

					# Remember where to insert a divider line
					set highBookIndex [expr {$entryIndex + 1}]
				}
			}
			
			if {[variant KANGBANDTK OANGBANDTK]} {
				# Some spell-casting classes can't learn any spells in
				# certain books. In these cases the book is not added
				# to the menu.
				if {![llength [angband spell find $bookNum]]} continue
			}

			incr entryIndex

			# Get information about this book
			set name [angband k_info info $bookNum name]

			# Strip off the [] from the book name
			set name [string trim $name {[]}]

			# Create a submenu for this book
			MakeOneBookMenu $oop $bookNum

			# Append an entry for this book to the book menu
			lappend entries [list -type cascade -menu MENU_BOOK_$bookNum \
				-label $name -identifier M_BOOK_$bookNum]

			# Keep a list of books with learnable spells
			lappend validBookList $bookNum
		}
	}

	if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {

		# There is at least one powerful book
		if {$highBookIndex != -1} {

			# Separator between normal/powerful books
			set entries [linsert $entries $highBookIndex \
				[list -type separator]]
		}
	}
	
	if {[variant ZANGBANDTK]} {

		# Divider line if character has 2 realms
		if {[llength [angband player spell_book]] == 2} {
			set entries [linsert $entries 4 [list -type separator]]
		}
	}

	NSMenu::MenuInsertEntries $mbarId -end MENU_BOOK $entries

	# Speed MenuFindEntry() in SetupMenus()
	if {!$SetupFaster} {
		NSMenu::SetIdentArray $mbarId
	}

	# Remember the menu widget for each submenu
	foreach bookNum $validBookList {
		set index [lindex [NSMenu::MenuFindEntry $mbarId M_BOOK_$bookNum] 1]
		Info $oop bookToIndex,$bookNum $index
		Info $oop indexToBook,$index $bookNum
	}

	# Keep a list of books with learnable spells
	Info $oop validBookList $validBookList

	return
}

# NSBookMenu::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookMenu::Info {oop info args} {

	global NSBookMenu

	# Verify the object
	NSObject::CheckObject NSBookMenu $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSBookMenu($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSBookMenu($oop,$info)
			}
		}
	}

	return
}

# NSBookMenu::MakeOneBookMenu --
#
#	Get a list of spells from the spell/prayer book given by
#	index and append them to the proper (sub)menu.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookMenu::MakeOneBookMenu {oop k_idx} {

	variable SetupFaster

	set mbarId [Info $oop mbar]

	set ident MENU_BOOK_$k_idx
	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier $ident]
	NSMenu::Info $menuId setupCmd "NSBookMenu::SetupCmd $oop"

	# Context-sensitive help
	NSMenu::Info $menuId menuSelectCmd \
		"NSBookMenu::MenuSelect $oop $k_idx"

	set i 0

	# Check each spell in the book
	foreach spell [angband spell find $k_idx] {

		# Get information about the spell
		angband spell info $k_idx $spell attrib
		
		# Append a menu command to cast the spell
		lappend entries [list -type command \
			-label "$attrib(char) $attrib(name)" \
			-command "NSBookMenu::Invoke $oop $k_idx $attrib(char)" \
			-underline 0 -identifier E_SPELL_${k_idx}_$spell]

		if {$SetupFaster} {
			# Remember the entry for this spell
			Info $oop spellToIndex,$k_idx,$spell $i
		}

		# Next entry
		incr i
	}

	Info $oop bookSpells,$k_idx [angband spell find $k_idx]
	Info $oop bookToMenu,$k_idx [NSMenu::Info $menuId menu]

	NSMenu::MenuInsertEntries $mbarId -end $ident $entries

	return
}

# NSBookMenu::SetupCmd --
#
#	Called when a menu is about to be posted. We use this to change the
#	setupMode of a menu so we don't need to pass a huge list of identifiers
#	to the MenuEnable() command.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookMenu::SetupCmd {oop menuId} {

	variable SetupFaster

	set defaultMode disabled
	if {$SetupFaster} {
		set defaultMode ""
	}
	set flags [angband inkey_flags]
	if {$flags eq "INKEY_CMD"} {
		set setupMode $defaultMode
	} elseif {$flags eq "INKEY_SPELL"} {
		set setupMode $defaultMode
	} else {
		# Just disable all the entries in the Book Menu, but
		# ignore the submenus.
		if {$menuId == [Info $oop menuId,book]} {
			set setupMode disabled
		} else {
			set setupMode ""
		}
	}
	NSMenu::Info $menuId setupMode $setupMode

	return
}

# NSBookMenu::SetupMenus --
#
#	Enable entries representing castable spells.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookMenu::SetupMenus {oop} {

	variable Priv
	variable SetupFaster

	set mbarId [Info $oop mbar]
	set validBookList [Info $oop validBookList]

	set flags [angband inkey_flags]

	# Build up the command for determining valid spells
	set switch ""
	
	# If waiting for a command, enable all the allowable books
	# and spells.
	if {$flags eq "INKEY_CMD"} {
		set books {}
		foreach spell_book [angband player spell_book] {
			foreach book [angband inventory find -tval $spell_book] {
				angband inventory info $book attrib
				if {[lsearch -exact $validBookList $attrib(k_idx)] == -1} {
					continue
				}
				if {[lsearch -exact $books $attrib(k_idx)] == -1} {
					lappend books $attrib(k_idx)
				}
			}
		}

		# We only want spells which can be cast
		append switch " -known yes -verify_mana yes"

	# If waiting for a spell, enable only the book the spell is in.
	} elseif {$flags eq "INKEY_SPELL"} {
		set books [angband inkey_other]

		# We only want spells which may be cast or studied
		append switch " -tester yes"

	# If we are not waiting for a command, and we aren't waiting for
	# a spell, then do nothing
	} else {

		# Done
		return
	}

	# Get player mana
	set pymana [lindex [angband player mana] 0]

	# Hack -- Bypass NSMenu::MenuEnable
	# This appears to be only marginally faster after other improvements.
	if {$SetupFaster} {
		
		set menuBook [Info $oop bookToMenu,book]
	
		foreach bookNum $validBookList {
			set state($bookNum) disabled
		}

		# Check each carried book
		foreach bookNum $books {
		
			set menu [Info $oop bookToMenu,$bookNum]
	
			# All spells are disabled by default
			foreach spell [Info $oop bookSpells,$bookNum] {
				set state($bookNum,$spell) disabled
			}

			# Check each legal spell
			foreach spell [angband spell find $bookNum {*}$switch] {
				set state($bookNum,$spell) normal
				set state($bookNum) normal
			}

			# Enable or disable each spell
			foreach spell [Info $oop bookSpells,$bookNum] {
				set index [Info $oop spellToIndex,$bookNum,$spell]
				set state2 $state($bookNum,$spell)
				if {$Priv(hasStateCmd)} {
					menuentrystate $menu $index $state2
				} else {
					if {[$menu entrycget $index -state] != $state2} {
						$menu entryconfigure $index -state $state2
					}
				}
			}
		}

		# Enable or disable each book
		foreach bookNum $validBookList {
			set index [Info $oop bookToIndex,$bookNum]
			set state2 $state($bookNum)
			if {$Priv(hasStateCmd)} {
				menuentrystate $menuBook $index $state2
			} else {
				if {[$menuBook entrycget $index -state] != $state2} {
					$menuBook entryconfigure $index -state $state2
				}
			}
		}

	# not SetupFaster
	} else {
	
		set identList {}
	
		foreach bookNum $books {
			set okay 0
			foreach spell [angband spell find $bookNum {*}$switch] {
				lappend identList E_SPELL_${bookNum}_$spell
				set okay 1
			}
			if {$okay} {
				lappend identList M_BOOK_$bookNum
			}
		}
	
		NSMenu::MenuEnable $mbarId $identList

	# not SetupFaster
	}

	return
}

# NSBookMenu::MenuSelect --
#
#	Displays spell memory when an entry is highlighted.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookMenu::MenuSelect {oop bookNum menuId index ident} {

	if {$index eq "none"} return

	if {!$bookNum} {
		NSRecall::RecallObjectKind [Info $oop indexToBook,$index]
	} else {
		set spellIndex [lindex [angband spell find $bookNum] $index]
		NSRecall::RecallSpell $bookNum $spellIndex
	}

	return
}

# NSBookMenu::Invoke --
#
#	Find the book in inventory and "angband keypress" to cast the spell.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookMenu::Invoke {oop bookNum charSpell} {

	# Find the first book of this type being carried
	set match [angband inventory find -k_idx $bookNum -limit 1]

	# This shouldn't be called if there are no books!
	if {![llength $match]} return

	# Waiting for a spell
	if {[angband inkey_flags] eq "INKEY_SPELL"} {
		angband keypress $charSpell
		return
	}

	# Get info about the first book
	set index [lindex $match 0]
	angband inventory info $index attrib

	# Cast the spell
	GetMagicInfo charCmd spellType
	set charBook $attrib(char)
	DoUnderlyingCommand $charCmd$charBook$charSpell

	return
}

# NSBookMenu::PopupSelect --
#
#	Show a pop-up menu of spell choices.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookMenu::PopupSelect {menu x y} {

	global PopupResult

	set bookNum [angband inkey_other]

	set PopupResult 0

	$menu delete 0 end
	set num 0

	# Check each legal spell
	foreach spell [angband spell find $bookNum -tester yes] {

		# Get information about the spell
		angband spell info $bookNum $spell attrib
		
		# Get the spell char
		set char $attrib(char)

		# Append menu entry
		$menu add command -label "$char $attrib(name)" \
			-command "angband keypress $char ; set PopupResult 1" -underline 0

		# Count the number of spells added to the menu
		incr num
lappend match $spell
	}
set ::Popup(book,$menu) $bookNum
set ::Popup(match,$menu) $match

	if {$num} {
		$menu add separator
	}
	$menu add command -label [mc Cancel]

	# Pressing and holding Button-3, popping up, then letting go selects 
	# an item, so wait a bit if it was a quick press-release
	after 100

	set script [bind $menu <<MenuSelect>>]
	bind $menu <<MenuSelect>> "NSRecall::MenuSelect $menu hook_spell"

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

# GetMagicInfo --
#
#	Return the underlying command char and magic type for the character.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc GetMagicInfo {_charCmd _spellType} {

	upvar $_charCmd charCmd
	upvar $_spellType spellType

	switch -- [angband player prayer_or_spell] {
		prayer {
			set charCmd p
			if {[variant ZANGBANDTK]} {
				set charCmd m
			}
			set spellType prayer
		}
		spell {
			set charCmd m
			set spellType spell
		}
	}

	return
}

