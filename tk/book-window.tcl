# File: book-window.tcl

# Purpose: the (new) Book Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSBookWindow {

	variable Priv

# namespace eval NSBookWindow
}

# NSBookWindow::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::InitModule {} {

	MsgCatInit book

	NSModule::LoadIfNeeded NSList
	NSModule::LoadIfNeeded NSToolbar

	InitImageIfNeeded Image_ButtonOptions button-options.gif
	InitImageIfNeeded Image_ButtonHelp button-help.gif

	NSObject::New NSBookWindow

	return
}

# NSBookWindow::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::CloseModule {} {

	catch {
		destroy [Window book]
	}

	return
}

# NSBookWindow::NSBookWindow --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::NSBookWindow {oop} {

	Info $oop book,current -1
	Info $oop book,match {}
	Info $oop book,request 0
	Info $oop spell,current -1
	Info $oop spell,match {}

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow book $win \
		"GetDefaultGeometry $win reqwidth reqheight" "" \
		"NSBookWindow::DisplayCmd $oop"

	# Update ourself when the font,magic value changes
	qebind NSBookWindow <Value-font,magic> \
		"NSBookWindow::ValueChanged_font_magic $oop"

	# Update ourself when the icon configuration changes
	qebind NSBookWindow <IconCfg> \
		"NSBookWindow::IconCfg $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSBookWindow $oop $win

	#
	# Global list of application windows
	#

	Global book,oop $oop
	Window book $win

	return
}

# NSBookWindow::~NSBookWindow --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::~NSBookWindow {oop} {

	return
}

# NSBookWindow::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::Info {oop info args} {

	global NSBookWindow

	# Verify the object
	NSObject::CheckObject NSBookWindow $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSBookWindow($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSBookWindow($oop,$info)
			}
		}
	}

	return
}

# NSBookWindow::InitWindow --
#
#	Create the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::InitWindow {oop} {

	set win .book$oop
	toplevel $win
#	wm title $win Book

#	wm resizable $win no no

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Do this *after* [wm withdraw] or it pops onscreen
	NSMainWindow::TransientToMain $win

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSBookWindow::Close $oop"

	Info $oop win $win

	#
	# Toolbar
	#

	set toolId [NSObject::New NSToolbar 20 $win]
	NSToolbar::AddTool $toolId -image Image_ButtonOptions -showlabel no \
		-command "DoCommandIfAllowed ="
	NSToolbar::AddTool $toolId -image Image_ButtonHelp -showlabel no \
		-command "DoCommandIfAllowed ?"

	#
	# List of books on the left
	#

	set iconWidth [expr {[icon width] + 8}]
	set iconHeight [expr {[icon height] + 8}]

	set frameBook $win.frameBook
	set treeB [NSList::New $frameBook -icon yes -text no]
	$treeB configure -width $iconWidth -height [expr {$iconHeight * 7}]

	# When a book is selected, display the spells
	NSList::OnSelection $treeB \
		"NSBookWindow::SelectionChanged_Book $oop %T %c %S %D"

	Info $oop book,tree $treeB

	#
	# List of spells on the right
	#

	set frameSpell $win.frameSpell
	set treeS [NSList::New $frameSpell -columns 5 -font magic]
	for {set C 1} {$C < 5} {incr C} {
		$treeS style layout s$C eTxt -padx 6
	}
	$treeS column configure 0 -text [mc Name] -tag name
	$treeS column configure 1 -text [mc Level] -justify right -tag level
	$treeS column configure 2 -text [mc Mana] -justify right -tag mana
	$treeS column configure 3 -text [mc Fail] -justify right -tag fail
	$treeS column configure 4 -text [mc Comment] -tag comment

	$treeS configure -showheader yes -width 400
	scan [$treeS column bbox 0] "%d %d %d %d" left top right bottom
	$treeS configure -height [expr {([font metrics [Value font,magic] -linespace] + 8) * 10 + ($bottom - $top)}]

	# Add text element for char) in Name column
	$treeS element create eTxt1 text -fill White
	$treeS style elements s0 {eSel.e eTxt1 eTxt}
	$treeS style layout s0 eSel.e -union {eTxt1 eTxt}
	$treeS style layout s0 eTxt1 -padx {6 0}
	$treeS style layout s0 eTxt -padx {0 6}

	# When a spell is selected, display the info
	NSList::OnSelection $treeS \
		"NSBookWindow::SelectionChanged_Spell $oop %T %c %S %D"

	# Double-click to select spell
	NSList::OnInvoke $treeS \
		"NSBookWindow::Invoke_Spell $oop %r"

	Info $oop spell,tree $treeS

	#
	# Geometry
	#

	grid rowconfig $win 0 -weight 0 -minsize 0
	grid rowconfig $win 1 -weight 1 -minsize 0
	grid columnconfig $win 0 -weight 0 -minsize 0
	grid columnconfig $win 1 -weight 1 -minsize 0
 
	pack forget [NSToolbar::Info $toolId frame]
	grid [NSToolbar::Info $toolId frame] \
		-row 0 -column 0 -rowspan 1 -columnspan 2 -sticky ew
	grid $frameBook \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky ns
	grid $frameSpell \
		-row 1 -column 1 -rowspan 1 -columnspan 1 -sticky news

	#
	# Context Menu
	#

	set m $win.context
	menu $m -tearoff 0
	bind $treeS <Button-3> "NSBookWindow::ContextMenu $oop $m %X %Y"

	#
	# Feed Term when keys pressed
	#

	Term_KeyPress_Bind [Info $oop book,tree]
	Term_KeyPress_Bind [Info $oop spell,tree]

	# XXX Hack -- Don't feed Tab to the Term
	bind [Info $oop book,tree] <KeyPress-Tab> {
		focus [tk_focusNext %W]
		break
	}

	# XXX Hack -- Don't feed Tab to the Term
	bind [Info $oop spell,tree] <KeyPress-Tab> {
		focus [tk_focusNext %W]
		break
	}

	# Left/Right arrow selects list
	bind [Info $oop book,tree] <KeyPress-Left> \
		"break"
	bind [Info $oop book,tree] <KeyPress-Right> \
		"NSBookWindow::SwitchList $oop spell ; break"
	bind [Info $oop spell,tree] <KeyPress-Left> \
		"NSBookWindow::SwitchList $oop book ; break"
	bind [Info $oop spell,tree] <KeyPress-Right> \
		"break"

	# Never focus on the window itself
	bind $win <FocusIn> "
		if {\[string equal %W $win]} {
			focus [Info $oop book,tree]
		}
	"

	# "Return" does cast/study
	bind [Info $oop spell,tree] <KeyPress-Return> \
		"NSBookWindow::InvokeSelected $oop ; break"

	return
}

# NSBookWindow::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::DisplayCmd {oop message first {bookNum 0}} {

	switch -- $message {
		preDisplay {
			SetList_Book $oop $bookNum

			# This fixes a bug, but the window should never be
			# displayed if there are no books.
			set treeB [Info $oop book,tree]
			if {[$treeB numitems] > 1} {
				NSList::Activate $treeB "root firstchild"
			}
		}
		postDisplay {
		}
	}

	return
}

# NSBookWindow::Close --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::Close {oop} {

	angband keypress \033

	return
}

# NSBookWindow::SetList_Book --
#
#	Display a list of browesable books.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::SetList_Book {oop requestedBook} {

	variable Priv

	set tree [Info $oop book,tree]

	# Clear the list
	NSList::Clear $tree

	# Check each book type
	foreach book_tval [angband player spell_book] {

		# Remember displayed book indexes
		set books($book_tval) {}

		# Check each book in the inventory
		foreach book [angband inventory find -tval $book_tval] {

			# Get info for this item
			angband inventory info $book attrib

			# Sometimes display a specific book
			if {$requestedBook && ($requestedBook != $attrib(k_idx))} continue

			# Only display one of each book
#			if {[lsearch -exact $books($book_tval) $attrib(k_idx)] != -1} continue

			# Remember displayed book indexes
			lappend books($book_tval) $attrib(k_idx)
		}

		# Check each book on the ground
		foreach book [angband floor find -tval $book_tval] {

			# Get info for this item
			angband floor info $book attrib

			# Sometimes display a specific book
			if {$requestedBook && ($requestedBook != $attrib(k_idx))} continue

			# Only display one of each book
#			if {[lsearch -exact $books($book_tval) $attrib(k_idx)] != -1} continue

			# Remember displayed book indexes
			lappend books($book_tval) $attrib(k_idx)
		}
	}

	# Now we sort book indexes by tval and increasing sval.
	# Make sure only one of each book is added to the list.
	# XXX Hack -- Assume the sval increases with k_idx
	set match {}
	foreach book_tval [angband player spell_book] {
		set match [concat $match [lsort -integer -unique $books($book_tval)]]
	}

	# Check each book
	foreach k_idx $match {

		# Get the icon
		set assign [angband k_info info $k_idx icon]

		# Add this book to the list
		set item [$tree item create]
		NSList::SetIcon $tree $item $assign
		$tree item lastchild root $item
	}

	# Remember displayed book indexes
	Info $oop book,match $match
	Info $oop book,request $requestedBook

	# Hack -- Clear the spell list
	NSList::Clear [Info $oop spell,tree]

	SetWidthOfCommentColumn $oop

	return
}

# NSBookWindow::SetList_Spell --
#
#	Fill the list with spells from the given book.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::SetList_Spell {oop bookNum} {

	set win [Info $oop win]
	set tree [Info $oop spell,tree]

	# Cleare the list
	NSList::Clear $tree

	set unknown 0

	# Keep a list of spell indexes
	set match {}

	# Get a list of spell indexes
	set spells [angband spell find $bookNum -tester yes]

	# Check each spell
	foreach spell $spells {

		# Get information about this spell
		angband spell info $bookNum $spell attrib

		# Colorize the spell name (from OAngband)
		switch -- $attrib(info) {
			unknown {
				set fill gray70

				# The character can learn this spell
				if {[angband player new_spells] && 
					($attrib(level) <= [angband player level])} {
					set fill [Value TERM_L_GREEN]
				}
			}
			untried {
				set fill [Value TERM_L_BLUE]
			}
			default {
				set fill White
			}
		}

		# Append the spell to the list
		set item [NSList::NewItem $tree]
		$tree item element configure $item 0 eTxt1 -text "$attrib(char)\) "
		NSList::SetText $tree $item $attrib(name)
		if {$fill ne "White"} {
			NSList::SetTextFill $tree $item $fill
		}
		NSList::SetTextEx $tree $item 1 $attrib(level)
		NSList::SetTextEx $tree $item 2 $attrib(mana)
		NSList::SetTextEx $tree $item 3 $attrib(chance)
		NSList::SetTextEx $tree $item 4 $attrib(info)

		# Count unknown spells
		if {!$attrib(known)} {
			incr unknown
		}

		# Keep a list of spell index
		lappend match $spell
	}

	# Keep a list of spell index
	Info $oop spell,match $match

	# Get info about this book
	set name [angband k_info info $bookNum name]
	set tval [angband k_info set $bookNum tval]

	# TV_MAGIC_BOOK --> Magic
	regexp {TV_(.*)_BOOK} $tval ignore bookType
	set bookType [string totitle $bookType]

	# Set window title
	set name [string trim $name \[\]]
	wm title $win [format [mc "%s Book - %s"] [mc $bookType] $name]

	return
}

# NSBookWindow::SelectionChanged_Book --
#
#	Called when the book selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::SelectionChanged_Book {oop tree count select deselect} {

	# Nothing was selected
	if {![llength $select]} {
		NSList::Clear [Info $oop spell,tree]
		Info $oop book,current -1
		return
	}

	# Get the (first) selected item
	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]

	set bookNum [lindex [Info $oop book,match] $row]
	Info $oop book,current $bookNum

	NSRecall::RecallObjectKind $bookNum

	SetList_Spell $oop $bookNum

	return
}

# NSBookWindow::SelectionChanged_Spell --
#
#	Called when the spell selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::SelectionChanged_Spell {oop tree count select deselect} {

	# Nothing was selected
	if {![llength $select]} {
		Info $oop spell,current -1
		return
	}

	# Get the (first) selected item
	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]

	# Get the book number
	set bookNum [Info $oop book,current]

	# Get the spell index
	set spellIndex [lindex [Info $oop spell,match] $row]
	Info $oop spell,current $row

	NSRecall::RecallSpell $bookNum $spellIndex

	return
}

# NSBookWindow::InvokeRow --
#
#	Do something when a spell is double-clicked or <Enter>'d.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::InvokeRow {oop row} {

	# No row was hit
	if {$row == -1} return

	# Get the book number
	set bookNum [Info $oop book,current]

	# Get the spell index
	set spellIndex [lindex [Info $oop spell,match] $row]

	set command [GetSpellCommand $oop $bookNum $spellIndex label]
	if {[string length command]} {
		eval $command
	}

	return
}

# NSBookWindow::InvokeSelected --
#
#	Invoke the selected spell.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::InvokeSelected {oop} {

	InvokeRow $oop [Info $oop spell,current]

	return
}

# NSBookWindow::Invoke_Spell --
#
#	Do something when a spell is double-clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::Invoke_Spell {oop row} {

	# Do something
	InvokeRow $oop $row

	return
}

# NSBookWindow::ContextMenu --
#
#	When a spell is right-clicked, pop up a context menu of actions.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::ContextMenu {oop menu x y} {

	# Get the list
	set tree [Info $oop spell,tree]

	set font [$menu cget -font]

	# Find the hit row
	set x1 [expr {$x - [winfo rootx $tree]}]
	set y1 [expr {$y - [winfo rooty $tree]}]
	set row [NSList::Point2Row $tree $x1 $y1]

	# Clear the menu
	$menu delete 0 end

	# No row is hit
	if {$row == -1} {

		$menu add command -label [mc Close] -command {angband keypress \033}
		$menu add separator
		$menu add command -label [mc Cancel]

		# Pop up the menu
		tk_popup $menu $x $y

		# Done
		return
	}

	# Get the book number
	set bookNum [Info $oop book,current]

	# Get the spell index
	set spellIndex [lindex [Info $oop spell,match] $row]

	set command [GetSpellCommand $oop $bookNum $spellIndex label]
	if {[string length $command]} {
		$menu add command -label $label -command $command \
			-font [BoldFont $font]
		$menu add separator
	}

	$menu add command -label [mc Close] -command {angband keypress \033}
	$menu add separator
	$menu add command -label [mc Cancel]

	# Let the user choose a command from the menu
	tk_popup $menu $x $y

	return
}

# NSBookWindow::IconCfg --
#
#	Called when the game's icon configuration changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::IconCfg {oop} {

	set treeB [Info $oop book,tree]
	set treeS [Info $oop spell,tree]

	# Set the width of the book list
	set oldWidth [winfo width $treeB]
	set newWidth [expr {[icon width] + 8}]
	$treeB configure -width $newWidth

	# Hack -- Resize the toplevel so the spell list is not resized
	set diff [expr {$newWidth - $oldWidth}]
	if {$diff} {
		set win [Info $oop win]
		set newWidth [expr {[winfo width $win] + $diff}]
		NSToplevel::SetTotalWidth $win $newWidth
	}

	if {![winfo ismapped [Info $oop win]]} return

	set currentB [$treeB selection get]
	set currentS [$treeS selection get]

	SetList_Book $oop [Info $oop book,request]
	if {$currentB ne ""} {
		NSList::Activate $treeB $currentB
	}
	if {$currentS ne ""} {
		NSList::Activate $treeS $currentS
	}

	return
}

# NSBookWindow::ValueChanged_font_magic --
#
#	Called when the font,magic value changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::ValueChanged_font_magic {oop} {

	if {![winfo ismapped [Info $oop win]]} return
	SetWidthOfCommentColumn $oop

	return
}

# NSBookWindow::SetWidthOfCommentColumn --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::SetWidthOfCommentColumn {oop} {

	set tree [Info $oop spell,tree]

	# Set width of "Comment" column so it doesn't change from book to book
	set maxWidth 0
	foreach bookNum [Info $oop book,match] {
		foreach spell [angband spell find $bookNum] {
			angband spell info $bookNum $spell attrib
			set width [font measure [Value font,magic] $attrib(info)]
			if {$width > $maxWidth} {
				set maxWidth $width
			}
		}
	}

	set padX [$tree style layout s4 eTxt -padx]
	if {[llength $padX] == 2} {
		set padX [expr {[lindex $padX 0] + [lindex $padX 1]}]
	}
	if {$maxWidth + $padX * 2 > [$tree column neededwidth comment]} {
		$tree column configure comment -width [expr {$maxWidth + $padX * 2}]
	}

	return
}

# NSBookWindow::GetSpellCommand --
#
#	Return a command that will allow the user to cast or study the
#	given spell in the given book.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::GetSpellCommand {oop bookNum spellIndex _label} {

	upvar $_label label

	# Get information about the spell
	angband spell info $bookNum $spellIndex attrib

	# Get the spell char
	set charSpell $attrib(char)

	# Get the command char and spell type for the character
	GetMagicInfo charCmd spellType

	# We are waiting for a spell
	if {[angband inkey_flags] eq "INKEY_SPELL"} {

		set label [format [mc "Select This %s"] [string totitle [mc $spellType]]]

		# Append a command to select the item
		return "angband keypress $charSpell"

	# We are waiting for a command
	} else {

		# The spell is known
		if {$attrib(known)} {
			set hook cast
		} else {
			set hook study
		}

		# Find a book in the inventory
		set match [angband inventory find -limit 1 -k_idx $bookNum -hook $hook]

		set charToggle ""
		set doToggle 0

		# The book is in the inventory
		if {[llength $match]} {

			# Get information about this book
			angband inventory info [lindex $match 0] iattrib

			# Get the inventory char
			set charBook $iattrib(char)

		# The book isn't in the inventory, so it must be on the floor.
		} else {

			if {[variant ANGBANDTK KANGBANDTK ZANGBANDTK]} {
				set easy_floor [Setting easy_floor]
			}
			if {[variant OANGBANDTK]} {
				set easy_floor 1
			}

			# We need to figure out if the '-' char needs to be used.
			if {[llength [angband inventory find -hook $hook]] ||
				!$easy_floor} {
				set charToggle -
				set doToggle 1
			}

			# Get a list of any books we can cast/study from
			set match [angband floor find -hook $hook]

			if {[variant ANGBANDTK KANGBANDTK OANGBANDTK]} {
				set query_flag [Setting floor_query_flag]
			}
			if {[variant ZANGBANDTK]} {
				set query_flag [Setting other_query_flag]
			}

			# With easy_floor
			if {$easy_floor} {

				# Multiple items, with easy_floor
				if {[llength $match] > 1} {

					# Find the char of the current book
					set index 0
					foreach o_idx $match {
						set k_idx [angband o_list set $o_idx k_idx]
						if {$k_idx == $bookNum} break
						incr index
					}
					set charBook [string index "abcdefghijklmnopqrstuvw" $index]

				# One item, with easy_floor
				} else {

					if {$doToggle && !$query_flag} {
						set charBook ""
					} else {
						set charBook a
					}
				}

			# Without easy_floor
			} else {

				# Multiple items, without easy_floor
				if {[llength $match] > 1} {

					# Multiple items, without easy_floor, with floor_query_flag
					if {$query_flag} {

						foreach o_idx $match {
							set k_idx [angband o_list set $o_idx k_idx]
							if {$k_idx == $bookNum} break
							append charToggle n
						}
						append charToggle y
						set charBook ""

					# Multiple items, without easy_floor, without floor_query_flag
					} else {

						set o_idx [lindex $match 0]
						set k_idx [angband o_list set $o_idx k_idx]

						# If the top item isn't the book, we can't select it!
						if {$k_idx != $bookNum} return ""

						set charBook ""
					}

				# One item, without easy_floor
				} else {

					# One item, without easy_floor, with floor_query_flag
					if {$query_flag} {

						append charToggle y
						set charBook ""

					# One item, without easy_floor, without floor_query_flag
					} else {
						set charBook ""
					}
				}
			}
		}

		# The spell is known
		if {$attrib(known)} {

			switch -- $spellType {
				prayer {
					set label [mc Recite]
				}
				spell {
					set label [mc Cast]
				}
			}

			# Allow the user to cast the spell
			return "DoUnderlyingCommand $charCmd$charToggle$charBook$charSpell"

		# The spell is not known
		} else {

			# The character can learn this spell
			if {[angband player new_spells] && ($attrib(level) <=
				[angband player level])} {

				# Build the command to study this spell or prayer.
				set command "DoUnderlyingCommand G"

				# Hack -- Bypass the "You can learn N new spells prompt"
				if {[variant ZANGBANDTK]} {
					append command "\\n"
				}

				# Which book
				append command $charToggle$charBook

				# Which spell (prayers are random)
				if {$spellType eq "spell"} {
					append command $charSpell
				}

				set label [mc Study]
				return $command
			}
		}
	}

	set label ""
	return ""
}

# NSBookWindow::SwitchList --
#
#	Focus on the book or spell list, updating the selection if desired.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSBookWindow::SwitchList {oop which} {

	set tree [Info $oop $which,tree]
	set current [Info $oop $which,current]

	if {[$tree numitems] == 1} return

	focus $tree
	if {$current == -1} {
		NSList::Activate $tree "root firstchild"
	}

	return
}

