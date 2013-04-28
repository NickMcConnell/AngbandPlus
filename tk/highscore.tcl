# File: highscore.tcl

# Purpose: the High Scores Window and related commands

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSHighScore {

	# The high score read by highscore_read
	variable highscore

	# The score for the current character
	variable highscore_index -1

	# List of name/value pairs
	variable highscore_list {}

	variable MenuString
	variable Priv

# namespace eval NSHighScore
}

# NSHighScore::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::InitModule {} {

	variable Priv

	MsgCatInit score

	NSModule::LoadIfNeeded NSList

	set Priv(find,string) ""
	set Priv(find,index) -1

	# Create the High Scores Window
	NSObject::New NSHighScore

	if {![NSModule::LoadIfNeeded NSRecord]} {
		#
		# The "wm transient" command always displays the window
		# in the "normal" state. I think this is a bug and the
		# window should be displayed in the same state as the
		# master window (that's what the 8.0.4 comment says).
		#
		# Anyways, when the game is over this causes the
		# Character Record Window to be displayed again,
		# forcing me to hide it. Because this flashes on the
		# screen I move the CRW offscreen, make it transient,
		# and then hide it again.
		#
# Fixed in 8.3b2
if {[info tclversion] >= 8.3} {
#	wm transient [Window record] [Window highscore]
} else {
		set win [Window record]
		set x [winfo x $win]
		set offset [ScreenWidth]
		wm geometry $win +$offset+[winfo y $win]
		update idletasks
		wm transient $win [Window highscore]
		wm withdraw $win
		update idletasks
		wm geometry $win +$x+[winfo y $win]
}
	}

	# Important: Since we always reload this module after a character
	# dies, we reload the record info here so the dead character's
	# record (if any) is properly read again. See "angband_display highscore".
	NSRecord::LoadRecords

	return
}

# NSHighScore::CloseModule --
#
#	One-time-only-ever cleanup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::CloseModule {} {

	catch {
		destroy [Window highscore]
	}

	return
}

# NSHighScore::NSHighScore --
#
#	Object constructor called by NSObject::New().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::NSHighScore {oop} {

	# Assume we will be called from C
	Info $oop interactive 1

	Info $oop selection,item ""
	Info $oop selection,row -1
	Info $oop sortBy pts

	InitWindow $oop

	set win [Info $oop win]

	NSWindowManager::RegisterWindow highscore $win \
		"GetDefaultGeometry $win screen main2" "" \
		"NSHighScore::DisplayCmd $oop"

	# Destroy the object along with the toplevel (later)
	NSUtils::DestroyObjectWithWidget NSHighScore $oop $win

	#
	# Global list of application windows
	#

	Global highscore,oop $oop
	Window highscore $win

	return
}

# NSHighScore::~NSHighScore --
#
#	Object destructor called by NSObject::Delete().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::~NSHighScore {oop} {

	return
}

# NSHighScore::Info --
#
#	Query and modify info.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::Info {oop info args} {

	global NSHighScore

	# Verify the object
	NSObject::CheckObject NSHighScore $oop

	# Set info
	if {[llength $args]} {
		switch -- $info {
			default {
				set NSHighScore($oop,$info) [lindex $args 0]
			}
		}

	# Get info
	} else {
		switch -- $info {
			default {
				return $NSHighScore($oop,$info)
			}
		}
	}

	return
}

# NSHighScore::InitWindow --
#
#	Create the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::InitWindow {oop} {

	global Angband

	set win .highscore$oop
	toplevel $win
	wm title $win [format [mc "%s Hall of Fame"] $Angband(name)]

	# Start out withdrawn (hidden)
	wm withdraw $win

	# Hack -- see "angband_display highscore"
	if {![angband player is_dead]} {
		# Do this *after* [wm withdraw] or it pops onscreen
		NSMainWindow::TransientToMain $win
	}

	# Do stuff when window closes
	wm protocol $win WM_DELETE_WINDOW "NSHighScore::Close $oop"

	Info $oop win $win

	InitMenus $oop

	# Divider
	MakeDivider $win.divider1 x

	#
	# High Score List
	#

	set font [Value font,fixed]
	set cw [font measure $font "W"]
	set fh [font metrics $font -linespace]
	set rowHeight [expr {$fh * 3 + 8}]
	set height [expr {$rowHeight * 5}]
	set width [expr {$cw * 80}]

	set frame $win.frameList
	NSList::New $frame -columns 12 -xscrollbar 1
	set tree $frame.tree
	$tree configure -width $width -height $height -showheader yes

	# Vertical divider between columns
	$tree element create eDiv rect -outline gray20 -outlinewidth 1 -open nes

	set C 0
	foreach tag {name race class clevel killedby dlevel score turns gold winner record day} {
		$tree column configure $C -tag $tag -text [mc $tag]
		incr C
	}
	foreach tag {clevel dlevel score turns gold} {
		$tree column configure $tag -justify right
	}
	# checkbutton for winner/record
	foreach tag {winner record} {
		NSList::ConfigColumn $tree $tag -text no -icon no -checkbutton yes
		$tree column configure $tag -justify center
	}
	$tree element configure eTxt -lines 1
	for {set C 0} {$C < [$tree numcolumns]} {incr C} {
		$tree column configure $C -textpadx 4 -button yes
		if {[$tree column cget $C -justify] eq "left"} {
			if {[lsearch [$tree style elements s$C] eTxt] != -1} {
				$tree style layout s$C eTxt -padx {4 4} -squeeze x
			}
		} else {
			if {[lsearch [$tree style elements s$C] eTxt] != -1} {
				$tree style layout s$C eTxt -padx {4 4} -squeeze x
			}
			$tree column configure $C -arrowside left
		}

		# Vertical divider between columns (except column 0)
		if {!$C} continue
		$tree style elements s$C [concat eDiv [$tree style elements s$C]]
		$tree style layout s$C eDiv -iexpand y -expand e -detach yes
	}
	$tree column configure score -arrow down

	NSList::OnSelection $tree \
		"NSHighScore::SelectionChanged $oop %T %c %S %D"

	NSList::OnInvoke $tree \
		"NSHighScore::Invoke $oop %r"

	foreach tag {name race class killedby day} {
		Info $oop sort,order,$tag -increasing
		Info $oop sort,opts,$tag -dictionary
	}
	foreach tag {score turns gold} {
		Info $oop sort,order,$tag -decreasing
		Info $oop sort,opts,$tag -integer
	}

	Info $oop sort,order,clevel -decreasing
	Info $oop sort,opts,clevel [list -command "NSHighScore::SortLevel $oop"]

	Info $oop sort,order,dlevel -decreasing
	Info $oop sort,opts,dlevel [list -command "NSHighScore::SortDepth $oop"]

	Info $oop sort,order,record -decreasing
	Info $oop sort,opts,record [list -command "NSHighScore::SortCheck $oop record"]

	Info $oop sort,order,winner -decreasing
	Info $oop sort,opts,winner [list -command "NSHighScore::SortCheck $oop winner"]

	Info $oop sort,column score
	Info $oop sort,order {score clevel dlevel turns}
	$tree notify install <Header-invoke>
	$tree notify bind $tree <Header-invoke> \
		"NSHighScore::Sort $oop %C 1"

	Info $oop tree $tree

	#
	# Statusbar
	#

	MakeStatusBar $win.statusBar 20

	#
	# Geometry
	#

	grid rowconfigure $win 0 -weight 0
	grid rowconfigure $win 1 -weight 1
	grid rowconfigure $win 2 -weight 0
	grid columnconfig $win 0 -weight 1

	if {[Platform windows]} {
		grid $win.divider1 \
			-row 0 -column 0 -rowspan 1 -columnspan 1 -sticky ew -pady 2
	}
	grid $win.frameList \
		-row 1 -column 0 -rowspan 1 -columnspan 1 -sticky news
	grid $win.statusBar \
		-row 2 -column 0 -rowspan 1 -columnspan 1 -sticky ew

	#
	# KAngbandTk and ZAngbandTk may show the Highscores Window
	# in the buildings, so we need to feed keys to the Term.
	#
	bind $win <KeyPress> {angband keypress %A}

	bind $win <KeyPress-Escape> "NSHighScore::Close $oop"
	bind $win <Control-KeyPress-w> "NSHighScore::Close $oop"
	bind $win <KeyPress-Delete> "NSHighScore::Delete $oop"

	set mbarId [Info $oop mbarId]
	bind $win <KeyPress-Return> "NSHighScore::MenuInvoke $oop $mbarId E_RECORD"
	bind $win <KeyPress-r> "NSHighScore::MenuInvoke $oop $mbarId E_RECORD"
	bind $win <KeyPress-f> "NSHighScore::MenuInvoke $oop $mbarId E_FIND"
	bind $win <KeyPress-g> "NSHighScore::MenuInvoke $oop $mbarId E_FIND_AGAIN"

	return
}

# NSHighScore::InitMenus --
#
#	Create the menus associated with the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::InitMenus {oop} {

	variable MenuString

	set win [Info $oop win]
	set mod "Ctrl"

	#
	# Menu bar
	#

	set mbarId [NSObject::New NSMenu $win -tearoff 0 \
		-postcommand "NSHighScore::SetupMenus $oop" -identifier MENUBAR]
	Info $oop mbarId $mbarId

	# Context-sensitive help
	NSMenu::Info $mbarId menuSelectCmd "NSHighScore::MenuSelect $oop"

	# Call our command when an entry is invoked
	NSMenu::Info $mbarId invokeCmd "NSHighScore::MenuInvoke $oop"

	#
	# Score Menu
	#

	NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_SCORE
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_SCORE -label [mc Score] -underline 0 \
		-identifier M_SCORE

	set entries {}
	lappend entries [list -type command -label [mc "Dump Scores"] \
		-underline 0 -identifier E_DUMP]
	lappend entries [list -type command -label [mc "Show Record"] \
		-underline 5 -accelerator r -identifier E_RECORD]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Find...] \
		-accelerator f -underline 0 -identifier E_FIND]
	lappend entries [list -type command -label [mc "Find Again"] \
		-accelerator g -underline 6 -identifier E_FIND_AGAIN]
	lappend entries [list -type separator]
	lappend entries [list -type command -label [mc Close] \
		-underline 0 -accelerator $mod+W -identifier E_CLOSE]

	NSMenu::MenuInsertEntries $mbarId -end MENU_SCORE $entries

	#
	# Columns Menu
	#

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_COLUMNS]
	NSMenu::Info $menuId setupMode normal

	set entries {}
	foreach tag {name race class clevel killedby dlevel score turns gold winner record day} {
		# FIXME: save/restore these settings
		Info $oop visible,$tag 1
		lappend entries [list -type checkbutton -label [mc $tag] \
			-variable NSHighScore($oop,visible,$tag) \
			-identifier E_COLUMN_$tag]
	}
	NSMenu::MenuInsertEntries $mbarId -end MENU_COLUMNS $entries

	#
	# Sort Menu
	#

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_SORT]
#	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
#		-menu MENU_SORT -label [mc Sort] -underline 1 \
#		-identifier M_SORT
	NSMenu::Info $menuId setupMode normal

	set entries {}
	# FIXME: save/restore this setting
	foreach tag {name race class clevel killedby dlevel score turns gold winner record day} {
		lappend entries [list -type radiobutton -label [mc $tag] \
			-variable NSHighScore($oop,sort,column) -value $tag \
			-identifier E_SORT_$tag]
	}
	NSMenu::MenuInsertEntries $mbarId -end MENU_SORT $entries

	#
	# View Menu
	#

	set menuId [NSObject::New NSMenu $mbarId -tearoff 0 -identifier MENU_VIEW]
	NSMenu::MenuInsertEntry $mbarId -end MENUBAR -type cascade \
		-menu MENU_VIEW -label [mc View] -underline 1 \
		-identifier M_VIEW
	NSMenu::Info $menuId setupMode normal

	set entries {}
	lappend entries [list -type cascade -menu MENU_COLUMNS -label [mc Columns] -identifier M_COLUMNS]
	lappend entries [list -type cascade -menu MENU_SORT -label [mc Sort] -identifier M_SORT]
	NSMenu::MenuInsertEntries $mbarId -end MENU_VIEW $entries

	set MenuString(M_SCORE) \
		"Contains commands for displaying and searching scores."
	set MenuString(E_DUMP) \
		"Writes the scores to a text file."
	set MenuString(E_RECORD) \
		"Displays the record for the selected score."
	set MenuString(E_FIND) \
		"Searches for a score containing a given string."
	set MenuString(E_FIND_AGAIN) \
		"Repeats the previous search."
	set MenuString(E_CLOSE) \
		"Closes the window."

	set MenuString(M_SORT) \
		"Contains commands for sorting scores."
	set MenuString(E_SORT_name) \
		"Sorts the scores by character name."
	set MenuString(E_SORT_race) \
		"Sorts the scores by character race."
	set MenuString(E_SORT_class) \
		"Sorts the scores by character class."
	set MenuString(E_SORT_clevel) \
		"Sorts the scores by experience level."
	set MenuString(E_SORT_killedby) \
		"Sorts the scores by cause of death."
	set MenuString(E_SORT_dlevel) \
		"Sorts the scores by dungeon level."
	set MenuString(E_SORT_score) \
		"Sorts the scores by total points."
	set MenuString(E_SORT_turns) \
		"Sorts the scores by game turns."
	set MenuString(E_SORT_gold) \
		"Sorts the scores by gold."
	set MenuString(E_SORT_winner) \
		"Sorts the scores by winner status."
	set MenuString(E_SORT_record) \
		"Sorts the scores by presence of record file."
	set MenuString(E_SORT_day) \
		"Sorts the scores by date."

	return
}

# NSHighScore::SetupMenus --
#
#	Prepare to post the menus.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::SetupMenus {oop mbarId} {

	variable highscore_list

	lappend identList E_DUMP E_FIND E_FIND_AGAIN E_CLOSE

	set item [Info $oop selection,item]
	if {$item ne ""} {
		set index [Info $oop match,$item]
		array set highscore [lindex $highscore_list $index]
		if {$highscore(_record) ne ""} {
			lappend identList E_RECORD
		}
	}

	NSMenu::MenuEnable $mbarId $identList

	return
}

# NSHighScore::MenuSelect --
#
#	Displays a help string associated with a menu entry.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::MenuSelect {oop menuId index ident} {

	variable MenuString

	switch -- $ident {
		{} {
			set desc {}
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

# NSHighScore::MenuInvoke --
#
#	Called when a menu entry is invoked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::MenuInvoke {oop menuId ident} {

	set win [Info $oop win]

	switch -glob -- $ident {
		E_DUMP {Dump $oop $win}
		E_RECORD {InvokeRow $oop [Info $oop selection,row]}
		E_FIND {Find $oop 0}
		E_FIND_AGAIN {Find $oop 1}
		E_CLOSE {Close $oop}
		E_COLUMN_* {
			scan $ident E_COLUMN_%s tag
			NSList::ColumnShow [Info $oop tree] $tag [Info $oop visible,$tag]
		}
		E_SORT_* {
			scan $ident E_SORT_%s tag
			Sort $oop $tag 0
		}
	}

	return
}

# NSHighScore::DisplayCmd --
#
#	Called by NSWindowManager::Display().
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::DisplayCmd {oop message first} {

	global Angband

	switch -- $message {
		preDisplay {
			if {![Info $oop interactive]} {
				wm title [Info $oop win] [format [mc "%s Hall of Fame"] $Angband(name)]
			}
			SetList $oop
			focus [Info $oop tree]
		}
		postDisplay {
		}
		reDisplay {
			SetList $oop
		}
	}

	return
}

# NSHighScore::Close --
#
#	Do something when closing the window.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::Close {oop} {

	# If the character is dead, then we call "tkwait window"
	if {[angband player is_dead]} {

		# XXX Hack -- The Character Record Window
		# is centered after character death. Don't stomp the
		# geometry requested by the NSWindowManager code.
		set NSWindowManager::Priv(record,setup) 0

		destroy [Info $oop win]
		return
	}

	if {![Info $oop interactive]} {
		NSWindowManager::Undisplay highscore
		Info $oop interactive 1
		return
	}

	angband keypress \033

	return
}

# NSHighScore::StatusBar --
#
#	Display text in the status bar, perhaps clearing it later.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::StatusBar {oop text zap} {

	set win [Info $oop win]
	set label [$win.statusBar itemcget t1 -label]
	$label configure -text $text
	if {$zap} {
		NSUtils::ZapLabel $label
	}

	return
}

# NSHighScore::SetList --
#
#	Fill the list with scores.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::SetList {oop} {

	variable highscore
	variable highscore_index
	variable highscore_list

	set win [Info $oop win]
	set tree [Info $oop tree]

	# Clear the list
	NSList::Clear $tree

	# List of scores
	GetTheScores $oop

	# Remember score-file index for each list item
	array unset ::NSHighScore $oop,match,*

	set playerItem ""
	set index 0
	foreach score $highscore_list {

		array set highscore $score

		# Append the high score, getting info from highscore[] variable
		set item [$tree item create]
		NSList::SetTextEx $tree $item name $highscore(who)
		if {$highscore_index == $index} {
			NSList::SetTextFillEx $tree $item name [Value TERM_L_BLUE]
			set playerItem $item
		}
		NSList::SetTextEx $tree $item race $highscore(race)
		NSList::SetTextEx $tree $item class $highscore(class)
		set clevel $highscore(cur_lev)
		if {$highscore(max_lev) != $highscore(cur_lev)} {
			set clevel $highscore(cur_lev)/$highscore(max_lev)
		}
		NSList::SetTextEx $tree $item clevel $clevel
		NSList::SetTextEx $tree $item killedby $highscore(how)
		if {[variant ANGBANDTK OANGBANDTK ZANGBANDTK]} {
			# FIXME: Arena in ZAngband
			if {$highscore(cur_dun)} {
				set dcur $highscore(cur_dun)
			} else {
				# FIXME: town name in ZAngband
				set dcur [mc Town]
			}
		# ANGBANDTK OANGBANDTK ZANGBANDTK
		}
		if {[variant KANGBANDTK]} {
			# Append "killed by xxx"
			if {$highscore(inside_arena)} {
				set dcur [mc Arena]
			} else {
				if {$highscore(cur_dun)} {
					set dcur $highscore(cur_dun)
				} else {
					set dcur [mc Town]
				}
			}

			# FIXME: add inside_quest to high_score
			if 0 {
				append text [format "\n               Killed by %s while questing" \
					$highscore(how)]
			}
		# KANGBANDTK
		}
		if {[string is integer $dcur]} {
			if {[Setting depth_in_feet]} {
				set dcur "[expr {$dcur * 50}]'"
			}
		}
		set dmax $highscore(max_dun)
		if {[Setting depth_in_feet]} {
			set dmax "[expr {$dmax * 50}]'"
		}
		set depth $dcur
		if {$highscore(cur_dun) ne $highscore(max_dun)} {
			set depth $dcur/$dmax
		}
		NSList::SetTextEx $tree $item dlevel $depth
		NSList::SetTextEx $tree $item score $highscore(pts)
		NSList::SetTextEx $tree $item turns $highscore(turns)
		NSList::SetTextEx $tree $item gold $highscore(gold)
		if {$highscore(_winner)} {
			$tree item state forcolumn $item winner checked
		}
		if {$highscore(_record) ne ""} {
			$tree item state forcolumn $item record checked
		}
		set when $highscore(day)
		if {[string index $when 0] eq "@"} {
			set YYYY [string range $when 1 4]
			set MM [string range $when 5 6]
			set DD [string range $when 7 8]
			set when $DD/$MM/$YYYY
		} elseif {[string match {*/*/*} $when]} {
			set MM [string range $when 0 1]
			set DD [string range $when 3 4]
			set YY [string range $when 6 7]
			if {$YY < 72} {
				set YYYY 20$YY
			} else {
				set YYYY 19$YY
			}
			set when $DD/$MM/$YYYY
		}
		NSList::SetTextEx $tree $item day $when
		$tree item lastchild root $item

		# Remember index into highscore_list for each list item
		Info $oop match,$item $index
		incr index
	}

	Sort $oop [Info $oop sort,column] 0

	# If the current character has a score, highlight and view it
	if {$playerItem ne ""} {
		NSList::Activate $tree $playerItem
	}

	return
}

# NSHighScore::SelectionChanged --
#
#	Called when the list selection changes.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::SelectionChanged {oop tree count select deselect} {

	variable highscore_list

	set win [Info $oop win]

	if {![llength $select]} {

		$win.statusBar itemconfigure t2 -text ""

		# No score is selected now
		Info $oop selection,row -1
		Info $oop selection,item ""
		return
	}

	set item [lindex $select 0]
	set row [NSList::Item2Row $tree $item]
	Info $oop selection,row $row
	Info $oop selection,item $item

	set index [Info $oop match,$item]
	array set highscore [lindex $highscore_list $index]
	if {$highscore(_record) ne ""} {
		set string [format [mc "Record %s."] $highscore(_record)]
	} else {
		set string [mc "No record available."]
	}

	# Indicate the score index
	if {$::DEBUG} {
		set index [Info $oop match,$item]
		array set highscore [lindex $highscore_list $index]
		set string "#$highscore(_index) $string"
	}

	$win.statusBar itemconfigure t2 -text $string

	return
}

# NSHighScore::GetTheScores --
#
#	Return a list of high scores. Read the high score file if present.
#	Also create an entry for the character, if not dead yet. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSHighScore::GetTheScores {oop} {

	variable highscore
	variable highscore_index
	variable highscore_list

	set highscore_index -1
	set highscore_list {}

	# Predict the character's score
	set predict [angband highscore predict -force highscore_predict]
	set highscore_predict(_index) -1
	if {[string equal $highscore_predict(how) "Ripe Old Age"]} {
		set highscore_predict(_winner) 1
	} else {
		set highscore_predict(_winner) 0
	}

	# In AngbandTk, if we are showing all the scores, then always predict the
	# character's score.

	# In KAngbandTk, if the scores are shown from the menu command,
	# not in a building, then predict the character's score.
	if {![Info $oop interactive]} {
		set predict 1
	}

	# Build the filename
	set buf [Path lib apex scores.raw]

	# See if the file exists
	if {[file exists $buf]} {

		# Read the high scores
		foreach score [angband highscore find -tester yes] {

			angband highscore info $score highscore
			set highscore(_index) $score
			if {[string equal $highscore(how) "Ripe Old Age"]} {
				set highscore(_winner) 1
			} else {
				set highscore(_winner) 0
			}

			lappend highscore_list [array get highscore]
		}
	}

	# Living character may get an entry
	if {$predict && ![angband player is_dead]} {
		lappend highscore_list [array get highscore_predict]
	}

	# Look for the current character's score (living or dead)
	set index 0
	foreach score $highscore_list {
		array set highscore $score
		if {[angband player is_dead]} {
			if {$highscore(who) eq $highscore_predict(who) &&
				($highscore(turns) == $highscore_predict(turns)) &&
				($highscore(cur_dun) == $highscore_predict(cur_dun)) &&
				($highscore(max_dun) == $highscore_predict(max_dun)) &&
				($highscore(cur_lev) == $highscore_predict(cur_lev)) &&
				($highscore(max_lev) == $highscore_predict(max_lev)) &&
				($highscore(p_c) == $highscore_predict(p_c)) &&
				($highscore(p_r) == $highscore_predict(p_r)) &&
				($highscore(pts) == $highscore_predict(pts))} {

				set highscore_index $index
				break
			}
		} else {
			if {$highscore(_index) == -1} {
				set highscore_index $index
				break
			}
		}
		incr index
	}

	# Get a list of character records associated with each score
	set list {}
	foreach record [NSRecord::MatchRecordsToScores $highscore_list] score $highscore_list {
		lappend score _record $record
		lappend list $score
	}
	set highscore_list $list

	return
}

# NSHighScore::FormatScore --
#
#	Return a formatted string for the current high score. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSHighScore::FormatScore {oop index} {

	variable highscore

	append text [format "%3d.%9s  %s the %s %s, Level %d" \
		$index $highscore(pts) $highscore(who) $highscore(race) \
		$highscore(class) $highscore(cur_lev)]

	# Append a "maximum level"
	if {$highscore(max_lev) > $highscore(cur_lev)} {
		append text " (Max $highscore(max_lev))"
	}

	if {[variant ANGBANDTK OANGBANDTK ZANGBANDTK]} {
		if {$highscore(cur_dun)} {
			append text [format "\n               Killed by %s on %s %d" \
				$highscore(how) "Dungeon Level" $highscore(cur_dun)]
		} else {
			append text [format "\n               Killed by %s in the Town" \
				$highscore(how)]
		}
	# ANGBANDTK OANGBANDTK ZANGBANDTK
	}
	if {[variant KANGBANDTK]} {
		# Append "killed by xxx"
		if {$highscore(inside_arena)} {
			append text [format "\n               Killed by %s in the Arena" \
				$highscore(how)]
		} else {
			if {$highscore(cur_dun)} {
				append text [format "\n               Killed by %s on %s %d" \
					$highscore(how) "Dungeon Level" $highscore(cur_dun)]
			} else {
				append text [format "\n               Killed by %s in the Town" \
					$highscore(how)]
			}
		}

		# FIXME: add inside_quest to high_score
		if 0 {
			append text [format "\n               Killed by %s while questing" \
				$highscore(how)]
		}
	# KANGBANDTK
	}

	# Append a "maximum level"
	if {$highscore(max_dun) > $highscore(cur_dun)} {
		append text " (Max $highscore(max_dun))"
	}

	# Hack -- extract the gold and such
	set user [string trimleft $highscore(uid)]

	set when [string trimleft $highscore(day)]
	if {[string index $when 0] eq "@"} {
		set YYYY [string range $when 1 4]
		set MM [string range $when 5 6]
		set DD [string range $when 7 8]
		set when $DD/$MM/$YYYY
	} elseif {[string match {*/*/*} $when]} {
		set MM [string range $when 0 1]
		set DD [string range $when 3 4]
		set YY [string range $when 6 7]
		if {$YY < 72} {
			set YYYY 20$YY
		} else {
			set YYYY 19$YY
		}
		set when $DD/$MM/$YYYY
	}

	set gold [string trimleft $highscore(gold)]
	set turn [string trimleft $highscore(turns)]

	# And still another line of info
	append text \
		[format "\n               (User %s, Date %s, Gold %s, Turn %s)." \
		$user $when $gold $turn]

	return $text
}

# NSHighScore::Dump --
#
#	Dump a list of high scores to a text file. 
#
# Arguments:
#	oop					OOP ID. See above.
#
# Results:
#	What happened.

proc NSHighScore::Dump {oop parent} {

	global Angband
	variable highscore
	variable highscore_list

	set win [Info $oop win]

	set fileName [tk_getSaveFile -initialfile scores.txt \
		-initialdir [PathUser] -parent $parent]
	if {![string length $fileName]} return

	if {[catch {openlf $fileName} fileId]} {
		set msg "The following error occurred while attempting to open "
		append msg "the score-dump file for writing:\n\n$fileId"
		tk_messageBox -title Oops -message $msg
		return
	}

	puts $fileId "  \[$Angband(name) $Angband(vers) Hall of Fame\]\n"

	GetTheScores $oop

	set index 0
	foreach score $highscore_list {
		array set highscore $score
		puts $fileId [FormatScore $oop [incr index]]\n
	}

	close $fileId

	return
}

# NSHighScore::Find --
#
#	Search for a score containing a string.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::Find {oop again} {

	variable highscore
	variable highscore_list
	variable Priv

	set tree [Info $oop tree]

	set numScores [llength $highscore_list]

	# Repeat the last search
	if {$again && [string length $Priv(find,string)]} {

		set string $Priv(find,string)

	# Enter a string to find, start from the beginning
	} else {

		# Ask the user for a string
		set string [NSUtils::StringBox -title [mc Find] \
			-initial $Priv(find,string) -prompt [mc find-prompt] \
			-buttons [list [mc Find] [mc Cancel]] -parent [Info $oop win]]

		# User cancelled
		if {![string length $string]} return

		# Search again
		set Priv(find,string) $string
		set Priv(find,index) -1
	}
	incr Priv(find,index)

	set string [string tolower $string]

	for {set row $Priv(find,index)} {$row < $numScores} {incr row} {
		set item [$tree index "root child $row"]
		set index [Info $oop match,$item]
		array set highscore [lindex $highscore_list $index]
		set text "$highscore(who) $highscore(race) $highscore(class) $highscore(how)"
		set text [string tolower $text]
		if {[string first $string $text] != -1} {
			NSList::Activate $tree "root child $row"
			set Priv(find,index) $row
			return
		}
	}

	set max $Priv(find,index)
	if {$max >= $numScores} {
		set max [expr {$numScores - 1}]
	}
	for {set row 0} {$row < $max} {incr row} {
		set item [$tree index "root child $row"]
		set index [Info $oop match,$item]
		array set highscore [lindex $highscore_list $index]
		set text "$highscore(who) $highscore(race) $highscore(class) $highscore(how)"
		set text [string tolower $text]
		if {[string first $string $text] != -1} {
			NSList::Activate $tree "root child $row"
			set Priv(find,index) $row
			return
		}
	}

	# The string wasn't found
	set Priv(find,index) -1

	return
}

# NSHighScore::InvokeRow --
#
#	Called when a list item is double-clicked etc.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::InvokeRow {oop row} {

	variable highscore_list

	if {$row == -1} return

	set tree [Info $oop tree]
	set item [$tree index "root child $row"]
	set index [Info $oop match,$item]
	array set highscore [lindex $highscore_list $index]

	if {$highscore(_record) ne ""} {
		if {[NSModule::LoadIfNeeded NSRecord]} {
			NSRecord::LoadRecords
		}
		NSWindowManager::Display record $highscore(_record)
	}

	return
}

# NSHighScore::Invoke --
#
#	Called when a list item is double-clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::Invoke {oop row} {

	InvokeRow $oop $row

	return
}

# NSHighScore::DeleteRow --
#
#	Delete a score (and record).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::DeleteRow {oop row} {

	variable highscore_list

	set win [Info $oop win]

	set tree [Info $oop tree]
	set item [$tree index "root child $row"]
	set index [Info $oop match,$item]
	array set highscore [lindex $highscore_list $index]
	if {$highscore(_index) == -1} return

	set answer [tk_messageBox -message "Really delete this score?" \
		-parent $win -title "Delete Score" -type yesno -icon question]
	if {$answer eq "no"} return

	angband highscore delete $highscore(_index)

	set record $highscore(_record)
	if {[string length $record]} {
		set answer [tk_messageBox -message "Delete $record?" \
			-parent $win -title "Delete Record" -type yesno -icon question]
		if {$answer eq "yes"} {
			set path [PathUser $record]
			file delete $path
		}
	}

	# Preserve the scroll position and selection
	set item [$tree index "nearest 0 0"]
	set rowTop 0
	if {$item ne ""} {
		set rowTop [NSList::Item2Row $tree $item]
	}
	set current [Info $oop selection,row]

	SetList $oop

	# Restore the scroll position and selection
	$tree yview scroll $rowTop units
	if {$current != -1} {
		set count [expr {[$tree numitems] - 1}]
		if {$current >= $count} {
			set current [expr {$count - 1}]
		}
		if {$count} {
			$tree selection add "root child $current"
		}
	}

	return
}

# NSHighScore::Delete --
#
#	Delete the selected score (and any record, if desired).
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::Delete {oop} {

	set current [Info $oop selection,row]
	if {$current == -1} return
	DeleteRow $oop $current

	return
}

# NSHighScore::Sort --
#
#	Called when a column header is clicked.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::Sort {oop column toggle} {

	set tree [Info $oop tree]

	set column [$tree column cget $column -tag]

	if {($column eq [Info $oop sort,column]) && $toggle} {
		if {[Info $oop sort,order,$column] eq "-decreasing"} {
			Info $oop sort,order,$column -increasing
			set arrow up
		} else {
			Info $oop sort,order,$column -decreasing
			set arrow down
		}
	} else {
		if {[Info $oop sort,order,$column] eq "-decreasing"} {
			set arrow down
		} else {
			set arrow up
		}
		Info $oop sort,column $column
	}
	$tree column configure all -arrow none
	$tree column configure $column -arrow $arrow

	set opts [Info $oop sort,opts,$column]
	lappend opts [Info $oop sort,order,$column]
	foreach tag [Info $oop sort,order] {
		if {$tag ne $column} {
			lappend opts -column $tag {*}[Info $oop sort,opts,$tag] \
				{*}[Info $oop sort,order,$tag]
		}
	}

	$tree item sort root -column $column {*}$opts

	return
}

# NSHighScore::SortLevel --
#
#	Item sort -command. Sort by max exp level.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::SortLevel {oop item1 item2} {

	variable highscore_list

	set index [Info $oop match,$item1]
	array set highscore1 [lindex $highscore_list $index]

	set index [Info $oop match,$item2]
	array set highscore2 [lindex $highscore_list $index]

	if {$highscore1(max_lev) < $highscore2(max_lev)} {
		return -1
	}
	if {$highscore1(max_lev) > $highscore2(max_lev)} {
		return 1
	}

	return 0
}

# NSHighScore::SortDepth --
#
#	Item sort -command. Sort by max depth.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::SortDepth {oop item1 item2} {

	variable highscore_list

	set index [Info $oop match,$item1]
	array set highscore1 [lindex $highscore_list $index]

	set index [Info $oop match,$item2]
	array set highscore2 [lindex $highscore_list $index]

	if {$highscore1(max_dun) < $highscore2(max_dun)} {
		return -1
	}
	if {$highscore1(max_dun) > $highscore2(max_dun)} {
		return 1
	}

	return 0
}

# NSHighScore::SortCheck --
#
#	Item sort -command. Sort by checkbutton on/off.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSHighScore::SortCheck {oop C item1 item2} {

	set T [Info $oop tree]

	set s1 [$T item state forcolumn $item1 $C]
	set s2 [$T item state forcolumn $item2 $C]
	if {$s1 eq $s2} { return 0 }
	if {[lsearch -exact $s1 checked] == -1} { return -1 }
	return 1
}

