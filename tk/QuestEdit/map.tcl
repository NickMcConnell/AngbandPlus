namespace eval MapEditor {
}

proc MapEditor::Info {uid name args} {

	global Window

	if {[llength $args]} {
		set Window(map,$uid,$name) [lindex $args 0]
	} elseif {$name == "win"} {
		return $Window(map,$uid)
	} else {
		return $Window(map,$uid,$name)
	}

	return
}

proc MapEditor::New {uid townOrQuest num} {

	global FInfo
	global QPref
	global Quest
	global Town
	global TPref
	global Window

	set w .mapeditor$uid
	toplevel $w
	wm title $w "Map Editor - $townOrQuest $num"
	set Window(map,$uid) $w
	wm transient $w $Window($townOrQuest,$num)

	wm protocol $w WM_DELETE_WINDOW "MapEditor::Close $uid"

	bind $w <KeyPress-Escape> "MapEditor::Close $uid"

	Info $uid townOrQuest $townOrQuest
	Info $uid num $num

	#
	# Menubar
	#

	menu $w.mbar -tearoff no
	$w configure -menu $w.mbar

	set m1 [menu $w.mbar.m1 -tearoff no]
	$m1 add command -label "Set Size" \
		-command "MapEditor::SetSize $uid"
	$m1 add command -label "New Feature" \
		-command "MapEditor::NewFeat $uid"
	$m1 add command -label "Remove Selected Feature" \
		-command "MapEditor::RemoveFeat $uid"
	$m1 add command -label "Convert 291-292" \
		-command "MapEditor::Convert $uid"
	$m1 add command -label "Close" \
		-command "MapEditor::Close $uid"
	$w.mbar add cascade -menu $m1 -label File

	set m2 [menu $w.mbar.m2 -tearoff no]
	$m2 add command -label "Copy" \
		-command "MapEditor::Copy $uid"
	$m2 add command -label "Paste" \
		-command "MapEditor::Paste $uid"

	set m2m1 [menu $w.mbar.m2.m1 -tearoff no]
	$m2m1 add command -label "Replace Some..." \
		-command "MapEditor::Replace $uid some"
	$m2m1 add command -label "Replace All..." \
		-command "MapEditor::Replace $uid all"
	if {$townOrQuest == "quest"} {
		$m2m1 add command -label "Insert Rows" \
			-command "MapEditor::Insert $uid row"
		$m2m1 add command -label "Insert Columns" \
			-command "MapEditor::Insert $uid column"
		$m2m1 add command -label "Delete Rows" \
			-command "MapEditor::Delete $uid row"
		$m2m1 add command -label "Delete Columns" \
			-command "MapEditor::Delete $uid column"
	}
	$m2 add cascade -label Selection -menu $m2m1

	$m2 add command -label "Undo" \
		-command "MapEditor::Undo $uid"
	$w.mbar add cascade -menu $m2 -label Edit

	#
	# Text + scrollbars
	#

	set t $w.t
	text $t -highlightthickness 0 -wrap none -font {Courier 8} \
		-yscrollcommand "$w.yscroll set" -xscrollcommand "$w.xscroll set" \
		-width 40 -height 20 -cursor "" -background gray
	scrollbar $w.yscroll -command "$t yview" -orient vertical
	scrollbar $w.xscroll -command "$t xview" -orient horizontal

	$t tag configure text -background white
	$t tag configure pos -background yellow
	$t tag configure select -background gray80
	$t tag configure hot -background turquoise2

	bindtags $t [list $t $w all]

	bind $t <Motion> \
		"MapEditor::Motion $uid %x %y"
	bind $t <ButtonPress-1> \
		"MapEditor::ButtonPress1 $uid %x %y"
	bind $t <Button1-Motion> \
		"MapEditor::Motion1 $uid %x %y"

	bind $t <ButtonPress-3> \
		"MapEditor::ButtonPress3 $uid %x %y"
	bind $t <Button3-Motion> \
		"MapEditor::Motion3 $uid %x %y"

	bind $t <Button1-Enter> \
		"MapEditor::StopAutoScan $uid"
	bind $t <Button3-Enter> \
		"MapEditor::StopAutoScan $uid"
	bind $t <Button1-Leave> \
		"MapEditor::Leave $uid %x %y"
	bind $t <Button3-Leave> \
		"MapEditor::Leave $uid %x %y"
	bind $t <ButtonRelease> \
		"MapEditor::StopAutoScan $uid"

	bind $w <KeyPress-p> \
		"MapEditor::SetStartPos $uid"

	frame $w.frameRight -borderwidth 0

	#
	# Letter list (q_pref.txt)
	#

	set frame [frame $w.frameList -borderwidth 0]
	listbox $frame.list -bg White -exportselection no \
		-yscrollcommand "$frame.yscroll set" -width 2 \
		-font {Courier 8}
	scrollbar $frame.yscroll -command "$frame.list yview" -orient vertical
	pack $frame.list -fill y -side left
	pack $frame.yscroll -fill y -side left
	pack $frame -in $w.frameRight -expand yes -fill y -side top

	if {$townOrQuest == "town"} {
		set letterList $TPref(list,letter)
		set featList $TPref(list,feat)
	} else {
		set letterList $QPref(list,letter)
		set featList $QPref(list,feat)
	}
	foreach letter $letterList feat $featList {
		$frame.list insert end $letter
	}

	bind $frame.list <<ListboxSelect>> \
		"MapEditor::ListboxSelect $uid pref \[%W curselection]"

	#
	# Letter list (this quest)
	#

	set frame [frame $w.frameList2 -borderwidth 0]
	listbox $frame.list -bg White -exportselection no \
		-yscrollcommand "$frame.yscroll set" -width 2 \
		-font {Courier 8}
	scrollbar $frame.yscroll -command "$frame.list yview" -orient vertical
	pack $frame.list -fill y -side left
	pack $frame.yscroll -fill y -side left
	pack $frame -in $w.frameRight -expand yes -fill y -side top

	bind $frame.list <Double-ButtonPress-1> \
		"MapEditor::EditFeat $uid \[%W nearest %y]"
	bind $frame.list <<ListboxSelect>> \
		"MapEditor::ListboxSelect $uid self \[%W curselection]"

	if {$townOrQuest == "town"} {
		set featList $Town($num,feat)
	} else {
		set featList $Quest($num,feat)
	}
	foreach list $featList {
		set letter [lindex $list 0]
		set f_idx [lindex $list 1]
		$frame.list insert end $letter
	}

	#
	# Status bar
	#

	text $w.status -height 2
	
	grid rowconfigure $w 0 -weight 1
	grid rowconfigure $w 1 -weight 0
	grid rowconfigure $w 2 -weight 0
	grid columnconfigure $w 0 -weight 1
	grid columnconfigure $w 1 -weight 0
	grid columnconfigure $w 2 -weight 0
	grid $t -row 0 -column 0 -sticky news
	grid $w.yscroll -row 0 -column 1 -sticky ns
	grid $w.xscroll -row 1 -column 0 -sticky we
	grid $w.frameRight -row 0 -column 2 -rowspan 2 -sticky ns
	grid $w.status -row 2 -column 0 -columnspan 3 -sticky we

	Info $uid tool X
	Info $uid after ""

	if {$townOrQuest == "town"} {
		SetMap $uid $Town($num,layout)
	} else {
		SetMap $uid $Quest($num,layout)
	}

	return
}

proc MapEditor::Close {uid} {

	global Quest
	global Town

	set w [Info $uid win]
	set t $w.t
	set layout {}
	foreach string [split [string trim [$t get 1.0 end]] \n] {
		lappend layout $string
	}

	set townOrQuest [Info $uid townOrQuest]
	set num [Info $uid num]
	if {$townOrQuest == "town"} {
		set Town($num,layout) $layout
	} else {
		set Quest($num,layout) $layout
	}

	destroy $w

	return
}

proc MapEditor::SetMap {uid map} {

	set t [Info $uid win].t
	$t delete 1.0 end
	foreach string $map {
		$t insert end $string text \n
	}
	$t delete "end - 1 chars"
	
	ShowStartPos $uid

	return
}

proc MapEditor::EditFeat {uid featIndex} {

	global Quest
	global Town

	set win [Info $uid win]
	set townOrQuest [Info $uid townOrQuest]
	set num [Info $uid num]
	if {$townOrQuest == "town"} {
		set featList $Town($num,feat)
	} else {
		set featList $Quest($num,feat)
	}
	set list [lindex $featList $featIndex]
	FeatEditor::New [Uid] $win $list "MapEditor::FeatCmd $uid $featIndex"

	return
}

proc MapEditor::FeatCmd {uid featIndex F} {

	global Quest
	global Town

	set w [Info $uid win]
	set townOrQuest [Info $uid townOrQuest]
	set num [Info $uid num]
	if {$townOrQuest == "town"} {
		# Get original (unedited) feat
		set F2 [lindex $Town($num,feat) $featIndex]

		# Hack -- Update any quests using this letter
		set letter [lindex $F2 0]
		foreach q $Town($num,quests) {
			if {[string equal $Town($num,$q,symbol,letter) $letter]} {
				set Town($num,$q,symbol,letter) [lindex $F 0]
			}
		}
		set Town($num,feat) [lreplace $Town($num,feat) $featIndex $featIndex $F]
	} else {
		set Quest($num,feat) [lreplace $Quest($num,feat) $featIndex $featIndex $F]
	}

	set listbox $w.frameList2.list
	$listbox delete $featIndex
	$listbox insert $featIndex "[lindex $F 0] - [FeatDesc $F]"

	return
}

proc MapEditor::ListboxSelect {uid where index} {

	global QPref
	global Quest
	global Town
	global TPref

	set w [Info $uid win]
	set townOrQuest [Info $uid townOrQuest]
	set num [Info $uid num]

	set string ""
	if {$townOrQuest == "town"} {
		switch -- $where {
			pref {
				set feat [lindex $TPref(list,feat) $index]
			}
			self {
				set feat [lindex $Town($num,feat) $index]
				set ch [lindex $feat 0]
				set list {}
				foreach q $Town($num,quests) {
					if {![llength $Quest($q,layout)]} continue
					if {[string equal $Town($num,$q,symbol,letter) $ch]} {
						lappend list $q
					}
				}
				if {[llength $list]} {
					set string "  Quest [join $list ", "]"
				}
			}
		}
	} else {
		switch -- $where {
			pref {
				set feat [lindex $QPref(list,feat) $index]
			}
			self {
				set feat [lindex $Quest($num,feat) $index]
			}
		}
	}
	$w.status delete 1.0 end
	$w.status insert end [FeatDesc $feat]$string

	Info $uid tool [lindex $feat 0]

	return
}

proc MapEditor::Motion {uid x y} {

	global QPref
	global Quest
	global Town
	global TPref

	set w [Info $uid win]
	set t $w.t

	$w.status delete 1.0 end
	$t tag remove hot 1.0 end

	set index [$t index @$x,$y]
	set letter [$t get $index]
	if {$letter == "" || $letter == "\n"} return

	scan $index "%d.%d" y1 x1
	incr y1 -1
	$w.status insert end "y,x=$y1,$x1 "

	$t tag add hot $index

	set townOrQuest [Info $uid townOrQuest]
	set num [Info $uid num]

	set string ""

	if {$townOrQuest == "town"} {
		set featList $Town($num,feat)
		set featPrefList $TPref(list,feat)
		set list {}
		foreach q $Town($num,quests) {
			if {![llength $Quest($q,layout)]} continue
			scan $Town($num,pos,quest,$q) "%d %d" y2 x2
			if {$y1 == $y2 && $x1 == $x2} {
				lappend list $q
			}
		}
		if {[llength $list]} {
			set string "  Quest [join $list ", "]"
		}
	} else {
		set featList $Quest($num,feat)
		set featPrefList $QPref(list,feat)
	}
	foreach feat $featList {
		set letter2 [lindex $feat 0]
		if {[string equal $letter $letter2]} {
			$w.status insert end [FeatDesc $feat]$string
			return
		}
	}
	foreach feat $featPrefList {
		set letter2 [lindex $feat 0]
		if {[string equal $letter $letter2]} {
			$w.status insert end [FeatDesc $feat]$string
			return
		}
	}
	$w.status insert end ???

	return
}

proc MapEditor::ButtonPress1 {uid x y} {

	set w [Info $uid win]
	set t $w.t

	$t tag remove hot 1.0 end

	set index [$t index @$x,$y]
	set letter [$t get $index]
	if {$letter == "" || $letter == "\n"} return

	SaveUndo $uid

	$t delete $index
	$t insert $index [Info $uid tool] text

	return
}

proc MapEditor::Motion1 {uid x y} {

	set w [Info $uid win]
	set t $w.t

	set index [$t index @$x,$y]
	set letter [$t get $index]
	if {$letter == "" || $letter == "\n"} return

	$t delete $index
	$t insert $index [Info $uid tool] text

	return
}

proc MapEditor::ButtonPress3 {uid x y} {

	set w [Info $uid win]
	set t $w.t

	$t tag remove hot 1.0 end
	$t tag remove select 1.0 end

	set index [$t index @$x,$y]
	set letter [$t get $index]
	if {$letter == "" || $letter == "\n"} return

	$t mark set anchor $index
	$t mark set anchor2 $index
	Motion3 $uid $x $y

	return
}

proc MapEditor::Motion3 {uid x y} {

	set w [Info $uid win]
	set t $w.t

	if {[lsearch -exact [$t mark names] anchor] == -1} return

	set p1 [$t index anchor]
	set p2 [$t index @$x,$y]
	$t mark set anchor2 $p2

	scan $p1 %d.%d y1 x1
	scan $p2 %d.%d y2 x2

	$w.t tag remove select 1.0 end

	if {$y1 > $y2} {
		set tmp $y1
		set y1 $y2
		set y2 $tmp
	}
	if {$x1 > $x2} {
		set tmp $x1
		set x1 $x2
		set x2 $tmp
	}

	incr x2

	for {set y $y1} {$y <= $y2} {incr y} {
		$t tag add select $y.$x1 $y.$x2
	}

	incr y1 -1
	incr y2 -1
	incr x2 -1
	$w.status delete 1.0 end
	$w.status insert end \
		"$y1 $x1  $y2 $x2  (h [expr $y2 - $y1 + 1] w [expr $x2 - $x1 + 1])"

	return
}

proc MapEditor::AutoScan {uid} {

	set w [Info $uid win]
	set t $w.t

	scan [Info $uid xy] "%d %d" x y

	if {![winfo exists $w]} return
	if {$y >= [winfo height $t]} {
		$t yview scroll 2 units
	} elseif {$y < 0} {
		$t yview scroll -2 units
	} elseif {$x >= [winfo width $t]} {
		$t xview scroll 2 units
	} elseif {$x < 0} {
		$t xview scroll -2 units
	} else {
		return
	}
	Info $uid after [after 50 "MapEditor::AutoScan $uid"]

	return
}

proc MapEditor::Leave {uid x y} {

	Info $uid xy "$x $y"
	AutoScan $uid

	return
}

proc MapEditor::StopAutoScan {uid} {

	if {[Info $uid after] == ""} return
	after cancel [Info $uid after]
	Info $uid after ""

	return
}

proc MapEditor::Replace {uid what} {

	set w [Info $uid win]
	set t $w.t

	if {$what == "some"} {
		set char1 [GetString $w "Original Letter" Letter: "" char]
		if {[string length $char1] != 1} return
	}

	set char2 [GetString $w "New Letter" Letter: "" char]
	if {[string length $char2] != 1} return

	SaveUndo $uid

	foreach {first last} [$t tag ranges select] {
		set buf [$t get $first $last]
		if {$what == "some"} {
			set buf [string map [list $char1 $char2] $buf]
		} else {
			set buf [string repeat $char2 [string length $buf]]
		}
		$t delete $first $last
		$t insert $first $buf text
	}

	return
}

proc MapEditor::Insert {uid what} {

	global Quest

	set w [Info $uid win]
	set t $w.t

	SaveUndo $uid

	scan [$t index end] %d.%d y3 x3

	set char [GetString $w "Insert Letter" Letter: "" char]
	if {[string length $char] != 1} return

	set townOrQuest [Info $uid townOrQuest]
	set num [Info $uid num]

	switch -- $what {
		column {
			scan [$t tag nextrange select 1.0] "%s %s" first last
			scan $first %d.%d y1 x1
			scan $last %d.%d y2 x2
			set string [string repeat $char [expr $x2 - $x1]]
			for {set y 1} {$y < $y3} {incr y} {
				$t insert $y.$x1 $string text
			}

			if {$townOrQuest == "quest"} {
				scan $Quest($num,pos) "%d %d" y x
				if {$x >= $x1} {
					incr x [expr $x2 - $x1]
				}
				set Quest($num,pos) "$y $x"
			}
		}
		row {
			set ranges [$t tag ranges select]
			set index1 [lrange $ranges 0 1]
			set index2 [lrange $ranges end-1 end]
			scan $index1 %d.%d y1 x1
			scan $index2 %d.%d y2 x2
			scan [$t index $y1.end] %d.%d y3 x3
			incr y2
			set string [string repeat $char $x3]
#			set string [string repeat $string\n [expr $y2 - $y1]]
			for {set y $y1} {$y < $y2} {incr y} {
				$t insert "$y.0 - 1 chars" \n {} $string text
			}

			if {$townOrQuest == "quest"} {
				scan $Quest($num,pos) "%d %d" y x
				if {$y >= $y1 - 1} {
					incr y [expr $y2 - $y1]
				}
				set Quest($num,pos) "$y $x"
			}
		}
	}

ShowStartPos $uid

	return
}

proc MapEditor::Delete {uid what} {

	global Quest

	set w [Info $uid win]
	set t $w.t

	scan [$t index end] %d.%d y3 x3

	SaveUndo $uid

	set townOrQuest [Info $uid townOrQuest]
	set num [Info $uid num]

	switch -- $what {
		column {
			scan [$t tag nextrange select 1.0] "%s %s" first last
			scan $first %d.%d y1 x1
			scan $last %d.%d y2 x2
			for {set y 1} {$y <= $y3} {incr y} {
				$t delete $y.$x1 $y.$x2
			}

			if {$townOrQuest == "quest"} {
				scan $Quest($num,pos) "%d %d" y x
				if {$x >= $x1} {
					incr x [expr $x1 - $x2]
				}
				set Quest($num,pos) "$y $x"
			}
		}
		row {
			set ranges [$t tag ranges select]
			set index1 [lrange $ranges 0 1]
			set index2 [lrange $ranges end-1 end]
			scan $index1 %d.%d y1 x1
			scan $index2 %d.%d y2 x2
			incr y2
			$t delete $y1.0 $y2.0

			if {$townOrQuest == "quest"} {
				scan $Quest($num,pos) "%d %d" y x
				if {$y >= $y1} {
					incr y [expr $y1 - $y2]
				}
				set Quest($num,pos) "$y $x"
			}
		}
	}

ShowStartPos $uid

	return
}

proc MapEditor::Paste {uid} {

	set w [Info $uid win]
	set map [selection get -displayof $w -selection CLIPBOARD]
	SetMap $uid [split [string trim $map] \n]

	return
}

proc MapEditor::Undo {uid} {

	global Quest

	set townOrQuest [Info $uid townOrQuest]
	set num [Info $uid num]

	set map [Info $uid undo,map]
	if {$townOrQuest == "quest"} {
		set pos [Info $uid undo,pos]
	}
	SaveUndo $uid

	if {$townOrQuest == "quest"} {
		set Quest($num,pos) $pos
	}
	SetMap $uid $map
		
	return
}

proc MapEditor::SaveUndo {uid} {

	global Quest

	set w [Info $uid win]
	set townOrQuest [Info $uid townOrQuest]
	set num [Info $uid num]
	Info $uid undo,map [split [string trim [$w.t get 1.0 end]] \n]
	if {$townOrQuest == "quest"} {
		Info $uid undo,pos $Quest($num,pos) 
	}

	return
}

proc MapEditor::NewFeat {uid} {

	global Quest
	global Town

	set townOrQuest [Info $uid townOrQuest]
	set num [Info $uid num]
	set feat [list . 1 8]
	if {$townOrQuest == "town"} {
		lappend Town($num,feat) $feat
		EditFeat $uid [expr [llength $Town($num,feat)] - 1]
	} else {
		lappend Quest($num,feat) $feat
		EditFeat $uid [expr [llength $Quest($num,feat)] - 1]
	}

	return
}

proc MapEditor::RemoveFeat {uid} {

	global Quest
	global Town

	set w [Info $uid win]
	set num [Info $uid num]
	set listbox $w.frameList2.list
	set index [$listbox curselection]
	set feat [lindex $Quest($num,feat) $index]

	set answer [tk_messageBox -type yesno \
		-message "Really delete 'F:[join $feat :]'?"]
	if {$answer == "no"} return

	set townOrQuest [Info $uid townOrQuest]
	if {$townOrQuest == "town"} {
		set Town($num,feat) [lreplace $Town($num,feat) $index $index]
	} else {
		set Quest($num,feat) [lreplace $Quest($num,feat) $index $index]
	}
	$listbox delete $index

	return
}

proc MapEditor::ShowStartPos {uid} {

	global Quest
	global Town

	set w [Info $uid win]
	set num [Info $uid num]
	set townOrQuest [Info $uid townOrQuest]
	set t $w.t

	$t tag remove pos 1.0 end

	if {$townOrQuest == "town"} {

		scan $Town($num,pos,dungeon,1) "%d %d" y x
		catch {
			$t tag add pos [expr $y + 1].$x
		}

		scan $Town($num,pos,quest,0) "%d %d" y x
		catch {
			$t tag add pos [expr $y + 1].$x
		}
		
		# Starting position for each quest
		foreach q $Town($num,quests) {
			catch {
				scan $Town($num,pos,quest,$q) "%d %d" y x
				$t tag add pos [expr $y + 1].$x
			}
		}
		return
	}

	scan $Quest($num,pos) "%d %d" y x
	catch {
		$t tag add pos [expr $y + 1].$x
	}

	return
}

proc MapEditor::SetStartPos {uid} {

	global Quest
	global Town

	set w [Info $uid win]
	set townOrQuest [Info $uid townOrQuest]
	set num [Info $uid num]
	set t $w.t

	scan [$t index hot.first] "%d.%d" y x
	incr y -1

	if {$townOrQuest == "town"} {
		set listS [list Birth Dungeon]
		set listQ [list 0 0]
		foreach q $Town($num,quests) {
			if {![llength $Quest($q,layout)]} continue
			lappend listS "Quest $q: $Quest($q,name)"
			lappend listQ $q
		}
		set choice [ListChoice $w Position $listS 0]
		if {$choice == -1} return
		set index [lindex $listQ $choice]
		if {$index} {
			set Town($num,pos,quest,$index) "$y $x"
		} elseif {$choice == 0} {
			set Town($num,pos,quest,0) "$y $x"
		} else {
			set Town($num,pos,dungeon,1) "$y $x"
		}
	} else {
		set Quest($num,pos) "$y $x"
	}

	ShowStartPos $uid

	return
}

# Convert KAngband 291 to 292
proc MapEditor::Convert {uid} {

	global Quest
	global QPref
	global Town
	global TPref

	set t [Info $uid win].t

	lappend charMap ";" ","       ; # dirt
	lappend charMap ":" ";"       ; # rubble
	lappend charMap "/" "-"       ; # grass
	lappend charMap " " "."       ; # ???

	set map {}
	foreach string [split [string trim [$t get 1.0 end]] \n] {
		lappend map [string map -nocase $charMap $string]
	}
	SetMap $uid $map
	
	return
}

proc MapEditor::SetSize {uid} {

	set w [Info $uid win]
	set t $w.t

	set map [split [string trim [$t get 1.0 end]] \n]
	set height [llength $map]
	set width [string length [lindex $map 0]]

	set height [GetString $w Height Height: $height number]
	if {$height == ""} return

	set width [GetString $w Width Width: $width number]
	if {$width == ""} return

	set mapNew {}
	set y 0
	foreach string $map {
		set d [expr {$width - [string length $string]}]
		if {$d > 0} {
			append string [string repeat . $d]
		} else {
			set string [string range $string 0 [expr $width - 1]]
		}
		lappend mapNew $string
		incr y
		if {$y == $height} break
	}
	for {} {$y < $height} {incr y} {
		lappend mapNew [string repeat . $width]
	}

	SetMap $uid $mapNew

	return
}

