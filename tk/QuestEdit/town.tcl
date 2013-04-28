namespace eval TownEditor {
}

proc TownEditor::New {townNum} {

	global Quest
	global Town
	global Window

	set w .towneditor$townNum
	toplevel $w
	wm title $w "Town Editor - $townNum: $Town($townNum,name)"
	wm geometry $w +[expr [winfo x .plotlist] + [winfo width .plotlist] + 8]+0
	set Window(town,$townNum) $w
	wm transient $w .plotlist

	wm protocol $w WM_DELETE_WINDOW "TownEditor::Close $townNum"

	bind $w <KeyPress-Escape> "TownEditor::Close $townNum"

	#
	# Menubar
	#

	menu $w.mbar -tearoff 0
	$w configure -menu $w.mbar
	menu $w.mbar.m -tearoff 0
	$w.mbar.m add command -label "Add Existing Quest" \
		-command "TownEditor::AddQuest $townNum"
	$w.mbar.m add command -label "New Quest" \
		-command "TownEditor::NewQuest $townNum"
	$w.mbar.m add command -label "Remove Selected Quest" \
		-command "TownEditor::RemoveQuest $townNum"
	$w.mbar.m add command -label "Write Town" \
		-command "WriteTown $townNum"
	$w.mbar.m add command -label "Edit Map" \
		-command "TownEditor::EditMap $townNum"
	$w.mbar.m add command -label "Close" \
		-command "TownEditor::Close $townNum"
	$w.mbar add cascade -menu $w.mbar.m -label Command

	#
	# Town name
	#

	set frame [frame $w.frameName -borderwidth 0]
	label $frame.label -text "Plot name:"
	entry $frame.entry -width 20 -textvariable Town($townNum,name)
	pack $frame.label $frame.entry -side left

	#
	# Quest list
	#

	set Window(town,$townNum,display) ""
	set Window(town,$townNum,display,index) -1

	set frame [frame $w.frameListQuest -borderwidth 0]
	label $frame.label -text Quests:
	listbox $frame.list -width 25 -height 10 -bg White -exportselection no \
		-yscrollcommand "$frame.yscroll set" \
		-listvariable Town($townNum,quest,list,both)
	scrollbar $frame.yscroll -orient vertical -command "$frame.list yview"
	pack $frame.label -side top -anchor w
	pack $frame.list -side left -fill y
	pack $frame.yscroll -side left -fill y

	bind $frame.list <Double-ButtonPress-1> \
		"QuestEditor::New \[lindex \$Town($townNum,quests) \[%W nearest %y]]"
	bind $frame.list <<ListboxSelect>> \
		"TownEditor::Select $townNum quest \[%W curselection]"

	set Window(town,$townNum,list,quest) $frame.list

	#
	# Building list
	#

	set frame [frame $w.frameListBldg -borderwidth 0]
	label $frame.label -text Buildings:
	listbox $frame.list -width 25 -height 5 -bg White -exportselection no \
		-yscrollcommand "$frame.yscroll set" \
		-listvariable Town($townNum,bldg,list,both)
	scrollbar $frame.yscroll -orient vertical -command "$frame.list yview"
	pack $frame.label -side top -anchor w
	pack $frame.list -side left -fill y
	pack $frame.yscroll -side left -fill y

	bind $frame.list <Double-ButtonPress-1> \
		"BldgEditor::New \[lindex \$Town($townNum,bldg) \[%W nearest %y]]"
	bind $frame.list <<ListboxSelect>> \
		"TownEditor::Select $townNum bldg \[%W curselection]"

	set Window(town,$townNum,list,bldg) $frame.list

	#
	# Contents
	#

	set frame [frame $w.frameContents -borderwidth 0]

	#
	# Contents (quest)
	#

	set frame [frame $w.frameQuestNext -borderwidth 2 -relief groove]
	label $frame.label -text "Next Quest:"
	pack $frame.label -side top -anchor w

	set frame2 [frame $frame.frameDone -borderwidth 0]
	label $frame2.label -text Done: -width 5
	entry $frame2.entry -width 3
	button $frame2.button -text Choose -command "TownEditor::ChooseQuest $townNum done"
	pack $frame2.label $frame2.entry $frame2.button -side left
	pack $frame2 -side top -anchor w -padx 10

	set frame2 [frame $frame.frameFail -borderwidth 0]
	label $frame2.label -text Fail: -width 5
	entry $frame2.entry -width 3
	button $frame2.button -text Choose -command "TownEditor::ChooseQuest $townNum fail"
	pack $frame2.label $frame2.entry $frame2.button -side left
	pack $frame2 -side top -anchor w -padx 10

	#
	# Quest reward
	#
	
	set frame [frame $w.frameQuestReward -borderwidth 2 -relief groove]
	set frame2 [frame $frame.frame2 -borderwidth 0]
	checkbutton $frame.cbtn -text "Give Reward" \
		-variable ::Window(town,$townNum,reward) \
		-command "TownEditor::CheckReward $townNum"
	label $frame.label -width 30 -anchor w
	button $frame.btn -text Choose -command "TownEditor::ChooseReward $townNum"
	pack $frame.cbtn -in $frame2 -side left
	pack $frame.btn -in $frame2 -side right
	pack $frame2 -side top -anchor w -fill x
	pack $frame.label -side top -anchor w

	#
	# Quest symbol
	#
	
	set frame [frame $w.frameQuestSymbol -borderwidth 2 -relief groove]
	label $frame.label -text "Map Symbol:"
	pack $frame.label -side top -anchor w
	# Letter
	set frame2 [frame $frame.frameLetter -borderwidth 0]
	label $frame2.label -text "Letter:" -width 8
	entry $frame2.entry -width 3
	pack $frame2.label $frame2.entry -side left
	pack $frame2 -side top -anchor w -fill x -padx 10
	# Feature
	set frame2 [frame $frame.frameFeat -borderwidth 0]
	label $frame2.label -text "Feature:" -width 8
	entry $frame2.entry -width 3
	button $frame2.btn -text Choose -command "TownEditor::ChooseFeat $townNum"
	pack $frame2.label $frame2.entry $frame2.btn -side left
	pack $frame2 -side top -anchor w -fill x -padx 10
	
	grid rowconfigure $w 0 -weight 0
	grid rowconfigure $w 1 -weight 1
	grid rowconfigure $w 2 -weight 1
	grid columnconfigure $w 0 -weight 0
	grid columnconfigure $w 1 -weight 1

	grid $w.frameName -row 0 -column 0
	grid $w.frameListQuest -row 1 -column 0 -sticky ns
	grid $w.frameListBldg -row 2 -column 0 -sticky ns
	grid $w.frameContents -row 1 -column 1 -rowspan 2 -sticky ns

	return
}

proc TownEditor::Close {townNum} {

	global Window

	set w $Window(town,$townNum)

	destroy $w
	array unset Window town,$townNum*

	return
}

proc TownEditor::AddQuest {townNum {questNum -1}} {

	global Quest
	global Town
	global TownDirty
	global Window

	set w $Window(town,$townNum)

	if {$questNum == -1} {

		# Get a list of quests not already in the town
		set listString {}
		set listNumber {}
		foreach questNum $Quest(list,number) string $Quest(list,both) {
			if {[lsearch -exact $Town($townNum,quests) $questNum] == -1} {
				lappend listString $string
				lappend listNumber $questNum
			}
		}
	
		# Ask for a quest
		set index [ListChoice $w "Choose a quest" $listString 0]
	
		# Cancel
		if {$index == -1} return

		set questNum [lindex $listNumber $index]
	}

	# Add the quest to the list of quests
	lappend Town($townNum,quests) $questNum
	set Town($townNum,quests) [lsort -integer $Town($townNum,quests)]

	# Update the quest listbox
	set i [lsearch -exact $Town($townNum,quests) $questNum]
	set Town($townNum,quest,list,both) \
		[linsert $Town($townNum,quest,list,both) $i "$questNum: $Quest($questNum,name)"]
	set listbox $Window(town,$townNum,list,quest)
	$listbox selection clear 0 end
	$listbox selection set $i
	$listbox see $i

	foreach i {0 1 2 3 4 5 6} {
		set Town($townNum,feat,$questNum,$i) {}
		set TownDirty($townNum,feat,$questNum,$i) {}
	}
	# Eh? Makes sure this town is dirty (I think).
	set Town($townNum,feat,$questNum,2) [list [list b 65 3 0 0 0 0 0 $questNum]]

	set Town($townNum,pos,quest,$questNum) "-1 -1"
	set TownDirty($townNum,pos,quest,$questNum) "-1 -1"

	set Town($townNum,$questNum,next,done) 0
	set Town($townNum,$questNum,next,fail) 0
	set Town($townNum,$questNum,symbol,letter) ""
	set Town($townNum,$questNum,symbol,feat) 0
	set Town($townNum,$questNum,reward) {}
	array set TownDirty [array get Town $townNum,$questNum,*]

	return
}

proc TownEditor::NewQuest {townNum} {

	global Quest
	global QuestDirty
	global Town
	global TownDirty
	global Window

	set w $Window(town,$townNum)
	set questNum [GetString $w "Quest Number" Number: "" integer]
	if {$questNum == ""} return

	if {[lsearch -exact $Quest(list,number) $questNum] != -1} return

	InitQuest $questNum

	set list $Quest(list,number)
	lappend list $questNum
	set list [lsort -integer $list]
	set i [lsearch -exact $list $questNum]
	set Quest(list,number) [linsert $Quest(list,number) $i $questNum]
	set Quest(list,name) [linsert $Quest(list,name) $i $Quest($questNum,name)]
	set Quest(list,both) [linsert $Quest(list,both) $i "$questNum: $Quest($questNum,name)"]

	array set QuestDirty [array get Quest $questNum,*]

if 1 {
	AddQuest $townNum $questNum
} else {

	# Add the quest to the list of quests in this town
	lappend Town($townNum,quests) $questNum
	set Town($townNum,quests) [lsort -integer $Town($townNum,quests)]

	# Update the quest listbox
	set i [lsearch -exact $Town($townNum,quests) $questNum]
	set Town($townNum,quest,list,both) \
		[linsert $Town($townNum,quest,list,both) $i "$questNum: $Quest($questNum,name)"]
	set listbox $Window(town,$townNum,list,quest)
	$listbox selection clear
	$listbox selection set $i
	$listbox see $i

	foreach i {0 1 2 3 4 5 6} {
		set Town($townNum,feat,$questNum,$i) {}
		set TownDirty($townNum,feat,$questNum,$i) {}
	}
	# ?
	set Town($townNum,feat,$questNum,2) [list [list b 65 3 0 0 0 0 0 $questNum]]

	set Town($townNum,pos,quest,$questNum) "-1 -1"
	set TownDirty($townNum,pos,quest,$questNum) "-1 -1"

	set Town($townNum,$questNum,next,done) 0
	set Town($townNum,$questNum,next,fail) 0
	set Town($townNum,$questNum,symbol,letter) ""
	set Town($townNum,$questNum,symbol,feat) 0
	set Town($townNum,$questNum,reward) {}
}

	QuestEditor::New $questNum

	return
}

proc TownEditor::RemoveQuest {townNum} {

	global Quest
	global Town
	global TownDirty
	global Window

	set w $Window(town,$townNum)
	set listbox $Window(town,$townNum,list,quest)

	foreach index [lsort -integer -decreasing [$listbox curselection]] {
		set questNum [lindex $Town($townNum,quests) $index]
		set Town($townNum,quests) \
			[lreplace $Town($townNum,quests) $index $index]
		set Town($townNum,quest,list,both) \
			[lreplace $Town($townNum,quest,list,both) $index $index]
		array unset Town $townNum,feat,$questNum,*
		array unset Town $townNum,pos,quest,$questNum
		array unset TownDirty $townNum,feat,$questNum,*
		array unset TownDirty $townNum,pos,quest,$questNum
	}

	return
}

proc TownEditor::Select {townNum which index} {

	global Town
	global Window

	set w $Window(town,$townNum)
	if {[string compare $which $Window(town,$townNum,display)]} {
		if {[llength [pack slaves $w.frameContents]]} {
			eval pack forget [pack slaves $w.frameContents]
		}
		switch -- $which {
			bldg {
				$Window(town,$townNum,list,quest) selection clear 0 end
			}
			quest {
				$Window(town,$townNum,list,bldg) selection clear 0 end
				pack $w.frameQuestNext -in $w.frameContents -side top -anchor w
				pack $w.frameQuestReward -in $w.frameContents -side top -anchor w
				pack $w.frameQuestSymbol -in $w.frameContents -side top -anchor w
			}
		}
	} elseif {$Window(town,$townNum,display,index) == $index} {
		return
	}
	switch -- $which {
		bldg {
		}
		quest {
			set curQuest [lindex $Town($townNum,quests) $index]
			set reward $Town($townNum,$curQuest,reward)
			set Window(town,$townNum,reward) [expr {[llength $reward] != 0}]
			$w.frameQuestReward.label configure -text [FeatDesc $reward -nofeat]
			$w.frameQuestNext.frameDone.entry configure -textvariable Town($townNum,$curQuest,next,done)
			$w.frameQuestNext.frameFail.entry configure -textvariable Town($townNum,$curQuest,next,fail)
			$w.frameQuestSymbol.frameLetter.entry configure -textvariable Town($townNum,$curQuest,symbol,letter)
			$w.frameQuestSymbol.frameFeat.entry configure -textvariable Town($townNum,$curQuest,symbol,feat)
			set Window(town,$townNum,questNum) $curQuest
		}
	}

	set Window(town,$townNum,display) $which
	set Window(town,$townNum,display,index) $index

	return
}

proc TownEditor::ChooseFeat {townNum which} {

	global FInfo
	global Town
	global Window

	set w $Window(town,$townNum)
	set curQuest $Window(town,$townNum,questNum)
	set feature $Town($townNum,$curQuest,symbol,feat,$which)
	set index [lsearch -exact $FInfo(list,f_idx) $feature]
	set index [ListChoice $w "Choose a feature" $FInfo(list,both) $index]
	if {$index == -1} return

	set Town($townNum,$curQuest,symbol,feat,$which) [lindex $FInfo(list,f_idx) $index]

	return
}

proc TownEditor::ChooseQuest {townNum which} {

	global Quest
	global Town
	global Window

	set w $Window(town,$townNum)
	set curQuest $Window(town,$townNum,questNum)
	set nextQuest $Town($townNum,$curQuest,next,$which)
	set index [lsearch -exact $Quest(list,number) $nextQuest]

	# Get a list of quests in the town
	set listString {}
	set listNumber {}
	foreach questNum $Quest(list,number) string $Quest(list,both) {
		if {[lsearch -exact $Town($townNum,quests) $questNum] != -1} {
			lappend listString $string
			lappend listNumber $questNum
		}
	}

	set index [ListChoice $w "Choose a quest" $listString $index]
	if {$index == -1} return

	set Town($townNum,$curQuest,next,$which) [lindex $listNumber $index]

	return
}

proc TownEditor::ChooseReward {townNum} {

	global Town
	global Window

	set win $Window(town,$townNum)
	set curQuest $Window(town,$townNum,questNum)
	set reward $Town($townNum,$curQuest,reward)

	# No current reward
	if {![llength $reward]} {

		# Find the default feat
		foreach feat $Town($townNum,feat) {
			if {[lindex $feat 0] == "!"} {
				set reward $feat
				break
			}
		}
	}
	FeatEditor::New [Uid] $win $reward "TownEditor::FeatCmd $townNum"

	return
}

proc TownEditor::FeatCmd {townNum feat} {

	global Town
	global Window

	set curQuest $Window(town,$townNum,questNum)
	set Town($townNum,$curQuest,reward) $feat
	$Window(town,$townNum).frameQuestReward.label configure \
		-text [FeatDesc $Town($townNum,$curQuest,reward) -nofeat]

	return
}

proc TownEditor::CheckReward {townNum} {

	global Town
	global Window

	if {!$Window(town,$townNum,reward)} {
		set answer [tk_messageBox -message "Forget the reward?" -type yesno]
		if {[string equal $answer no]} {
			set Window(town,$townNum,reward) 1
			return
		}
		set curQuest $Window(town,$townNum,questNum)
		set Town($townNum,$curQuest,reward) {}
		$Window(town,$townNum).frameQuestReward.label configure -text ""
	} else {
		ChooseReward $townNum
	}

	return
}

proc TownEditor::EditMap {townNum} {

	MapEditor::New [Uid] town $townNum

	return
}

