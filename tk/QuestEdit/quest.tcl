namespace eval QuestEditor {
}

proc QuestEditor::New {questNum} {

	global Quest
	global Window
	global QuestType

	set w .questeditor$questNum
	toplevel $w
	wm title $w "Quest Editor - $questNum: $Quest($questNum,name)"
	wm geometry $w +200+0
	set Window(quest,$questNum) $w
	wm transient $w .plotlist

	wm protocol $w WM_DELETE_WINDOW "QuestEditor::Close $questNum"

	bind $w <KeyPress-Escape> "QuestEditor::Close $questNum"

	#
	# Menubar
	#

	menu $w.mbar -tearoff 0
	$w configure -menu $w.mbar
	menu $w.mbar.m -tearoff 0
	$w.mbar.m add command -label "Write" \
		-command "WriteQuest $questNum"
	$w.mbar.m add command -label "Close" \
		-command "QuestEditor::Close $questNum"
	$w.mbar add cascade -menu $w.mbar.m -label Command

	set frame $w.frameTop
	frame $frame -borderwidth 0
	pack $frame -side top -anchor w

	#
	# Name
	#

	set frame $w.frameName
	frame $frame -borderwidth 0
	label $frame.label -text Name:
	entry $frame.entry -width 30 -textvariable Quest($questNum,name)
	pack $frame.label -side left
	pack $frame.entry -side left
	pack $frame -in $w.frameTop -side left

	#
	# Level
	#

	set frame $w.frameLevel
	frame $frame -borderwidth 0
	label $frame.label -text Level:
	entry $frame.entry -width 3 -textvariable Quest($questNum,level)
	pack $frame.label -side left
	pack $frame.entry -side left
	pack $frame -in $w.frameTop -side left

	#
	# Flags
	#

	set frame $w.frameFlags
	frame $frame -borderwidth 0
	label $frame.label -text Flags:
	set menu $frame.mb.menu
	menubutton $frame.mb -menu $menu \
		-textvariable Quest($questNum,flags) \
		-indicatoron yes -relief raised
	menu $menu -tearoff 0
	pack $frame.label -side left
	pack $frame.mb -side left
	pack $frame -in $w.frameTop -side left

	set i 0
	foreach flag {silent preset once} {
		set Window(quest,$questNum,flag,$i) [expr ($Quest($questNum,flags) & (1 << $i)) != 0]
		$menu add checkbutton -label $flag \
			-command "QuestEditor::SelectFlag $questNum $i" \
			-variable Window(quest,$questNum,flag,$i)
		incr i
	}

	#
	# Type
	#

#	set Window(quest,$questNum,typeT) [lindex $QuestType $Quest($questNum,type)]
	set Window(quest,$questNum,type) $Quest($questNum,type)

	set frame $w.frameType
	frame $frame -borderwidth 0
	label $frame.label -text Type:
	set menu $frame.mbtype.menu
	menubutton $frame.mbtype -menu $menu \
		-textvariable Window(quest,$questNum,typeT) \
		-indicatoron yes -relief raised
	menu $menu -tearoff 0
	pack $frame.label -side left
	pack $frame.mbtype -side left
	pack $frame -side top -anchor w

	set i 1
	foreach type [lrange $QuestType 1 end] {
		$menu add radiobutton -label $type \
			-value $i -variable Window(quest,$questNum,type) \
			-command "QuestEditor::SelectType $questNum $i"
		incr i
	}

	set frame $w.frameTypeContent
	frame $frame -borderwidth 0
	pack $frame -side top -anchor w -padx 20

	#
	# Artifact info
	#

	set frame $w.frameArtifact
	frame $frame -borderwidth 0
	label $frame.label -text Artifact:
	entry $frame.entry -width 3 -textvariable Quest($questNum,k_idx)
	button $frame.button -text Choose -command "QuestEditor::ChooseArtifact $questNum"
	pack $frame.label $frame.entry $frame.button -side left

	#
	# Monster info
	#

	set Window(quest,$questNum,r_idx,hint) $Quest($questNum,r_idx)
	set Window(quest,$questNum,max_num,hint) $Quest($questNum,max_num)

	if {$Quest($questNum,r_idx) == "*"} {
		set Window(quest,$questNum,radio,monster) random
	} else {
		set Window(quest,$questNum,radio,monster) exact
	}
	if {$Quest($questNum,max_num) == "*"} {
		set Window(quest,$questNum,radio,number) random
	} else {
		set Window(quest,$questNum,radio,number) exact
	}

	set frame $w.frameMonster
	frame $frame -borderwidth 0

	# r_idx
	set frame3 $frame.frameIndex
	frame $frame3 -borderwidth 2 -relief groove
	radiobutton $frame3.radio1 -text "Random monster" \
		-variable Window(quest,$questNum,radio,monster) -value random \
		-command "QuestEditor::SynchMonster $questNum"
	radiobutton $frame3.radio2 -text "Specific monster" \
		-variable Window(quest,$questNum,radio,monster) -value exact \
		-command "QuestEditor::SynchMonster $questNum"
	set frame2 $frame3.frameIndex
	frame $frame2 -borderwidth 0
	entry $frame2.entry -width 3 -textvariable ::Quest($questNum,r_idx)
	button $frame2.button -text Choose -command "QuestEditor::ChooseMonster $questNum"
	pack $frame3.radio1 -side top -anchor w
	pack $frame3.radio2 -side top -anchor w
	pack $frame2.entry -side left
	pack $frame2.button -side left
	pack $frame2 -side top -anchor w -padx 20
	pack $frame3 -side left

	# max_num
	set frame3 $frame.frameNumber
	frame $frame3 -borderwidth 2 -relief groove
	radiobutton $frame3.radio1 -text "Random number" \
		-variable Window(quest,$questNum,radio,number) -value random \
		-command "QuestEditor::SynchMonster $questNum"
	radiobutton $frame3.radio2 -text "Specific number" \
		-variable Window(quest,$questNum,radio,number) -value exact \
		-command "QuestEditor::SynchMonster $questNum"
	entry $frame3.entry -width 3 -textvariable Quest($questNum,max_num)
	pack $frame3.radio1 -side top -anchor w
	pack $frame3.radio2 -side top -anchor w
	pack $frame3.entry -side top -anchor w -padx 20
	pack $frame3 -side left

	SynchMonster $questNum
	SelectType $questNum $Quest($questNum,type)

	#
	# Status
	#

	set frame $w.frameStatus
	frame $frame -borderwidth 0
	pack $frame -side top -fill x

	listbox $frame.list -width 10 -height 6 -bg White -exportselection no
	pack $frame.list -side left -expand no -fill y

	set Window(quest,$questNum,status) 1

	foreach status {untaken taken completed rewarded finished failed failed-done} {
		$frame.list insert end $status
	}
	$frame.list selection set 0

	bind $frame.list <<ListboxSelect>> \
		"QuestEditor::SelectStatus $questNum \[%W curselection]"

	#
	# Text
	#

	text $frame.text -width 60 -height 10 -wrap none -font {Courier 8}
	pack $frame.text -side left -expand yes -fill both

	foreach string $Quest($questNum,text,0) {
		$frame.text insert end $string\n
	}

	#
	# Map
	#

	set frame $w.frameMap
	frame $frame -borderwidth 0
	button $frame.btnEdit -text "Edit Map" \
		-command "QuestEditor::EditMap $questNum"
	button $frame.btnDelete -text "Delete Map" \
		-command "QuestEditor::DeleteMap $questNum"
	pack $frame.btnEdit -side left
	pack $frame.btnDelete -side left
	pack $frame -side top -anchor w
	
	return
}

proc QuestEditor::Close {questNum} {

	global Window

	set w $Window(quest,$questNum)

	set status $Window(quest,$questNum,status)
	SaveText $questNum $status

	destroy $w
	array unset Window quest,$questNum*

	return
}

proc QuestEditor::ChooseArtifact {questNum} {

	global AInfo
	global Quest
	global Window

	set w $Window(quest,$questNum)
	set k_idx $Quest($questNum,k_idx)
	if {![string is integer $k_idx]} {
		set k_idx 0
	}
	set index [lsearch -exact $AInfo(list,a_idx) $k_idx]
	set index [ListChoice $w "Choose an artifact" $AInfo(list,both) $index]
	if {$index == -1} return

	set Quest($questNum,k_idx) [lindex $AInfo(list,a_idx) $index]

	return
}

proc QuestEditor::ChooseMonster {questNum} {

	global RInfo
	global Quest
	global Window

	set w $Window(quest,$questNum)
	set r_idx $Quest($questNum,r_idx)
	if {![string is integer $r_idx]} {
		set r_idx 0
	}
	set index [lsearch -exact $RInfo(list,r_idx) $r_idx]
	set index [ListChoice $w "Choose a monster" $RInfo(list,both) $index]
	if {$index == -1} return

	set Quest($questNum,r_idx) [lindex $RInfo(list,r_idx) $index]
	set Window(quest,$questNum,r_idx,hint) $Quest($questNum,r_idx)

	return
}

proc QuestEditor::SelectStatus {questNum status} {

	global Quest
	global Window

	set w $Window(quest,$questNum)
	set prevStatus $Window(quest,$questNum,status)
	if {$prevStatus != $status} {
		SaveText $questNum $prevStatus
		set Window(quest,$questNum,status) $status
	}
	$w.frameStatus.text delete 1.0 end

	foreach string $Quest($questNum,text,$status) {
		$w.frameStatus.text insert end $string\n
	}

	return
}

proc QuestEditor::SaveText {questNum status} {

	global Quest
	global Window

	set w $Window(quest,$questNum)
	set t $w.frameStatus.text
	set Quest($questNum,text,$status) {}
	foreach string [split [string trim [$t get 1.0 end]] \n] {
		lappend Quest($questNum,text,$status) $string
	}

	return
}

proc QuestEditor::SelectType {questNum typeIndex} {

	global Quest
	global QuestType
	global Window

	set w $Window(quest,$questNum)
	set Quest($questNum,type) $typeIndex

	set frame $w.frameTypeContent
	pack forget [pack slaves $frame]
	
	switch -- [lindex $QuestType $typeIndex] {
		kill-level {
			pack $w.frameMonster -in $frame
		}
		kill-any-level {
			pack $w.frameMonster -in $frame
		}
		find-artifact {
			pack $w.frameArtifact -in $frame
		}
		find-exit {
		}
		kill-number {
			pack $w.frameMonster -in $frame
		}
		kill-all {
#			set Quest($questNum,r_idx) 0
#			set Quest($questNum,max_num) 0
		}
		random {
			pack $w.frameMonster -in $frame
		}
	}

	set Window(quest,$questNum,typeT) [lindex $QuestType $Quest($questNum,type)]

	return
}

proc QuestEditor::SelectFlag {questNum n} {

	global Quest
	global QuestType
	global Window

	set w $Window(quest,$questNum)
	set flags 0
	foreach i {0 1 2} {
		if {$Window(quest,$questNum,flag,$i)} {
			set flags [expr $flags | (1 << $i)]
		}
	}
	set Quest($questNum,flags) $flags

	return
}

proc QuestEditor::SynchMonster {questNum} {

	global Quest
	global QuestType
	global Window

	set w $Window(quest,$questNum)

	if {$Window(quest,$questNum,radio,monster) == "random"} {
		set Window(quest,$questNum,r_idx,hint) $Quest($questNum,r_idx)
		set Quest($questNum,r_idx) *
		$w.frameMonster.frameIndex.frameIndex.entry configure -state disabled
	} else {
		set hint $Window(quest,$questNum,r_idx,hint)
		if {$hint != "*"} {
			set Quest($questNum,r_idx) $hint
		}
		$w.frameMonster.frameIndex.frameIndex.entry configure -state normal
	}

	if {$Window(quest,$questNum,radio,number) == "random"} {
		set Window(quest,$questNum,max_num,hint) $Quest($questNum,max_num)
		set Quest($questNum,max_num) *
		$w.frameMonster.frameNumber.entry configure -state disabled
	} else {
		set hint $Window(quest,$questNum,max_num,hint)
		if {$hint != "*"} {
			set Quest($questNum,max_num) $hint
		}
		$w.frameMonster.frameNumber.entry configure -state normal
	}

	return
}

proc QuestEditor::EditMap {questNum} {

	MapEditor::New [Uid] quest $questNum

	return
}

proc QuestEditor::DeleteMap {questNum} {

	global Quest
	global QuestType
	global Window

	set answer [tk_messageBox -type yesno -parent $Window(quest,$questNum) \
		-message "Really delete the map for quest #$questNum: $Quest($questNum,name)?"]
	if {$answer == "no"} return

	set Quest($questNum,layout) {}

	return
}

