namespace eval FeatEditor {
}

proc FeatEditor::New {id parent feat command} {

	global Quest
	global Window

	set w .feat$id
	toplevel $w
	wm title $w "Feature Editor"
	set Window(feat,$id) $w
	wm transient $w $parent

	wm protocol $w WM_DELETE_WINDOW "FeatEditor::Close $id"

	bind $w <KeyPress-Escape> "FeatEditor::Close $id"

	set Window(feat,$id,command) $command
	FromList $id $feat
	set Window(feat,$id,after) ""

	set frame [frame $w.frameLetter -borderwidth 0]
	label $frame.label -text Letter: -width 10 -anchor w
	entry $frame.entry -width 3 -textvariable Window(feat,$id,letter) \
		-validate key -validatecommand "FeatEditor::Validate $id letter %P"
	pack $frame.label $frame.entry -side left
	pack $frame -side top -anchor w

	set frame [frame $w.frameFeature -borderwidth 0]
	label $frame.label -text Feature: -width 10 -anchor w
	entry $frame.entry -width 3 -textvariable Window(feat,$id,feature) \
		-validate key -validatecommand "FeatEditor::Validate $id feature %P"
	button $frame.btn -text Choose -command "FeatEditor::ChooseFeat $id"
	pack $frame.label $frame.entry $frame.btn -side left
	pack $frame -side top -anchor w

	set frame [frame $w.frameInfo -borderwidth 0]
	label $frame.label -text "Cave Info:" -width 10 -anchor w
	set menu $frame.mb.menu
	menubutton $frame.mb -menu $menu \
		-textvariable Window(feat,$id,cave_info) \
		-indicatoron yes -relief raised
	menu $menu -tearoff no
	set i 0
	set cave_info $Window(feat,$id,cave_info)
	foreach s {mark glow icky room} {
		set Window(feat,$id,cave_info,$i) [expr ($cave_info & (1 << $i)) != 0]
		$menu add checkbutton -label $s \
			-variable Window(feat,$id,cave_info,$i) \
			-command "FeatEditor::ToggleInfo $id $i"
		incr i
	}
	pack $frame.label $frame.mb -side left
	pack $frame -side top -anchor w

	set frame [frame $w.frameMonster -borderwidth 0]
	label $frame.label -text Monster: -width 10 -anchor w
	entry $frame.entry -width 3 -textvariable Window(feat,$id,monster) \
		-validate key -validatecommand "FeatEditor::Validate $id monster %P"
	button $frame.btn -text Choose -command "FeatEditor::ChooseMonster $id"
	pack $frame.label $frame.entry $frame.btn -side left
	pack $frame -side top -anchor w

	set frame [frame $w.frameObject -borderwidth 0]
	label $frame.label -text Object: -width 10 -anchor w
	entry $frame.entry -width 3 -textvariable Window(feat,$id,object) \
		-validate key -validatecommand "FeatEditor::Validate $id object %P"
	button $frame.btn -text Choose -command "FeatEditor::ChooseObject $id"
	pack $frame.label $frame.entry $frame.btn -side left
	pack $frame -side top -anchor w

	set frame [frame $w.frameEgo -borderwidth 0]
	label $frame.label -text Ego: -width 10 -anchor w
	entry $frame.entry -width 3 -textvariable Window(feat,$id,ego) \
		-validate key -validatecommand "FeatEditor::Validate $id ego %P"
	button $frame.btn -text Choose -command "FeatEditor::ChooseEgo $id"
	pack $frame.label $frame.entry $frame.btn -side left
	pack $frame -side top -anchor w

	set frame [frame $w.frameArtifact -borderwidth 0]
	label $frame.label -text Artifact: -width 10 -anchor w
	entry $frame.entry -width 3 -textvariable Window(feat,$id,artifact) \
		-validate key -validatecommand "FeatEditor::Validate $id artifact %P"
	button $frame.btn -text Choose -command "FeatEditor::ChooseArtifact $id"
	pack $frame.label $frame.entry $frame.btn -side left
	pack $frame -side top -anchor w

	set frame [frame $w.frameSpecial -borderwidth 0]
	label $frame.label -text Quest: -width 10 -anchor w
	entry $frame.entry -width 3 -textvariable Window(feat,$id,special) \
		-validate key -validatecommand "FeatEditor::Validate $id special %P"
	button $frame.btn -text Choose -command "FeatEditor::ChooseSpecial $id"
	pack $frame.label $frame.entry $frame.btn -side left
	pack $frame -side top -anchor w

	set frame [frame $w.frameRandom -borderwidth 0]
	label $frame.label -text "Random:" -width 10 -anchor w
	set menu $frame.mb.menu
	menubutton $frame.mb -menu $menu \
		-textvariable Window(feat,$id,random) \
		-indicatoron yes -relief raised
	menu $menu -tearoff no
	set i 2
	set random $Window(feat,$id,random)
	foreach s {monster object ego artifact trap} {
		set Window(feat,$id,random,$i) [expr ($random & (1 << $i)) != 0]
		$menu add checkbutton -label $s \
			-variable Window(feat,$id,random,$i) \
			-command "FeatEditor::ToggleRandom $id $i"
		incr i
	}
	pack $frame.label $frame.mb -side left
	pack $frame -side top -anchor w

	text $w.text -width 40 -height 4
	pack $w.text -side top -expand yes -fill x -anchor w

	$w.text insert end F:[join $feat :]\n
	$w.text insert end [FeatDesc $feat]

	return
}

proc FeatEditor::Close {id} {

	global Window

	set command $Window(feat,$id,command)
	uplevel #0 $command [list [ToList $id]]

	set w $Window(feat,$id)
	destroy $w
	array unset Window feat,$id*

	return
}

proc FeatEditor::Validate {id what string} {

	global Window

	if {$Window(feat,$id,after) != ""} { return 1 }
	set Window(feat,$id,after) [after idle "FeatEditor::Change $id $what"]
	
	return 1
}

proc FeatEditor::Change {id what} {

	global Window

	set w $Window(feat,$id)
	set Window(feat,$id,after) ""
	
	set list [ToList $id]
	$w.text delete 1.0 end
	$w.text insert end F:[join $list :]\n
	$w.text insert end [FeatDesc $list]

	return 1
}

proc FeatEditor::FromList {id list} {

	global Window

	set Window(feat,$id,letter) [lindex $list 0]
	set Window(feat,$id,feature) [lindex $list 1]
	set Window(feat,$id,cave_info) [lindex $list 2]
	set Window(feat,$id,monster) 0
	set Window(feat,$id,object) 0
	set Window(feat,$id,ego) 0
	set Window(feat,$id,artifact) 0
	set Window(feat,$id,trap) 0
	set Window(feat,$id,special) 0
	set Window(feat,$id,random) 0

	set len [llength $list]
	set random 0

	if {[string index [lindex $list 0] 0] == "*"} {
		set random [expr $random | (1 << 1)]
	}

	if {$len >= 3} {
		set Window(feat,$id,cave_info) [lindex $list 2]
	}

	if {$len >= 4} {
		set monster [lindex $list 3]
		if {[string index $monster 0] == "*"} {
			set random [expr $random | (1 << 2)]
			if {[scan $monster *%d lev] == 1} {
				set Window(feat,$id,monster) $lev
			}
		} elseif {$monster} {
			set Window(feat,$id,monster) $monster
		}
	}

	if {$len >= 5} {
		set object [lindex $list 4]
		if {[string index $object 0] == "*"} {
			set random [expr $random | (1 << 3)]
			if {[scan $object *%d lev] == 1} {
				set Window(feat,$id,object) $lev
			}
		} elseif {$object} {
			set Window(feat,$id,object) $object
		}
	}

	if {$len >= 6} {
		set ego [lindex $list 5]
		if {[string index $ego 0] == "*"} {
			set random [expr $random | (1 << 4)]
			if {[scan $ego *%d lev] == 1} {
				set Window(feat,$id,ego) $lev
			}
		} elseif {$ego} {
			set Window(feat,$id,ego) $ego
		}
	}

	if {$len >= 7} {
		set art [lindex $list 6]
		if {[string index $art 0] == "*"} {
			set random [expr $random | (1 << 5)]
			if {[scan $art *%d lev] == 1} {
				set Window(feat,$id,artifact) $lev
			}
		} elseif {$art} {
			set Window(feat,$id,artifact) $art
		}
	}

	if {$len >= 8} {
		set trap [lindex $list 7]
		if {[string index $trap 0] == "*"} {
			set random [expr $random | (1 << 6)]
			if {[scan $trap *%d lev] == 1} {
				set Window(feat,$id,trap) $lev
			}
		} elseif {$trap} {
			set Window(feat,$id,trap) $trap
		}
	}

	if {$len == 9} {
		set Window(feat,$id,special) [lindex $list 8]
	}

	set Window(feat,$id,random) $random
	
	return
}

proc FeatEditor::ToList {id} {

	global Window

	set letter $Window(feat,$id,letter)
	set feature $Window(feat,$id,feature)
	set cave_info $Window(feat,$id,cave_info)
	set monster $Window(feat,$id,monster)
	set object $Window(feat,$id,object)
	set ego $Window(feat,$id,ego)
	set artifact $Window(feat,$id,artifact)
	set trap $Window(feat,$id,trap)
	set special $Window(feat,$id,special)
	set random $Window(feat,$id,random)

	if {$letter == ""} { set letter ? }
	if {$feature == ""} { set feature 0 }
	if {$monster == ""} { set monster 0 }
	if {$object == ""} { set object 0 }
	if {$ego == ""} { set ego 0 }
	if {$artifact == ""} { set artifact 0 }
	if {$trap == ""} { set trap 0 }
	if {$special == ""} { set special 0 }
	
	set doit 0
	set list {}
	if {$special} {
		set list [concat $special $list]
		set doit 1
	}
	if {$trap || ($random & (1 << 6))} {
		set s ""
		if {$random & (1 << 6)} {
			append s *
		}
		if {$trap} {
			append s $trap
		}
		set list [concat $s $list]
		set doit 1
	} elseif {$doit} {
		set list [concat 0 $list]
	}

	if {$artifact || ($random & (1 << 5))} {
		set s ""
		if {$random & (1 << 5)} {
			append s *
		}
		if {$artifact} {
			append s $artifact
		}
		set list [concat $s $list]
		set doit 1
	} elseif {$doit} {
		set list [concat 0 $list]
	}

	if {$ego || ($random & (1 << 4))} {
		set s ""
		if {$random & (1 << 4)} {
			append s *
		}
		if {$ego} {
			append s $ego
		}
		set list [concat $s $list]
		set doit 1
	} elseif {$doit} {
		set list [concat 0 $list]
	}

	if {$object || ($random & (1 << 3))} {
		set s ""
		if {$random & (1 << 3)} {
			append s *
		}
		if {$object} {
			append s $object
		}
		set list [concat $s $list]
		set doit 1
	} elseif {$doit} {
		set list [concat 0 $list]
	}

	if {$monster || ($random & (1 << 2))} {
		set s ""
		if {$random & (1 << 2)} {
			append s *
		}
		if {$monster} {
			append s $monster
		}
		set list [concat $s $list]
		set doit 1
	} elseif {$doit} {
		set list [concat 0 $list]
	}

	set list [concat [list $letter $feature $cave_info] $list]

	return $list
}

proc FeatEditor::ToggleInfo {id n} {

	global Window

	set w $Window(feat,$id)
	set flags 0
	foreach i {0 1 2 3} {
		if {$Window(feat,$id,cave_info,$i)} {
			set flags [expr $flags | (1 << $i)]
		}
	}
	set Window(feat,$id,cave_info) $flags

	Change $id cave_info

	return
}

proc FeatEditor::ToggleRandom {id n} {

	global Window

	set w $Window(feat,$id)
	set flags 0
	foreach i {2 3 4 5 6} {
		if {$Window(feat,$id,random,$i)} {
			set flags [expr $flags | (1 << $i)]
		}
	}
	set Window(feat,$id,random) $flags

	Change $id random

	return
}

proc FeatEditor::ChooseFeat {id} {

	global FInfo
	global Window

	set w $Window(feat,$id)
	set feature $Window(feat,$id,feature)
	set index [lsearch -exact $FInfo(list,f_idx) $feature]
	set index [ListChoice $w "Choose a feature" $FInfo(list,both) $index]
	if {$index == -1} return

	set Window(feat,$id,feature) [lindex $FInfo(list,f_idx) $index]

	Change $id feature

	return
}

proc FeatEditor::ChooseMonster {id} {

	global RInfo
	global Window

	set w $Window(feat,$id)
	set monster $Window(feat,$id,monster)
	set index [lsearch -exact $RInfo(list,r_idx) $monster]
	set index [ListChoice $w "Choose a monster" $RInfo(list,both) $index]
	if {$index == -1} return

	set Window(feat,$id,monster) [lindex $RInfo(list,r_idx) $index]

	FeatEditor::Change $id monster

	return
}

proc FeatEditor::ChooseObject {id} {

	global KInfo
	global Window

	set w $Window(feat,$id)
	set object $Window(feat,$id,object)
	set index [lsearch -exact $KInfo(list,k_idx) $object]
	set index [ListChoice $w "Choose an object" $KInfo(list,both) $index]
	if {$index == -1} return

	set Window(feat,$id,object) [lindex $KInfo(list,k_idx) $index]

	FeatEditor::Change $id object

	return
}

proc FeatEditor::ChooseArtifact {id} {

	global AInfo
	global Window

	set w $Window(feat,$id)
	set artifact $Window(feat,$id,artifact)
	set index [lsearch -exact $AInfo(list,a_idx) $artifact]
	set index [ListChoice $w "Choose an artifact" $AInfo(list,both) $index]
	if {$index == -1} return

	set Window(feat,$id,artifact) [lindex $AInfo(list,a_idx) $index]

	Change $id artifact

	return
}

proc FeatEditor::ChooseEgo {id} {

	global EInfo
	global Window

	set w $Window(feat,$id)
	set ego $Window(feat,$id,ego)
	set index [lsearch -exact $EInfo(list,e_idx) $ego]
	set index [ListChoice $w "Choose an ego" $EInfo(list,both) $index]
	if {$index == -1} return

	set Window(feat,$id,ego) [lindex $EInfo(list,e_idx) $index]

	Change $id ego

	return
}

proc FeatEditor::ChooseSpecial {id} {

	global Quest
	global Window

	set w $Window(feat,$id)
	set special $Window(feat,$id,special)
	set index [lsearch -exact $Quest(list,number) $special]
	set index [ListChoice $w "Choose a quest" $Quest(list,both) $index]
	if {$index == -1} return

	set Window(feat,$id,special) [lindex $Quest(list,number) $index]

	FeatEditor::Change $id special

	return
}

