# Hack -- Change the font
console eval ".console configure -font {Courier 9}"

# Hack -- Show initial prompt (picky me)
console eval ::tk::ConsolePrompt

# Hack -- Add a "Clear" menu item
console eval {.menubar.edit add command -label "Clear" -underline 4 \
	-command {.console delete 1.0 end ; tkConsolePrompt}}

set tcl_interactive 1 ; # So command abbrevs work
console show

set UniqueId 0
proc Uid {} {
	return [incr ::UniqueId]
}

proc TownList_New {} {

	global Town

	set w .plotlist
	toplevel $w
	wm title $w "Town List"
	wm geometry $w +0+0

	#
	# Town list
	#

	listbox $w.list -width 20 -height 8 -bg White -exportselection no \
		-listvariable Town(list,both)
	pack $w.list -expand yes -fill both
	
	bind $w.list <Double-ButtonPress-1> {
		TownEditor::New [lindex $Town(list,number) [%W nearest %y]]
	}

	return
}

proc WriteFile {path buf} {

	set chan [open $path w]
	fconfigure $chan -translation lf
	puts -nonewline $chan $buf
	close $chan

	return
}

proc ReadFile {path} {

	set chan [open $path]
	set result [read $chan]
	close $chan
	return $result
}

proc TryName {array index} {

	global $array

	if {[info exists ${array}($index,name)]} {
		return [set ${array}($index,name)]
	}
	return [join [list $array ($index)] ""]
}

proc lookup_kind {tval sval} {

	global KInfo

	foreach k_idx $KInfo(list,k_idx) {
		if {$KInfo($k_idx,tval) == $tval && $KInfo($k_idx,sval) == $sval} {
			return $k_idx
		}
	}
	
	return 0
}

proc FeatDesc {list args} {

	global AInfo
	global EInfo
	global FInfo
	global KInfo
	global RInfo

	set doFeat [expr {[lsearch -exact $args -nofeat] == -1}]
	
	set len [llength $list]
	if {!$len} {
		return ""
	}
	set f_idx [lindex $list 1]

	if {[string index $f_idx 0] == "*"} {
		set string "random feature"
	} else {
		set string [TryName FInfo $f_idx]
	}

	if {$doFeat} {
		if {$len >= 3} {
			set info [lindex $list 2]
			if {$info} {
				append string " ("
				set n 0
				foreach i {0 1 2 3} s {mark glow icky room} {
					if {$info & (1 << $i)} {
						if {$n} {
							append string "|"
						}
						append string $s
						incr n
					}
				}
				append string ")"
			}
		}
	}

	# Hack -- Accept "Home" if that's all it is
	if {!$doFeat && ($len > 3)} {
		set string ""
	}

	if {$len >= 4} {
		set r_idx [lindex $list 3]
		if {[string index $r_idx 0] == "*"} {
			append string ", random monster"
			if {[scan $r_idx *%d lev] == 1} {
				append string " (+$lev levels)"
			}
		} elseif {$r_idx} {
			append string ", [TryName RInfo $r_idx]"
		}
	}

	set object_index 0
	set random_object 0

	set ego_index 0
	set random_ego 0

	set art_index 0
	set random_art 0

	set trap_index 0
	set random_trap 0

	if {$len >= 5} {
		set zz [lindex $list 4]
		if {[string index $zz 0] == "*"} {
			set random_object 1
			if {[scan $zz *%d lev] == 1} {
				set object_index $lev
			}
		} elseif {$zz} {
			set object_index $zz
		}
	}

	if {$len >= 6} {
		set zz [lindex $list 5]
		if {[string index $zz 0] == "*"} {
			set random_ego 1
			if {[scan $zz *%d lev] == 1} {
				set ego_index $lev
			}
		} elseif {$zz} {
			set ego_index $zz
		}
	}

	if {$len >= 7} {
		set zz [lindex $list 6]
		if {[string index $zz 0] == "*"} {
			set random_art 1
			if {[scan $zz *%d lev] == 1} {
				set art_index $lev
			}
		} elseif {$zz} {
			set art_index $zz
		}
	}

	if {$len >= 8} {
		set zz [lindex $list 7]
		if {[string index $zz 0] == "*"} {
			set random_trap 1
			if {[scan $zz *%d lev] == 1} {
				set trap_index $lev
			}
		} elseif {$zz} {
			set trap_index $zz
		}
	}

	set string2 ""
	if {$random_art} {
		set string2 "random artifact"
	} elseif {$art_index} {
		if {[info exists AInfo($art_index,name)]} {
			set k_idx [lookup_kind $AInfo($art_index,tval) $AInfo($art_index,sval)]
			set name [TryName KInfo $k_idx]
		} else {
			set name artifact
		}
		set string2 "$name [TryName AInfo $art_index]"
	} elseif {$random_object && $random_trap} {
		set string2 "random object or trap"
		if {$object_index} {
			append string2 " (+$object_index levels)"
		}
	} elseif {$random_trap} {
		set string2 "trap"
	} elseif {$random_ego && $random_object} {
		set string2 "random ego"
	} elseif {$random_ego && $object_index} {
		set string2 "[TryName KInfo $object_index] (random ego)"
	} elseif {$ego_index && $object_index} {
		set string2 "[TryName KInfo $object_index] [TryName EInfo $ego_index]"
	} elseif {$random_object} {
		set string2 "random object"
		if {$object_index} {
			append string2 " (+$object_index levels)"
		}
	} elseif {$object_index} {
		set string2 "[TryName KInfo $object_index]"
	}

	if {[string length $string2]} {
		if {[string length $string]} {
			append string ", "
		}
		append string $string2
	}

	if {$len == 9} {
		set zz [lindex $list 8]
		append string ", special $zz"
	}

	return $string
}

proc VarChange {what index name1 name2 op} {

	global Quest
	global Town
	global Window

	if {$::TraceIgnore} return

if {[catch {
	switch -- $what {
		quest {
			switch -glob -- $name2 {
				*name {
					if {[info exists Window(quest,$index)]} {
						set w $Window(quest,$index)
						wm title $w "Quest Editor - $index: $Quest($index,name)"
					}
					foreach townNum $Town(list,number) {
						set list $Town($townNum,quests)
						set i [lsearch -exact $list $index]
						if {$i != -1} {
							set Town($townNum,quest,list,both) \
								[lreplace $Town($townNum,quest,list,both) $i $i \
								"$index: $Quest($index,name)"]
						}
					}
					set i [lsearch -exact $Quest(list,number) $index]
					set Quest(list,name) [lreplace $Quest(list,name) $i $i $Quest($index,name)]
					set Quest(list,both) [lreplace $Quest(list,both) $i $i "$index: $Quest($index,name)"]
				}
			}
		}
		town {
			switch -glob -- $name2 {
				*name {
					if {[info exists Window(town,$index)]} {
						set w $Window(town,$index)
						wm title $w "Town Editor - $index: $Town($index,name)"
					}
					set i [lsearch -exact $Town(list,number) $index]
					set Town(list,name) [lreplace $Town(list,name) $i $i $Town($index,name)]
					set Town(list,both) [lreplace $Town(list,both) $i $i "$index: $Town($index,name)"]
				}
			}
		}
	}
} err]} { puts $err }

	return
}

proc TraceVar {varName what index} {

	trace variable $varName w "VarChange $what $index"

	return
}

proc ReadTowns {} {

	global Town

	set buf [ReadFile C:/Programming/OmnibandTk/variant/KAngbandTk/lib/edit/pl_info.txt]
	set Town(list,number) {}
	foreach string [split $buf \n] {
		if {[string match N:* $string]} {
			set list [split $string :]
			set name [lindex $list 2]
			set townNum [expr [lindex $list 1] + 1]
			set Town($townNum,name) $name
			TraceVar ::Town($townNum,name) town $townNum

			ReadTown $townNum

			lappend Town(list,number) $townNum
			lappend Town(list,name) $name
			lappend Town(list,both) "$townNum: $name"
		}
	}

	return
}

proc ReadTown {townNum} {

	global Quest
	global Town
	global TownDirty

	set name [format t%07d.txt $townNum]
	set path [file join $::Root Save $name]
	if {[file exists $path]} {
		set buf [ReadFile $path]
	} else {
		set buf [ReadFile C:/Programming/OmnibandTk/variant/KAngbandTk/lib/edit/$name]
	}
	set Town($townNum,quests) {}
	set Town($townNum,quest,list,both) {}
	set Town($townNum,feat) {}
	set Town($townNum,layout) {}
	set Town($townNum,bldg,list,both) {}
	set curQuest 0
	set curStatus -1
	set curWhere ""
	foreach string [split $buf \n] {

		if {![string length $string]} continue
		if {[string index $string 0] == "#"} continue
		
		if {[string match {\?:\[EQU $QUEST*} $string]} {
			scan $string {%s $QUEST%d %d} ignore questNum status
			if {[lsearch -exact $Town($townNum,quests) $questNum] == -1} {
				lappend Town($townNum,quests) $questNum
				foreach i {0 1 2 3 4 5 6} {
					set Town($townNum,feat,$questNum,$i) {}
				}
				set Town($townNum,$questNum,next,done) 0
				set Town($townNum,$questNum,next,fail) 0
				set Town($townNum,$questNum,symbol,letter) ""
				set Town($townNum,$questNum,symbol,feat) 0
				set Town($townNum,$questNum,reward) {}
			}
			set curQuest $questNum
			set curStatus $status
			continue
		}

		if {[string equal ?:1 $string]} {
			set curQuest 0
			set curStatus 0
			continue
		}

		if {[string match {F:*} $string]} {
			set list [lrange [split $string :] 1 end]
			if {!$curQuest} {
				lappend Town($townNum,feat) $list
				continue
			}
			if {[string match F:b:65:3:0:0:0:0:0:* $string]} {
				if {$curStatus == 4} {
					set Town($townNum,$curQuest,next,done) [lindex $list 8]
				} elseif {$curStatus == 6} {
					set Town($townNum,$curQuest,next,fail) [lindex $list 8]
				}
			} elseif {$curStatus == 1} {
				# Quest entrance
				set Town($townNum,$curQuest,symbol,letter) [lindex $list 0]
				set Town($townNum,$curQuest,symbol,feat) [lindex $list 1]
			} elseif {[string equal F:b:65:3 $string]} {
puts "Ignoring \"$string\" in Town $townNum, quest $curQuest"
			} else {
				# Must be reward
				set Town($townNum,$curQuest,reward) $list
#				lappend Town($townNum,feat,$curQuest,$curStatus) $list
			}
			continue
		}

		# Layout
		if {[string match "D:*" $string]} {
			lappend Town($townNum,layout) [string range $string 2 end]
			continue
		}

		if {[string match {\?:\[EQU $LEAVING_*} $string]} {
			scan $string {%s $LEAVING_%s %d} ignore where number
			set curWhere [string tolower $where]
			set curNumber $number
			continue
		}

		if {[string match {P:*} $string]} {
			set list [split $string :]
			set y [lindex $list 1]
			set x [lindex $list 2]
			set Town($townNum,pos,$curWhere,$curNumber) "$y $x"
			continue
		}
	}
if 0 {
	# Now check each town feature. If a quest is using this feature,
	# then remove it from the list of town features and assign it to the
	# quest map symbol.
	set featList {}
	foreach list $Town($townNum,feat) {
		set letter [lindex $list 0]
		set feat [lindex $list 1]
		set addIt 1
		foreach q $Town($townNum,quests) {
			if {[string equal $letter $Town($townNum,$q,symbol,letter)]} {
				set Town($townNum,$q,symbol,feat,default) $feat
				set addIt 0
			}
		}
		if {$addIt} {
			lappend featList $list
		}
	}
	set Town($townNum,feat) $featList
}
	set Town($townNum,quests) [lsort -integer $Town($townNum,quests)]
	foreach q $Town($townNum,quests) {
		lappend Town($townNum,quest,list,both) "$q: $Quest($q,name)"
	}

	array set TownDirty [array get Town $townNum,*]

	return
}

proc ReadQuests {} {

	global Quest

	set wd [pwd]
	cd C:/Programming/OmnibandTk/variant/KAngbandTk/lib/edit
	set glob [glob q00*txt]
	cd [file join $::Root Save]
	set glob [concat $glob [glob -nocomplain q00*txt]]
	cd $wd

	set glob [lsort -dictionary $glob]

	set Quest(list,number) {}
	set Quest(list,name) {}
	set Quest(list,both) {}
	foreach name $glob {
		set name [file tail $name]
		scan $name {q%d.txt} questNum
		ReadQuest $questNum
		lappend Quest(list,number) $questNum
		lappend Quest(list,name) $Quest($questNum,name)
		lappend Quest(list,both) "$questNum: $Quest($questNum,name)"
	}

	return
}

proc InitQuest {questNum} {

	global Quest

	set Quest($questNum,name) Unknown

	set Quest($questNum,type) 1
	# num_mon always zero
	set Quest($questNum,num_mon) 0
	# cur_num always zero
	set Quest($questNum,cur_num) 0
	# max_num 1 or *
	set Quest($questNum,max_num) 1
	set Quest($questNum,level) 0
	# r_idx may be *
	set Quest($questNum,r_idx) 0
	# k_idx is artifact index
	set Quest($questNum,k_idx) 0
	set Quest($questNum,flags) 0

	foreach status {0 1 2 3 4 5 6} {
		set Quest($questNum,text,$status) {}
	}
	set Quest($questNum,layout) {}
	set Quest($questNum,pos) "-1 -1"
	set Quest($questNum,feat) {}

	set Quest($questNum,next,done) 0
	set Quest($questNum,next,fail) 0

	TraceVar ::Quest($questNum,name) quest $questNum

	return
}

proc ReadQuest {questNum} {

	global Quest
	global QuestDirty

	set name [format q%07d.txt $questNum]
	set path [file join $::Root Save $name]
	if {[file exists $path]} {
		set buf [ReadFile $path]
	} else {
		set buf [ReadFile C:/Programming/OmnibandTk/variant/KAngbandTk/lib/edit/$name]
	}

	InitQuest $questNum
	set status {}

	foreach string [split $buf \n] {

		# Expression
		if {[string match {\?:*} $string]} {
			scan $string {?:[%[^]]} expr
			switch -glob -- $expr {
				EQU* {
					set list [split $expr]
					set arg1 [lindex $list 1]
					set arg2 [lindex $list 2]
					if {[string match {$QUEST*} $arg1]} {
						set status $arg2
					} else {
						error "unhandled expression \"$expr\""
					}
				}
				LEQ* {
					set list [split $expr]
					set arg1 [lindex $list 1]
					set arg2 [lindex $list 2]
					if {[string match {$QUEST*} $arg1]} {
						set status {}
						for {set i 0} {$i <= $arg2} {incr i} {
							lappend status $i
						}
					} else {
						error "unhandled expression \"$expr\""
					}
				}
				default {
					error "unhandled expression \"$expr\""
				}
			}
			continue
		}

	 	# Name
		if {[string match "Q:*:N:*" $string]} {
			set list [split $string :]
			set name [lindex $list 3]
			set ::TraceIgnore 1
			set Quest($questNum,name) $name
			set ::TraceIgnore 0
			continue
		}

	 	# Type etc
		if {[string match "Q:*:Q:*" $string]} {
			set list [split $string :]
			set type [lindex $list 3]
			set num_mon [lindex $list 4]
			set cur_num [lindex $list 5]
			set max_num [lindex $list 6]
			set level [lindex $list 7]
			set r_idx [lindex $list 8]
			set k_idx [lindex $list 9]
			if {[llength $list] == 11} {
				set flags [lindex $list 10]
			} else {
				set flags 0
			}
			set Quest($questNum,type) $type
			# num_mon always zero
			set Quest($questNum,num_mon) $num_mon
			# cur_num always zero
			set Quest($questNum,cur_num) $cur_num
			# max_num 1 or *
			set Quest($questNum,max_num) $max_num
			set Quest($questNum,level) $level
			# r_idx may be *
			set Quest($questNum,r_idx) $r_idx
			# k_idx is artifact index
			set Quest($questNum,k_idx) $k_idx
			set Quest($questNum,flags) $flags
			continue
		}

		# Text
		if {[string match "Q:*:T:*" $string]} {
			set list [split $string :]
			# Hack -- handle : in comment
			set text [join [lrange $list 3 end] :]
			set i [lindex $status end]
			lappend Quest($questNum,text,$i) $text
			continue
		}

		# Layout
		if {[string match "D:*" $string]} {
			lappend Quest($questNum,layout) [string range $string 2 end]
			continue
		}

		# Position
		if {[string match "P:*" $string]} {
			set list [split $string :]
			set y [lindex $list 1]
			set x [lindex $list 2]
			set Quest($questNum,pos) "$y $x"
			continue
		}

		# Feature
		if {[string match {F:*} $string]} {
			set list [lrange [split $string :] 1 end]
			lappend Quest($questNum,feat) $list
			continue
		}
	}

	array set QuestDirty [array get Quest $questNum,*]

	return
}

proc ListChoice_Validate {string} {

	global ListChoice

	if {[string length $string]} {
		set index [lsearch -glob $ListChoice(list) *[join $string *]*]
		$ListChoice(listbox) selection clear 0 end
		if {$index != -1} {
			$ListChoice(listbox) selection set $index
			$ListChoice(listbox) see $index
		}
	}

	return 1
}

proc ListChoice {parent title list initial} {

	global ListChoice

	set w .choice
	if {![winfo exists $w]} {
		toplevel $w
		wm withdraw $w

		#
		# Search entry
		#

		set frame $w.frameFind
		frame $frame -borderwidth 0
		label $frame.label -text Find:
		entry $frame.entry -validatecommand "ListChoice_Validate %P" \
			-validate key
		pack $frame.label -side left
		pack $frame.entry -side left -fill x
		pack $frame -side top -fill x

		#
		# List
		#
		
		listbox $w.list -bg White -width 35 -height 25 \
			-listvariable ListChoice(list) \
			-yscrollcommand "$w.yscroll set"
		scrollbar $w.yscroll -command "$w.list yview"
		pack $w.list -side left -expand yes -fill both
		pack $w.yscroll -side left -fill y
		bind $w.list <Double-ButtonPress-1> {
			set ListChoice(choice) [%W nearest %y]
		}
		bind $w <KeyPress-Escape> {
			set ListChoice(choice) -1
		}

		set ListChoice(listbox) $w.list
	}

	set ListChoice(list) $list
	if {$initial < 0 || $initial >= [llength $list]} {
		set initial 0
	}
	$w.list selection clear 0 end
	$w.list selection set $initial
	$w.list see $initial

	wm geometry $w +[expr [winfo x $parent] + 20]+[expr [winfo y $parent] + 20]
	wm transient $w $parent
	wm title $w $title
	wm deiconify $w

	grab $w
	focus $w.list
	set ListChoice(choice) -1
	tkwait variable ListChoice(choice)

	grab release $w
	wm withdraw $w

	return $ListChoice(choice)
}

proc GetString {parent title prompt initial type} {

	global GetString

	set w .getstring
	if {![winfo exists $w]} {
		toplevel $w
		wm withdraw $w

		label $w.label
		entry $w.entry -width 25
		pack $w.label -side left
		pack $w.entry -side left

		bind $w <KeyPress-Escape> { set GetString(result) 0 }
		bind $w <KeyPress-Return> { set GetString(result) 1 }
	}

	$w.label configure -text $prompt
	$w.entry delete 0 end
	$w.entry insert end $initial

	wm geometry $w +[expr [winfo x $parent] + 20]+[expr [winfo y $parent] + 20]
	wm transient $w $parent
	wm title $w $title
	wm deiconify $w

	grab $w
	focus $w.entry
	set GetString(result) 0
	tkwait variable GetString(result)

	grab release $w
	wm withdraw $w

	if {$GetString(result)} {
		return [$w.entry get]
	}

	return ""
}

proc CheckDirtyQuest {q} {

	global Quest
	global QuestDirty
	global Dirty

	set result 0
	foreach name [array names Quest $q,*] {
		if {[string compare $Quest($name) $QuestDirty($name)]} {
			puts "CheckDirtyQuest $name"
			set Dirty(quest,$name) 1
			set result 1
		} else {
			set Dirty(quest,$name) 0
		}
	}

	return $result
}

proc CheckDirtyTown {t} {

	global Town
	global TownDirty
	global Dirty

	set result 0
	foreach name [array names Town $t,*] {
		if {[string compare $Town($name) $TownDirty($name)]} {
			puts "CheckDirtyTown $name"
			set Dirty(town,$name) 1
			set result 1
		} else {
			set Dirty(town,$name) 0
		}
	}

	foreach q $Town($t,quests) {
		if {$Dirty(quest,$q,name)} {
			puts "CheckDirtyTown $t quest $q,name"
			set result 1
		}
	}

	return $result
}

proc CheckDirty {{write 0}} {

	global Quest
	global Town

	foreach q $Quest(list,number) {
		if {[CheckDirtyQuest $q] && $write} {
			WriteQuest $q
		}
	}
	foreach t $Town(list,number) {
		if {[CheckDirtyTown $t] && $write} {
			WriteTown $t
		}
	}

	return
}

proc WriteTown {townNum {test 0}} {

	global FInfo
	global Quest
	global Town
	global TownDirty

	if {$test} {
		set chan stdout
	} else {
		set name [format t%07d.txt $townNum]
		set path [tk_getSaveFile -initialdir [file join $::Root Save] -initialfile $name]
		if {$path == ""} return
	
		set chan [open $path.tmp w]
		fconfigure $chan -translation lf
	}

	foreach F $Town($townNum,feat) {

		# Default comment
		set comment "# [FeatDesc $F]"

		# See if this feat is used by a quest
		# Quests may share a letter
		set list {}
		foreach questNum $Town($townNum,quests) {
			set letter $Town($townNum,$questNum,symbol,letter)
			if {[string equal $letter [lindex $F 0]]} {
				lappend list $questNum
			}
		}
		if {[llength $list]} {
			set feat [lindex $F 1]
			if {[info exists FInfo($feat,name)]} {
				set name $FInfo($feat,name)
			} else {
				set name ???
			}
			set comment "# Default for Quest [join $list ", "] = entrance is $name"
		}

		puts $comment
		puts $chan "F:[join $F :]\n"
	}

	foreach questNum $Town($townNum,quests) {
		puts $chan "############### Quest $questNum - $Quest($questNum,name) ###############\n"
if 1 {
		# The first quest
		if {$questNum == [lindex $Town($townNum,quests) 0]} {
			puts $chan "# Quest $questNum untaken"
			puts $chan "?:\[EQU \$QUEST$questNum 0]"
			puts $chan F:b:65:3:0:0:0:0:0:$questNum\n
		}

		set string "# Quest $questNum taken"
		if {[llength $Quest($questNum,layout)]} {
			append string ", entrance is quest entrance"
		}
		puts $chan $string
		puts $chan "?:\[EQU \$QUEST$questNum 1]"
		# quest entrance
		set letter $Town($townNum,$questNum,symbol,letter)
		if {$letter != ""} {
			set feat $Town($townNum,$questNum,symbol,feat)
			puts $chan F:${letter}:${feat}:3:0:0:0:0:0:$questNum
		}
		puts $chan F:b:65:3:0:0:0:0:0:$questNum
		foreach list $Town($townNum,feat,$questNum,1) {
			puts $chan "F:[join $list :]"
		}
		puts $chan ""

		puts $chan "# Quest $questNum completed"
		puts $chan "?:\[EQU \$QUEST$questNum 2]"
		puts $chan F:b:65:3:0:0:0:0:0:$questNum\n

		set q $Town($townNum,$questNum,next,done)
		if {$q} {
			set string "# Quest $questNum rewarding"
			set reward $Town($townNum,$questNum,reward)
			if {[llength $reward]} {
				append string ", reward is [FeatDesc $reward -nofeat]"
			}
			append string ", continue with quest $q"
			puts $chan $string
			puts $chan "?:\[EQU \$QUEST$questNum 3]"
			puts $chan F:b:65:3:0:0:0:0:0:$q
			# reward
			if {[llength $reward]} {
				puts $chan "F:[join $reward :]"
			}
			puts $chan ""

			set string "# Quest $questNum finished"
			append string ", continue with quest $q"
			puts $chan $string
			puts $chan "?:\[EQU \$QUEST$questNum 4]"
			puts $chan F:b:65:3:0:0:0:0:0:$q\n
		} else {
			set string "# Quest $questNum rewarding"
			set reward $Town($townNum,$questNum,reward)
			if {[llength $reward]} {
				append string ", reward is [FeatDesc $reward -nofeat]"
			}
			append string ", no new quest available"
			puts $chan $string
			puts $chan "?:\[EQU \$QUEST$questNum 3]"
			puts $chan F:b:65:3
			# reward
			if {[llength $reward]} {
				puts $chan "F:[join $reward :]"
			}
			puts $chan ""

			set string "# Quest $questNum finished"
			append string ", no new quest available"
			puts $chan $string
			puts $chan "?:\[EQU \$QUEST$questNum 4]"
			puts $chan F:b:65:3\n
		}

		# 'once' quests can fail
		if {$Quest($questNum,flags) & (1 << 2)} {
			puts $chan "# Quest $questNum failed, wait for the player to enter the castle"
			puts $chan "?:\[EQU \$QUEST$questNum 5]"
			puts $chan F:b:65:3:0:0:0:0:0:$questNum\n

			set q $Town($townNum,$questNum,next,fail)
			set string "# Quest $questNum failed but done"
			if {$q} {
				append string ", continue with quest $q"
			} else {
				set q $questNum
			}
			puts $chan $string
			puts $chan "?:\[EQU \$QUEST$questNum 6]"
			puts $chan F:b:65:3:0:0:0:0:0:$q\n
		}
} else {
		foreach status {0 1 2 3 4 5 6} {
			if {[llength $Town($townNum,feat,$questNum,$status)]} {
				puts $chan "?:\[EQU \$QUEST$questNum $status]"
				foreach list $Town($townNum,feat,$questNum,$status) {
					puts $chan "F:[join $list :]"
				}
				puts $chan ""
			}
		}
}
		puts $chan "?:1\n"		
	}

	foreach line $Town($townNum,layout) {
		puts $chan "D:$line"
	}
	puts $chan ""

	puts $chan "?:\[EQU \$LEAVING_DUNGEON 1]"
	puts $chan "P:[join $Town($townNum,pos,dungeon,1) :]\n"

	puts $chan "?:\[EQU \$LEAVING_QUEST 0]"
	puts $chan "P:[join $Town($townNum,pos,quest,0) :]\n"

	foreach questNum $Town($townNum,quests) {
		if {[llength $Quest($questNum,layout)]} {
			puts $chan "?:\[EQU $\LEAVING_QUEST $questNum]"
			puts $chan "P:[join $Town($townNum,pos,quest,$questNum) :]\n"
		}
	}

	if {$test} return

	close $chan

	if {[file exists $path]} {
		file rename -force $path $path.bak
	}
	file rename $path.tmp $path

	array set TownDirty [array get Town $townNum,*]

	return
}

proc WriteQuest {questNum {test 0}} {

	global Quest
	global QuestDirty

	if {$test} {
		set chan stdout
	} else {
		set name [format q%07d.txt $questNum]
		set path [tk_getSaveFile -initialdir [file join $::Root Save] -initialfile $name]
		if {$path == ""} return

		set chan [open $path.tmp w]
		fconfigure $chan -translation lf
	}

	puts $chan "Q:$questNum:N:$Quest($questNum,name)\n"

	set type $Quest($questNum,type)
	set max_num $Quest($questNum,max_num)
	set level $Quest($questNum,level)
	set r_idx $Quest($questNum,r_idx)
	set k_idx $Quest($questNum,k_idx)
	set flags $Quest($questNum,flags)
	puts $chan "Q:$questNum:Q:${type}:0:0:${max_num}:${level}:${r_idx}:${k_idx}:$flags\n"

	if {[llength $Quest($questNum,text,2)]} {
		puts $chan "?:\[LEQ \$QUEST$questNum 2]"
		foreach string $Quest($questNum,text,2) {
			puts $chan "Q:$questNum:T:$string"
		}
		puts $chan "?:1\n"
	}

	foreach status {3 4 5 6} {
		if {[llength $Quest($questNum,text,$status)]} {
			puts $chan "?:\[EQU \$QUEST$questNum $status]"
			foreach string $Quest($questNum,text,$status) {
				puts $chan "Q:$questNum:T:$string"
			}
			puts $chan "?:1\n"
		}
	}

	foreach list $Quest($questNum,feat) {
		puts $chan "# [FeatDesc $list]"
		puts $chan "F:[join $list :]\n"
	}

	if {[llength $Quest($questNum,layout)]} {
		foreach string $Quest($questNum,layout) {
			puts $chan D:$string
		}
		puts $chan ""

		puts $chan "P:[join $Quest($questNum,pos) :]"
	}

	if {$test} return

	close $chan

	if {[file exists $path]} {
		file rename -force $path $path.bak
	}
	file rename $path.tmp $path

	array set QuestDirty [array get Quest $questNum,*]

	return
}

proc ReadQPref {} {

	global QPref

	set path1 C:/Programming/OmnibandTk/variant/KAngbandTk/lib/edit/q_pref.txt
	set path2 [file join $::Root Config QPref]

	if {[file exists $path2] && ([file mtime $path1] < [file mtime $path2])} {
		uplevel #0 [list source $path2]
		return
	}

	set buf [ReadFile $path1]

	foreach string [split $buf \n] {

		if {[string match {F:*} $string]} {
			set list [lrange [split $string :] 1 end]
			set letter [lindex $list 0]
			set f_idx [lindex $list 1]

			set QPref($letter) $f_idx
			lappend QPref(list,letter) $letter
			lappend QPref(list,f_idx) $f_idx
			lappend QPref(list,feat) $list
		}
	}

	set buf ""
	foreach name [lsort -dictionary [array names QPref]] {
		append buf [list set QPref($name) $QPref($name)]\n
	}
	WriteFile $path2 $buf

	return
}

proc ReadTPref {} {

	global TPref

	set path1 C:/Programming/OmnibandTk/variant/KAngbandTk/lib/edit/t_pref.txt
	set path2 [file join $::Root Config TPref]

	if {[file exists $path2] && ([file mtime $path1] < [file mtime $path2])} {
		uplevel #0 [list source $path2]
		return
	}

	set buf [ReadFile $path1]

	foreach string [split $buf \n] {

		if {[string match {F:*} $string]} {
			set list [lrange [split $string :] 1 end]
			set letter [lindex $list 0]
			set f_idx [lindex $list 1]

			set TPref($letter) $f_idx
			lappend TPref(list,letter) $letter
			lappend TPref(list,f_idx) $f_idx
			lappend TPref(list,feat) $list
		}
	}

	set buf ""
	foreach name [lsort -dictionary [array names TPref]] {
		append buf [list set TPref($name) $TPref($name)]\n
	}
	WriteFile $path2 $buf

	return
}

proc ReadAInfo {} {

	global AInfo

	set path1 C:/Programming/OmnibandTk/variant/KAngbandTk/lib/edit/a_info.txt
	set path2 [file join $::Root Config AInfo]

	if {[file exists $path2] && ([file mtime $path1] < [file mtime $path2])} {
		uplevel #0 [list source $path2]
		return
	}

	set buf [ReadFile $path1]

	foreach string [split $buf \n] {

		if {[string match {N:*} $string]} {
			set list [split $string :]
			set a_idx [lindex $list 1]
			set name [lindex $list 2]
			set AInfo($a_idx,name) $name
			lappend AInfo(list,a_idx) $a_idx
			lappend AInfo(list,name) $name
			lappend AInfo(list,both) "$a_idx: $name"
			continue
		}

		if {[string match {I:*} $string]} {
			set list [split $string :]
			set AInfo($a_idx,tval) [lindex $list 1]
			set AInfo($a_idx,sval) [lindex $list 2]
			continue
		}
	}

	set buf ""
	foreach name [lsort -dictionary [array names AInfo]] {
		append buf [list set AInfo($name) $AInfo($name)]\n
	}
	WriteFile $path2 $buf

	return
}

proc ReadEInfo {} {

	global EInfo

	set path1 C:/Programming/OmnibandTk/variant/KAngbandTk/lib/edit/e_info.txt
	set path2 [file join $::Root Config EInfo]

	if {[file exists $path2] && ([file mtime $path1] < [file mtime $path2])} {
		uplevel #0 [list source $path2]
		return
	}

	set buf [ReadFile $path1]

	foreach string [split $buf \n] {
		if {[string match {N:*} $string]} {
			set list [split $string :]
			set e_idx [lindex $list 1]
			set name [lindex $list 2]

			set EInfo($e_idx,name) $name
			lappend EInfo(list,e_idx) $e_idx
			lappend EInfo(list,name) $name
			lappend EInfo(list,both) "$e_idx: $name"
		}
	}

	set buf ""
	foreach name [lsort -dictionary [array names EInfo]] {
		append buf [list set EInfo($name) $EInfo($name)]\n
	}
	WriteFile $path2 $buf

	return
}

proc ReadFInfo {} {

	global FInfo

	set path1 C:/Programming/OmnibandTk/variant/KAngbandTk/lib/edit/f_info.txt
	set path2 [file join $::Root Config FInfo]

	if {[file exists $path2] && ([file mtime $path1] < [file mtime $path2])} {
		uplevel #0 [list source $path2]
		return
	}

	set buf [ReadFile $path1]

	foreach string [split $buf \n] {

		if {[string match {N:*} $string]} {
			set list [split $string :]
			set f_idx [lindex $list 1]
			set name [lindex $list 2]

			set FInfo($f_idx,name) $name
			lappend FInfo(list,f_idx) $f_idx
			lappend FInfo(list,name) $name
			lappend FInfo(list,both) "$f_idx: $name"
		}
	}

	set buf ""
	foreach name [lsort -dictionary [array names FInfo]] {
		append buf [list set FInfo($name) $FInfo($name)]\n
	}
	WriteFile $path2 $buf

	return
}

proc ReadKInfo {} {

	global KInfo

	set path1 C:/Programming/OmnibandTk/variant/KAngbandTk/lib/edit/k_info.txt
	set path2 [file join $::Root Config KInfo]

	if {[file exists $path2] && ([file mtime $path1] < [file mtime $path2])} {
		uplevel #0 [list source $path2]
		return
	}

	set buf [ReadFile $path1]

	foreach string [split $buf \n] {

		if {[string match {N:*} $string]} {
			set list [split $string :]
			set k_idx [lindex $list 1]
			set name [lindex $list 2]
			regsub -all {&|~} $name "" name
			set name [string trim $name]

if {!$k_idx} continue

			set KInfo($k_idx,name) $name
			lappend KInfo(list,k_idx) $k_idx
			continue
		}

		if {[string match {I:*} $string]} {
			set list [split $string :]
			set KInfo($k_idx,tval) [lindex $list 1]
			set KInfo($k_idx,sval) [lindex $list 2]
			continue
		}
	}

	foreach k_idx $KInfo(list,k_idx) {
		set name $KInfo($k_idx,name)
		switch -- $KInfo($k_idx,tval) {
			40 { set name "Amulet of $name" }
			45 { set name "Ring of $name" }
			75 { set name "Potion of $name" }
			55 { set name "Staff of $name" }
			65 { set name "Wand of $name" }
			66 { set name "Rod of $name" }
			70 { set name "Scroll of $name" }
			80 {
				if {$KInfo($k_idx,sval) < 32} {
					set name "Mushroom of $name"
				}
			}
		}
		set KInfo($k_idx,name) $name
		lappend KInfo(list,name) $name
		lappend KInfo(list,both) "$k_idx: $name"
	}

	set buf ""
	foreach name [lsort -dictionary [array names KInfo]] {
		append buf [list set KInfo($name) $KInfo($name)]\n
	}
	WriteFile $path2 $buf

	return
}

proc ReadRInfo {} {

	global RInfo

	set path1 C:/Programming/OmnibandTk/variant/KAngbandTk/lib/edit/r_info.txt
	set path2 [file join $::Root Config RInfo]

	if {[file exists $path2] && ([file mtime $path1] < [file mtime $path2])} {
		uplevel #0 [list source $path2]
		return
	}

	set buf [ReadFile $path1]

	foreach string [split $buf \n] {
		if {[string match {N:*} $string]} {
			set list [split $string :]
			set r_idx [lindex $list 1]
			set name [lindex $list 2]

			if {!$r_idx} continue

			set RInfo($r_idx,name) $name
			lappend RInfo(list,r_idx) $r_idx
			lappend RInfo(list,name) $name
			lappend RInfo(list,both) "$r_idx: $name"
		}
	}

	set buf ""
	foreach name [lsort -dictionary [array names RInfo]] {
		append buf [list set RInfo($name) $RInfo($name)]\n
	}
	WriteFile $path2 $buf

	return
}

proc CleanPath {path} {

	set result {}
	foreach elem [file split $path] {
		if {[string equal $elem .]} continue
		if {[string equal $elem ..]} {
			set result [lrange $result 0 end-1]
			continue
		}
		lappend result $elem
	}

	return [eval file join $result]
}

set Root [file dirname [info script]]
if {[string equal [file pathtype $Root] relative]} {
	set Root [CleanPath [file join [pwd] $Root]]
}

update

source [file join $Root feat.tcl]
source [file join $Root map.tcl]
source [file join $Root quest.tcl]
source [file join $Root town.tcl]
source [file join $Root conv291.tcl]

set QuestType {none kill-level kill-any-level find-artifact find-exit kill-number kill-all random}

set clicks [clock clicks]
ReadAInfo
ReadEInfo
ReadFInfo
ReadKInfo
ReadRInfo
ReadQPref
ReadTPref
puts "Info: [expr [clock clicks] - $clicks]"

set clicks [clock clicks]
ReadQuests
puts "Quests: [expr [clock clicks] - $clicks]"

set clicks [clock clicks]
ReadTowns
puts "Towns: [expr [clock clicks] - $clicks]"

TownList_New

