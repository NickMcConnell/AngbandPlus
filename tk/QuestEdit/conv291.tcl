namespace eval Conv291 {
}

proc Conv291::ConvAll {} {

	file mkdir [file join $::Root Conv291]

	foreach plot {5 6 7 8} start {101 121 141 161} {
		ReadTownMap /programming/kangbandtk/lib/edit/291/w_info$plot.txt $plot
		ConvPlot /programming/kangbandtk/lib/edit/291/q_info$plot.txt $plot $start
		update
	}

	return
}

proc Conv291::ConvPlot {path townNum start} {

	variable vQuest
	variable vTown

	array unset vQuest

	ReadPlot $path $townNum $start

	# Write the town
	set name [format t%07d.txt $townNum]
	set path2 [file join C:/ Programming KAngbandTk lib edit $name]
	set chan [open $path2 w]
	fconfigure $chan -translation lf
	catch {
		WriteTown $chan $townNum
	} result
	if {$result != ""} { puts $result }
	close $chan
	
	# Write each quest to a file
	foreach questNum $vTown($townNum,quests) {
		set name [format q%07d.txt $questNum]
		set path2 [file join C:/ Programming KAngbandTk lib edit $name]
		set chan [open $path2 w]
		fconfigure $chan -translation lf
		catch {
			WriteQuest $chan $townNum $questNum
		} result
		if {$result != ""} { puts $result }
		close $chan
	}

	# Write q_info.txt entries
	set name [format q_info%d.txt $townNum]
	set path2 [file join C:/ Programming KAngbandTk lib edit $name]
	set chan [open $path2 w]
	fconfigure $chan -translation lf
	puts $chan "#####  Plot $townNum  #####\n"
	foreach questNum $vTown($townNum,quests) {
		puts $chan "# $vQuest($questNum,name)"
		puts $chan "?:\[EQU \$QUEST_NUMBER $questNum]"
		puts $chan "%:[format q%07d $questNum].txt\n"
	}
	close $chan
	
	return
}

proc Conv291::ReadPlot {path townNum start} {

	variable vQuest
	variable vTown

	set buf [ReadFile $path]

	foreach string [split $buf \n] {

		if {![string length $string]} continue
		if {[string index $string 0] == "#"} continue

		if {[string match {N:*} $string]} {
			set list [split $string :]
			set questNum [lindex $list 1]
			set questNum [expr {$start + $questNum - 100}]
			set name [lindex $list 2]
			if {[string equal $name Morgoth] || [string equal $name Sauron]} continue
			set vQuest($questNum,name) $name
			set vQuest($questNum,mon) {0 0 0 0 0 0 0 0 0 0}
			set vQuest($questNum,item) {0 0 0 0 0}
			set vQuest($questNum,kobjects) {0 0 0 0 0 0 0 0 0 0}
			set vQuest($questNum,eobjects) {0 0 0 0 0 0 0 0 0 0}
			set vQuest($questNum,text) {}
			lappend vTown($townNum,quests) $questNum
			continue
		}

		if {[string match {T?:*} $string]} {
			lappend vQuest($questNum,qtext) [string range $string 3 end]
			continue
		}

		if {[string match {Q:*} $string]} {
			set list [split $string :]
			set vQuest($questNum,type) [lindex $list 1]
			set vQuest($questNum,num_mon) [lindex $list 2]
			set vQuest($questNum,cur_num) [lindex $list 3]
			set vQuest($questNum,max_num) [lindex $list 4]
			set vQuest($questNum,level) [lindex $list 5]
			set vQuest($questNum,r_idx) [lindex $list 6]
			set vQuest($questNum,k_idx) [lindex $list 7]
			set vQuest($questNum,y) [lindex $list 8]
			set vQuest($questNum,x) [lindex $list 9]
			set vQuest($questNum,revert) [lindex $list 10]
			set vQuest($questNum,vaultused) [lindex $list 11]
			continue
		}

		if {[string match {Y:*} $string]} {
			set list [split $string :]
			set vQuest($questNum,mon) [lrange $list 1 10]
			set vQuest($questNum,item) [lrange $list 11 15]
			continue
		}

		if {[string match {K:*} $string]} {
			set list [split $string :]
			set vQuest($questNum,kobjects) [lrange $list 1 end]
			continue
		}

		if {[string match {E:*} $string]} {
			set list [split $string :]
			set vQuest($questNum,eobjects) [lrange $list 1 end]
			continue
		}

		if {[string match {D:*} $string]} {
			lappend vQuest($questNum,text) [string range $string 2 end]
			continue
		}
	}

	return
}

proc Conv291::ReadTownMap {path townNum} {

	variable vTown

	set buf [ReadFile $path]

	lappend charMap ";" ","       ; # dirt
	lappend charMap ":" ";"       ; # rubble
	lappend charMap "/" "-"       ; # grass
#	lappend charMap " " "."       ; # empty space -> floor ???
	lappend charMap "P" "."       ; # player start pos

	set knownChar $::TPref(list,letter)
	# Hack -- Will have to add 'D' door to town feats
	lappend knownChar D
	set vTown($townNum,hasD) 0

	set y 0
	foreach string [split $buf \n] {
		if {![string length $string]} break
		set x [string first P $string]
		if {$x != -1} {
			set pos "$y $x"
		}
		if {[string first D $string] != -1} {
			set vTown($townNum,hasD) 1
		}
		set string [string map -nocase $charMap $string]
foreach ch [split $string {}] {
	if {[lsearch -exact $knownChar $ch] == -1} {
		puts "unknown char '$ch' in town $townNum"
	}
}
		lappend vTown($townNum,map) $string
		incr y
	}

	set vTown($townNum,pos) $pos

	return
}

proc Conv291::GetTownCh {townNum y x} {

	variable vTown

	return [string index [lindex $vTown($townNum,map) $y] $x]
}

proc Conv291::SetTownCh {townNum y x ch} {

	variable vTown

	set string [lindex $vTown($townNum,map) $y]
	set string [string replace $string $x $x $ch]
	set vTown($townNum,map) [lreplace $vTown($townNum,map) $y $y $string]

	return
}

# Some default buildings
set Conv291::vBldgText {
############### Buildings ###############

#a
B:0:N:Library:Astinus Loremaster:Human?
B:0:A:0:Research Item:2000:2000:r:1:0
B:0:A:1:Town history:0:0:h:2:0
B:0:A:2:Race legends:0:0:l:3:0

#b
B:1:N:Castle:King Denegor II:Human
B:1:A:0:Look at busts of kings:0:0:l:5:0
B:1:A:1:Request quest:0:0:q:6:0

#c
B:2:N:Arena:Arack Beastmaster:Dwarf
B:2:A:0:Read poster:0:0:r:8:0
B:2:A:1:Arena rules:0:0:a:9:0
B:2:A:2:Enter arena:0:0:e:10:0
B:2:A:3:Look at plaque:0:0:l:11:0

#d
B:3:N:Gambling Den:Materim the Sly:Human
B:3:A:0:Game rules:0:0:r:13:0
B:3:A:1:In-between:0:0:b:12:0
B:3:A:2:Play craps:0:0:c:14:0
B:3:A:3:Spin the wheel:0:0:s:15:0
B:3:A:4:Play dice slots:0:0:d:16:0

#e
B:4:N:White Horse Inn:Goodman Otick:Human
B:4:A:0:Rest for the night:20:20:r:17:0
B:4:A:1:Buy food and drink:1:1:f:18:0

#f
B:5:N:Beastmaster:Lorien Goldenhair:Elf
B:5:A:0:Research monster:2000:2000:r:20:0

#g
B:6:N:Weaponmaster:Suiyan the Swift:Human
B:6:A:0:Compare weapons:1000:1000:c:21:0

#h
B:7:N:Fighters Hall:Barak the Brave:Human
B:7:A:0:Look at plaque:0:0:l:22:0
B:7:A:1:Enchant weapon:200:1000:w:23:0
B:7:A:2:Enchant armor:200:1000:a:24:0
B:7:C:2:0:0:0:0:0:0:0

#i
B:8:N:Tower of Sorcery:Irrident the Wise:Human
B:8:A:0:Look at spires:0:0:l:22:0
B:8:A:1:Recharge item:200:1000:r:25:0
B:8:A:2:Identify posessions:200:1000:i:26:0
B:8:C:0:2:0:0:1:0:0:0

#j
B:9:N:Inner Temple:Holy Crysania:Human
B:9:A:0:Look at busts:0:0:l:22:0
B:9:A:1:Healing prayer:200:1000:r:28:0
B:9:A:2:Restoration:200:1000:i:29:0
B:9:C:0:0:2:0:0:1:0:0

#k
B:10:N:Thieves Guild:Lardbottom the Vain:Hobbit
B:10:A:0:Look at wall:0:0:l:22:0
B:10:A:1:Get share of stolen goods:0:0:g:7:2
B:10:A:2:Rest for the night:0:100:r:17:0
B:10:A:3:Identify posessions:100:2000:i:26:0
B:10:C:0:0:0:2:0:0:0:0

#l
B:11:N:Rangers Tavern:Trallin Fleetfoot:Half-Elf
B:11:A:0:Look at plaque:0:0:l:22:0
B:11:A:1:Enchant arrows:200:1000:a:30:0
B:11:A:2:Enchant bows:200:1000:b:31:0
B:11:C:0:0:0:0:2:0:0:0

#m
B:12:N:Paladins Order:Sir Langor:Half-Elf
B:12:A:0:Look at shrine:0:0:l:22:0
B:12:A:1:Enchant armor:200:1000:a:24:0
B:12:A:2:See healers:200:1000:b:28:0
B:12:C:0:0:0:0:0:2:0:0

#n
B:13:N:Tower of Illusion:Lord Itsukama:Human
B:13:A:0:Look at spires:0:0:l:22:0
B:13:A:1:Recharge item:200:1000:r:25:0
B:13:A:2:Identify posessions:200:1000:i:26:0
B:13:C:0:0:0:1:0:0:2:0

#o
B:14:N:Druids Grove:Dorrio Greenleaf:Elf
B:14:A:0:Look at trees:0:0:l:22:0
B:14:A:1:Healing prayer:200:1000:r:28:0
B:14:A:2:Restoration:200:1000:i:29:0
B:14:C:0:0:0:0:0:0:0:2
}

# Some default rewards
# Note: Home must be given as a reward
# Find-artifact quests probably shouldn't get a reward
set Conv291::vReward {
	{! 1 3 0 41}
	{! 1 3 0 95}
	{! 1 3 0 129}
	{! 1 3 0 318}
	{! 1 3 0 214}
	{! 1 3 0 242}
	{! 1 3 0 155}
	{! 1 3 0 303}
	{! 1 3 0 285}
	{! 1 3 0 434}
	{! 1 3 0 291}
	{! 1 3 0 420}
	{! 1 3 0 375}
	{! 1 3 0 418}
	{! 1 3 0 199}
}

proc Conv291::WriteTown {chan townNum} {

	variable vQuest
	variable vTown

	puts $chan "# Default for castle reward"
	puts $chan "F:!:1:3\n"

	if {$vTown($townNum,hasD)} {
		puts $chan "# Broken door"
		puts $chan "F:D:5:3\n"
	}

	# Find quests sharing an entrance
	set i 0
	foreach q1 $vTown($townNum,quests) {
		incr i
		if {!$vQuest($q1,vaultused)} continue		
		set y1 $vQuest($q1,y)
		set x1 $vQuest($q1,x)
		set sameAs($q1) [list $q1]
		foreach q2 [lrange $vTown($townNum,quests) $i end] {
			if {!$vQuest($q2,vaultused)} continue		
			set y2 $vQuest($q2,y)
			set x2 $vQuest($q2,x)
			if {$y1 == $y2 && $x1 == $x2 && ![info exists seen($q2)]} {
				lappend sameAs($q1) $q2
				set seen($q2) 1
puts "$q2 shares entrance with $q1"
			}
		}
	}
	
	# Quest features
	set i 0
	foreach questNum $vTown($townNum,quests) {
		if {!$vQuest($questNum,vaultused)} continue

		# This quest shares entrance with a previous		
		if {[info exists seen($questNum)]} continue
		
		set ch [lindex {z y x w v u t s r q p ~ ` = @} $i]
		
		set y $vQuest($questNum,y)
		set x $vQuest($questNum,x)
		set chMap [GetTownCh $townNum $y $x]
		if {$chMap == "D"} {
			set f 5
		} else {
			set f $::TPref($chMap)
		}
		set name [TryName FInfo $f]
		puts $chan "# Default for Quest [join $sameAs($questNum) ", "] = entrance is $name"
		puts $chan "F:$ch:$f:3\n"
		foreach q2 $sameAs($questNum) {
			set qsym($q2) "$ch $f"
		}

		# Update the town map
		SetTownCh $townNum $y $x $ch

		incr i
	}

	# Home is a reward for killing Ulfast
	puts $chan "# Default for home (only available as a quest reward)"
	puts $chan F:8:61:3
	foreach questNum $vTown($townNum,quests) {
		if {$vQuest($questNum,type) == 1 && $vQuest($questNum,r_idx) == 211} {
			puts $chan "?:\[EQU \$QUEST$questNum 4]"
			puts $chan F:8:15:3\n
			break
		}
	}

	set i 0
	set rewardIndex 0
	foreach questNum $vTown($townNum,quests) {

		puts $chan "############### Quest $questNum - $vQuest($questNum,name) ###############\n"

		# The first quest
		if {!$i} {
			# Status 0
			puts $chan "# Quest $questNum untaken"
			puts $chan "?:\[EQU \$QUEST$questNum 0]"
			puts $chan F:b:65:3:0:0:0:0:0:$questNum\n
		}

		# Status 1
		set string "# Quest $questNum taken"
		if {$vQuest($questNum,vaultused)} {
			append string ", entrance is quest entrance"
		}
		puts $chan $string
		puts $chan "?:\[EQU \$QUEST$questNum 1]"
		# quest entrance
		if {$vQuest($questNum,vaultused)} {
			scan $qsym($questNum) "%s %d" ch f
			set f 112
			puts $chan F:${ch}:${f}:3:0:0:0:0:0:$questNum
		}
		puts $chan F:b:65:3:0:0:0:0:0:$questNum
		puts $chan ""

		# Status 2
		puts $chan "# Quest $questNum completed"
		puts $chan "?:\[EQU \$QUEST$questNum 2]"
		puts $chan F:b:65:3:0:0:0:0:0:$questNum\n

		# Pick a reward. Find-artifact quests get nothing.
		# One non-fail quest rewards the Home.
		set reward {}
		variable vReward
		if {$vQuest($questNum,type) != 3} {
			# Ulfast
			if {$vQuest($questNum,type) == 1 && $vQuest($questNum,r_idx) == 211} {
				set reward {8 15 3}
			} else {
				set reward [lindex $vReward $rewardIndex]
				if {![llength $reward]} {
					puts "ran out of rewards"
				}
				incr rewardIndex
			}
		}

		catch { unset nextQuest }

		# Not the last quest
		if {$i != [llength $vTown($townNum,quests)] - 1} {

			# Quest after this one
			set nextQuest [lindex $vTown($townNum,quests) [expr $i + 1]]

			# Status 3
			set string "# Quest $questNum rewarding"
			if {[llength $reward]} {
				append string ", reward is [FeatDesc $reward -nofeat]"
			}
			append string ", continue with quest $nextQuest"
			puts $chan $string
			puts $chan "?:\[EQU \$QUEST$questNum 3]"
			puts $chan F:b:65:3:0:0:0:0:0:$nextQuest
			# reward
			if {[llength $reward]} {
				puts $chan "F:[join $reward :]"
			}
			puts $chan ""

			# Status 4
			set string "# Quest $questNum finished"
			append string ", continue with quest $nextQuest"
			puts $chan $string
			puts $chan "?:\[EQU \$QUEST$questNum 4]"
			puts $chan F:b:65:3:0:0:0:0:0:$nextQuest\n

		# Last quest
		} else {

			# Status 3
			set string "# Quest $questNum rewarding"
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

		# QUEST_FLAG_ONCE quests can fail
		if {$vQuest($questNum,vaultused)} {

			# Status 5
			puts $chan "# Quest $questNum failed, wait for the player to enter the castle"
			puts $chan "?:\[EQU \$QUEST$questNum 5]"
			puts $chan F:b:65:3:0:0:0:0:0:$questNum\n

			set string "# Quest $questNum failed but done"
			# Not the last quest
			if {$i != [llength $vTown($townNum,quests)] - 1} {
				append string ", continue with quest $nextQuest"
				set q $nextQuest
			} else {
				set q $questNum
			}
			puts $chan $string

			# Status 6
			puts $chan "?:\[EQU \$QUEST$questNum 6]"
			puts $chan F:b:65:3:0:0:0:0:0:$q\n
		}

		puts $chan "?:1\n"

		incr i
	}

	# Buildings
	variable vBldgText
	puts $chan $vBldgText

	# Town map

	# Now add a ! n,s,w,e of castle entrance
	set y 0
	foreach string $vTown($townNum,map) {
		# Look for castle entrance
		set x [string first b $string]
		if {$x != -1} {
			lappend bList "$y $x"
		}
		incr y
	}
	scan [lindex $bList 0] "%d %d" y0 x0
	# Horizontal
	if {[llength $bList] == 1} {
		set x [expr $x0 + 1]
		set y [expr $y0 - 1]
		set ch [string index [lindex $vTown($townNum,map) $y] $x]
		if {$ch == "#"} {
			set y [expr $y0 + 1]
		}
	# Vertical
	} else {
		set x [expr $x0 - 1]
		set y [expr $y0 + 1]
		set ch [string index [lindex $vTown($townNum,map) $y] $x]
		if {$ch == "#"} {
			set x [expr $x0 + 1]
		}
	}
	SetTownCh $townNum $y $x !

	puts $chan "############### Town Layout ###############\n"
	set y 0
	foreach string $vTown($townNum,map) {
		puts $chan "D:$string"

		# Look for dungeon entrance
		set x [string first > $string]
		if {$x != -1} {
			set pos "$y $x"
		}		
		incr y
	}
	puts $chan ""

	# Position when leaving dungeon
	puts $chan "?:\[EQU \$LEAVING_DUNGEON 1]"
	puts $chan "P:[join $pos :]\n"

	# Default starting position
	puts $chan "?:\[EQU \$LEAVING_QUEST 0]"
	puts $chan "P:[join $vTown($townNum,pos) :]\n"

	# Returning from quest
	foreach questNum $vTown($townNum,quests) {
		if {!$vQuest($questNum,vaultused)} continue
		set y $vQuest($questNum,y)
		set x $vQuest($questNum,x)
		scan $qsym($questNum) "%s %d" ch f

		if 1 {
			set pos "$y $x"
		} else {
			# Pick a floor-like spot n, s, w or e of exit
			set ym1 [expr {$y - 1}]
			set yp1 [expr {$y + 1}]
			set xm1 [expr {$x - 1}]
			set xp1 [expr {$x + 1}]
			set poss [list $ym1 $x $yp1 $x $y $xm1 $y $xp1]
			unset pos
			foreach {y x} $poss {
				set chMap [string index [lindex $vTown($townNum,map) $y] $x]
				if {[string first $chMap  -.,] != -1} {
					set pos "$y $x"
					break
				}
			}
		}
		puts $chan "?:\[EQU $\LEAVING_QUEST $questNum]"
		puts $chan "P:[join $pos :]\n"
	}

	return
}

proc Conv291::WriteQuest {chan townNum questNum} {

	variable vQuest

	foreach name [array names vQuest $questNum,*] {
		scan $name %d,%s num name2
		set q($name2) $vQuest($name)
	}
	
	puts $chan "Q:$questNum:N:$q(name)\n"

	set flags 0
	if {$q(vaultused)} {
		set flags [expr $flags | 0x02] ; # QUEST_FLAG_PRESET
		set flags [expr $flags | 0x04] ; # QUEST_FLAG_ONCE
	}
	puts $chan "Q:$questNum:Q:$q(type):0:0:$q(max_num):$q(level):$q(r_idx):$q(k_idx):$flags\n"

	puts $chan "?:\[LEQ \$QUEST$questNum 2]"
	foreach string $q(qtext) {
		puts $chan "Q:$questNum:T:$string"
	}
	puts $chan "?:1\n"

	# Find-artifact quests get no reward. Ulfast gives home.
	if {$q(type) == 1 && $q(r_idx) == 211} {
		puts $chan "# Description for quest reward"
		puts $chan "?:\[EQU \$QUEST$questNum 3]"
		puts $chan "Q:$questNum:T:The house is yours!"
		puts $chan "?:1\n"
	} elseif {$q(type) != 3} {
		puts $chan "# Description for quest reward"
		puts $chan "?:\[EQU \$QUEST$questNum 3]"
		puts $chan "Q:$questNum:T:A reward for your good work awaits you outside."
		puts $chan "?:1\n"
	} else {
		puts $chan "# Description for quest reward"
		puts $chan "?:\[EQU \$QUEST$questNum 3]"
		puts $chan "Q:$questNum:T:Well done. You may keep the artifact."
		puts $chan "?:1\n"
	}

	if {$q(vaultused)} {
		puts $chan "# Description for quest failed"
		puts $chan "?:\[EQU \$QUEST$questNum 5]"
		puts $chan "Q:$questNum:T:You have failed me, beware you do not do so again!"
		puts $chan "?:1\n"
	}

	# Monsters
	foreach mon $q(mon) ch {a b c d e f g h i j} {
		if {!$mon} continue
		set F [list $ch 1 8 $mon]
		puts $chan "# [FeatDesc $F]"
		puts $chan "F:[join $F :]\n"
	}

	# Artifacts
	foreach item $q(item) ch {1 2 3 4 5} {
		if {!$item} continue
		set F [list $ch 1 8 0 0 0 $item]
		puts $chan "# [FeatDesc $F]"
		puts $chan "F:[join $F :]\n"
	}

	# Objects
	foreach kobj $q(kobjects) eobj $q(eobjects) ch {k l m n o p q r s t} {
		if {!$kobj} continue
		set F [list $ch 1 8 0 $kobj]
		if {$eobj} {
			lappend F $eobj
		}
		puts $chan "# [FeatDesc $F]"
		puts $chan "F:[join $F :]\n"
	}

	set knownChar $::QPref(list,letter)
	lappend knownChar a b c d e f g h i j k l m n o p q r s t 1 2 3 4 5
	# Is it an error that 'T' is not defined in q_pref.txt?
	lappend knownChar T

	if {$q(vaultused)} {

		# Hack -- For "find exit" quests, remove the up-staircase
		if {$q(type) == 4} {
			lappend charMap "<" "."       ; # up staircase
		}
		lappend charMap ";" ","       ; # dirt
		lappend charMap ":" ";"       ; # rubble
		lappend charMap "/" "-"       ; # grass
		lappend charMap "#" "%"       ; # granite (inner -> outer)
		lappend charMap "G" "*"       ; # object -> object
		lappend charMap "E" "<"       ; # quest exit
#		lappend charMap " " "."       ; # empty space -> floor ???
		lappend charMap "P" "."       ; # player start pos

		set y 0
		foreach string $q(text) {
			set x [string first P $string]
			if {$x != -1} {
				set pos "P:$y:$x"
			}
			set string [string map -nocase $charMap $string]
foreach ch [split $string {}] {
	if {[lsearch -exact $knownChar $ch] == -1} {
		puts "unknown char '$ch' in quest $questNum"
	}
}
			puts $chan D:$string
			incr y
		}
		puts $chan ""

		puts $chan $pos
	}

	return
}

