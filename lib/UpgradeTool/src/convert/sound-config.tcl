# File: sound-config.tcl

# Purpose: converter for VARIANT/tk/config/*.snd

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval CvtSoundCfg {

# namespace eval CvtSoundCfg
}

proc CvtSoundCfg::InitModule {} {

	variable InfoCompat
	variable max_r_idx_Src
	variable max_r_idx_Dst

	set verSrc [Global Src,versionStr]
	set verDst [Global Dst,versionStr]

	if {[lsearch -exact $NSVersionDataDst::vRInfoCompat $verSrc] == -1} {
		set InfoCompat 0
		LogMessage "r_info.txt $verSrc != r_info.txt $verDst"

		# Parse old r_info.txt
		Status "Reading old r_info.txt"
		init_r_info Src
		LogMessage "max_r_idx_Src == $max_r_idx_Src"
	
		# Parse new r_info.txt
		Status "Reading new r_info.txt"
		init_r_info Dst
		LogMessage "max_r_idx_Dst == $max_r_idx_Dst"

	} else {
		set InfoCompat 1
		LogMessage "r_info.txt $verSrc == r_info.txt $verDst"
	}

	return
}

proc CvtSoundCfg::CloseModule {} {

	return
}

proc CvtSoundCfg::CvtSoundCfg {oop} {

	return
}

proc CvtSoundCfg::~CvtSoundCfg {oop} {

	variable DataSrc
	variable DataDst

	array unset DataSrc
	array unset DataDst

	return
}

proc CvtSoundCfg::Info {oop info args} {

	global CvtSoundCfg

	# Set info
	if {[llength $args]} {
		set CvtSoundCfg($oop,$info) [lindex $args 0]

	# Get info
	} else {
		return $CvtSoundCfg($oop,$info)
	}

	return
}

proc CvtSoundCfg::Read {oop path} {

	variable DataSrc

	Info $oop exists [file exists $path]
	if {![Info $oop exists]} {
		LogMessage "$path\n  skipping (missing)"
		return
	}

	set verSrc $NSVersionDataSrc::vSoundVersion

	array unset DataSrc $oop,*

	namespace eval Version$verSrc "
		_Init $oop
		source {$path}
	"

	return
}

proc CvtSoundCfg::Convert {oop} {

	variable DataDst

	if {![Info $oop exists]} return

	set verSrc $NSVersionDataSrc::vSoundVersion
	set verDst $NSVersionDataDst::vSoundVersion

	array unset DataDst $oop,*

	if {$verSrc == $verDst} {
		array set DataDst [array get DataSrc $oop,*]
		return
	}

	for {set ver $verSrc} {$ver < $verDst} {incr ver} {
		Convert${ver}To[expr $ver + 1] $oop
	}

	return
}

proc CvtSoundCfg::Convert1To2 {oop} {
}

proc CvtSoundCfg::Convert2To3 {oop} {

	variable DataSrc
	variable DataDst

	# Fake a title
	set DataDst($oop,title) "Imported By UpgradeTool"

	# Use old sound,directory
	set DataDst($oop,dir,list) {}
	set value [CvtValue::GetValueSrc sound,directory]
	if {$value ne ""} {
		set path [file join {*}$value]
		if {[IsFileInX $path [PathSrc lib xtra sound] 0]} {
			set value {CPath lib sound}
		} elseif {[IsFileInX $path [CPathSrc sound] 0]} {
			set value {CPath lib sound}
		}
		lappend DataDst($oop,dir,list) $value
	}

	foreach group $DataSrc($oop,group,list) {
		foreach keyword $DataSrc($oop,keyword,$group) {
			foreach sound $DataSrc($oop,sound,$group,$keyword) {

				# Add "directory zero" index to each sound assignment.
				# This refers to the old sound,directory.
				lappend DataDst($oop,sound,$group,$keyword) 0 $sound
			}
		}

		# Same list of keywords in this group
		set DataDst($oop,keyword,$group) $DataSrc($oop,keyword,$group)
	}

	# Same list of groups
	set DataDst($oop,group,list) $DataSrc($oop,group,list)

	return
}

proc CvtSoundCfg::Update {oop} {

	variable DataDst
	variable InfoCompat
	variable r_idx2nameSrc
	variable r_idx2nameDst

	if {![Info $oop exists]} return

	set verSrc $NSVersionDataSrc::vSoundVersion
	set verDst $NSVersionDataDst::vSoundVersion

	LogMessage "Sound: Update version $verSrc to $verDst"

	# r_info.txt not compatible
	if {!$InfoCompat && [info exists DataDst($oop,keyword,monster)]} {
		set keywordList {}
		foreach keyword $DataDst($oop,keyword,monster) {
			set keyword2 [TranslateMonster $keyword]
			if {![string length $keyword2]} {
				scan $keyword2 "mon%d" r_idx2
				LogMessage "can't match \"$r_idx2nameSrc($r_idx2)\""
				continue
			}
			if {[string compare $keyword $keyword2]} {
				scan $keyword "mon%d" r_idx
				scan $keyword2 "mon%d" r_idx2
				LogMessage "convert $keyword --> $keyword2 ($r_idx2nameSrc($r_idx) -> $r_idx2nameDst($r_idx2))"
			}
			lappend keywordList $keyword2
			set data($oop,sound,monster,$keyword2) $DataDst($oop,sound,monster,$keyword)
			array unset DataDst $oop,sound,monster,$keyword
		}
		set DataDst($oop,keyword,monster) $keywordList
		array set DataDst [array get data]
	}

	Update$verDst $oop

	return
}

proc CvtSoundCfg::Update1 {oop} {

	return
}

proc CvtSoundCfg::Update2 {oop} {

	return
}

proc CvtSoundCfg::Update3 {oop} {

	return
}

proc CvtSoundCfg::Write {oop path} {

	if {![Info $oop exists]} return

	set verDst $NSVersionDataDst::vSoundVersion

	set buf [Write$verDst $oop]
	StringToFile $path $buf

	return
}

proc CvtSoundCfg::Write1 {oop} {

	return
}

proc CvtSoundCfg::Write2 {oop} {

	return
}

proc CvtSoundCfg::Write3 {oop} {

	variable DataDst

	append result [list Title $DataDst($oop,title)] \n
	foreach list $DataDst($oop,dir,list) {
		append result [list Dir $list] \n
	}
	foreach group $DataDst($oop,group,list) {
		append result [list Group $group] \n
		foreach keyword $DataDst($oop,keyword,$group) {
			foreach {dirIndex sound} $DataDst($oop,sound,$group,$keyword) {
				append result [list One $keyword $dirIndex $sound] \n
			}
		}
	}
	return $result
}

#
# Set the r_info[] array with name of each monster
#
proc CvtSoundCfg::init_r_info_txt {fileId which} {

	variable error_idx
	variable error_line
	variable r_idx2name$which
	variable r_name2idx$which
	variable r_idx2word$which
	variable r_dup_count$which
	
	# Current index
	set r_idx -1

	# Just before the first record
	set error_idx -1

	# Just before the first line
	set error_line 0

	# Parse
	while {[set count [gets $fileId buf]] != -1} {

		# Advance the line number
		incr error_line
		
		# Process 'N' for "New/Number/Name"
		if {[regexp "N:(.*):(.*)" $buf ignore r_idx r_name]} {
		
			# Verify information
#			if {$r_idx < $error_idx} {return 4}

			# Verify information
#			if {$r_idx > 547} {return 2}

			# Save the index
			set error_idx $r_idx

			# Save the name
			set r_idx2name${which}($r_idx) $r_name

			set r_name2 $r_name
			
			# Replace spaces in name with _
			regsub -all " " $r_name2 _ r_name2

			# Replace commas in name with _
			regsub -all , $r_name2 _ r_name2

			# There already exists a monster with this name
			if {[info exists r_name2idx${which}($r_name2)]} {
				if {![info exists r_dup_count${which}($r_name2)]} {
					set r_dup_count${which}($r_name2) 1
				}
				incr r_dup_count${which}($r_name2)
				append r_name2 <[set r_dup_count${which}($r_name2)]>

				LogMessage "duplicate $r_name2"
			}
			
			set r_idx2word${which}($r_idx) $r_name2
			set r_name2idx${which}($r_name2) $r_idx

#			LogMessage "\"$r_name\" -> $r_name2"
		}
	}
	
	# Success 
	return 0
}

# Fill the r_info[] array with number,name pairs by reading r_info.txt
proc CvtSoundCfg::init_r_info {which} {

	global errorInfo
	variable error_idx
	variable error_line
	variable max_r_idx_$which
	variable r_idx2name$which
	
	set result 0

	# Get lib/edit/r_info.txt
	set path [file join [Global dir,$which] lib edit r_info.txt]

	if {![file exists $path]} {
		LogMessage ">>>>> can't find file\n  $path"
		return
	}

	LogMessage "parsing $path"

	# Open /lib/edit/r_info.txt for reading
	set fileId [open $path r]

	# Parse the file and catch errors
	if {[catch {init_r_info_txt $fileId $which} errr] || $errr} {
		set err2 $errorInfo
		MsgBox -message "init_r_info_text:\n\nLine $error_line\nerror_idx: $error_idx\n\nError:$errr\n\nerrorInfo:\n$err2"
		set result 1
	}

	# Close the file
	close $fileId

	set max_r_idx_$which [expr $error_idx + 1]

	return $result
}

# Given "monN"-style keyword for a monster sound assignment in the
# old tk/config/sound file, return the "monN" keyword for the new
# tk/config/sound file
proc CvtSoundCfg::TranslateMonster {keyword} {

	variable r_idx2wordSrc
	variable r_name2idxDst

	scan $keyword "mon%d" r_idx
	set r_word $r_idx2wordSrc($r_idx)

	if {[info exists r_name2idxDst($r_word)]} {
		return mon$r_name2idxDst($r_word)
	}
	return ""
}

#
# Version 1
#
namespace eval CvtSoundCfg::Version1 {
proc _Init {oop} {
	variable ::CvtSoundCfg::DataSrc
	set DataSrc($oop) $oop
	set DataSrc($oop,group,list) {}
	return
}
namespace eval snd {
}
proc snd::grp {group} {
	variable ::CvtSoundCfg::DataSrc
	set oop $DataSrc(oop)
	set DataSrc($oop,keyword,group) {}
	lappend DataSrc($oop,group,list) $group
	return
}
proc snd::one {groupIndex keyword sound} {
	variable ::CvtSoundCfg::DataSrc
	set oop $DataSrc(oop)
	set group [lindex $DataSrc($oop,group,list) $groupIndex]
	if {[lsearch -exact $DataSrc($oop,keyword,$group) $keyword] == -1} {
		lappend DataSrc($oop,keyword,$group) $keyword
	}
	lappend DataSrc($oop,sound,$group,$keyword) $sound
	return
}
# CvtSoundCfg::Version1
}

#
# Version 2
#
namespace eval CvtSoundCfg::Version2 {
proc _Init {oop} {
	variable ::CvtSoundCfg::DataSrc
	set DataSrc(oop) $oop
	set DataSrc($oop,group,list) {}
	return
}
proc Group {group} {
	variable ::CvtSoundCfg::DataSrc
	set oop $DataSrc(oop)
	lappend DataSrc($oop,group,list) $group
	set DataSrc($oop,keyword,$group) {}
	set DataSrc(group) $group
	return
}
proc One {keyword sound} {
	variable ::CvtSoundCfg::DataSrc
	set oop $DataSrc(oop)
	set group $DataSrc(group)
	if {[lsearch -exact $DataSrc($oop,keyword,$group) $keyword] == -1} {
		lappend DataSrc($oop,keyword,$group) $keyword
	}
	lappend DataSrc($oop,sound,$group,$keyword) $sound
	return
}
# CvtSoundCfg::Version2
}

#
# Version 3
#
namespace eval CvtSoundCfg::Version3 {
proc _Init {oop} {
	variable ::CvtSoundCfg::DataSrc
	set DataSrc(oop) $oop
	set DataSrc($oop,dir,list) {}
	set DataSrc($oop,group,list) {}
	return
}
proc Title {title} {
	variable ::CvtSoundCfg::DataSrc
	set oop $DataSrc(oop)
	set DataSrc($oop,title) $title
	return
}
proc Dir {list} {
	variable ::CvtSoundCfg::DataSrc
	set oop $DataSrc(oop)
	lappend DataSrc($oop,dir,list) $list
	return
}
proc Group {group} {
	variable ::CvtSoundCfg::DataSrc
	set oop $DataSrc(oop)
	lappend DataSrc($oop,group,list) $group
	set DataSrc($oop,keyword,$group) {}
	set DataSrc(group) $group
	return
}
proc One {keyword dirIndex sound} {
	variable ::CvtSoundCfg::DataSrc
	set oop $DataSrc(oop)
	set group $DataSrc(group)
	if {[lsearch -exact $DataSrc($oop,keyword,$group) $keyword] == -1} {
		lappend DataSrc($oop,keyword,$group) $keyword
	}
	lappend DataSrc($oop,sound,$group,$keyword) $dirIndex $sound
	return
}
# CvtSoundCfg::Version3
}

