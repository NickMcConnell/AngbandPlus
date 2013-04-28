# File: .tcl

# Purpose: converter

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval CvtSetting {

# namespace eval CvtSetting
}

proc CvtSetting::InitModule {} {

	return
}

proc CvtSetting::CloseModule {} {

	return
}

proc CvtSetting::CvtSetting {oop} {

	return
}

proc CvtSetting::Info {oop info args} {

	global CvtSetting

	# Set info
	if {[llength $args]} {
		set CvtSetting($oop,$info) [lindex $args 0]

	# Get info
	} else {
		return $CvtSetting($oop,$info)
	}

	return
}

proc CvtSetting::Read {oop} {

	variable DataSrc
	
	set path [PathSrc tk config setting]
	Info $oop exists [file exists $path]
	if {![Info $oop exists]} {
		LogMessage "$path\n  skipping (missing)"
		return
	}

	set verSrc $NSVersionDataSrc::vSettingVersion

	array unset DataSrc $oop,*

	namespace eval Version$verSrc "
		_Init $oop
		source {[PathSrc tk config setting]}
	"

	return
}

proc CvtSetting::ReadDst {oop} {

	variable DataSrc
	variable DataDst
	
# FIXME: Check for existence

	set verDst $NSVersionDataDst::vSettingVersion

	array unset DataSrc $oop,*

	namespace eval Version$verDst "
		_Init $oop
		source {[PathDst tk config setting]}
	"

	array set DataDst [array get DataSrc $oop,*]

	return
}

proc CvtSetting::Convert {oop} {

	variable DataSrc
	variable DataDst

	if {![Info $oop exists]} return

	set verSrc $NSVersionDataSrc::vSettingVersion
	set verDst $NSVersionDataDst::vSettingVersion

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

proc CvtSetting::Convert1To2 {oop} {

	variable DataSrc
	variable DataDst

	return
}

proc CvtSetting::Update {oop} {

	variable DataDst

	if {![Info $oop exists]} return

	set verSrc $NSVersionDataSrc::vSettingVersion
	set verDst $NSVersionDataDst::vSettingVersion

	set upSrc $NSVersionDataSrc::vSettingUpdate
	set upDst $NSVersionDataDst::vSettingUpdate

	LogMessage "Setting: Update version $verSrc.$upSrc to $verDst.$upDst"

	# Get a list of value keywords in the dest version
	set settingDst $NSVersionDataDst::vSetting

	# Remove obsolete settings
	set names {}
	foreach name $DataDst($oop,list,name) {
		if {[lsearch -exact $settingDst $name] == -1} {
			array unset DataDst $oop,value,$name
			LogMessage "skipping outdated setting \"$name\""
			continue
		}
		lappend names $name
	}
	set DataDst($oop,list,name) $names

	Update$verDst $oop

	return
}

proc CvtSetting::Update1 {oop} {

	return
}

proc CvtSetting::Update2 {oop} {

	variable DataDst

	set upSrc $NSVersionDataSrc::vSettingUpdate
	set upDst $NSVersionDataDst::vSettingUpdate

	# Update 1: easy_disarm -> easy_alter
	if {$upSrc < 1} {
		set name easy_disarm
		if {[info exists DataDst($oop,value,$name)]} {
			set DataDst($oop,value,easy_alter) $DataDst($oop,value,$name)
			array unset DataDst $oop,value,$name
			set i [lsearch -exact $DataDst($oop,list,name) $name]
			set DataDst($oop,list,name) [lreplace $DataDst($oop,list,name) $i $i easy_alter]
		}
	}

	# Update 2: !auto_destroy -> verify_destroy_junk
	if {$upSrc < 2} {
		set name auto_destroy
		if {[info exists DataDst($oop,value,$name)]} {
			set DataDst($oop,value,verify_destroy_junk) [expr !$DataDst($oop,value,$name)]
			array unset DataDst $oop,value,$name
			set i [lsearch -exact $DataDst($oop,list,name) $name]
			set DataDst($oop,list,name) [lreplace $DataDst($oop,list,name) $i $i verify_destroy_junk]
		}
	}

	return
}

proc CvtSetting::Write {oop} {

	if {![Info $oop exists]} return

	set verDst $NSVersionDataDst::vSettingVersion

	set buf [Write$verDst $oop]
	StringToFile [PathDst tk config setting] $buf

	return
}

proc CvtSetting::Write1 {oop} {

	variable DataDst

	set buf ""
	foreach name $DataDst($oop,list,name) {
		append buf [list setg::one $name $DataDst($oop,value,$name)] \n
	}
	return $buf
}

proc CvtSetting::Write2 {oop} {

	variable DataDst

	set buf ""
	foreach name $DataDst($oop,list,name) {
		append buf [list One $name $DataDst($oop,value,$name)] \n
	}
	return $buf
}

#
# Version 1
#
namespace eval CvtSetting::Version1 {
proc _Init {oop} {
	variable ::CvtSetting::DataSrc
	set DataSrc(oop) $oop
	set DataSrc($oop,list,name) {}
	return
}
namespace eval setg {
}
proc setg::one {name value} {
	variable ::CvtSetting::DataSrc
	set oop $DataSrc(oop)
	set DataSrc($oop,value,$name) $value
	lappend DataSrc($oop,list,name) $name
	return
}
# CvtSetting::Version1
}

#
# Version 2
#
namespace eval CvtSetting::Version2 {
proc _Init {oop} {
	variable ::CvtSetting::DataSrc
	set DataSrc(oop) $oop
	set DataSrc($oop,list,name) {}
	return
}
proc One {name value} {
	variable ::CvtSetting::DataSrc
	set oop $DataSrc(oop)
	set DataSrc($oop,value,$name) $value
	lappend DataSrc($oop,list,name) $name
	return
}
# CvtSetting::Version2
}

