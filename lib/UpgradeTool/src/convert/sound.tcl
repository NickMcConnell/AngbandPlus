# File: sound.tcl

# Purpose: converter for VARIANT/tk/config/sound

#
# Copyright (c) 1997-2001 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval CvtSound {

# namespace eval CvtSound
}

proc CvtSound::InitModule {} {

	return
}

proc CvtSound::CloseModule {} {

	return
}

proc CvtSound::CvtSound {oop} {

	return
}

proc CvtSound::Info {oop info args} {

	global CvtSound

	# Set info
	if {[llength $args]} {
		set CvtSound($oop,$info) [lindex $args 0]

	# Get info
	} else {
		return $CvtSound($oop,$info)
	}

	return
}

proc CvtSound::Read {oop} {

	variable DataSrc

	set path [PathSrc tk config sound]
	Info $oop exists [file exists $path]
	if {![Info $oop exists]} {
		LogMessage "$path\n  skipping (missing)"
		return
	}

	set verSrc $NSVersionDataSrc::vSoundVersion
	set verDst $NSVersionDataDst::vSoundVersion

	if {$verSrc == $verDst} {
		# Will copy later
		return
	}

	# Version 1 & 2: A single tk/config/sound file
	if {$verSrc < 3} {
		set path [PathSrc tk config sound]
		set cvtId [NSObject::New CvtSoundCfg]
		CvtSoundCfg::Read $cvtId $path
		Info $oop cvtId $cvtId
		return
	}

	array unset DataSrc $oop,*

	# Version 3+: tk/config/sound and zero or more tk/config/*.snd
	namespace eval Version$verSrc "
		_Init $oop
		source {[PathSrc tk config sound]}
	"

	return
}

proc CvtSound::Convert {oop} {

	variable DataDst

	if {![Info $oop exists]} return

	set verSrc $NSVersionDataSrc::vSoundVersion
	set verDst $NSVersionDataDst::vSoundVersion

	array unset DataDst $oop,*

	for {set ver $verSrc} {$ver < $verDst} {incr ver} {
		Convert${ver}To[expr $ver + 1] $oop
	}

	return
}

proc CvtSound::Convert2To3 {oop} {

	variable DataSrc
	variable DataDst

	# Version 2: tk/config/sound
	# Version 3: tk/config/imported.snd
	set cvtId [Info $oop cvtId]
	CvtSoundCfg::Convert $cvtId

	return
}

proc CvtSound::Update {oop} {

	variable DataDst

	if {![Info $oop exists]} return

	set verSrc $NSVersionDataSrc::vSoundVersion
	set verDst $NSVersionDataDst::vSoundVersion

	for {set ver $verSrc} {$ver < $verDst} {incr ver} {
		Update${ver}To[expr $ver + 1] $oop
	}

	return
}

proc CvtSound::Update2To3 {oop} {

	# Version 2: tk/config/sound
	# Version 3: tk/config/imported.snd
	set cvtId [Info $oop cvtId]
	CvtSoundCfg::Update $cvtId

	return
}

proc CvtSound::Write {oop} {

	if {![Info $oop exists]} return

	set verSrc $NSVersionDataSrc::vSoundVersion
	set verDst $NSVersionDataDst::vSoundVersion

	if {$verSrc == $verDst} {

		# Version 1 & 2: A single tk/config/sound file
		if {$verDst < 3} {
			SynchFile [file join tk config sound] 1
			return
		}

		# Version 3+: tk/config/sound and zero or more tk/config/*.snd
		SynchDirectory {tk config} {} {sound *.snd}
		
		return
	}

	# Version 1 & 2: A single tk/config/sound file
	if {$verDst < 3} {
		set cvtId [Info $oop cvtId]
		CvtSoundCfg::Write $cvtId [PathDst tk config sound]
		return
	}

	# Version 3+: tk/config/sound and zero or more tk/config/*.snd
	if {$verSrc < 3} {

		# Step 1: Create new tk/config/sound
		set buf [list Config imported.snd 1]\n
		StringToFile [PathDst tk config sound] $buf
	
		# Step 2: Create tk/config/imported.snd
		set cvtId [Info $oop cvtId]
		CvtSoundCfg::Write $cvtId [PathDst tk config imported.snd]
		NSObject::Delete CvtSoundCfg $cvtId
		return
	}

	# Hypothetical Version 4 -- convert each file

	# Step 1: Write single tk/config/sound
	set buf [namespace eval Version$verSrc _Get$verDst]
	StringToFile [PathDst tk config sound] $buf

	# Step 2: Write multiple tk/config/*.snd
	set configList [namespace eval Version$verSrc _GetCfgList]
	foreach {cvtId file} $configList {
		CvtSoundCfg::Write $cvtId [PathDst tk config $file]
		NSObject::Delete CvtSoundCfg $cvtId
	}
	
	return
}

# Note: Version 1 and 2 are handled by CvtSoundCfg

#
# Version 3
#
namespace eval CvtSound::Version3 {
	proc _Init {oop} {
		variable ::CvtSound::DataSrc
		set DataSrc(oop) $oop
		set DataSrc($oop,config,list) {}
		return
	}
	proc Config {file active} {
		variable ::CvtSound::DataSrc
		set cvtId [NSObject::New CvtSoundCfg]
		CvtSoundCfg::Read $cvtId [PathSrc tk config $file]
		set DataSrc($oop,file,$cvtId) $file
		set DataSrc($oop,active,$cvtId) $active
		lappend DataSrc(config,list) $cvtId
		return
	}
}
