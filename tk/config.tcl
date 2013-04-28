# File: config.tcl

# Purpose: icon configuration management

#
# Copyright (c) 1997-2009 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval NSConfig {

	variable Priv

# namespace eval NSConfig
}

# NSConfig::InitModule --
#
#	One-time-only-ever initialization.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::InitModule {} {

	# Read tk/config, which contains a list of icon configurations
	Config::Config::Source [PathTk config config]

	# Set the default set of files to pass to SourceOne. These
	# can be overridden by scripts to use common configuration
	# files. See ShareConfigFile() below.
	SetPrefix [Value config,prefix]

	# Generate this event when the current icon configuration changes
	qeinstall <IconCfg>

	return
}

# NSConfig::Load --
#
#	Processes the set of files for the current icon configuration.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::Load {{again 0}} {

	variable Priv

	# Get the current configuration prefix
	set prefix [Global config,prefix]

	# Load icon configuration
	SourceOne $Priv(file,$prefix) 1
	IconCfg::Load {*}$Priv(args,$prefix)

	# These next files are automatically generated at shutdown
	Source [Global config,alternate] Config::Alternate
	Source [Global config,sprite] Config::Sprite
	Source [Global config,assign] Config::Assign

	# Create a vault for towns/quests
	Global vault,current [vault create]

	# Try "prefix-town", custom town layout and appearance
#	ReadTownFile [Global config,town]

	# Read the binary town.vlt file, if it exists.
	### (This was created by calling ReadTownFile() and then
	### "vault write [vault town] tk/config/town.vlt".)
	ReadTownVault

	# Custom icon assignments for each character
	Source char-icon Config::CharIcon
#	charicon::Assign

	IconCfg::PostOp

	if {!$again} {
		# Map symbols
		angband_load note "Initializing map..."
		SourceOne config-map 1
	}

#Colormapify

	return
}

# NSConfig::SwitchConfig --
#
#	Unload the current icon configuration, and load another.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::SwitchConfig {prefixNew {force 0}} {

	variable Priv

	set prefixOld [Global config,prefix]
	if {($prefixOld eq $prefixNew) && !$force} return
	if {[lsearch -exact $Priv(prefix) $prefixNew] == -1} {
		error "no such icon configuration \"$prefixNew\""
	}

	IconCfg::Unload
	qeunbind IconCfg
	namespace delete ::IconCfg

	SetPrefix $prefixNew
	Load 1

	qegenerate <IconCfg> {} "::TreeCtrl::PercentsCmd XXX"

	return
}

# NSConfig::InitIcons --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::InitIcons {iconWidth iconHeight style} {

	if {[Platform unix]} {
		set depth [winfo depth .]
	}
	if {[Platform windows]} {
		set depth [winfo depth .]
		if {$depth != 8} {
			set depth 16
		}
	}
	angband init_icons $iconWidth $iconHeight $depth $style

	return
}

# NSConfig::SetPrefix --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::SetPrefix {prefix} {

	Value config,prefix $prefix

	Global config,prefix $prefix
	Global config,alternate $prefix-alternate
	Global config,sprite $prefix-sprite
	Global config,assign $prefix-assign
	Global config,town $prefix-town

	return
}

# NSConfig::ShareConfigFile --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::ShareConfigFile {which file} {

	switch -- $which {
		alternate -
		sprite -
		assign -
		town {
			Global config,$which $file
		}

		default {
			error "unknown config file \"$which\""
		}
	}

	return
}

# NSConfig::SourceOne --
#
#	Looks for the given file in the tk/config directory. If it
#	exists, it is sourced at the global level. This command is
#	usually called from a icon configuration file, type ".cfg".
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::SourceOne {fileName {required 0}} {

	set fileName [file tail $fileName]
	set path [PathTk config $fileName]
	if {[file exists $path]} {
dbwin "NSConfig::SourceOne $path\n"
		uplevel #0 [list source $path]
		return
	}
	set path [CPathTk config $fileName]
	if {[file exists $path]} {
dbwin "NSConfig::SourceOne $path\n"
		uplevel #0 [list source $path]
		return
	}
	if {$required} {
		error "can't find file \"$fileName\""
	} else {
dbwin "WARNING: NSConfig::SourceOne: $fileName NOT FOUND\n"
	}

	return
}

# NSConfig::Source --
#
#	Looks for the given file in the tk/config directory. If it
#	exists, it is sourced in the given namespace. This command is
#	usually called from a icon configuration file, type ".cfg".
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::Source {fileName namespace} {

	set fileName [file tail $fileName]
	set fileName [PathTk config $fileName]
	if {[file exists $fileName]} {
		${namespace}::Source $fileName
	}

	return
}

# NSConfig::FileLibData --
#
#	Takes the "tail" of the given file name, and appends it to the
#	complete pathname of the lib/data directory.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::FileLibData {file} {

	set file [file tail $file]
	return [Path lib data $file]
}

# NSConfig::FindImage --
#
#	Find an image file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::FindImageFile {imageFile} {

	set path [PathTk image $imageFile]
	if {[file exists $path]} {
		return $path
	}
	set path [PathTk image dg [file tail $imageFile]]
	if {[file exists $path]} {
		return $path
	}
	set path [CPathTk image $imageFile]
	if {[file exists $path]} {
		return $path
	}
	set path [CPathTk image dg [file tail $imageFile]]
	if {[file exists $path]} {
		return $path
	}
	error "icon image file \"$imageFile\" was not found"

	return
}

# NSConfig::CheckIconData --
#
#	Creates an icon data file in the lib/data directory if it does
#	not already exist. If it exists but is older than the given
#	image file, it is overwritten.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::CheckIconData {fileImage fileData imageWidth imageHeight iconWidth iconHeight makeMask} {

	set force 0

dbwin "check_icon_data $fileImage $fileData\n"

	# The image file is required
	set fileImage [FindImageFile $fileImage]

	# The icon file may or may not exist
	set fileData [FileLibData $fileData]
	if {!$force && [file exists $fileData]} {
		set mtime [file mtime $fileImage]
		set mtime2 [file mtime $fileData]
		if {$mtime < $mtime2} return
	}

dbwin "makeicon \"$fileImage\" \"$fileData\"\n"

	if {[winfo exists $::AngbandPriv(load,win)]} {
		# Tell the user why she's waiting
		angband_load prompt "Writing icon file \"[file tail $fileData]...\""
	}

	# Build a command
	set command [list icon makeicon -iconwidth $iconWidth -iconheight $iconHeight -imagefile $fileImage -datafile $fileData]
	if {$imageWidth != $iconWidth} {
		append command " -imagewidth $imageWidth"
	}
	if {$imageHeight != $iconHeight} {
		append command " -imageheight $imageHeight"
	}
	if {$makeMask} {
		append command " -transparent white"
	}

	eval $command

	return
}

# NSConfig::CreateIconType --
#
#	Creates a new kind of icon from the given icon-image file
#	and optional mask-image file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::CreateIconType {iconType args} {

	array set cfg {
		-file ""
		-imagewidth 0
		-imageheight 0
		-imagesize 0
		-datafile ""
		-rle ""
	}
	set cfg(-iconwidth) [icon width]
	set cfg(-iconheight) [icon height]
	foreach {opt val} $args {
		if {![info exists cfg($opt)]} {
			error "unknown option \"$opt\""
		}
	}
	array set cfg $args

	set fileImage $cfg(-file)
	set imageWidth $cfg(-imagewidth)
	set imageHeight $cfg(-imageheight)

	if {$cfg(-imagesize)} {
		set imageWidth $cfg(-imagesize)
		set imageHeight $cfg(-imagesize)
	}

	if {[winfo exists $::AngbandPriv(load,win)]} {
		# Hack -- Tell user what is happening
		set canvas $::AngbandPriv(load,win).canvas
		$canvas itemconfigure message -text "Initializing icons: $iconType"
		update
	}

	set depth [icon depth]

	if {$cfg(-datafile) ne ""} {
		set fileData [file tail $cfg(-datafile)]
	} else {
		# dragon16.gif --> dragon16.$depth.icd
		set fileImage [file tail $fileImage]
		set fileData [file rootname $fileImage].$depth.icd

		# dragon16.$depth.icd -> dragon32.$depth.icd
		if {$imageWidth != $cfg(-iconwidth)} {
			regsub $imageWidth $fileData $cfg(-iconwidth) fileData
		}
	}

	# Try to read the icon data file twice.  If [icon createtype fails] it may
	# be because the screen pixel-format is different (at the same bit-depth).
	for {set attempt 0} {$attempt < 2} {incr attempt} {

		CheckIconData $fileImage $fileData $imageWidth $imageHeight $cfg(-iconwidth) $cfg(-iconheight) 0
		set fileData [FileLibData $fileData]

		# Now create the icon type
		if {[catch {
			icon createtype $iconType -file $fileData
			set attempt 1
		} result]} {

			# Sometimes I get errors from gzclose() when decompressing
			# a file. In these cases I will delete the file just in
			# case it was corrupted.
			set error $::errorInfo
			file delete $fileData

			if {$attempt} {
				# Propagate the error
				error $error
			}
		}
	}

	if {$cfg(-rle) ne ""} {
		icon rle $iconType $cfg(-rle)
	}

	return
}

# NSConfig::Maskify --
#
#	After the masks data is loaded, and any mask-assignments are done
#	(with the "icon mask" command), we must fiddle with the icon data
#	so it does not conflict with the mask data.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::Maskify {iconType} {

	icon rle $iconType White

	return
}

# NSConfig::NoMoreIcons --
#
#	This is a big silly hack called when all the icon types have
#	been created. It is used just so I can update the progress bar
#	during startup.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::NoMoreIcons {} {

	global AngbandPriv

	if {[winfo exists $::AngbandPriv(load,win)]} {
		set canvas $AngbandPriv(load,win).canvas
		$canvas itemconfigure message -text "Assigning icons..."
		update
	}

	return
}

# NSConfig::Colormapify --
#
#	After all the icon types have been created, the masks assigned,
#	and Maskify has been called where needed, then convert from the
#	palette-indexed data to colormap-indexed data.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

#UNUSED
proc NSConfig::Colormapify {} {

	foreach type [icon gettypes] {
		switch -- $type {
			blank -
			default -
			none {
			}
			default {
				if {[icon ascii isascii $type]} continue
				icon colormapify $type
			}
		}
	}

	return
}

# NSConfig::ReadTownFile --
#
#	Looks for the given file in the tk/config directory. If it
#	exists, it is parsed line by line.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::ReadTownFile {fileName} {

	set fileName [file tail $fileName]
	set fileName [PathTk config $fileName]

	# No problem if the file doesn't exist
	if {![file exists $fileName]} return

	# ZAngbandTk: only if vanilla_town is set
	if {[variant ZANGBANDTK] && ![angband setting set vanilla_town]} return

	# Hack -- Startup progress
	angband_load prompt "Reading town file..."

	if {[catch {open $fileName} fileId]} {
		set msg "The following error occurred while attempting to open "
		append msg "the \"town\" file for reading:\n\n$fileId"
		tk_messageBox -title Oops -message $msg
		return
	}

	set hasTown 0
	set symbol ""
	while {![eof $fileId]} {
		set count [gets $fileId lineBuf]
		if {$count == -1} break

		if {!$hasTown} {
			if {!$count} {
				set hasTown 1
				continue
			}
			lappend town $lineBuf
			continue
		}

		if {![string length $symbol]} {
			set symbol [lindex $lineBuf 1]
			set feature ""
			set curBlock ""
			set blockFeature ""
			continue
		}
		if {![string length $feature]} {
			set feature [lindex $lineBuf 1]
			continue
		}
		if {!$count} {
			set block(icon,$symbol) $curBlock
			set block(feature,$symbol) $blockFeature
			set symbol ""
			continue
		}
		set rowIcon ""
		set rowFeature ""
		foreach index [split $lineBuf] {
			if {[string index $index 0] eq "0"} {
				set index [string index $index 1]
			}
			if {$index != -1} {
				lappend rowIcon "town $index"
				lappend rowFeature $feature
			} else {
				lappend rowIcon "town 81"
				lappend rowFeature 0x01
			}
		}
		lappend curBlock $rowIcon
		lappend blockFeature $rowFeature
	}

	close $fileId

	# Get the height and width
	set height [llength $town]
	set width [string length [lindex $town 0]]

	# Create a vault to hold the town
	set vaultId [vault create -height $height -width $width]

	# Scan each grid in the town
	set icon [set feature ""]
	for {set y 0} {$y < $height} {incr y} {
		set rowIcon ""
		set rowFeature ""
		for {set x 0} {$x < $width} {incr x} {
			lappend rowIcon {town 81}
			lappend rowFeature 0x01
		}
		lappend icon $rowIcon
		lappend feature $rowFeature
	}

	# Fill town with type dirt icon and floor feature
	vault put $vaultId icon $icon
	vault put $vaultId iconbackground $icon
	vault put $vaultId feature $feature

	# Note that the custom town is not used by either KAngbandTk or
	# ZAngbandTk. Now that ZAngband has a "vanilla_town" option I
	# may add this custom town back to ZAngbandTk.
	
	if {[variant ANGBANDTK KANGBANDTK]} {
		set doorChar "12345678"
		set doorBase 0x08
	}
	if {[variant OANGBANDTK]} {
		set doorChar "123456789"
		set doorBase 0x40
	}
	if {[variant ZANGBANDTK]} {
		set doorChar "123456789"
		set doorBase 0x4A
	}

	set y 0
	foreach row $town {
		for {set x 0} {$x < $width} {incr x} {
			set char [string index $row $x]
			if {[info exists block(icon,$char)]} {
				vault put $vaultId icon $block(icon,$char) $y $x
				vault put $vaultId iconbackground $block(icon,$char) $y $x
				vault put $vaultId feature $block(feature,$char) $y $x
				continue
			}

			# Store doors
			set index [string first $char $doorChar]
			if {$index != -1} {
				vault put $vaultId icon [list [list {none 0}]] $y $x
				vault put $vaultId iconbackground [list [list {none 0}]] $y $x
				vault put $vaultId feature [list [expr {$index + $doorBase}]] $y $x
			}
		}
		incr y
	}

	# Set this as the town vault
	vault town $vaultId

	return
}

# NSConfig::ReadTownVault --
#
#	Description.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc NSConfig::ReadTownVault {} {

	global Angband
	global errorInfo

	# Vault ID of unique town vault
	Global vault,town 0

	# ZAngbandTk: only if vanilla_town is set. The vaults for the other
	# towns are read by the ".cfg" file.
	if {[variant ZANGBANDTK] && ![angband setting set vanilla_town]} return

	if {[variant ANGBANDTK OANGBANDTK ZANGBANDTK]} {
		if {[Global config,prefix] eq "dg32+iso"} return
		set name town.vlt
	}
	if {[variant KANGBANDTK]} {
		set prefix [Global config,prefix]
		if {[string match *dg32* $prefix]} { set prefix dg32 }
		set plotNum [struct set player_type 0 plot_num]
		set name [format t%07d $plotNum]-$prefix.vlt
	}
	set path [PathTk config $name]
	if {[file exists $path]} {
		if {[catch {
			Global vault,town [vault create -file $path]
		} result]} {

			set stack $errorInfo

			# Some people get an error. Note the problem and fail silently.
			if {[catch {open [Path errors.txt] a} fileId]} {
				if {$::DEBUG} {
					tk_messageBox -icon error -title Error \
						-message "Couldn't open [Path errors.txt]\n$fileId"
				}
			} else {
				catch {
					puts $fileId "***** $Angband(name) $Angband(vers)"
					puts $fileId $stack
					puts $fileId ""
				}
				close $fileId
			}
		}
	}

	return
}

# Config::Config --
#
#	A namespace with commands called when the tk/config/config file
#	is sourced.

namespace eval Config::Config {

	variable Priv

# namespace eval Config::Config
}

# Evaluate a script
proc Config::Config::Source {path} {

	source $path

	return
}

proc Config::Config::Config {prefix file args} {

	lappend NSConfig::Priv(prefix) $prefix
	set NSConfig::Priv(file,$prefix) $file
	set NSConfig::Priv(args,$prefix) $args

	return
}

# Alternate --
#
#	A namespace with commands called when the tk/config/alternate file
#	is sourced.
#

namespace eval Config::Alternate {

	variable Priv

# namespace eval Config::Alternate
}

proc Config::Alternate::Source {path} {

	variable Priv

	array unset Priv type
	source $path

	return
}

proc Config::Alternate::Type {type} {

	variable Priv

	lappend Priv(type) $type

	return
}

proc Config::Alternate::New {reason} {

	variable Priv

	set Priv(id) [alternate create $reason]

	return
}

proc Config::Alternate::Ins {typeIndex iconIndex} {

	variable Priv

	set type [lindex $Priv(type) $typeIndex]
	alternate insert $Priv(id) 1000 -type $type -index $iconIndex

	return
}

# Config::Alternate::Write --
#
#	Writes the tk/config/$prefix-alternate file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Config::Alternate::Write {} {

	set tempName [NSUtils::TempFileName [PathTk config]]
	if {[catch {openlf $tempName} fileId]} {
		set msg "The following error occurred while attempting to open "
		append msg "the \"alternate\" file for writing:\n\n$fileId"
		tk_messageBox -title Oops -message $msg
		return
	}

	puts $fileId "# Automatically generated. Do not edit.\n"

	set max [alternate count]
	if {$max} {
		set typeList [icon gettypes]
		set index 0
		foreach type $typeList {
			puts $fileId "Type $type"
			set type2index($type) $index
			incr index
		}
		puts $fileId ""
	
		for {set i 0} {$i < $max} {incr i} {
			puts $fileId "New [alternate configure $i -reason]"
			foreach frame [alternate get $i] {
				scan $frame "%s %n" iconType n
				set extra [string range $frame $n end]
				puts $fileId "Ins $type2index($iconType) $extra"
			}
			puts $fileId ""
		}
	}

	close $fileId

	set fileName [NSUtils::ReadLink [PathTk config [Global config,alternate]]]
	if {[file exists $fileName]} {
		file rename -force -- $fileName $fileName.bak
	}
	file rename -- $tempName $fileName

	return
}

# Config::Assign --
#
#	A namespace with commands called when the tk/config/assign file
#	is sourced.
#

namespace eval Config::Assign {

	variable Priv

#namespace eval Config::Assign
}

# Evaluate a script
proc Config::Assign::Source {path} {

	variable Priv

	array unset Priv assignType
	array unset Priv type
	source $path
if 0 {
	# Debug
	foreach group [assign groups] {
		set count [assign count $group]
		for {set i 0} {$i < $count} {incr i} {
			set assign [assign set $group $i]
			if {$assign eq "icon default 0"} {
				Debug "$group $i $assign"
			}
		}
	}
}
	return
}

# Add an assign type
proc Config::Assign::Assign {type} {

	variable Priv

	lappend Priv(assignType) $type

	return
}

# Add an icon type
proc Config::Assign::Type {type} {

	variable Priv

	lappend Priv(type) $type

	return
}

# Start assigning to this group
proc Config::Assign::Group {group} {

	variable Priv

	set Priv(group) $group

	return
}

proc Config::Assign::One {member assignTypeIndex args} {

	variable Priv

	set Priv(member) $member

	# Get the current assignment for this thing
	set assign [assign set $Priv(group) $member]
	scan $assign %s assignType
	
	### Hack -- Don't override "flavor" assignments
	switch -- $assignType {
		icon {
			scan $assign "%s %s" assignType iconType

			### Hack -- Don't override "none" assignments
			if {$iconType eq "none"} return
		}
		flavor {
			### Double-Hack -- Do override "flavor" assignments to insta-artifacts,
			### since they may have custom icons (ex Rings of Power).
			if {$Priv(group) eq "object"} {
				if {![angband k_info info $member insta_art]} return
			} else {
				return
			}
		}
	}

	# Get the type we are assigning
	set assignType [lindex $Priv(assignType) $assignTypeIndex]

	switch -- $assignType {
		alternate {
			set assign "alternate $args"
		}
		flavor {
			error "flavor assignments should not be saved"
		}
		icon {

			set typeIndex [lindex $args 0]
			set iconType [lindex $Priv(type) $typeIndex]

			### Hack -- Don't allow ascii assignments (they shouldn't be saved)
			if {[icon ascii isascii $iconType]} {
				return
			}

			set assign "icon $iconType [lrange $args 1 end]"
		}
		sprite {
			set assign "sprite $args"
		}
	}

	if {[catch {assign validate $assign}]} {
dbwin "assign validate failed: $assign\n"
		set assign "icon default 0"
	}

	assign set $Priv(group) $member $assign

	# If we just assigned an icon to the first character image, then
	# it means the user wants the same icon for every character.
	if {!$member && $Priv(group) eq "character"} {
		Global autoAssign 0
	}

	return
}

proc Config::Assign::Effect {effect typeIndex iconIndex} {

	variable Priv

	set iconType [lindex $Priv(type) $typeIndex]

	if {[catch {icon validate -type $iconType -index $iconIndex}]} {
		set iconType default
		set iconIndex 0
	}
	effect assign $Priv(group) $effect -type $iconType -index $iconIndex

	return
}

proc Config::Assign::Feat {light background} {

	variable Priv

	feature configure $Priv(member) -light $light -background $background

	return
}

# Config::Assign::Write --
#
#	Writes the tk/config/assign file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Config::Assign::Write {} {

	set tempName [NSUtils::TempFileName [PathTk config]]
	if {[catch {openlf $tempName} fileId]} {
		set msg "The following error occurred while attempting to open "
		append msg "the \"assign\" file for writing:\n\n$fileId"
		tk_messageBox -title Oops -message $msg
		return
	}
	
	puts $fileId "# Automatically generated. Do not edit.\n"

	# List of assign types
	set index 0
	foreach name [assign types] {
		puts $fileId "Assign $name"
		set ignoreAssign($name) 0
		set assign2index($name) $index
		incr index
	}
	puts $fileId ""

	# Don't save "flavor" assignments since they are different for each
	# character.
	set ignoreAssign(flavor) 1

	# List of icon types
	set index 0
	foreach type [icon gettypes] {
		puts $fileId "Type $type"
		set type2index($type) $index
		incr index
	}
	puts $fileId ""

	# Don't save ascii-type assignments, since the user can't change them.
	foreach iconType [icon gettypes] {
		if {[icon ascii isascii $iconType]} {
			set ignoreType($iconType) 1
		} else {
			set ignoreType($iconType) 0
		}
	}

	# Features
	set max [angband f_info max]
	puts $fileId "Group feature"
	for {set f_idx 1} {$f_idx < $max} {incr f_idx} {

		set assign [assign set feature $f_idx]
		scan $assign "%s %n" assignType n
		if {$ignoreAssign($assignType)} continue
		set assignIndex $assign2index($assignType)

		if {$assignType eq "icon"} {
			scan $assign "%s %s %n" assignType iconType n
			if {$ignoreType($iconType)} continue
			set typeIndex $type2index($iconType)
			set assignStr "$typeIndex [string range $assign $n end]"
		} else {
			set assignStr [string range $assign $n end]
		}
		puts $fileId "One $f_idx $assignIndex $assignStr"

		set light [feature configure $f_idx -light]
		set background [feature configure $f_idx -background]
		puts $fileId "Feat $light $background"
	}
	puts $fileId ""

	# Monsters
	set max [angband r_info max]
	puts $fileId "Group monster"
	for {set r_idx 1} {$r_idx < $max} {incr r_idx} {

		set assign [assign set monster $r_idx]
		scan $assign "%s %n" assignType n
		if {$ignoreAssign($assignType)} continue
		set assignIndex $assign2index($assignType)

		if {$assignType eq "icon"} {
			scan $assign "%s %s %n" assignType iconType n
			if {$ignoreType($iconType)} continue
			set typeIndex $type2index($iconType)
			set assignStr "$typeIndex [string range $assign $n end]"
		} else {
			set assignStr [string range $assign $n end]
		}
		puts $fileId "One $r_idx $assignIndex $assignStr"
	}
	puts $fileId ""

	# Objects
	set max [angband k_info max]
	puts $fileId "Group object"
	for {set k_idx 1} {$k_idx < $max} {incr k_idx} {

		set assign [assign set object $k_idx]
		scan $assign "%s %n" assignType n
		if {$ignoreAssign($assignType)} continue
		set assignIndex $assign2index($assignType)

		if {$assignType eq "icon"} {
			scan $assign "%s %s %n" assignType iconType n
			if {$ignoreType($iconType)} continue
			set typeIndex $type2index($iconType)
			set assignStr "$typeIndex [string range $assign $n end]"
		} else {
			set assignStr [string range $assign $n end]
		}
		puts $fileId "One $k_idx $assignIndex $assignStr"
	}
	puts $fileId ""

	# Effects (only assign-type "icon" can be assigned)
	foreach group [effect groups] {
		puts $fileId "Group $group"
		foreach effect [effect names $group] {
			set assign [effect assign $group $effect]
			scan $assign "%s %s %d" assignType iconType iconIndex
			if {$ignoreType($iconType)} continue
			set typeIndex $type2index($iconType)
			puts $fileId "Effect $effect $typeIndex $iconIndex"
		}
		puts $fileId ""
	}

	close $fileId

	set fileName [NSUtils::ReadLink [PathTk config [Global config,assign]]]
	if {[file exists $fileName]} {
		file rename -force -- $fileName $fileName.bak
	}
	file rename -- $tempName $fileName

	return
}

# Config::CharIcon --
#
#	A namespace with commands called when the tk/config/char-icon file
#	is sourced.

namespace eval Config::CharIcon {

	variable Priv

# namespace eval Config::CharIcon
}

# Evaluate a script
proc Config::CharIcon::Source {path} {

	variable Priv

	array unset Priv char*
	source $path

	Assign

	return
}

proc Config::CharIcon::Add {name prefix index icon} {

	variable Priv

	set Priv(icon,$name,$prefix,$index) $icon

	return
}

# If there is a user-specified icon assignment for the current character
# and configuration, assign it.
proc Config::CharIcon::Assign {} {

	variable Priv

	set name [angband player name]
	set prefix [Global config,prefix]
	set max [assign count character]
	
	for {set i 0} {$i < $max} {incr i} {
		if {[info exists Priv(icon,$name,$prefix,$i)]} {
			set assign $Priv(icon,$name,$prefix,$i)
			scan $assign "%s %n" assignType n
			set extra [string range $assign $n end]
			catch {
				assign set character $i $assign
				if {!$i} {
					Global autoAssign 0
				}
			}
		}
	}
	
	return
}

# When the character icon is automatically assigned, we must clear
# any user-specified icon assignment.
proc Config::CharIcon::AutoAssign {} {

	variable Priv

	set name [angband player name]
	set prefix [Global config,prefix]

	if {[info exists Priv(icon,$name,$prefix,0)]} {
		unset Priv(icon,$name,$prefix,0)
	}

	return
}

proc Config::CharIcon::Debug {} {

	variable Priv

	set name [angband player name]
	set prefix [Global config,prefix]

	set ignore ""
	if {[Global autoAssign]} {
		set ignore [assign set character 0]
	}
	set max [assign count character]
	for {set i 0} {$i < $max} {incr i} {
		set assign [assign set character $i]
		if {$assign ne $ignore} {
			set Priv(icon,$name,$prefix,$i) $assign
		} else {
			if {[info exists Priv(icon,$name,$prefix,$i)]} {
				unset Priv(icon,$name,$prefix,$i)
			}
		}
	}

	::Debug "\nBEGIN Config::CharIcon::Debug"
	set nameList [lsort [array names Priv icon,*]]
	foreach name $nameList {
		regexp "icon,(.*),(.*),(.*)" $name ignore charName prefix index
		set assign $Priv($name)
		::Debug "add \"$charName\" $prefix $index \"$assign\""
	}
	::Debug "\nEND Config::CharIcon::Debug"
	
	return
}

proc Config::CharIcon::Write {} {

	variable Priv

	set tempName [NSUtils::TempFileName [PathTk config]]
	if {[catch {openlf $tempName} fileId]} {
		set msg "The following error occurred while attempting to open "
		append msg "the \"char-icon\" file for writing:\n\n$fileId"
		tk_messageBox -title Oops -message $msg
		return
	}

	puts $fileId "# Automatically generated. Do not edit.\n"

	set name [angband player name]
	set prefix [Global config,prefix]

	# Hack -- If the extra "shape" assignments are the same as
	# the auto-assigned "normal" character icon, I do not save
	# them.
	set ignoreIcon ""

	if {[Global autoAssign]} {
		set ignoreIcon [assign set character 0]
	}
	set max [assign count character]
	for {set i 0} {$i < $max} {incr i} {
		set icon [assign set character $i]
		if {$icon ne $ignoreIcon} {
			set Priv(icon,$name,$prefix,$i) $icon
		} else {
			if {[info exists Priv(icon,$name,$prefix,$i)]} {
				unset Priv(icon,$name,$prefix,$i)
			}
		}
	}

	set nameList [lsort [array names Priv icon,*]]
	foreach name $nameList {
		regexp "icon,(.*),(.*),(.*)" $name ignore charName prefix index
		set icon $Priv($name)
		puts $fileId "Add \"$charName\" $prefix $index \"$icon\""
	}

	close $fileId

	set fileName [NSUtils::ReadLink [PathTk config char-icon]]
	if {[file exists $fileName]} {
		file rename -force -- $fileName $fileName.bak
	}
	file rename -- $tempName $fileName

	return
}

# Config::Feature --
#
#	A namespace with commands called when the tk/config/feature file
#	is sourced.
#

namespace eval Config::Feature {
}

# Evaluate a script
proc Config::Feature::Source {path} {

	source $path

	return
}

proc Config::Feature::Config {f_idx boring town} {

	feature configure $f_idx -boring $boring -town $town

	return
}

# Config::Feature::Write --
#
#	Writes the tk/config/feature file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Config::Feature::Write {} {

	set tempName [NSUtils::TempFileName [PathTk config]]
	if {[catch {openlf $tempName} fileId]} {
		set msg "The following error occurred while attempting to open "
		append msg "the \"feature\" file for writing:\n\n$fileId"
		tk_messageBox -title Oops -message $msg
		return
	}

	puts $fileId "# Automatically generated. Do not edit.\n"

	# Features
	set max [angband f_info max]
	for {set f_idx 1} {$f_idx < $max} {incr f_idx} {
		set string ""
		foreach option [list boring town] {
			append string " [feature configure $f_idx -$option]"
		}
		puts $fileId "Config $f_idx$string"
	}

	close $fileId

	set fileName [NSUtils::ReadLink [PathTk config feature]]
	if {[file exists $fileName]} {
		file rename -force -- $fileName $fileName.bak
	}
	file rename -- $tempName $fileName

	return
}

# Config::MsgColor --
#
#	A namespace with commands called when the tk/config/msgcolor file
#	is sourced.
#

namespace eval Config::MsgColor {

	variable Priv

# namespace eval Config::MsgColor
}

proc Config::MsgColor::Source {path} {

	source $path

	return
}

proc Config::MsgColor::Group {group} {

	variable Priv

	set Priv(group) $group

	return
}

proc Config::MsgColor::One {keyword attr} {

	variable Priv

	set group $Priv(group)

	# Ignore errors/incompatiblities
	catch {
		angband sound color $group $keyword $attr
	}

	return
}

# Config::MsgColor::Write --
#
#	Writes the tk/config/msgcolor file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Config::MsgColor::Write {} {

	set tempName [NSUtils::TempFileName [PathTk config]]
	if {[catch {openlf $tempName} chan]} {
		set msg "The following error occurred while attempting to open "
		append msg "the \"msgcolor\" file for writing:\n\n$chan"
		tk_messageBox -title Oops -message $msg
		return
	}

	puts $chan "# Automatically generated. Do not edit.\n"

	foreach group [angband sound groups] {
		if {$group eq "monster"} continue
		puts $chan "Group $group"
		set max [angband sound count $group]
		for {set i 0} {$i < $max} {incr i} {
			if {![angband sound exists $group $i]} continue
			set keyword [angband sound keyword $group $i]
			set attr [angband sound color $group $keyword]
			puts $chan [list One $keyword $attr]
		}
		puts $chan ""
	}

	close $chan

	set name msgcolor
	set fileName [NSUtils::ReadLink [PathTk config $name]]
	if {[file exists $fileName]} {
		file rename -force -- $fileName $fileName.bak
	}
	file rename -- $tempName $fileName

	return
}

# Config::Music --
#
#	A namespace with commands called when the tk/config/music file
#	is sourced.
#

namespace eval Config::Music {

	variable Priv

# namespace eval Config::Music
}

proc Config::Music::Source {path} {

	source $path

	return
}

proc Config::Music::Playlist {file active} {

	variable Priv

	if {[lindex $file 0] eq "Path"} {
		set file [eval $file]
	} elseif {[lindex $file 0] eq "CPath"} {
		set file [eval $file]
	} else {
		set file [file join {*}$file]
	}

	if {![file exists $file]} return

	set playlistId [playlist new -active $active -file $file]
	set Priv(playlistId) $playlistId

	source $file

	return
}

proc Config::Music::Title {title} {

	variable Priv

	playlist configure $Priv(playlistId) -title $title

	return
}

proc Config::Music::Location {args} {

	variable Priv

	playlist configure $Priv(playlistId) -location $args

	return
}

proc Config::Music::Level {min max} {

	variable Priv

	playlist configure $Priv(playlistId) -level [list $min $max]

	return
}

proc Config::Music::Song {list} {

	variable Priv

	if {[lindex $list 0] eq "Path"} {
		set path [eval $list]
	} elseif {[lindex $list 0] eq "CPath"} {
		set path [eval $list]
	} else {
		set path [file join {*}$list]
	}

	playlist append $Priv(playlistId) [list $path]

	return
}

# Config::Music::Write --
#
#	Writes the tk/config/music file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Config::Music::Write {} {

	# Didn't init music
	if {![Global music,avail]} return

	set tempName [NSUtils::TempFileName [PathTk config]]
	if {[catch {openlf $tempName} chan]} {
		set msg "The following error occurred while attempting to open "
		append msg "the \"music\" file for writing:\n\n$chan"
		tk_messageBox -title Oops -message $msg
		return
	}

	puts $chan "# Automatically generated. Do not edit.\n"

	foreach playlistId [playlist listof] {
		set active [playlist cget $playlistId -active]
		set file [playlist cget $playlistId -file]
		if {[IsFileInPath $file]} {
			set list [concat Path [StripCommon $file [Path]]]
		} elseif {[IsFileInCPath $file]} {
			set list [concat CPath [StripCommon $file [CPath]]]
		} else {
			set list [file split $file]
		}
		puts $chan [list Playlist $list $active]
	}

	close $chan

	set fileName [NSUtils::ReadLink [PathTk config music]]
	if {[file exists $fileName]} {
		file rename -force -- $fileName $fileName.bak
	}
	file rename -- $tempName $fileName

	foreach playlistId [playlist listof] {
		WritePlaylist $playlistId
	}

	return
}

# Config::Music::WritePlaylist --
#
#	Writes a music playlist file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Config::Music::WritePlaylist {playlistId} {

	set tempName [NSUtils::TempFileName [PathTk config]]
	if {[catch {openlf $tempName} chan]} {
		set msg "The following error occurred while attempting to open "
		append msg "a temporary file for writing:\n\n$chan"
		tk_messageBox -title Oops -message $msg
		return
	}

	puts $chan "# Automatically generated. Do not edit.\n"

	puts $chan [list Title [playlist cget $playlistId -title]]
	puts $chan "Location [playlist cget $playlistId -location]"
	puts $chan "Level [playlist cget $playlistId -level]"
	puts $chan ""

	foreach path [playlist songs $playlistId] {
		if {[IsFileInPath $path]} {
			set list [StripCommon $path [Path]]
			puts $chan [list Song [concat Path $list]]
		} elseif {[IsFileInCPath $path]} {
			set list [StripCommon $path [CPath]]
			puts $chan [list Song [concat CPath $list]]
		} else {
			puts $chan [list Song [file split $path]]
		}
	}
	puts $chan ""

	close $chan

	set path [playlist cget $playlistId -file]
	set fileName [NSUtils::ReadLink $path]
	if {[file exists $fileName]} {
		file rename -force -- $fileName $fileName.bak
	}
	file rename -- $tempName $fileName

	return
}

# Config::Sound --
#
#	A namespace with commands called when the tk/config/sound file
#	is sourced.
#

namespace eval Config::Sound {

	variable Priv

# namespace eval Config::Sound
}

proc Config::Sound::Source {path} {

	source $path

	return
}

proc Config::Sound::Config {file active} {

	variable Priv

	set config [angband sound config new -active $active -file $file]

	set Priv(config) $config
	set Priv(dir) {}

	set path [PathTk config $file]
	if {[file exists $path]} {
		source $path
	}

	return
}

# Like Config, but takes full path name
proc Config::Sound::ConfigHack {path active} {

	variable Priv

	set file [file tail $path]
	set config [angband sound config new -active $active -file $file]

	set Priv(config) $config
	set Priv(dir) {}

	if {[file exists $path]} {
		source $path
	}

	return
}

proc Config::Sound::Title {title} {

	variable Priv

	angband sound config configure $Priv(config) -title $title

	return
}

proc Config::Sound::Dir {list} {

	variable Priv

	if {[lindex $list 0] eq "Path"} {
		set path [eval $list]
	} elseif {[lindex $list 0] eq "CPath"} {
		set path [eval $list]
	} else {
		set path [file join {*}$list]
	}
	if {[file exists $path]} {
		set path [LongName $path]
	}
	set path [file nativename $path]

	# Ignore duplicate directories
	foreach dir [angband sound dir listof] {
		set path2 [angband sound dir cget $dir -path]
		if {$path eq $path2} {
			lappend Priv(dir) $dir
			return
		}
	}

	lappend Priv(dir) [angband sound dir new -path $path]

	return
}

proc Config::Sound::Group {group} {

	variable Priv

	set Priv(group) $group

	return
}

proc Config::Sound::One {keyword dir {sound ""}} {

	variable Priv

	# We allow assignment of "None" sounds so that an event
	# with multiple sounds assigned can sometimes play nothing.
#	if {![string length $sound]} return

	set config $Priv(config)
	set group $Priv(group)
	set dir [lindex $Priv(dir) $dir]

	# Ignore errors/incompatiblities
	catch {
		angband sound insert $config $group $keyword 1000 $dir $sound
	}

	return
}

# Config::Sound::Write --
#
#	Writes the tk/config/sound file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Config::Sound::Write {} {

	# No sounds were loaded
	if {![Value sound,wants]} return

	set tempName [NSUtils::TempFileName [PathTk config]]
	if {[catch {openlf $tempName} chan]} {
		set msg "The following error occurred while attempting to open "
		append msg "the \"sound\" file for writing:\n\n$chan"
		tk_messageBox -title Oops -message $msg
		return
	}

	puts $chan "# Automatically generated. Do not edit.\n"

	foreach config [angband sound config listof] {
		set active [angband sound config cget $config -active]
		set file [angband sound config cget $config -file]
		puts $chan [list Config $file $active]
	}

	close $chan

	set fileName [NSUtils::ReadLink [PathTk config sound]]
	if {[file exists $fileName]} {
		file rename -force -- $fileName $fileName.bak
	}
	file rename -- $tempName $fileName

	foreach config [angband sound config listof] {
		WriteConfig $config
	}

	return
}

# Config::Sound::WriteConfig --
#
#	Writes a sound configuration file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Config::Sound::WriteConfig {config} {

	set tempName [NSUtils::TempFileName [PathTk config]]
	if {[catch {openlf $tempName} chan]} {
		set msg "The following error occurred while attempting to open "
		append msg "the \"sound\" file for writing:\n\n$chan"
		tk_messageBox -title Oops -message $msg
		return
	}

	puts $chan "# Automatically generated. Do not edit.\n"

	puts $chan [list Title [angband sound config cget $config -title]]\n

	set dirList [angband sound dir listof]
	foreach dir $dirList {
		set path [angband sound dir cget $dir -path]
		if {[IsFileInPath $path]} {
			set list [StripCommon $path [Path]]
			puts $chan [list Dir [concat Path $list]]
		} elseif {[IsFileInCPath $path]} {
			set list [StripCommon $path [CPath]]
			puts $chan [list Dir [concat CPath $list]]
		} else {
			puts $chan [list Dir [file split $path]]
		}
	}
	puts $chan ""

	foreach group [angband sound groups] {
		puts $chan "Group $group"
		set max [angband sound count $group]
		for {set i 0} {$i < $max} {incr i} {
			if {![angband sound exists $group $i]} continue
			set keyword [angband sound keyword $group $i]
			foreach {dir snd} [angband sound assign $config $group $keyword] {
				set d [lsearch -exact $dirList $dir]
				puts $chan [list One $keyword $d $snd]
			}
		}
		puts $chan ""
	}

	close $chan

	set name [angband sound config cget $config -file]
	set fileName [NSUtils::ReadLink [PathTk config $name]]
	if {[file exists $fileName]} {
		file rename -force -- $fileName $fileName.bak
	}
	file rename -- $tempName $fileName

	return
}

# Config::Sprite --
#
#	A namespace with commands called when the tk/config/sprite file
#	is sourced.
#

namespace eval Config::Sprite {

	variable Priv

# namespace eval Config::Sprite
}

proc Config::Sprite::Source {path} {

	variable Priv

	array unset Priv type
	source $path

	return
}

proc Config::Sprite::Type {type} {

	variable Priv

	lappend Priv(type) $type

	return
}

proc Config::Sprite::New {delay reverse} {

	variable Priv

	set Priv(id) [sprite create]
	sprite configure $Priv(id) -delay $delay -reverse $reverse

	return
}

proc Config::Sprite::Ins {typeIndex iconIndex} {

	variable Priv

	set iconType [lindex $Priv(type) $typeIndex]
	if {[catch {icon validate -type $iconType -index $iconIndex}]} {
		set iconType default
		set iconIndex 0
	}
	sprite insert $Priv(id) 1000 -type $iconType -index $iconIndex

	return
}

# Config::Sprite::Write --
#
#	Writes the tk/config/sprite file.
#
# Arguments:
#	arg1					about arg1
#
# Results:
#	What happened.

proc Config::Sprite::Write {} {

	set tempName [NSUtils::TempFileName [PathTk config]]
	if {[catch {openlf $tempName} fileId]} {
		set msg "The following error occurred while attempting to open "
		append msg "the \"sprite\" file for writing:\n\n$fileId"
		tk_messageBox -title Oops -message $msg
		return
	}

	puts $fileId "# Automatically generated. Do not edit.\n"

	set max [sprite count]
	if {$max} {
		set typeList [icon gettypes]
		set index 0
		foreach type $typeList {
			puts $fileId "Type $type"
			set type2index($type) $index
			incr index
		}
		puts $fileId ""
	
		for {set i 0} {$i < $max} {incr i} {
			puts $fileId "New [sprite configure $i -delay] \
				[sprite configure $i -reverse]"
			foreach frame [sprite get $i] {
				scan $frame "%s %n" iconType n
				set extra [string range $frame $n end]
				puts $fileId "Ins $type2index($iconType) $extra"
			}
			puts $fileId ""
		}
	}

	close $fileId

	set fileName [NSUtils::ReadLink [PathTk config [Global config,sprite]]]
	if {[file exists $fileName]} {
		file rename -force -- $fileName $fileName.bak
	}
	file rename -- $tempName $fileName

	return
}

