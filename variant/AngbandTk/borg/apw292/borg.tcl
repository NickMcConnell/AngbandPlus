# File: borg.tcl

# Purpose: author-specific Borg variables and commands

#
# Copyright (c) 1997-2000 Tim Baker
#
# This software may be copied and distributed for educational, research, and
# not for profit purposes provided that this copyright and statement are
# included in all such copies.
#

namespace eval Borg {

# List of setting keywords handled by this Borg
set vSetting [list \
	auto_borg_message \
	auto_flag_save \
	auto_respawn_winners \
	auto_stop_king \
	borg_plays_risky \
	borg_scums_uniques \
	borg_slow_optimizehome \
	borg_uses_calcs \
	borg_uses_swaps \
	borg_worships_damage \
	borg_worships_speed \
	borg_worships_hp \
	borg_worships_mana \
	borg_worships_ac \
	borg_worships_gold
]
# FIXME: this option is disabled for now...
set i [lsearch -exact $vSetting borg_slow_optimizehome]
set vSetting [lreplace $vSetting $i $i]

# Help text for each setting
set vSettingHelp(auto_borg_message) \
"Turn this option on to memorize and log Borg messages."

set vSettingHelp(auto_flag_save) \
"Turn this option on to automatically save the game whenever the Borg\
enters a new dungeon."

set vSettingHelp(auto_respawn_winners) \
"After killing Morgoth, generally, the borg will stop and allow you\
to inspect the victory.  If you set auto_respawn_winners, then after\
killing Morgoth the borg will generate a new character.  A map file\
is created so you can see what the final battle looked like."

set vSettingHelp(auto_stop_king) \
"Stops the borg as soon as he wins the game."

set vSettingHelp(borg_plays_risky) \
"A risky borg is one who is not confined by character level requirements\
in order to dive deeper.  It is also more likely to stay in a battle than\
to teleport out.  A risky borg will dive faster but is more likely to die."

set vSettingHelp(borg_scums_uniques) \
"A borg who scums for uniques will set the auto_scum flag in order to generate\
more exciting levels and hopefully encounter a unique."

set vSettingHelp(borg_slow_optimizehome) \
"The borg has two routines to store items in the home.  One is very\
effective and will make the best possible choice for adding an item to the\
home.  Unfortunately, this function is very slow.  A much faster routine\
will do a fine job at storing items in the home but it is not as\
efficient."

set vSettingHelp(borg_uses_calcs) \
""

set vSettingHelp(borg_uses_swaps) \
"The borg is designed to use Swap Items in the inventory.  A swap item\
is an equipment item (like a sword or armour item) which posseses a\
desireable resistance or trait.  If the borg is in danger, then he will\
consider exchanging his current item for the swap item.  A prime example of\
this is at dungeon level 60.  He is required to have several resists in\
place.  If he does not have them then he will not dive deeper.  However,\
with Swaps enabled he will select an item to cover the open resist then\
dive down.  Having Swaps enabled allows the borg to dive deeper, faster.\
The draw back is the Swap Items take up two inventory slots."

set vSettingHelp(borg_worships_damage) \
""

set vSettingHelp(borg_worships_speed) \
""

set vSettingHelp(borg_worships_hp) \
""

set vSettingHelp(borg_worships_mana) \
""

set vSettingHelp(borg_worships_ac) \
""

set vSettingHelp(borg_worships_gold) \
""

# namespace eval Borg
}

# Change the value of a setting
proc Borg::SettingSet {keyword value} {

	borg setting set $keyword $value
}

# Return the value of a setting
proc Borg::SettingGet {keyword} {

	return [borg setting set $keyword]
}

# Return the brief description of a setting
proc Borg::SettingDesc {keyword} {

	return [borg setting desc $keyword]
}


