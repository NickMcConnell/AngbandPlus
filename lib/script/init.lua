-- Boolean constants definitions
FALSE=false
TRUE=true

-- Some classes definitions.
CLASS_GUNNER=18
CLASS_KENSAI=21
CLASS_NIGHT1=40

-- Some elements definitions.
GF_ELEMENTAL=24
GF_VULNERABILITY=160
GF_RESTORE_MANA=52
GF_RESTORE_STATS=58
GF_RESTORE_STATUS=59
GF_RESTORE_LEVELS=60
GF_RESTORATION=61
GF_CURE_MUTATIONS=62
GF_STRENGTH=65
GF_HEROISM=66
GF_STAR_HEROISM=67
GF_NIGHTMARES=167
GF_RACIAL_CHAMPION=169
GF_SOUL_CRUSH=170
GF_DURATION=171
GF_INSPIRE_COURAGE=172

-- Some variables
pointblankshot = 0
dashingshot = 0
immolating = 0
potion_throw = 0
defensive_strike = 0
stealth_attack = 0
iajutsu = 0
lionroar = 0
scorptail = 0
magicitemscroll = 0
forcecurse = 0
donthurtmonsters = 0
crushingblows = 0
normalattack = 0
critmod = 0
music = 0
no_effect_friendly = 0
no_effect_hostile = 0
hexblaze = 0
paralyze_shot = 0
counter_shot = 0
rapid_shot = 0
storm_shot = 0
piercing_shot = 0
evil_slayer = 0

-- Redirect error messages to Angband's msg_print()
_ALERT = function(text)
	msg_print(text)
end

_TRACEBACK = function(text)
	msg_print(text)
	msg_print(debug.traceback())
end

-- Load the modules
script_do_file(angband.build_script_path("event.lua"))
script_do_file(angband.build_script_path("combat.lua"))
script_do_file(angband.build_script_path("scripts.lua"))
script_do_file(angband.build_script_path("passive.lua"))
script_do_file(angband.build_script_path("abilities.lua"))
script_do_file(angband.build_script_path("elements.lua"))
script_do_file(angband.build_script_path("objects.lua"))
script_do_file(angband.build_script_path("ranged.lua"))
script_do_file(angband.build_script_path("events.lua"))
script_do_file(angband.build_script_path("generators.lua"))
script_do_file(angband.build_script_path("music.lua"))
