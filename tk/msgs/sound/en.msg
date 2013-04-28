set loc en

# Window title
::msgcat::mcset $loc Sound

# Sound menu
::msgcat::mcset $loc Sound
::msgcat::mcset $loc "Use Sound"

# Assign menu
::msgcat::mcset $loc Assign
::msgcat::mcset $loc "Insert Sound"
::msgcat::mcset $loc "Delete Sound"

# Config menu
::msgcat::mcset $loc Config
::msgcat::mcset $loc "New Configuration..."
::msgcat::mcset $loc "Add Configuration..."
::msgcat::mcset $loc "Edit Configuration..."
::msgcat::mcset $loc "Remove Configuration"
::msgcat::mcset $loc "Save"
::msgcat::mcset $loc "Revert"

# Directory menu
::msgcat::mcset $loc Directory
::msgcat::mcset $loc "Add Directory..."
::msgcat::mcset $loc "Edit Directory..."
::msgcat::mcset $loc "Remove Directory"

# Page titles
::msgcat::mcset $loc event Events
::msgcat::mcset $loc monster_attack "Monster Attacks"
::msgcat::mcset $loc monster_spell "Monster Spells"
::msgcat::mcset $loc martial_art "Martial Arts"

# History
::msgcat::mcset $loc History
::msgcat::mcset $loc "Sound History"
::msgcat::mcset $loc Monster
::msgcat::mcset $loc Description
::msgcat::mcset $loc "Sound File"
::msgcat::mcset $loc Group

# Sound volume
::msgcat::mcset $loc "Volume..."
::msgcat::mcset $loc "Sound Volume"

# Column titles
::msgcat::mcset $loc Configuration
::msgcat::mcset $loc "Sound Files"
::msgcat::mcset $loc "Assigned Sounds"

# Directory editor
::msgcat::mcset $loc dir-title-add "Add Directory"
::msgcat::mcset $loc dir-prompt-add \
	"Enter the location of a new sound directory. You can choose\
	the directory by clicking \"Browse...\" and selecting\
	a directory, or by typing in the name of a directory."
::msgcat::mcset $loc dir-title-edit "Edit Directory"
::msgcat::mcset $loc dir-prompt-edit \
	"The current sound directory is displayed below. You can change\
	the directory by clicking \"Browse...\" and selecting\
	another directory, or by typing in the name of another directory."

::msgcat::mcset $loc dir-title-remove "Remove Directory"
::msgcat::mcset $loc dir-msg-remove \
	"You are about to remove this directory from the list of directories:\n\n   \
	%s\n\nThis will not delete the actual folder from your hard drive.\n\
	You will lose any assignments to sounds in this directory in all\
	sound configurations.\n\nReally remove this sound directory?"

# Config editor
::msgcat::mcset $loc config-title-edit "Edit Configuration"
::msgcat::mcset $loc config-prompt-edit \
	"The file name should have the \".snd\" extension."
::msgcat::mcset $loc config-title-new "New Configuration"
::msgcat::mcset $loc config-prompt-new \
	"The file name should have the \".snd\" extension."
::msgcat::mcset $loc Title:
::msgcat::mcset $loc File:
::msgcat::mcset $loc config-title-exist "File Exists"
::msgcat::mcset $loc config-msg-exist \
	"There is already a file called \"%s\". Really use the file for the\
	new configuration?"
::msgcat::mcset $loc config-title-ext "File Name"
::msgcat::mcset $loc config-msg-ext \
	"Please use the \".snd\" extension."
::msgcat::mcset $loc config-title-notsnd "File Name"
::msgcat::mcset $loc config-msg-notsnd \
	"That file does not use the \".snd\" extension. Use it anyway?"

::msgcat::mcset $loc config-title-remove "Remove Configuration"
::msgcat::mcset $loc config-msg-remove \
	"You are about to remove this configuration from the list of\
	configurations:\n\n    %s\n\nRemoving a sound configuration does\
	not delete the sound configuration file on your hard drive. Really\
	remove this sound configuration?"

# "None" sound
::msgcat::mcset $loc sound-none None

# event
::msgcat::mcset $loc event,action_fail "Action failed"
::msgcat::mcset $loc event,store_enter "Enter a store"
::msgcat::mcset $loc event,store_leave "Leave a store"
::msgcat::mcset $loc event,store_purchase "Purchase an object"
::msgcat::mcset $loc event,store_sell "Sell an object"
::msgcat::mcset $loc event,store_sell_good "Sell a good object"
::msgcat::mcset $loc event,store_sell_bad "Sell a bad object"
::msgcat::mcset $loc event,home_enter "Enter the Home"
::msgcat::mcset $loc event,home_leave "Leave the Home"
::msgcat::mcset $loc event,activate "Activate an object"
::msgcat::mcset $loc event,use_food "Eat something"
::msgcat::mcset $loc event,use_potion "Quaff a potion"
::msgcat::mcset $loc event,use_rod "Zap a rod"
::msgcat::mcset $loc event,use_scroll "Read a scroll"
::msgcat::mcset $loc event,use_staff "Use a staff"
::msgcat::mcset $loc event,use_wand "Aim a wand"
::msgcat::mcset $loc event,use_fuel "Refuel a lantern or torch"
::msgcat::mcset $loc event,use_spike "Spike a door"
::msgcat::mcset $loc event,spell_cast "Cast a spell"
::msgcat::mcset $loc event,spell_learn "Learn a new spell"
::msgcat::mcset $loc event,spell_ball "Ball spell"
::msgcat::mcset $loc event,spell_beam "Beam spell"
::msgcat::mcset $loc event,spell_bolt "Bolt spell"
::msgcat::mcset $loc event,spell_teleport "Teleport"
::msgcat::mcset $loc event,spell_destruction "Destruction"
::msgcat::mcset $loc event,spell_earthquake "Earthquake"
::msgcat::mcset $loc event,spell_shatter "Shatterquake"
::msgcat::mcset $loc event,disarm_trap "Disarm a trap"
::msgcat::mcset $loc event,pick_lock "Pick a lock"
::msgcat::mcset $loc event,open "Open a door or chest"
::msgcat::mcset $loc event,close "Close a door"
::msgcat::mcset $loc event,pack_drop "Drop an object"
::msgcat::mcset $loc event,pack_pickup "Pick up an object"
::msgcat::mcset $loc event,pack_remove "Take off equipment"
::msgcat::mcset $loc event,pack_wear "Wear or wield an object"
::msgcat::mcset $loc event,pack_destroy "Pack object destroyed"
::msgcat::mcset $loc event,pack_overflow "Pack overflow"
::msgcat::mcset $loc event,pack_steal "Pack object stolen"
::msgcat::mcset $loc event,pack_disenchant "Pack object disenchanted"
::msgcat::mcset $loc event,pack_damage_acid "Pack object damaged by acid"
::msgcat::mcset $loc event,pack_drain "Pack object drained"
::msgcat::mcset $loc event,amnesia "Lose memory"
::msgcat::mcset $loc event,state_blind "Become blind"
::msgcat::mcset $loc event,state_confused "Become confused"
::msgcat::mcset $loc event,state_cut "Recieve a wound"
::msgcat::mcset $loc event,state_stun "Become stunned"
::msgcat::mcset $loc event,state_poisoned "Become poisoned"
::msgcat::mcset $loc event,state_paralyzed "Become paralyzed"
::msgcat::mcset $loc event,state_afraid "Become afraid"
::msgcat::mcset $loc event,state_hallucinate "Hallucinate"
::msgcat::mcset $loc event,state_hungry "Hungry"
::msgcat::mcset $loc event,state_hungry_end "No longer hungry"
::msgcat::mcset $loc event,state_full "Full"
::msgcat::mcset $loc event,state_full_end "No longer full"
::msgcat::mcset $loc event,state_gorged "Gorged"
::msgcat::mcset $loc event,state_gorged_end "No longer gorged"
::msgcat::mcset $loc event,state_fast "Move faster"
::msgcat::mcset $loc event,state_slow "Move slower"
::msgcat::mcset $loc event,state_shield "Mystic shield"
::msgcat::mcset $loc event,state_blessed "Become blessed"
::msgcat::mcset $loc event,state_hero "Become heroic"
::msgcat::mcset $loc event,state_berserk "Become berserk"
::msgcat::mcset $loc event,state_protection_evil "Protection from evil"
::msgcat::mcset $loc event,state_invulnerable "Become invulnerable"
::msgcat::mcset $loc event,state_see_invisible "See invisible"
::msgcat::mcset $loc event,state_infravision "Infravision"
::msgcat::mcset $loc event,state_resist_acid "Resist acid"
::msgcat::mcset $loc event,state_resist_lightning "Resist lightning"
::msgcat::mcset $loc event,state_resist_fire "Resist fire"
::msgcat::mcset $loc event,state_resist_cold "Resist frost"
::msgcat::mcset $loc event,state_resist_poison "Resist poison"
::msgcat::mcset $loc event,state_recover "Recover from condition"
::msgcat::mcset $loc event,state_change "End of temporary condition"
::msgcat::mcset $loc event,attack_hit "Attack (hit)"
::msgcat::mcset $loc event,attack_miss "Attack (miss)"
::msgcat::mcset $loc event,attack_good "Attack (good hit)"
::msgcat::mcset $loc event,attack_great "Attack (great hit)"
::msgcat::mcset $loc event,attack_superb "Attack (superb hit)"
::msgcat::mcset $loc event,attack_great2 "Attack (*great* hit)"
::msgcat::mcset $loc event,attack_superb2 "Attack (*superb* hit)"
::msgcat::mcset $loc event,attack_bow "Fire a missile weapon"
::msgcat::mcset $loc event,attack_throw "Throw an object"
::msgcat::mcset $loc event,bash "Bash something"
::msgcat::mcset $loc event,missile_hit "Thrown/fired object hit"
::msgcat::mcset $loc event,missile_miss "Thrown/fired object miss"
::msgcat::mcset $loc event,tunnel_rubble "Dig rubble"
::msgcat::mcset $loc event,tunnel_magma "Dig magma"
::msgcat::mcset $loc event,tunnel_quartz "Dig quartz"
::msgcat::mcset $loc event,tunnel_granite "Dig granite"
::msgcat::mcset $loc event,tunnel_permanent "Dig permanent"
::msgcat::mcset $loc event,tunnel_door "Dig door"
::msgcat::mcset $loc event,stat_drain "Stat was drained"
::msgcat::mcset $loc event,stat_restore "Stat was restored"
::msgcat::mcset $loc event,stat_gain "Stat was increased"
::msgcat::mcset $loc event,experience_drain "Experience was drained"
::msgcat::mcset $loc event,experience_restore "Experience was restored"
::msgcat::mcset $loc event,experience_level_up "Gain an experience level"
::msgcat::mcset $loc event,experience_level_down "Lose an experience level"
::msgcat::mcset $loc event,trap_miss "Trap failed"
::msgcat::mcset $loc event,trap_door "Trap (trap door)"
::msgcat::mcset $loc event,trap_pit_open "Trap (open pit)"
::msgcat::mcset $loc event,trap_pit_spiked "Trap (spiked pit)"
::msgcat::mcset $loc event,trap_pit_poison "Trap (poison pit)"
::msgcat::mcset $loc event,trap_rune_summon "Trap (summon rune)"
::msgcat::mcset $loc event,trap_rune_teleport "Trap (teleport rune)"
::msgcat::mcset $loc event,trap_spot_fire "Trap (fire spot)"
::msgcat::mcset $loc event,trap_spot_acid "Trap (acid spot)"
::msgcat::mcset $loc event,trap_dart_slow "Trap (slow dart)"
::msgcat::mcset $loc event,trap_dart_strength "Trap (strength dart)"
::msgcat::mcset $loc event,trap_dart_dexterity "Trap (dexterity dart)"
::msgcat::mcset $loc event,trap_dart_constitution "Trap (constitution dart)"
::msgcat::mcset $loc event,trap_gas_blind "Trap (blind gas)"
::msgcat::mcset $loc event,trap_gas_confuse "Trap (confuse gas)"
::msgcat::mcset $loc event,trap_gas_poison "Trap (poison gas)"
::msgcat::mcset $loc event,trap_gas_sleep "Trap (sleep gas)"
::msgcat::mcset $loc event,money "Pick up money (<200)"
::msgcat::mcset $loc event,money200 "Pick up money (200+)"
::msgcat::mcset $loc event,money600 "Pick up money (600+)"
::msgcat::mcset $loc event,monster_death "Monster killed"
::msgcat::mcset $loc event,monster_death_unique "Unique monster killed"
::msgcat::mcset $loc event,monster_kill_other "Monster kills other monster"
::msgcat::mcset $loc event,monster_multiply "Monster multiply"
::msgcat::mcset $loc event,monster_teleport "Monster teleport"
::msgcat::mcset $loc event,monster_pickup "Monster picks up object"
::msgcat::mcset $loc event,break_door "Door breaks open"
::msgcat::mcset $loc event,target_set "Target a monster or location"
::msgcat::mcset $loc event,walk "Walk a step"
::msgcat::mcset $loc event,depth_up "Go up a dungeon level"
::msgcat::mcset $loc event,depth_down "Go down a dungeon level"
::msgcat::mcset $loc event,depth_jump "Jump to a new level"
::msgcat::mcset $loc event,rest "Rest"
::msgcat::mcset $loc event,discover "Discover or sense something"
::msgcat::mcset $loc event,disturb "Disturbed"
::msgcat::mcset $loc event,disturb_unique "Disturbed by unique monster"
::msgcat::mcset $loc event,ident_worthless "Identify a worthless object"
::msgcat::mcset $loc event,ident_artifact "Identify an artifact"
::msgcat::mcset $loc event,ident_ego "Identify an ego object"
::msgcat::mcset $loc event,object_destroyed "Floor object destroyed"
::msgcat::mcset $loc event,object_auto_destroyed "Object auto-destroyed"
::msgcat::mcset $loc event,warn_hitpoints "Hitpoint warning"
::msgcat::mcset $loc event,character_death "Character death"
::msgcat::mcset $loc event,character_win "Win the game"
::msgcat::mcset $loc event,character_retire "Retire the character"
::msgcat::mcset $loc event,town_day "Ambient: Town (day)"
::msgcat::mcset $loc event,town_night "Ambient: Town (night)"
::msgcat::mcset $loc event,dungeon "Ambient: Dungeon"

if {[variant ANGBANDTK]} {
}

if {[variant KANGBANDTK]} {
::msgcat::mcset $loc event,walk_water "Walk a step (water)"
::msgcat::mcset $loc event,drown_in_water "Drown in water"
::msgcat::mcset $loc event,burned_by_lava "Burned by lava"
::msgcat::mcset $loc event,quest_done "Quest completed"
}

if {[variant OANGBANDTK]} {
::msgcat::mcset $loc event,monster_trap_set "Set a monster trap"
::msgcat::mcset $loc event,monster_trap_hit "Monster trips a monster trap"
::msgcat::mcset $loc event,walk_water "Walk a step (water)"
}

if {[variant ZANGBANDTK]} {
::msgcat::mcset $loc event,walk_water "Walk a step (water)"
::msgcat::mcset $loc event,drown_in_water "Drown in water"
::msgcat::mcset $loc event,burned_by_lava "Burned by lava"
::msgcat::mcset $loc event,quest_done "Quest completed"
::msgcat::mcset $loc event,wild_day "Ambient: Wilderness (day)"
::msgcat::mcset $loc event,wild_night "Ambient: Wilderness (night)"
::msgcat::mcset $loc event,mutate "Mutate"
::msgcat::mcset $loc event,lose_mutation "Lose mutation"
}

# monster_spell
::msgcat::mcset $loc monster_spell,SHRIEK "Shriek"
::msgcat::mcset $loc monster_spell,ARROW_3 "Fire Missile"
::msgcat::mcset $loc monster_spell,ARROW_4 "Fire Missiles"
::msgcat::mcset $loc monster_spell,BR_ACID "Breath Acid"
::msgcat::mcset $loc monster_spell,BR_ELEC "Breath Lightning"
::msgcat::mcset $loc monster_spell,BR_FIRE "Breath Fire"
::msgcat::mcset $loc monster_spell,BR_COLD "Breath Frost"
::msgcat::mcset $loc monster_spell,BR_POIS "Breath Poison Gas"
::msgcat::mcset $loc monster_spell,BR_NETH "Breath Nether"
::msgcat::mcset $loc monster_spell,BR_LITE "Breath Light"
::msgcat::mcset $loc monster_spell,BR_DARK "Breath Dark"
::msgcat::mcset $loc monster_spell,BR_CONF "Breath Confusion"
::msgcat::mcset $loc monster_spell,BR_SOUN "Breath Sound"
::msgcat::mcset $loc monster_spell,BR_CHAO "Breath Chaos"
::msgcat::mcset $loc monster_spell,BR_DISE "Breath Disenchantment"
::msgcat::mcset $loc monster_spell,BR_NEXU "Breath Nexus"
::msgcat::mcset $loc monster_spell,BR_TIME "Breath Time"
::msgcat::mcset $loc monster_spell,BR_INER "Breath Inertia"
::msgcat::mcset $loc monster_spell,BR_GRAV "Breath Gravity"
::msgcat::mcset $loc monster_spell,BR_SHAR "Breath Shards"
::msgcat::mcset $loc monster_spell,BR_PLAS "Breath Plasma"
::msgcat::mcset $loc monster_spell,BR_WALL "Breath Force"
::msgcat::mcset $loc monster_spell,BR_MANA "Breath Mana"
::msgcat::mcset $loc monster_spell,BA_ACID "Ball: Acid"
::msgcat::mcset $loc monster_spell,BA_ELEC "Ball: Lightning"
::msgcat::mcset $loc monster_spell,BA_FIRE "Ball: Fire"
::msgcat::mcset $loc monster_spell,BA_COLD "Ball: Frost"
::msgcat::mcset $loc monster_spell,BA_POIS "Ball: Poison Gas"
::msgcat::mcset $loc monster_spell,BA_NETH "Ball: Nether"
::msgcat::mcset $loc monster_spell,BA_WATE "Whirlpool"
::msgcat::mcset $loc monster_spell,BA_MANA "Mana Storm"
::msgcat::mcset $loc monster_spell,BA_DARK "Darkness Storm"
::msgcat::mcset $loc monster_spell,DRAIN_MANA "Drain Mana"
::msgcat::mcset $loc monster_spell,MIND_BLAST "Mind Blast"
::msgcat::mcset $loc monster_spell,BRAIN_SMASH "Brain Smash"
::msgcat::mcset $loc monster_spell,CAUSE_1 "Cause Wounds (Light)"
::msgcat::mcset $loc monster_spell,CAUSE_2 "Cause Wounds (Medium)"
::msgcat::mcset $loc monster_spell,CAUSE_3 "Cause Wounds (Serious)"
::msgcat::mcset $loc monster_spell,CAUSE_4 "Cause Wounds (Critical)"
::msgcat::mcset $loc monster_spell,BO_ACID "Bolt: Acid"
::msgcat::mcset $loc monster_spell,BO_ELEC "Bolt: Lightning"
::msgcat::mcset $loc monster_spell,BO_FIRE "Bolt: Fire"
::msgcat::mcset $loc monster_spell,BO_COLD "Bolt: Frost"
::msgcat::mcset $loc monster_spell,BO_POIS "Bolt: Poison"
::msgcat::mcset $loc monster_spell,BO_NETH "Bolt: Nether"
::msgcat::mcset $loc monster_spell,BO_WATE "Bolt: Water"
::msgcat::mcset $loc monster_spell,BO_MANA "Bolt: Mana"
::msgcat::mcset $loc monster_spell,BO_PLAS "Bolt: Plasma"
::msgcat::mcset $loc monster_spell,BO_ICEE "Bolt: Ice"
::msgcat::mcset $loc monster_spell,MISSILE "Magic Missile"
::msgcat::mcset $loc monster_spell,SCARE "Scare"
::msgcat::mcset $loc monster_spell,BLIND "Blind"
::msgcat::mcset $loc monster_spell,CONF "Confuse"
::msgcat::mcset $loc monster_spell,SLOW "Slow"
::msgcat::mcset $loc monster_spell,HOLD "Hold"
::msgcat::mcset $loc monster_spell,HASTE "Haste Self"
::msgcat::mcset $loc monster_spell,HEAL "Heal Self"
::msgcat::mcset $loc monster_spell,BLINK "Blink Self"
::msgcat::mcset $loc monster_spell,TPORT "Teleport Self"
::msgcat::mcset $loc monster_spell,TELE_TO "Teleport To"
::msgcat::mcset $loc monster_spell,TELE_AWAY "Teleport Away"
::msgcat::mcset $loc monster_spell,TELE_LEVEL "Teleport Level"
::msgcat::mcset $loc monster_spell,DARKNESS "Cause Darkness"
::msgcat::mcset $loc monster_spell,TRAPS "Create Traps"
::msgcat::mcset $loc monster_spell,FORGET "Cause Amnesia"
::msgcat::mcset $loc monster_spell,S_MONSTER "Summon Monster"
::msgcat::mcset $loc monster_spell,S_MONSTERS "Summon Monsters"
::msgcat::mcset $loc monster_spell,S_ANT "Summon Ants"
::msgcat::mcset $loc monster_spell,S_SPIDER "Summon Spiders"
::msgcat::mcset $loc monster_spell,S_HOUND "Summon Hounds"
::msgcat::mcset $loc monster_spell,S_DEMON "Summon Demons"
::msgcat::mcset $loc monster_spell,S_UNDEAD "Summon Undead"
::msgcat::mcset $loc monster_spell,S_DRAGON "Summon Dragons"
::msgcat::mcset $loc monster_spell,S_HI_UNDEAD "Summon Greater Undead"
::msgcat::mcset $loc monster_spell,S_HI_DRAGON "Summon Ancient Dragons"
::msgcat::mcset $loc monster_spell,S_UNIQUE "Summon Uniques"

if {[variant ANGBANDTK]} {
::msgcat::mcset $loc monster_spell,ARROW_1 "Fire Arrow"
::msgcat::mcset $loc monster_spell,ARROW_2 "Fire Arrows"
::msgcat::mcset $loc monster_spell,S_KIN "Summon Similar Creatures"
::msgcat::mcset $loc monster_spell,S_HI_DEMON "Summon Greater Demons"
::msgcat::mcset $loc monster_spell,S_HYDRA "Summon Hydras"
::msgcat::mcset $loc monster_spell,S_ANGEL "Summon Angels"
::msgcat::mcset $loc monster_spell,S_WRAITH "Summon Ringwraiths"
}

if {[variant KANGBANDTK]} {
::msgcat::mcset $loc monster_spell,ARROW_1 "Fire Arrow"
::msgcat::mcset $loc monster_spell,ARROW_2 "Fire Arrows"
::msgcat::mcset $loc monster_spell,S_KIN "Summon Similar Creatures"
::msgcat::mcset $loc monster_spell,S_HI_DEMON "Summon Greater Demons"
::msgcat::mcset $loc monster_spell,S_HYDRA "Summon Hydras"
::msgcat::mcset $loc monster_spell,S_ANGEL "Summon Angels"
::msgcat::mcset $loc monster_spell,S_WRAITH "Summon Ringwraiths"
}

if {[variant OANGBANDTK]} {
::msgcat::mcset $loc monster_spell,LASH "Whip/Spit"
::msgcat::mcset $loc monster_spell,SHOT "Fire Shot"
::msgcat::mcset $loc monster_spell,ARROW "Fire Arrow"
::msgcat::mcset $loc monster_spell,BOLT "Fire Bolt"
::msgcat::mcset $loc monster_spell,MISSL "Fire Missile"
::msgcat::mcset $loc monster_spell,PMISSL "Fire Missile (Poison)"
::msgcat::mcset $loc monster_spell,BEAM_ELEC "Beam: Lightning"
::msgcat::mcset $loc monster_spell,BEAM_ICE "Beam: Ice"
::msgcat::mcset $loc monster_spell,BEAM_NETH "Beam: Nether"
::msgcat::mcset $loc monster_spell,ARC_HFIRE "Arc: Fire"
::msgcat::mcset $loc monster_spell,ARC_FORCE "Arc: Force"
::msgcat::mcset $loc monster_spell,ADD_MANA "Add Mana"
::msgcat::mcset $loc monster_spell,CURE "Cure Self"
::msgcat::mcset $loc monster_spell,TELE_SELF_TO "Teleport Self To"
::msgcat::mcset $loc monster_spell,WOUND "Cause Wounds"
::msgcat::mcset $loc monster_spell,HUNGER "Cause Hunger"
::msgcat::mcset $loc monster_spell,S_KIN "Summon Similar Creatures"
::msgcat::mcset $loc monster_spell,S_ANIMAL "Summon Animal"
::msgcat::mcset $loc monster_spell,S_THIEF "Summon Thief"
::msgcat::mcset $loc monster_spell,S_BERTBILLTOM "Summon Bert, Bill and Tom"
::msgcat::mcset $loc monster_spell,S_HI_DEMON "Summon Greater Demons"
::msgcat::mcset $loc monster_spell,S_WRAITH "Summon Ringwraiths"
::msgcat::mcset $loc monster_spell,BOULDER "Throw Boulder"
::msgcat::mcset $loc monster_spell,BA_LITE "Invoke Starbursts"
::msgcat::mcset $loc monster_spell,BA_CONF "Ball: Confusion"
::msgcat::mcset $loc monster_spell,BA_SOUN "Ball: Sound"
::msgcat::mcset $loc monster_spell,BA_SHAR "Ball: Shards"
::msgcat::mcset $loc monster_spell,BA_STOR "Ball: Water"
::msgcat::mcset $loc monster_spell,BA_CHAO "Ball: Chaos"
::msgcat::mcset $loc monster_spell,BO_ICE "Bolt: Ice"
}

if {[variant ZANGBANDTK]} {
::msgcat::mcset $loc monster_spell,INVULNER "Invulnerability"
::msgcat::mcset $loc monster_spell,ARROW_1 "Fire Arrow"
::msgcat::mcset $loc monster_spell,ARROW_2 "Fire Arrows"
::msgcat::mcset $loc monster_spell,S_KIN "Summon Similar Creatures"
::msgcat::mcset $loc monster_spell,S_HYDRA "Summon Hydras"
::msgcat::mcset $loc monster_spell,S_ANGEL "Summon Angels"
::msgcat::mcset $loc monster_spell,ROCKET "Fire Rocket"
::msgcat::mcset $loc monster_spell,BA_NUKE "Ball: Radiation"
::msgcat::mcset $loc monster_spell,BR_NUKE "Breath Toxic Waste"
::msgcat::mcset $loc monster_spell,BA_CHAO "Ball: Logrus"
::msgcat::mcset $loc monster_spell,BR_DISI "Breath Disintegration"
::msgcat::mcset $loc monster_spell,HAND_DOOM "Hand Of Doom"
::msgcat::mcset $loc monster_spell,ANIM_DEAD "Raise Dead"
::msgcat::mcset $loc monster_spell,S_CYBER "Summon Cyberdemons"
::msgcat::mcset $loc monster_spell,S_AMBERITES "Summon Lords Of Amber"
}

# monster_attack
::msgcat::mcset $loc monster_attack,HIT "Hit"
::msgcat::mcset $loc monster_attack,TOUCH "Touch"
::msgcat::mcset $loc monster_attack,PUNCH "Punch"
::msgcat::mcset $loc monster_attack,KICK "Kick"
::msgcat::mcset $loc monster_attack,CLAW "Claw"
::msgcat::mcset $loc monster_attack,BITE "Bite"
::msgcat::mcset $loc monster_attack,STING "Sting"
::msgcat::mcset $loc monster_attack,BUTT "Butt"
::msgcat::mcset $loc monster_attack,CRUSH "Crush"
::msgcat::mcset $loc monster_attack,ENGULF "Engulf"
::msgcat::mcset $loc monster_attack,CRAWL "Crawl"
::msgcat::mcset $loc monster_attack,DROOL "Drool"
::msgcat::mcset $loc monster_attack,SPIT "Spit"
::msgcat::mcset $loc monster_attack,GAZE "Gaze"
::msgcat::mcset $loc monster_attack,WAIL "Wail"
::msgcat::mcset $loc monster_attack,SPORE "Spore"
::msgcat::mcset $loc monster_attack,BEG "Beg"
::msgcat::mcset $loc monster_attack,INSULT "Insult"
::msgcat::mcset $loc monster_attack,MOAN "Moan"

if {[variant ANGBANDTK]} {
}

if {[variant KANGBANDTK]} {
}

if {[variant OANGBANDTK]} {
::msgcat::mcset $loc monster_attack,REQUEST "Request"
}

if {[variant ZANGBANDTK]} {
::msgcat::mcset $loc monster_attack,CHARGE "Charge"
::msgcat::mcset $loc monster_attack,EXPLODE "Explode"
::msgcat::mcset $loc monster_attack,SHOW "Sing"
}

if {[variant OANGBANDTK]} {
# martial_art
::msgcat::mcset $loc martial_art,punch "Punch"
::msgcat::mcset $loc martial_art,kick "Kick"
::msgcat::mcset $loc martial_art,knee "Knee"
::msgcat::mcset $loc martial_art,chop "Chop"
::msgcat::mcset $loc martial_art,uppercut "Uppercut"
::msgcat::mcset $loc martial_art,boot "Boot"
::msgcat::mcset $loc martial_art,bang_on "Bang On"
::msgcat::mcset $loc martial_art,slam "Slam"
::msgcat::mcset $loc martial_art,grapple_with "Grapple With"
::msgcat::mcset $loc martial_art,hammer "Hammer"
::msgcat::mcset $loc martial_art,head_butt "Head Butt"
::msgcat::mcset $loc martial_art,strangle "Strangle"
::msgcat::mcset $loc martial_art,roundhouse_kick "Roundhouse Kick"
::msgcat::mcset $loc martial_art,assault "Assault"
::msgcat::mcset $loc martial_art,crush "Crush"
::msgcat::mcset $loc martial_art,double_kick "Double-Kick"
::msgcat::mcset $loc martial_art,thunderclap_belt "Thunderclap Belt"
::msgcat::mcset $loc martial_art,blizzard_gouge "Blizzard Gouge"
::msgcat::mcset $loc martial_art,tsunami_whirl "Tsunami Whirl"
::msgcat::mcset $loc martial_art,stormwind_chop "Stormwind Chop"
}

if {[variant ZANGBANDTK]} {
# martial_art
::msgcat::mcset $loc martial_art,punch "Punch"
::msgcat::mcset $loc martial_art,kick "Kick"
::msgcat::mcset $loc martial_art,strike "Strike"
::msgcat::mcset $loc martial_art,knee "Knee"
::msgcat::mcset $loc martial_art,elbow "Elbow"
::msgcat::mcset $loc martial_art,butt "Butt"
::msgcat::mcset $loc martial_art,kick2 "Kick"
::msgcat::mcset $loc martial_art,uppercut "Uppercut"
::msgcat::mcset $loc martial_art,double_kick "Double-kick"
::msgcat::mcset $loc martial_art,cats_claw "Cat's Claw"
::msgcat::mcset $loc martial_art,jump_kick "Jump kick"
::msgcat::mcset $loc martial_art,eagles_claw "Eagle's Claw"
::msgcat::mcset $loc martial_art,circle_kick "Circle kick"
::msgcat::mcset $loc martial_art,iron_fist "Iron Fist"
::msgcat::mcset $loc martial_art,flying_kick "Flying kick"
::msgcat::mcset $loc martial_art,dragon_fist "Dragon Fist"
::msgcat::mcset $loc martial_art,crushing_blow "Crushing Blow"
}

