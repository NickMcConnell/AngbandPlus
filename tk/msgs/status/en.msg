set loc en

::msgcat::mcset $loc Weak
::msgcat::mcset $loc Hungry
::msgcat::mcset $loc Full
::msgcat::mcset $loc Gorged

::msgcat::mcset $loc Blind
::msgcat::mcset $loc Confused
::msgcat::mcset $loc Afraid
::msgcat::mcset $loc Poisoned
::msgcat::mcset $loc Paralyzed!

::msgcat::mcset $loc "Rest %d"
::msgcat::mcset $loc "Rest *****"
::msgcat::mcset $loc "Rest &&&&&"
::msgcat::mcset $loc "Rep. %d"
::msgcat::mcset $loc "Repeat %d"
::msgcat::mcset $loc Searching

if {[variant OANGBANDTK]} {
::msgcat::mcset $loc Normal
::msgcat::mcset $loc Mouse
::msgcat::mcset $loc Ferret
::msgcat::mcset $loc Hound
::msgcat::mcset $loc Gazelle
::msgcat::mcset $loc Lion
::msgcat::mcset $loc Ent
::msgcat::mcset $loc Bat
::msgcat::mcset $loc Werewolf
::msgcat::mcset $loc Vampire
::msgcat::mcset $loc Wyrm
}

::msgcat::mcset $loc "Fast (%+d)"
::msgcat::mcset $loc "Slow (%+d)"

::msgcat::mcset $loc Study

::msgcat::mcset $loc "Mortal Wound"
::msgcat::mcset $loc "Deep gash"
::msgcat::mcset $loc "Severe cut"
::msgcat::mcset $loc "Nasty cut"
::msgcat::mcset $loc "Bad cut"
::msgcat::mcset $loc "Light cut"
::msgcat::mcset $loc Graze

::msgcat::mcset $loc "Knocked out"
::msgcat::mcset $loc "Heavy stun"
::msgcat::mcset $loc Stun

::msgcat::mcset $loc Wizard
::msgcat::mcset $loc Winner

#
# Extra status messages
#
::msgcat::mcset $loc Blessed
::msgcat::mcset $loc Hero
::msgcat::mcset $loc Berserk
::msgcat::mcset $loc "Res Acid"
::msgcat::mcset $loc "Res Cold"
::msgcat::mcset $loc "Res Elec"
::msgcat::mcset $loc "Res Fire"
::msgcat::mcset $loc "Res Poison"
::msgcat::mcset $loc "Prot Evil"
::msgcat::mcset $loc Shield

if {[variant ANGBANDTK KANGBANDTK ZANGBANDTK]} {
::msgcat::mcset $loc Invulnerable
}

::msgcat::mcset $loc Faster
::msgcat::mcset $loc Slower
::msgcat::mcset $loc Infravision
::msgcat::mcset $loc "See Invis"
::msgcat::mcset $loc Recall
::msgcat::mcset $loc Hallucinate

if {[variant KANGBANDTK]} {
::msgcat::mcset $loc "T/S Anchor"
::msgcat::mcset $loc Ghostly
::msgcat::mcset $loc Invisible
::msgcat::mcset $loc "Res Cha/Con"
::msgcat::mcset $loc "Res Lig/Dar"
::msgcat::mcset $loc "Res Sou/Sha"
::msgcat::mcset $loc "Res Nether"
::msgcat::mcset $loc "Res Nexus"
::msgcat::mcset $loc "Levitate"
::msgcat::mcset $loc "Sus Str"
::msgcat::mcset $loc "Sus Int"
::msgcat::mcset $loc "Sus Wis"
::msgcat::mcset $loc "Sus Dex"
::msgcat::mcset $loc "Sus Con"
::msgcat::mcset $loc "Sus Chr"
}

if {[variant OANGBANDTK]} {
::msgcat::mcset $loc "Att Acid"
::msgcat::mcset $loc "Att Elec"
::msgcat::mcset $loc "Att Fire"
::msgcat::mcset $loc "Att Cold"
::msgcat::mcset $loc "Att Poison"
::msgcat::mcset $loc ESP
::msgcat::mcset $loc "Res Magic"
::msgcat::mcset $loc Stealth
}

if {[variant ZANGBANDTK]} {
::msgcat::mcset $loc ESP
::msgcat::mcset $loc Wraith
}

