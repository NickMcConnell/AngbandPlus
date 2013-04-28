set loc en

# Window title
::msgcat::mcset $loc "Controls"

# Menu
::msgcat::mcset $loc "Rogue-like Keyset"

# Commands
::msgcat::mcset $loc "Walk commands"
::msgcat::mcset $loc "Run commands"
::msgcat::mcset $loc "Alter commands"
::msgcat::mcset $loc "Inventory commands"
::msgcat::mcset $loc "Object commands"
::msgcat::mcset $loc "Magic commands"
::msgcat::mcset $loc "Alter commands"
::msgcat::mcset $loc "Resting commands"
::msgcat::mcset $loc "Other movement commands"
::msgcat::mcset $loc "Searching commands"
::msgcat::mcset $loc "Misc commands"
::msgcat::mcset $loc "Information commands"
::msgcat::mcset $loc "Preferences commands"
::msgcat::mcset $loc "Saving and quitting commands"
::msgcat::mcset $loc "Cheating/Debugging commands"

# Commands
::msgcat::mcset $loc stay-pickup "Stay (With Pickup)"
::msgcat::mcset $loc stay-nopickup "Stay (Without Pickup)"
::msgcat::mcset $loc walk-pickup "Walk (With Pickup)"
::msgcat::mcset $loc walk-nopickup "Walk (Without Pickup)"
foreach d {NW N NE W E SW S SE} {
	::msgcat::mcset $loc "Walk $d"
	::msgcat::mcset $loc "Run $d"
	::msgcat::mcset $loc "Alter $d"
}
::msgcat::mcset $loc "Cast Spell"
::msgcat::mcset $loc "Rest for HP/SP"
::msgcat::mcset $loc "Rest as needed"
::msgcat::mcset $loc "Version"
::msgcat::mcset $loc "Suicide/Retire"
::msgcat::mcset $loc "Debugging Command"
::msgcat::mcset $loc "Wizard Mode"

::msgcat::mcset $loc Alt+
::msgcat::mcset $loc Ctrl+
::msgcat::mcset $loc Shift+

::msgcat::mcset $loc "Assign key"
::msgcat::mcset $loc verify-prompt "The keypress \"%s\" is used by the action \"%s\".\nAssign it to \"%s\"?"
