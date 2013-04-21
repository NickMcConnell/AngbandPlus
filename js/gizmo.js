/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

//Global Objects

var gizmoPrototype = function( name ){

	var o = gizmos.prototypes[name];
  for( p in o ){
    this[p] = o[p];
  }
	this.name = name;
	this.number = 1;

}; //base of all gizmo's

gizmoPrototype.prototype.toString = function(){
	return this.name;
}

gizmoPrototype.prototype.display = {};

gizmoPrototype.prototype.display.ok = function(){

	if( view.items.showEmptySlots ) return true;

	if( this.type == TV_GOLD ) return false;

	if( view.items.filter.type > 0)
		if( view.items.filter.type  !=  this.type ) return false;

	if( view.items.filter.userMagic )
		if( player.realm1 == realms[this.realm]._key ) return false;

	if( view.items.filter.hook != null )
		if ( !view.items.filter.hook( this ) ) return false;

	return (true);
}

gizmoPrototype.prototype.aware = function(){

	if( gizmos.prototypes[ this.name ].aware === undefined ){
		return (gizmos.prototypes[ this.name ].aware = false);
	}
	return gizmos.prototypes[ this.name ].aware;
}

gizmoPrototype.prototype.tried = function(){

	if( gizmos.prototypes[ this.name ].tried === undefined ){
		return (gizmos.prototypes[ this.name ].tried = false);
	}
	return gizmos.prototypes[ this.name ].tried;
}

gizmoPrototype.prototype.known = function(){

	if( gizmos.prototypes[ this.name ].known === undefined ){
		return (gizmos.prototypes[ this.name ].known = false);
	}
	return gizmos.prototypes[ this.name ].known;
}

gizmoPrototype.prototype.is = function(s){
	return ( undefined !== this[s] );
}

gizmoPrototype.prototype.has = function(s){
	return ( undefined !== this[s] );
}

gizmoPrototype.prototype.isArtifact = function(){
	return this.is( "ARTIFACT" );
}

gizmoPrototype.prototype.isEgo = function(){
	return this.is( "EGO" );
}

gizmoPrototype.prototype.isCursed = function(){
	return this.is( "CURSED" );
}

/*
* Creates a description of the item "gizmo", and returns it as a string.
*
* One can choose the "verbosity" of the description, including whether
* or not the "number" of items should be described, and how much detail
* should be used when describing the item.
*
* Note that all artifacts (when known) append an "Artifact Name", so we
* have special processing for "Specials" (artifact Lites, Rings, Amulets).
* The "Specials" never use "modifiers" if they are "known", since they
* have special "descriptions", such as "The Necklace of the Dwarves".
*
* Special Lite's use the "k_info" base-name (Phial, Star, or Arkenstone),
* plus the artifact name, just like any other artifact, if known.
*
* Special Ring's and Amulet's, if not "aware", use the same code as normal
* rings and amulets, and if "aware", use the "k_info" base-name (Ring or
* Amulet or Necklace).  They will NEVER "append" the "k_info" name.  But,
* they will append the artifact name, just like any artifact, if known.
*
* None of the Special Rings/Amulets are "EASY_KNOW", though they could be,
* at least, those which have no "pluses", such as the three artifact lites.
*
* If "prefix" then a "numeric" prefix will be pre-pended.
*
* Mode:
*   0 -- The Cloak of Death
*   1 -- The Cloak of Death [1,+3]
*   2 -- The Cloak of Death [1,+3] (+2 to Stealth)
*   3 -- The Cloak of Death [1,+3] (+2 to Stealth) {nifty}

 Julian Lighton's improved code */

gizmoPrototype.prototype.describe = function( prefix , mode ){

	var gizmo = this;
	var describer; //TVAL describer...
	var t;

  var name = {};
	name.base = "";
	name.mod = "";
	name.base = this.name;
	name.append = false;
	name.artifact = {}
	name.mod = "";
	name.type = {};
	name.type.found = false;
	name.show = {};
	name.show.armor = false;
	name.show.weapon = false;
  name.position = {};
  name.position.tilde = 0;
  name.position.ampersand = 0;

	var artifact = gizmo.isArtifact();
	var aware = gizmo.aware();
	var known = gizmo.known();
	var tried = gizmo.tried();

	if ( artifact && aware && known ){

		name.base = gizmo.artifact.name;
		if( name.base.substring(0,1) == '!' ){
			name.artifact.overrides = true; 
			name.base = name.base.substring( 1 );
		}else{
			name.artifact.overrides = false; 
			name.base = gizmo.name;
		}
	}

	if(gizmo.type == GOLD)
		return name.base.substring(1);

	if(gizmo.realm !== undefined ){
		name.mode = name.base;
		if( realmChoices[ player.vocation ][0] == CH_NONE )
			name.base = realms[ gizmo.realm ].illiterate;
		else
			name.base = realms[ gizmo.realm ].literate;
		name.type.found = true;
	}

	if( !name.type.found ){

		/* Get describer */
		describer = itemTypes.getType( gizmo.type );
		/* Make sure you found it */
		if( describer.type == gizmo.type ){

			name.type.found = true;
			name.show.weapon = ( describer.action == ACT_SHOW_WEAPON );
			name.show.armour = ( describer.action == ACT_SHOW_ARMOUR );

			if( describer.action == ACT_FULL_MONTY && !( artifact && aware ) ){

				if( describer.flavors != null )
					name.mod = describer.flavors[gizmo.subtype];

				if( aware ) 
					name.append = true;
				if ( ( _options["plain_descriptions"].value &&  aware ) || gizmo.ident & IDENT_STOREB)
					name.base = describer.plain;
				else
					name.base = describer.full;
			}
		}
	}


	if(!name.type.found)
		return "nothing";

	name.base = name.base.substring( 2 );

	/* The object "expects" a "number" */
	if (name.base.substring(0,1) == '&'){

		if( prefix ){

			if ( gizmo.number <= 0)
				t = "no more";

			else if (gizmo.number > 1)
				t = gizmo.number + '';

			else if( known && artifact ) //Hack -- The only one of its kind
				t = "The";

			else if( name.base.substring(2,3) == '#' && name.mod[0].substring(0,1).isVowel() ) 
				t = "an";

			else if( name.base.substring(2,3).isVowel() )
				t = "an";

			else
				t = "a";

		} else {

			if ( gizmo.number <= 0)
			  t = "no more";

			else if (gizmo.number > 1)
				t = gizmo.number + "";

			else if( known && artifact ) //Hack -- The only one of its kind
				t = "The";
		}
		//Fill in the count
		name.base = name.base.replace( "&" , t );
	}

  name.position.tilde    = name.base.indexOf( "~" );
  name.position.cardinal = name.base.indexOf( "#" );

  if( name.position.tilde > -1 && gizmo.number != 1 )
  {
    var c = name.base.substring( name.position.tilde , name.position.tilde + 1 );
    name.base = name.base.replace( '~' , ( c == 's' || c == 'h' )?"es":"s" );
  }else if( name.position.tilde > -1 )
    name.base = name.base.replace( '~' , '' );

  if( name.position.cardinal > -1 && name.mod != "" )
  {
    name.base = name.base.replace( '#' , name.mod );
  }

	/* Append the "kind name" to the "base name" */
	if( name.append )
		name.base = name.base + " of " + gizmo.name;

	/* Hack -- Append "Artifact" or "Special" names */
	if ( known )
	{
		/* Random arts should behave same as fixed, due to down copying & stuff */
		if( gizmo.isArtifact() && !name.artifact.overrides )
			name.base  = name.base + ' ' +  gizmo.artifact.name;

		/* Grab any ego-item name */
		if( gizmo.isEgo() )
			name.base  = name.base + ' ' +  gizmo.ego.name;
	}

	/* No more details wanted */
	if (mode < 1) 
		return name.base;

	/* Hack -- Chests must be described in detail */
	if (gizmo.type == CHEST && known )
	{
		if ( !gizmo.pval)
		{
			t = object_desc_str(t, " (empty)");
		}
		else if (gizmo.pval < 0) //Maybe disarmed / empty
		{
			if (chest_traps[gizmo.pval])
				name.base = name.base + " (disarmed)";
					else
				name.base = name.base + " (unlocked)";
		}
		else // Describe the traps, if any
		{
			switch (chest_traps[gizmo.pval])
			{
				case 0:
					base.name = base.name + " (Locked)";
				case CHEST_LOSE_STR:
					base.name = base.name + " (Poison Needle)";
				case CHEST_LOSE_CON:
					base.name = base.name + " (Poison Needle)";
				case CHEST_POISON:
					base.name = base.name + " (Gas Trap)";
				case CHEST_PARALYZE:
					base.name = base.name + " (Gas Trap)";
				case CHEST_EXPLODE:
					base.name = base.name + " (Explosion Device)";
				case CHEST_SUMMON:
					base.name = base.name + " (Summoning Runes)";
				default:
					base.name = base.name + " (Multiple traps)";
			}
		}
	}


	name.show.weapon = name.show.weapon || gizmo.has( 'SHOW_MODS' ) || ( gizmo.hitbonus !== undefined || gizmo.dambonus !== undefined );
	name.show.armour = name.show.armour || ( gizmo.armor !== undefined && gizmo.armor > 0 );

	if( name.show.weapon )
	{
		// Dump base weapon info
		switch (gizmo.type)
		{
			// Missiles and Weapons
			case SHOT:
			case BOLT:
			case ARROW:
			case HAFTED:
			case POLEARM:
			case BLADE:
			case DIGGER:
				name.base = name.base + ' (' + gizmo.damage + ')';
				break;
			// Launchers
			case LAUNCHER:
				var power = (gizmo.subtype % 10) + gizmo.has( "XTRA_MIGHT" )?1:0;
				name.base = name.base + ' (' + power + ')';
		}
	}

	/* Add the weapon & armor bonuses */
	if( known )
	{
		if (name.show.weapon)
			name.base = name.base + " (" + String.sign(gizmo.hitbonus) + "," + String.sign(gizmo.dambonus) + ")";
		else if (gizmo.hitbonus !== undefined && gizmo.hitbonus > 0 )
			name.base = name.base + ' (' + String.sign(gizmo.hitbonus) + ')';
		else if (gizmo.dambonus !== undefined && gizmo.dambonus > 0 )
			name.base = name.base + ' (' + String.sign(gizmo.dambonus) + ')';

		/* Show the armour class info */
		if (name.show.armour)
			name.base = name.base + ' [' + gizmo.armor + "," + String.sign(gizmo.acbonus) + ']';
		else if (gizmo.acbonus !== undefined && gizmo.acbonus > 0)
			name.base = name.base + ' [' + String.sign(gizmo.acbonus) + ']';
	}
	else if (name.show.armour)//Hack -- always show base armour, even unknown 
	{
			name.base = name.base + ' [' + gizmo.armor + ']';
	}


	/* No more details wanted */
	if(mode < 2) 
		return name.base;

	if( known && ( gizmo.type == STAFF || gizmo.type == WAND ) ) //Wands and Staf charge count in pval
		name.base = name.base + ' ' + gizmo.pval + 'charge' + gizmo.pval!=1?"s":"";
	else if( known && gizmo.type == TV_ROD && gizmo.pval > 0) //Rods have a "charging" indicator in pval */
		name.base = name.base + " charging";
	else if( gizmo.type == LITE && gizmo.pval > 0 ) //Hack^2 -- Process Lanterns/Torches which we hope are the only lights with pval = 0*/
		name.base = name.base + " (with " + gizmo.pval + " turns of light)";

	/* Dump "pval" flags for wearable items, first come , first served */
	if (known )
	{
		if( gizmo.has( "HIDE_TYPE" ) )   name.base = name.base + " "  + String.sign( pval );
		else if ( gizmo.has( "SPEED") )  name.base = name.base + " (" + String.sign( pval ) + " to movement speed)";
		else if (gizmo.has( 'BLOWS' ))   name.base = name.base + " (" + String.sign( pval ) + " to attack speed)";
		else if (gizmo.has( 'STEALTH' )) name.base = name.base + " (" + String.sign( pval ) + " to stealth)";
		else if (gizmo.has( 'SEARCH' ))  name.base = name.base + " (" + String.sign( pval ) + " to searching)";
		else if (gizmo.has( 'INFRA' ))   name.base = name.base + " (" + String.sign( pval ) + " to infravision)";
	}

	/* Indicate "charging" artifacts XXX XXX XXX */
	if( known && gizmo.timeout > 0 )
		name.base = name.base + " (charging)"; 

	/* No more details wanted */
	if( mode < 3 ) 
		return name.base;

	/* Use the standard inscription if available */
	if( gizmo.note )
		name.base = name.base + "{" + gizmo.note + "}"; 

	/* Note "cursed" if the item is known to be cursed */
	if( gizmo.isCursed() && ( known || gizmo.ident & IDENT_SENSE ) && !gizmo.note )
		name.base = name.base + " {cursed}"; 

	if( !known && gizmo.ident & IDENT_EMPTY ) //Mega-Hack -- note empty wands/staffs
		name.base = name.base + " {empty}"; 

	if( gizmo.discount ) //Note the discount, if any
		name.base = name.base + " { " + gizmo.discount + "% off}"; 

	/* Add "tried" if the object has been tested unsuccessfully */
	if( !aware && tried )
		name.base = name.base + " {tried}"; 

	return name.base;
}

//Private Objects


//Initialization

//no initialization


//Public Functions


//Private Functions

