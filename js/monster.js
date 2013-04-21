/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

var ALLY_NO        = 0;  // Hate every one
var ALLY_LUCIFER   = 1;  // Hate player and good guys
var ALLY_GOD       = 2;  // Hate player and bad guys
var ALLY_RACE      = 4;  // Upset one of the race, they're all upset OR hates everyone except same race
var ALLY_PLAYER    = 8;  // Ai is friendly towards player, can get upset
var ALLY_SELF      = 16; // True Neutral, giants in the 9th circle
var ALLY_COMPANION = 32; // AI nevers gets upset, friend with benefits

var MFLAG_VIEW     = 0x01    /* Monster is in line of sight */
var MFLAG_BORN     = 0x10    /* Monster is still being born */
var MFLAG_NICE     = 0x20    /* Monster is still being nice */
var MFLAG_SHOW     = 0x40    /* Monster is recently memorized */
var MFLAG_MARK     = 0x80    /* Monster is currently memorized */

var monsterPrototype = function(){
	this.isTarget = false;
};


//This better never go wrong ;]
monsterPrototype.prototype.has = function has(flag){

	try{
		  return monsters[this.r_idx].flags[flag]?true:false;
	}catch(e){}	
	return false;
}

//Syntactic sugar, has( "UNIQUE" ) just looks silly ;]
monsterPrototype.prototype.is = monsterPrototype.prototype.has;

monsterPrototype.prototype.canCross = function( feature ){

	if( feature == FEAT_WATER )
		return this.is( "AQUATIC" ) || this.has( "FLIGHT" );

	return true;
}

/* This function updates the monster record of the given monster
*
* This involves extracting the distance to the player, checking
* for visibility (natural, infravision, see-invis, telepathy),
* updating the monster visibility flag, redrawing or erasing the
* monster when the visibility changes, and taking note of any
* "visual" features of the monster (cold-blooded, invisible, etc).
*
* The only monster fields that are changed here are "cdis" (the
* distance from the player), "los" (clearly visible to player),
* and "ml" (visible to the player in any way).
*
* There are a few cases where the calling routine knows that the
* distance from the player to the monster has not changed, and so
* we have a special parameter "full" to request distance computation.
* This lets many calls to this function run very quickly.
*
* Note that every time a monster moves, we must call this function
* for that monster, and update distance.  Note that every time the
* player moves, we must call this function for every monster, and
* update distance.  Note that every time the player "state" changes
* in certain ways (including "blindness", "infravision", "telepathy",
* and "see invisible"), we must call this function for every monster.
*
* The routines that actually move the monsters call this routine
* directly, and the ones that move the player, or notice changes
* in the player state, call "update_monsters()".
*
* Routines that change the "illumination" of grids must also call
* this function, since the "visibility" of some monsters may be
* based on the illumination of their grid.
*
* Note that this function is called once per monster every time the
* player moves, so it is important to optimize it for monsters which
* are far away.  Note the optimization which skips monsters which
* are far away and were completely invisible last turn.
*
* Note the optimized "inline" version of the "Math.distance()" function.
*
* Note that only monsters on the current panel can be "visible",
* and then only if they are (1) in line of sight and illuminated
* by light or infravision, or (2) nearby and detected by telepathy.
*
* The player can choose to be disturbed by several things, including
* "player.disturb.move" (monster which is viewable moves in some way), and
* "disturb_near" (monster which is "easily" viewable moves in some
* way).  Note that "moves" includes "appears" and "disappears".
*
* Note the new "xtra" field which encodes several state flags such
* as "detected last turn", and "detected this turn", and "currently
* in line of sight", all of which are used for visibility testing. */

monsterPrototype.prototype.update = function( full ){

	var monster = this;
	var monsterRace = monsters[monster.r_idx];

	/* The current monster location */
  var fx = monster.x;
	var fy = monster.y;

	var cell = cave[fx][fy];

  var seen = false; //Seen at all
  var easy = false; //Seen by vision
  var hard = false; //Seen by telepathy

	/* Various extra flags */
  var do_empty_mind = false;
  var do_weird_mind = false;
  var do_invisible  = false;
  var do_cold_blood = false;

	/* Calculate approximated distance */
	if (full){
		var dy = Math.round( (player.y > fy) ? (player.y - fy) : (fy - player.y) );
		var dx = Math.round( (player.x > fx) ? (player.x - fx) : (fx - player.x) );
		monster.distance = Math.round( (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1)) );
	}

	//Skip "distant" monsters that were already not visible
	if (monster.distance > MAX_SIGHT && !monster.visible ) return;

	/* Normal line of sight, and player is not blind */
	if ( ( cell.info & CAVE_VIEW ) && !player.blind ){

		if (monster.distance <= player.calc.infravision){ //Check infravision
			do_cold_blood = monsterRace.has( "COLD_BLOOD" );
			if (!do_cold_blood) easy = seen = true;
		}

		if (cell.info & (CAVE_LITE | CAVE_GLOW)){ //Check light
			do_invisible = monsterRace.has( "INVISIBLE" ); //Check invisibility
			if (!do_invisible || player.see_inv) easy = seen = true;
		}
	}

	//In debug mode, or spell marked or wizard mode or already seen
	seen = (monster.mflag & MFLAG_MARK) || debug.mode || seen;

	/* Telepathy can see all "nearby" monsters with "minds" */
	if (player.telepathy){

		do_empty_mind = monsterRace.has( "EMPTY_MIND" ); //Check empty mind
		do_weird_mind = monsterRace.has( "WEIRD_MIND" ); //Check weird mind
		
		if( do_weird_mind )
			if( rng.base0.random(100) < 10 )
				hard = seen = true;

		hard = ( ( !do_empty_mind && !do_weird_mind ) || hard );
		seen = seen || hard;
	}

	/* The monster is now visible */
	if (seen){

		/* It was previously unseen */
		if (!monster.visible){

			monster.visible = true;

			/* Draw the monster */
			cave.illuminate(fx,fy);

			/* Update health bar as needed */
			if ( monster.isTarget ) player.redraw |= (PR_HEALTH);

			/* Update monster list window */
			player.window |= (PW_VISIBLE);

			monsterRace.sights++;

			/* Disturb on appearance */
			if (player.disturb.move){
				if (player.disturb.allies || !(monster.isAlly()))
					player.action.disturb(1, 0);
			}
		}

		/* Apply telepathy */
		if (hard){

			if( monsterRace.has( "SMART" ) ) monsterRace.observe( "SMART" );
			if( monsterRace.has( "STUPID" ) ) monsterRace.observe( "STUPID" );
		}

		/* Memorize various observable flags */
		if (do_empty_mind) monsterRace.observe( "EMPTY_MIND" );
		if (do_weird_mind) monsterRace.observe( "WEIRD_MIND" );
		if (do_cold_blood) monsterRace.observe( "COLD_BLOOD" );
		if (do_invisible)  monsterRace.observe( "INVISIBLE" );
	}

	/* The monster is not visible */
	else{

		/* It was previously seen */
		if (monster.visible){

			monster.visible = false;				//Monster is no longer visible
			cave.illuminate(fx,fy);					// Stop showing the monster
			player.window |= (PW_VISIBLE);	//Update monster list window

			/* Update health bar as needed */
			if (monster.isTarget) player.redraw |= (PR_HEALTH);

			/* Disturb on disappearance*/
			if (player.disturb.move && ( player.disturb.allies || !monster.isAlly() ) )
					player.action.disturb(1, 0);
		}
	}


	/* The monster is now easily visible */
	/* TODO, disturbing might be needed to trigger more.. */
	if (easy){

		if (!(monster.mflag & (MFLAG_VIEW))){

			monster.mflag |= (MFLAG_VIEW);

			if( player.disturb.near && ( player.disturb.allies || !monster.isAlly() ) )
				player.action.disturb(1, 0);

		}
	}else{ 	// The monster is not easily visible

		if (monster.mflag & (MFLAG_VIEW)){ 

			monster.mflag &= ~(MFLAG_VIEW); //No longer easier to see

			/* Disturb on disappearance */
			if( player.disturb.near && (player.disturb.allies || !monster.isAlly() ) )
					player.action.disturb(1, 0);
		}
	}
}


