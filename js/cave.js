/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

 var FEAT_NONE           = 0x00;
 var FEAT_FLOOR          = 0x01;
 var FEAT_INVIS          = 0x02;
 var FEAT_GLYPH          = 0x03;
 var FEAT_OPEN           = 0x04;
 var FEAT_BROKEN         = 0x05;
 var FEAT_LESS           = 0x06;
 var FEAT_MORE           = 0x07;
 var FEAT_PATH           = 0x08;
 var FEAT_TRAP_HEAD      = 0x10;
 var FEAT_TRAP_TAIL      = 0x1F;
 var FEAT_DOOR_HEAD      = 0x20;
 var FEAT_DOOR_TAIL      = 0x2F;
 var FEAT_SECRET         = 0x30;
 var FEAT_RUBBLE         = 0x31;
 var FEAT_MAGMA          = 0x32;
 var FEAT_QUARTZ         = 0x33;
 var FEAT_MAGMA_H        = 0x34;
 var FEAT_QUARTZ_H       = 0x35;
 var FEAT_MAGMA_K        = 0x36;
 var FEAT_QUARTZ_K       = 0x37;
 var FEAT_WALL_EXTRA     = 0x38;
 var FEAT_WALL_INNER     = 0x39;
 var FEAT_WALL_OUTER     = 0x3A;
 var FEAT_WALL_SOLID     = 0x3B;
 var FEAT_PERM_BUILDING  = 0x3C;
 var FEAT_PERM_INNER     = 0x3D;
 var FEAT_PERM_OUTER     = 0x3E;
 var FEAT_PERM_SOLID     = 0x3F;
 var FEAT_MINOR_GLYPH    = 0x40;
 var FEAT_PATTERN_START  = 0x41;
 var FEAT_PATTERN_1      = 0x42;
 var FEAT_PATTERN_2      = 0x43;
 var FEAT_PATTERN_3      = 0x44;
 var FEAT_PATTERN_4      = 0x45;
 var FEAT_PATTERN_END    = 0x46;
 var FEAT_PATTERN_OLD    = 0x47;
 var FEAT_PATTERN_XTRA1  = 0x48;
 var FEAT_PATTERN_XTRA2  = 0x49;
 var FEAT_SHOP_HEAD      = 0x4A;
 var FEAT_SHOP_TAIL      = 0x55;
 var FEAT_GATE           = 0x60;
 var FEAT_WATER          = 0x61;
 var FEAT_TREE           = 0x62;
 var FEAT_BUSH           = 0x63;

 var CAVE_MARK           = 0x01;    /* memorized feature */
 var CAVE_GLOW           = 0x02;    /* self-illuminating */
 var CAVE_ICKY           = 0x04;    /* part of a vault */
 var CAVE_ROOM           = 0x08;    /* part of a room */
 var CAVE_LITE           = 0x10;    /* lite flag  */
 var CAVE_VIEW           = 0x20;    /* view flag */
 var CAVE_TEMP           = 0x40;    /* temp flag */
 var CAVE_XTRA           = 0x80;    /* misc flag */


var cave;
var monsterFilterHook; //TODO: Not sure I like the placement of this variable

function initCave(){
  var x, y;
	cave = new Array();
  for (x = 0; x < MAX_WID; x++){
    cave[x] = new Array();
    for (y = 0; y < MAX_HGT; y++)
      cave[x][y] = createCell();
  }
  cave.initialize = _initialize;
  cave.placeMonsters = _placeMonsters;
  cave.placeMonster  = _placeMonster;
  cave.pickMonsterRace = _pickMonsterRace;
  cave.placeMonsterAux = _placeMonsterAux;
  cave.placeMonsterOne = _placeMonsterOne;

	//TODO create clearCave for level changes.

	cave.inBounds = _inBounds;
	cave.canPlaceMonster = _canPlaceMonster;
	cave.initialize();
  cave.monsterLevel = 0;
	cave.goodItemFlag = false; //TODO Might become cave.feeling.goodItem = false..
	cave.rating = 0; //TODO Might become cave.feeling.rating = 0..

}


/* Attempt to allocate a random monster in the dungeon.
 * Place the monster at least "dis" distance from the player.
 * Use "slp" to choose the initial "sleep" status
 * Use "cave.monsterLevel" for the monster level */
function _placeMonsters( dis , sleep ){

  var y, x;
  var attempts_left = 10000;
  var group = true;
	
	/* Find a legal, distant, unoccupied, space */
	while (attempts_left){

		/* Pick a location */
		y = rng.base0.random(cave.currentHeight);
		x = rng.base0.random(cave.currentWidth);

		/* Require "naked" floor grid */
		if (!cave[x][y].isEmpty()) continue;

		/* Accept far away grids */
    if (Math.distance(x, y,player.x, player.y) > dis) 
      return cave.placeMonster(x, y, sleep, group);

		attempts_left--;
	}

	if (debug.xtra || debug.hear)
		view.message("Warning! Could not allocate a new monster. Small level?");
	return false;
}

function _placeMonster( x , y , sleep , group ){

  var r_idx;
  var charm = false;

  /* Pick a monster */
  r_idx = cave.pickMonsterRace(cave.monsterLevel);

	/* Handle failure */
  if (!r_idx) return (false);

	/* Place monster, return info on how many were placed */
  return cave.placeMonsterAux(x, y, r_idx, sleep, group, charm);

}


/* Attempt to place a monster of the given race at the given location
 *
 * Note that certain monsters are now marked as requiring "friends".
 * These monsters, if successfully placed, and if the "grp" parameter
 * is true, will be surrounded by a "group" of identical monsters.
 *
 * Note that certain monsters are now marked as requiring an "escort",
 * which is a collection of monsters with similar "race" but lower level.
 *
 * Some monsters induce a fake "group" flag on their escorts.
 *
 * Note the "bizarre" use of non-recursion to prevent annoying output
 * when running a code profiler.
 *
 * Note the use of the new "monster allocation table" code to restrict
 * the "cave.pickMonsterRace()" function to "legal" escort types. */
function _placeMonsterAux(x, y, r_idx, sleep, group, charm){

  var i;
  var monsterCount;

  var monsterRace = monsters[r_idx];

  /* Place one monster, or fail */
  if (!cave.placeMonsterOne(x, y, r_idx, sleep, charm)) 
    return (0);
  monsterCount = 1;

  /* Escorts for certain monsters */
  if (monsterRace.has( "ESCORT" )){
	  
    /* Try to place several "escorts" */
    for (i = 0; i < 50; i++){
      var nx, ny, z, d = 3;

      /* Pick a location */
      /* DO NOT FORGET POINTER MAGIC! scatter(&nx, &ny, x, y, d, 0); */
      scatter( nx, ny, x, y, d, 0);

      if( monsterRace.r_escort > 0 ){
        /* Some monsters have their escorts defined */
        z = monsterRace.r_escort;
      }else{  
        /* Set the escort index */
        placeMonsterIdx = r_idx;
        
        /* Set the escort hook */
        monsterFilterHook = place_monster_okay;
        
        /* Prepare allocation table */
        get_mon_num_prep();
        
        /* Pick a random race */
        z = cave.pickMonsterRace(monsterRace.level);
        
        /* Remove restriction */
        monsterFilterHook = NULL;
        
        /* Prepare allocation table */
        get_mon_num_prep();
      }

      /* Handle failure */
      if (!z) break;
      
      /* Require empty grids */
      /*if (!!cave[nx][ny].isOccupied() || (cave[ny][nx].feat == FEAT_WATER)) continue;*/
      if(!can_cave.placeMonster(ny,nx,z))continue;

      /* Place a single escort */
      if( cave.placeMonsterOne(nx, ny, z, sleep, charm) )
        monsterCount++;

      /* Place a "group" of escorts if needed */
      if ((monsters[z].has( "FRIENDS" )) ||
        (monsterRace.has( "ESCORTS" ))){
        /* Place a group of monsters */
        monsterCount = monsterCount + cave.placeMonsterGroup(nx, ny, z, sleep, charm);
      }
    }
  }

  /* Require the "group" flag for further logic*/
  if (!group) return monsterCount;

  /* Friends for certain monsters */
  if (monsterRace.has( "FRIENDS" )){
    /* Attempt to place a group */
    monsterCount = monsterCount + cave.placeMonsterGroup(x, y, r_idx, sleep, charm);
  }

  /* Success */
  return monsterCount;
}

/*
* Attempt to place a monster of the given race at the given location.
*
* To give the player a sporting chance, any monster that appears in
* line-of-sight and is extremely dangerous can be marked as
* "FORCE_SLEEP", which will cause them to be placed with low energy,
* which often (but not always) lets the player move before they do.
*
* This routine refuses to place out-of-depth "FORCE_DEPTH" monsters.
*
* XXX XXX XXX Use special "here" and "dead" flags for unique monsters,
* remove old "cur_num" and "max_num" fields.
*
* XXX XXX XXX Actually, do something similar for artefacts, to simplify
* the "preserve" mode, and to make the "what artefacts" flag more useful.
*
* This is the only function which may place a monster in the dungeon,
* except for the savefile loading code.
*/
function _placeMonsterOne( x, y, r_idx, sleep, charm ){

	var i;

	var monsterRace = monsters[r_idx];

	var name = monsterRace.name;
	
	/*Variables for Satan's circle*/
  var lx,ly,x1,y1,k;

	/* Verify location */
	if (!cave.inBounds(x, y)) return (false);
	
	/* Paranoia */
	if (!r_idx) return (false);

	/* Require empty space */
	/* if ((!!cave[x][y].isOccupied()) || cave[y][x].feat == FEAT_WATER) return (false); */
	if (!cave.canPlaceMonster(y,x,r_idx)) return (false);

	/* Hack -- no creation on glyph of warding */
	if (cave[y][x].feat == FEAT_GLYPH) return (false);
	if (cave[y][x].feat == FEAT_MINOR_GLYPH) return (false);

	/* Paranoia */
	if (!monsterRace.name) return (false);

	/* Paranoid hack -- "unique" monsters must be "unique" */
	if ((monsterRace.has( "UNIQUE" )) &&
		(monsterRace.count.current >= monsterRace.count.max ))
			return (false);

	/*
	* Check quest monsters
	* Heino Vander Sanden
	*/
	if( monsterRace.has( "GUARDIAN" ) || monsterRace.has( "ALWAYS_GUARD" ) ){

		if( !quests.onQuest() || r_idx != quests.getGenus() )
			return false;

		if (monsterRace.currentCount >= ( quest.count.max - quest.count.current ) )
			return false;
	}

	/* Powerful monster */
	if (monsterRace.level > player.dungeonLevel){

		cave.rating += (monsterRace.level - player.dungeonLevel) * (monsterRace.is( "UNIQUE" )?2:1);

		if( debug.hear )
			view.message( "Deep " + (monsterRace.is( "UNIQUE" )?"Unique":"Monster") + monsterRace.name );
	}

	/* Access the location */
	var cell = cave[x][y];

	/* Make a new monster */
	var monster = new monsterPrototype();
	monster.r_idx = r_idx;
	monster.generation = 1;
	monster.confused = 0;
	monster.distance = 0;
	monster.visible = false;
	monster.stunned = 0;
	monster.energy = rng.base0.random(100)+900;
	monster.speed = monsterRace.speed;
	monster.sleep = 0;
	monster.flags = 0;
	monster.fear = 0;
	monster.y = y;
	monster.x = x;
	monster.symbol = monsterRace.symbol;
	monster.name = monsterRace.name;

	monster.faction = charm?ALLY_PLAYER:ALLY_NO;	

	if(monsterRace.has( "NEUTRAL" ))
		monster.faction = ALLY_SELF;

	cell.monster = monster;

	/* Enforce sleeping if needed */
	if (sleep && monsterRace.sleep)
		monster.sleep = (monsterRace.sleep * 2) + rng.base1.random(monsterRace.sleep * 10);

	/* Assign maximal hitpoints */
	if (monsterRace.has( "FORCE_MAXHP" ))
		monster.maxhp = rng.dice.cheat.rollString( monsterRace.hitdice );
	else
		monster.maxhp = rng.dice.rollString( monsterRace.hitdice );

	/* And start out fully healthy */
	monster.hp = monster.maxhp;

	if(!(monsterRace.has( "UNIQUE" ))){
		i = extract_energy[monsterRace.speed] / 10;
		if(i>0) monster.speed += rng.spread(0, i);
	}

	/* Force monster to wait for player */
	if(monsterRace.has( "FORCE_SLEEP" )){
		monster.flags = MFLAG_NICE;
		//TODO, I dont like mystery globals
		population.repairNeeded = true;
	}

	monster.flags |= (MFLAG_BORN);

	/* Update the monster */
	monster.update( true );

	/* Hack -- Count the monsters on the level */
	monsterRace.currentCount++;

	/* Hack -- Count the number of "reproducers" */
	if (monsterRace.has( "MULTIPLY" )) population.reproducers++;

	/* Hack -- Notice new multi-hued monsters */
	if (monsterRace.has( "ATTR_MULTI" )) population.shifters = true;
	
	population.push( monster );


	/* Hack - Satan's disintegrating powers have created a circular area for him*/
	//TODO : this needs a separate function!!
	if( player.dungeonLevel == DIS_END-1 ){

	x1 = x;
	y1 = y;
	
	/* Big area of affect */
	for (ly = (y1 - 15); ly <= (y1 + 15); ly++)
	{
		for (lx = (x1 - 15); lx <= (x1 + 15); lx++)
		{
			/* Skip illegal grids */
			if (!cave.inBounds(lx, ly)) continue;
			
			/* Extract the distance */
			k = Math.distance(y1, x1, ly, lx);
			
			/* Stay in the circle of death */
			if (k >= 16) continue;
			
			/* Dont Delete the monster, only Satan should be there */
			/* delete_monster(ly, lx); */
			
			/* Destroy valid grids */
			if (cave_valid_bold(ly, lx))
			{
				/* Delete objects */
				delete_object(ly, lx);
				
				/* Access the grid */
				cell = cave[lx][ly];
				
				/* Create floor */
				cell.feat = FEAT_FLOOR;
				
				/* No longer part of a room or vault */
				cell.info &= ~(CAVE_ROOM | CAVE_ICKY);
				
				/* No longer illuminated or known */
				cell.info &= ~(CAVE_MARK | CAVE_GLOW);
			}/* End of fixing cave*/
		}/* End of x */
	}/* End of y */ 
	}/* End of Satan code */	

	/* Success */
	return (true);
}


function isShieldedLevel( lvl ){
  return ((lvl > DIS_START) && (lvl < DIS_END  ))
}

function _inBounds(x,y){
	return(((y) > 0) && ((x) > 0) && ((y) < cave.currentHeight-1) && ((x) < cave.currentWidth-1))
}


/*Choose a monster race that seems "appropriate" to the given level
 *
 * This function uses the "prob2" field of the "monster allocation table",
 * and various local information, to calculate the "prob3" field of the
 * same table, which is then used to choose an "appropriate" monster, in
 * a relatively efficient manner.
 *
 * Note that "town" monsters will *only* be created in the town, and
 * "normal" monsters will *never* be created in the town, unless the
 * "level" is "modified", for example, by polymorph or summoning.
 *
 * There is a small chance (1/50) of "boosting" the given depth by
 * a small amount (up to four levels), except in the town.
 *
 * It is (slightly) more likely to acquire a monster of the given level
 * than one of a lower level.  This is done by choosing several monsters
 * appropriate to the given level and keeping the "hardest" one.
 *
 * Note that if no monsters are "appropriate", then this function will
 * fail, and return zero, but this should *almost* never happen. */

function _pickMonsterRace( monsterLevel ){
	
  var i, j, p;
  var r_idx;
  var total;
  
  /* Shielded levels only contain monsters of that depth */
  var shieldedLevel = isShieldedLevel( player.dungeonLevel )

  if( shieldedLevel )
    monsterLevel = player.dungeonLevel;  /* Just to be certain */

  /* Try twice to boost the level if we are not on a shielded level or in town 
	 * Boost can be up to +10 in worst case as we do this twice and cap at 5*/
  if (monsterLevel > 0 && !shieldedLevel)
    for( i in [1,2] )
      if (rng.base0.random(50) == 0){
        var levelBoost = monsterLevel >> 2 + 2;
        monsterLevel += ((levelBoost < 5) ? levelBoost : 5);
      }

  /* Reset total */
  total = 0;

  /* Process probabilities */
  for (i = 1; i < monsters.length; i++){
  
    /* Monsters are sorted by depth */
    if (monsters[i].level > monsterLevel) break;

    /* Hack -- No town monsters in dungeon */
    if ((monsterLevel > 0) && (monsters[i].level <= 0)) continue;

    /* Default the calculated probability*/
    monsters[i].finalProbability = 0;

    /* Access the actual race */
    var monsterRace = monsters[i];
    
    /* Hack -- "unique" monsters must be "unique" 
    TODO : this looks wrong to me.., should be 1 line*/
    if ((monsterRace.is( "UNIQUE" )) &&
      (monsterRace.count.current >= monsterRace.count.max))
        continue;
    
    /* Hack? -- no NO_SPAWN monsters */
    if ( (monsterRace.has( "NO_SPAWN" )) )
      continue;

    /* Hack -- shielded levels have no monsters from other depths*/
    if( shieldedLevel && monsterRace.level != player.dungeonLevel )
      continue;
    
    /* Hack non shielded levels have no monsters from shielded depths */
    if( !shieldedLevel && ( monsterRace.level > DIS_START && monsterRace.level < DIS_END ) )
      continue;
        
    /*Hack levels after Dis, should have no monsters from before Dis*/
    if( player.dungeonLevel >= DIS_END && monsterRace.level < DIS_END )
      continue;

    /* Total */
    total += monsters[i].localProbability;
    
    /* Accept */
    monsters[i].finalProbability = total;
  }

  /* No legal monsters */
  if (total <= 0) return (0);

  /* Pick a monster */
  i = monsters.getByProbability( rng.base0.random(total) ).index;
  
  /* Power boost dice throw*/
  p = rng.base0.random(100);

  /* Try for a "harder" monster once (50%) or twice (10%) */
  if (p < 60){

    j = monsters.getByProbability( rng.base0.random(total) ).index;
    if (monsters[j].level > monsters[i].level) i = j;
  }
  /* Try for a "harder" monster twice (10%) */
  if (p < 10){
    
    j = monsters.getByProbability( rng.base0.random(total) ).index;
    if (monsters[j].level > monsters[i].level) i = j;
  }
  
  /* Result */
  return (i);
}


function _initialize(){
  cave.dungeonReady = false;
  /* Mega-Hack -- no panel yet */
  cave.panelRowMin = 0;
  cave.panelRowMax = 0;
  cave.panelColMin = 0;
  cave.panelColMax = 0;
  /* Reset the monster generation level */
  cave.monsterLevel = population.level = player.dungeonLevel;
  /* Reset the object generation level */
  cave.objectLevel = loot.level = player.dungeonLevel;

  for (var x = 0; x < MAX_WID; x++)
    for (var y = 0; y < MAX_HGT; y++){
      cave[x][y].feat = FEAT_NONE; //FEAT_NONE;//FEAT_FLOOR;
      cave[x][y].info = 0; //Hmmmm
  }
}

//cave _floor_bold . cave[x,y].isLevel()
//Level means flat in this case
function _isLevel( x , y ){
  return (((!(this.feat & 0x20)) && (!(this.feat == FEAT_BUSH))) || (this.feat == FEAT_WATER));
}


//cave_ empty_bold . !cave[x,y].isOccupied()
// Line 1 -- forbid doors, rubble, seams, walls
// Line 2 -- forbid normal monsters
// Line 3 -- forbid the player
function _isOccupied(x,y){
  return !( this.isLevel(x,y) && this.monster == null && ( player.x != x || player.y !=y ) );
}


//cave _clean_bold . cave[x,y].isClean()
function _isClean(x,y){
  return ( this.feat == FEAT_FLOOR && this.objects.length == 0 );
}


//cave _naked_bold . cave[x,y].isEmpty()
// Line 1 -- forbid non-floors
// Line 2 -- forbid normal objects
// Line 3 -- forbid normal monsters
// Line 4 -- forbid the player
function _isEmpty(x,y){
	return ( this.feat == FEAT_FLOOR && this.objects.length == 0 && this.monster == null && ( player.x != x || player.y !=y ) );
}

/* Can a monster be placed somewhere ?
   This code used to be all over the place and has been centralized now
   since I realized that I will probably add more types of terrain
*/
function _canPlaceMonster( x , y , r_idx){

	/* Verify location */
	if (!cave.inBounds(x, y)) return (false);
	/* No walls */
	if(cave[x][y].isOccupied()) return (false); 
	/* Only on water in some cases */
	if( cave[y][x].feat == FEAT_WATER  && monsters[r_idx].canCross( FEAT_WATER ) )return (false);
	return (true);
}



function createCell(){

  var cell   = new Object();
  cell.info  = 0; /* Byte Hack -- cave flags */
	cell.feat  = 0; /* Byte Hack -- feature type */
	cell.objects = []; /* object(s) in this cell */
	cell.monster = null; /* monster in this cell */
  cell.isEmpty = _isEmpty;
  cell.isLevel = _isLevel;
  cell.isOccupied = _isOccupied;
  cell.isClean = _isClean;
  return cell;

}
