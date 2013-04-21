/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

var SMALL_LEVEL       = 3;      /* 1/chance of smaller size dungeon(3) */
var SAFE_MAX_ATTEMPTS = 5000;   /* give up after 5000 attempts */
var MAX_STORES        = 13;     /* 13 stores get generated */

var START_RANDOM = 0;
var START_STAIRS = 1;

var DIS_END = 27;
var DIS_START = 5;


function buildStore( n, yy, xx ){
  var y, x, y0, x0, y1, x1, y2, x2, tmp;

	/* Find the "center" of the store */
	y0 = yy * 9 + 7;
	x0 = xx * 15 + 9;

	/* Determine the store boundaries */
	y1 = y0 - rng.base1.random(2)-1;
	y2 = y0 + rng.base1.random(2)+1;
	x1 = x0 - rng.base1.random(3)-3;
	x2 = x0 + rng.base1.random(3)+3;

	/* Build an invulnerable rectangular building */
	for (y = y1; y <= y2; y++)
		for (x = x1; x <= x2; x++){
			/* Clear previous contents, add "basic" perma-wall, illuminate and know */
			cave[x][y].feat = FEAT_PERM_BUILDING; 
			cave[x][y].info |= (CAVE_GLOW | CAVE_MARK);
		}


	/* Pick a door direction (S,N,E,W) */
	tmp = 0;
	if ((xx > 0) && (yy == 0)) tmp=0;
	if ((xx < 3) && (yy == 3)) tmp=1;
	if ((xx == 0) && (yy < 3)) tmp=2;
	if ((xx == 3) && (yy > 0)) tmp=3;

  if( tmp == 0 ){ y = y2; x = rng.range(x1, x2); }
  if( tmp == 1 ){ y = y1; x = rng.range(x1, x2); }
  if( tmp == 2 ){ x = x2; y = rng.range(y1, y2); }
  if( tmp == 3 ){ x = x1; y = rng.range(y1, y2); }

	/* Clear previous contents, add a store door */
	cave[x][y].feat = FEAT_SHOP_HEAD + n;
	stores[n].x = x;
	stores[n].y = y;
}



function townGenHack(){
/*
* Generate the "consistent" town features, and place the player
*2
* Hack -- play with the R.N.G. to always yield the same town
* layout, including the size and shape of the buildings, the
* locations of the doorways, and the location of the stairs.
*/

  var y, x, k, n;
  var dummy = 0;

  var rooms = new Array();

  //Hack -- Use the "simple" RNG 
	rng.quick = true;
	//Hack -- Induce consistant town layout 
	player.townSeed = rng.base0.random(0x10000000);
	rng.current     = player.townSeed; 
	rng.seed( rng.current );

	/* Prepare an Array of "remaining stores", and count them */
	for ( n = 0; n < MAX_STORES-1; n++ )
    rooms[n] = n;

	/* Place four rows of stores */
	for ( y = 0; y < 4; y++ )
		/* Place four stores per row */
		for (x = 0; x < 4; x++)
			/* Only put buildings round the edge of the square */
			if ((x == 0) || (x == 3) || (y == 0) || (y == 3)){
				/* Pick a random unplaced store */
				k = rng.base0.random(n);
				/* Build that store at the proper location */
				buildStore( rooms[k] , y , x );
        /* Shift the stores down, remove one store */
				rooms[k] = rooms[--n];
			}


	/* Place a few trees... */
	for(n=0; n < rng.range(5,10); n++){
		x = rng.range(17,46);
		y = rng.range(12,29);
		/* Clear previous contents, add tree and memorize*/
		cave[x][y].feat = FEAT_TREE;
		cave[x][y].info |= (CAVE_MARK);
	}

	/* ...and a few bushes */
	for(n=0; n < rng.range(5,10); n++){
		x = rng.range(17,46);
		y = rng.range(12,29);
		/* Clear previous contents, add bush and memorize*/
		cave[x][y].feat = FEAT_BUSH;
		cave[x][y].info |= (CAVE_MARK);
	}


	/* Place the stairs */
	while (dummy < SAFE_MAX_ATTEMPTS){
		dummy++;

		/* Pick a location within the central area */
		y = rng.range(12, 29);
		x = rng.range(17,46);

		/* Require a "naked" floor grid */
		if (cave[x][y].isEmpty()) break;
	}

	if (dummy >= SAFE_MAX_ATTEMPTS){
		if (debug.room){
			debug.log("Warning! Could not place stairs!");
		}
	}

	/* Clear previous contents, add down stairs and memorize*/
	cave[x][y].feat = FEAT_MORE;
	cave[x][y].info |= (CAVE_MARK);

	/* Hack -- use the "complex" RNG */
	rng.quick = false;

	/* If the player teleported (or recalled) in, place them randomly */
	if(player.cameFrom == START_RANDOM){
		newPlayerSpot();
	}
	/* If the above was not used, x and y will be the stair location */
	else if(player.cameFrom == START_STAIRS){
		player.x = x;
    player.y = y;
	}
}

function townGen(){

  var i, y, x;

  /* Hack -- Start with basic floors */
  for (y = 0; y < cave.currentHeight-1; y++)
    for (x = 0; x < cave.currentWidth-1; x++)
      cave[x][y].feat = FEAT_FLOOR;

  /* Clear previous contents, add "solid" perma-wall */

  /* Special boundary walls -- Top & Bottom*/
  for (x = 0; x < cave.currentWidth; x++)
    cave[x][0].feat = cave[x][cave.currentHeight-1].feat = FEAT_PERM_SOLID;

   /* Special boundary walls -- Left & Right*/
  for (y = 0; y < cave.currentHeight; y++)
    cave[0][y].feat = cave[cave.currentWidth-1][y].feat = FEAT_PERM_SOLID;

  /* Hack -- Build the buildings/stairs (from memory) */
  townGenHack();

  /* Day Light */
  if ( (player.turn % (10 * TOWN_DAWN)) < ((10 * TOWN_DAWN) / 2) ){
    /* Lite up the town */
    for (y = 0; y < cave.currentHeight; y++)
      for (x = 0; x < cave.currentWidth; x++)
      {
        cave[y][x].info |= (CAVE_GLOW); /* Perma-Lite */
        if (_options.viewPermaGrids) 
          cave[y][x].info |= (CAVE_MARK); /* Memorize */
      }

    //replace_all_friends();

    /* Make some day-time residents */
    for (i = 0; i < 4 /*MIN_M_ALLOC_TD*/ ; i++)
      cave.placeMonsters(3, true);
  } else {
    /* Night Time */
    /* Make some night-time residents */
    for (i = 0; i < 8 /*MIN_M_ALLOC_TN*/; i++)
      cave.placeMonsters(3, true);
  }
}






function generateCave(){

  var i, num, tester1, tester2;
  var okay;

  player.old.x = player.x;
  player.old.y = player.y;


  /* The dungeon is not ready */
  cave.dungeonReady = false;

  /* Generate */
  for (num = 0; true; num++){

    okay = true;

    player.x = player.old.x;
    player.y = player.old.y;

    /* XXX XXX XXX XXX */
    loot.count = 1;

    cave.initialize();

		/* Reset the monster generation level */
		cave.monsterLevel = player.dungeonLevel;

		/* Reset the object generation level */
		cave.objectLevel = player.dungeonLevel;

    /* Nothing special here yet */
    cave.goodItemFlag = false;

    /* Nothing good here yet */
    cave.rating = 0;

    /* Build the town */
    if (player.dungeonLevel == 0)
    {
      /* Small town */
      cave.currentHeight = (2*SCREEN_HGT);
      cave.currentWidth  = (SCREEN_WID);

      /* Determine number of panels */
      cave.maxPanelRows = (cave.currentHeight / SCREEN_HGT) * 2 - 2;
      cave.maxPanelCols = (cave.currentWidth  / SCREEN_WID) * 2 - 2;

      /* Assume illegal panel */
      cave.panelRow = cave.maxPanelRows;
      cave.panelCol = cave.maxPanelCols;

      townGen();

    }else{

      /* Build a real level */
      /* Sometimes build a small dungeon */
      if ( randDice( SMALL_LEVEL ) == 1 || alwaysSmallDungeons ){

        if (debug.room)
          debug.log ("A 'small' dungeon level.");

        cave.currentHeight = randDice(MAX_HGT/SCREEN_HGT) * SCREEN_HGT;
        cave.currentWidth  = randDice(MAX_WID/SCREEN_WID) * SCREEN_WID;

        if (debug.room)
          debug.log( "Max panel columns (X) : " + cave.maxPanelCols + " , Max panel rows (Y) : " + cave.maxPanelRows );

      }else{

        /* Big dungeon */
        cave.currentHeight = MAX_HGT;
        cave.currentWidth = MAX_WID;
      
      }

      /* Determine number of panels */
      cave.maxPanelRows = (currentHeight / SCREEN_HGT) * 2 - 2;
      cave.maxPanelCols = (currentWidth / SCREEN_WID) * 2 - 2;

      /* Assume illegal panel */
      cave.panelRow = cave.maxPanelRows;
      cave.panelCol = cave.maxPanelCols;

      /* Make a dungeon */
      if (!generateDungeon())
      {
        why = "could not place player";
        okay = false;
      }
    }


		var rating = cave.rating;
		var feeling = 0;

    /* Extract the feeling */
    if (rating > 100) feeling = 2;
    else if (rating > 80) feeling = 3;
    else if (rating > 60) feeling = 4;
    else if (rating > 40) feeling = 5;
    else if (rating > 30) feeling = 6;
    else if (rating > 20) feeling = 7;
    else if (rating > 10) feeling = 8;
    else if (rating > 0) feeling = 9;
    else feeling = 10;

		cave.feeling = feeling;

    /* Hack -- Have a special feeling sometimes */
    if ( cave.goodItemFlag && !_options["preserveMode"].value ) feeling = 1;


    /* Hack -- no feeling in the town */
    if (player.dungeonLevel <= 0) feeling = 0;

    //We dont care about friends for now
    //replace_all_friends();

    /* Prevent object over-flow */
    if ( loot.count >= loot.maximumCount )
    {
      /* Message */
      why = "too many objects";

      /* Message */
      okay = false;
    }

    /* Prevent monster over-flow */
    if ( population.count >= population.maximumCount )
    {
      /* Message */
      why = "too many monsters";

      /* Message */
      okay = false;
    }

    /* Mega-Hack -- "auto-scum" */
    if ( _options["auto_scum"].value && (num < 100))
    {
      /* Require "goodness" */
      if ((feeling > 9) ||
        ((player.dungeonLevel >= 5) && (feeling > 8)) ||
        ((player.dungeonLevel >= 10) && (feeling > 7)) ||
        ((player.dungeonLevel >= 20) && (feeling > 6)) ||
        ((player.dungeonLevel >= 40) && (feeling > 5)))
      {
        /* Give message to debuggers */
        if ( debug.room || debug.hear || debug.peek || debug.xtra )
          why = "boring level";

        /* Try again */
        okay = false;
      }
    }

    /* Accept */
    if (okay) break;


    /* Message */
    if (why) debug.log("Generation restarted: " +  why);

    /* Wipe the objects */
    //wipe_o_list();

    /* Wipe the monsters */
    //remove_non_pets();
  }


  /* The dungeon is ready */
  cave.dungeonReady = true;

  /* Remember when this level was "created" */
  player.old.turn = player.turn;






//I day I will know why the legacy code would clear the grid again..
//  for (x = 0; x < MAX_WID; x++)
//    for (y = 0; y < MAX_HGT; y++){
//      cave[x][y].feat = FEAT_NONE;
//      cave[x][y].info = 0; //Hmmmm
//  }

}

/*
* Returns random co-ordinates for player/monster/object
*/
function newPlayerSpot(){
	var x, y;
	var max_attempts = 5000;
	/* Place the player */
	while (max_attempts--){
		/* Pick a legal spot */
		y = rng.range(1, cave.currentHeight - 2);
		x = rng.range(1, cave.currentWidth - 2);

		/* Must be a "naked" floor grid */
		if (!cave[x][y].isEmpty()) continue;

		/* Refuse to start on anti-teleport grids */
		if (cave[y][x].info & (CAVE_ICKY)) continue;
		
		/*Refuse to place the player in the center of Satan's level*/
		if( player.dungeonLevel == DIS_END - 1 ){
			if( x > (cave.currentWidth>>3) && 
				x < cave.currentWidth - (cave.currentWidth>>3) && 
				y > (cave.currentHeight>>3) && 
				y < cave.currentHeight - (cave.currentHeight>>3) )
				continue;
		}

		/* Done */
		break;

	}

	if (max_attempts < 1) /* Should be -1, actually if we failed... */
		return false;


	/* Save the new player grid */
	player.y = y;
	player.x = x;
	return true;
}
