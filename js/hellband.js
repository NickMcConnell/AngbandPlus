/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

//Global Objects

var PU_BONUS    = 0x00000001; /* Calculate bonuses */
var PU_TORCH    = 0x00000002; /* Calculate torch radius */
var PU_HP       = 0x00000010; /* Calculate chp and mhp */
var PU_MANA     = 0x00000020; /* Calculate csp and msp */
var PU_SPELLS   = 0x00000040; /* Calculate spells */
var PU_UN_VIEW  = 0x00010000; /* Forget view */
var PU_UN_LITE  = 0x00020000; /* Forget lite */
var PU_VIEW     = 0x00100000; /* Update view */
var PU_LITE     = 0x00200000; /* Update lite */
var PU_MONSTERS = 0x01000000; /* Update monsters */
var PU_DISTANCE = 0x02000000; /* Update distances */
var PU_FLOW     = 0x10000000; /* Update flow */

var TOWN_DAWN   = 10000;      /* Number of turns from dawn to dawn XXX */


// Number of grids used to display the dungeon (vertically). Must be a multiple of 11, probably hard-coded to 22.
var SCREEN_HGT = 22;
// Number of grids used to display the dungeon (horizontally). Must be a multiple of 33, probably hard-coded to 66.
var SCREEN_WID = 66;
// Maximum dungeon height in grids, must be a multiple of SCREEN_HGT, probably hard-coded to SCREEN_HGT * 3.
var MAX_HGT    = 66;
// Maximum dungeon width in grids, must be a multiple of SCREEN_WID, probably hard-coded to SCREEN_WID * 3.
var MAX_WID    = 198;

//Maximum LOS
var MAX_SIGHT = 20;

var game = new Object();           //The game object

//The alphabet, all bow before it ;]
var alphabet = "abcdefghijklmnopqrstuvwxyz".split("");
for( var key in alphabet )
	alphabet[alphabet[key]] = key;

//Private Objects

var _yyy;          //xxx

//Initialization

function initHellband(){
  document.hellband = new Object();
  game = new Object();
  
  game.elements = new Object( "fire,cold,electricity,acid,poison,confusion,sound,light,darkness,chaos,drain,shards,nexus,blindness,nether,fear" );
  game.elements.array = game.elements.split(",");
  
  game.stats = new Object( "strength,constitution,dexterity,intelligence,wisdom,charisma" );
  game.stats.array = game.stats.split(",");

  game.effects = new Object( "" );

  game.startUpMode = true;

}

//Public Functions

// Generates a random integer X where O<=X<M
function randInt( M ){
  return Math.floor(Math.random()*(M+1));
}
// Generates a random integer X where 1<=X<=M
function randDice( M ){
  return Math.floor(Math.random()*M+1);
}

// Generates a random integer X where A<=X<=B
function randRange( A , B ){
  return ((A)+(randInt(1+(B)-(A))));
}

// Generates a random integer X where A-D<=X<=A+D
function randSpread( A , D ){
  return ((A)+(randInt(1+(D)+(D)))-(D));
}

//TODO, replace me with rng.dice.roll()
function damRoll( num , sides ){
  var sum = 0;
  for (var i = 0; i < num; i++) 
    sum += randDice(sides);
  return sum;
}

function maxRoll( num , sides ){
  return ( num * sides );
}

//Private Functions

function doYYY(){}

function mainLoop(e){

  if( currentEvent._char == 'Z' ) document.location = 'http://www.google.com/';

  if( currentEvent._char == 'i' )
	{
		//Set the view up
		view.push();
		view.items.show = USE_INVEN;
		view.items.showEmptySlots = gizmos.tester.full = true;
		view.showInventory();
		view.items.showEmptySlots = gizmos.tester.full = false;
		
		//Calculate weight and capacity
		var weight = gear.getPackWeight();
		var pounds = Math.floor( weight / 10 );
		var ounces = weight % 10;
		var percentage = Math.floor( ( weight * 100) / (statAdjustments[ statIndex(player.strength) ][ADJ_WEIGHT] * 50 ) );

		view.cursor.set( 1 , 0 );
		view.print("Inventory: carrying " + pounds + "." + ounces + " pounds (" + percentage + "% of capacity). Command: ");
		view.clear.line.fromCursor();
		view.draw();
		//Discard what we had 
		view.pop();
		return
	}

  if( currentEvent._char == 'e' )
	{
		//Set the view up
		view.push();
		view.items.show = USE_EQUIP;
		view.items.showEmptySlots = gizmos.tester.full = true;
		view.showEquipment();
		view.items.showEmptySlots = gizmos.tester.full = false;
		
		//Calculate weight and capacity
		var weight = gear.getPackWeight();
		var pounds = Math.floor( weight / 10 );
		var ounces = weight % 10;
		var percentage = Math.floor( ( weight * 100) / (statAdjustments[ statIndex(player.strength) ][ADJ_WEIGHT] * 50 ) );

		view.cursor.set( 1 , 0 );
		view.print("Equipment: carrying " + pounds + "." + ounces + " pounds (" + percentage + "% of capacity). Command: ");
		view.clear.line.fromCursor();
		view.draw();
		//Discard what we had 
		view.pop();
		return
	}

	if( currentEvent._char == 'd' )
	{
		//TODO, repeater !!
		gear.getItem( "Drop which item?" , "You have nothing to drop." , USE_EQUIP | USE_INVEN );
		nextFunction = dropLoop;
		itemSelectedFunction = doDrop;
	}

  
  if( e == 37 ){ player.cmd.move( -1 ,  0 ); return; } //37 is left
  if( e == 39 ){ player.cmd.move(  1 ,  0 ); return; } //39 is right
  if( e == 38 ){ player.cmd.move(  0 , -1 ); return; } //38 is up
  if( e == 40 ){ player.cmd.move(  0 ,  1 ); return; } //40 is down

  if( currentEvent._char == '1' ) { player.cmd.move( -1 , +1 ); return; }
  if( currentEvent._char == '2' ) { player.cmd.move(  0 , +1 ); return; }
  if( currentEvent._char == '3' ) { player.cmd.move( +1 , +1 ); return; }

  if( currentEvent._char == '4' ) { player.cmd.move( -1 ,  0 ); return; }
  if( currentEvent._char == '6' ) { player.cmd.move( +1 ,  0 ); return; }

  if( currentEvent._char == '7' ) { player.cmd.move( -1 , -1 ); return; }
  if( currentEvent._char == '8' ) { player.cmd.move(  0 , -1 ); return; }
  if( currentEvent._char == '9' ) { player.cmd.move( +1 , -1 ); return; }


  view.showCave();

  

}

function doDrop( o )
{



}

function dropLoop( e )
{
	var which = currentEvent._char;
	var list;
	var gizmo = null;

	switch( which )
	{
		case "Escape":
		{
			nextFunction = mainLoop;   // Restore listener
			gizmos.tester.type = 0;    // Reset item type filter
			gizmos.tester.hook = null; // Reset item hook filter
			view.showCave();           // Restore dungeon view
			return;                    // Get out
		}
		case '/':
		{
			player.command.work = player.command.work==USE_EQUIP?USE_EQUIP:USE_INVEN;
			break;
		}
		case '-':
		{
			player.command.work = USE_FLOOR;
			break;
		}
		case '0':case '1': case '2': case '3':case '4': case '5': case '6':case '7': case '8': case '9':
		{
			break; //TODO, tags
		}
		case '\n':
		case '\r':
		{
			//TODO, default item ( if only 1 is displayed )
			break;
		}
		default:
		{
			//When the user presses a capital letter, we need 
			var needsVerification = alphabet[which]===undefined && alphabet[which.toLowerCase()]!==undefined;
				which = which.toLowerCase();
			if( which.match("[a-z]") == null )
				return;
			if( player.command.work == USE_EQUIP )list = gear;
			if( player.command.work == USE_INVEN )list = gear.pack;
			if( player.command.work == USE_FLOOR )list = cave[player.x][player.y].objects

			if(needsVerification)
			{
				//TODO
			}

			for( var key in list ){
				if( list[key].alpha !== undefined && list[key].alpha == which )
					gizmo = list[key].item;
			}
			
			if( gizmo != null )
				itemSelectedFunction( { item : gizmo , list : list } );
			
		}
	}
}



//REPEATER FUNCTIONALITY
//TODO: if this grows too much -> put in repeater.js

var repeater = [];

repeater.getObject = function(){

	o = this.pop();
	if( o.object === undefined )
		return null;

	return o.object;
}

repeater.setObject = function( where, index ){

	o = {};
	o.object = {};
	o.object.where = where;
	o.object.index = index;
	this.push( o );

}

repeater.clear = function(){ while(this.length>1)this.pop();  }