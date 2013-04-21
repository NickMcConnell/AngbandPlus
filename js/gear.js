/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

//Global Objects

var gear; //This will be attached to the player


//Private Objects


//Initialization

//no initialization
function initGear(){
  gear = new Object();
  gear.pack    = new Array();
	gear.pack.description = "In pack";
	gear.pack.descriptionVerb = "carrying in your pack";
  gear.wield   = new slot( "a" , "Wielding"      , "attacking monsters with"    );
  gear.bow     = new slot( "b" , "Shooting"      , "shooting missiles with"     );
  gear.left    = new slot( "c" , "On left hand"  , "wearing on your left hand"  );
  gear.right   = new slot( "d" , "On right hand" , "wearing on your right hand" );
  gear.neck    = new slot( "e" , "Around neck"   , "wearing around your neck"   );
  gear.lite    = new slot( "f" , "Light source"  , "using to light the way"     );
  gear.body    = new slot( "g" , "On body"       , "wearing on your body"       );
  gear.outer   = new slot( "h" , "About body"    , "wearing on your back"       );
  gear.arm     = new slot( "i" , "On arm"        , "wearing on your arm"        );
  gear.head    = new slot( "j" , "On head"       , "wearing on your head"       );
  gear.hands   = new slot( "k" , "On hands"      , "wearing on your hands"      );
  gear.feet    = new slot( "l" , "On feet"       , "wearing on your feet"       );
  gear.pouch_1 = new slot( "m" , "In pouch"      , "carrying in a pouch"        );
  gear.pouch_2 = new slot( "n" , "In pouch"      , "carrying in a pouch"        );
  gear.pouch_3 = new slot( "o" , "In pouch"      , "carrying in a pouch"        );
  gear.pouch_4 = new slot( "p" , "In pouch"      , "carrying in a pouch"        );
  gear.pouch_5 = new slot( "q" , "In pouch"      , "carrying in a pouch"        );
  gear.pouch_6 = new slot( "r" , "In pouch"      , "carrying in a pouch"        );

  gear.getEquipmentWeight = function(){
    return gear.wield.getWeight()   + 
           gear.bow .getWeight()    +
           gear.left.getWeight()    +
           gear.right.getWeight()   +
           gear.neck.getWeight()    +
           gear.lite.getWeight()    +
           gear.body.getWeight()    +
           gear.outer.getWeight()   +
           gear.arm .getWeight()    +
           gear.head.getWeight()    +
           gear.hands.getWeight()   +
           gear.feet.getWeight()    +
           gear.pouch_1.getWeight() +
           gear.pouch_2.getWeight() +
           gear.pouch_3.getWeight() +
           gear.pouch_4.getWeight() +
           gear.pouch_5.getWeight() +
           gear.pouch_6.getWeight() +
           gear.bow .getWeight()    +
           gear.left.getWeight();
  }

	gear.getPackWeight = function(){
		var key, sum = 0;
		for( key in gear.pack)
			if( gear.pack[key].weight !== undefined )
				sum = sum + gear.pack[key].weight;
		return sum;
	}

	gear.getItem = _getItem;
}


//Public Functions

var slot = function ( alpha , description  , actionVerb){
	this.item = new Object();
	this.description = description;
	this.actionVerb  = actionVerb;
	this.alpha = alpha;
}

  slot.prototype.getWeight = function(){ return this.item.weight===undefined?0    :this.item.weight;      }
  slot.prototype.get       = function(){ return this.item.get   ===undefined?0    :this.item.get();       }
  slot.prototype.has       = function(){ return this.item.has   ===undefined?false:this.item.has();       }

//Private Functions


/*
 * Let the user select an item, return the selected item where 'where' contains the array it belongs to.
 * if the user cancelled, null is returned.
 *
 * The selected item must satisfy the "gizmos.tester.hook()" function,
 * if that hook is set, and the "gizmos.tester.type", if that value is set.
 *
 * All "item_tester" restrictions are cleared before this function returns.
 *
 * The user is allowed to choose acceptable items from the equipment,
 * inventory, or floor, respectively, if the proper flag was given,
 * and there are any acceptable items in that location.
 *
 * The equipment or inventory are displayed (even if no acceptable
 * items are in that location) if the proper flag was given.
 *
 * If there are no acceptable items available anywhere, and "str" is
 * not NULL, then it will be used as the text of a warning message
 * before the function returns.
 *
 * Note that the user must press "-" to specify the item on the floor,
 * and there is no way to "examine" the item on the floor, while the
 * use of "capital" letters will "examine" an inventory/equipment item,
 * and prompt for its use.
 *
 * Global "player.command_new" is used when viewing the inventory or equipment
 * to allow the user to enter a command while viewing those screens, and
 * also to induce "auto-enter" of stores, and other such stuff.
 *
 * Global "player.command_see" may be set before calling this function to start
 * out in "browse" mode.  It is cleared before this function returns.
 *
 * Global "player.command_wrk" is used to choose between equip/inven/floor
 * listings.  It is equal to USE_INVEN or USE_EQUIP or USE_FLOOR, except
 * when this function is first called, when it is equal to zero, which will
 * cause it to be set to USE_INVEN.
 *
 * We always erase the prompt when we are done, leaving a blank line,
 * or a warning message, if appropriate, if no items are available.
 *
 * Note that the "easy_floor" option affects this function in several ways.
 *
 * Note that only "acceptable" floor objects get indexes, so between two
 * commands, the indexes of floor objects may change.  XXX XXX XXX
 */
 
function _getItem( queryString , failString , locations)
{

		var use = {};
	  use.inventory = locations & USE_INVEN;
	  use.equipment = locations & USE_EQUIP;
	  use.floor     = locations & USE_FLOOR;

	/* Require at least one legal choice */
	if (!use.inventory && !use.equipment && !allow.floor)
	{
		return false;
	}
	else
	{
		// Hack -- Start on equipment if requested
		if ( player.command.work == "")
		{
			if( use.inventory )
				player.command.work = USE_INVEN;
			else if( use.equipment )
				player.command.work = USE_EQUIP;
			else 
				player.command.work = USE_FLOOR;
		}
		else
		{
			if( player.command.work == USE_EQUIP )
				player.command.work = use.equipment?USE_EQUIP:use.inventory?USE_INVEN:USE_FLOOR;
			if( player.command.work == USE_INVEN )
				player.command.work = use.inventory?USE_INVEN:use.equipment?USE_EQUIP:USE_FLOOR;
			if( player.command.work == USE_FLOOR )
				player.command.work = use.floor?USE_FLOOR:use.inventory?USE_INVEN:USE_EQUIP;
		}
	}

		// Update
		player.window |= (PW_INVEN | PW_EQUIP);
		terms.redraw();

		/* Viewing inventory */
		if( player.command_wrk == USE_INVEN )
		{
			view.showInventory();
			s = "Inventory: "
			if( use.equipment ) s = s + " / for Equip,";
			if( use.floor     ) s = s + " - for Floor,";
		}
		/* Viewing equipment */
		else if( player.command_wrk == USE_EQUIP )
		{
			view.showEquipment();
			s =  "Equip:";
			if( use.inventory) s = s + " / for Inven,";
			if( use.floor    ) s = s + " - for floor,";
		}
		/* Viewing floor */
		else
		{
			view.showFloor();
			s = "Floor:";
			if( use.inventory ) s = s + " / for Inven,";
			else if( use.equipment) s = s + " / for Equip,";
		}

		s = s + " ESC";
		view.cursor.set( 1, 0 );
		view.print( s );
		return true;
}

function _getRepeaterItem( )
{
	var o = repeater.getObject();

	if( o != null )
	{
		if( o.where[o.index] !== undefined && o.where[o.index] !== null )
		{
			//Forget the gizmos.tester.type & hook restriction
			gizmos.tester.type = 0;
			gizmos.tester.hook = NULL;
			//a cookie if you can say this fast 10 times in a row
			return { item : o.where[o.index] , list : o.where };
		}
	}
	repeater.clear();
	return null;
}