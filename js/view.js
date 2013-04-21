/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

//Global Objects

var view;          //The js objects that is accessed by all other source

//Private Objects

var _console;       //The js object that is the actual HTML element
var _cursor;        //Location of cursor, only for writing purposes, not display
var _rows;          //Size of view in rows
var _columns;       //Size of view in columns

//Shortcut, for speed purposes
var cx = 0;  //Cursox x
var cy = 0;  //Cursor y
var ox = 0;  //offset x
var oy = 0;  //offset y

//For use with view.items only
var USE_EQUIP = 0x01;
var USE_INVEN = 0x02;
var USE_FLOOR = 0x04;

//The colors of Hellband..
var TERM_DARK    = 0  /* 'd' rgb: 0,0,0 */
var TERM_WHITE   = 1  /* 'w' rgb: 4,4,4 */
var TERM_SLATE   = 2  /* 's' rgb: 2,2,2 */
var TERM_ORANGE  = 3  /* 'o' rgb: 4,2,0 */
var TERM_RED     = 4  /* 'r' rgb: 3,0,0 */
var TERM_GREEN   = 5  /* 'g' rgb: 0,2,1 */
var TERM_BLUE    = 6  /* 'b' rgb: 0,0,4 */
var TERM_UMBER   = 7  /* 'u' rgb: 2,1,0 */
var TERM_L_DARK  = 8  /* 'D' rgb: 1,1,1 */
var TERM_L_WHITE = 9  /* 'W' rgb: 3,3,3 */
var TERM_VIOLET  = 10 /* 'v' rgb: 4,0,4 */
var TERM_YELLOW  = 11 /* 'y' rgb: 4,4,0 */
var TERM_L_RED   = 12 /* 'R' rgb: 4,0,0 */
var TERM_L_GREEN = 13 /* 'G' rgb: 0,4,0 */
var TERM_L_BLUE  = 14 /* 'B' rgb: 0,4,4 */
var TERM_L_UMBER = 15 /* 'U' rgb: 3,2,1 */

//Initialization

function initView(){

	var row,col;
  _console = document.getElementById("view");
  _columns=80
  _rows=27
  _cursor = new Object();
  view = new Object();
  for(row=0;row<_rows;row++)
      view[row]=new Array();
  view.clear = _clearView;
	view.clear.line = _clearLine;
	view.clear.line.fromCursor = _clearLineFromCursor;
  view.draw  = _drawView;
  view.print = _putString;
  view.printReversed = _putStringReverse;
  view.printCentered = _putCenteredString;
  view.cursor = _cursor;
  view.cursor.set = _setCursor;
  view.cursor.advance = _advanceCursor;
  view.cursor.retreat = _retreatCursor;
  view.showCave = _showCave;
	view.showInventory = _showInventory;
	view.showEquipment = _showEquipment
  view.clear();
  view.message = function( message ){};
  putIntro();
  view.draw();
	view.stack = [];
	view.push = _pushView;
	view.pop = _popView;
	view.items = [];
	view.items.show = USE_INVEN;
	view.items.showEmptySlots = true;
	view.items.filter = {};
	view.items.filter.type = 0;
	view.items.filter.userMagic = false;
	view.items.filter.hook = null;
	view.commandGap = 50;

	view.updateRequests = 0;
}

//Public Functions

//Public functions


function _pushView(){

	var viewClone = {};

	for(var row=0;row<_rows;row++)
      viewClone[row]=new Array();
  for(var col=0;col<_columns;col++)
    for(var row=0;row<_rows;row++)
      viewClone[row][col]=view[row][col];

	view.stack.push( viewClone );
}

function _popView(){

	var viewClone = view.stack.pop();

  for(var col=0;col<_columns;col++)
    for(var row=0;row<_rows;row++)
      view[row][col]=viewClone[row][col];
}

//Private Functions

function _clearView(){
  cx = 0;
  cy = 0;
  for(var col=0;col<_columns;col++)
    for(var row=0;row<_rows;row++)
      view[row][col]='&nbsp;';
}

function _clearLine(){

  for(var col=0;col<_columns;col++)
    view[cy][col]='&nbsp;';
}

function _clearLineFromCursor(){

  for(var col=cx;col<_columns;col++)
    view[cy][col]='&nbsp;';
}


//TODO: This could be way more efficient..
function _drawView(){
  var s = "";

  for(var row=0;row<_rows;row++)
    s = s + view[row].join("") + "<br>"

  _console.innerHTML = s;
  cx = 0;
  cy = 0;
}

function _putString( s ){
  var ta = (s + "").split("");
  for(var position=0;position<ta.length;position++){
    var c = ta[position];
    view[cy][cx] = (c==' '?'&nbsp;':c);
    view.cursor.advance();
  }
}

function _putStringReverse( s ){
  s = (s + "").reverse(); //Stringify and reverse
  for(var position=0;position<s.length;position++){
    var c = s.charAt(position);
    view[cy][cx] = (c==' '?'&nbsp;':c);
    view.cursor.retreat();
  }
}

function _putCenteredString( s ){
  var col = Math.ceil( ( _columns - s.length ) / 2 );
  _setCursor( col , cy );
  _putString( s );
}

function _setCursor( column , row ){
  cx = column;
  cy = row;
}

function _advanceCursor(){
  cx++;
  if( cx == _columns ){
    cx = 0;
    cy++;
  }
  if( cy == _rows ){
    cy = 0;
  }
}

function _retreatCursor(){
  cx--;
  if( cx == -1 ){
    cx = _columns-1;
    cy--;
  }
  if( cy < 0 ) cy = 0;
}

function _showFloor()
{
	list = cave[player.x][player.y].objects;
	//Index the pack
	var index = 0;
	for( var key in list )
	{
		if( list[key].item === undefined ) continue;
		list[key].alpha = alphabet[index];
		index++;
	}
	//Show the pack
	_showObjects( list );	
}

function _showInventory(){
	//Sort the pack
	gear.pack.sort( function( a , b )
	{
		var isItemA = a.item !== undefined;
		var isItemB = a.item !== undefined;
		//Some attributes are not items, sort them at the bottom
		if( !isItemA && !isItemB )return 0;
		if( !isItemA ) return -1;
		if( !isItemB ) return 1;
		//Calculate TVAL location, higher key means lower index
		var diff = itemTypes.getKey( b.item.type ) - itemTypes.getKey( a.item.type );
		//If we know, return the result
		if( diff != 0 ) return diff;
		//Otherwise return the subtype difference
		return b.item.subtype - a.item.subtype;
	} );
	//Index the pack
	var index = 0;
	for( var key in gear.pack )
	{
		if( gear.pack[key].item === undefined ) continue;
		gear.pack[key].alpha = alphabet[index];
		index++;
	}
	//Show the pack
	_showObjects( gear.pack );	
}

function _showEquipment(){
	_showObjects( gear );
}


function _showObjects( list )
{
	
	var row = 1;
	var gizmo, col, len, lim, i, l;
	var key;

	len = 79 - view.commandGap;         // Maximal length
	lim = 79 - 3 - ( 14 + 2 ) - 9 - 2;  // Require space for descriptions + labels + weights + equippy chars

	for( key in list ){
	
		if( list[key].item === undefined )
			continue;

		if( list[key].item.number === undefined   ){
			l = "(nothing)".length + (2 + 3) + (14 + 2) + 9 +2;
  		if( l > len ) len = l;
			continue;
		}

		gizmo = list[key].item;
		gizmo.displayName  = gizmo.describe( true, 3 );
		gizmo.displayColor = 'green';

		//Extract the optimal length with labels, weights and equippy chars
		l = gizmo.displayName.length + (2 + 3) + 9 +2;
		//Hack, optimal length is 16 smaller if there is no slot description
		if( list[key].description !== undefined ) l=l+14+2;
		if( l > len ) len = l;
	}

	//Hack -- Find a column to start in
	col = (len > 76) ? 0 : (79 - len);

	for( key in list )
	{
		if( list[key].item === undefined )
			continue;

		if( list[key].item.number === undefined )
			gizmo = { displayName : "(nothing)" , weight : 0 , number : 0 , symbol : '&' , display : gizmoPrototype.prototype.display }
		else
			gizmo = list[key].item;

		if( !gizmo.display.ok() )
			continue;

		view.cursor.set( col-2 , row );
		view.cursor.color = gizmo.displayColor;
		view.clear.line.fromCursor();
		view.print( list[key].alpha + ") " + gizmo.symbol + " " + (list[key].description || "") );
		if( list[key].description !== undefined )
			view.cursor.set( col+19 , row );

		view.print( ": " + gizmo.displayName );
		view.cursor.set( 71 , row );
		var weight  = gizmo.weight * gizmo.number;
		var pounds  = Math.floor( weight / 10 );
		var ounces  = weight % 10; 
		view.print( pounds + "." + ounces + " lb" );

		row++;
	}

	view.cursor.set( col , row );
	view.clear.line.fromCursor();

	// Save the new column
	view.commandGap = col;
}



function _showCave(  ){

  var s    = "";
  var cell = 0;
  var x = 0;
  var y = 0;


  //going east or south
  if( ox + _columns - player.x < 11 ) ox++;
  if( oy + _rows    - player.y < 11 ) oy++;

  if( player.y - oy < 11 ) oy--;
  if( player.x - ox < 11 ) ox--;


  //Bounding east and south
  if( ox + _columns > cave.currentWidth  ) ox = cave.currentWidth  - _columns;
  if( oy + _rows    > cave.currentHeight ) oy = cave.currentHeight - _rows;

  //Bounding west and north
  if( ox < 0 ) ox = 0;
  if( oy < 0 ) oy = 0;

  try{
  for( y = 0 ; y < _rows ; y++ ){
		for( x = 0 ; x < _columns ; x++ ){
			if ( cave[x + ox][y + oy].monster != null )
				view[y][x] =  cave[x + ox][y + oy].monster.symbol;
			else
				view[y][x] =  features[ cave[x + ox][y + oy].feat ].symbol ;
   }
  }
  }catch( e ){
    prompt( (x+ox) + "  " + (y+oy) );
  }

  view[player.y-oy][player.x-ox] = "@";
    
  view.draw();

}
