/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

//Global Objects

var nextFunction;
var currentEvent;

//Private Objects

var _upKeys;

//Initialization

//no initialization
function initKeyboard(){
  _upKeys = new Object();
  _upKeys[40] = "Down Arrow"
  _upKeys[39] = "Right Arrow"
  _upKeys[38] = "Up Arrow"
  _upKeys[37] = "Left Arrow"
  _upKeys[27] = "Escape"
}

//Public Functions

//Rationale :
//99% of the times we just need the keycode
//if the listener needs more it can access the global currentEvent

function processUpKeyEvent(event)
{
	currentEvent = event;
	currentEvent._code = event.which?event.which:event.keyCode;
	//If the assignment works, must be a special key
	//Hack, do not 'fix' !!
	if( _upKeys[ currentEvent._code ] !== undefined ){ 
		currentEvent._char = _upKeys[ currentEvent._code ]
		//debug.set( currentEvent._char );
		if(nextFunction){
			nextFunction( currentEvent._code );
			return false;
		}
	}
	return true;
}

function processKeyEvent(event){
	currentEvent = event;
	currentEvent._code = event.which?event.which:event.keyCode;
	currentEvent._char = getKeyChar( currentEvent._code );
	if( !_upKeys[ currentEvent._code ] ){
	//debug.set( currentEvent._code );
		if(nextFunction){
			nextFunction( currentEvent._code );
			return false
		}
	}
	return true;
}

//Rationale :
//-1 is up, +1 down
function getVerticalDirection( keyCode){
  if( keyCode == 38 || keyCode == 46 )return -1;
  if( keyCode == 40 || keyCode == 50 )return +1;
  var s = currentEvent._char;
  if( s == "2" )return -1;
  if( s == "8" )return +1;
  return 0;
}

//Rationale :
//13 is enter, 32 is space
function isAcceptance( keyCode ){
  return ( keyCode==13 || keyCode==32 )
}

function getKeyChar( keyCode){
  return String.fromCharCode( keyCode );
}

