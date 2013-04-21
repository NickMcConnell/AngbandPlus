/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

//Global Objects

var debug;

//Private Objects

//no private objects

//Initialization

function initDebug(){
  debug = document.getElementById("debug");
  debug.log = _logMessage;
  debug.set = _setMessage;
  debug.room = true; //We want to know about room creation
  debug.hear = true; 
  debug.peek = true;
  debug.xtra = true;
}

//Public Functions

//no public functions

//Private Functions

  function _logMessage( message ){
    debug.innerHTML = debug.innerHTML + message + "<br>"
  }

  function _setMessage( message ){
    debug.innerHTML = message + "<br>"
  }

