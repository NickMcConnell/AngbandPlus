/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/


var cmd = new Object();

function initCommands(){

  cmd.move = _move;

  player.cmd = cmd;
}

function _move( xDelta, yDelta ){
  if( !cave[player.x + xDelta][player.y + yDelta].isOccupied() ){
    player.x = player.x + xDelta;
    player.y = player.y + yDelta;
    view.showCave();
  }
}

