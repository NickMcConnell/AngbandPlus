/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/


/*
* Approximate Distance between two points.
*
* When either the X or Y component dwarfs the other component,
* this function is almost perfect, and otherwise, it tends to
* over-estimate about one grid per fifteen grids of distance.
*
* Algorithm: hypot(dy,dx) = max(dy,dx) + min(dy,dx) / 2
*/

Math.distance = distance;

function distance (x1, y1, x2, y2){

  var dy, dx, d;

	/* Find the absolute y/x distance components */
	dy = (y1 > y2) ? (y1 - y2) : (y2 - y1);
	dx = (x1 > x2) ? (x1 - x2) : (x2 - x1);

	/* Hack -- approximate the distance */
	d = (dy > dx) ? (dy + (dx>>1)) : (dx + (dy>>1));

	/* Return the distance */
	return (d);
}


// String trim..
String.prototype.trim = function(){ return this.replace(/^\s\s*/, '').replace(/\s\s*$/, ''); };
//Duh.. meant for substrings
String.prototype.isVowel = function(){ var u = this.toUpperCase(); return ( u=="A"||u=="E"||u=="I"||u=="O"||u=="U" ) }
//Add + or -, corner case 0 becomes +0
String.prototype.sign = function( n ){ return (n<0?"-":"+")+n }
//Swap the content linked to the keys
Math.swap = function( a ,b ){ var tmp = this[a]; this[a] = this[b] ; this[b] = tmp; }

//Array.prototype.clear = function(){ while(this.length>1)this.pop();  }

//Array.prototype.clear.DontEnum = true;

