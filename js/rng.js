/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

//Global Objects

var rng;
var RAND_DEG = 63; //Dont ask, I wont explain

//Private Objects


//Initialization

//no initialization
function initRNG(){
  rng = new Object();
  rng.unbiased = function(){ return true/*_options["randUnbiased"].value */  };
  rng.quick = false; /*quick mode*/
  rng.current = 0; /*current value*/
  rng.index = 1; /* current index for complex RNG */
  rng.states = new Array(); /*state table for complex RNG */
  rng.seed = _seed;
  rng.base0 = new Object();
  //Generates a random long integer X where O<=X<M.
  rng.base0.random = _random;
  rng.base1 = new Object();
  //Generates a random long integer X where 1<=X<=M.
  rng.base1.random = function(n){ return ( rng.base0.random(n) + 1 ); };
  //Generates a random long integer X where A<=X<=B
  rng.range = function(a,b){ return (a + rng.base0.random(1+b-a)) };
  //Generate a random long integer X where A-D<=X<=A+D
  rng.spread = function(a,b){ return rng.range(a-b,a+b); }
  rng.odds = function(p){ return (rng.base0.random(100) < (P)); }

	rng.dice = {};
	rng.dice.roll = _roll; 
	rng.dice.rollString = _rollString;
	rng.dice.cheat = {};
	rng.dice.cheat.roll = _rollCheat;
	rng.dice.cheat.rollString = _rollStringCheat;

}


//Public Functions


//Private Functions

function _roll(num,sides){
	var sum = 0;
	for (var i = 0; i < num; i++)
		sum += rng.base1.random(sides);
	return (sum);
}

function _rollString( s ){

	var ta = s.split("d");
	if( ta.length == 1 )
		return _roll( 1 , ta[0] * 1 );
	else
		return _roll( ta[0] * 1 , ta[1] * 1 );
}

function _rollCheat(num,sides){
	return num*sides;
}

function _rollStringCheat( s ){

	var ta = s.split("d");
	if( ta.length == 1 )
		return ta[0]*1;
	else
		return ( (ta[0]*1) * (ta[1]*1) );
}

function u32b(n){
  n=Math.floor(n);while( n < 0 ) n = n + 4294967295; return n % 4294967295;
}

//We are concerned this will not work
function _LCRNG(x){
  return u32b((x) * 1103515245 + 12345);
}

function _seed(seed){

  var i, j;

  /* Seed the table, dont trust the seed */
  rng.states[0] = u32b(seed);

  /* Propagate the seed */
  for (i = 1; i < RAND_DEG; i++)
    rng.states[i] = _LCRNG(rng.states[i-1]);

  /* Cycle the table ten times per degree */
  for (i = 0; i < RAND_DEG * 10; i++)
  {
    /* Acquire the next index */
    j = rng.index + 1;
    if (j == RAND_DEG) j = 0;

    /* Update the table, extract an entry */
    rng.states[j] += rng.states[rng.index];

    /*Evil, evil u32b cycling..*/
    rng.states[j] = u32b(rng.states[j]);
    /* Advance the index */
    rng.index = j;
  }
}

function _random(m){
  var r, n;

	/* Hack -- simple case */
	if (m <= 1) return (0);

	/* Partition size */
	n = Math.floor(0x10000000 / m);

	/* Use a simple RNG */
	if (rng.quick){
		/* Wait for it */
		while (true){
			/* Cycle the generator */
			r = rng.current = _LCRNG(rng.current);

			/* Mutate a 28-bit "random" number */
			r = u32b(u32b(r & 0x0FFFFFFF) / n);

			/* Done */
			if (r < m) break;
		}
	}

	/* Use a complex RNG */
	else
	{
		/* Wait for it */
		while (true){
		  var j;

			/* Acquire the next index */
			j = rng.index + 1;
			if (j == RAND_DEG) j = 0;

			/* Update the table, extract an entry */
			r = rng.states[j] = u32b(rng.states[j] + rng.states[rng.index]);

			/* Hack -- extract a 28-bit "random" number */
			r = u32b(u32b(r & 0x0FFFFFFF) / n);

			/* Advance the index */
			rng.index = j;

			/* Done */
			if (r < m) break;
		}
	}

	/* Use the value */
	//console.log("Rolled %i-1: %i" , m , r );
	return (r);
}



//Windows script host & FF FB testing, evil evil
//try{
//if( WScript || ( console && weAreTesting ) ){
//
//  var stdout = WScript.StdOut;
//
//  function write( s ){
//    if( WScript )
//      stdout.WriteLine( s )
//    else
//      console.log( s );
//  }
//}catch(e){}
//

