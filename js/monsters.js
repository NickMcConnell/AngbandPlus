/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

var monsters = new Object();

var monstersPrototype = function(){
};

var MAX_SIGHT = 20;

//This better never go wrong ;]
monstersPrototype.prototype.has = function has(flag){

		try{
			return this.flags[flag]?true:false;
		}catch(e){}
	return false;
}

//Syntactic sugar, has( "UNIQUE" ) just looks silly ;]
monstersPrototype.prototype.is = monstersPrototype.prototype.has;

function addMonster( index , name ){
	var monster = new monstersPrototype();
	monster.description = "";
	monster.speed = new Object();
	monster.blows = new Array();
	monster.flags = new Object();
	monster.spells = new Object();
	monster.index = index*1;
	monster.name  = name;
	monster.r_idx = 0;
	//TODO , this must come from savegame now..
	monster.count = {};
	monster.count.current = 0;
	monster.count.max = 0;
	monster.finalProbability = 0;
	monsters[index*1] = monster;
	return monsters[index*1];
}

function updateMonster( monster ){
	monsters[ monster.index ] = monster;
}

function initMonsters(){

	var value, key;


	monsters.getByProbability = _getByProbability;
	var ta = r_info.split("\n");
	var monster;
	for( line in ta ){
		var line = ta[line];
		var start = line.substring(0,1);
		var values = line.split(":");
		if(start=="N"){ //N:591:Lucifer Morningstar
			monster = addMonster( values[1] , values[2] );
		}else if( start == 'G' ){ //G:A:d
			monster.symbol = values[1];
			monster.color = values[2];
		}else if( start == 'I' ){ //I:155:10:200d150:111:175:0
			monster.speed.move   = values[1];  //speed
			monster.speed.attack = values[2];  //num_blows / atspd
			monster.hitdice      = values[3];  //hdice / hp1
			//We do hitdice now in a string, god bless modern languages
			//monster.hside = hp2;             //hside / hp2
			monster.noticeRange  = values[4];  //aaf 
			monster.armourClass  = values[5];  //ac
			monster.sleep        = values[6];  //sleep
		}else if( start == 'W' ){ //W:100:1:0:66666
			//TODO, remove extra
			monster.level  = values[1];
			monster.rarity = values[2];
			monster.localProbability = Math.floor( 100 / monster.rarity );
			monster.extra  = values[3];
			monster.exp    = values[4];
		}else if( start == 'B' ){ //B:CRUSH:SHATTER:22d10
			var blow = new Object();
			blow.method = values[1];
			blow.effect = values[2];
			blow.damage = values[3]; 
			// No check on method or effect, damage as dice formula
			// Store text, not idx
			monster.blows.push( blow );
		}else if( start == 'F' ){ //F:UNIQUE | ALWAYS_GUARD| ATTR_MULTI | ATTR_ANY | ESCORT | ESCORTS |
			values = values[1].split("|");
			for( key in values )
				monster.flags[values[key].trim()] = true;
		}else if( start == 'S' ){ //S:1_IN_3 | S_MONSTERS | BR_CHAO | BA_CHAO | BRAIN_SMASH | S_REAVER |\n\
			values = values[1].split("|");
			for( key in values ){
				value = values[key].trim();
				if(value=="") continue;
				if( value.substring( 0 , 5 ) == "1_IN_" )
					monster.spells.frequency = Math.floor( 100 / (value.substring(5) * 1) )*2;
				else
					monster.spells[value] = true;
			}
		}else if( start == 'D' ){ //D:The lord of hell himself. Most beautiful of the angels, he is
			monster.description += values[1];
		}
	}
	//HACK, fake array property
	monsters.length = monster.index;
}

//TODO, make this part of the monsters object
function _getByProbability( probability ){
  /* Loop monster, check probability , return good stuff */
  for (var i = 0; i < monsters.length; i++)
    if ( monsters[i].finalProbability > probability ) 
      return monsters[i];
    
  /* Handle failure*/
  return monsters[0];
}
