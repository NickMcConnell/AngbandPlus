/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

//Global Objects

var player;            //This contains the player

var PY_MAX_LEVEL = 50; //Maximum level

//Private Objects

//var _yyy;          //xxx

//Initialization

function initPlayer(){
  player = new Object();
  player.effects = new Object();
  player.precalc = new Object();
  player.old = new Object();
  player.putOverview      = _putOverviewFrame;
  player.calculateBonuses = _calculateBonuses;
  player.calculateBonus = _calculateBonus;
  player.x = 0;
  player.y = 1;
  player.dungeonLevel = 0;
  player.turn = 0;
  player.level = 1;
  player.cameFrom = START_RANDOM;
  player.redraw = 0;
	player.window = 0;
	player.disturb = {};
	player.disturb.move = true;
	player.action = {};
	player.action.disturb = function(){ alert("TODO: action disturb not implemented!") };

	player.magicShell = false;
	player.encumbered = {};
	player.encumbered.gloves = false;
	player.encumbered.amour = false;

	player.command = {};

}

//Public Functions

//function doXXX(){}

//Private Functions

function _putOverviewFrame( post ){
  view.clear();
  view.print("                           Hellband Character Sheet                             ");//0
  view.print("                                                                                ");//1
  view.print(" [============== Character ============]  [============== Stats ===============]");//2
  view.print("                                                                                ");//3
  view.print("  Race        :                            Strength     :                       ");//4
  view.print("  Class       :                            Intelligence :                       ");//5
  view.print("  Birth Sign  :                            Wisdom       :                       ");//6
  view.print("  Major Realm :                            Dexterity    :                       ");//7
  view.print("  Minor Realm :                            Constitution :                       ");//8
  view.print("  Patron      :                            Charisma     :                       ");//9
  view.print("                                                                                ");//10
  view.print(" [======= Combat =======] [======Progression =======] [====== Counters ========]");//11
  view.print("                                                                                ");//12
  view.print("  To Hit Bonus  :          Level      :                Gold        :            ");//13
  view.print("  Damage Bonus  :          Experience :                HitPoints   :            ");//14
  view.print("  Armor Class   :          Exp to Adv :                Mana        :            ");//15
  view.print("  Blows/Round   :          Factor     :                Infravision :            ");//16
  view.print("  Shots/Round   :                                                               ");//17
  view.print("                    [========= Adventuring Skills =========]                    ");//18
  view.print("                                                                                ");//19
  view.print("  Combat   :               Perception :                Occlusion   :            ");//20
  view.print("  Missiles :               Disarming  :                Appraising  :            ");//21
  view.print("  Wizardry :               Stealth    :                Alchemy     :            ");//22
  view.print("                                                                                ");//23
  view.print("                                                                                ");//24

  if(post)  post();

  _putOverviewData();
}

function _putOverviewData(){

  view.cursor.set( 18 ,  4 );  view.print( player.race );
  view.cursor.set( 18 ,  5 );  view.print( player.vocation );
  view.cursor.set( 18 ,  6 );  view.print( player.sign );
  view.cursor.set( 18 ,  7 );  view.print( player.realm1 );
  view.cursor.set( 18 ,  8 );  view.print( player.realm2 );
  view.cursor.set( 18 ,  8 );  view.print( player.patron );

  view.cursor.set( 58 ,  4 );  view.print( formatStat( player.calc.strength ) );
  view.cursor.set( 58 ,  5 );  view.print( formatStat( player.calc.intelligence ) );
  view.cursor.set( 58 ,  6 );  view.print( formatStat( player.calc.wisdom ) );
  view.cursor.set( 58 ,  7 );  view.print( formatStat( player.calc.dexterity ) );
  view.cursor.set( 58 ,  8 );  view.print( formatStat( player.calc.constitution ) );
  view.cursor.set( 58 ,  9 );  view.print( formatStat( player.calc.charisma ) );


  view.cursor.set( 23 ,  13 );  view.printReversed( player.calc.to_hit          );
  view.cursor.set( 23 ,  14 );  view.printReversed( player.calc.to_dam          );
  view.cursor.set( 23 ,  15 );  view.printReversed( player.calc.to_armor        );
  view.cursor.set( 23 ,  16 );  view.printReversed( player.calc.blows           );
  view.cursor.set( 23 ,  17 );  view.printReversed( player.calc.shots           );

  view.cursor.set( 50 ,  13 );  view.printReversed( player.level                );
  view.cursor.set( 50 ,  14 );  view.printReversed( player.experience           );
  view.cursor.set( 50 ,  15 );  view.printReversed( Math.round( experienceTable[player.level - 1] * player.calc.experience_rate / 100 ) );
  view.cursor.set( 50 ,  16 );  view.printReversed( player.calc.experience_rate );

  view.cursor.set( 77 ,  13 );  view.printReversed( player.gold                );
  view.cursor.set( 77 ,  14 );  view.printReversed( player.hitpoints + "/" + player.calc.hitpoints );
  view.cursor.set( 77 ,  15 );  view.printReversed( player.mana      + "/" + player.calc.mana );
  view.cursor.set( 77 ,  16 );  view.printReversed( player.calc.infravision );

  view.cursor.set( 13 ,  20 );  view.print( evaluateSkill( player.calc.combat     , 12 ) );
  view.cursor.set( 13 ,  21 );  view.print( evaluateSkill( player.calc.missiles   , 12 ) );
  view.cursor.set( 13 ,  22 );  view.print( evaluateSkill( player.calc.wizardry   , 6  ) );

  view.cursor.set( 40 ,  20 );  view.print( evaluateSkill( player.calc.perception , 6 ) );
  view.cursor.set( 40 ,  21 );  view.print( evaluateSkill( player.calc.disarming  , 8 ) );
  view.cursor.set( 40 ,  22 );  view.print( evaluateSkill( player.calc.stealth    , 1  ) );

  view.cursor.set( 69 ,  20 );  view.print( evaluateSkill( player.calc.occlusion  , 6 ) );
  view.cursor.set( 69 ,  21 );  view.print( evaluateSkill( player.calc.appraising , 6 ) );
  view.cursor.set( 69 ,  22 );  view.print( evaluateSkill( player.calc.alchemy    , 6  ) );

  view.draw();
}



//Calculates either stats or traits
//Stats are numbers, calculated by summing up all applicable properties
//Traits are booleans, calculated by or'ing all applicable properties
function _calculateBonus( p , calculation ){
  if( player.calculationMode == "sum" ){
    player.calc[p] = (player[p] || 0) + (player.race[p] || 0) + (player.vocation[p] || 0) + (player.sign[p] || 0);
  }
  if( player.calculationMode == "boolean" ){
    player.calc[p] = _calculateBonusBoolean( player          , p ) ||
                     _calculateBonusBoolean( player.race     , p ) ||
                     _calculateBonusBoolean( player.sign     , p ) ||
                     _calculateBonusBoolean( player.vocation , p );
  }
}

//Takes any object and a trait string
//if the target object.trait is a boolean, take the boolean
//if the target object.trait is a number, then return true if the player level is greater
//if the target object.trait is a function, then execute the function by passing the player level, return result
function _calculateBonusBoolean( o , p ){
  if( o[p] ){
    if( typeof o[p] == "boolean"  )return o[p];
    if( typeof o[p] == "number"   )return o[p] >= player.level;
    if( typeof o[p] == "function" )return o[p]( player.level );
  }
  return false;
}

function evaluateSkill( x,y ){
  y = Math.max( y , 1 );
  if( x < 0 )return "Very Bad";
  y = Math.round( x / y );
  if( y < 2  )return "Bad";
  if( y < 3  )return "Poor";
  if( y < 5  )return "Fair";
  if( y < 6  )return "Good";
  if( y < 7  )return "Very Good";
  if( y < 9  )return "Excellent";
  if( y < 14 )return "Superb";
  if( y < 18 )return "Supernatural";
  return "Divine [".concat( Math.round((y-17)*5/2) ).concat("]");
}

function formatStat( statValue ){
  statValue=statValue*1;
  return (statValue < 19 )?statValue:("18/".concat(statValue-18<10?"0":"").concat(statValue-18)); 
}

function incStat( stat ){
  var gain;
  var value = player[stat];

  //Cannot get better than 18+100
  if( value >= 18+100 )return false;

  //Less than 18 adds 1 or 2 points
  if (value < 18)return( setStat( stat , value + ((randInt(100) < 75) ? 1 : 2) ) );

  //Between 18 and 19+98, add some complex formula
  if (value < 18+98){
    gain  = Math.round((((18+100) - value) / 2 + 3) / 2);
    gain  = Math.max( gain , 1 );
    value = Math.floor( value + randDice(gain) + gain / 2 );
    value = Math.min( value , 18+99 );
    return( setStat( stat , value ) );
  }

  //In all other cases, add 1 point
  return setStat( stat , value+1 );
}

function damageStat( stat , amount ){
  
  var cur = player[stat];
  
  if (cur > 3){
    if (cur <= 18){
      if (amount > 90) cur--;
      if (amount > 50) cur--;
      if (amount > 20) cur--;
      cur--;
    }else{
      /* Hack -- Decrement by a random amount between one-quarter */
      /* and one-half of the stat bonus times the percentage, with a */
      /* minimum damage of half the percentage. -CWS */
      loss = Math.round((((cur-18) / 2 + 1) / 2 + 1));
      loss = Math.max( loss , 1 );
      loss = Math.round(((randDice(loss) + loss) * amount) / 100);
      loss = Math.min( loss , amount >> 1 ); // >> 2 is evil rounded split by 2
      cur = cur - loss;
      /* Hack -- Only reduce stat to 17 sometimes */
      if (cur < 18) cur = (amount <= 20) ? 18 : 17;
    }
    loss = player[stat] - cur;
    if( loss > 0 )
      return( setStat( stat , cur ) );
  }
  return false;
}


function decStat( stat, amount, permanent ){
  return damageStat( stat , amount ) || ( permanent?damageStat( stat+"Max" ):false );
}

function setStat( stat , value ){
  player[stat] = value;
  if( stat.indexOf("Max") > -1 )
    player[stat+"Max"] = Math.max( player[stat] , player[stat+"Max"] );
  view.updateRequests |= (PU_BONUS);
  return true;
}


var experienceTable =
[10,25,45,70,100,140,200,280,380,500,650,850,1100,1400,1800,2300,2900,3600,4400,5400,6800,8400,10200,12500,17500,25000,35000,50000,75000,100000,150000,200000,275000,350000,450000,550000,700000,850000,1000000,1250000,1500000,1800000,2100000,2400000,2700000,3000000,3500000,4000000,4500000,5000000];


function statIndex( statValue ){
  /* Values: 3, 4, ..., 17 */
  if( statValue <= 18) return (statValue - 3);
  /* Ranges: 18/00-18/09, ..., 18/210-18/219 */
  if (statValue <= 18+219) return Math.floor(15 + (statValue- 18) / 10);
  /* Range: 18/220+ */
  return 37;

}

var ADJ_SPELLS   = 0 ; /*(INT/WIS) -- Number of half-spells per level*/
var ADJ_MANA     = 1 ; /*(INT/WIS) -- extra half-mana-points per level*/
var ADJ_FAILURE  = 2 ; /*(INT/WIS) -- Minimum failure rate (percentage)*/
var ADJ_INTWIS   = 3 ; /*(INT/WIS) -- Various things*/
var ADJ_PRICE    = 4 ; /*(CHA) -- payment percentages*/
var ADJ_DEVICE   = 5 ; /*(INT) -- Magic devices*/
var ADJ_RESIST   = 6 ; /*(WIS) -- Saving throw*/
var ADJ_DEX_TRAP = 7 ; /*(DEX) -- disarming*/
var ADJ_INT_TRAP = 8 ; /*(INT) -- disarming*/
var ADJ_AC       = 9 ; /*(DEX) -- bonus to ac (plus 128)*/
var ADJ_DAM      = 10; /*(STR) -- bonus to dam (plus 128)*/
var ADJ_DEX_HIT  = 11; /*(DEX) -- bonus to hit (plus 128)*/
var ADJ_STR_HIT  = 12; /*(STR) -- bonus to hit (plus 128)*/
var ADJ_WEIGHT   = 13; /*(STR) -- weight limit in deca-pounds*/
var ADJ_SIZE     = 14; /*(STR) -- weapon weight limit in pounds*/
var ADJ_DIG      = 15; /*(STR) -- digging value*/
var ADJ_STR_BLOW = 16; /*(STR) -- help index into the "blow" table*/
var ADJ_DEX_BLOW = 17; /*(DEX) -- index into the "blow" table*/
var ADJ_DODGE    = 18; /*(DEX) -- chance of avoiding "theft" and "falling"*/
var ADJ_REGEN    = 19; /*(CON) -- base regeneration rate*/
var ADJ_HP       = 20; /*(CON) -- extra half-hitpoints per level (plus 128)*/

var statAdjustments =
[	/*1  2   3   4   5    6   7   8   9   10        11        12        13        14  15   16   17   18  19   20 21                            */
	[ 0, 0 , 99, 0 , 130, 0 , 0 , 0 , 0 , 128 + -4, 128 + -2, 128 + -3, 128 + -3, 5 , 4  , 0  , 3  , 0 , 0  , 0, 128 + -5 ],  /* 3             */
	[ 0, 0 , 99, 0 , 125, 0 , 0 , 0 , 0 , 128 + -3, 128 + -2, 128 + -2, 128 + -2, 6 , 5  , 0  , 4  , 0 , 1  , 0, 128 + -3 ],  /* 4             */
	[ 0, 0 , 99, 0 , 122, 0 , 0 , 0 , 0 , 128 + -2, 128 + -1, 128 + -2, 128 + -1, 7 , 6  , 1  , 5  , 0 , 2  , 0, 128 + -2 ],  /* 5             */
	[ 0, 0 , 99, 0 , 120, 0 , 0 , 0 , 0 , 128 + -1, 128 + -1, 128 + -1, 128 + -1, 8 , 7  , 2  , 6  , 0 , 3  , 0, 128 + -1 ],  /* 6             */
	[ 0, 0 , 99, 0 , 118, 0 , 0 , 0 , 0 , 128 +  0, 128 +  0, 128 + -1, 128 +  0, 9 , 8  , 3  , 7  , 0 , 4  , 0, 128 +  0 ],  /* 7             */
	[ 1, 1 , 50, 1 , 116, 1 , 1 , 0 , 1 , 128 +  0, 128 +  0, 128 +  0, 128 +  0, 10, 10 , 4  , 8  , 0 , 5  , 0, 128 +  0 ],  /* 8             */
	[ 1, 2 , 30, 1 , 114, 1 , 1 , 0 , 1 , 128 +  0, 128 +  0, 128 +  0, 128 +  0, 11, 12 , 4  , 9  , 0 , 5  , 0, 128 +  0 ],  /* 9             */
	[ 1, 2 , 20, 1 , 112, 1 , 1 , 0 , 1 , 128 +  0, 128 +  0, 128 +  0, 128 +  0, 12, 14 , 5  , 10 , 1 , 6  , 0, 128 +  0 ],  /* 10            */
	[ 1, 2 , 15, 1 , 110, 1 , 1 , 0 , 1 , 128 +  0, 128 +  0, 128 +  0, 128 +  0, 13, 16 , 5  , 11 , 1 , 6  , 0, 128 +  0 ],  /* 11            */
	[ 2, 2 , 12, 1 , 108, 1 , 1 , 0 , 1 , 128 +  0, 128 +  0, 128 +  0, 128 +  0, 14, 18 , 6  , 12 , 1 , 7  , 0, 128 +  0 ],  /* 12            */
	[ 2, 2 , 11, 1 , 106, 1 , 1 , 1 , 1 , 128 +  0, 128 +  0, 128 +  0, 128 +  0, 15, 20 , 6  , 13 , 1 , 7  , 0, 128 +  0 ],  /* 13            */
	[ 2, 2 , 10, 1 , 104, 1 , 1 , 1 , 1 , 128 +  0, 128 +  0, 128 +  0, 128 +  0, 16, 22 , 7  , 14 , 1 , 8  , 1, 128 +  0 ],  /* 14            */
	[ 2, 2 , 9 , 2 , 103, 2 , 2 , 1 , 2 , 128 +  1, 128 +  0, 128 +  0, 128 +  0, 17, 24 , 7  , 15 , 1 , 8  , 1, 128 +  1 ],  /* 15            */
	[ 2, 2 , 8 , 2 , 102, 2 , 2 , 2 , 2 , 128 +  1, 128 +  1, 128 +  1, 128 +  0, 18, 26 , 8  , 16 , 1 , 9  , 1, 128 +  1 ],  /* 16            */
	[ 2, 2 , 7 , 2 , 101, 2 , 2 , 2 , 2 , 128 +  1, 128 +  2, 128 +  2, 128 +  0, 19, 28 , 8  , 17 , 1 , 9  , 1, 128 +  2 ],  /* 17            */
	[ 2, 3 , 6 , 3 , 100, 3 , 3 , 4 , 3 , 128 +  2, 128 +  2, 128 +  3, 128 +  1, 20, 30 , 9  , 20 , 1 , 10 , 2, 128 +  3 ],  /* 18/00-18/09   */
	[ 2, 3 , 6 , 3 , 99 , 3 , 3 , 4 , 3 , 128 +  2, 128 +  2, 128 +  3, 128 +  1, 22, 30 , 10 , 30 , 2 , 10 , 2, 128 +  4 ],  /* 18/10-18/19   */
	[ 2, 3 , 5 , 3 , 98 , 4 , 3 , 4 , 3 , 128 +  2, 128 +  3, 128 +  3, 128 +  1, 24, 35 , 12 , 40 , 2 , 15 , 2, 128 +  4 ],  /* 18/20-18/29   */
	[ 2, 3 , 5 , 3 , 97 , 4 , 3 , 4 , 4 , 128 +  2, 128 +  3, 128 +  3, 128 +  1, 26, 40 , 15 , 50 , 2 , 15 , 2, 128 +  4 ],  /* 18/30-18/39   */
	[ 2, 3 , 5 , 3 , 96 , 5 , 3 , 5 , 4 , 128 +  2, 128 +  3, 128 +  3, 128 +  1, 28, 45 , 20 , 60 , 2 , 20 , 2, 128 +  4 ],  /* 18/40-18/49   */
	[ 3, 4 , 4 , 4 , 95 , 5 , 4 , 5 , 5 , 128 +  3, 128 +  3, 128 +  4, 128 +  1, 30, 50 , 25 , 70 , 3 , 25 , 3, 128 +  5 ],  /* 18/50-18/59   */
	[ 3, 4 , 4 , 4 , 94 , 6 , 4 , 5 , 6 , 128 +  3, 128 +  3, 128 +  4, 128 +  1, 31, 55 , 30 , 80 , 3 , 30 , 3, 128 +  6 ],  /* 18/60-18/69   */
	[ 3, 5 , 4 , 5 , 93 , 6 , 5 , 6 , 7 , 128 +  3, 128 +  4, 128 +  4, 128 +  2, 31, 60 , 35 , 90 , 4 , 35 , 3, 128 +  7 ],  /* 18/70-18/79   */
	[ 3, 6 , 4 , 6 , 92 , 7 , 5 , 6 , 8 , 128 +  4, 128 +  5, 128 +  4, 128 +  3, 32, 65 , 40 , 100, 4 , 40 , 3, 128 +  8 ],  /* 18/80-18/89   */
	[ 4, 7 , 3 , 7 , 91 , 7 , 6 , 7 , 9 , 128 +  5, 128 +  5, 128 +  5, 128 +  4, 32, 70 , 45 , 110, 5 , 45 , 3, 128 +  9 ],  /* 18/90-18/99   */
	[ 4, 8 , 3 , 8 , 90 , 8 , 7 , 8 , 10, 128 +  6, 128 +  6, 128 +  6, 128 +  5, 33, 80 , 50 , 120, 6 , 50 , 4, 128 + 10 ],  /* 18/100-18/109 */
	[ 4, 9 , 2 , 9 , 89 , 9 , 8 , 8 , 10, 128 +  7, 128 +  7, 128 +  7, 128 +  6, 33, 80 , 55 , 130, 7 , 60 , 4, 128 + 11 ],  /* 18/110-18/119 */
	[ 5, 10, 2 , 10, 88 , 10, 9 , 8 , 11, 128 +  8, 128 +  8, 128 +  8, 128 +  7, 34, 80 , 60 , 140, 8 , 70 , 5, 128 + 12 ],  /* 18/120-18/129 */
	[ 5, 11, 2 , 11, 87 , 11, 10, 8 , 12, 128 +  9, 128 +  9, 128 +  9, 128 +  8, 34, 80 , 65 , 150, 9 , 80 , 6, 128 + 13 ],  /* 18/130-18/139 */
	[ 5, 12, 2 , 12, 86 , 12, 11, 8 , 13, 128 +  9, 128 + 10, 128 +  9, 128 +  9, 35, 80 , 70 , 160, 10, 90 , 6, 128 + 14 ],  /* 18/140-18/149 */
	[ 5, 13, 1 , 13, 85 , 13, 12, 9 , 14, 128 + 10, 128 + 11, 128 + 10, 128 + 10, 35, 90 , 75 , 170, 11, 100, 7, 128 + 15 ],  /* 18/150-18/159 */
	[ 5, 14, 1 , 14, 84 , 14, 13, 9 , 15, 128 + 11, 128 + 12, 128 + 11, 128 + 11, 36, 90 , 80 , 180, 12, 100, 7, 128 + 16 ],  /* 18/160-18/169 */
	[ 5, 15, 1 , 15, 83 , 15, 14, 9 , 16, 128 + 12, 128 + 13, 128 + 12, 128 + 12, 36, 90 , 85 , 190, 14, 100, 8, 128 + 18 ],  /* 18/170-18/179 */
	[ 5, 16, 1 , 16, 82 , 16, 15, 9 , 17, 128 + 13, 128 + 14, 128 + 13, 128 + 13, 37, 90 , 90 , 200, 16, 100, 8, 128 + 20 ],  /* 18/180-18/189 */
	[ 5, 16, 1 , 17, 81 , 17, 16, 9 , 18, 128 + 14, 128 + 15, 128 + 14, 128 + 14, 37, 90 , 95 , 210, 18, 100, 8, 128 + 22 ],  /* 18/190-18/199 */
	[ 5, 17, 0 , 18, 80 , 18, 17, 10, 19, 128 + 15, 128 + 16, 128 + 15, 128 + 15, 38, 100, 100, 220, 20, 100, 9, 128 + 25 ],  /* 18/200-18/209 */
	[ 6, 17, 0 , 19, 79 , 19, 18, 10, 19, 128 + 15, 128 + 18, 128 + 15, 128 + 15, 38, 100, 100, 230, 20, 100, 9, 128 + 26 ],  /* 18/210-18/219 */
	[ 6, 18, 0 , 20, 78 , 20, 19, 10, 20, 128 + 16, 128 + 20, 128 + 16, 128 + 16, 39, 100, 100, 240, 20, 100, 9, 128 + 27 ]   /* 18/220+       */
];


  function _calculateBonuses(){
  //Radical clearing of values ;)


  var encumbered = player.encumbered;
  var msp = 0;

  player.calc = new Object();

  player.calculationMode = "sum";

  _calculateBonus( "combat" );
  _calculateBonus( "stealth" );
  _calculateBonus( "alchemy" );
  _calculateBonus( "missiles" );
  _calculateBonus( "wizardry" );
  _calculateBonus( "occlusion" );
  _calculateBonus( "disarming" );
  _calculateBonus( "searching" );
  _calculateBonus( "appraising" );
  _calculateBonus( "perception" );
  _calculateBonus( "infravision" );
  _calculateBonus( "experience_rate" );
  _calculateBonus( "hitdice" )

  _calculateBonus( "wisdom" );
  _calculateBonus( "charisma" );
  _calculateBonus( "strength" );
  _calculateBonus( "dexterity" );
  _calculateBonus( "constitution" );
  _calculateBonus( "intelligence" );

  player.calc.occlusion   = player.calc.occlusion + player.magicShell || 0;
  player.calc.lightArmor  = true; //TODO : fix this

  player.calc.to_hit    =  statAdjustments[ statIndex(player.dexterity) ][ ADJ_DEX_HIT ] - 128;
  player.calc.to_hit   +=  statAdjustments[ statIndex(player.strength ) ][ ADJ_STR_HIT ] - 128;
  player.calc.to_dam    =  statAdjustments[ statIndex(player.strength ) ][ ADJ_DAM     ] - 128;
  player.calc.to_armor  =  statAdjustments[ statIndex(player.dexterity) ][ ADJ_AC      ] - 128;
  player.calc.occlusion += statAdjustments[ statIndex(player.wisdom   ) ][ ADJ_RESIST  ];

  //Max hp calculation
  var bonus = Math.ceil( statAdjustments[ statIndex(player.constitution) ][ ADJ_HP] - 128);

  if (player.level == 1){
    player.calc.hitpoints = player.calc.hitdice + Math.floor(bonus / 2);
  }else{
    player.calc.hitpoints = player.precalc.hitdice[player.level-1] + Math.floor( bonus * player.level / 2 );
  }

  player.hitpoints = Math.min( player.hitpoints , player.calc.hitpoints );

  //Max mana calculation

  if( player.vocation.spell_book == TV_NO_BOOK ){
    //If the vocation does not do magic, there is no mana
    //for now..
    player.calc.mana = player.mana = 0;
  }else{
    
    //determine mage level
    var levels = player.vocation.spell_type == MENTAL ? player.level : player.level - player.vocation.first_spell;

    //ensure a positive level
    levels = Math.max( levels , 1 );

    /* Extract total mana */
    msp = statAdjustments[statIndex( player.calc[ player.vocation.spell_stat ] )][ADJ_MANA] * levels / 2;

  	/* Hack -- usually add one mana */
	if (msp) msp++;

  	/* Hack: High mages have a 25% mana bonus */
  	if ( player.vocation.toString() == "High Mage" )  msp = Math.floor(  msp * 1.25 );
  

    if( player.vocation.spell_type == MAGIC ){
	  //We assume no encumbrance
      player.encumbered.gloves = false;
      player.encumbered.armour = false;

      //Mages need no or special gloves
      if( gear.hands.equipped && !( gear.hands.has( FREE_ACTION ) || gear.hands.get( DEXTERITY ) > 0 ) ){
        msp = Math.floor( msp / 4 * 3 );
        player.encumbered.gloves = true;
      }

      //Mages need light armor

  		/* Determine the weight allowance */
      var encumbrance = (( gear.getEquipmentWeight() - player.vocation.mana_encumbrance ) / 10);

  		/* Heavy armour penalizes mana */
	  	if ( encumbrance > 0){
		  	player.encumbered.armour = true;
			  msp = msp - encumbrance;
  	  }
	}


  /* Mana must be an integer */
  msp = Math.ceil( msp );

  /* Mana can never be negative */
	if (msp < 0) msp = 0;

  player.calc.mana = msp;
  player.mana = Math.min( player.mana , player.calc.mana );

  if( !game.startUpMode ){
    if ( player.encumbered.gloves != encumbered.gloves )
	    view.message( player.encumbered.gloves ? "Your covered hands feel unsuitable for spellcasting." : "Your hands feel more suitable for spellcasting." );
    if ( player.encumbered.armour != encumbered.armour )
  		view.message( player.encumbered.armour ? "The weight of your armour encumbers your movement." : "You feel able to move more freely." );
  }


  }

  player.calc.shots_extra = 0;
  player.calc.shots_ammo  = 0;
  player.calc.digging     = 0;
  player.calc.shots       = 1;
  player.calc.blows       = 1;
  player.calc.speed       = 110;

  player.calculationMode = "boolean";

}

/*
* This table allows quick conversion from "speed" to "energy"
* The basic function WAS ((S>=110) ? (S-110) : (100 / (120-S)))
* Note that table access is *much* quicker than computation.
*
* Note that the table has been changed at high speeds.  From
* "Slow (-40)" to "Fast (+30)" is pretty much unchanged, but
* at speeds above "Fast (+30)", one approaches an asymptotic
* effective limit of 50 energy per turn.  This means that it
* is relatively easy to reach "Fast (+30)" and get about 40
* energy per turn, but then speed becomes very "expensive",
* and you must get all the way to "Fast (+50)" to reach the
* point of getting 45 energy per turn.  After that point,
* furthur increases in speed are more or less pointless,
* except to balance out heavy inventory.
*
* Note that currently the fastest monster is "Fast (+30)".
*
* It should be possible to lower the energy threshhold from
* 100 units to 50 units, though this may interact badly with
* the (compiled out) small random energy boost code.  It may
* also tend to cause more "clumping" at high speeds.
*/

/* This table has been inverted for Hellband. The new values
* are 1000/x where x is the old value. This gives the same spread
* but subtracting extract_energy[n] each move and always adding
* 10 per turn, rather than adding extract_energy[n] each turn and
* always subtracting 100.
*
* This has been done to allow the seperating out of movement speed
* and attack speed.
*/
var extract_energy = [

/* Slow */     1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,
/* Slow */     1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,
/* Slow */     1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,
/* Slow */     1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,
/* Slow */     1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,
/* Slow */     1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,
/* S-50 */     1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,  1000,
/* S-40 */     500,  500,  500,  500,  500,  500,  500,  500,  500,  500,
/* S-30 */     500,  500,  500,  500,  500,  500,  500,  333,  333,  333,
/* S-20 */     333,  333,  333,  333,  333,  250,  250,  250,  250,  250,
/* S-10 */     200,  200,  200,  200,  167,  167,  143,  143,  125,  111,
/* Norm */    100, 91, 83, 77, 71, 67, 63, 59, 56, 53,
/* F+10 */    50, 48, 45, 43, 42, 40, 38, 37, 36, 34,
/* F+20 */    33, 32, 31, 30, 29, 29, 28, 28, 27, 27,
/* F+30 */    26, 26, 26, 26, 25, 25, 25, 24, 24, 24,
/* F+40 */    24, 24, 24, 23, 23, 23, 23, 23, 23, 23,
/* F+50 */    22, 22, 22, 22, 22, 22, 22, 22, 22, 22,
/* F+60 */    21, 21, 21, 21, 21, 21, 21, 21, 21, 21,
/* F+70 */    20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
/* Fast */    20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
];
