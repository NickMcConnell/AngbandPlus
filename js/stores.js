/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/


var MAX_STORES   = 13;
var stores = new Array();

  for ( var n = 0; n < MAX_STORES-1; n++ ){
    var store            = new Object();
    store.x              = 0;           /* Coords of store in town, the door more specifically */
    store.y              = 0;           /* Coords of store in town, the door more specifically */
    store.bought         = false;       /* Flag for player purchase (only used on houses) */
    store.owner          = 0;           /* Owner index */
    store.insultCounter  = 0;           /* Insult counter */
    store.goodBuys       = 0;           /* Number of "good" buys */
    store.badBuys        = 0;           /* Number of "bad" buys */
    store.storeOpen      = 0;           /* Closed until this turn */
    store.stockTypes     = new Array(); /* Legal kinds/types of sold items */
    store.stock          = new Array(); /* Stock */
    stores[n] = store;
  }
