/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/


var population = [];

population.count = 0;
population.maximumCount= 512;
population.reproducers = 0;
population.shifters = false // shifters ~= shimmers

population.repairNeeded = false; //for M_SLEEP monsters