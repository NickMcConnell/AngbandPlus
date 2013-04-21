/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

var features = new Object();

function addFeature( index , description , symbol , color ){

  var feature = new Object();
  feature.index = index*1;
  feature.description = description;
  feature.symbol = symbol;
  feature.color = color;
  features[index*1] = feature;
}



addFeature("0"   , "nothing"                               , "&nbsp;" , "w");        // 0x01 --> open floor
addFeature("1"   , "open floor"                            , "." , "w");        // 0x02 --> invisible trap ("drawn as open floor")
addFeature("2"   , "invisible trap"                        , "." , "w" , "1");  // 0x03 --> glyph of warding
addFeature("3"   , "glyph of warding"                      , ";" , "y");        // 0x04 --> open door
addFeature("4"   , "open door"                             , "'" , "U");        // 0x05 --> broken door
addFeature("5"   , "broken door"                           , "'" , "U");        // 0x06 --> up stairs ("perm")
addFeature("6"   , "up staircase"                          , "<" , "r");        // 0x07 --> down stairs ("perm")
addFeature("7"   , "down staircase"                        , ">" , "r");        // 0x08 --> Path
addFeature("8"   , "path"                                  , "." , "y");        // 0x09 --> unused
addFeature("9"   , "nothing"                               , "#" , "B");        // 0x0A --> unused
addFeature("10"  , "nothing"                               , "#" , "G");        // 0x0B --> nothing
addFeature("11"  , "unused"                                , "#" , "g");        // 0x0C --> nothing
addFeature("12"  , "unused"                                , "5" , "b");        // 0x0D --> nothing
addFeature("13"  , "unused"                                , "6" , "r");        // 0x0E --> nothing
addFeature("14"  , "unused"                                , "7" , "D");        // 0x0F --> unused
addFeature("15"  , "nothing"                               , "8" , "y");        // 0x10 --> visible trap -- trap door
addFeature("16"  , "trap door"                             , "^" , "w");        // 0x11 --> visible trap -- open pit
addFeature("17"  , "pit"                                   , "^" , "s");        // 0x12 --> visible trap -- spiked pit
addFeature("18"  , "pit"                                   , "^" , "s");        // 0x13 --> visible trap -- poison pit
addFeature("19"  , "pit"                                   , "^" , "s");        // 0x14 --> visible trap -- rune -- summon
addFeature("20"  , "strange rune"                          , "^" , "o");        // 0x15 --> visible trap -- rune -- teleport
addFeature("21"  , "strange rune"                          , "^" , "o");        // 0x16 --> visible trap -- spot -- fire
addFeature("22"  , "discolored spot"                       , "^" , "u");        // 0x17 --> visible trap -- spot -- acid
addFeature("23"  , "discolored spot"                       , "^" , "u");        // 0x18 --> visible trap -- dart -- slow
addFeature("24"  , "dart trap"                             , "^" , "r");        // 0x19 --> visible trap -- dart -- lose str
addFeature("25"  , "dart trap"                             , "^" , "r");        // 0x1A --> visible trap -- dart -- lose dex
addFeature("26"  , "dart trap"                             , "^" , "r");        // 0x1B --> visible trap -- dart -- lose con
addFeature("27"  , "dart trap"                             , "^" , "r");        // 0x1C --> visible trap -- gas -- blind
addFeature("28"  , "gas trap"                              , "^" , "g");        // 0x1D --> visible trap -- gas -- confuse
addFeature("29"  , "gas trap"                              , "^" , "g");        // 0x1E --> visible trap -- gas -- poison
addFeature("30"  , "gas trap"                              , "^" , "g");        // 0x1F --> visible trap -- gas -- sleep
addFeature("31"  , "gas trap"                              , "^" , "g");        // 0x2x --> locked door ("power 0")
addFeature("32"  , "door"                                  , "+" , "U" , "32"); // 0x2x --> locked door ("power 1")
addFeature("33"  , "locked door"                           , "+" , "U" , "32"); // 0x2x --> locked door ("power 2")
addFeature("34"  , "locked door"                           , "+" , "U" , "32"); // 0x2x --> locked door ("power 3")
addFeature("35"  , "locked door"                           , "+" , "U" , "32"); // 0x2x --> locked door ("power 4")
addFeature("36"  , "locked door"                           , "+" , "U" , "32"); // 0x2x --> locked door ("power 5")
addFeature("37"  , "locked door"                           , "+" , "U" , "32"); // 0x2x --> locked door ("power 6")
addFeature("38"  , "locked door"                           , "+" , "U" , "32"); // 0x2x --> locked door ("power 7")
addFeature("39"  , "locked door"                           , "+" , "U" , "32"); // 0x2x --> jammed door ("power 0")
addFeature("40"  , "jammed door"                           , "+" , "U" , "32"); // 0x2x --> jammed door ("power 1")
addFeature("41"  , "jammed door"                           , "+" , "U" , "32"); // 0x2x --> jammed door ("power 2")
addFeature("42"  , "jammed door"                           , "+" , "U" , "32"); // 0x2x --> jammed door ("power 3")
addFeature("43"  , "jammed door"                           , "+" , "U" , "32"); // 0x2x --> jammed door ("power 4")
addFeature("44"  , "jammed door"                           , "+" , "U" , "32"); // 0x2x --> jammed door ("power 5")
addFeature("45"  , "jammed door"                           , "+" , "U" , "32"); // 0x2x --> jammed door ("power 6")
addFeature("46"  , "jammed door"                           , "+" , "U" , "32"); // 0x2x --> jammed door ("power 7")
addFeature("47"  , "jammed door"                           , "+" , "U" , "32"); // 0x30 --> secret door
addFeature("48"  , "secret door"                           , "#" , "w" , "56"); // 0x31 --> rubble
addFeature("49"  , "pile of rubble"                        , ":" , "w");        // 0x32 --> magma vein
addFeature("50"  , "magma vein"                            , "%" , "s");        // 0x33 --> quartz vein
addFeature("51"  , "quartz vein"                           , "%" , "w");        // 0x34 --> magma vein + treasure
addFeature("52"  , "magma vein"                            , "%" , "s" , "50"); // 0x35 --> quartz vein + treasure
addFeature("53"  , "quartz vein"                           , "%" , "w" , "51"); // 0x36 --> magma vein + known treasure
addFeature("54"  , "magma vein with treasure"              , "*" , "o");        // 0x37 --> quartz vein + known treasure
addFeature("55"  , "quartz vein with treasure"             , "*" , "o");        // 0x38 --> granite wall -- basic
addFeature("56"  , "granite wall"                          , "#" , "y");        // 0x39 --> granite wall -- inner
addFeature("57"  , "granite wall"                          , "#" , "w" , "56"); // 0x3A --> granite wall -- outer
addFeature("58"  , "granite wall"                          , "#" , "w" , "56"); // 0x3B --> granite wall -- solid
addFeature("59"  , "granite wall"                          , "#" , "w" , "56"); // 0x3C --> permanent wall -- building ("perm")
addFeature("60"  , "building wall"                         , "#" , "w");        // 0x3D --> permanent wall -- inner ("perm")
addFeature("61"  , "permanent wall"                        , "#" , "w");        // 0x3E --> permanent wall -- outer ("perm")
addFeature("62"  , "permanent wall"                        , "#" , "w" , "61"); // 0x3F --> permanent wall -- solid ("perm")
addFeature("63"  , "permanent wall"                        , "#" , "w" , "61");
addFeature("64"  , "explosive rune"                        , "*" , "R");
addFeature("65"  , "Pattern startpoint"                    , "*" , "w");
addFeature("66"  , "section of the Pattern"                , "*" , "B");
addFeature("67"  , "section of the Pattern"                , "*" , "b");
addFeature("68"  , "section of the Pattern"                , "*" , "B");
addFeature("69"  , "section of the Pattern"                , "*" , "b");
addFeature("70"  , "section of the Pattern"                , "*" , "W");
addFeature("71"  , "section of the Pattern (\"discharged\")" , "*" , "W");
addFeature("72"  , "Pattern exit"                          , "*" , "w");
addFeature("73"  , "corrupted section of the Pattern"      , "*" , "D");        // 0x08 --> shop -- general store ("perm")
addFeature("74"  , "General Store"                         , "1" , "U");        // 0x09 --> shop -- armoury ("perm")
addFeature("75"  , "Armoury"                               , "2" , "s");        // 0x0A --> shop -- weapon shop ("perm")
addFeature("76"  , "Weapon Smiths"                         , "3" , "w");        // 0x0B --> shop -- temple ("perm")
addFeature("77"  , "Temple"                                , "4" , "g");        // 0x0C --> shop -- alchemist ("perm")
addFeature("78"  , "Alchemy Shop"                          , "5" , "b");        // 0x0D --> shop -- magic shop ("perm")
addFeature("79"  , "Magic Shop"                            , "6" , "r");        // 0x0E --> shop -- black market ("perm")
addFeature("80"  , "Black Market"                          , "7" , "D");        // 0x0F --> shop -- home ("perm")
addFeature("81"  , "Home"                                  , "8" , "y");        // 0x0F --> shop -- Bookstore ("perm")
addFeature("82"  , "Bookstore"                             , "9" , "o");
addFeature("83"  , "Inn"                                   , "+" , "w");
addFeature("84"  , "Hall of Records"                       , "+" , "y");
addFeature("85"  , "Pawnbrokers"                           , "+" , "g");
addFeature("86"  , "nothing"                               , "x" , "w");
addFeature("87"  , "nothing"                               , "x" , "w");
addFeature("88"  , "nothing"                               , "x" , "w");
addFeature("89"  , "nothing"                               , "x" , "w");
addFeature("90"  , "nothing"                               , "x" , "w");
addFeature("91"  , "nothing"                               , "x" , "w");
addFeature("92"  , "nothing"                               , "x" , "w");
addFeature("93"  , "nothing"                               , "x" , "w");
addFeature("94"  , "nothing"                               , "x" , "w");
addFeature("95"  , "nothing"                               , "x" , "w");
addFeature("96"  , "town gate"                             , "+" , "u");
addFeature("97"  , "water"                                 , "#" , "B");
addFeature("98"  , "tree"                                  , "#" , "G");
addFeature("99"  , "bush"                                  , "#" , "g");
addFeature("100" , "border"                                , "." , "w" , "1");
addFeature("101" , "sea"                                   , "#" , "B" , "97");
addFeature("102" , "path border"                           , "." , "y" , "8");

