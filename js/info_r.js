/*********************************************************
* All sources in Hellbound are Copyright (c) 2008 Konijn *
* I Konijn, release all code and data under the terms of *
* of the GNU General Public License (version 2), as well *
* as under the traditional Angband license. Distribution *
* is allowed both under the terms of the GPL (version 2) *
* or under the terms of the traditional Angband license. *
*********************************************************/

var r_info = "# File: r_info.txt _\n\
\n\
# This file is used to initialize the 'lib/raw/r_info.raw' file, which is\n\
# used to initialize the 'monster race' information for the Angband game.\n\
# Do not modify this file unless you know exactly what you are doing,\n\
# unless you wish to risk possible system crashes and broken savefiles.\n\
\n\
# The format of a monster is as follows... ([] indicates an optional part)\n\
\n\
# N:<monster index>:<monster name>\n\
# G:<monster symbol>:<monster colour>\n\
# I:<movement speed>:<attack speed>:<hit points>:<perception>:<armour class>:<sleep>\n\
# W:<level found at>:<rarity>:<unused>:<kill score>\n\
# B:<attack type>[:<attack effect>][:<attack damage>]\n\
# F:<flag1> | <flag2>...\n\
# [S:<spell frequency>]\n\
# [S:<spell1> | <spell2>...]\n\
\n\
# 'I' is for information - speed, health, vision in tens of feet,\n\
# armor class, and alertness. 110 is normal speed. Alertness ranges\n\
# from 0 (ever vigilant for intruders) to 255 (prefers to ignore\n\
# intruders).\n\
\n\
# 'W' is for more information - level, rarity, and experience for\n\
# killing. The third slot is currently unused.\n\
\n\
# D - Dark Gray    w - White          s - Gray          o - Orange\n\
# r - Red          g - Green          b - Blue          u - Brown\n\
# d - Black        W - Light Gray     v - Violet        y - Yellow\n\
# R - Light Red    G - Light Green    B - Light Blue    U - Light Brown\n\
\n\
# (note that 'black' is the same color as the screen background, and\n\
# thus the monster will appear to be an empty black space if its\n\
# color is 'd'.)\n\
\n\
#Further notes : \n\
#Humans have open_door and bash_door, hardcoded for efficiency\n\
#Fallen Angels and Dragons have FLIGHT, hardcoded for efficiency\n\
#DROP_GOOD is no longer needed when DROP_GREAT is specified\n\
#FORCE_MAXHP is no longer needed when UNIQUE is specified\n\
\n\
##### The Player #####\n\
\n\
N:0:Player\n\
G:@:w\n\
\n\
\n\
##### Town monsters #####\n\
\n\
N:1:Needy urchin\n\
G:t:D\n\
I:110:1:1d4:4:1:40\n\
W:0:2:0:0\n\
B:BEG\n\
B:TOUCH:EAT_GOLD\n\
F:MALE | EVIL |\n\
F:RAND_25 |\n\
F:TAKE_ITEM | OPEN_DOOR\n\
D:He looks abandoned by his parents, he wonders \n\
D:about the weight of your purse. \n\
\n\
N:2:Ferocious cat\n\
G:f:U\n\
I:120:2:1d2:30:1:10\n\
W:0:3:0:0\n\
B:CLAW:HURT:1d2\n\
F:RAND_25 | \n\
F:ANIMAL\n\
D:A furball with sharp claws and a menacing look. \n\
\n\
N:3:Foaming bulldog\n\
G:C:U\n\
I:120:1:1d3:20:1:5\n\
W:0:3:0:0\n\
B:BITE:HURT:1d3\n\
F:RAND_50 | RAND_25 | \n\
F:ANIMAL\n\
D:The dog cant seem to decide if it should chase you \n\
D:or it's tail. \n\
\n\
N:4:Nadain, Knight of the Seventh Seal\n\
G:p:w\n\
I:110:4:35d10:40:10:3\n\
W:0:4:0:0\n\
B:MOAN\n\
B:MOAN\n\
F:UNIQUE | MALE |\n\
F:FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_90 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:NO_CONF | NO_SLEEP\n\
D:He's lost his mind in Inferno. The only thing he has left \n\
D:are his dreams of grandeur. He keeps on babbling about Seals \n\
D:while showing a blackened shield to any one interested. \n\
\n\
N:5:Inferno Supporter\n\
G:t:y\n\
I:110:1:1d2:6:1:0\n\
W:0:1:0:0\n\
F:MALE | RAND_25\n\
D:Nobody knows where they come from but their \n\
D:only purpose in life seems to be cheering the \n\
D:hapless heroes destined to die in Inferno. \n\
\n\
N:6:Wanderer\n\
G:t:g\n\
I:110:1:1d2:6:1:0\n\
W:0:1:0:0\n\
B:DROOL\n\
F:MALE | \n\
F:RAND_25 | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR\n\
D:His mind has been blasted by the presence \n\
D:of the Inferno. He lives of the charity of the \n\
D:townspeople. \n\
\n\
N:7:Raving lunatic\n\
G:t:G\n\
I:120:1:4d4:6:1:0\n\
W:0:1:0:0\n\
B:DROOL\n\
F:MALE | \n\
F:RAND_25 | \n\
F:TAKE_ITEM\n\
D:Drooling and comical, but then, what do you expect? \n\
\n\
N:8:Pitiful looking beggar\n\
G:t:g\n\
I:110:1:1d4:10:1:40\n\
W:0:1:0:0\n\
B:BEG\n\
F:MALE | \n\
F:RAND_25 | \n\
F:TAKE_ITEM | OPEN_DOOR\n\
D:You just can't help feeling sorry for him. \n\
\n\
N:9:Leecher\n\
G:u:r\n\
I:80:1:1d1:10:1:50\n\
W:0:1:0:0\n\
B:TOUCH:LOSE_CON\n\
B:TOUCH:LOSE_CON\n\
B:TOUCH:LOSE_CON\n\
B:TOUCH:LOSE_CON\n\
F:MALE | EVIL\n\
F:RAND_25 | \n\
F:TAKE_ITEM | OPEN_DOOR\n\
D:They steal life force for the demonic princes. Things must be \n\
D:bad if they can wander freely in this town. \n\
\n\
N:10:Shady Trader\n\
G:t:D\n\
I:110:2:2d8:10:8:99\n\
W:0:1:0:0\n\
B:HIT:HURT:1d6\n\
B:TOUCH:EAT_ITEM\n\
F:MALE | \n\
F:DROP_60 | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL\n\
D:He 'finds' new wares for the Black Market. \n\
D:From unwary adventurers... \n\
\n\
N:11:Passed out drunk\n\
G:t:y\n\
I:110:1:2d3:10:1:0\n\
W:0:1:0:0\n\
B:HIT:HURT:1d6\n\
F:MALE | \n\
F:ONLY_GOLD | DROP_60 | \n\
F:FORCE_SLEEP | NEVER_MOVE | \n\
F:STUPID | EMPTY_MIND | \n\
F:IM_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
D:He makes you glad to be sober. \n\
\n\
N:12:Merchant\n\
G:t:o\n\
I:110:1:3d3:10:1:255\n\
W:0:1:0:0\n\
B:HIT:HURT:1d3\n\
F:MALE | \n\
F:RAND_50 | \n\
F:ONLY_GOLD | DROP_60 | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR\n\
D:The typical ponce around town, with purse jingling, and looking for more \n\
D:amulets of adornment to buy. \n\
\n\
N:13:Mean looking mercenary\n\
G:t:R\n\
I:110:1:5d8:10:20:250\n\
W:0:1:0:0\n\
B:HIT:HURT:1d10\n\
F:MALE | \n\
F:RAND_50 | DROP_90 | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL\n\
D:No job is too low for him. \n\
\n\
N:14:Battle scarred veteran\n\
G:t:r\n\
I:110:1:7d8:10:30:250\n\
W:0:1:0:0\n\
B:HIT:HURT:2d6\n\
F:MALE | \n\
F:RAND_50 | DROP_90 | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR\n\
D:He doesn't take to strangers kindly. \n\
\n\
N:15:Foolhardy hero\n\
G:t:w\n\
I:109:1:3d6:50:15:4\n\
W:0:4:0:0\n\
B:HIT:HURT:1d6\n\
F:MALE |\n\
F:ONLY_ITEM | DROP_90 |\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR\n\
D:He wants to kill a hero to prove that he's hard. \n\
\n\
##### Normal monsters #####\n\
##### Level 1 #####\n\
\n\
#Dark red = murder \n\
N:16:Wandering Murderer\n\
G:p:r\n\
I:110:2:1d2:2:1:0\n\
W:1:1:0:3\n\
B:HIT:HURT:1d4\n\
D:This man has no idea what he is doing here. \n\
D:You get the impression he's not here by mistake though. \n\
D:He has not yet been judged by Minos. \n\
\n\
#Light red : lovers\n\
N:17:Wandering Lover\n\
G:p:R\n\
I:110:2:1d2:2:1:0\n\
W:1:1:0:3\n\
B:HIT:HURT:1d4\n\
F:FEMALE | STORM\n\
S:1_IN_3 |\n\
D:She seems to be seeking her lover, \n\
D:but some devillish magic keeps her from touching him ever again. \n\
D:She has not yet been judged by Minos. \n\
\n\
#Dark Umber : Gluts , they are always slower but hit harder\n\
N:18:Wandering Drunk\n\
G:p:u\n\
I:80:1:1d2:2:1:0\n\
W:1:1:0:5\n\
B:HIT:HURT:1d6\n\
B:HIT:EAT_FOOD\n\
F:MALE\n\
D:This one will be forced by Minos to lie in the mud under continual cold rain and hail. \n\
D:He has not yet been judged by Minos. \n\
\n\
#Light Umber : Sloth , they are always slower, hit softer. less xp\n\
N:19:Wandering Sloth\n\
G:p:U\n\
I:80:1:1d2:2:1:0\n\
W:1:1:0:1\n\
B:HIT:HURT:1d3\n\
F:MALE\n\
D:This one will be forced by Minos to remained trappped in the river Styx. \n\
D:He has not yet been judged by Minos. \n\
\n\
#Light Dark : Heretics \n\
N:20:Wandering Heretic\n\
G:p:D\n\
I:110:2:1d2:2:1:0\n\
W:1:1:0:3\n\
B:HIT:HURT:1d4\n\
F:MALE\n\
D:This one will be forced by Minos to remained trappped inside a burning tomb. \n\
D:He has not yet been judged by Minos. \n\
\n\
#Light Dark : Suicidal \n\
N:21:Wandering Suicidal\n\
G:p:D\n\
I:110:2:1d2:2:1:0\n\
W:1:1:0:3\n\
B:HIT:HURT:1d4\n\
F:MALE\n\
D:This one will be forced by Minos to be turned into a thorny black tree. \n\
D:He has not yet been judged by Minos. \n\
\n\
N:22:Wandering Hoarder\n\
G:p:y\n\
I:110:2:1d2:2:1:0\n\
W:1:1:0:3\n\
B:HIT:HURT:1d4\n\
F:MALE\n\
D:This one will burn for hoarding, he's very interested in your purse. \n\
D:He has not yet been judged by Minos. \n\
\n\
N:23:Wandering Squanderer\n\
G:p:y\n\
I:110:2:1d2:2:1:0\n\
W:1:1:0:3\n\
B:HIT:HURT:1d4\n\
F:MALE\n\
D:This one will burn for squandering the family fortune, he's very interested in your purse. \n\
D:He has not yet been judged by Minos. \n\
\n\
N:24:Demonic Scribe\n\
G:u:v\n\
I:110:1:1d2:2:1:0\n\
W:1:1:0:1\n\
B:HIT:HURT:1d4\n\
F:NEVER_MOVE | DEMON | EVIL | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_2 |\n\
S:BLINK\n\
D:This little imp teleports around, noting down \n\
D:all new visitors to Inferno. You deem it wise \n\
D:not to disturb the little imp. \n\
\n\
#Yah, this guy is rare, this could be an early char killer\n\
\n\
N:25:Off Duty scribe\n\
G:u:v\n\
I:110:1:1d2:2:1:0\n\
W:1:4:0:4\n\
B:HIT:EAT_GOLD:1d4\n\
F:NEVER_MOVE | \n\
F:IM_POIS | DEMON | EVIL | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_1 |\n\
S:BLINK | TPORT | CAUSE_1\n\
D:This little imp is off duty. \n\
D:He's wildy hopping around, shooting bolts at anything that moves. \n\
\n\
N:26:Fallen knight\n\
G:p:D\n\
I:110:1:3d5:7:10:40\n\
W:1:1:0:2\n\
B:BITE:HURT:1d2\n\
B:STING:HURT:1d2\n\
F:RAND_50 | \n\
F:WEIRD_MIND | BASH_DOOR | HURT_LITE\n\
F:GOOD | MALE \n\
D:It seems the knight has been corrupted by \n\
D:demonic powers. He now sports a large set of \n\
D:teeth, a poisonous sting and a murderous look \n\
D:in his eyes. \n\
\n\
N:27:Two Headed Jackal\n\
G:C:U\n\
I:110:1:1d4:10:3:10\n\
W:1:1:0:3\n\
B:BITE:HURT:1d1\n\
B:BITE:HURT:1d1\n\
F:ANIMAL\n\
D:It is a yapping snarling two headed jackal. You wonder if this \n\
D:is what Kerberos is all about. \n\
\n\
N:28:Bumbling Demon\n\
G:u:u\n\
I:100:2:4d6:4:35:99\n\
W:1:1:0:3\n\
B:BITE:HURT:1d3\n\
B:CRUSH:HURT:1d4\n\
F:RAND_25 | \n\
F:BASH_DOOR | \n\
F:DEMON | EVIL | HURT_LITE\n\
D:A fat looking demon with too many eye tentacles seems \n\
D:to look desperately for the exit. \n\
D:An embarrasment to the Demon Kings he has been sent \n\
D:here in the hope that some paladin will finish its \n\
D:miserable existance. \n\
\n\
N:29:Flaming icky thing\n\
G:i:r\n\
I:110:1:3d5:12:7:10\n\
W:1:1:0:3\n\
B:TOUCH:FIRE:1d2\n\
F:RAND_50 | RAND_25 | HURT_COLD | HEAL_FIRE\n\
F:EMPTY_MIND\n\
D:It is a smallish, flaming, icky creature. \n\
\n\
N:30:Whispy icky thing\n\
G:i:D\n\
I:110:1:2d5:12:6:10\n\
W:1:1:0:2\n\
B:TOUCH:HURT:1d2\n\
F:ATTR_CLEAR | \n\
F:RAND_50 | RAND_25 | \n\
F:INVISIBLE | EMPTY_MIND \n\
D:It is a smallish, whispy, icky creature. \n\
D:It looks more like a whisp of smoke then anything else. \n\
\n\
\n\
##### Level 2 #####\n\
\n\
#Dark red = Nephilim \n\
N:31:Wandering Nephilim\n\
G:P:r\n\
I:110:2:9d4:20:16:5\n\
W:2:1:0:6\n\
B:HIT:HURT:1d7\n\
F:MALE | DROP_60 | OPEN_DOOR | BASH_DOOR\n\
D:Half angel and half man he cannot enter the Heavens, \n\
D:affraid of Minos' judgement, he wanders in the lower levels. \n\
\n\
#Light red : lovers\n\
N:32:Panderer\n\
G:p:R\n\
I:110:2:9d4:20:16:5\n\
W:2:1:0:6\n\
B:HIT:HURT:1d7\n\
F:FEMALE | DROP_60 | OPEN_DOOR | BASH_DOOR | STORM\n\
S:1_IN_2 | \n\
D:She seems to be seeking new clients, \n\
D:but some devillish magic keeps her from touching any man ever again. \n\
D:She has not accepted Minos' judgement and prefers to dwell in the lower levels for eternity. \n\
\n\
#Dark Umber : Gluts , they are always slower but hit harder\n\
N:33:Glut\n\
G:p:u\n\
I:80:1:9d4:20:16:5\n\
W:2:1:0:9\n\
B:HIT:HURT:1d9\n\
B:HIT:EAT_FOOD\n\
F:MALE | DROP_60 | OPEN_DOOR | BASH_DOOR\n\
D:This one will be forced by Minos to lie in the mud under continual cold rain and hail. \n\
D:He has not accepted Minos' judgement and prefers to dwell in the lower levels for eternity. \n\
\n\
#Dark blue : Perjurer\n\
N:34:Perjurer\n\
G:p:b\n\
I:110:2:9d4:20:16:5\n\
W:2:1:0:6\n\
B:HIT:HURT:1d7\n\
F:MALE | DROP_60 | OPEN_DOOR | BASH_DOOR\n\
D:This one will be forced by Minos to be afflicted by a terrible disease. \n\
D:He has escaped Minos' judgement and prefers to dwell in the lower levels for eternity. \n\
\n\
#Light Blue : Alchemist \n\
N:35:Alchemist\n\
G:p:B\n\
I:110:2:9d4:20:16:5\n\
W:2:1:0:6\n\
B:HIT:HURT:1d7\n\
F:MALE | DROP_60 | OPEN_DOOR | BASH_DOOR\n\
D:This one will be forced by Minos to be afflicted by a terrible disease. \n\
D:He has escaped Minos' judgement and prefers to dwell in the lower levels for eternity. \n\
\n\
#Light Dark : Suicidal / Profligate\n\
N:36:Profligate\n\
G:p:D\n\
I:110:2:9d4:20:16:5\n\
W:2:1:0:6\n\
B:HIT:HURT:1d7\n\
F:MALE | DROP_60 | OPEN_DOOR | BASH_DOOR\n\
D:This one will be forced by Minos to be chased perpetually through the suicider trees by ferocious dogs. \n\
D:He has not accepted Minos' judgement and prefers to dwell in the lower levels for eternity. \n\
\n\
#Money based : yellow\n\
N:37:Counterfeiter\n\
G:p:y\n\
I:110:2:9d4:20:16:5\n\
W:2:1:0:6\n\
B:HIT:HURT:1d7\n\
F:MALE | DROP_60 | DROP_90 | OPEN_DOOR | BASH_DOOR\n\
D:This one will be forced by Minos to be afflicted by a terrible disease. \n\
D:He has escaped Minos' judgement and prefers to dwell in the lower levels for eternity. \n\
\n\
#Money based : yellow\n\
N:38:Usurer\n\
G:p:y\n\
I:110:2:9d4:20:16:5\n\
W:2:1:0:6\n\
B:HIT:HURT:1d7\n\
F:MALE | DROP_60 | DROP_90 | OPEN_DOOR | BASH_DOOR\n\
D:This one will burn for demanding unreasonable returns on loans. \n\
D:He has not accepted Minos' judgement and prefers to dwell in the lower levels for eternity. \n\
\n\
N:39:Villager\n\
G:t:g\n\
I:110:2:9d4:20:16:5\n\
W:2:1:0:6\n\
B:HIT:HURT:1d7\n\
F:MALE | DROP_60 | OPEN_DOOR | BASH_DOOR\n\
D:This enraged villager seems determined to put an end \n\
D:to all of Inferno. Better not get in his way. \n\
\n\
N:40:Small Wailer\n\
G:W:o\n\
I:110:1:2d7:20:16:10\n\
W:2:2:0:10\n\
B:WAIL:HURT:1d5\n\
F:PASS_WALL | NO_SLEEP | COLD_BLOOD | NONLIVING | NO_FEAR | EMPTY_MIND\n\
D:A childlike figure dwelling around, deafening any one in \n\
D:sight with it's piercing wails. \n\
\n\
N:41:Discord Bug\n\
G:I:v\n\
I:110:1:2d4:10:2:10\n\
W:2:1:0:4\n\
B:BITE:HURT:1d2\n\
B:CLAW:HURT:1d2\n\
F:RAND_25 | FRIENDS |\n\
F:STUPID | WEIRD_MIND |\n\
F:ANIMAL | NO_FEAR \n\
D:A strange bug; it emits sounds that form a strange \n\
D:harmony with the eversounding Discord of Inferno. \n\
\n\
N:42:Bloodmoss\n\
G:m:r\n\
I:100:2:1d10:10:3:10\n\
W:1:1:0:1\n\
B:TOUCH:HURT:1d1\n\
B:TOUCH:HURT:1d1\n\
F:NEVER_MOVE | STUPID | EMPTY_MIND | FRIENDS |\n\
F:HURT_LITE | NO_FEAR\n\
D:A strange fibrous growth springing up everywhere. \n\
D:It seems to feed on the blood of it's victims \n\
\n\
N:43:Shrieker\n\
G:g:R\n\
I:110:3:1d1:4:1:0\n\
W:2:2:0:1\n\
F:NEVER_MOVE | NEVER_BLOW | HURT_ROCK\n\
F:STUPID | EMPTY_MIND | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_4 | \n\
S:SHRIEK | SCARE\n\
D:A tall red statue producing a shrieking noise \n\
D:that intensifies Inferno's Discord. \n\
\n\
N:44:Grip, Hellhound\n\
G:C:r\n\
I:120:2:5d5:30:30:0\n\
W:2:1:0:60\n\
B:BITE:FIRE:1d2\n\
B:BITE:FIRE:1d2\n\
F:UNIQUE | \n\
F:FORCE_MAXHP | RAND_25 | \n\
F:BASH_DOOR | EVIL | IM_FIRE\n\
F:ANIMAL | NO_CONF | NO_SLEEP | NO_FEAR\n\
D:Killed and corrupted into an even more vicious creature, \n\
D:it seeks dominance over all canines of the lower levels. \n\
\n\
#2 fire attacks, double xp\n\
\n\
N:45:Fang, Hellhound\n\
G:C:r\n\
I:120:2:5d5:30:30:0\n\
W:2:1:0:60\n\
B:BITE:FIRE:1d2\n\
B:BITE:FIRE:1d2\n\
F:UNIQUE | \n\
F:FORCE_MAXHP | RAND_25 | IM_FIRE\n\
F:BASH_DOOR | EVIL\n\
F:ANIMAL | NO_CONF | NO_SLEEP | NO_FEAR\n\
D:Killed and corrupted into an even more vicious creature, \n\
D:it seeks dominance over all canines of the lower levels. \n\
\n\
##### Level 3 #####\n\
\n\
N:46:Stumbling Betrayer\n\
G:p:G\n\
I:100:1:4d4:5:4:10\n\
W:3:1:0:6\n\
B:CRAWL:HURT:1d9\n\
F:RAND_50 | \n\
F:BASH_DOOR | EVIL\n\
D:A large knife is stuck between his shoulder blades, \n\
D:yet the wound does not seem to be mortal. Instead \n\
D:the man keeps crawling in never ending agony. \n\
D:It seems the poetic justice of Inferno penetrates \n\
D:also the lower levels of Inferno. \n\
\n\
N:47:Novice Treasurehunter\n\
G:p:D\n\
I:100:2:4d4:5:4:10\n\
W:3:1:0:10\n\
B:TOUCH:EAT_GOLD\n\
B:TOUCH:EAT_GOLD\n\
B:TOUCH:EAT_GOLD\n\
F:MALE | EVIL\n\
F:DROP_60 | DROP_90 | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | \n\
D:A rather shifty individual, his strategy is hit and \n\
D:run. He rarely ventures below the first level of Inferno, \n\
D:pilfering bodies as he goes along. \n\
\n\
#e's become ectoplasm\n\
#a little bit harder, no money reward but half more xp\n\
\n\
N:48:Green ectoplasm\n\
G:e:g\n\
I:120:1:3d4:8:16:80\n\
W:3:1:0:6\n\
B:CRAWL:ACID:1d3\n\
F:RAND_50 | RAND_25 | \n\
F:STUPID | EMPTY_MIND | \n\
F:IM_ACID | IM_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
D:It's green and it's oozing. \n\
\n\
N:49:Rodent King\n\
G:r:u\n\
I:120:3:5d5:30:30:0\n\
W:3:2:0:64\n\
B:CLAW:HURT:1d2\n\
F:UNIQUE | ESCORT | ESCORTS |\n\
F:FORCE_MAXHP | BASH_DOOR | \n\
F:ANIMAL | NO_CONF | NO_SLEEP\n\
S:1_IN_6 | \n\
S:S_KIN\n\
D:A large black rodent, he's the leader of the pack. \n\
D:Minos gave him special instructions about you... \n\
\n\
N:50:Giant hungry rat\n\
G:r:W\n\
I:110:1:2d2:8:7:30\n\
W:3:1:0:1\n\
B:BITE:POISON:1d3\n\
F:RAND_25 | \n\
F:FRIENDS | ANIMAL\n\
D:It is a very vicious rodent looking for his next meal. \n\
\n\
N:51:Frostbite grashopper\n\
G:I:B\n\
I:120:1:4d5:6:6:15\n\
W:3:1:0:10\n\
B:CRAWL:COLD:1d2\n\
F:RAND_25 | HURT_FIRE | RES_COLD\n\
F:WEIRD_MIND |  \n\
F:ANIMAL | FRIENDS | \n\
S:1_IN_15 | SHRIEK\n\
D:Frosty blue they jump and shriek in random directions. \n\
D:These graphoppers seem to be carnivorous. \n\
\n\
N:52:Frostbite ant\n\
G:a:w\n\
I:110:2:3d6:8:16:80\n\
W:3:1:0:7\n\
B:BITE:COLD:1d4\n\
F:WEIRD_MIND | BASH_DOOR | HURT_FIRE | RES_COLD\n\
F:ANIMAL | \n\
D:It is about two feet long and has sharp pincers. \n\
D:It is a covered by a sheet of ice, giving it an \n\
D:otherwordly look. \n\
\n\
N:53:Dark Pool\n\
G:~:d\n\
I:110:4:10d10:2:1:0\n\
W:3:2:0:16\n\
B:CRUSH:EAT_LITE:2d4\n\
F:NEVER_MOVE | \n\
F:STUPID | EMPTY_MIND | HEAL_DARK | HURT_LITE\n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_5 | \n\
S:DARKNESS\n\
D:It keeps Inferno in shadows. \n\
\n\
N:54:Flaming Demonic Grunt\n\
G:u:R\n\
I:120:2:4d8:8:9:20\n\
W:3:1:0:12\n\
B:HIT:FIRE:1d2\n\
F:BASH_DOOR\n\
F:DEMON | EVIL | NO_FEAR | HEAL_FIRE | HURT_COLD\n\
D:A member of the demonic foot troops, this one \n\
D:carries a flaming pitch fork. \n\
\n\
N:55:Blue demonic grunt\n\
G:u:b\n\
I:120:2:4d8:8:9:20\n\
W:3:1:0:8\n\
B:HIT:COLD:1d2\n\
B:HIT:COLD:1d3\n\
F:DROP_60 | \n\
F:OPEN_DOOR | BASH_DOOR | HEAL_COLD | HURT_FIRE\n\
F:DEMON | EVIL | NO_FEAR\n\
D:A small demonic figure, it holds a frosty trident. Inferno is \n\
D:indeed a place where both frost and fire rule. \n\
\n\
N:56:Judas Iscariot\n\
G:p:b\n\
I:130:1:10d20:20:12:5\n\
W:3:3:0:20\n\
B:TOUCH:EAT_GOLD\n\
B:TOUCH:EAT_GOLD\n\
F:UNIQUE | MALE | SMART | \n\
F:RAND_50 | RAND_25 | \n\
F:ONLY_ITEM | DROP_90 | DROP_GOOD | DROP_GREAT |\n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR \n\
D:A sniveling headless wretch, burning with everlasting sorrow and regrets. \n\
D:Somehow he has managed to get in the lower levels while Satan is chewing his head. \n\
\n\
N:57:Scruffy looking banker\n\
G:p:s\n\
I:110:1:1d5:16:8:10\n\
W:3:1:0:4\n\
B:HIT:HURT:1d2\n\
B:TOUCH:EAT_GOLD\n\
B:TOUCH:EAT_GOLD\n\
F:MALE | EVIL\n\
F:DROP_60 | DROP_90\n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | \n\
D:A banker, in bedraggled clothes. His bewildered look \n\
D:is only briefly interrupted by the sight of your purse. \n\
\n\
N:58:Northerlight ectoplasm\n\
G:e:R\n\
I:110:1:3d6:2:6:10\n\
W:3:1:0:6\n\
B:GAZE:LOSE_STR:1d2\n\
B:GAZE:LOSE_STR:1d2\n\
B:GAZE:LOSE_STR:1d2\n\
F:RAND_50 | RAND_25 | \n\
F:STUPID | EMPTY_MIND | ATTR_MULTI | NO_FEAR | RES_FIRE\n\
S:1_IN_11 | \n\
S:DRAIN_MANA | DARKNESS | \n\
D:A swirling mass of ectoplasm, changing constantly its hue. \n\
D:It seems to suck up all the energy around it. \n\
\n\
N:59:Screeching harpy\n\
G:h:w\n\
I:110:4:2d5:16:17:10\n\
W:3:1:0:7\n\
B:CLAW:HURT:1d2\n\
B:CLAW:HURT:1d2\n\
B:BITE:HURT:1d2\n\
F:FEMALE | \n\
F:RAND_50 | \n\
F:ANIMAL | EVIL | RES_DARK\n\
D:A flying, screeching bird with a woman's face. \n\
D:She is attracted by Inferno's Discord. \n\
\n\
N:60:Large flaming snake\n\
G:J:r\n\
I:90:1:6d8:6:41:50\n\
W:3:1:0:14\n\
B:BITE:FIRE:1d5\n\
B:CRUSH:HURT:1d8\n\
F:RAND_25 | \n\
F:BASH_DOOR | \n\
F:ANIMAL | FRIENDS | ATTR_MULTI | RES_FIRE\n\
D:A flame covered reptile it is about ten feet long. \n\
\n\
\n\
##### Level 4 #####\n\
\n\
\n\
N:61:Biting copper coins\n\
G:$:u\n\
I:100:1:7d8:3:24:10\n\
W:4:1:0:15\n\
B:HIT:HURT:1d4\n\
B:TOUCH:POISON:2d2\n\
B:TOUCH:ACID:2d2\n\
F:ONLY_GOLD | DROP_1D2 | \n\
F:COLD_BLOOD | BASH_DOOR | \n\
F:ANIMAL | RES_POIS | FRIENDS | \n\
F:NO_CONF | NO_SLEEP \n\
D:It is a pile of coins with an attitude. \n\
D:The greedy cannot resist approaching them, \n\
D:suffering bite marks, poison and acid for eternity. \n\
\n\
#Dark red = murder \n\
N:62:Murderer\n\
G:p:r\n\
I:110:2:9d4:20:16:5\n\
W:4:1:0:6\n\
B:HIT:HURT:1d7\n\
F:MALE | DROP_90 | OPEN_DOOR | BASH_DOOR | FRIENDS | \n\
D:This man has no idea what he is doing here. \n\
D:You get the impression he's not here by mistake though. \n\
D:He has not accepted Minos' judgement and prefers to dwell in the lower levels for eternity to face eternity with his fellow sinners. \n\
\n\
#Dark Umber : Gluts , they are always slower but hit harder\n\
N:63:Glut\n\
G:p:u\n\
I:80:1:9d4:20:16:5\n\
W:4:1:0:9\n\
B:HIT:HURT:1d9\n\
B:HIT:EAT_FOOD\n\
F:MALE | DROP_90 | OPEN_DOOR | BASH_DOOR | FRIENDS | \n\
D:This one will be forced by Minos to lie in the mud under continual cold rain and hail. \n\
D:He has not accepted Minos' judgement and prefers to dwell in the lower levels for eternity to face eternity with his fellow sinners. \n\
\n\
#Light Umber : Sloth , they are always slower, hit softer. less xp\n\
N:64:Sloth\n\
G:p:U\n\
I:80:1:9d4:20:16:5\n\
W:4:1:0:3\n\
B:HIT:HURT:1d4\n\
F:MALE | DROP_90 | OPEN_DOOR | BASH_DOOR | FRIENDS | \n\
D:This one will be forced by Minos to remained trappped in the river Styx. \n\
D:He has not accepted Minos' judgement and prefers to dwell in the lower levels for eternity to face eternity with his fellow sinners. \n\
\n\
#Light Dark : Heretics \n\
N:65:Heretic\n\
G:p:D\n\
I:110:2:9d4:20:16:5\n\
W:4:1:0:6\n\
B:HIT:HURT:1d7\n\
F:MALE | DROP_90 | OPEN_DOOR | BASH_DOOR | FRIENDS | \n\
D:This one will be forced by Minos to remained trappped inside a burning tomb. \n\
D:He has not accepted Minos' judgement and prefers to dwell in the lower levels for eternity to face eternity with his fellow sinners. \n\
\n\
#Light Dark : Suicidal \n\
N:66:Suicidal\n\
G:p:D\n\
I:110:2:9d4:20:16:5\n\
W:4:1:0:6\n\
B:HIT:HURT:1d7\n\
F:MALE | DROP_90 | OPEN_DOOR | BASH_DOOR | FRIENDS | \n\
D:This one will be forced by Minos to be turned into a thorny black tree. \n\
D:He has not accepted Minos' judgement and prefers to dwell in the lower levels for eternity to face eternity with his fellow sinners. \n\
\n\
N:67:Hoarder\n\
G:p:y\n\
I:110:2:9d4:20:16:5\n\
W:4:1:0:6\n\
B:HIT:HURT:1d7\n\
F:MALE | DROP_60 | DROP_90 | OPEN_DOOR | BASH_DOOR | FRIENDS | \n\
D:This one will burn for hoarding, he's very interested in your purse. \n\
D:He has not accepted Minos' judgement and prefers to dwell in the lower levels for eternity to face eternity with his fellow sinners. \n\
\n\
N:68:Squanderer\n\
G:p:y\n\
I:110:2:9d4:20:16:5\n\
W:4:1:0:6\n\
B:HIT:HURT:1d7\n\
F:MALE | DROP_60 | DROP_90 | OPEN_DOOR | BASH_DOOR | FRIENDS | \n\
D:This one will burn for squandering the family fortune, he's very interested in your purse. \n\
D:He has not accepted Minos' judgement and prefers to dwell in the lower levels for eternity to face eternity with his fellow sinners. \n\
\n\
#Yay, one mean freeze worm\n\
#They come in numbers, but are slow\n\
#They will suck your heat out,\n\
#killing your potions, your strength and your constitution\n\
\n\
N:69:frostbite worm\n\
G:w:b\n\
I:80:1:5d8:7:12:10\n\
W:4:1:0:10\n\
B:CRAWL:COLD:1d2\n\
B:CRAWL:COLD:1d2\n\
B:CRAWL:LOSE_STR:1d2\n\
B:CRAWL:LOSE_CON:1d2\n\
F:RAND_50 | RAND_25 | FORCE_SLEEP\n\
F:STUPID | WEIRD_MIND | RES_COLD | HURT_FIRE\n\
F:COLD_BLOOD | FRIENDS | \n\
F:ANIMAL |  \n\
D:This worm will eat you, slowly, while you freeze and weaken. \n\
\n\
N:70:Grinding Teeth\n\
G:g:w\n\
I:110:2:6d8:20:6:5\n\
W:4:3:0:18\n\
B:HIT:HURT:1d5\n\
B:HIT:HURT:3d1\n\
F:FORCE_SLEEP | BASH_DOOR | NO_CONF | NO_FEAR | NO_SLEEP | NONLIVING | HURT_ROCK\n\
D:The grinding teeth of Hell are not a metaphore. It really is an  oversized set of teeth with an attitude. \n\
\n\
##### Level 5 , Minos' Dwelling#####\n\
\n\
N:71:Mercenary Excorcist\n\
G:p:W\n\
I:120:2:22d8:2:1:99\n\
W:5:1:0:35\n\
B:HIT:HURT:1d8\n\
F:NO_CONF | NO_SLEEP | NO_FEAR | GOOD\n\
S:1_IN_3 | \n\
S:BR_LITE | BLINK | TPORT\n\
D:One of the very few individuals to dare hit and run missions in Inferno. \n\
\n\
N:72:Road robber\n\
G:p:G\n\
I:110:2:3d5:10:10:10\n\
W:5:1:0:15\n\
B:HIT:HURT:1d6\n\
F:DROP_60 | DROP_90 | DROP_1D2\n\
F:OPEN_DOOR | BASH_DOOR | FRIENDS\n\
S:1_IN_8\n\
S:ARROW_1\n\
D:Even in Inferno these men hunt together. \n\
D:In their quest for robbing all newcomers, they try to \n\
D:stay away from the biting copper coins. \n\
\n\
#yah, fear the green succubus, for she will come\n\
#straight for you. Same xp, but better treasure.\n\
N:73:Green succubus\n\
G:u:g\n\
I:110:2:9d8:18:40:120\n\
W:5:1:0:30\n\
B:CRUSH:HURT:1d8\n\
B:SPIT:ACID:2d6\n\
F:FEMALE | \n\
F:TAKE_ITEM | DROP_90 | DROP_1D2\n\
F:BASH_DOOR | EVIL | IM_ACID | DEMON\n\
D:A large green female demon. Her green skin glistens with biting \n\
D:acid. She is part of Minos' harem. \n\
\n\
N:74:Black succubus\n\
G:u:D\n\
I:110:2:9d8:18:40:120\n\
W:5:1:0:30\n\
B:CRUSH:HURT:1d8\n\
B:SPIT:EAT_LITE:2d6\n\
F:FEMALE | \n\
F:TAKE_ITEM | DROP_90 | DROP_1D2\n\
F:BASH_DOOR | EVIL | HEAL_DARK | DEMON\n\
D:A large black female demon. Her skin looks as if she \n\
D:came straight out of a dark pool. \n\
\n\
N:75:Icy Lantern Holder\n\
G:p:b\n\
I:110:1:3d4:8:16:80\n\
W:5:1:0:7\n\
B:HIT:COLD:1d2\n\
B:HIT:HURT:1d2\n\
F:RAND_50 | RAND_25 | DROP_60 | STUPID | \n\
D:This man is punished for his lazyness. Now he must for eternity \n\
D:light up inferno with an icy cold lantern. There's not a lot of \n\
D:fight left in him. \n\
\n\
N:76:Copperhead snake\n\
G:J:o\n\
I:110:2:4d6:6:20:1\n\
W:5:1:0:15\n\
B:BITE:POISON:2d4\n\
F:RAND_50 | \n\
F:BASH_DOOR | FRIENDS | \n\
F:ANIMAL | IM_POIS\n\
D:It has a copper head and sharp venomous fangs. \n\
\n\
N:77:Angel Guide\n\
G:A:D\n\
I:110:1:13d9:20:32:30\n\
W:5:1:0:25\n\
B:HIT:HURT:1d10\n\
F:DROP_90 | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_POIS | FALLEN_ANGEL\n\
D:An angel of the Third Triad, it dwells in the lower level of Hell, \n\
D:acting out the orders of Minos. He beats sinners into submission \n\
D:and then drags them toward the Circle they belong to. \n\
\n\
N:78:Neutral Angel\n\
G:A:b\n\
I:110:1:13d9:20:32:30\n\
W:5:1:0:25\n\
B:HIT:HURT:1d10\n\
F:DROP_90 | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
D:An angel that refused to participate \n\
D:in the great batlle of Lucifer. It must \n\
D:dwell now forever in Limbo. \n\
\n\
N:79:Throne Defender\n\
G:A:D\n\
I:110:1:13d9:20:32:30\n\
W:5:1:0:25\n\
B:HIT:HURT:1d10\n\
F:DROP_90 | FORCE_SLEEP | FRIENDS\n\
F:EVIL | IM_POIS | FALLEN_ANGEL\n\
D:An angel of the Third Triad, its' duty is to defend Minos' Throne. \n\
\n\
N:80:Aeacean Bodyguard\n\
G:u:y\n\
I:110:1:13d9:20:32:30\n\
W:5:1:0:25\n\
B:HIT:HURT:1d10\n\
F:DROP_90 | FORCE_SLEEP | FRIENDS | BASH_DOOR | \n\
F:EVIL | IM_POIS | DEMON\n\
D:A lesser demon, it's duty is to protect \n\
D:Aeacus from the more dangerous sinners who are sometimes quite resourcefull. \n\
D:Aecus is a brother of Minos, judging European souls and selecting their punishment. \n\
\n\
N:81:Rhadamanthean Bodyguard\n\
G:u:W\n\
I:110:1:13d9:20:32:30\n\
W:5:1:0:25\n\
B:HIT:HURT:1d10\n\
F:DROP_90 | FORCE_SLEEP | FRIENDS | BASH_DOOR | \n\
F:EVIL | IM_POIS | DEMON\n\
D:A lesser demon, it's duty is to protect \n\
D:Rhadamanthus from the more dangerous sinners who are sometimes quite resourcefull. \n\
D:Rhadamanthus is a brother of Minos, judging Asian souls and selecting their punishment. \n\
\n\
N:82:Bodyguard\n\
G:u:G\n\
I:110:1:13d9:20:32:30\n\
W:5:1:0:25\n\
B:HIT:HURT:1d15\n\
F:DROP_90 | FORCE_SLEEP | FRIENDS | BASH_DOOR | \n\
F:EVIL | IM_POIS | DEMON\n\
D:A demon, it's duty is to protect \n\
D:Minos from the more dangerous sinners who are sometimes quite resourcefull. \n\
D:Minos is the most known Infernal judge, his vote is final. \n\
\n\
#Benvenuto continues, and find in these four principal poets the four cardinal virtues, with Homer signifying justice, Horace prudence, Ovid temperance, and \n\
Lucan fortitude.\n\
\n\
#Justice knows no fear\n\
N:83:Homer, Sovereign Poet\n\
G:p:W\n\
I:110:2:13d9:20:32:30\n\
W:5:2:0:30\n\
B:HIT:HURT:1d15\n\
F:GOOD | DROP_GOOD | NO_FEAR | DROP_1D2 | ONLY_ITEM | UNIQUE | MALE |  \n\
D:A great poet, but never baptised he has been granted access to Minos' \n\
D:residence. He will always be remembered for the Iliad and the Odyssey. \n\
D:He sees a great danger in your actions. \n\
\n\
#Prudence, he is a SMART unique, hopefully fleeing fast enough \n\
N:84:Quintus Horatius Flaccus, Scholar\n\
G:p:W\n\
I:110:2:13d9:20:32:30\n\
W:5:2:0:30\n\
B:HIT:HURT:1d15\n\
F:GOOD | DROP_GOOD | SMART | DROP_1D2 | ONLY_ITEM | UNIQUE | MALE | \n\
D:A great scholar and poet, but never baptised he has been granted access to Minos' \n\
D:residence. He is most famous for his 'Odes'. \n\
D:He firmly disapproves your actions. \n\
\n\
#Temperance, a little les xp, a little less damage\n\
N:85:Publius Ovidius Naso\n\
G:p:W\n\
I:110:2:13d9:20:32:30\n\
W:5:2:0:25\n\
B:HIT:HURT:1d10\n\
F:GOOD | DROP_GOOD | DROP_1D2 | ONLY_ITEM | UNIQUE | MALE |   \n\
D:A great Roman poet, but never baptised he has been granted access to Minos' \n\
D:residence. He is most famous for his 'Ars Amatoria' and 'Metamorphoses' \n\
D:He is wary about your actions. \n\
\n\
#Fortitude, a little more xp\n\
N:86:Marcus Annaeus Lucanus, Master Poet\n\
G:p:W\n\
I:110:2:14d10:20:32:30\n\
W:5:2:0:30\n\
B:HIT:HURT:1d15\n\
F:GOOD | DROP_GOOD | DROP_1D2 | ONLY_ITEM | UNIQUE | MALE | \n\
D:A great poet and historian, but never baptised he has been granted access to Minos' \n\
D:residence. He is most famous for 'Bellum civile' and is one of the outstanding figures of the Silver Latin period. \n\
D:He sees no good in your actions. \n\
\n\
N:87:Pope Celestine V\n\
G:p:W\n\
I:110:2:14d10:20:32:30\n\
W:5:3:0:40\n\
B:HIT:HURT:1d15\n\
F:GOOD | DROP_GREAT | DROP_1D2 | ONLY_ITEM | UNIQUE | MALE | \n\
S:1_IN_3 | \n\
S:HASTE | SLOW | BO_FIRE | HEAL | SCARE\n\
D:He is one of the popes that abdicated and even instituted \n\
D:abdication as a papal right. He lives in Minos' dwelling. \n\
\n\
N:88:Pope Liberius\n\
G:p:W\n\
I:110:2:14d10:20:32:30\n\
W:5:3:0:40\n\
B:HIT:HURT:1d15\n\
F:GOOD | DROP_GREAT | DROP_1D2 | ONLY_ITEM | UNIQUE | MALE | \n\
S:1_IN_3 | \n\
S:HASTE | SLOW | BO_FIRE | HEAL | SCARE\n\
D:This pope abdicated in AD 366. He lives in Minos' dwelling. \n\
\n\
N:89:Pope Benedict IX\n\
G:p:W\n\
I:110:2:14d10:20:32:30\n\
W:5:3:0:40\n\
B:HIT:HURT:1d15\n\
F:GOOD | DROP_GREAT | DROP_1D2 | ONLY_ITEM | UNIQUE | MALE | \n\
S:1_IN_3 | \n\
S:HASTE | SLOW | BO_FIRE | HEAL | SCARE \n\
D:This pope abdicated in AD 1044 because of his desorderly life so he could join a monastery. \n\
D:He lives in Minos' dwelling. \n\
\n\
N:90:Pope Marcellinus\n\
G:p:W\n\
I:110:2:14d10:20:32:30\n\
W:5:3:0:40\n\
B:HIT:HURT:1d15\n\
F:GOOD | DROP_GREAT | DROP_1D2 | ONLY_ITEM | UNIQUE | MALE | \n\
S:1_IN_3 | \n\
S:HASTE | SLOW | BO_FIRE | HEAL | SCARE | \n\
D:This pope abdicated in AD 308. He has been a martyr for his faith. \n\
D:He lives in Minos' dwelling. \n\
\n\
######### Inferno   ####################\n\
##### Level 6 , The Lovers' Level  #####\n\
\n\
#Semiramis\n\
#THelen of Troy\n\
#Cleopatra \n\
#Dido, for example, was a mythological queen who committed suicide because of her unrequited love for Aeneas. \n\
#Maenads were female worshippers of Dionysus, the Greek god of mystery, wine and intoxication, and the Roman god Bacchus. The word literally translates as \n\
'raving ones'. They were known as wild, insane women who could not be reasoned with. The mysteries of Dionysus inspired the women to ecstatic frenzy; they \n\
indulged in copious amounts of violence, bloodletting, sex and self-intoxication and mutilation. They were usually pictured as crowned with vine leaves, \n\
clothed in fawnskins and carrying the thyrsus, and dancing with the wild abandonment of complete union with primeval nature.\n\
# Vannozza (Giovanna) dei Cattani (1442 - ?) was one of the many mistresses of the Pope Alexander VI, whose relationship lasted the longest.\n\
\n\
N:91:Eelilim\n\
G:u:u\n\
I:110:2:5d5:30:30:20\n\
W:8:6:0:6\n\
G:u:s\n\
I:110:1:13d9:20:32:30\n\
W:6:1:0:25\n\
B:HIT:HURT:1d6\n\
B:HIT:LOSE_CON:1d2\n\
B:HIT:LOSE_CHA:1d2\n\
F:FRIENDS | DROP_60 | DROP_90 | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | DEMON | HURT_LITE\n\
D:Incarnations of Vanity, they now punish the sinners \n\
D:they have lured into Inferno. \n\
\n\
N:92:Biting silver coins\n\
G:$:s\n\
I:100:1:12d8:4:30:10\n\
W:6:1:0:18\n\
B:HIT:HURT:1d6\n\
B:TOUCH:POISON:2d3\n\
B:TOUCH:ACID:2d3\n\
F:ONLY_GOLD | DROP_60 | DROP_1D2 | \n\
F:COLD_BLOOD | BASH_DOOR | FRIENDS |\n\
F:ANIMAL | IM_POIS | NO_CONF | NO_SLEEP\n\
D:It is a pile of coins, crawling forward on thousands of tiny legs, \n\
D:corroding everything with poison and acid. Somehow it seems to \n\
D:focus on those that received payment for pleasure. \n\
\n\
# Francesca da Rimini , as in dante's story\n\
N:93:Francesca da Rimini\n\
G:p:o\n\
I:110:3:15d10:20:20:20\n\
W:6:1:0:100\n\
B:HIT:HURT:1d10\n\
F:UNIQUE | FEMALE | \n\
F:FORCE_MAXHP | ESCORT | \n\
F:DROP_1D2 | ONLY_GOLD | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
D:She was the beautiful daughter of Guido da Polenta of Ravenna. \n\
D:She had an arranged marriage with Giovanni Malatesta of Rimini, the Malatestan heir. \n\
D:Giovanni was brave but lame and deformed. Her father knew Francesca would refuse him, \n\
D:so the marriage was proxy through Paolo, handsome brother of Giovanni. \n\
D:Francesca fell in love with Paolo and was unaware of the deception until the morning after the wedding day. \n\
D:Paolo and Francesca became lovers after being seduced by their reading of the story of Lancelot and Guinevere. \n\
D:They were subsequently surprised and murdered by Giovanni in 1285. \n\
\n\
N:94:Rajm Casualty\n\
G:p:R\n\
I:90:1:8d4:4:12:5\n\
W:6:1:0:6\n\
B:HIT:HURT:1d6\n\
F:OPEN_DOOR | BASH_DOOR | FEMALE | \n\
F:STORM\n\
S:1_IN_3\n\
D:Rajm is an Arabic term that means to stone, it is the punishment of adultery. \n\
D:This poor woman was stoned to death. \n\
\n\
N:95:Fornicator\n\
G:p:R\n\
I:110:1:8d8:20:32:30\n\
W:6:1:0:16\n\
B:HIT:HURT:1d8\n\
F:MALE | DROP_60 | DROP_90 | ONLY_GOLD\n\
F:OPEN_DOOR | BASH_DOOR | MALE | \n\
F:STORM\n\
S:1_IN_3\n\
D:This poor devil had sex outside of marriage. \n\
\n\
N:96:Horned Lizard\n\
G:R:U\n\
I:110:1:3d4:20:4:15\n\
W:6:2:0:2\n\
B:BITE:HURT:1d1\n\
F:ANIMAL | FRIENDS\n\
D:This small lizard with an excessively sized horn seems to seek \n\
D:carnal pleasures. \n\
\n\
N:97:Horned cave lizard\n\
G:R:u\n\
I:110:1:3d6:8:16:80\n\
W:6:2:0:8\n\
B:BITE:HURT:1d5\n\
F:ANIMAL | FRIENDS\n\
D:It is the armoured cousin of the horned lizard with a powerful bite. \n\
\n\
N:98:Roman Whore\n\
G:p:R\n\
I:90:1:8d4:4:12:5\n\
W:6:1:0:6\n\
B:HIT:HURT:1d6\n\
F:OPEN_DOOR | BASH_DOOR | FEMALE | FRIENDS |  DROP_60 | DROP_90 | ONLY_GOLD | FEMALE | \n\
F:STORM\n\
S:1_IN_3\n\
D:These women were known throughout the civilized world, leading to \n\
D:the word 'fornication' itself. Fornix, meaning 'an archway' or 'vault' \n\
D:was the place where they could be solicited. \n\
\n\
N:99:Johann Burchard\n\
G:p:R\n\
I:130:4:10d10:20:15:20\n\
W:6:3:0:40\n\
B:CRUSH:HURT:1d8\n\
F:UNIQUE | OPEN_DOOR | BASH_DOOR | NO_FEAR\n\
F:ONLY_GOLD | DROP_1D2 | DROP_2D2 | MALE |  \n\
D:He was the papal Master of Ceremonies of Pope Alexander VI in 1501, he organized the \n\
D:infamous Ballet of Chestnuts. \n\
\n\
N:100:Pope Alexander VI\n\
G:p:o\n\
I:130:4:10d10:20:15:20\n\
W:6:3:0:40\n\
B:CRUSH:HURT:1d8\n\
F:UNIQUE | OPEN_DOOR | BASH_DOOR | NO_FEAR\n\
F:ONLY_GOLD | DROP_1D2 | DROP_2D2 | MALE |  \n\
D:He is the most controversial of the secular popes of the Renaissance, \n\
D:whose surname became a byword for low standards in the medieval papacy. \n\
\n\
N:101:Hierodule \n\
G:p:v\n\
I:110:1:7d4:20:10:5\n\
W:6:2:0:7\n\
B:HIT:HURT:1d5\n\
F:FEMALE | FORCE_SLEEP | \n\
F:FRIENDS | DROP_60 | DROP_90 | ONLY_GOLD | \n\
F:OPEN_DOOR | FEMALE |  \n\
S:1_IN_12 | \n\
S:HEAL | SCARE | CAUSE_1\n\
D:Hierodules are Greek temple prostitutes, \n\
D:these ones seem to have magical powers. \n\
\n\
N:102:Anahita\n\
G:U:v\n\
I:120:3:15d10:20:20:20\n\
W:6:3:0:150\n\
B:HIT:HURT:1d10\n\
F:UNIQUE | FEMALE | \n\
F:FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_GREAT | DROP_3D2\n\
F:OPEN_DOOR | BASH_DOOR | SMART | FEMALE |  \n\
S:1_IN_6 | \n\
S:HEAL | SCARE | CAUSE_1\n\
D:Together with Astoreth she was venerated as the goddess \n\
D:of love and war. Now she dwells in Inferno. \n\
\n\
N:103:Astoreth\n\
G:U:v\n\
I:120:3:15d10:20:20:20\n\
W:6:3:0:150\n\
B:HIT:HURT:1d10\n\
F:UNIQUE | FEMALE | \n\
F:FORCE_MAXHP |  \n\
F:ONLY_ITEM | DROP_GREAT | DROP_3D2\n\
F:OPEN_DOOR | BASH_DOOR | SMART | FEMALE |  \n\
S:1_IN_6 | \n\
S:HEAL | SCARE | CAUSE_1\n\
D:She is a female demon of lust. \n\
D:He head is adorned with cow's horns, she stands naked before you. \n\
\n\
N:104:Phoenician temple prostitute\n\
G:p:R\n\
I:110:1:8d8:20:32:30\n\
W:6:2:0:16\n\
B:HIT:HURT:1d8\n\
F:FEMALE | DROP_60 | DROP_90 | ONLY_GOLD | \n\
F:OPEN_DOOR | BASH_DOOR | FEMALE | \n\
D:This is not a common temple prostitute, \n\
D:it is best not to sollicit her attention. \n\
\n\
N:105:Canaanite Prostitute\n\
G:p:R\n\
I:110:1:8d8:20:32:30\n\
W:6:2:0:16\n\
B:HIT:HURT:1d8\n\
F:MALE | DROP_60 | DROP_90 | ONLY_GOLD | \n\
F:OPEN_DOOR | BASH_DOOR | FEMALE | \n\
F:STORM\n\
S:1_IN_3\n\
D:This male prostitute offered services to men \n\
D:in his home country. \n\
\n\
N:106:Qedeshim\n\
G:p:R\n\
I:110:1:8d8:20:32:30\n\
W:6:2:0:16\n\
B:HIT:HURT:1d8\n\
F:MALE | DROP_60 | DROP_90 | \n\
F:OPEN_DOOR | BASH_DOOR | MALE | \n\
D:These pagan priests regularly engaged in homosexual acts. \n\
\n\
N:107:Solon\n\
G:p:o\n\
I:130:4:10d10:20:15:20\n\
W:6:3:0:40\n\
B:CRUSH:HURT:1d8\n\
F:UNIQUE | OPEN_DOOR | BASH_DOOR | NO_FEAR\n\
F:ONLY_GOLD | DROP_1D2 | DROP_2D2 | MALE |  \n\
D:Archon of Attica ,  the founder of the pederastic educational tradition in Athens, \n\
D:and composed poetry praising the love of boys. instituted the first Athenian brothels \n\
D:(oik`iskoi) in the 6th century BC. \n\
\n\
N:108:Qedesh, goddess of Prostitution\n\
G:U:v\n\
I:120:3:15d10:20:20:20\n\
W:6:3:0:150\n\
B:HIT:HURT:1d10\n\
F:UNIQUE | FEMALE | \n\
F:FORCE_MAXHP |  \n\
F:ONLY_ITEM | DROP_GREAT | DROP_3D2 | \n\
F:OPEN_DOOR | BASH_DOOR | SMART | FEMALE |  \n\
S:1_IN_6 | \n\
S:HEAL | SCARE | CAUSE_1\n\
D:Also known as Aprodites Pandemo, she has been banished to Inferno. \n\
\n\
N:109:Priestesses of Inanna\n\
G:p:R\n\
I:120:2:6d8:20:10:5\n\
W:6:2:0:20\n\
B:HIT:HURT:1d4\n\
F:FEMALE | FORCE_SLEEP | \n\
F:ONLY_GOLD | DROP_1D2 | DROP_2D2 | \n\
F:OPEN_DOOR | BASH_DOOR | FEMALE | \n\
S:1_IN_3 | \n\
S:ARROW_1\n\
D:TThese temple slaves hunt their men. \n\
\n\
N:110:Pope John XII\n\
G:p:o\n\
I:130:4:10d10:20:15:20\n\
W:6:3:0:40\n\
B:CRUSH:HURT:1d8\n\
F:UNIQUE | OPEN_DOOR | BASH_DOOR | NO_FEAR\n\
F:ONLY_GOLD | DROP_1D2 | DROP_2D2 | MALE |  \n\
D:This pope turned the Basilica di San Giovanni in Laterano into a \n\
D:brothel and was accused of adultery and fornication. \n\
\n\
N:111:Pope Paul III\n\
G:p:o\n\
I:130:4:10d10:20:15:20\n\
W:6:3:0:40\n\
B:CRUSH:HURT:1d8\n\
F:UNIQUE | OPEN_DOOR | BASH_DOOR | NO_FEAR\n\
F:ONLY_GOLD | DROP_1D2 | DROP_2D2 | MALE |\n\
D:Pope Paul III (1534-1549) held off ordination in order to continue his promiscuous lifestyle, \n\
D:fathering four illegitimate children by his mistress. His nickname was 'Cardinal Petticoat' \n\
D:because his sister Giulia had been Alexander VI's mistress. He made his illegitimate son \n\
D:Pier Luigi Farnese the first Duke of Parma. \n\
\n\
N:112:Concubine\n\
G:p:R\n\
I:90:1:8d4:4:12:5\n\
W:6:1:0:6\n\
B:HIT:HURT:1d6\n\
F:OPEN_DOOR | BASH_DOOR | FEMALE | \n\
F:STORM\n\
S:1_IN_3\n\
D:She had sex outside a marriage and cannot ascend to the Heavens. \n\
\n\
N:113:Adulterer\n\
G:p:R\n\
I:110:1:8d8:20:32:30\n\
W:6:2:0:16\n\
B:HIT:HURT:1d8\n\
F:MALE | DROP_60 | DROP_90 | \n\
F:OPEN_DOOR | BASH_DOOR | MALE |\n\
F:STORM\n\
S:1_IN_3\n\
D:This poor sod cheated on his wife. \n\
\n\
N:114:Whencher\n\
G:p:R\n\
I:110:1:8d8:20:32:30\n\
W:6:2:0:16\n\
B:HIT:HURT:1d8\n\
F:MALE | DROP_60 | DROP_90 | \n\
F:OPEN_DOOR | BASH_DOOR | MALE | \n\
F:STORM\n\
S:1_IN_3\n\
D:These poor devil visited a prostitute and now pays a heavy price for it. \n\
\n\
N:115:Jade\n\
G:p:R\n\
I:90:1:8d4:4:12:5\n\
W:6:1:0:6\n\
B:HIT:HURT:1d6\n\
F:OPEN_DOOR | BASH_DOOR | FEMALE | \n\
F:STORM\n\
S:1_IN_3\n\
D:She has committed adultery, which bars her from the Heavens. \n\
\n\
N:116:Citizen of Sodom\n\
G:p:r\n\
I:110:1:8d8:20:32:30\n\
W:6:2:0:16\n\
B:HIT:HURT:1d8\n\
F:DROP_60 | DROP_90 | \n\
F:OPEN_DOOR | BASH_DOOR | MALE | \n\
D:Afer the destruction of Sodom, all citizens were placed \n\
D:in the second circle of Inferno. \n\
\n\
N:117:Citizen of Gomorrah\n\
G:p:r\n\
I:110:1:8d8:20:32:30\n\
W:6:2:0:16\n\
B:HIT:HURT:1d8\n\
F:DROP_60 | DROP_90 | \n\
F:OPEN_DOOR | BASH_DOOR | MALE | \n\
D:Afer the destruction of Gomorrah, all citizens were placed \n\
D:in the second circle of Inferno. \n\
\n\
N:118:Scarred beauty\n\
G:p:v\n\
I:110:1:15d8:2:12:99\n\
W:6:1:0:20\n\
B:HIT:HURT:1d2\n\
B:HIT:CONFUSE:1d2\n\
F:NEVER_MOVE | EMPTY_MIND | FEMALE\n\
D:Chained to the floor this lady has seen better days. \n\
D:Her punishment consists of scars and the loss of legs. \n\
D:It is best not to look at her directly if you want to keep \n\
D:your sanity. \n\
\n\
######### Inferno   ####################\n\
##### Level 7 , The Gluttons' Level  ###\n\
\n\
#Statius > Not enough known\n\
#Bonagiunta da Lucca - > Not enough known\n\
#Beelzebub: Gluttony (lord of the flies) , hmmmm, beelzebub is a tough guy, maybe I will put him in a palace ?\n\
#Folke the Fat of Bjälbo . Dante did not care for Nordics really , so it does not really fit\n\
#Stenkil , a great archer. Dante did not care for Nordics really , so it does not really fit\n\
\n\
N:119:Rat\n\
G:r:b\n\
I:120:1:8d8:30:25:10\n\
W:7:2:0:35\n\
B:CLAW:HURT:1d3\n\
B:CLAW:HURT:1d4\n\
B:CLAW:POISON:1d1\n\
F:ANIMAL | NO_FEAR | FRIENDS | RES_DARK\n\
D:Legend has it that the gluttons can only feed \n\
D:on rats, toads and snakes. However, since most \n\
D:gluttons barely move, these rats have taken an attitude. \n\
\n\
N:120:Snake\n\
G:J:D\n\
I:120:2:10d8:10:32:1\n\
W:7:2:0:40\n\
B:BITE:POISON:4d4\n\
F:RAND_50 | ANIMAL | RES_POIS | FRIENDS\n\
D:It has glistening black skin, a sleek body and highly venomous fangs. \n\
\n\
N:121:Frog\n\
G:F:G\n\
I:100:2:10d8:10:32:1\n\
W:7:2:0:20\n\
B:BITE:POISON:1d3\n\
B:BITE:POISON:1d3\n\
F:RAND_50 | ANIMAL | FRIENDS\n\
D:It has glistening green skin, its' mouth is sporting a barbed tongue ready to have lunch. \n\
\n\
N:122:Glutton Shade\n\
G:p:u\n\
I:100:2:8d8:1:20:250\n\
W:7:1:0:15\n\
B:CLAW:POISON:1d3\n\
B:HIT:POISON:1d3\n\
F:FRIENDS | PASS_WALL | HURT_LITE\n\
F:NO_CONF | NO_FEAR | EMPTY_MIND\n\
D:Gluttons, forced by Cerberus to lie in the mud under continual cold rain and hail. \n\
D:They have almost faded out of existence, only a shade of their former selves. \n\
D:Legend has it that they feed themselves with the feces that drop out of the sky \n\
D:and the rats, snakes and frogs that run around. \n\
\n\
N:123:Failed Poet\n\
G:p:o\n\
I:110:2:8d8:20:20:20\n\
W:7:4:0:20 \n\
B:HIT:HURT:1d3\n\
B:HIT:HURT:1d3\n\
F:DROP_60 | DROP_90 | MALE\n\
D:He fails to see why Minos chose the Second Circle as a proper punishment. \n\
\n\
N:124:Cannibal\n\
G:p:R\n\
I:120:1:8d8:30:25:10\n\
W:7:3:0:35\n\
B:CLAW:HURT:1d3\n\
B:CLAW:HURT:1d4\n\
B:CLAW:BLIND:1d1\n\
F:NO_FEAR | DROP_60 | MALE\n\
D:Little known is it that cannibals are also in the second circle, \n\
D:frustrated by the Glutton shades they hunt each other and the odd \n\
D:adventurer. \n\
\n\
N:125:Maenad\n\
G:p:v\n\
I:120:1:8d8:30:25:10\n\
W:7:3:0:35\n\
B:CLAW:HURT:1d3\n\
B:CLAW:HURT:1d4\n\
B:CLAW:BLIND:1d1\n\
F:NO_FEAR | DROP_60 | FEMALE | DROP_90 | FRIENDS\n\
D:Maenads are the female worshippers of Dionysus, the god of mystery, wine and intoxication. \n\
D:The word literally translates as 'raving ones'. They were known as wild, insane women who could not be reasoned with. \n\
D:The mysteries of Dionysus inspired the women to ecstatic frenzy; they indulged in copious amounts of violence, bloodletting, \n\
D:sex and self-intoxication and mutilation. They usually are crowned with vine leaves, clothed in fawnskins and carrying the thyrsus. \n\
D:They dance with the wild abandonment of complete union with primeval nature. \n\
\n\
N:126:Agave\n\
G:p:v\n\
I:120:4:19d10:20:32:30\n\
W:7:3:0:120\n\
B:CLAW:HURT:1d3\n\
B:CLAW:HURT:1d4\n\
B:CLAW:BLIND:1d1\n\
F:NO_FEAR | DROP_60 | FEMALE | DROP_90 | ESCORT | UNIQUE\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GOOD | FORCE_MAXHP\n\
D:A female Maenad, she was the queen of Thebes in Greek mythology, mother of Pentheus and daughter of Harmonia and Cadmus. \n\
\n\
N:127:Red swollen ectoplasma\n\
G:e:r\n\
I:110:1:5d8:2:6:10\n\
W:7:2:0:15\n\
B:GAZE:HURT:2d5\n\
B:GAZE:LOSE_CHA:2d1\n\
F:RAND_50 | RAND_25 | \n\
F:STUPID | EMPTY_MIND | \n\
D:This ectoplasma is swollen, full of blood. \n\
\n\
N:128:Ciacco\n\
G:p:y\n\
I:110:4:19d10:20:32:30\n\
W:7:2:0:80\n\
B:HIT:HURT:1d10\n\
F:UNIQUE | MALE | FORCE_MAXHP | ESCORT | \n\
F:ONLY_ITEM | DROP_1D2 | DROP_GOOD | \n\
D:A good of friend of Dante, he is obsessed with his prophecy \n\
D:that Florence will fall in the hands of the Neri. \n\
\n\
N:129:Charles le Gros\n\
G:p:y\n\
I:110:4:19d10:20:32:30\n\
W:7:2:0:80\n\
B:HIT:HURT:1d10\n\
F:UNIQUE | MALE | FORCE_MAXHP | ESCORT | \n\
F:ONLY_ITEM | DROP_1D2 | DROP_GREAT | \n\
D:Charles the Fat (in French: Charles le Gros) (c. 832–January 13, 888) was a king of East Franks, \n\
D:king of Italy, a King of France and, as Charles III, Holy Roman Emperor. He was the son of Louis the German. \n\
\n\
N:130:William VI of Aquitaine\n\
G:p:y\n\
I:110:4:19d10:20:32:30\n\
W:7:2:0:80\n\
B:HIT:HURT:1d10\n\
F:UNIQUE | MALE | FORCE_MAXHP | ESCORT | \n\
F:ONLY_ITEM | DROP_1D2 | DROP_GREAT | \n\
D:William VI of Aquitaine (1004-1038), nicknamed the Fat, was Duke of Aquitaine and Count of Poitiers as \n\
D:William IV of Poitou between 1030 and 1038. William was the eldest son of William V of Aquitaine by his \n\
D:first wife Almodis of Gevaudun. \n\
\n\
N:131:Peter II of Cyprus\n\
G:p:y\n\
I:110:4:19d10:20:32:30\n\
W:7:2:0:80\n\
B:HIT:HURT:1d10\n\
F:UNIQUE | MALE | FORCE_MAXHP | ESCORT | \n\
F:ONLY_ITEM | DROP_1D2 | DROP_GREAT | \n\
D:Peter II of Cyprus (c. 1357 – October 13, 1382), called The Fat, was king of Cyprus from January 17, \n\
D:1369 until his death. He was the son of Peter I of Cyprus and his second wife Eleanor of Aragon. \n\
\n\
N:132:Henry I of Cyprus\n\
G:p:y\n\
I:110:4:19d10:20:32:30\n\
W:7:2:0:80\n\
B:HIT:HURT:1d10\n\
F:UNIQUE | MALE | FORCE_MAXHP | ESCORT | \n\
F:ONLY_ITEM | DROP_1D2 | DROP_GREAT | \n\
D:Henry I of Cyprus, nicknamed the Fat, aka Henry of Lusignan, Henri de Lusignan (1217–1253) was King \n\
D:of Cyprus January 10, 1218–1253. He was the son of Hugh I of Cyprus and Alice of Champagne of Jerusalem. \n\
\n\
N:133:Cerberus\n\
G:C:R\n\
I:110:4:24d10:20:60:20\n\
W:7:3:0:230\n\
B:BITE:HURT:1d12\n\
F:UNIQUE | MALE | \n\
F:FORCE_MAXHP | BASH_DOOR |\n\
F:EVIL | IM_FIRE | IM_COLD | RES_ELEC | RES_POIS | DEMON\n\
D:A three-headed dog, he is in charge of Inferno's Third Circle. \n\
D:Unlike the descriptions from Greek mythology, he sports \n\
D:humanoid features such as hands with nails, a greasy beard, a large belly, and red eyes. \n\
D:If you dont know how to handle him, you really better just run away from him. \n\
\n\
######### Inferno   ####################\n\
##### Level 8 , The $$$ Level        ###\n\
##### Fourth Circle                  ###\n\
\n\
N:134:Pluto\n\
G:U:y\n\
I:120:4:24d10:20:60:20\n\
W:8:3:0:300\n\
B:CLAW:HURT:2d6\n\
F:UNIQUE | MALE | \n\
F:FORCE_MAXHP | BASH_DOOR | DROP_60 | DROP_90 | ONLY_GOLD | DROP_4D2 | DROP_3D2\n\
F:EVIL | IM_FIRE | IM_COLD | RES_ELEC | RES_POIS | DEMON\n\
D:Pluto is meant to symbolize riches, as he is the god of wealth that \n\
D:springs from soil in ancient mythology. This is appropriate because he \n\
D:guards those who hoarded money and those who spent it foolishly. \n\
\n\
N:135:Biting gold coins\n\
G:$:y\n\
I:100:1:18d8:5:36:10\n\
W:8:2:0:32\n\
B:HIT:HURT:2d5\n\
B:TOUCH:POISON:2d5\n\
B:TOUCH:ACID:1d5\n\
F:ONLY_GOLD | DROP_90 | DROP_1D2 | \n\
F:COLD_BLOOD | BASH_DOOR | \n\
F:ANIMAL | FRIENDS | \n\
F:IM_POIS | NO_CONF | NO_SLEEP\n\
D:It is a pile of coins, crawling forward on thousands of tiny legs. \n\
D:It seems to focus on the hoarders. \n\
\n\
N:136:Hoarder\n\
G:p:y\n\
I:110:2:6d8:20:6:5\n\
W:8:1:0:18\n\
B:HIT:HURT:1d5\n\
F:MALE | \n\
F:DROP_60 | DROP_90 | DROP_1D2 | \n\
F:OPEN_DOOR | BASH_DOOR\n\
S:1_IN_9 | \n\
S:ARROW_2\n\
D:They have no interest in rolling boulders, preferring \n\
D:to collect riches for eternity. \n\
\n\
N:137:Banker\n\
G:p:y\n\
I:110:2:6d8:20:6:5\n\
W:8:1:0:18\n\
B:HIT:HURT:1d5\n\
F:MALE | \n\
F:DROP_60 | DROP_90 | DROP_1D2 | \n\
F:OPEN_DOOR | BASH_DOOR\n\
S:1_IN_9 | \n\
S:ARROW_2\n\
D:Hunted down, this banker does not trust anybody, \n\
D:he looks wearily at you while he cocks another arrow. \n\
\n\
N:138:Usurer\n\
G:p:y\n\
I:110:2:6d8:20:6:5\n\
W:8:1:0:18\n\
B:HIT:HURT:1d5\n\
F:MALE | \n\
F:FRIENDS | DROP_60 | DROP_90 | DROP_1D2 | \n\
F:OPEN_DOOR | BASH_DOOR\n\
S:1_IN_9 | \n\
S:ARROW_2\n\
D:Hunted down by the squanderers, the ususers \n\
D:have decided to band together and settle the strife \n\
D:once and for all. \n\
\n\
#D:Ezekiel 22:12 There are hired murderers, loan racketeers, and extortioners everywhere! They never even think of me and my commands, says the Sovereign \n\
LORD.\n\
N:139:Extortioner\n\
G:p:y\n\
I:110:2:6d8:20:6:5\n\
W:8:1:0:18\n\
B:HIT:HURT:1d5\n\
F:MALE | \n\
F:FRIENDS | DROP_60 | DROP_90 | DROP_1D2 | \n\
F:OPEN_DOOR | BASH_DOOR\n\
S:1_IN_9 | \n\
S:ARROW_2\n\
D:Hunted down by the squanderers, the extortioners \n\
D:have decided to band together and settle the strife \n\
D:once and for all. \n\
\n\
N:140:Squanderer\n\
G:p:y\n\
I:110:2:6d8:20:6:5\n\
W:8:1:0:18\n\
B:HIT:HURT:1d5\n\
F:MALE | \n\
F:FRIENDS | DROP_60 | DROP_90 | DROP_1D2 | \n\
F:OPEN_DOOR | BASH_DOOR\n\
S:1_IN_9 | \n\
S:ARROW_2\n\
D:He has lost interest in rolling boulders, he has \n\
D:decided to hunt down the hoarders once and for all. \n\
D:He seems to ignore his wounds that should be mortal. \n\
\n\
N:141:Prodigal\n\
G:p:y\n\
I:110:2:6d8:20:6:5\n\
W:8:1:0:18\n\
B:HIT:HURT:1d5\n\
F:MALE | \n\
F:FRIENDS | DROP_60 | DROP_90 | DROP_1D2 | \n\
F:OPEN_DOOR | BASH_DOOR\n\
S:1_IN_9 | \n\
S:ARROW_2\n\
D:He has lost interest in rolling boulders, he has \n\
D:decided to hunt down the hoarders once and for all. \n\
D:He seems to ignore his wounds that should be mortal. \n\
\n\
N:142:Profligate\n\
G:p:y\n\
I:110:2:6d8:20:6:5\n\
W:8:1:0:18\n\
B:HIT:HURT:1d5\n\
F:MALE | \n\
F:FRIENDS | DROP_60 | DROP_90 | DROP_1D2 | \n\
F:OPEN_DOOR | BASH_DOOR\n\
S:1_IN_9 | \n\
S:ARROW_2\n\
D:He has lost interest in rolling boulders, he has \n\
D:decided to hunt down the hoarders once and for all. \n\
D:He seems to ignore his wounds that should be mortal. \n\
\n\
N:143:Wastrel\n\
G:p:y\n\
I:110:2:6d8:20:6:5\n\
W:8:1:0:18\n\
B:HIT:HURT:1d5\n\
F:MALE | \n\
F:FRIENDS | DROP_60 | DROP_90 | DROP_1D2 | \n\
F:OPEN_DOOR | BASH_DOOR\n\
S:1_IN_9 | \n\
S:ARROW_2\n\
D:He has lost interest in rolling boulders, he has \n\
D:decided to hunt down the hoarders once and for all. \n\
D:He seems to ignore his wounds that should be mortal. \n\
\n\
N:144:Lemure\n\
G:u:o\n\
I:110:1:13d9:20:32:30\n\
W:8:1:0:16\n\
B:HIT:HURT:1d8\n\
F:FRIENDS | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | DEMON | RES_FIRE | NO_FEAR | RES_COLD\n\
D:It is the larval form of a major demon. \n\
\n\
#Azza---Mentioned in Rabbinic lore, Solomonic lore. In the Talmud, Azza and Azael are said to have fathered the Sedim upon Naamah, before the Flood.\n\
N:145:Shedim of the Coin\n\
G:u:s\n\
I:110:1:13d9:20:32:30\n\
W:8:1:0:25\n\
B:HIT:HURT:1d10\n\
F:MALE | \n\
F:FRIENDS | DROP_60 | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | DEMON | HURT_LITE\n\
D:Shedim are the incarnations of the pagan idols. \n\
D:They have lured many away from the heavens with \n\
D:false promesses of beauty, power and wealth. \n\
\n\
N:146:Olivier\n\
G:U:D\n\
I:110:4:25d10:20:30:20\n\
W:8:2:0:150\n\
B:HIT:HURT:1d5\n\
B:TOUCH:EAT_GOLD\n\
F:UNIQUE | MALE |\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_1D2 | DROP_GOOD | DROP_GREAT | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | RES_LITE | HEAL_DARK | RES_POIS\n\
F:RES_TELE\n\
S:1_IN_5 | \n\
S:HEAL | SLOW | TRAPS | BO_COLD | BA_POIS\n\
D:This demon of the Third Hierarchy incarnates fierceness, greediness and envy. \n\
D:These days he dwells in the Fourth circle. \n\
\n\
N:147:Corrupted pope\n\
G:p:w\n\
I:110:2:6d10:20:16:5\n\
W:8:4:0:20\n\
B:HIT:HURT:1d7\n\
B:HIT:HURT:1d7\n\
B:HIT:HURT:1d7\n\
B:HIT:HURT:1d7\n\
F:MALE | SMART | \n\
F:DROP_60 | DROP_90 | DROP_4D2 | DROP_GOOD | FORCE_MAXHP | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
S:1_IN_9 | \n\
S:SCARE | CAUSE_1\n\
D:He thinks you will steal his money, the punishment he underwent rendered him unrecognizable. \n\
\n\
N:148:Corrupted cardinal\n\
G:p:w\n\
I:110:2:6d8:20:16:5\n\
W:8:2:0:20\n\
B:HIT:HURT:1d7\n\
B:HIT:HURT:1d7\n\
F:MALE | SMART | \n\
F:DROP_60 | DROP_90 | DROP_1D2 | DROP_GOOD |  \n\
F:OPEN_DOOR | BASH_DOOR | \n\
S:1_IN_9 | \n\
S:SCARE | CAUSE_1\n\
D:He thinks you will steal his money, the punishment he underwent rendered him unrecognizable. \n\
\n\
\n\
######### Inferno   ####################\n\
##### Level 9 , The the wrathfull    ###\n\
##### Fifth  Circle                  ###\n\
\n\
N:149:Sulfurous ectoplasma\n\
G:e:y\n\
I:110:2:7d8:14:18:20\n\
W:9:3:0:24\n\
B:CRAWL:ACID:1d2\n\
B:CRAWL:ACID:1d2\n\
B:CRAWL:ACID:1d2\n\
B:CRAWL:ACID:1d2\n\
F:RAND_50 | \n\
F:EMPTY_MIND | COLD_BLOOD | TAKE_ITEM | IM_ACID\n\
F:KILL_BODY | POWERFUL \n\
D:This ectoplasma is very corrosive and smelly. \n\
\n\
N:150:Berserker\n\
G:p:U\n\
I:110:3:11d9:20:24:10\n\
W:9:3:0:60\n\
B:HIT:HURT:1d3\n\
B:HIT:HURT:1d3\n\
B:HIT:HURT:1d3\n\
B:HIT:HURT:1d4\n\
F:REFLECTING | POWERFUL | EVIL\n\
F:BASH_DOOR | OPEN_DOOR\n\
F:ATTR_MULTI | ATTR_ANY | IM_POIS\n\
D:The ultimate giving in to the wrath that inhabits every soul, \n\
D:they inflict punishment on any one they see. \n\
\n\
N:151:Swamp Fly\n\
G:F:g\n\
I:110:3:2d10:2:30:70\n\
W:9:2:0:20\n\
B:BITE:POISON:1d5\n\
B:BITE:LOSE_CON:1d4\n\
F:FORCE_MAXHP | FORCE_SLEEP | FRIENDS | ANIMAL | RES_POIS\n\
D:You wonder if these flies try to poison \n\
D:you with their bites or just rip of the skin. \n\
\n\
N:152:Stinger bee\n\
G:I:y\n\
I:120:2:2d4:12:34:10\n\
W:9:2:0:22\n\
B:STING:POISON:1d4\n\
B:STING:LOSE_STR:1d4\n\
F:WEIRD_MIND | FRIENDS |ANIMAL | RES_POIS\n\
D:Attracted by the swamps' inhabitants, they find plenty of victims \n\
D:for their poisonous bites. \n\
\n\
N:153:Warlock\n\
G:p:b\n\
I:110:2:9d8:20:15:20\n\
W:9:2:0:30\n\
B:HIT:HURT:1d6\n\
F:MALE | \n\
F:FORCE_SLEEP | DROP_90 | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | HURT_LITE |RES_DARK | RES_COLD | RES_FIRE\n\
S:1_IN_8 | \n\
S:BLINK | CAUSE_1 | MISSILE\n\
D:Minos has rewarded his bloodlust with an \n\
D:eternal bloodbath. \n\
\n\
N:154:Filippo Argenti\n\
G:p:o\n\
I:110:4:21d10:100:25:0\n\
W:9:2:0:100\n\
B:HIT:HURT:1d12\n\
F:UNIQUE | MALE | EVIL\n\
F:FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR\n\
D:This hot-headed character, a member of the Florentine \n\
D:black faction is known for his violent temper. \n\
D:He throttled a man who had crossed him once. \n\
\n\
N:155:King Edward I of England\n\
G:p:o\n\
I:110:4:21d10:100:25:0\n\
W:9:2:0:100\n\
B:HIT:HURT:1d12\n\
F:UNIQUE | MALE | EVIL\n\
F:FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR\n\
D:Edward I (June 17, 1239–July 7, 1307), popularly known as 'Longshanks' because of his 6 foot 2 inch frame and the 'Hammer of the Scots'. \n\
\n\
N:156:Sir William Wallace\n\
G:p:o\n\
I:110:4:21d10:100:25:0\n\
W:9:2:0:100\n\
B:HIT:HURT:1d12\n\
F:UNIQUE | MALE | EVIL\n\
F:FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR\n\
D:Sir William Wallace (circa. 1270 – August 22, 1305), sometimes called The Wallace, was a Scottish knight who led his countrymen in \n\
D:resistance to English domination in the reign of King Edward I, during significant periods of the Wars of Scottish Independence. \n\
\n\
N:157:Philip IV of France\n\
G:p:o\n\
I:110:4:21d10:100:25:0\n\
W:9:2:0:100\n\
B:HIT:HURT:1d12\n\
F:UNIQUE | MALE | EVIL\n\
F:FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR\n\
D:Philip IV the Fair (French: Philippe IV le Bel) (1268 – November 29, 1314) was King of France from 1285 until his death. \n\
D:Philip arrested Jews so he could seize their assets to accommodate the inflated costs of modern warfare, \n\
D:personally defeated the Flemings at Mons in 1304, forcing a harsh peace treaty with humiliating penalties. \n\
D:He had all the Knights Templar in France simultaneously arrested by his agents. To be later tortured into admitting heresy in the Order. \n\
\n\
N:158:Edward, the Black Prince\n\
G:p:o\n\
I:110:4:21d10:100:25:0\n\
W:9:2:0:100\n\
B:HIT:HURT:1d12\n\
F:UNIQUE | MALE | EVIL\n\
F:FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR\n\
D:Edward of Woodstock, Prince of Wales, KG, known as the Black Prince (June 15, 1330 – June 8, 1376) \n\
D:was the eldest son of King Edward III of England and Philippa of Hainault. \n\
D:It is likely that his title was first coined by French chroniclers in reference to the ruinous \n\
D:military defeats he had inflicted on France or his cruelty in these. \n\
\n\
N:159:Bertran du Guesclin\n\
G:p:o\n\
I:110:4:21d10:100:25:0\n\
W:9:2:0:100\n\
B:HIT:HURT:1d12\n\
F:UNIQUE | MALE | EVIL\n\
F:FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR\n\
D:A French military commander that modernized the French \n\
D:style of warfare, introducing guerilla and 'small war' tactics of fighting. \n\
\n\
N:160:Ivo Taillefer\n\
G:p:o\n\
I:110:4:21d10:100:25:0\n\
W:9:2:0:100\n\
B:HIT:HURT:1d12\n\
F:UNIQUE | MALE | EVIL\n\
F:FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR\n\
D:William of Normandy 's minstrel and knight, Ivo Taillefer, begged his master for permission to strike the first blows of the battle. \n\
D:Permission was granted, and Taillefer rode before the English alone, tossing his sword and lance in the air and catching them while he sang an early \n\
version of The Song of Roland. \n\
D:An English champion came from the ranks, and Taillefer quickly slew him, taking his head as a trophy to show that God favored the invaders. \n\
\n\
N:161:Memnon of Rhodes\n\
G:p:o\n\
I:110:4:21d10:100:25:0\n\
W:9:2:0:100\n\
B:HIT:HURT:1d12\n\
F:UNIQUE | MALE | EVIL\n\
F:FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR\n\
D:Memnon of Rhodes (380 – 333 BC) was the commander of the Greek mercenaries working for the Persian \n\
D:king Darius III when Alexander the Great of Macedonia invaded Persia in 334 BC. \n\
\n\
N:162:Harald Hardrada\n\
G:p:o\n\
I:110:4:21d10:100:25:0\n\
W:9:2:0:100\n\
B:HIT:HURT:1d12\n\
F:UNIQUE | MALE | EVIL\n\
F:FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR\n\
D:Harald Hardrada ('Hardreign') who arrived in Constantinople in 1035, was employed as a Varangian Guard. \n\
D:He participated in eighteen battles and became Akolythos, the commander, of the Guard before returning \n\
D:home in 1043. He was killed at the Battle of Stamford Bridge in 1066 when his army was defeated by an \n\
D:English army commanded by King Harold Godwinson. \n\
\n\
N:163:El Cid\n\
G:p:o\n\
I:110:4:21d10:100:25:0\n\
W:9:2:0:100\n\
B:HIT:HURT:1d12\n\
F:UNIQUE | MALE | EVIL\n\
F:FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR\n\
D:Rodrigo Diaz de Vivar (c. 1044 – July 1099), nicknamed El Cid Campeador, was a Castilian military and political leader in medieval Spain. \n\
D:He was a man who had fought and beaten the select fighting-man of the opposite side in the presence of the two armies. Exiled by his king \n\
D:he became a man for hire. \n\
\n\
N:164:Owain of the Red Hand\n\
G:p:o\n\
I:110:4:21d10:100:25:0\n\
W:9:2:0:100\n\
B:HIT:HURT:1d12\n\
F:UNIQUE | MALE | EVIL\n\
F:FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR\n\
D:Owain Lawgoch was a claimant to the title of Prince of Gwynedd and of Wales. \n\
D:Owain was a soldier who served in Spain, France, Alsace and Switzerland. \n\
D:He led a Free Company fighting for the French against the English in the Hundred Years' War. \n\
\n\
N:165:Myrmidon\n\
G:p:W\n\
I:110:3:10d10:100:35:0\n\
W:9:1:0:25\n\
B:HIT:HURT:1d4\n\
B:HIT:HURT:1d6\n\
F:OPEN_DOOR | BASH_DOOR | FRIENDS | DROP_60 | DROP_90 | MALE\n\
D:The Myrmidons of Greek myth were known for their blind and remorseless loyalty to their leaders. \n\
\n\
N:166:Hoplite of 'The Ten Thousand'\n\
G:p:W\n\
I:110:3:10d10:10:35:10\n\
W:9:1:0:25\n\
B:HIT:HURT:1d4\n\
B:HIT:HURT:1d6\n\
F:OPEN_DOOR | BASH_DOOR | FRIENDS | DROP_60 | DROP_90 | MALE\n\
D:The Ten Thousand were a group of mercenary units, mainly Greek, drawn up by Cyrus the Younger to attempt to wrest the throne of the Persian Empire from his \n\
brother, Artaxerxes II. \n\
\n\
N:167:Bessi Warrior\n\
G:p:W\n\
I:110:3:10d10:10:35:10\n\
W:9:1:0:25\n\
B:HIT:HURT:1d4\n\
B:HIT:HURT:1d6\n\
F:OPEN_DOOR | BASH_DOOR | FRIENDS | DROP_60 | DROP_90 | MALE\n\
D:The Bessi warriors are described as the fiercest of the independent Thracian tribes. \n\
\n\
N:168:Dii swordman\n\
G:p:W\n\
I:110:3:10d10:10:35:10\n\
W:9:1:0:25\n\
B:HIT:HURT:1d4\n\
B:HIT:HURT:1d6\n\
F:OPEN_DOOR | BASH_DOOR | FRIENDS | DROP_60 | DROP_90 | MALE\n\
D:The Dii warriors are described as the most warlike infantry troops of the Thracians. \n\
\n\
N:169:Son of Mars\n\
G:p:r\n\
I:120:3:10d10:10:35:10\n\
W:9:3:0:75\n\
B:HIT:HURT:1d8\n\
B:HIT:HURT:1d8\n\
F:OPEN_DOOR | BASH_DOOR | FRIENDS | DROP_60 | DROP_90 | MALE | FORCE_MAXHP\n\
D:The Mamertines (Mamertini 'sons of Mars') were mercenaries of Italian origin who had \n\
D:been hired from their home in Campania by Agathocles, the king of Syracuse. \n\
\n\
N:170:Varangian\n\
G:p:W\n\
I:110:3:10d10:10:35:10\n\
W:9:1:0:25\n\
B:HIT:HURT:1d4\n\
B:HIT:HURT:1d6\n\
F:OPEN_DOOR | BASH_DOOR | FRIENDS | DROP_60 | DROP_90 | MALE\n\
D:Promoting trade, piracy and mercenary militarism, they roamed the river systems and \n\
D:portages of what later became Russia, reaching the Caspian Sea and Constantinople. \n\
\n\
N:171:Condottiero\n\
G:p:W\n\
I:110:3:10d10:10:35:10\n\
W:9:1:0:25\n\
B:HIT:HURT:1d4\n\
B:HIT:HURT:1d6\n\
F:OPEN_DOOR | BASH_DOOR | FRIENDS | DROP_60 | DROP_90 | MALE\n\
D:Condottieri (singular condottiero) were mercenary leaders employed by Italian \n\
D:city-states from the late Middle Ages until the mid-sixteenth century. \n\
\n\
N:172:Landsknecht\n\
G:p:W\n\
I:110:3:10d10:10:35:10\n\
W:9:1:0:25\n\
B:HIT:HURT:1d4\n\
B:HIT:HURT:1d6\n\
F:OPEN_DOOR | BASH_DOOR | FRIENDS | DROP_60 | DROP_90 | MALE\n\
D:Landsknechts (singular Landsknecht, German plural Landsknechte sometimes also \n\
D:in English publications) were European, most often German, mercenary pikemen \n\
D:and foot soldiers from the late 15th to the early 17th century, with the \n\
D:formidable reputation for being the most effective fighting troops during the European Renaissance. \n\
\n\
N:173:Swiss mercenary\n\
G:p:W\n\
I:110:3:10d10:10:35:10\n\
W:9:1:0:25\n\
B:HIT:HURT:1d4\n\
B:HIT:HURT:1d6\n\
F:OPEN_DOOR | BASH_DOOR | FRIENDS | DROP_60 | DROP_90 | MALE\n\
D:Swiss mercenaries were sought after during the latter half of the 15th century \n\
D:as being an effective fighting force, until their somewhat rigid battle formations \n\
D:became vulnerable to arquebuses and artillery being developed at about that period. \n\
\n\
######### Inferno   ####################\n\
##### Level 10 , The heretics        ###\n\
##### Sixth  Circle                  ###\n\
\n\
N:174:Flaming Tomb\n\
G:#:r\n\
I:100:4:6d6:20:40:0\n\
W:10:1:0:30\n\
B:HIT:HURT:5d5\n\
B:TOUCH:FIRE:4d4\n\
B:TOUCH:EXP_10:10d10\n\
F:NEVER_MOVE | NONLIVING | NO_FEAR | IM_LITE\n\
F:STUPID | EMPTY_MIND | NO_CONF | NO_SLEEP\n\
F:IM_FIRE | FORCE_MAXHP | IM_POIS | ANNOYED | NEVER_BLOW\n\
D:These tombs contain heretical souls for eternity. \n\
D:Better not mess with these. \n\
\n\
N:175:The Flaming Tomb of Farinata degli Uberti\n\
G:#:R\n\
I:100:4:6d6:20:40:0\n\
W:10:1:0:30\n\
B:HIT:HURT:5d5\n\
B:TOUCH:FIRE:4d4\n\
B:TOUCH:EXP_10:10d10\n\
F:NEVER_MOVE | NONLIVING | NO_FEAR | UNIQUE | MALE | IM_LITE\n\
F:STUPID | EMPTY_MIND | NO_CONF | NO_SLEEP\n\
F:IM_FIRE | FORCE_MAXHP | IM_POIS | ANNOYED | NEVER_BLOW\n\
D:'Behold Farinata who hath uprisen; thou shalt see him all from the girdle up. \n\
D:I had already fixed my face on his, and he straightened himself up with breast and front as though he had Hell in great scorn.' \n\
\n\
N:176:The Flaming Tomb of Cavalcante de' Cavalcanti\n\
G:#:R\n\
I:100:4:6d6:20:40:0\n\
W:10:1:0:30\n\
B:HIT:HURT:5d5\n\
B:TOUCH:FIRE:4d4\n\
B:TOUCH:EXP_10:10d10\n\
F:NEVER_MOVE | NONLIVING | NO_FEAR | UNIQUE | MALE | IM_LITE\n\
F:STUPID | EMPTY_MIND | NO_CONF | NO_SLEEP\n\
F:IM_FIRE | FORCE_MAXHP | IM_POIS | ANNOYED | NEVER_BLOW\n\
D:'To view uncovered down to the chin, a shade ... risen on its knees.' \n\
\n\
N:177:The Flaming Tomb of Epicurus\n\
G:#:R\n\
I:100:4:6d6:20:40:0\n\
W:10:1:0:30\n\
B:HIT:HURT:5d5\n\
B:TOUCH:FIRE:4d4\n\
B:TOUCH:EXP_10:10d10\n\
F:NEVER_MOVE | NONLIVING | NO_FEAR | UNIQUE | MALE | IM_LITE\n\
F:STUPID | EMPTY_MIND | NO_CONF | NO_SLEEP\n\
F:IM_FIRE | FORCE_MAXHP | IM_POIS | ANNOYED | NEVER_BLOW\n\
D:Suffering the burning heat of his coffin, \n\
D:Epicurus has come to realize that the soul is not mortal after all. \n\
\n\
N:178:The Flaming Tomb of Frederick II \n\
G:#:R\n\
I:100:4:6d6:20:40:0\n\
W:10:1:0:30\n\
B:HIT:HURT:5d5\n\
B:TOUCH:FIRE:4d4\n\
B:TOUCH:EXP_10:10d10\n\
F:NEVER_MOVE | NONLIVING | NO_FEAR | UNIQUE | MALE | IM_LITE\n\
F:STUPID | EMPTY_MIND | NO_CONF | NO_SLEEP\n\
F:IM_FIRE | FORCE_MAXHP | IM_POIS | ANNOYED | NEVER_BLOW\n\
D:The last in the line of reigning Holy Roman Emperors. Raised in Palermo, \n\
D:in the Kingdom of Sicily, Frederick was crowned emperor in Rome in 1220. \n\
D:A central figure in the conflicting claims of the empire and the papacy, \n\
D:he was twice excommunicated--in 1227 and 1245-- before his death in 1250. \n\
\n\
N:179:The Flaming Tomb of the Cardinal of the Ubaldini\n\
G:#:R\n\
I:100:4:6d6:20:40:0\n\
W:10:1:0:30\n\
B:HIT:HURT:5d5\n\
B:TOUCH:FIRE:4d4\n\
B:TOUCH:EXP_10:10d10\n\
F:NEVER_MOVE | NONLIVING | NO_FEAR | UNIQUE | MALE | IM_LITE\n\
F:STUPID | EMPTY_MIND | NO_CONF | NO_SLEEP\n\
F:IM_FIRE | FORCE_MAXHP | IM_POIS | ANNOYED | NEVER_BLOW\n\
\n\
N:180:The Flaming Tomb of Pope Anastasius II\n\
G:#:R\n\
I:100:4:6d6:20:40:0\n\
W:10:1:0:30\n\
B:HIT:HURT:5d5\n\
B:TOUCH:FIRE:4d4\n\
B:TOUCH:EXP_10:10d10\n\
F:NEVER_MOVE | NONLIVING | NO_FEAR | UNIQUE | MALE | IM_LITE\n\
F:STUPID | EMPTY_MIND | NO_CONF | NO_SLEEP\n\
F:IM_FIRE | FORCE_MAXHP | IM_POIS | ANNOYED | NEVER_BLOW\n\
D:He is said to have allowed the fifth-century heretic Photinus to communion. \n\
\n\
N:181:Bickering Hell Spawn\n\
G:u:g\n\
I:120:2:1d1:4:1:0\n\
W:10:3:0:3\n\
B:HIT:CONFUSE:1d1\n\
B:HIT:POISON:1d1\n\
F:NEVER_MOVE | COLD_BLOOD | MULTIPLY | HURT_LITE\n\
F:STUPID | EMPTY_MIND | DEMON\n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_5\n\
S:ARROW_1\n\
D:These malformed demons are constantly bickering, only \n\
D:interrupting their dispute to shoot arrows into the burning residents \n\
D:of Inferno. Their confused shouting seems to be a summoning \n\
D:spell in itself, generating more and more demons \n\
\n\
N:182:Bickering Hell Spawn\n\
G:u:w\n\
I:120:2:1d1:4:1:0\n\
W:10:3:0:3\n\
B:HIT:CONFUSE:1d1\n\
B:HIT:COLD:1d1\n\
F:NEVER_MOVE | COLD_BLOOD | MULTIPLY | HURT_LITE\n\
F:STUPID | EMPTY_MIND | DEMON\n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_5\n\
S:ARROW_1\n\
D:These malformed demons are constantly bickering, only \n\
D:interrupting their dispute to shoot arrows into the burning residents \n\
D:of Inferno. Their confused shouting seems to be a summoning \n\
D:spell in itself, generating more and more demons \n\
\n\
#The use of it's name is hardcoded in monster2.c in summon_specific_okay\n\
#under SUMMON_SKULL\n\
N:183:Screaming skull of Vengeance \n\
G:s:b\n\
I:145:10:99d111:100:165:0\n\
W:10:1:0:65000\n\
B:HIT:HURT:12d12\n\
B:HIT:HURT:12d12\n\
B:HIT:HURT:10d2\n\
B:HIT:HURT:3d2\n\
F:FORCE_MAXHP | REGENERATE | EVIL | IM_COLD | IM_POIS | PASS_WALL\n\
F:NO_CONF | NO_SLEEP | NO_FEAR | NO_SPAWN | FLIGHT\n\
S:1_IN_2 | \n\
S:TELE_TO | FORGET | \n\
D:Vengeance has sent it's emmisary, \n\
D:and it is screaming for you blood. \n\
\n\
#The use of it's name is hardcoded in monster2.c in summon_specific_okay\n\
#under SUMMON_SKULL\n\
N:184:Screaming skull of Fury \n\
G:s:r\n\
I:145:10:99d111:100:165:0\n\
W:10:1:0:65000\n\
B:HIT:HURT:12d12\n\
B:HIT:HURT:12d12\n\
B:HIT:HURT:10d2\n\
B:HIT:HURT:3d2\n\
F:FORCE_MAXHP | REGENERATE | EVIL | IM_COLD | IM_POIS | PASS_WALL\n\
F:NO_CONF | NO_SLEEP | NO_FEAR | NO_SPAWN | FLIGHT\n\
S:1_IN_2 | \n\
S:TELE_TO | FORGET | \n\
D:Fury has sent it's emmisary, \n\
D:and it is screaming for you blood. \n\
\n\
N:185:Megaera\n\
G:h:u\n\
I:110:4:24d10:20:60:20\n\
W:10:3:0:230\n\
B:HIT:HURT:3d2\n\
B:HIT:HURT:3d2\n\
F:UNIQUE | FEMALE | FORCE_MAXHP | EVIL\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR |\n\
F:RES_FIRE | RES_COLD | RES_ELEC | IM_POIS | RES_DARK\n\
D:One of the three sister furies, \n\
D:Daughters of Night, bloodstained with snakes in their hair and about their waists. \n\
D:She is about to exact divine vengeance on your soul. \n\
\n\
N:186:Tisiphone\n\
G:h:u\n\
I:110:4:24d10:20:60:20\n\
W:10:3:0:230\n\
B:HIT:HURT:3d2\n\
B:HIT:HURT:3d2\n\
F:UNIQUE | FEMALE | FORCE_MAXHP | EVIL\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR |\n\
F:RES_FIRE | RES_COLD | RES_ELEC | IM_POIS | RES_DARK\n\
D:One of the three sister furies, \n\
D:Daughters of Night, bloodstained with snakes in their hair and about their waists. \n\
D:She is about to exact divine vengeance on your soul. \n\
\n\
N:187:Allecto\n\
G:h:u\n\
I:110:4:24d10:20:60:20\n\
W:10:3:0:230\n\
B:HIT:HURT:3d2\n\
B:HIT:HURT:3d2\n\
F:UNIQUE | FEMALE | FORCE_MAXHP | EVIL\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR |\n\
F:RES_FIRE | RES_COLD | RES_ELEC | IM_POIS | RES_DARK\n\
D:One of the three sister furies, \n\
D:Daughters of Night, bloodstained with snakes in their hair and about their waists. \n\
D:She is about to exact divine vengeance on your soul. \n\
\n\
N:188:Hecate\n\
G:U:b\n\
I:110:4:24d10:20:60:20\n\
W:10:4:0:230\n\
B:HIT:HURT:3d2\n\
B:HIT:HURT:3d2\n\
F:UNIQUE | FEMALE | FORCE_MAXHP | EVIL\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:IM_FIRE | IM_COLD | RES_ELEC | IM_POIS | HURT_DARK | HEAL_LITE\n\
S:1_IN_2 | \n\
S:BR_LITE\n\
D:A moon-goddess, her powerfull magic can light up the entire sixth circle. \n\
D:She said to Solomon : 'I am the worst, and I make thee worse off than thou wast; because I will impose the bonds of Artemis.' \n\
\n\
######### Inferno   ######################\n\
##### Level 11 , The violent to others ###\n\
##### Seventh  Circle , Outer Ring     ###\n\
\n\
N:189:Minotaur\n\
G:h:U\n\
I:110:1:20d9:12:14:10\n\
W:11:1:0:30\n\
B:BUTT:HURT:2d5\n\
B:HIT:HURT:2d5\n\
F:BASH_DOOR | EVIL | RES_POIS | RES_DARK\n\
D:It is a cross between a human and a bull. They are the symbol \n\
D:of man with animal behavior, since they are part men, part beasts. \n\
D:They guard those that would be violent against their neighbours. \n\
\n\
N:190:Centaur\n\
G:h:u\n\
I:110:2:20d9:12:14:10\n\
W:11:1:0:30\n\
B:BUTT:HURT:2d5\n\
B:HIT:HURT:2d5\n\
F:BASH_DOOR | EVIL | RES_POIS | RES_DARK\n\
S:1_IN_5\n\
S:ARROW_1\n\
D:It is a cross between a human and a horse. They are the symbol \n\
D:of man with animal behavior, since they are part men, part beasts. \n\
D:They guard those that would be violent against their neighbours. \n\
\n\
#Golfimbul was an inspiration to us all;)\n\
#Max damage remains, but minimum and average damage is way higher\n\
#But then again, no resists at all ;)\n\
N:191:Chiron\n\
G:h:r\n\
I:110:4:24d10:20:60:20\n\
W:11:1:0:230\n\
B:HIT:HURT:2d3\n\
B:HIT:HURT:2d3\n\
F:UNIQUE | MALE | FORCE_MAXHP | ESCORT | ESCORTS\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR |\n\
F:RES_POIS | RES_DARK | \n\
S:1_IN_5\n\
S:ARROW_1\n\
D:He is the leader of the Centaurs. \n\
\n\
N:192:Nessus \n\
G:h:r\n\
I:110:4:24d10:20:60:20\n\
W:11:1:0:230\n\
B:HIT:HURT:2d3\n\
B:HIT:HURT:2d3\n\
F:UNIQUE | MALE | FORCE_MAXHP | ESCORT | ESCORTS\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR |\n\
F:RES_POIS | RES_DARK | \n\
S:1_IN_5\n\
S:ARROW_1\n\
D:'That is Nessus, Who died for the lovely Dejanira \n\
D:By taking his own revenge upon himself;' \n\
\n\
N:193:Pholus \n\
G:h:r\n\
I:110:4:24d10:20:60:20\n\
W:11:1:0:230\n\
B:HIT:HURT:2d3\n\
B:HIT:HURT:2d3\n\
F:UNIQUE | MALE | FORCE_MAXHP | ESCORT | ESCORTS\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR |\n\
F:RES_POIS | RES_DARK | \n\
S:1_IN_5\n\
S:ARROW_1\n\
D:'The last is Pholus, who was so full of frenzy.' \n\
\n\
N:194:Boiling Soul\n\
G:p:r\n\
I:120:1:7d7:30:30:20\n\
W:11:1:0:30\n\
B:HIT:HURT:1d4\n\
F:BASH_DOOR | OPEN_DOOR\n\
F:RES_FIRE\n\
D:'Thousands on thousands march around the ditch, \n\
D:Shooting at any soul that rises up \n\
D:Above the blood more than its guilt allows.' \n\
\n\
#TODO : add res_shards for these guys ?\n\
\n\
N:195:Tyrant\n\
G:p:R\n\
I:120:2:7d7:30:30:20\n\
W:11:1:0:30\n\
B:HIT:HURT:1d4\n\
F:BASH_DOOR | OPEN_DOOR | DROP_1D2 | DROP_GOOD | ONLY_ITEM | DROP_USEFUL | \n\
D:'There I saw people buried to their eyebrows, \n\
D:And the strong centaur said, 'These are tyrants \n\
D:Who wallowed in bloodshed and plundering.'' \n\
\n\
N:196:Alexander the Great of Pherae\n\
G:p:R\n\
I:110:4:24d10:20:60:20\n\
W:11:1:0:230\n\
B:HIT:HURT:2d3\n\
B:HIT:HURT:2d3\n\
F:BASH_DOOR | OPEN_DOOR | UNIQUE | MALE | DROP_1D2 | DROP_GREAT | ONLY_ITEM | DROP_GOOD | \n\
D:'Here they bewail their heartless crimes: here lie \n\
D:Both Alexander and fierce Dionysius \n\
D:Who brought long years of woe to Sicily;' \n\
\n\
N:197:Dionysius of Syracuse\n\
G:p:R\n\
I:110:4:24d10:20:60:20\n\
W:11:1:0:230\n\
B:HIT:HURT:2d3\n\
B:HIT:HURT:2d3\n\
F:BASH_DOOR | OPEN_DOOR | UNIQUE | MALE | DROP_1D2 | DROP_GREAT | ONLY_ITEM | DROP_GOOD | \n\
D:'Here they bewail their heartless crimes: here lie \n\
D:Both Alexander and fierce Dionysius \n\
D:Who brought long years of woe to Sicily;' \n\
D:Dionysius of Syracuse was a fifth-century tyrant. \n\
\n\
N:198:Azzolino da Romano\n\
G:p:R\n\
I:110:4:24d10:20:60:20\n\
W:11:1:0:230\n\
B:HIT:HURT:2d3\n\
B:HIT:HURT:2d3\n\
F:BASH_DOOR | OPEN_DOOR | UNIQUE | MALE | DROP_1D2 | DROP_GREAT | ONLY_ITEM | DROP_GOOD | \n\
D:'And there with his head of jet-black hair \n\
D:Is Azzolino; and that other blond one \n\
D:Is Opizzo d'Este, who in the world \n\
D:Actually was slain by his own stepson.' \n\
D:Azzolino da Romano (1194-1259) was a brutal Italian tyrant. \n\
\n\
N:199:Opizzo d'Este\n\
G:p:R\n\
I:110:4:24d10:20:60:20\n\
W:11:1:0:230\n\
B:HIT:HURT:2d3\n\
B:HIT:HURT:2d3\n\
F:BASH_DOOR | OPEN_DOOR | UNIQUE | MALE | DROP_1D2 | DROP_GREAT | ONLY_ITEM | DROP_GOOD | \n\
D:'And there with his head of jet-black hair \n\
D:Is Azzolino; and that other blond one \n\
D:Is Opizzo d'Este, who in the world \n\
D:Actually was slain by his own stepson.' \n\
D:Opizzo II d'Este (d. 1293) was a brutal Italian tyrant. \n\
\n\
N:200:Guy de Montfort\n\
G:p:R\n\
I:110:4:24d10:20:60:20\n\
W:11:1:0:230\n\
B:HIT:HURT:2d3\n\
B:HIT:HURT:2d3\n\
F:BASH_DOOR | OPEN_DOOR | UNIQUE | MALE | DROP_1D2 | DROP_GREAT | ONLY_ITEM | DROP_GOOD | \n\
D:'He pointed to one shade off by himself, \n\
D:And said, 'In God's own bosom, this one stabbed \n\
D:The heart that still drips blood upon the Thames.'' \n\
D:Guy de Montfort, to avenge his father's death, murdered Henry, nephew of Henry III of England, in 1271. \n\
\n\
N:201:Attila the Hun\n\
G:p:R\n\
I:140:4:24d10:20:60:20\n\
W:11:1:0:550\n\
B:HIT:HURT:6d8\n\
B:HIT:HURT:6d8\n\
B:HIT:HURT:6d8\n\
B:HIT:HURT:6d8\n\
F:BASH_DOOR | OPEN_DOOR | UNIQUE | MALE | DROP_1D2 | DROP_GREAT | ONLY_ITEM | DROP_GOOD | RES_FIRE\n\
D:'Heavenly justice there strikes with its goads \n\
D:That Attila who was a scourge on earth \n\
D:And Pyrrhus and Sextus, and forever milks \n\
D:The tears, released by boiling blood from both \n\
D:Rinier of Corneto and Rinier Pazzo \n\
D:Who waged such open warfare on the highways.' \n\
D:Attila the Hun, who ruled from 433 to 453, was called the Scourge of God. \n\
\n\
N:202:Pyrrhus, son of Achilles\n\
G:p:R\n\
I:110:4:24d10:20:60:20\n\
W:11:1:0:230\n\
B:HIT:HURT:2d3\n\
B:HIT:HURT:2d3\n\
F:BASH_DOOR | OPEN_DOOR | UNIQUE | MALE | DROP_1D2 | DROP_GREAT | ONLY_ITEM | DROP_GOOD | RES_FIRE\n\
D:'Heavenly justice there strikes with its goads \n\
D:That Attila who was a scourge on earth \n\
D:And Pyrrhus and Sextus, and forever milks \n\
D:The tears, released by boiling blood from both \n\
D:Rinier of Corneto and Rinier Pazzo \n\
D:Who waged such open warfare on the highways.' \n\
D:Pyrrhus, son of Achilles was a fighter in the Trojan war. \n\
\n\
N:203:Sextus, son of Pompey the Great\n\
G:p:R\n\
I:110:4:24d10:20:60:20\n\
W:11:1:0:230\n\
B:HIT:HURT:2d3\n\
B:HIT:HURT:2d3\n\
F:BASH_DOOR | OPEN_DOOR | UNIQUE | MALE | DROP_1D2 | DROP_GREAT | ONLY_ITEM | DROP_GOOD  | RES_FIRE \n\
D:'Heavenly justice there strikes with its goads \n\
D:That Attila who was a scourge on earth \n\
D:And Pyrrhus and Sextus, and forever milks \n\
D:The tears, released by boiling blood from both \n\
D:Rinier of Corneto and Rinier Pazzo \n\
D:Who waged such open warfare on the highways.' \n\
D:Sextus, son of Pompey the Great was a pirate of fame. \n\
\n\
N:204:Rinier of Corneto\n\
G:p:R\n\
I:110:4:24d10:20:60:20\n\
W:11:1:0:230\n\
B:HIT:HURT:2d3\n\
B:HIT:HURT:2d3\n\
F:BASH_DOOR | OPEN_DOOR | UNIQUE | MALE | DROP_1D2 | DROP_GREAT | ONLY_ITEM | DROP_GOOD  | RES_FIRE \n\
D:'Heavenly justice there strikes with its goads \n\
D:That Attila who was a scourge on earth \n\
D:And Pyrrhus and Sextus, and forever milks \n\
D:The tears, released by boiling blood from both \n\
D:Rinier of Corneto and Rinier Pazzo \n\
D:Who waged such open warfare on the highways.' \n\
D:Rinier of Cometo was a notorious highwayman. \n\
\n\
N:205:Rinier Pazzo\n\
G:p:R\n\
I:110:4:24d10:20:60:20\n\
W:11:1:0:230\n\
B:HIT:HURT:2d3\n\
B:HIT:HURT:2d3\n\
F:BASH_DOOR | OPEN_DOOR | UNIQUE | MALE | DROP_1D2 | DROP_GREAT | ONLY_ITEM | DROP_GOOD  | RES_FIRE \n\
D:'Heavenly justice there strikes with its goads \n\
D:That Attila who was a scourge on earth \n\
D:And Pyrrhus and Sextus, and forever milks \n\
D:The tears, released by boiling blood from both \n\
D:Rinier of Corneto and Rinier Pazzo \n\
D:Who waged such open warfare on the highways.' \n\
D:Rinier Pazzo was a notorious highwayman. \n\
\n\
######### Inferno   ####################\n\
##### Level 12 , The suicides        ###\n\
##### Seventh  Circle , Middle Ring  ###\n\
\n\
N:206:Thorny black tree\n\
G:#:v\n\
I:110:2:52d8:2:1:99\n\
W:12:1:0:60\n\
B:TOUCH:EXP_10\n\
B:TOUCH:EXP_10\n\
B:TOUCH:EXP_10\n\
B:TOUCH:EXP_10\n\
F:NEVER_MOVE | \n\
F:STUPID | EMPTY_MIND | FRIENDS | \n\
F:IM_POIS | NO_CONF | NO_SLEEP | NO_FEAR | HURT_FIRE | HEAL_LITE\n\
S:1_IN_11 | \n\
S:DRAIN_MANA\n\
D:Suicides — because they alienated themselves from their own bodies—spend \n\
D:eternity in the body of a tree, their own corpses hanging from the limbs. \n\
\n\
N:207:Pier delle Vigne\n\
G:#:o\n\
I:110:2:52d8:2:1:99\n\
W:12:1:0:60\n\
B:TOUCH:EXP_10\n\
F:NEVER_MOVE | \n\
F:STUPID | EMPTY_MIND | UNIQUE | \n\
F:IM_POIS | NO_CONF | NO_SLEEP | NO_FEAR | HURT_FIRE |  HEAL_LITE\n\
F:HURT_FIRE |  HEAL_LITE\n\
S:1_IN_11 | \n\
S:DRAIN_MANA\n\
D:Suicides — because they alienated themselves from their own bodies—spend \n\
D:eternity in the body of a tree, their own corpses hanging from the limbs. \n\
D:This particular one was a minister of Emperor Frederick II. He escaped \n\
D:the disdain of his peers with suicide. \n\
\n\
N:208:Ferocious Dog\n\
G:C:R\n\
I:120:5:9d8:20:30:30\n\
W:12:1:0:40\n\
B:CLAW:HURT:1d5\n\
B:BITE:HURT:1d8\n\
F:ANIMAL | FRIENDS | RES_FIRE | EVIL\n\
D:'In the crouching shade they gripped their teeth. \n\
D:And piece by piece they ripped him open-wide. \n\
D:And then they carried off his wretched limbs.' \n\
\n\
N:209:Lano da Siena\n\
G:p:y\n\
I:110:1:13d9:20:33:30\n\
W:12:2:0:50\n\
B:HIT:HURT:2d8\n\
F:UNIQUE\n\
F:DROP_60 | DROP_90 | DROP_1D2 | DROP_GREAT\n\
F:OPEN_DOOR | BASH_DOOR | \n\
D:Lano da Siena, a well-known spendthrift, died fighting the Aretines in 1287. \n\
D:He is eternally being chased by the ferocious dogs. \n\
\n\
N:210:Jacopo da Sant' Andrea of Padua\n\
G:p:y\n\
I:110:1:13d9:20:33:30\n\
W:12:2:0:50\n\
B:HIT:HURT:2d8\n\
F:UNIQUE\n\
F:DROP_60 | DROP_90 | DROP_1D2 | DROP_GREAT\n\
F:OPEN_DOOR | BASH_DOOR | \n\
D:Jacopo da Sant' Andrea of Padua, another squanderer, was murdered in 1239. \n\
\n\
N:211:Squanderer\n\
G:p:y\n\
I:110:1:13d9:20:33:30\n\
W:12:1:0:30\n\
B:HIT:HURT:2d8\n\
F:DROP_60 | DROP_90 | DROP_1D2 | DROP_GOOD\n\
F:OPEN_DOOR | BASH_DOOR | \n\
D:This squanderer ( or profligate ) is only a shade of his former self, \n\
D:he is continually hunted by the ferocious dogs. \n\
\n\
N:212:Hellbat\n\
G:b:r\n\
I:110:3:12d12:20:80:8\n\
W:12:3:0:65\n\
B:CLAW:HURT:1d4\n\
B:BITE:POISON:1d8\n\
F:ANIMAL | EVIL |  \n\
F:IM_COLD | IM_ELEC | IM_POIS | WEIRD_MIND\n\
D:Devil-bats, notoriously difficult to kill. \n\
\n\
N:213:Moaning spirit\n\
G:G:W\n\
I:120:2:5d8:14:20:10\n\
W:12:2:0:54\n\
B:WAIL:TERRIFY\n\
B:TOUCH:LOSE_DEX:1d8\n\
F:FORCE_SLEEP | RAND_25 | \n\
F:DROP_60 | DROP_90 | \n\
F:INVISIBLE | COLD_BLOOD | PASS_WALL | \n\
F:EVIL | UNDEAD | IM_COLD | NO_CONF | NO_SLEEP | HURT_LITE | HEAL_DARK\n\
S:1_IN_10 | \n\
S:TPORT | SCARE | SHRIEK\n\
D:A ghostly apparition that shrieks horribly. \n\
\n\
N:214:Harpy\n\
G:h:b\n\
I:120:4:13d8:12:30:30\n\
W:12:2:0:40\n\
B:BITE:HURT:2d4\n\
B:CLAW:HURT:2d4\n\
F:WEIRD_MIND | BASH_DOOR | ANIMAL | RES_DARK\n\
S:1_IN_10 | \n\
S:SHRIEK\n\
D:'The harpies, feeding on the black tree's foliage, \n\
D:Cause pain and then an outlet for the pain.' \n\
\n\
######### Inferno   ####################\n\
##### Level 13 , The violent against  ##\n\
#####            God, nature and arts ##\n\
##### Seventh  Circle , Inner Ring   ###\n\
\n\
N:215:Zoophile\n\
G:p:R\n\
I:110:1:8d8:20:32:30\n\
W:13:1:0:16\n\
B:HIT:HURT:1d8\n\
F:MALE | DROP_60 | DROP_90 | FRIENDS\n\
F:OPEN_DOOR | BASH_DOOR | MALE |  \n\
D:This man wasted his seed on beasts, violating one \n\
D:of man's oldest rules. \n\
\n\
N:216:Blasphemer\n\
G:p:v\n\
I:110:1:8d8:20:32:30\n\
W:13:1:0:16\n\
B:HIT:HURT:1d8\n\
F:MALE | DROP_60 | DROP_90 | FRIENDS\n\
F:OPEN_DOOR | BASH_DOOR | MALE |  \n\
D:He cursed the name of God and is not about to stop. \n\
\n\
N:217:Moneylender\n\
G:p:y\n\
I:110:1:8d8:20:32:30\n\
W:13:1:0:16\n\
B:HIT:HURT:1d8\n\
F:MALE | DROP_60 | DROP_90 | FRIENDS\n\
F:OPEN_DOOR | BASH_DOOR | MALE |  \n\
D:A large money pouch  around his neck he is batting \n\
D:away the flaming debris. \n\
\n\
N:218:Sodomite\n\
G:p:R\n\
I:110:1:8d8:20:32:30\n\
W:13:1:0:16\n\
B:HIT:HURT:1d8\n\
F:MALE | DROP_60 | DROP_90 | FRIENDS\n\
F:OPEN_DOOR | BASH_DOOR | MALE |  \n\
D:He will burn. \n\
\n\
N:219:Revenant Nephilim\n\
G:P:U\n\
I:110:1:13d9:20:33:30\n\
W:13:2:0:50\n\
B:HIT:HURT:2d8\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | GIANT | RES_DARK | RES_LITE | RES_FIRE\n\
D:A lascivious spirit, engendered of a giant man who dies in the massacre in the time of the giants. \n\
\n\
N:220:Capaneus\n\
G:P:U\n\
I:110:1:13d9:20:33:30\n\
W:13:2:0:50\n\
B:HIT:HURT:2d8\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | GIANT | UNIQUE\n\
D: Capaneus who took part in the siege against Thebes. He represents the blasphemers in the third round of the seventh circle. \n\
D:'That was one of the seven kings \n\
D:who laid siege to Thebes; he held and seems \n\
D:to hold God in disdain and prize him little.' \n\
\n\
N:221:Eteoclus\n\
G:P:U\n\
I:110:1:13d9:20:33:30\n\
W:13:2:0:50\n\
B:HIT:HURT:2d8\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | GIANT | UNIQUE\n\
D:One of the seven kings that laid siege to Thebes, \n\
D:his afterlife is linked with Capaneus. \n\
\n\
N:222:Hippomedon\n\
G:P:U\n\
I:110:1:13d9:20:33:30\n\
W:13:2:0:50\n\
B:HIT:HURT:2d8\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | GIANT | UNIQUE\n\
D:One of the seven kings that laid siege to Thebes, \n\
D:his afterlife is linked with Capaneus. \n\
\n\
N:223:Parthenopaeus\n\
G:P:U\n\
I:110:1:13d9:20:33:30\n\
W:13:2:0:50\n\
B:HIT:HURT:2d8\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | GIANT | UNIQUE\n\
D:One of the seven kings that laid siege to Thebes, \n\
D:his afterlife is linked with Capaneus. \n\
\n\
N:224:Polynices\n\
G:P:U\n\
I:110:1:13d9:20:33:30\n\
W:13:2:0:50\n\
B:HIT:HURT:2d8\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | GIANT | UNIQUE\n\
D:One of the seven kings that laid siege to Thebes, \n\
D:his afterlife is linked with Capaneus. \n\
\n\
N:225:Tydeus\n\
G:P:U\n\
I:110:1:13d9:20:33:30\n\
W:13:2:0:50\n\
B:HIT:HURT:2d8\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | GIANT | UNIQUE\n\
D:One of the seven kings that laid siege to Thebes, \n\
D:his afterlife is linked with Capaneus. \n\
\n\
N:226:Blasphemer\n\
G:p:g\n\
I:110:2:12d10:20:36:20\n\
W:13:2:0:45\n\
B:HIT:HURT:3d4\n\
F:MALE | DROP_60 | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
D:'More numerous were those who roamed around; \n\
D:Fewer were those stretched out for the torture, \n\
D:But looser were their tongues to tell their hurt.' \n\
\n\
N:227:Brunetto Latini\n\
G:p:g\n\
I:110:2:12d10:20:36:20\n\
W:13:2:0:45\n\
B:HIT:HURT:3d4\n\
F:MALE | DROP_60 | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
D:'And I, when he stretched out his arm to me, \n\
D:So fixed my eyes upon his burnt-out features \n\
D:Even his crusted face did not prevent me \n\
D:From apprehending him in my mind’s eye' \n\
\n\
N:228:Priscian\n\
G:p:g\n\
I:110:2:12d10:20:36:20\n\
W:13:2:0:45\n\
B:HIT:HURT:3d4\n\
F:MALE | DROP_60 | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
D:'Priscian travels with that stricken crowd, \n\
D:And Francesco d’Accorso too, and you may see, \n\
D:if you have any appetite for such scurf.' \n\
D:Priscian was a law professor of Bologna of the thirteenth century. \n\
\n\
N:229:Francesco d’Accorso\n\
G:p:g\n\
I:110:2:12d10:20:36:20\n\
W:13:2:0:45\n\
B:HIT:HURT:3d4\n\
F:MALE | DROP_60 | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
D:'Priscian travels with that stricken crowd, \n\
D:And Francesco d’Accorso too, and you may see, \n\
D:if you have any appetite for such scurf.' \n\
D:Francesco d'Accorso (1125-1294) taught law at Bologna and Oxford. \n\
\n\
N:230:Andrea de' Mozzi\n\
G:p:w\n\
I:110:2:12d10:20:36:20\n\
W:13:2:0:45\n\
B:HIT:HURT:3d4\n\
F:MALE | DROP_60 | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
D:'The one the Servant of Servants transferred \n\
D:From the Arno to the Bacchiglione river \n\
D:Where he left his organs stretched by sin.' \n\
\n\
N:231:Guido Guerra\n\
G:p:o\n\
I:110:2:12d10:20:36:20\n\
W:13:2:0:45\n\
B:HIT:HURT:3d4\n\
F:MALE | DROP_60 | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
D:'He was the grandson of the good Gualdrada; \n\
D:His name was Guido Guerra — in his life \n\
D:Much he achieved by counsel and his sword.' \n\
\n\
N:232:Tegghiaio Aldobrandi\n\
G:p:o\n\
I:110:2:12d10:20:36:20\n\
W:13:2:0:45\n\
B:HIT:HURT:3d4\n\
F:MALE | DROP_60 | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
D:'The other who thrashes the sand behind me \n\
D:Is Tegghiaio Aldobrandi, whose voice \n\
D:In the world above ought to have won favor.' \n\
\n\
N:233:Jacopo Rusticucci\n\
G:p:o\n\
I:110:2:12d10:20:36:20\n\
W:13:2:0:45\n\
B:HIT:HURT:3d4\n\
F:MALE | DROP_60 | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
D:'And I who am placed with them in this torment \n\
D:Was Jacopo Rusticucci, and surely \n\
D:My hell-cat wife — more than anyone — ruined me!' \n\
\n\
N:234:Guglielmo Borsiere\n\
G:p:o\n\
I:110:2:12d10:20:36:20\n\
W:13:2:0:45\n\
B:HIT:HURT:3d4\n\
F:MALE | DROP_60 | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
D:'Cavaliere di corte, uomo costumato molto e di laudevol maniera,' a well-mannered knight of the court. \n\
\n\
N:235:Gianfigliazzi of Florence\n\
G:p:y\n\
I:110:2:12d10:20:36:20\n\
W:13:2:0:75\n\
B:HIT:HURT:7d4\n\
F:MALE | DROP_4D2 | DROP_3D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | FORCE_MAXHP | AURA_FIRE | REGENERATE | \n\
F:RES_ACID | RES_FIRE | RES_COLD | RES_ELEC | RES_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR | \n\
D:'While I went among them, looking about \n\
D:I glimpsed a purse of yellow upon azure \n\
D:Which bore the face and figure of a lion.' \n\
D:This usurer is rich, rich and very dangerous. \n\
\n\
N:236:Ubriachi of Florence\n\
G:p:y\n\
I:110:2:12d10:20:36:20\n\
W:13:2:0:75\n\
B:HIT:HURT:7d4\n\
F:MALE | DROP_4D2 | DROP_3D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | FORCE_MAXHP | AURA_FIRE | REGENERATE | \n\
F:RES_ACID | RES_FIRE | RES_COLD | RES_ELEC | RES_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR | \n\
D:'Then, letting my gaze wander over them, \n\
D:I saw another purse as red as blood \n\
D:Displaying a goose whiter than butter.' \n\
D:This usurer is rich, rich and very dangerous. \n\
\n\
N:237:Scrovegni of Padua\n\
G:p:y\n\
I:110:2:12d10:20:36:20\n\
W:13:2:0:75\n\
B:HIT:HURT:7d4\n\
F:MALE | DROP_4D2 | DROP_3D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | FORCE_MAXHP | AURA_FIRE | REGENERATE | \n\
F:RES_ACID | RES_FIRE | RES_COLD | RES_ELEC | RES_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR | \n\
D:'And one who had an azure pregnant sow \n\
D:Represented on his small white pouch' \n\
D:This usurer is rich, rich and very dangerous. \n\
\n\
N:238:Vitaliano of Padua\n\
G:p:y\n\
I:110:2:12d10:20:36:20\n\
W:13:2:0:75\n\
B:HIT:HURT:7d4\n\
F:MALE | DROP_4D2 | DROP_3D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | FORCE_MAXHP | AURA_FIRE | REGENERATE | \n\
F:RES_ACID | RES_FIRE | RES_COLD | RES_ELEC | RES_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR | \n\
D:This usurer is rich, rich and very dangerous. \n\
\n\
N:239:Giovanni Buiamonte of Florence\n\
G:p:y\n\
I:110:2:12d10:20:36:20\n\
W:13:2:0:75\n\
B:HIT:HURT:7d4\n\
F:MALE | DROP_4D2 | DROP_3D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | FORCE_MAXHP | AURA_FIRE | REGENERATE | \n\
F:RES_ACID | RES_FIRE | RES_COLD | RES_ELEC | RES_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR | \n\
D:'Bring on the royal knight \n\
D:Who bears on him his pouch with the three goats!' \n\
D:This usurer is rich, rich and very dangerous. \n\
\n\
N:240:Rescinderer\n\
G:y:y\n\
I:120:4:13d8:12:18:1\n\
W:13:4:0:40\n\
B:TOUCH:ACID:1d6\n\
B:TOUCH:HURT:1d6\n\
F:STUPID | EMPTY_MIND | \n\
F:TAKE_ITEM | OPEN_DOOR | KILL_BODY | RES_FIRE\n\
D:A fast moving highly efficient little creature, it picks up everything from the floor \n\
D:and throws them into an iron burning vat that is strapped to it's back. \n\
\n\
######### Inferno   ####################\n\
##### Level 14 , Panderers            ##\n\
#####            and seducers         ##\n\
##### Eight  Circle , First Ditch    ###\n\
\n\
N:241:Geryon\n\
G:P:B\n\
I:130:4:34d10:20:40:40\n\
W:14:4:0:200\n\
B:HIT:HURT:3d5\n\
B:GAZE:TERRIFY\n\
F:UNIQUE |MALE | GIANT |\n\
F:FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_90 | DROP_GOOD | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | EVIL\n\
F:RES_ELEC | RES_FIRE | RES_COLD | RES_DARK\n\
D:'His face was the face of a saintly person, \n\
D:So placid was the surface of the skin, \n\
D:But his whole trunk was the shape of a snake. \n\
D:He had two paws, with hair up to his armpits; \n\
D:His back and breasts and both of his flanks \n\
D:Were painted gaudily with knots and loops \n\
D:Tartars or Turks never wove a cloth \n\
D:With more colors in background and design, \n\
D:Nor did Arachne ever loom such webs.' \n\
\n\
N:242:Pimp\n\
G:p:R\n\
I:120:2:8d8:20:20:40\n\
W:14:2:0:40\n\
B:BITE:HURT:1d8\n\
F:FRIENDS | BASH_DOOR | OPEN_DOOR | MALE\n\
D:He is supposed to walk single file, \n\
D:your arrival seems to have broken his routine. \n\
\n\
N:243:Seducer\n\
G:p:R\n\
I:120:2:8d8:20:20:40\n\
W:14:2:0:40\n\
B:BITE:HURT:1d8\n\
F:FRIENDS | BASH_DOOR | OPEN_DOOR | MALE\n\
D:He is supposed to walk single file, \n\
D:your arrival seems to have broken his routine. \n\
\n\
N:244:Whipping Devil\n\
G:u:D\n\
I:110:2:20d9:20:33:30\n\
W:14:2:0:75\n\
B:HIT:HURT:2d8\n\
F:FRIENDS | DROP_60 | OPEN_DOOR | BASH_DOOR | EVIL | DEMON\n\
F:RES_FIRE | RES_DARK\n\
D:' saw horned devils with their huge long whips \n\
D:Cruelly lashing those sinners from behind. \n\
D:Ah how they forced them to lift up their heels \n\
D:At the first strokes! There was nobody there \n\
D:Who waited for the second or the third!' \n\
\n\
N:245:Venedico Caccianemico	\n\
G:p:R\n\
I:120:2:8d8:20:20:40\n\
W:14:2:0:40\n\
B:BITE:HURT:1d8\n\
F:BASH_DOOR | OPEN_DOOR | MALE | UNIQUE\n\
D:Venedico Caccianemico, a noble Bolognese Guelph, was said to have been a \n\
D:procurer of his sister Ghisolabella to gain the favor of Obizzo II of Este. \n\
\n\
N:246:Jason\n\
G:p:v\n\
I:110:4:34d10:20:40:40\n\
W:14:3:0:200\n\
B:HIT:HURT:3d5\n\
B:GAZE:TERRIFY\n\
F:UNIQUE | MALE | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_4D2 | DROP_GREAT | OPEN_DOOR | BASH_DOOR | \n\
D:Jason, leader of the Argonauts, stole the golden fleece with the help of Medea. \n\
D:After leaving Colchis for Athens, he then deserted her (as he had Hypsipyle) and, \n\
D:in revenge, she murdered their two sons. \n\
\n\
\n\
######### Inferno   ####################\n\
##### Level 15 , Flatterers           ##\n\
##### Eight  Circle , 2nd Ditch      ###\n\
\n\
N:247:Flatterer\n\
G:p:u\n\
I:120:2:8d8:2:20:40\n\
W:15:1:0:40\n\
B:BITE:HURT:1d8\n\
F:FRIENDS | BASH_DOOR | OPEN_DOOR | MALE\n\
D:'And as I searched below there with my eyes \n\
D:I saw one with his head so smeared with shit \n\
D:You could not tell if he were lay or cleric.' \n\
\n\
N:248:Alessio Interminei of Lucca\n\
G:p:u\n\
I:120:2:8d8:20:20:40\n\
W:15:2:0:40\n\
B:BITE:HURT:1d8\n\
F:UNIQUE | BASH_DOOR | OPEN_DOOR | MALE\n\
D:'Down here I am sunk by the flatteries \n\
D:That my tongue never tired of repeating.' \n\
\n\
N:249:Thais the Whore\n\
G:p:o\n\
I:120:2:8d8:20:20:40\n\
W:15:2:0:40\n\
B:BITE:HURT:1d8\n\
F:UNIQUE | BASH_DOOR | OPEN_DOOR | FEMALE\n\
D:'Stretch your head forward a little farther \n\
D:So that your eyes may clearly catch the face \n\
D:Of that slatternly and smutty slut \n\
D:Who scratches herself with shit-blackened nails, \n\
D:Now squatting and now staggering to her feet. \n\
D:She is Thais the whore...' \n\
\n\
######### Inferno   ####################\n\
##### Level 16 , Simoniacs            ##\n\
##### Eight  Circle , 2nd Ditch      ###\n\
\n\
N:250:Simoniac\n\
G:p:o\n\
I:110:4:20d10:20:50:10\n\
W:16:1:0:75\n\
B:HIT:HURT:3d5\n\
F:BASH_DOOR | AURA_FIRE | OPEN_DOOR | MALE\n\
F:NO_CONF | NO_SLEEP\n\
D:'O miserable lot \n\
D:Who take the things of God that ought to be \n\
D:Wedded to goodness and in your greediness \n\
D:Adulterate them into gold and silver! \n\
D:Now the trumpet blast must sound for you \n\
D:Since you are stashed here into the third pocket.' \n\
\n\
N:251:Fire giant\n\
G:P:r\n\
I:110:2:20d8:20:60:50\n\
W:16:2:0:54\n\
B:HIT:FIRE:3d7\n\
B:HIT:HURT:3d7\n\
F:DROP_60 | \n\
F:OPEN_DOOR | BASH_DOOR | AURA_FIRE\n\
F:EVIL | GIANT | MALE | IM_FIRE\n\
D:A glowing fourteen foot tall giant. Flames drip from its red skin. \n\
\n\
N:252:Simon Magus\n\
G:p:o\n\
I:110:4:34d10:20:40:40\n\
W:16:3:0:400\n\
B:HIT:HURT:3d5\n\
B:GAZE:TERRIFY\n\
F:UNIQUE |MALE | \n\
F:FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_90 | DROP_GOOD | ATTR_MULTI\n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | AURA_FIRE\n\
F:EVIL\n\
S:1_IN_5 | \n\
S:BR_FIRE\n\
D:Simon Magus attempted to buy the miraculous power of the apostles \n\
D:(Acts 8:9-24) and is here as the father of the simoniacs of the third bolgia. \n\
\n\
N:253:Nicholas III\n\
G:p:o\n\
I:110:4:20d10:20:50:10\n\
W:16:1:0:75\n\
B:HIT:HURT:3d5\n\
F:BASH_DOOR | AURA_FIRE | OPEN_DOOR | MALE | ATTR_MULTI\n\
F:NO_CONF | NO_SLEEP | UNIQUE | FORCE_MAXHP\n\
F:ONLY_ITEM | DROP_4D2 | DROP_3D2 | DROP_GREAT\n\
D:'Who is that sinner, master, who suffers so, \n\
D:Writhing more than any of his comrades, \n\
D:I asked, the one the redder flame licks dry?' \n\
D:Pope from 1277 to 1280, who as a member of the Orsini family handed out benefices to his relatives. \n\
\n\
N:254:Boniface VIII\n\
G:p:o\n\
I:110:4:20d10:20:50:10\n\
W:16:1:0:75\n\
B:HIT:HURT:3d5\n\
F:BASH_DOOR | AURA_FIRE | OPEN_DOOR | MALE | ATTR_MULTI\n\
F:NO_CONF | NO_SLEEP | UNIQUE | FORCE_MAXHP\n\
F:ONLY_ITEM | DROP_4D2 | DROP_3D2 | DROP_GREAT\n\
D:Boniface VIII (d. 1303) is here because of his misuse of the papacy and 'the lovely lady,' the church. \n\
\n\
N:255:Fire Quasit\n\
G:u:r\n\
I:110:2:6d8:20:30:20\n\
W:16:3:0:50\n\
B:BITE:LOSE_DEX:1d6\n\
B:CLAW:HURT:1d3\n\
F:FORCE_SLEEP | \n\
F:RAND_25 | \n\
F:ONLY_ITEM | DROP_1D2 | AURA_FIRE\n\
F:SMART | INVISIBLE | BASH_DOOR | \n\
F:EVIL | DEMON | IM_FIRE | NONLIVING |\n\
S:1_IN_10 | \n\
S:BLINK | TPORT | TELE_TO | TELE_LEVEL | BLIND | CONF | SCARE\n\
D:The chaotic evil master's favourite pet. \n\
\n\
N:256:Flaming adamantite coins\n\
G:$:r\n\
I:120:3:20d25:5:50:10\n\
W:16:4:0:45\n\
B:BITE:FIRE:3d4\n\
B:TOUCH:POISON:3d5\n\
B:HIT:HURT:1d12\n\
F:ONLY_GOLD | DROP_90 | DROP_2D2 | BASH_DOOR | AURA_FIRE\n\
F:ANIMAL | \n\
F:IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_5\n\
S:BA_FIRE\n\
D:It is a pile of coins, slithering forward on thousands of tiny legs. \n\
\n\
\n\
######### Inferno   ####################\n\
##### Level 17 , Sorcerers            ##\n\
##### Eight  Circle , 4th Ditch      ###\n\
\n\
N:257:Grand master mystic\n\
G:p:o\n\
I:120:3:35d10:30:50:5\n\
W:17:3:0:80\n\
B:KICK:HURT:10d2\n\
B:KICK:HURT:10d2\n\
B:KICK:HURT:10d2\n\
B:KICK:HURT:10d2\n\
F:MALE\n\
F:FORCE_SLEEP | FORCE_MAXHP\n\
F:ONLY_ITEM | DROP_1D2\n\
F:INVISIBLE | OPEN_DOOR | BASH_DOOR\n\
F:RES_ACID | RES_POIS | NO_CONF | NO_SLEEP\n\
F:RAND_25 | RAND_50 | \n\
S:1_IN_5\n\
S:HEAL | S_MONSTERS\n\
D:An adept at unarmed combat, the mystic strikes with stunning power.  He \n\
D:can summon help from nature and is able to focus his power to ease any \n\
D:pain. \n\
\n\
N:258:Imp\n\
G:u:r\n\
I:110:2:6d8:20:30:20\n\
W:17:2:0:75\n\
B:HIT:POISON:3d4\n\
F:FORCE_SLEEP | \n\
F:RAND_25 | \n\
F:ONLY_ITEM | DROP_1D2 | \n\
F:SMART | INVISIBLE | COLD_BLOOD | BASH_DOOR | \n\
F:EVIL | DEMON | IM_FIRE | RES_TELE | RES_POIS | RES_ELEC\n\
S:1_IN_5 | \n\
S:BLINK | TPORT | TELE_TO | TELE_LEVEL | BLIND | CONF | BO_FIRE\n\
D:A favourite pet of wizards, this imp feels it is payback time. \n\
\n\
N:259:Illusionist\n\
G:p:y\n\
I:110:1:12d8:20:10:10\n\
W:17:1:0:50\n\
B:HIT:HURT:2d2\n\
F:MALE | \n\
F:RAND_25 | RAND_50 | \n\
F:FORCE_SLEEP | DROP_1D2 | \n\
F:SMART | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL\n\
S:1_IN_3 | \n\
S:HASTE | BLINK | TPORT | BLIND | HOLD | SLOW | CONF | DARKNESS\n\
D:A deceptive spell caster. \n\
\n\
N:260:Cultist\n\
G:p:B\n\
I:110:2:12d8:20:22:40\n\
W:17:1:0:36\n\
B:HIT:HURT:2d3\n\
F:MALE | DROP_1D2 | SMART | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:RAND_25 | RAND_50 | \n\
S:1_IN_3 |\n\
S:HEAL | SCARE | CAUSE_2 |S_MONSTER\n\
D:A robed spirit dedicated to his heathen god. \n\
\n\
N:261:Amphiaraus\n\
G:P:v\n\
I:120:5:50d10:20:50:20\n\
W:17:4:0:800\n\
B:HIT:HURT:3d6\n\
F:UNIQUE | MALE |FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | RES_POIS | REFLECTING | REGENERATE\n\
D:Amphiaraus was another of the seven kings who fought in the siege of Thebes. \n\
D:He was a seer who foresaw his own death; he is damned to the fourth bolgia. \n\
\n\
N:262:Tiresias\n\
G:p:v\n\
I:120:5:50d10:20:50:20\n\
W:17:4:0:800\n\
B:HIT:HURT:3d6\n\
F:UNIQUE | MALE |FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | RES_POIS | REFLECTING | REGENERATE\n\
D:'See Tiresias, who changed his likeness: \n\
D:Being a man he then became a woman, \n\
D:Transforming all the members of his body, \n\
D:Until, a second time, he had to strike \n\
D:The two lovemaking serpents with his staff \n\
D:Before he donned again his manly down.' \n\
\n\
N:263:Aruns\n\
G:p:v\n\
I:120:5:50d10:20:50:20\n\
W:17:4:0:800\n\
B:HIT:HURT:3d6\n\
F:UNIQUE | MALE |FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | RES_POIS | REFLECTING | REGENERATE\n\
D:Aruns was an Etruscan diviner who prophesied the Roman civil war. \n\
\n\
N:264:Manto\n\
G:p:v\n\
I:120:5:50d10:20:50:20\n\
W:17:4:0:800\n\
B:HIT:HURT:3d6\n\
F:UNIQUE | FEMALE |FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | RES_POIS | REFLECTING | REGENERATE | SMART\n\
S:1_IN_3 | \n\
S:HASTE | TPORT | BLIND | HOLD | CONF | S_MONSTERS | HEAL\n\
D:'And she who with her wild disheveled hair \n\
D:Covers up her breasts so you can’t see them \n\
D:And keeps all of her hairy parts to that side \n\
D:Was Manto, who had searched through many lands' \n\
\n\
N:265:Eurypylus\n\
G:p:v\n\
I:120:5:50d10:20:50:20\n\
W:17:4:0:800\n\
B:HIT:HURT:3d6\n\
F:UNIQUE | MALE |FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | RES_POIS | REFLECTING | REGENERATE\n\
D:'That one whose beard \n\
D:Streams down from his cheeks to his brown shoulders' \n\
D:Eurypylus was a greek augur involved in choosing the sailing day for Troy. \n\
\n\
N:266:Calchas\n\
G:p:v\n\
I:120:5:50d10:20:50:20\n\
W:17:4:0:800\n\
B:HIT:HURT:3d6\n\
F:UNIQUE | MALE |FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | RES_POIS | REFLECTING | REGENERATE\n\
D:Calchas was a greek augur involved in choosing the sailing day for Troy. \n\
\n\
N:267:Michael Scot\n\
G:p:v\n\
I:120:5:50d10:20:50:20\n\
W:17:4:0:800\n\
B:HIT:HURT:3d6\n\
F:UNIQUE | MALE |FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | RES_POIS | REFLECTING | REGENERATE\n\
D:'That other one, so thinned-out in the shanks, \n\
D:Was Michael Scot, who certainly perceived \n\
D:How to play the game of magic fraud.' \n\
\n\
N:268:Asdente\n\
G:p:v\n\
I:120:5:50d10:20:50:20\n\
W:17:4:0:800\n\
B:HIT:HURT:3d6\n\
F:UNIQUE | MALE |FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | RES_POIS | REFLECTING | REGENERATE\n\
D:'See Asdente, \n\
D:Who wishes now he had kept to his thread \n\
D:And shoe-leather, but he repents too late.' \n\
\n\
N:269:Soothsayer\n\
G:p:s\n\
I:110:2:24d8:20:75:50\n\
W:17:1:0:90\n\
B:HIT:HURT:3d8\n\
B:HIT:HURT:3d8\n\
F:RAND_25 | RAND_50 |\n\
F:DROP_60 | TAKE_ITEM | OPEN_DOOR | BASH_DOOR | \n\
D:''Look how he’s made a chest of his own shoulders: \n\
D:Because he wished to see too far ahead \n\
D:He stares behind and takes a backward path.' \n\
\n\
N:270:Witch\n\
G:p:s\n\
I:110:2:24d8:20:75:50\n\
W:17:1:0:90\n\
B:HIT:HURT:3d8\n\
B:HIT:HURT:3d8\n\
F:RAND_25 | RAND_50 |\n\
F:DROP_60 | TAKE_ITEM | OPEN_DOOR | BASH_DOOR | \n\
D:'See those wretched women who left needle, \n\
D:Spool, and spindle for their fortune-telling; \n\
D:They cast their spells with herbs and image-dolls.' \n\
\n\
N:271:Sorcerer\n\
G:p:o\n\
I:110:1:52d10:20:60:10\n\
W:17:3:0:150\n\
B:HIT:HURT:2d8\n\
F:MALE | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_90 | DROP_4D2 | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
B:HIT:HURT:3d8\n\
S:1_IN_4 | \n\
S:BLIND | CONF | CAUSE_3 |  \n\
S:BA_FIRE | BA_COLD | \n\
S:S_MONSTER | S_UNDEAD | S_DRAGON\n\
D:A human figure in robes, he moves with magically improved speed, and his \n\
D:hands are ablur with spell casting. \n\
\n\
######### Inferno   ####################\n\
##### Level 18 , Corrupt politicians  ##\n\
##### Eight  Circle , 5th Ditch      ###\n\
\n\
N:272:Fire spirit\n\
G:E:R\n\
I:120:3:10d9:16:30:20\n\
W:18:2:0:75\n\
B:HIT:FIRE:2d6\n\
F:RAND_25 | \n\
F:EMPTY_MIND | BASH_DOOR | \n\
F:EVIL | HEAL_FIRE | IM_POIS | HURT_COLD\n\
F:NO_CONF | NO_SLEEP | NO_FEAR | AURA_FIRE\n\
D:A whirlwind of sentient flame. \n\
\n\
N:273:Fire hound\n\
G:Z:r\n\
I:110:3:10d6:30:30:0\n\
W:18:4:0:70\n\
B:BITE:FIRE:1d3\n\
F:FORCE_SLEEP | AURA_FIRE\n\
F:FRIENDS | \n\
F:BASH_DOOR | \n\
F:ANIMAL | HEAL_FIRE | HURT_COLD\n\
S:1_IN_10 | \n\
S:BR_FIRE\n\
D:Flames lick at its feet and its tongue is a blade of fire. You can feel a \n\
D:furnace heat radiating from the creature. \n\
\n\
N:274:Firefly\n\
G:F:r\n\
I:120:1:3d8:12:20:50\n\
W:18:2:0:68\n\
F:FORCE_SLEEP | NEVER_BLOW | RAND_50 | RAND_25 | \n\
F:WEIRD_MIND | BASH_DOOR | \n\
F:ANIMAL | HEAL_FIRE | HURT_COLD\n\
S:1_IN_9 | \n\
S:BR_FIRE\n\
D:The size of a large bird, this fly is surrounded by crackling fire. \n\
\n\
N:275:Corrupt politician\n\
G:p:s\n\
I:110:2:24d8:20:75:50\n\
W:18:1:0:90\n\
B:HIT:HURT:3d8\n\
F:DROP_60 | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | EVIL | RES_FIRE\n\
D:He is avoiding the Malebranches. He seems to think \n\
D:you are one of their agents. \n\
\n\
N:276:Friar Gomita\n\
G:p:y\n\
I:110:2:24d8:20:75:50\n\
W:18:1:0:90\n\
B:HIT:HURT:3d8\n\
B:HIT:HURT:3d8\n\
B:HIT:HURT:3d8\n\
B:HIT:HURT:3d8\n\
F:DROP_60 | UNIQUE | DROP_90 | DROP_4D2 | DROP_3D2 | DROP_GREAT\n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | EVIL\n\
D:'That was Friar Gomita \n\
D:From Gallura, a purse for every fraud! \n\
D:He had his master’s enemies in his hands \n\
D:And treated them so that they sang his praises. \n\
D:He took their cash and let them off scot free, \n\
D:As he admits, and in his other dealings \n\
D:He was no petty thief but a royal one.' \n\
\n\
N:277:Michel Zanche\n\
G:p:y\n\
I:110:2:24d8:20:75:50\n\
W:18:1:0:90\n\
B:HIT:HURT:3d8\n\
B:HIT:HURT:3d8\n\
B:HIT:HURT:3d8\n\
B:HIT:HURT:3d8\n\
F:DROP_60 | UNIQUE | DROP_90 | DROP_4D2 | DROP_3D2 | DROP_GREAT\n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | EVIL\n\
D:Michel Zanche (d. 1290) administered another Sardinian district, Logodoro. \n\
D:He is usually seen in the neighbourhood of Friar Gomita. \n\
\n\
N:278:Malebranche\n\
G:U:r\n\
I:110:3:35d10:10:68:90\n\
W:18:2:0:550\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
F:FORCE_SLEEP | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | AURA_FIRE | NONLIVING |\n\
F:EVIL | DEMON | IM_FIRE | RES_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_7 | \n\
S:BO_FIRE | S_DEMON\n\
D:The Malebranche are the squadron leaders of the devils who torment \n\
D:the grafters of the fifth pocket of the eighth circle. The name means 'Evil-Claws.' \n\
\n\
N:279:Malacoda\n\
G:U:v\n\
I:110:3:35d10:10:68:90\n\
W:18:2:0:850\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
F:FORCE_SLEEP | UNIQUE | FORCE_MAXHP | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | AURA_FIRE | NONLIVING |\n\
F:EVIL | DEMON | IM_FIRE | RES_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_5 | \n\
S:BO_FIRE | S_DEMON | BR_FIRE\n\
D:A leader of the Malebranche, he has the power to summon them. \n\
\n\
N:280:Scarmiglione\n\
G:U:v\n\
I:110:3:30d10:10:68:90\n\
W:18:2:0:750\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
F:FORCE_SLEEP | UNIQUE | FORCE_MAXHP |  \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | AURA_FIRE | NONLIVING |\n\
F:EVIL | DEMON | IM_FIRE | RES_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_7 | \n\
S:BO_FIRE | S_DEMON\n\
D:It is a member of Malacoda's squad. \n\
\n\
N:281:Calcabrina\n\
G:U:v\n\
I:110:3:30d10:10:68:90\n\
W:18:2:0:750\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
F:FORCE_SLEEP | UNIQUE | FORCE_MAXHP |  \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | AURA_FIRE | NONLIVING |\n\
F:EVIL | DEMON | IM_FIRE | RES_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_7 | \n\
S:BO_FIRE | S_DEMON\n\
D:It is a member of Malacoda's squad. \n\
\n\
N:282:Cagnazzo\n\
G:U:v\n\
I:110:3:30d10:10:68:90\n\
W:18:2:0:750\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
F:FORCE_SLEEP | UNIQUE | FORCE_MAXHP |  \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | AURA_FIRE | NONLIVING |\n\
F:EVIL | DEMON | IM_FIRE | RES_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_7 | \n\
S:BO_FIRE | S_DEMON\n\
D:It is a member of Malacoda's squad. \n\
\n\
N:283:Alichino\n\
G:U:v\n\
I:110:3:30d10:10:68:90\n\
W:18:2:0:750\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
F:FORCE_SLEEP | UNIQUE | FORCE_MAXHP |  \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | AURA_FIRE | NONLIVING |\n\
F:EVIL | DEMON | IM_FIRE | RES_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_7 | \n\
S:BO_FIRE | S_DEMON\n\
D:It is a member of Malacoda's squad. \n\
\n\
N:284:Barbariccia\n\
G:U:v\n\
I:110:3:30d10:10:68:90\n\
W:18:2:0:750\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
F:FORCE_SLEEP | UNIQUE | FORCE_MAXHP |  \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | AURA_FIRE | NONLIVING |\n\
F:EVIL | DEMON | IM_FIRE | RES_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_7 | \n\
S:BO_FIRE | S_DEMON\n\
D:It is a member of Malacoda's squad. \n\
\n\
N:285:Libicocco\n\
G:U:v\n\
I:110:3:30d10:10:68:90\n\
W:18:2:0:750\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
F:FORCE_SLEEP | UNIQUE | FORCE_MAXHP |  \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | AURA_FIRE | NONLIVING |\n\
F:EVIL | DEMON | IM_FIRE | RES_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_7 | \n\
S:BO_FIRE | S_DEMON\n\
D:It is a member of Malacoda's squad. \n\
\n\
N:286:Draghignazzo\n\
G:U:v\n\
I:110:3:30d10:10:68:90\n\
W:18:2:0:750\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
F:FORCE_SLEEP | UNIQUE | FORCE_MAXHP |  \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | AURA_FIRE | NONLIVING |\n\
F:EVIL | DEMON | IM_FIRE | RES_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_7 | \n\
S:BO_FIRE | S_DEMON\n\
D:It is a member of Malacoda's squad. \n\
\n\
N:287:Ciriatto\n\
G:U:v\n\
I:110:3:30d10:10:68:90\n\
W:18:2:0:750\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
F:FORCE_SLEEP | UNIQUE | FORCE_MAXHP |  \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | AURA_FIRE | NONLIVING |\n\
F:EVIL | DEMON | IM_FIRE | RES_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_7 | \n\
S:BO_FIRE | S_DEMON\n\
D:It is a member of Malacoda's squad. \n\
\n\
N:288:Graffiacane\n\
G:U:v\n\
I:110:3:30d10:10:68:90\n\
W:18:2:0:750\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
F:FORCE_SLEEP | UNIQUE | FORCE_MAXHP |  \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | AURA_FIRE | NONLIVING |\n\
F:EVIL | DEMON | IM_FIRE | RES_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_7 | \n\
S:BO_FIRE | S_DEMON\n\
D:It is a member of Malacoda's squad. \n\
\n\
N:289:Farfarello\n\
G:U:v\n\
I:110:3:30d10:10:68:90\n\
W:18:2:0:750\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
F:FORCE_SLEEP | UNIQUE | FORCE_MAXHP |  \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | AURA_FIRE | NONLIVING |\n\
F:EVIL | DEMON | IM_FIRE | RES_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_7 | \n\
S:BO_FIRE | S_DEMON\n\
D:It is a member of Malacoda's squad. \n\
\n\
N:290:Rubicante\n\
G:U:v\n\
I:110:3:30d10:10:68:90\n\
W:18:2:0:750\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
F:FORCE_SLEEP | UNIQUE | FORCE_MAXHP |  \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | AURA_FIRE | NONLIVING |\n\
F:EVIL | DEMON | IM_FIRE | RES_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_7 | \n\
S:BO_FIRE | S_DEMON\n\
D:It is a member of Malacoda's squad. \n\
\n\
######### Inferno   ####################\n\
##### Level 19 , Hypocrites           ##\n\
##### Eight  Circle , 6th Ditch      ###\n\
\n\
###He might be too powerfull ###\n\
N:291:Caiaphas the High Priest\n\
G:p:o\n\
I:70:1:50d10:20:50:20\n\
W:19:4:0:1600\n\
B:HIT:HURT:3d6\n\
B:HIT:HURT:3d6\n\
F:UNIQUE | MALE | FORCE_MAXHP | SMART | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | DROP_GREAT\n\
F:OPEN_DOOR | BASH_DOOR | EVIL | ATTR_MULTI | \n\
S:1_IN_3 | \n\
S:TELE_TO | HEAL\n\
D:'That one you see nailed down \n\
D:Advised the Pharisees it was expedient \n\
D:To sacrifice one man for the people. \n\
D:Stretched out naked he lies, across the way, \n\
D:As you yourself see, and is made to feel \n\
D:The full weight of every passer-by.' \n\
D:It seems Caiaphas got rid of his cross. \n\
\n\
N:292:Annas\n\
G:p:o\n\
I:120:5:50d10:20:50:20\n\
W:19:4:0:800\n\
B:HIT:HURT:3d6\n\
F:UNIQUE | MALE | FORCE_MAXHP | SMART | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD |\n\
F:OPEN_DOOR | BASH_DOOR | EVIL |\n\
D:Annas, the father-in-law of Caiaphas took part in sentencing Jesus to death. \n\
D:It seems he got rid of his cross. \n\
\n\
N:293:Doomed Council Member\n\
G:#:R\n\
I:120:5:50d10:20:50:20\n\
W:19:4:0:80\n\
B:HIT:HURT:3d6\n\
F:UNIQUE | MALE | FORCE_MAXHP | SMART | \n\
F:ONLY_ITEM | EVIL | NEVER_MOVE\n\
D:This member of the council took part in sentencing Jesus to death. \n\
D:Now, he must spend eternity on a wooden cross. \n\
\n\
N:294:Catalano de’ Catalani\n\
G:p:y\n\
I:70:1:80d12:12:80:10\n\
W:19:1:0:160\n\
B:HIT:HURT:1d12\n\
F:BASH_DOOR | UNIQUE | \n\
D:A chief magistrates over Florence in 1266 to preserve peace, he failed. \n\
\n\
N:295:Loderingo degli Andalo\n\
G:p:y\n\
I:70:1:80d12:12:80:10\n\
W:19:1:0:160\n\
B:HIT:HURT:1d12\n\
F:BASH_DOOR | UNIQUE | \n\
D:A chief magistrates over Florence in 1266 to preserve peace, he failed. \n\
\n\
N:296:Hypocrite\n\
G:p:w\n\
I:70:1:80d12:12:80:10\n\
W:19:1:0:160\n\
B:HIT:HURT:1d12\n\
F:FORCE_SLEEP | \n\
F:EMPTY_MIND | COLD_BLOOD | BASH_DOOR | \n\
F:ATTR_MULTI | ATTR_ANY | \n\
F:IM_POIS | \n\
D:This hypocrite is sealed into a brightly painted leaden cloak. \n\
\n\
######### Inferno   ####################\n\
##### Level 20 , Thieves              ##\n\
##### Eight  Circle , 7th Ditch      ###\n\
\n\
N:297:Guild Thief\n\
G:p:b\n\
I:120:4:10d8:15:25:80\n\
W:20:3:0:95\n\
B:BITE:EAT_GOLD:8d1\n\
B:BITE:POISON:6d1\n\
B:BITE:EAT_GOLD:8d1\n\
B:BITE:POISON:6d1\n\
F:FRIENDS | RES_POIS | RES_TELE | MALE\n\
S:1_IN_3 | \n\
S:BLINK | TELE_TO\n\
D:A thief that never seems quite there. Everywhere you look he is just \n\
D:half-seen in the corner of one eye. \n\
\n\
N:298:Klepht\n\
G:p:D\n\
I:120:4:10d8:15:25:80\n\
W:20:3:0:95\n\
B:BITE:EAT_GOLD:8d1\n\
B:BITE:POISON:6d1\n\
B:BITE:EAT_GOLD:8d1\n\
B:BITE:POISON:6d1\n\
F:FRIENDS | MALE\n\
D:Klephts were bandits who lived in the Greek countryside when that country was a part of the Ottoman Empire. \n\
\n\
N:299:Master rogue\n\
G:p:D\n\
I:120:5:15d9:20:30:40\n\
W:20:1:0:110\n\
B:HIT:HURT:2d8\n\
B:HIT:EAT_GOLD:4d4\n\
F:MALE | \n\
F:DROP_2D2 | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL\n\
D:A thief of great power and shifty speed. \n\
\n\
#Ever so slightly out of depth ( original depth is 34 )\n\
N:300:Master thief\n\
G:p:D\n\
I:130:6:18d10:20:30:40\n\
W:20:2:0:350\n\
B:HIT:HURT:2d8\n\
B:HIT:EAT_GOLD:4d4\n\
B:HIT:EAT_ITEM:4d5\n\
F:MALE | \n\
F:DROP_90 | DROP_2D2 | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL\n\
D:Cool and confident, fast and lithe; protect your possessions quickly! \n\
\n\
#Claude Duval	(1643 – January 21, 1670) \n\
#Here lies DuVall: Reder, if male thou art,\n\
#Look to thy purse; if female, to thy heart.\n\
N:301:Francois Villon\n\
G:p:D\n\
I:130:6:18d10:20:30:40\n\
W:20:2:0:350\n\
B:HIT:HURT:2d8\n\
B:HIT:EAT_GOLD:4d4\n\
B:HIT:EAT_ITEM:4d5\n\
F:MALE | UNIQUE | \n\
F:DROP_90 | DROP_2D2 | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | ATTR_MULTI\n\
D:Francois Villon (ca. 1431 - ca. 1474) was a French poet, thief, and general vagabond. \n\
\n\
#Ever so slightly out of depth ( original depth is 52 )\n\
N:302:Grand master thief\n\
G:p:D\n\
I:140:7:25d100:40:90:0\n\
W:20:3:0:20000\n\
B:HIT:HURT:3d6\n\
B:TOUCH:EAT_GOLD:5d5\n\
B:TOUCH:EAT_ITEM:5d5\n\
F:MALE | FORCE_SLEEP | \n\
F:ONLY_ITEM | DROP_1D2 | DROP_4D2 | DROP_GOOD | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | \n\
F:RES_POIS | NO_CONF | NO_SLEEP | EVIL | RES_TELE\n\
S:1_IN_6 | \n\
S:TRAPS\n\
D:A furtive figure who makes you want to hide all your valuables. \n\
D:He still thinks that Inferno is one big treasure vault. \n\
\n\
N:303:Valefar, Duke of Hell\n\
G:U:u\n\
I:110:3:150d10:10:130:40\n\
W:20:3:0:900\n\
B:CLAW:HURT:11d2\n\
F:IM_POIS | OPEN_DOOR | BASH_DOOR | MALE | RES_PLAS | IM_FIRE | NONLIVING |\n\
F:IM_FIRE | NO_CONF | NO_SLEEP | EVIL | DEMON | UNIQUE | RES_TELE | ATTR_MULTI\n\
S:1_IN_2 |\n\
S:BO_PLAS | S_MONSTERS\n\
D:Shaped like a lion with the head of a man, he tempts people to steal and \n\
D:is in charge of a good relationship among thieves, but later he brings them to the gallows. \n\
\n\
N:304:Andromalius, Great Earl of Hell\n\
G:U:u\n\
I:110:3:150d10:10:130:40\n\
W:20:3:0:900\n\
B:CLAW:HURT:11d2\n\
F:IM_POIS | OPEN_DOOR | BASH_DOOR | MALE | RES_PLAS | IM_FIRE | NONLIVING |\n\
F:IM_FIRE | NO_CONF | NO_SLEEP | EVIL | DEMON | UNIQUE | RES_TELE | ATTR_MULTI\n\
S:1_IN_2 |\n\
S:BO_PLAS | S_MONSTERS\n\
D:He is holding a big serpent in his hand. He can bring back both a thief and the stolen goods, \n\
D:punishes all thieves and other wicked people, and discovers hidden treasures, all evilness, \n\
D:and all dishonest dealing. \n\
\n\
#This guy is not supposed to die ;)\n\
#Can you tell ? ;)\n\
N:305:Hermes, God of Thieves\n\
G:U:w\n\
I:155:10:250d10:10:130:40\n\
W:20:4:0:2500\n\
B:TOUCH:EAT_GOLD:11d5\n\
B:TOUCH:EAT_GOLD:11d5\n\
B:TOUCH:EAT_ITEM:11d5\n\
B:TOUCH:EAT_ITEM:11d5\n\
F:OPEN_DOOR | BASH_DOOR | MALE | RES_PLAS | NONLIVING |\n\
F:NO_CONF | NO_SLEEP | UNIQUE | RES_TELE\n\
F:IM_COLD | IM_ELEC | IM_POIS | IM_FIRE | IM_ACID\n\
S:1_IN_2 |\n\
S:BO_ELEC | TELE_AWAY\n\
D:'One of many shifts, blandly cunning, a robber, a cattle driver, a bringer of dreams, \n\
D:a watcher by night, a thief at the gates, one who was soon to show forth wonderful deeds among the deathless gods.'' \n\
\n\
#Probably someone assumed that Hades is a dark place and that, having been there for a while, he became sensitive?\n\
N:306:Tantalus\n\
G:p:D\n\
I:120:3:18d15:20:40:30\n\
W:20:2:0:500\n\
B:HIT:EAT_ITEM:11d5\n\
B:HIT:EAT_ITEM:11d5\n\
F:MALE | \n\
F:FORCE_SLEEP | \n\
F:ONLY_ITEM | DROP_2D2 | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:UNIQUE | ATTR_MULTI | HURT_LITE\n\
S:1_IN_5 | \n\
S:HASTE | BLIND | CONF | DARKNESS | BO_FIRE | BO_COLD | MISSILE \n\
D:Tantalus is known for having been welcomed to Zeus' table in Olympus. \n\
D:There he stole nectar and ambrosia, brought them back to his people, \n\
D:and revealed the secrets of the gods. \n\
\n\
N:307:Wyvern\n\
G:d:G\n\
I:120:6:100d5:20:65:20\n\
W:20:1:0:360\n\
B:BITE:HURT:2d6\n\
B:STING:POISON:1d6\n\
B:STING:POISON:1d6\n\
F:FORCE_SLEEP | \n\
F:ONLY_GOLD | DROP_60 | DROP_90 | IM_POIS |\n\
F:OPEN_DOOR | BASH_DOOR | MOVE_BODY | DRAGON | \n\
F:ANIMAL | EVIL\n\
D:A fast-moving and deadly dragonian animal. Beware its poisonous sting! \n\
\n\
N:308:Shadow drake\n\
G:d:D\n\
I:110:3:20d10:25:50:30\n\
W:20:2:0:700\n\
B:BITE:COLD:1d6\n\
F:FORCE_SLEEP | RAND_25 | \n\
F:ONLY_ITEM | DROP_2D2 | \n\
F:TAKE_ITEM | INVISIBLE | OPEN_DOOR | BASH_DOOR | \n\
F:ANIMAL | EVIL | DRAGON | IM_COLD | HEAL_DARK | HURT_LITE\n\
S:1_IN_6 | \n\
S:HASTE | SLOW | CONF | SCARE | DARKNESS\n\
D:It is a dragon-like form wrapped in shadow. Glowing red eyes shine out in \n\
D:the dark. \n\
\n\
N:309:Vanni Fucci\n\
G:p:b\n\
I:135:6:90d10:20:80:20\n\
W:20:2:0:1111\n\
B:HIT:HURT:5d5\n\
B:HIT:POISON:5d5\n\
B:HIT:EAT_GOLD:5d5\n\
B:HIT:EAT_ITEM:5d5\n\
F:UNIQUE | MALE |\n\
F:FORCE_MAXHP |\n\
F:ESCORT | ESCORTS | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | ATTR_MULTI\n\
F:EVIL | RES_POIS\n\
D:Vanni Fucci, bastard son of Fuccio de Lazzeri, a Black Guelph in Pistoia, robbed the cathedral there in 1293. \n\
\n\
N:310:Cacus\n\
G:h:s\n\
I:120:6:90d10:20:80:20\n\
W:20:2:0:1111\n\
B:HIT:POISON:5d5\n\
B:HIT:POISON:5d5\n\
B:HIT:FIRE:5d5\n\
B:HIT:FIRE:5d5\n\
F:UNIQUE | MALE |FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | ATTR_MULTI\n\
F:EVIL | IM_POIS\n\
D:A mythological monster, son of Hephaestus. He was killed by Heracles for stealing part of the cattle the hero had taken from Geryon. \n\
D:As a guardian of the thieves he punishes Vanni Fucci. \n\
\n\
N:311:Cianfa Donati\n\
G:p:b\n\
I:135:6:90d10:20:80:20\n\
W:20:2:0:1500\n\
B:HIT:HURT:5d5\n\
B:HIT:POISON:5d5\n\
B:HIT:EAT_GOLD:5d5\n\
B:HIT:EAT_ITEM:5d5\n\
F:UNIQUE | MALE |\n\
F:FORCE_MAXHP |\n\
F:ESCORT | ESCORTS | RES_TELE | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | PASS_WALL\n\
F:EVIL | RES_POIS | RES_TELE | SMART | ATTR_MULTI\n\
S:1_IN_3  \n\
S:TELE_TO | ARROW_4\n\
D:Cianfa belonged to the Donati family of Florence. \n\
\n\
N:312:Agnello Brunelleschi\n\
G:p:b\n\
I:135:6:90d10:20:80:20\n\
W:20:2:0:1500\n\
B:HIT:HURT:5d5\n\
B:HIT:POISON:5d5\n\
B:HIT:EAT_GOLD:5d5\n\
B:HIT:EAT_ITEM:5d5\n\
F:UNIQUE | MALE |\n\
F:FORCE_MAXHP |\n\
F:ESCORT | ESCORTS | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | PASS_WALL\n\
F:EVIL | RES_POIS | RES_TELE | SMART | ATTR_MULTI\n\
S:1_IN_3  \n\
S:TELE_TO | ARROW_4\n\
D:Cianfa belonged to the Donati family of Florence. \n\
\n\
N:313:Buoso Abati\n\
G:p:b\n\
I:135:6:90d10:20:80:20\n\
W:20:2:0:1500\n\
B:HIT:HURT:5d5\n\
B:HIT:POISON:5d5\n\
B:HIT:EAT_GOLD:5d5\n\
B:HIT:EAT_ITEM:5d5\n\
F:UNIQUE | MALE |\n\
F:FORCE_MAXHP |\n\
F:ESCORT | ESCORTS | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | PASS_WALL\n\
F:EVIL | RES_POIS | RES_TELE | SMART | ATTR_MULTI\n\
S:1_IN_3  \n\
S:TELE_TO | ARROW_4\n\
D:Cianfa belonged to the Donati family of Florence. \n\
\n\
N:314:Puccio Sciancato\n\
G:p:b\n\
I:135:6:90d10:20:80:20\n\
W:20:2:0:1500\n\
B:HIT:HURT:5d5\n\
B:HIT:POISON:5d5\n\
B:HIT:EAT_GOLD:5d5\n\
B:HIT:EAT_ITEM:5d5\n\
F:UNIQUE | MALE |\n\
F:FORCE_MAXHP |\n\
F:ESCORT | ESCORTS | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | PASS_WALL\n\
F:EVIL | RES_POIS | RES_TELE | SMART | ATTR_MULTI\n\
S:1_IN_3  \n\
S:TELE_TO | ARROW_4\n\
D:Puccio Sciancato de Galigai was a Florentine Ghibelline. \n\
D:A brother in crime with the other Florentine thieves. \n\
\n\
######### Inferno   ####################\n\
##### Level 21 , Fraudulent advisors  ##\n\
##### Eight  Circle , 8th Ditch      ###\n\
\n\
N:315:Evil Counselor\n\
G:p:R\n\
I:110:2:23d10:20:40:50\n\
W:21:1:0:85\n\
B:HIT:HURT:1d6\n\
B:BITE:HURT:3d4\n\
F:MALE | DROP_60 | OPEN_DOOR | BASH_DOOR | EVIL | AURA_FIRE | DROP_90\n\
D:His words have clouded judgement and caused ruin. \n\
\n\
N:316:Counselor of Haman\n\
G:p:R\n\
I:110:2:23d10:20:40:50\n\
W:21:1:0:85\n\
B:HIT:HURT:1d6\n\
B:BITE:HURT:3d4\n\
F:MALE | DROP_60 | OPEN_DOOR | BASH_DOOR | EVIL | AURA_FIRE\n\
D:Haman had 365 counselors, but none of their advice was good. \n\
\n\
N:317:Eteocles\n\
G:p:o\n\
I:110:2:22d10:14:60:30\n\
W:21:2:0:200\n\
B:BITE:HURT:5d8\n\
F:WEIRD_MIND | BASH_DOOR | ATTR_MULTI | MALE | UNIQUE | AURA_FIRE\n\
D:Eteocles and Polynices, twin sons of Oedipus, quarreled over the Theban throne and provoked the war of the seven kings against Thebes. \n\
\n\
N:318:Polynices\n\
G:p:o\n\
I:110:2:22d10:14:60:30\n\
W:21:2:0:200\n\
B:BITE:HURT:5d8\n\
F:WEIRD_MIND | BASH_DOOR | ATTR_MULTI | MALE | UNIQUE | AURA_FIRE\n\
D:Eteocles and Polynices, twin sons of Oedipus, quarreled over the Theban throne and provoked the war of the seven kings against Thebes. \n\
\n\
N:319:Ulysses\n\
G:p:o\n\
I:110:2:22d10:14:60:30\n\
W:21:2:0:200\n\
B:BITE:HURT:5d8\n\
F:WEIRD_MIND | BASH_DOOR | ATTR_MULTI | MALE | UNIQUE | AURA_FIRE\n\
D:Ulysses and Diomede, the Greek heroes, are punished in the eighth pocket for evil counselors \n\
D:because of their plot to deceive the Trojans with the wooden horse in which they hid until, \n\
D:within the city, they opened the gates to their army which destroyed the city. \n\
\n\
N:320:Diomede\n\
G:p:o\n\
I:110:2:22d10:14:60:30\n\
W:21:2:0:200\n\
B:BITE:HURT:5d8\n\
F:WEIRD_MIND | BASH_DOOR | ATTR_MULTI | MALE | UNIQUE | AURA_FIRE\n\
D:Ulysses and Diomede, the Greek heroes, are punished in the eighth pocket for evil counselors \n\
D:because of their plot to deceive the Trojans with the wooden horse in which they hid until, \n\
D:within the city, they opened the gates to their army which destroyed the city. \n\
\n\
N:321:Guido da Montefeltro\n\
G:p:o\n\
I:110:2:22d10:14:60:30\n\
W:21:2:0:200\n\
B:BITE:HURT:5d8\n\
F:WEIRD_MIND | BASH_DOOR | ATTR_MULTI | MALE | UNIQUE | AURA_FIRE\n\
D:Guido entered the Franciscan order in 1296 and became an adviser to Boniface VIII \n\
D:who under the pretense of amnesty for the Colonna family razed their stronghold of Penestrino \n\
\n\
N:322:Boniface VIII\n\
G:p:o\n\
I:110:2:22d10:14:60:30\n\
W:21:2:0:200\n\
B:BITE:HURT:5d8\n\
F:WEIRD_MIND | BASH_DOOR | ATTR_MULTI | MALE | UNIQUE | AURA_FIRE\n\
D:'Had not that high priest (evil overtake him!) \n\
D:Caused me to backslide into earlier crimes:' \n\
D:Boniface VIII, under the pretense of amnesty for the Colonna family razed their stronghold of Penestrino in 1298. \n\
\n\
N:323:Edmund Beaufort\n\
G:p:o\n\
I:110:2:22d10:14:60:30\n\
W:21:2:0:200\n\
B:BITE:HURT:5d8\n\
F:WEIRD_MIND | BASH_DOOR | ATTR_MULTI | MALE | UNIQUE | AURA_FIRE\n\
D:Edmund Beaufort, 1st Duke of Somerset (1406 – May 22, 1455) was the counselor of Henry VI. \n\
D:His romance with Katherine of France, his failures on Hundred Years' War and his involvement \n\
D:in the Wars of the Roses have earned him a warm spot in the Malebolge. \n\
\n\
N:324:Haman the Agatite\n\
G:p:o\n\
I:110:2:22d10:14:60:30\n\
W:21:2:0:200\n\
B:BITE:HURT:5d8\n\
F:WEIRD_MIND | BASH_DOOR | ATTR_MULTI | MALE | UNIQUE | AURA_FIRE\n\
D:He was a 6th Century BCE Persian noble and vizier of the empire under Persian King Ahasuerus. \n\
D:Haman attempted to convince Ahasuerus to order the killing of all the Jews of the lands he ruled. \n\
D:The plot was foiled by Queen Esther, the king's recent wife, who is herself a Jew. \n\
D:Haman and his 10 sons were hung from the gallows destined for the Jews. \n\
\n\
N:325:Zeresh\n\
G:p:o\n\
I:110:2:22d10:14:60:30\n\
W:21:2:0:200\n\
B:BITE:HURT:5d8\n\
F:WEIRD_MIND | BASH_DOOR | ATTR_MULTI | MALE | UNIQUE | AURA_FIRE\n\
D:The wife of Haman, she induced Haman to build gallows for the Jews, \n\
D:assuring him that this was the only way in which he would be able to prevail over his enemy. \n\
\n\
N:326:Guido Guidi\n\
G:p:o\n\
I:110:2:22d10:14:60:30\n\
W:21:2:0:200\n\
B:BITE:HURT:5d8\n\
F:WEIRD_MIND | BASH_DOOR | ATTR_MULTI | MALE | UNIQUE | AURA_FIRE\n\
D:Guido convinced Master Adam to counterfeit florins for their house, \n\
D:condemning him to an eternity shaped as a lute, lusting for water. \n\
\n\
N:327:Alexander Guidi\n\
G:p:o\n\
I:110:2:22d10:14:60:30\n\
W:21:2:0:200\n\
B:BITE:HURT:5d8\n\
F:WEIRD_MIND | BASH_DOOR | ATTR_MULTI | MALE | UNIQUE | AURA_FIRE\n\
D:Guido convinced Master Adam to counterfeit florins for their house, \n\
D:condemning him to an eternity shaped as a lute, lusting for water. \n\
\n\
#aka Achitophel\n\
N:328:Ahitophel, Brother of Impiety\n\
G:p:o\n\
I:110:2:22d10:14:60:30\n\
W:21:2:0:200\n\
B:BITE:HURT:5d8\n\
F:WEIRD_MIND | BASH_DOOR | ATTR_MULTI | MALE | UNIQUE | AURA_FIRE\n\
D:Ahitophel was greatly renowned for his sagacity, and a counselor of King David. \n\
D:But at the time of Absalom's revolt he deserted David and supported Absalom. \n\
\n\
N:329:Fire vortex\n\
G:v:r\n\
I:110:1:9d9:100:30:0\n\
W:21:1:0:100\n\
B:ENGULF:FIRE:3d3\n\
F:FORCE_SLEEP | RAND_50 | \n\
F:EMPTY_MIND | BASH_DOOR | POWERFUL | AURA_FIRE | HURT_COLD\n\
F:HEAL_FIRE | NO_FEAR | NO_CONF | NO_SLEEP | NONLIVING\n\
S:1_IN_6 | \n\
S:BR_FIRE\n\
D:A whirling maelstrom of fire. \n\
\n\
N:330:Flame vortex\n\
G:v:R\n\
I:120:2:32d10:100:40:0\n\
W:21:3:0:800\n\
B:ENGULF:FIRE:8d8\n\
F:FORCE_SLEEP | \n\
F:RAND_50 | RAND_25 | RES_PLAS | AURA_FIRE | AURA_ELEC | HURT_COLD\n\
F:EMPTY_MIND | BASH_DOOR | POWERFUL | \n\
F:HEAL_FIRE | HEAL_ELEC\n\
F:NO_CONF | NO_SLEEP | NO_FEAR | NONLIVING\n\
S:1_IN_6 | \n\
S:BR_PLAS\n\
D:A whirlpool of intense flame, charring the stones at your feet. \n\
\n\
######### Inferno   ####################\n\
##### Level 22 , Sowers of Discord    ##\n\
##### Eight  Circle , 9th Ditch      ###\n\
\n\
N:331:Fra Dolcino\n\
G:p:B\n\
I:110:2:25d12:16:60:30\n\
W:22:3:0:270\n\
B:BITE:HURT:5d4\n\
B:WAIL:TERRIFY:5d6\n\
F:WEIRD_MIND | BASH_DOOR | UNIQUE | REBORN\n\
D:Fra Dolcino in 1300 headed the Apostolic Brothers, an outlawed religious sect \n\
D:that was forcibly suppressed; he was burned at the stake in 1307. \n\
\n\
N:332:Pier da Medicina\n\
G:p:B\n\
I:110:2:25d12:16:60:30\n\
W:22:3:0:270\n\
B:BITE:HURT:5d4\n\
B:WAIL:TERRIFY:5d6\n\
F:WEIRD_MIND | BASH_DOOR | UNIQUE | REBORN\n\
D:Pier da Medicina, driven from Romagna in 1287, intrigued among its rulers \n\
D:to turn them against themselves. \n\
\n\
N:333:Malatestino of Rimini\n\
G:p:B\n\
I:110:2:25d12:16:60:30\n\
W:22:3:0:270\n\
B:BITE:HURT:5d4\n\
B:WAIL:TERRIFY:5d6\n\
F:WEIRD_MIND | BASH_DOOR | UNIQUE | REBORN\n\
D:To acquire Fano for himself, invited Angiolello da Carignano and Guido del Cassero, \n\
D:two of the town’s leaders, to meet him at La Cattolica, a cape between the two towns, \n\
D:and had them drowned. \n\
\n\
N:334:Curio\n\
G:p:B\n\
I:110:2:25d12:16:60:30\n\
W:22:3:0:270\n\
B:BITE:HURT:5d4\n\
B:WAIL:TERRIFY:5d6\n\
F:WEIRD_MIND | BASH_DOOR | UNIQUE | REBORN\n\
D:Curio is said by the Roman poet Lucan to have urged Caesar to cross the Rubicon, \n\
D:declaring war on the Republic in 49 B.C. \n\
\n\
N:335:Mosca dei Lambert\n\
G:p:B\n\
I:110:2:25d12:16:60:30\n\
W:22:3:0:270\n\
B:BITE:HURT:5d4\n\
B:WAIL:TERRIFY:5d6\n\
F:WEIRD_MIND | BASH_DOOR | UNIQUE | REBORN\n\
D:Mosca dei Lamberti suggested that one of the Buondelmonti be murdered rather than \n\
D:beaten; the act resulted in the strife between Ghibellines and Guelphs. \n\
\n\
N:336:Bertran de Born\n\
G:p:B\n\
I:110:2:25d12:16:60:30\n\
W:22:3:0:270\n\
B:BITE:HURT:5d4\n\
B:WAIL:TERRIFY:5d6\n\
F:WEIRD_MIND | BASH_DOOR | UNIQUE | REBORN\n\
D:Bertran de Born (1140-1215), a knight and troubadour, who was believed to have \n\
D:instigated a quarrel between Henry II of England and his son. He now walks around \n\
D:carrying his head in his hands. \n\
\n\
N:337:Geri del Bello\n\
G:p:B\n\
I:110:2:25d12:16:60:30\n\
W:22:3:0:270\n\
B:BITE:HURT:5d4\n\
B:WAIL:TERRIFY:5d6\n\
F:WEIRD_MIND | BASH_DOOR | UNIQUE | REBORN\n\
D:Geri del Bello was a first cousin of Dante’s father. Since he was slain in a feud, \n\
D:the family is called to revenge his death.. \n\
\n\
N:338:Sower of Discord\n\
G:p:v\n\
I:155:5:15d11:20:40:40\n\
W:22:1:0:60\n\
B:HIT:HURT:3d5\n\
F:MALE | DROP_1D2 | TAKE_ITEM | OPEN_DOOR | BASH_DOOR | EVIL\n\
D:Someone has been holding up He Who Carries the Sword, which \n\
D:means bad news for you. \n\
\n\
######### Inferno   ####################\n\
##### Level 23 , Falsifiers           ##\n\
##### Eight  Circle , 10th Ditch     ###\n\
\n\
N:339:Alchemist\n\
G:p:B\n\
I:120:2:7d10:20:16:20\n\
W:23:1:0:75\n\
B:HIT:HURT:1d6\n\
F:MALE | FORCE_SLEEP | \n\
F:ONLY_ITEM | DROP_1D2 | \n\
F:OPEN_DOOR | BASH_DOOR | EVIL | \n\
S:1_IN_5 | \n\
S:CONF | MISSILE | BO_MANA\n\
D:His skin is marked with pocks, if he's not trying to kill you, \n\
D:he's scratching his skin. \n\
\n\
N:340:Perjurer\n\
G:p:b\n\
I:120:2:7d10:20:16:20\n\
W:23:1:0:35\n\
B:HIT:POISON:1d6\n\
B:HIT:FIRE:1d6\n\
B:HIT:POISON:1d6\n\
B:HIT:FIRE:1d6\n\
F:MALE | FORCE_SLEEP | NEVER_MOVE\n\
F:ONLY_ITEM | DROP_1D2 | \n\
F:OPEN_DOOR | BASH_DOOR | EVIL | \n\
D:Punished for his grave lies he must remain immobile with burning fever. \n\
\n\
N:341:Counterfeiter\n\
G:p:y\n\
I:120:2:7d10:20:16:20\n\
W:23:1:0:35\n\
B:HIT:HURT:1d6\n\
F:MALE | FORCE_SLEEP | NEVER_MOVE\n\
F:ONLY_ITEM | DROP_1D2 | \n\
F:OPEN_DOOR | BASH_DOOR | EVIL | \n\
D:Punsihed for his sins, he must remain immobile in the shape of a broken musical instrument. \n\
\n\
N:342:Impersonator\n\
G:@:w\n\
I:120:2:7d10:20:16:20\n\
W:23:3:0:75\n\
B:HIT:HURT:1d6\n\
F:MALE | FORCE_SLEEP | \n\
F:ONLY_ITEM | DROP_1D2 | PASS_WALL\n\
F:OPEN_DOOR | BASH_DOOR | EVIL | \n\
S:1_IN_5 | \n\
S:CONF | MISSILE | BO_MANA\n\
D:Barely a shade, he seems to resemble someone you know. \n\
\n\
#And a such the pukelman lives on ;')\n\
N:343:Griffolino da Arezzo\n\
G:p:o\n\
I:110:2:80d12:12:80:10\n\
W:23:3:0:600\n\
B:HIT:HURT:3d6\n\
F:FORCE_SLEEP | UNIQUE | DROP_3D2 | DROP_GOOD\n\
F:OPEN_DOOR | BASH_DOOR | MALE\n\
F:RES_FIRE | RES_COLD | RES_ELEC | RES_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_4 | \n\
S:SLOW | CONF | BO_ACID\n\
D:Griffolino da Arezzo was said to have duped Alberto da Siena into paying him for flying lessons. \n\
D:The bishop had Griffolino burned at the stake for black magic. \n\
\n\
N:344:Master Adam\n\
G:p:o\n\
I:110:2:80d12:12:80:10\n\
W:23:3:0:600\n\
B:HIT:HURT:3d6\n\
F:FORCE_SLEEP | UNIQUE | DROP_3D2 | DROP_4D2 | DROP_2D2 | ONLY_GOLD\n\
F:OPEN_DOOR | BASH_DOOR | MALE\n\
F:RES_FIRE | RES_COLD | RES_ELEC | RES_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR | ATTR_MULTI\n\
S:1_IN_4 | \n\
S:SLOW | CONF | BO_ACID\n\
D: Master Adam was in the employ of the Conti Guidi of Romena, and coined debased florins for them. \n\
D:He was burned at Florence in 1281. \n\
\n\
N:345:Capocchio\n\
G:p:b\n\
I:110:2:80d12:12:80:10\n\
W:23:3:0:600\n\
B:HIT:HURT:3d6\n\
F:FORCE_SLEEP | UNIQUE | DROP_3D2 | DROP_4D2 | DROP_2D2 | ONLY_GOLD\n\
F:OPEN_DOOR | BASH_DOOR | MALE\n\
F:RES_FIRE | RES_COLD | RES_ELEC | RES_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR | ATTR_MULTI\n\
S:1_IN_4 | \n\
S:SLOW | CONF | BO_ACID\n\
D:Capocchio, who was burned at Siena in 1293 for practicing alchemy. \n\
\n\
N:346:Gianni Schicchi\n\
G:@:g\n\
I:110:2:80d12:12:80:10\n\
W:23:3:0:600\n\
B:HIT:HURT:3d6\n\
F:FORCE_SLEEP | UNIQUE | DROP_3D2 | DROP_GREAT\n\
F:OPEN_DOOR | BASH_DOOR | MALE\n\
F:RES_FIRE | RES_COLD | RES_ELEC | RES_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR | ATTR_MULTI\n\
S:1_IN_4 | \n\
S:SLOW | CONF | BO_ACID\n\
D:Gianni Schicchi, a mimic and member of the Cavalcanti clan, posed \n\
D:as the dying Buoso Donati — who was already dead — and made a new \n\
D:last will to benefit the heir and himself \n\
\n\
N:347:Myrrha\n\
G:@:v\n\
I:110:2:80d12:12:80:10\n\
W:23:3:0:600\n\
B:HIT:HURT:3d6\n\
F:FORCE_SLEEP | UNIQUE | DROP_3D2 | DROP_GREAT \n\
F:OPEN_DOOR | BASH_DOOR | FEMALE\n\
F:RES_FIRE | RES_COLD | RES_ELEC | RES_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR | ATTR_MULTI\n\
S:1_IN_4 | \n\
S:SLOW | CONF | BO_ACID\n\
D:Myrrha is with the falsifiers of the tenth pocket for tricking her father, King Cinyras of Cyprus, into sleeping with her \n\
\n\
N:348:Zuleika\n\
G:p:G\n\
I:110:2:80d12:12:80:10\n\
W:23:3:0:600\n\
B:HIT:HURT:3d6\n\
F:FORCE_SLEEP | UNIQUE | DROP_1D2 | DROP_GREAT \n\
F:OPEN_DOOR | BASH_DOOR | FEMALE\n\
F:RES_FIRE | RES_COLD | RES_ELEC | RES_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR | \n\
S:1_IN_4 | \n\
S:SLOW | CONF | BO_ACID\n\
D:'She is the wife who falsely accused Joseph' (Genesis 39:6-23) \n\
\n\
N:349:Sinon\n\
G:p:G\n\
I:110:2:80d12:12:80:10\n\
W:23:3:0:600\n\
B:HIT:HURT:3d6\n\
F:FORCE_SLEEP | UNIQUE | DROP_1D2 | DROP_GREAT \n\
F:OPEN_DOOR | BASH_DOOR | FEMALE\n\
F:RES_FIRE | RES_COLD | RES_ELEC | RES_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR | \n\
S:1_IN_4 | \n\
S:SLOW | CONF | BO_ACID\n\
D:Sinon, a Greek, convinced the Trojans to accept the horse as a gift (Aeneid II, 57-194) \n\
\n\
######### Inferno   ####################\n\
##### Level 24 , Giants               ##\n\
##### Ninth Circle , Entry           ###\n\
\n\
N:350:Nimrod\n\
G:P:u\n\
I:140:5:11d100:20:70:50\n\
W:24:2:0:2000\n\
B:HIT:HURT:5d5\n\
B:BITE:HURT:2d10\n\
B:HIT:COLD:5d5\n\
B:BITE:COLD:2d10\n\
F:UNIQUE | MALE | NEVER_MOVE | NEUTRAL | REBORN\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GREAT | ATTR_MULTI\n\
F:EVIL | GIANT | RES_POIS | SMART | RES_FIRE | RES_COLD | RES_DARK | RES_LITE \n\
S:1_IN_2 | \n\
S:TELE_TO | BO_ICEE | BA_CHAO\n\
D:Nimrod is commonly credited with building the Tower of Babel (Genesis 10: 8-10, 11:1-9), which is the cause for the different languages among people. \n\
\n\
N:351:Otus\n\
G:P:u\n\
I:140:5:11d100:20:70:50\n\
W:24:2:0:2000\n\
B:HIT:HURT:5d5\n\
B:BITE:HURT:2d10\n\
B:HIT:ELEC:5d5\n\
B:BITE:ELEC:2d10\n\
F:UNIQUE | MALE | NEVER_MOVE | NEUTRAL | REBORN\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GREAT | ATTR_MULTI\n\
F:EVIL | GIANT | RES_POIS | SMART | RES_FIRE | RES_COLD | RES_DARK | RES_LITE\n\
S:1_IN_8 | \n\
S:TELE_TO | BO_ICEE | BA_CHAO\n\
D:Otus, with his brother Ephialtes, attempted to overthrow the gods on Mount Olympus; he piled up mountains to ascend there. \n\
\n\
N:352:Ephialtes\n\
G:P:u\n\
I:140:5:11d100:20:70:50\n\
W:24:2:0:2000\n\
B:HIT:HURT:5d5\n\
B:BITE:HURT:2d10\n\
B:HIT:ELEC:5d5\n\
B:BITE:ELEC:2d10\n\
F:UNIQUE | MALE | NEVER_MOVE | NEUTRAL | REBORN\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GREAT | ATTR_MULTI\n\
F:EVIL | GIANT | RES_POIS | SMART | RES_FIRE | RES_COLD | RES_DARK | RES_LITE\n\
S:1_IN_2 | \n\
S:TELE_TO | BO_ICEE | BA_CHAO\n\
D:Ephialtes, with his brother Otus, attempted to overthrow the gods on Mount Olympus; he piled up mountains to ascend there. \n\
\n\
N:353:Briareus\n\
G:P:u\n\
I:140:5:11d100:20:70:50\n\
W:24:2:0:2000\n\
B:HIT:HURT:5d5\n\
B:BITE:HURT:2d10\n\
B:HIT:ELEC:5d5\n\
B:BITE:ELEC:2d10\n\
F:UNIQUE | MALE | NEVER_MOVE | NEUTRAL | REBORN\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GREAT | ATTR_MULTI\n\
F:EVIL | GIANT | RES_POIS | SMART | RES_FIRE | RES_COLD | RES_DARK | RES_LITE\n\
S:1_IN_2 | \n\
S:TELE_TO | BO_ICEE | BA_CHAO\n\
D:Briareus and Antaeus were sons of Tellus (the Earth). \n\
\n\
N:354:Briareus\n\
G:P:u\n\
I:140:5:11d100:20:70:50\n\
W:24:2:0:2000\n\
B:HIT:HURT:5d5\n\
B:BITE:HURT:2d10\n\
B:HIT:ELEC:5d5\n\
B:BITE:ELEC:2d10\n\
F:UNIQUE | MALE | NEVER_MOVE | NEUTRAL | REBORN\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GREAT | ATTR_MULTI\n\
F:EVIL | GIANT | RES_POIS | SMART | RES_FIRE | RES_COLD | RES_DARK | RES_LITE\n\
S:1_IN_2 | \n\
S:TELE_TO | BO_ICEE | BA_CHAO\n\
D:Briareus and Antaeus were sons of Tellus (the Earth). \n\
\n\
N:355:Tityus\n\
G:P:u\n\
I:140:5:11d100:20:70:50\n\
W:24:2:0:2000\n\
B:HIT:HURT:5d5\n\
B:BITE:HURT:2d10\n\
B:HIT:ELEC:5d5\n\
B:BITE:ELEC:2d10\n\
F:UNIQUE | MALE | NEVER_MOVE | NEUTRAL | REBORN\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GREAT | ATTR_MULTI\n\
F:EVIL | GIANT | RES_POIS | SMART | RES_FIRE | RES_COLD | RES_DARK | RES_LITE\n\
S:1_IN_2 | \n\
S:TELE_TO | BO_ICEE | BA_CHAO\n\
D:Tityas attempted to rape Leto and was slain by Apollo and Artemis. \n\
\n\
N:356:Typhon\n\
G:P:u\n\
I:140:5:11d100:20:70:50\n\
W:24:2:0:2000\n\
B:HIT:HURT:5d5\n\
B:BITE:HURT:2d10\n\
B:HIT:ELEC:5d5\n\
B:BITE:ELEC:2d10\n\
F:UNIQUE | MALE | NEVER_MOVE | NEUTRAL | REBORN\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GREAT | ATTR_MULTI\n\
F:EVIL | GIANT | RES_POIS | SMART | RES_FIRE | RES_COLD | RES_DARK | RES_LITE\n\
S:1_IN_2 | \n\
S:TELE_TO | BO_ICEE | BA_CHAO\n\
D:Typhon lead the Titans when they attacked and killed Dionysus. \n\
\n\
######### Inferno   ####################\n\
##### Level 25 , Traitors             ##\n\
##### Ninth Circle , Entry           ###\n\
##############  Caina  #################\n\
\n\
N:357:Kinslayer\n\
G:p:w\n\
I:110:3:40d10:20:65:70\n\
W:25:1:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 |  \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Escaped from the Cocytus, he has gained some fearsome powers. \n\
\n\
N:358:Alessandro degli Alberti\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:2:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Alessandro and Napoleone degli Alberti, sons of Alberto of Mangona, quarreled over their patrimony — including castles in the Val di Bisenzio — and killed \n\
each other. \n\
\n\
N:359:Napoleone degli Alberti\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:2:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Alessandro and Napoleone degli Alberti, sons of Alberto of Mangona, quarreled over their patrimony — including castles in the Val di Bisenzio — and killed \n\
each other. \n\
\n\
N:360:Mordred\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:2:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Modred betrayed his uncle King Arthur who lanced him through so that sunlight shone from the hole. \n\
\n\
N:361:Focaccia de’ Cancellieri \n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:2:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Focaccia de’ Cancellieri of Pistoia is a Tuscan kinslayer that met Dante 300 years ago. \n\
\n\
N:362:Sassol Mascheroni\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:2:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Sassol Mascheroni is a Tuscan kinslayer that met Dante 300 years ago. \n\
\n\
N:363:Camiscion de’ Pazzi\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:2:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Camiscion de’ Pazzis a Tuscan kinslayer that met Dante 300 years ago. \n\
\n\
N:364:Bocca degli Abbati\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:2:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Bocca degli Abbati, posing as a Florentine Guelph, but actually a Ghibelline, \n\
D:cut off the hand of the Guelph standard-bearer at the battle of Montaperti in \n\
D:1260 and helped cause that party’s defeat. \n\
\n\
##########################  Antenora  ##########################\n\
\n\
N:365:Betrayer of the Crown\n\
G:p:w\n\
I:110:3:40d10:20:65:70\n\
W:25:1:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 |  \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Escaped from the Cocytus, he has gained some fearsome powers. \n\
\n\
N:366:Betrayer of the Faith\n\
G:p:w\n\
I:110:3:40d10:20:65:70\n\
W:25:1:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 |  \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Escaped from the Cocytus, he has gained some fearsome powers. \n\
\n\
N:367:Betrayer of the Clan\n\
G:p:w\n\
I:110:3:40d10:20:65:70\n\
W:25:1:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 |  \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Escaped from the Cocytus, he has gained some fearsome powers. \n\
\n\
N:368:Antenor of Troy\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:2:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Antenor of Troy betrayed his city to the Greeks. \n\
\n\
N:369:Pope Clement V\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:2:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Pope Clement V issued an edict officially dissolving the Templars Order giving way to \n\
D:King Philip IV to brutally put an end to the Templars. \n\
\n\
N:370:Philip IV\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:2:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:King Philip IV has betrayed the Templars in order to support his wars. \n\
\n\
N:371:Buoso da Duera\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:2:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Buoso da Duera, a Ghibelline leader, was bribed by the French to betray his side. \n\
\n\
N:372:Tesauro dei Beccheria\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:2:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Tesauro dei Beccheria of Pavia, a papal legate in Tuscany, secretly worked for the Ghibellines; he was beheaded in 1258. \n\
\n\
N:373:Gianni de’ Soldanier\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:2:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Gianni de’ Soldanier, a Florentine Ghibelline, joined up with the Guelphs. \n\
\n\
N:374:Ganelon\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:2:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Ganelon betrayed Roland and the rear-guard to the Moors. \n\
\n\
N:375:Tebaldello de’ Zambrasi\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:1:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Tebaldello de’ Zambrasi of Faenza turned over his city to the Guelphs of Bologna in 1280. \n\
\n\
N:376:Count Ugolino\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:1:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Ugolino della Gherardesca, Count of Donoratico and a Pisan Guelph, plotted with Archbishop \n\
D:Ruggieri degli Ubaldini, a Ghibelline chief, against his own party in 1288. Then Ruggieri \n\
D:treacherously imprisoned him, his sons and grandsons. \n\
\n\
N:377:Archbishop Ruggieri\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:1:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Ugolino della Gherardesca, Count of Donoratico and a Pisan Guelph, plotted with Archbishop \n\
D:Ruggieri degli Ubaldini, a Ghibelline chief, against his own party in 1288. Then Ruggieri \n\
D:treacherously imprisoned him, his sons and grandsons. \n\
\n\
##########################  Ptolomea  ##########################\n\
\n\
N:378:Captain Ptolemy\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:1:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:He invited Simon Maccabaeus and his sons to a banquet and there killed them. \n\
\n\
N:379:Ptolemy XII\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:1:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:After his defeat in the Battle of Pharsalus Pompey sought asylum in Egypt. Initially Pothinus pretended \n\
D:to have accepted his request but on September 29, 48 BC, Pothinus had the general beheaded—hoping to win \n\
D:favor with Julius Caesar, who had defeated Pompey. \n\
\n\
N:380:Brother Alberigo\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:1:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Brother Alberigo, a Jovial Friar, murdered his brother Manfred and his nephews in 1285 at a banquet to \n\
D:which he invited them in Faenza. The signal for the murder was the call, 'Bring in the fruit.' \n\
\n\
N:381:Branca d’Oria\n\
G:p:B\n\
I:110:3:40d10:20:65:70\n\
W:25:1:0:1000\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_2D2 | DROP_GREAT | UNIQUE\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_10 |\n\
S:BR_COLD\n\
D:Branca d’Oria, a Ghibelline of Genoa, also invited a kinsman to a banquet, \n\
D:his father-in-law Michel Zanche, in 1275, to have him butchered. \n\
\n\
##### Level 26 #####\n\
\n\
N:382:Satan\n\
G:U:B\n\
I:155:10:200d150:111:175:0\n\
W:26:1:0:95\n\
B:CRUSH:UN_BONUS:22d10\n\
B:CRUSH:UN_BONUS:22d10\n\
B:CRUSH:UN_BONUS:22d10\n\
B:CRUSH:UN_BONUS:22d10\n\
F:UNIQUE | MALE\n\
F:EVIL | IM_POIS | IM_COLD | NO_CONF | NO_SLEEP\n\
F:REFLECTING | AURA_FIRE | AURA_ELEC | FALLEN_ANGEL\n\
F:ONLY_ITEM | DROP_1D2 | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_60\n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR | NO_STUN | RES_TELE\n\
F:DROP_GREAT | RES_NETH | NEVER_MOVE | FORCE_SLEEP\n\
F:SMART | POWERFUL |\n\
S:1_IN_1\n\
S:BR_DISI\n\
D:His name means adversary in Hebrew, and he is the angel who occupies that role in the Old Testament. \n\
D:One of God's angels of destruction, it is Satan who rains misfortune on the head of poor Job. In the \n\
D:New Testament, Satan becomes synonymous with the Devil. It is Satan who tempts Eve through the serpent \n\
D:and plagues Jesus in the desert. Often confused with Lucifer, Satan also was a Seraphim angel bedecked \n\
D:with 12 wings, twice the amount customarily allotted to an angel of that rank. He tempts humans to anger, \n\
D:in addition to every other sin imaginable. Satan was overthrown by the archangel Uriel. \n\
\n\
##### Level 27 #####\n\
\n\
N:383:Arch-vile\n\
G:u:W\n\
I:130:3:11d11:100:30:0\n\
W:21:1:0:300\n\
B:CLAW:HURT:3d9\n\
B:CLAW:HURT:3d9\n\
F:RAND_50 | EVIL | DEMON | FORCE_SLEEP | FORCE_MAXHP |\n\
F:OPEN_DOOR | BASH_DOOR | POWERFUL | COLD_BLOOD |\n\
F:IM_FIRE | RES_NETH |  NO_CONF | NO_SLEEP | NONLIVING | NO_STUN | \n\
S:1_IN_3 |\n\
S:BA_FIRE |\n\
D:A pale, corpse-like lesser demon, who moves very fast and spawns evil \n\
D:everywhere. \n\
\n\
N:384:Ghoul\n\
G:z:U\n\
I:110:3:15d9:30:30:20\n\
W:27:1:0:95\n\
B:CLAW:POISON:1d4\n\
B:BITE:PARALYZE:1d5\n\
F:DROP_60 | OPEN_DOOR | BASH_DOOR |\n\
F:EVIL | UNDEAD | FRIENDS | IM_POIS | IM_COLD | NO_CONF | NO_SLEEP\n\
F:COLD_BLOOD | HURT_LITE\n\
S:1_IN_9\n\
S:SCARE | HOLD\n\
D:He has been taken here by a demon with promises of power, \n\
D:now flesh is falling off in chunks from this decaying abomination. \n\
\n\
N:385:Ephippas\n\
G:U:u\n\
I:110:5:85d11:20:40:40\n\
W:27:4:0:500\n\
B:GAZE:TERRIFY\n\
B:HIT:POISON:6d6\n\
B:CLAW:LOSE_CON\n\
B:CLAW:LOSE_CON\n\
F:UNIQUE | MALE | DEMON | EVIL | ESCORTS | ESCORT\n\
F:FORCE_MAXHP | COLD_BLOOD | IM_POIS | IM_COLD | NO_FEAR |\n\
F:ONLY_ITEM | DROP_90 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | RES_TELE\n\
S:1_IN_5 |\n\
S:TRAPS | CAUSE_3 | DARKNESS | S_DEMON | S_DEVIL | SCARE | SLOW\n\
S:S_KIN\n\
D:Demon of the Arabian Wind, he can summon the son of Beelzebub from the depths of the sea. \n\
\n\
N:386:White wraith\n\
G:W:w\n\
I:110:4:15d8:20:40:10\n\
W:27:1:0:175\n\
B:HIT:HURT:1d6\n\
B:TOUCH:EXP_20\n\
F:FORCE_SLEEP | \n\
F:DROP_1D2 | \n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | HURT_LITE | NO_CONF | NO_SLEEP\n\
S:1_IN_8 | \n\
S:SCARE | CAUSE_2 | DARKNESS\n\
D:It is a tangible but ghostly form made of white fog. \n\
\n\
N:387:Lilitu\n\
G:h:v\n\
I:110:3:30d10:30:60:255\n\
W:27:6:0:220\n\
B:HIT:HURT:3d4\n\
F:FORCE_MAXHP | FORCE_SLEEP | NO_FEAR |\n\
F:ONLY_ITEM | DROP_2D2 | DEVIL | FEMALE |\n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | \n\
F:IM_ACID | IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_3 | \n\
S:BLIND | CONF | SCARE | FORGET | MISSILE\n\
D:A daughter of Lilith, she'll blow your mind. \n\
\n\
N:388:Ornias\n\
G:U:y\n\
I:120:6:80d11:20:80:20\n\
W:27:4:0:1000\n\
B:HIT:UN_BONUS:3d12\n\
B:TOUCH:EAT_GOLD\n\
F:UNIQUE | MALE | \n\
F:FORCE_MAXHP | FORCE_SLEEP | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | INVISIBLE |\n\
F:EVIL | IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | \n\
F:NO_CONF | NO_SLEEP | RES_DISE | RES_TELE\n\
S:1_IN_6 | \n\
S:HEAL | SCARE | BO_ACID | BA_ACID | TPORT | S_MONSTER \n\
D:Ornias (trans. 'pesky') is the first demon mentioned in the Testament of Solomon. \n\
D:While he goes in a trance he undergoes three transformations. Sometimes he is a man \n\
D:who craves the bodies of effeminate boys and when he touches them, they suffer great pain. \n\
D:Sometimes he becomes a creature with wings (flying) up to the heavenly regions. Finally, he can assume the appearance of a lion. \n\
\n\
N:389:Hellblade\n\
G:|:v\n\
I:120:2:13d13:20:40:20\n\
W:27:2:0:130\n\
B:HIT:EXP_20:1d8\n\
F:CHAR_MULTI | EVIL | IM_POIS | IM_COLD | \n\
F:FORCE_SLEEP | FORCE_MAXHP |\n\
F:COLD_BLOOD | BASH_DOOR | NONLIVING |\n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
D:A deadly blade of chaos, moving of its own volition. \n\
\n\
N:390:Bursting beetle\n\
G:K:r\n\
I:140:2:13d8:14:45:30\n\
W:27:7:0:300\n\
B:BITE:FIRE:3d4\n\
B:SPIT:FIRE:4d5\n\
B:BITE:FIRE:3d4\n\
B:SPIT:FIRE:4d5\n\
F:WEIRD_MIND | BASH_DOOR | FRIENDS\n\
F:ANIMAL | IM_FIRE | AURA_FIRE | MULTIPLY\n\
D:It is a giant beetle wreathed in flames. \n\
\n\
N:391:Headless\n\
G:H:u\n\
I:110:2:21d12:20:50:40\n\
W:27:1:0:175\n\
B:HIT:HURT:1d8\n\
F:FRIENDS | DROP_60 | OPEN_DOOR | BASH_DOOR\n\
F:EVIL \n\
S:1_IN_6\n\
S:SCARE\n\
D:Headless humanoid abominations created by a magical mutation. \n\
\n\
N:392:Ogre mage\n\
G:O:o\n\
I:110:5:30d12:20:40:30\n\
W:27:2:0:300\n\
B:HIT:HURT:3d8\n\
F:FORCE_SLEEP | \n\
F:DROP_1D2 | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | GIANT\n\
S:1_IN_4 | \n\
S:HEAL | HOLD | TRAPS | BA_COLD | \n\
S:S_MONSTER\n\
D:A hideous ogre wrapped in black sorcerous robes. \n\
\n\
#N:393:Grendel\n\
#G:O:g\n\
#I:120:5:15d100:20:100:20\n\
#W:27:2:0:1500\n\
#B:HIT:HURT:6d6\n\
#F:UNIQUE | MALE |\n\
#F:FORCE_MAXHP | \n\
#F:ESCORT | \n\
#F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
#F:OPEN_DOOR | BASH_DOOR | \n\
#F:EVIL | GIANT | IM_POIS\n\
#D:An ogre renowned for acts of surpassing cruelty. \n\
\n\
N:393:Vampire\n\
G:V:W\n\
I:110:4:25d12:20:45:10\n\
W:27:1:0:175\n\
B:HIT:HURT:1d6\n\
B:BITE:EXP_20:1d4\n\
F:FORCE_SLEEP | \n\
F:COLD_BLOOD | DROP_60 | DROP_1D2 | \n\
F:OPEN_DOOR | BASH_DOOR | REGENERATE | \n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | HURT_LITE | NO_CONF | NO_SLEEP\n\
S:1_IN_9 | \n\
S:TELE_TO | HOLD | SCARE | CAUSE_2 | MIND_BLAST | FORGET | DARKNESS\n\
D:It is a humanoid with an aura of power. You notice a sharp set of front \n\
D:teeth. \n\
\n\
N:394:Gorgimera\n\
G:h:D\n\
I:110:4:25d20:12:55:10\n\
W:27:2:0:200\n\
B:BITE:FIRE:1d3\n\
B:BITE:HURT:1d10\n\
B:GAZE:PARALYZE:2d4\n\
B:BUTT:HURT:1d3\n\
F:FORCE_SLEEP | \n\
F:BASH_DOOR | \n\
F:IM_FIRE\n\
S:1_IN_8 | \n\
S:BR_FIRE \n\
D:It has 3 heads - gorgon, goat and dragon - all attached to a \n\
D:lion's body. \n\
\n\
N:395:Pit Vulture \n\
G:B:D\n\
I:120:4:25d20:12:85:10\n\
W:27:2:0:280\n\
B:BITE:HURT:1d6\n\
F:ANIMAL | IM_FIRE | EVIL | DEVIL | IM_POIS | DEMON\n\
S:1_IN_6\n\
S:SCARE | HASTE\n\
D:An enormous vulture devil straight from the pits of hell, \n\
D:it seems to be coated with a tar-like substance that protects \n\
D:it from most blows. \n\
\n\
##### Level 28 #####\n\
\n\
N:396:Spirit succubus\n\
G:u:B\n\
I:110:4:30d15:20:75:120\n\
W:28:2:0:60\n\
B:CRUSH:HURT:2d8\n\
B:BITE:HURT:1d8\n\
F:FEMALE | \n\
F:FORCE_SLEEP | \n\
F:ONLY_ITEM | DROP_90 | DROP_2D2 | \n\
F:INVISIBLE | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | NO_CONF | NO_SLEEP\n\
S:1_IN_4 | \n\
S:HEAL | BLIND | MIND_BLAST | DARKNESS\n\
D:A wraithly beautiful demon, she is the \n\
D:most powerful of her kind. \n\
\n\
N:397:Osnokelis\n\
G:U:g\n\
I:120:4:25d100:20:100:20\n\
W:28:2:0:1500\n\
B:HIT:HURT:8d6\n\
F:UNIQUE | FEMALE |\n\
F:FORCE_MAXHP | \n\
F:ESCORT | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | DEMON | IM_POIS\n\
D:A very pretty demon, she seems to lack magical powers but makes it up with \n\
D:a set of spiked gauntlets and escorts. \n\
\n\
N:398:Black knight\n\
G:p:D\n\
I:120:4:30d10:20:70:10\n\
W:28:1:0:240\n\
B:HIT:HURT:5d5\n\
F:MALE | \n\
F:FORCE_SLEEP | DROP_1D2 | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL\n\
S:1_IN_8 | \n\
S:BLIND | SCARE | CAUSE_3 | DARKNESS\n\
D:He is a figure encased in deep black plate armour; he looks at you \n\
D:menacingly. \n\
\n\
N:399:Elder Horror\n\
G:H:v\n\
I:110:2:15d10:20:60:10\n\
W:28:1:0:200\n\
B:GAZE:LOSE_INT:2d6\n\
B:GAZE:LOSE_WIS:2d6\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_60 | DROP_1D2 | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | NO_CONF | NO_SLEEP\n\
S:1_IN_8 | \n\
S:BLIND | HOLD | SCARE | MIND_BLAST | BRAIN_SMASH | FORGET\n\
D:This Elder has sworn allegiance to Lucifer himself, it has a \n\
D:gruesome head, tentacular mouth, and piercing \n\
D:eyes. Claws reach out for you and you feel a presence invade your mind. \n\
\n\
N:400:Thammuz\n\
G:U:d\n\
I:120:6:70d12:10:80:12\n\
W:28:2:0:555\n\
B:CRUSH:HURT:2d10\n\
F:FORCE_SLEEP | FORCE_MAXHP | KILL_BODY | KILL_ITEM | UNIQUE | REFLECTING |\n\
F:COLD_BLOOD | BASH_DOOR | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS\n\
F:NO_FEAR | NO_CONF | NO_SLEEP | NONLIVING | RES_TELE | ATTR_MULTI\n\
D:Thammuz is a demon of low category, considered inventor of the Inquisition, fire guns, artillery, \n\
D:and the one that stimulates men to torture other people. Some treatises on demonology say that he \n\
D:is the ambassador of Hell to Spain. \n\
\n\
N:401:Dispater\n\
G:A:g\n\
I:120:4:40d12:20:50:20\n\
W:28:5:0:750\n\
B:HIT:POISON:3d4\n\
B:HIT:HURT:3d4\n\
F:UNIQUE | MALE | FALLEN_ANGEL\n\
F:FORCE_MAXHP | FORCE_SLEEP | \n\
F:RAND_25 | \n\
F:ONLY_ITEM | DROP_4D2 | DROP_GOOD | \n\
F:INVISIBLE | BASH_DOOR | RES_TELE\n\
F:EVIL | DEMON | IM_FIRE\n\
S:1_IN_5 | \n\
S:BLINK | TPORT | TELE_TO | TELE_AWAY | TELE_LEVEL | BLIND | \n\
S:CONF | SCARE | S_DEVIL\n\
D:The least of the fallen angels, Dispater is still a formiddable opponent. \n\
\n\
N:402:Basilisk\n\
G:R:D\n\
I:120:4:20d30:15:90:30\n\
W:28:3:0:350\n\
B:GAZE:PARALYZE\n\
B:BITE:HURT:2d12\n\
F:ONLY_ITEM | DROP_1D2 | \n\
F:OPEN_DOOR | BASH_DOOR | EVIL |\n\
F:ANIMAL | NO_CONF | NO_SLEEP\n\
S:1_IN_8\n\
S:BR_POIS\n\
D:An evil reptile whose eyes stare deeply at you and \n\
D:your soul starts to wilt! \n\
\n\
##### Level 29 #####\n\
\n\
N:403:Erebus\n\
G:v:s\n\
I:110:2:65d8:14:64:25\n\
W:29:4:0:500\n\
B:SPIT:ACID:1d8\n\
B:ENGULF:ACID:2d8\n\
B:CRUSH:HURT:4d8\n\
F:ANIMAL | EVIL | KILL_WALL | IM_ACID |\n\
F:DEVIL\n\
S:1_IN_9\n\
S:BR_ACID\n\
D:A whirling devil spewing chaos about it. \n\
\n\
N:404:Tephras\n\
G:U:o\n\
I:110:6:40d10:30:68:255\n\
W:29:6:0:400\n\
B:HIT:HURT:3d5\n\
F:FORCE_MAXHP | FORCE_SLEEP | NO_FEAR | GOOD | UNIQUE\n\
F:ONLY_ITEM | DROP_2D2 | DEMON\n\
F:SMART | TAKE_ITEM | OPEN_DOOR | BASH_DOOR | POWERFUL | \n\
F:IM_FIRE | IM_COLD | IM_POIS | NO_CONF | NO_SLEEP | RES_TELE\n\
S:1_IN_3 | \n\
S:HEAL | HASTE | BLIND | CONF | SCARE | TPORT | BLINK\n\
D:But the demon answered me : 'I am the spirit of the ashes Tephras. ' \n\
D:And I said to him: 'What is thy pursuit?' \n\
D:And he said: 'I bring darkness on men, and set fire to fields; and I bring homesteads to naught. But most busy am I in summer'. \n\
D:However, when I get an opportunity, I creep into corners of the wall, by night and day. \n\
\n\
N:405:Ring mimic\n\
G:=:w\n\
I:120:4:10d35:30:60:100\n\
W:29:4:0:200\n\
B:HIT:POISON:3d4\n\
F:CHAR_MULTI | \n\
F:FORCE_SLEEP | NEVER_MOVE | \n\
F:EMPTY_MIND | COLD_BLOOD | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_4 | \n\
S:BLIND | CONF | SCARE | CAUSE_2 | FORGET | \n\
S:BO_ACID | BO_FIRE | BO_COLD | BO_ELEC | \n\
S:S_MONSTER\n\
D:A strange creature that disguises itself as discarded objects to lure \n\
D:unsuspecting adventurers within reach of its venomous claws. \n\
\n\
##### Level 30 #####\n\
\n\
### The grigori ####\n\
\n\
N:406:Grigori\n\
G:A:B\n\
I:110:6:40d10:30:68:255\n\
W:30:6:0:400\n\
B:HIT:HURT:3d5\n\
F:FORCE_MAXHP | FORCE_SLEEP | NO_FEAR | GOOD |\n\
F:ONLY_ITEM | DROP_2D2 | FALLEN_ANGEL\n\
F:SMART | TAKE_ITEM | OPEN_DOOR | BASH_DOOR | POWERFUL | \n\
F:IM_FIRE | IM_COLD | IM_POIS | NO_CONF | NO_SLEEP | RES_TELE\n\
S:1_IN_3 | \n\
S:HEAL | HASTE | BLIND | CONF | SCARE | TPORT | BLINK\n\
D:They are a high order of angels and carry the title of the Watchers; \n\
D:also known as the Grigori. They resemble men in appearance, but are \n\
D:taller than giants. The Watchers were sent by God to instruct man in \n\
D:the beginnings of civilization; however, many became fallen angels \n\
D:after they descended to earth to teach man forbidden sciences and \n\
D:started cohabiting with mortal man. \n\
\n\
N:407:Samyaza\n\
G:A:o\n\
I:110:3:15d100:20:70:50\n\
W:33:7:0:3500\n\
B:HIT:HURT:5d5\n\
B:HIT:HURT:5d5\n\
B:HIT:HURT:5d5\n\
B:HIT:HURT:5d5\n\
F:UNIQUE | MALE | EVIL | FALLEN_ANGEL\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GREAT | ESCORT | ESCORTS\n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | \n\
S:1_IN_5\n\
S:BR_ELEC | BA_ELEC | BO_ELEC\n\
D:Samyaza is the first of the fallen Grigori leaders. \n\
D:His name means 'infamous rebellion'. \n\
\n\
N:408:Araqiel\n\
G:A:U\n\
I:110:3:11d100:20:150:50\n\
W:33:7:0:2000\n\
B:HIT:HURT:2d10\n\
B:HIT:HURT:2d10\n\
B:HIT:HURT:2d10\n\
B:HIT:HURT:2d10\n\
F:UNIQUE | MALE | EVIL |  FALLEN_ANGEL\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GOOD | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | PASS_WALL\n\
F:EVIL | FALLEN_ANGEL | \n\
S:1_IN_5\n\
S:TELE_TO | BLINK | TELE_AWAY | TELE_LEVEL\n\
D:Araqiel was the second of the fallen Grigori leaders. \n\
D:His name means 'Earth of god'. \n\
D:He is known to teach geomancy when he is in a better mood. \n\
D:Araqiel is also called Aretstikapha meaning 'world of distortion'. \n\
\n\
N:409:Kokabiel\n\
G:A:w\n\
I:110:3:12d100:8:80:80\n\
W:32:3:0:1200\n\
B:STING:FIRE:2d10\n\
B:STING:FIRE:2d5\n\
B:STING:FIRE:1d4\n\
B:STING:FIRE:2d5\n\
F:UNIQUE | MALE | EVIL |  FALLEN_ANGEL\n\
F:FORCE_SLEEP |  \n\
F:ESCORT | ESCORTS | \n\
F:ONLY_ITEM | DROP_1D2 | DROP_2D2 | DROP_GOOD | SMART |\n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | \n\
F:NO_CONF | NO_SLEEP\n\
S:1_IN_2 | \n\
S:BR_FIRE | S_DEMON | S_DEVIL | S_FALLEN | \n\
D:Kokabiel was the fourth of the fallen Grigori leaders. \n\
D:His name means 'Star of god'. \n\
D:He is known to teach about the constellations when he is in a better mood. \n\
D:Kokabiel is also called Penemue meaning 'instructor of language'. \n\
D:He commands 365,000 surrogate spirits to do his bidding. \n\
\n\
N:410:Ramiel\n\
G:A:y\n\
I:110:3:11d100:20:70:50\n\
W:33:7:0:2000\n\
B:HIT:ELEC:5d5\n\
B:HIT:ELEC:2d10\n\
B:HIT:ELEC:5d5\n\
B:HIT:ELEC:2d10\n\
F:UNIQUE | MALE | FALLEN_ANGEL | EVIL\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GOOD | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | HEAL_ELEC\n\
F:AURA_ELEC | \n\
S:1_IN_4\n\
S:BR_ELEC | BA_ELEC | BO_ELEC\n\
D:Ramiel was the sixth of the fallen Grigori leaders. \n\
D:His name means 'Thunder of God'. \n\
\n\
N:411:Daniel\n\
G:A:u\n\
I:110:3:15d100:20:70:50\n\
W:33:7:0:3500\n\
B:HIT:HURT:5d5\n\
B:HIT:HURT:5d5\n\
B:HIT:HURT:5d5\n\
B:HIT:HURT:5d5\n\
F:UNIQUE | MALE | EVIL |  FALLEN_ANGEL\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GOOD | HEAL_LITE | HURT_DARK\n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | NO_CONF | NO_FEAR | NO_SLEEP | NO_STUN \n\
S:1_IN_3\n\
S:BR_LITE | HEAL\n\
D:Daniel is the seventh of the fallen Grigori leaders. \n\
D:His name means 'Divine Judgement'. \n\
D:He is known to the signs of the sun when he is in a better mood. \n\
\n\
N:412:Chazaqiel\n\
G:A:B\n\
I:110:3:11d100:20:70:50\n\
W:33:7:0:3500\n\
B:HIT:ELEC:5d5\n\
B:HIT:ELEC:5d5\n\
B:HIT:ELEC:5d5\n\
B:HIT:ELEC:5d5\n\
F:UNIQUE | MALE | EVIL |  FALLEN_ANGEL\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GOOD | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | \n\
F:IM_ELEC | AURA_ELEC\n\
S:1_IN_5\n\
S:BR_ELEC | BA_ELEC | BO_ELEC | BO_WATE | BA_WATE\n\
D:Chazaqiel is the eight of the fallen Grigori leaders. \n\
D:His name means 'Cloud of God' \n\
D:He is known to teach meteorology when he is in a better mood. \n\
\n\
N:413:Baraqiel\n\
G:A:y\n\
I:110:3:11d100:20:70:50\n\
W:33:7:0:3500\n\
B:HIT:ELEC:5d5\n\
B:HIT:ELEC:5d5\n\
B:HIT:ELEC:5d5\n\
B:HIT:ELEC:5d5\n\
F:UNIQUE | MALE | EVIL |  FALLEN_ANGEL\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GOOD | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | \n\
F:IM_ELEC | AURA_ELEC\n\
S:1_IN_5\n\
S:BR_ELEC | BA_ELEC | BO_ELEC\n\
D:Baraqiel is the ninth of the fallen Grigori leaders. \n\
D:His name means 'Lightning of God' \n\
D:He is known to teach astronomy when he is in a better mood. \n\
\n\
#Also known as Armaros\n\
N:414:Amaros\n\
G:A:d\n\
I:110:3:11d100:20:70:50\n\
W:33:7:0:2000\n\
B:HIT:UN_BONUS:5d5\n\
B:HIT:UN_BONUS:5d5\n\
B:HIT:UN_BONUS:5d5\n\
B:HIT:UN_BONUS:5d5\n\
F:UNIQUE | MALE | EVIL |  FALLEN_ANGEL\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GOOD | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | \n\
F:ATTR_MULTI\n\
S:1_IN_5\n\
S:TELE_TO\n\
D:Amaros is the eleventh of the fallen Grigori leaders. \n\
D:His name means 'Cursed one'. \n\
D:He is known to teach the resolving of enchantments when he is in a better mood. \n\
\n\
#Also known as Sataniel\n\
N:415:Satariel\n\
G:A:d\n\
I:110:3:11d100:20:70:50\n\
W:33:7:0:2000\n\
B:HIT:HURT:5d5\n\
B:HIT:HURT:5d5\n\
B:HIT:HURT:5d5\n\
B:HIT:HURT:5d5\n\
F:UNIQUE | MALE | EVIL |  FALLEN_ANGEL\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GOOD | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | \n\
F:ATTR_MULTI\n\
S:BR_LITE\n\
D:Amaros is the seventeenth of the fallen Grigori leaders. \n\
D:His name means 'Side of God'. \n\
\n\
#The angelic she-version of the Mystic\n\
N:416:Sariel\n\
G:A:w\n\
I:110:3:15d100:20:70:50\n\
W:33:7:0:3500\n\
B:HIT:PARALYZE:20d2\n\
B:HIT:PARALYZE:20d2\n\
B:HIT:PARALYZE:20d1\n\
B:HIT:PARALYZE:15d1\n\
F:UNIQUE | FEMALE | EVIL |  FALLEN_ANGEL\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GREAT | ESCORT | ESCORTS\n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | \n\
S:1_IN_5\n\
S:BR_ELEC | BA_ELEC | BO_ELEC\n\
D:One of the fallen Grigori leaders, she was also an Archangel. \n\
D:His name means 'Moon of God'. \n\
D:She is known to teach about the moon when she in a better mood. \n\
D:She has an immense knowledge of magic, magical rituals, and magical power \n\
D:and impressive warrior abilities \n\
\n\
#He is escorted by the Se'irim\n\
N:417:Azazel, chief of the Se'irim\n\
G:D:o\n\
I:130:6:35d100:30:120:30\n\
W:33:6:0:18000\n\
B:BUTT:POISON:12d13\n\
B:HIT:POISON:10d10\n\
F:UNIQUE | MALE | FALLEN_ANGEL\n\
F:FORCE_SLEEP | FORCE_MAXHP | ESCORT | ESCORTS | ESCORT_575\n\
F:ONLY_ITEM | DROP_1D2 | DROP_4D2 | DROP_GOOD | ATTR_MULTI\n\
F:BASH_DOOR | \n\
F:EVIL | IM_FIRE | IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_6 | \n\
S:ARROW_4 | BO_MANA | BO_PLAS | BA_ELEC | BR_WALL\n\
D:Azazel is one of the Watchers, a group of fallen angels who mated with mortal women, \n\
D:giving rise to a race of hybrids known as the Nephilim. Azazel is particularly noteworthy \n\
D:among the grigori because it was he who taught men how to make weapons of war, as well as \n\
D:teaching women how to make and wear cosmetics. Eventually, Azazel's teachings created \n\
D:such iniquity that God decided to destroy all life on Earth with Noah's Flood. \n\
D:He looks like a dragon with hands and feet like a man, on his back six wings on the right and six on the left. \n\
\n\
N:418:Ghast\n\
G:z:u\n\
I:120:3:30d10:30:50:20\n\
W:30:3:0:130\n\
B:CLAW:PARALYZE:2d4\n\
B:CLAW:PARALYZE:2d4\n\
B:BITE:LOSE_CON:2d4\n\
B:BITE:LOSE_CHA:2d4\n\
F:DROP_60 | OPEN_DOOR | BASH_DOOR\n\
F:EVIL | UNDEAD | ESCORT | IM_POIS | IM_COLD | NO_CONF | NO_SLEEP\n\
F:COLD_BLOOD | HURT_LITE\n\
S:1_IN_7\n\
S:SCARE | HOLD\n\
D:This vile abomination is a relative of ghouls, and often leads packs \n\
D:of them. It smells foul, and its bite carries a rotting disease. \n\
\n\
N:419:Manticore\n\
G:h:o\n\
I:120:6:25d10:12:15:10\n\
W:30:2:0:300\n\
B:HIT:HURT:3d4\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:BASH_DOOR | \n\
F:EVIL\n\
S:1_IN_5 | \n\
S:ARROW_4\n\
D:It is a winged lion's body with a human torso and a tail covered in \n\
D:vicious spikes. \n\
\n\
##### Level 31 #####\n\
\n\
N:420:Ghost\n\
G:G:w\n\
I:120:5:13d8:20:30:10\n\
W:31:1:0:350\n\
B:WAIL:TERRIFY\n\
B:TOUCH:EXP_20\n\
B:CLAW:LOSE_INT:1d6\n\
F:FORCE_SLEEP | RAND_25 | DROP_60 | DROP_1D2 | \n\
F:INVISIBLE | COLD_BLOOD | TAKE_ITEM | PASS_WALL | \n\
F:EVIL | UNDEAD | IM_COLD | \n\
F:IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_15 | \n\
S:BLIND | HOLD | DRAIN_MANA\n\
D:You don't believe in them. \n\
\n\
##### Level 32 #####\n\
\n\
N:421:Ogre shaman\n\
G:O:b\n\
I:110:3:14d10:20:55:30\n\
W:32:2:0:250\n\
B:HIT:HURT:3d6\n\
F:FORCE_SLEEP | \n\
F:ONLY_ITEM | DROP_90 | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | GIANT\n\
S:1_IN_5 | \n\
S:TPORT | HOLD | SCARE | CAUSE_2 | TRAPS | BO_FIRE | \n\
S:S_MONSTER\n\
D:It is an ogre wrapped in furs and covered in grotesque body paints. \n\
\n\
N:422:The Mother Spider\n\
G:S:D\n\
I:110:3:12d100:8:80:80\n\
W:32:3:0:1200\n\
B:BITE:HURT:2d10\n\
B:STING:POISON:2d5\n\
B:STING:LOSE_STR:1d4\n\
B:STING:POISON:2d5\n\
F:UNIQUE | FEMALE | \n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ESCORT | ESCORTS | \n\
F:ONLY_ITEM | DROP_1D2 | DROP_2D2 | DROP_GOOD | \n\
F:SMART | BASH_DOOR |\n\
F:ANIMAL | EVIL | HURT_LITE | NO_CONF | NO_SLEEP\n\
S:1_IN_2 | \n\
S:HEAL | BLIND | SLOW | CONF | SCARE | CAUSE_3 | CAUSE_4 | \n\
S:TRAPS | BR_DARK\n\
S:S_SPIDER\n\
D:One of the 'incomplete creatures', she has found refuge here and keeps \n\
D:spawning more and more offspring. \n\
\n\
N:423:The Mother Worm\n\
G:w:D\n\
I:110:3:12d100:8:80:80\n\
W:32:3:0:1200\n\
B:BITE:HURT:2d10\n\
B:STING:POISON:2d5\n\
B:STING:LOSE_STR:1d4\n\
B:STING:POISON:2d5\n\
F:UNIQUE | FEMALE | \n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ESCORT | ESCORTS | \n\
F:ONLY_ITEM | DROP_1D2 | DROP_2D2 | DROP_GOOD | \n\
F:SMART | BASH_DOOR |\n\
F:ANIMAL | EVIL | HURT_LITE | NO_CONF | NO_SLEEP\n\
S:1_IN_2 | \n\
S:HEAL | BLIND | SLOW | CONF | SCARE | CAUSE_3 | CAUSE_4 | \n\
S:TRAPS | BR_DARK\n\
S:S_SPIDER\n\
D:One of the 'incomplete creatures', she has found refuge here and keeps \n\
D:spawning more and more offspring. \n\
\n\
##### Level 33 #####\n\
\n\
N:424:Cave troll\n\
G:T:u\n\
I:110:4:24d12:20:50:50\n\
W:33:3:0:350\n\
B:HIT:HURT:3d5\n\
F:MALE | \n\
F:FRIENDS | DROP_60 | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | TROLL | IM_POIS | HURT_LITE\n\
D:He is a vicious monster, feared for his ferocity. \n\
\n\
N:425:Barrow wight\n\
G:W:W\n\
I:110:3:15d10:20:40:10\n\
W:33:3:0:375\n\
B:HIT:HURT:1d8\n\
B:TOUCH:EXP_40\n\
F:FORCE_SLEEP | FRIENDS | DROP_60 | \n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | HURT_LITE | NO_CONF | NO_SLEEP\n\
S:1_IN_8 | \n\
S:HOLD | SCARE | CAUSE_2 | DARKNESS\n\
D:It is a ghostly nightmare of a entity. \n\
\n\
N:426:Giant skeleton troll\n\
G:s:b\n\
I:110:4:45d10:20:50:20\n\
W:33:3:0:325\n\
B:HIT:HURT:1d9\n\
B:BITE:HURT:1d5\n\
F:FORCE_MAXHP | \n\
F:EMPTY_MIND | COLD_BLOOD | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | TROLL | UNDEAD | \n\
F:IM_COLD | IM_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
D:It is the animated form of a massive troll. \n\
\n\
#N:438:Groo the Wanderer\n\
#G:p:o\n\
#I:110:5:11d100:20:70:50\n\
#W:33:7:0:2000\n\
#B:HIT:HURT:5d5\n\
#F:UNIQUE | MALE | WEIRD_MIND |\n\
#F:FORCE_MAXHP | \n\
#F:ONLY_ITEM | DROP_1D2 | DROP_GOOD | DROP_GREAT\n\
#F:DROP_CHOSEN |\n\
#F:OPEN_DOOR | BASH_DOOR | \n\
#F:TROLL | IM_COLD | IM_POIS\n\
#D:He who laughs at Groo's brains will find there is nothing to laugh \n\
#D:about... erm, nobody laughs at Groo and lives. \n\
\n\
N:427:Spectre\n\
G:G:G\n\
I:120:4:14d20:20:30:10\n\
W:33:3:0:350\n\
B:WAIL:TERRIFY\n\
B:TOUCH:EXP_40\n\
B:CLAW:LOSE_WIS:5d5\n\
F:FORCE_SLEEP | RAND_25 | \n\
F:ONLY_ITEM | DROP_90 | DROP_2D2 | \n\
F:COLD_BLOOD | TAKE_ITEM | PASS_WALL |\n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | \n\
F:NO_CONF | NO_SLEEP\n\
S:1_IN_15 | \n\
S:BLIND | HOLD | DRAIN_MANA | FORGET\n\
D:A phantasmal shrieking spirit. Its wail drives the intense cold of pure \n\
D:evil deep within your body. \n\
\n\
N:428:Pit Spider\n\
G:S:v\n\
I:120:6:45d10:30:68:255\n\
W:33:6:0:400\n\
B:HIT:HURT:4d3\n\
B:BITE:POISON:3d10\n\
F:FORCE_SLEEP | FORCE_MAXHP | NO_FEAR |\n\
F:ONLY_ITEM | DROP_1D2 | DROP_2D2 | REFLECTING |\n\
F:SMART | TAKE_ITEM | OPEN_DOOR | BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:NO_CONF | NO_SLEEP | DEVIL | DEMON\n\
S:1_IN_3 | \n\
S:HEAL | HASTE | BLIND | SCARE | MIND_BLAST | SLOW \n\
S:S_SPIDER\n\
D:Only in the pits of hell could a spider grow so big. \n\
\n\
##### Level 34 #####\n\
\n\
N:429:Night stalker\n\
G:H:w\n\
I:130:4:20d13:20:46:20\n\
W:34:3:0:310\n\
B:GAZE:HURT:6d6\n\
F:RAND_50 | \n\
F:EMPTY_MIND | INVISIBLE | COLD_BLOOD | \n\
F:OPEN_DOOR | BASH_DOOR | POWERFUL | \n\
F:EVIL | IM_COLD | IM_POIS | UNDEAD | IM_ELEC\n\
F:NO_CONF | NO_SLEEP | NO_FEAR | NONLIVING\n\
D:It is impossible to define its form but its violence is legendary. \n\
\n\
N:430:Carrion crawler\n\
G:c:g\n\
I:110:3:20d12:15:40:10\n\
W:34:2:0:100\n\
B:STING:PARALYZE:2d6\n\
F:RAND_25 | FRIENDS | \n\
F:WEIRD_MIND | BASH_DOOR | \n\
F:ANIMAL | IM_POIS\n\
D:A hideous centipede covered in slime and with glowing tentacles around its \n\
D:head. \n\
\n\
N:431:Lich\n\
G:L:w\n\
I:110:4:30d10:20:60:60\n\
W:34:3:0:800\n\
B:TOUCH:EXP_40\n\
B:TOUCH:UN_POWER\n\
B:TOUCH:LOSE_DEX:2d8\n\
B:TOUCH:LOSE_DEX:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | DROP_1D2 | \n\
F:SMART | COLD_BLOOD | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | HURT_LITE | \n\
F:NO_CONF | NO_SLEEP\n\
S:1_IN_4 | \n\
S:BLINK | TELE_TO | TELE_AWAY | BLIND | HOLD | SLOW | SCARE | \n\
S:CAUSE_3 | DRAIN_MANA | BRAIN_SMASH\n\
D:It is a skeletal form dressed in robes. It radiates vastly evil power. \n\
\n\
N:432:Master vampire\n\
G:V:s\n\
I:110:4:34d10:20:60:10\n\
W:34:3:0:750\n\
B:HIT:HURT:1d6\n\
B:BITE:EXP_40:1d4\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:DROP_4D2 | \n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | REGENERATE | \n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | HURT_LITE | NO_CONF | NO_SLEEP\n\
S:1_IN_6 | \n\
S:TELE_TO | HOLD | CONF | SCARE | CAUSE_3 | MIND_BLAST | FORGET | \n\
S:DARKNESS | BO_NETH\n\
D:It is a humanoid form dressed in robes. Power emanates from its chilling \n\
D:frame. \n\
\n\
N:433:Oriental vampire\n\
G:V:g\n\
I:110:3:28d12:20:60:10\n\
W:34:3:0:750\n\
B:HIT:HURT:1d6\n\
B:BITE:EXP_40:1d4\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:DROP_4D2 | \n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | REGENERATE | INVISIBLE | PASS_WALL |\n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | HURT_LITE | NO_CONF | NO_SLEEP | RES_TELE\n\
S:1_IN_6 | \n\
S:TELE_TO | HOLD | CONF | SCARE | CAUSE_3 | MIND_BLAST | FORGET | \n\
S:DARKNESS | BO_NETH\n\
D:The oriental vampire is a mist-like creature. \n\
\n\
N:434:Demon locust\n\
G:F:s\n\
I:120:2:18d20:12:50:40\n\
W:34:4:0:275\n\
B:BITE:HURT:1d6\n\
B:STING:POISON:1d4\n\
F:WEIRD_MIND | BASH_DOOR | FRIENDS | \n\
F:ANIMAL\n\
D:Abbadon's locusts, that have the faces of humans, tails of scorpions, and bodies of winged horses. \n\
\n\
##### Level 35 #####\n\
\n\
### The five satans ###\n\
\n\
N:435:Asb'el\n\
G:A:d\n\
I:120:7:18d100:25:100:10\n\
W:36:2:0:1200\n\
B:HIT:HURT:3d8\n\
F:UNIQUE | MALE | FALLEN_ANGEL | EVIL | ATTR_MULTI\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | KILL_BODY\n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_4 | \n\
S:TELE_TO | SHRIEK | SCARE\n\
D:The first of the Five Satans, a group of fallen angels. \n\
D:Each of these five has been cast into exile for different reasons. \n\
D:Their sins were also among the infractions that incited God to inflict the Great Flood upon the Earth. \n\
D:Asb'el's sins included marrying human women, and teaching humanity the secrets of the natural universe, which God did not intend for human knowledge. \n\
\n\
N:436:Gader'el\n\
G:A:d\n\
I:120:7:18d100:25:100:10\n\
W:36:2:0:1200\n\
B:HIT:HURT:3d8\n\
F:UNIQUE | MALE | FALLEN_ANGEL | EVIL | ATTR_MULTI\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | KILL_BODY\n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_4 | \n\
S:TELE_TO | SHRIEK | SCARE\n\
D:The second of the Five Satans, a group of fallen angels. \n\
D:Each of these five has been cast into exile for different reasons. \n\
D:Their sins were also among the infractions that incited God to inflict the Great Flood upon the Earth. \n\
D:Asb'el's sins included marrying human women, and teaching humanity the secrets of the natural universe, which God did not intend for human knowledge. \n\
D:He was the one that showed the children of the people all the blows of death, who misled Eve, \n\
D:who showed the children of the people how to make the instruments of death such as the shield, \n\
D:the breastplate, and the sword for warfare, and all the other instruments of death to the children of the people. \n\
\n\
#Yes , this one is alrady in Hell under another name, a cookie if you find 'em\n\
N:437:Pinem'e\n\
G:A:d\n\
I:120:7:18d100:25:100:10\n\
W:36:2:0:1200\n\
B:HIT:HURT:3d8\n\
F:UNIQUE | MALE | FALLEN_ANGEL | EVIL | ATTR_MULTI\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | KILL_BODY\n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_4 | \n\
S:TELE_TO | SHRIEK | SCARE\n\
D:The first of the Five Satans, a group of fallen angels. \n\
D:Each of these five has been cast into exile for different reasons. \n\
D:Their sins were also among the infractions that incited God to inflict the Great Flood upon the Earth. \n\
D:He demonstrated to the children of the people the bitter and the sweet and revealed to them all the \n\
D:secrets of their wisdom. Furthermore he caused the people to penetrate the secret of writing and the use of ink and paper. \n\
\n\
N:438:Kasadya\n\
G:A:d\n\
I:120:7:18d100:25:100:10\n\
W:36:2:0:1200\n\
B:HIT:HURT:3d8\n\
F:UNIQUE | MALE | FALLEN_ANGEL | EVIL | ATTR_MULTI\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | KILL_BODY\n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_4 | \n\
S:TELE_TO | SHRIEK | SCARE\n\
D:The first of the Five Satans, a group of fallen angels. \n\
D:Each of these five has been cast into exile for different reasons. \n\
D:Their sins were also among the infractions that incited God to inflict the Great Flood upon the Earth. \n\
D:He is the one who revealed to the children of the people the various flagellations of all evil— the \n\
D:flagellations of the souls and the demons, the smashing of the embryo in the womb so that it may be \n\
D:crushed, the flagellation of the soul, snake bites, sunstrokes, the son of the serpent, whose name is Taba'ta. \n\
\n\
N:439:Yeqon\n\
G:A:d\n\
I:120:7:18d100:25:100:10\n\
W:36:2:0:1200\n\
B:HIT:HURT:3d8\n\
F:UNIQUE | MALE | FALLEN_ANGEL | EVIL | ATTR_MULTI\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | KILL_BODY\n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_4 | \n\
S:TELE_TO | SHRIEK | SCARE\n\
D:The first of the Five Satans, a group of fallen angels. \n\
D:Each of these five has been cast into exile for different reasons. \n\
D:Their sins were also among the infractions that incited God to inflict the Great Flood upon the Earth. \n\
D:He is the one who misled all the children of the angels, brought them down upon the earth, \n\
D:and perverted them by the daughters of the people. \n\
\n\
N:440:Headless ghost\n\
G:g:u\n\
I:120:3:22d10:20:30:10\n\
W:35:2:0:1200\n\
B:CLAW:LOSE_INT:5d5\n\
B:CLAW:LOSE_WIS:5d5\n\
B:TOUCH:EXP_40\n\
B:TOUCH:TERRIFY\n\
F:COLD_BLOOD | DROP_2D2 | DROP_90 | EVIL | NO_SLEEP | \n\
F:ONLY_ITEM | PASS_WALL | RAND_25 | IM_COLD | NO_CONF | IM_POIS | TAKE_ITEM |\n\
F:UNDEAD |  \n\
S:1_IN_15 | BLIND | BO_COLD | DRAIN_MANA | FORGET | SCARE\n\
D:A phantasmal apparition with no head \n\
\n\
##### Level 36 #####\n\
\n\
N:441:Grey wraith\n\
G:W:s\n\
I:110:3:19d10:20:50:10\n\
W:36:1:0:700\n\
B:HIT:HURT:1d10\n\
B:TOUCH:EXP_40\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:DROP_60 | DROP_90 | \n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | UNDEAD | HURT_LITE | IM_COLD | IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_7 |\n\
S:HOLD | SCARE | CAUSE_3 | DARKNESS\n\
D:A tangible but ghostly form, made of grey fog. The air around it feels \n\
D:deathly cold. \n\
\n\
N:442:Raal's Tome of Destruction\n\
G:?:r\n\
I:120:1:50d15:20:150:15\n\
W:36:4:0:1500\n\
F:NEVER_MOVE | NEVER_BLOW\n\
F:FORCE_SLEEP | DROP_90 | DROP_GOOD | EVIL | COLD_BLOOD | EMPTY_MIND |\n\
F:FORCE_MAXHP | NO_CONF | NO_FEAR | NO_SLEEP | CHAR_MULTI |\n\
F:IM_ACID | IM_POIS | IM_COLD | IM_ELEC | HURT_FIRE | RES_NETH | RES_TELE\n\
S:1_IN_2 |\n\
S:BO_ACID | BR_FIRE | BO_MANA | BR_COLD | BR_POIS |\n\
S:BO_WATE | BA_POIS | BR_NETH\n\
D:A sentient arcane tome casting spells with malevolent intent. \n\
\n\
N:443:Colossus\n\
G:g:y\n\
I:100:5:30d100:15:150:10\n\
W:36:4:0:900\n\
B:HIT:HURT:10d10\n\
F:FORCE_MAXHP | \n\
F:EMPTY_MIND | COLD_BLOOD | BASH_DOOR | \n\
F:IM_FIRE | IM_COLD | IM_ELEC | \n\
F:IM_POIS | NONLIVING | REFLECTING |\n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_8\n\
S:ARROW_4\n\
D:An enormous construct resembling a titan made from stone. It strides \n\
D:purposefully towards you, swinging its slow fists with earth-shattering \n\
D:power. \n\
\n\
N:444:Tunnel Dragon\n\
G:d:U\n\
I:110:3:30d10:20:63:150\n\
W:36:2:0:950\n\
B:CLAW:HURT:1d8\n\
B:BITE:HURT:2d8\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:DROP_60 | DROP_90 | DROP_2D2 | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:DRAGON | KILL_WALL | KILL_ITEM | KILL_BODY\n\
S:1_IN_11 | \n\
S:SCARE | \n\
S:BR_DISI\n\
D:Tunnel Dragons have briefly appeared in Alpine Mythology, \n\
D:they destroy anything that blocks their path. \n\
\n\
N:445:Balaur\n\
G:d:g\n\
I:110:3:40d10:20:70:70\n\
W:36:1:0:1100\n\
B:CLAW:HURT:1d4\n\
B:BITE:HURT:1d6\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:DROP_60 | DROP_90 | DROP_2D2 | \n\
F:BASH_DOOR | \n\
F:EVIL | DRAGON | IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_9 | \n\
S:SCARE | \n\
S:BR_POIS\n\
D:a balaur is a creature similar to a dragon. \n\
D:A balaur is quite large, has fins, feet, and multiple serpent heads (usually three, sometimes seven, or even twelve). \n\
\n\
\n\
#N:478:Nightblade\n\
#G:h:D\n\
#I:120:4:19d13:20:60:10\n\
#W:36:2:0:315\n\
#B:HIT:POISON:3d4\n\
#B:HIT:HURT:3d4\n\
#B:HIT:LOSE_CON:3d4\n\
#F:MALE | \n\
#F:DROP_1D2 | FRIENDS | INVISIBLE |\n\
#F:OPEN_DOOR | BASH_DOOR | HURT_LITE |\n\
#F:EVIL | NO_CONF | NO_SLEEP\n\
#D:A dark elf so stealthy that he is almost impossible to see. \n\
\n\
N:446:Bodak\n\
G:u:D\n\
I:110:3:35d10:10:68:90\n\
W:36:2:0:750\n\
B:HIT:FIRE:4d6\n\
B:GAZE:EXP_20\n\
F:FORCE_SLEEP | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | AURA_FIRE | NONLIVING |\n\
F:EVIL | DEMON | IM_FIRE | IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_4 | \n\
S:BO_FIRE | BA_FIRE | \n\
S:S_DEMON\n\
D:It is a humanoid form composed of flames and hatred. \n\
\n\
N:447:Alu'u\n\
G:G:G\n\
I:110:4:35d10:10:70:50\n\
W:36:3:0:800\n\
B:WAIL:HURT:4d6\n\
B:TOUCH:LOSE_WIS\n\
F:FORCE_SLEEP | OPEN_DOOR | BASH_DOOR | NONLIVING |\n\
F:EVIL | DEMON | IM_POIS | IM_ACID | NO_CONF | NO_SLEEP | RES_TELE |\n\
F:DEVIL | INVISIBLE | PASS_WALL\n\
S:1_IN_4 |\n\
S:SCARE | TELE_AWAY | BA_SLIM | CAUSE_4 | BA_POIS |\n\
S:CONF | S_DEVIL | S_UNDEAD\n\
D:A ghostly human, turned into a devil by the whim of Belial \n\
\n\
N:448:Ipsissimus\n\
G:p:D\n\
I:110:2:28d10:20:50:10\n\
W:36:2:0:666\n\
B:HIT:HURT:2d6\n\
F:MALE | \n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_1D2 | \n\
F:SMART | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL\n\
S:1_IN_3 | \n\
S:HASTE | TPORT | TELE_TO | BLIND | HOLD | SCARE | CAUSE_3 | \n\
S:BO_NETH | MIND_BLAST | FORGET |\n\
S:S_UNDEAD | S_DEMON\n\
D:A gaunt figure, clothed in black robes, he has reached the highest state under the guidance of Lucifer himself. \n\
\n\
N:449:The Insane Crusader\n\
G:p:v\n\
I:120:7:18d100:25:100:10\n\
W:36:2:0:1200\n\
B:HIT:HURT:3d8\n\
F:UNIQUE | MALE |\n\
F:FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | RAND_25 |\n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_4 | \n\
S:TELE_TO | SHRIEK | SCARE\n\
D:Once a powerful adventurer, this poor fighter has seen a few too many \n\
D:eldritch horrors in his time. Any shred of lucidity is long gone, but \n\
D:he still remains dangerous. He wanders aimlessly through the dungeon \n\
D:randomly stiking at foes both real and imagined, all the while screaming \n\
D:out at the world which caused his condition. \n\
\n\
##### Level 37 #####\n\
\n\
#DEMON D&D\n\
N:450:Vrock\n\
G:U:s\n\
I:110:3:280d1:20:50:80\n\
W:37:2:0:1000\n\
B:CRUSH:HURT:8d12\n\
B:CRUSH:HURT:8d12\n\
B:HIT:POISON:3d4\n\
F:BASH_DOOR | DEMON | DROP_60 | EVIL | NO_SLEEP | ONLY_ITEM | \n\
F:OPEN_DOOR | NO_CONF | IM_FIRE\n\
S:1_IN_8 | BLIND | CONF\n\
D:It is a demon with a long neck and raking claws. \n\
\n\
N:451:Wiltering Horror\n\
G:H:G\n\
I:120:8:35d10:90:70:10\n\
W:37:2:0:1000\n\
B:TOUCH:LOSE_ALL:3d4\n\
B:TOUCH:EXP_40:3d4\n\
F:PASS_WALL | IM_POIS | IM_FIRE | IM_ELEC | NONLIVING |\n\
F:NO_CONF | NO_SLEEP | EVIL | EMPTY_MIND | KILL_ITEM | RAND_50\n\
S:1_IN_7\n\
S:BR_TIME | FORGET\n\
D:This horror is bent on undoing you. \n\
\n\
#TODO : Something way more in theme\n\
#N:487:Mother Ant\n\
#G:a:v\n\
#I:120:5:15d100:30:100:10\n\
#W:37:2:0:1000\n\
#B:BITE:HURT:2d12\n\
#F:UNIQUE | FEMALE | GOOD |\n\
#F:FORCE_SLEEP | FORCE_MAXHP | \n\
#F:ESCORT | ESCORTS | \n\
#F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
#F:WEIRD_MIND | OPEN_DOOR | BASH_DOOR | \n\
#F:ANIMAL | NO_CONF | NO_SLEEP\n\
#S:1_IN_2 | \n\
#S:S_ANT\n\
#D:She's upset because you hurt her children. \n\
\n\
N:452:Will o' the wisp\n\
G:u:y\n\
I:130:7:20d10:30:150:0\n\
W:37:4:0:500\n\
B:HIT:HURT:1d9\n\
F:FORCE_SLEEP | FORCE_MAXHP | RAND_50 | \n\
F:SMART | EMPTY_MIND | INVISIBLE | \n\
F:PASS_WALL | POWERFUL | \n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR | NONLIVING\n\
S:1_IN_2 | \n\
S:BLINK | TPORT | CONF | CAUSE_2\n\
D:A strange ball of glowing light. It disappears and reappears and seems to \n\
D:draw you to it. You seem somehow compelled to stand still and watch its \n\
D:strange dancing motion. \n\
\n\
##### Level 38 #####\n\
\n\
#DEMON D&D\n\
N:453:Hezrou\n\
G:U:v\n\
I:110:4:380d1:20:40:80\n\
W:38:3:0:1111\n\
B:HIT:POISON:3d4\n\
B:HIT:POISON:3d4\n\
F:BASH_DOOR | DEMON | DROP_2D2 | EVIL | NO_SLEEP | ONLY_ITEM |\n\
F:OPEN_DOOR | NO_CONF | IM_FIRE\n\
S:1_IN_9 | BO_FIRE | S_DEMON\n\
D:It is a demon of lizard form with cruel-looking jaws. \n\
\n\
N:454:Death knight\n\
G:p:s\n\
I:120:6:60d10:20:100:10\n\
W:38:1:0:1111\n\
B:HIT:HURT:5d5\n\
B:HIT:EXP_20\n\
F:FORCE_SLEEP | FORCE_MAXHP | SMART | \n\
F:ONLY_ITEM | DROP_1D2 | DROP_2D2 | \n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_COLD\n\
S:1_IN_5 | \n\
S:BLIND | SCARE | CAUSE_3 | BO_NETH | \n\
S:S_MONSTERS\n\
D:It is a humanoid form dressed in armour of an ancient form. From beneath \n\
D:its helmet, eyes glow a baleful red and seem to pierce you like lances of \n\
D:fire. \n\
\n\
N:455:Mandor, Master of Chaos\n\
G:p:v\n\
I:120:5:88d11:20:90:40\n\
W:38:5:0:1600\n\
B:HIT:HURT:5d5\n\
B:HIT:UN_POWER:5d5\n\
B:HIT:UN_BONUS:5d5\n\
F:UNIQUE | MALE |\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:SMART | OPEN_DOOR | TAKE_ITEM | BASH_DOOR | \n\
F:EVIL | RES_TELE\n\
S:1_IN_2 |\n\
S:BO_FIRE | BO_COLD | HOLD | BO_MANA | S_MONSTER\n\
S:TRAPS | BO_ICEE | HEAL | BO_PLAS | BA_CHAO\n\
D:Mandor is one of the greatest chaos Masters, a formidable magician. \n\
\n\
N:456:Emperor wight\n\
G:W:y\n\
I:120:4:38d10:20:40:10\n\
W:38:2:0:1600\n\
B:HIT:HURT:1d12\n\
B:TOUCH:EXP_80\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_90 | DROP_4D2 | \n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | HURT_LITE | NO_CONF | NO_SLEEP\n\
S:1_IN_6 | \n\
S:HOLD | SCARE | CAUSE_3 | BO_NETH\n\
D:Your life force is torn from your body as this powerful unearthly being \n\
D:approaches. \n\
\n\
N:457:Yaksha\n\
G:O:w\n\
I:120:6:50d10:30:68:255\n\
W:38:6:0:1800\n\
B:BITE:HURT:4d6\n\
F:FORCE_SLEEP | FORCE_MAXHP | SMART | NO_FEAR | INVISIBLE |\n\
F:ONLY_ITEM | DROP_1D2 | DROP_2D2 | REFLECTING | RES_TELE |\n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | EVIL | \n\
F:NO_CONF | NO_SLEEP | WEIRD_MIND | DEVIL\n\
S:1_IN_11 | \n\
S:HEAL | HASTE | TELE_AWAY | CONF | BO_MANA | BO_PLAS | \n\
S:S_MONSTERS | \n\
D:A cannibal devil of gluttony, eating anything and everything. \n\
\n\
N:458:Karakal, Spirit of Fire\n\
G:E:R\n\
I:120:6:15d100:12:50:50\n\
W:38:3:0:3000\n\
B:HIT:FIRE:6d6\n\
F:UNIQUE | \n\
F:FORCE_SLEEP | FORCE_MAXHP | RAND_25 | \n\
F:EMPTY_MIND | MALE | AURA_FIRE |\n\
F:KILL_ITEM | KILL_BODY | BASH_DOOR | POWERFUL | \n\
F:EVIL | IM_FIRE | \n\
F:IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_4 | \n\
S:BO_PLAS | BA_FIRE\n\
D:A towering fire elemental, Karakal burns everything beyond recognition. \n\
\n\
N:459:Black wraith\n\
G:W:D\n\
I:120:4:50d10:20:55:10\n\
W:38:2:0:1700\n\
B:HIT:HURT:1d12\n\
B:TOUCH:EXP_40\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_1D2 | DROP_2D2 | \n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | HURT_LITE | NO_CONF | NO_SLEEP\n\
S:1_IN_7 | \n\
S:BLIND | HOLD | SCARE | CAUSE_3 | BO_NETH\n\
D:A figure that seems made of void, its strangely human shape is cloaked in \n\
D:shadow. It reaches out at you. \n\
\n\
N:460:Nightgaunt\n\
G:U:D\n\
I:110:3:24d10:20:50:80\n\
W:38:2:0:1000\n\
B:CRUSH:LOSE_STR:1d5\n\
B:TOUCH:PARALYZE:3d4\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_60 |\n\
F:OPEN_DOOR | BASH_DOOR | POWERFUL | \n\
F:EVIL | DEMON | \n\
F:IM_FIRE | IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_7 | \n\
S:BLIND | CONF | BO_FIRE\n\
D:It is a black, horned humanoid with wings. \n\
\n\
N:461:Baron of hell\n\
G:U:U\n\
I:110:3:150d10:10:130:40\n\
W:38:3:0:900\n\
B:CLAW:HURT:11d2\n\
F:IM_POIS | OPEN_DOOR | BASH_DOOR | MALE | RES_PLAS | IM_FIRE | NONLIVING |\n\
F:IM_FIRE | NO_CONF | NO_SLEEP | EVIL | DEMON | FORCE_MAXHP | RES_TELE\n\
S:1_IN_2 |\n\
S:BO_PLAS\n\
D:A minor demon lord with a goat's head, tough to kill. \n\
\n\
##### Level 39 #####\n\
\n\
N:462:Nether wraith\n\
G:W:R\n\
I:120:4:48d10:20:55:10\n\
W:39:2:0:1700\n\
B:HIT:HURT:1d12\n\
B:TOUCH:EXP_80\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_90 | DROP_4D2 | \n\
F:INVISIBLE | COLD_BLOOD | PASS_WALL | \n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | \n\
F:HURT_LITE | NO_CONF | NO_SLEEP\n\
S:1_IN_6 | \n\
S:BLIND | SCARE | CAUSE_3 | MIND_BLAST | DARKNESS | BO_NETH\n\
D:A form that hurts the eye, death permeates the air around it. As it nears \n\
D:you, a coldness saps your soul. \n\
\n\
N:463:Lasha, Mistress of Water\n\
G:h:B\n\
I:120:5:20d100:12:40:50\n\
W:39:3:0:3250\n\
B:HIT:HURT:5d5\n\
F:UNIQUE | FEMALE | AQUATIC | \n\
F:FORCE_SLEEP | FORCE_MAXHP | RAND_25 | \n\
F:FEMALE | \n\
F:KILL_ITEM | KILL_BODY | BASH_DOOR | POWERFUL | \n\
F:EVIL | IM_POIS |\n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_4 | \n\
S:BO_ICEE | BO_WATE | BA_COLD | BA_WATE\n\
D:With the body of a beautiful mermaid, she hides her cruel \n\
D:nature well, until it is too late. \n\
\n\
N:464:Abezithibod\n\
G:d:v\n\
I:120:7:13d100:20:85:30\n\
W:39:3:0:3000\n\
B:BITE:HURT:2d12\n\
F:UNIQUE | MALE | ATTR_MULTI | ATTR_ANY |\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_4D2 | DROP_GOOD | RES_NEXU |\n\
F:OPEN_DOOR | BASH_DOOR | POWERFUL | \n\
F:EVIL | DRAGON | IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | \n\
F:NO_CONF | NO_SLEEP\n\
S:1_IN_4 | \n\
S:BR_ACID | BR_FIRE | BR_COLD | BR_ELEC | BR_SOUN | BR_CONF | \n\
S:BR_SHAR | BR_GRAV | BR_NEXU\n\
D:A fierce spirit and winged, and with a single wing, plotting against every spirit under heaven. \n\
\n\
N:465:Pit Leech\n\
G:w:D\n\
I:120:4:100d10:20:90:20\n\
W:39:2:0:2300\n\
B:CRUSH:SHATTER:3d11\n\
B:BITE:LOSE_CON:1d2\n\
F:IM_FIRE | RES_PLAS | IM_COLD | IM_POIS | RES_TELE\n\
F:KILL_WALL | ONLY_GOLD | DROP_4D2 | DROP_2D2 | DEVIL\n\
F:EVIL\n\
S:1_IN_5 | \n\
S:SCARE | CONF | HOLD | S_DEMON | \n\
S:MIND_BLAST | HEAL | HASTE | FORGET | BRAIN_SMASH\n\
D:A huge leech, blood dripping from it's infernal maw. \n\
D:It has been feeding on other devils, acquiring some \n\
D:of their magical skills. \n\
\n\
N:466:Night mare\n\
G:q:v\n\
I:120:5:15d100:30:85:0\n\
W:39:3:0:2900\n\
B:BITE:EXP_80:2d6\n\
B:HIT:HURT:3d8\n\
B:HIT:CONFUSE:6d6\n\
F:FORCE_MAXHP | \n\
F:ONLY_GOLD | DROP_2D2 | \n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | NO_CONF | NO_SLEEP\n\
D:A fearsome skeletal horse with glowing eyes, that watch you with little \n\
D:more than a hatred of all that lives. \n\
\n\
N:467:Vampire lord\n\
G:V:b\n\
I:120:5:16d100:20:70:10\n\
W:39:3:0:1800\n\
B:HIT:HURT:1d6\n\
B:BITE:EXP_80:1d6\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:DROP_60 | DROP_4D2 | \n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | REGENERATE | RES_TELE\n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | HURT_LITE | \n\
F:NO_CONF | NO_SLEEP\n\
S:1_IN_7 | \n\
S:BLIND | HOLD | SCARE | CAUSE_3 | CAUSE_4 | DRAIN_MANA | \n\
S:BRAIN_SMASH | DARKNESS | BO_NETH\n\
D:A foul wind chills your bones as this ghastly figure approaches. \n\
\n\
##### Level 40 #####\n\
\n\
#DEMON D&D\n\
N:468:Glabrezu\n\
G:U:U\n\
I:120:4:500d1:20:40:80\n\
W:40:2:0:1800\n\
B:HIT:POISON:3d4\n\
B:HIT:POISON:3d4\n\
F:BASH_DOOR | DEMON | DROP_90 | EVIL | NO_SLEEP | ONLY_ITEM | OPEN_DOOR |\n\
F:NO_CONF | IM_FIRE\n\
S:1_IN_9 | BO_FIRE | S_DEMON\n\
D:It is demon with arms and pincers, its form a true mockery of life. \n\
\n\
N:469:War troll\n\
G:T:g\n\
I:120:4:50d10:20:100:50\n\
W:40:3:0:800\n\
B:HIT:HURT:3d5\n\
F:FORCE_MAXHP | \n\
F:DROP_90 | REGENERATE | FRIENDS |\n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | TROLL | IM_POIS | NO_CONF | NO_SLEEP | NO_FEAR\n\
D:A massive troll, equipped with a scimitar and heavy armour. \n\
\n\
N:470:Lesser titan\n\
G:P:y\n\
I:120:5:10d100:30:80:15\n\
W:40:3:0:3500\n\
B:HIT:CONFUSE:6d6\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:DROP_2D2 | DROP_4D2 | \n\
F:SMART | TAKE_ITEM | OPEN_DOOR | BASH_DOOR |\n\
F:EVIL | GIANT | MALE\n\
S:1_IN_3 | \n\
S:HEAL | TELE_TO | SCARE | \n\
S:S_MONSTERS\n\
D:It is a humanoid figure thirty feet tall that gives off an aura of power \n\
D:and hate. \n\
\n\
N:471:Cult leader\n\
G:p:b\n\
I:120:4:52d10:20:60:10\n\
W:40:2:0:1800\n\
B:HIT:HURT:3d4\n\
F:MALE | \n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_90 | DROP_2D2 | \n\
F:SMART | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | NO_CONF | NO_SLEEP\n\
S:1_IN_2 | \n\
S:HEAL | BLIND | HOLD | CONF | CAUSE_3 | \n\
S:S_MONSTER | S_UNDEAD | S_DEVIL\n\
D:An evil priest, dressed all in black. Deadly spells hit you at an \n\
D:alarming rate as his black spiked mace rains down blow after blow on your \n\
D:pitiful frame. \n\
\n\
N:472:Sorcerer\n\
G:p:o\n\
I:130:5:52d10:20:60:10\n\
W:40:2:0:2150\n\
B:HIT:HURT:2d8\n\
F:MALE | \n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_90 | DROP_4D2 | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | NO_CONF | NO_SLEEP\n\
S:1_IN_2 | \n\
S:BLINK | TELE_TO | BLIND | CONF | CAUSE_3 | TRAPS | \n\
S:BO_ACID | BA_FIRE | BA_COLD | \n\
S:S_MONSTER | S_UNDEAD | S_DRAGON\n\
D:A human figure in robes, he moves with magically improved speed, and his \n\
D:hands are ablur with spell casting. \n\
\n\
#Obizuth , Medusa ;) , ' I have no work other than the destruction of children, and the making their ears to be deaf, and the working of evil to their eyes, \n\
N:473:Medusa, the Gorgon\n\
G:n:v\n\
I:120:6:24d100:30:100:5\n\
W:40:3:0:9000\n\
B:GAZE:EXP_80\n\
B:GAZE:PARALYZE\n\
B:HIT:HURT:8d6\n\
F:UNIQUE | FEMALE | \n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_1D2 | DROP_2D2 | DROP_GOOD | \n\
F:SMART | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_ACID | IM_FIRE | IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_2 | \n\
S:HOLD | SCARE | CAUSE_3 | BO_FIRE | BO_PLAS | BA_POIS | \n\
S:S_HYDRA\n\
D:Her face could sink a thousand ships. \n\
\n\
N:474:Death drake\n\
G:D:u\n\
I:120:4:10d100:25:100:30\n\
W:40:2:0:3500\n\
B:CLAW:HURT:4d10\n\
B:BITE:EXP_80:3d6\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_4D2 | RES_TELE\n\
F:INVISIBLE | TAKE_ITEM | \n\
F:PASS_WALL | POWERFUL | MOVE_BODY | RES_NETH |\n\
F:EVIL | DRAGON | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_6 | \n\
S:SLOW | CONF | SCARE | \n\
S:BR_NETH\n\
D:It is a dragon-like form wrapped in darkness. You cannot make out its \n\
D:true form but you sense its evil. \n\
\n\
N:475:Clubber demon\n\
G:U:s\n\
I:110:3:40d10:20:50:80\n\
W:40:2:0:1000\n\
B:HIT:HURT:8d12\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:FRIENDS | \n\
F:ONLY_ITEM | DROP_60 | \n\
F:OPEN_DOOR | BASH_DOOR | POWERFUL | NONLIVING |\n\
F:EVIL | DEMON | IM_FIRE | NO_CONF | NO_SLEEP\n\
S:1_IN_8 | \n\
S:BLIND | CONF\n\
D:It is a demon swinging wildly with two clubs. Not even remotely subtle. \n\
\n\
N:476:Death quasit\n\
G:u:w\n\
I:130:6:44d10:20:80:0\n\
W:40:3:0:1000\n\
B:BITE:LOSE_DEX:3d6\n\
B:CLAW:HURT:3d3\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_90 | DROP_2D2 | DROP_4D2 | NONLIVING |\n\
F:SMART | INVISIBLE | PASS_WALL | \n\
F:EVIL | DEMON | IM_FIRE | IM_POIS | RES_TELE\n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_10 | \n\
S:BLIND | CONF | SCARE | CAUSE_3 | FORGET | \n\
S:S_DEMON\n\
D:It is a demon of small stature, but its armoured frame moves with \n\
D:lightning speed and its powers make it a tornado of death and destruction. \n\
\n\
##### Level 41 #####\n\
\n\
N:477:Nalfeshnee\n\
G:U:r\n\
I:110:4:450d1:20:50:80\n\
W:41:1:0:5000\n\
B:HIT:POISON:3d4\n\
B:HIT:POISON:3d4\n\
B:HIT:POISON:3d4\n\
F:BASH_DOOR | DEMON | DROP_1D2 | EVIL | NO_SLEEP | ONLY_ITEM | OPEN_DOOR |\n\
F:NO_CONF | IM_FIRE\n\
S:1_IN_9 | BLIND | BR_FIRE | CONF | S_DEMON\n\
D:It is a large demon with the head of a giant boar.  Flames run up and down \n\
D:its length. \n\
\n\
N:478:Glaryssa, Succubus Queen\n\
G:U:W\n\
I:120:5:12d100:90:60:10\n\
W:41:3:0:8000\n\
B:CLAW:HURT:5d5\n\
B:HIT:LOSE_STR:4d4\n\
B:TOUCH:EXP_80:8d1\n\
F:UNIQUE | \n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_4D2 | DROP_GOOD |\n\
F:COLD_BLOOD | PASS_WALL | MOVE_BODY | NONLIVING |\n\
F:OPEN_DOOR | BASH_DOOR | IM_POIS | IM_COLD | DEMON | EVIL \n\
S:1_IN_3 |\n\
S:CAUSE_3 | HOLD | BLIND | BO_ACID | S_DEMON |\n\
S:FORGET | BO_NETH | MIND_BLAST | DARKNESS\n\
D:Drop dead gorgeous - literally. \n\
\n\
N:479:Master lich\n\
G:L:W\n\
I:120:4:18d100:20:80:50\n\
W:41:2:0:10000\n\
B:TOUCH:EXP_80\n\
B:TOUCH:UN_POWER\n\
B:TOUCH:LOSE_DEX:2d12\n\
F:FORCE_SLEEP | FORCE_MAXHP | SMART | RES_TELE\n\
F:ONLY_ITEM | DROP_2D2 | DROP_4D2 | \n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_3 | \n\
S:BLINK | TELE_TO | BLIND | HOLD | CONF | SCARE | CAUSE_3 | CAUSE_4 | \n\
S:DRAIN_MANA | BRAIN_SMASH | \n\
S:S_UNDEAD\n\
D:A skeletal form wrapped in robes. Powerful magic crackles along its bony \n\
D:fingers. \n\
\n\
N:480:Pit Feline\n\
G:f:D\n\
I:110:2:40d10:20:40:80\n\
W:41:3:0:2500\n\
B:CLAW:LOSE_STR:3d4\n\
B:BITE:EXP_20:3d4\n\
F:FORCE_SLEEP | FORCE_MAXHP | FRIENDS | DEVIL |\n\
F:ONLY_ITEM | DROP_2D2 |\n\
F:OPEN_DOOR | BASH_DOOR | POWERFUL | DEVIL |\n\
F:EVIL | DEMON | IM_FIRE | NO_CONF | NO_SLEEP\n\
S:1_IN_9 | \n\
S:BR_FIRE | S_DEMON | CONF\n\
D:This firebreathing demon cat comes straight out of the pit, \n\
D:together with the rest of her litter. \n\
\n\
#According to the Grimoire of Pope Honorius, a demon named Lucifuge Rofocale is in charge of Hell's government by order of Lucifer.\n\
\n\
N:481:Lucifuge\n\
G:j:D\n\
I:140:8:50d20:20:80:20\n\
W:41:2:0:2500\n\
B:ENGULF:ACID:5d6\n\
B:ENGULF:HURT:6d6\n\
F:REGENERATE | ONLY_ITEM | KILL_ITEM | DROP_2D2 | DROP_90 | DROP_60\n\
F:BASH_DOOR | EVIL | NO_CONF | NO_SLEEP | KILL_BODY | DEVIL\n\
F:FORCE_MAXHP | FORCE_SLEEP | HURT_LITE | POWERFUL |\n\
F:IM_ACID | IM_FIRE | RES_PLAS | IM_POIS | IM_COLD | IM_ELEC | RES_TELE\n\
D:A creature of living darkness, this devil wants nothing more than to \n\
D:snuff you out. \n\
\n\
N:482:Ahhazu\n\
G:U:y\n\
I:130:6:100d35:30:140:255\n\
W:41:6:0:15000\n\
B:GAZE:TERRIFY:4d4\n\
B:HIT:HURT:8d6\n\
F:FORCE_SLEEP | \n\
F:ONLY_ITEM | DROP_3D2 | DROP_4D2 | DROP_GOOD | NO_FEAR | REFLECTING |\n\
F:SMART | TAKE_ITEM | OPEN_DOOR | BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS |\n\
F:RES_TELE | DEVIL\n\
S:1_IN_3 | \n\
S:TELE_TO | BLIND | SCARE | CAUSE_2 | CAUSE_4 | BO_MANA | \n\
S:S_DEVIL | S_UNDEAD\n\
D:A sickly looking devil. Don't be fooled by its appearance - it's healthier \n\
D:than it looks. \n\
\n\
N:483:Pit Hound\n\
G:Z:D\n\
I:110:4:22d20:20:40:80\n\
W:41:2:0:1850\n\
B:CLAW:ACID:2d4\n\
B:CRUSH:HURT:3d4\n\
B:BITE:ACID:6d6\n\
F:FORCE_SLEEP | FORCE_MAXHP | NONLIVING |\n\
F:ONLY_ITEM | DROP_90 | REGENERATE | RES_TELE\n\
F:OPEN_DOOR | BASH_DOOR | POWERFUL | DEVIL | FRIENDS\n\
F:EVIL | NO_CONF | NO_SLEEP | HURT_FIRE | IM_POIS\n\
S:1_IN_9 | \n\
S:BO_FIRE | BO_ACID | S_DEVIL | MIND_BLAST | DARKNESS | \n\
D:A hound the size of an elephant, dripping caustic slime from \n\
D:teeth and claws. \n\
\n\
##### Level 42 #####\n\
\n\
N:484:Rahab\n\
G:D:D\n\
I:110:3:30d17:20:90:80\n\
W:42:2:0:2300\n\
B:BITE:LOSE_DEX:1d3\n\
B:BITE:POISON:1d3\n\
B:CRUSH:HURT:9d4\n\
F:FORCE_SLEEP | FORCE_MAXHP | DRAGON |\n\
F:ONLY_ITEM | DROP_1D2 | DEVIL |\n\
F:OPEN_DOOR | BASH_DOOR | POWERFUL | \n\
F:EVIL | HURT_LITE | IM_POIS | \n\
F:IM_FIRE | NO_CONF | NO_SLEEP\n\
S:1_IN_9 |\n\
S:BLIND | CONF | S_DEVIL | BR_DARK | BR_COLD\n\
D:An infernal dragon, cold and baleful. \n\
\n\
N:485:Shadow demon\n\
G:G:v\n\
I:120:5:10d20:30:30:20\n\
W:42:3:0:425\n\
B:TOUCH:EXP_80\n\
B:TOUCH:EXP_40\n\
B:CLAW:LOSE_INT:1d10\n\
B:CLAW:LOSE_WIS:1d10\n\
F:FORCE_SLEEP | \n\
F:ONLY_ITEM | DROP_1D2 | POWERFUL | REGENERATE | HURT_LITE |\n\
F:INVISIBLE | COLD_BLOOD | PASS_WALL | FRIENDS | RES_NETH |\n\
F:EVIL | UNDEAD | DEMON | IM_COLD | IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_8 | \n\
S:BO_NETH\n\
D:A mighty spirit of darkness of vaguely humanoid form. Razor-edged claws \n\
D:reach out to end your life as it glides towards you, seeking to suck the \n\
D:energy from your soul to feed its power. \n\
\n\
N:486:Iron lich\n\
G:L:s\n\
I:120:5:28d100:30:100:10\n\
W:42:4:0:4000\n\
B:BUTT:COLD:3d6\n\
B:BUTT:FIRE:3d6\n\
B:BUTT:ELEC:3d6\n\
F:FORCE_SLEEP | FORCE_MAXHP | REFLECTING |\n\
F:COLD_BLOOD | BASH_DOOR | \n\
F:EVIL | UNDEAD | POWERFUL | \n\
F:IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | RES_TELE\n\
F:ONLY_ITEM | DROP_60 | DROP_GOOD |\n\
F:NO_CONF | NO_SLEEP | FLIGHT\n\
S:1_IN_2 |\n\
S:BA_WATE | BR_FIRE | BO_ICEE | BA_ELEC | BA_COLD |\n\
S:CAUSE_4 | DRAIN_MANA | BRAIN_SMASH | \n\
S:S_UNDEAD\n\
D:It is a huge, twisted grey skull floating through the air. Its cold eyes \n\
D:burn with hatred towards all who live. \n\
\n\
##### Level 43 #####\n\
\n\
N:487:Marilith\n\
G:U:y\n\
I:120:4:730d1:20:75:80\n\
W:43:1:0:16000\n\
B:HIT:POISON:3d6\n\
B:HIT:POISON:3d6\n\
B:HIT:POISON:3d6\n\
B:HIT:POISON:3d6\n\
F:BASH_DOOR | DEMON | DROP_1D2 | EVIL | FEMALE | NO_SLEEP | ONLY_ITEM |\n\
F:OPEN_DOOR | NO_CONF | IM_FIRE\n\
S:1_IN_9 | BLIND | CAUSE_2 | S_DEMON\n\
D:A demon of female form with many arms, each bearing deadly weapons. \n\
\n\
N:488:Elder Tunnel Dragon\n\
G:D:U\n\
I:120:4:21d100:20:100:70\n\
W:43:1:0:13000\n\
B:CLAW:HURT:4d12\n\
B:BITE:HURT:6d12\n\
F:ATTR_MULTI | \n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | \n\
F:SMART | OPEN_DOOR | BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:EVIL | IM_ACID | IM_FIRE | IM_COLD | \n\
F:IM_ELEC | IM_POIS | NO_CONF | NO_SLEEP\n\
F:DRAGON | KILL_WALL | KILL_ITEM | KILL_BODY\n\
S:1_IN_11 | \n\
S:SCARE | \n\
S:BR_DISI\n\
D:Tunnel Dragons have briefly appeared in Alpine Mythology, \n\
D:they destroy anything that blocks their path. \n\
\n\
N:489:Elder Balaur\n\
G:D:g\n\
I:120:4:21d100:20:100:70\n\
W:43:1:0:13000\n\
B:CLAW:HURT:4d12\n\
B:BITE:HURT:6d12\n\
F:ATTR_MULTI | \n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | \n\
F:SMART | OPEN_DOOR | BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:EVIL | IM_ACID | IM_FIRE | IM_COLD | \n\
F:IM_ELEC | IM_POIS | NO_CONF | NO_SLEEP\n\
F:DRAGON \n\
S:1_IN_9 | \n\
S:SCARE | \n\
S:BR_POIS\n\
D:Balaurs are creatures similar to a dragons. \n\
D:They are quite large, has fins, feet, and multiple serpent heads (usually three, sometimes seven, or even twelve). \n\
\n\
N:490:Ethereal dragon\n\
G:D:o\n\
I:120:5:21d100:25:100:15\n\
W:43:2:0:11000\n\
B:CLAW:HURT:4d12\n\
B:BITE:HURT:6d12\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | \n\
F:INVISIBLE | \n\
F:PASS_WALL | POWERFUL | MOVE_BODY | \n\
F:DRAGON | NO_CONF | NO_SLEEP | HEAL_LITE | HEAL_DARK\n\
S:1_IN_5 | \n\
S:BLIND | CONF | \n\
S:BR_LITE | BR_DARK | BR_CONF\n\
D:A huge dragon emanating from the elemental plains, the ethereal dragon is \n\
D:a master of light and dark. Its form disappears from sight as it cloaks \n\
D:itself in unearthly shadows. \n\
\n\
N:491:Greater Lucifuge\n\
G:j:d\n\
I:120:4:12d100:20:75:80\n\
W:43:2:0:5000\n\
B:ENGULF:HURT:5d6\n\
B:ENGULF:LOSE_STR:1d6\n\
F:FORCE_SLEEP | FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_1D2 | NONLIVING | DEVIL |\n\
F:OPEN_DOOR | BASH_DOOR | POWERFUL | HURT_LITE | \n\
F:EVIL | IM_FIRE | NO_CONF | NO_SLEEP | RES_TELE | IM_DARK\n\
S:1_IN_9 | \n\
S:BLIND | CAUSE_2 | DARKNESS |\n\
S:S_DEVIL | HEAL \n\
D:Darkness incarnate, sucking light and life from the room. \n\
\n\
\n\
\n\
##### Level 44 #####\n\
\n\
\n\
\n\
N:492:Cult high priest\n\
G:p:w\n\
I:120:5:80d10:20:60:10\n\
W:44:2:0:5000\n\
B:HIT:HURT:3d5\n\
F:MALE | \n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_90 | DROP_4D2 | \n\
F:SMART | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | NO_CONF | NO_SLEEP\n\
S:1_IN_2 | \n\
S:HEAL | BLIND | HOLD | CAUSE_4 | BRAIN_SMASH | \n\
S:S_MONSTERS | S_UNDEAD | S_DEVIL\n\
D:A dark priest of the highest order. Powerful and evil, beware his many \n\
D:spells. \n\
\n\
N:493:Dreadmaster\n\
G:G:o\n\
I:120:6:12d100:20:100:10\n\
W:44:2:0:8000\n\
B:HIT:HURT:6d6\n\
B:HIT:LOSE_STR:3d4\n\
F:FORCE_SLEEP | FORCE_MAXHP | RAND_25 | \n\
F:ONLY_ITEM | DROP_1D2 | DROP_4D2 | \n\
F:SMART | TAKE_ITEM | INVISIBLE | COLD_BLOOD | PASS_WALL | \n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | NO_CONF | NO_SLEEP | RES_DARK | \n\
S:1_IN_9 | \n\
S:TELE_LEVEL | BLIND | HOLD | CONF | CAUSE_4 | DRAIN_MANA | BO_NETH | \n\
S:S_UNDEAD\n\
D:It is an unlife of power almost unequaled. An affront to existence, its \n\
D:very touch abuses and disrupts the flow of life, and its unearthly limbs, \n\
D:of purest black, crush rock and flesh with ease. \n\
\n\
N:494:The Crest of Dragons\n\
G:D:w\n\
I:120:6:20d100:20:130:70\n\
W:44:2:0:17000\n\
B:CLAW:HURT:4d10\n\
B:BITE:HURT:6d14\n\
F:UNIQUE | MALE | \n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_3D2 | DROP_4D2 | DROP_GOOD | \n\
F:BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:EVIL | DRAGON | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_3 | \n\
S:CONF | CAUSE_3 | \n\
S:BR_COLD\n\
D:'But I blind children in women's wombs, and twirl their ears round. And \n\
D:I make them deaf and mute. And I have again in my third head means of \n\
D:slipping in3. And I smite men in the limbless part of the body, and cause \n\
D:them to fall down, and foam, and grind their teeth.' \n\
\n\
##### Level 45 #####\n\
\n\
N:495:Cambion\n\
G:p:R\n\
I:120:3:25d25:20:70:20\n\
W:45:2:0:500\n\
B:HIT:HURT:5d5\n\
F:IM_POIS | IM_FIRE | IM_ELEC | IM_ACID | IM_COLD |\n\
F:NO_SLEEP | NO_FEAR | SHAPECHANGER |\n\
F:MALE | OPEN_DOOR | BASH_DOOR | DEVIL\n\
D:Most offspring of Lilitu and mortal die in the pits. This one was strong \n\
D:enough to fight his way out. \n\
\n\
N:496:Ekimmu\n\
G:W:d\n\
I:120:6:50d40:90:60:10\n\
W:45:3:0:9500\n\
B:HIT:TERRIFY:6d6\n\
B:HIT:EXP_80:4d6\n\
F:MALE | \n\
F:FORCE_MAXHP | ATTR_MULTI\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | RES_TELE\n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | MOVE_BODY | RES_NETH | SMART |\n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | HURT_LITE | NO_CONF | NO_SLEEP\n\
S:1_IN_6 |\n\
S:SCARE | HOLD | DRAIN_MANA | BR_NETH\n\
D:This ghost belongs to someone that has not been properly buried, \n\
D:he is vengeful towards the living, and that means you. \n\
D:They cause disease and inspire criminal behavior in the living. \n\
\n\
N:497:Neqa'el\n\
G:f:y\n\
I:120:6:50d40:90:60:10\n\
W:45:3:0:9500\n\
B:HIT:TERRIFY:6d6\n\
B:HIT:EXP_80:4d6\n\
F:MALE | \n\
F:FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | RES_TELE\n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | MOVE_BODY | RES_NETH | SMART | RES_LITE\n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | HURT_LITE | NO_CONF | NO_SLEEP\n\
S:1_IN_6 |\n\
S:SCARE | HOLD | DRAIN_MANA | BR_NETH\n\
D:These cat demons are ferocious, but fight only when necessary. \n\
D:When in human form it has long canine teeth, and a fierce and quick tongue. \n\
\n\
N:498:The Winged Dragon\n\
G:D:R\n\
I:120:6:20d100:20:100:70\n\
W:45:2:0:19000\n\
B:CLAW:HURT:4d10\n\
B:BITE:HURT:6d14\n\
F:UNIQUE | MALE | REFLECTING |\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_3D2 | DROP_4D2 | DROP_GOOD | \n\
F:BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:EVIL | DRAGON | IM_FIRE | NO_CONF | NO_SLEEP\n\
S:1_IN_3 | \n\
S:CONF | CAUSE_3 | \n\
S:BR_FIRE\n\
D:'In appearance like to a dragon, but having the face and hands of a man. \n\
D:And all its limbs, except the feet, were those of a dragon; and it had wings on its back. \n\
D:He is a spirit made into a god among men.' \n\
\n\
N:499:The Stormbringer\n\
G:|:D\n\
I:120:6:13d123:20:99:20\n\
W:45:2:0:13333\n\
B:WAIL:TERRIFY\n\
B:HIT:EXP_80:8d8\n\
F:CHAR_MULTI | EVIL | IM_POIS | IM_COLD | IM_FIRE | RES_NETH |\n\
F:FORCE_SLEEP | UNIQUE | FORCE_MAXHP |\n\
F:COLD_BLOOD | BASH_DOOR | NONLIVING |\n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
D:The mightiest of hellblades, a black runesword which thirsts for \n\
D:your soul. \n\
\n\
#TODO : Consider if a good creature fits in here\n\
#N:562:Ultra-elite paladin\n\
#G:p:w\n\
#I:120:8:70d10:20:100:20\n\
#W:45:2:0:1200\n\
#B:HIT:HURT:3d12\n\
#F:IM_POIS | IM_FIRE | IM_ELEC | IM_ACID | IM_COLD | GOOD |\n\
#F:RES_NETH | RES_NEXU | RES_DISE | RES_TELE\n\
#F:NO_SLEEP | NO_CONF | NO_FEAR | NO_STUN | \n\
#F:DROP_1D2 | DROP_90 | DROP_60 | FRIENDS | REFLECTING |\n\
#F:MALE | OPEN_DOOR | BASH_DOOR | FORCE_MAXHP\n\
#S:1_IN_12\n\
#S:HEAL \n\
#D:Fighting for a good cause, and they consider you an agent of evil. \n\
\n\
##### Level 46 #####\n\
\n\
N:500:Dracolich\n\
G:D:B\n\
I:120:6:35d100:25:120:30\n\
W:46:2:0:18000\n\
B:CLAW:HURT:4d12\n\
B:BITE:EXP_80:3d6\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_4D2 | DROP_GOOD | RES_TELE\n\
F:COLD_BLOOD | \n\
F:TAKE_ITEM | OPEN_DOOR | BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:EVIL | DRAGON | UNDEAD | IM_COLD | IM_POIS | \n\
F:NO_CONF | NO_SLEEP\n\
S:1_IN_6 | \n\
S:CONF | SCARE | \n\
S:BR_COLD | BR_NETH\n\
D:The skeletal form of a once-great dragon, enchanted by magic most \n\
D:perilous. Its animated form strikes with speed and drains life from its \n\
D:prey to satisfy its hunger. \n\
\n\
N:501:Greater titan\n\
G:P:v\n\
I:120:5:38d100:30:125:15\n\
W:46:3:0:13500\n\
B:HIT:CONFUSE:12d12\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_4D2 | DROP_GOOD | MOVE_BODY |\n\
F:SMART | TAKE_ITEM | OPEN_DOOR | BASH_DOOR |\n\
F:EVIL | GIANT | MALE\n\
S:1_IN_3 | \n\
S:HEAL | TELE_TO | \n\
S:S_MONSTERS\n\
D:A forty foot tall humanoid that shakes the ground as it walks. The power \n\
D:radiating from its frame shakes your courage, its hatred inspired by your \n\
D:defiance. \n\
\n\
##### Level 47 #####\n\
\n\
N:502:Barbazu\n\
G:U:G\n\
I:120:4:530d1:25:60:80\n\
W:47:2:0:9500\n\
B:HIT:POISON:4d10\n\
B:HIT:POISON:4d10\n\
B:HIT:LOSE_CON:10d2\n\
B:STING:POISON:5d5\n\
F:BASH_DOOR | DEMON | DROP_1D2 | EVIL | NO_SLEEP | ONLY_ITEM | \n\
F:OPEN_DOOR | NO_CONF | IM_POIS\n\
S:1_IN_10 | SCARE | S_DEMON\n\
D:A foul, humanoid creature with a long tail, clawed hands and feet, \n\
D: and a disgusting, wiry, snaky beard. They are the elite shock troops \n\
D: of the hells, capable of a terrifying berserk fury. \n\
\n\
#Everybody lovers th3 death mold\n\
N:503:Death mold\n\
G:m:D\n\
I:140:7:100d20:200:60:0\n\
W:47:1:0:1000\n\
B:HIT:UN_BONUS:7d7\n\
B:HIT:EXP_80:5d5\n\
F:FORCE_SLEEP | NEVER_MOVE | \n\
F:EVIL | IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
D:It is the epitome of all that is evil, in a mold. Its lifeless form draws \n\
D:power from sucking the souls of those that approach it, a nimbus of pure \n\
D:evil surrounds it. Luckily for you, it can't move. \n\
\n\
N:504:Rahab, Dragon of the Waters\n\
G:D:B\n\
I:120:6:25d110:20:100:70\n\
W:47:4:0:25000\n\
B:CLAW:HURT:4d10\n\
B:BITE:FIRE:14d6\n\
B:BITE:POISON:14d6\n\
F:UNIQUE | MALE | \n\
F:FORCE_SLEEP | FORCE_MAXHP | AQUATIC\n\
F:ONLY_ITEM | DROP_3D2 | DROP_4D2 | DROP_GOOD | DROP_90 | DROP_60 | \n\
F:BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:EVIL | DRAGON | IM_FIRE | NO_CONF | NO_SLEEP | IM_POIS \n\
S:1_IN_3 | \n\
S:CONF | CAUSE_3 | \n\
S:BR_ELEC | BR_POIS | SCARE | CAUSE_3 | CONF | BA_WATE\n\
D:# Rahab is the name of a sea-demon, a dragon of the waters, the 'ruler of the sea'. \n\
\n\
N:505:Tannin\n\
G:D:r\n\
I:120:6:28d100:20:120:70\n\
W:47:2:0:25000\n\
B:CLAW:HURT:7d12\n\
B:BITE:HURT:8d14\n\
F:UNIQUE | MALE | \n\
F:FORCE_SLEEP | FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_3D2 | DROP_4D2 | DROP_GOOD | \n\
F:BASH_DOOR | POWERFUL | MOVE_BODY |\n\
F:EVIL | DRAGON | IM_FIRE | NO_CONF | NO_SLEEP\n\
S:1_IN_5 | \n\
S:CONF | CAUSE_3 | \n\
S:BR_FIRE | \n\
S:S_DRAGON\n\
D:Tannin is the name of a demon associated either with a dragon or a serpent. \n\
D:Sometimes he is compared with Rahab, and also considered a dragon of the sea, especially associated with the Red Sea. \n\
\n\
##### Level 50 #####\n\
\n\
#Artistic freedom :\n\
#In demonology Aim (aka Aym or Haborym) is a Great Duke of Hell, very strong, and rules over \n\
D:twenty-six legions of demons. He sets cities, castles and great places on fire, makes men \n\
D:witty in all ways, and gives true answers concerning private matters. \n\
\n\
N:506:Aym\n\
G:U:r\n\
I:120:5:70d100:20:80:80\n\
W:50:2:0:10000\n\
B:HIT:FIRE:2d6\n\
B:HIT:FIRE:5d6\n\
F:FORCE_SLEEP | FORCE_MAXHP | AURA_FIRE |\n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | NONLIVING |\n\
F:OPEN_DOOR | BASH_DOOR | POWERFUL | MOVE_BODY | SMART | \n\
F:EVIL | DEMON | IM_FIRE | NO_CONF | NO_SLEEP | KILL_WALL |\n\
S:1_IN_4 | \n\
S:BR_FIRE | BO_PLAS | S_DEMON\n\
D:A minion of Haborym, he sets cities, castles and great places on fire. \n\
\n\
#He is escorted by the Aym\n\
N:507:Haborym, master of fire\n\
G:U:o\n\
I:130:6:35d100:30:120:30\n\
W:50:4:0:18000\n\
B:BUTT:FIRE:12d13\n\
B:HIT:FIRE:10d10\n\
F:UNIQUE | MALE |\n\
F:FORCE_SLEEP | FORCE_MAXHP | ESCORT | ESCORTS | ESCORT_573\n\
F:ONLY_ITEM | DROP_1D2 | DROP_4D2 | DROP_GOOD | \n\
F:BASH_DOOR | \n\
F:EVIL | IM_FIRE | IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_6 | \n\
S:SLOW | ARROW_4 | BO_MANA | BO_PLAS | BA_ELEC | \n\
S:BR_WALL\n\
D:Haborym is a very strong Duke of Hell, and rules over twenty-six legions of demons. He sets cities, \n\
D:castles and great places on fire, makes men witty in all ways, and gives true answers concerning private matters. \n\
D:He is a handsome man with three heads : serpent, man and cat. \n\
\n\
N:508:Se'irim\n\
G:q:D\n\
I:120:6:18d111:30:66:40\n\
W:50:1:0:6666\n\
B:GAZE:TERRIFY\n\
B:BUTT:HURT:6d6\n\
B:BITE:EXP_40\n\
B:BITE:LOSE_CON\n\
F:EVIL | DEMON | FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_2D2 \n\
F:OPEN_DOOR | BASH_DOOR | POWERFUL | MOVE_BODY | SMART | NONLIVING |\n\
F:NO_CONF | NO_SLEEP | HURT_LITE | IM_FIRE | IM_COLD\n\
S:1_IN_4 | \n\
S:BLIND | CONF | BRAIN_SMASH | SCARE |\n\
S:BA_NETH | FORGET | S_UNDEAD | DRAIN_MANA |\n\
S:S_DEMON | CAUSE_4 | BA_COLD\n\
D:It is a demonic creature from the lowest hell, vaguely resembling a \n\
D:large black he-goat. \n\
\n\
N:509:Nightwing\n\
G:W:D\n\
I:120:5:60d30:20:120:10\n\
W:50:4:0:6000\n\
B:TOUCH:POISON:3d5\n\
B:HIT:UN_BONUS:6d8\n\
F:FORCE_SLEEP | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | FLIGHT | \n\
F:SMART | COLD_BLOOD | OPEN_DOOR | BASH_DOOR | RES_TELE\n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | HURT_LITE | NO_CONF | NO_SLEEP\n\
S:1_IN_4 | \n\
S:BLIND | SCARE | CAUSE_4 | BRAIN_SMASH | \n\
S:BO_MANA | BO_NETH | BA_NETH | \n\
S:S_UNDEAD\n\
D:Everywhere colours seem paler and the air chiller. At the centre of the \n\
D:cold stands a mighty figure. Its wings envelop you in the chill of death \n\
D:as the nightwing reaches out to draw you into oblivion. Your muscles sag \n\
D:and your mind loses all will to fight as you stand in awe of this mighty \n\
D:being. \n\
\n\
N:510:Maulotaur\n\
G:h:u\n\
I:130:5:250d10:13:50:10\n\
W:50:2:0:4500\n\
B:BUTT:HURT:4d6\n\
B:HIT:SHATTER:5d6\n\
F:ONLY_ITEM | DROP_60 | DROP_GOOD | RES_TELE\n\
F:BASH_DOOR | STUPID | \n\
F:EVIL | IM_FIRE | FORCE_SLEEP | FORCE_MAXHP\n\
S:1_IN_5 |\n\
S:BO_FIRE | BO_PLAS | BA_FIRE\n\
D:It is a belligrent minotaur with some destructive magical arsenal, armed \n\
D:with a hammer. \n\
\n\
##### Level 51 #####\n\
\n\
N:511:Nether Fiend\n\
G:U:d\n\
I:120:5:60d10:30:100:0\n\
W:51:2:0:5000\n\
B:BITE:HURT:2d12\n\
B:CLAW:HURT:3d3\n\
F:FORCE_SLEEP | ATTR_MULTI\n\
F:FRIENDS | RES_NETH |\n\
F:DEMON | BASH_DOOR | \n\
F:ANIMAL | NO_CONF | NO_SLEEP\n\
S:1_IN_5 | \n\
S:BR_NETH\n\
D:You feel a soul-tearing chill upon viewing this beast, a ghostly form of \n\
D:darkness in the shape of a large dog. \n\
\n\
N:512:Demon Marquis\n\
G:U:b\n\
I:120:6:30d100:30:150:80\n\
W:51:2:0:17000\n\
B:CLAW:HURT:4d12\n\
B:BITE:HURT:5d14\n\
F:FORCE_SLEEP | FORCE_MAXHP | ESCORT | ESCORTS | \n\
F:ONLY_ITEM | DROP_3D2 | DROP_4D2 | DROP_GOOD | \n\
F:BASH_DOOR | SMART | \n\
F:EVIL | DEMON | NO_CONF | NO_SLEEP\n\
S:1_IN_6 | \n\
S:BLIND | CONF | SCARE | BR_DARK\n\
D:This marquis has conquered his title with force over the elder demons, \n\
D:your corpse will be just another of his trophies. He is extremely skillful in \n\
D:all forms of unarmed combat. \n\
\n\
N:513:Baphomet, master of blood\n\
G:U:D\n\
I:130:6:35d100:30:120:30\n\
W:51:4:0:18000\n\
B:BUTT:HURT:12d13\n\
B:HIT:HURT:10d10\n\
F:UNIQUE | MALE |\n\
F:FORCE_SLEEP | FORCE_MAXHP | ESCORT | ESCORTS | ESCORT_571\n\
F:ONLY_ITEM | DROP_1D2 | DROP_4D2 | DROP_GOOD | \n\
F:BASH_DOOR | \n\
F:EVIL | IM_FIRE | IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_6 | \n\
S:SLOW | ARROW_4 | BO_MANA | BO_PLAS | BA_ELEC | \n\
S:BR_WALL\n\
D:A fearsome bull-headed demon, Baphomet swings a mighty axe as he curses \n\
D:all that defy him. \n\
\n\
##### Level 52 #####\n\
\n\
N:514:Purson, Demon King\n\
G:U:r\n\
I:150:7:25d100:90:100:10\n\
W:52:3:0:45000\n\
B:HIT:LOSE_CHA:5d5\n\
B:TOUCH:EAT_ITEM\n\
B:TOUCH:LOSE_ALL:10d1\n\
B:TOUCH:EAT_GOLD\n\
F:UNIQUE | MALE | REFLECTING | ESCORT | ESCORTS | \n\
F:FORCE_SLEEP | FORCE_MAXHP | SMART |\n\
F:ONLY_ITEM | DROP_2D2 | DROP_4D2 | DROP_GOOD | DROP_GREAT |\n\
F:NO_STUN | NO_SLEEP | NO_CONF | \n\
F:IM_ELEC | IM_FIRE | IM_POIS | IM_COLD |\n\
F:COLD_BLOOD | RES_TELE\n\
S:1_IN_3 |\n\
S:CAUSE_4 | HOLD | DRAIN_MANA | SCARE | BLIND | \n\
S:S_UNDEAD | S_HI_UNDEAD | S_HI_DRAGON | S_UNIQUE | \n\
S:BA_NETH | FORGET | TRAPS | BRAIN_SMASH | TELE_AWAY\n\
D:Purson is a Great King of Hell, being served and obeyed by twenty-two legions of demons. \n\
D:He knows of hidden things, can find treasures, and tells past, present and future. He has  \n\
D:taken an aerial form and has the face of a lion, carrying a ferocious viper in his hand, \n\
D:and riding a bear. Before him you hear many trumpets sounding. \n\
\n\
##### Level 53 #####\n\
\n\
N:515:Revenant\n\
G:W:u\n\
I:125:3:830d4:30:35:5\n\
W:53:1:0:23000\n\
B:HIT:HURT:2d10\n\
B:HIT:HURT:2d10\n\
F:FORCE_SLEEP | FORCE_MAXHP | REGENERATE |\n\
F:DROP_90 | \n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | MOVE_BODY\n\
F:EVIL | UNDEAD | HURT_LITE | IM_COLD | IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_4|\n\
S:BO_FIRE\n\
D:Back from the grave, to wreak vengeance upon the living. A gaunt, tall, \n\
D:skeletal figure wearing a ruined plate mail. \n\
\n\
N:516:Demon Prince\n\
G:U:v\n\
I:130:5:45d55:30:80:5\n\
W:53:3:0:17500\n\
B:KICK:HURT:20d2\n\
B:HIT:POISON:20d1\n\
B:HIT:LOSE_ALL:15d1\n\
F:MALE | ESCORT | ESCORTS | \n\
F:FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_4D2 | EVIL |\n\
F:INVISIBLE | OPEN_DOOR | BASH_DOOR | SMART | POWERFUL | EVIL |\n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_2 | \n\
S:HEAL | MIND_BLAST | BA_NETH | S_DEMON \n\
D:This prince has conquered his title with force over the elder demons, \n\
D:your corpse will be just another of his trophies. He is extremely skillful in \n\
D:all forms of unarmed combat and controls the nether forces with disdainful ease. \n\
\n\
# King Beleth, ruler over 85 legions\n\
#  One of the governors in Hell, Beleth presides over 85 legions of demons. He rides a pale horse and is announced by a blare of trumpets.\n\
\n\
N:517:Beleth, Demon King\n\
G:U:D\n\
I:120:6:35d100:90:100:10\n\
W:53:3:0:50000\n\
B:HIT:HURT:10d10\n\
B:TOUCH:EXP_40\n\
F:UNIQUE | MALE | ESCORT | ESCORTS | \n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_3D2 | DROP_4D2 | DROP_GOOD | \n\
F:SMART | COLD_BLOOD | OPEN_DOOR | BASH_DOOR | MOVE_BODY | \n\
F:EVIL | UNDEAD | IM_ACID | IM_FIRE | IM_COLD | IM_POIS | \n\
F:HURT_LITE | NO_CONF | NO_SLEEP | RES_TELE\n\
S:1_IN_2 | \n\
S:TELE_LEVEL | BLIND | HOLD | SCARE | CAUSE_3 | CAUSE_4 | BO_MANA | \n\
S:BA_FIRE | BA_COLD | BA_NETH | \n\
S:S_UNDEAD | S_KIN\n\
D:One of the governors in Hell, Beleth presides over 85 legions of demons. He rides a pale horse and is announced by a blare of trumpets. \n\
\n\
##### Level 54 #####\n\
\n\
N:518:Elder pit hound\n\
G:Z:s\n\
I:120:5:60d15:30:100:0\n\
W:54:3:0:8000\n\
B:BITE:LOSE_WIS:2d12\n\
B:CLAW:HURT:3d3\n\
F:FORCE_SLEEP | FRIENDS | RES_NETH |\n\
F:INVISIBLE | PASS_WALL | EVIL | \n\
F:ANIMAL | NO_CONF | NO_SLEEP | DEVIL\n\
S:1_IN_5 | BLINK | TELE_TO | \n\
S:BR_NETH | BR_TIME\n\
D:Larger than a regular pit hound, and much more dangerous. \n\
\n\
N:519:Great ice wyrm\n\
G:D:w\n\
I:120:6:30d100:30:170:80\n\
W:54:2:0:20000\n\
B:CLAW:HURT:4d12\n\
B:BITE:COLD:5d14\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_3D2 | DROP_4D2 | DROP_GOOD | \n\
F:BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:EVIL | DRAGON | IM_COLD | NO_CONF | NO_SLEEP\n\
S:1_IN_6 | \n\
S:BLIND | CONF | SCARE | \n\
S:BR_COLD\n\
D:An immense dragon capable of awesome destruction. You have never felt \n\
D:such extreme cold, or witnessed such an icy stare. Begone quickly or feel \n\
D:its wrath! \n\
\n\
N:520:Celaeno, Elder Harpy\n\
G:B:r\n\
I:120:5:36d100:60:130:0\n\
W:54:3:0:40000\n\
B:BITE:POISON:12d6\n\
B:HIT:FIRE:9d12\n\
F:UNIQUE | RES_PLAS |\n\
F:FORCE_MAXHP | SMART | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | ESCORTS | EVIL | \n\
F:ANIMAL | IM_ACID | IM_FIRE | IM_ELEC | IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_3 | \n\
S:SCARE | CONF | BA_POIS | \n\
D:A harpy with the gift of prophecy, she's sees great peril in your future. \n\
\n\
N:521:Nightcrawler\n\
G:W:D\n\
I:120:5:80d60:20:160:10\n\
W:54:4:0:8100\n\
B:STING:LOSE_CON:8d8\n\
B:BITE:ACID:10d10\n\
F:FORCE_SLEEP | SMART | KILL_WALL |\n\
F:ONLY_ITEM | DROP_1D2 | DROP_2D2 | DROP_GOOD | \n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | UNDEAD | IM_FIRE | IM_COLD | IM_POIS | RES_TELE\n\
F:HURT_LITE | NO_CONF | NO_SLEEP\n\
S:1_IN_4 | \n\
S:BLIND | SCARE | BRAIN_SMASH | \n\
S:BO_MANA | BO_NETH | BA_NETH | \n\
S:BR_NETH | \n\
S:S_UNDEAD\n\
D:This intensely evil creature bears the form of a gargantuan black worm. \n\
D:Its gaping maw is a void of blackness, acid drips from its steely hide. \n\
D:It is like nothing you have ever seen before, and a terrible chill runs \n\
D:down your spine as you face it. \n\
\n\
##### Level 55 #####\n\
\n\
N:522:Hand druj\n\
G:s:D\n\
I:130:1:60d10:20:110:10\n\
W:55:4:0:12000\n\
F:FORCE_SLEEP | FORCE_MAXHP | NEVER_MOVE | NEVER_BLOW | RES_TELE\n\
F:SMART | COLD_BLOOD | \n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR | FLIGHT\n\
S:1_IN_1 | \n\
S:TELE_AWAY | BLIND | CONF | SCARE | CAUSE_3 | FORGET | DARKNESS\n\
D:A skeletal hand floating in the air, motionless except for its flexing \n\
D:fingers. \n\
\n\
N:523:Eye druj\n\
G:s:D\n\
I:130:1:10d100:20:90:10\n\
W:55:4:0:24000\n\
B:GAZE:EXP_80\n\
B:GAZE:EXP_80\n\
F:FORCE_SLEEP | FORCE_MAXHP | NEVER_MOVE | RES_TELE\n\
F:SMART | COLD_BLOOD | FLIGHT | \n\
F:EVIL | UNDEAD | \n\
F:IM_FIRE | IM_COLD | IM_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_1 | \n\
S:BO_MANA | BO_NETH | BA_NETH | \n\
S:S_UNDEAD\n\
D:A bloodshot eyeball floating in the air, you'd be forgiven for assuming it \n\
D:harmless. \n\
\n\
N:524:Skull druj\n\
G:s:D\n\
I:130:3:14d100:20:120:10\n\
W:55:4:0:25000\n\
B:BITE:EXP_80:4d4\n\
B:BITE:PARALYZE:4d4\n\
B:BITE:LOSE_INT:4d4\n\
B:BITE:LOSE_WIS:4d4\n\
F:FORCE_SLEEP | FORCE_MAXHP | NEVER_MOVE | RES_TELE\n\
F:SMART | COLD_BLOOD | \n\
F:EVIL | UNDEAD | \n\
F:IM_FIRE | IM_COLD | IM_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_1 | \n\
S:SLOW | CAUSE_4 | MIND_BLAST | BRAIN_SMASH | TRAPS | BO_PLAS | \n\
S:BO_NETH | BA_WATE | \n\
S:S_UNDEAD\n\
D:A glowing skull possessed by sorcerous power. It need not move, but \n\
D:merely blast you with mighty magic. \n\
\n\
N:525:Fallen Archon\n\
G:A:v\n\
I:140:3:32d20:100:80:0\n\
W:55:1:0:4000\n\
B:HIT:HURT:4d6\n\
B:HIT:HURT:5d6\n\
F:FALLEN_ANGEL | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR | GOOD\n\
S:1_IN_6 | \n\
S:BR_LITE\n\
D:These female angels have been banished in hell for fraternizing \n\
D:with humans. They have not turned to evil. \n\
\n\
#Yah, these might need reconsideration\n\
\n\
N:526:Lernean offspring\n\
G:M:B\n\
I:120:10:45d100:20:140:20\n\
W:55:1:0:5000\n\
B:BITE:POISON:4d6\n\
B:BITE:FIRE:5d6\n\
F:FORCE_SLEEP |  \n\
F:ONLY_GOLD | DROP_4D2 | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:ANIMAL |\n\
S:1_IN_3 | \n\
S:SCARE | BR_FIRE | BR_POIS | S_HYDRA\n\
D:Offspring of the Lernean legend, you realize you really dont \n\
D:want to fight it's twelve headed primogenitor. \n\
\n\
N:527:The Lernean Hydra\n\
G:M:G\n\
I:120:10:45d100:20:140:20\n\
W:55:2:0:20000\n\
B:BITE:POISON:8d6\n\
B:BITE:FIRE:12d6\n\
F:UNIQUE | \n\
F:FORCE_SLEEP | FORCE_MAXHP | SMART | ESCORT | ESCORTS | ESCORT_594\n\
F:ONLY_GOLD | DROP_3D2 | DROP_4D2 | \n\
F:OPEN_DOOR | BASH_DOOR | KILL_BODY | POWERFUL | \n\
F:ANIMAL | IM_FIRE | IM_POIS | \n\
F:NO_CONF | NO_SLEEP\n\
S:1_IN_3 | \n\
S:SCARE | \n\
S:BO_FIRE | BO_PLAS | BA_FIRE | BA_POIS | \n\
S:BR_FIRE | BR_POIS | \n\
S:S_HYDRA\n\
D:A massive legendary hydra. It has twelve powerful heads. Its many eyes \n\
D:stare at you as clouds of smoke and poisonous vapour rise from its \n\
D:seething form. It has retreated from the mortal realm into Inferno. \n\
\n\
N:528:Nahemah\n\
G:V:D\n\
I:130:6:40d100:20:145:10\n\
W:55:1:0:23000\n\
B:BITE:HURT:5d8\n\
B:BITE:EXP_80:6d6\n\
B:HIT:CONFUSE:6d6\n\
B:HIT:CONFUSE:6d6\n\
F:UNIQUE | FEMALE | \n\
F:FORCE_SLEEP | FORCE_MAXHP | SMART |\n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_GOOD | \n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | REGENERATE | DEMON | FALLEN_ANGEL\n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | HURT_LITE | NO_CONF | NO_SLEEP | RES_TELE\n\
S:1_IN_3 | \n\
S:BLIND | HOLD | SCARE | CAUSE_3 | CAUSE_4 | DRAIN_MANA | \n\
S:BRAIN_SMASH | BA_NETH | S_KIN\n\
D:She is a succubus and a fallen angel, she is said to have engaged, like Lilith, in intercourse with Adam. \n\
\n\
N:529:Hell wyrm\n\
G:D:r\n\
I:120:6:54d100:40:170:40\n\
W:55:2:0:23000\n\
B:CLAW:HURT:4d12\n\
B:BITE:HURT:5d14\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_GOOD | \n\
F:BASH_DOOR | POWERFUL | MOVE_BODY | AURA_FIRE |\n\
F:EVIL | DRAGON | IM_FIRE | NO_CONF | NO_SLEEP\n\
S:1_IN_6 | \n\
S:BLIND | CONF | SCARE | \n\
S:BR_FIRE\n\
D:A vast dragon of immense power. Fire leaps continuously from its huge \n\
D:form. The air around it scalds you. Its slightest glance burns you, and \n\
D:you truly realize how insignificant you are. \n\
\n\
#Astaroth (also Ashtaroth, Astarot, and Asteroth) is a Grand Duke of Hell; his main assistants are four demons called Aamon, Pruslas, Barbatos and \n\
Rashaverak.\n\
N:530:Asteroth\n\
G:A:b\n\
I:120:5:55d95:20:150:10\n\
W:55:1:0:23000\n\
B:CRUSH:HURT:14d8\n\
B:BITE:EXP_80:6d6\n\
F:UNIQUE |\n\
F:FORCE_SLEEP | FORCE_MAXHP | SMART | AURA_ELEC | FEMALE | ESCORT | ESCORTS | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_GOOD | FALLEN_ANGEL\n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | POWERFUL | SMART | NONLIVING |\n\
F:EVIL | IM_COLD | IM_POIS | HURT_LITE | NO_CONF | NO_SLEEP | RES_TELE | ATTR_MULTI\n\
S:1_IN_3 | \n\
S:BLIND | HOLD | SCARE | CAUSE_4 | DRAIN_MANA | \n\
S:BRAIN_SMASH | BA_WATE | S_DEVIL | HASTE |\n\
S:TPORT | TELE_AWAY | TELE_TO | HEAL | BR_DARK | BR_NETH\n\
D:The Treasurer of Hell. This devil was once a Seraph (Seraphim) but now carries a \n\
D:viper in his hand while riding backwards on a dragon. Astaroth also serves \n\
D:as a diabolical coach, giving pep talks to the newer demons when they lose heart \n\
D:and spurring them on to greater evil. He inspires sloth and idleness. \n\
\n\
##### Level 56 #####\n\
\n\
N:531:Nergal\n\
G:A:D\n\
I:130:6:55d99:20:120:20\n\
W:56:2:0:20000\n\
B:CRUSH:ACID:10d6\n\
B:CRUSH:COLD:10d6\n\
B:CRUSH:HURT:16d6\n\
F:UNIQUE | FALLEN_ANGEL | MALE | ESCORT | ESCORTS | \n\
F:FORCE_SLEEP | FORCE_MAXHP | SMART | DEVIL | \n\
F:ONLY_ITEM | DROP_1D2 | DROP_4D2 | DROP_GOOD | \n\
F:REGENERATE | ONLY_ITEM | KILL_ITEM | DROP_2D2 | DROP_90 | DROP_60\n\
F:BASH_DOOR | EVIL | NO_CONF | NO_SLEEP | KILL_BODY\n\
F:FORCE_MAXHP | FORCE_SLEEP | HURT_LITE | POWERFUL | RES_NETH | NONLIVING |\n\
F:IM_ACID | IM_FIRE | RES_PLAS | IM_POIS | IM_COLD | IM_ELEC | RES_TELE\n\
S:1_IN_5 |\n\
S:BRAIN_SMASH | MIND_BLAST | HASTE | TPORT |\n\
S:S_DEVIL | S_UNDEAD | S_HI_UNDEAD | S_KIN |\n\
S:BR_DARK | BR_SLIM | BR_ACID | BR_POIS\n\
D:He's in charge of hell's inquisition - destroying any devils who get out \n\
D:of line. \n\
\n\
N:532:Vinea\n\
G:A:D\n\
I:120:5:100d10:20:90:20\n\
W:56:2:0:2300\n\
B:CRUSH:SHATTER:3d11\n\
B:TOUCH:LOSE_CON:1d2\n\
F:IM_FIRE | RES_PLAS | IM_COLD | IM_POIS | RES_TELE\n\
F:UNIQUE | FORCE_MAXHP | DEVIL | ESCORT | ESCORTS | \n\
F:KILL_WALL | ONLY_GOLD | DROP_4D2 | DROP_2D2 | SMART\n\
F:EVIL\n\
S:1_IN_5 | \n\
S:SCARE | CONF | HOLD | S_DEVIL | \n\
S:MIND_BLAST | HEAL | HASTE | FORGET | BRAIN_SMASH | BR_ELEC | BA_WATE\n\
D:Vinea is an Earl and also a King of Hell, commanding 36 legions of demons. He can tell present, past and future, \n\
D:discover witches and hidden things, create storms and make the water rough by means of them, bring down walls and build towers. \n\
D:This demon is portrayed as a lion holding a snake in his hand and riding a black horse. \n\
\n\
N:533:Hauras\n\
G:A:R\n\
I:130:8:55d100:40:160:10\n\
W:56:2:0:25000\n\
B:HIT:FIRE:9d12\n\
F:UNIQUE | NO_FEAR | AURA_FIRE | REFLECTING | ESCORT | ESCORTS | \n\
F:FORCE_SLEEP | FORCE_MAXHP | FALLEN_ANGEL | MALE |\n\
F:ONLY_ITEM | DROP_3D2 | DROP_4D2 | DROP_GOOD | \n\
F:SMART | TAKE_ITEM | OPEN_DOOR | BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | RES_TELE\n\
S:1_IN_2 | \n\
S:TELE_TO | BLIND | \n\
S:BO_FIRE | BO_MANA | BA_FIRE | \n\
S:BR_FIRE | \n\
D:Formerly an angel of fire, he's lost his grace but not his heat. \n\
\n\
##### Level 57 #####\n\
\n\
N:534:Baal\n\
G:A:G\n\
I:130:8:60d100:40:170:10\n\
W:57:2:0:30000\n\
B:TOUCH:EXP_80\n\
B:HIT:BLIND:10d5\n\
B:HIT:HURT:10d10\n\
F:UNIQUE | RES_NETH | NO_FEAR | MALE | ESCORT | ESCORTS | \n\
F:FORCE_SLEEP | FORCE_MAXHP | SMART | REFLECTING | NONLIVING |\n\
F:ONLY_ITEM | DROP_3D2 | DROP_4D2 | DROP_GOOD | AURA_FIRE | NEVER_MOVE |\n\
F:SMART | TAKE_ITEM | OPEN_DOOR | BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | FALLEN_ANGEL\n\
S:1_IN_2 | \n\
S:TELE_TO | BLIND | BLINK | TPORT |\n\
S:BO_MANA | BO_NETH | BA_NETH |\n\
S:BR_NETH | BO_FIRE | BR_FIRE | BA_FIRE\n\
D:The Second Chief of Staff of the Abyss, Baal is the patron devil of idleness. He commands 60 or 70 legions of demons and resides in the \n\
D:eastern region of Hell. He shows himself as a pudgy creature with the arms of a spider and three heads: a toad, a cat, and a man. \n\
D:He seems to have a large aresenal of spells at his disposal... \n\
\n\
##### Level 58 #####\n\
\n\
N:535:Samael\n\
G:A:W\n\
I:120:7:75d100:20:125:70\n\
W:58:2:0:30000\n\
B:CLAW:HURT:5d12\n\
B:BITE:HURT:10d14\n\
F:UNIQUE | MALE | ESCORT | ESCORTS | \n\
F:FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_3D2 | DROP_4D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:FALLEN_ANGEL | IM_ACID | IM_FIRE | EVIL\n\
F:NO_CONF | NO_SLEEP | SMART\n\
S:1_IN_2 | \n\
S:BLIND | CONF | BR_POIS | \n\
D:An angelic figure who is accuser, seducer, and destroyer. \n\
D:His name stands for Venom of God. \n\
\n\
##### Level 59 #####\n\
\n\
N:536:Nightwalker\n\
G:W:D\n\
I:130:6:50d65:20:175:10\n\
W:59:4:0:15000\n\
B:HIT:UN_BONUS:10d10\n\
B:HIT:UN_BONUS:7d7\n\
F:FORCE_SLEEP |\n\
F:ONLY_ITEM | DROP_4D2 | DROP_GOOD | \n\
F:SMART | COLD_BLOOD | OPEN_DOOR | BASH_DOOR | RES_TELE\n\
F:EVIL | UNDEAD | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | \n\
F:HURT_LITE | NO_CONF | NO_SLEEP\n\
S:1_IN_4 | \n\
S:BLIND | SCARE | BRAIN_SMASH | \n\
S:BO_MANA | BO_NETH | BA_NETH | \n\
S:S_UNDEAD\n\
D:A huge giant garbed in black, more massive than a titan and stronger than \n\
D:a dragon. With terrible blows, it breaks your armour from your back, \n\
D:leaving you defenseless against its evil wrath. It can smell your fear, \n\
D:and you in turn smell the awful stench of death as this ghastly figure \n\
D:strides towards you menacingly. \n\
\n\
N:537:Belphegor\n\
G:A:v\n\
I:130:8:75d100:40:180:10\n\
W:59:2:0:35000\n\
B:HIT:UN_BONUS:6d8\n\
B:HIT:ACID:4d6\n\
B:HIT:HURT:10d10\n\
F:UNIQUE | FORCE_MAXHP | NO_FEAR | FEMALE |\n\
F:FORCE_SLEEP | FORCE_MAXHP | SMART | AURA_ELEC | REFLECTING | RES_TELE |\n\
F:ESCORT | FALLEN_ANGEL | NONLIVING |\n\
F:ONLY_ITEM | DROP_3D2 | DROP_4D2 | DROP_GOOD | \n\
F:SMART | TAKE_ITEM | OPEN_DOOR | BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS\n\
S:1_IN_2 | \n\
S:TELE_TO | BLIND | BO_MANA | \n\
S:S_DEVIL\n\
D:She's a temptress who lures men to their doom. \n\
\n\
##### Level 60 #####\n\
\n\
#TODO : This used to be a drop chosen, should find another one\n\
\n\
N:538:Paimon, Demon King\n\
G:U:v\n\
I:120:5:50d100:100:100:0\n\
W:60:1:0:35000\n\
B:HIT:UN_BONUS:6d8\n\
B:HIT:HURT:5d5\n\
F:UNIQUE | FEMALE | MALE | ESCORT | ESCORTS | \n\
F:FORCE_MAXHP | REFLECTING | RES_TELE\n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_GOOD |\n\
F:SMART | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_FIRE | IM_COLD | DEMON | \n\
F:IM_ELEC | IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_2 | \n\
S:HEAL | HASTE | TELE_AWAY | BLIND | CONF | MIND_BLAST |\n\
S:BA_WATE | \n\
S:S_UNDEAD | S_DEMON | S_DRAGON\n\
D:A high-ranking devil, Paimon takes the form of a young woman wearing a crown \n\
D:and riding a camel. He is the Master of Ceremonies in Hell and commands 200 \n\
D:legions of fiends. He supplants mortal thoughts with his own. \n\
\n\
##### Level 62 #####\n\
\n\
N:539:Shadowlord\n\
G:G:b\n\
I:120:5:30d100:20:150:10\n\
W:62:3:0:22500\n\
B:HIT:EXP_40:6d6\n\
B:HIT:LOSE_STR:4d6\n\
B:GAZE:TERRIFY:4d6\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | \n\
F:INVISIBLE | COLD_BLOOD | TAKE_ITEM | PASS_WALL | \n\
F:EVIL | UNDEAD |\n\
F:IM_COLD | IM_POIS | NO_CONF | NO_SLEEP | RES_TELE\n\
S:1_IN_3 |\n\
S:HOLD | DRAIN_MANA | BLIND | S_UNDEAD | CONF |\n\
S:SCARE | TELE_TO | TPORT | BRAIN_SMASH |\n\
S:BA_NETH | DARKNESS | SHRIEK\n\
D:An aura of hatred, cowardice and falsehood surrounds you as this \n\
D:cloaked figure floats towards you. \n\
\n\
##### Level 64 #####\n\
\n\
N:540:Osyluth\n\
G:U:W\n\
I:130:6:1750d1:20:75:80\n\
W:64:2:0:16000\n\
B:BITE:POISON:8d8\n\
B:HIT:LOSE_CHA:6d6\n\
B:HIT:POISON:6d6\n\
B:STING:LOSE_STR:5d5\n\
F:BASH_DOOR | DEMON | DROP_3D2 | DROP_GOOD | EVIL | INVISIBLE | MOVE_BODY | \n\
F:NO_SLEEP | ONLY_ITEM | OPEN_DOOR | IM_ACID | IM_COLD | NO_CONF | IM_ELEC |\n\
F:IM_FIRE | IM_POIS\n\
S:1_IN_6 | BA_COLD | BA_ELEC | BO_ICEE | SCARE | S_DEMON\n\
D:It is a demon made almost entirely out of bones. It is humanoid, but with \n\
D: a large tail similar to that of a giant scorpion, and emits a foul smell \n\
D: of decay and rot. They are despised even in the hells. \n\
\n\
#The mistress of the Neqa'el is accompanied by the ... Neqa'el\n\
N:541:Mistress of the Neqa'el \n\
G:f:o\n\
I:130:8:48d100:100:200:0\n\
W:64:2:0:30500\n\
B:HIT:CONFUSE:12d12\n\
B:TOUCH:LOSE_DEX:2d12\n\
B:HIT:BLIND:10d5\n\
B:HIT:PARALYZE:15d1\n\
F:UNIQUE | FEMALE | ESCORT | ESCORTS | ESCORT_564\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ESCORT | ESCORTS | \n\
F:ONLY_ITEM | DROP_4D2 | DROP_GOOD | \n\
F:INVISIBLE | OPEN_DOOR | BASH_DOOR | \n\
F:IM_FIRE | IM_COLD | IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_3 | \n\
S:TELE_TO | HEAL | S_KIN\n\
D:She resembles a large black cat with a white spot on her breast. \n\
\n\
##### Level 65 #####\n\
\n\
N:542:Demonic Horror\n\
G:H:v\n\
I:120:5:60d30:30:100:0\n\
W:65:1:0:10000\n\
B:BITE:HURT:2d12\n\
B:CLAW:HURT:3d3\n\
F:ATTR_MULTI | ATTR_ANY |\n\
F:FORCE_SLEEP | \n\
F:FRIENDS | \n\
F:BASH_DOOR | \n\
F:ANIMAL | NO_CONF | NO_SLEEP\n\
S:1_IN_5 | \n\
S:BR_DISE | \n\
D:A constantly changing form, this horror rushes towards you as if \n\
D:expecting mayhem and death ahead. It appears to have an almost kamikaze \n\
D:relish for combat. You suspect all may not be as it seems. \n\
\n\
##### Level 67 #####\n\
\n\
N:543:Allu\n\
G:U:v\n\
I:120:6:45d100:40:170:20\n\
W:67:1:0:29000\n\
B:CLAW:HURT:5d12\n\
B:BITE:HURT:8d14\n\
F:FORCE_MAXHP | RES_DISE | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_GOOD | \n\
F:BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:EVIL | DEMON | NO_CONF | NO_SLEEP\n\
S:1_IN_5 | \n\
S:BLIND | CONF | BR_DISE | \n\
D:Allu were a race of monstrous and faceless demons that destroyed \n\
D:all what they could capture. They were engendered during a man's \n\
D:sleep with Lilith or one of her demon servants. \n\
\n\
N:544:Fallen Ophanim\n\
G:A:W\n\
I:120:6:45d100:40:170:255\n\
W:67:1:0:29000\n\
B:HIT:HURT:5d12\n\
B:HIT:HURT:8d14\n\
F:FORCE_MAXHP | EVIL | FALLEN_ANGEL | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_GOOD | \n\
F:BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:NO_CONF | NO_SLEEP | SMART\n\
S:1_IN_3 | \n\
S:BLIND | CONF | SCARE | BR_SOUN | BR_DARK | \n\
D:This celestial being never sleeps, \n\
D:it used to be a guardian of God's throne. \n\
\n\
#This might be the last wyrm, balancing frost & fire, light & darkness,\n\
#or should I say alternating  ;] ( That is my evil grin )\n\
N:545:Divine Wyrm of Balance\n\
G:D:s\n\
I:120:6:49d100:40:170:255\n\
W:67:4:0:31000\n\
B:CLAW:HURT:5d12\n\
B:BITE:HURT:8d14\n\
F:ATTR_MULTI | ATTR_ANY |\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_GOOD | \n\
F:BASH_DOOR | POWERFUL | MOVE_BODY | RES_DISE |\n\
F:DRAGON | NO_CONF | NO_SLEEP\n\
S:1_IN_3 | \n\
S:BLIND | CONF | SCARE | \n\
S:BR_SOUN | BR_CHAO | BR_SHAR | BR_DISE | \n\
S:S_DRAGON | S_HI_DRAGON\n\
D:A massive dragon, it is thousands of years old and seeks to maintain the Divine Balance. \n\
D:It fears you will set in motion the Eschaton. \n\
\n\
#Go Quake ;)\n\
N:546:Demonic Shambler\n\
G:G:v\n\
I:130:5:50d100:40:150:50\n\
W:67:3:0:22500\n\
B:CLAW:HURT:3d12\n\
B:CRUSH:HURT:8d12\n\
F:FORCE_SLEEP | FORCE_MAXHP | RES_TELE\n\
F:ONLY_ITEM | DROP_1D2 | DROP_GOOD | \n\
F:BASH_DOOR | OPEN_DOOR | POWERFUL | MOVE_BODY | \n\
F:NO_CONF | NO_SLEEP | EVIL \n\
S:1_IN_3 |\n\
S:BR_ELEC\n\
D:This elemental demon is power incarnate; it looks like a huge polar bear \n\
D:with a huge gaping maw instead of a head. Lightning plays around his head, \n\
D:following its every move. \n\
\n\
##### Level 68 #####\n\
\n\
N:547:Sulphurous Great Wyrm\n\
G:D:y\n\
I:120:6:52d100:40:170:255\n\
W:68:4:0:34500\n\
B:CLAW:HURT:6d12\n\
B:BITE:HURT:9d14\n\
F:FORCE_SLEEP | FORCE_MAXHP | AURA_FIRE | AURA_ELEC | RES_TELE\n\
F:IM_FIRE | IM_ACID | IM_POIS | IM_COLD | IM_ELEC | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_GOOD |\n\
F:BASH_DOOR | POWERFUL | MOVE_BODY | SMART |\n\
F:DRAGON | NO_CONF | NO_SLEEP\n\
S:1_IN_3 |\n\
S:BR_FIRE | BR_ACID | BR_MANA\n\
D:A huge dragon whose scales shimmer in myriad hues. \n\
\n\
#N:548:TODO\n\
#G:G:R\n\
#I:130:7:65d100:20:150:10\n\
#W:68:2:0:35000\n\
#B:HIT:HURT:10d10\n\
#B:HIT:LOSE_STR:4d6\n\
#F:UNIQUE | MALE | \n\
#F:FORCE_SLEEP | FORCE_MAXHP |\n\
#F:ONLY_ITEM | DROP_3D2 | DROP_4D2 | DROP_GOOD | \n\
#F:INVISIBLE | COLD_BLOOD | PASS_WALL | \n\
#F:EVIL | UNDEAD | IM_COLD | \n\
#F:IM_POIS | NO_CONF | NO_SLEEP\n\
#S:1_IN_3 | \n\
#S:BLIND | HOLD | CONF | \n\
#S:BA_DARK | BA_NETH | \n\
#S:S_HI_UNDEAD | S_KIN\n\
#D:This huge affront to existence twists and tears at the fabric of space. \n\
#D:Darkness itself recoils from the touch of Tselakus as he leaves a trail \n\
#D:of death and destruction. Mighty claws rend reality as he \n\
#D:annihilates all in his path to your soul! \n\
\n\
##### Level 69 #####\n\
\n\
N:549:Fallen Cherub\n\
G:A:B\n\
I:130:7:60d100:40:200:255\n\
W:69:1:0:31000\n\
B:HIT:HURT:8d12\n\
B:HIT:FIRE:9d15\n\
F:FORCE_MAXHP | AURA_ELEC | EVIL | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_GOOD | \n\
F:BASH_DOOR | POWERFUL | MOVE_BODY | FALLEN_ANGEL | \n\
F:NO_CONF | NO_SLEEP | RES_TELE\n\
S:1_IN_3 |\n\
S:BR_GRAV | BR_FIRE | BR_DARK | BLIND\n\
D:Cherubim are of the highest rank in the hierarchy of angels, along with Seraphim. \n\
D:A double winged angel of the first triad, it master both light and dark and wields a large \n\
D:flaming sword. \n\
\n\
##### Level 70 #####\n\
\n\
#N:550:TODO\n\
#G:H:B\n\
#I:130:6:100d100:20:125:70\n\
#W:70:3:0:47500\n\
#B:CRUSH:ACID:8d12\n\
#B:CRUSH:FIRE:8d12\n\
#B:CRUSH:ELEC:8d12\n\
#B:CRUSH:POISON:10d14\n\
#F:ATTR_MULTI |\n\
#F:UNIQUE | AURA_FIRE | AURA_ELEC |\n\
#F:FORCE_SLEEP | FORCE_MAXHP | \n\
#F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_GOOD | DROP_GREAT |\n\
#F:OPEN_DOOR | BASH_DOOR | POWERFUL | MOVE_BODY | \n\
#F:EVIL |\n\
#F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | NO_CONF | NO_SLEEP\n\
#S:1_IN_2 | \n\
#S:BLIND | CONF | SCARE | \n\
#S:BR_ACID | BR_FIRE | BR_COLD | BR_ELEC | BR_POIS | \n\
#S:S_HI_DRAGON | S_MONSTERS\n\
#D:An elephantine horror with five trunks, each capable of breathing \n\
#D:destructive blasts of elements. \n\
\n\
##### Level 71 #####\n\
\n\
N:551:Bone golem\n\
G:g:D\n\
I:120:6:35d100:20:170:50\n\
W:71:2:0:23000\n\
B:HIT:UN_BONUS:6d8\n\
B:HIT:LOSE_STR:4d6\n\
F:FORCE_SLEEP | FORCE_MAXHP | SMART | \n\
F:ONLY_ITEM | DROP_1D2 | DROP_2D2 | DROP_GOOD | \n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | UNDEAD | IM_COLD | IM_POIS | RES_TELE\n\
F:NO_CONF | NO_SLEEP | KILL_WALL | NO_FEAR\n\
S:1_IN_3 | \n\
S:TELE_TO | BLIND | HOLD | CONF | CAUSE_3 | CAUSE_4 | DRAIN_MANA | \n\
S:BRAIN_SMASH | BA_MANA | BA_NETH | \n\
S:S_UNDEAD\n\
D:A skeletal form, black as night, constructed from the bones of its \n\
D:previous victims and inhabited by the soul of a lich of great power. \n\
\n\
##### Level 72 #####\n\
\n\
N:552:Vecna\n\
G:L:y\n\
I:130:6:50d100:20:85:50\n\
W:72:2:0:30000\n\
B:TOUCH:EXP_80\n\
B:TOUCH:UN_POWER\n\
B:TOUCH:LOSE_DEX:2d12\n\
F:UNIQUE | MALE | RES_TELE\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ESCORT |\n\
F:ONLY_ITEM | DROP_2D2 | DROP_4D2 | DROP_GOOD | \n\
F:SMART | COLD_BLOOD | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | UNDEAD | IM_COLD | \n\
F:IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_2 | \n\
S:BLINK | TELE_TO | BLIND | HOLD | CONF | SCARE | CAUSE_3 | CAUSE_4 | \n\
S:BRAIN_SMASH | TRAPS | BA_MANA | \n\
S:BO_MANA | BA_NETH | \n\
S:S_MONSTERS | S_UNDEAD | S_KIN\n\
D:He is a highly cunning, extremely magical being, spoken of in legends. \n\
D:This ancient shadow of death wilts any living thing it passes. \n\
\n\
##### Level 73 #####\n\
\n\
N:553:Eye of Cain\n\
G:e:y\n\
I:130:8:65d100:30:80:10\n\
W:73:2:0:16000\n\
B:GAZE:EXP_40:2d6\n\
B:GAZE:PARALYZE:2d6\n\
B:GAZE:UN_POWER:2d6\n\
B:GAZE:LOSE_INT:2d6\n\
F:UNIQUE | MALE | RES_TELE\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:SMART | BASH_DOOR | \n\
F:EVIL | IM_POIS | \n\
F:NO_CONF | NO_SLEEP\n\
S:1_IN_2 | \n\
S:BLIND | SLOW | CONF | SCARE | DRAIN_MANA | MIND_BLAST | FORGET | \n\
S:DARKNESS | BA_DARK | BO_ACID | BO_FIRE | BO_COLD | S_KIN\n\
D:A disembodied eye, floating in the air. His gaze seems to shred your \n\
D:soul and his spells crush your will. He is ancient, his history steeped \n\
D:in forgotten evils, his atrocities numerous and sickening. \n\
\n\
##### Level 75 #####\n\
\n\
N:554:Dommiel\n\
G:A:d\n\
I:120:7:130d100:8:160:80\n\
W:75:1:0:35000\n\
B:BITE:POISON:3d9\n\
B:BITE:LOSE_STR:3d9\n\
B:STING:POISON:2d5\n\
B:STING:LOSE_STR:2d5\n\
F:UNIQUE | RES_TELE | MALE | ESCORT | ESCORTS | \n\
F:FORCE_SLEEP | FORCE_MAXHP | FALLEN_ANGEL\n\
F:ONLY_ITEM | DROP_4D2 | DROP_GOOD | ATTR_MULTI\n\
F:SMART | BASH_DOOR | MOVE_BODY | NONLIVING |\n\
F:ANIMAL | EVIL | IM_POIS | HURT_LITE | NO_CONF | NO_SLEEP\n\
S:1_IN_3 |\n\
S:SCARE | BLIND | CONF | HOLD | BR_POIS |\n\
S:DARKNESS | BA_DARK | BR_DARK | S_DEMON | S_DEVIL\n\
D:The patron demon of terror and trembling, Dommiel is the adversary to Saint Peter and guards the gates of Hell. \n\
\n\
N:555:Nether wight\n\
G:W:B\n\
I:120:6:60d30:30:100:0\n\
W:75:4:0:10000\n\
B:BITE:HURT:2d12\n\
B:CLAW:HURT:3d3\n\
F:ATTR_MULTI | ATTR_ANY |\n\
F:FORCE_SLEEP | \n\
F:FRIENDS | RES_NETH | RES_PLAS | RES_NEXU | RES_DISE |\n\
F:BASH_DOOR | AURA_FIRE | AURA_ELEC |\n\
F:ANIMAL | IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | \n\
F:NO_CONF | NO_SLEEP\n\
S:1_IN_5 | \n\
S:BR_ACID | BR_FIRE | BR_COLD | BR_ELEC | BR_POIS | \n\
S:BR_LITE | BR_DARK | BR_SOUN | BR_CONF | BR_CHAO | BR_SHAR | \n\
S:BR_NETH | BR_DISE | BR_WALL | BR_INER | BR_TIME | \n\
S:BR_GRAV | BR_PLAS | BR_NEXU\n\
D:A dark, swirling form. It seems to be all hues and sizes and \n\
D:shapes, though the dominant form is that of a gigantic man. You feel very \n\
D:uncertain all of a sudden. \n\
\n\
N:556:Deception\n\
G:G:g\n\
I:120:6:60d30:30:100:0\n\
W:75:2:0:10000\n\
B:BITE:HURT:2d12\n\
B:CLAW:HURT:3d3\n\
F:ATTR_MULTI | ATTR_ANY |\n\
F:FORCE_SLEEP | UNIQUE\n\
F:RES_NETH | RES_PLAS | RES_NEXU | RES_DISE | ATTR_MULTI\n\
F:BASH_DOOR | AURA_FIRE | AURA_ELEC |\n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | EVIL\n\
F:NO_CONF | NO_SLEEP\n\
S:1_IN_5 | \n\
S:BR_ACID | BR_FIRE | BR_COLD | BR_ELEC | BR_POIS | \n\
S:BR_LITE | BR_DARK | BR_SOUN | BR_CONF | BR_CHAO | BR_SHAR | \n\
S:BR_NETH | BR_DISE | BR_WALL | BR_INER | BR_TIME | \n\
S:BR_GRAV | BR_PLAS | BR_NEXU\n\
D:'I am Deception, I deceive and weave snares here and there. I whet and excite heresies.' \n\
\n\
N:557:Strife\n\
G:G:g\n\
I:120:6:60d30:30:100:0\n\
W:75:2:0:10000\n\
B:BITE:HURT:2d12\n\
B:CLAW:HURT:3d3\n\
F:ATTR_MULTI | ATTR_ANY |\n\
F:FORCE_SLEEP | UNIQUE\n\
F:RES_NETH | RES_PLAS | RES_NEXU | RES_DISE | ATTR_MULTI\n\
F:BASH_DOOR | AURA_FIRE | AURA_ELEC |\n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | EVIL\n\
F:NO_CONF | NO_SLEEP\n\
S:1_IN_5 | \n\
S:BR_ACID | BR_FIRE | BR_COLD | BR_ELEC | BR_POIS | \n\
S:BR_LITE | BR_DARK | BR_SOUN | BR_CONF | BR_CHAO | BR_SHAR | \n\
S:BR_NETH | BR_DISE | BR_WALL | BR_INER | BR_TIME | \n\
S:BR_GRAV | BR_PLAS | BR_NEXU\n\
D:'I am Strife, strife of strifes. I bring timbers, stones, hangers, my weapons on the spot.' \n\
\n\
N:558:Battle\n\
G:G:g\n\
I:120:6:60d30:30:100:0\n\
W:75:2:0:10000\n\
B:BITE:HURT:2d12\n\
B:CLAW:HURT:3d3\n\
F:ATTR_MULTI | ATTR_ANY |\n\
F:FORCE_SLEEP | UNIQUE\n\
F:RES_NETH | RES_PLAS | RES_NEXU | RES_DISE | ATTR_MULTI\n\
F:BASH_DOOR | AURA_FIRE | AURA_ELEC |\n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | EVIL\n\
F:NO_CONF | NO_SLEEP\n\
S:1_IN_5 | \n\
S:BR_ACID | BR_FIRE | BR_COLD | BR_ELEC | BR_POIS | \n\
S:BR_LITE | BR_DARK | BR_SOUN | BR_CONF | BR_CHAO | BR_SHAR | \n\
S:BR_NETH | BR_DISE | BR_WALL | BR_INER | BR_TIME | \n\
S:BR_GRAV | BR_PLAS | BR_NEXU\n\
D:'I am called Klothod, which is Battle, and I cause the well-behaved to scatter and fall foul one of the other.' \n\
\n\
N:559:Jealousy\n\
G:G:g\n\
I:120:6:60d30:30:100:0\n\
W:75:2:0:10000\n\
B:BITE:HURT:2d12\n\
B:CLAW:HURT:3d3\n\
F:ATTR_MULTI | ATTR_ANY |\n\
F:FORCE_SLEEP | UNIQUE\n\
F:RES_NETH | RES_PLAS | RES_NEXU | RES_DISE | ATTR_MULTI\n\
F:BASH_DOOR | AURA_FIRE | AURA_ELEC |\n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | EVIL\n\
F:NO_CONF | NO_SLEEP\n\
S:1_IN_5 | \n\
S:BR_ACID | BR_FIRE | BR_COLD | BR_ELEC | BR_POIS | \n\
S:BR_LITE | BR_DARK | BR_SOUN | BR_CONF | BR_CHAO | BR_SHAR | \n\
S:BR_NETH | BR_DISE | BR_WALL | BR_INER | BR_TIME | \n\
S:BR_GRAV | BR_PLAS | BR_NEXU\n\
D:'I am Jealousy, I cause men to forget their sobriety and moderation. I part them and split them into parties; for Strife follows me hand in hand.' \n\
\n\
N:560:Power\n\
G:G:g\n\
I:120:6:60d30:30:100:0\n\
W:75:2:0:10000\n\
B:BITE:HURT:2d12\n\
B:CLAW:HURT:3d3\n\
F:ATTR_MULTI | ATTR_ANY |\n\
F:FORCE_SLEEP | UNIQUE\n\
F:RES_NETH | RES_PLAS | RES_NEXU | RES_DISE | ATTR_MULTI\n\
F:BASH_DOOR | AURA_FIRE | AURA_ELEC |\n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | EVIL\n\
F:NO_CONF | NO_SLEEP\n\
S:1_IN_5 | \n\
S:BR_ACID | BR_FIRE | BR_COLD | BR_ELEC | BR_POIS | \n\
S:BR_LITE | BR_DARK | BR_SOUN | BR_CONF | BR_CHAO | BR_SHAR | \n\
S:BR_NETH | BR_DISE | BR_WALL | BR_INER | BR_TIME | \n\
S:BR_GRAV | BR_PLAS | BR_NEXU\n\
D:'I am Power. By power I raise up tyrants and tear down kings. To all rebels I furnish power.' \n\
\n\
N:561:Error\n\
G:G:g\n\
I:120:6:60d30:30:100:0\n\
W:75:2:0:10000\n\
B:BITE:HURT:2d12\n\
B:CLAW:HURT:3d3\n\
F:ATTR_MULTI | ATTR_ANY |\n\
F:FORCE_SLEEP | UNIQUE\n\
F:RES_NETH | RES_PLAS | RES_NEXU | RES_DISE | ATTR_MULTI\n\
F:BASH_DOOR | AURA_FIRE | AURA_ELEC |\n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | EVIL\n\
F:NO_CONF | NO_SLEEP\n\
S:1_IN_5 | \n\
S:BR_ACID | BR_FIRE | BR_COLD | BR_ELEC | BR_POIS | \n\
S:BR_LITE | BR_DARK | BR_SOUN | BR_CONF | BR_CHAO | BR_SHAR | \n\
S:BR_NETH | BR_DISE | BR_WALL | BR_INER | BR_TIME | \n\
S:BR_GRAV | BR_PLAS | BR_NEXU\n\
D:'I am Error, O King Solomon. And I will make thee to err, as I have before made thee to err.' \n\
\n\
N:562:Envy\n\
G:U:g\n\
I:120:6:60d30:30:100:0\n\
W:75:2:0:10000\n\
B:BITE:HURT:2d12\n\
B:CLAW:HURT:3d3\n\
F:ATTR_MULTI | ATTR_ANY |\n\
F:FORCE_SLEEP | UNIQUE\n\
F:RES_NETH | RES_PLAS | RES_NEXU | RES_DISE | ATTR_MULTI\n\
F:BASH_DOOR | AURA_FIRE | AURA_ELEC |\n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | EVIL\n\
F:NO_CONF | NO_SLEEP\n\
S:1_IN_5 | \n\
S:BR_ACID | BR_FIRE | BR_COLD | BR_ELEC | BR_POIS | \n\
S:BR_LITE | BR_DARK | BR_SOUN | BR_CONF | BR_CHAO | BR_SHAR | \n\
S:BR_NETH | BR_DISE | BR_WALL | BR_INER | BR_TIME | \n\
S:BR_GRAV | BR_PLAS | BR_NEXU\n\
D:'I am called Envy. For I delight to devour heads, being desirous to secure for myself a head; but I do not eat enough, but am anxious to have such a head \n\
as thou hast.' \n\
\n\
##### Level 76 ###############\n\
\n\
N:563:Pit Fiend\n\
G:U:o\n\
I:130:7:2770d1:30:120:75\n\
W:76:2:0:18000\n\
B:BITE:LOSE_CON:5d10\n\
B:BITE:POISON:5d10\n\
B:CLAW:FIRE:6d10\n\
B:CLAW:FIRE:6d10\n\
F:BASH_DOOR | DEMON | DROP_2D2 | DROP_4D2 | DROP_GOOD | EVIL | MOVE_BODY | \n\
F:NO_SLEEP | ONLY_ITEM | OPEN_DOOR |  \n\
F:REGENERATE | NO_CONF | IM_FIRE | IM_POIS\n\
S:1_IN_5 | BA_FIRE | BR_FIRE | BR_POIS | CAUSE_4 | SCARE | S_DEVIL |\n\
D:Appearing as a giant, clawed and winged humanoid with a scaly black body \n\
D:and massive fangs dripping a foul green liquid, the Pit Fiend is a \n\
D:dreadful enemy from the lowest depths of the hells. They are often the \n\
D:commanders of vast demon armies. \n\
\n\
N:564:Hell knight\n\
G:p:s\n\
I:120:3:15d70:20:65:10\n\
W:76:1:0:9000\n\
B:HIT:HURT:10d5\n\
B:HIT:HURT:10d5\n\
B:HIT:HURT:10d5\n\
B:HIT:EXP_80\n\
F:FORCE_SLEEP | FORCE_MAXHP | SMART | IM_FIRE | IM_COLD | IM_POIS |\n\
F:ONLY_ITEM | DROP_90 | RES_NETH | RES_NEXU | RES_PLAS |\n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | EVIL\n\
S:1_IN_5 |\n\
S:BLIND | SCARE | CAUSE_3 | BA_NETH | BA_FIRE | BO_PLAS | S_MONSTERS\n\
D:It is a humanoid form dressed in armour of ancient style.  From beneath \n\
D:its helmet, eyes glow with hellfire. \n\
\n\
##### Level 77 #####\n\
\n\
N:565:Fallen Seraphim\n\
G:A:v\n\
I:120:3:10d100:60:50:60\n\
W:77:1:0:10000\n\
B:HIT:HURT:1d50\n\
F:NO_FEAR |FORCE_MAXHP\n\
F:DROP_2D2 | DROP_GOOD | ONLY_ITEM | RES_TELE |\n\
F:POWERFUL | AURA_ELEC | AURA_FIRE | FALLEN_ANGEL |\n\
F:BASH_DOOR | IM_ELEC | IM_FIRE | RES_NEXU | IM_COLD |\n\
F:IM_POIS | IM_ACID | RES_PLAS | RES_DISE | COLD_BLOOD | \n\
S:1_IN_5 |\n\
S:BR_CHAO\n\
D:Mightiest of the Angles, these ones direct their wrath towards you. \n\
\n\
N:566:Black reaver\n\
G:U:u\n\
I:120:8:70d101:90:90:90\n\
W:77:4:0:30000\n\
B:HIT:HURT:1d50\n\
F:RAND_25 | EVIL | DEMON | UNDEAD\n\
F:DROP_2D2 | DROP_GOOD | ONLY_ITEM | RES_TELE |\n\
F:IM_POIS | IM_FIRE | FORCE_SLEEP | FORCE_MAXHP | NONLIVING\n\
D:Clicking metal steps announce the arrival of this creature, \n\
D:A powerful undead warrior possessed by a major demon, it is \n\
D:unstoppable. \n\
\n\
##### Level 78 #####\n\
\n\
N:567:Klingsor, Evil Master of Magic\n\
G:p:y\n\
I:130:6:70d100:60:100:10\n\
W:78:3:0:40000\n\
B:HIT:UN_BONUS:6d8\n\
B:TOUCH:UN_POWER\n\
F:UNIQUE | MALE | POWERFUL | RES_TELE\n\
F:FORCE_SLEEP | FORCE_MAXHP | SMART | \n\
F:ONLY_ITEM | DROP_1D2 | DROP_GREAT | DROP_GOOD |\n\
F:DROP_CHOSEN |\n\
F:INVISIBLE | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | IM_FIRE | IM_COLD | \n\
F:IM_ELEC | NO_CONF | NO_SLEEP\n\
S:1_IN_2 | \n\
S:CAUSE_3 | TELE_TO | BA_FIRE | DRAIN_MANA | HOLD |\n\
S:TRAPS | BA_WATE | BO_PLAS | BA_NETH |\n\
S:BA_MANA | BA_DARK | S_HI_UNDEAD | BA_CHAO | CAIN\n\
D:Klingsor, whose hopeless effort to join the Knights of the Grail \n\
D:was thwarted, turned to black magic and became a deadly necromancer. \n\
\n\
N:568:Sargatanas\n\
G:A:r\n\
I:130:6:59d100:100:100:15\n\
W:78:1:0:35500\n\
B:HIT:HURT:10d15\n\
B:HIT:LOSE_CON:10d15\n\
F:UNIQUE | MALE | SMART | FALLEN_ANGEL | RES_TELE | ESCORT | ESCORTS | \n\
F:OPEN_DOOR | BASH_DOOR | \n\
F:FORCE_SLEEP | FORCE_MAXHP | NO_CONF | NO_SLEEP |\n\
F:DROP_4D2 | DROP_1D2 | DROP_GOOD | DROP_60 | DROP_90 | ONLY_ITEM |\n\
F:DROP_CHOSEN\n\
F:EVIL |IM_COLD | IM_POIS | IM_ACID | IM_ELEC | REGENERATE\n\
S:1_IN_4 |\n\
S:SCARE | CONF | TPORT | TELE_TO | S_MONSTER | DRAIN_MANA |\n\
S:CAUSE_3 | BO_ACID | BO_MANA | HOLD | BA_FIRE | BA_COLD |\n\
S:TRAPS | TELE_AWAY | HEAL | BRAIN_SMASH | BA_WATE | BA_NETH |\n\
S:FORGET | BO_WATE | BO_NETH | CAUSE_4 | DARKNESS |\n\
S:BO_PLAS | S_UNDEAD\n\
D:He's the Brigadeer of Infernal Spirits. He can move anywhere and \n\
D:get through any locked door. \n\
\n\
##### Level 80 #####\n\
\n\
N:569:Kunopaston\n\
G:A:W\n\
I:130:7:69d100:100:100:15\n\
W:80:1:0:38500\n\
B:HIT:HURT:13d13\n\
F:UNIQUE | MALE | SMART | FALLEN_ANGEL | RES_TELE\n\
F:OPEN_DOOR | BASH_DOOR |\n\
F:FORCE_SLEEP | FORCE_MAXHP | NO_CONF | NO_SLEEP |\n\
F:DROP_4D2 | DROP_1D2 | DROP_GOOD | DROP_60 | DROP_90 | ONLY_ITEM |\n\
F:EVIL | IM_COLD | IM_POIS | IM_ACID | IM_ELEC | IM_FIRE | REGENERATE\n\
S:1_IN_4 |\n\
S:SCARE | CONF | TPORT| HOLD | TELE_TO | DRAIN_MANA | TRAPS |\n\
S:TELE_AWAY | CAUSE_4 | S_MONSTERS | S_HI_DRAGON |\n\
S:S_DEMON | S_DEVIL | TELE_LEVEL | FORGET | HEAL\n\
D:Kunopaston , a fierce spirit of the sea, greedy of gold and silver. \n\
\n\
N:570:The Witch-King\n\
G:W:D\n\
I:130:7:60d100:90:120:10\n\
W:80:3:0:42000\n\
B:HIT:HURT:10d10\n\
B:HIT:EXP_80:5d5\n\
F:UNIQUE | MALE | RES_TELE\n\
F:FORCE_SLEEP | FORCE_MAXHP | SMART | \n\
F:ONLY_ITEM | DROP_3D2 | DROP_4D2 | DROP_GOOD | \n\
F:COLD_BLOOD | OPEN_DOOR | BASH_DOOR | MOVE_BODY | \n\
F:EVIL | UNDEAD | \n\
F:IM_COLD | IM_POIS | HURT_LITE | NO_CONF | NO_SLEEP\n\
S:1_IN_2 | \n\
S:TELE_AWAY | BLIND | HOLD | SCARE | CAUSE_3 | BRAIN_SMASH | \n\
S:BO_MANA | BA_NETH | \n\
S:S_KIN | S_HI_UNDEAD | S_HI_DRAGON | S_MONSTERS\n\
D:The Chief of the Wraiths. A fell being of devastating power. His \n\
D:spells are lethal and his combat blows crushingly hard. He moves at \n\
D:speed, and commands legions of evil to do his bidding. \n\
\n\
##### Level 82 #####\n\
\n\
N:571:Balaam\n\
G:A:B\n\
I:140:10:55d100:40:125:10\n\
W:82:2:0:32500\n\
B:CLAW:COLD:12d12\n\
B:CRUSH:HURT:12d12\n\
F:UNIQUE | ESCORT | RES_TELE | ESCORT | ESCORTS | \n\
F:FORCE_SLEEP | FORCE_MAXHP | MALE |\n\
F:ONLY_ITEM | DROP_4D2 | DROP_GOOD | AURA_ELEC |\n\
F:INVISIBLE | OPEN_DOOR | BASH_DOOR | NONLIVING |\n\
F:EVIL | DEMON | IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | \n\
F:IM_POIS | NO_CONF | NO_SLEEP | FALLEN_ANGEL\n\
S:1_IN_3 | \n\
S:BO_MANA | SCARE | BR_COLD | S_DEVIL | BO_ELEC | BA_ELEC |\n\
S:MIND_BLAST | CAUSE_4 | BA_CHAO | BA_WATE | S_HI_UNDEAD | S_KIN\n\
D:The unseen prophet. Balaam whispers advice. Whatever you do, don't \n\
D:take it. \n\
\n\
##### Level 83 #####\n\
\n\
N:572:Snake of Yig\n\
G:J:r\n\
I:120:3:48d10:25:80:30\n\
W:83:4:0:600\n\
B:BITE:POISON:3d12\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:RAND_25 | FRIENDS | AURA_FIRE |\n\
F:BASH_DOOR | MOVE_BODY | \n\
F:ANIMAL | EVIL | IM_FIRE\n\
S:1_IN_5 | \n\
S:BR_POIS\n\
D:It is a giant snake that drips with poison. \n\
\n\
##### Level 84 #####\n\
\n\
N:573:Charon\n\
G:s:B\n\
I:140:7:75d100:20:120:80\n\
W:84:2:0:45000\n\
B:GAZE:EXP_80\n\
B:TOUCH:POISON:3d5\n\
F:UNIQUE | MALE | RES_TELE\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_GOOD | DROP_GREAT |\n\
F:SMART | COLD_BLOOD | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | UNDEAD | IM_FIRE | IM_COLD | IM_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_1 | \n\
S:TELE_TO | SLOW | SCARE | CAUSE_4 | BRAIN_SMASH | \n\
S:BA_WATE | BA_NETH | S_HI_UNDEAD\n\
D:He seems to have exchanged the ferrying job with a wish to see you dead. \n\
\n\
N:574:Mephistopheles\n\
G:A:r\n\
I:140:7:30d222:20:150:50\n\
W:84:2:0:42500\n\
B:GAZE:EXP_80:1d5\n\
B:GAZE:TERRIFY:1d5\n\
B:TOUCH:FIRE:4d5\n\
B:TOUCH:UN_POWER:4d5\n\
F:MALE | UNIQUE | FALLEN_ANGEL | ESCORT | ESCORTS | \n\
F:FORCE_SLEEP | FORCE_MAXHP | OPEN_DOOR | BASH_DOOR | MOVE_BODY |\n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_GOOD | DROP_GREAT |\n\
F:IM_FIRE | RES_PLAS | RES_NETH | AURA_FIRE |\n\
F:NO_CONF | NO_SLEEP | NONLIVING | EVIL | DEMON |\n\
F:ESCORTS | IM_COLD | IM_POIS\n\
S:1_IN_3 |\n\
S:TELE_TO | SCARE | HOLD | BRAIN_SMASH |\n\
S:S_DEMON | S_HI_UNDEAD | S_UNDEAD |\n\
S:BR_FIRE | BR_NETH | S_REAVER | CAIN\n\
D:The patron prince of deceit, Mephistopheles is a smooth character with an engaging wit and a polished manner. Due to his uniquely entertaining personality, \n\
D:he is, on occasion, allowed an audience with God. Because of his dashing ways, he is often sent by the Devil to tempt modern humans to sell their souls. \n\
D:Mephistopheles is also the patron devil of trolls. \n\
\n\
N:575:Abaddon\n\
G:A:g\n\
I:130:7:85d100:50:185:20\n\
W:84:2:0:35000\n\
B:CLAW:POISON:5d10\n\
B:BITE:HURT:20d10\n\
B:CRUSH:UN_BONUS:5d12\n\
F:UNIQUE | ESCORT | ESCORTS | FALLEN_ANGEL | ESCORT | ESCORTS | \n\
F:FORCE_SLEEP | FORCE_MAXHP | RES_PLAS | RES_DISE | RES_TELE\n\
F:ONLY_ITEM | DROP_2D2 | DROP_4D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:EVIL | IM_FIRE | IM_COLD | NO_CONF | NO_SLEEP | IM_POIS\n\
S:1_IN_2 | \n\
S:BR_DISE | BR_POIS | BR_ACID\n\
D:He is the angel of the abyss, bringer of destruction. \n\
D:He commands one of hell's armies - that of pestilence and locusts. \n\
\n\
##### Level 86 #####\n\
\n\
N:576:Erebus \n\
G:v:G\n\
I:130:6:75d100:90:90:90\n\
W:86:2:0:44000\n\
B:CLAW:POISON:1d30\n\
B:CLAW:ACID:1d30\n\
B:TOUCH:UN_POWER:1d10\n\
B:CRUSH:UN_BONUS:2d33\n\
F:RAND_25 | KILL_ITEM | OPEN_DOOR | BASH_DOOR | RES_NETH |\n\
F:DROP_1D2 | DROP_2D2 | DROP_90 | ONLY_ITEM | FORCE_SLEEP | FORCE_MAXHP |\n\
F:EVIL | IM_POIS | IM_COLD | IM_ACID | IM_ELEC | RES_TELE | NONLIVING |\n\
F:POWERFUL | IM_FIRE | DEVIL\n\
S:1_IN_3\n\
S:SCARE | CONF | S_DEVIL | S_UNDEAD | DRAIN_MANA | BR_CHAO |\n\
S:BR_FIRE | TPORT | S_MONSTERS | BRAIN_SMASH | BR_NETH |\n\
S:HEAL | MIND_BLAST | BA_SLIM\n\
D:He is  a primordial god, the personification of darkness. He is the offspring of Chaos alone. \n\
D:He is the brother of Nyx and father of Aether by himself. \n\
D:With Nyx, he fathered Thanatos, Hypnos, Hemera, Moros, Charon, and the Keres. \n\
\n\
##### Level 90 #####\n\
\n\
N:577:Kaschei the Immortal\n\
G:L:v\n\
I:130:7:60d100:100:100:0\n\
W:90:3:0:45000\n\
B:HIT:UN_BONUS:6d8\n\
B:HIT:UN_BONUS:6d8\n\
B:HIT:HURT:5d5\n\
B:HIT:HURT:5d5\n\
F:UNIQUE | MALE | RES_TELE\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ESCORT | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_GOOD | DROP_GREAT | \n\
F:SMART | COLD_BLOOD | OPEN_DOOR | BASH_DOOR | \n\
F:EVIL | UNDEAD | \n\
F:IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | NO_CONF | NO_SLEEP |\n\
S:1_IN_3 | \n\
S:TPORT | BLIND | SCARE | CAUSE_4 | BRAIN_SMASH | \n\
S:BA_MANA | BO_MANA | BA_FIRE | \n\
S:S_MONSTERS | S_DEMON | S_HI_UNDEAD | CAIN\n\
D:A stench of corruption and decay surrounds this sorcerer, who has clearly \n\
D:risen from the grave to continue his foul plots and schemes. \n\
\n\
N:578:Bifrons\n\
G:A:v\n\
I:130:7:66d99:100:100:20\n\
W:90:3:0:45000\n\
B:TOUCH:HURT:40d5\n\
B:TOUCH:LOSE_CON:16d2\n\
F:UNIQUE | SMART | RES_TELE | MALE | ESCORT | ESCORTS | \n\
F:PASS_WALL | FORCE_SLEEP | FORCE_MAXHP | AURA_ELEC | AURA_FIRE |\n\
F:ONLY_ITEM | DROP_4D2 | DROP_GOOD | ATTR_MULTI | ATTR_ANY |\n\
F:SMART | COLD_BLOOD | OPEN_DOOR | BASH_DOOR | NONLIVING | FALLEN_ANGEL |\n\
F:EVIL | IM_ACID | IM_COLD | IM_ELEC | IM_POIS | IM_FIRE | NO_CONF | NO_SLEEP\n\
S:1_IN_3 |\n\
S:BO_MANA | BRAIN_SMASH | BA_MANA | S_MONSTERS | S_REAVER |\n\
S:BA_CHAO | S_DEVIL | S_HI_UNDEAD | S_HOUND | BR_MANA | BR_DISI\n\
D:The lord of unholy sorcery is a master of the art of summoning. \n\
\n\
N:579:Great Hell Wyrm\n\
G:D:y\n\
I:130:8:111d111:20:111:70\n\
W:90:4:0:47500\n\
B:CRUSH:COLD:8d12\n\
B:CRUSH:FIRE:8d12\n\
B:CRUSH:HURT:8d12\n\
B:CRUSH:HURT:10d18\n\
F:FORCE_SLEEP | FORCE_MAXHP | MOVE_BODY | AURA_FIRE | REFLECTING | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_GOOD | \n\
F:OPEN_DOOR | BASH_DOOR | POWERFUL | MOVE_BODY | HEAL_NETH | RES_DISE | IM_DARK | HURT_LITE\n\
F:DRAGON | EVIL | RES_TELE\n\
F:IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | NO_CONF | NO_SLEEP |\n\
F:RES_NETH | RES_NEXU | RES_PLAS |\n\
S:1_IN_3 | \n\
S:S_HI_DRAGON | S_DRAGON | S_KIN | \n\
S:BR_SLIM | BR_FIRE | \n\
S:BR_COLD | BR_POIS | BR_NETH | BR_LITE | BR_DARK | \n\
S:BR_CONF | \n\
S:BR_TIME | \n\
S:BR_WALL | BR_MANA | BR_DISI \n\
D:The mightiest of all Infernal dragons, it has sworn allegiance to Lucifer Monrningstar. \n\
\n\
##### Level 91 #####\n\
\n\
#KOBAL: The Dramatist and Manager of the Infernal Theater, Kobal tempts men with pretense and fraud.\n\
\n\
N:580:Kobal\n\
G:A:D\n\
I:130:7:65d99:100:100:20\n\
W:91:3:0:47500\n\
B:CRUSH:LOSE_WIS:20d5\n\
B:CRUSH:LOSE_INT:20d5\n\
B:BITE:LOSE_STR:10d2\n\
B:BITE:LOSE_CON:10d2\n\
F:UNIQUE | MALE | AURA_ELEC | RES_TELE | ESCORT | ESCORTS | \n\
F:ATTR_MULTI | FORCE_SLEEP | FORCE_MAXHP | PASS_WALL | ATTR_ANY |\n\
F:ONLY_ITEM | DROP_4D2 | DROP_GOOD | DROP_90 | NONLIVING |\n\
F:SMART | OPEN_DOOR | BASH_DOOR | REGENERATE | DEMON | FALLEN_ANGEL |\n\
F:EVIL | IM_ACID | IM_COLD | IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_3 |\n\
S:BO_MANA | BRAIN_SMASH | BA_DARK | S_MONSTERS |\n\
S:CAUSE_4 | HEAL | BR_CHAO | BR_CONF | BR_POIS | BR_SLIM |\n\
S:BA_CHAO | S_DEVIL | S_HI_UNDEAD | S_UNIQUE\n\
D:As the lord of corrupted entertainment, he manages the Infernal theater. \n\
D:He's sure to put up a spectacular fight. \n\
\n\
##### Level 92 #####\n\
\n\
N:581:Staff\n\
G:Z:d\n\
I:130:7:75d100:80:110:10\n\
W:92:2:0:40000\n\
B:CLAW:HURT:3d3\n\
B:BITE:POISON:4d4\n\
F:UNIQUE | MALE | ATTR_MULTI\n\
F:FORCE_SLEEP | FORCE_MAXHP | RAND_25 | \n\
F:ONLY_ITEM | DROP_1D2 | DROP_GOOD | AURA_FIRE |\n\
F:SMART | TAKE_ITEM | OPEN_DOOR | BASH_DOOR | MOVE_BODY | \n\
F:ANIMAL | EVIL | IM_FIRE | IM_POIS | NO_CONF | NO_SLEEP\n\
S:1_IN_4 | \n\
S:HEAL | SCARE | BRAIN_SMASH | \n\
S:BR_FIRE | \n\
S:S_HOUND\n\
D:This hound is so large and so powerful it can hold the stars of heaven back. \n\
\n\
##### Level 93 #####\n\
\n\
N:582:Choronzon\n\
G:A:s\n\
I:130:8:90d99:100:100:20\n\
W:93:3:0:49000\n\
B:CRUSH:LOSE_CON:30d4\n\
B:HIT:LOSE_STR:30d4\n\
B:TOUCH:LOSE_INT:1d50\n\
B:CRAWL:LOSE_WIS:1d50\n\
F:UNIQUE | RES_TELE | NONLIVING | MALE | ESCORT | ESCORTS | \n\
F:FORCE_SLEEP | FORCE_MAXHP | PASS_WALL | DEMON | RES_NEXU |\n\
F:FALLEN_ANGEL | ATTR_MULTI | ATTR_ANY | MALE | AURA_ELEC |\n\
F:ONLY_ITEM | DROP_4D2 | DROP_GOOD | DROP_GREAT | DROP_90 |\n\
F:SMART | OPEN_DOOR | BASH_DOOR | REGENERATE |\n\
F:EVIL | IM_ACID | IM_POIS | IM_FIRE | NO_CONF | NO_SLEEP\n\
S:1_IN_3 |\n\
S:TELE_AWAY | TELE_TO | TELE_LEVEL | TPORT | BR_NEXU | BA_CHAO |\n\
S:BA_MANA | BA_FIRE | S_MONSTERS | BRAIN_SMASH | MIND_BLAST |\n\
S:CAUSE_4 | HASTE | S_HI_UNDEAD | S_HI_DRAGON |\n\
S:S_DEVIL | S_DEMON | HEAL | S_SPIDER | S_HOUND | S_REAVER | CAIN\n\
D:Deeper than Dommiel, he is the gatekeeper of hell, with the hordes at his command. \n\
\n\
N:583:Berith\n\
G:A:s\n\
I:130:8:90d99:100:100:20\n\
W:93:3:0:49000\n\
B:CRUSH:LOSE_CON:30d4\n\
B:HIT:LOSE_STR:30d4\n\
B:TOUCH:LOSE_INT:1d50\n\
B:CRAWL:LOSE_WIS:1d50\n\
F:UNIQUE | RES_TELE | NONLIVING | MALE | ESCORT | ESCORTS | \n\
F:FORCE_SLEEP | FORCE_MAXHP | PASS_WALL | DEMON | RES_NEXU |\n\
F:FALLEN_ANGEL | ATTR_MULTI | ATTR_ANY | MALE | AURA_ELEC |\n\
F:ONLY_ITEM | DROP_4D2 | DROP_GOOD | DROP_GREAT | DROP_90 |\n\
F:SMART | OPEN_DOOR | BASH_DOOR | REGENERATE |\n\
F:EVIL | IM_ACID | IM_POIS | IM_FIRE | NO_CONF | NO_SLEEP\n\
S:1_IN_3 |\n\
S:TELE_AWAY | TELE_TO | TELE_LEVEL | TPORT | BR_NEXU | BA_CHAO |\n\
S:BA_MANA | BA_FIRE | S_MONSTERS | BRAIN_SMASH | MIND_BLAST |\n\
S:CAUSE_4 | HASTE | S_HI_UNDEAD | S_HI_DRAGON |\n\
S:S_DEVIL | S_DEMON | HEAL | S_SPIDER | S_HOUND | S_REAVER | CAIN\n\
D:Hell's Minister of Foreign Affairs, Chief Secretary, and Keeper of the Infernal Archives. He is often called upon to \n\
D:notarize pacts between humans and the Devil. A cantankerous spirit, he tempts men to be quarrelsome, contentious, and \n\
D:blasphemous. He also inspires them to commit murder. Berith is the patron devil of disobedience. \n\
\n\
#This dandified devil, the patron evil spirit behind passion, lechery, pleasure, luxury, and sensuality, runs the casinos and gambling houses in Hell.\n\
#Able to appear as the most beautiful, well-dressed young man (or woman), he tempts his victims by encouraging them to buy fancy clothes and follow \n\
#meaningless fads. He is the prince of profligates and loves to lure happily married couples into adulterous affairs, sometimes with himself. This \n\
#hapless devil prides himself on arranging ridiculously inappropriate marriages and leads humans into squandering their assets. He wreaks havoc in \n\
#convents and monasteries by seducing their inhabitants. His mother may have been human (or, according to some, she could have been Lilith), but his\n\
#father was an angel. When Asmodeus visits some poor human in his real form, he actually has three heads symbolic of lechery, those of a bull, \n\
#a ram, and a man. He also sports the feet of a rooster. He cavorts around on the back of a dragon wielding a spear. He is credited with having \n\
#invented carousels, music, dancing, drama, and, one may assume, recreational drug use. Asmodeus' adversary in Heaven is John the Baptist.\n\
#Asmodai\n\
N:584:Asmodeus\n\
G:A:B\n\
I:130:5:99d99:100:150:100\n\
W:93:3:0:50000\n\
B:CLAW:HURT:35d5\n\
B:TOUCH:ACID:35d5\n\
F:UNIQUE | RES_TELE | ESCORT | ESCORTS | \n\
F:FORCE_SLEEP | FORCE_MAXHP | KILL_WALL | DEMON | AURA_FIRE | AURA_ELEC |\n\
F:ATTR_MULTI | ESCORTS | ESCORT | POWERFUL | ATTR_ANY | NONLIVING |\n\
F:KILL_ITEM | KILL_WALL | FALLEN_ANGEL |\n\
F:ONLY_ITEM | DROP_4D2 | DROP_GOOD | DROP_90 | RES_DISE |\n\
F:STUPID | OPEN_DOOR | BASH_DOOR | REGENERATE | EMPTY_MIND |\n\
F:EVIL | IM_ACID | IM_POIS | IM_FIRE | NO_CONF | NO_SLEEP\n\
S:1_IN_5 |\n\
S:S_DEVIL | BR_CHAO | BR_DISE | BR_MANA | BA_WATE | BR_DISI\n\
D:Prince of lust, he follows his baser instincts. \n\
\n\
#Moloch in December\n\
N:585:Moloch\n\
G:U:B\n\
I:130:5:99d99:100:150:100\n\
W:93:3:0:50000\n\
B:CLAW:HURT:35d5\n\
B:TOUCH:ACID:35d5\n\
F:UNIQUE | RES_TELE | ESCORT | ESCORTS | \n\
F:FORCE_SLEEP | FORCE_MAXHP | KILL_WALL | DEMON | AURA_FIRE | AURA_ELEC |\n\
F:ATTR_MULTI | ESCORTS | ESCORT | POWERFUL | ATTR_ANY | NONLIVING |\n\
F:KILL_ITEM | KILL_WALL | \n\
F:ONLY_ITEM | DROP_4D2 | DROP_GOOD | DROP_90 | RES_DISE |\n\
F:STUPID | OPEN_DOOR | BASH_DOOR | REGENERATE | EMPTY_MIND |\n\
F:EVIL | IM_ACID | IM_POIS | IM_FIRE | NO_CONF | NO_SLEEP\n\
S:1_IN_5 |\n\
S:S_DEVIL | BR_CHAO | BR_DISE | BR_MANA | BA_WATE | BR_DISI\n\
D:A terrifying devil, Moloch serves as the Chief of the Army in Hell. He was once a Canaanite deity, worshipped by early Semites who \n\
D:sacrificed their firstborn children in the fires of his temple located just outside Jerusalem. Moloch's face and hands are smeared \n\
D:with the blood of murdered children and the tears shed by their grieving mothers. \n\
\n\
##### Level 95 #####\n\
\n\
#Leviathan in February\n\
N:586:Leviathan, Admiral of the Devil's Navy\n\
G:U:B\n\
I:130:8:80d100:100:140:0\n\
W:95:1:0:43000\n\
B:HIT:FIRE:9d12\n\
B:CRUSH:HURT:8d12\n\
B:TOUCH:UN_POWER\n\
F:UNIQUE | MALE | \n\
F:FORCE_SLEEP | FORCE_MAXHP | AQUATIC\n\
F:ESCORT | ESCORTS | KILL_WALL | AURA_FIRE | NONLIVING |\n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_GOOD | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:EVIL | DEMON | DRAGON | \n\
F:IM_FIRE | IM_ELEC | NO_CONF | NO_SLEEP\n\
S:1_IN_3 | \n\
S:BLIND | CONF | SCARE | \n\
S:BR_FIRE | S_KIN |\n\
S:S_DEMON | S_HI_UNDEAD | S_REAVER | BR_DISI\n\
D:It is depicted in the Bible (Isaiah 27:I) as the primordial she- (or he-) dragon of the sea. \n\
D:Created with Behemoth on the fifth day, these two gargantuan devils devour damned souls. \n\
D:Leviathan has hordes of water demons, sprites, and nymphs to do his bidding. \n\
D:On Judgement Day, this sea monster will swallow all but the saved. \n\
\n\
N:587:Behemoth, the Cupbearer\n\
G:U:B\n\
I:130:8:80d100:100:140:0\n\
W:95:1:0:43000\n\
B:HIT:FIRE:9d12\n\
B:CRUSH:HURT:8d12\n\
B:TOUCH:UN_POWER\n\
F:UNIQUE | MALE | \n\
F:FORCE_SLEEP | FORCE_MAXHP | AQUATIC | \n\
F:ESCORT | ESCORTS | KILL_WALL | AURA_FIRE | NONLIVING |\n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_GOOD | DROP_GREAT | \n\
F:OPEN_DOOR | BASH_DOOR | POWERFUL | MOVE_BODY | \n\
F:EVIL | DEMON | DRAGON | \n\
F:IM_FIRE | IM_ELEC | NO_CONF | NO_SLEEP\n\
S:1_IN_3 | \n\
S:BLIND | CONF | SCARE | \n\
S:BR_FIRE | S_KIN |\n\
S:S_DEMON | S_HI_UNDEAD | S_REAVER | BR_DISI\n\
D:The Devil's Cupbearer, Behemoth is the patron devil of despair and gluttony. \n\
D:He appears in the shape of a monstrous elephant with two bear's feet. \n\
D:He will also take the form of a crocodile, whale, or hippopotamus and \n\
D:is related to fellow demon, Leviathan. Behemoth presides over the feasts in Hell and \n\
D:is responsible for dishing up food and wine for the Devil. He entertains Hell's \n\
D:motley crew with song and plays the role of night watchman, as he is often \n\
D:awake all night. Unsurprisingly, he creates chaos and havoc in the lives of men. \n\
\n\
##### Level 96 #####\n\
\n\
N:588:Belial\n\
G:A:g\n\
I:130:6:100d100:100:140:100\n\
W:96:2:0:62500\n\
B:CRUSH:HURT:50d4\n\
B:CLAW:UN_POWER:15d2\n\
B:CLAW:UN_BONUS:15d2\n\
B:TOUCH:POISON:1d100\n\
F:UNIQUE | DEMON | NONLIVING | MALE | ESCORT | ESCORTS | \n\
F:FORCE_SLEEP | FORCE_MAXHP | ESCORT | ESCORTS | SMART | RES_PLAS | RES_NEXU |\n\
F:OPEN_DOOR | BASH_DOOR | POWERFUL | MOVE_BODY | REGENERATE | RES_NETH |\n\
F:ONLY_ITEM | DROP_2D2 | DROP_4D2 | DROP_1D2 | DROP_GOOD |\n\
F:DROP_GREAT | RES_DISE | RES_TELE | FALLEN_ANGEL\n\
F:EVIL | DEMON | IM_FIRE | IM_POIS | IM_ELEC | IM_ACID | NO_CONF | NO_SLEEP |\n\
S:1_IN_3\n\
S:TPORT | SCARE | BLIND | MIND_BLAST | BRAIN_SMASH | DRAIN_MANA |\n\
S:BR_POIS | BR_ACID | BR_FIRE | CONF | DARKNESS | FORGET | S_HI_UNDEAD |\n\
S:BR_SLIM | BR_NETH | BR_CHAO | BR_DISE | BR_DARK | BR_PLAS | BR_CONF\n\
S:BR_NEXU | S_DEVIL | S_REAVER | S_KIN | BR_DISI | CAIN\n\
D:The patron devil of arrogance, lies, and deceit, Belial is one of the great fallen angels. A prince in the angelic Order of Virtues, \n\
D:he fell directly after Lucifer. A great orator, he may appear briefly as a gentleman, but his true nature always reveals itself because \n\
D:the image is hollow. Belial tempts men to disloyalty, gossip, and rebellion, and instigates women to dress in finery, gossip in church, \n\
D:and indulge their children. \n\
\n\
##### Level 98 #####\n\
\n\
N:589:Beelzebub\n\
G:A:o\n\
I:130:8:105d100:100:160:0\n\
W:98:1:0:50000\n\
B:HIT:UN_BONUS:10d12\n\
B:TOUCH:UN_POWER\n\
F:UNIQUE | MALE | REFLECTING | ESCORT | ESCORTS | \n\
F:FORCE_SLEEP | FORCE_MAXHP |\n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_GOOD | DROP_GREAT |\n\
F:DROP_CHOSEN |\n\
F:SMART | OPEN_DOOR | BASH_DOOR | MOVE_BODY | REGENERATE | \n\
F:EVIL | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR | RES_TELE\n\
S:1_IN_2 | \n\
S:TPORT | TELE_LEVEL | BLIND | CONF | SCARE | CAUSE_4 | \n\
S:BRAIN_SMASH | FORGET |\n\
S:BO_ICEE | BO_MANA | BO_PLAS | \n\
S:BA_MANA | BA_FIRE | BA_WATE | BA_NETH | BA_DARK | \n\
S:S_MONSTERS | S_DEMON | S_HI_UNDEAD | S_HI_DRAGON | CAIN\n\
D:The Devil's Chief of Staff and second only to Lucifer in the organization of Hell. Known to many as the Prince of Death, \n\
D:Beelzebub was Lucifer's closest companion and a fellow Seraphim angel in Heaven. Beelzebub manifests himself in the shape \n\
D:of a fly and presides over the Order of the Fly. He tempts men with the grandparents of all sin, envy and pride. He can send \n\
D:plagues of flies. Heresy is attributed to Beelzebub, who blows his bellows into the ears of heretics. He is the master of the pit spawns, \n\
D:last of the Fallen Angels, lord of Tartarus, adjured by Eleéth. \n\
\n\
##### Level 99 ##### N:575:Lilith\n\
\n\
N:590:Lilith\n\
G:p:v\n\
I:145:10:99d111:100:165:0\n\
W:99:1:0:65000\n\
B:HIT:UN_BONUS:12d12\n\
B:HIT:UN_POWER:12d12\n\
B:HIT:CONFUSE:10d2\n\
B:HIT:BLIND:3d2\n\
F:UNIQUE | ALWAYS_GUARD | FEMALE | ESCORT | ESCORTS | \n\
F:ATTR_MULTI | RES_TELE | FALLEN_ANGEL\n\
F:FORCE_SLEEP | FORCE_MAXHP | \n\
F:ONLY_ITEM | DROP_2D2 | DROP_3D2 | DROP_4D2 | DROP_GOOD | DROP_GREAT |\n\
F:DROP_CHOSEN | REFLECTING | AURA_FIRE | AURA_ELEC |\n\
F:SMART | OPEN_DOOR | BASH_DOOR | MOVE_BODY | REGENERATE | \n\
F:EVIL | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | POWERFUL |\n\
F:NO_CONF | NO_SLEEP | NO_FEAR\n\
S:1_IN_2 | \n\
S:TPORT | SCARE | BLIND | S_DEMON | S_DEVIL | S_MONSTERS |\n\
S:TELE_TO | CONF | BO_MANA | BA_FIRE | BRAIN_SMASH | BA_NETH |\n\
S:BO_ICEE | CAUSE_4 | BA_WATE | BO_PLAS | TELE_LEVEL | TELE_AWAY |\n\
S:FORGET | DARKNESS | BA_MANA | S_HI_DRAGON | S_HI_UNDEAD | BA_CHAO\n\
S:S_FALLEN | S_REAVER | CAIN\n\
D:Vampiric Queen of Hell, Consort of Lucifer, First Woman. \n\
D:She has it all and she's not about to let you take it from her. \n\
\n\
##### Level 100 ##### N:576:Lucifer Morningstar\n\
\n\
N:591:Lucifer Morningstar\n\
G:A:d\n\
I:155:10:200d150:111:175:0\n\
W:100:1:0:66666\n\
B:CRUSH:SHATTER:22d10\n\
B:TOUCH:LOSE_ALL:10d12\n\
B:TOUCH:UN_POWER\n\
F:UNIQUE | ALWAYS_GUARD| ATTR_MULTI | ATTR_ANY | ESCORT | ESCORTS | \n\
F:FORCE_SLEEP | FORCE_MAXHP | MALE |\n\
F:REFLECTING | AURA_FIRE | AURA_ELEC | FALLEN_ANGEL\n\
F:ONLY_ITEM | DROP_1D2 | DROP_2D2 | DROP_3D2 | DROP_4D2 | \n\
F:DROP_GOOD | DROP_GREAT | DROP_CHOSEN | RES_NETH |\n\
F:SMART | KILL_WALL | KILL_BODY | POWERFUL |\n\
F:REGENERATE | NONLIVING |\n\
F:EVIL | IM_ACID | IM_FIRE | IM_COLD | IM_ELEC | IM_POIS | \n\
F:NO_CONF | NO_SLEEP | NO_FEAR | NO_STUN | RES_TELE\n\
S:1_IN_3 | \n\
S:S_MONSTERS | BR_CHAO | BA_CHAO | BRAIN_SMASH | S_REAVER |\n\
S:BR_NETH | HASTE | BR_MANA | S_HI_UNDEAD | S_HI_DRAGON | S_UNIQUE |\n\
S:S_FALLEN | BR_SLIM | BR_POIS | BR_DISI | CAIN | S_DEVIL\n\
D:The lord of hell himself. Most beautiful of the angels, he is \n\
D:devillishly handsome. His power is second only to God Himself.";
