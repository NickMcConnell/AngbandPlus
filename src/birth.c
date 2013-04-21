/* File: birth.c */

/* Purpose: create a player character */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 *
 *
 * James E. Wilson and Robert A. Koeneke released all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2 or any later version), 
 * or under the terms of the traditional Angband license. 
 *
 * All changes in Hellband are Copyright (c) 2005-2007 Konijn
 * I Konijn  release all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2), 
 * or under the terms of the traditional Angband license. 
 */ 

#include "angband.h"

/*
* Forward declare
*/
typedef struct birther birther;

/*
* A structure to hold "rolled" information
*/
struct birther
{
	s16b age;
	s16b wt;
	s16b ht;
	s16b sc;
	s16b birthday;

	s32b au;

	s16b stat[STAT_COUNT];

	char history[4][70];
};

/* The last character displayed */
static birther prev;

/* Frequencies of backgrounds to happen */
#define COMMON 60
#define LIKELY 30
#define UNLIKELY 15
#define RARE 5
#define END_GROUP 1

/* Binary options birth screen */
#define SHOW_PLUS TRUE
#define NO_PLUS FALSE
#define SHOW_NAMES FALSE
#define NO_NAMES TRUE

/* Player background information  */
typedef struct background_type background_type;
struct background_type
{
	cptr info;		/* Textual History */
	byte frequency;		/* Frequency of this entry */
	s16b bonus;		/* Social Class Bonus + 50 */
	u32b metadata;		/* validity check and end of group flag */
};

static background_type background[] = 
{
    {"You are the descendant of a Dwarven ",                                       UNLIKELY,            0,    BG_DWARF},                                                                                                        
    {"You are the last descendant of a Dwarven ",                                  COMMON,              10,   END_GROUP},                                                                                                       
    
    {"Miner. ",                                                                    COMMON,              0,    BG_DWARF},                                                                                                        
    {"Warrior. ",                                                                  COMMON,              0,    0},                                                                                                               
    {"Shaman. ",                                                                   UNLIKELY,            15,   0},                                                                                                               
    {"Lord. ",                                                                     RARE,                25,   0},                                                                                                               
    {"King. ",                                                                     RARE,                25,   END_GROUP},                                                                                                       
    
    {"You are the illegitimate and unacknowledged child ",                         RARE,                -20,  BG_FLORENTIAN | BG_NORDIC | BG_GIPSY | BG_DWARF | BG_ELF},                                                        
    {"You are the illegitimate but acknowledged child ",                           UNLIKELY,            -10,  0},                                                                                                               
    {"You are one of several children ",                                           COMMON,              0,    0},                                                                                                               
    {"You are the first child ",                                                   UNLIKELY,            15,   END_GROUP},                                                                                                       
    
    {"You are the adopted child ",                                                 COMMON,              5,    BG_GIANT|BG_OGRE|1|BG_TROLL|BG_WEREWOLF},                                                                                     
    {"You are the only child ",                                                    UNLIKELY,            0,    0},                                                                                                               
    {"You are one of several children of ",                                        RARE,                0,    END_GROUP},                                                                                                        
    
    {"of a guildsman. ",                                                           RARE,                25,   BG_GIANT|BG_OGRE|1|BG_TROLL|BG_WEREWOLF},                                                                                      
    {"of a hunter. ",                                                              COMMON,              25,   0},                                                                                                                
    {"of a fisherman. ",                                                           COMMON,              25,   0},                                                                                                               
    {"of a Shaman. ",                                                              RARE,                90,   END_GROUP},                                                                                                        
    
    {"of a Duke. ",                                                                RARE,                85,   BG_NORDIC| BG_DWARF | BG_ELF},                                                                                     
    {"of a Count. ",                                                               UNLIKELY,            65,   0},                                                                                                                
    {"of a guildsman. ",                                                           COMMON,              25,   0},                                                                                                                
    {"of a hunter. ",                                                              COMMON,              25,   0},                                                                                                                
    {"of a fisherman. ",                                                           COMMON,              25,   0},                                                                                                               
    {"of a Shaman. ",                                                              RARE,                90,   0},                                                                                                                
    {"of a bard. ",                                                                UNLIKELY,            25,   0},                                                                                                                
    {"of the Royal bloodline. ",                                                   RARE,                90,   END_GROUP},                                                                                                        
    
    {"of a clan leader. ",                                                         RARE,                60,   BG_GIPSY},                                                                                                         
    {"of a craftsman. ",                                                           RARE,                10,   0},                                                                                                                
    {"of a fortune-teller. ",                                                      RARE,                15,   0},                                                                                                                
    {"of a thief. ",                                                               RARE,                0,    END_GROUP},                                                                                                        
    
    {"of a serf. ",                                                                LIKELY,              0,    BG_FLORENTIAN},                                                                                                   
    {"of a yeoman. ",                                                              LIKELY,              10,   0},                                                                                                               
    {"of a townsman. ",                                                            COMMON,              20,   0},                                                                                                               
    {"of a guildsman. ",                                                           COMMON,              25,   0},                                                                                                                
    {"of a Duke. ",                                                                RARE,                85,   0},                                                                                                                
    {"of a Marquess. ",                                                            UNLIKELY,            75,   0},                                                                                                                
    {"of a Count. ",                                                               UNLIKELY,            65,   0},                                                                                                                
    {"of a Viscount. ",                                                            UNLIKELY,            50,   0},                                                                                                                
    {"of a Baron. ",                                                               LIKELY,              50,   0},                                                                                                                
    {"of a Seigneur. ",                                                            UNLIKELY,            50,   0},                                                                                                               
    {"of a Patrician. ",                                                           UNLIKELY,            50,   0},                                                                                                               
    {"of the Royal bloodLine. ",                                                   RARE,                90,   END_GROUP},                                                                                                        
    
    { "Your father has elven blood.",                                              UNLIKELY,            35,   BG_ELF},                                                                                                          
    { "Your mother has elven blood.",                                              UNLIKELY,            35,   0},                                                                                                               
    { "Your grandfather has elven blood.",                                         UNLIKELY,            35,   0},                                                                                                               
    { "Your grandmother has elven blood.",                                         UNLIKELY,            35,   END_GROUP},                                                                                                        
    
    {"You are one of several children of ",                                        COMMON,              0,    BG_GNOME|BG_LEPRECHAUN|BG_KOBOLD|BG_ATLANTIAN},                                                                   
    {"You are the only child of ",                                                 UNLIKELY,            15,   END_GROUP},                                                                                                       
    
    {"an Atlantian scholar. ",                                                     COMMON,              70,   BG_ATLANTIAN},                                                                                                    
    {"an Atlantian researcher. ",                                                  COMMON,              70,   0 },                                                                                                              
    {"an Atlantian guardian. ",                                                    COMMON,              80,   END_GROUP },                                                                                                      
    
    {"a gnome beggar. ",                                                           RARE,                10,   BG_GNOME},                                                                                                        
    {"a gnome braggart. ",                                                         COMMON,              15,   0},                                                                                                               
    {"a gnome prankster. ",                                                        COMMON,              15,   0},                                                                                                               
    {"a gnome scout. ",                                                            UNLIKELY,            60,   0},                                                                                                               
    {"a gnome illusionist. ",                                                      UNLIKELY,            30,   END_GROUP},                                                                                                       
    
    {"a shoemaker. ",                                                              COMMON,              15,   BG_LEPRECHAUN},                                                                                                   
    {"a storyteller. ",                                                            UNLIKELY,            30,   END_GROUP},                                                                                                        
    
    {"a kobold thief. ",                                                           COMMON,              -15,  BG_KOBOLD},                                                                                                       
    {"a kobold scout. ",                                                           LIKELY,              0,    0},                                                                                                               
    {"a kobold assassin. ",                                                        UNLIKELY,            -30,  END_GROUP},                                                                                                       
    
    {"Your real father is a descendant of a troll ",                               COMMON,              0,    BG_TROLL},                                                                                                        
    {"Your mother is a descendant of a troll ",                                    UNLIKELY,            0,    END_GROUP},                                                                                                       
    
    {"warrior. ",                                                                  COMMON,              0,    BG_TROLL},                                                                                                        
    {"Shaman. ",                                                                   UNLIKELY,            15,   0},                                                                                                               
    {"chief. ",                                                                    RARE,                1523, END_GROUP},                                                                                                       
    
    {"You have Ogre blood from your mothers' side, your father is unaware. ",      UNLIKELY,            0,    BG_OGRE},                                                                                                         
    {"You have Ogre blood from your fathers' side, your mother is unaware. ",      UNLIKELY,            0,    END_GROUP},                                                                                                       
    
    {"One of your forefathers on your mother's side mother was a Giant. ",         UNLIKELY,            0,    BG_GIANT},                                                                                                        
    {"One of your forefathers on your father's side mother was a Giant. ",         UNLIKELY,            0,    END_GROUP},                                                                                                       
    
    {"You are the the distant offspring of an unknown Titan. ",                    COMMON,              25,   BG_TITAN },                                                                                                       
    {"You are the the distant offspring of Themis. ",                              COMMON,              50,   0},                                                                                                               
    {"You are the the distant offspring of Mnemosyne. ",                           UNLIKELY,            40,   0 },                                                                                                              
    {"You are the the distant offspring of Okeanos. ",                             LIKELY,              35,   0 },                                                                                                              
    {"You are the the distant offspring of Crius. ",                               RARE,                25,   0 },                                                                                                              
    {"You are the the distant offspring of Hyperion. ",                            LIKELY,              35,   0 },                                                                                                              
    {"You are the the distant offspring of Kronos. ",                              LIKELY,              35,   END_GROUP },                                                                                                      
    
    {"You are a rebel child of ",                                                  COMMON,              75,   BG_NEPHILIM},                                                                                                     
    {"You are a long lost child of ",                                              UNLIKELY,            35,   END_GROUP},                                                                                                       
    
    {"someone with angel blood. ",                                                 UNLIKELY,            15,   BG_NEPHILIM},                                                                                                     
    {"an unknown child of an angel. ",                                             UNLIKELY,            15,   0},                                                                                                               
    {"an unknown angel. ",                                                         UNLIKELY,            15,   0},                                                                                                               
    {"Araqiel. ",                                                                  COMMON,              -20,  0},                                                                                                               
    {"Kokabiel. ",                                                                 COMMON,              -20,  0},                                                                                                               
    {"Samyaza. ",                                                                  COMMON,              -20,  0},                                                                                                               
    {"Ramiel. ",                                                                   COMMON,              -20,  0},                                                                                                               
    {"Daniel. ",                                                                   COMMON,              -20,  0},                                                                                                               
    {"Chazaqiel. ",                                                                COMMON,              -20,  0},                                                                                                               
    {"Azazel. ",                                                                   COMMON,              -20,  0},                                                                                                               
    {"Baraqiel. ",                                                                 COMMON,              -20,  0},                                                                                                               
    {"Sariel. ",                                                                   COMMON,              -20,  0},                                                                                                               
    {"one of the Grigori leaders. ",                                               COMMON,              -40,  0},                                                                                                               
    {"one of the Grigori. ",                                                       COMMON,              -20,  END_GROUP},                                                                                                       
    
    {"You are the black sheep of the family. ",                                    UNLIKELY,            -15,  BG_FLORENTIAN | BG_NORDIC | BG_LEPRECHAUN | BG_GNOME | BG_ELF | BG_DWARF | BG_GIANT | BG_OGRE | BG_TITAN |BG_WEREWOLF},       
    {"You are a credit to the family. ",                                           UNLIKELY,            10,   0},                                                                                                               
    {"You are a well liked child. ",                                               LIKELY,              5,    END_GROUP},
    
    {"You have left your family the moment you became a werewolf. ",               UNLIKELY,            30,   BG_WEREWOLF},
    {"After some family members went missing, you left the family. ",              LIKELY,              10,   0},
    {"You have slain most of your village and went hiding. ",                      LIKELY,              -5,   END_GROUP},

    {"You have been captured by the League, forced now to do their bidding.",      UNLIKELY,            -5,   BG_WEREWOLF},
    {"You have joined the League, hoping to find a cure.",                         LIKELY,              10,   END_GROUP},
    
    {"You have awakened 1 millenium ago, and yearn to retreat soon. ",             LIKELY,              35,   BG_ELDER},
    {"You have studied demonic magics for 700 years, and seek some adventure. ",   UNLIKELY,            20,   0},
    {"You are on a quest to find your Maker since the world is born. ",            RARE,                70,   END_GROUP},    
    
    {"You were born in Ireland. ",                                                 UNLIKELY,            15,   BG_FAE|BG_LEPRECHAUN|BG_GNOME },                                                                                  
    {"You were born in Scotland. ",                                                UNLIKELY,            15,   0 },                                                                                                              
    {"You were born in Whales. ",                                                  UNLIKELY,            15,   0 },                                                                                                              
    {"You were born under a full moon. ",                                          UNLIKELY,            15,   END_GROUP},                                                                                                       
    
    {"You have a great sense of humour, ",                                         UNLIKELY,            15,   BG_FAE|BG_LEPRECHAUN|BG_GNOME},                                                                                   
    {"You have the reputation of a prankster, ",                                   UNLIKELY,            -15,  0},                                                                                                               
    {"You have an insatiable wanderlust, ",                                        UNLIKELY,            15,   END_GROUP},                                                                                                       
    
    {"curly red hair, ",                                                           UNLIKELY,            0,    BG_FAE|BG_LEPRECHAUN|BG_GNOME},                                                                                   
    {"spiked red hair, ",                                                          UNLIKELY,            0,    END_GROUP},                                                                                                       
    
    {"blue eyes, and a very fair complexion.",                                     UNLIKELY,            0,    BG_FAE|BG_LEPRECHAUN|BG_GNOME},                                                                                   
    {"green eyes, and a fair complexion.",                                           UNLIKELY,            0,    END_GROUP},                                                                                                       
    
    {"You have a green complexion, ",                                              UNLIKELY,            0,    BG_KOBOLD},                                                                                                       
    {"You have a dark green complexion, ",                                         UNLIKELY,            0,    0},                                                                                                               
    {"You have a yellow complexion, ",                                             UNLIKELY,            0,    0},                                                                                                               
    {"You have a green complexion with red markings, ",                            UNLIKELY,            0,    END_GROUP},                                                                                                       
    
    {"bright eyes, ",                                                              UNLIKELY,            0,    BG_KOBOLD},                                                                                                       
    {"yellow eyes, ",                                                              UNLIKELY,            0,    0},                                                                                                               
    {"red eyes, ",                                                                 UNLIKELY,            0,    0},                                                                                                               
    {"snake-like eyes, ",                                                          UNLIKELY,            0,    END_GROUP },                                                                                                      
    
    {"and a long sinuous tail.",                                                   UNLIKELY,            0,    BG_KOBOLD},                                                                                                       
    {"and a short tail.",                                                          UNLIKELY,            0,    0},                                                                                                               
    {"and a muscular tail.",                                                       UNLIKELY,            0,    0},                                                                                                               
    {"and a long tail.",                                                           UNLIKELY,            0,    0},                                                                                                               
    {"and a sinuous tail.",                                                        UNLIKELY,            0,    END_GROUP },                                                                                                      
    
    /* Nordic eyes */                                                             
    {"You have dark brown eyes, ",                                                 RARE,                0,    BG_NORDIC| BG_DWARF | BG_ELF },                                                                                   
    {"You have brown eyes, ",                                                      RARE,                0,    0},                                                                                                               
    {"You have hazel eyes, ",                                                      UNLIKELY,            0,    0},                                                                                                               
    {"You have green eyes, ",                                                      LIKELY,              0,    0},                                                                                                               
    {"You have blue eyes, ",                                                       COMMON,              0,    0},                                                                                                               
    {"You have amber eyes, ",                                                      UNLIKELY,            5,    0},                                                                                                               
    {"You have grey eyes, ",                                                       LIKELY,              0,    0},                                                                                                                
    {"You have violet eyes, ",                                                     RARE,                5,    END_GROUP},                                                                                                                
    
    /* South Europe / Middle East eyes */                                         
    {"You have dark brown eyes, ",                                                 LIKELY,              0,    BG_GIPSY|BG_FLORENTIAN|BG_TITAN|BG_GIANT|BG_OGRE},                                                                
    {"You have brown eyes, ",                                                      COMMON,              0,    0},                                                                                                               
    {"You have hazel eyes, ",                                                      UNLIKELY,            0,    0},                                                                                                               
    {"You have green eyes, ",                                                      UNLIKELY,            0,    0},                                                                                                               
    {"You have blue eyes, ",                                                       UNLIKELY,            0,    0},                                                                                                               
    {"You have amber eyes, ",                                                      UNLIKELY,            0,    0},                                                                                                               
    {"You have grey eyes, ",                                                       UNLIKELY,            0,    END_GROUP},                                                                                                        
    
    /* World citizen */                                                           
    {"You have dark brown eyes, ",                                                 UNLIKELY,            0,    BG_NEPHILIM|BG_ATLANTIAN|BG_ELDER},                                                                               
    {"You have brown eyes, ",                                                      UNLIKELY,            0,    0},                                                                                                               
    {"You have hazel eyes, ",                                                      UNLIKELY,            0,    0},                                                                                                               
    {"You have green eyes, ",                                                      UNLIKELY,            0,    0},                                                                                                               
    {"You have blue eyes, ",                                                       UNLIKELY,            0,    0},                                                                                                               
    {"You have amber eyes, ",                                                      UNLIKELY,            0,    0},                                                                                                               
    {"You have violet eyes, ",                                                     RARE,                5,    0},                                                                                                                
    {"You have grey eyes, ",                                                       UNLIKELY,            0,    END_GROUP},                                                                                                        
    
    {"straight ",                                                                  UNLIKELY,            0,    BG_NEPHILIM|BG_ATLANTIAN|BG_ELDER|BG_GIPSY|BG_FLORENTIAN|BG_TITAN|BG_GIANT|BG_OGRE|BG_NORDIC| BG_DWARF | BG_ELF}, 
    {"wavy ",                                                                      UNLIKELY,            0,    0},                                                                                                               
    {"curly ",                                                                     UNLIKELY,            0,    END_GROUP},                                                                                                       
    
    /*Nordic hairs manga glasses ;}, */ 
    {"flaming red hair, ",                                                         UNLIKELY,            0,    BG_NORDIC| BG_DWARF | BG_ELF},                                                                                    
    {"arctic blond hair, ",                                                        COMMON,              0,    0},                                                                                                                
    {"jet black hair, ",                                                           UNLIKELY,            0,    0},                                                                                                                
    {"brown hair, ",                                                               LIKELY,              0,    END_GROUP},                                                                                                        
    
    /*South Europe / Middle East eyes manga glasses ;}, */ 
    {"jet black hair, ",                                                           UNLIKELY,            0,    BG_GIPSY|BG_FLORENTIAN|BG_TITAN|BG_GIANT|BG_OGRE},                                                                 
    {"brown hair, ",                                                               UNLIKELY,            0,    0},                                                                                                                
    {"dark brown hair, ",                                                          UNLIKELY,            0,    END_GROUP},                                                                                                        
    
    /* World citizen hair */                                                      
    {"flaming red hair, ",                                                         UNLIKELY,            0,    BG_NEPHILIM|BG_ATLANTIAN|BG_ELDER},                                                                               
    {"arctic blond hair, ",                                                        UNLIKELY,            0,    0},
    {"jet black hair, ",                                                           UNLIKELY,            0,    0}, 
    {"brown hair, ",                                                               UNLIKELY,            0,    END_GROUP},                                                                                                        
    
    /*Nordic Complexions */                                                       
    {"and a dark complexion.",                                                     RARE,                0,    BG_NORDIC| BG_DWARF | BG_ELF},                                                                                    
    {"and an average complexion.",                                                 RARE,                0,    0},                                                                                                               
    {"and a fair complexion.",                                                     COMMON,              0,    0},                                                                                                               
    {"and a very fair complexion.",                                                LIKELY,              0,    END_GROUP},                                                                                                       
    
    /*South Europe / Middle East complexions */                                   
    {"and a very dark complexion.",                                                COMMON,              0,    BG_GIPSY|BG_FLORENTIAN|BG_TITAN|BG_GIANT|BG_OGRE},                                                                
    {"and a dark complexion.",                                                     COMMON,              0,    0},                                                                                                               
    {"and an average complexion.",                                                 UNLIKELY,            0,    0},                                                                                                               
    {"and a fair complexion.",                                                     RARE,                0,    END_GROUP},                                                                                                       
    
    /*World citizen complexions */                                                
    {"and a very dark complexion.",                                                UNLIKELY,            0,    BG_NEPHILIM|BG_ATLANTIAN|BG_ELDER},                                                                               
    {"and a dark complexion.",                                                     UNLIKELY,            0,    0},                                                                                                               
    {"and an average complexion.",                                                 UNLIKELY,            0,    0},                                                                                                               
    {"and a fair complexion.",                                                     UNLIKELY,            0,    0},                                                                                                               
    {"and a very fair complexion.",                                                UNLIKELY,            0,    END_GROUP},                                                                                                       
    
    {"You have become corrupted in the last century. ",                             LIKELY,              0,    BG_HORROR},                                                                                                       
    {"You have become corrupted a millenium ago. ",                                 UNLIKELY,            0,    0},                                                                                                                
    {"You have become corrupted during the Great Flood. ",                          RARE,                10,   END_GROUP},                                                                                                        
    
    {"You have slimy skin, empty glowing eyes, and ",                              UNLIKELY,            15,   BG_HORROR},                                                                                                       
    {"You have slimy scales, a set of eyestalks, and ",                            UNLIKELY,            15,   END_GROUP},                                                                                                        
    
    {"three tentacles around your mouth.",                                         UNLIKELY,            0,    BG_HORROR},                                                                                                       
    {"four tentacles around your mouth.",                                          UNLIKELY,            0,    0},                                                                                                               
    {"five tentacles around your mouth.",                                          RARE,                -5,   END_GROUP},                                                                                                       
    
    {"You sire was a mindless demonic spawn.",                                     COMMON,              15,   BG_IMP},                                                                                                          
    {"You sire was a minor demon.",                                                LIKELY,              20,   0},                                                                                                                
    {"You sire was a major demon.",                                                UNLIKELY,            25,   0},                                                                                                                
    {"You sire was a demon lord.",                                                 RARE,                30,   END_GROUP},                                                                                                        
    
    {"You have a dark red skin, ",                                                 COMMON,              0,    BG_IMP},                                                                                                          
    {"You have a slimy green skin, ",                                              LIKELY,              -5,   0},                                                                                                               
    {"You have a jet black skin, ",                                                UNLIKELY,            5,    END_GROUP},                                                                                                       
    
    {"claws, fangs, spikes, and glowing red eyes.",                                UNLIKELY,            -10,  BG_IMP},                                                                                                          
    {"claws, fangs, and glowing red eyes.",                                        LIKELY,              -5,   0},                                                                                                               
    {"claws, and glowing red eyes.",                                               LIKELY,              -5,   END_GROUP},                                                                                                       
    
    {"You were created to guard ",                                                 UNLIKELY,            45,   BG_GUARDIAN},                                                                                                     
    {"You were created to protect ",                                               UNLIKELY,            45,   0},                                                                                                               
    {"You were created to preserve ",                                              UNLIKELY,            45,   0},                                                                                                               
    {"You were created to oversee ",                                               UNLIKELY,            45,   END_GROUP},                                                                                                        
    
    {"a lost Elder",                                                               UNLIKELY,            0,    BG_GUARDIAN},                                                                                                     
    {"the sarcophagus of an Elder",                                                UNLIKELY,            -10,  0},                                                                                                               
    {"the stasis chamber of an Elder",                                             UNLIKELY,            -10,  0},                                                                                                               
    {"the first Elder",                                                            RARE,                15,   END_GROUP},                                                                                                       
    
    {" Horror.",                                                                   RARE,                -5,   BG_GUARDIAN},                                                                                                     
    {".",                                                                          LIKELY,              0,    END_GROUP},                                                                                                       
    
    {"You were cursed because ",                                                   UNLIKELY,            0,    BG_SKELETON},                                                                                                     
    {"You were hexed because ",                                                    UNLIKELY,            0,    END_GROUP},                                                                                                        
    
    {"you slept with a medicine man's daughter. ",                                 UNLIKELY,            7,    BG_SKELETON},                                                                                                     
    {"sold fake magical amulets to a medicine man. ",                              UNLIKELY,            7,    0},                                                                                                               
    {"pretended to be more powerful than the local medicin man. ",                 UNLIKELY,            7,    0},                                                                                                               
    {"mistook a medicine man for the village fool. ",                              UNLIKELY,            7,    0},                                                                                                               
    {"you asked a medicine man for immortality. ",                                 UNLIKELY,            7,    0},                                                                                                               
    {"were in the wrong place at the wrong time. ",                                UNLIKELY,            7,    0},                                                                                                               
    {"you thought it would get you a role with Johny Depp. ",                      UNLIKELY,            7,    END_GROUP},                                                                                                       
    
    {"You have dirty, dry bones, ",                                                UNLIKELY,            0,    BG_SKELETON},                                                                                                     
    {"You have rotten black bones, ",                                              UNLIKELY,            0,    0},                                                                                                               
    {"You have filthy, brown bones, ",                                             UNLIKELY,            0,    0},                                                                                                               
    {"You have shining white bones, ",                                             UNLIKELY,            0,    END_GROUP},                                                                                                       
    
    {"and a blackened skull.",                                                     UNLIKELY,            0,    BG_SKELETON},                                                                                                     
    {"and a fractured skull.",                                                     UNLIKELY,            0,    0},                                                                                                               
    {"and empty eyesockets.",                                                      UNLIKELY,            0,    END_GROUP},                                                                                                       
    
    {"You have slime green eyes, ",                                                UNLIKELY,            5,    BG_MUMMY|BG_TROLL},                                                                                               
    {"You have puke yellow eyes, ",                                                UNLIKELY,            5,    0},                                                                                                               
    {"You have blue-bloodshot eyes, ",                                             UNLIKELY,            5,    0},                                                                                                               
    {"You have glowing red eyes, ",                                                UNLIKELY,            5,    END_GROUP},                                                                                                       
    
    {"dirty ",                                                                     UNLIKELY,            0,    BG_MUMMY|BG_TROLL},                                                                                               
    {"mangy ",                                                                     UNLIKELY,            0,    0},                                                                                                               
    {"oily ",                                                                      UNLIKELY,            0,    END_GROUP},                                                                                                       
    
    {"sea-weed green hair, ",                                                      UNLIKELY,            0,    BG_MUMMY|BG_TROLL},                                                                                               
    {"bright red hair, ",                                                          UNLIKELY,            0,    0},                                                                                                               
    {"dark purple hair, ",                                                         UNLIKELY,            0,    END_GROUP},                                                                                                       
    
    {"and green ",                                                                 UNLIKELY,            0,    BG_MUMMY|BG_TROLL},                                                                                               
    {"and blue ",                                                                  UNLIKELY,            0,    0},                                                                                                               
    {"and white ",                                                                 UNLIKELY,            0,    0},                                                                                                               
    {"and black ",                                                                 UNLIKELY,            0,    END_GROUP},                                                                                                       
    
    {"ulcerous skin.",                                                             UNLIKELY,            0,    BG_MUMMY|BG_TROLL},                                                                                               
    {"scabby skin.",                                                               UNLIKELY,            0,    0},                                                                                                               
    {"leprous skin.",                                                              UNLIKELY,            0,    END_GROUP},                                                                                                       
    
    {"You were created by the local dabbler in the dark arts. ",                   UNLIKELY,            0,    BG_MUMMY},                                                                                                        
    {"You were created by a priest of Mazghuna. ",                                 UNLIKELY,            0,    0},                                                                                                               
    {"You were created by a dark priest in Saqqara. ",                             UNLIKELY,            0,    0},                                                                                                               
    {"You were created by an evil priest of Dahshur. ",                            UNLIKELY,            0,    0},                                                                                                               
    {"You were created by a pact with Egyptian Sand Demons. ",                     UNLIKELY,            -5,   0},                                                                                                               
    {"You were created by the high priests of Gizeh. ",                            UNLIKELY,            5,    0},                                                                                                               
    {"You were created by the Pharaoh. ",                                          UNLIKELY,            15,   END_GROUP},                                                                                                       
    
    {"You arose from an unmarked grave. ",                                         LIKELY,              15,   BG_VAMPIRE},                                                                                                      
    {"In life you were a simple peasant, the victim of a powerful Vampire Lord. ", UNLIKELY,            25,   0},                                                                                                               
    {"In life you were a Vampire Hunter, but they got you. ",                      UNLIKELY,            60,   0},                                                                                                               
    {"In life you were a Necromancer. ",                                           RARE,                30,   0},                                                                                                               
    {"In life you were a powerful noble. ",                                        UNLIKELY,            25,   0},                                                                                                               
    {"In life you were a powerful and cruel tyrant. ",                             RARE,                55,   END_GROUP},                                                                                                       
    
    {"You have jet-black hair, ",                                                  UNLIKELY,            0,    BG_VAMPIRE},                                                                                                      
    {"You have matted brown hair, ",                                               UNLIKELY,            0,    0},                                                                                                               
    {"You have white hair, ",                                                      UNLIKELY,            0,    0},                                                                                                               
    {"You have a bald head, ",                                                     UNLIKELY,            0,    END_GROUP},                                                                                                       
    
    {"eyes like red coals and a deathly pale complexion. ",                        UNLIKELY,            0,    BG_VAMPIRE},                                                                                                      
    {"blank white eyes and an alabaster complexion. ",                             UNLIKELY,            0,    0},                                                                                                               
    {"feral yellow eyes and glistening fangs. ",                                   UNLIKELY,            0,    0},                                                                                                               
    {"bloodshot red eyes and the smell of death surrounding you.",                 UNLIKELY,            0,    END_GROUP},                                                                                                       

    {"You were refused the afterlife by a curse. ",                                LIKELY,              15,   BG_SPECTRE},
    {"You refused the afterlife with an oath. ",                                   LIKELY,              35,   0},
    {"You cheated death with a demonic pact. ",                                    UNLIKELY,            15,   0},
    {"You were a necromancer in life. ",                                           UNLIKELY,            35,   END_GROUP},

    {"You have eyes like red coals, ",                                             LIKELY,              0,    BG_SPECTRE},
    {"You have blank white eyes, ",                                                UNLIKELY,            -5,   0 },
    {"You have gaping holes where your eyes were, ",                               UNLIKELY,            -5,   END_GROUP },

    {"a deathly gray complexion. ",                                                LIKELY,              0,    BG_SPECTRE},
    {"an intense sadness is expressed in all your movements. ",                    UNLIKELY,            5,    END_GROUP },

    {"You are surrounded by an eerie green aura.",                                 UNLIKELY,            0,    BG_SPECTRE},
       {"You are constantly surrounded by a white fog.",                              LIKELY,              5,    END_GROUP},

    {"Your mother was a succubus. ",                                               LIKELY,              15,   BG_DEVILSPAWN|BG_SUCCUBUS|BG_IMP},
    {"Your father was an incubus. ",                                               LIKELY,              15,   0},
    {"Your mother was Glaryssa, the Succubus Queen. ",                               LIKELY,              65,   0},
    {"Your mother was Lilith, the first Woman. ",                                   LIKELY,              80,   0},
    {"You are created from raw Chaos itself. ",                                    LIKELY,              90,   0},
    {"You are a descendant of a Devil Prince. ",                                   LIKELY,              70,   END_GROUP},
        
    {"Your mother was Lilith, the first Woman. ",                                   LIKELY,              80,   BG_LILI},
    {"Your grandmother was Lilith, the first Woman. ",                               UNLIKELY,            65,   END_GROUP},    
    
    {"You have coal black eyes, ",                                                 LIKELY,              0,    BG_DEVILSPAWN|BG_SUCCUBUS|BG_IMP|BG_LILI},
    {"You have pale pink eyes, ",                                                  LIKELY,              0,    0},
    {"You have eyes like red embers, ",                                            LIKELY,              0,    0},
    {"You have beautiful green eyes, ",                                            UNLIKELY,            5,    END_GROUP},

    {"no hair at all, ",                                                           LIKELY,              15,   BG_DEVILSPAWN|BG_SUCCUBUS|BG_IMP|BG_LILI},    
    {"dirty brown hair, ",                                                         LIKELY,              15,   0},
    {"mangy gray hair, ",                                                          LIKELY,              15,   0},
    {"fine albino hair, ",                                                         UNLIKELY,            5,    END_GROUP},

    {"and the hooves of a goat.",                                                  LIKELY,              -5,   BG_DEVILSPAWN|BG_IMP|BG_LILI},
    {"and vestigial wings.",                                                       LIKELY,              0,    0},
    {"and slightly clawed fingers.",                                               LIKELY,              -3,   0},
    {"and an unnaturally wide smile.",                                             LIKELY,              -3,   0},
    {"and a slight sulphurous smell about you.",                                   LIKELY,              3,    0},
    {"and a bright red skin.",                                                     LIKELY,              0,    0},
    {"and a forked tongue.",                                                       LIKELY,              3,    END_GROUP},
    
    {"and a pitch black skin.",                                                    LIKELY,              0,    BG_SUCCUBUS},
    {"and a fiery red skin.",                                                       LIKELY,              0,    0},
    {"and an emerald green skin.",                                                 LIKELY,              0,    0},
    {"and a frosty blue skin.",                                                    LIKELY,              0,    END_GROUP},    
    
    { NULL,                                                                        0,                   0,    END_GROUP}, /* Ye famous null record */
};

/* Current stats */
static s16b stat_use[STAT_COUNT];
/* Autoroll limit */
static s16b stat_limit[STAT_COUNT];
/* Autoroll matches */
static s32b stat_match[STAT_COUNT];
/* Autoroll round */
static s32b auto_round;
/* Last round */
static s32b last_round;

/* Name segments for random player names */

static char *dwarf_syllable1[] = { "B", "D", "F", "G", "Gl", "H", "K", "L", "M", "N", "R", "S", "T", "Th", "V", };
static char *dwarf_syllable2[] = { "a", "e", "i", "o", "oi", "u", };
static char *dwarf_syllable3[] = { "bur", "fur", "gan", "gnus", "gnar", "li", "lin", "lir", "mli", "nar", "nus", "rin", "ran", "sin", "sil", "sur", };

static char *elf_syllable1[] = { "Al", "An", "Bal", "Bel", "Cal", "Cel", "El", "Elr", "Elv", "Eow", "Ear", "F", "Fal", "Fel", "Fin", "G", "Gal", "Gel", "Gl", "Is", "Lan", "Leg", "Lom", "N", "Nal", "Nel",  "S", "Sal", "Sel", "T", "Tal", "Tel", "Thr", "Tin", };
static char *elf_syllable2[] = { "a", "adrie", "ara", "e", "ebri", "ele", "ere", "i", "io", "ithra", "ilma", "il-Ga", "ili", "o", "orfi", "u", "y", };
static char *elf_syllable3[] = { "l", "las", "lad", "ldor", "ldur", "linde", "lith", "mir", "n", "nd", "ndel", "ndil", "ndir", "nduil", "ng", "mbor", "r", "rith", "ril", "riand", "rion", "s", "thien", "viel", "wen", "wyn", };

static char *gnome_syllable1[] = { "Aar", "An", "Ar", "As", "C", "H", "Han", "Har", "Hel", "Iir", "J", "Jan", "Jar", "K", "L", "M", "Mar", "N", "Nik", "Os", "Ol", "P", "R", "S", "Sam", "San", "T", "Ter", "Tom", "Ul", "V", "W", "Y", };
static char *gnome_syllable2[] = { "a", "aa",  "ai", "e", "ei", "i", "o", "uo", "u", "uu", };
static char *gnome_syllable3[] = { "ron", "re", "la", "ki", "kseli", "ksi", "ku", "ja", "ta", "na", "namari", "neli", "nika", "nikki", "nu", "nukka", "ka", "ko", "li", "kki", "rik", "po", "to", "pekka", "rjaana", "rjatta", "rjukka", "la", "lla", "lli", "mo", "nni", };

static char *human_syllable1[] = { "Ab", "Ac", "Ad", "Af", "Agr", "Ast", "As", "Al", "Adw", "Adr", "Ar", "B", "Br", "C", "Cr", "Ch", "Cad", "D", "Dr", "Dw", "Ed", "Eth", "Et", "Er", "El", "Eow", "F", "Fr", "G", "Gr", "Gw", "Gal", "Gl", "H", "Ha", "Ib", "Jer", "K", "Ka", "Ked", "L", "Loth", "Lar", "Leg", "M", "Mir", "N", "Nyd", "Ol", "Oc", "On", "P", "Pr", "R", "Rh", "S", "Sev", "T", "Tr", "Th", "V", "Y", "Z", "W", "Wic", };
static char *human_syllable2[] = { "a", "ae", "au", "ao", "are", "ale", "ali", "ay", "ardo", "e", "ei", "ea", "eri", "era", "ela", "eli", "enda", "erra", "i", "ia", "ie", "ire", "ira", "ila", "ili", "ira", "igo", "o", "oa", "oi", "oe", "ore", "u", "y", };
static char *human_syllable3[] = { "a", "and", "b", "bwyn", "baen", "bard", "c", "ctred", "cred", "ch", "can", "d", "dan", "don", "der", "dric", "dfrid", "dus", "f", "g", "gord", "gan", "l", "li", "lgrin", "lin", "lith", "lath", "loth", "ld", "ldric", "ldan", "m", "mas", "mos", "mar", "mond", "n", "nydd", "nidd", "nnon", "nwan", "nyth", "nad", "nn", "nnor", "nd", "p", "r", "ron", "rd", "s", "sh", "seth", "sean", "t", "th", "tha", "tlan", "trem", "tram", "v", "vudd", "w", "wan", "win", "wyn", "wyr", "wyr", "wyth", };

//static char *orc_syllable1[] = { "B", "Er", "G", "Gr", "H", "P", "Pr", "R", "V", "Vr", "T", "Tr", "M", "Dr", };
//static char *orc_syllable2[] = { "a", "i", "o", "oo", "u", "ui", };
//static char *orc_syllable3[] = { "dash", "dish", "dush", "gar", "gor", "gdush", "lo", "gdish", "k", "lg", "nak", "rag", "rbag", "rg", "rk", "ng", "nk", "rt", "ol", "urk", "shnak", "mog", "mak", "rak", };

static char *swamp_syllable1[] = { "Kr", "Skr", "Schr", "Sch", "Pr", "M" , };
static char *swamp_syllable2[] = { "ae", "ea", "owe", "oo", "ui", "oeu" , "eve" , };
static char *swamp_syllable3[] = { "dash", "dush", "gar", "gor", "nak", "rag", "rg", "rk", "mak", "rak", };

static char *angel_syllable1[] = { "Sa","A","U","Mi","Ra","Ana","Pa","Lu","She","Ga","Da","O","Pe","Lau", };
static char *angel_syllable2[] = { "br","m","l","z","zr","mm","mr","r","ral","ch","zaz","tr","n","lar", };
static char *angel_syllable3[] = { "iel","ial","ael","ubim","aphon","iel","ael", };

static char *illithid_syllable1[] = { "Cth","Az","Fth","Ts","Xo","Q'N","R'L","Ghata","L","Zz","Fl","Cl","S","Y", };
static char *illithid_syllable2[] = { "nar","loi","ul","lu","noth","thon","ath","'N","rhy","oth","aza","agn","oa","og", };
static char *illithid_syllable3[] ={ "l","a","u","oa","oggua","oth","ath","aggua","lu","lo","loth","lotha","agn","axl",};

/* Thomas, Peter, Paul , James , John , George , Matteus ,Marc , filip , Luke*/
static char *italian_male_firstnames[]   = { "Tommaso" , "Pietro" , "Paolo" , "Giacomo" , "Giovanni" , "Giorgio" , "Matteo" , "Marco" , "Filippo" , "Lucas" , "Dante" , "Arcolana" , };
static char *italian_female_firstnames[] = { "Beatrice" , "Deianira" , "Lucia" , "Penelope" , "Piccarda" , "Rachel" , "Carlotta" , "Carla", };
static char *italian_last_names[]        = { "da Casalodi" , "dei Bonacolsi" , "da Siena" , "Aldobrandi" , "dei Malavolti" , "Andalo" , "d'Arezzo" , "de'Mozzi" , "di Carignano" , "Argenti" , "da Sant'Andrea" , "Berti" , "Brunelleschi" , "Donati" , "Bruni", };

static char *romani_male_firstnames[]    = { "Andrezj" , "Emilian" , "Guaril" , "Harman" , "Luca" , "Mihai" , "Nicolae" , "Shandor" , "Simionce" , "Stefan" , "Tobar" , "Yanko" , "Yoska" , };
static char *romani_female_firstnames[]  = { "Esmeralda" , "Magdolna" , "Florica" , "Jaelle" , "Kizzy" , "Lyuba" , "Nadya" , "Tsura" , };
static char *romani_last_names[]         = { "Grey" , "Petulengro" , "Hearne" , "Lee" , "Caumlo" , "Marhshall" , "Stanley" , };

static char *nordic_male_firstnames[]    = { "Aksel" , "Asger" , "Bjorn" , "Eirik" , "Leif" , "Sigfinn" , "Tyr" , "Ulf" , "Ymir" , "Balder" , };
static char *nordic_female_firstnames[]  = { "Anniken" , "Asfrid" , "Dagny" , "Eira" , "Eisa" , "Eydis" , "Fylla" , "Gunnhild" , "Inga" , "Liv" , };
static char *nordic_last_names[]         = { "Olsson" , "Hansson" , "Karlsson" , "Gustavson" , "Erikson" , "Anderson" , "Hirvonen" , "Luhtanen" , "Sari" , "Uurainen" , };

static char *egypt_male_firstnames[]    = { "Akil" , "Amenemhat" , "Amun" , "Asar" , "Awan" , "Chenzira" , "Funsani" , "Haji" , "Jumoke" , "Paki" , };
static char *egypt_female_firstnames[]  = { "Akana" , "Azeneth" , "Bastet" , "Djeserit" , "Halima" , "Hasina" , "Irisi" , "Kiya" , "Naeehmah" , "Raziya" , };
static char *egypt_last_names[]         = { "Nakthoreb" , "Osorkon" , "Pseusennes" , "Ptolemy" , "Taharqa" , "Yewelot" , "Ahhotep" , "Amenemheb" , "Amenhotep" , "Hatshepsut" , "Horemheb" , "Isetnofret" , "Khaemwaset" , "Meritamen" , "Mutemwiya" , "Rekhmire" , "Tewosret" , };

static char *celtic_male_firstnames[]    = { "Abenzio" , "Annan" , "Anwell" , "Anyon" , "Beacan" , "Bryce" , "Camlin" , "Caradoc" , "Coalan" , "Drostan" , };

static char *giant_male_firstnames[]    = { "Agasthenes " , "Aristaios" , "Astraios" , "Azeios" , "Damysos" , "Emphytos" , "Euboios" , "Euphorbos" , "Euryalos" , "Eurmedon" , "Hyperbios" , "Khtonios" , "Koios" , "Mimon" , "Otos" , "Ouranion" , "Pankrates" , "Peloreus" , "Rhoikos" , "Sykeus" , "Theodamas" , "Theomises" , };

static char *titan_male_firstnames[]    = { "Adanus" , "Andes" , "Astraeus" , "Atlas" , "Coeus" , "Kreios" , "Epimetheus" , "Hoplodames" , "Iapetus" , "Lelantos" , "Megamedes" , "Melisseus" , "Menoitios" , "Mylinos" , "Okeanos" , "Olymbros" , "Ophion" , "Ostatus" , "Phorkys" , "Sykeus" , };
static char *titan_female_firstnames[]  = { "Anchiale" , "Anytos" , "Asteria" , "Aura" , "Klymene" , "Dione" , "Eos" , "Eurybia" , "Eurynome", "Leto" , "Metis" , "Mnemosyne " , "Phoibe" , "Rheia" , "Selene" , "Styx" , "Tethys" , "Theia" , "Themis" , "Tytaen" };


/* Hellband new/mad creation screen tables */

static cptr sexes_strings[] =
{
	"Lady",
	"Gentleman",
};

static cptr sexes_descriptions[COUNT_SEXES][COUNT_LINES] =
{
	/*0123456789012345678901234567890123456789012345678912345*/	
	/*Lady*/
	{"The League sports a few women, all worthy members.     ",
	 "Since the world in the year 1500 is ruled by men, you  ",
	 "have learned your skills outside of the public view.   ",
	 NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Gentleman*/
	{"You have been accepted quite soon in the League because",
	 "of your potential. You have never considered that being",
	 "male has made your progress in the League much easier. ",
	 NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},	
};


static cptr races_strings[] =
{
	"Human",
	"Faerie",
	"Spawn",
	"Elder",
};

static cptr races_descriptions[COUNT_RACES][COUNT_LINES] =
{
	/*0123456789012345678901234567890123456789012345678912345*/	
	/*Human*/
	{"Humans rule the world in the year 1500, however not all",
	 "of them are of pure blood. Others have been bitten by  ",
	 "vampires or werewolves. Humans born under the right    ",
	 "constellation have gained extra-ordinary powers.       ",
	 NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Faerie*/
	{"These little creatures are almost lost to the world,   ",
	 "but some have adapted to the ways of the humans. Mostly",
	 "found on the British Islands, some of them support the ",
	 "activities of the League. Compared to humans, faeries  ",
	 "are more dextrous, intelligent, charming, stealthier   ",
	 "and superior in magic. They are much weaker though.    ",
	 "Their magic keeps them from falling into traps.        ",
	 NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Spawn*/
	{"Creatures born in the pits of Hell, they all fight for ",
	 "their spot. Some win, some loose and some get thrown in",
	 "to the world of Man. The League being a source of much ",
	 "power and knowledge it attracts the occasional outcast ",
	 "Spawn. The league employs some of them, using a magical",
	 "bond that lasts a hundred years. Spawns are stronger,  ",
	 "faster, tougher and more intelligent. They are heinous ",
	 "though, providing little charisma.                     ",
	 NULL,NULL,NULL,NULL,NULL,NULL},
	/*Elder*/
	{"Little is known about the Elder, even the Elder have   ",
	 "forgotten where they come from, what their purpose is. ",
	 "It is a generally accepted idea that the Elder existed ",
	 "when the Earth was created, and they will be there when",
	 "the Earth will be undone. The Elder employ Guardians, a",
	 "subspecies of the Elder born to protect them. Elder are",
	 "charismatic, intelligent and superior in magic.        ",
	 NULL,NULL,NULL,NULL,NULL,NULL,NULL},
};

static int subraces[COUNT_RACES][2] = 
{
	{0 ,11},
	{12,15},
	{16,19},
	{20,22},	
};

static cptr subraces_strings[] =
{
	"Florentian",			/*0*/
	"Gipsy",			/*1*/
	"Nordic",			/*2*/
	"Atlantian",			/*3*/
	"Dwarf descendant",		/*4*/
	"Elf descendant",		/*5*/
	"Ogre descendant",		/*6*/
	"Troll descendant",		/*7*/
	"Giant descendant",		/*8*/
	"Titan descendant",		/*9*/	
	"Nephilim",			/*10*/
	"Afflicted",			/*11*/
	"Seelie Fae",			/*12*/
	"Gnome",			/*13*/
	"Leprechaun",			/*14*/
	"Kobold",			/*15*/	
	"Devilspawn",			/*16*/
	"Imp",				/*17*/
	"Succubus",			/*18*/
	"Lili",				/*19*/	
	"Elder",			/*20*/
	"Elder Guardian",		/*21*/
	"Horror",			/*22*/
};

static cptr subraces_descriptions[COUNT_SUBRACES][COUNT_LINES] =
{
	/*0123456789012345678901234567890123456789012345678912345*/	
	/*Florentian*/
	{   "Florentians are Italian citizens from the city of      ",
		"Florence. They are your basic human, with maybe a bit  ",
		"more interest in Inferno than the average person given ",
		"that Dante was a Florentian as well. Being an Italian  ",
		"they get discount in Italian shops.                    ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Gipsy*/
	{   "Gipsies are not very well liked, even though they are  ",
		"great entertainers and sport some of the most beautiful",
		"women. Gipsies are charismatic, have a knack for being ",
		"stealthy and gain the Second Sight when they become    ",
		"more experienced. They are slightly better in magic.   ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Nordic*/
	{   "Nordics are hardy men from the North. They are still   ",
		"very much in touch with Nature and its' spirits. This  ",
		"makes them slightly better at magical abilities.       ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Atlantian*/
	{   "Living in a dome on the bottom of the ocean, they have ",
		"no natural enemies and grown weak. They do however have",
		"a knack for magic and are resistant to darkness. Their ",
		"innate magical abilities allow to fire magical missiles",
		"at will.                                               ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Dwarf*/
	{   "True dwarfs have not dwelled on the planet surface     ",
		"since ages, but they have mingled with humans and some ",
		"of their descendants are almost as stocky, loudmouthed ",
		"and foul-tempered as they once were. They are hard to  ",
		"be blinded and find their ways easily under the ground.",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Elf*/
	{   "True elfs have not dwelled in the Scandinavian lands   ",
		"since ages, but they have mingled with humans and some ",
		"of their descendants show a startling gracefulness.    ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Ogre*/
	{   "Ogres were a race of large humanoid beings, fierce and ",
		"cruel monsters that ate human flesh. The most viscious ",
		"have been hunted down and subdued. The other ones have ",
		"taken the custom to shapeshift into a human form and   ",
		"lead a normal life among humans. Some can even trick a ",
		"human and procreate. The descendants of these ogres can",
		"still sport bulging muscles, and some still know how to",
		"shapeshift. Ogres are resistant to darkness, their     ",
		"strength cannot be drained and they can place magical  ",
		"traps that explode when touched. They tend be rich.    ",
		NULL,NULL,NULL,NULL},
	/*Troll*/
	{   "Trolls are the Scandinavian version of the German and  ",
		"French ogres. Their descendants even though civilized  ",
		"are uglier, stronger, stupider and regenerate faster.  ",
		"Experienced troll can enter into a berserker fury.     ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Giant*/
	{   "Like the descendants of the Titans they have concealed ",
		"themselves on an island in the Mediterranean sea. The  ",
		"League has found out about their existance and requires",
		"their assistance every now and then. Even though not   ",
		"very smart they make great adventurers with their solid",
		"toughness and strength. They resist strength draining  ",
		"attacks and shards. Experienced, they can smash stone  ",
		"into dust.                                             ",
		NULL,NULL,NULL,NULL,NULL,NULL},
	/*Titan*/
	{   "The largest of all, and superior in almost every aspect",
		"these descedants have been found on a remote island in ",
		"the Mediterranean sea, protected by ancient sorceries. ",
		"The League has managed to penetrate these sorceries and",
		"some of the inhabitants have decided to join them. They",
		"resist chaos, resistance and can spot the weaknesses of",
		"others.                                                ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Nephilim*/
	{   "Children of men and angels, they usually become giant  ",
		"man-eating creatures. It seems that at some point in   ",
		"their life Nephilim must give up their Angelic or their",
		"human heritage. Nephilim starting this adventure have  ",
		"not yet made this choice, allowing them to go either   ",
		"way.                                                   ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Afflicted*/
	{   "Either bitten by vampire or werewolf or undead, the    ",
		"afflicted have lost their humanity. This usually means ",
		"also a higher resistance than usual to nether, cold and",
		"darkness. It also means that they have lost the effects",
		"of any constellation they were born under.             ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},	
	/*Seelie Fae*/
	{   "Seelie Fae, or properly called Seelie Court, are good  ",
		"faeries of the British Isles. They are a beautifull to ",
		"behold, but frail and not very strong. They are very   ",
		"dextrous and have superior magic skills. Their magic   ",
		"prevents them from falling intro traps, from light and ",
		"it allows them to toss around magical sleeping dust.   ",
		"As they get more experienced, they become faster.      ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Gnome*/
	{   "Gnomes are a small, playful folk. Whilst being very    ",
		"intelligent, they suffer from an almost chronic failure",
		" to take anything seriously. Gnomes are constantly on  ",
		"the move, and are impossible to paralyse or slow. In   ",
		"fact, they can even teleport themself at higher levels.",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Leprechaun*/
	{   "Leprechauns are male faeries inhabiting Ireland.  They ", 
		"are into shoemaking, mischief and gold collections.    ",
		"There are no famous leprechauns yet, even though they  ",
		"are superior in magic, dexterity, charm and speed.     ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Kobold*/
	{   "Kobolds are malicious faeries inhabiting the Black     ", 
		"Forest. Some of their talents are very useful and for  ",
		"the right price they sometimes work with the League.   ",
		"They are masters in stealth and poison, an experienced ",
		"kobold even grows glands that allow it to spit poison  ",
		"darts. They are not an intelligent type of faerie, and ",
		"arent great lookers either.                            ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Devilspawn*/
	{   "Devilspawn are the progeny of mortals and demons. As   ",
		"such, they inherit some of the raw strength of their   ",
		"demonic parentage, but their mixed race tends to leave ",
		"their thoughts confused and their forms misshapen.     ",
		"Devilspawn are remembered by their demonic anscestors, ",
		"and as such they always get a demonic patron. Their    ",
		"association with the pandemonium of hell allows them to",
		"resist both confusion and sound attacks.               ",
		NULL,NULL,NULL,NULL,NULL,NULL},
	/*Imp*/
	{   "Imps are small red-skinned fire demons. Although not   ",
		"terribly strong or smart, they are tough and fast. As  ",
		"they are beings of fire, they have innate resistance to",
		"it, growing into immunity as they toughen up. They can ",
		"learn how to toss flame bolts, fireballs and can even  ",
		"gain Second Sight .                                    ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Succubus*/
	{   "Born in the pits of Hell, they have been selected as   ",
		"much for their beauty as their visciousness. They are  ",
		"demons that can take the form of a beautiful woman and ",
		"have a special draining attacks against men. They are  ",
		"intelligent, dextrous, fast and stealthy with a knack  ",
		"for magic. They resists chaos and confusion naturally. ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Lili*/
	{   "Born from Lilith and Asmodeus they know that Lillith   ",
		"will come one day after them. They join the League for ",
		"power, power they will use when the Day comes. Lili are",
		"beautiful and rebellious like their mother and sensual ",
		"like their father. Lilim resist chaos and confusion    ",
		"and are very tough.                                    ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Elder*/
	{	"The true Elder is a very tough creature, regenerating  ",
		"wounds even when almost completely destroyed. They are ",
		"a beautiful sight to behold and radiate light in the   ",
		"dark. Their senses are magically attuned and they have ",
		"the second sight. They are protected from light-based  ",
		"attacks.                                               ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Elder Guardian*/
	{	"Elder Guardians have been completely designed to defend",
		"their assigned Elder. A few Elder Guardians have lost  ",
		"the Elder they should guard and have joined the League,",
		"as a means to find back their protegee. They are slow, ",
		"not very bright but incredibly tough. They cannot use  ",
		"mortal food, only Ambrosia or magical means can sustain",
		"them. They have awesome defences, they cannot be bled  ",
		"or stunned. They are naturally resistant to poison and ",
		"have Second Sight.                                     ",
		NULL,NULL,NULL,NULL,NULL,},
	/*Horror*/
	{	"Some of the Elder have become Horrors, after recovering",
		"from grievous wounds their body has changed into a     ",
		"nightmarish creature. Slimy, their faces covered with  ",
		"tentacles they have gained even more mental powers at  ",
		"the cost of frailty. They can gain the Second Sight,   ",
		"sense minds from a distance and project mental energies",
		"in a direct attack.                                    ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL},
};

static cptr afflictions_strings[] =
{
	"Vampire",
	"Werewolf",
	"Skeleton",
	"Mummy",
	"Spectre",	
};

static cptr afflictions_descriptions[COUNT_AFFLICTIONS][COUNT_LINES] =
{
/*Vampire*/
{   "Vampires are mostly originating from the Karpates. They",
	"need to sustain themselves with blood and cannot bear  ",
	"light. They can see in the dark, so they do not need   ",
	"torches or lanterns. They resist poison, nether, cold, ",
	"bleeding and draining attacks. They are superior beings",
	"safe for their wisdom.                                 ",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL},
/*Werewolf*/
{   "Werewolves are mostly originating from the Black Forest",
	"in Germany. They tend to kill their beloved ones under ",
	"the full moon, so a lot of them leave their homes and  ",
	"wander. Experienced werewolves can trigger the change  ",
	"to Wolf at will. Canines are neutral to Werewolves and ",
	"will not attack them.                                  ",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL},
/*Skeleton*/
{   "Skeletons in Hellband are the remains of the poor souls",
	"that annoyed an African medicin man. They have can bind",
	"the soul to the bones of a person, rotting away all the",
	"skin in the progress. Not even ambrosia can feed them, ",
	"they rely soulely on magical means to feed themselves. ",
	"They are protected from bleeding, shards, poison, cold ",
	"and life draining. They all have the Second Sight.     ",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL},
/*Mummy*/
{   "Mummies are the new zombies, and I am all out of witty ",
	"description. So here's the deal. If you play a Mummy,  ",
	"you really should take the effort to write me a nice   ",
	"overview and I'll include it in Hellband.              ",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL},
/*Spectre*/
{   "Spectres are incorporeal undead spirits. As such, they ",
	"can walk through walls. This hurts them slightly, and  ",
	"it can leave them vulnerable. Their ectoplasmic form   ",
	"lacks strength and stamina, although it does not bleed.",
	"Having an unnatural lifeforce, spectres resist nether, ",  
	"poison, cold and life draining. In fact nether attacks ",
	"heal them up. They have the Second Sight, can scare and",
	"detect surrounding minds. Like most undead Spectres    ",
	"can only feed on Ambrosio or with magical means. Their ",
	"glowing ectoplasmic form emits light much like a torch.",
	NULL,
	NULL,
	NULL,
	NULL},
};

static cptr signs_strings[] =
{
	"Free",
	"Born under Draco",
	"Born under Serpens",
	"Born under Plutus",
	"Born under Morui", /*  Morui , Orui , Orion , */  
};

static cptr signs_descriptions[COUNT_AFFLICTIONS][COUNT_LINES] =
{
/*Free*/
{   "You have been born under no particular constellation,  ",
	"causing concern among the Elder Gods. You have no      ",
	"special powers or weaknesses.                          ",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL},
/*Draco*/
{   "The constellation Draco or 'Dragon' confers under rare ",
	"circumstances dragon powers to newborn children. Later ",
	"in their life they will discover resistance to many    ",
	"elements, they will also find that they can shapeshift ",
	"into a Dragonling; scaled, winged and capable to breath",
	"fire and other elements.                               ",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL},
/*Serpens*/
{   "The constellation Serpens or 'Serpent' confers under   ",
	"rare conditions powers of and over snakes. People born ",
	"under this constellation can resist poison and will not",
	"be attacked by snakes and serpents. They also can be   ",
	"very stealthy.                                         ",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL,
	NULL},
/*Plutus*/
{   "Even though Plutus' star is not classified under modern",
	"astronomy, it's effects on newborns can be profound.The",
	"need to amass large fortunes and to tell whether things",
	"are valuable or not. They also have the Second Sight,  ",
	"their belongings are protected from disenchantment and ",
	"they tend to discover things that were meant to stay   ",
	"hidden. The only drawback they have is that they cannot",
	"easily part with their money, they can only spend 10%  ",
	"of it at one time.                                     ",
	NULL,
	NULL,
	NULL,
	NULL,
	NULL},
/*Morui*/
{   "Stories are told of the people from the star Morui, now",
	"more commonly called Orion. It is said that they have  ",
	"mingled with humans and that their genes are stronger  ",
	"with children born under Orion. People born under Morui",
	"are better in every way save for an odd mind. They grow",
	"a tough subdermal chitin that resists acid and their   ",
	"thoughts are impossible to confuse. They can grow wings",
	"that help avoid pits and falls. As they get more       ",
	"experienced, they also get faster and gain the ability ",
	"to spit acid.                                          ",
	NULL,
	NULL,
	NULL,
	NULL},
};



static cptr classes_descriptions[MAX_CLASS][COUNT_LINES] =
{
	/*Warrior*/
	{   "Warriors are the simplest class. They get no magic or  ",
		"special abilities, other than learning to resist fear  ",
		"(min lev = 30). They simply fight. However, they are   ",
		"are tougher and better at fighting than any other.     ",
		NULL,  
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Mage*/
	{   "Mages study arcane magic, but do not specialize as     ",
		"strongly as high-mages do. Mages receive two realms of ",
		"magic of their choice. Mages struggle with combat when ",
		"not using spells.                                      ",
		NULL,  
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},
	/*Priest*/
	{   "Priests are divine magic specialists. Whilst not as    ",
		"good at combat as paladins, they are better at magic. ",
		"Priests get divine magic from either miracles or the   ",
		"death realm plus one other realm (although they can't  ",
		"take miracles and death magic. Priests who learn death ",
		"magic are called Cultists. Priests take religious vows ",
		"which prevent them from using edged weapons unless     ",
		"those weapons are blessed.",
		NULL,NULL,NULL,NULL,NULL,
		NULL},	
	/*Rogue*/
	{   "Rogues are masters of stealth. Although they are not as",
		"good as warriors in a straight fight, they can backstab",
		"sleeping or fleeing opponents doing large amounts of   ",
		"damage. Rogues also learn a very small amount of arcane",
		"magic from either the death, tarot or folk realm.     ",
		"Rogues who learn death magic are called Assassins.     ",
		"Rogues who learn tarot magic are called Card Sharps. ",
		"Rogues who learn folk magic are called Thieves.        ",
		NULL,NULL,NULL,NULL,NULL,
		NULL},	
	/*Ranger*/
	{   "Rangers are decent fighters, although they specialize  ",
		"in missile weapons. Like druids, they use divine magic ",
		"from the Nature realm. They are not as good as druids  ",
		"at nature magic, but make up for it by also learning a ",
		"second realm.                                          ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},	
	/*Paladin*/
	{   "Paladins are holy warriors. There are two types - true ",
		"Paladins and Death Knights. True paladins get divine   ",
		"magic from the Miracles realm, whereas death knights   ",
		"get divine magic from the Death realm. In either case, ",
		"their magic is not as strong as that of a priest, but  ",
		"they make up for this by fighting almost as well as a  ",
		"warrior does. Paladins can learn to resist the effects ",
		"of fear at a higher level.                             ",
		NULL,NULL,NULL,NULL,NULL,
		NULL},	
	/*Warrior-Mage*/
	{   "Warrior mages combine reasonable combat skills with two",
		"realms of arcane magic. One of their realms of magic   ",
		"must be Charms, but the other can be any. They are not ",
		"quite as good at fighting as warriors, and not quite as",
		"good at magic as true mages, but they make up for their",
		"lack of strength by combining it with a lack of        ",
		"weakness.                                              ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL},	
	/*Hell Knight*/
	{   "Hell Knights have made pacts with an infernal patron in",
		"exchange for physical prowess. As such, they are good  ",
		"warriors. Their patrons give them a small amount of    ",
		"divine magic from the chaos realm, and occasionally    ",
		"give them other rewards too. Hell Knights can learn to ",
		"resist the effects of chaos and fear.                  ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,
		NULL},	
	/*Mystic*/
	{   "Mystics are martial artists. As such they are masters  ",
		"of unarmed combat and increase their speed as they gain",
		"experience. However, they are severely hampered by     ",
		"wearing heavy armour. With experience, they can shrug  ",
		"off slowing and paralyzing attacks. As part of their   ",
		"meditations, mystics learn divine magic from the       ",
		"Somatic realm.                                         ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL},	
	/*Mindcrafter*/
	{   "Mindcrafters rely on the supernatural powers that their",
		"mind is capable of producing. Many of their powers are ",
		"similar to spells and are used in the same way. Some   ",
		"powers, however, are simply passive, not requiring     ",
		"active use. Mindcrafters can resists fear and confusion",
		". They can sustain their wisdom, and even sense other  ",
		"minds once they are very experienced. They can handle  ",
		"themselves in combat.                                  ",
		NULL,NULL,NULL,NULL,NULL,
		NULL},		
	/*High-Mage**/
	{   "High mages study arcane magic from a single realm to   ",
		"the exclusion of any other magic. As such, their       ",
		"magical abilities are purer than most other classes,   ",
		"and they get more spell points than other classes do. ",
		"However, their intense study leaves them weak in combat",
		"when not using spells. High mages of different realms  ",
		"have different names: Vivimancer, Sorceror, Naturist,  ",
		"Hell Knight, Necromancer, Summoner, Hedge Wizard or Zen",
		"Master.                                                ",
		NULL,NULL,NULL,NULL,NULL},			
	/*Druid**/
	{   "Druids are nature worshippers. As such, they use divine",
		"magic from the realm of Nature. They are better at     ",
		"nature magic than any other class. Like priests, druids",
		"are not allowed to use edged weapons unless those      ",
		"weapons are blessed.                                   ",
		NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL},		
	/*Warlock**/
	{   "Warlocks are people who have studied the magical       ",
		"arts of demon magic with the aid of an infernal patron.",
		"They are an arcane spell user, getting demonic spell   ",
		"and the choice of any other realm. They are better at  ",
		"demonic magic than any other class. Warlocks have      ",
		"an infernal patron who may bestow gifts upon them, and ",
		"they can learn how to resist the effects of chaos.     ",
		"Warlocks have great difficulty wielding any weapon     ",
		"that is not a weapon of chaos, since their pact with   ",
		"their patron involves only using the power of chaos.   ",
		NULL,NULL,NULL,
		NULL},		
	
};

/* Did the player already decide on stat points */
bool points_decided;
/* Storage for the decided stat points, as these get overwritten */
s16b cache_stat_max[6];	/* Cached "maximal" stat values */
s16b cache_stat_cur[6];	/* Cached "natural" stat values */

/* Allow player to modify the character by spending points */
static bool point_mod_player(void)
{
	char b1 = '[';
	char b2 = ']';
	char stat;
	char modpts[4] = "none";
	int x = 0;
	int i, points;
	
	/* Check first if the player already decided once, if so, retrieve those values */
	if( points_decided == TRUE )
	{
		for( i = 0 ; i < 6 ; i++ )
		{
			p_ptr->stat_cur[i] = cache_stat_cur[i];
			p_ptr->stat_max[i] = cache_stat_max[i];
		}
		return TRUE;
	}


	points = 34;
	sprintf(modpts,"%d",points);
	clear_from(23);
	while(1)
	{ 
		/* reset variable */
		i = 0; 

		/* Calculate the bonuses and hitpoints */
		p_ptr->update |= (PU_BONUS | PU_HP);

		/* Update stuff */
		update_stuff();

		/* Fully healed */
		p_ptr->chp = p_ptr->mhp;

		/* Fully rested */
		p_ptr->csp = p_ptr->msp;

		/* Display the player */
		display_player(0);

		/* Display Stat Menu */
		clear_from(23);
		sprintf(modpts,"%d",points);

		Term_putstr(73,1,-1,TERM_WHITE,"<-S/s->");
		Term_putstr(73,2,-1,TERM_WHITE,"<-I/i->");
		Term_putstr(73,3,-1,TERM_WHITE,"<-W/w->");
		Term_putstr(73,4,-1,TERM_WHITE,"<-D/d->");
		Term_putstr(73,5,-1,TERM_WHITE,"<-C/c->");
		Term_putstr(73,6,-1,TERM_WHITE,"<-H/h->");

		Term_gotoxy(2, 23);
		Term_addch(TERM_WHITE, b1);
		if(points == 0)
		{
			Term_addstr(-1, TERM_GREEN, modpts);
		}
		else if(points > 0)
		{
			Term_addstr(-1, TERM_YELLOW, modpts);
		}
		else
		{
			Term_addstr(-1, TERM_RED, modpts);
		}
		Term_addstr(-1, TERM_WHITE, " points left. Press 'ESC' whilst on 0 points to finish.");
		Term_addch(TERM_WHITE, b2);
		/* Get an entry */
		stat = inkey();

		/* ESC goes back to previous menu */
		if((stat == ESCAPE) && (points == 0)) break;

		/* Assign values to entries, stats 0 to 5 */
		switch(stat)
		{

			/* The index to a specific stat is retrieved */
		case 's':
			i = 1;
			break;
		case 'S':
			i = 1;
			break;
		case 'i':
			i = 2;
			break;
		case 'I':
			i = 2;
			break;
		case 'w':
			i = 3;
			break;
		case 'W':
			i = 3;
			break;
		case 'd':
			i = 4;
			break;
		case 'D':
			i = 4;
			break;
		case 'c':
			i = 5;
			break;
		case 'C':
			i = 5;
			break;
		case 'h':
			i = 6;
			break;
		case 'H':
			i = 6;
			break;
		default:
			i = 0;
		}  

		/* Test for invalid key */
		if(!i) continue;
		i--;

		/* Test for lower case (add to stat) or 
		upper case (subtract stat) */
		if(islower(stat) ) /* ('a' < stat) */
		{
			if(points <= 0)
				continue;
			/* different conditions for maximize on */
			if(maximise_mode) 
			{
				/* Max stat increase */
				if(p_ptr->stat_max[i] < 17)
				{
					p_ptr->stat_cur[i] = ++p_ptr->stat_max[i];
				}
				else
				{
					continue;
				}
			}
			else
			{
				/* Max stat increase, maximize off */
				x = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];
				if(x > 8) x = 8;
				if(x > 0) x *= 13;
				if(p_ptr->stat_max[i] < 18 + x)    
				{
					if(p_ptr->stat_max[i]> 17)
					{
						p_ptr->stat_max[i] += 10;
						p_ptr->stat_cur[i] += 10;
					}
					else
					{
						p_ptr->stat_cur[i] = ++p_ptr->stat_max[i];
					}
				}
				else
				{
					continue;
				}
			}

			/* Higher stats linearly cost more */
			if(p_ptr->stat_max[i] > 97) points--; 
			if(p_ptr->stat_max[i] > 67) points--;
			if(p_ptr->stat_max[i] > 18) points--;
			if(p_ptr->stat_max[i] > 14) points--;
			if(p_ptr->stat_max[i] > 3)  points--;
			continue;
		}
		else    /* Reduce stat case */
		{ 
			if(p_ptr->stat_use[i] > 3)
			{
				if(p_ptr->stat_max[i] > 27)
				{ 
					p_ptr->stat_max[i] -= 10;
					p_ptr->stat_cur[i] -= 10;
				}
				else
				{
					p_ptr->stat_cur[i] = --p_ptr->stat_max[i];
				}
			}
			else
			{
				continue;
			}
			/* Higher stats yield more mod points */
			if(p_ptr->stat_max[i] > 87) points++; 
			if(p_ptr->stat_max[i] > 57) points++;
			if(p_ptr->stat_max[i] > 17) points++;
			if(p_ptr->stat_max[i] > 13) points++;
			if(p_ptr->stat_max[i] > 2)  points++;
			continue;
		}
	}
	
		/* Store values into cache */
		for( i = 0 ; i < 6 ; i++ )
		{
			cache_stat_cur[i] = p_ptr->stat_cur[i];
			cache_stat_max[i] = p_ptr->stat_max[i];
		}
		/*Indicate the cache can be used*/
		points_decided = TRUE;
	
	return TRUE;
}

/*
 * Random Name Generator
 * based on a Javascript by Michael Hensley
 * "http://geocities.com/timessquare/castle/6274/"
 */
void create_random_name(int race, byte sex , char *name)
{
	/* Paranoia */
	if (!name) return;

	/* Select the monster type */
	switch (race)
	{
	/* Create the monster name */
	case DWARF:
	case GUARDIAN:
		strcpy(name, dwarf_syllable1[rand_int(sizeof(dwarf_syllable1) / sizeof(char*))]);
		strcat(name, dwarf_syllable2[rand_int(sizeof(dwarf_syllable2) / sizeof(char*))]);
		strcat(name, dwarf_syllable3[rand_int(sizeof(dwarf_syllable3) / sizeof(char*))]);
		break;
	case GIANT:
		strcpy(name, giant_male_firstnames[rand_int(sizeof(giant_male_firstnames) / sizeof(char*))]);			
		break;
	case TITAN:
		break;
		if( sex == SEX_MALE )
		{
			strcpy(name, titan_male_firstnames[rand_int(sizeof(titan_male_firstnames) / sizeof(char*))]);
		}
		else
		{
			strcpy(name, titan_female_firstnames[rand_int(sizeof(titan_female_firstnames) / sizeof(char*))]);
		}
	case LEPRECHAUN:
		strcpy(name, celtic_male_firstnames[rand_int(sizeof(celtic_male_firstnames) / sizeof(char*))]);
		break;
	case ELF:
	case FAE:		
		strcpy(name, elf_syllable1[rand_int(sizeof(elf_syllable1) / sizeof(char*))]);
		strcat(name, elf_syllable2[rand_int(sizeof(elf_syllable2) / sizeof(char*))]);
		strcat(name, elf_syllable3[rand_int(sizeof(elf_syllable3) / sizeof(char*))]);
		break;
	case GNOME:
		strcpy(name, gnome_syllable1[rand_int(sizeof(gnome_syllable1) / sizeof(char*))]);
		strcat(name, gnome_syllable2[rand_int(sizeof(gnome_syllable2) / sizeof(char*))]);
		strcat(name, gnome_syllable3[rand_int(sizeof(gnome_syllable3) / sizeof(char*))]);
		break;
	case FLORENTIAN:
		if( sex == SEX_MALE )
		{
			strcpy(name, italian_male_firstnames[rand_int(sizeof(italian_male_firstnames) / sizeof(char*))]);
		}
		else
		{
			strcpy(name, italian_female_firstnames[rand_int(sizeof(italian_female_firstnames) / sizeof(char*))]);
		}
		strcat(name, " " );
		strcat(name, italian_last_names[rand_int(sizeof(italian_last_names) / sizeof(char*))]);
		break;
	case GIPSY:
		if( sex == SEX_MALE )
		{
			strcpy(name, romani_male_firstnames[rand_int(sizeof(romani_male_firstnames) / sizeof(char*))]);
		}
		else
		{
			strcpy(name, romani_female_firstnames[rand_int(sizeof(romani_female_firstnames) / sizeof(char*))]);
		}
		strcat(name, " " );
		strcat(name, romani_last_names[rand_int(sizeof(romani_last_names) / sizeof(char*))]);
		break;
	case NORDIC:
	case TROLL:
		if( sex == SEX_MALE )
		{
			strcpy(name, nordic_male_firstnames[rand_int(sizeof(nordic_male_firstnames) / sizeof(char*))]);
		}
		else
		{
			strcpy(name, nordic_female_firstnames[rand_int(sizeof(nordic_female_firstnames) / sizeof(char*))]);
		}
		strcat(name, " " );
		strcat(name, nordic_last_names[rand_int(sizeof(nordic_last_names) / sizeof(char*))]);
		break;
	case MUMMY:
	case SKELETON:
	case ATLANTIAN:	
	case SPECTRE:
		if( sex == SEX_MALE )
		{
			strcpy(name, egypt_male_firstnames[rand_int(sizeof(egypt_male_firstnames) / sizeof(char*))]);
		}
		else
		{
			strcpy(name, egypt_female_firstnames[rand_int(sizeof(egypt_female_firstnames) / sizeof(char*))]);
		}
		strcat(name, " " );
		strcat(name, egypt_last_names[rand_int(sizeof(egypt_last_names) / sizeof(char*))]);
		break;
	case WEREWOLF:
	case VAMPIRE:
	case OGRE:		
		strcpy(name, human_syllable1[rand_int(sizeof(human_syllable1) / sizeof(char*))]);
		strcat(name, human_syllable2[rand_int(sizeof(human_syllable2) / sizeof(char*))]);
		strcat(name, human_syllable3[rand_int(sizeof(human_syllable3) / sizeof(char*))]);
		break;
	case KOBOLD:
		strcpy(name, swamp_syllable1[rand_int(sizeof(swamp_syllable1) / sizeof(char*))]);
		strcat(name, swamp_syllable2[rand_int(sizeof(swamp_syllable2) / sizeof(char*))]);
		strcat(name, swamp_syllable3[rand_int(sizeof(swamp_syllable3) / sizeof(char*))]);
		break;
	case HORROR:
	case ELDER:	
		strcpy(name, illithid_syllable1[rand_int(sizeof(illithid_syllable1) / sizeof(char*))]);
		strcat(name, illithid_syllable2[rand_int(sizeof(illithid_syllable2) / sizeof(char*))]);
		strcat(name, illithid_syllable3[rand_int(sizeof(illithid_syllable3) / sizeof(char*))]);
		break;
	case NEPHILIM:
	case IMP:
	case DEVILSPAWN:
	case SUCCUBUS:
	case LILI:
		strcpy(name, angel_syllable1[rand_int(sizeof(angel_syllable1) / sizeof(char*))]);
		strcat(name, angel_syllable2[rand_int(sizeof(angel_syllable2) / sizeof(char*))]);
		strcat(name, angel_syllable3[rand_int(sizeof(angel_syllable3) / sizeof(char*))]);
		break;
		/* Create an empty name */
	default:
		name[0] = '\0';
		break;
	}
}

u16b choose_realm(u16b choices)
{
	int picks[MAX_REALM] = {0};
	int byteflag = 1;
	int n, i, choice, dir;
	char c;
	/* These vars were used for a) , one day they  might come back
	int k;	char p2 = ')';
	*/
	char buf[80];
	
	cptr str;
	
	/* Yah, do me own centering logic, bad konijn! */
	int screen_width = 80;
	
	/*Collect all chooseable realms, store them in array*/
	/*Note that n contains how many realms can be chosen*/
	n = 0;
	for( i = 1 ; i < MAX_REALM+1 ; i ++ )
	{
		if( (choices & byteflag) && p_ptr->realm1 != i)
		{
			picks[n]=i;
			n++;
		}
		byteflag=byteflag*2; 
	}
	
	/* Load new birth screen with restart option ( yah, a waste of bytes, but also it guarantees a clean screen */	
	do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );
	
	/* Get Vocation for da question*/
	cp_ptr = &class_info[p_ptr->pclass];
	str = cp_ptr->title;
	
	/* Choose a realm ?*/
	choice = 0;
	while (1)
	{
		
		sprintf(buf,"What realm will you master, %s?", str);
		c_put_str(TERM_YELLOW, buf, 5, ((screen_width-(int)strlen(buf))>>1) );
		
		for(i=0;i<n;i++)
			c_put_str(TERM_L_BLUE, i==choice?">":" " , 8+i , 2 );			
				
		for(i=0;i<n;i++)
			c_put_str(i==choice?TERM_L_BLUE:TERM_L_WHITE, realm_names[picks[i]].name , 8+i, 3);
		
/*		for(i=0;i<COUNT_LINES;i++)
			put_str( races_descriptions[choice][i] , 8+i , 23 );*/
		
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S' || c == 's') return (0);		
		if (c == '*')
		{
			choice = randint(COUNT_RACES);
		}
		if (c == '?') do_cmd_help(syshelpfile_birth);
		if (c == '=')
		{
			Term_save();
			do_cmd_options_aux(7,"Startup Options");
			Term_load();
		}
		if( ( c== ' ') || (c == '\n') || (c == '\r') ) break;	
		/* Look up the direction */
		dir = get_keymap_dir(c);
		if(dir==2 || c=='2')
			choice=choice+1==n?0:choice+1;
		if(dir==8 || c=='8')
			choice=choice==0?n-1:choice-1;
		else bell();
	}		
	return picks[choice];
	
}

u16b choose_realm_randomly(u16b choices)
{
	int picks[MAX_REALM] = {0};
	int k, n;
	u16b  bitflag;
	byte realm;
	
	bitflag = 1;
	n = 0;
	
	for( realm = 1 ; realm <= MAX_REALM ; realm++ )
	{
		if( ( choices & bitflag ) && p_ptr->realm1 != realm )
		{
			picks[n] = realm;
			n++;
		}
		bitflag = bitflag * 2;
	}
	
	/* Get a realm */
	k=rand_range(0,n-1);
	return (picks[k]);
}

int get_realms()
{
	/* Shortcut */
	int pclas=p_ptr->pclass;
	/* Paranoia : set realms to null */
	p_ptr->realm1=p_ptr->realm2=0;
	/* Warriors and certain others get no realms */
	if(realm_choices[pclas][0] == (CH_NONE)) return TRUE;
	/* Other characters get at least one realm */
	p_ptr->realm1 = choose_realm( realm_choices[pclas][0] );
	/* Only some characters get a second realm */
	if(realm_choices[pclas][1] == (CH_NONE)) return TRUE;
	/* Other characters get at least one realm */
	p_ptr->realm2 = choose_realm( realm_choices[pclas][1] );
	return TRUE;
}

/* Get realms randomly without asking player */
void get_realms_randomly()
{
	/* Shortcut */
	int pclas=p_ptr->pclass;
	/* Paranoia : set realms to null */
	p_ptr->realm1=p_ptr->realm2=0;
	/* Warriors and certain others get no realms */
	if(realm_choices[pclas][0] == (CH_NONE)) return;
	/* Other characters get at least one realm */
	p_ptr->realm1 = choose_realm_randomly( realm_choices[pclas][0] );
	/* Only some characters get a second realm */
	if(realm_choices[pclas][1] == (CH_NONE)) return;
	/* Other characters get at least one realm */
	p_ptr->realm2 = choose_realm_randomly( realm_choices[pclas][1] );
}

/*
* Save the current data for later
*/
static void save_prev_data(void)
{
	int i;


	/*** Save the current data ***/

	/* Save the data */
	prev.age = p_ptr->age;
	prev.wt = p_ptr->wt;
	prev.ht = p_ptr->ht;
	prev.sc = p_ptr->sc;
	prev.au = p_ptr->au;

	/* Save the stats */
	for (i = 0; i < STAT_COUNT; i++)
	{
		prev.stat[i] = p_ptr->stat_max[i];
	}

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(prev.history[i], history[i]);
	}
}


/*
* Load the previous data
*/
static void load_prev_data(void)
{
	int        i;

	birther	temp;


	/*** Save the current data ***/

	/* Save the data */
	temp.age = p_ptr->age;
	temp.wt = p_ptr->wt;
	temp.ht = p_ptr->ht;
	temp.sc = p_ptr->sc;
	temp.au = p_ptr->au;
	temp.birthday = p_ptr->birthday;

	/* Save the stats */
	for (i = 0; i < STAT_COUNT; i++)
	{
		temp.stat[i] = p_ptr->stat_max[i];
	}

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(temp.history[i], history[i]);
	}


	/*** Load the previous data ***/

	/* Load the data */
	p_ptr->age = prev.age;
	p_ptr->wt = prev.wt;
	p_ptr->ht = prev.ht;
	p_ptr->sc = prev.sc;
	p_ptr->au = prev.au;

	/* Load the stats */
	for (i = 0; i < STAT_COUNT; i++)
	{
		p_ptr->stat_max[i] = prev.stat[i];
		p_ptr->stat_cur[i] = prev.stat[i];
	}

	/* Load the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(history[i], prev.history[i]);
	}


	/*** Save the current data ***/

	/* Save the data */
	prev.age = temp.age;
	prev.wt = temp.wt;
	prev.ht = temp.ht;
	prev.sc = temp.sc;
	prev.au = temp.au;

	/* Save the stats */
	for (i = 0; i < STAT_COUNT; i++)
	{
		prev.stat[i] = temp.stat[i];
	}

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(prev.history[i], temp.history[i]);
	}
}




/*
* Returns adjusted stat -JK-  Algorithm by -JWT-
*
* auto_roll is boolean and states maximum changes should be used rather
* than random ones to allow specification of higher values to wait for
*
* The "maximise_mode" code is important	-BEN-
*/
static int adjust_stat(int value, s16b amount, int auto_roll)
{
	int i;

	/* Negative amounts */
	if (amount < 0)
	{
		/* Apply penalty */
		for (i = 0; i < (0 - amount); i++)
		{
			if (value >= 18+10)
			{
				value -= 10;
			}
			else if (value > 18)
			{
				value = 18;
			}
			else if (value > 3)
			{
				value--;
			}
		}
	}

	/* Positive amounts */
	else if (amount > 0)
	{
		/* Apply reward */
		for (i = 0; i < amount; i++)
		{
			if (value < 18)
			{
				value++;
			}
			else if (maximise_mode)
			{
				value += 10;
			}
			else if (value < 18+70)
			{
				value += ((auto_roll ? 15 : randint(15)) + 5);
			}
			else if (value < 18+90)
			{
				value += ((auto_roll ? 6 : randint(6)) + 2);
			}
			else if (value < 18+100)
			{
				value++;
			}
		}
	}

	/* Return the result */
	return (value);
}




/*
* Roll for a characters stats
*
* For efficiency, we include a chunk of "calc_bonuses()".
*/
static void get_stats(void)
{
	int		i, j;

	int		bonus;

	int		dice[18];


	/* Roll and verify some stats */
	while (TRUE)
	{
		/* Roll some dice */
		for (j = i = 0; i < 18; i++)
		{
			/* Roll the dice */
			dice[i] = randint(3 + i % 3);

			/* Collect the maximum */
			j += dice[i];
		}

		/* Verify totals */
		if ((j > 42) && (j < 57)) break; /* 57 was 54...
										 I hate 'magic numbers' :< TY */
	}

	/* Acquire the stats */
	for (i = 0; i < STAT_COUNT; i++)
	{
		/* Extract 5 + 1d3 + 1d4 + 1d5 */
		j = 5 + dice[3*i] + dice[3*i+1] + dice[3*i+2];

		/* Save that value */
		p_ptr->stat_max[i] = j;

		/* Obtain a "bonus" for "race" and "class" */
		bonus = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

		/* Variable stat maxes */
		if (maximise_mode)
		{
			/* Start fully healed */
			p_ptr->stat_cur[i] = p_ptr->stat_max[i];

			/* Efficiency -- Apply the racial/class bonuses */
			stat_use[i] = modify_stat_value(p_ptr->stat_max[i], bonus);
		}

		/* Fixed stat maxes */
		else
		{
			/* Apply the bonus to the stat (somewhat randomly) */
			stat_use[i] = adjust_stat(p_ptr->stat_max[i], (s16b)bonus, FALSE);

			/* Save the resulting stat maximum */
			p_ptr->stat_cur[i] = p_ptr->stat_max[i] = stat_use[i];
		}
	}
}


/*
* Roll for some info that the auto-roller ignores
*/
static void get_extra(void)
{
	int		i, j;
	int		lastroll;
#ifdef SHOW_LIFE_RATE
	int         percent;
#endif



	/* Level one */
	p_ptr->max_plv = p_ptr->lev = 1;

	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp + cp_ptr->c_exp + bsp_ptr->r_exp;

	/* Hitdice */
	p_ptr->hitdie = rp_ptr->r_mhp + cp_ptr->c_mhp + bsp_ptr->r_mhp;

	/* Initial hitpoints */
	p_ptr->mhp = p_ptr->hitdie;

	/* Pre-calculate level 1 hitdice */
	player_hp[0] = p_ptr->hitdie;

	/* Roll out the hitpoints */

	/* 'Roll' the hitpoint values */
	lastroll = p_ptr->hitdie;
	for (i = 1; i < PY_MAX_LEVEL; i++)
	{
		player_hp[i]=lastroll;
		lastroll--;
		if(lastroll<1) lastroll = p_ptr->hitdie;
	}
	/* Now shuffle them */
	for(i=1;i<PY_MAX_LEVEL;i++)
	{
		j=randint(PY_MAX_LEVEL-1);
		lastroll=player_hp[i];
		player_hp[i]=player_hp[j];
		player_hp[j]=lastroll;
	}
	/* Make each a cumulative score */
	for(i=1;i<PY_MAX_LEVEL;i++)
	{
		player_hp[i] = player_hp[i-1] +player_hp[i];
	}
}


/*
* Get the racial history, and social class, using the "history charts".
*/
static void get_history(void)
{
	int		i, n;

    /* 15 lines for 1 history part should be more than enough */
    int     odds[15];
    int     social_class = 0;
    int     bg_index = 0;
    int     odds_index = 0;
    int     odds_total = 0;
    int     odd_counter = 0;
    int     odd_current = 0;
    bool    odd_found  = FALSE;

	char	*s, *t;
	char	buf[240];
    
    /* Hack^2 , bitflag will determine if background entry is valid or not for the player's race */
    u32b    race_bitflag = (1<<(p_ptr->prace+1));

	/* Clear the previous history strings */
	for (i = 0; i < 4; i++) history[i][0] = '\0';

	/* Clear the history text */
	buf[0] = '\0';

	/* Process the history */
	while ( background[ bg_index ].info != NULL )
	{
        if( background[bg_index].metadata & race_bitflag )
		{
            odds_index = 0;
            odds_total = 0;
            odd_current = 0;
            odd_found = FALSE;
            while( background[ bg_index ].metadata != END_GROUP )
			{
                odds_total += background[ bg_index ].frequency;
                odds[odds_index++] = background[ bg_index ].frequency;
                bg_index++;
            }
            /*Lame!! Blame brain rot.*/
            odds_total += background[ bg_index ].frequency;
            odds[odds_index++] = background[ bg_index ].frequency;            
            /*So which of the entries do we take*/
            odds_total = randint(odds_total);
            for( odd_counter = 0 ; odd_counter < odds_index && !odd_found ; odd_counter++)
			{
                odd_current += odds[odd_counter];
                if(  odd_current >= odds_total )
				{
                    odd_found = TRUE;
                    (void)strcat(buf, background[ bg_index - odds_index + odd_counter + 1 ].info );
                    social_class += background[ bg_index - odds_index + odd_counter + 1 ].bonus;
                }
            }
            if(!odd_found)
            {
                msg_fiddle("Could not find matching bg for %s" , background[ bg_index ].info );
            }
        }else{
            /* Loop til end of non-appropriate group */
            while( background[ bg_index ].metadata != END_GROUP )
                bg_index++;
        }
        /* Advance 1*/
        bg_index++;
  	}

	/* Verify social class */
	if (social_class > 100) social_class = 100;
	else if (social_class < 1) social_class = 1;

	/* Save the social class */
	p_ptr->sc = social_class;

	/* Skip leading spaces */
	for (s = buf; *s == ' '; s++) /* loop */;

	/* Get apparent length */
	n = (int)strlen(s);

	/* Kill trailing spaces */
	while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';


	/* Start at first line */
	i = 0;

	/* Collect the history */
	while (TRUE)
	{
		/* Extract remaining length */
		n = strlen(s);

		/* All done */
		if (n < 60)
		{
			/* Save one line of history */
			strcpy(history[i++], s);

			/* All done */
			break;
		}

		/* Find a reasonable break-point */
		for (n = 70; ((n > 0) && (s[n-1] != ' ')); n--) /* loop */;

		/* Save next location */
		t = s + n;

		/* Wipe trailing spaces */
		while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';

		/* Save one line of history */
		strcpy(history[i++], s);

		/* Start next line */
		for (s = t; *s == ' '; s++) /* loop */;
	}
}


/*
* Computes character's age, height, and weight
*/
static void get_ahw(void)
{
	/* Calculate the age */
	p_ptr->age = rp_ptr->b_age + randint(rp_ptr->m_age);
	p_ptr->birthday=randint(365);
	/* This isn't stored in the birther struct as it isn't important yet...*/
	p_ptr->startdate=randint(365);

	/* Calculate the height/weight for males */
	if (p_ptr->psex == SEX_MALE)
	{
		p_ptr->ht = randnor(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
		p_ptr->wt = randnor(rp_ptr->m_b_wt, rp_ptr->m_m_wt);
	}

	/* Calculate the height/weight for females */
	else if (p_ptr->psex == SEX_FEMALE)
	{
		p_ptr->ht = randnor(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
		p_ptr->wt = randnor(rp_ptr->f_b_wt, rp_ptr->f_m_wt);
	}
}

/*
* Get the player's starting money
*/
static void get_money(void)
{
	int        i, gold;

	/* Social Class determines starting gold */
	gold = (p_ptr->sc * 6) + randint(100) + 300;

	/* Process the stats */
	for (i = 0; i < STAT_COUNT; i++)
	{
		/* Mega-Hack -- reduce gold for high stats */
		if (stat_use[i] >= 18+50) gold -= 300;
		else if (stat_use[i] >= 18+20) gold -= 200;
		else if (stat_use[i] > 18) gold -= 150;
		else gold -= (stat_use[i] - 8) * 10;
	}

	/* Minimum 100 gold */
	if (gold < 100) gold = 100;

	/* Save the gold */
	p_ptr->au = gold;
}



/*
* Display stat values, subset of "put_stats()"
*
* See 'display_player()' for basic method.
*/
static void birth_put_stats(void)
{
	int		i, p;
	byte	attr;

	char	buf[80];


	/* Put the stats (and percents) */
	for (i = 0; i < STAT_COUNT; i++)
	{
		/* Put the stat */
		cnv_stat(stat_use[i], buf);
		c_put_str(TERM_L_GREEN, buf, 2 + i, 66);

		/* Put the percent */
		if (stat_match[i])
		{
			p = 1000L * stat_match[i] / auto_round;
			attr = (p < 100) ? TERM_YELLOW : TERM_L_GREEN;
			sprintf(buf, "%3d.%d%%", p/10, p%10);
			c_put_str(attr, buf, 2 + i, 73);
		}

		/* Never happened */
		else
		{
			c_put_str(TERM_RED, "(NONE)", 2 + i, 73);
		}
	}
}

/*
*  Initialise the matrix of quests
*/
void initialise_quests()
{
	int i,j;

	/* Start with no quests */
	for (i = 0; i < MAX_QUESTS; i++)
	{
		q_list[i].level = 0;
		q_list[i].r_idx = 0;
		q_list[i].cur_num = 0;
		q_list[i].max_num = 0;
	}

	/* Hack */
	j=0;
	for(i=0;i<MAX_R_IDX;i++)
	{
		if (r_info[i].flags1 & (RF1_ALWAYS_GUARD)) /* It's a quest monster */
		{
			q_list[j].level=r_info[i].level;
			q_list[j].r_idx=i;
			q_list[j].cur_num=1;
			q_list[j].max_num=1;
			j++;
		}
	}
}


/*
* Clear all the global "character" data
*/
static void player_wipe(void)
{
	int i;


	/* Hack -- zero the struct */
	WIPE(p_ptr, player_type);

    /* Evil patron is not set yet */
    p_ptr->evil_patron = -1;
    
	/* Wipe the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(history[i], "");
	}


	/* No weight */
	total_weight = 0;

	/* No items */
	inven_cnt = 0;
	equip_cnt = 0;

	/* Clear the inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_wipe(&inventory[i]);
	}


	/* Start with no artefacts made yet */
	for (i = 0; i < MAX_A_IDX; i++)
	{
		artefact_type *a_ptr = &a_info[i];
		a_ptr->cur_num = 0;
	}


	/* Start with no quests */
	/*initialise_quests();  DEAN */ 

	/* Reset the "objects" */
	for (i = 1; i < MAX_K_IDX; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Reset "tried" */
		k_ptr->tried = FALSE;

		/* Reset "aware" */
		k_ptr->aware = FALSE;
	}

	/* Reset the "alchemy" knowledge */
	for (i = 0; i < SV_POTION_MAX ; i++)
	{
		potion_alch[i].known1 = potion_alch[i].known2 =FALSE;
	}
	
	/* Reset the "monsters" */
	for (i = 1; i < MAX_R_IDX; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Hack -- Reset the counter */
		r_ptr->cur_num = 0;

		/* Hack -- Reset the max counter */
		r_ptr->max_num = 100;

		/* Hack -- Reset the max counter */
		if (r_ptr->flags1 & (RF1_UNIQUE)) r_ptr->max_num = 1;

		/* Clear player kills */
		r_ptr->r_pkills = 0;
	}


	/* Hack -- Well fed player */
	p_ptr->food = PY_FOOD_FULL - 1;


	/* Wipe the spells */
	spell_learned1 = spell_learned2 = 0L;
	spell_worked1 = spell_worked2 = 0L;
	spell_forgotten1 = spell_forgotten2 = 0L;
	for (i = 0; i < 64; i++) spell_order[i] = 99;


	/* Clear "Debug" options */
	debug_peek = FALSE;
	debug_hear = FALSE;
	debug_room = FALSE;
	debug_xtra = FALSE;
	debug_know = FALSE;
	debug_live = FALSE;
	debug_wild = FALSE;
	debug_mode = FALSE;
	
	/* Assume no winning game */
	total_winner = FALSE;

	/* Assume no panic save */
	panic_save = 0;

	/* Assume no Debugging */
	noscore = 0;
}


/*
 * Each player starts out with a few items, given as tval/sval pairs.
 * In addition, he always has some food and a few torches.
 */


#define ANY            255
#define ANY_BOOL_TRUE  254
#define ANY_BOOL_FALSE 253

birth_item birth_items[] =
{
{ ANY, CLASS_WARRIOR      ,&reallyTRUE          ,TV_RING       ,SV_RING_RES_FEAR               ,WORN    ,1 ,1 },
{ ANY, CLASS_WARRIOR      ,&reallyTRUE          ,TV_SWORD      ,SV_BROAD_SWORD                 ,WORN    ,1 ,1 },
{ ANY, CLASS_WARRIOR      ,&reallyTRUE          ,TV_HARD_ARMOR ,SV_CHAIN_MAIL                  ,WORN    ,1 ,1 },	
{ ANY, CLASS_MAGE         ,&reallyTRUE          ,TV_SWORD      ,SV_DAGGER                      ,WORN    ,1 ,1 },
{ ANY, CLASS_PRIEST       ,&reallyTRUE          ,TV_HAFTED     ,SV_MACE                        ,WORN    ,1 ,1 },
{ ANY, CLASS_ROGUE        ,&reallyTRUE          ,TV_SWORD      ,SV_DAGGER                      ,WORN    ,1 ,1 },
{ ANY, CLASS_ROGUE        ,&reallyTRUE          ,TV_SOFT_ARMOR ,SV_SOFT_LEATHER_ARMOR          ,WORN    ,1 ,1 },
{ ANY, CLASS_RANGER       ,&reallyTRUE          ,TV_SWORD      ,SV_BROAD_SWORD                 ,WORN    ,1 ,1 },	
{ ANY, CLASS_PALADIN      ,&reallyTRUE          ,TV_SWORD      ,SV_BROAD_SWORD                 ,WORN    ,1 ,1 },
{ ANY, CLASS_PALADIN      ,&reallyTRUE          ,TV_SCROLL     ,SV_SCROLL_PROTECTION_FROM_EVIL ,CARRIED ,1 ,1 },
{ ANY, CLASS_WARRIOR_MAGE ,&reallyTRUE          ,TV_SWORD      ,SV_SHORT_SWORD                 ,WORN    ,1 ,1 },
{ ANY, CLASS_HELL_KNIGHT  ,&reallyTRUE          ,TV_SWORD      ,SV_BROAD_SWORD                 ,WORN    ,1 ,1 },
{ ANY, CLASS_HELL_KNIGHT  ,&reallyTRUE          ,TV_HARD_ARMOR ,SV_METAL_SCALE_MAIL            ,WORN    ,1 ,1 },
{ ANY, CLASS_MYSTIC       ,&reallyTRUE          ,TV_POTION     ,SV_POTION_HEALING              ,CARRIED ,1 ,1 },
{ ANY, CLASS_MYSTIC       ,&reallyTRUE          ,TV_SOFT_ARMOR ,SV_ROBE                        ,WORN    ,1 ,1 },
{ ANY, CLASS_MINDCRAFTER  ,&reallyTRUE          ,TV_SWORD      ,SV_SMALL_SWORD                 ,WORN    ,1 ,1 },
{ ANY, CLASS_MINDCRAFTER  ,&reallyTRUE          ,TV_POTION     ,SV_POTION_RESTORE_MANA         ,CARRIED ,1 ,1 },
{ ANY, CLASS_MINDCRAFTER  ,&reallyTRUE          ,TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR          ,WORN    ,1 ,1 },
{ ANY, CLASS_HIGH_MAGE    ,&reallyTRUE          ,TV_SWORD      ,SV_DAGGER                      ,WORN    ,1 ,1 },
{ ANY, CLASS_HIGH_MAGE    ,&reallyTRUE          ,TV_BOOK_REALM1,1                              ,WORN    ,1 ,1 },
{ ANY, CLASS_DRUID        ,&reallyTRUE          ,TV_HAFTED     ,SV_QUARTERSTAFF                ,WORN    ,1 ,1 },
{ ANY, CLASS_DRUID        ,&reallyTRUE          ,TV_AMULET     ,SV_AMULET_BRILLIANCE           ,WORN    ,1 ,1 },
{ ANY, CLASS_DRUID        ,&reallyTRUE          ,TV_RING       ,SV_RING_SUSTAIN_MIND           ,WORN    ,1 ,1 },
{ ANY, ANY                ,&reallyTRUE          ,TV_SCROLL     ,SV_SCROLL_WORD_OF_RECALL       ,CARRIED ,1 ,1 },
{ ANY, ANY                ,&reallyTRUE          ,TV_SCROLL     ,SV_SCROLL_TELEPORT             ,CARRIED ,2 ,3 },
{ ANY, ANY_BOOL_TRUE      ,&(p_race.rations)    ,TV_FOOD       ,SV_FOOD_RATION                 ,CARRIED ,3 ,7 },
{ ANY, ANY_BOOL_FALSE     ,&(p_race.rations)    ,TV_SCROLL     ,SV_SCROLL_SATISFY_HUNGER       ,CARRIED ,2 ,5 },
{ ANY, ANY_BOOL_TRUE      ,&(p_race.hates_light),TV_SCROLL     ,SV_SCROLL_DARKNESS             ,CARRIED ,2 ,5 },
{ ANY, ANY_BOOL_TRUE      ,&(p_race.hates_light),TV_SCROLL     ,SV_SCROLL_LIGHT                ,CARRIED ,3 ,7 },
{ ANY, ANY_BOOL_FALSE     ,&(p_race.hates_light),TV_LITE       ,SV_LITE_LANTERN                ,WORN    ,1 ,1 },
};

static void player_outfit_helper( byte tval , byte sval, byte number , byte action )
{
	object_type	forge;
	object_type	*q_ptr;
	
	/* Get local object */
	q_ptr = &forge;
	
	/* Hack to initialize non-first spellbook of realm 1*/
	if (tval == TV_BOOK_REALM1 && sval > 0 ) 
		tval = TV_MIRACLES_BOOK + p_ptr->realm1 - 1;
	
	/*Prep the object with type and subtype*/
	object_prep(q_ptr, lookup_kind(tval, sval));
	
	/* Hack for posion dagger */
	if (tval == TV_SWORD && p_ptr->pclass == CLASS_ROGUE && p_ptr->realm1 == REALM_DEATH) 
		q_ptr->name2 = EGO_BRAND_POIS;
	
	/*Set the amount*/
	q_ptr->number = number;
	/* Treat them as if storebought to prevent trigger-happy squelching*/
	object_storebought(q_ptr);
	/*Carry or wear the item*/
	if( action == CARRIED)
		(void)inven_carry(q_ptr, FALSE);
	else 
		(void)outfit(q_ptr);
}


/*
* Init players with some belongings
*
* Having an item makes the player "aware" of its purpose.
*/
static void player_outfit(void)
{
	int i;

	/* Make sure first spellbooks is given */
	if( p_ptr->realm1 )
		/* Decided on which realm book, sval 0 , just 1 copy, carry in inventory */
		player_outfit_helper( TV_MIRACLES_BOOK + p_ptr->realm1 - 1, 0 , 1 , CARRIED );

	/* Make sure first spellbooks is given */
	if( p_ptr->realm2 )
		/* Decided on which realm book, sval 0 , just 1 copy, carry in inventory */
		player_outfit_helper( TV_MIRACLES_BOOK + p_ptr->realm2 - 1, 0 , 1 , CARRIED );

	/* Go over the birth items */
	for( i = 0 ; i < N_ELEMENTS( birth_items ) ; i++ )
	{
		/* Make sure the race matches */
		if( birth_items[i].prace == p_ptr->prace || birth_items[i].prace == ANY)
		{
			/* Make sure the class matches */
			if( birth_items[i].pclass == p_ptr->pclass || birth_items[i].pclass >= MAX_CLASS )
			{
				/* Make sure the boolean matches */
				if( *(birth_items[i].flag) == TRUE || ( *(birth_items[i].flag) == FALSE && birth_items[i].pclass == ANY_BOOL_FALSE ) )
				{
					/* Give the item */
					player_outfit_helper( birth_items[i].tval,
					                      birth_items[i].sval,
					                      (byte)rand_range( birth_items[i].minimum , birth_items[i].maximum ),
					                      birth_items[i].action );
				}
			}
		}
	}
}

/*
 * Wear an item out of nowhere ;)
 * Hack^2
 */
void outfit(object_type *q_ptr)
{
	int slot;
	object_type forge;
	object_type *o_ptr;

	o_ptr =q_ptr;

	/* Check the slot */
	slot = wield_slot(o_ptr);

	/* Get local object */
	q_ptr = &forge;

	/* Obtain local object */
	object_copy(q_ptr, o_ptr);

	/* Modify quantity */
	q_ptr->number = 1;

	/* Access the wield slot */
	o_ptr = &inventory[slot];

	/* Wear the new stuff */
	object_copy(o_ptr, q_ptr);

	/* Increase the weight */
	total_weight += q_ptr->weight;

	/* Increment the equip counter by hand */
	equip_cnt++;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Recalculate torch */
	p_ptr->update |= (PU_TORCH);

	/* Recalculate mana */
	p_ptr->update |= (PU_MANA);

	p_ptr->redraw |= (PR_EQUIPPY);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
}


static bool navigate_birth_screen( int *choice, int min, int max, bool *done )
{
	char c;
	int dir;
			
	c = inkey();
	if (c == 'Q') quit(NULL);
	if (c == 'S' || c == 's') return (FALSE);		
	if (c == '*')
	{
		*choice = rand_range(0,max-1);
		*done = TRUE;
	}
	if (c == '?') do_cmd_help(syshelpfile_birth);
	if (c == '=')
	{
		Term_save();
		do_cmd_options_aux(7,"Startup Options");
		Term_load();
	}
	if( ( c== ' ') || (c == '\n') || (c == '\r') )*done = TRUE;
	/* Look up the direction */
	dir = get_keymap_dir(c);
	if(dir==2 || c=='2')*choice=*choice+1==max?min:*choice+1;
	if(dir==8 || c=='8')*choice=*choice==min?max-1:*choice-1;
			
	return TRUE;
}

static void fill_birth_screen( char *title , int start , int end , int choice , int offset, cptr choices[], cptr descriptions[], byte mhp , byte exp, s16b stats[] , bool doplus, bool nolist )
{
	char buf[80];
	/* Yah, do me own centering logic, bad konijn! */
	int screen_width = 80;
	int i;

	/* Put title such as 'What's yer genus?" */
	c_put_str(TERM_YELLOW, title, 5, ((screen_width-strlen(title))>>1) );
	
	/* Put the > for the chosen option, remove phantom >'s at the same time */
	for(i=start;i<=end;i++)
		c_put_str(TERM_L_BLUE, i==choice?">":" " , 8+i , 2 );

	/* Put the options, the chosen option in L_BLUE, except if we dont want that list to be done here 
	   We might want that when the calling function does a specialized printing of the list
	*/
	if(!nolist)
	for(i=start;i<=end;i++)
		c_put_str(i==choice?TERM_L_BLUE:TERM_L_WHITE, choices[i+offset], 8+i, 3);
	
	/* Blank out description space to remove phantom text */
	for(i=0;i<COUNT_LINES;i++)
		put_str( descriptions[i]==NULL?"                                                       ":descriptions[i] , 8+i , 23 );
	
	/* Put Hit Dice if they are known*/
	if( mhp != 0 )
	{
		sprintf(buf,doplus?"Hite dice: +%d   ":"Hite dice: %d   ", mhp );
		put_str( buf , 7+COUNT_LINES , 23);
	}

	/* Put XP Factor if known*/
	if( exp != 0 )
	{
		sprintf(buf,doplus?"Experience Factor: +%d":"Experience Factor: %d", exp );
		put_str( buf , 7+COUNT_LINES , 55);
	}
	
	/* Put stat mods */
	sprintf(buf,"%s"," ");
	for( i=0; i < STAT_COUNT ; i++ )
	{
		if( stats[i] != 0 )
		{
			if(buf[0]==' ')
				sprintf(buf, "%s%d", stat_names[i] ,stats[i] );
			else
				sprintf(buf, "%s, %s%d", buf , stat_names[i] , stats[i] );
		}
	}
	put_str( buf , 6+COUNT_LINES , 23);
	
	/*Put the cursor next to the current choice for UI niceness*/
	/* Except of course if this routine isnt putting the choices */
	if(!nolist)
		c_put_str( TERM_L_BLUE, choices[choice+offset], 8+choice, 3);
}

/*
 *  Set stats in random mode, stat1 is highest, stat2 second highest and stat3 third highest stat to roll for
 *  I Have no clue whatsoever what the -9 lines are all about ;|
 */
static void fix_stat_limit( int mval[STAT_COUNT], byte stat1 , byte stat2 , byte stat3 )
{
	/*Fix highest stat*/
	stat_limit[stat1]=mval[stat1]-1;
	if (stat_limit[stat1] > 18) stat_limit[stat1] -= 9;
	/*Fix second highest stat*/
	stat_limit[stat2]=mval[stat2]-2;
	if (stat_limit[stat2] > 18) stat_limit[stat2] -= 9;
	if (stat_limit[stat2] > 18) stat_limit[stat2] -= 9;
	/*Fix third highest stat*/
	stat_limit[stat3]=mval[stat3]-4;
	if (stat_limit[stat3] > 18) stat_limit[stat3] -= 9;
	if (stat_limit[stat3] > 18) stat_limit[stat3] -= 9;
	if (stat_limit[stat3] > 18) stat_limit[stat3] -= 9;
	if (stat_limit[stat3] > 18) stat_limit[stat3] -= 9;
}


/*
 * Yeah, uh, this code here see got copy pasted by the previous author
 * and since I didnt want to double maintain, and have a smaller codebase
 * and have a smaller executable size, I put it here
 * it doesnt do anything specific really, except being called twice
 * hence the most original name '..._aux_aux'
 * Sorry, Konijn
 */

static bool player_birth_aux_aux( bool autoroll , bool point_mod){
	
	int stat_counter;
	int i;
	bool flag = FALSE;
	bool prev = FALSE;
	int mode = 0;
	char b1 = '[';
	char b2 = ']';
	char c;

	/* Clean up */
	clear_from(10);

	/*** Generate ***/

		/* Roll */
		while (TRUE)
		{
			/* Feedback */
			if (autoroll)
			{
				
				Term_clear();

				put_str("Name        :", 2, 1);
				put_str("Sex         :", 3, 1);
				put_str("Race        :", 4, 1);
				put_str("Class       :", 5, 1);

				c_put_str(TERM_L_BLUE, player_name, 2, 15);
				c_put_str(TERM_L_BLUE, sp_ptr->title, 3, 15);
				c_put_str(TERM_L_BLUE, rp_ptr->title, 4, 15);
				c_put_str(TERM_L_BLUE, class_sub_name[p_ptr->pclass][p_ptr->realm1], 5, 15);

				/* Label stats */
				for( stat_counter = 0 ; stat_counter < STAT_COUNT ; stat_counter++ )
					put_str( stat_names[stat_counter] , 2 + stat_counter , 61 );

				/* Note when we started */
				last_round = auto_round;

				/* Indicate the state */
				put_str("(Hit ESC to abort)", 11, 61);

				/* Label count */
				put_str("Round:", 9, 61);
			}

			/* Otherwise just get a character */
			else
			{
				/* Get a new character */
				if (point_mod)
				{
					for(i=0;i<STAT_COUNT;i++)
					{
						p_ptr->stat_cur[i] = p_ptr->stat_max[i] = 8;
					}
					point_mod_player();  
				}
				else
				{
					/* Get a new character */
					get_stats();
				}
			}

			/* Auto-roll */
			while (autoroll)
			{
				bool accept = TRUE;

				/* Get a new character */
				get_stats();

				/* Advance the round */
				auto_round++;

				/* Hack -- Prevent overflow */
				if (auto_round >= 1000000L) break;

				/* Check and count acceptable stats */
				for (i = 0; i < STAT_COUNT; i++)
				{
					/* This stat is okay */
					if (stat_use[i] >= stat_limit[i])
					{
						stat_match[i]++;
					}

					/* This stat is not okay */
					else
					{
						accept = FALSE;
					}
				}

				/* Break if "happy" */
				if (accept) break;

				/* Take note every 25 rolls */
				flag = (!(auto_round % 25L));

				/* Update display occasionally */
				if (flag || (auto_round < last_round + 100))
				{
					/* Dump data */
					birth_put_stats();

					/* Dump round */
					put_str(format("%6ld", auto_round), 9, 73);

					/* Make sure they see everything */
					Term_fresh();

					/* Delay 1/10 second */
					if (flag) Term_xtra(TERM_XTRA_DELAY, 100);

					/* Do not wait for a key */
					inkey_scan = TRUE;

					/* Check for a keypress */
					if (inkey()) break;
				}
			}

			/* Get a random name, again, for extra coolness */
			create_random_name(p_ptr->prace , p_ptr->psex , player_name);

			/* Flush input */
			flush();

			/* Display Mode */
			mode = 0;

			/* Roll for base hitpoints */
			get_extra();

			/* Roll for age/height/weight */
			get_ahw();

			/* Roll for social class */
			get_history();

			/* Roll for gold */
			get_money();

			/* Hack -- get a chaos patron even if you are not a Hell Knight */
			p_ptr->evil_patron = (randint(MAX_PATRON)) - 1;
			
			/*Clear all mutations*/
			p_ptr->muta1 = 0;
			p_ptr->muta2 = 0;
			p_ptr->muta3 = 0;

			/* Player is ready to move... */
			p_ptr->energy=1000;

			/* Player has no recal ritual yet */
			p_ptr->ritual = 0;

			/* Input loop */
			while (TRUE)
			{
				/* Calculate the bonuses and hitpoints */
				p_ptr->update |= (PU_BONUS | PU_HP);

				/* Update stuff */
				update_stuff();

				/* Fully healed */
				p_ptr->chp = p_ptr->mhp;

				/* Fully rested */
				p_ptr->csp = p_ptr->msp;
				p_ptr->energy = 1050;

				/* Display the player */
				display_player(mode);

				/* Prepare a prompt (must squeeze everything in) */
				Term_gotoxy(2, 23);
				Term_addch(TERM_WHITE, b1);
				Term_addstr(-1, TERM_WHITE, "'r' to reroll");
				if (prev) Term_addstr(-1, TERM_WHITE, ", 'p' for prev");
				if (mode) Term_addstr(-1, TERM_WHITE, ", 'h' for Misc.");
				else Term_addstr(-1, TERM_WHITE, ", 'h' for History");
				Term_addstr(-1, TERM_WHITE, ", or ESC to accept");
				Term_addch(TERM_WHITE, b2);

				/* Prompt and get a command */
				c = inkey();

				/* Quit */
				if (c == 'Q') quit(NULL);

				/* Start over */
				if (c == 'S' || c == 's'){
					/* If we restart at this point, then the player will want to re-choose stats most likely */
					points_decided = 0;
					return (FALSE);
				}

				/* Escape accepts the roll */
				if (c == ESCAPE) break;

				/* Reroll this character */
				if ((c == ' ') || (c == 'r')) break;

				/* Previous character */
				if (prev && (c == 'p'))
				{
					load_prev_data();
					continue;
				}

				/* Toggle the display */
				if ((c == 'H') || (c == 'h'))
				{
					mode = ((mode != 0) ? 0 : 1);
					continue;
				}

				/* Help */
				if (c == '?')
				{
					do_cmd_help(syshelpfile);
					continue;
				}

				/* Warning */
				bell();
			}

			/* Are we done? */
			if (c == ESCAPE) break;

			/* Save this for the "previous" character */
			save_prev_data();

			/* Note that a previous roll exists */
			prev = TRUE;
		}

		/* Clear prompt */
		clear_from(23);

		/*** Finish up ***/

		/* Allow name to be edited, recolour it, prepare savefile */
		get_name();

		/* Prompt for it */
		prt("['Q' to suicide, 'S' to start over, or ESC to continue]", 23, 10);

		/* Get a key */
		c = inkey();

		/* Quit */
		if (c == 'Q') quit(NULL);

		/* Start over */
		if (c == 'S' || c == 's') return (FALSE);

		/* Accept */
		return (TRUE);

}

/*
* Helper function for 'player_birth()'
*
* The delay may be reduced, but is recommended to keep players
* from continuously rolling up characters, which can be VERY
* expensive CPU wise.  And it cuts down on player stupidity.
*/
static bool player_birth_aux()
{
	int i, j, m, v, choice;
	int dir;
	//int mode = 0;
	int a_offset, a_end;

	//bool flag = FALSE;
	//bool prev = FALSE;
	bool quickstart = FALSE;
	bool done = FALSE;

	cptr str;
	
	char c;
	//int stat_counter;
	
	/* These vars were used for a) , one day they  might come back
		int n;	char p2 = ')';
	*/

	//char b1 = '[';
	//char b2 = ']';

	char buf[80];

	bool autoroll = FALSE;
	bool point_mod = TRUE;

	char inp[80];
	
	/* Yah, do me own centering logic, bad konijn! */
	int screen_width = 80;
	s16b zero_stats[] = {0,0,0,0,0,0};

	/*** Intro ***/
	
	/* Clear screen */
	Term_clear();
	
	choice = 0;
	
	do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth.txt" );
	
	/* Choose a sex*/
	while (1)
	{
		
		c_put_str(TERM_YELLOW, "Will you be a Lady or a Gentleman ?", 5, 22);

		for(i=0;i<COUNT_SEXES;i++)
			c_put_str(TERM_L_BLUE, i==choice?">":" " , 8+i , 2 );
		
		for(i=0;i<COUNT_SEXES;i++)
			c_put_str(i==choice?TERM_L_BLUE:TERM_L_WHITE, sexes_strings[i], 8+i, 3);
		
		for(i=0;i<COUNT_LINES;i++)
			put_str( sexes_descriptions[choice][i]==NULL?"":sexes_descriptions[choice][i] , 8+i , 23 );
		
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S' || c == 's')
		{
			quickstart = TRUE;
			break;
		}
		if (c == '*')
		{
			choice = rand_range(0,1);
			break;
		}
		if (c == '?') do_cmd_help(syshelpfile_birth);
		if (c == '=')
		{
			Term_save();
			do_cmd_options_aux(7,"Startup Options");
			Term_load();
		}
		if( ( c== ' ') || (c == '\n') || (c == '\r') ) break;	
		/* Look up the direction */
		dir = get_keymap_dir(c);
		if(dir==2 || c=='2')
			choice=choice+1==COUNT_SEXES?0:choice+1;
		if(dir==8 || c=='8')
			choice=choice==0?COUNT_SEXES-1:choice-1;
		else bell();
	}		

	if (quickstart)
	{
		/* Set sex */
		p_ptr->psex = (char)rand_range(0,1);
		sp_ptr = &sex_info[p_ptr->psex];
		str = sp_ptr->title;

		/*** Player race ***/
		while(1)
		{
			p_ptr->prace = (char)rand_range(0,COUNT_SUBRACES-1);
			hack_corruption = FALSE;
			if (p_ptr->prace==DEVILSPAWN) hack_corruption = TRUE;
			/*rp_ptr = &race_info[p_ptr->prace];*/
			p_race = race_info[p_ptr->prace];
			if( p_ptr->prace < AFFLICTED )
			{
				p_ptr->psign = (char)rand_range(0,COUNT_SIGNS-1);
				bsp_ptr = &sign_info[p_ptr->psign];				
			}
			if( p_ptr->prace != HUMAN && p_ptr->prace != AFFLICTED ) break;
		}

		/*** Player class ***/
		while(1)
		{
			p_ptr->pclass = (char)rand_range(0,MAX_CLASS-1);
			/* Analyze */
			cp_ptr = &class_info[p_ptr->pclass];
			mp_ptr = &realms_info[p_ptr->pclass];
			str = class_sub_name[p_ptr->pclass][p_ptr->realm1];
			/*Should be an authorized class*/
			if (rp_ptr->choice & (1L << p_ptr->pclass )) break;
		}

		/* Get a random name */
		create_random_name(p_ptr->prace , p_ptr->psex , player_name);
		/* Get random realms */
		get_realms_randomly();
		/* Initialize quests */
		MAX_Q_IDX = MAX_QUESTS;
		initialise_quests();

#ifndef FORBID_AUTOROLLER
		/* Set "autoroll" */
		autoroll = TRUE;
		/* Initialize */
		if (autoroll)
		{
			int mval[STAT_COUNT]; 
			/* Clear fields */
			auto_round = 0L;
			last_round = 0L;
			for (i = 0; i < STAT_COUNT; i++)
			{
				/* Reset the "success" counter */
				stat_match[i] = 0;
				/* Race/Class bonus */
				j = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];
				/* Obtain the "maximal" stat */
				m = adjust_stat(17, (s16b)j, TRUE);
				/* Save the maximum */
				mval[i] = m;
			}
			/* Hack in the minimum stats */
			for (i = 0; i < STAT_COUNT; i++)
			{
				stat_limit[i] = 0;
			}
			/* Save the minimum stat depending on class */
			switch(p_ptr->pclass)
			{
				case CLASS_WARRIOR:      fix_stat_limit( mval, A_STR , A_CON , A_DEX );break;
				case CLASS_MAGE:         fix_stat_limit( mval, A_INT , A_CON , A_STR );break;
				case CLASS_PRIEST:       fix_stat_limit( mval, A_WIS , A_CON , A_STR );break;
				case CLASS_ROGUE:        fix_stat_limit( mval, A_DEX , A_INT , A_CON );break;
				case CLASS_RANGER:       fix_stat_limit( mval, A_CON , A_DEX , A_INT );break;
				case CLASS_PALADIN:      fix_stat_limit( mval, A_STR , A_WIS , A_CON );break;
				case CLASS_WARRIOR_MAGE: fix_stat_limit( mval, A_STR , A_INT , A_CON );break;
				case CLASS_HELL_KNIGHT:  fix_stat_limit( mval, A_STR , A_INT , A_CON );break;
				case CLASS_MYSTIC:       fix_stat_limit( mval, A_DEX , A_WIS , A_CON );break;
				case CLASS_MINDCRAFTER:  fix_stat_limit( mval, A_WIS , A_CON , A_DEX );break;
				case CLASS_HIGH_MAGE:    fix_stat_limit( mval, A_INT , A_CON , A_STR );break;
				case CLASS_DRUID:        fix_stat_limit( mval, A_WIS , A_CHA , A_CON );break;
				case CLASS_WARLOCK:      fix_stat_limit( mval, A_INT , A_CON , A_DEX );break;
			}
		}

#endif /* ALLOW_AUTOROLLER */

		return player_birth_aux_aux( autoroll, FALSE );

	}
	else /* Interactive character */
	{

		/* Set sex */
		p_ptr->psex = choice;
		/* Get sex into a string */
		sp_ptr = &sex_info[p_ptr->psex];
		str = sp_ptr->address;
		/* Load new birth screen with restart option ( yah, a waste of bytes, but also it guarantees a clean screen */	
		do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );
		
		/* Choose a genus, set initial choice to 0, set up loop flag*/
		choice = 0;
		done = FALSE;
		
		while (!done)
		{
			/*Set up title*/
			sprintf(buf,"What is your genus, %s?", str);
			/*Set up screen*/
			fill_birth_screen( buf , 0 , COUNT_RACES-1 , choice , 0 , races_strings, races_descriptions[choice], 0, 0, zero_stats , FALSE, FALSE);
			/*Allow user to navigate/cancel/quit/read help*/
			if( navigate_birth_screen( &choice, 0, COUNT_RACES , &done ) == FALSE ) return FALSE;
		}
		
		/* Set temporary race until player has drilled down */
		p_ptr->prace = choice;
		
		/* Load new birth screen with restart option ( yah, a waste of bytes, but also it guarantees a clean screen */	
		do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );	
		
		/*Set some helper variables*/
		a_offset = subraces[p_ptr->prace][0];
		a_end = subraces[p_ptr->prace][1] - subraces[p_ptr->prace][0];
		
		/* Choose specific genus, set initial choice to 0, set up loop flag*/
		choice = 0;
		done   = FALSE;
		
		while (!done)
		{
			/*Set up title*/
			sprintf(buf,"What type of %s are you, %s?",  races_strings[p_ptr->prace] ,  str);
			/*Set up screen*/
			fill_birth_screen( buf , 0 , a_end , choice , a_offset , subraces_strings, subraces_descriptions[choice+a_offset], (race_info[choice+a_offset].r_mhp), (race_info[choice+a_offset].r_exp), race_info[choice+a_offset].r_adj , FALSE, FALSE);  
			/*Allow user to navigate/cancel/quit/read help*/
			if( navigate_birth_screen( &choice, 0 , a_end+1 , &done ) == FALSE ) return FALSE;
		}		
		
		/* Choice was just for UI, choice+a_offset has what we really want, which is now the new role of choice */
		choice = choice+a_offset;
		
		if( ( choice==SUCCUBUS || choice == LILI ) && p_ptr->psex == GENTLEMAN )
		{
			/*No cross dressing in my game ;)*/
			msg_note("Hellband does not support demon cross-dressers.");	
			/* Set sex */
			p_ptr->psex = LADY;
			/* Get sex into a string */
			sp_ptr = &sex_info[p_ptr->psex];
			str = sp_ptr->address;
		}
		
		if( ( choice==LEPRECHAUN || choice==LEPRECHAUN || choice==TITAN ) && p_ptr->psex == LADY )
		{
			/*No cross dressing in my game ;)*/
			msg_note("Hellband does not support cross-dressers.");	
			/* Set sex */
			p_ptr->psex = GENTLEMAN;
			/* Get sex into a string */
			sp_ptr = &sex_info[p_ptr->psex];
			str = sp_ptr->address;
		}
		
		/*Unless we are afflicated we get the right to be born under a constellation*/
		if( p_ptr->prace == R_HUMAN && choice!= AFFLICTED )
		{
			p_ptr->prace = choice;
			/* Choose specific birth sign, set initial choice to 0, set up loop flag*/
			choice = 0;
			done   = FALSE;
			/* Load new birth screen with restart option ( yah, a waste of bytes, but also it guarantees a clean screen */	
			do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );	
			
			while (!done)
			{
				/*Set up title*/
				sprintf(buf,"Which constellation were you born under, %s?",  str);
				/*Set up screen*/
				fill_birth_screen( buf , 0 , COUNT_SIGNS-1 , choice , 0 , signs_strings, signs_descriptions[choice], (sign_info[choice].r_mhp), (sign_info[choice].r_exp), sign_info[choice].r_adj , TRUE, FALSE );
				/*Allow user to navigate/cancel/quit/read help*/
				if( navigate_birth_screen( &choice, 0 ,COUNT_SIGNS , &done ) == FALSE ) return FALSE;
			}
			p_ptr->psign  = choice;
	}
		/* If we are afflicted, we get to choose which afflication*/
		else if( choice == AFFLICTED)
		{
			/* Choose specific affliction, set initial choice to 0, set up loop flag*/
			choice = 0;
			done = FALSE;
			/* Load new birth screen with restart option ( yah, a waste of bytes, but also it guarantees a clean screen */	
			do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );	
			
			while (!done)
			{
				/*Set up title*/
				sprintf(buf,"What is your affliction, %s?",  str);
				/*Set up screen*/
				fill_birth_screen( buf , 0 , COUNT_AFFLICTIONS-1 , choice , 0 , afflictions_strings, afflictions_descriptions[choice], 
				(race_info[choice+subraces[COUNT_RACES-1][1]+1].r_mhp), 
				(race_info[choice+subraces[COUNT_RACES-1][1]+1].r_exp), 
				race_info[choice+subraces[COUNT_RACES-1][1]+1].r_adj , TRUE, FALSE);
				/*Allow user to navigate/cancel/quit/read help*/
				if( navigate_birth_screen( &choice, 0 ,COUNT_AFFLICTIONS , &done ) == FALSE ) return FALSE;
			}			
			/* Serious hack, human afflictions must be the last afflictions!! or else !! damnation & all !!  */
			p_ptr->prace = choice+subraces[COUNT_RACES-1][1]+1;
			p_ptr->psign  = SIGN_FREE;
		}
		/*We just store the proper genus*/
		else
		{
			p_ptr->prace = choice;
			p_ptr->psign  = SIGN_FREE;	
		}
		/* Set sign */
		bsp_ptr = &sign_info[p_ptr->psign];
		
		/* Assume no corruptions */
		hack_corruption = FALSE;

		/* Set race */
		hack_corruption = FALSE;
		/* rp_ptr = &race_info[p_ptr->prace]; */
		p_race = race_info[p_ptr->prace];
		if (p_ptr->prace==DEVILSPAWN) hack_corruption= TRUE;	
		
		/* Get a random name now we have a race and a genus*/
		create_random_name(p_ptr->prace , p_ptr->psex , player_name);
		
		/* Roll for age/height/weight now that we have a race and a genus*/
		get_ahw();

		/* Choose specific class, set initial choice to 0, set up loop flag */
		choice = 0;
		done   = FALSE;
		/* Load new birth screen with restart option ( yah, a waste of bytes, but also it guarantees a clean screen */	
		do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );	

		while (!done)
		{
			/*Use helper variable to pass along class info*/
			cp_ptr = &class_info[choice];
			/*Set up screen title, because of silly double use of buf */
			sprintf(buf,"What is your vocation, %s?",  str);			
			/* Set up screen*/
			fill_birth_screen( buf , 0 , MAX_CLASS-1 , choice , 0 , NULL, classes_descriptions[choice], cp_ptr->c_mhp, cp_ptr->c_exp, cp_ptr->c_adj , TRUE, TRUE );
			/* Show classes , with braces if they are not advised */
			for(i=0;i<MAX_CLASS;i++)
			{
				cp_ptr = &class_info[i];
				if (!(rp_ptr->choice & (1L << i )))
				{
					sprintf(buf, "(%s)", cp_ptr->title);
				}
				else
				{
					sprintf(buf, "%s", cp_ptr->title);
				}
				c_put_str(i==choice?TERM_L_BLUE:TERM_L_WHITE, buf, 8+i, 3);
			}
			/*Damn, this is ugly just to put the cursor nicely ( basically print the choice again*/
			cp_ptr = &class_info[choice];
			if (!(rp_ptr->choice & (1L << choice )))
			{
				sprintf(buf, "(%s)", cp_ptr->title);
			}
			else
			{
				sprintf(buf, "%s", cp_ptr->title);
			}
			c_put_str(TERM_L_BLUE, buf, 8+choice, 3);
			
			/*Allow user to navigate/cancel/quit/read help*/
			if( navigate_birth_screen( &choice, 0 ,MAX_CLASS , &done ) == FALSE ) return FALSE;			
		}
		
		p_ptr->pclass = choice;
		cp_ptr = &class_info[p_ptr->pclass];
		mp_ptr = &realms_info[p_ptr->pclass];
		str = class_sub_name[p_ptr->pclass][p_ptr->realm1];

		/* Display */
		c_put_str(TERM_L_BLUE, class_sub_name[p_ptr->pclass][p_ptr->realm1], 5, 15);

		/* Clean up */

		clear_from(15);

		if(!get_realms())
			return FALSE;
		
		if(p_ptr->realm1)
		{
			if(p_ptr->realm2)
			{
				sprintf(buf,"%s/%s",realm_names[p_ptr->realm1].name,realm_names[p_ptr->realm2].name);
			}
			else
			{
				sprintf(buf,"%s",realm_names[p_ptr->realm1].name);
			}
		}

		/* Clear */
		clear_from(15);

		MAX_Q_IDX = MAX_QUESTS;
		initialise_quests();

#ifndef FORBID_AUTOROLLER

		/*** Autoroll ***/

		/* Set "autoroll" and "point_mod" */
		autoroll = use_autoroller;
		point_mod = (spend_points & !(use_autoroller));

		/* Initialize Autoroller if necessary */
		if (autoroll)
		{
			int mval[STAT_COUNT];
			
			do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );	
			sprintf(buf,"Enter minimum values for these stats");
			c_put_str(TERM_YELLOW, buf, 5, ((screen_width-strlen(buf))>>1) );			

			/* Clear fields */
			auto_round = 0L;
			last_round = 0L;

			/* Output the maximum stats */
			for (i = 0; i < STAT_COUNT; i++)
			{
				/* Reset the "success" counter */
				stat_match[i] = 0;

				/* Race/Class bonus */
				j = rp_ptr->r_adj[i] + cp_ptr->c_adj[i];

				/* Obtain the "maximal" stat */
				m = adjust_stat(17,(s16b) j, TRUE);

				/* Save the maximum */
				mval[i] = m;

				/* Extract a textual format */
				/* cnv_stat(m, inp); */

				/* Above 18 */
				if (m > 18)
				{
					sprintf(inp, "(Max of %2d):  |", (m - 19)/10 + 19);
				}

				/* From 3 to 18 */
				else
				{
					sprintf(inp, "(Max of %2d):  |", m);
				}

				/* Prepare a prompt */
				sprintf(buf, "%-5s%-20s", stat_names[i], inp);

				/* Dump the prompt */
				put_str(buf, 9 + i, 3);
			}

			/* Input the minimum stats */
			for (i = 0; i < STAT_COUNT; i++)
			{
				/* Get a minimum stat */
				while (TRUE)
				{
					/* char *s; */

					/* Move the cursor */
					put_str("", 9 + i, 30);

					/* Default */
					strcpy(inp, "");

					/* Get a response (or escape) */
					if (!askfor_aux(inp, 8)) inp[0] = '\0';

					v = atoi(inp);
					if(v>18) v=18+(v-18)*10;

					/* Break on valid input */
					if (v <= mval[i]) break;
				}

				/* Save the minimum stat */
				stat_limit[i] = (v > 0) ? v : 0;
			}
		}

#endif /* ALLOW_AUTOROLLER */

	return player_birth_aux_aux( autoroll, point_mod );
	}
	return TRUE;
}

/*
* Create a new character.
*
* Note that we may be called with "junk" leftover in the various
* fields, so we must be sure to clear them first.
*/
void player_birth(void)
{
	int n;

	/* Create a new character */
	while (1)
	{
		/* Wipe the player */
		player_wipe();

		/* Roll up a new character */
		if (player_birth_aux()) break;
	}


	/* Note player birth in the message recall */
	message_add(" ");
	message_add("  ");
	message_add("====================");
	message_add("  ");
	message_add(" ");


	/* Hack -- outfit the player */
	player_outfit();

	/* Shops */
	for (n = 0; n < MAX_STORES; n++)
	{
		/* Initialize */
		store_init(n);
	}
	
	store_maint_all(10);
}



