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

	s16b stat[6];

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

/* Player background information  */
typedef struct background_type background_type;
struct background_type
{
	cptr info;			    /* Textual History */
	byte frequency;			/* Frequency of this entry */
	s16b bonus;			    /* Social Class Bonus + 50 */
    u32b metadata;          /* validity check and end of group flag */
    
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
    
    {"You have left your family the moment you became a werewolf.",                UNLIKELY,            30,   BG_WEREWOLF},
    {"After some family members went missing, you left the family.",               LIKELY,              10,   0},
    {"You have slain most of your village and went hiding.",                       LIKELY,              -5,   END_GROUP},

    {"You have been captured by the League, forced now to do their bidding.",      UNLIKELY,            -5,   BG_WEREWOLF},
    {"You have joined the League, hoping to find a cure.",                         LIKELY,              10,   END_GROUP},
    
    {"You have awakened 1 millemium ago, and yearn to retreat soon.",              LIKELY,              35,   BG_ELDER},
    {"You have studied demonic magics for 700 years, and seek some adventure.",    UNLIKELY,            20,   0},
    {"You are on a quest to find your Maker since the world is born.",             RARE,                70,   END_GROUP},    
    
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
    {"green eyes, and fair complexion.",                                           UNLIKELY,            0,    END_GROUP},                                                                                                       
    
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
    
    {"You have become corrupted in the last century.",                             LIKELY,              0,    BG_HORROR},                                                                                                       
    {"You have become corrupted a millenium ago.",                                 UNLIKELY,            0,    0},                                                                                                                
    {"You have become corrupted during the Great Flood",                           RARE,                10,   END_GROUP},                                                                                                        
    
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
	{"Your father was an incubus. ",	                                           LIKELY,              15,   0},
	{"Your mother was Glaryssa, the Succubus Queen. ",	                           LIKELY,              65,   0},
    {"Your mother was Lilith, the first Woman. ",	                               LIKELY,              80,   0},
	{"You are created from raw Chaos itself. ",                                    LIKELY,              90,   0},
	{"You are a descendant of a Devil Prince. ",                                   LIKELY,              70,   END_GROUP},
        
    {"Your mother was Lilith, the first Woman. ",	                               LIKELY,              80,   BG_LILI},
    {"Your grandmother was Lilith, the first Woman. ",	                           UNLIKELY,            65,   END_GROUP},    
    
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
	{"and a firy red skin.",                                                       LIKELY,              0,    0},
	{"and an emerald green skin.",                                                 LIKELY,              0,    0},
	{"and a frosty blue skin.",                                                    LIKELY,              0,    END_GROUP},    
    
    { NULL,                                                                        0,                   0,    END_GROUP}, /* Ye famous null record */
};

/*
* Current stats
*/
static s16b stat_use[6];

/*
* Autoroll limit
*/
static s16b stat_limit[6];

/*
* Autoroll matches
*/
static s32b stat_match[6];

/*
* Autoroll round
*/
static s32b auto_round;

/*
* Last round
*/
static s32b last_round;

/*
* Name segments for random player names
*/

/* Dwarves */
static char *dwarf_syllable1[] =
{
	"B", "D", "F", "G", "Gl", "H", "K", "L", "M", "N", "R", "S", "T", "Th", "V",
};

static char *dwarf_syllable2[] =
{
	"a", "e", "i", "o", "oi", "u",
};

static char *dwarf_syllable3[] =
{
	"bur", "fur", "gan", "gnus", "gnar", "li", "lin", "lir", "mli", "nar", "nus", "rin", "ran", "sin", "sil", "sur",
};

/* Elves */
static char *elf_syllable1[] =
{
	"Al", "An", "Bal", "Bel", "Cal", "Cel", "El", "Elr", "Elv", "Eow", "Ear", "F", "Fal", "Fel", "Fin", "G", "Gal", "Gel", "Gl", "Is", "Lan", "Leg", "Lom", "N", "Nal", "Nel",  "S", "Sal", "Sel", "T", "Tal", "Tel", "Thr", "Tin",
};

static char *elf_syllable2[] =
{
	"a", "adrie", "ara", "e", "ebri", "ele", "ere", "i", "io", "ithra", "ilma", "il-Ga", "ili", "o", "orfi", "u", "y",
};

static char *elf_syllable3[] =
{
	"l", "las", "lad", "ldor", "ldur", "linde", "lith", "mir", "n", "nd", "ndel", "ndil", "ndir", "nduil", "ng", "mbor", "r", "rith", "ril", "riand", "rion", "s", "thien", "viel", "wen", "wyn",
};

/* Gnomes */
static char *gnome_syllable1[] =
{
	"Aar", "An", "Ar", "As", "C", "H", "Han", "Har", "Hel", "Iir", "J", "Jan", "Jar", "K", "L", "M", "Mar", "N", "Nik", "Os", "Ol", "P", "R", "S", "Sam", "San", "T", "Ter", "Tom", "Ul", "V", "W", "Y",
};

static char *gnome_syllable2[] =
{
	"a", "aa",  "ai", "e", "ei", "i", "o", "uo", "u", "uu",
};

static char *gnome_syllable3[] =
{
	"ron", "re", "la", "ki", "kseli", "ksi", "ku", "ja", "ta", "na", "namari", "neli", "nika", "nikki", "nu", "nukka", "ka", "ko", "li", "kki", "rik", "po", "to", "pekka", "rjaana", "rjatta", "rjukka", "la", "lla", "lli", "mo", "nni",
};

/* Hobbits are no more, one day maybe this colelction of syllables will be usefull again */
/*static char *hobbit_syllable1[] =
{
	"B", "Ber", "Br", "D", "Der", "Dr", "F", "Fr", "G", "H", "L", "Ler", "M", "Mer", "N", "P", "Pr", "Per", "R", "S", "T", "W",
};

static char *hobbit_syllable2[] =
{
	"a", "e", "i", "ia", "o", "oi", "u",
};

static char *hobbit_syllable3[] =
{
	"bo", "ck", "decan", "degar", "do", "doc", "go", "grin", "lba", "lbo", "lda", "ldo", "lla", "ll", "lo", "m", "mwise", "nac", "noc", "nwise", "p", "ppin", "pper", "tho", "to",
};*/

/* Human */
static char *human_syllable1[] =
{
	"Ab", "Ac", "Ad", "Af", "Agr", "Ast", "As", "Al", "Adw", "Adr", "Ar", "B", "Br", "C", "Cr", "Ch", "Cad", "D", "Dr", "Dw", "Ed", "Eth", "Et", "Er", "El", "Eow", "F", "Fr", "G", "Gr", "Gw", "Gal", "Gl", "H", "Ha", "Ib", "Jer", "K", "Ka", "Ked", "L", "Loth", "Lar", "Leg", "M", "Mir", "N", "Nyd", "Ol", "Oc", "On", "P", "Pr", "R", "Rh", "S", "Sev", "T", "Tr", "Th", "V", "Y", "Z", "W", "Wic",
};

static char *human_syllable2[] =
{
	"a", "ae", "au", "ao", "are", "ale", "ali", "ay", "ardo", "e", "ei", "ea", "eri", "era", "ela", "eli", "enda", "erra", "i", "ia", "ie", "ire", "ira", "ila", "ili", "ira", "igo", "o", "oa", "oi", "oe", "ore", "u", "y",
};

static char *human_syllable3[] =
{
	"a", "and", "b", "bwyn", "baen", "bard", "c", "ctred", "cred", "ch", "can", "d", "dan", "don", "der", "dric", "dfrid", "dus", "f", "g", "gord", "gan", "l", "li", "lgrin", "lin", "lith", "lath", "loth", "ld", "ldric", "ldan", "m", "mas", "mos", "mar", "mond", "n", "nydd", "nidd", "nnon", "nwan", "nyth", "nad", "nn", "nnor", "nd", "p", "r", "ron", "rd", "s", "sh", "seth", "sean", "t", "th", "tha", "tlan", "trem", "tram", "v", "vudd", "w", "wan", "win", "wyn", "wyr", "wyr", "wyth",
};

/* Orc */
static char *orc_syllable1[] =
{
	"B", "Er", "G", "Gr", "H", "P", "Pr", "R", "V", "Vr", "T", "Tr", "M", "Dr",
};

static char *orc_syllable2[] =
{
	"a", "i", "o", "oo", "u", "ui",
};

static char *orc_syllable3[] =
{
	"dash", "dish", "dush", "gar", "gor", "gdush", "lo", "gdish", "k", "lg", "nak", "rag", "rbag", "rg", "rk", "ng", "nk", "rt", "ol", "urk", "shnak", "mog", "mak", "rak",
};

static char *angel_syllable1[] =
{
	"Sa","A","U","Mi","Ra","Ana","Pa","Lu","She","Ga","Da","O","Pe","Lau",
};

static char *angel_syllable2[] =
{
	"br","m","l","z","zr","mm","mr","r","ral","ch","zaz","tr","n","lar",
};

static char *angel_syllable3[] =
{
	"iel","ial","ael","ubim","aphon","iel","ael",
};

static char *illithid_syllable1[] =
{
	"Cth","Az","Fth","Ts","Xo","Q'N","R'L","Ghata","L","Zz","Fl","Cl","S","Y",
};

static char *illithid_syllable2[] =
{
	"nar","loi","ul","lu","noth","thon","ath","'N","rhy","oth","aza","agn","oa","og",
};

static char *illithid_syllable3[] =
{
	"l","a","u","oa","oggua","oth","ath","aggua","lu","lo","loth","lotha","agn","axl",
};

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
	 "",
	 "","","","","","","","","",""},
	/*Gentleman*/
	{"You have been accepted quite soon in the League because",
	 "of your potential. You have never considered that being",
	 "male has made your progress in the League much easier. ",
	 "","","","","","","","","","",""},	
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
	 "                                                       ",
	 "                                                       ",
	 "                                                       ",
	 "                                                       ",
	 "","","","","",""},
	/*Faerie*/
	{"These little creatures are almost lost to the world,   ",
	 "but some have adapted to the ways of the humans. Mostly",
	 "found on the British Islands, some of them support the ",
	 "activities of the League. Compared to humans, faeries  ",
	 "are more dextrous, intelligent, charming, stealthier   ",
	 "and superior in magic. They are much weaker though.    ",
	 "Their magic keeps them from falling into traps.        ",
	 "                                                       ",
	 "","","","","",""},	
	/*Spawn*/
	{"Creatures born in the pits of Hell, they all fight for ",
	 "their spot. Some win, some loose and some get thrown in",
	 "to the world of Man. The League being a source of much ",
	 "power and knowledge it attracts the occasional outcast ",
	 "Spawn. The league employs some of them, using a magical",
	 "bond that lasts a hundred years. Spawns are stronger,  ",
	 "faster, tougher and more intelligent. They are heinous ",
	 "though, providing little charisma.",
	 "","","","","",""},	
	/*Elder*/
	{"Little is known about the Elder, even the Elder have   ",
	 "forgotten where they come from, what their purpose is. ",
	 "It is a generally accepted idea that the Elder existed ",
	 "when the Earth was created, and they will be there when",
	 "the Earth will be undone. The Elder employ Guardians, a",
	 "subspecies of the Elder born to protect them. Elder are",
	 "charismatic, intelligent and superior in magic.        ",
	 "                                                       ",
	 "","","","","",""},		
	
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
	"Gipsy",				/*1*/
	"Nordic",				/*2*/
	"Atlantian",			/*3*/
	"Dwarf descendant",		/*4*/
	"Elf descendant",		/*5*/
	"Ogre descendant",		/*6*/
	"Troll descendant",		/*7*/
	"Giant descendant",		/*8*/
	"Titan descendant",		/*9*/	
	"Nephilim",				/*10*/
	"Afflicted",			/*11*/
	"Seelie Fae",			/*12*/
	"Gnome",				/*13*/
	"Leprechaun",			/*14*/
	"Kobold",				/*15*/	
	"Devilspawn",			/*16*/
	"Imp",					/*17*/
	"Succubus",				/*18*/
	"Lili",					/*19*/	
	"Elder",				/*20*/
	"Elder Guardian",		/*21*/
	"Horror",				/*22*/
};

static cptr subraces_descriptions[COUNT_SUBRACES][COUNT_LINES] =
{
	/*0123456789012345678901234567890123456789012345678912345*/	
	/*Florentian*/
	{   "Florentians are citizens are Italians from the city of ",
		"Florence. They are your basic human, with maybe a bit  ",
		"more interest in Inferno than the average person given ",
		"that Dante was a Florentian as well. Being an Italian  ",
		"they get discount in Italian shops.                    ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Hit Dice : 10                    Experience Factor: 100"},
	/*Gipsy*/
	{   "Gipsies are not very well liked, even though they are  ",
		"great entertainers and sport some of the most beautiful",
		"women. Gipsies are charismatic, have a knack for being ",
		"stealthy and gain the Second Sight when they become    ",
		"more experienced. They are slightly better in magic.   ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Dex:+1 Cha:+1                                          ",
		"Hit Dice : 10                    Experience Factor: 110"},
	/*Nordic*/
	{   "Nordics are hardy men from the North. They are still   ",
		"very much in touch with Nature and its' spirits. This  ",
		"makes them slightly better at magical abilities.       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Int:+1 Wis:+1                                          ",
		"Hit Dice 10                      Experience Factor: 120"},
	/*Atlantian*/
	{   "Living in a dome on the bottom of the ocean, they have ",
		"no natural enemies and grown weak. They do however have",
		"a knack for magic and are resistant to darkness. Their ",
		"innate magical abilities allow to fire magical missiles",
		"at will.                                               ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -1, Int: +3, Wis: +2, Dex: +2, Con: -1, Cha: +1   ",
		"Hit dice: 9                      Experience Factor: 150"},
	/*Dwarf*/
	{   "True dwarfs have not dwelled on the planet surface     ",
		"since ages, but they had mingled with humans and some  ",
		"of their descendants are almost as stocky, loudmouthed ",
		"and foul-tempered as they once were. They are hard to  ",
		"be blinded and find their ways easily under the ground.",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +1, Int: +1, Wis: -1, Dex: -1, Con +1, Cha -1     ",
		"Hit dice: 11                     Experience Factor: 125"},
	/*Elf*/
	{   "True elfs have not dwelled in the Scandinavian lands   ",
		"since ages, but they had mingled with humans and some  ",
		"of their descendants show a startling gracefulness.    ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -1, Int: +1, Wis: +1, Dex: +1, Con: -1, Cha: +1   ",
		"Hit dice: 8                      Experience Factor: 120"},
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
		"                                                       ",
		"                                                       ",
		"Str: +3, Int: -1, Wis: -1, Dex: -1, Con: +3, Cha: -1   ",
		"Hit dice: 12                     Experience Factor: 130"},
	/*Troll*/
	{   "Trolls are the Scandinavian version of the German and  ",
		"French ogres. Their descendants even though civilized  ",
		"are uglier, stronger, stupider and regenerate faster. ",
		"Experienced troll can enter into a berserker fury.     ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +4, Int: -4, Wis: -2, Dex: -4, Con: +3, Cha: -2   ",
		"Hit dice: 12                     Experience Factor: 137"},
	/*Giant*/
	{   "Like the descendants of the Titans they have concealed ",
		"themselves on an island in the Mediterranean sea. The  ",
		"League has found out about their existance and requires",
		"their assistance every now and then. Even though not   ",
		"very smart they make great adventurers with their solid",
		"toughness and strength. They resist strength draining  ",
		"attacks and shards. Experienced, they can smash stone  ",
		"into dust.                                             ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +4, Int: -2, Wis: -2, Dex: -2, Con: +3, Cha: -3   ",
		"Hit dice: 13                     Experience Factor: 150"},
	/*Titan*/
	{   "The largest of all, and superior in almost every aspect",
		"these descedants have been found on a remote island in ",
		"the Mediterranean sea, protected by ancient sorceries. ",
		"The League has managed to penetrate these sorceries and",
		"some of the inhabitants have decided to join them. They",
		"resist chaos, resistance and can spot the weaknesses of",
		"others.                                                ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str:  +5, Int: +1, Wis: +1, Dex: -2, Con: +3, Cha: +1  ",
		"Hit dice: 14                     Experience Factor: 255"},
	/*Nephilim*/
	{   "Children of men and angels, they usually become giant  ",
		"man-eating creatures. It seems that at some point in   ",
		"their life Nephilim must give up their Angelic or their",
		"human heritage. Nephilim starting this adventure have  ",
		"not yet made this choice, allowing them to go either   ",
		"way.                                                   ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +1, Int: +2, Wis: +2, Dex: +2, Con: +3, Cha: +2   ",
		"Hit dice: 10                     Experience Factor: 225"},
	/*Afflicted*/
	{   "Either bitten by vampire or werewolf or undead, the    ",
		"afflicted have lost their humanity. This usually means ",
		"also a higher resistance than usual to nether, cold and",
		"darkness. It also means that they have lost the effects",
		"of any constellation they were born under.             ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       "},	
	/*Seelie Fae*/
	{   "Seelie Fae, or properly called Seelie Court, are good  ",
		"faeries of the British Isles. They are a beautifull to ",
		"behold, but frail and not very strong. They are very   ",
		"dextrous and have superior magic skills. Their magic   ",
		"prevents them from falling intro traps, from light and ",
		"it allows them to toss around magical sleeping dust.   ",
		"As they get more experienced, they become faster.      ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -4, Int: +3, Wis: +3, Dex: +3, Con: -2, Cha: +2   ",
		"Hit dice: 7                      Experience Factor: 175"},
	/*Gnome*/
	{   "Gnomes are a small, playful folk. Whilst being very    ",
		"intelligent, they suffer from an almost chronic failure",
		" to take anything seriously. Gnomes are constantly on  ",
		"the move, and are impossible to paralyse or slow. In   ",
		"fact, they can even teleport themself at higher levels.",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -1, Int: +2, Wis: +0, Dex: +2, Con: +1, Cha: -2   ",
		"Hit dice: 8                      Experience Factor: 135"},
	/*Leprechaun*/
	{   "Leprechauns are male faeries inhabiting Ireland.  They ", 
		"are into shoemaking, mischief and gold collections.    ",
		"There are no famous leprechauns yet, even though they  ",
		"are superior in magic, dexterity, charm and speed.     ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -4, Int: +3, Wis: +3, Dex: +4, Con: -4, Cha: +7   ",
		"Hit dice: 7                      Experience Factor: 100"},
	/*Kobold*/
	{   "Kobolds are malicious faeries inhabiting the Black     ", 
		"Forest. Some of their talents are very useful and for  ",
		"the right price they sometimes work with the League.   ",
		"They are masters in stealth and poison, an experienced ",
		"kobold even grows glands that allow it to spit poison  ",
		"darts. They are not an intelligent type of faerie, and ",
		"arent great lookers either.                            ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +1, Int: -1, Wis: +0, Dex: +1, Con: +0, Cha: -4   ",
		"Hit dice: 9                      Experience Factor: 125"},	
	/*Devilspawn*/
	{   "Devilspawn are the progeny of mortals and demons. As   ",
		"such, they inherit some of the raw strength of their   ",
		"demonic parentage, but their mixed race tends to leave ",
		"their thoughts confused and their forms misshapen.     ",
		"Devilspawn are remembered by their demonic anscestors, ",
		"and as such they always get a demonic patron. Their    ",
		"association with the pandemonium of hell allows them to",
		"resist both confusion and sound attacks.               ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +2, Int: -1, Wis: -1, Dex, -1, Con: +2, Cha: -4	",
		"Hit dice: 11                     Experience Factor: 140"},
	/*Imp*/
	{   "Imps are small red-skinned fire demons. Although not   ",
		"terribly strong or smart, they are tough and fast. As  ",
		"they are beings of fire, they have innate resistance to",
		"it, growing into immunity as they toughen up. They can ",
		"learn how to toss flame bolts, fireballs and can even  ",
		"gain Second Sight .                                    ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -1, Int: -1, Wis: -1, Dex: +1, Con: +2, Cha: -3   ",
		"Hit dice: 10                     Experience Factor: 110"},
	/*Succubus*/
	{   "Born in the pits of Hell, they have been selected as   ",
		"much for their beauty as their visciousness. They are  ",
		"demons that can take the form of a beautiful woman and ",
		"have a special draining attacks against men. They are  ",
		"intelligent, dextrous, fast and stealthy with a knack  ",
		"for magic. They resists chaos and confusion naturally. ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Int: +2, Dex, +2, Cha: +4                              ",
		"Hit dice: 11                     Experience Factor: 160"},
	/*Lili*/
	{   "Born from Lilith and Asmodeus they know that Lillith   ",
		"will come one day after them. They join the League for ",
		"power, power they will use when the Day comes. Lili are",
		"beautiful and rebellious like their mother and sensual ",
		"like their father. Lilim resist chaos and confusion    ",
		"and are very tough.                                    ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Con:4, Cha: +4                                         ",
		"Hit dice: 14                     Experience Factor: 160"},	
	/*Elder*/     
	{	"The true Elder is a very tough creature, regenerating  ",
		"wounds even when almost completely destroyed. They are ",
		"a beautiful sight to behold and radiate light in the   ",
		"dark. Their senses are magically attuned and they have ",
		"the second sight. They are protected from light-based  ",
		"attacks.                                               ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +1, Int: +3, Wis: +2, Dex: +3, Con: +1, Cha: +5   ",
		"Hit dice: 10                     Experience Factor: 200"},
	/*Elder Guardian*/
	{   "Elder Guardians have been completely designed to defend",
		"their assigned Elder. A few Elder Guardians have lost  ",
		"the Elder they should guard and have joined the League,",
		"as a means to find back their protegee. They are slow, ",
		"not very bright but incredibly tough. They cannot use  ",
		"mortal food, only Ambrosia or magical means can sustain",
		"them. They have awesome defences, they cannot be bled  ",
		"or stunned. They are naturally resistant to poison and ",
		"have Second Sight.                                     ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +4, Int: -5, Wis: -5, Dex: +0, Con: +4, Cha: -4   ",
		"Hit dice: 12                     Experience Factor: 200",},
	/*Horror*/     
	{	"Some of the Elder have become Horrors, after recovering",
		"from grievous wounds their body has changed into a     ",
		"nightmarish creature. Slimy, their faces covered with  ",
		"tentacles they have gained even more mental powers at  ",
		"the cost of frailty. They can gain the Second Sight,   ",
		"sense minds from a distance and project mental energies",
		"in a direct attack.                                    ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -3, Int: +4, Wis: +4, Dex: +0, Con: -2, Cha: -5   ",
		"Hit dice: 9                      Experience Factor: 140"},	
			
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
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"Str: +3, Int: +3, Wis: -1, Dex: -1, Con: +1, Cha: +2   ",
	"Hit dice: 11                     Experience Factor: 200"},
/*Werewolf*/
{   "Werewolves are mostly originating from the Black Forest",
	"in Germany. They tend to kill their beloved ones under ",
	"the full moon, so a lot of them leave their homes and  ",
	"wander. Experienced werewolves can trigger the change  ",
	"to Wolf at will. Canines are neutral to Werewolves and ",
	"will not attack them.                                  ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"Str: +3, Int: +3, Wis: -1, Dex: -1, Con: +1, Cha: +2   ",
	"Hit dice: 11                     Experience Factor: 200"},
/*Skeleton*/
{   "Skeletons in Hellband are the remains of the poor souls",
	"that annoyed an African medicin man. They have can bind",
	"the soul to the bones of a person, rotting away all the",
	"skin in the progress. Not even ambrosia can feed them, ",
	"they rely soulely on magical means to feed themselves. ",
	"They are protected from bleeding, shards, poison, cold ",
	"and life draining. They all have the Second Sigh.      ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"Str: +0, Int: -2, Wis: -2, Dex: +0, Con: +1, Cha: -4   ",
	"Hit dice: 10                     Experience Factor: 145"},
/*Mummy*/
{   "Mummies are the new zombies, and I am all out of witty ",
	"description. So here's the deal. If you play a Mummy,  ",
	"you really should take the effort to write me a nice   ",
	"overview and I'll include it in Hellband.              ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"Str: +2, Int: -6, Wis: -6, Dex: +1, Con: +4, Cha: -5   ",
	"Hit dice: 13                     Experience Factor: 135"},
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
	"                                                       ",
	"                                                       ",
	"Str: -5, Int: +4, Wis: +4, Dex: +2, Con: -3, Cha: -6   ",
	"Hit dice: 7                      Experience Factor: 180"},
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
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"Hit dice: +0                     Experience Factor: 0  "},
/*Draco*/
{   "The constellation Draco or 'Dragon' confers under rare ",
	"circumstances dragon powers to newborn children. Later ",
	"in their life they will discover resistance to many    ",
	"elements, they will also find that they can shapeshift ",
	"into a Dragonling; scaled, winged and capable to breath",
	"fire and other elements.                               ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"Str: +2, Int: +1, Wis: +1, Dex: +1, Con: +2, Cha: -3   ",
	"Hit dice: +1                     Experience Factor: 250"},
/*Serpens*/
{   "The constellation Serpens or 'Serpent' confers under   ",
	"rare conditions powers of and over snakes. People born ",
	"under this constellation can resist poison and will not",
	"be attacked by snakes and serpents. They also can be   ",
	"very stealthy.                                         ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"Hit dice: +0                     Experience Factor:  30"},
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
	"                                                       ",
	"                                                       ",
	"                                                       ",
	"Str: +1, Int: -1, Wis: +2, Dex: +0, Con: +2, Cha: +4   ",
	"Hit dice: +1                     Experience Factor: 135"},
/*Morui*/
{   "Stories are told of the people from the star Morui, now",
	"more commonly called Orion. It is said that they have  ",
	"mingled with humans and that their genes are stronger  ",
	"with children born under Orion. People born under Morui",
	"are better in every way save for an odd mind. They grow ",  
	"a tough subdermal chitin that resists acid and their   ",
	"thoughts are impossible to confuse. They can grow wings",
	"that help avoid pits and falls. As they get more       ",
	"experienced, they also get faster and gain the ability ",
	"to spit acid.                                          ",
	"                                                       ",
	"                                                       ",
	"Str: +2, Int: -1, Wis: -1, Dex: +1, Con: +2, Cha: -2   ",
	"Hit dice: +2                     Experience Factor: 135"},
};



static cptr classes_descriptions[MAX_CLASS][COUNT_LINES] =
{
	/*Warrior*/
	{   "Warriors are the simplest class. They get no magic or  ",
		"special abilities, other than learning to resist fear  ",
		"(min lev = 30). They simply fight. However, they are   ",
		"are tougher and better at fighting than any other.     ",
		"                                                       ",  
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +5, Int: -2, Wis: -2, Dex: +2, Con: +2, Cha: -1   ",
		"Hit dice: 9                      Experience Factor:   0"},
	/*Mage*/
	{   "Mages study arcane magic, but do not specialize as     ",
		"strongly as high-mages do. Mages receive two realms of ",
		"magic of their choice. Mages struggle with combat when ",
		"not using spells.                                      ",
		"                                                       ",  
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -5, Int: +3, Wis: +0, Dex: +1, Con: -2, Cha: +1   ",
		"Hit dice: 0                      Experience Factor:  30"},
	/*Priest*/
	{   "Priests are divine magic specialists. Whilst not as    ",
		"good at combat as paladins, they are better at magic. ",
		"Priests get divine magic from either miracles or the   ",
		"death realm plus one other realm (although they can't  ",
		"take miracles and death magic. Priests who learn death ",
		"magic are called Cultists. Priests take religious vows ",
		"which prevent them from using edged weapons unless     ",
		"those weapons are blessed.",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -1, Int: -3, Wis: +3, Dex: -1, Con: +0, Cha: +2   ",
		"Hit dice: 2                      Experience Factor:  20"},	
	/*Rogue*/
	{   "Rogues are masters of stealth. Although they are not as",
		"good as warriors in a straight fight, they can backstab",
		"sleeping or fleeing opponents doing large amounts of   ",
		"damage. Rogues also learn a very small amount of arcane",
		"magic from either the death, tarot or folk realm.     ",
		"Rogues who learn death magic are called Assassins.     ",
		"Rogues who learn tarot magic are called Card Sharps. ",
		"Rogues who learn folk magic are called Thieves.        ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +2, Int: +1, Wis: -2, Dex: +3, Con: +1, Cha: -1   ",
		"Hit dice: 6                      Experience Factor:  25"},	
	/*Ranger*/
	{   "Rangers are decent fighters, although they specialize  ",
		"in missile weapons. Like druids, they use divine magic ",
		"from the Nature realm. They are not as good as druids  ",
		"at nature magic, but make up for it by also learning a ",
		"second realm.                                          ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +2, Int: +2, Wis: +0, Dex: +1, Con: +1, Cha: +1   ",
		"Hit dice: 4                      Experience Factor:  30"},	
	/*Paladin*/
	{   "Paladins are holy warriors. There are two types - true ",
		"Paladins and Death Knights. True paladins get divine   ",
		"magic from the Miracles realm, whereas death knights   ",
		"get divine magic from the Death realm. In either case, ",
		"their magic is not as strong as that of a priest, but  ",
		"they make up for this by fighting almost as well as a  ",
		"warrior does. Paladins can learn to resist the effects ",
		"of fear at a higher level.                             ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +3, Int: -3, Wis: +1, Dex: +0, Con: +2, Cha: +2   ",
		"Hit dice: 6                      Experience Factor:  35"},	
	/*Warrior-Mage*/
	{   "Warrior mages combine reasonable combat skills with two",
		"realms of arcane magic. One of their realms of magic   ",
		"must be Charms, but the other can be any. They are not ",
		"quite as good at fighting as warriors, and not quite as",
		"good at magic as true mages, but they make up for their",
		"lack of strength by combining it with a lack of        ",
		"weakness.                                              ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +2, Int: +2, Wis: +0, Dex: +1, Con: +0, Cha: +1   ",
		"Hit dice: 4                      Experience Factor:  50"},	
	/*Hell Knight*/
	{   "Hell Knights have made pacts with an infernal patron in",
		"exchange for physical prowess. As such, they are good  ",
		"warriors. Their patrons give them a small amount of    ",
		"divine magic from the chaos realm, and occasionally    ",
		"give them other rewards too. Hell Knights can learn to ",
		"resist the effects of chaos and fear.                  ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +2, Int: +1, Wis: +0, Dex: +1, Con: +2, Cha: -2   ",
		"Hit dice: 6                      Experience Factor:  35"},	
	/*Mystic*/
	{   "Mystics are martial artists. As such they are masters  ",
		"of unarmed combat and increase their speed as they gain",
		"experience. However, they are severely hampered by     ",
		"wearing heavy armour. With experience, they can shrug  ",
		"off slowing and paralyzing attacks. As part of their   ",
		"meditations, mystics learn divine magic from the       ",
		"Somatic realm.                                         ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: +2, Int: -1, Wis: +1, Dex: +3, Con: +2, Cha: +1   ",
		"Hit dice: 6                      Experience Factor:  40"},	
	/*Mindcrafter*/
	{   "Mindcrafters rely on the supernatural powers that their",
		"mind is capable of producing. Many of their powers are ",
		"similar to spells and are used in the same way. Some   ",
		"powers, however, are simply passive, not requiring     ",
		"active use. Mindcrafters can resists fear and confusion",
		". They can sustain their wisdom, and even sense other  ",
		"minds once they are very experienced. They can handle  ",
		"themselves in combat.                                  ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -1, Int: +0, Wis: +3, Dex: -1, Con: -1, Cha: +2   ",
		"Hit dice: 2                      Experience Factor:  25"},		
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
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -5, Int: +4, Wis: +0, Dex: +0, Con: -2, Cha: +1   ",
		"Hit dice: 0                      Experience Factor:  30"},			
	/*Druid**/
	{   "Druids are nature worshippers. As such, they use divine",
		"magic from the realm of Nature. They are better at     ",
		"nature magic than any other class. Like priests, druids",
		"are not allowed to use edged weapons unless those      ",
		"weapons are blessed.                                   ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"                                                       ",
		"Str: -1, Int: -3, Wis: +4, Dex: -2, Con: +0, Cha: +3   ",
		"Hit dice: 2                      Experience Factor:  20"},		
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
		"                                                       ",
		"                                                       ",
		"Str: -5, Int: +4, Wis: +0, Dex: +0, Con: -2, Cha: +1   ",
		"Hit dice: 0                      Experience Factor:  30"},		
	
};


/* Allow player to modify the character by spending points */
static bool point_mod_player(void)
{
	char b1 = '[';
	char b2 = ']';
	char stat;
	char modpts[4] = "none";
	/*char *buf = "zero";*/
	/*int tmp = 0;*/
	/*int hp = 0;*/
	/*int addhp = 0; */
	int x = 0;
	int i, points;


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
			if(p_ptr->stat_max[i] > 3) points--;
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
			if(p_ptr->stat_max[i] > 2) points++;
			continue;
		}
	}
	return TRUE;
}

/*
* Random Name Generator
* based on a Javascript by Michael Hensley
* "http://geocities.com/timessquare/castle/6274/"
*/
void create_random_name(int race, char *name)
{
	/* Paranoia */
	if (!name) return;

	/* Select the monster type */
	switch (race)
	{
		/* Create the monster name */
	case DWARF:
	case GIANT:
	case GUARDIAN:
		strcpy(name, dwarf_syllable1[rand_int(sizeof(dwarf_syllable1) / sizeof(char*))]);
		strcat(name, dwarf_syllable2[rand_int(sizeof(dwarf_syllable2) / sizeof(char*))]);
		strcat(name, dwarf_syllable3[rand_int(sizeof(dwarf_syllable3) / sizeof(char*))]);
		break;
	case ELF:
	case FAE:
	case LEPRECHAUN:
	case ATLANTIAN:	
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
	case TITAN:
	case SKELETON:
	case SPECTRE:
	case VAMPIRE:
	case MUMMY:
	case GIPSY:
	case NORDIC:	
		strcpy(name, human_syllable1[rand_int(sizeof(human_syllable1) / sizeof(char*))]);
		strcat(name, human_syllable2[rand_int(sizeof(human_syllable2) / sizeof(char*))]);
		strcat(name, human_syllable3[rand_int(sizeof(human_syllable3) / sizeof(char*))]);
		break;
	case OGRE:
	case TROLL:
	case KOBOLD:
		strcpy(name, orc_syllable1[rand_int(sizeof(orc_syllable1) / sizeof(char*))]);
		strcat(name, orc_syllable2[rand_int(sizeof(orc_syllable2) / sizeof(char*))]);
		strcat(name, orc_syllable3[rand_int(sizeof(orc_syllable3) / sizeof(char*))]);
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

u16b choose_realm(byte choices)
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
	/*Note that n contains how many realms can be chose*/
	n = 0;
	for( i = 1 ; i < MAX_REALM ; i ++ )
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
		c_put_str(TERM_YELLOW, buf, 5, ((screen_width-strlen(buf))>>1) );
		
		for(i=0;i<n;i++)
			c_put_str(TERM_L_BLUE, i==choice?">":" " , 8+i , 2 );			
				
		for(i=0;i<n;i++)
			c_put_str(i==choice?TERM_L_BLUE:TERM_L_WHITE, realm_names[picks[i]], 8+i, 3);
		
/*		for(i=0;i<COUNT_LINES;i++)
			put_str( races_descriptions[choice][i] , 8+i , 23 );*/
		
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S') return (0);		
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

u16b choose_realm_randomly(byte choices)
{
	int picks[MAX_REALM] = {0};
	int k, n;
	n = 0;
	/* Hack: Allow priests to specialize in Miracles or Death magic */
	if ((choices & CH_CHAOS) && p_ptr->realm1 != REALM_CHAOS)
	{
		picks[n]=REALM_CHAOS;
		n++;
	}
	if ((choices & CH_DEMONIC) && p_ptr->realm1 != REALM_DEMONIC)
	{
		picks[n]=REALM_DEMONIC;
		n++;
	}    
	if ((choices & CH_SOMATIC) && p_ptr->realm1 != REALM_SOMATIC)
	{
		picks[n]=REALM_SOMATIC;
		n++;
	}
	if ((choices & CH_DEATH) && p_ptr->realm1 != REALM_DEATH)
	{
		picks[n]=REALM_DEATH;
		n++;
	}
	if ((choices & CH_CHARMS) && p_ptr->realm1 != REALM_CHARMS)
	{
		picks[n]=REALM_CHARMS;
		n++;
	}
	if ((choices & CH_MIRACLES) && p_ptr->realm1 != REALM_MIRACLES)
	{
		picks[n]=REALM_MIRACLES;
		n++;
	}
	if ((choices & CH_NATURE) && p_ptr->realm1 != REALM_NATURE)
	{
		picks[n]=REALM_NATURE;
		n++;
	}
	if ((choices & CH_TAROT) && p_ptr->realm1 != REALM_TAROT)
	{
		picks[n]=REALM_TAROT;
		n++;
	}
	if ((choices & CH_SORCERY) && p_ptr->realm1 != REALM_SORCERY)
	{
		picks[n]=REALM_SORCERY;
		n++;
	}
	/* Get a realm */
	k=rand_range(0,n-1);
	return (picks[k]);
}

int get_realms()
{

	int pclas=p_ptr->pclass;

	/* First we have null realms */
	p_ptr->realm1=p_ptr->realm2=0;

	/* Warriors and certain others get no realms */

	if (realm_choices[pclas] == (CH_NONE)) return TRUE;

	/* Other characters get at least one realm */

	switch (pclas)
	{
	case CLASS_WARRIOR_MAGE:
		p_ptr->realm1 = REALM_CHARMS;
		break;
	case CLASS_HELL_KNIGHT:
		p_ptr->realm1 = REALM_DEMONIC;
		break;
	case CLASS_PRIEST:
		/* Hack... priests can be 'dark' priests and choose death instead of Miracles, but not both */		
		p_ptr->realm1 = choose_realm( CH_MIRACLES | CH_DEATH);
		if(!p_ptr->realm1)return FALSE;
		break;
	case CLASS_RANGER:
		p_ptr->realm1 = REALM_NATURE;
		break;
	case CLASS_MYSTIC:
		p_ptr->realm1 = REALM_SOMATIC;
		break;
	case CLASS_DRUID:
		p_ptr->realm1 = REALM_NATURE;
		break;
	case CLASS_WARLOCK:
		p_ptr->realm1 = REALM_DEMONIC;
		break;
	default:
		p_ptr->realm1 = choose_realm(realm_choices[pclas]);
		if(!p_ptr->realm1)return FALSE;
	}

	/* Paladins, Oathbreakers and Rogues get no second realm */
	if (pclas == CLASS_PALADIN || pclas == CLASS_ROGUE || pclas == CLASS_HELL_KNIGHT
		|| pclas == CLASS_MYSTIC || pclas == CLASS_HIGH_MAGE || pclas == CLASS_DRUID) return TRUE;
	else
		p_ptr->realm2 = choose_realm(realm_choices[pclas]);
		if(!p_ptr->realm2)return FALSE;
		return TRUE;
}

/* Get realms randomly without asking player */
void get_realms_randomly()
{
	int pclas=p_ptr->pclass;
	/* First we have null realms */
	p_ptr->realm1=p_ptr->realm2=0;
	/* Warriors and certain others get no realms */
	if (realm_choices[pclas] == (CH_NONE)) return;
	/* Other characters get at least one realm */
	switch (pclas)
	{
	case CLASS_WARRIOR_MAGE:
		p_ptr->realm1 = 7;
		break;
	case CLASS_HELL_KNIGHT:
		p_ptr->realm1 = 4;
		break;
	case CLASS_PRIEST:
		p_ptr->realm1 = choose_realm_randomly( CH_MIRACLES | CH_DEATH);
		/* Hack... priests can be 'dark' priests and choose death instead
		of life, but not both */
		break;
	case CLASS_RANGER:
		p_ptr->realm1 = 3;
		break;
	case CLASS_DRUID:
		p_ptr->realm1 = 3;
		break;
	case CLASS_WARLOCK:
		p_ptr->realm1 = 4;
		break;
	default:
		p_ptr->realm1 = choose_realm_randomly(realm_choices[pclas]);
	}
	/* Paladins, Hell Knights and rogues get no second realm */
	if (pclas == CLASS_PALADIN || pclas == CLASS_ROGUE || pclas == CLASS_HELL_KNIGHT
		|| pclas == CLASS_MYSTIC || pclas == CLASS_HIGH_MAGE || pclas == CLASS_DRUID) return;
	else
		p_ptr->realm2 = choose_realm_randomly(realm_choices[pclas]);
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
	for (i = 0; i < 6; i++)
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
	for (i = 0; i < 6; i++)
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
	for (i = 0; i < 6; i++)
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
	for (i = 0; i < 6; i++)
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
	for (i = 0; i < 6; i++)
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
        if( background[bg_index].metadata & race_bitflag ){
            odds_index = 0;
            odds_total = 0;
            odd_current = 0;
            odd_found = FALSE;
            while( background[ bg_index ].metadata != END_GROUP ){
                odds_total += background[ bg_index ].frequency;
                odds[odds_index++] = background[ bg_index ].frequency;
                bg_index++;
            }
            /*Lame!! Blame brain rot.*/
            odds_total += background[ bg_index ].frequency;
            odds[odds_index++] = background[ bg_index ].frequency;            
            /*So which of the entries do we take*/
            odds_total = randint(odds_total);
            for( odd_counter = 0 ; odd_counter < odds_index && !odd_found ; odd_counter++){
                odd_current += odds[odd_counter];
                if(  odd_current >= odds_total ){
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
	n = strlen(s);

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
	for (i = 0; i < 6; i++)
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
	for (i = 0; i < 6; i++)
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
* Get number of quest monsters for quest i
* Heino Vander Sanden
*/
int get_number_monster(int i)
{
	int num;

	if ((r_info[q_list[i].r_idx].flags1 & (RF1_UNIQUE)) ||
		(r_info[q_list[i].r_idx].flags2 & (RF2_MULTIPLY)))
		return (1);
	else
	{
		if (r_info[q_list[i].r_idx].flags1 & (RF1_FRIENDS))
			num = 10;
		else
			num = 5;

		num += rand_range(1, (q_list[i].level / 3) + 5);
		return (num);
	}
}

/*
* Get random monster
* Heino Vander Sanden
*/
int get_rnd_q_monster(int q_idx)
{
	int r_idx,j,tmp;

	tmp = rand_range(1,10);
	/* first level 6 monster (87), last monster (573) */
	switch (tmp)
	{
	case 1 : r_idx = rand_range(181,220); break;
	case 2 : r_idx = rand_range(221,260); break;
	case 3 : r_idx = rand_range(261,300); break;
	case 4 : r_idx = rand_range(301,340); break;
	case 5 : r_idx = rand_range(341,380); break;
	case 6 : r_idx = rand_range(381,420); break;
	case 7 : r_idx = rand_range(421,460); break;
	case 8 : r_idx = rand_range(461,500); break;
	case 9 : r_idx = rand_range(501,530); break;
	case 10 : r_idx = rand_range(531,560); break;
	default : r_idx = rand_range (87,573);
	}
	/* Don't allow multipliers to be random guardians */
	if (r_info[r_idx].flags2 & (RF2_MULTIPLY)) return (0);
	/* Don't allow duplicate guardians */
	for (j = 2; j < q_idx; j++)
	{
		if (q_list[j].r_idx == r_idx) return (0);
	}
	return (r_idx);
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

static byte player_init[MAX_CLASS][3][3] =
{
	{
		/* Warrior */
	{ TV_RING, SV_RING_RES_FEAR , WORN }, /* Warriors need it! */
	{ TV_SWORD, SV_BROAD_SWORD , WORN},
	{ TV_HARD_ARMOR, SV_CHAIN_MAIL , WORN }
	},
	
	{
		/* Mage */
	{ TV_BOOK_REALM1, 0 , CARRIED}, 
	{ TV_SWORD, SV_DAGGER , WORN },
	{ TV_BOOK_REALM2, 0 , CARRIED} 
	},
	
	{
		/* Priest */
	{ TV_BOOK_REALM1, 0 , CARRIED}, 
	{ TV_HAFTED, SV_MACE , WORN},
	{ TV_BOOK_REALM2, 0 , CARRIED} 
	},
	
	{
		/* Rogue */
	{ TV_BOOK_REALM1, 0 , CARRIED}, 
	{ TV_SWORD, SV_DAGGER , WORN },
	{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR , WORN }
	},
	
	{
		/* Ranger */
	{ TV_NATURE_BOOK, 0 , CARRIED},
	{ TV_SWORD, SV_BROAD_SWORD , WORN},
	{ TV_BOOK_REALM2, 0 , CARRIED}  
	},
	
	{
		/* Paladin */
	{ TV_BOOK_REALM1, 0 , CARRIED },
	{ TV_SWORD, SV_BROAD_SWORD , WORN },
	{ TV_SCROLL, SV_SCROLL_PROTECTION_FROM_EVIL , CARRIED }
	},
	
	{
		/* Warrior-Mage */
	{ TV_BOOK_REALM1, 0 , CARRIED }, 
	{ TV_SWORD, SV_SHORT_SWORD , WORN},
	{ TV_BOOK_REALM2, 0 , WORN} 
	},
	
	{
		/* Hell Knight */
	{ TV_BOOK_REALM1, 0 , CARRIED}, 
	{ TV_SWORD, SV_BROAD_SWORD , WORN},
	{ TV_HARD_ARMOR, SV_METAL_SCALE_MAIL , WORN }
	},
	
	{
		/* Mystic */
	{ TV_BOOK_REALM1, 0 , CARRIED },
	{ TV_POTION, SV_POTION_HEALING , CARRIED},
	{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR , WORN},
	},
	
	
	{
		/* Mindcrafter */
	{ TV_SWORD, SV_SMALL_SWORD, WORN },
	{ TV_POTION, SV_POTION_RESTORE_MANA , CARRIED},
	{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR , WORN},
	},
	
	{
		/* High Mage */
	{ TV_BOOK_REALM1, 0 , CARRIED}, 
	{ TV_BOOK_REALM1, 1 , CARRIED}, 
	{ TV_SWORD, SV_DAGGER , WORN}, 		
	},
	
	{
		/* Druid */
	{TV_BOOK_REALM1,0 , CARRIED}, 
	{ TV_HAFTED, SV_QUARTERSTAFF , WORN},
	{TV_AMULET,SV_AMULET_BRILLIANCE , WORN},
	},
	
	{
		/* Warlock */
	{ TV_BOOK_REALM1, 0 , CARRIED}, 
	{ TV_RING, SV_RING_SUSTAIN_MIND , WORN },
	{ TV_BOOK_REALM2, 0 , CARRIED} 
	},
	
};


static void player_give( byte tval , byte sval , byte number )
{
	object_type	forge;
	object_type	*q_ptr;
	
	/* Get local object */
	q_ptr = &forge;
	/*Prep the object with type and subtype*/
	object_prep(q_ptr, lookup_kind(tval, sval));
	/*Set the amount*/
	q_ptr->number = number;
	/* Treat them as if storebought to prevent trigger-happy squelching*/
	object_storebought(q_ptr);
	/*Put them in inventory of player*/
	(void)inven_carry(q_ptr, FALSE);
}



/*
* Init players with some belongings
*
* Having an item makes the player "aware" of its purpose.
*/
static void player_outfit(void)
{
	int i, tv, sv;

	object_type	forge;
	object_type	*q_ptr;
	/*1 scroll of recall*/	
	player_give(TV_SCROLL, SV_SCROLL_WORD_OF_RECALL, (byte)rand_range(1,1) ); 
	/*2 or 3 scrolls of teleport */
	player_give(TV_SCROLL, SV_SCROLL_TELEPORT, (byte)rand_range(2,3) ); 

	if (!rp_ptr->rations)
	{
		/* Give the player scrolls of satisfy hunger */
		player_give(TV_SCROLL, SV_SCROLL_SATISFY_HUNGER, (byte)rand_range(2,5) ); 
	}
	else
	{
		/* Give the player some food */
		player_give(TV_FOOD, SV_FOOD_RATION, (byte)rand_range(3, 7) ); 		
	}

	if (rp_ptr->hates_light)
	{
		/* Give the player scrolls of light, wtf?? */
		player_give(TV_SCROLL, SV_SCROLL_LIGHT, (byte)rand_range(3,7) ); 
		/* Give the player scrolls of DARKNESS! */
		player_give(TV_SCROLL, SV_SCROLL_DARKNESS, (byte)rand_range(2,5) ); 		
	}
	else
	{
		/* Hack -- Give the player a lantern, hack because we force pval and outfit it  */
		q_ptr = &forge;
		object_prep(q_ptr, lookup_kind(TV_LITE, SV_LITE_LANTERN));
		q_ptr->number = (char)rand_range(1,1);
		q_ptr->pval = 15000;
		object_storebought(q_ptr);
		outfit( q_ptr );
	}

	/* Hack -- Give the player three useful objects */
	for (i = 0; i < 3; i++)
	{
		/* Look up standard equipment */
		tv = player_init[p_ptr->pclass][i][0];
		sv = player_init[p_ptr->pclass][i][1];

		/* Hack to initialize spellbook of realm 1*/
		if (tv  == TV_BOOK_REALM1) 
			tv = TV_MIRACLES_BOOK + p_ptr->realm1 - 1;
		/* Hack to initialize spellbook of realm 2*/		
		else if (tv == TV_BOOK_REALM2) 
			tv = TV_MIRACLES_BOOK + p_ptr->realm2 - 1;
		/* Fearless races do not need rings of res fear */
		else if (tv == TV_RING && sv == SV_RING_RES_FEAR &&  rp_ptr->fearless  )
			sv = SV_RING_SUSTAIN_BODY;

		/* Get local object */
		q_ptr = &forge;
		/* Hack -- Give the player an object */
		object_prep(q_ptr, lookup_kind(tv, sv));
		/* Assassins begin the game with a poisoned dagger */
		if (tv == TV_SWORD && p_ptr->pclass == CLASS_ROGUE && p_ptr->realm1 == REALM_DEATH) 
		{
			q_ptr->name2 = EGO_BRAND_POIS;
		}

		object_storebought(q_ptr);
		/*Carry or wear the item*/
		if(player_init[p_ptr->pclass][i][2]==CARRIED)
			(void)inven_carry(q_ptr, FALSE);
		else 
			(void)outfit(q_ptr);
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



/*
* Generate the additional quests
* Heino Vander Sanden, Jimmy De Laet, and Robert Ruehlmann
*/
void player_birth_quests(void)
{
	int i,j;
	bool same_level;

	/* Generate to MAX_Q_IDX with random quests */
	j=0;
	do
	{
		if(q_list[j].r_idx == 0) break;
		j++;
	} while (j < MAX_QUESTS);
	if(j >= MAX_QUESTS) return; /* No room for random quests */
	for (i=j; i<MAX_Q_IDX; i++)
	{
		do
		{
			same_level = FALSE;

			/* Get a random monster */
			do
			{
				q_list[i].r_idx = get_rnd_q_monster(i);
			}
			while (!q_list[i].r_idx);

			/* Set the quest level to the level of the monster */
			q_list[i].level = r_info[q_list[i].r_idx].level;

			/* Quest monster at least 2 levels out of depth */
			q_list[i].level -= rand_range(2, 3+(q_list[i].level / 6));

			/* No 2 quests on the same level */
			for (j = 0; j<i; j++)
			{
				if (q_list[i].level == q_list[j].level)
				{
					same_level = TRUE;
					break;
				}
			}
		}
		while (same_level);

		/* Make sure uniques aren't created outside their level */
		if (r_info[q_list[i].r_idx].flags1 & RF1_UNIQUE) r_info[q_list[i].r_idx].flags1 |= RF1_ALWAYS_GUARD;

		q_list[i].max_num = get_number_monster(i);
	}
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
	int i, j, k, m, v, choice;
	int dir;
	int mode = 0;
	int a_offset, a_end;

	bool flag = FALSE;
	bool prev = FALSE;
	bool quickstart = FALSE;

	cptr str;
	
	char c;

	
	/* These vars were used for a) , one day they  might come back
		int n;	char p2 = ')';
	*/

	char b1 = '[';
	char b2 = ']';

	char buf[80];

	bool autoroll = FALSE;
	bool point_mod = TRUE;

	char inp[80];
	
	/* Yah, do me own centering logic, bad konijn! */
	int screen_width = 80;


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
			put_str( sexes_descriptions[choice][i] , 8+i , 23 );
		
		c = inkey();
		if (c == 'Q') quit(NULL);
		if (c == 'S')
		{
			quickstart = TRUE;
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
		msg_print("Autoroller disabled for now. ( Quitting ) ");
		msg_print( NULL);		
		quit(NULL);
		
		/*** Player sex ***/
		/* Set sex */
		p_ptr->psex =(char)rand_range(0,1);
		sp_ptr = &sex_info[p_ptr->psex];
		str = sp_ptr->title;
		/* Display */
		c_put_str(TERM_L_BLUE, str, 3, 15);

		/*** Player race ***/
		/* Set race */
		k=rand_range(0,COUNT_RACES-1);
		hack_corruption = FALSE;
		if (k==DEVILSPAWN) hack_corruption = TRUE;
		p_ptr->prace = k;
		rp_ptr = &race_info[p_ptr->prace];
		str = rp_ptr->title;
		/* Display */
		/*c_put_str(TERM_L_BLUE, str, 4, 15);*/
		/*** Player class ***/
		while(1)
		{
			p_ptr->pclass = (char)rand_range(0,MAX_CLASS-1);
			/* Analyze */
			cp_ptr = &class_info[p_ptr->pclass];
			mp_ptr = &realms_info[p_ptr->pclass];
			str = class_sub_name[p_ptr->pclass][p_ptr->realm1];

			if (rp_ptr->choice & (1L << p_ptr->pclass )) break;
		}
		/* Display */
		/*c_put_str(TERM_L_BLUE, class_sub_name[p_ptr->pclass][p_ptr->realm1], 5, 15);*/


		/* Get a random name */
		create_random_name(p_ptr->prace,player_name);
		/* Display */
		c_put_str(TERM_L_BLUE, player_name, 2, 13);

		get_realms_randomly();

		if(p_ptr->realm1)
		{
			if(p_ptr->realm2)
			{
				sprintf(buf,"%s/%s",realm_names[p_ptr->realm1],realm_names[p_ptr->realm2]);
			}
			else
			{
				sprintf(buf,"%s",realm_names[p_ptr->realm1]);
			}
		}

		/*if (p_ptr->realm1 || p_ptr->realm2) put_str("Magic       :", 6, 1); */
		/*if (p_ptr->realm1) c_put_str(TERM_L_BLUE, buf,6,15); */

		/* Generate quests */
		/* Set max number of quest */
		/*MAX_Q_IDX =randint(20)+10; 
		if ( MAX_Q_IDX > MAX_QUESTS ){
			
		}
		*/
		MAX_Q_IDX = MAX_QUESTS;
		initialise_quests();
		/*player_birth_quests();*/

#ifdef ALLOW_AUTOROLLER
		/* Set "autoroll" */
		autoroll = TRUE;
		/* Initialize */
		if (autoroll)
		{
			int mval[6];
			/* Clear fields */
			auto_round = 0L;
			last_round = 0L;
			/* Clean up */
			clear_from(10);
			/* Prompt for the minimum stats */
			put_str("Enter minimums for: ", 8, 3);
			/* Output the maximum stats */
			for (i = 0; i < 6; i++)
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
			for (i = 0; i < 6; i++)
			{
				stat_limit[i] = 0;
			}
			/* Save the minimum stat depending on class */
			switch(p_ptr->pclass)
			{
			case CLASS_WARRIOR:
				stat_limit[A_STR]=mval[A_STR]-1;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				stat_limit[A_CON]=mval[A_CON]-2;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				stat_limit[A_DEX]=mval[A_DEX]-4;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				break;
			case CLASS_MAGE:
				stat_limit[A_INT]=mval[A_INT]-1;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				stat_limit[A_DEX]=mval[A_DEX]-2;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				stat_limit[A_CON]=mval[A_CON]-4;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				break;
			case CLASS_PRIEST:
				stat_limit[A_WIS]=mval[A_WIS]-1;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				stat_limit[A_CHA]=mval[A_CHA]-2;
				if (stat_limit[A_CHA] > 18) stat_limit[A_CHA] -= 9;
				if (stat_limit[A_CHA] > 18) stat_limit[A_CHA] -= 9;
				stat_limit[A_CON]=mval[A_CON]-4;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				break;
			case CLASS_ROGUE:
				stat_limit[A_DEX]=mval[A_DEX]-1;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				stat_limit[A_INT]=mval[A_INT]-2;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				stat_limit[A_STR]=mval[A_STR]-4;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				break;
			case CLASS_RANGER:
				stat_limit[A_CON]=mval[A_CON]-1;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				stat_limit[A_INT]=mval[A_INT]-2;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				stat_limit[A_STR]=mval[A_STR]-4;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				break;
			case CLASS_PALADIN:
				stat_limit[A_STR]=mval[A_STR]-1;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				stat_limit[A_WIS]=mval[A_WIS]-2;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				stat_limit[A_CON]=mval[A_CON]-4;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				break;
			case CLASS_WARRIOR_MAGE:
				stat_limit[A_CON]=mval[A_CON]-1;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				stat_limit[A_INT]=mval[A_INT]-2;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				stat_limit[A_DEX]=mval[A_DEX]-4;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				break;
			case CLASS_HELL_KNIGHT:
				stat_limit[A_STR]=mval[A_STR]-1;
				if (stat_limit[A_STR] > 18) stat_limit[A_STR] -= 9;
				stat_limit[A_INT]=mval[A_INT]-2;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				stat_limit[A_CON]=mval[A_CON]-4;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				break;
			case CLASS_MYSTIC:
				stat_limit[A_DEX]=mval[A_DEX]-1;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				stat_limit[A_WIS]=mval[A_WIS]-2;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				stat_limit[A_CON]=mval[A_CON]-4;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				break;
			case CLASS_MINDCRAFTER:
				stat_limit[A_WIS]=mval[A_WIS]-1;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				stat_limit[A_CON]=mval[A_CON]-2;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				stat_limit[A_DEX]=mval[A_DEX]-4;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				break;
			case CLASS_HIGH_MAGE:
				stat_limit[A_INT]=mval[A_INT]-1;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				stat_limit[A_CON]=mval[A_CON]-2;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				stat_limit[A_DEX]=mval[A_DEX]-4;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				break;
			case CLASS_DRUID:
				stat_limit[A_WIS]=mval[A_WIS]-1;
				if (stat_limit[A_WIS] > 18) stat_limit[A_WIS] -= 9;
				stat_limit[A_CHA]=mval[A_CHA]-2;
				if (stat_limit[A_CHA] > 18) stat_limit[A_CHA] -= 9;
				if (stat_limit[A_CHA] > 18) stat_limit[A_CHA] -= 9;
				stat_limit[A_CON]=mval[A_CON]-4;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				break;
			case CLASS_WARLOCK:
				stat_limit[A_INT]=mval[A_INT]-1;
				if (stat_limit[A_INT] > 18) stat_limit[A_INT] -= 9;
				stat_limit[A_CON]=mval[A_CON]-2;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				if (stat_limit[A_CON] > 18) stat_limit[A_CON] -= 9;
				stat_limit[A_DEX]=mval[A_DEX]-4;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				if (stat_limit[A_DEX] > 18) stat_limit[A_DEX] -= 9;
				break;
			}
		}

#endif /* ALLOW_AUTOROLLER */

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
				put_str("STR:", 2 + A_STR, 61);
				put_str("INT:", 2 + A_INT, 61);
				put_str("WIS:", 2 + A_WIS, 61);
				put_str("DEX:", 2 + A_DEX, 61);
				put_str("CON:", 2 + A_CON, 61);
				put_str("CHA:", 2 + A_CHA, 61);

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
				get_stats();
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
				for (i = 0; i < 6; i++)
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

			/* Flush input */
			flush();


			/*** Display ***/

			/* Mode */
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

			p_ptr->muta1 = 0;
			p_ptr->muta2 = 0;
			p_ptr->muta3 = 0;

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
				if (c == 'S') return (FALSE);

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
		if (c == 'S') return (FALSE);

		/* Accept */
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
		
		/* Choose a genus ?*/
		choice = 0;
		while (1)
		{
			
			sprintf(buf,"What is your genus, %s?", str);
			c_put_str(TERM_YELLOW, buf, 5, ((screen_width-strlen(buf))>>1) );
			
			for(i=0;i<COUNT_RACES;i++)
				c_put_str(TERM_L_BLUE, i==choice?">":" " , 8+i , 2 );			
			
			for(i=0;i<COUNT_RACES;i++)
				c_put_str(i==choice?TERM_L_BLUE:TERM_L_WHITE, races_strings[i], 8+i, 3);
			
			for(i=0;i<COUNT_LINES;i++)
				put_str( races_descriptions[choice][i] , 8+i , 23 );
			
			c = inkey();
			if (c == 'Q') quit(NULL);
			if (c == 'S') return (FALSE);		
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
				choice=choice+1==COUNT_RACES?0:choice+1;
			if(dir==8 || c=='8')
				choice=choice==0?COUNT_RACES-1:choice-1;
			else bell();
		}		
		
		/* Set temporary race until player has drilled down */
		p_ptr->prace = choice;
		
		/* Load new birth screen with restart option ( yah, a waste of bytes, but also it guarantees a clean screen */	
		do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );	
		
		/*Set some helper variables*/
		a_offset = subraces[p_ptr->prace][0];
		a_end = subraces[p_ptr->prace][1] - subraces[p_ptr->prace][0];
		
		/* Choose specific genus ?*/
		choice = 0;
		while (1)
		{
			
			sprintf(buf,"What type of %s are you, %s?",  races_strings[p_ptr->prace] ,  str);
			c_put_str(TERM_YELLOW, buf, 5, ((screen_width-strlen(buf))>>1) );
			
			for(i=0;i<=a_end;i++)
				c_put_str(TERM_L_BLUE, i==choice?">":" " , 8+i , 2 );					
			
			for(i=0;i<=a_end;i++)
				c_put_str(i==choice?TERM_L_BLUE:TERM_L_WHITE, subraces_strings[i+a_offset], 8+i, 3);
			
			for(i=0;i<COUNT_LINES;i++)
				put_str( subraces_descriptions[choice+a_offset][i] , 8+i , 23 );
			
			c = inkey();
			if (c == 'Q') quit(NULL);
			if (c == 'S') return (FALSE);	
			if (c == '*')
			{
				choice = randint(COUNT_SUBRACES);
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
			/*get_key_map does not alway work, so we hardcode some stuff*/
			if(dir==2 || c=='2')
				choice=choice ==a_end?0:choice+1;
			if(dir==8 || c=='8')
				choice=choice==0?a_end:choice-1;
			else bell();
		}		
		
		/* Choice was just for UI, choice+a_offset has what we really want, which is now the new role of choice */
		choice = choice+a_offset;
		
		if( ( choice==SUCCUBUS || choice == LILI ) && p_ptr->psex == GENTLEMAN )
		{
		    /*No cross dressing in my game ;)*/
			msg_print("Hellband does not support demon crossdressers.");	
		    /* Set sex */
		    p_ptr->psex = LADY;
		    /* Get sex into a string */
		    sp_ptr = &sex_info[p_ptr->psex];
		    str = sp_ptr->address;
		}
		
		/*Unless we are afflicated we get the right to be born under a constellation*/
		if( p_ptr->prace == R_HUMAN && choice!= AFFLICTED )
		{
			p_ptr->prace = choice;
			/* Choose specific birth sign ?*/
			choice = 0;
			/* Load new birth screen with restart option ( yah, a waste of bytes, but also it guarantees a clean screen */	
			do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );	
			
			while (1)
			{
				
				sprintf(buf,"Which constellation were you born under, %s?",  str);
				c_put_str(TERM_YELLOW, buf, 5, ((screen_width-strlen(buf))>>1) );
				
				for(i=0;i<COUNT_SIGNS;i++)
					c_put_str(TERM_L_BLUE, i==choice?">":" " , 8+i , 2 );				
				
				for(i=0;i<COUNT_SIGNS;i++)
					c_put_str(i==choice?TERM_L_BLUE:TERM_L_WHITE, signs_strings[i], 8+i, 3);
				
				for(i=0;i<COUNT_LINES;i++)
					put_str( signs_descriptions[choice][i] , 8+i , 23 );
				
				c = inkey();
				if (c == 'Q') quit(NULL);
				if (c == 'S') return (FALSE);	
				if (c == '*')
				{
					choice = randint(COUNT_SIGNS);
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
				/*get_key_map does not alway work, so we hardcode some stuff*/
				if(dir==2 || c=='2')
					choice=choice+1==COUNT_SIGNS?0:choice+1;
				if(dir==8 || c=='8')
					choice=choice==0?COUNT_SIGNS-1:choice-1;
				else bell();
			}		
			p_ptr->psign  = choice;
	}
		/* If we are afflicted, we get to choose which afflication*/
		else if( choice == AFFLICTED)
		{
			/* Choose specific affliction ?*/
			choice = 0;
			/* Load new birth screen with restart option ( yah, a waste of bytes, but also it guarantees a clean screen */	
			do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );	
			
			while (1)
			{
				
				sprintf(buf,"What is your affliction, %s?",  str);
				c_put_str(TERM_YELLOW, buf, 5, ((screen_width-strlen(buf))>>1) );
				
				for(i=0;i<COUNT_AFFLICTIONS;i++)
					c_put_str(TERM_L_BLUE, i==choice?">":" " , 8+i , 2 );					
				
				for(i=0;i<COUNT_AFFLICTIONS;i++)
					c_put_str(i==choice?TERM_L_BLUE:TERM_L_WHITE, afflictions_strings[i], 8+i, 3);
				
				for(i=0;i<COUNT_LINES;i++)
					put_str( afflictions_descriptions[choice][i] , 8+i , 23 );
				
				c = inkey();
				if (c == 'Q') quit(NULL);
				if (c == 'S') return (FALSE);	
				if (c == '*')
				{
					choice = randint(COUNT_AFFLICTIONS);
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
				/*get_key_map does not alway work, so we hardcode some stuff*/
				if(dir==2 || c=='2')
					choice=choice+1==COUNT_AFFLICTIONS?0:choice+1;
				if(dir==8 || c=='8')
					choice=choice==0?COUNT_AFFLICTIONS-1:choice-1;
				else bell();
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
		rp_ptr = &race_info[p_ptr->prace];
		if (p_ptr->prace==DEVILSPAWN) hack_corruption= TRUE;	
		
		/* Get a random name now we have a race*/
		create_random_name(p_ptr->prace,player_name);

		/* Choose specific class ?*/
		choice = 0;
		/* Load new birth screen with restart option ( yah, a waste of bytes, but also it guarantees a clean screen */	
		do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );	
		
		while (1)
		{
			
			sprintf(buf,"What is your vocation, %s?",  str);
			c_put_str(TERM_YELLOW, buf, 5, ((screen_width-strlen(buf))>>1) );
			
			for(i=0;i<MAX_CLASS;i++)
				c_put_str(TERM_L_BLUE, i==choice?">":" " , 8+i , 2 );		
			
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
			
			for(i=0;i<COUNT_LINES;i++)
				put_str( classes_descriptions[choice][i] , 8+i , 23 );
			
			c = inkey();
			if (c == 'Q') quit(NULL);
			if (c == 'S') return (FALSE);	
			if (c == '*')
			{
				choice = randint(MAX_CLASS);
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
			/*get_key_map does not alway work, so we hardcode some stuff*/
			if(dir==2 || c=='2')
				choice=choice+1==MAX_CLASS?0:choice+1;
			if(dir==8 || c=='8')
				choice=choice==0?MAX_CLASS-1:choice-1;
			else bell();
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
				sprintf(buf,"%s/%s",realm_names[p_ptr->realm1],realm_names[p_ptr->realm2]);
			}
			else
			{
				sprintf(buf,"%s",realm_names[p_ptr->realm1]);
			}
		}

		/*if (p_ptr->realm1 || p_ptr->realm2) put_str("Magic       :", 6, 1);*/
		/*if (p_ptr->realm1) c_put_str(TERM_L_BLUE, buf,6,15);*/

		/* Clear */
		clear_from(15);


		/* Generate quests */
		/* Set max number of quest */
		/*MAX_Q_IDX =randint(20)+10;*/
		MAX_Q_IDX = MAX_QUESTS;
		/*player_birth_quests();*/
        initialise_quests();

#ifdef ALLOW_AUTOROLLER

		/*** Autoroll ***/


		/* Set "autoroll" and "point_mod" */
		autoroll = use_autoroller;
		point_mod = (spend_points & !(use_autoroller));

		/* Initialize Autoroller if necessary */
		if (autoroll)
		{
			int mval[6];
			
			do_cmd_load_screen( ANGBAND_DIR_FILE ,  "birth2.txt" );	
			sprintf(buf,"Enter minimum values for these stats");
			c_put_str(TERM_YELLOW, buf, 5, ((screen_width-strlen(buf))>>1) );			

			/* Clear fields */
			auto_round = 0L;
			last_round = 0L;

			/* Output the maximum stats */
			for (i = 0; i < 6; i++)
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
			for (i = 0; i < 6; i++)
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
				c_put_str(TERM_L_BLUE, cp_ptr->title, 5, 15);

				/* Label stats */
				put_str("STR:", 2 + A_STR, 61);
				put_str("INT:", 2 + A_INT, 61);
				put_str("WIS:", 2 + A_WIS, 61);
				put_str("DEX:", 2 + A_DEX, 61);
				put_str("CON:", 2 + A_CON, 61);
				put_str("CHA:", 2 + A_CHA, 61);

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
					for(i=0;i<6;i++)
					{
						p_ptr->stat_cur[i] = p_ptr->stat_max[i] = 8;
					}
					point_mod_player();  
				}
				else
				{
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
				for (i = 0; i < 6; i++)
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

			/* Flush input */
			flush();


			/*** Display ***/

			/* Mode */
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
				if (c == 'S') return (FALSE);

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

		/* Get a name, recolour it, prepare savefile */

		get_name();


		/* Prompt for it */
		prt("['Q' to suicide, 'S' to start over, or ESC to continue]", 23, 10);

		/* Get a key */
		c = inkey();

		/* Quit */
		if (c == 'Q') quit(NULL);

		/* Start over */
		if (c == 'S') return (FALSE);

		/* Accept */
		return (TRUE);
	}
	/* Just in case */
	return (TRUE);
}


/*
* Create a new character.
*
* Note that we may be called with "junk" leftover in the various
* fields, so we must be sure to clear them first.
*/
void player_birth(void)
{
	int i, n;

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

		/* Ignore home, hall  and pawnbrokers */
		if ((n != STORE_HOME) &&
			(n != STORE_HALL) &&
			(n != STORE_PAWN))
		{
			/* Maintain the shop (ten times) */
			for (i = 0; i < 10; i++) store_maint(n);
		}
	}
}



