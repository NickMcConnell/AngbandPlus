/*
 * File: list-blow-methods.h
 * Purpose: Monster race blow methods
 *
 * Fields:
 * cut - whether this attack can cause cuts
 * stun - whether this attack can stun
 * miss - whether the player is notified when the attack misses
 * phys - whether the attack has physical damage
 * msg - sound/message to display
 * act - action string to append (insult and moan are NULL as they are special)
 * desc - string used in monster recall
 * flavor - death message to display
 */

/*  symbol	cut		stun	miss	phys	msg             act                         desc                flavor */
RBM(NONE,	false,	false, 	false, 	false, 	MSG_GENERIC,	"",							"",                 NULL)
RBM(HIT,	true,	true, 	true, 	true, 	MSG_MON_HIT,	"hits %s.", 				"hit",              NULL)
RBM(TOUCH,	false,	false, 	true, 	false, 	MSG_MON_TOUCH,	"touches %s.",				"touch",            NULL)
RBM(PUNCH,	false,	true, 	true, 	true, 	MSG_MON_PUNCH,	"punches %s.",				"punch",            "punched to death")
RBM(KICK,	false,	true, 	true, 	true, 	MSG_MON_KICK,	"kicks %s.",				"kick",             "kicked to death")
RBM(CLAW,	true,	false, 	true, 	true, 	MSG_MON_CLAW,	"claws %s.",				"claw",             "eviscerated")
RBM(BITE,	true,	false, 	true, 	true, 	MSG_MON_BITE,	"bites %s.",				"bite",             "shred to pieces")
RBM(STING,	false,	false, 	true, 	true, 	MSG_MON_STING,	"stings %s.",				"sting",            NULL)
RBM(BUTT,	false,	true, 	true, 	true, 	MSG_MON_BUTT,	"butts %s.",				"butt",             NULL)
RBM(CRUSH,	false,	true, 	true, 	true, 	MSG_MON_CRUSH,	"crushes %s.",				"crush",            "crushed to pulp")
RBM(ENGULF,	false,	false, 	true, 	false, 	MSG_MON_ENGULF,	"engulfs %s.",				"engulf",           NULL)
RBM(CRAWL,	false,	false, 	false, 	false, 	MSG_MON_CRAWL,	"crawls on %s.",			"crawl on you",     NULL)
RBM(DROOL,	false,	false, 	false, 	false, 	MSG_MON_DROOL,	"drools on %s.",			"drool on you",     NULL)
RBM(SPIT,	false,	false, 	false, 	false, 	MSG_MON_SPIT,	"spits on %s.", 			"spit",             NULL)
RBM(GAZE,	false,	false, 	false, 	false, 	MSG_MON_GAZE,	"gazes at %s.", 			"gaze",             NULL)
RBM(WAIL,	false,	false, 	false, 	false, 	MSG_MON_WAIL,	"wails at %s.", 			"wail",             NULL)
RBM(SPORE,	false,	false, 	false, 	false, 	MSG_MON_SPORE,	"releases spores at %s.",	"release spores",   NULL)
RBM(BEG,	false,	false, 	false, 	false, 	MSG_MON_BEG,	"begs %s for money.",		"beg",              NULL)
RBM(INSULT,	false,	false, 	false, 	false, 	MSG_MON_INSULT,	NULL,						"insult",           NULL)
RBM(MOAN,	false,	false, 	false, 	false, 	MSG_MON_MOAN,	NULL,						"moan",             NULL)
