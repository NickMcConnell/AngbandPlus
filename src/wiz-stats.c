/**
 * \file wiz-stats.c
 * \brief Statistics collection on dungeon generation
 *
 * Copyright (c) 2008 Andi Sidwell
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 *  * Redistributions of source code must retain the above copyright notice,
 *    this list of conditions and the following disclaimer.
 *
 *  * Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 */
#include "math.h"
#include "angband.h"
#include "cave.h"
#include "cmds.h"
#include "effects.h"
#include "game-input.h"
#include "generate.h"
#include "init.h"
#include "mon-make.h"
#include "monster.h"
#include "obj-init.h"
#include "obj-make.h"
#include "obj-pile.h"
#include "obj-randart.h"
#include "obj-tval.h"
#include "obj-util.h"
#include "object.h"
#include "ui-command.h"
#include "wizard.h"

/**
 * The stats programs here will provide information on the dungeon, the monsters
 * in it, and the items that they drop.  Statistics are gotten from a given
 * level by generating a new level, collecting all the items (noting if they
 * were generated in a vault).  Then all non-unique monsters are killed and
 * their stats are tracked.
 * The items from these monster drops are then collected and analyzed.  Lastly,
 * all unique monsters are killed, and their drops are analyzed.  In this way,
 * it is possible to separate unique drops and normal monster drops.
 *
 * There are two options for simulating the entirety of the dungeon.  There is
 * a "diving" option that begins each level with all artifacts and uniques
 * available; and there is a "level-clearing" option that simulates all 100
 * levels of the dungeon, removing artifacts and uniques as they are
 * discovered/killed.  "diving" option only catalogues every 5 levels.
 *
 * At the end of the "level-clearing" log file, extra post-processing is done
 * to find the mean and standard deviation for the level you are likely to
 * first gain an item with a key resistance or item.
 * 
 * In addition to these sims there is a shorter sim that tests for dungeon
 * connectivity.
*/

double *wiz_stats_prob;

#ifdef USE_STATS

/*** Statsgen ***/

/* Logfile to store results in */
static ang_file *stats_log = NULL;

 /* this is the size of arrays used to calculate mean and std_dev.
  * these values will be calculated over the first TRIES_SIZE attempts
  * or less if TRIES_SIZE is less than tries
  */
 #define TRIES_SIZE 100
 #define MAX_LVL 101
 
 /* default for number of tries */
int tries=50;
/* the simulation number that we are on */
int iter;
/* amount to add each time an item comes up */
static double addval;
/* flag for whether we are in clearing mode */
bool clearing = false;
/* flag for regenning randart */
bool regen = false;

/*** These are items to track for each iteration ***/
/* total number of artifacts, egos found */
static int art_it[TRIES_SIZE];
static int ego_it[TRIES_SIZE];

/*** handle gold separately ***/
/* gold */
static double gold_total[MAX_LVL], gold_floor[MAX_LVL], gold_mon[MAX_LVL];


typedef enum stat_code
{
	ST_BEGIN,
	ST_EQUIPMENT,
	ST_FA_EQUIPMENT,
	ST_SI_EQUIPMENT,
	ST_RESIST_EQUIPMENT,
	ST_RBASE_EQUIPMENT,
	ST_RPOIS_EQUIPMENT,
	ST_RNEXUS_EQUIPMENT,
	ST_RBLIND_EQUIPMENT,
	ST_RCONF_EQUIPMENT,
	ST_SPEED_EQUIPMENT,
	ST_TELEP_EQUIPMENT,
	ST_ARMORS,
	ST_BAD_ARMOR,
	ST_AVERAGE_ARMOR,
	ST_GOOD_ARMOR,	
	ST_STR_ARMOR,
	ST_INT_ARMOR,
	ST_WIS_ARMOR,
	ST_DEX_ARMOR,
	ST_CON_ARMOR,
	ST_FAULTY_ARMOR,
	ST_WEAPONS,
	ST_BAD_WEAPONS,
	ST_AVERAGE_WEAPONS,
	ST_GOOD_WEAPONS,
	ST_SLAY_WEAPONS,
	ST_SLAYEVIL_WEAPONS,
	ST_KILL_WEAPONS,
	ST_BRAND_WEAPONS,
	ST_WESTERNESSE_WEAPONS,
	ST_DEFENDER_WEAPONS,
	ST_GONDOLIN_WEAPONS,
	ST_HOLY_WEAPONS,
	ST_XTRABLOWS_WEAPONS,
	ST_TELEP_WEAPONS,
	ST_HUGE_WEAPONS,
	ST_ENDGAME_WEAPONS,
	ST_MORGUL_WEAPONS,
	ST_GUNS,
	ST_BAD_GUNS,
	ST_AVERAGE_GUNS,
	ST_GOOD_GUNS,
	ST_VERYGOOD_GUNS,
	ST_XTRAMIGHT_GUNS,
	ST_XTRASHOTS_GUNS,
	ST_BUCKLAND_GUNS,
	ST_TELEP_GUNS,
	ST_FAULTY_GUNS,
	ST_PILLS,
	ST_GAINSTAT_PILLS,
	ST_HEALING_PILLS,
	ST_BIGHEAL_PILLS,
	ST_CARDS,
	ST_ENDGAME_CARDS,
	ST_ACQUIRE_CARDS,
	ST_RODS,
	ST_UTILITY_RODS,
	ST_TELEPOTHER_RODS,
	ST_DETECTALL_RODS,
	ST_ENDGAME_RODS,
	ST_DEVICES,
	ST_SPEED_DEVICES,
	ST_DESTRUCTION_DEVICES,
	ST_KILL_DEVICES,
	ST_ENDGAME_DEVICES,
	ST_WANDS,
	ST_TELEPOTHER_WANDS,
	ST_AMMO,
	ST_BAD_AMMO,
	ST_AVERAGE_AMMO,
	ST_GOOD_AMMO,
	ST_BRANDSLAY_AMMO,
	ST_VERYGOOD_AMMO,
	ST_AWESOME_AMMO,
	ST_SLAYEVIL_AMMO,
	ST_HOLY_AMMO,
	ST_END
}	
stat_code;


struct stat_data
{
	stat_code st;
	char *name;
};

static const struct stat_data stat_message[] =
{
	{ST_BEGIN, ""},
	{ST_EQUIPMENT, "\n ***EQUIPMENT*** \n All:       "},
	{ST_FA_EQUIPMENT, " Free Action "},
	{ST_SI_EQUIPMENT, " See Invis   "},
	{ST_RESIST_EQUIPMENT, " Low Resist  "},
	{ST_RBASE_EQUIPMENT, " Resist Base "},
	{ST_RPOIS_EQUIPMENT, " Resist Pois "},
	{ST_RNEXUS_EQUIPMENT, " Res. Nexus  "},
	{ST_RBLIND_EQUIPMENT, " Res. Blind  "},	
	{ST_RCONF_EQUIPMENT, " Res. Conf.  "},
	{ST_SPEED_EQUIPMENT, " Speed       "},
	{ST_TELEP_EQUIPMENT, " Telepathy   "},
	{ST_ARMORS,  "\n ***ARMOR***      \n All:      "},
	{ST_BAD_ARMOR, " Bad         "},
	{ST_AVERAGE_ARMOR, " Average     "},
	{ST_GOOD_ARMOR, " Good        "},	
	{ST_STR_ARMOR, " +Strength   "},
	{ST_INT_ARMOR, " +Intel.     "},
	{ST_WIS_ARMOR, " +Wisdom     "},
	{ST_DEX_ARMOR, " +Dexterity  "},
	{ST_CON_ARMOR, " +Const.     "},
	{ST_FAULTY_ARMOR, " Faulty       "},
	{ST_WEAPONS, "\n ***WEAPONS***   \n All:       "},
	{ST_BAD_WEAPONS, " Bad         "},
	{ST_AVERAGE_WEAPONS, " Average     "},
	{ST_GOOD_WEAPONS, " Good        "},
	{ST_SLAY_WEAPONS, " Weak Slay   "},
	{ST_SLAYEVIL_WEAPONS, " Slay evil   "},
	{ST_KILL_WEAPONS, " *Slay*      "},
	{ST_BRAND_WEAPONS, " Brand       "},
	{ST_WESTERNESSE_WEAPONS, " Westernesse "},
	{ST_DEFENDER_WEAPONS, " Defender    "},
	{ST_GONDOLIN_WEAPONS, " Gondolin    "},
	{ST_HOLY_WEAPONS, " Holy Avengr "},
	{ST_XTRABLOWS_WEAPONS, " Extra Blows "},
	{ST_TELEP_WEAPONS, " Telepathy   "},
	{ST_HUGE_WEAPONS, " Huge        "},//MoD, SoS and BoC
	{ST_ENDGAME_WEAPONS, " Endgame     "},//MoD, SoS and BoC with slay evil or x2B
	{ST_MORGUL_WEAPONS, " Morgul      "},
	{ST_GUNS, "\n ***LAUNCHERS*** \n All:        "},
	{ST_BAD_GUNS, " Bad         "},
	{ST_AVERAGE_GUNS, " Average     "},
	{ST_GOOD_GUNS, " Good        "},
	{ST_VERYGOOD_GUNS, " Very Good   "},//Power > 15
	{ST_XTRAMIGHT_GUNS, " Extra might "},
	{ST_XTRASHOTS_GUNS, " Extra shots "},
	{ST_BUCKLAND_GUNS, " Buckland    "},
	{ST_TELEP_GUNS, " Telepathy   "},
	{ST_FAULTY_GUNS, " Faulty      "},
	{ST_PILLS, "\n ***PILLS***   \n All:        "},
	{ST_GAINSTAT_PILLS, " Gain stat   "},//includes *enlight*
	{ST_HEALING_PILLS, " Healing     "},
	{ST_BIGHEAL_PILLS, " Big heal    "},//*heal* and life
	{ST_CARDS, "\n ***CARDS***   \n All:        "},
	{ST_ENDGAME_CARDS, " Endgame     "},// destruction, banish, mass banish, rune
	{ST_ACQUIRE_CARDS, " Acquire.    "},
	{ST_RODS, "\n ***GADGETS***      \n All:        "},
	{ST_UTILITY_RODS, " Utility     "},//dtrap, dstairs, dobj, light, illum
	{ST_TELEPOTHER_RODS, " Tele Other  "},
	{ST_DETECTALL_RODS, " Detect all  "},
	{ST_ENDGAME_RODS, " Endgame     "},//speed, healing
	{ST_DEVICES, "\n ***DEVICES***    \n All:        "},
	{ST_SPEED_DEVICES, " Speed       "},
	{ST_DESTRUCTION_DEVICES, " Destruction "},
	{ST_KILL_DEVICES, " Kill        "},//dispel evil, power, holiness
	{ST_ENDGAME_DEVICES, " Endgame     "},//healing, magi, banishment
	{ST_WANDS, "\n ***WANDS***     \n All:        "},
	{ST_TELEPOTHER_WANDS, " Tele Other  "},
	{ST_AMMO, "\n ***AMMO***      \n All:        "},
	{ST_BAD_AMMO, " Bad         "},
	{ST_AVERAGE_AMMO, " Average     "},
	{ST_GOOD_AMMO, " Good        "},
	{ST_BAD_AMMO, " Brand       "},
	{ST_VERYGOOD_AMMO, " Very Good   "},//seeker or mithril
	{ST_AWESOME_AMMO, " Awesome     "},//seeker, mithril + brand
	{ST_SLAYEVIL_AMMO, " Slay evil   "},
	{ST_HOLY_AMMO, " Holy might  "},
};	

double stat_all[ST_END][3][MAX_LVL];
	
/* Values for things we want to find the level where it's
 * most likely to be first found */
typedef enum stat_first_find
{
	ST_FF_BEGIN,
	ST_FF_FA,
	ST_FF_SI,
	ST_FF_RPOIS,
	ST_FF_RNEXUS,
	ST_FF_RCONF,
	ST_FF_RBLIND,
	ST_FF_TELEP,
	ST_FF_END
}	
stat_first_find;

struct stat_ff_data
{
	stat_first_find st_ff;
	stat_code st;
	char *name;
};

static const struct stat_ff_data stat_ff_message[] =
{
	{ST_FF_BEGIN,ST_BEGIN,""},
	{ST_FF_FA,	ST_FA_EQUIPMENT,		"FA     \t"},
	{ST_FF_SI,	ST_SI_EQUIPMENT,		"SI     \t"},
	{ST_FF_RPOIS,	ST_RPOIS_EQUIPMENT,	"Rpois  \t"},
	{ST_FF_RNEXUS,	ST_RNEXUS_EQUIPMENT,  "Rnexus \t"},
	{ST_FF_RCONF,	ST_RCONF_EQUIPMENT,	"Rconf  \t"},
	{ST_FF_RBLIND,	ST_RBLIND_EQUIPMENT,	"Rblind \t"},
	{ST_FF_TELEP,	ST_TELEP_EQUIPMENT,	"Telep  \t"},
};

int stat_ff_all[ST_FF_END][TRIES_SIZE];



/* basic artifact info */
static double art_total[MAX_LVL], art_spec[MAX_LVL], art_norm[MAX_LVL];

/* artifact level info */
static double art_shal[MAX_LVL], art_ave[MAX_LVL], art_ood[MAX_LVL];

/* where normal artifacts come from */
static double art_mon[MAX_LVL], art_uniq[MAX_LVL], art_floor[MAX_LVL], art_vault[MAX_LVL], art_mon_vault[MAX_LVL];



/* basic ego info */
static double ego_total[MAX_LVL];

/* ego level info */
static double ego_shal[MAX_LVL], ego_ave[MAX_LVL], ego_ood[MAX_LVL];

/* where egos come from */
static double ego_mon[MAX_LVL], ego_uniq[MAX_LVL], ego_floor[MAX_LVL], ego_vault[MAX_LVL], ego_mon_vault[MAX_LVL];


/* monster info */
static double mon_total[MAX_LVL], mon_ood[MAX_LVL], mon_deadly[MAX_LVL];

/* unique info */
static double uniq_total[MAX_LVL], uniq_ood[MAX_LVL], uniq_deadly[MAX_LVL];


/* set everything to 0.0 to begin */
static void init_stat_vals()
{
	int i,j,k;

	for (i = 0; i < ST_END;i++)
		for (j = 0; j < 3; k = j++)
			for (k = 0; k < MAX_LVL; k++)
				stat_all[i][j][k] = 0.0;
				
	for (i = 1; i < TRIES_SIZE; i++)
		art_it[i] = 0;
	
	for (i = 0; i < ST_FF_END; i++)
		for (j = 0; j < TRIES_SIZE; j++)
			stat_ff_all[i][j] = 0.0;
}

/*
 *	Record the first level we find something
 */
static bool first_find(stat_first_find st)
{
	/* make sure we're not on an iteration above our array limit */
	if (iter >= TRIES_SIZE) return false;

	/* make sure we haven't found it earlier on this iteration */
	if (stat_ff_all[st][iter] > 0) return false;

	/* assign the depth to this value */
	stat_ff_all[st][iter] = player->depth;

	/* success */
	return true;
}

/*
 * Add the number of drops for a specific stat
 */
static void add_stats(stat_code st, bool vault, bool mon, int number)
{
	int lvl;
	
	/* get player level */
	lvl=player->depth;
	
	/* be careful about bounds */
	if ((lvl > MAX_LVL) || (lvl < 0)) return;
	
	/* add to the total */
	stat_all[st][0][lvl] += addval * number;
	
	/* add to the total from vaults */
	if ((!mon) && (vault)) stat_all[st][2][lvl] += addval * number;
	
	/* add to the total from monsters */
	if (mon) stat_all[st][1][lvl] += addval * number;

}	
	
/*
 * This will get data on an object
 * It gets a lot of stuff, pretty much everything that I
 * thought was reasonable to get.  However, you might have
 * a much different opinion.  Luckily, I tried to make it
 * trivial to add new items to log.
*/ 
static void get_obj_data(const struct object *obj, int y, int x, bool mon,
						 bool uniq)
{

	bool vault = square_isvault(cave, loc(x, y));
	int number = obj->number;
	static int lvl;
	struct artifact *art;

	double gold_temp = 0;

	assert(obj->kind);

	/* get player depth */
	lvl = player->depth;

	/* check for some stuff that we will use regardless of type */
	/* originally this was armor, but I decided to generalize it */

	/* has free action */
	if (of_has(obj->flags, OF_FREE_ACT)) {
		/* add the stats */
		add_stats(ST_FA_EQUIPMENT, vault, mon, number);

		/* record first level */
		first_find(ST_FF_FA);
	}


	/* has see invis */
	if (of_has(obj->flags, OF_SEE_INVIS)){

		add_stats(ST_SI_EQUIPMENT, vault, mon, number);
		first_find(ST_FF_SI);
	}
	/* has at least one basic resist */
 	if ((obj->el_info[ELEM_ACID].res_level == 1) ||
		(obj->el_info[ELEM_ELEC].res_level == 1) ||
		(obj->el_info[ELEM_COLD].res_level == 1) ||
		(obj->el_info[ELEM_FIRE].res_level == 1)){

			add_stats(ST_RESIST_EQUIPMENT, vault, mon, number);
	}

	/* has rbase */
	if ((obj->el_info[ELEM_ACID].res_level == 1) &&
		(obj->el_info[ELEM_ELEC].res_level == 1) &&
		(obj->el_info[ELEM_COLD].res_level == 1) &&
		(obj->el_info[ELEM_FIRE].res_level == 1))
		add_stats(ST_RBASE_EQUIPMENT, vault, mon, number);

	/* has resist poison */
	if (obj->el_info[ELEM_POIS].res_level == 1){

		add_stats(ST_RPOIS_EQUIPMENT, vault, mon, number);
		first_find(ST_FF_RPOIS);
		
	}
	/* has resist nexus */
	if (obj->el_info[ELEM_NEXUS].res_level == 1){

		add_stats(ST_RNEXUS_EQUIPMENT, vault, mon, number);
		first_find(ST_FF_RNEXUS);
	}
	/* has resist blind */
	if (of_has(obj->flags, OF_PROT_BLIND)){

		add_stats(ST_RBLIND_EQUIPMENT, vault, mon, number);
		first_find(ST_FF_RBLIND);
	}

	/* has resist conf */
	if (of_has(obj->flags, OF_PROT_CONF)){

		add_stats(ST_RCONF_EQUIPMENT, vault, mon, number);
		first_find(ST_FF_RCONF);
	}

	/* has speed */
	if (obj->modifiers[OBJ_MOD_SPD] != 0)
		add_stats(ST_SPEED_EQUIPMENT, vault, mon, number);

	/* has telepathy */
	if (of_has(obj->flags, OF_TELEPATHY)){

		add_stats(ST_TELEP_EQUIPMENT, vault, mon, number);
		first_find(ST_FF_TELEP);
	}

	switch(obj->tval){

		/* armor */
		case TV_BOOTS:
		case TV_GLOVES:
		case TV_HELM:
		case TV_CROWN:
		case TV_SHIELD:
		case TV_CLOAK:
		case TV_BELT:
		case TV_SOFT_ARMOR:
		case TV_HARD_ARMOR:
		case TV_DRAG_ARMOR:{

			/* do not include artifacts */
			if (obj->artifact) break;

			/* add to armor total */
			add_stats(ST_ARMORS, vault, mon, number);

			/* check if bad, good, or average */
			if (obj->to_a < 0)
				add_stats(ST_BAD_ARMOR, vault, mon, number);
			if (obj->to_h == 0)
				add_stats(ST_AVERAGE_ARMOR, vault, mon, number);
			if (obj->to_h > 0)
				add_stats(ST_GOOD_ARMOR, vault, mon, number);

			/* has str boost */
			if (obj->modifiers[OBJ_MOD_STR] != 0)
				add_stats(ST_STR_ARMOR, vault, mon, number);

			/* has dex boost */
			if (obj->modifiers[OBJ_MOD_DEX] != 0)
				add_stats(ST_DEX_ARMOR, vault, mon, number);

			/* has int boost */
			if (obj->modifiers[OBJ_MOD_INT] != 0)
				add_stats(ST_INT_ARMOR, vault, mon, number);

			if (obj->modifiers[OBJ_MOD_WIS] != 0)
				add_stats(ST_WIS_ARMOR, vault, mon, number);

			if (obj->modifiers[OBJ_MOD_CON] != 0)
				add_stats(ST_CON_ARMOR, vault, mon, number);

			if (obj->faults)
				add_stats(ST_FAULTY_ARMOR, vault, mon, number);

			break;
		}

		/* weapons */
		case TV_DIGGING:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_SWORD:{

			/* do not include artifacts */
			if (obj->artifact) break;

			/* add to weapon total */
			add_stats(ST_WEAPONS, vault, mon, number);

			/* check if bad, good, or average */
			if ((obj->to_h < 0)  && (obj->to_d < 0))
				add_stats(ST_BAD_WEAPONS, vault, mon, number);
			if ((obj->to_h == 0) && (obj->to_d == 0))
				add_stats(ST_AVERAGE_WEAPONS, vault, mon, number);
			if ((obj->to_h > 0) && (obj->to_d > 0))
				add_stats(ST_GOOD_WEAPONS, vault, mon, number);

			/* Egos by name - changes results a little */
			if (obj->ego) {
				/* slay evil */
				if (strstr(obj->ego->name, "of Slay Evil"))
					add_stats(ST_SLAYEVIL_WEAPONS, vault, mon, number);

				/* slay weapons */
				else if (strstr(obj->ego->name, "of Slay"))
					add_stats(ST_SLAY_WEAPONS, vault, mon, number);
				/* kill flag */
				if (strstr(obj->ego->name, "of *Slay"))
					add_stats(ST_KILL_WEAPONS, vault, mon, number);

				/* determine westernesse by flags */
				if (strstr(obj->ego->name, "Westernesse"))
					add_stats(ST_WESTERNESSE_WEAPONS, vault, mon, number);

				/* determine defender by flags */
				if (strstr(obj->ego->name, "Defender"))
					add_stats(ST_DEFENDER_WEAPONS, vault, mon, number);

				/* determine gondolin by flags */
				if (strstr(obj->ego->name, "Gondolin"))
					add_stats(ST_GONDOLIN_WEAPONS, vault, mon, number);

				/* determine holy avenger by flags */
				if (strstr(obj->ego->name, "Avenger"))
					add_stats(ST_HOLY_WEAPONS, vault, mon, number);

				/* is morgul */
				if (strstr(obj->ego->name, "Morgul"))
					add_stats(ST_MORGUL_WEAPONS, vault, mon, number);
			}

			/* branded weapons */
			if (obj->brands)
				add_stats(ST_BRAND_WEAPONS, vault, mon, number);

			/* extra blows */
			if (obj->modifiers[OBJ_MOD_BLOWS] > 0)
				add_stats(ST_XTRABLOWS_WEAPONS, vault, mon, number);

			/* telepathy */
			if (of_has(obj->flags, OF_TELEPATHY))
				add_stats(ST_TELEP_WEAPONS, vault, mon, number);

			/* is a top of the line weapon */
			if (((obj->tval == TV_HAFTED) &&
				 (!strstr(obj->kind->name, "Disruption"))) ||
				((obj->tval == TV_POLEARM) &&
				 (!strstr(obj->kind->name, "Slicing"))) ||
				((obj->tval == TV_SWORD) &&
				 (!strstr(obj->kind->name, "Chaos")))) {
				add_stats(ST_HUGE_WEAPONS, vault, mon, number);

				/* is uber need to fix ACB
				if ((of_has(obj->flags, OF_SLAY_EVIL)) || (obj->modifiers[OBJ_MOD_BLOWS] > 0))
				add_stats(ST_UBWE, vault, mon, number); */

			}

			break;
		}

		/* launchers */
		case TV_GUN:{

			/* do not include artifacts */
			if (obj->artifact) break;

			/* add to launcher total */
			add_stats(ST_GUNS, vault, mon, number);

			/* check if bad, average, good, or very good */
			if ((obj->to_h < 0) && (obj->to_d < 0))
				add_stats(ST_BAD_GUNS, vault, mon, number);
			if ((obj->to_h == 0) && (obj->to_d == 0))
				add_stats(ST_AVERAGE_GUNS, vault, mon, number);
			if ((obj->to_h > 0) && (obj->to_d > 0))
				add_stats(ST_GOOD_GUNS, vault, mon, number);
			if ((obj->to_h > 15) || (obj->to_d > 15))
				add_stats(ST_VERYGOOD_GUNS, vault, mon, number);

			/* check for xtra might and/or shots */
			if (obj->modifiers[OBJ_MOD_SHOTS] > 0)
				add_stats(ST_XTRASHOTS_GUNS, vault, mon, number);

			if (obj->modifiers[OBJ_MOD_MIGHT] > 0)
				add_stats(ST_XTRAMIGHT_GUNS, vault, mon, number);

			/* check for buckland */
			if ((obj->modifiers[OBJ_MOD_MIGHT] > 0) &&
				(obj->modifiers[OBJ_MOD_SHOTS] > 0))
					add_stats(ST_BUCKLAND_GUNS, vault, mon, number);

			/* has telep */
			if (of_has(obj->flags, OF_TELEPATHY))
				add_stats(ST_TELEP_GUNS, vault, mon, number);

			/* is faulty */
			if (obj->faults)
				add_stats(ST_FAULTY_GUNS, vault, mon, number);
			break;
		}

		/* pill */
		case TV_PILL:{

			/* Add total amounts */
			add_stats(ST_PILLS, vault, mon, number);

			/* Stat gain */
			if (strstr(obj->kind->name, "Strength") ||
				strstr(obj->kind->name, "Intelligence") ||
				strstr(obj->kind->name, "Wisdom") ||
				strstr(obj->kind->name, "Dexterity") ||
				strstr(obj->kind->name, "Constitution")) {
				add_stats(ST_GAINSTAT_PILLS, vault, mon, number);
			} else if (strstr(obj->kind->name, "Augmentation")) {
				/* Augmentation counts as 5 stat gain pots */
				add_stats(ST_GAINSTAT_PILLS, vault, mon, number * 5);
			} else if (strstr(obj->kind->name, "*Enlightenment*")) {
				/* *Enlight* counts as 2 stat pots */
				add_stats(ST_GAINSTAT_PILLS, vault, mon, number * 2);
			} else if ((strstr(obj->kind->name, "Life")) ||
					   (strstr(obj->kind->name, "*Healing*"))) {
				add_stats(ST_BIGHEAL_PILLS, vault, mon, number);
			} else if (strstr(obj->kind->name, "Healing")) {
				add_stats(ST_HEALING_PILLS, vault, mon, number);
			}
			break;
		}

		/* cards */
		case TV_CARD:{

			/* add total amounts */
			add_stats(ST_CARDS, vault, mon, number);

			if (strstr(obj->kind->name, "Banishment") ||
				strstr(obj->kind->name, "Mass Banishment") ||
				strstr(obj->kind->name, "Rune of Protection") ||
				strstr(obj->kind->name, "*Destruction*")) {
				add_stats(ST_ENDGAME_CARDS, vault, mon, number);
			} else if (strstr(obj->kind->name, "Acquirement")) {
				add_stats(ST_ACQUIRE_CARDS, vault, mon, number);
			} else if (strstr(obj->kind->name, "*Acquirement*")) {
				/* do the effect of 2 acquires */
				add_stats(ST_ACQUIRE_CARDS, vault, mon, number * 2);
			}
			break;
		}

		/* rods */
		case TV_GADGET:{

			/* add to total */
			add_stats(ST_RODS, vault, mon, number);

			if (strstr(obj->kind->name, "Trap Detection") ||
				strstr(obj->kind->name, "Treasure Detection") ||
				strstr(obj->kind->name, "Door/Stair Location") ||
				strstr(obj->kind->name, "Illumination") ||
				strstr(obj->kind->name, "Light")) {
				add_stats(ST_UTILITY_RODS, vault, mon, number);
			} else if (strstr(obj->kind->name, "Teleport Other")) {
				add_stats(ST_TELEPOTHER_RODS, vault, mon, number);
			} else if (strstr(obj->kind->name, "Detection")) {
				add_stats(ST_DETECTALL_RODS, vault, mon, number);
			} else if (strstr(obj->kind->name, "Speed") ||
					   strstr(obj->kind->name, "Healing")) {
				add_stats(ST_ENDGAME_RODS, vault, mon, number);
			}
			break;
		}

		/* devices */
		case TV_DEVICE:{

			add_stats(ST_DEVICES, vault, mon, number);

			if (strstr(obj->kind->name, "Speed")) {
				add_stats(ST_SPEED_DEVICES, vault, mon, number);
			} else if (strstr(obj->kind->name, "*Destruction*")) {
				add_stats(ST_DESTRUCTION_DEVICES, vault, mon, number);
			} else if (strstr(obj->kind->name, "Dispel Evil") ||
					   strstr(obj->kind->name, "Power") ||
					   strstr(obj->kind->name, "Holiness")) {
				add_stats(ST_KILL_DEVICES, vault, mon, number);
			} else if (strstr(obj->kind->name, "Healing") ||
					   strstr(obj->kind->name, "Banishment") ||
					   strstr(obj->kind->name, "the Magi")) {
				add_stats(ST_ENDGAME_DEVICES, vault, mon, number);
			}
			break;
		}

		case TV_WAND:{

			add_stats(ST_WANDS, vault, mon, number);

			if (strstr(obj->kind->name, "Teleport Other"))
				add_stats(ST_TELEPOTHER_WANDS, vault, mon, number);
			break;
		}

		case TV_AMMO_6:
		case TV_AMMO_9:
		case TV_AMMO_12:{

			add_stats(ST_AMMO, vault, mon, number);

			/* check if bad, average, good */
			if ((obj->to_h < 0) && (obj->to_d < 0))
				add_stats(ST_BAD_AMMO, vault, mon, number);
			if ((obj->to_h == 0) && (obj->to_d == 0))
				add_stats(ST_AVERAGE_AMMO, vault, mon, number);
			if ((obj->to_h > 0) && (obj->to_d > 0))
				add_stats(ST_GOOD_AMMO, vault, mon, number);

			if (obj->ego)
				add_stats(ST_BRANDSLAY_AMMO, vault, mon, number);

			if (strstr(obj->kind->name, "Seeker") ||
				strstr(obj->kind->name, "Mithril")) {

				/* Mithril and seeker ammo */
				add_stats(ST_VERYGOOD_AMMO, vault, mon, number);

				/* Ego mithril and seeker ammo */
				if (obj->ego) {
					add_stats(ST_AWESOME_AMMO, vault, mon, number);

					if (strstr(obj->ego->name, "of Slay Evil"))
						add_stats(ST_SLAYEVIL_AMMO, vault, mon, number);

					if (strstr(obj->ego->name, "of Holy Might"))
						add_stats(ST_HOLY_AMMO, vault, mon, number);
				}
			}
			break;
		}
	}
	/* check to see if we have an ego */
	if (obj->ego){

		/* add to ego level total */
		ego_total[lvl] += addval;

		/* add to the ego iteration total */
		if (iter < TRIES_SIZE) ego_it[iter]++;

		/* Obtain the ego info */
		struct ego_item *ego = obj->ego;

		/* ego is shallow */
		if (ego->alloc_min < (player->depth / 2)) ego_shal[lvl] += addval;

		/* artifact is close to the player depth */
		if ((ego->alloc_min >= player->depth / 2) &&
			(ego->alloc_min <= player->depth )) ego_ave[lvl] += addval;

		/* artifact is out of depth */
		if (ego->alloc_min > (player->depth)) ego_ood[lvl] += addval;

		/* did it come from a monster? */
		if (mon) ego_mon[lvl] += addval;

		/* did it come from a unique? */
		if (uniq) ego_uniq[lvl] += addval;

		/* was it in a vault? */
		if (vault){
			/* did a monster drop it ?*/
			if ((mon) || (uniq)) ego_mon_vault[lvl] += addval;
			else ego_vault[lvl] += addval;
		} else {
			/* was it just lyin' on the floor? */
			if ((!uniq) && (!mon)) ego_floor[lvl] += addval;
		}
	}

	/* check to see if we have an artifact */
	if (obj->artifact){

		/* add to artifact level total */
		art_total[lvl] += addval;

		/* add to the artifact iteration total */
		if (iter < TRIES_SIZE) art_it[iter]++;

		/* Obtain the artifact info */
		art = obj->artifact;

		//debugging, print out that we found the artifact
		//msg_format("Found artifact %s",art->name);

		/* artifact is shallow */
		if (art->alloc_min < (player->depth - 20)) art_shal[lvl] += addval;

		/* artifact is close to the player depth */
		if ((art->alloc_min >= player->depth - 20) &&
			(art->alloc_min <= player->depth )) art_ave[lvl] += addval;

		/* artifact is out of depth */
		if (art->alloc_min > (player->depth)) art_ood[lvl] += addval;

		/* check to see if it's a special artifact */
		if (obj->tval == TV_LIGHT) {
			/* increment special artifact counter */
			art_spec[lvl] += addval;
		} else {
			/* increment normal artifacts */
			art_norm[lvl] += addval;

			/* did it come from a monster? */
			if (mon) art_mon[lvl] += addval;

			/* did it come from a unique? */
			if (uniq) art_uniq[lvl] += addval;

			/* was it in a vault? */
			if (vault){

				/* did a monster drop it ?*/
				if ((mon) || (uniq)) art_mon_vault[lvl] += addval;
				else art_vault[lvl] += addval;
			} else {
				/* was it just lyin' on the floor? */
				if ((!uniq) && (!mon)) art_floor[lvl] += addval;
			}
		}
		/* preserve the artifact */
		if (!(clearing)) art->created = false;
	}

	/* Get info on gold. */
	if (obj->tval == TV_GOLD){

		int temp = obj->pval;
		gold_temp = temp;
	    gold_total[lvl] += (gold_temp / tries);

		/*From a monster? */
		if ((mon) || (uniq)) gold_mon[lvl] += (gold_temp / tries);
		else gold_floor[lvl] += (gold_temp / tries);
	}

}



/* 
 * A rewrite of monster death that gets rid of some features
 * that we don't want to deal with.  Namely, no notifying the
 * player and no generation of Morgoth artifacts
 * 
 * It also replaces drop near with a new function that drops all 
 * the items on the exact square that the monster was on.
 */
void monster_death_stats(int m_idx)
{
	struct object *obj;
	struct monster *mon;
	bool uniq;

	assert(m_idx > 0);
	mon = cave_monster(cave, m_idx);

	/* Check if monster is UNIQUE */
	uniq = rf_has(mon->race->flags,RF_UNIQUE);

	/* Mimicked objects will have already been counted as floor objects */
	mon->mimicked_obj = NULL;

	/* Drop objects being carried */
	obj = mon->held_obj;
	while (obj) {
		struct object *next = obj->next;

		/* Object no longer held */
		obj->held_m_idx = 0;

		/* Get data */
		get_obj_data(obj, mon->grid.y, mon->grid.x, true, uniq);

		/* Delete the object */
		delist_object(cave, obj);
		object_delete(&obj);

		/* Next */
		obj = next;
	}

	/* Forget objects */
	mon->held_obj = NULL;
}



/**
 * This will collect stats on a monster avoiding all unique monsters.
 * Afterwards it will kill the monsters.
 */
static bool stats_monster(struct monster *mon, int i)
{
	static int lvl;

	/* get player depth */
	lvl = player->depth;


	/* Increment monster count */
	mon_total[lvl] += addval;

	/* Increment unique count if appropriate */
	if (rf_has(mon->race->flags, RF_UNIQUE)){

		/* add to total */
		uniq_total[lvl] += addval;

		/* kill the unique if we're in clearing mode */
		if (clearing) mon->race->max_num = 0;

		/* debugging print that we killed it
		   msg_format("Killed %s",race->name); */
	}

	/* Is it mostly dangerous (10 levels ood or less?)*/
	if ((mon->race->level > player->depth) && 
		(mon->race->level <= player->depth + 10)) {

			mon_ood[lvl] += addval;

			/* Is it a unique */
			if (rf_has(mon->race->flags, RF_UNIQUE))
				uniq_ood[lvl] += addval;
	}


	/* Is it deadly? */
	if (mon->race->level > player->depth + 10){

		mon_deadly[lvl] += addval;

		/* Is it a unique? */
		if (rf_has(mon->race->flags, RF_UNIQUE))
			uniq_deadly[lvl] += addval;
	}

	/* Generate treasure */
	monster_death_stats(i);

	/* remove the monster */
	delete_monster_idx(i);

	/* success */
	return true;
}


/**
 * Print heading infor for the file
 */
static void print_heading(void)
{
	/* PRINT INFO STUFF */
	file_putf(stats_log," This is a Monte Carlo simulation, results are arranged by level \n");
	file_putf(stats_log," Monsters:  OOD means between 1 and 10 levels deep, deadly is more than \n");
	file_putf(stats_log,"            10 levels deep \n");
	file_putf(stats_log," Artifacts: info on artifact location (vault, floor, etc) \n");
	file_putf(stats_log,"		     do not include special artifacts, only weapons and armor \n");
	file_putf(stats_log," Ego items: info on ego location (vault, floor, etc) \n");
	file_putf(stats_log," Weapons  : Big dice weapons are either BoC, SoS, or Mod.  Uber \n");
	file_putf(stats_log,"            weapons, are one of the above with xblows or slay evil\n");
	file_putf(stats_log," Launchers: xtra shots and xtra might are only logged for x3 or\n");
	file_putf(stats_log,"            better.  Very good has +to hit or + to dam > 15\n");
	file_putf(stats_log," Armor:     Low resist armor may have more than one basic resist (acid, \n");
	file_putf(stats_log,"		     elec, fire, cold) but not all. \n");
	file_putf(stats_log," Pills:   Aug counts as 5 pills, *enlight* as 2.  Healing pills are \n");
	file_putf(stats_log,"			 only *Healing* and Life\n");
	file_putf(stats_log," Cards:   Endgame cards include *Dest*, Rune, MBan and Ban \n");
	file_putf(stats_log,"    		 *Acq* counts as two Acq cards");
	file_putf(stats_log," Gadgets: 	 Utility gadgets: d-obj, d-stairs, d-traps, light, illum \n");
	file_putf(stats_log,"    		 Endgame gadgets: Speed, Healing \n");
	file_putf(stats_log," Devices: 	 Kill devices: dispel evil, power, holiness. \n");
	file_putf(stats_log,"    		 Power devices: healing, magi, banishment \n");
}

/**
 * Print all the stats for each level
 */
static void print_stats(int lvl)
{

	int i;
	
	/* check bounds on lvl */
	if ((lvl < 0) || (lvl > 100)) return;

	/* print level heading */
	file_putf(stats_log,"\n");
	file_putf(stats_log,"******** LEVEL %d , %d tries********* \n",lvl, tries);
	file_putf(stats_log,"\n");

	/* print monster heading */
	file_putf(stats_log," MONSTER INFO \n");
	file_putf(stats_log," Total monsters: %f OOD: %f Deadly: %f \n",
				mon_total[lvl], mon_ood[lvl], mon_deadly[lvl]);
	file_putf(stats_log," Unique monsters: %f OOD: %f Deadly: %f \n",
				uniq_total[lvl], uniq_ood[lvl], uniq_deadly[lvl]);
	/* print artifact heading */

	file_putf(stats_log,"\n EGO-ITEM INFO \n");

	/* basic ego info */
	file_putf(stats_log,"Total egos: %f  Shallow: %f  Average: %f  Ood: %f \n",
		ego_total[lvl], ego_shal[lvl],ego_ave[lvl],ego_ood[lvl]);

	/* more advanced info */
	file_putf(stats_log,"From vaults: %f  From floor (no vault): %f \n",
		ego_vault[lvl],ego_floor[lvl]);
	file_putf(stats_log,"Uniques: %f  Monsters: %f  Vault denizens: %f \n",
		ego_uniq[lvl], ego_mon[lvl], ego_mon_vault[lvl]);


	file_putf(stats_log,"\n ARTIFACT INFO \n");

	/* basic artifact info */
	file_putf(stats_log,"Total artifacts: %f  Special artifacts: %f  Weapons/armor: %f \n",
		art_total[lvl], art_spec[lvl], art_norm[lvl]);

	/* artifact depth info */
	file_putf(stats_log,"Shallow: %f  Average: %f  Ood: %f \n",
		art_shal[lvl],art_ave[lvl],art_ood[lvl]);
		
	/* more advanced info */
	file_putf(stats_log,"From vaults: %f  From floor (no vault): %f \n",
		art_vault[lvl],art_floor[lvl]);
	file_putf(stats_log,"Uniques: %f  Monsters: %f  Vault denizens: %f \n",
		art_uniq[lvl], art_mon[lvl], art_mon_vault[lvl]);

		
	for (i=ST_BEGIN; i<ST_END; i++){	
		file_putf(stats_log, "%s%f From Monsters: %f In Vaults: %f \n",	stat_message[i].name, stat_all[i][0][lvl], stat_all[i][1][lvl], stat_all[i][2][lvl]);
	}	


}

/**
 *Compute and print the mean and standard deviation for an array of known size
 */
static void mean_and_stdv(int array[TRIES_SIZE])
{
	int k, maxiter;
	double tot = 0, mean, stdev, temp = 0;

	/* Get the maximum iteration value */
	maxiter = MIN(tries, TRIES_SIZE); 

	/* Sum the array */
	for (k = 0; k < maxiter; k++)
		tot += array[k];

	/* Compute the mean */
	mean = tot / maxiter;

	/* Sum up the squares */
	for (k = 0; k < maxiter; k++) temp += (array[k] - mean) * (array[k] - mean);

	/* Compute standard dev */
	stdev = sqrt(temp / tries);

	/* Print to file */
	file_putf(stats_log," mean: %f  std-dev: %f \n",mean,stdev);

}

/**
 * Calculated the probability of finding an item by a specific level,
 * and print it to the output file
 */

static void prob_of_find(double stat[MAX_LVL])
{
	static int lvl, tmpcount;
	double find = 0.0, tmpfind = 0.0;

	/* Skip town level */
	for (lvl = 1; lvl < MAX_LVL ; lvl++) {

		/* Calculate the probability of not finding the stat */
		tmpfind=(1 - stat[lvl]);

		/* Maximum probability is 98% */
		if (tmpfind < 0.02) tmpfind = 0.02;

		/* Multiply probabilities of not finding */
		if (find <= 0) find = tmpfind; else find *= tmpfind;

		/* Increase count to 5 */
		tmpcount++;

		/* Print output every 5 levels */
		if (tmpcount == 5) {

			/* print it */
			file_putf(stats_log,"%f \t",1-find);

			/* reset temp counter */
			tmpcount=0;
		}
	}

	/* Put a new line in prep of next entry */
	file_putf(stats_log,"\n"); 
 }

#if 0
/**
 * Left this function unlinked for now
 */
static double total(double stat[MAX_LVL])
{
	int k;
	double out = 0;

	for (k = 0; k < MAX_LVL; k++)
		out += stat[k];

	return out;
} 
#endif

/**
 * Post process select items
 */
static void post_process_stats(void)
{
	double arttot;
	int i,k;

	/* Output a title */
	file_putf(stats_log,"\n");
	file_putf(stats_log,"***** POST PROCESSING *****\n");
	file_putf(stats_log,"\n");
	file_putf(stats_log,"Item \t5\t\t\t10\t\t\t15\t\t\t20\t\t\t25\t\t\t");
	file_putf(stats_log,"30\t\t\t35\t\t\t40\t\t\t45\t\t\t50\t\t\t");
	file_putf(stats_log,"55\t\t\t60\t\t\t65\t\t\t70\t\t\t75\t\t\t");
	file_putf(stats_log,"80\t\t\t85\t\t\t90\t\t\t95\t\t\t100\n");
	
	for (i = 1; i < ST_FF_END; i++) {
			file_putf(stats_log, stat_ff_message[i].name);
			prob_of_find(stat_all[stat_ff_message[i].st][0]);
			mean_and_stdv(stat_ff_all[i]);
	}

	/* Print artifact total */
	arttot = 0;

	for (k = 0; k < MAX_LVL; k++)
		arttot += art_total[k];

	file_putf(stats_log,"\n");
	file_putf(stats_log,"Total number of artifacts found %f \n",arttot);
	mean_and_stdv(art_it);

	/* Temporary stuff goes here */
}



/**
 * Scans the dungeon for objects
 */
static void scan_for_objects(void)
{ 
	int y, x;

	for (y = 1; y < cave->height - 1; y++) {
		for (x = 1; x < cave->width - 1; x++) {
			struct loc grid = loc(x, y);
			struct object *obj;

			while ((obj = square_object(cave, grid))) {
				/* Get data on the object */
				get_obj_data(obj, y, x, false, false);

				/* Delete the object */
				square_delete_object(cave, grid, obj, false, false);
			}
		}
	}
}

/**
 * This will scan the dungeon for monsters and then kill each
 * and every last one.
 */
static void scan_for_monsters(void)
{ 
	int i;

	/* Go through the monster list */
	for (i = 1; i < cave_monster_max(cave); i++) {
		struct monster *mon = cave_monster(cave, i);

		/* Skip dead monsters */
		if (!mon->race) continue;

		stats_monster(mon, i);
	}
}

/**
 * This is the entry point for generation statistics.
 */
static void stats_collect_level(void)
{
	/* Make a dungeon */
	prepare_next_level(&cave, player);

	/* Scan for objects, these are floor objects */
	scan_for_objects();

	/* Get stats (and kill) all non-unique monsters */
	scan_for_monsters();

}

/**
 * This code will go through the artifact list and make each artifact
 * uncreated so that our sim player can find them again!
 */
static void uncreate_artifacts(void)
{
	int i;

	/* Loop through artifacts */
	for (i = 0; z_info && i < z_info->a_max; i++) {
		struct artifact *art = &a_info[i];

		/* Uncreate */
		art->created = false;
	}
}

/**
 * This will revive all the uniques so the sim player
 * can kill them again.
 */
static void revive_uniques(void)
{
	int i;

	for (i = 1; i < z_info->r_max - 1; i++) {
		/* Get the monster info */
		struct monster_race *race = &r_info[i];

		/* Revive the unique monster */
		if (rf_has(race->flags, RF_UNIQUE)) race->max_num = 1;
	}
}

/* Generate non-special artifacts at each level 1-100 (by steps).
 * Record the number of each generated.
 */
static void artifact_stats(void)
{
	static const byte levels[] = {
		1, 2, 3, 4, 5, 6,
		8, 10, 12, 14, 16, 18, 20,
		25, 30, 35, 40, 45, 50,
		60, 70, 80, 90, 98, 127
	};
	double *prob[sizeof(levels)];
	struct object *obj;
	int *count = mem_zalloc(sizeof(int) * z_info->a_max * sizeof(levels));
	int *artifacts = mem_zalloc(sizeof(int) * sizeof(levels));
	int *objects = mem_zalloc(sizeof(int) * sizeof(levels));
	int depth = player->depth;
	for(size_t i=0;i<sizeof(levels);i++) {
		int lev = levels[i];
		player->depth = lev;
		msg("Level %d...", lev);
		/* Show the level to check on status */
		do_cmd_redraw();
		int *levcount = count + (z_info->a_max * i);
		do {
			objects[i]++;
			/* Make a non-artifact object */
			obj = make_artifact(lev, 0);
			bool ok = (obj != NULL);
			if (ok) {
				obj->artifact->created = false;
				artifacts[i]++;
				int a_idx = obj->artifact - a_info;
				levcount[a_idx]++;
				object_delete(&obj);
			}
		} while (artifacts[i] < tries);
		prob[i] = mem_alloc(sizeof(double) * z_info->a_max);
		memcpy(prob[i], wiz_stats_prob, sizeof(double) * z_info->a_max);
		double sum = 0.0;
		/* Normalize */
		for(int j=0;j<z_info->a_max;j++)
			sum += prob[i][j];
		for(int j=0;j<z_info->a_max;j++)
			prob[i][j] = (prob[i][j] * 100.0) / sum;
		fprintf(stderr,"Level %d, %d arts, %d objs\n", lev, artifacts[i], objects[i]);
	}
	char buf[1024];
	char num[32];
	strcpy(buf, "            Artifact Name            ");
	for(size_t l=0;l<sizeof(levels);l++) {
		strcat(buf+31,"Level ");
		sprintf(num, "%d%s  ", levels[l], levels[l] < 10 ? " " : "" );
		strcat(buf, num);
	}
	strcat(buf, "\n");
	file_putf(stats_log, buf);
	for(size_t a=1;a<z_info->a_max;a++) {
		memset(buf, ' ', 32);
		strcpy(buf + (30 - strlen(a_info[a].name)), a_info[a].name);
		for(int i=0;i<30;i++)
			buf[i] = ((buf[i] >= ' ') && (buf[i] <= 'z')) ? buf[i] : '-';
		for(size_t l=0;l<sizeof(levels);l++) {
			int *levcount = count + (z_info->a_max * l);
			strnfmt(num, sizeof(num), "%9d ", levcount[a]);
			strcat(buf+30, num);
		}
		strcat(buf, "\n");
		file_putf(stats_log, buf);

		memset(buf, ' ', 32);
		buf[32] = 0;
		for(size_t l=0;l<sizeof(levels);l++) {
			strnfmt(num, sizeof(num), (prob[l][a] < 10.0) ? "%.07lf " : "%.06lf ", prob[l][a]);
			strcat(buf+30, num);
		}
		strcat(buf, "\n");
		file_putf(stats_log, buf);

	}
	player->depth = depth;
}

/* Generate ego items at each level 1-100 (by steps).
 * Record the number of each generated.
 */
static void ego_stats(void)
{
	static const byte levels[] = {
		1, 2, 3, 4, 5, 6,
		8, 10, 12, 14, 16, 18, 20,
		25, 30, 35, 40, 45, 50,
		60, 70, 80, 90, 98, 127
	};
	/*static const byte levels[] = {
		1, 2, 3, 4, 5, 6,
		7, 8, 9, 10, 11, 12,
		14, 16, 18, 20, 22, 24,
		26, 28, 30, 32, 34, 36,
		38, 40, 42, 45, 50, 55,
		60, 65, 70, 75, 80, 85,
		90, 95, 98, 100, 110, 127
	};*/
	double *prob[sizeof(levels)];
	struct object *obj;
	int *count = mem_zalloc(sizeof(int) * z_info->e_max * sizeof(levels));
	int *egos = mem_zalloc(sizeof(int) * sizeof(levels));
	int *objects = mem_zalloc(sizeof(int) * sizeof(levels));
	int depth = player->depth;
	for(size_t i=0;i<sizeof(levels);i++) {
		int lev = levels[i];
		player->depth = lev;
		msg("Level %d...", lev);
		/* Show the level to check on status */
		do_cmd_redraw();
		int *levcount = count + (z_info->e_max * i);
		do {
			s32b value;
			objects[i]++;
			/* Make a (normal) object */
			obj = make_object(cave, lev, false, false, false, &value, 0);
			bool ok = (obj && obj->ego);
			if (ok) {
				egos[i]++;
				int e_idx = obj->ego - e_info;
				levcount[e_idx]++;
			}
			if (obj)
				object_delete(&obj);
		} while (egos[i] < tries);
		prob[i] = mem_alloc(sizeof(double) * z_info->e_max);
		
		// no theoreticals - yet
		//memcpy(prob[i], wiz_stats_prob, sizeof(double) * z_info->e_max);
		
		// so just convert to %
		for(int j=0;j<z_info->e_max;j++) {
			prob[i][j] = levcount[j];
		}
		
		double sum = 0.0;
		/* Normalize */
		for(int j=0;j<z_info->e_max;j++)
			sum += prob[i][j];
		for(int j=0;j<z_info->e_max;j++)
			prob[i][j] = (prob[i][j] * 100.0) / sum;
		fprintf(stderr,"Level %d, %d egos, %d objs\n", lev, egos[i], objects[i]);
	}
	char buf[1024];
	char num[32];
	strcpy(buf, "                 Ego Name       ");
	for(size_t l=0;l<sizeof(levels);l++) {
		strcat(buf+31,"Level ");
		sprintf(num, "%d%s  ", levels[l], levels[l] < 10 ? " " : "" );
		strcat(buf, num);
	}
	strcat(buf, "\n");
	file_putf(stats_log, buf);
	for(size_t a=1;a<(size_t)(z_info->e_max - 1);a++) {
		memset(buf, ' ', 32);
		strcpy(buf + (30 - strlen(e_info[a].name)), e_info[a].name);
		for(int i=0;i<30;i++)
			buf[i] = ((buf[i] >= ' ') && (buf[i] <= 'z')) ? buf[i] : '-';
		for(size_t l=0;l<sizeof(levels);l++) {
			int *levcount = count + (z_info->e_max * l);
			strnfmt(num, sizeof(num), "%9d ", levcount[a]);
			strcat(buf+30, num);
		}
		strcat(buf, "\n");
		file_putf(stats_log, buf);

		memset(buf, ' ', 32);
		buf[32] = 0;
		for(size_t l=0;l<sizeof(levels);l++) {
			strnfmt(num, sizeof(num), (prob[l][a] < 10.0) ? "%.07lf " : "%.06lf ", prob[l][a]);
			strcat(buf+30, num);
		}
		strcat(buf, "\n");
		file_putf(stats_log, buf);

	}
	player->depth = depth;
}

/**
 * This function loops through the level and does N iterations of
 * the stat calling function, assuming diving style.
 */ 
static void diving_stats(void)
{
	int depth;

	/* Iterate through levels */
	for (depth = 0; depth < MAX_LVL; depth += 5) {
		player->depth = depth;
		if (player->depth == 0) player->depth = 1;

		/* Do many iterations of each level */
		for (iter = 0; iter < tries; iter++)
		     stats_collect_level();

		/* Print the output to the file */
		print_stats(depth);

		/* Show the level to check on status */
		do_cmd_redraw();
	}
}

/**
 * This function loops through the level and does N iterations of
 * the stat calling function, assuming clearing style.
 */ 
static void clearing_stats(void)
{
	int depth;

	/* Do many iterations of the game */
	for (iter = 0; iter < tries; iter++) {
		/* Move all artifacts to uncreated */
		uncreate_artifacts();

		/* Move all uniques to alive */
		revive_uniques();

		/* Do randart regen */
		if ((regen) && (iter<tries)) {
			/* Get seed */
			int seed_randart = randint0(0x10000000);

			/* Restore the standard artifacts */
			cleanup_parser(&randart_parser);
			deactivate_randart_file();
			run_parser(&artifact_parser);

			/* regen randarts */
			do_randart(seed_randart, false);
		}

		/* Do game iterations */
		for (depth = 1 ; depth < MAX_LVL; depth++) {
			/* Debug 
			msg_format("Attempting level %d",depth); */

			/* Move player to that depth */
			player->depth = depth;

			/* Get stats */
			stats_collect_level();

			/* Debug
			msg_format("Finished level %d,depth"); */
		}

		msg("Iteration %d complete",iter);
	}

	/* Restore original artifacts */
	if (regen) {
		cleanup_parser(&randart_parser);
		if (OPT(player, birth_randarts)) {
			activate_randart_file();
			run_parser(&randart_parser);
			deactivate_randart_file();
		} else {
			run_parser(&artifact_parser);
		}
	}

	/* Print to file */
	for (depth = 0 ;depth < MAX_LVL; depth++)
		print_stats(depth);

	/* Post processing */
	post_process_stats();

	/* Display the current level */
	do_cmd_redraw(); 
}

/**
 * Prompt the user for sim type and number of sims for a stats run
 */
static int stats_prompt(void)
{
	static int temp,simtype = 1;
	static char tmp_val[100];
	static char prompt[80];

	/* This is the prompt for no. of tries*/
	strnfmt(prompt, sizeof(prompt), "Num of simulations: ");

	/* This is the default value (50) */
	strnfmt(tmp_val, sizeof(tmp_val), "%d", tries);

	/* Ask for the input */
	if (!get_string(prompt, tmp_val, 7)) return 0;

	/* Get the new value */
	temp = atoi(tmp_val);

	/* Convert */
	if (temp < 1) temp = 1;

	/* Save */
	tries = temp;

	/* Set 'value to add' for arrays */
	addval = 1.0 / tries;

	/* Get info on what type of run to do */
	strnfmt(prompt, sizeof(prompt), "Type of Sim: Diving (1), Clearing (2), Artifacts (3), Egos (4) ");

	/* Set default */
	strnfmt(tmp_val, sizeof(tmp_val), "%d", simtype);

	/* Get the input */
	if (!get_string(prompt, tmp_val, 4)) return 0;

	temp = atoi(tmp_val);

	/* Make sure that the type is good */
	if ((temp >= 1) && (temp <= 4))
		simtype = temp;
	else
		return 0;

	/* For clearing sim, check for randart regen */
	if (temp == 2) {
		/* Prompt */
		strnfmt(prompt, sizeof(prompt), "Regen randarts? (warning SLOW)");

		regen = get_check(prompt) ? true : false;
	}

	return simtype;
}

/**
 * This is the function called from wiz-debug.c.
 */
void stats_collect(void)
{
	static int simtype;
	static bool auto_flag;
	bool artifacts = false;
	bool egos = false;
	char buf[1024];

	/* Prompt the user for sim params */
	simtype = stats_prompt();

	/* Make sure the results are good! */
	if (!((simtype >= 1) && (simtype <= 4)))
		return; 

	/* Are we in diving or clearing mode */
	switch(simtype) {
		case 1:
			clearing = false;
			break;
		case 2:
			clearing = true;
			break;
		case 3:
			artifacts = true;
			break;
		case 4:
			egos = true;
			break;
	}

	/* Open log file */
	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "stats.log");
	stats_log = file_open(buf, MODE_WRITE, FTYPE_TEXT);

	/* Logging didn't work */
	if (!stats_log) {
		msg("Error - can't open stats.log for writing.");
		exit(1);
	}

	/* Turn on auto-more.  This will clear prompts for items
	 * that drop under the player, or that can't fit on the 
	 * floor due to too many items.  This is a very small amount
	 * of items, even on deeper levels, so it's not worth worrying
	 * too much about.
	 */
	 auto_flag = false;
	 
	 if (!OPT(player, auto_more)) {
		/* Remember that we turned off auto_more */
		auto_flag = true;

		/* Turn on auto-more */
		option_set(option_name(OPT_auto_more),true);
	}

	/* Print heading for the file */
	print_heading();

	/* Make sure all stats are 0 */
	init_stat_vals();

	if (artifacts) {
		artifact_stats();
	} else if (egos) {
		ego_stats();
	} else {
		/* Select diving option */
		if (!clearing) diving_stats();

		/* Select clearing option */
		if (clearing) clearing_stats();
	}

	/* Turn auto-more back off */
	if (auto_flag) option_set(option_name(OPT_auto_more), false);

	/* Close log file */
	if (!file_close(stats_log)) {
		msg("Error - can't close stats.log file.");
		exit(1);
	}
}

#define DIST_MAX 10000

void calc_cave_distances(int **cave_dist)
{
	int dist;

	/* Squares with distance from player of n - 1 */
	struct loc *ogrids;
	int n_old, cap_old;

	/* Squares with distance from player of n */
	struct loc *ngrids;
	int n_new, cap_new;

	/*
	 * The perimeter of the cave should overestimate the space needed so
	 * there's fewer reallocations within the loop.
	 */
	cap_old = 2 * (cave->width + cave->height - 1);
	ogrids = mem_alloc(cap_old * sizeof(*ogrids));
	cap_new = cap_old;
	ngrids = mem_alloc(cap_new * sizeof(*ngrids));

	/* The player's location is the first one to test. */
	ogrids[0] = player->grid;
	n_old = 1;

	/* Distance from player starts at 0 */
	dist = 0;

	/* Assign the distance value to the first square (player) */
	cave_dist[ogrids[0].y][ogrids[0].x] = dist;

	do {
		int i, n_tmp;
		struct loc *gtmp;

		n_new = 0;
		dist++;

		/* Loop over all visited squares of the previous iteration */
		for (i = 0; i < n_old; i++){
			int d;
			/* Get the square we want to look at */
			int oy = ogrids[i].y;
			int ox = ogrids[i].x;

			/* debug
			msg("x: %d y: %d dist: %d %d ",ox,oy,dist-1,i); */

			/* Get all adjacent squares */
			for (d = 0; d < 8; d++) {
				/* Adjacent square location */
				int ty = oy + ddy_ddd[d];
				int tx = ox + ddx_ddd[d];

				if (!(square_in_bounds_fully(cave, loc(tx, ty)))) continue;

				/* Have we been here before? */
				if (cave_dist[ty][tx] >= 0) continue;

				/*
				 * Impassable terrain which isn't a door or
				 * rubble blocks progress.
				 */
				if (!square_ispassable(cave, loc(tx, ty)) &&
					!square_isdoor(cave, loc(tx, ty)) &&
					!square_isrubble(cave, loc(tx, ty))) continue;

				/* Add the new location */
				if (n_new == cap_new - 1) {
					cap_new *= 2;
					ngrids = mem_realloc(ngrids,
						cap_new * sizeof(ngrids));
				}
				ngrids[n_new].y = ty;
				ngrids[n_new].x = tx;
				++n_new;

				/* Assign the distance to that spot */
				cave_dist[ty][tx] = dist;

				/* debug
				msg("x: %d y: %d dist: %d ",tx,ty,dist); */
			}
		}

		/* Swap the lists; do not need to preserve n_old. */
		gtmp = ogrids;
		ogrids = ngrids;
		ngrids = gtmp;
		n_tmp = cap_old;
		cap_old = cap_new;
		cap_new = n_tmp;
		n_old = n_new;
	} while (n_old > 0 && dist < DIST_MAX);

	mem_free(ngrids);
	mem_free(ogrids);
}

void pit_stats(void)
{
	int tries = 1000;
	int depth = 0;
	int hist[z_info->pit_max];
	int j, p;
	int type = 1;

	char tmp_val[100];

	/* Initialize hist */
	for (p = 0; p < z_info->pit_max; p++)
		hist[p] = 0;

	/* Format default value */
	strnfmt(tmp_val, sizeof(tmp_val), "%d", tries);

	/* Ask for the input - take the first 7 characters*/
	if (!get_string("Num of simulations: ", tmp_val, 7)) return;

	/* Get the new value */
	tries = atoi(tmp_val);
	if (tries < 1) tries = 1;

	/* Format second default value */
	strnfmt(tmp_val, sizeof(tmp_val), "%d", type);

	/* Ask for the input - take the first 7 characters*/
	if (!get_string("Pit type: ", tmp_val, 7)) return;

	/* get the new value */
	type = atoi(tmp_val);
	if (type < 1) type = 1;

	/* Format second default value */
	strnfmt(tmp_val, sizeof(tmp_val), "%d", player->depth);

	/* Ask for the input - take the first 7 characters*/
	if (!get_string("Depth: ", tmp_val, 7)) return;

	/* get the new value */
	depth = atoi(tmp_val);
	if (depth < 1) depth = 1;

	for (j = 0; j < tries; j++) {
		int i;
		int pit_idx = 0;
		int pit_dist = 999;

		for (i = 0; i < z_info->pit_max; i++) {
			int offset, dist;
			struct pit_profile *pit = &pit_info[i];

			if (!pit->name || pit->room_type != type) continue;

			offset = Rand_normal(pit->ave, 10);
			dist = ABS(offset - depth);

			if (dist < pit_dist && one_in_(pit->rarity)) {
				pit_idx = i;
				pit_dist = dist;
			}
		}

		hist[pit_idx]++;
	}

	for (p = 0; p < z_info->pit_max; p++) {
		struct pit_profile *pit = &pit_info[p];
		if (pit->name)
			msg("Type: %s, Number: %d.", pit->name, hist[p]);
	}

	return;
}


/**
 * Gather whether the dungeon has disconnects in it and whether the player
 * is disconnected from the stairs
 */
void disconnect_stats(void)
{
	int i, y, x;

	int **cave_dist;

	bool has_dsc, has_dsc_from_stairs;

	static int temp;
	static char tmp_val[100];
	static char prompt[50];

	long dsc_area = 0, dsc_from_stairs = 0;
	bool stop_for_dis;
	char path[1024];
	ang_file *disfile;

	/* This is the prompt for no. of tries */
	strnfmt(prompt, sizeof(prompt), "Num of simulations: ");

	/* This is the default value (50) */
	strnfmt(tmp_val, sizeof(tmp_val), "%d", tries);

	/* Ask for the input */
	if (!get_string(prompt,tmp_val,7)) return;

	/* Get the new value */
	temp = atoi(tmp_val);

	/* Try at least once */
	if (temp < 1)
		temp = 1;

	/* Save */
	tries = temp;

	stop_for_dis = get_check("Stop if disconnected level found? ");

	path_build(path, sizeof(path), ANGBAND_DIR_USER, "disconnect.html");
	disfile = file_open(path, MODE_WRITE, FTYPE_TEXT);
	if (disfile) {
		dump_level_header(disfile, "Disconnected Levels");
	}

	for (i = 1; i <= tries; i++) {
		/* Assume no disconnected areas */
		has_dsc = false;

		/* Assume you can't get to stairs */
		has_dsc_from_stairs = true;

		/* Make a new cave */
		prepare_next_level(&cave, player);

		/* Allocate the distance array */
		cave_dist = mem_zalloc(cave->height * sizeof(int*));
		for (y = 0; y < cave->height; y++)
			cave_dist[y] = mem_zalloc(cave->width * sizeof(int));

		/* Set all cave spots to inaccessible */
		for (y = 0; y < cave->height; y++)
			for (x = 1; x < cave->width; x++)
				cave_dist[y][x] = -1;

		/* Fill the distance array with the correct distances */
		calc_cave_distances(cave_dist);

		/* Cycle through the dungeon */
		for (y = 1; y < cave->height - 1; y++) {
			for (x = 1; x < cave->width - 1; x++) {
				struct loc grid = loc(x, y);

				/*
				 * Don't care about impassable terrain that's
				 * not a closed or secret door or impassable
				 * rubble.
				 */
				if (!square_ispassable(cave, grid) &&
					!square_isdoor(cave, grid) &&
					!square_isrubble(cave, grid)) continue;

				/* Can we get there? */
				if (cave_dist[y][x] >= 0) {

					/* Is it a  down stairs? */
					if (square_isdownstairs(cave, grid)) {

						has_dsc_from_stairs = false;

						/* debug
						msg("dist to stairs: %d",cave_dist[y][x]); */
					}
					continue;
				}

				/* Ignore vaults as they are often disconnected */
				if (square_isvault(cave, grid)) continue;

				/* We have a disconnected area */
				has_dsc = true;
			}
		}

		if (has_dsc_from_stairs) dsc_from_stairs++;

		if (has_dsc) dsc_area++;

		if (has_dsc || has_dsc_from_stairs) {
			if (disfile) {
				dump_level_body(disfile, "Disconnected Level",
					cave, cave_dist);
			}
			if (stop_for_dis) i = tries;
		}

		/* Free arrays */
		for (y = 0; y < cave->height; y++)
			mem_free(cave_dist[y]);
		mem_free(cave_dist);
	}

	msg("Total levels with disconnected areas: %ld",dsc_area);
	msg("Total levels isolated from stairs: %ld",dsc_from_stairs);
	if (disfile) {
		dump_level_footer(disfile);
		if (file_close(disfile)) {
			msg("Map is in disconnect.html.");
		}
	}

	/* Redraw the level */
	do_cmd_redraw();
}


#else /* USE_STATS */

void stats_collect(void)
{
	msg("Statistics generation not turned on in this build.");
}

void disconnect_stats(void)
{
	msg("Statistics generation not turned on in this build.");
}

void pit_stats(void)
{
	msg("Statistics generation not turned on in this build.");
}
#endif /* USE_STATS */
