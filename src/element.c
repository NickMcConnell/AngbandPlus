/* File: element.c */

/* Purpose: Elementalist code */
/* Modified from mind.c */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

#define MAX_ELEMENT_POWERS 17

typedef struct element_power element_power;
struct element_power
{
	int     min_lev;
	int     mana_cost;
	int     fail;
	int     elem;
	const char *name;
};

static cptr element_title[MAX_ELEMENTS] =
{
#ifdef JP
	"±ê",
	"É¹",
	"¶õ",
	"³¤",
	"°Ç",
	"º®ÆÙ",
	"ÃÏ",
	"áïµ¤",
#else
	"Fire",
	"Ice",
	"Sky",
	"Sea",
	"Darkness",
	"Chaos",
	"Earth",
	"Death",
#endif
};

static int element_type[MAX_ELEMENTS][3] =
{
	{GF_FIRE, GF_PLASMA, GF_HOLY_FIRE},
	{GF_COLD, GF_INERTIA, GF_TIME},
	{GF_ELEC, GF_LITE, GF_MANA},
	{GF_ACID, GF_WATER, GF_DISINTEGRATE},
	{GF_DARK, GF_NETHER, GF_HELL_FIRE},
	{GF_CONFUSION, GF_NEXUS, GF_CHAOS}, 
	{GF_SHARDS, GF_FORCE, GF_METEOR},
	{GF_POIS, GF_OLD_DRAIN, GF_DISENCHANT},
#if 0
	{GF_GRAVITY, GF_GRAVITY, GF_SOUND},
	{GF_ROCKET, GF_NUKE, XXX},
#endif
};

static cptr element_name[MAX_ELEMENTS][3] =
{
#ifdef JP
	{"²Ð±ê", "¥×¥é¥º¥Þ", "À»µ¤"},
	{"Îäµ¤", "ÃÙÆß", "»þ´Ö"},
	{"ÅÅ·â", "¸÷", "ËâÎÏ"},
	{"»À", "¿å", "Ê¬²ò"},
	{"°Å¹õ", "ÃÏ¹ö", "¼Ùµ¤"},
	{"º®Íð", "°ø²Ìº®Íð", "¥«¥ª¥¹"},
	{"ÇËÊÒ", "¥Õ¥©¡¼¥¹", "ð¨ÀÐ"},
	{"ÆÇ", "µÛ·ì", "Îô²½"},
#else
	{"Fire", "Plasma", "Holy"},
	{"Ice", "Inertia", "Time"},
	{"Lightning", "Light", "Mana"},
	{"Acid", "Water", "Disin"},
	{"Dark", "Nether", "Evil"},
	{"Conf", "Nexus", "Chaos"},
	{"Shard", "Force", "Meteo"},
	{"Poison", "Drain", "Disenchant"},
#endif
};

struct element_tips_data{
	const char* tips; 
	const char* element[MAX_ELEMENTS];  
};

struct element_tips_data element_tips[MAX_ELEMENT_POWERS] =
{
	/* format , level */
#ifdef JP
	{ "¼å¤¤%s¤ÎÌð¤òÊü¤Ä¡£",
	  {"²Ð±ê","Îäµ¤","ÅÅ·â","»À", "°Å¹õ","º®ÍðÂ°À­","ÇËÊÒ","ÆÇ"} },
	{ "%s¶á¤¯¤ÎÁ´¤Æ¤Î¥â¥ó¥¹¥¿¡¼¤ò´¶ÃÎ¤¹¤ë¡£",
	  {"","","","", "","","",""} },
	{ "%s¥¢¥¤¥Æ¥à¤ÎÊ·°Ïµ¤¤òÃÎ¤ë¡£",
	  {"","","","", "","","",""} },
	{ "%s²ø²æ¤ÈÂÎÎÏ¤ò¾¯¤·²óÉü¤µ¤»¤ë¡£",
	  {"","","","", "","","",""} },
	{ "%s¤ÎÌð¤òÊü¤Ä¡£",
	  {"¥×¥é¥º¥Þ","ÃÙÆß","Á®¸÷","¿å", "ÃÏ¹ö","°ø²Ìº®Íð","¥Õ¥©¡¼¥¹","µÛ·ì"} },
	{ "¶á¤¯¤ÎËâË¡¤Î¥¢¥¤¥Æ¥à¤ò´¶ÃÎ¤¹¤ë¡£",
	  {NULL,NULL,NULL,NULL, NULL,NULL,NULL,NULL} },
	{ "¹â°ÒÎÏ¤Ç¼ÍÄø¤¬Ã»¤¤%s¤Î¥Ó¡¼¥à¤òÊü¤Ä¡£",
	  {"²Ð±ê","Îäµ¤","ÅÅ·â","»À", "°Å¹õ","º®ÍðÂ°À­","ÇËÊÒ","ÆÇ"} },
	{ "%s¤Îµå¤òÊü¤Ä¡£",
	  {"²Ð±ê","Îäµ¤","ÅÅ·â","»À", "°Å¹õ","º®Íð","ÇËÊÒ","ÆÇ"} },
	{ "%s¤Î¥Ö¥ì¥¹¤òÅÇ¤¯¡£",
	  {"¥×¥é¥º¥Þ","ÃÙÆß","Á®¸÷","¿å", "ÃÏ¹ö","°ø²Ìº®Íð","¥Õ¥©¡¼¥¹","À¸Ì¿ÎÏµÛ¼ý"} },
	{ "%sÂÑÀ­¤Î¤Ê¤¤¥â¥ó¥¹¥¿¡¼¤ò1ÂÎËõ»¦¤¹¤ë¡£",
	  {"²Ð±ê","Îäµ¤","ÅÅ·â","»À", "°Å¹õ","º®Íð","ÇËÊÒ","ÆÇ"} },
	{ "%s¤ÎÌð¤òÊü¤Ä¡£",
	  {"À»µ¤","»þ´ÖµÕÅ¾","ËâÎÏ","Ê¬²ò", "¼Ùµ¤","¥«¥ª¥¹","ÍÏ´ä","Îô²½"} },
	{ "»ë³¦Æâ¤ÎÁ´¤Æ¤ÎÅ¨¤Ë%s¥À¥á¡¼¥¸¤òÍ¿¤¨¤ë¡£",
	  {"²Ð±ê¤Ë¤è¤ë","Îäµ¤¤Ë¤è¤ë","ÅÅ·â¤Ë¤è¤ë","»À¤Ë¤è¤ë", "°Å¹õ¤Ë¤è¤ë","º®ÍðÂ°À­¤Î","ÇËÊÒ¤Ë¤è¤ë","ÆÇ¤Ë¤è¤ë"} },
	{ "%s¤Îµå¤òÊü¤Ä¡£",
	  {"¥×¥é¥º¥Þ","ÃÙÆß","Á®¸÷","¿å", "ÃÏ¹ö","°ø²Ìº®Íð","¥Õ¥©¡¼¥¹","À¸Ì¿ÎÏµÛ¼ý"} },
	{ "¥é¥ó¥À¥à¤ÊÊý¸þ¤Ë%s¤ÎÌð¤òÊü¤Ä¡£",
	  {"²Ð±ê","Îäµ¤","ÅÅ·â","»À", "°Å¹õ","º®ÍðÂ°À­","ÇËÊÒ","ÆÇ"} },
	{ "%s¤ÎµðÂç¤Êµå¤òÊü¤Ä¡£",
	  {"¥×¥é¥º¥Þ","ÃÙÆß","Á®¸÷","¿å", "ÃÏ¹ö","°ø²Ìº®Íð","¥Õ¥©¡¼¥¹","À¸Ì¿ÎÏµÛ¼ý"} },
	{ "%s¤Î¥Ö¥ì¥¹¤òÅÇ¤¯¡£",
	  {"²Ð±ê","Îäµ¤","ÅÅ·â","»À", "°Å¹õ","º®Íð","ÇËÊÒ","ÆÇ"} },
	{ "%s¤ÎµðÂç¤Êµå¤òÊü¤Ä¡£",
	  {"À»µ¤","»þ´ÖµÕÅ¾","ËâÎÏ","Ê¬²ò", "¼Ùµ¤","¥«¥ª¥¹","ð¨ÀÐ","Îô²½"} },
#else
	{ "Fire a weak bolt of %s.",
	  {"fire","ice","lightning","acid", "dark","confusion","shard","poison"} },
	{ "Detects monsters.",
	  {NULL,NULL,NULL,NULL, NULL,NULL,NULL,NULL} },
	{ "Gives feeling of an item.",
	  {NULL,NULL,NULL,NULL, NULL,NULL,NULL,NULL} },
	{ "Heals HP and wounds a little.",
	  {NULL,NULL,NULL,NULL, NULL,NULL,NULL,NULL} },
	{ "Fire a bolt of %s.",
	  {"plasma","inertia","light","water", "nether","nexus","force","drain life"} },
	{ "Detects magic devices.",
	  {NULL,NULL,NULL,NULL, NULL,NULL,NULL,NULL} },
	{ "Fire a short and strong beam of %s.",
	  {"fire","ice","lightning","acid", "dark","confusion","shard","poison"} },
	{ "Fire a ball of %s.",
	  {"fire","ice","lightning","acid", "dark","confusion","shard","poison"} },
	{ "Fire a breath of %s.",
	  {"plasma","inertia","light","water", "nether","nexus","force","drain life"} },
	{ "Erase a monster unless it resists %s.",
	  {"fire","ice","lightning","acid", "dark","confusion","shard","poison"} },
	{ "Fire a bolt of %s.",
	  {"holy fire","time","mana","disintegrate", "hell fire","chaos","meteo","disenchant"} },
	{ "Inflict all monsters with %s damage.",
	  {"fire","ice","lightning","acid", "dark","confusion","shard","poison"} },
	{ "Fire a ball of %s.",
	  {"plasma","inertia","light","water", "nether","nexus","force","drain life"} },
	{ "Fire some bolts of %s toward random direction.",
	  {"fire","ice","lightning","acid", "dark","confusion","shard","poison"} },
	{ "Fire a large ball of %s.",
	  {"plasma","inertia","light","water", "nether","nexus","force","drain life"} },
	{ "Fire a breath of %s.",
	  {"fire","ice","lightning","acid", "dark","confusion","shard","poison"} },
	{ "Fire a huge ball of %s.",
	  {"holy fire","time","mana","disintegrate", "hell fire","chaos","meteo","disenchant"} },
#endif
};

static element_power element_powers[MAX_ELEMENT_POWERS] =
{
	/* Level gained,  cost,  %fail,  type, name */
#ifdef JP
	{  1,  1,  15, 0, "%s¤ÎÌð" },
	{  2,  1,  20, 0, "¥â¥ó¥¹¥¿¡¼´¶ÃÎ" },
	{  5,  5,  50, 0, "µ¼»÷´ÕÄê" },
	{  6,  5,  35, 0, "½ý¤Î¼£Ìþ" },
	{  8,  6,  35, 1, "%s¤ÎÌð" },
	{ 10,  8,  60, 0, "ËâË¡´¶ÃÎ" },
	{ 15, 10,  45, 0, "%s¤ÎÊÜ" },
	{ 18, 15,  70, 0, "%s¤Îµå" },
	{ 21, 20,  70, 1, "%s¤Î¥Ö¥ì¥¹" },
	{ 24, 20,  75, 0, "¥â¥ó¥¹¥¿¡¼¾ÃÌÇ" },
	{ 25, 15,  60, 2, "%s¤ÎÌð" },
	{ 28, 30,  75, 0, "¸µÁÇ¤ÎÇÈÆ°" },
	{ 28, 22,  75, 1, "%s¤Îµå" },
	{ 33, 35,  75, 0, "Àºµ¤Íð¼Í" },
	{ 35, 30,  75, 1, "%s¤ÎÍò" },
	{ 42, 48,  75, 0, "%s¤Î¥Ö¥ì¥¹" },
	{ 45, 60,  80, 2, "%s¤ÎÍò" },
#else
	{  1,  1,  15, 0, "%s Bolt" },
	{  2,  1,  20, 0, "Detect Monster" },
	{  5,  5,  50, 0, "Psychometry" },
	{  6,  5,  35, 0, "Cure Wounds" },
	{  8,  6,  35, 1, "%s Bolt" },
	{ 10,  8,  60, 0, "Detect Magical Objs" },
	{ 15, 10,  45, 0, "%s Whip" },
	{ 18, 15,  50, 0, "%s Ball" },
	{ 21, 20,  75, 1, "Breath of %s" },
	{ 24, 20,  60, 0, "Annihilation" },
	{ 25, 15,  50, 2, "%s Bolt" },
	{ 28, 30,  65, 0, "Elemental Wave" },
	{ 28, 22,  75, 1, "%s Ball" },
	{ 33, 35,  75, 0, "%s Blast" },
	{ 35, 30,  75, 1, "%s Storm" },
	{ 42, 48,  75, 0, "Breath of %s" },
	{ 45, 60,  80, 2, "%s Storm" },
#endif
};


cptr element_realm_name(void)
{
	return element_title[p_ptr->realm1];
}

static s16b element_cost(int cost)
{
	int tmp_mana = cost;

	if (p_ptr->dec_mana) tmp_mana = tmp_mana * 3 / 4;
	if (tmp_mana < 1) tmp_mana = 1;

	return (tmp_mana);
}

bool elemental_genocide(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	char m_name[80];
	int type = p_ptr->realm1;

	monster_desc(m_name, m_ptr, 0);

#ifdef JP
	msg_format("%s¤¬%s¤òÊñ¤ß¹þ¤ó¤À¡£", element_name[type][0], m_name);
#else
	msg_format("The %s surrounds %s. ", element_name[type][0], m_name);
#endif

	switch(type)
	{
	case 0:
		if (r_ptr->flags3 & (RF3_IM_FIRE)) return FALSE;
		break;
	case 1:
		if (r_ptr->flags3 & (RF3_IM_COLD)) return FALSE;
		break;
	case 2:
		if (r_ptr->flags3 & (RF3_IM_ELEC)) return FALSE;
		break;
	case 3:
		if (r_ptr->flags3 & (RF3_IM_ACID)) return FALSE;
		break;
	case 4:
		if ((r_ptr->flags4 & (RF4_BR_DARK)) ||
		    (r_ptr->flags3 & RF3_ORC) ||
			(r_ptr->flags3 & RF3_HURT_LITE)) return FALSE;
		break;
	case 5:
		if ((r_ptr->flags4 & (RF4_BR_CONF)) ||
			(r_ptr->flags3 & (RF3_NO_CONF))) return FALSE;
		break;
	case 6:
		if (r_ptr->flags4 & (RF4_BR_SHAR)) return FALSE;
		break;
	case 7:
		if (r_ptr->flags3 & (RF3_IM_POIS)) return FALSE;
		break;
	}

	return TRUE;
}

static void element_info(char *p, int power)
{
	int plev = p_ptr->lev;

	strcpy(p, "");

	switch (power)
	{
#ifdef JP
	case 0:  sprintf(p, " Â»½ý:%dd%d", 3 + ((plev - 1) / 5), 4); break;
	case 3:  sprintf(p, " ²óÉü:%dd%d", 2, 8); break;
	case 4:  sprintf(p, " Â»½ý:%dd%d", 8 + ((plev - 5) / 4), 8); break;
	case 6:  sprintf(p, " Â»½ý:%d", (50 + plev * 2)); break;
	case 7:  sprintf(p, " Â»½ý:%d", 55 + plev); break;
	case 8: { int dam = p_ptr->chp / 2; sprintf(p, " Â»½ý:%d", (dam > 120) ? 120 : dam); break; }
	case 9:  sprintf(p, " ¸úÎÏ:%d", 50 + plev); break;
	case 10: sprintf(p, " Â»½ý:%dd%d", 12 + ((plev - 5) / 4), 8); break;
	case 11: sprintf(p, " Â»½ý:d%d", plev * 4); break;
	case 12: sprintf(p, " Â»½ý:%d", 75 + plev); break;
	case 13: sprintf(p, " Â»½ý:%dd%d", 6 + plev / 8, 8); break;
	case 14: sprintf(p, " Â»½ý:%d", 120 + plev * 2); break;
	case 15: sprintf(p, " Â»½ý:%d", p_ptr->chp * 2 / 3); break;
	case 16: sprintf(p, " Â»½ý:%d", 300 + plev * 5); break;
#else
	case 0:  sprintf(p, " dam %dd%d", 3 + ((plev - 1) / 5), 4); break;
	case 3:  sprintf(p, " heal %dd%d", 2, 8); break;
	case 4:  sprintf(p, " dam %dd%d", 8 + ((plev - 5) / 4), 8); break;
	case 6:  sprintf(p, " dam %d", (50 + plev * 2)); break;
	case 7:  sprintf(p, " dam %d", 55 + plev); break;
	case 8:  { int dam = p_ptr->chp / 2; sprintf(p, " dam %d", (dam > 120) ? 120 : dam); break; }
	case 9:  sprintf(p, " pow %d", 50 + plev); break;
	case 10: sprintf(p, " dam %dd%d", 12 + ((plev - 5) / 4), 8); break;
	case 11: sprintf(p, " dam d%d", plev * 4); break;
	case 12: sprintf(p, " dam %d", 75 + plev); break;
	case 13: sprintf(p, " dam %dd%d", 6 + plev / 8, 8); break;
	case 14: sprintf(p, " dam %d", 120 + plev * 2); break;
	case 15: sprintf(p, " dam %d", p_ptr->chp * 2 / 3); break;
	case 16: sprintf(p, " dam %d", 300 + plev * 5); break;
#endif
	}
}


void display_element_list(void)
{
	int             i;
	int             y = 1;
	int             x = 1;
	int             plev = p_ptr->lev;
	int             chance;
	int             mana_cost;
	element_power spell;
	char            spellname[80];
	char            comment[80];
	char            psi_desc[80];
	
	/* Display a list of spells */
	prt("", y, x);
#ifdef JP
	put_str("Ì¾Á°", y, x + 5);
	put_str("Lv   MP ¼ºÎ¨ ¸ú²Ì", y, x + 35);
#else
	put_str("Name", y, x + 5);
	put_str("Lv Mana Fail Info", y, x + 35);
#endif
	
	
	/* Dump the spells */
	for (i = 0; i < MAX_ELEMENT_POWERS; i++)
	{
		/* Access the available spell */
		spell = element_powers[i];
		if (spell.min_lev > plev) break;

		mana_cost = element_cost(spell.mana_cost);
		chance = get_spell_chance(spell.fail, spell.min_lev, mana_cost);

		/* Get info */
		element_info(comment, i);

		/* Get spell name */
		sprintf(spellname, spell.name, element_name[p_ptr->realm1][spell.elem]);

		/* Dump the spell */
		sprintf(psi_desc, "  %c) %-30s%2d %4d %3d%%%s",
			I2A(i), spellname,
			spell.min_lev, mana_cost, chance, comment);
		
		Term_putstr(x, y + i + 1, -1, TERM_WHITE, psi_desc);
	}
	return;
}


/*
 * Allow user to choose a mindcrafter power.
 *
 * If a valid spell is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 * The "prompt" should be "cast", "recite", or "study"
 * The "known" should be TRUE for cast/pray, FALSE for study
 *
 * nb: This function has a (trivial) display bug which will be obvious
 * when you run it. It's probably easy to fix but I haven't tried,
 * sorry.
 */
static int get_element_power(int *sn, bool only_browse)
{
	int             i;
	int             num = 0;
	int             y = 1;
	int             x = 20;
	int             plev = p_ptr->lev;
	int             chance;
	int             mana_cost;
	int             ask;
	char            choice;
	char            out_val[160];
	char            comment[80];
#ifdef JP
	cptr            p = "ËâË¡";
#else
	cptr            p = "magic";
#endif
	element_power spell;
	bool            flag, redraw;

	/* Assume cancelled */
	*sn = (-1);

#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (element_powers[*sn].min_lev <= plev)
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT -- TNB */

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	for (i = 0; i < MAX_ELEMENT_POWERS; i++)
	{
		if (element_powers[i].min_lev <= plev)
		{
			num++;
		}
	}

	/* Build a prompt (accept all spells) */
	if (only_browse)
	{
#ifdef JP
		(void)strnfmt(out_val, 78, "(%^s %c-%c, '*'¤Ç°ìÍ÷, ESC) ¤É¤Î%s¤Ë¤Ä¤¤¤ÆÃÎ¤ê¤Þ¤¹¤«¡©",
#else
		(void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) Use which %s? ",
#endif
			      p, I2A(0), I2A(num - 1), p);
	}
	else
	{
#ifdef JP
		(void)strnfmt(out_val, 78, "(%^s %c-%c, '*'¤Ç°ìÍ÷, ESC) ¤É¤Î%s¤ò»È¤¤¤Þ¤¹¤«¡©",
#else
		(void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) Use which %s? ",
#endif
			  p, I2A(0), I2A(num - 1), p);
	}

	/* Get a spell from the user */
	choice = always_show_list ? ESCAPE : 1;
	while (!flag)
	{
		if(choice == ESCAPE) choice = ' ';
		else if( !get_com(out_val, &choice) )break; 

		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				char spellname[80];
				char psi_desc[80];

				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				screen_save();

				/* Display a list of spells */
				prt("", y, x);
#ifdef JP
				put_str("Ì¾Á°", y, x + 5);
				put_str("Lv   MP ¼ºÎ¨ ¸ú²Ì", y, x + 35);
#else
				put_str("Name", y, x + 5);
				put_str("Lv Mana Fail Info", y, x + 35);
#endif

				/* Dump the spells */
				for (i = 0; i < MAX_ELEMENT_POWERS; i++)
				{
					/* Access the spell */
					spell = element_powers[i];
					if (spell.min_lev > plev)   break;

					mana_cost = element_cost(spell.mana_cost);
					chance = get_spell_chance(spell.fail, spell.min_lev, mana_cost);

					/* Get info */
					element_info(comment, i);

					/* Get spell name */
					sprintf(spellname, spell.name, element_name[p_ptr->realm1][spell.elem]);

					/* Dump the spell --(-- */
					sprintf(psi_desc, "  %c) %-30s%2d %4d %3d%%%s",
						I2A(i), spellname,
						spell.min_lev, mana_cost, chance, comment);
					prt(psi_desc, y + i + 1, x);
				}

				/* Clear the bottom line */
				prt("", y + i + 1, x);
			}

			/* Hide the list */
			else
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				screen_load();
			}

			/* Redo asking */
			continue;
		}

		/* Note verify */
		ask = isupper(choice);

		/* Lowercase */
		if (ask) choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell();
			continue;
		}

		/* Save the spell index */
		spell = element_powers[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
#ifdef JP
			(void) strnfmt(tmp_val, 78, "%s¤ò»È¤¤¤Þ¤¹¤«¡©", element_powers[i].name);
#else
			(void)strnfmt(tmp_val, 78, "Use %s? ", element_powers[i].name);
#endif

			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) screen_load();

	/* Show choices */
	if (show_choices)
	{
		/* Update */
		p_ptr->window |= (PW_SPELL);

		/* Window stuff */
		window_stuff();
	}

	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = i;

#ifdef ALLOW_REPEAT /* TNB */

	repeat_push(*sn);

#endif /* ALLOW_REPEAT -- TNB */

	/* Success */
	return (TRUE);
}


/*
 * do_cmd_cast calls this function if the player's class
 * is 'mindcrafter'.
 */
static bool cast_elementalist_spell(int spell)
{
	int dir;
	int dam;
	int plev = p_ptr->lev;
	int type = p_ptr->realm1;
	int beam = plev;


	/* spell code */
	switch (spell)
	{
	case 0:	/* Elemental bolt */
		if (!get_aim_dir(&dir)) return FALSE;

		fire_bolt(element_type[type][0], dir,
			  damroll(3 + ((plev - 1) / 5), 4));  
		break;
	case 1:	/* Detect monsters */
		detect_monsters_normal(DETECT_RAD_DEFAULT);
		detect_monsters_invis(DETECT_RAD_DEFAULT);
		break;
	case 2:
		/* Psychometry */
		return psychometry();
	case 3:
		(void)hp_player(damroll(2, 8));
		(void)set_cut(p_ptr->cut - 10);
		break;
	case 4:
		if (!get_aim_dir(&dir)) return FALSE;

		dam = damroll(8 + ((plev - 5) / 4), 8);
		if (fire_bolt_or_beam(beam, element_type[type][1], dir, dam))
		{
			if (element_type[type][1] == GF_OLD_DRAIN) (void)hp_player(dam / 2);
		}
		break;
	case 5:
		detect_objects_magic(DETECT_RAD_DEFAULT);
		break;
	case 6:
		project_length = 2;
		if (!get_aim_dir(&dir)) return FALSE;

		fire_beam(element_type[type][0], dir, (50 + plev * 2));
		project_length = 0;
		break;
	case 7:
		if (!get_aim_dir(&dir)) return FALSE;

		dam = 55 + plev;
		fire_ball(element_type[type][0], dir, dam, 2);
		break;
	case 8:
		if (!get_aim_dir(&dir)) return FALSE;

		dam = p_ptr->chp / 2;
		if (dam > 120) dam = 120;
		if (fire_ball(element_type[type][1], dir, dam, -3))
		{
			if (element_type[type][1] == GF_OLD_DRAIN) (void)hp_player(dam / 2);
		}
		break;
	case 9:
		if (!get_aim_dir(&dir)) return FALSE;

		fire_ball_hide(GF_E_GENOCIDE, dir, (plev + 50), 0);
		break;
	case 10:
		if (!get_aim_dir(&dir)) return FALSE;

		dam = damroll(12 + ((plev - 5) / 4), 8);
		fire_bolt_or_beam(beam, element_type[type][2], dir, dam);  
		break;
	case 11:
		dam = randint0(plev * 4);
		project_hack(element_type[type][0], dam);
		break;
	case 12:
		if (!get_aim_dir(&dir)) return FALSE;

		dam = 75 + plev;
		if (fire_ball(element_type[type][1], dir, dam, 2))
		{
			if (element_type[type][1] == GF_OLD_DRAIN) (void)hp_player(dam / 2);
		}
		break;
	case 13:
	{
		int k;
		int num = damroll(5, 3);
		int y, x;
		int attempts;
		
		for (k = 0; k < num; k++)
		{
			attempts = 1000;
			
			while (attempts--)
			{
				scatter(&y, &x, py, px, 4, 0);
				
				if (!cave_floor_bold(y, x)) continue;
				
				if ((y != py) || (x != px)) break;
			}
			
			project(0, 0, y, x, damroll(6 + plev / 8, 8), element_type[type][0],
				(PROJECT_BEAM | PROJECT_THRU | PROJECT_GRID | PROJECT_KILL));
		}
		break;
	}
	case 14:
		if (!get_aim_dir(&dir)) return FALSE;

		dam = 125 + plev * 2;
		if (fire_ball(element_type[type][1], dir, dam, 3))
		{
			if (element_type[type][1] == GF_OLD_DRAIN) (void)hp_player(dam / 2);
		}
		break;
	case 15:
		if (!get_aim_dir(&dir)) return FALSE;

		dam = p_ptr->chp * 2 / 3;
		fire_ball(element_type[type][0], dir, dam, -3);
		break;
	case 16:
		if (!get_aim_dir(&dir)) return FALSE;

		dam = 300 + plev * 5;
		fire_ball(element_type[type][2], dir, dam, 5);
		break;
	default:
#ifdef JP
		msg_print("¤Ê¤Ë¡©");
#else
		msg_print("Zap?");
#endif
	}

	return TRUE;
}


/*
 * do_cmd_cast calls this function if the player's class
 * is 'mindcrafter'.
 */
void do_cmd_element(void)
{
	int             n = 0;
	int             chance;
	int             mana_cost;
	int             plev = p_ptr->lev;
	int             old_csp = p_ptr->csp;
	element_power spell;
	bool            cast;


	/* not if confused */
	if (p_ptr->confused)
	{
#ifdef JP
		msg_print("º®Íð¤·¤Æ¤¤¤Æ½¸Ãæ¤Ç¤­¤Ê¤¤¡ª");
#else
		msg_print("You are too confused!");
#endif
		return;
	}

	/* get power */
	if (!get_element_power(&n, FALSE)) return;

	spell = element_powers[n];
	mana_cost = element_cost(spell.mana_cost);

	/* Verify "dangerous" spells */
	if (mana_cost > p_ptr->csp)
	{
		/* Warning */
#ifdef JP
		msg_print("£Í£Ð¤¬Â­¤ê¤Þ¤»¤ó¡£");
#else
		msg_print("You do not have enough mana to use this power.");
#endif

		if (!over_exert) return;

		/* Verify */
#ifdef JP
		if (!get_check("¤½¤ì¤Ç¤âÄ©Àï¤·¤Þ¤¹¤«? ")) return;
#else
		if (!get_check("Attempt it anyway? ")) return;
#endif
	}

	/* Spell failure chance */
	chance = get_spell_chance(spell.fail, spell.min_lev, mana_cost);

	/* Failed spell */
	if (randint0(100) < chance)
	{
		if (flush_failure) flush();
#ifdef JP
		msg_format("Àº¿À¤Î½¸Ãæ¤Ë¼ºÇÔ¤·¤¿¡ª");
#else
		msg_format("You failed to concentrate hard enough!");
#endif
		sound(SOUND_FAIL);

		if (randint1(100) < (chance / 2))
		{
			/* Elemental storm */
#ifdef JP
			msg_print("¸µÁÇ¤ÎÎÏ¤¬À©¸æ¤Ç¤­¤Ê¤¤ÈÅÎ®¤È¤Ê¤Ã¤Æ²òÊü¤µ¤ì¤¿¡ª");
#else
			msg_print("Elemental power unleashes its power in an uncontrollable storm!");
#endif
			project(1, 2 + plev / 10, py, px, plev * 2,
				element_type[p_ptr->realm1][0],
				PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM);
			p_ptr->csp = MAX(0, p_ptr->csp - plev * MAX(1, plev / 10));
		}
	}
	else
	{
		sound(SOUND_ZAP);

		/* Cast the spell */
		cast = cast_elementalist_spell(n);

		if (!cast) return;
	}

	/* Take a turn */
	energy_use = 100;

	/* Sufficient mana */
	if (mana_cost <= old_csp)
	{
		/* Use some mana */
		p_ptr->csp -= mana_cost;

		/* Limit */
		if (p_ptr->csp < 0) p_ptr->csp = 0;
	}

	/* Over-exert the player */
	else
	{
		int oops = mana_cost - old_csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;

		/* Message */
#ifdef JP
		msg_print("Àº¿À¤ò½¸Ãæ¤·¤¹¤®¤Æµ¤¤ò¼º¤Ã¤Æ¤·¤Þ¤Ã¤¿¡ª");
#else
		msg_print("You faint from the effort!");
#endif

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint1(5 * oops + 1));

		/* Damage CON (possibly permanently) */
		if (randint0(100) < 50)
		{
			bool perm = (randint0(100) < 25);

			/* Message */
#ifdef JP
			msg_print("ÂÎ¤ò°­¤¯¤·¤Æ¤·¤Þ¤Ã¤¿¡ª");
#else
			msg_print("You have damaged your health!");
#endif

			/* Reduce constitution */
			(void)dec_stat(A_CON, 15 + randint1(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);
	p_ptr->window |= (PW_SPELL);
}

/*
 * do_cmd_cast calls this function if the player's class
 * is 'mindcrafter'.
 */
void do_cmd_elem_browse(void)
{
	int n = 0;
	int j, line;
	char temp[62 * 5];
	char tip_string[62 * 5];
	screen_save();

	while(1)
	{
		/* get power */
		if (!get_element_power(&n, TRUE))
		{
			screen_load();
			return;
		}

		/* Clear lines, position cursor  (really should use strlen here) */
		Term_erase(12, 21, 255);
		Term_erase(12, 20, 255);
		Term_erase(12, 19, 255);
		Term_erase(12, 18, 255);
		Term_erase(12, 17, 255);
		Term_erase(12, 16, 255);

		sprintf(tip_string, element_tips[n].tips, element_tips[n].element[p_ptr->realm1]);
		if( p_ptr->realm1 == ELEMENT_REALM_DEATH &&
		    element_powers[n].elem == 1 )
			strcat(tip_string, "°ÒÎÏ¤ÎÈ¾Ê¬¤À¤±HP¤¬ÂÎÎÏ²óÉü¤¹¤ë¡£");
		roff_to_buf(tip_string, 62, temp, sizeof(temp));
		for(j = 0, line = 17; temp[j]; j += (1 + strlen(&temp[j])))
		{
			prt(&temp[j], line, 15);
			line++;
		}

		if (p_ptr->lev > 37) pause_line(21);
	}
}

void element_bonuses(void)
{
	switch(p_ptr->realm1)
	{
	case 0:
		p_ptr->resist_fire = TRUE;
		if (p_ptr->lev > 29) p_ptr->immune_fire = TRUE;
		break;
	case 1:
		p_ptr->resist_cold = TRUE;
		if (p_ptr->lev > 29) p_ptr->immune_cold = TRUE;
		break;
	case 2:
		p_ptr->resist_elec = TRUE;
		if (p_ptr->lev > 29) p_ptr->immune_elec = TRUE;
		break;
	case 3:
		p_ptr->resist_acid = TRUE;
		if (p_ptr->lev > 29) p_ptr->immune_acid = TRUE;
		break;
	case 4:
		p_ptr->resist_dark = TRUE;
		if (p_ptr->lev > 29) p_ptr->resist_neth = TRUE;
		break;
	case 5:
		p_ptr->resist_conf = TRUE;
		if (p_ptr->lev > 29) p_ptr->resist_chaos = TRUE;
		break;
	case 6:
		p_ptr->resist_shard = TRUE;
		if (p_ptr->lev > 29) p_ptr->reflect = TRUE;
		break;
	case 7:
		p_ptr->resist_pois = TRUE;
		if (p_ptr->lev > 29) p_ptr->resist_disen = TRUE;
		break;
	}
}

void element_flags(u32b *f1, u32b *f2, u32b *f3)
{
	switch(p_ptr->realm1)
	{
	case 0:
		(*f2) |= (TR2_RES_FIRE);
		if (p_ptr->lev > 29) (*f2) |= (TR2_IM_FIRE);
		break;
	case 1:
		(*f2) |= (TR2_RES_COLD);
		if (p_ptr->lev > 29) (*f2) |= (TR2_IM_COLD);
		break;
	case 2:
		(*f2) |= (TR2_RES_ELEC);
		if (p_ptr->lev > 29) (*f2) |= (TR2_IM_ELEC);
		break;
	case 3:
		(*f2) |= (TR2_RES_ACID);
		if (p_ptr->lev > 29) (*f2) |= (TR2_IM_ACID);
		break;
	case 4:
		(*f2) |= (TR2_RES_DARK);
		if (p_ptr->lev > 29) (*f2) |= (TR2_RES_NETHER);
		break;
	case 5:
		(*f2) |= (TR2_RES_CONF);
		if (p_ptr->lev > 29) (*f2) |= (TR2_RES_CHAOS);
		break;
	case 6:
		(*f2) |= (TR2_RES_SHARDS);
		if (p_ptr->lev > 29) (*f2) |= (TR2_REFLECT);
		break;
	case 7:
		(*f2) |= (TR2_RES_POIS);
		if (p_ptr->lev > 29) (*f2) |= (TR2_RES_DISEN);
		break;
	}
}

void element_immunity(u32b *f1, u32b *f2, u32b *f3)
{
	switch(p_ptr->realm1)
	{
	case 0:
		if (p_ptr->lev > 29) (*f2) |= (TR2_RES_FIRE);
		break;
	case 1:
		if (p_ptr->lev > 29) (*f2) |= (TR2_RES_COLD);
		break;
	case 2:
		if (p_ptr->lev > 29) (*f2) |= (TR2_RES_ELEC);
		break;
	case 3:
		if (p_ptr->lev > 29) (*f2) |= (TR2_RES_ACID);
		break;
	}
}


byte choose_element(void)
{
	int picks[MAX_ELEMENTS] = {0};
	int k, i, n, cs, os;
	char c;
	char sym[MAX_ELEMENTS];
	char p2 = ')';
	char buf[80], cur[80];

	/* Extra info */
#ifdef JP
	put_str ("Ãí°Õ¡§¸µÁÇ·ÏÅý¤ÎÁªÂò¤Ë¤è¤ê¤¢¤Ê¤¿¤¬½¬ÆÀ¤¹¤ë¼öÊ¸¤Î¥¿¥¤¥×¤¬·è¤Þ¤ê¤Þ¤¹¡£", 23, 5);
#else
	put_str ("Note: The system of element will determine which spells you can learn.", 23, 5);
#endif

#if 0
	cs = n = 0;
#else
	n = 0;
#endif
	for (i = 0; i < MAX_ELEMENTS; i++)
	{
		if (n < 26)
			sym[n] = I2A(n);
		else
			sym[n] = ('A' + n - 26);
		sprintf(buf, "%c%c %s", sym[n], p2, element_title[i]);
		put_str(buf, 12 + (n/5), 2 + 15 * (n%5));
		picks[n++] = i;
	}
#ifdef JP
	sprintf(cur, "%c%c %s", '*', p2, "¥é¥ó¥À¥à");
#else
	sprintf(cur, "%c%c %s", '*', p2, "Random");
#endif

	/* Get a realm */
	k = -1;
	cs = 0;
	os = n;
	while (1)
	{
		/* Move Cursol */
		if (cs != os)
		{
			c_put_str(TERM_WHITE, cur, 12 + (os/5), 2 + 15 * (os%5));
			put_str("                                   ", 3, 40);
			put_str("                                   ", 4, 40);

			if(cs == n)
			{
#ifdef JP
				sprintf(cur, "%c%c %s", '*', p2, "¥é¥ó¥À¥à");
#else
				sprintf(cur, "%c%c %s", '*', p2, "Random");
#endif
			}
			else
			{
				sprintf(cur, "%c%c %s", sym[cs], p2, element_title[picks[cs]]);
			}
			c_put_str(TERM_YELLOW, cur, 12 + (cs/5), 2 + 15 * (cs%5));
			os = cs;
		}

		if (k >= 0) break;

#ifdef JP
		sprintf(buf, "·ÏÅý¤òÁª¤ó¤Ç²¼¤µ¤¤(%c-%c) ('='½é´ü¥ª¥×¥·¥ç¥óÀßÄê): ", sym[0], sym[n-1]);
#else
		sprintf(buf, "Choose a system (%c-%c) ('=' for options): ", sym[0], sym[n-1]);
#endif

		put_str(buf, 10, 10);
		c = inkey();
		if (c == '8')
		{
			if (cs >= 5) cs -= 5;
			continue;
		}
		if (c == '4')
		{
			if (cs > 0) cs--;
			continue;
		}
		if (c == '6')
		{
			if (cs < n) cs++;
			continue;
		}
		if (c == '2')
		{
			if ((cs + 5) <= n) cs += 5;
			continue;
		}
		if (c == ' ' || c == '\r' || c == '\n')
		{
			if(cs == n)
			{
				k = randint0(n);
				break;
			}
			else
			{
				k = cs;
				break;
			}
		}
		if (c == '*')
		{
			k = randint0(n);
			break;
		}
		if (c == 'Q')
		{
			remove_loc();
			quit(NULL);
		}
		if (c == 'S') return 255;
		k = (islower(c) ? A2I(c) : -1);
		if ((k >= 0) && (k < n))
		{
			cs = k;
			continue;
		}
		k = (isupper(c) ? (26 + c - 'A') : -1);
		if ((k >= 26) && (k < n))
		{
			cs = k;
			continue;
		}
		else k = -1;
		if (c == '?') do_cmd_help();
		else if (c == '=')
		{
			screen_save();
#ifdef JP
			do_cmd_options_aux(6, "½é´ü¥ª¥×¥·¥ç¥ó");
#else
			do_cmd_options_aux(6, "Startup Options");
#endif
			screen_load();
		}
		else bell();
	}

	/* Clean up */
	clear_from(10);

	return (picks[k]);
}
