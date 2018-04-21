/* File: misc.c */

/* Purpose: misc code */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 *
 * James E. Wilson and Robert A. Koeneke and Ben Harrison have released all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2 or any later version),
 * or under the terms of the traditional Angband license.
 *
 * All changes in Hellband are Copyright (c) 2005-2007 Konijn
 * I Konijn  release all changes to the Angband code under the terms of the GNU General Public License (version 2),
 * as well as under the traditional Angband license. It may be redistributed under the terms of the GPL (version 2),
 * or under the terms of the traditional Angband license.
 */

#include "angband.h"

/* Total status line */
static int  status_offset;
static char status_quark[80];
static size_t depth_length;

bool place_status_quark( byte attribute , int carret_shift )
{
	size_t status_quark_size;
	size_t new_length;

	status_quark_size = strlen( status_quark );
	new_length = status_offset + status_quark_size;

	if( new_length > SCREEN_WID /*- depth_length */ ){
		status_offset = new_length;
		strcpy( status_quark , "" );
		return 0;
	}

	c_prt( attribute , status_quark, ROW_STATUS , status_offset );

	strcpy( status_quark , "" );
	status_offset = new_length + carret_shift;
	return 1;
}

/* This should replace all calls to c_put_str and put_str */
bool place_status_cptr( cptr status , byte attribute , int carret_shift )
{
	strcpy( status_quark , status );
	return place_status_quark( attribute , carret_shift );
}

/*
* Converts stat num into a six-char (right justified) string
*/
void cnv_stat(int val, char *out_val)
{
	/* Above 18 */
	if (val > 18)
	{
		int bonus = (val - 18);

		if (bonus > 220)
		{
			sprintf(out_val, "  40+");
		}
		else if (bonus > 0)
		{
			bonus = (bonus-1) / 10;
			sprintf(out_val,"  %d",bonus+19);
		}
	}
	/* From 3 to 18 */
	else
	{
		sprintf(out_val, "  %2d", val);
	}
}

/*
* Modify a stat value by a "modifier", return new value
*
* Stats go up: 3,4,...,17,18,18/10,18/20,...,18/220
* Or even: 18/13, 18/23, 18/33, ..., 18/220
*
* Stats go down: 18/220, 18/210,..., 18/10, 18, 17, ..., 3
* Or even: 18/13, 18/03, 18, 17, ..., 3
*/
s16b modify_stat_value(int value, int amount)
{
	int    i;

	/* Reward */
	if (amount > 0)
	{
		/* Apply each point */
		for (i = 0; i < amount; i++)
		{
			/* One point at a time */
			if (value < 18) value++;

			/* Ten "points" at a time */
			else value += 10;
		}
	}

	/* Penalty */
	else if (amount < 0)
	{
		/* Apply each point */
		for (i = 0; i < (0 - amount); i++)
		{
			/* Ten points at a time */
			if (value >= 18+10) value -= 10;

			/* Hack -- prevent weirdness */
			else if (value > 18) value = 18;

			/* One point at a time */
			else if (value > 3) value--;
		}
	}

	/* Return new value */
	return (value);
}

/*
 Prints "title", including "wizard" or "winner" as needed.
 static void prt_title(void)
 See Hellband 0.8.7 for implementation
*/

/*
* Prints level
*/
static void prt_level(void)
{
	/* Prefix */
	place_status_cptr( "LVL:" , TERM_WHITE , 0 );
	/* Prepare */
	sprintf(status_quark, "%d", p_ptr->lev);
	/* Dump */
	place_status_quark( p_ptr->lev >= p_ptr->max_plv ? TERM_L_GREEN : TERM_YELLOW ,1 );
}

/*
* Display the experience
*/
static void prt_exp(void)
{
	/* At level 50, no point in showing */
    if (p_ptr->lev >= PY_MAX_LEVEL) return;
	/* Prefix */
	place_status_cptr( (p_ptr->exp >= p_ptr->max_exp)?"EXP:":"exp:" , TERM_WHITE , 0 );
	/* Prepare */
	(void)sprintf(status_quark, "%ld", reverse_xp?(long)(player_exp[p_ptr->lev - 1] * p_ptr->expfact / 100L - p_ptr->exp):(long)p_ptr->exp);
	/* Dump */
	place_status_quark( (p_ptr->exp >= p_ptr->max_exp)?TERM_L_GREEN:TERM_YELLOW ,1 );
}

/* Prints current gold */
static void prt_gold(void)
{
	/* Prefix */
	place_status_cptr( "AU:" , TERM_WHITE , 0 );
	/* Prepare */
	sprintf(status_quark, "%ld", (long)p_ptr->au);
	/* Dump */
	place_status_quark( TERM_YELLOW ,1);
}

/* Prints current AC */
static void prt_ac(void)
{
	/* Prefix */
	place_status_cptr( "AC:" , TERM_WHITE , 0 );
	/* Prepare */
	sprintf( status_quark , "%d", p_ptr->dis_ac + p_ptr->dis_to_a);
	place_status_quark( TERM_L_GREEN ,1);
}

/* Return a color corresponding to current hp/mp or future others and the hitpoint warning setting */
byte health_colour( s16b current, s16b max )
{
	if (current >= max)
		return TERM_L_GREEN;
	else if (current > (max * hitpoint_warn) / 10)
		return TERM_YELLOW;
	else
		return TERM_RED;
}

/*
* Prints Cur/Max hit points
*/
static void prt_hp(void)
{
	int hp_size = 1;
	/* Determine hp width */
	if( p_ptr->mhp > 9 )  hp_size = 2;
	if( p_ptr->mhp > 99 ) hp_size = 3;
	if( p_ptr->mhp > 999 )hp_size = 4;
	/* Prefix */
	place_status_cptr( "HP:" , TERM_WHITE , 0 );
	/* Prepare */
	sprintf(status_quark , "%*d/%*d", hp_size , p_ptr->chp , hp_size , p_ptr->mhp );
	/* Dump */
	place_status_quark( health_colour( p_ptr->chp ,  p_ptr->mhp ) , 1 );
}

/*
* Prints players max/cur spell points
*/
static void prt_sp(void)
{
	int sp_size = 1;

	/* Dont show spell points for blood mages or warriors */
	if( p_ptr->pclass == CLASS_BLOOD_MAGE || p_ptr->pclass == CLASS_WARRIOR )
		return;

	/* Determine sp width */
	if( p_ptr->msp > 9 )  sp_size = 2;
	if( p_ptr->msp > 99 ) sp_size = 3;
	if( p_ptr->msp > 999 )sp_size = 4;
	/* Prefix */
	place_status_cptr( "SP:" , TERM_WHITE , 0 );
	/* Prepare */
	sprintf(status_quark , "%*d/%*d", sp_size , p_ptr->csp , sp_size , p_ptr->msp );
	/* Dump */
	place_status_quark( health_colour( p_ptr->csp ,  p_ptr->msp ) , 1 );
}

/*
* Prints depth in stat area
*/
static void prt_depth(void)
{
	char depths[32];

	if (dun_level==0)		(void)strcpy(depths, "Town");
	else if (dun_level==1)	(void)strcpy(depths, "Sewers");
	else if (dun_level>1 && dun_level < 4 ) (void)strcpy(depths, "Gates of Hell");
	else if (dun_level==4)  (void)strcpy(depths, "Acheron's Shores");
	else if (dun_level==5)	(void)strcpy(depths, "1st Circle: Limbo");
	else if (dun_level==6)	(void)strcpy(depths, "2nd Circle: Lust");
	else if (dun_level==7)	(void)strcpy(depths, "3rd Circle: Gluttony");
	else if (dun_level==8)	(void)strcpy(depths, "4th Circle: Greed");
	else if (dun_level==9)	(void)strcpy(depths, "5th Circle: Wrath");
	else if (dun_level==10)	(void)strcpy(depths, "6th Circle: Heresy");
	else if (dun_level==11)	(void)strcpy(depths, "Phlegethon's Shores");
	else if (dun_level==12)	(void)strcpy(depths, "7th Circle: Suicide");
	else if (dun_level==13)	(void)strcpy(depths, "7th Circle: Abandon");
	else if (dun_level==14)	(void)strcpy(depths, "1st Bolgia: Seduction");
	else if (dun_level==15)	(void)strcpy(depths, "2nd Bolgia: Flattery");
	else if (dun_level==16)	(void)strcpy(depths, "3rd Bolgia: Simony");
	else if (dun_level==17)	(void)strcpy(depths, "4th Bolgia: Sorcery");
	else if (dun_level==18)	(void)strcpy(depths, "5th Bolgia: Corruption");
	else if (dun_level==19)	(void)strcpy(depths, "6th Bolgia: Hypocrisy");
	else if (dun_level==20)	(void)strcpy(depths, "7th Bolgia: Thievery");
	else if (dun_level==21)	(void)strcpy(depths, "8th Bolgia: Deception");
	else if (dun_level==22)	(void)strcpy(depths, "9th Bolgia: Discord");
	else if (dun_level==23)	(void)strcpy(depths, "10th Bolgia: Forgery");
	else if (dun_level==24)	(void)strcpy(depths, "9th Circle: Entry");
	else if (dun_level==25)	(void)strcpy(depths, "9th Circle: Betrayal");
	else if (dun_level==26)	(void)strcpy(depths, "9th Circle: Judecca");
	else if (depth_in_feet)	(void)sprintf(depths, "Hell (%d feet)",dun_level * 50);
	else (void)sprintf(depths, "Hell (Level %d)", dun_level);

	depth_length = strlen( depths );
	prt( depths, ROW_STATUS-1, SCREEN_WID - depth_length);
	//We want to keep a space before the depth, and depth_length will be used elsewhere
	depth_length++;
}

/* Prints status of hunger */
static void prt_hunger_bad(void)
{
	if (p_ptr->food < PY_FOOD_FAINT)      place_status_cptr( "Starving" , TERM_RED    , 1);
	else if (p_ptr->food < PY_FOOD_WEAK)  place_status_cptr( "Weak"     , TERM_ORANGE , 1);
	else if (p_ptr->food > PY_FOOD_MAX)   place_status_cptr( "Gorged"   , TERM_RED  , 1);
}

/* Prints status of hunger */
static void prt_hunger_ok(void)
{
         if (p_ptr->food > PY_FOOD_WEAK && p_ptr->food < PY_FOOD_ALERT) place_status_cptr( "Hungry"   , TERM_YELLOW , 1);
	else if (p_ptr->food > PY_FOOD_FULL && p_ptr->food < PY_FOOD_MAX  ) place_status_cptr( "Full"     , TERM_L_GREEN, 1);
}

/* Prints Blind status */
static void prt_blind(void)
{
	if (p_ptr->blind) place_status_cptr( "Blind" , TERM_ORANGE, 1);
}

/* Prints Confusion status */
static void prt_confused(void)
{
	if (p_ptr->confused) place_status_cptr( "Confused" , TERM_ORANGE, 1);
}

/*Print Invulernable status*/
static void prt_invuln(void)
{
	if(p_ptr->invuln) place_status_cptr("IN", TERM_BLUE,   0);
	if(p_ptr->invuln) place_status_cptr("VU", TERM_RED,    0);
	if(p_ptr->invuln) place_status_cptr("LN", TERM_ORANGE, 0);
	if(p_ptr->invuln) place_status_cptr("ER", TERM_BLUE,   0);
	if(p_ptr->invuln) place_status_cptr("AB", TERM_GREEN,  0);
	if(p_ptr->invuln) place_status_cptr("LE", TERM_RED,    1);
}

static void prt_lowered_stats(void){
	int i;
	for(i=0;i<STAT_COUNT;i++)
	{
		if( p_ptr->stat_cur[i] < p_ptr->stat_max[i])
		{
		    place_status_cptr(desc_stat_neg[i], TERM_YELLOW, 1);
		}
	}
}

static void prt_wraith_form(void)
{
	if(p_ptr->wraith_form) place_status_cptr("Wraith Form", TERM_WHITE, 1);
}

/* Prints Fear status */
static void prt_afraid(void)
{
	if (p_ptr->afraid) place_status_cptr( "Afraid" , TERM_ORANGE, 1);
}

/*
* Prints Poisoned status
*/
static void prt_poisoned(void)
{
	if (p_ptr->poisoned) place_status_cptr( "Poisoned" , TERM_ORANGE, 1);
}

/*
* Prints Searching, Resting, Paralysis, or 'count' status
* Display is always exactly 10 characters wide (see below)
*
* This function was a major bottleneck when resting, so a lot of
* the text formatting code was optimized in place below.
*/
static bool prt_state(void)
{
	/* Paralysis */ if( p_ptr->paralyzed) return place_status_cptr( "Paralyzed!" , TERM_RED   , 1 );
	/* Resting   */ if (resting)          return place_status_cptr( "Resting!"   , TERM_WHITE , 1 );
	/* Repeating */ if (command_rep)      return place_status_cptr( "Repeating!" , TERM_WHITE , 1 );
	/* Searching */ if (p_ptr->searching) return place_status_cptr( "Searching"  , TERM_WHITE , 1 );
	return -1;
}

/*
* Prints the speed of a character.			-CJS-
*/
static void prt_speed(void)
{
	int i = p_ptr->pspeed;

	/* Hack -- Visually "undo" the Search Mode Slowdown */
	if (p_ptr->searching) i += 10;

	/* Only show fast or slow, not regular speed, which is 110 itself */
	if (i > 110)
		place_status_cptr( format("Fast+%d", i-110) , TERM_L_BLUE , 1 );
	else if( i < 110 )
		place_status_cptr( format("Slow-%d", 110-i) , TERM_L_UMBER , 1 );
}

static void prt_study(void)
{
	/* Only show study indicator in town, where it matters  */
	if(p_ptr->new_spells && dun_level == 0)
		place_status_cptr( "Study" , TERM_WHITE , 1 );
}


static void prt_cut(void)
{
	int c = p_ptr->cut;

	if (c > 1000)     place_status_cptr("Mortal Wound" , TERM_L_RED  , 1 );
	else if (c > 200) place_status_cptr("Deep gash"    , TERM_RED    , 1 );
	else if (c > 100) place_status_cptr("Severe cut"   , TERM_RED    , 1 );
	else if (c > 50)  place_status_cptr("Nasty cut"    , TERM_ORANGE , 1 );
	else if (c > 25)  place_status_cptr("Bad cut"      , TERM_ORANGE , 1 );
	else if (c > 10)  place_status_cptr("Light cut"    , TERM_YELLOW , 1 );
	else if (c)       place_status_cptr("Graze"        , TERM_YELLOW , 1 );
}

static void prt_stun(void)
{
	int s = p_ptr->stun;

	if (s > 100)      place_status_cptr( "Knocked out" , TERM_RED    , 1);
	else if (s > 50)  place_status_cptr( "Heavy stun"  , TERM_ORANGE , 1);
	else if (s)       place_status_cptr( "Stun"        , TERM_ORANGE , 1);
}

/*
* Redraw the "monster health bar"	-DRS-
* Rather extensive modifications by	-BEN-
*
* The "monster health bar" provides visual feedback on the "health"
* of the monster currently being "tracked".  There are several ways
* to "track" a monster, including targetting it, attacking it, and
* affecting it (and nobody else) with a ranged attack.
*
* Display the monster health bar (affectionately known as the
* "health-o-meter").  Clear health bar if nothing is being tracked.
* Auto-track current target monster when bored.  Note that the
* health-bar stops tracking any monster that "disappears".
*
* Modified to give "character" as well as colour info by Dean Anderson
*/
static void health_redraw(void)
{

#ifdef DRS_SHOW_HEALTH_BAR

	/* Not tracking */
	if (!health_who)
	{
		/* Erase the health bar */
		Term_erase(COL_INFO, ROW_INFO, 12);
	}

	/* Tracking an unseen monster */
	else if (!m_list[health_who].ml)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a hallucinatory monster */
	else if (p_ptr->image)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a dead monster (???) */
	else if (m_list[health_who].hp < 0)
	{
		/* Indicate that the monster health is "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");
	}

	/* Tracking a visible monster */
	else
	{
		int pct, len;
		char *smb;

		monster_type *m_ptr = &m_list[health_who];

		/* Default to almost dead */
		byte attr = TERM_RED;
		smb = "**********";

		/* Extract the "percent" of health */
		pct = 100L * m_ptr->hp / m_ptr->maxhp;

		/* Badly wounded */
		if (pct >= 10) attr = TERM_L_RED;

		/* Wounded */
		if (pct >= 25) attr = TERM_ORANGE;

		/* Somewhat Wounded */
		if (pct >= 60) attr = TERM_YELLOW;

		/* Healthy */
		if (pct >= 100) attr = TERM_L_GREEN;

		/* stunned */
		if (m_ptr->stunned) {
			attr = TERM_VIOLET;
			smb = "STUN******";
		}
        
		/* confused */
		if (m_ptr->confused) {
			attr = TERM_VIOLET;
			smb = "CONF******";
		}
        
		/* Afraid */
		if (m_ptr->monfear) {
			attr = TERM_VIOLET;
			smb = "AFRAID****";
		}
		/* Asleep */
		if (m_ptr->csleep) {
			attr = TERM_BLUE;
			smb = "SLEEPING**";
		}
		if (is_ally(m_ptr)) {
			attr = TERM_L_UMBER;
			smb = "ALLY******";
		}
		/* Convert percent into "health" */
		len = (pct < 10) ? 1 : (pct < 90) ? (pct / 10 + 1) : 10;

		/* Default to "unknown" */
		Term_putstr(COL_INFO, ROW_INFO, 12, TERM_WHITE, "[----------]");

		/* Dump the current "health" (use '*' symbols) */
		Term_putstr(COL_INFO + 1, ROW_INFO, len, attr, smb);
	}

#endif

}


/*
* Display basic info (mostly left of map)
*/
static void prt_status(void)
{
	status_offset = 0;

	/* Current depth */	prt_depth();

	/* Hitpoints */   prt_hp();
	/* Spellpoints */ prt_sp();
	/* Armor */       prt_ac();
	
	/* Level*/	prt_level();
	/* EXP*/    prt_exp();
	/* Gold */	prt_gold();

	/* Speed */ prt_speed();
	/* Study */ prt_study();
	/* State */ prt_state(); /* Paralyzed, Resting, Searching, Repeating.. */

	prt_invuln();
	prt_wraith_form();
	prt_confused();
	prt_blind();
	prt_hunger_bad();
	prt_stun();
	prt_poisoned();
	prt_afraid();
	prt_cut();
	prt_hunger_ok();
	prt_lowered_stats();

	/* Current depth */	prt_depth();
}

/*
* Display extra info (mostly below map)
* static void prt_frame_extra(void)
* See Hellband 0.8.7 for implementation
*/

/*
* XXX XXX XXX XXX
*/
extern void display_spell_list(void);

static void display_messages()
{
	int i, w, h, x, y;

	/* Get size */
	Term_get_size(&w, &h);

	for (i = 0; i < h; i++)
	{
		/* Dump the message on the appropriate line */
		Term_putstr(0, (h - 1) - i, -1, TERM_WHITE, message_str((short)i));
		/* Cursor */
		Term_locate(&x, &y);
		/* Clear to end of line */
		Term_erase(x, y, 255);
	}
}


static void fix_term_type( u32b term_type )
{
	int j;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		term *old = Term;

		/* No window */
		if (!angband_term[j]) continue;

		/* No relevant flags */
		if (!(window_flag[j] & (term_type))) continue;

		/* Activate */
		Term_activate(angband_term[j]);

		if( PW_INVEN          == term_type ){	display_inven();         }
		if( PW_EQUIP          == term_type ){	display_equip();         }
		if( PW_SPELL          == term_type ){	display_spell_list();    }
		if( PW_PLAYER         == term_type ){	display_player(0);       }
		if( PW_VISIBLE        == term_type ){	display_visible();       }
		if( PW_VISIBLE_ITEMS  == term_type ){	display_visible_items(); }
		if( PW_OVERHEAD       == term_type ){	int cy, cx;	display_map(&cy, &cx); }
		if( PW_MONSTER        == term_type ){	if (monster_race_idx) display_roff(monster_race_idx); }
		if( PW_OBJECT         == term_type ){	if (term_k_idx || term_o_ptr != NULL ) display_koff(term_k_idx); }
		if( PW_MESSAGE        == term_type ){	display_messages();	}

		/* Fresh */
		Term_fresh();

		/* Restore */
		Term_activate(old);
	}
}

/*
 * Calculate number of spells player should have, and forget,
* or remember, spells until that number is properly reflected.
*
* Note that this function induces various "status" messages,
* which must be bypasses until the character is created.
*/
static void calc_spells(void)
{
	int			i, j, k, levels;
	int			num_allowed, num_known;

	int use_realm1 = p_ptr->realm1 - 1;
	int use_realm2 = p_ptr->realm2 - 1;
	int which;
	int max_spells;
    
    magic_type   s_magic;
    magic_type  *s_ptr = &s_magic;

	cptr p = ((mp_ptr->spell_book == TV_SORCERY_BOOK) ? "spell" : "prayer");

	/* Hack -- must be literate */
	if (!mp_ptr->spell_book) return;

	/* Make sure mystics dont get in trouble */
	if (!p_ptr->realm1) return;

	/* Hack -- wait for creation */
	if (!character_generated) return;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Determine the number of spells allowed */
	levels = p_ptr->lev - mp_ptr->spell_first + 1;

	/* Hack -- no negative spells */
	if (levels < 0) levels = 0;

	/* Extract total allowed spells */
	num_allowed = (adj_stat[p_ptr->stat_ind[mp_ptr->spell_stat]][ADJ_SPELLS] * levels / 2);

	/* Assume none known */
	num_known = 0;

	/*If we deal with 1 realm, check 32 spells, otherwise check 64*/
	max_spells = p_ptr->realm2?64:32;


	/* Count the number of spells we know */
	for (j = 0; j < max_spells; j++)
	{
		/* Count known spells */
		if ((j < 32) ?
			(spell_learned1 & (1L << j)) :
		(spell_learned2 & (1L << (j - 32))))
		{
			num_known++;
		}
	}

	/* See how many spells we must forget or may learn */
	p_ptr->new_spells = num_allowed - num_known;

	/* Forget spells which are too hard */
	for (i = max_spells-1; i >= 0; i--)
	{
		/* Efficiency -- all done */
		if (!spell_learned1 && !spell_learned2) break;

		/* Access the spell */
		j = spell_order[i];

		/* Skip non-spells */
		if (j >= 99) continue;

		/* Get the spell */
		if (j < 32)
		{
		/* Access the spell */
			get_extended_spell_info( use_realm1, j , s_ptr );
		}
		else{
		/* Access the spell */
			get_extended_spell_info( use_realm2, j%32 , s_ptr );
		}

		/* Skip spells we are allowed to know */
		if (s_ptr->slevel <= p_ptr->lev) continue;

		/* Is it known? */
		if ((j < 32) ?
			(spell_learned1 & (1L << j)) :
		(spell_learned2 & (1L << (j - 32))))
		{
			/* Mark as forgotten */
			if (j < 32)
			{
				spell_forgotten1 |= (1L << j);
				which = use_realm1;
			}
			else
			{
				spell_forgotten2 |= (1L << (j - 32));
				which = use_realm2;
			}

			/* No longer known */
			if (j < 32)
			{
				spell_learned1 &= ~(1L << j);
				which = use_realm1;
			}
			else
			{
				spell_learned2 &= ~(1L << (j - 32));
				which = use_realm2;
			}

			/* Message */
			msg_format("You have forgotten the %s of %s.", p, spells[which][j%32].name);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Forget spells if we know too many spells */
	for (i = max_spells-1; i >= 0; i--)
	{
		/* Stop when possible */
		if (p_ptr->new_spells >= 0) break;

		/* Efficiency -- all done */
		if (!spell_learned1 && !spell_learned2) break;

		/* Get the (i+1)th spell learned */
		j = spell_order[i];

		/* Skip unknown spells */
		if (j >= 99) continue;

		/* Forget it (if learned) */
		if ((j < 32) ?
			(spell_learned1 & (1L << j)) :
		(spell_learned2 & (1L << (j - 32))))
		{
			/* Mark as forgotten */
			if (j < 32)
			{
				spell_forgotten1 |= (1L << j);
				which = use_realm1;
			}
			else
			{
				spell_forgotten2 |= (1L << (j - 32));
				which = use_realm2;
			}

			/* No longer known */
			if (j < 32)
			{
				spell_learned1 &= ~(1L << j);
				which = use_realm1;
			}
			else
			{
				spell_learned2 &= ~(1L << (j - 32));
				which = use_realm2;
			}

			/* Message */
			msg_format("You have forgotten the %s of %s.", p, spells[which][j%32].name);

			/* One more can be learned */
			p_ptr->new_spells++;
		}
	}


	/* Check for spells to remember */
	for (i = 0; i < max_spells; i++)
	{
		/* None left to remember */
		if (p_ptr->new_spells <= 0) break;

		/* Efficiency -- all done */
		if (!spell_forgotten1 && !spell_forgotten2) break;

		/* Get the next spell we learned */
		j = spell_order[i];

		/* Skip unknown spells */
		if (j >= 99) break;

		/* Get the spell */
		if (j < 32)
		{
			get_extended_spell_info( use_realm1, j , s_ptr );
		}
		else{
			get_extended_spell_info( use_realm2, j%32 , s_ptr );
		}

		/* Skip spells we cannot remember */
		if (s_ptr->slevel > p_ptr->lev) continue;

		/* First set of spells */
		if ((j < 32) ?
			(spell_forgotten1 & (1L << j)) :
		(spell_forgotten2 & (1L << (j - 32))))
		{
			/* No longer forgotten */
			if (j < 32)
			{
				spell_forgotten1 &= ~(1L << j);
				which = use_realm1;
			}
			else
			{
				spell_forgotten2 &= ~(1L << (j - 32));
				which = use_realm2;
			}

			/* Known once more */
			if (j < 32)
			{
				spell_learned1 |= (1L << j);
				which = use_realm1;
			}
			else
			{
				spell_learned2 |= (1L << (j - 32));
				which = use_realm2;
			}
			
			/* Message */
			msg_format("You have remembered the %s of %s.",	p, spells[which][j%32].name);

			/* One less can be learned */
			p_ptr->new_spells--;
		}
	}


	/* Assume no spells available */
	k = 0;

	/* Count spells that can be learned */
	for (j = 0; j < max_spells; j++)
	{
		/* Get the spell */
		if (j < 32)
		{
			get_extended_spell_info( use_realm1, j , s_ptr );
		}
		else{
			get_extended_spell_info( use_realm2, j%32 , s_ptr );
		}
		
		/* Skip spells we cannot remember */
		if (s_ptr->slevel > p_ptr->lev) continue;

		/* Skip spells we already know */
		if ((j < 32) ?
			(spell_learned1 & (1L << j)) :
		(spell_learned2 & (1L << (j - 32))))
		{
			continue;
		}

		/* Count it */
		k++;
	}

	/*Make sure we dont have more spell slots than slots*/
	if(k>max_spells)k = max_spells;


	/* Cannot learn more spells than exist */
	if (p_ptr->new_spells > k) p_ptr->new_spells = k;

	/* Spell count changed */
	if (p_ptr->old_spells != p_ptr->new_spells)
	{
		/* Message if needed */
		if (p_ptr->new_spells)
		{
			/* Message */
			msg_format("You can learn %d more %s%s.",
				p_ptr->new_spells, p,
				(p_ptr->new_spells != 1) ? "s" : "");
		}

		/* Save the new_spells value */
		p_ptr->old_spells = p_ptr->new_spells;

		/* Redraw Study Status */
		p_ptr->redraw |= (PR_STUDY);
	}
}


/*
* Calculate maximum mana.  You do not need to know any spells.
* Note that mana is lowered by heavy (or inappropriate) armour.
*
* This function induces status messages.
*/
static void calc_mana(void)
{
	int		msp, levels, cur_wgt, max_wgt;

	object_type	*o_ptr;


	/* Hack -- Must be literate */
	if (!mp_ptr->spell_book) return;

	if (p_ptr->pclass == CLASS_ORPHIC)
	{
		levels = p_ptr->lev;
	}
	else
	{
		/* Extract "effective" player level */
		levels = (p_ptr->lev - mp_ptr->spell_first) + 1;
	}


	/* Hack -- no negative mana */
	if (levels < 0) levels = 0;

	/* Extract total mana */
	msp = adj_stat[p_ptr->stat_ind[mp_ptr->spell_stat]][ADJ_MANA] * levels / 2;

	/* Hack -- usually add one mana */
	if (msp) msp++;

	/* Hack: High mages have a 25% mana bonus */
	if (msp && (p_ptr->pclass == CLASS_HIGH_MAGE || p_ptr->pclass == CLASS_BLOOD_MAGE))
		msp += msp / 4;

	/* Only mages are affected */
	if (mp_ptr->spell_book == TV_SORCERY_BOOK)
	{
		u32b f1, f2, f3;

		/* Assume player is not encumbered by gloves */
		p_ptr->cumber_glove = FALSE;

		/* Get the gloves */
		o_ptr = &inventory[INVEN_HANDS];

		/* Examine the gloves */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Normal gloves hurt mage-type spells */
		if (o_ptr->k_idx &&
			!(f2 & (TR2_FREE_ACT)) &&
			!((f1 & (TR1_DEX)) && (o_ptr->pval > 0)))
		{
			/* Encumbered */
			p_ptr->cumber_glove = TRUE;

			/* Reduce mana */
			msp = (3 * msp) / 4;
		}

		/* Assume player not encumbered by armour */
		p_ptr->cumber_armour = FALSE;

		/* Weigh the armour */
		cur_wgt = 0;
		cur_wgt += inventory[INVEN_BODY].weight;
		cur_wgt += inventory[INVEN_HEAD].weight;
		cur_wgt += inventory[INVEN_ARM].weight;
		cur_wgt += inventory[INVEN_OUTER].weight;
		cur_wgt += inventory[INVEN_HANDS].weight;
		cur_wgt += inventory[INVEN_FEET].weight;

		/* Determine the weight allowance */
		max_wgt = mp_ptr->spell_weight;

		/* Heavy armour penalizes mana */
		if (((cur_wgt - max_wgt) / 10) > 0)
		{
			/* Encumbered */
			p_ptr->cumber_armour = TRUE;

			/* Reduce mana */
			msp -= ((cur_wgt - max_wgt) / 10);
		}

	}

	/* Mana can never be negative */
	if (msp < 0) msp = 0;


	/* Maximum mana has changed */
	if (p_ptr->msp != msp)
	{

		/* XXX XXX XXX New mana maintenance */

		/* Enforce maximum */
		if (p_ptr->csp >= msp)
		{
			p_ptr->csp = msp;
			p_ptr->csp_frac = 0;
		}


		/* Save new mana */
		p_ptr->msp = msp;

		/* Display mana later */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
		p_ptr->window |=(PW_SPELL);
	}


	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Take note when "glove state" changes */
	if (p_ptr->old_cumber_glove != p_ptr->cumber_glove)
	{
		/* Message */
		if (p_ptr->cumber_glove)
		{
			msg_print("Your covered hands feel unsuitable for spellcasting.");
		}
		else
		{
			msg_print("Your hands feel more suitable for spellcasting.");
		}

		/* Save it */
		p_ptr->old_cumber_glove = p_ptr->cumber_glove;
	}


	/* Take note when "armour state" changes */
	if (p_ptr->old_cumber_armour != p_ptr->cumber_armour)
	{
		/* Message */
		if (p_ptr->cumber_armour)
		{
			msg_print("The weight of your armour encumbers your movement.");
		}
		else
		{
			msg_print("You feel able to move more freely.");
		}

		/* Save it */
		p_ptr->old_cumber_armour = p_ptr->cumber_armour;
	}
}



/*
* Calculate the players (maximal) hit points
* Adjust current hitpoints if necessary
*/
static void calc_hitpoints(void)
{
	int bonus, mhp;

	/* Un-inflate "half-hitpoint bonus per level" value */
	bonus = ((int)(adj_stat[p_ptr->stat_ind[A_CON]][ADJ_HP]) - 128);

	/* Calculate hitpoints */
	mhp = player_hp[p_ptr->lev-1] + (bonus * p_ptr->lev / 2);

	/* Always have at least one hitpoint per level */
	if (mhp < p_ptr->lev + 1) mhp = p_ptr->lev + 1;

	/* Factor in the hero / superhero settings */
	if (p_ptr->hero) mhp += 10;
	if (p_ptr->shero) mhp += 30;

	/* Factor in blood mage */
	if( p_ptr->pclass == CLASS_BLOOD_MAGE )
	{
		mhp = mhp + p_ptr->msp;
	}

	/* New maximum hitpoints */
	if (p_ptr->mhp != mhp)
	{

		/* XXX XXX XXX New hitpoint maintenance */

		/* Enforce maximum */
		if (p_ptr->chp >= mhp)
		{
			p_ptr->chp = mhp;
			p_ptr->chp_frac = 0;
		}

		/* Save the new max-hitpoints */
		p_ptr->mhp = mhp;

		/* Display hitpoints (later) */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
	}
}



/*
* Extract and set the current "lite radius"
*
* SWD: Experimental modification: multiple light sources have additive effect.
*
*/
static void calc_torch(void)
{
	int i;
	object_type *o_ptr;
	u32b f1, f2, f3;

	/* Assume no light */
	p_ptr->cur_lite = 0;

	/* Loop through all wielded items */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Examine actual lites */
		if ((i == INVEN_LITE) && (o_ptr->k_idx) && (o_ptr->tval == TV_LITE))
		{
			/* Torches (with fuel) provide some lite */
			if ((o_ptr->sval == SV_LITE_TORCH) && (o_ptr->pval > 0))
			{
				p_ptr->cur_lite += 1;
				continue;
			}

			/* Lanterns (with fuel) provide more lite */
			if ((o_ptr->sval == SV_LITE_LANTERN) && (o_ptr->pval > 0))
			{
				p_ptr->cur_lite += 2;
				continue;
			}

			/* Orbs provide permanent lite */
			if (o_ptr->sval == SV_LITE_ORB)
			{
				p_ptr->cur_lite += 2;
				continue;
			}

			/* Artifact Lites provide permanent, bright, lite */
			if (artefact_p(o_ptr))
			{
				p_ptr->cur_lite += 3;
				continue;
			}

			/* notreached */
		}
		else
		{
			/* Skip empty slots */
			if (!o_ptr->k_idx) continue;

			/* Extract the flags */
			object_flags(o_ptr, &f1, &f2, &f3);

			/* does this item glow? */
			if (f3 & TR3_LITE) p_ptr->cur_lite++;
		}

	}

	/* max radius is 5 without rewriting other code -- */
	/* see cave.c:update_lite() and defines.h:LITE_MAX */
	if (p_ptr->cur_lite > 5) p_ptr->cur_lite = 5;

	/* check if the player doesn't have a lite source, */
	/* but does glow as an intrinsic.                  */
	if (p_ptr->cur_lite == 0 && p_ptr->lite) p_ptr->cur_lite = 1;

	/* end experimental mods */

	/* Reduce lite when running if requested */
	if (running && view_reduce_lite)
	{
		/* Reduce the lite radius if needed */
		if (p_ptr->cur_lite > 1) p_ptr->cur_lite = 1;
	}

	/* Notice changes in the "lite radius" */
	if (p_ptr->old_lite != p_ptr->cur_lite)
	{
		/* Update the lite */
		p_ptr->update |= (PU_LITE);

		/* Update the monsters */
		p_ptr->update |= (PU_MONSTERS);

		/* Remember the old lite */
		p_ptr->old_lite = p_ptr->cur_lite;
	}
}



/*
* Computes current weight limit.
*/
static int weight_limit(void)
{
	int i;

	/* Weight limit based only on strength */
	i = adj_stat[p_ptr->stat_ind[A_STR]][ADJ_WEIGHT] * 100;

	/* Return the result */
	return (i);
}


static void calc_object_flag_bonuses( u32b f1 , u32b f2 , u32b f3 , s16b pval, int *extra_blows , int *extra_shots )
{
	/* Affect stats */
	if (f1 & (TR1_STR)) p_ptr->stat_add[A_STR] += pval;
	if (f1 & (TR1_INT)) p_ptr->stat_add[A_INT] += pval;
	if (f1 & (TR1_WIS)) p_ptr->stat_add[A_WIS] += pval;
	if (f1 & (TR1_DEX)) p_ptr->stat_add[A_DEX] += pval;
	if (f1 & (TR1_CON)) p_ptr->stat_add[A_CON] += pval;
	if (f1 & (TR1_CHA)) p_ptr->stat_add[A_CHA] += pval;
	/* Affect stealth */
	if (f1 & (TR1_STEALTH)) p_ptr->skill_stl += pval;
	/* Affect searching ability (factor of five) */
	if (f1 & (TR1_SEARCH)) p_ptr->skill_srh += (pval * 5);
	/* Affect searching frequency (factor of five) */
	if (f1 & (TR1_SEARCH)) p_ptr->skill_fos += (pval * 5);
	/* Affect infravision */
	if (f1 & (TR1_INFRA)) p_ptr->see_infra += pval;
	/* Affect digging (factor of 20) */
	if (f1 & (TR1_TUNNEL)) p_ptr->skill_dig += (pval * 20);
	/* Affect speed */
	if (f1 & (TR1_SPEED)) p_ptr->pspeed += pval;
	/* Affect blows */
	if (f1 & (TR1_BLOWS)) *extra_blows += pval;
	/* Hack -- cause earthquakes */
	if (f1 & (TR1_IMPACT)) p_ptr->impact = TRUE;
	/* Boost shots */
	if (f3 & (TR3_XTRA_SHOTS)) *extra_shots += 1;
	/* Various flags */
	if (f3 & (TR3_AGGRAVATE)) p_ptr->aggravate = TRUE;
	if (f3 & (TR3_TELEPORT)) p_ptr->teleport = TRUE;
	if (f3 & (TR3_DRAIN_EXP)) p_ptr->exp_drain = TRUE;
	if (f3 & (TR3_BLESSED)) p_ptr->bless_blade = TRUE;
	if (f3 & (TR3_XTRA_MIGHT)) p_ptr->xtra_might = TRUE;
	if (f3 & (TR3_SLOW_DIGEST)) p_ptr->slow_digest = TRUE;
	if (f3 & (TR3_REGEN)) p_ptr->regenerate = TRUE;
	if (f3 & (TR3_TELEPATHY)) p_ptr->telepathy = TRUE;
	if (f3 & (TR3_LITE)) p_ptr->lite = TRUE;
	if (f3 & (TR3_SEE_INVIS)) p_ptr->see_inv = TRUE;
	if (f3 & (TR3_FEATHER)) p_ptr->ffall = TRUE;
	if (f2 & (TR2_FREE_ACT)) p_ptr->free_act = TRUE;
	if (f2 & (TR2_HOLD_LIFE)) p_ptr->hold_life = TRUE;
	if (f3 & (TR3_WRAITH)) p_ptr->wraith_form = MAX(p_ptr->wraith_form, 20);
	/* Immunity flags */
	if (f2 & (TR2_IM_FIRE)) p_ptr->immune_fire = TRUE;
	if (f2 & (TR2_IM_ACID)) p_ptr->immune_acid = TRUE;
	if (f2 & (TR2_IM_COLD)) p_ptr->immune_cold = TRUE;
	if (f2 & (TR2_IM_ELEC)) p_ptr->immune_elec = TRUE;
	/* Resistance flags */
	if (f2 & (TR2_RES_ACID)) p_ptr->resist_acid = TRUE;
	if (f2 & (TR2_RES_ELEC)) p_ptr->resist_elec = TRUE;
	if (f2 & (TR2_RES_FIRE)) p_ptr->resist_fire = TRUE;
	if (f2 & (TR2_RES_COLD)) p_ptr->resist_cold = TRUE;
	if (f2 & (TR2_RES_POIS)) p_ptr->resist_pois = TRUE;
	if (f2 & (TR2_RES_FEAR)) p_ptr->resist_fear = TRUE;
	if (f2 & (TR2_RES_CONF)) p_ptr->resist_conf = TRUE;
	if (f2 & (TR2_RES_SOUND)) p_ptr->resist_sound = TRUE;
	if (f2 & (TR2_RES_LITE)) p_ptr->resist_lite = TRUE;
	if (f2 & (TR2_RES_DARK)) p_ptr->resist_dark = TRUE;
	if (f2 & (TR2_RES_CHAOS)) p_ptr->resist_chaos = TRUE;
	if (f2 & (TR2_RES_DISEN)) p_ptr->resist_disen = TRUE;
	if (f2 & (TR2_RES_SHARDS)) p_ptr->resist_shard = TRUE;
	if (f2 & (TR2_RES_NEXUS)) p_ptr->resist_nexus = TRUE;
	if (f2 & (TR2_RES_BLIND)) p_ptr->resist_blind = TRUE;
	if (f2 & (TR2_RES_NETHER)) p_ptr->resist_neth = TRUE;
	if (f2 & (TR2_REFLECT)) p_ptr->reflect = TRUE;
	if (f3 & (TR3_SH_FIRE)) p_ptr->sh_fire = TRUE;
	if (f3 & (TR3_SH_ELEC)) p_ptr->sh_elec = TRUE;
	if (f3 & (TR3_NO_MAGIC)) p_ptr->anti_magic = TRUE;
	if (f3 & (TR3_NO_TELE)) p_ptr->anti_tele = TRUE;
	/* Sustain flags */
	if (f2 & (TR2_SUST_STR)) p_ptr->sustain_str = TRUE;
	if (f2 & (TR2_SUST_INT)) p_ptr->sustain_int = TRUE;
	if (f2 & (TR2_SUST_WIS)) p_ptr->sustain_wis = TRUE;
	if (f2 & (TR2_SUST_DEX)) p_ptr->sustain_dex = TRUE;
	if (f2 & (TR2_SUST_CON)) p_ptr->sustain_con = TRUE;
	if (f2 & (TR2_SUST_CHA)) p_ptr->sustain_cha = TRUE;
}

/*
* Calculate the players current "state", taking into account
* not only race/class intrinsics, but also objects being worn
* and temporary spell effects.
*
* See also calc_mana() and calc_hitpoints().
*
* Take note of the new "speed code", in particular, a very strong
* player will start slowing down as soon as he reaches 150 pounds,
* but not until he reaches 450 pounds will he be half as fast as
* a normal kobold.  This both hurts and helps the player, hurts
* because in the old days a player could just avoid 300 pounds,
* and helps because now carrying 300 pounds is not very painful.
*
* The "weapon" and "bow" do *not* add to the bonuses to hit or to
* damage, since that would affect non-combat things.  These values
* are actually added in later, at the appropriate place.
*
* This function induces various "status" messages.
*/
static void calc_bonuses(void)
{
	int			i, j, hold;

	int			old_speed;

	int			old_telepathy;
	int			old_see_inv;

	int			old_dis_ac;
	int			old_dis_to_a;

	int			extra_blows;
	int			extra_shots;

	object_type		*o_ptr;

	u32b		f1, f2, f3;

	/* Save the old speed */
	old_speed = p_ptr->pspeed;

	/* Save the old vision stuff */
	old_telepathy = p_ptr->telepathy;
	old_see_inv = p_ptr->see_inv;

	/* Save the old armour class */
	old_dis_ac = p_ptr->dis_ac;
	old_dis_to_a = p_ptr->dis_to_a;

	/* Clear extra blows/shots */
	extra_blows = extra_shots = 0;

	/* Clear the stat modifiers */
	for (i = 0; i < 6; i++) p_ptr->stat_add[i] = 0;


	/* Clear the Displayed/Real armour class */
	p_ptr->dis_ac = p_ptr->ac = 0;

	/* Clear the Displayed/Real Bonuses */
	p_ptr->dis_to_h = p_ptr->to_h = 0;
	p_ptr->dis_to_d = p_ptr->to_d = 0;
	p_ptr->dis_to_a = p_ptr->to_a = 0;


	/* Clear all the flags */
	p_ptr->aggravate = FALSE;
	p_ptr->teleport = FALSE;
	p_ptr->exp_drain = FALSE;
	p_ptr->bless_blade = FALSE;
	p_ptr->xtra_might = FALSE;
	p_ptr->impact = FALSE;
	p_ptr->see_inv = FALSE;
	p_ptr->free_act = FALSE;
	p_ptr->slow_digest = FALSE;
	p_ptr->regenerate = FALSE;
	p_ptr->ffall = FALSE;
	p_ptr->hold_life = FALSE;
	p_ptr->telepathy = FALSE;
	p_ptr->lite = FALSE;
	p_ptr->sustain_str = FALSE;
	p_ptr->sustain_int = FALSE;
	p_ptr->sustain_wis = FALSE;
	p_ptr->sustain_con = FALSE;
	p_ptr->sustain_dex = FALSE;
	p_ptr->sustain_cha = FALSE;
	p_ptr->resist_acid = FALSE;
	p_ptr->resist_elec = FALSE;
	p_ptr->resist_fire = FALSE;
	p_ptr->resist_cold = FALSE;
	p_ptr->resist_pois = FALSE;
	p_ptr->resist_conf = FALSE;
	p_ptr->resist_sound = FALSE;
	p_ptr->resist_lite = FALSE;
	p_ptr->resist_dark = FALSE;
	p_ptr->resist_chaos = FALSE;
	p_ptr->resist_disen = FALSE;
	p_ptr->resist_shard = FALSE;
	p_ptr->resist_nexus = FALSE;
	p_ptr->resist_blind = FALSE;
	p_ptr->resist_neth = FALSE;
	p_ptr->resist_fear = FALSE;
	p_ptr->reflect = FALSE;
	p_ptr->sh_fire = FALSE;
	p_ptr->sh_elec = FALSE;
	p_ptr->anti_magic = FALSE;
	p_ptr->anti_tele = FALSE;

	p_ptr->immune_acid = FALSE;
	p_ptr->immune_elec = FALSE;
	p_ptr->immune_fire = FALSE;
	p_ptr->immune_cold = FALSE;

	/* Base infravision (purely racial) */
	p_ptr->see_infra = rp_ptr->infra + bsp_ptr->infra;

	/* Base skill -- disarming */
	p_ptr->skill_dis = rp_ptr->r_dis + cp_ptr->c_dis + bsp_ptr->r_dis;

	/* Base skill -- magic devices */
	p_ptr->skill_dev = rp_ptr->r_dev + cp_ptr->c_dev + bsp_ptr->r_dev;

	/* Base skill -- saving throw */
	p_ptr->skill_sav = rp_ptr->r_sav + cp_ptr->c_sav + bsp_ptr->r_sav;

	/* Base skill -- stealth */
	p_ptr->skill_stl = rp_ptr->r_stl + cp_ptr->c_stl + bsp_ptr->r_stl;

	/* Base skill -- searching ability */
	p_ptr->skill_srh = rp_ptr->r_srh + cp_ptr->c_srh + bsp_ptr->r_srh;

	/* Base skill -- searching frequency */
	p_ptr->skill_fos = rp_ptr->r_fos + cp_ptr->c_fos + bsp_ptr->r_fos;

	/* Base skill -- combat (normal) */
	p_ptr->skill_thn = rp_ptr->r_thn + cp_ptr->c_thn + bsp_ptr->r_thn;

	/* Base skill -- combat (shooting) */
	p_ptr->skill_thb = rp_ptr->r_thb + cp_ptr->c_thb + bsp_ptr->r_thb;

	/* Base skill -- combat (throwing) */
	p_ptr->skill_tht = rp_ptr->r_thb + cp_ptr->c_thb + bsp_ptr->r_thb;

	/* Base skill -- digging */
	p_ptr->skill_dig = 0;
		
	/* Start with "normal" speed */
	p_ptr->pspeed = 110;

	/* Start with a single blow per turn */
	p_ptr->num_blow = 1;

	/* Start with a single shot per turn */
	p_ptr->num_fire = 1;

	/* Reset the "xtra" tval */
	p_ptr->tval_xtra = 0;

	/* Reset the "ammo" tval */
	p_ptr->tval_ammo = 0;

	/* Hack -- apply racial/class stat maxes */
	if (maximise_mode)
	{
		/* Apply the racial modifiers */
		for (i = 0; i < 6; i++)
		{
			/* Modify the stats for "race" and freak bonus*/
			p_ptr->stat_add[i] += (rp_ptr->r_adj[i] + cp_ptr->c_adj[i] + bsp_ptr->r_adj[i] + calc_freak_stat_bonus(i) );
		}
	}

	/* I'm adding the corruptions here for the lack of a better place... */
	if (p_ptr->muta3)
	{
		if (p_ptr->muta3 & COR3_XTRA_FAT)		p_ptr->pspeed -= 2;
		if (p_ptr->muta3 & COR3_GOAT_LEGS)		p_ptr->pspeed += 3;
		if (p_ptr->muta3 & COR3_SHORT_LEG)		p_ptr->pspeed -= 3;
		if (p_ptr->muta3 & COR3_FLESH_ROT)		p_ptr->regenerate = FALSE;
		if (p_ptr->muta3 & COR3_STENCH)			p_ptr->skill_stl -= 3;
		if (p_ptr->muta3 & COR3_INFRAVIS)		p_ptr->see_infra += 3;
		if (p_ptr->muta3 & COR3_MAGIC_RES)		p_ptr->skill_sav += (15 + (p_ptr->lev / 5));

		if (p_ptr->muta3 & COR3_GLOW_EYES)
		{
			p_ptr->skill_fos += 15;
			p_ptr->skill_srh += 15;
		}

		if (p_ptr->muta3 & COR3_WART_SKIN)
		{
			p_ptr->to_a += 5;
			p_ptr->dis_to_a += 5;
		}

		if (p_ptr->muta3 & COR3_SCALES)
		{
			p_ptr->to_a += 10;
			p_ptr->dis_to_a += 10;
		}

		if (p_ptr->muta3 & COR3_IRON_SKIN)
		{
			p_ptr->to_a += 25;
			p_ptr->dis_to_a += 25;
		}

		if (p_ptr->muta3 & COR3_MOTION)
		{
			p_ptr->free_act =TRUE;
			p_ptr->skill_stl += 1;
		}
	}

	player_flags( &f1 , &f2 , &f3 );
	calc_object_flag_bonuses( f1 , f2 , f3 , 0 , &extra_blows , &extra_shots );

	/* Scan the usable inventory */
	for (i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
		o_ptr = &inventory[i];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract the item flags */
		object_flags(o_ptr, &f1, &f2, &f3);

		/* Apply item flag bonuses */

		calc_object_flag_bonuses( f1 , f2 , f3 , o_ptr->pval , &extra_blows , &extra_shots );

		/* Modify the base armour class */
		p_ptr->ac += o_ptr->ac;

		/* The base armour class is always known */
		p_ptr->dis_ac += o_ptr->ac;

		/* Apply the bonuses to armour class */
		p_ptr->to_a += o_ptr->to_a;

		/* Apply the mental bonuses to armour class, if known */
		if (object_known_p(o_ptr)) p_ptr->dis_to_a += o_ptr->to_a;

		/* Hack -- do not apply "weapon" bonuses */
		if (i == INVEN_WIELD) continue;

		/* Hack -- do not apply "bow" bonuses */
		if (i == INVEN_BOW) continue;

		/* Apply the bonuses to hit/damage */
		p_ptr->to_h += o_ptr->to_h;
		p_ptr->to_d += o_ptr->to_d;

		/* Apply the mental bonuses tp hit/damage, if known */
		if (object_known_p(o_ptr)) p_ptr->dis_to_h += o_ptr->to_h;
		if (object_known_p(o_ptr)) p_ptr->dis_to_d += o_ptr->to_d;
	}

	/* Huh, pretty sure this is the right, new, place */
	if(p_ptr->magic_shell) p_ptr->anti_magic = TRUE;

	/* Mystic get extra ac for armour _not worn_ */
	if ((p_ptr->pclass == CLASS_MYSTIC) && !(mystic_heavy_armour()))
	{
		if (!(inventory[INVEN_BODY].k_idx))
		{
			p_ptr->to_a += (p_ptr->lev * 3) / 2;
			p_ptr->dis_to_a += (p_ptr->lev * 3) / 2;
		}
		if (!(inventory[INVEN_OUTER].k_idx) && (p_ptr->lev > 15))
		{
			p_ptr->to_a += ((p_ptr->lev - 13) / 3);
			p_ptr->dis_to_a += ((p_ptr->lev - 13) / 3);
		}
		if (!(inventory[INVEN_ARM].k_idx) && (p_ptr->lev > 10))
		{
			p_ptr->to_a += ((p_ptr->lev - 8) / 3);
			p_ptr->dis_to_a += ((p_ptr->lev - 8) / 3);
		}
		if (!(inventory[INVEN_HEAD].k_idx)&& (p_ptr->lev > 4))
		{
			p_ptr->to_a += (p_ptr->lev - 2) / 3;
			p_ptr->dis_to_a += (p_ptr->lev -2) / 3;
		}
		if (!(inventory[INVEN_HANDS].k_idx))
		{
			p_ptr->to_a += (p_ptr->lev / 2);
			p_ptr->dis_to_a += (p_ptr->lev / 2);
		}
		if (!(inventory[INVEN_FEET].k_idx))
		{
			p_ptr->to_a += (p_ptr->lev / 3);
			p_ptr->dis_to_a += (p_ptr->lev / 3);
		}
	}

	/* Hack -- aura of fire also provides light */
	if (p_ptr->sh_fire || p_ptr->prace == ELDER ) p_ptr->lite = TRUE;

	if (p_ptr->prace == GUARDIAN) /* Golems also get an intrinsic AC bonus */
	{
		p_ptr->to_a += 20 + (p_ptr->lev / 5);
		p_ptr->dis_to_a += 20 + (p_ptr->lev / 5);
	}

	/* Calculate stats */
	for (i = 0; i < 6; i++)
	{
		int top, use, ind;

		/* Extract the new "stat_use" value for the stat */
		top = modify_stat_value(p_ptr->stat_max[i], p_ptr->stat_add[i]);

		/* Notice changes */
		if (p_ptr->stat_top[i] != top)
		{
			/* Save the new value */
			p_ptr->stat_top[i] = top;

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
		}

		/* Extract the new "stat_use" value for the stat */
		use = modify_stat_value(p_ptr->stat_cur[i], p_ptr->stat_add[i]);

		if ((i == A_CHA) && (p_ptr->muta3 & COR3_ILL_NORM))
		{
			/* 10 to 18/90 charisma, guaranteed, based on level */
			if (use < 8 + 2 * p_ptr->lev)
			{
				use = 8 + 2 * p_ptr->lev;
			}
		}

		/* Notice changes */
		if (p_ptr->stat_use[i] != use)
		{
			/* Save the new value */
			p_ptr->stat_use[i] = use;

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
		}


		/* Values: 3, 4, ..., 17 */
		if (use <= 18) ind = (use - 3);

		/* Ranges: 18/00-18/09, ..., 18/210-18/219 */
		else if (use <= 18+219) ind = (15 + (use - 18) / 10);

		/* Range: 18/220+ */
		else ind = (37);

		/* Notice changes */
		if (p_ptr->stat_ind[i] != ind)
		{
			/* Save the new index */
			p_ptr->stat_ind[i] = ind;

			/* Change in CON affects Hitpoints */
			if (i == A_CON)
			{
				p_ptr->update |= (PU_HP);
			}

			/* Change in INT may affect Mana/Spells */
			else if (i == A_INT)
			{
				if (mp_ptr->spell_stat == A_INT)
				{
					p_ptr->update |= (PU_MANA | PU_SPELLS);
				}
				if( p_ptr->pclass == CLASS_BLOOD_MAGE )
				{
					p_ptr->update |= (PU_HP);
				}
			}

			/* Change in WIS may affect Mana/Spells */
			else if (i == A_WIS)
			{
				if (mp_ptr->spell_stat == A_WIS)
				{
					p_ptr->update |= (PU_MANA | PU_SPELLS);
				}
			}

			/* Window stuff */
			p_ptr->window |= (PW_PLAYER);
		}
	}


	/* Apply temporary "stun" */
	if (p_ptr->stun > 50)
	{
		p_ptr->to_h -= 20;
		p_ptr->dis_to_h -= 20;
		p_ptr->to_d -= 20;
		p_ptr->dis_to_d -= 20;
	}
	else if (p_ptr->stun)
	{
		p_ptr->to_h -= 5;
		p_ptr->dis_to_h -= 5;
		p_ptr->to_d -= 5;
		p_ptr->dis_to_d -= 5;
	}


	/* Invulnerability */
	if (p_ptr->invuln)
	{
		p_ptr->to_a += 100;
		p_ptr->dis_to_a += 100;
	}

	/* wraith_form */
	if (p_ptr->wraith_form)
	{
		p_ptr->to_a += 100;
		p_ptr->dis_to_a += 100;
		p_ptr->reflect = TRUE;
	}

	/* Temporary blessing */
	if (p_ptr->blessed)
	{
		p_ptr->to_a += 5;
		p_ptr->dis_to_a += 5;
		p_ptr->to_h += 10;
		p_ptr->dis_to_h += 10;
	}

	/* Temprory shield */
	if (p_ptr->shield)
	{
		p_ptr->to_a += 50;
		p_ptr->dis_to_a += 50;
	}

	/* Temporary "Hero" */
	if (p_ptr->hero)
	{
		p_ptr->to_h += 12;
		p_ptr->dis_to_h += 12;
	}

	/* Temporary "Beserk" */
	if (p_ptr->shero)
	{
		p_ptr->to_h += 24;
		p_ptr->dis_to_h += 24;
		p_ptr->to_a -= 10;
		p_ptr->dis_to_a -= 10;
	}

	/* Temporary "fast" */
	if (p_ptr->fast)
	{
		p_ptr->pspeed += 10;
	}

	/* Temporary "slow" */
	if (p_ptr->slow)
	{
		p_ptr->pspeed -= 10;
	}

	/* Morui(ex-Klackons) become faster, so do fae's */
	if ((p_ptr->psign == SIGN_MORUI) || (p_ptr->prace == FAE) )
	{
		p_ptr->pspeed += (p_ptr->lev) / 10;
	}
	
	/* Putting back the double fast Klackon Mystics */
	/* mystics if they dont wear too heavy armour */
	if( ((p_ptr->pclass == CLASS_MYSTIC) && !(mystic_heavy_armour())) )
	{
		p_ptr->pspeed += (p_ptr->lev) / 10;
	}

	/* Temporary telepathy */
	if (p_ptr->tim_esp)
	{
		p_ptr->telepathy = TRUE;
	}

	/* Temporary see invisible */
	if (p_ptr->tim_invis)
	{
		p_ptr->see_inv = TRUE;
	}

	/* Temporary infravision boost */
	if (p_ptr->tim_infra)
	{
		p_ptr->see_infra++;
	}

	/* Hack -- Res Chaos -> Res Conf */
	if (p_ptr->resist_chaos)
	{
		p_ptr->resist_conf = TRUE;
	}

	/* Hack -- Hero/Shero -> Res fear */
	if (p_ptr->hero || p_ptr->shero)
	{
		p_ptr->resist_fear = TRUE;
	}

	if (p_ptr->oppose_fear)  p_ptr->resist_fear = TRUE;
	if (p_ptr->oppose_conf)  p_ptr->resist_conf = TRUE;

	/* Hack -- Telepathy Change */
	if (p_ptr->telepathy != old_telepathy)
	{
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Hack -- See Invis Change */
	if (p_ptr->see_inv != old_see_inv)
	{
		p_ptr->update |= (PU_MONSTERS);
	}

	/* Extract the current weight (in tenth pounds) */
	j = total_weight;

	/* Extract the "weight limit" (in tenth pounds) */
	i = weight_limit();

	/* XXX XXX XXX Apply "encumbrance" from weight */
	if (j > i/2) p_ptr->pspeed -= ((j - (i/2)) / (i / 10));

	/* Bloating slows the player down (a little) */
	if (p_ptr->food >= PY_FOOD_MAX) p_ptr->pspeed -= 10;

	/* Searching slows the player down */
	if (p_ptr->searching) p_ptr->pspeed -= 10;

	/* Display the speed (if needed) */
	if (p_ptr->pspeed != old_speed) p_ptr->redraw |= (PR_SPEED);


	/* Actual Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->to_a += ((int)(adj_stat[p_ptr->stat_ind[A_DEX]][ADJ_AC]) - 128);
	p_ptr->to_d += ((int)(adj_stat[p_ptr->stat_ind[A_STR]][ADJ_DAM]) - 128);
	p_ptr->to_h += ((int)(adj_stat[p_ptr->stat_ind[A_DEX]][ADJ_DEX_HIT]) - 128);
	p_ptr->to_h += ((int)(adj_stat[p_ptr->stat_ind[A_STR]][ADJ_STR_HIT]) - 128);

	/* Displayed Modifier Bonuses (Un-inflate stat bonuses) */
	p_ptr->dis_to_a += ((int)(adj_stat[p_ptr->stat_ind[A_DEX]][ADJ_AC]) - 128);
	p_ptr->dis_to_d += ((int)(adj_stat[p_ptr->stat_ind[A_STR]][ADJ_DAM]) - 128);
	p_ptr->dis_to_h += ((int)(adj_stat[p_ptr->stat_ind[A_DEX]][ADJ_DEX_HIT]) - 128);
	p_ptr->dis_to_h += ((int)(adj_stat[p_ptr->stat_ind[A_STR]][ADJ_STR_HIT]) - 128);

	/* Redraw armour (if needed) */
	if ((p_ptr->dis_ac != old_dis_ac) || (p_ptr->dis_to_a != old_dis_to_a))
	{
		/* Redraw */
		p_ptr->redraw |= (PR_ARMOR);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
	}

	/* Obtain the "hold" value */
	hold = adj_stat[p_ptr->stat_ind[A_STR]][ADJ_WEIGHT];

	/* Examine the "current bow" */
	o_ptr = &inventory[INVEN_BOW];

	/* Assume not heavy */
	p_ptr->heavy_shoot = FALSE;

	/* It is hard to carholdry a heavy bow */
	if (hold < o_ptr->weight / 10)
	{
		/* Hard to wield a heavy bow */
		p_ptr->to_h += 2 * (hold - o_ptr->weight / 10);
		p_ptr->dis_to_h += 2 * (hold - o_ptr->weight / 10);

		/* Heavy Bow */
		p_ptr->heavy_shoot = TRUE;
	}


	/* Compute "extra shots" if needed */
	if (o_ptr->k_idx && !p_ptr->heavy_shoot)
	{
		/* Take note of required "tval" for missiles */
		switch (o_ptr->sval)
		{
		case SV_SLING:
			{
				p_ptr->tval_ammo = TV_SHOT;
				break;
			}

		case SV_SHORT_BOW:
		case SV_LONG_BOW:
			{
				p_ptr->tval_ammo = TV_ARROW;
				break;
			}

		case SV_LIGHT_XBOW:
		case SV_HEAVY_XBOW:
			{
				p_ptr->tval_ammo = TV_BOLT;
				break;
			}
		}

		/* Hack -- Reward High Level Rangers using Bows */
		if (((p_ptr->pclass == 4) && (p_ptr->tval_ammo == TV_ARROW)))
		{
			/* Extra shot at level 20 */
			if (p_ptr->lev >= 20) p_ptr->num_fire++;

			/* Extra shot at level 40 */
			if (p_ptr->lev >= 40) p_ptr->num_fire++;
		}

		/* Addendum -- also "Reward" high level warriors,
		with _any_ missile weapon -- TY */
		if (p_ptr->pclass == CLASS_WARRIOR
			&& (p_ptr->tval_ammo <= TV_BOLT)
			&& (p_ptr->tval_ammo >= TV_SHOT))
		{
			/* Extra shot at level 25 */
			if (p_ptr->lev >= 25) p_ptr->num_fire++;

			/* Extra shot at level 50 */
			if (p_ptr->lev >= 50) p_ptr->num_fire++;
		}

		/* Add in the "bonus shots" */
		p_ptr->num_fire += extra_shots;

		/* Require at least one shot */
		if (p_ptr->num_fire < 1) p_ptr->num_fire = 1;
	}


	/* Examine the "main weapon" */
	o_ptr = &inventory[INVEN_WIELD];


	/* Assume not heavy */
	p_ptr->heavy_wield = FALSE;

	/* It is hard to hold a heavy weapon */
	if (hold < o_ptr->weight / 10)
	{
		/* Hard to wield a heavy weapon */
		p_ptr->to_h += 2 * (hold - o_ptr->weight / 10);
		p_ptr->dis_to_h += 2 * (hold - o_ptr->weight / 10);

		/* Heavy weapon */
		p_ptr->heavy_wield = TRUE;
	}


	/* Normal weapons */
	if (o_ptr->k_idx && !p_ptr->heavy_wield)
	{
		int str_index, dex_index;

		int num = 0, wgt = 0, mul = 0, div = 0;

		/* Analyze the class */
		switch (p_ptr->pclass)
		{
		case CLASS_WARRIOR: num = 6; wgt = 30; mul = 5; break;

		case CLASS_MAGE: case CLASS_HIGH_MAGE: case CLASS_WARLOCK: case CLASS_BLOOD_MAGE:
			num = 4; wgt = 40; mul = 2; break;

		case CLASS_PRIEST: case CLASS_ORPHIC: case CLASS_DRUID:
			num = 5; wgt = 35; mul = 3; break;

		case CLASS_ROGUE:   num = 5; wgt = 30; mul = 3; break;

		case CLASS_RANGER:  num = 5; wgt = 35; mul = 4; break;

		case CLASS_WARRIOR_MAGE: num = 5; wgt = 35; mul = 3; break;

		case CLASS_HELL_KNIGHT: case CLASS_PALADIN:case CLASS_BLACK_KNIGHT:case CLASS_CHAOS_KNIGHT:
			num = 5; wgt = 30; mul = 4; break;

		case CLASS_MYSTIC: num = (p_ptr->lev<40?3:4);
			wgt = 40; mul = 4; break;
		}

		/* Enforce a minimum "weight" (tenth pounds) */
		div = ((o_ptr->weight < wgt) ? wgt : o_ptr->weight);

		/* Access the strength vs weight */
		str_index = (adj_stat[p_ptr->stat_ind[A_STR]][ADJ_STR_BLOW] * mul / div);

		/* Maximal value */
		if (str_index > 11) str_index = 11;

		/* Index by dexterity */
		dex_index = (adj_stat[p_ptr->stat_ind[A_DEX]][ADJ_DEX_BLOW]);

		/* Maximal value */
		if (dex_index > 11) dex_index = 11;

		/* Use the blows table */
		p_ptr->num_blow = blows_table[str_index][dex_index];

		/* Maximal value */
		if (p_ptr->num_blow > num) p_ptr->num_blow = num;

		/* Add in the "bonus blows" */
		p_ptr->num_blow += extra_blows;

		/* Level bonus for warriors (1-3) */
		if (p_ptr->pclass == CLASS_WARRIOR) p_ptr->num_blow += (p_ptr->lev) / 15;

		/* Require at least one blow */
		if (p_ptr->num_blow < 1) p_ptr->num_blow = 1;


		/* Boost digging skill by weapon weight */
		p_ptr->skill_dig += (o_ptr->weight / 10);
	}


	/* Different calculation for mystics with empty hands */
	else if (p_ptr->pclass == CLASS_MYSTIC && mystic_empty_hands())
	{

		p_ptr->num_blow = 0;

		if (p_ptr->lev >  9) p_ptr->num_blow++;
		if (p_ptr->lev > 19) p_ptr->num_blow++;
		if (p_ptr->lev > 29) p_ptr->num_blow++;
		if (p_ptr->lev > 34) p_ptr->num_blow++;
		if (p_ptr->lev > 39) p_ptr->num_blow++;
		if (p_ptr->lev > 44) p_ptr->num_blow++;
		if (p_ptr->lev > 49) p_ptr->num_blow++;

		if (mystic_heavy_armour())
			p_ptr->num_blow /= 2;

		p_ptr->num_blow += 1 + extra_blows;

		if (!mystic_heavy_armour())
		{
			p_ptr->to_h += (p_ptr->lev / 3);
			p_ptr->to_d += (p_ptr->lev / 3);

			p_ptr->dis_to_h += (p_ptr->lev / 3);
			p_ptr->dis_to_d += (p_ptr->lev / 3);
		}
	}

	/* Assume okay */
	p_ptr->icky_wield = FALSE;
	mystic_armour_aux = FALSE;

	/* Extra bonus for warriors... */
	if (p_ptr->pclass == CLASS_WARRIOR)
	{
		p_ptr->to_h += (p_ptr->lev/5);
		p_ptr->to_d += (p_ptr->lev/5);

		p_ptr->dis_to_h += (p_ptr->lev/5);
		p_ptr->dis_to_d += (p_ptr->lev/5);
	}

	/* Priest weapon penalty for non-blessed edged weapons */
	if (((p_ptr->pclass == CLASS_PRIEST) || (p_ptr->pclass == CLASS_DRUID)) && (!p_ptr->bless_blade) &&
		((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)))
	{
		/* Reduce the real bonuses */
		p_ptr->to_h -= 2;
		p_ptr->to_d -= 2;

		/* Reduce the mental bonuses */
		p_ptr->dis_to_h -= 2;
		p_ptr->dis_to_d -= 2;

		/* Icky weapon */
		p_ptr->icky_wield = TRUE;
	}
	/* Weapon penalty for Warlocks if not using a blade of chaos*/
	if ((p_ptr->pclass == CLASS_WARLOCK) && (inventory[INVEN_WIELD].k_idx) &&
		((o_ptr->tval != TV_SWORD) || (o_ptr->sval != SV_BLADE_OF_CHAOS)))
	{
		u32b f1,f2,f3;
		/* 'Chaotic' weapons are allowed, and so are demonic ones */
		object_flags(o_ptr,&f1,&f2,&f3);
		if(!(f1 & TR1_CHAOTIC) && !(o_ptr->name2==EGO_DEMONIC))
		{
			/* Reduce the real bonuses */
			p_ptr->to_h -= 10;
			p_ptr->to_d -= 10;

			/* Reduce the mental bonuses */
			p_ptr->dis_to_h -= 10;
			p_ptr->dis_to_d -= 10;

			/* Icky weapon */
			p_ptr->icky_wield = TRUE;
		}
	}

	if (mystic_heavy_armour())
	{
		mystic_armour_aux = TRUE;
	}


	/* Affect Skill -- stealth (bonus one) */
	p_ptr->skill_stl += 1;

	/* Affect Skill -- disarming (DEX and INT) */
	p_ptr->skill_dis += adj_stat[p_ptr->stat_ind[A_DEX]][ADJ_DEX_TRAP];
	p_ptr->skill_dis += adj_stat[p_ptr->stat_ind[A_INT]][ADJ_INT_TRAP];

	/* Affect Skill -- magic devices (INT) */
	p_ptr->skill_dev += adj_stat[p_ptr->stat_ind[A_INT]][ADJ_DEVICE];

	/* Affect Skill -- saving throw (WIS) */
	p_ptr->skill_sav += adj_stat[p_ptr->stat_ind[A_WIS]][ADJ_RESIST];

	/* Affect Skill -- digging (STR) */
	p_ptr->skill_dig += adj_stat[p_ptr->stat_ind[A_STR]][ADJ_DIG];

	/* Affect Skill -- disarming (Level, by Class) */
	p_ptr->skill_dis += (cp_ptr->x_dis * p_ptr->lev / 10);

	/* Affect Skill -- magic devices (Level, by Class) */
	p_ptr->skill_dev += (cp_ptr->x_dev * p_ptr->lev / 10);

	/* Affect Skill -- saving throw (Level, by Class) */
	p_ptr->skill_sav += (cp_ptr->x_sav * p_ptr->lev / 10);

	/* Affect Skill -- stealth (Level, by Class) */
	p_ptr->skill_stl += (cp_ptr->x_stl * p_ptr->lev / 10);

	/* Affect Skill -- search ability (Level, by Class) */
	p_ptr->skill_srh += (cp_ptr->x_srh * p_ptr->lev / 10);

	/* Affect Skill -- search frequency (Level, by Class) */
	p_ptr->skill_fos += (cp_ptr->x_fos * p_ptr->lev / 10);

	/* Affect Skill -- combat (normal) (Level, by Class) */
	p_ptr->skill_thn += (cp_ptr->x_thn * p_ptr->lev / 10);

	/* Affect Skill -- combat (shooting) (Level, by Class) */
	p_ptr->skill_thb += (cp_ptr->x_thb * p_ptr->lev / 10);

	/* Affect Skill -- combat (throwing) (Level, by Class) */
	p_ptr->skill_tht += (cp_ptr->x_thb * p_ptr->lev / 10);

	/* Limit Skill -- stealth from 0 to 30 */
	if (p_ptr->skill_stl > 30) p_ptr->skill_stl = 30;
	if (p_ptr->skill_stl < 0) p_ptr->skill_stl = 0;

	/* Limit Skill -- digging from 1 up */
	if (p_ptr->skill_dig < 1) p_ptr->skill_dig = 1;

	if ((p_ptr->anti_magic) && (p_ptr->skill_sav < 95))
		p_ptr->skill_sav = 95;

	/* Hack -- handle "xtra" mode */
	if (character_xtra) return;

	/* Take note when "heavy bow" changes */
	if (p_ptr->old_heavy_shoot != p_ptr->heavy_shoot)
	{
		/* Message */
		if (p_ptr->heavy_shoot)
		{
			msg_print("You have trouble wielding such a heavy bow.");
		}
		else if (inventory[INVEN_BOW].k_idx)
		{
			msg_print("You have no trouble wielding your bow.");
		}
		else
		{
			msg_print("You feel relieved to put down your heavy bow.");
		}

		/* Save it */
		p_ptr->old_heavy_shoot = p_ptr->heavy_shoot;
	}


	/* Take note when "heavy weapon" changes */
	if (p_ptr->old_heavy_wield != p_ptr->heavy_wield)
	{
		/* Message */
		if (p_ptr->heavy_wield)
		{
			msg_print("You have trouble wielding such a heavy weapon.");
		}
		else if (inventory[INVEN_WIELD].k_idx)
		{
			msg_print("You have no trouble wielding your weapon.");
		}
		else
		{
			msg_print("You feel relieved to put down your heavy weapon.");
		}

		/* Save it */
		p_ptr->old_heavy_wield = p_ptr->heavy_wield;
	}


	/* Take note when "illegal weapon" changes */
	if (p_ptr->old_icky_wield != p_ptr->icky_wield)
	{
		/* Message */
		if (p_ptr->icky_wield)
		{
			if(p_ptr->pclass == CLASS_WARLOCK)
			{
				msg_print("Your weapon restricts the flow of chaos through you.");
			}
			else
			{
				msg_print("You do not feel comfortable with your weapon.");
			}
		}
		else if (inventory[INVEN_WIELD].k_idx)
		{
			msg_print("You feel comfortable with your weapon.");
		}
		else
		{
			if(p_ptr->pclass == CLASS_WARLOCK)
			{
				msg_print("You feel aligned with your patron again.");
			}
			else
			{
				msg_print("You feel more comfortable after removing your weapon.");
			}
		}
		/* Save it */
		p_ptr->old_icky_wield = p_ptr->icky_wield;
	}

	if (p_ptr->pclass == CLASS_MYSTIC && (mystic_armour_aux != mystic_notify_aux))

	{
		if (mystic_heavy_armour())
			msg_print("The weight of your armour disrupts your balance.");
		else
			msg_print("You regain your balance.");
		mystic_notify_aux = mystic_armour_aux;
	}

}



/*
* Handle "p_ptr->notice"
*/
void notice_stuff(void)
{
	/* Notice stuff */
	if (!p_ptr->notice) return;


	/* Combine the pack */
	if (p_ptr->notice & (PN_COMBINE))
	{
		p_ptr->notice &= ~(PN_COMBINE);
		combine_pack();
	}

	/* Reorder the pack */
	if (p_ptr->notice & (PN_REORDER))
	{
		p_ptr->notice &= ~(PN_REORDER);
		reorder_pack();
	}
}


/*
* Handle "p_ptr->update"
*/
void update_stuff(void)
{
	/* Update stuff */
	if (!p_ptr->update) return;

	if (p_ptr->update & (PU_BONUS))
	{
		p_ptr->update &= ~(PU_BONUS);
		calc_bonuses();
	}

	if (p_ptr->update & (PU_TORCH))
	{
		p_ptr->update &= ~(PU_TORCH);
		calc_torch();
	}

	if (p_ptr->update & (PU_MANA))
	{
		p_ptr->update &= ~(PU_MANA);
		calc_mana();
	}

	if (p_ptr->update & (PU_HP))
	{
		p_ptr->update &= ~(PU_HP);
		calc_hitpoints();
	}

	if (p_ptr->update & (PU_SPELLS))
	{
		p_ptr->update &= ~(PU_SPELLS);
		calc_spells();
	}

	/* Character is not ready yet, no screen updates */
	if (!character_generated) return;

	/* Character is in "icky" mode, no screen updates */
	if (character_icky) return;

	if (p_ptr->update & (PU_UN_LITE))
	{
		p_ptr->update &= ~(PU_UN_LITE);
		forget_lite();
	}

	if (p_ptr->update & (PU_UN_VIEW))
	{
		p_ptr->update &= ~(PU_UN_VIEW);
		forget_view();
	}

	if (p_ptr->update & (PU_VIEW))
	{
		p_ptr->update &= ~(PU_VIEW);
		update_view();
	}

	if (p_ptr->update & (PU_LITE))
	{
		p_ptr->update &= ~(PU_LITE);
		update_lite();
	}

	if (p_ptr->update & (PU_FLOW))
	{
		p_ptr->update &= ~(PU_FLOW);
		update_flow();
	}

	if (p_ptr->update & (PU_DISTANCE))
	{
		p_ptr->update &= ~(PU_DISTANCE);
		p_ptr->update &= ~(PU_MONSTERS);
		update_monsters(TRUE);
	}

	if (p_ptr->update & (PU_MONSTERS))
	{
		p_ptr->update &= ~(PU_MONSTERS);
		update_monsters(FALSE);
	}
}


/*
* Handle "p_ptr->redraw"
*/
void redraw_stuff(void)
{
	/* Redraw stuff */
	if (!p_ptr->redraw) return;

	/* Character is not ready yet, no screen updates */
	if (!character_generated) return;

	/* Character is in "icky" mode, no screen updates */
	if (character_icky) return;

	/* Elder Hack */
	if (p_ptr->redraw & (PR_MISC))
	{
		p_ptr->redraw &= ~(PR_MISC);
	}

	/* Hack -- clear the screen */
	if (p_ptr->redraw & (PR_WIPE))
	{
		p_ptr->redraw &= ~(PR_WIPE);
		msg_print(NULL);
		Term_clear();
	}

	if (p_ptr->redraw & (PR_MAP))
	{
		p_ptr->redraw &= ~(PR_MAP);
		p_ptr->redraw &= ~(PR_HEALTH);
		p_ptr->redraw &= ~(PR_DEPTH);
		prt_map();
		health_redraw();
		prt_depth();
	}

	/* New and improved */
	if (
		p_ptr->redraw & (PR_BASIC) ||
		p_ptr->redraw & (PR_TITLE) ||
		p_ptr->redraw & (PR_LEV) ||
		p_ptr->redraw & (PR_EXP) ||
		p_ptr->redraw & (PR_ARMOR) ||
		p_ptr->redraw & (PR_HP) ||
		p_ptr->redraw & (PR_MANA) ||
		p_ptr->redraw & (PR_GOLD) ||
		p_ptr->redraw & (PR_DEPTH) ||
		p_ptr->redraw & (PR_EXTRA) ||
		p_ptr->redraw & (PR_CUT) ||
		p_ptr->redraw & (PR_STUN) ||
		p_ptr->redraw & (PR_HUNGER) ||
		p_ptr->redraw & (PR_BLIND) ||
		p_ptr->redraw & (PR_CONFUSED) ||
		p_ptr->redraw & (PR_AFRAID) ||
		p_ptr->redraw & (PR_POISONED) ||
		p_ptr->redraw & (PR_STATE) ||
		p_ptr->redraw & (PR_SPEED) ||
		p_ptr->redraw & (PR_STUDY)
	   )
	{
		p_ptr->redraw &= ~(PR_BASIC);
		p_ptr->redraw &= ~(PR_MISC | PR_TITLE );
		p_ptr->redraw &= ~(PR_LEV | PR_EXP | PR_GOLD);
		p_ptr->redraw &= ~(PR_ARMOR | PR_HP | PR_MANA);
		p_ptr->redraw &= ~(PR_DEPTH);
		p_ptr->redraw &= ~(PR_EXTRA);
		p_ptr->redraw &= ~(PR_CUT | PR_STUN);
		p_ptr->redraw &= ~(PR_HUNGER);
		p_ptr->redraw &= ~(PR_BLIND | PR_CONFUSED);
		p_ptr->redraw &= ~(PR_AFRAID | PR_POISONED);
		p_ptr->redraw &= ~(PR_STATE | PR_SPEED | PR_STUDY);
		prt_status();
		prt_depth();
	}

	if (p_ptr->redraw & (PR_HEALTH))
	{
		p_ptr->redraw &= ~(PR_HEALTH);
		health_redraw();
	}
}


/*
* Handle "p_ptr->window"
*/
void window_stuff(void)
{
	int i,j;

	u32b mask = 0L;
	u32b flag = 1;


	/* Nothing to do */
	if (!p_ptr->window) return;

	/* Scan windows */
	for (j = 0; j < 8; j++)
	{
		/* Save usable flags */
		if (angband_term[j]) mask |= window_flag[j];
	}

	/* Apply usable flags */
 	p_ptr->window &= mask;

	/* Nothing to do */
	if (!p_ptr->window) return;

	/* Basicaly we loop through the bitflags, by starting
	   with 1 and shifting to the left ( aka multiplying by 2 )
	   this should go over all the flags, since the treatment of each flag
	   is 95% percent I condensed the difference in a big if statement
	*/
	for( i = 0 ; i < PW_FLAG_COUNT ; i++ ){
		if (p_ptr->window & (flag))
		{
			p_ptr->window &= ~(flag);
			fix_term_type( flag );
		}
		flag = flag << 1;
	}
}


/*
* Handle "p_ptr->update" and "p_ptr->redraw" and "p_ptr->window"
*/
void handle_stuff(void)
{
	/* Update stuff */
	if (p_ptr->update) update_stuff();

	/* Redraw stuff */
	if (p_ptr->redraw) redraw_stuff();

	/* Window stuff */
	if (p_ptr->window) window_stuff();
}


bool mystic_empty_hands()
{
	if (!(p_ptr->pclass == CLASS_MYSTIC))
		return FALSE;

	return !(inventory[INVEN_WIELD].k_idx);
}

bool mystic_heavy_armour()
{
	u16b mystic_arm_wgt = 0;
	/* Weigh the armour */
	if (!(p_ptr->pclass == CLASS_MYSTIC))
		return FALSE;

	mystic_arm_wgt += inventory[INVEN_BODY].weight;
	mystic_arm_wgt += inventory[INVEN_HEAD].weight;
	mystic_arm_wgt += inventory[INVEN_ARM].weight;
	mystic_arm_wgt += inventory[INVEN_OUTER].weight;
	mystic_arm_wgt += inventory[INVEN_HANDS].weight;
	mystic_arm_wgt += inventory[INVEN_FEET].weight;

	return (mystic_arm_wgt > ( 100 + (p_ptr->lev * 4))) ;
}

