/* File: h-spell.c */

/*
 * Copyright (c) 2001 Nathan Brown, Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"


#define HIGH_ELEC_BOLT		0
#define HIGH_ELEC_BALL		1
#define HIGH_ELEC_SWARM		2
#define HIGH_LIGHT_AREA		3
#define HIGH_LIGHT_BEAM		4
#define HIGH_LIGHT_FLOOD	5
#define HIGH_DARK_BOLT		6
#define HIGH_DARK_BALL		7
#define HIGH_DARK_FLOOD		8

#define HIGH_FIRE_BOLT		9
#define HIGH_FIRE_BALL		10
#define HIGH_FIRE_SWARM		11
#define HIGH_COLD_BOLT		12
#define HIGH_COLD_BALL		13
#define HIGH_COLD_SWARM		14
#define HIGH_PLASMA_FLOOD	15
#define HIGH_ICE_BEAM		16

#define HIGH_ACID_BOLT		17
#define HIGH_ACID_BEAM		18
#define HIGH_ACID_BALL		19
#define HIGH_ACID_FLOOD		20
#define HIGH_STONE_MUD		21
#define HIGH_SLOW_BOLT		22
#define HIGH_SLOW_BALL		23

#define HIGH_MAP_AREA		24
#define HIGH_MAP_ALL		25
#define HIGH_DET_FEAT		26
#define HIGH_SHARD_BOLT		27
#define HIGH_SHARD_BALL		28
#define HIGH_FORCE_BOLT		29
#define HIGH_FORCE_SWARM	30

#define HIGH_HEAL			31
#define HIGH_CURE			32
#define HIGH_RESTORE		33
#define HIGH_FOOD			34
#define HIGH_POIS_BOLT		35
#define HIGH_POIS_BALL		36
#define HIGH_SPEED			37
#define HIGH_HEROISM		38

#define HIGH_BLINK			39
#define HIGH_RECALL			42
#define HIGH_TIME_BOLT		43
#define HIGH_TIME_BALL		44
#define HIGH_GRAV_BOLT		45
#define HIGH_GRAV_BALL		46
#define HIGH_GRAV_SWARM		47
#define HIGH_NEXUS_FLOOD	48

#define HIGH_MANA_BOLT		49
#define HIGH_MANA_BEAM		50
#define HIGH_MANA_BALL		51
#define HIGH_DISEN_FLOOD	52
#define HIGH_CHAOS_SWARM	53
#define HIGH_ENCH_WEAP		54
#define HIGH_ENCH_ARMOR		55
#define HIGH_RECHARGE		56

#define BOOK1(x) (((x) < 0) ? 0 : (x) < 32 ? (1L << (x)) : 0)
#define BOOK2(x) (((x) < 0) ? 0 : (x) < 32 ? 0 : (1L << ((x) % 32)))

#define BOOK(a, b, c, d, e, f, g, h, i) \
{ \
	(BOOK1(a) | BOOK1(b) | BOOK1(c) | BOOK1(d) | BOOK1(e) | \
	 BOOK1(f) | BOOK1(g) | BOOK1(h) | BOOK1(i)), \
	(BOOK2(a) | BOOK2(b) | BOOK2(c) | BOOK2(d) | BOOK2(e) | \
	 BOOK2(f) | BOOK2(g) | BOOK2(h) | BOOK2(i)) \
}

const u32b h_spell_group[10][2] =
{
	/* Darkness and Light */
	BOOK(HIGH_ELEC_BOLT,
		 HIGH_ELEC_BALL,
		 HIGH_ELEC_SWARM,
		 HIGH_LIGHT_AREA,
		 HIGH_LIGHT_BEAM,
		 HIGH_LIGHT_FLOOD,
		 HIGH_DARK_BOLT,
		 HIGH_DARK_BALL,
		 HIGH_DARK_FLOOD),

	/* Fire and Cold */
	BOOK(HIGH_FIRE_BOLT,
		 HIGH_FIRE_BALL,
		 HIGH_FIRE_SWARM,
		 HIGH_COLD_BOLT,
		 HIGH_COLD_BALL,
		 HIGH_COLD_SWARM,
		 HIGH_PLASMA_FLOOD,
		 HIGH_ICE_BEAM,
		 -1),

	/* Water spells */
	BOOK(HIGH_ACID_BOLT,
		 HIGH_ACID_BEAM,
		 HIGH_ACID_BALL,
		 HIGH_ACID_FLOOD,
		 HIGH_STONE_MUD,
		 HIGH_SLOW_BOLT,
		 HIGH_SLOW_BALL,
		 -1,
		 -1),
	
	/* Secrets of Earth */		 
	BOOK(HIGH_MAP_AREA,
		 HIGH_MAP_ALL,
		 HIGH_DET_FEAT,
		 HIGH_SHARD_BOLT,
		 HIGH_SHARD_BALL,
		 HIGH_FORCE_BOLT,
		 HIGH_FORCE_SWARM,
		 -1,
		 -1),

	/* Life Forces */
	BOOK(HIGH_HEAL,
		 HIGH_CURE,
		 HIGH_RESTORE,
		 HIGH_FOOD,
		 HIGH_POIS_BOLT,
		 HIGH_POIS_BALL,
		 HIGH_SPEED,
		 HIGH_HEROISM,
		 -1),

	/* Time and Space */
	BOOK(HIGH_BLINK,
		 HIGH_RECALL,
		 HIGH_TIME_BOLT,
		 HIGH_TIME_BALL,
		 HIGH_GRAV_BOLT,
		 HIGH_GRAV_BALL,
		 HIGH_GRAV_SWARM,
		 HIGH_NEXUS_FLOOD,
		 -1),

	/* Pure Magic */
	BOOK(HIGH_MANA_BOLT,
		 HIGH_MANA_BEAM,
		 HIGH_MANA_BALL,
		 HIGH_DISEN_FLOOD,
		 HIGH_CHAOS_SWARM,
		 HIGH_ENCH_WEAP,
		 HIGH_ENCH_ARMOR,
		 HIGH_RECHARGE,
		 -1)
};

cptr realm_names[HREALM_MAX] = 
{
	"Light/Dark",
	"Fire/Cold",
	"Water",
	"Earth",
	"Life",
	"Time/Space",
	"Pure Magic"
};

const magic_type high_info[PY_MAX_SPELLS] = 
{
	/*{lev,mana,fail,exp}*/
	{5,1,10,1},
	{10,2,20,2},
	{15,3,30,3},
	{1,1,5,1},
	{7,1,12,2},
	{15,3,30,3},
	{5,1,10,1},
	{10,2,20,2},
	{15,3,30,3},

	{5,1,10,1},
	{10,2,20,2},
	{15,3,30,3},
	{5,1,10,1},
	{10,2,20,2},
	{15,3,30,3},
	{20,4,40,4},
	{7,2,12,1},

	{5,1,10,1},
	{7,2,12,1},
	{10,2,20,2},
	{15,3,30,3},
	{2,2,5,1},
	{5,1,10,1},
	{10,2,20,2},

	{10,2,20,2},
	{20,4,40,4},
	{10,2,20,2},
	{5,1,10,1},
	{10,2,20,2},
	{5,1,10,1},
	{15,3,30,3},

	{1,1,15,1},
	{10,2,20,2},
	{20,10,40,4},
	{10,5,20,2},
	{5,1,10,1},
	{10,2,20,2},
	{10,2,20,2},
	{10,2,20,2},

	{1,1,15,1},
	{20,7,30,3},
	{5,1,10,1},
	{10,2,20,2},
	{7,2,15,1},
	{12,3,20,2},
	{18,4,25,3},
	{15,3,30,3},

	{5,2,5,1},
	{10,3,5,2},
	{15,4,15,3},
	{20,4,30,3},
	{20,5,40,4},
	{25,10,50,5},
	{25,10,50,5},
	{15,2,25,2},
};


static cptr h_spell_names[PY_MAX_SPELLS] = 
{
	"Lightning Bolt",
	"Ball Lightning",
	"Lightning Storm",
	"Illumination",
	"Spear of Light",
	"Flare",
	"Shadow Bolt",
	"Shade Strike",
	"Void Strike",

	"Fire Bolt",
	"Fire Ball",
	"Fire Storm",
	"Cold Bolt",
	"Cold Ball",
	"Chill Storm",
	"Plasma Flow",
	"Spear of Ice",

	"Acid Bolt",
	"Water Spear",
	"Acid Ball",
	"Flood",
	"Dissonve Wall",
	"Enmire",
	"Quicksand",

	"Magic Map",
	"Clairvoyance",
	"Detect Traps/Doors",
	"Shard Bolt",
	"Shard Strike",
	"Force Bolt",
	"Force Storm",

	"Heal",
	"Cure",
	"Restoration",
	"Create Food",
	"Venom",
	"Stinking Cloud",
	"Speed",
	"Heroism",

	"Blink",
	"Recall",
	"Time Bolt",
	"Temporal Rift",
	"Gravity Bolt",
	"Gravity Strike",
	"Gravity Storm",
	"Teleport Storm",

	"Power Bolt",
	"Power Spear",
	"Mana Ball",
	"Null Flood",
	"Chaos Storm",
	"Enchant Weapon",
	"Enchant Armor",
	"Recharge Item",
};

cptr get_h_spell_name(int spell)
{
	return h_spell_names[spell];
}



bool cast_high(int spell, int arg)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int dir;

	int plev = p_ptr->lev;
	int beam;

	/* arg = get_quantity("at what level?", p_ptr->lev); */

	/* Hack -- chance of "beam" instead of "bolt" */
	beam = beam_chance();

	/* Spells. */
	switch (spell)
	{
		case HIGH_ELEC_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam+10, GF_ELEC, dir, damroll(arg, plev));
			break;
		}
		case HIGH_ELEC_BALL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_ELEC, dir, damroll(arg + plev/5,6),arg);
			break;
		}
		case HIGH_ELEC_SWARM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_swarm(arg, GF_ELEC, dir, damroll(plev/10,6),2+arg/5);
			break;
		}
		case HIGH_LIGHT_AREA:
		{
			(void)lite_area(damroll(2, (plev / 2)), arg);
			break;
		}
		case HIGH_LIGHT_BEAM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_beam(GF_LITE, dir, arg + plev);
		}
		case HIGH_LIGHT_FLOOD:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_flood(arg, GF_LITE, dir, plev);
			break;
		}
		case HIGH_DARK_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam, GF_DARK, dir, arg * plev);
			break;
		}
		case HIGH_DARK_BALL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_DARK, dir, rand_int(plev), arg);
			break;
		}
		case HIGH_DARK_FLOOD:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_flood(arg, GF_DARK, dir, plev);
		}
		case HIGH_FIRE_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam, GF_FIRE, dir, damroll(arg, plev));
			break;
		}
		case HIGH_FIRE_BALL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_FIRE, dir, damroll(arg + plev/5,6),arg);
			break;
		}
		case HIGH_FIRE_SWARM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_swarm(arg, GF_FIRE, dir, damroll(plev/10,6),2+arg/5);
			break;
		}
		case HIGH_COLD_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam, GF_COLD, dir, damroll(arg, plev));
			break;
		}
		case HIGH_COLD_BALL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_COLD, dir, damroll(arg + plev/5,6),arg);
			break;
		}
		case HIGH_COLD_SWARM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_swarm(arg, GF_COLD, dir, damroll(plev/5,6),2+arg/5);
			break;
		}
		case HIGH_PLASMA_FLOOD:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_flood(arg, GF_PLASMA, dir, plev);
			break;
		}
		case HIGH_ICE_BEAM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_beam(GF_ICE, dir, damroll(arg, 6));
			break;
		}
		case HIGH_ACID_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam, GF_ACID, dir, damroll(arg, plev));
			break;
		}
		case HIGH_ACID_BEAM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_beam(GF_ACID, dir, damroll(arg, 3));
			break;
		}
		case HIGH_ACID_BALL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_ACID, dir, damroll(arg + plev/5,6),arg);
			break;
		}
		case HIGH_ACID_FLOOD:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_flood(arg, GF_ACID, dir, plev/10);
			break;
		}
		case HIGH_STONE_MUD:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			(void)wall_to_mud(dir);
			break;
		}
		case HIGH_SLOW_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam, GF_OLD_SLOW, dir, arg * plev);
			break;
		}
		case HIGH_SLOW_BALL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_OLD_SLOW, dir, (arg + plev/5)*6,arg);
			break;
		}
		case HIGH_MAP_AREA:
		{
			map_area();
			break;
		}
		case HIGH_MAP_ALL:
		{
			wiz_lite();
			break;
		}
		case HIGH_DET_FEAT:
		{
			(void)detect_traps();
			(void)detect_doors();
			(void)detect_stairs();
			break;
		}
		case HIGH_SHARD_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam, GF_SHARD, dir, damroll(arg, plev));
			break;
		}
		case HIGH_SHARD_BALL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_SHARD, dir, damroll(arg + plev/5,6),arg);
			break;
		}
		case HIGH_FORCE_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam, GF_FORCE, dir, damroll(arg, plev));
			break;
		}
		case HIGH_FORCE_SWARM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_swarm(arg, GF_FORCE, dir, damroll(plev/10,6),2+arg/5);
			break;
		}
		case HIGH_HEAL:
		{
			(void)hp_player(damroll(arg, (2+plev/5)));
			(void)set_stun(0);
			(void)set_cut(0);
			break;
		}
		case HIGH_CURE:
		{
			(void)hp_player(arg * 10);
			(void)set_afraid(0);
			(void)set_poisoned(0);
			(void)set_stun(0);
			(void)set_cut(0);
			(void)set_blind(0);
		}
		case HIGH_RESTORE:
		{
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_CHR);
			break;
		}
		case HIGH_FOOD:
		{
			(void)set_food(PY_FOOD_MAX - 1);
			break;
		}
		case HIGH_POIS_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam, GF_POIS, dir, damroll(arg, plev));
			break;
		}
		case HIGH_POIS_BALL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_POIS, dir, damroll(arg + plev/5,6),arg);
			break;
		}
		case HIGH_SPEED:
		{
			if (!p_ptr->fast)
			{
				(void)set_fast(arg * plev/10);
			}
			else
			{
				(void)set_fast(p_ptr->fast + arg);
			}
			break;
		}
		case HIGH_HEROISM:
		{
			(void)hp_player(10);
			(void)set_hero(p_ptr->hero + arg * plev);
			(void)set_afraid(0);
			break;
		}
		case HIGH_BLINK:
		{
			teleport_player(arg);
			break;
		}
		case HIGH_RECALL:
		{
			set_recall();
			break;
		}
		case HIGH_TIME_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam, GF_TIME, dir, damroll(arg, plev));
			break;
		}
		case HIGH_TIME_BALL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_TIME, dir, damroll(arg + plev/5,6),arg);
			break;
		}
		case HIGH_GRAV_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam, GF_GRAVITY, dir, damroll(arg, plev));
			break;
		}
		case HIGH_GRAV_BALL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_GRAVITY, dir, damroll(arg + plev/5,6),arg);
			break;
		}
		case HIGH_GRAV_SWARM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_swarm(arg, GF_GRAVITY, dir, damroll(plev/10,6),2+arg/5);
			break;
		}
		case HIGH_NEXUS_FLOOD:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_flood(arg, GF_NEXUS, dir, plev);
			break;
		}
		case HIGH_MANA_BOLT:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_bolt_or_beam(beam-10, GF_MANA, dir, damroll(arg, plev));
			break;
		}
		case HIGH_MANA_BEAM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_beam(GF_MANA, dir, arg);
			break;
		}
		case HIGH_MANA_BALL:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_ball(GF_MANA, dir, damroll(arg + plev/5,6),arg);
			break;
		}
		case HIGH_DISEN_FLOOD:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_flood(arg, GF_DISENCHANT, dir, plev);
			break;
		}
		case HIGH_CHAOS_SWARM:
		{
			if (!get_aim_dir(&dir)) return (FALSE);
			fire_swarm(arg, GF_CHAOS, dir, damroll(plev/10,6),2+arg/5);
			break;
		}
		case HIGH_ENCH_WEAP:
		{
			(void)enchant_spell(arg, arg + plev, 0);
			break;
		}
		case HIGH_ENCH_ARMOR:
		{
			(void)enchant_spell(0, 0, 2 * arg + plev);
			break;
		}
		case HIGH_RECHARGE:
		{
			(void)recharge(arg * plev);
			break;
		}
	}

	/* Success */
	return (TRUE);
}

void print_realms(int y, int x)
{
	int i;

	cptr comment;

	char out_val[160];

	byte line_attr;

	/* Title the list */
	prt("", y, x);
	put_str("Name", y, x + 5);
	put_str("Info", y, x + 48);

	/* Dump the spells */
	for (i = 0; i < HREALM_MAX; i++)
	{
		
		/* Get extra info */
		comment = " unknown";
		line_attr = TERM_L_BLUE;

		if ((p_ptr->realm1 == i)||(p_ptr->realm2 == i)||(p_ptr->realm3 == i))
		{
			comment = "   known";
			line_attr = TERM_WHITE;
		}

		/* Dump the spell --(-- */
		sprintf(out_val, "  %c) %-42s%s",
		        I2A(i), realm_names[i], comment);
		c_prt(line_attr, out_val, y + i + 1, x);
	}

	/* Clear the bottom line */
	prt("", y + i + 1, x);
}

void get_new_realm()
{
	char index;
	bool redraw = FALSE;
	bool done = FALSE;
	while(!done && get_com("which realm? hit space, *, or ? for list", &index))
	{
		/* Request redraw */
		if ((index == ' ') || (index == '*') || (index == '?'))
		{
			/* Hide the list */
			if (redraw)
			{
				/* Load screen */
				screen_load();
				/* Hide list */
				redraw = FALSE;
			}
			/* Show the list */
			else
			{
				/* Show list */
				redraw = TRUE;
				/* Save screen */
				screen_save();
				/* Display a list of spells */
				print_realms( 1, 20);
			}
			/* Ask again */
			continue;
		}
		if(!p_ptr->realm1)
		{
			if (A2I(index)>= 7 || A2I(index) < 0)  break;
			p_ptr->realm1 = A2I(index);
			msg_print("You now know one realm.");
			done = (TRUE);
		}
		else if (A2I(index) == p_ptr->realm1)
		{
			msg_print("You already know that realm.");
			done = (FALSE);
		}
		else if(!p_ptr->realm2)
		{
			if (A2I(index)>= 7 || A2I(index) < 0)  break;
			p_ptr->realm2 = A2I(index);
			msg_print("You now know two realms.");
			done = (TRUE);
		}
		else if (A2I(index) == p_ptr->realm2)
		{
			msg_print("You already know that realm.");
			done = (FALSE);
		}
		else if(!p_ptr->realm3)
		{
			if (A2I(index)>= 7 || A2I(index) < 0)  break;
			p_ptr->realm3 = A2I(index);
			msg_print("You now know three realms.");
			done = (TRUE);
		}
		else if (A2I(index) == p_ptr->realm3)
		{
			msg_print("You already know that realm.");
			done = (FALSE);
		}
		else if (p_ptr->realm1 && p_ptr->realm2 && p_ptr->realm3)
		{
			msg_print("you already know three realms of high magic.");
			done = (TRUE);
		}
	}
	if (redraw) screen_load();
	return;
}

cptr get_h_spel_info(spell)
{
	static char p[80];

	int plev = p_ptr->lev;

	/* Default */
	strcpy(p, "");

	/* Analyze the spell */
	switch (spell)
	{
	case HIGH_ELEC_BOLT:
		sprintf(p, " dam #d%d", plev);
		break;
	case HIGH_ELEC_BALL:
		sprintf(p, " dam #+%dd6 10", plev/5);
		break;
	case HIGH_ELEC_SWARM:
		sprintf(p, " dam %dd6", plev/10);
		break;
	case HIGH_LIGHT_AREA:
		sprintf(p, " rad #");
		break;
	case HIGH_LIGHT_BEAM:
		sprintf(p, " dam # +%d", plev);
		break;
	case HIGH_LIGHT_FLOOD:
		sprintf(p, " dam %d", plev);
		break;
	case HIGH_DARK_BOLT:
		sprintf(p, " dam (%dx#)", plev);
		break;
	case HIGH_DARK_BALL:
		sprintf(p, " dam d%d", plev);
		break;
	case HIGH_DARK_FLOOD:
		sprintf(p, " dam %d (x#)", plev);
		break;
	case HIGH_FIRE_BOLT:
		sprintf(p, " dam #d%d", plev);
		break;
	case HIGH_FIRE_BALL:
		sprintf(p, " dam (#+%d)d6", plev/5);
		break;
	case HIGH_FIRE_SWARM:
		sprintf(p, " dam %dd6 (x#)", plev/10);
		break;
	case HIGH_COLD_BOLT:
		sprintf(p, " dam #d%d", plev);
		break;
	case HIGH_COLD_BALL:
		sprintf(p, " dam (#+%d)d6", plev/5);
		break;
	case HIGH_COLD_SWARM:
		sprintf(p, " dam %dd6 (x#)", plev/10);
		break;
	case HIGH_PLASMA_FLOOD:
		sprintf(p, " dam %d (x#)", plev);
		break;
	case HIGH_ICE_BEAM:
		sprintf(p, " dam #d%d6");
		break;
	case HIGH_ACID_BOLT:
		sprintf(p, " dam #d%d", plev);
		break;
	case HIGH_ACID_BALL:
		sprintf(p, " dam (#+%d)d6", plev/5);
		break;
	case HIGH_ACID_FLOOD:
		sprintf(p, " dam %dd6 (x#)", plev/10);
		break;
	case HIGH_STONE_MUD:
		sprintf(p, "");
		break;
	case HIGH_SLOW_BOLT:
		sprintf(p, " pow %d#", plev);
		break;
	case HIGH_SLOW_BALL:
		sprintf(p, " pow 6(#+%d)", plev/5);
		break;
	case HIGH_MAP_AREA:
		sprintf(p, "");
		break;
	case HIGH_MAP_ALL:
		sprintf(p, "");
		break;
	case HIGH_DET_FEAT:
		sprintf(p, " detect ^+<>");
		break;
	case HIGH_SHARD_BOLT:
		sprintf(p, " dam #d%d", plev);
		break;
	case HIGH_SHARD_BALL:
		sprintf(p, " dam #+%dd6", plev/5);
		break;
	case HIGH_FORCE_BOLT:
		sprintf(p, " dam #d%d (x#)", plev);
		break;
	case HIGH_FORCE_SWARM:
		sprintf(p, " dam %dd6 (x#)", plev/10);
		break;
	case HIGH_HEAL:
		sprintf(p, " heal #d%d", 2+plev/5);
		break;
	case HIGH_CURE:
		sprintf(p, " heal 10#");
		break;
	case HIGH_RESTORE:
		sprintf(p, "");
		break;	
	case HIGH_FOOD:
		sprintf(p, "");
		break;
	case HIGH_POIS_BOLT:
		sprintf(p, " dam #d%d", plev);
		break;
	case HIGH_POIS_BALL:
		sprintf(p, " dam #+%dd6", plev/5);
		break;
	case HIGH_SPEED:
		sprintf(p, " dur #x%d", plev/10);
		break;
	case HIGH_HEROISM:
		sprintf(p, " dur #x%d", plev);
		break;
	case HIGH_BLINK:
		sprintf(p, " range #");
		break;
	case HIGH_RECALL:
		sprintf(p, "");
		break;
	case HIGH_TIME_BOLT:
		sprintf(p, " dam #d%d", plev);
		break;
	case HIGH_TIME_BALL:
		sprintf(p, " dam #+%dd6", plev/5);
		break;
	case HIGH_GRAV_BOLT:
		sprintf(p, " dam #d%d", plev);
		break;
	case HIGH_GRAV_BALL:
		sprintf(p, " dam #+%dd6", plev/5);
		break;
	case HIGH_GRAV_SWARM:
		sprintf(p, " dam %dd6 (x#)", plev/10);
		break;
	case HIGH_NEXUS_FLOOD:
		sprintf(p, " dam %d (x#)", plev);
		break;
	case HIGH_MANA_BOLT:
		sprintf(p, " dam #d%d", plev);
		break;
	case HIGH_MANA_BEAM:
		sprintf(p, " dam #");
		break;
	case HIGH_MANA_BALL:
		sprintf(p, " dam #+%dd6", plev/5);
		break;
	case HIGH_DISEN_FLOOD:
		sprintf(p, " dam %d (x#)", plev);
		break;
	case HIGH_CHAOS_SWARM:
		sprintf(p, " dam %dd6 (x#)", plev/10);
		break;
	case HIGH_ENCH_WEAP:
		sprintf(p, " max (+#,+%d+#)", plev);
		break;	
	case HIGH_ENCH_ARMOR:
		sprintf(p, " max [+%d+2x#]", plev);
		break;	
	case HIGH_RECHARGE:
		sprintf(p, " pow #x%d", plev);
		break;	
	}
	return (p);
}