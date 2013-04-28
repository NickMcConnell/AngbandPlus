divert(-1)
# PosBand -- A variant of Angband roguelike
#
# Copyright (c) 2004 Ben Harrison, Robert Ruehlmann and others
#
# This software may be copied and distributed for educational, research,
# and not for profit purposes provided that this copyright and statement
# are included in all such copies.  Other copyrights may also apply.
# 
# NPPAngband Copyright (c) 2003-2004 Jeff Greene
# PosBand Copyright (c) 2004-2005 Alexander Ulyanov

# powers.m4: powers input file for the preprocessor

# Starting with 0.3.0, PosBand uses a single-source system for generating the
# (numerous) monster powers. The source is this file. The following files
# are generated from it:
#
# - powers_defines.inc:	  for powers.c, tabels.c (OUT=defines)
# - powers_strings.inc:   for parse.c		(OUT=strings)
# - powers_titles.inc:    for tables.c		(OUT=titles)
# - powers_info.inc:      for powers.c		(OUT=info)
# - powers_code.inc:      for powers.c		(OUT=code)
#
# The file (the meaningful part) consists of power() macros.
# They must have four arguments:
# - Power internal name (how it appears in monster.txt), e.g. BR_FIRE_BOLT
#   NOTE! It must NOT be enclosed in any quotes!
# - Power meaningful name (how it is displayed in 'U' menu), in "" quotes.
# - Code which sprintf()'s the "Info" field to s variable, in {{}} quotes.
# - Code which is actually executed when the power is invoked, in {{}} quotes.
#
# If you add the powers, please keep them in sorted order! Although you don't
# want to mess with this file, do you?

# This file must be parsed with M4 preprocessor (only GNU M4 1.4 was checked).
# It may not be available on non-Unux systems, so *.inc files are distributed
# with the sources.

# If you know another convenient way to parse a single file into five C sources,
# let me know.  It can be done with C preprocessor, but even in more
# messy way.

divert(0)dnl
dnl
dnl ********** Header stuff **********
dnl
changequote({{, }})dnl
ifelse(OUT, defines, {{
/* powers_defines.inc: generated from powers.m4, DO NOT EDIT */

enum
{
	PWR_NULL dnl
define({{power}}, {{,
	PWR_$1}})}},
OUT, strings, {{
/* powers_strings.inc: generated from powers.m4, DO NOT EDIT */

/*
 * Body powers
 */
static cptr r_info_pwr[] =
{
	""dnl
define({{power}}, {{,
	"$1"}})}},
OUT, titles, {{
/* powers_titles.inc: generated from powers.m4, DO NOT EDIT */

/*
 * Power descriptions (unthemed)
 * Themed descriptions will replace generic if applicable and available
 */
cptr pwr_desc[] =
{
        NULL dnl
define({{power}}, {{,
	$2}})}},
OUT, info, {{
/* powers_info.inc: generated from powers.m4, DO NOT EDIT */

/*
 * Returns additional info for power
 */
cptr power_info(int power)
{
	static char s[40];
          
	strcpy(s, "");
                  
	switch (r_info[p_ptr->m_r_idx].body.pwr[power].type)
	{
define({{power}}, {{
case PWR_$1:
$3
	break;
}})
}},
OUT, code, {{
/* powers_code.inc: generated from powers.m4, DO NOT EDIT */

/* Actually use power */
bool use_power(int power)
{
	int py = p_ptr->py, y;
	int px = p_ptr->px, x;

	int dir, i, j, count, item, squelch;
	int plev = p_ptr->lev;
	
	bool fear;
	
	char m_name[80], o_name[100];
	
	monster_race *r_ptr;
	monster_type *m_ptr;
	object_type object_type_body;
	object_type *o_ptr;

	/* Hack -- chance of "beam" instead of "bolt" */
	int beam = plev; /* too much? */

	monster_race *mr_ptr = &r_info[p_ptr->m_r_idx];

	p_ptr->energy_use = 0;
        
        switch (power)
        {
define({{power}}, {{
case PWR_$1:
	{
$4
	}
	break;
}})

}}, {{#error Please define the OUT macro for m4 call.}})dnl
dnl
dnl ********** Actual powers **********
dnl
power(ABSORB_MANA, "Absorb Mana From Air",
{{
	sprintf(s, "mana +%d*grids", p_ptr->lev / 2);
}},{{
	i = 0;
	for (dir = 0; dir < 8; dir++)
	{
		y = p_ptr->py + ddy_ddd[dir];
		x = p_ptr->px + ddx_ddd[dir];
		if (cave_empty_bold(y, x)) i++;
	}

	/* Require at least 4 empty grids */
	if (i < 4)
	{
		msg_print("There is not enough room to absorb mana from.");
		return (FALSE);
	}

	for (; i > 0; i--)
	{
		p_ptr->csp += plev / 2;
	}

	if (p_ptr->csp >= p_ptr->msp)
	{
		p_ptr->csp = p_ptr->msp;
		p_ptr->csp_frac = 0;
	}

	p_ptr->energy_use = 500;
}}) dnl
power(ALTER_REALITY, "Alter Reality",
{{}},{{
	msg_print("The world changes!");
	p_ptr->leaving = TRUE;
}}) dnl
power(ATTACK_DIST, "Far-Reaching Attack",
{{}},{{
	bool all_polearms = TRUE, anything_wielded = FALSE;

	for (i = INVEN_EQUIP; i < INVEN_TOTAL; i++)
	{
		if (!(mr_ptr->body.weapon_mask & EQUIP_SLOT(i))) continue;

		o_ptr = &inventory[i];
		if (!o_ptr->k_idx) continue;
		anything_wielded = TRUE;
		if (o_ptr->tval != TV_POLEARM) all_polearms = FALSE;
	}

	if (!anything_wielded)
	{
		msg_print("You must wield something.");
		return(FALSE);
	}

	if (!all_polearms)
	{
		msg_print("You can do that only with a polearm.");
		return (FALSE);
	}
	
	if (!target_set_interactive(TARGET_KILL)) return (FALSE);
	if (!p_ptr->target_who) return (FALSE);
	
	m_ptr = &mon_list[p_ptr->target_who];
	if (m_ptr->cdis > 2)
	{
		msg_print("This monster is too far.");
		return (FALSE);
	}
	
	msg_print("You attack a monster nearby.");
	py_attack(m_ptr->fy, m_ptr->fx);
}}) dnl
power(ATTACK_SUPER, "Powerful Attack",
{{
	sprintf(s, "+10,+%d", p_ptr->lev * 2 / 3);
}},{{
	if (!get_rep_dir(&dir)) return (FALSE);
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];
	
	if (!cave_m_idx[y][x])
	{
		msg_print("There are no monsters here!");
		return (FALSE);
	}
	
	p_ptr->to_h += 10;
	p_ptr->to_d += plev * 2 / 3;
	msg_print("You hit the monster with full force!");
	py_attack(y, x);
	p_ptr->to_h -= 10;
	p_ptr->to_d -= plev * 2 / 3;
}}) dnl
power(ATTACK_WHIRL, "Whirlwind Attack",
{{}},{{
	msg_print("You turn around and hit all nearby enemies at once!");
	count = 0;
	
	for (i = 0; i < 8; i++)
	{
		if (cave_m_idx[p_ptr->py + ddy_ddd[i]][p_ptr->px + ddx_ddd[i]])
		{
			py_attack(p_ptr->py + ddy_ddd[i], p_ptr->px + ddx_ddd[i]);
			count++;
		}
	}
	
	if (!count)
	{
		msg_print("...Unfortunately there are no enemies near you.");
		return (FALSE);
	}
}}) dnl
power(AWAKE_TREE, "Uproot the Trees",
{{
	sprintf(s, "rad %d", 1 + p_ptr->lev / 20);
}},{{
	/* Target */
	if (!target_set_interactive(TARGET_GRID)) return (FALSE);

	msg_print("You awake the trees!");
	project_star(-1, 1 + p_ptr->lev / 20, p_ptr->target_row, p_ptr->target_col,
		plev, GF_AWAKE_TREE, 0);
}}) dnl
power(BASILISK_GAZE, "Basilisk Gaze",
{{}}, {{
	if (!get_rep_dir(&dir)) return (FALSE);
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];
	
	if (!cave_m_idx[y][x])
	{
		msg_print("There are no monsters here!");
		return (FALSE);
	}
	
	m_ptr = &mon_list[cave_m_idx[y][x]];
	r_ptr = &r_info[m_ptr->r_idx];
	monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);

	msg_format("You glare at %s with your deadly gaze!", m_name);

	/* Don't harm non-living or quester mobs */
	if (monster_nonliving(r_ptr) || r_ptr->flags1 & (RF1_QUESTOR))
	{
		msg_print("Nothing happens.");
		return (TRUE);  /* Mana is used anyway. */
	}
	
	/* Saving throw. Pretty straightforward */
	/* Hacks -- uniques are very unlikely to be affected */
	if (rand_int(plev * 2 + 10) < r_ptr->level * (r_ptr->flags1 & (RF1_UNIQUE) ? 3 : 1))
	{
		msg_format("%^s resists the effects!", m_name);
	}
	else
	{
		msg_format("%^s is turned to stone.", m_name);

		/* Hack -- remember the monster race index */
		i = m_ptr->r_idx;
		
		/* BAMF! The monster disappears in a puff of smoke */
		delete_monster(y, x);

		/* Create a statue */
		o_ptr = &object_type_body;
		object_wipe(o_ptr);
		object_prep(o_ptr, lookup_kind(TV_STATUE, 0));
		o_ptr->pval = i;
		
		/*
		 * Statue is 5x heavier than corpse. If corpse is undefined,
		 * assume that the statue weighs 250lb
		 */
		o_ptr->weight = (r_ptr->body.weight ? r_ptr->body.weight * 5 : 250);

		/* ...Here it is! */
		drop_near(o_ptr, -1, y, x);
	}
}}) dnl
power(BEAM_OF_LIGHT, "Spear of Light",
{{
	sprintf(s, "dam 6d8");
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("A line of blue shimmering light appears.");
	lite_line(dir);
}}) dnl
power(BERSERK, "Berserk Strength",
{{
	sprintf(s, "dur 25+d25");
}},{{
	(void)hp_player(30);
	(void)set_shero(p_ptr->shero + randint(25) + 25);
	(void)set_afraid(0);
}}) dnl
power(BLINK, "Blink",
{{
	sprintf(s, "dist 10");
}},{{
	msg_print("You blink away.");
	teleport_player(10);
}}) dnl
power(BRAIN_SMASH, "Brain Smashing",
{{}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You look deep into monster's eyes.");
	/* XXX XXX XXX This makes three separate bolts - not good... */
	(void)slow_monster(dir);
	(void)confuse_monster(dir, plev);
	(void)fear_monster(dir, plev);
}}) dnl
power(BR_CHAOS_BALL, "Breathe Chaos",
{{
	sprintf(s, "%d len %d", p_ptr->lev * 4, 8 + p_ptr->lev/10);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe Chaos.");
	fire_arc(GF_CHAOS, dir, plev * 4, 8 + plev / 10, plev > 30 ? 45 : 30);
}}) dnl
power(BR_COLD_BALL, "Breathe Cold",
{{
	sprintf(s, "%d len %d", p_ptr->lev * 5, 8 + p_ptr->lev/10);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe frost.");
	fire_arc(GF_COLD, dir, plev * 5, 8 + plev / 10, plev > 30 ? 45 : 30);
}}) dnl
power(BR_COLD_BOLT, "Breathe Cold Bolt/Beam",
{{
	sprintf(s, "%dd8", (p_ptr->lev < 5 ? 0 : p_ptr->lev - 5) * 2/3 + 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe frost bolt.");
	if (randint(100) < beam)
	{
		msg_print("It turns into a chillingly cold beam!");
		fire_beam(GF_COLD, dir,
			  damroll((plev - 5) * 2/3 + 3, 8));
	}
	else
		fire_bolt(GF_COLD, dir,
			  damroll((plev - 5) * 2/3 + 3, 8));
}}) dnl
power(BR_CONFU_BALL, "Breathe Confusion",
{{
	sprintf(s, "%d len %d", p_ptr->lev * 3, 8 + p_ptr->lev/10);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe confusion.");
	fire_arc(GF_CONFUSION, dir, plev * 3, 8 + plev / 10, plev > 30 ? 45 : 30);
}}) dnl
power(BR_CRYO_BALL, "Breathe Freezing",
{{
	sprintf(s, "%d len %d", p_ptr->lev * 2, 8 + p_ptr->lev/10);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe freezing.");
	fire_arc(GF_CRYO, dir, plev * 2, 8 + plev / 10, plev > 30 ? 45 : 30);
}}) dnl
power(BR_DARK_BALL, "Breathe Darkness",
{{
	sprintf(s, "%d len %d", p_ptr->lev * 4, 8 + p_ptr->lev/10);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe darkness.");
	fire_arc(GF_DARK, dir, plev * 4, 8 + plev / 10, plev > 30 ? 45 : 30);
}}) dnl
power(BR_DARK_BOLT, "Breathe Darkness Bolt/Beam",
{{
	sprintf(s, "%dd7", (p_ptr->lev < 5 ? 0 : p_ptr->lev - 5) * 2/3 + 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe darkness bolt.");
	if (randint(100) < beam)
	{
		msg_print("It turns into a beam of unearthly shadows!");
		fire_beam(GF_DARK, dir,
			  damroll((plev - 5) * 2/3 + 3, 7));
	}
	else
		fire_bolt(GF_DARK, dir,
			  damroll((plev - 5) * 2/3 + 3, 7));
}}) dnl
power(BR_DISEN_BALL, "Breathe Disenchant",
{{
	sprintf(s, "%d len %d", p_ptr->lev * 3, 8 + p_ptr->lev/10);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe nothingness.");
	fire_arc(GF_DISENCHANT, dir, plev * 3, 8 + plev / 10, plev > 30 ? 45 : 30);
}}) dnl
power(BR_DISEN_BOLT, "Breathe Disenchant Bolt/Beam",
{{
	sprintf(s, "%dd6", (p_ptr->lev < 5 ? 0 : p_ptr->lev - 5) * 2/3 + 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe a bolt of nothingness.");
	if (randint(100) < beam)
	{
		msg_print("It turns into a beam of the Void!");
		fire_beam(GF_DISENCHANT, dir,
			  damroll((plev - 5) * 2/3 + 3, 6));
	}
	else
		fire_bolt(GF_DISENCHANT, dir,
			  damroll((plev - 5) * 2/3 + 3, 6));
}}) dnl
power(BR_ELEC_BALL, "Breathe Lightning",
{{
	sprintf(s, "%d len %d", p_ptr->lev * 5, 8 + p_ptr->lev/10);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe lighning.");
	fire_arc(GF_ELEC, dir, plev * 5, 8 + plev / 10, plev > 30 ? 45 : 30);
}}) dnl
power(BR_ELEC_BOLT, "Breathe Lightning Beam",
{{
	sprintf(s, "%dd8", (p_ptr->lev < 5 ? 0 : p_ptr->lev - 5) * 2/3 + 3);
}},{{
	/* Always beams, like mage spell */
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe a long spark of lightning.");
	fire_beam(GF_ELEC, dir, damroll((plev - 5) * 2/3 + 3, 8));
}}) dnl
power(BR_FEAR_BALL, "Cloud of Fear",
{{
	sprintf(s, "pwr %d", p_ptr->lev * 6);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe pure fear.");
	fire_arc(GF_TURN_ALL, dir, plev * 6, 8 + plev / 10, plev > 30 ? 45 : 30);
}}) dnl
power(BR_FIRE_BALL, "Breathe Fire",
{{
	sprintf(s, "%d len %d", p_ptr->lev * 5, 8 + p_ptr->lev/10);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe fire.");
	fire_arc(GF_FIRE, dir, plev * 5, 8 + plev / 10, plev > 30 ? 45 : 30);
}}) dnl
power(BR_FIRE_BOLT, "Breathe Fire Bolt/Beam",
{{
	sprintf(s, "%dd8", (p_ptr->lev < 5 ? 0 : p_ptr->lev - 5) * 2/3 + 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe fire bolt.");
	if (randint(100) < beam)
	{
		msg_print("It turns into a beam of hellfire!");
		fire_beam(GF_FIRE, dir, damroll((plev - 5) * 2/3 + 3, 8));
	}
	else
	fire_bolt(GF_FIRE, dir, damroll((plev - 5) * 2/3 + 3, 8));
}}) dnl
power(BR_ICE_BALL, "Breathe Ice",
{{
	sprintf(s, "%d len %d", p_ptr->lev * 7, 8 + p_ptr->lev/10);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe ice.");
	fire_arc(GF_ICE, dir, plev * 7, 8 + plev / 10, plev > 30 ? 45 : 30);
}}) dnl
power(BR_LITE_BALL, "Breathe Light",
{{
	sprintf(s, "%d len %d", p_ptr->lev * 3, 8 + p_ptr->lev/10);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe light.");
	fire_arc(GF_LITE, dir, plev * 3, 8 + plev / 10, plev > 30 ? 45 : 30);
}}) dnl
power(BR_LITE_BOLT, "Breathe Light Bolt/Beam",
{{
	sprintf(s, "%dd6", (p_ptr->lev < 5 ? 0 : p_ptr->lev - 5) * 2/3 + 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe light bolt.");
	if (randint(100) < beam)
	{
		msg_print("It turns into a beam of blinding light!");
		fire_beam(GF_LITE, dir,
			  damroll((plev - 5) * 2/3 + 3, 6));
	}
	else
		fire_bolt(GF_LITE, dir,
			  damroll((plev - 5) * 2/3 + 3, 6));
}}) dnl
power(BR_MANA_BALL, "Breathe Raw Mana",
{{
	sprintf(s, "%d len %d", p_ptr->lev * 6, 8 + p_ptr->lev/10);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe raw mana.");
	fire_arc(GF_MANA, dir, plev * 6, 8 + plev / 10, plev > 30 ? 45 : 30);
}}) dnl
power(BR_NETHR_BALL, "Breathe Nether",
{{
	sprintf(s, "%d len %d", p_ptr->lev * 5, 8 + p_ptr->lev/10);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe nether.");
	fire_arc(GF_NETHER, dir, plev * 5, 8 + plev / 10, plev > 30 ? 45 : 30);
}}) dnl
power(BR_NETHR_BOLT, "Breathe Nether Bolt/Beam",
{{
	sprintf(s, "%dd10", (p_ptr->lev < 5 ? 0 : p_ptr->lev - 5) * 2/3 + 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe a bolt of nether.");
	if (randint(100) < beam)
	{
		msg_print("It turns into a beam of the Death!");
		fire_beam(GF_NETHER, dir,
			  damroll((plev - 5) * 2/3 + 3, 10));
	}
	else
		fire_bolt(GF_NETHER, dir,
			  damroll((plev - 5) * 2/3 + 3, 10));
}}) dnl
power(BR_NEXUS_BALL, "Breathe Nexus",
{{
	sprintf(s, "%d len %d", p_ptr->lev * 5, 8 + p_ptr->lev/10);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe nexus.");
	fire_arc(GF_NEXUS, dir, plev * 5, 8 + plev / 10, plev > 30 ? 45 : 30);
}}) dnl
power(BR_PLASMA_BALL, "Breathe Plasma",
{{
	sprintf(s, "%d len %d", p_ptr->lev * 4, 8 + p_ptr->lev/10);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe plasma.");
	fire_arc(GF_PLASMA, dir, plev * 4, 8+ plev / 10, plev > 30 ? 45 : 30);
}}) dnl
power(BR_POIS_BOLT, "Breathe Poison Bolt/Beam",
{{
	sprintf(s, "%dd8", (p_ptr->lev < 5 ? 0 : p_ptr->lev - 5) * 2/3 + 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe poison bolt.");
	if (randint(100) < beam)
	{
		msg_print("It turns into a beam of noxious gases!");
		fire_beam(GF_POIS, dir, damroll((plev - 5) * 2/3 + 3, 8));
	}
	else
	fire_bolt(GF_POIS, dir, damroll((plev - 5) * 2/3 + 3, 8));
}}) dnl
power(BR_POIS_BALL, "Breathe Gas",
{{
	sprintf(s, "%d len %d", p_ptr->lev * 6, 8 + p_ptr->lev/10);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe gas.");
	fire_arc(GF_POIS, dir, plev * 6, 8 + plev / 10, plev > 30 ? 45 : 30);
}}) dnl
power(BR_WIND_BALL, "Breathe Winds",
{{
	sprintf(s, "%d len %d", p_ptr->lev * 6, 8 + p_ptr->lev/10);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You breathe winds.");
	fire_arc(GF_WIND, dir, plev * 6, 8 + plev / 10, plev > 30 ? 45 : 30);
}}) dnl
power(CALL_LIGHT, "Call Light",
{{}},{{
        (void)lite_area(damroll(2, (plev / 2)), (plev / 10) + 1);
}}) dnl
power(CAST_ACID_BOLT, "Acid Bolt",
{{
	sprintf(s, "%dd10", (p_ptr->lev < 10 ? 0 : p_ptr->lev - 10) * 2/3 + 4);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You conjure a bolt of acid.");
	if (randint(100) < beam)
	{
		msg_print("It turns into a beam of liquid death!");
		fire_beam(GF_ACID, dir,
			  damroll((plev < 10 ? 0 : plev - 10) * 2/3 + 4, 10));
	}
	else
		fire_bolt(GF_ACID, dir,
			  damroll((plev < 10 ? 0 : plev - 10) * 2/3 + 4, 10));
}}) dnl
power(CAST_COLD_BALL, "Frost Ball",
{{
	sprintf(s, "%d rad %d", p_ptr->lev * 4, (p_ptr->lev) > 35 ? 3 : 2);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You conjure a ball of frost.");
	fire_ball(GF_COLD, dir, plev * 4, plev > 35 ? 3 : 2);
}}) dnl
power(CAST_COLD_BOLT, "Frost Bolt",
{{
	sprintf(s, "%dd8", (p_ptr->lev < 10 ? 0 : p_ptr->lev - 10) * 3/4 + 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You conjure a bolt of frost.");
	if (randint(100) < beam)
	{
		msg_print("It turns into a chillingly cold beam!");
		fire_beam(GF_COLD, dir,
			  damroll((plev < 10 ? 0 : plev - 10) * 3/4 + 3, 8));
	}
	else
		fire_bolt(GF_COLD, dir,
			  damroll((plev < 10 ? 0 : plev - 10) * 3/4 + 3, 8));
}}) dnl
power(CAST_DARK_BALL, "Darkness Ball",
{{
	sprintf(s, "%d rad %d", p_ptr->lev * 3, (p_ptr->lev > 30) ? 3 : 2);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You conjure a ball of darkness.");
	fire_ball(GF_DARK, dir, plev * 3, plev > 35 ? 3 : 2);
}}) dnl
power(CAST_DARK_STORM, "Darkness Storm",
{{
	sprintf(s, "%d rad 20", p_ptr->lev * 5 / 2);
}},{{
	msg_print("You invoke a storm of darkness.");
	fire_ball(GF_DARK, 0, plev * 5, 20);
}}) dnl
power(CAST_DISENCHANT_BOLT, "Disenchantment Bolt",
{{
	sprintf(s, "%dd8", (p_ptr->lev < 10 ? 0 : p_ptr->lev - 10) * 3/4 + 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You conjure a bolt of nothingness.");
	if (randint(100) < beam)
	{
		msg_print("It turns into a beam of the Void!");
		fire_beam(GF_DISENCHANT, dir,
			  damroll((plev < 10 ? 0 : plev - 10) * 3/4 + 3, 8));
	}
	else
		fire_bolt(GF_DISENCHANT, dir,
			  damroll((plev < 10 ? 0 : plev - 10) * 3/4 + 3, 8));
}}) dnl
power(CAST_ELEC_BOLT, "Spark",
{{
	sprintf(s, "%dd8", (p_ptr->lev < 10 ? 0 : p_ptr->lev - 10) * 3/4 + 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You conjure an electric spark.");
	if (randint(100) < beam)
	{
		msg_print("It turns into a bright arc!");
		fire_beam(GF_ELEC, dir,
			  damroll((plev < 10 ? 0 : plev - 10) * 3/4 + 3, 8));
	}
	else
		fire_bolt(GF_ELEC, dir,
			  damroll((plev < 10 ? 0 : plev - 10) * 3/4 + 3, 8));
}}) dnl
power(CAST_ELEC_BALL, "Ball Lightning",
{{
	sprintf(s, "%d rad %d", p_ptr->lev * 5, (p_ptr->lev) > 35 ? 3 : 2);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You conjure a ball lightning.");
	fire_ball(GF_ELEC, dir, plev * 5, plev > 35 ? 3 : 2);
}}) dnl
power(CAST_ELEC_BEAM, "Lightning",
{{
	sprintf(s, "%dd10", (p_ptr->lev < 20 ? 0 : p_ptr->lev - 20) * 2/3 + 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You release a blinding lighning bolt!");
	fire_beam(GF_ELEC, dir,
		  damroll((p_ptr->lev < 20 ? 0 : p_ptr->lev - 20) * 2/3 + 3, 12));
}}) dnl
power(CAST_FIRE_BALL, "Fire Ball",
{{
	sprintf(s, "%d rad %d", p_ptr->lev * 4, (p_ptr->lev) > 35 ? 3 : 2);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You conjure a ball of fire.");
	fire_ball(GF_FIRE, dir, plev * 4, plev > 35 ? 3 : 2);
}}) dnl
power(CAST_FIRE_BOLT, "Fire Bolt",
{{
	sprintf(s, "%dd8", (p_ptr->lev < 10 ? 0 : p_ptr->lev - 10) * 3/4 + 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You conjure a bolt of fire.");
	if (randint(100) < beam)
	{
		msg_print("It turns into a searingly hot beam!");
		fire_beam(GF_FIRE, dir,
			  damroll((plev < 10 ? 0 : plev - 10) * 3/4 + 3, 8));
	}
	else
		fire_bolt(GF_FIRE, dir,
			  damroll((plev < 10 ? 0 : plev - 10) * 3/4 + 3, 8));
}}) dnl
power(CAST_FIRE_JUMP, "Fiery Lightning",
{{
	sprintf(s, "dam %d rad 1 red 25%%", p_ptr->lev * 8);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	
	/* Ensure there is a monster target */
	if (!target_okay() || !p_ptr->target_who)
	{
		msg_print("You must target a monster!");
		return (FALSE);
	}
	
	msg_print("You conjure a small jumping fireball!");
	fire_jump_ball(GF_FIRE, plev * 8, 1, 75, 25);
}}) dnl
power(CAST_ICE_BALL, "Ice Storm",
{{
	sprintf(s, "%d rad %d", p_ptr->lev * 3, (p_ptr->lev) > 40 ? 4 : 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You invoke a powerful ice storm!");
	fire_ball(GF_ICE, dir, plev * 3, plev > 40 ? 4 : 3);
}}) dnl
power(CAST_ICE_BOLT, "Ice Bolt",
{{
	sprintf(s, "%dd12", (p_ptr->lev < 20 ? 0 : p_ptr->lev - 20) * 2/3 + 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You conjure a bolt of ice.");
	fire_bolt(GF_ICE, dir,
		  damroll((p_ptr->lev < 20 ? 0 : p_ptr->lev - 20) * 2/3 + 3, 12));
}}) dnl
power(CAST_LITE_ORB, "Light Orb",
{{
	sprintf(s, "%d rad 1", p_ptr->lev * 5);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You throw a sphere of light.");
	fire_ball(GF_LITE, dir, plev * 5, 1);
}}) dnl
power(CAST_MAGIC_MISSILE, "Magic Missile",
{{
	sprintf(s, "%dd4", 3 + ((p_ptr->lev - 1) / 5));
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You cast a magic missile.");
	/* Nevermind beam description... */
	fire_bolt_or_beam(beam-10, GF_MISSILE, dir,
			  damroll(3 + (plev - 1) / 5, 4));
}}) dnl
power(CAST_MANA_BOLT_1, "Mana Bolt ",
{{
	sprintf(s, "%dd10", (p_ptr->lev < 10 ? 0 : p_ptr->lev - 10) / 5 + 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You cast a magical bolt.");
	fire_bolt(GF_MISSILE, dir,  damroll((p_ptr->lev < 10 ? 0 : p_ptr->lev - 10) / 5 + 3, 10));
}}) dnl
power(CAST_MANA_BOLT_2, "Mana Bolt",
{{
	sprintf(s, "%dd14", (p_ptr->lev < 10 ? 0 : p_ptr->lev - 10) / 4 + 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You cast a bolt of pure energy.");
	fire_bolt(GF_MISSILE, dir,  damroll((p_ptr->lev < 10 ? 0 : p_ptr->lev - 10) / 4 + 3, 14));
}}) dnl
power(CAST_MANA_STORM, "Mana Storm",
{{
	sprintf(s, "%d rad 5", p_ptr->lev * 7);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You invoke the mana storm!");
	fire_ball(GF_MANA, dir, plev * 7, 5);
}}) dnl
power(CAST_NETHER_BALL, "Death Orb",
{{
	sprintf(s, "%d rad %d", p_ptr->lev * 9/2, (p_ptr->lev > 30) ? 3 : 2);                        
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You blast your enemies with an orb of undeath!");
	fire_ball(GF_NETHER, dir, plev * 9/2, plev > 30 ? 3 : 2);
}}) dnl
power(CAST_NETHER_BOLT, "Nether Bolt",
{{
	sprintf(s, "%dd8", (p_ptr->lev < 11 ? 1 : p_ptr->lev - 9));
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You conjure a bolt of nether forces.");
	if (randint(100) < beam)
	{
		msg_print("It turns into a beam of pure undeath!");
		fire_beam(GF_NETHER, dir,
			  damroll((plev < 11 ? 1 : plev - 9), 8));
	}
	else
		fire_bolt(GF_NETHER, dir,
			  damroll((plev < 11 ? 1 : plev - 9), 8));
}}) dnl
power(CAST_NEXUS_BALL, "Nexus Storm",
{{
	sprintf(s, "%d rad %d", p_ptr->lev * 4, (p_ptr->lev) > 40 ? 4 : 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You invoke a powerful nexus storm!");
	fire_ball(GF_NEXUS, dir, plev * 4, plev > 40 ? 4 : 3);
}}) dnl
power(CAST_NEXUS_BURST, "Space Disruption",
{{
	sprintf(s, "dam %d", p_ptr->lev * 7 / 2);
}},{{
	msg_print("You disrupt the space!");
	project_star(-1, 4, p_ptr->py, p_ptr->px, plev * 7 / 2, GF_NEXUS, PROJECT_PASS);
}}) dnl
power(CAST_PLASMA_BOLT, "Plasma Bolt",
{{
	sprintf(s, "%dd12", (p_ptr->lev < 20 ? 0 : p_ptr->lev - 20) * 2/3 + 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You conjure a bolt of hot plasma.");
	fire_bolt(GF_PLASMA, dir,
		  damroll((p_ptr->lev < 20 ? 0 : p_ptr->lev - 20) * 2/3 + 3, 12));
}}) dnl
power(CAST_PLASMA_BALL, "Plasma Storm",
{{
	sprintf(s, "%d rad %d", p_ptr->lev * 6, (p_ptr->lev) > 40 ? 4 : 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You invoke a powerful plasma storm!");
	fire_ball(GF_PLASMA, dir, plev * 6, plev > 40 ? 4 : 3);
}}) dnl
power(CAST_STINKING_CLOUD, "Stinking Cloud",
{{
	sprintf(s, "%d rad 2", 10 + (p_ptr->lev / 2));
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You conjure a cloud of poisonous gas.");
	fire_ball(GF_POIS, dir, 10 + (plev / 2), 2);
}}) dnl
power(CAST_WATER_BALL, "Whirlpool",
{{
	sprintf(s, "%d rad %d", p_ptr->lev * 7/2, (p_ptr->lev) > 45 ? 3 : 2);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You engulf your foes in whirlpool!");
	fire_ball(GF_WATER, dir, plev * 7/2, plev > 45 ? 3 : 2);
}}) dnl
power(CAST_WATER_BEAM, "Waterfall",
{{
	sprintf(s, "%dd10", (p_ptr->lev < 20 ? 0 : p_ptr->lev - 20) * 2/3 + 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("Water stream rushes through dungeon!");
	fire_beam(GF_WATER, dir,
		  damroll((p_ptr->lev < 20 ? 0 : p_ptr->lev - 20) * 2/3 + 3, 12));
}}) dnl
power(CAST_WIND_BURST, "Cyclone",
{{
	sprintf(s, "dam %d", p_ptr->lev * 7 / 2);
}},{{
	msg_print("You invoke a cyclone!");
	project_star(-1, 4, p_ptr->py, p_ptr->px, plev * 7 / 2, GF_WIND, PROJECT_PASS);
}}) dnl
power(CAST_WIND_ORB, "Wind Blast",
{{
	sprintf(s, "%d rad 1", p_ptr->lev * 4);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You conjure a powerful blast of wind.");
	fire_ball(GF_WIND, dir, plev * 4, 1);
}}) dnl
power(CONFUSE, "Confuse",
{{}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You try to confuse the monster.");
	(void)confuse_monster(dir, plev);
}}) dnl
power(CURE_CRITICAL, "Cure Critital Wounds",
{{
	sprintf(s, "heal 6d10");
}},{{
	msg_print("You cure critical wounds on your body.");
	(void)hp_player(damroll(6, 10));
	(void)set_cut(0);
}}) dnl
power(CURE_LIGHT, "Cure Light Wounds",
{{
	sprintf(s, "heal 2d10");
}},{{
	msg_print("You cure light wounds on your body.");
	(void)hp_player(damroll(2, 10));
	(void)set_cut(p_ptr->cut - 10);
}}) dnl
power(CURE_SERIOUS, "Cure Serious Wounds",
{{
	sprintf(s, "heal 4d10");
}},{{
	msg_print("You cure serious wounds on your body.");
	(void)hp_player(damroll(4, 10));
	(void)set_cut(0);
}}) dnl
power(CURSE_BIG, "Priest's Sacrifice",
{{
	sprintf(s, "dam 15d20");
}},{{
	/* Hack -- directly affect monster */
	if (!get_aim_dir(&dir)) return (FALSE);
	if (!p_ptr->target_who) return (FALSE);
	m_ptr = &mon_list[p_ptr->target_who];
	r_ptr = &r_info[m_ptr->r_idx];
	x = m_ptr->fx;
	y = m_ptr->fy;
	monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);
	msg_format("You point at %s, incanting terribly.", m_name);
	/* It's hard not to wake up if someone is trying to sacrifice you... */
	m_ptr->csleep = 0;
	/* Saving throw. Pretty straightforward */
	if (rand_int(plev * 2 + 70) < r_ptr->level)
	{
		msg_format("%^s resists the effects!", m_name);
	}
	else
	{
		if (mon_take_hit(p_ptr->target_who, damroll(15, 20), &fear, " is consumed by the dark flames!", -1))
		{
			project_ball(-1, 1, y, x, y, x, 100, GF_DARK, 0, 0);
		}
	}
}}) dnl
power(CURSE_MED, "Acolyte's Sacrifice",
{{
	sprintf(s, "dam 8d10");
}},{{
	/* Hack -- directly affect monster */
	if (!get_aim_dir(&dir)) return (FALSE);
	if (!p_ptr->target_who) return (FALSE);
	m_ptr = &mon_list[p_ptr->target_who];
	r_ptr = &r_info[m_ptr->r_idx];
	x = m_ptr->fx;
	y = m_ptr->fy;
	monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);
	msg_format("You point at %s and curse horribly.", m_name);
	/* It's hard not to wake up if someone is trying to sacrifice you... */
	m_ptr->csleep = 0;
	/* Saving throw. Pretty straightforward */
	if (rand_int(plev * 2 + 60) < r_ptr->level)
	{
		msg_format("%^s resists the effects!", m_name);
	}
	else
	{
		/* Additional effect when the monster dies */
		if (mon_take_hit(p_ptr->target_who, damroll(8, 10), &fear, " vanishes in black mist.", -1))
		{
			project_ball(-1, 1, y, x, y, x, 40, GF_NETHER, 0, 0);
		}
	}
}}) dnl
power(CURSE_MORTAL, "Prophet's Sacrifice",
{{
	sprintf(s, "dam 35d35");
}},{{
	/* Hack -- directly affect monster */
	if (!get_aim_dir(&dir)) return (FALSE);
	if (!p_ptr->target_who) return (FALSE);
	m_ptr = &mon_list[p_ptr->target_who];
	r_ptr = &r_info[m_ptr->r_idx];
	x = m_ptr->fx;
	y = m_ptr->fy;
	monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);
	msg_format("You point at %s and scream the word 'DIE!'", m_name);
	/* It's hard not to wake up if someone is trying to sacrifice you... */
	m_ptr->csleep = 0;
	/* Saving throw. Pretty straightforward */
	if (rand_int(plev * 2 + 80) < r_ptr->level)
	{
		msg_format("%^s resists the effects!", m_name);
	}
	else
	{
		if (mon_take_hit(p_ptr->target_who, damroll(35, 35), &fear, " is lost in raging thunderstorm!", -1))
		{
			project_ball(-1, 2, y, x, y, x, 150, GF_WIND, 0, 0);
		}
	}
}}) dnl
power(CURSE_SMALL, "Believer's Sacrifice",
{{
	sprintf(s, "dam 3d5");
}},{{
	/* Hack -- directly affect monster */
	if (!get_aim_dir(&dir)) return (FALSE);
	if (!p_ptr->target_who) return (FALSE);
	m_ptr = &mon_list[p_ptr->target_who];
	r_ptr = &r_info[m_ptr->r_idx];
	monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);
	msg_format("You point at %s and curse.", m_name);
	/* It's hard not to wake up if someone is trying to sacrifice you... */
	m_ptr->csleep = 0;
	/* Saving throw. Pretty straightforward */
	if (rand_int(plev * 2 + 50) < r_ptr->level)
	{
		msg_format("%^s resists the effects!", m_name);
	}
	else
	{
		mon_take_hit(p_ptr->target_who, damroll(3, 5), &fear, " is sacrificed to your god.", -1);
	}
}}) dnl
power(DETECT_ALL, "Detect All",
{{}},{{
	(void)detect_all();
}}) dnl
power(DETECT_CHAOS, "Detect Chaos",
{{}},{{
	(void)detect_monsters_chaotic();
}}) dnl
power(DETECT_EVIL, "Detect Evil",
{{}},{{
	(void)detect_monsters_evil();
}}) dnl
power(DETECT_LAW, "Detect Law",
{{}},{{
	(void)detect_monsters_lawful();
}}) dnl
power(DETECT_MONSTERS, "Detect Monsters",
{{}},{{
	(void)detect_monsters_normal();
}}) dnl
power(DIMENSION_DOOR, "Dimensional Door",
{{
	sprintf(s, "rad %d", p_ptr->lev * 2);
}},{{
	if (!tgt_pt(&x, &y)) return (FALSE);
	if (!cave_empty_bold(y, x) || (cave_info[y][x] & CAVE_ICKY) ||
		(distance(y, x, p_ptr->py, p_ptr->px) > plev * 2) || !(cave_info[y][x] & CAVE_MARK))
	{
		msg_print("You fail to enter dimensional portal!");
		teleport_player(plev);
	}
	else
	{
		msg_print("You step through dimensions.");
		teleport_player_to(y, x);
	}
}}) dnl
power(DISPEL_LIFE, "Dispel Monsters",
{{
	sprintf(s, "dam %d", p_ptr->lev * 2);
}},{{
	/* XXX XXX XXX This should be dispel *life* */
	msg_print("You dispel all creatures!");
	(void)dispel_monsters(plev * 2);
}}) dnl
power(FORCE_CHARM_BY_GHOST, "Force Obey",
{{}},{{
	/* Hack -- directly affect monster */
	if (!get_aim_dir(&dir)) return (FALSE);
	if (!p_ptr->target_who) return (FALSE);
	m_ptr = &mon_list[p_ptr->target_who];
	r_ptr = &r_info[m_ptr->r_idx];
	x = m_ptr->fx;
	y = m_ptr->fy;
	monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);
	msg_format("You gaze intently at %s.", m_name);
	/* No undeads */
	if (r_ptr->flags3 & (RF3_UNDEAD))
	{
		msg_format("%^s gazes back at you and bursts in laugh.", m_name);
	}
	else if (monster_nonliving(r_ptr))
	{
		msg_format("%^s does not seem to be impressed.", m_name);
	}
	else
	{
		project(-1, 0, y, x, y, x, plev * 5 / 2, GF_CHARM, PROJECT_KILL, 0, 0);
	}
}}) dnl
power(ENCHANT_WEAPON, "Enchant Weapon",
{{}}, {{
	enchant_spell(rand_int(4) + plev / 20,
                      rand_int(4) + plev / 20, 0);
}}) dnl
power(FORCE_PSEUDOID, "Examine Object",
{{}},{{
	item_tester_hook = item_tester_nonpseudoid;
	
	if (!get_item(&item, "Concentrate on which item? ", "You have nothing to concentrate on.",
		(USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);
	
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}
	
	i = value_check_aux1(o_ptr);

	/* Squelch it? */
	if (item < INVEN_EQUIP)
	{
		squelch = squelch_itemp(o_ptr, i, 0);
	}
	
	/* Get an object description */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

	/* Message (equipment) */
	if (item >= INVEN_EQUIP)
	{
		msg_format("You feel the %s (%c) you are %s %s %s...",
			   o_name, index_to_label(item), describe_use(item),
			   ((o_ptr->number == 1) ? "is" : "are"),
			   inscrip_text[i - INSCRIP_NULL]);
	}

	/* Message (inventory) */
	else if (item >= 0)
	{
		msg_format("You feel the %s (%c) in your pack %s %s...  %s",
			   o_name, index_to_label(item),
			   ((o_ptr->number == 1) ? "is" : "are"),
			   inscrip_text[i - INSCRIP_NULL],
			    ((squelch == 1) ? "(Squelched)" :
			    ((squelch == -1) ? "(Squelch Failed)" : "")));
	}
	
	/* Message (floor) */
	else
	{
		msg_format("You feel the %s on the floor %s %s...  %s",
			   o_name,
			   ((o_ptr->number == 1) ? "is" : "are"),
			   inscrip_text[i - INSCRIP_NULL],
			    ((squelch == 1) ? "(Squelched)" :
			    ((squelch == -1) ? "(Squelch Failed)" : "")));
	}

	/* Sense the object */
	o_ptr->discount = i;

	/* The object has been "sensed" */
	o_ptr->ident |= (IDENT_SENSE);

	/* Squelch it if necessary */
	do_squelch_item(squelch, item, o_ptr);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN | PW_EQUIP);
}}) dnl
power(HASTE, "Haste",
{{
	sprintf(s, "dur %d+d20", p_ptr->lev);
}},{{
	msg_print("You try to haste yourself magically.");
	if (!p_ptr->fast)
	{
		(void)set_fast(randint(20) + plev);
	}
	else
	{
		(void)set_fast(p_ptr->fast + randint(5));
	}
}}) dnl
power(HEAL, "Heal",
{{
	sprintf(s, "heal 300");
}},{{
	msg_print("You heal yourself!");
	(void)hp_player(300);
	(void)set_stun(0);
	(void)set_cut(0);
}}) dnl
power(HEAL_BONES, "Heal with Bones",
{{
	sprintf(s, "heal corpse lvl x 5");
}},{{
	/* Get the object */
	item_tester_hook = item_tester_bones;
	
	if (!get_item(&item, "Use which bones? ", "You have no suitable bones.",
		(USE_EQUIP | USE_INVEN | USE_FLOOR))) return (FALSE);
	
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}
	else
	{
		o_ptr = &o_list[0 - item];
	}

	/* Get an object description */
	object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);
	
	/* Hack -- issue the message */
	if (strstr(o_name, "Bones"))
	{
		msg_format("You insert %s into your skeleton.", o_name);
	}
	else
	{
		msg_format("You insert the bones of %s into your skeleton.", o_name);
	}
	
	/* Heal */
	if (o_ptr->tval == TV_CORPSE)
	{
		hp_player(r_info[o_ptr->pval].level * 5);
	}
	
	/* Destroy the object */
	if (item >= 0)
	{
		inven_item_increase(item, -1);
		inven_item_describe(item);
		inven_item_optimize(item);
	}
	else
	{
		floor_item_increase(0 - item, -1);
		floor_item_describe(0 - item);
		floor_item_optimize(0 - item);
	}
	break;

}}) dnl
power(HEAL_PET, "Heal Pet",
{{
	sprintf(s, "heal %d%% or 3d10", p_ptr->lev);
}},{{
	/* Target */
	if (!target_set_interactive(TARGET_KILL)) return (FALSE);
	if (!p_ptr->target_who) return (FALSE);
	
	/* Ensure that monster is a pet */
	m_ptr = &mon_list[p_ptr->target_who];
	if (!(m_ptr->align & AL_PET_MASK))
	{
		msg_print("This monster is not your pet!");
		return (FALSE);
	}
	
	i = damroll(3, 10);
	j = m_ptr->maxhp * (plev * 2 / 3) / 100;
	if (j > i) i = j;
	
	if (m_ptr->hp + i > m_ptr->maxhp) i = m_ptr->maxhp - m_ptr->hp;
	
	msg_print("You use your powers to heal your pet.");
	m_ptr->hp += i;
	p_ptr->redraw |= (PR_HEALTH);
}}) dnl
power(HEAL_WALL, "Merge Wall to Self",
{{
	sprintf(s, "heal %d", 100 + p_ptr->lev * 3);
}},{{
	if (!get_rep_dir(&dir)) return (FALSE);
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];
	if (cave_feat[y][x] < FEAT_WALL_EXTRA || cave_feat[y][x] > FEAT_WALL_SOLID)
	{
		msg_print("You must use the section of granite wall.");
		return (FALSE);
	}
	
	p_ptr->energy_use = 250 + rand_int(250);
	cave_set_feat(y, x, FEAT_FLOOR);
	hp_player(100 + plev * 3);
}}) dnl
power(HEROISM, "Heroism",
{{
	sprintf(s, "dur 25+d25");
}},{{
	(void)hp_player(10);
	(void)set_hero(p_ptr->hero + randint(25) + 25);
	(void)set_afraid(0);
}}) dnl
power(HOLY_ORB_SMALL, "Holy Smoke",
{{
	sprintf(s, "3d6 rad 1");
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("The cloud of light smoke engulfs your target.");
	fire_ball(GF_HOLY_ORB, dir, damroll(3, 6), 1);
}}) dnl
power(IDENTIFY, "Identify",
{{}},{{
	if (p_ptr->al_special == PAL_CHAOS) msg_print("You listen to the voices from the Chaos...");
	(void)ident_spell();
}}) dnl
power(LIGHT_HEAL, "Healing Light",
{{
	sprintf(s, "heal %d", ((p_ptr->lev < 20 ? 20 : p_ptr->lev) - 20) * 5 + 100);
}},{{
	if (!(cave_info[p_ptr->py][p_ptr->px] & (CAVE_GLOW)))
	{
		msg_print("There is no light here to use.");
	}
	else
	{
		unlite_area(plev, 3);
		hp_player(((plev < 20 ? 20 : plev) - 20) * 5 + 100);
	}
}}) dnl
power(MAKE_ABYSS, "Crush the Dungeon",
{{
	sprintf(s, "rad %d", 5 + p_ptr->lev / 10);
}},{{
	/* Target */
	if (!target_set_interactive(TARGET_KILL | TARGET_GRID)) return (FALSE);

	msg_print("You change the world!");
	project_star(-1, 5 + p_ptr->lev / 10, p_ptr->target_row, p_ptr->target_col,
		1, GF_MAKE_ABYSS, PROJECT_PASS);
}}) dnl
power(MAKE_FIRE, "Kindle the Fires",
{{
	sprintf(s, "rad %d", 5 + p_ptr->lev / 10);
}},{{
	/* Target */
	if (!target_set_interactive(TARGET_KILL | TARGET_GRID)) return (FALSE);

	msg_print("You change the world!");
	project_star(-1, 5 + p_ptr->lev / 10, p_ptr->target_row, p_ptr->target_col,
		1, GF_MAKE_FIRE, PROJECT_PASS);
}}) dnl
power(MAKE_GRASS, "Grow the Grass",
{{
	sprintf(s, "rad %d", 5 + p_ptr->lev / 10);
}},{{
	/* Target */
	if (!target_set_interactive(TARGET_KILL | TARGET_GRID)) return (FALSE);

	msg_print("You change the world!");
	project_star(-1, 5 + p_ptr->lev / 10, p_ptr->target_row, p_ptr->target_col,
		1, GF_MAKE_GRASS, PROJECT_PASS);
}}) dnl
power(MAKE_ICE, "Freeze the Air",
{{
	sprintf(s, "rad %d", 5 + p_ptr->lev / 10);
}},{{
	/* Target */
	if (!target_set_interactive(TARGET_KILL | TARGET_GRID)) return (FALSE);

	msg_print("You change the world!");
	project_star(-1, 5 + p_ptr->lev / 10, p_ptr->target_row, p_ptr->target_col,
		1, GF_MAKE_ICE, PROJECT_PASS);
}}) dnl
power(MAKE_LAVA, "Melt the Stone",
{{
	sprintf(s, "rad %d", 5 + p_ptr->lev / 10);
}},{{
	/* Target */
	if (!target_set_interactive(TARGET_KILL | TARGET_GRID)) return (FALSE);

	msg_print("You change the world!");
	project_star(-1, 5 + p_ptr->lev / 10, p_ptr->target_row, p_ptr->target_col,
		1, GF_MAKE_LAVA, PROJECT_PASS);
}}) dnl
power(MAKE_TREE, "Raise the Trees",
{{
	sprintf(s, "rad %d", 5 + p_ptr->lev / 10);
}},{{
	/* Target */
	if (!target_set_interactive(TARGET_KILL | TARGET_GRID)) return (FALSE);

	msg_print("You change the world!");
	project_star(-1, 5 + p_ptr->lev / 10, p_ptr->target_row, p_ptr->target_col,
		1, GF_MAKE_TREE, PROJECT_PASS);
}}) dnl
power(MAKE_WALL, "Build the Walls",
{{
	sprintf(s, "rad %d", 2 + p_ptr->lev / 20);
}},{{
	/* Target */
	if (!target_set_interactive(TARGET_KILL | TARGET_GRID)) return (FALSE);

	msg_print("You change the world!");
	project_star(-1, 2 + p_ptr->lev / 20, p_ptr->target_row, p_ptr->target_col,
		1, GF_MAKE_WALL, PROJECT_PASS);
}}) dnl
power(MAKE_WATER, "Fill the Lake",
{{
	sprintf(s, "rad %d", 5 + p_ptr->lev / 10);
}},{{
	/* Target */
	if (!target_set_interactive(TARGET_KILL | TARGET_GRID)) return (FALSE);

	msg_print("You change the world!");
	project_star(-1, 5 + p_ptr->lev / 10, p_ptr->target_row, p_ptr->target_col,
		1, GF_MAKE_WATER, PROJECT_PASS);
}}) dnl
power(OPPOSE_COLD, "Oppose Cold",
{{
	sprintf(s, "dur 30+d30");
}},{{
	(void)set_oppose_cold(p_ptr->oppose_cold + randint(30) + 30);
}}) dnl
power(OPPOSE_ELEC, "Oppose Electricity",
{{
	sprintf(s, "dur 30+d30");
}},{{
	(void)set_oppose_elec(p_ptr->oppose_elec + randint(30) + 30);
}}) dnl
power(OPPOSE_FIRE, "Oppose Fire",
{{
	sprintf(s, "dur 30+d30");
}},{{
        (void)set_oppose_fire(p_ptr->oppose_fire + randint(30) + 30);
}}) dnl
power(PARALYZE, "Sleep",
{{}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You stare deep into monster's eyes.");
	(void)sleep_monster(dir);
}}) dnl
power(PROJECT_CHAOS, "Project Chaos",
{{
	sprintf(s, "dam %d", p_ptr->lev * 2);
}},{{
	msg_print("You invoke the raw Chaos upon your enemies!");
	project_los(GF_CHAOS, plev * 2);
}}) dnl
power(PROJECT_CRYO, "Call the Ice of Helcaraxe",
{{
	sprintf(s, "dam %d", p_ptr->lev * 3);
}},{{
	msg_print("You invoke the extreme cold upon your enemies!");
	project_los(GF_CRYO, plev * 3);
}}) dnl
power(PROJECT_DISEN, "Project Disenchant",
{{
	sprintf(s, "dam %d", p_ptr->lev * 3 / 2);
}},{{
	msg_print("You invoke the very Void upon your enemies!");
	project_los(GF_DISENCHANT, plev * 3 / 2);
}}) dnl
power(REMEMBRANCE, "Remembrance",
{{}},{{
	(void)restore_level();
}}) dnl
power(RESISTANCE, "Resistance",
{{
	sprintf(s, "dur 20+d20");
}},{{
	i = randint(20) + 20;
	(void)set_oppose_acid(p_ptr->oppose_acid + i);
	(void)set_oppose_elec(p_ptr->oppose_acid + i);
	(void)set_oppose_fire(p_ptr->oppose_acid + i);
	(void)set_oppose_cold(p_ptr->oppose_acid + i);
	(void)set_oppose_pois(p_ptr->oppose_acid + i);
}}) dnl
power(RESTORATION, "Restoration",
{{}},{{
	(void)do_res_stat(A_STR);
	(void)do_res_stat(A_INT);
	(void)do_res_stat(A_WIS);
	(void)do_res_stat(A_DEX);
	(void)do_res_stat(A_CON);
	(void)do_res_stat(A_CHR);
}}) dnl
power(SCARE, "Scare",
{{}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You try to scare the monster.");
	(void)fear_monster(dir, plev); 
}}) dnl
power(SCARE_ALL, "Scare All",
{{}},{{
	msg_print("You try to scare everything in sight.");
	project_los(GF_TURN_ALL, plev);
}}) dnl
power(SET_PROJECT_ELEC, "Electric Discharge",
{{
	sprintf(s, "dam %dd8 rad %d dur 25", p_ptr->lev / 5, p_ptr->lev / 5);
}},{{
	msg_print("You invoke a powerful electric discharge.");
	p_ptr->project_elec += 25;
}}) dnl
power(SHIFT_PLANES, "Shift Planes",
{{
	sprintf(s, "dur 20+d20");
}},{{
	(void)set_tim_immaterial(p_ptr->tim_immaterial + randint(20) + 20);
}}) dnl
power(SHRIEK, "Shriek",
{{}},{{
	msg_print("You make a high pitched shriek.");
	aggravate_monsters(0);
	if (one_in_(2))
	{
		if (one_in_(2))
		{
			if (summon_specific_pet(py, px, p_ptr->depth, 0))
			{
				msg_print("Your shriek attracts a friendly monster!");
			}
		}
		else
		{
			if (summon_specific(py, px, p_ptr->depth, 0))
			{
				msg_print("Your shriek attracts a hostile monster!");
			}
		}        			
	}
}}) dnl
power(SLOW, "Slow",
{{}},
{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You try to slow down the monster.");
	slow_monster(dir);
}}) dnl
power(SPAWN, "Spawn",
{{}},
{{
	int hp;
	r_ptr = &r_info[p_ptr->m_r_idx];
	if (r_ptr->flags1 & (RF1_FORCE_MAXHP))
	{
		hp = r_ptr->hdice * r_ptr->hside;
	}
	else
	{
		/* XXX Not very precise */
		hp = (r_ptr->hdice * r_ptr->hside) / 2;
	}
	if (p_ptr->chp <= hp)
	{
		msg_print("You are too weak to spawn.");
		return (FALSE);
	}
	if (p_ptr->chp - hp <= (p_ptr->mhp * op_ptr->hitpoint_warn / 10))
	{
		if (!get_check("This may critically lower your hit points! Are you sure? ")) return (FALSE);
	}
	
	i = 0;
	summon_pets_hack = TRUE;
	place_monster_group(py, px, p_ptr->m_r_idx, FALSE, 2);
	summon_pets_hack = FALSE;
	take_hit(hp, "failed attempt to spawn");
	return (TRUE);
}}) dnl
power(SPIT_ACID, "Spit Acid",
{{
	sprintf(s, "%dd5", 3 + p_ptr->lev / 4);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You spit acid.");
	fire_bolt(GF_ACID, dir, damroll(3 + plev / 4, 5));
}}) dnl
power(SPIT_POISON, "Spit Poison",
{{
	sprintf(s, "%dd5", 3 + p_ptr->lev / 3);
}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You spit venom.");
	fire_bolt(GF_POIS, dir, damroll(3 + plev / 3, 5));
}}) dnl
power(STAR_HEAL, "*Healing*",
{{
	sprintf(s, "heal 1000");
}},{{
	msg_print("Your god heals you!");
	(void)hp_player(1000);
	(void)set_stun(0);
	(void)set_cut(0);
}}) dnl
power(STEAL, "Steal",
{{}},{{
	/* Based on do_cmd_steal() */
	if (!get_aim_dir(&dir)) return (FALSE);
	y = py + ddy[dir];
	x = px + ddx[dir];
	
	if (cave_m_idx[y][x] > 0)
	{
		monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
		
		if (m_ptr->ml)
		{
			py_steal(y, x);
			return (TRUE);
		}
	}

	msg_print("You don't see anything to steal from.");
	return (FALSE);
}}) dnl
power(SUMM_AINU, "Summon Maiar",
{{}},{{
	count = 0;
	for (i = 0; i < 3; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_AINU);
	if (count)
	{
		msg_print("You summon the Maiar from Valinor!");
	}
}}) dnl
power(SUMM_BALROG, "Summon Demon Commander",
{{}},{{
	if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_BALROG))
	{
		msg_print("Lord of Hell answers to your call!");
	}
}}) dnl
power(SUMM_DARK_ELF, "Summon Dark Elf",
{{}},{{
	if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_DARK_ELF))
	{
		msg_print("You summon an evil dark elf!");
	}
}}) dnl
power(SUMM_DEMON, "Summon Demon",
{{}},{{
	if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_DEMON))
	{
		msg_print("You summon a demon from a lower plane!");
	}
}}) dnl
power(SUMM_DEMON_SUMM, "Summon Demon Summoner",
{{}},{{
	if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_DEMON_SUMM))
	{
		msg_print("You summon a demon summoner!");
	}
}}) dnl
power(SUMM_DRAGON, "Summon Dragon",
{{}},{{
	if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_DRAGON))
	{
		msg_print("You summon a dragon!");
	}
}}) dnl
power(SUMM_DRAGON_ANCIENT, "Summon Ancient Dragon",
{{}},{{
	if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_HI_DRAGON))
	{
		msg_print("You summon an ancient wyrm!");
	}
}}) dnl
power(SUMM_DRAGON_MATURE, "Summon Mature Dragons",
{{}},{{
	count = 0;
	for (i = 0; i < 5; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_MATURE_DRAGON);
	if (count)
	{
		msg_print("You summon mighty dragons!");
	}
}}) dnl
power(SUMM_DRAGON_SUMM, "Summon Dragon Summoner",
{{}},{{
	if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_DRAGON_SUMM))
	{
		msg_print("You summon a dragon summoner!");
	}
}}) dnl
power(SUMM_ELEMENTAL, "Summon Elemental",
{{}},{{
	count = 0;
	for (i = 0; i < 5; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_ELEMENTAL);
	if (count)
	{
		msg_print("You conjure some elementals!");
	}
}}) dnl
power(SUMM_GOLEM, "Summon Golem",
{{}},{{
	if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_GOLEM))
	{
		msg_print("You magically construct a golem!");
	}
}}) dnl
power(SUMM_HI_DEMON, "Summon Greater Demons",
{{}},{{
	count = 0;
	for (i = 0; i < 8; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_HI_DEMON);
	if (count)
	{
		msg_print("You summon greater demons!");
	}
}}) dnl
power(SUMM_HI_DRAGON, "Summon Ancient Dragons",
{{}},{{
	count = 0;
	for (i = 0; i < 8; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_HI_DRAGON);
	if (count)
	{
		msg_print("You summon ancient wyrms!");
	}
}}) dnl
power(SUMM_HI_UNDEAD, "Summon Greater Undeads",
{{}},{{
	count = 0;
	for (i = 0; i < 8; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_HI_UNDEAD);
	if (count)
	{
		msg_print("You summon the shadows of death!");
	}
}}) dnl
power(SUMM_HYDRA, "Summon Hydras",
{{}},{{
	count = 0;
	for (i = 0; i < 5; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_HYDRA);
	if (count)
	{
		msg_print("You summon some hydras!");
	}
}}) dnl
power(SUMM_LAWFUL, "Summon Lawful Creatures",
{{}},{{
	count = 0;
	for (i = 0; i < 3; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_LAWFUL);
	if (count)
	{
		msg_print("You summon the minions of Law!");
	}
}}) dnl	
power(SUMM_LICH, "Summon Lich",
{{}},{{
	if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_LICH))
	{
		msg_print("You summon a sorcerer from his grave!");
	}
}}) dnl
power(SUMM_MAGMA_ELEM_WALL, "Melt the Wall to Magma",
{{}},{{
	if (!get_rep_dir(&dir)) return (FALSE);
	y = p_ptr->py + ddy[dir];
	x = p_ptr->px + ddx[dir];
	if (cave_feat[y][x] < FEAT_WALL_EXTRA || cave_feat[y][x] > FEAT_WALL_SOLID)
	{
		msg_print("You must use the section of granite wall.");
		return (FALSE);
	}
	
	p_ptr->energy_use = 250 + rand_int(250);
	cave_set_feat(y, x, FEAT_FLOOR);
	summon_pets_hack = TRUE;
	/* Hack -- Magma elemental and Greater magma elemental index hardcoded */
	msg_print("You melt the wall to summon the minion!");
	place_monster_one(y, x, (plev > 40 ? 656 : 397), FALSE, plev > 35 ? 35 : plev);
	summon_pets_hack = FALSE;
}}) dnl
power(SUMM_OGRE, "Summon Ogres",
{{}},{{
	if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_OGRE))
	{
		msg_print("You summon a band of ogres!");
	}
}}) dnl
power(SUMM_ORC, "Summon Orcs",
{{}},{{
	if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_ORC))
	{
		msg_print("You summon a band of orcs!");
	}
}}) dnl
power(SUMM_SPIDER, "Summon Spiders",
{{}},{{
	count = 0;
	for (i = 0; i < 8; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_SPIDER);
	if (count)
	{
		msg_print("You summon spiders from Nan Dungortheb!");
	}
}}) dnl
power(SUMM_TROLL, "Summon Trolls",
{{}},{{
	count = 0;
	for (i = 0; i < 5; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_TROLL);
	if (count)
	{
		msg_print("You summon some ugly trolls!");
	}
}}) dnl
power(SUMM_ULTIMATE, "Summon Ultimate Pets",
{{}},{{
	count = 0;
	for (i = 0; i < 4; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_ULTIMATE);
	if (count)
	{
		msg_print("You summon some *ULTIMATE* allies!");
	}
}}) dnl
power(SUMM_UNDEAD, "Summon Undead",
{{}},{{
	if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_UNDEAD))
	{
		msg_print("You raise an undead slave from the grave!");
	}
}}) dnl
power(SUMM_UNDEAD_DRAGON, "Summon Undead Dragons",
{{}},{{
	count = 0;
	for (i = 0; i < 5; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_UNDEAD_DRAGON);
	if (count)
	{
		msg_print("You summon the shadows of the Great Wyrms!");
	}
}}) dnl
power(SUMM_UNDEAD_SUMM, "Summon Undead Summoner",
{{}},{{
	if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_UNDEAD_SUMM))
	{
		msg_print("You summon an undead summoner!");
	}
}}) dnl
power(SUMM_VORTEX, "Summon Vortex",
{{}},{{
	count = 0;
	for (i = 0; i < 5; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_VORTEX);
	if (count)
	{
		msg_print("You conjure elemental vortices!");
	}
}}) dnl
power(SUMM_VROCK, "Summon Demon Troops",
{{}},{{
	if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_VROCK))
	{
		msg_print("Troopers from Hell answer to your call!");
	}
}}) dnl
power(SUMM_WIGHT_WRAITH, "Summon Wight/Wraith",
{{}},{{
	if (summon_specific_pet(py, px, p_ptr->depth, SUMMON_WIGHT_WRAITH))
	{
		msg_print("You summon a ghostly figure from the grave!");
	}
}}) dnl
power(SUMM_YEEK, "Summon Yeeks",
{{}},{{
	count = 0;
	for (i = 0; i < 5; i++) count += summon_specific_pet(py, px, p_ptr->depth, SUMMON_YEEK);
	if (count)
	{
		msg_print("You summon some wildly screaming yeeks!");
	}
}}) dnl
power(TELEPORT, "Teleport",
{{
	sprintf(s, "dist 100");
}},{{
	msg_print("You teleport across the level.");
	teleport_player(100);
}}) dnl
power(TELEPORT_AWAY, "Teleport Away",
{{}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You teleport a monster away.");
	(void)teleport_monster(dir);
}}) dnl
power(WEB_BALL, "Weave Webs",
{{
	sprintf(s, "rad 2");
}},{{
	msg_print("You weave webs around you.");
	fire_bolt_beam_special(GF_WEBBING, 0, 1, 2,
		PROJECT_BOOM | PROJECT_GRID | PROJECT_THRU);
}}) dnl
power(WEB_RAY, "Spit Webs",
{{}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	msg_print("You spit webs.");
	fire_bolt_beam_special(GF_WEBBING, dir, 1, 7,
		PROJECT_BEAM | PROJECT_GRID | PROJECT_THRU);
}}) dnl
power(WONDER, "Wonder",
{{}},{{
	if (!get_aim_dir(&dir)) return (FALSE);
	if (p_ptr->al_special == PAL_CHAOS) msg_print("You invoke the force of Chaos...");
	else msg_print("You invoke a random spell...");
	spell_wonder(dir);
}}) dnl
power(WORD_OF_DESTRUCTION, "Word of Destruction",
{{}},{{
	msg_print("You completely wipe the nearby area!");
	destroy_area(py, px, 15, TRUE);
}}) dnl
power(WORD_OF_RECALL, "Word of Recall",
{{}},{{
	set_recall();
}}) dnl
dnl
dnl ********** Footer stuff **********
dnl
ifelse(OUT, defines, {{
};
}},
OUT, strings, {{,
	NULL
};
}},
OUT, titles, {{
};
}},
OUT, info, {{
}
	return s;
}
}},
OUT, code, {{
}
	return (TRUE);
}
}}, {{#error Please define the OUT macro for m4 call.}})
dnl
dnl End of powers.m4
dnl
