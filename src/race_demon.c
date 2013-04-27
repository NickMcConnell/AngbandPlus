#include "angband.h"

static cptr _desc = 
	"Demons are powerful servants of evil. At the moment, the only subspecies of "
	"demon is the Balrog, but more will certainly be added in the future.\n \n"
	"Balrogs are powerful in melee and have access to a wide array of demonic spells "
	"of great power. Their devilish offense is unrivaled among all the monster races and "
	"at high levels they may unleash deadly hellfire upon their foes. Their summoning talents "
	"are also very good, but summoning a high demon requires a human sacrifice. "
	"Intelligence determines their casting abilities.\n \n"
	"Demons cannot eat normal food, but must feast upon the remains of their human "
	"enemies. Demons are unaffected by the Eldritch Horror and at high enough level, "
	"they become immune to the charge draining attacks of their enemies.";

/******************************************************************************
 *                       40
 * Balrog: Lesser Balrog -> Greater Balrog
 ******************************************************************************/
static spell_info _spells[] = {
	{  2,  1, 20, detect_unlife_spell},
	{  3,  2, 25, evil_bless_spell},
	{  4,  5, 30, resist_fire_spell},
	{  7,  5, 45, scare_spell},
	{  9,  7, 40, fire_bolt_spell},
	{ 10,  7, 40, nether_bolt_spell},
	{ 11,  9, 35, summon_manes_spell},
	{ 20, 15, 50, plasma_bolt_spell},
	{ 25, 16, 50, fire_ball_spell},
	{ 27, 20, 60, flow_of_lava_spell},
	{ 30, 25, 60, recharging_spell},
	{ 32, 28, 70, nether_ball_spell},
	{ 34, 30, 80, plasma_ball_spell},
	{ 36, 70, 85, summon_demon_spell},
	{ 37, 40, 80, kiss_of_succubus_spell},
	{ 40, 35, 50, brain_smash_spell},
	{ 43, 90, 90, summon_greater_demon_spell},
	{ 45, 80, 85, hellfire_spell},
	{ -1, -1, -1, NULL}
};
static int _get_spells(spell_info* spells, int max) {
	return get_spells_aux(spells, max, _spells);
}
static void _balrog_birth(void) 
{ 
	object_type	forge;

	p_ptr->current_r_idx = MON_LESSER_BALROG;

	object_prep(&forge, lookup_kind(TV_RING, SV_RING_DAMAGE));
	forge.to_d = 5;
	add_outfit(&forge);
	
	object_prep(&forge, lookup_kind(TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL));
	add_outfit(&forge);

	object_prep(&forge, lookup_kind(TV_HAFTED, SV_WHIP));
	forge.name2 = EGO_BRAND_FIRE;
	forge.dd = 2;
	forge.ds = 6;
	forge.to_h = 5;
	forge.to_d = 5;
	add_outfit(&forge);
}
static void _calc_bonuses(void) {
	p_ptr->align -= 200;

	res_add(RES_FIRE);
	res_add(RES_NETHER);
	
	p_ptr->hold_life = TRUE;
	p_ptr->no_eldritch = TRUE;
	p_ptr->pspeed += p_ptr->lev/10;
	p_ptr->sh_fire = TRUE;
	
	if (p_ptr->lev >= 10) 
		p_ptr->see_inv = TRUE;

	if (p_ptr->lev >= 30)
	{
		res_add(RES_FIRE);
		res_add(RES_CHAOS);
	}

	if (p_ptr->lev >= 40)
	{
		res_add_immune(RES_FIRE);
		res_add(RES_NETHER);
		p_ptr->kill_wall = TRUE;
		p_ptr->no_charge_drain = TRUE;
	}
}
static void _get_flags(u32b flgs[TR_FLAG_SIZE]) {
	add_flag(flgs, TR_RES_FIRE);
	add_flag(flgs, TR_RES_NETHER);

	add_flag(flgs, TR_HOLD_LIFE);
	add_flag(flgs, TR_SH_FIRE);

	if (p_ptr->lev >= 10)
	{
		add_flag(flgs, TR_SPEED);
		add_flag(flgs, TR_SEE_INVIS);
	}
	if (p_ptr->lev >= 30)
	{
		add_flag(flgs, TR_RES_CHAOS);
	}
}
static void _get_immunities(u32b flgs[TR_FLAG_SIZE]) {
	if (p_ptr->lev >= 40)
		add_flag(flgs, TR_RES_FIRE);
}
static void _gain_level(int new_level) {
	if (p_ptr->current_r_idx == MON_LESSER_BALROG && new_level >= 40)
	{
		p_ptr->current_r_idx = MON_GREATER_BALROG;
		msg_print("You have evolved into a Greater Balrog.");
		p_ptr->redraw |= PR_MAP;
	}
}
static race_t *_balrog_get_race_t(void)
{
	static race_t me = {0};
	static bool   init = FALSE;
	static cptr   titles[2] =  {"Lesser Balrog", "Greater Balrog"};	
	int           rank = 0;

	if (p_ptr->lev >= 40) rank++;

	if (!init)
	{
		me.skills.dis = -5;
		me.skills.dev = 20;
		me.skills.stl = -2;
		me.skills.srh =  3;
		me.skills.fos = 10;

		me.exp = 350;

		me.birth = _balrog_birth;
		me.get_spells = _get_spells;
		me.calc_bonuses = _calc_bonuses;
		me.get_flags = _get_flags;
		me.get_immunities = _get_immunities;
		me.gain_level = _gain_level;
		init = TRUE;
	}

	me.subname = titles[rank];
	me.stats[A_STR] =  4 + 3*rank;
	me.stats[A_INT] =  3 + 2*rank;
	me.stats[A_WIS] = -10;
	me.stats[A_DEX] =  2 + 2*rank;
	me.stats[A_CON] =  4 + 2*rank;
	me.stats[A_CHR] =  2 + rank;
	me.skills.sav = 20 + 15*rank;
	me.skills.thn = 40 + 30*rank;
	me.skills.thb = 20 + 20*rank;
	me.infra = 5 + 10*rank;
	me.life = 110 + 15*rank;

	return &me;
}

static caster_info * _caster_info(void)
{
	static caster_info me = {0};
	static bool init = FALSE;
	if (!init)
	{
		me.magic_desc = "devilish power";
		me.which_stat = A_INT;
		me.weight = 450;
		me.options = CASTER_ALLOW_DEC_MANA;
		init = TRUE;
	}
	return &me;
}

/**********************************************************************
 * Public
 **********************************************************************/
race_t *mon_demon_get_race_t(void)
{
	race_t *result = NULL;

	switch (p_ptr->psubrace)
	{
	/* TODO: Other demon types are certainly desirable ... */
	default: /* Birth Menus */
		result = _balrog_get_race_t();
	}

	result->name = "Demon";
	result->desc = _desc;
	result->flags = RACE_IS_MONSTER | RACE_IS_DEMON | RACE_IS_NONLIVING;
	result->caster_info = _caster_info;

	result->boss_r_idx = MON_GOTHMOG;
	return result;
}
