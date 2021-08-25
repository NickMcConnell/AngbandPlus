/* File: bookless.c
 * Purpose: commands for bookless spells
 */

#include "angband.h"
#include "cave.h"
#include "effects.h"
#include "spells.h"
#include "bookless.h"
#include "target.h"

/* Globals for last spell */
int last_bookless_dir = -1;
struct spellholder last_bookless_spell;

/* Assassins */
void Assassin_poison_dart(int dir) {
	fire_bolt(GF_POIS, dir, 15 + p_ptr->lev);
}
void Assassin_fume_cloud(int dir) {
	fire_ball(GF_POIS, dir, 10 + (p_ptr->lev * 2), 2);
}
void Assassin_shadow_portal(int dir)
{
	(void)teleport_player(25 + p_ptr->lev);
}
void Assassin_corrosion(int dir)
{
	fire_beam(GF_ACID, dir, p_ptr->lev * 6);
}
void Assassin_withering(int dir)
{
	int plev = p_ptr->lev;
	fire_ball(GF_NETHER, dir, plev * plev / 2, 1);
}
void Assassin_astral_vision(int dir)
{
	wiz_light();
	(void)detect_traps(TRUE);
	(void)detect_monsters_normal(TRUE);
	(void)detect_monsters_invis(TRUE);
}

struct spellholder assassin_spell_info[] = {
	{ "Poison Dart", 1, 1, 15, "Hurls a bolt of poison.",
		&Assassin_poison_dart, 1 },
	{ "Fume Cloud", 4, 4, 25, "Conjures a cloud of poisonous fumes.",
		&Assassin_fume_cloud, 1 },
	{ "Shadow Portal", 8, 8, 35, "Teleports you out of enemies' sight.",
		&Assassin_shadow_portal, 0 },
	{ "Corrosion", 12, 12, 45, "Conjures a beam of acidic vapor.",
		&Assassin_corrosion, 1 },
	{ "Withering", 16, 16, 55, "Conjures a small, powerful ball of nether.",
		&Assassin_withering, 1 },
	{ "Astral Vision", 20, 20, 65, "Lights and maps the entire level.",
		&Assassin_astral_vision, 0 },
	NULL,
};

/* Avatars */
void Avatar_holy_light(int dir)
{
	(void)inc_timed(TMD_HERO, randint1(25) + p_ptr->lev, TRUE);
	(void)inc_timed(TMD_PROTEVIL, randint1(25) + p_ptr->lev, TRUE);
}
void Avatar_sense_evil(int dir)
{
	(void)detect_monsters_evil(TRUE);
}
void Avatar_phasing(int dir)
{
	teleport_player(3 * p_ptr->lev);
}
void Avatar_weigh_magic(int dir)
{
	(void)ident_spell();
}
void Avatar_divine_fury(int dir)
{
	(void)inc_timed(TMD_SHERO, randint1(50) + p_ptr->lev, TRUE);
	(void)inc_timed(TMD_FAST, randint1(50) + p_ptr->lev, TRUE);
}
void Avatar_retribution(int dir)
{
	fire_beam(GF_HOLY_ORB, dir, 6 * p_ptr->lev);
}
void Avatar_mass_exorcism(int dir)
{
	(void)dispel_undead(3 * p_ptr->lev);
	(void)dispel_evil(3 * p_ptr->lev);
}
void Avatar_deluge(int dir)
{
	fire_ball(GF_WATER, dir, 9 * p_ptr->lev, 14);
}

struct spellholder avatar_spell_info[] = {
	{ "Holy light", 1, 1, 0, "Grants Heroism and protection from evil.",
		&Avatar_holy_light, 0 },
	{ "Sense evil", 3, 2, 0, "Detects evil monsters.",
		&Avatar_sense_evil, 0 },
	{ "Phasing", 6, 3, 0, "Teleports you a good distance.",
		&Avatar_phasing, 0 },
	{ "Weigh magic", 9, 4, 0, "Identifies an object.",
		&Avatar_weigh_magic, 0 },
	{ "Divine fury", 12, 5, 0, "Grants speed and berserk strength.",
		&Avatar_divine_fury, 0 },
	{ "Retribution", 15, 6, 0, "Invokes a beam of holy fire.",
		&Avatar_retribution, 1 },
	{ "Mass Exorcism", 18, 7, 0, "Dispels undead and evil monsters.",
		&Avatar_mass_exorcism, 0 },
	{ "Deluge", 21, 8, 0, "Invokes a terrible flood.",
		&Avatar_deluge, 1 },
	NULL,
};

/* Pyromancers */
void Pyro_fire_bolt(int dir)
{
	fire_bolt(GF_FIRE, dir, 12 + damroll(4, p_ptr->lev));
}
void Pyro_sense_heat(int dir)
{
	(void)detect_monsters_normal(TRUE);
}
void Pyro_plasma_bolt(int dir)
{
	fire_bolt(GF_PLASMA, dir, 8 + damroll(6, p_ptr->lev));
}
void Pyro_flicker(int dir)
{
	teleport_player(5 + p_ptr->lev / 2);
}
void Pyro_fire_ball(int dir)
{
	fire_ball(GF_FIRE, dir, 8 * p_ptr->lev, 4);
}
void Pyro_heat_rays(int dir)
{
	(void)project_los(GF_PLASMA, 4 * p_ptr->lev, FALSE);
}
void Pyro_knowledge(int dir)
{
	wiz_light();
}
void Pyro_mana_bolt(int dir)
{
	fire_bolt(GF_MANA, dir, damroll(10, p_ptr->lev));
}

struct spellholder pyro_spell_info[] = {
	{ "Fire bolt", 1, 1, 5, "Conjures a bolt of fire.",
		&Pyro_fire_bolt, 1 },
	{ "Sense heat", 4, 3, 10, "Detects monsters in your vicinity.",
		&Pyro_sense_heat, 0 },
	{ "Plasma Bolt", 8, 8, 15, "Conjures a bolt of plasma.",
		&Pyro_plasma_bolt, 1 },
	{ "Flicker", 14,  6, 15, "Teleports you a short distance.",
		&Pyro_flicker, 0 },
	{ "Fire ball", 22, 12, 20, "Conjures a powerful ball of fire.",
		&Pyro_fire_ball, 1 },
	{ "Heat Rays", 28, 20, 25, "Burns all enemies in your sight.",
		&Pyro_heat_rays, 0 },
	{ "Knowledge", 32, 18, 30, "Lights and maps the whole level.",
		&Pyro_knowledge, 0 },
	{ "Mana Bolt", 36, 24, 35, "Summons a bolt of pure energy.",
		&Pyro_mana_bolt, 1 },
	NULL,
};

/* Reapers */
void Reaper_manaburst(int dir)
{
	int i;
	for (i = 0; i < 1 + (p_ptr->lev / 6); i++)
	{
		fire_bolt(GF_MANA, dir, 10 + p_ptr->lev);
	}
}
void Reaper_berserk(int dir)
{
	(void)inc_timed(TMD_SHERO, randint1(25) + p_ptr->lev, TRUE);
	(void)inc_timed(TMD_FAST, randint1(25) + p_ptr->lev, TRUE);
}
void Reaper_darksight(int dir)
{
	(void)map_area();
	(void)detect_monsters_normal(TRUE);
	(void)detect_monsters_invis(TRUE);
}
void Reaper_indominable(int dir)
{
	int plev = p_ptr->lev;
	(void)inc_timed(TMD_OPP_FIRE, randint1(50) + plev, TRUE);
	(void)inc_timed(TMD_OPP_COLD, randint1(50) + plev, TRUE);
	(void)inc_timed(TMD_OPP_ACID, randint1(50) + plev, TRUE);
	(void)inc_timed(TMD_OPP_ELEC, randint1(50) + plev, TRUE);
	(void)inc_timed(TMD_OPP_CONF, randint1(50) + plev, TRUE);
	(void)inc_timed(TMD_OPP_POIS, randint1(50) + plev, TRUE);
}

struct spellholder reaper_spell_info[] = {
	{ "Manaburst", 5, 3, 10, "Conjures a barrage of mana bolts.",
		&Reaper_manaburst, 1 },
	{ "Berserk", 10, 6, 20, "Grants increased speed and strength.",
		&Reaper_berserk, 0 },
	{ "Darksight", 15, 9, 30, "Allows you to see through walls.",
		&Reaper_darksight, 0 },
	{ "Indominable", 20, 12, 40, "Protects you from physical and psychic harm.",
		&Reaper_indominable, 0 },
	NULL,
};

/* Sappers */
void Sapper_flare(int dir)
{
	fire_ball(GF_FIRE, dir, damroll(4, 4), 1);
	fire_ball(GF_LIGHT_WEAK, dir, 0, 10);
}
void Sapper_smoke_bomb(int dir)
{
	fire_ball(GF_POIS, dir, damroll(5, 15), 3);
	fire_ball(GF_DARK_WEAK, dir, 0, 10);
}
void Sapper_brand_ammo(int dir)
{
	(void)brand_ammo();
}
void Sapper_phosphor_smog(int dir)
{
	fire_ball(GF_LIGHT, dir, 100, 3);
	fire_ball(GF_DARK, dir, 100, 3);
	fire_ball(GF_POIS, dir, 100, 3);
}
void Sapper_airburst(int dir)
{
	fire_ball(GF_SOUND, dir, 150, 2);
	fire_ball(GF_SOUND, dir, 150, 2);
}
void Sapper_freezing_fog(int dir)
{
	fire_ball(GF_ICE, dir, 450, 4);
}
void Sapper_incinerant(int dir)
{
	fire_ball(GF_PLASMA, dir, 550, 5);
}
void Sapper_concussor(int dir)
{
	fire_ball(GF_LIGHT, dir, 400, 8);
	fire_ball(GF_SOUND, dir, 400, 8);
	fire_ball(GF_SHARD, dir, 400, 8);
}

struct spellholder sapper_spell_info[] = {
	{ "Flare", 1, 1, 5, "Creates a small, bright explosion",
		&Sapper_flare, 1 },
	{ "Smoke bomb", 2, 3, 10, "Spews toxic black smoke",
		&Sapper_smoke_bomb, 1 },
	{ "Brand ammo", 4, 6, 15, "Brands ammo with fire, frost, or venom",
		&Sapper_brand_ammo, 0 },
	{ "Phosphor smog", 8, 10, 20, "Generates a bright, poisonous fog",
		&Sapper_phosphor_smog, 1 },
	{ "Airburst", 12, 12, 25, "Produces concussive shockwaves",
		&Sapper_airburst, 1 },
	{ "Freezing fog", 16, 18, 30, "Fills the air with icy mist",
		&Sapper_freezing_fog, 1 },
	{ "Incinerant", 22, 20, 35, "Spills liquid fire upon your foes",
		&Sapper_incinerant, 1 },
	{ "Concussor", 30, 28, 40, "Generates a deafening explosion",
		&Sapper_concussor, 1 },
	NULL,
};

void Shield_stone_strike(int dir)
{
	fire_bolt(GF_SHARD, dir, 5 * p_ptr->lev);
}
void Shield_earthen_shield(int dir)
{
	(void)inc_timed(TMD_SHIELD, randint1(25) + p_ptr->lev, TRUE);
	(void)inc_timed(TMD_OPP_FIRE, randint1(25) + p_ptr->lev, TRUE);
	(void)inc_timed(TMD_OPP_COLD, randint1(25) + p_ptr->lev, TRUE);
	(void)inc_timed(TMD_OPP_ACID, randint1(25) + p_ptr->lev, TRUE);
	(void)inc_timed(TMD_OPP_ELEC, randint1(25) + p_ptr->lev, TRUE);
}
void Shield_fortress(int dir)
{
	(void)door_creation();
}
void Shield_shatter(int dir)
{
	earthquake(p_ptr->py, p_ptr->px, p_ptr->lev / 2);
}
void Shield_battle_cry(int dir)
{
	(void)project_los(GF_SOUND, 3 * p_ptr->lev, FALSE);
}
void Shield_shockwave(int dir)
{
	fire_ball(GF_SOUND, dir, 5 * p_ptr->lev, p_ptr->lev / 10);
}
void Shield_destruction(int dir)
{
	destroy_area(p_ptr->py, p_ptr->px, p_ptr->lev / 2, TRUE);
}

/* Shieldmaidens */
struct spellholder shield_spell_info[] = {
	{ "Stone Strike", 1, 1, 10, "Hurls a bolt of stone shards",
		&Shield_stone_strike, 1 },
	{ "Earthen Shield", 5, 4, 20, "Creates a protective shield of rock",
		&Shield_earthen_shield, 0 },
	{ "Fortress", 10, 8, 25, "Raises stone doors around you",
		&Shield_fortress, 0 },
	{ "Shatter", 15, 14, 30, "Causes an earthquake",
		&Shield_shatter, 0 },
	{ "Battle Cry", 20, 16, 35, "Inflicts sound damage on all visible monsters",
		&Shield_battle_cry, 0 },
	{ "Shockwave", 25, 18, 45, "Projects a stunning ball of sound",
		&Shield_shockwave, 1 },
	{ "Destruction", 30, 22, 55, "Completely levels the area around you",
		&Shield_destruction, 0 },
	NULL,
};


void Sniper_bullet_time(int dir)
{
	(void)inc_timed(TMD_FAST, 100 + p_ptr->lev, TRUE);
}

void Sniper_psi_trance(int dir)
{
	(void)map_area();
	(void)detect_monsters_normal(TRUE);
	(void)detect_monsters_invis(TRUE);
}

void Sniper_spirit_barrier(int dir)
{
	(void)inc_timed(TMD_HERO, 100 + p_ptr->lev, TRUE);
	(void)inc_timed(TMD_SHIELD, 100 + p_ptr->lev, TRUE);
}

void Sniper_mind_lock(int dir)
{
	(void)project_los(GF_OLD_SLEEP, 10 * p_ptr->lev, FALSE);
}

/* Snipers */

struct spellholder sniper_spell_info[] = {
	{ "Bullet Time", 1, 1, 15, "Grants temporary supernatural speed",
			&Sniper_bullet_time, 0 },
	{ "Psi Trance", 1, 1, 20, "Detects monsters and maps the area",
			&Sniper_psi_trance, 0 },
	{ "Spirit Barrier", 1, 1, 25, "Shields you and makes you heroic",
		&Sniper_spirit_barrier, 0 },
	{ "Mind Lock", 1, 1, 35, "Attempts to paralyze all visible monsters",
			&Sniper_mind_lock, 0 },
	NULL,
};

int get_bookless_fail(struct spellholder spell)
{
	int minfail;
	int plev = p_ptr->lev;
	int chance = spell.fail;
	
	/* Level adjustment */
	chance -= 3 * (plev - spell.level);
	
	/* Stat adjustment */
	chance -= 3 * (adj_mag_stat[p_ptr->state.stat_ind[cp_ptr->spell_stat]] - 1);
	
	/* Get the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->state.stat_ind[cp_ptr->spell_stat]];
	if (chance < minfail) chance = minfail;
	
	/* Stunning makes spells harder */
	if (p_ptr->timed[TMD_STUN] > 50) chance += 25;
	else if (p_ptr->timed[TMD_STUN]) chance += 15;
	
	/* Always leave a 5% chance of working */
	if (chance > 95) chance = 95;
	
	return chance;
	
}

void show_bookles_menu(struct spellholder *spell_info)
{
	int i;
	int y = 1;
	int x = 0;
	
	prt("Name | Lv | Mana | Fail | Desc", y, x);
	for (i = 0; spell_info[i].name != NULL; i++)
	{
		if (spell_info[i].level <= p_ptr->lev)
		{
			prt(format("%c) %s %d %d %d%% %s",
				I2A(i),
				spell_info[i].name,
				spell_info[i].level,
				spell_info[i].cost,
				get_bookless_fail(spell_info[i]),
				spell_info[i].desc), y + i + 1, x);
		}
	}
}

void cast_bookless(struct spellholder spell, int dir)
{
	/* Check for blindness and confusion, which prevent casting */
	if (p_ptr->timed[TMD_CONFUSED])
	{
		msg_print("You are too confused!");
		return;
	}
	
	
	if (p_ptr->timed[TMD_BLIND])
	{
		msg_print("You are blind!");
		return;
	}

	if (p_ptr->csp < spell.cost)
	{
		/* Can't cast without enough SP */
		msg_print("Not enough mana to cast this spell.");
		return;
	}

	/* Use mana and energy even if we fail */
	p_ptr->csp -= spell.cost; /* Use some SP */
	p_ptr->redraw |= (PR_MANA); /* Redraw SP counter */
	p_ptr->energy_use = 100; /* Use a turn */

	/* Possibly fail the spell */
	if (randint1(100) <= get_bookless_fail(spell))
	{
		msg_print("You failed to get the spell off!");
		return;
	}
	
	spell.spell(dir);
	return;
}

int get_bookless_size(struct spellholder *spell_info)
{
	int i;
	for (i = 0; spell_info[i].name != NULL; i++);
	return i;
}

void do_cmd_bookless()
{
	char choice; /* Spell choice */
	bool casting; /* Whether the player aborted */
	int dir; /* Spell direction, if applicable */

	/* Points to the structs for whatever realm */
	struct spellholder *spell_info;
	
	/* Disable repeat, we handle it separately
	 * for bookless spells
	 */
	cmd_disable_repeat();
	
	/* Which realm? */
	if player_has(PF_CAST_ASSASSIN)
		spell_info = assassin_spell_info;
	else if player_has(PF_CAST_AVATAR)
		spell_info = avatar_spell_info;
	else if player_has(PF_CAST_PYRO)
		spell_info = pyro_spell_info;
	else if player_has(PF_CAST_REAPER)
		spell_info = reaper_spell_info;
	else if player_has(PF_CAST_SAPPER)
		spell_info = sapper_spell_info;
	else if player_has(PF_CAST_SHIELD)
		spell_info = shield_spell_info;
	else if player_has(PF_CAST_SNIPER)
		spell_info = sniper_spell_info;
	else /* No bookless realm */
		spell_info = NULL;
		
	if (spell_info == NULL)
	{
		msg_print("You have no magical powers.");
		return;
	}
	
	/* Screen state must be saved first to prevent display corruption */
	screen_save();
	show_bookles_menu(spell_info);
	while (casting = get_com("Cast which spell? (Esc to exit)", &choice))
	{
		if (A2I(choice) < 0 ||
			A2I(choice) >= get_bookless_size(spell_info))
			continue; /* Spell does not exist */

		if (spell_info[A2I(choice)].level > p_ptr->lev)
			continue; /* Spell is not yet allowed */
			
		break; /* Spell has been chosen! */
	}

	screen_load(); /* Restore screen state */
	
	/* Return if aborted */
	if (!casting)
		return;
	/* Get a direction if the spell needs it */
	if (spell_info[A2I(choice)].needs_dir && !get_aim_dir(&dir))
		return;		
	/* Set the necessary globals for repeat casting */
	last_bookless_dir = dir;
	last_bookless_spell = spell_info[A2I(choice)];

	/* Cast the spell */
	cast_bookless(spell_info[A2I(choice)], dir);
}

void do_cmd_repeat_bookless()
{
	if (last_bookless_dir == -1) /* Implies no spell also */
		return;
	/* If old target is dead, get a new one */
	if (!target_okay() && last_bookless_dir == 5)
		if (!get_aim_dir(&last_bookless_dir))
			return;
	cast_bookless(last_bookless_spell, last_bookless_dir);
}
