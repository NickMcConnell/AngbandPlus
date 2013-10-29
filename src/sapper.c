/* File: sapper.c
 * Purpose: abilities for the sapper class
 */
 
#include "angband.h"
#include "cave.h"
#include "effects.h"
#include "spells.h"
#include "bookless.h"
#include "target.h"

#define SAP_FLARE		0
#define SAP_SMOKEBOMB	1
#define SAP_BRAND_AMMO	2
#define SAP_PHOS_SMOG	3
#define SAP_AIR_BURST	4
#define SAP_FREEZE_FOG	5
#define SAP_INCINERANT	6
#define SAP_CONCUSSOR	7

struct spellholder sapper_spell_info[] = {
	{ "Flare", 1, 1, 5, "dam 4d4 rad 1" },
	{ "Smoke bomb", 2, 3, 10, "dam 5d15 rad 3" },
	{ "Brand ammo", 4, 6, 15, "" },
	{ "Phosphor smog", 8, 10, 20, "dam 100*3 rad 3" },
	{ "Airburst", 12, 12, 25, "dam 150*2 rad 2" },
	{ "Freezing fog", 16, 18, 30, "dam 450 rad 4" },
	{ "Incinerant", 22, 20, 35, "dam 550 rad 5" },
	{ "Concussor", 30, 28, 40, "dam 400*3 rad 8" },
};

bool cast_sapper_spell(int spell)
{
	int dir;
	
	switch(spell)
	{
		case SAP_FLARE:
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_FIRE, dir, damroll(4, 4), 1);
			fire_ball(GF_LIGHT_WEAK, dir, 0, 10);
			break;
		case SAP_SMOKEBOMB:
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_POIS, dir, damroll(5, 15), 3);
			fire_ball(GF_DARK_WEAK, dir, 0, 10);
			break;
		case SAP_BRAND_AMMO:
			if (!brand_ammo()) return FALSE;
			break;
		case SAP_PHOS_SMOG:
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_LIGHT, dir, 100, 3);
			fire_ball(GF_DARK, dir, 100, 3);
			fire_ball(GF_POIS, dir, 100, 3);
			break;
		case SAP_AIR_BURST:
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_SOUND, dir, 150, 2);
			fire_ball(GF_SOUND, dir, 150, 2);
			break;
		case SAP_FREEZE_FOG:
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_ICE, dir, 450, 4);
			break;
		case SAP_INCINERANT:
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_PLASMA, dir, 550, 5);
			break;
		case SAP_CONCUSSOR:
			if (!get_aim_dir(&dir)) return FALSE;
			fire_ball(GF_LIGHT, dir, 400, 8);
			fire_ball(GF_SOUND, dir, 400, 8);
			fire_ball(GF_SHARD, dir, 400, 8);
	}
	
	return TRUE;
}

int get_sapper_fail(int spell)
{
	int minfail;
	int plev = p_ptr->lev;
	int chance = sapper_spell_info[spell].fail;
	
	/* Level adjustment */
	chance -= 3 * (plev - sapper_spell_info[spell].level);
	
	/* Stat adjustment */
	chance -= 3 * (adj_mag_stat[p_ptr->state.stat_ind[cp_ptr->spell_stat]] - 1);
	
	/* Get the minimum fail rate */
	minfail = adj_mag_fail[p_ptr->state.stat_ind[cp_ptr->spell_stat]];
	if (chance < minfail) chance = minfail;
	
	/* Stunning makes spells harder */
	if (p_ptr->timed[TMD_STUN] > 50) chance += 25;
	else if (p_ptr->timed[TMD_STUN]) chance += 15;
	
	/* Always leave a 5% chance of working */
	if (chance > 95) chance = 95;
	
	return chance;
}

void print_sapper_menu(void)
{
	int i;
	int y = 1;
	int x = 0;
	
	prt("Name    Lv    Mana    Fail    Info ", y, x);
	for (i = 0; i <= SAP_CONCUSSOR; i++)
	{
		if (sapper_spell_info[i].level <= p_ptr->lev)
		{
			prt(format("%c) %16s %4d %4d %4d%% %s",
				I2A(i),
				sapper_spell_info[i].name,
				sapper_spell_info[i].level,
				sapper_spell_info[i].cost,
				get_sapper_fail(i),
				sapper_spell_info[i].desc), y + i + 1, x);
		}
	}
	
	return;
}

void do_cmd_sapper(void)
{
	char choice;
	bool casting;
	
	/* Not while confused */
	if (p_ptr->timed[TMD_CONFUSED])
	{
		msg_print("You are too confused!");
		return;
	}
	/* Nor while blind */
	if (p_ptr->timed[TMD_BLIND])
	{
		msg_print("You are blind!");
		return;
	}
	
	screen_save();
	print_sapper_menu();
	while (casting = get_com("Use which recipe? (Esc to exit)", &choice))
	{
		if (sapper_spell_info[A2I(tolower(choice))].level > p_ptr->lev)
			continue;
		
		if (A2I(choice) < 0 || A2I(choice) > SAP_CONCUSSOR)
			continue;

		if (p_ptr->csp < sapper_spell_info[A2I(choice)].cost)
		{
			msg_print("You are too tired to use this recipe.");
			continue;
		}

		break;
	}
	
	screen_load();
	if (casting)
	{
		if (randint1(100) <= get_sapper_fail(A2I(choice)))
		{
			msg_print("Oh no! The recipe blows up in your face!");
			take_hit(damroll(2, 10), "a botched explosive concoction");
		}
		else if (!cast_sapper_spell(A2I(choice)))
			return;
		p_ptr->csp -= sapper_spell_info[A2I(choice)].cost;
		p_ptr->redraw |= (PR_MANA);
		p_ptr->energy_use = 100;
	}

	return;
}
