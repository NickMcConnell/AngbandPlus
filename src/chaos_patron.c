#include "angband.h"


#define REW_POLY_SLF    1
#define REW_GAIN_EXP    2
#define REW_LOSE_EXP    3
#define REW_GOOD_OBJ    4
#define REW_GREA_OBJ    5
#define REW_CHAOS_WP    6
#define REW_GOOD_OBS    7
#define REW_GREA_OBS    8
#define REW_TY_CURSE    9
#define REW_SUMMON_M    10
#define REW_H_SUMMON    11
#define REW_DO_HAVOC    12
#define REW_GAIN_ABL    13
#define REW_LOSE_ABL    14
#define REW_RUIN_ABL    15
#define REW_AUGM_ABL    16
#define REW_POLY_WND    17
#define REW_HEAL_FUL    18
#define REW_HURT_LOT    19
#define REW_CURSE_WP    20
#define REW_CURSE_AR    21
#define REW_PISS_OFF    22
#define REW_WRATH       23
#define REW_DESTRUCT    24
#define REW_GENOCIDE    25
#define REW_MASS_GEN    26
#define REW_DISPEL_C    27
#define REW_DAMNATION   28 /* new chaos mage rewards */
#define REW_DISORDER    29
#define REW_EXULTATION  30
#define REW_ENERGISE    31
#define REW_DEVICE      32 /* fin */
#define REW_IGNORE      33
#define REW_SER_UNDE    34
#define REW_SER_DEMO    35
#define REW_SER_MONS    36  
#define REW_MUTATE		37

/* reward types */
#define REW_TYPE_PUNISH  0
#define REW_TYPE_ANNOY   1
#define REW_TYPE_REWARD  2
#define REW_TYPE_FAVOUR  3
#define REW_CATEGORIES   4

/* patron attitudes */
#define AMBIVALENT 0
#define SCORNFUL 1
#define AMUSED 2
#define INTERESTED 3
#define APPROVING 4
#define ATTITUDE_MAX 5


/*todo: this*/
struct chaos_patron
{
	int stat;
	int rewards[REW_CATEGORIES][5];
	cptr name;
	cptr title;
	int noticechance[PATRON_EFFECT_MAX];
	int attitudes[PATRON_EFFECT_MAX];
};

int chaos_stats[MAX_PATRON] =
{
	A_CON,  /* Slortar */
	A_CON,  /* Mabelode */
	A_STR,  /* Chardros */
	A_STR,  /* Hionhurn */
	A_STR,  /* Xiombarg */

	A_INT,  /* Pyaray */
	A_STR,  /* Balaan */
	A_INT,  /* Arioch */
	A_CON,  /* Eequor */
	A_CHR,  /* Narjhan */

	-1,     /* Balo */
	A_STR,  /* Khorne */
	A_CHR,  /* Slaanesh */
	A_CON,  /* Nurgle */
	A_INT,  /* Tzeentch */

	A_STR,  /* Khaine */
};

bool worships_chaos(void) 
{	
	return (p_ptr->pclass == CLASS_CHAOS_WARRIOR || p_ptr->pclass == CLASS_CHAOS_MAGE || mut_present(MUT_CHAOS_GIFT));
}

void chaos_forge_weapon() {
	object_type forge;
	int         dummy = 0, dummy2 = 0;

	dummy = TV_SWORD;
	switch (randint1(p_ptr->lev))
	{
	case 0: case 1:
		dummy2 = SV_DAGGER;
		break;
	case 2: case 3:
		dummy2 = SV_MAIN_GAUCHE;
		break;
	case 4:
		dummy2 = SV_TANTO;
		break;
	case 5: case 6:
		dummy2 = SV_RAPIER;
		break;
	case 7: case 8:
		dummy2 = SV_SMALL_SWORD;
		break;
	case 9: case 10:
		dummy2 = SV_BASILLARD;
		break;
	case 11: case 12: case 13:
		dummy2 = SV_SHORT_SWORD;
		break;
	case 14: case 15:
		dummy2 = SV_SABRE;
		break;
	case 16: case 17:
		dummy2 = SV_CUTLASS;
		break;
	case 18:
		dummy2 = SV_WAKIZASHI;
		break;
	case 19:
		dummy2 = SV_KHOPESH;
		break;
	case 20:
		dummy2 = SV_TULWAR;
		break;
	case 21:
		dummy2 = SV_BROAD_SWORD;
		break;
	case 22: case 23:
		dummy2 = SV_LONG_SWORD;
		break;
	case 24: case 25:
		dummy2 = SV_SCIMITAR;
		break;
	case 26:
		dummy2 = SV_NINJATO;
		break;
	case 27:
		dummy2 = SV_KATANA;
		break;
	case 28: case 29:
		dummy2 = SV_BASTARD_SWORD;
		break;
	case 30:
		dummy2 = SV_GREAT_SCIMITAR;
		break;
	case 31:
		dummy2 = SV_CLAYMORE;
		break;
	case 32:
		dummy2 = SV_ESPADON;
		break;
	case 33:
		dummy2 = SV_TWO_HANDED_SWORD;
		break;
	case 34:
		dummy2 = SV_FLAMBERGE;
		break;
	case 35:
		dummy2 = SV_NO_DACHI;
		break;
	case 36:
		dummy2 = SV_EXECUTIONERS_SWORD;
		break;
	case 37:
		dummy2 = SV_ZWEIHANDER;
		break;
	case 38:
		dummy2 = SV_HAYABUSA;
		break;
	default:
		dummy2 = SV_BLADE_OF_CHAOS;
	}

	object_prep(&forge, lookup_kind(dummy, dummy2));
	forge.to_h = 3 + randint1(dun_level) % 10;
	forge.to_d = 3 + randint1(dun_level) % 10;
	one_resistance(&forge);
	forge.name2 = EGO_WEAPON_CHAOS;

	drop_near(&forge, -1, py, px);
}
void chaos_gift_device() {
	object_type forge;
	int         dummy = 0;

	switch (randint0(4))
	{
	case 0:
		dummy = TV_STAFF;
		break;
	case 1:
		dummy = TV_ROD;
		break;
	case 2: case 3:
		dummy = TV_WAND;
		break;
	}

	object_prep(&forge, lookup_kind(dummy, 0));
	if (!obj_create_device(&forge, rand_range(p_ptr->lev, p_ptr->lev * 2), 0, AM_GOOD))
	{
		obj_create_device(&forge, rand_range(p_ptr->lev, p_ptr->lev * 2), 0, 0);
	}

	drop_near(&forge, -1, py, px);
}
void chaos_patron_event(int effect)
{
	char        wrath_reason[32] = "";
	int         count = 0;
	switch (effect)
	{
	case REW_POLY_SLF:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		if (one_in_(3))
		{
			msg_print("'Thou needst a new form, mortal!'");
			do_poly_self();
		}
		else
		{
			msg_print("'Thou needst a different form, mortal!'");

			/* This is a temporary polymorph */
			do_spell(REALM_CHAOS, 28, SPELL_CAST);
		}

		break;
	case REW_GAIN_EXP:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Well done, mortal! Lead on!'");
		if (p_ptr->prace == RACE_ANDROID)
			msg_print("But, nothing happen.");
		else if (p_ptr->exp < PY_MAX_EXP)
		{
			s32b ee = (p_ptr->exp / 2) + 10;
			if (ee > 100000L) ee = 100000L;
			msg_print("You feel more experienced.");
			gain_exp(ee);
		}
		break;
	case REW_LOSE_EXP:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Continue your performance, slave.'");

		if (p_ptr->prace == RACE_ANDROID)
			msg_print("But, nothing happen.");
		else
		{
			/* Lose some experience (permanently) */
			s32b ee = (p_ptr->exp / 6);
			if (ee > 100000L) ee = 100000L;
			p_ptr->exp -= ee;
			p_ptr->max_exp -= ee;
			check_experience();
		}
		break;
	case REW_GOOD_OBJ:
		msg_format("The voice of %s whispers:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Use my gift wisely.'");
		acquirement(py, px, 1, FALSE, FALSE);
		break;
	case REW_GREA_OBJ:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Use my gift wisely.'");

		acquirement(py, px, 1, TRUE, FALSE);
		break;
	case REW_CHAOS_WP:
	{
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Thy deed hath earned thee a worthy blade.'");

		chaos_forge_weapon();
		break;
	}
	case REW_GOOD_OBS:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Thy deed hath earned thee a worthy reward.'");

		acquirement(py, px, randint1(2) + 1, FALSE, FALSE);
		break;
	case REW_GREA_OBS:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Behold, mortal, how generously I reward thy loyalty.'");

		acquirement(py, px, randint1(2) + 1, TRUE, FALSE);
		break;
	case REW_TY_CURSE:
		msg_format("The voice of %s thunders:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Thou art growing arrogant, mortal.'");

		activate_ty_curse(FALSE, &count);
		break;
	case REW_SUMMON_M:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'My pets, destroy the arrogant mortal!'");
		for (int i = 0; i < randint1(5) + 1; i++)
			summon_specific(0, py, px, dun_level, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
		break;
	case REW_H_SUMMON:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Thou needst worthier opponents!'");
		activate_hi_summon(py, px, FALSE);
		break;
	case REW_DO_HAVOC:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Death and destruction! This pleaseth me!'");
		call_chaos(100);
		break;
	case REW_GAIN_ABL:
		msg_format("The voice of %s rings out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Stay, mortal, and let me mold thee.'");
		if (one_in_(3) && !(chaos_stats[p_ptr->chaos_patron] < 0))
			do_inc_stat(chaos_stats[p_ptr->chaos_patron]);
		else
			do_inc_stat(randint0(6));
		break;
	case REW_LOSE_ABL:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'I grow tired of thee, mortal.'");

		if (one_in_(3) && !(chaos_stats[p_ptr->chaos_patron] < 0))
			do_dec_stat(chaos_stats[p_ptr->chaos_patron]);
		else
			do_dec_stat(randint0(6));
		break;
	case REW_RUIN_ABL:
		msg_format("The voice of %s thunders:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Thou needst a lesson in humility, mortal!'");
		msg_print("You feel less powerful!");

		for (int i = 0; i < 6; i++)
			dec_stat(i, 10 + randint1(15), TRUE);
		break;
	case REW_POLY_WND:
		msg_format("You feel the power of %s touch you.", chaos_patrons[p_ptr->chaos_patron]);
		do_poly_wounds();
		break;
	case REW_AUGM_ABL:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Receive this modest gift from me!'");
		for (int i = 0; i < 6; i++)
			do_inc_stat(i);
		break;
	case REW_HURT_LOT:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Suffer, pathetic fool!'");
		fire_ball(GF_DISINTEGRATE, 0, p_ptr->lev * 4, 4);
		take_hit(DAMAGE_NOESCAPE, p_ptr->lev * 4, wrath_reason);
		break;
	case REW_HEAL_FUL:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Rise, my servant!'");
		restore_level();
		set_poisoned(0, TRUE);
		set_blind(0, TRUE);
		set_confused(0, TRUE);
		set_image(0, TRUE);
		set_stun(0, TRUE);
		set_cut(0, TRUE);
		hp_player(5000);
		for (int i = 0; i < 6; i++)
			do_res_stat(i);
		break;
	case REW_CURSE_WP:
	{
		int slot = equip_random_slot(object_is_melee_weapon);
		if (slot)
		{
			msg_format("The voice of %s booms out:",
				chaos_patrons[p_ptr->chaos_patron]);
			msg_print("'Thou reliest too much on thy weapon.'");
			curse_weapon(FALSE, slot);
		}
		break;
	}
	case REW_CURSE_AR:
	{
		int slot = equip_random_slot(object_is_armour);
		if (slot)
		{
			msg_format("The voice of %s booms out:",
				chaos_patrons[p_ptr->chaos_patron]);
			msg_print("'Thou reliest too much on thine equipment.'");
			curse_armor(slot);
		}
		break;
	}
	case REW_PISS_OFF:
		msg_format("The voice of %s whispers:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Now thou shalt pay for annoying me.'");
		switch (randint1(4))
		{
		case 1:
			activate_ty_curse(FALSE, &count);
			break;
		case 2:
			activate_hi_summon(py, px, FALSE);
			break;
		case 3:
			if (one_in_(2))
			{
				int slot = equip_random_slot(object_is_melee_weapon);
				if (slot)
					curse_weapon(FALSE, slot);
			}
			else
			{
				int slot = equip_random_slot(object_is_armour);
				if (slot)
					curse_armor(slot);
			}
			break;
		default:
			for (int i = 0; i < 6; i++)
				dec_stat(i, 10 + randint1(15), TRUE);
			break;
		}
		break;
	case REW_WRATH:
		msg_format("The voice of %s thunders:", chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Die, mortal!'");

		take_hit(DAMAGE_LOSELIFE, p_ptr->lev * 4, wrath_reason);
		for (int i = 0; i < 6; i++)
			dec_stat(i, 10 + randint1(15), FALSE);
		activate_hi_summon(py, px, FALSE);
		activate_ty_curse(FALSE, &count);
		if (one_in_(2))
		{
			int slot = equip_random_slot(object_is_melee_weapon);
			if (slot)
				curse_weapon(FALSE, slot);
		}
		if (one_in_(2))
		{
			int slot = equip_random_slot(object_is_armour);
			if (slot)
				curse_armor(slot);
		}
		break;
	case REW_DESTRUCT:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Death and destruction! This pleaseth me!'");
		destroy_area(py, px, 25, 3 * p_ptr->lev);
		break;
	case REW_GENOCIDE:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Let me relieve thee of thine oppressors!'");
		symbol_genocide(0, FALSE);
		break;
	case REW_MASS_GEN:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Let me relieve thee of thine oppressors!'");
		mass_genocide(0, FALSE);
		break;
	case REW_DISPEL_C:
		msg_format("You can feel the power of %s assault your enemies!",
			chaos_patrons[p_ptr->chaos_patron]);
		dispel_monsters(p_ptr->lev * 4);
		break;
		/* these next few entries exist outside the standard patron tables */
	case REW_DAMNATION:
		/* no saves! */
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("Your soul is mine mortal!");
		switch (randint0(4)) {
		case 0:
			set_confused(p_ptr->confused + randint0(6) + 3, FALSE);
			break;
		case 1:
			set_image(p_ptr->image + randint0(12) + 6, FALSE);
			break;
		case 2:
			curse_equipment(100, 1);
			break;
		case 3:
			p_ptr->csp = randint0(p_ptr->csp / 2);
			break;
		}
		break;
	case REW_DISORDER:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("Let Chaos reign!");
		project_hack(GF_CHAOS, randint1(p_ptr->lev));
		break;
	case REW_EXULTATION:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("Onward my servant!");
		switch (randint0(4))
		{
		case 0:
			set_hero(p_ptr->hero + rand_range(50, 100), FALSE);
			break;
		case 1:
			set_blessed(p_ptr->blessed + rand_range(50, 100), FALSE);
			break;
		case 2:
			set_fast(p_ptr->fast + rand_range(30, 60), FALSE);
			break;
		case 3:
			set_tim_sh_fire(p_ptr->tim_sh_fire + rand_range(20, 70), FALSE);
			break;
		case 4:
			set_mimic(rand_range(50, 100), MIMIC_DEMON, FALSE);
			break;
		}
		break;
	case REW_ENERGISE:
		msg_format("The voice of %s booms out:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("Let the power of chaos flow!");
		do_energise();
		break;
	case REW_DEVICE:
		msg_format("The voice of %s whispers:",
			chaos_patrons[p_ptr->chaos_patron]);
		msg_print("'Use my gift wisely.'");
		chaos_gift_device();
		break;
	case REW_IGNORE:
		msg_format("%s pointedly ignores you.",
			chaos_patrons[p_ptr->chaos_patron]);
		break;
	case REW_SER_DEMO:
		msg_format("%s rewards you with a demonic servant!", chaos_patrons[p_ptr->chaos_patron]);
		if (!summon_specific(-1, py, px, dun_level, SUMMON_DEMON, PM_FORCE_PET))
			msg_print("Nobody ever turns up...");
		break;
	case REW_SER_MONS:
		msg_format("%s rewards you with a servant!", chaos_patrons[p_ptr->chaos_patron]);
		if (!summon_specific(-1, py, px, dun_level, 0, PM_FORCE_PET))
			msg_print("Nobody ever turns up...");
		break;
	case REW_SER_UNDE:
		msg_format("%s rewards you with an undead servant!", chaos_patrons[p_ptr->chaos_patron]);
		if (!summon_specific(-1, py, px, dun_level, SUMMON_UNDEAD, PM_FORCE_PET))
			msg_print("Nobody ever turns up...");
		break;
	case REW_MUTATE:
		if (mut_count(mut_unlocked_pred) > randint1(5))
		{
			if (one_in_(3))
			{
				int newmuts = 1;
				msg_format("%^s rewards you with new mutations!",
					chaos_patrons[p_ptr->chaos_patron]);
				mut_gain_random(NULL);
				while (one_in_(newmuts))
				{
					mut_lose_random(NULL);
					mut_gain_random(NULL);
					newmuts++;
				}
			}
			else
			{
				msg_format("%^s rewards you with a new mutation!",
					chaos_patrons[p_ptr->chaos_patron]);
				mut_gain_random(NULL);
			}
		}
		else
		{
			{
				msg_format("%^s rewards you with a mutation!",
					chaos_patrons[p_ptr->chaos_patron]);
				mut_gain_random(NULL);
			}
		}
		break;
	default:
		msg_format("The voice of %s stammers:", chaos_patrons[p_ptr->chaos_patron]);
		msg_format("'Uh... uh... the answer's %d, what's the question?'", effect);
	}
}

int chaos_rewards[MAX_PATRON][REW_CATEGORIES][5] =
{
	/* Slortar the Old: */
	{
		{ REW_WRATH, REW_CURSE_WP, REW_CURSE_AR, REW_RUIN_ABL, REW_LOSE_ABL },
		{ REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_POLY_WND, REW_POLY_SLF },
		{ REW_POLY_SLF, REW_POLY_SLF, REW_GAIN_ABL, REW_GAIN_ABL, REW_GAIN_EXP },
		{ REW_GOOD_OBJ, REW_CHAOS_WP, REW_GREA_OBJ, REW_AUGM_ABL, REW_AUGM_ABL }
	},

	/* Mabelode the Faceless: */
	{
		{ REW_WRATH, REW_CURSE_WP, REW_CURSE_AR, REW_H_SUMMON, REW_SUMMON_M },
		{ REW_SUMMON_M, REW_IGNORE, REW_IGNORE, REW_POLY_WND, REW_POLY_WND },
		{ REW_POLY_SLF, REW_HEAL_FUL, REW_HEAL_FUL, REW_GAIN_ABL, REW_SER_UNDE },
		{ REW_CHAOS_WP, REW_GOOD_OBJ, REW_GOOD_OBJ, REW_GOOD_OBS, REW_GOOD_OBS }
	},

	/* Chardros the Reaper: */
	{
		{ REW_WRATH, REW_WRATH, REW_HURT_LOT, REW_PISS_OFF, REW_H_SUMMON },
		{ REW_SUMMON_M, REW_IGNORE, REW_IGNORE, REW_DESTRUCT, REW_SER_UNDE },
		{ REW_GENOCIDE, REW_MASS_GEN, REW_MASS_GEN, REW_DISPEL_C, REW_GOOD_OBJ },
		{ REW_CHAOS_WP, REW_GOOD_OBS, REW_GOOD_OBS, REW_AUGM_ABL, REW_AUGM_ABL }
	},

	/* Hionhurn the Executioner: */
	{
		{ REW_WRATH, REW_WRATH, REW_CURSE_WP, REW_CURSE_AR, REW_RUIN_ABL },
		{ REW_IGNORE, REW_IGNORE, REW_SER_UNDE, REW_DESTRUCT, REW_GENOCIDE },
		{ REW_MASS_GEN, REW_MASS_GEN, REW_HEAL_FUL, REW_GAIN_ABL, REW_GAIN_ABL },
		{ REW_CHAOS_WP, REW_GOOD_OBS, REW_GOOD_OBS, REW_AUGM_ABL, REW_AUGM_ABL }
	},

	/* Xiombarg the Sword-Queen: */
	{
		{ REW_TY_CURSE, REW_TY_CURSE, REW_PISS_OFF, REW_RUIN_ABL, REW_LOSE_ABL },
		{ REW_IGNORE, REW_POLY_SLF, REW_POLY_SLF, REW_POLY_WND, REW_POLY_WND },
		{ REW_GENOCIDE, REW_DISPEL_C, REW_GOOD_OBJ, REW_GOOD_OBJ, REW_SER_MONS },
		{ REW_GAIN_ABL, REW_CHAOS_WP, REW_GAIN_EXP, REW_AUGM_ABL, REW_GOOD_OBS }
	},


	/* Pyaray the Tentacled Whisperer of Impossible Secretes: */
	{
		{ REW_WRATH, REW_TY_CURSE, REW_PISS_OFF, REW_H_SUMMON, REW_H_SUMMON },
		{ REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_POLY_WND, REW_POLY_SLF },
		{ REW_POLY_SLF, REW_SER_DEMO, REW_HEAL_FUL, REW_GAIN_ABL, REW_GAIN_ABL },
		{ REW_CHAOS_WP, REW_DO_HAVOC, REW_GOOD_OBJ, REW_GREA_OBJ, REW_GREA_OBS }
	},

	/* Balaan the Grim: */
	{
		{ REW_TY_CURSE, REW_HURT_LOT, REW_CURSE_WP, REW_CURSE_AR, REW_RUIN_ABL },
		{ REW_SUMMON_M, REW_LOSE_EXP, REW_POLY_SLF, REW_POLY_SLF, REW_POLY_WND },
		{ REW_SER_UNDE, REW_HEAL_FUL, REW_HEAL_FUL, REW_GAIN_EXP, REW_GAIN_EXP },
		{ REW_CHAOS_WP, REW_GOOD_OBJ, REW_GOOD_OBS, REW_GREA_OBS, REW_AUGM_ABL }
	},
	/* Arioch, Duke of Hell: */
	{
		{ REW_WRATH, REW_PISS_OFF, REW_RUIN_ABL, REW_LOSE_EXP, REW_H_SUMMON },
		{ REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_POLY_SLF },
		{ REW_POLY_SLF, REW_MASS_GEN, REW_SER_DEMO, REW_HEAL_FUL, REW_CHAOS_WP },
		{ REW_CHAOS_WP, REW_GOOD_OBJ, REW_GAIN_EXP, REW_GREA_OBJ, REW_AUGM_ABL }
	},

	/* Eequor, Blue Lady of Dismay: */
	{
		{ REW_WRATH, REW_TY_CURSE, REW_PISS_OFF, REW_CURSE_WP, REW_RUIN_ABL },
		{ REW_IGNORE, REW_IGNORE, REW_POLY_SLF, REW_POLY_SLF, REW_POLY_WND },
		{ REW_GOOD_OBJ, REW_GOOD_OBJ, REW_SER_MONS, REW_HEAL_FUL, REW_GAIN_EXP },
		{ REW_GAIN_ABL, REW_CHAOS_WP, REW_GOOD_OBS, REW_GREA_OBJ, REW_AUGM_ABL }
	},

	/* Narjhan, Lord of Beggars: */
	{
		{ REW_WRATH, REW_CURSE_AR, REW_CURSE_WP, REW_CURSE_WP, REW_CURSE_AR },
		{ REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_POLY_SLF, REW_POLY_SLF },
		{ REW_POLY_WND, REW_HEAL_FUL, REW_HEAL_FUL, REW_GAIN_EXP, REW_AUGM_ABL },
		{ REW_GOOD_OBJ, REW_GOOD_OBJ, REW_CHAOS_WP, REW_GREA_OBJ, REW_GREA_OBS }
	},

	/* Balo the Jester: */
	{
		{ REW_WRATH, REW_SER_DEMO, REW_CURSE_WP, REW_CURSE_AR, REW_LOSE_EXP },
		{ REW_GAIN_ABL, REW_LOSE_ABL, REW_POLY_WND, REW_POLY_SLF, REW_IGNORE },
		{ REW_DESTRUCT, REW_MASS_GEN, REW_CHAOS_WP, REW_GREA_OBJ, REW_HURT_LOT },
		{ REW_AUGM_ABL, REW_RUIN_ABL, REW_H_SUMMON, REW_GREA_OBS, REW_AUGM_ABL }
	},

	/* Khorne the Bloodgod: */
	{
		{ REW_WRATH, REW_HURT_LOT, REW_HURT_LOT, REW_H_SUMMON, REW_H_SUMMON },
		{ REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_SER_MONS, REW_SER_DEMO },
		{ REW_POLY_SLF, REW_POLY_WND, REW_HEAL_FUL, REW_GOOD_OBJ, REW_GOOD_OBJ },
		{ REW_CHAOS_WP, REW_GOOD_OBS, REW_GOOD_OBS, REW_GREA_OBJ, REW_GREA_OBS }
	},

	/* Slaanesh: */
	{
		{ REW_WRATH, REW_PISS_OFF, REW_PISS_OFF, REW_RUIN_ABL, REW_LOSE_ABL },
		{ REW_LOSE_EXP, REW_IGNORE, REW_IGNORE, REW_POLY_WND, REW_SER_DEMO },
		{ REW_POLY_SLF, REW_HEAL_FUL, REW_HEAL_FUL, REW_GOOD_OBJ, REW_GAIN_EXP },
		{ REW_GAIN_EXP, REW_CHAOS_WP, REW_GAIN_ABL, REW_GREA_OBJ, REW_AUGM_ABL }
	},

	/* Nurgle: */
	{
		{ REW_WRATH, REW_PISS_OFF, REW_HURT_LOT, REW_RUIN_ABL, REW_LOSE_ABL },
		{ REW_LOSE_EXP, REW_IGNORE, REW_IGNORE, REW_IGNORE, REW_POLY_SLF },
		{ REW_POLY_SLF, REW_POLY_WND, REW_HEAL_FUL, REW_GOOD_OBJ, REW_GAIN_ABL },
		{ REW_GAIN_ABL, REW_SER_UNDE, REW_CHAOS_WP, REW_GREA_OBJ, REW_AUGM_ABL }
	},
	/* Tzeentch: */
	{
		{ REW_WRATH, REW_CURSE_WP, REW_CURSE_AR, REW_RUIN_ABL, REW_LOSE_ABL },
		{ REW_LOSE_EXP, REW_IGNORE, REW_POLY_SLF, REW_POLY_SLF, REW_POLY_SLF },
		{ REW_POLY_SLF, REW_POLY_WND, REW_HEAL_FUL, REW_CHAOS_WP, REW_GREA_OBJ },
		{ REW_GAIN_ABL, REW_GAIN_ABL, REW_GAIN_EXP, REW_GAIN_EXP, REW_AUGM_ABL }
	},

	/* Khaine: */
	{
		{ REW_WRATH, REW_HURT_LOT, REW_PISS_OFF, REW_LOSE_ABL, REW_LOSE_EXP },
		{ REW_IGNORE, REW_IGNORE, REW_DISPEL_C, REW_DO_HAVOC, REW_DO_HAVOC, },
		{ REW_POLY_SLF, REW_POLY_SLF, REW_GAIN_EXP, REW_GAIN_ABL, REW_GAIN_ABL },
		{ REW_SER_MONS, REW_GOOD_OBJ, REW_CHAOS_WP, REW_GREA_OBJ, REW_GOOD_OBS }
	}
};

void chaos_patron_reward(int category)
{
	int         type, effect;
	type = randint1(5);
	if (type < 1) type = 1;
	if (type > 5) type = 5;
	type--;
	effect = chaos_rewards[p_ptr->chaos_patron][category][type];
	chaos_patron_event(effect);
}

/* Patron name and title */
cptr chaos_patron_name(int which)
{
	cptr name;
	switch (which)
	{
	case 0: name = "Slortar the Old"; break;
	case 1: name = "Mabelode the Faceless"; break;
	case 2: name = "Chardros the Reaper"; break;
	case 3: name = "Hionhurn the Executioner"; break;
	case 4: name = "Xiombarg the Sword-Queen"; break;
	case 5: name = "Pyaray the Tentacled Whisperer of Impossible Secrets"; break;
	case 6: name = "Balaan the Grim"; break;
	case 7: name = "Arioch, Duke of Hell"; break;
	case 8: name = "Eequor, Blue Lady of Dismay"; break;
	case 9: name = "Narjhan, Lord of Beggars"; break;
	case 10: name = "Balo the Jester"; break;
	case 11: name = "Khorne the Blood God"; break;
	case 12: name = "Slaanesh the Prince of Pleasure"; break;
	case 13: name = "Nurgle the Plague Lord"; break;
	case 14: name = "Tzeentch the Changer of Ways"; break;
	case 15: name = "Khaela Mensha Khaine"; break;
	default: name = "Gwarl the Destroyer"; break;
	}
	return name;
}

/* assigns chances for a patron noticing the player */
/* melee hit , kill weakling, kill, kill unique, kill famous, kill good, kill demon, cast, villiany, chance, take hit, level up */
int chaos_effect_notice[MAX_PATRON][PATRON_EFFECT_MAX] =
{
	{ 343, 169, 27, 7, 1, 7, 11, 131, 21, 14, 10, 1 },
	{ 343, 169, 21, 7, 1, 7, 11, 131, 21, 14, 7, 1 },
	{ 343, 169, 27, 7, 1, 7, 11, 131, 21, 14, 7, 1 },
	{ 343, 169, 27, 5, 1, 7, 11, 131, 21, 14, 7, 1 },
	{ 313, 169, 27, 7, 1, 7, 11, 131, 21, 14, 7, 1 },
	{ 343, 169, 27, 7, 1, 7, 11, 131, 21, 14, 7, 1 },
	{ 343, 218, 27, 7, 1, 7, 8, 131, 21, 14, 7, 1 },
	{ 343, 169, 27, 7, 1, 7, 11, 131, 17, 14, 7, 1 },
	{ 343, 169, 27, 7, 1, 7, 11, 131, 21, 14, 7, 1 },
	{ 343, 91, 27, 7, 1, 7, 11, 131, 21, 14, 7, 1 },
	{ 343, 169, 27, 7, 1, 7, 11, 131, 21, 11, 7, 1 },
	{ 343, 169, 27, 5, 1, 7, 11, 131, 21, 14, 7, 1 },
	{ 343, 169, 27, 7, 1, 5, 11, 131, 21, 14, 7, 1 },
	{ 343, 169, 27, 7, 1, 7, 11, 131, 21, 14, 5, 1 },
	{ 343, 169, 27, 7, 1, 7, 11, 131, 27, 11, 7, 1 },
	{ 343, 169, 27, 7, 1, 7, 11, 131, 21, 14, 7, 1 },
};

/* defines attitudes toward player actions */
/* melee hit , kill weakling, kill, kill unique, kill famous, kill good, kill demon, cast, villiany, chance, take hit, level up */
int chaos_attitudes[MAX_PATRON][PATRON_EFFECT_MAX] =
{
	{ AMBIVALENT, SCORNFUL, AMBIVALENT, AMUSED, APPROVING, APPROVING, INTERESTED, AMBIVALENT, AMUSED, AMUSED, AMBIVALENT, APPROVING },
	{ AMBIVALENT, SCORNFUL, AMBIVALENT, AMUSED, APPROVING, APPROVING, INTERESTED, AMBIVALENT, AMUSED, AMUSED, AMBIVALENT, APPROVING },
	{ AMBIVALENT, AMBIVALENT, AMBIVALENT, AMUSED, APPROVING, APPROVING, AMBIVALENT, AMBIVALENT, AMUSED, AMUSED, AMBIVALENT, APPROVING },
	{ AMBIVALENT, AMBIVALENT, AMBIVALENT, AMUSED, APPROVING, APPROVING, INTERESTED, AMBIVALENT, AMUSED, AMUSED, AMBIVALENT, APPROVING },
	{ AMBIVALENT, SCORNFUL, AMBIVALENT, AMUSED, APPROVING, APPROVING, INTERESTED, AMBIVALENT, AMUSED, AMUSED, AMBIVALENT, APPROVING },
	{ AMBIVALENT, SCORNFUL, AMBIVALENT, AMUSED, APPROVING, APPROVING, INTERESTED, INTERESTED, AMUSED, AMUSED, AMBIVALENT, APPROVING },
	{ AMBIVALENT, SCORNFUL, AMBIVALENT, AMUSED, APPROVING, APPROVING, AMUSED, AMBIVALENT, AMUSED, AMUSED, AMBIVALENT, APPROVING },
	{ AMBIVALENT, SCORNFUL, AMBIVALENT, AMUSED, APPROVING, APPROVING, INTERESTED, AMBIVALENT, AMUSED, AMUSED, INTERESTED, APPROVING },
	{ AMBIVALENT, SCORNFUL, INTERESTED, AMUSED, APPROVING, APPROVING, INTERESTED, AMBIVALENT, AMUSED, AMUSED, INTERESTED, APPROVING },
	{ AMBIVALENT, SCORNFUL, INTERESTED, AMUSED, APPROVING, APPROVING, INTERESTED, AMBIVALENT, AMUSED, AMUSED, AMBIVALENT, APPROVING },
	{ AMBIVALENT, SCORNFUL, AMBIVALENT, AMUSED, APPROVING, APPROVING, INTERESTED, AMBIVALENT, AMUSED, AMUSED, AMBIVALENT, APPROVING },
	{ AMUSED, SCORNFUL, AMBIVALENT, AMUSED, APPROVING, APPROVING, INTERESTED, SCORNFUL, AMUSED, AMUSED, AMBIVALENT, APPROVING },
	{ AMBIVALENT, INTERESTED, AMBIVALENT, AMUSED, APPROVING, APPROVING, INTERESTED, AMBIVALENT, AMUSED, AMUSED, AMBIVALENT, APPROVING },
	{ AMBIVALENT, SCORNFUL, AMBIVALENT, AMUSED, APPROVING, APPROVING, INTERESTED, AMBIVALENT, AMUSED, AMUSED, APPROVING, APPROVING },
	{ AMBIVALENT, SCORNFUL, AMBIVALENT, AMUSED, APPROVING, APPROVING, INTERESTED, AMUSED, AMUSED, AMUSED, AMBIVALENT, APPROVING },
	{ AMBIVALENT, AMBIVALENT, AMBIVALENT, AMUSED, APPROVING, APPROVING, AMUSED, AMBIVALENT, AMUSED, AMUSED, AMBIVALENT, APPROVING }
};

/* do we want a punishment, a questionable reward, or a good reward? */
/* extra logic goes in this function to retain the integrity of the Zangband reward tables */
/* unfortunately khorne makes for a rather benign deity unless you manage to piss him off badly */
/* his scornful attitude toward spellcasting leads to some special behaviour */
void chaos_choose_effect(int reason)
{
	int attitude = chaos_attitudes[p_ptr->chaos_patron][reason];
	int punish_chance = 0;
	int reward_chance = 0;
	switch (attitude)
	{
	case AMBIVALENT:
		punish_chance = 39;
		reward_chance = 21;
		break;
	case SCORNFUL:
		punish_chance = 13;
		reward_chance = 169;
		break;
	case AMUSED:
		punish_chance = 39;
		reward_chance = 7;
		break;
	case APPROVING:
		punish_chance = 91;
		reward_chance = 3;
		break;
	case INTERESTED:
		punish_chance = 13;
		reward_chance = 7;
		break;
	default:
		break;
	}
	/* Khorne never rewards casters! */
	if (attitude == SCORNFUL && reason == PATRON_CAST) reward_chance = 0;
	if (one_in_(chaos_effect_notice[p_ptr->chaos_patron][reason]))
	{
		if (punish_chance && one_in_(punish_chance))
		{
			chaos_patron_reward(REW_TYPE_PUNISH);
		}
		else if (reward_chance && one_in_(reward_chance))
		{
			if (one_in_(6) && p_ptr->chaos_patron != PATRON_KHORNE)
			{
				chaos_patron_event(REW_DEVICE);
			}
			else chaos_patron_reward(REW_TYPE_FAVOUR);
		}
		else
		{
			if (attitude == SCORNFUL && reason == PATRON_CAST)
			{
				/* getting off easy! */
				chaos_patron_event(REW_DAMNATION);
			}
			/* mutations may be too much here, we'll have to see - small chance to mutate every time we're not given explicit favour or punishment */
			else if (one_in_(16)) {
				chaos_patron_event(REW_MUTATE);
			}
			else if (randint0(punish_chance + reward_chance) < punish_chance) 
			{
				/* new good effects will be sponging off the old ones */
				if (one_in_(8) && p_ptr->chaos_patron != PATRON_KHORNE)
				{
					chaos_patron_event(REW_ENERGISE);
				}
				else if (one_in_(7))
				{
					chaos_patron_event(REW_EXULTATION);
				}
				else if (one_in_(6))
				{
					chaos_patron_event(REW_DISORDER);
				}
				else 
				{
					chaos_patron_reward(REW_TYPE_REWARD);
				}
			}
			else
			{
				if (one_in_(6)) 
				{
					chaos_patron_event(REW_DAMNATION);
				}
				else 
				{
					chaos_patron_reward(REW_TYPE_ANNOY);
				}
			}
		}
	}
}



