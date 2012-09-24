/* File: wizard2.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Purpose: Wizard commands */

#include "angband.h"


/*
 * Roll the hitdie -- aux of do_cmd_rerate()
 */
void do_cmd_rerate_aux(void)
{
	/* Minimum hitpoints at highest level */
	int min_value = p_ptr->hitdie + ((PY_MAX_LEVEL + 2) * (p_ptr->hitdie + 1)) * 3 / 8;

	/* Maximum hitpoints at highest level */
	int max_value = p_ptr->hitdie + ((PY_MAX_LEVEL + 2) * (p_ptr->hitdie + 1)) * 5 / 8;

	int i;

	/* Rerate */
	while (1)
	{
		/* Pre-calculate level 1 hitdice */
		p_ptr->player_hp[0] = p_ptr->hitdie;

		for (i = 1; i < 4; i++)
		{
			p_ptr->player_hp[0] += randint1(p_ptr->hitdie);
		}

		/* Roll the hitpoint values */
		for (i = 1; i < PY_MAX_LEVEL; i++)
		{
			p_ptr->player_hp[i] = p_ptr->player_hp[i - 1] + randint1(p_ptr->hitdie);
		}

		/* Require "valid" hitpoints at highest level */
		if ((p_ptr->player_hp[PY_MAX_LEVEL - 1] >= min_value) &&
		    (p_ptr->player_hp[PY_MAX_LEVEL - 1] <= max_value)) break;
	}
}


/*
 * Hack -- Rerate Hitpoints
 */
void do_cmd_rerate(bool display)
{
	int percent;

	/* Rerate */
	do_cmd_rerate_aux();

	percent = (int)(((long)p_ptr->player_hp[PY_MAX_LEVEL - 1] * 200L) /
		(2 * p_ptr->hitdie +
		((PY_MAX_LEVEL - 1+3) * (p_ptr->hitdie + 1))));


	/* Update and redraw hitpoints */
	p_ptr->update |= (PU_HP);
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	/* Handle stuff */
	handle_stuff();

	/* Message */
	if (display)
	{
#ifdef JP
		msg_format("現在の体力ランクは %d/100 です。", percent);
#else
		msg_format("Your life rate is %d/100 now.", percent);
#endif
		p_ptr->knowledge |= KNOW_HPRATE;
	}
	else
	{
#ifdef JP
		msg_print("体力ランクが変わった。");
#else
		msg_print("Life rate is changed.");
#endif
		p_ptr->knowledge &= ~(KNOW_HPRATE);
	}
}


#ifdef ALLOW_WIZARD

/*
 * Dimension Door
 */
static bool wiz_dimension_door(void)
{
	int	x = 0, y = 0;

	if (!tgt_pt(&x, &y)) return FALSE;

	teleport_player_to(y, x, TELEPORT_NONMAGICAL);

	return (TRUE);
}


/*
 * Create the artifact of the specified number -- DAN
 *
 */
static void wiz_create_named_art(int a_idx)
{
	create_named_art(a_idx, py, px);
}


/*
 * Hack -- quick debugging hook
 */
static void do_cmd_wiz_hack_chris1(void)
{
	int a_idx = get_quantity("Which One? ", max_a_idx);
	int k_idx = lookup_kind(a_info[a_idx].tval, a_info[a_idx].sval);
	s16b dummy_name = quark_add("");
	int ct = get_quantity("How Many?", 10000);
	int ct_speed = 0;
	int ct_immunity = 0;
	int ct_would_be_immunities = 0;
	int ct_blows = 0;
	int ct_telepathy = 0;
	int ct_aggravate = 0;
	int ct_darkness = 0;
	int ct_spell_power = 0;
	int ct_pval = 0;
	int pow_base = 0;
	int i;

	{
		object_type forge = {0};
		char buf[MAX_NLEN];

		if (!create_named_art_aux(a_idx, &forge)) return;
		pow_base = object_value_real(&forge);
		identify_item(&forge);

		forge.ident |= (IDENT_MENTAL); 
		object_desc(buf, &forge, 0);

		msg_format("Replacing %s (Cost: %d):", buf, pow_base);
	}
	for (i = 0; i < ct; i++)
	{
		object_type forge = {0};
		char buf[MAX_NLEN];
		int value;

		if (1)
		{
			create_replacement_art(a_idx, &forge);
		}
		else
		{
			artifact_type  *a_ptr = &a_info[a_idx];
			int				k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

			object_prep(&forge, k_idx);
			create_artifact(&forge, CREATE_ART_GOOD);
		}
		identify_item(&forge);

		forge.ident |= (IDENT_MENTAL); 
		forge.art_name = dummy_name;
		object_desc(buf, &forge, 0);
		value = object_value_real(&forge);
		ct_pval += forge.pval;

		if (have_flag(forge.art_flags, TR_IM_ACID)
		 || have_flag(forge.art_flags, TR_IM_COLD)
		 || have_flag(forge.art_flags, TR_IM_FIRE)
		 || have_flag(forge.art_flags, TR_IM_ELEC))
		{
			int ct = 0;
			ct_immunity++;
			
			if (have_flag(forge.art_flags, TR_IM_ACID)) ct++;
			if (have_flag(forge.art_flags, TR_IM_COLD)) ct++;
			if (have_flag(forge.art_flags, TR_IM_FIRE)) ct++;
			if (have_flag(forge.art_flags, TR_IM_ELEC)) ct++;

		/*	if (ct > 0)
				msg_format("%s (Cost: %d)", buf, value);*/
		}

		if (have_flag(forge.art_flags, TR_SPEED))
			ct_speed++;

		if (have_flag(forge.art_flags, TR_BLOWS))
			ct_blows++;

		if (have_flag(forge.art_flags, TR_TELEPATHY))
			ct_telepathy++;

		if (have_flag(forge.art_flags, TR_AGGRAVATE))
			ct_aggravate++;

		if (immunity_hack)
		{
			ct_would_be_immunities++;
		}

		if ( have_flag(forge.art_flags, TR_WILD)
		  || have_flag(forge.art_flags, TR_ORDER) )
		{
		}

		if (have_flag(forge.art_flags, TR_DARKNESS))
		{
			ct_darkness++;
		/*	drop_near(&forge, -1, py, px); */
		}

		if (have_flag(forge.art_flags, TR_SPELL_POWER))
		{
			ct_spell_power++;
		/*	drop_near(&forge, -1, py, px);*/
		}
		msg_format(" %d) %s (%.1f%%)", i+1, buf, (double)value/(double)pow_base*100.0);
		/*drop_near(&forge, -1, py, px);*/
		/*value = object_value_real(&forge);*/
	}

	msg_format("Generated %d artifacts.  %d had immunity.  %d had speed.  %d had extra attacks.", ct, ct_immunity, ct_speed, ct_blows);
	msg_format("%d had telepathy. %d had aggravation.", ct_telepathy, ct_aggravate);
/*	msg_format("%d had darkness. %d had spell power.", ct_darkness, ct_spell_power); */
	msg_format("%d would be immunities created.", ct_would_be_immunities);
	msg_format("%.2f average pval.", (double)ct_pval/(double)ct);

}

static void do_cmd_wiz_hack_chris2(void)
{
	int i;
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		if (inventory[i].name3)
		{
			create_replacement_art(inventory[i].name3, &inventory[i]);
			identify_item(&inventory[i]);
			inventory[i].ident |= IDENT_MENTAL; 
		}
	}
}

static void do_cmd_wiz_hack_chris3_imp(FILE* file)
{
	int k_idx = get_quantity("Enter k_idx: ", 1000);
	int ct = get_quantity("How Many?", 10000);
	int depths[] = { 1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, -1 };
	int i, j;
	int counts[1024];

	fprintf(file, "Lv\tEgo\tCt\n");

	for (i = 0; ; i++)
	{
		int depth = depths[i];
		if (depth < 0) break;

		dun_level = depth;
		object_level = depth;

		for (j = 0; j < 1024; j++)
		{
			counts[j] = 0;
		}

		for (j = 0; j < ct; j++)
		{
			object_type forge;

			object_prep(&forge, k_idx);
			apply_magic(&forge, depth, 0);

			#if 1
			if (forge.name2 == EGO_MIGHT)
			{
				char buf[MAX_NLEN];

				identify_item(&forge);
				forge.ident |= (IDENT_MENTAL); 
				object_desc(buf, &forge, 0);
				msg_print(buf);
			/*	drop_near(&forge, -1, py, px); */
			}
			#endif

			counts[forge.name2]++;
		}

		for (j = 1; j < 1024; j++)
		{
			if (counts[j])
			{
				fprintf(file, "%d\t%s\t%d\n", depth, e_name + e_info[j].name, counts[j]);
			}
		}
	}
}

static void do_cmd_wiz_hack_chris3(void)
{
	int		fd = -1;
	FILE	*fff = NULL;
	char	buf[1024];
	int old_dun_level = dun_level;
	int old_object_level = object_level;

	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "Egos8.txt");
	fff = my_fopen(buf, "w");

	if (!fff)
	{
		prt("Failed!", 0, 0);
		(void)inkey();
		return;
	}

	do_cmd_wiz_hack_chris3_imp(fff);

	dun_level = old_dun_level;
	object_level = old_object_level;

	my_fclose(fff);
	msg_print("Successful.");
	msg_print(NULL);
}

static void do_cmd_wiz_hack_chris4_imp(FILE* file)
{
	int i;
	fprintf(file, "Old\tNew\tDelta\tObject\n");
	for (i = 1; i < max_a_idx; i++)
	{
		artifact_type *a_ptr = &a_info[i];
		object_type forge;

		object_prep(&forge, lookup_kind(a_ptr->tval, a_ptr->sval));

		if (object_is_melee_weapon(&forge) || object_is_armour(&forge) || object_is_jewelry(&forge))
		{
			char buf[MAX_NLEN];
			s32b new_score = 0, old_score = 0;

			forge.name1 = i;
			forge.pval = a_ptr->pval;
			forge.ac = a_ptr->ac;
			forge.dd = a_ptr->dd;
			forge.ds = a_ptr->ds;
			forge.to_a = a_ptr->to_a;
			forge.to_h = a_ptr->to_h;
			forge.to_d = a_ptr->to_d;
			forge.weight = a_ptr->weight;

			if (a_ptr->gen_flags & TRG_CURSED) forge.curse_flags |= (TRC_CURSED);
			if (a_ptr->gen_flags & TRG_HEAVY_CURSE) forge.curse_flags |= (TRC_HEAVY_CURSE);
			if (a_ptr->gen_flags & TRG_PERMA_CURSE) forge.curse_flags |= (TRC_PERMA_CURSE);

			random_artifact_resistance(&forge, a_ptr);

			identify_item(&forge);
			forge.ident |= (IDENT_MENTAL); 
			object_desc(buf, &forge, 0);

			new_score = new_object_cost(&forge);
			old_score = object_value_real(&forge);


			fprintf(file, "%d\t%d\t%d\t%s\n", 
				old_score,
				new_score,
				new_score - old_score,
				buf
			);
		}
	}	

	for (i = 0; i < 5000; )
	{
		int k = randint1(max_k_idx);
		object_type forge;

		if (k_info[k].gen_flags & TRG_INSTA_ART) continue;

		object_prep(&forge, k);

		if (object_is_melee_weapon(&forge) || object_is_armour(&forge) || object_is_jewelry(&forge))
		{
			s32b new_score = 0, old_score = 0;
			char buf[MAX_NLEN];

			apply_magic(&forge, dun_level, AM_GREAT);

			/*if (forge.name2)*/
			{
				identify_item(&forge);
				forge.ident |= (IDENT_MENTAL); 
				object_desc(buf, &forge, 0);

				new_score = new_object_cost(&forge);
				old_score = object_value_real(&forge);

				fprintf(file, "%d\t%d\t%d\t%s\n", 
					old_score,
					new_score,
					new_score - old_score,
					buf
				);

				i++;
			}
		}
	}
}

static FILE    *_wiz_dbg_file = NULL;

static void _wiz_dbg_hook(cptr msg)
{
	fprintf(_wiz_dbg_file, "%s\n", msg);
}

static void do_cmd_wiz_hack_chris4(void)
{
	int		fd = -1;
	FILE	*fff = NULL;
	char	buf[1024];

	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "Scoring8.txt");
	fff = my_fopen(buf, "w");

	if (!fff)
	{
		prt("Failed!", 0, 0);
		(void)inkey();
		return;
	}

	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "ScoringDetails8.txt");
	_wiz_dbg_file = my_fopen(buf, "w");
	cost_calc_hook = _wiz_dbg_hook;

	do_cmd_wiz_hack_chris4_imp(fff);
	
	cost_calc_hook = NULL;
	my_fclose(_wiz_dbg_file);

	my_fclose(fff);
	msg_print("Successful.");
	msg_print(NULL);
}

static void do_cmd_wiz_hack_chris5(void)
{
	int r_idx = 1102;
	int i;

	for (i = 0; i < 8; i++)
	{
		monster_race *r_ptr = &r_info[r_idx];
		if (((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(r_ptr->flags7 & (RF7_NAZGUL))) &&
			(r_ptr->cur_num >= r_ptr->max_num))
		{
			r_ptr->cur_num = 0;
			r_ptr->max_num = 1;
		}

		summon_named_creature(0, py, px, r_idx, PM_ALLOW_SLEEP | PM_ALLOW_GROUP);
	}
}

static void do_cmd_wiz_hack_chris6_imp(FILE *file, bool replace)
{
	int a_idx, i;
	int ct = 30;
	int dummy_name = quark_add("");
	double qual_tot = 0.0, qual = 0.0;
	int a_ct = 0;
	s16b old_level = object_level;

	ct = get_quantity("How Many of Each? ", 100);

	if (replace)
		fprintf(file, "Replacement Artifacts\n\n\n");
	else
		fprintf(file, "Random Artifacts (*NOT* Replacements!)\n\n\n");

	for (a_idx = 1; a_idx < max_a_idx; a_idx++)
	{
		object_type forge = {0};
		char buf[MAX_NLEN];
		int pow_base = 0;
		int pow_tot = 0;
		int pow = 0;
		int pval_tot = 0;
		int speed_tot = 0;
		int att_tot = 0;

		if (!create_named_art_aux(a_idx, &forge)) continue;
		if (replace)
		{
			if (object_is_weapon_ammo(&forge))
			{
				forge.to_h = MAX(10, forge.to_h);
				forge.to_d = MAX(10, forge.to_d);
			}
			if (object_is_armour(&forge))
			{
				forge.to_a = MAX(10, forge.to_a);
			}
		}
		pow_base = object_value_real(&forge);
		identify_item(&forge);
		object_level = a_info[a_idx].level;

		forge.ident |= (IDENT_MENTAL); 
		object_desc(buf, &forge, 0);

		fprintf(file, "====================================================================================================\n");
		fprintf(file, "%d:%s Score = %d\n", a_idx, buf, pow_base);
		fprintf(file, "====================================================================================================\n");

		for (i = 0; i < ct; i++)
		{
			if (replace)
			{
				create_replacement_art(a_idx, &forge);
			}
			else
			{
				artifact_type  *a_ptr = &a_info[a_idx];
				int				k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

				if (!k_idx) continue;

				object_prep(&forge, k_idx);
				create_artifact(&forge, CREATE_ART_GOOD);
			}
			pow = object_value_real(&forge);
			pow_tot += pow;
			pval_tot += forge.pval;
			if (have_flag(forge.art_flags, TR_SPEED))
				speed_tot += forge.pval;
			if (have_flag(forge.art_flags, TR_BLOWS))
				att_tot += forge.pval;
			identify_item(&forge);

			forge.ident |= (IDENT_MENTAL); 
			forge.art_name = dummy_name;
			object_desc(buf, &forge, 0);

			fprintf(file, "%s (%.1f%%)\n", buf, (double)pow/(double)pow_base*100.0);
		}
		fprintf(file, "\npval = %.2f\n", (double)pval_tot/(double) ct);
		fprintf(file, "speed = %.2f\n", (double)speed_tot/(double)ct);
		if (att_tot)
			fprintf(file, "blows = %.2f\n", (double)att_tot/(double)ct);

		if (pow_base)
		{
			qual = ((double)pow_tot/(double)ct)/(double)pow_base;

			if (qual > 10.0)
			{
				fprintf(file, "quality = %.1f%% (Discard High)\n", qual*100.0);
			}
			else if (replace && qual < 0.15)
			{
				fprintf(file, "quality = %.1f%% (Discard Low)\n", qual*100.0);
			}
			else
			{
				a_ct++;
				qual_tot += qual;
				fprintf(file, "quality = %.1f%%\n", qual*100.0);
			}
		}
		fprintf(file, "\n\n\n");
	}

	fprintf(file, "Total: %.1f%%\n", qual_tot/(double)a_ct*100.0);

	object_level = old_level;
}

static void do_cmd_wiz_hack_chris6(void)
{
	int		fd = -1;
	FILE	*fff = NULL;
	char	buf[1024];
	bool replace = get_check("Generate Replacement Artifacts?");

	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "Arts8.txt");
	fff = my_fopen(buf, "w");

	if (!fff)
	{
		prt("Failed!", 0, 0);
		(void)inkey();
		return;
	}

	do_cmd_wiz_hack_chris6_imp(fff, replace);

	my_fclose(fff);
	msg_print("Successful.");
	msg_print(NULL);
}

static void do_cmd_wiz_hack_chris7_imp(FILE* file)
{
	int ct = get_quantity("How Many Thousands?", 1000);
	int depths[] = { 1, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, -1 };
	int i, j;
	int counts[1024];
	int good_counts[1024];

	fprintf(file, "||Lv||Obj||Avg||Good||\n");

	for (i = 0; ; i++)
	{
		int depth = depths[i];
		if (depth < 0) break;

		dun_level = depth;
		object_level = depth;

		for (j = 0; j < 1024; j++)
		{
			counts[j] = 0;
			good_counts[j] = 0;
		}

		for (j = 0; j < ct * 1000; j++)
		{
			object_type forge;
			int k;

			object_wipe(&forge);
			if (!make_object(&forge, 0)) continue;
			identify_item(&forge);
			forge.ident |= (IDENT_MENTAL); 

			k = is_autopick(&forge);
			if (k >= 0 && k < 1024)
			{
				counts[k]++;
			}

			object_wipe(&forge);
			if (!make_object(&forge, AM_GOOD)) continue;
			identify_item(&forge);
			forge.ident |= (IDENT_MENTAL); 

			k = is_autopick(&forge);
			if (k >= 0 && k < 1024)
			{
				good_counts[k]++;
			}
		}

		for (j = 1; j < 1024; j++)
		{
			if (counts[j] || good_counts[j])
			{
				fprintf(file, "||%d||%s||%d||%d||\n", depth, autopick_list[j].name, counts[j], good_counts[j]);
			}
		}
	}
}

static void do_cmd_wiz_hack_chris7(void)
{
	int		fd = -1;
	FILE	*fff = NULL;
	char	buf[1024];
	int old_dun_level = dun_level;
	int old_object_level = object_level;

	path_build(buf, sizeof(buf), ANGBAND_DIR_USER, "Objects1.txt");
	fff = my_fopen(buf, "w");

	if (!fff)
	{
		prt("Failed!", 0, 0);
		(void)inkey();
		return;
	}

	do_cmd_wiz_hack_chris7_imp(fff);

	dun_level = old_dun_level;
	object_level = old_object_level;

	my_fclose(fff);
	msg_print("Successful.");
	msg_print(NULL);
}

static void _strip_class_name(char *buf, int idx)
{
	char *t;
	cptr str = class_info[idx].title;

	for (t = buf; *str; str++)
	{
		if (*str != '-') *t++ = *str;
	}
	*t = '\0';
}

static void _strip_race_name(char *buf, int idx)
{
	char *t;
	cptr str = get_race_t_aux(idx, 0)->name;

	for (t = buf; *str; str++)
	{
		if (*str != '-') *t++ = *str;
	}
	*t = '\0';
}

static void _auto_class_spoiler_name(char *buf, int idx)
{
	char tmp[256];
	_strip_class_name(tmp, idx);
	sprintf(buf, "Auto%sSpoilers", tmp);
}

static void _auto_realm_spoiler_name(char *buf, int idx)
{
	sprintf(buf, "Auto%sSpoilers", realm_names[idx]);
}

static void _auto_race_spoiler_name(char *buf, int idx)
{
	char tmp[256];
	_strip_race_name(tmp, idx);
	sprintf(buf, "Auto%sSpoilers", tmp);
}

typedef void(*_file_fn)(FILE*);
static void _wiki_file(cptr name, _file_fn fn)
{
	int		fd = -1;
	FILE	*fff = NULL;
	char	buf[1024];

	/* HARD CODED!!! Sorry :( */
	sprintf(buf, "c:\\src\\cheng\\chengband.wiki\\%s", name);
	fff = my_fopen(buf, "w");

	if (!fff)
	{
		path_build(buf, sizeof(buf), ANGBAND_DIR_USER, name);
		fff = my_fopen(buf, "w");

		if (!fff)
		{
			prt("Failed!", 0, 0);
			(void)inkey();
			return;
		}
	}

	fprintf(fff, "#summary Automatically Generated (Do not make manual updates!)\n");
	fn(fff);
	fprintf(fff, "\n\n_Automatically generated for Chengband %d.%d.%d._\n",
		    FAKE_VER_MAJOR-10, FAKE_VER_MINOR, FAKE_VER_PATCH);


	my_fclose(fff);
	msg_print("Successful.");
	msg_print(NULL);
}

static cptr _race_design_page(int i)
{
	if (i == RACE_DEMIGOD)
		return "DemigodParentage";
	return 0;
}

cptr race_spoiler_page(int i)
{
	static char buf[256];
	_auto_race_spoiler_name(buf, i);
	return buf;
}

static void _race_tbl1(FILE* fff)
{
	int i;
	char buf[1024];

	for (i = 0; i < MAX_RACES; i++)
	{
		race_t *race_ptr = get_race_t_aux(i, 0);
		cptr spoiler = race_spoiler_page(i);
		if (i % 5 == 0)
			fprintf(fff, "|| || *Str* || *Int* || *Wis* || *Dex* || *Con* || *Chr* || *Dis* || *Dev* || *Sav* || *Stl* || *Srh* || *Fos* || *Thn* || *Thb* || *HD* || *If* || *Exp* ||\n");

		if (spoiler)
			sprintf(buf, "[%s %s]", spoiler, race_ptr->name);
		else
			sprintf(buf, "%s", race_ptr->name);


		fprintf(fff, "||%s||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%4d||\n",
			buf, 
			race_ptr->stats[A_STR], race_ptr->stats[A_INT], race_ptr->stats[A_WIS], 
			race_ptr->stats[A_DEX], race_ptr->stats[A_CON], race_ptr->stats[A_CHR], 
			race_ptr->skills.dis, race_ptr->skills.dev, race_ptr->skills.sav,
			race_ptr->skills.stl, race_ptr->skills.srh, race_ptr->skills.fos,
			race_ptr->skills.thn, race_ptr->skills.thb,
			race_ptr->hd, race_ptr->infra,
			race_ptr->exp
		);
	}
}

static void _mimic_race_tbl1(FILE* fff)
{
	int i;
	char buf[1024];

	fprintf(fff, "|| || *Str* || *Int* || *Wis* || *Dex* || *Con* || *Chr* || *Dis* || *Dev* || *Sav* || *Stl* || *Srh* || *Fos* || *Thn* || *Thb* || *HD* || *If* || *Exp* ||\n");

	for (i = MIMIC_MIN; i <= MIMIC_MAX; i++)
	{
		race_t *race_ptr = get_race_t_aux(i, 0);
		cptr spoiler = race_spoiler_page(i);

		if (spoiler)
			sprintf(buf, "[%s %s]", spoiler, race_ptr->name);
		else
			sprintf(buf, "%s", race_ptr->name);


		fprintf(fff, "||%s||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%4d||\n",
			buf, 
			race_ptr->stats[A_STR], race_ptr->stats[A_INT], race_ptr->stats[A_WIS], 
			race_ptr->stats[A_DEX], race_ptr->stats[A_CON], race_ptr->stats[A_CHR], 
			race_ptr->skills.dis, race_ptr->skills.dev, race_ptr->skills.sav,
			race_ptr->skills.stl, race_ptr->skills.srh, race_ptr->skills.fos,
			race_ptr->skills.thn, race_ptr->skills.thb,
			race_ptr->hd, race_ptr->infra,
			race_ptr->exp
		);
	}
}

static void _demigod_wiki(FILE* fff)
{ 
	int i;

	fprintf(fff, "_Note: This is the automatically generated spoiler page. For the design page, please go [DemigodParentage here]._\n\n");
	fprintf(fff, "\n\n[http://angband.oook.cz/ladder-browse.php?v=Chengband&r=%s&c=&n=&e=&s=0 View Ladder]\n", "Demigod");
	fprintf(fff, "\n= Stats =\n\n");
	fprintf(fff, "|| || *Str* || *Int* || *Wis* || *Dex* || *Con* || *Chr* || *Dis* || *Dev* || *Sav* || *Stl* || *Srh* || *Fos* || *Thn* || *Thb* || *HD* || *Exp* ||\n");
	for (i = 0; i < MAX_DEMIGOD_TYPES; i++)
	{
		race_t *race_ptr = get_race_t_aux(RACE_DEMIGOD, i);
		fprintf(fff, "||%s||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%4d||\n",
			demigod_info[i].name,
			race_ptr->stats[A_STR], race_ptr->stats[A_INT], race_ptr->stats[A_WIS], 
			race_ptr->stats[A_DEX], race_ptr->stats[A_CON], race_ptr->stats[A_CHR], 
			race_ptr->skills.dis, race_ptr->skills.dev, race_ptr->skills.sav,
			race_ptr->skills.stl, race_ptr->skills.srh, race_ptr->skills.fos,
			race_ptr->skills.thn, race_ptr->skills.thb,
			race_ptr->hd,
			race_ptr->exp
		); 
	}

	fprintf(fff, "\nDemigods gain [SpecialAbilities special abilities] of their choice at L20 and L40.\n");

	for (i = 0; i < MAX_DEMIGOD_TYPES; i++) 
	{
		race_t *race_ptr = get_race_t_aux(RACE_DEMIGOD, i);

		if (race_ptr)
		{
			fprintf(fff, "\n\n= %s =\n\n", demigod_info[i].name);
			fprintf(fff, "%s\n", demigod_info[i].desc);

			if (race_ptr->spoiler_dump)
				race_ptr->spoiler_dump(fff);
		}
	}
}


static void _personality_tbl1(FILE* fff)
{
	int i;

	fprintf(fff, "|| || *Str* || *Int* || *Wis* || *Dex* || *Con* || *Chr* || *Dis* || *Dev* || *Sav* || *Stl* || *Srh* || *Fos* || *Thn* || *Thb* || *HD* || *Exp* ||\n");
	for (i = 0; i < MAX_SEIKAKU; i++)
	{
		player_seikaku *a_ptr = &seikaku_info[i];

		fprintf(fff, "||%s||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%3d||%4d||\n",
			a_ptr->title, 
			a_ptr->a_adj[0], a_ptr->a_adj[1], a_ptr->a_adj[2], 
			a_ptr->a_adj[3], a_ptr->a_adj[4], a_ptr->a_adj[5], 
			a_ptr->skills.dis, a_ptr->skills.dev, a_ptr->skills.sav,
			a_ptr->skills.stl, a_ptr->skills.srh, a_ptr->skills.fos,
			a_ptr->skills.thn, a_ptr->skills.thb, 
			a_ptr->a_mhp,
			a_ptr->a_exp
		);
	}

	fprintf(fff, "\n= Descriptions =\n\n");
	for (i = 0; i < MAX_SEIKAKU; i++)
	{
		fprintf(fff, "*%s*\n", seikaku_info[i].title);
		fprintf(fff, "%s\n\n", birth_get_personality_desc(i));
	}
}

static cptr _class_design_page(int i)
{
	switch (i)
	{
	case CLASS_MAULER: return "MaulerSpoilers";
	case CLASS_PSION: return "PsionSpoilers";
	case CLASS_SCOUT: return "ScoutSpoilers";
	case CLASS_RAGE_MAGE: return "RageMageSpoilers";
	case CLASS_WARLOCK: return "WarlockSpoilers";
	case CLASS_WEAPONMASTER: return "WeaponmasterSpoilers";
	case CLASS_RUNE_KNIGHT: return "RuneKnightSpoilers";
	case CLASS_DUELIST: return "DuelistSpoilers";
	case CLASS_BLOOD_KNIGHT: return "BloodKnightSpoilers";
	}
	return 0;
}

static cptr _class_spoiler_page(int i)
{
	static char buf[256];
	_auto_class_spoiler_name(buf, i);
	return buf;
}

static void _class_tbl1(FILE* fff)
{
	int i;
	char buf[1024];

	for (i = 0; i < MAX_CLASS; i++)
	{
		player_class *class_ptr = &class_info[i];
		cptr title = class_ptr->title;
		cptr spoiler = _class_spoiler_page(i);

		if (spoiler)
			sprintf(buf, "[%s %s]", spoiler, title);
		else
			sprintf(buf, "%s", title);

		if (i % 5 == 0)
			fprintf(fff, "|| || *Str* || *Int* || *Wis* || *Dex* || *Con* || *Chr* || *Dis* || *Dev* || *Sav* || *Stl* || *Srh* || *Fos* || *Thn* || *Thb* || *HD* || *Exp* || *Pet* ||\n");

		fprintf(fff, "||%s||%3d||%3d||%3d||%3d||%3d||%3d||%d,%d||%d,%d||%d,%d||%3d||%3d||%3d||%d,%d||%d,%d||%3d||%4d||%3d||\n",
			buf, 
			class_ptr->c_adj[0], class_ptr->c_adj[1], class_ptr->c_adj[2], 
			class_ptr->c_adj[3], class_ptr->c_adj[4], class_ptr->c_adj[5], 
			class_ptr->base_skills.dis, class_ptr->extra_skills.dis, 
			class_ptr->base_skills.dev, class_ptr->extra_skills.dev, 
			class_ptr->base_skills.sav, class_ptr->extra_skills.sav,
			class_ptr->base_skills.stl, 
			class_ptr->base_skills.srh, 
			class_ptr->base_skills.fos,
			class_ptr->base_skills.thn, class_ptr->extra_skills.thn, 
			class_ptr->base_skills.thb, class_ptr->extra_skills.thb, 
			class_ptr->c_mhp,
			class_ptr->c_exp,
			class_ptr->pet_upkeep_div
		);
	}
}

static int _next_skill_desc(char *buf, int class_idx, int tv, int sv)
{
	int kval;

	if (sv == -1) return -1;

	for (;;)
	{
		sv++;
		if (sv >= 64)
		{
			sprintf(buf, " || || ");;
			return -1;
		}

		if (tv == TV_BOW && (sv == SV_CRIMSON || sv == SV_RAILGUN)) continue;

		kval = lookup_kind(tv, sv);
		if (kval)
		{
			int min = s_info[class_idx].w_start[tv-TV_WEAPON_BEGIN][sv];
			int max = s_info[class_idx].w_max[tv-TV_WEAPON_BEGIN][sv];
			char name[1024];

			strip_name(name, kval);
			sprintf(buf, " %s || %d || %d ", name, min, max);
			return sv;
		}
	}

	return -1;
}

static void _spell_info(char *buf, cptr name, magic_type *info)
{
	if (info->slevel == 99)
		sprintf(buf, "%s||--||--||--", name);
	else
		sprintf(buf, "%s||%d||%d||%d", name, info->slevel, info->smana, info->sfail);
}

static void _spellbook_name(char *buf, int tval, int sval)
{
	int kval = lookup_kind(tval, sval);
	strip_name(buf, kval);
}

static void _class_spoilers(FILE* fff, int idx)
{
	int i;
	int counters[5];
	char bufs[5][1024];
	cptr design = _class_design_page(idx);
	
	player_class *class_ptr = &class_info[idx];
	player_magic *magic_ptr = &m_info[idx];
	class_t      *class_ptr2 = get_class_t_aux(idx, 0);

	if (design)
	{
		fprintf(fff, "_Note: This is the automatically generated spoiler page. For the design page, please go [%s here]._\n\n", design);
	}

	fprintf(fff, "= %s =\n\n", class_ptr->title);
	fprintf(fff, "== Description ==\n");
	if (class_ptr2)
		fprintf(fff, "%s\n\n", class_ptr2->desc);
	else
		fprintf(fff, "%s\n\n", birth_get_class_desc(idx));

	fprintf(fff, "\n\n[http://angband.oook.cz/ladder-browse.php?v=Chengband&r=&c=%s&n=&e=&s=0 View Ladder]\n", class_ptr->title);

	if (magic_ptr->spell_stat)
	{
		int j,k;
		bool first = TRUE;

		for (i = 1; i <= MAX_MAGIC; i++)
		{
			int bit = (1 << (i-1));
			bool ok = FALSE;
			int max = 4;

			if (idx == CLASS_RED_MAGE && i != REALM_NECROMANCY)
			{
				ok = TRUE;
				max = 2;
			}
			if (idx == CLASS_SORCERER && i != REALM_NECROMANCY) ok = TRUE;
			if (realm_choices1[idx] & bit) ok = TRUE;
			if (realm_choices2[idx] & bit) ok = TRUE;

			if (ok)
			{
				char books[4][1024];
				int tval = TV_LIFE_BOOK + (i - 1);

				if (first)
				{
					fprintf(fff, "\n== Spells ==\n");
					first = FALSE;
				}

				for (k = 0; k < max; k++)
				{
					_spellbook_name(books[k], tval, k);
				}

				if (max == 2)
				{
					char spoil[256];
					_auto_realm_spoiler_name(spoil, i);
					fprintf(fff, "|| *[%s %s]* || || || || || || || ||\n", spoil, realm_names[i]);
					fprintf(fff, "|| *%s* || *Lvl* || *Mana* || *Fail* || *%s* || *Lvl* || *Mana* || *Fail* ||\n", books[0], books[1]);
				}
				else
				{
					char spoil[256];
					_auto_realm_spoiler_name(spoil, i);
					fprintf(fff, "|| *[%s %s]* || || || || || || || || || || || || || || || ||\n", spoil, realm_names[i]);
					fprintf(fff, "|| *%s* || *Lvl* || *Mana* || *Fail* || *%s* || *Lvl* || *Mana* || *Fail* || *%s* || *Lvl* || *Mana* || *Fail* || *%s* || *Lvl* || *Mana* || *Fail* ||\n", books[0], books[1], books[2], books[3]);
				}

				for (j = 0; j < 8; j++)
				{
					char names[4][1024];
					char bufs[4][1024];

					for (k = 0; k < max; k++)
					{
						int l = 8*k + j;
						sprintf(names[k], "%s", do_spell(i, l, SPELL_NAME));

						_spell_info(bufs[k], names[k], &magic_ptr->info[i-1][l]);
					}

					if (max == 2)
						fprintf(fff, "||%s||%s||\n", bufs[0], bufs[1]);
					else
						fprintf(fff, "||%s||%s||%s||%s||\n", bufs[0], bufs[1], bufs[2], bufs[3]);
				}
			}
		}
	}

	{
	class_t *class_ptr = get_class_t_aux(idx, 0);

		/* Hook for extra info */
		if (class_ptr && class_ptr->spoiler_dump)
			class_ptr->spoiler_dump(fff);
	}

	fprintf(fff, "\n== Skills ==\n");
	fprintf(fff, "|| *Skill* || *Min* || *Max* ||\n");
	fprintf(fff, "|| Martial Arts || %d || %d ||\n", s_info[idx].s_start[SKILL_MARTIAL_ARTS], s_info[idx].s_max[SKILL_MARTIAL_ARTS]);
	fprintf(fff, "|| Dual Wielding || %d || %d ||\n", s_info[idx].s_start[SKILL_DUAL_WIELDING], s_info[idx].s_max[SKILL_DUAL_WIELDING]);
	fprintf(fff, "|| Riding || %d || %d ||\n", s_info[idx].s_start[SKILL_RIDING], s_info[idx].s_max[SKILL_RIDING]);

	fprintf(fff, "\n== Weapon Skills ==\n");

	/* Yuk to get 5 tables in 1 side by side ... Sorry :( */
	for (i = 0; i < 5; i++)
		counters[i] = 0;

	fprintf(fff, "\n|| *Bows* || *Min* || *Max* || *Diggers* || *Min* || *Max* || *Hafted* || *Min* || *Max* || *Polearms* || *Min* || *Max* || *Swords* || *Min* || *Max* ||\n");
	for (;;)
	{
		for (i = 0; i < 5; i++)
			counters[i] = _next_skill_desc(bufs[i], idx, TV_WEAPON_BEGIN + i, counters[i]);

		if (counters[0] == -1 && counters[1] == -1 && counters[2] == -1 && counters[3] == -1 && counters[4] == -1)
			break;

		fprintf(fff, "|| %s || %s || %s || %s || %s ||\n", bufs[0], bufs[1], bufs[2], bufs[3], bufs[4]);
	}
}

static int _class_idx_hack = 0;
static void _class_wiki_imp(FILE* fff) { _class_spoilers(fff, _class_idx_hack); }
static void _class_wiki_file(int idx)
{
	char buf[256];
	char file_name[256];
	_auto_class_spoiler_name(buf, idx);
	sprintf(file_name, "%s.wiki", buf);
	_class_idx_hack = idx;
	_wiki_file(file_name, _class_wiki_imp);
}

static void _spell_tbl1(FILE* fff)
{
	int i;
	char buf[256];
	for (i = 1; i <= MAX_MAGIC; i++)
	{
		_auto_realm_spoiler_name(buf, i);
		fprintf(fff, "*[%s %s]*\n", buf, realm_names[i]);
		fprintf(fff, "%s\n\n", birth_get_realm_desc(i));
	}
}

static void _realm_spoilers(FILE* fff, int idx)
{
	int i, j, k;
	int tval = TV_LIFE_BOOK + (idx - 1);

	fprintf(fff, "= *%s* =\n", realm_names[idx]);
	for (i = 0; i < 4; i++)
	{
		char book_name[256];
		char spell_name[256];
		char spell_desc[1024];

		_spellbook_name(book_name, tval, i);

		fprintf(fff, "\n== *%s* ==\n", book_name);
		for (j = 0; j < 8; j++)
		{
			cptr val;
			k = 8*i + j;

			/* do_spell() is old school. It uses a cptr to encode all the info.
			   In case a verb is not handled, it returns "". BTW, NULL means
			   that casting was canceled ... Also, the return value often
			   (sometimes?) points to a global buffer so you need to know
			   when this happens and act accordingly. */
			val = do_spell(idx, k, SPELL_SPOIL_NAME);
			if (strlen(val) == 0)
				val = do_spell(idx, k, SPELL_NAME);
			sprintf(spell_name, "`%s`", val);

			val = do_spell(idx, k, SPELL_SPOIL_DESC);
			if (strlen(val) == 0)
				val = do_spell(idx, k, SPELL_DESC);
			sprintf(spell_desc, "`%s`", val);

			fprintf(fff, "  * %s: %s\n", spell_name, spell_desc);
		}
	}
}

static int _realm_idx_hack = 0;
static void _realm_wiki_imp(FILE* fff) { _realm_spoilers(fff, _realm_idx_hack); }
static void _realm_wiki_file(int idx)
{
	char buf[256];
	char file_name[256];
	_auto_realm_spoiler_name(buf, idx);
	sprintf(file_name, "%s.wiki", buf);
	_realm_idx_hack = idx;
	_wiki_file(file_name, _realm_wiki_imp);
}

static void _race_spoilers(FILE *fff, int idx)
{
	race_t *race_ptr = get_race_t_aux(idx, 0);
	if (!race_ptr)
	{
		fprintf(fff, "Under Construction\n");
		return;
	}

	fprintf(fff, "= %s =\n\n", race_ptr->name);
	fprintf(fff, "%s\n", race_ptr->desc);
	if (idx >= MIMIC_MIN)
		fprintf(fff, "\n\n[http://angband.oook.cz/ladder-browse.php?v=Chengband&r=%%5B%s%%5D&c=&n=&e=&s=0 View Ladder]\n", race_ptr->name);
	else
		fprintf(fff, "\n\n[http://angband.oook.cz/ladder-browse.php?v=Chengband&r=%s&c=&n=&e=&s=0 View Ladder]\n", race_ptr->name);

	fprintf(fff, "\n== Stats and Skills ==\n");
	fprintf(fff, "|| *Stat* || *Adj* || *Skill* || *Amt* ||\n");
	fprintf(fff, "|| Str || %d || Device || %d ||\n", race_ptr->stats[A_STR], race_ptr->skills.dev);
	fprintf(fff, "|| Int || %d || Disarm || %d ||\n", race_ptr->stats[A_INT], race_ptr->skills.dis);
	fprintf(fff, "|| Wis || %d || Save || %d ||\n", race_ptr->stats[A_WIS], race_ptr->skills.sav);
	fprintf(fff, "|| Dex || %d || Stealth || %d ||\n", race_ptr->stats[A_DEX], race_ptr->skills.stl);
	fprintf(fff, "|| Con || %d || Search || %d ||\n", race_ptr->stats[A_CON], race_ptr->skills.srh);
	fprintf(fff, "|| Chr || %d || Frequency || %d ||\n", race_ptr->stats[A_CHR], race_ptr->skills.fos);
	fprintf(fff, "|| HD || %d || Melee || %d ||\n", race_ptr->hd, race_ptr->skills.thn);
	fprintf(fff, "|| Exp || %d%% || Bows || %d ||\n", race_ptr->exp, race_ptr->skills.thb);

	if (race_ptr->spoiler_dump)
		race_ptr->spoiler_dump(fff);
}

static int _race_idx_hack = 0;
static void _race_wiki_imp(FILE* fff) { _race_spoilers(fff, _race_idx_hack); }
static void _race_wiki_file(int idx)
{
	char buf[256];
	char file_name[256];
	_auto_race_spoiler_name(buf, idx);
	sprintf(file_name, "%s.wiki", buf);
	_race_idx_hack = idx;
	_wiki_file(file_name, _race_wiki_imp);
}

bool spoiler_hack = FALSE;
static void do_cmd_wiz_hack_chris8(void)
{
	int i;
	spoiler_hack = TRUE;
	_wiki_file("AutoRacesSpoilers.wiki", _race_tbl1);
	_wiki_file("AutoMimicRacesSpoilers.wiki", _mimic_race_tbl1);
	_wiki_file("AutoDemigodSpoilers.wiki", _demigod_wiki);

	_wiki_file("AutoClassesSpoilers.wiki", _class_tbl1);
	_wiki_file("AutoPersonalitiesSpoilers.wiki", _personality_tbl1);
	_wiki_file("AutoSpellsSpoilers.wiki", _spell_tbl1);

	for (i = 0; i < MAX_RACES; i++)
	{
		if (i == RACE_DEMIGOD) continue;
		_race_wiki_file(i);
	}

	for (i = MIMIC_MIN; i <= MIMIC_MAX; i++)
	{
		_race_wiki_file(i);
	}

	for (i = 0; i < MAX_CLASS; i++)
		_class_wiki_file(i);

	for (i = 1; i <= MAX_MAGIC; i++)
		_realm_wiki_file(i);

	spoiler_hack = FALSE;
}

#ifdef MONSTER_HORDES

/* Summon a horde of monsters */
static void do_cmd_summon_horde(void)
{
	int wy = py, wx = px;
	int attempts = 1000;

	while (--attempts)
	{
		scatter(&wy, &wx, py, px, 3, 0);
		if (cave_empty_bold(wy, wx)) break;
	}

	(void)alloc_horde(wy, wx);
}

#endif /* MONSTER_HORDES */


/*
 * Output a long int in binary format.
 */
static void prt_binary(u32b flags, int row, int col)
{
	int        	i;
	u32b        bitmask;

	/* Scan the flags */
	for (i = bitmask = 1; i <= 32; i++, bitmask *= 2)
	{
		/* Dump set bits */
		if (flags & bitmask)
		{
			Term_putch(col++, row, TERM_BLUE, '*');
		}

		/* Dump unset bits */
		else
		{
			Term_putch(col++, row, TERM_WHITE, '-');
		}
	}
}


#define K_MAX_DEPTH 110

/*
 * Output a rarity graph for a type of object.
 */
static void prt_alloc(byte tval, byte sval, int row, int col)
{
	int i, j;
	int home = 0;
	u32b maxr = 1, maxt = 1, ratio;
	u32b rarity[K_MAX_DEPTH];
	u32b total[K_MAX_DEPTH];
	s32b maxd = 1, display[22];
	byte c = TERM_WHITE;
	cptr r = "+--common--+";
	object_kind *k_ptr;


	/* Get the entry */
	alloc_entry *table = alloc_kind_table;

	/* Wipe the tables */
	(void)C_WIPE(rarity, K_MAX_DEPTH, u32b);
	(void)C_WIPE(total, K_MAX_DEPTH, u32b);
	(void)C_WIPE(display, 22, s32b);

	/* Scan all entries */
	for (i = 0; i < K_MAX_DEPTH; i++)
	{
		int total_frac = 0;
		for (j = 0; j < alloc_kind_size; j++)
		{
			int prob = 0;

			if (table[j].level <= i)
			{
				prob = table[j].prob1 * GREAT_OBJ * K_MAX_DEPTH;
			}
			else if (table[j].level - 1 > 0)
			{
				prob = table[j].prob1 * i * K_MAX_DEPTH / (table[j].level - 1);
			}

			/* Acquire this kind */
			k_ptr = &k_info[table[j].index];

			/* Accumulate probabilities */
			total[i] += prob / (GREAT_OBJ * K_MAX_DEPTH);
			total_frac += prob % (GREAT_OBJ * K_MAX_DEPTH);

			/* Accumulate probabilities */
			if ((k_ptr->tval == tval) && (k_ptr->sval == sval))
			{
				home = k_ptr->level;
				rarity[i] += prob;
			}
		}
		total[i] += total_frac / (GREAT_OBJ * K_MAX_DEPTH);
	}

	/* Find maxima */
	for (i = 0; i < K_MAX_DEPTH; i++)
	{
		if (rarity[i] > maxr) maxr = rarity[i];
		if (total[i] > maxt) maxt = total[i];
	}

	if (maxr / (GREAT_OBJ * K_MAX_DEPTH) != 0)
		ratio = maxt / (maxr / (GREAT_OBJ * K_MAX_DEPTH));
	else
		ratio = 99999L;

	/* Simulate a log graph */
	if (ratio > 1000)
	{
		c = TERM_L_WHITE;
		r = "+-uncommon-+";
	}
	if (ratio > 3000)
	{
		c = TERM_SLATE;
		r = "+---rare---+";
	}
	if (ratio > 32768L)
	{
		c = TERM_L_DARK;
		r = "+-VeryRare-+";
	}

	/* Calculate probabilities for each range */
	for (i = 0; i < 22; i++)
	{
		/* Shift the values into view */

		int possibility = 0;
		for (j = i * K_MAX_DEPTH / 22; j < (i + 1) * K_MAX_DEPTH / 22; j++)
			possibility += rarity[j] * (100 * maxt / total[j]);

		possibility = possibility / maxr;

		/* display[i] = log_{sqrt(2)}(possibility) */
		display[i] = 0;
		while (possibility)
		{
			display[i]++;
			possibility = possibility * 1000 / 1414;
		}

		/* Track maximum */
		if (display[i] > maxd) maxd = display[i];
	}

	/* Normalize */
	if (maxd > 10) for (i = 0; i < 22; i++)
	{
		display[i] = display[i] - maxd + 10;
	}

	/* Graph the rarities */
	for (i = 0; i < 22; i++)
	{
		Term_putch(col, row + i + 1, TERM_WHITE,  '|');

		prt(format("%d", (i * K_MAX_DEPTH / 220) % 10), row + i + 1, col);

		if (display[i] <= 0) 
			continue;

		/* Note the level */
		if ((i * K_MAX_DEPTH / 22 <= home) && (home < (i + 1) * K_MAX_DEPTH / 22))
		{
			c_prt(TERM_RED, format("%.*s", display[i], "**********"), row + i + 1, col + 1);
		}
		else
		{
			c_prt(c, format("%.*s", display[i], "**********"), row + i + 1, col + 1);
		}
	}

	/* Make it look nice */
	prt(r, row, col);
}


/*
 * Hack -- Teleport to the target
 */
static void do_cmd_wiz_bamf(void)
{
	/* Must have a target */
	if (!target_who) return;

	/* Teleport to the target */
	teleport_player_to(target_row, target_col, TELEPORT_NONMAGICAL);
}


/*
 * Aux function for "do_cmd_wiz_change()".	-RAK-
 */
static void do_cmd_wiz_change_aux(void)
{
	int i, j;
	int tmp_int;
	long tmp_long;
	s16b tmp_s16b;
	char tmp_val[160];
	char ppp[80];


	/* Query the stats */
	for (i = 0; i < 6; i++)
	{
		/* Prompt */
		sprintf(ppp, "%s (3-%d): ", stat_names[i], p_ptr->stat_max_max[i]);

		/* Default */
		sprintf(tmp_val, "%d", p_ptr->stat_max[i]);

		/* Query */
		if (!get_string(ppp, tmp_val, 3)) return;

		/* Extract */
		tmp_int = atoi(tmp_val);

		/* Verify */
		if (tmp_int > p_ptr->stat_max_max[i]) tmp_int = p_ptr->stat_max_max[i];
		else if (tmp_int < 3) tmp_int = 3;

		/* Save it */
		p_ptr->stat_cur[i] = p_ptr->stat_max[i] = tmp_int;
	}


	/* Default */
	sprintf(tmp_val, "%d", WEAPON_EXP_MASTER);

	/* Query */
#ifdef JP
	if (!get_string("熟練度: ", tmp_val, 9)) return;
#else
	if (!get_string("Proficiency: ", tmp_val, 9)) return;
#endif

	/* Extract */
	tmp_s16b = atoi(tmp_val);

	/* Verify */
	if (tmp_s16b < WEAPON_EXP_UNSKILLED) tmp_s16b = WEAPON_EXP_UNSKILLED;
	if (tmp_s16b > WEAPON_EXP_MASTER) tmp_s16b = WEAPON_EXP_MASTER;

	for (j = 0; j <= TV_WEAPON_END - TV_WEAPON_BEGIN; j++)
	{
		for (i = 0;i < 64;i++)
		{
			int max = skills_weapon_max(TV_WEAPON_BEGIN + j, i);
			p_ptr->weapon_exp[j][i] = tmp_s16b;
			if (p_ptr->weapon_exp[j][i] > max) p_ptr->weapon_exp[j][i] = max;
		}
	}

	for (j = 0; j < 10; j++)
	{
		p_ptr->skill_exp[j] = tmp_s16b;
		if (p_ptr->skill_exp[j] > s_info[p_ptr->pclass].s_max[j]) p_ptr->skill_exp[j] = s_info[p_ptr->pclass].s_max[j];
	}

	for (j = 0; j < 32; j++)
		p_ptr->spell_exp[j] = (tmp_s16b > SPELL_EXP_MASTER ? SPELL_EXP_MASTER : tmp_s16b);
	for (; j < 64; j++)
		p_ptr->spell_exp[j] = (tmp_s16b > SPELL_EXP_EXPERT ? SPELL_EXP_EXPERT : tmp_s16b);

	/* Default */
	sprintf(tmp_val, "%ld", (long)(p_ptr->au));

	/* Query */
	if (!get_string("Gold: ", tmp_val, 9)) return;

	/* Extract */
	tmp_long = atol(tmp_val);

	/* Verify */
	if (tmp_long < 0) tmp_long = 0L;

	/* Save */
	p_ptr->au = tmp_long;


	/* Default */
	sprintf(tmp_val, "%ld", (long)(p_ptr->max_exp));

	/* Query */
	if (!get_string("Experience: ", tmp_val, 9)) return;

	/* Extract */
	tmp_long = atol(tmp_val);

	/* Verify */
	if (tmp_long < 0) tmp_long = 0L;

	if (p_ptr->prace != RACE_ANDROID)
	{
		/* Save */
		p_ptr->max_exp = tmp_long;
		p_ptr->exp = tmp_long;

		/* Update */
		check_experience();
	}
}


/*
 * Change various "permanent" player variables.
 */
static void do_cmd_wiz_change(void)
{
	/* Interact */
	do_cmd_wiz_change_aux();

	/* Redraw everything */
	do_cmd_redraw();
}


/*
 * Wizard routines for creating objects		-RAK-
 * And for manipulating them!                   -Bernd-
 *
 * This has been rewritten to make the whole procedure
 * of debugging objects much easier and more comfortable.
 *
 * The following functions are meant to play with objects:
 * Create, modify, roll for them (for statistic purposes) and more.
 * The original functions were by RAK.
 * The function to show an item's debug information was written
 * by David Reeve Sward <sward+@CMU.EDU>.
 *                             Bernd (wiebelt@mathematik.hu-berlin.de)
 *
 * Here are the low-level functions
 * - wiz_display_item()
 *     display an item's debug-info
 * - wiz_create_itemtype()
 *     specify tval and sval (type and subtype of object)
 * - wiz_tweak_item()
 *     specify pval, +AC, +tohit, +todam
 *     Note that the wizard can leave this function anytime,
 *     thus accepting the default-values for the remaining values.
 *     pval comes first now, since it is most important.
 * - wiz_reroll_item()
 *     apply some magic to the item or turn it into an artifact.
 * - wiz_roll_item()
 *     Get some statistics about the rarity of an item:
 *     We create a lot of fake items and see if they are of the
 *     same type (tval and sval), then we compare pval and +AC.
 *     If the fake-item is better or equal it is counted.
 *     Note that cursed items that are better or equal (absolute values)
 *     are counted, too.
 *     HINT: This is *very* useful for balancing the game!
 * - wiz_quantity_item()
 *     change the quantity of an item, but be sane about it.
 *
 * And now the high-level functions
 * - do_cmd_wiz_play()
 *     play with an existing object
 * - wiz_create_item()
 *     create a new object
 *
 * Note -- You do not have to specify "pval" and other item-properties
 * directly. Just apply magic until you are satisfied with the item.
 *
 * Note -- For some items (such as wands, staffs, some rings, etc), you
 * must apply magic, or you will get "broken" or "uncharged" objects.
 *
 * Note -- Redefining artifacts via "do_cmd_wiz_play()" may destroy
 * the artifact.  Be careful.
 *
 * Hack -- this function will allow you to create multiple artifacts.
 * This "feature" may induce crashes or other nasty effects.
 */

/*
 * Just display an item's properties (debug-info)
 * Originally by David Reeve Sward <sward+@CMU.EDU>
 * Verbose item flags by -Bernd-
 */
static void wiz_display_item(object_type *o_ptr)
{
	int i, j = 13;
	u32b flgs[TR_FLAG_SIZE];
	char buf[256];

	/* Extract the flags */
	object_flags(o_ptr, flgs);

	/* Clear the screen */
	for (i = 1; i <= 23; i++) prt("", i, j - 2);

	prt_alloc(o_ptr->tval, o_ptr->sval, 1, 0);

	/* Describe fully */
	object_desc(buf, o_ptr, OD_STORE);

	prt(buf, 2, j);

	prt(format("kind = %-5d  level = %-4d  tval = %-5d  sval = %-5d",
		   o_ptr->k_idx, k_info[o_ptr->k_idx].level,
		   o_ptr->tval, o_ptr->sval), 4, j);

	prt(format("number = %-3d  wgt = %-6d  ac = %-5d    damage = %dd%d",
		   o_ptr->number, o_ptr->weight,
		   o_ptr->ac, o_ptr->dd, o_ptr->ds), 5, j);

	prt(format("pval = %-5d  toac = %-5d  tohit = %-4d  todam = %-4d",
		   o_ptr->pval, o_ptr->to_a, o_ptr->to_h, o_ptr->to_d), 6, j);

	prt(format("name1 = %-4d  name2 = %-4d  cost = %ld",
		   o_ptr->name1, o_ptr->name2, (long)object_value_real(o_ptr)), 7, j);

	prt(format("ident = %04x  xtra1 = %-4d  xtra2 = %-4d  timeout = %-d",
		   o_ptr->ident, o_ptr->xtra1, o_ptr->xtra2, o_ptr->timeout), 8, j);

	prt(format("xtra3 = %-4d  xtra4 = %-4d  xtra5 = %-4d  cursed  = %-d",
		   o_ptr->xtra3, o_ptr->xtra4, o_ptr->xtra5, o_ptr->curse_flags), 9, j);

	prt("+------------FLAGS1------------+", 10, j);
	prt("AFFECT........SLAY........BRAND.", 11, j);
	prt("      mf      cvae      xsqpaefc", 12, j);
	prt("siwdccsossidsahanvudotgddhuoclio", 13, j);
	prt("tnieohtctrnipttmiinmrrnrrraiierl", 14, j);
	prt("rtsxnarelcfgdkcpmldncltggpksdced", 15, j);
	prt_binary(flgs[0], 16, j);

	prt("+------------FLAGS2------------+", 17, j);
	prt("SUST....IMMUN.RESIST............", 18, j);
	prt("      reaefctrpsaefcpfldbc sn   ", 19, j);
	prt("siwdcciaclioheatcliooeialoshtncd", 20, j);
	prt("tnieohdsierlrfraierliatrnnnrhehi", 21, j);
	prt("rtsxnaeydcedwlatdcedsrekdfddrxss", 22, j);
	prt_binary(flgs[1], 23, j);

	prt("+------------FLAGS3------------+", 10, j+32);
	prt("fe cnn t      stdrmsiiii d ab   ", 11, j+32);
	prt("aa aoomywhs lleeieihgggg rtgl   ", 12, j+32);
	prt("uu utmacaih eielgggonnnnaaere   ", 13, j+32);
	prt("rr reanurdo vtieeehtrrrrcilas   ", 14, j+32);
	prt("aa algarnew ienpsntsaefctnevs   ", 15, j+32);
	prt_binary(flgs[2], 16, j+32);

	prt("+------------FLAGS4------------+", 17, j+32);
	prt("KILL....ESP.........            ", 18, j+32);
	prt("aeud tghaud tgdhegnu            ", 19, j+32);
	prt("nvneoriunneoriruvoon            ", 20, j+32);
	prt("iidmroamidmroagmionq            ", 21, j+32);
	prt("mlenclnmmenclnnnldlu            ", 22, j+32);
	prt_binary(flgs[3], 23, j+32);
}


/*
 * A structure to hold a tval and its description
 */
typedef struct tval_desc
{
	int        tval;
	cptr       desc;
} tval_desc;

/*
 * A list of tvals and their textual names
 */
static tval_desc tvals[] =
{
	{ TV_SWORD,             "Sword"                },
	{ TV_POLEARM,           "Polearm"              },
	{ TV_HAFTED,            "Hafted Weapon"        },
	{ TV_BOW,               "Bow"                  },
	{ TV_ARROW,             "Arrows"               },
	{ TV_BOLT,              "Bolts"                },
	{ TV_SHOT,              "Shots"                },
	{ TV_SHIELD,            "Shield"               },
	{ TV_CROWN,             "Crown"                },
	{ TV_HELM,              "Helm"                 },
	{ TV_GLOVES,            "Gloves"               },
	{ TV_BOOTS,             "Boots"                },
	{ TV_CLOAK,             "Cloak"                },
	{ TV_DRAG_ARMOR,        "Dragon Scale Mail"    },
	{ TV_HARD_ARMOR,        "Hard Armor"           },
	{ TV_SOFT_ARMOR,        "Soft Armor"           },
	{ TV_RING,              "Ring"                 },
	{ TV_AMULET,            "Amulet"               },
	{ TV_LITE,              "Lite"                 },
	{ TV_POTION,            "Potion"               },
	{ TV_SCROLL,            "Scroll"               },
	{ TV_WAND,              "Wand"                 },
	{ TV_STAFF,             "Staff"                },
	{ TV_ROD,               "Rod"                  },
	{ TV_LIFE_BOOK,         "Life Spellbook"       },
	{ TV_SORCERY_BOOK,      "Sorcery Spellbook"    },
	{ TV_NATURE_BOOK,       "Nature Spellbook"     },
	{ TV_CHAOS_BOOK,        "Chaos Spellbook"      },
	{ TV_DEATH_BOOK,        "Death Spellbook"      },
	{ TV_TRUMP_BOOK,        "Trump Spellbook"      },
	{ TV_ARCANE_BOOK,       "Arcane Spellbook"     },
	{ TV_CRAFT_BOOK,        "Craft Spellbook"},
	{ TV_DAEMON_BOOK,       "Daemon Spellbook"},
	{ TV_CRUSADE_BOOK,      "Crusade Spellbook"},
	{ TV_NECROMANCY_BOOK,   "Necromancy Spellbook"},
	{ TV_MUSIC_BOOK,        "Music Spellbook"      },
	{ TV_HISSATSU_BOOK,     "Book of Kendo"        },
	{ TV_HEX_BOOK,          "Hex Spellbook"        },
	{ TV_RAGE_BOOK,         "Rage Spellbook"       },
	{ TV_BURGLARY_BOOK,     "Thieve's Guide"       },
	{ TV_PARCHMENT,         "Parchment" },
	{ TV_WHISTLE,           "Whistle"	},
	{ TV_SPIKE,             "Spikes"               },
	{ TV_DIGGING,           "Digger"               },
	{ TV_CHEST,             "Chest"                },
	{ TV_CAPTURE,           "Capture Ball"         },
	{ TV_CARD,              "Express Card"         },
	{ TV_FIGURINE,          "Magical Figurine"     },
	{ TV_STATUE,            "Statue"               },
	{ TV_CORPSE,            "Corpse"               },
	{ TV_FOOD,              "Food"                 },
	{ TV_FLASK,             "Flask"                },
	{ TV_JUNK,              "Junk"                 },
	{ TV_SKELETON,          "Skeleton"             },
	{ 0,                    NULL                   }
};


/*
 * Strip an "object name" into a buffer
 */
void strip_name(char *buf, int k_idx)
{
	char *t;

	object_kind *k_ptr = &k_info[k_idx];

	cptr str = (k_name + k_ptr->name);


	/* Skip past leading characters */
	while ((*str == ' ') || (*str == '&') || (*str == '[')) str++;

	/* Copy useful chars */
	for (t = buf; *str; str++)
	{
#ifdef JP
		if (iskanji(*str)) {*t++ = *str++; *t++ = *str; continue;}
#endif
		if (*str != '~' && *str != ']') *t++ = *str;
	}

	/* Terminate the new name */
	*t = '\0';
}


/*
 * Specify tval and sval (type and subtype of object) originally
 * by RAK, heavily modified by -Bernd-
 *
 * This function returns the k_idx of an object type, or zero if failed
 *
 * List up to 50 choices in three columns
 */
static int wiz_create_itemtype(void)
{
	int i, num, max_num;
	int col, row;
	int tval;

	cptr tval_desc;
	char ch;

	int choice[80];

	char buf[160];


	/* Clear screen */
	Term_clear();

	/* Print all tval's and their descriptions */
	for (num = 0; (num < 80) && tvals[num].tval; num++)
	{
		row = 2 + (num % 20);
		col = 20 * (num / 20);
		ch = listsym[num];
		prt(format("[%c] %s", ch, tvals[num].desc), row, col);
	}

	/* Me need to know the maximal possible tval_index */
	max_num = num;

	/* Choose! */
	if (!get_com("Get what type of object? ", &ch, FALSE)) return (0);

	/* Analyze choice */
	for (num = 0; num < max_num; num++)
	{
		if (listsym[num] == ch) break;
	}

	/* Bail out if choice is illegal */
	if ((num < 0) || (num >= max_num)) return (0);

	/* Base object type chosen, fill in tval */
	tval = tvals[num].tval;
	tval_desc = tvals[num].desc;


	/*** And now we go for k_idx ***/

	/* Clear screen */
	Term_clear();

	/* We have to search the whole itemlist. */
	for (num = 0, i = 1; (num < 80) && (i < max_k_idx); i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Analyze matching items */
		if (k_ptr->tval == tval)
		{
			/* Prepare it */
			row = 2 + (num % 20);
			col = 20 * (num / 20);
			ch = listsym[num];
			strcpy(buf,"                    ");

			/* Acquire the "name" of object "i" */
			strip_name(buf, i);

			/* Print it */
			prt(format("[%c] %s", ch, buf), row, col);

			/* Remember the object index */
			choice[num++] = i;
		}
	}

	/* Me need to know the maximal possible remembered object_index */
	max_num = num;

	/* Choose! */
	if (!get_com(format("What Kind of %s? ", tval_desc), &ch, FALSE)) return (0);

	/* Analyze choice */
	for (num = 0; num < max_num; num++)
	{
		if (listsym[num] == ch) break;
	}

	/* Bail out if choice is "illegal" */
	if ((num < 0) || (num >= max_num)) return (0);

	/* And return successful */
	return (choice[num]);
}


/*
 * Tweak an item
 */
static void wiz_tweak_item(object_type *o_ptr)
{
	cptr p;
	char tmp_val[80];


	/* Hack -- leave artifacts alone */
	if (object_is_artifact(o_ptr)) return;

	p = "Enter new 'pval' setting: ";
	sprintf(tmp_val, "%d", o_ptr->pval);
	if (!get_string(p, tmp_val, 5)) return;
	o_ptr->pval = atoi(tmp_val);
	wiz_display_item(o_ptr);

	p = "Enter new 'to_a' setting: ";
	sprintf(tmp_val, "%d", o_ptr->to_a);
	if (!get_string(p, tmp_val, 5)) return;
	o_ptr->to_a = atoi(tmp_val);
	wiz_display_item(o_ptr);

	p = "Enter new 'to_h' setting: ";
	sprintf(tmp_val, "%d", o_ptr->to_h);
	if (!get_string(p, tmp_val, 5)) return;
	o_ptr->to_h = atoi(tmp_val);
	wiz_display_item(o_ptr);

	p = "Enter new 'to_d' setting: ";
	sprintf(tmp_val, "%d", o_ptr->to_d);
	if (!get_string(p, tmp_val, 5)) return;
	o_ptr->to_d = atoi(tmp_val);
	wiz_display_item(o_ptr);
}


/*
 * Apply magic to an item or turn it into an artifact. -Bernd-
 */
static void wiz_reroll_item(object_type *o_ptr)
{
	object_type forge;
	object_type *q_ptr;

	char ch;

	bool changed = FALSE;


	/* Hack -- leave artifacts alone */
	if (object_is_artifact(o_ptr)) return;


	/* Get local object */
	q_ptr = &forge;

	/* Copy the object */
	object_copy(q_ptr, o_ptr);


	/* Main loop. Ask for magification and artifactification */
	while (TRUE)
	{
		/* Display full item debug information */
		wiz_display_item(q_ptr);

		/* Ask wizard what to do. */
		if (!get_com("[a]ccept, [w]orthless, [c]ursed, [n]ormal, [g]ood, [e]xcellent, [s]pecial? ", &ch, FALSE))
		{
			/* Preserve wizard-generated artifacts */
			if (object_is_fixed_artifact(q_ptr))
			{
				a_info[q_ptr->name1].cur_num = 0;
				q_ptr->name1 = 0;
			}

			changed = FALSE;
			break;
		}

		/* Create/change it! */
		if (ch == 'A' || ch == 'a')
		{
			changed = TRUE;
			break;
		}

		/* Preserve wizard-generated artifacts */
		if (object_is_fixed_artifact(q_ptr))
		{
			a_info[q_ptr->name1].cur_num = 0;
			q_ptr->name1 = 0;
		}

		switch(ch)
		{
			/* Apply bad magic, but first clear object */
			case 'w': case 'W':
			{
				object_prep(q_ptr, o_ptr->k_idx);
				apply_magic(q_ptr, dun_level, AM_NO_FIXED_ART | AM_GOOD | AM_GREAT | AM_CURSED);
				break;
			}
			/* Apply bad magic, but first clear object */
			case 'c': case 'C':
			{
				object_prep(q_ptr, o_ptr->k_idx);
				apply_magic(q_ptr, dun_level, AM_NO_FIXED_ART | AM_GOOD | AM_CURSED);
				break;
			}
			/* Apply normal magic, but first clear object */
			case 'n': case 'N':
			{
				object_prep(q_ptr, o_ptr->k_idx);
				apply_magic(q_ptr, dun_level, AM_NO_FIXED_ART);
				break;
			}
			/* Apply good magic, but first clear object */
			case 'g': case 'G':
			{
				object_prep(q_ptr, o_ptr->k_idx);
				apply_magic(q_ptr, dun_level, AM_NO_FIXED_ART | AM_GOOD);
				break;
			}
			/* Apply great magic, but first clear object */
			case 'e': case 'E':
			{
				object_prep(q_ptr, o_ptr->k_idx);
				apply_magic(q_ptr, dun_level, AM_NO_FIXED_ART | AM_GOOD | AM_GREAT);
				break;
			}
			/* Apply special magic, but first clear object */
			case 's': case 'S':
			{
				object_prep(q_ptr, o_ptr->k_idx);
				apply_magic(q_ptr, dun_level, AM_GOOD | AM_GREAT | AM_SPECIAL);

				/* Failed to create artifact; make a random one */
				if (!object_is_artifact(q_ptr)) create_artifact(q_ptr, CREATE_ART_NORMAL);
				break;
			}
		}
		q_ptr->iy = o_ptr->iy;
		q_ptr->ix = o_ptr->ix;
		q_ptr->next_o_idx = o_ptr->next_o_idx;
		q_ptr->marked = o_ptr->marked;
	}


	/* Notice change */
	if (changed)
	{
		/* Apply changes */
		object_copy(o_ptr, q_ptr);

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);
	}
}



/*
 * Try to create an item again. Output some statistics.    -Bernd-
 *
 * The statistics are correct now.  We acquire a clean grid, and then
 * repeatedly place an object in this grid, copying it into an item
 * holder, and then deleting the object.  We fiddle with the artifact
 * counter flags to prevent weirdness.  We use the items to collect
 * statistics on item creation relative to the initial item.
 */
static void wiz_statistics(object_type *o_ptr)
{
	u32b i, matches, better, worse, other, correct;

	u32b test_roll = 1000000;

	char ch;
	cptr quality;

	u32b mode;

	object_type forge;
	object_type	*q_ptr;

	cptr q = "Rolls: %ld  Correct: %ld  Matches: %ld  Better: %ld  Worse: %ld  Other: %ld";

	cptr p = "Enter number of items to roll: ";
	char tmp_val[80];


	/* XXX XXX XXX Mega-Hack -- allow multiple artifacts */
	if (object_is_fixed_artifact(o_ptr)) 
		a_info[o_ptr->name1].cur_num = 0;


	/* Interact */
	while (TRUE)
	{
		cptr pmt = "Roll for [n]ormal, [g]ood, or [e]xcellent treasure? ";

		/* Display item */
		wiz_display_item(o_ptr);

		/* Get choices */
		if (!get_com(pmt, &ch, FALSE)) break;

		if (ch == 'n' || ch == 'N')
		{
			mode = 0L;
			quality = "normal";
		}
		else if (ch == 'g' || ch == 'G')
		{
			mode = AM_GOOD;
			quality = "good";
		}
		else if (ch == 'e' || ch == 'E')
		{
			mode = AM_GOOD | AM_GREAT;
			quality = "excellent";
		}
		else
		{
			break;
		}

		sprintf(tmp_val, "%ld", test_roll);
		if (get_string(p, tmp_val, 10)) test_roll = atol(tmp_val);
		test_roll = MAX(1, test_roll);

		/* Let us know what we are doing */
		msg_format("Creating a lot of %s items. Base level = %d.",
					  quality, dun_level);
		msg_print(NULL);

		/* Set counters to zero */
		correct = matches = better = worse = other = 0;

		/* Let's rock and roll */
		for (i = 0; i <= test_roll; i++)
		{
			/* Output every few rolls */
			if ((i < 100) || (i % 100 == 0))
			{
				/* Do not wait */
				inkey_scan = TRUE;

				/* Allow interupt */
				if (inkey())
				{
					/* Flush */
					flush();

					/* Stop rolling */
					break;
				}

				/* Dump the stats */
				prt(format(q, i, correct, matches, better, worse, other), 0, 0);
				Term_fresh();
			}


			/* Get local object */
			q_ptr = &forge;

			/* Wipe the object */
			object_wipe(q_ptr);

			/* Create an object */
			make_object(q_ptr, mode);


			/* XXX XXX XXX Mega-Hack -- allow multiple artifacts */
			if (object_is_fixed_artifact(q_ptr)) 
				a_info[q_ptr->name1].cur_num = 0;


			/* Test for the same tval and sval. */
			if ((o_ptr->tval) != (q_ptr->tval)) continue;
			if ((o_ptr->sval) != (q_ptr->sval)) continue;

			/* One more correct item */
			correct++;

			/* Check for match */
			if ((q_ptr->pval == o_ptr->pval) &&
				 (q_ptr->to_a == o_ptr->to_a) &&
				 (q_ptr->to_h == o_ptr->to_h) &&
				 (q_ptr->to_d == o_ptr->to_d) &&
				 (q_ptr->name1 == o_ptr->name1))
			{
				matches++;
			}

			/* Check for better */
			else if ((q_ptr->pval >= o_ptr->pval) &&
						(q_ptr->to_a >= o_ptr->to_a) &&
						(q_ptr->to_h >= o_ptr->to_h) &&
						(q_ptr->to_d >= o_ptr->to_d))
			{
				better++;
			}

			/* Check for worse */
			else if ((q_ptr->pval <= o_ptr->pval) &&
						(q_ptr->to_a <= o_ptr->to_a) &&
						(q_ptr->to_h <= o_ptr->to_h) &&
						(q_ptr->to_d <= o_ptr->to_d))
			{
				worse++;
			}

			/* Assume different */
			else
			{
				other++;
			}
		}

		/* Final dump */
		msg_format(q, i, correct, matches, better, worse, other);
		msg_print(NULL);
	}


	/* Hack -- Normally only make a single artifact */
	if (object_is_fixed_artifact(o_ptr)) a_info[o_ptr->name1].cur_num = 1;
}


/*
 * Change the quantity of a the item
 */
static void wiz_quantity_item(object_type *o_ptr)
{
	int         tmp_int, tmp_qnt;

	char        tmp_val[100];


	/* Never duplicate artifacts */
	if (object_is_artifact(o_ptr)) return;

	/* Store old quantity. -LM- */
	tmp_qnt = o_ptr->number;

	/* Default */
	sprintf(tmp_val, "%d", o_ptr->number);

	/* Query */
	if (get_string("Quantity: ", tmp_val, 2))
	{
		/* Extract */
		tmp_int = atoi(tmp_val);

		/* Paranoia */
		if (tmp_int < 1) tmp_int = 1;
		if (tmp_int > 99) tmp_int = 99;

		/* Accept modifications */
		o_ptr->number = tmp_int;
	}

	if (o_ptr->tval == TV_ROD)
	{
		o_ptr->pval = o_ptr->pval * o_ptr->number / tmp_qnt;
	}
}

/* debug command for blue mage */
static void do_cmd_wiz_blue_mage(void)
{

	int				i = 0;
	int				j = 0;
	s32b            f4 = 0, f5 = 0, f6 = 0;	

	for (j=1; j<6; j++)
	{

		set_rf_masks(&f4, &f5, &f6, j);

		for (i = 0; i < 32; i++)
		{
			if ((0x00000001 << i) & f4) p_ptr->magic_num2[i] = 1;
		}
		for (; i < 64; i++)
		{
			if ((0x00000001 << (i - 32)) & f5) p_ptr->magic_num2[i] = 1;
		}
		for (; i < 96; i++)
		{
			if ((0x00000001 << (i - 64)) & f6) p_ptr->magic_num2[i] = 1;
		}
	}
}


/*
 * Play with an item. Options include:
 *   - Output statistics (via wiz_roll_item)
 *   - Reroll item (via wiz_reroll_item)
 *   - Change properties (via wiz_tweak_item)
 *   - Change the number of items (via wiz_quantity_item)
 */
static void do_cmd_wiz_play(void)
{
	int item;

	object_type	forge;
	object_type *q_ptr;

	object_type *o_ptr;

	char ch;

	bool changed;

	cptr q, s;

	item_tester_no_ryoute = TRUE;
	/* Get an item */
	q = "Play with which object? ";
	s = "You have nothing to play with.";
	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}
	
	/* The item was not changed */
	changed = FALSE;


	/* Save the screen */
	screen_save();


	/* Get local object */
	q_ptr = &forge;

	/* Copy object */
	object_copy(q_ptr, o_ptr);


	/* The main loop */
	while (TRUE)
	{
		/* Display the item */
		wiz_display_item(q_ptr);

		/* Get choice */
		if (!get_com("[a]ccept [s]tatistics [r]eroll [t]weak [q]uantity? ", &ch, FALSE))
		{
			changed = FALSE;
			break;
		}

		if (ch == 'A' || ch == 'a')
		{
			changed = TRUE;
			break;
		}

		if (ch == 's' || ch == 'S')
		{
			wiz_statistics(q_ptr);
		}

		if (ch == 'r' || ch == 'r')
		{
			wiz_reroll_item(q_ptr);
		}

		if (ch == 't' || ch == 'T')
		{
			wiz_tweak_item(q_ptr);
		}

		if (ch == 'q' || ch == 'Q')
		{
			wiz_quantity_item(q_ptr);
		}
	}


	/* Restore the screen */
	screen_load();


	/* Accept change */
	if (changed)
	{
		/* Message */
		msg_print("Changes accepted.");

		/* Recalcurate object's weight */
		if (item >= 0)
		{
			p_ptr->total_weight += (q_ptr->weight * q_ptr->number)
				- (o_ptr->weight * o_ptr->number);
		}

		/* Change */
		object_copy(o_ptr, q_ptr);


		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Combine / Reorder the pack (later) */
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_SPELL | PW_PLAYER);
	}

	/* Ignore change */
	else
	{
		msg_print("Changes ignored.");
	}
}


/*
 * Wizard routine for creating objects		-RAK-
 * Heavily modified to allow magification and artifactification  -Bernd-
 *
 * Note that wizards cannot create objects on top of other objects.
 *
 * Hack -- this routine always makes a "dungeon object", and applies
 * magic to it, and attempts to decline cursed items.
 */
static void wiz_create_item(void)
{
	object_type	forge;
	object_type *q_ptr;
	int n = 1;

	int k_idx;


	/* Save the screen */
	screen_save();

	/* Get object base type */
	k_idx = wiz_create_itemtype();

	/* Restore the screen */
	screen_load();


	/* Return if failed */
	if (!k_idx) return;

	if (k_info[k_idx].gen_flags & TRG_INSTA_ART)
	{
		int i;

		/* Artifactify */
		for (i = 1; i < max_a_idx; i++)
		{
			/* Ignore incorrect tval */
			if (a_info[i].tval != k_info[k_idx].tval) continue;

			/* Ignore incorrect sval */
			if (a_info[i].sval != k_info[k_idx].sval) continue;

			/* Create this artifact */
			(void)create_named_art(i, py, px);

			/* All done */
			msg_print("Allocated(INSTA_ART).");

			return;
		}
	}
	else
	{
		n = get_quantity("How many? ", 99);	
	}

	/* Get local object */
	q_ptr = &forge;

	/* Create the item */
	object_prep(q_ptr, k_idx);

	/* Apply magic */
	apply_magic(q_ptr, dun_level, AM_NO_FIXED_ART);
	q_ptr->number = n;
 
	/* Drop the object from heaven */
	(void)drop_near(q_ptr, -1, py, px);

	/* All done */
	msg_print("Allocated.");
}


/*
 * Cure everything instantly
 */
static void do_cmd_wiz_cure_all(void)
{
	/* Restore stats */
	(void)res_stat(A_STR);
	(void)res_stat(A_INT);
	(void)res_stat(A_WIS);
	(void)res_stat(A_CON);
	(void)res_stat(A_DEX);
	(void)res_stat(A_CHR);

	/* Restore the level */
	(void)restore_level();

	/* Heal the player */
	if (p_ptr->chp < p_ptr->mhp)
	{
		p_ptr->chp = p_ptr->mhp;
		p_ptr->chp_frac = 0;

		/* Redraw */
		p_ptr->redraw |= (PR_HP);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER);
	}

	/* Restore mana */
	if (p_ptr->csp < p_ptr->msp)
	{
		p_ptr->csp = p_ptr->msp;
		p_ptr->csp_frac = 0;

		p_ptr->redraw |= (PR_MANA);
		p_ptr->window |= (PW_PLAYER);
		p_ptr->window |= (PW_SPELL);
	}

	/* Cure stuff */
	(void)set_blind(0, TRUE);
	(void)set_confused(0, TRUE);
	(void)set_poisoned(0, TRUE);
	fear_clear_p();
	(void)set_paralyzed(0, TRUE);
	(void)set_image(0, TRUE);
	(void)set_stun(0, TRUE);
	(void)set_cut(0, TRUE);
	(void)set_slow(0, TRUE);

	/* No longer hungry 
	(void)set_food(PY_FOOD_MAX - 1);*/
}


/*
 * Go to any level
 */
static void do_cmd_wiz_jump(void)
{
	/* Ask for level */
	if (command_arg <= 0)
	{
		char	ppp[80];

		char	tmp_val[160];
		int		tmp_dungeon_type;

		/* Prompt */
		sprintf(ppp, "Jump which dungeon : ");

		/* Default */
		sprintf(tmp_val, "%d", dungeon_type);

		/* Ask for a level */
		if (!get_string(ppp, tmp_val, 2)) return;

		tmp_dungeon_type = atoi(tmp_val);
		if (!d_info[tmp_dungeon_type].maxdepth || (tmp_dungeon_type > max_d_idx)) tmp_dungeon_type = DUNGEON_ANGBAND;

		/* Prompt */
		sprintf(ppp, "Jump to level (0, %d-%d): ", d_info[tmp_dungeon_type].mindepth, d_info[tmp_dungeon_type].maxdepth);

		/* Default */
		sprintf(tmp_val, "%d", dun_level);

		/* Ask for a level */
		if (!get_string(ppp, tmp_val, 10)) return;

		/* Extract request */
		command_arg = atoi(tmp_val);

		dungeon_type = tmp_dungeon_type;
	}

	/* Paranoia */
	if (command_arg < d_info[dungeon_type].mindepth) command_arg = 0;

	/* Paranoia */
	if (command_arg > d_info[dungeon_type].maxdepth) command_arg = d_info[dungeon_type].maxdepth;

	/* Accept request */
	msg_format("You jump to dungeon level %d.", command_arg);

	if (autosave_l) do_cmd_save_game(TRUE);

	/* Change level */
	dun_level = command_arg;

	prepare_change_floor_mode(CFM_RAND_PLACE);

	if (!dun_level) dungeon_type = 0;
	p_ptr->inside_arena = FALSE;
	p_ptr->wild_mode = FALSE;

	leave_quest_check();

	if (record_stair) do_cmd_write_nikki(NIKKI_WIZ_TELE,0,NULL);

	p_ptr->inside_quest = 0;
	energy_use = 0;

	/* Prevent energy_need from being too lower than 0 */
	p_ptr->energy_need = 0;

	/*
	 * Clear all saved floors
	 * and create a first saved floor
	 */
	prepare_change_floor_mode(CFM_FIRST_FLOOR);

	/* Leaving */
	p_ptr->leaving = TRUE;
}


/*
 * Become aware of a lot of objects
 */
static void do_cmd_wiz_learn(void)
{
	int i;

	object_type forge;
	object_type *q_ptr;

	/* Scan every object */
	for (i = 1; i < max_k_idx; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Induce awareness */
		if (k_ptr->level <= command_arg)
		{
			/* Get local object */
			q_ptr = &forge;

			/* Prepare object */
			object_prep(q_ptr, i);

			/* Awareness */
			object_aware(q_ptr);
		}
	}
}


/*
 * Summon some creatures
 */
static void do_cmd_wiz_summon(int num)
{
	int i;

	for (i = 0; i < num; i++)
	{
		(void)summon_specific(0, py, px, dun_level, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE));
	}
}


/*
 * Summon a creature of the specified type
 *
 * XXX XXX XXX This function is rather dangerous
 */
static void do_cmd_wiz_named(int r_idx)
{
	int x = px;
	int y = py;

	if (target_who < 0)
	{
		x = target_col;
		y = target_row;
	}

	{
		monster_race *r_ptr = &r_info[r_idx];
		if (((r_ptr->flags1 & (RF1_UNIQUE)) ||
				(r_ptr->flags7 & (RF7_NAZGUL))) &&
			(r_ptr->cur_num >= r_ptr->max_num))
		{
			r_ptr->cur_num = 0;
			r_ptr->max_num = 1;
		}
	}

	(void)summon_named_creature(0, y, x, r_idx, (PM_ALLOW_SLEEP | PM_ALLOW_GROUP));
}


/*
 * Summon a creature of the specified type
 *
 * XXX XXX XXX This function is rather dangerous
 */
static void do_cmd_wiz_named_friendly(int r_idx)
{
	(void)summon_named_creature(0, py, px, r_idx, (PM_ALLOW_SLEEP | PM_ALLOW_GROUP | PM_FORCE_PET));
}



/*
 * Hack -- Delete all nearby monsters
 */
static void do_cmd_wiz_zap(void)
{
	int i;


	/* Genocide everyone nearby */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip the mount */
		if (i == p_ptr->riding) continue;

		/* Delete nearby monsters */
		if (m_ptr->cdis <= MAX_SIGHT)
		{
			if (record_named_pet && is_pet(m_ptr) && m_ptr->nickname)
			{
				char m_name[80];

				monster_desc(m_name, m_ptr, MD_INDEF_VISIBLE);
				do_cmd_write_nikki(NIKKI_NAMED_PET, RECORD_NAMED_PET_WIZ_ZAP, m_name);
			}

			delete_monster_idx(i);
		}
	}
}


/*
 * Hack -- Delete all monsters
 */
static void do_cmd_wiz_zap_all(void)
{
	int i;

	/* Genocide everyone */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip the mount */
		if (i == p_ptr->riding) continue;

		if (record_named_pet && is_pet(m_ptr) && m_ptr->nickname)
		{
			char m_name[80];

			monster_desc(m_name, m_ptr, MD_INDEF_VISIBLE);
			do_cmd_write_nikki(NIKKI_NAMED_PET, RECORD_NAMED_PET_WIZ_ZAP, m_name);
		}

		/* Delete this monster */
		delete_monster_idx(i);
	}
}


/*
 * Create desired feature
 */
static void do_cmd_wiz_create_feature(void)
{
	static int   prev_feat = 0;
	static int   prev_mimic = 0;
	cave_type    *c_ptr;
	feature_type *f_ptr;
	char         tmp_val[160];
	int          tmp_feat, tmp_mimic;
	int          y, x;

	if (!tgt_pt(&x, &y)) return;

	c_ptr = &cave[y][x];

	/* Default */
	sprintf(tmp_val, "%d", prev_feat);

	/* Query */
#ifdef JP
	if (!get_string("地形: ", tmp_val, 3)) return;
#else
	if (!get_string("Feature: ", tmp_val, 3)) return;
#endif

	/* Extract */
	tmp_feat = atoi(tmp_val);
	if (tmp_feat < 0) tmp_feat = 0;
	else if (tmp_feat >= max_f_idx) tmp_feat = max_f_idx - 1;

	/* Default */
	sprintf(tmp_val, "%d", prev_mimic);

	/* Query */
#ifdef JP
	if (!get_string("地形 (mimic): ", tmp_val, 3)) return;
#else
	if (!get_string("Feature (mimic): ", tmp_val, 3)) return;
#endif

	/* Extract */
	tmp_mimic = atoi(tmp_val);
	if (tmp_mimic < 0) tmp_mimic = 0;
	else if (tmp_mimic >= max_f_idx) tmp_mimic = max_f_idx - 1;

	cave_set_feat(y, x, tmp_feat);
	c_ptr->mimic = tmp_mimic;

	f_ptr = &f_info[get_feat_mimic(c_ptr)];

	if (have_flag(f_ptr->flags, FF_GLYPH) ||
	    have_flag(f_ptr->flags, FF_MON_TRAP))
		c_ptr->info |= (CAVE_OBJECT);
	else if (have_flag(f_ptr->flags, FF_MIRROR))
		c_ptr->info |= (CAVE_GLOW | CAVE_OBJECT);

	/* Notice */
	note_spot(y, x);

	/* Redraw */
	lite_spot(y, x);

	/* Update some things */
	p_ptr->update |= (PU_FLOW);

	prev_feat = tmp_feat;
	prev_mimic = tmp_mimic;
}


#define NUM_O_SET 8
#define NUM_O_BIT 32

/*
 * Hack -- Dump option bits usage
 */
static void do_cmd_dump_options(void)
{
	int  i, j;
	FILE *fff;
	char buf[1024];
	int  **exist;

	/* Build the filename */
	path_build(buf, sizeof buf, ANGBAND_DIR_USER, "opt_info.txt");

	/* File type is "TEXT" */
	FILE_TYPE(FILE_TYPE_TEXT);

	/* Open the file */
	fff = my_fopen(buf, "a");

	/* Oops */
	if (!fff)
	{
#ifdef JP
		msg_format("ファイル %s を開けませんでした。", buf);
#else
		msg_format("Failed to open file %s.", buf);
#endif
		msg_print(NULL);
		return;
	}

	/* Allocate the "exist" array (2-dimension) */
	C_MAKE(exist, NUM_O_SET, int *);
	C_MAKE(*exist, NUM_O_BIT * NUM_O_SET, int);
	for (i = 1; i < NUM_O_SET; i++) exist[i] = *exist + i * NUM_O_BIT;

	/* Check for exist option bits */
	for (i = 0; option_info[i].o_desc; i++)
	{
		option_type *ot_ptr = &option_info[i];
		if (ot_ptr->o_var) exist[ot_ptr->o_set][ot_ptr->o_bit] = i + 1;
	}

	fprintf(fff, "[Option bits usage on Hengband %d.%d.%d]\n\n",
	        FAKE_VER_MAJOR - 10, FAKE_VER_MINOR, FAKE_VER_PATCH);

	fputs("Set - Bit (Page) Option Name\n", fff);
	fputs("------------------------------------------------\n", fff);
	/* Dump option bits usage */
	for (i = 0; i < NUM_O_SET; i++)
	{
		for (j = 0; j < NUM_O_BIT; j++)
		{
			if (exist[i][j])
			{
				option_type *ot_ptr = &option_info[exist[i][j] - 1];
				fprintf(fff, "  %d -  %02d (%4d) %s\n",
				        i, j, ot_ptr->o_page, ot_ptr->o_text);
			}
			else
			{
				fprintf(fff, "  %d -  %02d\n", i, j);
			}
		}
		fputc('\n', fff);
	}

	/* Free the "exist" array (2-dimension) */
	C_KILL(*exist, NUM_O_BIT * NUM_O_SET, int);
	C_KILL(exist, NUM_O_SET, int *);

	/* Close it */
	my_fclose(fff);

#ifdef JP
	msg_format("オプションbit使用状況をファイル %s に書き出しました。", buf);
#else
	msg_format("Option bits usage dump saved to file %s.", buf);
#endif
}


#ifdef ALLOW_SPOILERS

/*
 * External function
 */
extern void do_cmd_spoilers(void);

#endif /* ALLOW_SPOILERS */



/*
 * Hack -- declare external function
 */
extern void do_cmd_debug(void);



/*
 * Ask for and parse a "debug command"
 * The "command_arg" may have been set.
 */
void do_cmd_debug(void)
{
	int     x, y, n;
	char    cmd;


	/* Get a "debug command" */
	get_com("Debug Command: ", &cmd, FALSE);

	/* Analyze the command */
	switch (cmd)
	{
	/* Nothing */
	case ESCAPE:
	case ' ':
	case '\n':
	case '\r':
		break;

#ifdef ALLOW_SPOILERS

	/* Hack -- Generate Spoilers */
	case '"':
		do_cmd_spoilers();
		break;

#endif /* ALLOW_SPOILERS */

	/* Hack -- Help */
	case '?':
		do_cmd_help();
		break;

	/* Cure all maladies */
	case 'a':
		do_cmd_wiz_cure_all();
		break;

	/* Know alignment */
	case 'A':
		msg_format("Your alignment is %d.", p_ptr->align);
		break;

	/* Teleport to target */
	case 'b':
		do_cmd_wiz_bamf();
		break;

	case 'B':
		battle_monsters();
		break;

	/* Create any object */
	case 'c':
		wiz_create_item();
		break;

	/* Create a named artifact */
	case 'C':
		n = get_quantity("Which One? ", max_a_idx);
		wiz_create_named_art(n);
		break;

	/* Detect everything */
	case 'd':
		detect_all(DETECT_RAD_ALL * 3);
		break;

	/* Dimension_door */
	case 'D':
		wiz_dimension_door();
		break;

	/* Edit character */
	case 'e':
		do_cmd_wiz_change();
		break;

	/* Blue Mage Only */
	case 'E':
		if (p_ptr->pclass == CLASS_BLUE_MAGE)
		{
			do_cmd_wiz_blue_mage();
		}
		break;

	/* View item info */
	case 'f':
		identify_fully(FALSE);
		break;

	/* Create desired feature */
	case 'F':
		do_cmd_wiz_create_feature();
		break;

	/* Good Objects */
	case 'g':
		if (command_arg <= 0) command_arg = 1;
		acquirement(py, px, command_arg, FALSE, TRUE);
		break;

	/* Hitpoint rerating */
	case 'h':
		do_cmd_rerate(TRUE);
		break;

#ifdef MONSTER_HORDES
	case 'H':
		do_cmd_summon_horde();
		break;
#endif /* MONSTER_HORDES */

	/* Identify */
	case 'i':
		(void)identify_fully(FALSE);
		break;

	/* Go up or down in the dungeon */
	case 'j':
		do_cmd_wiz_jump();
		break;

	/* Self-Knowledge */
	case 'k':
		self_knowledge();
		break;

	/* Learn about objects */
	case 'l':
		do_cmd_wiz_learn();
		break;

	/* Magic Mapping */
	case 'm':
		map_area(DETECT_RAD_ALL * 3);
		break;

	/* Mutation */
	case 'M':
	{
	/*
		for (n = 0; n < 120; n++)
			mut_gain_random(NULL);*/
		mut_gain_choice(mut_human_pred);
	/*
		n = get_quantity("Which One? ", 500);
		if (n == 500)
		{
			int i;
			for (i = 0; i < 32; ++i)
				mut_gain(i);
		}
		else
			mut_gain(n);*/
		break;
	}
	/* Specific reward */
	case 'r':
		(void)gain_level_reward(command_arg);
		break;

	/* Summon _friendly_ named monster */
	case 'N':
		do_cmd_wiz_named_friendly(command_arg);
		break;

	/* Summon Named Monster */
	case 'n':
		n = get_quantity("Which One? ", 2000);
		do_cmd_wiz_named(n);
		break;

	/* Dump option bits usage */
	case 'O':
		do_cmd_dump_options();
		break;

	/* Object playing routines */
	case 'o':
		do_cmd_wiz_play();
		break;

	/* Phase Door */
	case 'p':
		teleport_player(10, 0L);
		break;

	/* Complete a Quest -KMW- */
	case 'q':
	{
		int i;
		for (i = 0; i < max_quests; i++)
		{
			if (i == QUEST_OBERON || i == QUEST_SERPENT)
				continue;

			if (is_fixed_quest_idx(i) && quest[i].status == QUEST_STATUS_TAKEN)
			{
				quest[i].status++;
				msg_print("Completed Quest");
				msg_print(NULL);
				break;
			}
		}
		if (i == max_quests)
		{
			msg_print("No current quest");
			msg_print(NULL);
		}
		break;
	}
	/* Make every dungeon square "known" to test streamers -KMW- */
	case 'u':
		for (y = 0; y < cur_hgt; y++)
		{
			for (x = 0; x < cur_wid; x++)
			{
				cave[y][x].info |= (CAVE_GLOW | CAVE_MARK);
			}
		}
		wiz_lite(FALSE);
		break;

	/* Summon Random Monster(s) */
	case 's':
		if (command_arg <= 0) command_arg = 1;
		do_cmd_wiz_summon(command_arg);
		break;

	/* Teleport */
	case 't':
		teleport_player(100, 0L);
		break;

	/* Very Good Objects */
	case 'v':
		if (command_arg <= 0) command_arg = 1;
		acquirement(py, px, command_arg, TRUE, TRUE);
		break;

	/* Wizard Light the Level */
	case 'w':
		wiz_lite((bool)(p_ptr->pclass == CLASS_NINJA));
		break;

	/* Increase Experience */
	case 'x':
		gain_exp(command_arg ? command_arg : (p_ptr->exp + 1));
		break;

	/* Zap Monsters (Genocide) */
	case 'z':
		do_cmd_wiz_zap();
		break;

	/* Zap Monsters (Omnicide) */
	case 'Z':
		do_cmd_wiz_zap_all();
		break;

	/* Hack -- whatever I desire */
	case '1':
		do_cmd_wiz_hack_chris1();
		break;

	case '2':
		do_cmd_wiz_hack_chris2();
		break;

	case '3':
		do_cmd_wiz_hack_chris3();
		break;

	case '4':
		do_cmd_wiz_hack_chris4();
		break;

	case '5':
		do_cmd_wiz_hack_chris5();
		break;

	case '6':
		do_cmd_wiz_hack_chris6();
		break;

	case '7':
		do_cmd_wiz_hack_chris7();
		break;

	case '8':
		do_cmd_wiz_hack_chris8();
		break;

	/* Not a Wizard Command */
	default:
		dispel_player();
		/*msg_print("That is not a valid debug command.");*/
		break;
	}
}


#else

#ifdef MACINTOSH
static int i = 0;
#endif

#endif

