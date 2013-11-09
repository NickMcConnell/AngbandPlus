/* Purpose: create a player character */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Forward declare
 */
//typedef struct hist_type hist_type;
typedef struct birth_menu birth_menu;

/*
 * A structure to hold the menus
 */
struct birth_menu
{
	cptr name;
	int real;
};


static cptr realm_jouhou[MAX_REALM] =
{
"魔道は感知、鑑定、攻撃に優れたメイジ系の基本となる魔法領域です。",
"炎系の魔法です。炎系の攻撃魔法が充実しています。また、多くの魔法は火のエレメントの影響を受けます。",
"水系の魔法です。水系の攻撃魔法が充実しています。また、多くの魔法は水のエレメントの影響を受けます。",
"大地系の魔法です。大地系の攻撃魔法が充実しています。また、多くの魔法は大地のエレメントの影響を受けます。",
"風系の魔法です。風系の攻撃魔法が充実しています。また、多くの魔法は風のエレメントの影響を受けます。",
"神聖は回復能力に優れた魔法です。治療や防御、感知魔法が多く含まれていますが、攻撃呪文もわずかに持っています。特に高レベルの呪文にはアンデッドを塵に帰す力があると言われています。",
"黒魔術である暗黒の魔法ほど邪悪なカテゴリーはありません。これらの呪文は比較的学ぶのが困難ですが、高レベルになると術者に生物とアンデッドを自由に操る能力を与えます。残念なことに、もっとも強力な呪文はその触媒として術者自身の血を必要とし、詠唱中にしばしば術者を傷つけます。",
"共生は、ペットと助け合うことに重きを置いた魔法領域です。ペットを回復したり、ペットを召喚したりすることができます。",
"ウィッチは補助魔法領域です。攻撃魔法はほとんどありませんが、自分の能力を強化したり、エレメントに影響を及ぼしたりする魔法が多くあります。また、高レベルになると自分や敵のエレメントを一時的に変更できる魔法もあります。",
"竜言語魔法は古代文明により開発され、現在では失われてしまった魔法領域です。攻撃魔法は凄まじい破壊力をもち、またその他の魔法は信じられないような効果を発動します。",
"破邪は「正義」の魔法です。直接敵を傷つける魔法が多く含まれ、特に邪悪な敵に対する力は恐るべきものがあります。しかし、善良な敵にはあまり効果がありません。",
};


static void birth_quit(void)
{
	remove_loc();
	quit(NULL);
}


/*
 *  Show specific help file
 */
static void show_help(cptr helpfile)
{
	/* Save screen */
	screen_save();

	/* Peruse the help file */
	(void)show_file(TRUE, helpfile, NULL, 0, 0);

	/* Load screen */
	screen_load();
}


/*
 * Save the current data for later
 */
static void save_prev_data(birther *birther_ptr)
{
	int i;

	/* Save the data */
	birther_ptr->psex = p_ptr->psex;
	birther_ptr->prace = p_ptr->prace;
	birther_ptr->pclass = p_ptr->pclass;
	birther_ptr->pelem = p_ptr->pelem;
	birther_ptr->age = p_ptr->age;
	birther_ptr->ht = p_ptr->ht;
	birther_ptr->wt = p_ptr->wt;
	birther_ptr->sc = p_ptr->sc;

	for (i = 0; i <= MAX_GOLD; i++)
	{
		birther_ptr->au[i] = p_ptr->au[i];
	}

	/* Save the stats */
	for (i = 0; i < A_MAX; i++)
	{
		birther_ptr->stat_max[i] = p_ptr->stat_max[i];
	}

	/* Save the hp & mana */
	birther_ptr->race_hp_lv1 = p_ptr->race_hp[0];
	birther_ptr->race_sp_lv1 = p_ptr->race_sp[0];
	birther_ptr->class_hp_lv1 = p_ptr->class_hp[p_ptr->pclass][0];
	birther_ptr->class_sp_lv1 = p_ptr->class_sp[p_ptr->pclass][0];

	/* Save the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(birther_ptr->history[i], p_ptr->history[i]);
	}
}


/*
 * Load the previous data
 */
static void load_prev_data(bool swap)
{
	int i;

	birther	temp;

	/*** Save the current data ***/
	if (swap) save_prev_data(&temp);


	/*** Load the previous data ***/

	/* Load the data */
	p_ptr->psex = previous_char.psex;
	p_ptr->prace = previous_char.prace;
	p_ptr->pclass = previous_char.pclass;
	p_ptr->pelem = p_ptr->celem = previous_char.pelem;
	p_ptr->age = previous_char.age;
	p_ptr->ht = previous_char.ht;
	p_ptr->wt = previous_char.wt;
	p_ptr->sc = previous_char.sc;

	for (i = 0; i <= MAX_GOLD; i++)
	{
		p_ptr->au[i] = previous_char.au[i];
	}

	/* Load the stats */
	for (i = 0; i < A_MAX; i++)
	{
		p_ptr->stat_cur[i] = p_ptr->stat_max[i] = previous_char.stat_max[i];
	}

	/* Load the hp & mana */
	p_ptr->race_hp[0] = previous_char.race_hp_lv1;
	p_ptr->race_sp[0] = previous_char.race_sp_lv1;
	p_ptr->class_hp[p_ptr->pclass][0] = previous_char.class_hp_lv1;
	p_ptr->class_sp[p_ptr->pclass][0] = previous_char.class_sp_lv1;
	p_ptr->mhp = p_ptr->race_hp[0] + p_ptr->class_hp[p_ptr->pclass][0];
	p_ptr->chp = p_ptr->mhp;
	p_ptr->msp = p_ptr->race_sp[0] + p_ptr->class_sp[p_ptr->pclass][0];
	p_ptr->csp = p_ptr->msp;

	/* Load the history */
	for (i = 0; i < 4; i++)
	{
		strcpy(p_ptr->history[i], previous_char.history[i]);
	}

	/*** Save the previous data ***/
	if (swap)
	{
		COPY(&previous_char, &temp, birther);
	}
}




/*
 * Returns adjusted stat -JK-  Algorithm by -JWT-
 */
int adjust_stat(int value, int amount)
{
	int i;

	/* Negative amounts */
	if (amount < 0)
	{
		/* Apply penalty */
		for (i = 0; i < (0 - amount); i++)
		{
			if (value >= 18+10)
			{
				value -= 10;
			}
			else if (value > 18)
			{
				value = 18;
			}
			else if (value > 3)
			{
				value--;
			}
		}
	}

	/* Positive amounts */
	else if (amount > 0)
	{
		/* Apply reward */
		for (i = 0; i < amount; i++)
		{
			if (value < 18)
			{
				value++;
			}
			else
			{
				value += 10;
			}
		}
	}

	/* Return the result */
	return value;
}




/*
 * Roll for some info that the auto-roller ignores
 */
static void get_extra(bool roll_hitdie)
{
	int            i, j;
	cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];
	s32b           tmp32s;

	/* Experience factor */
	p_ptr->expfact = rp_ptr->r_exp;
	for (i = 0; i < max_c_idx; i++) p_ptr->cexpfact[i] = class_info[i].c_exp;

	(void)C_WIPE(p_ptr->cexp_info, max_c_idx, cexp_info_type);

	for (i = 0; i < MAX_WT; i++)
		p_ptr->weapon_exp[i] = 0;

	for (i = 0; i < 10; i++)
		p_ptr->skill_exp[i] = 0;

	for (i = 0; i <= MAX_REALM; i++)
		p_ptr->magic_exp[i] = p_ptr->s_ptr->s_eff[i];

	/* Roll for hit point unless quick-start */
	if (roll_hitdie)
	{
		p_ptr->race_hp[0] = p_ptr->race_sp[0] = p_ptr->class_hp[p_ptr->pclass][0] = p_ptr->class_sp[p_ptr->pclass][0] = 0;

		/* Gain level 1 HP/mana */
		for (i = 1; i < 4; i++)
		{
			tmp32s = rand_spread(rp_ptr->r_mhp, 1);
			p_ptr->race_hp[0] += MAX(tmp32s, 0);

			tmp32s = rand_spread(rp_ptr->r_msp, 1);
			p_ptr->race_sp[0] += MAX(tmp32s, 0);

			tmp32s = rand_spread(cp_ptr->c_mhp, 1);
			p_ptr->class_hp[p_ptr->pclass][0] += MAX(tmp32s, 0);

			tmp32s = rand_spread(cp_ptr->c_msp, 1);
			p_ptr->class_sp[p_ptr->pclass][0] += MAX(tmp32s, 0);
		}
	}

	/* Paranoia */
	for (i = 1; i < PY_MAX_LEVEL; i++)
	{
		p_ptr->race_hp[i] = p_ptr->race_sp[i] = 0;
	}

	for (i = 0; i < max_c_idx; i++)
	{
		for (j = (i == p_ptr->pclass) ? 1 : 0; j < PY_MAX_LEVEL; j++)
		{
			p_ptr->class_hp[i][j] = p_ptr->class_sp[i][j] = 0;
		}
	}

	/* Initial hitpoints */
	p_ptr->mhp = p_ptr->race_hp[0] + p_ptr->class_hp[p_ptr->pclass][0];

	/* Initial mana */
	p_ptr->msp = p_ptr->race_sp[0] + p_ptr->class_sp[p_ptr->pclass][0];

	/* Initial class */
	cexp_ptr->max_max_clev = cexp_ptr->max_clev = cexp_ptr->clev = 1;

	/* No feedback HP & mana */
	p_ptr->player_ghp = p_ptr->player_gsp = 0;

	/* Initial skills */
	p_ptr->gx_dis = cp_ptr->x_dis;
	p_ptr->gx_dev = cp_ptr->x_dev;
	p_ptr->gx_sav = cp_ptr->x_sav;
	p_ptr->gx_stl = cp_ptr->x_stl;
	p_ptr->gx_srh = cp_ptr->x_srh;
	p_ptr->gx_fos = cp_ptr->x_fos;
	p_ptr->gx_spd = rp_ptr->rx_spd + cp_ptr->x_spd;
	p_ptr->gx_thn = cp_ptr->x_thn;
	p_ptr->gx_thb = cp_ptr->x_thb;

	p_ptr->death_regen = 5000;
}


/*
 * Get the racial history, and social class, using the "history charts".
 */
static void get_history(void)
{
	int i, n, chart, roll, social_class;

	char *s, *t;

	char buf[240];

	/* Clear the previous history strings */
	for (i = 0; i < 4; i++) p_ptr->history[i][0] = '\0';

	/* Clear the history text */
	buf[0] = '\0';

	if (astral_mode)
	{
		/* Acquire the textual history */
		strcat(buf, "あなたは先祖の剣に導かれて幻影の街に降り立ちました。その剣はあなたの手に吸い付いて離れようとしません。あなたの生死はあなたの勇敢さ、注意深さ、計画性、そしてあなたの持つ運にかかっています。あなたに幸運と、フィラーハの加護を。");
	}
	else
	{
		/* Initial social class */
		social_class = randint1(4);

		/* Starting place */
		chart = rp_ptr->hist;


		/* Process the history */
		while (chart)
		{
			/* Start over */
			i = 0;

			/* Roll for nobility */
			roll = randint1(100);


			/* Access the proper entry in the table */
			while ((chart != h_info[i].chart) || (roll > h_info[i].roll)) i++;

			/* Acquire the textual history */
			my_strcat(buf, (h_text + h_info[i].text), sizeof(buf));

			/* Add in the social class */
			social_class += (int)(h_info[i].bonus) - 50;

			/* Enter the next chart */
			chart = h_info[i].next;
		}


		/* Verify social class */
		if (social_class > 100) social_class = 100;
		else if (social_class < 1) social_class = 1;

		/* Save the social class */
		p_ptr->sc = social_class;
	}


	/* Skip leading spaces */
	for (s = buf; *s == ' '; s++) /* loop */;

	/* Get apparent length */
	n = strlen(s);

	/* Kill trailing spaces */

	while ((n > 0) && (s[n-1] == ' ')) s[--n] = '\0';

	{
		char temp[64 * 4];

		roff_to_buf(s, 60, temp, sizeof temp);
		t = temp;
		for (i = 0; i < 4; i++)
		{
			if (t[0] == 0) break;
			else
			{
				strcpy(p_ptr->history[i], t);
				t += strlen(t) + 1;
			}
		}
	}
}


/*
 * Computes character's age, height, and weight
 * by henkma
 */
static void get_ahw(void)
{
  int h_percent; /* 身長が平均にくらべてどのくらい違うか. */


  /* Calculate the age */
  p_ptr->age = rp_ptr->b_age + randint1(rp_ptr->m_age);
  
  /* Calculate the height/weight for males */
  if (p_ptr->psex == SEX_MALE)
	{
	  p_ptr->ht = randnor(rp_ptr->m_b_ht, rp_ptr->m_m_ht);
	  h_percent = (int)(p_ptr->ht) * 100 / (int)(rp_ptr->m_b_ht);
	  p_ptr->wt = randnor((int)(rp_ptr->m_b_wt) * h_percent /100
						  , (int)(rp_ptr->m_m_wt) * h_percent / 300 );
	}
  
  /* Calculate the height/weight for females */
  else if (p_ptr->psex == SEX_FEMALE)
	{
	  p_ptr->ht = randnor(rp_ptr->f_b_ht, rp_ptr->f_m_ht);
	  h_percent = (int)(p_ptr->ht) * 100 / (int)(rp_ptr->f_b_ht);
	  p_ptr->wt = randnor((int)(rp_ptr->f_b_wt) * h_percent /100
						  , (int)(rp_ptr->f_m_wt) * h_percent / 300 );
	}
}



static void k_info_reset(void)
{
	int i;

	/* Reset the "objects" */
	for (i = 1; i < max_k_idx; i++)
	{
		object_kind *k_ptr = &k_info[i];

		/* Reset "tried" */
		k_ptr->tried = FALSE;

		/* Reset "aware" */
		k_ptr->aware = FALSE;
	}
}


/*
 * Clear all the global "character" data
 */
static void player_wipe(void)
{
	int i;

	/* Hack -- zero the struct */
	(void)WIPE(p_ptr, player_type);

	/* Wipe the quests */
	for (i = MIN_RANDOM_QUEST; i <= MAX_RANDOM_QUEST_ASTRAL; i++)
	{
		r_info[quest[i].r_idx].flags1 &= ~(RF1_QUESTOR);
	}
	for (i = 0; i < max_quests; i++)
	{
		quest[i].status = QUEST_STATUS_UNTAKEN;

		quest[i].cur_num = 0;
		quest[i].max_num = 0;
		quest[i].type = 0;
		quest[i].level = 0;
		quest[i].r_idx = 0;
		quest[i].complev = 0;
	}

	/* No items */
	inven_cnt = 0;
	equip_cnt = 0;

	/* Clear the inventory */
	for (i = 0; i < INVEN_TOTAL; i++)
	{
		object_wipe(&inventory[i]);
	}

	mw_old_weight = 0;
	mw_diff_to_melee = 0;


	/* Reset the "monsters" */
	for (i = 1; i < max_r_idx; i++)
	{
		monster_race *r_ptr = &r_info[i];

		/* Hack -- Reset the counter */
		r_ptr->cur_num = 0;

		/* Hack -- Reset the max counter */
		r_ptr->max_num = 100;

		/* Hack -- Reset the max counter */
		if (r_ptr->flags1 & RF1_UNIQUE) r_ptr->max_num = 1;
		if (r_ptr->flags7 & RF7_NAZGUL) r_ptr->max_num = 8;

		/* Hack -- Ronwe is dead */
		if (i == MON_RONWE) r_ptr->max_num = 0;

		/* Clear player kills */
		r_ptr->r_pkills = 0;

		/* Default elements */
		if (r_ptr->flags3 & RF3_ELEM_MULTI) r_ptr->r_elem = NO_ELEM;
		else if (r_ptr->flags3 & RF3_ELEM_FIRE) r_ptr->r_elem = ELEM_FIRE;
		else if (r_ptr->flags3 & RF3_ELEM_AQUA) r_ptr->r_elem = ELEM_AQUA;
		else if (r_ptr->flags3 & RF3_ELEM_EARTH) r_ptr->r_elem = ELEM_EARTH;
		else if (r_ptr->flags3 & RF3_ELEM_WIND) r_ptr->r_elem = ELEM_WIND;
		else if (r_ptr->flags1 & RF1_UNIQUE) r_ptr->r_elem = randint0(ELEM_NUM);
		else r_ptr->r_elem = NO_ELEM;
	}


	/* Hack -- Well fed player */
	p_ptr->food = PY_FOOD_FULL - 1;

	p_ptr->infected = 0;

	/* Clean the mutation count */
	mutant_regenerate_mod = 100;

	/* Clear "cheat" options */
	cheat_peek = FALSE;
	cheat_hear = FALSE;
	cheat_room = FALSE;
	cheat_xtra = FALSE;
	cheat_know = FALSE;
	cheat_live = FALSE;
	cheat_save = FALSE;

	stop_the_time_player = FALSE;

	/* Default pet command settings */
	p_ptr->pet_follow_distance = PET_FOLLOW_DIST;
	p_ptr->pet_extra_flags = (PF_TELEPORT | PF_ATTACK_SPELL | PF_SUMMON_SPELL | PF_DISI_SPELL);

	/* Wipe the recall depths */
	for (i = 0; i < max_d_idx; i++)
	{
		max_dlv[i] = 0;
	}

	/* Level one */
	p_ptr->max_max_plv = p_ptr->max_plv = p_ptr->lev = 1;

	/* Initialize arena and rewards information -KMW- */
	p_ptr->arena_number = 0;
	p_ptr->inside_arena = FALSE;
	p_ptr->inside_quest = 0;
	p_ptr->exit_bldg = TRUE; /* only used for arena now -KMW- */

	/* Set the recall dungeon accordingly */
	dungeon_type = 0;
	p_ptr->recall_dungeon = DUNGEON_ARMORICA;

	/* Reset chaos frame */
	for (i = 0; i < ETHNICITY_NUM; i++)
	{
		chaos_frame[i] = 0;
	}

	/* Reset effect of "The Fool" */
	fool_effect_status = FOOL_STATUS_NONE;

	misc_event_flags = 0L;

	ambush_flag = FALSE;

	p_ptr->max_max_dlv = 0;
	p_ptr->max_dlv_mult = 0;
	p_ptr->winner_mult = 0;
	p_ptr->ogre_mult = 0;

}


/*
 *  Hook function for quest monsters
 */
static bool mon_hook_quest(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Random quests are in the dungeon */
	if (r_ptr->flags8 & RF8_WILD_ONLY) return FALSE;

	/* No random quests for aquatic monsters */
	if (r_ptr->flags7 & RF7_AQUATIC) return FALSE;

	/* No random quests for multiplying monsters */
	if (r_ptr->flags2 & RF2_MULTIPLY) return FALSE;

	/* No quests to kill friendly monsters */
	if (r_ptr->flags7 & RF7_FRIENDLY) return FALSE;

	return TRUE;
}


/*
 *  Initialize random quests and final quests
 */
static void init_dungeon_quests(void)
{
	int i;
	monster_race    *r_ptr;
	int min_random_quest = astral_mode ? MIN_RANDOM_QUEST_ASTRAL : MIN_RANDOM_QUEST;
	int number_of_quests = astral_mode ? MAX_RANDOM_QUEST_ASTRAL - MIN_RANDOM_QUEST_ASTRAL + 1 : MAX_RANDOM_QUEST - MIN_RANDOM_QUEST + 1;

	/* Init the random quests */
	init_flags = INIT_ASSIGN;
	p_ptr->inside_quest = min_random_quest;

	process_dungeon_file("q_info.txt", 0, 0, 0, 0);

	p_ptr->inside_quest = 0;

	/* Prepare allocation table */
	get_mon_num_prep(mon_hook_quest, NULL);

	/* Generate quests */
	for (i = min_random_quest + number_of_quests - 1; i >= min_random_quest; i--)
	{
		quest_type      *q_ptr = &quest[i];
		monster_race    *quest_r_ptr;
		int             r_idx;
		int             mon_lev = astral_mode ? (100 - q_ptr->level) : q_ptr->level;

		q_ptr->status = QUEST_STATUS_TAKEN;

		while (1)
		{
			/*
			 * Random monster 5 - 10 levels out of depth
			 * (depending on level)
			 */
			r_idx = get_mon_num(mon_lev + 5 + randint1(mon_lev / 10));
			r_ptr = &r_info[r_idx];

			if(!(r_ptr->flags1 & RF1_UNIQUE)) continue;

			if(r_ptr->flags1 & RF1_QUESTOR) continue;

			if(r_ptr->flags7 & RF7_FRIENDLY) continue;

			if(r_ptr->flags7 & RF7_AQUATIC) continue;

			if(r_ptr->flags8 & RF8_WILD_ONLY) continue;

			if (monster_is_runeweapon(r_idx)) continue;

			/*
			 * Accept monsters that are 2 - 6 levels
			 * out of depth depending on the quest level
			 */
			if (r_ptr->level > (mon_lev + (mon_lev / 20))) break;
		}

		q_ptr->r_idx = r_idx;
		quest_r_ptr = &r_info[q_ptr->r_idx];

		/* Mark uniques */
		quest_r_ptr->flags1 |= RF1_QUESTOR;

		q_ptr->max_num = 1;
	}

	if (astral_mode)
	{
		/* Init the quest (Compensation of Runeweapon) */
		init_flags = INIT_ASSIGN;
		p_ptr->inside_quest = QUEST_RUNEWEAPON;

		process_dungeon_file("q_info.txt", 0, 0, 0, 0);

		quest[QUEST_RUNEWEAPON].status = QUEST_STATUS_TAKEN;
	}
	else
	{
		/* Init the two main quests (Lancelot + Dolgarua) */
		init_flags = INIT_ASSIGN;
		p_ptr->inside_quest = QUEST_LANCELOT;

		process_dungeon_file("q_info.txt", 0, 0, 0, 0);

		quest[QUEST_LANCELOT].status = QUEST_STATUS_TAKEN;

		p_ptr->inside_quest = QUEST_DOLGARUA;

		process_dungeon_file("q_info.txt", 0, 0, 0, 0);

		quest[QUEST_DOLGARUA].status = QUEST_STATUS_TAKEN;

		p_ptr->inside_quest = QUEST_ARMORICA;

		process_dungeon_file("q_info.txt", 0, 0, 0, 0);

		quest[QUEST_ARMORICA].status = QUEST_STATUS_TAKEN;
	}

	p_ptr->inside_quest = 0;
}

/*
 * Reset turn
 */
static void init_turn(void)
{
	turn = 1;
	turn_limit = TURNS_PER_TICK * TOWN_DAWN * (MAX_DAYS - 1) + TURNS_PER_TICK * TOWN_DAWN * 3 / 4;

	dungeon_turn = 1;
	dungeon_turn_limit = TURNS_PER_TICK * TOWN_DAWN * (MAX_DAYS - 1) + TURNS_PER_TICK * TOWN_DAWN * 3 / 4;
}


/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
void player_outfit(void)
{
	int i;
	object_type	forge;
	object_type	*q_ptr;
	const start_item *e_ptr;


	/* Get local object */
	q_ptr = &forge;

	/* Give the player some food */
	if (!(rp_ptr->r_flags & PRF_NO_DIGEST))
	{
		/* Food rations */
		object_prep(q_ptr, lookup_kind(TV_FOOD, SV_FOOD_RATION));
		q_ptr->number = (byte)rand_range(3, 7);
		object_aware(q_ptr);
		object_known(q_ptr);
		(void)inven_carry(q_ptr);
	}
	/* Get local object */
	q_ptr = &forge;

	/* Hack -- Give the player some torches */
	if (!(class_info[p_ptr->pclass].c_flags & PCF_SEE_DARK_GRID))
	{
		object_prep(q_ptr, lookup_kind(TV_LITE, SV_LITE_TORCH));
		q_ptr->number = (byte)rand_range(3, 6);
		q_ptr->xtra4 = 2500;
		object_aware(q_ptr);
		object_known(q_ptr);
		(void)inven_carry(q_ptr);
	}

	/* Hack -- Give the player his equipment */
	for (i = 0; i < MAX_START_ITEMS; i++)
	{
		/* Access the item */
		e_ptr = &(cp_ptr->start_items[i]);

		/* Get local object */
		q_ptr = &forge;

		/* Hack	-- Give the player an object */
		if (e_ptr->tval > 0)
		{
			/* Get the object_kind */
			int k_idx = lookup_kind(e_ptr->tval, e_ptr->sval);

			/* Valid item? */
			if (!k_idx) continue;

			/* Prepare the item */
			object_prep(q_ptr, k_idx);
			q_ptr->number = (byte)rand_range(e_ptr->min, e_ptr->max);
			object_aware(q_ptr);
			object_known(q_ptr);
			(void)inven_carry(q_ptr);
		}
	}

	if (astral_mode)
	{
		runeweapon_type *runeweapon = &runeweapon_list[1];
		u32b flgs[TR_FLAG_SIZE];

		object_copy(q_ptr, &runeweapon->weapon);
		q_ptr->curse_flags |= (TRC_CURSED | TRC_HEAVY_CURSE | TRC_PERMA_CURSE);
		object_copy(&inventory[(runeweapon->weapon.tval != TV_BOW) ? INVEN_RARM : INVEN_BOW], q_ptr);

		runeweapon->status |= (RW_STATUS_FOUND);

		/* Increase the weight */
		p_ptr->total_weight += q_ptr->weight;

		/* Increment the equip counter by hand */
		equip_cnt++;

		/* Recalculate bonuses */
		p_ptr->update |= (PU_BONUS);

		/* Recalculate torch */
		p_ptr->update |= (PU_TORCH);

		/* Recalculate mana */
		p_ptr->update |= (PU_MANA);

		p_ptr->redraw |= (PR_EQUIPPY);

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);

		object_flags(q_ptr, flgs);
	}
}


/* Locations of the tables on the screen */
#define HEADER_ROW		1
#define INSTRUCT_ROW	3
#define QUESTION_ROW	7
#define TABLE_ROW		10
#define POINT_ROW		14

#define QUESTION_COL	2
#define SEX_COL			2
#define RACE_COL		14
#define RACE_AUX_COL    34
#define CLASS_COL		34
#define CLASS_AUX_COL   54
#define ELEM_COL		54
#define POINT_COL		10

#define INVALID_CHOICE 255

#define MAX_MENUS		25

/*
 * Clear the previous question
 */
static void clear_question(void)
{
	int i;

	for (i = QUESTION_ROW; i < TABLE_ROW; i++)
	{
		/* Clear line, position cursor */
		Term_erase(0, i, 255);
	}
}

/*
 * Generic "get choice from menu" function
 */
static int get_player_choice(birth_menu *choices, int num, int col, int wid,
                             cptr helpfile, void (*hook)(int))
{
	int top = 0, cur = 0, i;
	char c;
	char buf[80];
	bool done = FALSE;
	int hgt;
	byte attr;

	/* Autoselect if able */
	if (num == 1) done = TRUE;

	/* Clear */
	for (i = TABLE_ROW; i < Term->hgt; i++)
	{
		/* Clear */
		Term_erase(col, i, Term->wid - wid);
	}

	/* Choose */
	while (TRUE)
	{
		hgt = Term->hgt - TABLE_ROW - 1;

		/* Redraw the list */
		for (i = 0; ((i + top < num) && (i <= hgt)); i++)
		{
			if (i + top < 26)
			{
				sprintf(buf, "%c) %s", I2A(i + top), choices[i + top].name);
			}
			else
			{
				/* ToDo: Fix the ASCII dependency */
				sprintf(buf, "%c) %s", 'A' + (i + top - 26), choices[i + top].name);
			}

			/* Clear */
			Term_erase(col, i + TABLE_ROW, wid);

			/* Display */
			/* Highlight the current selection */
			if (i == (cur - top)) attr = TERM_L_BLUE;
			else attr = TERM_WHITE;

			Term_putstr(col, i + TABLE_ROW, wid, attr, buf);
		}

		if (done) return choices[cur].real;

		/* Display auxiliary information if any is available. */
		if (hook) hook(choices[cur].real);

		/* Move the cursor */
		put_str("", TABLE_ROW + cur - top, col);

		c = inkey();

		switch (c)
		{
		case 'Q':
			quit(NULL);

		case 'S':
			/* Mega Hack - go back. */
			return INVALID_CHOICE;

		case '*':
			/* Select a legal choice at random */
			cur = randint0(num);

			/* Move it onto the screen */
			if ((cur < top) || (cur > top + hgt))
			{
				top = cur;
			}

			/* Done */
			done = TRUE;
			break;

		case '?':
			show_help(helpfile);
			break;

		case '=':
			screen_save();
#ifdef JP
			do_cmd_options_aux(OPT_PAGE_BIRTH, "初期オプション((*)はスコアに影響)");
#else
			do_cmd_options_aux(OPT_PAGE_BIRTH, "Startup Opts((*)s effect score)");
#endif
			screen_load();
			break;

		case '\n':
		case '\r':
		case ' ':
			/* Done */
			return choices[cur].real;

		/* Going up? */
		case '8':
			if (cur != 0)
			{
				/* Move selection */
				cur--;
			}

			if ((top > 0) && ((cur - top) < 4))
			{
				/* Scroll up */
				top--;
			}
			break;

		/* Going down? */
		case '2':
			if (cur != (num - 1))
			{
				/* Move selection */
				cur++;
			}

			if ((top + hgt < (num - 1)) && ((top + hgt - cur) < 4))
			{
				/* Scroll down */
				top++;
			}
			break;

		default:
			if (isalpha(c))
			{
				int choice;

				if (islower(c))
				{
					choice = A2I(c);
				}
				else
				{
					choice = c - 'A' + 26;
				}

				/* Validate input */
				if ((choice > -1) && (choice < num))
				{
					cur = choice;

					/* Move it onto the screen */
					if ((cur < top) || (cur > top + hgt))
					{
						top = cur;
					}

					/* Done */
					done = TRUE;
				}
				else
				{
					bell();
				}
			}

			/* Invalid input */
			else bell();
			break;
		}
	}

	/* NOTREACHED */
}

/*
 * Display additional information about each race during the selection.
 */
static void race_aux_hook(int race)
{
	int i;
	char s[50];

	if ((race < 0) || (race >= max_p_idx)) return;

	/* Display relevant details. */
	for (i = 0; i < A_MAX; i++)
	{
		sprintf(s, "%s%+d", stat_names[i],
		race_info[race].r_adj[i]);
		Term_putstr(RACE_AUX_COL, TABLE_ROW + i, -1, TERM_WHITE, s);
	}

#ifdef JP
	sprintf(s, "ヒットダイス: %ld ", race_info[race].r_mhp);
	Term_putstr(RACE_AUX_COL, TABLE_ROW + A_MAX, -1, TERM_WHITE, s);
	sprintf(s, "マジックダイス: %ld ", race_info[race].r_msp);
	Term_putstr(RACE_AUX_COL, TABLE_ROW + A_MAX + 1, -1, TERM_WHITE, s);
	sprintf(s, "経験値: %d%% ", race_info[race].r_exp);
	Term_putstr(RACE_AUX_COL, TABLE_ROW + A_MAX + 2, -1, TERM_WHITE, s);
	sprintf(s, "赤外線視力: %d ft ", race_info[race].infra * 10);
	Term_putstr(RACE_AUX_COL, TABLE_ROW + A_MAX + 3, -1, TERM_WHITE, s);
#else
	sprintf(s, "Hit die: %ld ", race_info[race].r_mhp);
	Term_putstr(RACE_AUX_COL, TABLE_ROW + A_MAX, -1, TERM_WHITE, s);
	sprintf(s, "Mana die: %ld ", race_info[race].r_msp);
	Term_putstr(RACE_AUX_COL, TABLE_ROW + A_MAX + 1, -1, TERM_WHITE, s);
	sprintf(s, "Experience: %d%% ", race_info[race].r_exp);
	Term_putstr(RACE_AUX_COL, TABLE_ROW + A_MAX + 2, -1, TERM_WHITE, s);
	sprintf(s, "Infravision: %d ft ", race_info[race].infra * 10);
	Term_putstr(RACE_AUX_COL, TABLE_ROW + A_MAX + 3, -1, TERM_WHITE, s);
#endif
}

/*
 * Player race
 */
static bool get_player_race(void)
{
	int i, num = 0;
	birth_menu races[MAX_MENUS];

	/* Extra info */
#ifdef JP
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		"《種族》によってキャラクターの先天的な資質やボーナスが変化します。");
#else
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		"Your 'race' determines various intrinsic factors and bonuses.");
#endif

	/* Tabulate races */
	for (i = 0; i < max_p_idx; i++)
	{
		if (((race_info[i].r_flags & PRF_SEX_MALE) && (p_ptr->psex == SEX_FEMALE))
			|| ((race_info[i].r_flags == PRF_SEX_FEMALE) && (p_ptr->psex == SEX_MALE))
			|| (race_info[i].r_flags & PRF_EVOLUTION))
			continue;

		races[num].name = p_name + race_info[i].name;
		races[num++].real = i;
	}

#ifdef JP
	p_ptr->prace = get_player_choice(races, num, RACE_COL, 20, "jraceclas.txt#TheRaces", race_aux_hook);
#else
	p_ptr->prace = get_player_choice(races, num, RACE_COL, 20, "raceclas.txt#TheRaces", race_aux_hook);
#endif

	/* No selection? */
	if (p_ptr->prace == INVALID_CHOICE)
	{
		p_ptr->prace = 0;
		return (FALSE);
	}

	/* Save the race pointer */
	rp_ptr = &race_info[p_ptr->prace];

	/* Success */
	return (TRUE);
}


/*
 * Display additional information about each class during the selection.
 */
static void class_aux_hook(int class_idx)
{
	int i;
	char s[128], buf[8];

	if ((class_idx < 0) || (class_idx >= max_c_idx)) return;

	/* Display relevant details. */
	for (i = 0; i < A_MAX; i++)
	{
		if (class_info[class_idx].c_need[i]) cnv_stat(class_info[class_idx].c_need[i], buf);
		else strcpy(buf, "      ");
		sprintf(s, "%s%s", stat_names[i], buf);
		Term_putstr(CLASS_AUX_COL, TABLE_ROW + i, -1, TERM_WHITE, s);
	}
}


/*
 * Player class
 */
static bool get_player_class(void)
{
	int  i, num = 0;
	birth_menu classes[MAX_MENUS];

#ifdef JP
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		"《クラス》によってキャラクターの技能や魔法などが変化します。");
#else
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		"Your 'class' determines skill, magic realm, etc.");
#endif

	/* Tabulate classes */
	for (i = 0; i < max_c_idx; i++)
	{
		if (can_choose_class(i, CLASS_CHOOSE_MODE_BIRTH))
		{
			/* Save the string */
			classes[num].name = c_name + class_info[i].name;
			classes[num++].real = i;
		}
	}

#ifdef JP
	p_ptr->pclass = get_player_choice(classes, num, CLASS_COL, 20, "jraceclas.txt#TheClasses", class_aux_hook);
#else
	p_ptr->pclass = get_player_choice(classes, num, CLASS_COL, 20, "raceclas.txt#TheClasses", class_aux_hook);
#endif

	/* No selection? */
	if (p_ptr->pclass == INVALID_CHOICE)
	{
		p_ptr->pclass = 0;

		return (FALSE);
	}

	/* Set class */
	cp_ptr = &class_info[p_ptr->pclass];
	mp_ptr = &m_info[p_ptr->pclass];
	p_ptr->s_ptr = &s_info[p_ptr->pclass];

	return (TRUE);
}

/*
 * Player sex
 */
static bool get_player_sex(void)
{
	int i;
	birth_menu genders[MAX_SEXES];

	/* Extra info */
#ifdef JP
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_L_RED,
		"《性別》の違いは種族とクラスの選択に影響を及ぼします。");
#else
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_L_RED,
		"Your 'sex' effects race choice and class choice.");
#endif

	/* Tabulate genders */
	for (i = 0; i < MAX_SEXES; i++)
	{
		genders[i].name = sex_info[i].title;
		genders[i].real = i;
	}

#ifdef JP
	p_ptr->psex = get_player_choice(genders, MAX_SEXES, SEX_COL, 15, "jhelp.hlp", NULL);
#else
	p_ptr->psex = get_player_choice(genders, MAX_SEXES, SEX_COL, 15, "help.hlp", NULL);
#endif

	/* No selection? */
	if (p_ptr->psex == INVALID_CHOICE)
	{
		p_ptr->psex = 0;
		return (FALSE);
	}

	/* Save the sex pointer */
	sp_ptr = &sex_info[p_ptr->psex];

	return (TRUE);
}

/*
 * Player element
 */
static bool get_player_element(void)
{
	int i;
	birth_menu elements[ELEM_NUM];

#ifdef JP
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		"《エレメント》の違いは一部のクラスの魔法に影響を及ぼします。");
#else
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		"Your 'element' effects magic realm of several classes.");
#endif

	/* Tabulate elements */
	for (i = 0; i < ELEM_NUM; i++)
	{
		elements[i].name = elem_names[i];
		elements[i].real = i;
	}

#ifdef JP
	i = get_player_choice(elements, ELEM_NUM, ELEM_COL, 15, "jraceclas.txt#TheElements", NULL);
#else
	i = get_player_choice(elements, ELEM_NUM, ELEM_COL, 15, "raceclas.txt#TheElements", NULL);
#endif

	/* No selection? */
	if (i == INVALID_CHOICE)
	{
		p_ptr->pelem = -1;
		return (FALSE);
	}

	/* Save the element */
	p_ptr->pelem = i;
	p_ptr->celem = i;

	return (TRUE);
}

/*
 * Helper function for 'player_birth()'.
 *
 * This function allows the player to select a sex, race, and class, and
 * modify options (including the birth options).
 */
static bool player_birth_aux_1(void)
{
	/*** Instructions ***/

	/* Clear screen */
	Term_clear();

	/* Display some helpful information */
#ifdef JP
	Term_putstr(QUESTION_COL, HEADER_ROW, -1, TERM_L_BLUE,
	            "以下のメニューからキャラクタの要素を選んでください。");
	Term_putstr(QUESTION_COL, INSTRUCT_ROW, -1, TERM_WHITE,
	            "移動キーで項目をスクロールさせ、Enterで決定します。");
	Term_putstr(QUESTION_COL, INSTRUCT_ROW + 1, -1, TERM_WHITE,
	            "'*' でランダム決定、'S' でやり直します。");
	Term_putstr(QUESTION_COL, INSTRUCT_ROW + 2, -1, TERM_WHITE,
	            "'=' で初期オプション設定、'?' でヘルプ、'Q' で終了します。");

	/* Hack - highlight the key names */
	Term_putstr(QUESTION_COL + 0, INSTRUCT_ROW, -1, TERM_L_GREEN, "移動キー");
	Term_putstr(QUESTION_COL + 32, INSTRUCT_ROW, -1, TERM_L_GREEN, "Enter");
	Term_putstr(QUESTION_COL + 1, INSTRUCT_ROW + 1, -1, TERM_L_GREEN, "*");
	Term_putstr(QUESTION_COL + 21, INSTRUCT_ROW + 1, -1, TERM_L_GREEN, "S");
	Term_putstr(QUESTION_COL + 1, INSTRUCT_ROW + 2, -1, TERM_L_GREEN, "=");
	Term_putstr(QUESTION_COL + 27, INSTRUCT_ROW + 2, -1, TERM_L_GREEN, "?");
	Term_putstr(QUESTION_COL + 41, INSTRUCT_ROW + 2, -1, TERM_L_GREEN, "Q");
#else
	Term_putstr(QUESTION_COL, HEADER_ROW, -1, TERM_L_BLUE,
	            "Please select your character from the menu below:");
	Term_putstr(QUESTION_COL, INSTRUCT_ROW, -1, TERM_WHITE,
	            "Use the movement keys to scroll the menu, Enter to select the current");
	Term_putstr(QUESTION_COL, INSTRUCT_ROW + 1, -1, TERM_WHITE,
	            "menu item, '*' for a random menu item, 'S' to restart the character");
	Term_putstr(QUESTION_COL, INSTRUCT_ROW + 2, -1, TERM_WHITE,
	            "selection, '=' for the birth options, '?' for help, or 'Q' to quit.");

	/* Hack - highlight the key names */
	Term_putstr(QUESTION_COL + 8, INSTRUCT_ROW, -1, TERM_L_GREEN, "movement keys");
	Term_putstr(QUESTION_COL + 42, INSTRUCT_ROW, -1, TERM_L_GREEN, "Enter");
	Term_putstr(QUESTION_COL + 12, INSTRUCT_ROW + 1, -1, TERM_L_GREEN, "*");
	Term_putstr(QUESTION_COL + 40, INSTRUCT_ROW + 1, -1, TERM_L_GREEN, "S");
	Term_putstr(QUESTION_COL + 12, INSTRUCT_ROW + 2, -1, TERM_L_GREEN, "=");
	Term_putstr(QUESTION_COL + 39, INSTRUCT_ROW + 2, -1, TERM_L_GREEN, "?");
	Term_putstr(QUESTION_COL + 56, INSTRUCT_ROW + 2, -1, TERM_L_GREEN, "Q");
#endif

	/* Choose the player's sex */
	if (!get_player_sex()) return (FALSE);

	/* Clean up */
	clear_question();

	/* Choose the player's race */
	if (!get_player_race()) return (FALSE);

	/* Clean up */
	clear_question();

	/* Choose the player's class */
	if (!get_player_class()) return (FALSE);

	/* Clean up */
	clear_question();

	/* Choose the player's element */
	if (!get_player_element()) return (FALSE);

	/* Clear */
	Term_clear();

	screen_save();
#ifdef JP
	do_cmd_options_aux(OPT_PAGE_BIRTH, "初期オプション((*)はスコアに影響)");
#else
	do_cmd_options_aux(OPT_PAGE_BIRTH, "Startup Opts((*)s effect score)");
#endif
	screen_load();

	/* Clear */
	Term_clear();

	/* Reset turn; before auto-roll and after choosing race */
	init_turn();

	/* Done */
	return (TRUE);
}

/*
 * Initial stat costs (initial stats always range from 10 to 18 inclusive).
 */
#define POINTS		60
#define BASE_STAT	10
#define STAT_LIMIT  18

static int birth_stat_costs[(STAT_LIMIT - BASE_STAT) + 1] = { 0, 1, 2, 4, 7, 11, 16, 22, 30 };

/*
 * Helper function for 'player_birth()'.
 *
 * This function handles "point-based" character creation.
 *
 * The player selects, for each stat, a value from 10 to 18 (inclusive),
 * each costing a certain amount of points (as above), from a pool of 54
 * available points, to which race/class modifiers are then applied.
 *
 * Each unused point is converted into 50 gold pieces.
 */

static bool player_birth_aux_2(void)
{
	int i, j;

	byte stat = 0, old_stat = 0;

	byte stats[A_MAX];
	byte base_stats[A_MAX];

	int cost = 0, fix_cost, remain_point;

	bool flag = FALSE;

	char buf[80];
	char base[8], min[8], max[8], inp[8];

	/* Initialize stats */
	for (i = 0; i < A_MAX; i++)
	{
		int c_need = cp_ptr->c_need[i] ? adjust_stat(cp_ptr->c_need[i], -rp_ptr->r_adj[i]) : 0;

		/* Initial base stats */
		if (c_need > BASE_STAT) base_stats[i] = (byte)c_need;
		else base_stats[i] = BASE_STAT;

		/* Initial stats */
		stats[i] = base_stats[i];
		cost += birth_stat_costs[stats[i] - BASE_STAT];
	}
	fix_cost = cost;

	/* Roll for base hitpoints */
	get_extra(TRUE);

	/* Roll for age/height/weight */
	get_ahw();

	/* Roll for social class */
	get_history();

#ifdef JP
	Term_putstr(QUESTION_COL, HEADER_ROW, -1, TERM_L_BLUE,
	            "キャラクタの能力値を設定してください。");
	put_str("性別   : ", INSTRUCT_ROW, QUESTION_COL - 1);
	put_str("種族   : ", INSTRUCT_ROW + 1, QUESTION_COL - 1);
	put_str("クラス : ", INSTRUCT_ROW + 2, QUESTION_COL - 1);
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		"選択したクラスによって必要なコストがかかった状態での設定となっています。");
	put_str("        基本値  最小値  最大値  (コスト)  種族     合計値", POINT_ROW - 1, POINT_COL);
#else
	Term_putstr(QUESTION_COL, HEADER_ROW, -1, TERM_L_BLUE,
	            "Configure stats of character.");
	put_str("Sex    : ", INSTRUCT_ROW, QUESTION_COL - 1);
	put_str("Race   : ", INSTRUCT_ROW + 1, QUESTION_COL - 1);
	put_str("Class  : ", INSTRUCT_ROW + 2, QUESTION_COL - 1);
	Term_putstr(QUESTION_COL, QUESTION_ROW, -1, TERM_YELLOW,
		"Points are already reduced by chosen class.");
	put_str("          Base     Min     Max    (Cost)  Race      Total", POINT_ROW - 1, POINT_COL);
#endif
	c_put_str(TERM_L_BLUE, sp_ptr->title, INSTRUCT_ROW, QUESTION_COL + 8);
	c_put_str(TERM_L_BLUE, p_name + rp_ptr->name, INSTRUCT_ROW + 1, QUESTION_COL + 8);
	c_put_str(TERM_L_BLUE, c_name + cp_ptr->name, INSTRUCT_ROW + 2, QUESTION_COL + 8);

	for (i = 0; i < A_MAX; i++)
	{
		/* Prepare a prompt */
		cnv_stat(stats[i], base);
		cnv_stat(base_stats[i], min);
		remain_point = POINTS - fix_cost + birth_stat_costs[base_stats[i] - BASE_STAT];
		for (j = base_stats[i] - BASE_STAT; j < (STAT_LIMIT - BASE_STAT); j++)
		{
			if (birth_stat_costs[j + 1] > remain_point) break;
		}
		cnv_stat(BASE_STAT + j, max);
		cnv_stat(adjust_stat(stats[i], rp_ptr->r_adj[i]), inp);
		sprintf(buf, "%6s  %6s  %6s  %6s      (%2d)   %+3d  =  %6s",
			stat_names[i], base, min, max, birth_stat_costs[stats[i] - BASE_STAT],
			rp_ptr->r_adj[i], inp);
		put_str(buf, POINT_ROW + i, POINT_COL);
	}

	/* Interact */
	while (TRUE)
	{
		/* Reset cost */
		cost = 0;

		/* Process stats */
		for (i = 0; i < A_MAX; i++)
		{
			/* Total cost */
			cost += birth_stat_costs[stats[i] - BASE_STAT];
		}

		/* Restrict cost */
		if (cost > POINTS)
		{
			/* Warning */
			bell();

			/* Reduce stat */
			stats[stat]--;

			/* Recompute costs */
			continue;
		}

		c_put_str(TERM_WHITE, buf, POINT_ROW + old_stat, POINT_COL);

		/* Prompt XXX XXX XXX */
#ifdef JP
		sprintf(buf, "コスト合計: %2d/%d  2/8/j/k で移動,  4/6/h/l で変更,  Enter で決定", cost, POINTS);
#else
		sprintf(buf, "Total Cost %2d/%d.  Use 2/8/j/k to move, 4/6/h/l to modify, Enter to accept.", cost, POINTS);
#endif
		c_put_str(TERM_L_BLUE, buf, POINT_ROW - 3, POINT_COL);

		if (cost < POINTS)
		{
#ifdef JP
			sprintf(buf, "使われていないポイントは$%dの紙幣に変換されます。        ", (50 * (POINTS - cost)));
#else
			sprintf(buf, "Unused points are converted into $%d notes.        ", (50 * (POINTS - cost)));
#endif
			put_str(buf, POINT_ROW + A_MAX + 1, POINT_COL);
		}
		else clear_from(POINT_ROW + A_MAX + 1);

		/* Prepare a prompt */
		cnv_stat(stats[stat], base);
		cnv_stat(base_stats[stat], min);
		remain_point = POINTS - fix_cost + birth_stat_costs[base_stats[stat] - BASE_STAT];
		for (j = base_stats[stat] - BASE_STAT; j < (STAT_LIMIT - BASE_STAT); j++)
		{
			if (birth_stat_costs[j + 1] > remain_point) break;
		}
		cnv_stat(BASE_STAT + j, max);
		cnv_stat(adjust_stat(stats[stat], rp_ptr->r_adj[stat]), inp);
		sprintf(buf, "%6s  %6s  %6s  %6s      (%2d)   %+3d  =  %6s",
			stat_names[stat], base, min, max, birth_stat_costs[stats[stat] - BASE_STAT],
			rp_ptr->r_adj[stat], inp);
		c_put_str(TERM_L_GREEN, buf, POINT_ROW + stat, POINT_COL);
		c_put_str(TERM_YELLOW, base, POINT_ROW + stat, POINT_COL + 8);

		old_stat = stat;

		/* Place cursor just after current base stat */
		Term_gotoxy(POINT_COL + 14, POINT_ROW + stat);

		/* Get key */
		switch (inkey())
		{
		/* Quit */
		case 'Q':
			quit(NULL);

		/* Start over */
		case 'S':
			return (FALSE);

		/* Done */
		case '\r':
		case '\n':
		case ESCAPE:
			flag = TRUE;
			break;

		/* Prev stat */
		case '8':
		case 'k':
			stat = (stat + A_MAX - 1) % A_MAX;
			break;

		/* Next stat */
		case '2':
		case 'j':
			stat = (stat + 1) % A_MAX;
			break;

		/* Decrease stat */
		case '4':
		case 'h':
			if (stats[stat] > base_stats[stat]) stats[stat]--;
			break;

		/* Increase stat */
		case '6':
		case 'l':
			if (stats[stat] < STAT_LIMIT) stats[stat]++;
			break;
		}
		if (flag) break;
	}

	/* Process stats */
	for (i = 0; i < A_MAX; i++)
	{
		p_ptr->stat_cur[i] = p_ptr->stat_max[i] = stats[i];
	}

	/* Gold is inversely proportional to cost */
	p_ptr->au[SV_GOLD_NOTE] = (50 * (POINTS - cost)) + (easy_band ? 10000000L : 100);

	/* Calculate the bonuses and hitpoints */
	p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_GOLD);

	/* Update stuff */
	update_stuff();

	/* Fully healed */
	p_ptr->chp = p_ptr->mhp;

	/* Fully rested */
	p_ptr->csp = p_ptr->msp;

	/* Done */
	return (TRUE);
}


/*
 *  Character background edit-mode
 */
static void edit_history(void)
{
	char old_history[4][60];
	int y = 0, x = 0;
	int i, j;

	/* Edit character background */
	for (i = 0; i < 4; i++)
	{
		sprintf(old_history[i], "%s", p_ptr->history[i]);
	}
	/* Turn 0 to space */
	for (i = 0; i < 4; i++)
	{
		for (j = 0; p_ptr->history[i][j]; j++) /* loop */;

		for (; j < 59; j++) p_ptr->history[i][j] = ' ';
		p_ptr->history[i][59] = '\0';
	}
	display_player(1);
#ifdef JP
	c_put_str(TERM_L_GREEN, "(キャラクターの生い立ち - 編集モード)", 11, 20);
#else
	c_put_str(TERM_L_GREEN, "(Character Background - Edit Mode)", 11, 20);
#endif

	while (TRUE)
	{
		int skey;
		char c;

		for (i = 0; i < 4; i++)
		{
			put_str(p_ptr->history[i], i + 12, 10);
		}
#ifdef JP
		if (iskanji2(p_ptr->history[y], x))
			c_put_str(TERM_L_BLUE, format("%c%c", p_ptr->history[y][x],p_ptr->history[y][x+1]), y + 12, x + 10);
		else
#endif
		c_put_str(TERM_L_BLUE, format("%c", p_ptr->history[y][x]), y + 12, x + 10);

		/* Place cursor just after cost of current stat */
		Term_gotoxy(x + 10, y + 12);

		/* Get special key code */
		skey = inkey_special(TRUE);

		/* Get a character code */
		if (!(skey & SKEY_MASK)) c = (char)skey;
		else c = 0;

		if (skey == SKEY_UP || c == KTRL('p'))
		{
			y--;
			if (y < 0) y = 3;
#ifdef JP
			if ((x > 0) && (iskanji2(p_ptr->history[y], x-1))) x--;
#endif
		}
		else if (skey == SKEY_DOWN || c == KTRL('n'))
		{
			y++;
			if (y > 3) y = 0;
#ifdef JP
			if ((x > 0) && (iskanji2(p_ptr->history[y], x-1))) x--;
#endif
		}
		else if (skey == SKEY_RIGHT || c == KTRL('f'))
		{
#ifdef JP
			if (iskanji2(p_ptr->history[y], x)) x++;
#endif
			x++;
			if (x > 58)
			{
				x = 0;
				if (y < 3) y++;
			}
		}
		else if (skey == SKEY_LEFT || c == KTRL('b'))
		{
			x--;
			if (x < 0)
			{
				if (y)
				{
					y--;
					x = 58;
				}
				else x = 0;
			}

#ifdef JP
			if ((x > 0) && (iskanji2(p_ptr->history[y], x-1))) x--;
#endif
		}
		else if (c == '\r' || c == '\n')
		{
			Term_erase(0, 11, 255);
			Term_erase(0, 17, 255);
#ifdef JP
			put_str("(キャラクターの生い立ち - 編集済み)", 11, 20);
#else
			put_str("(Character Background - Edited)", 11, 20);
#endif
			break;
		}
		else if (c == ESCAPE)
		{
			clear_from(11);
#ifdef JP
			put_str("(キャラクターの生い立ち)", 11, 25);
#else
			put_str("(Character Background)", 11, 25);
#endif

			for (i = 0; i < 4; i++)
			{
				sprintf(p_ptr->history[i], "%s", old_history[i]);
				put_str(p_ptr->history[i], i + 12, 10);
			}
			break;
		}
		else if (c == '\010')
		{
			x--;
			if (x < 0)
			{
				if (y)
				{
					y--;
					x = 58;
				}
				else x = 0;
			}

			p_ptr->history[y][x] = ' ';
#ifdef JP
			if ((x > 0) && (iskanji2(p_ptr->history[y], x - 1)))
			{
				x--;
				p_ptr->history[y][x] = ' ';
			}
#endif
		}
#ifdef JP
		else if (iskanji(c) || isprint(c))
#else
		else if (isprint(c)) /* BUGFIX */
#endif
		{
#ifdef JP
			if (iskanji2(p_ptr->history[y], x))
			{
				p_ptr->history[y][x+1] = ' ';
			}

			if (iskanji(c))
			{
				if (x > 57)
				{
					x = 0;
					y++;
					if (y > 3) y = 0;
				}

				if (iskanji2(p_ptr->history[y], x+1))
				{
					p_ptr->history[y][x+2] = ' ';
				}

				p_ptr->history[y][x++] = c;

				c = inkey();
			}
#endif
			p_ptr->history[y][x++] = c;
			if (x > 58)
			{
				x = 0;
				y++;
				if (y > 3) y = 0;
			}
		}
	} /* while (TRUE) */

}


/*
 * Helper function for 'player_birth()'
 */
static bool player_birth_full(void)
{
	char c;

	/* Ask questions */
	if (!player_birth_aux_1()) return FALSE;
	if (!player_birth_aux_2()) return FALSE;

	/*** Edit character background ***/
	edit_history();

	/* Get a name, recolor it, prepare savefile */
	get_name();

	/* Process the player name (accept as savefile name) */
	process_player_name(creating_savefile);

	/* Display the player */
	display_player(0);

	/*** Finish up ***/

	/* Prompt for it */
#ifdef JP
	prt("[ 'Q' 中断, 'S' 初めから, Enter ゲーム開始 ]", 23, 14);
#else
	prt("['Q'uit, 'S'tart over, or Enter to continue]", 23, 10);
#endif


	/* Get a key */
	c = inkey();

	/* Quit */
	if (c == 'Q') birth_quit();

	/* Start over */
	if (c == 'S') return (FALSE);


	/* Initialize random quests */
	init_dungeon_quests();

	/* Save character data for quick start */
	save_prev_data(&previous_char);
	previous_char.quick_ok = TRUE;

	/* Accept */
	return (TRUE);
}


/*
 * Helper function for 'player_birth()'
 */
static bool player_birth_quick(bool prepare_astral)
{
	runeweapon_type *runeweapon = &runeweapon_list[1];
	bool illegal_generated = FALSE;

	if (prepare_astral)
	{
		/* Clear screen */
		Term_clear();

		illegal_generated = (runeweapon->status & RW_STATUS_ILLEGAL) ? TRUE : FALSE;

#ifdef JP
		put_str("アストラルモードに突入できます。", 2, 2);
		put_str("(スナップドラゴン武器を装備して死者の宮殿の98階からスタート)", 3, 2);

		if (illegal_generated)
			c_put_str(TERM_L_RED, "不正なスナップドラゴン武器なのでスコアには記録されません。", 7, 2);

		put_str("突入しますか？[y/n, 'Q'終了]", 5, 2);
#else
		put_str("You can enter astral mode.", 2, 2);
		put_str("(Start from dungeon level 98 of Death Palace with Runeweapon)", 3, 2);

		if (illegal_generated)
			c_put_str(TERM_L_RED, "Illegal snap dragon weapon, so your score will not be recorded.", 7, 2);

		put_str("Enter astral mode? [y/n, 'Q' Quit]", 5, 2);
#endif
	}
	else
	{
		if (!previous_char.quick_ok) return FALSE;

		/* Clear screen */
		Term_clear();

		/* Extra info */
#ifdef JP
		put_str("クイック・スタートを使うと以前と全く同じキャラクターで始められます。", 2, 2);
		put_str("クイック・スタートを使いますか？[y/n, 'Q'終了]", 4, 2);
#else
		put_str("Do you want to use the quick start function(same character as your last one).", 2, 2);
		put_str("Use quick start? [y/n, 'Q' Quit]", 4, 2);
#endif
	}

	/* Choose */
	while (1)
	{
		switch (inkey())
		{
		case 'Q':
			quit(NULL);

		case 'Y':
		case 'y':
			load_prev_data(FALSE);

			if (prepare_astral)
			{
				astral_mode = TRUE;

				/* Create "the Body of Ancestor" */
				create_runeweapon(1);

				/* Special history for astral mode */
				get_history();
			}

			init_dungeon_quests();
			init_turn();

			sp_ptr = &sex_info[p_ptr->psex];
			rp_ptr = &race_info[p_ptr->prace];
			cp_ptr = &class_info[p_ptr->pclass];
			mp_ptr = &m_info[p_ptr->pclass];
			p_ptr->s_ptr = &s_info[p_ptr->pclass];

			/* Calc hitdie, but don't roll */
			get_extra(FALSE);

			/* Calculate the bonuses and hitpoints */
			p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_GOLD);

			/* Update stuff */
			update_stuff();

			/* Fully healed */
			p_ptr->chp = p_ptr->mhp;

			/* Fully rested */
			p_ptr->csp = p_ptr->msp;

			/* Process the player name */
			process_player_name(FALSE);

			if (prepare_astral)
			{
				int  i;
				char tmp_str[80];

				/* Clear screen */
				Term_clear();

				/* Clear the text */
				for (i = 0; i < 10; i++)
				{
					quest_text[i][0] = '\0';
				}

				quest_text_line = 0;

				/* Get the quest text */
				init_flags = INIT_SHOW_TEXT | INIT_ASSIGN;
				p_ptr->inside_quest = QUEST_RUNEWEAPON;
				process_dungeon_file("q_info.txt", 0, 0, 0, 0);
				p_ptr->inside_quest = 0;

				sprintf(tmp_str, "%s (%s)", runeweapon->ancestor, p_name + race_info[runeweapon->race].name);
				prt(tmp_str, 2, 1);

				prt(quest[QUEST_RUNEWEAPON].name, 7, 0);

				for (i = 0; i < 10; i++)
				{
					c_put_str(TERM_YELLOW, quest_text[i], i + 8, 0);
				}

				/* Wait for response */
				pause_line(20);

				if (illegal_generated)
				{
					/* Mark savefile */
					p_ptr->noscore |= 0x0002;
				}
			}

			/* Generated */
			return TRUE;

		case 'N':
		case 'n':
			/* Don't use quick start */
			return FALSE;

		case '?':
#ifdef JP
			show_help("jbirth.txt#QuickStart");
#else
			show_help("birth.txt#QuickStart");
#endif
			break;
		}
	}

	/* NOTREACHED */
}


/*
 * Create a new character.
 *
 * Note that we may be called with "junk" leftover in the various
 * fields, so we must be sure to clear them first.
 */
void player_birth(void)
{
	int i, j;
	char buf[80];
	bool allow_astral_mode;
	s32b ancestor_au[MAX_GOLD + 1];

	playtime = 0;

	/* 
	 * Wipe monsters in old dungeon
	 * This wipe destroys value of m_list[].cur_num .
	 */
	wipe_m_list();

	process_runeweapon_list();
	allow_astral_mode = astral_mode;
	astral_mode = FALSE;

	if (allow_astral_mode)
	{
		object_type *o_ptr;

		/* Preserve gold of ancestor */
		for (i = 0; i <= MAX_GOLD; i++)
		{
			ancestor_au[i] = p_ptr->au[i];
		}

		/* Allocate it */
		C_MAKE(ancestor_inventory, INVEN_TOTAL, object_type);
		ancestor_inven_cnt = 0;
		for (i = 0; i < INVEN_TOTAL; i++)
		{
			o_ptr = &inventory[i];

			if (!o_ptr->k_idx) continue;

			if (object_is_runeweapon(o_ptr) && o_ptr->art_name && (o_ptr->xtra3 > 0)) continue;

			/* Hack -- memorize it */
			o_ptr->marked |= OM_FOUND;

			object_copy(&ancestor_inventory[ancestor_inven_cnt], o_ptr);
			ancestor_inven_cnt++;
		}
		/* If no items are in the inventory, free the array */
		if (!ancestor_inven_cnt) C_KILL(ancestor_inventory, INVEN_TOTAL, object_type);
	}

	/* Create a new character */
	while (1)
	{
		/* Wipe the player */
		player_wipe();

		/* Astral mode creation */
		if (allow_astral_mode)
		{
			if (player_birth_quick(TRUE)) break;
		}

		/* Quick creation */
		if (player_birth_quick(FALSE)) break;

		/* Roll up a new character */
		if (player_birth_full()) break;
	}

	/* Note player birth in the message recall */
	message_add(" ");
	message_add("  ");
	message_add("====================");
	message_add(" ");
	message_add("  ");

#ifdef JP
	do_cmd_write_nikki(NIKKI_GAMESTART, 1, "-------- 新規ゲーム開始 --------");
#else
	do_cmd_write_nikki(NIKKI_GAMESTART, 1, "-------- Start New Game --------");
#endif
	do_cmd_write_nikki(NIKKI_HIGAWARI, 0, NULL);

#ifdef JP
	sprintf(buf,"                            性別に%sを選択した。", sex_info[p_ptr->psex].title);
#else
	sprintf(buf,"                            choose %s personality.", sex_info[p_ptr->psex].title);
#endif
	do_cmd_write_nikki(NIKKI_BUNSHOU, 1, buf);

#ifdef JP
	sprintf(buf,"                            種族に%sを選択した。", p_name + race_info[p_ptr->prace].name);
#else
	sprintf(buf,"                            choose %s race.", p_name + race_info[p_ptr->prace].name);
#endif
	do_cmd_write_nikki(NIKKI_BUNSHOU, 1, buf);

#ifdef JP
	sprintf(buf,"                            クラスに%sを選択した。", c_name + class_info[p_ptr->pclass].name);
#else
	sprintf(buf,"                            choose %s class.", c_name + class_info[p_ptr->pclass].name);
#endif
	do_cmd_write_nikki(NIKKI_BUNSHOU, 1, buf);

#ifdef JP
	sprintf(buf,"                            エレメントに%sを選択した。", elem_names[p_ptr->pelem]);
#else
	sprintf(buf,"                            choose %s element.", elem_names[p_ptr->pelem]);
#endif
	do_cmd_write_nikki(NIKKI_BUNSHOU, 1, buf);

	if (p_ptr->noscore)
#ifdef JP
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "不正なキャラクタで開始してスコアを残せなくなった。");
#else
		do_cmd_write_nikki(NIKKI_BUNSHOU, 0, "give up recording score to use illegal character.");
#endif

	/* Default elements of Denim & Vice are dependent on player's element*/
	r_info[MON_VICE].r_elem = get_opposite_elem(p_ptr->pelem);
	r_info[MON_DENIM].r_elem = p_ptr->pelem;

	if (astral_mode)
	{
		for (i = 0; i <= MAX_GOLD; i++) p_ptr->au[i] += ancestor_au[i];
		p_ptr->update |= (PU_GOLD);
		update_stuff();
		move_inventory_to_home();
	}
	else
	{
		/* Free the array */
		if (ancestor_inventory) C_KILL(ancestor_inventory, INVEN_TOTAL, object_type);

		/* Init array for "Monster Stock" */
		(void)C_WIPE(stock_mon, MAX_STOCK_MON, monster_type);

		/* Start with no artifacts made yet */
		for (i = 0; i < max_a_idx; i++)
		{
			artifact_type *a_ptr = &a_info[i];
			a_ptr->cur_num = 0;
		}

		/* Reset the objects */
		k_info_reset();
	}

	/* Init the shops */
	for (i = 1; i < max_towns; i++)
	{
		for (j = 0; j < MAX_STORES; j++)
		{
			if (!astral_mode || (i != 1) || (j != STORE_HOME))
			{
				/* Initialize */
				store_init(i, j);
			}
		}
	}

	/* Init shop of Lost Island */
	store_init(NO_TOWN, STORE_BLACK);


	/* Generate the random seeds for the wilderness */
	seed_wilderness();

	/* Set the message window flag as default */
	if (!window_flag[1])
		window_flag[1] |= PW_MESSAGE;

	/* Set the inv/equip window flag as default */
	if (!window_flag[2])
		window_flag[2] |= PW_INVEN;
}


void dump_yourself(FILE *fff)
{
	char temp[80*10];
	int i, j;
	cptr t;

	if (!fff) return;

	if (!(cp_ptr->c_flags & PCF_REINCARNATE))
	{
		roff_to_buf(p_text + race_info[p_ptr->prace].text, 78, temp, sizeof temp);
		fprintf(fff, "\n\n");
#ifdef JP
		fprintf(fff, "種族: %s\n", p_name + race_info[p_ptr->prace].name);
#else
		fprintf(fff, "Race: %s\n", p_name + race_info[p_ptr->prace].name);
#endif
		t = temp;
		for (i = 0; i < 10; i++)
		{
			if (t[0] == 0) break;
			fprintf(fff, "%s\n",t);
			t += strlen(t) + 1;
		}
	}

	roff_to_buf(c_text + class_info[p_ptr->pclass].text, 78, temp, sizeof temp);
	fprintf(fff, "\n");
#ifdef JP
	fprintf(fff, "クラス: %s\n", c_name + class_info[p_ptr->pclass].name);
#else
	fprintf(fff, "Class: %s\n", c_name + class_info[p_ptr->pclass].name);
#endif
	t = temp;
	for (i = 0; i < 10; i++)
	{
		if (t[0] == 0) break; 
		fprintf(fff, "%s\n",t);
		t += strlen(t) + 1;
	}
	for (i = 1; i <= MAX_REALM; i++)
	{
		if (can_use_realm(i))
		{
			fprintf(fff, "\n");
			roff_to_buf(realm_jouhou[i - 1], 78, temp, sizeof temp);
#ifdef JP
			fprintf(fff, "魔法: %s\n", realm_names[i]);
#else
			fprintf(fff, "Realm: %s\n", realm_names[i]);
#endif
			t = temp;
			for (j = 0; j < 10; j++)
			{
				if (t[0] == 0) break;
				fprintf(fff, "%s\n",t);
				t += strlen(t) + 1;
			}
		}
	}

	if (p_ptr->special_blow)
	{
		special_blow_type *sb_ptr;

		for (i = 0; i < MAX_SB + MAX_TEMPLE_SB; i++)
		{
			if (p_ptr->special_blow & (0x00000001L << i))
			{
				if (i >= MAX_SB) sb_ptr = &temple_blow_info[i - MAX_SB];
				else sb_ptr = &special_blow_info[i];

				fprintf(fff, "\n");
				roff_to_buf(sb_ptr->text, 78, temp, sizeof temp);

				if (i >= MAX_SB)
#ifdef JP
					fprintf(fff, "必殺技: %s (テンプルナイトレベル: %2d, コスト: %2d)\n", sb_ptr->name, sb_ptr->level, sb_ptr->cost);
#else
					fprintf(fff, "Special Blow: %s (Level: %2d, Cost: %2d)\n", sb_ptr->name, sb_ptr->level, sb_ptr->cost);
#endif
				else
#ifdef JP
					fprintf(fff, "必殺技: %s (レベル: %2d, コスト: %2d)\n", sb_ptr->name, sb_ptr->level, sb_ptr->cost);
#else
					fprintf(fff, "Special Blow: %s (Level: %2d, Cost: %2d)\n", sb_ptr->name, sb_ptr->level, sb_ptr->cost);
#endif

				fprintf(fff, "対象武器:");
				for (j = 1; j <= MAX_WT; j++)
				{
					if (weapon_type_bit(j) & sb_ptr->weapon_type) fprintf(fff, " %s", weapon_skill_name[j]);
				}
				fprintf(fff, "\n");
				t = temp;
				for (j = 0; j < 10; j++)
				{
					if (t[0] == 0) break;
					fprintf(fff, "%s\n",t);
					t += strlen(t) + 1;
				}
			}
		}
	}

	if (p_ptr->resurrection_cnt)
	{
#ifdef JP
		fprintf(fff,"\n 魔法やアイテムなどによる蘇生回数: %d 回\n", p_ptr->resurrection_cnt);
#else
		fprintf(fff,"\n Count of self resurrection by magic, item, etc.: %d times\n", p_ptr->resurrection_cnt);
#endif
	}

	if (p_ptr->materialize_cnt)
	{
#ifdef JP
		fprintf(fff,"\nユニーク・モンスター蘇生回数: %d 回\n", p_ptr->materialize_cnt);
#else
		fprintf(fff,"\nUnique monster resurrection count: %d times\n", p_ptr->materialize_cnt);
#endif
	}

	if (p_ptr->reincarnate_cnt)
	{
#ifdef JP
#ifdef L64
		fprintf(fff,"\nリーンカーネイト回数: %d 回 (最高レベル: %d)\n", p_ptr->reincarnate_cnt, p_ptr->max_max_plv);
#else
		fprintf(fff,"\nリーンカーネイト回数: %d 回 (最高レベル: %ld)\n", p_ptr->reincarnate_cnt, p_ptr->max_max_plv);
#endif
#else
#ifdef L64
		fprintf(fff,"\nReincarnation count: %d times (Maximum level: %d)\n", p_ptr->reincarnate_cnt, p_ptr->max_max_plv);
#else
		fprintf(fff,"\nReincarnation count: %d times (Maximum level: %ld)\n", p_ptr->reincarnate_cnt, p_ptr->max_max_plv);
#endif
#endif
	}
}
