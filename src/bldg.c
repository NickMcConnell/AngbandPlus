/* File: bldg.c */

/*
 * Purpose: Building commands
 * Created by Ken Wigle for Kangband - a variant of Angband 2.8.3
 * -KMW-
 *
 * Rewritten for Kangband 2.8.3i using Kamband's version of
 * bldg.c as written by Ivan Tkatchev
 *
 * Changed for ZAngband by Robert Ruehlmann
 */

#include "angband.h"


void have_nightmare(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];
	char m_name[80];
	bool happened = FALSE;
	int power = r_ptr->level + 10;
	cptr desc = r_name + r_ptr->name;


	/* Describe it */
#ifndef JP
	if (!(r_ptr->flags1 & RF1_UNIQUE))
		sprintf(m_name, "%s %s", (is_a_vowel(desc[0]) ? "an" : "a"), desc);
	else
#endif
		sprintf(m_name, "%s", desc);

	if (!(r_ptr->flags1 & RF1_UNIQUE))
	{
		if (r_ptr->flags1 & RF1_FRIENDS) power /= 2;
	}
	else power *= 2;

	if (saving_throw(p_ptr->skill_sav * 100 / power))
	{
#ifdef JP
	msg_format("夢の中で%sに追いかけられた。", m_name);
#else
		msg_format("%^s chases you through your dreams.", m_name);
#endif


		/* Safe */
		return;
	}

	if (p_ptr->image)
	{
		/* Something silly happens... */
#ifdef JP
msg_format("%s%sの顔を見てしまった！",
#else
		msg_format("You behold the %s visage of %s!",
#endif

					  funny_desc[randint0(MAX_SAN_FUNNY)], m_name);

		if (one_in_(3))
		{
			msg_print(funny_comments[randint0(MAX_SAN_COMMENT)]);
			p_ptr->image = p_ptr->image + randint1(r_ptr->level);
		}

		/* Never mind; we can't see it clearly enough */
		return;
	}

	/* Something frightening happens... */
#ifdef JP
msg_format("%s%sの顔を見てしまった！",
#else
	msg_format("You behold the %s visage of %s!",
#endif

				  horror_desc[randint0(MAX_SAN_HORROR)], desc);

	r_ptr->r_flags2 |= RF2_ELDRITCH_HORROR;
#if 0
	switch(p_ptr->prace)
	{
		case RACE_VAMPIRE:
		{
			if (saving_throw(10 + p_ptr->lev)) return;

			break;
		}
	}
#endif
	/* Mind blast */
	if (!saving_throw(p_ptr->skill_sav * 100 / power))
	{
		if (!p_ptr->resist_conf)
		{
			(void)set_confused(p_ptr->confused + randint0(4) + 4);
		}
		if (!p_ptr->resist_chaos && one_in_(3))
		{
			(void)set_image(p_ptr->image + randint0(250) + 150);
		}
		return;
	}

	/* Lose int & wis */
	if (!saving_throw(p_ptr->skill_sav * 100 / power))
	{
		do_dec_stat(A_INT);
		do_dec_stat(A_WIS);
		return;
	}

	/* Brain smash */
	if (!saving_throw(p_ptr->skill_sav * 100 / power))
	{
		if (!p_ptr->resist_conf)
		{
			(void)set_confused(p_ptr->confused + randint0(4) + 4);
		}
		if (!p_ptr->free_act)
		{
			(void)set_paralyzed(p_ptr->paralyzed + randint0(4) + 4);
		}
		while (!saving_throw(p_ptr->skill_sav))
		{
			(void)do_dec_stat(A_INT);
		}
		while (!saving_throw(p_ptr->skill_sav))
		{
			(void)do_dec_stat(A_WIS);
		}
		if (!p_ptr->resist_chaos)
		{
			(void)set_image(p_ptr->image + randint0(250) + 150);
		}
		return;
	}

	/* Permanent lose int & wis */
	if (!saving_throw(p_ptr->skill_sav * 100 / power))
	{
		if (dec_stat(A_INT, 10, TRUE)) happened = TRUE;
		if (dec_stat(A_WIS, 10, TRUE)) happened = TRUE;
		if (happened)
		{
#ifdef JP
msg_print("以前より正気でなくなった気がする。");
#else
			msg_print("You feel much less sane than before.");
#endif

		}
		return;
	}

	/* Amnesia */
	if (!saving_throw(p_ptr->skill_sav * 100 / power))
	{
		if (lose_all_info())
		{
#ifdef JP
msg_print("あまりの恐怖に全てのことを忘れてしまった！");
#else
			msg_print("You forget everything in your utmost terror!");
#endif

		}
		return;
	}

	/* Else gain permanent insanity */
	if ((p_ptr->muta & MUT_MORONIC) && (p_ptr->muta & MUT_BERS_RAGE) &&
		((p_ptr->muta & MUT_COWARDICE) || (p_ptr->resist_fear)) &&
		((p_ptr->muta & MUT_HALLU) || (p_ptr->resist_chaos)))
	{
		/* The poor bastard already has all possible insanities! */
		return;
	}

	while (!happened)
	{
		switch (randint1(4))
		{
			case 1:
			{
				if (!(p_ptr->muta & MUT_MORONIC))
				{
#ifdef JP
					msg_print("あなたは完璧な馬鹿になった！");
#else
					msg_print("You turn into an utter moron!");
#endif
					if (p_ptr->muta & MUT_HYPER_INT)
					{
#ifdef JP
						msg_print("あなたの脳は生体コンピュータではなくなった。");
#else
						msg_print("Your brain is no longer a living computer.");
#endif
						p_ptr->muta &= ~(MUT_HYPER_INT);
					}
					p_ptr->muta |= MUT_MORONIC;
					happened = TRUE;
				}
				break;
			}
			case 2:
			{
				if (!(p_ptr->muta & MUT_COWARDICE) && !p_ptr->resist_fear)
				{
#ifdef JP
					msg_print("あなたはパラノイアになった！");
#else
					msg_print("You become paranoid!");
#endif

					/* Duh, the following should never happen, but anyway... */
					if (p_ptr->muta & MUT_FEARLESS)
					{
#ifdef JP
						msg_print("あなたはもう恐れ知らずではなくなった。");
#else
						msg_print("You are no longer fearless.");
#endif
						p_ptr->muta &= ~(MUT_FEARLESS);
					}

					p_ptr->muta |= MUT_COWARDICE;
					happened = TRUE;
				}
				break;
			}
			case 3:
			{
				if (!(p_ptr->muta & MUT_HALLU) && !p_ptr->resist_chaos)
				{
#ifdef JP
					msg_print("幻覚をひき起こす精神錯乱に陥った！");
#else
					msg_print("You are afflicted by a hallucinatory insanity!");
#endif
					p_ptr->muta |= MUT_HALLU;
					happened = TRUE;
				}
				break;
			}
			default:
			{
				if (!(p_ptr->muta & MUT_BERS_RAGE))
				{
#ifdef JP
					msg_print("激烈な感情の発作におそわれるようになった！");
#else
					msg_print("You become subject to fits of berserk rage!");
#endif
					p_ptr->muta |= MUT_BERS_RAGE;
					happened = TRUE;
				}
				break;
			}
		}
	}

	p_ptr->update |= PU_BONUS;
	handle_stuff();
}


bool get_nightmare(int r_idx)
{
	monster_race *r_ptr = &r_info[r_idx];

	/* Require eldritch horrors */
	if (!(r_ptr->flags2 & (RF2_ELDRITCH_HORROR))) return (FALSE);

	/* Require high level */
	if (r_ptr->level <= p_ptr->lev) return (FALSE);

	/* Accept this monster */
	return (TRUE);
}


/* Array of places to find an inscription */
static cptr find_quest[] =
{
#ifdef JP
"床にメッセージが刻まれている:",
#else
	"You find the following inscription in the floor",
#endif

#ifdef JP
"壁にメッセージが刻まれている:",
#else
	"You see a message inscribed in the wall",
#endif

#ifdef JP
"メッセージを見つけた:",
#else
	"There is a sign saying",
#endif

#ifdef JP
"何かが階段の上に書いてある:",
#else
	"Something is written on the staircase",
#endif

#ifdef JP
"巻物を見つけた。メッセージが書いてある:",
#else
	"You find a scroll with the following message",
#endif

};


/*
 * Discover quest
 */
void quest_discovery(int q_idx)
{
	quest_type      *q_ptr = &quest[q_idx];
	monster_race    *r_ptr = &r_info[q_ptr->r_idx];
	int             q_num = q_ptr->max_num;
	char            name[80];

	/* No quest index */
	if (!q_idx) return;

	strcpy(name, (r_name + r_ptr->name));

	msg_print(find_quest[rand_range(0, 4)]);
	msg_print(NULL);

	if (q_num == 1)
	{
		/* Unique */
#ifdef JP
msg_format("注意せよ！この階は%sによって守られている！", name);
#else
		msg_format("Beware, this level is protected by %s!", name);
#endif

	}
	else
	{
		/* Normal monsters */
#ifdef JP
msg_format("注意しろ！この階は%d体の%sによって守られている！", q_num, name);
#else
		plural_aux(name);
		msg_format("Be warned, this level is guarded by %d %s!", q_num, name);
#endif

	}
}


/*
 * Hack -- Check if a level is a "quest" level
 */
int quest_number(int level)
{
	int i;

	/* Check quests */
	if (p_ptr->inside_quest)
		return (p_ptr->inside_quest);

	for (i = 0; i < max_quests; i++)
	{
		if (quest[i].status != QUEST_STATUS_TAKEN) continue;

		if ((quest[i].type == QUEST_TYPE_KILL_LEVEL) &&
			!(quest[i].flags & QUEST_FLAG_PRESET) &&
		    (quest[i].level == level))
			return (i);
	}

	/* Check for random quest */
	return (random_quest_number(level));
}


/*
 * Return the index of the random quest on this level
 * (or zero)
 */
int random_quest_number(int level)
{
	int i;

	for (i = MIN_RANDOM_QUEST; i < MAX_RANDOM_QUEST + 1; i++)
	{
		if ((quest[i].type == QUEST_TYPE_RANDOM) &&
		    (quest[i].status == QUEST_STATUS_TAKEN) &&
		    (quest[i].level == level))
		{
			return i;
		}
	}

	/* Nope */
	return 0;
}


/*
 * Check Failed Random Quest
 */
void check_failed_random_quest(void)
{
	int i;

	for (i = MIN_RANDOM_QUEST; i < MAX_RANDOM_QUEST + 1; i++)
	{
		quest_type *q_ptr = &quest[i];

		if (q_ptr->status == QUEST_STATUS_FAILED)
		{
			/* Not Quest Monster any more */
			r_info[q_ptr->r_idx].flags1 &= ~(RF1_QUESTOR);
			q_ptr->status = QUEST_STATUS_FAILED_DONE;
		}
	}
}


/* hack as in leave_store in store.c */
static bool leave_bldg = FALSE;

static bool reinit_wilderness = FALSE;

static bool is_owner(building_type *bldg)
{
	if (bldg->member_class[p_ptr->pclass] == BUILDING_OWNER)
	{
		return (TRUE);
	}

	if (bldg->member_race[p_ptr->prace] == BUILDING_OWNER)
	{
		return (TRUE);
	}

	if ((bldg->member_realm[p_ptr->realm1] == BUILDING_OWNER) ||
		(bldg->member_realm[p_ptr->realm2] == BUILDING_OWNER))
	{
		return (TRUE);
	}

	return (FALSE);
}


static bool is_member(building_type *bldg)
{
	if (bldg->member_class[p_ptr->pclass])
	{
		return (TRUE);
	}

	if (bldg->member_race[p_ptr->prace])
	{
		return (TRUE);
	}

	if ((bldg->member_realm[p_ptr->realm1]) || (bldg->member_realm[p_ptr->realm2]))
	{
		return (TRUE);
	}

	return (FALSE);
}


/*
 * Clear the building information
 */
static void clear_bldg(int min_row, int max_row)
{
	int   i;

	for (i = min_row; i <= max_row; i++)
		prt("", i, 0);
}

static void building_prt_gold(void)
{
	char tmp_str[MAX_NLEN];

#ifdef JP
	prt("手持ちのお金: ", 23,53);
#else
	prt("Gold Remaining: ", 23, 53);
#endif
	sprintf(tmp_str, "%9ld", (long)p_ptr->au);
	prt(tmp_str, 23, 68);
}


/*
 * Display a building.
 */
static void show_building(building_type* bldg)
{
	char buff[20];
	int i;
	byte action_color;
	char tmp_str[MAX_NLEN];

	Term_clear();

	sprintf(tmp_str, "%s", bldg->owner_name);
	put_str(tmp_str, 2, 10);

	sprintf(tmp_str, "%s", bldg->name);
	prt(tmp_str, 2, 50);

#ifdef JP
	prt("コマンド:", 19, 0);
#else
	prt("You may:", 19, 0);
#endif

	for (i = 0; i < 6; i++)
	{
		if (bldg->letters[i])
		{
			if (bldg->action_restr[i] == 0)
			{
				if ((is_owner(bldg) && (bldg->member_costs[i] == 0)) ||
					(!is_owner(bldg) && (bldg->other_costs[i] == 0)))
				{
					action_color = TERM_WHITE;
					buff[0] = '\0';
				}
				else if (is_owner(bldg))
				{
					action_color = TERM_YELLOW;
#ifdef JP
					sprintf(buff, "($%d)", bldg->member_costs[i]);
#else
					sprintf(buff, "(%dgp)", bldg->member_costs[i]);
#endif
				}
				else
				{
					action_color = TERM_YELLOW;
#ifdef JP
					sprintf(buff, "($%d)", bldg->other_costs[i]);
#else
					sprintf(buff, "(%dgp)", bldg->other_costs[i]);
#endif
				}
			}
			else if (bldg->action_restr[i] == 1)
			{
				if (!is_member(bldg))
				{
					action_color = TERM_L_DARK;
#ifdef JP
					strcpy(buff, "(閉店)");
#else
					strcpy(buff, "(closed)");
#endif
				}
				else if ((is_owner(bldg) && (bldg->member_costs[i] == 0)) ||
					(is_member(bldg) && (bldg->other_costs[i] == 0)))
				{
					action_color = TERM_WHITE;
					buff[0] = '\0';
				}
				else if (is_owner(bldg))
				{
					action_color = TERM_YELLOW;
#ifdef JP
					sprintf(buff, "($%d)", bldg->member_costs[i]);
#else
					sprintf(buff, "(%dgp)", bldg->member_costs[i]);
#endif
				}
				else
				{
					action_color = TERM_YELLOW;
#ifdef JP
					sprintf(buff, "($%d)", bldg->other_costs[i]);
#else
					sprintf(buff, "(%dgp)", bldg->other_costs[i]);
#endif
				}
			}
			else
			{
				if (!is_owner(bldg))
				{
					action_color = TERM_L_DARK;
#ifdef JP
					strcpy(buff, "(閉店)");
#else
					strcpy(buff, "(closed)");
#endif
				}
				else if (bldg->member_costs[i] != 0)
				{
					action_color = TERM_YELLOW;
#ifdef JP
					sprintf(buff, "($%d)", bldg->member_costs[i]);
#else
					sprintf(buff, "(%dgp)", bldg->member_costs[i]);
#endif
				}
				else
				{
					action_color = TERM_WHITE;
					buff[0] = '\0';
				}
			}

			sprintf(tmp_str," %c) %s %s", bldg->letters[i], bldg->act_names[i], buff);
			c_put_str(action_color, tmp_str, 19+(i/2), 35*(i%2));
		}
	}

#ifdef JP
	prt(" ESC) 建物を出る", 23, 0);
#else
	prt(" ESC) Exit building", 23, 0);
#endif
}


/* reset timed flags */
static void reset_tim_flags(void)
{
	p_ptr->fast = 0;            /* Timed -- Fast */
	p_ptr->slow = 0;            /* Timed -- Slow */
	p_ptr->blind = 0;           /* Timed -- Blindness */
	p_ptr->paralyzed = 0;       /* Timed -- Paralysis */
	p_ptr->confused = 0;        /* Timed -- Confusion */
	p_ptr->afraid = 0;          /* Timed -- Fear */
	p_ptr->image = 0;           /* Timed -- Hallucination */
	p_ptr->poisoned = 0;        /* Timed -- Poisoned */
	p_ptr->cut = 0;             /* Timed -- Cut */
	p_ptr->stun = 0;            /* Timed -- Stun */

	p_ptr->protevil = 0;        /* Timed -- Protection */
	p_ptr->invuln = 0;          /* Timed -- Invulnerable */
	p_ptr->hero = 0;            /* Timed -- Heroism */
	p_ptr->shero = 0;           /* Timed -- Super Heroism */
	p_ptr->shield = 0;          /* Timed -- Shield Spell */
	p_ptr->blessed = 0;         /* Timed -- Blessed */
	p_ptr->tim_invis = 0;       /* Timed -- Invisibility */
	p_ptr->tim_infra = 0;       /* Timed -- Infra Vision */
	p_ptr->tim_radar = 0;		/* Timed -- Radar Eye */

	p_ptr->oppose_acid = 0;     /* Timed -- oppose acid */
	p_ptr->oppose_elec = 0;     /* Timed -- oppose lightning */
	p_ptr->oppose_fire = 0;     /* Timed -- oppose heat */
	p_ptr->oppose_cold = 0;     /* Timed -- oppose cold */
	p_ptr->oppose_pois = 0;     /* Timed -- oppose poison */

	p_ptr->confusing = 0;       /* Touch of Confusion */
}


/*
 * arena commands
 */
static void arena_comm(int cmd)
{
	monster_race    *r_ptr;
	cptr            name;


	switch (cmd)
	{
		case BACT_ARENA:
			if (p_ptr->arena_number == MAX_ARENA_MONS)
			{
				clear_bldg(5, 19);
#ifdef JP
prt("アリーナの優勝者！", 5, 0);
prt("おめでとう！あなたは全ての敵を倒しました。", 7, 0); 
prt("賞金として $10,000 が与えられます。", 8, 0);
#else
				prt("               Arena Victor!", 5, 0);
				prt("Congratulations!  You have defeated all before you.", 7, 0);
				prt("For that, receive the prize: 10,000 gold pieces", 8, 0);
#endif

				prt("", 10, 0);
				prt("", 11, 0);
				p_ptr->au += 10000;
#ifdef JP
msg_print("スペースキーで続行");
#else
				msg_print("Press the space bar to continue");
#endif

				msg_print(NULL);
				p_ptr->arena_number++;
			}
			else if (p_ptr->arena_number > MAX_ARENA_MONS)
			{
#ifdef JP
msg_print("あなたはアリーナに入り、しばらくの間栄光にひたった。");
#else
				msg_print("You enter the arena briefly and bask in your glory.");
#endif

				msg_print(NULL);
			}
			else
			{
				p_ptr->inside_arena = TRUE;
				p_ptr->exit_bldg = FALSE;
				reset_tim_flags();
				p_ptr->leaving = TRUE;
				leave_bldg = TRUE;
			}
			break;
		case BACT_POSTER:
			if (p_ptr->arena_number == MAX_ARENA_MONS)
#ifdef JP
msg_print("あなたは勝利者だ。 アリーナでのセレモニーに参加しなさい。");
#else
				msg_print("You are victorious. Enter the arena for the ceremony.");
#endif

			else if (p_ptr->arena_number > MAX_ARENA_MONS)
#ifdef JP
msg_print("あなたはすべての敵に勝利した。");
#else
				msg_print("You have won against all foes.");
#endif

			else
			{
				r_ptr = &r_info[arena_monsters[p_ptr->arena_number]];
				name = (r_name + r_ptr->name);
#ifdef JP
msg_format("%s に挑戦するものはいないか？", name);
#else
				msg_format("Do I hear any challenges against: %s", name);
#endif

				msg_print(NULL);
			}
			break;
		case BACT_ARENA_RULES:

			/* Save screen */
			screen_save();

			/* Peruse the arena help file */
#ifdef JP
(void)show_file("arena_j.txt", NULL, 0, 0);
#else
			(void)show_file("arena.txt", NULL, 0, 0);
#endif


			/* Load screen */
			screen_load();

			break;
	}
}


/*
 * display fruit for dice slots
 */
static void display_fruit(int row, int col, int fruit)
{
	switch (fruit)
	{
		case 0: /* lemon */
#ifdef JP
			c_put_str(TERM_YELLOW, "   ####.", row, col);
			c_put_str(TERM_YELLOW, "  #    #", row + 1, col);
			c_put_str(TERM_YELLOW, " #     #", row + 2, col);
			c_put_str(TERM_YELLOW, "#      #", row + 3, col);
			c_put_str(TERM_YELLOW, "#      #", row + 4, col);
			c_put_str(TERM_YELLOW, "#     # ", row + 5, col);
			c_put_str(TERM_YELLOW, "#    #  ", row + 6, col);
			c_put_str(TERM_YELLOW, ".####   ", row + 7, col);
			prt(                   " レモン ", row + 8, col);
#else
			c_put_str(TERM_YELLOW, "   ####.", row, col);
			c_put_str(TERM_YELLOW, "  #    #", row + 1, col);
			c_put_str(TERM_YELLOW, " #     #", row + 2, col);
			c_put_str(TERM_YELLOW, "#      #", row + 3, col);
			c_put_str(TERM_YELLOW, "#      #", row + 4, col);
			c_put_str(TERM_YELLOW, "#     # ", row + 5, col);
			c_put_str(TERM_YELLOW, "#    #  ", row + 6, col);
			c_put_str(TERM_YELLOW, ".####   ", row + 7, col);
			prt(                   " Lemon  ", row + 8, col);
#endif

			break;
		case 1: /* orange */
#ifdef JP
			c_put_str(TERM_ORANGE, "   ##   ", row, col);
			c_put_str(TERM_ORANGE, "  #..#  ", row + 1, col);
			c_put_str(TERM_ORANGE, " #....# ", row + 2, col);
			c_put_str(TERM_ORANGE, "#......#", row + 3, col);
			c_put_str(TERM_ORANGE, "#......#", row + 4, col);
			c_put_str(TERM_ORANGE, " #....# ", row + 5, col);
			c_put_str(TERM_ORANGE, "  #..#  ", row + 6, col);
			c_put_str(TERM_ORANGE, "   ##   ", row + 7, col);
			prt(                   "オレンジ", row + 8, col);
#else
			c_put_str(TERM_ORANGE, "   ##   ", row, col);
			c_put_str(TERM_ORANGE, "  #..#  ", row + 1, col);
			c_put_str(TERM_ORANGE, " #....# ", row + 2, col);
			c_put_str(TERM_ORANGE, "#......#", row + 3, col);
			c_put_str(TERM_ORANGE, "#......#", row + 4, col);
			c_put_str(TERM_ORANGE, " #....# ", row + 5, col);
			c_put_str(TERM_ORANGE, "  #..#  ", row + 6, col);
			c_put_str(TERM_ORANGE, "   ##   ", row + 7, col);
			prt(                   " Orange ", row + 8, col);
#endif

			break;
		case 2: /* sword */
#ifdef JP
			c_put_str(TERM_SLATE, "   Λ   " , row, col);
			c_put_str(TERM_SLATE, "   ||   " , row + 1, col);
			c_put_str(TERM_SLATE, "   ||   " , row + 2, col);
			c_put_str(TERM_SLATE, "   ||   " , row + 3, col);
			c_put_str(TERM_SLATE, "   ||   " , row + 4, col);
			c_put_str(TERM_SLATE, "   ||   " , row + 5, col);
			c_put_str(TERM_UMBER, " |=亜=| " , row + 6, col);
			c_put_str(TERM_UMBER, "   目   " , row + 7, col);
			prt(                  " 剣     " , row + 8, col);
#else
			c_put_str(TERM_SLATE, "   /\\  " , row, col);
			c_put_str(TERM_SLATE, "   ##   " , row + 1, col);
			c_put_str(TERM_SLATE, "   ##   " , row + 2, col);
			c_put_str(TERM_SLATE, "   ##   " , row + 3, col);
			c_put_str(TERM_SLATE, "   ##   " , row + 4, col);
			c_put_str(TERM_SLATE, "   ##   " , row + 5, col);
			c_put_str(TERM_UMBER, " ###### " , row + 6, col);
			c_put_str(TERM_UMBER, "   ##   " , row + 7, col);
			prt(                  " Sword  " , row + 8, col);
#endif

			break;
		case 3: /* shield */
#ifdef JP
			c_put_str(TERM_SLATE, " ###### ", row, col);
			c_put_str(TERM_SLATE, "#      #", row + 1, col);
			c_put_str(TERM_SLATE, "# ++++ #", row + 2, col);
			c_put_str(TERM_SLATE, "# +==+ #", row + 3, col);
			c_put_str(TERM_SLATE, "#  ++  #", row + 4, col);
			c_put_str(TERM_SLATE, " #    # ", row + 5, col);
			c_put_str(TERM_SLATE, "  #  #  ", row + 6, col);
			c_put_str(TERM_SLATE, "   ##   ", row + 7, col);
			prt(                  "  盾    ", row + 8, col);
#else
			c_put_str(TERM_SLATE, " ###### ", row, col);
			c_put_str(TERM_SLATE, "#      #", row + 1, col);
			c_put_str(TERM_SLATE, "# ++++ #", row + 2, col);
			c_put_str(TERM_SLATE, "# +==+ #", row + 3, col);
			c_put_str(TERM_SLATE, "#  ++  #", row + 4, col);
			c_put_str(TERM_SLATE, " #    # ", row + 5, col);
			c_put_str(TERM_SLATE, "  #  #  ", row + 6, col);
			c_put_str(TERM_SLATE, "   ##   ", row + 7, col);
			prt(                  " Shield ", row + 8, col);
#endif

			break;
		case 4: /* plum */
#ifdef JP
			c_put_str(TERM_VIOLET, "   ##   ", row, col);
			c_put_str(TERM_VIOLET, " ###### ", row + 1, col);
			c_put_str(TERM_VIOLET, "########", row + 2, col);
			c_put_str(TERM_VIOLET, "########", row + 3, col);
			c_put_str(TERM_VIOLET, "########", row + 4, col);
			c_put_str(TERM_VIOLET, " ###### ", row + 5, col);
			c_put_str(TERM_VIOLET, "  ####  ", row + 6, col);
			c_put_str(TERM_VIOLET, "   ##   ", row + 7, col);
			prt(                   " プラム ", row + 8, col);
#else
			c_put_str(TERM_VIOLET, "   ##   ", row, col);
			c_put_str(TERM_VIOLET, " ###### ", row + 1, col);
			c_put_str(TERM_VIOLET, "########", row + 2, col);
			c_put_str(TERM_VIOLET, "########", row + 3, col);
			c_put_str(TERM_VIOLET, "########", row + 4, col);
			c_put_str(TERM_VIOLET, " ###### ", row + 5, col);
			c_put_str(TERM_VIOLET, "  ####  ", row + 6, col);
			c_put_str(TERM_VIOLET, "   ##   ", row + 7, col);
			prt(                   "  Plum  ", row + 8, col);
#endif

			break;
		case 5: /* cherry */
#ifdef JP
			c_put_str(TERM_RED, "      ##", row, col);
			c_put_str(TERM_RED, "   ###  ", row + 1, col);
			c_put_str(TERM_RED, "  #..#  ", row + 2, col);
			c_put_str(TERM_RED, "  #..#  ", row + 3, col);
			c_put_str(TERM_RED, " ###### ", row + 4, col);
			c_put_str(TERM_RED, "#..##..#", row + 5, col);
			c_put_str(TERM_RED, "#..##..#", row + 6, col);
			c_put_str(TERM_RED, " ##  ## ", row + 7, col);
			prt(                "チェリー", row + 8, col);
#else
			c_put_str(TERM_RED, "      ##", row, col);
			c_put_str(TERM_RED, "   ###  ", row + 1, col);
			c_put_str(TERM_RED, "  #..#  ", row + 2, col);
			c_put_str(TERM_RED, "  #..#  ", row + 3, col);
			c_put_str(TERM_RED, " ###### ", row + 4, col);
			c_put_str(TERM_RED, "#..##..#", row + 5, col);
			c_put_str(TERM_RED, "#..##..#", row + 6, col);
			c_put_str(TERM_RED, " ##  ## ", row + 7, col);
			prt(                " Cherry ", row + 8, col);
#endif

			break;
	}
}


/*
 * gamble_comm
 */
static bool gamble_comm(int cmd)
{
	int roll1, roll2, roll3, choice, odds, win;
	s32b wager;
	s32b maxbet;
	s32b oldgold;
#ifdef JP
	static const char *fruit[6] = {"レモン", "オレンジ", "剣", "盾", "プラム", "チェリー"};
#else
	static const char *fruit[6] = {"Lemon", "Orange", "Sword", "Shield", "Plum", "Cherry"};
#endif

	char out_val[MAX_NLEN], tmp_str[MAX_NLEN], again;
	cptr p;

	screen_save();

	if (cmd == BACT_GAMBLE_RULES)
	{
		/* Peruse the gambling help file */
#ifdef JP
		(void)show_file("jgambling.txt", NULL, 0, 0);
#else
		(void)show_file("gambling.txt", NULL, 0, 0);
#endif
	}
	else
	{
		/* No money */
		if (p_ptr->au < 1)
		{
#ifdef JP
			msg_print("おい！おまえ一文なしじゃないか！こっから出ていけ！");
#else
			msg_print("Hey! You don't have gold - get out of here!");
#endif
			msg_print(NULL);
			screen_load();
			return FALSE;
		}

		clear_bldg(5, 23);

		/* Set maximum bet */
		if (p_ptr->lev < 10)
			maxbet = p_ptr->lev * 100;
		else
			maxbet = p_ptr->lev * 1000;

		/* We can't bet more than we have */
		maxbet = MIN(maxbet, p_ptr->au);

		/* Get the wager */
		strcpy(out_val, "");
#ifdef JP
		sprintf(tmp_str,"賭け金 (1-%ld)？", maxbet);
#else
		sprintf(tmp_str,"Your wager (1-%ld) ? ", maxbet);
#endif

		/*
		 * Use get_string() because we may need more than
		 * the s16b value returned by get_quantity().
		 */
		if (get_string(tmp_str, out_val, 32))
		{
			/* Strip spaces */
			for (p = out_val; *p == ' '; p++);

			/* Get the wager */
			wager = atol(p);

			if (wager > p_ptr->au)
			{
#ifdef JP
				msg_print("おい！金が足りないじゃないか！出ていけ！");
#else
				msg_print("Hey! You don't have the gold - get out of here!");
#endif
				msg_print(NULL);
				screen_load();
				return (FALSE);
			}
			else if (wager > maxbet)
			{
#ifdef JP
				msg_format("%ldゴールドだけ受けよう。残りは取っときな。", maxbet);
#else
				msg_format("I'll take %ld gold of that. Keep the rest.", maxbet);
#endif
				wager = maxbet;
			}
			else if (wager < 1)
			{
#ifdef JP
				msg_print("ＯＫ、１ゴールドからはじめよう。");
#else
				msg_print("Ok, we'll start with 1 gold.");
#endif
				wager = 1;
			}
			msg_print(NULL);
			win = FALSE;
			odds = 0;
			oldgold = p_ptr->au;

#ifdef JP
			sprintf(tmp_str, "ゲーム前の所持金: %9ld", oldgold);
#else
			sprintf(tmp_str, "Gold before game: %9ld", oldgold);
#endif
			prt(tmp_str, 20, 2);

#ifdef JP
			sprintf(tmp_str, "現在の掛け金:     %9ld", wager);
#else
			sprintf(tmp_str, "Current Wager:    %9ld", wager);
#endif
			prt(tmp_str, 21, 2);

			/* Prevent savefile-scumming of the casino */
			Rand_quick = TRUE;
			Rand_value = time(NULL);

			do
			{
				switch (cmd)
				{
				 case BACT_IN_BETWEEN: /* Game of In-Between */
#ifdef JP
					c_put_str(TERM_GREEN, "イン・ビトイーン",5,2);
#else
					c_put_str(TERM_GREEN, "In Between", 5, 2);
#endif
					odds = 3;
					win = FALSE;
					roll1 = randint1(10);
					roll2 = randint1(10);
					choice = randint1(10);
#ifdef JP
					sprintf(tmp_str, "黒ダイス: %d        黒ダイス: %d", roll1, roll2);
#else
					sprintf(tmp_str, "Black die: %d       Black Die: %d", roll1, roll2);
#endif
					prt(tmp_str, 8, 3);
#ifdef JP
					sprintf(tmp_str, "赤ダイス: %d", choice);
#else
					sprintf(tmp_str, "Red die: %d", choice);
#endif
					prt(tmp_str, 11, 14);
					if (((choice > roll1) && (choice < roll2)) ||
						((choice < roll1) && (choice > roll2)))
						win = TRUE;
					break;
				case BACT_CRAPS:  /* Game of Craps */
#ifdef JP
					c_put_str(TERM_GREEN, "クラップス", 5, 2);
#else
					c_put_str(TERM_GREEN, "Craps", 5, 2);
#endif
					win = 3;
					odds = 1;
					roll1 = randint1(6);
					roll2 = randint1(6);
					roll3 = roll1 + roll2;
					choice = roll3;
#ifdef JP
					sprintf(tmp_str, "１振りめ: %d %d      Total: %d", roll1, 
#else
					sprintf(tmp_str, "First roll: %d %d    Total: %d", roll1,
#endif
						 roll2, roll3);
					prt(tmp_str, 7, 5);
					if ((roll3 == 7) || (roll3 == 11))
						win = TRUE;
					else if ((roll3 == 2) || (roll3 == 3) || (roll3 == 12))
						win = FALSE;
					else
						do
						{
#ifdef JP
							msg_print("なにかキーを押すともう一回振ります。");
#else
							msg_print("Hit any key to roll again");
#endif
							msg_print(NULL);
							roll1 = randint1(6);
							roll2 = randint1(6);
							roll3 = roll1 + roll2;

#ifdef JP
							sprintf(tmp_str, "出目: %d %d          合計:      %d",
#else
							sprintf(tmp_str, "Roll result: %d %d   Total:     %d",
#endif
								 roll1, roll2, roll3);
							prt(tmp_str, 8, 5);
							if (roll3 == choice)
								win = TRUE;
							else if (roll3 == 7)
								win = FALSE;
						} while ((win != TRUE) && (win != FALSE));
					break;

				case BACT_SPIN_WHEEL:  /* Spin the Wheel Game */
					win = FALSE;
					odds = 8;
#ifdef JP
					c_put_str(TERM_GREEN, "ルーレット", 5, 2);
#else
					c_put_str(TERM_GREEN, "Wheel", 5, 2);
#endif
					prt("0  1  2  3  4  5  6  7  8  9", 7, 5);
					prt("--------------------------------", 8, 3);
					strcpy(out_val, "");
#ifdef JP
					get_string("何番？ (0-9): ", out_val, 32);
#else
					get_string("Pick a number (0-9): ", out_val, 32);
#endif
					for (p = out_val; *p == ' '; p++);
					choice = atol(p);
					if (choice < 0)
					{
#ifdef JP
						msg_print("0番にしとくぜ。");
#else
						msg_print("I'll put you down for 0.");
#endif
						choice = 0;
					}
					else if (choice > 9)
					{
#ifdef JP
						msg_print("ＯＫ、9番にしとくぜ。");
#else
						msg_print("Ok, I'll put you down for 9.");
#endif
						choice = 9;
					}
					msg_print(NULL);
					roll1 = randint0(10);
#ifdef JP
					sprintf(tmp_str, "ルーレットは回り、止まった。勝者は %d番だ。",
#else
					sprintf(tmp_str, "The wheel spins to a stop and the winner is %d",
#endif
						roll1);
					prt(tmp_str, 13, 3);
					prt("", 9, 0);
					prt("*", 9, (3 * roll1 + 5));
					if (roll1 == choice)
						win = TRUE;
					break;

				case BACT_DICE_SLOTS: /* The Dice Slots */
#ifdef JP
					c_put_str(TERM_GREEN, "ダイス・スロット", 5, 2);
#else
					c_put_str(TERM_GREEN, "Dice Slots", 5, 2);
#endif
					win = FALSE;
					roll1 = randint1(6);
					roll2 = randint1(6);
					choice = randint1(6);
					(void)sprintf(tmp_str, "%s %s %s", fruit[roll1 - 1], fruit[roll2 - 1],
						 fruit[choice - 1]);
					prt(tmp_str, 15, 37);
					prt("/--------------------------\\", 7, 2);
					prt("\\--------------------------/", 17, 2);
					display_fruit(8,  3, roll1 - 1);
					display_fruit(8, 12, roll2 - 1);
					display_fruit(8, 21, choice - 1);
					if ((roll1 == roll2) && (roll2 == choice))
					{
						win = TRUE;
						if (roll1 == 1)
							odds = 4;
						else if (roll1 == 2)
							odds = 6;
						else
							odds = roll1 * roll1;
					}
					else if ((roll1 == 6) && (roll2 == 6))
					{
						win = TRUE;
						odds = choice + 1;
					}
					break;
				}

				if (win)
				{
#ifdef JP
					prt("あなたの勝ち", 16, 37);
#else
					prt("YOU WON", 16, 37);
#endif
					p_ptr->au += odds * wager;
#ifdef JP
					sprintf(tmp_str, "倍率: %d", odds);
#else
					sprintf(tmp_str, "Payoff: %d", odds);
#endif
					prt(tmp_str, 17, 37);
				}
				else
				{
#ifdef JP
					prt("あなたの負け", 16, 37);
#else
					prt("You Lost", 16, 37);
#endif
					p_ptr->au -= wager;
					prt("", 17, 37);
				}
#ifdef JP
				sprintf(tmp_str, "現在の所持金:    %9ld", p_ptr->au);
#else
				sprintf(tmp_str, "Current Gold:     %9ld", p_ptr->au);
#endif
				prt(tmp_str, 22, 2);
#ifdef JP
				prt("もう一度(Y/N)？", 18, 37);
				move_cursor(18, 54);
#else
				prt("Again(Y/N)?", 18, 37);
				move_cursor(18, 49);
#endif

				again = inkey();
				if (wager > p_ptr->au)
				{
#ifdef JP
					msg_print("おい！金が足りないじゃないか！ここから出て行け！");
#else
					msg_print("Hey! You don't have the gold - get out of here!");
#endif
					msg_print(NULL);

					/* Get out here */
					break;
				}
			} while ((again == 'y') || (again == 'Y'));

			/* Switch back to complex RNG */
			Rand_quick = FALSE;

			prt("", 18, 37);
			if (p_ptr->au >= oldgold)
#ifdef JP
				msg_print("「今回は儲けたな！でも次はこっちが勝ってやるからな、絶対に！」");
#else
				msg_print("You came out a winner! We'll win next time, I'm sure.");
#endif
			else
#ifdef JP
				msg_print("「金をスッてしまったな、わはは！うちに帰った方がいいぜ。」");
#else
				msg_print("You lost gold! Haha, better head home.");
#endif
		}
		msg_print(NULL);
	}
	screen_load();
	return (TRUE);
}


/*
 * inn commands
 * Note that resting for the night was a perfect way to avoid player
 * ghosts in the town *if* you could only make it to the inn in time (-:
 * Now that the ghosts are temporarily disabled in 2.8.X, this function
 * will not be that useful.  I will keep it in the hopes the player
 * ghost code does become a reality again. Does help to avoid filthy urchins.
 * Resting at night is also a quick way to restock stores -KMW-
 */
static bool inn_comm(int cmd)
{
	switch (cmd)
	{
		case BACT_FOOD: /* Buy food & drink */
			if (p_ptr->food >= PY_FOOD_FULL)
			{
#ifdef JP
				msg_print("今は満腹だ。");
#else
				msg_print("You are full now.");
#endif
				return FALSE;
			}

#ifdef JP
			msg_print("バーテンはいくらかの食べ物とビールをくれた。");
#else
			msg_print("The barkeep gives you some gruel and a beer.");
#endif

			msg_print(NULL);
			(void)set_food(PY_FOOD_MAX - 1);
			break;

		case BACT_REST: /* Rest for the night */
			if ((p_ptr->poisoned) || (p_ptr->cut))
			{
#ifdef JP
				msg_print("あなたに必要なのは部屋ではなく、治療者です。");
#else
				msg_print("You need a healer, not a room.");
#endif

				msg_print(NULL);
#ifdef JP
				msg_print("すみません、でもうちで誰かに死なれちゃ困りますんで。");
#else
				msg_print("Sorry, but don't want anyone dying in here.");
#endif

				return (FALSE);
			}
			else
			{
				int prev_day, prev_hour, prev_min;

				extract_day_hour_min(&prev_day, &prev_hour, &prev_min);

				turn = ((turn / (10L * TOWN_DAWN / 2)) + 1) * (10L * TOWN_DAWN / 2);
				p_ptr->chp = p_ptr->mhp;

				if (take_notes)
				{
					if ((prev_hour >= 18) && (prev_hour <= 23)) add_note(" ", 'D');
				}

				if (ironman_nightmare)
				{
#ifdef JP
					msg_print("眠りに就くと恐ろしい光景が心をよぎった。");
#else
					msg_print("Horrible visions flit through your mind as you sleep.");
#endif

					/* Pick a nightmare */
					get_mon_num_prep(get_nightmare, NULL);

					/* Have some nightmares */
					while (1)
					{
						have_nightmare(get_mon_num(MAX_DEPTH));

						if (!one_in_(3)) break;
					}

					/* Remove the monster restriction */
					get_mon_num_prep(NULL, NULL);

#ifdef JP
					msg_print("あなたは絶叫して目を覚ました。");
#else
					msg_print("You awake screaming.");
#endif
				}
				else
				{
					set_blind(0);
					set_confused(0);
					p_ptr->stun = 0;
					p_ptr->chp = p_ptr->mhp;
					p_ptr->csp = p_ptr->msp;

					if ((prev_hour >= 6) && (prev_hour <= 17))
					{
#ifdef JP
						msg_print("あなたはリフレッシュして目覚め、夕方を迎えた。");
#else
						msg_print("You awake refreshed for the evening.");
#endif
					}
					else
					{
#ifdef JP
						msg_print("あなたはリフレッシュして目覚め、新たな日を迎えた。");
#else
						msg_print("You awake refreshed for the new day.");
#endif
					}
				}

				msg_print(NULL);
			}
			break;

		case BACT_RUMORS: /* Listen for rumors */
			{
				char Rumor[1024];

#ifdef JP
				if (!get_rnd_line_jonly("rumors_j.txt", 0, Rumor, 10))
#else
				if (!get_rnd_line("rumors.txt", 0, Rumor))
#endif

					msg_format("%s", Rumor);
				msg_print(NULL);
				break;
			}
	}

	return (TRUE);
}


/*
 * Share gold for thieves
 */
static void share_gold(void)
{
	int i = (p_ptr->lev * 2) * 10;
#ifdef JP
msg_format("＄%d を手に入れた。", i);
#else
	msg_format("You collect %d gold pieces", i);
#endif

	msg_print(NULL);
	p_ptr->au += i;
}


/*
 * Display quest information
 */
static void get_questinfo(int questnum)
{
	int     i;
	int     old_quest;
	char    tmp_str[MAX_NLEN];


	/* Clear the text */
	for (i = 0; i < 10; i++)
	{
		quest_text[i][0] = '\0';
	}

	quest_text_line = 0;

	/* Set the quest number temporary */
	old_quest = p_ptr->inside_quest;
	p_ptr->inside_quest = questnum;

	/* Get the quest text */
	init_flags = INIT_SHOW_TEXT | INIT_ASSIGN;
	process_dungeon_file("q_info.txt", 0, 0, 0, 0);

	/* Reset the old quest number */
	p_ptr->inside_quest = old_quest;

	/* Print the quest info */
#ifdef JP
sprintf(tmp_str, "クエスト情報 (危険度: %d 階相当)", quest[questnum].level);
#else
	sprintf(tmp_str, "Quest Information (Danger level: %d)", quest[questnum].level);
#endif

	prt(tmp_str, 5, 0);

	prt(quest[questnum].name, 7, 0);

	for (i = 0; i < 10; i++)
	{
		c_put_str(TERM_YELLOW, quest_text[i], i + 8, 0);
	}
}


/*
 * Request a quest from the Lord.
 */
static void castle_quest(void)
{
	quest_type      *q_ptr;
#if 0
	monster_race    *r_ptr;
	cptr            name;
#endif

	/* Current quest of the building */
	int             q_index = cave[py][px].special;


	clear_bldg(7, 18);

	/* Is there a quest available at the building? */
	if (!q_index)
	{
#ifdef JP
		put_str("今のところクエストはありません。", 8, 0);
#else
		put_str("I don't have a quest for you at the moment.", 8, 0);
#endif
		return;
	}

	q_ptr = &quest[q_index];

	/* Quest is completed */
	if (q_ptr->status == QUEST_STATUS_COMPLETED)
	{
		/* Rewarded quest */
		q_ptr->status = QUEST_STATUS_REWARDED;

		get_questinfo(q_index);

		reinit_wilderness = TRUE;
	}
	/* Failed quest */
	else if (q_ptr->status == QUEST_STATUS_FAILED)
	{
		get_questinfo(q_index);

		/* Mark quest as done (but failed) */
		q_ptr->status = QUEST_STATUS_FAILED_DONE;

		reinit_wilderness = TRUE;
	}
	/* Quest is still unfinished */
	else if (q_ptr->status == QUEST_STATUS_TAKEN)
	{
#ifdef JP
		put_str("あなたは現在のクエストを終了させていません！", 8, 0);
		put_str("CTRL-Qを使えばクエストの状態がチェックできます。", 9, 0);
		put_str("クエストを終わらせたら戻って来て下さい。", 12, 0);
#else
		put_str("You have not completed your current quest yet!", 8, 0);
		put_str("Use CTRL-Q to check the status of your quest.", 9, 0);
		put_str("Return when you have completed your quest.", 12, 0);
#endif
	}
	/* No quest yet */
	else if (q_ptr->status == QUEST_STATUS_UNTAKEN)
	{
		/* Assign a new quest */
#if 0
		if (q_ptr->type == QUEST_TYPE_KILL_ANY_LEVEL)
		{
			if (q_ptr->r_idx == 0)
			{
				/* Random monster at least 5 - 10 levels out of deep */
				q_ptr->r_idx = get_mon_num(q_ptr->level + 4 + randint1(6));
			}

			r_ptr = &r_info[q_ptr->r_idx];

			while ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->rarity != 1))
			{
				q_ptr->r_idx = get_mon_num(q_ptr->level) + 4 + randint1(6);
				r_ptr = &r_info[q_ptr->r_idx];
			}

			if (q_ptr->max_num == 0)
			{
				/* Random monster number */
				if (randint1(10) > 7)
					q_ptr->max_num = 1;
				else
					q_ptr->max_num = randint1(3) + 1;
			}

			q_ptr->cur_num = 0;
			name = (r_name + r_ptr->name);
#ifdef JP
			msg_format("クエスト: %sを %d体倒す", name,q_ptr->max_num);
#else
			msg_format("Your quest: kill %d %s", q_ptr->max_num, name);
#endif
			msg_print(NULL);
		}
		else
		{
			get_questinfo(q_index);
		}
#else
		get_questinfo(q_index);
#endif

		/* check */
#ifdef JP
		if (!get_check("このクエストの依頼を受けますか？"))
#else
		if (!get_check("Do you accept this quest? "))
#endif
		{
			int i;
			for (i = 5; i < 18; i++)
				prt ("", i, 0);
			return;
		}

#ifdef JP
		msg_print("クエストの依頼を受けました。");
#else
		msg_print("You accept this quest.");
#endif

		q_ptr->status = QUEST_STATUS_TAKEN;

		reinit_wilderness = TRUE;
	}
}


/*
 * Display town history
 */
static void town_history(void)
{
	/* Save screen */
	screen_save();

	/* Peruse the building help file */
#ifdef JP
	(void)show_file("jbldg.txt", NULL, 0, 0);
#else
	(void)show_file("bldg.txt", NULL, 0, 0);
#endif


	/* Load screen */
	screen_load();
}


/*
 * Display the damage figure of an object
 * (used by compare_weapon_aux1)
 *
 * Only accurate for the current weapon, because it includes
 * the current +dam of the player.
 */
static void compare_weapon_aux2(object_type *o_ptr, int numblows,
				int r, int c, int mult, cptr attr, byte color)
{
	char tmp_str[MAX_NLEN];
	int maxdam, mindam;
	int dambonus;

	dambonus = o_ptr->to_d + p_ptr->to_d;

	/* Effect of damage dice */
	maxdam = (o_ptr->ds * o_ptr->dd * mult / 10) + dambonus;
	mindam = (o_ptr->dd * mult / 10) + dambonus;

	/* number of blows */
	maxdam *= numblows;
	mindam *= numblows;

	/* Print the intro text */
	c_put_str(color, attr, r, c);

	/* Calculate the min and max damage figures */
#ifdef JP
	sprintf(tmp_str, "１ターン: %d-%d ダメージ",
#else
	sprintf(tmp_str, "Attack: %d-%d damage",
#endif
			mindam, maxdam);

	/* Print the damage */
	put_str(tmp_str, r, c + 8);
}


/*
 * Show the damage figures for the various monster types
 *
 * Only accurate for the current weapon, because it includes
 * the current number of blows for the player.
 */
static void compare_weapon_aux1(object_type *o_ptr, int col, int r)
{
	u32b f1, f2, f3;

	/* Get the flags of the weapon */
	object_flags(o_ptr, &f1, &f2, &f3);

	/* Extra brands */
	if (p_ptr->tim_brand) f1 |= p_ptr->xtra_brand;

	/* Print the relevant lines */
#ifdef JP
	if (f1 & TR1_SLAY_HUMAN)  compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 25, "人間:",  TERM_YELLOW);
	if (f1 & TR1_SLAY_ANIMAL) compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 25, "動物:",  TERM_YELLOW);
	if (f1 & TR1_SLAY_EVIL)   compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 20, "邪悪:",  TERM_YELLOW);
	if (f1 & TR1_SLAY_UNDEAD) compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 30, "不死:",  TERM_YELLOW);
	if (f1 & TR1_SLAY_DEMON)  compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 30, "悪魔:",  TERM_YELLOW);
	if (f1 & TR1_SLAY_ORC)    compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 30, "オーク:",  TERM_YELLOW);
	if (f1 & TR1_SLAY_TROLL)  compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 30, "トロル:",  TERM_YELLOW);
	if (f1 & TR1_SLAY_GIANT)  compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 30, "巨人:",  TERM_YELLOW);
	if (f1 & TR1_SLAY_DRAGON) compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 30, "竜:",  TERM_YELLOW);
	if (f1 & TR1_KILL_DRAGON) compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 50, "竜:",  TERM_YELLOW);
	if (f1 & TR1_BRAND_ACID)  compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 25, "酸属性:",  TERM_RED);
	if (f1 & TR1_BRAND_ELEC)  compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 25, "電属性:",  TERM_RED);
	if (f1 & TR1_BRAND_FIRE)  compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 25, "炎属性:",  TERM_RED);
	if (f1 & TR1_BRAND_COLD)  compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 25, "冷属性:",  TERM_RED);
	if (f1 & TR1_BRAND_POIS)  compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 25, "毒属性:",  TERM_RED);
#else
	if (f1 & TR1_SLAY_HUMAN)  compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 25, "Humans:", TERM_YELLOW);
	if (f1 & TR1_SLAY_ANIMAL) compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 25, "Animals:", TERM_YELLOW);
	if (f1 & TR1_SLAY_EVIL)   compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 20, "Evil:", TERM_YELLOW);
	if (f1 & TR1_SLAY_UNDEAD) compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 30, "Undead:", TERM_YELLOW);
	if (f1 & TR1_SLAY_DEMON)  compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 30, "Demons:", TERM_YELLOW);
	if (f1 & TR1_SLAY_ORC)    compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 30, "Orcs:", TERM_YELLOW);
	if (f1 & TR1_SLAY_TROLL)  compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 30, "Trolls:", TERM_YELLOW);
	if (f1 & TR1_SLAY_GIANT)  compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 30, "Giants:", TERM_YELLOW);
	if (f1 & TR1_SLAY_DRAGON) compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 30, "Dragons:", TERM_YELLOW);
	if (f1 & TR1_KILL_DRAGON) compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 50, "Dragons:", TERM_YELLOW);
	if (f1 & TR1_BRAND_ACID)  compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 25, "Acid:", TERM_RED);
	if (f1 & TR1_BRAND_ELEC)  compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 25, "Elec:", TERM_RED);
	if (f1 & TR1_BRAND_FIRE)  compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 25, "Fire:", TERM_RED);
	if (f1 & TR1_BRAND_COLD)  compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 25, "Cold:", TERM_RED);
	if (f1 & TR1_BRAND_POIS)  compare_weapon_aux2(o_ptr, p_ptr->num_blow[0], r++, col, 25, "Poison:", TERM_RED);
#endif
}


static int hit_prob(int to_h, int ac)
{
	int chance, bonus;
	int prob = 0;
	int ac2 = ac * 3 / 4;

	bonus = (p_ptr->to_h + to_h + calc_melee_bonus(0));
	chance = p_ptr->skill_thn + bonus * BTH_PLUS_ADJ;
	if (chance > 0 && ac2 < chance) prob = (100 * (chance - ac2) / chance);
	return (5 + 95 * prob / 100);
}


/*
 * Displays all info about a weapon
 *
 * Only accurate for the current weapon, because it includes
 * various info about the player's +to_dam and number of blows.
 */
static void list_weapon(object_type *o_ptr, int row, int col)
{
	char o_name[MAX_NLEN];
	char tmp_str[MAX_NLEN];

	int maxdam, mindam;
	int dambonus;

	/* Print the weapon name */
	object_desc(o_name, o_ptr, OD_NAME_ONLY);
	c_put_str(TERM_YELLOW, o_name, row, col);

	/* Print the player's number of blows */
#ifdef JP
	sprintf(tmp_str, "攻撃回数: %d", p_ptr->num_blow[0]);
#else
	sprintf(tmp_str, "Number of Blows: %d", p_ptr->num_blow[0]);
#endif
	put_str(tmp_str, row+1, col);

#ifdef JP
	/* Print hit probabilities */
	sprintf(tmp_str, "敵のAC:   25  50  75 100 150 200");
			put_str(tmp_str, row+2, col);
	sprintf(tmp_str, "命中率:  %2d%% %2d%% %2d%% %2d%% %2d%% %2d%%", 
			hit_prob(o_ptr->to_h, 25), hit_prob(o_ptr->to_h, 50),
			hit_prob(o_ptr->to_h, 75), hit_prob(o_ptr->to_h, 100),
			hit_prob(o_ptr->to_h, 150), hit_prob(o_ptr->to_h, 200));
	put_str(tmp_str, row+3, col);
#endif

#ifdef JP
	c_put_str(TERM_YELLOW, "可能なダメージ:", row+5, col);
#else
	c_put_str(TERM_YELLOW, "Possible Damage:", row+5, col);
#endif

	dambonus = o_ptr->to_d + p_ptr->to_d;

	/* Effect of damage dice */
	maxdam = (o_ptr->ds * o_ptr->dd) + dambonus;
	mindam = o_ptr->dd + dambonus;

	/* Damage for one blow (if it hits) */
#ifdef JP
	sprintf(tmp_str, "攻撃一回につき %d-%d",
			mindam, maxdam);
	put_str(tmp_str, row+6, col+1);
#else
	sprintf(tmp_str, "One Strike: %d-%d damage",
			mindam, maxdam);
	put_str(tmp_str, row+6, col+1);
#endif

	/* rescale */
	maxdam *= p_ptr->num_blow[0];
	mindam *= p_ptr->num_blow[0];

	/* Damage for the complete attack (if all blows hit) */
#ifdef JP
	sprintf(tmp_str, "１ターンにつき %d-%d",
			mindam, maxdam);
	put_str(tmp_str, row+7, col+1);
#else
	sprintf(tmp_str, "One Attack: %d-%d damage",
			mindam, maxdam);
	put_str(tmp_str, row+7, col+1);
#endif
}


/*
 * Hook to specify "ego"
 */
static bool item_tester_hook_ego_item(const object_type *o_ptr)
{
	if (!o_ptr->name2) return (FALSE);

	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			return (FALSE);
		}
	}
	return (TRUE);
}


/*
 * Hook to specify "weapon"
 */
bool item_tester_hook_melee_weapon(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Hook to specify "ammo"
 */
static bool item_tester_hook_ammo(const object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SHOT:
		case TV_ARROW:
		case TV_BOLT:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}


/*
 * Compare weapons
 *
 * Copies the weapons to compare into the weapon-slot and
 * compares the values for both weapons.
 */
static bool compare_weapons(void)
{
	int item, item2;
	object_type *o1_ptr, *o2_ptr;
	object_type orig_weapon;
	object_type *i_ptr;
	cptr q, s;
	int row = 2;

	screen_save();

	/* Clear the screen */
	clear_bldg(0, 22);

	/* Store copy of original wielded weapon */
	i_ptr = &inventory[INVEN_WIELD];
	object_copy(&orig_weapon, i_ptr);

	/* Only compare melee weapons */
	item_tester_hook = item_tester_hook_melee_weapon;

	/* Get the first weapon */
#ifdef JP
	q = "第一の武器は？";
	s = "比べるものがありません。";
#else
	q = "What is your first weapon? ";
	s = "You have nothing to compare.";
#endif

	if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN)))
	{
		screen_load();
		return (FALSE);
	}

	/* Clear the screen */
	clear_bldg(0, 22);

	/* Get the item (in the pack) */
	o1_ptr = &inventory[item];

	/* Only compare melee weapons */
	item_tester_hook = item_tester_hook_melee_weapon;

	/* Get the second weapon */
#ifdef JP
	q = "第二の武器は？";
	s = "比べるものがありません。";
#else
	q = "What is your second weapon? ";
	s = "You have nothing to compare.";
#endif

	if (!get_item(&item2, q, s, (USE_EQUIP | USE_INVEN)))
	{
		screen_load();
		return (FALSE);
	}

	/* Clear the screen */
	clear_bldg(0, 22);

	/* Get the item (in the pack) */
	o2_ptr = &inventory[item2];

	/* Copy first weapon into the weapon slot (if it's not already there) */
	if (o1_ptr != i_ptr)
		object_copy(i_ptr, o1_ptr);

	/* Get the new values */
	calc_bonuses();

	/* List the new values */
	list_weapon(o1_ptr, row, 2);
	compare_weapon_aux1(o1_ptr, 2, row + 8);

	/* Copy second weapon into the weapon slot (if it's not already there) */
	if (o2_ptr != i_ptr)
		object_copy(i_ptr, o2_ptr);
	else
		object_copy(i_ptr, &orig_weapon);

	/* Get the new values */
	calc_bonuses();

	/* List the new values */
	list_weapon(o2_ptr, row, 40);
	compare_weapon_aux1(o2_ptr, 40, row + 8);

	/* Copy back the original weapon into the weapon slot */
	object_copy(i_ptr, &orig_weapon);

	/* Reset the values for the old weapon */
	calc_bonuses();

#ifdef JP
	put_str("(一番高いダメージが適用されます。複数の倍打効果は足し算されません。)", 21, 5);
	put_str("現在の能力から判断すると、あなたの武器は以下のような威力を発揮します: ", 0, 0);
#else
	put_str("(Only highest damage applies per monster. Special damage not cumulative.)", 21, 0);
	put_str("Based on your current abilities, here is what your weapons will do: ", 0, 0);
#endif

	(void) inkey();
	screen_load();

	/* Done */
	return (TRUE);
}


/*
 * Forge ego item
 */
static bool forge_ego_item(void)
{
	int item;
	int cost;
	cptr q, s;
	object_type *o_ptr;
	object_type dummy;
	object_type *q_ptr = &dummy;
#ifdef JP
	cptr basenm;
#else
	char basenm[MAX_NLEN];
#endif

	/* Get an item */
#ifdef JP
	q = "どのアイテムを鍛えますか？";
	s = "鍛えられるものがありません。";
#else
	q = "Forge which item? ";
	s = "You have nothing to forge.";
#endif

	/* Only forge ego items */
	item_tester_hook = item_tester_hook_ego_item;
	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return (FALSE);

	/* Get the item (in the pack) */
	o_ptr = &inventory[item];

	/* It is already named */
	if (o_ptr->ego_name)
	{
#ifdef JP
		msg_format("これ以上鍛えることはできません！");
#else
		msg_format("You can not forge it more!");
#endif
		return (FALSE);
	}

	/* They are too many */
	if (o_ptr->number > 1)
	{
#ifdef JP
		msg_format("一度に複数を鍛えることはできません！");
#else
		msg_format("They are too many to forge at once!");
#endif
		return (FALSE);
	}

	/* Get the value of one of the items (except curses) */
	object_copy(q_ptr, o_ptr);
	q_ptr->art_flags3 &= ~(TR3_CURSED | TR3_HEAVY_CURSE);
	cost = (object_value_real(q_ptr) + 100000L);

#ifdef JP
	if (!get_check(format("＄%dかかりますがよろしいですか？ ", cost))) return (FALSE);
#else
	if (!get_check(format("Costs %d gold, okay? ", cost))) return (FALSE);
#endif

	/* Check if the player has enough money */
	if (p_ptr->au < cost)
	{
#ifdef JP
		basenm = get_object_name(o_ptr);
		msg_format("%sを改良するだけのゴールドがありません！", basenm);
#else
		object_desc(basenm, o_ptr, OD_NAME_ONLY);
		msg_format("You do not have the gold to improve %s!", basenm);
#endif
		msg_print(NULL);
		return (FALSE);
	}

	/* Forge it */
	create_named_ego(o_ptr);

#ifdef JP
	basenm = get_object_name(o_ptr);
	msg_format("＄%d で%sを改良しました。", cost, basenm);
#else
	object_desc(basenm, o_ptr, OD_NAME_ONLY);
	msg_format("Improved %s for %d gold.", basenm, cost);
#endif
	msg_print(NULL);

	/* Charge the money */
	p_ptr->au -= cost;

	/* Something happened */
	return (TRUE);
}

/*
 * Hook to specify "broken weapon"
 */
static bool item_tester_hook_broken_weapon(const object_type *o_ptr)
{
	if (o_ptr->tval != TV_SWORD) return FALSE;

	switch (o_ptr->sval)
	{
	case SV_BROKEN_DAGGER:
	case SV_BROKEN_SWORD:
		if (!o_ptr->name1) return TRUE;
	}

	return FALSE;
}

/*
 * Repair broken weapon
 */
static bool repair_broken_weapon(void)
{
	int         item;
	s32b        cost;
	cptr        q, s;
	object_type *o_ptr;
	object_type dummy;
	object_type *q_ptr = &dummy;
	object_kind *k_ptr, *old_k_ptr;
	byte        new_sval;
#ifdef JP
	cptr basenm;
#else
	char basenm[MAX_NLEN];
#endif

	/* Get an item */
#ifdef JP
	q = "どの折れた武器を修復しますか？";
	s = "修復できる折れた武器がありません。";
#else
	q = "Repair which broken weapon? ";
	s = "You have no broken weapon to repair.";
#endif

	/* Only forge broken weapons */
	item_tester_hook = item_tester_hook_broken_weapon;
	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return (FALSE);

	/* Get the item (in the pack) */
	o_ptr = &inventory[item];

	/* They are too many */
	if (o_ptr->number > 1)
	{
#ifdef JP
		msg_format("一度に複数を修復することはできません！");
#else
		msg_format("They are too many to repair at once!");
#endif
		return (FALSE);
	}

	/* Get the value of one of the items (except curses) */
	if (o_ptr->art_name)
	{
		object_copy(q_ptr, o_ptr);
		q_ptr->art_flags3 &= ~(TR3_CURSED | TR3_HEAVY_CURSE | TR3_PERMA_CURSE);
		cost = object_value_real(q_ptr) + 1000L;
		if (cost < 1000L) cost = 1000L;
	}
	else
	{
		cost = 1000L + (o_ptr->name2 ? e_info[o_ptr->name2].cost : 0L);
	}

#ifdef JP
	if (!get_check(format("＄%dかかりますがよろしいですか？ ", cost))) return FALSE;
#else
	if (!get_check(format("Costs %d gold, okay? ", cost))) return FALSE;
#endif

	/* Check if the player has enough money */
	if (p_ptr->au < cost)
	{
#ifdef JP
		basenm = get_object_name(o_ptr);
		msg_format("%sを修復するだけのゴールドがありません！", basenm);
#else
		object_desc(basenm, o_ptr, OD_NAME_ONLY);
		msg_format("You do not have the gold to repair %s!", basenm);
#endif
		msg_print(NULL);
		return FALSE;
	}

	if (o_ptr->sval == SV_BROKEN_DAGGER) new_sval = SV_DAGGER;
	else
	{
		int i, light_sword_num = 0;
		byte *light_sword_sval_idx;

		C_MAKE(light_sword_sval_idx, max_k_idx, byte);
		for (i = 1; i < max_k_idx; i++)
		{
			k_ptr = &k_info[i];
			if (k_ptr->tval == TV_SWORD)
			{
				switch (k_ptr->sval)
				{
				case SV_BROKEN_DAGGER: case SV_BROKEN_SWORD:
					break;
				default:
					if ((k_ptr->weight >= 30) && (k_ptr->weight <= 80))
						light_sword_sval_idx[light_sword_num++] = k_ptr->sval;
					break;
				}
			}
		}

		if (!light_sword_num)
		{
#ifdef JP
			msg_print("修復できませんでした。");
#else
			msg_print("Failed to repair.");
#endif
			C_KILL(light_sword_sval_idx, max_k_idx, byte);
			return FALSE;
		}

		new_sval = light_sword_sval_idx[randint0(light_sword_num)];
		C_KILL(light_sword_sval_idx, max_k_idx, byte);
	}


	object_copy(q_ptr, o_ptr);
	old_k_ptr = &k_info[o_ptr->k_idx];

	q_ptr->k_idx = lookup_kind(TV_SWORD, new_sval);
	k_ptr = &k_info[q_ptr->k_idx];
	q_ptr->sval = new_sval;
	q_ptr->weight = k_ptr->weight + o_ptr->weight - old_k_ptr->weight;
	q_ptr->to_h += randint0(5);
	q_ptr->to_d += randint0(5);
	q_ptr->ac = k_ptr->ac + o_ptr->ac - old_k_ptr->ac;
	q_ptr->dd = k_ptr->dd + o_ptr->dd - old_k_ptr->dd;
	q_ptr->ds = k_ptr->ds + o_ptr->ds - old_k_ptr->ds;

	if (q_ptr->name2 && !q_ptr->ego_name && one_in_(7))
	{
		/* Forge it */
		create_named_ego(q_ptr);

#ifdef JP
		msg_print("これはかなりの業物だったようだ。");
#else
		msg_print("This blade seems to be exceptionally.");
#endif
	}

#ifdef JP
	basenm = get_object_name(q_ptr);
	msg_format("＄%dで%sに修復しました。", cost, basenm);
#else
	object_desc(basenm, q_ptr, OD_NAME_ONLY);
	msg_format("Repaired into %s for %d gold.", basenm, cost);
#endif
	msg_print(NULL);

	/* Charge the money */
	p_ptr->au -= cost;

	/* Copyback */
	object_copy(o_ptr, q_ptr);
	p_ptr->update |= PU_BONUS;
	handle_stuff();

	/* Something happened */
	return TRUE;
}

/*
 * Enchant item
 */
static bool enchant_item(int cost, int to_hit, int to_dam, int to_ac)
{
	int         i, item;
	bool        okay = FALSE;
	object_type *o_ptr;
	cptr        q, s;
	int         maxenchant = (p_ptr->lev / 5);
	char        tmp_str[MAX_NLEN];


	clear_bldg(5, 18);
#ifdef JP
	prt(format("現在のあなたのレベルだと、+%d まで改良できます。", maxenchant), 5, 0);
	prt(format(" 改良の料金は一個につき＄%d です。", cost), 7, 0);
#else
	prt(format("  Based on your skill, we can improve up to +%d.", maxenchant), 5, 0);
	prt(format("  The price for the service is %d gold per item.", cost), 7, 0);
#endif

	/* Get an item */
#ifdef JP
	q = "どのアイテムを改良しますか？";
	s = "改良できるものがありません。";
#else
	q = "Improve which item? ";
	s = "You have nothing to improve.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_EQUIP))) return (FALSE);

	/* Get the item (in the pack) */
	o_ptr = &inventory[item];

	/* Check if the player has enough money */
	if (p_ptr->au < (cost * o_ptr->number))
	{
		object_desc(tmp_str, o_ptr, OD_NAME_ONLY);
#ifdef JP
		msg_format("%sを改良するだけのゴールドがありません！", tmp_str);
#else
		msg_format("You do not have the gold to improve %s!", tmp_str);
#endif
		msg_print(NULL);
		return (FALSE);
	}

	/* Enchant to hit */
	for (i = 0; i < to_hit; i++)
	{
		if (o_ptr->to_h < maxenchant)
		{
			if (enchant(o_ptr, 1, (ENCH_TOHIT | ENCH_FORCE)))
			{
				okay = TRUE;
				break;
			}
		}
	}

	/* Enchant to damage */
	for (i = 0; i < to_dam; i++)
	{
		if (o_ptr->to_d < maxenchant)
		{
			if (enchant(o_ptr, 1, (ENCH_TODAM | ENCH_FORCE)))
			{
				okay = TRUE;
				break;
			}
		}
	}

	/* Enchant to AC */
	for (i = 0; i < to_ac; i++)
	{
		if (o_ptr->to_a < maxenchant)
		{
			if (enchant(o_ptr, 1, (ENCH_TOAC | ENCH_FORCE)))
			{
				okay = TRUE;
				break;
			}
		}
	}

	/* Failure */
	if (!okay)
	{
		/* Flush */
		if (flush_failure) flush();

		/* Message */
#ifdef JP
		msg_print("改良に失敗した。");
#else
		msg_print("The improvement failed.");
#endif

		return (FALSE);
	}
	else
	{
		object_desc(tmp_str, o_ptr, OD_NAME_AND_ENCHANT);
#ifdef JP
		msg_format("＄%d で%sに改良しました。", cost * o_ptr->number, tmp_str);
#else
		msg_format("Improved into %s for %d gold.", tmp_str, cost * o_ptr->number);
#endif
		msg_print(NULL);

		/* Charge the money */
		p_ptr->au -= (cost * o_ptr->number);

		/* Something happened */
		return (TRUE);
	}
}


/*
 * Recharge rods, wands and staves
 *
 * The player can select the number of charges to add
 * (up to a limit), and the recharge never fails.
 *
 * The cost for rods depends on the level of the rod. The prices
 * for recharging wands and staves are dependent on the cost of
 * the base-item.
 */
static void building_recharge(void)
{
	int         item, lev;
	object_type *o_ptr;
	object_kind *k_ptr;
	cptr        q, s;
	int         price;
	int         charges;
	int         max_charges;
	char        tmp_str[MAX_NLEN];


	/* Display some info */
	msg_flag = FALSE;
	clear_bldg(5, 18);
#ifdef JP
	prt("  再充填の費用はアイテムの種類によります。", 6, 0);
#else
	prt("  The prices of recharge depend on the type.", 6, 0);
#endif

	/* Only accept legal items */
	item_tester_hook = item_tester_hook_recharge;

	/* Get an item */
#ifdef JP
q = "どのアイテムに魔力を充填しますか? ";
s = "魔力を充填すべきアイテムがない。";
#else
	q = "Recharge which item? ";
	s = "You have nothing to recharge.";
#endif

	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return;

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

	k_ptr = &k_info[o_ptr->k_idx];

	/*
	 * We don't want to give the player free info about
	 * the level of the item or the number of charges.
	 */
	/* The item must be "known" */
	if (!object_known_p(o_ptr))
	{
#ifdef JP
		msg_format("充填する前に鑑定されている必要があります！");
#else
		msg_format("The item must be identified first!");
#endif
		msg_print(NULL);

		if ((p_ptr->au >= 50) &&
#ifdef JP
			get_check("＄50で鑑定しますか？ "))
#else
			get_check("Identify for 50 gold? "))
#endif

		{
			/* Pay the price */
			p_ptr->au -= 50;

			/* Identify it */
			identify_item(o_ptr);

			/* Description */
			object_desc(tmp_str, o_ptr, 0);

#ifdef JP
			msg_format("%s です。", tmp_str);
#else
			msg_format("You have: %s.", tmp_str);
#endif

			/* Auto-inscription */
			autopick_alter_item(item, FALSE);

			/* Update the gold display */
			building_prt_gold();
		}
		else
		{
			return;
		}
	}

	/* Extract the object "level" */
	lev = get_object_level(o_ptr);

	/* Price for a rod */
	if (o_ptr->tval == TV_ROD)
	{
		if (o_ptr->timeout > 0)
		{
			/* Fully recharge */
			price = (lev * o_ptr->timeout) / k_ptr->pval;
		}
		else
		{
			/* No recharge necessary */
#ifdef JP
			msg_format("それは再充填する必要はありません。");
#else
			msg_format("That doesn't need to be recharged.");
#endif
			msg_print(NULL);
			return;
		}
	}
	else if (o_ptr->tval == TV_STAFF)
	{
		/* Price per charge ( = double the price paid by shopkeepers for the charge) */
		price = (get_object_cost(o_ptr) / 10) * o_ptr->number;

		/* Pay at least 10 gold per charge */
		price = MAX(10, price);
	}
	else
	{
		/* Price per charge ( = double the price paid by shopkeepers for the charge) */
		price = (get_object_cost(o_ptr) / 10);

		/* Pay at least 10 gold per charge */
		price = MAX(10, price);
	}

	/* Limit the number of charges for wands and staves */
	if (((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF)) &&
	     (o_ptr->pval / o_ptr->number >= k_ptr->pval))
	{
		if ((o_ptr->tval == TV_WAND) && (o_ptr->number == 1))
#ifdef JP
			msg_print("このワンドはもう充分に充填されています。");
#else
			msg_print("This wand is already fully charged.");
#endif
		else if ((o_ptr->tval == TV_WAND) && (o_ptr->number > 1))
#ifdef JP
			msg_print("このワンドはもう充分に充填されています。");
#else
			msg_print("These wands are already fully charged.");
#endif
		else if ((o_ptr->tval == TV_STAFF) && (o_ptr->number == 1))
#ifdef JP
			msg_print("このスタッフはもう充分に充填されています。");
#else
			msg_print("This staff is already fully charged.");
#endif
		else if ((o_ptr->tval == TV_STAFF) && (o_ptr->number > 1))
#ifdef JP
			msg_print("このスタッフはもう充分に充填されています。");
#else
			msg_print("These staffs are already fully charged.");
#endif
		msg_print(NULL);
		return;
	}

	/* Check if the player has enough money */
	if (p_ptr->au < price)
	{
		object_desc(tmp_str, o_ptr, OD_NAME_ONLY);
#ifdef JP
		msg_format("%sを再充填するには＄%d 必要です！", tmp_str,price );
#else
		msg_format("You need %d gold to recharge %s!", price, tmp_str);
#endif
		msg_print(NULL);
		return;
	}

	if (o_ptr->tval == TV_ROD)
	{
#ifdef JP
		if (get_check(format("そのロッドを＄%d で再充填しますか？",
					price)))
#else
		if (get_check(format("Recharge the %s for %d gold? ",
					((o_ptr->number > 1) ? "rods" : "rod"), price)))
#endif
		{
			/* Recharge fully */
			o_ptr->timeout = 0;
		}
		else
		{
			return;
		}
	}
	else
	{
		if (o_ptr->tval == TV_STAFF)
			max_charges = k_ptr->pval - o_ptr->pval;
		else
			max_charges = o_ptr->number * k_ptr->pval - o_ptr->pval;

		/* Get the quantity for staves and wands */
#ifdef JP
		charges = get_quantity(format("一回分＄%d で何回分充填しますか？",
#else
		charges = get_quantity(format("Add how many charges for %d gold? ",
#endif
			      price), MIN(p_ptr->au / price, max_charges));

		/* Do nothing */
		if (charges < 1) return;

		/* Get the new price */
		price *= charges;

		/* Recharge */
		o_ptr->pval += charges;

		/* We no longer think the item is empty */
		o_ptr->ident &= ~(IDENT_EMPTY);
	}

	/* Give feedback */
	object_desc(tmp_str, o_ptr, 0);
#ifdef JP
	msg_format("%sを＄%d で再充填しました。", tmp_str, price);
#else
	msg_format("%^s %s recharged for %d gold.", tmp_str, ((o_ptr->number > 1) ? "were" : "was"), price);
#endif
	msg_print(NULL);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN);

	/* Pay the price */
	p_ptr->au -= price;

	/* Finished */
	return;
}


/*
 * Recharge rods, wands and staves
 *
 * The player can select the number of charges to add
 * (up to a limit), and the recharge never fails.
 *
 * The cost for rods depends on the level of the rod. The prices
 * for recharging wands and staves are dependent on the cost of
 * the base-item.
 */
static void building_recharge_all(void)
{
	int         i;
	int         lev;
	object_type *o_ptr;
	object_kind *k_ptr;
	int         price = 0;
	int         total_cost = 0;


	/* Display some info */
	msg_flag = FALSE;
	clear_bldg(5, 18);
#ifdef JP
	prt("  再充填の費用はアイテムの種類によります。", 6, 0);
#else
	prt("  The prices of recharge depend on the type.", 6, 0);
#endif

	/* Calculate cost */
	for ( i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];

		/* skip non magic device */
		if (o_ptr->tval < TV_STAFF || o_ptr->tval > TV_ROD) continue;

		/* need identified */
		if (!object_known_p(o_ptr)) total_cost += 50;

		/* Extract the object "level" */
		lev = get_object_level(o_ptr);

		k_ptr = &k_info[o_ptr->k_idx];

		switch (o_ptr->tval)
		{
		case TV_ROD:
			price = (lev * o_ptr->timeout) / k_ptr->pval;
			break;

		case TV_STAFF:
			/* Price per charge ( = double the price paid by shopkeepers for the charge) */
			price = (get_object_cost(o_ptr) / 10) * o_ptr->number;

			/* Pay at least 10 gold per charge */
			price = MAX(10, price);

			/* Fully charge */
			price = (k_ptr->pval - o_ptr->pval) * price;
			break;

		case TV_WAND:
			/* Price per charge ( = double the price paid by shopkeepers for the charge) */
			price = (get_object_cost(o_ptr) / 10);

			/* Pay at least 10 gold per charge */
			price = MAX(10, price);

			/* Fully charge */
			price = (o_ptr->number * k_ptr->pval - o_ptr->pval) * price;
			break;
		}

		/* if price <= 0 then item have enough charge */
		if (price > 0) total_cost += price;
	}

	if (!total_cost)
	{
#ifdef JP
		msg_print("充填する必要はありません。");
#else
		msg_print("That doesn't need to be recharged.");
#endif

		msg_print(NULL);
		return;
	}

	/* Check if the player has enough money */
	if (p_ptr->au < total_cost)
	{
#ifdef JP
		msg_format("すべてのアイテムを再充填するには＄%d 必要です！", total_cost );
#else
		msg_format("You need %d gold to recharge all items!",total_cost);
#endif
		msg_print(NULL);
		return;
	}

#ifdef JP
	if (!get_check(format("すべてのアイテムを ＄%d で再充填しますか？",  total_cost))) return;
#else
	if (!get_check(format("Recharge all items for %d gold?", total_cost))) return;
#endif

	for (i = 0; i < INVEN_PACK; i++)
	{
		o_ptr = &inventory[i];
		k_ptr = &k_info[o_ptr->k_idx];

		/* skip non magic device */
		if (o_ptr->tval < TV_STAFF || o_ptr->tval > TV_ROD) continue;

		/* Identify it */
		if (!object_known_p(o_ptr))
		{
			identify_item(o_ptr);

			/* Auto-inscription */
			autopick_alter_item(i, FALSE);
		}

		/* Recharge */
		switch (o_ptr->tval)
		{
		case TV_ROD:
			o_ptr->timeout = 0;
			break;
		case TV_STAFF:
			if (o_ptr->pval < k_ptr->pval) o_ptr->pval = k_ptr->pval;
			/* We no longer think the item is empty */
			o_ptr->ident &= ~(IDENT_EMPTY);
			break;
		case TV_WAND:
			if (o_ptr->pval < o_ptr->number * k_ptr->pval)
				o_ptr->pval = o_ptr->number * k_ptr->pval;
			/* We no longer think the item is empty */
			o_ptr->ident &= ~(IDENT_EMPTY);
			break;
		}
	}

	/* Give feedback */
#ifdef JP
	msg_format("＄%d で再充填しました。", total_cost);
#else
	msg_format("You pay %d gold.", total_cost);
#endif
	msg_print(NULL);

	/* Combine / Reorder the pack (later) */
	p_ptr->notice |= (PN_COMBINE | PN_REORDER);

	/* Window stuff */
	p_ptr->window |= (PW_INVEN);

	/* Pay the price */
	p_ptr->au -= total_cost;

	/* Finished */
	return;
}


/*
 * Execute a building command
 */
static void bldg_process_command(building_type *bldg, int i)
{
	int bact = bldg->actions[i];
	int bcost;
	bool paid = FALSE;
	int amt;

	if (is_owner(bldg))
		bcost = bldg->member_costs[i];
	else
		bcost = bldg->other_costs[i];

	/* action restrictions */
	if (((bldg->action_restr[i] == 1) && !is_member(bldg)) ||
	    ((bldg->action_restr[i] == 2) && !is_owner(bldg)))
	{
#ifdef JP
		msg_print("それを選択する権利はありません！");
#else
		msg_print("You have no right to choose that!");
#endif
		msg_print(NULL);
		return;
	}

	/* check gold (HACK - Recharge uses variable costs) */
	if ((bact != BACT_RECHARGE) && (bact != BACT_RECHARGE_ALL) &&
	    (((bldg->member_costs[i] > p_ptr->au) && is_owner(bldg)) ||
	     ((bldg->other_costs[i] > p_ptr->au) && !is_owner(bldg))))
	{
#ifdef JP
		msg_print("お金が足りません！");
#else
		msg_print("You do not have the gold!");
#endif
		msg_print(NULL);
		return;
	}

	switch (bact)
	{
		case BACT_NOTHING:
			/* Do nothing */
			break;
		case BACT_RESEARCH_ITEM:
			paid = identify_fully();
			break;
		case BACT_TOWN_HISTORY:
			town_history();
			break;
		case BACT_RACE_LEGENDS:
			race_legends();
			break;
		case BACT_QUEST:
			castle_quest();
			break;
		case BACT_KING_LEGENDS:
		case BACT_ARENA_LEGENDS:
		case BACT_LEGENDS:
			show_highclass();
			break;
		case BACT_POSTER:
		case BACT_ARENA_RULES:
		case BACT_ARENA:
			arena_comm(bact);
			break;
		case BACT_IN_BETWEEN:
		case BACT_CRAPS:
		case BACT_SPIN_WHEEL:
		case BACT_DICE_SLOTS:
		case BACT_GAMBLE_RULES:
			gamble_comm(bact);
			break;
		case BACT_REST:
		case BACT_RUMORS:
		case BACT_FOOD:
			paid = inn_comm(bact);
			break;
		case BACT_RESEARCH_MONSTER:
			paid = research_mon();
			break;
		case BACT_COMPARE_WEAPONS:
			paid = compare_weapons();
			break;
		case BACT_FORGE_EGO:
			forge_ego_item();
			break;
		case BACT_REPAIR_BROKEN:
			repair_broken_weapon();
			break;
		case BACT_ENCHANT_WEAPON:
			item_tester_hook = item_tester_hook_melee_weapon;
			enchant_item(bcost, 1, 1, 0);
			break;
		case BACT_ENCHANT_ARMOR:
			item_tester_hook = item_tester_hook_armour;
			enchant_item(bcost, 0, 0, 1);
			break;
		case BACT_RECHARGE:
			building_recharge();
			break;
		case BACT_RECHARGE_ALL:
			building_recharge_all();
			break;
		case BACT_IDENTS: /* needs work */
#ifdef JP
			if (!get_check("持ち物を全て鑑定してよろしいですか？")) break;
#else
			if (!get_check("Do you pay for identify all your possession? ")) break;
#endif
			identify_pack();

			/* Combine / Reorder the pack (later) */
			p_ptr->notice |= (PN_COMBINE | PN_REORDER);
#ifdef JP
			msg_print(" 持ち物全てが鑑定されました。");
#else
			msg_print("Your posessions have been identified.");
#endif
			msg_print(NULL);
			paid = TRUE;
			break;
		case BACT_LEARN:
			do_cmd_study();
			break;
		case BACT_HEALING: /* needs work */
			hp_player(200);
			set_poisoned(0);
			set_blind(0);
			set_confused(0);
			set_cut(0);
			set_stun(0);
			paid = TRUE;
			break;
		case BACT_RESTORE: /* needs work */
			if (do_res_stat(A_STR)) paid = TRUE;
			if (do_res_stat(A_INT)) paid = TRUE;
			if (do_res_stat(A_WIS)) paid = TRUE;
			if (do_res_stat(A_DEX)) paid = TRUE;
			if (do_res_stat(A_CON)) paid = TRUE;
			if (do_res_stat(A_CHR)) paid = TRUE;
			break;
		case BACT_GOLD: /* set timed reward flag */
			if (!p_ptr->rewards[BACT_GOLD])
			{
				share_gold();
				p_ptr->rewards[BACT_GOLD] = TRUE;
			}
			else
			{
#ifdef JP
				msg_print("今日の分け前はすでに支払ったぞ！");
#else
				msg_print("You just had your daily allowance!");
#endif
				msg_print(NULL);
			}
			break;
		case BACT_ENCHANT_ARROWS:
			item_tester_hook = item_tester_hook_ammo;
			enchant_item(bcost, 1, 1, 0);
			break;
		case BACT_ENCHANT_BOW:
			item_tester_tval = TV_BOW;
			enchant_item(bcost, 1, 1, 0);
			break;
		case BACT_RECALL:
			if (recall_player(1)) paid = TRUE;
			break;
		case BACT_TELEPORT_LEVEL:
#ifdef JP
			amt = get_quantity("何階にテレポートしますか？", 98);
#else
			amt = get_quantity("Teleport to which level? ", 98);
#endif
			if (amt > 0)
			{
				p_ptr->word_recall = 1;
				p_ptr->max_dlv = amt;
#ifdef JP
				msg_print("回りの大気が張りつめてきた...");
#else
				msg_print("The air about you becomes charged...");
#endif

				paid = TRUE;
				p_ptr->redraw |= (PR_STATUS);
			}
			break;
		case BACT_LOSE_MUTATION:
			if (!p_ptr->muta)
			{
#ifdef JP
				msg_print("治す必要はない。");
#else
				msg_print("You have no mutations.");
#endif
				msg_print(NULL);
			}
			else
			{
				while(!lose_mutation(0));

				paid = TRUE;
			}
			break;
	}

	if (paid)
	{
		p_ptr->au -= bcost;
	}
}


/*
 * Enter quest level
 */
void do_cmd_quest(void)
{
	if (cave[py][px].feat != FEAT_QUEST_ENTER)
	{
#ifdef JP
		msg_print("ここにはクエストの入口はない。");
#else
		msg_print("You see no quest level here.");
#endif
		return;
	}
	else
	{
		/* check */
#ifdef JP
		msg_print("ここにはクエストへの入口があります。");
		if (!get_check("クエストに入りますか？")) return;
#else
		msg_print("There is an entry of a quest.");
		if (!get_check("Do you enter? ")) return;
#endif

		/* Player enters a new quest */
		p_ptr->oldpy = 0;
		p_ptr->oldpx = 0;

		leave_quest_check();

		p_ptr->inside_quest = cave[py][px].special;
		if (quest[leaving_quest].type != QUEST_TYPE_RANDOM) dun_level = 1;
		p_ptr->leaving = TRUE;
	}
}


/*
 * Do building commands
 */
void do_cmd_bldg(void)
{
	int             i, which;
	char            command;
	bool            validcmd;
	building_type   *bldg;

	if (!((cave[py][px].feat >= FEAT_BLDG_HEAD) &&
		  (cave[py][px].feat <= FEAT_BLDG_TAIL)))
	{
#ifdef JP
		msg_print("ここには建物はない。");
#else
		msg_print("You see no building here.");
#endif
		return;
	}

	which = (cave[py][px].feat - FEAT_BLDG_HEAD);

	bldg = &building[which];

	/* Don't re-init the wilderness */
	reinit_wilderness = FALSE;

	if ((which == 2) && p_ptr->inside_arena && !p_ptr->exit_bldg)
	{
#ifdef JP
		prt("ゲートは閉まっている。モンスターがあなたを待っている！",0,0);
#else
		prt("The gates are closed.  The monster awaits!", 0, 0);
#endif
		return;
	}
	else if ((which == 2) && p_ptr->inside_arena)
	{
		p_ptr->leaving = TRUE;
		p_ptr->inside_arena = FALSE;
	}
	else
	{
		p_ptr->oldpy = py;
		p_ptr->oldpx = px;
	}

	/* Forget the lite */
	forget_lite();

	/* Forget the view */
	forget_view();

	/* Hack -- Increase "icky" depth */
	character_icky++;

	command_arg = 0;
	command_rep = 0;
	command_new = 0;

	show_building(bldg);
	leave_bldg = FALSE;

	while (!leave_bldg)
	{
		validcmd = FALSE;
		prt("", 1, 0);

		building_prt_gold();

		command = inkey();

		if (command == ESCAPE)
		{
			leave_bldg = TRUE;
			p_ptr->inside_arena = FALSE;
			break;
		}

		for (i = 0; i < 6; i++)
		{
			if (bldg->letters[i])
			{
				if (bldg->letters[i] == command)
				{
					validcmd = TRUE;
					break;
				}
			}
		}

		if (validcmd)
			bldg_process_command(bldg, i);

		/* Notice stuff */
		notice_stuff();

		/* Handle stuff */
		handle_stuff();
	}

	/* Flush messages XXX XXX XXX */
	msg_print(NULL);

	/* Reinit wilderness to activate quests ... */
	if (reinit_wilderness)
		p_ptr->leaving = TRUE;

	/* Hack -- Decrease "icky" depth */
	character_icky--;

	/* Clear the screen */
	Term_clear();

	/* Update the visuals */
	p_ptr->update |= (PU_VIEW | PU_MONSTERS | PU_BONUS | PU_LITE | PU_MON_LITE);

	/* Redraw entire screen */
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_EQUIPPY | PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
}
