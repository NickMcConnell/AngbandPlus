/* File: melee1.c */

/* Purpose: Monster attacks */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Critical blow.  All hits that do 95% of total possible damage,
 * and which also do at least 20 damage, or, sometimes, N damage.
 * This is used only to determine "cuts" and "stuns".
 */
static int monster_critical(int dice, int sides, int dam, bool do_stun)
{
	int max = 0;
	int total = dice * sides;

	/* Must do at least 95% of perfect */
	if (dam < total * 19 / 20) return (0);

	/* Weak blows rarely work */
	if ((dam < 20) && (randint0(100) >= dam)) return (0);

	/* Perfect damage */
	if (dam >= total && dam >= 40) max++;

	/* Super-charge */
	if (dam >= 20)
	{
		while (randint0(100) < 2) max++;
	}

	/* Critical damage */
	if (do_stun)
	{
		if (dam > 140) return (6 + max);
		if (dam > 100) return (5 + max);
		if (dam > 70) return (4 + max);
		if (dam > 40) return (3 + max);
		if (dam > 21) return (2 + max);
		return (1 + max);
	}
	else
	{
		if (dam > 45) return (6 + max);
		if (dam > 33) return (5 + max);
		if (dam > 25) return (4 + max);
		if (dam > 18) return (3 + max);
		if (dam > 11) return (2 + max);
		return (1 + max);
	}
}





/*
 * Determine if a monster attack against the player succeeds.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match monster power against player armor.
 */
static int check_hit(int power, int level, int ac, int stun)
{
	int i, k;

	/* Percentile dice */
	k = randint0(100);

	if (stun && one_in_(2)) return FALSE;

	/* Hack -- Always miss or hit */
	if (k < 10) return (k < 5);

	/* Calculate the "attack quality" */
	i = (power + (level * 3));

	/* Power and Level compete against Armor */
	if ((i > 0) && (randint1(i) > ((ac * 3) / 4))) return (TRUE);

	/* Assume miss */
	return (FALSE);
}



static cptr desc_wail[] =
{
	"はさめざめと泣いた。",
	"はむせび声をあげた。",
	"は悲痛な声で泣いた。",
	"は嘆き悲しんだ。",
};


/*
 * Hack -- possible "insult" messages
 */
static cptr desc_insult[] =
{
#ifdef JP
	"があなたを侮辱した！",
	"があなたを軽蔑した！",
	"があなたを辱めた！",
	"があなたを穢した！",
	"があなたの回りで踊った！",
#else
	"insults you!",
	"gives you the finger!",
	"humiliates you!",
	"defiles you!",
	"dances around you!",
#endif
};



/*
 * Attack the player via physical attacks.
 */
bool make_attack_normal(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int ap_cnt;

	int i, k, tmp, rlev;
	int do_cut, do_stun;

	s32b gold;

	object_type *o_ptr;

	char o_name[MAX_NLEN];

	char m_name[80];

	char ddesc[80];

	bool blinked;
	bool touched = FALSE, fear = FALSE, alive = TRUE;
	bool explode = FALSE;
	bool resist_drain = FALSE;
	bool do_silly_attack = (one_in_(2) && p_ptr->image);
	int syouryaku = 0;
	int get_damage = 0;

	/* Not allowed to attack */
	if (r_ptr->flags1 & (RF1_NEVER_BLOW)) return (FALSE);

	/* ...nor if friendly */
	if (!is_hostile(m_ptr)) return FALSE;

	/* Extract the effective monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);


	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Get the "died from" information (i.e. "a kobold") */
	monster_desc(ddesc, m_ptr, 0x288);

	/* Assume no blink */
	blinked = FALSE;

	/* Scan through all four blows */
	for (ap_cnt = 0; ap_cnt < 4; ap_cnt++)
	{
		bool visible = FALSE;
		bool obvious = FALSE;

		int power = 0;
		int damage = 0;

		cptr act = NULL;

		/* Extract the attack infomation */
		int effect = r_ptr->blow[ap_cnt].effect;
		int method = r_ptr->blow[ap_cnt].method;
		int d_dice = r_ptr->blow[ap_cnt].d_dice;
		int d_side = r_ptr->blow[ap_cnt].d_side;

		/* Total armor */
		int ac = p_ptr->ac + p_ptr->to_a;

		cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];


		if (!m_ptr->r_idx) break;

		/* Hack -- no more attacks */
		if (!method) break;

		if (is_pet(m_ptr) && (r_ptr->flags1 & RF1_UNIQUE) && (method == RBM_EXPLODE))
		{
			method = RBM_HIT;
			d_dice /= 10;
		}

		/* Stop if player is dead or gone */
		if (!p_ptr->playing || p_ptr->is_dead) break;
		if (distance(py, px, m_ptr->fy, m_ptr->fx) > 1) break;

		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		if (method == RBM_SHOOT) continue;

		/* Extract visibility (before blink) */
		if (m_ptr->ml) visible = TRUE;

		/* Extract the attack "power" */
		switch (effect)
		{
			case RBE_HURT:      power = 60; break;
			case RBE_POISON:    power =  5; break;
			case RBE_UN_BONUS:  power = 20; break;
			case RBE_UN_POWER:  power = 15; break;
			case RBE_EAT_GOLD:  power =  5; break;
			case RBE_EAT_ITEM:  power =  5; break;
			case RBE_EAT_FOOD:  power =  5; break;
			case RBE_EAT_LITE:  power =  5; break;
			case RBE_ACID:      power =  0; break;
			case RBE_ELEC:      power = 10; break;
			case RBE_FIRE:      power = 10; break;
			case RBE_COLD:      power = 10; break;
			case RBE_BLIND:     power =  2; break;
			case RBE_CONFUSE:   power = 10; break;
			case RBE_TERRIFY:   power = 10; break;
			case RBE_PARALYZE:  power =  2; break;
			case RBE_LOSE_STR:  power =  0; break;
			case RBE_LOSE_DEX:  power =  0; break;
			case RBE_LOSE_CON:  power =  0; break;
			case RBE_LOSE_INT:  power =  0; break;
			case RBE_LOSE_WIS:  power =  0; break;
			case RBE_LOSE_CHR:  power =  0; break;
			case RBE_LOSE_ALL:  power =  2; break;
			case RBE_SHATTER:   power = 60; break;
			case RBE_EXP_10:    power =  5; break;
			case RBE_EXP_20:    power =  5; break;
			case RBE_EXP_40:    power =  5; break;
			case RBE_EXP_80:    power =  5; break;
			case RBE_DISEASE:   power =  5; break;
			case RBE_TIME:      power =  5; break;
			case RBE_EXP_VAMP:  power =  5; break;
			case RBE_DR_MANA:   power =  5; break;
			case RBE_SUPERHURT: power = 60; break;
			case RBE_STONE:     power =  2; break;
			case RBE_HOLY:      power = 30; break;
			case RBE_HELL:      power = 30; break;
		}


		/* Monster hits player */
		if (!effect || check_hit(power, rlev, ac, m_ptr->stunned))
		{
			/* Always disturbing */
			disturb(1, 0);


			/* Hack -- Apply "protection from evil" */
			if ((p_ptr->protevil > 0) &&
			    (r_ptr->flags3 & RF3_EVIL) &&
			    (p_ptr->lev >= rlev) &&
			    ((randint0(100) + p_ptr->lev) > 50))
			{
				/* Remember the Evil-ness */
				if (m_ptr->ml)
				{
					r_ptr->r_flags3 |= RF3_EVIL;
				}

				/* Message */
#ifdef JP
				if (syouryaku)
					msg_format("撃退した。");
				else
					msg_format("%^sは撃退された。", m_name);
				syouryaku = 1;/*２回目以降は省略 */
#else
				msg_format("%^s is repelled.", m_name);
#endif


				/* Hack -- Next attack */
				continue;
			}


			/* Assume no cut or stun */
			do_cut = do_stun = 0;

			/* Describe the attack method */
			switch (method)
			{
				case RBM_HIT:
				{
#ifdef JP
					act = "殴られた。";
#else
					act = "hits you.";
#endif

					do_cut = do_stun = 1;
					touched = TRUE;
					sound(SOUND_HIT);
					break;
				}

				case RBM_TOUCH:
				{
#ifdef JP
					act = "触られた。";
#else
					act = "touches you.";
#endif

					touched = TRUE;
					sound(SOUND_TOUCH);
					break;
				}

				case RBM_PUNCH:
				{
#ifdef JP
					act = "パンチされた。";
#else
					act = "punches you.";
#endif

					touched = TRUE;
					do_stun = 1;
					sound(SOUND_HIT);
					break;
				}

				case RBM_KICK:
				{
#ifdef JP
					act = "蹴られた。";
#else
					act = "kicks you.";
#endif

					touched = TRUE;
					do_stun = 1;
					sound(SOUND_HIT);
					break;
				}

				case RBM_CLAW:
				{
#ifdef JP
					act = "ひっかかれた。";
#else
					act = "claws you.";
#endif

					touched = TRUE;
					do_cut = 1;
					sound(SOUND_CLAW);
					break;
				}

				case RBM_BITE:
				{
#ifdef JP
					act = "噛まれた。";
#else
					act = "bites you.";
#endif

					do_cut = 1;
					touched = TRUE;
					sound(SOUND_BITE);
					break;
				}

				case RBM_STING:
				{
#ifdef JP
					act = "刺された。";
#else
					act = "stings you.";
#endif

					touched = TRUE;
					sound(SOUND_STING);
					break;
				}

				case RBM_SLASH:
				{
#ifdef JP
					act = "斬られた。";
#else
					act = "slashes you.";
#endif

					touched = TRUE;
					do_cut = 1;
					sound(SOUND_CLAW);
					break;
				}

				case RBM_BUTT:
				{
#ifdef JP
					act = "角で突かれた。";
#else
					act = "butts you.";
#endif

					do_stun = 1;
					touched = TRUE;
					sound(SOUND_HIT);
					break;
				}

				case RBM_CRUSH:
				{
#ifdef JP
					act = "体当たりされた。";
#else
					act = "crushes you.";
#endif

					do_stun = 1;
					touched = TRUE;
					sound(SOUND_CRUSH);
					break;
				}

				case RBM_ENGULF:
				{
#ifdef JP
					act = "飲み込まれた。";
#else
					act = "engulfs you.";
#endif

					touched = TRUE;
					sound(SOUND_CRUSH);
					break;
				}

				case RBM_CHARGE:
				{
#ifdef JP
					syouryaku = -1;
					act = "は請求書をよこした。";
#else
					act = "charges you.";
#endif

					touched = TRUE;
					sound(SOUND_BUY); /* Note! This is "charges", not "charges at". */
					break;
				}

				case RBM_CRAWL:
				{
#ifdef JP
					syouryaku = -1;
					act = "が体の上を這い回った。";
#else
					act = "crawls on you.";
#endif

					touched = TRUE;
					sound(SOUND_SLIME);
					break;
				}

				case RBM_DROOL:
				{
#ifdef JP
					act = "よだれをたらされた。";
#else
					act = "drools on you.";
#endif

					sound(SOUND_SLIME);
					break;
				}

				case RBM_SPIT:
				{
#ifdef JP
					act = "唾を吐かれた。";
#else
					act = "spits on you.";
#endif

					sound(SOUND_SLIME);
					break;
				}

				case RBM_EXPLODE:
				{
					syouryaku = -1;
#ifdef JP
					act = "は爆発した。";
#else
					act = "explodes.";
#endif

					explode = TRUE;
					break;
				}

				case RBM_GAZE:
				{
#ifdef JP
					act = "にらまれた。";
#else
					act = "gazes at you.";
#endif

					break;
				}

				case RBM_WAIL:
				{
					syouryaku = -1;
					act = desc_wail[randint0((sizeof desc_wail) / (sizeof (cptr)))];
					sound(SOUND_WAIL);
					break;
				}

				case RBM_SPORE:
				{
#ifdef JP
					act = "胞子を飛ばされた。";
#else
					act = "releases spores at you.";
#endif

					sound(SOUND_SLIME);
					break;
				}

				case RBM_BEG:
				{
#ifdef JP
					act = "金をせがまれた。";
#else
					act = "begs you for money.";
#endif

					sound(SOUND_MOAN);
					break;
				}

				case RBM_INSULT:
				{
					syouryaku = -1;
					act = desc_insult[randint0((sizeof desc_insult) / (sizeof (cptr)))];
					sound(SOUND_MOAN);
					break;
				}

				case RBM_SING:
				{
					syouryaku = -1;
					switch (effect)
					{
					case RBE_POISON:    act = "は毒々しい歌を歌った。"; break;
					case RBE_UN_BONUS:  act = "は劣化の歌を歌った。"; break;
					case RBE_UN_POWER:  act = "は魔力を吸い上げる歌を歌った。"; break;
					case RBE_EAT_LITE:  act = "は光をかき消す歌を歌った。"; break;
					case RBE_ACID:      act = "は酸の歌を歌った。"; break;
					case RBE_ELEC:      act = "は稲妻の歌を歌った。"; break;
					case RBE_FIRE:      act = "は火炎の歌を歌った。"; break;
					case RBE_COLD:      act = "は吹雪の歌を歌った。"; break;
					case RBE_BLIND:     act = "は暗闇の歌を歌った。"; break;
					case RBE_CONFUSE:   act = "は誘惑の歌を歌った。"; break;
					case RBE_TERRIFY:   act = "は恐怖の歌を歌った。"; break;
					case RBE_PARALYZE:  act = "は束縛の歌を歌った。"; break;
					case RBE_LOSE_STR:  act = "は脆弱の歌を歌った。"; break;
					case RBE_LOSE_INT:  act = "は無知の歌を歌った。"; break;
					case RBE_LOSE_WIS:  act = "は愚鈍の歌を歌った。"; break;
					case RBE_LOSE_DEX:  act = "は不器用の歌を歌った。"; break;
					case RBE_LOSE_CON:  act = "は不健康の歌を歌った。"; break;
					case RBE_LOSE_CHR:  act = "は醜悪の歌を歌った。"; break;
					case RBE_LOSE_ALL:  act = "は腐敗の歌を歌った。"; break;
					case RBE_SHATTER:   act = "は破壊の歌を歌った。"; break;
					case RBE_EXP_10:
					case RBE_EXP_20:
					case RBE_EXP_40:
					case RBE_EXP_80:
						act = "は冥界の歌を歌った。"; break;
					case RBE_DISEASE:   act = "は肉体を蝕む歌を歌った。"; break;
					case RBE_TIME:      act = "は時の流れを歪める歌を歌った。"; break;
					case RBE_EXP_VAMP:  act = "は生命力を奪う歌を歌った。"; break;
					case RBE_DR_MANA:   act = "は魔力を奪う歌を歌った。"; break;
					case RBE_STONE:     act = "は石化の歌を歌った。"; break;
					case RBE_HOLY:
						switch (randint1(3))
						{
						case 1: act = "は聖なる歌を歌った。"; break;
						case 2: act = "は厳かな歌を歌った。"; break;
						case 3: act = "は安らかな歌を歌った。"; break;
						}
						break;
					case RBE_HELL:
						switch (randint1(4))
						{
						case 1: act = "は邪悪な歌を歌った。"; break;
						case 2: act = "は禍々しい歌を歌った。"; break;
						case 3: act = "は汚らわしい歌を歌った。"; break;
						case 4: act = "は呪われた歌を歌った。"; break;
						}
						break;
					default:            act =  "は歌った。"; break;
					}

					sound(SOUND_SING);
					break;
				}
			}

			/* Message */
			if (act)
			{
				if (do_silly_attack)
				{
					syouryaku = -1;
					act = silly_attacks[randint0(MAX_SILLY_ATTACK)];
				}
#ifdef JP
				if (syouryaku == 0)
					msg_format("%^sに%s", m_name, act);
				else if (syouryaku == 1)
					msg_format("%s", act);
				else /* if (syouryaku == -1) */
					msg_format("%^s%s", m_name, act);
				syouryaku = 1;/*２回目以降は省略 */
#else
				msg_format("%^s %s%s", m_name, act, do_silly_attack ? " you." : "");
#endif
			}

			/* Hack -- assume all attacks are obvious */
			obvious = TRUE;

			/* Roll out the damage */
			damage = damroll(d_dice, d_side);
			if (m_ptr->melt_weapon)
			{
				damage -= 10;
				if (damage < 0) damage = 0;
			}
			damage = modify_dam_by_elem(m_idx, 0, damage, GF_MISSILE, MODIFY_ELEM_MODE_MELEE);

			/*
			 * Skip the effect when exploding, since the explosion
			 * already causes the effect.
			 */
			if (explode)
				damage = 0;
			/* Apply appropriate damage */
			switch (effect)
			{
				case 0:
				{
					/* Hack -- Assume obvious */
					obvious = TRUE;

					/* Hack -- No damage */
					damage = 0;

					break;
				}

				case RBE_SUPERHURT:
				{
					if ((randint1(rlev*2+300) > (ac+200)) || one_in_(13)) {
						int tmp_damage = damage-(damage*((ac < 200) ? ac : 200) / 250);
#ifdef JP
						msg_print("痛恨の一撃！");
#else
						msg_print("It was a critical hit!");
#endif

						tmp_damage = MAX(damage, tmp_damage*2);

						/* Take damage */
						ACTIVATE_MULTISHADOW();
						get_damage += take_hit(DAMAGE_ATTACK, tmp_damage, ddesc);
						STOP_MULTISHADOW();
						break;
					}
				}
				case RBE_HURT:
				{
					/* Obvious */
					obvious = TRUE;

					/* Hack -- Player armor reduces total damage */
					damage -= (damage * ((ac < 200) ? ac : 200) / 250);

					/* Take damage */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);
					STOP_MULTISHADOW();

					break;
				}

				case RBE_POISON:
				{
					if (explode) break;

					ACTIVATE_MULTISHADOW();

					/* Take "poison" effect */
					if (!(p_ptr->resist_pois || p_ptr->oppose_pois) && !IS_MULTISHADOW(0))
					{
						if (set_poisoned(p_ptr->poisoned + randint1(rlev) + 5))
						{
							obvious = TRUE;
						}
					}

					/* Take some damage */
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);
					STOP_MULTISHADOW();

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_POIS);

					break;
				}

				case RBE_UN_BONUS:
				{
					if (explode) break;

					ACTIVATE_MULTISHADOW();

					/* Allow complete resist */
					if (!p_ptr->resist_disen && !IS_MULTISHADOW(0))
					{
						/* Apply disenchantment */
						if (apply_disenchant()) obvious = TRUE;
					}

					/* Take some damage */
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);
					STOP_MULTISHADOW();

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_DISEN);

					break;
				}

				case RBE_UN_POWER:
				{
					/* Take some damage */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead || IS_MULTISHADOW(0))
					{
						STOP_MULTISHADOW();
						break;
					}
					STOP_MULTISHADOW();

					/* Find an item */
					for (k = 0; k < 10; k++)
					{
						/* Pick an item */
						i = randint0(INVEN_PACK);

						/* Obtain the item */
						o_ptr = &inventory[i];

						/* Skip non-objects */
						if (!o_ptr->k_idx) continue;

						/* Drain charged wands/staffs */
						if (((o_ptr->tval == TV_STAFF) ||
						     (o_ptr->tval == TV_WAND)) &&
						    (o_ptr->pval))
						{
							/* Calculate healed hitpoints */
							int heal = rlev * o_ptr->pval;
							if (o_ptr->tval == TV_STAFF)
								heal *= o_ptr->number;

							/* Don't heal more than max hp */
							heal = MIN(heal, m_ptr->maxhp - m_ptr->hp);

							/* Message */
#ifdef JP
							msg_print("ザックからエネルギーが吸い取られた！");
#else
							msg_print("Energy drains from your pack!");
#endif


							/* Obvious */
							obvious = TRUE;

							/* Heal the monster */
							m_ptr->hp += heal;

							/* Redraw (later) if needed */
							if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
							if (p_ptr->riding == m_idx) p_ptr->redraw |= (PR_UHEALTH);

							/* Uncharge */
							o_ptr->pval = 0;

							/* Combine / Reorder the pack */
							p_ptr->notice |= (PN_COMBINE | PN_REORDER);

							/* Window stuff */
							p_ptr->window |= (PW_INVEN);

							/* Done */
							break;
						}
					}

					break;
				}

				case RBE_EAT_GOLD:
				{
					/* Take some damage */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					/* Confused monsters cannot steal successfully. -LM-*/
					if (m_ptr->confused || p_ptr->is_dead || IS_MULTISHADOW(0))
					{
						STOP_MULTISHADOW();
						break;
					}
					STOP_MULTISHADOW();

					/* Obvious */
					obvious = TRUE;

					/* Saving throw (unless paralyzed) based on dex and level */
					if (!p_ptr->paralyzed &&
					    (randint0(100) < (adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
					                      p_ptr->lev)))
					{
						/* Saving throw message */
#ifdef JP
						msg_print("しかし素早く財布を守った！");
#else
						msg_print("You quickly protect your money pouch!");
#endif


						/* Occasional blink anyway */
						if (randint0(3)) blinked = TRUE;
					}

					/* Eat gold */
					else
					{
						gold = (p_ptr->au_sum / 10) + randint1(25);
						if (gold < 2) gold = 2;
						if (gold > 5000) gold = (p_ptr->au_sum / 20) + randint1(3000);
						if (gold > p_ptr->au_sum) gold = p_ptr->au_sum;
						p_ptr->au_sum -= gold;
						if (gold <= 0)
						{
#ifdef JP
							msg_print("しかし何も盗まれなかった。");
#else
							msg_print("Nothing was stolen.");
#endif

						}
						else if (p_ptr->au_sum)
						{
#ifdef JP
							msg_print("財布が軽くなった気がする。");
							msg_format("$%ld のお金が盗まれた！", (long)gold);
#else
							msg_print("Your purse feels lighter.");
							msg_format("%ld coins were stolen!", (long)gold);
#endif
						}
						else
						{
#ifdef JP
							msg_print("財布が軽くなった気がする。");
							msg_print("お金が全部盗まれた！");
#else
							msg_print("Your purse feels lighter.");
							msg_print("All of your coins were stolen!");
#endif
						}

						/* Update gold */
						p_ptr->update |= (PU_GOLD);

						/* Redraw gold */
						p_ptr->redraw |= (PR_GOLD);

						/* Window stuff */
						p_ptr->window |= (PW_PLAYER);

						/* Blink away */
						blinked = TRUE;
					}

					break;
				}

				case RBE_EAT_ITEM:
				{
					/* Take some damage */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					/* Confused monsters cannot steal successfully. -LM-*/
					if (m_ptr->confused || p_ptr->is_dead || IS_MULTISHADOW(0))
					{
						STOP_MULTISHADOW();
						break;
					}
					STOP_MULTISHADOW();

					/* Saving throw (unless paralyzed) based on dex and level */
					if (!p_ptr->paralyzed &&
					    (randint0(100) < (adj_dex_safe[p_ptr->stat_ind[A_DEX]] +
					                      p_ptr->lev)))
					{
						/* Saving throw message */
#ifdef JP
						msg_print("しかしあわててザックを取り返した！");
#else
						msg_print("You grab hold of your backpack!");
#endif


						/* Occasional "blink" anyway */
						blinked = TRUE;

						/* Obvious */
						obvious = TRUE;

						/* Done */
						break;
					}

					/* Find an item */
					for (k = 0; k < 10; k++)
					{
						s16b o_idx;

						/* Pick an item */
						i = randint0(INVEN_PACK);

						/* Obtain the item */
						o_ptr = &inventory[i];

						/* Skip non-objects */
						if (!o_ptr->k_idx) continue;

						/* Skip artifacts */
						if (artifact_p(o_ptr) || o_ptr->art_name) continue;

						/* Get a description */
						object_desc(o_name, o_ptr, FALSE, 3);

						/* Message */
#ifdef JP
						msg_format("%s(%c)を%s盗まれた！",
						           o_name, index_to_label(i),
						           ((o_ptr->number > 1) ? "一つ" : ""));
#else
						msg_format("%sour %s (%c) was stolen!",
						           ((o_ptr->number > 1) ? "One of y" : "Y"),
						           o_name, index_to_label(i));
#endif


						/* Make an object */
						o_idx = o_pop();
						
						/* Success */
						if (o_idx)
						{
							object_type *j_ptr;

							/* Get new object */
							j_ptr = &o_list[o_idx];

							/* Copy object */
							object_copy(j_ptr, o_ptr);

							/* Modify number */
							j_ptr->number = 1;

							/* Hack -- If a rod or wand, allocate total
							 * maximum timeouts or charges between those
							 * stolen and those missed. -LM-
							 */
							if ((o_ptr->tval == TV_ROD) || (o_ptr->tval == TV_WAND))
							{
								j_ptr->pval = o_ptr->pval / o_ptr->number;
								o_ptr->pval -= j_ptr->pval;
							}

							/* Forget mark */
							j_ptr->marked = 0;

							/* Memorize monster */
							j_ptr->held_m_idx = m_idx;

							/* Build stack */
							j_ptr->next_o_idx = m_ptr->hold_o_idx;

							/* Build stack */
							m_ptr->hold_o_idx = o_idx;
						}

						/* Steal the items */
						inven_item_increase(i, -1);
						inven_item_optimize(i);

						/* Obvious */
						obvious = TRUE;

						/* Blink away */
						blinked = TRUE;

						/* Done */
						break;
					}

					break;
				}

				case RBE_EAT_FOOD:
				{
					/* Take some damage */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead || IS_MULTISHADOW(0))
					{
						STOP_MULTISHADOW();
						break;
					}
					STOP_MULTISHADOW();

					/* Steal some food */
					for (k = 0; k < 10; k++)
					{
						/* Pick an item from the pack */
						i = randint0(INVEN_PACK);

						/* Get the item */
						o_ptr = &inventory[i];

						/* Skip non-objects */
						if (!o_ptr->k_idx) continue;

						/* Skip non-food objects */
						if ((o_ptr->tval != TV_FOOD) && !((o_ptr->tval == TV_CORPSE) && (o_ptr->sval))) continue;

						/* Get a description */
						object_desc(o_name, o_ptr, FALSE, 0);

						/* Message */
#ifdef JP
						msg_format("%s(%c)を%s食べられてしまった！",
						          o_name, index_to_label(i),
						          ((o_ptr->number > 1) ? "一つ" : ""));
#else
						msg_format("%sour %s (%c) was eaten!",
						           ((o_ptr->number > 1) ? "One of y" : "Y"),
						           o_name, index_to_label(i));
#endif


						/* Steal the items */
						inven_item_increase(i, -1);
						inven_item_optimize(i);

						/* Obvious */
						obvious = TRUE;

						/* Done */
						break;
					}

					break;
				}

				case RBE_EAT_LITE:
				{
					/* Access the lite */
					o_ptr = &inventory[INVEN_LITE];

					/* Take some damage */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead || IS_MULTISHADOW(0))
					{
						STOP_MULTISHADOW();
						break;
					}
					STOP_MULTISHADOW();

					/* Drain fuel */
					if ((o_ptr->xtra4 > 0) && (!artifact_p(o_ptr)))
					{
						/* Reduce fuel */
						o_ptr->xtra4 -= (250 + randint1(250));
						if (o_ptr->xtra4 < 1) o_ptr->xtra4 = 1;

						/* Notice */
						if (!p_ptr->blind)
						{
#ifdef JP
							msg_print("明かりが暗くなってしまった。");
#else
							msg_print("Your light dims.");
#endif

							obvious = TRUE;
						}

						/* Window stuff */
						p_ptr->window |= (PW_EQUIP);
					}

					break;
				}

				case RBE_ACID:
				{
					if (explode) break;

					/* Obvious */
					obvious = TRUE;

					/* Message */
#ifdef JP
					msg_print("酸を浴びせられた！");
#else
					msg_print("You are covered in acid!");
#endif


					/* Special damage */
					get_damage += acid_dam(damage, ddesc);

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_ACID);

					break;
				}

				case RBE_ELEC:
				{
					if (explode) break;

					/* Obvious */
					obvious = TRUE;

					/* Message */
#ifdef JP
					msg_print("電撃を浴びせられた！");
#else
					msg_print("You are struck by electricity!");
#endif


					/* Special damage */
					get_damage += elec_dam(damage, ddesc);

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_ELEC);

					break;
				}

				case RBE_FIRE:
				{
					if (explode) break;

					/* Obvious */
					obvious = TRUE;

					/* Message */
#ifdef JP
					msg_print("全身が炎に包まれた！");
#else
					msg_print("You are enveloped in flames!");
#endif


					/* Special damage */
					get_damage += fire_dam(damage, ddesc);

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_FIRE);

					break;
				}

				case RBE_COLD:
				{
					if (explode) break;

					/* Obvious */
					obvious = TRUE;

					/* Message */
#ifdef JP
					msg_print("全身が冷気で覆われた！");
#else
					msg_print("You are covered with frost!");
#endif


					/* Special damage */
					get_damage += cold_dam(damage, ddesc);

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_COLD);

					break;
				}

				case RBE_BLIND:
				{
					/* Take damage */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead)
					{
						STOP_MULTISHADOW();
						break;
					}

					/* Increase "blind" */
					if (!p_ptr->resist_blind && !IS_MULTISHADOW(0))
					{
						if (set_blind(p_ptr->blind + 10 + randint1(rlev)))
						{
							obvious = TRUE;
						}
					}
					STOP_MULTISHADOW();

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_BLIND);

					break;
				}

				case RBE_CONFUSE:
				{
					if (explode) break;

					/* Take damage */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead)
					{
						STOP_MULTISHADOW();
						break;
					}

					/* Increase "confused" */
					if (!p_ptr->resist_conf && !IS_MULTISHADOW(0))
					{
						if (set_confused(p_ptr->confused + 3 + randint1(rlev)))
						{
							obvious = TRUE;
						}
					}
					STOP_MULTISHADOW();

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_CONF);

					break;
				}

				case RBE_TERRIFY:
				{
					/* Take damage */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead)
					{
						STOP_MULTISHADOW();
						break;
					}

					/* Increase "afraid" */
					if (IS_MULTISHADOW(0))
					{
						/* Nothing */
					}
					if (p_ptr->resist_fear)
					{
#ifdef JP
						msg_print("しかし恐怖に侵されなかった！");
#else
						msg_print("You stand your ground!");
#endif

						obvious = TRUE;
					}
					else if (randint0(100 + r_ptr->level/2) < p_ptr->skill_sav)
					{
#ifdef JP
						msg_print("しかし恐怖に侵されなかった！");
#else
						msg_print("You stand your ground!");
#endif

						obvious = TRUE;
					}
					else
					{
						if (set_afraid(p_ptr->afraid + 3 + randint1(rlev)))
						{
							obvious = TRUE;
						}
					}
					STOP_MULTISHADOW();

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_FEAR);

					break;
				}

				case RBE_PARALYZE:
				{
					/* Take damage */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead)
					{
						STOP_MULTISHADOW();
						break;
					}

					/* Increase "paralyzed" */
					if (IS_MULTISHADOW(0))
					{
						/* Nothing */
					}
					if (p_ptr->free_act)
					{
#ifdef JP
						msg_print("しかし効果がなかった！");
#else
						msg_print("You are unaffected!");
#endif

						obvious = TRUE;
					}
					else if (randint0(100 + r_ptr->level/2) < p_ptr->skill_sav)
					{
#ifdef JP
						msg_print("しかし効力を跳ね返した！");
#else
						msg_print("You resist the effects!");
#endif

						obvious = TRUE;
					}
					else
					{
						if (!p_ptr->paralyzed)
						{
							if (set_paralyzed(3 + randint1(rlev)))
							{
								obvious = TRUE;
							}
						}
					}
					STOP_MULTISHADOW();

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_FREE);

					break;
				}

				case RBE_LOSE_STR:
				{
					/* Damage (physical) */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead || IS_MULTISHADOW(0))
					{
						STOP_MULTISHADOW();
						break;
					}
					STOP_MULTISHADOW();

					/* Damage (stat) */
					if (do_dec_stat(A_STR)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_INT:
				{
					/* Damage (physical) */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead || IS_MULTISHADOW(0))
					{
						STOP_MULTISHADOW();
						break;
					}
					STOP_MULTISHADOW();

					/* Damage (stat) */
					if (do_dec_stat(A_INT)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_WIS:
				{
					/* Damage (physical) */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead || IS_MULTISHADOW(0))
					{
						STOP_MULTISHADOW();
						break;
					}
					STOP_MULTISHADOW();

					/* Damage (stat) */
					if (do_dec_stat(A_WIS)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_DEX:
				{
					/* Damage (physical) */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead || IS_MULTISHADOW(0))
					{
						STOP_MULTISHADOW();
						break;
					}
					STOP_MULTISHADOW();

					/* Damage (stat) */
					if (do_dec_stat(A_DEX)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_CON:
				{
					/* Damage (physical) */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead || IS_MULTISHADOW(0))
					{
						STOP_MULTISHADOW();
						break;
					}
					STOP_MULTISHADOW();

					/* Damage (stat) */
					if (do_dec_stat(A_CON)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_CHR:
				{
					/* Damage (physical) */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead || IS_MULTISHADOW(0))
					{
						STOP_MULTISHADOW();
						break;
					}
					STOP_MULTISHADOW();

					/* Damage (stat) */
					if (do_dec_stat(A_CHR)) obvious = TRUE;

					break;
				}

				case RBE_LOSE_ALL:
				{
					/* Damage (physical) */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead || IS_MULTISHADOW(0))
					{
						STOP_MULTISHADOW();
						break;
					}
					STOP_MULTISHADOW();

					/* Damage (stats) */
					if (do_dec_stat(A_STR)) obvious = TRUE;
					if (do_dec_stat(A_DEX)) obvious = TRUE;
					if (do_dec_stat(A_CON)) obvious = TRUE;
					if (do_dec_stat(A_INT)) obvious = TRUE;
					if (do_dec_stat(A_WIS)) obvious = TRUE;
					if (do_dec_stat(A_CHR)) obvious = TRUE;

					break;
				}

				case RBE_SHATTER:
				{
					/* Obvious */
					obvious = TRUE;

					/* Hack -- Reduce damage based on the player armor class */
					damage -= (damage * ((ac < 200) ? ac : 200) / 250);

					/* Take damage */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);
					STOP_MULTISHADOW();

					/* Radius 8 earthquake centered at the monster */
					if (damage > 23 || explode)
					{
						earthquake(m_ptr->fy, m_ptr->fx, 8);
					}

					break;
				}

				case RBE_EXP_10:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead || IS_MULTISHADOW(0))
					{
						STOP_MULTISHADOW();
						break;
					}
					STOP_MULTISHADOW();

					if (p_ptr->hold_life && (randint0(100) < 95))
					{
#ifdef JP
						msg_print("しかし自己の生命力を守りきった！");
#else
						msg_print("You keep hold of your life force!");
#endif

					}
					else
					{
						s32b cd = damroll(10, 6) + (cexp_ptr->cexp / 100) * MON_DRAIN_LIFE;
						s32b rd = damroll(10, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;
						if (p_ptr->hold_life)
						{
#ifdef JP
							msg_print("生命力を少し吸い取られた気がする！");
#else
							msg_print("You feel your life slipping away!");
#endif

							lose_class_exp(cd / 10);
							lose_racial_exp(rd / 10);
						}
						else
						{
#ifdef JP
							msg_print("生命力が体から吸い取られた気がする！");
#else
							msg_print("You feel your life draining away!");
#endif

							lose_class_exp(cd);
							lose_racial_exp(rd);
						}
					}
					break;
				}

				case RBE_EXP_20:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead || IS_MULTISHADOW(0))
					{
						STOP_MULTISHADOW();
						break;
					}
					STOP_MULTISHADOW();

					if (p_ptr->hold_life && (randint0(100) < 90))
					{
#ifdef JP
						msg_print("しかし自己の生命力を守りきった！");
#else
						msg_print("You keep hold of your life force!");
#endif

					}
					else
					{
						s32b cd = damroll(20, 6) + (cexp_ptr->cexp / 100) * MON_DRAIN_LIFE;
						s32b rd = damroll(20, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;
						if (p_ptr->hold_life)
						{
#ifdef JP
							msg_print("生命力を少し吸い取られた気がする！");
#else
							msg_print("You feel your life slipping away!");
#endif

							lose_class_exp(cd / 10);
							lose_racial_exp(rd / 10);
						}
						else
						{
#ifdef JP
							msg_print("生命力が体から吸い取られた気がする！");
#else
							msg_print("You feel your life draining away!");
#endif

							lose_class_exp(cd);
							lose_racial_exp(rd);
						}
					}
					break;
				}

				case RBE_EXP_40:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead || IS_MULTISHADOW(0))
					{
						STOP_MULTISHADOW();
						break;
					}
					STOP_MULTISHADOW();

					if (p_ptr->hold_life && (randint0(100) < 75))
					{
#ifdef JP
						msg_print("しかし自己の生命力を守りきった！");
#else
						msg_print("You keep hold of your life force!");
#endif

					}
					else
					{
						s32b cd = damroll(40, 6) + (cexp_ptr->cexp / 100) * MON_DRAIN_LIFE;
						s32b rd = damroll(40, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;
						if (p_ptr->hold_life)
						{
#ifdef JP
							msg_print("生命力を少し吸い取られた気がする！");
#else
							msg_print("You feel your life slipping away!");
#endif

							lose_class_exp(cd / 10);
							lose_racial_exp(rd / 10);
						}
						else
						{
#ifdef JP
							msg_print("生命力が体から吸い取られた気がする！");
#else
							msg_print("You feel your life draining away!");
#endif

							lose_class_exp(cd);
							lose_racial_exp(rd);
						}
					}
					break;
				}

				case RBE_EXP_80:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead || IS_MULTISHADOW(0))
					{
						STOP_MULTISHADOW();
						break;
					}
					STOP_MULTISHADOW();

					if (p_ptr->hold_life && (randint0(100) < 50))
					{
#ifdef JP
						msg_print("しかし自己の生命力を守りきった！");
#else
						msg_print("You keep hold of your life force!");
#endif

					}
					else
					{
						s32b cd = damroll(80, 6) + (cexp_ptr->cexp / 100) * MON_DRAIN_LIFE;
						s32b rd = damroll(80, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;
						if (p_ptr->hold_life)
						{
#ifdef JP
							msg_print("生命力を少し吸い取られた気がする！");
#else
							msg_print("You feel your life slipping away!");
#endif

							lose_class_exp(cd / 10);
							lose_racial_exp(rd / 10);
						}
						else
						{
#ifdef JP
							msg_print("生命力が体から吸い取られた気がする！");
#else
							msg_print("You feel your life draining away!");
#endif

							lose_class_exp(cd);
							lose_racial_exp(rd);
						}
					}
					break;
				}

				case RBE_DISEASE:
				{
					/* Take some damage */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead || IS_MULTISHADOW(0))
					{
						STOP_MULTISHADOW();
						break;
					}
					STOP_MULTISHADOW();

					/* Take "poison" effect */
					if (!(p_ptr->resist_pois || p_ptr->oppose_pois))
					{
						if (set_poisoned(p_ptr->poisoned + randint1(rlev) + 5))
						{
							obvious = TRUE;
						}
					}

					/* Damage CON (10% chance)*/
					if (randint1(100) < 11)
					{
						/* 1% chance for perm. damage */
						bool perm = one_in_(10);
						if (dec_stat(A_CON, randint1(10), perm))
						{
#ifdef JP
							msg_print("病があなたを蝕んでいる気がする。");
#else
							msg_print("You feel strange sickness.");
#endif

							obvious = TRUE;
						}
					}

					break;
				}
				case RBE_TIME:
				{
					if (explode) break;

					ACTIVATE_MULTISHADOW();
					if (!p_ptr->resist_time && !IS_MULTISHADOW(0))
					{
						switch (randint1(10))
						{
							case 1: case 2: case 3: case 4: case 5:
							{
#ifdef JP
								msg_print("人生が逆戻りした気がする。");
#else
								msg_print("You feel life has clocked back.");
#endif

								lose_class_exp(100 + (cexp_ptr->cexp / 100) * MON_DRAIN_LIFE);
								lose_racial_exp(100 + (p_ptr->exp / 100) * MON_DRAIN_LIFE);
								break;
							}

							case 6: case 7: case 8: case 9:
							{
								int stat = randint0(A_MAX);

								switch (stat)
								{
#ifdef JP
									case A_STR: act = "強く"; break;
									case A_INT: act = "聡明で"; break;
									case A_WIS: act = "賢明で"; break;
									case A_DEX: act = "器用で"; break;
									case A_CON: act = "健康で"; break;
									case A_CHR: act = "美しく"; break;
#else
									case A_STR: act = "strong"; break;
									case A_INT: act = "bright"; break;
									case A_WIS: act = "wise"; break;
									case A_DEX: act = "agile"; break;
									case A_CON: act = "hale"; break;
									case A_CHR: act = "beautiful"; break;
#endif

								}

#ifdef JP
								msg_format("あなたは以前ほど%sなくなってしまった...。", act);
#else
								msg_format("You're not as %s as you used to be...", act);
#endif


								p_ptr->stat_cur[stat] = (p_ptr->stat_cur[stat] * 3) / 4;
								if (p_ptr->stat_cur[stat] < 3) p_ptr->stat_cur[stat] = 3;
								p_ptr->update |= (PU_BONUS);
								break;
							}

							case 10:
							{
#ifdef JP
								msg_print("あなたは以前ほど力強くなくなってしまった...。");
#else
								msg_print("You're not as powerful as you used to be...");
#endif


								for (k = 0; k < A_MAX; k++)
								{
									p_ptr->stat_cur[k] = (p_ptr->stat_cur[k] * 7) / 8;
									if (p_ptr->stat_cur[k] < 3) p_ptr->stat_cur[k] = 3;
								}
								p_ptr->update |= (PU_BONUS);
								break;
							}
						}
					}

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_TIME);

					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);
					STOP_MULTISHADOW();

					break;
				}
				case RBE_EXP_VAMP:
				{
					/* Obvious */
					obvious = TRUE;

					/* Take damage */
					ACTIVATE_MULTISHADOW();
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);

					if (p_ptr->is_dead || IS_MULTISHADOW(0))
					{
						STOP_MULTISHADOW();
						break;
					}
					STOP_MULTISHADOW();

					if (p_ptr->hold_life && (randint0(100) < 50))
					{
#ifdef JP
						msg_print("しかし自己の生命力を守りきった！");
#else
						msg_print("You keep hold of your life force!");
#endif

						resist_drain = TRUE;
					}
					else
					{
						s32b cd = damroll(60, 6) + (cexp_ptr->cexp / 100) * MON_DRAIN_LIFE;
						s32b rd = damroll(60, 6) + (p_ptr->exp / 100) * MON_DRAIN_LIFE;
						if (p_ptr->hold_life)
						{
#ifdef JP
							msg_print("生命力が少し体から抜け落ちた気がする！");
#else
							msg_print("You feel your life slipping away!");
#endif

							lose_class_exp(cd / 10);
							lose_racial_exp(rd / 10);
						}
						else
						{
#ifdef JP
							msg_print("生命力が体から吸い取られた気がする！");
#else
							msg_print("You feel your life draining away!");
#endif

							lose_class_exp(cd);
							lose_racial_exp(rd);
						}
					}

					/* Heal the attacker? */
					if (!(rp_ptr->r_flags & PRF_UNDEAD) && !(cp_ptr->c_flags & PCF_UNDEAD) &&
					    (damage > 5) && !resist_drain)
					{
						bool did_heal = FALSE;

						if (m_ptr->hp < m_ptr->maxhp) did_heal = TRUE;

						/* Heal */
						m_ptr->hp += damroll(4, damage / 6);
						if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

						/* Redraw (later) if needed */
						if (p_ptr->health_who == m_idx) p_ptr->redraw |= (PR_HEALTH);
						if (p_ptr->riding == m_idx) p_ptr->redraw |= (PR_UHEALTH);

						/* Special message */
						if ((m_ptr->ml) && (did_heal))
						{
#ifdef JP
							msg_format("%sは体力を回復したようだ。", m_name);
#else
							msg_format("%^s appears healthier.", m_name);
#endif

						}
					}

					break;
				}
				case RBE_DR_MANA:
				{
					/* Obvious */
					obvious = TRUE;

					ACTIVATE_MULTISHADOW();
					if (IS_MULTISHADOW(0))
					{
#ifdef JP
						msg_print("攻撃は幻影に命中し、あなたには届かなかった。");
#else
						msg_print("The attack hits Shadow, you are unharmed!");
#endif
					}
					else
					{
						do_cut = 0;

						/* Take damage */
						p_ptr->csp -= damage;
						if (p_ptr->csp < 0)
						{
							p_ptr->csp = 0;
							p_ptr->csp_frac = 0;
						}

						p_ptr->redraw |= (PR_MANA);
					}

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_MANA);

					STOP_MULTISHADOW();
					break;
				}
				case RBE_STONE:
				{
					if (explode) break;

					ACTIVATE_MULTISHADOW();
					if (IS_MULTISHADOW(0))
					{
						/* Nothing */
					}
					else if (p_ptr->resist_stone)
					{
#ifdef JP
						msg_print("しかし効果がなかった！");
#else
						msg_print("You are unaffected!");
#endif

						obvious = TRUE;
					}
					else if (randint0(100 + r_ptr->level/2) < p_ptr->skill_sav)
					{
#ifdef JP
						msg_print("しかし効力を跳ね返した！");
#else
						msg_print("You resist the effects!");
#endif

						obvious = TRUE;
					}
					else if (p_ptr->resist_acid || p_ptr->oppose_acid || p_ptr->immune_acid || p_ptr->resist_shard)
					{
						if (!p_ptr->stoning) set_stoning(1);
					}
					else
					{
#ifdef JP
						msg_print("生きたまま石像になっていく...");
#else
						msg_print("You are into a living statue...");
#endif
						p_ptr->is_dead |= DEATH_STONED;
					}

					/* Learn about the player */
					update_smart_learn(m_idx, DRS_STONE);

					/* Take damage */
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);
					STOP_MULTISHADOW();

					break;
				}
				case RBE_HOLY:
				{
					if (explode) break;

					ACTIVATE_MULTISHADOW();
					if ((get_your_alignment_gne() == ALIGN_GNE_EVIL) || p_ptr->ogre_equip || (prace_is_(RACE_GHOST)) || (prace_is_(RACE_SKELETON)))
					{
#ifdef JP
						if (!IS_MULTISHADOW(0)) msg_print("ひどい痛手を受けた！");
#else
						if (!IS_MULTISHADOW(0)) msg_print("You are hit hard!");
#endif
						if ((prace_is_(RACE_GHOST)) || (prace_is_(RACE_SKELETON)) || (get_your_alignment_gne() == ALIGN_GNE_EVIL)) damage = damage * 7 / 5;
						if (p_ptr->ogre_equip) damage *= 2;
					}

					/* Hack -- Player armor reduces total damage */
					damage -= (damage * ((ac < 200) ? ac : 200) / 250);

					/* Take some damage */
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);
					STOP_MULTISHADOW();

					break;
				}
				case RBE_HELL:
				{
					if (explode) break;

					ACTIVATE_MULTISHADOW();
					if ((get_your_alignment_gne() == ALIGN_GNE_GOOD) && !(prace_is_(RACE_GHOST)) && !(prace_is_(RACE_SKELETON)))
					{
#ifdef JP
						if (!IS_MULTISHADOW(0)) msg_print("ひどい痛手を受けた！");
#else
						if (!IS_MULTISHADOW(0)) msg_print("You are hit hard!");
#endif
						damage = damage * 7 / 5;
					}

					/* Hack -- Player armor reduces total damage */
					damage -= (damage * ((ac < 200) ? ac : 200) / 250);

					/* Take some damage */
					get_damage += take_hit(DAMAGE_ATTACK, damage, ddesc);
					STOP_MULTISHADOW();

					break;
				}
			}

			/* Hack -- only one of cut or stun */
			if (do_cut && do_stun)
			{
				/* Cancel cut */
				if (randint0(100) < 50)
				{
					do_cut = 0;
				}

				/* Cancel stun */
				else
				{
					do_stun = 0;
				}
			}

			/* Handle cut */
			if (do_cut)
			{
				int k = 0;

				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d_dice, d_side, damage, FALSE);

				/* Roll for damage */
				switch (tmp)
				{
					case 0: k = 0; break;
					case 1: k = randint1(5); break;
					case 2: k = randint1(5) + 5; break;
					case 3: k = randint1(20) + 20; break;
					case 4: k = randint1(50) + 50; break;
					case 5: k = randint1(100) + 100; break;
					case 6: k = 300; break;
					default: k = 500; break;
				}

				/* Apply the cut */
				if (k) (void)set_cut(p_ptr->cut + k);
			}

			/* Handle stun */
			if (do_stun)
			{
				int k = 0;

				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d_dice, d_side, damage, TRUE);

				/* Roll for damage */
				switch (tmp)
				{
					case 0: k = 0; break;
					case 1: k = randint1(5); break;
					case 2: k = randint1(5) + 10; break;
					case 3: k = randint1(10) + 20; break;
					case 4: k = randint1(15) + 30; break;
					case 5: k = randint1(20) + 40; break;
					case 6: k = 80; break;
					default: k = 150; break;
				}

				/* Apply the stun */
				if (k) (void)set_stun(p_ptr->stun + k);
			}

			if (explode)
			{
				sound(SOUND_EXPLODE);

				if (mon_take_hit(m_idx, m_ptr->hp + 1, &fear, NULL, FALSE))
				{
					blinked = FALSE;
					alive = FALSE;
				}
			}

			if (touched)
			{
				if ((r_ptr->flags2 & RF2_VAMPIRE) && !p_ptr->infected && 
					(p_ptr->psex == SEX_MALE) && (!(rp_ptr->r_flags & PRF_UNDEAD)) && (!(cp_ptr->c_flags & PCF_UNDEAD)) && 
					(!prace_is_(RACE_FAIRY)) && (!prace_is_(RACE_GREMLIN)) && (!prace_is_(RACE_PUMPKINHEAD)))
					
				{
					if (p_ptr->lev < randint1(damage + r_ptr->level))
					{
#ifdef JP
						msg_format("傷口が焼けるように熱い！");
#else
#endif
						p_ptr->infected = TRUE;
					}
				}

				if (p_ptr->sh_fire && alive && !p_ptr->is_dead)
				{
					if (!(r_ptr->flagsr & RFR_RES_FIRE))
					{
						int dam = damroll(2, 6);

						/* Modify the damage */
						dam = mon_damage_mod(m_ptr, dam, FALSE);
						dam = modify_dam_by_elem(0, m_idx, dam, GF_FIRE, MODIFY_ELEM_MODE_MELEE);

#ifdef JP
						msg_format("%^sは突然熱くなった！", m_name);
						if (mon_take_hit(m_idx, dam, &fear,
						    "は灰の山になった。", FALSE))
#else
						msg_format("%^s is suddenly very hot!", m_name);

						if (mon_take_hit(m_idx, dam, &fear,
						    " turns into a pile of ash.", FALSE))
#endif

						{
							blinked = FALSE;
							alive = FALSE;
						}
					}
					else
					{
						if (m_ptr->ml)
							r_ptr->r_flagsr |= RFR_RES_FIRE;
					}
				}

				if (p_ptr->sh_elec && alive && !p_ptr->is_dead)
				{
					if (!(r_ptr->flagsr & RFR_RES_ELEC))
					{
						int dam = damroll(2, 6);

						/* Modify the damage */
						dam = mon_damage_mod(m_ptr, dam, FALSE);
						dam = modify_dam_by_elem(0, m_idx, dam, GF_ELEC, MODIFY_ELEM_MODE_MELEE);

#ifdef JP
						msg_format("%^sは電撃をくらった！", m_name);
						if (mon_take_hit(m_idx, dam, &fear,
						    "は燃え殻の山になった。", FALSE))
#else
						msg_format("%^s gets zapped!", m_name);

						if (mon_take_hit(m_idx, dam, &fear,
						    " turns into a pile of cinder.", FALSE))
#endif

						{
							blinked = FALSE;
							alive = FALSE;
						}
					}
					else
					{
						if (m_ptr->ml)
							r_ptr->r_flagsr |= RFR_RES_ELEC;
					}
				}

				if (p_ptr->sh_cold && alive && !p_ptr->is_dead)
				{
					if (!(r_ptr->flagsr & RFR_RES_COLD))
					{
						int dam = damroll(2, 6);

						/* Modify the damage */
						dam = mon_damage_mod(m_ptr, dam, FALSE);
						dam = modify_dam_by_elem(0, m_idx, dam, GF_COLD, MODIFY_ELEM_MODE_MELEE);

#ifdef JP
						msg_format("%^sは冷気をくらった！", m_name);
						if (mon_take_hit(m_idx, dam, &fear,
						    "は凍りついた。", FALSE))
#else
						msg_format("%^s is very cold!", m_name);

						if (mon_take_hit(m_idx, dam, &fear,
						    " was frozen.", FALSE))
#endif

						{
							blinked = FALSE;
							alive = FALSE;
						}
					}
					else
					{
						if (m_ptr->ml)
							r_ptr->r_flagsr |= RFR_RES_COLD;
					}
				}

				/* by henkma */
				if (p_ptr->dustrobe && alive && !p_ptr->is_dead)
				{
					if (!(r_ptr->flagsr & RFR_RES_SHAR))
					{
						int dam = damroll(2, 6);

						/* Modify the damage */
						dam = mon_damage_mod(m_ptr, dam, FALSE);
						dam = modify_dam_by_elem(0, m_idx, dam, GF_SHARDS, MODIFY_ELEM_MODE_MELEE);

#ifdef JP
						msg_format("%^sは破片をくらった！", m_name);
						if (mon_take_hit(m_idx, dam, &fear,
						    "はズタズタになった。", FALSE))
#else
						msg_format("%^s gets zapped!", m_name);

						if (mon_take_hit(m_idx, dam, &fear,
						    " had torn to pieces.", FALSE))
#endif
						  
						{
							blinked = FALSE;
							alive = FALSE;
						}
					}
					else
					{
						if (m_ptr->ml)
							r_ptr->r_flagsr |= RFR_RES_SHAR;
					}
				}

				if (p_ptr->tim_sh_holy && alive && !p_ptr->is_dead)
				{
					if (r_ptr->flags3 & RF3_EVIL)
					{
						int dam = damroll(2, 6);

						/* Modify the damage */
						dam = mon_damage_mod(m_ptr, dam, FALSE);
						dam = modify_dam_by_elem(0, m_idx, dam, GF_HOLY_FIRE, MODIFY_ELEM_MODE_MELEE);

#ifdef JP
						msg_format("%^sは聖なるオーラで傷ついた！", m_name);
						if (mon_take_hit(m_idx, dam, &fear,
						    "は倒れた。", FALSE))
#else
						msg_format("%^s is injured by holy power!", m_name);

						if (mon_take_hit(m_idx, dam, &fear,
						    " is destroyed.", FALSE))
#endif

						{
							blinked = FALSE;
							alive = FALSE;
						}
						if (m_ptr->ml)
							r_ptr->r_flags3 |= RF3_EVIL;
					}
				}
			}
		}

		/* Monster missed player */
		else
		{
			/* Analyze failed attacks */
			switch (method)
			{
				case RBM_HIT:
				case RBM_TOUCH:
				case RBM_PUNCH:
				case RBM_KICK:
				case RBM_CLAW:
				case RBM_BITE:
				case RBM_STING:
				case RBM_SLASH:
				case RBM_BUTT:
				case RBM_CRUSH:
				case RBM_ENGULF:
				case RBM_CHARGE:

				/* Visible monsters */
				if (m_ptr->ml)
				{
					/* Disturbing */
					disturb(1, 0);

					/* Message */
#ifdef JP
					if (syouryaku)
						msg_format("かわした。");
					else
						msg_format("%^sの攻撃をかわした。", m_name);
					syouryaku = 1;/*２回目以降は省略 */
#else
					msg_format("%^s misses you.", m_name);
#endif

				}
				damage = 0;

				break;
			}
		}


		/* Analyze "visible" monsters only */
		if (visible && !do_silly_attack)
		{
			/* Count "obvious" attacks (and ones that cause damage) */
			if (obvious || damage || (r_ptr->r_blows[ap_cnt] > 10))
			{
				/* Count attacks of this type */
				if (r_ptr->r_blows[ap_cnt] < MAX_UCHAR)
				{
					r_ptr->r_blows[ap_cnt]++;
				}
			}
		}

		if (p_ptr->riding && damage)
		{
			char m_name[80];
			monster_desc(m_name, &m_list[p_ptr->riding], 0);
			if (rakuba((damage > 200) ? 200 : damage, FALSE))
			{
#ifdef JP
				msg_format("%^sから落ちてしまった！", m_name);
#else
				msg_format("You have fallen from %s.", m_name);
#endif
			}
		}
	}

	if (p_ptr->tim_eyeeye && get_damage > 0 && !p_ptr->is_dead)
	{
#ifdef JP
		msg_format("攻撃が%s自身を傷つけた！", m_name);
#else
		char m_name_self[80];

		/* hisself */
		monster_desc(m_name_self, m_ptr, 0x23);

		msg_format("The attack of %s has wounded %s!", m_name, m_name_self);
#endif
		project(0, 0, m_ptr->fy, m_ptr->fx, get_damage, GF_MISSILE, PROJECT_KILL, MODIFY_ELEM_MODE_MAGIC);
		set_tim_eyeeye(p_ptr->tim_eyeeye-5, TRUE);
	}


	/* Blink away */
	if (blinked && alive && !p_ptr->is_dead)
	{
#ifdef JP
		msg_print("泥棒は笑って逃げた！");
#else
		msg_print("The thief flees laughing!");
#endif

		teleport_away(m_idx, MAX_SIGHT * 2 + 5);
	}


	/* Always notice cause of death */
	if (p_ptr->is_dead && (r_ptr->r_deaths < MAX_SHORT) && !p_ptr->inside_arena)
	{
		r_ptr->r_deaths++;
	}

	if (m_ptr->ml && fear && alive && !p_ptr->is_dead)
	{
		sound(SOUND_FLEE);
#ifdef JP
		msg_format("%^sは恐怖で逃げ出した！", m_name);
#else
		msg_format("%^s flees in terror!", m_name);
#endif

	}

	/* Assume we attacked */
	return (TRUE);
}
