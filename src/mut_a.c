/*
 * Mutations that cannot be activated as normal spells.
 * These mutations might be timed effects, or just things
 * like "Horns" that you simply have.  They might be augmentations
 * like "Super Human He-man".
 *
 * We are still implementing all mutations as spells for
 * uniformity.
 *
 * Again, spells are (stateless) objects, implemented as functions.
 */

#include "angband.h"

/*
void foo_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("", ""));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("", ""));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("", ""));
		break;
	case SPELL_CALC_BONUS:
		break;
	case SPELL_PROCESS:
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
*/

void alcohol_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Alcohol", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("Your body starts producing alcohol!", "あなたはアルコールを分泌するようになった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("Your body stops producing alcohol!", "あなたはアルコールを分泌しなくなった！"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("Your body produces alcohol.", "あなたの体はアルコールを分泌する。"));
		break;
	case SPELL_PROCESS:
		if (randint1(6400) == 321)
		{
			if (!p_ptr->resist_conf && !p_ptr->resist_chaos)
			{
				disturb(0, 0);
				p_ptr->redraw |= PR_EXTRA;
				msg_print(T("You feel a SSSCHtupor cOmINg over yOu... *HIC*!", "いひきがもーろーとひてきたきがふる...ヒック！"));
			}

			if (!p_ptr->resist_conf)
				set_confused(p_ptr->confused + randint0(20) + 15, FALSE);

			if (!p_ptr->resist_chaos)
			{
				if (one_in_(20))
				{
					msg_print(NULL);
					if (one_in_(3)) lose_all_info();
					else wiz_dark();
					teleport_player_aux(100, TELEPORT_NONMAGICAL | TELEPORT_PASSIVE);
					wiz_dark();
					msg_print(T("You wake up somewhere with a sore head...", "あなたは見知らぬ場所で目が醒めた...頭が痛い。"));
					msg_print(T("You can't remember a thing, or how you got here!", "何も覚えていない。どうやってここに来たかも分からない！"));
				}
				else if (one_in_(3))
				{
					msg_print(T("Thishcischs GooDSChtuff!", "き〜れいなちょおちょらとんれいる〜"));
					set_image(p_ptr->image + randint0(150) + 150, FALSE);
				}
			}
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void attract_animal_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Attract Animals", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You start attracting animals.", "動物を引き付けるようになった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You stop attracting animals.", "動物を引き寄せなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You attract animals.", "あなたは動物を引きつける。"));
		break;
	case SPELL_PROCESS:
		if (!p_ptr->anti_magic && one_in_(7000))
		{
			bool pet = one_in_(3);
			u32b mode = PM_ALLOW_GROUP;

			if (pet) mode |= PM_FORCE_PET;
			else mode |= (PM_ALLOW_UNIQUE | PM_NO_PET);

			if (summon_specific((pet ? -1 : 0), py, px, dun_level, SUMMON_ANIMAL, mode))
			{
				msg_print(T("You have attracted an animal!", "動物を引き寄せた！"));
				disturb(0, 0);
			}
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void attract_demon_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Attract Demons", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You start attracting demons.", "悪魔を引き付けるようになった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You stop attracting demons.", "デーモンを引き寄せなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You attract demons.", "あなたはデーモンを引きつける。"));
		break;
	case SPELL_PROCESS:
		if (!p_ptr->anti_magic && (randint1(6666) == 666))
		{
			bool pet = one_in_(6);
			u32b mode = PM_ALLOW_GROUP;

			if (pet) mode |= PM_FORCE_PET;
			else mode |= (PM_ALLOW_UNIQUE | PM_NO_PET);

			if (summon_specific((pet ? -1 : 0), py, px,
						dun_level, SUMMON_DEMON, mode))
			{
				msg_print(T("You have attracted a demon!", "あなたはデーモンを引き寄せた！"));
				disturb(0, 0);
			}
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void attract_dragon_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Attract Dragon", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You start attracting dragons.", "あなたはドラゴンを引きつけるようになった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You stop attracting dragons.", "ドラゴンを引き寄せなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You attract dragons.", "あなたはドラゴンを引きつける。"));
		break;
	case SPELL_PROCESS:
		if (!p_ptr->anti_magic && one_in_(3000))
		{
			bool pet = one_in_(5);
			u32b mode = PM_ALLOW_GROUP;

			if (pet) mode |= PM_FORCE_PET;
			else mode |= (PM_ALLOW_UNIQUE | PM_NO_PET);

			if (summon_specific((pet ? -1 : 0), py, px, dun_level, SUMMON_DRAGON, mode))
			{
				msg_print(T("You have attracted a dragon!", "ドラゴンを引き寄せた！"));
				disturb(0, 0);
			}
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void beak_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Beak", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("Your mouth turns into a sharp, powerful beak!", "口が鋭く強いクチバシに変化した！"));
		mut_lose(MUT_TRUNK);
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("Your mouth reverts to normal!", "口が普通に戻った！"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You have a beak (dam. 2d4).", "あなたはクチバシが生えている。(ダメージ 2d4)"));
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void berserk_rage_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Berserk Rage", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You become subject to fits of berserk rage!", "あなたは狂暴化の発作を起こすようになった！"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You are no longer subject to fits of berserk rage!", "凶暴化の発作にさらされなくなった！"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You are subject to berserker fits.", "あなたは狂戦士化の発作を起こす。"));
		break;
	case SPELL_PROCESS:
		if (!p_ptr->shero && one_in_(3000))
		{
			disturb(0, 0);
			cast_berserk();
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void chaos_deity_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Chaos Deity", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You attract the notice of a chaos deity!", "あなたはカオスの守護悪魔の注意を惹くようになった。"));
		/* In case it isn't obvious, every character has a chaos deity assigned at birth. */
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You lose the attention of the chaos deities.", "混沌の神々の興味を惹かなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("Chaos deities give you gifts.", "あなたはカオスの守護悪魔から褒美をうけとる。"));
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void cowardice_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Cowardice", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You become an incredible coward!", "信じられないくらい臆病になった！"));
		mut_lose(MUT_FEARLESS);
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You are no longer an incredible coward!", "もう信じがたいほど臆病ではなくなった！"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You are subject to cowardice.", "あなたは時々臆病になる。"));
		break;
	case SPELL_PROCESS:
		if (!p_ptr->resist_fear && (randint1(3000) == 13))
		{
			disturb(0, 0);
			msg_print(T("It's so dark... so scary!", "とても暗い... とても恐い！"));
			set_afraid(p_ptr->afraid + 13 + randint1(26), FALSE);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void eat_light_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Eat Light", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You feel a strange kinship with Ungoliant.", "あなたはウンゴリアントに奇妙な親しみを覚えるようになった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You feel the world's a brighter place.", "世界が明るいと感じる。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You sometimes feed off of the light around you.", "あなたは時々周囲の光を吸収して栄養にする。"));
		break;
	case SPELL_PROCESS:
		if (one_in_(3000))
		{
			object_type *o_ptr;

			msg_print(T("A shadow passes over you.", "影につつまれた。"));
			msg_print(NULL);

			/* Absorb light from the current possition */
			if ((cave[py][px].info & (CAVE_GLOW | CAVE_MNDK)) == CAVE_GLOW)
			{
				hp_player(10);
			}

			o_ptr = &inventory[INVEN_LITE];

			/* Absorb some fuel in the current lite */
			if (o_ptr->tval == TV_LITE)
			{
				/* Use some fuel (except on artifacts) */
				if (!object_is_fixed_artifact(o_ptr) && (o_ptr->xtra4 > 0))
				{
					/* Heal the player a bit */
					hp_player(o_ptr->xtra4 / 20);

					/* Decrease life-span of lite */
					o_ptr->xtra4 /= 2;

					msg_print(T("You absorb energy from your light!", "光源からエネルギーを吸収した！"));
					notice_lite_change(o_ptr);
				}
			}

			/*
			 * Unlite the area (radius 10) around player and
			 * do 50 points damage to every affected monster
			 */
			unlite_area(50, 10);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void flatulence_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Flatulence", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You become subject to uncontrollable flatulence.", "あなたは制御不能な強烈な屁をこくようになった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You are no longer subject to uncontrollable flatulence.", "もう強烈な屁はこかなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You are subject to uncontrollable flatulence.", "あなたは制御できない強烈な屁をこく。"));
		break;
	case SPELL_PROCESS:
		if (randint1(3000) == 13)
		{
			disturb(0, 0);
			/* Seriously, this the best mutation!  Ever!! :D */
			msg_print(T("BRRAAAP! Oops.", "ブゥーーッ！おっと。"));
			msg_print(NULL);
			fire_ball(GF_POIS, 0, p_ptr->lev, 3);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void fumbling_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Fumbling", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("Your feet grow to four times their former size.", "あなたの脚は長さが四倍になった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("Your feet shrink to their former size.", "脚が元の大きさに戻った。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You occasionally stumble and drop things.", "あなたはよくつまづいて物を落とす。"));
		break;
	case SPELL_PROCESS:
		if (one_in_(10000))
		{
			int slot = 0;
			object_type *o_ptr = NULL;

			disturb(0, 0);
			msg_print(T("You trip over your own feet!", "足がもつれて転んだ！"));
			take_hit(DAMAGE_NOESCAPE, randint1(p_ptr->wt / 6), T("tripping", "転倒"), -1);
			msg_print(NULL);

			if (buki_motteruka(INVEN_RARM))
			{
				slot = INVEN_RARM;
				o_ptr = &inventory[INVEN_RARM];

				if (buki_motteruka(INVEN_LARM) && one_in_(2))
				{
					o_ptr = &inventory[INVEN_LARM];
					slot = INVEN_LARM;
				}
			}
			else if (buki_motteruka(INVEN_LARM))
			{
				o_ptr = &inventory[INVEN_LARM];
				slot = INVEN_LARM;
			}

			if (slot && !object_is_cursed(o_ptr))
			{
				msg_print(T("You drop your weapon!", "武器を落としてしまった！"));
				inven_drop(slot, 1);
				msg_print("Press 'Y' to continue.");
				flush();
				for (;;)
				{
					char ch = inkey();
					if (ch == 'Y') break;
				}
				prt("", 0, 0);
				msg_flag = FALSE;
			}
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void hallucination_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Hallucination", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You are afflicted by a hallucinatory insanity!", "あなたは幻覚を引き起こす精神錯乱に侵された。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You are no longer afflicted by a hallucinatory insanity!", "幻覚をひき起こす精神障害を起こさなくなった！"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You have a hallucinatory insanity.", "あなたは幻覚を引き起こす精神錯乱に侵されている。"));
		break;
	case SPELL_PROCESS:
		if (!p_ptr->resist_chaos && randint1(6400) == 42)
		{
			disturb(0, 0);
			p_ptr->redraw |= PR_EXTRA;
			set_image(p_ptr->image + randint0(50) + 20, FALSE);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void he_man_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("He-man", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You turn into a superhuman he-man!", "超人的に強くなった！"));
		mut_lose(MUT_PUNY);
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("Your muscles revert to normal.", "筋肉が普通に戻った。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You are superhumanly strong (+4 STR).", "あなたは超人的に強い。(腕力+4)"));
		break;
	case SPELL_CALC_BONUS:
		p_ptr->stat_add[A_STR] += 4;
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void horns_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Horns", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("Horns pop forth into your forehead!", "額に角が生えた！"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("Your horns vanish from your forehead!", "額から角が消えた！"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You have horns (dam. 2d6).", "あなたは角が生えている。(ダメージ 2d6)"));
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void hp_to_sp_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("HP to SP", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You are subject to fits of painful clarity.", "痛みを伴う精神明瞭化の発作を起こすようになった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You are no longer subject to fits of painful clarity.", "痛みを伴う精神明瞭化の発作に襲われなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("Your blood sometimes rushes to your head.", "あなたは時々頭に血がどっと流れる。"));
		break;
	case SPELL_PROCESS:
		if (!p_ptr->anti_magic && one_in_(4000))
		{
			int wounds = p_ptr->msp - p_ptr->csp;

			if (wounds > 0)
			{
				int healing = p_ptr->chp;

				if (healing > wounds)
					healing = wounds;

				p_ptr->csp += healing;

				p_ptr->redraw |= (PR_MANA);
				take_hit(DAMAGE_LOSELIFE, healing, T("blood rushing to the head", "頭に昇った血"), -1);
			}
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void invulnerability_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Invulnerability", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You are blessed with fits of invulnerability.", "あなたは祝福され、無敵状態になる発作を起こすようになった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You are no longer blessed with fits of invulnerability.", "無敵状態の発作を起こさなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You occasionally feel invincible.", "あなたは時々負け知らずな気分になる。"));
		break;
	case SPELL_PROCESS:
		if (!p_ptr->anti_magic && one_in_(5000))
		{
			disturb(0, 0);
			msg_print(T("You feel invincible!", "無敵な気がする！"));

			msg_print(NULL);
			set_invuln(randint1(8) + 8, FALSE);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void nausea_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Nausea", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("Your stomach starts to roil nauseously.", "胃袋がピクピクしはじめた。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("Your stomach stops roiling.", "胃が痙攣しなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You have a seriously upset stomach.", "あなたの胃は非常に落ち着きがない。"));
		break;
	case SPELL_PROCESS:
		if (!p_ptr->slow_digest && one_in_(9000))
		{
			disturb(0, 0);
			
			msg_print(T("Your stomach roils, and you lose your lunch!", "胃が痙攣し、食事を失った！"));
			msg_print(NULL);

			set_food(PY_FOOD_WEAK);
			
			if (music_singing_any()) stop_singing();
			if (hex_spelling_any()) stop_hex_spell_all();
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void normality_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Normality", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You feel strangely normal.", "あなたは奇妙なほど普通になった気がする。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You feel normally strange.", "普通に奇妙な感じがする。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You may be mutated, but you're recovering.", "あなたは変異していたが、回復してきている。"));
		break;
	case SPELL_PROCESS:
		if (one_in_(5000))
		{
			if (mut_lose_random(NULL))
				msg_print(T("You feel oddly normal.", "奇妙なくらい普通になった気がする。"));
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void polymorph_wounds_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Polymorph Wounds", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You feel forces of chaos entering your old scars.", "あなたはカオスの力が古い傷に入り込んでくるのを感じた。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You feel forces of chaos departing your old scars.", "古い傷からカオスの力が去っていった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("Your health is subject to chaotic forces.", "あなたの健康はカオスの力に影響を受ける。"));
		break;
	case SPELL_PROCESS:
		if (one_in_(3000))
			do_poly_wounds();
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void produce_mana_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Produce Mana", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You start producing magical energy uncontrollably.", "あなたは制御不能な魔法のエネルギーを発生するようになった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You stop producing magical energy uncontrollably.", "制御不能な魔法のエネルギーを発生しなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You are producing magical energy uncontrollably.", "あなたは制御不能な魔法のエネルギーを発している。"));
		break;
	case SPELL_PROCESS:
		if (!p_ptr->anti_magic && one_in_(9000))
		{
			int dir = 0;
			disturb(0, 0);
			msg_print(T("Magical energy flows through you! You must release it!", "魔法のエネルギーが突然あなたの中に流れ込んできた！エネルギーを解放しなければならない！"));
			flush();
			msg_print(NULL);
			(void)get_hack_dir(&dir);
			fire_ball(GF_MANA, dir, p_ptr->lev * 2, 3);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void puny_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Puny", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("Your muscles wither away...", "筋肉が弱ってしまった..."));
		mut_lose(MUT_HYPER_STR);
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("Your muscles revert to normal.", "筋肉が普通に戻った。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You are puny (-4 STR).", "あなたは虚弱だ。(腕力-4)"));
		break;
	case SPELL_CALC_BONUS:
		p_ptr->stat_add[A_STR] -= 4;
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void random_banish_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Random Banish", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You feel a terrifying power lurking behind you.", "恐ろしい力があなたの背後に潜んでいる気がする。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You no longer feel a terrifying power lurking behind you.", "背後に恐ろしい力を感じなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You sometimes cause nearby creatures to vanish.", "あなたは時々近くのモンスターを消滅させる。"));
		break;
	case SPELL_PROCESS:
		if (one_in_(9000))
		{
			disturb(0, 0);
			msg_print(T("You suddenly feel almost lonely.", "突然ほとんど孤独になった気がする。"));

			banish_monsters(100);
			if (!dun_level && p_ptr->town_num)
			{
				int n;

				/* Pick a random shop (except home) */
				do
				{
					n = randint0(MAX_STORES);
				}
				while ((n == STORE_HOME) || (n == STORE_MUSEUM));

				msg_print(T("You see one of the shopkeepers running for the hills!", "店の主人が丘に向かって走っている！"));
				store_shuffle(n);
			}
			msg_print(NULL);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void random_teleport_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Random Teleportation", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("Your position seems very uncertain...", "あなたの位置は非常に不確定になった。"));
		mut_lose(MUT_TELEPORT);
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("Your position seems more certain.", "あなたの位置はより確定的になった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You are teleporting randomly.", "あなたはランダムにテレポートする。"));
		break;
	case SPELL_PROCESS:
		if (!p_ptr->resist_nexus && !p_ptr->anti_tele && (randint1(5000) == 88))
		{
			disturb(0, 0);
			msg_print(T("Your position suddenly seems very uncertain...", "あなたの位置は突然ひじょうに不確定になった..."));
			msg_print(NULL);
			teleport_player(40, TELEPORT_PASSIVE);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void raw_chaos_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Raw Chaos", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You feel the universe is less stable around you.", "周囲の空間が不安定になった気がする。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You feel the universe is more stable around you.", "周囲の空間が安定した気がする。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You occasionally are surrounded with raw chaos.", "あなたはしばしば純カオスに包まれる。"));
		break;
	case SPELL_PROCESS:
		if (!p_ptr->anti_magic && one_in_(8000))
		{
			disturb(0, 0);
			msg_print(T("You feel the world warping around you!", "周りの空間が歪んでいる気がする！"));
			msg_print(NULL);
			fire_ball(GF_CHAOS, 0, p_ptr->lev, 8);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void scorpion_tail_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Scorpion Tail", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You grow a scorpion tail!", "サソリの尻尾が生えてきた！"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You lose your scorpion tail!", "サソリの尻尾がなくなった！"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You have a scorpion tail (poison, 3d7).", "あなたはサソリの尻尾が生えている。(毒、ダメージ 3d7)"));
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void shadow_walk_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Shadow Walk", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You feel like reality is as thin as paper.", "あなたは現実が紙のように薄いと感じるようになった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You feel like you're trapped in reality.", "物質世界に捕らわれている気がする。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You occasionally stumble into other shadows.", "あなたはしばしば他の「影」に迷い込む。"));
		break;
	case SPELL_PROCESS:
		if (!p_ptr->anti_magic && one_in_(12000) && !p_ptr->inside_arena)
			alter_reality();
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void sp_to_hp_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("SP to HP", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You are subject to fits of magical healing.", "魔法の治癒の発作を起こすようになった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You are no longer subject to fits of magical healing.", "魔法の治癒の発作に襲われなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("Your blood sometimes rushes to your muscles.", "あなたは時々血が筋肉にどっと流れる。"));
		break;
	case SPELL_PROCESS:
		if (one_in_(2000))
		{
			int wounds = p_ptr->mhp - p_ptr->chp;

			if (wounds > 0)
			{
				int healing = p_ptr->csp;

				if (healing > wounds)
					healing = wounds;

				hp_player(healing);
				p_ptr->csp -= healing;

				p_ptr->redraw |= (PR_MANA);
			}
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void speed_flux_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Speed Flux", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You become manic-depressive.", "あなたは躁鬱質になった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You are no longer manic-depressive.", "躁鬱質でなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You move faster or slower randomly.", "あなたはランダムに早く動いたり遅く動いたりする。"));
		break;
	case SPELL_PROCESS:
		if (one_in_(6000))
		{
			disturb(0, 0);
			if (one_in_(2))
			{
				msg_print(T("You feel less energetic.", "精力的でなくなった気がする。"));
				if (p_ptr->fast > 0)
					set_fast(0, TRUE);
				else
					set_slow(randint1(30) + 10, FALSE);
			}
			else
			{
				msg_print(T("You feel more energetic.", "精力的になった気がする。"));
				if (p_ptr->slow > 0)
					set_slow(0, TRUE);
				else
					set_fast(randint1(30) + 10, FALSE);
			}
			msg_print(NULL);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void tentacles_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Tentacles", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("Evil-looking tentacles sprout from your sides.", "邪悪な触手が体の両側に生えてきた。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("Your tentacles vanish from your sides.", "触手が消えた。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You have evil looking tentacles (dam 2d5).", "あなたは邪悪な触手を持っている。(ダメージ 2d5)"));
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void trunk_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Elephantine Trunk", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("Your nose grows into an elephant-like trunk.", "あなたの鼻は伸びて象の鼻のようになった。"));
		mut_lose(MUT_BEAK);
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("Your nose returns to a normal length.", "鼻が普通の長さに戻った。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You have an elephantine trunk (dam 1d4).", "あなたは象のような鼻を持っている。(ダメージ 1d4)"));
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void warning_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Warning", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You suddenly feel paranoid.", "あなたは突然パラノイアになった気がする。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You no longer feel paranoid.", "パラノイアでなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You receive warnings about your foes.", "あなたは敵に関する警告を感じる。"));
		break;
	case SPELL_PROCESS:
		if (one_in_(1000))
		{
			int danger_amount = 0;
			int monster;

			for (monster = 0; monster < m_max; monster++)
			{
				monster_type    *m_ptr = &m_list[monster];
				monster_race    *r_ptr = &r_info[m_ptr->r_idx];

				/* Skip dead monsters */
				if (!m_ptr->r_idx) continue;

				if (r_ptr->level >= p_ptr->lev)
				{
					danger_amount += r_ptr->level - p_ptr->lev + 1;
				}
			}

			if (danger_amount > 100)
				msg_print(T("You feel utterly terrified!", "非常に恐ろしい気がする！"));

			else if (danger_amount > 50)
				msg_print(T("You feel terrified!", "恐ろしい気がする！"));

			else if (danger_amount > 20)
				msg_print(T("You feel very worried!", "非常に心配な気がする！"));

			else if (danger_amount > 10)
				msg_print(T("You feel paranoid!", "心配な気がする！"));

			else if (danger_amount > 5)
				msg_print(T("You feel almost safe.", "ほとんど安全な気がする。"));

			else
				msg_print(T("You feel lonely.", "寂しい気がする。"));
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void wasting_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Horrible Wasting", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You suddenly contract a horrible wasting disease.", "あなたは突然おぞましい衰弱病にかかった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You are cured of the horrible wasting disease!", "おぞましい衰弱病が治った！"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You have a horrible wasting disease.", "あなたは衰弱する恐ろしい病気にかかっている。"));
		break;
	case SPELL_PROCESS:
		if (one_in_(3000))
		{
			int which_stat = randint0(6);
			int sustained = FALSE;

			switch (which_stat)
			{
			case A_STR:
				if (p_ptr->sustain_str) sustained = TRUE;
				break;
			case A_INT:
				if (p_ptr->sustain_int) sustained = TRUE;
				break;
			case A_WIS:
				if (p_ptr->sustain_wis) sustained = TRUE;
				break;
			case A_DEX:
				if (p_ptr->sustain_dex) sustained = TRUE;
				break;
			case A_CON:
				if (p_ptr->sustain_con) sustained = TRUE;
				break;
			case A_CHR:
				if (p_ptr->sustain_chr) sustained = TRUE;
				break;
			default:
				msg_print(T("Invalid stat chosen!", "不正な状態！"));
				sustained = TRUE;
				break;
			}

			if (!sustained)
			{
				disturb(0, 0);
				msg_print(T("You can feel yourself wasting away!", "自分が衰弱していくのが分かる！"));
				msg_print(NULL);
				dec_stat(which_stat, randint1(6) + 6, one_in_(6));
			}
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void weird_mind_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Weird Mind", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("Your thoughts suddenly take off in strange directions.", "あなたの思考は突然おかしな方向に向き始めた。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("Your thoughts return to boring paths.", "思考が退屈な方向に戻った。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("Your mind randomly expands and contracts.", "あなたの精神はランダムに拡大したり縮小したりしている。"));
		break;
	case SPELL_PROCESS:
		if (!p_ptr->anti_magic && one_in_(3000))
		{
			if (p_ptr->tim_esp > 0)
			{
				msg_print(T("Your mind feels cloudy!", "精神にもやがかかった！"));
				set_tim_esp(0, TRUE);
			}
			else
			{
				msg_print(T("Your mind expands!", "精神が広がった！"));
				set_tim_esp(p_ptr->lev, FALSE);
			}
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void wraith_mut(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Wraithform", ""));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You start to fade in and out of the physical world.", "あなたは幽体化したり実体化したりするようになった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You are firmly in the physical world.", "あなたは物質世界にしっかり存在している。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You fade in and out of physical reality.", "あなたの肉体は幽体化したり実体化したりする。"));
		break;
	case SPELL_PROCESS:
		if (!p_ptr->anti_magic && one_in_(3000))
		{
			disturb(0, 0);
			msg_print(T("You feel insubstantial!", "非物質化した！"));
			msg_print(NULL);
			set_wraith_form(randint1(p_ptr->lev / 2) + (p_ptr->lev / 2), FALSE);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

