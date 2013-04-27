/* File: spells2.c */

/* Purpose: Spell code (part 2) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * self-knowledge... idea from nethack.  Useful for determining powers and
 * resistences of items.  It saves the screen, clears it, then starts listing
 * attributes, a screenful at a time.  (There are a LOT of attributes to
 * list.  It will probably take 2 or 3 screens for a powerful character whose
 * using several artifacts...) -CFT
 *
 * It is now a lot more efficient. -BEN-
 *
 * See also "identify_fully()".
 *
 * XXX XXX XXX Use the "show_file()" method, perhaps.
 */
void self_knowledge(void)
{
	int i = 0, j, k;

	u32b f1 = 0L, f2 = 0L, f3 = 0L;

	object_type *o_ptr;

	char Dummy[80], Liferating[80];

	cptr info[220];

	int plev = p_ptr->lev;

	int percent;

	strcpy(Dummy, "");
	strcpy(Liferating, "");

	percent = (int)(((long)player_hp[PY_MAX_LEVEL - 1] * 200L) /
		( 2 * p_ptr->hitdie +
		( (PY_MAX_LEVEL - 1 + 3) * (p_ptr->hitdie + 1))));

#ifdef JP
sprintf(Liferating, "現在の体力ランクは %d/100です。", percent);
#else
	sprintf(Liferating, "Your current Life Rating is %d/100.", percent);
#endif

	info[i++] = Liferating;

	/* Acquire item flags from equipment */
	for (k = INVEN_WIELD; k < INVEN_TOTAL; k++)
	{
		u32b t1, t2, t3;

		o_ptr = &inventory[k];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		/* Extract the flags */
		object_flags(o_ptr, &t1, &t2, &t3);

		/* Extract flags */
		f1 |= t1;
		f2 |= t2;
		f3 |= t3;
	}

	/* Extra brands */
	if (p_ptr->tim_brand)
	{
		f1 |= p_ptr->xtra_brand;
	}

	/* Racial powers... */
	switch (p_ptr->prace)
	{
		case RACE_DWARF:
			if (plev > 4)
#ifdef JP
info[i++] = "あなたは罠とドアと階段を感知できる。(5 MP)";
#else
				info[i++] = "You can find traps, doors and stairs (cost 5).";
#endif

			break;
		case RACE_HOBBIT:
			if (plev > 14)
			{
#ifdef JP
info[i++] = "あなたは食料を生成できる。(10 MP)";
#else
				info[i++] = "You can produce food (cost 10).";
#endif

			}
			break;
		case RACE_HALF_ORC:
			if (plev > 2)
#ifdef JP
info[i++] = "あなたは恐怖を除去できる。(5 MP)";
#else
				info[i++] = "You can remove fear (cost 5).";
#endif

			break;
		case RACE_BARBARIAN:
			if (plev > 7)
#ifdef JP
info[i++] = "あなたは狂暴化することができる。(10 MP) ";
#else
				info[i++] = "You can enter berserk fury (cost 10).";
#endif

			break;
#if 0
		case RACE_VAMPIRE:
			if (plev > 1)
			{
#ifdef JP
sprintf(Dummy, "あなたは敵から %d-%d HP の生命力を吸収できる。(%d MP)",
#else
				sprintf(Dummy, "You can steal life from a foe, dam. %d-%d (cost %d).",
#endif

				    plev + MAX(1, plev / 10), plev + plev * MAX(1, plev / 10), 1 + (plev / 3));
				info[i++] = Dummy;
			}
			break;
#endif
		default:
			break;
	}

	if (p_ptr->muta)
	{
		if (p_ptr->muta & MUT_BERS_RAGE)
		{
#ifdef JP
			info[i++] = "あなたは肉体野獣化の発作を起こす。";
#else
			info[i++] = "You are subject to berserker fits.";
#endif
		}
		if (p_ptr->muta & MUT_COWARDICE)
		{
#ifdef JP
			info[i++] = "あなたは時々臆病になる。";
#else
			info[i++] = "You are subject to cowardice.";
#endif
		}
		if (p_ptr->muta & MUT_HALLU)
		{
#ifdef JP
			info[i++] = "あなたは幻覚を引き起こす精神錯乱に侵されている。";
#else
			info[i++] = "You have a hallucinatory insanity.";
#endif
		}
		if (p_ptr->muta & MUT_NORMALITY)
		{
#ifdef JP
			info[i++] = "あなたは変異していたが、回復してきている。";
#else
			info[i++] = "You may be mutated, but you're recovering.";
#endif
		}
		if (p_ptr->muta & MUT_WASTING)
		{
#ifdef JP
			info[i++] = "あなたは衰弱する恐ろしい病気にかかっている。";
#else
			info[i++] = "You have a horrible wasting disease.";
#endif
		}
		if (p_ptr->muta & MUT_WARNING)
		{
#ifdef JP
			info[i++] = "あなたは敵に関する警告を感じる。";
#else
			info[i++] = "You receive warnings about your foes.";
#endif
		}
		if (p_ptr->muta & MUT_HYPER_STR)
		{
#ifdef JP
			info[i++] = "あなたは超人的に強い。(腕力+4)";
#else
			info[i++] = "You are superhumanly strong (+4 STR).";
#endif
		}
		if (p_ptr->muta & MUT_PUNY)
		{
#ifdef JP
			info[i++] = "あなたは虚弱だ。(腕力-4)";
#else
			info[i++] = "You are puny (-4 STR).";
#endif
		}
		if (p_ptr->muta & MUT_HYPER_INT)
		{
#ifdef JP
			info[i++] = "あなたの脳は生体コンピュータだ。(知能＆賢さ+4)";
#else
			info[i++] = "Your brain is a living computer (+4 INT/WIS).";
#endif
		}
		if (p_ptr->muta & MUT_MORONIC)
		{
#ifdef JP
			info[i++] = "あなたは精神薄弱だ。(知能＆賢さ-4)";
#else
			info[i++] = "You are moronic (-4 INT/WIS).";
#endif
		}
		if (p_ptr->muta & MUT_RESILIENT)
		{
#ifdef JP
			info[i++] = "あなたは非常にタフだ。(耐久+4)";
#else
			info[i++] = "You are very resilient (+4 CON).";
#endif
		}
		if (p_ptr->muta & MUT_XTRA_FAT)
		{
#ifdef JP
			info[i++] = "あなたは極端に太っている。(耐久+2,スピード-2)";
#else
			info[i++] = "You are extremely fat (+2 CON, -2 speed).";
#endif
		}
		if (p_ptr->muta & MUT_ALBINO)
		{
#ifdef JP
			info[i++] = "あなたはアルビノだ。(耐久-4)";
#else
			info[i++] = "You are albino (-4 CON).";
#endif
		}
		if (p_ptr->muta & MUT_FLESH_ROT)
		{
#ifdef JP
			info[i++] = "あなたの肉体は腐敗している。(耐久-2,魅力-1)";
#else
			info[i++] = "Your flesh is rotting (-2 CON, -1 CHR).";
#endif
		}
		if (p_ptr->muta & MUT_SILLY_VOI)
		{
#ifdef JP
			info[i++] = "あなたの声は間抜けなキーキー声だ。(魅力-4)";
#else
			info[i++] = "Your voice is a silly squeak (-4 CHR).";
#endif
		}
		if (p_ptr->muta & MUT_ILL_NORM)
		{
#ifdef JP
			info[i++] = "あなたは幻影に覆われている。";
#else
			info[i++] = "Your appearance is masked with illusion.";
#endif
		}
		if (p_ptr->muta & MUT_XTRA_EYES)
		{
#ifdef JP
			info[i++] = "あなたは余分に二つの目を持っている。(探索+15)";
#else
			info[i++] = "You have an extra pair of eyes (+15 search).";
#endif
		}
		if (p_ptr->muta & MUT_XTRA_LEGS)
		{
#ifdef JP
			info[i++] = "あなたは余分に二本の足が生えている。(加速+3)";
#else
			info[i++] = "You have an extra pair of legs (+3 speed).";
#endif
		}
		if (p_ptr->muta & MUT_SHORT_LEG)
		{
#ifdef JP
			info[i++] = "あなたの足は短い突起だ。(加速-3)";
#else
			info[i++] = "Your legs are short stubs (-3 speed).";
#endif
		}
		if (p_ptr->muta & MUT_ELEC_TOUC)
		{
#ifdef JP
			info[i++] = "あなたの血管には電流が流れている。";
#else
			info[i++] = "Electricity is running through your veins.";
#endif
		}
		if (p_ptr->muta & MUT_FIRE_BODY)
		{
			/* Unnecessary, actually... */
		}
		if (p_ptr->muta & MUT_SCALES)
		{
#ifdef JP
			info[i++] = "あなたの肌は鱗になっている。(魅力-1, AC+10)";
#else
			info[i++] = "Your skin has turned into scales (-1 CHR, +10 AC).";
#endif
		}
		if (p_ptr->muta & MUT_IRON_SKIN)
		{
#ifdef JP
			info[i++] = "あなたの肌は鉄でできている。(器用-1, AC+25)";
#else
			info[i++] = "Your skin is made of steel (-1 DEX, +25 AC).";
#endif
		}
		if (p_ptr->muta & MUT_WINGS)
		{
#ifdef JP
			info[i++] = "あなたは羽を持っている。";
#else
			info[i++] = "You have wings.";
#endif
		}
		if (p_ptr->muta & MUT_FEARLESS)
		{
			/* Unnecessary */
		}
		if (p_ptr->muta & MUT_REGEN)
		{
			/* Unnecessary */
		}
		if (p_ptr->muta & MUT_ESP)
		{
			/* Unnecessary */
		}
		if (p_ptr->muta & MUT_LIMBER)
		{
#ifdef JP
			info[i++] = "あなたの体は非常にしなやかだ。(器用+3)";
#else
			info[i++] = "Your body is very limber (+3 DEX).";
#endif
		}
		if (p_ptr->muta & MUT_ARTHRITIS)
		{
#ifdef JP
			info[i++] = "あなたはいつも関節に痛みを感じている。(器用-3)";
#else
			info[i++] = "Your joints ache constantly (-3 DEX).";
#endif
		}
		if (p_ptr->muta & MUT_RES_TIME)
		{
#ifdef JP
			info[i++] = "あなたは時間逆転攻撃から守られている。";
#else
			info[i++] = "You are protected from the ravages of time.";
#endif
		}
		if (p_ptr->muta & MUT_VULN_ELEM)
		{
#ifdef JP
			info[i++] = "あなたは元素の攻撃に弱い。";
#else
			info[i++] = "You are susceptible to damage from the elements.";
#endif
		}
		if (p_ptr->muta & MUT_MOTION)
		{
#ifdef JP
			info[i++] = "あなたの動作は正確で力強い。(隠密+1)";
#else
			info[i++] = "Your movements are precise and forceful (+1 STL).";
#endif
		}
	}

	if (p_ptr->blind)
	{
#ifdef JP
		info[i++] = "あなたは目が見えない。";
#else
		info[i++] = "You cannot see.";
#endif
	}
	if (p_ptr->confused)
	{
#ifdef JP
		info[i++] = "あなたは混乱している。";
#else
		info[i++] = "You are confused.";
#endif
	}
	if (p_ptr->afraid)
	{
#ifdef JP
		info[i++] = "あなたは恐怖に侵されている。";
#else
		info[i++] = "You are terrified.";
#endif
	}
	if (p_ptr->cut)
	{
#ifdef JP
		info[i++] = "あなたは出血している。";
#else
		info[i++] = "You are bleeding.";
#endif
	}
	if (p_ptr->stun)
	{
#ifdef JP
		info[i++] = "あなたはもうろうとしている。";
#else
		info[i++] = "You are stunned.";
#endif
	}
	if (p_ptr->poisoned)
	{
#ifdef JP
		info[i++] = "あなたは毒に侵されている。";
#else
		info[i++] = "You are poisoned.";
#endif
	}
	if (p_ptr->image)
	{
#ifdef JP
		info[i++] = "あなたは幻覚を見ている。";
#else
		info[i++] = "You are hallucinating.";
#endif
	}
	if (p_ptr->aggravate)
	{
#ifdef JP
		info[i++] = "あなたはモンスターを怒らせている。";
#else
		info[i++] = "You aggravate monsters.";
#endif
	}
	if (p_ptr->teleport)
	{
#ifdef JP
		info[i++] = "あなたの位置はひじょうに不安定だ。";
#else
		info[i++] = "Your position is very uncertain.";
#endif
	}
	if (p_ptr->blessed)
	{
#ifdef JP
		info[i++] = "あなたは公正さを感じている。";
#else
		info[i++] = "You feel rightous.";
#endif
	}
	if (p_ptr->hero)
	{
#ifdef JP
		info[i++] = "あなたはヒーロー気分だ。";
#else
		info[i++] = "You feel heroic.";
#endif
	}
	if (p_ptr->shero)
	{
#ifdef JP
		info[i++] = "あなたは戦闘狂だ。";
#else
		info[i++] = "You are in a battle rage.";
#endif
	}
	if (p_ptr->protevil)
	{
#ifdef JP
		info[i++] = "あなたは邪悪なる存在から守られている。";
#else
		info[i++] = "You are protected from evil.";
#endif
	}
	if (p_ptr->shield)
	{
#ifdef JP
		info[i++] = "あなたは神秘のシールドで守られている。";
#else
		info[i++] = "You are protected by a mystic shield.";
#endif
	}
	if (p_ptr->invuln)
	{
#ifdef JP
		info[i++] = "あなたは現在傷つかない。";
#else
		info[i++] = "You are temporarily invulnerable.";
#endif
	}
	if (p_ptr->tim_wraith)
	{
#ifdef JP
		info[i++] = "あなたは一時的に幽体化している。";
#else
		info[i++] = "You are temporarily incorporeal.";
#endif
	}
	else if (p_ptr->wraith_form)
	{
#ifdef JP
		info[i++] = "あなたは幽体化している。";
#else
		info[i++] = "You are incorporeal.";
#endif
	}
	if (p_ptr->confusing)
	{
#ifdef JP
		info[i++] = "あなたの手は赤く輝いている。";
#else
		info[i++] = "Your hands are glowing dull red.";
#endif
	}
	if (p_ptr->searching)
	{
#ifdef JP
		info[i++] = "あなたはひじょうに注意深く周囲を見渡している。";
#else
		info[i++] = "You are looking around very carefully.";
#endif
	}
	if (p_ptr->new_spells)
	{
#ifdef JP
		info[i++] = "あなたは呪文や祈りを学ぶことができる。";
#else
		info[i++] = "You can learn some spells/prayers.";
#endif
	}
	if (p_ptr->word_recall)
	{
#ifdef JP
		info[i++] = "あなたはすぐに帰還するだろう。";
#else
		info[i++] = "You will soon be recalled.";
#endif
	}
	if (p_ptr->see_infra)
	{
#ifdef JP
		info[i++] = "あなたの瞳は赤外線に敏感である。";
#else
		info[i++] = "Your eyes are sensitive to infrared light.";
#endif
	}
	if (p_ptr->see_inv)
	{
#ifdef JP
		info[i++] = "あなたは透明なモンスターを見ることができる。";
#else
		info[i++] = "You can see invisible creatures.";
#endif
	}
	if (p_ptr->ffall)
	{
#ifdef JP
		info[i++] = "あなたは飛ぶことができる。";
#else
		info[i++] = "You can fly.";
#endif
	}
	if (p_ptr->free_act)
	{
#ifdef JP
		info[i++] = "あなたは麻痺知らずの効果を持っている。";
#else
		info[i++] = "You have free action.";
#endif
	}
	if (p_ptr->regenerate)
	{
#ifdef JP
		info[i++] = "あなたは素早く体力を回復する。";
#else
		info[i++] = "You regenerate quickly.";
#endif
	}
	if (p_ptr->slow_digest)
	{
#ifdef JP
		info[i++] = "あなたは食欲が少ない。";
#else
		info[i++] = "Your appetite is small.";
#endif
	}
	if (p_ptr->telepathy)
	{
#ifdef JP
		info[i++] = "あなたはテレパシー能力を持っている。";
#else
		info[i++] = "You have ESP.";
#endif
	}
	if (p_ptr->hold_life)
	{
#ifdef JP
		info[i++] = "あなたは自己の生命力をしっかりと維持する。";
#else
		info[i++] = "You have a firm hold on your life force.";
#endif
	}
	if (p_ptr->reflect)
	{
#ifdef JP
		info[i++] = "あなたは矢やボルトを反射する。";
#else
		info[i++] = "You reflect arrows and bolts.";
#endif
	}
	if (p_ptr->sh_fire)
	{
#ifdef JP
		info[i++] = "あなたは炎のオーラに包まれている。";
#else
		info[i++] = "You are surrounded with a fiery aura.";
#endif
	}
	if (p_ptr->sh_elec)
	{
#ifdef JP
		info[i++] = "あなたは電気に包まれている。";
#else
		info[i++] = "You are surrounded with electricity.";
#endif
	}
	if (p_ptr->sh_cold)
	{
#ifdef JP
		info[i++] = "あなたは冷気のオーラに包まれている。";
#else
		info[i++] = "You are surrounded with a cold aura.";
#endif
	}
	if (p_ptr->anti_magic)
	{
#ifdef JP
		info[i++] = "あなたは反魔法シールドに包まれている。";
#else
		info[i++] = "You are surrounded by an anti-magic shell.";
#endif
	}
	if (p_ptr->anti_tele)
	{
#ifdef JP
		info[i++] = "あなたはテレポートできない。";
#else
		info[i++] = "You cannot teleport.";
#endif
	}
	if (p_ptr->lite)
	{
#ifdef JP
		info[i++] = "あなたの身体は光っている。";
#else
		info[i++] = "You are carrying a permanent light.";
#endif
	}
	if (p_ptr->warning)
	{
#ifdef JP
		info[i++] = "あなたは行動の前に危険を察知することができる。";
#else
		info[i++] = "You will be warned before dangerous actions.";
#endif
	}
	if (p_ptr->dec_mana)
	{
#ifdef JP
		info[i++] = "あなたは少ない消費魔力で魔法を唱えることができる。";
#else
		info[i++] = "You can cast spell with fewer mana.";
#endif
	}

	if (p_ptr->immune_acid)
	{
#ifdef JP
		info[i++] = "あなたは酸に対する完全なる免疫を持っている。";
#else
		info[i++] = "You are completely immune to acid.";
#endif
	}
	else if ((p_ptr->resist_acid) && (p_ptr->oppose_acid))
	{
#ifdef JP
		info[i++] = "あなたは酸への強力な耐性を持っている。";
#else
		info[i++] = "You resist acid exceptionally well.";
#endif
	}
	else if ((p_ptr->resist_acid) || (p_ptr->oppose_acid))
	{
#ifdef JP
		info[i++] = "あなたは酸への耐性を持っている。";
#else
		info[i++] = "You are resistant to acid.";
#endif
	}

	if (p_ptr->immune_elec)
	{
#ifdef JP
		info[i++] = "あなたは電撃に対する完全なる免疫を持っている。";
#else
		info[i++] = "You are completely immune to lightning.";
#endif
	}
	else if ((p_ptr->resist_elec) && (p_ptr->oppose_elec))
	{
#ifdef JP
		info[i++] = "あなたは電撃への強力な耐性を持っている。";
#else
		info[i++] = "You resist lightning exceptionally well.";
#endif
	}
	else if ((p_ptr->resist_elec) || (p_ptr->oppose_elec))
	{
#ifdef JP
		info[i++] = "あなたは電撃への耐性を持っている。";
#else
		info[i++] = "You are resistant to lightning.";
#endif
	}

	if (p_ptr->immune_fire)
	{
#ifdef JP
		info[i++] = "あなたは火に対する完全なる免疫を持っている。";
#else
		info[i++] = "You are completely immune to fire.";
#endif
	}
	else if ((p_ptr->resist_fire) && (p_ptr->oppose_fire))
	{
#ifdef JP
		info[i++] = "あなた火への強力な耐性を持っている。";
#else
		info[i++] = "You resist fire exceptionally well.";
#endif
	}
	else if ((p_ptr->resist_fire) || (p_ptr->oppose_fire))
	{
#ifdef JP
		info[i++] = "あなたは火への耐性を持っている。";
#else
		info[i++] = "You are resistant to fire.";
#endif
	}

	if (p_ptr->immune_cold)
	{
#ifdef JP
		info[i++] = "あなたは冷気に対する完全なる免疫を持っている。";
#else
		info[i++] = "You are completely immune to cold.";
#endif
	}
	else if ((p_ptr->resist_cold) && (p_ptr->oppose_cold))
	{
#ifdef JP
		info[i++] = "あなたは冷気への強力な耐性を持っている。";
#else
		info[i++] = "You resist cold exceptionally well.";
#endif
	}
	else if ((p_ptr->resist_cold) || (p_ptr->oppose_cold))
	{
#ifdef JP
		info[i++] = "あなたは冷気への耐性を持っている。";
#else
		info[i++] = "You are resistant to cold.";
#endif
	}

	if ((p_ptr->resist_pois) && (p_ptr->oppose_pois))
	{
#ifdef JP
		info[i++] = "あなたは毒への強力な耐性を持っている。";
#else
		info[i++] = "You resist poison exceptionally well.";
#endif
	}
	else if ((p_ptr->resist_pois) || (p_ptr->oppose_pois))
	{
#ifdef JP
		info[i++] = "あなたは毒への耐性を持っている。";
#else
		info[i++] = "You are resistant to poison.";
#endif
	}

	if (p_ptr->resist_lite)
	{
#ifdef JP
		info[i++] = "あなたは閃光への耐性を持っている。";
#else
		info[i++] = "You are resistant to bright light.";
#endif
	}
#if 0
	if (p_ptr->prace == RACE_VAMPIRE)
	{
#ifdef JP
		info[i++] = "あなたは閃光に弱い。";
#else
		info[i++] = "You are susceptible to damage from bright light.";
#endif
	}
#endif
	if (p_ptr->wraith_form)
	{
#ifdef JP
		info[i++] = "あなたは暗黒を吸収できる。";
#else
		info[i++] = "You can drain darkness.";
#endif
	}
#if 0
	else if (p_ptr->prace == RACE_VAMPIRE)
	{
#ifdef JP
		info[i++] = "あなたは暗黒に対する完全なる免疫を持っている。";
#else
		info[i++] = "You are completely immune to darkness.";
#endif
	}
#endif
	else if (p_ptr->resist_dark)
	{
#ifdef JP
		info[i++] = "あなたは暗黒への耐性を持っている。";
#else
		info[i++] = "You are resistant to darkness.";
#endif
	}
	if (p_ptr->resist_conf)
	{
#ifdef JP
		info[i++] = "あなたは混乱への耐性を持っている。";
#else
		info[i++] = "You are resistant to confusion.";
#endif
	}
	if (p_ptr->resist_sound)
	{
#ifdef JP
		info[i++] = "あなたは音波の衝撃への耐性を持っている。";
#else
		info[i++] = "You are resistant to sonic attacks.";
#endif
	}
	if (p_ptr->resist_disen)
	{
#ifdef JP
		info[i++] = "あなたは劣化への耐性を持っている。";
#else
		info[i++] = "You are resistant to disenchantment.";
#endif
	}
	if (p_ptr->resist_chaos)
	{
#ifdef JP
		info[i++] = "あなたはカオスの力への耐性を持っている。";
#else
		info[i++] = "You are resistant to chaos.";
#endif
	}
	if (p_ptr->resist_shard)
	{
#ifdef JP
		info[i++] = "あなたは破片の攻撃への耐性を持っている。";
#else
		info[i++] = "You are resistant to blasts of shards.";
#endif
	}
	if (p_ptr->resist_nexus)
	{
#ifdef JP
		info[i++] = "あなたは因果混乱の攻撃への耐性を持っている。";
#else
		info[i++] = "You are resistant to nexus attacks.";
#endif
	}
	if (p_ptr->resist_neth)
	{
#ifdef JP
		info[i++] = "あなたは地獄の力への耐性を持っている。";
#else
		info[i++] = "You are resistant to nether forces.";
#endif
	}
	if (p_ptr->resist_fear)
	{
#ifdef JP
		info[i++] = "あなたは全く恐怖を感じない。";
#else
		info[i++] = "You are completely fearless.";
#endif
	}
	if (p_ptr->resist_blind)
	{
#ifdef JP
		info[i++] = "あなたの目は盲目への耐性を持っている。";
#else
		info[i++] = "Your eyes are resistant to blindness.";
#endif
	}

	if (p_ptr->sustain_str)
	{
#ifdef JP
		info[i++] = "あなたの腕力は維持されている。";
#else
		info[i++] = "Your strength is sustained.";
#endif
	}
	if (p_ptr->sustain_int)
	{
#ifdef JP
		info[i++] = "あなたの知能は維持されている。";
#else
		info[i++] = "Your intelligence is sustained.";
#endif
	}
	if (p_ptr->sustain_wis)
	{
#ifdef JP
		info[i++] = "あなたの賢さは維持されている。";
#else
		info[i++] = "Your wisdom is sustained.";
#endif
	}
	if (p_ptr->sustain_con)
	{
#ifdef JP
		info[i++] = "あなたの耐久力は維持されている。";
#else
		info[i++] = "Your constitution is sustained.";
#endif
	}
	if (p_ptr->sustain_dex)
	{
#ifdef JP
		info[i++] = "あなたの器用さは維持されている。";
#else
		info[i++] = "Your dexterity is sustained.";
#endif
	}
	if (p_ptr->sustain_chr)
	{
#ifdef JP
		info[i++] = "あなたの魅力は維持されている。";
#else
		info[i++] = "Your charisma is sustained.";
#endif
	}

	if (f1 & (TR1_STR))
	{
#ifdef JP
		info[i++] = "あなたの腕力は装備によって影響を受けている。";
#else
		info[i++] = "Your strength is affected by your equipment.";
#endif
	}
	if (f1 & (TR1_INT))
	{
#ifdef JP
		info[i++] = "あなたの知能は装備によって影響を受けている。";
#else
		info[i++] = "Your intelligence is affected by your equipment.";
#endif
	}
	if (f1 & (TR1_WIS))
	{
#ifdef JP
		info[i++] = "あなたの賢さは装備によって影響を受けている。";
#else
		info[i++] = "Your wisdom is affected by your equipment.";
#endif
	}
	if (f1 & (TR1_DEX))
	{
#ifdef JP
		info[i++] = "あなたの器用さは装備によって影響を受けている。";
#else
		info[i++] = "Your dexterity is affected by your equipment.";
#endif
	}
	if (f1 & (TR1_CON))
	{
#ifdef JP
		info[i++] = "あなたの耐久力は装備によって影響を受けている。";
#else
		info[i++] = "Your constitution is affected by your equipment.";
#endif
	}
	if (f1 & (TR1_CHR))
	{
#ifdef JP
		info[i++] = "あなたの魅力は装備によって影響を受けている。";
#else
		info[i++] = "Your charisma is affected by your equipment.";
#endif
	}

	if (f1 & (TR1_MAGIC_MASTERY))
	{
#ifdef JP
		info[i++] = "あなたの魔法道具使用能力は装備によって影響を受けている。";
#else
		info[i++] = "Your magic device is affected by your equipment.";
#endif
	}
	if (f1 & (TR1_STEALTH))
	{
#ifdef JP
		info[i++] = "あなたの隠密行動能力は装備によって影響を受けている。";
#else
		info[i++] = "Your stealth is affected by your equipment.";
#endif
	}
	if (f1 & (TR1_SEARCH))
	{
#ifdef JP
		info[i++] = "あなたの探索能力は装備によって影響を受けている。";
#else
		info[i++] = "Your searching ability is affected by your equipment.";
#endif
	}
	if (f1 & (TR1_INFRA))
	{
#ifdef JP
		info[i++] = "あなたの赤外線視力は装備によって影響を受けている。";
#else
		info[i++] = "Your infravision is affected by your equipment.";
#endif
	}
	if (f1 & (TR1_TUNNEL))
	{
#ifdef JP
		info[i++] = "あなたの採掘能力は装備によって影響を受けている。";
#else
		info[i++] = "Your digging ability is affected by your equipment.";
#endif
	}
	if (f1 & (TR1_SPEED))
	{
#ifdef JP
		info[i++] = "あなたのスピードは装備によって影響を受けている。";
#else
		info[i++] = "Your speed is affected by your equipment.";
#endif
	}
	if (f1 & (TR1_BLOWS))
	{
#ifdef JP
		info[i++] = "あなたの攻撃速度は装備によって影響を受けている。";
#else
		info[i++] = "Your attack speed is affected by your equipment.";
#endif
	}


	/* Access the current weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Analyze the weapon */
	if (o_ptr->k_idx)
	{
		/* Indicate Blessing */
		if (f3 & (TR3_BLESSED))
		{
#ifdef JP
info[i++] = "あなたの武器は神の祝福を受けている。";
#else
			info[i++] = "Your weapon has been blessed by the gods.";
#endif

		}

		if (f1 & (TR1_CHAOTIC))
		{
#ifdef JP
info[i++] = "あなたの武器はカオスの属性をもつ。";
#else
			info[i++] = "Your weapon is branded with chaotic effect.";
#endif

		}

		/* Hack */
		if (f1 & (TR1_IMPACT))
		{
#ifdef JP
info[i++] = "あなたの武器は打撃で地震を発生することができる。";
#else
			info[i++] = "The impact of your weapon can cause earthquakes.";
#endif

		}

		if (f1 & (TR1_VORPAL))
		{
#ifdef JP
info[i++] = "あなたの武器は非常に鋭い。";
#else
			info[i++] = "Your weapon is very sharp.";
#endif

		}

		if (f1 & (TR1_VAMPIRIC))
		{
#ifdef JP
info[i++] = "あなたの武器は敵から生命力を吸収する。";
#else
			info[i++] = "Your weapon drains life from your foes.";
#endif

		}

		/* Special "Attack Bonuses" */
		if (f1 & (TR1_BRAND_ACID))
		{
#ifdef JP
info[i++] = "あなたの武器は敵を溶かす。";
#else
			info[i++] = "Your weapon melts your foes.";
#endif

		}
		if (f1 & (TR1_BRAND_ELEC))
		{
#ifdef JP
info[i++] = "あなたの武器は敵を感電させる。";
#else
			info[i++] = "Your weapon shocks your foes.";
#endif

		}
		if (f1 & (TR1_BRAND_FIRE))
		{
#ifdef JP
info[i++] = "あなたの武器は敵を燃やす。";
#else
			info[i++] = "Your weapon burns your foes.";
#endif

		}
		if (f1 & (TR1_BRAND_COLD))
		{
#ifdef JP
info[i++] = "あなたの武器は敵を凍らせる。";
#else
			info[i++] = "Your weapon freezes your foes.";
#endif

		}
		if (f1 & (TR1_BRAND_POIS))
		{
#ifdef JP
info[i++] = "あなたの武器は敵を毒で侵す。";
#else
			info[i++] = "Your weapon poisons your foes.";
#endif

		}

		/* Special "slay" flags */
		if (f1 & (TR1_SLAY_HUMAN))
		{
#ifdef JP
			info[i++] = "あなたの武器は人間に対して強い力を発揮する。";
#else
			info[i++] = "Your weapon strikes at humans with extra force.";
#endif
		}
		if (f1 & (TR1_SLAY_ANIMAL))
		{
#ifdef JP
info[i++] = "あなたの武器は動物に対して強い力を発揮する。";
#else
			info[i++] = "Your weapon strikes at animals with extra force.";
#endif

		}
		if (f1 & (TR1_SLAY_EVIL))
		{
#ifdef JP
info[i++] = "あなたの武器は邪悪なる存在に対して強い力を発揮する。";
#else
			info[i++] = "Your weapon strikes at evil with extra force.";
#endif

		}
		if (f1 & (TR1_SLAY_UNDEAD))
		{
#ifdef JP
info[i++] = "あなたの武器はアンデッドに対して神聖なる力を発揮する。";
#else
			info[i++] = "Your weapon strikes at undead with holy wrath.";
#endif

		}
		if (f1 & (TR1_SLAY_DEMON))
		{
#ifdef JP
info[i++] = "あなたの武器はデーモンに対して神聖なる力を発揮する。";
#else
			info[i++] = "Your weapon strikes at demons with holy wrath.";
#endif

		}
		if (f1 & (TR1_SLAY_ORC))
		{
#ifdef JP
info[i++] = "あなたの武器はオークに対して特に強い力を発揮する。";
#else
			info[i++] = "Your weapon is especially deadly against orcs.";
#endif

		}
		if (f1 & (TR1_SLAY_TROLL))
		{
#ifdef JP
info[i++] = "あなたの武器はトロルに対して特に強い力を発揮する。";
#else
			info[i++] = "Your weapon is especially deadly against trolls.";
#endif

		}
		if (f1 & (TR1_SLAY_GIANT))
		{
#ifdef JP
info[i++] = "あなたの武器はジャイアントに対して特に強い力を発揮する。";
#else
			info[i++] = "Your weapon is especially deadly against giants.";
#endif

		}
		if (f1 & (TR1_SLAY_DRAGON))
		{
#ifdef JP
info[i++] = "あなたの武器はドラゴンに対して特に強い力を発揮する。";
#else
			info[i++] = "Your weapon is especially deadly against dragons.";
#endif

		}

		/* Special "kill" flags */
		if (f1 & (TR1_KILL_DRAGON))
		{
#ifdef JP
info[i++] = "あなたの武器はドラゴンの天敵である。";
#else
			info[i++] = "Your weapon is a great bane of dragons.";
#endif

		}

		if (f2 & (TR2_THROW))
		{
#ifdef JP
info[i++] = "あなたの武器は投げるのに適している。";
#else
			info[i++] = "Your weapon can be thrown well.";
#endif

		}
	}


	/* Save the screen */
	screen_save();

	/* Erase the screen */
	for (k = 1; k < 24; k++) prt("", k, 13);

	/* Label the information */
#ifdef JP
prt("        あなたの状態:", 1, 15);
#else
	prt("     Your Attributes:", 1, 15);
#endif


	/* We will print on top of the map (column 13) */
	for (k = 2, j = 0; j < i; j++)
	{
		/* Show the info */
		prt(info[j], k++, 15);

		/* Every 20 entries (lines 2 to 21), start over */
		if ((k == 22) && (j+1 < i))
		{
#ifdef JP
prt("-- 続く --", k, 15);
#else
			prt("-- more --", k, 15);
#endif

			inkey();
			for (; k > 2; k--) prt("", k, 15);
		}
	}

	/* Pause */
#ifdef JP
prt("[何かキーを押すとゲームに戻ります]", k, 13);
#else
	prt("[Press any key to continue]", k, 13);
#endif

	inkey();

	/* Restore the screen */
	screen_load();
}


/*
 * Detect all traps on current panel
 */
bool detect_traps_aux(int range, bool known)
{
	int             x, y, dist;
	bool            detect = FALSE;
	cave_type       *c_ptr;

	/* Scan the current panel */
	for (y = 1; y < cur_hgt - 1; y++)
	{
		for (x = 1; x <= cur_wid - 1; x++)
		{
			if (distance(py, px, y, x) > range) continue;

			/* Access the grid */
			c_ptr = &cave[y][x];

			/* Detect invisible traps */
			if (c_ptr->feat == FEAT_INVIS)
			{
				/* Pick a trap */
				pick_trap(y, x);
			}

			/* Detect traps */
			if (is_trap(c_ptr->feat))
			{
				/* Hack -- Memorize */
				c_ptr->info |= (CAVE_MARK);

				/* Redraw */
				/* lite_spot(y, x); */

				/* Obvious */
				detect = TRUE;
			}
		}
	}

	/* Unmark safe grids */
	if (known || detect)
	{
		for (y = 1; y < cur_hgt - 1; y++)
		{
			for (x = 1; x <= cur_wid - 1; x++)
			{
				dist = distance(py, px, y, x);
				if (dist > range) continue;

				/* Access the grid */
				c_ptr = &cave[y][x];

				if (dist < range)
					c_ptr->info |= (CAVE_IN_DETECT);

				c_ptr->info &= ~(CAVE_UNSAFE);

				/* Redraw */
				lite_spot(y, x);
			}
		}

		p_ptr->dtrap = TRUE;
	}

	/* Describe */
	if (detect)
	{
#ifdef JP
		msg_print("トラップの存在を感じとった！");
#else
		msg_print("You sense the presence of traps!");
#endif
	}

	/* Result */
	return (detect);
}

bool detect_traps(int range)
{
	return detect_traps_aux(range, TRUE);
}

/*
 * Detect all doors on current panel
 */
bool detect_doors(int range)
{
	int y, x;
	bool detect = FALSE;
	cave_type *c_ptr;

	/* Scan the panel */
	for (y = 1; y < cur_hgt - 1; y++)
	{
		for (x = 1; x < cur_wid - 1; x++)
		{
			if (distance(py, px, y, x) > range) continue;

			c_ptr = &cave[y][x];

			/* Detect secret doors */
			if (c_ptr->feat == FEAT_SECRET)
			{
				/* Pick a door */
				place_closed_door(y, x);
			}

			/* Detect doors */
			if (((c_ptr->feat >= FEAT_DOOR_HEAD) &&
			     (c_ptr->feat <= FEAT_DOOR_TAIL)) ||
			    ((c_ptr->feat == FEAT_OPEN) ||
			     (c_ptr->feat == FEAT_BROKEN)))
			{
				/* Hack -- Memorize */
				c_ptr->info |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				/* Obvious */
				detect = TRUE;
			}
		}
	}

	/* Describe */
	if (detect)
	{
#ifdef JP
		msg_print("ドアの存在を感じとった！");
#else
		msg_print("You sense the presence of doors!");
#endif
	}

	/* Result */
	return (detect);
}


/*
 * Detect all stairs on current panel
 */
bool detect_stairs(int range)
{
	int y, x;
	bool detect = FALSE;
	cave_type *c_ptr;

	/* Scan the panel */
	for (y = 1; y < cur_hgt - 1; y++)
	{
		for (x = 1; x < cur_wid - 1; x++)
		{
			if (distance(py, px, y, x) > range) continue;

			c_ptr = &cave[y][x];

			/* Detect stairs */
			if ((c_ptr->feat == FEAT_LESS) ||
			    (c_ptr->feat == FEAT_MORE) ||
				(c_ptr->feat == FEAT_LESS_LESS) ||
			    (c_ptr->feat == FEAT_MORE_MORE))
			{
				/* Hack -- Memorize */
				c_ptr->info |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				/* Obvious */
				detect = TRUE;
			}
		}
	}

	/* Describe */
	if (detect)
	{
#ifdef JP
		msg_print("階段の存在を感じとった！");
#else
		msg_print("You sense the presence of stairs!");
#endif
	}

	/* Result */
	return (detect);
}


/*
 * Detect any treasure on the current panel
 */
bool detect_treasure(int range)
{
	int y, x;
	bool detect = FALSE;
	cave_type *c_ptr;

	/* Scan the current panel */
	for (y = 1; y < cur_hgt; y++)
	{
		for (x = 1; x < cur_wid; x++)
		{
			if (distance(py, px, y, x) > range) continue;

			c_ptr = &cave[y][x];

			/* Notice embedded gold */
			if ((c_ptr->feat == FEAT_MAGMA_H) ||
			    (c_ptr->feat == FEAT_QUARTZ_H))
			{
				/* Expose the gold */
				c_ptr->feat += 0x02;
			}

			/* Magma/Quartz + Known Gold */
			if ((c_ptr->feat == FEAT_MAGMA_K) ||
			    (c_ptr->feat == FEAT_QUARTZ_K))
			{
				/* Hack -- Memorize */
				c_ptr->info |= (CAVE_MARK);

				/* Redraw */
				lite_spot(y, x);

				/* Detect */
				detect = TRUE;
			}
		}
	}

	/* Describe */
	if (detect)
	{
#ifdef JP
		msg_print("埋蔵された財宝の存在を感じとった！");
#else
		msg_print("You sense the presence of buried treasure!");
#endif
	}

	/* Result */
	return (detect);
}



/*
 * Detect all "gold" objects on the current panel
 */
bool detect_objects_gold(int range)
{
	int i, y, x;
	bool detect = FALSE;

	/* Scan objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Only detect nearby objects */
		if (distance(py, px, y, x) > range) continue;

		/* Detect "gold" objects */
		if (o_ptr->tval == TV_GOLD)
		{
			/* Hack -- memorize it */
			o_ptr->marked |= OM_FOUND;

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			detect = TRUE;
		}
	}

	/* Describe */
	if (detect)
	{
#ifdef JP
		msg_print("財宝の存在を感じとった！");
#else
		msg_print("You sense the presence of treasure!");
#endif
	}

	if (detect_monsters_string(range, "$"))
	{
		detect = TRUE;
	}

	/* Result */
	return (detect);
}


/*
 * Detect all "normal" objects on the current panel
 */
bool detect_objects_normal(int range)
{
	int i, y, x;
	bool detect = FALSE;

	/* Scan objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Only detect nearby objects */
		if (distance(py, px, y, x) > range) continue;

		/* Detect "real" objects */
		if (o_ptr->tval != TV_GOLD)
		{
			/* Hack -- memorize it */
			o_ptr->marked |= OM_FOUND;

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			detect = TRUE;
		}
	}

	/* Describe */
	if (detect)
	{
#ifdef JP
		msg_print("アイテムの存在を感じとった！");
#else
		msg_print("You sense the presence of objects!");
#endif
	}

	if (detect_monsters_string(range, "!=?|/`"))
	{
		detect = TRUE;
	}

	/* Result */
	return (detect);
}

/*
 * Detect all "magic" objects on the current panel.
 *
 * This will light up all spaces with "magic" items, including artifacts,
 * ego-items, potions, scrolls, books, rods, wands, staves, amulets, rings,
 * and "enchanted" items of the "good" variety.
 *
 * It can probably be argued that this function is now too powerful.
 */
bool detect_objects_magic(int range)
{
	int i, y, x, tv;
	bool detect = FALSE;

	/* Scan all objects */
	for (i = 1; i < o_max; i++)
	{
		object_type *o_ptr = &o_list[i];

		/* Skip dead objects */
		if (!o_ptr->k_idx) continue;

		/* Skip held objects */
		if (o_ptr->held_m_idx) continue;

		/* Location */
		y = o_ptr->iy;
		x = o_ptr->ix;

		/* Only detect nearby objects */
		if (distance(py, px, y, x) > range) continue;

		/* Examine the tval */
		tv = o_ptr->tval;

		/* Artifacts, misc magic items, or enchanted wearables */
		if (artifact_p(o_ptr) ||
			ego_item_p(o_ptr) ||
			o_ptr->art_name ||
		    (tv == TV_AMULET) ||
			(tv == TV_RING) ||
		    (tv == TV_STAFF) ||
			(tv == TV_WAND) ||
			(tv == TV_ROD) ||
		    (tv == TV_SCROLL) ||
			(tv == TV_POTION) ||
		    (tv == TV_LIFE_BOOK) ||
			(tv == TV_SORCERY_BOOK) ||
		    ((o_ptr->to_a > 0) || (o_ptr->to_h + o_ptr->to_d > 0)))
		{
			/* Memorize the item */
			o_ptr->marked |= OM_FOUND;

			/* Redraw */
			lite_spot(y, x);

			/* Detect */
			detect = TRUE;
		}
	}

	/* Describe */
	if (detect)
	{
#ifdef JP
		msg_print("魔法のアイテムの存在を感じとった！");
#else
		msg_print("You sense the presence of magic objects!");
#endif
	}

	/* Return result */
	return (detect);
}


/*
 * Detect all "normal" monsters on the current panel
 */
/*
 * Detect all "normal" monsters on the current panel
 */
bool detect_monsters_normal(int range)
{
	int i, y, x;
	bool flag = FALSE;

	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (distance(py, px, y, x) > range) continue;

		/* Detect all non-invisible monsters */
		if ((!(r_ptr->flags2 & RF2_INVISIBLE)) ||
		    p_ptr->see_inv || p_ptr->tim_invis)
		{
			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Update the monster */
			update_mon(i, FALSE);

			/* Detect */
			flag = TRUE;
		}
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
#ifdef JP
		msg_print("モンスターの存在を感じとった！");
#else
		msg_print("You sense the presence of monsters!");
#endif
	}

	/* Result */
	return (flag);
}


/*
 * Detect all "invisible" monsters around the player
 */
bool detect_monsters_invis(int range)
{
	int i, y, x;
	bool flag = FALSE;

	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (distance(py, px, y, x) > range) continue;

		/* Detect invisible monsters */
		if (r_ptr->flags2 & RF2_INVISIBLE)
		{
			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Update the monster */
			update_mon(i, FALSE);

			/* Detect */
			flag = TRUE;
		}
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
#ifdef JP
		msg_print("透明な生物の存在を感じとった！");
#else
		msg_print("You sense the presence of invisible creatures!");
#endif
	}

	/* Result */
	return (flag);
}



/*
 * Detect all "evil" monsters on current panel
 */
bool detect_monsters_evil(int range)
{
	int i, y, x;
	bool flag = FALSE;

	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (distance(py, px, y, x) > range) continue;

		/* Detect evil monsters */
		if (r_ptr->flags3 & RF3_EVIL)
		{
			/* Take note that they are evil */
			r_ptr->r_flags3 |= (RF3_EVIL);

			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Update the monster */
			update_mon(i, FALSE);

			/* Detect */
			flag = TRUE;
		}
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
#ifdef JP
		msg_print("邪悪なる生物の存在を感じとった！");
#else
		msg_print("You sense the presence of evil creatures!");
#endif
	}

	/* Result */
	return (flag);
}


/*
 * Detect all (string) monsters on current panel
 */
bool detect_monsters_string(int range, cptr Match)
{
	int i, y, x;
	bool flag = FALSE;

	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (distance(py, px, y, x) > range) continue;

		/* Detect monsters with the same symbol */
		if (my_strchr(Match, r_ptr->d_char))
		{
			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Update the monster */
			update_mon(i, FALSE);

			/* Detect */
			flag = TRUE;
		}
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
#ifdef JP
		msg_print("モンスターの存在を感じとった！");
#else
		msg_print("You sense the presence of monsters!");
#endif
	}

	/* Result */
	return (flag);
}


/*
 * A "generic" detect monsters routine, tagged to flags3
 */
/*
 * A "generic" detect monsters routine, tagged to flags3
 */
bool detect_monsters_xxx(int range, u32b match_flag)
{
	int  i, y, x;
	bool flag = FALSE;
#ifdef JP
	cptr desc_monsters = "変なモンスター";
#else
	cptr desc_monsters = "weird monsters";
#endif

	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (distance(py, px, y, x) > range) continue;

		/* Detect evil monsters */
		if (r_ptr->flags3 & (match_flag))
		{
			/* Take note that they are something */
			r_ptr->r_flags3 |= (match_flag);

			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Update the monster */
			update_mon(i, FALSE);

			/* Detect */
			flag = TRUE;
		}
	}

	/* Describe */
	if (flag)
	{
		switch (match_flag)
		{
			case RF3_DEMON:
#ifdef JP
				desc_monsters = "デーモン";
#else
				desc_monsters = "demons";
#endif
				break;
			case RF3_UNDEAD:
#ifdef JP
				desc_monsters = "アンデッド";
#else
				desc_monsters = "the undead";
#endif
				break;
		}

		/* Describe result */
#ifdef JP
		msg_format("%sの存在を感じとった！", desc_monsters);
#else
		msg_format("You sense the presence of %s!", desc_monsters);
#endif
		msg_print(NULL);
	}

	/* Result */
	return (flag);
}


/*
 * Detect everything
 */
bool detect_all(int range)
{
	bool detect = FALSE;

	/* Detect everything */
	if (detect_traps(range)) detect = TRUE;
	if (detect_doors(range)) detect = TRUE;
	if (detect_stairs(range)) detect = TRUE;
	if (detect_treasure(range)) detect = TRUE;
	if (detect_objects_gold(range)) detect = TRUE;
	if (detect_objects_normal(range)) detect = TRUE;
	if (detect_monsters_invis(range)) detect = TRUE;
	if (detect_monsters_normal(range)) detect = TRUE;

	/* Result */
	return (detect);
}


/*
 * Apply a "project()" directly to all viewable monsters
 *
 * Note that affected monsters are NOT auto-tracked by this usage.
 *
 * To avoid misbehavior when monster deaths have side-effects,
 * this is done in two passes. -- JDL
 */
bool project_hack(int typ, int dam)
{
	int     i, x, y;
	u16b    flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;
	bool    obvious = FALSE;


	/* Mark all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Require line of sight */
		if (!player_has_los_bold(y, x)) continue;

		/* Mark the monster */
		m_ptr->mflag |= (MFLAG_TEMP);
	}

	/* Affect all marked monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];

		/* Skip unmarked monsters */
		if (!(m_ptr->mflag & (MFLAG_TEMP))) continue;

		/* Remove mark */
		m_ptr->mflag &= ~(MFLAG_TEMP);

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Jump directly to the target monster */
		if (project(0, 0, y, x, dam, typ, flg)) obvious = TRUE;
	}

	/* Result */
	return (obvious);
}


/*
 * Speed monsters
 */
bool speed_monsters(void)
{
	return (project_hack(GF_OLD_SPEED, p_ptr->lev));
}

/*
 * Slow monsters
 */
bool slow_monsters(void)
{
	return (project_hack(GF_OLD_SLOW, p_ptr->lev));
}

/*
 * Sleep monsters
 */
bool sleep_monsters(void)
{
	return (project_hack(GF_OLD_SLEEP, p_ptr->lev));
}


/*
 * Banish evil monsters
 */
bool banish_evil(int dist)
{
	return (project_hack(GF_AWAY_EVIL, dist));
}


/*
 * Turn undead
 */
bool turn_undead(void)
{
	return (project_hack(GF_TURN_UNDEAD, p_ptr->lev));
}


/*
 * Dispel undead monsters
 */
bool dispel_undead(int dam)
{
	return (project_hack(GF_DISP_UNDEAD, dam));
}

/*
 * Dispel evil monsters
 */
bool dispel_evil(int dam)
{
	return (project_hack(GF_DISP_EVIL, dam));
}

/*
 * Dispel good monsters
 */
bool dispel_good(int dam)
{
	return (project_hack(GF_DISP_GOOD, dam));
}

/*
 * Dispel all monsters
 */
bool dispel_monsters(int dam)
{
	return (project_hack(GF_DISP_ALL, dam));
}

/*
 * Dispel 'living' monsters
 */
bool dispel_living(int dam)
{
	return (project_hack(GF_DISP_LIVING, dam));
}

/*
 * Dispel demons
 */
bool dispel_demons(int dam)
{
	return (project_hack(GF_DISP_DEMON, dam));
}


/*
 * Wake up all monsters, and speed up "los" monsters.
 */
void aggravate_monsters(int who)
{
	int     i;
	bool    sleep = FALSE;
	bool    speed = FALSE;


	/* Aggravate everyone nearby */
	for (i = 1; i < m_max; i++)
	{
		monster_type    *m_ptr = &m_list[i];
		monster_race    *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip aggravating monster (or player) */
		if (i == who) continue;

		/* Wake up nearby sleeping monsters */
		if (m_ptr->cdis < MAX_SIGHT * 2)
		{
			/* Wake up */
			if (m_ptr->csleep)
			{
				/* Wake up */
				m_ptr->csleep = 0;
				if (r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) p_ptr->update |= (PU_MON_LITE);
				sleep = TRUE;

				/* Redraw (later) if needed */
				if (p_ptr->health_who == i)
					p_ptr->redraw |= (PR_HEALTH);
			}
		}

		/* Speed up monsters in line of sight */
		if (player_has_los_bold(m_ptr->fy, m_ptr->fx))
		{
			/* Speed up (instantly) to racial base + 10 */
			if (m_ptr->mspeed < r_ptr->speed + 10)
			{
				/* Speed up */
				m_ptr->mspeed = r_ptr->speed + 10;
				speed = TRUE;
			}
		}
	}

	/* Messages */
#ifdef JP
if (speed) msg_print("付近で何かが突如興奮したような感じを受けた！");
else if (sleep) msg_print("何かが突如興奮したような騒々しい音が遠くに聞こえた！");
#else
	if (speed) msg_print("You feel a sudden stirring nearby!");
	else if (sleep) msg_print("You hear a sudden stirring in the distance!");
#endif

}


/*
 * ironman_hengbandONの時抹殺に抵抗できる仕様を追加。powerが0なら抵抗不可。
 */
bool remove_monster(int power, int m_idx, cptr s)
{
	int msec = delay_factor * delay_factor * delay_factor;
	bool resist = FALSE;
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	bool seen = is_seen(m_ptr);

	/* Hack -- Skip Unique Monsters */
	if (r_ptr->flags1 & (RF1_UNIQUE)) resist = TRUE;

	/* Hack -- Skip Quest Monsters */
	if (r_ptr->flags1 & RF1_QUESTOR) resist = TRUE;

	/* Hack -- Monster have resistance to genocide */
	if (power && (m_ptr->flags & MF_NO_GENO)) resist = TRUE;

	/* Saving throw */
	if (ironman_hengband && power && r_ptr->level > randint1(power)) resist = TRUE;

	if (resist)
	{
		char m_name[80];
		monster_desc(m_name, m_ptr, 0);
		if (seen)
		{
#ifdef JP
			msg_format("%^sには効果がなかった。", m_name);
#else
			msg_format("%^s is unaffected. ", m_name);
#endif
		}
		if (m_ptr->csleep)
		{
			m_ptr->csleep = 0;
			if (r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) p_ptr->update |= (PU_MON_LITE);

			if (seen)
			{
#ifdef JP
				msg_format("%^sは目を覚ました。", m_name);
#else
				msg_format("%^s wakes up.", m_name);
#endif
			}
		}
		if (is_friendly(m_ptr))
		{
			set_hostile(m_ptr);

			if (seen)
			{
#ifdef JP
				msg_format("%^sは怒った！。", m_name);
#else
				msg_format("%^s gets angry!", m_name);
#endif
			}
		}

		/* Monster occationally acquire resistance to genocide */
		if (one_in_(13)) m_ptr->flags |= MF_NO_GENO;
	}
	else
	{
		/* Delete the monster */
		delete_monster_idx(m_idx);
	}

	if (power)
	{
#ifdef JP
		take_hit(randint1(3), format("%^sの呪文を唱えた疲労", s));
#else
		take_hit(randint1(3), format("the strain of casting %^s", s));
#endif
	}

	/* Visual feedback */
	move_cursor_relative(py, px);

	/* Redraw */
	p_ptr->redraw |= (PR_HP);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);

	/* Handle */
	handle_stuff();

	/* Fresh */
	Term_fresh();

	/* Delay */
	Term_xtra(TERM_XTRA_DELAY, msec);

	return (!resist);
}


/*
 * Delete terget non-unique/non-quest monsters from the level
 */
bool annihilation(int power, int dir)
{
	return fire_ball_hide(GF_GENOCIDE, dir, power, 0);
}


/*
 * Delete all non-unique/non-quest monsters of a given "type" from the level
 */
bool genocide(int power)
{
	int     i;
	char    typ;
	bool    result = FALSE;

	/* Prevent genocide in quest levels */
	if ((p_ptr->inside_quest && quest[p_ptr->inside_quest].type != QUEST_TYPE_RANDOM))
	{
		return (FALSE);
	}

	/* Mega-Hack -- Get a monster symbol */
#ifdef JP
	(void)(get_com("どの種類(文字)のモンスターを抹殺しますか: ", &typ));
#else
	(void)(get_com("Choose a monster race (by symbol) to genocide: ", &typ));
#endif

	/* Delete the monsters of that "type" */
	for (i = 1; i < m_max; i++)
	{
		monster_type    *m_ptr = &m_list[i];
		monster_race    *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip "wrong" monsters */
		if (r_ptr->d_char != typ) continue;

		/* Take note */
#ifdef JP
		result |= remove_monster(power, i, "抹殺");
#else
		result |= remove_monster(power, i, "Genocide");
#endif
	}

	return (result);
}


/*
 * Delete all nearby (non-unique) monsters
 */
/*
 * ironman_hengbandONの時抹殺に抵抗できる仕様を追加。powerが0なら抵抗不可。
 */
bool mass_genocide(int power)
{
	int     i;
	bool    result = FALSE;


	/* Prevent mass genocide in quest levels */
	if ((p_ptr->inside_quest && quest[p_ptr->inside_quest].type != QUEST_TYPE_RANDOM))
	{
		return (FALSE);
	}

	/* Delete the (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type    *m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip distant monsters */
		if (m_ptr->cdis > MAX_SIGHT) continue;

		/* Note effect */
#ifdef JP
		result |= remove_monster(power, i, "周辺抹殺");
#else
		result |= remove_monster(power, i, "Mass Genocide");
#endif
	}

	return (result);
}



/*
 * Probe nearby monsters
 */
bool probing(void)
{
	int     i, speed;
	bool    probe = FALSE;
	char buf[256];


	/* Probe all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Require line of sight */
		if (!player_has_los_bold(m_ptr->fy, m_ptr->fx)) continue;

		/* Probe visible monsters */
		if (m_ptr->ml)
		{
			char key;
			char m_name[80];

			/* Start the message */
#ifdef JP
			if (!probe) { msg_print("調査中..."); msg_print(NULL); }
#else
			if (!probe) { msg_print("Probing..."); msg_print(NULL); }
#endif

			/* Get "the monster" or "something" */
			monster_desc(m_name, m_ptr, 0x04);

			speed = m_ptr->mspeed - 110;
			if(m_ptr->hasted) speed += 10;
			if(m_ptr->slowed) speed -= 10;

			/* Describe the monster */
#ifdef JP
			sprintf(buf,"%s ... HP:%d/%d AC:%d 速度:%s%d ", m_name, m_ptr->hp, m_ptr->maxhp, r_ptr->ac, (speed > 0) ? "+" : "", speed);
#else
			sprintf(buf, "%s ... HP:%d/%d AC:%d speed:%s%d ", m_name, m_ptr->hp, m_ptr->maxhp, r_ptr->ac, (speed > 0) ? "+" : "", speed);
#endif
			buf[strlen(buf)-1] = '\0';
#ifdef JP
			prt(format("%s ('r': 思い出)", buf),0,0);
#else
			prt(format("%s ('r' to recall)", buf),0,0);
#endif

			/* HACK : Add the line to message buffer */
			message_add(buf);
			p_ptr->window |= (PW_MESSAGE);
			window_stuff();

			/* Learn all of the non-spell, non-treasure flags */
			lore_do_probe(i);

			if (m_ptr->ml) move_cursor_relative(m_ptr->fy, m_ptr->fx);
			key = inkey();

			Term_erase(0, 0, 255);

			if (key == 'r')
			{
				bool oldcheat = cheat_know;

				cheat_know = TRUE;
				screen_roff(m_ptr->r_idx, 1);
				cheat_know = oldcheat;

				(void)inkey();
				do_cmd_redraw();
			}

			/* Probe worked */
			probe = TRUE;
		}
	}

	/* Done */
	if (probe)
	{
#ifdef JP
		msg_print("これで全部です。");
#else
		msg_print("That's all.");
#endif
	}

	/* Result */
	return (probe);
}



/*
 * The spell of destruction
 *
 * This spell "deletes" monsters (instead of "killing" them).
 *
 * Later we may use one function for both "destruction" and
 * "earthquake" by using the "full" to select "destruction".
 */
bool destroy_area(int y1, int x1, int r, int full)
{
	int       y, x, k, t;
	cave_type *c_ptr;
	bool      flag = FALSE;

	/* Unused */
	(void)full;

	/* Prevent destruction of quest levels and town */
	if ((p_ptr->inside_quest && quest[p_ptr->inside_quest].type != QUEST_TYPE_RANDOM) || !dun_level)
	{
		return (FALSE);
	}

	/* Big area of affect */
	for (y = (y1 - r); y <= (y1 + r); y++)
	{
		for (x = (x1 - r); x <= (x1 + r); x++)
		{
			/* Skip illegal grids */
			if (!in_bounds(y, x)) continue;

			/* Extract the distance */
			k = distance(y1, x1, y, x);

			/* Stay in the circle of death */
			if (k > r) continue;

			/* Access the grid */
			c_ptr = &cave[y][x];

			/* Lose room and vault */
			c_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY | CAVE_UNSAFE);

			/* Lose light and knowledge */
			c_ptr->info &= ~(CAVE_MARK | CAVE_GLOW);

			/* Hack -- Notice player affect */
			if ((x == px) && (y == py))
			{
				/* Hurt the player later */
				flag = TRUE;

				/* Do not hurt this grid */
				continue;
			}

			/* Hack -- Skip the epicenter */
			if ((y == y1) && (x == x1)) continue;

			if (r_info[m_list[c_ptr->m_idx].r_idx].flags1 & RF1_QUESTOR)
			{
				/* Heal the monster */
				m_list[c_ptr->m_idx].hp = m_list[c_ptr->m_idx].maxhp;

				/* Try to teleport away quest monsters */
				if (!teleport_away(c_ptr->m_idx, (r * 2) + 1)) continue;
			}
			else
			{
				/* Delete the monster (if any) */
				delete_monster(y, x);
			}

			/* Preserve Artifacts */
			if (ironman_hengband && preserve_mode)
			{
				s16b this_o_idx, next_o_idx;

				/* Scan all objects in the grid */
				for (this_o_idx = c_ptr->o_idx; this_o_idx; this_o_idx = next_o_idx)
				{
					object_type *o_ptr;

					/* Acquire object */
					o_ptr = &o_list[this_o_idx];

					/* Acquire next object */
					next_o_idx = o_ptr->next_o_idx;

					/* Hack -- Preserve unknown artifacts */
					if (artifact_p(o_ptr) && !object_known_p(o_ptr))
					{
						/* Mega-Hack -- Preserve the artifact */
						a_info[o_ptr->name1].cur_num = 0;
					}
				}
			}
					
			/* Destroy "valid" grids */
			if (cave_valid_bold(y, x) ||
			    (ironman_hengband && !cave_perma_bold(y, x)))
			{
				/* Delete objects */
				delete_object(y, x);

				/* Wall (or floor) type */
				t = randint0(200);

				/* Granite */
				if (t < 20)
				{
					/* Create granite wall */
					c_ptr->feat = FEAT_WALL_EXTRA;
				}

				/* Quartz */
				else if (t < 70)
				{
					/* Create quartz vein */
					c_ptr->feat = FEAT_QUARTZ;
				}

				/* Magma */
				else if (t < 100)
				{
					/* Create magma vein */
					c_ptr->feat = FEAT_MAGMA;
				}

				/* Floor */
				else
				{
					/* Create floor */
					c_ptr->feat = FEAT_FLOOR;
				}
			}
		}
	}


	/* Hack -- Affect player */
	if (flag)
	{
		/* Message */
#ifdef JP
msg_print("燃えるような閃光が発生した！");
#else
		msg_print("There is a searing blast of light!");
#endif


		/* Blind the player */
		if (!p_ptr->resist_blind && !p_ptr->resist_lite)
		{
			/* Become blind */
			(void)set_blind(p_ptr->blind + 10 + randint1(10));
		}
	}

	forget_flow();

	/* Mega-Hack -- Forget the view and lite */
	p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	/* Success */
	return (TRUE);
}


/*
 * Induce an "earthquake" of the given radius at the given location.
 *
 * This will turn some walls into floors and some floors into walls.
 *
 * The player will take damage and "jump" into a safe grid if possible,
 * otherwise, he will "tunnel" through the rubble instantaneously.
 *
 * Monsters will take damage, and "jump" into a safe grid if possible,
 * otherwise they will be "buried" in the rubble, disappearing from
 * the level in the same way that they do when genocided.
 *
 * Note that thus the player and monsters (except eaters of walls and
 * passers through walls) will never occupy the same grid as a wall.
 * Note that as of now (2.7.8) no monster may occupy a "wall" grid, even
 * for a single turn, unless that monster can pass_walls or kill_walls.
 * This has allowed massive simplification of the "monster" code.
 */
bool earthquake_aux(int cy, int cx, int r, int m_idx)
{
	int             i, t, y, x, yy, xx, dy, dx, oy, ox;
	int             damage = 0;
	int             sn = 0, sy = 0, sx = 0;
	bool            hurt = FALSE;
	cave_type       *c_ptr;
	bool            map[32][32];
	cptr            s=NULL;


	/* Prevent destruction of quest levels and town */
	if ((p_ptr->inside_quest && quest[p_ptr->inside_quest].type != QUEST_TYPE_RANDOM) || !dun_level)
	{
		return (FALSE);
	}

	/* Paranoia -- Enforce maximum range */
	if (r > 12) r = 12;

	/* Clear the "maximal blast" area */
	for (y = 0; y < 32; y++)
	{
		for (x = 0; x < 32; x++)
		{
			map[y][x] = FALSE;
		}
	}

	/* Check around the epicenter */
	for (dy = -r; dy <= r; dy++)
	{
		for (dx = -r; dx <= r; dx++)
		{
			/* Extract the location */
			yy = cy + dy;
			xx = cx + dx;

			/* Skip illegal grids */
			if (!in_bounds(yy, xx)) continue;

			/* Skip distant grids */
			if (distance(cy, cx, yy, xx) > r) continue;

			/* Access the grid */
			c_ptr = &cave[yy][xx];

			/* Lose room and vault */
			c_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY | CAVE_UNSAFE);

			/* Lose light and knowledge */
			c_ptr->info &= ~(CAVE_GLOW | CAVE_MARK);

			/* Skip the epicenter */
			if (!dx && !dy) continue;

			/* Skip most grids */
			if (randint0(100) < 85) continue;

			/* Damage this grid */
			map[16+yy-cy][16+xx-cx] = TRUE;

			/* Hack -- Take note of player damage */
			if ((yy == py) && (xx == px)) hurt = TRUE;
		}
	}

	/* First, affect the player (if necessary) */
	if (hurt)
	{
		/* Check around the player */
		for (i = 0; i < 8; i++)
		{
			/* Access the location */
			y = py + ddy_ddd[i];
			x = px + ddx_ddd[i];

			/* Skip non-empty grids */
			if (!cave_empty_bold(y, x)) continue;

			/* Important -- Skip "quake" grids */
			if (map[16+y-cy][16+x-cx]) continue;

			/* Count "safe" grids */
			sn++;

			/* Randomize choice */
			if (randint0(sn) > 0) continue;

			/* Save the safe location */
			sy = y; sx = x;
		}

		/* Random message */
		switch (randint1(3))
		{
			case 1:
			{
#ifdef JP
				msg_print("ダンジョンの壁が崩れた！");
#else
				msg_print("The cave ceiling collapses!");
#endif
				break;
			}
			case 2:
			{
#ifdef JP
				msg_print("ダンジョンの床が不自然にねじ曲がった！");
#else
				msg_print("The cave floor twists in an unnatural way!");
#endif
				break;
			}
			default:
			{
#ifdef JP
				msg_print("ダンジョンが揺れた！崩れた岩が頭に降ってきた！");
#else
				msg_print("The cave quakes!  You are pummeled with debris!");
#endif
				break;
			}
		}

		/* Hurt the player a lot */
		if (!sn)
		{
			/* Message and damage */
#ifdef JP
			msg_print("あなたはひどい怪我を負った！");
#else
			msg_print("You are severely crushed!");
#endif
			damage = 150 + randint1(100);
		}

		/* Destroy the grid, and push the player to safety */
		else
		{
			/* Calculate results */
			switch (randint1(3))
			{
				case 1:
				{
#ifdef JP
					msg_print("降り注ぐ岩をうまく避けた！");
#else
					msg_print("You nimbly dodge the blast!");
#endif
					damage = 0;
					break;
				}
				case 2:
				{
#ifdef JP
					msg_print("岩石があなたに直撃した!");
#else
					msg_print("You are bashed by rubble!");
#endif
					damage = damroll(10, 4);
					(void)set_stun(p_ptr->stun + randint1(50));
					break;
				}
				case 3:
				{
#ifdef JP
					msg_print("あなたは床と壁との間に挟まれてしまった！");
#else
					msg_print("You are crushed between the floor and ceiling!");
#endif
					damage = damroll(20, 4);
					(void)set_stun(p_ptr->stun + randint1(50));
					break;
				}
			}

			/* Save the old location */
			oy = py;
			ox = px;

			/* Move the player to the safe location */
			py = sy;
			px = sx;

			/* Redraw the old spot */
			lite_spot(oy, ox);

			/* Redraw the new spot */
			lite_spot(py, px);

			/* Check for new panel */
			verify_panel();
		}

		/* Important -- no wall on player */
		map[16+py-cy][16+px-cx] = FALSE;

		/* Take some damage */
		if (damage)
		{
			char m_name[80];
			monster_type *m_ptr = &m_list[m_idx];

			/* Describe the monster */
			monster_desc(m_name, m_ptr, 0);

			if (m_idx)
			{
#ifdef JP
				s = format("%sの起こした地震", m_name);
#else
				s = format("an earthquake caused by %s", m_name);
#endif
			}
			else
			{
#ifdef JP
				s = "地震";
#else
				s = "an earthquake";
#endif
			}
		}
		take_hit(damage, s);
	}


	/* Examine the quaked region */
	for (dy = -r; dy <= r; dy++)
	{
		for (dx = -r; dx <= r; dx++)
		{
			/* Extract the location */
			yy = cy + dy;
			xx = cx + dx;

			/* Skip unaffected grids */
			if (!map[16+yy-cy][16+xx-cx]) continue;

			/* Access the grid */
			c_ptr = &cave[yy][xx];

			/* Process monsters */
			if (c_ptr->m_idx)
			{
				monster_type *m_ptr = &m_list[c_ptr->m_idx];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];

				/* Quest monsters */
				if (r_ptr->flags1 & RF1_QUESTOR)
				{
					/* No wall on quest monsters */
					map[16+yy-cy][16+xx-cx] = FALSE;

					continue;
				}

				/* Most monsters cannot co-exist with rock */
				if (!(r_ptr->flags2 & (RF2_KILL_WALL)) &&
				    !(r_ptr->flags2 & (RF2_PASS_WALL)))
				{
					char m_name[80];

					/* Assume not safe */
					sn = 0;

					/* Monster can move to escape the wall */
					if (!(r_ptr->flags1 & (RF1_NEVER_MOVE)))
					{
						/* Look for safety */
						for (i = 0; i < 8; i++)
						{
							/* Access the grid */
							y = yy + ddy_ddd[i];
							x = xx + ddx_ddd[i];

							/* Skip non-empty grids */
							if (!cave_empty_bold(y, x)) continue;

							/* Hack -- no safety on glyph of warding */
							if (cave[y][x].feat == FEAT_GLYPH) continue;
							if (cave[y][x].feat == FEAT_MINOR_GLYPH) continue;

							/* ... nor on the Pattern */
							if ((cave[y][x].feat <= FEAT_PATTERN_XTRA2) &&
							    (cave[y][x].feat >= FEAT_PATTERN_START))
								continue;

							/* Important -- Skip "quake" grids */
							if (map[16+y-cy][16+x-cx]) continue;

							/* Count "safe" grids */
							sn++;

							/* Randomize choice */
							if (randint0(sn) > 0) continue;

							/* Save the safe grid */
							sy = y; sx = x;
						}
					}

					/* Describe the monster */
					monster_desc(m_name, m_ptr, 0);

					/* Scream in pain */
#ifdef JP
					msg_format("%^sは苦痛で泣きわめいた！", m_name);
#else
					msg_format("%^s wails out in pain!", m_name);
#endif

					/* Take damage from the quake */
					damage = (sn ? damroll(4, 8) : (m_ptr->hp + 1));

					/* Monster is certainly awake */
					m_ptr->csleep = 0;

					/* Apply damage directly */
					m_ptr->hp -= damage;

					/* Delete (not kill) "dead" monsters */
					if (m_ptr->hp < 0)
					{
						/* Message */
#ifdef JP
						msg_format("%^sは岩石に埋もれてしまった！", m_name);
#else
						msg_format("%^s is embedded in the rock!", m_name);
#endif

						/* Delete the monster */
						delete_monster(yy, xx);

						/* No longer safe */
						sn = 0;
					}

					/* Hack -- Escape from the rock */
					if (sn)
					{
						int m_idx = cave[yy][xx].m_idx;

						/* Update the new location */
						cave[sy][sx].m_idx = m_idx;

						/* Update the old location */
						cave[yy][xx].m_idx = 0;

						/* Move the monster */
						m_ptr->fy = sy;
						m_ptr->fx = sx;

						/* Update the monster (new location) */
						update_mon(m_idx, TRUE);

						/* Redraw the old grid */
						lite_spot(yy, xx);

						/* Redraw the new grid */
						lite_spot(sy, sx);
					}
				}
			}
		}
	}


	/* Examine the quaked region */
	for (dy = -r; dy <= r; dy++)
	{
		for (dx = -r; dx <= r; dx++)
		{
			/* Extract the location */
			yy = cy + dy;
			xx = cx + dx;

			/* Skip unaffected grids */
			if (!map[16+yy-cy][16+xx-cx]) continue;

			/* Access the cave grid */
			c_ptr = &cave[yy][xx];

			/* Paranoia -- never affect player */
			if ((yy == py) && (xx == px)) continue;

			/* Destroy location (if valid) */
			if (cave_valid_bold(yy, xx))
			{
				bool floor = cave_floor_bold(yy, xx);

				/* Delete objects */
				delete_object(yy, xx);

				/* Wall (or floor) type */
				t = (floor ? randint0(100) : 200);

				/* Granite */
				if (t < 20)
				{
					/* Create granite wall */
					c_ptr->feat = FEAT_WALL_EXTRA;
				}

				/* Quartz */
				else if (t < 70)
				{
					/* Create quartz vein */
					c_ptr->feat = FEAT_QUARTZ;
				}

				/* Magma */
				else if (t < 100)
				{
					/* Create magma vein */
					c_ptr->feat = FEAT_MAGMA;
				}

				/* Floor */
				else
				{
					/* Create floor */
					c_ptr->feat = FEAT_FLOOR;
				}
			}
		}
	}


	/* Mega-Hack -- Forget the view and lite */
	p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE);

	/* Update the monsters */
	p_ptr->update |= (PU_DISTANCE);

	/* Update the health bar */
	p_ptr->redraw |= (PR_HEALTH);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	/* Success */
	return (TRUE);
}

bool earthquake(int cy, int cx, int r)
{
	return earthquake_aux(cy, cx, r, 0);
}


/*
 * This routine clears the entire "temp" set.
 *
 * This routine will Perma-Lite all "temp" grids.
 *
 * This routine is used (only) by "lite_room()"
 *
 * Dark grids are illuminated.
 *
 * Also, process all affected monsters.
 *
 * SMART monsters always wake up when illuminated
 * NORMAL monsters wake up 1/4 the time when illuminated
 * STUPID monsters wake up 1/10 the time when illuminated
 */
static void cave_temp_room_lite(void)
{
	int i, j;

	/* Clear them all */
	for (i = 0; i < temp_n; i++)
	{
		for (j = 0; j < 8; j++)
		{
			int y = temp_y[i] + ddy_cdd[j];
			int x = temp_x[i] + ddx_cdd[j];

			cave_type *c_ptr = &cave[y][x];

			/* Verify */
			if (!in_bounds2(y, x)) continue;

			/* No longer in the array */
			c_ptr->info &= ~(CAVE_TEMP);

			/* Update only non-CAVE_GLOW grids */
			if (c_ptr->info & (CAVE_GLOW)) continue;

			/* Perma-Lite */
			c_ptr->info |= (CAVE_GLOW);

			/* Process affected monsters */
			if (c_ptr->m_idx)
			{
				int chance = 25;

				monster_type    *m_ptr = &m_list[c_ptr->m_idx];

				monster_race    *r_ptr = &r_info[m_ptr->r_idx];

				/* Update the monster */
				update_mon(c_ptr->m_idx, FALSE);

				/* Stupid monsters rarely wake up */
				if (r_ptr->flags2 & (RF2_STUPID)) chance = 10;

				/* Smart monsters always wake up */
				if (r_ptr->flags2 & (RF2_SMART)) chance = 100;

				/* Sometimes monsters wake up */
				if (m_ptr->csleep && (randint0(100) < chance))
				{
					/* Wake up! */
					m_ptr->csleep = 0;
					if (r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) p_ptr->update |= (PU_MON_LITE);

					/* Notice the "waking up" */
					if (m_ptr->ml)
					{
						char m_name[80];

						/* Acquire the monster name */
						monster_desc(m_name, m_ptr, 0);

						/* Dump a message */
#ifdef JP
msg_format("%^sが目を覚ました。", m_name);
#else
						msg_format("%^s wakes up.", m_name);
#endif

						
						/* Redraw the health bar */
						if (p_ptr->health_who == c_ptr->m_idx)
							p_ptr->redraw |= (PR_HEALTH);
					}
				}
			}

			/* Note */
			note_spot(y, x);

			/* Redraw */
			lite_spot(y, x);
		}
	}

	/* None left */
	temp_n = 0;
}



/*
 * This routine clears the entire "temp" set.
 *
 * This routine will "darken" all "temp" grids.
 *
 * In addition, some of these grids will be "unmarked".
 *
 * This routine is used (only) by "unlite_room()"
 *
 * Also, process all affected monsters
 */
static void cave_temp_room_unlite(void)
{
	int i, j;

	/* Clear them all */
	for (i = 0; i < temp_n; i++)
	{
		for (j = 0; j < 8; j++)
		{
			int y = temp_y[i] + ddy_cdd[j];
			int x = temp_x[i] + ddx_cdd[j];

			cave_type *c_ptr = &cave[y][x];

			/* Verify */
			if (!in_bounds2(y, x)) continue;

			/* No longer in the array */
			c_ptr->info &= ~(CAVE_TEMP);

			/* Darken the grid */
			c_ptr->info &= ~(CAVE_GLOW);

			/* Hack -- Forget "boring" grids */
			if ((c_ptr->feat <= FEAT_INVIS) || (c_ptr->feat == FEAT_WALL_INVIS))
			{
				/* Forget the grid */
				c_ptr->info &= ~(CAVE_MARK);

				/* Notice */
				note_spot(y, x);
			}

			/* Process affected monsters */
			if (c_ptr->m_idx)
			{
				/* Update the monster */
				update_mon(c_ptr->m_idx, FALSE);
			}

			/* Redraw */
			lite_spot(y, x);
		}
	}

	/* None left */
	temp_n = 0;
}


/*
 * Determine how much contiguous open space this grid is next to
 */
static int next_to_open(int cy, int cx)
{
	int i;

	int y, x;

	int len = 0;
	int blen = 0;

	for (i = 0; i < 16; i++)
	{
		y = cy + ddy_cdd[i % 8];
		x = cx + ddx_cdd[i % 8];

		/* Found a wall, break the length */
		if (!cave_floor_bold(y, x))
		{
			/* Track best length */
			if (len > blen)
			{
				blen = len;
			}

			len = 0;
		}
		else
		{
			len++;
		}
	}

	return (MAX(len, blen));
}


static int next_to_walls_adj(int cy, int cx)
{
	int i;

	int y, x;

	int c = 0;

	for (i = 0; i < 8; i++)
	{
		y = cy + ddy_ddd[i];
		x = cx + ddx_ddd[i];

		if (!cave_floor_bold(y, x)) c++;
	}

	return c;
}


/*
 * Aux function -- see below
 */
static void cave_temp_room_aux(int y, int x, bool only_room)
{
	cave_type *c_ptr;

	/* Verify */
	if (!in_bounds(y, x)) return;

	/* Get the grid */
	c_ptr = &cave[y][x];

	/* Avoid infinite recursion */
	if (c_ptr->info & (CAVE_TEMP)) return;

	/* Do not "leave" the current room */
	if (!(c_ptr->info & (CAVE_ROOM)))
	{
		if (only_room) return;

		/* If a wall, exit */
		if (!cave_floor_bold(y, x)) return;
		
		/* Do not exceed the maximum spell range */
		if (distance(py, px, y, x) > MAX_RANGE) return;
		
		
		/* Verify this grid */
		/*
		* The reason why it is ==6 instead of >5 is that 8 is impossible
		* due to the check for cave_bold above.
		* 7 lights dead-end corridors (you need to do this for the
		* checkboard interesting rooms, so that the boundary is lit
		* properly.
		* This leaves only a check for 6 bounding walls!
		*/
		if ((next_to_walls_adj(y, x) == 6) && (next_to_open(y, x) <= 1)) return;
	}

	/* Paranoia -- verify space */
	if (temp_n == TEMP_MAX) return;

	/* Mark the grid as "seen" */
	c_ptr->info |= (CAVE_TEMP);

	/* Add it to the "seen" set */
	temp_y[temp_n] = y;
	temp_x[temp_n] = x;
	temp_n++;
}

/*
 * Aux function -- see below
 */
static void cave_temp_lite_room_aux(int y, int x)
{
	cave_temp_room_aux(y, x, FALSE);
}

/*
 * Aux function -- see below
 */
static void cave_temp_unlite_room_aux(int y, int x)
{
	cave_temp_room_aux(y, x, TRUE);
}


/*
 * Illuminate any room containing the given location.
 */
void lite_room(int y1, int x1)
{
	int i, x, y;

	/* Add the initial grid */
	cave_temp_lite_room_aux(y1, x1);

	/* While grids are in the queue, add their neighbors */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		/* Walls get lit, but stop light */
		if (!cave_floor_bold(y, x)) continue;

		/* Spread adjacent */
		cave_temp_lite_room_aux(y + 1, x);
		cave_temp_lite_room_aux(y - 1, x);
		cave_temp_lite_room_aux(y, x + 1);
		cave_temp_lite_room_aux(y, x - 1);

		/* Spread diagonal */
		cave_temp_lite_room_aux(y + 1, x + 1);
		cave_temp_lite_room_aux(y - 1, x - 1);
		cave_temp_lite_room_aux(y - 1, x + 1);
		cave_temp_lite_room_aux(y + 1, x - 1);
	}

	/* Now, lite them all up at once */
	cave_temp_room_lite();
}


/*
 * Darken all rooms containing the given location
 */
void unlite_room(int y1, int x1)
{
	int i, x, y;

	/* Add the initial grid */
	cave_temp_unlite_room_aux(y1, x1);

	/* Spread, breadth first */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		/* Walls get dark, but stop darkness */
		if (!cave_floor_bold(y, x)) continue;

		/* Spread adjacent */
		cave_temp_unlite_room_aux(y + 1, x);
		cave_temp_unlite_room_aux(y - 1, x);
		cave_temp_unlite_room_aux(y, x + 1);
		cave_temp_unlite_room_aux(y, x - 1);

		/* Spread diagonal */
		cave_temp_unlite_room_aux(y + 1, x + 1);
		cave_temp_unlite_room_aux(y - 1, x - 1);
		cave_temp_unlite_room_aux(y - 1, x + 1);
		cave_temp_unlite_room_aux(y + 1, x - 1);
	}

	/* Now, darken them all at once */
	cave_temp_room_unlite();
}



/*
 * Hack -- call light around the player
 * Affect all monsters in the projection radius
 */
bool lite_area(int dam, int rad)
{
	u16b flg = PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
#ifdef JP
msg_print("白い光が辺りを覆った。");
#else
		msg_print("You are surrounded by a white light.");
#endif

	}

	/* Hook into the "project()" function */
	(void)project(0, rad, py, px, dam, GF_LITE_WEAK, flg);

	/* Lite up the room */
	lite_room(py, px);

	/* Assume seen */
	return (TRUE);
}


/*
 * Hack -- call darkness around the player
 * Affect all monsters in the projection radius
 */
bool unlite_area(int dam, int rad)
{
	u16b flg = PROJECT_GRID | PROJECT_KILL;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
#ifdef JP
msg_print("暗闇が辺りを覆った。");
#else
		msg_print("Darkness surrounds you.");
#endif

	}

	/* Hook into the "project()" function */
	(void)project(0, rad, py, px, dam, GF_DARK_WEAK, flg);

	/* Lite up the room */
	unlite_room(py, px);

	/* Assume seen */
	return (TRUE);
}



/*
 * Cast a ball spell
 * Stop if we hit a monster, act as a "ball"
 * Allow "target" mode to pass over monsters
 * Affect grids, objects, and monsters
 */
static bool fire_ball_aux(int typ, int dir, int dam, int rad, u16b flg)
{
	int tx, ty;

	/* Use the given direction */
	tx = px + 99 * ddx[dir];
	ty = py + 99 * ddy[dir];

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		flg &= ~(PROJECT_STOP);
		tx = target_col;
		ty = target_row;
	}

	/* Analyze the "dir" and the "target".  Hurt items on floor. */
	return (project(0, rad, ty, tx, dam, typ, flg));
}

bool fire_ball(int typ, int dir, int dam, int rad)
{
	u16b flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	return (fire_ball_aux(typ, dir, dam, rad, flg));
}

bool fire_ball_hide(int typ, int dir, int dam, int rad)
{
	u16b flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_HIDE;

	return (fire_ball_aux(typ, dir, dam, rad, flg));
}


/*
 * Switch position with a monster.
 */
bool teleport_swap(int dir)
{
	int tx, ty;
	cave_type * c_ptr;
	monster_type * m_ptr;
	monster_race * r_ptr;

	if ((dir == 5) && target_okay())
	{
		tx = target_col;
		ty = target_row;
	}
	else
	{
		tx = px + ddx[dir];
		ty = py + ddy[dir];
	}
	c_ptr = &cave[ty][tx];

	if (!c_ptr->m_idx)
	{
#ifdef JP
msg_print("それとは場所を交換できません。");
#else
		msg_print("You can't trade places with that!");
#endif


		/* Failure */
		return FALSE;
	}

	m_ptr = &m_list[c_ptr->m_idx];
	r_ptr = &r_info[m_ptr->r_idx];

	if (r_ptr->flags3 & RF3_RES_TELE)
	{
#ifdef JP
msg_print("テレポートを邪魔された！");
#else
		msg_print("Your teleportation is blocked!");
#endif


		/* Failure */
		return FALSE;
	}

	sound(SOUND_TELEPORT);

	cave[py][px].m_idx = c_ptr->m_idx;

	/* Update the old location */
	c_ptr->m_idx = 0;

	/* Move the monster */
	m_ptr->fy = (byte)py;
	m_ptr->fx = (byte)px;

	/* Move the player */
	px = tx;
	py = ty;

	tx = m_ptr->fx;
	ty = m_ptr->fy;

	/* Update the monster (new location) */
	update_mon(cave[ty][tx].m_idx, TRUE);

	/* Redraw the old grid */
	lite_spot(ty, tx);

	/* Redraw the new grid */
	lite_spot(py, px);

	/* Check for new panel (redraw map) */
	verify_panel();

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE);

	/* Update the monsters */
	p_ptr->update |= (PU_DISTANCE);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();

	/* Success */
	return TRUE;
}


/*
 * Hack -- apply a "projection()" in a direction (or at the target)
 */
bool project_hook(int typ, int dir, int dam, u16b flg)
{
	int tx, ty;

	/* Pass through the target if needed */
	if ((typ != GF_JUMP && typ != GF_JUMP_ATTACK) || dir != 5)
	{
		flg |= (PROJECT_THRU);
	}

	/* Use the given direction */
	tx = px + ddx[dir];
	ty = py + ddy[dir];

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		tx = target_col;
		ty = target_row;
	}

	/* Analyze the "dir" and the "target", do NOT explode */
	return (project(0, 0, ty, tx, dam, typ, flg));
}


/*
 * Cast a bolt spell
 * Stop if we hit a monster, as a "bolt"
 * Affect monsters (not grids or objects)
 */
bool fire_bolt(int typ, int dir, int dam)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(typ, dir, dam, flg));
}


/*
 * Cast a beam spell
 * Pass through monsters, as a "beam"
 * Affect monsters (not grids or objects)
 */
bool fire_beam(int typ, int dir, int dam)
{
	u16b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL;
	return (project_hook(typ, dir, dam, flg));
}


/*
 * Cast a bolt spell, or rarely, a beam spell
 */
bool fire_bolt_or_beam(int prob, int typ, int dir, int dam)
{
	if (randint0(100) < prob)
	{
		return (fire_beam(typ, dir, dam));
	}
	else
	{
		return (fire_bolt(typ, dir, dam));
	}
}


/*
 * Some of the old functions
 */
bool lite_line(int dir)
{
	u16b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL;
	return (project_hook(GF_LITE_WEAK, dir, damroll(6, 8), flg));
}


bool drain_life(int dir, int dam)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_OLD_DRAIN, dir, dam, flg));
}


bool wall_to_mud(int dir)
{
	u16b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	return (project_hook(GF_KILL_WALL, dir, 20 + randint1(30), flg));
}


bool wizard_lock(int dir)
{
	u16b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	return (project_hook(GF_JAM_DOOR, dir, 20 + randint1(30), flg));
}


bool destroy_door(int dir)
{
	u16b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (project_hook(GF_KILL_DOOR, dir, 0, flg));
}


bool disarm_trap(int dir)
{
	u16b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (project_hook(GF_KILL_TRAP, dir, 0, flg));
}


bool heal_monster(int dir)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_OLD_HEAL, dir, damroll(4, 6), flg));
}


bool speed_monster(int dir)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_OLD_SPEED, dir, p_ptr->lev, flg));
}


bool slow_monster(int dir)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_OLD_SLOW, dir, p_ptr->lev, flg));
}


bool sleep_monster(int dir)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_OLD_SLEEP, dir, p_ptr->lev, flg));
}


bool stasis_monster(int dir)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_STASIS, dir, p_ptr->lev, flg));
}


bool confuse_monster(int dir, int plev)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_OLD_CONF, dir, plev, flg));
}


bool stun_monster(int dir, int plev)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_STUN, dir, plev, flg));
}


bool poly_monster(int dir)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_OLD_POLY, dir, p_ptr->lev, flg));
}


bool clone_monster(int dir)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_OLD_CLONE, dir, 0, flg));
}


bool fear_monster(int dir, int plev)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_TURN_ALL, dir, plev, flg));
}


bool death_ray(int dir, int plev)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_DEATH_RAY, dir, plev * 200, flg));
}


bool teleport_monster(int dir)
{
	u16b flg = PROJECT_BEAM | PROJECT_KILL;
	return (project_hook(GF_AWAY_ALL, dir, MAX_SIGHT * 5, flg));
}


/*
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */
bool door_creation(void)
{
	u16b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, 1, py, px, 0, GF_MAKE_DOOR, flg));
}


bool trap_creation(void)
{
	u16b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, 1, py, px, 0, GF_MAKE_TRAP, flg));
}


bool glyph_creation(void)
{
	u16b flg = PROJECT_GRID | PROJECT_ITEM;
	return (project(0, 1, py, px, 0, GF_MAKE_GLYPH, flg));
}


bool wall_stone(void)
{
	u16b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;

	bool dummy = (project(0, 1, py, px, 0, GF_STONE_WALL, flg));

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE);

	/* Update the monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	return dummy;
}


bool destroy_doors_touch(void)
{
	u16b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, 1, py, px, 0, GF_KILL_DOOR, flg));
}


bool sleep_monsters_touch(void)
{
	u16b flg = PROJECT_KILL | PROJECT_HIDE;
	return (project(0, 1, py, px, p_ptr->lev, GF_OLD_SLEEP, flg));
}


void call_chaos(void)
{
	int Chaos_type, dummy, dir;
	int plev = p_ptr->lev;
	bool line_chaos = FALSE;

	int hurt_types[30] =
	{
		GF_ELEC,      GF_POIS,    GF_ACID,    GF_COLD,
		GF_FIRE,      GF_MISSILE, GF_ARROW,   GF_PLASMA,
		GF_HOLY_FIRE, GF_WATER,   GF_LITE,    GF_DARK,
		GF_FORCE,     GF_INERTIA, GF_MANA,    GF_METEOR,
		GF_ICE,       GF_CHAOS,   GF_NETHER,  GF_DISENCHANT,
		GF_SHARDS,    GF_SOUND,   GF_NEXUS,   GF_CONFUSION,
		GF_TIME,      GF_GRAVITY, GF_ROCKET,  GF_NUKE,
		GF_HELL_FIRE, GF_DISINTEGRATE
	};

	Chaos_type = hurt_types[randint0(30)];
	if (randint1(4) == 1) line_chaos = TRUE;

	if (randint1(6) == 1)
	{
		for (dummy = 1; dummy < 10; dummy++)
		{
			if (dummy - 5)
			{
				if (line_chaos)
					fire_beam(Chaos_type, dummy, 75);
				else
					fire_ball(Chaos_type, dummy, 75, 2);
			}
		}
	}
	else if (randint1(3) == 1)
	{
		fire_ball(Chaos_type, 0, 300, 8);
	}
	else
	{
		if (!get_aim_dir(&dir)) return;
		if (line_chaos)
			fire_beam(Chaos_type, dir, 150);
		else
			fire_ball(Chaos_type, dir, 150, 3 + (plev / 35));
	}
}


/*
 * Activate the evil Topi Ylinen curse
 * rr9: Stop the nasty things when a Cyberdemon is summoned
 * or the player gets paralyzed.
 */
bool activate_ty_curse(bool stop_ty, int *count)
{
	int stat = 0;

	u16b flg = (PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_JUMP);

	do
	{
		switch (randint1(34))
		{
			case 28: case 29:
			{
				if (!(*count))
				{
#ifdef JP
					msg_print("地面が揺れた...");
#else
					msg_print("The ground trembles...");
#endif
					earthquake(py, px, 5 + randint0(10));
					if (randint1(6) != 1) break;
				}
			}
			case 30: case 31:
			{
				if (!(*count))
				{
#ifdef JP
					msg_print("純粋な魔力の次元への扉が開いた！");
#else
					msg_print("A portal opens to a plane of raw mana!");
#endif
					destroy_area(py, px, 20, TRUE);
					project(1, 3, py, px, damroll(10, 5), GF_MANA, flg);
					if (randint1(6) != 1) break;
				}
			}
			case 32: case 33:
			{
				if (!(*count))
				{
#ifdef JP
					msg_print("周囲の空間が歪んだ！");
#else
					msg_print("Space warps about you!");
#endif
					teleport_player(damroll(10, 10));
					if (randint0(13)) (*count) += activate_hi_summon();
					if (randint1(6) != 1) break;
				}
			}
			case 34:
			{
#ifdef JP
				msg_print("エネルギーのうねりを感じた！");
#else
				msg_print("You feel a surge of energy!");
#endif
				wall_breaker();
				if (!randint0(7))
				{
					project(1, 7, py, px, 50, GF_KILL_WALL, flg);
				}
				if (randint1(6) != 1) break;
			}
			case 1: case 2: case 3: case 16: case 17:
			{
				aggravate_monsters(0);
				if (randint1(6) != 1) break;
			}
			case 4: case 5: case 6:
			{
				(*count) += activate_hi_summon();
				if (randint1(6) != 1) break;
			}
			case 7: case 8: case 9: case 18:
			{
				(*count) += summon_specific(0, py, px, dun_level, 0, TRUE, FALSE, FALSE);
				if (randint1(6) != 1) break;
			}
			case 10: case 11: case 12:
			{
#ifdef JP
				msg_print("生命力が体から吸い取られた気がする！");
#else
				msg_print("You feel your life draining away...");
#endif
				lose_exp(p_ptr->exp / 16);
				if (randint1(6) != 1) break;
			}
			case 13: case 14: case 15: case 19: case 20:
			{
				if (stop_ty || (p_ptr->free_act && (randint1(100) < p_ptr->skill_sav)))
				{
					/* Do nothing */ ;
				}
				else
				{
#ifdef JP
					msg_print("彫像になった気分だ！");
#else
					msg_print("You feel like a statue!");
#endif
					if (p_ptr->free_act)
					{
						set_paralyzed(p_ptr->paralyzed + 1);
					}
					else
					{
						set_paralyzed(p_ptr->paralyzed + randint1(13));
					}
					stop_ty = TRUE;
				}
				if (randint1(6) != 1) break;
			}
			case 21: case 22: case 23:
			{
				(void)do_dec_stat(randint0(6));
				if (randint1(6) != 1) break;
			}
			case 24:
			{
#ifdef JP
				msg_print("ほえ？私は誰？ここで何してる？");
#else
				msg_print("Huh? Who am I? What am I doing here?");
#endif
				lose_all_info();
				if (randint1(6) != 1) break;
			}
			case 25:
			{
				/*
				 * Only summon Cyberdemons deep in the dungeon.
				 */
				if ((dun_level > 65) && !stop_ty)
				{
					(*count) += summon_cyber(-1, py, px);
					stop_ty = TRUE;
					break;
				}
				if (randint1(6) != 1) break;
			}
			default:
			{
				while (stat < 6)
				{
					do
					{
						(void)do_dec_stat(stat);
					}
					while (randint1(2) == 1);

					stat++;
				}
			}
		}
	}
	while ((randint1(3) == 1) && !stop_ty);

	return stop_ty;
}


int activate_hi_summon(void)
{
	int i;
	int count = 0;

	for (i = 0; i < (randint1(9) + (dun_level / 40)); i++)
	{
		switch (randint1(26) + (dun_level / 20))
		{
			case 1: case 2:
				count += summon_specific(0, py, px, dun_level, SUMMON_ANT, TRUE, FALSE, FALSE);
				break;
			case 3: case 4:
				count += summon_specific(0, py, px, dun_level, SUMMON_SPIDER, TRUE, FALSE, FALSE);
				break;
			case 5: case 6:
				count += summon_specific(0, py, px, dun_level, SUMMON_HOUND, TRUE, FALSE, FALSE);
				break;
			case 7: case 8:
				count += summon_specific(0, py, px, dun_level, SUMMON_HYDRA, TRUE, FALSE, FALSE);
				break;
			case 9: case 10:
				count += summon_specific(0, py, px, dun_level, SUMMON_ANGEL, TRUE, FALSE, FALSE);
				break;
			case 11: case 12:
				count += summon_specific(0, py, px, dun_level, SUMMON_UNDEAD, TRUE, FALSE, FALSE);
				break;
			case 13: case 14:
				count += summon_specific(0, py, px, dun_level, SUMMON_DRAGON, TRUE, FALSE, FALSE);
				break;
			case 15: case 16:
				count += summon_specific(0, py, px, dun_level, SUMMON_DEMON, TRUE, FALSE, FALSE);
				break;
			case 17:
				count += summon_specific(0, py, px, dun_level, SUMMON_HI_DEMON, TRUE, FALSE, FALSE);
				break;
			case 18: case 19:
				count += summon_specific(0, py, px, dun_level, SUMMON_UNIQUE, TRUE, FALSE, FALSE);
				break;
			case 20: case 21:
				count += summon_specific(0, py, px, dun_level, SUMMON_HI_UNDEAD, TRUE, FALSE, FALSE);
				break;
			case 22: case 23:
				count += summon_specific(0, py, px, dun_level, SUMMON_HI_DRAGON, TRUE, FALSE, FALSE);
				break;
			case 24: case 25:
				count += summon_specific(0, py, px, 100, SUMMON_CYBER, TRUE, FALSE, FALSE);
				break;
			default:
				count += summon_specific(0, py, px, (((dun_level * 3) / 2) + 5), 0, TRUE, FALSE, FALSE);
		}
	}

	return count;
}


/* ToDo: check */
int summon_cyber(int who, int y, int x)
{
	int i;
	int max_cyber = (dun_level / 50) + randint1(6);
	int count = 0;

	bool friendly = FALSE;
	bool pet = FALSE;

	/* Summoned by a monster */
	if (who > 0)
	{
		monster_type *m_ptr = &m_list[who];
		friendly = is_friendly(m_ptr);
		pet = is_pet(m_ptr);
	}

	for (i = 0; i < max_cyber; i++)
	{
		count += summon_specific(who, y, x, 100, SUMMON_CYBER, FALSE, friendly, pet);
	}

	return count;
}


void wall_breaker(void)
{
	int i;
	int y, x;
	int attempts = 1000;

	if (randint1(80 + p_ptr->lev) < 70)
	{
		while (attempts--)
		{
			scatter(&y, &x, py, px, 4, 0);

			if ((y != py) || (x != px)) break;
		}

		project(0, 0, y, x, 20 + randint1(30), GF_KILL_WALL,
				  (PROJECT_BEAM | PROJECT_THRU | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL));
	}
	else if (randint1(100) > 30)
	{
		earthquake(py, px, 1);
	}
	else
	{
		int num = damroll(5, 3);

		for (i = 0; i < num; i++)
		{
			while (1)
			{
				scatter(&y, &x, py, px, 4, 0);

				if ((y != py) || (x != px)) break;
			}

			project(0, 0, y, x, 20 + randint1(30), GF_KILL_WALL,
					  (PROJECT_BEAM | PROJECT_THRU | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL));
		}
	}
}


/*
 * Detect all "nonliving", "undead" or "demonic" monsters on current panel
 */
bool detect_monsters_nonliving(void)
{
	int     i, y, x;
	bool    flag = FALSE;

	/* Scan monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		/* Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Only detect nearby monsters */
		if (!panel_contains(y, x)) continue;

		/* Detect non-living monsters */
		if (!monster_living(r_ptr))
		{
			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag |= (MFLAG_MARK | MFLAG_SHOW);

			/* Update the monster */
			update_mon(i, FALSE);

			/* Detect */
			flag = TRUE;
		}
	}

	/* Describe */
	if (flag)
	{
		/* Describe result */
#ifdef JP
msg_print("自然でないモンスターの存在を感じた！");
#else
		msg_print("You sense the presence of unnatural beings!");
#endif

	}

	/* Result */
	return (flag);
}


/*
 * Confuse monsters
 */
bool confuse_monsters(int dam)
{
	return (project_hack(GF_OLD_CONF, dam));
}


/*
 * Charm monsters
 */
bool charm_monsters(int dam)
{
	return (project_hack(GF_CHARM, dam));
}


/*
 * Charm animals
 */
bool charm_animals(int dam)
{
	return (project_hack(GF_CONTROL_ANIMAL, dam));
}


/*
 * Stun monsters
 */
bool stun_monsters(int dam)
{
	return (project_hack(GF_STUN, dam));
}


/*
 * Stasis monsters
 */
bool stasis_monsters(int dam)
{
	return (project_hack(GF_STASIS, dam));
}


/*
 * Mindblast monsters
 */
bool mindblast_monsters(int dam)
{
	return (project_hack(GF_PSI, dam));
}


/*
 * Banish all monsters
 */
bool banish_monsters(int dist)
{
	return (project_hack(GF_AWAY_ALL, dist));
}


/*
 * Turn evil
 */
bool turn_evil(int dam)
{
	return (project_hack(GF_TURN_EVIL, dam));
}


/*
 * Turn everyone
 */
bool turn_monsters(int dam)
{
	return (project_hack(GF_TURN_ALL, dam));
}


/*
 * Death-ray all monsters (note: OBSCENELY powerful)
 */
bool deathray_monsters(void)
{
	return (project_hack(GF_DEATH_RAY, p_ptr->lev * 200));
}


bool charm_monster(int dir, int plev)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CHARM, dir, plev, flg));
}


bool control_one_undead(int dir, int plev)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CONTROL_UNDEAD, dir, plev, flg));
}


bool charm_animal(int dir, int plev)
{
	u16b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CONTROL_ANIMAL, dir, plev, flg));
}


/*
 * Musou Attack Spell
 */
/* Nanka(突撃) */
bool charge_monster(void)
{
	int             y, x;
	int             dir;

	if (!get_rep_dir(&dir)) return FALSE;

	if (dir == 5) return FALSE;
	y = py + ddy[dir];
	x = px + ddx[dir];

	if (!cave[y][x].m_idx)
	{
#ifdef JP
		msg_print("その方向にはモンスターはいません。");
#else
		msg_print("You don't see any monster in this direction");
#endif
		return FALSE;
	}

	py_attack(y, x);

	if (!player_can_enter(cave[y][x].feat) || is_trap(cave[y][x].feat))
		return TRUE;

	y += ddy[dir];
	x += ddx[dir];

	if (player_can_enter(cave[y][x].feat) && !is_trap(cave[y][x].feat) && !cave[y][x].m_idx)
	{
		int oy, ox;

		msg_print(NULL);

		/* Save the old location */
		oy = py;
		ox = px;

		/* Move the player */
		py = y;
		px = x;

		/* Forget the flow */
		forget_flow();

		/* Redraw the old spot */
		lite_spot(oy, ox);

		/* Redraw the new spot */
		lite_spot(py, px);

		/* Check for new panel (redraw map) */
		verify_panel();

		/* Update stuff */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE);

		/* Update the monsters */
		p_ptr->update |= (PU_DISTANCE);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

		/* Handle stuff XXX XXX XXX */
		handle_stuff();
	}

	return TRUE;
}
