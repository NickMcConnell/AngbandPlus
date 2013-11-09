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
	s32b align_gne = friend_align_gne;

	u32b flgs[TR_FLAG_SIZE];

	object_type *o_ptr;

	char Dummy[80];
	char buf[80];
	char amfld_buf[80];

	cptr info[220];

	int plev = p_ptr->lev;
	int clev = p_ptr->cexp_info[p_ptr->pclass].clev;
	int mhp = p_ptr->mhp;
	int chp = p_ptr->chp;

	for (j = 0; j < TR_FLAG_SIZE; j++)
		flgs[j] = 0L;

	/* Acquire item flags from equipment */
	for (k = INVEN_RARM; k < INVEN_TOTAL; k++)
	{
		u32b tflgs[TR_FLAG_SIZE];

		o_ptr = &inventory[k];

		/* Skip non-objects */
		if (!o_ptr->k_idx) continue;

		object_flags(o_ptr, tflgs);

		/* Extract flags */
		for (j = 0; j < TR_FLAG_SIZE; j++)
			flgs[j] |= tflgs[j];
	}

	for (k = 0; k < ETHNICITY_NUM; k++) align_gne += chaos_frame[k];

	switch (p_ptr->pclass)
	{
	case CLASS_LICH:
	case CLASS_VAMPIRE:
		if (align_gne > -300) align_gne = -300;
		break;

	case CLASS_ANGELKNIGHT:
		if (align_gne < 300) align_gne = 300;
		break;
	}

#ifdef JP
	sprintf(Dummy, "現在のアラインメント : %s(%ld)-%s(%ld)",
		your_alignment_gne(), align_gne, your_alignment_lnc(), p_ptr->align);
#else
	sprintf(Dummy, "Your alighnment : %s(%ld)-%s(%ld)",
		your_alignment_gne(), align_gne, your_alignment_lnc(), p_ptr->align);
#endif
	strcpy(buf, Dummy);
	info[i++] = buf;
	info[i++] = "";
	
	/* Racial powers... */
	switch (p_ptr->prace)
	{
		case RACE_HAWKMAN:
			if (get_cur_pelem() == ELEM_WIND)
			{
#ifdef JP
				info[i++] = "あなたは風の力で攻撃することができる。(8 MP)";
#else
				info[i++] = "You can attack using wind force (cost 8).";
#endif
			}
			break;
		case RACE_LIZARDMAN:
			sprintf(Dummy,
#ifdef JP
			        "あなたは %d+d%d ダメージのブレスを吐ける。(%d MP)",
#else
			        "You can breathe, dam. %d+d%d (cost %d).",
#endif
			        chp / ((get_cur_pelem() == ELEM_AQUA) ? 3 : 10), chp / 10, mhp / 10);

			info[i++] = Dummy;
			break;
		case RACE_FAIRY:
			if (plev > 11)
#ifdef JP
				info[i++] = "あなたは敵を眠らせる魔法の粉を投げることができる。(12 MP)";
#else
				info[i++] = "You can throw magical dust which induces sleep (cost 12).";
#endif

			break;
		case RACE_SKELETON:
			if (plev > 29)
#ifdef JP
				info[i++] = "あなたは失った生命力を回復することができる。(30 MP)";
#else
				info[i++] = "You can restore lost life forces (cost 30).";
#endif

			break;
		case RACE_GHOST:
			if (plev > 3)
#ifdef JP
				info[i++] = "あなたは泣き叫んで敵を恐怖させることができる。(3 MP)";
#else
				info[i++] = "You can wail to terrify your enemies (cost 3).";
#endif

			break;
		case RACE_GREMLIN:
			if (plev > 3)
#ifdef JP
				info[i++] = "あなたは泣き叫んで敵を恐怖させることができる。(3 MP)";
#else
				info[i++] = "You can wail to terrify your enemies (cost 3).";
#endif

			break;
		case RACE_PUMPKINHEAD:
			if (plev > 1)
#ifdef JP
				info[i++] = "あなたは腐った魔法のカボチャで攻撃することができる。(3 MP)";
#else
				info[i++] = "You can attack using rotten magical pumpkin (cost 3).";
#endif

			if (plev > 39)
#ifdef JP
				info[i++] = "あなたは破滅の手と同等の攻撃を繰り出すことができる。(80 MP)";
#else
				info[i++] = "You can invoke the Hand of Doom (cost 80).";
#endif

			if (plev > 44)
#ifdef JP
				info[i++] = "あなたは自爆的な攻撃をすることができる。(90 MP)";
#else
				info[i++] = "nanka (cost 90).";
#endif

			break;
		case RACE_GORGON:
			if (plev > 24)
#ifdef JP
				info[i++] = "あなたは邪眼で周囲の敵を石化させることができる。(30 MP)";
#else
				info[i++] = "You can gaze to stone your enemies (cost 30).";
#endif

			break;
		case RACE_MERMAID:
			info[i++] = "あなたは水の中で加速を得る。";
			info[i++] = "あなたは水の中で電撃に弱くなり、それ以外のダメージが軽減される。";
			info[i++] = "あなたは足元を水溜まりにすることができる。(コスト: 1)";
			if (plev > 13)
			{
				info[i++] = "あなたは魅惑の歌を歌うことができる。(コスト: 28)";
			}
			if (plev > 25)
			{
				info[i++] = "あなたは周囲を水浸しにすることができる。(コスト: 40)";
			}
			break;
		default:
			break;
	}

	if ((p_ptr->pclass != CLASS_TERRORKNIGHT)
		&& (p_ptr->pclass != CLASS_SWORDMASTER)
		&& (p_ptr->pclass != CLASS_NINJA)
		&& (p_ptr->pclass != CLASS_NINJAMASTER)
		&& (p_ptr->pclass != CLASS_VAMPIRE))
	{
#ifdef JP
		info[i++] = "あなたは小石を投げることができる。(コスト: 0)";
#else
		info[i++] = "You can throw a pebble. (cost 0)";
#endif
	}

	if (((p_ptr->pclass == CLASS_KNIGHT) || (p_ptr->pclass == CLASS_VALKYRIE) && (clev > 9))
		|| (p_ptr->pclass == CLASS_GENERAL))
		
	{
#ifdef JP
		info[i++] = "あなたはモンスターに乗って無理矢理ペットにすることができる。";
#else
		info[i++] = "You can ride on a hostile monster forcibly to turn it into pet.";
#endif
	}

	if (((p_ptr->pclass == CLASS_WIZARD)
		|| (p_ptr->pclass == CLASS_SIRENE)
		|| (p_ptr->pclass == CLASS_LICH)
		|| (p_ptr->pclass == CLASS_HIGHWITCH))
		&& (clev > 24))
	{
#ifdef JP
		info[i++] = "あなたはアイテムの魔力を吸収することができる。(コスト: 1)";
#else
		info[i++] = "You can absorb charges from an item (cost 1).";
#endif
	}

	switch(p_ptr->pclass)
	{
		case CLASS_KNIGHT:
			if (clev > 9)
			{
				info[i++] = "あなたは邪悪なる存在を感知できる。(コスト: 5)";
			}
			if (clev > 19)
			{
				info[i++] = "あなたは直線距離を突撃できる。(コスト: 20)";
			}
			if (clev > 44)
			{
				info[i++] = "あなたは武器・防具を完全に鑑定できる。(コスト: 20)";
			}
			else if (clev > 29)
			{
				info[i++] = "あなたは武器・防具を鑑定できる。(コスト: 20)";
			}
			break;
		case CLASS_BERSERKER:
			if (clev > 9)
			{
				info[i++] = "あなたは短い距離をテレポートできる。(コスト: 5)";
			}
			break;
		case CLASS_TERRORKNIGHT:
			if (clev > 4)
			{
				info[i++] = "あなたは生命のあるモンスターを感知できる。(コスト: 1)";
			}
			if (clev > 9)
			{
				info[i++] = "あなたは大きな石を投げつけることができる。(コスト: 2)";
			}
			if (clev > 24)
			{
				info[i++] = "あなたは複数の敵対する幽霊を召喚できる。(コスト: 20)";
			}
			if (clev > 29)
			{
				info[i++] = "あなたは地震を起こすことができる。(コスト: 10)";
			}
			if (clev > 39)
			{
				info[i++] = "あなたは隣接する全方向に攻撃することができる。(コスト: 20)";
			}
			if (clev > 44)
			{
				info[i++] = "あなたは空腹を満たすことができる。(コスト: 6)";
			}
			break;
		case CLASS_BEASTTAMER:
			if (clev > 9)
			{
				info[i++] = "あなたはクモを召喚できる。(コスト: 10)";
			}
			if (clev > 14)
			{
				info[i++] = "あなたは魔獣を魅了することができる。(コスト: 8)";
			}
			if (clev > 34)
			{
				info[i++] = "あなたは複数の魔獣を召喚できる。(コスト: 30)";
			}
			break;
		case CLASS_SWORDMASTER:
			info[i++] = "あなたは指弾を使うことができる。(コスト: 0)";
			info[i++] = "あなたは思考できるモンスターを感知できる。(コスト: 1)";
			if (clev > 5)
			{
				info[i++] = "あなたは武器を手元に戻るように投げることができる。(コスト: 15)";
			}
			if (clev > 11)
			{
				info[i++] = "あなたはモンスターを朦朧とさせることができる。(コスト: 10)";
			}
			if (clev > 16)
			{
				info[i++] = "あなたは攻撃した相手の背後に抜けることができる。(コスト: 20)";
			}
			if (clev > 23)
			{
				info[i++] = "あなたは素早く相手に近寄ることができる。(コスト: 30)";
			}
			if (clev > 38)
			{
				info[i++] = "あなたは強力な三段攻撃を行うことができる。(コスト: 80)";
			}
			break;
		case CLASS_DRAGOON:
#ifdef JP
			info[i++] = "あなたはドラゴンの天敵である。";
#else
			info[i++] = "You are a great bane of dragons.";
#endif
			break;
		case CLASS_NINJA:
			info[i++] = "あなたは手裏剣を投げることができる。(コスト: 0)";
			if (clev > 4)
			{
#ifdef JP
				info[i++] = "あなたは速度と引き替えに隠密を高めることができる。(コスト: 7)";
#else
				info[i++] = "You can increase stealth with speed penalty (cost 7).";
#endif
			}
			if (clev > 7)
			{
#ifdef JP
				info[i++] = "あなたは攻撃して即座に逃げることができる。(コスト: 10)";
#else
				info[i++] = "You can hit a monster and teleport at a time (cost 10).";
#endif
			}
			break;
		case CLASS_WARLOCK:
			if (clev > 29)
			{
#ifdef JP
				info[i++] = "あなたはアイテムを完全に鑑定することができる。(20 MP)";
#else
				info[i++] = "You can *identify* an item (cost 20).";
#endif
				info[i++] = "あなたは死体や岩からゴーレムを製造できる。(コスト: 30)";
			}
			break;
		case CLASS_EXORCIST:
#ifdef JP
			info[i++] = "あなたはアンデッドに対して神聖なる力を発揮する。";
#else
			info[i++] = "You strikes at undead with holy wrath.";
#endif
#ifdef JP
			info[i++] = "あなたはデーモンに対して神聖なる力を発揮する。";
#else
			info[i++] = "You strikes at demons with holy wrath.";
#endif
			/* Fall through */
		case CLASS_CLERIC:
		case CLASS_PRIEST:
#ifdef JP
			info[i++] = "あなたは神の祝福を受けている。";
#else
			info[i++] = "You have been blessed by the gods.";
#endif
			break;
		case CLASS_VALKYRIE:
			if (clev > 8)
			{
				info[i++] = "あなたは2体までの敵を串刺しにできる。(コスト: 16)";
			}
			break;
		case CLASS_ARCHER:
			if (clev > 9)
			{
				info[i++] = "あなたはモンスターを飛び越える射撃を行える。(コスト: 10)";
			}
			if (clev > 27)
			{
				info[i++] = "あなたは命中しやすい射撃を行える。(コスト: 30)";
			}
			if (clev > 34)
			{
				info[i++] = "あなたは非常に素早い射撃を行える。(コスト: 40)";
			}
			break;
		case CLASS_DRAGONTAMER:
			if (clev > 24)
			{
				info[i++] = "あなたはドラゴンを魅了することができる。(コスト: 15)";
			}
			if (clev > 29)
			{
				info[i++] = "あなたはドラゴンを召喚できる。(コスト: 25)";
			}
			if (clev > 39)
			{
				info[i++] = "あなたは古代ドラゴンを召喚できる。(コスト: 40)";
			}
			break;
		case CLASS_LICH:
			if (clev > 39)
			{
				info[i++] = "あなたは上級アンデッドを召喚できる。(コスト: 40)";
			}
			break;
		case CLASS_ANGELKNIGHT:
#ifdef JP
			info[i++] = "あなたは神の祝福を受けている。";
#else
			info[i++] = "You have been blessed by the gods.";
#endif
			if (clev > 27)
			{
				info[i++] = "あなたはアンデッドを浄化する歌を歌うことができる。(コスト: 20)";
			}
			if (clev > 43)
			{
				info[i++] = "あなたは魔法を封じる歌を歌うことができる。(コスト: 30)";
			}
			break;
		case CLASS_HIGHWITCH:
			if (clev > 29)
			{
				info[i++] = "あなたは複数のパンプキンヘッドを召喚できる。(コスト: 30)";
			}
			break;
		case CLASS_GUNNER:
			info[i++] = "あなたは体温のあるモンスターを感知できる。(コスト: 1)";
			if (clev > 9)
			{
				info[i++] = "あなたは自分の周囲の地形を感知できる。(コスト: 25)";
			}
			if (clev > 19)
			{
				info[i++] = "あなたは短い直線距離を一瞬で走れる。(コスト: 30)";
			}
			if (clev > 24)
			{
				info[i++] = "あなたは周囲のモンスターを調査できる。(コスト: 20)";
			}
			if (clev > 27)
			{
				info[i++] = "あなたは命中しやすい射撃を行える。(コスト: 30)";
			}
			if (clev > 39)
			{
				info[i++] = "あなたは壁を飛び越えられる。(コスト: 35)";
			}
			break;
		case CLASS_LORD:
			info[i++] = "あなたはモンスターを説得できる。(コスト: 5)";
			if (clev > 14) info[i++] = "あなたは複数の増援を召喚できる。(コスト: 30)";
			break;
		case CLASS_GENERAL:
			info[i++] = "あなたは直線距離を突撃できる。(コスト: 20)";
			if (clev > 14) info[i++] = "音速の衝撃波を放てる。(コスト: 20)";
			if (clev > 29) info[i++] = "あなたは複数の増援を召喚できる。(コスト: 30)";
			break;
		case CLASS_NINJAMASTER:
#ifdef JP
			info[i++] = "あなたは手裏剣を投げることができる。(コスト: 0)";
			info[i++] = "あなたは速度と引き替えに隠密を高めることができる。(コスト: 7)";
			info[i++] = "あなたは攻撃して即座に逃げることができる。(コスト: 10)";
#else
			info[i++] = "You can throw shuriken (cost: 0)";
			info[i++] = "You can increase stealth with speed penalty (cost 7).";
			info[i++] = "You can hit a monster and teleport at a time (cost 10).";
#endif
			break;
		case CLASS_FREYA:
			info[i++] = "あなたは2体までの敵を串刺しにできる。(コスト: 16)";
			if (clev > 29) info[i++] = "あなたは強力な三段攻撃を行うことができる。(コスト: 35)";
			if (clev > 39) info[i++] = "あなたは隣接する全方向に攻撃することができる。(コスト: 20)";
			break;
		case CLASS_CRESCENT:
			if (clev > 24) info[i++] = "あなたは雷属性を帯びた射撃を行える。(コスト: 30)";
			if (clev > 34) info[i++] = "あなたは*風*属性を帯びた射撃を行える。(コスト: 50)";
			break;
		case CLASS_VAMPIRE:
			if (clev > 1) info[i++] = "あなたの影を矢にして飛ばす事が出来る。(コスト: 2)";
			sprintf(Dummy,
#ifdef JP
				"あなたは敵から %d-%d HP の生命力を吸収できる。(%d MP)",
#else
				"You can steal life from a foe, dam. %d-%d (cost %d).",
#endif
					plev + MAX(1, plev / 10), plev + plev * MAX(1, plev / 10), 1 + (plev / 3));
			if (clev > 1) info[i++] = Dummy;
			if (clev > 11) info[i++] = "あなたの睨みは催眠効果をもつ。(コスト: 12)";
			if (clev > 41) info[i++] = "あなたは影の扉をくぐり抜ける事が出来る。(コスト: 30)";
			break;
		case CLASS_MEDIUM:
#ifdef JP
			info[i++] = "あなたは神の祝福を受けている。";
#else
			info[i++] = "You have been blessed by the gods.";
#endif
			if (clev > 4)
			{
#ifdef JP
				info[i++] = "あなたは水の上で体の穢れを落とすことができる。";
#else
				info[i++] = "You can drop impurity of the body on water. ";
#endif
			}
			if (clev > 19)
			{
#ifdef JP
				info[i++] = "あなたは敵を貫通する聖なる矢を放つことができる。";
#else
				info[i++] = "You can fire a holy arrow.";
#endif
			}
			break;
	}

	if (p_ptr->muta1)
	{
		if (p_ptr->muta1 & MUT1_SPIT_ACID)
		{
#ifdef JP
			info[i++] = "あなたは酸を吹きかけることができる。(ダメージ レベルX1)";
#else
			info[i++] = "You can spit acid (dam lvl).";
#endif

		}
		if (p_ptr->muta1 & MUT1_BR_FIRE)
		{
#ifdef JP
			info[i++] = "あなたは炎のブレスを吐くことができる。(ダメージ レベルX2)";
#else
			info[i++] = "You can breathe fire (dam lvl * 2).";
#endif

		}
		if (p_ptr->muta1 & MUT1_HYPN_GAZE)
		{
#ifdef JP
			info[i++] = "あなたの睨みは催眠効果をもつ。";
#else
			info[i++] = "Your gaze is hypnotic.";
#endif

		}
		if (p_ptr->muta1 & MUT1_TELEKINES)
		{
#ifdef JP
			info[i++] = "あなたは念動力をもっている。";
#else
			info[i++] = "You are telekinetic.";
#endif

		}
		if (p_ptr->muta1 & MUT1_VTELEPORT)
		{
#ifdef JP
			info[i++] = "あなたは自分の意思でテレポートできる。";
#else
			info[i++] = "You can teleport at will.";
#endif

		}
		if (p_ptr->muta1 & MUT1_MIND_BLST)
		{
#ifdef JP
			info[i++] = "あなたは精神攻撃を行える。(ダメージ 3〜12d3)";
#else
			info[i++] = "You can Mind Blast your enemies (3 to 12d3 dam).";
#endif

		}
		if (p_ptr->muta1 & MUT1_RADIATION)
		{
#ifdef JP
			info[i++] = "あなたは自分の意思で強い放射線を発生することができる。(ダメージ レベルX2)";
#else
			info[i++] = "You can emit hard radiation at will (dam lvl * 2).";
#endif

		}
		if (p_ptr->muta1 & MUT1_VAMPIRISM)
		{
#ifdef JP
			info[i++] = "あなたは吸血鬼のように敵から生命力を吸収することができる。(ダメージ レベルX2)";
#else
			info[i++] = "You can drain life from a foe like a vampire (dam lvl * 2).";
#endif

		}
		if (p_ptr->muta1 & MUT1_SMELL_MET)
		{
#ifdef JP
			info[i++] = "あなたは近くにある貴金属をかぎ分けることができる。";
#else
			info[i++] = "You can smell nearby precious metal.";
#endif

		}
		if (p_ptr->muta1 & MUT1_SMELL_MON)
		{
#ifdef JP
			info[i++] = "あなたは近くのモンスターの存在をかぎ分けることができる。";
#else
			info[i++] = "You can smell nearby monsters.";
#endif

		}
		if (p_ptr->muta1 & MUT1_BLINK)
		{
#ifdef JP
			info[i++] = "あなたは短い距離をテレポートできる。";
#else
			info[i++] = "You can teleport yourself short distances.";
#endif

		}
		if (p_ptr->muta1 & MUT1_EAT_ROCK)
		{
#ifdef JP
			info[i++] = "あなたは硬い岩を食べることができる。";
#else
			info[i++] = "You can consume solid rock.";
#endif

		}
		if (p_ptr->muta1 & MUT1_SWAP_POS)
		{
#ifdef JP
			info[i++] = "あなたは他の者と場所を入れ替わることができる。";
#else
			info[i++] = "You can switch locations with another being.";
#endif

		}
		if (p_ptr->muta1 & MUT1_SHRIEK)
		{
#ifdef JP
			info[i++] = "あなたは身の毛もよだつ叫び声を発することができる。(ダメージ レベルX2)";
#else
			info[i++] = "You can emit a horrible shriek (dam 2 * lvl).";
#endif

		}
		if (p_ptr->muta1 & MUT1_ILLUMINE)
		{
#ifdef JP
			info[i++] = "あなたは明るい光を放つことができる。";
#else
			info[i++] = "You can emit bright light.";
#endif

		}
		if (p_ptr->muta1 & MUT1_DET_CURSE)
		{
#ifdef JP
			info[i++] = "あなたは邪悪な魔法の危険を感じとることができる。";
#else
			info[i++] = "You can feel the danger of evil magic.";
#endif

		}
		if (p_ptr->muta1 & MUT1_BERSERK)
		{
#ifdef JP
			info[i++] = "あなたは自分の意思で狂乱戦闘状態になることができる。";
#else
			info[i++] = "You can drive yourself into a berserk frenzy.";
#endif

		}
		if (p_ptr->muta1 & MUT1_POLYMORPH)
		{
#ifdef JP
			info[i++] = "あなたは自分の意志で変化できる。";
#else
			info[i++] = "You can polymorph yourself at will.";
#endif

		}
		if (p_ptr->muta1 & MUT1_MIDAS_TCH)
		{
#ifdef JP
			info[i++] = "あなたは通常アイテムを金に変えることができる。";
#else
			info[i++] = "You can turn ordinary items to gold.";
#endif

		}
		if (p_ptr->muta1 & MUT1_GROW_MOLD)
		{
#ifdef JP
			info[i++] = "あなたは周囲にキノコを生やすことができる。";
#else
			info[i++] = "You can cause mold to grow near you.";
#endif

		}
		if (p_ptr->muta1 & MUT1_RESIST)
		{
#ifdef JP
			info[i++] = "あなたは元素の攻撃に対して身を硬くすることができる。";
#else
			info[i++] = "You can harden yourself to the ravages of the elements.";
#endif

		}
		if (p_ptr->muta1 & MUT1_EARTHQUAKE)
		{
#ifdef JP
			info[i++] = "あなたは周囲のダンジョンを崩壊させることができる。";
#else
			info[i++] = "You can bring down the dungeon around your ears.";
#endif

		}
		if (p_ptr->muta1 & MUT1_EAT_MAGIC)
		{
#ifdef JP
			info[i++] = "あなたは魔法のエネルギーを自分の物として使用できる。";
#else
			info[i++] = "You can consume magic energy for your own use.";
#endif

		}
		if (p_ptr->muta1 & MUT1_WEIGH_MAG)
		{
#ifdef JP
			info[i++] = "あなたは自分に影響を与える魔法の力を感じることができる。";
#else
			info[i++] = "You can feel the strength of the magics affecting you.";
#endif

		}
		if (p_ptr->muta1 & MUT1_STERILITY)
		{
#ifdef JP
			info[i++] = "あなたは集団的生殖不能を起こすことができる。";
#else
			info[i++] = "You can cause mass impotence.";
#endif

		}
		if (p_ptr->muta1 & MUT1_PANIC_HIT)
		{
#ifdef JP
			info[i++] = "あなたは攻撃した後身を守るため逃げることができる。";
#else
			info[i++] = "You can run for your life after hitting something.";
#endif

		}
		if (p_ptr->muta1 & MUT1_DAZZLE)
		{
#ifdef JP
			info[i++] = "あなたは混乱と盲目を引き起こす放射能を発生することができる。 ";
#else
			info[i++] = "You can emit confusing, blinding radiation.";
#endif

		}
		if (p_ptr->muta1 & MUT1_LASER_EYE)
		{
#ifdef JP
			info[i++] = "あなたは目からレーザー光線を発することができる。(ダメージ レベルX2)";
#else
			info[i++] = "Your eyes can fire laser beams (dam 2 * lvl).";
#endif

		}
		if (p_ptr->muta1 & MUT1_RECALL)
		{
#ifdef JP
			info[i++] = "あなたは街とダンジョンの間を行き来することができる。";
#else
			info[i++] = "You can travel between town and the depths.";
#endif

		}
		if (p_ptr->muta1 & MUT1_BANISH)
		{
#ifdef JP
			info[i++] = "あなたは邪悪なモンスターを地獄に落とすことができる。";
#else
			info[i++] = "You can send evil creatures directly to Hell.";
#endif

		}
		if (p_ptr->muta1 & MUT1_COLD_TOUCH)
		{
#ifdef JP
			info[i++] = "あなたは敵を触って凍らせることができる。(ダメージ レベルX3)";
#else
			info[i++] = "You can freeze things with a touch (dam 3 * lvl).";
#endif

		}
		if (p_ptr->muta1 & MUT1_LAUNCHER)
		{
#ifdef JP
			info[i++] = "あなたはアイテムを力強く投げることができる。";
#else
			info[i++] = "You can hurl objects with great force.";
#endif

		}
	}

	if (p_ptr->muta2)
	{
		if (p_ptr->muta2 & MUT2_BERS_RAGE)
		{
#ifdef JP
			info[i++] = "あなたは狂戦士化の発作を起こす。";
#else
			info[i++] = "You are subject to berserker fits.";
#endif

		}
		if (p_ptr->muta2 & MUT2_COWARDICE)
		{
#ifdef JP
			info[i++] = "あなたは時々臆病になる。";
#else
			info[i++] = "You are subject to cowardice.";
#endif

		}
		if (p_ptr->muta2 & MUT2_RTELEPORT)
		{
#ifdef JP
			info[i++] = "あなたはランダムにテレポートする。";
#else
			info[i++] = "You are teleporting randomly.";
#endif

		}
		if (p_ptr->muta2 & MUT2_ALCOHOL)
		{
#ifdef JP
			info[i++] = "あなたの体はアルコールを分泌する。";
#else
			info[i++] = "Your body produces alcohol.";
#endif

		}
		if (p_ptr->muta2 & MUT2_HALLU)
		{
#ifdef JP
			info[i++] = "あなたは幻覚を引き起こす精神錯乱に侵されている。";
#else
			info[i++] = "You have a hallucinatory insanity.";
#endif

		}
		if (p_ptr->muta2 & MUT2_ELEM_MULTI)
		{
#ifdef JP
			info[i++] = "あなたの固有エレメントは絶えず変化している。";
#else
			info[i++] = "Your fixed element is constantly changing.";
#endif

		}
		if (p_ptr->muta2 & MUT2_PROD_MANA)
		{
#ifdef JP
			info[i++] = "あなたは制御不能な魔法のエネルギーを発している。";
#else
			info[i++] = "You are producing magical energy uncontrollably.";
#endif

		}
		if (p_ptr->muta2 & MUT2_ATT_DEMON)
		{
#ifdef JP
			info[i++] = "あなたはデーモンを引きつける。";
#else
			info[i++] = "You attract demons.";
#endif

		}
		if (p_ptr->muta2 & MUT2_SCOR_TAIL)
		{
#ifdef JP
			info[i++] = "あなたはサソリの尻尾が生えている。(毒、ダメージ 3d7)";
#else
			info[i++] = "You have a scorpion tail (poison, 3d7).";
#endif

		}
		if (p_ptr->muta2 & MUT2_HORNS)
		{
#ifdef JP
			info[i++] = "あなたは角が生えている。(ダメージ 2d6)";
#else
			info[i++] = "You have horns (dam. 2d6).";
#endif

		}
		if (p_ptr->muta2 & MUT2_BEAK)
		{
#ifdef JP
			info[i++] = "あなたはクチバシが生えている。(ダメージ 2d4)";
#else
			info[i++] = "You have a beak (dam. 2d4).";
#endif

		}
		if (p_ptr->muta2 & MUT2_SPEED_FLUX)
		{
#ifdef JP
			info[i++] = "あなたはランダムに早く動いたり遅く動いたりする。";
#else
			info[i++] = "You move faster or slower randomly.";
#endif

		}
		if (p_ptr->muta2 & MUT2_BANISH_ALL)
		{
#ifdef JP
			info[i++] = "あなたは時々近くのモンスターを消滅させる。";
#else
			info[i++] = "You sometimes cause nearby creatures to vanish.";
#endif

		}
		if (p_ptr->muta2 & MUT2_EAT_LIGHT)
		{
#ifdef JP
			info[i++] = "あなたは時々周囲の光を吸収して栄養にする。";
#else
			info[i++] = "You sometimes feed off of the light around you.";
#endif

		}
		if (p_ptr->muta2 & MUT2_TRUNK)
		{
#ifdef JP
			info[i++] = "あなたは象のような鼻を持っている。(ダメージ 1d4)";
#else
			info[i++] = "You have an elephantine trunk (dam 1d4).";
#endif

		}
		if (p_ptr->muta2 & MUT2_ATT_ANIMAL)
		{
#ifdef JP
			info[i++] = "あなたは動物を引きつける。";
#else
			info[i++] = "You attract animals.";
#endif

		}
		if (p_ptr->muta2 & MUT2_TENTACLES)
		{
#ifdef JP
			info[i++] = "あなたは邪悪な触手を持っている。(ダメージ 2d5)";
#else
			info[i++] = "You have evil looking tentacles (dam 2d5).";
#endif

		}
		if (p_ptr->muta2 & MUT2_RAW_CHAOS)
		{
#ifdef JP
			info[i++] = "あなたはしばしば純カオスに包まれる。";
#else
			info[i++] = "You occasionally are surrounded with raw chaos.";
#endif

		}
		if (p_ptr->muta2 & MUT2_NORMALITY)
		{
#ifdef JP
			info[i++] = "あなたは変異していたが、回復してきている。";
#else
			info[i++] = "You may be mutated, but you're recovering.";
#endif

		}
		if (p_ptr->muta2 & MUT2_WRAITH)
		{
#ifdef JP
			info[i++] = "あなたの肉体は幽体化したり実体化したりする。";
#else
			info[i++] = "You fade in and out of physical reality.";
#endif

		}
		if (p_ptr->muta2 & MUT2_POLY_WOUND)
		{
#ifdef JP
			info[i++] = "あなたの健康はカオスの力に影響を受ける。";
#else
			info[i++] = "Your health is subject to chaotic forces.";
#endif

		}
		if (p_ptr->muta2 & MUT2_WASTING)
		{
#ifdef JP
			info[i++] = "あなたは衰弱する恐ろしい病気にかかっている。";
#else
			info[i++] = "You have a horrible wasting disease.";
#endif

		}
		if (p_ptr->muta2 & MUT2_ATT_DRAGON)
		{
#ifdef JP
			info[i++] = "あなたはドラゴンを引きつける。";
#else
			info[i++] = "You attract dragons.";
#endif

		}
		if (p_ptr->muta2 & MUT2_WEIRD_MIND)
		{
#ifdef JP
			info[i++] = "あなたの精神はランダムに拡大したり縮小したりしている。";
#else
			info[i++] = "Your mind randomly expands and contracts.";
#endif

		}
		if (p_ptr->muta2 & MUT2_NAUSEA)
		{
#ifdef JP
			info[i++] = "あなたの胃は非常に落ち着きがない。";
#else
			info[i++] = "You have a seriously upset stomach.";
#endif

		}
		if (p_ptr->muta2 & MUT2_TAROT)
		{
#ifdef JP
			info[i++] = "あなたはタロットカードの力を感じる。";
#else
			info[i++] = "You feel the power of tarot cards.";
#endif

		}
		if (p_ptr->muta2 & MUT2_ALTER_REALITY)
		{
#ifdef JP
			info[i++] = "あなたの周りの現実は歪んでいる。";
#else
			info[i++] = "Reality around you is distorted.";
#endif

		}
		if (p_ptr->muta2 & MUT2_WARNING)
		{
#ifdef JP
			info[i++] = "あなたは敵に関する警告を感じる。";
#else
			info[i++] = "You receive warnings about your foes.";
#endif

		}
		if (p_ptr->muta2 & MUT2_INVULN)
		{
#ifdef JP
			info[i++] = "あなたは時々負け知らずな気分になる。";
#else
			info[i++] = "You occasionally feel invincible.";
#endif

		}
		if (p_ptr->muta2 & MUT2_SP_TO_HP)
		{
#ifdef JP
			info[i++] = "あなたは時々血が筋肉にどっと流れる。";
#else
			info[i++] = "Your blood sometimes rushes to your muscles.";
#endif

		}
		if (p_ptr->muta2 & MUT2_HP_TO_SP)
		{
#ifdef JP
			info[i++] = "あなたは時々頭に血がどっと流れる。";
#else
			info[i++] = "Your blood sometimes rushes to your head.";
#endif

		}
		if (p_ptr->muta2 & MUT2_DISARM)
		{
#ifdef JP
			info[i++] = "あなたはよくつまづいて物を落とす。";
#else
			info[i++] = "You occasionally stumble and drop things.";
#endif

		}
	}

	if (p_ptr->muta3)
	{
		if (p_ptr->muta3 & MUT3_HYPER_STR)
		{
#ifdef JP
			info[i++] = "あなたは超人的に強い。(腕力+4)";
#else
			info[i++] = "You are superhumanly strong (+4 STR).";
#endif

		}
		if (p_ptr->muta3 & MUT3_PUNY)
		{
#ifdef JP
			info[i++] = "あなたは虚弱だ。(腕力-4)";
#else
			info[i++] = "You are puny (-4 STR).";
#endif

		}
		if (p_ptr->muta3 & MUT3_HYPER_INT)
		{
#ifdef JP
			info[i++] = "あなたの脳は生体コンピュータだ。(知能＆賢さ+4)";
#else
			info[i++] = "Your brain is a living computer (+4 INT/WIS).";
#endif

		}
		if (p_ptr->muta3 & MUT3_MORONIC)
		{
#ifdef JP
			info[i++] = "あなたは精神薄弱だ。(知能＆賢さ-4)";
#else
			info[i++] = "You are moronic (-4 INT/WIS).";
#endif

		}
		if (p_ptr->muta3 & MUT3_RESILIENT)
		{
#ifdef JP
			info[i++] = "あなたは非常にタフだ。(耐久+4)";
#else
			info[i++] = "You are very resilient (+4 CON).";
#endif

		}
		if (p_ptr->muta3 & MUT3_XTRA_FAT)
		{
#ifdef JP
			info[i++] = "あなたは極端に太っている。(耐久+2,スピード-2)";
#else
			info[i++] = "You are extremely fat (+2 CON, -2 speed).";
#endif

		}
		if (p_ptr->muta3 & MUT3_ALBINO)
		{
#ifdef JP
			info[i++] = "あなたはアルビノだ。(耐久-4)";
#else
			info[i++] = "You are albino (-4 CON).";
#endif

		}
		if (p_ptr->muta3 & MUT3_FLESH_ROT)
		{
#ifdef JP
			info[i++] = "あなたの肉体は腐敗している。(耐久-2,魅力-1)";
#else
			info[i++] = "Your flesh is rotting (-2 CON, -1 CHR).";
#endif

		}
		if (p_ptr->muta3 & MUT3_SILLY_VOI)
		{
#ifdef JP
			info[i++] = "あなたの声は間抜けなキーキー声だ。(魅力-4)";
#else
			info[i++] = "Your voice is a silly squeak (-4 CHR).";
#endif

		}
		if (p_ptr->muta3 & MUT3_BLANK_FAC)
		{
#ifdef JP
			info[i++] = "あなたはのっぺらぼうだ。(魅力-1)";
#else
			info[i++] = "Your face is featureless (-1 CHR).";
#endif

		}
		if (p_ptr->muta3 & MUT3_ILL_NORM)
		{
#ifdef JP
			info[i++] = "あなたは幻影に覆われている。";
#else
			info[i++] = "Your appearance is masked with illusion.";
#endif

		}
		if (p_ptr->muta3 & MUT3_XTRA_EYES)
		{
#ifdef JP
			info[i++] = "あなたは余分に二つの目を持っている。(探索+15)";
#else
			info[i++] = "You have an extra pair of eyes (+15 search).";
#endif

		}
		if (p_ptr->muta3 & MUT3_MAGIC_RES)
		{
#ifdef JP
			info[i++] = "あなたは魔法への耐性をもっている。";
#else
			info[i++] = "You are resistant to magic.";
#endif

		}
		if (p_ptr->muta3 & MUT3_XTRA_NOIS)
		{
#ifdef JP
			info[i++] = "あなたは変な音を発している。(隠密-3)";
#else
			info[i++] = "You make a lot of strange noise (-3 stealth).";
#endif

		}
		if (p_ptr->muta3 & MUT3_INFRAVIS)
		{
#ifdef JP
			info[i++] = "あなたは素晴らしい赤外線視力を持っている。(+3)";
#else
			info[i++] = "You have remarkable infravision (+3).";
#endif

		}
		if (p_ptr->muta3 & MUT3_XTRA_LEGS)
		{
#ifdef JP
			info[i++] = "あなたは余分に二本の足が生えている。(加速+3)";
#else
			info[i++] = "You have an extra pair of legs (+3 speed).";
#endif

		}
		if (p_ptr->muta3 & MUT3_SHORT_LEG)
		{
#ifdef JP
			info[i++] = "あなたの足は短い突起だ。(加速-3)";
#else
			info[i++] = "Your legs are short stubs (-3 speed).";
#endif

		}
		if (p_ptr->muta3 & MUT3_ELEC_TOUC)
		{
#ifdef JP
			info[i++] = "あなたの血管には電流が流れている。";
#else
			info[i++] = "Electricity is running through your veins.";
#endif

		}
		if (p_ptr->muta3 & MUT3_FIRE_BODY)
		{
#ifdef JP
			info[i++] = "あなたの体は炎につつまれている。";
#else
			info[i++] = "Your body is enveloped in flames.";
#endif
		}
		if (p_ptr->muta3 & MUT3_WART_SKIN)
		{
#ifdef JP
			info[i++] = "あなたの肌はイボに被われている。(魅力-2, AC+5)";
#else
			info[i++] = "Your skin is covered with warts (-2 CHR, +5 AC).";
#endif

		}
		if (p_ptr->muta3 & MUT3_SCALES)
		{
#ifdef JP
			info[i++] = "あなたの肌は鱗になっている。(魅力-1, AC+10)";
#else
			info[i++] = "Your skin has turned into scales (-1 CHR, +10 AC).";
#endif

		}
		if (p_ptr->muta3 & MUT3_IRON_SKIN)
		{
#ifdef JP
			info[i++] = "あなたの肌は鉄でできている。(器用-1, AC+25)";
#else
			info[i++] = "Your skin is made of steel (-1 DEX, +25 AC).";
#endif

		}
		if (p_ptr->muta3 & MUT3_WINGS)
		{
#ifdef JP
			info[i++] = "あなたは羽を持っている。";
#else
			info[i++] = "You have wings.";
#endif

		}
		if (p_ptr->muta3 & MUT3_FEARLESS)
		{
			/* Unnecessary */
		}
		if (p_ptr->muta3 & MUT3_REGEN)
		{
			/* Unnecessary */
		}
		if (p_ptr->muta3 & MUT3_ESP)
		{
			/* Unnecessary */
		}
		if (p_ptr->muta3 & MUT3_LIMBER)
		{
#ifdef JP
			info[i++] = "あなたの体は非常にしなやかだ。(器用+3)";
#else
			info[i++] = "Your body is very limber (+3 DEX).";
#endif

		}
		if (p_ptr->muta3 & MUT3_ARTHRITIS)
		{
#ifdef JP
			info[i++] = "あなたはいつも関節に痛みを感じている。(器用-3)";
#else
			info[i++] = "Your joints ache constantly (-3 DEX).";
#endif

		}
		if (p_ptr->muta3 & MUT3_VULN_ELEM)
		{
#ifdef JP
			info[i++] = "あなたは元素の攻撃に弱い。";
#else
			info[i++] = "You are susceptible to damage from the elements.";
#endif

		}
		if (p_ptr->muta3 & MUT3_MOTION)
		{
#ifdef JP
			info[i++] = "あなたの動作は正確で力強い。(隠密+1)";
#else
			info[i++] = "Your movements are precise and forceful (+1 STL).";
#endif

		}
		if (p_ptr->muta3 & MUT3_GOOD_LUCK)
		{
#ifdef JP
			info[i++] = "あなたは白いオーラにつつまれている。";
#else
			info[i++] = "There is a white aura surrounding you.";
#endif
		}
		if (p_ptr->muta3 & MUT3_BAD_LUCK)
		{
#ifdef JP
			info[i++] = "あなたは黒いオーラにつつまれている。";
#else
			info[i++] = "There is a black aura surrounding you.";
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
	if (p_ptr->cursed & TRC_TY_CURSE)
	{
#ifdef JP
		info[i++] = "あなたは邪悪な怨念に包まれている。";
#else
		info[i++] = "You carry an ancient foul curse.";
#endif

	}
	if (p_ptr->cursed & TRC_AGGRAVATE)
	{
#ifdef JP
		info[i++] = "あなたはモンスターを怒らせている。";
#else
		info[i++] = "You aggravate monsters.";
#endif

	}
	if (p_ptr->cursed & TRC_DRAIN_EXP)
	{
#ifdef JP
		info[i++] = "あなたは経験値を吸われている。";
#else
		info[i++] = "You are drained.";
#endif

	}
	if (p_ptr->cursed & TRC_SLOW_REGEN)
	{
#ifdef JP
		info[i++] = "あなたの回復力は非常に遅い。";
#else
		info[i++] = "You regenerate slowly.";
#endif

	}
	if (p_ptr->cursed & TRC_ADD_L_CURSE)
	{
#ifdef JP
		info[i++] = "あなたの弱い呪いは増える。"; /* 暫定的 -- henkma */
#else
		info[i++] = "Your weak curses multiply.";
#endif

	}
	if (p_ptr->cursed & TRC_ADD_H_CURSE)
	{
#ifdef JP
		info[i++] = "あなたの強い呪いは増える。"; /* 暫定的 -- henkma */
#else
		info[i++] = "Your heavy curses multiply.";
#endif

	}
	if (p_ptr->cursed & TRC_CALL_ANIMAL)
	{
#ifdef JP
		info[i++] = "あなたは動物に狙われている。";
#else
		info[i++] = "You attract animals.";
#endif

	}
	if (p_ptr->cursed & TRC_CALL_DEMON)
	{
#ifdef JP
		info[i++] = "あなたは悪魔に狙われている。";
#else
		info[i++] = "You attract demons.";
#endif

	}
	if (p_ptr->cursed & TRC_CALL_DRAGON)
	{
#ifdef JP
		info[i++] = "あなたはドラゴンに狙われている。";
#else
		info[i++] = "You attract dragons.";
#endif

	}
	if (p_ptr->cursed & TRC_COWARDICE)
	{
#ifdef JP
		info[i++] = "あなたは時々臆病になる。";
#else
		info[i++] = "You are subject to cowardice.";
#endif

	}
	if (p_ptr->cursed & TRC_TELEPORT)
	{
#ifdef JP
		info[i++] = "あなたの位置はひじょうに不安定だ。";
#else
		info[i++] = "Your position is very uncertain.";
#endif

	}
	if (p_ptr->cursed & TRC_LOW_MELEE)
	{
#ifdef JP
		info[i++] = "あなたの武器は攻撃を外しやすい。";
#else
		info[i++] = "Your weapon causes you to miss blows.";
#endif

	}
	if (p_ptr->cursed & TRC_LOW_AC)
	{
#ifdef JP
		info[i++] = "あなたは攻撃を受けやすい。";
#else
		info[i++] = "You are subject to be hit.";
#endif

	}
	if (p_ptr->cursed & TRC_LOW_MAGIC)
	{
#ifdef JP
		info[i++] = "あなたは魔法を失敗しやすい。";
#else
		info[i++] = "You are subject to fail spellcasting.";
#endif

	}
	if (p_ptr->cursed & TRC_FAST_DIGEST)
	{
#ifdef JP
		info[i++] = "あなたはすぐお腹がへる。";
#else
		info[i++] = "You have a good appetite.";
#endif

	}
	if (p_ptr->cursed & TRC_DRAIN_HP)
	{
#ifdef JP
		info[i++] = "あなたは体力を吸われている。";
#else
		info[i++] = "You are drained.";
#endif

	}
	if (p_ptr->cursed & TRC_DRAIN_MANA)
	{
#ifdef JP
		info[i++] = "あなたは魔力を吸われている。";
#else
		info[i++] = "You brain is drained.";
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
	if (p_ptr->wraith_form_perm)
	{
#ifdef JP
		info[i++] = "あなたは永続的に幽体化している。";
#else
		info[i++] = "You are permanently incorporeal.";
#endif

	}
	else if (p_ptr->wraith_form)
	{
#ifdef JP
		info[i++] = "あなたは一時的に幽体化している。";
#else
		info[i++] = "You are temporarily incorporeal.";
#endif

	}
	if (p_ptr->special_attack & ATTACK_CONFUSE)
	{
#ifdef JP
		info[i++] = "あなたの手は赤く輝いている。";
#else
		info[i++] = "Your hands are glowing dull red.";
#endif

	}
	switch (p_ptr->action)
	{
		case ACTION_SEARCH:
#ifdef JP
			info[i++] = "あなたはひじょうに注意深く周囲を見渡している。";
#else
			info[i++] = "You are looking around very carefully.";
#endif
			break;
	}
	if (p_ptr->word_recall)
	{
#ifdef JP
		info[i++] = "あなたはすぐに帰還するだろう。";
#else
		info[i++] = "You will soon be recalled.";
#endif

	}
	if (p_ptr->alter_reality)
	{
#ifdef JP
		info[i++] = "あなたはすぐにこの世界を離れるだろう。";
#else
		info[i++] = "You will soon be altered.";
#endif

	}
	if (p_ptr->inhibit_flood)
	{
#ifdef JP
		info[i++] = "あなたは現在はまだ大洪水を使えない。";
#else
		info[i++] = "You cannot use great flood now.";
#endif

	}
	if (p_ptr->tim_resurrection)
	{
#ifdef JP
		info[i++] = "あなたは死んでも1度だけ復活できる。";
#else
		info[i++] = "You can revive only once.";
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
	if (p_ptr->regenerate_mana)
	{
#ifdef JP
		info[i++] = "あなたは素早く魔力を回復する。";
#else
		info[i++] = "You regenerate mana quickly.";
#endif

	}

	if (p_ptr->no_digest)
	{
#ifdef JP
		info[i++] = "あなたは食料を必要としない。";
#else
		info[i++] = "You don't need any foods.";
#endif

	}
	else if (p_ptr->slow_digest)
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
	if (p_ptr->esp_dragon)
	{
#ifdef JP
		info[i++] = "あなたはドラゴンの存在を感じる能力を持っている。";
#else
		info[i++] = "You sense dragons.";
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
	if (p_ptr->wind_guard)
	{
		if (p_ptr->stat_use[A_INT] >= (18 + 150))
		{
#ifdef JP
		info[i++] = "あなたは矢やボルトを回避する。";
#else
		info[i++] = "You avoid arrows and bolts.";
#endif
		}
		else
		{
#ifdef JP
		info[i++] = "あなたは物理的な矢やボルトを回避する。";
#else
		info[i++] = "You avoid physical arrows and bolts.";
#endif
		}

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
		info[i++] = "You are surrounded with an aura of coldness.";
#endif

	}
	if (p_ptr->tim_sh_holy)
	{
#ifdef JP
		info[i++] = "あなたは聖なるオーラに包まれている。";
#else
		info[i++] = "You are surrounded with a holy aura.";
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
	if (p_ptr->anti_magic_field > 0)
	{
#ifdef JP
		sprintf(amfld_buf, "あなたは反魔法フィールドを張っている。(半径: %d)", p_ptr->anti_magic_field);
#else
		sprintf(amfld_buf, "You are surrounded by an anti-magic field (radius %d).", p_ptr->anti_magic_field);
#endif
		info[i++] = amfld_buf;

	}
	if (p_ptr->anti_tele)
	{
#ifdef JP
		info[i++] = "あなたはテレポートできない。";
#else
		info[i++] = "You cannot teleport.";
#endif

	}
	if (p_ptr->fear_field)
	{
#ifdef JP
		info[i++] = "あなたは恐怖フィールドを張っている。";
#else
		info[i++] = "You are surrounded by a fear field.";
#endif

	}
	if (p_ptr->earth_spike)
	{
#ifdef JP
		info[i++] = "あなたは自分の意志でのみテレポートできる。";
#else
		info[i++] = "You can teleport only by yourself.";
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
		info[i++] = "You will be warned before dangerous action.";
#endif

	}
	if (p_ptr->dec_mana)
	{
#ifdef JP
		info[i++] = "あなたは少ない消費魔力で魔法を唱えることができる。";
#else
		info[i++] = "You can cast spells with fewer mana points.";
#endif

	}
	if (p_ptr->easy_spell)
	{
#ifdef JP
		info[i++] = "あなたは低い失敗率で魔法を唱えることができる。";
#else
		info[i++] = "Fail rate of your magic is decreased.";
#endif

	}
	if (p_ptr->heavy_spell)
	{
#ifdef JP
		info[i++] = "あなたは高い失敗率で魔法を唱えなければいけない。";
#else
		info[i++] = "Fail rate of your magic is increased.";
#endif

	}
	if (p_ptr->mighty_throw)
	{
#ifdef JP
		info[i++] = "あなたは強く物を投げる。";
#else
		info[i++] = "You can throw objects powerfully.";
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
		info[i++] = "あなたは火への強力な耐性を持っている。";
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

	if (p_ptr->zoshonel_protect && !p_ptr->immune_cold)
	{
#ifdef JP
		info[i++] = "あなたは冷気に弱い。";
#else
		info[i++] = "You are susceptible to damage from cold.";
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

	if (p_ptr->ogre_equip || p_ptr->hurt_lite)
	{
#ifdef JP
		info[i++] = "あなたは閃光に弱い。";
#else
		info[i++] = "You are susceptible to damage from bright light.";
#endif

	}

	if (WRAITH_FORM() || p_ptr->evil_equip || (p_ptr->pclass == CLASS_VAMPIRE))
	{
#ifdef JP
		info[i++] = "あなたは暗黒に対する完全なる免疫を持っている。";
#else
		info[i++] = "You are completely immune to darkness.";
#endif
	}
	else
	{
		if (p_ptr->resist_dark)
		{
#ifdef JP
			info[i++] = "あなたは暗黒への耐性を持っている。";
#else
			info[i++] = "You are resistant to darkness.";
#endif

		}

		if (prace_is_(RACE_FAIRY))
		{
#ifdef JP
			info[i++] = "あなたは暗黒に弱い。";
#else
			info[i++] = "You are susceptible to damage from darkness.";
#endif

		}
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
	if (p_ptr->resist_stone)
	{
#ifdef JP
		info[i++] = "あなたは石化への耐性を持っている。";
#else
		info[i++] = "You are resistant to stone.";
#endif

	}

	if (prace_is_(RACE_GHOST))
	{
#ifdef JP
		info[i++] = "あなたは地獄の力を吸収できる。";
#else
		info[i++] = "You can drain nether forces.";
#endif

	}
	else if (p_ptr->evil_equip)
	{
#ifdef JP
		info[i++] = "あなたは地獄の力に対する完全なる免疫を持っている。";
#else
		info[i++] = "You are completely immune to nether forces.";
#endif
	}
	else if (p_ptr->resist_neth)
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
	if (p_ptr->resist_time)
	{
#ifdef JP
		info[i++] = "あなたは時間逆転への耐性を持っている。";
#else
		info[i++] = "You are resistant to time.";
#endif

	}
	if (p_ptr->resist_water)
	{
#ifdef JP
		info[i++] = "あなたは水への耐性を持っている。";
#else
		info[i++] = "You are resistant to water.";
#endif

	}
	if (p_ptr->zoshonel_protect)
	{
#ifdef JP
		info[i++] = "あなたは水に弱い。";
#else
		info[i++] = "You are susceptible to damage from water.";
#endif
#ifdef JP
		info[i++] = "あなたは*水*に弱い。";
#else
		info[i++] = "You are susceptible to damage from *aqua*.";
#endif
#ifdef JP
		info[i++] = "あなたはプラズマに対する完全なる免疫を持っている。";
#else
		info[i++] = "You are completely immune to plasma.";
#endif

	}
	if (p_ptr->ogre_equip)
	{
#ifdef JP
		info[i++] = "あなたは聖なる力に弱い。";
#else
		info[i++] = "You are susceptible to damage from holy force.";
#endif

	}

	if (p_ptr->smell_equip)
	{
#ifdef JP
		info[i++] = "あなたは周りのモンスターを悪臭で起こしている。";
#else
		info[i++] = "Your smell wakes monsters up.";
#endif

	}

	if (p_ptr->infected)
	{
#ifdef JP
		info[i++] = "あなたは感染している。";
#else
		info[i++] = "You are infected.";
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

	if (have_flag(flgs, TR_STR))
	{
#ifdef JP
		info[i++] = "あなたの腕力は装備によって影響を受けている。";
#else
		info[i++] = "Your strength is affected by your equipment.";
#endif

	}
	if (have_flag(flgs, TR_INT))
	{
#ifdef JP
		info[i++] = "あなたの知能は装備によって影響を受けている。";
#else
		info[i++] = "Your intelligence is affected by your equipment.";
#endif

	}
	if (have_flag(flgs, TR_WIS))
	{
#ifdef JP
		info[i++] = "あなたの賢さは装備によって影響を受けている。";
#else
		info[i++] = "Your wisdom is affected by your equipment.";
#endif

	}
	if (have_flag(flgs, TR_DEX))
	{
#ifdef JP
		info[i++] = "あなたの器用さは装備によって影響を受けている。";
#else
		info[i++] = "Your dexterity is affected by your equipment.";
#endif

	}
	if (have_flag(flgs, TR_CON))
	{
#ifdef JP
		info[i++] = "あなたの耐久力は装備によって影響を受けている。";
#else
		info[i++] = "Your constitution is affected by your equipment.";
#endif

	}
	if (have_flag(flgs, TR_CHR))
	{
#ifdef JP
		info[i++] = "あなたの魅力は装備によって影響を受けている。";
#else
		info[i++] = "Your charisma is affected by your equipment.";
#endif

	}

	if (have_flag(flgs, TR_STEALTH))
	{
#ifdef JP
		info[i++] = "あなたの隠密行動能力は装備によって影響を受けている。";
#else
		info[i++] = "Your stealth is affected by your equipment.";
#endif

	}
	if (have_flag(flgs, TR_SEARCH))
	{
#ifdef JP
		info[i++] = "あなたの探索能力は装備によって影響を受けている。";
#else
		info[i++] = "Your searching ability is affected by your equipment.";
#endif

	}
	if (have_flag(flgs, TR_INFRA))
	{
#ifdef JP
		info[i++] = "あなたの赤外線視力は装備によって影響を受けている。";
#else
		info[i++] = "Your infravision is affected by your equipment.";
#endif

	}
	if (have_flag(flgs, TR_TUNNEL))
	{
#ifdef JP
		info[i++] = "あなたの採掘能力は装備によって影響を受けている。";
#else
		info[i++] = "Your digging ability is affected by your equipment.";
#endif

	}
	if (have_flag(flgs, TR_SPEED))
	{
#ifdef JP
		info[i++] = "あなたのスピードは装備によって影響を受けている。";
#else
		info[i++] = "Your speed is affected by your equipment.";
#endif

	}
	if (have_flag(flgs, TR_BLOWS))
	{
#ifdef JP
		info[i++] = "あなたの攻撃速度は装備によって影響を受けている。";
#else
		info[i++] = "Your attack speed is affected by your equipment.";
#endif

	}


	/* Access the current weapon */
	o_ptr = &inventory[INVEN_RARM];

	/* Analyze the weapon */
	if (o_ptr->k_idx)
	{
		/* Indicate Blessing */
		if (have_flag(flgs, TR_BLESSED))
		{
#ifdef JP
			info[i++] = "あなたの武器は神の祝福を受けている。";
#else
			info[i++] = "Your weapon has been blessed by the gods.";
#endif

		}

		if (have_flag(flgs, TR_UNHOLY))
		{
#ifdef JP
			info[i++] = "あなたの武器は邪悪で穢れている。";
#else
			info[i++] = "Your weapon has been poluted by evil.";
#endif

		}

		if (have_flag(flgs, TR_CHAOTIC))
		{
#ifdef JP
			info[i++] = "あなたの武器はカオスの属性をもつ。";
#else
			info[i++] = "Your weapon is branded with Chaos.";
#endif

		}

		/* Hack */
		if (have_flag(flgs, TR_IMPACT))
		{
#ifdef JP
			info[i++] = "あなたの武器は打撃で地震を発生することができる。";
#else
			info[i++] = "The impact of your weapon can cause earthquakes.";
#endif

		}

		if (o_ptr->name2 == EGO_EARTHQUAKES)
		{
#ifdef JP
			info[i++] = "あなたの武器は敵を朦朧とさせる。";
#else
			info[i++] = "Your weapon stuns your foes.";
#endif

		}

		if (have_flag(flgs, TR_EXTRA_VORPAL))
		{
#ifdef JP
			info[i++] = "あなたの武器は伝説的に鋭い。";
#else
			info[i++] = "Your weapon is legendary sharp.";
#endif

		}
		else if (have_flag(flgs, TR_VORPAL))
		{
#ifdef JP
			info[i++] = "あなたの武器は非常に鋭い。";
#else
			info[i++] = "Your weapon is very sharp.";
#endif

		}

		if (have_flag(flgs, TR_VAMPIRIC))
		{
#ifdef JP
			info[i++] = "あなたの武器は敵から生命力を吸収する。";
#else
			info[i++] = "Your weapon drains life from your foes.";
#endif

		}

		/* Special "Attack Bonuses" */
		if ((have_flag(flgs, TR_BRAND_ACID)) || (p_ptr->special_attack & ATTACK_ACID))
		{
#ifdef JP
			info[i++] = "あなたの武器は敵を溶かす。";
#else
			info[i++] = "Your weapon melts your foes.";
#endif

		}
		if ((have_flag(flgs, TR_BRAND_ELEC)) || (p_ptr->special_attack & ATTACK_ELEC))
		{
#ifdef JP
			info[i++] = "あなたの武器は敵を感電させる。";
#else
			info[i++] = "Your weapon shocks your foes.";
#endif

		}
		if ((have_flag(flgs, TR_BRAND_FIRE)) || (p_ptr->special_attack & ATTACK_FIRE))
		{
#ifdef JP
			info[i++] = "あなたの武器は敵を燃やす。";
#else
			info[i++] = "Your weapon burns your foes.";
#endif

		}
		if ((have_flag(flgs, TR_BRAND_COLD)) || (p_ptr->special_attack & ATTACK_COLD))
		{
#ifdef JP
			info[i++] = "あなたの武器は敵を凍らせる。";
#else
			info[i++] = "Your weapon freezes your foes.";
#endif

		}
		if ((have_flag(flgs, TR_BRAND_POIS)) || (p_ptr->special_attack & ATTACK_POIS))
		{
#ifdef JP
			info[i++] = "あなたの武器は敵を毒で侵す。";
#else
			info[i++] = "Your weapon poisons your foes.";
#endif

		}

		/* Special "slay" flags */
		if (have_flag(flgs, TR_KILL_ANIMAL))
		{
#ifdef JP
			info[i++] = "あなたの武器は動物の天敵である。";
#else
			info[i++] = "Your weapon is a great bane of animals.";
#endif

		}
		else if (have_flag(flgs, TR_SLAY_ANIMAL))
		{
#ifdef JP
			info[i++] = "あなたの武器は動物に対して強い力を発揮する。";
#else
			info[i++] = "Your weapon strikes at animals with extra force.";
#endif

		}

		if (have_flag(flgs, TR_KILL_EVIL))
		{
#ifdef JP
			info[i++] = "あなたの武器は邪悪なる存在の天敵である。";
#else
			info[i++] = "Your weapon is a great bane of evil.";
#endif

		}
		else if (have_flag(flgs, TR_SLAY_EVIL))
		{
#ifdef JP
			info[i++] = "あなたの武器は邪悪なる存在に対して強い力を発揮する。";
#else
			info[i++] = "Your weapon strikes at evil with extra force.";
#endif

		}

		if (have_flag(flgs, TR_KILL_GOOD))
		{
#ifdef JP
			info[i++] = "あなたの武器は善良なる存在の天敵である。";
#else
			info[i++] = "Your weapon is a great bane of good.";
#endif

		}
		else if ((have_flag(flgs, TR_SLAY_GOOD)) || (p_ptr->special_attack & ATTACK_EVIL))
		{
#ifdef JP
			info[i++] = "あなたの武器は善良な存在に対して強い力を発揮する。";
#else
			info[i++] = "Your weapon strikes at good with extra force.";
#endif

		}

		if (have_flag(flgs, TR_KILL_LIVING))
		{
#ifdef JP
			info[i++] = "あなたの武器は生命のある存在の天敵である。";
#else
			info[i++] = "Your weapon is a great bane of livings.";
#endif

		}
		else if ((have_flag(flgs, TR_SLAY_LIVING)) || (p_ptr->special_attack & ATTACK_EVIL))
		{
#ifdef JP
			info[i++] = "あなたの武器は生命のある存在に対して強い力を発揮する。";
#else
			info[i++] = "Your weapon strikes at livings with extra force.";
#endif

		}

		if (have_flag(flgs, TR_KILL_HUMAN))
		{
#ifdef JP
			info[i++] = "あなたの武器は人間の天敵である。";
#else
			info[i++] = "Your weapon is a great bane of humans.";
#endif

		}
		else if (have_flag(flgs, TR_SLAY_HUMAN))
		{
#ifdef JP
			info[i++] = "あなたの武器は人間に対して特に強い力を発揮する。";
#else
			info[i++] = "Your weapon is especially deadly against humans.";
#endif

		}

		if (have_flag(flgs, TR_KILL_UNDEAD))
		{
#ifdef JP
			info[i++] = "あなたの武器はアンデッドの天敵である。";
#else
			info[i++] = "Your weapon is a great bane of undead.";
#endif

		}
		else if (have_flag(flgs, TR_SLAY_UNDEAD))
		{
#ifdef JP
			info[i++] = "あなたの武器はアンデッドに対して神聖なる力を発揮する。";
#else
			info[i++] = "Your weapon strikes at undead with holy wrath.";
#endif

		}

		if (have_flag(flgs, TR_KILL_DEMON))
		{
#ifdef JP
			info[i++] = "あなたの武器はデーモンの天敵である。";
#else
			info[i++] = "Your weapon is a great bane of demons.";
#endif

		}
		else if (have_flag(flgs, TR_SLAY_DEMON))
		{
#ifdef JP
			info[i++] = "あなたの武器はデーモンに対して神聖なる力を発揮する。";
#else
			info[i++] = "Your weapon strikes at demons with holy wrath.";
#endif

		}

		if (have_flag(flgs, TR_KILL_ORC))
		{
#ifdef JP
			info[i++] = "あなたの武器はオークの天敵である。";
#else
			info[i++] = "Your weapon is a great bane of orcs.";
#endif

		}
		else if (have_flag(flgs, TR_SLAY_ORC))
		{
#ifdef JP
			info[i++] = "あなたの武器はオークに対して特に強い力を発揮する。";
#else
			info[i++] = "Your weapon is especially deadly against orcs.";
#endif

		}

		if (have_flag(flgs, TR_KILL_TROLL))
		{
#ifdef JP
			info[i++] = "あなたの武器はトロルの天敵である。";
#else
			info[i++] = "Your weapon is a great bane of trolls.";
#endif

		}
		else if (have_flag(flgs, TR_SLAY_TROLL))
		{
#ifdef JP
			info[i++] = "あなたの武器はトロルに対して特に強い力を発揮する。";
#else
			info[i++] = "Your weapon is especially deadly against trolls.";
#endif

		}

		if (have_flag(flgs, TR_KILL_GIANT))
		{
#ifdef JP
			info[i++] = "あなたの武器はジャイアントの天敵である。";
#else
			info[i++] = "Your weapon is a great bane of giants.";
#endif

		}
		else if (have_flag(flgs, TR_SLAY_GIANT))
		{
#ifdef JP
			info[i++] = "あなたの武器はジャイアントに対して特に強い力を発揮する。";
#else
			info[i++] = "Your weapon is especially deadly against giants.";
#endif

		}

		if (have_flag(flgs, TR_KILL_DRAGON))
		{
#ifdef JP
			info[i++] = "あなたの武器はドラゴンの天敵である。";
#else
			info[i++] = "Your weapon is a great bane of dragons.";
#endif

		}
		else if (have_flag(flgs, TR_SLAY_DRAGON))
		{
#ifdef JP
			info[i++] = "あなたの武器はドラゴンに対して特に強い力を発揮する。";
#else
			info[i++] = "Your weapon is especially deadly against dragons.";
#endif

		}

		if (have_flag(flgs, TR_FORCE_WEAPON))
		{
#ifdef JP
			info[i++] = "あなたの武器はMPを使って攻撃する。";
#else
			info[i++] = "Your weapon causes greate damages using your MP.";
#endif

		}
		if (have_flag(flgs, TR_THROW))
		{
#ifdef JP
			info[i++] = "あなたの武器は投げやすい。";
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


static int report_magics_aux(int dur)
{
	if (dur <= 5)
	{
		return 0;
	}
	else if (dur <= 10)
	{
		return 1;
	}
	else if (dur <= 20)
	{
		return 2;
	}
	else if (dur <= 50)
	{
		return 3;
	}
	else if (dur <= 100)
	{
		return 4;
	}
	else if (dur <= 200)
	{
		return 5;
	}
	else
	{
		return 6;
	}
}

static cptr report_magic_durations[] =
{
#ifdef JP
	"ごく短い間",
	"少しの間",
	"しばらくの間",
	"多少長い間",
	"長い間",
	"非常に長い間",
	"信じ難いほど長い間",
	"モンスターを攻撃するまで"
#else
	"for a short time",
	"for a little while",
	"for a while",
	"for a long while",
	"for a long time",
	"for a very long time",
	"for an incredibly long time",
	"until you hit a monster"
#endif

};


/*
 * Report all currently active magical effects.
 */
void report_magics(void)
{
	int     i = 0, j, k;
	char    Dummy[80];
	cptr    info[128];
	int     info2[128];


	if (p_ptr->blind)
	{
		info2[i]  = report_magics_aux(p_ptr->blind);
#ifdef JP
		info[i++] = "あなたは目が見えない。";
#else
		info[i++] = "You cannot see";
#endif

	}
	if (p_ptr->confused)
	{
		info2[i]  = report_magics_aux(p_ptr->confused);
#ifdef JP
		info[i++] = "あなたは混乱している。";
#else
		info[i++] = "You are confused";
#endif

	}
	if (p_ptr->afraid)
	{
		info2[i]  = report_magics_aux(p_ptr->afraid);
#ifdef JP
		info[i++] = "あなたは恐怖に侵されている。";
#else
		info[i++] = "You are terrified";
#endif

	}
	if (p_ptr->poisoned)
	{
		info2[i]  = report_magics_aux(p_ptr->poisoned);
#ifdef JP
		info[i++] = "あなたは毒に侵されている。";
#else
		info[i++] = "You are poisoned";
#endif

	}
	if (p_ptr->image)
	{
		info2[i]  = report_magics_aux(p_ptr->image);
#ifdef JP
		info[i++] = "あなたは幻覚を見ている。";
#else
		info[i++] = "You are hallucinating";
#endif

	}
	if (p_ptr->blessed)
	{
		info2[i]  = report_magics_aux(p_ptr->blessed);
#ifdef JP
		info[i++] = "あなたは公正さを感じている。";
#else
		info[i++] = "You feel rightous";
#endif

	}
	if (p_ptr->hero)
	{
		info2[i]  = report_magics_aux(p_ptr->hero);
#ifdef JP
		info[i++] = "あなたはヒーロー気分だ。";
#else
		info[i++] = "You feel heroic";
#endif

	}
	if (p_ptr->shero)
	{
		info2[i]  = report_magics_aux(p_ptr->shero);
#ifdef JP
		info[i++] = "あなたは戦闘狂だ。";
#else
		info[i++] = "You are in a battle rage";
#endif

	}
	if (p_ptr->protevil)
	{
		info2[i]  = report_magics_aux(p_ptr->protevil);
#ifdef JP
		info[i++] = "あなたは邪悪なる存在から守られている。";
#else
		info[i++] = "You are protected from evil";
#endif

	}
	if (p_ptr->shield)
	{
		info2[i]  = report_magics_aux(p_ptr->shield);
#ifdef JP
		info[i++] = "あなたは神秘のシールドで守られている。";
#else
		info[i++] = "You are protected by a mystic shield";
#endif

	}
	if (p_ptr->invuln)
	{
		info2[i]  = report_magics_aux(p_ptr->invuln);
#ifdef JP
		info[i++] = "無敵でいられる。";
#else
		info[i++] = "You are invulnerable";
#endif

	}
	if (WRAITH_FORM())
	{
		info2[i]  = report_magics_aux(p_ptr->wraith_form);
		if (p_ptr->wraith_form_perm) info2[i] = 6;
#ifdef JP
		info[i++] = "幽体化できる。";
#else
		info[i++] = "You are incorporeal";
#endif

	}
	if (p_ptr->special_attack & ATTACK_CONFUSE)
	{
		info2[i]  = 7;
#ifdef JP
		info[i++] = "あなたの手は赤く輝いている。";
#else
		info[i++] = "Your hands are glowing dull red";
#endif

	}
	if (p_ptr->word_recall)
	{
		info2[i]  = report_magics_aux(p_ptr->word_recall);
#ifdef JP
		info[i++] = "この後帰還の詔を発動する。";
#else
		info[i++] = "You are waiting to be recalled";
#endif

	}
	if (p_ptr->alter_reality)
	{
		info2[i]  = report_magics_aux(p_ptr->alter_reality);
#ifdef JP
		info[i++] = "この後現実変容が発動する。";
#else
		info[i++] = "You waiting to be altered";
#endif

	}
	if (p_ptr->inhibit_flood)
	{
		info2[i]  = report_magics_aux(p_ptr->inhibit_flood);
#ifdef JP
		info[i++] = "大洪水が使えない。";
#else
		info[i++] = "You waiting to be able to use great flood";
#endif

	}
	if (p_ptr->oppose_acid)
	{
		info2[i]  = report_magics_aux(p_ptr->oppose_acid);
#ifdef JP
		info[i++] = "あなたは酸への耐性を持っている。";
#else
		info[i++] = "You are resistant to acid";
#endif

	}
	if (p_ptr->oppose_elec)
	{
		info2[i]  = report_magics_aux(p_ptr->oppose_elec);
#ifdef JP
		info[i++] = "あなたは電撃への耐性を持っている。";
#else
		info[i++] = "You are resistant to lightning";
#endif

	}
	if (p_ptr->oppose_fire)
	{
		info2[i]  = report_magics_aux(p_ptr->oppose_fire);
#ifdef JP
		info[i++] = "あなたは火への耐性を持っている。";
#else
		info[i++] = "You are resistant to fire";
#endif

	}
	if (p_ptr->oppose_cold)
	{
		info2[i]  = report_magics_aux(p_ptr->oppose_cold);
#ifdef JP
		info[i++] = "あなたは冷気への耐性を持っている。";
#else
		info[i++] = "You are resistant to cold";
#endif

	}
	if (p_ptr->oppose_pois)
	{
		info2[i]  = report_magics_aux(p_ptr->oppose_pois);
#ifdef JP
		info[i++] = "あなたは毒への耐性を持っている。";
#else
		info[i++] = "You are resistant to poison";
#endif

	}

	/* Save the screen */
	screen_save();

	/* Erase the screen */
	for (k = 1; k < 24; k++) prt("", k, 13);

	/* Label the information */
#ifdef JP
	prt("           魔法        :", 1, 15);
#else
	prt("     Your Current Magic:", 1, 15);
#endif


	/* We will print on top of the map (column 13) */
	for (k = 2, j = 0; j < i; j++)
	{
		/* Show the info */
#ifdef JP
		sprintf(Dummy, "あなたは%s%s", info[j],
#else
		sprintf(Dummy, "%s %s.", info[j],
#endif

			report_magic_durations[info2[j]]);
		prt(Dummy, k++, 15);

		/* Every 20 entries (lines 2 to 21), start over */
		if ((k == 22) && (j + 1 < i))
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
bool detect_traps(int range, bool known)
{
	int             x, y;
	bool            detect = FALSE;
	cave_type       *c_ptr;

	/* Scan the current panel */
	for (y = 1; y < cur_hgt - 1; y++)
	{
		for (x = 1; x <= cur_wid - 1; x++)
		{
			int dist = distance(py, px, y, x);
			if (dist > range) continue;

			/* Access the grid */
			c_ptr = &cave[y][x];

			/* Mark as detected */
			if (dist <= range && known)
			{
				if (dist <= range - 1)
					c_ptr->info |= (CAVE_IN_DETECT);

				c_ptr->info &= ~(CAVE_UNSAFE);

				/* Redraw */
				lite_spot(y, x);
			}

			/* Detect traps */
			if (is_trap(c_ptr->feat))
			{
				/* Hack -- Memorize */
				c_ptr->info |= (CAVE_MARK);

				if (c_ptr->mimic)
				{
					/* Disclose a hidden trap */
					disclose_grid(y, x);
				}
				else
				{
					/* Redraw */
					lite_spot(y, x);
				}

				/* Obvious */
				detect = TRUE;
			}
		}
	}

	if (known) p_ptr->dtrap = TRUE;

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
			if (is_hidden_door(c_ptr))
			{
				/* Pick a door */
				disclose_grid(y, x);
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
			    (c_ptr->feat == FEAT_LESS_LESS) ||
			    (c_ptr->feat == FEAT_MORE) ||
			    (c_ptr->feat == FEAT_MORE_MORE) ||
			    (c_ptr->feat == FEAT_ENTRANCE) ||
			    (c_ptr->feat == FEAT_ENTRANCE_UPWARD))
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
		    (tv == TV_STONE) ||
		    (tv == TV_AMULET) ||
		    (tv == TV_RING) ||
		    (tv == TV_STAFF) ||
		    (tv == TV_WAND) ||
		    (tv == TV_ROD) ||
		    (tv == TV_SCROLL) ||
		    (tv == TV_POTION) ||
		    (tv == TV_MAGERY_BOOK) ||
		    (tv == TV_FIRE_BOOK) ||
		    (tv == TV_AQUA_BOOK) ||
		    (tv == TV_EARTH_BOOK) ||
		    (tv == TV_WIND_BOOK) ||
		    (tv == TV_HOLY_BOOK) ||
		    (tv == TV_DEATH_BOOK) ||
		    (tv == TV_SYMBIOTIC_BOOK) ||
		    (tv == TV_WITCH_BOOK) ||
		    (tv == TV_DRAKONITE_BOOK) ||
		    (tv == TV_CRUSADE_BOOK) ||
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
			m_ptr->mflag2 |= (MFLAG2_MARK | MFLAG2_SHOW);

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
			m_ptr->mflag2 |= (MFLAG2_MARK | MFLAG2_SHOW);

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
			m_ptr->mflag2 |= (MFLAG2_MARK | MFLAG2_SHOW);

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
 * Detect all "nonliving", "undead" or "demonic" monsters on current panel
 */
bool detect_monsters_nonliving(int range)
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
		if (distance(py, px, y, x) > range) continue;

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
			m_ptr->mflag2 |= (MFLAG2_MARK | MFLAG2_SHOW);

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
 * Detect all "living" monsters
 */
bool detect_monsters_living(int range)
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
		if (distance(py, px, y, x) > range) continue;

		/* Detect living monsters */
		if (monster_living(r_ptr))
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
			m_ptr->mflag2 |= (MFLAG2_MARK | MFLAG2_SHOW);

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
		msg_print("自然なモンスターの存在を感じた！");
#else
		msg_print("You sense the presence of natural beings!");
#endif

	}

	/* Result */
	return (flag);
}


/*
 * Detect all "hot blood" monsters
 */
bool detect_monsters_thermal(int range)
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
		if (distance(py, px, y, x) > range) continue;

		/* Detect hot blood monsters */
		if (!(r_ptr->flags2 & RF2_COLD_BLOOD) || (r_ptr->flags2 & RF2_AURA_FIRE))
		{
			if (r_ptr->flags2 & RF2_AURA_FIRE) r_ptr->r_flags2 |= (RF2_AURA_FIRE);

			/* Update monster recall window */
			if (p_ptr->monster_race_idx == m_ptr->r_idx)
			{
				/* Window stuff */
				p_ptr->window |= (PW_MONSTER);
			}

			/* Repair visibility later */
			repair_monsters = TRUE;

			/* Hack -- Detect monster */
			m_ptr->mflag2 |= (MFLAG2_MARK | MFLAG2_SHOW);

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
		msg_print("体温のあるモンスターの存在を感じた！");
#else
		msg_print("You sense the presence of thermal beings!");
#endif

	}

	/* Result */
	return (flag);
}


/*
 * Detect all monsters it has mind on current panel
 */
bool detect_monsters_mind(int range)
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
		if (distance(py, px, y, x) > range) continue;

		/* Detect non-living monsters */
		if (!(r_ptr->flags2 & RF2_EMPTY_MIND))
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
			m_ptr->mflag2 |= (MFLAG2_MARK | MFLAG2_SHOW);

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
		msg_print("殺気を感じとった！");
#else
		msg_print("You sense the presence of someone's mind!");
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
		if (strchr(Match, r_ptr->d_char))
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
			m_ptr->mflag2 |= (MFLAG2_MARK | MFLAG2_SHOW);

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
 * Detect everything
 */
bool detect_all(int range)
{
	bool detect = FALSE;

	/* Detect everything */
	if (detect_traps(range, TRUE)) detect = TRUE;
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
	u32b    flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;
	bool    obvious = FALSE;


	/* Affect all (nearby) monsters */
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

		/* Jump directly to the target monster */
		if (project(0, 0, y, x, dam, typ, flg, MODIFY_ELEM_MODE_MAGIC)) obvious = TRUE;
	}

	/* Result */
	return (obvious);
}


/*
 * Speed monsters
 */
bool speed_monsters(int plev)
{
	return (project_hack(GF_OLD_SPEED, plev));
}

/*
 * Slow monsters
 */
bool slow_monsters(int plev)
{
	return (project_hack(GF_OLD_SLOW, plev));
}

/*
 * Sleep monsters
 */
bool sleep_monsters(int plev)
{
	return (project_hack(GF_OLD_SLEEP, plev));
}


/*
 * Banish evil monsters
 */
bool banish_evil(int dist)
{
	return (project_hack(GF_AWAY_EVIL, dist));
}


/*
 * Dispel undead monsters
 */
bool dispel_undead(int dam)
{
	return project_hack(GF_DISP_UNDEAD, dam);
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
 * Crusade
 */
bool crusade(int plev)
{
	return (project_hack(GF_CRUSADE, plev));
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
/*		monster_race    *r_ptr = &r_info[m_ptr->r_idx]; */

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip aggravating monster (or player) */
		if (i == who) continue;

		/* Wake up nearby sleeping monsters */
		if ((who ? m_ptr->ddis : m_ptr->cdis) < MAX_SIGHT * 2)
		{
			/* Wake up */
			if (m_ptr->csleep)
			{
				/* Wake up */
				m_ptr->csleep = 0;
				if (r_info[m_ptr->r_idx].flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) p_ptr->update |= (PU_MON_LITE);
				sleep = TRUE;
			}
			if (!is_pet(m_ptr)) m_ptr->mflag2 |= MFLAG2_NOPET;
		}

		/* Speed up monsters in line of sight */
		if (player_has_los_bold(m_ptr->fy, m_ptr->fx))
		{
			if (!is_pet(m_ptr))
			{
				m_ptr->fast = MIN(200, m_ptr->fast + 100);
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
	if (p_ptr->riding) p_ptr->update |= PU_BONUS;
}



/*
 * Delete all non-unique/non-quest monsters of a given "type" from the level
 */
bool symbol_genocide(int power, int player_cast)
{
	int     i;
	char    typ;
	bool    result = FALSE;
	int     msec = delay_factor * delay_factor * delay_factor;

	/* Prevent genocide in quest levels */
	if ((p_ptr->inside_quest && !random_quest_number(dun_level)) || p_ptr->inside_arena)
	{
		return (FALSE);
	}

	/* Mega-Hack -- Get a monster symbol */
#ifdef JP
	while(!get_com("どの種類(文字)のモンスターを抹殺しますか: ", &typ, FALSE));
#else
	while(!get_com("Choose a monster race (by symbol) to genocide: ", &typ, FALSE));
#endif


	/* Delete the monsters of that "type" */
	for (i = 1; i < m_max; i++)
	{
		monster_type    *m_ptr = &m_list[i];
		monster_race    *r_ptr = &r_info[m_ptr->r_idx];
		bool angry = FALSE;
		char m_name[80];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip "wrong" monsters */
		if (r_ptr->d_char != typ) continue;

		if (is_pet(m_ptr) && !player_cast) continue;

		/* Hack -- Skip Unique Monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) angry = TRUE;

		/* Hack -- Skip Quest Monsters */
		else if (r_ptr->flags1 & RF1_QUESTOR) angry = TRUE;

		else if (r_ptr->flags7 & RF7_UNIQUE2) angry = TRUE;

		else if (i == p_ptr->riding) angry = TRUE;

		else if (player_cast && (r_ptr->level > randint0(power))) angry = TRUE;

		else if (player_cast && (m_ptr->mflag2 & MFLAG2_NOGENO)) angry = TRUE;

		/* Delete the monster */
		else delete_monster_idx(i);

		if (angry && player_cast)
		{
			monster_desc(m_name, m_ptr, 0);
			if (m_ptr->ml && !p_ptr->blind)
			{
#ifdef JP
				msg_format("%^sには効果がなかった。", m_name);
#else
				msg_format("%^s is unaffected.", m_name);
#endif
			}
			if (m_ptr->csleep)
			{
				m_ptr->csleep = 0;
				if (r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) p_ptr->update |= (PU_MON_LITE);
				if (m_ptr->ml && !p_ptr->blind)
				{
#ifdef JP
					msg_format("%^sが目を覚ました。", m_name);
#else
					msg_format("%^s wakes up.", m_name);
#endif
				}
			}
			if (is_friendly(m_ptr) && !is_pet(m_ptr))
			{
				if (m_ptr->ml && !p_ptr->blind)
				{
#ifdef JP
					msg_format("%sは怒った！", m_name);
#else
					msg_format("%^s gets angry!", m_name);
#endif
				}
				set_hostile(m_ptr);
			}
			if (one_in_(13)) m_ptr->mflag2 |= MFLAG2_NOGENO;
		}

		if (player_cast)
		{
			/* Take damage */
#ifdef JP
			take_hit(DAMAGE_GENO, randint1(4), "抹殺の呪文を唱えた疲労");
#else
			take_hit(DAMAGE_GENO, randint1(4), "the strain of casting Genocide");
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

		/* Take note */
		result = TRUE;
	}

	return (result);
}


/*
 * Delete all nearby (non-unique) monsters
 */
bool mass_genocide(int power, int player_cast)
{
	int     i;
	bool    result = FALSE;
	int     msec = delay_factor * delay_factor * delay_factor;


	/* Prevent mass genocide in quest levels */
	if ((p_ptr->inside_quest && !random_quest_number(dun_level)) || p_ptr->inside_arena)
	{
		return (FALSE);
	}

	/* Delete the (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type    *m_ptr = &m_list[i];
		monster_race    *r_ptr = &r_info[m_ptr->r_idx];
		bool angry = FALSE;
		char m_name[80];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip distant monsters */
		if (m_ptr->cdis > MAX_SIGHT) continue;

		if (is_pet(m_ptr) && !player_cast) continue;

		/* Hack -- Skip unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) angry = TRUE;

		/* Hack -- Skip Quest Monsters */
		else if (r_ptr->flags1 & RF1_QUESTOR) angry = TRUE;

		else if (r_ptr->flags7 & RF7_UNIQUE2) angry = TRUE;

		else if (i == p_ptr->riding) angry = TRUE;

		else if (player_cast && (r_ptr->level > randint0(power))) angry = TRUE;

		else if (player_cast && (m_ptr->mflag2 & MFLAG2_NOGENO)) angry = TRUE;

		/* Delete the monster */
		else delete_monster_idx(i);

		if (angry && player_cast)
		{
			monster_desc(m_name, m_ptr, 0);
			if (m_ptr->ml && !p_ptr->blind)
			{
#ifdef JP
				msg_format("%^sには効果がなかった。", m_name);
#else
				msg_format("%^s is unaffected.", m_name);
#endif
			}
			if (m_ptr->csleep)
			{
				m_ptr->csleep = 0;
				if (r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) p_ptr->update |= (PU_MON_LITE);
				if (m_ptr->ml && !p_ptr->blind)
				{
#ifdef JP
					msg_format("%^sが目を覚ました。", m_name);
#else
					msg_format("%^s wakes up.", m_name);
#endif
				}
			}
			if (is_friendly(m_ptr) && !is_pet(m_ptr))
			{
				if (m_ptr->ml && !p_ptr->blind)
				{
#ifdef JP
					msg_format("%sは怒った！", m_name);
#else
					msg_format("%^s gets angry!", m_name);
#endif
				}
				set_hostile(m_ptr);
			}
			if (one_in_(13)) m_ptr->mflag2 |= MFLAG2_NOGENO;
		}

		if (player_cast)
		{
			/* Hack -- visual feedback */
#ifdef JP
			take_hit(DAMAGE_GENO, randint1(3), "周辺抹殺の呪文を唱えた疲労");
#else
			take_hit(DAMAGE_GENO, randint1(3), "the strain of casting Mass Genocide");
#endif

		}

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

		/* Note effect */
		result = TRUE;
	}

	return (result);
}



/*
 * Delete all nearby (non-unique) undead
 */
bool mass_genocide_undead(int power, int player_cast)
{
	int     i;
	bool    result = FALSE;
	int     msec = delay_factor * delay_factor * delay_factor;


	/* Prevent mass genocide in quest levels */
	if ((p_ptr->inside_quest && !random_quest_number(dun_level)) || p_ptr->inside_arena)
	{
		return (FALSE);
	}

	/* Delete the (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		monster_type    *m_ptr = &m_list[i];
		monster_race    *r_ptr = &r_info[m_ptr->r_idx];
		bool angry = FALSE;
		char m_name[80];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		if (!(r_ptr->flags3 & RF3_UNDEAD)) continue;

		/* Skip distant monsters */
		if (m_ptr->cdis > MAX_SIGHT) continue;

		if (is_pet(m_ptr) && !player_cast) continue;

		/* Hack -- Skip unique monsters */
		if (r_ptr->flags1 & (RF1_UNIQUE)) angry = TRUE;

		/* Hack -- Skip Quest Monsters */
		else if (r_ptr->flags1 & RF1_QUESTOR) angry = TRUE;

		else if (r_ptr->flags7 & RF7_UNIQUE2) angry = TRUE;

		else if (i == p_ptr->riding) angry = TRUE;

		else if (player_cast && (r_ptr->level > randint0(power))) angry = TRUE;

		else if (player_cast && (m_ptr->mflag2 & MFLAG2_NOGENO)) angry = TRUE;

		/* Delete the monster */
		else delete_monster_idx(i);

		if (angry && player_cast)
		{
			monster_desc(m_name, m_ptr, 0);
			if (m_ptr->ml && !p_ptr->blind)
			{
#ifdef JP
				msg_format("%^sには効果がなかった。", m_name);
#else
				msg_format("%^s is unaffected.", m_name);
#endif
			}
			if (m_ptr->csleep)
			{
				m_ptr->csleep = 0;
				if (r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) p_ptr->update |= (PU_MON_LITE);
				if (m_ptr->ml && !p_ptr->blind)
				{
#ifdef JP
					msg_format("%^sが目を覚ました。", m_name);
#else
					msg_format("%^s wakes up.", m_name);
#endif
				}
			}
			if (is_friendly(m_ptr) && !is_pet(m_ptr))
			{
				if (m_ptr->ml && !p_ptr->blind)
				{
#ifdef JP
					msg_format("%sは怒った！", m_name);
#else
					msg_format("%^s gets angry!", m_name);
#endif
				}
				set_hostile(m_ptr);
			}
			if (one_in_(13)) m_ptr->mflag2 |= MFLAG2_NOGENO;
		}

		if (player_cast)
		{
			/* Hack -- visual feedback */
#ifdef JP
			take_hit(DAMAGE_GENO, randint1(3), "アンデッド消滅の呪文を唱えた疲労");
#else
			take_hit(DAMAGE_GENO, randint1(3), "the strain of casting Mass Genocide");
#endif

		}

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

		/* Note effect */
		result = TRUE;
	}

	return (result);
}



/*
 * Probe nearby monsters
 */
bool probing(void)
{
	int     i, speed, ac;
	int cu, cv;
	bool    probe = FALSE;
	char buf[256];
	cptr align_gne;
	cptr align_lnc;

	cu = Term->scr->cu;
	cv = Term->scr->cv;
	Term->scr->cu = 0;
	Term->scr->cv = 1;

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
			char m_name[80];

			/* Start the message */
			if (!probe)
			{
#ifdef JP
				msg_print("調査中...");
#else
				msg_print("Probing...");
#endif
			}

			msg_print(NULL);

			if (m_ptr->ap_r_idx != m_ptr->r_idx)
			{
				m_ptr->ap_r_idx = m_ptr->r_idx;
				lite_spot(m_ptr->fy, m_ptr->fx);
			}
			/* Get "the monster" or "something" */
			monster_desc(m_name, m_ptr, 0x204);

			speed = m_ptr->mspeed - 110;
			if (m_ptr->stoning) speed -= m_ptr->stoning / 5;
			if(m_ptr->fast) speed += 10;
			if(m_ptr->slow) speed -= 10;

			ac = r_ptr->ac;
			if (m_ptr->stoning) ac += m_ptr->stoning / 5;

			/* Get the monster's alignment */
#ifdef JP
			if ((r_ptr->flags3 & RF3_EVIL) && (r_ptr->flags3 & RF3_GOOD)) align_gne = "善悪";
			else if (r_ptr->flags3 & RF3_EVIL) align_gne = "邪悪";
			else if (r_ptr->flags3 & RF3_GOOD) align_gne = "善良";
			else if ((m_ptr->sub_align & SUB_ALIGN_EVIL) && (m_ptr->sub_align & SUB_ALIGN_GOOD)) align_gne = "中立(善悪)";
			else if (m_ptr->sub_align & SUB_ALIGN_EVIL) align_gne = "中立(邪悪)";
			else if (m_ptr->sub_align & SUB_ALIGN_GOOD) align_gne = "中立(善良)";
			else align_gne = "中立";

			if ((r_ptr->flags7 & RF7_LAWFUL) && (r_ptr->flags7 & RF7_CHAOTIC)) align_lnc = "秩沌";
			else if (r_ptr->flags7 & RF7_LAWFUL) align_lnc = "秩序";
			else if (r_ptr->flags7 & RF7_CHAOTIC) align_lnc = "混沌";
			else if ((m_ptr->sub_align & SUB_ALIGN_LAWFUL) && (m_ptr->sub_align & SUB_ALIGN_CHAOTIC)) align_lnc = "中立(秩沌)";
			else if (m_ptr->sub_align & SUB_ALIGN_LAWFUL) align_lnc = "中立(秩序)";
			else if (m_ptr->sub_align & SUB_ALIGN_CHAOTIC) align_lnc = "中立(混沌)";
			else align_lnc = "中立";
#else
			if ((r_ptr->flags3 & RF3_EVIL) && (r_ptr->flags3 & RF3_GOOD)) align_gne = "g.&e.";
			else if (r_ptr->flags3 & RF3_EVIL) align_gne = "evil";
			else if (r_ptr->flags3 & RF3_GOOD) align_gne = "good";
			else if ((m_ptr->sub_align & SUB_ALIGN_EVIL) && (m_ptr->sub_align & SUB_ALIGN_GOOD)) align_gne = "neutral(g.&e.)";
			else if (m_ptr->sub_align & SUB_ALIGN_EVIL) align_gne = "neutral(evil)";
			else if (m_ptr->sub_align & SUB_ALIGN_GOOD) align_gne = "neutral(good)";
			else align_gne = "neutral";

			if ((r_ptr->flags7 & RF7_LAWFUL) && (r_ptr->flags7 & RF7_CHAOTIC)) align_lnc = "l.&c.";
			else if (r_ptr->flags7 & RF7_LAWFUL) align_lnc = "lawful";
			else if (r_ptr->flags7 & RF7_CHAOTIC) align_lnc = "chaotic";
			else if ((m_ptr->sub_align & SUB_ALIGN_LAWFUL) && (m_ptr->sub_align & SUB_ALIGN_CHAOTIC)) align_lnc = "neutral(l.&c.)";
			else if (m_ptr->sub_align & SUB_ALIGN_LAWFUL) align_lnc = "neutral(lawful)";
			else if (m_ptr->sub_align & SUB_ALIGN_CHAOTIC) align_lnc = "neutral(chaotic)";
			else align_lnc = "neutral";
#endif

			/* Describe the monster */
#ifdef JP
#ifdef L64
			sprintf(buf,"%s ... 属性:%s-%s HP:%d/%d AC:%d 速度:%s%d 経験:", m_name, align_gne, align_lnc, m_ptr->hp, m_ptr->maxhp, ac, (speed > 0) ? "+" : "", speed);
#else
			sprintf(buf,"%s ... 属性:%s-%s HP:%ld/%ld AC:%d 速度:%s%d 経験:", m_name, align_gne, align_lnc, m_ptr->hp, m_ptr->maxhp, ac, (speed > 0) ? "+" : "", speed);
#endif
#else
#ifdef L64
			sprintf(buf, "%s ... align:%s-%s HP:%d/%d AC:%d speed:%s%d exp:", m_name, align_gne, align_lnc, m_ptr->hp, m_ptr->maxhp, ac, (speed > 0) ? "+" : "", speed);
#else
			sprintf(buf, "%s ... align:%s-%s HP:%ld/%ld AC:%d speed:%s%d exp:", m_name, align_gne, align_lnc, m_ptr->hp, m_ptr->maxhp, ac, (speed > 0) ? "+" : "", speed);
#endif
#endif
			if (r_ptr->next_r_idx)
			{
				strcat(buf, format("%d/%d ", m_ptr->exp, r_ptr->next_exp));
			}
			else
			{
				strcat(buf, "xxx ");
			}

#ifdef JP
			if (m_ptr->csleep) strcat(buf,"睡眠 ");
			if (m_ptr->stunned) strcat(buf,"朦朧 ");
			if (m_ptr->monfear) strcat(buf,"恐怖 ");
			if (m_ptr->confused) strcat(buf,"混乱 ");
			if (m_ptr->stoning) strcat(buf,"石化 ");
			if (m_ptr->silent || m_ptr->silent_song) strcat(buf,"沈黙 ");
			if (m_ptr->invulner) strcat(buf,"無敵 ");
#else
			if (m_ptr->csleep) strcat(buf,"sleeping ");
			if (m_ptr->stunned) strcat(buf,"stunned ");
			if (m_ptr->monfear) strcat(buf,"scared ");
			if (m_ptr->confused) strcat(buf,"confused ");
			if (m_ptr->stoning) strcat(buf,"stoning ");
			if (m_ptr->silent || m_ptr->silent_song) strcat(buf,"silent ");
			if (m_ptr->invulner) strcat(buf,"invulnerable ");
#endif
			buf[strlen(buf)-1] = '\0';
			prt(buf,0,0);

			/* HACK : Add the line to message buffer */
			message_add(buf);
			p_ptr->window |= (PW_MESSAGE);
			window_stuff();

			if (m_ptr->ml) move_cursor_relative(m_ptr->fy, m_ptr->fx);
			inkey();

			Term_erase(0, 0, 255);

			/* Learn everything about this monster */
			if (lore_do_probe(m_ptr->r_idx))
			{
				char buf[80];

				/* Get base name of monster */
				strcpy(buf, (r_name + r_ptr->name));

#ifdef JP
				/* Note that we learnt some new flags  -Mogami- */
				msg_format("%sについてさらに詳しくなった気がする。", buf);
#else
				/* Pluralize it */
				plural_aux(buf);

				/* Note that we learnt some new flags  -Mogami- */
				msg_format("You now know more about %s.", buf);
#endif
				/* Clear -more- prompt */
				msg_print(NULL);
			}

			/* Probe worked */
			probe = TRUE;
		}
	}

	Term->scr->cu = cu;
	Term->scr->cv = cv;
	Term_fresh();

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
bool destroy_area(int y1, int x1, int r)
{
	int       y, x, k, t;
	cave_type *c_ptr;
	bool      flag = FALSE;


	/* Prevent destruction of quest levels and town */
	if ((p_ptr->inside_quest && quest_is_fixed(p_ptr->inside_quest)) || !dun_level)
	{
		return (FALSE);
	}

	/* Big area of affect */
	for (y = (y1 - r); y <= (y1 + r); y++)
	{
		for (x = (x1 - r); x <= (x1 + r); x++)
		{
			monster_type *m_ptr;
			monster_race *r_ptr;

			/* Skip illegal grids */
			if (!in_bounds(y, x)) continue;

			/* Extract the distance */
			k = distance(y1, x1, y, x);

			/* Stay in the circle of death */
			if (k > r) continue;

			/* Access the grid */
			c_ptr = &cave[y][x];
			m_ptr = &m_list[c_ptr->m_idx];
			r_ptr = &r_info[m_ptr->r_idx];

			/* Lose room and vault */
			c_ptr->info &= ~(CAVE_ROOM | CAVE_ICKY | CAVE_UNSAFE | CAVE_OBJECT);

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

			if ((r_ptr->flags1 & RF1_QUESTOR))
			{
				/* Heal the monster */
				m_list[c_ptr->m_idx].hp = m_list[c_ptr->m_idx].maxhp;

				/* Try to teleport away quest monsters */
				if (!teleport_away(c_ptr->m_idx, (r * 2) + 1)) continue;
			}
			else
			{
				if (c_ptr->m_idx)
				{
					if (record_named_pet && is_pet(&m_list[c_ptr->m_idx]) && m_list[c_ptr->m_idx].nickname)
					{
						char m_name[80];

						monster_desc(m_name, &m_list[c_ptr->m_idx], 0x08);
						do_cmd_write_nikki(NIKKI_NAMED_PET, 6, m_name);
					}
				}
				/* Delete the monster (if any) */
				delete_monster(y, x);
			}

			if (preserve_mode)
			{
				s16b this_o_idx, next_o_idx = 0;

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
			delete_object(y, x);

			if (p_ptr->use_decoy)
			{
				if ((y == p_ptr->decoy_y) && (x == p_ptr->decoy_x)) break_decoy();
			}

			/* Destroy "valid" grids */
			if (!cave_perma_bold(y, x))
			{
				/* Wall (or floor) type */
				t = randint0(200);

				/* Granite */
				if (t < 20)
				{
					/* Create granite wall */
					cave_set_feat(y, x, FEAT_WALL_EXTRA);
				}

				/* Quartz */
				else if (t < 70)
				{
					/* Create quartz vein */
					cave_set_feat(y, x, FEAT_QUARTZ);
				}

				/* Magma */
				else if (t < 100)
				{
					/* Create magma vein */
					cave_set_feat(y, x, FEAT_MAGMA);
				}

				/* Floor */
				else
				{
					/* Create floor */
					cave_force_set_floor(y, x);
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
 * A monster falls into air...
 */
bool mon_fall_into_air(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	if (m_idx == p_ptr->riding) return FALSE;

	if (!(r_ptr->flags7 & RF7_CAN_FLY))
	{
		char m_name[80];

		if (m_ptr->ml || is_pet(m_ptr))
		{
			monster_desc(m_name, m_ptr, 0);
#ifdef JP
			msg_format("%^sは空中に消えていった...", m_name);
#else
			msg_format("%^s falls into air...", m_name);
#endif
			sound(SOUND_FALL);
		}

		if (record_named_pet && is_pet(m_ptr) && m_ptr->nickname)
		{
			char pm_name[80];

			monster_desc(pm_name, m_ptr, 0x08);
			do_cmd_write_nikki(NIKKI_NAMED_PET, 9, pm_name);
		}

		/* Check for quest completion */
		check_quest_completion(m_ptr);

		delete_monster_idx(m_idx);
		return TRUE;
	}

	return FALSE;
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


	/* Prevent destruction of quest levels and town */
	if ((p_ptr->inside_quest && quest_is_fixed(p_ptr->inside_quest)) || !dun_level)
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
	if (hurt && !prace_is_(RACE_GHOST) && !WRAITH_FORM())
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

			if (cave[y][x].m_idx) continue;

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
			damage = 200;
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
					damage = damroll(10, 4);
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

			if (p_ptr->riding)
			{
				cave[oy][ox].m_idx = cave[py][px].m_idx;
				cave[py][px].m_idx = p_ptr->riding;
				m_list[p_ptr->riding].fy = py;
				m_list[p_ptr->riding].fx = px;
				update_mon(p_ptr->riding, TRUE);
			}

			/* Redraw the old spot */
			lite_spot(oy, ox);

			/* Redraw the new spot */
			lite_spot(py, px);

			/* Check for new panel */
			verify_panel();

			set_mermaid_in_water();
		}

		/* Important -- no wall on player */
		map[16+py-cy][16+px-cx] = FALSE;

		/* Take some damage */
		ACTIVATE_MULTISHADOW();

		if (damage)
		{
			char *killer;

			if (m_idx)
			{
				char m_name[80];
				monster_type *m_ptr = &m_list[m_idx];

				/* Get the monster's real name */
				monster_desc(m_name, m_ptr, 0x288);

#ifdef JP
				killer = format("%sの起こした地震", m_name);
#else
				killer = format("an earthquake caused by %s", m_name);
#endif
			}
			else
			{
#ifdef JP
				killer = "地震";
#else
				killer = "an earthquake";
#endif
			}
			take_hit(DAMAGE_ATTACK, damage, killer);
		}

		STOP_MULTISHADOW();
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

			if (c_ptr->m_idx == p_ptr->riding) continue;

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
							if (is_glyph_grid(&cave[y][x])) continue;
							if (is_explosive_rune_grid(&cave[y][x])) continue;

							/* Important -- Skip "quake" grids */
							if (map[16+y-cy][16+x-cx]) continue;

							if (cave[y][x].m_idx) continue;
							if ((y == py) && (x == px)) continue;

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
					if (show_damage && m_ptr->ml && (damage > 0) && sn)
#ifdef JP
						msg_format("%^sに%dのダメージ。", m_name, damage);
#else
						msg_format("%^s takes %d damages.", m_name, damage);
#endif

					/* Delete (not kill) "dead" monsters */
					if (m_ptr->hp < 0)
					{
						/* Message */
#ifdef JP
						msg_format("%^sは岩石に埋もれてしまった！", m_name);
#else
						msg_format("%^s is embedded in the rock!", m_name);
#endif

						if (c_ptr->m_idx)
						{
							if (record_named_pet && is_pet(&m_list[c_ptr->m_idx]) && m_list[c_ptr->m_idx].nickname)
							{
								char m2_name[80];

								monster_desc(m2_name, m_ptr, 0x08);
								do_cmd_write_nikki(NIKKI_NAMED_PET, 7, m2_name);
							}
						}

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

						if (cave[sy][sx].feat == FEAT_AIR)
						{
							(void)mon_fall_into_air(m_idx);
						}
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
/*			if ((yy == py) && (xx == px)) continue; */

			/* Destroy location (if valid) */
			if (cave_valid_bold(yy, xx))
			{
				bool floor = cave_floor_bold(yy, xx);

				/* Delete objects */
				delete_object(yy, xx);

				/* Clear runes flag */
				c_ptr->info &= ~(CAVE_OBJECT);

				if (p_ptr->use_decoy)
				{
					if ((yy == p_ptr->decoy_y) && (xx == p_ptr->decoy_x)) break_decoy();
				}

				/* Wall (or floor) type */
				t = (floor ? randint0(100) : 200);

				/* Granite */
				if (t < 20)
				{
					/* Create granite wall */
					cave_set_feat(yy, xx, FEAT_WALL_EXTRA);
				}

				/* Quartz */
				else if (t < 70)
				{
					/* Create quartz vein */
					cave_set_feat(yy, xx, FEAT_QUARTZ);
				}

				/* Magma */
				else if (t < 100)
				{
					/* Create magma vein */
					cave_set_feat(yy, xx, FEAT_MAGMA);
				}

				/* Floor */
				else
				{
					/* Create floor */
					cave_force_set_floor(yy, xx);
				}
			}
		}
	}


	/* Mega-Hack -- Forget the view and lite */
	p_ptr->update |= (PU_UN_VIEW | PU_UN_LITE);

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE | PU_BONUS);

	/* Update the monsters */
	p_ptr->update |= (PU_DISTANCE);

	/* Update the health bar */
	p_ptr->redraw |= (PR_HEALTH | PR_UHEALTH);

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


void discharge_minion(void)
{
	int i;
	bool okay = TRUE;

	for (i = 1; i < m_max; i++)
	{
		monster_type *m_ptr = &m_list[i];
		if (!m_ptr->r_idx || !is_pet(m_ptr)) continue;
		if (m_ptr->nickname) okay = FALSE;
	}
	if (!okay || p_ptr->riding)
	{
#ifdef JP
		if (!get_check("本当に全ペットを爆破しますか？"))
#else
		if (!get_check("You will blast all pets. Are you sure? "))
#endif
			return;
	}
	for (i = 1; i < m_max; i++)
	{
		int dam;
		monster_type *m_ptr = &m_list[i];
		monster_race *r_ptr;
		int fy, fx;

		if (!m_ptr->r_idx || !is_pet(m_ptr)) continue;
		r_ptr = &r_info[m_ptr->r_idx];

		/* Uniques resist discharging */
		if (r_ptr->flags1 & RF1_UNIQUE)
		{
			char m_name[80];
			monster_desc(m_name, m_ptr, 0x00);
#ifdef JP
			msg_format("%sは爆破されるのを嫌がり、勝手に自分の世界へと帰った。", m_name);
#else
			msg_format("%^s resists to be blasted, and run away.", m_name);
#endif
			delete_monster_idx(i);
			continue;
		}
		dam = m_ptr->hp / 8;
		if (dam > 666) dam = 666;
		fy = m_ptr->fy;
		fx = m_ptr->fx;
		delete_monster_idx(i);
		project(0, 2+(r_ptr->level/20), fy, fx, dam, GF_PLASMA,
			PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_MONSTER | PROJECT_JUMP, MODIFY_ELEM_MODE_MAGIC);
	}
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
	int i;

	/* Clear them all */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		cave_type *c_ptr = &cave[y][x];

		/* No longer in the array */
		c_ptr->info &= ~(CAVE_TEMP);

		/* Update only non-CAVE_GLOW grids */
		/* if (c_ptr->info & (CAVE_GLOW)) continue; */

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
	int i;

	/* Clear them all */
	for (i = 0; i < temp_n; i++)
	{
		int y = temp_y[i];
		int x = temp_x[i];

		cave_type *c_ptr = &cave[y][x];

		/* No longer in the array */
		c_ptr->info &= ~(CAVE_TEMP);

		/* Darken the grid */
		c_ptr->info &= ~(CAVE_GLOW);

		/* Hack -- Forget "boring" grids */
		if ((c_ptr->feat <= FEAT_INVIS) || (c_ptr->feat == FEAT_DIRT) || (c_ptr->feat == FEAT_GRASS) || (c_ptr->feat == FEAT_SWAMP) || (c_ptr->feat == FEAT_TUNDRA))
		{
			/* Forget the grid */
			if (!view_torch_grids) c_ptr->info &= ~(CAVE_MARK);

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
static void cave_temp_room_aux(int cy, int cx, int y, int x, bool only_room)
{
	cave_type *c_ptr;

	/* Get the grid */
	c_ptr = &cave[y][x];

	/* Avoid infinite recursion */
	if (c_ptr->info & (CAVE_TEMP)) return;

	/* Do not "leave" the current room */
	if (!(c_ptr->info & (CAVE_ROOM)))
	{
		if (only_room) return;

		/* Verify */
		if (!in_bounds2(y, x)) return;

		/* Do not exceed the maximum spell range */
		if (distance(cy, cx, y, x) > MAX_RANGE) return;

		/* Verify this grid */
		/*
		 * The reason why it is ==6 instead of >5 is that 8 is impossible
		 * due to the check for cave_bold above.
		 * 7 lights dead-end corridors (you need to do this for the
		 * checkboard interesting rooms, so that the boundary is lit
		 * properly.
		 * This leaves only a check for 6 bounding walls!
		 */
		if (in_bounds(y, x) && cave_floor_bold(y, x) &&
		    (next_to_walls_adj(y, x) == 6) && (next_to_open(y, x) <= 1)) return;
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
	cave_temp_room_aux(py, px, y, x, FALSE);
}

/*
 * Aux function -- see below
 */
static void cave_temp_unlite_room_aux(int y, int x)
{
	cave_temp_room_aux(py, px, y, x, TRUE);
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
	u32b flg = PROJECT_GRID | PROJECT_KILL;

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
	(void)project(0, rad, py, px, dam, GF_LITE_WEAK, flg, MODIFY_ELEM_MODE_MAGIC);

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
	u32b flg = PROJECT_GRID | PROJECT_KILL;

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
	(void)project(0, rad, py, px, dam, GF_DARK_WEAK, flg, MODIFY_ELEM_MODE_MAGIC);

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
bool fire_ball(int typ, int dir, int dam, int rad, bool no_reduce)
{
	int tx, ty;

	u32b flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	if (no_reduce) flg |= PROJECT_NO_REDUCE;

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
	return (project(0, rad, ty, tx, dam, typ, flg, MODIFY_ELEM_MODE_MAGIC));
}


/*
 * Cast a ball spell
 * Stop if we hit a monster, act as a "ball"
 * Allow "target" mode to pass over monsters
 * Affect grids, objects, and monsters
 */
bool fire_rocket(int typ, int dir, int dam, int rad, bool no_reduce)
{
	int tx, ty;

	u32b flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	if (no_reduce) flg |= PROJECT_NO_REDUCE;

	/* Use the given direction */
	tx = px + 99 * ddx[dir];
	ty = py + 99 * ddy[dir];

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		tx = target_col;
		ty = target_row;
	}

	/* Analyze the "dir" and the "target".  Hurt items on floor. */
	return (project(0, rad, ty, tx, dam, typ, flg, MODIFY_ELEM_MODE_MAGIC));
}


/*
 * Cast a ball spell
 * Stop if we hit a monster, act as a "ball"
 * Allow "target" mode to pass over monsters
 * Affect grids, objects, and monsters
 */
bool fire_ball_hide(int typ, int dir, int dam, int rad, bool no_reduce)
{
	int tx, ty;

	u32b flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_HIDE;

	if (no_reduce) flg |= PROJECT_NO_REDUCE;

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
	return (project(0, rad, ty, tx, dam, typ, flg, MODIFY_ELEM_MODE_MAGIC));
}


/*
 * Cast a meteor spell, defined as a ball spell cast by an arbitary monster, 
 * player, or outside source, that starts out at an arbitrary location, and 
 * leaving no trail from the "caster" to the target.  This function is 
 * especially useful for bombardments and similar. -LM-
 *
 * Option to hurt the player.
 */
bool fire_meteor(int who, int typ, int y, int x, int dam, int rad, bool no_reduce)
{
	u32b flg = PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;

	if (no_reduce) flg |= PROJECT_NO_REDUCE;

	/* Analyze the "target" and the caster. */
	return (project(who, rad, y, x, dam, typ, flg, MODIFY_ELEM_MODE_MAGIC));
}


bool fire_blast(int typ, int dir, int dd, int ds, int num, int dev)
{
	int ly, lx, ld;
	int ty, tx, y, x;
	int i;

	u32b flg = PROJECT_FAST | PROJECT_THRU | PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE | PROJECT_GRID;

	/* Assume okay */
	bool result = TRUE;

	/* Use the given direction */
	if (dir != 5)
	{
		ly = ty = py + 20 * ddy[dir];
		lx = tx = px + 20 * ddx[dir];
	}

	/* Use an actual "target" */
	else if (dir == 5)
	{
		tx = target_col;
		ty = target_row;

		lx = 20 * (tx - px) + px;
		ly = 20 * (ty - py) + py;
	}

	ld = distance(py, px, ly, lx);

	/* Blast */
	for (i = 0; i < num; i++)
	{
		while (1)
		{
			/* Get targets for some bolts */
			y = rand_spread(ly, ld * dev / 20);
			x = rand_spread(lx, ld * dev / 20);

			if (distance(ly, lx, y, x) <= ld * dev / 20) break;
		}

		/* Analyze the "dir" and the "target". */
		if (!project(0, 0, y, x, damroll(dd, ds), typ, flg, MODIFY_ELEM_MODE_MAGIC))
		{
			result = FALSE;
		}
	}

	return (result);
}


/*
 * Switch position with a monster.
 */
bool teleport_swap(int dir, int plev)
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

	if (p_ptr->anti_tele)
	{
#ifdef JP
		msg_print("不思議な力がテレポートを防いだ！");
#else
		msg_print("A mysterious force prevents you from teleporting!");
#endif

		return FALSE;
	}

	if (!c_ptr->m_idx || (c_ptr->m_idx == p_ptr->riding))
	{
#ifdef JP
		msg_print("それとは場所を交換できません。");
#else
		msg_print("You can't trade places with that!");
#endif


		/* Failure */
		return FALSE;
	}

	if ((c_ptr->info & CAVE_ICKY) || (distance(ty, tx, py, px) > plev * 3 / 2 + 10))
	{
#ifdef JP
		msg_print("失敗した。");
#else
		msg_print("Failed to swap.");
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

		m_ptr->csleep = 0;
		if (r_ptr->flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) p_ptr->update |= (PU_MON_LITE);

		/* Failure */
		return FALSE;
	}

	sound(SOUND_TELEPORT);

	cave[py][px].m_idx = c_ptr->m_idx;

	/* Update the old location */
	c_ptr->m_idx = p_ptr->riding;

	/* Move the monster */
	m_ptr->fy = py;
	m_ptr->fx = px;

	/* Move the player */
	px = tx;
	py = ty;

	if (p_ptr->riding)
	{
		m_list[p_ptr->riding].fy = ty;
		m_list[p_ptr->riding].fx = tx;

		/* Update the monster (new location) */
		update_mon(p_ptr->riding, TRUE);
	}

	tx = m_ptr->fx;
	ty = m_ptr->fy;

	m_ptr->csleep = 0;

	/* Update the monster (new location) */
	update_mon(cave[ty][tx].m_idx, TRUE);

	/* Redraw the old grid */
	lite_spot(ty, tx);

	/* Redraw the new grid */
	lite_spot(py, px);

	/* Check for new panel (redraw map) */
	verify_panel();

	set_mermaid_in_water();

	/* Update stuff */
	p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_BONUS | PU_MON_LITE);

	/* Update the monsters */
	p_ptr->update |= (PU_DISTANCE);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	/* Redraw the health bar */
	if (p_ptr->health_who == cave[ty][tx].m_idx)
		p_ptr->redraw |= (PR_HEALTH);

	/* Handle stuff XXX XXX XXX */
	handle_stuff();

	/* Success */
	return TRUE;
}


/*
 * Hack -- apply a "projection()" in a direction (or at the target)
 */
bool project_hook(int typ, int dir, int dam, u32b flg)
{
	int tx, ty;
	int mod_elem_mode = MODIFY_ELEM_MODE_MAGIC;

	/* Pass through the target if needed */
	flg |= (PROJECT_THRU);

	/* Use the given direction */
	tx = px + ddx[dir];
	ty = py + ddy[dir];

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		tx = target_col;
		ty = target_row;
	}

	if (hack_elem_mod_mode != -1)
	{
		mod_elem_mode = hack_elem_mod_mode;
		hack_elem_mod_mode = -1;
	}

	/* Analyze the "dir" and the "target", do NOT explode */
	return (project(0, 0, ty, tx, dam, typ, flg, mod_elem_mode));
}


/*
 * Cast a bolt spell
 * Stop if we hit a monster, as a "bolt"
 * Affect monsters (not grids or objects)
 */
bool fire_bolt(int typ, int dir, int dam)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE | PROJECT_GRID | PROJECT_AVOIDABLE;
	return (project_hook(typ, dir, dam, flg));
}


/*
 * Cast a beam spell
 * Pass through monsters, as a "beam"
 * Affect monsters (not grids or objects)
 */
bool fire_beam(int typ, int dir, int dam)
{
	u32b flg = PROJECT_BEAM | PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM;
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
	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL;
	return (project_hook(GF_LITE_WEAK, dir, damroll(6, 8), flg));
}


bool drain_life(int dir, int dam)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_OLD_DRAIN, dir, dam, flg));
}


bool wall_to_mud(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
	return (project_hook(GF_KILL_WALL, dir, 20 + randint1(30), flg));
}


bool destroy_door(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (project_hook(GF_KILL_DOOR, dir, 0, flg));
}


bool disarm_trap(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_GRID | PROJECT_ITEM;
	return (project_hook(GF_KILL_TRAP, dir, 0, flg));
}


bool heal_monster(int dir, int dam)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_OLD_HEAL, dir, dam, flg));
}


bool speed_monster(int dir, int plev)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_OLD_SPEED, dir, plev, flg));
}


bool slow_monster(int dir, int plev)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_OLD_SLOW, dir, plev, flg));
}


bool sleep_monster(int dir, int plev)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_OLD_SLEEP, dir, plev, flg));
}


bool stasis_evil(int dir, int plev)
{
	return (fire_ball_hide(GF_STASIS_EVIL, dir, plev, 0, FALSE));
}


bool confuse_monster(int dir, int plev)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_OLD_CONF, dir, plev, flg));
}


bool stun_monster(int dir, int plev)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_STUN, dir, plev, flg));
}


bool poly_monster(int dir, int plev)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return project_hook(GF_OLD_POLY, dir, plev, flg);
}


bool fear_monster(int dir, int plev)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_TURN_ALL, dir, plev, flg));
}


bool death_ray(int dir, int plev)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL | PROJECT_REFLECTABLE;
	return (project_hook(GF_DEATH_RAY, dir, plev * 200, flg));
}


bool teleport_monster(int dir)
{
	u32b flg = PROJECT_BEAM | PROJECT_KILL;
	return (project_hook(GF_AWAY_ALL, dir, MAX_SIGHT * 5, flg));
}

/*
 * Hooks -- affect adjacent grids (radius 1 ball attack)
 */
bool trap_creation(int y, int x)
{
	u32b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, 1, y, x, 0, GF_MAKE_TRAP, flg, MODIFY_ELEM_MODE_NONE));
}


bool tree_creation(void)
{
	u32b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, 1, py, px, 0, GF_MAKE_TREE, flg, MODIFY_ELEM_MODE_NONE));
}


bool glyph_creation(void)
{
	u32b flg = PROJECT_GRID | PROJECT_ITEM;
	return (project(0, 1, py, px, 0, GF_MAKE_GLYPH, flg, MODIFY_ELEM_MODE_NONE));
}


bool destroy_doors_touch(void)
{
	u32b flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE;
	return (project(0, 1, py, px, 0, GF_KILL_DOOR, flg, MODIFY_ELEM_MODE_NONE));
}


bool sleep_monsters_touch(int plev)
{
	u32b flg = PROJECT_KILL | PROJECT_HIDE;
	return (project(0, 1, py, px, plev, GF_OLD_SLEEP, flg, MODIFY_ELEM_MODE_MAGIC));
}


bool animate_dead(int who, int y, int x)
{
	u32b flg = PROJECT_ITEM | PROJECT_HIDE;
	return (project(who, 5, y, x, 0, GF_ANIM_DEAD, flg, MODIFY_ELEM_MODE_NONE));
}


void call_chaos(int plev)
{
	int Chaos_type, dummy, dir;
	bool line_chaos = FALSE;

	int hurt_types[] =
	{
		GF_ACID,        GF_ELEC,         GF_FIRE,      GF_COLD,
		GF_POIS,        GF_LITE,         GF_DARK,      GF_NETHER,
		GF_WATER,       GF_PLASMA,       GF_SHARDS,    GF_SOUND,
		GF_CONFUSION,   GF_CHAOS,        GF_STONE,     GF_DISENCHANT,
		GF_FORCE,       GF_INERTIA,      GF_TIME,      GF_GRAVITY,
		GF_ICE,         GF_NUKE,         GF_ROCKET,    GF_MISSILE,
		GF_PHYSICAL,    GF_BLUNT,        GF_EDGED,     GF_MANA,
		GF_METEOR,      GF_DISINTEGRATE, GF_HOLY_FIRE, GF_HELL_FIRE,
		GF_GODLY_SPEAR, GF_PURE_FIRE,    GF_PURE_AQUA, GF_PURE_EARTH,
		GF_PURE_WIND
	};

	Chaos_type = hurt_types[randint0((sizeof hurt_types) / (sizeof (int)))];
	if (one_in_(4)) line_chaos = TRUE;

	if (one_in_(6))
	{
		for (dummy = 1; dummy < 10; dummy++)
		{
			if (dummy - 5)
			{
				if (line_chaos)
					fire_beam(Chaos_type, dummy, 150);
				else
					fire_ball(Chaos_type, dummy, 150, 2, FALSE);
			}
		}
	}
	else if (one_in_(3))
	{
		fire_ball(Chaos_type, 0, 500, 8, FALSE);
	}
	else
	{
		if (!get_aim_dir(&dir)) return;
		if (line_chaos)
			fire_beam(Chaos_type, dir, 250);
		else
			fire_ball(Chaos_type, dir, 250, 3 + (plev / 35), FALSE);
	}
}


static void wall_breaker(void)
{
	int i;
	int y, x;
	int attempts = 1000;

	if (randint1(80 + p_ptr->lev) < 70)
	{
		while(attempts--)
		{
			scatter(&y, &x, py, px, 4, 0);

			if (!cave_floor_bold(y, x)) continue;

			if ((y != py) || (x != px)) break;
		}

		project(0, 0, y, x, 20 + randint1(30), GF_KILL_WALL,
				  (PROJECT_BEAM | PROJECT_THRU | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL), MODIFY_ELEM_MODE_NONE);
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
			while(1)
			{
				scatter(&y, &x, py, px, 10, 0);

				if ((y != py) && (x != px)) break;
			}

			project(0, 0, y, x, 20 + randint1(30), GF_KILL_WALL,
					  (PROJECT_BEAM | PROJECT_THRU | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL), MODIFY_ELEM_MODE_NONE);
		}
	}
}


/*
 * Activate the evil Topi Ylinen curse
 * rr9: Stop the nasty things when a Cyberdemon is summoned
 * or the player gets paralyzed.
 */
bool activate_ty_curse(bool stop_ty, int *count)
{
	int     i = 0;

	u32b flg = (PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_JUMP);

	do
	{
		switch (randint1(34))
		{
		case 28: case 29:
			if (!(*count))
			{
#ifdef JP
				msg_print("地面が揺れた...");
#else
				msg_print("The ground trembles...");
#endif

				earthquake(py, px, 5 + randint0(10));
				if (!one_in_(6)) break;
			}
		case 30: case 31:
			if (!(*count))
			{
				int dam = damroll(10, 10);
#ifdef JP
				msg_print("純粋な魔力の次元への扉が開いた！");
#else
				msg_print("A portal opens to a plane of raw mana!");
#endif

				project(0, 8, py, px, dam, GF_MANA, flg, MODIFY_ELEM_MODE_NONE);
#ifdef JP
				take_hit(DAMAGE_NOESCAPE, dam, "純粋な魔力の解放");
#else
				take_hit(DAMAGE_NOESCAPE, dam, "released pure mana");
#endif
				if (!one_in_(6)) break;
			}
		case 32: case 33:
			if (!(*count))
		{
#ifdef JP
				msg_print("周囲の空間が歪んだ！");
#else
				msg_print("Space warps about you!");
#endif

				teleport_player(damroll(10, 10));
				if (randint0(13)) (*count) += activate_hi_summon(py, px);
				if (!one_in_(6)) break;
			}
		case 34:
#ifdef JP
			msg_print("エネルギーのうねりを感じた！");
#else
			msg_print("You feel a surge of energy!");
#endif

			wall_breaker();
			if (!randint0(7))
			{
				project(0, 7, py, px, 50, GF_KILL_WALL, flg, MODIFY_ELEM_MODE_NONE);
#ifdef JP
				take_hit(DAMAGE_NOESCAPE, 50, "エネルギーのうねり");
#else
				take_hit(DAMAGE_NOESCAPE, 50, "surge of energy");
#endif
			}
			if (!one_in_(6)) break;
		case 1: case 2: case 3: case 16: case 17:
			aggravate_monsters(0);
			if (!one_in_(6)) break;
		case 4: case 5: case 6:
			(*count) += activate_hi_summon(py, px);
			if (!one_in_(6)) break;
		case 7: case 8: case 9: case 18:
			(*count) += summon_specific(0, py, px, dun_level, 0, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
			if (!one_in_(6)) break;
		case 10: case 11: case 12:
			{
				cexp_info_type *cexp_ptr = &p_ptr->cexp_info[p_ptr->pclass];

#ifdef JP
				msg_print("生命力が体から吸い取られた気がする！");
#else
				msg_print("You feel your life draining away...");
#endif

				lose_class_exp(cexp_ptr->cexp / 16);
				lose_racial_exp(p_ptr->exp / 16);
			}
			if (!one_in_(6)) break;
		case 13: case 14: case 15: case 19: case 20:
			if (stop_ty || (p_ptr->free_act && (randint1(125) < p_ptr->skill_sav)))
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
					set_paralyzed(p_ptr->paralyzed + randint1(3));
				else
					set_paralyzed(p_ptr->paralyzed + randint1(13));
				stop_ty = TRUE;
			}
			if (!one_in_(6)) break;
		case 21: case 22: case 23:
			(void)do_dec_stat(randint0(A_MAX));
			if (!one_in_(6)) break;
		case 24:
#ifdef JP
			msg_print("ほえ？私は誰？ここで何してる？");
#else
			msg_print("Huh? Who am I? What am I doing here?");
#endif

			lose_all_info();
			if (!one_in_(6)) break;
		case 25:
			/*
			 * Only summon Cyberdemons deep in the dungeon.
			 */
			if ((dun_level > 65) && !stop_ty)
			{
				(*count) += summon_cyber(0, py, px);
				stop_ty = TRUE;
				break;
			}
			if (!one_in_(6)) break;
		default:
			while (i < A_MAX)
			{
				do
				{
					(void)do_dec_stat(i);
				}
				while (one_in_(2));

				i++;
			}
		}
	}
	while (one_in_(3) && !stop_ty);

	return stop_ty;
}


int activate_hi_summon(int y, int x)
{
	int i;
	int count = 0;
	int summon_lev;
	u32b mode = PM_ALLOW_GROUP | PM_NO_PET;

	summon_lev = dun_level;

	for (i = 0; i < (randint1(7) + (dun_level / 40)); i++)
	{
		switch (randint1(25) + (dun_level / 20))
		{
			case 1: case 2:
				count += summon_specific(0, y, x, summon_lev, SUMMON_ANT, mode);
				break;
			case 3: case 4:
				count += summon_specific(0, y, x, summon_lev, SUMMON_SPIDER, mode);
				break;
			case 5: case 6:
				count += summon_specific(0, y, x, summon_lev, SUMMON_HOUND, mode);
				break;
			case 7: case 8:
				count += summon_specific(0, y, x, summon_lev, SUMMON_BEAST, mode);
				break;
			case 9: case 10:
				count += summon_specific(0, y, x, summon_lev, SUMMON_ANGEL, mode);
				break;
			case 11: case 12:
				count += summon_specific(0, y, x, summon_lev, SUMMON_UNDEAD, mode);
				break;
			case 13: case 14:
				count += summon_specific(0, y, x, summon_lev, SUMMON_DRAGON, mode);
				break;
			case 15: case 16:
				count += summon_specific(0, y, x, summon_lev, SUMMON_DEMON, mode);
				break;
			case 17: case 18: case 19:
				count += summon_specific(0, y, x, summon_lev, SUMMON_UNIQUE, (mode | PM_ALLOW_UNIQUE));
				break;
			case 20: case 21:
				count += summon_specific(0, y, x, summon_lev, SUMMON_HI_UNDEAD, (mode | PM_ALLOW_UNIQUE));
				break;
			case 22: case 23:
				count += summon_specific(0, y, x, summon_lev, SUMMON_HI_DRAGON, (mode | PM_ALLOW_UNIQUE));
				break;
			case 24:
				count += summon_specific(0, y, x, 100, SUMMON_CYBER, mode);
				break;
			default:
				count += summon_specific(0, y, x, (((summon_lev * 3) / 2) + 5), 0, (mode | PM_ALLOW_UNIQUE));
		}
	}

	return count;
}


/* ToDo: check */
int summon_cyber(int who, int y, int x)
{
	int i;
	int max_cyber = (easy_band ? 1 : (dun_level / 50) + randint1(2));
	int count = 0;
	u32b mode = PM_ALLOW_GROUP;

	/* Summoned by a monster */
	if (who > 0)
	{
		monster_type *m_ptr = &m_list[who];
		if (is_pet(m_ptr)) mode |= PM_FORCE_PET;
	}

	if (max_cyber > 4) max_cyber = 4;

	for (i = 0; i < max_cyber; i++)
	{
		count += summon_specific(who, y, x, 100, SUMMON_CYBER, mode);
	}

	return count;
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
 * Charm males
 */
bool charm_males(int dam)
{
	int     i, x, y;
	u32b    flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;
	bool    obvious = FALSE;
	monster_type *m_ptr;
	monster_race *r_ptr;


	/* Affect all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Require line of sight */
		if (!player_has_los_bold(y, x)) continue;

		r_ptr = &r_info[m_ptr->r_idx];

		/* Must be heterosexual */
		if (!(r_ptr->flags1 & RF1_MALE)) continue;

		/* Jump directly to the target monster */
		if (project(0, 0, y, x, dam, GF_CHARM, flg, MODIFY_ELEM_MODE_MAGIC)) obvious = TRUE;
	}

	/* Result */
	return (obvious);
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
 * Turn everyone
 */
bool turn_monsters(int dam)
{
	return (project_hack(GF_TURN_ALL, dam));
}


bool charm_monster(int dir, int plev)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CHARM, dir, plev, flg));
}


bool control_one_undead(int dir, int plev)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CONTROL_UNDEAD, dir, plev, flg));
}


bool charm_animal(int dir, int plev)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CONTROL_ANIMAL, dir, plev, flg));
}


bool charm_beast(int dir, int plev)
{
	u32b flg = PROJECT_STOP | PROJECT_KILL;
	return (project_hook(GF_CONTROL_BEAST, dir, plev, flg));
}


bool fire_elem_ball(s16b elem, int dir, int rad, s16b amount)
{
	int tx, ty, typ;
	bool ret_val = FALSE;

	u32b flg = PROJECT_STOP | PROJECT_GRID;

	switch (elem)
	{
	case ELEM_FIRE:
		typ = GF_PURE_FIRE;
		break;

	case ELEM_AQUA:
		typ = GF_PURE_AQUA;
		break;

	case ELEM_EARTH:
		typ = GF_PURE_EARTH;
		break;

	case ELEM_WIND:
		typ = GF_PURE_WIND;
		break;

	default:
		return FALSE;
	}

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

	/* Analyze the "dir" and the "target".  Don't hurt items on floor. */
	hack_elem_amount = amount;
	ret_val = project(0, rad, ty, tx, 0, typ, flg, MODIFY_ELEM_MODE_NONE);
	hack_elem_amount = 0;

	return ret_val;
}


void range_restricted_target(int dir, int range, int *y, int *x, bool stop_before_wall)
{
	int ty, tx, nx, ny;
	int over_1 = 0, over_2 = 0;
	u32b flg = PROJECT_STOP;
	u16b path_g[128];
	int path_n, i;

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		flg &= ~(PROJECT_STOP);
		tx = target_col;
		ty = target_row;
	}
	else
	{
		/* Use the given direction */
		ty = py + 99 * ddy[dir];
		tx = px + 99 * ddx[dir];

		/* If not in bounds... */
		if (!in_bounds2(ty, tx))
		{
			if (ty < 0) over_1 = -ty;
			else if (ty >= cur_hgt) over_1 = ty - cur_hgt + 1;
			if (tx < 0) over_2 = -tx;
			else if (tx >= cur_wid) over_2 = tx - cur_wid + 1;

			if (over_1 < over_2) over_1 = over_2;

			ty += over_1 * (-ddy[dir]);
			tx += over_1 * (-ddx[dir]);
		}
	}

	path_n = project_path(path_g, range, py, px, ty, tx, flg);

	ty = py;
	tx = px;

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		ny = GRID_Y(path_g[i]);
		nx = GRID_X(path_g[i]);

		/* Hack -- Balls explode before reaching walls */
		if (!cave_floor_bold(ny, nx) && stop_before_wall) break;

		/* Advance */
		ty = ny;
		tx = nx;
	}

	*y = ty;
	*x = tx;
}


/*
 * Summon God
 */
bool summon_god(int typ, int dam)
{
	return (project_hack(typ, dam));
}


#define CALL_THE_ELEMENTAL_RAD         3
#define CALL_THE_ELEMENTAL_ARRAY_SIZE 37

/*
 * Call the elemental
 */
static bool call_the_elemental(int who, int y, int x, int dam_base, int dam_dice,
	int dam_side, int attacks, int typ)
{
	monster_type *m_ptr;

	bool player_damaged = FALSE;
	int  i, cur_rand;
	int  targets_array[CALL_THE_ELEMENTAL_ARRAY_SIZE];
	int  targets_num = 0;
	int  cur_target;
	int  ty, tx, tdam;
	bool target_dead;
	u32b flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM;

	/* Scan player (unless casted by player) */
	if (who)
	{
		if (los(y, x, py, px) && (distance(y, x, py, px) <= CALL_THE_ELEMENTAL_RAD))
			targets_array[targets_num++] = 0;
		if (p_ptr->use_decoy)
		{
			if (los(y, x, p_ptr->decoy_y, p_ptr->decoy_x) &&
			    (distance(y, x, p_ptr->decoy_y, p_ptr->decoy_x) <= CALL_THE_ELEMENTAL_RAD))
				targets_array[targets_num++] = -1;
		}
	}

	/* Scan target monsters */
	for (i = 1; i < m_max; i++)
	{
		m_ptr = &m_list[i];

		/* Caster is not affected */
		if (i == who) continue;

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Skip distant monsters */
		if (distance(y, x, m_ptr->fy, m_ptr->fx) > CALL_THE_ELEMENTAL_RAD) continue;

		/* Require line of sight */
		if (!los(y, x, m_ptr->fy, m_ptr->fx)) continue;

		/* Mark the monster */
		targets_array[targets_num++] = i;
	}

	if (!targets_num) return FALSE;

	for (i = 0; (i < attacks) && targets_num; i++)
	{
		cur_rand = randint0(targets_num);
		cur_target = targets_array[cur_rand];
		target_dead = FALSE;
		if (cur_target > 0)
		{
			if (!m_list[cur_target].r_idx) target_dead = TRUE; /* Monster is already dead */
			else
			{
				ty = m_list[cur_target].fy;
				tx = m_list[cur_target].fx;
			}
		}
		else if (!cur_target)
		{
			if (p_ptr->is_dead) target_dead = TRUE; /* Player is already dead */
			else
			{
				ty = py;
				tx = px;
				player_damaged = TRUE;
			}
		}
		else
		{
			if (!p_ptr->use_decoy) target_dead = TRUE; /* Player's decoy is already broken */
			else
			{
				ty = p_ptr->decoy_y;
				tx = p_ptr->decoy_x;
			}
		}

		/* If target is already dead, move the last target to here. */
		if (target_dead)
		{
			if (cur_rand < (targets_num - 1))
				targets_array[cur_rand] = targets_array[targets_num - 1];
			targets_num--;
			i--;
			continue;
		}

		tdam = dam_base + damroll(dam_dice, dam_side);

		/* Jump directly to the target monster/player */
		project(who, 0, ty, tx, tdam, typ, flg, MODIFY_ELEM_MODE_MAGIC);
		if (p_ptr->action == ACTION_ELEMSCOPE) lite_spot(ty, tx);
	}

	return player_damaged;
}

void cast_call_the_elemental(int typ, int dir, int dam_base, int dam_dice,
	int dam_side, int attacks_base, int use_stat)
{
	int tx, ty;
	int pstat = p_ptr->stat_use[use_stat];
	int attacks;

	/* Extra attacks boost (boost until attacks_base+5) */
	attacks = attacks_base;
	if (pstat >= (18 + 100)) attacks++;
	if (pstat >= (18 + 150)) attacks++;
	if (pstat >= (18 + 180)) attacks++;
	if (pstat >= (18 + 200)) attacks++;
	if (pstat >= (18 + 220)) attacks++;

	range_restricted_target(dir, MAX_RANGE, &ty, &tx, TRUE);
	(void)call_the_elemental(0, ty, tx, dam_base, dam_dice, dam_side, attacks, typ);
}

void get_mon_project_point(int who, int *y, int *x)
{
	monster_type *m_ptr = &m_list[who];
	int          nx, ny;
	u16b         path_g[128];
	int          path_n, i;

	path_n = project_path(path_g, MAX_RANGE, m_ptr->fy, m_ptr->fx, *y, *x, 0L);

	*y = m_ptr->fy;
	*x = m_ptr->fx;

	/* Project along the path */
	for (i = 0; i < path_n; ++i)
	{
		ny = GRID_Y(path_g[i]);
		nx = GRID_X(path_g[i]);

		/* Hack -- Balls explode before reaching walls */
		if (!cave_floor_bold(ny, nx)) break;

		/* Advance */
		*y = ny;
		*x = nx;
	}
}

bool mon_cast_call_the_elemental(int who, int y, int x, int dam_base, int dam_dice,
	int dam_side, int attacks_base, int typ)
{
	monster_type *m_ptr = &m_list[who];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	int          level = r_ptr->level ? r_ptr->level : 1;
	int          attacks;

	/* Extra attacks boost (boost until attacks_base+5) */
	if (level >= 40) attacks = attacks_base - 3 + level / 10;
	else attacks = attacks_base;
	if (attacks > (attacks_base + 5)) attacks = attacks_base + 5;

	get_mon_project_point(who, &y, &x);
	return call_the_elemental(who, y, x, dam_base, dam_dice, dam_side, attacks, typ);
}

/*
 * Apply a "project()" directly to all viewable living monsters
 */
bool project_hack_living(int typ, int dam)
{
	int     i, x, y;
	u32b    flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;
	bool    obvious = FALSE;
	monster_type *m_ptr;

	/* Affect all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Require line of sight */
		if (!player_has_los_bold(y, x)) continue;

		/* Living monsters only */
		if (!monster_living(&r_info[m_ptr->r_idx])) continue;

		/* Jump directly to the target monster */
		if (project(0, 0, y, x, dam, typ, flg, MODIFY_ELEM_MODE_MAGIC)) obvious = TRUE;
	}

	/* Result */
	return (obvious);
}

/*
 * Apply a "project()" directly to all viewable undead monsters
 */
bool project_hack_undead(int typ, int dam)
{
	int     i, x, y;
	u32b    flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;
	bool    obvious = FALSE;
	monster_type *m_ptr;

	/* Affect all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Require line of sight */
		if (!player_has_los_bold(y, x)) continue;

		/* Undead monsters only */
		if (!(r_info[m_ptr->r_idx].flags3 & RF3_UNDEAD)) continue;

		/* Jump directly to the target monster */
		if (project(0, 0, y, x, dam, typ, flg, MODIFY_ELEM_MODE_MAGIC)) obvious = TRUE;
	}

	/* Result */
	return (obvious);
}

/*
 * Stone gaze
 */
bool stone_gaze(int who)
{
	int     i, x, y, power;
	u32b    flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;
	int     gy, gx;
	monster_type *m_ptr;
	bool    player_resist_stone = FALSE;

	/* Get the gazer's position */
	if (who > 0)
	{
		gy = m_list[who].fy;
		gx = m_list[who].fx;
		power = r_info[m_list[who].r_idx].level;
	}
	else
	{
		gy = py;
		gx = px;
		power = p_ptr->lev * 2;
	}

	/* Affect player unless gazed by player */
	if (((who > 0) && (who != p_ptr->riding) &&
		 los(gy, gx, py, px) && los(py, px, gy, gx) &&
		 (distance(gy, gx, py, px) <= MAX_SIGHT)) || (who < 0))
	{
		if (p_ptr->resist_stone)
		{
#ifdef JP
			msg_print("しかし効果がなかった！");
#else
			msg_print("You are unaffected!");
#endif
			player_resist_stone = TRUE;
		}
		else if ((inventory[INVEN_RARM].k_idx && (inventory[INVEN_RARM].tval == TV_SHIELD)) ||
		         (inventory[INVEN_LARM].k_idx && (inventory[INVEN_LARM].tval == TV_SHIELD)))
		{
#ifdef JP
			msg_print("盾で邪眼を防いだ！");
#else
			msg_print("Your shield protects you from stone gaze!");
#endif
		}
		else if (p_ptr->blind)
		{
#ifdef JP
			msg_print("しかし目が見えないので効果がなかった！");
#else
			msg_print("You are unaffected because you are blind!");
#endif
		}
		else project(who, 0, py, px, power, GF_OLD_STONE, flg, MODIFY_ELEM_MODE_MAGIC);
	}

	/* Affect all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		m_ptr = &m_list[i];

		/* Gazer is not affected */
		if (i == who) continue;

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		if (i == p_ptr->riding) continue;

		/* Location */
		y = m_ptr->fy;
		x = m_ptr->fx;

		/* Require line of sight (dual) */
		if (!los(gy, gx, y, x)) continue;
		if (!los(y, x, gy, gx)) continue;

		/* Skip distant monsters */
		if (distance(gy, gx, y, x) > MAX_SIGHT) continue;

		/* Sleeping monsters are not affected */
		if (m_ptr->csleep)
		{
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Get the monster name (or "it") */
				monster_desc(m_name, m_ptr, 0);
#ifdef JP
				msg_format("%^sは眠っているので効果がなかった！", m_name);
#else
				msg_format("%^s is unaffected because sleeping!", m_name);
#endif
			}
			continue;
		}

		/* Confusing monsters are not affected */
		if (m_ptr->confused)
		{
			if (m_ptr->ml)
			{
				char m_name[80];

				/* Get the monster name (or "it") */
				monster_desc(m_name, m_ptr, 0);
#ifdef JP
				msg_format("%^sは混乱して視線が不安定なので効果がなかった！", m_name);
#else
				msg_format("%^s is unaffected because confusing!", m_name);
#endif
			}
			continue;
		}

		/* Jump directly to the target monster */
		project(who, 0, y, x, power, GF_OLD_STONE, flg, MODIFY_ELEM_MODE_MAGIC);
	}

	/* Result */
	return player_resist_stone;
}

bool inc_area_elem(int who, s16b elem, int amount, int rad, bool dec_opposite)
{
	u32b flg = PROJECT_GRID | PROJECT_HIDE;
	int i, x, y;
	cave_type *c_ptr;
	int cy, cx;
	bool only_room;

	/* Paranoia */
	if (elem == NO_ELEM) return FALSE;
	if (!amount) return FALSE;
	if (!rad) return FALSE;

	/* Get the caster's position */
	if (who > 0)
	{
		cy = m_list[who].fy;
		cx = m_list[who].fx;
	}
	else
	{
		cy = py;
		cx = px;
	}

	/* If rad < 0, only near grids */
	if (rad < 0)
	{
		rad = 0 - rad;
		only_room = TRUE;
	}
	else only_room = FALSE;

	/* Hack -- Message */
	if (!p_ptr->blind)
	{
		if (amount > 0)
#ifdef JP
			msg_format("%sのエレメントが活性化された。", elem_names[elem]);
#else
			msg_format("The %s elememt is enhanced.", elem_names[elem]);
#endif
		else
#ifdef JP
			msg_format("%sのエレメントが弱まった。", elem_names[elem]);
#else
			msg_format("The %s elememt is weakened.", elem_names[elem]);
#endif
	}

	/* Add the initial grid */
	cave_temp_room_aux(cy, cx, cy, cx, only_room);

	/* While grids are in the queue, add their neighbors */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		/* Walls get increased, but stop increasing */
		if (!cave_floor_bold(y, x)) continue;

		/* Spread adjacent */
		cave_temp_room_aux(cy, cx, y + 1, x, only_room);
		cave_temp_room_aux(cy, cx, y - 1, x, only_room);
		cave_temp_room_aux(cy, cx, y, x + 1, only_room);
		cave_temp_room_aux(cy, cx, y, x - 1, only_room);

		/* Spread diagonal */
		cave_temp_room_aux(cy, cx, y + 1, x + 1, only_room);
		cave_temp_room_aux(cy, cx, y - 1, x - 1, only_room);
		cave_temp_room_aux(cy, cx, y - 1, x + 1, only_room);
		cave_temp_room_aux(cy, cx, y + 1, x - 1, only_room);
	}

	/* Hook into the "project()" function */
	(void)project(who, rad, cy, cx, 0, GF_CAVE_TEMP, flg, MODIFY_ELEM_MODE_NONE);

	/* Clear them all */
	for (i = 0; i < temp_n; i++)
	{
		y = temp_y[i];
		x = temp_x[i];

		c_ptr = &cave[y][x];

		/* No longer in the array */
		c_ptr->info &= ~(CAVE_TEMP);

		/* Increase the element */
		change_grid_elem(c_ptr, elem, amount);
	}

	/* None left */
	temp_n = 0;

	if (dec_opposite) inc_area_elem(who, get_opposite_elem(elem), -amount, rad, FALSE);

	if (p_ptr->action == ACTION_ELEMSCOPE) p_ptr->redraw |= (PR_MAP);

	/* Assume seen */
	return (TRUE);
}

bool mermaid_water_flow(void)
{
	u32b flg = PROJECT_GRID | PROJECT_HIDE;
	int i, x, y;
	cave_type *c_ptr;
	bool boundary;
	byte feat;
	int done = FALSE;

	/* Add the initial grid */
	cave_temp_room_aux(py, px, py, px, FALSE);

	/* While grids are in the queue, add their neighbors */
	for (i = 0; i < temp_n; i++)
	{
		x = temp_x[i], y = temp_y[i];

		/* Walls get increased, but stop increasing */
		if (!cave_floor_bold(y, x)) continue;

		/* Spread adjacent */
		cave_temp_room_aux(py, px, y + 1, x, FALSE);
		cave_temp_room_aux(py, px, y - 1, x, FALSE);
		cave_temp_room_aux(py, px, y, x + 1, FALSE);
		cave_temp_room_aux(py, px, y, x - 1, FALSE);

		/* Spread diagonal */
		cave_temp_room_aux(py, px, y + 1, x + 1, FALSE);
		cave_temp_room_aux(py, px, y - 1, x - 1, FALSE);
		cave_temp_room_aux(py, px, y - 1, x + 1, FALSE);
		cave_temp_room_aux(py, px, y + 1, x - 1, FALSE);
	}

	/* Hook into the "project()" function */
	(void)project(0, 2, py, px, 0, GF_CAVE_TEMP, flg, MODIFY_ELEM_MODE_NONE);

	/* Clear them all */
	for (i = 0; i < temp_n; i++)
	{
		y = temp_y[i];
		x = temp_x[i];

		c_ptr = &cave[y][x];

		/* No longer in the array */
		c_ptr->info &= ~(CAVE_TEMP);

		boundary = boundary_floor_bold(y, x);
		feat = boundary ? c_ptr->mimic : c_ptr->feat;

		switch (feat)
		{
		case FEAT_FLOOR:
		case FEAT_DARK_PIT:
		case FEAT_DIRT:
		case FEAT_GRASS:
		case FEAT_FLOWER:
		case FEAT_DEEP_GRASS:
		case FEAT_SWAMP:
		case FEAT_TUNDRA:
			if (boundary)
			{
				byte prev_feat = c_ptr->mimic;

				c_ptr->mimic = FEAT_SHAL_WATER;

				/* Notice */
				note_spot(y, x);

				/* Redraw */
				lite_spot(y, x);

				apply_grid_effect(y, x, prev_feat, TRUE);
			}
			else cave_set_feat(y, x, FEAT_SHAL_WATER);
			if (c_ptr->m_idx)
			{
				monster_type *m_ptr = &m_list[c_ptr->m_idx];
				monster_race *r_ptr = &r_info[m_ptr->r_idx];
				if (!(r_ptr->flags7 & RF7_CAN_FLY) && m_ptr->csleep)
				{
					m_ptr->csleep = 0;
					if (m_ptr->ml && !p_ptr->blind)
					{
						char m_name[80];

						monster_desc(m_name, m_ptr, 0);
#ifdef JP
						msg_format("%^sが目を覚ました。", m_name);
#else
						msg_format("%^s wakes up.", m_name);
#endif
					}
				}
			}
			done = TRUE;
			break;
		}
	}

	if (done)
	{
		msg_print("周囲が水浸しになった。");
		set_mermaid_in_water();
	}
	else msg_print("何も起きなかった。");

	/* None left */
	temp_n = 0;

	/* Assume seen */
	return (TRUE);
}

/*
 * Song of Silence
 * If dam == 0, monsters' silence is stopped.
 */
void song_of_silence(int dam)
{
	int     i;
	monster_type *m_ptr;
	monster_race *r_ptr;
	bool          old_silent_song;
	bool          seen;
	char          m_name[80];

	/* Affect all (nearby) monsters */
	for (i = 1; i < m_max; i++)
	{
		m_ptr = &m_list[i];

		/* Paranoia -- Skip dead monsters */
		if (!m_ptr->r_idx) continue;

		seen = m_ptr->ml;
		r_ptr = &r_info[m_ptr->r_idx];
		old_silent_song = m_ptr->silent_song;
		monster_desc(m_name, m_ptr, 0);

		if (!dam || !player_has_los_bold(m_ptr->fy, m_ptr->fx))
			m_ptr->silent_song = FALSE;
		else if (!m_ptr->silent_song)
		{
			if ((r_ptr->flags3 & RF3_NO_STUN) ||
				((r_ptr->flags1 & RF1_UNIQUE) && !one_in_(6)) ||
				(r_ptr->level > randint1((dam - 10) < 1 ? 1 : (dam - 10)) + 10))
			{
				if (seen)
				{
#ifdef JP
					msg_format("%^sには効果がなかった！", m_name);
#else
					msg_format("%^s is unaffected!", m_name);
#endif
					/* Memorize a flag */
					if (r_ptr->flags3 & RF3_NO_STUN) r_ptr->r_flags3 |= (RF3_NO_STUN);
				}
			}
			else
			{
				m_ptr->silent_song = TRUE;
			}
			m_ptr->csleep = 0;
		}

		if (seen)
		{
			if ((m_ptr->silent_song != old_silent_song) && !m_ptr->silent)
			{
				if (m_ptr->silent_song)
				{
#ifdef JP
					msg_format("%^sは沈黙した！", m_name);
#else
					msg_format("%^s is quiet!", m_name);
#endif
				}
				else
				{
#ifdef JP
					msg_format("%^sの沈黙が解けた。", m_name);
#else
					msg_format("Silence of %^s is expired.", m_name);
#endif
				}
			}
		}

		/* Redraw (later) if needed */
		if (p_ptr->health_who == i) p_ptr->redraw |= (PR_HEALTH);
		if (p_ptr->riding == i) p_ptr->redraw |= (PR_UHEALTH);

		if (p_ptr->use_decoy) break_decoy();
	}
}

/*
 * Song of Temptation
 */
void song_of_temptation(void)
{
	int  i, x, y;
	u32b flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;
	int  drain_dam = damroll(2, p_ptr->stat_use[A_CHR] / 4);
	int  charm_power = p_ptr->stat_use[A_CHR] / 2;

	/* Affect all (nearby) monsters */
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

		/* Jump directly to the target monster */
		(void)project(0, 0, y, x, drain_dam, GF_DUAL_DRAIN, flg, MODIFY_ELEM_MODE_MAGIC);
		if (m_ptr->r_idx) (void)project(0, 0, y, x, charm_power, GF_CHARM, flg, MODIFY_ELEM_MODE_MAGIC);
	}
}

/*
 * Knock back
 */
void knock_back(int who, int y, int x, int base_dam)
{
	int ty, tx, cy, cx, ny, nx;
	int mover_y, mover_x, dy, dx, d_abs;
	int target_m_idx = 0;
	cave_type    *c_ptr;
	monster_type *m_ptr = NULL;
	monster_race *r_ptr = NULL;
	int dam1;
	bool fear = FALSE;
	bool do_monster;
	bool do_player;
	bool boundary_floor;
	char m_name[80];

	/* Attacker's name (prepared before polymorph)*/
	char killer[80];

	/* Not in bounds */
	if (!in_bounds(y, x)) return;

	c_ptr = &cave[y][x];

	if (p_ptr->use_decoy)
	{
		if ((y == p_ptr->decoy_y) && (x == p_ptr->decoy_x))
		{
			if (who) break_decoy();
			return;
		}
	}

	/* No target there */
	if (((y != py) || (x != px)) && !c_ptr->m_idx) return;

	if (who > 0)
	{
		monster_desc(killer, &m_list[who], 0x288);
		mover_y = m_list[who].fy;
		mover_x = m_list[who].fx;
	}
	else
	{
		mover_y = py;
		mover_x = px;
	}

	dy = y - mover_y;
	dx = x - mover_x;
	d_abs = (ABS(dy) > ABS(dx)) ? ABS(dy) : ABS(dx);
	dy /= d_abs;
	dx /= d_abs;

	/* No need to move */
	if (!dy && !dx) return;

	if (c_ptr->m_idx)
	{
		if (who == c_ptr->m_idx) return;
		m_ptr = &m_list[c_ptr->m_idx];
		r_ptr = &r_info[m_ptr->r_idx];
		monster_desc(m_name, m_ptr, 0);
		m_ptr->csleep = 0;
		target_m_idx = c_ptr->m_idx;
		do_monster = TRUE;

		/* Redraw (later) if needed */
		if (p_ptr->health_who == c_ptr->m_idx) p_ptr->redraw |= (PR_HEALTH);
		if (p_ptr->riding == c_ptr->m_idx) p_ptr->redraw |= (PR_UHEALTH);
	}
	else do_monster = FALSE;

	if ((y == py) && (x == px))
	{
		if (!who) return;
		do_player = TRUE;
	}
	else do_player = FALSE;

	ty = y + dy;
	tx = x + dx;
	cy = y;
	cx = x;

	while (1)
	{
		/* Stop at the target */
		if ((cy == ty) && (cx == tx)) return;

		/* Not in bounds (Paranoia) */
		if (!in_bounds(cy, cx)) return;

		ny = cy;
		nx = cx;
		mmove2(&ny, &nx, y, x, ty, tx);

		c_ptr = &cave[ny][nx];
		boundary_floor = boundary_floor_grid(c_ptr);

		/* Stopped by monsters */
		if (c_ptr->m_idx)
		{
			monster_type *n_ptr = &m_list[c_ptr->m_idx];
			monster_race *s_ptr = &r_info[n_ptr->r_idx];
			char n_name[80];
			int dam2 = MAX(base_dam / ((c_ptr->m_idx == p_ptr->riding) ? 4 : 2), 1);
			int dam3;

			dam1 = MAX(base_dam / 2, 1);

			/* Describe the monster */
			monster_desc(n_name, n_ptr, 0);

			/* Sound */
			sound(SOUND_HITWALL);

			if (do_player) msg_format("%^sに衝突した！", n_name);
			else msg_format("%^sが%^sに衝突した！", m_name, n_name);

			/* Redraw (later) if needed */
			if (p_ptr->health_who == c_ptr->m_idx) p_ptr->redraw |= (PR_HEALTH);
			if (p_ptr->riding == c_ptr->m_idx) p_ptr->redraw |= (PR_UHEALTH);

			/* Monster is certainly awake */
			n_ptr->csleep = 0;

			dam3 = randint1(dam2);

			/* "Unique" and "quest" monsters can only be "killed" by the player. */
			if ((s_ptr->flags1 & (RF1_UNIQUE | RF1_QUESTOR)) || (s_ptr->flags7 & (RF7_NAZGUL | RF7_UNIQUE2)))
			{
				if (dam3 > n_ptr->hp) dam3 = n_ptr->hp;
			}

			/* Apply damage directly */
			n_ptr->hp -= dam3;
			if (show_damage && n_ptr->ml && (dam3 > 0))
#ifdef JP
				msg_format("%^sに%dのダメージ。", n_name, dam3);
#else
				msg_format("%^s takes %d damages.", n_name, dam3);
#endif

			if (c_ptr->m_idx == p_ptr->riding)
#ifdef JP
				take_hit(DAMAGE_NOESCAPE, randint1(dam2), killer);
#else
				take_hit(DAMAGE_NOESCAPE, randint1(dam2), killer);
#endif

			/* Delete dead monster */
			if (n_ptr->hp < 0)
			{
				/* Make a sound */
				if (r_ptr->flags1 & RF1_MALE)
				{
					sound(SOUND_M_KILL);
				}
				else if (r_ptr->flags1 & RF1_MALE)
				{
					sound(SOUND_F_KILL);
				}
				else if (monster_living(r_ptr))
				{
					sound(SOUND_KILL);
				}
				else
				{
					sound(SOUND_N_KILL);
				}

				/* Message */
				if (n_ptr->ml) msg_format("%^s%s", n_name, extract_note_dies(s_ptr));

				/* Generate treasure, etc */
				monster_death(c_ptr->m_idx, FALSE, FALSE);

				/* Delete the monster */
				delete_monster_idx(c_ptr->m_idx);
			}
			else if (!who)
			{
				/* Anger monster */
				anger_monster(n_ptr);
			}
			break;
		}

		/* Stopped by player */
		if ((ny == py) && (nx == px))
		{
			/* Sound */
			sound(SOUND_HITWALL);

			msg_format("%^sがあなたに衝突した！", m_name);
			dam1 = MAX(base_dam / 2, 1);
			take_hit(DAMAGE_NOESCAPE, randint1(dam1), killer);
			break;
		}

		/* Stopped by walls/doors */
		if (!cave_floor_bold(ny, nx) && !boundary_floor)
		{
			/* Sound */
			sound(SOUND_HITWALL);

			if (do_player) msg_print("壁に叩き付けられた！");
			else msg_format("%^sは壁に叩き付けられた！", m_name);
			dam1 = randint1(base_dam);
			break;
		}

		/* Handle "Boundary Air" */
		if (boundary_floor && (c_ptr->mimic == FEAT_AIR))
		{
			if (do_monster)
			{
				if (mon_fall_into_air(target_m_idx)) return;
			}
			if (do_player)
			{
				if (!p_ptr->ffall)
				{
					fall_into_air();
					return;
				}
			}
		}

		/* Boundary floor mimic */
		if (boundary_floor) return;

		if (do_monster)
		{
			/* Move the monster */
			m_ptr->fy = ny;
			m_ptr->fx = nx;

			/* Update the new location */
			c_ptr->m_idx = target_m_idx;

			/* Update the old location */
			cave[cy][cx].m_idx = 0;

			/* Update the monster */
			update_mon(target_m_idx, TRUE);

			/* Redraw the old grid */
			lite_spot(cy, cx);

			/* Redraw the new grid */
			lite_spot(ny, nx);

			if (c_ptr->feat == FEAT_AIR)
			{
				if (mon_fall_into_air(target_m_idx)) return;
			}

			if (p_ptr->use_decoy)
			{
				if ((ny == p_ptr->decoy_y) && (nx == p_ptr->decoy_x)) break_decoy();
			}
		}

		if (do_player)
		{
			/* Move the player */
			py = ny;
			px = nx;

			/* Redraw the old spot */
			lite_spot(cy, cx);

			/* Redraw the new spot */
			lite_spot(py, px);

			set_mermaid_in_water();

			/* Check for new panel (redraw map) */
			verify_panel();
		}

		/* Update stuff */
		p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MON_LITE | PU_DISTANCE | PU_MONSTERS | PU_BONUS);

		/* Window stuff */
		p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

		/* Handle stuff XXX XXX XXX */
		handle_stuff();

		/* Save the new location */
		cx = nx;
		cy = ny;

		if (p_ptr->leaving) return;
	}

	if (do_monster && do_player) dam1 = MAX(dam1 / 2, 1);

	/* Damage monster */
	if (do_monster)
	{
		if (who)
		{
			mon_take_hit_mon(FALSE, target_m_idx, randint1(dam1), &fear, extract_note_dies(r_ptr), who);
		}
		else
		{
			if (!mon_take_hit(target_m_idx, randint1(dam1), &fear, extract_note_dies(r_ptr), FALSE))
			{
				/* Anger monster */
				anger_monster(m_ptr);
			}
		}

		/* Take note */
		if (fear && m_ptr->ml)
		{
			/* Sound */
			sound(SOUND_FLEE);

			/* Message */
#ifdef JP
			msg_format("%^sは恐怖して逃げ出した！", m_name);
#else
			msg_format("%^s flees in terror!", m_name);
#endif
		}
	}

	/* Damage player */
	if (do_player)
	{
		take_hit(DAMAGE_NOESCAPE, randint1(dam1), killer);
	}
}
