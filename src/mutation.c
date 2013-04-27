/* File: mutation.c */

/* Purpose: Mutation effects (and racial powers) */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


bool gain_random_mutation(int choose_mut)
{
	int     attempts_left = 20;
	cptr    muta_desc = "";
	bool    muta_chosen = FALSE;
	u32b    muta_which = 0;
	u32b    *muta_class = NULL;

	if (choose_mut) attempts_left = 1;

	while (attempts_left--)
	{
		switch (choose_mut ? choose_mut : randint1(65))
		{
		case 1: case 2:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_BERS_RAGE;
#ifdef JP
			muta_desc = "あなたは狂暴化の発作を起こすようになった！";
#else
			muta_desc = "You become subject to fits of berserk rage!";
#endif
			break;
		case 3: case 4:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_COWARDICE;
#ifdef JP
			muta_desc = "信じられないくらい臆病になった！";
#else
			muta_desc = "You become an incredible coward!";
#endif
			break;
		case 5: case 6:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_HALLU;
#ifdef JP
			muta_desc = "あなたは幻覚を引き起こす精神錯乱に侵されている。";
#else
			muta_desc = "You are afflicted by a hallucinatory insanity!";
#endif
			break;
		case 7:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_NORMALITY;
#ifdef JP
			muta_desc = "あなたは奇妙なほど普通になった気がする。";
#else
			muta_desc = "You feel strangely normal.";
#endif
			break;
		case 8: case 9: case 10:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_WASTING;
#ifdef JP
			muta_desc = "あなたは突然おぞましい衰弱病にかかった。";
#else
			muta_desc = "You suddenly contract a horrible wasting disease.";
#endif
			break;
		case 11:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_RES_TIME;
#ifdef JP
			muta_desc = "不死になった気分がする。";
#else
			muta_desc = "You feel immortal.";
#endif
			break;
		case 12: case 13:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_WARNING;
#ifdef JP
			muta_desc = "あなたは突然パラノイアになった気がする。";
#else
			muta_desc = "You suddenly feel paranoid.";
#endif
			break;
		case 14: case 15: case 16:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_HYPER_STR;
#ifdef JP
			muta_desc = "超人的に強くなった！";
#else
			muta_desc = "You turn into a superhuman he-man!";
#endif
			break;
		case 17: case 18: case 19:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_PUNY;
#ifdef JP
			muta_desc = "筋肉が弱ってしまった...";
#else
			muta_desc = "Your muscles wither away...";
#endif
			break;
		case 20: case 21:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_HYPER_INT;
#ifdef JP
			muta_desc = "あなたの脳は生体コンピュータに進化した！";
#else
			muta_desc = "Your brain evolves into a living computer!";
#endif
			break;
		case 22: case 23:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_MORONIC;
#ifdef JP
			muta_desc = "脳が萎縮してしまった...";
#else
			muta_desc = "Your brain withers away...";
#endif
			break;
		case 24: case 25:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_RESILIENT;
#ifdef JP
			muta_desc = "並外れてタフになった。";
#else
			muta_desc = "You become extraordinarily resilient.";
#endif
			break;
		case 26: case 27: case 28:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_XTRA_FAT;
#ifdef JP
			muta_desc = "あなたは気持ち悪いくらい太った！";
#else
			muta_desc = "You become sickeningly fat!";
#endif
			break;
		case 29: case 30: case 31:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_ALBINO;
#ifdef JP
			muta_desc = "アルビノになった！弱くなった気がする...";
#else
			muta_desc = "You turn into an albino! You feel frail...";
#endif
			break;
		case 32: case 33: case 34:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_FLESH_ROT;
#ifdef JP
			muta_desc = "あなたの肉体は腐敗する病気に侵された！";
#else
			muta_desc = "Your flesh is afflicted by a rotting disease!";
#endif
			break;
		case 35: case 36:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_SILLY_VOI;
#ifdef JP
			muta_desc = "声が間抜けなキーキー声になった！";
#else
			muta_desc = "Your voice turns into a ridiculous squeak!";
#endif
			break;
		case 37: case 38:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_ILL_NORM;
#ifdef JP
			muta_desc = "心の安らぐ幻影を映し出すようになった。";
#else
			muta_desc = "You start projecting a reassuring image.";
#endif
			break;
		case 39: case 40: case 41:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_XTRA_EYES;
#ifdef JP
			muta_desc = "新たに二つの目が出来た！";
#else
			muta_desc = "You grow an extra pair of eyes!";
#endif
			break;
		case 42: case 43:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_XTRA_LEGS;
#ifdef JP
			muta_desc = "新たに二本の足が生えてきた！";
#else
			muta_desc = "You grow an extra pair of legs!";
#endif
			break;
		case 44: case 45: case 46:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_SHORT_LEG;
#ifdef JP
			muta_desc = "足が短い突起になってしまった！";
#else
			muta_desc = "Your legs turn into short stubs!";
#endif
			break;
		case 47:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_ELEC_TOUC;
#ifdef JP
			muta_desc = "血管を電流が流れ始めた！";
#else
			muta_desc = "Electricity starts running through you!";
#endif
			break;
		case 48:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_FIRE_BODY;
#ifdef JP
			muta_desc = "あなたの体は炎につつまれている。";
#else
			muta_desc = "Your body is enveloped in flames!";
#endif
			break;
		case 49: case 50: case 51:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_SCALES;
#ifdef JP
			muta_desc = "肌が黒い鱗に変わった！";
#else
			muta_desc = "Your skin turns into black scales!";
#endif
			break;
		case 52:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_IRON_SKIN;
#ifdef JP
			muta_desc = "あなたの肌は鉄になった！";
#else
			muta_desc = "Your skin turns to steel!";
#endif
			break;
		case 53: case 54:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_WINGS;
#ifdef JP
			muta_desc = "背中に羽が生えた。";
#else
			muta_desc = "You grow a pair of wings.";
#endif
			break;
		case 55:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_FEARLESS;
#ifdef JP
			muta_desc = "完全に怖れ知らずになった。";
#else
			muta_desc = "You become completely fearless.";
#endif
			break;
		case 56:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_REGEN;
#ifdef JP
			muta_desc = "急速に回復し始めた。";
#else
			muta_desc = "You start regenerating.";
#endif
			break;
		case 57:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_ESP;
#ifdef JP
			muta_desc = "テレパシーの能力を得た！";
#else
			muta_desc = "You develop a telepathic ability!";
#endif
			break;
		case 58: case 59:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_LIMBER;
#ifdef JP
			muta_desc = "筋肉がしなやかになった。";
#else
			muta_desc = "Your muscles become limber.";
#endif
			break;
		case 60: case 61: case 62:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_ARTHRITIS;
#ifdef JP
			muta_desc = "関節が突然痛み出した。";
#else
			muta_desc = "Your joints suddenly hurt.";
#endif
			break;
		case 63:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_VULN_ELEM;
#ifdef JP
			muta_desc = "妙に無防備になった気がする。";
#else
			muta_desc = "You feel strangely exposed.";
#endif
			break;
		case 64: case 65:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_MOTION;
#ifdef JP
			muta_desc = "体の動作がより正確になった。";
#else
			muta_desc = "You move with new assurance.";
#endif
			break;
		default:
			muta_class = NULL;
			muta_which = 0;
		}

		if (muta_class && muta_which)
		{
			if (!(*muta_class & muta_which))
			{
				muta_chosen = TRUE;
			}
		}
		if (muta_chosen) break;
	}

	if (!muta_chosen)
	{
#ifdef JP
		msg_print("普通になった気がする。");
#else
		msg_print("You feel normal.");
#endif
		return FALSE;
	}
	else
	{
#ifdef JP
		msg_print("突然変異した！");
#else
		msg_print("You mutate!");
#endif
		msg_print(muta_desc);
		*muta_class |= muta_which;

		if (muta_class == &(p_ptr->muta))
		{
			if (muta_which == MUT_PUNY)
			{
				if (p_ptr->muta & MUT_HYPER_STR)
				{
#ifdef JP
					msg_print("あなたはもう超人的に強くはない！");
#else
					msg_print("You no longer feel super-strong!");
#endif
					p_ptr->muta &= ~(MUT_HYPER_STR);
				}
			}
			else if (muta_which == MUT_HYPER_STR)
			{
				if (p_ptr->muta & MUT_PUNY)
				{
#ifdef JP
					msg_print("あなたはもう虚弱ではない！");
#else
					msg_print("You no longer feel puny!");
#endif
					p_ptr->muta &= ~(MUT_PUNY);
				}
			}
			else if (muta_which == MUT_MORONIC)
			{
				if (p_ptr->muta & MUT_HYPER_INT)
				{
#ifdef JP
					msg_print("あなたの脳はもう生体コンピュータではない。");
#else
					msg_print("Your brain is no longer a living computer.");
#endif
					p_ptr->muta &= ~(MUT_HYPER_INT);
				}
			}
			else if (muta_which == MUT_HYPER_INT)
			{
				if (p_ptr->muta & MUT_MORONIC)
				{
#ifdef JP
					msg_print("あなたはもう精神薄弱ではない。");
#else
					msg_print("You are no longer moronic.");
#endif
					p_ptr->muta &= ~(MUT_MORONIC);
				}
			}
			else if (muta_which == MUT_IRON_SKIN)
			{
				if (p_ptr->muta & MUT_SCALES)
				{
#ifdef JP
					msg_print("鱗がなくなった。");
#else
					msg_print("You lose your scales.");
#endif
					p_ptr->muta &= ~(MUT_SCALES);
				}
				if (p_ptr->muta & MUT_FLESH_ROT)
				{
#ifdef JP
					msg_print("肉体が腐乱しなくなった。");
#else
					msg_print("Your flesh rots no longer.");
#endif
					p_ptr->muta &= ~(MUT_FLESH_ROT);
				}
			}
			else if ((muta_which == MUT_SCALES) ||
				 (muta_which == MUT_FLESH_ROT))
			{
				if (p_ptr->muta & MUT_IRON_SKIN)
				{
#ifdef JP
					msg_print("あなたの肌はもう鉄ではない。");
#else
					msg_print("Your skin is no longer made of steel.");
#endif
					p_ptr->muta &= ~(MUT_IRON_SKIN);
				}
			}
			else if (muta_which == MUT_FEARLESS)
			{
				if (p_ptr->muta & MUT_COWARDICE)
				{
#ifdef JP
					msg_print("臆病でなくなった。");
#else
					msg_print("You are no longer cowardly.");
#endif
					p_ptr->muta &= ~(MUT_COWARDICE);
				}
			}
			else if (muta_which == MUT_FLESH_ROT)
			{
				if (p_ptr->muta & MUT_REGEN)
				{
#ifdef JP
					msg_print("急速に回復しなくなった。");
#else
					msg_print("You stop regenerating.");
#endif
					p_ptr->muta &= ~(MUT_REGEN);
				}
			}
			else if (muta_which == MUT_REGEN)
			{
				if (p_ptr->muta & MUT_FLESH_ROT)
				{
#ifdef JP
					msg_print("肉体が腐乱しなくなった。");
#else
					msg_print("Your flesh stops rotting.");
#endif
					p_ptr->muta &= ~(MUT_FLESH_ROT);
				}
			}
			else if (muta_which == MUT_LIMBER)
			{
				if (p_ptr->muta & MUT_ARTHRITIS)
				{
#ifdef JP
					msg_print("関節が痛くなくなった。");
#else
					msg_print("Your joints stop hurting.");
#endif
					p_ptr->muta &= ~(MUT_ARTHRITIS);
				}
			}
			else if (muta_which == MUT_ARTHRITIS)
			{
				if (p_ptr->muta & MUT_LIMBER)
				{
#ifdef JP
					msg_print("あなたはしなやかでなくなった。");
#else
					msg_print("You no longer feel limber.");
#endif
					p_ptr->muta &= ~(MUT_LIMBER);
				}
			}
			else if (muta_which == MUT_COWARDICE)
			{
				if (p_ptr->muta & MUT_FEARLESS)
				{
#ifdef JP
					msg_print("恐れ知らずでなくなった。");
#else
					msg_print("You no longer feel fearless.");
#endif
					p_ptr->muta &= ~(MUT_FEARLESS);
				}
			}
		}

		mutant_regenerate_mod = calc_mutant_regenerate_mod();
		p_ptr->update |= PU_BONUS;
		handle_stuff();
		return TRUE;
	}
}


bool lose_mutation(int choose_mut)
{
	int attempts_left = 20;
	cptr muta_desc = "";
	bool muta_chosen = FALSE;
	u32b muta_which = 0;
	u32b *muta_class = NULL;

	if (choose_mut) attempts_left = 1;

	while (attempts_left--)
	{
		switch (choose_mut ? choose_mut : randint1(75))
		{
		case 1: case 2:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_BERS_RAGE;
#ifdef JP
			muta_desc = "凶暴化の発作にさらされなくなった！";
#else
			muta_desc = "You are no longer subject to fits of berserk rage!";
#endif
			break;
		case 3: case 4:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_COWARDICE;
#ifdef JP
			muta_desc = "もう信じがたいほど臆病ではなくなった！";
#else
			muta_desc = "You are no longer an incredible coward!";
#endif
			break;
		case 5: case 6:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_HALLU;
#ifdef JP
			muta_desc = "幻覚をひき起こす精神障害を起こさなくなった！";
#else
			muta_desc = "You are no longer afflicted by a hallucinatory insanity!";
#endif
			break;
		case 7:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_NORMALITY;
#ifdef JP
			muta_desc = "普通に奇妙な感じがする。";
#else
			muta_desc = "You feel normally strange.";
#endif
			break;
		case 8: case 9:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_WASTING;
#ifdef JP
			muta_desc = "おぞましい衰弱病が治った！";
#else
			muta_desc = "You are cured of the horrible wasting disease!";
#endif
			break;
		case 10: case 11: case 12:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_RES_TIME;
#ifdef JP
			muta_desc = "不死でなくなった気がする。";
#else
			muta_desc = "You feel all too mortal.";
#endif
			break;
		case 13: case 14:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_WARNING;
#ifdef JP
			muta_desc = "パラノイアでなくなった。";
#else
			muta_desc = "You no longer feel paranoid.";
#endif
			break;
		case 15: case 16: case 17:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_HYPER_STR;
#ifdef JP
			muta_desc = "筋肉が普通に戻った。";
#else
			muta_desc = "Your muscles revert to normal.";
#endif
			break;
		case 18: case 19:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_PUNY;
#ifdef JP
			muta_desc = "筋肉が普通に戻った。";
#else
			muta_desc = "Your muscles revert to normal.";
#endif
			break;
		case 20: case 21: case 22: case 23:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_HYPER_INT;
#ifdef JP
			muta_desc = "脳が普通に戻った。";
#else
			muta_desc = "Your brain reverts to normal.";
#endif
			break;
		case 24: case 25:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_MORONIC;
#ifdef JP
			muta_desc = "脳が普通に戻った。";
#else
			muta_desc = "Your brain reverts to normal.";
#endif
			break;
		case 26: case 27: case 28:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_RESILIENT;
#ifdef JP
			muta_desc = "普通の丈夫さに戻った。";
#else
			muta_desc = "You become ordinarily resilient again.";
#endif
			break;
		case 29:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_XTRA_FAT;
#ifdef JP
			muta_desc = "奇跡的なダイエットに成功した！";
#else
			muta_desc = "You benefit from a miracle diet!";
#endif
			break;
		case 30: case 31:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_ALBINO;
#ifdef JP
			muta_desc = "アルビノでなくなった！";
#else
			muta_desc = "You are no longer an albino!";
#endif
			break;
		case 32:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_FLESH_ROT;
#ifdef JP
			muta_desc = "肉体を腐敗させる病気が治った！";
#else
			muta_desc = "Your flesh is no longer afflicted by a rotting disease!";
#endif
			break;
		case 33:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_SILLY_VOI;
#ifdef JP
			muta_desc = "声質が普通に戻った。";
#else
			muta_desc = "Your voice returns to normal.";
#endif
			break;
		case 34: case 35: case 36:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_ILL_NORM;
#ifdef JP
			muta_desc = "心が安らぐ幻影を映し出さなくなった。";
#else
			muta_desc = "You stop projecting a reassuring image.";
#endif
			break;
		case 37: case 38:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_XTRA_EYES;
#ifdef JP
			muta_desc = "余分な目が消えてしまった！";
#else
			muta_desc = "Your extra eyes vanish!";
#endif
			break;
		case 39: case 40: case 41: case 42:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_XTRA_LEGS;
#ifdef JP
			muta_desc = "余分な脚が消えてしまった！";
#else
			muta_desc = "Your extra legs disappear!";
#endif
			break;
		case 43: case 44:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_SHORT_LEG;
#ifdef JP
			muta_desc = "脚の長さが普通に戻った。";
#else
			muta_desc = "Your legs lengthen to normal.";
#endif
			break;
		case 45: case 46: case 47:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_ELEC_TOUC;
#ifdef JP
			muta_desc = "体を電流が流れなくなった。";
#else
			muta_desc = "Electricity stops running through you.";
#endif
			break;
		case 48: case 49:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_FIRE_BODY;
#ifdef JP
			muta_desc = "体が炎に包まれなくなった。";
#else
			muta_desc = "Your body is no longer enveloped in flames.";
#endif
			break;
		case 50: case 51:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_SCALES;
#ifdef JP
			muta_desc = "鱗が消えた！";
#else
			muta_desc = "Your scales vanish!";
#endif
			break;
		case 52: case 53: case 54:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_IRON_SKIN;
#ifdef JP
			muta_desc = "肌が肉にもどった！";
#else
			muta_desc = "Your skin reverts to flesh!";
#endif
			break;
		case 55: case 56:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_WINGS;
#ifdef JP
			muta_desc = "背中の羽根が取れ落ちた。";
#else
			muta_desc = "Your wings fall off.";
#endif
			break;
		case 57: case 58: case 59:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_FEARLESS;
#ifdef JP
			muta_desc = "再び恐怖を感じるようになった。";
#else
			muta_desc = "You begin to feel fear again.";
#endif
			break;
		case 60: case 61: case 62:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_REGEN;
#ifdef JP
			muta_desc = "急速回復しなくなった。";
#else
			muta_desc = "You stop regenerating.";
#endif
			break;
		case 63: case 64: case 65: case 66:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_ESP;
#ifdef JP
			muta_desc = "テレパシーの能力を失った！";
#else
			muta_desc = "You lose your telepathic ability!";
#endif
			break;
		case 67: case 68: case 69:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_LIMBER;
#ifdef JP
			muta_desc = "筋肉が硬くなった。";
#else
			muta_desc = "Your muscles stiffen.";
#endif
			break;
		case 70: case 71:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_ARTHRITIS;
#ifdef JP
			muta_desc = "関節が痛くなくなった。";
#else
			muta_desc = "Your joints stop hurting.";
#endif
			break;
		case 72: case 73:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_VULN_ELEM;
#ifdef JP
			muta_desc = "無防備な感じはなくなった。";
#else
			muta_desc = "You feel less exposed.";
#endif
			break;
		case 74: case 75:
			muta_class = &(p_ptr->muta);
			muta_which = MUT_MOTION;
#ifdef JP
			muta_desc = "動作の正確さがなくなった。";
#else
			muta_desc = "You move with less assurance.";
#endif
			break;
		default:
			muta_class = NULL;
			muta_which = 0;
		}

		if (muta_class && muta_which)
		{
			if (*(muta_class) & muta_which)
			{
				muta_chosen = TRUE;
			}
		}
		if (muta_chosen) break;
	}

	if (!muta_chosen)
	{
		return FALSE;
	}
	else
	{
		msg_print(muta_desc);
		*(muta_class) &= ~(muta_which);

		p_ptr->update |= PU_BONUS;
		handle_stuff();
		mutant_regenerate_mod = calc_mutant_regenerate_mod();
		return TRUE;
	}
}


void dump_mutations(FILE *OutFile)
{
	if (!OutFile) return;

	if (p_ptr->muta)
	{
		if (p_ptr->muta & MUT_BERS_RAGE)
		{
#ifdef JP
			fprintf(OutFile, " あなたは肉体野獣化の発作を起こす。\n");
#else
			fprintf(OutFile, " You are subject to berserker fits.\n");
#endif
		}
		if (p_ptr->muta & MUT_COWARDICE)
		{
#ifdef JP
			fprintf(OutFile, " あなたは時々臆病になる。\n");
#else
			fprintf(OutFile, " You are subject to cowardice.\n");
#endif
		}
		if (p_ptr->muta & MUT_HALLU)
		{
#ifdef JP
			fprintf(OutFile, " あなたは幻覚を引き起こす精神錯乱に侵されている。\n");
#else
			fprintf(OutFile, " You have a hallucinatory insanity.\n");
#endif
		}
		if (p_ptr->muta & MUT_NORMALITY)
		{
#ifdef JP
			fprintf(OutFile, " あなたは変異していたが、回復してきている。\n");
#else
			fprintf(OutFile, " You may be mutated, but you're recovering.\n");
#endif
		}
		if (p_ptr->muta & MUT_WASTING)
		{
#ifdef JP
			fprintf(OutFile, " あなたは衰弱する恐ろしい病気にかかっている。\n");
#else
			fprintf(OutFile, " You have a horrible wasting disease.\n");
#endif
		}
		if (p_ptr->muta & MUT_WARNING)
		{
#ifdef JP
			fprintf(OutFile, " あなたは敵に関する警告を感じる。\n");
#else
			fprintf(OutFile, " You receive warnings about your foes.\n");
#endif
		}
		if (p_ptr->muta & MUT_HYPER_STR)
		{
#ifdef JP
			fprintf(OutFile, " あなたは超人的に強い。(腕力+4)\n");
#else
			fprintf(OutFile, " You are superhumanly strong (+4 STR).\n");
#endif
		}
		if (p_ptr->muta & MUT_PUNY)
		{
#ifdef JP
			fprintf(OutFile, " あなたは虚弱だ。(腕力-4)\n");
#else
			fprintf(OutFile, " You are puny (-4 STR).\n");
#endif
		}
		if (p_ptr->muta & MUT_HYPER_INT)
		{
#ifdef JP
			fprintf(OutFile, " あなたの脳は生体コンピュータだ。(知能＆賢さ+4)\n");
#else
			fprintf(OutFile, " Your brain is a living computer (+4 INT/WIS).\n");
#endif
		}
		if (p_ptr->muta & MUT_MORONIC)
		{
#ifdef JP
			fprintf(OutFile, " あなたは精神薄弱だ。(知能＆賢さ-4)\n");
#else
			fprintf(OutFile, " You are moronic (-4 INT/WIS).\n");
#endif
		}
		if (p_ptr->muta & MUT_RESILIENT)
		{
#ifdef JP
			fprintf(OutFile, " あなたの体は弾力性に富んでいる。(耐久+4)\n");
#else
			fprintf(OutFile, " You are very resilient (+4 CON).\n");
#endif
		}
		if (p_ptr->muta & MUT_XTRA_FAT)
		{
#ifdef JP
			fprintf(OutFile, " あなたは極端に太っている。(耐久+2,スピード-2)\n");
#else
			fprintf(OutFile, " You are extremely fat (+2 CON, -2 speed).\n");
#endif
		}
		if (p_ptr->muta & MUT_ALBINO)
		{
#ifdef JP
			fprintf(OutFile, " あなたはアルビノだ。(耐久-4)\n");
#else
			fprintf(OutFile, " You are albino (-4 CON).\n");
#endif
		}
		if (p_ptr->muta & MUT_FLESH_ROT)
		{
#ifdef JP
			fprintf(OutFile, " あなたの肉体は腐敗している。(耐久-2,魅力-1)\n");
#else
			fprintf(OutFile, " Your flesh is rotting (-2 CON, -1 CHR).\n");
#endif
		}
		if (p_ptr->muta & MUT_SILLY_VOI)
		{
#ifdef JP
			fprintf(OutFile, " あなたの声は間抜けなキーキー声だ。(魅力-4)\n");
#else
			fprintf(OutFile, " Your voice is a silly squeak (-4 CHR).\n");
#endif
		}
		if (p_ptr->muta & MUT_ILL_NORM)
		{
#ifdef JP
			fprintf(OutFile, " あなたは幻影に覆われている。\n");
#else
			fprintf(OutFile, " Your appearance is masked with illusion.\n");
#endif
		}
		if (p_ptr->muta & MUT_XTRA_EYES)
		{
#ifdef JP
			fprintf(OutFile, " あなたは余分に二つの目を持っている。(探索+15)\n");
#else
			fprintf(OutFile, " You have an extra pair of eyes (+15 search).\n");
#endif
		}
		if (p_ptr->muta & MUT_XTRA_LEGS)
		{
#ifdef JP
			fprintf(OutFile, " あなたは余分に二本の足が生えている。(加速+3)\n");
#else
			fprintf(OutFile, " You have an extra pair of legs (+3 speed).\n");
#endif
		}
		if (p_ptr->muta & MUT_SHORT_LEG)
		{
#ifdef JP
			fprintf(OutFile, " あなたの足は短い突起だ。(加速-3)\n");
#else
			fprintf(OutFile, " Your legs are short stubs (-3 speed).\n");
#endif
		}
		if (p_ptr->muta & MUT_ELEC_TOUC)
		{
#ifdef JP
			fprintf(OutFile, " あなたの血管には電流が流れている。\n");
#else
			fprintf(OutFile, " Electricity is running through your veins.\n");
#endif
		}
		if (p_ptr->muta & MUT_FIRE_BODY)
		{
#ifdef JP
			fprintf(OutFile, " あなたの体は炎につつまれている。\n");
#else
			fprintf(OutFile, " Your body is enveloped in flames.\n");
#endif
		}
		if (p_ptr->muta & MUT_SCALES)
		{
#ifdef JP
			fprintf(OutFile, " あなたの肌は鱗になっている。(魅力-1, AC+10)\n");
#else
			fprintf(OutFile, " Your skin has turned into scales (-1 CHR, +10 AC).\n");
#endif
		}
		if (p_ptr->muta & MUT_IRON_SKIN)
		{
#ifdef JP
			fprintf(OutFile, " あなたの肌は鉄でできている。(器用-1, AC+25)\n");
#else
			fprintf(OutFile, " Your skin is made of steel (-1 DEX, +25 AC).\n");
#endif
		}
		if (p_ptr->muta & MUT_WINGS)
		{
#ifdef JP
			fprintf(OutFile, " あなたは羽を持っている。\n");
#else
			fprintf(OutFile, " You have wings.\n");
#endif
		}
		if (p_ptr->muta & MUT_FEARLESS)
		{
#ifdef JP
			fprintf(OutFile, " あなたは全く恐怖を感じない。\n");
#else
			fprintf(OutFile, " You are completely fearless.\n");
#endif
		}
		if (p_ptr->muta & MUT_REGEN)
		{
#ifdef JP
			fprintf(OutFile, " あなたは急速に回復する。\n");
#else
			fprintf(OutFile, " You are regenerating.\n");
#endif
		}
		if (p_ptr->muta & MUT_ESP)
		{
#ifdef JP
			fprintf(OutFile, " あなたはテレパシーを持っている。\n");
#else
			fprintf(OutFile, " You are telepathic.\n");
#endif
		}
		if (p_ptr->muta & MUT_LIMBER)
		{
#ifdef JP
			fprintf(OutFile, " あなたの体は非常にしなやかだ。(器用+3)\n");
#else
			fprintf(OutFile, " Your body is very limber (+3 DEX).\n");
#endif
		}
		if (p_ptr->muta & MUT_ARTHRITIS)
		{
#ifdef JP
			fprintf(OutFile, " あなたはいつも関節に痛みを感じている。(器用-3)\n");
#else
			fprintf(OutFile, " Your joints ache constantly (-3 DEX).\n");
#endif
		}
		if (p_ptr->muta & MUT_RES_TIME)
		{
#ifdef JP
			fprintf(OutFile, " あなたは時間逆転攻撃から守られている。\n");
#else
			fprintf(OutFile, " You are protected from the ravages of time.\n");
#endif
		}
		if (p_ptr->muta & MUT_VULN_ELEM)
		{
#ifdef JP
			fprintf(OutFile, " あなたは元素の攻撃に弱い。\n");
#else
			fprintf(OutFile, " You are susceptible to damage from the elements.\n");
#endif
		}
		if (p_ptr->muta & MUT_MOTION)
		{
#ifdef JP
			fprintf(OutFile, " あなたの動作は正確で力強い。(隠密+1)\n");
#else
			fprintf(OutFile, " Your movements are precise and forceful (+1 STL).\n");
#endif
		}
	}
}


/*
 * List mutations we have...
 */
void do_cmd_knowledge_mutations(void)
{
	FILE *fff;
	char file_name[1024];

	/* Open a new file */
	fff = my_fopen_temp(file_name, 1024);

	/* Dump the mutations to file */
	if (fff) dump_mutations(fff);

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
#ifdef JP
show_file(file_name, "突然変異", 0, 0);
#else
	show_file(file_name, "Mutations", 0, 0);
#endif


	/* Remove the file */
	fd_kill(file_name);
}


static int count_bits(u32b x)
{
	int n = 0;

	if (x) do
	{
		n++;
	}
	while (0 != (x = x & (x - 1)));

	return (n);
}


static int count_mutations(void)
{
	return (count_bits(p_ptr->muta));
}


/*
 * Return the modifier to the regeneration rate
 * (in percent)
 */
int calc_mutant_regenerate_mod(void)
{
	int regen;
	int mod = 10;
	int count = count_mutations();

	/* No negative modifier */
	if (count <= 0) return 100;

	regen = 100 - count * mod;

	/* Max. 90% decrease in regeneration speed */
	if (regen < 10) regen = 10;

	return (regen);
}


bool mutation_power_aux(u32b power)
{
	energy_use = 0;
#ifdef JP
	msg_format("能力 %s は実装されていません。", power);
#else
	msg_format("Power %s not implemented. Oops.", power);
#endif
	return FALSE;
}
