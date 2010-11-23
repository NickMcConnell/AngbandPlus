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
