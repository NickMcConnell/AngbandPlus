#include "angband.h"

void heroism_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Heroism", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, "Temporarily grants increased combat prowess and great bravery.");
		break;
	case SPELL_CAST:
		set_hero(randint1(25) + 25, FALSE);
		set_afraid(0, TRUE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_heroism(void) { return cast_spell(heroism_spell); }

void hide_in_mud_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Hide in Mud", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, "Gain the ability to pass into walls temporarily, as well as extra resistance to acid.");
		break;
	case SPELL_CAST:
		set_kabenuke(randint1(p_ptr->lev/2) + p_ptr->lev/2, FALSE);
		set_oppose_acid(p_ptr->lev, FALSE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void identify_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Identify", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, "Identify a single object.");
		break;
	case SPELL_CAST:
		var_set_bool(res, ident_spell(FALSE));
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_identify(void) { return cast_spell(identify_spell); }

void identify_fully_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Identify True", "真・鑑定"));
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_CAST:
		var_set_bool(res, identify_fully(FALSE));
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_identify_fully(void) { return cast_spell(identify_fully_spell); }

void healing_I_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Healing I", "体力回復"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Powerful healing magic:  heals hitpoints, cuts and stun.", "極めて強力な回復呪文で、負傷と朦朧状態も全快する。"));
		break;
	case SPELL_INFO:
		var_set_string(res, format("Heals %d", spell_power(300)));
		break;
	case SPELL_CAST:
		hp_player(spell_power(300));
		set_stun(0, TRUE);
		set_cut(0, TRUE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void healing_II_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Healing II", "体力回復"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Powerful healing magic:  heals hitpoints, cuts and stun.", "極めて強力な回復呪文で、負傷と朦朧状態も全快する。"));
		break;
	case SPELL_INFO:
		var_set_string(res, format("Heals %d", spell_power(500)));
		break;
	case SPELL_CAST:
		hp_player(spell_power(500));
		set_stun(0, TRUE);
		set_cut(0, TRUE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void hell_lance_spell(int cmd, variant *res)
{
	int dam = spell_power(p_ptr->lev * 3);
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Hell Lance", "ヘル・ランス"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires a beam of pure hellfire.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, dam));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (get_aim_dir(&dir))
		{
			fire_beam(GF_HELL_FIRE, dir, dam);
			var_set_bool(res, TRUE);
		}
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_hell_lance(void) { return cast_spell(hell_lance_spell); }

void holy_lance_spell(int cmd, variant *res)
{
	int dam = spell_power(p_ptr->lev * 3);
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Holy Lance", "ホーリー・ランス"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires a beam of pure holiness.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, dam));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (get_aim_dir(&dir))
		{
			fire_beam(GF_HOLY_FIRE, dir, dam);
			var_set_bool(res, TRUE);
		}
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_holy_lance(void) { return cast_spell(holy_lance_spell); }

void hp_to_sp_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Convert HP to SP", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Converts HP into SP", ""));
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

			if (wounds > 0 && p_ptr->pclass != CLASS_RUNE_KNIGHT)
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

	case SPELL_CAST:
	{
		int gain_sp = take_hit(DAMAGE_USELIFE, p_ptr->lev, T("thoughtless convertion from HP to SP", "ＨＰからＭＰへの無謀な変換"), -1) / 5;
		if (gain_sp && p_ptr->pclass != CLASS_RUNE_KNIGHT)
		{
			p_ptr->csp += gain_sp;
			if (p_ptr->csp > p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
			}

			p_ptr->redraw |= PR_MANA;
		}
		else
			msg_print(T("You failed to convert.", "変換に失敗した。"));

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void hypnotic_gaze_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Hypnotic Gaze");
		break;
	case SPELL_DESC:
		var_set_string(res, "Attempt to charm a monster.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("Your eyes look mesmerizing...", "催眠眼の能力を得た。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("Your eyes look uninteresting.", "あなたの目はつまらない目になった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("Your gaze is hypnotic.", "あなたの睨みは催眠効果をもつ。"));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (get_aim_dir(&dir))
		{
			msg_print(T("Your eyes look mesmerizing...", "あなたの目は幻惑的になった..."));
			charm_monster(dir, p_ptr->lev);
			var_set_bool(res, TRUE);
		}
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_hypnotic_gaze(void) { return cast_spell(hypnotic_gaze_spell); }

void imp_fire_spell(int cmd, variant *res)
{
	const int ball_lev = 30;
	switch (cmd)
	{
	case SPELL_NAME:
		if (p_ptr->lev >= ball_lev)
			var_set_string(res, T("Fire Ball", ""));
		else
			var_set_string(res, T("Fire Bolt", ""));
		break;
	case SPELL_SPOIL_NAME:
		var_set_string(res, "Fire Bolt/Ball");
		break;
	case SPELL_DESC:
		if (p_ptr->lev >= ball_lev)
			var_set_string(res, T("Generate a Fire Ball on chosen target.", ""));
		else
			var_set_string(res, T("Hurls a fiery missile at chosen target.", ""));
		break;
	case SPELL_SPOIL_DESC:
		var_set_string(res, "Fire Bolt for L damage. At L30, does a radius 2 Fire Ball for 2L damage instead.");
		break;
	case SPELL_INFO:
		if (p_ptr->lev >= ball_lev)
			var_set_string(res, info_damage(0, 0, spell_power(p_ptr->lev * 2)));
		else
			var_set_string(res, info_damage(0, 0, spell_power(p_ptr->lev)));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		if (p_ptr->lev >= ball_lev)
			fire_ball(GF_FIRE, dir, spell_power(p_ptr->lev * 2), 2);
		else
			fire_bolt(GF_FIRE, dir, spell_power(p_ptr->lev));
		var_set_bool(res, TRUE);
		break;
	}
	case SPELL_COST_EXTRA:
		if (p_ptr->lev >= ball_lev)
			var_set_int(res, 7);
		else
			var_set_int(res, 0);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void kutar_expand_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Expand Horizontally", "横に伸びる"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Expand like a cat, gaining +50 AC but becoming more susceptible to magical attacks.", ""));
		break;
	case SPELL_CAST:
		set_tsubureru(randint1(20) + 30, FALSE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void laser_eye_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Laser Eye", "レーザー・アイ"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Fires a laser beam.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(p_ptr->lev*2)));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("Your eyes burn for a moment.", "あなたの目は一瞬焼け付いた。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("Your eyes burn for a moment, then feel soothed.", "眼が少しの間焼き付いて、痛みが和らいだ。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("Your eyes can fire laser beams.", "あなたは目からレーザー光線を発射することができる。"));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (get_aim_dir(&dir))
		{
			fire_beam(GF_LITE, dir, spell_power(2 * p_ptr->lev));
			var_set_bool(res, TRUE);
		}
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_laser_eye(void) { return cast_spell(laser_eye_spell); }

void light_area_spell(int cmd, variant *res)
{
	int dice = 2;
	int sides = p_ptr->lev / 2;
	int rad = spell_power(p_ptr->lev / 10 + 1);

	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Light Area");
		break;
	case SPELL_DESC:
		var_set_string(res, "Lights up nearby area and the inside of a room permanently.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You can light up rooms with your presence.", "あなたは光り輝いて部屋を明るくするようになった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You can no longer light up rooms with your presence.", "部屋を明るく照らすことが出来なくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can emit bright light.", "あなたは明るい光を放つことができる。"));
		break;
	case SPELL_CAST:
		lite_area(spell_power(damroll(dice, sides)), rad);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_light_area(void) { return cast_spell(light_area_spell); }

void living_trump_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Living Trump", "人間トランプ"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Gives mutation which makes you teleport randomly or makes you able to teleport at will.", "ランダムにテレポートする突然変異か、自分の意思でテレポートする突然変異が身につく。"));
		break;
	case SPELL_CAST:
	{
		int mutation = one_in_(7) ? MUT_TELEPORT : MUT_TELEPORT_RND;

		if (mut_gain(mutation))
			msg_print(T("You have turned into a Living Trump.", "あなたは生きているカードに変わった。"));
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

