#include "angband.h"

/*
void foo_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "");
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_INFO:
		var_set_string(res, "");
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
	case SPELL_CAST:
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_foo(void) { return cast_spell(foo_spell); }
*/

void berserk_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Berserk");
		break;
	case SPELL_DESC:
		var_set_string(res, "Enter a berserk frenzy, gaining great combat bonuses, but losing the ability to think clearly.");
		break;
	case SPELL_CAST:
		msg_print("Raaagh!  You feel like hitting something.");
		set_afraid(0, TRUE);
		set_shero(10 + randint1(p_ptr->lev), FALSE);
		hp_player(30);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_berserk(void) { return cast_spell(berserk_spell); }

void breathe_fire_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Breathe Fire", "炎のブレス"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Breathes Fire at your opponent.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You gain the ability to breathe fire.", "火を吐く能力を得た。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You lose the ability to breathe fire.", "炎のブレスを吐く能力を失った。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can breathe fire (dam lvl * 2).", "あなたは炎のブレスを吐くことができる。(ダメージ レベルX2)"));
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, 2 * p_ptr->lev));
		break;
	case SPELL_COST_EXTRA:
		var_set_int(res, p_ptr->lev);
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (get_aim_dir(&dir))
		{
			racial_stop_mouth();
			msg_print(T("You breathe fire...", "あなたは火炎のブレスを吐いた..."));
			fire_ball(GF_FIRE, dir, 2 * p_ptr->lev, 1 + (p_ptr->lev / 20));
			var_set_bool(res, TRUE);
		}
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_breathe_fire(void) { return cast_spell(breathe_fire_spell); }

void detect_monsters_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Detect Monsters");
		break;
	case SPELL_DESC:
		var_set_string(res, "Detects nearby monsters.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You smell filthy monsters.", "モンスターの臭いを嗅ぎ分けられるようになった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You no longer smell filthy monsters.", "不潔なモンスターの臭いを嗅げなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can smell nearby monsters.", "あなたは近くのモンスターの存在をかぎ分けることができる。"));
		break;
	case SPELL_CAST:
		detect_monsters_normal(DETECT_RAD_DEFAULT);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_detect_monsters(void) { return cast_spell(detect_monsters_spell); }

void detect_traps_spell(int cmd, variant *res)
{
	int rad = DETECT_RAD_DEFAULT;

	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Detect Traps");
		break;
	case SPELL_DESC:
		var_set_string(res, "Detects traps in your vicinity.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_radius(rad));
		break;
	case SPELL_CAST:
		detect_traps(rad, TRUE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_detect_traps(void) { return cast_spell(detect_traps_spell); }

void detect_treasure_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Detect Treasure");
		break;
	case SPELL_DESC:
		var_set_string(res, "Detects nearby treasure.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You smell a metallic odor.", "金属の匂いを嗅ぎ分けられるようになった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You no longer smell a metallic odor.", "金属の臭いを嗅げなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can smell nearby precious metal.", "あなたは近くにある貴金属をかぎ分けることができる。"));
		break;
	case SPELL_CAST:
		detect_treasure(DETECT_RAD_DEFAULT);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_detect_treasure(void) { return cast_spell(detect_treasure_spell); }


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

void light_area_spell(int cmd, variant *res)
{
	int dice = 2;
	int sides = spell_power(p_ptr->lev / 2);
	int rad = spell_power(p_ptr->lev / 10 + 1);

	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Light Area");
		break;
	case SPELL_DESC:
		var_set_string(res, "Lights up nearby area and the inside of a room permanently.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(dice, sides, 0));
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

void mind_blast_spell(int cmd, variant *res)
{
	int dice = 3 + (p_ptr->lev - 1)/5;
	int sides = 3;
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Mind Blast", "精神攻撃"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Attempt to blast your opponent with psionic energy.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(spell_power(dice), sides, 0));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You gain the power of Mind Blast.", "精神攻撃の能力を得た。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You lose the power of Mind Blast.", "精神攻撃の能力を失った。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can Mind Blast your enemies.", "あなたは敵を精神攻撃できる。"));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (get_aim_dir(&dir))
		{
			msg_print(T("You concentrate...", "集中している..."));
			fire_bolt(GF_PSI, dir, spell_power(damroll(dice, sides)));
			var_set_bool(res, TRUE);
		}
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_mind_blast(void) { return cast_spell(mind_blast_spell); }

void phase_door_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Phase Door");
		break;
	case SPELL_DESC:
		var_set_string(res, "A short range teleport.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You gain the power of minor teleportation.", "近距離テレポートの能力を得た。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You lose the power of minor teleportation.", "近距離テレポートの能力を失った。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can teleport yourself short distances.", "あなたは短い距離をテレポートできる。"));
		break;
	case SPELL_CAST:
		teleport_player(10, 0);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_phase_door(void) { return cast_spell(phase_door_spell); }

void polish_shield_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Polish Shield");
		break;
	case SPELL_DESC:
		var_set_string(res, "Makes your shield reflect missiles and bolt spells.");
		break;
	case SPELL_CAST:
		polish_shield();
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_polish_shield(void) {	return cast_spell(polish_shield_spell); }

void radiation_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Emit Radiation", "放射能"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Generates a huge ball of radiation centered on you.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(p_ptr->lev)));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You start emitting hard radiation.", "あなたは強い放射線を発生し始めた。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You stop emitting hard radiation.", "あなたは放射能を発生しなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can emit hard radiation at will.", "あなたは自分の意思で放射能を発生することができる。"));
		break;
	case SPELL_CAST:
		msg_print(T("Radiation flows from your body!", "体から放射能が発生した！"));
		fire_ball(GF_NUKE, 0, spell_power(p_ptr->lev * 2), 3 + (p_ptr->lev / 20));
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_radiation(void) { return cast_spell(radiation_spell); }

void recharging_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Recharging");
		break;
	case SPELL_DESC:
		var_set_string(res, "Attempts to recharge staffs, wands or rods.  Items may be destroyed on failure.");
		break;
	case SPELL_CAST:
		var_set_bool(res, recharge(4 * p_ptr->lev));
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_recharging(void) { return cast_spell(recharging_spell); }

void spit_acid_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Spit Acid", "酸の唾"));
		break;
	case SPELL_DESC:
		if (p_ptr->lev < 25)
			var_set_string(res, "Spits a bolt of acid.");
		else
			var_set_string(res, "Spits a ball of acid.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You gain the ability to spit acid.", "酸を吐く能力を得た。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You lose the ability to spit acid.", "酸を吹きかける能力を失った。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can spit acid (dam lvl).", "あなたは酸を吹きかけることができる。(ダメージ レベルX1)"));
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, p_ptr->lev));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (get_aim_dir(&dir))
		{
			racial_stop_mouth();
			msg_print(T("You spit acid...", "酸を吐きかけた..."));
			if (p_ptr->lev < 25) fire_bolt(GF_ACID, dir, p_ptr->lev);
			else fire_ball(GF_ACID, dir, p_ptr->lev, 2);
			var_set_bool(res, TRUE);
		}
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_spit_acid(void) { return cast_spell(spit_acid_spell); }

void summon_tree_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Summon Tree");
		break;
	case SPELL_DESC:
		var_set_string(res, "Attempts to summon a tree.");
		break;
	case SPELL_CAST:
	{
		int attempts = 0;
		int x, y, dir;

		for (;;)
		{
			if (attempts > 4)
			{
				msg_print("No trees arrive.");
				break;
			}

			dir = randint0(9);
			if (dir == 5) continue;

			attempts++;
			y = py + ddy[dir];
			x = px + ddx[dir];

			if (!in_bounds(y, x)) continue;
			if (!cave_naked_bold(y, x)) continue;
			if (player_bold(y, x)) continue;

			cave_set_feat(y, x, feat_tree);
			break;
		}
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_summon_tree(void) { return cast_spell(summon_tree_spell); }

void telekinesis_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Telekinesis", "念動力"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Attempts to fetch a distant object.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You gain the ability to move objects telekinetically.", "物体を念動力で動かす能力を得た。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You lose the ability to move objects telekinetically.", "念動力で物を動かす能力を失った。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You are telekinetic.", "あなたは念動力をもっている。"));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (get_aim_dir(&dir))
		{
			fetch(dir, p_ptr->lev * 10, TRUE);
			var_set_bool(res, TRUE);
		}
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_telekinesis(void) { return cast_spell(telekinesis_spell); }

void teleport_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Teleport", "テレポート"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Escape to a distant location.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You gain the power of teleportation at will.", "自分の意思でテレポートする能力を得た。"));
		mut_lose(MUT_TELEPORT_RND);
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You lose the power of teleportation at will.", "自分の意思でテレポートする能力を失った。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can teleport at will.", "あなたは自分の意思でテレポートできる。"));
		break;
	case SPELL_CAST:
		teleport_player(10 + 4 * p_ptr->lev, 0);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_teleport(void) { return cast_spell(teleport_spell); }

void vampirism_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Vampiric Drain", "吸血ドレイン"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Suck blood from an adjacent monster, gaining hp in the process.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, p_ptr->lev * 2));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You become vampiric.", "生命力を吸収できるようになった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You are no longer vampiric.", "吸血の能力を失った。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can drain life from a foe like a vampire.", "あなたは吸血鬼のように敵から生命力を吸収することができる。"));
		break;
	case SPELL_CAST:
	{
		int x, y, dummy;
		cave_type *c_ptr;
		int dir = 0;

		var_set_bool(res, FALSE);

		/* Only works on adjacent monsters */
		if (!get_rep_dir2(&dir)) break;

		var_set_bool(res, TRUE);

		y = py + ddy[dir];
		x = px + ddx[dir];
		c_ptr = &cave[y][x];

		racial_stop_mouth();

		if (!(c_ptr->m_idx))
		{
			msg_print(T("You bite into thin air!", "何もない場所に噛みついた！"));
			break;
		}

		msg_print(T("You grin and bare your fangs...", "あなたはニヤリとして牙をむいた..."));
		dummy = p_ptr->lev * 2;

		if (drain_life(dir, dummy))
		{
			/* No heal if we are "full" */
			if (p_ptr->food < PY_FOOD_FULL)
				hp_player(dummy);
			else
				msg_print(T("You were not hungry.", "あなたは空腹ではありません。"));

			/* Gain nutritional sustenance: 150/hp drained
			 * A Food ration gives 5000 food points (by contrast)
			 * Don't ever get more than "Full" this way
			 * But if we ARE Gorged,  it won't cure us 
			 */
			dummy = p_ptr->food + MIN(5000, 100 * dummy);
			if (p_ptr->food < PY_FOOD_MAX)   /* Not gorged already */
				set_food(dummy >= PY_FOOD_MAX ? PY_FOOD_MAX-1 : dummy);
		}
		else
			msg_print(T("Yechh. That tastes foul.", "げぇ！ひどい味だ。"));

		break;
	}
	case SPELL_COST_EXTRA:
		var_set_int(res, p_ptr->lev / 3);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_vampirism(void) { return cast_spell(vampirism_spell); }
