#include "angband.h"

/*
void foo_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("", ""));
		break;
	case SPELL_DESC:
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

void alchemy_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Alchemy", "ミダスの手"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Turns valuable items into gold.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You gain the Midas touch.", "「ミダス王の手」の能力を得た。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You lose the Midas touch.", "ミダスの手の能力を失った。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can turn ordinary items to gold.", "あなたは通常アイテムを金に変えることができる。"));
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (alchemy())
			var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_alchemy(void) { return cast_spell(alchemy_spell); }

void android_ray_gun_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Ray Gun", "レイガン"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(5 + (p_ptr->lev+1) / 2)));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		
		msg_print(T("You fire your ray gun.", "レイガンを発射した。"));
		fire_bolt(GF_MISSILE, dir, spell_power(5 + (p_ptr->lev+1) / 2));
		
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void android_blaster_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Blaster", "ブラスター"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(5 + p_ptr->lev)));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;

		msg_print(T("You fire your blaster.", "ブラスターを発射した。"));
		fire_bolt(GF_MISSILE, dir, spell_power(5 + p_ptr->lev));

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void android_bazooka_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Bazooka", "バズーカ"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires your bazooka at a nearby monster.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(25 + p_ptr->lev * 2)));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;

		msg_print(T("You fire your bazooka.", "バズーカを発射した。"));
		fire_ball(GF_MISSILE, dir, spell_power(25 + p_ptr->lev * 2), 2);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void android_beam_cannon_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Beam Cannon", "ビームキャノン"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(25 + p_ptr->lev * 3)));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;

		msg_print(T("You fire a beam cannon.", "ビームキャノンを発射した。"));
		fire_beam(GF_MISSILE, dir, spell_power(25 + p_ptr->lev * 3));

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void android_rocket_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Rocket Launcher", "ロケット"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Launches a powerful rocket at your opponent.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(p_ptr->lev * 7)));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;

		msg_print(T("You launch a rocket.", "ロケットを発射した。"));
		fire_rocket(GF_ROCKET, dir, spell_power(p_ptr->lev * 7), 2);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void banish_evil_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Banish Evil", "邪悪消滅"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Attempts to remove a single evil opponent.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You feel a holy wrath fill you.", "神聖な怒りの力に満たされた。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You no longer feel a holy wrath.", "神聖な怒りの力を感じなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can send evil creatures directly to Hell.", "あなたは邪悪なモンスターを地獄に落とすことができる。"));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		int x, y;
		cave_type *c_ptr;
		monster_type *m_ptr;
		monster_race *r_ptr;

		if (!get_rep_dir2(&dir)) 
		{
			var_set_bool(res, FALSE);
			break;
		}

		var_set_bool(res, TRUE);

		y = py + ddy[dir];
		x = px + ddx[dir];
		c_ptr = &cave[y][x];

		if (!c_ptr->m_idx)
		{
			msg_print(T("You sense no evil there!", "邪悪な存在を感じとれません！"));
			break;
		}

		m_ptr = &m_list[c_ptr->m_idx];
		r_ptr = &r_info[m_ptr->r_idx];

		if ((r_ptr->flags3 & RF3_EVIL) &&
			!(r_ptr->flags1 & RF1_QUESTOR) &&
			!(r_ptr->flags1 & RF1_UNIQUE) &&
			!p_ptr->inside_arena && !p_ptr->inside_quest &&
			(r_ptr->level < randint1(p_ptr->lev+50)) &&
			!(m_ptr->mflag2 & MFLAG2_NOGENO))
		{
			if (record_named_pet && is_pet(m_ptr) && m_ptr->nickname)
			{
				char m_name[80];

				monster_desc(m_name, m_ptr, MD_INDEF_VISIBLE);
				do_cmd_write_nikki(NIKKI_NAMED_PET, RECORD_NAMED_PET_GENOCIDE, m_name);
			}

			/* Delete the monster, rather than killing it. */
			delete_monster_idx(c_ptr->m_idx);
			msg_print(T("The evil creature vanishes in a puff of sulfurous smoke!", "その邪悪なモンスターは硫黄臭い煙とともに消え去った！"));
		}
		else
		{
			msg_print(T("Your invocation is ineffectual!", "祈りは効果がなかった！"));
			if (one_in_(13)) m_ptr->mflag2 |= MFLAG2_NOGENO;
		}
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_banish_evil(void) { return cast_spell(banish_evil_spell); }

void battle_frenzy_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Battle Frenzy", "狂乱戦士"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Gives another bonus to hit and HP, immunity to fear for a while. Hastes you. But decreases AC.", "狂戦士化し、恐怖を除去し、加速する。"));
		break;
	case SPELL_CAST:
	{
		int b_base = spell_power(25);
		int sp_base = spell_power(p_ptr->lev / 2);
		int sp_sides = 20 + p_ptr->lev / 2;

		set_shero(randint1(25) + 25, FALSE);
		hp_player(30);
		set_afraid(0, TRUE);
		set_fast(randint1(sp_sides) + sp_base, FALSE);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void berserk_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Berserk", "狂戦士化"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Enter a berserk frenzy, gaining great combat bonuses, but losing the ability to think clearly.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You feel a controlled rage.", "制御できる激情を感じる。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You no longer feel a controlled rage.", "制御できる激情を感じなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can drive yourself into a berserk frenzy.", "あなたは自分の意思で狂乱戦闘状態になることができる。"));
		break;
	case SPELL_CAST:
	{
		bool heal = !p_ptr->shero;
		msg_print("Raaagh!  You feel like hitting something.");
		set_afraid(0, TRUE);
		set_shero(10 + randint1(p_ptr->lev), FALSE);
		if (heal)
			hp_player(30);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_berserk(void) { return cast_spell(berserk_spell); }

void bless_weapon_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Bless Weapon", "武器祝福"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Blesses your current weapon.", ""));
		break;
	case SPELL_CAST:
		var_set_bool(res, bless_weapon());
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void brain_smash_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Brain Smash", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Gaze intently at a single foe, causing damage, confusion and stunning", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(12, spell_power(12), 0));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		fire_ball_hide(GF_BRAIN_SMASH, dir, spell_power(damroll(12, 12)), 0);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void breathe_disintegration_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Breathe Disintegration", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("A disintegration breath.  Not even the dungeon walls can withstand its power!", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(MIN(p_ptr->chp / 6, 150))));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;

		stop_mouth();
		msg_print(T("You breathe disintegration.", "分解のブレスを吐いた。"));
		fire_ball(GF_DISINTEGRATE, dir, 
			spell_power(MIN(p_ptr->chp / 6, 150)), 
			(p_ptr->lev > 40 ? -3 : -2));

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void breathe_fire_I_spell(int cmd, variant *res)
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
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(2 * p_ptr->lev)));
		break;
	case SPELL_COST_EXTRA:
		var_set_int(res, (p_ptr->lev+1)/2);
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (get_aim_dir(&dir))
		{
			stop_mouth();
			msg_print(T("You breathe fire...", "あなたは火炎のブレスを吐いた..."));
			fire_ball(GF_FIRE, dir, spell_power(2 * p_ptr->lev), -1 - (p_ptr->lev / 20));
			var_set_bool(res, TRUE);
		}
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_breathe_fire_I(void) { return cast_spell(breathe_fire_I_spell); }

void breathe_fire_II_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Breathe Fire", "炎のブレス"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Breathes Fire at your opponent.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(p_ptr->chp*2/5)));
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
			stop_mouth();
			msg_print(T("You breathe fire...", "あなたは火炎のブレスを吐いた..."));
			fire_ball(GF_FIRE, dir, spell_power(p_ptr->chp*2/5), -1 - (p_ptr->lev / 20));
			var_set_bool(res, TRUE);
		}
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void cause_wounds_I_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Cause Light Wounds", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Attempts to damage a single foe.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(3, spell_power(8), 0));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		fire_ball_hide(GF_CAUSE_1, dir, spell_power(damroll(3, 8)), 0);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void cause_wounds_II_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Cause Medium Wounds", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Attempts to damage a single foe.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(8, spell_power(8), 0));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		fire_ball_hide(GF_CAUSE_2, dir, spell_power(damroll(8, 8)), 0);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void cause_wounds_III_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Cause Critical Wounds", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Attempts to damage a single foe.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(10, spell_power(15), 0));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		fire_ball_hide(GF_CAUSE_3, dir, spell_power(damroll(10, 15)), 0);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void cause_wounds_IV_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Cause Mortal Wounds", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Attempts to damage a single foe.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(15, spell_power(15), 0));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		fire_ball_hide(GF_CAUSE_4, dir, spell_power(damroll(15, 15)), 0);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void clairvoyance_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Clairvoyance", "千里眼"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Maps and lights whole dungeon level and gives telepathy for a while.", "その階全体を永久に照らし、ダンジョン内すべてのアイテムを感知する。さらに、一定時間テレパシー能力を得る。"));
		break;
	case SPELL_CAST:
		chg_virtue(V_KNOWLEDGE, 1);
		chg_virtue(V_ENLIGHTEN, 1);

		wiz_lite(p_ptr->tim_superstealth > 0);

		if (!p_ptr->telepathy)
			set_tim_esp(randint1(30) + 25, FALSE);

		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void clear_mind_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Clear Mind", "明鏡止水"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("", ""));
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (total_friends)
		{
			msg_print(T("You need to concentrate on your pets now.", "今はペットを操ることに集中していないと。"));
			return;
		}
		if (p_ptr->pclass == CLASS_RUNE_KNIGHT)
		{
			msg_print("Your mind remains cloudy.");
			return;
		}

		msg_print(T("You feel your head clear a little.", "少し頭がハッキリした。"));

		p_ptr->csp += (3 + p_ptr->lev/20);
		if (p_ptr->csp >= p_ptr->msp)
		{
			p_ptr->csp = p_ptr->msp;
			p_ptr->csp_frac = 0;
		}

		p_ptr->redraw |= (PR_MANA);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void cold_touch_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Cold Touch", "凍結の手"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Freeze things with your icy fingers!");
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(2 * p_ptr->lev)));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("Your hands get very cold.", "あなたの両手はとても冷たくなった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("Your hands warm up.", "手が暖かくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can freeze things with a touch.", "あなたは物を触って凍らせることができる。"));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		int x, y;
		cave_type *c_ptr;

		if (!get_rep_dir2(&dir))
		{
			var_set_bool(res, FALSE);
			break;
		}
		var_set_bool(res, TRUE);
		y = py + ddy[dir];
		x = px + ddx[dir];
		c_ptr = &cave[y][x];

		if (!c_ptr->m_idx)
		{
			msg_print(T("You wave your hands in the air.", "あなたは何もない場所で手を振った。"));
			break;
		}
		fire_bolt(GF_COLD, dir, spell_power(2 * p_ptr->lev));
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_cold_touch(void) { return cast_spell(cold_touch_spell); }

void confusing_lights_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Confusing Lights", "幻惑の光"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Emits confusing lights, slowing, stunning, confusing, scaring and freezing nearby monsters.");
		break;
	case SPELL_CAST:
		msg_print(T("You glare nearby monsters with a dazzling array of confusing lights!", "辺りを睨んだ..."));
		slow_monsters();
		stun_monsters(p_ptr->lev * 4);
		confuse_monsters(p_ptr->lev * 4);
		turn_monsters(p_ptr->lev * 4);
		stasis_monsters(p_ptr->lev * 4);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void create_food_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Create Food", "食糧生成"));
		break;
	case SPELL_DESC:
		if (p_ptr->prace == RACE_HOBBIT)
			var_set_string(res, "It's time for second breakfast!  Cook up a tasty meal.");
		else
			var_set_string(res, "Create a ration of tasty food.");
		break;
	case SPELL_CAST:
	{
		object_type forge;

		object_prep(&forge, lookup_kind(TV_FOOD, SV_FOOD_RATION));
		drop_near(&forge, -1, py, px);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_create_food(void) { return cast_spell(create_food_spell); }

void darkness_storm_I_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Darkness Storm", "暗黒の嵐"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires a huge ball of darkness.", "巨大な暗黒の球を放つ。"));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(100 + p_ptr->lev * 2)));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		msg_print(T("You invoke a darkness storm.", "暗黒の嵐の呪文を念じた。"));
		fire_ball(GF_DARK, dir, spell_power(100 + p_ptr->lev * 2), spell_power(4));
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void darkness_storm_II_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Darkness Storm", "暗黒の嵐"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires a huge ball of darkness of unmatched power", "巨大な暗黒の球を放つ。"));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(10, spell_power(10), spell_power(50 + p_ptr->lev * 8)));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		msg_print(T("You invoke a darkness storm.", "暗黒の嵐の呪文を念じた。"));
		fire_ball(GF_DARK, dir, 
			spell_power(50 + p_ptr->lev * 8 + damroll(10, 10)), 
			spell_power(4));
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void dazzle_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Dazzle", "眩惑"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Emits dazzling lights, stunning, confusing and scaring nearby monsters.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You gain the ability to emit dazzling lights.", "眩い閃光を発する能力を得た。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You lose the ability to emit dazzling lights.", "まばゆい閃光を発する能力を失った。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can emit confusing, blinding radiation.", "あなたは混乱と盲目を引き起こす放射能を発生することができる。"));
		break;
	case SPELL_CAST:
		stun_monsters(p_ptr->lev * 4);
		confuse_monsters(p_ptr->lev * 4);
		turn_monsters(p_ptr->lev * 4);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_dazzle(void) { return cast_spell(dazzle_spell); }

void demon_breath_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Breathe Fire/Nether", "地獄/火炎のブレス"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Breathe a powerful blast of either fire or nether at your opponent.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(p_ptr->lev * 3)));
		break;
	case SPELL_CAST:
	{
		int type = (one_in_(2) ? GF_NETHER : GF_FIRE);
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;

		stop_mouth();

		msg_format(T("You breathe %s.", "あなたは%sのブレスを吐いた。"),
			((type == GF_NETHER) ? T("nether", "地獄") : T("fire", "火炎")));

		fire_ball(type, dir, spell_power(p_ptr->lev * 3), -(p_ptr->lev / 15) - 1);
		var_set_bool(res, TRUE);
		break;
	}
	case SPELL_COST_EXTRA:
		var_set_int(res, p_ptr->lev/3);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void destruction_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Word of Destruction");
		break;
	case SPELL_DESC:
		var_set_string(res, "Destroys everything in your nearby vicinity ... except you, of course.");
		break;
	case SPELL_CAST:
		destroy_area(py, px, 12 + randint1(4), FALSE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_destruction(void) { return cast_spell(destruction_spell); }

void detect_curses_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Detect Curses", "呪い感知"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Detected cursed items in your inventory.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You can feel evil magics.", "邪悪な魔法を感知できるようになった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You can no longer feel evil magics.", "邪悪な魔法を感じられなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can feel the danger of evil magic.", "あなたは邪悪な魔法の危険を感じとることができる。"));
		break;
	case SPELL_CAST:
	{
		int i;

		for (i = 0; i < INVEN_TOTAL; i++)
		{
			object_type *o_ptr = &inventory[i];

			if (!o_ptr->k_idx) continue;
			if (!object_is_cursed(o_ptr)) continue;

			o_ptr->feeling = FEEL_CURSED;
		}
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_detect_curses(void) { return cast_spell(detect_curses_spell); }

void detect_doors_stairs_traps_spell(int cmd, variant *res)
{
	int rad = DETECT_RAD_DEFAULT;

	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Detect Doors & Traps", "ドアと罠 感知"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Detects doors, stairs, and traps in your vicinity.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_radius(rad));
		break;
	case SPELL_CAST:
		detect_traps(rad, TRUE);
		detect_doors(rad);
		detect_stairs(rad);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_detect_doors_stairs_traps(void) { return cast_spell(detect_doors_stairs_traps_spell); }

void detect_menace_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Detect Ferocity", "殺気感知"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Detects nearby menacing monsters.  Only intelligent monsters are detected.", ""));
		break;
	case SPELL_CAST:
		detect_monsters_mind(DETECT_RAD_DEFAULT);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

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

void detect_objects_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Detect Objects");
		break;
	case SPELL_DESC:
		var_set_string(res, "Detects nearby objects.");
		break;
	case SPELL_CAST:
		detect_objects_normal(DETECT_RAD_DEFAULT);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_detect_objects(void) { return cast_spell(detect_objects_spell); }

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

void dimension_door_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Dimension Door");
		break;
	case SPELL_DESC:
		var_set_string(res, "Open a portal to another dimension and step to a nearby location with great precision.");
		break;
	case SPELL_CAST:
		var_set_bool(res, dimension_door());
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_dimension_door(void) { return cast_spell(dimension_door_spell); }

void disintegrate_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Disintegrate", "原子分解"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires a huge ball of disintegration.", "巨大な分解の球を放つ。"));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(p_ptr->lev + 70)));
		break;
	case SPELL_CAST:
	{
		int dam = spell_power(p_ptr->lev + 70);
		int rad = 3 + p_ptr->lev / 40;
		int dir;
			
		var_set_bool(res, FALSE);

		if (!get_aim_dir(&dir)) return;
		fire_ball(GF_DISINTEGRATE, dir, dam, rad);
			
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void dominate_living_I_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Dominate a Living Thing", "生物支配"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("", ""));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		fire_ball_hide(GF_CONTROL_LIVING, dir, p_ptr->lev, 0);
		var_set_bool(res, TRUE);
		break;
	}
	case SPELL_COST_EXTRA:
		var_set_int(res, (p_ptr->lev+3)/4);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void dominate_living_II_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Dominate Living Things", "真・生物支配"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("", ""));
		break;
	case SPELL_CAST:
		project_hack(GF_CONTROL_LIVING, p_ptr->lev);
		var_set_bool(res, TRUE);
		break;
	case SPELL_COST_EXTRA:
		var_set_int(res, (p_ptr->lev+20)/2);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void double_magic_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Double Magic", "連続魔"));
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!can_do_cmd_cast()) return;
		handle_stuff();
		do_cmd_cast();
		handle_stuff();
		if (!p_ptr->paralyzed && can_do_cmd_cast())
			do_cmd_cast();
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void draconian_breath_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Draconian Breath", "ブレス"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires an elemental, or perhaps special, breath at your foes.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(p_ptr->lev * 2)));
		break;
	case SPELL_CAST:
	{/* Sorry ... I made no effort to clean this up :( */
		int plev = p_ptr->lev;
		int dir = 0;
		int  Type = (one_in_(3) ? GF_COLD : GF_FIRE);
#ifdef JP
		cptr Type_desc = ((Type == GF_COLD) ? "冷気" : "炎");
#else
		cptr Type_desc = ((Type == GF_COLD) ? "cold" : "fire");
#endif

		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;

		if (randint1(100) < plev)
		{
			switch (p_ptr->pclass)
			{
				case CLASS_WARRIOR:
				case CLASS_BERSERKER:
				case CLASS_RANGER:
				case CLASS_TOURIST:
				case CLASS_IMITATOR:
				case CLASS_ARCHER:
				case CLASS_SMITH:
					if (one_in_(3))
					{
						Type = GF_MISSILE;
#ifdef JP
						Type_desc = "エレメント";
#else
						Type_desc = "the elements";
#endif
					}
					else
					{
						Type = GF_SHARDS;
#ifdef JP
						Type_desc = "破片";
#else
						Type_desc = "shards";
#endif
					}
					break;
				case CLASS_MAGE:
				case CLASS_WARRIOR_MAGE:
				case CLASS_HIGH_MAGE:
				case CLASS_SORCERER:
				case CLASS_MAGIC_EATER:
				case CLASS_RED_MAGE:
				case CLASS_BLUE_MAGE:
				case CLASS_MIRROR_MASTER:
					if (one_in_(3))
					{
						Type = GF_MANA;
#ifdef JP
						Type_desc = "魔力";
#else
						Type_desc = "mana";
#endif
					}
					else
					{
						Type = GF_DISENCHANT;
#ifdef JP
						Type_desc = "劣化";
#else
						Type_desc = "disenchantment";
#endif
					}
					break;
				case CLASS_CHAOS_WARRIOR:
					if (!one_in_(3))
					{
						Type = GF_CONFUSION;
#ifdef JP
						Type_desc = "混乱";
#else
						Type_desc = "confusion";
#endif
					}
					else
					{
						Type = GF_CHAOS;
#ifdef JP
						Type_desc = "カオス";
#else
						Type_desc = "chaos";
#endif
					}
					break;
				case CLASS_MONK:
				case CLASS_SAMURAI:
				case CLASS_FORCETRAINER:
					if (!one_in_(3))
					{
						Type = GF_CONFUSION;
#ifdef JP
						Type_desc = "混乱";
#else
						Type_desc = "confusion";
#endif
					}
					else
					{
						Type = GF_SOUND;
#ifdef JP
						Type_desc = "轟音";
#else
						Type_desc = "sound";
#endif
					}
					break;
				case CLASS_MINDCRAFTER:
					if (!one_in_(3))
					{
						Type = GF_CONFUSION;
#ifdef JP
						Type_desc = "混乱";
#else
						Type_desc = "confusion";
#endif
					}
					else
					{
						Type = GF_PSI;
#ifdef JP
						Type_desc = "精神エネルギー";
#else
						Type_desc = "mental energy";
#endif
					}
					break;
				case CLASS_PRIEST:
				case CLASS_PALADIN:
					if (one_in_(3))
					{
						Type = GF_HELL_FIRE;
#ifdef JP
						Type_desc = "地獄の劫火";
#else
						Type_desc = "hellfire";
#endif
					}
					else
					{
						Type = GF_HOLY_FIRE;
#ifdef JP
						Type_desc = "聖なる炎";
#else
						Type_desc = "holy fire";
#endif
					}
					break;
				case CLASS_ROGUE:
				case CLASS_NINJA:
					if (one_in_(3))
					{
						Type = GF_DARK;
#ifdef JP
						Type_desc = "暗黒";
#else
						Type_desc = "darkness";
#endif
					}
					else
					{
						Type = GF_POIS;
#ifdef JP
						Type_desc = "毒";
#else
						Type_desc = "poison";
#endif
					}
					break;
				case CLASS_BARD:
					if (!one_in_(3))
					{
						Type = GF_SOUND;
#ifdef JP
						Type_desc = "轟音";
#else
						Type_desc = "sound";
#endif
					}
					else
					{
						Type = GF_CONFUSION;
#ifdef JP
						Type_desc = "混乱";
#else
						Type_desc = "confusion";
#endif
					}
					break;
			}
		}

		stop_mouth();

#ifdef JP
		msg_format("あなたは%sのブレスを吐いた。", Type_desc);
#else
		msg_format("You breathe %s.", Type_desc);
#endif

		fire_ball(Type, dir, spell_power(plev * 2),
			-(plev / 15) - 1);

		var_set_bool(res, TRUE);
		break;
	}
	case SPELL_COST_EXTRA:
		var_set_int(res, (p_ptr->lev+1)/2);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void earthquake_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Earthquake", "地震"));
		break;
	case SPELL_DESC:
		var_set_string(res, "The walls will tremble and the gound will shake.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You gain the ability to wreck the dungeon.", "ダンジョンを破壊する能力を得た。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You lose the ability to wreck the dungeon.", "ダンジョンを壊す能力を失った。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can bring down the dungeon around your ears.", "あなたは周囲のダンジョンを崩壊させることができる。"));
		break;
	case SPELL_CAST:
		earthquake(py, px, 10);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_earthquake(void) { return cast_spell(earthquake_spell); }

void eat_magic_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Eat Magic", "魔力食い"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Consumes magical devices to regain spell points.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("Your magic items look delicious.", "魔法のアイテムが美味そうに見える。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("Your magic items no longer look delicious.", "魔法のアイテムはもう美味しそうに見えなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can consume magic energy for your own use.", "あなたは魔法のエネルギーを自分の物として使用できる。"));
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (eat_magic(p_ptr->lev * 2))
			var_set_bool(res, TRUE);
		break;
	case SPELL_FAIL_MIN:
		var_set_int(res, 11);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_eat_magic(void) { return cast_spell(eat_magic_spell); }

void eat_rock_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Eat Rock", "岩食い"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Consumes nearby rock.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("The walls look delicious.", "壁が美味しそうに見える。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("The walls look unappetizing.", "壁は美味しそうに見えなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can consume solid rock.", "あなたは硬い岩を食べることができる。"));
		break;
	case SPELL_CAST:
	{
		int x, y;
		cave_type *c_ptr;
		feature_type *f_ptr, *mimic_f_ptr;
		int dir = 0;

		var_set_bool(res, FALSE);

		if (!get_rep_dir2(&dir)) break;
		y = py + ddy[dir];
		x = px + ddx[dir];
		c_ptr = &cave[y][x];
		f_ptr = &f_info[c_ptr->feat];
		mimic_f_ptr = &f_info[get_feat_mimic(c_ptr)];

		stop_mouth();

		if (!have_flag(mimic_f_ptr->flags, FF_HURT_ROCK))
		{
			msg_print(T("You cannot eat this feature.", "この地形は食べられない。"));
			break;
		}
		else if (have_flag(f_ptr->flags, FF_PERMANENT))
		{
			msg_format(T("Ouch!  This %s is harder than your teeth!", "いてっ！この%sはあなたの歯より硬い！"), 
				f_name + mimic_f_ptr->name);

			break;
		}
		else if (c_ptr->m_idx)
		{
			monster_type *m_ptr = &m_list[c_ptr->m_idx];
			msg_print(T("There's something in the way!", "何かが邪魔しています！"));
			if (!m_ptr->ml || !is_pet(m_ptr)) py_attack(y, x, 0);
			break;
		}
		else if (have_flag(f_ptr->flags, FF_TREE))
		{
			msg_print(T("You don't like the woody taste!", "木の味は好きじゃない！"));
			break;
		}
		else if (have_flag(f_ptr->flags, FF_GLASS))
		{
			msg_print(T("You don't like the glassy taste!", "ガラスの味は好きじゃない！"));
			break;
		}
		else if (have_flag(f_ptr->flags, FF_DOOR) || have_flag(f_ptr->flags, FF_CAN_DIG))
		{
			set_food(p_ptr->food + 3000);
		}
		else if (have_flag(f_ptr->flags, FF_MAY_HAVE_GOLD) || have_flag(f_ptr->flags, FF_HAS_GOLD))
		{
			set_food(p_ptr->food + 5000);
		}
		else
		{
			msg_format(T("This %s is very filling!", "この%sはとてもおいしい！"), 
				f_name + mimic_f_ptr->name);
			set_food(p_ptr->food + 10000);
		}

		/* Destroy the wall */
		cave_alter_feat(y, x, FF_HURT_ROCK);

		/* Move the player */
		move_player_effect(y, x, MPE_DONT_PICKUP);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_eat_rock(void) { return cast_spell(eat_rock_spell); }

void evocation_spell(int cmd, variant *res)
{
	int power = spell_power(p_ptr->lev * 4);
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Evocation", "召魂"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Dispels, scares and banishes all monsters in view.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, power));
		break;
	case SPELL_CAST:
		dispel_monsters(power);
		turn_monsters(power);
		banish_monsters(power);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void enchantment_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Enchantment", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, "Attempts to enchant a weapon, ammo or armor.");
		break;
	case SPELL_CAST:
	{
		int         item;
		bool        okay = FALSE;
		object_type *o_ptr;
		char        o_name[MAX_NLEN];

		var_set_bool(res, FALSE);

		item_tester_hook = object_is_weapon_armour_ammo;
		item_tester_no_ryoute = TRUE;

		if (!get_item(&item, "Enchant which item? ", "You have nothing to enchant.", (USE_EQUIP | USE_INVEN | USE_FLOOR))) return;

		if (item >= 0)
			o_ptr = &inventory[item];
		else
			o_ptr = &o_list[0 - item];

		object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

		if (object_is_weapon_ammo(o_ptr))
		{
			if (enchant(o_ptr, randint0(4) + 1, ENCH_TOHIT)) okay = TRUE;
			if (enchant(o_ptr, randint0(4) + 1, ENCH_TODAM)) okay = TRUE;
		}
		else
		{
			if (enchant(o_ptr, randint0(3) + 2, ENCH_TOAC)) okay = TRUE;			
		}
			

		msg_format("%s %s glow%s brightly!",
			   ((item >= 0) ? "Your" : "The"), o_name,
			   ((o_ptr->number > 1) ? "" : "s"));

		if (!okay)
		{
			if (flush_failure) flush();
			msg_print("The enchantment failed.");
			if (one_in_(3)) chg_virtue(V_ENCHANT, -1);
		}
		else
			chg_virtue(V_ENCHANT, 1);

		calc_android_exp();
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_enchantment(void) { return cast_spell(enchantment_spell); }

void explosive_rune_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Explosive Rune", "爆発のルーン"));
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_CAST:
		msg_print(T("You carefully set an explosive rune...", "爆発のルーンを慎重に仕掛けた..."));
		explosive_rune();
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void eye_for_an_eye_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("An Eye for an Eye", "目には目を"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Gives special aura for a while. When you are attacked by a monster, the monster are injured with same amount of damage as you take.", "一定時間、自分がダメージを受けたときに攻撃を行ったモンスターに対して同等のダメージを与える。"));
		break;
	case SPELL_CAST:
		set_tim_eyeeye(spell_power(randint1(10) + 10), FALSE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void glyph_of_warding_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Glyph of Warding", "結界の紋章"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Sets a glyph on the floor beneath you. Monsters cannot attack you if you are on a glyph, but can try to break glyph.", "自分のいる床の上に、モンスターが通り抜けたり召喚されたりすることができなくなるルーンを描く。"));
		break;
	case SPELL_CAST:
		warding_glyph();
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void grow_mold_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Grow Mold", "カビ発生"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Surrounds yourself with moldy things.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You feel a sudden affinity for mold.", "突然カビに親しみを覚えた。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You feel a sudden dislike for mold.", "突然カビが嫌いになった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can cause mold to grow near you.", "あなたは周囲にキノコを生やすことができる。"));
		break;
	case SPELL_CAST:
	{
		int i;
		for (i = 0; i < 8; i++)
		{
			summon_specific(-1, py, px, p_ptr->lev, SUMMON_BIZARRE1, PM_FORCE_PET);
		}
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_grow_mold(void) { return cast_spell(grow_mold_spell); }

void heroism_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Heroism", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_CAST:
		set_hero(randint1(25) + 25, FALSE);
		hp_player(10);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_heroism(void) { return cast_spell(heroism_spell); }

void identify_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Identify", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, "");
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
	case SPELL_DESC:
		if (p_ptr->lev >= ball_lev)
			var_set_string(res, T("", ""));
		else
			var_set_string(res, T("", ""));
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

void magic_mapping_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Magic Mapping");
		break;
	case SPELL_DESC:
		var_set_string(res, "Maps the dungeon in your vicinity.");
		break;
	case SPELL_CAST:
		map_area(DETECT_RAD_MAP);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_magic_mapping(void) { return cast_spell(magic_mapping_spell); }

void magic_missile_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Magic Missile", "マジック・ミサイル"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires a weak bolt of unresistable magic.", "弱い魔法の矢を放つ。"));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(spell_power(3 + ((p_ptr->lev - 1) / 5)), 4, 0));
		break;
	case SPELL_CAST:
	{
		int dice = spell_power(3 + ((p_ptr->lev - 1) / 5));
		int sides = 4;
		int dir = 0;

		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		fire_bolt_or_beam(beam_chance() - 10, GF_MISSILE, dir, damroll(dice, sides));
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_magic_missile(void) { return cast_spell(magic_missile_spell); }

void mana_branding_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Mana Branding", "魔法剣"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Makes current weapon some elemental branded. You must wield weapons.", "一定時間、武器に冷気、炎、電撃、酸、毒のいずれかの属性をつける。武器を持たないと使えない。"));
		break;
	case SPELL_CAST:
		var_set_bool(res, choose_ele_attack());
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void mana_bolt_I_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Mana Bolt I", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires a bolt of pure mana.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(1, spell_power(p_ptr->lev * 7 / 2), spell_power(50)));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;

		msg_print(T("You cast a mana bolt.", "魔力の矢の呪文を唱えた。"));
		fire_bolt(GF_MANA, dir, spell_power(randint1(p_ptr->lev * 7 / 2) + 50));

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void mana_bolt_II_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Mana Bolt II", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires a powerful bolt of pure mana.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(1, spell_power(p_ptr->lev * 7), spell_power(100)));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;

		msg_print(T("You cast a mana bolt.", "魔力の矢の呪文を唱えた。"));
		fire_bolt(GF_MANA, dir, spell_power(randint1(p_ptr->lev * 7) + 100));

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void mana_storm_I_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Mana Storm", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires a large ball of pure mana.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(10, spell_power(10), spell_power(p_ptr->lev * 5)));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;

		msg_print(T("You cast a mana storm.", "魔力の嵐の呪文を念じた。"));
		fire_ball(GF_MANA, dir, spell_power(p_ptr->lev * 5 + damroll(10, 10)), 4);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void mana_storm_II_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Mana Storm", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires a large ball of pure mana.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(10, spell_power(10), spell_power(p_ptr->lev * 8 + 50)));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;

		msg_print(T("You cast a mana storm.", "魔力の嵐の呪文を念じた。"));
		fire_ball(GF_MANA, dir, spell_power(p_ptr->lev * 8 + 50 + damroll(10, 10)), 4);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void massacre_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Massacre", "皆殺し"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Attack all adjacent monsters in a fit of wild, uncontrollable fury.", ""));
		break;
	case SPELL_CAST:
	{
		int              dir, x, y;
		cave_type       *c_ptr;
		monster_type    *m_ptr;

		for (dir = 0; dir < 8; dir++)
		{
			y = py + ddy_ddd[dir];
			x = px + ddx_ddd[dir];
			c_ptr = &cave[y][x];

			m_ptr = &m_list[c_ptr->m_idx];

			if (c_ptr->m_idx && (m_ptr->ml || cave_have_flag_bold(y, x, FF_PROJECT)))
				py_attack(y, x, 0);
		}
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

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

void orb_of_entropy_spell(int cmd, variant *res)
{
	int base;

	if (p_ptr->pclass == CLASS_MAGE || p_ptr->pclass == CLASS_HIGH_MAGE || p_ptr->pclass == CLASS_SORCERER)
		base = p_ptr->lev + p_ptr->lev / 2;
	else
		base = p_ptr->lev + p_ptr->lev / 4;

	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Orb of Entropy", "エントロピーの球"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires a ball which damages living monsters.", "生命のある者に効果のある球を放つ。"));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(3, spell_power(6), spell_power(base)));
		break;
	case SPELL_CAST:
	{
		int dir;
		int rad = (p_ptr->lev < 30) ? 2 : 3;

		var_set_bool(res, FALSE);
		
		if (!get_aim_dir(&dir)) return;
		fire_ball(GF_OLD_DRAIN, dir, spell_power(damroll(3, 6) + base), rad);
		
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void panic_hit_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Panic Hit", "ヒット＆アウェイ"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Attack an adjacent monster and attempt a getaway.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You suddenly understand how thieves feel.", "突然、泥棒の気分が分かるようになった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You no longer feel jumpy.", "あちこちへ跳べる気分がなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can run for your life after hitting something.", "あなたは攻撃した後身を守るため逃げることができる。"));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		int x, y;

		var_set_bool(res, FALSE);
		if (!get_rep_dir2(&dir)) break;
		y = py + ddy[dir];
		x = px + ddx[dir];
		if (cave[y][x].m_idx)
		{
			py_attack(y, x, 0);
			if (randint0(p_ptr->skill_dis) < 7)
				msg_print(T("You failed to teleport.", "うまく逃げられなかった。"));
			else 
				teleport_player(30, 0L);
	
			var_set_bool(res, TRUE);
		}
		else
		{
			msg_print(T("You don't see any monster in this direction", "その方向にはモンスターはいません。"));
			msg_print(NULL);
			/* No Charge for this Action ... */
		}
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_panic_hit(void) { return cast_spell(panic_hit_spell); }

void pattern_mindwalk_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Pattern Mindwalking", "パターン・ウォーク"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Walk the pattern in your mind.  Restores life and stats.");
		break;
	case SPELL_CAST:
		msg_print(T("You picture the Pattern in your mind and walk it...", "あなたは「パターン」を心に描いてその上を歩いた..."));

		set_poisoned(0, TRUE);
		set_image(0, TRUE);
		set_stun(0, TRUE);
		set_cut(0, TRUE);
		set_blind(0, TRUE);
		set_afraid(0, TRUE);
		do_res_stat(A_STR);
		do_res_stat(A_INT);
		do_res_stat(A_WIS);
		do_res_stat(A_DEX);
		do_res_stat(A_CON);
		do_res_stat(A_CHR);
		restore_level();
		
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void phase_door_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Phase Door", "ショート・テレポート"));
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

void poison_dart_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Poison Dart", "毒のダーツ"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Fires a poison dart at a single foe.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(p_ptr->lev)));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		msg_print(T("You throw a dart of poison.", "毒のダーツを投げた。"));
		fire_bolt(GF_POIS, dir, p_ptr->lev);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

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

void polymorph_demonlord_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Polymorph Demonlord", "魔王変化"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Mimic a demon lord for a while. Loses abilities of original race and gets great abilities as a demon lord. Even hard walls can't stop your walking.", "悪魔の王に変化する。変化している間は本来の種族の能力を失い、代わりに悪魔の王としての能力を得、壁を破壊しながら歩く。"));
		break;
	case SPELL_INFO:
		var_set_string(res, info_duration(spell_power(15), spell_power(15)));
		break;
	case SPELL_CAST:
	{
		int base = spell_power(15);
		set_mimic(base + randint1(base), MIMIC_DEMON_LORD, FALSE);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void polymorph_self_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Polymorph", "変身"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Mutates yourself.  This can be dangerous!");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("Your body seems mutable.", "体が変異しやすくなった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("Your body seems stable.", "あなたの体は安定したように見える。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can polymorph yourself at will.", "あなたは自分の意志で変化できる。"));
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (get_check(T("You will polymorph yourself. Are you sure? ", "変身します。よろしいですか？")))
		{
			do_poly_self();
			var_set_bool(res, TRUE);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_polymorph_self(void) { return cast_spell(polymorph_self_spell); }

void power_throw_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Throw Object", "アイテム投げ"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Hurl an object with great force.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("Your throwing arm feels much stronger.", "あなたの物を投げる手はかなり強くなった気がする。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("Your throwing arm feels much weaker.", "物を投げる手が弱くなった気がする。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can hurl objects with great force.", "あなたはアイテムを力強く投げることができる。"));
		break;
	case SPELL_COST_EXTRA:
		var_set_int(res, p_ptr->lev);
		break;
	case SPELL_CAST:
		var_set_bool(res, do_cmd_throw_aux(2 + p_ptr->lev / 40, FALSE, 0));
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_power_throw(void) { return cast_spell(power_throw_spell); }

void probing_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Probe Monster", "モンスター調査"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Determines the abilities, strengths and weaknesses of nearby monsters.", ""));
		break;
	case SPELL_CAST:
		probing();
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_probing(void) { return cast_spell(probing_spell); }

void protection_from_evil_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Protection from Evil");
		break;
	case SPELL_DESC:
		var_set_string(res, "Attempts to prevent evil monsters from attacking you.  When a weak evil monster melees you, it may be repelled by the forces of good.");
		break;
	case SPELL_CAST:
		set_protevil(randint1(3 * p_ptr->lev) + 25, FALSE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_protection_from_evil(void) { return cast_spell(protection_from_evil_spell); }

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

void recall_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Recall", "帰還"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Travel back and forth between the town and the dungeon.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You feel briefly homesick, but it passes.", "少しだけホームシックになったが、すぐ直った。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You feel briefly homesick.", "少しの間ホームシックになった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can travel between town and the depths.", "あなたは街とダンジョンの間を行き来することができる。"));
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (word_of_recall())
			var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_recall(void) { return cast_spell(recall_spell); }

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

void remove_curse_I_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Remove Curse");
		break;
	case SPELL_DESC:
		var_set_string(res, "Uncurses an item so that you may remove it.");
		break;
	case SPELL_CAST:
		if (remove_curse())
			msg_print(T("You feel as if someone is watching over you.", "誰かに見守られているような気がする。"));
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_remove_curse_I(void) { return cast_spell(remove_curse_I_spell); }

void remove_curse_II_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "*Remove Curse*");
		break;
	case SPELL_DESC:
		var_set_string(res, "Uncurses an item so that you may remove it.  Even heavily cursed items can be removed.");
		break;
	case SPELL_CAST:
		if (remove_all_curse())
			msg_print(T("You feel as if someone is watching over you.", "誰かに見守られているような気がする。"));
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_remove_curse_II(void) { return cast_spell(remove_curse_II_spell); }

void remove_fear_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Remove Fear", "恐怖除去"));
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_CAST:
		set_afraid(0, TRUE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_remove_fear(void) { return cast_spell(remove_fear_spell); }

void resistance_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Resistance", "全耐性"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Gives resistance to fire, cold, electricity, acid and poison for a while. These resistances can be added to which from equipment for more powerful resistances.", "一定時間、酸、電撃、炎、冷気、毒に対する耐性を得る。装備による耐性に累積する。"));
		break;
	case SPELL_CAST:
	{
		int base = spell_power(20);

		set_oppose_acid(randint1(base) + base, FALSE);
		set_oppose_elec(randint1(base) + base, FALSE);
		set_oppose_fire(randint1(base) + base, FALSE);
		set_oppose_cold(randint1(base) + base, FALSE);
		set_oppose_pois(randint1(base) + base, FALSE);
	
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void resist_elements_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Resist Elements", "エレメント耐性"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Protect yourself from the ravages of the elements.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You feel like you can protect yourself.", "あなたは自分自身を守れる気がする。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You feel like you might be vulnerable.", "傷つき易くなった気がする。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can harden yourself to the ravages of the elements.", "あなたは元素の攻撃に対して身を硬くすることができる。"));
		break;
	case SPELL_COST_EXTRA:
	{
		int n = 0;
		if (p_ptr->lev >= 20)
			n += 5;
		if (p_ptr->lev >= 30)
			n += 5;
		if (p_ptr->lev >= 40)
			n += 5;
		if (p_ptr->lev >= 50)
			n += 5;
		var_set_int(res, n);
		break;
	}
	case SPELL_CAST:
	{
		int num = p_ptr->lev / 10;
		int dur = randint1(20) + 20;

		if (randint0(5) < num)
		{
			set_oppose_acid(dur, FALSE);
			num--;
		}
		if (randint0(4) < num)
		{
			set_oppose_elec(dur, FALSE);
			num--;
		}
		if (randint0(3) < num)
		{
			set_oppose_fire(dur, FALSE);
			num--;
		}
		if (randint0(2) < num)
		{
			set_oppose_cold(dur, FALSE);
			num--;
		}
		if (num)
		{
			set_oppose_pois(dur, FALSE);
			num--;
		}
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_resist_elements(void) { return cast_spell(resist_elements_spell); }

void resist_poison_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Resist Poison", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, "Provides temporary resistance to poison.");
		break;
	case SPELL_CAST:
		set_oppose_pois(randint1(20) + 20, FALSE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void restore_life_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Restore Life");
		break;
	case SPELL_DESC:
		var_set_string(res, "Regain all lost experience.");
		break;
	case SPELL_CAST:
		restore_level();
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_restore_life(void) { return cast_spell(restore_life_spell); }

void rocket_I_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Magic Rocket I", "マジック・ロケット"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires a magic rocket.", "ロケットを発射する。"));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(120 + p_ptr->lev * 2)));
		break;	
	case SPELL_CAST:
	{
		int dir = 0;
		int dam = spell_power(120 + p_ptr->lev * 2);
		int rad = 2;

		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;

		msg_print(T("You launch a rocket!", "ロケット発射！"));
		fire_rocket(GF_ROCKET, dir, dam, rad);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void rocket_II_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Magic Rocket II", "マジック・ロケット"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires a magic rocket of unsurpassable fire power.", "ロケットを発射する。"));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(500)));
		break;	
	case SPELL_CAST:
	{
		int dir = 0;
		int dam = spell_power(500);
		int rad = 2;

		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;

		msg_print(T("You launch a rocket!", "ロケット発射！"));
		fire_rocket(GF_ROCKET, dir, dam, rad);

		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void rush_attack_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Rush Attack", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Charge a nearby monster and attack with your weapons.", ""));
		break;
	case SPELL_CAST:
		var_set_bool(res, rush_attack(NULL));
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void satisfy_hunger_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Satisfy Hunger");
		break;
	case SPELL_DESC:
		var_set_string(res, "Fills your belly with pure yuminess.");
		break;
	case SPELL_CAST:
		set_food(PY_FOOD_MAX - 1);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_satisfy_hunger(void) { return cast_spell(satisfy_hunger_spell); }

void scare_monster_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Scare Monster", "モンスター恐慌"));
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		stop_mouth();
		/*
		msg_print(T("You make a horrible scream!", "身の毛もよだつ叫び声を上げた！");
		msg_print(T("You emit an eldritch howl!", "あなたはおどろおどろしい叫び声をあげた！"));
		*/
		fear_monster(dir, p_ptr->lev);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void shadow_shifting_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Shadow Shifting", "シャドウ・シフト"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Recreates the current dungeon level after a short delay.");
		break;
	case SPELL_CAST:
		msg_print(T("You start walking around.", "あなたは歩き周り始めた。"));
		alter_reality();
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void shoot_arrow_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Shoot Arrow", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires an arrow.", ""));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(2, spell_power(7), 0));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		msg_print(T("You fire an arrow.", "矢を放った。"));
		fire_bolt(GF_ARROW, dir, spell_power(damroll(2, 7)));
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void shriek_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Shriek", "叫び"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Generates a large sound ball centered on you.");
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(p_ptr->lev)));
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("Your vocal cords get much tougher.", "あなたの声は相当強くなった。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("Your vocal cords get much weaker.", "あなたの声質は弱くなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can emit a horrible shriek.", "あなたは身の毛もよだつ叫び声を発することができる。"));
		break;
	case SPELL_CAST:
		stop_mouth();
		fire_ball(GF_SOUND, 0, spell_power(2 * p_ptr->lev), 8);
		aggravate_monsters(0);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_shriek(void) { return cast_spell(shriek_spell); }

void sleeping_dust_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Sleeping Dust", "眠り粉"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("", ""));
		break;
	case SPELL_CAST:
		msg_print(T("You throw some magic dust...", "あなたは魔法の粉を投げつけた..."));
		if (p_ptr->lev < 25) sleep_monsters_touch();
		else sleep_monsters();
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void smell_metal_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Smell Metal", "金属嗅覚"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Smells nearby metallic odors.");
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
		stop_mouth();
		detect_treasure(DETECT_RAD_DEFAULT);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void smell_monsters_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Smell Monsters", "敵臭嗅覚"));
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
		stop_mouth();
		detect_monsters_normal(DETECT_RAD_DEFAULT);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void sp_to_hp_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Convert SP to HP", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Converts SP into HP", ""));
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
	case SPELL_CAST:
		if (p_ptr->csp >= p_ptr->lev / 5)
		{
			p_ptr->csp -= p_ptr->lev / 5;
			p_ptr->redraw |= PR_MANA;
			hp_player(p_ptr->lev);
		}
		else
			msg_print(T("You failed to convert.", "変換に失敗した。"));

		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

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
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(p_ptr->lev * 2)));
		break;
	case SPELL_COST_EXTRA:
		var_set_int(res, p_ptr->lev/5);
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (get_aim_dir(&dir))
		{
			stop_mouth();
			msg_print(T("You spit acid...", "酸を吐きかけた..."));
			if (p_ptr->lev < 25) fire_bolt(GF_ACID, dir, p_ptr->lev);
			else fire_ball(GF_ACID, dir, spell_power(p_ptr->lev * 2), 2);
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

void starburst_I_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Star Burst", "スターバースト"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires a huge ball of powerful light.", "巨大な閃光の球を放つ。"));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(100 + p_ptr->lev * 2)));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		msg_print(T("You invoke a starburst.", "スターバーストの呪文を念じた。"));
		fire_ball(GF_LITE, dir, spell_power(100 + p_ptr->lev * 2), spell_power(4));
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void starburst_II_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Star Burst", "スターバースト"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires a huge ball of powerful light.", "巨大な閃光の球を放つ。"));
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(10, spell_power(10), spell_power(50 + p_ptr->lev * 8)));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		msg_print(T("You invoke a starburst.", "スターバーストの呪文を念じた。"));
		fire_ball(GF_LITE, dir, 
			spell_power(50 + p_ptr->lev * 8 + damroll(10, 10)), 
			spell_power(4));
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void sterility_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Sterility", "増殖阻止"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Stops breeding monsters from ... umm ... doing the nasty.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You can give everything around you a headache.", "周りの全ての者に頭痛を起こすことができる。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You hear a massed sigh of relief.", "たくさんの安堵の吐息が聞こえた。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can cause mass impotence.", "あなたは集団的生殖不能を起こすことができる。"));
		break;
	case SPELL_CAST:
		msg_print(T("You suddenly have a headache!", "突然頭が痛くなった！"));
		take_hit(DAMAGE_LOSELIFE, randint1(17) + 17, T("the strain of forcing abstinence", "禁欲を強いた疲労"), -1);

		/* Fake a population explosion. */
		num_repro += MAX_REPRO;
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_sterility(void) { return cast_spell(sterility_spell); }

void stone_skin_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, "Stone Skin");
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_CAST:
		set_shield(randint1(30) + 20, FALSE);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_stone_skin(void) { return cast_spell(stone_skin_spell); }

void stone_to_mud_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Stone to Mud", "岩石溶解"));
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		wall_to_mud(dir);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_stone_to_mud(void) { return cast_spell(stone_to_mud_spell); }

void summon_manes_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Summon Manes", "古代の死霊召喚"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Attempts to summon some demonic friends.", "古代の死霊を召喚する。"));
		break;
	case SPELL_CAST:
		if (!summon_specific(-1, py, px, (p_ptr->lev * 3) / 2, SUMMON_MANES, (PM_ALLOW_GROUP | PM_FORCE_PET)))
			msg_print(T("No Manes arrive.", "古代の死霊は現れなかった。"));

		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void summon_tree_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		if (p_ptr->lev >= 45)
			var_set_string(res, "Summon Trees");
		else
			var_set_string(res, "Summon Tree");
		break;
	case SPELL_DESC:
		if (p_ptr->lev >= 45)
			var_set_string(res, "Attempts to summon many trees");
		else
			var_set_string(res, "Attempts to summon a tree.");
		break;
	case SPELL_CAST:
		if (p_ptr->lev >= 45)
		{
			tree_creation();
			var_set_bool(res, TRUE);
		}	
		else
		{
			int attempts = 0;
			int x, y, dir;

			var_set_bool(res, TRUE);
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
		}
		break;
	case SPELL_COST_EXTRA:
	{
		int n = 0;
		if (p_ptr->lev >= 45)
			n += 30;

		var_set_int(res, n);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_summon_tree(void) { return cast_spell(summon_tree_spell); }

void super_stealth_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Hide in Darkness", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Grants the stealth of the Ninja!  You may hide in shadows and see in the dark.  Your light radius is decreased by 3.", ""));
		break;
	case SPELL_CAST:
		if (p_ptr->tim_superstealth)
		{
			msg_print("You are already moving in the shadows.");
			var_set_bool(res, FALSE);
		}
		else
		{
			set_tim_superstealth(spell_power(randint1(p_ptr->lev/2) + p_ptr->lev/2), FALSE);
			var_set_bool(res, TRUE);
		}
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}

void swap_pos_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Swap Position", "位置交換"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Swap locations with a given monster.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You feel like walking a mile in someone else's shoes.", "他人の靴で一マイル歩くような気分がする。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You feel like staying in your own shoes.", "あなたは自分の靴に留まる感じがする。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can switch locations with another being.", "あなたは他の者と場所を入れ替わることができる。"));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);

		project_length = -1;
		if (get_aim_dir(&dir))
		{
			teleport_swap(dir);
			var_set_bool(res, TRUE);
		}
		project_length = 0;
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_swap_pos(void) { return cast_spell(swap_pos_spell); }

void sword_dance_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Sword Dancing", "剣の舞い"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Attacks adjacent monsters randomly.");
		break;
	case SPELL_CAST:
	{
		int y = 0, x = 0, i, dir = 0;
		cave_type *c_ptr;

		for (i = 0; i < 6; i++)
		{
			dir = randint0(8);
			y = py + ddy_ddd[dir];
			x = px + ddx_ddd[dir];
			c_ptr = &cave[y][x];

			if (c_ptr->m_idx)
				py_attack(y, x, 0);
			else
				msg_print(T("You attack the empty air.", "攻撃が空をきった。"));
		}
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void take_photo_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Take Photograph", "写真撮影"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Creates something to show the kids back home!", ""));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, TRUE);
		if (!get_aim_dir(&dir)) return;
		project_length = 1;
		fire_beam(GF_PHOTO, dir, 1);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

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

void teleport_other_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Teleport Other", "テレポート・アウェイ"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Teleports all monsters on the line away unless resisted.", "モンスターをテレポートさせるビームを放つ。抵抗されると無効。"));
		break;
	case SPELL_CAST:
	{
		int dir;
		int power = spell_power(p_ptr->lev);

		var_set_bool(res, FALSE);

		if (!get_aim_dir(&dir)) return;
		fire_beam(GF_AWAY_ALL, dir, power);
			
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

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

void teleport_level_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Teleport Level", ""));
		break;
	case SPELL_DESC:
		var_set_string(res, "Escape to another level.");
		break;
	case SPELL_CAST:
		var_set_bool(res, FALSE);
		if (!get_check("Are you sure? (Teleport Level)")) return;
		teleport_level(0);
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_teleport_level(void) { return cast_spell(teleport_level_spell); }

void throw_boulder_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Throw Boulder", "岩石投げ（ダメージ"));
		break;
	case SPELL_DESC:
		var_set_string(res, "");
		break;
	case SPELL_INFO:
		var_set_string(res, info_damage(0, 0, spell_power(3 * p_ptr->lev)));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		msg_print(T("You throw a huge boulder.", "巨大な岩を投げた。"));
		fire_bolt(GF_MISSILE, dir, spell_power(3 * p_ptr->lev));
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

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
		var_set_string(res, info_damage(0, 0, spell_power(p_ptr->lev * 2)));
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
		var_set_bool(res, FALSE);
		if (d_info[dungeon_type].flags1 & DF1_NO_MELEE)
		{
			msg_print(T("Something prevent you from attacking.", "なぜか攻撃することができない。"));
			return;
		}
		else
		{
			int x, y, dummy;
			cave_type *c_ptr;
			int dir = 0;

			/* Only works on adjacent monsters */
			if (!get_rep_dir2(&dir)) break;

			var_set_bool(res, TRUE);

			y = py + ddy[dir];
			x = px + ddx[dir];
			c_ptr = &cave[y][x];

			stop_mouth();

			if (!(c_ptr->m_idx))
			{
				msg_print(T("You bite into thin air!", "何もない場所に噛みついた！"));
				break;
			}

			msg_print(T("You grin and bare your fangs...", "あなたはニヤリとして牙をむいた..."));
			dummy = spell_power(p_ptr->lev * 2);

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
		}
		break;
	case SPELL_COST_EXTRA:
		var_set_int(res, p_ptr->lev / 3);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_vampirism(void) { return cast_spell(vampirism_spell); }

void weigh_magic_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Weigh Magic", "魔力感知"));
		break;
	case SPELL_DESC:
		var_set_string(res, "Determine the strength of magics affecting you.");
		break;
	case SPELL_GAIN_MUT:
		msg_print(T("You feel you can better understand the magic around you.", "あなたは周囲にある魔法をより良く理解できる気がする。"));
		break;
	case SPELL_LOSE_MUT:
		msg_print(T("You no longer sense magic.", "魔力を感じられなくなった。"));
		break;
	case SPELL_MUT_DESC:
		var_set_string(res, T("You can feel the strength of the magics affecting you.", "あなたは自分に影響を与える魔法の力を感じることができる。"));
		break;
	case SPELL_CAST:
		report_magics();
		var_set_bool(res, TRUE);
		break;
	default:
		default_spell(cmd, res);
		break;
	}
}
bool cast_weigh_magic(void) { return cast_spell(weigh_magic_spell); }

void wonder_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Wonder", "ワンダー"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Fires something with random effects.", "モンスターにランダムな効果を与える。"));
		break;
	case SPELL_CAST:
	{
		int dir = 0;
		var_set_bool(res, FALSE);
		if (!get_aim_dir(&dir)) return;
		cast_wonder(dir);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}

void wraithform_spell(int cmd, variant *res)
{
	switch (cmd)
	{
	case SPELL_NAME:
		var_set_string(res, T("Wraithform", "幽体化"));
		break;
	case SPELL_DESC:
		var_set_string(res, T("Leave the world of the living and travel the shadows of the underwold.  You gain passwall and great resistance to damage.", "一定時間、壁を通り抜けることができ受けるダメージが軽減される幽体の状態に変身する。"));
		break;
	case SPELL_INFO:
		var_set_string(res, info_duration(spell_power(p_ptr->lev/2), spell_power(p_ptr->lev/2)));
		break;
	case SPELL_CAST:
	{
		int base = spell_power(p_ptr->lev / 2);
		set_wraith_form(randint1(base) + base, FALSE);
		var_set_bool(res, TRUE);
		break;
	}
	default:
		default_spell(cmd, res);
		break;
	}
}
