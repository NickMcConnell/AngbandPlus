/* File: do-spell.c */

/* Purpose: Do everything for each spell */

#include "angband.h"


/*
 * Generate dice info string such as "foo 2d10"
 */
static cptr info_string_dice(cptr str, int dice, int sides, int base)
{
	/* Fix value */
	if (!dice)
		return format("%s%d", str, base);

	/* Dice only */
	else if (!base)
		return format("%s%dd%d", str, dice, sides);

	/* Dice plus base value */
	else
		return format("%s%dd%d%+d", str, dice, sides, base);
}


/*
 * Generate damage-dice info string such as "dam 2d10"
 */
static cptr info_damage(int dice, int sides, int base)
{
#ifdef JP
	return info_string_dice("損傷:", dice, sides, base);
#else
	return info_string_dice("dam ", dice, sides, base);
#endif
}


/*
 * Generate duration info string such as "dur 20+1d20"
 */
static cptr info_duration(int base, int sides)
{
#ifdef JP
	return format("期間:%d+1d%d", base, sides);
#else
	return format("dur %d+1d%d", base, sides);
#endif
}


/*
 * Generate range info string such as "range 5"
 */
static cptr info_range(int range)
{
#ifdef JP
	return format("範囲:%d", range);
#else
	return format("range %d", range);
#endif
}


/*
 * Generate heal info string such as "heal 2d8"
 */
static cptr info_heal(int dice, int sides, int base)
{
#ifdef JP
	return info_string_dice("回復:", dice, sides, base);
#else
	return info_string_dice("heal ", dice, sides, base);
#endif
}


/*
 * Generate delay info string such as "delay 15+1d15"
 */
static cptr info_delay(int base, int sides)
{
#ifdef JP
	return format("遅延:%d+1d%d", base, sides);
#else
	return format("delay %d+1d%d", base, sides);
#endif
}


/*
 * Generate multiple-damage info string such as "dam 25 each"
 */
static cptr info_multi_damage(int dam)
{
#ifdef JP
	return format("損傷:各%d", dam);
#else
	return format("dam %d each", dam);
#endif
}


/*
 * Generate multiple-damage-dice info string such as "dam 5d2 each"
 */
static cptr info_multi_damage_dice(int dice, int sides)
{
#ifdef JP
	return format("損傷:各%dd%d", dice, sides);
#else
	return format("dam %dd%d each", dice, sides);
#endif
}


/*
 * Generate multiple-damage-dice-base info string such as "dam 10 + d10 each"
 */
static cptr info_multi_damage_dice_base(int base, int sides)
{
#ifdef JP
	return format("損傷:各%d+d%d", base, sides);
#else
	return format("dam %d+d%d each", base, sides);
#endif
}


/*
 * Generate call_the_element info string such as "d (15 + d15)*3"
 */
static cptr info_call_the_elemental(int base, int sides, int attacks)
{
#ifdef JP
	return format("損:(%d + d%d)*%d", base, sides, attacks);
#else
	return format("d (%d + d%d)*%d", base, sides, attacks);
#endif
}


/*
 * Generate power info string such as "power 100"
 */
static cptr info_power(int power)
{
#ifdef JP
	return format("効力:%d", power);
#else
	return format("power %d", power);
#endif
}


/*
 * Generate radius info string such as "rad 100"
 */
static cptr info_radius(int rad)
{
#ifdef JP
	return format("半径:%d", rad);
#else
	return format("rad %d", rad);
#endif
}


void stop_singing(void)
{
	if (!p_ptr->singing && !p_ptr->restart_singing) return;

	if (p_ptr->restart_singing)
	{
		p_ptr->restart_singing = 0;
		return;
	}
	if (!p_ptr->singing) return;

	/* Hack -- if called from set_action(), avoid recursive loop */
	if (p_ptr->action == ACTION_SING) set_action(ACTION_NONE);

	switch (p_ptr->singing)
	{
	case MUSIC_SILENT:
		song_of_silence(0);
		break;
	}

	p_ptr->singing = MUSIC_NONE;
	p_ptr->song_start = 0;

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS | PU_HP);

	/* Redraw status bar */
	p_ptr->redraw |= (PR_STATUS);
}


/*
 * Drop 10+1d10 disintegration ball at random places near the target
 */
static bool cast_wrath_of_the_god(int dam, int rad)
{
	int x, y, tx, ty;
	int dir, i;
	int b = 10 + randint1(10);

	if (!get_aim_dir(&dir)) return FALSE;
	range_restricted_target(dir, MAX_RANGE, &ty, &tx, TRUE);

	for (i = 0; i < b; i++)
	{
		int count = 20, d = 0;

		while (count--)
		{
			int dx, dy;

			x = tx - 5 + randint0(11);
			y = ty - 5 + randint0(11);

			dx = (tx > x) ? (tx - x) : (x - tx);
			dy = (ty > y) ? (ty - y) : (y - ty);

			/* Approximate distance */
			d = (dy > dx) ? (dy + (dx >> 1)) : (dx + (dy >> 1));
			/* Within the radius */
			if (d < 5) break;
		}

		if (count < 0) continue;

		/* Cannot penetrate perm walls */
		if (!in_bounds(y,x) ||
		    cave_stop_disintegration(y,x) ||
		    !in_disintegration_range(ty, tx, y, x))
			continue;

		project(0, rad, y, x, dam, GF_DISINTEGRATE, PROJECT_JUMP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL, MODIFY_ELEM_MODE_MAGIC);
	}

	return TRUE;
}


static bool cast_stop_the_time(void)
{
	if (stop_the_time_player)
	{
#ifdef JP
		msg_print("既に時は止まっている。");
#else
		msg_print("Time is already stopped.");
#endif
		return FALSE;
	}
	stop_the_time_player = TRUE;
#ifdef JP
	msg_print("時を止めた！");
#else
	msg_print("You stopped the time!");
#endif
	msg_print(NULL);

	/* Hack */
	p_ptr->energy_need -= 1000 + (100 * (4 + damroll(2, 2)))*TURNS_PER_TICK/10;

	/* Redraw map */
	p_ptr->redraw |= (PR_MAP);

	/* Update monsters */
	p_ptr->update |= (PU_MONSTERS);

	/* Window stuff */
	p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

	handle_stuff();

	return TRUE;
}


static bool cast_melt_weapon(int power)
{
	int tx, ty;

#ifdef JP
	msg_print("誰の攻撃力を弱めるか指定して下さい。");
#else
	msg_print("Choose the target to weaken.");
#endif
	if (!tgt_pt(&tx, &ty, FALSE)) return FALSE;
	if (!player_has_los_bold(ty, tx))
	{
#ifdef JP
		msg_print("その場所を指定することはできません。");
#else
		msg_print("You can't specify that place.");
#endif
		return FALSE;
	}

	if (!cave[ty][tx].m_idx)
	{
#ifdef JP
		msg_print("そこにはモンスターがいません。");
#else
		msg_print("There is no monster.");
#endif
		return FALSE;
	}

	if (cave[ty][tx].m_idx)
	{
		monster_type *m_ptr = &m_list[cave[ty][tx].m_idx];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		char m_name[80];

		monster_desc(m_name, m_ptr, 0);
		if (r_ptr->level > randint1((power - 10) < 1 ? 1 : (power - 10)) + 10)
		{
#ifdef JP
			msg_format("%^sには効果がなかった！", m_name);
#else
			msg_format("%^s is unaffected!", m_name);
#endif
		}
		else if (!MON_MELT_WEAPON(m_ptr))
		{
#ifdef JP
			msg_format("%^sの攻撃力が弱まったようだ。", m_name);
#else
			msg_format("Damage of %^s is seems to weakened.", m_name);
#endif
			(void)set_monster_melt_weapon(cave[ty][tx].m_idx, power);
		}
	}
	return TRUE;
}


static bool cast_dispel_magic(void)
{
	int tx, ty;
	int dir;

#ifdef JP
	msg_print("誰の魔力を消去するか指定して下さい。");
#else
	msg_print("Choose the target to dispel.");
#endif
	if (!get_aim_dir(&dir)) return FALSE;
	range_restricted_target(dir, MAX_RANGE, &ty, &tx, FALSE);

	if (!((ty == py) && (tx == px)) && !cave[ty][tx].m_idx)
	{
#ifdef JP
		msg_print("そこには誰もいません。");
#else
		msg_print("There is no one.");
#endif
		return FALSE;
	}

	if ((ty == py) && (tx == px))
	{
		dispel_player();

		if (p_ptr->riding) dispel_monster_status(p_ptr->riding);
	}
	else if (cave[ty][tx].m_idx)
	{
		dispel_monster_status(cave[ty][tx].m_idx);
	}
	return TRUE;
}


static bool cast_change_element(int power)
{
	int tx, ty;
	int dir;

#ifdef JP
	msg_print("誰のエレメントを反転させるか指定して下さい。");
#else
	msg_print("Choose the target to revert elements.");
#endif
	if (!get_aim_dir(&dir)) return FALSE;
	range_restricted_target(dir, MAX_RANGE, &ty, &tx, FALSE);

	if (!((ty == py) && (tx == px)) && !cave[ty][tx].m_idx)
	{
#ifdef JP
		msg_print("そこには誰もいません。");
#else
		msg_print("There is no one.");
#endif
		return FALSE;
	}

	if ((ty == py) && (tx == px))
	{
		if (!p_ptr->opposite_pelem) set_opposite_pelem(power);
	}
	else if (cave[ty][tx].m_idx)
	{
		monster_type *m_ptr = &m_list[cave[ty][tx].m_idx];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		char m_name[80];

		monster_desc(m_name, m_ptr, 0);
		if (r_ptr->level > randint1((power - 10) < 1 ? 1 : (power - 10)) + 10)
		{
#ifdef JP
			msg_format("%^sには効果がなかった！", m_name);
#else
			msg_format("%^s is unaffected!", m_name);
#endif
		}
		else if (!MON_OPPOSITE_ELEM(m_ptr))
		{
#ifdef JP
			msg_format("%^sのエレメントが反転した。", m_name);
#else
			msg_format("Elements of %^s are reverted.", m_name);
#endif
			(void)set_monster_opposite_elem(cave[ty][tx].m_idx, power);
			if (p_ptr->action == ACTION_ELEMSCOPE) lite_spot(m_ptr->fy, m_ptr->fx);
		}
	}
	return TRUE;
}


static cptr do_magery_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

	int dir;
	int mlev = p_ptr->magic_exp[REALM_MAGERY]/10;

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "マジック・ミサイル";
		if (desc) return "弱い魔法のボルトを放つ。";
#else
		if (name) return "Magic Missile";
		if (desc) return "Fires a weak bolt of magic.";
#endif
    
		{
			int dice = 3 + ((mlev - 1) / 5);
			int sides = 4;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_bolt(GF_MISSILE, dir, damroll(dice, sides));
			}
		}
		break;

	case 1:
#ifdef JP
		if (name) return "モンスター感知";
		if (desc) return "近くの全ての見えるモンスターを感知する。";
#else
		if (name) return "Detect Monsters";
		if (desc) return "Detects all monsters in your vicinity unless invisible.";
#endif
    
		{
			int rad = DETECT_RAD_DEFAULT;

			if (info) return info_radius(rad);

			if (cast)
			{
				detect_monsters_normal(rad);
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "ショート・テレポート";
		if (desc) return "近距離のテレポートをする。";
#else
		if (name) return "Phase Door";
		if (desc) return "Teleport short distance.";
#endif
    
		{
			int range = 10;

			if (info) return info_range(range);

			if (cast)
			{
				teleport_player(range);
			}
		}
		break;

	case 3:
#ifdef JP
		if (name) return "罠と扉感知";
		if (desc) return "近くの全ての罠と扉と階段を感知する。";
#else
		if (name) return "Detect Doors and Traps";
		if (desc) return "Detects traps, doors, and stairs in your vicinity.";
#endif
    
		{
			int rad = DETECT_RAD_DEFAULT;

			if (info) return info_radius(rad);

			if (cast)
			{
				detect_traps(rad, TRUE);
				detect_doors(rad);
				detect_stairs(rad);
			}
		}
		break;

	case 4:
#ifdef JP
		if (name) return "ライト・エリア";
		if (desc) return "光源が照らしている範囲か部屋全体を永久に明るくする。";
#else
		if (name) return "Light Area";
		if (desc) return "Lights up nearby area and the inside of a room permanently.";
#endif
    
		{
			int dice = 2;
			int sides = mlev / 2;
			int rad = mlev / 10 + 1;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				lite_area(damroll(dice, sides), rad);
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "罠／扉破壊";
		if (desc) return "一直線上の全ての罠と扉を破壊する。";
#else
		if (name) return "Trap & Door Destruction";
		if (desc) return "Fires a beam which destroy traps and doors.";
#endif
    
		{
			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				destroy_door(dir);
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "テレポート";
		if (desc) return "遠距離のテレポートをする。";
#else
		if (name) return "Teleport";
		if (desc) return "Teleport long distance.";
#endif
    
		{
			int range = mlev * 5;

			if (info) return info_range(range);

			if (cast)
			{
				teleport_player(range);
			}
		}
		break;

	case 7:
#ifdef JP
		if (name) return "魔力炸裂";
		if (desc) return "魔法の球を放つ。";
#else
		if (name) return "Mana Burst";
		if (desc) return "Fires a ball of magic.";
#endif
    
		{
			int dice = 3;
			int sides = 5;
			int rad = (mlev < 30) ? 2 : 3;
			int base;

			if (p_ptr->pclass == CLASS_WIZARD ||
			    p_ptr->pclass == CLASS_SIRENE ||
			    p_ptr->pclass == CLASS_LICH ||
			    p_ptr->pclass == CLASS_ARCHMAGE)
				base = mlev + mlev / 2;
			else
				base = mlev + mlev / 4;


			if (info) return info_damage(dice, sides, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_MISSILE, dir, damroll(dice, sides) + base, rad, FALSE);

				/*
				 * Shouldn't actually use GF_MANA, as
				 * it will destroy all items on the
				 * floor
				 */
			}
		}
		break;

	case 8:
#ifdef JP
		if (name) return "岩石溶解";
		if (desc) return "壁を溶かして床にする。";
#else
		if (name) return "Stone to Mud";
		if (desc) return "Turns one rock square to mud.";
#endif
    
		{
			int dice = 3 + ((mlev - 5) / 4);
			int sides = 8;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				wall_to_mud(dir);
			}
		}
		break;

	case 9:
#ifdef JP
		if (name) return "魔法の地図";
		if (desc) return "周辺の地形を感知する。";
#else
		if (name) return "Magic Mapping";
		if (desc) return "Maps nearby area.";
#endif
    
		{
			int rad = DETECT_RAD_MAP;

			if (info) return info_radius(rad);

			if (cast)
			{
				map_area(rad);
			}
		}
		break;

	case 10:
#ifdef JP
		if (name) return "鑑定";
		if (desc) return "アイテムを1つ識別する。レベル35でアイテムの能力を完全に知ることができる。";
#else
		if (name) return "Identify";
		if (desc) return "Identifies an item.";
#endif
    
		{
			if (cast)
			{
				if (mlev < 35)
				{
					if (!ident_spell(FALSE)) return NULL;
				}
				else
				{
					if (!identify_fully(FALSE)) return NULL;
				}
			}
		}
		break;

	case 11:
#ifdef JP
		if (name) return "精神感知";
		if (desc) return "一定時間、テレパシー能力を得る。";
#else
		if (name) return "Sense Minds";
		if (desc) return "Gives telepathy for a while.";
#endif
    
		{
			int base = 25;
			int sides = 30;

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_tim_esp(randint1(sides) + base, FALSE);
			}
		}
		break;

	case 12:
#ifdef JP
		if (name) return "魔力の矢";
		if (desc) return "純粋な魔力のボルトを放つ。";
#else
		if (name) return "Mana Bolt";
		if (desc) return "Fires a beam of pure mana.";
#endif
    
		{
			int dice = 11 + (mlev - 5) / 4;
			int sides = 8;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_bolt(GF_MANA, dir, damroll(dice, sides));
			}
		}
		break;

	case 13:
#ifdef JP
		if (name) return "空腹充足";
		if (desc) return "満腹にする。";
#else
		if (name) return "Satisfy Hunger";
		if (desc) return "Satisfies hunger.";
#endif
    
		{
			if (cast)
			{
				set_food(PY_FOOD_MAX - 1);
			}
		}
		break;

	case 14:
#ifdef JP
		if (name) return "帰還の呪文";
		if (desc) return "地上にいるときはダンジョンの最深階へ、ダンジョンにいるときは地上へと移動する。";
#else
		if (name) return "Word of Recall";
		if (desc) return "Recalls player from dungeon to town, or from town to the deepest level of dungeon.";
#endif
    
		{
			int base = 15;
			int sides = 20;

			if (info) return info_delay(base, sides);

			if (cast)
			{
				if (!word_of_recall()) return NULL;
			}
		}
		break;

	case 15:
#ifdef JP
		if (name) return "調査";
		if (desc) return "モンスターの属性、残り体力、最大体力、スピード、正体を知る。";
#else
		if (name) return "Probing";
		if (desc) return "Proves all monsters' alignment, HP, speed and their true character.";
#endif
    
		{
			if (cast)
			{
				probing();
			}
		}
		break;

	case 16:
#ifdef JP
		if (name) return "魔力充填";
		if (desc) return "杖/魔法棒の充填回数を増やすか、充填中のロッドの充填時間を減らす。";
#else
		if (name) return "Recharging";
		if (desc) return "Recharges staffs, wands or rods.";
#endif
    
		{
			int power;
			if ((p_ptr->pclass == CLASS_WITCH) || (p_ptr->pclass == CLASS_HIGHWITCH)) power = mlev * 8;
			else power = mlev * 4;

			if (info) return info_power(power);

			if (cast)
			{
				if (!recharge(power)) return NULL;
			}
		}
		break;

	case 17:
#ifdef JP
		if (name) return "テレポート・レベル";
		if (desc) return "瞬時に上か下の階にテレポートする。";
#else
		if (name) return "Teleport Level";
		if (desc) return "Teleport to up or down stairs in a moment.";
#endif
    
		{
			if (cast)
			{
#ifdef JP
				if (!get_check("本当に他の階にテレポートしますか？")) return NULL;
#else
				if (!get_check("Are you sure? (Teleport Level)")) return NULL;
#endif
				teleport_level(0);
			}
		}
		break;

	case 18:
#ifdef JP
		if (name) return "テレポート・アウェイ";
		if (desc) return "一直線上のモンスターをテレポートさせる。抵抗されると無効。";
#else
		if (name) return "Teleport Away";
		if (desc) return "Teleports all monsters on the line away unless resisted.";
#endif
    
		{
			int power = mlev;

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_beam(GF_AWAY_ALL, dir, power);
			}
		}
		break;

	case 19:
#ifdef JP
		if (name) return "全感知";
		if (desc) return "近くの全てのモンスター、罠、扉、階段、財宝、そしてアイテムを感知する。";
#else
		if (name) return "Detection True";
		if (desc) return "Detects all monsters, traps, doors, stairs, treasures and items in your vicinity.";
#endif
    
		{
			int rad = DETECT_RAD_DEFAULT;

			if (info) return info_radius(rad);

			if (cast)
			{
				detect_all(rad);
			}
		}
		break;

	case 20:
#ifdef JP
		if (name) return "魔法ロケット";
		if (desc) return "ロケットを発射する。";
#else
		if (name) return "Magic Rocket";
		if (desc) return "Fires a magic rocket.";
#endif
    
		{
			int dam = 120 + mlev * 2;
			int rad = 2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

#ifdef JP
				msg_print("ロケット発射！");
#else
				msg_print("You launch a rocket!");
#endif

				fire_rocket(GF_ROCKET, dir, dam, rad, FALSE);
			}
		}
		break;

	case 21:
#ifdef JP
		if (name) return "千里眼";
		if (desc) return "その階全体を永久に照らし、ダンジョン内すべてのアイテムを感知する。さらに、一定時間テレパシー能力を得る。";
#else
		if (name) return "Clairvoyance";
		if (desc) return "Maps and lights whole dungeon level. Knows all objects location. And gives telepathy for a while.";
#endif
    
		{
			int base = 25;
			int sides = 30;

			if (info) return info_duration(base, sides);

			if (cast)
			{
				wiz_lite(FALSE);

				if (!p_ptr->telepathy)
				{
					set_tim_esp(randint1(sides) + base, FALSE);
				}
			}
		}
		break;

	case 22:
#ifdef JP
		if (name) return "悪魔召喚";
		if (desc) return "悪魔を1体召喚する。";
#else
		if (name) return "Summon Demon";
		if (desc) return "Summons a demon.";
#endif
    
		{
			if (cast)
			{
				u32b mode = 0L;
				bool pet = !one_in_(3);

				if (pet) mode |= PM_FORCE_PET;
				else mode |= (PM_NO_PET | PM_IGNORE_AMGRID);

				if (summon_specific((pet ? -1 : 0), py, px, (mlev * 3) / 2, SUMMON_DEMON, mode))
				{
#ifdef JP
					msg_print("硫黄の悪臭が充満した。");
#else
					msg_print("The area fills with a stench of sulphur and brimstone.");
#endif

					if (pet)
					{
#ifdef JP
						msg_print("「ご用でございますか、ご主人様」");
#else
						msg_print("'What is thy bidding... Master?'");
#endif
					}
					else
					{
#ifdef JP
						msg_print("「卑しき者よ、我は汝の下僕にあらず！ お前の魂を頂くぞ！」");
#else
						msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
#endif
					}
				}
			}
		}
		break;

	case 23:
#ifdef JP
		if (name) return "街移動";
		if (desc) return "街へ移動する。地上にいるときしか使えない。";
#else
		if (name) return "Teleport to town";
		if (desc) return "Teleport to a town which you choose in a moment. Can only be used outdoors.";
#endif
    
		{
			if (cast)
			{
				if (!tele_town(TRUE)) return NULL;
			}
		}
		break;

	case 24:
#ifdef JP
		if (name) return "古代の龍召喚";
		if (desc) return "1体の古代ドラゴンを召喚する。";
#else
		if (name) return "Trump Ancient Dragon";
		if (desc) return "Summons an ancient dragon.";
#endif
    
		{
			if (cast)
			{
				if (!summon_specific(-1, py, px, mlev * 2 / 3 + randint1(mlev/2), SUMMON_HI_DRAGON, PM_FORCE_PET))
				{
#ifdef JP
					msg_print("古代ドラゴンは現れなかった。");
#else
					msg_print("No ancient dragons arrive.");
#endif
				}
			}
		}
		break;

	case 25:
#ifdef JP
		if (name) return "魔力の嵐";
		if (desc) return "非常に強力で巨大な純粋な魔力の球を放つ。";
#else
		if (name) return "Mana Storm";
		if (desc) return "Fires an extremely powerful huge ball of pure mana.";
#endif
    
		{
			int dam = 300 + mlev * 4;
			int rad = 4;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_MANA, dir, dam, rad, FALSE);
			}
		}
		break;

	}

	return "";
}


static cptr do_fire_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

	int dir;
	int mlev = p_ptr->magic_exp[REALM_FIRE]/10;
	int pstat = p_ptr->stat_use[A_INT];

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "スパークスフィア";
		if (desc) return "プラズマのビームを放つ。";
#else
		if (name) return "Spark sphere";
		if (desc) return "Fires a bolt of plasma.";
#endif
    
		{
			int dice = 3 + (mlev - 1) / 5;
			int sides = 4;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_beam(GF_PLASMA, dir, damroll(dice, sides));
			}
		}
		break;

	case 1:
#ifdef JP
		if (name) return "デフゾショネル";
		if (desc) return "光源が照らしている範囲か部屋全体の火のエレメントを強め、水のエレメントを弱める。";
#else
		if (name) return "Deft-Zoshonell";
		if (desc) return "Increase FIRE, decrease AQUA nearby area and the inside of a room permanently.";
#endif
    
		{
			int amount = 2;
			int rad = (mlev / 10) + 1;

			if (pstat >= (18 + 100)) amount++;
			if (pstat >= (18 + 150)) amount++;
			if (pstat >= (18 + 200)) amount++;

			if (info) return info_radius(rad);

			if (cast)
			{
				inc_area_elem(0, ELEM_FIRE, amount, rad, TRUE);
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "ファイア・ボルト";
		if (desc) return "炎のボルトを放つ。";
#else
		if (name) return "Fire bolt";
		if (desc) return "Fires a bolt of fire.";
#endif
    
		{
			int dice = 8 + (mlev - 1) / 5;
			int sides = 8;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_bolt(GF_FIRE, dir, damroll(dice, sides));
			}
		}
		break;

	case 3:
#ifdef JP
		if (name) return "スタンスローター";
		if (desc) return "麻痺の球を放つ。";
#else
		if (name) return "Stun slaughter";
		if (desc) return "Fires a ball of stun.";
#endif
    
		{
			int power = mlev + 50;
			int rad = 1;
			if (pstat >= (18 + 100))
			{
				power = mlev + 60;
				rad++;
			}
			if (pstat >= (18 + 150))
			{
				power = mlev + 70;
				rad++;
			}

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_STASIS, dir, power, rad, FALSE);
			}
		}
		break;

	case 4:
#ifdef JP
		if (name) return "ファイアストーム";
		if (desc) return "指定した地点から一定範囲に火炎攻撃を行う。";
#else
		if (name) return "Fire storm";
		if (desc) return "Fires a ball of fire.";
#endif
    
		{
			int sides = mlev;
			int base = 10 + mlev * 2;
			int rad = (pstat >= (18 + 150)) ? 3 : 2;

			if (info) return info_damage(0, 0, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_FIRE, dir, base + randint1(sides), rad, TRUE);
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "クリアスカイ";
		if (desc) return "天候を良化させる。";
#else
		if (name) return "Clear sky";
		if (desc) return "weather to fine.";
#endif
    
		{
			int power = -2;
			if (pstat >= (18 + 150)) power--;
			if (pstat >= (18 + 200)) power--;

			if (cast)
			{
				msg_print("天候が穏やかになっていく...");
				set_weather(power, power, power);
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "ヒートウェポン";
		if (desc) return "武器に炎の属性をつける。";
#else
		if (name) return "Heat weapon";
		if (desc) return "Makes current weapon fire branded.";
#endif
    
		{
			if (cast)
			{
				brand_weapon(EGO_ZOSHONELL);
			}
		}
		break;

	case 7:
#ifdef JP
		if (name) return "不滅の肉体";
		if (desc) return "一定時間、時間逆転への耐性を得る。";
#else
		if (name) return "Immortal Body";
		if (desc) return "Gives resistance to time for a while.";
#endif
    
		{
			int sides = 20;
			int base = 20;

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_tim_res_time(randint1(sides)+base, FALSE);
			}
		}
		break;

	case 8:
#ifdef JP
		if (name) return "サラマンダー";
		if (desc) return "指定した地点から一定範囲内のモンスターに無差別に火炎攻撃を繰り返す。";
#else
		if (name) return "Salamander";
		if (desc) return "Fires a bolt or beam of fire.";
#endif
    
		{
			int base = mlev;
			int attacks = 3;
			if (pstat >= (18 + 100)) attacks++;
			if (pstat >= (18 + 150)) attacks++;
			if (pstat >= (18 + 180)) attacks++;
			if (pstat >= (18 + 200)) attacks++;
			if (pstat >= (18 + 220)) attacks++;

			if (info) return info_call_the_elemental(base, base, attacks);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

#ifdef JP
				msg_format("精霊サラマンダーを召喚した。");
#else
				msg_format("You called the Salamander.");
#endif
				cast_call_the_elemental(GF_FIRE, dir, base, 1, base, 3, A_INT);
			}
		}
		break;

	case 9:
#ifdef JP
		if (name) return "クリムゾンノート";
		if (desc) return "プラズマのブレスを吐く。";
#else
		if (name) return "Crimson Naught";
		if (desc) return "Breath of plasma.";
#endif
    
		{
			int base = MIN(p_ptr->chp / 2, 350);

			if (info) return info_damage(0, 0, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_PLASMA, dir, base, -2, FALSE);
			}
		}
		break;

	case 10:
#ifdef JP
		if (name) return "スルトの狂焔";
		if (desc) return "ターゲットの周囲に*火炎*の球を多数発生させる。";
#else
		if (name) return "Fire of Surtr";
		if (desc) return "Fires balls of *fire*.";
#endif
    
		{
			int dam = mlev * 3 + 25;

			if (info) return info_multi_damage(dam);

			if (cast)
			{
				int x, y, tx, ty;
				int dir, i;
				int b = 5 + randint1(10);

				if (!get_aim_dir(&dir)) return NULL;

				range_restricted_target(dir, MAX_RANGE, &ty, &tx, TRUE);

				for (i = 0; i < b; i++)
				{
					int count = 20, d = 0;

					while (count--)
					{
						int dx, dy;

						x = tx - 5 + randint0(11);
						y = ty - 5 + randint0(11);

						dx = (tx > x) ? (tx - x) : (x - tx);
						dy = (ty > y) ? (ty - y) : (y - ty);

						/* Approximate distance */
						d = (dy > dx) ? (dy + (dx >> 1)) : (dx + (dy >> 1));
						/* Within the radius */
						if (d < 5) break;
					}

					if (count < 0) continue;

					/* Cannot penetrate walls */
					if (!in_bounds(y, x) || !los(ty, tx, y, x)) continue;

					project(0, 2, y, x, dam, GF_PURE_FIRE, PROJECT_JUMP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL, MODIFY_ELEM_MODE_MAGIC);
				}
			}
		}
		break;

	case 11:
#ifdef JP
		if (name) return "ストライクノヴァ";
		if (desc) return "火炎耐性のないモンスター1体に大ダメージを与える。火炎耐性があるモンスターには無効。";
#else
		if (name) return "Strike Nova";
		if (desc) return "";
#endif
    
		{
			int base = 999;

			if (info) return info_damage(0, 0, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_STRIKE_NOVA, dir, base, 0, FALSE);
			}
		}
		break;

	case 12:
#ifdef JP
		if (name) return "ゾショネルの加護";
		if (desc) return "一定時間、火炎のオーラと火炎とプラズマへの免疫が付き、HPと腕力と器用さと打撃攻撃能力が上昇し、加速するが、冷気と水と*水*に弱くなる。";
#else
		if (name) return "Zoshonell protect";
		if (desc) return "Gives immunity to fire and plasma, fire aura,  for a while..";
#endif
    
		{
			int sides = 10;
			int base = 10;

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_zoshonel_protect(randint1(sides) + base, FALSE);
			}
		}
		break;

	case 13:
#ifdef JP
		if (name) return "スーパーノヴァ";
		if (desc) return "視界内の範囲に対してプラズマ攻撃を行い、自分もダメージを受ける。さらに、天候を快晴・酷暑にする。禁呪を使用する者は人心を失う。";
#else
		if (name) return "Super Nova";
		if (desc) return "Damage by plasma in sight, weather to fine.";
#endif
    
		{
			int base = mlev * 8;
			int sides = mlev * 5;

			if (info) return info_multi_damage_dice_base(base, sides);

			if (cast)
			{
				project(0, MAX_SIGHT, py, px, base + randint1(sides), GF_PLASMA, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
#ifdef JP
				take_hit(DAMAGE_NOESCAPE, base + randint1(sides), "スーパーノヴァの呪文の巻き添え");
#else
				take_hit(DAMAGE_NOESCAPE, base + randint1(sides), "the strain of casting Super Nova");
#endif

				set_weather(-15, 0, -15);
				change_chaos_frame(ETHNICITY_WALSTANIAN, -1);
				change_chaos_frame(ETHNICITY_GARGASTAN, -1);
				change_chaos_frame(ETHNICITY_BACRUM, -1);
			}
		}
		break;


	}

	return "";
}


static cptr do_aqua_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

	int dir;
	int mlev = p_ptr->magic_exp[REALM_AQUA]/10;
	int pstat = p_ptr->stat_use[A_INT];

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "デフグルーザ";
		if (desc) return "光源が照らしている範囲か部屋全体の水のエレメントを強め、火のエレメントを弱める。";
#else
		if (name) return "Deft Gruza";
		if (desc) return "Increase FIRE, decrease AQUA nearby area and the inside of a room permanently.";
#endif
    
		{
			int amount = 2;
			int rad = (mlev / 10) + 1;

			if (pstat >= (18 + 100)) amount++;
			if (pstat >= (18 + 150)) amount++;
			if (pstat >= (18 + 200)) amount++;


			if (info) return info_radius(rad);

			if (cast)
			{
				inc_area_elem(0, ELEM_AQUA, amount, rad, TRUE);
			}
		}
		break;

	case 1:
#ifdef JP
		if (name) return "極寒の矢";
		if (desc) return "極寒のボルトを放つ。";
#else
		if (name) return "Ice bolt";
		if (desc) return "Fires a bolt of ice.";
#endif
    
		{
			int dice = 6 + (mlev - 1) / 5;
			int sides = 8;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_beam(GF_ICE, dir, damroll(dice, sides));
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "アイスブラスト";
		if (desc) return "指定した地点から一定範囲に冷気攻撃を行う。";
#else
		if (name) return "Ice Blast";
		if (desc) return "Fires a ball of cold.";
#endif
    
		{
			int sides = mlev;
			int base = 10 + mlev * 2;
			int rad = (pstat >= (18 + 150)) ? 3 : 2;

			if (info) return info_damage(1, sides, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_COLD, dir, base + randint1(sides), rad, TRUE);
			}
		}
		break;

	case 3:
#ifdef JP
		if (name) return "クリアブラッド";
		if (desc) return "怪我を全快させ、毒を体から完全に取り除き、体力を少し回復させる。";
#else
		if (name) return "Clear blood";
		if (desc) return "Heals all cut and poison status. Heals HP a little.";
#endif
    
		{
			int dice = 4;
			int sides = 8;

			if (info) return info_heal(dice, sides, 0);

			if (cast)
			{
				set_poisoned(0);
				set_cut(0);
				hp_player(damroll(dice, sides));
			}
		}
		break;

	case 4:
#ifdef JP
		if (name) return "スロウムーブ";
		if (desc) return "モンスター1体を減速させる。抵抗されると無効。";
#else
		if (name) return "Slow Monster";
		if (desc) return "Attempts to slow a monster.";
#endif
    
		{
			int power = mlev;

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				slow_monster(dir, power);
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "絶対零度";
		if (desc) return "一定時間、冷気のオーラを得る。";
#else
		if (name) return "Absolute zero";
		if (desc) return "Gives aura of cold for a while.";
#endif
    
		{
			int sides = 25 + mlev;
			int base = mlev;

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_tim_sh_cold(randint1(sides)+base, FALSE);
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "ウォーター・ボール";
		if (desc) return "水の球を放つ。";
#else
		if (name) return "Water ball";
		if (desc) return "Fires a ball of water.";
#endif
    
		{
			int base = 100 + mlev * 3 / 2;
			int rad = (mlev / 12) + 1;

			if (info) return info_damage(0, 0, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_WATER, dir, base, rad, TRUE);
			}
		}
		break;

	case 7:
#ifdef JP
		if (name) return "ポイズンハザード";
		if (desc) return "自分を中心とした一定範囲に水、毒、混乱攻撃を行う。知能が高いと劣化、生命力吸収攻撃も行う。";
#else
		if (name) return "Poison hazard";
		if (desc) return "";
#endif
    
		{
			int rad = mlev / 10;
			int dam = 15 + mlev / 2;
			int attacks = 3;
			bool disenchant = FALSE;
			bool drain = FALSE;

			if (pstat >= (18 + 150))
			{
				disenchant = TRUE;
				attacks++;
			}
			if (pstat >= (18 + 200))
			{
				drain = TRUE;
				attacks++;
			}

#ifdef JP
			if (info) return format("損傷:%d*%d", dam, attacks);
#else
			if (info) return format("dam %d*%d", dam, attacks);
#endif

			if (cast)
			{
				project(0, rad, py, px, dam, GF_WATER, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
				project(0, rad, py, px, dam, GF_POIS, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
				project(0, rad, py, px, dam, GF_CONFUSION, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
				if (disenchant) project(0, rad, py, px, dam, GF_DISENCHANT, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
				if (drain) project(0, rad, py, px, dam, GF_OLD_DRAIN, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
			}
		}
		break;

	case 8:
#ifdef JP
		if (name) return "潜水";
		if (desc) return "水のある場所から水のある場所へ移動する。距離が遠すぎると距離に応じてダメージを受ける。";
#else
		if (name) return "Diving";
		if (desc) return "Teleport to water spot from water spot.";
#endif
    
		{
			int range = (prace_is_(RACE_MERMAID)) ? mlev + 20 : mlev / 2 + 10;

			if (info) return info_range(range);

			if (cast)
			{
				if (!aqua_diving(mlev)) return NULL;
			}
		}
		break;

	case 9:
#ifdef JP
		if (name) return "フェンリル";
		if (desc) return "指定した地点から一定範囲内のモンスターに無差別に冷気攻撃を繰り返す。";
#else
		if (name) return "Fenrer";
		if (desc) return "";
#endif
    
		{
			int base = mlev;
			int attacks = 3;
			if (pstat >= (18 + 100)) attacks++;
			if (pstat >= (18 + 150)) attacks++;
			if (pstat >= (18 + 180)) attacks++;
			if (pstat >= (18 + 200)) attacks++;
			if (pstat >= (18 + 220)) attacks++;

			if (info) return info_call_the_elemental(base, base, attacks);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

#ifdef JP
				msg_format("精霊フェンリルを召喚した。");
#else
				msg_format("You called the Fenrer.");
#endif
				cast_call_the_elemental(GF_COLD, dir, base, 1, base, 3, A_INT);
			}
		}
		break;

	case 10:
#ifdef JP
		if (name) return "マーシーレイン";
		if (desc) return "自分を中心とした一定範囲に体力を大幅に回復させ、傷、毒、朦朧、混乱から全快させる雨を降らせる。";
#else
		if (name) return "Mercy Rain";
		if (desc) return "";
#endif
    
		{
			int base = 500;

			if (info) return info_heal(0, 0, base);

			if (cast)
			{
				hp_player(base);
				project(0, 3, py, px, base, GF_OLD_HEAL, PROJECT_KILL | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_NONE);
				set_poisoned(0);
				set_confused(0);
				set_stun(0);
				set_cut(0);
			}
		}
		break;

	case 11:
#ifdef JP
		if (name) return "大洪水";
		if (desc) return "現在の階を再構成する。再構成された階は水浸しになる。1度使うとしばらくの間は使えない。";
#else
		if (name) return "Water flood";
		if (desc) return "Recreates current dungeon level by water flood.";
#endif
    
		{
			if (cast)
			{
				if (!alter_with_flood()) return NULL;
			}
		}
		break;

	case 12:
#ifdef JP
		if (name) return "ブルー・スパイラル";
		if (desc) return "強力でごく小さな水の球、ごく小さな魔力吸収の球で攻撃する。魔力を持つモンスターからはMPを吸収する。";
#else
		if (name) return "Blue spiral";
		if (desc) return "Fires a ball of water.";
#endif
    
		{
			int dice = 10;
			int sides = mlev;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_WATER, dir, damroll(dice, sides), 0, FALSE);
				fire_ball(GF_DRAIN_MANA, dir, damroll(2, mlev), 0, FALSE);
			}
		}
		break;

	case 13:
#ifdef JP
		if (name) return "アイスレクイエム";
		if (desc) return "視界内の範囲に対して極寒攻撃を行い、自分もダメージを受ける。さらに、天候を豪雪にする。禁呪を使用する者は人心を失う。";
#else
		if (name) return "Ice Requiem";
		if (desc) return "Attack by ice in sight.";
#endif
    
		{
			int base = mlev * 8;
			int sides = mlev * 5;

			if (info) return info_multi_damage_dice_base(base, sides);

			if (cast)
			{
				project(0, MAX_SIGHT, py, px, base + randint1(sides), GF_ICE, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
#ifdef JP
				take_hit(DAMAGE_NOESCAPE, base + randint1(sides), "アイスレクイエムの呪文の巻き添え");
#else
				take_hit(DAMAGE_NOESCAPE, base + randint1(sides), "the strain of casting Ice Requiem");
#endif

				set_weather(15, 0, 15);
				change_chaos_frame(ETHNICITY_WALSTANIAN, -1);
				change_chaos_frame(ETHNICITY_GARGASTAN, -1);
				change_chaos_frame(ETHNICITY_BACRUM, -1);
			}
		}
		break;


	}

	return "";
}


static cptr do_earth_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

	int dir;
	int mlev = p_ptr->magic_exp[REALM_EARTH]/10;
	int pstat = p_ptr->stat_use[A_INT];

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "エイクオブゾーン";
		if (desc) return "ごく小さな酸の球を放つ。たまにそのモンスターを麻痺させる追加効果も発生する。";
#else
		if (name) return "Ache of Xorn";
		if (desc) return "Fires a ball of acid.";
#endif
    
		{
			int dice = 3 + ((mlev - 1) / 5);
			int sides = 4;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_ACID, dir, damroll(dice, sides), 0, FALSE);
				if (one_in_(3)) fire_ball(GF_STASIS, dir, 50 + mlev, 0, FALSE);
			}
		}
		break;

	case 1:
#ifdef JP
		if (name) return "デフバーサ";
		if (desc) return "光源が照らしている範囲か部屋全体の地のエレメントを強め、風のエレメントを弱める。";
#else
		if (name) return "Deft Bertha";
		if (desc) return "Increase EARTH, decrease WIND nearby area and the inside of a room permanently.";
#endif
    
		{
			int amount = 2;
			int rad = (mlev / 10) + 1;

			if (pstat >= (18 + 100)) amount++;
			if (pstat >= (18 + 150)) amount++;
			if (pstat >= (18 + 200)) amount++;


			if (info) return info_radius(rad);

			if (cast)
			{
				inc_area_elem(0, ELEM_EARTH, amount, rad, TRUE);
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "アシッド・ボルト";
		if (desc) return "酸のボルトを放つ。";
#else
		if (name) return "Acid bolt";
		if (desc) return "Fires a bolt of acid.";
#endif
    
		{
			int dice = 8 + (mlev - 5) / 4;
			int sides = 8;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_beam(GF_ACID, dir, damroll(dice, sides));
			}
		}
		break;

	case 3:
#ifdef JP
		if (name) return "森林創造";
		if (desc) return "周囲に木を作り出す。";
#else
		if (name) return "Forest Creation";
		if (desc) return "Creates trees in all adjacent squares.";
#endif
    
		{
			if (cast)
			{
				tree_creation();
			}
		}
		break;

	case 4:
#ifdef JP
		if (name) return "アシッドクラウド";
		if (desc) return "指定した地点から一定範囲に酸攻撃を行う。";
#else
		if (name) return "Acid Cloud";
		if (desc) return "Fires a ball of acid.";
#endif
    
		{
			int sides = mlev;
			int base = 10 + mlev * 2;
			int rad = (pstat >= (18 + 150)) ? 3 : 2;

			if (info) return info_damage(1, sides, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_ACID, dir, base + randint1(sides), rad, TRUE);
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "ジャンプウォール";
		if (desc) return "1マスの壁やモンスターを飛び越える。着地地点が床ではない場合は何も起きない。";
#else
		if (name) return "Jump Wall";
		if (desc) return "";
#endif
    
		{
			if (cast)
			{
				if (!jump_wall()) return NULL;
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "肌石化";
		if (desc) return "一定時間、石化への耐性を得て、ACを上昇させる。";
#else
		if (name) return "Stone Skin";
		if (desc) return "Gives bonus to AC for a while.";
#endif
    
		{
			int sides = mlev / 2;
			int base = mlev / 2;

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_shield(randint1(sides)+base, FALSE);
			}
		}
		break;

	case 7:
#ifdef JP
		if (name) return "地竜精";
		if (desc) return "酸のビーム、破片のビームを放つ。知能が高いと生命力吸収のビームも付き、与えたダメージだけ体力が回復する。";
#else
		if (name) return "Earth-dragon spirits";
		if (desc) return "Fires a beam of acid and shard.";
#endif
    
		{
			int dice1 = 10;
			int sides1 = mlev / 3;
			int dice2 = 2;
			int sides2 = mlev;
			bool drain = (pstat >= (18 + 200)) ? TRUE : FALSE;
#ifdef JP
			static const char s_dam[] = "損傷:";
#else
			static const char s_dam[] = "dam ";
#endif

			if (info)
			{
				if (drain) return format("%s%dd%d*2+%dd%d", s_dam, dice1, sides1, dice2, sides2);
				else return format("%s%dd%d*2", s_dam, dice1, sides1);
			}

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_beam(GF_ACID, dir, damroll(dice1, sides1));
				fire_beam(GF_SHARDS, dir, damroll(dice1, sides1));
				if (drain) fire_beam(GF_NEW_DRAIN, dir, damroll(dice2, sides2));
			}
		}
		break;

	case 8:
#ifdef JP
		if (name) return "大地の楔";
		if (desc) return "一定時間、自分以外の意志でテレポートしなくなる。さらに、浮遊が無効化される。";
#else
		if (name) return "Earth spike";
		if (desc) return "";
#endif
    
		{
			int sides = 6;
			int base = 6;

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_earth_spike(base + randint1(sides), FALSE);
			}
		}
		break;

	case 9:
#ifdef JP
		if (name) return "ノーム";
		if (desc) return "指定した地点から一定範囲内のモンスターに無差別に酸攻撃を繰り返す。";
#else
		if (name) return "Gnome";
		if (desc) return "";
#endif
    
		{
			int base = mlev;
			int attacks = 3;
			if (pstat >= (18 + 100)) attacks++;
			if (pstat >= (18 + 150)) attacks++;
			if (pstat >= (18 + 180)) attacks++;
			if (pstat >= (18 + 200)) attacks++;
			if (pstat >= (18 + 220)) attacks++;

			if (info) return info_call_the_elemental(base, base, attacks);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

#ifdef JP
				msg_format("精霊ノームを召喚した。");
#else
				msg_format("You called the Gnome.");
#endif
				cast_call_the_elemental(GF_ACID, dir, base, 1, base, 3, A_INT);
			}
		}
		break;

	case 10:
#ifdef JP
		if (name) return "クラッグプレス";
		if (desc) return "指定した地点から一定範囲に隕石攻撃を行う。";
#else
		if (name) return "Crack Breath";
		if (desc) return "Make a meteor ball.";
#endif
    
		{
			int sides = mlev * 2;
			int base = 100 + mlev;

			if (info) return info_damage(1, sides, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_METEOR, dir, base + randint1(sides), 2, TRUE);
			}
		}
		break;

	case 11:
#ifdef JP
		if (name) return "ペトロブレス";
		if (desc) return "石化のブレスを吐く。";
#else
		if (name) return "Petro breath";
		if (desc) return "Breath of stoning.";
#endif
    
		{
			int base = MIN(p_ptr->chp / 2, 350);

			if (info) return info_damage(0, 0, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return FALSE;

				fire_ball(GF_STONE, dir, base, -2, FALSE);
			}
		}
		break;

	case 12:
#ifdef JP
		if (name) return "アースクェイク";
		if (desc) return "視界内の範囲に対して破片攻撃を行い、自分もダメージを受ける。禁呪を使用する者は人心を失う。";
#else
		if (name) return "Earthquake";
		if (desc) return "Attakcs by shards in sight.";
#endif
    
		{
			int base = mlev * 8;
			int sides = mlev * 5;

			if (info) return info_multi_damage_dice_base(base, sides);

			if (cast)
			{
				project(0, MAX_SIGHT, py, px, base + randint1(sides), GF_SHARDS, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
#ifdef JP
				take_hit(DAMAGE_NOESCAPE, base + randint1(sides), "アースクェイクの呪文の巻き添え");
#else
				take_hit(DAMAGE_NOESCAPE, base + randint1(sides), "the strain of casting Earthquake");
#endif

				change_chaos_frame(ETHNICITY_WALSTANIAN, -1);
				change_chaos_frame(ETHNICITY_GARGASTAN, -1);
				change_chaos_frame(ETHNICITY_BACRUM, -1);
			}
		}
		break;


	}

	return "";
}


static cptr do_wind_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

	int dir;
	int mlev = p_ptr->magic_exp[REALM_WIND]/10;
	int pstat = p_ptr->stat_use[A_INT];

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "デッドショット";
		if (desc) return "轟音のビームを放つ。";
#else
		if (name) return "Deadshot";
		if (desc) return "Fires a bolt of sound.";
#endif
    
		{
			int dice = 3 + ((mlev - 1) / 5);
			int sides = 4;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_beam(GF_SOUND, dir, damroll(dice, sides));
			}
		}
		break;

	case 1:
#ifdef JP
		if (name) return "デフハーネラ";
		if (desc) return "光源が照らしている範囲か部屋全体の風のエレメントを強め、地のエレメントを弱める。";
#else
		if (name) return "Deft Hahnela";
		if (desc) return "Increase WIND, decrease EARTH nearby area and the inside of a room permanently.";
#endif
    
		{
			int amount = 2;
			int rad = (mlev / 10) + 1;

			if (pstat >= (18 + 100)) amount++;
			if (pstat >= (18 + 150)) amount++;
			if (pstat >= (18 + 200)) amount++;


			if (info) return info_radius(rad);

			if (cast)
			{
				inc_area_elem(0, ELEM_WIND, amount, rad, TRUE);
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "風の護り";
		if (desc) return "一定時間、射撃を回避する能力を得る。知能が高いと魔法のボルトやロケットも回避する。";
#else
		if (name) return "Wind's gurad";
		if (desc) return "Protect from shoot for a while.";
#endif
    
		{
			int sides = mlev;
			int base = 20;

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_wind_guard(randint1(sides)+base, FALSE);
			}
		}
		break;

	case 3:
#ifdef JP
		if (name) return "サンダー・ボルト";
		if (desc) return "電撃のボルトを放つ。";
#else
		if (name) return "Thunder bolt";
		if (desc) return "Fires a bolt of lightning.";
#endif
    
		{
			int dice = 8 + (mlev - 5) / 4;
			int sides = 8;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_bolt(GF_ELEC, dir, damroll(dice, sides));
			}
		}
		break;

	case 4:
#ifdef JP
		if (name) return "サンダーフレア";
		if (desc) return "指定した地点から一定範囲に電撃攻撃を行う。";
#else
		if (name) return "Thunder Flare";
		if (desc) return "Fires a ball of lightning.";
#endif
    
		{
			int sides = mlev;
			int base = 10 + mlev * 2;
			int rad = (pstat >= (18 + 150)) ? 3 : 2;

			if (info) return info_damage(1, sides, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_ELEC, dir, base + randint1(sides), rad, TRUE);
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "クイックムーブ";
		if (desc) return "一定時間、加速する。";
#else
		if (name) return "Quick Move";
		if (desc) return "Hastes you for a while.";
#endif
    
		{
			int base = mlev;
			int sides = 25 + mlev;

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_fast(randint1(sides) + base, FALSE);
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "コールストーム";
		if (desc) return "天候を悪化させる。";
#else
		if (name) return "Call storm";
		if (desc) return "Weather to bad.";
#endif
    
		{
			int power = 2;

			if (pstat >= (18 + 150)) power++;
			if (pstat >= (18 + 200)) power++;

			if (cast)
			{
				msg_print("天候が荒れていく...");

				set_weather(power, power, power);
			}
		}
		break;

	case 7:
#ifdef JP
		if (name) return "トルネード";
		if (desc) return "自分を中心とした*風*の球、轟音の球、重力の球を発生させる。";
#else
		if (name) return "Tornado";
		if (desc) return "";
#endif
    
		{
			int rad = mlev / 10;
			int dam1 = 40 + mlev / 2;
			int dam2 = 20 + mlev / 2;

#ifdef JP
			if (info) return format("損傷:%d+%d*2", dam1, dam2);
#else
			if (info) return format("dam %d+%d*2", dam1, dam2);
#endif


			if (cast)
			{
				project(0, rad, py, px, dam1, GF_PURE_WIND, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM, MODIFY_ELEM_MODE_MAGIC);
				project(0, rad, py, px, dam2, GF_SOUND, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM, MODIFY_ELEM_MODE_MAGIC);
				project(0, rad, py, px, dam2, GF_GRAVITY, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM, MODIFY_ELEM_MODE_MAGIC);
			}
		}
		break;

	case 8:
#ifdef JP
		if (name) return "サンダーバード";
		if (desc) return "指定した地点から一定範囲内のモンスターに無差別に電撃攻撃を繰り返す。";
#else
		if (name) return "Thunderbird";
		if (desc) return "";
#endif
    
		{
			int base = mlev;
			int attacks = 3;
			if (pstat >= (18 + 100)) attacks++;
			if (pstat >= (18 + 150)) attacks++;
			if (pstat >= (18 + 180)) attacks++;
			if (pstat >= (18 + 200)) attacks++;
			if (pstat >= (18 + 220)) attacks++;

			if (info) return info_call_the_elemental(base, base, attacks);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

#ifdef JP
				msg_format("精霊サンダーバードを召喚した。");
#else
				msg_format("You called the Thunderbird.");
#endif
				cast_call_the_elemental(GF_ELEC, dir, base, 1, base, 3, A_INT);
			}
		}
		break;

	case 9:
#ifdef JP
		if (name) return "ハーネラの罵声";
		if (desc) return "轟音のブレスを吐く。";
#else
		if (name) return "Howl of Hahnela";
		if (desc) return "Breath of sound.";
#endif
    
		{
			int base = MIN(p_ptr->chp / 2, 350);

			if (info) return info_damage(0, 0, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return FALSE;

				fire_ball(GF_SOUND, dir, base, -2, FALSE);
			}
		}
		break;

	case 10:
#ifdef JP
		if (name) return "次元の扉";
		if (desc) return "短距離内の指定した場所にテレポートする。";
#else
		if (name) return "Dimension Door";
		if (desc) return "Teleport to given location.";
#endif
    
		{
			int range = mlev / 2 + 10;

			if (info) return info_range(range);

			if (cast)
			{
#ifdef JP
				msg_print("次元の扉が開いた。目的地を選んで下さい。");
#else
				msg_print("You open a dimensional gate. Choose a destination.");
#endif

				if (!dimension_door(mlev)) return NULL;
			}
		}
		break;

	case 11:
#ifdef JP
		if (name) return "エアリアルクライ";
		if (desc) return "視界内の範囲に対して轟音攻撃を行い、自分もダメージを受ける。さらに、天候を暴風にする。禁呪を使用する者は人心を失う。";
#else
		if (name) return "Aerial Cry";
		if (desc) return "Attacks by sound.";
#endif
    
		{
			int base = mlev * 8;
			int sides = mlev * 5;

			if (info) return info_multi_damage_dice_base(base, sides);

			if (cast)
			{
				project(0, MAX_SIGHT, py, px, base + randint1(sides), GF_SHARDS, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
#ifdef JP
				take_hit(DAMAGE_NOESCAPE, base + randint1(sides), "エアリアルクライの呪文の巻き添え");
#else
				take_hit(DAMAGE_NOESCAPE, base + randint1(sides), "the strain of casting Aerial Cry");
#endif

				set_weather(0, 15, 0);
				change_chaos_frame(ETHNICITY_WALSTANIAN, -1);
				change_chaos_frame(ETHNICITY_GARGASTAN, -1);
				change_chaos_frame(ETHNICITY_BACRUM, -1);
			}
		}
		break;


	}

	return "";
}


static cptr do_holy_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

	int dir;
	int mlev = p_ptr->magic_exp[REALM_HOLY]/10;
	int pstat = p_ptr->stat_use[A_WIS];

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "邪悪存在感知";
		if (desc) return "近くの邪悪なモンスターを感知する。";
#else
		if (name) return "Detect Evil";
		if (desc) return "Detects all evil monsters in your vicinity.";
#endif
    
		{
			int rad = DETECT_RAD_DEFAULT;

			if (info) return info_radius(rad);

			if (cast)
			{
				detect_monsters_evil(rad);
			}
		}
		break;

	case 1:
#ifdef JP
		if (name) return "祝福";
		if (desc) return "一定時間、命中率とACにボーナスを得る。";
#else
		if (name) return "Bless";
		if (desc) return "Gives bonus to hit and AC for a few turns.";
#endif
    
		{
			int base = 12;

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_blessed(randint1(base) + base, FALSE);
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "光の召喚";
		if (desc) return "光源が照らしている範囲か部屋全体を永久に明るくする。";
#else
		if (name) return "Call Light";
		if (desc) return "Lights up nearby area and the inside of a room permanently.";
#endif
    
		{
			int dice = 2;
			int sides = mlev / 2;
			int rad = mlev / 10 + 1;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				lite_area(damroll(dice, sides), rad);
			}
		}
		break;

	case 3:
#ifdef JP
		if (name) return "ヒーリング";
		if (desc) return "体力を中程度回復させ、負傷と朦朧状態も全快する。";
#else
		if (name) return "Cure Critical Wounds";
		if (desc) return "Heals cut, stun and HP.";
#endif
    
		{
			int dice = 8;
			int sides = 10;

			if (info) return info_heal(dice, sides, 0);

			if (cast)
			{
				hp_player(damroll(dice, sides));
				set_stun(0);
				set_cut(0);
			}
		}
		break;

	case 4:
#ifdef JP
		if (name) return "罠 & 隠し扉感知";
		if (desc) return "近くの全ての罠と扉と階段を感知する。";
#else
		if (name) return "Detect Doors & Traps";
		if (desc) return "Detects traps, doors, and stairs in your vicinity.";
#endif
    
		{
			int rad = DETECT_RAD_DEFAULT;

			if (info) return info_radius(rad);

			if (cast)
			{
				detect_traps(rad, TRUE);
				detect_doors(rad);
				detect_stairs(rad);
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "空腹充足";
		if (desc) return "満腹にする。";
#else
		if (name) return "Satisfy Hunger";
		if (desc) return "Satisfies hunger.";
#endif
    
		{
			if (cast)
			{
				set_food(PY_FOOD_MAX - 1);
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "キュアポイズン";
		if (desc) return "体内の毒を取り除く。";
#else
		if (name) return "Cure Poison";
		if (desc) return "Cure poison status.";
#endif
    
		{
			if (cast)
			{
				set_poisoned(0);
			}
		}
		break;

	case 7:
#ifdef JP
		if (name) return "キュアカース";
		if (desc) return "アイテムにかかった弱い呪いを解除する。レベルが上がると強力な呪いも解除する。";
#else
		if (name) return "Remove Curse";
		if (desc) return "Removes normal curses from equipped items.";
#endif

		{
			if (cast)
			{
				if (mlev < 40)
				{
					if (remove_curse())
					{
#ifdef JP
						msg_print("誰かに見守られているような気がする。");
#else
						msg_print("You feel as if someone is watching over you.");
#endif
					}
				}
				else
				{
					if (remove_all_curse())
					{
#ifdef JP
						msg_print("誰かに見守られているような気がする。");
#else
						msg_print("You feel as if someone is watching over you.");
#endif
					}
				}
			}
		}
		break;

	case 8:
#ifdef JP
		if (name) return "予見";
		if (desc) return "周辺の地形を感知し、近くの罠、扉、階段、全ての見えるモンスターを感知する。";
#else
		if (name) return "Divination";
		if (desc) return "Maps nearby area. Detects all monsters, traps, doors and stairs.";
#endif
    
		{
			int rad1 = DETECT_RAD_MAP;
			int rad2 = DETECT_RAD_DEFAULT;

			if (info) return info_radius(MAX(rad1, rad2));

			if (cast)
			{
				map_area(rad1);
				detect_traps(rad2, TRUE);
				detect_doors(rad2);
				detect_stairs(rad2);
				detect_monsters_normal(rad2);
			}
		}
		break;

	case 9:
#ifdef JP
		if (name) return "神の槍";
		if (desc) return "無傷球をも切り裂く純粋なエネルギーのビームを放つ。";
#else
		if (name) return "Goddly spear";
		if (desc) return "Fires a bolt of pure energy.";
#endif
    
		{
			int sides = mlev * 2;
			int base = 100;

			if (info) return info_damage(1, sides, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return FALSE;

				fire_beam(GF_GODLY_SPEAR, dir, base + randint1(sides));
			}
		}
		break;

	case 10:
#ifdef JP
		if (name) return "結界の紋章";
		if (desc) return "自分のいる床の上に、モンスターが通り抜けたり召喚されたりすることができなくなるルーンを描く。";
#else
		if (name) return "Glyph of Warding";
		if (desc) return "Sets a glyph on the floor beneath you. Monsters cannot attack you if you are on a glyph, but can try to break glyph.";
#endif
    
		{
			if (cast)
			{
				warding_glyph();
			}
		}
		break;

	case 11:
#ifdef JP
		if (name) return "ヒーリングオール";
		if (desc) return "極めて強力な回復呪文で、負傷と朦朧状態も全快する。";
#else
		if (name) return "Healing all";
		if (desc) return "Much powerful healing magic, and heals cut and stun completely.";
#endif
    
		{
			int heal = 1000;

			if (info) return info_heal(0, 0, heal);

			if (cast)
			{
				hp_player(heal);
				set_stun(0);
				set_cut(0);
			}
		}
		break;

	case 12:
#ifdef JP
		if (name) return "クリアランス";
		if (desc) return "毒、負傷を全快させ、狂戦士化、恐怖、幻覚、徐々に進行する石化を直す。";
#else
		if (name) return "Clearance";
		if (desc) return "heals poison, fear, stun and cut status.";
#endif
    
		{
			if (cast)
			{
				set_poisoned(0);
				set_cut(0);
				set_afraid(0);
				set_image(0);
				set_shero(0, TRUE);
				set_stoning(0);
			}
		}
		break;

	case 13:
#ifdef JP
		if (name) return "対邪悪結界";
		if (desc) return "邪悪なモンスターの攻撃を防ぐバリアを張る。";
#else
		if (name) return "Protection from Evil";
		if (desc) return "Gives aura which protect you from evil monster's physical attack.";
#endif
    
		{
			int base = 3 * mlev;
			int sides = 25;

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_protevil(randint1(sides) + base, FALSE);
			}
		}
		break;

	case 14:
#ifdef JP
		if (name) return "ヒーリングプラス";
		if (desc) return "自分及び自分の周囲のモンスターの体力を大幅に回復し、負傷と朦朧状態も全快させる。";
#else
		if (name) return "Healing plus";
		if (desc) return "Much powerful healing magic, and heals cut and stun completely.";
#endif
    
		{
			int heal = 300;

			if (info) return info_heal(0, 0, heal);

			if (cast)
			{
				hp_player(heal);
				project(0, 2, py, px, heal, GF_OLD_HEAL, PROJECT_KILL | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_NONE);
				set_stun(0);
				set_cut(0);
			}
		}
		break;

	case 15:
#ifdef JP
		if (name) return "全耐性";
		if (desc) return "一定時間、酸、電撃、炎、冷気、毒に対する耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Resistance";
		if (desc) return "Gives resistance to fire, cold, electricity, acid and poison for a while. These resistances can be added to which from equipment for more powerful resistances.";
#endif
    
		{
			int base = 20;

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_oppose_acid(randint1(base) + base, FALSE);
				set_oppose_elec(randint1(base) + base, FALSE);
				set_oppose_fire(randint1(base) + base, FALSE);
				set_oppose_cold(randint1(base) + base, FALSE);
				set_oppose_pois(randint1(base) + base, FALSE);
			}
		}
		break;

	case 16:
#ifdef JP
		if (name) return "布教の言葉";
		if (desc) return "視界内の全てのモンスターを魅了する。抵抗されると無効。";
#else
		if (name) return "Day of the Dove";
		if (desc) return "Attempts to charm all monsters in sight.";
#endif
    
		{
			int power = mlev + 50;

			if (info) return info_power(power);

			if (cast)
			{
				charm_monsters(power);
			}
		}
		break;

	case 17:
#ifdef JP
		if (name) return "全復活";
		if (desc) return "すべてのステータスと経験値を回復する。";
#else
		if (name) return "Restoration";
		if (desc) return "Restores all stats and experience.";
#endif
    
		{
			if (cast)
			{
				do_res_stat(A_STR);
				do_res_stat(A_INT);
				do_res_stat(A_WIS);
				do_res_stat(A_DEX);
				do_res_stat(A_CON);
				do_res_stat(A_CHR);
				restore_level();
			}
		}
		break;

	case 18:
#ifdef JP
		if (name) return "神聖なる光";
		if (desc) return "その階全体を永久に照らし、ダンジョン内すべてのアイテム、モンスターを感知する。";
#else
		if (name) return "Clairvoyance";
		if (desc) return "Maps and lights whole dungeon level. Knows all objects location. And gives telepathy for a while.";
#endif
    
		{
			if (cast)
			{
				wiz_lite(FALSE);
			}
		}
		break;

	case 19:
#ifdef JP
		if (name) return "イグニスファタス";
		if (desc) return "指定した地点から一定範囲内のモンスターに無差別に聖なる攻撃を繰り返す。";
#else
		if (name) return "Ignis Fatuus";
		if (desc) return "";
#endif
    
		{
			int base = mlev;
			int attacks = 1;
			if (pstat >= (18 + 100)) attacks++;
			if (pstat >= (18 + 150)) attacks++;
			if (pstat >= (18 + 180)) attacks++;
			if (pstat >= (18 + 200)) attacks++;
			if (pstat >= (18 + 220)) attacks++;

			if (info) return info_call_the_elemental(base, base, attacks);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

#ifdef JP
				msg_format("精霊イグニスファタスを召喚した。");
#else
				msg_format("You called the Ignis Fatuus.");
#endif
				cast_call_the_elemental(GF_HOLY_FIRE, dir, base, 1, base, 1, A_WIS);
			}
		}
		break;

	case 20:
#ifdef JP
		if (name) return "天使召喚";
		if (desc) return "天使を1体召喚する。";
#else
		if (name) return "Summon Angel";
		if (desc) return "Summons an angel.";
#endif
    
		{
			if (cast)
			{
				bool pet = !one_in_(3);
				u32b mode = 0L;

				if (pet) mode |= PM_FORCE_PET;
				else mode |= (PM_NO_PET | PM_IGNORE_AMGRID);
				if (!(pet && (mlev < 50))) mode |= PM_ALLOW_GROUP;

				if (summon_specific((pet ? -1 : 0), py, px, (mlev * 3) / 2, SUMMON_ANGEL, mode))
				{
					if (pet)
					{
#ifdef JP
						msg_print("「ご用でございますか、ご主人様」");
#else
						msg_print("'What is thy bidding... Master?'");
#endif
					}
					else
					{
#ifdef JP
						msg_print("「我は汝の下僕にあらず！ 悪行者よ、悔い改めよ！」");
#else
						msg_print("Mortal! Repent of thy impiousness.");
#endif
					}
				}
			}
		}
		break;

	case 21:
#ifdef JP
		if (name) return "真・結界";
		if (desc) return "自分のいる床と周囲8マスの床の上に、モンスターが通り抜けたり召喚されたりすることができなくなるルーンを描く。";
#else
		if (name) return "Warding True";
		if (desc) return "Creates glyphs in all adjacent squares and under you.";
#endif
    
		{
			int rad = 1;

			if (info) return info_radius(rad);

			if (cast)
			{
				warding_glyph();
				glyph_creation();
			}
		}
		break;

	case 22:
#ifdef JP
		if (name) return "パーフェクトヒール";
		if (desc) return "最強の治癒の魔法で、負傷と毒も全快し、狂戦士化、恐怖、幻覚、徐々に進行する石化も直す。";
#else
		if (name) return "Healing True";
		if (desc) return "The greatest healing magic. Heals all HP, cut and stun.";
#endif
    
		{
			int heal = 5000;

			if (info) return info_heal(0, 0, heal);

			if (cast)
			{
				hp_player(heal);
				set_poisoned(0);
				set_cut(0);
				set_afraid(0);
				set_shero(0, TRUE);
				set_image(0);
				set_stoning(0);
			}
		}
		break;

	case 23:
#ifdef JP
		if (name) return "リザレクション";
		if (desc) return "一定時間、死亡しても完全復活できる能力を得る。全MPを消費する。";
#else
		if (name) return "Resurrection";
		if (desc) return "Gives resurrection for a while.";
#endif
    
		{
			int base = 6;

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_tim_resurrection(base + randint1(base), FALSE);
			}
		}
		break;


	}

	return "";
}


static cptr do_death_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

	int dir;
	int mlev = p_ptr->magic_exp[REALM_DEATH]/10;

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "アンデッド従属";
		if (desc) return "アンデッド1体を魅了する。抵抗されると無効。";
#else
		if (name) return "Enslave Undead";
		if (desc) return "Attempts to charm an undead monster.";
#endif
    
		{
			int power = mlev + 50;

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				control_one_undead(dir, power);
			}
		}
		break;

	case 1:
#ifdef JP
		if (name) return "暗黒治療";
		if (desc) return "盲目、毒、混乱、朦朧状態、負傷を全快させ、幻覚を直す。";
#else
		if (name) return "Dark Cure";
		if (desc) return "heals poison, fear, stun and cut status.";
#endif
    
		{
			if (cast)
			{
				set_blind(0);
				set_poisoned(0);
				set_confused(0);
				set_stun(0);
				set_cut(0);
				set_image(0);
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "死体廃熱化";
		if (desc) return "死体からエネルギーを吸い取り、体力を回復する。吸い取られた死体は灰になる。";
#else
		if (name) return "Drain corpse";
		if (desc) return "drain energy from corpse.";
#endif
    
		{
			if (cast)
			{
				if (!get_energy_from_corpse()) return NULL;
			}
		}
		break;

	case 3:
#ifdef JP
		if (name) return "闇の焔";
		if (desc) return "邪悪な力を持つ球を放つ。善良なモンスターに対して大きなダメージを与える。";
#else
		if (name) return "Dark fire";
		if (desc) return "Fires a ball of hell fire.";
#endif
    
		{
			int dice = 3;
			int sides = 6;
			int base = mlev + mlev / 2;
			int rad = (mlev < 30) ? 2 : 3;

			if (info) return info_damage(dice, sides, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_HELL_FIRE, dir, damroll(dice, sides) + base, rad, FALSE);
			}
		}
		break;

	case 4:
#ifdef JP
		if (name) return "チャージスペル";
		if (desc) return "一定時間、知能と賢さを上昇させ、MPの回復力が増強される。";
#else
		if (name) return "Charge Spell";
		if (desc) return "Gain Int and Wis for a while.";
#endif
    
		{
			int base = mlev;
			int sides = mlev + 25;

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_chargespell(randint1(sides) + base, FALSE);
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "ナイトメア";
		if (desc) return "ごく小さな暗黒の球を放つ。しばしば眠りも引き起こす。";
#else
		if (name) return "Nightmare";
		if (desc) return "Fire a ball of dark.";
#endif
    
		{
			int dice = 4 + (mlev - 10) / 5;
			int sides = 4;
			int base = 30 + mlev;

			if (info) return info_damage(dice, sides, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return FALSE;

				fire_ball(GF_DARK, dir, base + damroll(dice, sides), 0, FALSE);
				if (!one_in_(3)) fire_ball_hide(GF_OLD_SLEEP, dir, mlev + 50, 0, FALSE);
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "空間歪曲";
		if (desc) return "視界内の全てのモンスターに対して重力攻撃を行う。";
#else
		if (name) return "Distort";
		if (desc) return "Attacks by gravity in sight.";
#endif
    
		{
			int dice = mlev / 5 + 10;
			int sides = 10;

			if (info) return info_multi_damage_dice(dice, sides);

			if (cast)
			{
				project_hack(GF_GRAVITY, damroll(dice, sides));
			}
		}
		break;

	case 7:
#ifdef JP
		if (name) return "モンスター消滅";
		if (desc) return "モンスター1体を消し去る。経験値やアイテムは手に入らない。抵抗されると無効。";
#else
		if (name) return "Genocide One";
		if (desc) return "Attempts to vanish a monster.";
#endif
    
		{
			int power = mlev + 50;

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball_hide(GF_GENOCIDE, dir, power, 0, FALSE);
			}
		}
		break;

	case 8:
#ifdef JP
		if (name) return "死者召喚";
		if (desc) return "アンデッドを召喚する。レベルが上がると上級アンデッドを召喚する。";
#else
		if (name) return "Summon undead";
		if (desc) return "Summons an undead monster.";
#endif
    
		{
			if (cast)
			{
				int type;
				bool pet = one_in_(3);
				u32b mode = 0L;

				type = (mlev > 47 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD);

				if (!pet || (pet && (mlev > 24) && one_in_(3)))
					mode |= PM_ALLOW_GROUP;

				if (pet) mode |= PM_FORCE_PET;
				else mode |= (PM_ALLOW_UNIQUE | PM_NO_PET | PM_IGNORE_AMGRID);

				if (summon_specific((pet ? -1 : 0), py, px, (mlev * 3) / 2, type, mode))
				{
#ifdef JP
					msg_print("冷たい風があなたの周りに吹き始めた。それは腐敗臭を運んでいる...");
#else
					msg_print("Cold winds begin to blow around you, carrying with them the stench of decay...");
#endif


					if (pet)
					{
#ifdef JP
						msg_print("古えの死せる者共があなたに仕えるため土から甦った！");
#else
						msg_print("Ancient, long-dead forms arise from the ground to serve you!");
#endif
					}
					else
					{
#ifdef JP
						msg_print("死者が甦った。眠りを妨げるあなたを罰するために！");
#else
						msg_print("'The dead arise... to punish you for disturbing them!'");
#endif
					}
				}
			}
		}
		break;

	case 9:
#ifdef JP
		if (name) return "ワードオブペイン";
		if (desc) return "モンスター1体に現在の自分の受けているダメージと同じダメージを与える。";
#else
		if (name) return "Word of Pain";
		if (desc) return "";
#endif
    
		{
			int dam = MAX(p_ptr->mhp - p_ptr->chp, 0);

			if (info) info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return FALSE;

				fire_ball(GF_WORD_OF_PAIN, dir, 0, 0, FALSE);
			}
		}
		break;

	case 10:
#ifdef JP
		if (name) return "破壊の言葉";
		if (desc) return "周辺のアイテム、モンスター、地形を破壊する。";
#else
		if (name) return "Word of Destruction";
		if (desc) return "Destroy everything in nearby area.";
#endif
    
		{
			int base = 13;
			int sides = 5;

			if (cast)
			{
				destroy_area(py, px, base + randint0(sides));
			}
		}
		break;

	case 11:
#ifdef JP
		if (name) return "周辺蘇生";
		if (desc) return "周囲の死体や骨を生き返す。";
#else
		if (name) return "Animate dead";
		if (desc) return "Resurrects nearby corpse and skeletons. And makes these your pets.";
#endif
    
		{
			if (cast)
			{
				animate_dead(0, py, px);
			}
		}
		break;

	case 12:
#ifdef JP
		if (name) return "ペット爆破";
		if (desc) return "全てのペットを強制的に爆破させる。";
#else
		if (name) return "Explode Pets";
		if (desc) return "Makes all pets explode.";
#endif
    
		{
			if (cast)
			{
				discharge_minion();
			}
		}
		break;

	case 13:
#ifdef JP
		if (name) return "吸血の刃";
		if (desc) return "武器に吸血の属性をつける。";
#else
		if (name) return "Vampiric Branding";
		if (desc) return "Makes current weapon Vampiric.";
#endif
    
		{
			if (cast)
			{
				brand_weapon(EGO_VAMPIRIC);
			}
		}
		break;

	case 14:
#ifdef JP
		if (name) return "腐敗の大地";
		if (desc) return "光源が照らしている範囲か部屋全体の自分のエレメント以外のエレメントを弱める。";
#else
		if (name) return "Corrupted area";
		if (desc) return "Decrease all but your element nearby area and the inside of a room permanently.";
#endif
    
		{
			int amount = -4;
			int rad = -((mlev / 10) - 1);
			int pstat = p_ptr->stat_use[mp_ptr->spell_stat];

			if (pstat >= (18 + 100)) amount -= 2;
			if (pstat >= (18 + 150)) amount -= 2;
			if (pstat >= (18 + 200)) amount -= 2;

			if (info) return info_power(amount);

			if (cast)
			{
				int i;

				for (i = MIN_ELEM; i < ELEM_NUM; i++)
				{
					if (i == get_cur_pelem()) continue;
					inc_area_elem(0, i, amount, rad, FALSE);
				}
			}
		}
		break;

	case 15:
#ifdef JP
		if (name) return "ライフフォース";
		if (desc) return "モンスター1体から生命力を吸いとる。吸いとった生命力によって体力が回復する。";
#else
		if (name) return "Vampirism True";
		if (desc) return "Fires 3 bolts. Each of the bolts absorbs some HP from a monster and gives them to you.";
#endif
    
		{
			int dam = 100;

#ifdef JP
			if (info) return format("損傷:3*%d", dam);
#else
			if (info) return format("dam 3*%d", dam);
#endif

			if (cast)
			{
				int i;

				if (!get_aim_dir(&dir)) return NULL;

				for (i = 0; i < 3; i++)
					fire_ball(GF_NEW_DRAIN, dir, dam, 0, FALSE);
			}
		}
		break;

	case 16:
#ifdef JP
		if (name) return "ダークロア";
		if (desc) return "指定した地点から一定範囲内のモンスターに無差別に邪悪な攻撃を繰り返す。";
#else
		if (name) return "Dark lore";
		if (desc) return "";
#endif
    
		{
			int base = mlev;
			int attacks = 1;
			int pstat = p_ptr->stat_use[A_WIS];

			if (pstat >= (18 + 100)) attacks++;
			if (pstat >= (18 + 150)) attacks++;
			if (pstat >= (18 + 180)) attacks++;
			if (pstat >= (18 + 200)) attacks++;
			if (pstat >= (18 + 220)) attacks++;

			if (info) return info_call_the_elemental(base, base, attacks);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

#ifdef JP
				msg_format("精霊ファントムを召喚した。");
#else
				msg_format("You called the Phantom.");
#endif
				cast_call_the_elemental(GF_HELL_FIRE, dir, base, 1, base, 1, A_WIS);
			}
		}
		break;

	case 17:
#ifdef JP
		if (name) return "冥界への招待";
		if (desc) return "視界内の全てのモンスターに対して邪悪な攻撃を行い、眠らせる。善良なモンスターには大きなダメージを与える。";
#else
		if (name) return "Invite to Hell";
		if (desc) return "";
#endif
    
		{
			int dam = mlev * 4;
			int power = mlev + 50;

			if (info) info_multi_damage(dam);

			if (cast)
			{
				project_hack_living(GF_HELL_FIRE, dam);
				project_hack_living(GF_OLD_SLEEP, power);
			}
		}
		break;

	case 18:
#ifdef JP
		if (name) return "抹殺";
		if (desc) return "指定した文字のモンスターを現在の階から消し去る。抵抗されると無効。";
#else
		if (name) return "Genocide";
		if (desc) return "Eliminates an entire class of monster, exhausting you.  Powerful or unique monsters may resist.";
#endif
    
		{
			int power = mlev + 50;

			if (info) return info_power(power);

			if (cast)
			{
				symbol_genocide(power, TRUE);
			}
		}
		break;

	case 19:
#ifdef JP
		if (name) return "暗黒の嵐";
		if (desc) return "巨大な暗黒の球を放つ。";
#else
		if (name) return "Darkness Storm";
		if (desc) return "Fires a huge ball of darkness.";
#endif
    
		{
			int dam = 100 + mlev * 4;
			int rad = 4;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_DARK, dir, dam, rad, FALSE);
			}
		}
		break;

	case 20:
#ifdef JP
		if (name) return "闇の破神剣";
		if (desc) return "一定時間、攻撃力と命中率が強化され、武器が生命ある存在及び善良な存在に強くなる。利き腕の武器を外すと無効。";
#else
		if (name) return "Dark ragnarok";
		if (desc) return "";
#endif
    
		{
			int base = mlev /2;

			if (info) info_duration(base, base);

			if (cast)
			{
				if (!buki_motteruka(INVEN_RARM))
				{
#ifdef JP
					msg_format("利き腕に武器を持たないと闇の破神剣は使えない。");
#else
					msg_format("You cannot use evil weapon with no main weapon.");
#endif
					return NULL;
				}

				set_evil_weapon(randint1(base) + base, FALSE, INVEN_RARM, FALSE);
			}
		}
		break;

	case 21:
#ifdef JP
		if (name) return "周辺抹殺";
		if (desc) return "自分の周囲にいるモンスターを現在の階から消し去る。抵抗されると無効。";
#else
		if (name) return "Mass Genocide";
		if (desc) return "Eliminates all nearby monsters, exhausting you.  Powerful or unique monsters may be able to resist.";
#endif
    
		{
			int power = mlev + 50;

			if (info) return info_power(power);

			if (cast)
			{
				mass_genocide(power, TRUE);
			}
		}
		break;

	case 22:
#ifdef JP
		if (name) return "地獄の劫火";
		if (desc) return "非常に強力で巨大な邪悪な球を放つ。善良なモンスターには大きなダメージを与える。";
#else
		if (name) return "Hellfire";
		if (desc) return "Fires a powerful ball of evil power. Hurts good monsters greatly.";
#endif
    
		{
			int dam = 666;
			int rad = 3;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_HELL_FIRE, dir, dam, rad, FALSE);
#ifdef JP
				take_hit(DAMAGE_USELIFE, 20 + randint1(30), "地獄の劫火の呪文を唱えた疲労");
#else
				take_hit(DAMAGE_USELIFE, 20 + randint1(30), "the strain of casting Hellfire");
#endif
			}
		}
		break;

	case 23:
#ifdef JP
		if (name) return "パラダイム";
		if (desc) return "時を止める。全MPを消費する。";
#else
		if (name) return "Paradigm";
		if (desc) return "Stops time. Consumes all of your SP.";
#endif
    
		{
			if (cast)
			{
				if (!cast_stop_the_time()) return NULL;
			}
		}
		break;

	case 24:
#ifdef JP
		if (name) return "デッドスクリーム";
		if (desc) return "視界内の範囲に対して暗黒攻撃を行い、自分もダメージを受ける。禁呪を使用する者は人心を失う。";
#else
		if (name) return "Dead Scream";
		if (desc) return "";
#endif
    
		{
			int base = mlev * 8;
			int sides = mlev * 5;

			if (info) return info_multi_damage_dice_base(base, sides);

			if (cast)
			{
				project(0, MAX_SIGHT, py, px, base + randint1(sides), GF_DARK, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
#ifdef JP
				take_hit(DAMAGE_NOESCAPE, base + randint1(sides), "デッドスクリームの呪文の巻き添え");
#else
				take_hit(DAMAGE_NOESCAPE, base + randint1(sides), "the strain of casting Dead Scream");
#endif

				change_chaos_frame(ETHNICITY_WALSTANIAN, -1);
				change_chaos_frame(ETHNICITY_GARGASTAN, -1);
				change_chaos_frame(ETHNICITY_BACRUM, -1);
			}
		}
		break;


	}

	return "";
}


static bool ang_sort_comp_pet(vptr u, vptr v, int a, int b)
{
	u16b *who = (u16b*)(u);

	int w1 = who[a];
	int w2 = who[b];

	monster_type *m_ptr1 = &m_list[w1];
	monster_type *m_ptr2 = &m_list[w2];
	monster_race *r_ptr1 = &r_info[m_ptr1->r_idx];
	monster_race *r_ptr2 = &r_info[m_ptr2->r_idx];

	/* Unused */
	(void)v;

	if (m_ptr1->nickname && !m_ptr2->nickname) return TRUE;
	if (m_ptr2->nickname && !m_ptr1->nickname) return FALSE;

	if (!m_ptr1->parent_m_idx && m_ptr2->parent_m_idx) return TRUE;
	if (!m_ptr2->parent_m_idx && m_ptr1->parent_m_idx) return FALSE;

	if ((r_ptr1->flags1 & RF1_UNIQUE) && !(r_ptr2->flags1 & RF1_UNIQUE)) return TRUE;
	if ((r_ptr2->flags1 & RF1_UNIQUE) && !(r_ptr1->flags1 & RF1_UNIQUE)) return FALSE;

	if (r_ptr1->level > r_ptr2->level) return TRUE;
	if (r_ptr2->level > r_ptr1->level) return FALSE;

	if (m_ptr1->hp > m_ptr2->hp) return TRUE;
	if (m_ptr2->hp > m_ptr1->hp) return FALSE;
	
	return w1 <= w2;
}


static cptr do_symbiotic_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

	int dir;
	int mlev = p_ptr->magic_exp[REALM_SYMBIOTIC]/10;

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "モンスター感知";
		if (desc) return "近くの全ての見えるモンスターを感知する。";
#else
		if (name) return "Detect Monsters";
		if (desc) return "Detects all monsters in your vicinity unless invisible.";
#endif
    
		{
			int rad = DETECT_RAD_DEFAULT;

			if (info) return info_radius(rad);

			if (cast)
			{
				detect_monsters_normal(rad);
			}
		}
		break;

	case 1:
#ifdef JP
		if (name) return "動物馴らし";
		if (desc) return "動物1体を魅了する。抵抗されると無効。";
#else
		if (name) return "Taim Monster";
		if (desc) return "Attempts to charm a monster.";
#endif
    
		{
			int power = mlev;

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				charm_monster(dir, power);
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "友人治療";
		if (desc) return "モンスター1体の体力を回復させ、麻痺、恐怖、朦朧、混乱を治療する。";
#else
		if (name) return "Heal Monster";
		if (desc) return "Heal a monster.";
#endif
    
		{
			int dice = 4;
			int sides = 6;

			if (info) return info_heal(dice, sides, 0);

			if (cast)
			{
				bool result;
				bool old_target_pet = target_pet;

				result = get_aim_dir(&dir);

				target_pet = old_target_pet;

				if (!result) return NULL;

				fire_ball(GF_OLD_HEAL, dir, damroll(4, 6), 0, FALSE);
			}
		}
		break;

	case 3:
#ifdef JP
		if (name) return "蔦絡み";
		if (desc) return "視界内の全てのモンスターを減速させる。抵抗されると無効。";
#else
		if (name) return "Entangle";
		if (desc) return "Attempts to slow all monsters in sight.";
#endif
    
		{
			int power = mlev;

			if (info) return info_power(power);

			if (cast)
			{
				slow_monsters(power);
			}
		}
		break;

	case 4:
#ifdef JP
		if (name) return "食料生成";
		if (desc) return "自分の足元に食料を作り出す。";
#else
		if (name) return "Produce Food";
		if (desc) return "Produces a Ration of Food.";
#endif
    
		{
			if (cast)
			{
				object_type forge, *q_ptr = &forge;

#ifdef JP
				msg_print("食料を生成した。");
#else
				msg_print("A food ration is produced.");
#endif

				/* Create the food ration */
				object_prep(q_ptr, lookup_kind(TV_FOOD, SV_FOOD_RATION));

				/* Drop the object from heaven */
				drop_near(q_ptr, -1, py, px);
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "ペット集合";
		if (desc) return "ペットを呼び寄せる。";
#else
		if (name) return "Call pet";
		if (desc) return "Call all pets.";
#endif
    
		{
			if (cast)
			{
				int pet_ctr, i;
				u16b *who;
				int max_pet = 0;
				u16b dummy_why;

				/* Allocate the "who" array */
				C_MAKE(who, max_m_idx, u16b);

				/* Process the monsters (backwards) */
				for (pet_ctr = m_max - 1; pet_ctr >= 1; pet_ctr--)
				{
					if (is_pet(&m_list[pet_ctr]) && (p_ptr->riding != pet_ctr))
					  who[max_pet++] = pet_ctr;
				}

				/* Select the sort method */
				ang_sort_comp = ang_sort_comp_pet;
				ang_sort_swap = ang_sort_swap_hook;

				ang_sort(who, &dummy_why, max_pet);

				/* Process the monsters (backwards) */
				for (i = 0; i < max_pet; i++)
				{
					pet_ctr = who[i];
					teleport_to(pet_ctr, py, px, 2, 100, TRUE);
				}

				/* Free the "who" array */
				C_KILL(who, max_m_idx, u16b);
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "四足獣召喚";
		if (desc) return "1体の四足獣('q'で表されるモンスター)を召喚する。";
#else
		if (name) return "Summon Animal";
		if (desc) return "Summons an animal.";
#endif
    
		{
			if (cast)
			{
				summon_kin_type = 'q';
				if (!summon_specific(-1, py, px, mlev * 2 / 3 + randint1(mlev/2), SUMMON_KIN, PM_FORCE_PET))
				{
#ifdef JP
					msg_print("動物は現れなかった。");
#else
					msg_print("No animals arrive.");
#endif
				}
			}
		}
		break;

	case 7:
#ifdef JP
		if (name) return "友からの知らせ";
		if (desc) return "近くの全てのモンスター、罠、扉、階段、財宝、アイテム、そして地形を感知する。";
#else
		if (name) return "Nature Awareness";
		if (desc) return "Maps nearby area. Detects all monsters, traps, doors and stairs.";
#endif
    
		{
			int rad1 = DETECT_RAD_MAP;
			int rad2 = DETECT_RAD_DEFAULT;

			if (info) return info_radius(MAX(rad1, rad2));

			if (cast)
			{
				map_area(rad1);
				detect_all(rad2);
			}
		}
		break;

	case 8:
#ifdef JP
		if (name) return "スピード・モンスター";
		if (desc) return "モンスター1体を加速させる。";
#else
		if (name) return "Haste Monster";
		if (desc) return "Hastes a monster.";
#endif
    
		{
			int power = mlev;

			if (info) info_power(power);

			if (cast)
			{
				bool result;

				/* Temporary enable target_pet option */
				bool old_target_pet = target_pet;
				target_pet = TRUE;

				result = get_aim_dir(&dir);

				/* Restore target_pet option */
				target_pet = old_target_pet;

				if (!result) return NULL;

				speed_monster(dir, power);
			}
		}
		break;

	case 9:
#ifdef JP
		if (name) return "動物友和";
		if (desc) return "視界内の全ての動物を魅了する。抵抗されると無効。";
#else
		if (name) return "Animal Friendship";
		if (desc) return "Attempts to charm all animals in sight.";
#endif
    
		{
			int power = mlev + 50;

			if (info) return info_power(power);

			if (cast)
			{
				charm_animals(power);
			}
		}
		break;

	case 10:
#ifdef JP
		if (name) return "増殖阻止";
		if (desc) return "この階の増殖するモンスターが増殖できなくなる。";
#else
		if (name) return "Sterilization";
		if (desc) return "Prevents any breeders on current level from breeding.";
#endif
    
		{
			if (cast)
			{
				num_repro += MAX_REPRO;
			}
		}
		break;

	case 11:
#ifdef JP
		if (name) return "ハウンド召喚";
		if (desc) return "1グループのハウンドを召喚する。";
#else
		if (name) return "Summon Hounds";
		if (desc) return "Summons a group of hounds.";
#endif
    
		{
			if (cast)
			{
				u32b mode = PM_ALLOW_GROUP | PM_FORCE_PET;
				if (!summon_specific(-1, py, px, mlev * 2 / 3 + randint1(mlev/2), SUMMON_HOUND, mode))
				{
#ifdef JP
					msg_print("ハウンドは現れなかった。");
#else
					msg_print("No hounds arrive.");
#endif
				}
			}
		}
		break;

	case 12:
#ifdef JP
		if (name) return "友への愛";
		if (desc) return "自分の周辺のペットの体力を回復し、麻痺、恐怖、朦朧、混乱を治療する。";
#else
		if (name) return "Heal Monsters";
		if (desc) return "Heal nearby monsters.";
#endif
    
		{
			int heal = 200;

			if (info) info_heal(0, 0, heal);

			if (cast)
			{
				int     i, x, y;
				u32b    flg = PROJECT_JUMP | PROJECT_KILL | PROJECT_HIDE;
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

					/* Skip distant monsters */
					if (distance(py, px, y, x) > 3) continue;

					/* Player's pets only */
					if (!is_pet(m_ptr)) continue;

					/* Jump directly to the target monster */
					project(0, 0, y, x, heal, GF_OLD_HEAL, flg, MODIFY_ELEM_MODE_NONE);
				}
			}
		}
		break;

	case 13:
#ifdef JP
		if (name) return "動物召喚";
		if (desc) return "複数の動物を召喚する。";
#else
		if (name) return "Summon Animals";
		if (desc) return "Summons animals.";
#endif
    
		{
			if (cast)
			{
				int  i;
				bool fail = TRUE;

				for (i = 0; i < 1 + ((mlev - 15)/ 10); i++)
				{
					if (summon_specific(-1, py, px, mlev, SUMMON_ANIMAL, (PM_ALLOW_GROUP | PM_FORCE_PET)))
						fail = FALSE;
				}

				if (fail)
#ifdef JP
					msg_print("動物は現れなかった。");
#else
					msg_print("No animals arrive.");
#endif
			}
		}
		break;

	case 14:
#ifdef JP
		if (name) return "真・友人治療";
		if (desc) return "モンスター1体の体力を大幅に回復し、石化、麻痺、恐怖、朦朧、混乱を治療する。";
#else
		if (name) return "*Heal Monster*";
		if (desc) return "Heal a monster.";
#endif
    
		{
			int heal = 1000;

			if (info) return info_heal(0, 0, heal);

			if (cast)
			{
				bool result;
				bool old_target_pet = target_pet;

				result = get_aim_dir(&dir);

				target_pet = old_target_pet;

				if (!result) return NULL;

				fire_ball(GF_STAR_HEAL, dir, heal, 0, FALSE);
			}
		}
		break;

	}

	return "";
}


static cptr do_witch_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

	int dir;
	int mlev = p_ptr->magic_exp[REALM_WITCH]/10;
	int pstat;
	s16b chosen_elem;

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "赤外線視力";
		if (desc) return "一定時間、赤外線視力が増強される。";
#else
		if (name) return "Infravision";
		if (desc) return "Gives infravision for a while.";
#endif
    
		{
			int base = 100;

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_tim_infra(base + randint1(base), FALSE);
			}
		}
		break;

	case 1:
#ifdef JP
		if (name) return "チャーム";
		if (desc) return "魅了の球を放つ。魅力が高いと視界内の全てのモンスターを魅了する。抵抗されると無効。";
#else
		if (name) return "Charm Monster";
		if (desc) return "Attempts to charm a monster.";
#endif
    
		{
			int power, rad;
			pstat = p_ptr->stat_use[A_CHR];

			if (pstat < (18 + 180))
			{
				if (pstat >= (18 + 150))
				{
					power = mlev + 70;
					rad = 4;
				}
				else if (pstat >= (18 + 100))
				{
					power = mlev + 60;
					rad = 3;
				}
				else
				{
					power = mlev + 50;
					rad = 2;
				}
			}
			else power = mlev + 50;

			if (info) return info_power(power);

			if (cast)
			{
				if (pstat < (18 + 180))
				{
					if (!get_aim_dir(&dir)) return NULL;

					fire_ball(GF_CHARM, dir, power, rad, FALSE);
				}
				else
				{
					charm_monsters(power);
				}
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "混乱の歌";
		if (desc) return "視界内の全てのモンスターを混乱させる。抵抗されると無効。";
#else
		if (name) return "Illusion Pattern";
		if (desc) return "Attempts to confuse all monsters in sight.";
#endif
    
		{
			int power = mlev + 50;

			if (info) return info_power(power);

			if (cast)
			{
				confuse_monsters(power);
			}
		}
		break;

	case 3:
#ifdef JP
		if (name) return "ヒーロー気分";
		if (desc) return "一定時間、ヒーロー気分になる。";
#else
		if (name) return "Heroism";
		if (desc) return "Removes fear, and gives bonus to hit and 10 more HP for a while.";
#endif
    
		{
			int base = 25;

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_hero(randint1(base) + base, FALSE);
				hp_player(10);
				set_afraid(0);
			}
		}
		break;

	case 4:
#ifdef JP
		if (name) return "デフエレメント";
		if (desc) return "光源が照らしている範囲か部屋全体の任意のエレメントを強め、その反対のエレメントを弱める。";
#else
		if (name) return "Deft element";
		if (desc) return "Increase choose element, decrease opposite element nearby area and the inside of a room permanently.";
#endif
    
		{
			int amount = 2;
			int rad = (mlev / 10) + 1;
			pstat = p_ptr->stat_use[A_INT];

			if (pstat >= (18 + 100)) amount++;
			if (pstat >= (18 + 150)) amount++;
			if (pstat >= (18 + 200)) amount++;

			if (info) info_power(amount);

			if (cast)
			{
				chosen_elem = choose_elem();
				if (chosen_elem == NO_ELEM)
				{
#ifdef JP
					msg_print("エレメントを活性化させるのをやめた。");
#else
					msg_print("You cancel enhancing the elememt.");
#endif
					return NULL;
				}

				inc_area_elem(0, chosen_elem, amount, rad, TRUE);
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "風の護り";
		if (desc) return "一定時間、射撃を回避する能力を得る。知能が高いと魔法のボルトやロケットも回避する。";
#else
		if (name) return "Wind's gurad";
		if (desc) return "Satisfies hunger.";
#endif
    
		{
			int sides = mlev;
			int base = 20;

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_wind_guard(randint1(sides)+base, FALSE);
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "クリアブラッド";
		if (desc) return "怪我を全快させ、毒を体から完全に取り除き、体力を少し回復させる。";
#else
		if (name) return "Clear blood";
		if (desc) return "Heals all cut and poison status. Heals HP a little.";
#endif
    
		{
			int dice = 4;
			int sides = 8;

			if (info) return info_heal(dice, sides, 0);

			if (cast)
			{
				set_poisoned(0);
				set_cut(0);
				hp_player(damroll(dice, sides));
			}
		}
		break;

	case 7:
#ifdef JP
		if (name) return "クイックムーブ";
		if (desc) return "一定時間、加速する。";
#else
		if (name) return "Quick Move";
		if (desc) return "Hastes you for a while.";
#endif
    
		{
			int base = mlev;
			int sides = 25 + mlev;

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_fast(randint1(sides) + base, FALSE);
			}
		}
		break;

	case 8:
#ifdef JP
		if (name) return "スロウムーブ";
		if (desc) return "モンスター1体を減速させる。抵抗されると無効。";
#else
		if (name) return "Slow Monster";
		if (desc) return "Attempts to slow a monster.";
#endif
    
		{
			int power = mlev;

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				slow_monster(dir, power);
			}
		}
		break;

	case 9:
#ifdef JP
		if (name) return "窒息の手";
		if (desc) return "ごく小さな生命力吸収の球を放つ。しばしば沈黙も引き起こす。";
#else
		if (name) return "Choked hand";
		if (desc) return "Fire a ball of life drain.";
#endif
    
		{
			int dice = 3;
			int sides = 5;
			int base = mlev + mlev / 2;
			int power = mlev + 50;

			if (info) return info_damage(dice, sides, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				if (fire_ball(GF_OLD_DRAIN, dir, damroll(dice, sides) + base, 0, FALSE))
				{
					if (!one_in_(3)) fire_ball(GF_SILENT, dir, power, 0, FALSE);
				}
			}
		}
		break;

	case 10:
#ifdef JP
		if (name) return "スタンスローター";
		if (desc) return "麻痺の球を放つ。";
#else
		if (name) return "Stun slaughter";
		if (desc) return "Fires a ball of stun.";
#endif
    
		{
			int power = mlev + 50;
			int rad = 1;
			if (pstat >= (18 + 100))
			{
				power = mlev + 60;
				rad++;
			}
			if (pstat >= (18 + 150))
			{
				power = mlev + 70;
				rad++;
			}

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_STASIS, dir, power, rad, FALSE);
			}
		}
		break;

	case 11:
#ifdef JP
		if (name) return "爆発のルーン";
		if (desc) return "自分のいる床の上に、モンスターが通ると爆発してダメージを与えるルーンを描く。";
#else
		if (name) return "Explosive Rune";
		if (desc) return "Sets a glyph under you. The glyph will explode when a monster moves on it.";
#endif
    
		{
			if (cast)
			{
				explosive_rune(mlev);
			}
		}
		break;

	case 12:
#ifdef JP
		if (name) return "ジャンプウォール";
		if (desc) return "1マスの壁やモンスターを飛び越える。着地地点が床ではない場合は何も起きない。";
#else
		if (name) return "Jump Wall";
		if (desc) return "";
#endif
    
		{
			if (cast)
			{
				if (!jump_wall()) return NULL;
			}
		}
		break;

	case 13:
#ifdef JP
		if (name) return "ポイズンクラウド";
		if (desc) return "指定した地点から一定範囲に毒攻撃を行う。";
#else
		if (name) return "Poison cloud";
		if (desc) return "Fire a ball of poison.";
#endif
    
		{
			int sides = mlev;
			int base = 10 + mlev * 2;
			int rad = (pstat >= (18 + 150)) ? 3 : 2;

			if (info) return info_damage(1, sides, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_POIS, dir, base + randint1(sides), rad, TRUE);
			}
		}
		break;

	case 14:
#ifdef JP
		if (name) return "ダミー";
		if (desc) return "自分の場所を欺くための人形を足元に置く。";
#else
		if (name) return "Dummy";
		if (desc) return "Sets a dummy under you.";
#endif
    
		{
			if (cast)
			{
				if (!set_decoy()) return NULL;
			}
		}
		break;

	case 15:
#ifdef JP
		if (name) return "全耐性";
		if (desc) return "一定時間、酸、電撃、炎、冷気、毒に対する耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Resistance";
		if (desc) return "Gives resistance to fire, cold, electricity, acid and poison for a while. These resistances can be added to which from equipment for more powerful resistances.";
#endif
    
		{
			int base = 20;

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_oppose_acid(randint1(base) + base, FALSE);
				set_oppose_elec(randint1(base) + base, FALSE);
				set_oppose_fire(randint1(base) + base, FALSE);
				set_oppose_cold(randint1(base) + base, FALSE);
				set_oppose_pois(randint1(base) + base, FALSE);
			}
		}
		break;

	case 16:
#ifdef JP
		if (name) return "マジカルウェポン";
		if (desc) return "一定時間、攻撃力と命中率が強化され、武器の重さをほとんど感じなくなり、武器に冷気、炎、電撃、酸、毒のいずれかの属性をつける。利き腕の武器を外すと無効。";
#else
		if (name) return "Magical weapon";
		if (desc) return "";
#endif
    
		{
			int base = mlev / 2;

			if (info) return info_duration(base, base);

			if (cast)
			{
				if (!choose_magical_weapon()) return NULL;
			}
		}
		break;

	case 17:
#ifdef JP
		if (name) return "メルトウェポン";
		if (desc) return "モンスター1体の直接攻撃力を弱める。";
#else
		if (name) return "Melt Weapon";
		if (desc) return "";
#endif
    
		{
			if (cast)
			{
				int power = mlev * 2;
				if (!cast_melt_weapon(power)) return NULL;
			}
		}
		break;

	case 18:
#ifdef JP
		if (name) return "プレイエレメント";
		if (desc) return "光源が照らしている範囲か部屋全体の任意のエレメントを強める。";
#else
		if (name) return "Pray element";
		if (desc) return "Increase choose element nearby area and the inside of a room permanently.";
#endif
    
		{
			int amount = 4;
			int rad = (mlev / 10) + 1;
			pstat = p_ptr->stat_use[A_INT];

			if (pstat >= (18 + 100)) amount += 2;
			if (pstat >= (18 + 150)) amount += 2;
			if (pstat >= (18 + 200)) amount += 2;

			if (info) info_power(amount);

			if (cast)
			{
				chosen_elem = choose_elem();
				if (chosen_elem == NO_ELEM)
				{
#ifdef JP
					msg_print("エレメントを活性化させるのをやめた。");
#else
					msg_print("You cancel enhancing the elememt.");
#endif
					return NULL;
				}

				inc_area_elem(0, chosen_elem, amount, rad, FALSE);
			}
		}
		break;

	case 19:
#ifdef JP
		if (name) return "救援召喚";
		if (desc) return "自分の種族に対応したシンボルのモンスターを召喚する。";
#else
		if (name) return "Summon aid";
		if (desc) return "Summon player's race symbol monsters.";
#endif
    
		{
			if (cast)
			{
#ifdef JP
				msg_print("援軍を召喚した。");
#else
				msg_print("You summon minions.");
#endif
				summon_kin_player(mlev * 2 / 3 + randint1(mlev/2), py, px, PM_FORCE_PET);
			}
		}
		break;

	case 20:
#ifdef JP
		if (name) return "*元素*ビーム";
		if (desc) return "自分の現在のエレメントと同じ*元素*のビームを放つ。";
#else
		if (name) return "*element* beam";
		if (desc) return "Fire a beam of *element*.";
#endif
    
		{
			int sides = 100;
			int base = mlev * 2;

			if (info) return info_damage(1, sides, base);

			if (cast)
			{
				int pure_elem_typ = GF_GODLY_SPEAR;
				switch (get_cur_pelem())
				{
				case NO_ELEM:
					pure_elem_typ = GF_MANA;
					break;
				case ELEM_FIRE:
					pure_elem_typ = GF_PURE_FIRE;
					break;
				case ELEM_AQUA:
					pure_elem_typ = GF_PURE_AQUA;
					break;
				case ELEM_EARTH:
					pure_elem_typ = GF_PURE_EARTH;
					break;
				case ELEM_WIND:
					pure_elem_typ = GF_PURE_WIND;
					break;
				}

				if (!get_aim_dir(&dir)) return NULL;

				fire_beam(pure_elem_typ, dir, base + randint1(sides));
			}
		}
		break;

	case 21:
#ifdef JP
		if (name) return "魔法の鎧";
		if (desc) return "一定時間、魔法防御力とACが上がり、混乱と盲目の耐性、反射能力、麻痺知らず、浮遊を得る。";
#else
		if (name) return "Magical armor";
		if (desc) return "Gives resistance to magic, bonus to AC, resistance to confusion, blindness, reflection, free action and levitation for a while.";
#endif
    
		{
			int base = 20;

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_magicdef(randint1(base) + base, FALSE);
			}
		}
		break;

	case 22:
#ifdef JP
		if (name) return "ミダスの手";
		if (desc) return "アイテム1つをお金に変える。";
#else
		if (name) return "Midas Touch";
		if (desc) return "Turn object to gold.";
#endif
    
		{
			if (cast)
			{
				if (!alchemy()) return NULL;
			}
		}
		break;

	case 23:
#ifdef JP
		if (name) return "モルド召喚";
		if (desc) return "複数のモルドを召喚する。";
#else
		if (name) return "Summon molds";
		if (desc) return "Summons some molds.";
#endif
    
		{
			if (cast)
			{
				int  i;
				bool fail = TRUE;

				for (i = 0; i < 1 + ((mlev - 15)/ 10); i++)
				{
					if (summon_specific(-1, py, px, mlev * 2 / 3 + randint1(mlev/2), SUMMON_MOLD, (PM_ALLOW_GROUP | PM_FORCE_PET)))
						fail = FALSE;
				}

				if (fail)
#ifdef JP
					msg_print("モルドは現れなかった。");
#else
					msg_print("No molds arrive.");
#endif
			}
		}
		break;

	case 24:
#ifdef JP
		if (name) return "コールストーム";
		if (desc) return "天候を悪化させる。";
#else
		if (name) return "Call storm";
		if (desc) return "Weather to bad.";
#endif
    
		{
			int power = 2;

			if (pstat >= (18 + 150)) power++;
			if (pstat >= (18 + 200)) power++;

			if (cast)
			{
				msg_print("天候が荒れていく...");

				set_weather(power, power, power);
			}
		}
		break;

	case 25:
#ifdef JP
		if (name) return "魔力消去";
		if (desc) return "自分またはモンスター1体にかかった魔法を解除する。";
#else
		if (name) return "Dispel magic";
		if (desc) return "";
#endif
    
		{
			if (cast)
			{
				if (!cast_dispel_magic()) return NULL;
			}
		}
		break;

	case 26:
#ifdef JP
		if (name) return "マーシーレイン";
		if (desc) return "自分を中心とした一定範囲に体力を大幅に回復させ、傷、毒、朦朧、混乱から全快させる雨を降らせる。";
#else
		if (name) return "Mercy Rain";
		if (desc) return "";
#endif
    
		{
			int base = 500;

			if (info) return info_heal(0, 0, base);

			if (cast)
			{
				hp_player(base);
				project(0, 3, py, px, base, GF_OLD_HEAL, PROJECT_KILL | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_NONE);
				set_poisoned(0);
				set_confused(0);
				set_stun(0);
				set_cut(0);
			}
		}
		break;

	case 27:
#ifdef JP
		if (name) return "ペトロクラウド";
		if (desc) return "指定した地点から一定範囲に石化攻撃を行う。";
#else
		if (name) return "Petro Cloud";
		if (desc) return "Fires a ball of stonning.";
#endif
    
		{
			int base = 200 + mlev * 3;
			pstat = p_ptr->stat_use[A_INT];

			if (info) return info_damage(0, 0, base);

			if (cast)
			{
				int rad = (pstat >= (18 + 150)) ? 3 : 2;

				if (!get_aim_dir(&dir)) return FALSE;

				fire_ball(GF_STONE, dir, base, rad, FALSE);
			}
		}
		break;

	case 28:
#ifdef JP
		if (name) return "次元の扉";
		if (desc) return "短距離内の指定した場所にテレポートする。";
#else
		if (name) return "Dimension Door";
		if (desc) return "Teleport to given location.";
#endif
    
		{
			int range = mlev / 2 + 10;

			if (info) return info_range(range);

			if (cast)
			{
#ifdef JP
				msg_print("次元の扉が開いた。目的地を選んで下さい。");
#else
				msg_print("You open a dimensional gate. Choose a destination.");
#endif

				if (!dimension_door(mlev)) return NULL;
			}
		}
		break;

	case 29:
#ifdef JP
		if (name) return "チェンジエレメント";
		if (desc) return "自分またはモンスター1体のエレメントを反転させる。";
#else
		if (name) return "Change element";
		if (desc) return "Change element of player or monster.";
#endif
    
		{
			if (cast)
			{
				int power = mlev * 2;
				if (!cast_change_element(power)) return NULL;
			}
		}
		break;

	case 30:
#ifdef JP
		if (name) return "幽体化";
		if (desc) return "一定時間、壁を通り抜けることができ受けるダメージが軽減される幽体の状態に変身する。";
#else
		if (name) return "Wraithform";
		if (desc) return "Becomes wraith form which gives ability to pass walls and makes all damages half.";
#endif
    
		{
			int base = mlev / 2;

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_wraith_form(randint1(base) + base, FALSE);
			}
		}
		break;
	}

	return "";
}


static cptr do_drakonite_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

	int mlev = p_ptr->magic_exp[REALM_DRAKONITE]/10;

#ifdef JP
	static const char s_dam[] = "損傷:";
#else
	static const char s_dam[] = "dam ";
#endif

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "マーティライズ";
		if (desc) return "今まで倒したユニーク・モンスターを復活させる。クエストで出現したユニーク・モンスターには無効。";
#else
		if (name) return "Martyrs";
		if (desc) return "";
#endif
    
		{
			if (cast)
			{
				int i;
				bool *no_revive;
				bool revived = FALSE;

				/* Allocate the "no_revive" array */
				C_MAKE(no_revive, max_r_idx, bool);

				/* Scan the random quests */
				for (i = 1; i < max_quests; i++)
				{
					if (quest[i].r_idx) no_revive[quest[i].r_idx] = TRUE;
				}

				/* Scan the arena */
				for (i = 0; i < MAX_ARENA_MONS + 4; i++)
				{
					no_revive[arena_info[i].r_idx] = TRUE;
				}

				/* Scan the monster races */
				for (i = 0; i < max_r_idx; i++)
				{
					monster_race *r_ptr = &r_info[i];

					if (!r_ptr->name) continue;

					/* Unique monsters only */
					if (!(r_ptr->flags1 & RF1_UNIQUE)) continue;

					/* Questors don't revive */
					if (r_ptr->flags1 & RF1_QUESTOR) continue;

					/* Unique monster is alive, no need to revive */
					if (r_ptr->max_num) continue;

					/* This unique is stayed dead... */
					if (no_revive[i]) continue;

					/* The dead unique monster is come back!! */
					r_ptr->max_num = 1;
					revived = TRUE;
				}

				/* Free the "no_revive" array */
				C_KILL(no_revive, max_r_idx, bool);

				if (revived)
				{
	#ifdef JP
					msg_format("葬った仇敵達が帰ってくるのを感じる。");
	#else
					msg_format("You feel slain foes coming back.");
	#endif
					/* Window stuff */
					p_ptr->window |= (PW_MONSTER);

					if (p_ptr->materialize_cnt < MAX_SHORT) p_ptr->materialize_cnt++;
				}
			}
		}
		break;

	case 1:
#ifdef JP
		if (name) return "ドミニオン";
		if (desc) return "視界内の全てのモンスターを抵抗できずに確実に減速させる。ユニーク・モンスターは減速しない場合がある。";
#else
		if (name) return "Dominion";
		if (desc) return "";
#endif
    
		{
			int sides = mlev + 25;
			int base = mlev;
#ifdef JP
			if (info) return format("効力:%d+d%d", base, sides);
#else
			if (info) return format("power %d+d%d", base, sides);
#endif

			if (cast)
			{
				project_hack(GF_NEW_SLOW, randint1(sides) + base);
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "テンペスト";
		if (desc) return "視界内の範囲に対して*風*攻撃を行う。さらに、天候を悪化させる。";
#else
		if (name) return "Tempest";
		if (desc) return "";
#endif
    
		{
			int sides = mlev * 5;
			int base = mlev * 6;

			if (info) return format("%s%d+d%d", s_dam, base, sides);

			if (cast)
			{
				project(0, MAX_SIGHT, py, px, base + randint1(sides), GF_PURE_WIND, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
				set_weather(8, 8, 8);
			}
		}
		break;

	case 3:
#ifdef JP
		if (name) return "アニヒレーション";
		if (desc) return "視界内の範囲に対して*火炎*攻撃を行う。さらに、天候を良好化させる。";
#else
		if (name) return "Annihilation";
		if (desc) return "";
#endif
    
		{
			int sides = mlev * 5;
			int base = mlev * 6;

			if (info) return format("%s%d+d%d", s_dam, base, sides);

			if (cast)
			{
				project(0, MAX_SIGHT, py, px, base + randint1(sides), GF_PURE_FIRE, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
				set_weather(-8, -8, -8);
			}
		}
		break;

	case 4:
#ifdef JP
		if (name) return "メテオストライク";
		if (desc) return "視界内の範囲に対して*大地*攻撃を行い、混乱させる。";
#else
		if (name) return "Meteor Strike";
		if (desc) return "";
#endif
    
		{
			int sides = mlev * 5;
			int base = mlev * 6;
			int power = mlev + 50;

			if (info) return format("%s%d+d%d", s_dam, base, sides);

			if (cast)
			{
				project(0, MAX_SIGHT, py, px, base + randint1(sides), GF_PURE_EARTH, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
				confuse_monsters(power);
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "ホワイトミュート";
		if (desc) return "視界内の範囲に対して*水*攻撃を行い、麻痺させる。";
#else
		if (name) return "White Mute";
		if (desc) return "";
#endif
    
		{
			int sides = mlev * 5;
			int base = mlev * 6;
			int power = mlev + 50;

			if (info) return format("%s%d+d%d", s_dam, base, sides);

			if (cast)
			{
				project(0, MAX_SIGHT, py, px, base + randint1(sides), GF_PURE_AQUA, PROJECT_KILL | PROJECT_GRID | PROJECT_ITEM | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
				stasis_monsters(power);
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "リーンカーネイト";
		if (desc) return "全ての能力基本値、体力、MP、技能をいくらかの割合で引き継いだまま自分の最大レベルを1に戻す。";
#else
		if (name) return "Reincarnate";
		if (desc) return "Reincarnate.";
#endif
    
		{
			if (cast)
			{
#ifdef JP
				if (!get_check("レベル1のキャラクタに転生します。よろしいですか？ ")) return NULL;
#else
				if (!get_check("Reincarnate as level 1 character. Are you sure? ")) return NULL;
#endif
				reincarnation();
			}
		}
		break;

	case 7:
#ifdef JP
		if (name) return "スナップドラゴン";
		if (desc) return "自分を能力に応じた武器に変える。不正のないプレイヤーが変身した武器は子孫が発見できる可能性がある。";
#else
		if (name) return "Snapdragon";
		if (desc) return "Turn player to runeweapon.";
#endif
    
		{
			if (cast)
			{
				int i;

#ifdef JP
				if (!get_check("本当に武器に変化しますか？")) return NULL;
#else
				if (!get_check("Do you really want to commit change into a weapon? ")) return NULL;
#endif
				/* Special Verification for Snap Dragon */
#ifdef JP
				prt("確認のため '@' を押して下さい。", 0, 0);
#else
				prt("Please verify CHANGE by typing the '@' sign: ", 0, 0);
#endif

				flush();
				i = inkey();
				prt("", 0, 0);
				if (i != '@') return NULL;

				snap_dragon();
			}
		}
		break;

	}

	return "";
}


static cptr do_crusade_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;

	int	dir;
	int mlev = p_ptr->magic_exp[REALM_CRUSADE]/10;
	int pstat = p_ptr->stat_use[A_WIS];;

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "邪悪存在感知";
		if (desc) return "近くの邪悪なモンスターを感知する。";
#else
		if (name) return "Detect Evil";
		if (desc) return "Detects all evil monsters in your vicinity.";
#endif
    
		{
			int rad = DETECT_RAD_DEFAULT;

			if (info) return info_radius(rad);

			if (cast)
			{
				detect_monsters_evil(rad);
			}
		}
		break;

	case 1:
#ifdef JP
		if (name) return "ライトニングボウ";
		if (desc) return "閃光のビームを放つ。";
#else
		if (name) return "Litghtning bow";
		if (desc) return "Fire a beam of lite.";
#endif
    
		{
			int dice = 3 + (mlev - 1) /5;
			int sides = 4;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_beam(GF_LITE, dir, damroll(dice, sides));
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "聖域";
		if (desc) return "隣接した全てのモンスターを眠らせる。抵抗されると無効。";
#else
		if (name) return "Sanctuary";
		if (desc) return "Attempts to sleep monsters in the adjacent squares.";
#endif
    
		{
			int power = mlev;

			if (info) return info_power(power);

			if (cast)
			{
				sleep_monsters_touch(power);
			}
		}
		break;

	case 3:
#ifdef JP
		if (name) return "身体浄化";
		if (desc) return "傷、毒、朦朧から全快する。";
#else
		if (name) return "Purify";
		if (desc) return "Heals all cut, stun and poison status.";
#endif
    
		{
			if (cast)
			{
				set_cut(0);
				set_poisoned(0);
				set_stun(0);
			}
		}
		break;

	case 4:
#ifdef JP
		if (name) return "透明視認";
		if (desc) return "一定時間、透明なものが見えるようになる。";
#else
		if (name) return "See Invisible";
		if (desc) return "Gives see invisible for a while.";
#endif
    
		{
			int base = 24;

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_tim_invis(randint1(base) + base, FALSE);
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "スターダスト";
		if (desc) return "ターゲット付近に閃光のボルトを連射する。";
#else
		if (name) return "Star Dust";
		if (desc) return "Fires many bolts of light near the target.";
#endif
    
		{
			int dice = 3 + (mlev - 1) / 9;
			int sides = 2;

			if (info) return info_multi_damage_dice(dice, sides);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_blast(GF_LITE, dir, dice, sides, 10, 3);
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "封魔";
		if (desc) return "邪悪なモンスターの動きを止める。";
#else
		if (name) return "Arrest";
		if (desc) return "Attempts to paralyze an evil monster.";
#endif
    
		{
			int power = mlev * 2;

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				stasis_evil(dir, power);
			}
		}
		break;

	case 7:
#ifdef JP
		if (name) return "聖なる光球";
		if (desc) return "聖なる力をもつ宝珠を放つ。邪悪なモンスターに対して大きなダメージを与えるが、善良なモンスターには効果がない。";
#else
		if (name) return "Holy Orb";
		if (desc) return "Fires a ball with holy power. Hurts evil monsters greatly, but don't effect good monsters.";
#endif
    
		{
			int dice = 3;
			int sides = 6;
			int rad = (mlev < 30) ? 2 : 3;
			int base = mlev + mlev / 2;

			if (info) return info_damage(dice, sides, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_HOLY_FIRE, dir, damroll(dice, sides) + base, rad, FALSE);
			}
		}
		break;

	case 8:
#ifdef JP
		if (name) return "フェイス";
		if (desc) return "視界内の全てのアンデッドにダメージを与え、テレポートさせる。";
#else
		if (name) return "Face";
		if (desc) return "Damages all undead and demons in sight, and scares all evil monsters in sight.";
#endif
    
		{
			int sides = mlev * 4;

			if (info) return info_damage(1, sides, 0);

			if (cast)
			{
				dispel_undead(randint1(sides));
				project_hack_undead(GF_AWAY_ALL, sides);
			}
		}
		break;

	case 9:
#ifdef JP
		if (name) return "裁きの雷";
		if (desc) return "強力な電撃のボルトを放つ。";
#else
		if (name) return "Judgment Thunder";
		if (desc) return "Fires a powerful bolt of lightning.";
#endif
    
		{
			int dam = mlev * 5;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_bolt(GF_ELEC, dir, dam);
			}
		}
		break;

	case 10:
#ifdef JP
		if (name) return "神の怒り";
		if (desc) return "ターゲットの周囲に分解の球を多数落とす。";
#else
		if (name) return "Wrath of the God";
		if (desc) return "Drops many balls of disintegration near the target.";
#endif
    
		{
			int dam = mlev * 3 + 25;
			int rad = 2;

			if (info) return info_multi_damage(dam);

			if (cast)
			{
				if (!cast_wrath_of_the_god(dam, rad)) return NULL;
			}
		}
		break;

	case 11:
#ifdef JP
		if (name) return "マジックボム";
		if (desc) return "自分を中心とした一定範囲に聖なる攻撃を行い、自分の体力の1/4を失う。";
#else
		if (name) return "Magic bomb";
		if (desc) return "Fires a huge ball of holy power nearby.";
#endif
    
		{
			int dam = p_ptr->chp;
			int rad = 4;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				project(0, rad, py, px, dam, GF_HOLY_FIRE, PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL | PROJECT_NO_REDUCE, MODIFY_ELEM_MODE_MAGIC);
#ifdef JP
				take_hit(DAMAGE_USELIFE, p_ptr->chp / 4, "自殺的なマジックボム");
#else
				take_hit(DAMAGE_USELIFE, p_ptr->chp / 4, "a suicidal Magic Bomb");
#endif
			}
		}
		break;

	case 12:
#ifdef JP
		if (name) return "士気高揚";
		if (desc) return "一定時間、ヒーロー気分になる。";
#else
		if (name) return "Heroism";
		if (desc) return "Removes fear, and gives bonus to hit and 10 more HP for a while.";
#endif
    
		{
			int base = 25;

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_hero(randint1(base) + base, FALSE);
				hp_player(10);
				set_afraid(0);
			}
		}
		break;

	case 13:
#ifdef JP
		if (name) return "イクソシズム";
		if (desc) return "アンデッドを現在の階から消し去る球を放つ。抵抗されると無効。";
#else
		if (name) return "Exorcism";
		if (desc) return "Eliminates undead monsters, exhausting you.  Powerful or unique monsters may be able to resist.";
#endif
    
		{
			int dummy = (pstat >= (18 + 150)) ? 3 : 2;
			int power = mlev * dummy;

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_GENOCIDE_UNDEAD, dir, power, dummy, FALSE);
			}
		}
		break;

	case 14:
#ifdef JP
		if (name) return "トランキライズ";
		if (desc) return "狂戦士化し、恐怖を除去する。";
#else
		if (name) return "Berserk";
		if (desc) return "Gives bonus to hit and HP, immunity to fear for a while. But decreases AC.";
#endif
    
		{
			int base = 25;

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_shero(randint1(base) + base, FALSE);
				hp_player(30);
				set_afraid(0);
			}
		}
		break;

	case 15:
#ifdef JP
		if (name) return "聖なるオーラ";
		if (desc) return "一定時間、邪悪なモンスターを傷つける聖なるオーラを得る。";
#else
		if (name) return "Holy Aura";
		if (desc) return "Gives aura of holy power which injures evil monsters which attacked you for a while.";
#endif
    
		{
			int base = 20;

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_tim_sh_holy(randint1(base) + base, FALSE);
			}
		}
		break;

	case 16:
#ifdef JP
		if (name) return "聖なる刃";
		if (desc) return "通常の武器に滅邪の属性をつける。";
#else
		if (name) return "Holy Blade";
		if (desc) return "Makes current weapon especially deadly against evil monsters.";
#endif
    
		{
			if (cast)
			{
				brand_weapon(EGO_KILL_EVIL);
			}
		}
		break;

	case 17:
#ifdef JP
		if (name) return "シャイニング";
		if (desc) return "自分の体力の1/3の威力の聖なる攻撃を行う。抵抗されると無効。";
#else
		if (name) return "Shining";
		if (desc) return "Eliminates all nearby undead monsters, exhausting you.  Powerful or unique monsters may be able to resist.";
#endif
    
		{
			int base = mlev + 60;

			if (info) return info_damage(0, 0, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_SHINING, dir, base, 0, FALSE);
			}
		}
		break;

	case 18:
#ifdef JP
		if (name) return "聖なる御言葉";
		if (desc) return "視界内の邪悪な存在に大きなダメージを与え、体力を回復し、毒、恐怖、朦朧状態、負傷から全快する。";
#else
		if (name) return "Holy Word";
		if (desc) return "Damages all evil monsters in sight, heals HP somewhat, and completely heals poison, fear, stun and cut status.";
#endif
    
		{
			int dam_sides = mlev * 6;
			int heal = 100;

#ifdef JP
			if (info) return format("損:1d%d/回%d", dam_sides, heal);
#else
			if (info) return format("dam:d%d/h%d", dam_sides, heal);
#endif

			if (cast)
			{
				dispel_evil(randint1(dam_sides));
				hp_player(heal);
				set_afraid(0);
				set_poisoned(0);
				set_stun(0);
				set_cut(0);
			}
		}
		break;

	case 19:
#ifdef JP
		if (name) return "スターバースト";
		if (desc) return "巨大な閃光の球を放つ。";
#else
		if (name) return "Star Burst";
		if (desc) return "Fires a huge ball of powerful light.";
#endif
    
		{
			int dam = 100 + mlev * 2;
			int rad = 4;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_LITE, dir, dam, rad, FALSE);
			}
		}
		break;

	case 20:
#ifdef JP
		if (name) return "ハルマゲドン";
		if (desc) return "周辺のアイテム、モンスター、地形を破壊する。";
#else
		if (name) return "Armageddon";
		if (desc) return "Destroy everything in nearby area.";
#endif
    
		{
			int base = 13;
			int sides = 5;

			if (cast)
			{
				destroy_area(py, px, base + randint0(sides));
			}
		}
		break;

	case 21:
#ifdef JP
		if (name) return "目には目を";
		if (desc) return "一定時間、自分がダメージを受けたときに攻撃を行ったモンスターに対して同等のダメージを与える。";
#else
		if (name) return "An Eye for an Eye";
		if (desc) return "Gives special aura for a while. When you are attacked by a monster, the monster are injured with same amount of damage as you take.";
#endif
    
		{
			int base = 10;

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_tim_eyeeye(randint1(base) + base, FALSE);
			}
		}
		break;

	case 22:
#ifdef JP
		if (name) return "ホーリーランス";
		if (desc) return "聖なるビームを放つ。";
#else
		if (name) return "Holy lance";
		if (desc) return "Fires a huge beam of holy.";
#endif
    
		{
			int dam = 200 + mlev * 2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_beam(GF_HOLY_FIRE, dir, dam);
			}
		}
		break;

	case 23:
#ifdef JP
		if (name) return "スターティアラ";
		if (desc) return "隣接するモンスターに聖なるダメージを与え、視界内のモンスターにダメージ、減速、朦朧、混乱、恐怖、眠りを与える。さらに体力を回復する。";
#else
		if (name) return "Star Tiara";
		if (desc) return "Damages all adjacent monsters with holy power. Damages and attempt to slow, stun, confuse, scare and freeze all monsters in sight. And heals HP.";
#endif
    
		{
			int b_dam = mlev * 11;
			int d_dam = mlev * 4;
			int heal = 100;
			int power = mlev * 4;

#ifdef JP
			if (info) return format("回%d/損%d+%d", heal, d_dam, b_dam/2);
#else
			if (info) return format("h%d/dm%d+%d", heal, d_dam, b_dam/2);
#endif

			if (cast)
			{
				project(0, 1, py, px, b_dam, GF_HOLY_FIRE, PROJECT_KILL, MODIFY_ELEM_MODE_MAGIC);
				dispel_monsters(d_dam);
				slow_monsters(mlev);
				stun_monsters(power);
				confuse_monsters(power);
				turn_monsters(power);
				stasis_monsters(power);
				hp_player(heal);
			}
		}
		break;

	case 24:
#ifdef JP
		if (name) return "聖戦";
		if (desc) return "視界内の善良なモンスターをペットにしようとし、ならなかった場合及び善良でないモンスターを恐怖させる。さらに多数の加速された騎士を召喚し、ヒーロー、祝福、加速、対邪悪結界を得る。";
#else
		if (name) return "Crusade";
		if (desc) return "Attempts to charm all good monsters in sight, and scare all non-charmed monsters, and summons great number of knights, and gives heroism, bless, speed and protection from evil.";
#endif
    
		{
			if (cast)
			{
				int base = 25;
				int sp_sides = 20 + mlev;
				int sp_base = mlev;

				int i;
				crusade(mlev * 4);
				for (i = 0; i < 12; i++)
				{
					int attempt = 10;
					int my, mx;

					while (attempt--)
					{
						scatter(&my, &mx, py, px, 4, 0);

						/* Require empty grids */
						if (cave_empty_bold2(my, mx)) break;
					}
					if (attempt < 0) continue;
					summon_specific(-1, my, mx, mlev, SUMMON_HUMANS, (PM_ALLOW_GROUP | PM_FORCE_PET | PM_HASTE | PM_ALLOW_UNIQUE));
				}
				set_hero(randint1(base) + base, FALSE);
				set_blessed(randint1(base) + base, FALSE);
				set_fast(randint1(sp_sides) + sp_base, FALSE);
				set_protevil(randint1(base) + base, FALSE);
				set_afraid(0);
			}
		}
		break;
	}

	return "";
}


/*
 * Do everything for each spell
 */
cptr do_spell(int realm, int spell, int mode)
{
	switch (realm)
	{
	case REALM_MAGERY:     return do_magery_spell(spell, mode);
	case REALM_FIRE:  return do_fire_spell(spell, mode);
	case REALM_AQUA:   return do_aqua_spell(spell, mode);
	case REALM_EARTH:    return do_earth_spell(spell, mode);
	case REALM_WIND:    return do_wind_spell(spell, mode);
	case REALM_HOLY:   return do_holy_spell(spell, mode);
	case REALM_DEATH:    return do_death_spell(spell, mode);
	case REALM_SYMBIOTIC:  return do_symbiotic_spell(spell, mode);
	case REALM_WITCH:   return do_witch_spell(spell, mode);
	case REALM_DRAKONITE:    return do_drakonite_spell(spell, mode);
	case REALM_CRUSADE:  return do_crusade_spell(spell, mode);
	}

	return NULL;
}
