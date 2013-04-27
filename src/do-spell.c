/* File: do-spell.c */

/* Purpose: Do everything for each spell */

#include "angband.h"

/* Hack: Increase spell power! */
static int _current_realm_hack = 0;

int spell_power_aux(int pow, int bonus)
{
	return pow + pow*bonus/13;
}

int spell_power(int pow)
{
	int tmp = p_ptr->spell_power;
	if (p_ptr->tim_blood_rite)
		tmp += 7;
	if (_current_realm_hack && _current_realm_hack == p_ptr->easy_realm1)
		tmp += 2;
	return spell_power_aux(pow, tmp);
}

int device_power_aux(int pow, int bonus)
{
	return pow + pow*bonus/20;
}

int device_power(int pow)
{
	return device_power_aux(pow, p_ptr->device_power);
}

int spell_cap_aux(int cap, int bonus)
{
	return cap + cap*bonus/13;
}

int spell_cap(int cap)
{
	return spell_cap_aux(cap, p_ptr->spell_cap);
}

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
cptr info_damage(int dice, int sides, int base)
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
cptr info_duration(int base, int sides)
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
cptr info_range(int range)
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
cptr info_heal(int dice, int sides, int base)
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
cptr info_delay(int base, int sides)
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
 * Generate power info string such as "power 100"
 */
cptr info_power(int power)
{
#ifdef JP
	return format("効力:%d", power);
#else
	return format("power %d", power);
#endif
}


/*
 * Generate power info string such as "power 1d100"
 */
static cptr info_power_dice(int dice, int sides)
{
#ifdef JP
	return format("効力:%dd%d", dice, sides);
#else
	return format("power %dd%d", dice, sides);
#endif
}


/*
 * Generate radius info string such as "rad 100"
 */
cptr info_radius(int rad)
{
#ifdef JP
	return format("半径:%d", rad);
#else
	return format("rad %d", rad);
#endif
}


/*
 * Generate weight info string such as "max wgt 15"
 */
static cptr info_weight(int weight)
{
#ifdef JP
	return format("最大重量:%d.%dkg", lbtokg1(weight/10), lbtokg2(weight/10));
#else
	return format("max wgt %d", weight/10);
#endif
}


/*
 * Prepare standard probability to become beam for fire_bolt_or_beam()
 */
int beam_chance(void)
{
	if (p_ptr->pclass == CLASS_MAGE || p_ptr->pclass == CLASS_BLOOD_MAGE || p_ptr->pclass == CLASS_NECROMANCER)
		return p_ptr->lev;
	if (p_ptr->pclass == CLASS_HIGH_MAGE || p_ptr->pclass == CLASS_SORCERER)
		return p_ptr->lev + 10;

	return p_ptr->lev / 2;
}


/*
 * Handle summoning and failure of trump spells
 */
static bool trump_summoning(int num, bool pet, int y, int x, int lev, int type, u32b mode)
{
	int plev = p_ptr->lev;

	int who;
	int i;
	bool success = FALSE;

	/* Default level */ 
	if (!lev) lev = spell_power(plev) + randint1(spell_power(plev));

	if (pet)
	{
		/* Become pet */
		mode |= PM_FORCE_PET;

		/* Only sometimes allow unique monster */
		if (mode & PM_ALLOW_UNIQUE)
		{
			/* Forbid often */
			if (randint1(50 + plev) >= plev / 10)
				mode &= ~PM_ALLOW_UNIQUE;
		}

		/* Player is who summons */
		who = -1;
	}
	else
	{
		/* Prevent taming, allow unique monster */
		mode |= PM_NO_PET;

		/* Behave as if they appear by themselfs */
		who = 0;
	}

	for (i = 0; i < num; i++)
	{
		if (summon_specific(who, y, x, lev, type, mode))
			success = TRUE;
	}

	if (!success)
	{
#ifdef JP
		msg_print("誰もあなたのカードの呼び声に答えない。");
#else
		if (p_ptr->pclass == CLASS_NECROMANCER)
			msg_print("Nobody answers to your foul summons.");
		else
			msg_print("Nobody answers to your Trump call.");
#endif
	}

	return success;
}


/*
 * This spell should become more useful (more controlled) as the
 * player gains experience levels.  Thus, add 1/5 of the player's
 * level to the die roll.  This eliminates the worst effects later on,
 * while keeping the results quite random.  It also allows some potent
 * effects only at high level.
 */
void cast_wonder(int dir)
{
	int plev = p_ptr->lev;
	int die = randint1(100) + plev / 5;
	int vir = virtue_number(V_CHANCE);

	if (vir)
	{
		if (p_ptr->virtues[vir - 1] > 0)
		{
			while (randint1(400) < p_ptr->virtues[vir - 1]) die++;
		}
		else
		{
			while (randint1(400) < (0-p_ptr->virtues[vir - 1])) die--;
		}
	}

	if (p_ptr->pclass == CLASS_WILD_TALENT)
		die += randint1(25 + p_ptr->lev/2);

	if (die < 26)
		chg_virtue(V_CHANCE, 1);

	if (die > 100)
	{
#ifdef JP
		msg_print("あなたは力がみなぎるのを感じた！");
#else
		msg_print("You feel a surge of power!");
#endif
	}

	if (die < 8) clone_monster(dir);
	else if (die < 14) speed_monster(dir);
	else if (die < 26) heal_monster(dir, damroll(4, 6));
	else if (die < 31) poly_monster(dir);
	else if (die < 36)
		fire_bolt_or_beam(beam_chance() - 10, GF_MISSILE, dir,
				  damroll(3 + ((plev - 1) / 5), 4));
	else if (die < 41) confuse_monster(dir, plev);
	else if (die < 46) fire_ball(GF_POIS, dir, 20 + (plev / 2), 3);
	else if (die < 51) (void)lite_line(dir);
	else if (die < 56)
		fire_bolt_or_beam(beam_chance() - 10, GF_ELEC, dir,
				  damroll(3 + ((plev - 5) / 4), 8));
	else if (die < 61)
		fire_bolt_or_beam(beam_chance() - 10, GF_COLD, dir,
				  damroll(5 + ((plev - 5) / 4), 8));
	else if (die < 66)
		fire_bolt_or_beam(beam_chance(), GF_ACID, dir,
				  damroll(6 + ((plev - 5) / 4), 8));
	else if (die < 71)
		fire_bolt_or_beam(beam_chance(), GF_FIRE, dir,
				  damroll(8 + ((plev - 5) / 4), 8));
	else if (die < 76) drain_life(dir, 75);
	else if (die < 81) fire_ball(GF_ELEC, dir, 30 + plev / 2, 2);
	else if (die < 86) fire_ball(GF_ACID, dir, 40 + plev, 2);
	else if (die < 91) fire_ball(GF_ICE, dir, 70 + plev, 3);
	else if (die < 96) fire_ball(GF_FIRE, dir, 80 + plev, 3);
	else if (die < 101) drain_life(dir, 100 + plev);
	else if (die < 104)
	{
		earthquake(py, px, 12);
	}
	else if (die < 106)
	{
		(void)destroy_area(py, px, 13 + randint0(5), 2 * p_ptr->lev);
	}
	else if (die < 108)
	{
		symbol_genocide(plev+50, TRUE);
	}
	else if (die < 110) dispel_monsters(120);
	else /* RARE */
	{
		dispel_monsters(150);
		slow_monsters(p_ptr->lev);
		sleep_monsters(p_ptr->lev);
		hp_player(300);
	}
}


static void cast_invoke_spirits(int dir)
{
	int plev = p_ptr->lev;
	int die = spell_power(randint1(100) + plev / 5);
	int vir = virtue_number(V_CHANCE);

	if (vir)
	{
		if (p_ptr->virtues[vir - 1] > 0)
		{
			while (randint1(400) < p_ptr->virtues[vir - 1]) die++;
		}
		else
		{
			while (randint1(400) < (0-p_ptr->virtues[vir - 1])) die--;
		}
	}

#ifdef JP
	msg_print("あなたは死者たちの力を招集した...");
#else
	msg_print("You call on the power of the dead...");
#endif
	if (die < 26)
		chg_virtue(V_CHANCE, 1);

	if (die > 100)
	{
#ifdef JP
		msg_print("あなたはおどろおどろしい力のうねりを感じた！");
#else
		msg_print("You feel a surge of eldritch force!");
#endif
	}


	if (die < 8)
	{
#ifdef JP
		msg_print("なんてこった！あなたの周りの地面から朽ちた人影が立ち上がってきた！");
#else
		msg_print("Oh no! Mouldering forms rise from the earth around you!");
#endif

		(void)summon_specific(0, py, px, dun_level, SUMMON_UNDEAD, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
		chg_virtue(V_UNLIFE, 1);
	}
	else if (die < 14)
	{
#ifdef JP
		msg_print("名状し難い邪悪な存在があなたの心を通り過ぎて行った...");
#else
		msg_print("An unnamable evil brushes against your mind...");
#endif

		fear_add_p(FEAR_TERRIFIED);
	}
	else if (die < 26)
	{
#ifdef JP
		msg_print("あなたの頭に大量の幽霊たちの騒々しい声が押し寄せてきた...");
#else
		msg_print("Your head is invaded by a horde of gibbering spectral voices...");
#endif

		set_confused(p_ptr->confused + randint1(4) + 4, FALSE);
	}
	else if (die < 31)
	{
		poly_monster(dir);
	}
	else if (die < 36)
	{
		fire_bolt_or_beam(beam_chance() - 10, GF_MISSILE, dir,
				  damroll(3 + ((plev - 1) / 5), 4));
	}
	else if (die < 41)
	{
		confuse_monster (dir, plev);
	}
	else if (die < 46)
	{
		fire_ball(GF_POIS, dir, 20 + (plev / 2), 3);
	}
	else if (die < 51)
	{
		(void)lite_line(dir);
	}
	else if (die < 56)
	{
		fire_bolt_or_beam(beam_chance() - 10, GF_ELEC, dir,
				  damroll(3+((plev-5)/4),8));
	}
	else if (die < 61)
	{
		fire_bolt_or_beam(beam_chance() - 10, GF_COLD, dir,
				  damroll(5+((plev-5)/4),8));
	}
	else if (die < 66)
	{
		fire_bolt_or_beam(beam_chance(), GF_ACID, dir,
				  damroll(6+((plev-5)/4),8));
	}
	else if (die < 71)
	{
		fire_bolt_or_beam(beam_chance(), GF_FIRE, dir,
				  damroll(8+((plev-5)/4),8));
	}
	else if (die < 76)
	{
		drain_life(dir, 75);
	}
	else if (die < 81)
	{
		fire_ball(GF_ELEC, dir, 30 + plev / 2, 2);
	}
	else if (die < 86)
	{
		fire_ball(GF_ACID, dir, 40 + plev, 2);
	}
	else if (die < 91)
	{
		fire_ball(GF_ICE, dir, 70 + plev, 3);
	}
	else if (die < 96)
	{
		fire_ball(GF_FIRE, dir, 80 + plev, 3);
	}
	else if (die < 101)
	{
		drain_life(dir, 100 + plev);
	}
	else if (die < 104)
	{
		earthquake(py, px, 12);
	}
	else if (die < 106)
	{
		(void)destroy_area(py, px, 13 + randint0(5), 2 * p_ptr->lev);
	}
	else if (die < 108)
	{
		symbol_genocide(plev+50, TRUE);
	}
	else if (die < 110)
	{
		dispel_monsters(120);
	}
	else
	{ /* RARE */
		dispel_monsters(150);
		slow_monsters(p_ptr->lev);
		sleep_monsters(p_ptr->lev);
		hp_player(300);
	}

	if (die < 31)
	{
#ifdef JP
		msg_print("陰欝な声がクスクス笑う。「もうすぐおまえは我々の仲間になるだろう。弱き者よ。」");
#else
		msg_print("Sepulchral voices chuckle. 'Soon you will join us, mortal.'");
#endif
	}
}


static void wild_magic(int spell)
{
	int counter = 0;
	int type = SUMMON_BIZARRE1 + randint0(6);

	if (type < SUMMON_BIZARRE1) type = SUMMON_BIZARRE1;
	else if (type > SUMMON_BIZARRE6) type = SUMMON_BIZARRE6;

	switch (randint1(spell) + randint1(8) + 1)
	{
	case 1:
	case 2:
	case 3:
		teleport_player(10, TELEPORT_PASSIVE);
		break;
	case 4:
	case 5:
	case 6:
		teleport_player(100, TELEPORT_PASSIVE);
		break;
	case 7:
	case 8:
		teleport_player(200, TELEPORT_PASSIVE);
		break;
	case 9:
	case 10:
	case 11:
		unlite_area(10, 3);
		break;
	case 12:
	case 13:
	case 14:
		lite_area(damroll(2, 3), 2);
		break;
	case 15:
		destroy_doors_touch();
		break;
	case 16: case 17:
		wall_breaker();
	case 18:
		sleep_monsters_touch();
		break;
	case 19:
	case 20:
		trap_creation(py, px);
		break;
	case 21:
	case 22:
		door_creation();
		break;
	case 23:
	case 24:
	case 25:
		aggravate_monsters(0);
		break;
	case 26:
		earthquake(py, px, 5);
		break;
	case 27:
	case 28:
		mut_gain_random(NULL);
		break;
	case 29:
	case 30:
		apply_disenchant(1);
		break;
	case 31:
		lose_all_info();
		break;
	case 32:
		fire_ball(GF_CHAOS, 0, spell + 5, 1 + (spell / 10));
		break;
	case 33:
		wall_stone();
		break;
	case 34:
	case 35:
		while (counter++ < 8)
		{
			(void)summon_specific(0, py, px, (dun_level * 3) / 2, type, (PM_ALLOW_GROUP | PM_NO_PET));
		}
		break;
	case 36:
	case 37:
		activate_hi_summon(py, px, FALSE);
		break;
	case 38:
		(void)summon_cyber(-1, py, px);
		break;
	default:
		{
			int count = 0;
			(void)activate_ty_curse(FALSE, &count);
			break;
		}
	}
}


static void cast_shuffle(void)
{
	int plev = p_ptr->lev;
	int dir;
	int die;
	int vir = virtue_number(V_CHANCE);
	int i;

	/* Card sharks and high mages get a level bonus */
	if ((p_ptr->pclass == CLASS_ROGUE) ||
	    (p_ptr->pclass == CLASS_HIGH_MAGE) ||
	    (p_ptr->pclass == CLASS_SORCERER))
		die = (randint1(110)) + plev / 5;
	else
		die = randint1(120);


	if (vir)
	{
		if (p_ptr->virtues[vir - 1] > 0)
		{
			while (randint1(400) < p_ptr->virtues[vir - 1]) die++;
		}
		else
		{
			while (randint1(400) < (0-p_ptr->virtues[vir - 1])) die--;
		}
	}

#ifdef JP
	msg_print("あなたはカードを切って一枚引いた...");
#else
	msg_print("You shuffle the deck and draw a card...");
#endif

	if (die < 30)
		chg_virtue(V_CHANCE, 1);

	if (die < 7)
	{
#ifdef JP
		msg_print("なんてこった！《死》だ！");
#else
		msg_print("Oh no! It's Death!");
#endif

		for (i = 0; i < randint1(3); i++)
			activate_hi_summon(py, px, FALSE);
	}
	else if (die < 14)
	{
#ifdef JP
		msg_print("なんてこった！《悪魔》だ！");
#else
		msg_print("Oh no! It's the Devil!");
#endif

		summon_specific(0, py, px, dun_level, SUMMON_DEMON, (PM_ALLOW_GROUP | PM_ALLOW_UNIQUE | PM_NO_PET));
	}
	else if (die < 18)
	{
		int count = 0;
#ifdef JP
		msg_print("なんてこった！《吊られた男》だ！");
#else
		msg_print("Oh no! It's the Hanged Man.");
#endif

		activate_ty_curse(FALSE, &count);
	}
	else if (die < 22)
	{
#ifdef JP
		msg_print("《不調和の剣》だ。");
#else
		msg_print("It's the swords of discord.");
#endif

		aggravate_monsters(0);
	}
	else if (die < 26)
	{
#ifdef JP
		msg_print("《愚者》だ。");
#else
		msg_print("It's the Fool.");
#endif

		do_dec_stat(A_INT);
		do_dec_stat(A_WIS);
	}
	else if (die < 30)
	{
#ifdef JP
		msg_print("奇妙なモンスターの絵だ。");
#else
		msg_print("It's the picture of a strange monster.");
#endif

		trump_summoning(1, FALSE, py, px, (dun_level * 3 / 2), (32 + randint1(6)), PM_ALLOW_GROUP | PM_ALLOW_UNIQUE);
	}
	else if (die < 33)
	{
#ifdef JP
		msg_print("《月》だ。");
#else
		msg_print("It's the Moon.");
#endif

		unlite_area(10, 3);
	}
	else if (die < 38)
	{
#ifdef JP
		msg_print("《運命の輪》だ。");
#else
		msg_print("It's the Wheel of Fortune.");
#endif

		wild_magic(randint0(32));
	}
	else if (die < 40)
	{
#ifdef JP
		msg_print("テレポート・カードだ。");
#else
		msg_print("It's a teleport trump card.");
#endif

		teleport_player(10, TELEPORT_PASSIVE);
	}
	else if (die < 42)
	{
#ifdef JP
		msg_print("《正義》だ。");
#else
		msg_print("It's Justice.");
#endif

		set_blessed(p_ptr->lev, FALSE);
	}
	else if (die < 47)
	{
#ifdef JP
		msg_print("テレポート・カードだ。");
#else
		msg_print("It's a teleport trump card.");
#endif

		teleport_player(100, TELEPORT_PASSIVE);
	}
	else if (die < 52)
	{
#ifdef JP
		msg_print("テレポート・カードだ。");
#else
		msg_print("It's a teleport trump card.");
#endif

		teleport_player(200, TELEPORT_PASSIVE);
	}
	else if (die < 60)
	{
#ifdef JP
		msg_print("《塔》だ。");
#else
		msg_print("It's the Tower.");
#endif

		wall_breaker();
	}
	else if (die < 72)
	{
#ifdef JP
		msg_print("《節制》だ。");
#else
		msg_print("It's Temperance.");
#endif

		sleep_monsters_touch();
	}
	else if (die < 80)
	{
#ifdef JP
		msg_print("《塔》だ。");
#else
		msg_print("It's the Tower.");
#endif

		earthquake(py, px, 5);
	}
	else if (die < 82)
	{
#ifdef JP
		msg_print("友好的なモンスターの絵だ。");
#else
		msg_print("It's the picture of a friendly monster.");
#endif

		trump_summoning(1, TRUE, py, px, (dun_level * 3 / 2), SUMMON_BIZARRE1, 0L);
	}
	else if (die < 84)
	{
#ifdef JP
		msg_print("友好的なモンスターの絵だ。");
#else
		msg_print("It's the picture of a friendly monster.");
#endif

		trump_summoning(1, TRUE, py, px, (dun_level * 3 / 2), SUMMON_BIZARRE2, 0L);
	}
	else if (die < 86)
	{
#ifdef JP
		msg_print("友好的なモンスターの絵だ。");
#else
		msg_print("It's the picture of a friendly monster.");
#endif

		trump_summoning(1, TRUE, py, px, (dun_level * 3 / 2), SUMMON_BIZARRE4, 0L);
	}
	else if (die < 88)
	{
#ifdef JP
		msg_print("友好的なモンスターの絵だ。");
#else
		msg_print("It's the picture of a friendly monster.");
#endif

		trump_summoning(1, TRUE, py, px, (dun_level * 3 / 2), SUMMON_BIZARRE5, 0L);
	}
	else if (die < 96)
	{
#ifdef JP
		msg_print("《恋人》だ。");
#else
		msg_print("It's the Lovers.");
#endif

		if (get_aim_dir(&dir))
			charm_monster(dir, MIN(p_ptr->lev, 20));
	}
	else if (die < 101)
	{
#ifdef JP
		msg_print("《隠者》だ。");
#else
		msg_print("It's the Hermit.");
#endif

		wall_stone();
	}
	else if (die < 111)
	{
#ifdef JP
		msg_print("《審判》だ。");
#else
		msg_print("It's the Judgement.");
#endif

		do_cmd_rerate(FALSE);
		mut_lose_all();

		{
			msg_print("Press Space to continue.");
			flush();
			for (;;)
			{
				char ch = inkey();
				if (ch == ' ') break;
			}
			prt("", 0, 0);
			msg_flag = FALSE;
		}
	}
	else if (die < 120)
	{
#ifdef JP
		msg_print("《太陽》だ。");
#else
		msg_print("It's the Sun.");
#endif

		chg_virtue(V_KNOWLEDGE, 1);
		chg_virtue(V_ENLIGHTEN, 1);
		wiz_lite(p_ptr->tim_superstealth > 0);
	}
	else
	{
#ifdef JP
		msg_print("《世界》だ。");
#else
		msg_print("It's the World.");
#endif

		if (p_ptr->exp < PY_MAX_EXP)
		{
			s32b ee = (p_ptr->exp / 25) + 1;
			if (ee > 5000) ee = 5000;
#ifdef JP
			msg_print("更に経験を積んだような気がする。");
#else
			msg_print("You feel more experienced.");
#endif

			gain_exp(ee);
		}
	}
}


/*
 * Drop 10+1d10 meteor ball at random places near the player
 */
static void cast_meteor(int dam, int rad)
{
	int i;
	int b = 10 + randint1(10);

	for (i = 0; i < b; i++)
	{
		int y, x;
		int count;

		for (count = 0; count <= 20; count++)
		{
			int dy, dx, d;

			x = px - 8 + randint0(17);
			y = py - 8 + randint0(17);

			dx = (px > x) ? (px - x) : (x - px);
			dy = (py > y) ? (py - y) : (y - py);

			/* Approximate distance */
			d = (dy > dx) ? (dy + (dx >> 1)) : (dx + (dy >> 1));

			if (d >= 9) continue;

			if (!in_bounds(y, x) || !projectable(py, px, y, x)
			    || !cave_have_flag_bold(y, x, FF_PROJECT)) continue;

			/* Valid position */
			break;
		}

		if (count > 20) continue;

		project(0, rad, y, x, dam, GF_METEOR, PROJECT_KILL | PROJECT_JUMP | PROJECT_ITEM, -1);
	}
}


/*
 * Drop 10+1d10 disintegration ball at random places near the target
 */
static bool cast_wrath_of_the_god(int dam, int rad)
{
	int x, y, tx, ty;
	int nx, ny;
	int dir, i;
	int b = 10 + randint1(10);

	if (!get_aim_dir(&dir)) return FALSE;

	/* Use the given direction */
	tx = px + 99 * ddx[dir];
	ty = py + 99 * ddy[dir];

	/* Hack -- Use an actual "target" */
	if ((dir == 5) && target_okay())
	{
		tx = target_col;
		ty = target_row;
	}

	x = px;
	y = py;

	while (1)
	{
		/* Hack -- Stop at the target */
		if ((y == ty) && (x == tx)) break;

		ny = y;
		nx = x;
		mmove2(&ny, &nx, py, px, ty, tx);

		/* Stop at maximum range */
		if (MAX_RANGE <= distance(py, px, ny, nx)) break;

		/* Stopped by walls/doors */
		if (!cave_have_flag_bold(ny, nx, FF_PROJECT)) break;

		/* Stopped by monsters */
		if ((dir != 5) && cave[ny][nx].m_idx != 0) break;

		/* Save the new location */
		x = nx;
		y = ny;
	}
	tx = x;
	ty = y;

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

		project(0, rad, y, x, dam, GF_DISINTEGRATE, PROJECT_JUMP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL, -1);
	}

	return TRUE;
}


/*
 * An "item_tester_hook" for offer
 */
static bool item_tester_offer(object_type *o_ptr)
{
	/* Flasks of oil are okay */
	if (o_ptr->tval != TV_CORPSE) return (FALSE);

	if (o_ptr->sval != SV_CORPSE) return (FALSE);

	if (my_strchr("pht", r_info[o_ptr->pval].d_char)) return (TRUE);

	/* Assume not okay */
	return (FALSE);
}


/*
 * Daemon spell Summon Greater Demon
 */
bool cast_summon_greater_demon(void)
{
	int plev = p_ptr->lev;
	int item;
	cptr q, s;
	int summon_lev;
	object_type *o_ptr;

	item_tester_hook = item_tester_offer;
#ifdef JP
	q = "どの死体を捧げますか? ";
	s = "捧げられる死体を持っていない。";
#else
	q = "Sacrifice which corpse? ";
	s = "You have nothing to sacrifice.";
#endif
	if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return FALSE;

	/* Get the item (in the pack) */
	if (item >= 0)
	{
		o_ptr = &inventory[item];
	}

	/* Get the item (on the floor) */
	else
	{
		o_ptr = &o_list[0 - item];
	}

	summon_lev = plev * 2 / 3 + r_info[o_ptr->pval].level;

	if (summon_specific(-1, py, px, summon_lev, SUMMON_HI_DEMON, (PM_ALLOW_GROUP | PM_FORCE_PET)))
	{
#ifdef JP
		msg_print("硫黄の悪臭が充満した。");
#else
		msg_print("The area fills with a stench of sulphur and brimstone.");
#endif


#ifdef JP
		msg_print("「ご用でございますか、ご主人様」");
#else
		msg_print("'What is thy bidding... Master?'");
#endif

		/* Decrease the item (from the pack) */
		if (item >= 0)
		{
			inven_item_increase(item, -1);
			inven_item_describe(item);
			inven_item_optimize(item);
		}

		/* Decrease the item (from the floor) */
		else
		{
			floor_item_increase(0 - item, -1);
			floor_item_describe(0 - item);
			floor_item_optimize(0 - item);
		}
	}
	else
	{
#ifdef JP
		msg_print("悪魔は現れなかった。");
#else
		msg_print("No Greater Demon arrive.");
#endif
	}

	return TRUE;
}



static cptr do_life_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
	bool spoil = (mode == SPELL_SPOIL_DESC) ? TRUE : FALSE;

	int plev = p_ptr->lev;

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "軽傷の治癒";
		if (desc) return "怪我と体力を少し回復させる。";
#else
		if (name) return "Cure Light Wounds";
		if (desc) return "Heals cut and HP a little.";
		if (spoil) return "Reduces player cut status by 10 and heals player 2d10 hp.";
#endif
    
		{
			int dice = 2;
			int sides = 10;

			if (info) return info_heal(dice, sides, 0);

			if (cast)
			{
				hp_player(spell_power(damroll(dice, sides)));
				set_cut(p_ptr->cut - 10, TRUE);
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
		if (spoil) return "Grants +5 AC and +10 To Hit for 12 rounds.";
#endif
    
		{
			int base = spell_power(12);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_blessed(randint1(base) + base, FALSE);
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "回復力強化";
		if (desc) return "一定時間、回復力が増強される。";
#else
		if (name) return "Regeneration";
		if (desc) return "Gives regeneration ability for a while.";
		if (spoil) return "Player regenerates hp and sp twice as fast as normal for 80+d80 rounds.";
#endif
    
		{
			int base = spell_power(80);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_tim_regen(base + randint1(base), FALSE);
			}
		}
		break;

	case 3:
#ifdef JP
		if (name) return "光の召喚";
		if (desc) return "光源が照らしている範囲か部屋全体を永久に明るくする。";
#else
		if (name) return "Call Light";
		if (desc) return "Lights up nearby area and the inside of a room permanently.";
#endif
    
		{
			int dice = 2;
			int sides = plev / 2;
			int rad = spell_power(plev / 10 + 1);

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				lite_area(spell_power(damroll(dice, sides)), rad);
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
		if (name) return "重傷の治癒";
		if (desc) return "怪我と体力を中程度回復させる。";
#else
		if (name) return "Cure Medium Wounds";
		if (desc) return "Heals cut and HP more.";
		if (spoil) return "Reduces player cut status to cut/2 - 20 and heals player 4d10 hp.";
#endif
    
		{
			int dice = 4;
			int sides = 10;

			if (info) return info_heal(dice, sides, 0);

			if (cast)
			{
				hp_player(spell_power(damroll(dice, sides)));
				set_cut((p_ptr->cut / 2) - 20, TRUE);
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "解毒";
		if (desc) return "体内の毒を取り除く。";
#else
		if (name) return "Cure Poison";
		if (desc) return "Cure poison status.";
#endif
    
		{
			if (cast)
			{
				set_poisoned(0, TRUE);
			}
		}
		break;

	case 7:
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
				if (p_ptr->fasting)
				{
					msg_print("You break your fast.");
					p_ptr->redraw |= PR_STATUS;
					p_ptr->fasting = FALSE;
				}
			}
		}
		break;

	case 8:
#ifdef JP
		if (name) return "解呪";
		if (desc) return "アイテムにかかった弱い呪いを解除する。";
#else
		if (name) return "Remove Curse";
		if (desc) return "Removes normal curses from equipped items.";
#endif

		{
			if (cast)
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
		}
		break;

	case 9:
		if (name) return "Fasting";
		if (desc) return "Begin a religious fast. In time, your god may restore you!";
		if (spoil) return "Player begins a fast. Once hungry there is a small chance that the player will have a random stat restored, or will have their life restored.";
    
		if (cast)
		{
			if (p_ptr->fasting)
			{
				msg_print("You are already fasting. Perhaps you should pray as well?");
				return NULL;
			}
			msg_print("You begin to fast.");
			set_food(p_ptr->food/2);
			p_ptr->redraw |= PR_STATUS;
			p_ptr->fasting = TRUE;
		}
		break;

	case 10:
#ifdef JP
		if (name) return "致命傷の治癒";
		if (desc) return "体力を大幅に回復させ、負傷と朦朧状態も全快する。";
#else
		if (name) return "Cure Critical Wounds";
		if (desc) return "Heals cut, stun and HP greatly.";
		if (spoil) return "Removes cuts and stuns and heals player 8d10 hp.";
#endif
    
		{
			int dice = 8;
			int sides = 10;

			if (info) return info_heal(dice, sides, 0);

			if (cast)
			{
				hp_player(spell_power(damroll(dice, sides)));
				set_stun(0, TRUE);
				set_cut(0, TRUE);
			}
		}
		break;

	case 11:
#ifdef JP
		if (name) return "耐熱耐寒";
		if (desc) return "一定時間、火炎と冷気に対する耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Resist Heat and Cold";
		if (desc) return "Gives resistance to fire and cold. These resistances can be added to which from equipment for more powerful resistances.";
#endif
    
		{
			int base = spell_power(20);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_oppose_cold(randint1(base) + base, FALSE);
				set_oppose_fire(randint1(base) + base, FALSE);
			}
		}
		break;

	case 12:
#ifdef JP
		if (name) return "周辺感知";
		if (desc) return "周辺の地形を感知する。";
#else
		if (name) return "Sense Surroundings";
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

	case 13:
#ifdef JP
		if (name) return "パニック・アンデッド";
		if (desc) return "視界内のアンデッドを恐怖させる。抵抗されると無効。";
#else
		if (name) return "Turn Undead";
		if (desc) return "Attempts to scare undead monsters in sight.";
#endif
    
		{
			if (cast)
			{
				turn_undead();
			}
		}
		break;

	case 14:
#ifdef JP
		if (name) return "体力回復";
		if (desc) return "極めて強力な回復呪文で、負傷と朦朧状態も全快する。";
#else
		if (name) return "Healing";
		if (desc) return "Much powerful healing magic, and heals cut and stun completely.";
		if (spoil) return "Removes cuts and stuns and heals player 300 hp.";
#endif
    
		{
			int heal = spell_power(300);

			if (info) return info_heal(0, 0, heal);

			if (cast)
			{
				hp_player(heal);
				set_stun(0, TRUE);
				set_cut(0, TRUE);
			}
		}
		break;

	case 15:
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

	case 16:
#ifdef JP
		if (name) return "*解呪*";
		if (desc) return "アイテムにかかった強力な呪いを解除する。";
#else
		if (name) return "Dispel Curse";
		if (desc) return "Removes normal and heavy curse from equipped items.";
#endif
    
		{
			if (cast)
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
		break;

	case 17:
#ifdef JP
		if (name) return "鑑識";
		if (desc) return "アイテムを識別する。";
#else
		if (name) return "Perception";
		if (desc) return "Identifies an item.";
#endif
    
		{
			if (cast)
			{
				if (!ident_spell(NULL)) return NULL;
			}
		}
		break;

	case 18:
#ifdef JP
		if (name) return "アンデッド退散";
		if (desc) return "視界内の全てのアンデッドにダメージを与える。";
#else
		if (name) return "Dispel Undead";
		if (desc) return "Damages all undead monsters in sight.";
		if (spoil) return "All undead monsters in the player's line of sight take 1d(L*5) damage.";
#endif
    
		{
			int dice = 1;
			int sides = spell_power(plev * 5);

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				dispel_undead(damroll(dice, sides));
			}
		}
		break;

	case 19:
		if (name) return "Sustaining";
		if (desc) return "Grants temporary stat sustains, depending on your level.";
		if (spoil) return "Player gains up to L/7 stat sustains for L turns.";
    
		{
			int dur = spell_power(p_ptr->lev);

			if (info) return info_duration(dur, 0);

			if (cast)
			{
				int num = p_ptr->lev / 7;

				if (randint0(7) < num)
				{
					set_tim_hold_life(dur, FALSE);
					num--;
				}
				if (randint0(6) < num)
				{
					set_tim_sustain_con(dur, FALSE);
					num--;
				}
				if (randint0(5) < num)
				{
					set_tim_sustain_str(dur, FALSE);
					num--;
				}
				if (randint0(4) < num)
				{
					set_tim_sustain_int(dur, FALSE);
					num--;
				}
				if (randint0(3) < num)
				{
					set_tim_sustain_dex(dur, FALSE);
					num--;
				}
				if (randint0(2) < num)
				{
					set_tim_sustain_wis(dur, FALSE);
					num--;
				}
				if (num)
				{
					set_tim_sustain_chr(dur, FALSE);
					num--;
				}

			}
		}
		break;

	case 20:
		if (name) return "Cure Mutation";
		if (desc) return "Remove a random mutation.";
		if (spoil) return "Remove a random mutation. There is a 1 in 100/L chance of removing a bad mutation only.";
    
		if (cast)
		{
			if (one_in_(100/p_ptr->lev))
				mut_lose_random(mut_bad_pred);
			else
				mut_lose_random(NULL);
		}
		break;

	case 21:
#ifdef JP
		if (name) return "帰還の詔";
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

	case 22:
		if (name) return "Transcendence";
		if (desc) return "For a short while, any damage you receive will be absorbed by your spell points.";
    
		{
			int dur = spell_power(p_ptr->lev/10);

			if (info) return format("dur %d", dur);

			if (cast)
				set_tim_transcendence(dur, FALSE);
		}
		break;

	case 23:
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

	case 24:
#ifdef JP
		if (name) return "不毛化";
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

	case 25:
#ifdef JP
		if (name) return "全感知";
		if (desc) return "近くの全てのモンスター、罠、扉、階段、財宝、そしてアイテムを感知する。";
#else
		if (name) return "Detection";
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

	case 26:
#ifdef JP
		if (name) return "アンデッド消滅";
		if (desc) return "自分の周囲にいるアンデッドを現在の階から消し去る。抵抗されると無効。";
#else
		if (name) return "Annihilate Undead";
		if (desc) return "Eliminates all nearby undead monsters, exhausting you.  Powerful or unique monsters may be able to resist.";
#endif
    
		{
			int power = spell_power(plev + 50);

			if (info) return info_power(power);

			if (cast)
			{
				mass_genocide_undead(power, TRUE);
			}
		}
		break;

	case 27:
#ifdef JP
		if (name) return "千里眼";
		if (desc) return "その階全体を永久に照らし、ダンジョン内すべてのアイテムを感知する。";
#else
		if (name) return "Clairvoyance";
		if (desc) return "Maps and lights whole dungeon level. Knows all objects location. And gives telepathy for a while.";
#endif
    
		{
			if (cast)
			{
				wiz_lite(p_ptr->tim_superstealth > 0);
			}
		}
		break;

	case 28:
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

	case 29:
#ifdef JP
		if (name) return "*体力回復*";
		if (desc) return "最強の治癒の魔法で、負傷と朦朧状態も全快する。";
#else
		if (name) return "Healing True";
		if (desc) return "The greatest healing magic. Heals all HP, cut and stun.";
		if (spoil) return "Removes cuts and stuns, and heals the player 2000hp.";
#endif
    
		{
			int heal = spell_power(2000);

			if (info) return info_heal(0, 0, heal);

			if (cast)
			{
				hp_player(heal);
				set_stun(0, TRUE);
				set_cut(0, TRUE);
			}
		}
		break;

	case 30:
#ifdef JP
		if (name) return "聖なるビジョン";
		if (desc) return "アイテムの持つ能力を完全に知る。";
#else
		if (name) return "Holy Vision";
		if (desc) return "Fully identifies an item.";
#endif
    
		{
			if (cast)
			{
				if (!identify_fully(NULL)) return NULL;
			}
		}
		break;

	case 31:
#ifdef JP
		if (name) return "究極の耐性";
		if (desc) return "一定時間、あらゆる耐性を付け、ACと魔法防御能力を上昇させる。";
#else
		if (name) return "Ultimate Resistance";
		if (desc) return "Gives ultimate resistance, bonus to AC and speed.";
		if (spoil) return "Player gains all resistances, auras, sustains, FA, SI, slow digestion, regeneration, levitation and reflection as well as double base resistance, haste, and +100AC for X+dX rounds where X=L/2.";
#endif
    
		{
			int base = spell_power(plev / 2);

			if (info) return info_duration(base, base);

			if (cast)
			{
				int v = randint1(base) + base;
				set_fast(v, FALSE);
				set_oppose_acid(v, FALSE);
				set_oppose_elec(v, FALSE);
				set_oppose_fire(v, FALSE);
				set_oppose_cold(v, FALSE);
				set_oppose_pois(v, FALSE);
				set_ultimate_res(v, FALSE);
			}
		}
		break;
	}

	return "";
}


static cptr do_sorcery_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
	bool spoil = (mode == SPELL_SPOIL_DESC) ? TRUE : FALSE;

	int dir;
	int plev = p_ptr->lev;

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
				if (mut_present(MUT_ASTRAL_GUIDE))
					energy_use = 30;
				teleport_player(range, 0L);
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "罠と扉感知";
		if (desc) return "近くの全ての扉と罠を感知する。";
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

	case 3:
#ifdef JP
		if (name) return "ライト・エリア";
		if (desc) return "光源が照らしている範囲か部屋全体を永久に明るくする。";
#else
		if (name) return "Light Area";
		if (desc) return "Lights up nearby area and the inside of a room permanently.";
#endif
    
		{
			int dice = 2;
			int sides = plev / 2;
			int rad = plev / 10 + 1;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				lite_area(spell_power(damroll(dice, sides)), rad);
			}
		}
		break;

	case 4:
#ifdef JP
		if (name) return "パニック・モンスター";
		if (desc) return "モンスター1体を混乱させる。抵抗されると無効。";
#else
		if (name) return "Confuse Monster";
		if (desc) return "Attempts to confuse a monster.";
#endif
    
		{
			int power = spell_power((plev * 3) / 2);

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				confuse_monster(dir, power);
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "テレポート";
		if (desc) return "遠距離のテレポートをする。";
#else
		if (name) return "Teleport";
		if (desc) return "Teleport long distance.";
#endif
    
		{
			int range = plev * 5;

			if (info) return info_range(range);

			if (cast)
			{
				if (mut_present(MUT_ASTRAL_GUIDE))
					energy_use = 30;
				teleport_player(range, 0L);
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "スリープ・モンスター";
		if (desc) return "モンスター1体を眠らせる。抵抗されると無効。";
#else
		if (name) return "Sleep Monster";
		if (desc) return "Attempts to sleep a monster.";
#endif
    
		{
			int power = spell_power(plev * 3 /2);

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				sleep_monster(dir);
			}
		}
		break;

	case 7:
#ifdef JP
		if (name) return "魔力充填";
		if (desc) return "杖/魔法棒の充填回数を増やすか、充填中のロッドの充填時間を減らす。";
#else
		if (name) return "Recharging";
		if (desc) return "Recharges staffs, wands or rods.";
#endif
    
		{
			int power = spell_power(plev * 4);

			if (info) return info_power(power);

			if (cast)
			{
				if (!recharge(power)) return NULL;
			}
		}
		break;

	case 8:
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

	case 9:
#ifdef JP
		if (name) return "鑑定";
		if (desc) return "アイテムを識別する。";
#else
		if (name) return "Identify";
		if (desc) return "Identifies an item.";
#endif
    
		{
			if (cast)
			{
				if (!ident_spell(NULL)) return NULL;
			}
		}
		break;

	case 10:
#ifdef JP
		if (name) return "スロウ・モンスター";
		if (desc) return "モンスター1体を減速さる。抵抗されると無効。";
#else
		if (name) return "Slow Monster";
		if (desc) return "Attempts to slow a monster.";
#endif
    
		{
			int power = spell_power(plev * 2);

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				slow_monster(dir);
			}
		}
		break;

	case 11:
		if (plev < 35)
		{
			if (name) return "Mass Sleep";
			if (desc) return "Attempts to sleep all monsters in sight.";
		}
		else
		{
			if (name) return "Mass Stasis";
			if (desc) return "Attempts to suspend all monsters in sight.";
		}
    
		{
			int power = spell_power(plev * 4);

			if (info) return info_power(power);

			if (cast)
			{
				if (plev < 35)
					sleep_monsters(power);
				else
					stasis_monsters(power);
			}
		}
		break;

	case 12:
#ifdef JP
		if (name) return "テレポート・モンスター";
		if (desc) return "モンスターをテレポートさせるビームを放つ。抵抗されると無効。";
#else
		if (name) return "Teleport Away";
		if (desc) return "Teleports all monsters on the line away unless resisted.";
#endif
    
		{
			int power = spell_power(plev * 2);

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_beam(GF_AWAY_ALL, dir, power);
			}
		}
		break;

	case 13:
#ifdef JP
		if (name) return "スピード";
		if (desc) return "一定時間、加速する。";
#else
		if (name) return "Haste Self";
		if (desc) return "Hastes you for a while.";
#endif
    
		{
			int base = spell_power(plev);
			int sides = spell_power(20 + plev);

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_fast(randint1(sides) + base, FALSE);
			}
		}
		break;

	case 14:
#ifdef JP
		if (name) return "真・感知";
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

	case 15:
#ifdef JP
		if (name) return "真・鑑定";
		if (desc) return "アイテムの持つ能力を完全に知る。";
#else
		if (name) return "Identify True";
		if (desc) return "*Identifies* an item.";
#endif
    
		{
			if (cast)
			{
				if (!identify_fully(NULL)) return NULL;
			}
		}
		break;

	case 16:
		if (name) return "Inventory Protection";
		if (desc) return "For a short while, items in your pack have a chance to resist destruction.";
    
		{
			int base = spell_power(30);

			if (info) return info_duration(30, base);

			if (cast)
				set_tim_inven_prot(base + randint1(base), FALSE);
		}
		break;

	case 17:
#ifdef JP
		if (name) return "階段生成";
		if (desc) return "自分のいる位置に階段を作る。";
#else
		if (name) return "Stair Creation";
		if (desc) return "Creates a stair which goes down or up.";
#endif
    
		if (cast)
			stair_creation(FALSE);
		break;

	case 18:
#ifdef JP
		if (name) return "精神感知";
		if (desc) return "一定時間、テレパシー能力を得る。";
#else
		if (name) return "Sense Minds";
		if (desc) return "Gives telepathy for a while.";
#endif
    
		{
			int base = 25;
			int sides = spell_power(30);

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_tim_esp(randint1(sides) + base, FALSE);
			}
		}
		break;

	case 19:
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
				if (!tele_town()) return NULL;
			}
		}
		break;

	case 20:
#ifdef JP
		if (name) return "自己分析";
		if (desc) return "現在の自分の状態を完全に知る。";
#else
		if (name) return "Self Knowledge";
		if (desc) return "Gives you useful info regarding your current resistances, the powers of your weapon and maximum limits of your stats.";
#endif
    
		{
			if (cast)
			{
				self_knowledge();
			}
		}
		break;

	case 21:
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

	case 22:
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

	case 23:
#ifdef JP
		if (name) return "次元の扉";
		if (desc) return "短距離内の指定した場所にテレポートする。";
#else
		if (name) return "Dimension Door";
		if (desc) return "Teleport to given location.";
#endif
    
		{
			int range = spell_power(plev / 2 + 10);

			if (info) return info_range(range);

			if (cast)
			{
#ifdef JP
				msg_print("次元の扉が開いた。目的地を選んで下さい。");
#else
				msg_print("You open a dimensional gate. Choose a destination.");
#endif

				if (!dimension_door(range)) return NULL;
			}
		}
		break;

	case 24:
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

	case 25:
		if (name) return "Door Creation";
		if (desc) return "Creates doors on all surrounding squares.";
    
		if (cast)
		{
			project(0, 1, py, px, 0, GF_MAKE_DOOR, PROJECT_GRID | PROJECT_ITEM | PROJECT_HIDE, -1);
			p_ptr->update |= (PU_FLOW);
			p_ptr->redraw |= (PR_MAP);
		}
		break;

	case 26:
#ifdef JP
		if (name) return "念動力";
		if (desc) return "アイテムを自分の足元へ移動させる。";
#else
		if (name) return "Telekinesis";
		if (desc) return "Pulls a distant item close to you.";
#endif
    
		{
			int weight = spell_power(plev * 15);

			if (info) return info_weight(weight);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fetch(dir, weight, FALSE);
			}
		}
		break;

	case 27:
#ifdef JP
		if (name) return "千里眼";
		if (desc) return "その階全体を永久に照らし、ダンジョン内すべてのアイテムを感知する。さらに、一定時間テレパシー能力を得る。";
#else
		if (name) return "Clairvoyance";
		if (desc) return "Maps and lights whole dungeon level. Knows all objects location. And gives telepathy for a while.";
#endif
    
		{
			int base = 25;
			int sides = spell_power(30);

			if (info) return info_duration(base, sides);

			if (cast)
			{
				chg_virtue(V_KNOWLEDGE, 1);
				chg_virtue(V_ENLIGHTEN, 1);

				wiz_lite(p_ptr->tim_superstealth > 0);

				if (!p_ptr->telepathy)
				{
					set_tim_esp(randint1(sides) + base, FALSE);
				}
			}
		}
		break;

	case 28:
		if (name) return "Device Mastery";
		if (desc) return "For a very short time, your magical devices are more powerful.";

		{
			int base = spell_power(p_ptr->lev/10);

			if (info) return info_duration(base, base);

			if (cast)
				set_tim_device_power(base + randint1(base), FALSE);
		}
		break;

	case 29:
#ifdef JP
		if (name) return "錬金術";
		if (desc) return "アイテム1つをお金に変える。";
#else
		if (name) return "Alchemy";
		if (desc) return "Turns an item into 1/3 of its value in gold.";
#endif
    
		{
			if (cast)
			{
				if (!alchemy()) return NULL;
			}
		}
		break;

	case 30:
#ifdef JP
		if (name) return "怪物追放";
		if (desc) return "視界内の全てのモンスターをテレポートさせる。抵抗されると無効。";
#else
		if (name) return "Banishment";
		if (desc) return "Teleports all monsters in sight away unless resisted.";
#endif
    
		{
			int power = spell_power(plev * 4);

			if (info) return info_power(power);

			if (cast)
			{
				banish_monsters(power);
			}
		}
		break;

	case 31:
#ifdef JP
		if (name) return "無傷の球";
		if (desc) return "一定時間、ダメージを受けなくなるバリアを張る。切れた瞬間に少しターンを消費するので注意。";
#else
		if (name) return "Globe of Invulnerability";
		if (desc) return "Generates barrier which completely protect you from almost all damages. Takes a few your turns when the barrier breaks or duration time is exceeded.";
#endif
    
		{
			int base = 4;

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_invuln(spell_power(randint1(base) + base), FALSE);
			}
		}
		break;
	}

	return "";
}


static cptr do_nature_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
	bool spoil = (mode == SPELL_SPOIL_DESC) ? TRUE : FALSE;

#ifdef JP
	static const char s_dam[] = "損傷:";
	static const char s_rng[] = "射程";
#else
	static const char s_dam[] = "dam ";
	static const char s_rng[] = "rng ";
#endif

	int dir;
	int plev = p_ptr->lev;

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "モンスター感知";
		if (desc) return "近くの全ての見えるモンスターを感知する。";
#else
		if (name) return "Detect Creatures";
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
		if (name) return "稲妻";
		if (desc) return "電撃の短いビームを放つ。";
#else
		if (name) return "Lightning";
		if (desc) return "Fires a short beam of lightning.";
#endif
    
		{
			int dice = 3 + (plev - 1) / 5;
			int sides = 4;
			int range = spell_power(plev / 6 + 2);

			if (info) return format("%s%dd%d %s%d", s_dam, spell_power(dice), sides, s_rng, range);

			if (cast)
			{
				project_length = range;

				if (!get_aim_dir(&dir)) return NULL;

				fire_beam(GF_ELEC, dir, spell_power(damroll(dice, sides)));
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "罠と扉感知";
		if (desc) return "近くの全ての罠と扉を感知する。";
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

	case 3:
#ifdef JP
		if (name) return "食糧生成";
		if (desc) return "食料を一つ作り出す。";
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

	case 4:
#ifdef JP
		if (name) return "日の光";
		if (desc) return "光源が照らしている範囲か部屋全体を永久に明るくする。";
#else
		if (name) return "Daylight";
		if (desc) return "Lights up nearby area and the inside of a room permanently.";
#endif
    
		{
			int dice = 2;
			int sides = spell_power(plev / 2);
			int rad = spell_power((plev / 10) + 1);

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				lite_area(damroll(dice, sides), rad);

				if ((prace_is_(RACE_VAMPIRE) || (p_ptr->mimic_form == MIMIC_VAMPIRE)) && !res_save_default(RES_LITE))
				{
					msg_print("The daylight scorches your flesh!");
					take_hit(DAMAGE_NOESCAPE, damroll(2, 2), "daylight", -1);
				}
			}
		}
		break;

	case 5:
		if (name) return "Wind Walker";
		if (desc) return "Grants temporary levitation.";
    
		{
			int dur = spell_power(30);

			if (info) return info_duration(dur, dur);

			if (cast)
				set_tim_levitation(randint1(dur) + dur, FALSE);
		}
		break;

	case 6:
#ifdef JP
		if (name) return "環境への耐性";
		if (desc) return "一定時間、冷気、炎、電撃に対する耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Resist Environment";
		if (desc) return "Gives resistance to fire, cold and electricity for a while. These resistances can be added to which from equipment for more powerful resistances.";
#endif
    
		{
			int base = spell_power(20);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_oppose_cold(randint1(base) + base, FALSE);
				set_oppose_fire(randint1(base) + base, FALSE);
				set_oppose_elec(randint1(base) + base, FALSE);
			}
		}
		break;

	case 7:
#ifdef JP
		if (name) return "傷と毒治療";
		if (desc) return "怪我を全快させ、毒を体から完全に取り除き、体力を少し回復させる。";
#else
		if (name) return "Cure Wounds & Poison";
		if (desc) return "Heals all cut and poison status. Heals HP a little.";
#endif
    
		{
			int dice = 2;
			int sides = spell_power(8);

			if (info) return info_heal(dice, sides, 0);

			if (cast)
			{
				hp_player(damroll(dice, sides));
				set_cut(0, TRUE);
				set_poisoned(0, TRUE);
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
			int dice = 1;
			int sides = 30;
			int base = 20;

			if (info) return info_damage(dice, sides, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				wall_to_mud(dir);
			}
		}
		break;

	case 9:
#ifdef JP
		if (name) return "アイス・ボルト";
		if (desc) return "冷気のボルトもしくはビームを放つ。";
#else
		if (name) return "Frost Bolt";
		if (desc) return "Fires a bolt or beam of cold.";
#endif
    
		{
			int dice = 3 + (plev - 5) / 4;
			int sides = 8;

			if (info) return info_damage(spell_power(dice), sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_bolt_or_beam(beam_chance() - 10, GF_COLD, dir, spell_power(damroll(dice, sides)));
			}
		}
		break;

	case 10:
#ifdef JP
		if (name) return "自然の覚醒";
		if (desc) return "周辺の地形を感知し、近くの罠、扉、階段、全てのモンスターを感知する。";
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
				detect_traps(rad2, TRUE);
				detect_doors(rad2);
				detect_stairs(rad2);
				detect_monsters_normal(rad2);
			}
		}
		break;

	case 11:
#ifdef JP
		if (name) return "ファイア・ボルト";
		if (desc) return "火炎のボルトもしくはビームを放つ。";
#else
		if (name) return "Fire Bolt";
		if (desc) return "Fires a bolt or beam of fire.";
#endif
    
		{
			int dice = 5 + (plev - 5) / 4;
			int sides = 8;

			if (info) return info_damage(spell_power(dice), sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_bolt_or_beam(beam_chance() - 10, GF_FIRE, dir, spell_power(damroll(dice, sides)));
			}
		}
		break;

	case 12:
#ifdef JP
		if (name) return "太陽光線";
		if (desc) return "光線を放つ。光りを嫌うモンスターに効果がある。";
#else
		if (name) return "Ray of Sunlight";
		if (desc) return "Fires a beam of light which damages to light-sensitive monsters.";
#endif
    
		{
			int dice = 6;
			int sides = 8;

			if (info) return info_damage(dice, spell_power(sides), 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
#ifdef JP
				msg_print("太陽光線が現れた。");
#else
				msg_print("A line of sunlight appears.");
#endif

				project_hook(GF_LITE_WEAK, dir, spell_power(damroll(6, 8)), PROJECT_BEAM | PROJECT_GRID | PROJECT_KILL);
			}
		}
		break;

	case 13:
#ifdef JP
		if (name) return "足かせ";
		if (desc) return "視界内の全てのモンスターを減速させる。抵抗されると無効。";
#else
		if (name) return "Entangle";
		if (desc) return "Attempts to slow all monsters in sight.";
#endif
    
		{
			int power = spell_power(plev * 2);

			if (info) return info_power(power);

			if (cast)
			{
				slow_monsters(power);
			}
		}
		break;

	case 14:
#ifdef JP
		if (name) return "動物召喚";
		if (desc) return "動物を1体召喚する。";
#else
		if (name) return "Nature's Gate";
		if (desc) return "Summons one or more animals. At higher levels, might summon hounds, reptiles or even an Ent!";
#endif
    
		if (cast)
		{
			bool success = FALSE;
			if (plev < 30)
				success = trump_summoning(1, TRUE, py, px, 0, SUMMON_ANIMAL_RANGER, PM_ALLOW_GROUP);
			else if (plev < 47)
			{
				switch (randint1(3))
				{
				case 1:
					success = trump_summoning(1, TRUE, py, px, 0, SUMMON_HOUND, PM_ALLOW_GROUP);
					break;
				case 2:
					success = trump_summoning(1, TRUE, py, px, 0, SUMMON_HYDRA, PM_ALLOW_GROUP);
					break;
				case 3:
					success = trump_summoning((1 + (plev - 15)/ 10), TRUE, py, px, 0, SUMMON_ANIMAL_RANGER, PM_ALLOW_GROUP);
					break;
				}
			}
			else
			{
				if (one_in_(5))
					success = trump_summoning(1, TRUE, py, px, 0, SUMMON_ENT, PM_ALLOW_GROUP);
			}
			if (!success)
				msg_print(T("No help arrives.", "動物は現れなかった。"));
		}
		break;

	case 15:
#ifdef JP
		if (name) return "薬草治療";
		if (desc) return "体力を大幅に回復させ、負傷、朦朧状態、毒から全快する。";
#else
		if (name) return "Herbal Healing";
		if (desc) return "Heals HP greatly. And heals cut, stun and poison completely.";
#endif
    
		{
			int heal = spell_power(500);

			if (info) return info_heal(0, 0, heal);

			if (cast)
			{
				hp_player(heal);
				set_stun(0, TRUE);
				set_cut(0, TRUE);
				set_poisoned(0, TRUE);
			}
		}
		break;

	case 16:
#ifdef JP
		if (name) return "階段生成";
		if (desc) return "自分のいる位置に階段を作る。";
#else
		if (name) return "Stair Building";
		if (desc) return "Creates a stair which goes down or up.";
#endif
    
		{
			if (cast)
			{
				stair_creation(FALSE);
			}
		}
		break;

	case 17:
#ifdef JP
		if (name) return "肌石化";
		if (desc) return "一定時間、ACを上昇させる。";
#else
		if (name) return "Stone Skin";
		if (desc) return "Gives bonus to AC for a while.";
#endif
    
		{
			int base = spell_power(20);
			int sides = spell_power(30);

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_shield(randint1(sides) + base, FALSE);
			}
		}
		break;

	case 18:
#ifdef JP
		if (name) return "真・耐性";
		if (desc) return "一定時間、酸、電撃、炎、冷気、毒に対する耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Resistance True";
		if (desc) return "Gives resistance to fire, cold, electricity, acid and poison for a while. These resistances can be added to which from equipment for more powerful resistances.";
#endif
    
		{
			int base = spell_power(20);

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

	case 19:
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

	case 20:
#ifdef JP
		if (name) return "試金石";
		if (desc) return "アイテムの持つ能力を完全に知る。";
#else
		if (name) return "Stone Tell";
		if (desc) return "*Identifies* an item.";
#endif
    
		{
			if (cast)
			{
				if (!identify_fully(NULL)) return NULL;
			}
		}
		break;

	case 21:
#ifdef JP
		if (name) return "石の壁";
		if (desc) return "自分の周囲に花崗岩の壁を作る。";
#else
		if (name) return "Wall of Stone";
		if (desc) return "Creates granite walls in all adjacent squares.";
#endif
    
		{
			if (cast)
			{
				wall_stone();
			}
		}
		break;

	case 22:
#ifdef JP
		if (name) return "腐食防止";
		if (desc) return "アイテムを酸で傷つかないよう加工する。";
#else
		if (name) return "Protect from Corrosion";
		if (desc) return "Makes an equipment acid-proof.";
#endif
    
		{
			if (cast)
			{
				if (!rustproof()) return NULL;
			}
		}
		break;

	case 23:
#ifdef JP
		if (name) return "陽光召喚";
		if (desc) return "自分を中心とした光の球を発生させる。さらに、その階全体を永久に照らし、ダンジョン内すべてのアイテムを感知する。";
#else
		if (name) return "Call Sunlight";
		if (desc) return "Generates ball of light centered on you. Maps and lights whole dungeon level. Knows all objects location.";
#endif
    
		{
			int dam = spell_power(150);
			int rad = 8;

			if (info) return info_damage(0, 0, dam/2);

			if (cast)
			{
				fire_ball(GF_LITE, 0, dam, rad);
				chg_virtue(V_KNOWLEDGE, 1);
				chg_virtue(V_ENLIGHTEN, 1);
				wiz_lite(FALSE);

				if ((prace_is_(RACE_VAMPIRE) || (p_ptr->mimic_form == MIMIC_VAMPIRE)) && !res_save_default(RES_LITE))
				{
					msg_print("The sunlight scorches your flesh!");
					take_hit(DAMAGE_NOESCAPE, 50, "sunlight", -1);
				}
			}
		}
		break;

	case 24:
#ifdef JP
		if (name) return "地震";
		if (desc) return "周囲のダンジョンを揺らし、壁と床をランダムに入れ変える。";
#else
		if (name) return "Earthquake";
		if (desc) return "Shakes dungeon structure, and results in random swapping of floors and walls.";
#endif
    
		{
			int rad = spell_power(10);

			if (info) return info_radius(rad);

			if (cast)
			{
				earthquake(py, px, rad);
			}
		}
		break;

	case 25:
		if (name) return "Fire Storm";
		if (desc) return "Fires a huge ball of fire.";
    
		{
			int dam = spell_power(60 + plev * 2);
			int rad = plev / 12 + 1;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_FIRE, dir, dam, rad);
			}
		}
		break;

	case 26:
#ifdef JP
		if (name) return "ブリザード";
		if (desc) return "巨大な冷気の球を放つ。";
#else
		if (name) return "Blizzard";
		if (desc) return "Fires a huge ball of cold.";
#endif
    
		{
			int dam = spell_power(70 + plev * 2);
			int rad = plev / 12 + 1;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_COLD, dir, dam, rad);
			}
		}
		break;

	case 27:
#ifdef JP
		if (name) return "稲妻嵐";
		if (desc) return "巨大な電撃の球を放つ。";
#else
		if (name) return "Lightning Storm";
		if (desc) return "Fires a huge electric ball.";
#endif
    
		{
			int dam = spell_power(90 + plev * 2);
			int rad = plev / 12 + 1;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_ELEC, dir, dam, rad);
				break;
			}
		}
		break;

	case 28:
#ifdef JP
		if (name) return "渦潮";
		if (desc) return "巨大な水の球を放つ。";
#else
		if (name) return "Whirlpool";
		if (desc) return "Fires a huge ball of water.";
#endif
    
		{
			int dam = spell_power(100 + plev * 2);
			int rad = plev / 12 + 1;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_WATER, dir, dam, rad);
			}
		}
		break;

	case 29:
		if (name) return "Ice Bolt";
		if (desc) return "Fires a bolt of ice.";
    
		{
			int dice = 5 + 15*plev/50;
			int sides = 15;

			if (info) return info_damage(spell_power(dice), sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_bolt(GF_ICE, dir, spell_power(damroll(dice, sides)));
			}
		}
		break;

	case 30:
		if (name) return "Gravity Storm";
		if (desc) return "Fires a huge ball of gravity.";
    
		{
			int dam = spell_power(70 + plev * 2);
			int rad = plev / 12 + 1;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_GRAVITY, dir, dam, rad);
			}
		}
		break;

	case 31:
		if (name) return "Nature's Wrath";
		if (desc) return "You unleash Nature's full fury, the exact consequences of which can't be predicted.";

		if (cast)
		{
			int i;
			switch (randint1(6))
			{
			case 1: /* The original effect: Line of Sight damage, earthquake, disintegration ball */
				msg_print("Nature's Fury is unleashed!");
				dispel_monsters(spell_power(4 * plev));
				earthquake(py, px, spell_power(20 + plev / 2));
				project(0, spell_power(1 + plev / 12), py, px, spell_power((100 + plev) * 2), GF_DISINTEGRATE, PROJECT_KILL | PROJECT_ITEM, -1);
				break;

			case 2: /* Deadly bolt of lightning */
				msg_print("Your hands crackle with electricity!");
				if (!get_aim_dir(&dir)) return NULL;
				fire_bolt(GF_ELEC, dir, spell_power(plev * 8));
				break;

			case 3: /* Immense thunderclap */
				msg_print("There is a large thunderclap!");
				project_hack(GF_SOUND, spell_power(plev * 5));
				break;

			case 4: /* Gravitational Wave */
				msg_print("Space warps around you!");
				project_hack(GF_GRAVITY, spell_power(plev * 4));
				break;

			case 5: /* Elemental Storm */
				msg_print("You unleash the elements!");
				project(0, spell_power(1 + plev / 12), py, px, spell_power((120 + plev) * 2), GF_FIRE, PROJECT_KILL | PROJECT_ITEM, -1);
				project(0, spell_power(1 + plev / 12), py, px, spell_power((120 + plev) * 2), GF_COLD, PROJECT_KILL | PROJECT_ITEM, -1);
				project(0, spell_power(1 + plev / 12), py, px, spell_power((120 + plev) * 2), GF_ELEC, PROJECT_KILL | PROJECT_ITEM, -1);
				break;

			case 6: /* Rock Storm */
				msg_print("You fire a storm of boulders!");
				if (!get_aim_dir(&dir)) return NULL;
				for (i = 0; i < 3; i++)
					fire_ball(GF_SHARDS, dir, spell_power(70 + plev), 1);
				break;
			}
		}
    	break;
	}

	return "";
}


static cptr do_chaos_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
	bool spoil = (mode == SPELL_SPOIL_DESC) ? TRUE : FALSE;

#ifdef JP
	static const char s_dam[] = "損傷:";
	static const char s_random[] = "ランダム";
#else
	static const char s_dam[] = "dam ";
	static const char s_random[] = "random";
#endif

	int dir;
	int plev = p_ptr->lev;

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "マジック・ミサイル";
		if (desc) return "弱い魔法の矢を放つ。";
#else
		if (name) return "Magic Missile";
		if (desc) return "Fires a weak bolt of magic.";
#endif
    
		{
			int dice = spell_power(3 + ((plev - 1) / 5));
			int sides = 4;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_bolt_or_beam(beam_chance() - 10, GF_MISSILE, dir, damroll(dice, sides));
			}
		}
		break;

	case 1:
#ifdef JP
		if (name) return "トラップ/ドア破壊";
		if (desc) return "隣接する罠と扉を破壊する。";
#else
		if (name) return "Trap / Door Destruction";
		if (desc) return "Destroys all traps in adjacent squares.";
#endif
    
		{
			int rad = 1;

			if (info) return info_radius(rad);

			if (cast)
			{
				destroy_doors_touch();
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "閃光";
		if (desc) return "光源が照らしている範囲か部屋全体を永久に明るくする。";
#else
		if (name) return "Flash of Light";
		if (desc) return "Lights up nearby area and the inside of a room permanently.";
#endif
    
		{
			int dice = 2;
			int sides = spell_power(plev / 2);
			int rad = (plev / 10) + 1;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				lite_area(damroll(dice, sides), rad);
			}
		}
		break;

	case 3:
#ifdef JP
		if (name) return "混乱の手";
		if (desc) return "相手を混乱させる攻撃をできるようにする。";
#else
		if (name) return "Touch of Confusion";
		if (desc) return "Attempts to confuse the next monster that you hit.";
#endif
    
		{
			if (cast)
			{
				if (!(p_ptr->special_attack & ATTACK_CONFUSE))
				{
#ifdef JP
					msg_print("あなたの手は光り始めた。");
#else
					msg_print("Your hands start glowing.");
#endif

					p_ptr->special_attack |= ATTACK_CONFUSE;
					p_ptr->redraw |= (PR_STATUS);
				}
			}
		}
		break;

	case 4:
#ifdef JP
		if (name) return "魔力炸裂";
		if (desc) return "魔法の球を放つ。";
#else
		if (name) return "Mana Burst";
		if (desc) return "Fires a ball of magic.";
#endif
    
		{
			int dice = 3;
			int sides = spell_power(5);
			int rad = spell_power((plev < 30) ? 2 : 3);
			int base;

			if (p_ptr->pclass == CLASS_MAGE ||
			    p_ptr->pclass == CLASS_BLOOD_MAGE ||
			    p_ptr->pclass == CLASS_HIGH_MAGE ||
			    p_ptr->pclass == CLASS_SORCERER)
				base = spell_power(plev + plev / 2);
			else
				base = spell_power(plev + plev / 4);


			if (info) return info_damage(dice, sides, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_MISSILE, dir, damroll(dice, sides) + base, rad);

				/*
				 * Shouldn't actually use GF_MANA, as
				 * it will destroy all items on the
				 * floor
				 */
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "ファイア・ボルト";
		if (desc) return "炎のボルトもしくはビームを放つ。";
#else
		if (name) return "Fire Bolt";
		if (desc) return "Fires a bolt or beam of fire.";
#endif
    
		{
			int dice = spell_power(8 + (plev - 5) / 4);
			int sides = 8;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_bolt_or_beam(beam_chance(), GF_FIRE, dir, damroll(dice, sides));
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "力の拳";
		if (desc) return "ごく小さな分解の球を放つ。";
#else
		if (name) return "Fist of Force";
		if (desc) return "Fires a tiny ball of disintegration.";
#endif
    
		{
			int dice = spell_power(8 + ((plev - 5) / 4));
			int sides = 8;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_DISINTEGRATE, dir,
					damroll(dice, sides), 0);
			}
		}
		break;

	case 7:
#ifdef JP
		if (name) return "テレポート";
		if (desc) return "遠距離のテレポートをする。";
#else
		if (name) return "Teleport Self";
		if (desc) return "Teleport long distance.";
#endif
    
		{
			int range = plev * 5;

			if (info) return info_range(range);

			if (cast)
			{
				teleport_player(range, 0L);
			}
		}
		break;

	case 8:
#ifdef JP
		if (name) return "ワンダー";
		if (desc) return "モンスターにランダムな効果を与える。";
#else
		if (name) return "Wonder";
		if (desc) return "Fires something with random effects.";
#endif
    
		{
			if (info) return s_random;

			if (cast)
			{

				if (!get_aim_dir(&dir)) return NULL;

				cast_wonder(dir);
			}
		}
		break;

	case 9:
#ifdef JP
		if (name) return "カオス・ボルト";
		if (desc) return "カオスのボルトもしくはビームを放つ。";
#else
		if (name) return "Chaos Bolt";
		if (desc) return "Fires a bolt or ball of chaos.";
#endif
    
		{
			int dice = spell_power(10 + (plev - 5) / 4);
			int sides = 8;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_bolt_or_beam(beam_chance(), GF_CHAOS, dir, damroll(dice, sides));
			}
		}
		break;

	case 10:
#ifdef JP
		if (name) return "ソニック・ブーム";
		if (desc) return "自分を中心とした轟音の球を発生させる。";
#else
		if (name) return "Sonic Boom";
		if (desc) return "Generates a ball of sound centered on you.";
#endif
    
		{
			int dam = spell_power(60 + plev*3/2);
			int rad = spell_power(plev / 10 + 2);

			if (info) return info_damage(0, 0, dam/2);

			if (cast)
			{
#ifdef JP
				msg_print("ドーン！部屋が揺れた！");
#else
				msg_print("BOOM! Shake the room!");
#endif

				project(0, rad, py, px, dam, GF_SOUND, PROJECT_KILL | PROJECT_ITEM, -1);
			}
		}
		break;

	case 11:
#ifdef JP
		if (name) return "破滅の矢";
		if (desc) return "純粋な魔力のビームを放つ。";
#else
		if (name) return "Doom Bolt";
		if (desc) return "Fires a beam of pure mana.";
#endif
    
		{
			int dice = spell_power(11 + (plev - 5) / 4);
			int sides = 8;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_beam(GF_MANA, dir, damroll(dice, sides));
			}
		}
		break;

	case 12:
#ifdef JP
		if (name) return "ファイア・ボール";
		if (desc) return "炎の球を放つ。";
#else
		if (name) return "Fire Ball";
		if (desc) return "Fires a ball of fire.";
#endif
    
		{
			int dam = spell_power(plev + 55);
			int rad = spell_power(2);

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_FIRE, dir, dam, rad);
			}
		}
		break;

	case 13:
#ifdef JP
		if (name) return "テレポート・アウェイ";
		if (desc) return "モンスターをテレポートさせるビームを放つ。抵抗されると無効。";
#else
		if (name) return "Teleport Other";
		if (desc) return "Teleports all monsters on the line away unless resisted.";
#endif
    
		{
			int power = spell_power(plev*2);

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_beam(GF_AWAY_ALL, dir, power);
			}
		}
		break;

	case 14:
#ifdef JP
		if (name) return "破壊の言葉";
		if (desc) return "周辺のアイテム、モンスター、地形を破壊する。";
#else
		if (name) return "Word of Destruction";
		if (desc) return "Destroy everything in nearby area.";
#endif
    
		{
			int base = 12;
			int sides = 4;

			if (cast)
			{
				destroy_area(py, px, base + randint1(sides), spell_power(4 * p_ptr->lev));
			}
		}
		break;

	case 15:
#ifdef JP
		if (name) return "ログルス発動";
		if (desc) return "巨大なカオスの球を放つ。";
#else
		if (name) return "Invoke Logrus";
		if (desc) return "Fires a huge ball of chaos.";
#endif
    
		{
			int dam = spell_power(plev * 2 + 99);
			int rad = spell_power(plev / 5);

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_CHAOS, dir, dam, rad);
			}
		}
		break;

	case 16:
#ifdef JP
		if (name) return "他者変容";
		if (desc) return "モンスター1体を変身させる。抵抗されると無効。";
#else
		if (name) return "Polymorph Other";
		if (desc) return "Attempts to polymorph a monster.";
#endif
    
		{
			int power = spell_power(plev);

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				poly_monster(dir);
			}
		}
		break;

	case 17:
#ifdef JP
		if (name) return "連鎖稲妻";
		if (desc) return "全方向に対して電撃のビームを放つ。";
#else
		if (name) return "Chain Lightning";
		if (desc) return "Fires lightning beams in all directions.";
#endif
    
		{
			int dice = spell_power(5 + plev / 10);
			int sides = 8;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				for (dir = 0; dir <= 9; dir++)
					fire_beam(GF_ELEC, dir, damroll(dice, sides));
			}
		}
		break;

	case 18:
#ifdef JP
		if (name) return "魔力封入";
		if (desc) return "杖/魔法棒の充填回数を増やすか、充填中のロッドの充填時間を減らす。";
#else
		if (name) return "Arcane Binding";
		if (desc) return "Recharges staffs, wands or rods.";
#endif
    
		{
			int power = spell_power(90);

			if (info) return info_power(power);

			if (cast)
			{
				if (!recharge(power)) return NULL;
			}
		}
		break;

	case 19:
#ifdef JP
		if (name) return "原子分解";
		if (desc) return "巨大な分解の球を放つ。";
#else
		if (name) return "Disintegrate";
		if (desc) return "Fires a huge ball of disintegration.";
#endif
    
		{
			int dam = spell_power(plev + 70);
			int rad = 3 + plev / 40;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_DISINTEGRATE, dir, dam, rad);
			}
		}
		break;

	case 20:
#ifdef JP
		if (name) return "現実変容";
		if (desc) return "現在の階を再構成する。";
#else
		if (name) return "Alter Reality";
		if (desc) return "Recreates current dungeon level.";
#endif
    
		{
			int base = 15;
			int sides = 20;

			if (info) return info_delay(base, sides);

			if (cast)
			{
				alter_reality();
			}
		}
		break;

	case 21:
#ifdef JP
		if (name) return "マジック・ロケット";
		if (desc) return "ロケットを発射する。";
#else
		if (name) return "Magic Rocket";
		if (desc) return "Fires a magic rocket.";
#endif
    
		{
			int dam = spell_power(120 + plev * 2);
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

				fire_rocket(GF_ROCKET, dir, dam, rad);
			}
		}
		break;

	case 22:
#ifdef JP
		if (name) return "混沌の刃";
		if (desc) return "武器にカオスの属性をつける。";
#else
		if (name) return "Chaos Branding";
		if (desc) return "Makes current weapon a Chaotic weapon.";
#endif
    
		{
			if (cast)
			{
				brand_weapon(EGO_CHAOTIC);
			}
		}
		break;

	case 23:
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
				else mode |= PM_NO_PET;
				if (!(pet && (plev < 50))) mode |= PM_ALLOW_GROUP;

				if (summon_specific((pet ? -1 : 0), py, px, (plev * 3) / 2, SUMMON_DEMON, mode))
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

	case 24:
#ifdef JP
		if (name) return "重力光線";
		if (desc) return "重力のビームを放つ。";
#else
		if (name) return "Beam of Gravity";
		if (desc) return "Fires a beam of gravity.";
#endif
    
		{
			int dice = spell_power(9 + (plev - 5) / 4);
			int sides = 8;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_beam(GF_GRAVITY, dir, damroll(dice, sides));
			}
		}
		break;

	case 25:
#ifdef JP
		if (name) return "流星群";
		if (desc) return "自分の周辺に隕石を落とす。";
#else
		if (name) return "Meteor Swarm";
		if (desc) return "Makes meteor balls fall down to nearby random locations.";
#endif
    
		{
			int dam = spell_power(plev * 2);
			int rad = 2;

			if (info) return info_multi_damage(dam);

			if (cast)
			{
				cast_meteor(dam, rad);
			}
		}
		break;

	case 26:
#ifdef JP
		if (name) return "焔の一撃";
		if (desc) return "自分を中心とした超巨大な炎の球を発生させる。";
#else
		if (name) return "Flame Strike";
		if (desc) return "Generate a huge ball of fire centered on you.";
#endif
    
		{
			int dam = spell_power(300 + 3 * plev);
			int rad = 8;

			if (info) return info_damage(0, 0, dam/2);

			if (cast)
			{
				fire_ball(GF_FIRE, 0, dam, rad);
			}
		}
		break;

	case 27:
#ifdef JP
		if (name) return "混沌召来";
		if (desc) return "ランダムな属性の球やビームを発生させる。";
#else
		if (name) return "Call Chaos";
		if (desc) return "Generate random kind of balls or beams.";
#endif
    
		{
			if (info) return format("%s150 / 250", s_dam);

			if (cast)
			{
				call_chaos();
			}
		}
		break;

	case 28:
#ifdef JP
		if (name) return "自己変容";
		if (desc) return "自分を変身させようとする。";
#else
		if (name) return "Polymorph Self";
		if (desc) return "Polymorphs yourself.";
#endif
    
		{
			if (cast)
			{
#ifdef JP
				if (!get_check("変身します。よろしいですか？")) return NULL;
#else
				if (!get_check("You will polymorph yourself. Are you sure? ")) return NULL;
#endif
				do_poly_self();
			}
		}
		break;

	case 29:
#ifdef JP
		if (name) return "魔力の嵐";
		if (desc) return "非常に強力で巨大な純粋な魔力の球を放つ。";
#else
		if (name) return "Mana Storm";
		if (desc) return "Fires an extremely powerful huge ball of pure mana.";
#endif
    
		{
			int dam = spell_power(300 + plev * 4);
			int rad = spell_power(4);

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_MANA, dir, dam, rad);
			}
		}
		break;

	case 30:
#ifdef JP
		if (name) return "ログルスのブレス";
		if (desc) return "非常に強力なカオスの球を放つ。";
#else
		if (name) return "Breathe Logrus";
		if (desc) return "Fires an extremely powerful ball of chaos.";
#endif
    
		{
			int dam = spell_power(p_ptr->chp);
			int rad = spell_power(2);

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_CHAOS, dir, dam, rad);
			}
		}
		break;

	case 31:
#ifdef JP
		if (name) return "虚無召来";
		if (desc) return "自分に周囲に向かって、ロケット、純粋な魔力の球、放射性廃棄物の球を放つ。ただし、壁に隣接して使用すると広範囲を破壊する。";
#else
		if (name) return "Call the Void";
		if (desc) return "Fires rockets, mana balls and nuclear waste balls in all directions each unless you are not adjacent to any walls. Otherwise *destroys* huge area.";
#endif
    
		{
			if (info) return format("%s3 * 175", s_dam);

			if (cast)
			{
				call_the_();
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
	bool spoil = (mode == SPELL_SPOIL_DESC) ? TRUE : FALSE;

#ifdef JP
	static const char s_dam[] = "損傷:";
	static const char s_random[] = "ランダム";
#else
	static const char s_dam[] = "dam ";
	static const char s_random[] = "random";
#endif

	int dir;
	int plev = p_ptr->lev;

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "無生命感知";
		if (desc) return "近くの生命のないモンスターを感知する。";
#else
		if (name) return "Detect Unlife";
		if (desc) return "Detects all nonliving monsters in your vicinity.";
#endif
    
		{
			int rad = DETECT_RAD_DEFAULT;

			if (info) return info_radius(rad);

			if (cast)
			{
				detect_monsters_nonliving(rad);
			}
		}
		break;

	case 1:
#ifdef JP
		if (name) return "呪殺弾";
		if (desc) return "ごく小さな邪悪な力を持つボールを放つ。善良なモンスターには大きなダメージを与える。";
#else
		if (name) return "Malediction";
		if (desc) return "Fires a tiny ball of evil power which hurts good monsters greatly.";
#endif
    
		{
			int dice = 3 + (plev - 1) / 5;
			int sides = 4;
			int rad = 0;

			if (info) return info_damage(spell_power(dice), sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				/*
				 * A radius-0 ball may (1) be aimed at
				 * objects etc., and will affect them;
				 * (2) may be aimed at ANY visible
				 * monster, unlike a 'bolt' which must
				 * travel to the monster.
				 */

				fire_ball(GF_HELL_FIRE, dir, spell_power(damroll(dice, sides)), rad);

				if (one_in_(5))
				{
					/* Special effect first */
					int effect = randint1(1000);

					if (effect == 666)
						fire_ball_hide(GF_DEATH_RAY, dir, spell_power(plev * 200), 0);
					else if (effect < 500)
						fire_ball_hide(GF_TURN_ALL, dir, spell_power(plev), 0);
					else if (effect < 800)
						fire_ball_hide(GF_OLD_CONF, dir, spell_power(plev), 0);
					else
						fire_ball_hide(GF_STUN, dir, spell_power(plev), 0);
				}
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "邪悪感知";
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

	case 3:
#ifdef JP
		if (name) return "悪臭雲";
		if (desc) return "毒の球を放つ。";
#else
		if (name) return "Stinking Cloud";
		if (desc) return "Fires a ball of poison.";
#endif
    
		{
			int dam = spell_power(10 + plev / 2);
			int rad = 2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_POIS, dir, dam, rad);
			}
		}
		break;

	case 4:
#ifdef JP
		if (name) return "黒い眠り";
		if (desc) return "1体のモンスターを眠らせる。抵抗されると無効。";
#else
		if (name) return "Black Sleep";
		if (desc) return "Attempts to sleep a monster.";
#endif
    
		{
			int power = spell_power(plev * 2);

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				sleep_monster(dir);
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "耐毒";
		if (desc) return "一定時間、毒への耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Undead Resistance";
		if (desc) return "Gives resistance to poison and cold. This resistance can be added to which from equipment for more powerful resistance.";
#endif
    
		{
			int base = spell_power(20);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_oppose_cold(randint1(base) + base, FALSE);
				set_oppose_pois(randint1(base) + base, FALSE);
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "恐慌";
		if (desc) return "モンスター1体を恐怖させ、朦朧させる。抵抗されると無効。";
#else
		if (name) return "Horrify";
		if (desc) return "Attempts to scare and stun a monster.";
#endif
    
		{
			int power = spell_power(plev * 2);

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fear_monster(dir, power);
				stun_monster(dir, power);
			}
		}
		break;

	case 7:
#ifdef JP
		if (name) return "アンデッド従属";
		if (desc) return "アンデッド1体を魅了する。抵抗されると無効。";
#else
		if (name) return "Enslave Undead";
		if (desc) return "Attempts to charm an undead monster.";
#endif
    
		{
			int power = spell_power(plev * 2);

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				control_one_undead(dir, power);
			}
		}
		break;

	case 8:
#ifdef JP
		if (name) return "エントロピーの球";
		if (desc) return "生命のある者に効果のある球を放つ。";
#else
		if (name) return "Orb of Entropy";
		if (desc) return "Fires a ball which damages living monsters.";
#endif
    
		{
			int dice = 3;
			int sides = 6;
			int rad = (plev < 30) ? 2 : 3;
			int base;

			if (p_ptr->pclass == CLASS_MAGE ||
			    p_ptr->pclass == CLASS_BLOOD_MAGE ||
			    p_ptr->pclass == CLASS_HIGH_MAGE ||
			    p_ptr->pclass == CLASS_SORCERER)
				base = plev + plev / 2;
			else
				base = plev + plev / 4;


			if (info) return info_damage(dice, spell_power(sides), spell_power(base));

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_OLD_DRAIN, dir, spell_power(damroll(dice, sides) + base), rad);
			}
		}
		break;

	case 9:
#ifdef JP
		if (name) return "地獄の矢";
		if (desc) return "地獄のボルトもしくはビームを放つ。";
#else
		if (name) return "Nether Bolt";
		if (desc) return "Fires a bolt or beam of nether.";
#endif
    
		{
			int dice = 8 + (plev - 5) / 4;
			int sides = 8;

			if (info) return info_damage(spell_power(dice), sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_bolt_or_beam(beam_chance(), GF_NETHER, dir, spell_power(damroll(dice, sides)));
			}
		}
		break;

	case 10:
#ifdef JP
		if (name) return "殺戮雲";
		if (desc) return "自分を中心とした毒の球を発生させる。";
#else
		if (name) return "Cloud kill";
		if (desc) return "Generate a ball of poison centered on you.";
#endif
    
		{
			int dam = spell_power((30 + plev) * 2);
			int rad = spell_power(plev / 10 + 2);

			if (info) return info_damage(0, 0, dam/2);

			if (cast)
			{
				project(0, rad, py, px, dam, GF_POIS, PROJECT_KILL | PROJECT_ITEM, -1);
			}
		}
		break;

	case 11:
#ifdef JP
		if (name) return "モンスター消滅";
		if (desc) return "モンスター1体を消し去る。経験値やアイテムは手に入らない。抵抗されると無効。";
#else
		if (name) return "Genocide One";
		if (desc) return "Attempts to vanish a monster.";
#endif
    
		{
			int power = spell_power(plev + 50);

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball_hide(GF_GENOCIDE, dir, power, 0);
			}
		}
		break;

	case 12:
#ifdef JP
		if (name) return "毒の刃";
		if (desc) return "武器に毒の属性をつける。";
#else
		if (name) return "Poison Branding";
		if (desc) return "Makes current weapon poison branded.";
#endif
    
		{
			if (cast)
			{
				brand_weapon(EGO_BRAND_POIS);
			}
		}
		break;

	case 13:
#ifdef JP
		if (name) return "吸血ドレイン";
		if (desc) return "モンスター1体から生命力を吸いとる。吸いとった生命力によって満腹度が上がる。";
#else
		if (name) return "Vampiric Drain";
		if (desc) return "Absorbs some HP from a monster and gives them to you. You will also gain nutritional sustenance from this.";
#endif
    
		{
			int dice = 1;
			int sides = spell_power(plev * 2);
			int base = spell_power(plev * 2);

			if (info) return info_damage(dice, sides, base);

			if (cast)
			{
				int dam = base + damroll(dice, sides);

				if (!get_aim_dir(&dir)) return NULL;

				if (drain_life(dir, dam))
				{
					chg_virtue(V_SACRIFICE, -1);
					chg_virtue(V_VITALITY, -1);

					hp_player(dam);

					/*
					 * Gain nutritional sustenance:
					 * 150/hp drained
					 *
					 * A Food ration gives 5000
					 * food points (by contrast)
					 * Don't ever get more than
					 * "Full" this way But if we
					 * ARE Gorged, it won't cure
					 * us
					 */
					dam = p_ptr->food + MIN(5000, 100 * dam);

					/* Not gorged already */
					if (p_ptr->food < PY_FOOD_MAX)
						set_food(dam >= PY_FOOD_MAX ? PY_FOOD_MAX - 1 : dam);
				}
			}
		}
		break;

	case 14:
#ifdef JP
		if (name) return "反魂の術";
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

	case 15:
#ifdef JP
		if (name) return "抹殺";
		if (desc) return "指定した文字のモンスターを現在の階から消し去る。抵抗されると無効。";
#else
		if (name) return "Genocide";
		if (desc) return "Eliminates an entire class of monster, exhausting you.  Powerful or unique monsters may resist.";
#endif
    
		{
			int power = spell_power(plev+50);

			if (info) return info_power(power);

			if (cast)
			{
				symbol_genocide(power, TRUE);
			}
		}
		break;

	case 16:
#ifdef JP
		if (name) return "狂戦士化";
		if (desc) return "狂戦士化し、恐怖を除去する。";
#else
		if (name) return "Berserk";
		if (desc) return "Gives bonus to hit and HP, immunity to fear for a while. But decreases AC.";
#endif
    
		{
			int base = spell_power(25);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_shero(randint1(base) + base, FALSE);
				hp_player(30);
			}
		}
		break;

	case 17:
#ifdef JP
		if (name) return "悪霊召喚";
		if (desc) return "ランダムで様々な効果が起こる。";
#else
		if (name) return "Invoke Spirits";
		if (desc) return "Causes random effects.";
#endif
    
		{
			if (info) return s_random;

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				cast_invoke_spirits(dir);
			}
		}
		break;

	case 18:
#ifdef JP
		if (name) return "暗黒の矢";
		if (desc) return "暗黒のボルトもしくはビームを放つ。";
#else
		if (name) return "Dark Bolt";
		if (desc) return "Fires a bolt or beam of darkness.";
#endif
    
		{
			int dice = 4 + (plev - 5) / 4;
			int sides = 8;

			if (info) return info_damage(spell_power(dice), sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_bolt_or_beam(beam_chance(), GF_DARK, dir, spell_power(damroll(dice, sides)));
			}
		}
		break;

	case 19:
#ifdef JP
		if (name) return "狂乱戦士";
		if (desc) return "狂戦士化し、恐怖を除去し、加速する。";
#else
		if (name) return "Battle Frenzy";
		if (desc) return "Gives another bonus to hit and HP, immunity to fear for a while. Hastes you. But decreases AC.";
#endif
    
		{
			int b_base = spell_power(25);
			int sp_base = spell_power(plev / 2);
			int sp_sides = 20 + plev / 2;

			if (info) return info_duration(b_base, b_base);

			if (cast)
			{
				set_hero(randint1(b_base) + b_base, FALSE);
				set_blessed(randint1(b_base) + b_base, FALSE);
				set_fast(randint1(sp_sides) + sp_base, FALSE);
			}
		}
		break;

	case 20:
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

	case 21:
#ifdef JP
		if (name) return "真・吸血";
		if (desc) return "モンスター1体から生命力を吸いとる。吸いとった生命力によって体力が回復する。";
#else
		if (name) return "Vampirism True";
		if (desc) return "Fires 3 bolts. Each of the bolts absorbs some HP from a monster and gives them to you.";
#endif
    
		{
			int dam = spell_power(100);

			if (info) return format("%s3*%d", s_dam, dam);

			if (cast)
			{
				int i;

				if (!get_aim_dir(&dir)) return NULL;

				chg_virtue(V_SACRIFICE, -1);
				chg_virtue(V_VITALITY, -1);

				for (i = 0; i < 3; i++)
				{
					if (drain_life(dir, dam))
						hp_player(dam);
				}
			}
		}
		break;

	case 22:
#ifdef JP
		if (name) return "死の言魂";
		if (desc) return "視界内の生命のあるモンスターにダメージを与える。";
#else
		if (name) return "Nether Wave";
		if (desc) return "Damages all living monsters in sight.";
#endif
    
		{
			int sides = spell_power(plev * 3);

			if (info) return info_damage(1, sides, 0);

			if (cast)
			{
				dispel_living(randint1(sides));
			}
		}
		break;

	case 23:
#ifdef JP
		if (name) return "暗黒の嵐";
		if (desc) return "巨大な暗黒の球を放つ。";
#else
		if (name) return "Darkness Storm";
		if (desc) return "Fires a huge ball of darkness.";
#endif
    
		{
			int dam = spell_power(100 + plev * 2);
			int rad = spell_power(4);

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_DARK, dir, dam, rad);
			}
		}
		break;

	case 24:
#ifdef JP
		if (name) return "死の光線";
		if (desc) return "死の光線を放つ。";
#else
		if (name) return "Death Ray";
		if (desc) return "Fires a beam of death.";
#endif
    
		{
			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				death_ray(dir, plev);
			}
		}
		break;

	case 25:
#ifdef JP
		if (name) return "死者召喚";
		if (desc) return "1体のアンデッドを召喚する。";
#else
		if (name) return "Raise the Dead";
		if (desc) return "Summons an undead monster.";
#endif
    
		{
			if (cast)
			{
				int type;
				bool pet = one_in_(3);
				u32b mode = 0L;

				type = (plev > 47 ? SUMMON_HI_UNDEAD : SUMMON_UNDEAD);

				if (!pet || (pet && (plev > 24) && one_in_(3)))
					mode |= PM_ALLOW_GROUP;

				if (pet) mode |= PM_FORCE_PET;
				else mode |= (PM_ALLOW_UNIQUE | PM_NO_PET);

				if (summon_specific((pet ? -1 : 0), py, px, (plev * 3) / 2, type, mode))
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

					chg_virtue(V_UNLIFE, 1);
				}
			}
		}
		break;

	case 26:
#ifdef JP
		if (name) return "死者の秘伝";
		if (desc) return "アイテムを1つ識別する。レベルが高いとアイテムの能力を完全に知ることができる。";
#else
		if (name) return "Esoteria";
		if (desc) return "Identifies an item. Or *identifies* an item at higher level.";
#endif
    
		{
			if (cast)
			{
				if (randint1(50) > spell_power(plev))
				{
					if (!ident_spell(NULL)) return NULL;
				}
				else
				{
					if (!identify_fully(NULL)) return NULL;
				}
			}
		}
		break;

	case 27:
#ifdef JP
		if (name) return "吸血鬼変化";
		if (desc) return "一定時間、吸血鬼に変化する。変化している間は本来の種族の能力を失い、代わりに吸血鬼としての能力を得る。";
#else
		if (name) return "Polymorph Vampire";
		if (desc) return "Mimic a vampire for a while. Loses abilities of original race and gets abilities as a vampire.";
#endif
    
		{
			int base = spell_power(10 + plev / 2);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_mimic(base + randint1(base), MIMIC_VAMPIRE, FALSE);
			}
		}
		break;

	case 28:
#ifdef JP
		if (name) return "生命力復活";
		if (desc) return "失った経験値を回復する。";
#else
		if (name) return "Restore Life";
		if (desc) return "Restore lost experience.";
#endif
    
		{
			if (cast)
			{
				restore_level();
			}
		}
		break;

	case 29:
#ifdef JP
		if (name) return "周辺抹殺";
		if (desc) return "自分の周囲にいるモンスターを現在の階から消し去る。抵抗されると無効。";
#else
		if (name) return "Mass Genocide";
		if (desc) return "Eliminates all nearby monsters, exhausting you.  Powerful or unique monsters may be able to resist.";
#endif
    
		{
			int power = spell_power(plev + 50);

			if (info) return info_power(power);

			if (cast)
			{
				mass_genocide(power, TRUE);
			}
		}
		break;

	case 30:
#ifdef JP
		if (name) return "地獄の劫火";
		if (desc) return "邪悪な力を持つ宝珠を放つ。善良なモンスターには大きなダメージを与える。";
#else
		if (name) return "Hellfire";
		if (desc) return "Fires a powerful ball of evil power. Hurts good monsters greatly.";
#endif
    
		{
			int dam = spell_power(666);
			int rad = 3;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_HELL_FIRE, dir, dam, rad);
#ifdef JP
				take_hit(DAMAGE_USELIFE, 20 + randint1(30), "地獄の劫火の呪文を唱えた疲労", -1);
#else
				take_hit(DAMAGE_USELIFE, 20 + randint1(30), "the strain of casting Hellfire", -1);
#endif
			}
		}
		break;

	case 31:
#ifdef JP
		if (name) return "幽体化";
		if (desc) return "一定時間、壁を通り抜けることができ受けるダメージが軽減される幽体の状態に変身する。";
#else
		if (name) return "Wraithform";
		if (desc) return "Becomes wraith form which gives ability to pass walls and makes all damages half.";
#endif
    
		{
			int base = spell_power(plev / 2);

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

static cptr do_trump_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
	bool fail = (mode == SPELL_FAIL) ? TRUE : FALSE;
	bool spoil = (mode == SPELL_SPOIL_DESC) ? TRUE : FALSE;

#ifdef JP
	static const char s_random[] = "ランダム";
#else
	static const char s_random[] = "random";
#endif

	int dir;
	int plev = p_ptr->lev;
	int x = px;
	int y = py;

	if (!fail && use_old_target && target_okay() && los(py, px, target_row, target_col) && !one_in_(3))
	{
		y = target_row;
		x = target_col;
	}

	switch (spell)
	{
	case 0:
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
				if (mut_present(MUT_ASTRAL_GUIDE))
					energy_use = 30;
				teleport_player(range, 0L);
			}
		}
		break;

	case 1:
#ifdef JP
		if (name) return "蜘蛛のカード";
		if (desc) return "蜘蛛を召喚する。";
#else
		if (name) return "Trump Spiders";
		if (desc) return "Summons spiders.";
#endif
    
		{
			if (cast || fail)
			{
#ifdef JP
				msg_print("あなたは蜘蛛のカードに集中する...");
#else
				msg_print("You concentrate on the trump of an spider...");
#endif

				if (trump_summoning(1, !fail, y, x, 0, SUMMON_SPIDER, PM_ALLOW_GROUP))
				{
					if (fail)
					{
#ifdef JP
						msg_print("召喚された蜘蛛は怒っている！");
#else
						msg_print("The summoned spiders get angry!");
#endif
					}
				}
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "シャッフル";
		if (desc) return "カードの占いをする。";
#else
		if (name) return "Shuffle";
		if (desc) return "Causes random effects.";
#endif
    
		{
			if (info) return s_random;

			if (cast)
			{
				if (TRUE || get_check("Are you sure you wish to shuffle?"))
					cast_shuffle();
				else
					return NULL;
			}
		}
		break;

	case 3:
#ifdef JP
		if (name) return "フロア・リセット";
		if (desc) return "最深階を変更する。";
#else
		if (name) return "Reset Recall";
		if (desc) return "Resets the 'deepest' level for recall spell.";
#endif
    
		{
			if (cast)
			{
				if (!reset_recall()) return NULL;
			}
		}
		break;

	case 4:
#ifdef JP
		if (name) return "テレポート";
		if (desc) return "遠距離のテレポートをする。";
#else
		if (name) return "Teleport";
		if (desc) return "Teleport long distance.";
#endif
    
		{
			int range = plev * 4;

			if (info) return info_range(range);

			if (cast)
			{
				if (mut_present(MUT_ASTRAL_GUIDE))
					energy_use = 30;
				teleport_player(range, 0L);
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "感知のカード";
		if (desc) return "一定時間、テレパシー能力を得る。";
#else
		if (name) return "Trump Spying";
		if (desc) return "Gives telepathy for a while.";
#endif
    
		{
			int base = spell_power(25);
			int sides = spell_power(30);

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_tim_esp(randint1(sides) + base, FALSE);
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "テレポート・モンスター";
		if (desc) return "モンスターをテレポートさせるビームを放つ。抵抗されると無効。";
#else
		if (name) return "Teleport Away";
		if (desc) return "Teleports all monsters on the line away unless resisted.";
#endif
    
		{
			int power = spell_power(plev*2);

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_beam(GF_AWAY_ALL, dir, power);
			}
		}
		break;

	case 7:
#ifdef JP
		if (name) return "動物のカード";
		if (desc) return "1体の動物を召喚する。";
#else
		if (name) return "Trump Animals";
		if (desc) return "Summons an animal.";
#endif
    
		{
			if (cast || fail)
			{
				int type = (!fail ? SUMMON_ANIMAL_RANGER : SUMMON_ANIMAL);

#ifdef JP
				msg_print("あなたは動物のカードに集中する...");
#else
				msg_print("You concentrate on the trump of an animal...");
#endif

				if (trump_summoning(1, !fail, y, x, 0, type, 0L))
				{
					if (fail)
					{
#ifdef JP
						msg_print("召喚された動物は怒っている！");
#else
						msg_print("The summoned animal gets angry!");
#endif
					}
				}
			}
		}
		break;

	case 8:
#ifdef JP
		if (name) return "移動のカード";
		if (desc) return "アイテムを自分の足元へ移動させる。";
#else
		if (name) return "Trump Reach";
		if (desc) return "Pulls a distant item close to you.";
#endif
    
		{
			int weight = spell_power(plev * 15);

			if (info) return info_weight(weight);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fetch(dir, weight, FALSE);
			}
		}
		break;

	case 9:
#ifdef JP
		if (name) return "カミカゼのカード";
		if (desc) return "複数の爆発するモンスターを召喚する。";
#else
		if (name) return "Trump Kamikaze";
		if (desc) return "Summons monsters which explode by itself.";
#endif
    
		{
			if (cast || fail)
			{
				int x, y;
				int type;

				if (cast)
				{
					if (!target_set(TARGET_KILL)) return NULL;
					x = target_col;
					y = target_row;
				}
				else
				{
					/* Summons near player when failed */
					x = px;
					y = py;
				}

				if (p_ptr->pclass == CLASS_BEASTMASTER)
					type = SUMMON_KAMIKAZE_LIVING;
				else
					type = SUMMON_KAMIKAZE;

#ifdef JP
				msg_print("あなたはカミカゼのカードに集中する...");
#else
				msg_print("You concentrate on several trumps at once...");
#endif

				if (trump_summoning(2 + randint0(plev / 7), !fail, y, x, 0, type, 0L))
				{
					if (fail)
					{
#ifdef JP
						msg_print("召喚されたモンスターは怒っている！");
#else
						msg_print("The summoned creatures get angry!");
#endif
					}
				}
			}
		}
		break;

	case 10:
#ifdef JP
		if (name) return "幻霊召喚";
		if (desc) return "1体の幽霊を召喚する。";
#else
		if (name) return "Phantasmal Servant";
		if (desc) return "Summons a ghost.";
#endif
    
		{
			/* Phantasmal Servant is not summoned as enemy when failed */
			if (cast)
			{
				int summon_lev = plev * 2 / 3 + randint1(plev / 2);

				if (trump_summoning(1, !fail, y, x, (summon_lev * 3 / 2), SUMMON_PHANTOM, 0L))
				{
#ifdef JP
					msg_print("御用でございますか、御主人様？");
#else
					msg_print("'Your wish, master?'");
#endif
				}
			}
		}
		break;

	case 11:
#ifdef JP
		if (name) return "スピード・モンスター";
		if (desc) return "モンスター1体を加速させる。";
#else
		if (name) return "Haste Monster";
		if (desc) return "Hastes a monster.";
#endif
    
		{
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

				speed_monster(dir);
			}
		}
		break;

	case 12:
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

	case 13:
#ifdef JP
		if (name) return "次元の扉";
		if (desc) return "短距離内の指定した場所にテレポートする。";
#else
		if (name) return "Dimension Door";
		if (desc) return "Teleport to given location.";
#endif
    
		{
			int range = plev / 2 + 10;

			if (info) return info_range(range);

			if (cast)
			{
#ifdef JP
				msg_print("次元の扉が開いた。目的地を選んで下さい。");
#else
				msg_print("You open a dimensional gate. Choose a destination.");
#endif

				if (!dimension_door(range)) return NULL;
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
		if (name) return "怪物追放";
		if (desc) return "視界内の全てのモンスターをテレポートさせる。抵抗されると無効。";
#else
		if (name) return "Banish";
		if (desc) return "Teleports all monsters in sight away unless resisted.";
#endif
    
		{
			int power = spell_power(plev * 4);

			if (info) return info_power(power);

			if (cast)
			{
				banish_monsters(power);
			}
		}
		break;

	case 16:
#ifdef JP
		if (name) return "位置交換のカード";
		if (desc) return "1体のモンスターと位置を交換する。";
#else
		if (name) return "Swap Position";
		if (desc) return "Swap positions of you and a monster.";
#endif
    
		{
			if (cast)
			{
				bool result;

				/* HACK -- No range limit */
				project_length = -1;

				result = get_aim_dir(&dir);

				/* Restore range to default */
				project_length = 0;

				if (!result) return NULL;

				teleport_swap(dir);
			}
		}
		break;

	case 17:
#ifdef JP
		if (name) return "アンデッドのカード";
		if (desc) return "1体のアンデッドを召喚する。";
#else
		if (name) return "Trump Undead";
		if (desc) return "Summons an undead monster.";
#endif
    
		{
			if (cast || fail)
			{
#ifdef JP
				msg_print("あなたはアンデッドのカードに集中する...");
#else
				msg_print("You concentrate on the trump of an undead creature...");
#endif

				if (trump_summoning(1, !fail, y, x, 0, SUMMON_UNDEAD, 0L))
				{
					if (fail)
					{
#ifdef JP
						msg_print("召喚されたアンデッドは怒っている！");
#else
						msg_print("The summoned undead creature gets angry!");
#endif
					}
				}
			}
		}
		break;

	case 18:
#ifdef JP
		if (name) return "爬虫類のカード";
		if (desc) return "1体のヒドラを召喚する。";
#else
		if (name) return "Trump Reptiles";
		if (desc) return "Summons a hydra.";
#endif
    
		{
			if (cast || fail)
			{
#ifdef JP
				msg_print("あなたは爬虫類のカードに集中する...");
#else
				msg_print("You concentrate on the trump of a reptile...");
#endif

				if (trump_summoning(1, !fail, y, x, 0, SUMMON_HYDRA, 0L))
				{
					if (fail)
					{
#ifdef JP
						msg_print("召喚された爬虫類は怒っている！");
#else
						msg_print("The summoned reptile gets angry!");
#endif
					}
				}
			}
		}
		break;

	case 19:
#ifdef JP
		if (name) return "モンスターのカード";
		if (desc) return "複数のモンスターを召喚する。";
#else
		if (name) return "Trump Monsters";
		if (desc) return "Summons some monsters.";
#endif
    
		{
			if (cast || fail)
			{
				int type;

#ifdef JP
				msg_print("あなたはモンスターのカードに集中する...");
#else
				msg_print("You concentrate on several trumps at once...");
#endif

				if (p_ptr->pclass == CLASS_BEASTMASTER)
					type = SUMMON_LIVING;
				else
					type = 0;

				if (trump_summoning((1 + (plev - 15)/ 10), !fail, y, x, 0, type, 0L))
				{
					if (fail)
					{
#ifdef JP
						msg_print("召喚されたモンスターは怒っている！");
#else
						msg_print("The summoned creatures get angry!");
#endif
					}
				}

			}
		}
		break;

	case 20:
#ifdef JP
		if (name) return "ハウンドのカード";
		if (desc) return "1グループのハウンドを召喚する。";
#else
		if (name) return "Trump Hounds";
		if (desc) return "Summons a group of hounds.";
#endif
    
		{
			if (cast || fail)
			{
#ifdef JP
				msg_print("あなたはハウンドのカードに集中する...");
#else
				msg_print("You concentrate on the trump of a hound...");
#endif

				if (trump_summoning(1, !fail, y, x, 0, SUMMON_HOUND, PM_ALLOW_GROUP))
				{
					if (fail)
					{
#ifdef JP
						msg_print("召喚されたハウンドは怒っている！");
#else
						msg_print("The summoned hounds get angry!");
#endif
					}
				}
			}
		}
		break;

	case 21:
#ifdef JP
		if (name) return "トランプの刃";
		if (desc) return "武器にトランプの属性をつける。";
#else
		if (name) return "Trump Branding";
		if (desc) return "Makes current weapon a Trump weapon.";
#endif
    
		{
			if (cast)
			{
				brand_weapon(EGO_TRUMP);
			}
		}
		break;

	case 22:
#ifdef JP
		if (name) return "人間トランプ";
		if (desc) return "ランダムにテレポートする突然変異か、自分の意思でテレポートする突然変異が身につく。";
#else
		if (name) return "Living Trump";
		if (desc) return "Gives mutation which makes you teleport randomly or makes you able to teleport at will.";
#endif
    
		{
			if (cast)
			{
				int mutation;

				if (one_in_(7))
					/* Teleport control */
					mutation = MUT_TELEPORT;
				else
					/* Random teleportation (uncontrolled) */
					mutation = MUT_TELEPORT_RND;

				/* Gain the mutation */
				if (mut_gain(mutation))
				{
#ifdef JP
					msg_print("あなたは生きているカードに変わった。");
#else
					msg_print("You have turned into a Living Trump.");
#endif
				}
			}
		}
		break;

	case 23:
#ifdef JP
		if (name) return "サイバーデーモンのカード";
		if (desc) return "1体のサイバーデーモンを召喚する。";
#else
		if (name) return "Trump Cyberdemon";
		if (desc) return "Summons a cyber demon.";
#endif
    
		{
			if (cast || fail)
			{
#ifdef JP
				msg_print("あなたはサイバーデーモンのカードに集中する...");
#else
				msg_print("You concentrate on the trump of a Cyberdemon...");
#endif

				if (trump_summoning(1, !fail, y, x, 0, SUMMON_CYBER, 0L))
				{
					if (fail)
					{
#ifdef JP
						msg_print("召喚されたサイバーデーモンは怒っている！");
#else
						msg_print("The summoned Cyberdemon gets angry!");
#endif
					}
				}
			}
		}
		break;

	case 24:
#ifdef JP
		if (name) return "予見のカード";
		if (desc) return "近くの全てのモンスター、罠、扉、階段、財宝、そしてアイテムを感知する。";
#else
		if (name) return "Trump Divination";
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

	case 25:
#ifdef JP
		if (name) return "知識のカード";
		if (desc) return "アイテムの持つ能力を完全に知る。";
#else
		if (name) return "Trump Lore";
		if (desc) return "*Identifies* an item.";
#endif
    
		{
			if (cast)
			{
				if (!identify_fully(NULL)) return NULL;
			}
		}
		break;

	case 26:
#ifdef JP
		if (name) return "回復モンスター";
		if (desc) return "モンスター1体の体力を回復させる。";
#else
		if (name) return "Heal Monster";
		if (desc) return "Heal a monster.";
#endif
    
		{
			int heal = spell_power(plev * 10 + 200);

			if (info) return info_heal(0, 0, heal);

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

				heal_monster(dir, heal);
			}
		}
		break;

	case 27:
#ifdef JP
		if (name) return "ドラゴンのカード";
		if (desc) return "1体のドラゴンを召喚する。";
#else
		if (name) return "Trump Dragon";
		if (desc) return "Summons a dragon.";
#endif
    
		{
			if (cast || fail)
			{
#ifdef JP
				msg_print("あなたはドラゴンのカードに集中する...");
#else
				msg_print("You concentrate on the trump of a dragon...");
#endif

				if (trump_summoning(1, !fail, y, x, 0, SUMMON_DRAGON, 0L))
				{
					if (fail)
					{
#ifdef JP
						msg_print("召喚されたドラゴンは怒っている！");
#else
						msg_print("The summoned dragon gets angry!");
#endif
					}
				}
			}
		}
		break;

	case 28:
#ifdef JP
		if (name) return "隕石のカード";
		if (desc) return "自分の周辺に隕石を落とす。";
#else
		if (name) return "Trump Meteor";
		if (desc) return "Makes meteor balls fall down to nearby random locations.";
#endif
    
		{
			int dam = spell_power(plev * 2);
			int rad = 2;

			if (info) return info_multi_damage(dam);

			if (cast)
			{
				cast_meteor(dam, rad);
			}
		}
		break;

	case 29:
#ifdef JP
		if (name) return "デーモンのカード";
		if (desc) return "1体の悪魔を召喚する。";
#else
		if (name) return "Trump Demon";
		if (desc) return "Summons a demon.";
#endif
    
		{
			if (cast || fail)
			{
#ifdef JP
				msg_print("あなたはデーモンのカードに集中する...");
#else
				msg_print("You concentrate on the trump of a demon...");
#endif

				if (trump_summoning(1, !fail, y, x, 0, SUMMON_DEMON, 0L))
				{
					if (fail)
					{
#ifdef JP
						msg_print("召喚されたデーモンは怒っている！");
#else
						msg_print("The summoned demon gets angry!");
#endif
					}
				}
			}
		}
		break;

	case 30:
#ifdef JP
		if (name) return "地獄のカード";
		if (desc) return "1体の上級アンデッドを召喚する。";
#else
		if (name) return "Trump Greater Undead";
		if (desc) return "Summons a greater undead.";
#endif
    
		{
			if (cast || fail)
			{
#ifdef JP
				msg_print("あなたは強力なアンデッドのカードに集中する...");
#else
				msg_print("You concentrate on the trump of a greater undead being...");
#endif
				/* May allow unique depend on level and dice roll */
				if (trump_summoning(1, !fail, y, x, 0, SUMMON_HI_UNDEAD, PM_ALLOW_UNIQUE))
				{
					if (fail)
					{
#ifdef JP
						msg_print("召喚された上級アンデッドは怒っている！");
#else
						msg_print("The summoned greater undead creature gets angry!");
#endif
					}
				}
			}
		}
		break;

	case 31:
#ifdef JP
		if (name) return "古代ドラゴンのカード";
		if (desc) return "1体の古代ドラゴンを召喚する。";
#else
		if (name) return "Trump Ancient Dragon";
		if (desc) return "Summons an ancient dragon.";
#endif
    
		{
			if (cast)
			{
				int type;

				if (p_ptr->pclass == CLASS_BEASTMASTER)
					type = SUMMON_HI_DRAGON_LIVING;
				else
					type = SUMMON_HI_DRAGON;

#ifdef JP
				msg_print("あなたは古代ドラゴンのカードに集中する...");
#else
				msg_print("You concentrate on the trump of an ancient dragon...");
#endif

				/* May allow unique depend on level and dice roll */
				if (trump_summoning(1, !fail, y, x, 0, type, PM_ALLOW_UNIQUE))
				{
					if (fail)
					{
#ifdef JP
						msg_print("召喚された古代ドラゴンは怒っている！");
#else
						msg_print("The summoned ancient dragon gets angry!");
#endif
					}
				}
			}
		}
		break;
	}

	return "";
}


static cptr do_arcane_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
	bool spoil = (mode == SPELL_SPOIL_DESC) ? TRUE : FALSE;

	int dir;
	int plev = p_ptr->lev;

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "電撃";
		if (desc) return "電撃のボルトもしくはビームを放つ。";
#else
		if (name) return "Zap";
		if (desc) return "Fires a bolt or beam of lightning.";
#endif
    
		{
			int dice = spell_power(3 + (plev - 1) / 5);
			int sides = 3;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_bolt_or_beam(beam_chance() - 10, GF_ELEC, dir, damroll(dice, sides));
			}
		}
		break;

	case 1:
#ifdef JP
		if (name) return "魔法の施錠";
		if (desc) return "扉に鍵をかける。";
#else
		if (name) return "Wizard Lock";
		if (desc) return "Locks a door.";
#endif
    
		{
			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				wizard_lock(dir);
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "透明体感知";
		if (desc) return "近くの透明なモンスターを感知する。";
#else
		if (name) return "Detect Invisibility";
		if (desc) return "Detects all invisible monsters in your vicinity.";
#endif
    
		{
			int rad = DETECT_RAD_DEFAULT;

			if (info) return info_radius(rad);

			if (cast)
			{
				detect_monsters_invis(rad);
			}
		}
		break;

	case 3:
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

	case 4:
#ifdef JP
		if (name) return "ショート・テレポート";
		if (desc) return "近距離のテレポートをする。";
#else
		if (name) return "Blink";
		if (desc) return "Teleport short distance.";
#endif
    
		{
			int range = 10;

			if (info) return info_range(range);

			if (cast)
			{
				if (mut_present(MUT_ASTRAL_GUIDE))
					energy_use = 30;
				teleport_player(range, 0L);
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "ライト・エリア";
		if (desc) return "光源が照らしている範囲か部屋全体を永久に明るくする。";
#else
		if (name) return "Light Area";
		if (desc) return "Lights up nearby area and the inside of a room permanently.";
#endif
    
		{
			int dice = 2;
			int sides = spell_power(plev / 2);
			int rad = plev / 10 + 1;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				lite_area(damroll(dice, sides), rad);
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "罠と扉 破壊";
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

	case 7:
#ifdef JP
		if (name) return "軽傷の治癒";
		if (desc) return "怪我と体力を少し回復させる。";
#else
		if (name) return "Cure Light Wounds";
		if (desc) return "Heals cut and HP a little.";
#endif
    
		{
			int dice = 2;
			int sides = spell_power(8);

			if (info) return info_heal(dice, sides, 0);

			if (cast)
			{
				hp_player(damroll(dice, sides));
				set_cut(p_ptr->cut - 10, TRUE);
			}
		}
		break;

	case 8:
#ifdef JP
		if (name) return "罠と扉 感知";
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

	case 9:
#ifdef JP
		if (name) return "燃素";
		if (desc) return "光源に燃料を補給する。";
#else
		if (name) return "Phlogiston";
		if (desc) return "Adds more turns of light to a lantern or torch.";
#endif
    
		{
			if (cast)
			{
				phlogiston();
			}
		}
		break;

	case 10:
#ifdef JP
		if (name) return "財宝感知";
		if (desc) return "近くの財宝を感知する。";
#else
		if (name) return "Detect Treasure";
		if (desc) return "Detects all treasures in your vicinity.";
#endif
    
		{
			int rad = DETECT_RAD_DEFAULT;

			if (info) return info_radius(rad);

			if (cast)
			{
				detect_treasure(rad);
				detect_objects_gold(rad);
			}
		}
		break;

	case 11:
#ifdef JP
		if (name) return "魔法 感知";
		if (desc) return "近くの魔法がかかったアイテムを感知する。";
#else
		if (name) return "Detect Enchantment";
		if (desc) return "Detects all magical items in your vicinity.";
#endif
    
		{
			int rad = DETECT_RAD_DEFAULT;

			if (info) return info_radius(rad);

			if (cast)
			{
				detect_objects_magic(rad);
			}
		}
		break;

	case 12:
#ifdef JP
		if (name) return "アイテム感知";
		if (desc) return "近くの全てのアイテムを感知する。";
#else
		if (name) return "Detect Objects";
		if (desc) return "Detects all items in your vicinity.";
#endif
    
		{
			int rad = DETECT_RAD_DEFAULT;

			if (info) return info_radius(rad);

			if (cast)
			{
				detect_objects_normal(rad);
			}
		}
		break;

	case 13:
#ifdef JP
		if (name) return "解毒";
		if (desc) return "毒を体内から完全に取り除く。";
#else
		if (name) return "Cure Poison";
		if (desc) return "Cures poison status.";
#endif
    
		{
			if (cast)
			{
				set_poisoned(0, TRUE);
			}
		}
		break;

	case 14:
#ifdef JP
		if (name) return "耐冷";
		if (desc) return "一定時間、冷気への耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Resist Cold";
		if (desc) return "Gives resistance to cold. This resistance can be added to which from equipment for more powerful resistance.";
#endif
    
		{
			int base = spell_power(20);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_oppose_cold(randint1(base) + base, FALSE);
			}
		}
		break;

	case 15:
#ifdef JP
		if (name) return "耐火";
		if (desc) return "一定時間、炎への耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Resist Fire";
		if (desc) return "Gives resistance to fire. This resistance can be added to which from equipment for more powerful resistance.";
#endif
    
		{
			int base = spell_power(20);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_oppose_fire(randint1(base) + base, FALSE);
			}
		}
		break;

	case 16:
#ifdef JP
		if (name) return "耐電";
		if (desc) return "一定時間、電撃への耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Resist Lightning";
		if (desc) return "Gives resistance to electricity. This resistance can be added to which from equipment for more powerful resistance.";
#endif
    
		{
			int base = spell_power(20);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_oppose_elec(randint1(base) + base, FALSE);
			}
		}
		break;

	case 17:
#ifdef JP
		if (name) return "耐酸";
		if (desc) return "一定時間、酸への耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Resist Acid";
		if (desc) return "Gives resistance to acid. This resistance can be added to which from equipment for more powerful resistance.";
#endif
    
		{
			int base = spell_power(20);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_oppose_acid(randint1(base) + base, FALSE);
			}
		}
		break;

	case 18:
#ifdef JP
		if (name) return "重傷の治癒";
		if (desc) return "怪我と体力を中程度回復させる。";
#else
		if (name) return "Cure Medium Wounds";
		if (desc) return "Heals cut and HP more.";
#endif
    
		{
			int dice = 4;
			int sides = spell_power(8);

			if (info) return info_heal(dice, sides, 0);

			if (cast)
			{
				hp_player(damroll(dice, sides));
				set_cut((p_ptr->cut / 2) - 50, TRUE);
			}
		}
		break;

	case 19:
#ifdef JP
		if (name) return "テレポート";
		if (desc) return "遠距離のテレポートをする。";
#else
		if (name) return "Teleport";
		if (desc) return "Teleport long distance.";
#endif
    
		{
			int range = plev * 5;

			if (info) return info_range(range);

			if (cast)
			{
				if (mut_present(MUT_ASTRAL_GUIDE))
					energy_use = 30;
				teleport_player(range, 0L);
			}
		}
		break;

	case 20:
#ifdef JP
		if (name) return "鑑定";
		if (desc) return "アイテムを識別する。";
#else
		if (name) return "Identify";
		if (desc) return "Identifies an item.";
#endif
    
		{
			if (cast)
			{
				if (!ident_spell(NULL)) return NULL;
			}
		}
		break;

	case 21:
#ifdef JP
		if (name) return "岩石溶解";
		if (desc) return "壁を溶かして床にする。";
#else
		if (name) return "Stone to Mud";
		if (desc) return "Turns one rock square to mud.";
#endif
    
		{
			int dice = 1;
			int sides = 30;
			int base = 20;

			if (info) return info_damage(dice, sides, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				wall_to_mud(dir);
			}
		}
		break;

	case 22:
#ifdef JP
		if (name) return "閃光";
		if (desc) return "光線を放つ。光りを嫌うモンスターに効果がある。";
#else
		if (name) return "Ray of Light";
		if (desc) return "Fires a beam of light which damages to light-sensitive monsters.";
#endif
    
		{
			int dice = 6;
			int sides = 8;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

#ifdef JP
				msg_print("光線が放たれた。");
#else
				msg_print("A line of light appears.");
#endif

				lite_line(dir);
			}
		}
		break;

	case 23:
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

	case 24:
#ifdef JP
		if (name) return "透明視認";
		if (desc) return "一定時間、透明なものが見えるようになる。";
#else
		if (name) return "See Invisible";
		if (desc) return "Gives see invisible for a while.";
#endif
    
		{
			int base = spell_power(24);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_tim_invis(randint1(base) + base, FALSE);
			}
		}
		break;

	case 25:
#ifdef JP
		if (name) return "耐毒";
		if (desc) return "一定時間、毒への耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Resist Poison";
		if (desc) return "Gives resistance to poison. This resistance can be added to which from equipment for more powerful resistance.";
#endif
    
		{
			int base = spell_power(20);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_oppose_pois(randint1(base) + base, FALSE);
			}
		}
		break;

	case 26:
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

	case 27:
#ifdef JP
		if (name) return "テレポート・モンスター";
		if (desc) return "モンスターをテレポートさせるビームを放つ。抵抗されると無効。";
#else
		if (name) return "Teleport Away";
		if (desc) return "Teleports all monsters on the line away unless resisted.";
#endif
    
		{
			int power = spell_power(plev);

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_beam(GF_AWAY_ALL, dir, power);
			}
		}
		break;

	case 28:
		if (name) return T("Recharging", "魔力充填");
		if (desc) return T("Recharges staves, wands or rods.", "杖/魔法棒の充填回数を増やすか、充填中のロッドの充填時間を減らす。");

		{
			int power = spell_power(plev * 3 / 2);

			if (info) return info_power(power);

			if (cast)
			{
				if (!recharge(power)) return NULL;
			}
		}
		break;

	case 29:
#ifdef JP
		if (name) return "全感知";
		if (desc) return "近くの全てのモンスター、罠、扉、階段、財宝、そしてアイテムを感知する。";
#else
		if (name) return "Detection";
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

	case 30:
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

	case 31:
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
				chg_virtue(V_KNOWLEDGE, 1);
				chg_virtue(V_ENLIGHTEN, 1);

				wiz_lite(p_ptr->tim_superstealth > 0);

				if (!p_ptr->telepathy)
				{
					set_tim_esp(randint1(sides) + base, FALSE);
				}
			}
		}
		break;
	}

	return "";
}


static cptr do_craft_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
	bool spoil = (mode == SPELL_SPOIL_DESC) ? TRUE : FALSE;

	int plev = p_ptr->lev;

	switch (spell)
	{
	case 0:
		if (name) return "Minor Enchantment";
		if (desc) return "Attempts to increase +to-hit, +to-dam of a weapon, or to increase +AC of armor.";
    
		if (cast)
		{
			int         item;
			bool        okay = FALSE;
			object_type *o_ptr;
			char        o_name[MAX_NLEN];

			item_tester_hook = object_is_weapon_armour_ammo;
			item_tester_no_ryoute = TRUE;

			if (!get_item(&item, "Enchant which item? ", "You have nothing to enchant.", (USE_EQUIP | USE_INVEN | USE_FLOOR))) return NULL;

			if (item >= 0)
				o_ptr = &inventory[item];
			else
				o_ptr = &o_list[0 - item];

			object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));

			if (object_is_weapon_ammo(o_ptr))
			{
				if (one_in_(2))
				{
					if (enchant(o_ptr, 1, ENCH_TOHIT | ENCH_MINOR_HACK)) okay = TRUE;
				}
				else
				{
					if (enchant(o_ptr, 1, ENCH_TODAM | ENCH_MINOR_HACK)) okay = TRUE;
				}
			}
			else
			{
				if (enchant(o_ptr, 1, ENCH_TOAC | ENCH_MINOR_HACK)) okay = TRUE;			
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
			{
				o_ptr->discount = 99;
				chg_virtue(V_ENCHANT, 1);
			}

			calc_android_exp();
		}
		break;

	case 1:
#ifdef JP
		if (name) return "回復力強化";
		if (desc) return "一定時間、回復力が増強される。";
#else
		if (name) return "Regeneration";
		if (desc) return "Gives regeneration ability for a while.";
#endif
    
		{
			int base = spell_power(80);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_tim_regen(base + randint1(base), FALSE);
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "空腹充足";
		if (desc) return "満腹になる。";
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

	case 3:
#ifdef JP
		if (name) return "耐冷気";
		if (desc) return "一定時間、冷気への耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Resist Cold";
		if (desc) return "Gives resistance to cold. This resistance can be added to which from equipment for more powerful resistance.";
#endif
    
		{
			int base = spell_power(20);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_oppose_cold(randint1(base) + base, FALSE);
			}
		}
		break;

	case 4:
#ifdef JP
		if (name) return "耐火炎";
		if (desc) return "一定時間、炎への耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Resist Fire";
		if (desc) return "Gives resistance to fire. This resistance can be added to which from equipment for more powerful resistance.";
#endif
    
		{
			int base = spell_power(20);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_oppose_fire(randint1(base) + base, FALSE);
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "士気高揚";
		if (desc) return "一定時間、ヒーロー気分になる。";
#else
		if (name) return "Heroism";
		if (desc) return "Removes fear, and gives bonus to hit and 10 more HP for a while.";
#endif
    
		{
			int base = spell_power(25);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_hero(randint1(base) + base, FALSE);
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "耐電撃";
		if (desc) return "一定時間、電撃への耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Resist Lightning";
		if (desc) return "Gives resistance to electricity. This resistance can be added to which from equipment for more powerful resistance.";
#endif
    
		{
			int base = spell_power(20);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_oppose_elec(randint1(base) + base, FALSE);
			}
		}
		break;

	case 7:
#ifdef JP
		if (name) return "耐酸";
		if (desc) return "一定時間、酸への耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Resist Acid";
		if (desc) return "Gives resistance to acid. This resistance can be added to which from equipment for more powerful resistance.";
#endif
    
		{
			int base = spell_power(20);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_oppose_acid(randint1(base) + base, FALSE);
			}
		}
		break;

	case 8:
#ifdef JP
		if (name) return "透明視認";
		if (desc) return "一定時間、透明なものが見えるようになる。";
#else
		if (name) return "See Invisibility";
		if (desc) return "Gives see invisible for a while.";
#endif
    
		{
			int base = spell_power(24);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_tim_invis(randint1(base) + base, FALSE);
			}
		}
		break;

	case 9:
		if (name) return "Elemental Cloak";
		if (desc) return "You gain protective elemental auras for a short time.";
		{
			int base = spell_power(10 + plev / 2);

			if (info) return info_duration(base, base);

			if (cast)
				set_tim_sh_elements(randint1(base) + base, FALSE);
		}
		break;

	case 10:
#ifdef JP
		if (name) return "耐毒";
		if (desc) return "一定時間、毒への耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Resist Poison";
		if (desc) return "Gives resistance to poison. This resistance can be added to which from equipment for more powerful resistance.";
#endif
    
		{
			int base = spell_power(20);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_oppose_pois(randint1(base) + base, FALSE);
			}
		}
		break;

	case 11:
		if (name) return T("Berserk", "狂戦士化");
		if (desc) return T("Gives bonus to hit and HP, immunity to fear for a while. But decreases AC.", "狂戦士化し、恐怖を除去する。");

		{
			int base = spell_power(25);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_shero(randint1(base) + base, FALSE);
				hp_player(30);
			}
		}
		break;

	case 12:
#ifdef JP
		if (name) return "自己分析";
		if (desc) return "現在の自分の状態を完全に知る。";
#else
		if (name) return "Self Knowledge";
		if (desc) return "Gives you useful info regarding your current resistances, the powers of your weapon and maximum limits of your stats.";
#endif
    
		{
			if (cast)
			{
				self_knowledge();
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
			int base = spell_power(3 * plev);
			int sides = spell_power(25);

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_protevil(randint1(sides) + base, FALSE);
			}
		}
		break;

	case 14:
		if (name) return "Giant Strength";
		if (desc) return "For a short time, you grow to a gigantic height and gain great powers of combat.";
		{
			int base = spell_power(5 + plev / 10);

			if (info) return info_duration(base, base);

			if (cast)
				set_tim_building_up(randint1(base) + base, FALSE);
		}
		break;

	case 15:
#ifdef JP
		if (name) return "魔法剣";
		if (desc) return "一定時間、武器に冷気、炎、電撃、酸、毒のいずれかの属性をつける。武器を持たないと使えない。";
#else
		if (name) return "Mana Branding";
		if (desc) return "Makes current weapon some elemental branded. You must wield weapons.";
#endif
    
		{
			int base = plev / 2;

			if (info) return info_duration(base, base);

			if (cast)
			{
				if (!choose_ele_attack()) return NULL;
			}
		}
		break;

	case 16:
#ifdef JP
		if (name) return "テレパシー";
		if (desc) return "一定時間、テレパシー能力を得る。";
#else
		if (name) return "Telepathy";
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

	case 17:
#ifdef JP
		if (name) return "肌石化";
		if (desc) return "一定時間、ACを上昇させる。";
#else
		if (name) return "Stone Skin";
		if (desc) return "Gives bonus to AC for a while.";
#endif
    
		{
			int base = 30;
			int sides = 20;

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_shield(randint1(sides) + base, FALSE);
			}
		}
		break;

	case 18:
#ifdef JP
		if (name) return "全耐性";
		if (desc) return "一定時間、酸、電撃、炎、冷気、毒に対する耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Resistance";
		if (desc) return "Gives resistance to fire, cold, electricity, acid and poison for a while. These resistances can be added to which from equipment for more powerful resistances.";
#endif
    
		{
			int base = spell_power(20);

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

	case 19:
#ifdef JP
		if (name) return "スピード";
		if (desc) return "一定時間、加速する。";
#else
		if (name) return "Haste Self";
		if (desc) return "Hastes you for a while.";
#endif
    
		{
			int base = spell_power(plev);
			int sides = spell_power(20 + plev);

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_fast(randint1(sides) + base, FALSE);
			}
		}
		break;

	case 20:
		if (name) return "Whirlwind Attack";
		if (desc) return "Attacks all adjacent monsters.";
    
		{
			if (cast)
			{
				int y = 0, x = 0;
				cave_type       *c_ptr;
				monster_type    *m_ptr;
				int dir;

				for (dir = 0; dir < 8; dir++)
				{
					y = py + ddy_ddd[dir];
					x = px + ddx_ddd[dir];
					c_ptr = &cave[y][x];
					m_ptr = &m_list[c_ptr->m_idx];
					if (c_ptr->m_idx && (m_ptr->ml || cave_have_flag_bold(y, x, FF_PROJECT)))
						py_attack(y, x, 0);
				}
			}
		}
		break;

	case 21:
		if (name) return T("Recharging", "魔力充填");
		if (desc) return T("Recharges staves, wands or rods.", "杖/魔法棒の充填回数を増やすか、充填中のロッドの充填時間を減らす。");

		{
			int power = spell_power(plev * 3);

			if (info) return info_power(power);

			if (cast)
			{
				if (!recharge(power)) return NULL;
			}
		}
		break;

	case 22:
		if (name) return "Weaponmastery";
		if (desc) return "For a short time, your melee weapon becomes more deadly.";
		{
			int base = spell_power(3 + plev / 10);

			if (info) return info_duration(base, base);

			if (cast)
				set_tim_weaponmastery(randint1(base) + base, FALSE);
		}
		break;

	case 23:
#ifdef JP
		if (name) return "魔法の鎧";
		if (desc) return "一定時間、魔法防御力とACが上がり、混乱と盲目の耐性、反射能力、麻痺知らず、浮遊を得る。";
#else
		if (name) return "Magical armor";
		if (desc) return "Gives resistance to magic, bonus to AC, resistance to confusion, blindness, reflection, free action and levitation for a while.";
#endif
    
		{
			int base = spell_power(20);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_magicdef(randint1(base) + base, FALSE);
			}
		}
		break;

	case 24:
#ifdef JP
		if (name) return "呪い粉砕";
		if (desc) return "アイテムにかかった強力な呪いを解除する。";
#else
		if (name) return "Remove All Curse";
		if (desc) return "Removes normal and heavy curse from equipped items.";
#endif
    
		{
			if (cast)
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
		break;

	case 25:
		if (name) return "Walk through Wall";
		if (desc) return "Gives ability to pass walls for a while.";
    
		{
			int base = spell_power(plev / 3);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_kabenuke(randint1(base) + base, FALSE);
			}
		}
		break;

	case 26:
#ifdef JP
		if (name) return "完全なる知識";
		if (desc) return "アイテムの持つ能力を完全に知る。";
#else
		if (name) return "Knowledge True";
		if (desc) return "*Identifies* an item.";
#endif
    
		{
			if (cast)
			{
				if (!identify_fully(NULL)) return NULL;
			}
		}
		break;

	case 27:
#ifdef JP
		if (name) return "武器強化";
		if (desc) return "武器の命中率修正とダメージ修正を強化する。";
#else
		if (name) return "Enchantment";
		if (desc) return "Attempts to increase +to-hit, +to-dam of a weapon, or to increase +AC of armor.";
#endif
    
		{
			if (cast)
			{
				if (!cast_enchantment()) return NULL;
			}
		}
		break;

	case 28:
#ifdef JP
		if (name) return "武器属性付与";
		if (desc) return "武器にランダムに属性をつける。";
#else
		if (name) return "Brand Weapon";
		if (desc) return "Makes current weapon a random ego weapon.";
#endif
    
		if (cast) brand_weapon(-1);
		break;

	case 29:
#ifdef JP
		if (name) return "人間トランプ";
		if (desc) return "ランダムにテレポートする突然変異か、自分の意思でテレポートする突然変異が身につく。";
#else
		if (name) return "Living Trump";
		if (desc) return "Gives mutation which makes you teleport randomly or makes you able to teleport at will.";
#endif
    
		if (cast)
		{
			int mutation;

			if (one_in_(7) || dun_level == 0)
				mutation = MUT_TELEPORT;
			else
				mutation = MUT_TELEPORT_RND;

			if (mut_gain(mutation))
				msg_print(T("You have turned into a Living Trump.", "あなたは生きているカードに変わった。"));
		}
		break;

	case 30:
#ifdef JP
		if (name) return "属性への免疫";
		if (desc) return "一定時間、冷気、炎、電撃、酸のいずれかに対する免疫を得る。";
#else
		if (name) return "Immunity";
		if (desc) return "Gives an immunity to fire, cold, electricity or acid for a while.";
#endif
    
		{
			int base = spell_power(13);

			if (info) return info_duration(base, base);

			if (cast)
			{
				if (!choose_ele_immune(base + randint1(base))) return NULL;
			}
		}
		break;

	case 31:
		if (name) return "Force Branding";
		if (desc) return "Temporarily brands your weapon with force.";
    
		{
		int base = spell_power(plev / 4);

			if (info) return info_duration(base, base);
			if (cast)
			{
				set_tim_force(base + randint1(base), FALSE);
			}
		}
		break;
	}

	return "";
}


static cptr do_daemon_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
	bool spoil = (mode == SPELL_SPOIL_DESC) ? TRUE : FALSE;

#ifdef JP
	static const char s_dam[] = "損傷:";
#else
	static const char s_dam[] = "dam ";
#endif

	int dir;
	int plev = p_ptr->lev;

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "マジック・ミサイル";
		if (desc) return "弱い魔法の矢を放つ。";
#else
		if (name) return "Magic Missile";
		if (desc) return "Fires a weak bolt of magic.";
#endif
    
		{
			int dice = 3 + (plev - 1) / 5;
			int sides = 4;

			if (info) return info_damage(spell_power(dice), sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_bolt_or_beam(beam_chance() - 10, GF_MISSILE, dir, spell_power(damroll(dice, sides)));
			}
		}
		break;

	case 1:
#ifdef JP
		if (name) return "無生命感知";
		if (desc) return "近くの生命のないモンスターを感知する。";
#else
		if (name) return "Detect Unlife";
		if (desc) return "Detects all nonliving monsters in your vicinity.";
#endif
    
		{
			int rad = DETECT_RAD_DEFAULT;

			if (info) return info_radius(rad);

			if (cast)
			{
				detect_monsters_nonliving(rad);
			}
		}
		break;

	case 2:
#ifdef JP
		if (name) return "邪なる祝福";
		if (desc) return "一定時間、命中率とACにボーナスを得る。";
#else
		if (name) return "Evil Bless";
		if (desc) return "Gives bonus to hit and AC for a few turns.";
#endif
    
		{
			int base = spell_power(12);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_blessed(randint1(base) + base, FALSE);
			}
		}
		break;

	case 3:
#ifdef JP
		if (name) return "耐火炎";
		if (desc) return "一定時間、炎への耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Resist Fire";
		if (desc) return "Gives resistance to fire for a while. This resistance can be added to which from equipment for more powerful resistances.";
#endif
    
		{
			int base = spell_power(20);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_oppose_fire(randint1(base) + base, FALSE);
			}
		}
		break;

	case 4:
#ifdef JP
		if (name) return "恐慌";
		if (desc) return "モンスター1体を恐怖させ、朦朧させる。抵抗されると無効。";
#else
		if (name) return "Horrify";
		if (desc) return "Attempts to scare and stun a monster.";
#endif
    
		{
			int power = spell_power(plev * 2);

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fear_monster(dir, power);
				stun_monster(dir, power);
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "地獄の矢";
		if (desc) return "地獄のボルトもしくはビームを放つ。";
#else
		if (name) return "Nether Bolt";
		if (desc) return "Fires a bolt or beam of nether.";
#endif
    
		{
			int dice = 6 + (plev - 5) / 4;
			int sides = 8;

			if (info) return info_damage(spell_power(dice), sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_bolt_or_beam(beam_chance(), GF_NETHER, dir, spell_power(damroll(dice, sides)));
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "古代の死霊召喚";
		if (desc) return "古代の死霊を召喚する。";
#else
		if (name) return "Summon Manes";
		if (desc) return "Summons a manes.";
#endif
    
		{
			if (cast)
			{
				if (!summon_specific(-1, py, px, spell_power(plev * 3 / 2), SUMMON_MANES, (PM_ALLOW_GROUP | PM_FORCE_PET)))
				{
#ifdef JP
					msg_print("古代の死霊は現れなかった。");
#else
					msg_print("No Manes arrive.");
#endif
				}
			}
		}
		break;

	case 7:
#ifdef JP
		if (name) return "地獄の焔";
		if (desc) return "邪悪な力を持つボールを放つ。善良なモンスターには大きなダメージを与える。";
#else
		if (name) return "Hellish Flame";
		if (desc) return "Fires a ball of evil power. Hurts good monsters greatly.";
#endif
    
		{
			int dice = 3;
			int sides = 6;
			int rad = (plev < 30) ? 2 : 3;
			int base;

			if (p_ptr->pclass == CLASS_MAGE ||
			    p_ptr->pclass == CLASS_BLOOD_MAGE ||
			    p_ptr->pclass == CLASS_HIGH_MAGE ||
			    p_ptr->pclass == CLASS_SORCERER)
				base = plev + plev / 2;
			else
				base = plev + plev / 4;


			if (info) return info_damage(dice, spell_power(sides), spell_power(base));

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_HELL_FIRE, dir, spell_power(damroll(dice, sides) + base), rad);
			}
		}
		break;

	case 8:
#ifdef JP
		if (name) return "デーモン支配";
		if (desc) return "悪魔1体を魅了する。抵抗されると無効";
#else
		if (name) return "Dominate Demon";
		if (desc) return "Attempts to charm a demon.";
#endif
    
		{
			int power = spell_power(plev * 2);

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				control_one_demon(dir, power);
			}
		}
		break;

	case 9:
#ifdef JP
		if (name) return "ビジョン";
		if (desc) return "周辺の地形を感知する。";
#else
		if (name) return "Vision";
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
		if (name) return "耐地獄";
		if (desc) return "一定時間、地獄への耐性を得る。";
#else
		if (name) return "Resist Nether";
		if (desc) return "Gives resistance to nether for a while.";
#endif
    
		{
			int base = spell_power(20);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_tim_res_nether(randint1(base) + base, FALSE);
			}
		}
		break;

	case 11:
#ifdef JP
		if (name) return "プラズマ・ボルト";
		if (desc) return "プラズマのボルトもしくはビームを放つ。";
#else
		if (name) return "Plasma bolt";
		if (desc) return "Fires a bolt or beam of plasma.";
#endif
    
		{
			int dice = 11 + (plev - 5) / 4;
			int sides = 8;

			if (info) return info_damage(spell_power(dice), sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_bolt_or_beam(beam_chance(), GF_PLASMA, dir, spell_power(damroll(dice, sides)));
			}
		}
		break;

	case 12:
#ifdef JP
		if (name) return "ファイア・ボール";
		if (desc) return "炎の球を放つ。";
#else
		if (name) return "Fire Ball";
		if (desc) return "Fires a ball of fire.";
#endif
    
		{
			int dam = spell_power(plev + 55);
			int rad = 2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_FIRE, dir, dam, rad);
			}
		}
		break;

	case 13:
#ifdef JP
		if (name) return "炎の刃";
		if (desc) return "武器に炎の属性をつける。";
#else
		if (name) return "Fire Branding";
		if (desc) return "Makes current weapon fire branded.";
#endif
    
		{
			if (cast)
			{
				brand_weapon(EGO_BRAND_FIRE);
			}
		}
		break;

	case 14:
#ifdef JP
		if (name) return "地獄球";
		if (desc) return "大きな地獄の球を放つ。";
#else
		if (name) return "Nether Ball";
		if (desc) return "Fires a huge ball of nether.";
#endif
    
		{
			int dam = spell_power(plev * 3 / 2 + 100);
			int rad = spell_power(plev / 20 + 2);

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_NETHER, dir, dam, rad);
			}
		}
		break;

	case 15:
#ifdef JP
		if (name) return "デーモン召喚";
		if (desc) return "悪魔1体を召喚する。";
#else
		if (name) return "Summon Demon";
		if (desc) return "Summons a demon.";
#endif
    
		{
			if (cast)
			{
				bool pet = !one_in_(3);
				u32b mode = 0L;

				if (pet) mode |= PM_FORCE_PET;
				else mode |= PM_NO_PET;
				if (!(pet && (plev < 50))) mode |= PM_ALLOW_GROUP;

				if (summon_specific((pet ? -1 : 0), py, px, spell_power(plev*2/3+randint1(plev/2)), SUMMON_DEMON, mode))
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
				else
				{
#ifdef JP
					msg_print("悪魔は現れなかった。");
#else
					msg_print("No demons arrive.");
#endif
				}
				break;
			}
		}
		break;

	case 16:
#ifdef JP
		if (name) return "悪魔の目";
		if (desc) return "一定時間、テレパシー能力を得る。";
#else
		if (name) return "Devilish Eye";
		if (desc) return "Gives telepathy for a while.";
#endif
    
		{
			int base = spell_power(30);
			int sides = 25;

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_tim_esp(randint1(base) + sides, FALSE);
			}
		}
		break;

	case 17:
#ifdef JP
		if (name) return "悪魔のクローク";
		if (desc) return "恐怖を取り除き、一定時間、炎と冷気の耐性、炎のオーラを得る。耐性は装備による耐性に累積する。";
#else
		if (name) return "Devilish Cloak";
		if (desc) return "Gives resistance to fire, acid and poison as well as an aura of fire. These resistances can be added to which from equipment for more powerful resistances.";
#endif
    
		{
			int base = spell_power(20);

			if (info) return info_duration(base, base);

			if (cast)
			{
				int dur = randint1(base) + base;
					
				set_oppose_fire(dur, FALSE);
				set_oppose_acid(dur, FALSE);
				set_oppose_pois(dur, FALSE);
				set_tim_sh_fire(dur, FALSE);
				break;
			}
		}
		break;

	case 18:
#ifdef JP
		if (name) return "溶岩流";
		if (desc) return "自分を中心とした炎の球を作り出し、床を溶岩に変える。";
#else
		if (name) return "The Flow of Lava";
		if (desc) return "Generates a ball of fire centered on you which transforms floors to magma.";
#endif
    
		{
			int dam = spell_power((55 + plev) * 2);
			int rad = 3;

			if (info) return info_damage(0, 0, dam/2);

			if (cast)
			{
				fire_ball(GF_FIRE, 0, dam, rad);
				fire_ball_hide(GF_LAVA_FLOW, 0, 2 + randint1(2), rad);
			}
		}
		break;

	case 19:
#ifdef JP
		if (name) return "プラズマ球";
		if (desc) return "プラズマの球を放つ。";
#else
		if (name) return "Plasma Ball";
		if (desc) return "Fires a ball of plasma.";
#endif
    
		{
			int dam = spell_power(plev * 3 / 2 + 80);
			int rad = spell_power(2 + plev / 40);

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_PLASMA, dir, dam, rad);
			}
		}
		break;

	case 20:
#ifdef JP
		if (name) return "悪魔変化";
		if (desc) return "一定時間、悪魔に変化する。変化している間は本来の種族の能力を失い、代わりに悪魔としての能力を得る。";
#else
		if (name) return "Polymorph Demon";
		if (desc) return "Mimic a demon for a while. Loses abilities of original race and gets abilities as a demon.";
#endif
    
		{
			int base = spell_power(10 + plev / 2);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_mimic(base + randint1(base), MIMIC_DEMON, FALSE);
			}
		}
		break;

	case 21:
#ifdef JP
		if (name) return "地獄の波動";
		if (desc) return "視界内の全てのモンスターにダメージを与える。善良なモンスターに特に大きなダメージを与える。";
#else
		if (name) return "Nether Wave";
		if (desc) return "Damages all monsters in sight. Hurts good monsters greatly.";
#endif
    
		{
			int sides1 = spell_power(plev * 2);
			int sides2 = spell_power(plev * 2);

			if (info) return format("%sd%d+d%d", s_dam, sides1, sides2);

			if (cast)
			{
				dispel_monsters(randint1(sides1));
				dispel_good(randint1(sides2));
			}
		}
		break;

	case 22:
#ifdef JP
		if (name) return "サキュバスの接吻";
		if (desc) return "因果混乱の球を放つ。";
#else
		if (name) return "Kiss of Succubus";
		if (desc) return "Fires a ball of nexus.";
#endif
    
		{
			int dam = spell_power(100 + plev * 2);
			int rad = 4;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_NEXUS, dir, dam, rad);
			}
		}
		break;

	case 23:
#ifdef JP
		if (name) return "破滅の手";
		if (desc) return "破滅の手を放つ。食らったモンスターはそのときのHPの半分前後のダメージを受ける。";
#else
		if (name) return "Doom Hand";
		if (desc) return "Attempts to make a monster's HP almost half.";
#endif
    
		{
			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
#ifdef JP
				else msg_print("<破滅の手>を放った！");
#else
				else msg_print("You invoke the Hand of Doom!");
#endif

				fire_ball_hide(GF_HAND_DOOM, dir, spell_power(plev * 5 / 2), 0);
			}
		}
		break;

	case 24:
#ifdef JP
		if (name) return "士気高揚";
		if (desc) return "一定時間、ヒーロー気分になる。";
#else
		if (name) return "Raise the Morale";
		if (desc) return "Gives bonus to hit and 10 more HP for a while.";
#endif
    
		{
			int base = spell_power(25);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_hero(randint1(base) + base, FALSE);
			}
		}
		break;

	case 25:
#ifdef JP
		if (name) return "不滅の肉体";
		if (desc) return "一定時間、時間逆転への耐性を得る。";
#else
		if (name) return "Immortal Body";
		if (desc) return "Gives resistance to time for a while.";
#endif
    
		{
			int base = spell_power(20);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_tim_res_time(randint1(base)+base, FALSE);
			}
		}
		break;

	case 26:
#ifdef JP
		if (name) return "狂気の円環";
		if (desc) return "自分を中心としたカオスの球、混乱の球を発生させ、近くのモンスターを魅了する。";
#else
		if (name) return "Insanity Circle";
		if (desc) return "Generate balls of chaos, confusion and charm centered on you.";
#endif
    
		{
			int dam = spell_power(50 + plev);
			int power = spell_power(20 + plev);
			int rad = spell_power(3 + plev / 20);

			if (info) return format("%s%d+%d", s_dam, dam/2, dam/2);

			if (cast)
			{
				fire_ball(GF_CHAOS, 0, dam, rad);
				fire_ball(GF_CONFUSION, 0, dam, rad);
				fire_ball(GF_CHARM, 0, power, rad);
			}
		}
		break;

	case 27:
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

	case 28:
#ifdef JP
		if (name) return "グレーターデーモン召喚";
		if (desc) return "上級デーモンを召喚する。召喚するには人間('p','h','t'で表されるモンスター)の死体を捧げなければならない。";
#else
		if (name) return "Summon Greater Demon";
		if (desc) return "Summons greater demon. It need to sacrifice a corpse of human ('p','h' or 't').";
#endif
    
		{
			if (cast)
			{
				if (!cast_summon_greater_demon()) return NULL;
			}
		}
		break;

	case 29:
#ifdef JP
		if (name) return "地獄嵐";
		if (desc) return "超巨大な地獄の球を放つ。";
#else
		if (name) return "Nether Storm";
		if (desc) return "Generate a huge ball of nether.";
#endif
    
		{
			int dam = spell_power(plev * 15);
			int rad = spell_power(plev / 5);

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_NETHER, dir, dam, rad);
			}
		}
		break;

	case 30:
#ifdef JP
		if (name) return "血の呪い";
		if (desc) return "自分がダメージを受けることによって対象に呪いをかけ、ダメージを与え様々な効果を引き起こす。";
#else
		if (name) return "Bloody Curse";
		if (desc) return "Puts blood curse which damages and causes various effects on a monster. You also take damage.";
#endif
    
		{
			int dam = spell_power(500);
			int rad = 0;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball_hide(GF_BLOOD_CURSE, dir, dam, rad);
#ifdef JP
				take_hit(DAMAGE_USELIFE, 20 + randint1(30), "血の呪い", -1);
#else
				take_hit(DAMAGE_USELIFE, 20 + randint1(30), "Blood curse", -1);
#endif
			}
		}
		break;

	case 31:
#ifdef JP
		if (name) return "魔王変化";
		if (desc) return "悪魔の王に変化する。変化している間は本来の種族の能力を失い、代わりに悪魔の王としての能力を得、壁を破壊しながら歩く。";
#else
		if (name) return "Polymorph Demonlord";
		if (desc) return "Mimic a demon lord for a while. Loses abilities of original race and gets great abilities as a demon lord. Even hard walls can't stop your walking.";
#endif
    
		{
			int base = spell_power(15);

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_mimic(base + randint1(base), MIMIC_DEMON_LORD, FALSE);
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
	bool spoil = (mode == SPELL_SPOIL_DESC) ? TRUE : FALSE;

	int dir;
	int plev = p_ptr->lev;

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "懲罰";
		if (desc) return "電撃のボルトもしくはビームを放つ。";
#else
		if (name) return "Punishment";
		if (desc) return "Fires a bolt or beam of lightning.";
#endif
    
		{
			int dice = spell_power(3 + (plev - 1) / 5);
			int sides = 4;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_bolt_or_beam(beam_chance() - 10, GF_ELEC, dir, damroll(dice, sides));
			}
		}
		break;

	case 1:
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

	case 2:
#ifdef JP
		if (name) return "恐怖除去";
		if (desc) return "恐怖を取り除く。";
#else
		if (name) return "Remove Fear";
		if (desc) return "Removes fear.";
#endif
    
		if (cast)
			fear_clear_p();
		break;

	case 3:
#ifdef JP
		if (name) return "威圧";
		if (desc) return "モンスター1体を恐怖させる。抵抗されると無効。";
#else
		if (name) return "Scare Monster";
		if (desc) return "Attempts to scare a monster.";
#endif
    
		{
			int power = spell_power(plev);

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fear_monster(dir, power);
			}
		}
		break;

	case 4:
#ifdef JP
		if (name) return "聖域";
		if (desc) return "隣接した全てのモンスターを眠らせる。抵抗されると無効。";
#else
		if (name) return "Sanctuary";
		if (desc) return "Attempts to sleep monsters in the adjacent squares.";
#endif
    
		{
			int power = plev;

			if (info) return info_power(power);

			if (cast)
			{
				sleep_monsters_touch();
			}
		}
		break;

	case 5:
#ifdef JP
		if (name) return "入口";
		if (desc) return "中距離のテレポートをする。";
#else
		if (name) return "Portal";
		if (desc) return "Teleport medium distance.";
#endif
    
		{
			int range = 25 + plev / 2;

			if (info) return info_range(range);

			if (cast)
			{
				if (mut_present(MUT_ASTRAL_GUIDE))
					energy_use = 30;
				teleport_player(range, 0L);
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "スターダスト";
		if (desc) return "ターゲット付近に閃光のボルトを連射する。";
#else
		if (name) return "Star Dust";
		if (desc) return "Fires many bolts of light near the target.";
#endif
    
		{
			int dice = spell_power(3 + (plev - 1) / 9);
			int sides = 2;

			if (info) return info_multi_damage_dice(dice, sides);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_blast(GF_LITE, dir, dice, sides, 10, 3);
			}
		}
		break;

	case 7:
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
				set_cut(0, TRUE);
				set_poisoned(0, TRUE);
				set_stun(0, TRUE);
			}
		}
		break;

	case 8:
#ifdef JP
		if (name) return "邪悪飛ばし";
		if (desc) return "邪悪なモンスター1体をテレポートさせる。抵抗されると無効。";
#else
		if (name) return "Scatter Evil";
		if (desc) return "Attempts to teleport an evil monster away.";
#endif
    
		{
			int power = MAX_SIGHT * 5;

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_AWAY_EVIL, dir, power, 0);
			}
		}
		break;

	case 9:
#ifdef JP
		if (name) return "聖なる光球";
		if (desc) return "聖なる力をもつ宝珠を放つ。邪悪なモンスターに対して大きなダメージを与えるが、善良なモンスターには効果がない。";
#else
		if (name) return "Holy Orb";
		if (desc) return "Fires a ball with holy power. Hurts evil monsters greatly, but don't effect good monsters.";
#endif
    
		{
			int dice = 3;
			int sides = spell_power(6);
			int rad = (plev < 30) ? 2 : 3;
			int base;

			if (p_ptr->pclass == CLASS_PRIEST ||
			    p_ptr->pclass == CLASS_HIGH_MAGE ||
			    p_ptr->pclass == CLASS_SORCERER)
				base = spell_power(plev + plev / 2);
			else
				base = spell_power(plev + plev / 4);


			if (info) return info_damage(dice, sides, base);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_HOLY_FIRE, dir, damroll(dice, sides) + base, rad);
			}
		}
		break;

	case 10:
#ifdef JP
		if (name) return "悪魔払い";
		if (desc) return "視界内の全てのアンデッド及び悪魔にダメージを与え、邪悪なモンスターを恐怖させる。";
#else
		if (name) return "Exorcism";
		if (desc) return "Damages all undead and demons in sight, and scares all evil monsters in sight.";
#endif
    
		{
			int sides = spell_power(plev);
			int power = spell_power(plev);

			if (info) return info_damage(1, sides, 0);

			if (cast)
			{
				dispel_undead(randint1(sides));
				dispel_demons(randint1(sides));
				turn_evil(power);
			}
		}
		break;

	case 11:
#ifdef JP
		if (name) return "解呪";
		if (desc) return "アイテムにかかった弱い呪いを解除する。";
#else
		if (name) return "Remove Curse";
		if (desc) return "Removes normal curses from equipped items.";
#endif
    
		{
			if (cast)
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
		}
		break;

	case 12:
#ifdef JP
		if (name) return "透明視認";
		if (desc) return "一定時間、透明なものが見えるようになる。";
#else
		if (name) return "Sense Unseen";
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

	case 13:
#ifdef JP
		if (name) return "対邪悪結界";
		if (desc) return "邪悪なモンスターの攻撃を防ぐバリアを張る。";
#else
		if (name) return "Protection from Evil";
		if (desc) return "Gives aura which protect you from evil monster's physical attack.";
#endif
    
		{
			int base = 25;
			int sides = 3 * plev;

			if (info) return info_duration(base, sides);

			if (cast)
			{
				set_protevil(randint1(sides) + sides, FALSE);
			}
		}
		break;

	case 14:
#ifdef JP
		if (name) return "裁きの雷";
		if (desc) return "強力な電撃のボルトを放つ。";
#else
		if (name) return "Judgment Thunder";
		if (desc) return "Fires a powerful bolt of lightning.";
#endif
    
		{
			int dam = spell_power(plev * 5);

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_bolt(GF_ELEC, dir, dam);
			}
		}
		break;

	case 15:
#ifdef JP
		if (name) return "聖なる御言葉";
		if (desc) return "視界内の邪悪な存在に大きなダメージを与え、体力を回復し、毒、恐怖、朦朧状態、負傷から全快する。";
#else
		if (name) return "Holy Word";
		if (desc) return "Damages all evil monsters in sight, heals HP somewhat, and completely heals poison, stun and cut status.";
#endif
    
		{
			int dam_sides = spell_power(plev * 6);
			int heal = spell_power(100);

#ifdef JP
			if (info) return format("損:1d%d/回%d", dam_sides, heal);
#else
			if (info) return format("dam:d%d/h%d", dam_sides, heal);
#endif

			if (cast)
			{
				dispel_evil(randint1(dam_sides));
				hp_player(heal);
				set_poisoned(0, TRUE);
				set_stun(0, TRUE);
				set_cut(0, TRUE);
			}
		}
		break;

	case 16:
#ifdef JP
		if (name) return "開かれた道";
		if (desc) return "一直線上の全ての罠と扉を破壊する。";
#else
		if (name) return "Unbarring Ways";
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

	case 17:
#ifdef JP
		if (name) return "封魔";
		if (desc) return "邪悪なモンスターの動きを止める。";
#else
		if (name) return "Arrest";
		if (desc) return "Attempts to paralyze an evil monster.";
#endif
    
		{
			int power = spell_power(plev * 2);

			if (info) return info_power(power);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				stasis_evil(dir);
			}
		}
		break;

	case 18:
		if (name) return "Angelic Cloak";
		if (desc) return "Gives resistance to acid, cold and lightning. Gives aura of holy power which injures evil monsters which attacked you for a while.";
    
		{
			int base = 20;

			if (info) return info_duration(base, base);

			if (cast)
			{
				set_oppose_acid(randint1(base) + base, FALSE);
				set_oppose_cold(randint1(base) + base, FALSE);
				set_oppose_elec(randint1(base) + base, FALSE);
				set_tim_sh_holy(randint1(base) + base, FALSE);
			}
		}
		break;

	case 19:
#ifdef JP
		if (name) return "アンデッド&悪魔退散";
		if (desc) return "視界内の全てのアンデッド及び悪魔にダメージを与える。";
#else
		if (name) return "Dispel Undead & Demons";
		if (desc) return "Damages all undead and demons in sight.";
#endif
    
		{
			int sides = spell_power(plev * 4);

			if (info) return info_damage(1, sides, 0);

			if (cast)
			{
				dispel_undead(randint1(sides));
				dispel_demons(randint1(sides));
			}
		}
		break;

	case 20:
#ifdef JP
		if (name) return "邪悪退散";
		if (desc) return "視界内の全ての邪悪なモンスターにダメージを与える。";
#else
		if (name) return "Dispel Evil";
		if (desc) return "Damages all evil monsters in sight.";
#endif
    
		{
			int sides = spell_power(plev * 4);

			if (info) return info_damage(1, sides, 0);

			if (cast)
			{
				dispel_evil(randint1(sides));
			}
		}
		break;

	case 21:
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
				brand_weapon(EGO_SLAY_EVIL);
			}
		}
		break;

	case 22:
#ifdef JP
		if (name) return "スターバースト";
		if (desc) return "巨大な閃光の球を放つ。";
#else
		if (name) return "Star Burst";
		if (desc) return "Fires a huge ball of powerful light.";
#endif
    
		{
			int dam = spell_power(100 + plev * 2);
			int rad = spell_power(4);

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_LITE, dir, dam, rad);
			}
		}
		break;

	case 23:
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
				else mode |= PM_NO_PET;
				if (!(pet && (plev < 50))) mode |= PM_ALLOW_GROUP;

				if (summon_specific((pet ? -1 : 0), py, px, (plev * 3) / 2, SUMMON_ANGEL, mode))
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

	case 24:
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
			}
		}
		break;

	case 25:
#ifdef JP
		if (name) return "呪い退散";
		if (desc) return "アイテムにかかった強力な呪いを解除する。";
#else
		if (name) return "Dispel Curse";
		if (desc) return "Removes normal and heavy curse from equipped items.";
#endif
    
		{
			if (cast)
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
		break;

	case 26:
#ifdef JP
		if (name) return "邪悪追放";
		if (desc) return "視界内の全ての邪悪なモンスターをテレポートさせる。抵抗されると無効。";
#else
		if (name) return "Banish Evil";
		if (desc) return "Teleports all evil monsters in sight away unless resisted.";
#endif
    
		{
			int power = spell_power(100);

			if (info) return info_power(power);

			if (cast)
			{
				if (banish_evil(power))
				{
#ifdef JP
					msg_print("神聖な力が邪悪を打ち払った！");
#else
					msg_print("The holy power banishes evil!");
#endif

				}
			}
		}
		break;

	case 27:
#ifdef JP
		if (name) return "ハルマゲドン";
		if (desc) return "周辺のアイテム、モンスター、地形を破壊する。";
#else
		if (name) return "Armageddon";
		if (desc) return "Destroy everything in nearby area.";
#endif
    
		{
			int base = 12;
			int sides = 4;

			if (cast)
			{
				destroy_area(py, px, base + randint1(sides), spell_power(4 * p_ptr->lev));
			}
		}
		break;

	case 28:
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

	case 29:
#ifdef JP
		if (name) return "神の怒り";
		if (desc) return "ターゲットの周囲に分解の球を多数落とす。";
#else
		if (name) return "Wrath of the God";
		if (desc) return "Drops many balls of disintegration near the target.";
#endif
    
		{
			int dam = spell_power(plev * 3 + 25);
			int rad = 2;

			if (info) return info_multi_damage(dam);

			if (cast)
			{
				if (!cast_wrath_of_the_god(dam, rad)) return NULL;
			}
		}
		break;

	case 30:
#ifdef JP
		if (name) return "神威";
		if (desc) return "隣接するモンスターに聖なるダメージを与え、視界内のモンスターにダメージ、減速、朦朧、混乱、恐怖、眠りを与える。さらに体力を回復する。";
#else
		if (name) return "Divine Intervention";
		if (desc) return "Damages all adjacent monsters with holy power. Damages and attempt to slow, stun, confuse, scare and freeze all monsters in sight. And heals HP.";
#endif
    
		{
			int b_dam = spell_power(plev * 9);
			int d_dam = spell_power(plev * 4);
			int heal = spell_power(100);
			int power = spell_power(plev * 4);

#ifdef JP
			if (info) return format("回%d/損%d+%d", heal, d_dam, b_dam/2);
#else
			if (info) return format("h%d/dm%d+%d", heal, d_dam, b_dam/2);
#endif

			if (cast)
			{
				project(0, 1, py, px, b_dam, GF_HOLY_FIRE, PROJECT_KILL, -1);
				dispel_monsters(d_dam);
				slow_monsters(power);
				stun_monsters(power);
				confuse_monsters(power);
				turn_monsters(power);
				stasis_monsters(power/3);
				hp_player(heal);
			}
		}
		break;

	case 31:
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
				int sp_sides = 20 + plev;
				int sp_base = plev;

				int i;
				crusade();
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
					summon_specific(-1, my, mx, plev, SUMMON_KNIGHTS, (PM_ALLOW_GROUP | PM_FORCE_PET | PM_HASTE));
				}
				set_hero(randint1(base) + base, FALSE);
				set_blessed(randint1(base) + base, FALSE);
				set_fast(randint1(sp_sides) + sp_base, FALSE);
				set_protevil(randint1(base) + base, FALSE);
				fear_clear_p();
			}
		}
		break;
	}

	return "";
}


static cptr do_music_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
	bool fail = (mode == SPELL_FAIL) ? TRUE : FALSE;
	bool cont = (mode == SPELL_CONT) ? TRUE : FALSE;
	bool stop = (mode == SPELL_STOP) ? TRUE : FALSE;
	bool spoil = (mode == SPELL_SPOIL_DESC) ? TRUE : FALSE;

#ifdef JP
	static const char s_dam[] = "損傷:";
#else
	static const char s_dam[] = "dam ";
#endif

	int dir;
	int plev = p_ptr->lev;

	switch (spell)
	{
	case 0:
#ifdef JP
		if (name) return "遅鈍の歌";
		if (desc) return "視界内の全てのモンスターを減速させる。抵抗されると無効。";
#else
		if (name) return "Song of Holding";
		if (desc) return "Attempts to slow all monsters in sight.";
#endif
    
		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("ゆっくりとしたメロディを口ずさみ始めた．．．");
#else
			msg_print("You start humming a slow, steady melody...");
#endif
			bard_start_singing(spell, MUSIC_SLOW);
		}

		{
			int power = plev;

			if (info) return info_power(power);

			if (cont)
			{
				slow_monsters(power);
			}
		}
		break;

	case 1:
#ifdef JP
		if (name) return "祝福の歌";
		if (desc) return "命中率とACのボーナスを得る。";
#else
		if (name) return "Song of Blessing";
		if (desc) return "Gives bonus to hit and AC for a few turns.";
#endif
    
		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("厳かなメロディを奏で始めた．．．");
#else
			msg_print("The holy power of the Music of the Ainur enters you...");
#endif
			bard_start_singing(spell, MUSIC_BLESS);
		}

		if (stop)
		{
			if (!p_ptr->blessed)
			{
#ifdef JP
				msg_print("高潔な気分が消え失せた。");
#else
				msg_print("The prayer has expired.");
#endif
			}
		}

		break;

	case 2:
#ifdef JP
		if (name) return "崩壊の音色";
		if (desc) return "轟音のボルトを放つ。";
#else
		if (name) return "Wrecking Note";
		if (desc) return "Fires a bolt of sound.";
#endif
    
		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		{
			int dice = spell_power(4 + (plev - 1) / 5);
			int sides = 4;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_bolt(GF_SOUND, dir, damroll(dice, sides));
			}
		}
		break;

	case 3:
#ifdef JP
		if (name) return "朦朧の旋律";
		if (desc) return "視界内の全てのモンスターを朦朧させる。抵抗されると無効。";
#else
		if (name) return "Stun Pattern";
		if (desc) return "Attempts to stun all monsters in sight.";
#endif
    
		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("眩惑させるメロディを奏で始めた．．．");
#else
			msg_print("You weave a pattern of sounds to bewilder and daze...");
#endif
			bard_start_singing(spell, MUSIC_STUN);
		}

		{
			int dice = spell_power(plev / 10);
			int sides = 2;

			if (info) return info_power_dice(dice, sides);

			if (cont)
			{
				stun_monsters(damroll(dice, sides));
			}
		}

		break;

	case 4:
#ifdef JP
		if (name) return "生命の流れ";
		if (desc) return "体力を少し回復させる。";
#else
		if (name) return "Flow of Life";
		if (desc) return "Heals HP a little.";
#endif
    
		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("歌を通して体に活気が戻ってきた．．．");
#else
			msg_print("Life flows through you as you sing a song of healing...");
#endif
			bard_start_singing(spell, MUSIC_L_LIFE);
		}

		{
			int dice = 2;
			int sides = spell_power(6);

			if (info) return info_heal(dice, sides, 0);

			if (cont)
			{
				hp_player(damroll(dice, sides));
			}
		}

		break;

	case 5:
#ifdef JP
		if (name) return "太陽の歌";
		if (desc) return "光源が照らしている範囲か部屋全体を永久に明るくする。";
#else
		if (name) return "Song of the Sun";
		if (desc) return "Lights up nearby area and the inside of a room permanently.";
#endif
    
		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		{
			int dice = 2;
			int sides = plev / 2;
			int rad = plev / 10 + 1;

			if (info) return info_damage(dice, sides, 0);

			if (cast)
			{
#ifdef JP
				msg_print("光り輝く歌が辺りを照らした。");
#else
				msg_print("Your uplifting song brings brightness to dark places...");
#endif

				lite_area(damroll(dice, sides), rad);
			}
		}
		break;

	case 6:
#ifdef JP
		if (name) return "恐怖の歌";
		if (desc) return "視界内の全てのモンスターを恐怖させる。抵抗されると無効。";
#else
		if (name) return "Song of Fear";
		if (desc) return "Attempts to scare all monsters in sight.";
#endif
    
		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("おどろおどろしいメロディを奏で始めた．．．");
#else
			msg_print("You start weaving a fearful pattern...");
#endif
			bard_start_singing(spell, MUSIC_FEAR);			
		}

		{
			int power = spell_power(plev);

			if (info) return info_power(power);

			if (cont)
			{
				project_hack(GF_TURN_ALL, power);
			}
		}

		break;

	case 7:
#ifdef JP
		if (name) return "戦いの歌";
		if (desc) return "ヒーロー気分になる。";
#else
		if (name) return "Heroic Ballad";
		if (desc) return "Removes fear, and gives bonus to hit and 10 more HP for a while.";
#endif

		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("激しい戦いの歌を歌った．．．");
#else
			msg_print("You start singing a song of intense fighting...");
#endif

			(void)hp_player(10);
			fear_clear_p();

			/* Recalculate hitpoints */
			p_ptr->update |= (PU_HP);

			bard_start_singing(spell, MUSIC_HERO);
		}

		if (stop)
		{
			if (!p_ptr->hero)
			{
#ifdef JP
				msg_print("ヒーローの気分が消え失せた。");
#else
				msg_print("The heroism wears off.");
#endif
				/* Recalculate hitpoints */
				p_ptr->update |= (PU_HP);
			}
		}

		break;

	case 8:
#ifdef JP
		if (name) return "霊的知覚";
		if (desc) return "近くの罠/扉/階段を感知する。レベル15で全てのモンスター、20で財宝とアイテムを感知できるようになる。レベル25で周辺の地形を感知し、40でその階全体を永久に照らし、ダンジョン内のすべてのアイテムを感知する。この効果は歌い続けることで順に起こる。";
#else
		if (name) return "Clairaudience";
		if (desc) return "Detects traps, doors and stairs in your vicinity. And detects all monsters at level 15, treasures and items at level 20. Maps nearby area at level 25. Lights and know the whole level at level 40. These effects occurs by turns while this song continues.";
#endif
    
		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("静かな音楽が感覚を研ぎ澄まさせた．．．");
#else
			msg_print("Your quiet music sharpens your sense of hearing...");
#endif

			/* Hack -- Initialize the turn count */
			p_ptr->magic_num1[2] = 0;

			bard_start_singing(spell, MUSIC_DETECT);
		}

		{
			int rad = DETECT_RAD_DEFAULT;

			if (info) return info_radius(rad);

			if (cont)
			{
				int count = p_ptr->magic_num1[2];

				if (count >= 19) wiz_lite(FALSE);
				if (count >= 11)
				{
					map_area(rad);
					if (plev > 39 && count < 19)
						p_ptr->magic_num1[2] = count + 1;
				}
				if (count >= 6)
				{
					/* There are too many hidden treasure.  So... */
					/* detect_treasure(rad); */
					detect_objects_gold(rad);
					detect_objects_normal(rad);

					if (plev > 24 && count < 11)
						p_ptr->magic_num1[2] = count + 1;
				}
				if (count >= 3)
				{
					detect_monsters_invis(rad);
					detect_monsters_normal(rad);

					if (plev > 19 && count < 6)
						p_ptr->magic_num1[2] = count + 1;
				}
				detect_traps(rad, TRUE);
				detect_doors(rad);
				detect_stairs(rad);

				if (plev > 14 && count < 3)
					p_ptr->magic_num1[2] = count + 1;
			}
		}

		break;

	case 9:
#ifdef JP
		if (name) return "魂の歌";
		if (desc) return "視界内の全てのモンスターに対して精神攻撃を行う。";
#else
		if (name) return "Soul Shriek";
		if (desc) return "Damages all monsters in sight with PSI damages.";
#endif

		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("精神を捻じ曲げる歌を歌った．．．");
#else
			msg_print("You start singing a song of soul in pain...");
#endif
			bard_start_singing(spell, MUSIC_PSI);
		}

		{
			int dice = 1;
			int sides = spell_power(plev * 3 / 2);

			if (info) return info_damage(dice, sides, 0);

			if (cont)
			{
				project_hack(GF_PSI, damroll(dice, sides));
			}
		}

		break;

	case 10:
#ifdef JP
		if (name) return "知識の歌";
		if (desc) return "自分のいるマスと隣りのマスに落ちているアイテムを鑑定する。";
#else
		if (name) return "Song of Lore";
		if (desc) return "Identifies all items which are in the adjacent squares.";
#endif
    
		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("この世界の知識が流れ込んできた．．．");
#else
			msg_print("You recall the rich lore of the world...");
#endif
			bard_start_singing(spell, MUSIC_ID);
		}

		{
			int rad = 1;

			if (info) return info_radius(rad);

			/*
			 * 歌の開始時にも効果発動：
			 * MP不足で鑑定が発動される前に歌が中断してしまうのを防止。
			 */
			if (cont || cast)
			{
				project(0, rad, py, px, 0, GF_IDENTIFY, PROJECT_ITEM, -1);
			}
		}

		break;

	case 11:
#ifdef JP
		if (name) return "隠遁の歌";
		if (desc) return "隠密行動能力を上昇させる。";
#else
		if (name) return "Hiding Tune";
		if (desc) return "Gives improved stealth.";
#endif

		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("あなたの姿が景色にとけこんでいった．．．");
#else
			msg_print("Your song carries you beyond the sight of mortal eyes...");
#endif
			bard_start_singing(spell, MUSIC_STEALTH);
		}

		if (stop)
		{
			if (!p_ptr->tim_stealth)
			{
#ifdef JP
				msg_print("姿がはっきりと見えるようになった。");
#else
				msg_print("You are no longer hided.");
#endif
			}
		}

		break;

	case 12:
#ifdef JP
		if (name) return "幻影の旋律";
		if (desc) return "視界内の全てのモンスターを混乱させる。抵抗されると無効。";
#else
		if (name) return "Illusion Pattern";
		if (desc) return "Attempts to confuse all monsters in sight.";
#endif
    
		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("辺り一面に幻影が現れた．．．");
#else
			msg_print("You weave a pattern of sounds to beguile and confuse...");
#endif
			bard_start_singing(spell, MUSIC_CONF);
		}

		{
			int power = plev * 2;

			if (info) return info_power(power);

			if (cont)
			{
				confuse_monsters(power);
			}
		}

		break;

	case 13:
#ifdef JP
		if (name) return "破滅の叫び";
		if (desc) return "視界内の全てのモンスターに対して轟音攻撃を行う。";
#else
		if (name) return "Doomcall";
		if (desc) return "Damages all monsters in sight with booming sound.";
#endif
    
		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("轟音が響いた．．．");
#else
			msg_print("The fury of the Downfall of Numenor lashes out...");
#endif
			bard_start_singing(spell, MUSIC_SOUND);
		}

		{
			int dice = spell_power(10 + plev / 5);
			int sides = 7;

			if (info) return info_damage(dice, sides, 0);

			if (cont)
			{
				project_hack(GF_SOUND, damroll(dice, sides));
			}
		}

		break;

	case 14:
#ifdef JP
		if (name) return "フィリエルの歌";
		if (desc) return "周囲の死体や骨を生き返す。";
#else
		if (name) return "Firiel's Song";
		if (desc) return "Resurrects nearby corpse and skeletons. And makes these your pets.";
#endif
    
		{
			/* Stop singing before start another */
			if (cast || fail) bard_stop_singing();

			if (cast)
			{
#ifdef JP
				msg_print("生命と復活のテーマを奏で始めた．．．");
#else
				msg_print("The themes of life and revival are woven into your song...");
#endif

				animate_dead(0, py, px);
			}
		}
		break;

	case 15:
#ifdef JP
		if (name) return "旅の仲間";
		if (desc) return "視界内の全てのモンスターを魅了する。抵抗されると無効。";
#else
		if (name) return "Fellowship Chant";
		if (desc) return "Attempts to charm all monsters in sight.";
#endif

		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("安らかなメロディを奏で始めた．．．");
#else
			msg_print("You weave a slow, soothing melody of imploration...");
#endif
			bard_start_singing(spell, MUSIC_CHARM);
		}

		{
			int dice = spell_power(10 + plev / 15);
			int sides = 6;

			if (info) return info_power_dice(dice, sides);

			if (cont)
			{
				charm_monsters(damroll(dice, sides));
			}
		}

		break;

	case 16:
#ifdef JP
		if (name) return "分解音波";
		if (desc) return "壁を掘り進む。自分の足元のアイテムは蒸発する。";
#else
		if (name) return "Sound of disintegration";
		if (desc) return "Makes you be able to burrow into walls. Objects under your feet evaporate.";
#endif

		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("粉砕するメロディを奏で始めた．．．");
#else
			msg_print("You weave a violent pattern of sounds to break wall.");
#endif
			bard_start_singing(spell, MUSIC_WALL);
		}

		{
			/*
			 * 歌の開始時にも効果発動：
			 * MP不足で効果が発動される前に歌が中断してしまうのを防止。
			 */
			if (cont || cast)
			{
				project(0, 0, py, px,
					0, GF_DISINTEGRATE, PROJECT_KILL | PROJECT_ITEM | PROJECT_HIDE, -1);
			}
		}
		break;

	case 17:
#ifdef JP
		if (name) return "元素耐性";
		if (desc) return "酸、電撃、炎、冷気、毒に対する耐性を得る。装備による耐性に累積する。";
#else
		if (name) return "Finrod's Resistance";
		if (desc) return "Gives resistance to fire, cold, electricity, acid and poison. These resistances can be added to which from equipment for more powerful resistances.";
#endif
    
		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("元素の力に対する忍耐の歌を歌った。");
#else
			msg_print("You sing a song of perseverance against powers...");
#endif
			bard_start_singing(spell, MUSIC_RESIST);
		}

		if (stop)
		{
			if (!p_ptr->oppose_acid)
			{
#ifdef JP
				msg_print("酸への耐性が薄れた気がする。");
#else
				msg_print("You feel less resistant to acid.");
#endif
			}

			if (!p_ptr->oppose_elec)
			{
#ifdef JP
				msg_print("電撃への耐性が薄れた気がする。");
#else
				msg_print("You feel less resistant to elec.");
#endif
			}

			if (!p_ptr->oppose_fire)
			{
#ifdef JP
				msg_print("火への耐性が薄れた気がする。");
#else
				msg_print("You feel less resistant to fire.");
#endif
			}

			if (!p_ptr->oppose_cold)
			{
#ifdef JP
				msg_print("冷気への耐性が薄れた気がする。");
#else
				msg_print("You feel less resistant to cold.");
#endif
			}

			if (!p_ptr->oppose_pois)
			{
#ifdef JP
				msg_print("毒への耐性が薄れた気がする。");
#else
				msg_print("You feel less resistant to pois.");
#endif
			}
		}

		break;

	case 18:
#ifdef JP
		if (name) return "ホビットのメロディ";
		if (desc) return "加速する。";
#else
		if (name) return "Hobbit Melodies";
		if (desc) return "Hastes you.";
#endif

		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("軽快な歌を口ずさみ始めた．．．");
#else
			msg_print("You start singing joyful pop song...");
#endif
			bard_start_singing(spell, MUSIC_SPEED);
		}

		if (stop)
		{
			if (!p_ptr->fast)
			{
#ifdef JP
				msg_print("動きの素早さがなくなったようだ。");
#else
				msg_print("You feel yourself slow down.");
#endif
			}
		}

		break;

	case 19:
#ifdef JP
		if (name) return "歪んだ世界";
		if (desc) return "近くのモンスターをテレポートさせる。抵抗されると無効。";
#else
		if (name) return "World Contortion";
		if (desc) return "Teleports all nearby monsters away unless resisted.";
#endif
    
		{
			int rad = spell_power(plev / 15 + 1);
			int power = spell_power(plev * 3 + 1);

			if (info) return info_radius(rad);

			/* Stop singing before start another */
			if (cast || fail) bard_stop_singing();

			if (cast)
			{
#ifdef JP
				msg_print("歌が空間を歪めた．．．");
#else
				msg_print("Reality whirls wildly as you sing a dizzying melody...");
#endif

				project(0, rad, py, px, power, GF_AWAY_ALL, PROJECT_KILL, -1);
			}
		}
		break;

	case 20:
#ifdef JP
		if (name) return "退散の歌";
		if (desc) return "視界内の全てのモンスターにダメージを与える。邪悪なモンスターに特に大きなダメージを与える。";
#else
		if (name) return "Dispelling chant";
		if (desc) return "Damages all monsters in sight. Hurts evil monsters greatly.";
#endif
    
		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("耐えられない不協和音が敵を責め立てた．．．");
#else
			msg_print("You cry out in an ear-wracking voice...");
#endif
			bard_start_singing(spell, MUSIC_DISPEL);
		}

		{
			int m_sides = spell_power(plev * 3);
			int e_sides = spell_power(plev * 3);

			if (info) return format("%s1d%d+1d%d", s_dam, m_sides, e_sides);

			if (cont)
			{
				dispel_monsters(randint1(m_sides));
				dispel_evil(randint1(e_sides));
			}
		}
		break;

	case 21:
#ifdef JP
		if (name) return "サルマンの甘言";
		if (desc) return "視界内の全てのモンスターを減速させ、眠らせようとする。抵抗されると無効。";
#else
		if (name) return "The Voice of Saruman";
		if (desc) return "Attempts to slow and sleep all monsters in sight.";
#endif
    
		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("優しく、魅力的な歌を口ずさみ始めた．．．");
#else
			msg_print("You start humming a gentle and attractive song...");
#endif
			bard_start_singing(spell, MUSIC_SARUMAN);
		}

		{
			int power = spell_power(plev);

			if (info) return info_power(power);

			if (cont)
			{
				slow_monsters(power);
				sleep_monsters(power);
			}
		}

		break;

	case 22:
#ifdef JP
		if (name) return "嵐の音色";
		if (desc) return "轟音のビームを放つ。";
#else
		if (name) return "Song of the Tempest";
		if (desc) return "Fires a beam of sound.";
#endif
    
		{
			int dice = spell_power(15 + (plev - 1) / 2);
			int sides = 10;

			if (info) return info_damage(dice, sides, 0);

			/* Stop singing before start another */
			if (cast || fail) bard_stop_singing();

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_beam(GF_SOUND, dir, damroll(dice, sides));
			}
		}
		break;

	case 23:
#ifdef JP
		if (name) return "もう一つの世界";
		if (desc) return "現在の階を再構成する。";
#else
		if (name) return "Ambarkanta";
		if (desc) return "Recreates current dungeon level.";
#endif
    
		{
			int base = 15;
			int sides = 20;

			if (info) return info_delay(base, sides);

			/* Stop singing before start another */
			if (cast || fail) bard_stop_singing();

			if (cast)
			{
#ifdef JP
				msg_print("周囲が変化し始めた．．．");
#else
				msg_print("You sing of the primeval shaping of Middle-earth...");
#endif

				alter_reality();
			}
		}
		break;

	case 24:
#ifdef JP
		if (name) return "破壊の旋律";
		if (desc) return "周囲のダンジョンを揺らし、壁と床をランダムに入れ変える。";
#else
		if (name) return "Wrecking Pattern";
		if (desc) return "Shakes dungeon structure, and results in random swapping of floors and walls.";
#endif

		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("破壊的な歌が響きわたった．．．");
#else
			msg_print("You weave a pattern of sounds to contort and shatter...");
#endif
			bard_start_singing(spell, MUSIC_QUAKE);
		}

		{
			int rad = 10;

			if (info) return info_radius(rad);

			if (cont)
			{
				earthquake(py, px, 10);
			}
		}

		break;


	case 25:
#ifdef JP
		if (name) return "停滞の歌";
		if (desc) return "視界内の全てのモンスターを麻痺させようとする。抵抗されると無効。";
#else
		if (name) return "Stationary Shriek";
		if (desc) return "Attempts to freeze all monsters in sight.";
#endif
    
		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("ゆっくりとしたメロディを奏で始めた．．．");
#else
			msg_print("You weave a very slow pattern which is almost likely to stop...");
#endif
			bard_start_singing(spell, MUSIC_STASIS);
		}

		{
			int power = spell_power(plev * 4);

			if (info) return info_power(power);

			if (cont)
			{
				stasis_monsters(power);
			}
		}

		break;

	case 26:
#ifdef JP
		if (name) return "守りの歌";
		if (desc) return "自分のいる床の上に、モンスターが通り抜けたり召喚されたりすることができなくなるルーンを描く。";
#else
		if (name) return "Endurance";
		if (desc) return "Sets a glyph on the floor beneath you. Monsters cannot attack you if you are on a glyph, but can try to break glyph.";
#endif
    
		{
			/* Stop singing before start another */
			if (cast || fail) bard_stop_singing();

			if (cast)
			{
#ifdef JP
				msg_print("歌が神聖な場を作り出した．．．");
#else
				msg_print("The holy power of the Music is creating sacred field...");
#endif

				warding_glyph();
			}
		}
		break;

	case 27:
#ifdef JP
		if (name) return "英雄の詩";
		if (desc) return "加速し、ヒーロー気分になり、視界内の全てのモンスターにダメージを与える。";
#else
		if (name) return "The Hero's Poem";
		if (desc) return "Hastes you. Gives heroism. Damages all monsters in sight.";
#endif
    
		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("英雄の歌を口ずさんだ．．．");
#else
			msg_print("You chant a powerful, heroic call to arms...");
#endif
			(void)hp_player(10);

			/* Recalculate hitpoints */
			p_ptr->update |= (PU_HP);

			bard_start_singing(spell, MUSIC_SHERO);
		}

		if (stop)
		{
			if (!p_ptr->hero)
			{
#ifdef JP
				msg_print("ヒーローの気分が消え失せた。");
#else
				msg_print("The heroism wears off.");
#endif
				/* Recalculate hitpoints */
				p_ptr->update |= (PU_HP);
			}

			if (!p_ptr->fast)
			{
#ifdef JP
				msg_print("動きの素早さがなくなったようだ。");
#else
				msg_print("You feel yourself slow down.");
#endif
			}
		}

		{
			int dice = 1;
			int sides = plev * 3;

			if (info) return info_damage(dice, sides, 0);

			if (cont)
			{
				dispel_monsters(damroll(dice, sides));
			}
		}
		break;

	case 28:
#ifdef JP
		if (name) return "ヤヴァンナの助け";
		if (desc) return "強力な回復の歌で、負傷と朦朧状態も全快する。";
#else
		if (name) return "Relief of Yavanna";
		if (desc) return "Powerful healing song. Also heals cut and stun completely.";
#endif
    
		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
			msg_print("歌を通して体に活気が戻ってきた．．．");
#else
			msg_print("Life flows through you as you sing the song...");
#endif
			bard_start_singing(spell, MUSIC_H_LIFE);
		}

		{
			int dice = spell_power(15);
			int sides = 10;

			if (info) return info_heal(dice, sides, 0);

			if (cont)
			{
				hp_player(damroll(dice, sides));
				set_stun(0, TRUE);
				set_cut(0, TRUE);
			}
		}

		break;

	case 29:
#ifdef JP
		if (name) return "再生の歌";
		if (desc) return "すべてのステータスと経験値を回復する。";
#else
		if (name) return "Goddess' rebirth";
		if (desc) return "Restores all stats and experience.";
#endif
    
		{
			/* Stop singing before start another */
			if (cast || fail) bard_stop_singing();

			if (cast)
			{
#ifdef JP
				msg_print("暗黒の中に光と美をふりまいた。体が元の活力を取り戻した。");
#else
				msg_print("You strewed light and beauty in the dark as you sing. You feel refreshed.");
#endif
				(void)do_res_stat(A_STR);
				(void)do_res_stat(A_INT);
				(void)do_res_stat(A_WIS);
				(void)do_res_stat(A_DEX);
				(void)do_res_stat(A_CON);
				(void)do_res_stat(A_CHR);
				(void)restore_level();
			}
		}
		break;

	case 30:
#ifdef JP
		if (name) return "サウロンの魔術";
		if (desc) return "非常に強力でごく小さい轟音の球を放つ。";
#else
		if (name) return "Wizardry of Sauron";
		if (desc) return "Fires an extremely powerful tiny ball of sound.";
#endif
    
		{
			int dice = spell_power(50 + plev);
			int sides = 10;
			int rad = 0;

			if (info) return info_damage(dice, sides, 0);

			/* Stop singing before start another */
			if (cast || fail) bard_stop_singing();

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;

				fire_ball(GF_SOUND, dir, damroll(dice, sides), rad);
			}
		}
		break;

	case 31:
#ifdef JP
		if (name) return "フィンゴルフィンの挑戦";
		if (desc) return "ダメージを受けなくなるバリアを張る。";
#else
		if (name) return "Fingolfin's Challenge";
		if (desc) return "Generates barrier which completely protect you from almost all damages. Takes a few your turns when the barrier breaks.";
#endif
    
		/* Stop singing before start another */
		if (cast || fail) bard_stop_singing();

		if (cast)
		{
#ifdef JP
				msg_print("フィンゴルフィンの冥王への挑戦を歌った．．．");
#else
				msg_print("You recall the valor of Fingolfin's challenge to the Dark Lord...");
#endif

				/* Redraw map */
				p_ptr->redraw |= (PR_MAP);
		
				/* Update monsters */
				p_ptr->update |= (PU_MONSTERS);
		
				/* Window stuff */
				p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);

				bard_start_singing(spell, MUSIC_INVULN);
		}

		if (stop)
		{
			if (!p_ptr->invuln)
			{
#ifdef JP
				msg_print("無敵ではなくなった。");
#else
				msg_print("The invulnerability wears off.");
#endif
				/* Redraw map */
				p_ptr->redraw |= (PR_MAP);

				/* Update monsters */
				p_ptr->update |= (PU_MONSTERS);

				/* Window stuff */
				p_ptr->window |= (PW_OVERHEAD | PW_DUNGEON);
			}
		}

		break;
	}

	return "";
}



/* Hex */
static bool item_tester_hook_weapon_except_bow(object_type *o_ptr)
{
	switch (o_ptr->tval)
	{
		case TV_SWORD:
		case TV_HAFTED:
		case TV_POLEARM:
		case TV_DIGGING:
		{
			return (TRUE);
		}
	}

	return (FALSE);
}

static bool item_tester_hook_cursed(object_type *o_ptr)
{
	return (bool)(object_is_cursed(o_ptr));
}

static cptr do_hex_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
	bool fail = (mode == SPELL_FAIL) ? TRUE : FALSE;
	bool cont = (mode == SPELL_CONT) ? TRUE : FALSE;
	bool stop = (mode == SPELL_STOP) ? TRUE : FALSE;
	bool spoil = (mode == SPELL_SPOIL_DESC) ? TRUE : FALSE;

	bool add = TRUE;

	int plev = p_ptr->lev;
	int power;

	switch (spell)
	{
	/*** 1st book (0-7) ***/
	case 0:
#ifdef JP
		if (name) return "邪なる祝福";
		if (desc) return "祝福により攻撃精度と防御力が上がる。";
#else
		if (name) return "Evily blessing";
		if (desc) return "Attempts to increase +to_hit of a weapon and AC";
#endif
		if (cast)
		{
			if (!p_ptr->blessed)
			{
#ifdef JP
				msg_print("高潔な気分になった！");
#else
				msg_print("You feel righteous!");
#endif
			}
		}
		if (stop)
		{
			if (!p_ptr->blessed)
			{
#ifdef JP
				msg_print("高潔な気分が消え失せた。");
#else
				msg_print("The prayer has expired.");
#endif
			}
		}
		break;

	case 1:
#ifdef JP
		if (name) return "軽傷の治癒";
		if (desc) return "HPや傷を少し回復させる。";
#else
		if (name) return "Cure light wounds";
		if (desc) return "Heals cut and HP a little.";
#endif
		if (info) return info_heal(1, 10, 0);
		if (cast)
		{
#ifdef JP
			msg_print("気分が良くなってくる。");
#else
			msg_print("You feel better and better.");
#endif
		}
		if (cast || cont)
		{
			hp_player(damroll(1, 10));
			set_cut(p_ptr->cut - 10, TRUE);
		}
		break;

	case 2:
#ifdef JP
		if (name) return "悪魔のオーラ";
		if (desc) return "炎のオーラを身にまとい、回復速度が速くなる。";
#else
		if (name) return "Demonic aura";
		if (desc) return "Gives fire aura and regeneration.";
#endif
		if (cast)
		{
#ifdef JP
			msg_print("体が炎のオーラで覆われた。");
#else
			msg_print("You have enveloped by fiery aura!");
#endif
		}
		if (stop)
		{
#ifdef JP
			msg_print("炎のオーラが消え去った。");
#else
			msg_print("Fiery aura disappeared.");
#endif
		}
		break;

	case 3:
#ifdef JP
		if (name) return "悪臭霧";
		if (desc) return "視界内のモンスターに微弱量の毒のダメージを与える。";
#else
		if (name) return "Stinking mist";
		if (desc) return "Deals few damages of poison to all monsters in your sight.";
#endif
		power = plev / 2 + 5;
		if (info) return info_damage(1, power, 0);
		if (cast || cont)
		{
			project_hack(GF_POIS, randint1(power));
		}
		break;

	case 4:
#ifdef JP
		if (name) return "腕力強化";
		if (desc) return "術者の腕力を上昇させる。";
#else
		if (name) return "Extra might";
		if (desc) return "Attempts to increase your strength.";
#endif
		if (cast)
		{
#ifdef JP
			msg_print("何だか力が湧いて来る。");
#else
			msg_print("You feel you get stronger.");
#endif
		}
		break;

	case 5:
#ifdef JP
		if (name) return "武器呪縛";
		if (desc) return "装備している武器を呪う。";
#else
		if (name) return "Curse weapon";
		if (desc) return "Curses your weapon.";
#endif
		if (cast)
		{
			int item;
			char *q, *s;
			char o_name[MAX_NLEN];
			object_type *o_ptr;
			u32b f[TR_FLAG_SIZE];

			item_tester_hook = item_tester_hook_weapon_except_bow;
#ifdef JP
			q = "どれを呪いますか？";
			s = "武器を装備していない。";
#else
			q = "Which weapon do you curse?";
			s = "You wield no weapons.";
#endif

			if (!get_item(&item, q, s, (USE_EQUIP))) return FALSE;

			o_ptr = &inventory[item];
			object_desc(o_name, o_ptr, OD_NAME_ONLY);
			object_flags(o_ptr, f);

#ifdef JP
			if (!get_check(format("本当に %s を呪いますか？", o_name))) return FALSE;
#else
			if (!get_check(format("Do you curse %s, really？", o_name))) return FALSE;
#endif

			if (!one_in_(3) &&
				(object_is_artifact(o_ptr) || have_flag(f, TR_BLESSED)))
			{
#ifdef JP
				msg_format("%s は呪いを跳ね返した。", o_name);
#else
				msg_format("%s resists the effect.", o_name);
#endif
				if (one_in_(3))
				{
					if (o_ptr->to_d > 0)
					{
						o_ptr->to_d -= randint1(3) % 2;
						if (o_ptr->to_d < 0) o_ptr->to_d = 0;
					}
					if (o_ptr->to_h > 0)
					{
						o_ptr->to_h -= randint1(3) % 2;
						if (o_ptr->to_h < 0) o_ptr->to_h = 0;
					}
					if (o_ptr->to_a > 0)
					{
						o_ptr->to_a -= randint1(3) % 2;
						if (o_ptr->to_a < 0) o_ptr->to_a = 0;
					}
#ifdef JP
					msg_format("%s は劣化してしまった。", o_name);
#else
					msg_format("Your %s was disenchanted!", o_name);
#endif
				}
			}
			else
			{
				int power = 0;
#ifdef JP
				msg_format("恐怖の暗黒オーラがあなたの%sを包み込んだ！", o_name);
#else
				msg_format("A terrible black aura blasts your %s!", o_name);
#endif
				o_ptr->curse_flags |= (TRC_CURSED);

				if (object_is_artifact(o_ptr) || object_is_ego(o_ptr))
				{

					if (one_in_(3)) o_ptr->curse_flags |= (TRC_HEAVY_CURSE);
					if (one_in_(666))
					{
						o_ptr->curse_flags |= (TRC_TY_CURSE);
						if (one_in_(666)) o_ptr->curse_flags |= (TRC_PERMA_CURSE);

						add_flag(o_ptr->art_flags, TR_AGGRAVATE);
						add_flag(o_ptr->art_flags, TR_VORPAL);
						add_flag(o_ptr->art_flags, TR_VAMPIRIC);
#ifdef JP
						msg_print("血だ！血だ！血だ！");
#else
						msg_print("Blood, Blood, Blood!");
#endif
						power = 2;
					}
				}

				o_ptr->curse_flags |= get_curse(power, o_ptr);
			}

			p_ptr->update |= (PU_BONUS);
			add = FALSE;
		}
		break;

	case 6:
#ifdef JP
		if (name) return "邪悪感知";
		if (desc) return "周囲の邪悪なモンスターを感知する。";
#else
		if (name) return "Evil detection";
		if (desc) return "Detects evil monsters.";
#endif
		if (info) return info_range(MAX_SIGHT);
		if (cast)
		{
#ifdef JP
			msg_print("邪悪な生物の存在を感じ取ろうとした。");
#else
			msg_print("You attend to the presence of evil creatures.");
#endif
		}
		break;

	case 7:
#ifdef JP
		if (name) return "我慢";
		if (desc) return "数ターン攻撃を耐えた後、受けたダメージを地獄の業火として周囲に放出する。";
#else
		if (name) return "Patience";
		if (desc) return "Bursts hell fire strongly after patients any damage while few turns.";
#endif
		power = MIN(200, (p_ptr->magic_num1[2] * 2));
		if (info) return info_damage(0, 0, power);
		if (cast)
		{
			int a = 3 - (p_ptr->pspeed - 100) / 10;
			int r = 3 + randint1(3) + MAX(0, MIN(3, a));

			if (p_ptr->magic_num2[2] > 0)
			{
#ifdef JP
				msg_print("すでに我慢をしている。");
#else
				msg_print("You are already patienting.");
#endif
				return NULL;
			}

			p_ptr->magic_num2[1] = 1;
			p_ptr->magic_num2[2] = r;
			p_ptr->magic_num1[2] = 0;
#ifdef JP
			msg_print("じっと耐えることにした。");
#else
			msg_print("You decide to patient all damages.");
#endif
			add = FALSE;
		}
		if (cont)
		{
			int rad = 2 + (power / 50);

			p_ptr->magic_num2[2]--;

			if ((p_ptr->magic_num2[2] <= 0) || (power >= 200))
			{
#ifdef JP
				msg_print("我慢が解かれた！");
#else
				msg_print("Time for end of patioence!");
#endif
				if (power)
				{
					project(0, rad, py, px, power, GF_HELL_FIRE,
						(PROJECT_STOP | PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL), -1);
				}
				if (p_ptr->wizard)
				{
#ifdef JP
					msg_format("%d点のダメージを返した。", power);
#else
					msg_format("You return %d damages.", power);
#endif
				}

				/* Reset */
				p_ptr->magic_num2[1] = 0;
				p_ptr->magic_num2[2] = 0;
				p_ptr->magic_num1[2] = 0;
			}
		}
		break;

	/*** 2nd book (8-15) ***/
	case 8:
#ifdef JP
		if (name) return "氷の鎧";
		if (desc) return "氷のオーラを身にまとい、防御力が上昇する。";
#else
		if (name) return "Ice armor";
		if (desc) return "Gives cold aura and bonus to AC.";
#endif
		if (cast)
		{
#ifdef JP
			msg_print("体が氷の鎧で覆われた。");
#else
			msg_print("You have enveloped by ice armor!");
#endif
		}
		if (stop)
		{
#ifdef JP
			msg_print("氷の鎧が消え去った。");
#else
			msg_print("Ice armor disappeared.");
#endif
		}
		break;

	case 9:
#ifdef JP
		if (name) return "重傷の治癒";
		if (desc) return "体力や傷を多少回復させる。";
#else
		if (name) return "Cure serious wounds";
		if (desc) return "Heals cut and HP more.";
#endif
		if (info) return info_heal(2, 10, 0);
		if (cast)
		{
#ifdef JP
			msg_print("気分が良くなってくる。");
#else
			msg_print("You feel better and better.");
#endif
		}
		if (cast || cont)
		{
			hp_player(damroll(2, 10));
			set_cut((p_ptr->cut / 2) - 10, TRUE);
		}
		break;

	case 10:
#ifdef JP
		if (name) return "薬品吸入";
		if (desc) return "呪文詠唱を中止することなく、薬の効果を得ることができる。";
#else
		if (name) return "Inhail potion";
		if (desc) return "Quaffs a potion without canceling of casting a spell.";
#endif
		if (cast)
		{
			p_ptr->magic_num1[0] |= (1L << HEX_INHAIL);
			do_cmd_quaff_potion();
			p_ptr->magic_num1[0] &= ~(1L << HEX_INHAIL);
			add = FALSE;
		}
		break;

	case 11:
#ifdef JP
		if (name) return "吸血霧";
		if (desc) return "視界内のモンスターに微弱量の生命力吸収のダメージを与える。与えたダメージの分、体力が回復する。";
#else
		if (name) return "Vampiric mist";
		if (desc) return "Deals few dameges of drain life to all monsters in your sight.";
#endif
		power = (plev / 2) + 5;
		if (info) return info_damage(1, power, 0);
		if (cast || cont)
		{
			project_hack(GF_OLD_DRAIN, randint1(power));
		}
		break;

	case 12:
		if (name) return "Swords to runeswords";
		if (desc) return "Gives vorpal ability to your weapon. Increases damages by your weapon acccording to curse of your weapon.";
		if (cast)
		{
			if (p_ptr->weapon_ct > 1)
				msg_print("Your weapons glow bright black.");
			else
				msg_print("Your weapon glows bright black.");
		}
		if (stop)
			msg_format("Brightness of weapon%s disappeared.", (p_ptr->weapon_ct <= 1) ? "" : "s");
		break;

	case 13:
#ifdef JP
		if (name) return "混乱の手";
		if (desc) return "攻撃した際モンスターを混乱させる。";
#else
		if (name) return "Touch of confusion";
		if (desc) return "Confuses a monster when you attack.";
#endif
		if (cast)
		{
#ifdef JP
			msg_print("あなたの手が赤く輝き始めた。");
#else
			msg_print("Your hands glow bright red.");
#endif
		}
		if (stop)
		{
#ifdef JP
			msg_print("手の輝きがなくなった。");
#else
			msg_print("Brightness on your hands disappeard.");
#endif
		}
		break;

	case 14:
#ifdef JP
		if (name) return "肉体強化";
		if (desc) return "術者の腕力、器用さ、耐久力を上昇させる。攻撃回数の上限を 1 増加させる。";
#else
		if (name) return "Building up";
		if (desc) return "Attempts to increases your strength, dexterity and constitusion.";
#endif
		if (cast)
		{
#ifdef JP
			msg_print("身体が強くなった気がした。");
#else
			msg_print("You feel your body is developed more now.");
#endif
		}
		break;

	case 15:
#ifdef JP
		if (name) return "反テレポート結界";
		if (desc) return "視界内のモンスターのテレポートを阻害するバリアを張る。";
#else
		if (name) return "Anti teleport barrier";
		if (desc) return "Obstructs all teleportations by monsters in your sight.";
#endif
		power = plev * 3 / 2;
		if (info) return info_power(power);
		if (cast)
		{
#ifdef JP
			msg_print("テレポートを防ぐ呪いをかけた。");
#else
			msg_print("You feel anyone can not teleport except you.");
#endif
		}
		break;

	/*** 3rd book (16-23) ***/
	case 16:
#ifdef JP
		if (name) return "衝撃のクローク";
		if (desc) return "電気のオーラを身にまとい、動きが速くなる。";
#else
		if (name) return "Cloak of shock";
		if (desc) return "Gives lightning aura and a bonus to speed.";
#endif
		if (cast)
		{
#ifdef JP
			msg_print("体が稲妻のオーラで覆われた。");
#else
			msg_print("You have enveloped by electrical aura!");
#endif
		}
		if (stop)
		{
#ifdef JP
			msg_print("稲妻のオーラが消え去った。");
#else
			msg_print("Electrical aura disappeared.");
#endif
		}
		break;

	case 17:
#ifdef JP
		if (name) return "致命傷の治癒";
		if (desc) return "体力や傷を回復させる。";
#else
		if (name) return "Cure critical wounds";
		if (desc) return "Heals cut and HP greatry.";
#endif
		if (info) return info_heal(4, 10, 0);
		if (cast)
		{
#ifdef JP
			msg_print("気分が良くなってくる。");
#else
			msg_print("You feel better and better.");
#endif
		}
		if (cast || cont)
		{
			hp_player(damroll(4, 10));
			set_stun(0, TRUE);
			set_cut(0, TRUE);
			set_poisoned(0, TRUE);
		}
		break;

	case 18:
#ifdef JP
		if (name) return "呪力封入";
		if (desc) return "魔法の道具に魔力を再充填する。";
#else
		if (name) return "Recharging";
		if (desc) return "Recharges a magic device.";
#endif
		power = plev * 2;
		if (info) return info_power(power);
		if (cast)
		{
			if (!recharge(power)) return NULL;
			add = FALSE;
		}
		break;

	case 19:
#ifdef JP
		if (name) return "死者復活";
		if (desc) return "死体を蘇らせてペットにする。";
#else
		if (name) return "Animate Dead";
		if (desc) return "Raises corpses and skeletons from dead.";
#endif
		if (cast)
		{
#ifdef JP
			msg_print("死者への呼びかけを始めた。");
#else
			msg_print("You start to call deads.!");
#endif
		}
		if (cast || cont)
		{
			animate_dead(0, py, px);
		}
		break;

	case 20:
#ifdef JP
		if (name) return "防具呪縛";
		if (desc) return "装備している防具に呪いをかける。";
#else
		if (name) return "Curse armor";
		if (desc) return "Curse a piece of armour that you wielding.";
#endif
		if (cast)
		{
			int item;
			char *q, *s;
			char o_name[MAX_NLEN];
			object_type *o_ptr;
			u32b f[TR_FLAG_SIZE];

			item_tester_hook = object_is_armour;
#ifdef JP
			q = "どれを呪いますか？";
			s = "防具を装備していない。";
#else
			q = "Which piece of armour do you curse?";
			s = "You wield no piece of armours.";
#endif

			if (!get_item(&item, q, s, (USE_EQUIP))) return FALSE;

			o_ptr = &inventory[item];
			object_desc(o_name, o_ptr, OD_NAME_ONLY);
			object_flags(o_ptr, f);

#ifdef JP
			if (!get_check(format("本当に %s を呪いますか？", o_name))) return FALSE;
#else
			if (!get_check(format("Do you curse %s, really？", o_name))) return FALSE;
#endif

			if (!one_in_(3) &&
				(object_is_artifact(o_ptr) || have_flag(f, TR_BLESSED)))
			{
#ifdef JP
				msg_format("%s は呪いを跳ね返した。", o_name);
#else
				msg_format("%s resists the effect.", o_name);
#endif
				if (one_in_(3))
				{
					if (o_ptr->to_d > 0)
					{
						o_ptr->to_d -= randint1(3) % 2;
						if (o_ptr->to_d < 0) o_ptr->to_d = 0;
					}
					if (o_ptr->to_h > 0)
					{
						o_ptr->to_h -= randint1(3) % 2;
						if (o_ptr->to_h < 0) o_ptr->to_h = 0;
					}
					if (o_ptr->to_a > 0)
					{
						o_ptr->to_a -= randint1(3) % 2;
						if (o_ptr->to_a < 0) o_ptr->to_a = 0;
					}
#ifdef JP
					msg_format("%s は劣化してしまった。", o_name);
#else
					msg_format("Your %s was disenchanted!", o_name);
#endif
				}
			}
			else
			{
				int power = 0;
#ifdef JP
				msg_format("恐怖の暗黒オーラがあなたの%sを包み込んだ！", o_name);
#else
				msg_format("A terrible black aura blasts your %s!", o_name);
#endif
				o_ptr->curse_flags |= (TRC_CURSED);

				if (object_is_artifact(o_ptr) || object_is_ego(o_ptr))
				{

					if (one_in_(3)) o_ptr->curse_flags |= (TRC_HEAVY_CURSE);
					if (one_in_(666))
					{
						o_ptr->curse_flags |= (TRC_TY_CURSE);
						if (one_in_(666)) o_ptr->curse_flags |= (TRC_PERMA_CURSE);

						add_flag(o_ptr->art_flags, TR_AGGRAVATE);
						add_flag(o_ptr->art_flags, TR_RES_POIS);
						add_flag(o_ptr->art_flags, TR_RES_DARK);
						add_flag(o_ptr->art_flags, TR_RES_NETHER);
#ifdef JP
						msg_print("血だ！血だ！血だ！");
#else
						msg_print("Blood, Blood, Blood!");
#endif
						power = 2;
					}
				}

				o_ptr->curse_flags |= get_curse(power, o_ptr);
			}

			p_ptr->update |= (PU_BONUS);
			add = FALSE;
		}
		break;

	case 21:
		if (name) return "Cloak of shadow";
		if (desc) return "Gives aura of shadow.";
		if (cast)
		{
			int slot = equip_find_first(object_is_cloak);
			object_type *o_ptr = NULL;

			if (!slot)
			{
				msg_print("You are not wearing a cloak.");
				return NULL;
			}
			o_ptr = equip_obj(slot);
			if (!object_is_cursed(o_ptr))
			{
				msg_print("Your cloak is not cursed.");
				return NULL;
			}
			else
			{
				msg_print("You have enveloped by shadow aura!");
			}
		}
		if (cont)
		{
			int slot = equip_find_first(object_is_cloak);
			if (!slot || !object_is_cursed(equip_obj(slot)))
			{
				do_spell(REALM_HEX, spell, SPELL_STOP);
				p_ptr->magic_num1[0] &= ~(1L << spell);
				p_ptr->magic_num2[0]--;
				if (!p_ptr->magic_num2[0]) set_action(ACTION_NONE);
			}
		}
		if (stop)
		{
			msg_print("Shadow aura disappeared.");
		}
		break;

	case 22:
#ifdef JP
		if (name) return "苦痛を魔力に";
		if (desc) return "視界内のモンスターに精神ダメージ与え、魔力を吸い取る。";
#else
		if (name) return "Pains to mana";
		if (desc) return "Deals psychic damages to all monsters in sight, and drains some mana.";
#endif
		power = plev * 3 / 2;
		if (info) return info_damage(1, power, 0);
		if (cast || cont)
		{
			project_hack(GF_PSI_DRAIN, randint1(power));
		}
		break;

	case 23:
#ifdef JP
		if (name) return "目には目を";
		if (desc) return "打撃や魔法で受けたダメージを、攻撃元のモンスターにも与える。";
#else
		if (name) return "Eye for an eye";
		if (desc) return "Returns same damage which you got to the monster which damaged you.";
#endif
		if (cast)
		{
#ifdef JP
			msg_print("復讐したい欲望にかられた。");
#else
			msg_print("You wish strongly you want to revenge anything.");
#endif
		}
		break;

	/*** 4th book (24-31) ***/
	case 24:
#ifdef JP
		if (name) return "反増殖結界";
		if (desc) return "その階の増殖するモンスターの増殖を阻止する。";
#else
		if (name) return "Anti multiply barrier";
		if (desc) return "Obstructs all multiplying by monsters in entire floor.";
#endif
		if (cast)
		{
#ifdef JP
			msg_print("増殖を阻止する呪いをかけた。");
#else
			msg_print("You feel anyone can not already multiply.");
#endif
		}
		break;

	case 25:
#ifdef JP
		if (name) return "生命力復活";
		if (desc) return "経験値を徐々に復活し、減少した能力値を回復させる。";
#else
		if (name) return "Restore life";
		if (desc) return "Restores life energy and status.";
#endif
		if (cast)
		{
#ifdef JP
			msg_print("生命力が戻り始めた。");
#else
			msg_print("You feel your life energy starting to return.");
#endif
		}
		if (cast || cont)
		{
			bool flag = FALSE;
			int d = (p_ptr->max_exp - p_ptr->exp);
			int r = (p_ptr->exp / 20);
			int i;

			if (d > 0)
			{
				if (d < r)
					p_ptr->exp = p_ptr->max_exp;
				else
					p_ptr->exp += r;

				/* Check the experience */
				check_experience();

				flag = TRUE;
			}
			for (i = A_STR; i < 6; i ++)
			{
				if (p_ptr->stat_cur[i] < p_ptr->stat_max[i])
				{
					if (p_ptr->stat_cur[i] < 18)
						p_ptr->stat_cur[i]++;
					else
						p_ptr->stat_cur[i] += 10;

					if (p_ptr->stat_cur[i] > p_ptr->stat_max[i])
						p_ptr->stat_cur[i] = p_ptr->stat_max[i];

					/* Recalculate bonuses */
					p_ptr->update |= (PU_BONUS);

					flag = TRUE;
				}
			}

			if (!flag)
			{
#ifdef JP
				msg_format("%sの呪文の詠唱をやめた。", do_spell(REALM_HEX, HEX_RESTORE, SPELL_NAME));
#else
				msg_format("Finish casting '%^s'.", do_spell(REALM_HEX, HEX_RESTORE, SPELL_NAME));
#endif
				p_ptr->magic_num1[0] &= ~(1L << HEX_RESTORE);
				if (cont) p_ptr->magic_num2[0]--;
				if (p_ptr->magic_num2) p_ptr->action = ACTION_NONE;

				/* Redraw status */
				p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);
				p_ptr->redraw |= (PR_EXTRA);

				return "";
			}
		}
		break;

	case 26:
#ifdef JP
		if (name) return "呪力吸収";
		if (desc) return "呪われた武器の呪いを吸収して魔力を回復する。";
#else
		if (name) return "Drain curse power";
		if (desc) return "Drains curse on your weapon and heals SP a little.";
#endif
		if (cast)
		{
			int item;
			char *s, *q;
			u32b f[TR_FLAG_SIZE];
			object_type *o_ptr;

			item_tester_hook = item_tester_hook_cursed;
#ifdef JP
			q = "どの装備品から吸収しますか？";
			s = "呪われたアイテムを装備していない。";
#else
			q = "Which cursed equipment do you drain mana from?";
			s = "You have no cursed equipment.";
#endif

			if (!get_item(&item, q, s, (USE_EQUIP))) return FALSE;

			o_ptr = &inventory[item];
			object_flags(o_ptr, f);

			p_ptr->csp += (p_ptr->lev / 5) + randint1(p_ptr->lev / 5);
			if (have_flag(f, TR_TY_CURSE) || (o_ptr->curse_flags & TRC_TY_CURSE)) p_ptr->csp += randint1(5);
			if (p_ptr->csp > p_ptr->msp) p_ptr->csp = p_ptr->msp;

			if (o_ptr->curse_flags & TRC_PERMA_CURSE)
			{
				/* Nothing */
			}
			else if (o_ptr->curse_flags & TRC_HEAVY_CURSE)
			{
				if (one_in_(7))
				{
#ifdef JP
					msg_print("呪いを全て吸い取った。");
#else
					msg_print("Heavy curse vanished away.");
#endif
					o_ptr->curse_flags = 0L;
				}
			}
			else if ((o_ptr->curse_flags & (TRC_CURSED)) && one_in_(3))
			{
#ifdef JP
				msg_print("呪いを全て吸い取った。");
#else
				msg_print("Curse vanished away.");
#endif
				o_ptr->curse_flags = 0L;
			}

			add = FALSE;
		}
		break;

	case 27:
		if (name) return "Swords to vampires";
		if (desc) return "Gives vampiric ability to your weapon.";
		if (cast)
		{
			if (p_ptr->weapon_ct > 1)
				msg_print("Your weapons want more blood now.");
			else
				msg_print("Your weapon wants more blood now.");
		}
		if (stop)
			msg_format("Thirsty of weapon%s disappeared.", (p_ptr->weapon_ct <= 1) ? "" : "s");
		break;

	case 28:
#ifdef JP
		if (name) return "朦朧の言葉";
		if (desc) return "視界内のモンスターを朦朧とさせる。";
#else
		if (name) return "Word of stun";
		if (desc) return "Stuns all monsters in your sight.";
#endif
		power = plev * 4;
		if (info) return info_power(power);
		if (cast || cont)
		{
			stun_monsters(power);
		}
		break;

	case 29:
#ifdef JP
		if (name) return "影移動";
		if (desc) return "モンスターの隣のマスに瞬間移動する。";
#else
		if (name) return "Moving into shadow";
		if (desc) return "Teleports you close to a monster.";
#endif
		if (cast)
		{
			int i, y, x, dir;
			bool flag;

			for (i = 0; i < 3; i++)
			{
				if (!tgt_pt(&x, &y)) return FALSE;

				flag = FALSE;

				for (dir = 0; dir < 8; dir++)
				{
					int dy = y + ddy_ddd[dir];
					int dx = x + ddx_ddd[dir];
					if (dir == 5) continue;
					if(cave[dy][dx].m_idx) flag = TRUE;
				}

				if (!cave_empty_bold(y, x) || (cave[y][x].info & CAVE_ICKY) ||
					(distance(y, x, py, px) > plev + 2))
				{
#ifdef JP
					msg_print("そこには移動できない。");
#else
					msg_print("Can not teleport to there.");
#endif
					continue;
				}
				break;
			}

			if (flag && randint0(plev * plev / 2))
			{
				teleport_player_to(y, x, 0L);
			}
			else
			{
#ifdef JP
				msg_print("おっと！");
#else
				msg_print("Oops!");
#endif
				if (mut_present(MUT_ASTRAL_GUIDE))
					energy_use = 30;
				teleport_player(30, 0L);
			}

			add = FALSE;
		}
		break;

	case 30:
#ifdef JP
		if (name) return "反魔法結界";
		if (desc) return "視界内のモンスターの魔法を阻害するバリアを張る。";
#else
		if (name) return "Anti magic barrier";
		if (desc) return "Obstructs all magic spell of monsters in your sight.";
#endif
		power = plev * 3 / 2;
		if (info) return info_power(power);
		if (cast)
		{
#ifdef JP
			msg_print("魔法を防ぐ呪いをかけた。");
#else
			msg_print("You feel anyone can not cast spells except you.");
#endif
		}
		break;

	case 31:
#ifdef JP
		if (name) return "復讐の宣告";
		if (desc) return "数ターン後にそれまで受けたダメージに応じた威力の地獄の劫火の弾を放つ。";
#else
		if (name) return "Revenge sentence";
		if (desc) return "Fires  a ball of hell fire to try revenging after few turns.";
#endif
		power = p_ptr->magic_num1[2];
		if (info) return info_damage(0, 0, power);
		if (cast)
		{
			int r;
			int a = 3 - (p_ptr->pspeed - 100) / 10;
			r = 1 + randint1(2) + MAX(0, MIN(3, a));

			if (p_ptr->magic_num2[2] > 0)
			{
#ifdef JP
				msg_print("すでに復讐は宣告済みだ。");
#else
				msg_print("You already pronounced your revenge.");
#endif
				return NULL;
			}

			p_ptr->magic_num2[1] = 2;
			p_ptr->magic_num2[2] = r;
#ifdef JP
			msg_format("あなたは復讐を宣告した。あと %d ターン。", r);
#else
			msg_format("You pronounce your revenge. %d turns left.", r);
#endif
			add = FALSE;
		}
		if (cont)
		{
			p_ptr->magic_num2[2]--;

			if (p_ptr->magic_num2[2] <= 0)
			{
				int dir;

				if (power)
				{
					command_dir = 0;

					do
					{
#ifdef JP
						msg_print("復讐の時だ！");
#else
						msg_print("Time to revenge!");
#endif
					}
					while (!get_aim_dir(&dir));

					fire_ball(GF_HELL_FIRE, dir, power, 1);

					if (p_ptr->wizard)
					{
#ifdef JP
						msg_format("%d点のダメージを返した。", power);
#else
						msg_format("You return %d damages.", power);
#endif
					}
				}
				else
				{
#ifdef JP
					msg_print("復讐する気が失せた。");
#else
					msg_print("You are not a mood to revenge.");
#endif
				}
				p_ptr->magic_num1[2] = 0;
			}
		}
		break;
	}

	/* start casting */
	if ((cast) && (add))
	{
		/* add spell */
		p_ptr->magic_num1[0] |= 1L << (spell);
		p_ptr->magic_num2[0]++;

		if (p_ptr->action != ACTION_SPELL) set_action(ACTION_SPELL);
	}

	/* Redraw status */
	if (!info)
	{
		p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);
		p_ptr->redraw |= (PR_EXTRA | PR_HP | PR_MANA);
	}

	return "";
}

static bool _necro_check_touch(void)
{
	int slot;
	if (p_ptr->afraid)
	{
		msg_print("You are too scared to do that!");
		return FALSE;
	}
	if (equip_find_first(object_is_melee_weapon))
	{
		msg_print("You can't touch while wielding a weapon.");
		return FALSE;
	}

	slot = equip_find_object(TV_GLOVES, SV_ANY);
	if (slot && equip_obj(slot)->name1 != ART_HAND_OF_VECNA)
	{
		msg_print("You can't touch while wielding gloves.");
		return FALSE;
	}
	return TRUE;
}

static cptr _necro_info_damage(int dice, int sides, int base)
{
	if (equip_find_artifact(ART_HAND_OF_VECNA))
	{
		dice *= 2;
		base *= 2;
	}
	return info_damage(dice, spell_power(sides), spell_power(base));
}

static int _necro_damroll(int dice, int sides, int base)
{
	if (equip_find_artifact(ART_HAND_OF_VECNA))
	{
		dice *= 2;
		base *= 2;
	}
	return damroll(dice, spell_power(sides)) + spell_power(base);
}

void on_p_hit_m(int m_idx)
{
	if (p_ptr->special_attack & ATTACK_CONFUSE)
	{
		monster_type *m_ptr = &m_list[m_idx];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];
		char          m_name[MAX_NLEN];

		monster_desc(m_name, m_ptr, 0);

		p_ptr->special_attack &= ~(ATTACK_CONFUSE);
		msg_print(T("Your hands stop glowing.", "手の輝きがなくなった。"));
		p_ptr->redraw |= (PR_STATUS);

		if (r_ptr->flags3 & RF3_NO_CONF)
		{
			if (is_original_ap_and_seen(m_ptr)) r_ptr->r_flags3 |= RF3_NO_CONF;
			msg_format(T("%^s is unaffected.", "%^sには効果がなかった。"), m_name);
		}
		else if (randint0(100) < r_ptr->level)
		{
			msg_format(T("%^s is unaffected.", "%^sには効果がなかった。"), m_name);
		}
		else
		{
			msg_format(T("%^s appears confused.", "%^sは混乱したようだ。"), m_name);
			(void)set_monster_confused(m_idx, MON_CONFUSED(m_ptr) + 10 + randint0(p_ptr->lev) / 5);
		}
	}
}

static bool _necro_do_touch(int type, int dice, int sides, int base)
{
	int x, y;
	int dir;
	int m_idx = 0;

	if (!_necro_check_touch()) return FALSE;

	/* For ergonomics sake, use currently targeted monster.  This allows
	   a macro of \e*tmaa or similar to pick an adjacent foe, while
	   \emaa*t won't work, since get_rep_dir2() won't allow a target. */
	if (use_old_target && target_okay())
	{
		y = target_row;
		x = target_col;
		m_idx = cave[y][x].m_idx;
		if (m_idx)
		{
			if (m_list[m_idx].cdis > 1)
				m_idx = 0;
			else
				dir = 5; /* Hack so that fire_ball() works correctly */
		}
	}

	if (!m_idx)
	{
		if (!get_rep_dir2(&dir)) return FALSE;
		if (dir == 5) return FALSE;

		y = py + ddy[dir];
		x = px + ddx[dir];
		m_idx = cave[y][x].m_idx;

		if (!m_idx)
		{
			msg_print("There is no monster there.");
			return FALSE;
		}

	}

	if (m_idx)
	{
		int dam;
		monster_type *m_ptr = &m_list[m_idx];

		if (!is_hostile(m_ptr) &&
			!(p_ptr->stun || p_ptr->confused || p_ptr->image ||
			IS_SHERO() || !m_ptr->ml))
		{
			if (!get_check("Really hit it? "))
				return FALSE;
		}

		dam = _necro_damroll(dice, sides, base);
		on_p_hit_m(m_idx);
		touch_zap_player(m_idx);
		if (fire_ball(type, dir, dam, 0))
		{
			if (type == GF_OLD_DRAIN)
				hp_player(dam);
		}
	}
	return TRUE;
}

static void _necro_do_summon(int what, int num, bool fail)
{
	int x = px;
	int y = py;

	if (fail) /* Failing spells should not be insta-death ... */
		num = MAX(1, num/4);
	else
		num = spell_power(num);

	if (!fail && use_old_target && target_okay() && los(py, px, target_row, target_col) && !one_in_(3))
	{
		y = target_row;
		x = target_col;
	}
	if (trump_summoning(num, !fail, y, x, 0, what, PM_ALLOW_UNIQUE))
	{
		if (fail)
		{
			if (num == 1)
				msg_print("The summoned monster gets angry!");
			else
				msg_print("The summoned monsters get angry!");
		}
	}
}

bool repose_of_the_dead = FALSE;

static cptr do_necromancy_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
	bool fail = (mode == SPELL_FAIL) ? TRUE : FALSE;
	bool spoil = (mode == SPELL_SPOIL_DESC) ? TRUE : FALSE;

	int plev = p_ptr->lev;

	switch (spell)
	{
	/* Stench of Death */
	case 0:
		if (name) return "Cold Touch";
		if (desc) return "Damage an adjacent monster with a chilling touch.";
		if (spoil) return "Touches an adjacent monster for 2d6+L cold damage.";
		if (info) return _necro_info_damage(2, 6, plev);
		if (cast && !_necro_do_touch(GF_COLD, 2, 6, plev)) return NULL;
		break;

	case 1:
		if (name) return "Summon Rat";
		if (desc) return "Summons a rat to feast on the dead!";
		if (spoil) return "Summons a single rat.";
		if (cast || fail) _necro_do_summon(SUMMON_RAT, 1, fail);
		break;

	case 2:
		if (name) return "Detect Life";
		if (desc) return "Detects all living monsters in your vicinity.";
		if (info) return info_radius(DETECT_RAD_DEFAULT);
		if (cast) detect_monsters_living(DETECT_RAD_DEFAULT, "You sense the presence of life around you.");
		break;

	case 3:
		if (name) return "Detect Unlife";
		if (desc) return "Detects all nonliving monsters in your vicinity.";
		if (info) return info_radius(DETECT_RAD_DEFAULT);
		if (cast) detect_monsters_nonliving(DETECT_RAD_DEFAULT);
		break;

	case 4:
		if (name) return "Poison Touch";
		if (desc) return "Damage an adjacent monster with a venomous touch.";
		if (spoil) return "Touches an adjacent monster for 4d6+L poison damage.";
		if (info) return _necro_info_damage(4, 6, plev);
		if (cast && !_necro_do_touch(GF_POIS, 4, 6, plev)) return NULL;
		break;

	case 5:
		if (name) return "Summon Bats";
		if (desc) return "Summons bats to feast on the living!";
		if (spoil) return "Summons 3 + (1d3-1) bats.";
		if (cast || fail) _necro_do_summon(SUMMON_BAT, 3 + randint0(3), fail);
		break;

	case 6:
		if (name) return "Eldritch Howl";
		if (desc) return "Emit a terrifying howl.";
		if (spoil) return "Emits a terrifying howl.  All monsters in line of sight are stricken with fear if they miss a saving throw. If they miss two saving throws, they are frozen with terror.";
		if (cast) project_hack(GF_ELDRITCH_HOWL, spell_power(plev * 3));
		break;

	case 7:
		if (name) return "Black Touch";
		if (desc) return "Damage an adjacent monster with a dark touch.";
		if (spoil) return "Touches an adjacent monster for 6d6+3L/2 darkness damage.";
		if (info) return _necro_info_damage(6, 6, plev * 3 / 2);
		if (cast && !_necro_do_touch(GF_DARK, 6, 6, plev * 3 / 2)) return NULL;
		break;

	/* Sepulchral Ways */
	case 8:
		if (name) return "Summon Wolves";
		if (desc) return "Summons wolves to feast on the living!";
		if (spoil) return "Summons 3 + (1d3-1) wolves.";
		if (cast || fail) _necro_do_summon(SUMMON_WOLF, 3 + randint0(3), fail);
		break;

	case 9:
		if (name) return "Black Cloak";
		if (desc) return "You become shrouded in darkness.";
		if (spoil) return "Player gains superstealth, nocturnal vision, and decreased light radius for L+dL turns.";
		if (cast) 
		{
			if (p_ptr->tim_superstealth)
			{
				msg_print("You are already moving in the shadows.");
				return NULL;
			}
			set_tim_superstealth(spell_power(randint1(p_ptr->lev) + p_ptr->lev), FALSE);
		}
		break;

	case 10:
		if (name) return "Undead Sight";
		if (desc) return "Learn about your nearby surroundings by communing with the dead.";
		if (spoil) return "Maps nearby area.";
		if (info) return info_radius(DETECT_RAD_MAP);
		if (cast)
		{
			map_area(DETECT_RAD_MAP);
			detect_traps(DETECT_RAD_DEFAULT, TRUE);
			detect_doors(DETECT_RAD_DEFAULT);
			detect_stairs(DETECT_RAD_DEFAULT);
		}
		break;

	case 11:
		if (name) return "Undead Lore";
		if (desc) return "Ask the dead to examine an object for you.";
		if (spoil) return "Identifies a chosen object.";
		if (cast) ident_spell(NULL);
		break;

	case 12:
		if (name) return "Repelling Touch";
		if (desc) return "Conjure a foul wind to blow an adjacent monster away.";
		if (spoil) return "An adjacent monster is blown back by up to 10 squares, but takes no physical damage.";
    
		if (cast)
		{
			int y, x, dir;

			if (!_necro_check_touch()) return NULL;
			if (!get_rep_dir2(&dir)) return NULL;
			if (dir == 5) return NULL;

			y = py + ddy[dir];
			x = px + ddx[dir];

			if (!cave[y][x].m_idx)
			{
				msg_print("There is no monster.");
				return NULL;
			}
			else
			{
				int i;
				int ty = y, tx = x;
				int oy = y, ox = x;
				int m_idx = cave[y][x].m_idx;
				monster_type *m_ptr = &m_list[m_idx];
				char m_name[80];
	
				monster_desc(m_name, m_ptr, 0);
				touch_zap_player(cave[y][x].m_idx);	
	
				for (i = 0; i < 10; i++)
				{
					y += ddy[dir];
					x += ddx[dir];
					if (cave_empty_bold(y, x))
					{
						ty = y;
						tx = x;
					}
					else break;
				}
				if ((ty != oy) || (tx != ox))
				{
					msg_format("A foul wind blows %s away!", m_name);
					cave[oy][ox].m_idx = 0;
					cave[ty][tx].m_idx = m_idx;
					m_ptr->fy = ty;
					m_ptr->fx = tx;
	
					update_mon(m_idx, TRUE);
					lite_spot(oy, ox);
					lite_spot(ty, tx);
	
					if (r_info[m_ptr->r_idx].flags7 & (RF7_LITE_MASK | RF7_DARK_MASK))
						p_ptr->update |= (PU_MON_LITE);
				}
			}
		}
		break;

	case 13:
		if (name) return "Vampiric Touch";
		if (desc) return "Steal life from an adjacent foe.";
		if (spoil) return "Touches an adjacent monster for 4L damage. Player regains an equal amount of hp, but non-living monsters resist.";
		if (info) return _necro_info_damage(0, 0, plev * 4);
		if (cast && !_necro_do_touch(GF_OLD_DRAIN, 0, 0, plev * 4)) return NULL;
		break;

	case 14:
		if (name) return "Dread of Night";
		if (desc) return "Summons Dread to do your bidding.  Beware of failure!";
		if (spoil) return "Summons 5 + (1d5-1) Dread.";
		if (cast || fail) _necro_do_summon(SUMMON_DREAD, 5 + randint0(5), fail);
		break;

	case 15:
		if (name) return "Entomb";
		if (desc) return "Entombs chosen foe.";
		if (spoil) return "Targetted monster is surrounded by rubble or walls.";
		if (cast)
		{
			int dir; 
			if (!get_aim_dir(&dir)) return NULL;
			fire_ball_hide(GF_ENTOMB, dir, p_ptr->lev, 0);
			p_ptr->update |= (PU_FLOW);
			p_ptr->redraw |= (PR_MAP);
		}
		break;

	/* Return of the Dead */
	case 16:
		if (name) return "Summon Zombies";
		if (desc) return "The dead are back and hungry for brains!";
		if (spoil) return "Summons 10 + (1d10-1) zombies.";
		if (cast || fail) _necro_do_summon(SUMMON_ZOMBIE, 10 + randint0(10), fail);
		break;

	case 17:
		if (name) return "Summon Skeletons";
		if (desc) return "Summon skeletal assistance.";
		if (spoil) return "Summons 2 + (1d2-1) skeletons.";
		if (cast || fail) _necro_do_summon(SUMMON_SKELETON, 2 + randint0(2), fail);
		break;

	case 18:
		if (name) return "Summon Ghosts";
		if (desc) return "Recall the spirits of slain warriors for unholy servitude.";
		if (spoil) return "Summons 2 + (1d2-1) ghosts.";
		if (cast || fail) _necro_do_summon(SUMMON_GHOST, 2 + randint0(2), fail);
		break;

	case 19:
		if (name) return "Summon Vampires";
		if (desc) return "Its time to command the commanders!";
		if (spoil) return "Summons 2 + (1d2-1) vampires.";
		if (cast || fail) _necro_do_summon(SUMMON_VAMPIRE, 2 + randint0(2), fail);
		break;

	case 20:
		if (name) return "Summon Wraiths";
		if (desc) return "Summon wights and wraiths to do your bidding.";
		if (spoil) return "Summons 2 + (1d2-1) wights.";
		if (cast || fail) _necro_do_summon(SUMMON_WIGHT, 2 + randint0(2), fail);
		break;

	case 21:
		if (name) return "Summon Liches";
		if (desc) return "Call forth former necromancers.";
		if (spoil) return "Summons 1 + (1d2-1) liches.";
		if (cast || fail) _necro_do_summon(SUMMON_LICH, 1 + randint0(2), fail);
		break;

	case 22:
		if (name) return "Unholy Word";
		if (desc) return "Utter an unspeakable word.  The morale of your visible evil pets is temporarily boosted and they will serve you with renewed enthusiasm.";
		if (spoil) return "All evil pets in the player's line of sight are no longer stunned, confused or afraid, get healed by 6L hp, and get temporarily hasted for 100 rounds.";
		if (cast) project_hack(GF_UNHOLY_WORD, p_ptr->lev * 6);
		break;

	case 23:
		if (name) return "Lost Cause";
		if (desc) return "Make a last ditch Kamikaze effort for victory!";
		if (spoil) return "All of the player's pets are exploded damaging any nearby monsters, including the player.";
		if (cast) discharge_minion();
		break;

	/* Necromatic Tome */
	case 24:
		if (name) return "Draining Touch";
		if (desc) return "Steal mana from an adjacent foe.";
		if (spoil) return "Touches an adjacent monster for 5d5+L/2 damage. Player regains an equal amount of sp, but non-magical monsters resist.";
		if (info) return _necro_info_damage(5, 5, plev/2);
		if (cast && !_necro_do_touch(GF_DRAINING_TOUCH, 5, 5, plev/2)) return NULL;
		break;

	case 25:
		if (name) return "Unhallow Ground";
		if (desc) return "Makes the current square unholy.";
		if (spoil) return "Creates an unholy glyph which monsters may not pass. Monsters may not attack the player who stands on the glyph, though there is a small chance that monsters may break the glyph every round.";
		if (cast) warding_glyph(); /* TODO: Add new cave feature! */
		break;

	case 26:
	{
		int base = spell_power(20);
		if (name) return "Shield of the Dead";
		if (desc) return "Grants temporary protection";
		if (spoil) return "Player gains nether, cold and poison resistance for L+dL rounds.";
		if (info) return info_duration(base, base);
		if (cast)
		{
			set_tim_res_nether(randint1(base) + base, FALSE);
			set_oppose_pois(randint1(base) + base, FALSE);
			set_oppose_cold(randint1(base) + base, FALSE);
			set_shield(randint1(base) + base, FALSE);
		}
		break;
	}
	case 27:
		if (name) return "Rending Touch";
		if (desc) return "Damage an adjacent monster with a disintegrating touch.";
		if (spoil) return "Touches an adjacent monster for 20d20+L disintegration damage.";
		if (info) return _necro_info_damage(20, 20, p_ptr->lev);
		if (cast && !_necro_do_touch(GF_DISINTEGRATE, 20, 20, p_ptr->lev)) return NULL;
		break;

	case 28:
		if (name) return "Repose of the Dead";
		if (desc) return "Sleep the sleep of the dead for a few rounds, during which time nothing can awaken you, except perhaps death.  When (if?) you wake up, you will be thoroughly refreshed!";
		if (spoil) return "Player is paralyzed for 4 + 1d4 rounds. Upon awakening, stats and life are restored.";
		if (cast)
		{
			if (!get_check("You will enter a deep slumber. Are you sure?")) return NULL;
			repose_of_the_dead = TRUE;
			set_paralyzed(4 + randint1(4), FALSE);
		}
		break;

	case 29:
		if (name) return "Sepulchral Wind";
		if (desc) return "You call forth the wind of the dead.  All nearby monsters are blown away!";
		{
			int power = spell_power(plev * 4);
			if (info) return info_power(power);
			if (cast) banish_monsters(power);
		}
		break;

	case 30:
		if (name) return "Deadly Touch";
		if (desc) return "Attempt to kill an adjacent monster.";
		if (cast && !_necro_do_touch(GF_DEATH_TOUCH, 0, 0, p_ptr->lev * 200)) return NULL;
		break;

	case 31:
		if (name) return "Necromancy";
		if (desc) return "Bridge the world of the living with the world of the dead!  Vast hordes of undead will come forth to serve the one true necromancer!";
		if (cast)
		{
			int i;
			int base = 25;
			int sp_sides = 20 + plev;
			int sp_base = plev;
			int power = spell_power(plev);

			power += randint1(power);

			for (i = 0; i < 18; i++)
			{
				int attempt = 10;
				int my, mx, what;

				while (attempt--)
				{
					scatter(&my, &mx, py, px, 4, 0);

					/* Require empty grids */
					if (cave_empty_bold2(my, mx)) break;
				}
				if (attempt < 0) continue;
				switch (randint1(4))
				{
				case 1: what = SUMMON_LICH; break;
				case 2: what = SUMMON_WIGHT; break;
				case 3: what = SUMMON_VAMPIRE; break;
				case 4: what = SUMMON_GHOST; break;
				}
				summon_specific(-1, my, mx, power, what, (PM_ALLOW_GROUP | PM_FORCE_PET | PM_HASTE));
			}
			set_fast(randint1(sp_sides) + sp_base, FALSE);
		}
		break;

	}

	return "";
}

static cptr _rogue_pick_pocket(void)
{
	int y, x, m_idx, dir;
	monster_type *m_ptr;
	monster_race *r_ptr;
	char          m_name[MAX_NLEN];
	char	      o_name[MAX_NLEN];

	if (!get_rep_dir2(&dir)) return NULL;
	if (dir == 5) return NULL;

	y = py + ddy[dir];
	x = px + ddx[dir];

	if (!cave[y][x].m_idx)
	{
		msg_print("There is no monster.");
		return NULL;
	}

	m_idx = cave[y][x].m_idx;
	m_ptr = &m_list[m_idx];
	r_ptr = &r_info[m_ptr->r_idx];

	if (!m_ptr->ml || p_ptr->image) /* Can't see it, so can't steal! */
	{
		msg_print("There is no monster.");
		return NULL;
	}

	monster_desc(m_name, m_ptr, 0);

	if ( !mon_save_p(m_ptr->r_idx, A_DEX) 
	  || (MON_CSLEEP(m_ptr) && !mon_save_p(m_ptr->r_idx, A_DEX)))
	{
		object_type loot = {0};

		if (m_ptr->hold_o_idx && one_in_(2))
		{
			object_copy(&loot, &o_list[m_ptr->hold_o_idx]);
			delete_object_idx(m_ptr->hold_o_idx);
			loot.held_m_idx = 0;
		}
		else if (m_ptr->drop_ct > m_ptr->stolen_ct)
		{
			if (get_monster_drop(m_idx, &loot))
			{
				m_ptr->stolen_ct++;
				if (r_ptr->flags1 & RF1_UNIQUE)
					r_ptr->stolen_ct++;
			}
		}

		if (!loot.k_idx)
		{
			msg_print("There is nothing to steal!");
		}
		else 
		{
			object_desc(o_name, &loot, 0);
			if (mon_save_p(m_ptr->r_idx, A_DEX))
			{
				msg_format("Oops! You drop %s.", o_name);
				drop_near(&loot, -1, y, x);
			}
			else if (loot.tval == TV_GOLD)
			{
				msg_format("You steal %d gold pieces worth of %s.", (int)loot.pval, o_name);
				sound(SOUND_SELL);
				p_ptr->au += loot.pval;
				p_ptr->redraw |= (PR_GOLD);
				p_ptr->window |= (PW_PLAYER);
			}
			else if (!inven_carry_okay(&loot))
			{
				msg_format("You have no room for %s.", o_name);
				drop_near(&loot, -1, y, x);
			}
			else
			{
				int slot = inven_carry(&loot);
				msg_format("You steal %s (%c).", o_name, index_to_label(slot));
			}
		}

		if ((r_ptr->flags1 & RF1_UNIQUE) || mon_save_p(m_ptr->r_idx, A_DEX))
		{
			set_monster_csleep(m_idx, 0);					
			if ( allow_ticked_off(r_ptr) 
			  && ((r_ptr->flags1 & RF1_UNIQUE) || mon_save_p(m_ptr->r_idx, A_DEX)) )
			{
				msg_format("%^s wakes up and looks very mad!", m_name);
				m_ptr->smart |= SM_TICKED_OFF;
			}
			else
				msg_format("%^s wakes up.", m_name);
		}

		if (loot.k_idx)
		{
			if (mon_save_p(m_ptr->r_idx, A_DEX))
				msg_print("You fail to run away!");
			else
			{
				if (p_ptr->lev < 35 || get_check("Run away?"))
					teleport_player(25 + p_ptr->lev/2, 0L);
			}
		}
	}
	else if (MON_CSLEEP(m_ptr))
	{
		set_monster_csleep(m_idx, 0);			
		if (allow_ticked_off(r_ptr))
		{
			msg_format("Failed! %^s wakes up and looks very mad!", m_name);
			m_ptr->smart |= SM_TICKED_OFF;
		}
		else
			msg_format("Failed! %^s wakes up.", m_name);
	}
	else if (allow_ticked_off(r_ptr))
	{
		msg_format("Failed! %^s looks very mad!", m_name);
		m_ptr->smart |= SM_TICKED_OFF;
	}
	else
	{
		msg_print("Failed!");
	}

	if (is_friendly(m_ptr) || is_pet(m_ptr))
	{
		msg_format("%^s suddenly becomes hostile!", m_name);
		set_hostile(m_ptr);
	}
	return "";
}

static cptr _rogue_negotiate(void)
{
	int           m_idx = 0;
	monster_type *m_ptr;
	monster_race *r_ptr;
	char          m_name[MAX_NLEN];

	if (target_set(TARGET_MARK))
	{
		msg_flag = FALSE; /* Bug ... we get an extra -more- prompt after target_set() ... */
		if (target_who > 0)
			m_idx = target_who;
		else
			m_idx = cave[target_row][target_col].m_idx;
	}

	if (!m_idx)
	{
		msg_print("There is no monster.");
		return NULL;
	}

	m_ptr = &m_list[m_idx];
	r_ptr = &r_info[m_ptr->r_idx];

	if (!m_ptr->ml || p_ptr->image)
	{
		msg_print("There is no monster.");
		return NULL;
	}

	monster_desc(m_name, m_ptr, 0);

	if (is_pet(m_ptr) || is_friendly(m_ptr))
	{
		msg_format("%^s is already in your services.", m_name);
		return NULL;
	}

	set_monster_csleep(m_idx, 0);

	if ((r_ptr->flags2 & RF2_THIEF) && is_original_ap_and_seen(m_ptr))
		r_ptr->r_flags2 |= RF2_THIEF;

	if (!(r_ptr->flags2 & RF2_THIEF))
	{
		msg_format("%^s is not open to any sort of deal!", m_name);
	}
	else if (!mon_save_p(m_ptr->r_idx, A_CHR))
	{
		int cost = 10 + r_ptr->level * 100;

		if (r_ptr->flags1 & RF1_UNIQUE)
			cost *= 10;

		if (p_ptr->au >= cost)
		{
			msg_format("%^s says 'My services will cost you %d gold pieces.'", m_name, cost);

			if (get_check("Do you pay?"))
			{
				sound(SOUND_SELL);
				p_ptr->au -= cost;
				p_ptr->redraw |= PR_GOLD;
				p_ptr->window |= PW_PLAYER;

				if (mon_save_p(m_ptr->r_idx, A_CHR))
				{
					msg_format("%^s says 'Fool! Never trust a thief!'", m_name);
					m_ptr->smart |= SM_TICKED_OFF;
				}
				else
				{
					msg_format("%^s says 'Deal!'", m_name);
					if (!(r_ptr->flags1 & RF1_UNIQUE) && !mon_save_p(m_ptr->r_idx, A_CHR))
						set_pet(m_ptr);
					else
						set_friendly(m_ptr);					
				}
			}
			else
			{
				msg_format("%^s says 'Scoundrel!'", m_name);
				m_ptr->smart |= SM_TICKED_OFF;
			}
		}
		else
		{
			msg_format("%^s says 'Hah! You can't afford my help!", m_name);
		}
	}
	else
	{
		msg_format("%^s is insulted you would ask such a question!", m_name);
		m_ptr->smart |= SM_TICKED_OFF;
	}
	return "";
}


static cptr do_burglary_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
	bool fail = (mode == SPELL_FAIL) ? TRUE : FALSE;
	bool spoil = (mode == SPELL_SPOIL_DESC) ? TRUE : FALSE;

	int plev = p_ptr->lev;
	int rad = DETECT_RAD_DEFAULT;
	int dir;

	if (plev >= 45)
		rad = DETECT_RAD_ALL;
	else
		rad += plev;

	switch (spell)
	{
	/* Burglar's Handbook */
	case 0:
		if (name) return "Detect Traps";
		if (desc) return "Detects nearby traps.";
		if (info) return info_radius(rad);
		if (cast)
			detect_traps(rad, TRUE);
		break;

	case 1:
		if (name) return "Disarm Traps";
		if (desc) return "Fires a beam which disarms traps.";
    
		if (cast)
		{
			if (!get_aim_dir(&dir)) return NULL;
			disarm_trap(dir);
		}
		break;

	case 2:
		if (name) return "Detect Treasure";
		if (desc) return "Detects all treasures in your vicinity.";
		if (info) return info_radius(rad);

		if (cast)
		{
			detect_treasure(rad);
			detect_objects_gold(rad);
		}
		break;

	case 3:
		if (name) return "Detect Objects";
		if (desc) return "Detects all items in your vicinity.";
		if (info) return info_radius(rad);

		if (cast)
			detect_objects_normal(rad);
		break;

	case 4:
		if (name) return "See in the Dark";
		if (desc) return "Gives infravision for a while."; 
		{
			int base = spell_power(100);

			if (info) return info_duration(base, base);

			if (cast)
				set_tim_infra(base + randint1(base), FALSE);
		}
		break;

	case 5:
		if (name) return "Tread Softly";
		if (desc) return "Grants enhanced stealth for a bit."; 
		{
			int base = spell_power(50);

			if (info) return info_duration(base, base);
			if (cast)
				set_tim_dark_stalker(base + randint1(base), FALSE);
		}
		break;

	case 6:
		if (name) return "Minor Getaway";
		if (desc) return "Teleport medium distance.";
    
		{
			int range = 30;

			if (info) return info_range(range);

			if (cast)
			{
				if (mut_present(MUT_ASTRAL_GUIDE))
					energy_use = 30;
				teleport_player(range, 0L);
			}
		}
		break;

	case 7:
		if (name) return "Set Minor Trap";
		if (desc) return "Sets a weak trap under you. This trap will have various weak effects on a passing monster.";

		if (cast)    
			set_trap(py, px, feat_rogue_trap1);
		break;

	/* Thieving Ways */
	case 8:
		if (name) return "Map Escape Route";
		if (desc) return "Maps nearby area.";
		if (info) return info_radius(rad);

		if (cast)
			map_area(rad);
		break;

	case 9:
		if (name) return "Pick Pocket";
		if (desc) return "Attempt to steal an item or treasure from an adjacent monster.";

		if (cast)
			return _rogue_pick_pocket();
		break;

	case 10:
		if (name) return "Negotiate";
		if (desc) return "Attempt to bargain for the services of a nearby thief.";

		if (cast)
			return _rogue_negotiate();
		break;

	case 11:
		if (name) return "Fetch Object";
		if (desc) return "Pulls a distant item close to you.";
    
		{
			int weight = spell_power(plev * 15);
			if (info) return info_weight(weight);
			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fetch(dir, weight, FALSE);
			}
		}
		break;

	case 12:
		if (name) return "Eye for Danger";
		if (desc) return "Gives telepathy for a while.";
		{
			int base = 25;
			int sides = 30;

			if (info) return info_duration(base, sides);

			if (cast)
				set_tim_esp(randint1(sides) + base, FALSE);
		}
		break;

	case 13:
		if (name) return "Examine Loot";
		if (desc) return "Identifies an item.";
    
		if (cast)
		{
			if (!ident_spell(NULL)) 
				return NULL;
		}
		break;

	case 14:
		if (name) return "Set Major Trap";
		if (desc) return "Sets a trap under you. This trap will have various effects on a passing monster.";

		if (cast)    
			set_trap(py, px, feat_rogue_trap2);
		break;

	case 15:
		if (name) return "Make Haste";
		if (desc) return "Hastes you for a while.";
   
		{
			int base = spell_power(plev);
			int sides = spell_power(20 + plev);

			if (info) return info_duration(base, sides);

			if (cast)
				set_fast(randint1(sides) + base, FALSE);
		}
		break;

	/* Great Escapes */
	case 16:
		if (name) return "Create Stairs";
		if (desc) return "Creates a flight of stairs underneath you.";
    
		if (cast)
			stair_creation(FALSE);
		break;

	case 17:
		if (name) return "Panic Hit";
		if (desc) return "Attack an adjacent monster and attempt a getaway.";
    
		if (cast)
		{
			int dir = 0;
			int x, y;

			if (!get_rep_dir2(&dir)) return NULL;
			y = py + ddy[dir];
			x = px + ddx[dir];
			if (cave[y][x].m_idx)
			{
				py_attack(y, x, 0);
				if (randint0(p_ptr->skills.dis) < 7)
					msg_print(T("You failed to teleport.", "うまく逃げられなかった。"));
				else 
					teleport_player(30, 0);
			}
			else
			{
				msg_print(T("You don't see any monster in this direction", "その方向にはモンスターはいません。"));
				msg_print(NULL);
				return NULL;
			}
		}
		break;

	case 18:
		if (name) return "Panic Shot";
		if (desc) return "Shoot a nearby monster and attempt a getaway.";
    
		if (cast)
		{
			if (!do_cmd_fire()) return NULL;
			if (randint0(p_ptr->skills.dis) < 7)
				msg_print(T("You failed to teleport.", "うまく逃げられなかった。"));
			else 
				teleport_player(30, 0);
		}
		break;

	case 19:
		if (name) return "Panic Summons";
		if (desc) return "Summon assistance and attempt a getaway.";
    
		if (cast)
		{
			trump_summoning(damroll(2, 3), !fail, py, px, 0, SUMMON_THIEF, PM_ALLOW_GROUP);

			if (randint0(p_ptr->skills.dis) < 7)
				msg_print(T("You failed to teleport.", "うまく逃げられなかった。"));
			else 
				teleport_player(30, 0);
		}
		break;

	case 20:
		if (name) return "Panic Traps";
		if (desc) return "Set multiple weak traps and attempt a getaway.";
    
		if (cast)
		{
			int y = 0, x = 0;
			int dir;

			for (dir = 0; dir <= 8; dir++)
			{
				y = py + ddy_ddd[dir];
				x = px + ddx_ddd[dir];

				set_trap(y, x, feat_rogue_trap1);
			}
			
			if (randint0(p_ptr->skills.dis) < 7)
				msg_print(T("You failed to teleport.", "うまく逃げられなかった。"));
			else 
				teleport_player(30, 0);
		}
		break;

	case 21:
		if (name) return "Flee Level";
		if (desc) return "Flee your current level without delay.";
    
		if (cast)
		{
			if (!get_check("Are you sure? (Flee Level)")) return NULL;
			teleport_level(0);
		}
		break;

	case 22:
		if (name) return "New Beginnings";
		if (desc) return "Recreates current dungeon level after a short delay.";
		if (info) return info_delay(15, 20);

		if (cast)
			alter_reality();
		break;

	case 23:
		if (name) return "Major Getaway";
		if (desc) return "Teleport long distance with very little energy use.";
    
		{
			int range = plev * 5;

			if (info) return info_range(range);

			if (cast)
			{
				energy_use = 15;
				teleport_player(range, 0L);
			}
		}
		break;

	/* Book of Shadows */
	case 24:
		if (name) return "Protect Loot";
		if (desc) return "For a long time, items in your inventory will have a chance at resisting destruction.";
   
		{
			int base = spell_power(plev*2);
			int sides = spell_power(plev*2);

			if (info) return info_duration(base, sides);

			if (cast)
				set_tim_inven_prot(randint1(sides) + base, FALSE);
		}
		break;

	case 25:
		if (name) return "Teleport To";
		if (desc) return "Teleport a visible monster next to you without disturbing it.";

		if (cast)
		{
			monster_type *m_ptr;
			monster_race *r_ptr;
			char m_name[80];

			if (!target_set(TARGET_KILL)) return NULL;
			if (!cave[target_row][target_col].m_idx) return NULL;
			if (!player_has_los_bold(target_row, target_col)) return NULL;
			if (!projectable(py, px, target_row, target_col)) return NULL;

			m_ptr = &m_list[cave[target_row][target_col].m_idx];
			r_ptr = &r_info[m_ptr->r_idx];
			monster_desc(m_name, m_ptr, 0);
			if (r_ptr->flagsr & RFR_RES_TELE)
			{
				if ((r_ptr->flags1 & (RF1_UNIQUE)) || (r_ptr->flagsr & RFR_RES_ALL))
				{
					if (is_original_ap_and_seen(m_ptr)) r_ptr->r_flagsr |= RFR_RES_TELE;
					msg_format("%s is unaffected!", m_name);
					break;
				}
				else if (r_ptr->level > randint1(100))
				{
					if (is_original_ap_and_seen(m_ptr)) r_ptr->r_flagsr |= RFR_RES_TELE;
					msg_format("%s resists!", m_name);
					break;
				}
			}
			msg_format("You command %s to return.", m_name);
			teleport_monster_to(cave[target_row][target_col].m_idx, py, px, 100, TELEPORT_PASSIVE);
		}
		break;

	case 26:
		if (name) return "Walk Quickly";
		if (desc) return "For a while, movement will cost less energy.";
   
		{
			int base = spell_power(plev);
			int sides = spell_power(20 + plev);

			if (info) return info_duration(base, sides);

			if (cast)
				set_tim_quick_walk(randint1(sides) + base, FALSE);
		}
		break;

	case 27:
		if (name) return "Shadow Storm";
		if (desc) return "Fires a huge ball of darkness.";
    
		{
			int dam = spell_power(10 * (plev - 20));
			int rad = spell_power(4);

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_DARK, dir, dam, rad);
			}
		}
		break;

	case 28:
		if (name) return "Hide in Shadows";
		if (desc) return "You become shrouded in darkness.";
		{
			int d = p_ptr->lev;
			if (info) return info_duration(spell_power(d), spell_power(d));
			if (cast) 
			{
				if (p_ptr->tim_superstealth)
				{
					msg_print("You are already hiding in the shadows.");
					return NULL;
				}
				set_tim_superstealth(spell_power(randint1(d) + d), FALSE);
			}
		}
		break;

	case 29:
		if (name) return "Hide in Stone";
		if (desc) return "For a short time, you may move through walls.";
		{
			int d = p_ptr->lev/3;
			if (info) return info_duration(spell_power(d), spell_power(d));
			if (cast) 
				set_kabenuke(spell_power(randint1(d) + d), FALSE);
		}
		break;

	case 30:
		if (name) return "Set Ultimate Trap";
		if (desc) return "Sets an extremely powerful trap under you. This trap will have various strong effects on a passing monster.";

		if (cast)    
			set_trap(py, px, feat_rogue_trap3);
		break;

	case 31:
		if (name) return "Assassinate";
		if (desc) return "Attempt to instantly kill a sleeping monster.";

		if (cast)
		{
			int y, x, dir;
			if (!get_rep_dir2(&dir)) return NULL;
			if (dir == 5) return NULL;

			y = py + ddy[dir];
			x = px + ddx[dir];

			if (cave[y][x].m_idx)
			{
				monster_type *m_ptr = &m_list[cave[y][x].m_idx];
				if (MON_CSLEEP(m_ptr))
					py_attack(y, x, ROGUE_ASSASSINATE);
				else
				{
					msg_print("This only works for sleeping monsters.");
					return NULL;
				}
			}
			else
			{
				msg_print(T("There is no monster.", "その方向にはモンスターはいません。"));
				return NULL;
			}
		}
		break;

	}

	return "";
}

static cptr do_armageddon_spell(int spell, int mode)
{
	bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
	bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
	bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
	bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
	bool fail = (mode == SPELL_FAIL) ? TRUE : FALSE;
	bool spoil = (mode == SPELL_SPOIL_DESC) ? TRUE : FALSE;

	int plev = p_ptr->lev;
	int dir;

	switch (spell)
	{
	/* Book of Elements */
	case 0:
		if (name) return "Lightning Bolt";
		if (desc) return "Fires a bolt or beam of electricity.";
    
		{
			int dice = 3 + plev / 4;
			int sides = 8;

			if (info) return info_damage(spell_power(dice), sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_bolt_or_beam(beam_chance(), GF_ELEC, dir, spell_power(damroll(dice, sides)));
			}
		}
		break;
	case 1:
		if (name) return "Frost Bolt";
		if (desc) return "Fires a bolt or beam of cold.";
    
		{
			int dice = 4 + plev / 4;
			int sides = 8;

			if (info) return info_damage(spell_power(dice), sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_bolt_or_beam(beam_chance(), GF_COLD, dir, spell_power(damroll(dice, sides)));
			}
		}
		break;
	case 2:
		if (name) return "Fire Bolt";
		if (desc) return "Fires a bolt or beam of fire.";
    
		{
			int dice = 5 + plev / 4;
			int sides = 8;

			if (info) return info_damage(spell_power(dice), sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_bolt_or_beam(beam_chance(), GF_FIRE, dir, spell_power(damroll(dice, sides)));
			}
		}
		break;
	case 3:
		if (name) return "Acid Bolt";
		if (desc) return "Fires a bolt or beam of acid.";
    
		{
			int dice = 5 + plev / 4;
			int sides = 8;

			if (info) return info_damage(spell_power(dice), sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_bolt_or_beam(beam_chance(), GF_ACID, dir, spell_power(damroll(dice, sides)));
			}
		}
		break;
	case 4:
		if (name) return "Lightning Ball";
		if (desc) return "Fires a ball of electricity.";
    
		{
			int dam = spell_power(3*plev/2 + 20);
			int rad = 2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_ELEC, dir, dam, rad);
			}
		}
		break;
	case 5:
		if (name) return "Frost Ball";
		if (desc) return "Fires a ball of cold.";
    
		{
			int dam = spell_power(3*plev/2 + 25);
			int rad = 2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_COLD, dir, dam, rad);
			}
		}
		break;
	case 6:
		if (name) return "Fire Ball";
		if (desc) return "Fires a ball of fire.";
    
		{
			int dam = spell_power(3*plev/2 + 30);
			int rad = 2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_FIRE, dir, dam, rad);
			}
		}
		break;
	case 7:
		if (name) return "Acid Ball";
		if (desc) return "Fires a ball of acid.";
    
		{
			int dam = spell_power(3*plev/2 + 35);
			int rad = 2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_ACID, dir, dam, rad);
			}
		}
		break;

	/* Earth, Wind and Fire */
	case 8:
		if (name) return "Shard Bolt";
		if (desc) return "Fires a bolt or beam of shards.";
    
		{
			int dice = 7 + plev / 4;
			int sides = 8;

			if (info) return info_damage(spell_power(dice), sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_bolt_or_beam(beam_chance(), GF_SHARDS, dir, spell_power(damroll(dice, sides)));
			}
		}
		break;
	case 9:
		if (name) return "Gravity Bolt";
		if (desc) return "Fires a bolt or beam of gravity.";
    
		{
			int dice = 5 + plev / 4;
			int sides = 8;

			if (info) return info_damage(spell_power(dice), sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_bolt_or_beam(beam_chance(), GF_GRAVITY, dir, spell_power(damroll(dice, sides)));
			}
		}
		break;
	case 10:
		if (name) return "Plasma Bolt";
		if (desc) return "Fires a bolt or beam of plasma.";
    
		{
			int dice = 11 + plev / 4;
			int sides = 8;

			if (info) return info_damage(spell_power(dice), sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_bolt_or_beam(beam_chance(), GF_PLASMA, dir, spell_power(damroll(dice, sides)));
			}
		}
		break;
	case 11:
		if (name) return "Meteor";
		if (desc) return "Fires a meteor.";
    
		{
			int dam = spell_power(plev + 60);
			int rad = 2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_METEOR, dir, dam, rad);
			}
		}
		break;
	case 12:
		if (name) return "Thunderclap";
		if (desc) return "Generates a ball of sound centered on you.";

		{
			int dam = spell_power((40 + plev)*2);
			int rad = plev / 10 + 2;

			if (info) return info_damage(0, 0, dam/2);

			if (cast)
			{
				msg_print("BOOM!");
				project(0, rad, py, px, dam, GF_SOUND, PROJECT_KILL | PROJECT_ITEM, -1);
			}
		}
		break;

	case 13:
		if (name) return "Windblast";
		if (desc) return "Fires a microburst of strong winds.";
    
		{
			int dam = spell_power(plev + 40);
			int rad = 2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_TELEKINESIS, dir, dam, rad);
			}
		}
		break;
	case 14:
		if (name) return "Hellstorm";
		if (desc) return "Generates a huge ball of fire centered on you.";

		{
			int dam = spell_power((6 * plev)*2);
			int rad = 8;

			if (info) return info_damage(0, 0, dam/2);

			if (cast)
				fire_ball(GF_FIRE, 0, dam, rad);
		}
		break;
	case 15:
		if (name) return "Rocket";
		if (desc) return "Fires a rocket.";
    
		{
			int dam = spell_power(60 + plev * 4);
			int rad = 2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				msg_print("You launch a rocket!");
				fire_rocket(GF_ROCKET, dir, dam, rad);
			}
		}
		break;

	/* Path of Destruction */
	case 16:
		if (name) return "Ice Bolt";
		if (desc) return "Fires a bolt of ice.";
    
		{
			int dice = 5 + plev/4;
			int sides = 15;

			if (info) return info_damage(spell_power(dice), sides, 0);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_bolt(GF_ICE, dir, spell_power(damroll(dice, sides)));
			}
		}
		break;
	case 17:
		if (name) return "Water Ball";
		if (desc) return "Fires a ball of water.";
    
		{
			int dam = spell_power(2*plev + 30);
			int rad = 2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_WATER, dir, dam, rad);
			}
		}
		break;
	case 18:
		if (name) return "Breathe Lightning";
		if (desc) return "Breathes a cone of electricity at chosen target.";
    
		{
			int dam = spell_power(9*plev/2);
			int rad = plev > 40 ? -3 : -2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_ELEC, dir, dam, rad);
			}
		}
		break;
	case 19:
		if (name) return "Breathe Frost";
		if (desc) return "Breathes a cone of cold at chosen target.";
    
		{
			int dam = spell_power(9*plev/2);
			int rad = plev > 40 ? -3 : -2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_COLD, dir, dam, rad);
			}
		}
		break;
	case 20:
		if (name) return "Breathe Fire";
		if (desc) return "Breathes a cone of fire at chosen target.";
    
		{
			int dam = spell_power(5*plev);
			int rad = plev > 40 ? -3 : -2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_FIRE, dir, dam, rad);
			}
		}
		break;
	case 21:
		if (name) return "Breathe Acid";
		if (desc) return "Breathes a cone of acid at chosen target.";
    
		{
			int dam = spell_power(5*plev);
			int rad = plev > 40 ? -3 : -2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_ACID, dir, dam, rad);
			}
		}
		break;
	case 22:
		if (name) return "Breathe Plasma";
		if (desc) return "Breathes a cone of plasma at chosen target.";
    
		{
			int dam = spell_power(11*plev/2);
			int rad = plev > 40 ? -3 : -2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_PLASMA, dir, dam, rad);
			}
		}
		break;
	case 23:
		if (name) return "Breathe Gravity";
		if (desc) return "Breathes a cone of gravity at chosen target.";
    
		{
			int dam = spell_power(4*plev);
			int rad = plev > 40 ? -3 : -2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_GRAVITY, dir, dam, rad);
			}
		}
		break;

	/* Day of Ragnarok */
	case 24:
		if (name) return "Mana Bolt";
		if (desc) return "Fires a bolt of mana.";
    
		{
			int dice = 1;
			int sides = 5*plev;

			if (info) return info_damage(dice, spell_power(sides), spell_power(50));

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_bolt(GF_MANA, dir, spell_power(damroll(dice, sides) + 50));
			}
		}
		break;
	case 25:
		if (name) return "Plasma Ball";
		if (desc) return "Fires a ball of plasma.";
    
		{
			int dam = spell_power(2*plev + 90);
			int rad = 3;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_PLASMA, dir, dam, rad);
			}
		}
		break;
	case 26:
		if (name) return "Mana Ball";
		if (desc) return "Fires a ball of pure mana.";
    
		{
			int dam = spell_power(4*plev + 100);
			int rad = 3;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_MANA, dir, dam, rad);
			}
		}
		break;
	case 27:
		if (name) return "Breathe Sound";
		if (desc) return "Breathes a cone of sound at chosen target.";
    
		{
			int dam = spell_power(6*plev);
			int rad = plev > 40 ? -3 : -2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_SOUND, dir, dam, rad);
			}
		}
		break;
	case 28:
		if (name) return "Breathe Inertia";
		if (desc) return "Breathes a cone of inertia at chosen target.";
    
		{
			int dam = spell_power(5*plev);
			int rad = plev > 40 ? -3 : -2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_INERT, dir, dam, rad);
			}
		}
		break;
	case 29:
		if (name) return "Breathe Disintegration";
		if (desc) return "Breathes a cone of disintegration at chosen target.";
    
		{
			int dam = spell_power(7*plev);
			int rad = plev > 40 ? -3 : -2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_DISINTEGRATE, dir, dam, rad);
			}
		}
		break;
	case 30:
		if (name) return "Breathe Mana";
		if (desc) return "Breathes a cone of mana at chosen target.";
    
		{
			int dam = spell_power(9*plev);
			int rad = plev > 40 ? -3 : -2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_MANA, dir, dam, rad);
			}
		}
		break;
	case 31:
		if (name) return "Breathe Shards";
		if (desc) return "Breathes a cone of shards at chosen target.";
    
		{
			int dam = spell_power(10*plev);
			int rad = plev > 40 ? -3 : -2;

			if (info) return info_damage(0, 0, dam);

			if (cast)
			{
				if (!get_aim_dir(&dir)) return NULL;
				fire_ball(GF_SHARDS, dir, dam, rad);
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
	cptr result = NULL;

	_current_realm_hack = realm;

	switch (realm)
	{
	case REALM_LIFE:     result = do_life_spell(spell, mode); break;
	case REALM_SORCERY:  result = do_sorcery_spell(spell, mode); break;
	case REALM_NATURE:   result = do_nature_spell(spell, mode); break;
	case REALM_CHAOS:    result = do_chaos_spell(spell, mode); break;
	case REALM_DEATH:    result = do_death_spell(spell, mode); break;
	case REALM_TRUMP:    result = do_trump_spell(spell, mode); break;
	case REALM_ARCANE:   result = do_arcane_spell(spell, mode); break;
	case REALM_CRAFT:    result = do_craft_spell(spell, mode); break;
	case REALM_DAEMON:   result = do_daemon_spell(spell, mode); break;
	case REALM_CRUSADE:  result = do_crusade_spell(spell, mode); break;
	case REALM_MUSIC:    result = do_music_spell(spell, mode); break;
	case REALM_HISSATSU: result = do_hissatsu_spell(spell, mode); break;
	case REALM_HEX:      result = do_hex_spell(spell, mode); break;
	case REALM_NECROMANCY: result = do_necromancy_spell(spell, mode); break;
	case REALM_ARMAGEDDON: result = do_armageddon_spell(spell, mode); break;
	case REALM_BURGLARY: result = do_burglary_spell(spell, mode); break;
	}

	_current_realm_hack = 0;
	return result;
}

int get_realm_idx(cptr name)
{
	int i;
	for (i = 0; i < MAX_REALM; i++)
	{
		if (strcmp(name, realm_names[i]) == 0)
			return i;
	}
	return -1;
}