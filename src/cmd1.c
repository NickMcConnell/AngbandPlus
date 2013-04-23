#define CMD1_C
/* File: cmd1.c */

/* Purpose: Physical player attacks, racial/chaos activations. */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Determine if the player "hits" a monster (normal combat).
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
static bool test_hit(int chance, int ac, int vis)
{
	int k;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Instant miss or hit */
	if (k < 10) return (k < 5);

	/* Never hit */
	if (chance <= 0) return (FALSE);

	/* Invisible monsters are harder to hit */
	if (!vis) chance = (chance + 1) / 2;

	/* Power competes against armor */
	if (rand_int(chance) < (ac * 3 / 4)) return (FALSE);

	/* Assume hit */
	return (TRUE);
}



/*
 * Critical hits (from objects thrown by player)
 * Factor in item weight, total plusses, and player level.
 */
static s16b critical_shot(int weight, int plus, int dam)
{
	int i, k;

	/* Extract "shot" power */
	i = (weight + ((p_ptr->to_h + plus) * 4) + (skill_set[SKILL_MISSILE].value));

	/* Critical hit */
	if (randint(5000) <= i)
	{
		k = weight + randint(500);

		if (k < 500)
		{
			msg_print("It was a good hit!");
			dam = 2 * dam + 5;
		}
		else if (k < 1000)
		{
			msg_print("It was a great hit!");
			dam = 2 * dam + 10;
		}
		else
		{
			msg_print("It was a superb hit!");
			dam = 3 * dam + 15;
		}
	}

	return (dam);
}



/*
 * Critical hits (by player)
 *
 * Factor in weapon weight, total plusses, player level.
 */
static s16b critical_norm(int weight, int plus, int dam)
{
	int i, k;

	/* Extract "blow" power */
	i = (weight + ((p_ptr->to_h + plus) * 5) + (skill_set[p_ptr->wield_skill].value));

	/* Chance */
	if (randint(5000) <= i)
	{
		k = weight + randint(650);

		if (k < 400)
		{
			msg_print("It was a good hit!");
			dam = 2 * dam + 5;
		}
		else if (k < 700)
		{
			msg_print("It was a great hit!");
			dam = 2 * dam + 10;
		}
		else if (k < 900)
		{
			msg_print("It was a superb hit!");
			dam = 3 * dam + 15;
		}
		else if (k < 1300)
		{
			msg_print("It was a *GREAT* hit!");
			dam = 3 * dam + 20;
		}
		else
		{
			msg_print("It was a *SUPERB* hit!");
			dam = ((7 * dam) / 2) + 25;
		}
	}

	return (dam);
}


typedef struct flag_type flag_type;
struct flag_type
{
	int set;
	u32b flag;
};

typedef struct slay_type slay_type;
struct slay_type
{
	flag_type obj;
	flag_type mon;
	s16b mult;
	bool have; /* Should the monster have the flag (a slay) or not (a brand)? */
};

/*
 * Array of the slays and brands an object can have, with the relevant monster
 * flags for each.
 */
static slay_type slay_flags[] =
{
	{{TR1, TR1_SLAY_ANIMAL}, {RF3, RF3_ANIMAL}, 2, TRUE},
	{{TR1, TR1_SLAY_EVIL}, {RF3, RF3_EVIL}, 2, TRUE},
	{{TR1, TR1_SLAY_UNDEAD}, {RF3, RF3_UNDEAD}, 3, TRUE},
	{{TR1, TR1_SLAY_DEMON}, {RF3, RF3_DEMON}, 3, TRUE},
	{{TR1, TR1_SLAY_ORC}, {RF3, RF3_ORC}, 3, TRUE},
	{{TR1, TR1_SLAY_TROLL}, {RF3, RF3_TROLL}, 3 , TRUE},
	{{TR1, TR1_SLAY_GIANT}, {RF3, RF3_GIANT}, 3, TRUE},
	{{TR1, TR1_SLAY_DRAGON}, {RF3, RF3_DRAGON}, 3, TRUE},
	{{TR1, TR1_KILL_DRAGON}, {RF3, RF3_DRAGON}, 5, TRUE},
	{{TR1, TR1_X15_DRAGON}, {RF3, RF3_DRAGON}, 15, TRUE},
	{{TR1, TR1_BRAND_ACID}, {RF3, RF3_IM_ACID}, 3, FALSE},
	{{TR1, TR1_BRAND_ELEC}, {RF3, RF3_IM_ELEC}, 3, FALSE},
	{{TR1, TR1_BRAND_FIRE}, {RF3, RF3_IM_FIRE}, 3, FALSE},
	{{TR1, TR1_BRAND_COLD}, {RF3, RF3_IM_COLD}, 3, FALSE},
	{{TR1, TR1_BRAND_POIS}, {RF3, RF3_IM_POIS}, 3, FALSE},
};

/*
 * Check if the flag is present in the flags array.
 */
static bool flag_p(u32b *flags, flag_type *flag)
{
	return ((flags[flag->set] & flag->flag) == flag->flag);
}

/*
 * Extract the "total damage" from a given object hitting a given monster.
 *
 * Note that "flasks of oil" do NOT do fire damage, although they
 * certainly could be made to do so.  XXX XXX
 *
 * Note that most brands and slays are x3, except Slay Animal (x2),
 * Slay Evil (x2), and Kill dragon (x5).
 */
static s16b tot_dam_aux(object_ctype *o_ptr, int tdam, monster_type *m_ptr)
{
	slay_type *slay;

	int mult = 1;

	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	u32b f[MAX_FLAG_SETS], *rf[MAX_FLAG_SETS];

	/* Extract the flags */
	object_flags(o_ptr, f+TR1, f+TR2, f+TR3);

	f[RF1] = r_ptr->flags1;
	f[RF2] = r_ptr->flags2;
	f[RF3] = r_ptr->flags3;
	/* f[RF4] = r_ptr->flags4; */
	/* f[RF5] = r_ptr->flags5; */
	/* f[RF6] = r_ptr->flags6; */

	rf[RF1] = &r_ptr->r_flags1;
	rf[RF2] = &r_ptr->r_flags2;
	rf[RF3] = &r_ptr->r_flags3;
	/* *rf[RF4] = &r_ptr->r_flags4; */
	/* *rf[RF5] = &r_ptr->r_flags5; */
	/* *rf[RF6] = &r_ptr->r_flags6; */

	FOR_ALL_IN(slay_flags, slay)
	{
		bool has = flag_p(f, &slay->mon);

		/* The object doesn't have the flag. */
		if (!flag_p(f, &slay->obj)) continue;

		/* The monster is susceptible to this slay/brand. */
		if (slay->have == has)
		{
			if (mult < slay->mult) mult = slay->mult;
		}

		/* The monster has the relevant flag. */
		if (m_ptr->ml && has)
		{
			*rf[slay->mon.set] |= slay->mon.flag;
		}
	}

	/* Return the total damage */
	return (tdam * mult);
}


static void touch_zap_player(monster_type *m_ptr)
{
		int             aura_damage = 0;
		monster_race    *r_ptr = &r_info[m_ptr->r_idx];

		if (r_ptr->flags2 & (RF2_AURA_FIRE))
		{
			if (!(p_ptr->immune_fire))
			{
				aura_damage
					= damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

				msg_print("You are suddenly very hot!");

				if (p_ptr->oppose_fire) aura_damage = (aura_damage+2) / 3;
				if (p_ptr->resist_fire) aura_damage = (aura_damage+2) / 3;

				take_hit(aura_damage,
					format("%v", monster_desc_f2, m_ptr, 0x88), m_ptr->r_idx);
				r_ptr->r_flags2 |= RF2_AURA_FIRE;
				handle_stuff();
			}
		}


		if (r_ptr->flags2 & (RF2_AURA_ELEC))
		{
			if (!(p_ptr->immune_elec))
			{
				aura_damage
					= damroll(1 + (r_ptr->level / 26), 1 + (r_ptr->level / 17));

				if (p_ptr->oppose_elec) aura_damage = (aura_damage+2) / 3;
				if (p_ptr->resist_elec) aura_damage = (aura_damage+2) / 3;

				msg_print("You get zapped!");
				take_hit(aura_damage,
					format("%v", monster_desc_f2, m_ptr, 0x88), m_ptr->r_idx);
				r_ptr->r_flags2 |= RF2_AURA_ELEC;
				handle_stuff();
			}
		}
}

static void do_natural_attack(s16b m_idx, natural_attack *n_ptr)
{
	bool fear[1];
	int             k, bonus, chance;
	monster_type    *m_ptr = &m_list[m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];

	/* Slow down the attack */
	energy_use += TURN_ENERGY/10;

	/* Calculate the "attack quality" */
	bonus = p_ptr->to_h;
	chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));

	/* Test for hit */
	if (test_hit(chance, r_ptr->ac, m_ptr->ml))
	{
			/* Sound */
			sound(SOUND_HIT);

			msg_format("You hit %v with your %s.",
				monster_desc_f2, m_ptr, 0, n_ptr->desc);

			/* Give experience if it wasn't too easy */
			if (chance < (r_ptr->ac * 3))
			{
				skill_exp(SKILL_CLOSE);
			}

			k = damroll(n_ptr->dd, n_ptr->ds);
			k = critical_norm(n_ptr->wgt, p_ptr->to_h, k);

			/* Apply the player damage bonuses */
			k += p_ptr->to_d;

			/* No negative damage */
			if (k < 0) k = 0;

			/* Complex message */
			if (cheat_wzrd)
			{
				msg_format("You do %d (out of %d) damage.", k, m_ptr->hp);
			}


			if (m_ptr->smart & SM_ALLY)
			{
				msg_format("%^v gets angry!", monster_desc_f2, m_ptr, 0);
				m_ptr->smart &= ~SM_ALLY;
			}

		if (n_ptr->typ == GF_HIT)
		{
			/* Melee attack. */
			mon_take_hit(m_idx, k, fear, NULL);
		}
		else
		{
			/* Magic attack. */
			project(0, 0, m_ptr->fy, m_ptr->fx, k, n_ptr->typ, PROJECT_KILL);
		}
		touch_zap_player(m_ptr);
	}

	/* Player misses */
	else
	{
		/* Sound */
		sound(SOUND_MISS);

		/* Message */
		msg_format("You miss %v.", monster_desc_f2, m_ptr, 0);
	}
}

/*
 * Choose a martial arts attack from those in ma_blows based on your current
 * skill level.
 */
static martial_arts *choose_ma_attack(int skill)
{
	martial_arts *ma_ptr, *new_ptr;
	int i,max;

	/* Find the last martial arts technique the player may be able to use. */
	for (max = 0; max < MAX_MA; max++)
	{
		if (ma_blows[max].min_level > skill) break;
	}

	/* Very low skills use a special "unskilled" blow. */
	if (!max) return ma_blows+MAX_MA;

	/* As do stunned or confused characters (rather than the weak skilled blow
	 * used in other variants). */
	if (p_ptr->stun || p_ptr->confused) return ma_blows+MAX_MA;

	for (i = MAX(1, skill/14), ma_ptr = ma_blows+MAX_MA; i; i--)
	{
		do
		{
			new_ptr = &ma_blows[(rand_int(max))];
		}
		while (rand_int(skill/2*2) < new_ptr->chance);

		/* keep the highest level attack available we found */
		if (new_ptr->min_level > ma_ptr->min_level)
		{
			ma_ptr = new_ptr;
			if (cheat_wzrd && cheat_xtra)
			{
				msg_print("Attack re-selected.");
			}
		}
	}
	return ma_ptr;
}

/*
 * Return TRUE if a monster can be kicked in the ankle.
 * This gives some unintuitive results.
 */
static PURE bool mon_has_knee(const monster_race *r_ptr)
{
	/* What would this knee do? */
	if (r_ptr->flags1 & RF1_NEVER_MOVE) return FALSE;

	/* Monsters with symbols which suggest no knees... */
	if (strchr("UjmeEv$,DdsbBFIJQSXclnw!=?", r_ptr->gfx.dc))
		return FALSE;

	/* Assume it has knees. */
	return TRUE;
}

/*
 * Calculate the damage a martial art and carry out various side effects.
 */
static int do_ma_attack(monster_type *m_ptr)
{
	int k, resist_stun = 0, stun_effect = 0;
	bool survives;
	int skill = skill_set[SKILL_MA].value;
	monster_race *r_ptr = r_info+m_ptr->r_idx;
	martial_arts *ma_ptr = choose_ma_attack(skill);

	/* Calculate the stun resistance. */
	if (r_ptr->flags1 & RF1_UNIQUE) resist_stun += 88;
	if (r_ptr->flags3 & RF3_NO_CONF) resist_stun += 44;
	if (r_ptr->flags3 & RF3_NO_SLEEP) resist_stun += 44;
	if (!live_monster_wide_p(r_ptr)) resist_stun += 88;

	/* Calculate the damage. */
	k = damroll(ma_ptr->dd, ma_ptr->ds);
	k = critical_norm((skill/2) * (randint(10)), ma_ptr->min_level/2, k);

	/* Hack - say some extra stuff if the monster will survive. */
	survives = (k + p_ptr->to_d < m_ptr->hp);

	if (ma_ptr->effect == MA_KNEE && r_ptr->flags1 & RF1_MALE)
	{
		msg_format("You hit %v in the groin with your knee!",
			monster_desc_f2, m_ptr, 0);

		if (survives)
		{
			msg_format("%^v moans in agony!", monster_desc_f2, m_ptr, 0);
			stun_effect = 7 + randint(13);
			resist_stun /= 3;
		}
	}

	else if (ma_ptr->effect == MA_SLOW && mon_has_knee(r_ptr))
	{
		msg_format("You kick %v in the ankle.", monster_desc_f2, m_ptr, 0);

		if (survives)
		{
			if (!(r_ptr->flags1 & RF1_UNIQUE) &&
				(randint(skill_set[SKILL_MA].value/2) > r_ptr->level)
				&& m_ptr->mspeed > 60)
			{
				msg_format("%^v starts limping slower.",
					monster_desc_f2, m_ptr, 0);
				m_ptr->mspeed -= 10;
			}
		}
	}

	else
	{
		if (ma_ptr->effect)
		{
			stun_effect = rand_range(ma_ptr->effect/2+1, ma_ptr->effect/2*2);
		}

		msg_format(ma_ptr->desc, monster_desc_f2, m_ptr, 0);
	}

	if (stun_effect && survives)
	{
		if ((skill_set[SKILL_MA].value/2) >
			randint(r_ptr->level + resist_stun + 10))
		{
			if (m_ptr->stunned)
			{
				msg_format("%^v is more stunned.", monster_desc_f2, m_ptr, 0);
			}
			else
			{
				msg_format("%^v is stunned.", monster_desc_f2, m_ptr, 0);
			}
			m_ptr->stunned += (stun_effect);
		}
	}
	return k;
}

/*
 * Player attacks a (poor, defenseless) creature        -RAK-
 *
 * If no "weapon" is available, then "punch" the monster one time.
 */
void py_attack(int y, int x)
{
	int                     k, bonus, chance;

	cave_type               *c_ptr = &cave[y][x];

	monster_type    *m_ptr = &m_list[c_ptr->m_idx];
	monster_race    *r_ptr = &r_info[m_ptr->r_idx];

	object_type             *o_ptr;

	C_TNEW(m_name, MNAME_MAX, char);


	bool fear = FALSE, old_fear = (m_ptr->monfear != 0);

	bool        backstab = FALSE, vorpal_cut = FALSE, chaos_effect = FALSE;
	bool        stab_fleeing = FALSE;
	bool        do_quake = FALSE;
	bool drain_life = FALSE;
	u32b        f1, f2, f3; /* A massive hack -- life-draining weapons */
	bool        no_extra = FALSE;



	if ((m_ptr->csleep) && (m_ptr->ml))
	{
		/* Can't backstab creatures that we can't see, right? */
		backstab = TRUE;
	}
	if ((old_fear) && (m_ptr->ml) && (rand_int(50) < p_ptr->skill_stl))
	{
		stab_fleeing = TRUE;
	}

	/* Disturb the player */
	/*disturb(1);*/


	/* Disturb the monster */
	m_ptr->csleep = 0;


	/* Extract monster name (or "it") */
	strnfmt(m_name, MNAME_MAX, "%v", monster_desc_f2, m_ptr, 0);

	/* Auto-Recall if possible and visible */
	if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

	/* Track a new monster */
	if (m_ptr->ml) health_track(c_ptr->m_idx);



		/* Stop if friendly */
		if (m_ptr->smart & SM_ALLY &&
			! (p_ptr->stun || p_ptr->confused || p_ptr->image ||
				((p_has_mutation(MUT_BERS_RAGE)) && p_ptr->shero) ||
				!(m_ptr->ml)))
	{
		if (inventory[INVEN_WIELD].name1 != ART_STORMBRINGER)
	{
		msg_format("You stop to avoid hitting %s.", m_name);

		TFREE(m_name);
		return;
	}

			msg_format("Your black blade greedily attacks %s!",
				m_name);

	}



	/* Handle player fear */
	if (p_ptr->afraid)
	{   /* Message */
		if (m_ptr->ml)
		msg_format("You are too afraid to attack %s!", m_name);

		else
		msg_format ("There is something scary in your way!");

		TFREE(m_name);

		/* Done */
		return;
	}


	/* Access the weapon */
	o_ptr = &inventory[INVEN_WIELD];

	/* Calculate the "attack quality" */
	bonus = p_ptr->to_h + o_ptr->to_h;
	chance = (p_ptr->skill_thn + (bonus * BTH_PLUS_ADJ));

	/* Attack speed is based on theoretical number of blows per 60 turns*/
	energy_use=(TURN_ENERGY*60/p_ptr->num_blow);
		/* Test for hit */
		if (test_hit(chance, r_ptr->ac, m_ptr->ml))
		{
			/* Sound */
			sound(SOUND_HIT);

			/* Message */
			if (!(backstab || stab_fleeing))
				{
					if (!(ma_empty_hands()))
					msg_format("You hit %s.", m_name);
				}
			else if (backstab)
			{
				msg_format("You cruelly stab the helpless, sleeping %v!",
					monster_desc_aux_f3, r_ptr, 1, 0);
				skill_exp(SKILL_STEALTH);
			}
			else
			{
				msg_format("You backstab the fleeing %v!",
					monster_desc_aux_f3, r_ptr, 1, 0);
				skill_exp(SKILL_STEALTH);
			}

			/* Give experience if it wasn't too easy */
			if (chance < (r_ptr->ac * 3))
			{
				skill_exp(p_ptr->wield_skill);
			}
				/* Hack -- bare hands do one damage */
			k = 1;


		object_flags(o_ptr, &f1, &f2, &f3);

		if ((f1 & TR1_CHAOTIC) && (randint(2)==1))
			chaos_effect = TRUE;
		else
			chaos_effect = FALSE;

		if ((f1 & TR1_VAMPIRIC) || (chaos_effect && (randint(5)<3)))
		/* Prepare for drain... */
		{
		chaos_effect = FALSE;
		if (live_monster_wide_p(r_ptr)) drain_life = TRUE;
	}

	if (f1 & TR1_VORPAL && (randint((o_ptr->name1 == ART_VORPAL_BLADE)?3:6) == 1))
		vorpal_cut = TRUE;
	else vorpal_cut = FALSE;

	/* Handle martial arts. */
	if (ma_empty_hands())
	{
		k = do_ma_attack(m_ptr);
		if ((chance < (r_ptr->ac * 3)) ||
			(skill_set[SKILL_MA].value * 3 < r_ptr->ac * 4))
		{
			skill_exp(SKILL_MA);
		}
	}

	/* Handle normal weapon */
	else if (o_ptr->k_idx)
	{
		k = damroll(o_ptr->dd, o_ptr->ds);
		k = tot_dam_aux(o_ptr, k, m_ptr);
		if (backstab)
		{
			backstab = FALSE;
			k *= (3 + (skill_set[SKILL_STEALTH].value / 20));
		}
		else if (stab_fleeing)
		{
			k = ((3 * k) / 2);
		}
		if ((p_ptr->impact && ((k > 50) || randint(7)==1))
			|| (chaos_effect && (randint(250)==1)))
			{
			do_quake = TRUE;
			chaos_effect = FALSE;
			}

		k = critical_norm(o_ptr->weight, o_ptr->to_h, k);

		if (vorpal_cut)
		{
			int chance = (o_ptr->name1 == ART_VORPAL_BLADE) ? 2 : 4;
			int i;

			if ((o_ptr->name1 == ART_DEMONBLADE) && one_in(2))
			{
				msg_format("%v", get_rnd_line_f1, "chainswd.txt");
			}
			else if (o_ptr->name1 == ART_VORPAL_BLADE)
			{
				msg_print("Your Vorpal Blade goes snicker-snack!");
			}
			else
			{
				msg_format("Your weapon cuts deep into %s!", m_name);
			}

			/* Paranoia - it's only improbable. */
			for (i = 1; i < 32767 && one_in(chance); i++) ;

			/* Multiply to get the new damage. */
			if (32767 / i < k) k = 32767;
			else k *= i;
		}

		k += o_ptr->to_d;

			}

			/* Apply the player damage bonuses */
			k += p_ptr->to_d;

			/* No negative damage */
			if (k < 0) k = 0;

			/* Complex message */
			if (cheat_wzrd)
			{
				msg_format("You do %d (out of %d) damage.", k, m_ptr->hp);
			}

		/* Drain the life (message if it has an effect). */
		if (drain_life)
		{
			/* Heal by about 1/3 of the damage caused. */
			int drain_heal = damroll(4,(k / 6));

			/* Never more than was taken or needed or too much too quickly. */
			drain_heal = MIN(MIN(MIN(drain_heal, m_ptr->hp),
				p_ptr->mhp - p_ptr->chp),
				MAX_VAMPIRIC_DRAIN - p_ptr->vamp_drain);

			/* Remember how much the player has been healed by recently. */
			add_flag(TIMED_VAMP, drain_heal);

			/* Give a message if anything happened. */
			if (drain_heal)
				msg_format("Your weapon drains life from %s!", m_name);

			/* Gain the drained HP. */
			hp_player(drain_heal);
		}


			/* Damage, check for fear and death */
			mon_take_hit(c_ptr->m_idx, k, &fear, NULL);

			if (m_ptr->r_idx && m_ptr->smart & SM_ALLY)
			{
				msg_format("%^s gets angry!", m_name);
				m_ptr->smart &= ~SM_ALLY;
			}


		touch_zap_player(m_ptr);


			/* Confusion attack */
			if ((p_ptr->confusing) || (chaos_effect && (randint(10)!=1)))
			{
				/* Cancel glowing hands */
			if (p_ptr->confusing)
			{
				p_ptr->confusing = FALSE;
				msg_print("Your hands stop glowing.");
			}

			/* Prevent chaotic effects further down */
			chaos_effect = FALSE;


				/* Confuse the monster */
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					if (m_ptr->ml)
					{
						r_ptr->r_flags3 |= (RF3_NO_CONF);
					}

					msg_format("%^s is unaffected.", m_name);
				}
				else if (rand_int(100) < r_ptr->level)
				{
					msg_format("%^s is unaffected.", m_name);
				}
				else
				{
					msg_format("%^s appears confused.", m_name);
					m_ptr->confused += 10 + rand_int(skill_set[SKILL_DEVICE].value) / 10;
				}
			}

	else if (chaos_effect && (randint(2)==1))
	{
		chaos_effect = FALSE;
		msg_format("%^s disappears!", m_name);
		teleport_away(c_ptr->m_idx, 50);
		no_extra = TRUE;

	}


	else if (chaos_effect && cave_floor_bold(y,x)
			&& (randint(90) > r_ptr->level))
	{
		if (!((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags4 & RF4_BR_CHAO) || (r_ptr->flags1 & RF1_GUARDIAN)))
		{

				/* Pick a "new" monster race */
		int tmp = poly_r_idx(m_ptr->r_idx);
		chaos_effect = FALSE;

		/* Handle polymorph */
		if (tmp != m_ptr->r_idx)
		{
			monster_race *r2_ptr = &r_info[tmp];
			byte i = 0;
			/* Hack - SHAPECHANGERs change regularly anyway. */
			if (r_ptr->flags1 & RF1_ATTR_MULTI && r_ptr->flags2 & RF2_SHAPECHANGER) i |= 1;
			if (r2_ptr->flags1 & RF1_ATTR_MULTI && r2_ptr->flags2 & RF2_SHAPECHANGER) i |= 2;
			switch (i)
			{
				case 0: msg_format("%^s changes!", m_name); break;
				case 1: msg_format("%^s stops changing!", m_name); break;
				case 2: msg_format("%^s starts changing rapidly!", m_name); break;
				case 3: msg_format("%^s changes subtly!", m_name); break;
			}

			/* "Kill" the "old" monster */
			delete_monster_idx(c_ptr->m_idx,TRUE);

			/* Create a new monster (no groups) */
			(void)place_monster_aux(y, x, tmp, FALSE, FALSE, FALSE, FALSE);

			/* XXX XXX XXX Hack -- Assume success */

			/* Hack -- Get new monster */
			m_ptr = &m_list[c_ptr->m_idx];

			/* Oops, we need a different name... */
			strnfmt(m_name, MNAME_MAX, "%v", monster_desc_f2, m_ptr, 0);

			/* Hack -- Get new race */
			r_ptr = &r_info[m_ptr->r_idx];

			fear = FALSE;

			}
		}
		else
		msg_format("%^s is unaffected.", m_name);

		}
		}

		/* Player misses */
		else
		{
			/* Sound */
			sound(SOUND_MISS);

			backstab = FALSE; /* Clumsy! */

			/* Message */
			msg_format("You miss %s.", m_name);
		}

	/*
	 * Chaos features which yield extra 'natural' attacks
	 * only give attacks occasionally - depending on number
	 * on speed of player with weapon
	 */
	if ((!no_extra) && one_in((p_ptr->num_blow+30)/60))
	{
		natural_attack *n_ptr;
		FOR_ALL_IN(natural_attacks, n_ptr)
		{
			if (p_has_mutation(n_ptr->mut) && m_ptr->r_idx)
			{
				do_natural_attack(c_ptr->m_idx, n_ptr);
			}
		}
	}

	/* Hack -- delay fear messages */
	if (m_ptr->r_idx && m_ptr->ml && !old_fear && m_ptr->monfear)
	{
		/* Sound */
		sound(SOUND_FLEE);

		/* Message */
		msg_format("%^s flees in terror!", m_name);
	}


	/* Mega-Hack -- apply earthquake brand only once*/
	if (do_quake) earthquake(py, px, 10);

	TFREE(m_name);
}


/*
 * Attack a monster.
 */
void do_cmd_attack(void)
{
	int y, x;

	cave_type *c_ptr;

	bool more = FALSE;


	/* Set repeat if requested. */
	cnv_arg_to_rep();

	/* Get a direction to attack, or Abort */
	if (get_rep_target(&x, &y))
	{
		/* Get grid */
		c_ptr = &cave[y][x];

		/* Oops */
		if (!c_ptr->m_idx)
		{
			/* Take a turn */
			energy_use = extract_energy[p_ptr->pspeed];
			msg_print("You attack the empty air.");
		}
		/* Try fighting. */
		else
		{
			py_attack(y, x);
			if (c_ptr->m_idx) more = TRUE;
		}
	}

	/* Cancel repetition unless we can continue */
	if (!more) disturb(0);
}

/*
 * Determines the odds of an object breaking when thrown at a monster
 *
 * Note that artifacts never break, see the "drop_near()" function.
 */
static int breakage_chance(object_type *o_ptr)
{
	/* Examine the item type */
	switch (o_ptr->tval)
	{
		/* Always break */
		case TV_FLASK:
		case TV_POTION:
		case TV_BOTTLE:
		case TV_FOOD:
		case TV_JUNK:
		{
			return (100);
		}

		/* Often break */
		case TV_LITE:
		case TV_SCROLL:
		case TV_ARROW:
		case TV_SKELETON:
		case TV_CHARM:
		{
			return (50);
		}

		/* Sometimes break */
		case TV_WAND:
		case TV_SHOT:
		case TV_BOLT:
		case TV_SPIKE:
		{
			return (25);
		}
	}

	/* Rarely break */
	return (10);
}

/*
 * Throw or fire an object from the pack or floor.
 *
 * Note that most thrown things are completely safe from damage unless you
 * throw them at monsters.
 *
 * Return TRUE if an object was fired, FALSE otherwise.
 */
static bool do_cmd_fire_aux(object_type *o_ptr,
	int tdis, int tdam, int chance)
{
	int dir;
	int breakage, y, x, ty, tx, nx, ny;
	int cur_dis, visible;

	object_type q_ptr[1];

	bool do_break = FALSE;

	byte missile_attr;
	char missile_char;


	/* Get a direction (or cancel) */
	if (!get_aim_dir(&dir)) return FALSE;

	/* Obtain a local object */
	object_copy(q_ptr, o_ptr);

	/* Single object */
	q_ptr->number = 1;

	/* Reduce and describe object */
	item_increase(o_ptr, -1);
	item_describe(o_ptr);
	item_optimize(o_ptr);

	/* Sound */
	sound(SOUND_SHOOT);

	/* Find the color and symbol for the object for throwing */
	missile_attr = object_attr(q_ptr);
	missile_char = object_char(q_ptr);

	/* Start at the player */
	ny = y = py;
	nx = x = px;

	/* Predict the "target" location */
	get_dir_target(&tx, &ty, dir, NULL);

	/* Hack -- Handle stuff */
	handle_stuff();


	/* Travel until out of range or stopped */
	for (cur_dis = 0; cur_dis <= tdis; )
	{
		/* Hack -- Stop at the target */
		if ((y == ty) && (x == tx)) break;

		/* Calculate the new location (see "project()") */
		mmove2(&ny, &nx, py, px, ty, tx);

		/* Stopped by walls/doors */
		if (!cave_floor_bold(ny, nx)) break;

		/* Advance the distance */
		cur_dis++;

		/* Save the new location */
		x = nx;
		y = ny;


		/* The player can see the (on screen) missile */
		if (panel_contains(y, x) && player_can_see_bold(y, x))
		{
			/* Draw, Hilite, Fresh, Pause, Erase */
			print_rel(missile_char, missile_attr, y, x);
			move_cursor_relative(y, x);
			Term_fresh();
			Term_xtra(TERM_XTRA_DELAY, delay_factor);
			lite_spot(y, x);
			Term_fresh();
		}

		/* The player cannot see the missile */
		else
		{
			/* Pause anyway, for consistancy */
			Term_xtra(TERM_XTRA_DELAY, delay_factor);
		}


		/* Monster here, Try to hit it */
		if (cave[y][x].m_idx)
		{
			cave_type *c_ptr = &cave[y][x];

			monster_type *m_ptr = &m_list[c_ptr->m_idx];
			monster_race *r_ptr = &r_info[m_ptr->r_idx];

			/* Check the visibility */
			visible = m_ptr->ml;

			/* Note the collision */
			do_break = TRUE;

			/* Did we hit it (penalize range) */
			if (test_hit(chance - cur_dis, r_ptr->ac, m_ptr->ml))
			{
				bool fear = FALSE;

				/* Assume a default death */
				cptr note_dies = " dies.";

				/* Give experience (if it wasn't too easy) */
				if ((chance - cur_dis) < (r_ptr->ac * 3))
				{
					skill_exp(SKILL_MISSILE);
				}

				/* Some monsters get "destroyed" */
				if ((r_ptr->flags3 & (RF3_DEMON)) ||
					(r_ptr->flags3 & (RF3_UNDEAD)) ||
					(r_ptr->flags3 & (RF3_CTHULOID)) ||
					(r_ptr->flags2 & (RF2_STUPID)) ||
					(strchr("Evg", r_ptr->gfx.dc)))
				{
					/* Special note at death */
					note_dies = " is destroyed.";
				}


				/* Handle unseen monster */
				if (!visible)
				{
					/* Invisible monster */
					msg_format("The %v finds a mark.",
						object_desc_f3, q_ptr, FALSE, 3);
				}

				/* Handle visible monster */
				else
				{
					/* Message */
					msg_format("The %v hits %v.",
						object_desc_f3, q_ptr, FALSE, 3,
						monster_desc_f2, m_ptr, 0);

					/* Hack -- Track this monster race */
					if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

					/* Hack -- Track this monster */
					if (m_ptr->ml) health_track(c_ptr->m_idx);
				}

				/* Apply special damage XXX XXX XXX */
				tdam = tot_dam_aux(q_ptr, tdam, m_ptr);
				tdam = critical_shot(q_ptr->weight, q_ptr->to_h, tdam);

				/* No negative damage */
				if (tdam < 0) tdam = 0;

				/* Complex message */
				if (cheat_wzrd)
				{
					msg_format("You do %d (out of %d) damage.",
								tdam, m_ptr->hp);
				}

				/* Hit the monster, check for death */
				if (mon_take_hit(c_ptr->m_idx, tdam, &fear, note_dies))
				{
					/* Dead monster */
				}

				/* No death */
				else
				{
					/* Message */
					message_pain(m_ptr, tdam);

					/* Anger friends */
					if ((m_ptr->smart & SM_ALLY)
					&& (k_info[q_ptr->k_idx].tval != TV_POTION))
					{
						msg_format("%^v gets angry!", monster_desc_f2, m_ptr, 0);
						m_ptr->smart &= ~SM_ALLY;
					}

					/* Take note */
					if (fear && m_ptr->ml)
					{
						/* Sound */
						sound(SOUND_FLEE);

						/* Message */
						msg_format("%^v flees in terror!",
							monster_desc_f2, m_ptr, 0);
					}
				}
			}

			/* Stop looking */
			break;
		}
	}

	/* Potions can break against the wall and the seashore. */
	if (k_info[q_ptr->k_idx].tval == TV_POTION)
	{
		if (!cave_floor_bold(ny, nx) || cave[ny][nx].feat == FEAT_WATER)
			do_break = TRUE;
	}

	/* Chance of breakage (during attacks) */
	if (do_break)
	{
		breakage = breakage_chance(q_ptr);
	}
	else
	{
		breakage = 0;
	}

	/* Drop (or break) near that location */
	drop_near(q_ptr, breakage, y, x);

	/* An object was fired. */
	return TRUE;
}

/*
 * Fire an object from the pack or floor.
 *
 * You may only fire items that "match" your missile launcher.
 *
 * You must use slings + pebbles/shots, bows + arrows, xbows + bolts.
 *
 * See "calc_bonuses()" for more calculations and such.
 *
 * Note that "firing" a missile is MUCH better than "throwing" it.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Objects are more likely to break if they "attempt" to hit a monster.
 *
 * Rangers (with Bows) and Anyone (with "Extra Shots") get extra shots.
 *
 * The "extra shot" code works by decreasing the amount of energy
 * required to make each shot, spreading the shots out over time.
 *
 * Note that when firing missiles, the launcher multiplier is applied
 * after all the bonuses are added in, making multipliers very useful.
 *
 * Note that Bows of "Extra Might" get extra range and an extra bonus
 * for the damage multiplier.
 *
 * Note that Bows of "Extra Shots" give an extra shot.
 */
void do_cmd_fire(object_type *o_ptr)
{
	/* Get the "bow" (if any) */
	object_type *bow_ptr = &inventory[INVEN_BOW];

	/* Use the proper number of shots */
	int thits = p_ptr->num_fire;

	/* Base damage from thrown object plus launcher bonus */
	int tdam = damroll(o_ptr->dd, o_ptr->ds) + o_ptr->to_d + bow_ptr->to_d;

	int mult = get_bow_mult(bow_ptr);

	/* Base range */
	int tdis = 10 + 5 * mult;

	/* Chance of hitting */
	int chance = (p_ptr->to_h + o_ptr->to_h + bow_ptr->to_h);
	chance = (p_ptr->skill_thb + (chance * BTH_PLUS_ADJ));

	/* Get extra "power" from "extra might" */
	if (p_ptr->xtra_might) mult++;

	/* Boost the damage */
	tdam *= mult;

	/* Take time if completed. */
	if (do_cmd_fire_aux(o_ptr, tdis, tdam, chance))
		energy_use = (60*TURN_ENERGY / thits);
}

/*
 * Throw an object from the pack or floor.
 *
 * Note: "unseen" monsters are very hard to hit.
 *
 * Should throwing a weapon do full damage?  Should it allow the magic
 * to hit bonus of the weapon to have an effect?  Should it ever cause
 * the item to be destroyed?  Should it do any damage at all?
 */
static void do_cmd_throw_aux(object_type *o_ptr, int mult)
{
	/* Extract a "distance multiplier" */
	/* Changed for 'launcher' chaos feature */
	int p = 10 + 2 * (mult - 1);

	/* Enforce a minimum "weight" of one pound */
	int q = ((o_ptr->weight > 10) ? o_ptr->weight : 10);

	/* Hack -- Distance -- Reward strength, penalize weight */
	int tdis = MIN((adj_str_blow[p_ptr->stat_ind[A_STR]] + 20) * p / q, 10);

	/* Hack -- Base damage from thrown object */
	int tdam = damroll(o_ptr->dd, o_ptr->ds) + o_ptr->to_d * mult;

	/* Chance of hitting */
	int chance = (p_ptr->skill_tht + (p_ptr->to_h * BTH_PLUS_ADJ));

	/* Take time if completed. */
	if (do_cmd_fire_aux(o_ptr, tdis, tdam, chance))
		energy_use = extract_energy[p_ptr->pspeed];
}

/*
 * Throw a missile with normal power.
 */
void do_cmd_throw(object_type *o_ptr)
{
	do_cmd_throw_aux(o_ptr, 1);
}

/*
 * Hack - throw a missile (which has not yet been chosen) hard.
 */
void do_cmd_throw_hard(int mult)
{
	object_type *o_ptr = get_object_from_function(do_cmd_throw);

	/* Aborted. */
	if (!o_ptr) return;

	do_cmd_throw_aux(o_ptr, mult);
}

/*
 * If a power has a negative cost, the cost given is a reference to this table.
 * This can only express minc+plev/levc at present, but could be replaced by a
 * more complex expression, or by pointers to a string and a function to allow
 * arbitrary functions.
 */

typedef struct powercosttype powercosttype;
struct powercosttype
{
	byte minc;
	byte levc;
};
static powercosttype powercosts[] = {
{0,0},{0,1},
{0,0},{1,3},
{0,0},{5,5},
};

/* The methods by which a power may be derived. */
#define POWER_RACIAL 1 /* Relevant if (p_ptr->prace == power % MAX_RACES). */
#define POWER_MUTA 2 /* Relevant if (p_has_mutation(power). */
#define POWER_DISMISS 3 /* Relevant if you have pets. */

/* The string will be turned into a longer form later.
 * In general, this is text+"     "+"(racial, cost power->cost, power->use_stat power->difficulty)".
 * If power->cost is negative, the cost is taken from powercosts[-power->cost].
 * If text2 isn't 0, it is added as a separate statement after the cost, separated from that and
 * power->use_stat by commas.
 * The number of spaces will be adjusted as necessary.
 */
/*
 * The table of powers, as defined above. Note that the effects of powers must be described in use_innate_power().
 */
static race_power pet_powers[2] =
{
	{PO_PETS+PET_DISMISS_ONE, 0, 0, 0, 0, "dismiss ally",0, 0},
	{PO_PETS+PET_DISMISS_MANY, 0, 0, 0, 0, "dismiss allies",0, 0},
};

static race_power mut_powers[] =
{
	{PO_MUTA+MUT_SPIT_ACID, 9, 9, A_DEX, 15,
		"spit acid", "dam lvl", "You spit acid..."},
	{PO_MUTA+MUT_BR_FIRE, 20, -1, A_CON, 18,
		"fire breath", "dam lvl*2", "You breathe fire..."},
	{PO_MUTA+MUT_HYPN_GAZE, 12, 12, A_CHR, 18,
		"hypnotic gaze", 0, "Your eyes look mesmerizing..."},
	{PO_MUTA+MUT_TELEKINES, 9, 9, A_WIS, 14,
		"telekinesis", 0, "You concentrate..."},
	{PO_MUTA+MUT_VTELEPORT, 7, 7, A_WIS, 15,
		"teleport", 0, "You concentrate..."},
	{PO_MUTA+MUT_MIND_BLST, 5, 3, A_WIS, 15,
		"mind blast", 0, "You concentrate..."},
	{PO_MUTA+MUT_RADIATION, 15, 15, A_CON, 14,
		"emit radiation", 0, "Radiation flows from your body!"},
	{PO_MUTA+MUT_VAMPIRISM, 13, -1, A_CON, 14, "vampiric drain",0, 0},
	{PO_MUTA+MUT_SMELL_MET, 3, 2, A_INT, 12, "smell metal",0, 0},
	{PO_MUTA+MUT_SMELL_MON, 5, 4, A_INT, 15, "smell monsters",0, 0},
	{PO_MUTA+MUT_BLINK, 3, 3, A_WIS, 12, "blink",0, 0},
	{PO_MUTA+MUT_EAT_ROCK, 8, 12, A_CON, 18, "eat rock",0, 0},
	{PO_MUTA+MUT_SWAP_POS, 15, 12, A_DEX, 16, "swap position",0, 0},
	{PO_MUTA+MUT_SHRIEK, 4, 4, A_CON, 6, "shriek",0, 0},
	{PO_MUTA+MUT_ILLUMINE, 3, 2, A_INT, 10, "illuminate",0, 0},
	{PO_MUTA+MUT_DET_CURSE, 7, 14, A_WIS, 14, "detect curses",0, 0},
	{PO_MUTA+MUT_BERSERK, 8, 8, A_STR, 14, "berserk",0, 0},
	{PO_MUTA+MUT_POLYMORPH, 18, 20, A_CON, 18, "polymorph",0, 0},
	{PO_MUTA+MUT_MIDAS_TCH, 10, 5, A_INT, 12, "midas touch",0, 0},
	{PO_MUTA+MUT_GROW_MOULD, 1, 6, A_CON, 14, "grow mould",0, 0},
	{PO_MUTA+MUT_RESIST, 10, 12, A_CON, 12, "resist elements",0, 0},
	{PO_MUTA+MUT_EARTHQUAKE, 12, 12, A_STR, 16, "earthquake",0, 0},
	{PO_MUTA+MUT_EAT_MAGIC, 17, 1, A_WIS, 15, "eat magic",0, 0},
		{PO_MUTA+MUT_WEIGH_MAG, 6, 6, A_INT, 10, "weigh magic",0, 0},
	{PO_MUTA+MUT_STERILITY, 20, 40, A_CHR, 18,
		"sterilize", 0, "You suddenly have a headache!"},
	{PO_MUTA+MUT_PANIC_HIT, 10, 12, A_DEX, 14, "panic hit",0, 0},
	{PO_MUTA+MUT_DAZZLE, 7, 15, A_CHR, 8, "dazzle",0, 0},
	{PO_MUTA+MUT_EYE_BEAM, 7, 10, A_WIS, 9, "eye beams",0, 0},
	{PO_MUTA+MUT_RECALL, 17, 50, A_INT, 16, "recall",0, 0},
	{PO_MUTA+MUT_BANISH, 25, 25, A_WIS, 18, "banish evil",0, 0},
	{PO_MUTA+MUT_COLD_TOUCH, 2, 2, A_CON, 11, "cold touch",0, 0},
	{PO_MUTA+MUT_LAUNCHER, 1, -1, A_STR, 6, "throw object",0, 0}
};
#define MAX_POWERS 36 /* There shouldn't be more powers than this at a time. */
static race_power *cur_powers[MAX_POWERS];


#define RACIAL_MIN_X 13 /* The distance across the screen at which the display starts. */
#define MIN_STRING_LEN 22 /* The space allowed for all description strings. Longer strings are still longer. */

/*
 * Actually put a racial or mutation-based activation into effect.
 * See the individual entries for details.
 *
 * Note that there is no special treatment for aborted powers.
 */
static void use_innate_power(race_power *pw_ptr)
{
	int plev = MAX(skill_set[SKILL_RACIAL].value/2,1);

	/* Print a message, if desired. */
	if (pw_ptr->atext) msg_print(pw_ptr->atext);

	/* Use the power. */
	use_known_power(pw_ptr->idx, plev);
}

/*
 * Determine the chance of using a given power based on features of the power and the player.
 * The power should succeed if rand_int((*denom))<(*num).
 */
static void racial_success_chance(race_power *pw_ptr, s16b *num, s16b *denom)
{
	int difficulty = pw_ptr->difficulty;


	const int plev = MAX(1,skill_set[SKILL_RACIAL].value/2);
	const int stat = p_ptr->stat_cur[pw_ptr->use_stat];

	/* Should never happen, but... */
	if (difficulty > 100)
	{
		difficulty = 100;
	}

	/* Adjust by racial skill */
	if (plev > pw_ptr->min_level + 30) difficulty -= 10;
	else difficulty += (pw_ptr->min_level-plev)/3;

	/* Put an upper limit on difficulty. */
	if (difficulty > 100) difficulty = 100;

	/* Stunning is excluded from the upper limit calculation, but included
	 * in the lower limit one. */
	if (p_ptr->stun) difficulty += p_ptr->stun;

	/* Put a lower limit on difficulty. */
	if (difficulty < 4) difficulty = 4;

	/* Halve it to approximate a range from difficulty/2 to difficulty below. */
	difficulty /= 2;

	/* Calculate the numerator. */
	if (p_ptr->confused)
		(*num) = 0;
	else if (stat > difficulty * 2)
		(*num) = difficulty*(2*stat-3*difficulty+1)/2;
	else if (stat > difficulty)
		(*num) = (stat-difficulty)*(stat-difficulty+1)/2;
	else
		(*num = 0);

	/* Calculate the denominator */
	(*denom) = stat*difficulty;
}

/*
 * Work out the style of a power for various purposes.
 */
static int power_method(race_power *pw_ptr)
{
	/* Pet powers are in an array. */
	if (pw_ptr >= pet_powers && pw_ptr < END_PTR(pet_powers))
	{
		return POWER_DISMISS;
	}

	/* Mutation powers are in an array. */
	if (pw_ptr >= mut_powers && pw_ptr < END_PTR(mut_powers))
	{
		return POWER_MUTA;
	}

	/* Racial powers are more awkward to find, but they're the only choices. */
	return POWER_RACIAL;
}

/*
 * Determine the cost of a racial power.
 */
int power_cost(const race_power *pw_ptr, int lev)
{
	if (pw_ptr->cost > 0)
	{
		return pw_ptr->cost;
	}
	else
	{
		powercosttype *pc_ptr = &powercosts[-pw_ptr->cost];

		/* Paranoia - division by 0? */
		if (!(pc_ptr->levc))
		{
			return 999;
		}
		else
		{
			/* Return the numerical cost. */
			return lev / pc_ptr->levc + pc_ptr->minc;
		}
	}
}

/*
 * Describe a power for do_cmd_racial_power.
 * In general, this becomes text+"     "+"(racial, cost power->cost, power->use_stat power->difficulty)".
 * If power->cost is negative, the cost is taken from powercosts[-power->cost].
 * If there is a ; in the string, the text immediately after it is added as a separate statement after
 * the cost, separated from that and power->use_stat by commas. This can contain semi-colons.
 * The number of spaces will be adjusted as necessary.
 * Display the powers available in choose_power()
 */
static void racial_string(byte idx, byte *x, char * text)
{
	race_power *pw_ptr = cur_powers[idx];
	int type = power_method(pw_ptr);

	/* Set the distance from the left margin. */
	(*x) = RACIAL_MIN_X;

	sprintf(text, ")  %s", pw_ptr->text);
	if (type == POWER_DISMISS)
	{
		sprintf(text, ")  %s", pw_ptr->text);
	}
	/* Racial and mutated powers have a more complex format. */
	else
	{
		cptr racstr = (type == POWER_RACIAL) ? "racial, " : "";
		cptr t2str1 = (pw_ptr->text2) ? pw_ptr->text2 : "";
		cptr t2str2 = (pw_ptr->text2) ? ", " : "";
		s16b num, denom;
		int perc;
		const int plev = MAX(1,skill_set[SKILL_RACIAL].value/2);
		const int cost = power_cost(pw_ptr, plev);

		racial_success_chance(pw_ptr, &num, &denom);

		perc = 100-num*100/denom;

		sprintf(text, ")  %-*s(%scost %d, %s%s%.3s %d %d%%)",
			MIN_STRING_LEN, pw_ptr->text, racstr, cost, t2str1, t2str2,
			stat_names[pw_ptr->use_stat], pw_ptr->difficulty, perc);
	}
}

/*
 * The confirmation string for do_cmd_racial_power.
 */
static void racial_confirm_string(byte choice, char * out)
{
	/* Prompt */
	strnfmt(out, 78, "Use %s? ", cur_powers[choice]->text);
}

/*
 * Count the powers the player has.
 */
static int count_powers(void)
{
	int i, pets, total;

	for (pets = 0, i = 1; (i < m_max) && (pets < 2); i++)
	{
		monster_type *m_ptr = &m_list[i];
		if (m_ptr->smart & SM_ALLY) pets++;
	}

	/* Add in the racial powers. */
	for (total = 0; total < rp_ptr->powers; total++)
	{
		cur_powers[total] = rp_ptr->power+total;
	}

	/* Add in the mutation-based powers. */
	for (i = 0; i < (int)N_ELEMENTS(mut_powers) && (total < MAX_POWERS); i++)
	{
		race_power *ptr = &mut_powers[i];
		if (p_has_mutation(ptr->idx - PO_MUTA))
			cur_powers[total++] = mut_powers+i;
	}

	/* Add in the pet power for the current number of pets, if any. */
	if (pets == 1)
	{
		cur_powers[total++] = pet_powers+0;
	}
	else if (pets)
	{
		cur_powers[total++] = pet_powers+1;
	}

	return total;
}

/*
 * A generic menu display function for lists of options such as a spell list.
 * "display" must set the x co-ordinate for the display and return the string which is to be displayed
 * to describe option "num" when the full list is displayed.
 * "confirm" must display a prompt asking for confirmation of option "choice" and return the
 * String1 and string2 are configurable strings in the initial prompt.
 * string contains the string displayed at the top of the screen when the command is called.
 * (*display)(byte num) is a pointer to the function which describes an option, taking its index.
 * number as a parameter and returning a string.
 * (*confirm)(byte num) is a pointer to the function which asks for confirmation of a selection, taking the index
 * of the option as a parameter. It should return the player's response.
 * The function returns the index of the option chosen, 255 if none are chosen.
 */
static byte display_list(void (*display)(byte, byte *, char *), void (*confirm)(byte, char *), byte num, cptr string1, cptr string2, cptr substring)
{
	bool ask, started = FALSE;
	char choice;
	int select, first = 0, last = 0;
	int page = (substring) ? 22 : 23;

	while (1)
	{
		if (started || !show_choices_main)
		{
			(void)get_com(&choice, "(%s %c-%c, *=List %s, ESC=exit) %s ",
				string1, I2A(0), (num > 26) ? I2A(MIN(num-1-first, page-1)) :
				I2A(num - 1), (!last) ? "Open" : (last < num) ? "More" :
				"Close", string2);
		}
		else
		{
			/* Hack - pretend the player pressed space initially given show_choices_main. */
			choice = ' ';
			started = TRUE;
		}
		switch (choice)
		{
			/* Don't choose anything. */
			case ESCAPE:
			{
				if (last) Term_load();
				return 255;
			}
			/* Request redraw */
			case ' ': case '*': case '?':
			{
				/* Avoid leaving old text on the screen. */
				if (last) Term_load();

				if (last < num)
				{
					byte y, x, ctr;
					first = last;
					y = 24-page;
					if (substring) prt(substring, 1, x);

					if (!first) Term_save();
					for (ctr = 0; ctr < num-first && ctr < page; ctr++)
					{
						char text[160];
						(*display)(last+ctr, &x, text+1);
						/* Use a-z if possible, but use the currently displayed page otherwise. */
						if (num > 26)
							text[0] = I2A(ctr);
						else
							text[0] = I2A(ctr+last);
						prt(text, y+ctr, x);
					}
					prt ("", y+ctr, x);
					last += ctr;
				}
				else
				{
					first = last = 0;
				}
				break;
			}
			/* Choose any default option. */
			case '\r':
			{
				if (num != 1) break;
				choice = 'a';
			}
			/* Possible valid choices. */
			case 'a': case 'b': case 'c': case 'd': case 'e': case 'f': case 'g': case 'h':
			case 'i': case 'j': case 'k': case 'l': case 'm': case 'n': case 'o': case 'p':
			case 'q': case 'r': case 's': case 't': case 'u': case 'v': case 'w': case 'x':
			case 'y': case 'z': case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
			case 'G': case 'H': case 'I': case 'J': case 'K': case 'L': case 'M': case 'N':
			case 'O': case 'P': case 'Q': case 'R': case 'S': case 'T': case 'U': case 'V':
			case 'W': case 'X': case 'Y': case 'Z':
			{
				ask = ISUPPER(choice);
				select = A2I(FORCELOWER(choice));
				/* If there are more than 26 entries, adjust for the current position of 'a'.*/
				if (num > 26) select += first;
				/* A plausible choice, */
				if (select < num)
				{
					if (ask) /* Need confirmation. */
					{
						char tmp_val[160];
						(*confirm)(select, tmp_val);
						if (!get_check(tmp_val)) select = 255;
					}
					if (last) Term_load();
					return select;
				}
				/* else fall through. */
			}
			/* Not a valid choice. */
			default:
			{
				bell(0);
			}
		}
	}
}

/*
 * Decide whether a power has been used successfully.
 */
static bool racial_aux(race_power *pw_ptr)
{
	const int plev = MAX(1,skill_set[SKILL_RACIAL].value/2);
	bool use_hp = FALSE;
	bool use_chi = FALSE;
	bool use_mana = TRUE;
	s16b num, denom;
	int cost = power_cost(pw_ptr, plev);

	if (p_ptr->cchi >= p_ptr->csp)
	{
		use_mana = FALSE;
		use_chi = TRUE;
		if (p_ptr->cchi < cost)
		{
			use_chi = FALSE;
			use_hp = TRUE;
		}
	}
	else
	{
		if (p_ptr->csp < cost)
		{
			use_mana = FALSE;
			use_hp = TRUE;
		}
	}


	if (p_ptr->confused)
	{
		msg_print("You are too confused to use this power.");
		return FALSE;
	}

	else if (use_hp && (p_ptr->chp < cost))
	{
		if (!(get_check("Really use the power in your weakened state? ")))
		{
			return FALSE;
		}
	}

	/* Else attempt to do it! */

	/* take time and pay the price */
	energy_use = spell_energy(plev,pw_ptr->min_level);

	/* Hack - use num to store the actual cost of the attempt. */
	num = rand_range(cost/2+1, cost/2*2);

	if (use_hp)
	{
		take_hit (num, "concentrating too hard", MON_CONCENTRATING_TOO_HARD);
		p_ptr->redraw |= PR_HP;
	}
	else if (use_mana)
	{
		p_ptr->csp -= num;
		p_ptr->redraw |= PR_MANA;
	}
	else
	{
		p_ptr->cchi -= num;
		p_ptr->redraw |= PR_MANA;
	}

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER);


	/* Success? */
	racial_success_chance(pw_ptr, &num, &denom);
	if (rand_int(denom) < num)
	{
		skill_exp(SKILL_RACIAL);
		return TRUE;
	}
	else
	{
		msg_print("You've failed to concentrate hard enough.");
	}
	return FALSE;
}

/*
 * Use a racial/mutated power.
 */
void do_cmd_racial_power(void)
{
	/* Count the available powers. */
	byte total = count_powers();

	/* Assume free for now. Will be changed if the player attempts to use a power. */
	energy_use = 0;

	if (!total)
	{
		msg_print("You have no abilities to use.");
		return;
	}

	/* Display the available powers */
	total = display_list(racial_string, racial_confirm_string, total, "Powers", "Use which ability?", 0);

	/* Something has been selected */
	if (total != 255)
	{
		race_power *pw_ptr = cur_powers[total];

		switch (power_method(pw_ptr))
		{
			case POWER_RACIAL: case POWER_MUTA:
			/* Try to use the power. */
			if (racial_aux(pw_ptr))
				use_innate_power(pw_ptr);
			break;
			case POWER_DISMISS:
			use_innate_power(pw_ptr);
			break;
			default:
			msg_print("You don't know what that does.");
		}
	}
}

