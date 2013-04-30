/* File: melee1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this cofxright and statement
 * are included in all such copies.  Other cofxrights may also apply.
 *
 * UnAngband (c) 2001-3 Andrew Doull. Modifications to the Angband 2.9.1
 * source code are released under the Gnu Public License. See www.fsf.org
 * for current GPL license details. Addition permission granted to
 * incorporate modifications in all Angband variants as defined in the
 * Angband variants FAQ. See rec.games.roguelike.angband for FAQ.
 */

#include "angband.h"



/*
 * Critical blows by monsters can inflict cuts and stuns.
 */
static int monster_critical(int dice, int sides, int dam, int effect)
{
	int max = 0;
	int bonus;
	int total = dice * sides;


	/* Special case -- wounding/battering attack */
	if ((effect == GF_WOUND) || (effect == GF_BATTER))
	{
		/* Must do at least 70% of perfect */
		if (dam < total * 7 / 10) return (0);

		max = 1;
	}

	/* Standard attack */
	else
	{
		/* Weak blows rarely work */
		if ((rand_int(20) >= dam) || (!rand_int(3))) return (0);

		/* Must do at least 90% of perfect */
		if (dam < total * 9 / 10) return (0);
	}


	/* Perfect damage */
	if (dam == total) max++;

	/* Get bonus to critical damage (never greater than 6) */
	bonus = MIN(6, dam / 8);

	/* Critical damage  (never greater than 6 + max) */
	return (randint(bonus) + max);
}




/*
 * Determine if a monster attack against the player succeeds.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match monster power against player armor.
 */
static int check_hit(int power, int level, int m_idx)
{
	int i, k, ac;

	monster_type *m_ptr = &m_list[m_idx];

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Always miss or hit */
	if (k < 10) return (k < 5);

	/* Calculate the "attack quality".  Stunned monsters are hindered. */
	i = (power + (m_ptr->stunned ? level * 2 : level * 3));

	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Some rooms make the player vulnerible */
	if (cave_info[p_ptr->py][p_ptr->px] & (CAVE_ROOM))
	{
		/* Special rooms affect some of this */
		int by = p_ptr->py/BLOCK_HGT;
		int bx = p_ptr->px/BLOCK_HGT;

		/* Get the room */
		if(room_info[dun_room[by][bx]].flags & (ROOM_CURSED))
		{
			/* Halve the effective armor class */
			ac /= 2;
		}
	}							    

	/* Power and Level compete against Armor */
	if ((i > 0) && (randint(i) > ((ac * 3) / 4))) return (TRUE);

	/* Assume miss */
	return (FALSE);
}


/*
 * Hack -- possible "insult" messages
 */
static cptr desc_insult[] =
{
	"insults you!",
	"insults your mother!",
	"gives you the finger!",
	"humiliates you!",
	"defiles you!",
	"dances around you!",
	"makes obscene gestures!",
	"moons you!!!"
};



/*
 * Hack -- possible "insult" messages
 */
static cptr desc_moan[] =
{
	"seems sad about something.",
	"asks if you have seen his dogs.",
	"tells you to get off his land.",
	"mumbles something about mushrooms."
};


/*
 * Attack the player via physical attacks.
 */
bool make_attack_normal(int m_idx)
{
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];
	monster_lore *l_ptr = &l_list[m_ptr->r_idx];

	int ap_cnt;

	int tmp, ac, rlev;
	int do_cut, do_stun, touched;

	char m_name[80];

	char ddesc[80];

	bool blinked;


	/* Not allowed to attack */
	if (r_ptr->flags1 & (RF1_NEVER_BLOW)) return (FALSE);


	/* Total armor */
	ac = p_ptr->ac + p_ptr->to_a;

	/* Extract the effective monster level */
	rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);


	/* Get the monster name (or "it") */
	monster_desc(m_name, m_ptr, 0);

	/* Get the "died from" information (i.e. "a goblin") */
	monster_desc(ddesc, m_ptr, 0x88);


	/* Assume no blink */
	blinked = FALSE;

	/* Scan through all four blows */
	for (ap_cnt = 0; ap_cnt < 4; ap_cnt++)
	{
		bool visible = FALSE;
		bool obvious = FALSE;

		int power = 0;
		int damage = 0;

		cptr act = NULL;

		/* Extract the attack infomation */
		int effect = r_ptr->blow[ap_cnt].effect;
		int method = r_ptr->blow[ap_cnt].method;
		int d_dice = r_ptr->blow[ap_cnt].d_dice;
		int d_side = r_ptr->blow[ap_cnt].d_side;


		/* Hack -- no more attacks */
		if (!method) break;


		/* Handle "leaving" */
		if (p_ptr->leaving) break;

		/* Extract visibility (before blink) */
		if (m_ptr->ml) visible = TRUE;

		/* Skip 'tricky' attacks */
		if (method > RBM_MAX_NORMAL) continue;

		/* Assume no cut or stun or touched */
		do_cut = do_stun = touched = 0;

		/* Extract the attack "power". Elemental attacks upgraded. */
		switch (effect)
		{
			case GF_HURT: power = 60; break;
			case GF_WOUND: power = 60; break;
			case GF_BATTER: power = 60; break;
			case GF_SHATTER: power = 60; break;

			case GF_UN_BONUS:	power = 20; break;
			case GF_UN_POWER:	power = 15; break;
			case GF_LOSE_MANA: power = 45; break;
			case GF_EAT_GOLD:	power =  5; break;
			case GF_EAT_ITEM:	power =  5; break;
			case GF_EAT_FOOD:	power =  45; break;
			case GF_EAT_LITE:	power =  45; break;
			case GF_HUNGER: power = 45; break;

			case GF_POIS:	power =  25; break;
			case GF_ACID:		power = 50; break;
			case GF_ELEC:		power = 50; break;
			case GF_FIRE:		power = 50; break;
			case GF_COLD:		power = 50; break;

			case GF_BLIND:	power =  5; break;
			case GF_CONFUSION:	power = 10; break;
			case GF_TERRIFY:	power = 10; break;
			case GF_PARALYZE:	power =  5; break;
			case GF_HALLU:     power = 10; break;
			case GF_DISEASE:	power = 10; break;

			case GF_LOSE_STR:	power =  0; break;
			case GF_LOSE_DEX:	power =  0; break;
			case GF_LOSE_CON:	power =  0; break;
			case GF_LOSE_INT:	power =  0; break;
			case GF_LOSE_WIS:	power =  0; break;
			case GF_LOSE_CHR:	power =  0; break;
			case GF_LOSE_ALL:	power =  2; break;

			case GF_EXP_10:	power =  5; break;
			case GF_EXP_20:	power =  5; break;
			case GF_EXP_40:	power =  5; break;
			case GF_EXP_80:	power =  5; break;

			/* Need to add extra flavours in here */
		}

		/* Roll out the damage */
		damage = damroll(d_dice, d_side);

		/* Describe the attack method */
		switch (method)
		{
			case RBM_HIT:
			{
				/* Handle special effect types */
				if (effect == GF_WOUND)
				{
					if      (damage >= 30) act = "gouges you";
					else if (damage >= 20) act = "slashes you";
					else if (damage >= 5)  act = "cuts you";
					else                act = "scratches you";

					/* Usually don't stun */
					if (!rand_int(5)) do_stun = 1;

					do_cut = touched = 1;
				}
				else if (effect == GF_BATTER)
				{
					if      (damage >= 30) act = "bludgeons you";
					else if (damage >= 20) act = "batters you";
					else if (damage >= 5)  act = "bashes you";
					else                act = "hits you";

					/* Usually don't cut */
					if (!rand_int(5)) do_cut = 1;

					do_stun = touched = 1;
				}
				else
				{
					act = "hits you";
					do_cut = do_stun = touched = 1;
				}

				break;
			}

			case RBM_TOUCH:
			{
				act = "touches you";
				touched = 1;
				break;
			}

			case RBM_PUNCH:
			{
				act = "punches you";
				do_stun = touched = 1;
				break;
			}

			case RBM_KICK:
			{
				act = "kicks you";
				do_stun = touched = 1;
				break;
			}

			case RBM_CLAW:
			{
				if      (damage >= 25) act = "slashes you";
				else if (damage >=  5) act = "claws you";
				else                act = "scratches you";
				do_cut = touched = 1;
				break;
			}

			case RBM_BITE:
			{
				if (damage >= 5) act = "bites you";
				else          act = "nips you";
				do_cut = touched = 1;
				break;
			}

			case RBM_PECK:
			{
				act = "pecks you";
				do_stun = touched = 1;
				break;
			}

			case RBM_STING:
			{
				act = "stings you";
				touched = 1;
				break;
			}

			case RBM_VOMIT:
			{
				act = "vomits on you";
				touched = 1;
				break;
			}

			case RBM_BUTT:
			{
				if (damage >= rand_range(10, 20)) act = "tramples you";
				else                           act = "butts you";
				do_stun = touched = 1;
				break;
			}

			case RBM_CRUSH:
			{
				if (damage >= 10) act = "crushes you";
				else           act = "squeezes you";
				do_stun = touched = 1;
				break;
			}

			case RBM_ENGULF:
			{
				if (damage >= randint(50)) act = "envelops you";
				else                    act = "engulfs you";
				touched = 1;
				break;
			}

			case RBM_CRAWL:
			{
				act = "crawls on you";
				touched = 1;
				break;
			}

			case RBM_DROOL:
			{
				act = "drools on you";
				break;
			}

			case RBM_SLIME:
			{
				act = "slimes you!";
				break;
			}

			case RBM_SPIT:
			{
				act = "spits on you";
				break;
			}

			case RBM_GAZE:
			{
				if      (damage >= rand_range(20, 30))
					act = "glares at you terribly";
				else if (damage >= rand_range(5, 30))
					act = "gazes upon you";
				else act = "gazes at you";
				break;
			}

			case RBM_WAIL:
			{
				act = "wails horribly";
				break;
			}

			case RBM_SPORE:
			{
				act = "releases a cloud of spores";
				break;
			}

			case RBM_LASH:
			{
				act = "lashes you with a whip";
				touched = 1;
				break;
			}

			case RBM_BEG:
			{
				act = "begs you for money";
				break;
			}

			case RBM_INSULT:
			{
				act = desc_insult[rand_int(8)];
				break;
			}

			case RBM_MOAN:
			{
				act = desc_moan[rand_int(4)];
				break;
			}
		}

		/* Monster hits player */
		if (!effect || check_hit(power, rlev, m_idx))
		{
			/* Always disturbing */
			disturb(1, 0);

			/* Hack -- Apply "protection from evil" */
			if ((p_ptr->protevil > 0) &&
			    (r_ptr->flags3 & (RF3_EVIL)) &&
			    (p_ptr->lev >= rlev) &&
			    ((rand_int(100) + p_ptr->lev) > 50))
			{
				/* Remember the Evil-ness */
				if (m_ptr->ml)
				{
					l_ptr->flags3 |= (RF3_EVIL);
				}

				/* Message */
				msg_format("%^s is repelled.", m_name);

				/* Hack -- Next attack */
				continue;
			}


			/* Message */
			if (act)
			{
				if (damage > p_ptr->chp / 3)
					msg_format("%^s %s!", m_name, act);
				else
					msg_format("%^s %s.", m_name, act);
			}

			/* Check for usage */
			if (rand_int(100)<damage)
			{
				int slot;

				/* Pick a (possibly empty) inventory slot */
				switch (randint(6))
				{
					case 1: slot = INVEN_BODY; break;
					case 2: slot = INVEN_ARM; break;
					case 3: slot = INVEN_OUTER; break;
					case 4: slot = INVEN_HANDS; break;
					case 5: slot = INVEN_HEAD; break;
					case 6: slot = INVEN_FEET; break;
				}

				/* Object used? */
				object_usage(slot);
			}

			if (effect)
			{
				/* New result routine */
				obvious = project_p(m_idx,0,p_ptr->py,p_ptr->px,damage,effect);
			}
			else
			{
				obvious = TRUE;
			}

			/* Hack -- only one of cut or stun */
			if (do_cut && do_stun)
			{
				/* Cancel cut */
				if (rand_int(100) < 50)
				{
					do_cut = 0;
				}

				/* Cancel stun */
				else
				{
					do_stun = 0;
				}
			}

			/* Handle cut */
			if (do_cut)
			{
				int k;

				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d_dice, d_side, damage, effect);

				/* Roll for damage */
				switch (tmp)
				{
					case 0: k = 0; break;
					case 1: k = randint(5); break;
					case 2: k = randint(5) + 5; break;
					case 3: k = randint(20) + 20; break;
					case 4: k = randint(50) + 50; break;
					case 5: k = randint(100) + 100; break;
					case 6: k = 300; break;
					default: k = 500; break;
				}

				/* Apply the cut */
				if (k) (void)set_cut(p_ptr->cut + k);
			}

			/* Handle stun */
			if (do_stun)
			{
				int k;

				/* Critical hit (zero if non-critical) */
				tmp = monster_critical(d_dice, d_side, damage, effect);

				/* Roll for damage */
				switch (tmp)
				{
					case 0: k = 0; break;
					case 1: k = randint(5); break;
					case 2: k = randint(8) + 8; break;
					case 3: k = randint(15) + 15; break;
					case 4: k = randint(25) + 25; break;
					case 5: k = randint(35) + 35; break;
					case 6: k = randint(45) + 45; break;
					default: k = 100; break;
				}

				/* Apply the stun */
				if (k) (void)set_stun(p_ptr->stun + k);
			}
		}

		/* Monster missed player */
		else if (touched)
		{
			/* Visible monsters */
			if (m_ptr->ml)
			{
				/* Disturbing */
				disturb(1, 0);

				/* Message */
				msg_format("%^s misses you.", m_name);
			}
		}


		/* Analyze "visible" monsters only */
		if (visible)
		{
			/* Count "obvious" attacks (and ones that cause damage) */
			if (obvious || damage || (l_ptr->blows[ap_cnt] > 10))
			{
				/* Count attacks of this type */
				if (l_ptr->blows[ap_cnt] < MAX_UCHAR)
				{
					l_ptr->blows[ap_cnt]++;
				}
			}
		}
	}


	/* Blink away */
	if (blinked)
	{
		msg_print("There is a puff of smoke!");
		teleport_away(m_idx, MAX_SIGHT * 2 + 5);
	}


	/* Always notice cause of death */
	if (p_ptr->is_dead && (l_ptr->deaths < MAX_SHORT))
	{
		l_ptr->deaths++;
	}


	/* Assume we attacked */
	return (TRUE);
}



/*
 * Handle monster hitting a real trap.
 */
void mon_hit_trap(int m_idx, int y, int x)
{
	feature_type *f_ptr;
	monster_type *m_ptr = &m_list[m_idx];
	monster_race *r_ptr = &r_info[m_ptr->r_idx];

	int feat = cave_feat[y][x];

	bool fear;

	/* Option */
	if (!variant_hit_traps) return;

	/* Hack --- don't activate unknown invisible traps */
	if (cave_feat[y][x] == FEAT_INVIS) return;

	/* Get feature */
	f_ptr = &f_info[cave_feat[y][x]];

	/* Hack --- trapped doors */
	/* XXX XXX Dangerous */
	while (!(f_ptr->spell) && !(f_ptr->blow.method) && (f_ptr->flags1 & (FF1_TRAP)))
	{
		pick_trap(y,x);

		/* Error */
		if (cave_feat[y][x] == feat) break;

		feat = cave_feat[y][x];

		/* Get feature */
		f_ptr = &f_info[feat];

	}

	/* Use covered or bridged if necessary */
	if ((f_ptr->flags2 & (FF2_COVERED)) || (f_ptr->flags2 & (FF2_BRIDGED)))
	{
		f_ptr = &f_info[f_ptr->mimic];
	}

	/* Hack -- monster falls onto trap */
	if ((m_ptr->fy!=y)|| (m_ptr->fx !=x))
	{
		/* Move monster */
		monster_swap(m_ptr->fy, m_ptr->fx, y, x);
	}

	/* Apply the object */
	if ((cave_o_idx[y][x]) && (f_ptr->flags1 & (FF1_HIT_TRAP)))
	{
		object_type *o_ptr = &o_list[cave_o_idx[y][x]];

		char o_name[80];

		int power = 0;

		switch (o_ptr->tval)
		{
			case TV_BOW:
			{
				object_type *j_ptr;
				u32b f1,f2,f3;

				int i, shots = 1;

				/* Get bow */
				j_ptr = o_ptr;

				/* Get bow flags */
				object_flags(o_ptr,&f1,&f2,&f3);

				/* Apply extra shots */
				if (f1 & (TR1_SHOTS)) shots += j_ptr->pval;

				/* Test for hit */
				for (i = 0; i < shots; i++)
				{
					if (j_ptr->next_o_idx)
					{
						int ammo = j_ptr->next_o_idx;
						object_type *i_ptr;
						object_type object_type_body;

						/* Use ammo instead of bow */
						o_ptr = &o_list[ammo];

						/* Describe ammo */
						object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);

						if ((ammo) && (test_hit_fire((j_ptr->to_h + o_ptr->to_h)* BTH_PLUS_ADJ + f_ptr->power,  r_ptr->ac * (r_ptr->flags2 & (RF2_ARMOR) ? 2 : 1), TRUE)))
						{
							int k, mult;

							switch (j_ptr->sval)
							{
								case SV_SLING:
								case SV_SHORT_BOW:
								mult = 2;
								break;
								case SV_LONG_BOW:
								case SV_LIGHT_XBOW:
									mult = 3;
									break;
								case SV_HEAVY_XBOW:
									mult = 4;
									break;
								default:
									mult = 1;
									break;
							}

							/* Apply extra might */
							if (f1 & (TR1_MIGHT)) mult += j_ptr->pval;

							k = damroll(o_ptr->dd, o_ptr->ds);
							k *= mult;

							k = tot_dam_aux(o_ptr, k, m_ptr);

							k = critical_shot(o_ptr->weight, o_ptr->to_h + j_ptr->to_h, k);
							k += o_ptr->to_d + j_ptr->to_d;

							/* No negative damage */
							if (k < 0) k = 0;

							/* Trap description */
							msg_format("%^s hits you.",o_name);

							/* Damage, check for fear and death */
							(void)mon_take_hit(cave_m_idx[y][x], k, &fear, NULL);

						}
						else
						{
							/* Trap description */
							msg_format("%^s narrowly misses you.",o_name);
						}

						/* Get local object */
						i_ptr = &object_type_body;

						/* Obtain a local object */
						object_copy(i_ptr, o_ptr);

						/* Modify quantity */
						i_ptr->number = 1;

						/* Drop nearby - some chance of breakage */
						drop_near(i_ptr,y,x,breakage_chance(i_ptr));

						/* Decrease the item */
						floor_item_increase(ammo, -1);
						floor_item_optimize(ammo);

						break;
					}
					else
					{
						/* Disarm */
						cave_alter_feat(y,x,FS_DISARM);
					}
				}
			}

			case TV_SHOT:
			case TV_ARROW:
			case TV_BOLT:
			case TV_HAFTED:
			case TV_SWORD:
			case TV_POLEARM:
			{
				object_type *i_ptr;
				object_type object_type_body;

				/* Describe ammo */
				object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);

				/* Test for hit */
				if (test_hit_norm(o_ptr->to_h * BTH_PLUS_ADJ + f_ptr->power, r_ptr->ac, TRUE))
				{
					int k;

					k = damroll(o_ptr->dd, o_ptr->ds);

					k = tot_dam_aux(o_ptr, k, m_ptr);

					k = critical_norm(o_ptr->weight, o_ptr->to_h, k);
					k += o_ptr->to_d;

					/* Armour reduces total damage */
					k -= (k * ((p_ptr->ac < 150) ? p_ptr->ac : 150) / 250);

					/* No negative damage */
					if (k < 0) k = 0;

					/* Trap description */
					msg_format("%^s hits you.",o_name);

					/* Damage, check for fear and death */
					(void)mon_take_hit(cave_m_idx[y][x], k, &fear, NULL);

				}
				else
				{
					/* Trap description */
					msg_format("%^s narrowly misses you.",o_name);					
				}

				/* Get local object */
				i_ptr = &object_type_body;

				/* Obtain a local object */
				object_copy(i_ptr, o_ptr);

				/* Modify quantity */
				i_ptr->number = 1;

				/* Drop nearby - some chance of breakage */
				drop_near(i_ptr,y,x,breakage_chance(i_ptr));

				/* Decrease the item */
				floor_item_increase(cave_o_idx[y][x], -1);
				floor_item_optimize(cave_o_idx[y][x]);

				/* Disarm if runs out */
				if (!cave_o_idx[y][x]) cave_alter_feat(y,x,FS_DISARM);

				break;
			}

			case TV_WAND:
			case TV_STAFF:
			{
				if (o_ptr->pval > 0)
				{
					/* Get item effect */
					get_spell(&power, "use", o_ptr, FALSE);

					/* XXX Hack -- new unstacking code */
					o_ptr->stackc++;

					/* No spare charges */	
					if (o_ptr->stackc >= o_ptr->number)
					{
						/* Use a charge off the stack */
						o_ptr->pval--;

						/* Reset the stack count */
						o_ptr->stackc = 0;
					}

					/* XXX Hack -- unstack if necessary */
					if ((o_ptr->number > 1) &&
					((!variant_pval_stacks) || 
					((!object_known_p(o_ptr) && (o_ptr->pval == 2) && (o_ptr->stackc > 1)) ||
					  (!object_known_p(o_ptr) && (rand_int(o_ptr->number) <= o_ptr->stackc) &&
					  (o_ptr->stackc != 1) && (o_ptr->pval > 2)))))
					{
						object_type *i_ptr;
						object_type object_type_body;

						/* Get local object */
						i_ptr = &object_type_body;

						/* Obtain a local object */
						object_copy(i_ptr, o_ptr);

						/* Modify quantity */
						i_ptr->number = 1;

						/* Reset stack counter */
						i_ptr->stackc = 0;
 
				 		/* Unstack the used item */
				 		o_ptr->number--;

						/* Reduce the charges on the new item */
						if (o_ptr->stackc > 1)
						{
							i_ptr->pval-=2;
							o_ptr->stackc--;
						}
						else if (!o_ptr->stackc)
						{
							i_ptr->pval--;
							o_ptr->pval++;
							o_ptr->stackc = o_ptr->number-1;
						}

						(void)floor_carry(y,x,i_ptr);
					}
				}
				else
				{
					/* Disarm if runs out */
					cave_alter_feat(y,x,FS_DISARM);
				}

				break;
			}

			case TV_ROD:
			case TV_DRAG_ARMOR:
			{
				if (!((o_ptr->timeout) && ((!o_ptr->stackc) || (o_ptr->stackc >= o_ptr->number))))
				{
					int tmpval;

					/* Store pval */
					tmpval = o_ptr->timeout;

					/* Time rod out */
					o_ptr->timeout = o_ptr->pval;

					/* Get item effect */
					get_spell(&power, "use", o_ptr, FALSE);

					/* Has a power */
					/* Hack -- check if we are stacking rods */
					if ((o_ptr->timeout > 0) && (!(tmpval) || stack_force_times))
					{
						/* Hack -- one more rod charging */
						if (o_ptr->timeout) o_ptr->stackc++;

						/* Reset stack count */
						if (o_ptr->stackc == o_ptr->number) o_ptr->stackc = 0;

						/* Hack -- always use maximum timeout */
						if (tmpval > o_ptr->timeout) o_ptr->timeout = tmpval;
					}

					/* XXX Hack -- unstack if necessary */
					if ((o_ptr->number > 1) && (o_ptr->timeout > 0))
					{
						object_type *i_ptr;
						object_type object_type_body;

						/* Get local object */
						i_ptr = &object_type_body;

						/* Obtain a local object */
						object_copy(i_ptr, o_ptr);

						/* Modify quantity */
						i_ptr->number = 1;

						/* Clear stack counter */
						i_ptr->stackc = 0;

						/* Restore "charge" */
						o_ptr->timeout = tmpval;

						/* Unstack the used item */
						o_ptr->number--;

						/* Reset the stack if required */
						if (o_ptr->stackc == o_ptr->number) o_ptr->stackc = 0;

						(void)floor_carry(y,x,i_ptr);
					}
				}
				break;
			}

			case TV_POTION:
			case TV_SCROLL:
			case TV_FLASK:
			case TV_FOOD:
			{
				/* Hack -- boring food */
				if ((o_ptr->tval == TV_FOOD) && (o_ptr->sval >= SV_FOOD_MIN_FOOD))
				{
					/* Disarm */
					cave_alter_feat(y,x,FS_DISARM);
				}
				else
				{
					/* Get item effect */
					get_spell(&power, "use", o_ptr, FALSE);

					/* Decrease the item */
					floor_item_increase(cave_o_idx[y][x], -1);
					floor_item_optimize(cave_o_idx[y][x]);

					/* Disarm if runs out */
					if (!cave_o_idx[y][x]) cave_alter_feat(y,x,FS_DISARM);
				}

				break;
			}

			case TV_RUNESTONE:
			{
				u32b runes = p_ptr->cur_runes;

				int num = 0;
				s16b book[26];

				/* Hack -- use current rune */
				p_ptr->cur_runes = (2 << (o_ptr->sval-1));

				/* Fill the book with spells */
				fill_book(o_ptr,book,&num);

				/* Unhack */
				p_ptr->cur_runes = runes;

				/* Get a power */
				power = book[rand_int(num)];

				/* Decrease the item */
				floor_item_increase(cave_o_idx[y][x], -1);
				floor_item_optimize(cave_o_idx[y][x]);

				/* Disarm if runs out */
				if (!cave_o_idx[y][x]) cave_alter_feat(y,x,FS_DISARM);

				break;
			}

			default:
			{
				/* Disarm */
				cave_alter_feat(y,x,FS_DISARM);

				break;
			}
		}

		/* Has a power */
		if (power > 0)
		{
			spell_type *s_ptr = &s_info[power];

			int ap_cnt;

			/* Object is used */
			if (k_info[o_ptr->k_idx].used < MAX_SHORT) k_info[o_ptr->k_idx].used++;

			/* Scan through all four blows */
			for (ap_cnt = 0; ap_cnt < 4; ap_cnt++)
			{
				int damage = 0;

				/* Extract the attack infomation */
				int effect = s_ptr->blow[ap_cnt].effect;
				int method = s_ptr->blow[ap_cnt].method;
				int d_dice = s_ptr->blow[ap_cnt].d_dice;
				int d_side = s_ptr->blow[ap_cnt].d_side;
				int d_plus = s_ptr->blow[ap_cnt].d_plus;

				/* Hack -- no more attacks */
				if (!method) break;

				/* Mega hack -- dispel evil/undead objects */
				if (!d_side)
				{
					d_plus += 25 * d_dice;
				}

				/* Roll out the damage */
				if ((d_dice) && (d_side))
				{
					damage = damroll(d_dice, d_side) + d_plus;
				}
				else
				{
					damage = d_plus;
				}

				(void)project_m(0,0,y,x,damage, effect);
				(void)project_f(0,0,y,x,damage, effect);
			}
		}
	}

	/* Regular traps */
	else
	{
		if (f_ptr->spell)
		{
	      		make_attack_spell_aux(0,y,x,f_ptr->spell);
		}
		else if (f_ptr->blow.method)
		{
			int damage = damroll(f_ptr->blow.d_side,f_ptr->blow.d_dice);
   
			/* Apply the blow */
			project_m(0, 0, y, x, damage, f_ptr->blow.effect);
		}

		/* Get feature */
		f_ptr = &f_info[cave_feat[p_ptr->py][p_ptr->px]];

		if (f_ptr->flags1 & (FF1_HIT_TRAP))
		{
			/* Modify the location hit by the trap */
			cave_alter_feat(y,x,FS_HIT_TRAP);
		}
		else if (f_ptr->flags1 & (FF1_SECRET))
		{
			/* Discover */
			cave_alter_feat(y,x,FS_SECRET);
		}
	}
}




#if 0

/*
 * The running algorithm  -CJS-
 *
 * Modified for monsters. This lets them navigate corridors 'quickly'
 * and correctly turn corners. We use the monster run algorithm to
 * move now.
 *
 * We should be careful not to to let a monster run past the player
 * unless they are fleeing.
 *
 * We should be careful to 'disturb' the monster if they are in a room
 * and the player becomes visible or moves while visible.
 *
 * Because monsters are effectively running, we should be careful to
 * update the run parameters if they are 'staggering', and not let
 * them run while confused.
 *
 */




/*
 * Hack -- allow quick "cycling" through the legal directions
 */
static byte cycle[] =
{ 1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Hack -- map each direction into the "middle" of the "cycle[]" array
 */
static byte chome[] =
{ 0, 8, 9, 10, 7, 0, 11, 6, 5, 4 };



/*
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diaginal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. ???
 *
 * Diagonal Corridor    Blunt Corridor (?)
 *       # #		  #
 *       #x#		 @x#
 *       @p.		  p
 */
static void mon_run_init(int dir)
{
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	int i, row, col;

	bool deepleft, deepright;
	bool shortleft, shortright;

	/* Save the direction */
	m_ptr->run_cur_dir = dir;

	/* Assume running straight */
	m_ptr->run_old_dir = dir;

	/* Assume looking for open area */
	m_ptr->mflag |= MFLAG_RUN_OPEN_AREA;

	/* Assume not looking for breaks */
	m_ptr->mflag &= ~(MFLAG_RUN_BREAK_RIGHT);
	m_ptr->mflag &= ~(MFLAG_RUN_BREAK_LEFT);

	/* Assume no nearby walls */
	deepleft = deepright = FALSE;
	shortright = shortleft = FALSE;

	/* Find the destination grid */
	row = fy + ddy[dir];
	col = fx + ddx[dir];

	/* Extract cycle index */
	i = chome[dir];

	/* Check for nearby wall */
	if (see_wall(cycle[i+1], fy, fx))
	{
		m_ptr->mflag |= MFLAG_RUN_BREAK_LEFT;
		shortleft = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i+1], row, col))
	{
		m_ptr->mflag |= MFLAG_RUN_BREAK_LEFT;
		deepleft = TRUE;
	}

	/* Check for nearby wall */
	if (see_wall(cycle[i-1], fy, fx))
	{
		m_ptr->mflag |= MFLAG_RUN_BREAK_RIGHT;
		shortright = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i-1], row, col))
	{
		m_ptr->mflag |= MFLAG_RUN_BREAK_RIGHT;
		deepright = TRUE;
	}

	/* Looking for a break */
	if ((m_ptr->mflag & (MFLAG_RUN_BREAK_LEFT)) && (m_ptr->mflag & (MFLAG_RUN_BREAK_RIGHT)))
	{
		/* Not looking for open area */
		m_ptr->mflag &= ~(MFLAG_RUN_OPEN_AREA);

		/* Hack -- allow angled corridor entry */
		if (dir & 0x01)
		{
			if (deepleft && !deepright)
			{
				m_ptr->run_old_dir = cycle[i - 1];
			}
			else if (deepright && !deepleft)
			{
				m_ptr->run_old_dir = cycle[i + 1];
			}
		}

		/* Hack -- allow blunt corridor entry */
		else if (see_wall(cycle[i], row, col))
		{
			if (shortleft && !shortright)
			{
				m_ptr->run_old_dir = cycle[i - 2];
			}
			else if (shortright && !shortleft)
			{
				m_ptr->run_old_dir = cycle[i + 2];
			}
		}
	}
}


/*
 * Update the current "run" path
 *
 * Return TRUE if the running should be stopped
 */
static bool run_test(void)
{
	int fy = m_ptr->fy;
	int fx = m_ptr->fx;

	int prev_dir;
	int new_dir;
	int check_dir = 0;

	int row, col;
	int i, max;
	int option, option2;

	int feat;

	bool notice;

	/* No options yet */
	option = 0;
	option2 = 0;

	/* Where we came from */
	prev_dir = m_ptr->run_old_dir;


	/* Range of newly adjacent grids */
	max = (prev_dir & 0x01) + 1;


	/* Look at every newly adjacent square. */
	for (i = -max; i <= max; i++)
	{
		s16b this_o_idx, next_o_idx = 0;


		/* New direction */
		new_dir = cycle[chome[prev_dir] + i];

		/* New location */
		row = fy + ddy[new_dir];
		col = fx + ddx[new_dir];


		/* Visible monsters abort running */
		if (cave_m_idx[row][col] > 0)
		{
			return (TRUE);
		}

		/* Analyze unknown grids and floors */
		if (cave_floor_bold(row, col))
		{
			/* Looking for open area */
			if (m_ptr->mflag & (MFLAG_RUN_OPEN_AREA))
			{
				/* Nothing */
			}

			/* The first new direction. */
			else if (!option)
			{
				option = new_dir;
			}

			/* Three new directions. Stop running. */
			else if (option2)
			{
				return (TRUE);
			}

			/* Two non-adjacent new directions.  Stop running. */
			else if (option != cycle[chome[prev_dir] + i - 1])
			{
				return (TRUE);
			}

			/* Two new (adjacent) directions (case 1) */
			else if (new_dir & 0x01)
			{
				check_dir = cycle[chome[prev_dir] + i - 2];
				option2 = new_dir;
			}

			/* Two new (adjacent) directions (case 2) */
			else
			{
				check_dir = cycle[chome[prev_dir] + i + 1];
				option2 = option;
				option = new_dir;
			}
		}

		/* Obstacle, while looking for open area */
		else
		{
			if (m_ptr->mflag & (MFLAG_RUN_OPEN_AREA))
			{
				if (i < 0)
				{
					/* Break to the right */
					m_ptr->mflag |= MFLAG_RUN_BREAK_RIGHT;
				}

				else if (i > 0)
				{
					/* Break to the left */
					m_ptr->mflag |= MFLAG_RUN_BREAK_LEFT;
				}
			}
		}
	}


	/* Looking for open area */
	if (m_ptr->mflag & (MFLAG_RUN_OPEN_AREA)run_open_area)
	{
		/* Hack -- look again */
		for (i = -max; i < 0; i++)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = fy + ddy[new_dir];
			col = fx + ddx[new_dir];

			/* Get feature */
			feat = cave_feat[row][col];

			/* Get mimiced feature */
			feat = f_info[feat].mimic;

			/* Unknown grid or non-wall */
			/* Was: cave_floor_bold(row, col) */
			if (!(cave_info[row][col] & (CAVE_MARK)) ||
			    (!(f_info[feat].flags1 & (FF1_WALL))) )
			{
				/* Looking to break right */
				if ((m_ptr->mflag & (MFLAG_RUN_BREAK_RIGHT)))
				{
					return (TRUE);
				}
			}


			/* Obstacle */
			else
			{
				/* Looking to break left */
				if ((m_ptr->mflag & (MFLAG_RUN_BREAK_LEFT)))
				{
					return (TRUE);
				}
			}
		}

		/* Hack -- look again */
		for (i = max; i > 0; i--)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = fy + ddy[new_dir];
			col = fx + ddx[new_dir];

			/* Get feature */
			feat = cave_feat[row][col];

			/* Get mimiced feature */
			feat = f_info[feat].mimic;

			/* Unknown grid or non-wall */
			/* Was: cave_floor_bold(row, col) */
			if (!(cave_info[row][col] & (CAVE_MARK)) ||
			    (!(f_info[feat].flags1 & (FF1_WALL))))
			{
				/* Looking to break left */
				if ((m_ptr->mflag & (MFLAG_RUN_BREAK_LEFT)))
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break right */
				if ((m_ptr->mflag & (MFLAG_RUN_BREAK_RIGHT)))
				{
					return (TRUE);
				}
			}
		}
	}


	/* Not looking for open area */
	else
	{
		/* No options */
		if (!option)
		{
			return (TRUE);
		}

		/* One option */
		else if (!option2)
		{
			/* Primary option */
			m_ptr->run_cur_dir = option;

			/* No other options */
			m_ptr->run_old_dir = option;
		}

		/* Two options, examining corners */
		else if (run_use_corners && !run_cut_corners)
		{
			/* Primary option */
			m_ptr->run_cur_dir = option;

			/* Hack -- allow curving */
			m_ptr->run_old_dir = option2;
		}

		/* Two options, pick one */
		else
		{
			/* Get next location */
			row = fy + ddy[option];
			col = fx + ddx[option];

			/* Don't see that it is closed off. */
			/* This could be a potential corner or an intersection. */
			if (!see_wall(option, row, col) ||
			    !see_wall(check_dir, row, col))
			{
				/* Can not see anything ahead and in the direction we */
				/* are turning, assume that it is a potential corner. */
				if (run_use_corners &&
				    see_nothing(option, row, col) &&
				    see_nothing(option2, row, col))
				{
					m_ptr->run_cur_dir = option;
					m_ptr->run_old_dir = option2;
				}

				/* STOP: we are next to an intersection or a room */
				else
				{
					return (TRUE);
				}
			}

			/* This corner is seen to be enclosed; we cut the corner. */
			else if (run_cut_corners)
			{
				m_ptr->run_cur_dir = option2;
				m_ptr->run_old_dir = option2;
			}

			/* This corner is seen to be enclosed, and we */
			/* deliberately go the long way. */
			else
			{
				m_ptr->run_cur_dir = option;
				m_ptr->run_old_dir = option2;
			}
		}
	}


	/* About to hit a known wall, stop */
	if (see_wall(m_ptr->run_cur_dir, fy, fx))
	{
		return (TRUE);
	}


	/* Failure */
	return (FALSE);
}



/*
 * Take one step along the current "run" path
 *
 * Called with a real direction to begin a new run, and with zero
 * to continue a run in progress.
 */
void mon_run_step(int dir)
{
	/* Start run */
	if (dir)
	{
		/* Initialize */
		run_init(dir);
	}

	/* Continue run */
	else
	{
		/* Update run */
		if (run_test())
		{
			/* Done */
			return;
		}
	}

}


#endif

