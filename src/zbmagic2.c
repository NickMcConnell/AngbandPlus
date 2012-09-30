/* File: zbmagic2.c */
/* Purpose: Medium level stuff for the Borg -BEN- */

#include "angband.h"

#ifdef ALLOW_BORG

#include "zborg1.h"
#include "zborg2.h"
#include "zborg3.h"
#include "zborg4.h"
#include "zborg5.h"
#include "zborg6.h"
#include "zbmagic.h"


bool borg_simulate; /* Simulation flag */


/*
 * New method for handling attacks, missiles, and spells
 *
 * Every turn, we evaluate every known method of causing damage
 * to monsters, and evaluate the "reward" inherent in each of
 * the known methods which is usable at that time, and then
 * we actually use whichever method, if any, scores highest.
 *
 * For each attack, we need a function which will determine the best
 * possible result of using that attack, and return its value.  Also,
 * if requested, the function should actually perform the action.
 *
 * Note that the functions should return zero if the action is not
 * usable, or if the action is not useful.
 *
 * These functions need to apply some form of "cost" evaluation, to
 * prevent the use of expensive spells with minimal reward.  Also,
 * we should always prefer attacking by hand to using spells if the
 * damage difference is "small", since there is no "cost" in making
 * a physical attack.
 *
 * We should take account of "spell failure", as well as "missile
 * missing" and "blow missing" probabilities.
 *
 * Note that the functions may store local state information when
 * doing a "simulation" and then they can use this information if
 * they are asked to implement their strategy.
 *
 * There are several types of damage inducers:
 *
 *   Using rods
 *   Activate Dragon Armour
 *   Elemental Rings
 *   Activating Artifacts
 *   Launching missiles
 *   Throwing objects
 *	 Reading scrolls
 *   Attacking physically
 *   Casting spells
 *   Using staffs
 *   Aimng wands
 *   Racial Powers
 *   Mutation Powers
 *   Emergency use of spells
 *
 * The order of these attack types is not random.  If two attacks do the same
 * damage then the first in the list will prevail.  So that is why the rods
 * are first and fainting is last.
 */

#define BF_MIN					0

enum
{
	BF_ROD,					/* Recharging objects */
	BF_DRAGON_ARMOUR,
	BF_RING,
	BF_ARTIFACT,
	BF_LAUNCH,				/* Cheap objects */
	BF_OBJECT,
	BF_SCROLL,
	BF_THRUST,				/* Rest to restore hp/sp */
	BF_SPELLCASTER,
	BF_MINDCRAFTER,
	BF_STAFF,				/* Objects with charges */
	BF_WAND,
	BF_RACIAL,				/* Powers that hurt to execute */
	BF_MUTATE,
	BF_SPELL_RESERVE,		/* Emergency spell uses */
	BF_MIND_RESERVE,
	BF_SPELL_FAINT,			/* Fainting spell uses */
	BF_MIND_FAINT,

	BF_MAX
};


/* Attack styles */
#define BORG_BOLT		1
#define BORG_BEAM		2
#define BORG_BALL		3
#define BORG_DISPEL		4
#define BORG_BLAST		5
#define BORG_TOUCH		6

/* What is the radius of the borg ball attacks? */
#define BORG_BALL_RAD0	0
#define BORG_BALL_RAD1	1
#define BORG_BALL_RAD2	2
#define BORG_BALL_RAD3	3
#define BORG_BALL_RAD4	4
#define BORG_BALL_RAD8	8


/*
 * Guess how much damage a physical attack will do to a monster
 */
static int borg_thrust_damage_one(int i)
{
	int dam;
	int mult;

	borg_kill *kill;

	monster_race *r_ptr;

	list_item *l_ptr;

	int chance;

	/* Examine current weapon */
	l_ptr = &equipment[EQUIP_WIELD];

	/* Monster record */
	kill = &borg_kills[i];

	/* Monster race */
	r_ptr = &r_info[kill->r_idx];

	/* Damage */
	dam = (l_ptr->dd * (l_ptr->ds + 1) / 2);

	/* here is the place for slays and such */
	mult = 1;

	if (((FLAG(bp_ptr, TR_SLAY_ANIMAL)) && (FLAG(r_ptr, RF_ANIMAL))) ||
		((FLAG(bp_ptr, TR_SLAY_EVIL)) && (FLAG(r_ptr, RF_EVIL))))
		mult = 2;

	if (((FLAG(bp_ptr, TR_SLAY_UNDEAD)) && (FLAG(r_ptr, RF_ANIMAL))) ||
		((FLAG(bp_ptr, TR_SLAY_DEMON)) && (FLAG(r_ptr, RF_DEMON))) ||
		((FLAG(bp_ptr, TR_SLAY_ORC)) && (FLAG(r_ptr, RF_ORC))) ||
		((FLAG(bp_ptr, TR_SLAY_TROLL)) && (FLAG(r_ptr, RF_TROLL))) ||
		((FLAG(bp_ptr, TR_SLAY_GIANT)) && (FLAG(r_ptr, RF_GIANT))) ||
		((FLAG(bp_ptr, TR_SLAY_DRAGON)) && (FLAG(r_ptr, RF_DRAGON))) ||
		((FLAG(bp_ptr, TR_BRAND_ACID)) && !(FLAG(r_ptr, RF_IM_ACID))) ||
		((FLAG(bp_ptr, TR_BRAND_FIRE)) && !(FLAG(r_ptr, RF_IM_FIRE))) ||
		((FLAG(bp_ptr, TR_BRAND_COLD)) && !(FLAG(r_ptr, RF_IM_COLD))) ||
		((FLAG(bp_ptr, TR_BRAND_ELEC)) && !(FLAG(r_ptr, RF_IM_ELEC))))
		mult = 3;

	if ((FLAG(bp_ptr, TR_KILL_DRAGON)) && (FLAG(r_ptr, RF_DRAGON)))
		mult = 5;

	/* add the multiplier */
	dam *= mult;

	/* add weapon bonuses */
	dam += l_ptr->to_d;

	/* add player bonuses */
	dam += bp_ptr->to_h;

	/* multiply the damage for the whole round of attacks */
	dam *= bp_ptr->blows;

	/* reduce for % chance to hit (AC) */
	chance = bp_ptr->skill_thn + (bp_ptr->to_h + l_ptr->to_h) * 3;
	if ((r_ptr->ac * 3 / 4) > 0)
		chance = (chance * 100) / (r_ptr->ac * 3 / 4);

	/* 5% automatic success/fail */
	if (chance > 95) chance = 95;
	if (chance < 5) chance = 5;

	/* add 20% to chance to give a bit more weight to weapons */
	if (bp_ptr->lev > 15 &&
		borg_class != CLASS_MAGE &&
		borg_class != CLASS_HIGH_MAGE &&
		borg_class != CLASS_MINDCRAFTER) chance += 20;

	dam = (dam * chance) / 100;

	/* Limit damage to twice maximal hitpoints */
	if (dam > kill->power * 2) dam = kill->power * 2;

	/* Reduce the damage if a mage, they should not melee if they can avoid it */
	if ((borg_class == CLASS_MAGE ||
		borg_class != CLASS_HIGH_MAGE ||
		borg_class != CLASS_MINDCRAFTER) &&
		bp_ptr->max_lev < 40) dam = dam * 6 / 10;

	/*
	 * Enhance the preceived damage on Uniques.  This way we target them
	 * Keep in mind that he should hit the uniques but if he has a
	 * x5 great bane of dragons, he will tend attack the dragon since the
	 * precieved (and actual) damage is higher.  But don't select
	 * the town uniques (maggot does no damage)
	 *
	 */
	if (FLAG(r_ptr, RF_UNIQUE) && bp_ptr->depth >= 1) dam += (dam * 5);

	/* Hack -- ignore Maggot until later.  Player will chase Maggot
	 * down all accross the screen waking up all the monsters.  Then
	 * he is stuck in a comprimised situation.
	 */
	if (FLAG(r_ptr, RF_UNIQUE) && bp_ptr->depth == 0)
	{
		dam = dam * 2 / 3;

		/* Dont hunt maggot until later */
		if (bp_ptr->lev < 5) dam = 0;
	}

	/* give a small bonus for whacking a breeder */
	if (FLAG(r_ptr, RF_MULTIPLY))
		dam = (dam * 3 / 2);

	/* Enhance the preceived damgage to summoner in order to influence the
	 * choice of targets.
	 */
	if (FLAG(r_ptr, RF_S_KIN) ||
		FLAG(r_ptr, RF_S_CYBER) ||
		FLAG(r_ptr, RF_S_MONSTER) ||
		FLAG(r_ptr, RF_S_MONSTERS) ||
		FLAG(r_ptr, RF_S_ANT) ||
		FLAG(r_ptr, RF_S_SPIDER) ||
		FLAG(r_ptr, RF_S_HOUND) ||
		FLAG(r_ptr, RF_S_HYDRA) ||
		FLAG(r_ptr, RF_S_ANGEL) ||
		FLAG(r_ptr, RF_S_DEMON) ||
		FLAG(r_ptr, RF_S_UNDEAD) ||
		FLAG(r_ptr, RF_S_DRAGON) ||
		FLAG(r_ptr, RF_S_HI_UNDEAD) ||
		FLAG(r_ptr, RF_S_HI_DRAGON) ||
		FLAG(r_ptr, RF_S_AMBERITES) ||
		FLAG(r_ptr, RF_S_UNIQUE) ||
		FLAG(r_ptr, RF_QUESTOR))
		dam += ((dam * 3) / 2);

	/* To conserve mana, for keeping GOI up, increase the value of melee */
	if (borg_goi)
	{
		dam += (dam * 15 / 10);
	}

	/* dont hurt friends or pets */
	if (kill->m_flags & (MONST_FRIEND | MONST_PET)) dam = -10;

	/* Invuln monsters take no dam */
	if (kill->m_flags & MONST_INVULN) dam = 0;

	/* Damage */
	return (dam);
}



/*
 * Simulate/Apply the optimal result of making a physical attack
 */
static int borg_attack_thrust(void)
{
	int p, dir;

	int i, b_i = 0;
	int d, b_d = 0;

	map_block *mb_ptr;
	borg_kill *kill;

	if (borg_simulate)
	{
		/* Too afraid to attack */
		if (bp_ptr->status.afraid) return (0);

		/* Examine possible destinations */
		for (i = 0; i < borg_next_n; i++)
		{
			int x = borg_next_x[i];
			int y = borg_next_y[i];

			/* Acquire grid */
			mb_ptr = map_loc(x, y);

			/* Calculate "average" damage */
			d = borg_thrust_damage_one(mb_ptr->kill);

			/* No damage */
			if (d <= 0) continue;

			/* Obtain the monster */
			kill = &borg_kills[mb_ptr->kill];

			/* Hack -- avoid waking most "hard" sleeping monsters */
			if ((kill->m_flags & MONST_ASLEEP) && (d <= kill->power))
			{
				/* Calculate danger */
				borg_full_damage = TRUE;
				p = borg_danger_aux(x, y, 1, mb_ptr->kill, TRUE);
				borg_full_damage = FALSE;

				if (p > avoidance / 2)
					continue;
			}

			/* Hack -- ignore sleeping town monsters */
			if (!bp_ptr->depth && (kill->m_flags & MONST_ASLEEP)) continue;


			/* Calculate "danger" to player */
			borg_full_damage = TRUE;
			p = borg_danger_aux(c_x, c_y, 2, mb_ptr->kill, TRUE);
			borg_full_damage = FALSE;

			/* Reduce "bonus" of partial kills */
			if (d <= kill->power) p = p / 10;

			/* Add the danger to the damage */
			d += p;

			/* Ignore lower damage */
			if (d < b_d) continue;

			/* Save the info */
			b_i = i;
			b_d = d;
		}

		/* If damage was found */
		if (b_d)
		{
			/* Save the location */
			g_x = borg_next_x[b_i];
			g_y = borg_next_y[b_i];
		}
		/* Better safe than sorry */
		else
		{
			g_x = c_x;
			g_y = c_y;
		}

		/* End of simulation */
		return (b_d);
	}

	/* Get the spot on the map */
	mb_ptr = map_loc(g_x, g_y);

	/* Note */
	borg_note
		("# Facing %s at (%d,%d).",
		 mon_race_name(&r_info[mb_ptr->monster]), g_x, g_y);
	borg_note
		("# Attacking with weapon '%s'", equipment[EQUIP_WIELD].o_name);

	/* Get a direction for attacking */
	dir = borg_extract_dir(c_x, c_y, g_x, g_y);

	/* Attack the grid */
	borg_keypress('+');
	borg_keypress(I2D(dir));

	/* Success */
	return (b_d);
}


/*
 * Guess how much damage a spell attack will do to a monster
 *
 * We only handle the "standard" damage types.
 *
 * We are paranoid about monster resistances
 *
 * He tends to waste all of his arrows on a monsters immediately adjacent
 * to him.  Then he has no arrows for the rest of the level.  We will
 * decrease the damage if the monster is adjacent and we are getting low
 * on missiles.
 *
 * We will also decrease the value of the missile attack on breeders or
 * high clevel borgs town scumming.
 */
static int borg_launch_damage_one(int i, int dam, int typ)
{
	int p1, p2 = 0;
	bool borg_use_missile = FALSE;

	/* Monster record */
	borg_kill *kill = &borg_kills[i];

	/* Monster race */
	monster_race *r_ptr = &r_info[kill->r_idx];

	/* all danger checks are with maximal damage */
	borg_full_damage = TRUE;

	/* Analyze the damage type */
	switch (typ)
	{
		case GF_MISSILE:
		{
			/* Magic Missile */
			break;
		}

		case GF_ARROW:
		{
			/* Standard Arrow */
			if (distance(c_y, c_x, kill->y, kill->x) == 1 &&
				!(FLAG(r_ptr, RF_UNIQUE))) dam /= 5;
			break;
		}

		case GF_ARROW_EXPLOSION:
		{
			/* Explosion arrows are really just flaming arrows with a kick */
			dam += 100;

			/* Fall through */
		}
		case GF_ARROW_FLAME:
		{
			/* Arrow of Flame */
			if (!(FLAG(r_ptr, RF_IM_FIRE))) dam *= 3;
			if (distance(c_y, c_x, kill->y, kill->x) == 1 &&
				!(FLAG(r_ptr, RF_UNIQUE))) dam /= 5;
			break;
		}

		case GF_ARROW_FROST:
		{
			/* Arrow of Frost */
			if (!(FLAG(r_ptr, RF_IM_COLD))) dam *= 3;
			if (distance(c_y, c_x, kill->y, kill->x) == 1 &&
				!(FLAG(r_ptr, RF_UNIQUE))) dam /= 5;
			break;
		}

		case GF_ARROW_SHOCKING:
		{
			/* Arrow of Shocking */
			if (!(FLAG(r_ptr, RF_IM_ELEC))) dam *= 3;
			if (distance(c_y, c_x, kill->y, kill->x) == 1 &&
				!(FLAG(r_ptr, RF_UNIQUE))) dam /= 5;
			break;
		}

		case GF_ARROW_ANIMAL:
		{
			/* Arrow of Hurt Animal */
			if (FLAG(r_ptr, RF_ANIMAL)) dam *= 2;
			if (distance(c_y, c_x, kill->y, kill->x) == 1 &&
				!(FLAG(r_ptr, RF_UNIQUE))) dam /= 5;
			break;
		}

		case GF_ARROW_EVIL:
		{
			/* Arrow of hurt evil */
			if (FLAG(r_ptr, RF_EVIL)) dam *= 2;
			if (distance(c_y, c_x, kill->y, kill->x) == 1 &&
				!(FLAG(r_ptr, RF_UNIQUE))) dam /= 5;
			break;
		}

		case GF_ARROW_DRAGON:
		{
			/* Arrow of slay dragon */
			if (FLAG(r_ptr, RF_DRAGON)) dam *= 3;
			if (distance(c_y, c_x, kill->y, kill->x) == 1 &&
				!(FLAG(r_ptr, RF_UNIQUE))) dam /= 5;
			break;
		}

		case GF_MANA:
		{
			/* Pure damage */

			/* only use mana storm against uniques... this */
			/* should cut down on some mana use. */
			if (!borg_fighting_unique || bp_ptr->able.mana < 3)
				dam /= 2;
			if (borg_fighting_unique && bp_ptr->able.mana > 7)
				dam *= 2;
			break;
		}


		case GF_ACID:
		{
			/* Acid */
			if (FLAG(r_ptr, RF_IM_ACID)) dam /= 9;
			break;
		}

		case GF_ELEC:
		{
			/* Electricity */
			if (FLAG(r_ptr, RF_IM_ELEC)) dam /= 9;
			break;
		}

		case GF_FIRE:
		{
			/* Fire damage */
			if (FLAG(r_ptr, RF_IM_FIRE)) dam /= 9;
			break;
		}

		case GF_COLD:
		{
			/* Cold */
			if (FLAG(r_ptr, RF_IM_COLD)) dam /= 9;
			break;
		}

		case GF_ELEMENTS:
		{
			/* Hack -- Equal chance of all elements to be cast */
			if (FLAG(r_ptr, RF_IM_COLD)) dam /= 4;
			if (FLAG(r_ptr, RF_IM_ELEC)) dam /= 4;
			if (FLAG(r_ptr, RF_IM_FIRE)) dam /= 4;
			if (FLAG(r_ptr, RF_IM_ACID)) dam /= 4;
			break;
		}

		case GF_POIS:
		{
			/* Poison */
			if (FLAG(r_ptr, RF_IM_POIS)) dam /= 9;
			break;
		}

		case GF_NUKE:
		{
			/* Nuke */
			if (FLAG(r_ptr, RF_IM_POIS)) dam = (dam * 3) / 9;
			break;
		}

		case GF_ICE:
		{
			/* Ice */
			if (FLAG(r_ptr, RF_IM_COLD)) dam /= 9;
			break;
		}

		case GF_HELL_FIRE:
		{
			/* Holy Orb */
			if (FLAG(r_ptr, RF_EVIL)) dam *= 2;
			break;
		}

		case GF_HOLY_FIRE:
		{
			/* Holy Orb */
			if (FLAG(r_ptr, RF_GOOD)) dam = 0;
			else if (FLAG(r_ptr, RF_EVIL)) dam *= 2;
			else
				dam = (dam * 3) / 9;
			break;
		}

		case GF_DISP_UNDEAD:
		{
			/* dispel undead */
			if (!(FLAG(r_ptr, RF_UNDEAD))) dam = 0;
			break;
		}

		case GF_DISP_DEMON:
		{
			/* Dispel Demon */
			if (!(FLAG(r_ptr, RF_DEMON))) dam = 0;
			break;
		}

		case GF_DISP_UNDEAD_DEMON:
		{
			/* Dispel Demons and Undead (Exorcism Spell) */
			if (!(FLAG(r_ptr, RF_UNDEAD))) dam = 0;
			if (!(FLAG(r_ptr, RF_DEMON))) dam = 0;
			break;
		}

		case GF_DISP_EVIL:
		{
			/*  Dispel Evil */
			if (!(FLAG(r_ptr, RF_EVIL))) dam = 0;
			break;
		}

		case GF_HOLY_WORD:
		{
			/*  Holy Word */
			if (!(FLAG(r_ptr, RF_EVIL))) dam = 0;
			break;
		}

		case GF_LITE_WEAK:
		{
			/* Weak Lite */
			if (!(FLAG(r_ptr, RF_HURT_LITE))) dam = 0;
			break;
		}

		case GF_LITE:
		{
			/* Regular Lite */
			break;
		}

		case GF_OLD_DRAIN:
		case GF_DEATH_RAY:
		{
			/* Drain Life / Psi / Vamp. */
			if (!monster_living(r_ptr))
			{
				dam = 0;
			}
			break;
		}

		case GF_PSI:
		case GF_PSI_DRAIN:
		{
			if (FLAG(r_ptr, RF_EMPTY_MIND))
			{
				dam = 0;
			}
			else if ((FLAG(r_ptr, RF_STUPID)) ||
					 (FLAG(r_ptr, RF_WEIRD_MIND)) ||
					 (FLAG(r_ptr, RF_ANIMAL)) ||
					 (r_ptr->hdice * 2 > (3 * dam / 2)))
			{
				dam /= 3;
			}
			else if (((FLAG(r_ptr, RF_UNDEAD)) ||
					  (FLAG(r_ptr, RF_DEMON))) &&
					 (r_ptr->hdice * 2 > bp_ptr->lev / 2))
			{
				dam = 0;
			}
			break;
		}

		case GF_KILL_WALL:
		{
			/* Stone to Mud */
			if (!(FLAG(r_ptr, RF_HURT_ROCK))) dam = 0;
			break;
		}

		case GF_NETHER:
		{
			/* Nether */

			if (FLAG(r_ptr, RF_UNDEAD))
			{
				dam = 0;
			}
			else if (FLAG(r_ptr, RF_BR_NETH))
			{
				dam *= 3;
				dam /= 9;
			}
			else if (FLAG(r_ptr, RF_EVIL))
			{
				dam /= 2;
			}
			break;
		}

		case GF_CHAOS:
		{
			/* Chaos */

			if ((FLAG(r_ptr, RF_BR_CHAO)) || (FLAG(r_ptr, RF_DEMON)))
			{
				dam *= 3;
				dam /= 9;
			}
			break;
		}

		case GF_GRAVITY:
		{
			/* Gravity */

			if (FLAG(r_ptr, RF_BR_GRAV))
			{
				dam *= 2;
				dam /= 9;
			}
			break;
		}

		case GF_SHARDS:
		{
			/* Shards */
			if (FLAG(r_ptr, RF_BR_SHAR))
			{
				dam *= 3;
				dam /= 9;
			}
			break;
		}

		case GF_ROCKET:
		{
			/* Rockets */
			if (FLAG(r_ptr, RF_BR_SHAR))
			{
				dam /= 2;
			}
			break;
		}

		case GF_SOUND:
		{
			/* Sound */
			if (FLAG(r_ptr, RF_BR_SOUN))
			{
				dam *= 2;
				dam /= 9;
			}
			break;
		}

		case GF_PLASMA:
		{
			/* Plasma */
			if ((FLAG(r_ptr, RF_BR_PLAS)) || (FLAG(r_ptr, RF_RES_PLAS)))
			{
				dam *= 2;
				dam /= 9;
			}
			break;
		}

		case GF_FORCE:
		{
			/* Force */
			if (FLAG(r_ptr, RF_BR_WALL))
			{
				dam *= 2;
				dam /= 9;
			}
			break;
		}

		case GF_DARK:
		{
			/* Dark */
			if (FLAG(r_ptr, RF_BR_DARK))
			{
				dam *= 2;
				dam /= 9;
			}
			break;
		}

		case GF_WATER:
		{
			/* Water */
			if (FLAG(r_ptr, RF_RES_WATE))
			{
				dam *= 2;
				dam /= 9;
			}
			break;
		}

		case GF_DISINTEGRATE:
		{
			/* Disintegrate */
			if (FLAG(r_ptr, RF_RES_DISE))
			{
				dam *= 2;
				dam /= 9;
			}
			break;
		}
		case GF_TELEKINESIS:
		{
			if (FLAG(r_ptr, RF_UNIQUE)) dam /= 3;
			break;
		}

		case GF_METEOR:
		{
			/* Meteor */
			break;
		}

		case GF_DISP_GOOD:
		{
			/* Dispel Good */
			if (!(FLAG(r_ptr, RF_GOOD))) dam = 0;
			break;
		}

		case GF_DISP_LIVING:
		{
			/* Dispel Living */
			if (!monster_living(r_ptr)) dam = 0;
			break;
		}

		case GF_CONFUSION:
		case GF_DISENCHANT:
		case GF_NEXUS:
		case GF_INERTIA:
		case GF_TIME:
		{
			/* Weird attacks */
			dam /= 2;
			break;
		}

		case GF_DOMINATION:
		case GF_CHARM:
		case GF_CONTROL_UNDEAD:
		case GF_CONTROL_ANIMAL:
		{
			/* Really weird attacks */
			dam = 0;
			break;
		}

		case GF_OLD_HEAL:
		case GF_OLD_CLONE:
		case GF_OLD_SPEED:
		case GF_DARK_WEAK:
		case GF_KILL_DOOR:
		case GF_KILL_TRAP:
		case GF_MAKE_WALL:
		case GF_MAKE_DOOR:
		case GF_MAKE_TRAP:
		case GF_AWAY_UNDEAD:
		case GF_TURN_EVIL:
		{
			/* Various */
			dam = 0;
			break;
		}

		case GF_AWAY_ALL:
		{
			/* These spells which put the monster out of commission, we
			 * look at the danger of the monster prior to and after being
			 * put out of commission.  The difference is the damage.
			 * The following factors are considered when we
			 * consider the spell:
			 *
			 * 1. Is it already comprised by that spell?
			 * 2. Is it comprimised by another spell?
			 * 3. Does it resist the modality?
			 * 4. Will it make it's savings throw better than half the time?
			 * 5. We generally ignore these spells for breeders.
			 *
			 * The spell sleep II and sanctuary have a special consideration
			 * since the monsters must be adjacent to the player.
			 */


			dam = borg_danger_aux(c_x, c_y, 1, i, TRUE);

			/* try not to teleport away uniques.   These are the guys you are trying */
			/* to kill! */
			if (FLAG(r_ptr, RF_UNIQUE))
			{
				/* If this unique is causing the danger, get rid of it */
				if (dam > avoidance * 3 && bp_ptr->depth <= 95)
				{
					/* get rid of this unique */
				}
				else
					dam = -999;
			}
			break;
		}

		case GF_DISP_ALL:
		{
			/* In Z this does hurt Uniques but not in V */
			break;
		}

		case GF_OLD_CONF:
		{
			dam = 0;
			if (FLAG(r_ptr, RF_NO_CONF)) break;
			if (FLAG(r_ptr, RF_MULTIPLY)) break;
			if (kill->
				m_flags & (MONST_ASLEEP | MONST_CONFUSED | MONST_FEAR)) break;
			if ((r_ptr->hdice * 2 >=
				 (bp_ptr->lev <
				  13) ? bp_ptr->lev : (((bp_ptr->lev - 10) /
										4) * 3) + 10)) break;
			dam = -999;
			if (FLAG(r_ptr, RF_UNIQUE)) break;
			borg_confuse_spell = FALSE;
			p1 = borg_danger_aux(c_x, c_y, 1, i, TRUE);
			borg_confuse_spell = TRUE;
			p2 = borg_danger_aux(c_x, c_y, 1, i, TRUE);
			borg_confuse_spell = FALSE;
			dam = (p1 - p2);
			break;
		}

		case GF_TURN_ALL:
		{
			dam = 0;
			if (FLAG(r_ptr, RF_NO_FEAR)) break;
			if (kill->
				m_flags & (MONST_ASLEEP | MONST_CONFUSED | MONST_FEAR)) break;
			if ((r_ptr->hdice * 2 >=
				 (bp_ptr->lev <
				  13) ? bp_ptr->lev : (((bp_ptr->lev - 10) /
										4) * 3) + 10)) break;
			dam = -999;
			if (FLAG(r_ptr, RF_UNIQUE)) break;
			borg_fear_mon_spell = FALSE;
			p1 = borg_danger_aux(c_x, c_y, 1, i, TRUE);
			borg_fear_mon_spell = TRUE;
			p2 = borg_danger_aux(c_x, c_y, 1, i, TRUE);
			borg_fear_mon_spell = FALSE;
			dam = (p1 - p2);
			break;
		}

		case GF_OLD_SLOW:
		{
			dam = 0;
			if (kill->
				m_flags & (MONST_ASLEEP | MONST_CONFUSED | MONST_FEAR)) break;
			if ((r_ptr->hdice * 2 >=
				 (bp_ptr->lev <
				  13) ? bp_ptr->lev : (((bp_ptr->lev - 10) /
										4) * 3) + 10)) break;
			dam = -999;
			if (FLAG(r_ptr, RF_UNIQUE)) break;
			borg_slow_spell = FALSE;
			p1 = borg_danger_aux(c_x, c_y, 1, i, TRUE);
			borg_slow_spell = TRUE;
			p2 = borg_danger_aux(c_x, c_y, 1, i, TRUE);
			borg_slow_spell = FALSE;
			dam = (p1 - p2);
			break;
		}

		case GF_OLD_SLEEP:
		case GF_STASIS:
		{
			dam = 0;
			if (FLAG(r_ptr, RF_NO_SLEEP)) break;
			if (kill->
				m_flags & (MONST_ASLEEP | MONST_CONFUSED | MONST_FEAR)) break;
			if ((r_ptr->hdice * 2 >=
				 (bp_ptr->lev <
				  13) ? bp_ptr->lev : (((bp_ptr->lev - 10) /
										4) * 3) + 10)) break;
			dam = -999;
			if (FLAG(r_ptr, RF_UNIQUE)) break;
			borg_sleep_spell = FALSE;
			p1 = borg_danger_aux(c_x, c_y, 1, i, TRUE);
			borg_sleep_spell = TRUE;
			p2 = borg_danger_aux(c_x, c_y, 1, i, TRUE);
			borg_sleep_spell = FALSE;
			dam = (p1 - p2);
			break;
		}

		case GF_OLD_POLY:
		{
			dam = 0;
			if ((r_ptr->hdice * 2 >=
				 (bp_ptr->lev <
				  13) ? bp_ptr->lev : (((bp_ptr->lev - 10) /
										4) * 3) + 10)) break;
			dam = -999;
			if (FLAG(r_ptr, RF_UNIQUE)) break;
			dam = borg_danger_aux(c_x, c_y, 2, i, TRUE);
			/* dont bother unless he is a scary monster */
			if (dam < avoidance * 2) dam = 0;
			break;
		}

		case GF_TURN_UNDEAD:
		{
			if (FLAG(r_ptr, RF_UNDEAD))
			{
				dam = 0;
				if (kill->
					m_flags & (MONST_ASLEEP | MONST_CONFUSED | MONST_FEAR))
					break;
				if (r_ptr->level > bp_ptr->lev - 5) break;
				borg_fear_mon_spell = FALSE;
				p1 = borg_danger_aux(c_x, c_y, 1, i, TRUE);
				borg_fear_mon_spell = TRUE;
				p2 = borg_danger_aux(c_x, c_y, 1, i, TRUE);
				borg_fear_mon_spell = FALSE;
				dam = (p1 - p2);
			}
			else
			{
				dam = 0;
			}
			break;
		}

		case GF_AWAY_EVIL:
		{
			/* Banishment-- cast when in extreme danger (checked in borg_defense). */
			if (FLAG(r_ptr, RF_EVIL))
			{
				/* try not teleport away uniques. */
				if (FLAG(r_ptr, RF_UNIQUE))
				{
					/* Banish ones with escorts */
					if (FLAG(r_ptr, RF_ESCORT))
					{
						dam = 0;
					}
					else
					{
						/* try not Banish non escorted uniques */
						dam = -500;
					}

				}
				else
				{
					/* damage is the danger of the baddie */
					dam = borg_danger_aux(c_x, c_y, 1, i, TRUE);
				}
			}
			else
			{
				dam = 0;
			}
			break;
		}

	}

	/* use Missiles on certain types of monsters */
	if ((borg_danger_aux(kill->x, kill->y, 1, i, TRUE) >= avoidance * 3 / 10) ||
		(FLAG(r_ptr, RF_FRIENDS) /* monster has friends */  &&
		 r_ptr->level >= bp_ptr->lev - 5 /* close levels */ ) ||
		(kill->ranged_attack /* monster has a ranged attack */ ) ||
		(FLAG(r_ptr, RF_UNIQUE)) ||
		(FLAG(r_ptr, RF_MULTIPLY)) ||
		(bp_ptr->lev <= 5 /* stil very weak */ ))
	{
		borg_use_missile = TRUE;
	}

	/* Restore normal calcs of danger */
	borg_full_damage = FALSE;

	/* dont hurt friends or pets */
	if (kill->m_flags & (MONST_FRIEND | MONST_PET)) dam = -10;

	/* Invuln monsters take no dam */
	if (kill->m_flags & MONST_INVULN) dam = 0;

	/* Return Damage as pure danger of the monster */
	if (typ == GF_AWAY_ALL || typ == GF_AWAY_EVIL) return (dam);

	/* Limit damage to twice maximal hitpoints */
	if (dam > kill->power * 2) dam = kill->power * 2;

	/* give a small bonus for whacking a unique */
	/* this should be just enough to give prefrence to wacking uniques */
	if ((FLAG(r_ptr, RF_UNIQUE)) && bp_ptr->depth >= 1)
		dam = (dam * 5);

	/*
	 * Hack -- ignore Maggot until later.  Player will chase Maggot
	 * down all accross the screen waking up all the monsters.  Then
	 * he is stuck in a comprimised situation.
	 */
	if ((FLAG(r_ptr, RF_UNIQUE)) && bp_ptr->depth == 0)
	{
		dam = dam * 2 / 3;

		/* Dont hunt maggot until later */
		if (bp_ptr->lev < 5) dam = 0;
	}

	/* give a small bonus for whacking a breeder */
	if (FLAG(r_ptr, RF_MULTIPLY))
		dam = (dam * 3 / 2);

	/*
	 * Enhance the preceived damage to summoner in order to influence the
	 * choice of targets.
	 */
	if ((FLAG(r_ptr, RF_S_KIN)) ||
		(FLAG(r_ptr, RF_S_CYBER)) ||
		(FLAG(r_ptr, RF_S_MONSTER)) ||
		(FLAG(r_ptr, RF_S_MONSTERS)) ||
		(FLAG(r_ptr, RF_S_ANT)) ||
		(FLAG(r_ptr, RF_S_SPIDER)) ||
		(FLAG(r_ptr, RF_S_HOUND)) ||
		(FLAG(r_ptr, RF_S_HYDRA)) ||
		(FLAG(r_ptr, RF_S_ANGEL)) ||
		(FLAG(r_ptr, RF_S_DEMON)) ||
		(FLAG(r_ptr, RF_S_UNDEAD)) ||
		(FLAG(r_ptr, RF_S_DRAGON)) ||
		(FLAG(r_ptr, RF_S_HI_UNDEAD)) ||
		(FLAG(r_ptr, RF_S_HI_DRAGON)) ||
		(FLAG(r_ptr, RF_S_AMBERITES)) ||
		(FLAG(r_ptr, RF_S_UNIQUE)) ||
		(FLAG(r_ptr, RF_QUESTOR)))
		dam += ((dam * 3) / 2);

	/* Try to conserve missiles. */
	if ((!borg_use_missile) &&
		(typ == GF_ARROW ||
		(typ >= GF_ARROW_FLAME && typ <= GF_ARROW_DRAGON)))
	{
		/* Set damage to zero, force borg to melee attack */
		dam = 0;
	}

	/* Damage */
	return (dam);
}


/* Simulate the launching of a bolt at a monster */
static int borg_launch_aux_hack(int i, int dam, int typ)
{
	int d, p, x, y;

	map_block *mb_ptr;

	/* Monster */
	borg_kill *kill = &borg_kills[i];

	/* Skip dead monsters */
	if (!kill->r_idx) return (0);

	/* Require current knowledge */
	if (kill->when < borg_t) return (0);

	/* Acquire location */
	x = kill->x;
	y = kill->y;

	/* Bounds checking */
	if (!map_in_bounds(x, y)) return (0);

	/* Acquire the grid */
	mb_ptr = map_loc(x, y);

	/* Calculate damage */
	d = borg_launch_damage_one(i, dam, typ);

	/* Calculate danger */
	borg_full_damage = TRUE;
	p = borg_danger_aux(x, y, 1, i, TRUE);
	borg_full_damage = FALSE;

	/* Return Damage as pure danger of the monster */
	if (typ == GF_AWAY_ALL || typ == GF_AWAY_EVIL) return (d);

	/* Return 0 if the true damge (w/o the danger bonus) is 0 */
	if (d <= 0) return (d);

	/* Hack -- avoid waking most "hard" sleeping monsters */
	if ((kill->m_flags & MONST_ASLEEP) && (p > avoidance / 2) &&
		(d < kill->power))
	{
		return (-999);
	}

	/* Hack -- ignore sleeping town monsters */
	if (!bp_ptr->depth && (kill->m_flags & MONST_ASLEEP))
	{
		return (0);
	}

	/* Calculate "danger" to player */
	borg_full_damage = TRUE;
	p = borg_danger_aux(c_x, c_y, 2, i, TRUE);
	borg_full_damage = FALSE;

	/* Reduce "bonus" of partial kills */
	if (d < kill->power) p = p / 10;

	/* Add in power */
	d += p;

	/* Result */
	return (d);
}


/* Determine the "reward" of casting a bolt.*/
static int borg_launch_bolt(int dam, int typ, int max)
{
	int i;
	int x, y;
	int n, b_n = 0;

	map_block *mb_ptr;

	/* Loop through all the boltable monsters */
	for (i = 0; i < borg_bolt_n; i++)
	{
		/* Acquire location */
		x = borg_bolt_x[i];
		y = borg_bolt_y[i];

		/* Maximal distance */
		if (distance(c_x, c_y, x, y) > max) break;

		/* Get the grid */
		mb_ptr = map_loc(x, y);

		/* Collect damage */
		n = borg_launch_aux_hack(mb_ptr->kill, dam, typ);

		/* Is it better than before? */
		if (n <= b_n) continue;

		/* Track this location */
		b_n = n;
		g_x = x;
		g_y = y;
	}

	/* Result */
	return (b_n);
}


/* Determine the "reward" of casting a beam. */
int borg_launch_beam(int dam, int typ, int max)
{
	int i;
	int x, y;
	int n, b_n = 0;

	map_block *mb_ptr;

	/* Loop through all the beamable monsters */
	for (i = 0; i < borg_beam_n; i++)
	{
		/* Acquire location of the beamable monsters */
		x = borg_beam_x[i];
		y = borg_beam_y[i];

		/* Maximal distance */
		if (distance(c_x, c_y, x, y) > max) break;

		/* Check the path for the beam */
		borg_mmove_init(c_x, c_y, x, y);

		/* Reset Counters */
		x = c_x;
		y = c_y;
		n = 0;

		/* Loop through the possible grids on the path */
		while (TRUE)
		{
			/* Bounds checking */
			if (!map_in_bounds(x, y)) break;

			/* Get the grid */
			mb_ptr = map_loc(x, y);

			/* Maximal distance */
			if (distance(c_x, c_y, x, y) > max) break;

			/* Collect damage */
			n = borg_launch_aux_hack(mb_ptr->kill, dam, typ);

			/* Stop beaming when the beam hits a wall */
			if (borg_cave_wall_grid(mb_ptr)) break;

			/* Get next grid */
			borg_mmove(&x, &y, c_x, c_y);
		}

		/* Is it better than before? */
		if (n <= b_n) continue;

		/* Track this location */
		b_n = n;
		g_x = x;
		g_y = y;
	}

	/* Result */
	return (b_n);
}

/* Determine the "reward" of casting a dispel */
static int borg_launch_dispel(int dam, int typ, int rad)
{
	int i;
	int x, y;
	int n = 0;

	map_block *mb_ptr;

	/* Loop through all the monsters in LOS */
	for (i = 0; i < borg_beam_n; i++)
	{
		/* Acquire location */
		x = borg_beam_x[i];
		y = borg_beam_y[i];

		/* Maximal distance */
		if (distance(c_x, c_y, x, y) > rad) continue;

		/* Get the grid */
		mb_ptr = map_loc(x, y);

		/* Collect damage */
		n += borg_launch_aux_hack(mb_ptr->kill, dam, typ);
	}

	/* Just making sure */
	g_x = c_x;
	g_y = c_y;

	/* Result */
	return (n);
}


static int borg_ball_item(map_block *mb_ptr, int typ)
{
	object_kind *k_ptr = &k_info[mb_ptr->object];

	/* check destroyed stuff. */
	if (!mb_ptr->object) return (0);

	switch (typ)
	{
		case GF_ACID:
		{
			/* rings/boots cost extra (might be speed!) */
			if (k_ptr->tval == TV_BOOTS) return (-200);
		}

		case GF_ELEC:
		{
			/* rings/boots cost extra (might be speed!) */
			if (k_ptr->tval == TV_RING) return (-200);
		}

		case GF_FIRE:
		{
			/* rings/boots cost extra (might be speed!) */
			if (k_ptr->tval == TV_BOOTS) return (-200);
		}

		case GF_COLD:
		{
			/* So many nice potions to be missed */
			if (k_ptr->tval == TV_POTION) return (-200);
		}

		case GF_MANA:
		{
			/* Used against uniques, allow the stuff to burn */
			return (0);
		}

		default: return (0);
	}
}


/* Determine the "reward" of casting a ball with radius = 0.*/
static int borg_launch_ball_zero(int dam, int typ, int max)
{
	int i;
	int x, y;
	int n, b_n = 0;

	map_block *mb_ptr;

	/* Loop through all the ballable monsters in LOS */
	for (i = 0; i < borg_beam_n; i++)
	{
		/* Acquire location */
		x = borg_beam_x[i];
		y = borg_beam_y[i];

		/* Maximal distance */
		if (distance(c_x, c_y, x, y) > max) continue;

		/* Get the grid */
		mb_ptr = map_loc(x, y);

		/* Collect damage */
		n = borg_launch_aux_hack(mb_ptr->kill, dam, typ);

		/* Does this cost me items? */
		n += borg_ball_item(mb_ptr, typ);

		/* Is it better than before? */
		if (n <= b_n) continue;

		/* Track this location */
		b_n = n;
		g_x = x;
		g_y = y;
	}

	/* Result */
	return (b_n);
}


/* Determine the "reward" of casting a ball centered on the player. */
static int borg_launch_blast(int dam, int typ, int max)
{
	int i, r;
	int x, y;
	int n = 0;

	map_block *mb_ptr;

	/* Loop through all the ballable monsters in LOS */
	for (i = 0; i < borg_beam_n; i++)
	{
		/* Acquire location */
		x = borg_beam_x[i];
		y = borg_beam_y[i];

		/* What is the distance */
		r = distance(c_x, c_y, x, y);

		/* Maximal distance */
		if (r > max) continue;

		/* Get the grid */
		mb_ptr = map_loc(x, y);

		/* Collect damage */
		n = borg_launch_aux_hack(mb_ptr->kill, dam / (r + 1), typ);

		/* Does this cost me items? */
		n += borg_ball_item(mb_ptr, typ);
	}

	/* Result */
	return (n);
}


/* Determine the "reward" of an attack on the monsters around the borg */
static int borg_launch_touch(int dam, int typ)
{
	int i;
	int x, y;
	int n = 0;

	map_block *mb_ptr;

	/* Loop through all the touchable monsters in LOS */
	for (i = 0; i < borg_next_n; i++)
	{
		/* Acquire location */
		x = borg_next_x[i];
		y = borg_next_y[i];

		/* Get the grid */
		mb_ptr = map_loc(x, y);

		/* Collect damage */
		n = borg_launch_aux_hack(mb_ptr->kill, dam, typ);
	}

	/* Result */
	return (n);
}


/*
 * Determine the "reward" of casting a ball
 *
 * Basically, we sum the "rewards" of doing the appropriate amount of
 * damage to each of the "affected" monsters.
 *
 */
static int borg_launch_ball(int rad, int dam, int typ, int max)
{
	int i, j, r;
	int x, y, x1, y1;
	int n, b_n = 0;

	map_block *mb_ptr;

	/* Balls with rad = 0 get special treatment */
	if (rad == BORG_BALL_RAD0) return (borg_launch_ball_zero(dam, typ, max));

	/* Loop through all the grids with a monster or monster next to it */
	for (i = 0; i < borg_ball_n; i++)
	{
		/* Acquire location */
		x = borg_ball_x[i];
		y = borg_ball_y[i];

		/* Maximal distance */
		if (distance(c_x, c_y, x, y) > max) continue;

		/* Reset counter */
		n = 0;

		/* loop through all close monsters to find the ones hit by the ball */
		for (j = 0; j < borg_temp_n; j++)
		{
			/* Acquire location */
			x1 = borg_temp_x[j];
			y1 = borg_temp_y[j];

			/* Get the distance */
			r = distance(x, y, x1, y1);

			/* Is it within blast radius */
			if (r > rad) continue;

			/* Bounds checking */
			if (!map_in_bounds(x1, y1)) continue;

			/* Get the grid */
			mb_ptr = map_loc(x1, y1);

			/* Collect damage, lowered by distance */
			n += borg_launch_aux_hack(mb_ptr->kill, dam / (r + 1), typ);

			/* Does this cost me items? */
			n += borg_ball_item(mb_ptr, typ);
		}

		/* Is it a better location than before? */
		if (n <= b_n) continue;

		/* Track it */
		b_n = n;
		g_x = x;
		g_y = y;
	}

	/* Result */
	return (b_n);
}


/* Whirlwind -- Attacks all adjacent monsters */
static int borg_attack_whirlwind(void)
{
	int y = 0, x = 0;
	int i;
	int dam = 0;

	map_block *mb_ptr;

	if (borg_simulate)
	{
		/* Scan neighboring grids */
		for (i = 0; i < borg_next_n; i++)
		{
			/* Fetch the coords */
			y = borg_next_y[i];
			x = borg_next_x[i];

			/* Fetch the spot on the map */
			mb_ptr = map_loc(x, y);

			/* is there a kill next to me */
			if (mb_ptr->kill)
			{
				/* Calculate "average" damage */
				dam += borg_thrust_damage_one(mb_ptr->kill);
			}
		}

		/* Return the damage for consideration */
		return (dam);
	}

	/* Not supposed to happen */
	borg_oops("The borg can't cast Whirlwind from here");
	return (0);
}


/*
 * This function assumes that the string act contains "rad. xxx" 
 * and converts xxx to a number
 */
static int borg_find_radius(cptr act)
{
	char *here;

	/* Just checking */
	if (!act) return (0);

	/* Find the substring for the radius */
	here = strstr(act, "rad. ");

	/* If no radius is mentioned give up */
	if (!here) return (0);

	/* Jump past the search string */
	here = here + 5;

	/* Return the radius */
	return (atoi(here));
}


/*
 * This function assumes that the string act contains "(xxx" or (xdy
 * and converts xxx or xdy to a number.
 */
static int borg_find_damage(cptr act)
{
	char *here;
	int dam = 0, ds = 0, level = 0;

	/* Just checking */
	if (!act) return (0);

	/* Find the substring for the damage */
	here = strstr(act, "(");

	/* If no damage is mentioned give up */
	if (!here) return (0);

	/* Jump past the search string */
	here = here + 1;

	/* If the damage is multiplied by level */
	if (prefix(act, "level * "))
	{
		/* Supply the level */
		level = bp_ptr->lev;

		/* Jump past the substring */
		act += 8;
	}

	/* As long as the string has digits in it */
	while (*here - '0' >= 0 && *here - '0' <= 9)
	{
		/* create the damage */
		dam = dam * 10 + *here++ - '0';
	}

	/* Multiply if necessary */
	if (level) dam *= level;

	/* Is the damage composed of dd and ds? */
	if (*here == 'd')
	{
		/* Jump past the die */
		here = here + 1;

		/* create the die */
		ds = atoi(here);

		/* calculate the average damage */
		dam = dam * (ds + 1) / 2;
	}

	/* return the damage found */
	return (dam);
}


/* Determine if this activation can do damage */
static int borg_damage_artifact_monster(cptr act)
{
	int rad = 0,
		gf = 0,
		dam = 0,
		style = 0;
	bool stop;

	/* Go through the string, word for word */
	while (act)
	{
		/* Initialize */
		stop = FALSE;

		/* For efficiency first check the first letter */
		switch (*act)
		{
			case 'a':
			{
				if (prefix(act, "acid")) gf = GF_ACID;
				else if (prefix(act, "arrow"))
				{
					style = BORG_BOLT;
					gf = GF_ARROW;
				}

				break;
			}
			case 'b':
			{
				if (prefix(act, "bolt")) style  = BORG_BOLT;
				else if (prefix(act, "beam")) style  = BORG_BEAM;
				else if (prefix(act, "ball")) style  = BORG_BALL;
				else if (prefix(act, "breathe")) style  = BORG_BALL;
				else if (prefix(act, "blast")) style  = BORG_BLAST;
				else if (prefix(act, "banish"))
				{
					style  = BORG_DISPEL;
					dam = 30;

					if (prefix(act, "banishment")) gf = GF_AWAY_ALL;
					else if (prefix(act, "banish evil")) gf = GF_AWAY_EVIL;
					else if (prefix(act, "banish undead")) gf = GF_AWAY_UNDEAD;
				}

				break;
			}
			case 'c':
			{
				if (prefix(act, "cloud")) style  = BORG_BALL;
				else if (prefix(act, "cold")) gf = GF_COLD;
				else if (prefix(act, "confusion")) gf = GF_CONFUSION;
				else if (prefix(act, "confuse")) gf = GF_OLD_CONF;
				else if (prefix(act, "chaos")) gf = GF_CHAOS;
				else if (prefix(act, "call"))
				{
					style  = BORG_BALL;
					dam = 150;
				}

				break;
			}
			case 'd':
			{
				if (prefix(act, "dark")) gf = GF_DARK;
				if (prefix(act, "drain life"))
				{
					style = BORG_BOLT;
					gf = GF_OLD_DRAIN;
				}
				if (prefix(act, "dispel"))
				{
					style = BORG_DISPEL;

					if (prefix(act, "dispel evil")) gf = GF_DISP_EVIL;
					else if (prefix(act, "dispel good")) gf = GF_DISP_GOOD;
					else if (prefix(act, "dispel demons")) gf = GF_DISP_DEMON;
					else if (prefix(act, "dispel living")) gf = GF_DISP_LIVING;
					else if (prefix(act, "dispel monster")) gf = GF_DISP_ALL;
				}

				break;
			}
			case 'e':
			{
				if (prefix(act, "elements")) gf = GF_MISSILE;
				else if (prefix(act, "every")) stop = TRUE;

				break;
			}
			case 'f':
			{
				/* Hack to prevent holy/hell fire from being overwritten */
				if (prefix(act, "fire") && !gf) gf = GF_FIRE;
				else if (prefix(act, "frost")) gf = GF_COLD;
				else if (prefix(act, "force")) gf = GF_FORCE;

				break;
			}
			case 'g':
			{
				if (prefix(act, "gravity")) gf = GF_GRAVITY;

				break;
			}
			case 'h':
			{
				if (prefix(act, "holy fire")) gf = GF_HOLY_FIRE;
				else if (prefix(act, "hell fire")) gf = GF_HELL_FIRE;

				break;
			}
			case 'i':
			{
				if (prefix(act, "inertia")) gf = GF_INERTIA;
				else if (prefix(act, "ice")) gf = GF_ICE;
				else if (prefix(act, "illumination"))
				{
					style = BORG_DISPEL;
					gf = GF_LITE_WEAK;
					dam = 18;
				}

				break;
			}
			case 'l':
			{
				if (prefix(act, "large")) rad = BORG_BALL_RAD3;
				else if (prefix(act, "lightning")) gf = GF_ELEC;
				else if (prefix(act, "light"))
				{
					gf = GF_LITE;
				
					if (prefix(act, "light area"))
					{
						style = BORG_DISPEL;
						gf = GF_LITE_WEAK;
						rad = BORG_BALL_RAD3;
					}
				}

				break;
			}
			case 'm':
			{
				if (prefix(act, "mana")) gf = GF_MANA;
				else if (prefix(act, "missile"))
				{
					style = BORG_BOLT;
					gf = GF_MISSILE;
				}
				/* Hack to prevent overwriting sleep_touch */
				else if (prefix(act, "monster") && !style)
				{
					style = BORG_BOLT;

					if (prefix(act, "monsters")) style = BORG_DISPEL;
				}

				break;
			}
			case 'n':
			{
				if (prefix(act, "nether")) gf = GF_NETHER;
				else if (prefix(act, "nexus")) gf = GF_NEXUS;
				else if (prefix(act, "nuke")) gf = GF_NUKE;

				break;
			}
			case 'p':
			{
				if (prefix(act, "poison")) gf = GF_POIS;
				else if (prefix(act, "plasma")) gf = GF_PLASMA;

				break;
			}
			case 'r':
			{
				if (prefix(act, "rad.")) rad = borg_find_radius(act);
				else if (prefix(act, "rocket"))
				{
					style = BORG_BALL;
					gf = GF_ROCKET;
				}

				break;
			}
			case 's':
			{
				if (prefix(act, "star")) gf = GF_ELEC;
				else if (prefix(act, "stinking")) gf = GF_POIS;
				else if (prefix(act, "shards")) gf = GF_SHARDS;
				else if (prefix(act, "sound")) gf = GF_SOUND;
				else if (prefix(act, "sunlight")) gf = GF_LITE_WEAK;
				else if (prefix(act, "sleep"))
				{
					gf = GF_OLD_SLEEP;
					dam = 20;
					
					if (prefix(act, "sleep nearby")) style = BORG_TOUCH;
				}
				else if (prefix(act, "slow"))
				{
					gf = GF_OLD_SLOW;
					dam = 20;
				}
				if (prefix(act, "strangling"))
				{
					style = BORG_BOLT;
					gf = GF_OLD_DRAIN;
				}
				if (prefix(act, "stone to mud"))
				{
					style = BORG_BOLT;
					gf = GF_KILL_WALL;
				}

				break;
			}
			case 't':
			{
				if (prefix(act, "time")) gf = GF_TIME;
				else if (prefix(act, "turn"))
				{
					style = BORG_DISPEL;
					dam = 20;

					if (prefix(act, "turns")) stop = TRUE;
					else if (prefix(act, "turn monsters")) gf = GF_TURN_ALL;
					else if (prefix(act, "turn evil")) gf = GF_TURN_EVIL;
				}
				else if (prefix(act, "teleport away"))
				{
					style = BORG_BEAM;
					gf = GF_AWAY_ALL;
					dam = 50;
				}

				break;
			}
			case 'v':
			{
				if (prefix(act, "vampiric drain"))
				{
					style = BORG_BOLT;
					gf = GF_OLD_DRAIN;
				}

				break;
			}
			case 'w':
			{
				if (prefix(act, "water")) gf = GF_WATER;
				else if (prefix(act, "whirlwind"))
				{
					style = BORG_TOUCH;
					dam = 1;
					gf = MAX_GF;
				}

				break;
			}
			case '(':
			{
				dam = borg_find_damage(act);

				break;
			}
			default: break;
		}

		/* Cut off */
		if (stop) break;

		/* Skip until next word */
		while (act && !stop)
		{
			/* Stop after a space was read */
			stop = *act == ' ';

			/* Next letter */
			act++;
		}
	}

	/* Not enough info */
	if (!style || !dam || !gf) return (0);

	/* Calculate the potential damage */
	switch (style)
	{
		case BORG_BOLT: return (borg_launch_bolt(dam, gf, MAX_RANGE));

		case BORG_BEAM: return (borg_launch_beam(dam, gf, MAX_RANGE));

		case BORG_BALL:
		{
			/* Set to default */
			if (!rad) rad = BORG_BALL_RAD2;

			return (borg_launch_ball(rad, dam, gf, MAX_RANGE));
		}

		case BORG_DISPEL:
		{
			/* Set to default */
			if (!rad) rad = MAX_RANGE;
			
			return (borg_launch_dispel(dam, gf, rad));
		}

		case BORG_BLAST: return (borg_launch_blast(dam, gf, rad));

		case BORG_TOUCH:
		{
			/* Hacking Whirlwind */
			if (gf == MAX_GF) return (borg_attack_whirlwind());

			return (borg_launch_touch(dam, gf));
		}

		default: return (0);
	}
}


/* Simulate/Apply the optimal result of activating an artifact */
static int borg_attack_artifact(int *b_slot)
{
	int i, n, b_n = 0;
	list_item *l_ptr;
	cptr act;

	if (borg_simulate)
	{
		for (i = 0; i < equip_num; i++)
		{
			/* What item is this */
			l_ptr = look_up_equip_slot(i);

			/* Is this item an artifact that can be activated now? */
			if (!borg_check_artifact(l_ptr, TRUE)) continue;

			/* Hack!  Get the activation */
			act = item_activation(&p_ptr->equipment[i]);

			/* Get the attack value */
			n = borg_damage_artifact_monster(act);

			/* Is it better than before? */
			if (n <= b_n) continue;

			/* Keep track of the scroll */
			*b_slot = i;
			b_n = n;
		}

		/* Return the value of the simulation */
		return (b_n);
	}

	/* Set the target */
	borg_target(g_x, g_y);

	/* Do it */
	borg_note("# Activating artifact %s", equipment[*b_slot].o_name);

	/* Activate the artifact */
	borg_keypress('A');
	borg_keypress(I2A(*b_slot));

	/* Set our shooting flag */
	successful_target = BORG_FRESH_TARGET;

	/* Value */
	return (b_n);
}


static int borg_scroll_damage_monster(int sval)
{
	switch (sval)
	{
		case SV_SCROLL_ICE:
		{
			/* With resistancy it is safe to read this scroll */
			if (FLAG(bp_ptr, TR_RES_COLD))
			{
				/* How much damage from a cold ball? */
				return (borg_launch_dispel(150, GF_COLD, BORG_BALL_RAD4));
			}
			return (0);
		}

		case SV_SCROLL_FIRE:
		{
			/* With resistancy it is safe to read this scroll */
			if (FLAG(bp_ptr, TR_RES_FIRE))
			{
				/* How much damage from a fire ball? */
				return (borg_launch_dispel(75, GF_FIRE, BORG_BALL_RAD4));
			}
			return (0);
		}

		/* Scroll of Logrus */
		case SV_SCROLL_CHAOS:
		{
			/* With resistancy it is safe to read this scroll */
			if (FLAG(bp_ptr, TR_RES_CHAOS))
			{
				/* How much damage from a chaos ball? */
				return (borg_launch_dispel(225, GF_CHAOS, BORG_BALL_RAD4));
			}
			return (0);
		}

		case SV_SCROLL_DISPEL_UNDEAD:
		{
			/* Damage all the undead in LOS. */
			return (borg_launch_dispel(60, GF_DISP_UNDEAD, MAX_SIGHT));
		}

		default:
		{
			/* This scroll is a dud, damagewise*/
			return (0);
		}
	}
}


/*
 * Simulate/Apply the optimal result of reading a scroll
 *
 */
static int borg_attack_scroll(int *b_slot)
{
	int n, b_n = 0;
	int k;

	/* Simulation */
	if (borg_simulate)
	{
		/* No reading while blind, confused, or hallucinating */
		if (bp_ptr->status.blind || bp_ptr->status.confused ||
			bp_ptr->status.image) return (0);

		/* Try all scrolls */
		for (k = 0; k < inven_num; k++)
		{
			list_item *l_ptr = &inventory[k];

			/* Skip the wrong scrolls */
			if (l_ptr->tval != TV_SCROLL) continue;

			/* How much damage does this scroll do? */
			n = borg_scroll_damage_monster(k_info[l_ptr->k_idx].sval);
					
			/* Is it better than before? */
			if (n <= b_n) continue;

			/* Keep track of the scroll */
			*b_slot = k;
			b_n = n;
		}
		/* Return the value of the simulation */
		return (b_n);
	}

	/* Do it */
	borg_note("# Reading scroll '%s'", inventory[*b_slot].o_name);

	/* Read the scroll */
	borg_keypress('r');
	borg_keypress(I2A(*b_slot));

	/* Set our shooting flag */
	successful_target = BORG_FRESH_TARGET;

	/* Value */
	return (b_n);
}


/* This function checks if some missile has a certain damage_type */
static bool borg_missile_equals_type(list_item *l_ptr, int gf_i)
{
	/* Just making sure it is a missile */
	if (!l_ptr ||
		l_ptr->tval < TV_SHOT ||
		l_ptr->tval > TV_BOLT) return (FALSE);

	switch (gf_i)
	{
		/* Normal, unidentified, wounding or slaying missiles */
		case GF_ARROW:
		{
			if (borg_obj_is_ego_art(l_ptr) &&
				!strstr(l_ptr->o_name, "Wounding") &&
				!strstr(l_ptr->o_name, "Returning") &&
				!strstr(l_ptr->o_name, "Slaying")) return (FALSE);

			return (TRUE);
		}

		/* Flaming missiles */
		case GF_ARROW_FLAME:
			return (KN_FLAG(l_ptr, TR_BRAND_FIRE));

		/* Freezing missiles */
		case GF_ARROW_FROST:
			return (KN_FLAG(l_ptr, TR_BRAND_COLD));

		/* Electric missiles */
		case GF_ARROW_SHOCKING:
			return (KN_FLAG(l_ptr, TR_BRAND_ELEC));

		/* Amimal missiles */
		case GF_ARROW_ANIMAL:
			return (KN_FLAG(l_ptr, TR_SLAY_ANIMAL));

		/* Evil missiles */
		case GF_ARROW_EVIL:
			return (KN_FLAG(l_ptr, TR_SLAY_EVIL));

		/* Dragon missiles */
		case GF_ARROW_DRAGON:
			return (KN_FLAG(l_ptr, TR_SLAY_DRAGON));

       	/* Exploding missiles */
		case GF_ARROW_EXPLOSION:
			return (KN_FLAG(l_ptr, TR_EXPLODE));

		default:
		{
			return (FALSE);
		}
	}
}


/* This function returns the damage type of some missile */
static bool borg_missile_type(list_item *l_ptr)
{
	/* Just making sure it is a missile */
	if (!l_ptr ||
		l_ptr->tval < TV_SHOT ||
		l_ptr->tval > TV_BOLT) return (0);

	/* Cursed missiles are ignored */
	if (!streq(l_ptr->o_name, "") &&
		strstr(l_ptr->o_name, "{cursed")) return (GF_NONE);

	/* Flaming missiles */
	if (KN_FLAG(l_ptr, TR_BRAND_FIRE)) return (GF_ARROW_FLAME);

	/* Freezing missiles */
	if (KN_FLAG(l_ptr, TR_BRAND_COLD)) return (GF_ARROW_FROST);

	/* Electric missiles */
	if (KN_FLAG(l_ptr, TR_BRAND_ELEC)) return (GF_ARROW_SHOCKING);

	/* Amimal missiles */
	if (KN_FLAG(l_ptr, TR_SLAY_ANIMAL)) return (GF_ARROW_ANIMAL);

	/* Evil missiles */
	if (KN_FLAG(l_ptr, TR_SLAY_EVIL)) return (GF_ARROW_EVIL);

	/* Dragon missiles */
	if (KN_FLAG(l_ptr, TR_SLAY_DRAGON)) return (GF_ARROW_DRAGON);

	/* Exploding missiles */
	if (KN_FLAG(l_ptr, TR_EXPLODE)) return (GF_ARROW_EXPLOSION);

	/* None of the listed types so it must be a normal missile */
	return (GF_ARROW);
}


/*
 * Simulate/Apply the optimal result of launching a missile
 *
 * Check out which ammo is available and then call the apropriate routine for it
 *
 */
static int borg_attack_launch(int *b_slot)
{
	int n, b_n = 0;
	int b_x = 0, b_y = 0;
	int i, k, b_k = 0;
	int d, b_d;
	int gf_i;

	list_item *l_ptr;
	list_item *bow = look_up_equip_slot(EQUIP_BOW);

	/* Simulation */
	if (borg_simulate)
	{
		/* No firing while blind, confused, or hallucinating */
		if (bp_ptr->status.blind || bp_ptr->status.confused ||
			bp_ptr->status.image) return (0);

		/* Is there a bow? */
		if (!bow) return (0);

		/* Scan the pack to find out where the missiles are */
		for (i = 0; i < inven_num; i++)
		{
			l_ptr = &inventory[i];

			/* Skip non-missiles */
			if (l_ptr->tval != my_ammo_tval) continue;

			/* Skip missiles that have been considered already */
			if (l_ptr->treat_as == TREAT_AS_GONE)
			{
				l_ptr->treat_as = TREAT_AS_NORM;
				continue;
			}

			/* Determine type */
			gf_i = borg_missile_type(l_ptr);

			/* Reset tracker */
			b_d = 0;

			/* Search the rest of the missiles for the current type */
			for (k = i ; k < inven_num; k++)
			{
				l_ptr = &inventory[k];

				/* Stop when a non-missile is encountered */
				if (l_ptr->tval < my_ammo_tval) break;

				/* Is this an missile of the current type? */
				if (!borg_missile_equals_type(l_ptr, gf_i)) continue;

				/* Skip this missile in the future loops */
				if (k != i) l_ptr->treat_as = TREAT_AS_GONE;

				/* Determine average damage */
				d = (l_ptr->dd * (l_ptr->ds + 1) / 2);
				d = d + (100 + 3 * (l_ptr->to_d + bow->to_d)) / 100;
				d = d * bp_ptr->b_max_dam / 6;

				/* Is it better than the previous missile */
				if (d <= b_d) continue;

				/* Missiles tend to miss a lot, let's assume 50% */
				d = d / 2;

				/* Track this missile */
				b_d = d;
				b_k = k;
			}
		
			/* Find a target */
			n = borg_launch_bolt(b_d, gf_i, MAX_RANGE);
					
			/* Is it better than before? */
			if (n <= b_n) continue;

			*b_slot = b_k;
			b_n = n;
			b_x = g_x;
			b_y = g_y;
		}

		/* Set the targetting globals */
		g_x = b_x;
		g_y = b_y;

		/* Return the value of the simulation */
		return (b_n);
	}

	/* Set the target */
	borg_target(g_x, g_y);

	/* Do it */
	borg_note("# Firing missile '%s'", inventory[*b_slot].o_name);

	/* Fire */
	borg_keypress('f');

	/* Use the missile */
	borg_keypress(I2A(*b_slot));

	/* Reset our shooting flag */
	if (successful_target == BORG_TARGET)
	{
		successful_target = BORG_ARROW_TARGET;
	}

	/*
	 * Arrows tend to miss so there is a count down. BORG_ARROW_TARGET is a bit
	 * larger then BORG_FRESH_TARGET.  This has as a result that the borg has
	 * five shots to hit a monster across unknown terrain.  After that he'll
	 * stick to monsters in known terrain
	 */
	successful_target = successful_target - 1;

	/* Value */
	return (b_n);
}


/*
 * This procedure determines the damage that an object can do when thrown.
 * If you want to avoid a certain object to be thrown then it should appear
 * in the switch.
 */
static int borg_throw_damage(list_item *l_ptr, int *typ)
{
	int tval = l_ptr->tval;
	int d;

	/* Determine average damage from object */
	d = (l_ptr->dd * (l_ptr->ds + 1) / 2);

	/* Set the damage type */
	*typ = GF_ARROW;

	/* Skip un-identified, non-average, objects */
	if (!borg_obj_known_p(l_ptr) &&
		!strstr(l_ptr->o_name, "{average") &&
		!strstr(l_ptr->o_name, "{cursed") &&
		!strstr(l_ptr->o_name, "{dubious")) return (0);

	/* What sort of object have we here? */
	switch (tval)
	{
		/* Don't throw all the flasks when wearing a lantern */
		case TV_FLASK:
		{
			list_item* q_ptr = look_up_equip_slot(EQUIP_LITE);

			/*
			 * Don't throw the flask if the borg wields a lantern and
			 * he has only a few flasks. 
			 * Throw it anyway if he is fighting a unique
			 */
			if (q_ptr &&
				k_info[q_ptr->k_idx].sval == SV_LITE_LANTERN &&
				bp_ptr->able.fuel <= 7 &&
				!borg_fighting_unique) return (0);

			/* Throw the flask */
			return (d);
		}

		case TV_LITE:
		{
			list_item* q_ptr = look_up_equip_slot(EQUIP_LITE);

			/* If it is not a torch don't throw it */
			if (k_info[l_ptr->k_idx].sval != SV_LITE_TORCH) return (0);

			/* It the borg is wielding a torch keep 7 in reserve for light */
			if (q_ptr &&
				k_info[q_ptr->k_idx].sval == SV_LITE_TORCH &&
				bp_ptr->able.fuel <= 7) return (0);

			/* Throw the torch */
			return (d);
		}

		case TV_POTION:
		{
			/* Which potion is this? */
			switch (k_info[l_ptr->k_idx].sval)
			{
				case SV_POTION_RUINATION:
				case SV_POTION_DETONATIONS:
				{
					/* Set damage and damage type */
					*typ = GF_SHARDS;
					return (25 * (25 + 1) / 2);
				}
				case SV_POTION_DEATH:
				{
					/* Set damage and damage type */
					*typ = GF_DEATH_RAY;
					return (25 * (25 + 1) / 2);
				}
				case SV_POTION_POISON:
				{
					/* Set damage and damage type */
					*typ = GF_POIS;
					return (3 * (6 + 1) / 2);
					break;
				}
				case SV_POTION_RESTORE_MANA:
				{
					/* Only warriors should throw this */
					if (borg_class == CLASS_WARRIOR)
					{
						/* Set damage and damage type */
						*typ = GF_MANA;
						return (10 * (10 + 1) / 2);
					}
				}
				default:
				{
					/* Don't throw any other potion */
					return (0);
				}
			}
		}

		case TV_FOOD:
		case TV_SCROLL:
		case TV_ROD:
		case TV_WAND:
		case TV_STAFF:
		case TV_AMULET:
		case TV_RING:
		case TV_LIFE_BOOK:
		case TV_SORCERY_BOOK:
		case TV_NATURE_BOOK:
		case TV_CHAOS_BOOK:
		case TV_DEATH_BOOK:
		case TV_TRUMP_BOOK:
		case TV_ARCANE_BOOK:
		{
			/* Don't throw these, they have better uses */
			return (0);
		}

		default:
		{
			/* Anything else can go */
			return (d);
		}
	}
}


/*
 * Simulate/Apply the optimal result of throwing an object
 *
 * First choose the "best" object to throw, then check targets.
 */
static int borg_attack_object(int *b_slot, int mult)
{
	int n, b_n = 0;
	int b_x = 0, b_y = 0;
	int slot;

	int d, typ, r;
	int div, mul;


	if (borg_simulate)
	{
		/* No firing while blind, confused, or hallucinating */
		if (bp_ptr->status.blind || bp_ptr->status.confused ||
			bp_ptr->status.image) return (0);

		/* Scan the pack */
		for (slot = 0; slot < inven_num; slot++)
		{
			list_item *l_ptr = &inventory[slot];

			d = borg_throw_damage(l_ptr, &typ);

			/* Ignore 0 damage */
			if (d <= 0) continue;

			/* Extract a "distance multiplier" */
			mul = 5 + 5 * mult;

			/* Enforce a minimum "weight" of one pound */
			div = ((l_ptr->weight > 10) ? l_ptr->weight : 10);

			/* Hack -- Distance -- Reward strength, penalize weight */
			r = (adj_str_blow[my_stat_ind[A_STR]] + 20) * mul / div;

			/* Max distance of 10 */
			if (r > 10) r = 10;

			/* Choose optimal location */
			n = borg_launch_bolt(d, typ, r);

			if (n <= b_n) continue;

			/* Track */
			*b_slot = slot;
			b_n = n;
			b_x = g_x;
			b_y = g_y;
		}

		/* Set globals */
		g_x = b_x;
		g_y = b_y;

		/* Simulation */
		return (b_n);
	}

	/* Do it */
	borg_note("# Throwing painful object '%s'", inventory[*b_slot].o_name);

	/* Set the target */
	borg_target(g_x, g_y);
	
	/* Fire */
	borg_keypress('v');

	/* Use the object */
	borg_keypress(I2A(*b_slot));

	/* Set our shooting flag */
	successful_target = BORG_FRESH_TARGET;

	/* Value */
	return (b_n);
}


static int borg_ring_damage_monster(int sval)
{
	switch (sval)
	{
		case SV_RING_ICE:
			return (borg_launch_ball(BORG_BALL_RAD2, 100, GF_COLD, MAX_RANGE));

		case SV_RING_ACID:
			return (borg_launch_ball(BORG_BALL_RAD2, 100, GF_ACID, MAX_RANGE));

		case SV_RING_FLAMES:
			return (borg_launch_ball(BORG_BALL_RAD2, 100, GF_FIRE, MAX_RANGE));

		default:
			return (0);
	}
}


/*
 * Simulate/Apply the optimal result of Activating a ring
 */
static int borg_attack_ring(int *b_slot)
{
	int sval_l = -1, sval_r;
	int n = 0, b_n = 0;
	int b_x = c_x, b_y = c_y;

	if (borg_simulate)
	{
		/* Check the equipment */
		list_item *l_ptr = look_up_equip_slot(EQUIP_LEFT);

		/* Make sure the ring is IDed */
		if (l_ptr && borg_obj_known_p(l_ptr))
		{
			/* Check charge */
			if (!l_ptr->timeout)
			{
				/* Can we activate this ring */
				if (borg_use_item_fail(l_ptr, FALSE))
				{
					/* Which ring is this? */
					sval_l = k_info[l_ptr->k_idx].sval;

					/* Get the damage */
					b_n = borg_ring_damage_monster(sval_l);

					/* Make a note this is the left finger */
					*b_slot = EQUIP_LEFT;

					/* Keep track of the target */
					b_x = g_x;
					b_y = g_y;
				}
			}
		}

		/* Check the equipment */
		l_ptr = look_up_equip_slot(EQUIP_RIGHT);

		/* Make sure the ring is IDed */
		if (l_ptr && borg_obj_known_p(l_ptr))
		{
			/* Check charge */
			if (!l_ptr->timeout)
			{
				/* Can we activate this ring */
				if (borg_use_item_fail(l_ptr, FALSE))
				{
					/* Which ring is this? */
					sval_r = k_info[l_ptr->k_idx].sval;

					/* Not the same as the ring just tried */
					if (sval_r != sval_l)
					{
						/* Get the damage */
						n = borg_ring_damage_monster(sval_r);
					}
				}
			}
		}

		/* Which finger has bigger damage? */
		if (n > b_n)
		{
			/* So it is the right finger */
			*b_slot = EQUIP_RIGHT;

			/* Switch over the damage and the target */
			b_n = n;
			b_x = g_x;
			b_y = g_y;
		}

		/* Set the target global */
		g_x = b_x;
		g_y = b_y;

		/* Return damage */
		return (b_n);
	}

	/* Set the target */
	borg_target(g_x, g_y);

	/* Do it */
	borg_note("# Activating %s", equipment[*b_slot].o_name);

	/* Activate the ring*/
	borg_keypress('A');
	borg_keypress(I2A(*b_slot));

	/* Set our shooting flag */
	successful_target = BORG_FRESH_TARGET;

	/* Value */
	return (0);
}




/* How much damage does a dragon armour do? */
static int borg_dragon_damage_monster(int sval)
{
	int chance, gf_typ;

	switch (sval)
	{
		case SV_DRAGON_BLUE:
			return (borg_launch_ball(BORG_BALL_RAD2, 330, GF_ELEC, MAX_RANGE));

		case SV_DRAGON_WHITE:
			return (borg_launch_ball(BORG_BALL_RAD2, 370, GF_COLD, MAX_RANGE));

		case SV_DRAGON_BLACK:
			return (borg_launch_ball(BORG_BALL_RAD2, 430, GF_ACID, MAX_RANGE));

		case SV_DRAGON_GREEN:
			return (borg_launch_ball(BORG_BALL_RAD2, 500, GF_POIS, MAX_RANGE));

		case SV_DRAGON_RED:
			return (borg_launch_ball(BORG_BALL_RAD2, 670, GF_FIRE, MAX_RANGE));

		case SV_DRAGON_MULTIHUED:
		{
			chance = randint0(5);
			gf_typ =  (chance == 0) ? GF_ELEC :
					 ((chance == 1) ? GF_COLD :
					 ((chance == 2) ? GF_ACID :
					 ((chance == 3) ? GF_POIS
									: GF_FIRE)));
			return (borg_launch_ball(BORG_BALL_RAD2, 840, gf_typ, MAX_RANGE));
		}

		case SV_DRAGON_BRONZE:
			return (borg_launch_ball(BORG_BALL_RAD2, 400, GF_CONFUSION, MAX_RANGE));

		case SV_DRAGON_GOLD:
			return (borg_launch_ball(BORG_BALL_RAD2, 430, GF_SOUND, MAX_RANGE));

		case SV_DRAGON_CHAOS:
		{
			chance = randint0(2);
			gf_typ = (chance == 0) ? GF_CHAOS
								   : GF_DISENCHANT;
			return (borg_launch_ball(BORG_BALL_RAD2, 740, gf_typ, MAX_RANGE));
		}

		case SV_DRAGON_LAW:
		{
			chance = randint0(2);
			gf_typ = (chance == 0) ? GF_SOUND
								   : GF_SHARDS;
			return (borg_launch_ball(BORG_BALL_RAD2, 750, gf_typ, MAX_RANGE));
		}

		case SV_DRAGON_BALANCE:
		{
			chance = randint0(4);
			gf_typ =  (chance == 0) ? GF_CHAOS :
					 ((chance == 1) ? GF_SOUND :
					 ((chance == 2) ? GF_SHARDS
									: GF_DISENCHANT));
			return (borg_launch_ball(BORG_BALL_RAD2, 840, gf_typ, MAX_RANGE));
		}

		case SV_DRAGON_SHINING:
		{
			chance = randint0(2);
			gf_typ = (chance == 0) ? GF_LITE
								   : GF_DARK;
			return (borg_launch_ball(BORG_BALL_RAD2, 670, gf_typ, MAX_RANGE));
		}

		case SV_DRAGON_POWER:
			return (borg_launch_ball(BORG_BALL_RAD3, 1000, GF_MISSILE, MAX_RANGE));

		default:
			return (0);
	}
}


/*
 * Simulate/Apply the optimal result of Activating a Dragon armour
 */
static int borg_attack_dragon(void)
{
	if (borg_simulate)
	{
		/* Check the equipment */
		list_item *l_ptr = look_up_equip_slot(EQUIP_BODY);

		/* Skip incorrect armours */
		if (!l_ptr || l_ptr->tval != TV_DRAG_ARMOR) return (0);

		/* Make Sure Mail is IDed */
		if (!borg_obj_known_p(l_ptr)) return (0);

		/* Check charge */
		if (l_ptr->timeout) return (0);

		/* Can we activate this dragon armour */
		if (!borg_use_item_fail(l_ptr, FALSE)) return (0);

		/* Return the damage */
		return (borg_dragon_damage_monster(k_info[l_ptr->k_idx].sval));
	}

	/* Set the target */
	borg_target(g_x, g_y);

	/* Do it */
	borg_note("# Activating %s", equipment[EQUIP_BODY].o_name);

	/* Activate the dragon armour*/
	borg_keypress('A');
	borg_keypress(I2A(EQUIP_BODY));

	/* Set our shooting flag */
	successful_target = BORG_FRESH_TARGET;

	/* Value */
	return (0);
}


static int borg_rod_damage_monster(int sval)
{
	switch (sval)
	{
		case SV_ROD_ELEC_BOLT:
			return (borg_launch_bolt(22, GF_ELEC, MAX_RANGE));

		case SV_ROD_COLD_BOLT:
			return (borg_launch_bolt(27, GF_COLD, MAX_RANGE));

		case SV_ROD_ACID_BOLT:
			return (borg_launch_bolt(27, GF_ACID, MAX_RANGE));

		case SV_ROD_FIRE_BOLT:
			return (borg_launch_bolt(45, GF_OLD_SLEEP, MAX_RANGE));

		case SV_ROD_LITE:
			return (borg_launch_beam(27, GF_LITE_WEAK, MAX_RANGE));

		case SV_ROD_ILLUMINATION:
			return (borg_launch_dispel(18, GF_LITE_WEAK, BORG_BALL_RAD2));

		case SV_ROD_DRAIN_LIFE:
			return (borg_launch_bolt(150, GF_OLD_DRAIN, MAX_RANGE));

		case SV_ROD_ELEC_BALL:
			return (borg_launch_ball(BORG_BALL_RAD2, 75, GF_ELEC, MAX_RANGE));

		case SV_ROD_COLD_BALL:
			return (borg_launch_ball(BORG_BALL_RAD2, 100, GF_COLD, MAX_RANGE));

		case SV_ROD_ACID_BALL:
			return (borg_launch_ball(BORG_BALL_RAD2, 125, GF_ACID, MAX_RANGE));

		case SV_ROD_FIRE_BALL:
			return (borg_launch_ball(BORG_BALL_RAD2, 150, GF_FIRE, MAX_RANGE));

		case SV_ROD_SLOW_MONSTER:
			return (borg_launch_bolt(10, GF_OLD_SLOW, MAX_RANGE));

		case SV_ROD_SLEEP_MONSTER:
			return (borg_launch_bolt(10, GF_OLD_SLEEP, MAX_RANGE));

		case SV_ROD_PESTICIDE:
			return (borg_launch_ball(BORG_BALL_RAD3, 8, GF_POIS, MAX_RANGE));

		case SV_ROD_HAVOC:
			/* This has a random damage type, so just hope it is not resisted */
			return (borg_launch_ball(BORG_BALL_RAD2, 200, GF_MISSILE, MAX_RANGE));

		default:
			return (0);
	}
}


/*
 * Simulate/Apply the optimal result of using an attack rod
 */
static int borg_attack_rod(int *b_slot)
{
	int n, b_n = -1;
	int k;
	int b_x = 0, b_y = 0;
	list_item *l_ptr;

	/* Simulation */
	if (borg_simulate)
	{
		/* No firing while blind, confused, or hallucinating */
		if (bp_ptr->status.blind || bp_ptr->status.confused ||
			bp_ptr->status.image) return (0);

		/* Paranoia */
		if (randint0(100) < 5) return (0);

		/* Check the inventory for rods */
		for (k = 0; k < inven_num; k++)
		{
			l_ptr = &inventory[k];

			/* Skip non rods */
			if (l_ptr->tval != TV_ROD) continue;

			/* Skip if the whole pile is recharging */
			if (l_ptr->timeout == l_ptr->number) continue;

			/* Skip too hard rods */
			if (!borg_use_item_fail(l_ptr, FALSE)) continue;

			/* Get the damage done by the rod */
			n = borg_rod_damage_monster(k_info[l_ptr->k_idx].sval);

			/* Skip low results */
			if (n <= b_n) continue;

			/* Track this rod */
			b_n = n;
			*b_slot = k;

			/* Track the target */
			b_x = g_x;
			b_y = g_y;
		}

		/* Set the globals */
		g_x = b_x;
		g_y = b_y;

		return (b_n);
	}

	/* Set the target */
	borg_target(g_x, g_y);

	/* Tell what is zapped */
	borg_note("# Zapping %s", inventory[*b_slot].o_name);

	/* Zap the rod */
	borg_keypress('z');
	borg_keypress(I2A(*b_slot));

	/* Set our shooting flag */
	successful_target = BORG_FRESH_TARGET;

	/* Value */
	return (b_n);
}




/*  Find out how much damage a wand does */
static int borg_wand_damage_monster(int sval)
{
	switch (sval)
	{
		case SV_WAND_MAGIC_MISSILE:
			return (borg_launch_bolt(7, GF_MISSILE, MAX_RANGE));

		case SV_WAND_COLD_BOLT:
			return (borg_launch_bolt(27, GF_COLD, MAX_RANGE));

		case SV_WAND_ACID_BOLT:
			return (borg_launch_bolt(27, GF_ACID, MAX_RANGE));

		case SV_WAND_FIRE_BOLT:
			return (borg_launch_bolt(45, GF_FIRE, MAX_RANGE));

		case SV_WAND_SLOW_MONSTER:
			return (borg_launch_bolt(10, GF_OLD_SLOW, MAX_RANGE));

		case SV_WAND_SLEEP_MONSTER:
			return (borg_launch_bolt(10, GF_OLD_SLEEP, MAX_RANGE));

		case SV_WAND_CONFUSE_MONSTER:
			return (borg_launch_bolt(7, GF_OLD_CONF, MAX_RANGE));

		case SV_WAND_FEAR_MONSTER:
			return (borg_launch_bolt(7, GF_TURN_ALL, MAX_RANGE));

		case SV_WAND_ANNIHILATION:
			return (borg_launch_bolt(175, GF_OLD_DRAIN, MAX_RANGE));

		case SV_WAND_DRAIN_LIFE:
			return (borg_launch_bolt(150, GF_OLD_DRAIN, MAX_RANGE));

		case SV_WAND_LITE:
			return (borg_launch_beam(27, GF_LITE_WEAK, MAX_RANGE));

		case SV_WAND_STINKING_CLOUD:
			return (borg_launch_ball(BORG_BALL_RAD2, 15, GF_POIS, MAX_RANGE));

		case SV_WAND_ELEC_BALL:
			return (borg_launch_ball(BORG_BALL_RAD2, 75, GF_ELEC, MAX_RANGE));

		case SV_WAND_COLD_BALL:
			return (borg_launch_ball(BORG_BALL_RAD2, 100, GF_COLD, MAX_RANGE));

		case SV_WAND_ACID_BALL:
			return (borg_launch_ball(BORG_BALL_RAD2, 125, GF_ACID, MAX_RANGE));

		case SV_WAND_FIRE_BALL:
			return (borg_launch_ball(BORG_BALL_RAD2, 150, GF_FIRE, MAX_RANGE));

		case SV_WAND_WONDER:
		{
			/* check the danger */
			if (borg_launch_bolt(35, GF_MISSILE, MAX_RANGE) > 0 &&
				borg_danger(c_x, c_y, 1, TRUE) >= (avoidance * 2))
			{
				/* note the use of the wand in the emergency */
				borg_note("# Emergency use of a Wand of Wonder.");

				/* make the wand appear deadly */
				return (999);
			}
			else
			{
				return (0);
			}
		}

		case SV_WAND_DRAGON_COLD:
			return (borg_launch_ball(BORG_BALL_RAD3, 200, GF_COLD, MAX_RANGE));

		case SV_WAND_DRAGON_FIRE:
			return (borg_launch_ball(BORG_BALL_RAD3, 250, GF_FIRE, MAX_RANGE));

		case SV_WAND_ROCKETS:
			return (borg_launch_ball(BORG_BALL_RAD2, 250, GF_ROCKET, MAX_RANGE));

		default:
			return (0);
	}
}


/*
 * Simulate/Apply the optimal result of aiming a wand
 *
 * Check out which wand is available and then call the apropriate routine for it
 *
 */
static int borg_attack_wand(int *b_slot)
{
	int n, b_n = 0;
	int b_x = 0, b_y = 0;
	int i, k;
	int sval;

	list_item *l_ptr;

	/* Simulation */
	if (borg_simulate)
	{
		/* No firing while blind, confused, or hallucinating */
		if (bp_ptr->status.blind || bp_ptr->status.confused ||
			bp_ptr->status.image) return (0);

		/* Scan the pack to find out where the wands are */
		for (k = 0; k < inven_num; k++)
		{
			l_ptr = &inventory[k];

			/* Skip non-wands */
			if (l_ptr->tval != TV_WAND) continue;

			/* Skip wands that have been considered already */
			if (l_ptr->treat_as == TREAT_AS_GONE)
			{
				l_ptr->treat_as = TREAT_AS_NORM;
				continue;
			}

			/* Is this wand identified? */
			if (borg_obj_known_p(l_ptr))
			{
				/* Does it have charges? */
				if (!l_ptr->pval) continue;
			}
			else
			{
				/* Is it inscribed as {empty} */
				if (strstr(l_ptr->o_name, "{empty}")) continue;
			}

			/* Determine type */
			sval = k_info[l_ptr->k_idx].sval;

			/* Search the rest of the wands for the current type */
			i = borg_slot_from(TV_WAND, sval, k + 1);

			/* Skip this wand in the future loops */
			if (i != -1) inventory[i].treat_as = TREAT_AS_GONE;

			/* Find out how much damage this wand can do */
			n = borg_wand_damage_monster(sval);
					
			/* Is it better than before? */
			if (n <= b_n) continue;

			*b_slot = k;
			b_n = n;
			b_x = g_x;
			b_y = g_y;
		}

		/* Set the targetting globals */
		g_x = b_x;
		g_y = b_y;

		/* Return the value of the simulation */
		return (b_n);
	}

	/* Set the target */
	borg_target(g_x, g_y);

	/* Do it */
	borg_note("# Aiming %s", inventory[*b_slot].o_name);

	/* Fire */
	borg_keypress('a');

	/* Use the wand */
	borg_keypress(I2A(*b_slot));

	/* Set our shooting flag */
	successful_target = BORG_FRESH_TARGET;

	/* Value */
	return (b_n);
}


/*
 * Simulate/Apply the optimal result of making a racial physical attack
 */
static int borg_vampire_damage_monster(int dam)
{
	int p;

	int i, b_i = -1;
	int d, b_d = -1;
	int x, b_x = c_x;
	int y, b_y = c_y;

	map_block *mb_ptr;
	borg_kill *kill;
	monster_race *r_ptr;

	/* Too afraid to attack */
	if (bp_ptr->status.afraid) return (0);

	/* Fill the belly */
	if (!bp_ptr->status.full) dam = dam * 13 / 10;
	if (bp_ptr->status.hungry) dam = dam * 13 / 10;

	/* Examine possible destinations */
	for (i = 0; i < borg_next_n; i++)
	{
		x = borg_next_x[i];
		y = borg_next_y[i];

		/* Acquire grid */
		mb_ptr = map_loc(x, y);

		/* Obtain the monster */
		kill = &borg_kills[mb_ptr->kill];

		/* monster race */
		r_ptr = &r_info[mb_ptr->monster];

		/* Dont attack our buddies */
		if (kill->m_flags & MONST_PET) continue;

		/* Base Dam */
		d = dam;

		/* Drain works only on the living */
		if (!monster_living(r_ptr)) continue;

		/* Hack -- avoid waking most "hard" sleeping monsters */
		if ((kill->m_flags & MONST_ASLEEP) && (d <= kill->power))
		{
			/* Calculate danger */
			borg_full_damage = TRUE;
			p = borg_danger_aux(x, y, 1, mb_ptr->kill, TRUE);
			borg_full_damage = FALSE;

			if (p > avoidance / 2)
				continue;
		}

		/* Hack -- ignore sleeping town monsters */
		if (!bp_ptr->depth && (kill->m_flags & MONST_ASLEEP)) continue;

		/* Calculate "danger" to player */
		borg_full_damage = TRUE;
		p = borg_danger_aux(c_x, c_y, 2, mb_ptr->kill, TRUE);
		borg_full_damage = FALSE;

		/* Reduce "bonus" of partial kills */
		if (d <= kill->power) p = p / 10;

		/* Add the danger to the damage */
		d += p;

		/* Ignore lower damage */
		if ((b_i >= 0) && (d < b_d)) continue;

		/* Save the damage info */
		b_d = d;

		/* Keep the target spot */
		b_x = x;
		b_y = y;
	}

	/* Nothing to attack */
	if (b_d <= 0) return (0);

	/* Track the global */
	g_x = b_x;
	g_y = b_y;

	/* Return the simulation */
	return (b_d);
}


/* Simulate the damage done by the various racial abilities */
static int borg_racial_damage_monster(int race)
{
	int rad, dam;
	switch (race)
	{
		case RACE_VAMPIRE:
		{
			/* Suck Blood */
			dam = bp_ptr->lev + ((bp_ptr->lev / 2) * MAX(1, bp_ptr->lev / 10));	/* Dmg */
			return (borg_vampire_damage_monster(dam));
		}

		case RACE_CYCLOPS:
		{
			/* Throw Boulder */
			dam = bp_ptr->lev * 3 / 2;
			return (borg_launch_bolt(dam, GF_MISSILE, MAX_RANGE));
		}

		case RACE_DARK_ELF:
		{
			/* Magic Missile */
			dam = (3 + ((bp_ptr->lev - 1)) / 4) * 5;
			return (borg_launch_bolt(dam, GF_MISSILE, MAX_RANGE));
		}

		case RACE_DRACONIAN:
		{
			/* Draconian Breath */
			dam = 2 * bp_ptr->lev;
			rad = 1 + bp_ptr->lev / 15;
			return (borg_launch_ball(rad, dam, GF_FIRE, MAX_RANGE));
		}

		case RACE_IMP:
		{
			/* Fireball */
			dam = bp_ptr->lev;
			rad = (bp_ptr->lev >= 30) ? BORG_BALL_RAD2 : BORG_BALL_RAD1;
			return (borg_launch_ball(rad, dam, GF_FIRE, MAX_RANGE));
		}

		case RACE_KLACKON:
		{
			/* Acidball */
			dam = bp_ptr->lev;
			rad = (bp_ptr->lev >= 25) ? BORG_BALL_RAD2 : BORG_BALL_RAD1;
			return (borg_launch_ball(rad, dam, GF_ACID, MAX_RANGE));
		}

		case RACE_KOBOLD:
		{
			/* Poison bolt */
			dam = bp_ptr->lev;
			return (borg_launch_bolt(dam, GF_POIS, MAX_RANGE));
		}

		case RACE_MIND_FLAYER:
		{
			/* Mindblast bolt */
			dam = bp_ptr->lev;
			return (borg_launch_bolt(dam, GF_PSI, MAX_RANGE));
		}

		case RACE_SPRITE:
		{
			/* Sleep III */
			dam = bp_ptr->lev;
			rad = MAX_SIGHT;
			return (borg_launch_dispel(dam, GF_OLD_SLEEP, rad));
		}

		case RACE_YEEK:
		{
			/* Scare Mon */
			dam = bp_ptr->lev;
			return (borg_launch_bolt(dam, GF_TURN_ALL, MAX_RANGE));
		}
	}
	
	/* Paranoia */
	return (0);
}


/* Simulate/Apply the optimal result of Using a racial power. */
static int borg_attack_racial(void)
{
	if (borg_simulate)
	{
		/* Check for ability */
		if (!borg_racial_check(borg_race, TRUE)) return (FALSE);

		/* What is the damage? */
		return (borg_racial_damage_monster(borg_race));
	}

	/* Note */
	borg_note("# Racial Attack ");

	/* Set the target */
	borg_target(g_x, g_y);

	/* Activate */
	borg_keypress('U');

	/* Select the power.  All racial attack are in the first spot */
	borg_keypress('a');

	if (borg_race == RACE_VAMPIRE)
	{
		/* Bite to the grid next to the borg */
		borg_keypress(I2D(borg_extract_dir(c_x, c_y, g_x, g_y)));
	}

	/* Set our shooting flag */
	successful_target = BORG_FRESH_TARGET;

	/* Success */
	return (0);
}


/* How much damage can a mutation do */
static int borg_mutate_damage_monster(u32b mut_nr, int *slot)
{
	int n, dam, rad = MAX_RANGE;

	/* What mutation have we here? */
	switch (mut_nr)
	{
		/* Acid ball */
		case MUT1_SPIT_ACID:
		{
			dam = bp_ptr->lev;
			rad = 1 + bp_ptr->lev / 30;
			return (borg_launch_ball(rad, dam, GF_ACID, MAX_RANGE));
		}

		/* Fire breath */
		case MUT1_BR_FIRE:
		{
			dam = bp_ptr->lev * 2;
			rad = 1 + bp_ptr->lev / 20;
			return (borg_launch_ball(rad, dam, GF_FIRE, MAX_RANGE));
		}

		/* Psi bolt */
		case MUT1_MIND_BLST:
		{
			dam = 2 * (3 + (bp_ptr->lev - 1) / 5);
			return (borg_launch_bolt(dam, GF_PSI, rad));
		}

		/* Nuke'em */
		case MUT1_RADIATION:
		{
			dam = 2 * bp_ptr->lev;
			rad = 3 + bp_ptr->lev / 20;
			return (borg_launch_ball(rad, dam, GF_NUKE, MAX_RANGE));
		}

		/* Have a bite */
		case MUT1_VAMPIRISM:
		{
			dam = 2 * bp_ptr->lev;
			return (borg_vampire_damage_monster(dam));
		}

		/* Sound of Music */
		case MUT1_SHRIEK:
		{
			dam = 2 * bp_ptr->lev;
			rad = 8;
			return (borg_launch_dispel(dam, GF_SOUND, rad));
		}

		/* Light area */
		case MUT1_ILLUMINE:
		{
			dam = bp_ptr->lev;
			rad = 1 + bp_ptr->lev / 10;
			return (borg_launch_dispel(dam, GF_LITE_WEAK, rad));
		}

		/* hit and phase door in one move like a novice rogue */
		case MUT1_PANIC_HIT:
		{
			/* Its damage is at least equal to a normal hit */
			dam = borg_attack_thrust();

			/* If there are a few monsters around then add bonus */
			if (borg_temp_n < 5) dam = dam * 15 / 10;

			/* Return the damage */
			return (dam);
		}

		/* Mass confuse, stun and scare */
		case MUT1_DAZZLE:
		{
			dam = 20;
			n = borg_launch_dispel(dam, GF_OLD_CONF, rad);
			return (n + borg_launch_dispel(dam, GF_TURN_ALL, rad));
		}

		/* Lite beam */
		case MUT1_LASER_EYE:
		{
			dam = 2 * bp_ptr->lev;
			return (borg_launch_beam(dam, GF_LITE, rad));
		}

		/* Touch to freeze */
		case MUT1_COLD_TOUCH:
		{
			dam = 2 * bp_ptr->lev;
			rad = 1;
			return (borg_launch_bolt(dam, GF_COLD, rad));
		}

		/* Throw something */
		case MUT1_LAUNCHER:
		{
			/* This is not a real radius, it is a factor */
			rad = 2 + bp_ptr->lev / 30;

			return (borg_attack_object(slot, rad));
		}

		/* dud mutation */
		default: return (0);
	}
}


/* Simulate/Apply the optimal result of Using a mutation. */
static int borg_attack_mutation(int *b_slot, int *b_spell)
{
	int i, n, b_n = 0;
	int b_x = c_x, b_y = c_y;
	u32b mut_nr = 0;
	int slot, spell;

	if (borg_simulate)
	{
		/* Find out if the there isn't a racial in the way */
		spell = borg_count_racial(borg_race) - 1;

		/* Loop through all the bits in bp_ptr->muta1 */
		for (i = 1; i < 32; i++)
		{
			/* get the current mutation */
			mut_nr = (mut_nr) ? mut_nr * 2 : 1;

			/* Does the borg have this mutation? */
			if (!(bp_ptr->muta1 & mut_nr)) continue;

			/* Advance the letter index */
			spell += 1;

			/* Check if it is castable right now */
			if (!borg_mutation_check(mut_nr, TRUE)) continue;

			/* What is the damage? */
			n = borg_mutate_damage_monster(mut_nr, &slot);

			/* Is it more than before? */
			if (n <= b_n) continue;

			/* Track it */
			b_n = n;
			*b_spell = spell;
			*b_slot = slot;
			b_x = g_x;
			b_y = g_y;
		}

		/* Set the globals */
		g_x = b_x;
		g_y = b_y;

		/* return the damage indication */
		return (b_n);
	}

	/* Note */
	borg_note("# Mutation Attack ");
	borg_note("With letter = %c", I2A(*b_spell));

	/* Set the target */
	borg_target(g_x, g_y);

	/* Activate */
	borg_keypress('U');

	/* Select the mutation */
	borg_keypress(I2A(*b_spell));

	/* Is this mutation that hits a neighbour? */
	if (bp_ptr->muta1 & MUT1_VAMPIRISM ||
		bp_ptr->muta1 & MUT1_PANIC_HIT)
	{
		/* Bite the neighbour */
		borg_keypress(I2D(borg_extract_dir(c_x, c_y, g_x, g_y)));
	}

	/* Set our shooting flag */
	successful_target = BORG_FRESH_TARGET;

	/* Success */
	return (0);
}


/* Figure out how much damage mindcrafter spells do */
static int borg_mindcrafter_damage_monster(int spell)
{
	int dam, rad;

	/* Which spell is this */
	switch (spell)
	{
		case MIND_NEURAL_BL:
		{
			/* Set damage and radius */
			dam = 3 + ((bp_ptr->lev - 1) / 4) * (3 + (bp_ptr->lev / 15)) / 2;
			rad = BORG_BALL_RAD0;

			/* Return the damage */
			return (borg_launch_ball(rad, dam, GF_PSI, MAX_RANGE));
		}

		case MIND_PULVERISE:
		{
			/* Set damage */
			dam = 8 + (bp_ptr->lev - 5) / 4;

			/* Is the borg grown up? */
			if (bp_ptr->lev < 20)
			{
				rad = BORG_BALL_RAD0;
			}
			else
			{
				rad = 1 + (bp_ptr->lev - 20) / 8;
			}

			/* Return the damage */
			return (borg_launch_ball(rad, dam, GF_SOUND, MAX_RANGE));
		}

		case MIND_MIND_WAVE:
		{
			/* This spell becomes the main staple after lvl 25 */

			/* First mind_wave doesn't reach far */
			if (bp_ptr->lev < 25)
			{
				/* Set radius */
				rad = 2 + bp_ptr->lev / 10;
				dam = bp_ptr->lev * 3 / 2;
		}
			else
			{
				/* Set radius */
				rad = MAX_SIGHT;
				dam = bp_ptr->lev * ((bp_ptr->lev - 5) / 10 + 1);
			}

			/* Return the damage */
			return (borg_launch_dispel(dam, GF_PSI, rad));
		}

		case MIND_PSYCHIC_DR:
		{
			/* Set damage and radius */
			dam = 7 * bp_ptr->lev / 4;
			rad = BORG_BALL_RAD0;

			/* Return the damage */
			return (borg_launch_ball(rad, dam, GF_PSI_DRAIN, MAX_RANGE));
		}

		case MIND_TELE_WAVE:
		{
			/* set the radius */
			rad = 3 + bp_ptr->lev / 10;

			/* Life begins at 40 */
			if (bp_ptr->lev < 40)
			{
				/* Set damage */
				dam = bp_ptr->lev * 3;
			}
			else
			{
				/* Set damage */
				dam = bp_ptr->lev * 4;
			}

			/* Return the damage */
			return (borg_launch_dispel(dam, GF_TELEKINESIS, rad));
		}

		default:
		{
			/* Any other spell does no damage */
			return (0);
		}
	}
}


/* Check the mindcrafter spells for damage */
static int borg_attack_mindcrafter(int *b_spell)
{
	int spell;
	int n, b_n = 0;
	int b_x = 0, b_y = 0;
	int fail_rate = (borg_fighting_unique ? 40 : 25);

	if (borg_simulate)
	{
		/* Are you a mindcrafter? */
		if (borg_class != CLASS_MINDCRAFTER) return (0);

		/* No firing while blind, confused, or hallucinating */
		if ((bp_ptr->status.blind && !(FLAG(bp_ptr, TR_TELEPATHY))) ||
			bp_ptr->status.confused ||
			bp_ptr->status.image) return (0);

		/* Loop through the spells */
		for (spell = 0; spell < MINDCRAFT_MAX; spell++)
		{
			borg_mind *as = &borg_minds[spell];

			/* Paranoia */
			if (randint0(100) < 5) continue;

			/* Require ability (right now) */
			if (!borg_mindcr_okay_fail(spell, as->level, fail_rate)) continue;

			/* Choose optimal location */
			n = borg_mindcrafter_damage_monster(spell);

			/* Penalize mana usage (Add 1 to stimulate neural blast) */
			n = n + 1 - as->power;

			/* Compare with previous */
			if (n <= b_n) continue;

			/* Track this spell */
			b_n = n;
			*b_spell = spell;
			b_x = g_x;
			b_y = g_y;
		}

		/* Set the global coords */
		g_x = b_x;
		g_y = b_y;

		/* Simulation */
		return (b_n);
	}

	/* Set target for some spells */
	if (*b_spell == MIND_NEURAL_BL ||
		*b_spell == MIND_PULVERISE ||
		*b_spell == MIND_PSYCHIC_DR) borg_target(g_x, g_y);

	/* Cast the spell */
	(void)borg_mindcr(*b_spell, borg_minds[*b_spell].level);

	/* Set our shooting flag */
	successful_target = BORG_FRESH_TARGET;

	/* Value */
	return (b_n);
}


/* Check the mindcrafter spells for damage in emergency cases */
static int borg_attack_mindcrafter_reserve(bool faint, int *b_spell)
{
	int spell;
	int n, b_n = 0;
	int b_x = 0, b_y = 0;
	int fail_rate = (borg_fighting_unique ? 40 : 25);

	int monster;

	/* Fake our Mana */
	int sv_mana = bp_ptr->csp;

	bool spell_success;

	borg_kill *kill;

	if (borg_simulate)
	{
		/* Are you a mindcrafter? */
		if (borg_class != CLASS_MINDCRAFTER) return (0);

		/* No firing while blind, confused, or hallucinating */
		if ((bp_ptr->status.blind && !(FLAG(bp_ptr, TR_TELEPATHY))) ||
			bp_ptr->status.confused ||
			bp_ptr->status.image) return (0);

		/* Fainting is only for little guys */
		if (faint && bp_ptr->lev >= 20) return (0);

		/* Only big guys have reserve_mana */
		if (!faint && !borg_reserve_mana()) return (0);

		/* There can only be one monster closeby */
		if (borg_bolt_n != 1) return (0);

		/* Must be dangerous */
		if (faint && borg_danger(c_x, c_y, 1, TRUE) < avoidance * 2) return (0);

		/* Loop through the spells */
		for (spell = 0; spell < MINDCRAFT_MAX; spell++)
		{
			borg_mind *as = &borg_minds[spell];

			/* Paranoia */
			if (randint0(100) < 5) continue;

			/* No point of trying unknown spells */
			if (bp_ptr->lev < as->level) continue;

			/* Require inability (right now) */
			if (borg_mindcr_okay_fail(spell, as->level, fail_rate)) continue;

			/* If there is enough mana then keep trying */
			if (!faint && bp_ptr->csp < as->power) continue;

			/* Does the lack of mana bust the failrate? */
			if (faint &&
				borg_mindcr_fail_rate(spell, bp_ptr->lev) > fail_rate) continue;

			/* Pretend there is enough mana */
			bp_ptr->csp = bp_ptr->msp;

			/* Can you cast the spell now? */
			spell_success = borg_mindcr_okay_fail(spell, as->level, fail_rate);

			/* Restore original mana */
			bp_ptr->csp = sv_mana;

			/* The fail rate was too bad */
			if (!spell_success) continue;

			/* Choose optimal location */
			n = borg_mindcrafter_damage_monster(spell);

			/* Find the index to the monster */
			monster = map_loc(borg_bolt_x[0], borg_bolt_y[0])->kill;

			/* Find the actual monster */
			kill = &borg_kills[monster];

			/* If the monster has more HP then a good hit don't try */
			if (kill->power > n * 15 / 10) continue;

			/* Compare with previous */
			if (n <= b_n) continue;

			/* Track this spell */
			b_n = n;
			*b_spell = spell;
			b_x = g_x;
			b_y = g_y;
		}

		/* Set the global coords */
		g_x = b_x;
		g_y = b_y;

		/* Simulation */
		return (b_n);
	}

	/* make a note */
	borg_note("Emergency mindcr use: %s", (faint) ? "faint" : "reserve");

	/* Set target for some spells */
	if (*b_spell == MIND_NEURAL_BL ||
		*b_spell == MIND_PULVERISE ||
		*b_spell == MIND_PSYCHIC_DR) borg_target(g_x, g_y);

	/* Pretend the borg has enough mana for this */
	bp_ptr->csp = bp_ptr->msp;

	/* Cast the spell */
	(void)borg_mindcr(*b_spell, borg_minds[*b_spell].level);

	/* Close your eyes */
	if (faint)
	{
		/* confirm the spell use */
		borg_press_faint_accept();
	}

	/* Get the right amount of mana */
	bp_ptr->csp = MAX(0, sv_mana - borg_minds[*b_spell].power);

	/* Set our shooting flag */
	successful_target = BORG_FRESH_TARGET;

	/* Value */
	return (b_n);
}


/* This function returns the damage done by life spells */
static int borg_life_damage_monster(int book, int spell)
{
	int rad, dam, typ;

	switch (book)
	{
		/* Book of Common Prayer */
		case 0:
		{
			switch (spell)
			{
				/* Spell -- Call Light */
				case 4:
				{
					dam = bp_ptr->lev / 2;
					rad = bp_ptr->lev / 10 + 1;
					typ = GF_LITE_WEAK;

					/* How much damage is that? */
					return (borg_launch_dispel(dam, typ, rad));
				}

				default: return (0);
			}
		}

		/* High Mass */
		case 1:
		{
			switch (spell)
			{
				/* Spell -- Holy Orb */
				case 4:
				{
					/* Set basic damage */
					dam = 11 + bp_ptr->lev + bp_ptr->lev / 4;

					/* Is the borg a natural at this spell? */
					if (borg_class == CLASS_PRIEST ||
						borg_class == CLASS_HIGH_MAGE) dam += bp_ptr->lev / 4;

					/* High levels get a higher radius */
					rad = (bp_ptr->lev < 30) ? BORG_BALL_RAD2 : BORG_BALL_RAD3;

					typ = GF_HOLY_FIRE;

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
				}

				default: return (0);
			}
		}

		/* Book of the Unicorn */
		case 2:
		{
			switch (spell)
			{
				/* Spell -- Exorcism */
				case 0:
				{
					dam = bp_ptr->lev;
					rad = MAX_SIGHT;
					typ = GF_DISP_UNDEAD_DEMON;

					/* How much damage is that? */
					return (borg_launch_dispel(dam, typ, rad));
				}

				/* Spell -- Disp Undead & Demon */
				case 2:
				{
					dam = bp_ptr->lev * 3;
					rad = MAX_SIGHT;
					typ = GF_DISP_UNDEAD_DEMON;

					/* How much damage is that? */
					return (borg_launch_dispel(dam, typ, rad));
				}

				/* Spell -- Disp Evil */
				case 4:
				{
					dam = bp_ptr->lev * 4;
					rad = MAX_SIGHT;
					typ = GF_DISP_EVIL;

					/* How much damage is that? */
					return (borg_launch_dispel(dam, typ, rad));
				}

				/* Holy Word */
				case 6:
				{
					/* 
					 * Holy Word is the same as Disp Evil plus heal
					 * So only cast this when the borg has lost HP
					 */
					if (bp_ptr->mhp - bp_ptr->chp >= 300)
					{
						/* Increased damage to make him cast it */
						dam = bp_ptr->lev * 10;
						rad = MAX_SIGHT;
						typ = GF_DISP_EVIL;

						/* Choose optimal location-- */
						return (borg_launch_dispel(dam, typ, rad));
					}
					else
					{
						/* Don't bother if it doesn't heal (much) */
						return (0);
					}
				}

				default: return (0);
			}
		}

		/* Blessings of the Grail */
		case 3:
		{
			switch (spell)
			{
				/* Spell -- Divine Intervention */
				case 6:
				{
					int n;

					dam = 777;
					rad = BORG_BALL_RAD1;
					typ = GF_HOLY_FIRE;

					/* if hurting, add bonus */
					if (bp_ptr->mhp - bp_ptr->chp >= 200) dam = (dam * 12) / 10;

					/* Is the borg hasted? */
					if (borg_speed)
					{
						/* bonus for refreshing the speedy */
						dam = (dam * 11) / 10;
					}
					else
					{
						/* if no speedy, add bonus */
						dam = (dam * 13) / 10;
					}

					/* How much damage is that? */
					n = borg_launch_dispel(dam, typ, rad);

					/* If the borg damages a neighbour */
					if (n > 0)
					{
						/* There is a neighbour.  Now add in the other damage */
						dam = bp_ptr->lev * 4;
						rad = MAX_SIGHT;
						typ = GF_DISP_ALL;

						/* How much damage is that in total? */
						return (n + borg_launch_dispel(dam, typ, rad));
					}
					/* There is no neighbour */
					else
					{
						/* why bother with this expensive spell */
						return (0);
					}
				}

				default: return (0);
			}
		}

		default:
		{
			borg_oops("Trying to cast from life book = %d", book);
			return (0);
		}
	}
}


/* This function returns the damage done by sorcery spells */
static int borg_sorcery_damage_monster(int book, int spell)
{
	int rad, dam, typ;

	switch (book)
	{
		/* Beginner's Handbook */
		case 0:
		{
			switch (spell)
			{
				/* Spell -- Light area */
				case 3:
				{
					dam = bp_ptr->lev / 2;
					rad = bp_ptr->lev / 10 + 1;
					typ = GF_LITE_WEAK;

					/* How much damage is that? */
					return (borg_launch_dispel(dam, typ, rad));
				}

				case 4:
				{
					/* Spell -- Confuse Monster */
					dam = 10;
					typ = GF_OLD_CONF;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				case 6:
				{
					/* Spell -- Sleep I */
					dam = 10;
					typ = GF_OLD_SLEEP;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				default: return (0);
			}
		}

		/* Master Sorcerer's Handbook */
		case 1:
		{
			switch (spell)
			{
				/* Slow Monster */
				case 2:
				{
					dam = 10;
					typ = GF_OLD_SLOW;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				/* Spell -- Mass Sleep */
				case 3:
				{
					rad = MAX_SIGHT;
					dam = 10;
					typ = GF_OLD_SLEEP;

					/* How much damage is that? */
					return (borg_launch_dispel(dam, typ, rad));
				}

				default: return (0);
			}
		}

		/* No attack spells in Pattern Sorcery */
		case 2: return (0);

		/* Grimoire of Power */
		case 3:
		{
			switch(spell)
			{
				/* Spell -- Stasis */
				case 0:
				{
					dam = 10;
					typ = GF_STASIS;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				default: return (0);
			}
		}

		default:
		{
			borg_oops("Is book %d really a sorcery book?", book);
			return (0);
		}
	}
}


/* This function returns the damage done by nature spells */
static int borg_nature_damage_monster(int book, int spell)
{
	int rad, dam, typ;

	switch (book)
	{
		/* Call of the Wild */
		case 0:
		{
			switch (spell)
			{
				/* Spell -- Day Light */
				case 4:
				{
					dam = bp_ptr->lev / 2;
					rad = bp_ptr->lev / 10 + 1;
					typ = GF_LITE_WEAK;

					/* How much damage is that? */
					return (borg_launch_dispel(dam, typ, rad));
				}

				default: return (0);
			}
		}

		/* Nature Mastery */
		case 1:
		{
			switch (spell)
			{
				/* Spell -- Stone to Mud */
				case 0:
				{
					dam = 35;
					typ = GF_KILL_WALL;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				/* Spell -- lightning bolt */
				case 1:
				{
					dam = (13 + (bp_ptr->lev - 5) / 4) * 9 / 2;
					typ = GF_ELEC;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				/* Spell -- frost bolt */
				case 3:
				{
					dam = (5 + (bp_ptr->lev - 5) / 4) * 9 / 2;
					typ = GF_COLD;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				/* Spell -- Sunlight */
				case 4:
				{
					dam = 27;
					typ = GF_LITE_WEAK;

					/* Choose optimal location-- */
					return (borg_launch_beam(dam, typ, MAX_RANGE));
				}

				/* Spell -- Entangle */
				case 5:
				{
					rad = MAX_SIGHT;
					dam = 10;
					typ = GF_OLD_SLOW;

					/* How much damage is that? */
					return (borg_launch_dispel(dam, typ, rad));
				}

				default: return (0);
			}
		}

		/* Nature's Gifts has no damage spells */
		case 2: return (0);

		/* Nature's Wrath */
		case 3:
		{
			switch (spell)
			{
				/* Whirlwind */
				case 1:
				{
					return (borg_attack_whirlwind());
				}

				/* Blizzard */
				case 2:
				{
					dam = 70 + bp_ptr->lev;
					rad = bp_ptr->lev / 12 + 1;
					typ = GF_COLD;

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
				}

				/* Elec Storm */
				case 3:
				{
					dam = 90 + bp_ptr->lev;
					rad = bp_ptr->lev / 12 + 1;
					typ = GF_ELEC;

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
				}

				/* Whirlpool */
				case 4:
				{
					dam = 100 + bp_ptr->lev;
					rad = bp_ptr->lev / 12 + 1;
					typ = GF_WATER;

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
				}

				/* Call Sunlight */
				case 5:
				{
					/* Does light hurt? */
					if (FLAG(bp_ptr, TR_HURT_LITE) &&
						!FLAG(bp_ptr, TR_RES_LITE) &&
						!FLAG(bp_ptr, TR_IM_LITE))
					{
						/* Don't cast this */
						return (0);
					}
					else
					{
						dam = 150;
						rad = BORG_BALL_RAD8;
						typ = GF_LITE;

						/* Choose optimal location-- */
						return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
					}
				}

				/* Natures Wrath */
				case 7:
				{
					int n;

					/* Dispell all monsters */
					dam = 4 * bp_ptr->lev;
					rad = MAX_SIGHT;
					typ = GF_DISP_ALL;

					/* Calculate dispell damage */
					n = borg_launch_dispel(dam, typ, rad);

					/* Disintegrate ball centered on self */
					dam = bp_ptr->lev + 100;
					rad = bp_ptr->lev / 12 + 1;
					typ = GF_DISINTEGRATE;

					/* Return dispell + ball damage */
					return (n + borg_launch_dispel(dam, typ, rad));
				}

				default: return (0);
			}
		}

		default:
		{
			borg_oops("Is book %d really a Nature book?", book);
			return (0);
		}
	}
}


/* This function returns the damage done by chaos spells */
static int borg_chaos_damage_monster(int book, int spell)
{
	int rad, dam, typ;

	switch (book)
	{
		/* Sign of Chaos */
		case 0:
		{
			switch (spell)
			{
				/* Magic Missile */
				case 0:
				{
					dam = (3 + ((bp_ptr->lev - 1) / 5)) * 5 / 2;
					typ = GF_MISSILE;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				/* Flash of Light */
				case 2:
				{
					dam = bp_ptr->lev / 2 + 1;
					rad = bp_ptr->lev / 10 + 1;
					typ = GF_LITE_WEAK;

					/* How much damage is that? */
					return (borg_launch_dispel(dam, typ, rad));
				}

				/* Mana Burst */
				case 4:
				{
					/* Set basic damage */
					dam = 9 + bp_ptr->lev + bp_ptr->lev / 4;

					/* Is the borg a natural at this spell? */
					if (borg_class == CLASS_PRIEST ||
						borg_class == CLASS_HIGH_MAGE) dam += bp_ptr->lev / 4;

					/* Set radius and type */
					rad = ((bp_ptr->lev < 30) ? BORG_BALL_RAD2 : BORG_BALL_RAD3);
					typ = GF_MISSILE;

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
				}

				/* Fire Bolt */
				case 5:
				{
					dam = (8 + ((bp_ptr->lev - 5) / 4)) * 9 / 2;
					typ = GF_FIRE;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				/* Fist of Force */
				case 6:
				{
					dam = (8 + ((bp_ptr->lev - 5) / 4)) * 9 / 2;
					typ = GF_DISINTEGRATE;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				default: return (0);
			}
		}

		/* Chaos Mastery */
		case 1:
		{
			switch (spell)
			{
				/* Chaos Bolt */
				case 1:
				{
					dam = (10 + ((bp_ptr->lev - 5) / 4)) * 9 / 2;
					typ = GF_CHAOS;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				/* Sonic Boom */
				case 2:
				{
					dam = bp_ptr->lev + 45;
					rad = bp_ptr->lev / 10 + 2;
					typ = GF_SOUND;

					/* How much damage is that? */
					return (borg_launch_dispel(dam, typ, rad));
				}

				/* Doom Bolt */
				case 3:
				{
					dam = (11 + (bp_ptr->lev - 5) / 4) * 9 / 2;
					typ = GF_MANA;

					/* Choose optimal location-- */
					return (borg_launch_beam(dam, typ, MAX_RANGE));
				}

				/* Fire ball */
				case 4:
				{
					rad = BORG_BALL_RAD2;
					dam = bp_ptr->lev + 55;
					typ = GF_FIRE;

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
				}

				/* Invoke Logrus */
				case 7:
				{
					rad = bp_ptr->lev / 5;
					dam = bp_ptr->lev + 66;
					typ = GF_CHAOS;

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
				}

				default: return (0);
			}
		}

		/* Chaos Channels */
		case 2:
		{
			switch (spell)
			{
				/* Polymorph */
				case 0:
				{
					dam = 10;
					typ = GF_OLD_POLY;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				/* Chain Lightning */
				case 1:
				{
					rad = BORG_BALL_RAD8;
					dam = (5 + (bp_ptr->lev / 10)) * 9 / 2;
					typ = GF_ELEC;

					/* How much damage is that? */
					return (borg_launch_dispel(dam, typ, rad));
				}

				/* Disintegration */
				case 3:
				{
					rad = (bp_ptr->lev < 40) ? BORG_BALL_RAD3 : BORG_BALL_RAD4;
					dam = bp_ptr->lev + 80;
					typ = GF_DISINTEGRATE;

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
				}

				default: return (0);
			}
		}

		/* Armageddon Tome */
		case 3:
		{
			switch (spell)
			{
				/* Gravity */
				case 0:
				{
					dam = (9 + ((bp_ptr->lev - 5) / 4)) * 9 / 2;
					typ = GF_GRAVITY;

					/* Choose optimal location-- */
					return (borg_launch_beam(dam, typ, MAX_RANGE));
				}

				/* Meteor Swarm */
				case 1:
				{
					rad = BORG_BALL_RAD3;
					dam = bp_ptr->lev + 65;
					typ = GF_METEOR;

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
				}

				/* Flamestrike */
				case 2:
				{
					rad = BORG_BALL_RAD8;
					dam = 150 + bp_ptr->lev * 2;
					typ = GF_FIRE;

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
				}

				/* Rocket */
				case 4:
				{
					rad = BORG_BALL_RAD2;
					dam = 120 + bp_ptr->lev;
					typ = GF_SHARDS;

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
				}

				/* Mana Storm */
				case 5:
				{
					rad = BORG_BALL_RAD4;
					dam = 300 + bp_ptr->lev * 2;
					typ = GF_MANA;

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
				}

				/* Breath Logrus */
				case 6:
				{
					rad = BORG_BALL_RAD2;
					dam = bp_ptr->chp;
					typ = GF_CHAOS;

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
				}

				/* Call Void */
				case 7:
				{
					int y = 0, x = 0;
					int i;

					map_block *mb_ptr;

					dam = 0;

					/* Scan neighboring grids */
					for (i = 0; i < borg_next_n; i++)
					{
						/* Fetch the coords */
						y = borg_next_y[i];
						x = borg_next_x[i];

						mb_ptr = map_loc(x, y);

						/* is there a wall next to me */
						if (mb_ptr->feat >= FEAT_MAGMA &&
							mb_ptr->feat <= FEAT_PERM_SOLID)
						{
							/* Don't cast it when the borg is next to a wall */
							return (0);
						}
						else
						{
							/* Set the radius */
							rad = BORG_BALL_RAD2;
							dam = 175;

							/* Calculate "average" damage */
							dam += borg_launch_ball
								(rad, dam, GF_SHARDS, MAX_RANGE);
							dam += borg_launch_ball
								(rad, dam, GF_MANA, MAX_RANGE);
							dam += borg_launch_ball
								(BORG_BALL_RAD4, dam, GF_NUKE, MAX_RANGE);
						}

					}

					/* Return the damage for consideration */
					return (dam);
				}

				default: return (0);
			}
		}

		default:
		{
			borg_oops("Is book %d really a sorcery book?", book);
			return (0);
		}
	}
}


/* This function returns the damage done by death spells */
static int borg_death_damage_monster(int book, int spell)
{
	int rad, dam, typ;

	switch (book)
	{
		/* Black Prayers */
		case 0:
		{
			switch (spell)
			{
				/* Malediction */
				case 1:
				{
					rad = BORG_BALL_RAD1;
					dam = (3 + ((bp_ptr->lev - 1) / 5)) / 2;
					typ = GF_HELL_FIRE;

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
				}

				/* Poison Ball */
				case 4:
				{
					rad = BORG_BALL_RAD2;
					dam = 10 + bp_ptr->lev / 2;
					typ = GF_POIS;

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
				}

				/* Black Sleep */
				case 5:
				{
					dam = 10;
					typ = GF_OLD_SLEEP;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				/* Horrify */
				case 6:
				{
					dam = 10;
					typ = GF_TURN_ALL;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				default: return (0);
			}
		}

		/* Black Mass */
		case 1:
		{
			switch (spell)
			{
				/* Entropy */
				case 0:
				{
					rad = (bp_ptr->lev < 30) ? BORG_BALL_RAD2 : BORG_BALL_RAD3;

					/* Set basic damage */
					dam = 10 + bp_ptr->lev + bp_ptr->lev / 4;

					/* Is the borg a natural at this spell? */
					if (borg_class == CLASS_PRIEST ||
						borg_class == CLASS_HIGH_MAGE) dam += bp_ptr->lev / 4;

					typ = GF_OLD_DRAIN;

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
				}

				/* Nether Bolt */
				case 1:
				{
					dam = (6 + ((bp_ptr->lev - 5)) * 9 / 2);
					typ = GF_HELL_FIRE;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				/* Terror */
				case 2:
				{
					dam = bp_ptr->lev + 30;
					typ = GF_TURN_ALL;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				/* Vamp Drain */
				case 4:
				{
					dam = (bp_ptr->lev +
						  (bp_ptr->lev / 2 * ((9 + bp_ptr->lev) / 10)));
					typ = GF_OLD_DRAIN;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				/* Dispel Good */
				case 5:
				{
					rad = MAX_SIGHT;
					dam = bp_ptr->lev * 4;
					typ = GF_DISP_GOOD;

					/* How much damage is that? */
					return (borg_launch_dispel(dam, typ, rad));
				}

				default: return (0);
			}
		}

		/* Black Channels */
		case 2:
		{
			switch (spell)
			{
				/* Dark Bolt */
				case 2:
				{
					dam = (4 + ((bp_ptr->lev - 5) / 4) * 9 / 2);
					typ = GF_DARK;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				/* Vampirism True */
				case 4:
				{
					dam = 300;
					typ = GF_OLD_DRAIN;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				/* DarknessStorm */
				case 6:
				{
					dam = 120;
					rad = BORG_BALL_RAD4;
					typ = GF_DARK;

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
				}

				default: return (0);
			}
		}

		/* Necronomicon */
		case 3:
		{
			switch (spell)
			{
				/* Death Ray */
				case 0:
				{
					dam = bp_ptr->lev * 50;
					typ = GF_DEATH_RAY;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				/* Word of Death */
				case 3:
				{
					rad = MAX_SIGHT;
					dam = 3 * bp_ptr->lev;
					typ = GF_OLD_DRAIN;

					/* How much damage is that? */
					return (borg_launch_dispel(dam, typ, rad));
				}

				/* Evocation */
				case 4:
				{
					dam = bp_ptr->lev * 4;
					typ = GF_OLD_DRAIN;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				/* HellFire */
				case 5:
				{
					dam = 666;
					typ = GF_OLD_DRAIN;

					if (bp_ptr->chp < 200) return (0)
						;
					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				default: return (0);
			}
		}
		default:
		{
			borg_oops("Is book %d really a death book?", book);
			return (0);
		}
	}
}


/* This function returns the damage done by trump spells */
static int borg_trump_damage_monster(int book, int spell)
{
	int rad, dam;

	switch (book)
	{
		/* Conjuring & Tricks */
		case 0:
		{
			switch (spell)
			{
				/* Spell -- Mind Blast */
				case 1:
				{
					rad = BORG_BALL_RAD0;
					dam = 6 + 2 * (bp_ptr->lev - 1) / 5;

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, GF_PSI, MAX_RANGE));
				}

				default: return (0);
			}
		}

		/* No attack spells in Deck of Many Things */
		case 1: return (0);

		/* Trumps of Doom */
		case 2:
		{
			switch (spell)
			{
				/* Spell -- Death dealing */
				case 6:
				{
					dam = 3 * bp_ptr->lev;

					/* Choose optimal location-- */
					return (borg_launch_dispel(dam, GF_DISP_LIVING, MAX_RANGE));
				}

				default: return (0);
			}
		}

		/* Five Aces */
		case 3: return (0);

		default:
		{
			borg_oops("Trying to cast from trump book %d", book);
			return (0);
		}
	}
}


/* This function returns the damage done by arcane spells */
static int borg_arcane_damage_monster(int book, int spell)
{
	int rad, dam, typ;

	switch (book)
	{
		/* Cantrips for Beginners */
		case 0:
		{
			switch (spell)
			{
				/* Spell -- Zap */
				case 0:
				{
					dam = (3 + (bp_ptr->lev - 1) / 5) / 2;
					typ = GF_ELEC;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				/* Spell -- Light Area */
				case 5:
				{
					dam = bp_ptr->lev / 2;
					rad = bp_ptr->lev / 10 + 1;
					typ = GF_LITE_WEAK;

					/* How much damage is that? */
					return (borg_launch_dispel(dam, typ, rad));
				}

				default: return (0);
			}
		}

		/* No damage spells in Minor Arcana */
		case 1: return (0);

		/* Major Arcana */
		case 2:
		{
			switch(spell)
			{
				/* Spell -- Stone to Mud */
				case 4:
				{
					dam = 35;
					typ = GF_KILL_WALL;

					/* Choose optimal location-- */
					return (borg_launch_bolt(dam, typ, MAX_RANGE));
				}

				/* Spell -- Light Beam */
				case 5:
				{
					dam = 27;
					typ = GF_LITE_WEAK;

					/* Choose optimal location-- */
					return (borg_launch_beam(dam, typ, MAX_RANGE));
				}

				default: return (0);
			}
		}

		/* Manual of Mastery */
		case 3:
		{
			switch(spell)
			{
				/* Spell -- Elem Ball */
				case 4:
				{
					dam = 75 + bp_ptr->lev;
					rad = BORG_BALL_RAD2;

					/* Guess which type it will be */
					switch (randint1(4))
					{
						case 1: typ = GF_FIRE;
							break;
						case 2: typ = GF_ELEC;
							break;
						case 3: typ = GF_COLD;
							break;
						default: typ = GF_ACID;
							break;
					}

					/* Choose optimal location-- */
					return (borg_launch_ball(rad, dam, typ, MAX_RANGE));
				}

				default: return (0);
			}
		}

		default:
		{
			borg_oops("Trying to cast from arcane book %d", book);
			return (0);
		}
	}
}


/* Figure out the damage for the spells */
static int borg_spell_damage_monster(int realm, int book, int spell)
{
	switch (realm)
	{
		case REALM_LIFE:
		{
			return (borg_life_damage_monster(book, spell));
		}

		case REALM_SORCERY:
		{
			return (borg_sorcery_damage_monster(book, spell));
		}

		case REALM_NATURE:
		{
			return (borg_nature_damage_monster(book, spell));
		}

		case REALM_CHAOS:
		{
			return (borg_chaos_damage_monster(book, spell));
		}

		case REALM_DEATH:
		{
			return (borg_death_damage_monster(book, spell));
		}

		case REALM_TRUMP:
		{
			return (borg_trump_damage_monster(book, spell));
		}

		case REALM_ARCANE:
		{
			return (borg_arcane_damage_monster(book, spell));
		}

		default:
		{
			borg_oops("Trying to cast with an unknown realm.");
			return (0);
		}

	}
}


/* Check the spells for damage */
static int borg_attack_spell(int *b_slot, int *b_spell)
{
	int realm, book, spell;
	int k, n, b_n = 0;
	int b_x = 0, b_y = 0;
	int fail_rate = (borg_fighting_unique ? 40 : 25);
	list_item *l_ptr;

	if (borg_simulate)
	{
		/* No firing while blind, confused, or hallucinating */
		if (bp_ptr->status.blind ||
			bp_ptr->status.confused ||
			bp_ptr->status.image) return (0);

		/* Spells are not for warriors or mindcrafters */
		if (borg_class == CLASS_WARRIOR ||
			borg_class == CLASS_MINDCRAFTER) return (0);

		/* Loop through the inventory */
		for (k = 0; k < inven_num; k++)
		{
			l_ptr = &inventory[k];

			/* Stop after the books have been seen */
			if (l_ptr->tval < TV_BOOKS_MIN) break;

			/* Realize which realm this is */
			realm = l_ptr->tval - TV_BOOKS_MIN + 1;

			/* Is this a realm that the borg knows? */
			if (!borg_has_realm(realm)) continue;

			/* Realize which book this is */
			book = k_info[l_ptr->k_idx].sval;

			/* Loop through the spells */
			for (spell = 0; spell < 8; spell++)
			{
				/* Paranoia */
				if (randint0(100) < 5) continue;

				/* Require ability (right now) */
				if (!borg_spell_okay_fail(realm, book, spell, fail_rate)) continue;

				/* Choose optimal location */
				n = borg_spell_damage_monster(realm, book, spell);

				/* Penalize mana usage */
				n = n - borg_spell_mana(realm, book, spell);

				/* Compare with previous */
				if (n <= b_n) continue;

				/* Track this spell */
				b_n = n;
				*b_slot = k;
				*b_spell = spell;
				b_x = g_x;
				b_y = g_y;
			}
		}

		/* Set the global coords */
		g_x = b_x;
		g_y = b_y;

		/* Simulation */
		return (b_n);
	}

	/* Get the book */
	l_ptr = &inventory[*b_slot];

	/* Find out the realm and the book */
	realm = l_ptr->tval - TV_BOOKS_MIN + 1;
	book = k_info[l_ptr->k_idx].sval;

	/* Set the target (Okay if it is a dud target) */
	borg_target(g_x, g_y);

	/* Cast the spell */
	(void)borg_spell(realm, book, *b_spell);

	/* Set our shooting flag */
	successful_target = BORG_FRESH_TARGET;

	/* Value */
	return (b_n);
}


/* Try to use the reserve mana for attacking anyway if there is one monster */
static int borg_attack_spell_reserve(bool faint, int *b_slot, int *b_spell)
{
	int realm, book, spell = 0;

	int k, n, b_n = 0;
	int b_x = 0, b_y = 0;

	int fail_rate = (borg_fighting_unique ? 40 : 25);
	int power, monster;

	/* Fake our Mana */
	int sv_mana = bp_ptr->csp;

	bool spell_success;

	borg_kill *kill;
	list_item *l_ptr;

	if (borg_simulate)
	{
		/* Spells are not for warriors or mindcrafters */
		if (borg_class == CLASS_WARRIOR ||
			borg_class == CLASS_MINDCRAFTER) return (0);

		/* Fainting is only for little guys */
		if (faint && bp_ptr->lev >= 20) return (0);

		/* Only big guys have reserve_mana */
		if (!faint && !borg_reserve_mana()) return (0);

		/* No firing while blind, confused, or hallucinating */
		if (bp_ptr->status.blind ||
			bp_ptr->status.confused ||
			bp_ptr->status.image) return (0);

		/* There can only be one monster closeby */
		if (borg_bolt_n != 1) return (0);

		/* Must be dangerous */
		if (faint && borg_danger(c_x, c_y, 1, TRUE) < avoidance * 2) return (0);

		/* Loop through the inventory */
		for (k = 0; k < inven_num; k++)
		{
			l_ptr = &inventory[k];

			/* Stop after the books have been seen */
			if (l_ptr->tval < TV_BOOKS_MIN) break;

			/* Realize which realm this is */
			realm = l_ptr->tval - TV_BOOKS_MIN + 1;

			/* Is this a realm that the borg knows? */
			if (!borg_has_realm(realm)) continue;

			/* Realize which book this is */
			book = k_info[l_ptr->k_idx].sval;

			/* Loop through the spells */
			for (spell = 0; spell < 8; spell++)
			{
				borg_magic *as = &borg_magics[realm][book][spell];

				/* Paranoia */
				if (randint0(100) < 5) continue;

				/* There is no point trying too high level spells */
				if (as->level > bp_ptr->lev) continue;

				/* Require inability (right now) */
				if (borg_spell_okay_fail(realm, book, spell, fail_rate))
					continue;

				/* Collect the mana for this spell */
				power = borg_spell_mana(realm, book, spell);

				/* If there is enough mana then keep trying */
				if (!faint && bp_ptr->csp < power) continue;

				/* Does the lack of mana bust the failrate? */
				if (faint &&
					borg_spell_fail_rate(realm, book, spell) > fail_rate)
					continue;

				/* Pretend there is enough mana  */
				bp_ptr->csp = bp_ptr->msp;

				/* Can you cast the spell now? */
				spell_success = borg_spell_okay_fail(realm, book, spell, fail_rate);

				/* Restore original mana */
				bp_ptr->csp = sv_mana;

				/* The fail rate was too bad */
				if (!spell_success) continue;

				/* Choose optimal location */
				n = borg_spell_damage_monster(realm, book, spell);

				/* Find the index to the monster */
				monster = map_loc(borg_bolt_x[0], borg_bolt_y[0])->kill;

				/* Find the actual monster */
				kill = &borg_kills[monster];

				/* If the monster won't die of this don't bother trying */
				if (kill->power > n) continue;

				/* Compare with previous */
				if (n <= b_n) continue;

				/* Track this spell */
				b_n = n;
				*b_slot = k;
				*b_spell = spell;
				b_x = g_x;
				b_y = g_y;
			}
		}

		/* Set the global coords */
		g_x = b_x;
		g_y = b_y;

		/* Simulation */
		return (b_n);
	}

	/* Make a note */
	borg_note("Emergency spell use: %s", (faint) ? "faint" : "reserve");

	/* Get the book */
	l_ptr = &inventory[*b_slot];

	/* Find out the realm and the book */
	realm = l_ptr->tval - TV_BOOKS_MIN + 1;
	book = k_info[l_ptr->k_idx].sval;

	/* Set the target (Okay if it is a dud target) */
	borg_target(g_x, g_y);

	/* Pretend the borg has enough mana for this */
	bp_ptr->csp = bp_ptr->msp;

	/* Cast the spell */
	(void)borg_spell(realm, book, *b_spell);

	/* Close your eyes */
	if (faint)
	{
		/* confirm the spell use */
		borg_press_faint_accept();
	}

	/* Get the right amount of mana */
	bp_ptr->csp = MAX(0, sv_mana - borg_magics[realm][book][spell].power);

	/* Set our shooting flag */
	successful_target = BORG_FRESH_TARGET;

	/* Value */
	return (b_n);
}


/* Find out the damage done by certain staffs */
static int borg_staff_damage_monster(int sval)
{
	int charge_penalty = 20;
	int rad = MAX_SIGHT;
	int dam = 60;
	int typ;

	switch (sval)
	{
		case SV_STAFF_STARLITE:
		{
			/*
			 * Actually this staff picks 5d3 random targets that are in LOS
			 * and send a light beam at them.
			 * The borg assumes that two of those beams hit the best target.
			 * This can easily happen in a hallway
			 */
			return (2 * borg_launch_beam(27, GF_LITE_WEAK, MAX_RANGE));
		}

		case SV_STAFF_SLEEP_MONSTERS:
		{
			/* Set the type */
			typ = GF_OLD_SLEEP;

			break;
		}

		case SV_STAFF_SLOW_MONSTERS:
		{
			/* Set the type */
			typ = GF_OLD_SLOW;

			break;
		}

		case SV_STAFF_DISPEL_EVIL:
		{
			/* Set the type */
			typ = GF_DISP_EVIL;

			break;
		}

		case SV_STAFF_POWER:
		{
			/* Set the damage, type and penalty for using a charge */
			dam = 300;
			typ = GF_DISP_ALL;
			charge_penalty = 50;

			break;
		}

		case SV_STAFF_HOLINESS:
		{
			/* Set the damage, type and penalty for using a charge */
			dam = 300;
			typ = GF_DISP_EVIL;
			charge_penalty = 50;

			/* If you are low on HP take 200 bonus for healing */
			if (bp_ptr->chp < bp_ptr->mhp / 2) dam += 200;
			
			break;
		}
		default:
		{
			/* Any other staff doesn't do damage */
			return (0);
		}
	}

	/* Return the damage */
	return (borg_launch_dispel(dam, typ, rad) - charge_penalty);
}


/*
 *  Simulate/Apply the optimal result of using a "dispel" staff
 */
static int borg_attack_staff(int *b_slot)
{
	int i, k;
	int n, b_n = 0;
	int sval;

	if (borg_simulate)
	{
		/* No firing while blind, confused, or hallucinating */
		if (bp_ptr->status.blind || bp_ptr->status.confused ||
			bp_ptr->status.image) return (0);

		/* Paranoia */
		if (randint0(100) < 5) return (0);

		/* Go through the inventory */
		for (k = 0; k < inven_num; k++)
		{
			list_item *l_ptr = &inventory[k];

			/* look for staffs */
			if (l_ptr->tval != TV_STAFF) continue;

			/* Skip staffs that have been considered already */
			if (l_ptr->treat_as == TREAT_AS_GONE)
			{
				/* Back to normal */
				l_ptr->treat_as = TREAT_AS_NORM;
				continue;
			}

			/* Is this staff identified? */
			if (borg_obj_known_p(l_ptr))
			{
				/* Does it have charges? */
				if (!l_ptr->pval) continue;
			}
			else
			{
				/* Is it inscribed as {empty} */
				if (strstr(l_ptr->o_name, "{empty}")) continue;
			}

			/* Determine type */
			sval = k_info[l_ptr->k_idx].sval;

			/* Search the rest of the staffs for the current type */
			i = borg_slot_from(TV_STAFF, sval, k + 1);

			/* Loop through the rest of the inventory */
			while (i != -1)
			{
				/* Skip this staff in the future loops */
				inventory[i].treat_as = TREAT_AS_GONE;

				/* Search the rest of the staffs for the current type */
				i = borg_slot_from(TV_STAFF, sval, i + 1);
			}

			/* Find out the damgage it can do */
			n = borg_staff_damage_monster(sval);

			/* Is it better than before? */
			if (n <= b_n) continue;

			/* Track it */
			b_n = n;
			*b_slot = k;
		}

		/* Simulation */
		return (b_n);
	}

	/* Make a note */
	borg_note("Using a %s", inventory[*b_slot].o_name);

	/* Use the staff */
	borg_keypress('u');
	borg_keypress(I2A(*b_slot));

	/* Set our shooting flag */
	successful_target = BORG_FRESH_TARGET;

	/* Finished */
	return (0);
}


/*
 * Simulate/Apply the optimal result of using the given "type" of attack
 */
static int borg_attack_aux(int what, int *slot, int *spell)
{
	/* Analyze */
	switch (what)
	{
		case BF_ROD:
		{
			/* Any damage inducing rod */
			return (borg_attack_rod(slot));
		}

		case BF_DRAGON_ARMOUR:
		{
			/* Any damage inducing dragon armour */
			return (borg_attack_dragon());
		}

		case BF_RING:
		{
			/* Any damage inducing ring */
			return (borg_attack_ring(slot));
		}

		case BF_ARTIFACT:
		{
			/* Any damage inducing artifact */
			return (borg_attack_artifact(slot));
		}

		case BF_LAUNCH:
		{
			/* Fire something with your launcher */
			return (borg_attack_launch(slot));
		}

		case BF_OBJECT:
		{
			/* Object attack */
			return (borg_attack_object(slot, 1));
		}

		case BF_SCROLL:
		{
			/* Read some scroll that is nasty */
			return (borg_attack_scroll(slot));
		}

		case BF_THRUST:
		{
			/* Physical attack */
			return (borg_attack_thrust());
		}

		case BF_SPELLCASTER:
		{
			/* Check the spells for damage */
			return (borg_attack_spell(slot, spell));
		}

		case BF_MINDCRAFTER:
		{
			/* Check the Mindcrafter spells for damage */
			return (borg_attack_mindcrafter(spell));
		}

		case BF_STAFF:
		{
			/* Any damage inducing staff */
			return (borg_attack_staff(slot));
		}

		case BF_WAND:
		{
			/* Any damage inducing wand */
			return (borg_attack_wand(slot));
		}

		case BF_RACIAL:
		{
			/* Any damage inducing racial powers */
			return (borg_attack_racial());
		}

		case BF_MUTATE:
		{
			/* Any damage inducing mutation */
			return (borg_attack_mutation(slot, spell));
		}

		case BF_SPELL_RESERVE:
		{
			/* Check the spells again if in trouble */
			return (borg_attack_spell_reserve(FALSE, slot, spell));
		}

		case BF_MIND_RESERVE:
		{
			/* Check the Mindcrafter spells for damage */
			return (borg_attack_mindcrafter_reserve(FALSE, spell));
		}

		case BF_SPELL_FAINT:
		{
			/* Check the spells again if in trouble */
			return (borg_attack_spell_reserve(TRUE, slot, spell));
		}

		case BF_MIND_FAINT:
		{
			/* Check the Mindcrafter spells for damage */
			return (borg_attack_mindcrafter_reserve(TRUE, spell));
		}

	}

	/* report code mistake */
	borg_oops("The BF_value %d is not in the switch", what);

	/* Oops */
	return (0);
}


/* This procedure adds a grid coords to borg_ball */
static void borg_add_temp_ball(int x, int y)
{
	int k;

	/* Check the grid array */
	for (k = 0; k < borg_ball_n; k++)
		{
		/* Has this grid been used already? */
		if ((borg_ball_x[k] == x) && (borg_ball_y[k] == y))
		{
			/* Don't stick in doubles */
			return;
		}
	}

	/* Stick this grid in the temp_grid array */
	borg_ball_x[borg_ball_n] = x;
	borg_ball_y[borg_ball_n] = y;
	borg_ball_n++;
}


/* This procedure adds a grid coords to borg_beam */
static void borg_add_temp_beam(int x, int y)
{
	/* Stick this monster in the temp ball array */
	borg_beam_x[borg_beam_n] = x;
	borg_beam_y[borg_beam_n] = y;
	borg_beam_n++;
}


/* This procedure adds a grid coords to borg_bolt */
static void borg_add_temp_bolt(int x, int y)
{
	/* Stick this monster in the temp bolt array */
	borg_bolt_x[borg_bolt_n] = x;
	borg_bolt_y[borg_bolt_n] = y;
	borg_bolt_n++;
}


/* This procedure adds a grid coords to borg_bolt */
static void borg_add_temp_next(int x, int y)
{
	/* Stick this monster in the temp bolt array */
	borg_next_x[borg_next_n] = x;
	borg_next_y[borg_next_n] = y;
	borg_next_n++;
}


/*
 * This procedure fills the temp monster arrays with coords of monsters in LOS.
 * The basic idea behind these arrays is that they are all checked beforehand
 * so if a procdure wants to use them there is no need for checking for walls,
 * LOS, walls, etc.
 * 
 * borg_temp contains all the monsters within range, as in the old situation.
 * borg_bolt contains all the monsters that can be hit by a bolt.
 * borg_beam contains all the monsters than can be hit by a beam.
 * borg_ball contains all the coords where you can target a ball and hit a
 *		monster directly and also the targetable grids next to any monster.
 *
 * If the borg has ESP then this routine will deliver monsters that are not in
 * LOS, because they are hidden by walls on unknown terrain.  This is where
 * successful_target comes in.  If the borg attempted a distance attack in the
 * previous move then then suc_target is set to FALSE.  If the borg hit something
 * apparently there is no wall in the way and succ_target is set to TRUE.  Then
 * the borg can use borg_los and borg_bolt_los for this turn.  However, if the
 * borg failed to hit the target then there must be a wall in the way and the
 * borg will use borg_los_pure and borg_bolt_los_pure.
 */
void borg_temp_fill(void)
{
	int i;
	int dist;
	int x, y, dx, dy;
	int x1, y1;

	/* Avoid doing this costly procedure twice to get the same data */
	if (borg_temp_fill_valid &&
		borg_temp_fill_valid == borg_t) return;

	/* Set the counter to this turn */
	borg_temp_fill_valid = borg_t;

	/* Reset lists */
	borg_temp_n = 0;
	borg_next_n = 0;
	borg_bolt_n = 0;
	borg_beam_n = 0;
	borg_ball_n = 0;

	/* Find "nearby" monsters */
	for (i = 1; i < borg_kills_nxt; i++)
	{
		/* Get a monster */
		borg_kill *kill = &borg_kills[i];

		/* Skip dead monsters */
		if (!kill->r_idx) continue;

		/* Require current knowledge */
		if (kill->when < borg_t) continue;

		/* Ignore multiplying monsters and when fleeing from scaries */
		if (goal_ignoring && !bp_ptr->status.afraid &&
			FLAG(&r_info[kill->r_idx], RF_MULTIPLY)) continue;

		/* If it is a pet, ignore it */
		if (kill->m_flags & (MONST_FRIEND | MONST_PET)) continue;

		/* Acquire location */
		x = kill->x;
		y = kill->y;

		/* How far is this monster? */
		dist = distance(c_x, c_y, x, y);

		/* Not too far away */
		if (dist > MAX_RANGE) continue;

		/* Bounds checking */
		if (!map_in_bounds(x, y)) continue;

		/* Save the location (careful) */
		borg_temp_x[borg_temp_n] = x;
		borg_temp_y[borg_temp_n] = y;
		borg_temp_n++;

		/* Keep the coords of the monster */
		x1 = x;
		y1 = y;

		/* Is the monster next to the borg? */
		if (distance(c_x, c_y, x, y) == 1) borg_add_temp_next(x, y);

		for (dx = -1; dx <= 1; dx++)
		{
			for (dy = -1; dy <= 1; dy++)
			{
				/* Keep the coords of the target grid */
				x = x1 + dx;
				y = y1 + dy;

				/* Bounds checking */
				if (!map_in_bounds(x, y)) continue;

				/* How far is this grid */
				dist = distance(c_x, c_y, x, y);

				/* Is this grid out of range? */
				if (dist > MAX_RANGE) continue;

				/* If the borg has no ESP
				 * or the borg has ESP and has just hit his target
				 * assume there is no wall in the way
				 * OR
				 * If the borg has ESP and has just missed his target
				 * assume there is a wall in the way
				 */
				if (((!FLAG(bp_ptr, TR_TELEPATHY) ||
					(FLAG(bp_ptr, TR_TELEPATHY) && successful_target)) &&
					borg_los(c_x, c_y, x, y))
					||
					(FLAG(bp_ptr, TR_TELEPATHY) && !successful_target &&
					borg_los_pure(c_x, c_y, x, y)))
				{
					/* If it is not a wall it is OK for a ball */
					if (!borg_cave_wall_grid(map_loc(x, y)))
					{
						/* Add the coords to the ball array */
						borg_add_temp_ball(x, y);
					}

					/* is this square on the monster? */
					if (!dx && !dy)
					{
						/* Add the coords to the beam array */
						borg_add_temp_beam(x1, y1);
					}
				}

				/* Is this monster targetable without going through a monster? */
				if (!dx && !dy)
				{
					/*
					 * If the borg has no ESP or
					 * the borg has ESP and has just hit his target
					 * then assume unknown terrain is not a wall
					 * OR
					 * If the borg has ESP and has just missed his target
					 * then assume unknown terrain is a wall
					 */
					if (((!FLAG(bp_ptr, TR_TELEPATHY) ||
						(FLAG(bp_ptr, TR_TELEPATHY) && successful_target)) &&
						borg_bolt_los(c_x, c_y, x1, y1))
						||
						(FLAG(bp_ptr, TR_TELEPATHY) &&
						!successful_target &&
						borg_bolt_los_pure(c_x, c_y, x1, y1)))
					{
						/* Add the coords to the bolt array */
						borg_add_temp_bolt(x1, y1);
					}
				}
			}
		}
	}
}


/*
 * Attack nearby monsters, in the best possible way, if any.
 *
 * We consider a variety of possible attacks, including physical attacks
 * on adjacent monsters, missile attacks on nearby monsters, spell/prayer
 * attacks on nearby monsters, and wand/rod attacks on nearby monsters.
 *
 * Basically, for each of the known "types" of attack, we "simulate" the
 * "optimal" result of using that attack, and then we "apply" the "type"
 * of attack which appears to have the "optimal" result.
 *
 * When calculating the "result" of using an attack, we only consider the
 * effect of the attack on visible, on-screen, known monsters, which are
 * within 16 grids of the player.  This prevents most "spurious" attacks,
 * but we can still be fooled by situations like creeping coins which die
 * while out of sight, leaving behind a pile of coins, which we then find
 * again, and attack with distance attacks, which have no effect.  Perhaps
 * we should "expect" certain results, and take note of failure to observe
 * those effects.  XXX XXX XXX
 *
 * See above for the "semantics" of each "type" of attack.
 */
bool borg_attack(bool boosted_bravery)
{
	int n, b_n = 0;
	int b_x = 0, b_y = 0;
	int g, b_g = -1;
	int slot, b_slot = -1;
	int spell, b_spell = -1;

	/* Nobody around */
	if (!borg_kills_cnt) return (FALSE);

	/* Set the attacking flag so that danger is boosted for monsters */
	/* we want to attack first. */
	borg_attacking = TRUE;

	/* no attacking most scaryguys, try to get off the level */
	if (scaryguy_on_level)
	{
		/* probably Grip or Fang. */
		if (bp_ptr->depth <= 5 && bp_ptr->depth != 0 &&
			borg_fighting_unique)
		{
			/* Try to fight Grip and Fang. */
		}
		else if (boosted_bravery)
		{
			/* Try to fight if being Boosted */
		}
		else
		{
			/* Flee from other scary guys */
			borg_attacking = FALSE;
			return (FALSE);
		}
	}

	/* Check the surroundings for monsters */
	borg_temp_fill();

	/* Are there monsters to kill? */
	if (!borg_ball_n)
	{
		borg_attacking = FALSE;
		return (FALSE);
	}

	/* Simulate */
	borg_simulate = TRUE;

	/* Set default target */
	g_x = c_x;
	g_y = c_y;

	/* Analyze the possible attacks */
	for (g = BF_MIN; g < BF_MAX; g++)
	{
		/* Clear the parameters */
		slot = -1;
		spell = -1;

		/* Simulate */
		n = borg_attack_aux(g, &slot, &spell);

		/* Track "best" attack  <= */
		if (n <= b_n) continue;

		/* Track best */
		b_g = g;
		b_n = n;

		/* Track the globals */
		b_x = g_x;
		b_y = g_y;
		b_slot = slot;
		b_spell = spell;
	}

	/* Nothing good */
	if (b_n <= 0)
	{
		borg_attacking = FALSE;
		return (FALSE);
	}


	/* Note */
	borg_note("# Performing attack type %d with value %d.", b_g, b_n);

	/* Instantiate */
	borg_simulate = FALSE;

	/* set globals back for this attack */
	g_x = b_x;
	g_y = b_y;

	/* Instantiate */
	(void)borg_attack_aux(b_g, &b_slot, &b_spell);

	borg_attacking = FALSE;

	/* Success */
	return (TRUE);
}

#else

#ifdef MACINTOSH
static int HACK = 0;
#endif

#endif
