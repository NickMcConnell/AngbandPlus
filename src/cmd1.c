/* File: cmd1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "cmds.h"


/*
 * Determine if the player "hits" a monster.
 *
 * Note -- Always miss 5%, always hit 5%, otherwise random.
 */
bool test_hit(int chance, int ac, int vis)
{
	int k;

	/* Percentile dice */
	k = rand_int(100);

	/* Hack -- Instant miss or hit */
	if (k < 10) return (k < 5);

	/* Penalize invisible targets */
	if (!vis) chance = chance / 2;

	/* Power competes against armor */
	if ((chance > 0) && (rand_int(chance) >= (ac * 3 / 4))) return (TRUE);

	/* Assume miss */
	return (FALSE);
}



/*
 * Critical hits (from objects thrown/shot by the player)
 * Factor in item weight, total plusses, and player level.
 */
int critical_shot(int weight, int plus, int dam, int thrown, int excrit)
{
	int i, k;

	/* Extract "shot" power */
	i = (weight + ((p_ptr->to_h + plus) * 4) + (p_ptr->lev * 2));
	if (excrit > 0) i += excrit * (10 + (goodluck/3));
	else if (excrit < 0) i -= excrit * (10 + (badluck/3));
	else
	{
		i += (goodluck * 3);
		i -= (badluck * 2);
	}
	if (p_ptr->peace) i = ((i * 9) / 10);

    /* (thrown is 2 if throwing something that isn't a weapon) */
	if (thrown > 1) i = ((i * 3) / 5);
	/* (thrown is 0 if shooting or throwing a weapon meant for throwing) */
	/* critical less likely when throwing a weapon not meant for throwing */
	else if (thrown) i = ((i * 4) / 5);

	/* Critical hit */
	if (randint(5000) <= i)
	{
		k = weight + randint(500);
		if (excrit > 10) k += randint(excrit * 2);

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
int critical_norm(monster_type *m_ptr, int weight, int plus, int dam, int excrit)
{
	int i, k;
	int dstun = 0;
    monster_race *r_ptr;

	/* Extract "blow" power */
	i = weight + ((p_ptr->to_h + plus) * 5) + (p_ptr->lev * 3);
	if (weight > 150) excrit += 2;
	else if (weight > 90) excrit += 1;
	if (excrit > 0) i += excrit * (10 + (goodluck/3));
	else if (excrit < 0) i -= excrit * (10 + (badluck/3));
	else
	{
		i += (goodluck * 3);
		i -= (badluck * 2);
	}
	if (p_ptr->peace) i -= 50;

	/* Chance */
	if (randint(5000) <= i)
	{
		k = weight + randint(650);
		if (excrit > 10) k += randint(excrit * 2);

		if (k < 400)
		{
			sound(MSG_HIT_GOOD);
			msg_print("It was a good hit!");
			dam = 2 * dam + 5;
			if (randint(110) < goodluck/2 + 20) dstun = 1 + randint(2);
		}
		else if (k < 700)
		{
			sound(MSG_HIT_GREAT);
			msg_print("It was a great hit!");
			dam = 2 * dam + 10;
			if (randint(80) < goodluck/2 + 20) dstun = 2 + randint(3);
		}
		else if (k < 900)
		{
			sound(MSG_HIT_SUPERB);
			msg_print("It was a superb hit!");
			dam = 3 * dam + 15;
			if (randint(70) < goodluck/2 + 20) dstun = 3 + randint(2 + goodluck/4);
		}
		else if (k < 1300)
		{
			sound(MSG_HIT_HI_GREAT);
			msg_print("It was a *GREAT* hit!");
			dam = 3 * dam + 20;
			if (randint(40) < goodluck/2 + 20) dstun = 3 + randint(3 + goodluck/4);
		}
		else
		{
			sound(MSG_HIT_HI_SUPERB);
			msg_print("It was a *SUPERB* hit!");
			dam = ((7 * dam) / 2) + 25;
			if (randint(30) < goodluck/2 + 20) dstun = 3 + randint(3 + goodluck/3);
		}
	}
	else 
	{
		sound(MSG_HIT);
	}

	/* Make sure monster can be stunned before applying stun */
	r_ptr = &r_info[m_ptr->r_idx];
    if ((dstun) && (!(r_ptr->flags3 & RF3_NO_STUN)))
    {
		int tmp;

	    /* Get the monster name */
		char m_name[80];
		monster_desc(m_name, sizeof(m_name), m_ptr, 0);
		
		/* allow a saving throw */
		if (randint(100 + goodluck) > 96-(r_ptr->level*3)/4) dstun = 0;

        /* Get stunned */
		if ((m_ptr->stunned) && (dstun))
		{
			msg_format("%^s is more dazed.", m_name);
			tmp = m_ptr->stunned + (dstun / 3);
		}
		else if (dstun)
		{
			msg_format("%^s is dazed.", m_name);
			tmp = dstun;
		}

		/* Apply stun */
		m_ptr->stunned = (tmp < 200) ? tmp : 200;
    }

	return (dam);
}



/*
 * Extract the "total damage" from a given object hitting a given monster.
 * (apply extra damage from slays and brands)
 *
 * Note that most brands and slays are x3, except Slay Animal/evil (x2),
 * and Kill dragon/demon/undead (x5).
 */
int tot_dam_aux(object_type *o_ptr, int tdam, int strd, monster_type *m_ptr)
{
	int mult = 1;
	int multi = 0;
	int tbrand = 0;
	int otdam = tdam;
	monster_race *r_ptr;
	monster_lore *l_ptr;
	byte ftval;
    bool brandc, branda, brandf, brande, brandv, brandlite = FALSE;
    bool acoat = FALSE;
    object_type *j_ptr; /* for bows */
    object_type *lo_ptr; /* light source */

	/* Extract the flags */
	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	r_ptr = &r_info[m_ptr->r_idx];
	l_ptr = &l_list[m_ptr->r_idx];
	
	/* fake tval */
    ftval = o_ptr->tval;
    
    /* weapon brands (Cafe V.) */
    brandc = FALSE; /* cold */
    branda = FALSE; /* acid */
    brandf = FALSE; /* fire */
    brande = FALSE; /* elec */
    brandv = FALSE; /* pois (venom) */

	/* permanent brands on the object */
    if (f1 & (TR1_BRAND_COLD)) brandc = TRUE;
	if (f1 & (TR1_BRAND_ACID)) branda = TRUE;
	if (f1 & (TR1_BRAND_FIRE)) brandf = TRUE;
	if (f1 & (TR1_BRAND_ELEC)) brande = TRUE;
	if (f1 & (TR1_BRAND_POIS)) brandv = TRUE;

	/* temporary branding spells */
	if ((o_ptr->timedbrand) && (o_ptr->thisbrand))
	{
		tbrand = o_ptr->thisbrand;
		if (tbrand == 1) brandc = TRUE; /* cold */
		else if (tbrand == 2) brandf = TRUE; /* fire */
		else if (tbrand == 3) brande = TRUE; /* electric */
		else if (tbrand == 4) branda = TRUE; /* x3 acid (unused for temporary branding spells) */
		else if (tbrand == 5) brandv = TRUE; /* venom */
		else if (tbrand == 6) acoat = TRUE; /* x2 acid coat */
	}
	
	/* Thrown weapon sometimes get branded by elemental rings */
	/* no strd means it's thrown */
    if (!strd)
    {
       int ringbrand;
       int brandodd = ((p_ptr->skills[SKILL_THT] + goodluck) / 2) - 5;
       if (p_ptr->lev < 20) brandodd += 5;
		if ((f3 & (TR3_THROWN)) || (f3 & (TR3_PTHROW)))
		{
			ringbrand = thrown_brand();
			/* if ringbrand > 10 then character is wearing more than one */
			/* elemental ring so more likely to get brand from ring */
			if (ringbrand > 10) brandodd += 10;
			/* semi throwing weapons */
			if (!(f3 & (TR3_THROWN))) brandodd = brandodd/2;
			/* grenades never get branded from rings */
			if (o_ptr->tval == TV_FLASK) brandodd = 0;
			
			if (randint(100) < brandodd)
			{
				/* get brand from ring(s) */
				if ((ringbrand == 1) || ((ringbrand > 10) && (ringbrand < 20))) 
					brandc = TRUE;
				if ((ringbrand == 3) || (ringbrand == 13) || (ringbrand == 23) || 
					(ringbrand == 34) || (ringbrand == 35)) branda = TRUE;
				if ((ringbrand == 2) || (ringbrand == 12) || (ringbrand == 23) || 
					(ringbrand == 24) || (ringbrand == 25)) brandf = TRUE;
				if ((ringbrand == 4) || (ringbrand == 14) || (ringbrand == 24) || 
					(ringbrand == 34) || (ringbrand == 45)) brande = TRUE;
				if ((ringbrand == 5) || (ringbrand == 15) || (ringbrand == 25) || 
					(ringbrand == 35) || (ringbrand == 45)) brandv = TRUE;
				brandlite = TRUE; /* get any slays/brands from light also */
			}
		}
	}
	/* fired ammo (apply slays & brands from the launcher) */
	else if (p_ptr->ammo_tval == o_ptr->tval)
	{
		u32b f[4];
		/* get the launcher */
		j_ptr = &inventory[INVEN_BOW];

		/* Apply slays / brands from the shooter to the ammo */
		object_flags(j_ptr, &f[0], &f[1], &f[2], &f[3]);
		f1 |= f[0];
		f2 |= f[1];

	    if (f1 & (TR1_BRAND_COLD)) brandc = TRUE;
		if (f1 & (TR1_BRAND_ACID)) branda = TRUE;
		if (f1 & (TR1_BRAND_FIRE)) brandf = TRUE;
		if (f1 & (TR1_BRAND_ELEC)) brande = TRUE;
		if (f1 & (TR1_BRAND_POIS)) brandv = TRUE;
	}	
	/* melee (apply ring brands) */
    else /* there's an strd */
    {
        if (p_ptr->brand_cold) brandc = TRUE;
        if (p_ptr->brand_acid) branda = TRUE;
        if (p_ptr->brand_fire) brandf = TRUE;
        if (p_ptr->brand_elec) brande = TRUE;
        if (p_ptr->brand_pois) brandv = TRUE;
		brandlite = TRUE;
	}

	/* handle light sources with slays / brands */
	if (brandlite)
	{
		u32b f[4];

		/* get light source */
		lo_ptr = &inventory[INVEN_LITE];

		/* Apply slays/brands from the light source */
		object_flags(lo_ptr, &f[0], &f[1], &f[2], &f[3]);
		f1 |= f[0];
		f1 |= f[1];
	}

	/* Brand (Acid) */
/*	if (f1 & (TR1_BRAND_ACID)) */
/*	if (p_ptr->brand_acid)     */
	if (branda)
	{
		/* Notice immunity */
		if (r_ptr->flags3 & (RF3_IM_ACID))
		{
			if (m_ptr->ml)
			{
				l_ptr->flags3 |= (RF3_IM_ACID);
			}
		}
		/* Otherwise, take the damage */
		else
		{
			if (mult < 3) mult = 3;
		}
	}

	/* Acid Coating (used for ammo only) */
	/* (nothing ever has COAT_ACID anymore) */
	if ((f2 & (TR2_COAT_ACID)) || (acoat))
	{
		/* Notice immunity */
		if (r_ptr->flags3 & (RF3_IM_ACID))
		{
			if (m_ptr->ml)
			{
				l_ptr->flags3 |= (RF3_IM_ACID);
			}
		}
		/* Otherwise, take the damage */
		else
		{
			if (mult < 2) mult = 2;
		}
	}

	/* Brand (Elec) */
/* 	if (f1 & (TR1_BRAND_ELEC)) */
	if (brande)
	{
		/* Notice immunity */
		if (r_ptr->flags3 & (RF3_IM_ELEC))
		{
			if (m_ptr->ml)
			{
				l_ptr->flags3 |= (RF3_IM_ELEC);
			}
		}
		/* Otherwise, take the damage */
		else
		{
			if (mult < 3) mult = 3;
			else multi += 1;
		}
	}

	/* Brand (Fire) */
/* 	if (f1 & (TR1_BRAND_FIRE)) */
	if (brandf)
	{
		/* Notice immunity */
		if (r_ptr->flags3 & (RF3_IM_FIRE))
		{
			if (m_ptr->ml) l_ptr->flags3 |= (RF3_IM_FIRE);
		}
		if (r_ptr->flags3 & (RF3_HURT_FIRE))
		{
			multi += 1;
			if (m_ptr->ml) l_ptr->flags3 |= (RF3_HURT_FIRE);
		}
		/* Otherwise, take the damage */
		else
		{
			if (mult < 3) mult = 3;
			else multi += 1;
		}
	}

	/* Brand (Cold) */
/* 	if (f1 & (TR1_BRAND_COLD)) */
	if (brandc)
	{
		/* Notice immunity */
		if (r_ptr->flags3 & (RF3_IM_COLD))
		{
			if (m_ptr->ml) l_ptr->flags3 |= (RF3_IM_COLD);
		}
		if (r_ptr->flags3 & (RF3_HURT_COLD))
		{
			multi += 1;
			if (m_ptr->ml) l_ptr->flags3 |= (RF3_HURT_COLD);
		}
		/* Otherwise, take the damage */
		else
		{
			if (mult < 3) mult = 3;
			else multi += 1;
		}
	}

	/* Brand (Poison) */
/* 	if (f1 & (TR1_BRAND_POIS)) */
	if (brandv)
	{
		/* Notice immunity */
		if (r_ptr->flags3 & (RF3_IM_POIS))
		{
			if (m_ptr->ml)
			{
				l_ptr->flags3 |= (RF3_IM_POIS);
			}
		}
		/* poison has different effect on light fairies */
		else if ((r_ptr->d_char == 'y') && (r_ptr->flags3 & (RF3_HURT_DARK)))
		{
			if (!m_ptr->confused < 50)
			{
				if (tdam/2 > 28) m_ptr->confused += 20 + randint(29);
			    else m_ptr->confused += 10 + randint(tdam);
				multi += 1;
			}
			else
			{
				m_ptr->confused += 4;
				multi += 2;
			}
		}
		/* Otherwise, take the damage */
		else
		{
			if (mult < 3) mult = 3;
			else multi += 1;
		}
	}

	/* Slay Animal */
	if ((f2 & (TR2_SLAY_ANIMAL)) &&
	    (r_ptr->flags3 & (RF3_ANIMAL)))
	{
		if (m_ptr->ml) l_ptr->flags3 |= (RF3_ANIMAL);
		if (mult < 2) mult = 2;
	}

	/* Slay Evil */
	if ((f1 & (TR1_SLAY_EVIL)) && (m_ptr->evil))
	{
		if (m_ptr->ml)
		{
			  if (r_ptr->flags3 & (RF3_EVIL)) l_ptr->flags3 |= (RF3_EVIL);
			  else if (r_ptr->flags2 & (RF2_S_EVIL2)) l_ptr->flags2 |= (RF2_S_EVIL2);
			  else if (r_ptr->flags2 & (RF2_S_EVIL1)) l_ptr->flags2 |= (RF2_S_EVIL1);
		}
		if (mult < 2) mult = 2;
	}

	/* Silver damage (SLAY_WERE, different from slay silver) */
	if ((f1 & (TR1_SLAY_WERE)) && (r_ptr->flags3 & (RF3_HURT_SILV)))
	{
		if (m_ptr->ml)
		{
			l_ptr->flags3 |= (RF3_HURT_SILV);
			if (r_ptr->flags3 & (RF3_DEMON)) l_ptr->flags3 |= (RF3_DEMON);
		}
		if (mult < 3) mult = 3;
		/* only x2 slay that can stack with other slays/brands */
		else multi += 1;
	}

	/* Slay Undead */
	if ((f1 & (TR1_SLAY_UNDEAD)) && (r_ptr->flags3 & (RF3_UNDEAD)))
	{
		if (m_ptr->ml)
		{
			l_ptr->flags3 |= (RF3_UNDEAD);
		}
		if (mult < 3) mult = 3;
		else multi += 1;
	}

	/* Slay Demon */
	if ((f1 & (TR1_SLAY_DEMON)) && (r_ptr->flags3 & (RF3_DEMON)))
	{
		if (m_ptr->ml)
		{
			l_ptr->flags3 |= (RF3_DEMON);
		}
		if (mult < 3) mult = 3;
		else multi += 1;
	}

	/* Holy Wrath (Sanctify for battle) */
	if (p_ptr->timed[TMD_SANCTIFY])
	{
		/* maybe should also include undead */
		if (r_ptr->flags3 & (RF3_DEMON))
		{
			if (m_ptr->ml) l_ptr->flags3 |= (RF3_DEMON);
			if (mult < 3) mult = 3;
			else multi += 1;
		}
               
		if (m_ptr->evil)
		{
			if (m_ptr->ml)
			{
				if (r_ptr->flags3 & (RF3_EVIL)) l_ptr->flags3 |= (RF3_EVIL);
				else if (r_ptr->flags2 & (RF2_S_EVIL2)) l_ptr->flags2 |= (RF2_S_EVIL2);
				else if (r_ptr->flags2 & (RF2_S_EVIL1)) l_ptr->flags2 |= (RF2_S_EVIL1);
			}
		  if (mult < 2) mult = 2;
		}
	}

	/* Slay Orc */
	if ((f1 & (TR1_SLAY_ORC)) && (r_ptr->flags3 & (RF3_ORC)))
	{
		if (m_ptr->ml) l_ptr->flags3 |= (RF3_ORC);
		if (mult < 3) mult = 3;
		else multi += 1;
	}

	/* Slay Troll */
	if ((f1 & (TR1_SLAY_TROLL)) && (r_ptr->flags3 & (RF3_TROLL)))
	{
		if (m_ptr->ml) l_ptr->flags3 |= (RF3_TROLL);
		if (mult < 3) mult = 3;
		else multi += 1;
	}

	/* Slay Giant */
	if ((f1 & (TR1_SLAY_GIANT)) && (r_ptr->flags3 & (RF3_GIANT)))
	{
		if (m_ptr->ml) l_ptr->flags3 |= (RF3_GIANT);
		if (mult < 3) mult = 3;
		else multi += 1;
	}

	/* Slay Dragon */
	if ((f1 & (TR1_SLAY_DRAGON)) && (r_ptr->flags3 & (RF3_DRAGON)))
	{
		if (m_ptr->ml) l_ptr->flags3 |= (RF3_DRAGON);
		if (mult < 3) mult = 3;
	}

	/* Slay Silver */
	if ((f1 & (TR1_SLAY_SILVER)) && (r_ptr->flags3 & (RF3_SILVER)))
	{
		if (m_ptr->ml) l_ptr->flags3 |= (RF3_SILVER);
		if (mult < 3) mult = 3;
		else multi += 1;
	}

	/* Slay bug */
	if ((f1 & (TR1_SLAY_BUG)) && (r_ptr->flags3 & (RF3_BUG)))
	{
		if (m_ptr->ml) l_ptr->flags3 |= (RF3_BUG);
		if (mult < 3) mult = 3;
		else multi += 1;
	}

	/* Slay lite */
	if ((f1 & (TR1_SLAY_LITE)) && (r_ptr->flags3 & (RF3_HURT_DARK)))
	{
		if (m_ptr->ml) l_ptr->flags3 |= (RF3_HURT_DARK);
		if (mult < 3) mult = 3;
		else multi += 1;
	}

	/* Execute Dragon */
	if ((f2 & (TR2_KILL_DRAGON)) && (r_ptr->flags3 & (RF3_DRAGON)))
	{
		if (m_ptr->ml) l_ptr->flags3 |= (RF3_DRAGON);
		if (mult < 5) mult = 5;
	}

	/* Execute demon */
	if ((f2 & (TR2_KILL_DEMON)) &&  (r_ptr->flags3 & (RF3_DEMON)))
	{
		if (m_ptr->ml) l_ptr->flags3 |= (RF3_DEMON);
		if (mult < 5) mult = 5;
	}

	/* Execute undead */
	if ((f2 & (TR2_KILL_UNDEAD)) &&  (r_ptr->flags3 & (RF3_UNDEAD)))
	{
		if (m_ptr->ml) l_ptr->flags3 |= (RF3_UNDEAD);
		if (mult < 5) mult = 5;
	}

	/* SLAY_WERE less effective against silver monsters */
	if ((f1 & (TR1_SLAY_WERE)) && (r_ptr->flags3 & (RF3_SILVER)))
	{
		if (mult >= 3) mult -= 1;
		else if (mult > 1) mult = 1;
		else if (multi >= 1) multi -= 1;
	}
			
	/* light damage */
	if ((f3 & (TR3_LITE)) && (r_ptr->flags3 & (RF3_HURT_LITE)))
	{
		if (mult < 5) multi += 1;
		if (m_ptr->ml) l_ptr->flags3 |= (RF3_HURT_LITE);
	}

	/* most NONMONSTERs are like walls, so TUNNELers should damage them more */
	if ((f1 & (TR1_TUNNEL)) && (r_ptr->flags7 & (RF7_NONMONSTER)))
	{
		int digpow = o_ptr->pval;
        if ((mult < 3 + digpow) && (r_ptr->flags3 & (RF3_HURT_ROCK))) mult = 3 + digpow;
		else if (mult < 1 + digpow) mult = 1 + digpow;
		else if (digpow > 1) multi += digpow;
		else multi += 1;
	}
    /* diggers damage HURT_ROCK monsters */
	else if ((f1 & (TR1_TUNNEL)) && (r_ptr->flags3 & (RF3_HURT_ROCK)))
	{
		if (mult < 5)
		{
			if (o_ptr->pval > 1) multi += o_ptr->pval;
			else multi += 1;
			if (m_ptr->ml) l_ptr->flags3 |= (RF3_HURT_ROCK);
		}
	}
	
	/* grenades effectively have a x2 shards brand */
    if ((f4 & TR4_EXPLODE_A) && ((f3 & TR3_THROWN) || (f3 & TR3_PTHROW)))
	{
		if ((mult < 2) && (!(r_ptr->flags4 & (RF4_BR_SHAR)))) mult = 2;
		else multi += 1;
	}

#ifdef EFG
	/* EFGchange can learn awareness by throwing for damage */
 	if ((tdam * mult > 0) && (!object_aware_p(o_ptr)))
 	{
        /* DJA: prevent IDing wands/rods and stuff this way */
 	    if (o_ptr->tval < 30) object_aware(o_ptr);
    }
	/* ??? and if it does no damage, should we remember "thrown" aka "tried" ? */
#endif

    /* figure in decimal of strength bonus (melee only) */
    tdam = (((tdam * 10) + strd) * mult) / 10;

	/* slay added on to a brand or two brands/slays apply */
	/* each multi adds 25% of old damage (from before multipliers) */
	if (multi) tdam += (otdam * multi) / 4;

	/* Return the total damage */
	return (tdam);
}

/* Oops, you hit yourself.. */
int self_dam_aux(const object_type *o_ptr, int tdam)
{
	int multl = 1; /* not real multiplier */

	/* Extract the flags */
	u32b f1, f2, f3, f4;
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

    /* thrown weapons never get ringbrands when hitting yourself */
    if (f3 & (TR3_THROWN))
	{
		/* brands */
		if ((f1 & (TR1_BRAND_ACID)) && (!p_ptr->immune_acid) && (!p_ptr->resist_acid))
		{
		   if (multl < 4) multl = 4;
		}
		else if ((f1 & (TR1_BRAND_ACID)) && (!p_ptr->immune_acid))
		{
		   if (multl < 2) multl = 2; /* resistant */
		}
	
		if ((f1 & (TR1_BRAND_FIRE)) && (!p_ptr->immune_fire) && (!p_ptr->resist_fire))
		{
	       if (multl < 4) multl = 4;
	    }
		else if ((f1 & (TR1_BRAND_FIRE)) && (!p_ptr->immune_fire))
		{
	       if (multl < 2) multl = 2; /* resistant */
	    }
	
		if ((f1 & (TR1_BRAND_COLD)) && (!p_ptr->immune_cold) && (!p_ptr->resist_cold))
		{
	       if (multl < 4) multl = 4;
	    }
		else if ((f1 & (TR1_BRAND_COLD)) && (!p_ptr->immune_cold))
		{
	       if (multl < 2) multl = 2; /* resistant */
	    }
	
		if ((f1 & (TR1_BRAND_ELEC)) && (!p_ptr->immune_elec) && (!p_ptr->resist_elec))
		{
	       if (multl < 4) multl = 4;
	    }
		else if ((f1 & (TR1_BRAND_ELEC)) && (!p_ptr->immune_elec))
		{
	       if (multl < 2) multl = 2; /* resistant */
	    }
	
		if ((f1 & (TR1_BRAND_POIS)) && (!p_ptr->resist_pois) && (!p_ptr->weakresist_pois))
		{
	       if (multl < 4) multl = 4;
	    }
		else if ((f1 & (TR1_BRAND_POIS)) && (!p_ptr->resist_pois))
		{
	       if (multl < 3) multl = 3; /* weak resistance */
	    }
		else if (f1 & (TR1_BRAND_POIS))
		{
	       if (multl < 2) multl = 2; /* resistant */
	    }
	}
	else
	{
		/* brands */
		if ((p_ptr->brand_acid) && (!p_ptr->immune_acid) && (!p_ptr->resist_acid))
		{
		   if (multl < 4) multl = 4;
		}
		else if ((p_ptr->brand_acid) && (!p_ptr->immune_acid))
		{
		   if (multl < 2) multl = 2; /* resistant */
		}
	
		if ((p_ptr->brand_fire) && (!p_ptr->immune_fire) && (!p_ptr->resist_fire))
		{
	       if (multl < 4) multl = 4;
	    }
		else if ((p_ptr->brand_fire) && (!p_ptr->immune_fire))
		{
	       if (multl < 2) multl = 2; /* resistant */
	    }
	
		if ((p_ptr->brand_cold) && (!p_ptr->immune_cold) && (!p_ptr->resist_cold))
		{
	       if (multl < 4) multl = 4;
	    }
		else if ((p_ptr->brand_cold) && (!p_ptr->immune_cold))
		{
	       if (multl < 2) multl = 2; /* resistant */
	    }
	
		if ((p_ptr->brand_elec) && (!p_ptr->immune_elec) && (!p_ptr->resist_elec))
		{
	       if (multl < 4) multl = 4;
	    }
		else if ((p_ptr->brand_elec) && (!p_ptr->immune_elec))
		{
	       if (multl < 2) multl = 2; /* resistant */
	    }
	
		if ((p_ptr->brand_pois) && (!p_ptr->resist_pois) && (!p_ptr->weakresist_pois))
		{
	       if (multl < 4) multl = 4;
	    }
		else if ((p_ptr->brand_pois) && (!p_ptr->resist_pois))
		{
	       if (multl < 3) multl = 3; /* weak resistance */
	    }
		else if (p_ptr->brand_pois)
		{
	       if (multl < 2) multl = 2; /* resistant */
	    }
	}

    /* these things add to the multiplier */
    if ((cp_ptr->spell_book == TV_DARK_BOOK) && 
		((f1 & (TR1_SLAY_EVIL)) || (f3 & (TR3_GOOD_WEAP))))
    {
       multl += 1;
    }

    if ((f3 & (TR3_BAD_WEAP)) && (cp_ptr->spell_book == TV_PRAYER_BOOK) && (!o_ptr->blessed))
    {
       multl += 1;
    }

	/* racial slays */
	if ((f1 & (TR1_SLAY_ORC)) && (p_ptr->prace == 6))
    {
       multl += 1;
    }
	else if ((f1 & (TR1_SLAY_TROLL)) && (p_ptr->prace == 7))
    {
       multl += 1;
    }
	/* fairy gnome and power sprite are light fairies */
	else if ((f1 & (TR1_SLAY_LITE)) && ((p_ptr->prace == 12) || (p_ptr->prace == 15)))
    {
       multl += 1;
    }
    
	/* Return the total damage (not true multipliers when hitting yourself) */
	/* maximum possible multl is 6 (brand + alignment slay + racial slay) */
	if (multl == 2) return ((tdam * 4) / 3); /* 1.33x */
	else if (multl == 3) return ((tdam * 5) / 3); /* 1.67x */
	else if (multl == 4) return (tdam * 2); /* 2x */
	else if (multl == 5) return ((tdam * 7) / 3); /* 2.33x */
	else if (multl > 5) return ((tdam * 8) / 3); /* 2.67x */
	else return tdam; /* mult < 2 */
}



/*
 * Search for hidden things
 */
void search(void)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x, chance;
	bool cantsee;

	object_type *o_ptr;


	/* Start with base search ability */
	chance = p_ptr->skills[SKILL_SRH];

	/* figure in blindness and darkvision */
    cantsee = FALSE;
	if (p_ptr->timed[TMD_BLIND]) cantsee = TRUE;
	if (no_lite() && (!p_ptr->darkvis)) cantsee = TRUE;

	/* Penalize various conditions (don't stack halucenation with blindness) */
	if (cantsee) chance = chance / 10;
	else if (p_ptr->timed[TMD_IMAGE]) chance = chance / 4;

	if (p_ptr->timed[TMD_CONFUSED]) chance = chance / 4;
	else if (p_ptr->timed[TMD_FRENZY] || p_ptr->timed[TMD_SHERO]) chance = chance / 2;
	/* already penalized some by IMAGE, CONFUSED, FRENZY, and SHERO elseware */

	/* Search the nearby grids, which are always in bounds */
	for (y = (py - 1); y <= (py + 1); y++)
	{
		for (x = (px - 1); x <= (px + 1); x++)
		{
			/* Sometimes, notice things */
			if (rand_int(100) < chance)
			{
				/* Invisible trap */
				if (cave_feat[y][x] == FEAT_INVIS)
				{
					/* Pick a trap */
					pick_trap(y, x);

					/* Message */
					msg_print("You have found a trap.");

					/* Disturb */
					disturb(0, 0);
				}

				/* Secret door */
				if (cave_feat[y][x] == FEAT_SECRET)
				{
					/* Message */
					msg_print("You have found a secret door.");

					/* Pick a door */
					place_closed_door(y, x);

					/* Disturb */
					disturb(0, 0);
				}

				/* prevent finding objects buried in granite */
				if (cave_feat[y][x] >= FEAT_WALL_EXTRA) continue;

				/* Scan all objects in the grid */
				for (o_ptr = get_first_object(y, x); o_ptr; o_ptr = get_next_object(o_ptr))
				{
					/* Find objects buried in rubble */
					if (o_ptr->hidden)
					{
						o_ptr->hidden = 0;
						if (!squelch_hide_item(o_ptr))
						{
							msg_print("You have found an object in the rubble!");
							disturb(0, 0);

							/* Notice & redraw */
							note_spot(y, x);
							lite_spot(y, x);
						}
					}
					
					/* Skip non-chests */
					if (o_ptr->tval != TV_CHEST) continue;

					/* Skip disarmed chests */
					if (o_ptr->pval <= 0) continue;

					/* Skip non-trapped chests */
					if (!chest_traps[o_ptr->pval]) continue;

					/* Identify once */
					if (!object_known_p(o_ptr))
					{
						/* Message */
						msg_print("You have discovered a trap on the chest!");

						/* Know the trap */
						object_known(o_ptr);

						/* Notice it */
						disturb(0, 0);
					}
				}
			}
		}
	}
}


/*
 * Pickup all gold at the player's current location.
 */
static void py_pickup_gold(void)
{
    int py, px;

	s32b total_gold = 0L;
	byte *treasure;

	s16b this_o_idx, next_o_idx = 0;

	object_type *o_ptr;

	int sound_msg;

    if (spellswitch == 24)
    {
		py = p_ptr->target_row;
		px = p_ptr->target_col;
    }
    else 
    {
	    py = p_ptr->py;
	    px = p_ptr->px;
    }

	/* Allocate and wipe an array of ordinary gold objects */
	C_MAKE(treasure, SV_GOLD_MAX, byte);
	(void)C_WIPE(treasure, SV_GOLD_MAX, byte);

	/* Pick up all the ordinary gold objects */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		int gold_type;

		/* Get the object */
		o_ptr = &o_list[this_o_idx];

		/* Get the next object */
		next_o_idx = o_ptr->next_o_idx;

		/* Ignore if not legal treasure */
		if ((o_ptr->tval != TV_GOLD) ||
		    (o_ptr->sval >= SV_GOLD_MAX)) continue;

		/* Hack -- adjust treasure type (to avoid picking up "gold, gold, and gold") */
		gold_type = o_ptr->sval;
		if ((gold_type == SV_COPPER2) || (gold_type == SV_COPPER3))
			gold_type = SV_COPPER1;
		if ((gold_type == SV_SILVER2) || (gold_type == SV_SILVER3))
			gold_type = SV_SILVER1;
		if ((gold_type == SV_GOLD2)   || (gold_type == SV_GOLD3))
			gold_type = SV_GOLD1;
		if (gold_type == SV_GARNETS2)
			gold_type = SV_GARNETS1;

		/* Note that we have this kind of treasure */
		treasure[gold_type]++;

		/* Increment total value */
        if (adult_cansell)
           {
		   total_gold += (s32b)o_ptr->pval;
           }
        else        
           {
		   /* EFGchange allow larger gold values */
		   total_gold += o_ptr->number * o_ptr->pval;
           }

		/* Delete the gold */
		delete_object_idx(this_o_idx);
	}

	/* Pick up the gold, if present */
	if (total_gold)
	{
		char buf[1024];
		char tmp[80];
		int i, count, total, k_idx;

		/* Build a message */
		(void)strnfmt(buf, sizeof(buf), "You have found %ld gold pieces worth of ",  total_gold);

		/* Count the types of treasure present */
		for (total = 0, i = 0; i < SV_GOLD_MAX; i++)
		{
			if (treasure[i]) total++;
		}

		/* List the treasure types */
		for (count = 0, i = 0; i < SV_GOLD_MAX; i++)
		{
			/* Skip if no treasure of this type */
			if (!treasure[i]) continue;

			/* Get this object index */
			k_idx = lookup_kind(TV_GOLD, i);

			/* Skip past errors  XXX */
			if (k_idx <= 0) continue;

			/* Get the object name */
			object_kind_name(tmp, sizeof tmp, k_idx, TRUE);

			/* Build up the pickup string */
			my_strcat(buf, tmp, sizeof(buf));

			/* Added another kind of treasure */
			count++;

			/* Add a comma if necessary */
			if ((total > 2) && (count < total)) my_strcat(buf, ",", sizeof(buf));

			/* Add an "and" if necessary */
			if ((total >= 2) && (count == total-1)) my_strcat(buf, " and", sizeof(buf));

			/* Add a space or period if necessary */
			if (count < total) my_strcat(buf, " ", sizeof(buf));
			else               my_strcat(buf, ".", sizeof(buf));
		}

		/* Determine which sound to play */
		if      (total_gold < 200) sound_msg = MSG_MONEY1;
		else if (total_gold < 600) sound_msg = MSG_MONEY2;
		else                       sound_msg = MSG_MONEY3;

		/* Display the message */
		message(sound_msg, 0, buf);

		/* Add gold to purse */
		p_ptr->au += total_gold;

		/* Redraw gold */
		p_ptr->redraw |= (PR_GOLD);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
	}

	/* Free the gold array */
	FREE(treasure);
}


/*
 * Objects that combine with items already in the quiver get picked
 * up, placed in the quiver, and combined automatically.
 */
static bool quiver_carry(object_type *o_ptr, int o_idx)
{
	int i;

	object_type *i_ptr, *j_ptr = NULL;

	char name[80];

	/* Must be ammo. */
	if (!ammo_p(o_ptr) && !is_throwing_weapon(o_ptr)) return (FALSE);

	/* Known or sensed cursed ammo is avoided */
	if (cursed_p(o_ptr) && ((o_ptr->ident & (IDENT_SENSE)) || object_known_p(o_ptr))) return (FALSE);

	/* Check quiver space */
	if (!quiver_carry_okay(o_ptr, o_ptr->number, -1)) return (FALSE);

	/* Check quiver for similar objects. */
	for (i = INVEN_QUIVER; i < END_QUIVER; i++)
	{
		/* Get object in that slot. */
		i_ptr = &inventory[i];

		/* Ignore empty objects */
		if (!i_ptr->k_idx)
		{
			/* But save first empty slot, see later */
			if (!j_ptr) j_ptr = i_ptr;

			continue;
		}

		/* Look for similar. */
		if (object_similar(i_ptr, o_ptr))
		{

			/* Absorb floor object. */
			object_absorb(i_ptr, o_ptr);

			/* Remember this slot */
			j_ptr = i_ptr;

			/* Done */
			break;
		}
	}

	/* Can't combine the ammo. Search for the "=g" inscription */
	if (i >= END_QUIVER)
	{
		char *s;

		/* Full quiver or no inscription at all */
		if (!j_ptr || !(o_ptr->note)) return (FALSE);

		/* Search the '=' character in the inscription */
		s = strchr(quark_str(o_ptr->note), '=');

		while (TRUE)
		{
			/* We reached the end of the inscription */
			if (!s) return (FALSE);

			/* We found the "=g" inscription */
			if (s[1] == 'g')
			{
				/* Put the ammo in the empty slot */
				object_copy(j_ptr, o_ptr);

				/* Done */
				break;
			}

			/* Keep looking */
			s = strchr(s + 1, '=');
		}
	}

	/*
	 * Increase carried weight.
     * Note that o_ptr has the right number of missiles to add.
	 */
	p_ptr->total_weight += o_ptr->weight * o_ptr->number;

	/* Reorder the quiver, track the index */
	i = reorder_quiver(j_ptr - inventory);

	/* Get the final slot */
	j_ptr = &inventory[i];

	/* Cursed! */
	if (cursed_p(j_ptr))
	{
		/* Warn the player */
		sound(MSG_CURSED);
		msg_print("Oops! It feels deathly cold!");

		/* Remove special inscription, if any */
		j_ptr->pseudo = 0;

		/* Sense the object if allowed */
		if (j_ptr->pseudo == 0) j_ptr->pseudo = INSCRIP_CURSED;

		/* The object has been "sensed" */
		j_ptr->ident |= (IDENT_SENSE);
	}

	/* Describe the object */
	object_desc(name, sizeof(name), j_ptr, TRUE, 3);

	/* Message */
	msg_format("You have %s (%c).", name, index_to_label(i));

	/* Delete the object */
	delete_object_idx(o_idx);

	/* Update "p_ptr->pack_size_reduce" */
	find_quiver_size();

	/* Recalculate bonuses */
	p_ptr->update |= (PU_BONUS);

	/* Window stuff */
	p_ptr->window |= (PW_EQUIP | PW_INVEN);

	return (TRUE);
}


/*
 * Determine if the object can be picked up automatically.
 */
static bool auto_pickup_okay(const object_type *o_ptr)
{
	const char *s;

	/*** Negative checks ***/

	/* It can't be carried */
	if (!inven_carry_okay(o_ptr)) return (FALSE);

	/* never auto pickup big rocks */
	if ((o_ptr->tval == TV_SKELETON) && (o_ptr->sval == SV_BIG_ROCK)) return (FALSE);


	/*** Positive checks ***/

	/* Pickup if it matches the inventory */
	if (pickup_inven && inven_stack_okay(o_ptr)) return (TRUE);

	/* Vacuum up everything if requested */
	if (pickup_always) return (TRUE);

	/* Check inscription */
	if (o_ptr->note)
	{
		/* Find a '=' */
		s = strchr(quark_str(o_ptr->note), '=');

		/* Process permissions */
		while (s)
		{
			/* =g ('g'et) means auto pickup */
			if (s[1] == 'g') return (TRUE);

			/* Find another '=' */
			s = strchr(s + 1, '=');
		}
	}

	/* Don't auto pickup */
	return (FALSE);
}


/*
 * Carry an object and delete it.
 */
static void py_pickup_aux(int o_idx, bool msg)
{
	int slot;

	char o_name[80];
	object_type *o_ptr = &o_list[o_idx];

	/* Carry the object */
	slot = inven_carry(o_ptr);

	/* Handle errors (paranoia) */
	if (slot < 0) return;

	/* Get the new object */
	o_ptr = &inventory[slot];

#ifdef instantpseudo
	/* Sometimes do instant pseudo on pickup */
	if ((!o_ptr->hadinstant) && (p_ptr->lev >= 10) &&
		(!object_known_p(o_ptr)) &&
		(!(o_ptr->ident & IDENT_SENSE)))
	{
		int pschance = cp_ptr->sense_base / 1000;
		object_kind *k_ptr = &k_info[o_ptr->k_idx];

		/* invert to make it higher the better */
		if (pschance < 100) pschance = 101 - pschance;
		else pschance = 0;

		/* factor how easy it is to recognise the item */
		if (artifact_p(o_ptr)) pschance += 25;
		else if (obviously_excellent(o_ptr, FALSE, NULL)) pschance += 12;
		else if ((f1 & TR1_BRAND_COLD) || (f1 & TR1_BRAND_FIRE) ||
			(f1 & TR1_BRAND_ELEC) || (f1 & TR1_BRAND_ACID) ||
			(f1 & TR1_BRAND_POIS)) pschance += 10;
		else if (o_ptr->weight != k_ptr->weight) pschance += 8;
		else if (ego_item_p(o_ptr)) pschance += 6;
		
		/* factor character level */
		pschance = (pschance * p_ptr->lev) / 100;
		
		/* not having heavy pseudo is a handicap */
        if (!(cp_ptr->flags & CF_PSEUDO_ID_HEAVY)) pschance /= 2;

		/* roll for instant pseudo on pickup and maybe sense it */
        if (randint(100) < pschance) sense_one(o_ptr);
		
		/* item has had a chance at instead pseudo */
		o_ptr->hadinstant = 1;
	}
#endif

	/* Set squelch status */
	p_ptr->notice |= PN_SQUELCH;

	/* Optionally, display a message */
	if (msg)
	{
		/* Describe the object */
		object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

		/* Message */
		msg_format("You have %s (%c).", o_name, index_to_label(slot));
	}

	/* Delete the object */
	delete_object_idx(o_idx);
}

/* a version of twall */
static void norubble(int y, int x)
{
	/* only do this if this spot has rubble */
	if (cave_feat[y][x] != FEAT_RUBBLE) return;

	/* Forget the wall */
	cave_info[y][x] &= ~(CAVE_MARK);

	/* Remove the feature */
	cave_set_feat(y, x, FEAT_FLOOR);

	/* Update the visuals */
	p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);

	/* Fully update the flow */
	p_ptr->update |= (PU_FORGET_FLOW | PU_UPDATE_FLOW);
}


/*
 * Pick up objects and treasure on the floor.  -LM-
 *
 * Called with pickup:
 * 0 to act according to the player's settings
 * 1 to quickly pickup single objects and present a menu for more
 * 2 to force a menu for any number of objects
 *
 * Scan the list of objects in that floor grid.   Pick up gold automatically.
 * Pick up objects automatically until pile or backpack space is full if
 * auto-pickup option is on, carry_query_floor option is not, and menus are
 * not forced (which the "get" command does). Otherwise, store objects on
 * floor in an array, and tally both how many there are and can be picked up.
 *
 * If not picking up anything, indicate objects on the floor.  Show more
 * details if the "pickup_detail" option is set.  Do the same thing if we
 * don't have room for anything.
 *
 * If we are picking up objects automatically, and have room for at least
 * one, allow the "pickup_detail" option to display information about objects
 * and prompt the player.  Otherwise, automatically pick up a single object
 * or use a menu for more than one.
 *
 * Pick up multiple objects using Tim Baker's menu system.   Recursively
 * call this function (forcing menus for any number of objects) until
 * objects are gone, backpack is full, or player is satisfied.
 *
 * We keep track of number of objects picked up to calculate time spent.
 * This tally is incremented even for automatic pickup, so we are careful
 * (in "dungeon.c" and elsewhere) to handle pickup as either a separate
 * automated move or a no-cost part of the stay still or 'g'et command.
 *
 * Note the lack of chance for the character to be disturbed by unmarked
 * objects.  They are truly "unknown".
 */
s16b py_pickup(int pickup)
{
    int py, px;
    bool telemove = FALSE; /* for telekinesis */
	char o_name[80];

	s16b this_o_idx, next_o_idx = 0;

	object_type *o_ptr;

	/* Objects picked up.  Used to determine time cost of command. */
	byte objs_picked_up = 0;

	int floor_num = 0, floor_list[MAX_FLOOR_STACK + 1], floor_o_idx = 0;

	int can_pickup = 0;
	bool call_function_again = FALSE;

	bool blind = ((p_ptr->timed[TMD_BLIND]) || (no_lite()));
	bool msg = TRUE;

	bool auto_okay = p_ptr->auto_pickup_okay;

    /* pickup at target for telekinesis */
	if (spellswitch == 24)
    {
        py = p_ptr->target_row;
        px = p_ptr->target_col;
    }
    else
    { 
        py = p_ptr->py;
        px = p_ptr->px;
    }

	/* Reset auto_pickup_okay */
	p_ptr->auto_pickup_okay = TRUE;

	/* Nothing to pick up -- return */
	if (!cave_o_idx[py][px])
         return (0);


	/* Always pickup gold, effortlessly */
	py_pickup_gold();


	/* Scan the remaining objects */
	for (this_o_idx = cave_o_idx[py][px]; this_o_idx; this_o_idx = next_o_idx)
	{
		/* Get the object and the next object */
		o_ptr = &o_list[this_o_idx];
		next_o_idx = o_ptr->next_o_idx;

		/* Ignore all hidden objects and non-objects */
		if (squelch_hide_item(o_ptr) || !o_ptr->k_idx) continue;

		/* tekekinesis may not move special vault chests */
        if (spellswitch == 24)
		{
			if ((o_ptr->tval == TV_CHEST) && ((o_ptr->sval == SV_SP_GOLD_CHEST) ||
            (o_ptr->sval == SV_SP_SILVER_CHEST))) continue;
		}

		/* Hack -- disturb */
		disturb(0, 0);

		/* Test for quiver auto-pickup */
		if (quiver_carry(o_ptr, this_o_idx)) continue;

		/* Automatically pick up items into the backpack */
		if (auto_okay && auto_pickup_okay(o_ptr))
		{
			/* Pick up the object with message */
			py_pickup_aux(this_o_idx, TRUE);
			objs_picked_up++;

			continue;
		}

		/* Tally objects and store them in an array. */

		/* Remember this object index */
		floor_list[floor_num] = this_o_idx;

		/* Count non-gold objects that remain on the floor. */
		floor_num++;

		/* Tally objects that can be picked up.*/
		if (inven_carry_okay(o_ptr))
			can_pickup++;

		/* XXX Hack -- Enforce limit */
		if (floor_num == MAX_FLOOR_STACK) break;
	}

    /* telekinesis with a full inventory */
    if ((!can_pickup) && (floor_num) && (spellswitch == 24))
    {
        if (!get_check("Your pack is full, move an item to the floor at your feet? ")) return (0);
        else telemove = TRUE;
    }

	/* There are no objects left */
	if (!floor_num)	return objs_picked_up;

	/* Get hold of the last floor index */
	floor_o_idx = floor_list[floor_num - 1];


	/* Mention the objects if PC is not picking them up. */
	if ((pickup == 0 || !can_pickup) && (!telemove))
	{
		const char *p = "see";

		/* One object */
		if (floor_num == 1)
		{
			if (blind)            p = "feel";
			else if (!can_pickup) p = "have no room for";

			/* Get the object */
			o_ptr = &o_list[floor_o_idx];

			/* Describe the object.  Less detail if blind. */
			if (blind) object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 0);
			else       object_desc(o_name, sizeof(o_name), o_ptr, TRUE, 3);

			/* Message */
			message_flush();
			msg_format("You %s %s.", p, o_name);
		}
		else
		{
			/* Optionally, display more information about floor items */
			if (pickup_detail)
			{
				if (blind)            p = "feel something on the floor";
				else if (!can_pickup) p = "have no room for the following objects";

				/* Scan all marked objects in the grid */
				(void)scan_floor(floor_list, &floor_num, py, px, 3);

				/* Save screen */
				screen_save();

				/* Display objects on the floor */
				show_floor(floor_list, floor_num, FALSE);

				/* Display prompt */
				prt(format("You %s: ", p), 0, 0);

				/* Move cursor back to character, if needed */
				if (hilite_player) move_cursor_relative(p_ptr->py, p_ptr->px);

				/* Wait for it.  Use key as next command. */
				p_ptr->command_new = inkey();

				/* Restore screen */
				screen_load();
			}

			/* Show less detail */
			else
			{
				message_flush();

				if (!can_pickup)
					msg_print("You have no room for any of the items on the floor.");
				else
					msg_format("You %s a pile of %d items.", (blind ? "feel" : "see"), floor_num);
			}
		}

		/* Done */
		return (objs_picked_up);
	}


	/* We can pick up objects.  Menus are not requested (yet). */
	if (pickup == 1)
	{
		/* Scan floor (again) */
		(void)scan_floor(floor_list, &floor_num, py, px, 3);

		/* Use a menu interface for multiple objects, or pickup single objects */
		if (floor_num > 1)
			pickup = 2;
		else
			this_o_idx = floor_o_idx;
	}


	/* Display a list if requested. */
	if (pickup == 2)
	{
		cptr q, s;
		int item;

		/* Restrict the choices */
		if (!telemove) item_tester_hook = inven_carry_okay;

		/* Request a list */
		p_ptr->command_see = TRUE;

		/* Get an object or exit. */
		q = "Get which item?";
		if (telemove) q = "Move which item?";
		s = "You see nothing there.";
		if (!get_item(&item, q, s, USE_FLOOR))
			return (objs_picked_up);
		
		this_o_idx = 0 - item;
		if (!(spellswitch == 24)) call_function_again = TRUE;

		/* With a list, we do not need explicit pickup messages */
		msg = FALSE;
	}

	/* Pick up object, if legal */
	if (this_o_idx)
	{
		int y, x;
		o_ptr = &o_list[this_o_idx];

		/* You just picked up some rubble, so remove the feature */
		if ((o_ptr->tval == TV_SKELETON) && (o_ptr->sval == SV_BIG_ROCK))
		{
			/* Location of rock */
			y = o_ptr->iy;
			x = o_ptr->ix;

			norubble(y, x);
		}

        /* telekinesis with a full inventory */
		if (telemove)
		{
             /* move the object to the PC's space */
             if (!floor_carry(p_ptr->py, p_ptr->px, o_ptr))
             {
                 msg_print("There's no space on the floor to put it!");
                 return (0);
             }
             else
             {
                 /* if we moved the object, make sure it's not in two places at once */
                 delete_object_idx(this_o_idx); /* (floor_carry makes a new object) */
             }
             /* don't use drop_near because that can make the object dissapear */
             /* If something is valuable enough to use telekinesis to get it, */
             /* you don't want it to dissapear. */
        }
		/* Pick up the object */
		else
        {
             py_pickup_aux(this_o_idx, msg);
        }

		/* Indicate an object picked up. */
		objs_picked_up = 1;
	}

	/*
	 * If requested, call this function recursively.  Count objects picked
	 * up.  Force the display of a menu in all cases.
	 */
	if (call_function_again) objs_picked_up += py_pickup(2);

	/* Indicate how many objects have been picked up. */
	return (objs_picked_up);
}



/*
 * Determine if a trap affects the player.
 * Always miss 5% of the time, Always hit 5% of the time.
 * Otherwise, match trap power against player armor.
 */
static bool check_hit(int power)
{
    /* DJA: Dex bonus to ac counts extra against traps */
	int pac = p_ptr->ac + p_ptr->to_a + ((int)(adj_dex_ta[p_ptr->stat_ind[A_DEX]]) - 128);
    return test_hit(power, pac, TRUE);
}


/*
 * Handle player hitting a real trap
 */
void hit_trap(int y, int x)
{
	int i, num, dam;

	cptr name = "a trap";

	int traphit;
	if (p_ptr->depth > 55) traphit = 164 + ((p_ptr->depth-54)*2) + badluck;
    else if (p_ptr->depth > 15) traphit = 110 + p_ptr->depth;
    else if (p_ptr->depth < 3) traphit = 110 + (p_ptr->depth*5);
    else traphit = 125;

	/* Disturb the player */
	disturb(0, 0);

	/* Analyze XXX XXX XXX */
	switch (cave_feat[y][x])
	{
		case FEAT_TRAP_HEAD + 0x00:
		{
			if ((p_ptr->ffall) && (randint(100) < (goodluck+3) * 4))
			{
                msg_print("You almost fall through a trap door..");
				msg_print("You catch hold of the edge of the trap door!");
                if (get_check("Fall down to next level?"))
                {
				   msg_print("You float gently down to the next level.");
                }
                else
                {
				   msg_print("You pull yourself back up.");
			       break;
                }
			}
			else if (p_ptr->ffall)
			{
                msg_print("You fall through a trap door!");
				msg_print("You float gently down to the next level.");
			}
			else
			{
                msg_print("You fall through a trap door!");
				dam = damroll(2, 8);
				take_hit(dam, name);
			}

			/* New depth */
			p_ptr->depth++;

			/* Leaving */
			p_ptr->leaving = TRUE;

			break;
		}

		case FEAT_TRAP_HEAD + 0x01:
		{
			/* earthquake trap */
			if (p_ptr->depth > 65)
			{
				msg_print("You trigger an earthquake trap!");
				/* mode 4 damages the PC's square even if it's the centre square */
                if (p_ptr->depth > 87) earthquake(p_ptr->py, p_ptr->px, 6, 75, 4, FALSE);
                /* mode 1 reduces the maximum damage that a quake can cause */
                /* (next to the PC so that it'll have a chance of hurting the PC) */
				else earthquake(p_ptr->py, p_ptr->px+1, 6, 80, 1, FALSE);
			}
			else /* pit */
			{
				msg_print("You fall into a pit!");
				if (p_ptr->ffall)
				{
					msg_print("You float gently to the bottom of the pit.");
				}
				else
				{
					dam = damroll(2, 6);
					take_hit(dam, name);
				}
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x02:
		{
			msg_print("You fall into a spiked pit!");

			if (p_ptr->ffall)
			{
				msg_print("You float gently to the floor of the pit.");
				msg_print("You carefully avoid touching the spikes.");
			}

			else
			{
				/* Base damage */
				dam = damroll(2, 6);

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					msg_print("You are impaled!");

					dam = dam * 2;
					(void)inc_timed(TMD_CUT, randint(dam));
				}

				/* Take the damage */
				take_hit(dam, name);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x03:
		{
			msg_print("You fall into a spiked pit!");

			if (p_ptr->ffall)
			{
				msg_print("You float gently to the floor of the pit.");
				msg_print("You carefully avoid touching the spikes.");
			}

			else
			{
				/* Base damage */
				dam = damroll(2, 6);

				/* Extra spike damage */
				if (rand_int(100) < 50)
				{
					msg_print("You are impaled on poisonous spikes!");

					dam = dam * 2;
					(void)inc_timed(TMD_CUT, randint(dam));

					if (p_ptr->resist_pois || p_ptr->timed[TMD_OPP_POIS])
					{
						msg_print("The poison does not affect you!");
					}
					else if (p_ptr->weakresist_pois)
					{
						msg_print("You partially resist the poison.");
                        dam = (dam * 4) / 3;
						(void)inc_timed(TMD_POISONED, randint(dam));
                    }
					else
					{
						dam = dam * 2;
						(void)inc_timed(TMD_POISONED, randint(dam));
					}
				}

				/* Take the damage */
				take_hit(dam, name);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x04: /* summoning rune */
		{
			sound(MSG_SUM_MONSTER);
			msg_print("You are enveloped in a cloud of smoke!");
			cave_info[y][x] &= ~(CAVE_MARK);
			cave_set_feat(y, x, FEAT_FLOOR);
			if (p_ptr->depth < 8) num = 2 + randint(2);
			else num = 2 + randint(3);
			if (randint(40) < (goodluck+2)*2) num -= 1;
			for (i = 0; i < num; i++)
			{
				(void)summon_specific(y, x, p_ptr->depth, 0);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x05:
		{
			bool controlled;
			int resistcrtl;
			msg_print("You hit a teleport trap!");
			controlled = FALSE;
			resistcrtl = (p_ptr->depth * 3) / 2;
            /* controlled teleport (random if you don't chose a target) */
		    if (p_ptr->telecontrol)
		    {
                if (control_tport(resistcrtl, 150)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(100);
			break;
		}

		case FEAT_TRAP_HEAD + 0x06:
		{
			msg_print("You are enveloped in flames!");
            if (p_ptr->depth > 80) dam = damroll(4, 9);
            else if (p_ptr->depth > 56) dam = damroll(4, 8);
            else if (p_ptr->depth > 28) dam = damroll(4, 7);
            else if (p_ptr->depth < 4) dam = damroll(4, 4);
			else dam = damroll(4, 6);
			fire_dam(dam, "a fire trap");
			break;
		}

		case FEAT_TRAP_HEAD + 0x07:
		{
			msg_print("You are splashed with acid!");
            if (p_ptr->depth > 81) dam = damroll(4, 9);
            else if (p_ptr->depth > 58) dam = damroll(4, 8);
            else if (p_ptr->depth > 29) dam = damroll(4, 7);
			else dam = damroll(4, 6);
			acid_dam(dam, "an acid trap");
			break;
		}

		case FEAT_TRAP_HEAD + 0x08:
		{
			if (check_hit(traphit))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, (4 + p_ptr->depth/20));
				take_hit(dam, name);
				/* (bypasses free action so it stays effective) */
				(void)inc_timed(TMD_SLOW, rand_int(20) + 20);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x09:
		{
			if (check_hit(traphit))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, (4 + p_ptr->depth/20));
				take_hit(dam, name);
				(void)do_dec_stat(A_STR, 0);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0A:
		{
			if (check_hit(traphit))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, (4 + p_ptr->depth/20));
				take_hit(dam, name);
				(void)do_dec_stat(A_DEX, 0);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0B:
		{
			if (check_hit(traphit))
			{
				msg_print("A small dart hits you!");
				dam = damroll(1, (4 + p_ptr->depth/20));
				take_hit(dam, name);
				(void)do_dec_stat(A_CON, 0);
			}
			else
			{
				msg_print("A small dart barely misses you.");
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0C:
		{
			msg_print("You are surrounded by a black gas!");
			if (!p_ptr->resist_blind)
			{
				if (p_ptr->depth < 2) (void)inc_timed(TMD_BLIND, rand_int(25) + 10);
                else (void)inc_timed(TMD_BLIND, rand_int(50) + 25);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0D:
		{
			msg_print("You are surrounded by a gas of scintillating colors!");
			if (!p_ptr->resist_confu)
			{
				if (p_ptr->depth < 2) (void)inc_timed(TMD_CONFUSED, rand_int(16) + 8);
				else (void)inc_timed(TMD_CONFUSED, rand_int(20) + 10);
			}
			break;
		}

		case FEAT_TRAP_HEAD + 0x0E:
		{
            /* chance of silver trap instead of poison */
			if ((p_ptr->depth > 55) && (randint(100) < (p_ptr->depth/3)+5))
			{
			    int save = p_ptr->skills[SKILL_SAV];
				msg_print("You are surrounded by a strange silver gas!");
			    if (p_ptr->resist_charm) save += 10;
                if (rand_int(100 + (p_ptr->depth / 4) + badluck/2) < save)
                {
                   msg_print("You resist the effects!");
                }
                else
                {
                    if (rand_int(101 + badluck/2) > save)
                    {
                       (void)inc_timed(TMD_AMNESIA, rand_int(20) + 20);
                    }
					/* silver poison */
					p_ptr->silver += randint((p_ptr->depth / 33) + 1);

					msg_print("you feel silver magic corrupting your mind!");
					/* notice it */
					p_ptr->redraw |= (PR_SILVER);
                }
            }
            else
            {
			    msg_print("You are surrounded by a pungent green gas!");
			    if (!p_ptr->resist_pois && !p_ptr->timed[TMD_OPP_POIS])
			    {
                   if (p_ptr->depth < 2) (void)inc_timed(TMD_POISONED, rand_int(14) + 6);
                   else if (p_ptr->depth > 21) (void)inc_timed(TMD_POISONED, rand_int(10 + p_ptr->depth/2) + 11);
                   else (void)inc_timed(TMD_POISONED, rand_int(20) + 10);
			    }
            }
		    break;
		}

		case FEAT_TRAP_HEAD + 0x0F:
		{
            /* chance of hallucenation trap instead of paralysis */
			/* (everyone has free action by dl50) */
			if ((p_ptr->depth >= 55) && (randint(100) < p_ptr->depth/2+15))
			{
                msg_print("You are surrounded by a purple haze!");
                if ((p_ptr->resist_chaos) || (p_ptr->timed[TMD_TSIGHT]) ||
                   (rand_int(101 + badluck/2) < p_ptr->skills[SKILL_SAV]))
                {
                   msg_print("You resist the effects!");
                }
                else
                {
                   (void)inc_timed(TMD_IMAGE, rand_int(15) + 15);
			    }
            }
            else
            {
                msg_print("You are surrounded by a strange white mist!");
			    if (!p_ptr->free_act)
			    {
				   if (p_ptr->depth < 2) (void)inc_timed(TMD_PARALYZED, rand_int(8) + 3);
                   else (void)inc_timed(TMD_PARALYZED, rand_int(10) + 5);
			    }
            }
		    break;
		}
	}
}

/* accidently hit yourself with a "DANGER"ous weapon */
void py_attackself(const object_type *o_ptr)
{
     int ouch;

     ouch = damroll(o_ptr->dd, o_ptr->ds);
     ouch = self_dam_aux(o_ptr, ouch);

     /* object damage bonus after semi-multipliers */
     if (o_ptr->k_idx) ouch += o_ptr->to_d;
     /* not all damage bonuses apply */

     /* Confusion attack */
     if ((p_ptr->confusing) && (randint(45) < (badluck+20)))
     {
        /* Cancel glowing hands */
		if (randint(45) < (badluck+20)) p_ptr->confusing = FALSE;

        /* umber hulk always has glowing hands */
	    if (cp_ptr->flags & CF_HULK_CONF) p_ptr->confusing = TRUE;

		/* Message */
		if (p_ptr->confusing == FALSE)
        {
           msg_print("Your hands stop glowing.");
        }

		if (!p_ptr->resist_confu)
		{
		   inc_timed(TMD_CONFUSED, randint(badluck+2) + (ouch/3));
		}
     }

     take_hit(ouch, "your own weapon");
}

/*
 * Attack the monster at the given location
 *
 * If no "weapon" is available, then "punch" the monster one time.
 *
 * DJA: Would not be too hard to add duel-wielding
 * if INVEN_ARM is a weapon then on last blow change o_ptr to INVEN_ARM
 * would have to include more stuff within the "once for each blow" WHILE loop
 * (WHILE loop would start at "get the weapon")
 * The question is how to balance it..
 */
void py_attack(int y, int x)
{
	int num = 0, k, bonus, chance;
	int hit = 0;
	int estl, excrit, blindfight;
	bool monhigher = FALSE;
	bool pchigher = FALSE;

	monster_type *m_ptr;
	monster_race *r_ptr;
	monster_lore *l_ptr;

	object_type *o_ptr;
	u32b f1, f2, f3, f4;

	char m_name[80];

	bool fear = FALSE;
	bool do_quake = FALSE;

	/* Get the monster */
	m_ptr = &mon_list[cave_m_idx[y][x]];
	r_ptr = &r_info[m_ptr->r_idx];
	l_ptr = &l_list[m_ptr->r_idx];

	/* Disturb the player */
	disturb(0, 0);


	/* Extract monster name (or "it") */
	monster_desc(m_name, sizeof(m_name), m_ptr, 0);

	/* Auto-Recall if possible and visible */
	if (m_ptr->ml) monster_race_track(m_ptr->r_idx);

	/* Track a new monster */
	if (m_ptr->ml) health_track(cave_m_idx[y][x]);


	/* Handle player fear / charm (doesn't apply to NONMONSTERs) */
	if (((p_ptr->timed[TMD_AFRAID]) || (p_ptr->timed[TMD_TERROR]) || (p_ptr->timed[TMD_CHARM])) &&
		(!(r_ptr->flags7 & (RF7_NONMONSTER))))
	{
		/* Message */
	    if (p_ptr->timed[TMD_CHARM])
	    {
           msg_format("You are too good a mood to try to hurt %s!", m_name);
        }
		else msg_format("You are too afraid to attack %s!", m_name);

		/* Done */
		return;
	}

	/* Get the weapon */
	o_ptr = &inventory[INVEN_WIELD];
	
	/* Calculate the "attack quality" */
	bonus = p_ptr->to_h + o_ptr->to_h;
	chance = (p_ptr->skills[SKILL_THN] + (bonus * BTH_PLUS_ADJ));

	/* hard to hit what you can't see */
	if (p_ptr->skills[SKILL_FOS] >= 45) blindfight = 0;
	else blindfight = 45 - p_ptr->skills[SKILL_FOS];

	if ((!m_ptr->ml) && (m_ptr->csleep)) chance -= blindfight;
	/* especially if it is aware of you */
	else if (!m_ptr->ml) chance = ((chance * 4) / 5) - blindfight;
	
	/** easier to hit if it hasn't noticed you **/
	/* assassin bonus */
	if ((m_ptr->csleep) && (cp_ptr->flags & CF_ASSASSIN)) chance += 16;
	/* asleep */
	else if ((m_ptr->csleep) && (!m_ptr->roaming)) chance += 12;
	/* awake but hasn't noticed you */
	else if (m_ptr->csleep) chance += 2;
	/* to balance: a little penalty if monster is aware of you */
	else if (chance > 8) chance -= 3;
	else if (chance > 6) chance = 6;

	/* higher ground has the advantage */
	if ((cave_feat[p_ptr->py][p_ptr->px] == FEAT_OPEN_PIT) &&
		(cave_feat[y][x] != FEAT_OPEN_PIT))
		monhigher = TRUE;
	if ((cave_feat[y][x] == FEAT_OPEN_PIT) &&
		(cave_feat[p_ptr->py][p_ptr->px] != FEAT_OPEN_PIT))
		pchigher = TRUE;
	if ((cave_feat[y][x] == FEAT_RUBBLE) &&
		(cave_feat[p_ptr->py][p_ptr->px] != FEAT_RUBBLE))
		monhigher = TRUE;
	if ((cave_feat[p_ptr->py][p_ptr->px] == FEAT_RUBBLE) &&
		(cave_feat[y][x] != FEAT_RUBBLE))
		pchigher = TRUE;

	if (r_ptr->flags2 & (RF2_FLY)) pchigher = FALSE;
	/* power sprites can fly */
	if (p_ptr->prace == 15) monhigher = FALSE;

	/* hard to miss a tree.. */
	if (r_ptr->flags7 & (RF7_NONMONSTER)) chance += 20;
	else if (monhigher) chance -= 8;
	else if (pchigher) chance += 8;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);
	
	/* crc == weapon crit chance:  5 = +0 */
	if (o_ptr->crc < 5) excrit = o_ptr->crc - 6; /* (negative) */
	else excrit = o_ptr->crc - 5;
	if (f2 & TR2_EXTRA_CRIT) excrit += 14; /* was 11 */
	if (bonus-1 > 10) excrit += (bonus-1)/10;

	if ((m_ptr->csleep) && (!m_ptr->roaming) && (cp_ptr->flags & CF_ASSASSIN)) 
		excrit += 7;
	/* asleep */
	else if ((m_ptr->csleep) && (!m_ptr->roaming)) excrit += 4;
	else if ((m_ptr->csleep) && (cp_ptr->flags & CF_ASSASSIN)) excrit += 3;
	
	/* Attack once for each legal blow */
	while (num++ < p_ptr->num_blow)
	{
		/* Test for hit */
		if (test_hit(chance, r_ptr->ac, m_ptr->ml))
		{
			/* monsters can't hide easily when you're attacking it */
            if (m_ptr->monseen < 10) m_ptr->monseen += 2;

			/* Message */
			message_format(MSG_GENERIC, m_ptr->r_idx, "You hit %s.", m_name);
			
			/* if you hit a charmed animal, the sphere of charm dissapears */
			if (m_ptr->charmed)
			{
               (void)clear_timed(TMD_SPHERE_CHARM);
            }

			/* Hack -- bare hands do one damage */
			k = 1;
			
            /* barbarians and hulks to more damage with bare hands */
			if ((!o_ptr->k_idx) && (cp_ptr->flags & CF_HEAVY_BONUS))
			{
				if (p_ptr->lev >= 5) k += 1 + randint(p_ptr->lev/5);
                excrit += 2;
			}

			/* Handle normal weapon */
			if (o_ptr->k_idx)
			{
				int strb, strdec, nocritk;
				bool doublehit = FALSE;
				k = damroll(o_ptr->dd, o_ptr->ds);

				/* double weapons (should it be 2,4 & 6 or just 2?) */
				if ((o_ptr->sbdd) && ((num == 2) || (num == 4) || (num == 6)))
				{
					k = damroll(o_ptr->sbdd, o_ptr->sbds);
					doublehit = TRUE;
				}
				
				/* complex strength bonus by weight */
                strb = 10 * ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
				if ((o_ptr->weight / 10) < 2) strb = strb / 6;
				else if ((o_ptr->weight / 10) < 3) strb = strb / 4;
				else if ((o_ptr->weight / 10) < 4) strb = strb / 2;
				else if ((o_ptr->weight / 10) < 5) strb = (strb * 2) / 3;
				else if ((o_ptr->weight / 10) < 6) strb = (strb * 3) / 4;
				else if ((o_ptr->weight / 10) < 7) strb = (strb * 5) / 6;
				if (cp_ptr->flags & CF_HEAVY_BONUS) /* barbarians like heavy weapons */
				{
					if ((o_ptr->weight / 10) > 26) strb = (strb * 13) / 6;
					else if ((o_ptr->weight / 10) > 20) strb = strb * 2;
					else if ((o_ptr->weight / 10) > 17) strb = (strb * 7) / 4;
					else if ((o_ptr->weight / 10) > 15) strb = (strb * 3) / 2;
					else if ((o_ptr->weight / 10) > 12) strb = (strb * 4) / 3;
					else if ((o_ptr->weight / 10) > 10) strb = (strb * 5) / 4;
					else if ((o_ptr->weight / 10) == 10) strb = (strb * 6) / 5;
					if ((o_ptr->weight / 10) > 10) strb += 1;
				}
				else
				{
					if ((o_ptr->weight / 10) > 25) strb = strb * 2;
					else if ((o_ptr->weight / 10) > 21) strb = (strb * 7) / 4;
					else if ((o_ptr->weight / 10) > 17) strb = (strb * 3) / 2;
					else if ((o_ptr->weight / 10) > 15) strb = (strb * 4) / 3;
					else if ((o_ptr->weight / 10) > 12) strb = (strb * 5) / 4;
					else if ((o_ptr->weight / 10) > 10) strb = (strb * 6) / 5;
				}
				strdec = (strb / 10);
				if (doublehit) k += strdec/2;
				else k += strdec;
				/* decimal */
				strb -= strdec * 10;
				/* do not pass 0 to tot_dam_aux */
				if (strb < 1) strb = 1;
#if oldbreak
                /* (done away with the breakpoints) */
                /* DJA: add strength bonus to damage before multipliers */
                /* (no strength bonus for very light weapons) */
	            if ((o_ptr->weight / 10) > 4)
	            {
                   k += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
                   
                   /* double strength bonus for very heavy weapons (heavier than 15 lb) */
                   if ((o_ptr->weight / 10) > 15) k += ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
                }
#endif

				k = tot_dam_aux(o_ptr, k, strb, m_ptr);
				nocritk = k;
				k = critical_norm(m_ptr, o_ptr->weight, o_ptr->to_h, k, excrit);
				/* earthquake blows */
				if (p_ptr->impact && (!do_quake))
				{
					/* at least 48 damage or crit */
					int quk, qrad;
					if (nocritk > 59) quk = 3 + ((nocritk-48)/12);
					else if (k > 47) quk = 3;
					/* very common on critical hit, rarely otherwise */
					if (k > nocritk) quk += 72 + ((k-nocritk)/10);
					/* roll for quake */
					if (rand_int(100) < quk)
					{
						do_quake = TRUE;
						if ((k > nocritk) && (k > 40)) qrad = 7 + ((k-5)/nocritk);
						else qrad = 6 + k/50;
						earthquake(p_ptr->py, p_ptr->px, qrad, 0, 0, FALSE);
					}
				}
			}

			/* spirit of the balrog (after multipliers) */
			if (p_ptr->timed[TMD_BALROG])
			{
				/* Notice immunity */
				if (r_ptr->flags3 & (RF3_IM_FIRE))
				{
					if (m_ptr->ml)
					{
						l_ptr->flags3 |= (RF3_IM_FIRE);
						msg_format("%^s resists your fire.", m_name);
					}
				}
				/* Otherwise, take fire damage */
				else
				{
                    k += (k/4) + randint(k/3);
				}
				/* Demons & Undead don't take dark damage */
			    if ((r_ptr->flags3 & (RF3_UNDEAD)) || (r_ptr->flags3 & (RF3_DEMON)))
				{
					if (m_ptr->ml)
					{
				       if (r_ptr->flags3 & (RF3_DEMON)) l_ptr->flags3 |= (RF3_DEMON);
				       if (r_ptr->flags3 & (RF3_UNDEAD)) l_ptr->flags3 |= (RF3_UNDEAD);
				       msg_format("%^s is immune to your darkness.", m_name);
					}
				}
				/* Otherwise, take darkness damage */
				else if (r_ptr->flags4 & (RF4_BR_DARK))
				{
                    k += (k/5) + randint(k/4);
                    msg_format("%^s resists your darkness.", m_name);
                }
				else if (r_ptr->flags3 & (RF3_HURT_DARK))
				{
                    if (m_ptr->ml) l_ptr->flags3 |= (RF3_HURT_DARK);
                    k += (k/3) + randint(k/2);
                    msg_format("%^s cringes.", m_name);
                }
				else
				{
                    k += (k/4) + randint(k/3);
				}
			}
            
			/* assassin bonus against sleeping monsters */
            if ((m_ptr->csleep) && (cp_ptr->flags & CF_ASSASSIN))
            {
               if (k/5 > 1) k += (k/5) + randint(k/2);
   			   else k += 1 + randint(k/2);
            }

            /* object damage bonus after multipliers */
			if (o_ptr->k_idx) 
			{
				k += o_ptr->to_d;
			}

			/* Apply the player damage bonuses */
			k += p_ptr->to_d;

            /* remove strength bonus */
/* (because I added it separatly from other to-dam bonuses before multipliers) */
        	if (o_ptr->k_idx)
            {
               k -= ((int)(adj_str_td[p_ptr->stat_ind[A_STR]]) - 128);
            }

			/* No negative damage */
			if (k < 0) k = 0;

			/* Complex message */
			if ((p_ptr->wizard) || (p_ptr->noscore & NOSCORE_DEBUG))
			{
                msg_format("You do %d (out of %d) damage.", k, m_ptr->hp);
			}

			/* Damage, check for fear and death */
			if (mon_take_hit(cave_m_idx[y][x], k, &fear, NULL)) break;

			/* cancel truce (only if still alive) */
			if (m_ptr->truce)
			{
				m_ptr->truce = 0;
				msg_format("%^s cancells your truce.", m_name);
			}
			
			/* Remember that you hit even if you didn't do damage */
			if (k == 0) hit = 1;
			else hit = 2;

			/* Confusion attack */
			if (p_ptr->confusing)
			{
                /* Cancel glowing hands */
				p_ptr->confusing = FALSE;

                /* umber hulk always has glowing hands */
	            if (cp_ptr->flags & CF_HULK_CONF)
                {
       	           p_ptr->confusing = TRUE;
                }

				/* Message */
				if (!p_ptr->confusing)
                {
                    msg_print("Your hands stop glowing.");
                }

				/* Confuse the monster */
				if (r_ptr->flags3 & (RF3_NO_CONF))
				{
					if (m_ptr->ml)
					{
						l_ptr->flags3 |= (RF3_NO_CONF);
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
					m_ptr->confused += 10 + rand_int(p_ptr->lev) / 5;
				}
			}

            /* damage to self */
            /* much less likely to happen if you hit the monster */
            if (p_ptr->accident)
            {
               int badchance = 170 - (badluck * 3);
               /* levels of frenzy effect */
               if (p_ptr->timed[TMD_FRENZY] > 19) badchance -= 30;
               else if (p_ptr->timed[TMD_FRENZY] > 9) badchance -= 15;
               
               if (goodluck > 10) badchance += (goodluck * 3);
               if (randint(badchance) < 5)
               {
                  int breakprot = 67;
				  if (artifact_p(o_ptr)) breakprot -= a_info[o_ptr->name1].level;
				  if ((p_ptr->timed[TMD_PROTEVIL]) && (f3 & TR3_BAD_WEAP) && (randint(100) < breakprot))
				  {
					  msg_format("Your own weapon tries to attacks you but is repelled!");
				  }
				  else
				  {
					  msg_format("Your own weapon attacks you!");
	                  py_attackself(o_ptr);
				  }
               }
            }

		}

		/* Player misses */
		else
		{
			char m_poss[80];
			/* Get the monster possessive ("his"/"her"/"its") */
			monster_desc(m_poss, sizeof(m_poss), m_ptr, 0x22);
			
			/* choose which message to use */
			if ((r_ptr->flags7 & (RF7_NONMONSTER)) || 
				((strchr("#E", r_ptr->d_char)) && (randint(100) < 70 + chance/5)) || 
				((r_ptr->ac >= 50) && (randint(100) < 15 + chance/5 + r_ptr->ac/10) &&
				(strchr("%$DJPXacglnq", r_ptr->d_char))))
			{
				/* alternate message: some monsters are hard to miss (#E) */
				message_format(MSG_MISS, m_ptr->r_idx, "You blow glances harmlessly off of %s.", m_name);
			}
			else if ((r_ptr->ac >= 45) && (strchr("KOTUop,", r_ptr->d_char)) && 
				(chance > 45) && (randint(100) < 15 + chance/5 + r_ptr->ac/10))
			{
				/* alternate message: some monsters have heavy armor */
				message_format(MSG_MISS, m_ptr->r_idx, "You blow glances harmlessly off %s armor.", m_poss);
			}
            else
			{
				/* normal miss message */
				message_format(MSG_MISS, m_ptr->r_idx, "You miss %s.", m_name);
			}

			/* diturb sleeping monster */
			if (m_ptr->csleep)
			{			
				estl = 10 - p_ptr->skills[SKILL_STL];
				if (randint(50) < 15 + goodluck) estl = 0;
				if (estl > m_ptr->csleep)
				{
					m_ptr->roaming = 0;
					m_ptr->csleep = 0;
				}
				else if (estl > 0)
				{
					m_ptr->csleep -= estl;
				}
				else if ((goodluck < 4) && (m_ptr->csleep > 1))
				{
					m_ptr->csleep -= 1;
				}
			}

            /* damage to self */
            if (p_ptr->accident)
            {
               int badchance = 170 - (badluck * 3);
               /* levels of frenzy effect */
               if (p_ptr->timed[TMD_FRENZY] > 19) badchance -= 30;
               else if (p_ptr->timed[TMD_FRENZY] > 9) badchance -= 15;
               
               if (goodluck > 10) badchance += (goodluck * 3);
               if (randint(badchance) < 10)
               {
                  int breakprot = 67;
				  if (artifact_p(o_ptr)) breakprot -= a_info[o_ptr->name1].level;
				  if ((p_ptr->timed[TMD_PROTEVIL]) && (randint(100) < breakprot))
				  {
					  msg_format("Your own weapon tries to attacks you but is repelled!");
				  }
				  else
				  {
					  msg_format("..and accidently hit yourself with your own weapon!");
	                  py_attackself(o_ptr);
				  }
               }
            }
		}
	}


	/* Hack -- delay fear messages */
	if (fear && m_ptr->ml)
	{
		/* Message */
		message_format(MSG_FLEE, m_ptr->r_idx, "%^s flees in terror!", m_name);
	}

    /* apply exp drain */
    if (p_ptr->exp_drain)
    {
       int odd = 40;
       if (k > 4) odd += k/5;
       else if (k < 1) odd -= 10;
       rxp_drain(odd);
    }

	/* Mega-Hack -- apply earthquake brand */
	/* now triggers before monster death drop */
	/* if ((do_quake) && (randint(100) < 66)) earthquake(p_ptr->py, p_ptr->px, 10, 0, 0, FALSE); */
}




/*
 * Move player in the given direction, with the given "pickup" flag.
 *
 * This routine should only be called when energy has been expended.
 *
 * Note that this routine handles monsters in the destination grid,
 * and also handles attempting to move into walls/doors/rubble/etc.
 */
void move_player(int dir)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int y, x;
	bool moveit = FALSE;
	bool rubble = FALSE;
	bool openpit = FALSE;

	int climbstr, climbdif;
	bool mighty = FALSE;

	/* if you're currently in a pit, you have to try to climb out */
	if (cave_feat[py][px] == FEAT_OPEN_PIT) openpit = TRUE;

	/* Find the result of moving */
	y = py + ddy[dir];
	x = px + ddx[dir];


	/* Hack -- attack monsters */
	if (cave_m_idx[y][x] > 0)
	{
		monster_type *m_ptr = &mon_list[cave_m_idx[y][x]];
		monster_race *r_ptr = &r_info[m_ptr->r_idx];

		if ((r_ptr->flags1 & (RF1_CHAR_MULTI)) && (m_ptr->disguised))
		{
			message(MSG_HITWALL, 0, "You realize that that's a mimmic!");
			m_ptr->disguised = 0;
			/* wake it up */
			m_ptr->csleep = 0;
			m_ptr->roaming = 0;
			/* redraw undisguised */
			update_mon(cave_m_idx[y][x], 0);
		}
		else
		{
			/* Attack */
			py_attack(y, x);
		}
	}

	/* Optionally alter known traps/doors on movement */
	else if (easy_alter &&
	         (cave_info[y][x] & (CAVE_MARK)) &&
	         (cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
	         (cave_feat[y][x] <= FEAT_DOOR_TAIL))
	{
		/*
		 * There should always be an explicit confirmation made before fiddling
		 * with traps.  XXX XXX
		 */

		/* Auto-repeat if not already repeating */
		if (!p_ptr->command_rep && (p_ptr->command_arg <= 0))
		{
			/* Repeat 99 times */
			p_ptr->command_rep = 99;

			/* Reset the command count */
			p_ptr->command_arg = 0;
		}

		/* Alter */
		do_cmd_alter();
	}

	/* Player can not walk through walls */
	else if ((!cave_floor_bold(y, x)) || (cave_feat[y][x] == FEAT_OPEN_PIT))
	{
		/* Disturb the player */
		disturb(0, 0);

		/* Notice unknown obstacles */
		if (!(cave_info[y][x] & (CAVE_MARK)))
		{
			/* Rubble */
			if (cave_feat[y][x] == FEAT_RUBBLE)
			{
				message(MSG_HITWALL, 0, "You feel a pile of rubble blocking your way.");
				cave_info[y][x] |= (CAVE_MARK);
				lite_spot(y, x);
				rubble = TRUE;
			}

			/* open pit (not a trap, but can act like one if you're blind) */
			else if (cave_feat[y][x] == FEAT_OPEN_PIT)
			{
				int climb = adj_wis_sav[p_ptr->stat_ind[A_DEX]] * 2; /* (0-38) */
				if (climb > 0) climb = climb + randint(climb);
				cave_info[y][x] |= (CAVE_MARK);
				lite_spot(y, x);
				if ((randint(100) < (90-climb)) && (p_ptr->prace != 15))
				{
					message(MSG_HITWALL, 0, "You fall into a pit!");
					if (p_ptr->depth >= 72) take_hit(damroll(2, 9), "a pit");
					else if (p_ptr->depth >= 32) take_hit(damroll(2, p_ptr->depth/8), "a pit");
					else take_hit(damroll(2, 4), "a pit");
				}
				else
				{
					if (p_ptr->prace == 15) /* */;
					/* no damage */
					else message(MSG_HITWALL, 0, "You slide into a pit.");
				}
				moveit = TRUE;
			}

			/* Closed door */
			else if (cave_feat[y][x] <= FEAT_DOOR_TAIL)
			{
				message(MSG_HITWALL, 0, "You feel a door blocking your way.");
				cave_info[y][x] |= (CAVE_MARK);
				lite_spot(y, x);
			}

			/* Wall (or secret door) */
			else
			{
				message(MSG_HITWALL, 0, "You feel a wall blocking your way.");
				cave_info[y][x] |= (CAVE_MARK);
				lite_spot(y, x);
			}
		}

		/* Mention known obstacles */
		else
		{
			/* Rubble */
			if (cave_feat[y][x] == FEAT_RUBBLE)
			{
                /* message is unnessesary when we have a prompt to climb the rubble */
                /* message(MSG_HITWALL, 0, "There is a pile of rubble blocking your way."); */
				rubble = TRUE;
			}

			/* open pit (not a trap) */
			else if (cave_feat[y][x] == FEAT_OPEN_PIT)
			{
				/* adj_wis_sav[p_ptr->stat_ind[A_DEX]] is (0-19), climb is lower the better */
                int climb = 15 - adj_wis_sav[p_ptr->stat_ind[A_DEX]];
				int roll = rand_int(100);
				/* usually fall into the pit when being controlled */
				if (p_ptr->timed[TMD_MIND_CONTROL]) 
					climb = 95 - (adj_wis_sav[p_ptr->stat_ind[A_DEX]] * 2);
				/* small chance of damage */
				if ((roll < (15-climb)) && (p_ptr->prace != 15))
				{
					message(MSG_HITWALL, 0, "You fall into the pit!");
					if (roll > 60) roll = 60;
					take_hit((roll+1) / 2, "a pit"); /* max 8 damage unless controlled */
				}
				else
				{
					if (p_ptr->prace == 15) /* */;
					/* no damage */
					else message(MSG_HITWALL, 0, "You slide into the pit.");
				}
				moveit = TRUE;
			}

			/* Closed door */
			else if (cave_feat[y][x] <= FEAT_DOOR_TAIL)
			{
				message(MSG_HITWALL, 0, "There is a door blocking your way.");
			}

			/* Wall (or secret door) */
			else
			{
				message(MSG_HITWALL, 0, "There is a wall blocking your way.");
            }
		}
	}
	else /* nothing in the way */
	{
        moveit = TRUE;
    }


	/* held by a monster */
	if ((p_ptr->timed[TMD_BEAR_HOLD]) && (p_ptr->held_m_idx) && (moveit))
	{
		monster_type *m_ptr = &mon_list[p_ptr->held_m_idx];
		char m_name[80];
		monster_desc(m_name, sizeof(m_name), m_ptr, 0x00);
		/* (only when moving away from the monster) */
		if (distance(y, x, m_ptr->fy, m_ptr->fx) > m_ptr->cdis)
		{
			monster_race *r_ptr = &r_info[m_ptr->r_idx];
			int wrestle, holdfast, rlev;
			rlev = ((r_ptr->level >= 6) ? r_ptr->level : 6);

			if (r_ptr->flags1 & (RF1_UNIQUE))
			{
				holdfast = rlev * 3;
				if (holdfast < 25) holdfast = 25;
			}
			else holdfast = (rlev * 5) / 2 + randint(rlev/2); /* (x2.5 +dx0.5) */
			wrestle = adj_str_wgt[p_ptr->stat_ind[A_STR]] + goodluck/2;
			wrestle += adj_str_wgt[p_ptr->stat_ind[A_DEX]];
			if (p_ptr->free_act) wrestle += 20;
	
			/* attempt to pull free */
			if (randint(wrestle) > holdfast)
			{
				p_ptr->held_m_idx = 0;
				msg_print("You pull free.");
				clear_timed(TMD_BEAR_HOLD);
			}
			else
			{
				msg_format("You are being held by %s!", m_name);
				return;
			}
		}
	}

    if (rubble)
    {
        bool climbit = FALSE;
        if (p_ptr->timed[TMD_MIND_CONTROL]) climbit = TRUE;
        else if (p_ptr->prace == 15) climbit = TRUE;
        else if (get_check("Try to climb over the rubble? ")) climbit = TRUE;
        if (climbit)
        {
			if (p_ptr->timed[TMD_MIGHTY_HURL]) mighty = TRUE;
			/* an extremely strong barbarian or hulk is also mighty */
			if ((((int)(adj_con_fix[p_ptr->stat_ind[A_STR]]) - 128) > 7) && 
				(cp_ptr->flags & CF_HEAVY_BONUS)) mighty = TRUE;
			/* climbing strength based on encumberance and strength */
			/* reference: STR10: (adj_str_wgt[p_ptr->stat_ind[A_STR]] * 10) = 130 */
			/* reference: STR14: (adj_str_wgt[p_ptr->stat_ind[A_STR]] * 10) = 170 */
			/* reference: STR16: (adj_str_wgt[p_ptr->stat_ind[A_STR]] * 10) = 200 */
			/* reference: STR18: (adj_str_wgt[p_ptr->stat_ind[A_STR]] * 10) = 230 */
			/* reference: STR18/99: (adj_str_wgt[p_ptr->stat_ind[A_STR]] * 10) = 310 */
			/* (p_ptr->total_weight/10) is usually close to half of the above */
            climbstr = (adj_str_wgt[p_ptr->stat_ind[A_STR]] * 9);
            climbstr += (adj_str_wgt[p_ptr->stat_ind[A_DEX]] * 2);
            climbdif = 68 + (p_ptr->depth/4); /* was 95 + (p_ptr->depth/4) */
            /* cap difficulty */
            if (climbdif > 90 - goodluck) climbdif -= (climbdif-90-goodluck)/2;
			/* harder to climb onto rubble from inside a pit */
			if (openpit) climbdif += 12;
			/* always succeed with MIGHTY_HURL */
			if (mighty) climbstr += 200;
            /* give weak characters a chance */
            if (climbstr < 200) climbstr += (200 - climbstr) / 2;
            /* power sprites can fly */
            if (p_ptr->prace == 15)
            {
               msg_print("You fly over the rubble.");
                moveit = TRUE;
            } 
            else if (climbstr < climbdif + 2)
            {
               msg_print("You're too weak to climb over the rubble.");
            } 
            else if (climbstr - (p_ptr->total_weight/11) < climbdif + 2)
            {
               msg_print("You're carrying too much to climb over the rubble.");
            }
            else /* attempt the climb */
            {
               /* encumberance (rarely less than ~70-80) */
               climbstr -= (p_ptr->total_weight/12);
                   
			   if ((randint(climbstr) > climbdif) || (climbstr >= climbdif + 90))
			   {
                  msg_print("You climb over the rubble.");
                  moveit = TRUE;
               }
               else
               {
                  if ((p_ptr->total_weight/13) > randint(climbstr))
                  {
                     msg_print("Your encumberance makes you fall as you try to climb.");
                     (void)inc_timed(TMD_PARALYZED, 1 + randint(2));
                  }
                  else
                  {
                     msg_print("You fail to climb over the rubble.");
                  }
               }
            }
        }
        else /* decide not to climb the rubble so no movement */
        {
           p_ptr->energy_use = 0;
        }
    }
	/* don't climb out of one pit into another -assumed to be the same pit */
	else if (cave_feat[y][x] == FEAT_OPEN_PIT) /* */;
	/* have to climb out of the pit */
	else if ((openpit) && (moveit))
	{
		if (p_ptr->timed[TMD_MIGHTY_HURL]) mighty = TRUE;
		/* an extremely strong barbarian or hulk is also mighty */
		if ((((int)(adj_con_fix[p_ptr->stat_ind[A_STR]]) - 128) > 7) &&
			(cp_ptr->flags & CF_HEAVY_BONUS)) mighty = TRUE;
		/* climbing strength based on encumberance and strength */
        climbstr = (adj_str_wgt[p_ptr->stat_ind[A_STR]] * 8);
        climbstr += (adj_str_wgt[p_ptr->stat_ind[A_DEX]] * 3);
		/* climbdif = 68 + (p_ptr->depth/4); (easier than climbing over rubble) */
        climbdif = 53 + (p_ptr->depth/3);
		if (climbdif > 80) climbdif = 81; /* cap */
		/* always succeed with MIGHTY_HURL */
		if (mighty) climbstr += 200;
        /* give weak characters a chance */
        if (climbstr < 160) climbstr += (160 - climbstr) / 2;
        if (p_ptr->prace == 15)
        {
            /* power sprites can fly over pits */;
        } 
        else if (climbstr < climbdif + 2)
        {
			msg_print("You're too weak to climb out of the pit.");
			moveit = FALSE;
        } 
		else if (climbstr - (p_ptr->total_weight/11) < climbdif + 2)
		{
			msg_print("You're carrying too much to climb out of the pit.");
			moveit = FALSE;
		}
		else /* attempt the climb */
		{
			/* encumberance (rarely less than ~70-80) */
			climbstr -= (p_ptr->total_weight/12);
                   
			if ((randint(climbstr) > climbdif) || (climbstr >= climbdif + 80))
			{
				msg_print("You climb out of the pit.");
				moveit = TRUE;
			}
			else
			{
				moveit = FALSE;
				if ((p_ptr->total_weight/13) > randint(climbstr))
				{
					msg_print("Your encumberance makes you fall as you try to climb.");
					(void)inc_timed(TMD_PARALYZED, 1 + randint(2));
				}
				else
				{
					msg_print("You fail to climb out of the pit.");
				}
			}
		}
	}

	/* Normal movement */
	if (moveit)
	{
		int trapalert;
		/* Sound XXX XXX XXX */
		/* sound(MSG_WALK); */

		/* Move player */
		monster_swap(py, px, y, x);

		/* New location */
		y = py = p_ptr->py;
		x = px = p_ptr->px;
		
		trapalert = p_ptr->skills[SKILL_FOS];
		/* mainly to help dwarves who have darkvision but horrible alertness */
		if (p_ptr->darkvis) trapalert += 6;

		/* Spontaneous Searching */
		if ((trapalert >= 50) ||
		    (0 == rand_int(50 - trapalert)))
		{
			search();
		}

		/* Continuous Searching */
		if (p_ptr->searching)
		{
			search();
		}


		/* Handle "store doors" */
		if ((cave_feat[y][x] >= FEAT_SHOP_HEAD) &&
		    (cave_feat[y][x] <= FEAT_SHOP_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hack -- Enter store */
			p_ptr->command_new = '_';

			/* Handle objects now.  XXX */
			p_ptr->energy_use = py_pickup(2) * 10;
		}


		/* All other grids (including traps) */
		else
		{
			/* Handle objects (later) */
			p_ptr->notice |= (PN_PICKUP);
		}


		/* Discover invisible traps */
		if (cave_feat[y][x] == FEAT_INVIS)
		{
			/* Disturb */
			disturb(0, 0);

			/* Message */
			msg_print("You found a trap!");

			/* Pick a trap */
			pick_trap(y, x);

			/* Hit the trap */
			hit_trap(y, x);
		}

		/* Set off a visible trap */
		else if ((cave_feat[y][x] >= FEAT_TRAP_HEAD) &&
		         (cave_feat[y][x] <= FEAT_TRAP_TAIL))
		{
			/* Disturb */
			disturb(0, 0);

			/* Hit the trap */
			hit_trap(y, x);
		}
	}
	
	p_ptr->window |= (PW_OBJLIST);
}


/*
 * Hack -- Check for a "known wall" (see below)
 */
static int see_wall(int dir, int y, int x)
{
	/* Get the new location */
	y += ddy[dir];
	x += ddx[dir];

	/* Illegal grids are not known walls XXX XXX XXX */
	if (!in_bounds(y, x)) return (FALSE);

	/* Non-wall grids are not known walls */
	if (cave_feat[y][x] < FEAT_SECRET) return (FALSE);

	/* Unknown walls are not known walls */
	if (!(cave_info[y][x] & (CAVE_MARK))) return (FALSE);

	/* Default */
	return (TRUE);
}




/*
 * The running algorithm  -CJS-
 *
 * Basically, once you start running, you keep moving until something
 * interesting happens.  In an enclosed space, you run straight, but
 * you follow corners as needed (i.e. hallways).  In an open space,
 * you run straight, but you stop before entering an enclosed space
 * (i.e. a room with a doorway).  In a semi-open space (with walls on
 * one side only), you run straight, but you stop before entering an
 * enclosed space or an open space (i.e. running along side a wall).
 *
 * All discussions below refer to what the player can see, that is,
 * an unknown wall is just like a normal floor.  This means that we
 * must be careful when dealing with "illegal" grids.
 *
 * No assumptions are made about the layout of the dungeon, so this
 * algorithm works in hallways, rooms, town, destroyed areas, etc.
 *
 * In the diagrams below, the player has just arrived in the grid
 * marked as '@', and he has just come from a grid marked as 'o',
 * and he is about to enter the grid marked as 'x'.
 *
 * Running while confused is not allowed, and so running into a wall
 * is only possible when the wall is not seen by the player.  This
 * will take a turn and stop the running.
 *
 * Several conditions are tracked by the running variables.
 *
 *   p_ptr->run_open_area (in the open on at least one side)
 *   p_ptr->run_break_left (wall on the left, stop if it opens)
 *   p_ptr->run_break_right (wall on the right, stop if it opens)
 *
 * When running begins, these conditions are initialized by examining
 * the grids adjacent to the requested destination grid (marked 'x'),
 * two on each side (marked 'L' and 'R').  If either one of the two
 * grids on a given side is a wall, then that side is considered to
 * be "closed".  Both sides enclosed yields a hallway.
 *
 *    LL                     @L
 *    @x      (normal)       RxL   (diagonal)
 *    RR      (east)          R    (south-east)
 *
 * In the diagram below, in which the player is running east along a
 * hallway, he will stop as indicated before attempting to enter the
 * intersection (marked 'x').  Starting a new run in any direction
 * will begin a new hallway run.
 *
 * #.#
 * ##.##
 * o@x..
 * ##.##
 * #.#
 *
 * Note that a minor hack is inserted to make the angled corridor
 * entry (with one side blocked near and the other side blocked
 * further away from the runner) work correctly. The runner moves
 * diagonally, but then saves the previous direction as being
 * straight into the gap. Otherwise, the tail end of the other
 * entry would be perceived as an alternative on the next move.
 *
 * In the diagram below, the player is running east down a hallway,
 * and will stop in the grid (marked '1') before the intersection.
 * Continuing the run to the south-east would result in a long run
 * stopping at the end of the hallway (marked '2').
 *
 * ##################
 * o@x       1
 * ########### ######
 * #2          #
 * #############
 *
 * After each step, the surroundings are examined to determine if
 * the running should stop, and to determine if the running should
 * change direction.  We examine the new current player location
 * (at which the runner has just arrived) and the direction from
 * which the runner is considered to have come.
 *
 * Moving one grid in some direction places you adjacent to three
 * or five new grids (for straight and diagonal moves respectively)
 * to which you were not previously adjacent (marked as '!').
 *
 *   ...!              ...
 *   .o@!  (normal)    .o.!  (diagonal)
 *   ...!  (east)      ..@!  (south east)
 *                      !!!
 *
 * If any of the newly adjacent grids are "interesting" (monsters,
 * objects, some terrain features) then running stops.
 *
 * If any of the newly adjacent grids seem to be open, and you are
 * looking for a break on that side, then running stops.
 *
 * If any of the newly adjacent grids do not seem to be open, and
 * you are in an open area, and the non-open side was previously
 * entirely open, then running stops.
 *
 * If you are in a hallway, then the algorithm must determine if
 * the running should continue, turn, or stop.  If only one of the
 * newly adjacent grids appears to be open, then running continues
 * in that direction, turning if necessary.  If there are more than
 * two possible choices, then running stops.  If there are exactly
 * two possible choices, separated by a grid which does not seem
 * to be open, then running stops.  Otherwise, as shown below, the
 * player has probably reached a "corner".
 *
 *    ###             o##
 *    o@x  (normal)   #@!   (diagonal)
 *    ##!  (east)     ##x   (south east)
 *
 * In this situation, there will be two newly adjacent open grids,
 * one touching the player on a diagonal, and one directly adjacent.
 * We must consider the two "option" grids further out (marked '?').
 * We assign "option" to the straight-on grid, and "option2" to the
 * diagonal grid.  For some unknown reason, we assign "check_dir" to
 * the grid marked 's', which may be incorrectly labelled.
 *
 *    ###s
 *    o@x?   (may be incorrect diagram!)
 *    ##!?
 *
 * If both "option" grids are closed, then there is no reason to enter
 * the corner, and so we can cut the corner, by moving into the other
 * grid (diagonally).  If we choose not to cut the corner, then we may
 * go straight, but we pretend that we got there by moving diagonally.
 * Below, we avoid the obvious grid (marked 'x') and cut the corner
 * instead (marked 'n').
 *
 *    ###:               o##
 *    o@x#   (normal)    #@n    (maybe?)
 *    ##n#   (east)      ##x#
 *                       ####
 *
 * If one of the "option" grids is open, then we may have a choice, so
 * we check to see whether it is a potential corner or an intersection
 * (or room entrance).  If the grid two spaces straight ahead, and the
 * space marked with 's' are both open, then it is a potential corner
 * and we enter it if requested.  Otherwise, we stop, because it is
 * not a corner, and is instead an intersection or a room entrance.
 *
 *    ###
 *    o@x
 *    ##!#
 *
 * I do not think this documentation is correct.
 */




/*
 * Hack -- allow quick "cycling" through the legal directions
 */
static const byte cycle[] =
{ 1, 2, 3, 6, 9, 8, 7, 4, 1, 2, 3, 6, 9, 8, 7, 4, 1 };

/*
 * Hack -- map each direction into the "middle" of the "cycle[]" array
 */
static const byte chome[] =
{ 0, 8, 9, 10, 7, 0, 11, 6, 5, 4 };



/*
 * Initialize the running algorithm for a new direction.
 *
 * Diagonal Corridor -- allow diaginal entry into corridors.
 *
 * Blunt Corridor -- If there is a wall two spaces ahead and
 * we seem to be in a corridor, then force a turn into the side
 * corridor, must be moving straight into a corridor here. (?)
 *
 * Diagonal Corridor    Blunt Corridor (?)
 *       # #                  #
 *       #x#                 @x#
 *       @p.                  p
 */
static void run_init(int dir)
{
	int py = p_ptr->py;
	int px = p_ptr->px;

	int i, row, col;

	bool deepleft, deepright;
	bool shortleft, shortright;


	/* Save the direction */
	p_ptr->run_cur_dir = dir;

	/* Assume running straight */
	p_ptr->run_old_dir = dir;

	/* Assume looking for open area */
	p_ptr->run_open_area = TRUE;

	/* Assume not looking for breaks */
	p_ptr->run_break_right = FALSE;
	p_ptr->run_break_left = FALSE;

	/* Assume no nearby walls */
	deepleft = deepright = FALSE;
	shortright = shortleft = FALSE;

	/* Find the destination grid */
	row = py + ddy[dir];
	col = px + ddx[dir];

	/* Extract cycle index */
	i = chome[dir];

	/* Check for nearby wall */
	if (see_wall(cycle[i+1], py, px))
	{
		p_ptr->run_break_left = TRUE;
		shortleft = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i+1], row, col))
	{
		p_ptr->run_break_left = TRUE;
		deepleft = TRUE;
	}

	/* Check for nearby wall */
	if (see_wall(cycle[i-1], py, px))
	{
		p_ptr->run_break_right = TRUE;
		shortright = TRUE;
	}

	/* Check for distant wall */
	else if (see_wall(cycle[i-1], row, col))
	{
		p_ptr->run_break_right = TRUE;
		deepright = TRUE;
	}

	/* Looking for a break */
	if (p_ptr->run_break_left && p_ptr->run_break_right)
	{
		/* Not looking for open area */
		p_ptr->run_open_area = FALSE;

		/* Hack -- allow angled corridor entry */
		if (dir & 0x01)
		{
			if (deepleft && !deepright)
			{
				p_ptr->run_old_dir = cycle[i - 1];
			}
			else if (deepright && !deepleft)
			{
				p_ptr->run_old_dir = cycle[i + 1];
			}
		}

		/* Hack -- allow blunt corridor entry */
		else if (see_wall(cycle[i], row, col))
		{
			if (shortleft && !shortright)
			{
				p_ptr->run_old_dir = cycle[i - 2];
			}
			else if (shortright && !shortleft)
			{
				p_ptr->run_old_dir = cycle[i + 2];
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
	int py = p_ptr->py;
	int px = p_ptr->px;

	int prev_dir;
	int new_dir;
	int check_dir = 0;

	int row, col;
	int i, max, inv;
	int option, option2;


	/* No options yet */
	option = 0;
	option2 = 0;

	/* Where we came from */
	prev_dir = p_ptr->run_old_dir;


	/* Range of newly adjacent grids */
	max = (prev_dir & 0x01) + 1;


	/* Look at every newly adjacent square. */
	for (i = -max; i <= max; i++)
	{
		object_type *o_ptr;


		/* New direction */
		new_dir = cycle[chome[prev_dir] + i];

		/* New location */
		row = py + ddy[new_dir];
		col = px + ddx[new_dir];


		/* Visible monsters abort running */
		if (cave_m_idx[row][col] > 0)
		{
			monster_type *m_ptr = &mon_list[cave_m_idx[row][col]];

			/* Visible monster */
			if (m_ptr->ml) return (TRUE);
		}

		/* Visible objects abort running */
		for (o_ptr = get_first_object(row, col); o_ptr; o_ptr = get_next_object(o_ptr))
		{
			/* Visible object */
			if (o_ptr->marked && !squelch_hide_item(o_ptr)) return (TRUE);
		}


		/* Assume unknown */
		inv = TRUE;

		/* Check memorized grids */
		if (cave_info[row][col] & (CAVE_MARK))
		{
			bool notice = TRUE;

			/* Examine the terrain */
			switch (cave_feat[row][col])
			{
				/* New features (don't run into a pit or water) */
				case FEAT_OPEN_PIT:
				case FEAT_WATER:
				{
					/* notice = TRUE */ break;
				}

				/* Floors */
				case FEAT_FLOOR:

				/* Invis traps */
				case FEAT_INVIS:

				/* Secret doors */
				case FEAT_SECRET:

				/* Normal veins */
				case FEAT_MAGMA:
				case FEAT_QUARTZ:

				/* Hidden treasure */
				case FEAT_MAGMA_H:
				case FEAT_QUARTZ_H:

				/* Walls */
				case FEAT_WALL_EXTRA:
				case FEAT_WALL_INNER:
				case FEAT_WALL_OUTER:
				case FEAT_WALL_SOLID:
				case FEAT_PERM_EXTRA:
				case FEAT_PERM_INNER:
				case FEAT_PERM_OUTER:
				case FEAT_PERM_SOLID:
				{
					/* Ignore */
					notice = FALSE;

					/* Done */
					break;
				}
			}

			/* Interesting feature */
			if (notice) return (TRUE);

			/* The grid is "visible" */
			inv = FALSE;
		}

		/* Analyze unknown grids and floors */
		if (inv || cave_floor_bold(row, col))
		{
			/* Looking for open area */
			if (p_ptr->run_open_area)
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
			if (p_ptr->run_open_area)
			{
				if (i < 0)
				{
					/* Break to the right */
					p_ptr->run_break_right = TRUE;
				}

				else if (i > 0)
				{
					/* Break to the left */
					p_ptr->run_break_left = TRUE;
				}
			}
		}
	}


	/* Looking for open area */
	if (p_ptr->run_open_area)
	{
		/* Hack -- look again */
		for (i = -max; i < 0; i++)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = py + ddy[new_dir];
			col = px + ddx[new_dir];

			/* Unknown grid or non-wall */
			/* Was: cave_floor_bold(row, col) */
			if (!(cave_info[row][col] & (CAVE_MARK)) ||
			    (cave_feat[row][col] < FEAT_SECRET))
			{
				/* Looking to break right */
				if (p_ptr->run_break_right)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break left */
				if (p_ptr->run_break_left)
				{
					return (TRUE);
				}
			}
		}

		/* Hack -- look again */
		for (i = max; i > 0; i--)
		{
			new_dir = cycle[chome[prev_dir] + i];

			row = py + ddy[new_dir];
			col = px + ddx[new_dir];

			/* Unknown grid or non-wall */
			/* Was: cave_floor_bold(row, col) */
			if (!(cave_info[row][col] & (CAVE_MARK)) ||
			    (cave_feat[row][col] < FEAT_SECRET))
			{
				/* Looking to break left */
				if (p_ptr->run_break_left)
				{
					return (TRUE);
				}
			}

			/* Obstacle */
			else
			{
				/* Looking to break right */
				if (p_ptr->run_break_right)
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
			p_ptr->run_cur_dir = option;

			/* No other options */
			p_ptr->run_old_dir = option;
		}

		/* Two options, examining corners */
		else
		{
			/* Primary option */
			p_ptr->run_cur_dir = option;

			/* Hack -- allow curving */
			p_ptr->run_old_dir = option2;
		}
	}


	/* About to hit a known wall, stop */
	if (see_wall(p_ptr->run_cur_dir, py, px))
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
void run_step(int dir)
{
	int x, y;

	/* Start run */
	if (dir)
	{
		/* Initialize */
		run_init(dir);

		/* Hack -- Set the run counter */
		p_ptr->running = (p_ptr->command_arg ? p_ptr->command_arg : 1000);

		/* Calculate torch radius */
		p_ptr->update |= (PU_TORCH);
	}

	/* Continue run */
	else
	{
		if (!p_ptr->running_withpathfind)
		{
			/* Update run */
			if (run_test())
			{
				/* Disturb */
				disturb(0, 0);
	
				/* Done */
				return;
			}
		}
		else
		{
			/* Abort if we have finished */
			if (pf_result_index < 0)
			{
				disturb(0, 0);
				p_ptr->running_withpathfind = FALSE;
				return;
			}
			/* Abort if we would hit a wall */
			else if (pf_result_index == 0)
			{
				/* Get next step */
				y = p_ptr->py + ddy[pf_result[pf_result_index] - '0'];
				x = p_ptr->px + ddx[pf_result[pf_result_index] - '0'];

				/* Known wall */
				if ((cave_info[y][x] & (CAVE_MARK)) && !cave_floor_bold(y, x))
				{
					disturb(0,0);
					p_ptr->running_withpathfind = FALSE;
					return;
				}
			}
			/* Hack -- walking stick lookahead.
			 *
			 * If the player has computed a path that is going to end up in a wall,
			 * we notice this and convert to a normal run. This allows us to click
			 * on unknown areas to explore the map.
			 *
			 * We have to look ahead two, otherwise we don't know which is the last
			 * direction moved and don't initialise the run properly.
			 */
			else if (pf_result_index > 0)
			{
				/* Get next step */
				y = p_ptr->py + ddy[pf_result[pf_result_index] - '0'];
				x = p_ptr->px + ddx[pf_result[pf_result_index] - '0'];

				/* Known wall */
				if ((cave_info[y][x] & (CAVE_MARK)) && !cave_floor_bold(y, x))
				{
					disturb(0,0);
					p_ptr->running_withpathfind = FALSE;
					return;
				}

				/* Get step after */
				y = y + ddy[pf_result[pf_result_index-1] - '0'];
				x = x + ddx[pf_result[pf_result_index-1] - '0'];

				/* Known wall */
				if ((cave_info[y][x] & (CAVE_MARK)) && !cave_floor_bold(y, x))
				{
					p_ptr->running_withpathfind = FALSE;

					run_init(pf_result[pf_result_index] - '0');
				}
			}

			p_ptr->run_cur_dir = pf_result[pf_result_index--] - '0';

			/* Hack -- allow easy_alter */
			p_ptr->command_dir = p_ptr->run_cur_dir;
		}
	}


	/* Decrease counter */
	p_ptr->running--;

	/* Take time */
	p_ptr->energy_use = 100;

	/* Move the player.  Never pick up objects */
	p_ptr->auto_pickup_okay = FALSE;
	move_player(p_ptr->run_cur_dir);
}
