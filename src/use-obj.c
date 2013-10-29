/* File: use_obj.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"
#include "script.h"


static bool eat_food(object_type *o_ptr, bool *ident)
{
	int price, die, time;
	
	/* Analyze the food */
	switch (o_ptr->sval)
	{
		case SV_FOOD_POISON:
		{
			if (!(p_ptr->resist_pois || p_ptr->timed[TMD_OPP_POIS]))
			{
				if (inc_timed(TMD_POISONED, randint(15) + 15))
				{
                    int dds = o_ptr->ds; /* damroll(o_ptr->dd, o_ptr->ds) == 9d4 */
                    int ddd = o_ptr->dd;
					/* ID-by use should not be a killer if you're careful to eat with near-full HP */
					if ((p_ptr->lev < 6) && (dds >= 4)) dds = dds / 2;
					else if ((p_ptr->lev < 6) && (dds == 3)) dds -= 1;
					if ((p_ptr->weakresist_pois) && (ddd >= 4)) ddd = (ddd+1)/2;
					take_hit(damroll(ddd, dds), "poisonous food");
					/* else take_hit(damroll(6, 6), "poisonous food"); */
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_FOOD_SECOND_SIGHT:
		{
			if (inc_timed(TMD_BLIND, randint(240) + 240))
			{
                if (p_ptr->resist_blind) dec_timed(TMD_BLIND, 240 - badluck);
				*ident = TRUE;
			}
            (void)inc_timed(TMD_MESP, randint(580) + 400);
			break;
		}

		/* changed to 'good mood' but name not changed in the code */
        case SV_FOOD_PARANOIA:
		{
			if ((!p_ptr->resist_charm))
			{
				   if (inc_timed(TMD_CHARM, randint(8) + 10)) *ident = TRUE;
			}
			break;
		}

		case SV_FOOD_TERROR:
		{
			if (inc_timed(TMD_TERROR, randint(12) + 18)) *ident = TRUE;
			break;
		}

        /* note: rchaos and true sight prevent the rare possibility of +2 luck */
		case SV_FOOD_HALLUCINATION:
		{
            die = randint(100);
            /* more likely to gain luck if you're already hallucenating */
            if (p_ptr->timed[TMD_IMAGE]) die -= 11;
            
			if (die < 10)
            {
               p_ptr->luck += 1;
               msg_print("like this stuff is groovy, man!");
            }

			if ((!p_ptr->resist_chaos) && (!p_ptr->timed[TMD_TSIGHT]))
			{
				if (inc_timed(TMD_IMAGE, rand_int(210 + badluck*2) + 210))
				{
				   *ident = TRUE;
                   if (randint(100) < 3)
                   {
                      p_ptr->luck += 1;
                      inc_timed(TMD_IMAGE, randint(18) + randint(18));
                      if (die < 10) msg_print("You never felt so high in your life!");
                      else msg_print("Wow, this stuff is groovy, man!");
                   }
				}
			}
			break;
		}

		case SV_FOOD_STONE_SKIN:
		{
			if (inc_timed(TMD_STONESKIN, randint(120) + 90)) *ident = TRUE;
			break;
		}

		case SV_FOOD_LUCKFINDING: /* pricy luckfinding */
		{
            if ((p_ptr->luck > 37) && (p_ptr->luck < 41) && (randint(100) < 50))
            {
                  p_ptr->luck += 1;
            }
            else if (p_ptr->luck > 37)
            {
                  msg_print("This tastes rather bland.");
                  break;
            }
            else if (p_ptr->luck < 38)
            {
                  p_ptr->luck += randint(3);
            }
            msg_print("This is good stuff.");
            price = randint(6);
            /* maybe avoid lowering strength depending on luck */
            if ((price == 1) && ((badluck*2) - goodluck < rand_int(6) - rand_int(2))) 
                price = randint(5) + 1;
            /* strength drain isn't always permanent */
            if ((price == 1) && (goodluck - badluck >= 0))
                (void)dec_stat(A_STR, 20 - goodluck, FALSE);
            /* otherwise permanent & bypasses sustains */
			else if (price == 1) (void)dec_stat(A_STR, 10, TRUE);
			else if (price == 2) (void)dec_stat(A_DEX, 10, TRUE);
			else if (price == 3) (void)dec_stat(A_CON, 10, TRUE);
			else if (price == 4) (void)dec_stat(A_INT, 10, TRUE);
			else if (price == 5) (void)dec_stat(A_WIS, 10, TRUE);
			else if (price == 6) (void)dec_stat(A_CHR, 10, TRUE);
			take_hit(damroll(3, 6), "poisonous clover");
            msg_print("..but it must have been poisonous.");
			*ident = TRUE;
			break;
		}

		case SV_FOOD_MUNCHKIN: /* unlucky munchkin */
		{
            if (p_ptr->luck < 1) break;
            if ((p_ptr->luck < 3) && (randint(100) < 50))
            {
                  p_ptr->luck -= 1;
            }
            else if (p_ptr->luck < 3)
            {
               inc_timed(TMD_WITCH, randint(113) + 113);
               break;
            }
            else if (p_ptr->luck > 2)
            {
                  p_ptr->luck -= randint(2) + 1;
            }
            msg_print("..That didn't taste so good.");
			*ident = TRUE;
			time = randint(113) + 100;
			/* spellcasters get magic bonuses, others get other buffs */
			if (cp_ptr->flags & CF_ZERO_FAIL)
			{
               inc_timed(TMD_BRAIL, time);
               inc_timed(TMD_SINVIS, time);
            }
            else
            {
               inc_timed(TMD_HERO, time);
               inc_timed(TMD_SHERO, time);
               inc_timed(TMD_BLESSED, time);
            }
			break;
		}

		case SV_FOOD_POTLUCK:
		{
            int die = randint(100);
            if (randint(100) < 50)
            {
               if (goodluck) die += goodluck + randint(goodluck+1);
               if (badluck) die -= badluck + randint(badluck+1);
            }
            if (die < 1) /* with bad luck only */
            {
               inc_timed(TMD_STUN, randint(20) + 20 + badluck);
               inc_timed(TMD_TERROR, randint(30) + 30 + badluck);
               inc_timed(TMD_AMNESIA, randint(50) + 50 + badluck);
               inc_timed(TMD_BLIND, randint(60) + 30 + badluck);
			   take_hit(damroll(3, 6), "poisonous food");
            }
            else if ((die < 10) && (!p_ptr->timed[TMD_SUST_SPEED])) inc_timed(TMD_SLOW, randint(100) + 100);
            else if (die < 20) /* poison razor */
            {
               inc_timed(TMD_CUT, randint(20) + 20 + badluck);
               if (!(p_ptr->resist_pois || p_ptr->timed[TMD_OPP_POIS]))
               {
                  inc_timed(TMD_POISONED, randint(20) + 20 + badluck);
               }
            }
            else if (die < 25) inc_timed(TMD_CURSE, randint(80) + 80);
            else if (die < 30) inc_timed(TMD_AMNESIA, randint(100) + 100);
            else if (die < 40) /* blah */
            {
               int time = randint(150) + 150;
               inc_timed(TMD_WOPP_POIS, time);
               inc_timed(TMD_SUST_SPEED, time);
               if (!p_ptr->resist_blind) inc_timed(TMD_BLIND, time-10);
            }
            else if (die < 50) /* charm & zap */
            {
               int time = randint(200) + 200;
               inc_timed(TMD_ZAPPING, time);
               inc_timed(TMD_SPHERE_CHARM, time);
               if (!p_ptr->resist_confu) inc_timed(TMD_CONFUSED, (time/3));
            }
            else if (die < 60) /* minor bonuses */
            {
               int time = randint(200) + 200;
               inc_timed(TMD_SINFRA, time);
               inc_timed(TMD_WSHIELD, time);
			   inc_timed(TMD_MIGHTY_HURL, (time/5));
            }
            else if (die < 70) /* Dark resistance & darkvision */
            {
               int time = randint(200) + 200;
               inc_timed(TMD_DARKVIS, time);
               inc_timed(TMD_OPP_DARK, time);
               inc_timed(TMD_HOLDLIFE, time);
            }
            else if (die < 80) /* curing */
            {
			   clear_timed(TMD_BLIND);
			   clear_timed(TMD_POISONED);
			   clear_timed(TMD_STUN);
			   clear_timed(TMD_CUT);
			   clear_timed(TMD_CONFUSED);
			   clear_timed(TMD_AMNESIA);
			   if (!p_ptr->timed[TMD_SUST_SPEED]) clear_timed(TMD_SLOW);
			   hp_player(damroll(3, 9));
               inc_timed(TMD_OPP_POIS, randint(150) + 150);
			   if (p_ptr->silver > PY_SILVER_HEALTHY) p_ptr->silver -= 1;
               if (p_ptr->slime > PY_SLIME_HEALTHY) p_ptr->slime -= 1;
            }
            else if (die < 90) /* anti-evil */
            {
               int time = randint(200) + 200;
               inc_timed(TMD_PROTEVIL, time);
               inc_timed(TMD_SANCTIFY, time);
            }
            else if (die < 95) /* undead slayer */
            {
               int time = randint(200) + 200;
               inc_timed(TMD_OPP_NETHR, time);
               inc_timed(TMD_CLEAR_MIND, time);
               inc_timed(TMD_DAYLIGHT, time);
            }
            else if (die < 100) /* dragonfighter */
            {
               int time = randint(200) + 200;
               inc_timed(TMD_QUIVERGUARD, time);
               inc_timed(TMD_BR_SHIELD, time);
               inc_timed(TMD_ESP, time);
            }
            /* (die > 99, with good luck only) */
            else if (get_check("You got the luckiest effects! What's your style? y=melee, n=ranged"))
            {
               /* luckiest effects: melee */
               int time = randint(200) + 200;
               inc_timed(TMD_SHIELD, time);
               inc_timed(TMD_XATTACK, time);
               inc_timed(TMD_PROTEVIL2, time);
            }
            else /* luckiest effects: ranged */
            {
               int time = randint(200) + 200;
               inc_timed(TMD_SHADOW, time);
               if (!p_ptr->timed[TMD_SUST_SPEED]) inc_timed(TMD_FAST, time);
               inc_timed(TMD_TSIGHT, time);
            }
			*ident = TRUE;
			break;
		}

		/* changed to 'Idiocy' but name not changed in the code */
        case SV_FOOD_DISEASE:
		{
			if (randint((badluck*2)+10) >= 20)
			{
               (void)do_dec_stat(A_INT, 0);
               (void)do_dec_stat(A_WIS, 0);
            }
            /* drain according to spell stat */
            else if ((randint(badluck/2 + 10) > 4) && (cp_ptr->spell_book))
            {
               if (cp_ptr->spell_stat == A_INT) (void)do_dec_stat(A_INT, 0);
               else (void)do_dec_stat(A_WIS, 0);
            }
			else if (randint(100) < 50) (void)do_dec_stat(A_INT, 0);
			else (void)do_dec_stat(A_WIS, 0);
			p_ptr->silver += 3;
			/* confusion or amnesia */
			if (randint(badluck/2 + 10) > 5)
			{
               if (randint(100) < 50) inc_timed(TMD_AMNESIA, randint(50) + 25 + badluck);
               else inc_timed(TMD_CONFUSED, randint(50) + 25 + badluck);
            }
			/* ID-by use should not be a killer if you're careful to eat with near-full HP */
            if (p_ptr->lev < 15) take_hit(damroll(o_ptr->dd, o_ptr->ds-2), "poisonous food");
			else take_hit(damroll(o_ptr->dd, o_ptr->ds), "poisonous food");
			/* take_hit(damroll(8, 8), "poisonous food"); */
			*ident = TRUE;
			break;
		}

		case SV_FOOD_UNHEALTH:
		{
			if (randint((badluck*2)+10) >= 20)
			{
               (void)do_dec_stat(A_CON, 0);
               (void)do_dec_stat(A_STR, 0);
            }
			else if (randint(100) < 50) (void)do_dec_stat(A_CON, 0);
			else (void)do_dec_stat(A_STR, 0);
			p_ptr->slime += 5;
			if (randint(badluck/2 + 10) > 6)
			{
               inc_timed(TMD_CUT, randint(25) + 25 + badluck);
            }
            
			/* ID-by use should not be a killer if you're careful to eat with near-full HP */
            if (p_ptr->lev < 15) take_hit(damroll(o_ptr->dd, o_ptr->ds/2), "poisonous food");
			else take_hit(damroll(o_ptr->dd, o_ptr->ds), "poisonous food");
			/* take_hit(damroll(10, 10), "poisonous food"); */
			*ident = TRUE;
			break;
		}

		case SV_FOOD_SINGING_DRUNK:
		{
			/* not as important as STR, CON or spell stat, so more severe */
			clear_timed(TMD_SUPER_ROGUE);
			clear_timed(TMD_SHADOW);
			if (!p_ptr->timed[TMD_SUST_SPEED]) clear_timed(TMD_FAST);
            if (randint((badluck*2)+10) >= 20)
			{
               /* usually bypasses sustains */
               (void)do_dec_stat(A_DEX, 80 + badluck);
               (void)do_dec_stat(A_CHR, 80 + badluck);
            }
			else if (randint(badluck/2 + 10) > 4)
			{
               (void)do_dec_stat(A_DEX, 0);
               (void)do_dec_stat(A_CHR, 0);
            }
			else if (randint(100) < 60) (void)do_dec_stat(A_DEX, 10 + badluck);
			else (void)do_dec_stat(A_CHR, 15 + badluck);
			if (randint((badluck+1)/2 + 10) > 4)
			{
               msg_print("The mushroom screams for help as you eat it!");
               aggravate_monsters(0);
            }
			if ((randint((badluck+1)/2 + 10) > 5) && (!p_ptr->timed[TMD_SUST_SPEED]))
            {
               if (p_ptr->spadjust) p_ptr->spadjust -= randint(4);
               else
               {
                  p_ptr->spadjust = 0 - (1 + randint(2));
                  inc_timed(TMD_ADJUST, randint(25) + 25 + badluck);
               }
            }
			/* possibly lower charisma twice */
			if (randint(badluck/2 + 10) > 7) (void)do_dec_stat(A_CHR, 15 + badluck);
			/* ID-by use should not be a killer if you're careful to eat with near-full HP */
            if (p_ptr->lev < 15) take_hit(damroll(o_ptr->dd, o_ptr->ds-2), "poisonous food");
			else take_hit(damroll(o_ptr->dd, o_ptr->ds), "poisonous food");
			/* take_hit(damroll(3, 11), "poisonous food"); */
			*ident = TRUE;
			break;
		}

		case SV_FOOD_EMERGENCY:
		{
			int dur = randint(25 + goodluck/2) + 15 + goodluck/4;
			bool addemer = FALSE;
			(void)hp_player(200);
            if (inc_timed(TMD_OPP_FIRE, dur)) *ident = TRUE;
			if (inc_timed(TMD_OPP_COLD, dur)) *ident = TRUE;
			/* this hallucenation bypasses chaos resistance */
            if (inc_timed(TMD_IMAGE, randint(dur*3) + dur)) *ident = TRUE;
			if (dur > 41) dur = 41;
			/* additional emergencies (only helps with one of them) */
			if ((p_ptr->luck < 10) && (randint(100) < 60 + (40-(p_ptr->luck*2))))
			{
				p_ptr->luck += 6 + randint(20 - (p_ptr->luck+6));
				addemer = TRUE;
			}
			else if (p_ptr->silver >= PY_SILVER_LEVELTWO)
			{
				p_ptr->silver -= (5 + randint(6));
				addemer = TRUE;
			}
			else if (p_ptr->slime >= PY_SLIME_LEVELTWO)
			{
				p_ptr->slime -= (11 + randint(11));
				addemer = TRUE;
			}
			else if (p_ptr->corrupt > 45)
			{
				p_ptr->corrupt -= dur;
				addemer = TRUE;
			}
			else if (p_ptr->luck < 13) p_ptr->luck += 2;
			else if (p_ptr->slime >= PY_SLIME_LEVELONE) p_ptr->slime -= 2;
			else if (p_ptr->silver >= PY_SILVER_LEVELONE) p_ptr->silver -= 1;
			else if (p_ptr->corrupt > 15) p_ptr->corrupt -= 1;
			/* helping an additional emergency has an additional cost */
			if (addemer)
			{
				int dep = p_ptr->depth;
				if (!dep) dep = 1;
                if (summon_specific(p_ptr->py, p_ptr->px, dep, 0)) *ident = TRUE;
			}
			break;
		}

		case SV_FOOD_FIRST_SIGHT:
		{
			if (inc_timed(TMD_2ND_THOUGHT, randint(150) + 150)) *ident = TRUE;
			break;
		}

		case SV_FOOD_FAST_ATTACKS:
		{
			if (inc_timed(TMD_XATTACK, randint(115) + 75)) *ident = TRUE;
			break;
		}

		case SV_FOOD_MAD_FRENZY:
		{
			if (clear_timed(TMD_AFRAID)) *ident = TRUE;
			if (clear_timed(TMD_TERROR)) *ident = TRUE;
			if (inc_timed(TMD_FRENZY, randint(100) + 100)) *ident = TRUE;
			break;
		}

		case SV_FOOD_CURE_SERIOUS:
		{
			int cure = damroll(4, 8);
			int curep = (p_ptr->mhp * 20) / 100;
			if (cure < curep) cure = curep;
			if (hp_player(cure)) *ident = TRUE;
			if (p_ptr->slime >= PY_SLIME_LEVELONE) p_ptr->slime -= 2;
            else if (p_ptr->slime > PY_SLIME_HEALTHY) p_ptr->slime -= 1;
			if (p_ptr->silver > PY_SILVER_HEALTHY) p_ptr->silver -= 1;
			break;
		}

		case SV_FOOD_CLEAR_MIND:
		{
			if (clear_timed(TMD_CONFUSED)) *ident = TRUE;
			if (clear_timed(TMD_FRENZY)) *ident = TRUE;
			if (clear_timed(TMD_IMAGE)) *ident = TRUE;
			if (clear_timed(TMD_AFRAID)) *ident = TRUE;
			if (p_ptr->silver > PY_SILVER_HEALTHY) *ident = TRUE;
			if (p_ptr->silver > PY_SILVER_HEALTHY) p_ptr->silver -= 1 + randint(2);
			if (p_ptr->silver < PY_SILVER_HEALTHY) p_ptr->silver = PY_SILVER_HEALTHY;
			if (inc_timed(TMD_CLEAR_MIND, randint(30 + goodluck/2) + 10 + goodluck/4)) *ident = TRUE;
			if (do_res_stat(A_WIS)) *ident = TRUE;
			if (do_res_stat(A_INT)) *ident = TRUE;
			break;
		}

		case SV_FOOD_HEALTH:
		{
			if (clear_timed(TMD_POISONED)) *ident = TRUE;
			if (clear_timed(TMD_CUT)) *ident = TRUE;
			if (clear_timed(TMD_BLIND)) *ident = TRUE;
			if (inc_timed(TMD_OPP_POIS, randint(30 + goodluck/2) + 10 + goodluck/4)) *ident = TRUE;
			if (p_ptr->slime > PY_SLIME_HEALTHY) *ident = TRUE;
            if (p_ptr->slime > PY_SLIME_HEALTHY) p_ptr->slime -= 3 + randint(2);
			if (p_ptr->slime < PY_SLIME_HEALTHY) p_ptr->slime = PY_SLIME_HEALTHY;
			if (do_res_stat(A_STR)) *ident = TRUE;
			if (do_res_stat(A_CON)) *ident = TRUE;
			break;
		}

		case SV_FOOD_SNEAKINESS:
		{
			if (clear_timed(TMD_STUN)) *ident = TRUE;
			if (clear_timed(TMD_AMNESIA)) *ident = TRUE;
			if (clear_timed(TMD_CHARM)) *ident = TRUE;
			if (!p_ptr->timed[TMD_SUST_SPEED])
			{
				if (clear_timed(TMD_SLOW)) *ident = TRUE;
			}
			if (inc_timed(TMD_SUPER_ROGUE, randint(50 + goodluck/2) + 25 + goodluck/4)) *ident = TRUE;
			if (do_res_stat(A_DEX)) *ident = TRUE;
			if (do_res_stat(A_CHR)) *ident = TRUE;
			break;
		}


		case SV_FOOD_RATION:
		case SV_FOOD_BISCUIT:
		case SV_FOOD_JERKY:
		case SV_FOOD_SLIME_MOLD:
		{
			msg_print("That tastes good.");
			*ident = TRUE;
			break;
		}

		case SV_FOOD_WAYBREAD:
		{
			int cure, curep;
			msg_print("That tastes good and healthy.");
			(void)clear_timed(TMD_POISONED);
			cure = damroll(4, 8);
			curep = (p_ptr->mhp * 17) / 100;
			if (cure < curep) cure = curep;
			(void)hp_player(cure);
			if (p_ptr->silver > PY_SILVER_HEALTHY) p_ptr->silver -= 5;
			if (p_ptr->silver < PY_SILVER_HEALTHY) p_ptr->silver = PY_SILVER_HEALTHY;
            if (p_ptr->slime > PY_SLIME_HEALTHY) p_ptr->slime -= 5;
			if (p_ptr->slime < PY_SLIME_HEALTHY) p_ptr->slime = PY_SLIME_HEALTHY;
			*ident = TRUE;
			break;
		}

		case SV_FOOD_PINT_OF_ALE:
		case SV_FOOD_PINT_OF_WINE:
		{
			msg_print("That tastes good.");
			*ident = TRUE;
			break;
		}
	}

	/* Food can feed the player */
	(void)set_food(p_ptr->food + o_ptr->pval);

    /* Waybread is supposed to be the perfect food and shouldn't satiate so easily */
    if ((o_ptr->sval == SV_FOOD_WAYBREAD) && (goodluck > 0))
    {
       if (p_ptr->food > PY_FOOD_MAX) p_ptr->food = PY_FOOD_MAX;
    }
	
    /* notice changes of slime and silver poison levels */
    p_ptr->redraw |= (PR_SILVER);
    p_ptr->redraw |= (PR_SLIME);

	return (TRUE);
}


static bool quaff_potion(object_type *o_ptr, bool *ident)
{
	int time;

	/* Analyze the potion */
	switch (o_ptr->sval)
	{
		case SV_POTION_WATER:
		case SV_POTION_APPLE_JUICE:
		{
			if (p_ptr->prace == 16) msg_print("Some liquid flows down your clay throat to no effect.");
            else msg_print("You feel less thirsty.");
	        (void)set_food(p_ptr->food + o_ptr->pval);
			*ident = TRUE;
			break;
		}
		
		case SV_POTION_SLIME_MOLD: /* Pepsi */
		{
            if ((randint(100) < 4) && (p_ptr->luck < 35))
            {
               p_ptr->luck += 1;
               msg_print("This is good stuff.");
            }
            else
            {
				if (p_ptr->prace == 16) msg_print("Some liquid flows down your clay throat to no effect.");
				else msg_print("You feel less thirsty.");
            }
			*ident = TRUE;
			break;
		}

        case SV_POTION_FOUR_LEAF:
        {
            if (((p_ptr->luck == 38) || (p_ptr->luck == 39)) && (randint(100) < 50))
            {
                  p_ptr->luck += 1;
                  msg_print("This is good stuff.");
			      *ident = TRUE;
            }
			else if ((p_ptr->luck < 38) && (p_ptr->luck < p_ptr->maxluck - 1))
            {
                  if (p_ptr->luck < p_ptr->maxluck - 5) p_ptr->luck += 1 + randint(p_ptr->maxluck - p_ptr->luck);
				  else p_ptr->luck += 2;
                  if (p_ptr->luck < 20) msg_print("This is good stuff, you feel less unlucky.");
				  else msg_print("This is good stuff, you feel like you've found something that was lost.");
			      *ident = TRUE;
            }
            else if (p_ptr->luck < 38)
            {
                  p_ptr->luck += randint(2);
                  msg_print("This is good stuff.");
			      *ident = TRUE;
            }
			else if (p_ptr->prace == 16) msg_print("Some liquid flows down your clay throat to no effect.");
            else msg_print("You feel less thirsty.");
			break;
        }

		case SV_POTION_SLOWNESS:
		{
			if (p_ptr->timed[TMD_SUST_SPEED])
			{
				msg_print("Your pace falters for a moment, but doesn't change.");
				*ident = TRUE;
				break;
			}
			if (inc_timed(TMD_SLOW, randint(22) + 18)) *ident = TRUE;
			break;
		}

		case SV_POTION_SALT_WATER:
		{
			/* golems can neither eat, get hungry, nor get poisoned */
			if (p_ptr->prace == 16)
			{
				msg_print("This potion tastes salty..");
				*ident = TRUE;
				break;
			}
			msg_print("The potion makes you vomit!");
			(void)set_food(PY_FOOD_STARVE - 1);
			(void)clear_timed(TMD_POISONED);
			(void)inc_timed(TMD_PARALYZED, 4);
			*ident = TRUE;
			break;
		}

		case SV_POTION_POISON:
		{
			if (!(p_ptr->resist_pois || p_ptr->timed[TMD_OPP_POIS]))
			{
				if (inc_timed(TMD_POISONED, rand_int(15) + 10))
				{
                    if (randint(6) < (badluck + 3 - (goodluck/3)))
                    {
                       int sick = randint(6);
                       if (sick == 1) (void)do_dec_stat(A_STR, 0);
                       if (sick == 2) (void)do_dec_stat(A_CON, 0);
                       if (sick == 3) (void)do_dec_stat(A_DEX, 0);
                    }
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_BLINDNESS:
		{
			if ((p_ptr->resist_blind) && (!p_ptr->timed[TMD_BLIND]))
			{
                msg_print("Your vision dims for a moment and then clears.");
			    *ident = TRUE;
			}
			else if (!p_ptr->resist_blind)
			{
				inc_timed(TMD_BLIND, randint(180) + 120);
			    *ident = TRUE;
            }
			break;
		}

		case SV_POTION_CONFUSION:
		{
			if (!p_ptr->resist_confu)
			{
				if (inc_timed(TMD_CONFUSED, rand_int(24) + 12))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_POTION_SLEEP:
		{
			if (p_ptr->free_act)
			{
                msg_print("You feel sleepy for a moment, but the feeling passes.");
            }
			else
			{
				inc_timed(TMD_PARALYZED, rand_int(6) + 3);
			}
			*ident = TRUE;
			break;
		}

		/* effect largely copied from GF_TIME effect */
		case SV_POTION_MULTIHUED_POISON:
		{
			int sick;
			cptr act = NULL;

			/* message to let the player know why he didn't get the effect he was expecting.. */
			msg_print("Ack! It's a potion of Multi-hued Poison disguised as another potion!");
			if (inkey() == ESCAPE) ;/* do nothing (just a -more- to make sure player has a chance to read the message) */

			if (!(p_ptr->resist_pois || p_ptr->timed[TMD_OPP_POIS]))
			{
				inc_timed(TMD_POISONED, rand_int(15) + 10);
			}
			/* poison resistance or sustains won't protect from the drains */
			switch (randint(6))
			{
				case 1: sick = A_STR; act = "strong"; break;
				case 2: sick = A_INT; act = "bright"; break;
				case 3: sick = A_WIS; act = "wise"; break;
				case 4: sick = A_DEX; act = "agile"; break;
				case 5: sick = A_CON; act = "hale"; break;
				case 6: sick = A_CHR; act = "beautiful"; break;
			}
			msg_format("You're not as %s as you used to be...", act);
			/* lower the stat */
			if (badluck >= 3)
			{
				if (p_ptr->stat_cur[sick] >= 18+60) p_ptr->stat_cur[sick] = (p_ptr->stat_cur[sick] * 3) / 4;
				else if (p_ptr->stat_cur[sick] > 18+10) p_ptr->stat_cur[sick] = (p_ptr->stat_cur[sick] * 4) / 5;
                else p_ptr->stat_cur[sick] -= 2;
            }
			else
            {
				if (p_ptr->stat_cur[sick] >= 18+60) p_ptr->stat_cur[sick] = (p_ptr->stat_cur[sick] * 4) / 5;
				else if (p_ptr->stat_cur[sick] > 18+10) p_ptr->stat_cur[sick] = (p_ptr->stat_cur[sick] * 5) / 6;
                else p_ptr->stat_cur[sick] -= randint(2);
            }
			if (p_ptr->stat_cur[sick] < 3) p_ptr->stat_cur[sick] = 3;
			*ident = TRUE;
			break;
		}
		
		/* now called potion of mediocrity */
		/* has a similar (but more interesting) effect */
		case SV_POTION_RUINATION:
		{
			int i;
			for (i = 0; i < A_MAX; i++)
			{
				int target = 14;
				/* possible chance to raise a stat but never raise charisma */
				if (i == A_CHR)
				{
					target = 10;
					if (p_ptr->stat_cur[i] < target) break;
				}
				/* -25 or reduce to 14 */
				if (p_ptr->stat_cur[i] > target + 25)
				{
					p_ptr->stat_cur[i] -= 25;
				}
				else
				{
					p_ptr->stat_cur[i] = target;
				}
				/* lower max */
				if (p_ptr->stat_max[i] > target + 25)
				{
					p_ptr->stat_max[i] -= 25;
				}
				else
				{
					p_ptr->stat_max[i] = target;
				}
			}
			/* cancell all timed buffs */
	        (void)clear_timed(TMD_PROTEVIL);
	        (void)clear_timed(TMD_PROTEVIL2);
	        (void)clear_timed(TMD_HERO);
	        (void)clear_timed(TMD_SHERO);
	        (void)clear_timed(TMD_BLESSED);
	        (void)clear_timed(TMD_SANCTIFY);
	        (void)clear_timed(TMD_MIGHTY_HURL);
	        (void)clear_timed(TMD_DAYLIGHT);
	        (void)clear_timed(TMD_SKILLFUL);
	        (void)clear_timed(TMD_SHADOW);
	        (void)clear_timed(TMD_SUPER_ROGUE);
	        (void)clear_timed(TMD_2ND_THOUGHT);
	        (void)clear_timed(TMD_BECOME_LICH);
	        (void)clear_timed(TMD_BALROG);
	        (void)clear_timed(TMD_PROTDEAD);
	        (void)clear_timed(TMD_HOLDLIFE);
	        (void)clear_timed(TMD_STONESKIN);
	        (void)clear_timed(TMD_ESP);
	        (void)clear_timed(TMD_BRAIL);
	        (void)clear_timed(TMD_TSIGHT);
	        (void)clear_timed(TMD_SINVIS);
	        (void)clear_timed(TMD_SHIELD);
	        (void)clear_timed(TMD_FAST);
			msg_print("You feel boring and altogether unheroic.");
			*ident = TRUE;
			break;
		}

		case SV_POTION_DEC_STR:
		{
			if (do_dec_stat(A_STR, 0)) *ident = TRUE;
			break;
		}

		case SV_POTION_DEC_INT:
		{
			if (do_dec_stat(A_INT, 0)) *ident = TRUE;
			break;
		}

		case SV_POTION_DEC_WIS:
		{
			if (do_dec_stat(A_WIS, 0)) *ident = TRUE;
			break;
		}

		case SV_POTION_DEC_DEX:
		{
			if (do_dec_stat(A_DEX, 0)) *ident = TRUE;
			break;
		}

		case SV_POTION_DEC_CON:
		{
			if (do_dec_stat(A_CON, 0)) *ident = TRUE;
			break;
		}

		case SV_POTION_DEC_CHR:
		{
			if (do_dec_stat(A_CHR, 0)) *ident = TRUE;
			break;
		}

		case SV_POTION_DETONATIONS:
		{
			msg_print("Massive explosions rupture your body!");
			take_hit(damroll(99, 5), "a potion of Detonation");
			(void)inc_timed(TMD_STUN, 75);
			(void)inc_timed(TMD_CUT, 2000 + randint(2000));
			*ident = TRUE;
			break;
		}

		case SV_POTION_PURITY:
		{
			bool slime = FALSE;
            if (clear_timed(TMD_AMNESIA)) *ident = TRUE;
			if (clear_timed(TMD_IMAGE)) *ident = TRUE;
			if (p_ptr->silver > PY_SILVER_HEALTHY)
			{
                       p_ptr->silver = PY_SILVER_HEALTHY;
                       *ident = TRUE;
            }
            if (p_ptr->slime > PY_SLIME_HEALTHY)
			{
                       slime = TRUE;
                       p_ptr->slime = PY_SLIME_HEALTHY;
                       *ident = TRUE;
            }
            if (p_ptr->corrupt > 11) p_ptr->corrupt /= 3;
            else if (p_ptr->corrupt > 6) p_ptr->corrupt -= 6;
            else if (p_ptr->corrupt > 0) p_ptr->corrupt = 0;
			if (set_timed(TMD_STUN, (p_ptr->timed[TMD_STUN] / 2))) *ident = TRUE;
			if (set_timed(TMD_POISONED, (p_ptr->timed[TMD_POISONED] / 2))) *ident = TRUE;
			if (*ident == TRUE)
            {
               if (slime) msg_print("Your mind and body are purified.");
               else msg_print("Your mind is purified.");
            }
			else if (p_ptr->prace == 16) msg_print("Some liquid flows down your clay throat to no effect.");
			else msg_print("You feel less thirsty.");
			break;
		}

		case SV_POTION_INFRAVISION: /* alertness */
		{
			if (inc_timed(TMD_SINFRA, 100 + randint(100)))
			{
				*ident = TRUE;
			}
			break;
		}

		/* Super spellcasting */
        case SV_POTION_AUTO_BRAIL:
		{
			/* partially restores mana also */
            if (p_ptr->csp + (p_ptr->msp/10) + 1 < p_ptr->msp)
			{
                p_ptr->csp += randint(1 + (p_ptr->msp/10));
				msg_print("Your feel your head clear a little bit.");
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
				*ident = TRUE;
			}
            if (inc_timed(TMD_BRAIL, 72 + randint(84)))
			{
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_DETECT_INVIS: /* see invisible */
		{
			if (p_ptr->timed[TMD_2ND_THOUGHT])
			{
				msg_print("Your first sight supresses detection.");
				*ident = TRUE;
				break;
			}
			time = (p_ptr->skills[SKILL_DEV] + ((goodluck+1)/2)) / 2;
			if (time < 12) time = 12;
			if (time > 35) time = 34 + ((time - 34) / 2);
			if (inc_timed(TMD_SINVIS, time + randint(time)))
			{
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_DETECT_MON:
		{
            if ((p_ptr->timed[TMD_2ND_THOUGHT]) && (goodluck < 14))
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
				if (detect_monsters_normal(TRUE)) *ident = TRUE;
				if (detect_monsters_invis()) *ident = TRUE;
			}
			break;
		}

		case SV_POTION_TMP_BOOST:
		{
			int bonus = 0;
			if (!badluck) bonus = 1;
			bonus = (goodluck + bonus) * 10;
			if (inc_timed(TMD_TMPBOOST, 101 + randint(110 + bonus)))
			{
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_CURE_POISON:
		{
			if (clear_timed(TMD_POISONED)) *ident = TRUE;
			break;
		}

		case SV_POTION_DBREATH:
		{
			int dir;
			msg_print("Breathe in what direction?");
			*ident = TRUE;
			if (!get_aim_dir(&dir)) /* still gets used up */
			{
				msg_print("You have an unpleasant cough..");
				dir = 0;
			}
			if (randint(100) < 45) fire_ball(GF_COLD, dir, 80, 2);
			else fire_ball(GF_FIRE, dir, 80, 2);
			if (randint(100) < 65 + ((badluck+1)/2)) (void)set_food(p_ptr->food - (30 + badluck + randint(60)));
			break;
		}

		case SV_POTION_SPEED:
		{
			if (p_ptr->timed[TMD_SUST_SPEED])
			{
				msg_print("You cannot be hasted while your speed is sustained");
				break;
			}
			if ((!p_ptr->timed[TMD_FAST]) && (!p_ptr->timed[TMD_SUST_SPEED]))
			{
				if (set_timed(TMD_FAST, randint(22) + 18)) *ident = TRUE;
			}
			else
			{
				(void)inc_timed(TMD_FAST, 6);
			}
			break;
		}

		case SV_POTION_RESIST_ELEC_ACID:
		{
            int time = rand_int(12) + 12;
			if (inc_timed(TMD_OPP_ACID, time))
			{
				*ident = TRUE;
			}
			if (inc_timed(TMD_OPP_ELEC, time))
			{
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESIST_HEAT:
		{
			if (inc_timed(TMD_OPP_FIRE, rand_int(12) + 12))
			{
				*ident = TRUE;
			}
			break;
		}
		
        case SV_POTION_RESIST_POISON:
		{
			if ((rand_int(100) < (badluck - 1) * 2) &&
				(inc_timed(TMD_WOPP_POIS, randint(12) + 12)))
			{
				*ident = TRUE;
			}
			if (inc_timed(TMD_OPP_POIS, rand_int(12) + 12))
			{
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESIST_COLD:
		{
			if (inc_timed(TMD_OPP_COLD, rand_int(12) + 12))
			{
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_HEROISM:
		{
			if (hp_player(10)) *ident = TRUE;
			if (clear_timed(TMD_AFRAID)) *ident = TRUE;
            if (p_ptr->peace)
        	   {
                 msg_print("The peaceful magic prevents you from becoming heroic.");
                 *ident = TRUE;
                 break;
               }
			if (inc_timed(TMD_HERO, randint(26) + 24)) *ident = TRUE;
			break;
		}

		case SV_POTION_BERSERK_STRENGTH:
		{
			if (hp_player(30)) *ident = TRUE;
			if (clear_timed(TMD_AFRAID)) *ident = TRUE;
            if (p_ptr->peace)
        	   {
                 msg_print("The peaceful magic prevents you from going into a rage.");
                 *ident = TRUE;
               }
        	else if (p_ptr->timed[TMD_CHARM])
        	   {
                 msg_print("you're in too good a mood to go into a battle rage.");
                 *ident = TRUE;
               }
            else
               {
			     if (inc_timed(TMD_SHERO, randint(26) + 24)) *ident = TRUE;
               }
               
			break;
		}

		case SV_POTION_CURE_LIGHT:
		{
			int cure = damroll(2, 8);
			int curep = p_ptr->mhp / 10;
			if (cure < curep) cure = curep;
			if (hp_player(cure)) *ident = TRUE;
			if (clear_timed(TMD_BLIND)) *ident = TRUE;
			if (dec_timed(TMD_CUT, 10)) *ident = TRUE;
			break;
		}

		case SV_POTION_CURE_SERIOUS:
		{
			int cure = damroll(4, 8);
			int curep = (p_ptr->mhp * 18) / 100;
			if (cure < curep) cure = curep;
			if (hp_player(cure)) *ident = TRUE;
			if (clear_timed(TMD_BLIND)) *ident = TRUE;
			if (clear_timed(TMD_CONFUSED)) *ident = TRUE;
			if (set_timed(TMD_CUT, (p_ptr->timed[TMD_CUT] / 2) - 50)) *ident = TRUE;
			break;
		}

		case SV_POTION_CURE_CRITICAL:
		{
			int cure = damroll(6, 8);
			int curep = (p_ptr->mhp * 23) / 100;
			if (cure < curep) cure = curep;
			if (hp_player(cure)) *ident = TRUE;
			if (clear_timed(TMD_BLIND)) *ident = TRUE;
			if (clear_timed(TMD_CONFUSED)) *ident = TRUE;
			if (clear_timed(TMD_POISONED)) *ident = TRUE;
			if (clear_timed(TMD_STUN)) *ident = TRUE;
			if (clear_timed(TMD_CUT)) *ident = TRUE;
			if (clear_timed(TMD_AMNESIA)) *ident = TRUE;
			break;
		}

		case SV_POTION_HEALING:
		{
			int cure = 300;
			int curep = p_ptr->mhp / 2;
			if (cure < curep) cure = curep;
			if (hp_player(cure)) *ident = TRUE;
			if (clear_timed(TMD_BLIND)) *ident = TRUE;
			if (clear_timed(TMD_CONFUSED)) *ident = TRUE;
			if (clear_timed(TMD_POISONED)) *ident = TRUE;
			if (clear_timed(TMD_STUN)) *ident = TRUE;
			if (clear_timed(TMD_CUT)) *ident = TRUE;
			if (clear_timed(TMD_AMNESIA)) *ident = TRUE;
			if (p_ptr->silver > PY_SILVER_HEALTHY)
			{
				p_ptr->silver = p_ptr->silver - 4;
				*ident = TRUE;
			}
			if (p_ptr->silver < PY_SILVER_HEALTHY) p_ptr->silver = PY_SILVER_HEALTHY;
            if (p_ptr->slime > PY_SLIME_HEALTHY)
			{
				p_ptr->slime = p_ptr->slime - 6;
				*ident = TRUE;
			}
			if (p_ptr->slime < PY_SLIME_HEALTHY) p_ptr->slime = PY_SLIME_HEALTHY;
			break;
		}

		case SV_POTION_STAR_HEALING:
		{
			if (hp_player(1200)) *ident = TRUE;
			if (clear_timed(TMD_BLIND)) *ident = TRUE;
			if (clear_timed(TMD_CONFUSED)) *ident = TRUE;
			if (clear_timed(TMD_POISONED)) *ident = TRUE;
			if (clear_timed(TMD_STUN)) *ident = TRUE;
			if (clear_timed(TMD_CUT)) *ident = TRUE;
			if (clear_timed(TMD_AMNESIA)) *ident = TRUE;
			if (p_ptr->silver > PY_SILVER_HEALTHY) p_ptr->silver = p_ptr->silver - 15;
			if (p_ptr->silver < PY_SILVER_HEALTHY) p_ptr->silver = PY_SILVER_HEALTHY;
            if (p_ptr->slime > PY_SLIME_HEALTHY) p_ptr->slime = p_ptr->slime - 18;
			if (p_ptr->slime < PY_SLIME_HEALTHY) p_ptr->slime = PY_SLIME_HEALTHY;
			break;
		}

		case SV_POTION_LIFE:
		{
			(void)clear_timed(TMD_BECOME_LICH);
			msg_print("You feel life flow through your body!");
			restore_level();
			(void)clear_timed(TMD_POISONED);
			(void)clear_timed(TMD_BLIND);
			(void)clear_timed(TMD_CONFUSED);
			(void)clear_timed(TMD_AMNESIA);
			(void)clear_timed(TMD_IMAGE);
			(void)clear_timed(TMD_STUN);
			(void)clear_timed(TMD_CUT);
			if (p_ptr->silver > PY_SILVER_HEALTHY) p_ptr->silver = PY_SILVER_HEALTHY;
			if (p_ptr->slime > PY_SLIME_HEALTHY) p_ptr->slime = PY_SLIME_HEALTHY;
			(void)do_res_stat(A_STR);
			(void)do_res_stat(A_CON);
			(void)do_res_stat(A_DEX);
			(void)do_res_stat(A_WIS);
			(void)do_res_stat(A_INT);
			(void)do_res_stat(A_CHR);

			/* Recalculate max. hitpoints */
			update_stuff();

			hp_player(5000);

			*ident = TRUE;
			break;
		}

		case SV_POTION_RESTORE_MANA:
		{
			if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				msg_print("Your feel your head clear.");
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
				*ident = TRUE;
			}
			break;
		}

		case SV_POTION_RESTORE_EXP:
		{
			if (restore_level()) *ident = TRUE;
			if (p_ptr->silver > PY_SILVER_HEALTHY) p_ptr->silver = p_ptr->silver - 1;
			if (p_ptr->silver < PY_SILVER_HEALTHY) p_ptr->silver = PY_SILVER_HEALTHY;
			break;
		}

		case SV_POTION_RES_BRAWN:
		{
			if (do_res_stat(A_STR))
			{
				*ident = TRUE;
				/* try to pull free if being held */
				if ((p_ptr->timed[TMD_BEAR_HOLD]) &&
					(20 + adj_str_wgt[p_ptr->stat_ind[A_STR]] + goodluck/2 > randint(100)))
				{
					p_ptr->held_m_idx = 0;
					msg_print("You pull free.");
					clear_timed(TMD_BEAR_HOLD);
				}
			}
			if (do_res_stat(A_CON)) *ident = TRUE;
			break;
		}

		case SV_POTION_RES_INTELLECT:
		{
			if (do_res_stat(A_INT)) *ident = TRUE;
			if (do_res_stat(A_WIS)) *ident = TRUE;
			break;
		}

		case SV_POTION_RES_SNEAKINESS:
		{
			if (do_res_stat(A_DEX)) *ident = TRUE;
			if (do_res_stat(A_CHR)) *ident = TRUE;
			break;
		}

#if notremoved
		case SV_POTION_RES_WIS:
		{
			if (do_res_stat(A_WIS)) *ident = TRUE;
			break;
		}

		case SV_POTION_RES_DEX:
		{
			if (do_res_stat(A_DEX)) *ident = TRUE;
			break;
		}

		case SV_POTION_RES_CON:
		{
			if (do_res_stat(A_CON)) *ident = TRUE;
			break;
		}
#endif

		case SV_POTION_INC_STR:
		{
			int value;
			value = p_ptr->stat_cur[A_STR];
			if (value < 18+100)
			{
				if (do_inc_stat(A_STR)) *ident = TRUE;
			}
			else /* already at max */
			{
				(void)inc_timed(TMD_MIGHTY_HURL, randint(30) + 30);
			}
			/* try to pull free if being held */
			if ((p_ptr->timed[TMD_BEAR_HOLD]) &&
				(20 + adj_str_wgt[p_ptr->stat_ind[A_STR]] + goodluck/2 > randint(100)))
			{
				p_ptr->held_m_idx = 0;
				msg_print("You pull free.");
				clear_timed(TMD_BEAR_HOLD);
			}
			break;
		}

		case SV_POTION_INC_INT:
		{
			int value;
			value = p_ptr->stat_cur[A_INT];
			if (value < 18+100)
			{
				if (do_inc_stat(A_INT)) *ident = TRUE;
			}
			else /* already at max */
			{
				(void)inc_timed(TMD_CLEAR_MIND, randint(30) + 30);
				(void)inc_timed(TMD_BRAIL, randint(15) + 15);
			}
			break;
		}

		case SV_POTION_INC_WIS:
		{
			int value;
			value = p_ptr->stat_cur[A_WIS];
			if (value < 18+100)
			{
				if (do_inc_stat(A_WIS)) *ident = TRUE;
			}
			else /* already at max */
			{
				(void)inc_timed(TMD_TSIGHT, randint(30) + 30);
				if (p_ptr->silver > PY_SILVER_HEALTHY) p_ptr->silver -= 1;
			}
			break;
		}

		case SV_POTION_INC_DEX:
		{
			int value;
			value = p_ptr->stat_cur[A_DEX];
			if (value < 18+100)
			{
				if (do_inc_stat(A_DEX)) *ident = TRUE;
			}
			else /* already at max */
			{
				int time = randint(30) + 30;
				(void)inc_timed(TMD_SUST_SPEED, time);
				(void)inc_timed(TMD_WSHIELD, time); /* for dodge ac */
			}
			break;
		}

		case SV_POTION_INC_CON:
		{
			int value;
			value = p_ptr->stat_cur[A_CON];
			if (value < 18+100)
			{
				if (do_inc_stat(A_CON)) *ident = TRUE;
			}
			else /* already at max */
			{
				(void)inc_timed(TMD_OPP_POIS, randint(30) + 30);
				if (p_ptr->slime > PY_SLIME_HEALTHY + 3) p_ptr->slime -= 3;
				else if (p_ptr->slime > PY_SLIME_HEALTHY + 3) p_ptr->slime = PY_SLIME_HEALTHY;
			}
			break;
		}

		case SV_POTION_INC_CHR:
		{
			int value;
			value = p_ptr->stat_cur[A_CHR];
			if (value < 18+100)
			{
				if (do_inc_stat(A_CHR)) *ident = TRUE;
			}
			else /* already at max */
			{
				(void)inc_timed(TMD_SPHERE_CHARM, randint(30) + 30);
				if ((randint(100) < 33) && (p_ptr->luck < 40))
				{
					p_ptr->luck += 1;
					msg_print("You feel lucky");
				}
			}
			break;
		}

		case SV_POTION_AUGMENTATION:
		{
			if (do_inc_stat(A_STR)) *ident = TRUE;
			if (do_inc_stat(A_INT)) *ident = TRUE;
			if (do_inc_stat(A_WIS)) *ident = TRUE;
			if (do_inc_stat(A_DEX)) *ident = TRUE;
			if (do_inc_stat(A_CON)) *ident = TRUE;
			if (do_inc_stat(A_CHR)) *ident = TRUE;

			/* all maxxed out */
			if (*ident == FALSE)
			{
				int time = randint(35) + 35;
				(void)inc_timed(TMD_HOLDLIFE, time);
				(void)inc_timed(TMD_ESP, time);
				(void)inc_timed(TMD_FAST, time);
				(void)inc_timed(TMD_INVULN, time/7);
			}
			break;
		}

		case SV_POTION_ENLIGHTENMENT:
		{
			msg_print("An image of your surroundings forms in your mind...");
			wiz_lite();
			*ident = TRUE;
			break;
		}

		case SV_POTION_STAR_ENLIGHTENMENT:
		{
			msg_print("You begin to feel more enlightened...");
			message_flush();
			wiz_lite();
			(void)do_inc_stat(A_INT);
			(void)do_inc_stat(A_WIS);
			(void)detect_traps();
			(void)detect_doorstairs(FALSE);
			(void)detect_treasure();
			(void)detect_objects_normal(TRUE);
			identify_pack();
			self_knowledge(TRUE);
			*ident = TRUE;
			break;
		}

		case SV_POTION_SELF_KNOWLEDGE:
		{
			msg_print("You begin to know yourself a little better...");
			if (p_ptr->silver > PY_SILVER_HEALTHY) p_ptr->silver = p_ptr->silver - 3;
			if (p_ptr->silver < PY_SILVER_HEALTHY) p_ptr->silver = PY_SILVER_HEALTHY;
			message_flush();
			identify_pack();
			self_knowledge(TRUE);
			*ident = TRUE;
			break;
		}

		case SV_POTION_EXPERIENCE:
		{
			if (p_ptr->exp < PY_MAX_EXP)
			{
				s32b ee = (p_ptr->exp / 2) + 10;
				if (ee > 100000L) ee = 100000L;
				msg_print("You feel more experienced.");
				gain_exp(ee);
				*ident = TRUE;
			}
			break;
		}
	}

	/* !multi-hued poison uses pval for something else */
	if (o_ptr->sval != SV_POTION_MULTIHUED_POISON)
	{
		/* Most potions also give slight nourishment */
		(void)set_food(p_ptr->food + o_ptr->pval);
	}

    /* notice changes of slime and silver poison levels */
    p_ptr->redraw |= (PR_SILVER);
    p_ptr->redraw |= (PR_SLIME);

	return (TRUE);
}


static bool read_scroll(object_type *o_ptr, bool *ident)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
	int idagain;

	int k;

	bool used_up = TRUE;

    /* special for treasure map */
    if ((o_ptr->tval == TV_SPECIAL) && (o_ptr->sval == SV_TREASURE))
    {
       if (p_ptr->find_vault > 5)
       {
          msg_print("You are already seeking a vault.");
          used_up = FALSE;
       }

       p_ptr->find_vault = goodluck + 25 + randint(15 + (goodluck+1)/2);

       if ((goodluck < 4) && (badluck < 13) && (randint(7) == 1))
       {
            p_ptr->luck += 1;
            p_ptr->find_vault += 14 + randint(p_ptr->lev/3);
            msg_print("You feel lucky.");
       }
       msg_print("You learn the way through the maze of stairs.");

       /* if you're lucky it also detects stairs */
       if (randint(p_ptr->find_vault + 7 + (goodluck/2)) > 38) (void)detect_doorstairs(TRUE);

       *ident = TRUE;
       return (used_up);
    }

	/* Analyze the scroll */
	switch (o_ptr->sval)
	{
		case SV_SCROLL_DARKNESS:
		{
			if (!p_ptr->resist_blind)
			{
				(void)inc_timed(TMD_BLIND, 3 + randint(5));
			}
			if (unlite_area(10, 3, FALSE)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_AGGRAVATE_MONSTER:
		{
            if ((p_ptr->peace) && (randint(20) < (goodluck * 2)))
            {
               msg_print("There is a muffled high pitched squeak.");
            }
            else
            {
			   msg_print("There is a high pitched humming noise.");
			   aggravate_monsters(0);
            }
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_CURSE_ARMOR:
		{
            if ((goodluck > 16) && (randint(100) < 20))
            {
               msg_print("You feel as if you just escaped a nasty curse..");
               break; /* (very lucky) */
            }
			if (curse_armor()) *ident = TRUE;
			break;
		}

		case SV_SCROLL_CURSE_WEAPON:
		{
            int badness = randint(2) + 1; /* 2 or 3 */
            if ((goodluck > 16) && (randint(100) < 20))
            {
               msg_print("You feel as if you just escaped a nasty curse..");
               break; /* (very lucky) */
            }
            if ((badluck > 15) && (badness < 3)) badness += 2;
            else if (badluck > 5) badness += 1;
            else if ((goodluck > 15) && (badness > 2)) badness -= 2;
            else if (goodluck > 5) badness -= 1;
			if (curse_weapon(badness)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_SUMMON_MONSTER:
		{
			sound(MSG_SUM_MONSTER);
			for (k = 0; k < randint(3); k++)
			{
				int dep = p_ptr->depth;
				if (!dep) dep = 1;
                if (summon_specific(py, px, dep, 0))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_SCROLL_SUMMON_UNDEAD:
		{
			sound(MSG_SUM_UNDEAD);
			for (k = 0; k < randint(3); k++)
			{
				int dep = p_ptr->depth;
				if (dep < 3) dep = 3;
				if (summon_specific(py, px, dep, SUMMON_UNDEAD))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_SCROLL_TRAP_CREATION:
		{
			if (trap_creation()) *ident = TRUE;
			/* EFGchange learn flavor of trap creation */
			/* I have never ever seen that return true */
			msg_print("You hear the floor shifting.");
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_PHASE_DOOR:
		{
			bool controlled = FALSE;
            /* controlled teleport (random if you don't chose a target) */
		    if (p_ptr->telecontrol)
		    {
                if (control_tport(100, 12)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(10);
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_TELEPORT:
		{
			bool controlled = FALSE;
            /* controlled teleport (random if you don't chose a target) */
		    if (p_ptr->telecontrol)
		    {
                if (control_tport(0, 150)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(100);
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_TELEPORT_LEVEL:
		{
			(void)teleport_player_level();
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_WORD_OF_RECALL:
		{
			set_recall(rand_int(20) + 15);
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_IDENTIFY:
		{
			*ident = TRUE;
			if (!ident_spell()) used_up = FALSE;
			/* chance for it not to be used up */
			/* in theory a scroll of identify could be used an infinite */
			/* number of times if you get very lucky */
			idagain = (p_ptr->skills[SKILL_DEV] + goodluck) / 2;
			if ((goodluck == 1) || (goodluck == 11)) idagain += 1;
			if (goodluck > 11) idagain += randint(goodluck - 10);
			if ((randint(idagain) > 18) && (badluck < 12) && (used_up))
			{
			   msg_print("The writing on the scroll doesn't dissapear!");
               used_up = FALSE;

			    /* Take a turn (because otherwise */
				/* the energy only gets used if the scroll gets used up */
				p_ptr->energy_use = 100;
            }
			break;
		}

		case SV_SCROLL_STAR_IDENTIFY:
		{
			*ident = TRUE;
			if (!identify_fully()) used_up = FALSE;
			/* chance for it not to be used up (much rarer than normal ?ID) */
			/* in theory a scroll of identify could be used an infinite */
			/* number of times if you get very lucky */
			idagain = p_ptr->skills[SKILL_DEV] + (goodluck/2);
			if ((randint(idagain) > 75) && (badluck < 7))
			{
			   msg_print("The writing on the scroll doesn't dissapear!");
               used_up = FALSE;

			    /* Take a turn (because otherwise */
				/* the energy only gets used if the scroll gets used up */
				p_ptr->energy_use = 100;
            }
			break;
		}

		case SV_SCROLL_REMOVE_CURSE:
		{
			if (remove_curse())
			{
				msg_print("You feel as if someone is watching over you.");
				*ident = TRUE;
			}
			break;
		}

		case SV_SCROLL_STAR_REMOVE_CURSE:
		{
			remove_all_curse();
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_ENCHANT_ARMOR:
		{
			*ident = TRUE;
			if (!enchant_spell(0, 0, 1, FALSE, FALSE)) used_up = FALSE;
			break;
		}

		case SV_SCROLL_ENCHANT_WEAPON_TO_HIT:
		{
			if (!enchant_spell(1, 0, 0, FALSE, FALSE)) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_ENCHANT_WEAPON_TO_DAM:
		{
			if (!enchant_spell(0, 1, 0, FALSE, FALSE)) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ENCHANT_ARMOR:
		{
			if (!enchant_spell(0, 0, randint(3) + 2, FALSE, TRUE)) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ENCHANT_WEAPON:
		{
			if (!enchant_spell(randint(3), randint(3), 0, FALSE, TRUE)) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_RECHARGING:
		{
			if (!recharge(60)) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_LIGHT:
		{
			if (lite_area(damroll(2, 8), 2)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_MAPPING:
		{
			map_area();
			*ident = TRUE;
			break;
		}
		
		case SV_SCROLL_DEEP_DESCENT:
		{
			(void)deep_descent();
			*ident = TRUE;
			break;
        }

        /* detect gold removed */
/*		case SV_SCROLL_DETECT_GOLD:
		{
			if (detect_treasure()) *ident = TRUE;
			break;
		} */

		case SV_SCROLL_DETECT_ITEM:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
               *ident = TRUE;
            }
            else
            {
			  if (detect_treasure()) *ident = TRUE;
			  if (detect_objects_normal(FALSE)) *ident = TRUE;
            }
			break;
		}

		case SV_SCROLL_DETECT_TRAP:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
               *ident = TRUE;
            }
            else
            {
			   if (detect_traps()) *ident = TRUE;
            }
			break;
		}

		case SV_SCROLL_DETECT_DOOR:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
               *ident = TRUE;
            }
            else
            {
			  if (detect_doorstairs(FALSE)) *ident = TRUE;
            }
			break;
		}

		case SV_SCROLL_DETECT_INVIS:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  if (detect_monsters_invis()) *ident = TRUE;
            }
			break;
		}

		case SV_SCROLL_SATISFY_HUNGER:
		{
			if (set_food(PY_FOOD_MAX - 1)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_BLESSING:
		{
			if (inc_timed(TMD_BLESSED, randint(12) + 6)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_HOLY_CHANT:
		{
			if (inc_timed(TMD_BLESSED, randint(19) + 17)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_HOLY_PRAYER:
		{
			if (inc_timed(TMD_BLESSED, randint(42) + 30)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_MONSTER_CONFUSION:
		{
			if (p_ptr->confusing == 0)
			{
				msg_print("Your hands begin to glow.");
				p_ptr->confusing = TRUE;
				*ident = TRUE;
			}
			break;
		}
		
		case SV_SCROLL_BRAND_WEAPON:
		{
			brand_weapon(3);
			break;
		}

		case SV_SCROLL_PROTECTION_FROM_EVIL:
		{
			k = 3 * p_ptr->lev;
			if (inc_timed(TMD_PROTEVIL, randint(25) + k)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_RUNE_OF_PROTECTION:
		{
			if (!warding_glyph()) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_TRAP_DOOR_DESTRUCTION:
		{
			if (destroy_doors_touch()) *ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_DESTRUCTION:
		{
			destroy_area(py, px, 15, TRUE);
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_DISPEL_UNDEAD:
		{
			if (dispel_undead(60)) *ident = TRUE;
			break;
		}

		case SV_SCROLL_BANISHMENT:
		{
			if (!banishment()) used_up = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_MASS_BANISHMENT:
		{
			(void)mass_banishment();
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_ACQUIREMENT:
		{
			acquirement(py, px, 1, TRUE);
			*ident = TRUE;
			break;
		}

		case SV_SCROLL_STAR_ACQUIREMENT:
		{
			acquirement(py, px, randint(2) + 1, TRUE);
			*ident = TRUE;
			break;
		}
	}

	return (used_up);
}


static bool use_staff(object_type *o_ptr, bool *ident)
{
	int py = p_ptr->py;
	int px = p_ptr->px;
    bool casted;

	int k, dir;

	bool use_charge = TRUE;

	/* Analyze the staff */
	switch (o_ptr->sval)
	{
		case SV_STAFF_DARKNESS:
		{
			bool strongdark = FALSE;
			/* occationally use GF_DARK instead of DARK_WEAK */
			if ((randint(p_ptr->skills[SKILL_DEV] + (goodluck*2)) > 60) &&
				(cp_ptr->spell_book != TV_PRAYER_BOOK)) strongdark = TRUE;
			if (!p_ptr->resist_blind)
			{
				(void)inc_timed(TMD_BLIND, 3 + randint(6));
			}
			if (unlite_area(10, 3, strongdark)) *ident = TRUE;
			break;
		}

		case SV_STAFF_STRIKING:
		{
			*ident = TRUE;
  	        if (!get_aim_dir(&dir))
  	        {
	           use_charge = FALSE;
               return (FALSE);
            }
			fire_bolt(GF_MISSILE, dir, damroll(3, 9));
			break;
        }

#if blah
		case SV_STAFF_SLOWNESS:
		{
			if (p_ptr->timed[TMD_SUST_SPEED])
			{
				msg_print("Your pace falters for a moment, but doesn't change.");
			}
			if (inc_timed(TMD_SLOW, randint(25) + 17)) *ident = TRUE;
			break;
		}
#endif

		case SV_STAFF_ZAPPING:
		{
			if (inc_timed(TMD_ZAPPING, randint(15) + 30 + (goodluck/2))) *ident = TRUE;
			break;
		}
		
		case SV_STAFF_CHARM_ANIMAL:
        {
			if (inc_timed(TMD_SPHERE_CHARM, randint(60 + goodluck) + 40)) *ident = TRUE;
			break;
        }

		case SV_STAFF_HASTE_MONSTERS:
		{
			if (speed_monsters()) *ident = TRUE;
			break;
		}

		case SV_STAFF_SUMMONING:
		{
			sound(MSG_SUM_MONSTER);
			for (k = 0; k < randint(4); k++)
			{
				if (summon_specific(py, px, p_ptr->depth, 0))
				{
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_STAFF_TELEPORTATION:
		{
			bool controlled = FALSE;
            /* controlled teleport (random if you don't chose a target) */
		    if (p_ptr->telecontrol)
		    {
                if (control_tport(0, 150)) controlled = TRUE;
                if (!controlled) msg_print("You fail to control the teleportation.");
            }

            if (!controlled) teleport_player(100);
			*ident = TRUE;
			break;
		}

		case SV_STAFF_IDENTIFY:
		{
			if (!ident_spell()) use_charge = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_STAFF_REMOVE_CURSE:
		{
			if (remove_curse())
			{
				if (!p_ptr->timed[TMD_BLIND])
				{
					msg_print("The staff glows blue for a moment...");
				}
				*ident = TRUE;
			}
			break;
		}

		case SV_STAFF_STARLITE:
		{
			bool strong = FALSE;
			if (randint(p_ptr->skills[SKILL_DEV] + goodluck) > 65)
				strong = TRUE;
			if (!p_ptr->timed[TMD_BLIND])
			{
				if (strong) msg_print("The end of the staff shines with blinding light...");
				else msg_print("The end of the staff glows brightly...");
			}
			if (strong) for (k = 0; k < 8; k++) strong_lite_line(ddd[k]);
			else for (k = 0; k < 8; k++) lite_line(ddd[k]);
			/* a drawback of the strong effect (only if you have bad luck) */
			if ((badluck) && (!goodluck) && (strong) && (!p_ptr->timed[TMD_BLIND]) &&
				(rand_int(100) < (badluck+1)*4))
			{
				(void)inc_timed(TMD_BLIND, randint(4 + badluck/2) + 2);
			}
			*ident = TRUE;
			break;
		}

		case SV_STAFF_LITE:
		{
			if (lite_area(damroll(2, 8), 2)) *ident = TRUE;
			break;
		}

		case SV_STAFF_MAPPING:
		{
            if ((p_ptr->timed[TMD_2ND_THOUGHT]) && (goodluck < 14))
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  map_area();
            }
			*ident = TRUE;
			break;
		}

/*		case SV_STAFF_DETECT_GOLD:
		{
			break;
		} combined with object detection */

		case SV_STAFF_DETECT_ITEM:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
               *ident = TRUE;
            }
            else
            {
			  if (detect_objects_normal(FALSE)) *ident = TRUE;
			  if (detect_treasure()) *ident = TRUE;
            }
			break;
		}

		case SV_STAFF_TELEKINESIS:
		{
			*ident = TRUE;
			/* pick up objects from a distance (in cmd2.c) */
			if (!do_telekinesis(9 + goodluck/2))
			{
				/* returns FALSE only if no direction was entered */
				use_charge = FALSE;
            }
			break;
		}

		case SV_STAFF_DETECT_TRAP:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
               *ident = TRUE;
            }
            else
            {
			  if (detect_traps()) *ident = TRUE;
            }
			break;
		}

		case SV_STAFF_DETECT_DOOR:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
               *ident = TRUE;
            }
            else
            {
			  if (detect_doorstairs(FALSE)) *ident = TRUE;
            }
			break;
		}

		case SV_STAFF_DETECT_INVIS:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
               *ident = TRUE;
            }
            else
            {
			  if (detect_monsters_invis()) *ident = TRUE;
            }
			break;
		}

		case SV_STAFF_DETECT_EVIL:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
               *ident = TRUE;
            }
            else
            {
			  if (detect_monsters_evil()) *ident = TRUE;
            }
			break;
		}

		case SV_STAFF_CURE_LIGHT:
		{
			int cure;
			if (p_ptr->mhp / 20 >= 7) cure = damroll(2, p_ptr->mhp / 20);
			else if (p_ptr->mhp < 96) cure = randint(8);
			else cure = randint(p_ptr->mhp / 11);
			if (hp_player(cure)) *ident = TRUE;
			break;
		}

		case SV_STAFF_CURING:
		{
			if (clear_timed(TMD_BLIND)) *ident = TRUE;
			if (clear_timed(TMD_POISONED)) *ident = TRUE;
			if (clear_timed(TMD_CONFUSED)) *ident = TRUE;
			if (clear_timed(TMD_STUN)) *ident = TRUE;
			if (clear_timed(TMD_CUT)) *ident = TRUE;
			if (rand_int(25) < 5 + goodluck*2)
			{
				if (randint(100) < 35) { if (clear_timed(TMD_AMNESIA)) *ident = TRUE; }
				if (p_ptr->silver > PY_SILVER_HEALTHY)
				{
					p_ptr->silver -= 1;
					if (p_ptr->silver < PY_SILVER_HEALTHY) p_ptr->silver = PY_SILVER_HEALTHY;
					*ident = TRUE;
				}
            	if (p_ptr->slime > PY_SLIME_HEALTHY)
				{
					p_ptr->slime -= 2;
					if (p_ptr->slime < PY_SLIME_HEALTHY) p_ptr->slime = PY_SLIME_HEALTHY;
					*ident = TRUE;
				}
			}
			break;
		}

		case SV_STAFF_HEALING:
		{
			if (hp_player(300)) *ident = TRUE;
			if (clear_timed(TMD_STUN)) *ident = TRUE;
			if (clear_timed(TMD_CUT)) *ident = TRUE;
			break;
		}

		case SV_STAFF_THE_MAGI:
		{
			if (do_res_stat(A_INT)) *ident = TRUE;
			if (p_ptr->csp < p_ptr->msp)
			{
				p_ptr->csp = p_ptr->msp;
				p_ptr->csp_frac = 0;
				*ident = TRUE;
				msg_print("Your feel your head clear.");
				p_ptr->redraw |= (PR_MANA);
				p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
			}
			break;
		}

		case SV_STAFF_SLEEP_MONSTERS:
		{
            int pwr = p_ptr->lev + 2 + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
			if (sleep_monsters(pwr)) *ident = TRUE;
			break;
		}

		case SV_STAFF_SLOW_MONSTERS:
		{
            int pwr = p_ptr->lev + 2 + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
			if (slow_monsters(pwr)) *ident = TRUE;
			break;
		}

		case SV_STAFF_SPEED:
		{
			if (p_ptr->timed[TMD_SUST_SPEED])
			{
				msg_print("You cannot be hasted while your speed is sustained");
				break;
			}
			if ((!p_ptr->timed[TMD_FAST]) && (!p_ptr->timed[TMD_SUST_SPEED]))
			{
				if (set_timed(TMD_FAST, randint(25) + 17)) *ident = TRUE;
			}
			else if (!p_ptr->timed[TMD_SUST_SPEED])
			{
				(void)inc_timed(TMD_FAST, 6);
			}
			break;
		}

		case SV_STAFF_PROBING:
		{
			probing();
			*ident = TRUE;
			break;
		}

		case SV_STAFF_DISPEL_EVIL:
		{
			if (dispel_evil(60)) *ident = TRUE;
			break;
		}

		case SV_STAFF_POWER:
		{
			if (dispel_monsters(120)) *ident = TRUE;
			break;
		}

		case SV_STAFF_HOLINESS:
		{
			if (dispel_evil(120)) *ident = TRUE;
			k = 3 * p_ptr->lev;
			if (inc_timed(TMD_PROTEVIL, randint(25) + k)) *ident = TRUE;
			if (clear_timed(TMD_POISONED)) *ident = TRUE;
			if (clear_timed(TMD_AFRAID)) *ident = TRUE;
			if (hp_player(50)) *ident = TRUE;
			if (clear_timed(TMD_STUN)) *ident = TRUE;
			if (clear_timed(TMD_CUT)) *ident = TRUE;
			break;
		}

		case SV_STAFF_BANISHMENT:
		{
			if (!banishment()) use_charge = FALSE;
			*ident = TRUE;
			break;
		}

		case SV_STAFF_EARTHQUAKES:
		{
			earthquake(py, px, 10, 80, 0, FALSE);
			*ident = TRUE;
			break;
		}
		
		case SV_STAFF_MANAFREE:
		{
			if (!cp_ptr->spell_book)
	        {
		       msg_print("You feel as if you're missing out on something..");
			   break;
	        }
            p_ptr->manafree = 1;
	        if (cp_ptr->spell_book == TV_PRAYER_BOOK)
		       casted = do_cmd_pray();
	        else if (cp_ptr->spell_book == TV_NEWM_BOOK)
		         casted = do_cmd_castnew();
	        else if (cp_ptr->spell_book == TV_LUCK_BOOK)
		         casted = do_cmd_castluck();
	        else if (cp_ptr->spell_book == TV_CHEM_BOOK)
		         casted = do_cmd_castchem();
 	        else if (cp_ptr->spell_book == TV_DARK_BOOK)
		         casted = do_cmd_castblack();
	        else casted = do_cmd_cast();
	        
            if (!casted)
            {
               p_ptr->manafree = 0;
               use_charge = FALSE;
            }
            /* figures how many staff charges to use in */
            /* do_cmd_use_staff() in cmd6.c */
            
            *ident = TRUE;
			break;
		}

		case SV_STAFF_DESTRUCTION:
		{
			destroy_area(py, px, 15, TRUE);
			*ident = TRUE;
			break;
		}
	}
	
    return (use_charge);
}

/* takes sval instead of o_ptr to allow for the mimmic wand spell */
bool wand_effect(int sval, bool *ident, int enhanced, int dir)
{
	int die, pwr, dis;
	int luckdev = (p_ptr->skills[SKILL_DEV] + goodluck + p_ptr->lev) / 2;

	/* Not identified yet */
	*ident = FALSE;

	/* XXX Hack -- Wand of wonder can do anything before it */
	if (sval == SV_WAND_WONDER)
	{
        sval = rand_int(SV_WAND_WONDER);
        if ((goodluck > 9) && (randint(100) < 3)) sval += 1;
        if ((goodluck > 14) && (randint(2550 - (goodluck*105)) == 3)) sval += randint(4);
        if ((goodluck > 9) && (sval < SV_WAND_CLONE_MONSTER)) sval += randint(15);
        if (sval >= SV_WAND_WONDER) msg_print("You feel a lucky surge of power!");
    }

	/* Analyze the wand */
	switch (sval)
	{
		case SV_WAND_HEAL_MONSTER:
		{
			if (heal_monster(dir, damroll(4, 6) + enhanced)) *ident = TRUE;
			break;
		}

		case SV_WAND_HASTE_MONSTER:
		{
			if (speed_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_CLONE_MONSTER:
		{
			if (clone_monster(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_TELEPORT_AWAY:
		{
			int dis = p_ptr->skills[SKILL_DEV] + goodluck + randint(p_ptr->skills[SKILL_DEV] + 2);
            if (teleport_monster(dir, dis)) *ident = TRUE;
			break;
		}

		case SV_WAND_DISARMING:
		{
			if (disarm_trap(dir, 0)) *ident = TRUE;
			break;
		}

 		case SV_WAND_TUNNELDIGGER:
		{
			/* spellswitch 31 makes it not stop at the first wall */
			spellswitch = 31;
            if (wall_to_mud(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_STONE_TO_MUD:
		{
			if (wall_to_mud(dir)) *ident = TRUE;
			break;
		}

		case SV_WAND_LITE:
		{
			if ((randint(p_ptr->skills[SKILL_DEV] + goodluck) > 70 - enhanced))
			{
				msg_print("A powerful beam of white light appears.");
				strong_lite_line(dir);
			}
			else
			{
				msg_print("A line of blue shimmering light appears.");
				lite_line(dir);
			}
			*ident = TRUE;
			break;
		}

		case SV_WAND_SLEEP_MONSTER:
		{
            pwr = p_ptr->lev + adj_chr_charm[p_ptr->stat_ind[A_CHR]] + enhanced;
			if (randint(p_ptr->skills[SKILL_DEV] + goodluck + enhanced) > 70)
				pwr += 7;
			if (sleep_monster(dir, pwr)) *ident = TRUE;
			break;
		}

		case SV_WAND_SLOW_MONSTER:
		{
            pwr = p_ptr->lev + adj_chr_charm[p_ptr->stat_ind[A_CHR]] + enhanced;
			if (randint(p_ptr->skills[SKILL_DEV] + goodluck + enhanced) > 70)
				pwr += 7;
			if (slow_monster(dir, pwr)) *ident = TRUE;
			break;
		}

		case SV_WAND_CONFUSE_MONSTER:
		{
			pwr = (p_ptr->lev + 10) / 2;
            pwr += adj_chr_charm[p_ptr->stat_ind[A_CHR]] + enhanced;
			if (randint(p_ptr->skills[SKILL_DEV] + goodluck + enhanced) > 70)
				pwr += 7;
			if (confuse_monster(dir, pwr)) *ident = TRUE;
			break;
		}

		case SV_WAND_FEAR_MONSTER:
		{
            pwr = (p_ptr->lev + 13) / 2;
            pwr += adj_chr_charm[p_ptr->stat_ind[A_CHR]] + enhanced;
			if (randint(p_ptr->skills[SKILL_DEV] + goodluck + enhanced) > 70)
				pwr += 7;
			/* sometimes fire a ball instead */
			if (randint(p_ptr->skills[SKILL_DEV] + goodluck + enhanced) > 70)
			{
				if (fire_ball(GF_TURN_ALL, dir, pwr, 2)) *ident = TRUE;
			}
			else
			{
				if (fear_monster(dir, pwr)) *ident = TRUE;
			}
			break;
		}

		case SV_WAND_DRAIN_LIFE:
		{
			pwr = 150;
			if (randint(p_ptr->skills[SKILL_DEV] + goodluck) > 70)
				pwr += randint(goodluck + 2);
			if (pwr + enhanced < 200) pwr += enhanced;
			else if (enhanced) pwr += (pwr + enhanced - 198) / 4;
			if (drain_life(dir, pwr)) *ident = TRUE;
			break;
		}

		case SV_WAND_POLYMORPH:
		{
			pwr = p_ptr->lev + enhanced;
			if (randint(p_ptr->skills[SKILL_DEV] + goodluck + enhanced) > 70)
				pwr += randint(goodluck + 3);
			if (poly_monster(dir, pwr)) *ident = TRUE;
			break;
		}

		case SV_WAND_STINKING_CLOUD:
		{
			pwr = 12 + enhanced;
			if (randint(luckdev) > 60)
				pwr += randint(p_ptr->skills[SKILL_DEV]/2);
			fire_ball(GF_POIS, dir, pwr, 2);
			*ident = TRUE;
			break;
		}

		case SV_WAND_MAGIC_MISSILE:
		{
			pwr = damroll(3, 4) + enhanced;
			if (randint(luckdev) > 60)
				pwr += randint(p_ptr->skills[SKILL_DEV]/2);
			fire_bolt_or_beam(20, GF_MISSILE, dir, pwr);
			*ident = TRUE;
			break;
		}

		case SV_WAND_ACID_BOLT:
		{
			pwr = damroll(10, 8) + enhanced;
			if (randint(luckdev) > 65)
				pwr += randint(p_ptr->skills[SKILL_DEV]/2 + 1);
			fire_bolt_or_beam(20, GF_ACID, dir, pwr);
			*ident = TRUE;
			break;
		}

		case SV_WAND_ELEC_BOLT:
		{
			int beamer = 20;
			pwr = damroll(6, 6) + enhanced;
			if (randint(luckdev) > 62)
				pwr += randint((p_ptr->skills[SKILL_DEV] - 7) / 2);
			if (randint(luckdev) > 62)
				beamer += randint(p_ptr->lev/2);
			fire_bolt_or_beam(beamer, GF_ELEC, dir, pwr);
			*ident = TRUE;
			break;
		}

		case SV_WAND_FIRE_BOLT:
		{
			pwr = damroll(12, 8) + enhanced;
			if (randint(luckdev) > 65)
				pwr += randint(p_ptr->skills[SKILL_DEV]/2 + 2);
			fire_bolt_or_beam(20, GF_FIRE, dir, pwr);
			*ident = TRUE;
			break;
		}

		case SV_WAND_COLD_BOLT:
		{
			pwr = damroll(6, 8) + enhanced;
			if (randint(luckdev) > 65)
				pwr += randint(p_ptr->skills[SKILL_DEV]/2);
			fire_bolt_or_beam(20, GF_COLD, dir, pwr);
			*ident = TRUE;
			break;
		}

		case SV_WAND_ACID_BALL:
		{
			pwr = 120 + enhanced;
			if (randint(luckdev) > 65)
				pwr += randint(p_ptr->skills[SKILL_DEV]/2 + 5);
			fire_ball(GF_ACID, dir, pwr, 2);
			*ident = TRUE;
			break;
		}

		case SV_WAND_ELEC_BALL:
		{
			pwr = 64 + enhanced;
			if (randint(luckdev) > 67)
				pwr += randint(p_ptr->skills[SKILL_DEV]/2 + 7);
			fire_ball(GF_ELEC, dir, pwr, 2);
			*ident = TRUE;
			break;
		}

		case SV_WAND_FIRE_BALL:
		{
			pwr = 144 + enhanced;
			if (randint(luckdev) > 63)
				pwr += randint(p_ptr->skills[SKILL_DEV]/2 + 5);
			fire_ball(GF_FIRE, dir, pwr, 2);
			*ident = TRUE;
			break;
		}

		case SV_WAND_COLD_BALL:
		{
			pwr = 96 + enhanced;
			if (randint(luckdev) > 68)
				pwr += randint(p_ptr->skills[SKILL_DEV]/2 + 7);
			fire_ball(GF_COLD, dir, 96, 2);
			*ident = TRUE;
			break;
		}

		case SV_WAND_WONDER:
		{
            /* only activates with high luck, and rarely then */
            pwr = (p_ptr->lev + 10 + goodluck) / 2;
            pwr += adj_chr_charm[p_ptr->stat_ind[A_CHR]] + enhanced;

            die = randint(10);
			lite_line(dir);
			fire_ball(GF_POIS, dir, goodluck * randint(6), 4);
			if (die < 7)
            {
			   dis = ((p_ptr->skills[SKILL_DEV]*3)/4) + goodluck + randint(p_ptr->skills[SKILL_DEV] + (goodluck*2) + 2);
			   if (badluck) dis -= ((badluck/2) + (p_ptr->skills[SKILL_DEV] / 4));
			   if ((dis < 30) && (randint(100) < 25)) dis += 1 + goodluck/2 + randint(1 + p_ptr->skills[SKILL_DEV]);
               teleport_monster(dir, dis);
            }
			else if ((die == 7) || (die == 8)) slow_monster(dir, pwr);
			else if (die == 9) poly_monster(dir, pwr - 1);
			else if (die == 10) drain_life(dir, 120 + randint(goodluck));
			fear_monster(dir, pwr);
			sleep_monster(dir, pwr);
			*ident = TRUE;
			/* msg_print("Oops.  Wand of wonder activated."); */
			break;
		}

		case SV_WAND_STORMS:
		{
			int elecdam = 50 + randint(p_ptr->skills[SKILL_DEV] + 15);
			int waterdam = 50 + randint(p_ptr->skills[SKILL_DEV] + 15);
			if ((enhanced) && (elecdam + waterdam + enhanced < 200))
			{
				elecdam += enhanced/2;
				waterdam += (enhanced+1)/2;
			}
			fire_ball(GF_ELEC, dir, elecdam, 3);
			fire_ball(GF_WATER, dir, waterdam, 3);
			*ident = TRUE;
			break;
		}

		case SV_WAND_DRAGON_FIRE:
		{
			fire_ball(GF_FIRE, dir, 200, 3);
			*ident = TRUE;
			break;
		}

		case SV_WAND_DRAGON_COLD:
		{
			pwr = 160;
			if (randint(luckdev + enhanced) > 70)
				pwr += randint(p_ptr->skills[SKILL_DEV]/2);
			if (pwr + enhanced < 200) pwr += enhanced;
			else if (enhanced) pwr += (pwr + enhanced - 198) / 4;
			fire_ball(GF_COLD, dir, pwr, 3);
			*ident = TRUE;
			break;
		}

		case SV_WAND_DRAGON_BREATH:
		{
			switch (randint(5))
			{
				case 1:
				{
					fire_ball(GF_ACID, dir, 200, 3);
					break;
				}

				case 2:
				{
					if (enhanced < 40) fire_ball(GF_ELEC, dir, 160 + enhanced, 3);
					else fire_ball(GF_ELEC, dir, 160, 3);
					break;
				}

				case 3:
				{
					fire_ball(GF_FIRE, dir, 200, 3);
					break;
				}

				case 4:
				{
					if (enhanced < 40) fire_ball(GF_COLD, dir, 160 + enhanced, 3);
					else fire_ball(GF_COLD, dir, 160, 3);
					break;
				}

				default:
				{
					fire_ball(GF_POIS, dir, 120 + enhanced, 4);
					break;
				}
			}

			*ident = TRUE;
			break;
		}

		case SV_WAND_ANNIHILATION:
		{
            spellswitch = 8; /* enhance drain life (may do more than 250 damage) */
            if (drain_life(dir, 250)) *ident = TRUE;
			break;
		}
	}

	return TRUE;
}


static bool aim_wand(object_type *o_ptr, bool *ident)
{
	int chance, sval, dir, lev, enhanced;
	bool fluke = FALSE;
	bool returnthis;

	/* for alchemist's enhance wand spell */
	if ((o_ptr->enhance) && (o_ptr->pval > o_ptr->enhancenum) && 
		(o_ptr->enhancenum < o_ptr->number))
	{
		/* not all the items in this stack are enhanced */
		if (rand_int(o_ptr->number) < o_ptr->enhancenum) 
			enhanced = o_ptr->enhance/o_ptr->enhancenum + 1; /* (round up) */
		else enhanced = 0;
	}
	else
	{
		enhanced = o_ptr->enhance; /* 0 unless enhanced */
	}

	/* The wand is already empty! (should come before chance of success) */
	if (o_ptr->pval <= 0)
	{
		/* takes a turn only if you didn't already know it was empty */
		/* (you always know charges if aware) */
		if (!object_aware_p(o_ptr))
		{
			p_ptr->energy_use = 100;
			msg_print("You realize that the wand is out of charges.");
		}
		else
		{
			msg_print("The wand has no charges left.");
		}
		if (flush_failure) flush();
		o_ptr->ident |= (IDENT_EMPTY);
		p_ptr->notice |= (PN_COMBINE | PN_REORDER);
		p_ptr->window |= (PW_INVEN);
		return (FALSE);
	}

	/* Allow direction to be cancelled for free */
	if (!get_aim_dir(&dir)) return (FALSE);

	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Get the item difficulty (now separated from item level) */
	lev = k_info[o_ptr->k_idx].extra;

	/* cursed wands are harder to use */
	/* (currently wands are never cursed but that will likely change) */
	if (cursed_p(o_ptr)) lev += 5 + (badluck+1)/3;

	/* blessed devices are easier to use */
	if ((o_ptr->blessed > 1) && (lev > 23)) lev -= 12;
	else if ((o_ptr->blessed > 1) && (lev > 12)) lev = 12;
	else if ((o_ptr->blessed) && (lev > 4)) lev -= 4;

	/* Base chance of success */
	chance = p_ptr->skills[SKILL_DEV];

	/* Confusion hurts skill */
	if (p_ptr->timed[TMD_CONFUSED])
    {
       if (goodluck > 16) chance = (chance * 8) / 9;
       else if (goodluck > 9) chance = (chance * 3) / 4;
       else if (goodluck > 2) chance = (chance * 2) / 3;
	   else if (chance >= 60) chance = ((chance * 2) / 3) - (5 + (badluck/2));
       else chance = (chance / 2) - (badluck/3);
    }

	/* High level objects are harder */
	/* no limit now that difficulty is separated from depth */
	/* (very few devices have difficulty > 50) */
	/* chance = chance - ((lev > 50) ? 50 : lev); */
	chance = chance - lev;

	/* Give everyone a (slight) chance (USE_DEVICE==3) */
	if (chance < USE_DEVICE) /* 1 in 3 success rate at best */
	{
		if (chance + 1 < 2) chance = 2;
		else chance += 1;
		if (lev < 9) lev = 9;
		if (rand_int(lev) < chance) fluke = TRUE; /* success */
	}

	/* Roll for usage */
	if ((randint(chance) < USE_DEVICE) && (!fluke))
	{
		if (flush_failure) flush();
		msg_print("You failed to use the wand properly.");
		return (FALSE);
	}

	/* XXX Hack -- Extract the "sval" effect */
	sval = o_ptr->sval;

	/*** do the wand effect (effects moved to another function) ***/
	returnthis = wand_effect(sval, ident, enhanced, dir);

	/* return now only if FALSE */
	if (!returnthis) return (FALSE);

	/* enhancement wears off when last charge gets used up */
	if ((o_ptr->pval == 1) && (o_ptr->enhance))
	{
		o_ptr->enhance = 0;
		o_ptr->enhancenum = 0;
	}
	/* can't have more items enhanced than there are charges left */
	else if ((o_ptr->enhance) && (o_ptr->pval) && 
		(o_ptr->enhancenum < o_ptr->number))
	{
		if (o_ptr->enhancenum > o_ptr->pval) o_ptr->enhancenum = o_ptr->pval;
	}

	return (TRUE);
}


/* takes sval instead of o_ptr to allow for the mimmic wand spell */
bool rod_effect(int sval, bool *ident, int enhanced, int dir)
{
	bool used_charge = TRUE;

	/* Not identified yet */
	*ident = FALSE;

	/* Analyze the rod */
	switch (sval)
	{
		case SV_ROD_DETECT_TRAP:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
               *ident = TRUE;
            }
            else
            {
			  if (detect_traps()) *ident = TRUE;
            }
			break;
		}

		case SV_ROD_DETECT_DOOR:
		{
            if (p_ptr->timed[TMD_2ND_THOUGHT])
            {
               msg_print("Your first sight supresses detection.");
               *ident = TRUE;
            }
            else
            {
			  if (detect_doorstairs(FALSE)) *ident = TRUE;
            }
			break;
		}

		case SV_ROD_IDENTIFY:
		{
			*ident = TRUE;
			if (!ident_spell()) used_charge = FALSE;
			break;
		}

		case SV_ROD_RECALL:
		{
			if ((enhanced) && (enhanced <= 12)) set_recall(rand_int(18) + 17 - enhanced);
			else if (enhanced) set_recall(rand_int(18) + 5);
			else set_recall(rand_int(20) + 15);
			*ident = TRUE;
			break;
		}

		case SV_ROD_ILLUMINATION:
		{
			if (lite_area(damroll(2, 8) + enhanced, 2)) *ident = TRUE;
			break;
		}

		case SV_ROD_MAPPING:
		{
            if ((p_ptr->timed[TMD_2ND_THOUGHT]) && (goodluck < 12))
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  map_area();
            }
			*ident = TRUE;
			break;
		}

		case SV_ROD_DETECTION:
		{
            if ((p_ptr->timed[TMD_2ND_THOUGHT]) && (goodluck < 14))
            {
               msg_print("Your first sight supresses detection.");
            }
            else
            {
			  detect_all(TRUE);
            }
			*ident = TRUE;
			break;
		}

		case SV_ROD_PROBING:
		{
			probing();
			*ident = TRUE;
			break;
		}

		case SV_ROD_CURING:
		{
			if (clear_timed(TMD_BLIND)) *ident = TRUE;
			if (clear_timed(TMD_POISONED)) *ident = TRUE;
			if (clear_timed(TMD_CONFUSED)) *ident = TRUE;
			if (clear_timed(TMD_AMNESIA)) *ident = TRUE;
			if (clear_timed(TMD_STUN)) *ident = TRUE;
			if (clear_timed(TMD_CUT)) *ident = TRUE;
			break;
		}

		case SV_ROD_HEALING:
		{
			if (hp_player(500)) *ident = TRUE;
			if (clear_timed(TMD_STUN)) *ident = TRUE;
			if (clear_timed(TMD_CUT)) *ident = TRUE;
			break;
		}

		case SV_ROD_RESTORATION:
		{
			if (restore_level()) *ident = TRUE;
			if (do_res_stat(A_STR)) *ident = TRUE;
			if (do_res_stat(A_INT)) *ident = TRUE;
			if (do_res_stat(A_WIS)) *ident = TRUE;
			if (do_res_stat(A_DEX)) *ident = TRUE;
			if (do_res_stat(A_CON)) *ident = TRUE;
			if (do_res_stat(A_CHR)) *ident = TRUE;
			break;
		}

		case SV_ROD_SPEED:
		{
			if (p_ptr->timed[TMD_SUST_SPEED])
			{
				msg_print("You cannot be hasted while your speed is sustained");
			}
			if ((!p_ptr->timed[TMD_FAST]) && (!p_ptr->timed[TMD_SUST_SPEED]))
			{
				if (set_timed(TMD_FAST, randint(31) + 17)) *ident = TRUE;
			}
			else if (!p_ptr->timed[TMD_SUST_SPEED])
			{
				(void)inc_timed(TMD_FAST, 6);
			}
			break;
		}

		case SV_ROD_TELEPORT_AWAY:
		{
			int dis = p_ptr->skills[SKILL_DEV] + goodluck + randint(p_ptr->skills[SKILL_DEV] + 5);
            if (teleport_monster(dir, dis)) *ident = TRUE;
			break;
		}

		case SV_ROD_DISARMING:
		{
			if (disarm_trap(dir, 0)) *ident = TRUE;
			break;
		}

		case SV_ROD_LITE:
		{
			msg_print("A line of blue shimmering light appears.");
			if ((enhanced) && (rand_int(100) < 90 - enhanced)) strong_lite_line(dir);
			lite_line(dir);
			*ident = TRUE;
			break;
		}

		case SV_ROD_SLEEP_MONSTER:
		{
            int pwr = p_ptr->lev + 5 + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
			if (sleep_monster(dir, pwr + enhanced)) *ident = TRUE;
			break;
		}

		case SV_ROD_SLOW_MONSTER:
		{
            int pwr = p_ptr->lev + 5 + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
			if (slow_monster(dir, pwr + enhanced)) *ident = TRUE;
			break;
		}

		case SV_ROD_DRAIN_LIFE:
		{
			int draindam = 150;
			if (enhanced < 50) draindam += enhanced;
			else if (enhanced) draindam += randint(30) + 10;
			if (drain_life(dir, 150)) *ident = TRUE;
			break;
		}

		case SV_ROD_POLYMORPH:
		{
			if (poly_monster(dir, p_ptr->lev + 5 + enhanced)) *ident = TRUE;
			break;
		}

		case SV_ROD_ACID_BOLT:
		{
			fire_bolt_or_beam(10, GF_ACID, dir, damroll(12, 8) + enhanced);
			*ident = TRUE;
			break;
		}

		case SV_ROD_ELEC_BOLT:
		{
			fire_bolt_or_beam(10, GF_ELEC, dir, damroll(6, 6) + enhanced);
			*ident = TRUE;
			break;
		}

		case SV_ROD_FIRE_BOLT:
		{
			fire_bolt_or_beam(10, GF_FIRE, dir, damroll(16, 8) + enhanced);
			*ident = TRUE;
			break;
		}

		case SV_ROD_COLD_BOLT:
		{
			fire_bolt_or_beam(10, GF_COLD, dir, damroll(10, 8) + enhanced);
			*ident = TRUE;
			break;
		}

		case SV_ROD_ACID_BALL:
		{
			fire_ball(GF_ACID, dir, 120 + enhanced, 2);
			*ident = TRUE;
			break;
		}

		case SV_ROD_ELEC_BALL:
		{
			fire_ball(GF_ELEC, dir, 64 + enhanced, 2);
			*ident = TRUE;
			break;
		}

		case SV_ROD_FIRE_BALL:
		{
			int dam = 144;
			if (enhanced > 55) dam += randint(45) + 10;
			else dam += enhanced;
			fire_ball(GF_FIRE, dir, dam, 2);
			*ident = TRUE;
			break;
		}

		case SV_ROD_COLD_BALL:
		{
			fire_ball(GF_COLD, dir, 96 + enhanced, 2);
			*ident = TRUE;
			break;
		}
	}

	return used_charge;
}

static bool zap_rod(object_type *o_ptr, bool *ident)
{
	int chance, dir, lev, sval, enhanced;
	bool fluke = FALSE;
	bool used_charge;
	object_kind *k_ptr = &k_info[o_ptr->k_idx];

	/* for alchemist's enhance wand spell */
	if ((o_ptr->enhance) && (o_ptr->enhancenum < o_ptr->number))
	{
		/* not all the items in this stack are enhanced */
		if (rand_int(o_ptr->number) < o_ptr->enhancenum) 
			enhanced = (o_ptr->enhance+1)/o_ptr->enhancenum;
		else enhanced = 0;
	}
	else
	{
		enhanced = o_ptr->enhance; /* 0 unless enhanced */
	}

	/* Still charging? (before taking a turn & chance of failure) */
	/* you always know when a rod is charging so no need to check awareness */
	if (o_ptr->timeout > (o_ptr->pval - k_ptr->pval))
	{
		if (flush_failure) flush();

		if (o_ptr->number == 1)
			msg_print("The rod is still charging");
		else
			msg_print("The rods are all still charging");

		return FALSE;
	}

	/* Get a direction (unless KNOWN not to need it) */
	if ((o_ptr->sval >= SV_ROD_MIN_DIRECTION) || !object_aware_p(o_ptr))
	{
		/* Get a direction, allow cancel */
		if (!get_aim_dir(&dir)) return FALSE;
	}


	/* Take a turn */
	p_ptr->energy_use = 100;

	/* Not identified yet */
	*ident = FALSE;

	/* Extract the item diffuculty (now separated from item level) */
	lev = k_info[o_ptr->k_idx].extra;

	/* cursed rods are harder to use */
	/* (currently rods are never cursed but that will likely change) */
	if (cursed_p(o_ptr)) lev += 5 + badluck/3;

	/* blessed devices are easier to use */
	if ((o_ptr->blessed > 1) && (lev > 23)) lev -= 12;
	else if ((o_ptr->blessed > 1) && (lev > 12)) lev = 12;
	else if ((o_ptr->blessed) && (lev > 4)) lev -= 4;

	/* Base chance of success */
	chance = p_ptr->skills[SKILL_DEV];

	/* Confusion hurts skill */
	/* should be able to use rod of curing to cure confusion */
	if ((p_ptr->timed[TMD_CONFUSED]) && (o_ptr->sval != SV_ROD_CURING))
	{
       if (goodluck > 16) chance = (chance * 8) / 9;
       else if (goodluck > 9) chance = (chance * 3) / 4;
       else if (goodluck > 2) chance = (chance * 2) / 3;
	   else if (chance >= 60) chance = ((chance * 2) / 3) - (5 + (badluck/2));
       else chance = (chance / 2) - (badluck/3);
	}

	/* High level objects are harder */
	/* no limit now that difficulty is separated from depth */
	/* (very few devices have difficulty > 50) */
	/* chance = chance - ((lev > 50) ? 50 : lev); */
	chance = chance - lev;

	/* Give everyone a (slight) chance (USE_DEVICE==3) */
	if (chance < USE_DEVICE) /* 33% success rate (1 in 3) at best */
	{
		if (chance + 1 < 2) chance = 2;
		else chance += 1;
		if (lev < 9) lev = 9;
		if (rand_int(lev) < chance) fluke = TRUE; /* success */
	}

	/* Roll for usage */
	if ((randint(chance) < USE_DEVICE) && (!fluke))
	{
		if (flush_failure) flush();
		msg_print("You failed to use the rod properly.");
		return FALSE;
	}

	/* get the sval */
	sval = o_ptr->sval;

	/* Sound */
	sound(MSG_ZAP_ROD);


	/*** do the rod effect (effects moved to another function) ***/
	used_charge = rod_effect(sval, ident, enhanced, dir);

	/* Drain the charge */
	if (used_charge) o_ptr->timeout += k_ptr->pval;

	/* rods enhanced by the alchemy spell */
	if (o_ptr->enhance)
	{
		/* enhancement wears off with usage */
		if (randint(100) < 80 - (goodluck/2))
		{
			if (o_ptr->enhance > 2) o_ptr->enhance -= randint(2);
			else o_ptr->enhance -= 1;
		}
		if (!o_ptr->enhance)
		{
			/* message */
			char o_name[80];
			object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 1);
			msg_format("The enhancement on the %s has worn off.", o_name);
			o_ptr->enhancenum = 0;
		}
		else if ((o_ptr->enhance < o_ptr->enhancenum*3) && (o_ptr->enhancenum > 1))
			o_ptr->enhancenum -= 1;
	}

	return TRUE;
}


/*
 * Activate a wielded object.  Wielded objects never stack.
 * And even if they did, activatable objects never stack.
 *
 * Currently, only (some) artifacts, and Dragon Scale Mail, can be activated.
 * But one could, for example, easily make an activatable "Ring of Plasma".
 *
 * Note that it always takes a turn to activate an artifact, even if
 * the user hits "escape" at the "direction" prompt.
 */
static bool activate_object(object_type *o_ptr, bool *ident)
{
	int k, dir, i, chance, dis;
	bool controlled;
	int enhanced = o_ptr->enhance;

	/* Activate the artifact */
	message(MSG_ACT_ARTIFACT, 0, "You activate it...");

	/* Artifacts */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];

		/* Get the basic name of the object */
		char o_name[80];
		object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 0);

		switch (a_ptr->activation)
		{
			case ACT_ILLUMINATION:
			{
				msg_format("The %s wells with clear light...", o_name);
				lite_area(damroll(2, 15), 3);
				break;
			}
			
			case ACT_CHARM_ANIMAL:
            {
			   inc_timed(TMD_SPHERE_CHARM, randint(60 + goodluck) + 60);
			   break;
            }

			case ACT_MAGIC_MAP:
			{
				msg_format("The %s shines brightly...", o_name);
				map_area();
				break;
			}

			case ACT_CLAIRVOYANCE:
			{
				msg_format("The %s glows a deep green...", o_name);
				wiz_lite();
				(void)detect_traps();
				(void)detect_doorstairs(FALSE);
				break;
			}

			case ACT_PROT_EVIL:
			{
				msg_format("The %s lets out a shrill wail...", o_name);
				k = 3 * p_ptr->lev;
				(void)inc_timed(TMD_PROTEVIL, randint(25) + k);
				break;
			}

			case ACT_DISP_EVIL:
			{
				msg_format("The %s floods the area with goodness...", o_name);
				dispel_evil(p_ptr->lev * 5);
				break;
			}

			case ACT_HASTE2:
			{
				msg_format("The %s glows brightly...", o_name);
				if ((p_ptr->timed[TMD_SUST_SPEED]) && (goodluck < 9))
				{
					msg_print("You cannot be hasted while your speed is sustained");
					break;
				}
				if (!p_ptr->timed[TMD_FAST])
				{
					(void)set_timed(TMD_FAST, randint(90) + 60);
				}
				else
				{
					(void)inc_timed(TMD_FAST, 6);
				}
				break;
			}

			case ACT_FIRE3:
			{
				msg_format("The %s glows deep red...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				fire_ball(GF_FIRE, dir, 120, 3);
				break;
			}

			case ACT_FROST5:
			{
				msg_format("The %s glows bright white...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				fire_ball(GF_COLD, dir, 200, 3);
				break;
			}

			case ACT_ELEC2:
			{
				msg_format("The %s glows deep blue...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				fire_ball(GF_ELEC, dir, 250, 3);
				break;
			}

			case ACT_BIZZARE:
			{
				msg_format("The %s glows intensely black...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				ring_of_power(dir);
				break;
			}

			case ACT_STAR_BALL:
			{
				msg_format("Your %s is surrounded by lightning...", o_name);
				for (i = 0; i < 8; i++) fire_ball(GF_ELEC, ddd[i], 150, 3);
				break;
			}

			case ACT_RAGE_BLESS_RESIST:
			{
				msg_format("Your %s glows many colours...", o_name);
				(void)hp_player(30);
				(void)clear_timed(TMD_AFRAID);
				(void)clear_timed(TMD_CHARM);
				(void)clear_timed(TMD_FRENZY);
	            if (p_ptr->peace) /* prevents rage */;
	            else (void)inc_timed(TMD_SHERO, randint(60) + 30);
				(void)inc_timed(TMD_BLESSED, randint(60) + 30);
				(void)inc_timed(TMD_OPP_ACID, randint(60) + 30);
				(void)inc_timed(TMD_OPP_ELEC, randint(60) + 30);
				(void)inc_timed(TMD_OPP_FIRE, randint(60) + 30);
				(void)inc_timed(TMD_OPP_COLD, randint(60) + 30);
				(void)inc_timed(TMD_OPP_POIS, randint(60) + 30);
				break;
			}

			case ACT_HEAL2:
			{
				msg_format("Your %s glows a bright white...", o_name);
				msg_print("You feel much better...");
				(void)hp_player(1000);
				(void)clear_timed(TMD_CUT);
				break;
			}

			case ACT_PHASE:
			{
				msg_format("Your %s twists space around you...", o_name);
			    controlled = FALSE;
			    /* controlled teleport (random if you don't chose a target) */
		        if (p_ptr->telecontrol)
		        {
                   if (control_tport(100, 12)) controlled = TRUE;
                   if (!controlled) msg_print("You fail to control the teleportation.");
                }

                if (!controlled) teleport_player(10);
			    break;
			}

			case ACT_BANISHMENT:
			{
				msg_format("Your %s glows deep blue...", o_name);
				if (!banishment()) return FALSE;
				break;
			}

			case ACT_TRAP_DOOR_DEST:
			{
				msg_format("Your %s glows bright red...", o_name);
				destroy_doors_touch();
				break;
			}

			case ACT_DETECT:
			{
				msg_format("Your %s glows bright white...", o_name);
				msg_print("An image forms in your mind...");
				detect_all(TRUE);
				break;
			}

			case ACT_HEAL1:
			{
				msg_format("Your %s glows deep blue...", o_name);
				msg_print("You feel a warm tingling inside...");
				(void)hp_player(500);
				(void)clear_timed(TMD_CUT);
				break;
			}

			case ACT_RESIST:
			{
				int dura = randint(18) + 24;
                msg_format("Your %s glows many colours...", o_name);
				(void)inc_timed(TMD_OPP_ACID, dura);
				(void)inc_timed(TMD_OPP_ELEC, dura);
				(void)inc_timed(TMD_OPP_FIRE, dura);
				(void)inc_timed(TMD_OPP_COLD, dura);
				(void)inc_timed(TMD_OPP_POIS, dura);
				break;
			}

			case ACT_SLEEP:
			{
				int pwr = p_ptr->lev + 2 + adj_chr_charm[p_ptr->stat_ind[A_CHR]];
				msg_format("Your %s glows deep blue...", o_name);
				/* now has a radius of (at least) 3 */
				fire_spread(GF_OLD_SLEEP, pwr, 3 + (goodluck/4));
				/* sleep_monsters_touch(); */
				break;
			}

			case ACT_RECHARGE1:
			{
				msg_format("Your %s glows bright yellow...", o_name);
				if (!recharge(60)) return FALSE;
				break;
			}

			case ACT_TELEPORT:
		    {
				msg_format("Your %s twists space around you...", o_name);
			    controlled = FALSE;
			    /* controlled teleport (random if you don't chose a target) */
		        if (p_ptr->telecontrol)
		        {
                   if (control_tport(0, 175)) controlled = TRUE;
                   if (!controlled) msg_print("You fail to control the teleportation.");
                }

                if (!controlled) teleport_player(100);
			    break;
		    }

			case ACT_RESTORE_LIFE:
			{
				msg_format("Your %s glows a deep red...", o_name);
				restore_level();
				break;
			}

			case ACT_MISSILE:
			{
				msg_format("Your %s glows extremely brightly...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				fire_bolt(GF_MISSILE, dir, damroll(2, 6) + enhanced);
				break;
			}

			case ACT_FIRE1:
			{
				msg_format("Your %s is covered in fire...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				fire_bolt(GF_FIRE, dir, damroll(9, 8) + enhanced);
				break;
			}

			case ACT_FROST1:
			{
				msg_format("Your %s is covered in frost...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				fire_bolt(GF_COLD, dir, damroll(6, 8) + enhanced);
				break;
			}

			case ACT_LIGHTNING_BOLT:
			{
				msg_format("Your %s is covered in sparks...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				fire_bolt(GF_ELEC, dir, damroll(4, 8) + enhanced);
				break;
			}

			case ACT_ACID1:
			{
				msg_format("Your %s is covered in acid...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				fire_bolt(GF_ACID, dir, damroll(5, 8) + enhanced);
				break;
			}

			case ACT_ARROW:
			{
				msg_format("Your %s grows magical spikes...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				fire_bolt(GF_ARROW, dir, 150);
				break;
			}

			case ACT_HASTE1:
			{
				msg_format("Your %s glows bright green...", o_name);
				if ((p_ptr->timed[TMD_SUST_SPEED]) && (goodluck < 9))
				{
					msg_print("You cannot be hasted while your speed is sustained");
					break;
				}
				if (!p_ptr->timed[TMD_FAST])
				{
					(void)set_timed(TMD_FAST, randint(18) + 24);
				}
				else
				{
					(void)inc_timed(TMD_FAST, 6);
				}
				break;
			}

			case ACT_REM_FEAR_POIS:
			{
				msg_format("Your %s glows deep blue...", o_name);
				(void)clear_timed(TMD_AFRAID);
				(void)clear_timed(TMD_POISONED);
				if (randint(100) < 33) (void)clear_timed(TMD_CHARM);
				break;
			}

			case ACT_STINKING_CLOUD:
			{
				msg_format("Your %s throbs deep green...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				fire_ball(GF_POIS, dir, 12 + enhanced, 3);
				break;
			}

			case ACT_FROST2:
			{
				msg_format("Your %s is covered in frost...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				fire_ball(GF_COLD, dir, 48 + enhanced, 2);
				break;
			}

			case ACT_FROST4:
			{
				msg_format("Your %s glows a pale blue...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				fire_bolt(GF_COLD, dir, damroll(12, 8) + enhanced);
				break;
			}

			case ACT_FROST3:
			{
				msg_format("Your %s glows a intense blue...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				fire_ball(GF_COLD, dir, 100 + enhanced, 2);
				break;
			}

			case ACT_FIRE2:
			{
				msg_format("Your %s rages in fire...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				fire_ball(GF_FIRE, dir, 72 + enhanced, 2);
				break;
			}

			case ACT_HOLY_FIRE:
			{
				int litedam = damroll(3, 8);
				int firedam = damroll(9, 8);
				msg_format("Your %s bursts with holy fire...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				if ((litedam + firedam + enhanced < 150) && (enhanced))
				{
					litedam += (enhanced + 1) / 2;
					firedam += enhanced / 2;
				}
				fire_beam(GF_LITE, dir, litedam);
				fire_bolt_or_beam(20 + goodluck, GF_FIRE, dir, firedam);
				break;
			}

			case ACT_DRAIN_LIFE2:
			{
				msg_format("Your %s glows black...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				drain_life(dir, 120 + enhanced);
				break;
			}

			case ACT_STONE_TO_MUD:
			{
				msg_format("Your %s pulsates...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				wall_to_mud(dir);
				break;
			}

			case ACT_MASS_BANISHMENT:
			{
				msg_format("Your %s lets out a long, shrill note...", o_name);
				(void)mass_banishment();
				break;
			}

			case ACT_CURE_WOUNDS:
			{
				msg_format("Your %s radiates deep purple...", o_name);
				if (enhanced > 2) hp_player(damroll(4, 8) + (enhanced-2));
				else hp_player(damroll(4, 8));
				(void)set_timed(TMD_CUT, (p_ptr->timed[TMD_CUT] / 2) - 50);
				break;
			}

			case ACT_TELE_AWAY:
			{
				msg_format("Your %s glows deep red...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
			    dis = ((p_ptr->skills[SKILL_DEV]*5)/3) + goodluck + randint(20 + p_ptr->skills[SKILL_DEV]);
                teleport_monster(dir, dis);
				break;
			}

			case ACT_WOR:
			{
				msg_format("Your %s glows soft white...", o_name);
				if (enhanced > 15) set_recall(rand_int(15) + 5);
				else if (enhanced) set_recall(rand_int(22-enhanced) + 12);
				else set_recall(rand_int(20) + 15);
				break;
			}

			case ACT_CONFUSE: /* added possibility for ball effect */
			{
				if (!get_aim_dir(&dir)) return FALSE;
				if (randint(20 + adj_chr_charm[p_ptr->stat_ind[A_CHR]] + goodluck) >= 45)
				{
					msg_format("Your %s radiates scintillating colours...", o_name);
					fire_ball(GF_OLD_CONF, dir, 20 + adj_chr_charm[p_ptr->stat_ind[A_CHR]] + enhanced, 4);
				}
				else
				{
					msg_format("Your %s glows in scintillating colours...", o_name);
					confuse_monster(dir, 20 + adj_chr_charm[p_ptr->stat_ind[A_CHR]] + enhanced);
				}
				break;
			}

			case ACT_IDENTIFY:
			{
				msg_format("Your %s glows yellow...", o_name);
				if (!ident_spell()) return FALSE;
				break;
			}

			case ACT_PROBE:
			{
				msg_format("Your %s glows brightly...", o_name);
				probing();
				break;
			}

			case ACT_DRAIN_LIFE1:
			{
				msg_format("Your %s glows white...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				drain_life(dir, 90 + enhanced);
				break;
			}

			case ACT_FIREBRAND:
			{
				msg_format("Your %s glows deep red...", o_name);
				if (!brand_bolts()) return FALSE;
				break;
			}

			case ACT_SNOWBALL:
			{
				msg_format("Your %s glows frosty white...", o_name);
				inc_timed(TMD_OPP_COLD, randint(20) + 20);
				/* (BRAND_COLD is now on the sling of snowballs */
				/* so frost branding of shots no longer used) */
				/* if (!snowball_shot()) return FALSE; */
				break;
			}
			
			case ACT_TUNNELDIG:
		    {
			    /* spellswitch 31 makes it not stop at the first wall */
			    /* set in golem_to_mud() */
				msg_format("Your %s pulsates powerfully...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				golem_to_mud(dir);
			    break;
		    }

			case ACT_STARLIGHT:
			{
				msg_format("Your %s glows with the light of a thousand stars...", o_name);
				for (k = 0; k < 8; k++) strong_lite_line(ddd[k]);
				break;
			}

			case ACT_MANA_BOLT:
			{
				msg_format("Your %s glows white...", o_name);
				if (!get_aim_dir(&dir)) return FALSE;
				fire_bolt(GF_MANA, dir, damroll(12, 8) + enhanced);
				break;
			}

			case ACT_BERSERKER:
			{
				(void)clear_timed(TMD_CHARM);
                if (p_ptr->peace)
        	    {
                   msg_format("Your %s glows in anger, but is stifled by peaceful magic.", o_name);
                   return FALSE; /* don't make it have to recharge */
                }
				msg_format("Your %s glows in anger...", o_name);
			    inc_timed(TMD_SHERO, randint(50) + 50);
				break;
			}

			case ACT_SUN_HERO:
			{
				int time = randint(45) + 45;
                if (p_ptr->peace)
        	    {
				   msg_format("Your %s shines like the sun...", o_name);
				   inc_timed(TMD_DAYLIGHT, time);
                }
                else
                {
				   msg_format("Your %s shines like the sun and burns with fury...", o_name);
				   inc_timed(TMD_DAYLIGHT, time);
				   (void)clear_timed(TMD_CHARM);
			       inc_timed(TMD_SHERO, time);
                }
				break;
			}
		}

		/* Set the recharge time */
		if (a_ptr->randtime)
			o_ptr->timeout = a_ptr->time + (byte)randint(a_ptr->randtime);
		else
			o_ptr->timeout = a_ptr->time;

		/* artifacts enhanced by the alchemy spell */
		if (o_ptr->enhance)
		{
			/* enhancement wears off with usage */
			if (randint(100) < 90 - (goodluck/2))
			{
				if (o_ptr->enhance > 2) o_ptr->enhance -= randint(2);
				else o_ptr->enhance -= 1;
			}
			if (!o_ptr->enhance)
			{
				/* message char o_name[80]; */
				object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 1);
				msg_format("The enhancement on the %s has worn off.", o_name);
				o_ptr->enhancenum = 0;
			}
		}

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Done */
		return TRUE;
	}


	/* Hack -- Dragon Scale Mail can be activated as well */
	if (o_ptr->tval == TV_DRAG_ARMOR)
	{
		/* Get a direction for breathing (or abort) */
		if (!get_aim_dir(&dir)) return FALSE;

		/* Branch on the sub-type */
		switch (o_ptr->sval)
		{
			case SV_DRAGON_BLUE:
			{
				sound(MSG_BR_ELEC);
				msg_print("You breathe lightning.");
				fire_ball(GF_ELEC, dir, 100, 2);
				o_ptr->timeout = rand_int(540) + 360;
				break;
			}

			case SV_DRAGON_WHITE:
			{
				sound(MSG_BR_FROST);
				msg_print("You breathe frost.");
				fire_ball(GF_COLD, dir, 110, 2);
				o_ptr->timeout = rand_int(540) + 360;
				break;
			}

			case SV_DRAGON_BLACK:
			{
				sound(MSG_BR_ACID);
				msg_print("You breathe acid.");
				fire_ball(GF_ACID, dir, 130, 2);
				o_ptr->timeout = rand_int(540) + 360;
				break;
			}

			case SV_DRAGON_GREEN:
			{
				sound(MSG_BR_GAS);
				msg_print("You breathe poison gas.");
				fire_ball(GF_POIS, dir, 150, 2);
				o_ptr->timeout = rand_int(540) + 360;
				break;
			}

			case SV_DRAGON_RED:
			{
				sound(MSG_BR_FIRE);
				msg_print("You breathe fire.");
				fire_ball(GF_FIRE, dir, 200, 2);
				o_ptr->timeout = rand_int(540) + 360;
				break;
			}

			case SV_DRAGON_MULTIHUED:
			{
				chance = rand_int(5);
				sound(     ((chance == 1) ? MSG_BR_ELEC :
				            ((chance == 2) ? MSG_BR_FROST :
				             ((chance == 3) ? MSG_BR_ACID :
				              ((chance == 4) ? MSG_BR_GAS : MSG_BR_FIRE)))));
				msg_format("You breathe %s.",
				           ((chance == 1) ? "lightning" :
				            ((chance == 2) ? "frost" :
				             ((chance == 3) ? "acid" :
				              ((chance == 4) ? "poison gas" : "fire")))));
				fire_ball(((chance == 1) ? GF_ELEC :
				           ((chance == 2) ? GF_COLD :
				            ((chance == 3) ? GF_ACID :
				             ((chance == 4) ? GF_POIS : GF_FIRE)))),
				          dir, 250, 2);
				o_ptr->timeout = rand_int(240) + 240;
				break;
			}

			case SV_DRAGON_BRONZE:
			{
				sound(MSG_BR_CONF);
				msg_print("You breathe confusion.");
				fire_ball(GF_CONFUSION, dir, 120, 2);
				o_ptr->timeout = rand_int(540) + 360;
				break;
			}

			case SV_DRAGON_GOLD: /* */
			{
				sound(MSG_BR_SOUND);
				msg_print("You breathe sound.");
				fire_ball(GF_SOUND, dir, 130, 2);
				o_ptr->timeout = rand_int(540) + 360;
				break;
			}

			case SV_DRAGON_CHAOS:
			{
				chance = rand_int(2);
				sound(((chance == 1 ? MSG_BR_CHAOS : MSG_BR_DISENCHANT)));
				msg_format("You breathe %s.",
				           ((chance == 1 ? "chaos" : "disenchantment")));
				fire_ball((chance == 1 ? GF_CHAOS : GF_DISENCHANT),
				          dir, 220, 2);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}

			case SV_DRAGON_ETHEREAL:
			{
				chance = rand_int(4);
				msg_format("You breathe %s.",
				           ((chance == 1) ? "light" :
				            ((chance == 2) ? "darkness" :
				             ((chance == 3) ? "confusion" : "nexus"))));
				fire_ball(((chance == 1) ? GF_LITE :
				           ((chance == 2) ? GF_DARK :
				            ((chance == 3) ? GF_CONFUSION : GF_NEXUS))),
				          dir, 230, 2);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}

			case SV_DRAGON_LAW: /* Silver DSM */
			{
#if oldlaw
				chance = rand_int(2);
				sound(((chance == 1 ? MSG_BR_SOUND : MSG_BR_SHARDS)));
				msg_format("You breathe %s.",
				           ((chance == 1 ? "sound" : "shards")));
				fire_ball((chance == 1 ? GF_SOUND : GF_SHARD),
				          dir, 230, 2);
#else
				sound(MSG_BR_CONF);
				msg_print("You breathe nexus.");
				fire_ball(GF_NEXUS, dir, 110, 2);
#endif
				o_ptr->timeout = rand_int(270) + 360;
				break;
			}

			case SV_DRAGON_BALANCE:
			{
				chance = rand_int(4);
				msg_format("You breathe %s.",
				           ((chance == 1) ? "chaos" :
				            ((chance == 2) ? "disenchantment" :
				             ((chance == 3) ? "sound" : "shards"))));
				fire_ball(((chance == 1) ? GF_CHAOS :
				           ((chance == 2) ? GF_DISENCHANT :
				            ((chance == 3) ? GF_SOUND : GF_SHARD))),
				          dir, 250, 2);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}

			case SV_DRAGON_SHINING:
			{
				chance = rand_int(2);
				sound(((chance == 0 ? MSG_BR_LIGHT : MSG_BR_DARK)));
				msg_format("You breathe %s.",
				           ((chance == 0 ? "light" : "darkness")));
				fire_ball((chance == 0 ? GF_LITE : GF_DARK), dir, 150, 2);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}

			/* used to be only GF_MISSILE for 300 damage */
			case SV_DRAGON_POWER:
			{
				sound(MSG_BR_ELEMENTS);
				msg_print("You breathe the elements.");
				fire_ball(GF_ACID, dir, 45, 2);
				fire_ball(GF_FIRE, dir, 45, 2);
				fire_ball(GF_ELEC, dir, 45, 2);
				fire_ball(GF_COLD, dir, 45, 2);
				fire_ball(GF_POIS, dir, 45, 2);
				fire_ball(GF_MISSILE, dir, 75, 2);
				o_ptr->timeout = rand_int(300) + 300;
				break;
			}
		}

		/* Window stuff */
		p_ptr->window |= (PW_INVEN | PW_EQUIP);

		/* Success */
		return TRUE;
	}

	/* Hack -- some Rings can be activated for double resist and element ball */
	if (o_ptr->tval == TV_RING)
	{
		/* Get a direction for firing (or abort) */
		if (!get_aim_dir(&dir)) return FALSE;

		/* Branch on the sub-type */
		switch (o_ptr->sval)
		{
			case SV_RING_ACID:
			{
				fire_ball(GF_ACID, dir, 70 + enhanced, 2);
				inc_timed(TMD_OPP_ACID, randint(24) + 18);
				o_ptr->timeout = rand_int(30) + 60;
				break;
			}

			case SV_RING_FLAMES:
			{
				fire_ball(GF_FIRE, dir, 80 + enhanced, 2);
				inc_timed(TMD_OPP_FIRE, randint(24) + 18);
				o_ptr->timeout = rand_int(30) + 60;
				break;
			}

			case SV_RING_ICE:
			{
				fire_ball(GF_COLD, dir, 75 + enhanced, 2);
				inc_timed(TMD_OPP_COLD, randint(24) + 18);
				o_ptr->timeout = rand_int(30) + 60;
				break;
			}

			case SV_RING_LIGHTNING:
			{
				fire_ball(GF_ELEC, dir, 85 + enhanced, 2);
				inc_timed(TMD_OPP_ELEC, randint(24) + 18);
				o_ptr->timeout = rand_int(30) + 60;
				break;
			}
		}

		/* elemental rings enhanced by the 'enhance wand' alchemy spell */
		if (o_ptr->enhance)
		{
			/* enhancement wears off with usage */
			if (randint(100) < 90 - (goodluck/2))
			{
				if (o_ptr->enhance > 2) o_ptr->enhance -= randint(2);
				else o_ptr->enhance -= 1;
			}
			if (!o_ptr->enhance)
			{
				/* message */
				char o_name[80];
				object_desc(o_name, sizeof(o_name), o_ptr, FALSE, 1);
				msg_format("The enhancement on the %s has worn off.", o_name);
				o_ptr->enhancenum = 0;
			}
		}

		/* Window stuff */
		p_ptr->window |= (PW_EQUIP);

		/* Success */
		return TRUE;
	}

	/* Mistake */
	msg_print("Oops.  That object cannot be activated.");

	/* Not used up */
	return (FALSE);
}


bool use_object(object_type *o_ptr, bool *ident)
{
	bool used;

	/* Analyze the object */
	switch (o_ptr->tval)
	{
		case TV_FOOD:
		{
			used = eat_food(o_ptr, ident);
			break;
		}

		case TV_POTION:
		{
			used = quaff_potion(o_ptr, ident);
			break;
		}
		
		case TV_SPECIAL:
		case TV_SCROLL:
		{
			used = read_scroll(o_ptr, ident);
			break;
		}

		case TV_STAFF:
		{
			used = use_staff(o_ptr, ident);
			break;
		}

		case TV_WAND:
		{
			used = aim_wand(o_ptr, ident);
			break;
		}

		case TV_ROD:
		{
			used = zap_rod(o_ptr, ident);
			break;
		}

		default:
		{
			used = activate_object(o_ptr, ident);
			break;
		}
	}

	return (used);
}


static cptr act_description[ACT_MAX] =
{
	"illumination",
	"magic mapping",
	"clairvoyance",
	"protection from evil",
	"dispel evil (x5)",
	"heal (500)",
	"heal (1000)",
	"cure wounds (4d8)",
	"haste self (4+d3 minutes)",
	"haste self (10+d15 minutes)",
	"fire bolt (9d8)",
	"fire ball (72)",
	"large fire ball (120)",
	"frost bolt (6d8)",
	"frost ball (48)",
	"frost ball (100)",
	"frost bolt (12d8)",
	"large frost ball (200)",
	"acid bolt (5d8)",
	"recharge item I",
	"sleep II",
	"lightning bolt (4d8)",
	"large lightning ball (250)",
	"banishment",
	"mass banishment",
	"identify",
	"drain life (90)",
	"drain life (120)",
	"bizarre things",
	"star ball (150)",
	"berserk rage, bless, and resistance",
	"phase door",
	"door and trap destruction",
	"detection",
	"resistance (4+d3 minutes)",
	"teleport",
	"restore life levels",
	"magic missile (2d6)",
	"a magical arrow (150)",
	"remove fear and cure poison",
	"stinking cloud (12)",
	"stone to mud",
	"teleport away",
	"word of recall",
	"confuse monster",
	"probing",
	"fire branding of bolts",
	"starlight (10d8)",
	"mana bolt (12d8)",
	"berserk rage (10+d5 minutes)",
	"cold resistance (4+d4 minutes)",
	"sphere of animal charming",
	"tunneldigging",
	"daylight and berserk rage",
	"fire bolt (9d8) and light beam (3d8)"
};



/*
 * Determine the "Activation" (if any) for an artifact
 */
void describe_item_activation(const object_type *o_ptr)
{
	u32b f1, f2, f3, f4;
	s32b minute, rminute, sec, rsec;

	/* Extract the flags */
	object_flags(o_ptr, &f1, &f2, &f3, &f4);

	/* Require activation ability */
	if (!(f3 & TR3_ACTIVATE)) return;

	/* Artifact activations */
	if (o_ptr->name1)
	{
		artifact_type *a_ptr = &a_info[o_ptr->name1];

		/* Paranoia */
		if (a_ptr->activation >= ACT_MAX) return;

		/* Some artifacts can be activated */
		text_out(act_description[a_ptr->activation]);
		
		/* convert recharge time to minutes.  I don't want to */
		/* show anything to the player in game turns anymore */
		/* remember that the timeout only decrements once every 10 game turns */
		if (a_ptr->time >= 5) minute = (a_ptr->time + 1) / 6;
		else if (a_ptr->time) sec = a_ptr->time * 10;
		/* estimate random recharge time */
		if (((a_ptr->randtime >= 11) && (a_ptr->time)) || (a_ptr->randtime >= 17))
			rminute = (a_ptr->randtime + 1) / 6;
		else rsec = a_ptr->randtime * 10;

		/* Output the number of minutes/seconds to recharge */
		/* (round to nearest minute or 30 seconds) */
		if (a_ptr->time && a_ptr->randtime)
		{
			if ((minute) && (rminute))
				text_out(format(" about every %d+d%d minutes", minute, rminute));
			else if ((minute) && (rsec >= 30))
				text_out(format(" about every %d minutes", minute + 1));
			else if (minute > 1)
				text_out(format(" about every %d minute", minute));
			else text_out(" about every minute");
		}
		else if (a_ptr->time)
		{
			if (minute > 114)
			{
				s32b halfhour = (minute + 4) / 30; /* estimate */
				s16b hour = halfhour / 2;
				halfhour = (int)(halfhour - hour * 2);
				if (halfhour == 1) text_out(format(" about every %d and a half hours", hour));
				else text_out(format(" about every %d hours", hour));
			}
			else if (minute > 1) text_out(format(" about every %d minutes", minute));
			else if ((minute) || (sec >= 50)) text_out(format(" about every minute", minute));
			else text_out(format(" every %d seconds", sec));
		}
		else if (a_ptr->randtime)
		{
			if (rminute) text_out(format(" about every d%d minutes", rminute));
			else if (rsec >= 80) text_out(" about every 2 minutes");
			else if (rsec >= 40) text_out(" about every minute");
			else text_out(" every 30 seconds");
		}

		return;
	}

	/* Ring activations */
	if (o_ptr->tval == TV_RING)
	{
		/* Branch on the sub-type */
		switch (o_ptr->sval)
		{
			case SV_RING_ACID:
			{
				text_out("acid resistance (3+d4 minutes) and acid ball (70) every 10+d5 minutes");
				break;
			}
			case SV_RING_FLAMES:
			{
				text_out("fire resistance (3+d4 minutes) and fire ball (80) every 10+d5 minutes");
				break;
			}
			case SV_RING_ICE:
			{
				text_out("cold resistance (3+d4 minutes) and cold ball (75) every 10+d5 minutes");
				break;
			}

			case SV_RING_LIGHTNING:
			{
				text_out("electricity resistance (3+d4 minutes) and electricity ball (85) every 10+d5 minutes");
				break;
			}
		}

		return;
	}

	/* Require dragon scale mail */
	if (o_ptr->tval != TV_DRAG_ARMOR) return;

	/* Branch on the sub-type */
	switch (o_ptr->sval)
	{
		case SV_DRAGON_BLUE:
		{
			text_out("breathe lightning (100) every 60+d90 minutes");
			break;
		}
		case SV_DRAGON_WHITE:
		{
			text_out("breathe frost (110) every 60+d90 minutes");
			break;
		}
		case SV_DRAGON_BLACK:
		{
			text_out("breathe acid (130) every 60+d90 minutes");
			break;
		}
		case SV_DRAGON_GREEN:
		{
			text_out("breathe poison gas (150) every 60+d90 minutes");
			break;
		}
		case SV_DRAGON_RED:
		{
			text_out("breathe fire (200) every 60+d90 minutes");
			break;
		}
		case SV_DRAGON_MULTIHUED:
		{
			text_out("breathe multi-hued (250) every 40+d40 minutes");
			break;
		}
		case SV_DRAGON_BRONZE:
		{
			text_out("breathe confusion (120) every 60+d90 minutes");
			break;
		}
		case SV_DRAGON_GOLD:
		{
			text_out("breathe sound (130) every 60+d90 minutes");
			break;
		}
		case SV_DRAGON_CHAOS:
		{
			text_out("breathe chaos/disenchant (220) every 50+d50 minutes");
			break;
		}
		case SV_DRAGON_LAW:
		{
			text_out("breathe sound/shards (230) every 50+d50 minutes");
			break;
		}
		case SV_DRAGON_BALANCE:
		{
			text_out("breathe balance (250) every 50+d50 minutes");
			break;
		}
		case SV_DRAGON_SHINING:
		{
			text_out("breathe light/darkness (200) 50+d50 minutes");
			break;
		}
		case SV_DRAGON_POWER:
		{
			text_out("breathe the elements (300) every 50+d50 minutes");
			break;
		}
		case SV_DRAGON_ETHEREAL:
		{
			text_out("breathe light, dark, confusion, or nexus (230) every 50+d50 minutes");
			break;
		}
	}
}
