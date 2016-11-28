
/* File: xtra3.c */

/* Describe weird attacks, set grace, interpret grace on a 1-10 scale, 
 * describe and implement crowning. great, good, nasty and deadly praying 
 * effects, convert the A_LUC stat to a signed int value, code to count 
 * mutations
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


void describe_attack(int type, char* r) {
  switch (type) {
  case GF_ARROW:       strcpy(r, "small arrows"); break;
  case GF_MISSILE:     strcpy(r, "small darts"); break;
  case GF_MANA:        strcpy(r, "pure energy"); break;
  case GF_LITE_WEAK:   strcpy(r, "light"); break;
  case GF_DARK_WEAK:   strcpy(r, "dark"); break;
  case GF_FLOOD:       strcpy(r, "rainwater"); break;
  case GF_WATER:       strcpy(r, "water"); break;
  case GF_PLASMA:      strcpy(r, "white-hot fire"); break;
  case GF_METEOR:      strcpy(r, "meteors"); break;
  case GF_ICE:         strcpy(r, "ice"); break;
  case GF_GRAVITY:     strcpy(r, "heavyness"); break;
  case GF_INERTIA:     strcpy(r, "impelling force"); break;
  case GF_FORCE:       strcpy(r, "force"); break;
  case GF_TIME:        strcpy(r, "pure time"); break;
  case GF_ACID:        strcpy(r, "acid"); break;
  case GF_ELEC:        strcpy(r, "lightning"); break;
  case GF_FIRE:        strcpy(r, "flames"); break;
  case GF_COLD:        strcpy(r, "cold"); break;
  case GF_POIS:        strcpy(r, "pungent gas"); break;
  case GF_LITE:        strcpy(r, "pure light"); break;
  case GF_DARK:        strcpy(r, "pure dark"); break;
  case GF_CONFUSION:   strcpy(r, "scintillating colors"); break;
  case GF_SOUND:       strcpy(r, "high-pitched noise"); break;
  case GF_CHAOS:       strcpy(r, "chaos"); break;
  case GF_NEXUS:       strcpy(r, "black, heavy clouds"); break;
  case GF_NETHER:      strcpy(r, "nothingness"); break;
  case GF_DISENCHANT:  strcpy(r, "emptiness"); break;
  case GF_KILL_WALL:
  case GF_KILL_DOOR:
  case GF_KILL_TRAP:
  case GF_MAKE_WALL:
  case GF_MAKE_DOOR:
  case GF_MAKE_TRAP:
    strcpy(r, "swirling pebbles");
    break;

  default:
    /* Get something silly randomly */
    get_rnd_line("sfail.txt", r);
    break;
  }
}

/*
 * Set "p_ptr->grace", notice observable changes
 */
void set_grace(s32b v) {
  p_ptr->grace = v;
  p_ptr->update |= PU_BONUS;
  handle_stuff();
}


/*
 * Return a number denoting your current standing with your god,
 * ranging from 0 (really bad) to 10 (really good).
 * Note that 0-5 mean ``cursed'', 6-7 mean ``neutral'',
 * and 8-10 mean ``blessed''.
 */

int interpret_grace(void) {
  if (p_ptr->grace <      -30000) return 0;
  else if (p_ptr->grace < -20000) return 1;
  else if (p_ptr->grace < -10000) return 2;
  else if (p_ptr->grace <      0) return 3;

  else if (p_ptr->grace > 160000) return 10;
  else if (p_ptr->grace >  80000) return 9;
  else if (p_ptr->grace >  40000) return 8;
  else if (p_ptr->grace >  20000) return 7;
  else if (p_ptr->grace >  10000) return 6;
  else if (p_ptr->grace >   5000) return 5;
  else if (p_ptr->grace >      0) return 4;

  /* Should never happen! */
  return -1;
}


void grace_message(void)
{
    cptr name = deity_info[p_ptr->pgod-1].name;
    
    switch(interpret_grace())
    {
      case 0:
	 msg_format("%s hates you", name);
	 break;
      case 1:
	 msg_format("%s is very angry at you.", name);
	 break;
      case 2:
	 msg_format("%s is rather angry at you.", name);
	 break;
      case 3:
	 msg_format("%s is neutral to you.", name);
	 break;
      case 4:
	 msg_format("%s is appeased.", name);
	 break;
      case 5:
	 msg_format("%s is pleased.", name);
	 break;
      case 6:
	 msg_format("%s is very pleased.", name);
	 break;
      case 7:
	 msg_format("%s is greatly pleased.", name);
	 break;
      case 8:
	 msg_print("You feel more lucky.");
	 if (p_ptr->stat_cur[A_LUC] < 14)
	 (void)do_inc_stat(A_LUC);
	 break;
      case 9:
	 msg_print("You are spiritually elated.");
	 if (p_ptr->stat_cur[A_LUC] < 18)
	 (void)do_inc_stat(A_LUC);
	 break;
      case 10:
	 msg_print("You are ready for championhood.");
	 break;
      default:
	 msg_format("%s is perplexed about your status", name);
    }	 
}

void crowning(void)
{

    cptr name = deity_info[p_ptr->pgod-1].name;
    int align = deity_info[p_ptr->pgod-1].align;
    cptr what_align;
    object_type forge;
    object_type *o_ptr;
    int k_idx;

    if (p_ptr->champion) return; /* Can be crowned only once */
	
    switch(align)
    {
      case ALIGN_GOOD:
	what_align = "good";
	break;
      case ALIGN_NEUT:
	what_align = "balance";
	break;
      case ALIGN_EVIL:
	what_align = "evil";
	break;
      default:
	what_align = "computing";
    }
	
    msg_format("The grace of %s touches you...", name);
    msg_format("Arise, my holy champion of %s!", what_align);
    
    o_ptr = &forge;
    object_wipe(o_ptr);
    
    while (!((o_ptr->tval >= TV_BOW) && (o_ptr->tval <= TV_DRAG_ARMOR)))
    {
    k_idx = get_obj_num(object_level);
    object_prep(o_ptr, k_idx);
    }
     
    create_artifact(o_ptr, FALSE);
    drop_near(o_ptr, -1, py, px);
    
    /* We are the champions... */
    
    p_ptr->champion = TRUE;
}    

static bool fire_explosion_ugly(int y, int x, int type, int rad, int dam,
				byte god) 
			
{
    return fire_explosion(y, x, type, rad, dam);
}

/*
 * Here come the side effect functions. They are classified according
 * to how beneficial they are, to avoid a giant nested switch/case
 * statement.
 */

/* Great side effect. */

void great_side_effect(void) {
  int tmp;

  tmp = randint(100);

  if (tmp <= 10) {
    if (summon_specific(py, px, dun_level+rand_spread(10, 5), 
				damroll(4,6), TRUE, TRUE, TRUE)) 
    {
      msg_print("Something materializes out of thin air.");
    } else {
      msg_print("You feel a strange tingling, but the feeling passes.");
    }
  } else if (tmp <= 20) {
    acquirement(py, px, 1, 1, 1);
  } else if (tmp <= 30) {
    enchant_spell(0, 0, randint(3) + 2);
  } else if (tmp <= 40) {
    enchant_spell(randint(3), randint(3), 0);
  } else if (tmp <= 50) {
    fire_explosion(py, px, GF_MAKE_GLYPH, 9, 0);
  } else if (tmp <= 60) {
    recharge(100);
  } else if (tmp <= 70) {
    remove_all_curse();
  } else if (tmp <= 80) {
    identify_fully();
  } else if (tmp <= 90) {
    restore_level();
    hp_player(5000);
    (void)set_poisoned(0);
    (void)set_blind(0);
    (void)set_confused(0);
    (void)set_image(0);
    (void)set_stun(0);
    (void)set_cut(0);
    (void)do_res_stat(A_STR);
    (void)do_res_stat(A_CON);
    (void)do_res_stat(A_DEX);
    (void)do_res_stat(A_WIS);
    (void)do_res_stat(A_INT);
    (void)do_res_stat(A_CHR);
  } else if (tmp <= 100) {
    do_inc_stat(A_STR);
    do_inc_stat(A_INT);
    do_inc_stat(A_WIS);
    do_inc_stat(A_DEX);
    do_inc_stat(A_CON);
    do_inc_stat(A_CHR);
  }
}



/* Good side effect. */

void good_side_effect(void) {
  int tmp;

  tmp = randint(100);

  if (tmp <= 10) {
    if (summon_specific(py, px, dun_level, damroll(4,6), FALSE, TRUE, TRUE))
    {
      msg_print("Something materializes out of thin air.");
    } else {
      msg_print("You feel a strange tingling, but the feeling passes.");
    }
  } else if (tmp <= 20) {
    acquirement(py, px, 1, 0, 1);
  } else if (tmp <= 30) {
    enchant_spell(0, 0, 1);
  } else if (tmp <= 40) {
    enchant_spell(1, 1, 0);
  } else if (tmp <= 50) {
    fire_explosion(py, px, GF_MAKE_GLYPH, 4, 0);
  } else if (tmp <= 60) {
    recharge(60);
  } else if (tmp <= 70) {
    remove_curse();
  } else if (tmp <= 80) {
    ident_spell();
  } else if (tmp <= 90) {
    hp_player(1200);
    (void)set_poisoned(0);
    (void)set_blind(0);
    (void)set_confused(0);
    (void)set_image(0);
    (void)set_stun(0);
    (void)set_cut(0);
  } else if (tmp <= 100) {
    wiz_lite();
  }
}


/* Nasty side effect. */

void nasty_side_effect(void) {
  int tmp;

  tmp = randint(100) - (luck() / 2);
  if (tmp < 10) {
    set_poisoned(p_ptr->poisoned + 10 + randint(10));
  } else if (tmp < 20) {
    set_confused(p_ptr->confused + 10 + randint(10));
  } else if (tmp < 30) {
    set_blind(p_ptr->blind + 10 + randint(10));
  } else if (tmp < 40) {
    set_slow(p_ptr->slow + 10 + randint(10));
  } else if (tmp < 50) {
    set_cut(p_ptr->cut + 10 + randint(100));
  } else if (tmp < 60) {
    set_stun(p_ptr->stun + randint(110));
  } else if (tmp < 70) {
    msg_print("You hear a loud shriek!");
    aggravate_monsters(1);
  } else if (tmp < 80) {
    set_image(p_ptr->image + 20 + randint(20));
  } else if (tmp < 90) {
    msg_print("You feel very sick.");
    set_slow(p_ptr->slow + 10 + randint(10));
    set_confused(p_ptr->confused + 10 + randint(10));
    set_poisoned(p_ptr->poisoned + 10 + randint(10));
  } else if (tmp < 100) {
    if (summon_specific(py, px, dun_level+randint(7), damroll(4,6), FALSE, FALSE, FALSE)) {
      msg_print("Something materializes out of thin air.");
    } else {
      msg_print("You feel a strange tingling, but the feeling passes.");
    }
  }
}

/* Deadly side effect. */

void deadly_side_effect(bool god) {
  int tmp;
  bool (*boom)(int, int, int, int, int, byte);

  byte pgd;

  if (god) {
    boom = fire_godly_wrath;
    pgd = p_ptr->pgod - 1;
  } else {
    boom = fire_explosion_ugly;
    pgd = 0;
  }

  tmp = randint(100);

  if (tmp <= 10) {
    if (summon_specific(py, px, dun_level+20, damroll(4,6), FALSE, FALSE, FALSE)) {
      msg_print("Something materializes out of thin air.");
    } else {
      msg_print("You feel a strange tingling, but the feeling passes.");
    }
  } else if (tmp <= 20) {
    activate_ty_curse();
  } else if (tmp <= 30) {
    msg_print("The world twists!");
    destroy_area(py, px, 9, 0);
  } else if (tmp <= 40) {
    msg_print("Your nerves and muscles feel weak and lifeless.");
    (void)dec_stat(A_STR, 5, TRUE);
    (void)dec_stat(A_INT, 5, TRUE);
    (void)dec_stat(A_WIS, 5, TRUE);
    (void)dec_stat(A_DEX, 5, TRUE);
    (void)dec_stat(A_CON, 5, TRUE);
    (void)dec_stat(A_CHR, 5, TRUE);
  } else if (tmp <= 50) {
    msg_print("You feel somehow inadequate...");
    p_ptr->exp -= (p_ptr->exp / 4);
    p_ptr->max_exp -= (p_ptr->exp / 4);
    check_experience();
  } else if (tmp <= 60) {
    msg_print("Your whole life flashes before your eyes.");
    boom(py, px, GF_TIME, 5, 100, pgd);
  } else if (tmp <= 70) {
    msg_print("Everything seems grayer somehow...");
    boom(py, px, GF_DISENCHANT, 5, 100, pgd);
  } else if (tmp <= 80) {
    msg_print("There is a loud cackle...");
    boom(py, px, GF_MAKE_TRAP, 9, 0, pgd);
  } else if (tmp <= 90) {
    msg_print("Something is trying to destroy your brain!");
    take_sanity_hit(damroll(10,10), "an angry deity");
  } else if (tmp <= 100) {
    godly_wrath_blast(pgd);
  }
}

/*
 * Fire a godly blast from the sky.
 * Note that only attacks which are not resisted are used.
 * (Gods are omnipotent, aren't they?)
 */

void godly_wrath_blast(byte god) {
  int tmp;
  int type = 0;
  bool ok = FALSE;

  while (1) {
    tmp = randint(10);

    switch (tmp) {
    case 1:
      if (!p_ptr->immune_acid) {
	type = GF_ACID;
	ok = TRUE;
	msg_print("You are blasted by acid from the sky!");
      }
      break;

    case 2:
      if (!p_ptr->immune_elec) {
	type = GF_ELEC;
	ok = TRUE;
	msg_print("You are blasted by a giant ball lightning from the sky!");
      }
      break;

    case 3:
      if (!(p_ptr->resist_pois || p_ptr->oppose_pois)) {
	type = GF_POIS;
	ok = TRUE;
	msg_print("A poisonous cloud descends from the sky!");
      }
      break;

    case 4:
      if (!p_ptr->resist_neth) {
	type = GF_NETHER;
	ok = TRUE;
	msg_print("A force of death surrounds you!");
      }
      break;

    case 5:
      if (!(p_ptr->resist_sound || p_ptr->resist_conf)) {
	type = GF_WATER;
	ok = TRUE;
	msg_print("A flood of water falls from the sky!");
      }
      break;

    case 6:
      if (!p_ptr->resist_chaos) {
	type = GF_CHAOS;
	ok = TRUE;
	msg_print("You are blasted by a gale of chaos!");
      }
      break;

    case 7:
      if (!p_ptr->resist_shard) {
	type = GF_SHARDS;
	ok = TRUE;
	msg_print("A multitude of shards descends on you from the sky!");
      }
      break;

    case 8:
      if (!p_ptr->resist_disen) {
	type = GF_DISENCHANT;
	ok = TRUE;
	msg_print("You feel your magical aura wane!");
      }
      break;

    case 9:
      type = GF_TIME;
      ok = TRUE;
      msg_print("A dizzying array of blinking dots falls from the sky!");
      break;

    case 10:
      type = GF_METEOR;
      ok = TRUE;
      msg_print("A giant meteor falls on you from the sky!");
      break;
    }

    if (ok) break;
  }

  fire_godly_wrath(py, px, type, 1, damroll(4, 5), god);
}

/*
 * Compute a percentage modifier based on the A_LUC stat, from -30 to
 * more than 30 (actually 39 with maximize, 21 without).
 */
int luck()
{
	int t, mod;

	t = p_ptr->stat_cur[A_LUC];
	if (t < 4)
		mod = -30;
	else if (t < 5)
		mod = -20;
	else if (t < 6)
		mod = -10;
	else if (t < 7)
		mod = -5;
	else if (t < 14)
		mod = 0;
	else if (t < 16)
		mod = 2;
	else if (t < 18)
		mod = 5;
	else if (t < 43)
		mod = t / 3;
	else
		mod = t / 5 + 7;                /* REALLY lucky dude! */

	if (phase_of_the_moon() == 4)
	   return (mod > 0 ? mod : 1);
	else if (phase_of_the_moon() == 0)
	   return (mod < 0 ? mod : -1);
	else if (friday_13th())
	   return (mod < 0 ? mod*=2 : -2);  /* Decidedly bad luck, at
					       most -2 */
	else return mod;
}

extern int count_bits(u32b x)
{
	int n = 0;

	if (x) do
	{
		n++;
	}
	while (0 != (x = x&(x-1)));

	return (n);
}

extern int count_mutations(void)
{
	return (count_bits(p_ptr->muta1) +
		count_bits(p_ptr->muta2) +
		count_bits(p_ptr->muta3));
}

