
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

        if (p_ptr->birthday == bst(DAY)) 
	   return (mod > 0 ? mod : 1);           

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
