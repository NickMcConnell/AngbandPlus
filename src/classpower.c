/* File: classpower.c */
/* Purpose: Special Class abilities code */


/*
 * Copyright (c) 2002 Courtney Campbell
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

/*
 * Large sections of this code have been copied almost verbatim from other
 * Variants. In no way am I saying that I'm anywhere near as cool as them
 * For a list of variants that I stole from, check the info that came with 
 * the source. I do not take credit for coming up with this code myself.
 * I come so far only because I stand on the shoulders of giants. -CCC
 */
 
 #include "angband.h"
 
 mind_power mind_powers[9] =
 {
  /* Officer Powers */
  {
    {
      /* Level gained,  cost,  %fail,  name */
      /* this is ok */
      { 1,   2,  10, "Inspire Fear"},
      { 5,  4,  20, "Command"},
      { 8,  10,  35 , "Enlist"},
      { 10,  20,  55, "Stiff Upper Lip"},
      { 13,  5,   30, "Sense Recruits"},
      { 15,  50,  80, "Defensive Techniques"},
      { 18,  30,  99, "Mass Enlistment"},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      
    }
  },
  
  /* Aesthete Powers */
  
  {
    {
      /* Level gained,  cost,  %fail,  name */
      /* this also seems ok */
      { 1,   1,  10, "Arrogant Laugh"},
      { 2,   2,  10, "Treasure Sense"},
      { 5,  20,  65, "Enhance Weapon"},
      { 5,  20,  65, "Enhance Armor"},
      { 10, 10,  40, "Legendary Lore"},
      { 12,  5,  30, "Duck out the Back"},
      { 14, 30,  60, "Item Overhaul"},
      { 18, 40,  60, "Touch of Fortune"},
      { 20, 20,  70, "Merchant's Trail"},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      
    }
  },
  
  /* Explorer Powers */
  /* Farily general power set */
  {
    {
      /* Level gained,  cost,  %fail,  name */
      /* this is also ok */
      { 1,  1,   35, "Light area"},
      { 2,  3,   30, "Detection"},
      { 5,  5,   25, "Portal"},
      { 8, 10,  20, "Satisfy Hunger"},
      { 10, 20,  40, "Identify"},
      { 12, 25,  35, "Word of Recall"},
      { 15, 60,  55, "Recharge Item"},
      { 18, 30,  60, "Alchemy"},
      { 20, 25,  30, "Resistance"},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      
    }
  },
  
  /* Medium Powers */
  /* Mindcrafters */
  {
	{
		/* change these, and add skills */
		{1, 1, 15, "Neural Blast"},
		{2, 1, 20, "Precognition"},
		{3, 2, 25, "Minor Displacement"},
		{6, 6, 35, "Major Displacement"},
		{7, 7, 50, "Psychic Disturbance"},
		{8, 7, 30, "Spirit Blast"},
		{9, 12, 50, "Character Armour"},
		{10, 12, 60, "Psychometry"},
		{12, 15, 50, "Domination"},
		{14, 10, 45, "Soul Purge"},
		{16, 15, 50, "Drawing upon the Sprits"},
		{18, 10, 40, "Psychic Drain"},
		{20, 20, 45, "Entropic blast"},
        { 99,  0,   0, ""},
        { 99,  0,   0, ""},
        { 99,  0,   0, ""},
        { 99,  0,   0, ""},
        { 99,  0,   0, ""},
        { 99,  0,   0, ""},
        { 99,  0,   0, ""},
        { 99,  0,   0, ""},
		
	}

  },
  
  /* Reckoner Powers */
  /* Reality altering powers, . I'll have to work on this a bit */
  /* eventually adding more editing powers */
  {
    {
      /* Level gained,  cost,  %fail,  name */
      { 1,  1,   20, "Spacial Distortion"},
      { 3,  2,   25, "Photoshift"},
      { 6,  5,   30, "Chronoshift"},
      { 8,  8,   45, "Living Bond Destoyer"},
      { 10,  12,  50, "Non-Living Bond Destoyer"},
      { 12,  20,  45, "Conservation of Vital Force"},
      { 14,  25,  40, "Code of Recall"},
      { 16,  30,  50, "Darwin's Logic"},
      { 18,  35,  50, "Mind of the Machine"},
      { 20,  80,  75, "Rewrite Reality"},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      
    }
  },
  /* Tourist Powers */
  /* Traveling powers */
  {
    {
      /* Level gained,  cost,  %fail,  name */
      { 1,  1,   40,  "Jump"},
      { 3,  2,   40,  "Portal"},
      { 7,  3,   40,  "Teleport"},
      { 10,  10,  40, "Etherial Walk"},
      { 12,  20,   40,  "Word of Recall"},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      
    }
  },
  /* Hussar combat powers */
  /* Go look and see what combat moves and manuvers are */
  /* This should probably have more skills than 3, less than druid, and mentalist */
  {
    {
      /* Level gained,  cost,  %fail,  name */
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      { 99,  0,   0, ""},
      
    }
  },
  /* Naturlist powers */
  /* ack! No idea! Stole Z's Druid! */
  {
    {
      /* Level gained,  cost,  %fail,  name */
      /* Change these and add skills */
      { 1,  1,   10, "Detect Creatures"},
      { 2,  3,   15, "First Aid"},
      { 3,  3,   20, "Detect Doors + Traps"},
      { 4,  5,   25, "Produce Food"},
      { 5,  6,   30, "Daylight"},
      { 6,  10, 45, "Animal Taming"},
      { 7,  11, 40, "Resist Environment"},
      { 8,  14, 50, "Natural Healing"},
      { 9,  15, 55, "Stone to Mud"},
      { 10,  7,  55, "Lightning Bolt"},
      { 11,  4,  33, "Ray of Sunlight"},
      { 12,  17, 43, "Entangle"},
      { 13,  20, 60, "Stoneskin"},
      { 14,  20, 65, "Resistance true"},
      { 15,  30, 70, "Stone Tell"},
      { 16,  35, 70, "Earthquake"},
      { 17,  14, 65, "Blizzard"},
      { 18,  18, 65, "Lightning Storm"},
      { 19,  20, 65, "Whirlpool"},
      { 19,  10, 70, "Call Sunlight"},
      { 20,  50, 70, "Nature's Wrath"},
      
    }
  },
  /* Ninjitsu powers I stole this ninja from heng */
  /* Sigh I need to fix these too. */
  {
    {
      /* Level gained,  cost,  %fail,  name */
      /* hm. Should do something with this. */
      { 1,  1,  20, "Create Darkness"},
      { 2,  2,  25, "Detect Near"},
      { 3,  3,  25, "Hide in Leafs"},
      { 4,  3,  30, "Kawarimi"},
      { 5,  8,  35, "Absconding"},
      { 6, 10,  35, "Hit and Away"},
      { 7, 10,  40, "Bind Monster"},
      { 8, 12,  70, "Ancient Knowledge"},
      { 9, 10,  50, "Floating"},
      { 10, 12,  45, "Hide in Flame"},
      { 11, 20,  40, "Nyusin"},
      { 12,  5,  50, "Syuriken Spreading"},
      { 13, 15,  55, "Chain Hook"},
      { 14, 32,  60, "Smoke Ball"},
      { 15, 32,  60, "Swap Position"},
      { 16, 30,  70, "Glyph of Explosion"},
      { 17, 40,  40, "Hide in Mud"},
      { 18, 35,  50, "Hide in Mist"},
      { 19, 40,  60, "Rengoku-Kaen"},
      { 20, 50,  55, "Bunshin"},
      { 99,  0,   0, ""},
      
    }
  },
};

/* This function could be _much_ _*MUCH*_ more informative */
void mindcraft_info(char *p, int use_mind, int power)
{
	/* going to remove this and add in skills */
	int plev = p_ptr->lev;

	strcpy(p, "");

	switch (use_mind)
    {
		case MIND_OFFICER:		
		switch (power)
			{
				case 0:  strcpy(p, " fear & confusion"); break;
				case 1:  strcpy(p, " stun monsters"); break;
				case 2:  strcpy(p, " charm monsters"); break;
				case 3:  sprintf(p, " heal %dd%d", (p_ptr->skills[SK_OFFICER].skill_rank), (p_ptr->skills[SK_OFFICER].skill_rank * 4)); break;
				case 4:  strcpy(p, " detect monsters"); break;
				case 5:  sprintf(p, " prot. %d + 1d25", (6 * p_ptr->skills[SK_OFFICER].skill_rank)); break;
				case 6:  strcpy(p, " mass charm"); break;
				case 7:  break;
				case 8:  break;
				case 9:  break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
			    case 15: break;
			    case 16: break;
			    case 17: break;
			    case 18: break;
			    case 19: break;
			    case 20: break;
			}
		break;
    	case MIND_AESTHETE:				
    	switch (power)
	  		{
		  		case 0:  strcpy(p, " fear or aggravates"); break;
				case 1:  strcpy(p, " treasure sense"); break;
				case 2:  strcpy(p, " enhance weapon"); break;
				case 3:  strcpy(p, " enhance armor"); break;
				case 4:  strcpy(p, " identify");  break;
				case 5:  sprintf(p, " range %d", plev * 2); break;
				case 6:  strcpy(p, " recharge"); break;
				case 7:  strcpy(p, " midas touch"); break;
				case 8:  strcpy(p, " recall"); break;
				case 9:  break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
			    case 15: break;
			    case 16: break;
			    case 17: break;
			    case 18: break;
			    case 19: break;
			    case 20: break;

	  		}
	  	break;
    	case MIND_EXPLORER:	
    	switch (power)
	    	{
		    	case 0:  strcpy(p, " light"); break;
				case 1:  strcpy(p, " detection"); break;
				case 2:  sprintf(p, " range %d", p_ptr->skills[SK_EXPLORER].skill_rank * 3); break;
				case 3:  strcpy(p, " satisfy hunger");  break;
				case 4:  strcpy(p, " identify"); break;
				case 5:  strcpy(p, " recall"); break;
				case 6:  strcpy(p, " recharge"); break;
				case 7:  strcpy(p, " extract resources"); break;
				case 8:  sprintf(p, " dur %d-%d", (plev + 1), (plev + plev)); break;
				case 9:  break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
			    case 15: break;
			    case 16: break;
			    case 17: break;
			    case 18: break;
			    case 19: break;
			    case 20: break;
			}
		break;
		case MIND_MEDIUM: 	
		switch (power)
	    	{
    			case 0:  sprintf(p, " dam %dd%d", 3 + (((p_ptr->skills[SK_MEDIUM].skill_rank) / 2) + 2), (3 + p_ptr->skills[SK_MEDIUM].skill_rank / 6));  break;
				case 1:  strcpy(p, " detect"); break;
				case 2:  sprintf(p, " range %d", (p_ptr->skills[SK_MEDIUM].skill_rank * 2)); break;
				case 3:  sprintf(p, " range %d", p_ptr->skills[SK_MEDIUM].skill_rank * 10); break;
				case 4:  strcpy(p, " chaos"); break;
				case 5:  sprintf(p, " dam %dd8", p_ptr->skills[SK_MEDIUM].skill_rank); break;
				case 6:  sprintf(p, " dur %d", (p_ptr->skills[SK_MEDIUM].skill_rank + p_ptr->skills[SK_MESMERIC_WILL].skill_rank * 2));break;
				case 7:  strcpy(p, " identify"); break;
				case 8:  strcpy(p, " dominate"); break;
				case 9:  sprintf(p, " dam %dd4", (p_ptr->skills[SK_MEDIUM].skill_rank + p_ptr->skills[SK_MESMERIC_WILL].skill_rank) * 2); break;
				case 10:  sprintf(p, " dur 11-%d", ((p_ptr->skills[SK_MEDIUM].skill_rank + p_ptr->skills[SK_MESMERIC_WILL].skill_rank) * 2) + 10); break;
				case 11: sprintf(p, " dam %dd3 Stun", (p_ptr->skills[SK_MEDIUM].skill_rank + p_ptr->skills[SK_MESMERIC_WILL].skill_rank) * 2); break;
				case 12: sprintf(p, " dam %d", p_ptr->skills[SK_MEDIUM].skill_rank * (p_ptr->skills[SK_MESMERIC_WILL].skill_rank / 3)); break;
				case 13: break;
				case 14: break;
			    case 15: break;
			    case 16: break;
			    case 17: break;
			    case 18: break;
			    case 19: break;
			    case 20: break;

			}
		break;
		case MIND_RECKONER: 	
		switch (power)
	    	{
    			case 0:  sprintf(p, " range %d", (p_ptr->skills[SK_RECKONER].skill_rank < 15) ? p_ptr->skills[SK_RECKONER].skill_rank : (p_ptr->skills[SK_RECKONER].skill_rank * 3)); break;
				case 1:  strcpy(p, " light"); break;
				case 2:  strcpy(p, " time"); break;
				case 3:  sprintf(p, " dam %dd%d", (p_ptr->skills[SK_RECKONER].skill_rank), (p_ptr->skills[SK_RECKONER].skill_rank)); break;
				case 4:  sprintf(p, " dam %dd%d", (p_ptr->skills[SK_RECKONER].skill_rank), (p_ptr->skills[SK_RECKONER].skill_rank)); break;
				case 5:  sprintf(p, " heal %dd%d", (p_ptr->skills[SK_RECKONER].skill_rank), (p_ptr->skills[SK_RECKONER].skill_rank * 2)); break;
				case 6:  strcpy(p, " recall"); break;
				case 7:  strcpy(p, " polymorph"); break;
				case 8:  strcpy(p, " mapping"); break;
				case 9:  strcpy(p, " total rewrite"); break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
			    case 15: break;
			    case 16: break;
			    case 17: break;
			    case 18: break;
			    case 19: break;
			    case 20: break;
			}
		break;
		case MIND_TOURIST: 	
		switch (power)
	    	{
    			case 0:  sprintf(p, " range %d", ((p_ptr->skills[SK_TOURIST].skill_rank / 4) + 1)); break;
				case 1:  sprintf(p, " range %d", (p_ptr->skills[SK_TOURIST].skill_rank / 2) + 2); break;
				case 2:  sprintf(p, " range %d", p_ptr->skills[SK_TOURIST].skill_rank * 2); break;
				case 3:  sprintf(p, " range %d", (p_ptr->skills[SK_TOURIST].skill_rank * 10)); break;
				case 4:  break;
				case 5:  break;
				case 6:  break;
				case 7:  break;
				case 8:  break;
				case 9:  break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
			    case 15: break;
			    case 16: break;
			    case 17: break;
			    case 18: break;
			    case 19: break;
			    case 20: break;
			}
		break;	
		case MIND_HUSSAR: 	
		switch (power)
	    	{
    			case 0:  break;
				case 1:  break;
				case 2:  break;
				case 3:  break;
				case 4:  break;
				case 5:  break;
				case 6:  break;
				case 7:  break;
				case 8:  break;
				case 9:  break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
			    case 15: break;
			    case 16: break;
			    case 17: break;
			    case 18: break;
			    case 19: break;
			    case 20: break;
			}
		break;
		case MIND_NATURE: 	
		switch (power)
	    	{
    			case 0:  break;
				case 1:  strcpy(p, " heal 2d8"); break;
				case 2:  break;
				case 3:  break;
				case 4:  sprintf(p, " damage 2d%d", p_ptr->skills[SK_RECKONER].skill_rank * 3); break;
				case 5:  break;
				case 6:  break;
				case 7:  sprintf(p, " heal %dd8", plev); break;
				case 8:  break;
				case 9:  sprintf(p, " damage %dd8", (3 + (p_ptr->skills[SK_RECKONER].skill_rank / 3))); break;
				case 10: break;
				case 11: break;
				case 12: strcpy(p, " dur 1d20 + 30"); break;
				case 13: strcpy(p, " dur 1d20 + 20"); break;
				case 14: break;
			    case 15: break;
			    case 16: sprintf(p, " damage %dd%d", (70 + (p_ptr->skills[SK_RECKONER].skill_rank * 3)), (p_ptr->skills[SK_RECKONER].skill_rank / 5) + 1); break;
			    case 17: sprintf(p, " damage %dd%d", (90 + (p_ptr->skills[SK_RECKONER].skill_rank * 3)), (p_ptr->skills[SK_RECKONER].skill_rank / 5) + 1); break;
			    case 18: sprintf(p, " damage %dd%d", (100 + (p_ptr->skills[SK_RECKONER].skill_rank * 3)), (p_ptr->skills[SK_RECKONER].skill_rank / 5) + 1); break;
			    case 19: strcpy(p, " 150d8"); break;
			    case 20: break;
			}	
		break;
		case MIND_NINJA: 	
		switch (power)
	    	{
    			case 0:  break;
				case 1:  break;
				case 2:  break;
				case 3:  break;
				case 4:  break;
				case 5:  break;
				case 6:  break;
				case 7:  break;
				case 8:  break;
				case 9:  break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
			    case 15: break;
			    case 16: break;
			    case 17: break;
			    case 18: break;
			    case 19: break;
			    case 20: break;
			}
		break;
    }
	
}
	
/*
 * Allow user to choose a class power.
 *
 * If a valid spell is chosen, saves it in '*sn' and returns TRUE
 * If the user hits escape, returns FALSE, and set '*sn' to -1
 * If there are no legal choices, returns FALSE, and sets '*sn' to -2
 *
 * The "prompt" should be "cast", "recite", or "study"
 * The "known" should be TRUE for cast/pray, FALSE for study
 *
 * nb: This function has a (trivial) display bug which will be obvious
 * when you run it. It's probably easy to fix but I haven't tried,
 * sorry.
 */
 
 /* making changes for skills */

static int get_class_power(int *sn)
    {
      int 	i;
      int 	num = 0;
      int   y = 1;
      int   x = 10;
      int   minfail =0;
      int   skill;
      int   chance;
      int   ask;
      int   use_mind;
      char  choice;
      char  out_val[160];
      char  comment[80];
      cptr  p;

      mind_type  spell;
      mind_power *mind_ptr;
      bool       flag, redraw;

      
    /* Insert if /if else statement here for classflag. */
    
    if (p_ptr->pclass == CLASS_OFFICER)
    {
    	use_mind = MIND_OFFICER;
	    p = "commanding presence";
	    skill = SK_OFFICER;
    }
    else if (p_ptr->pclass == CLASS_AESTHETE)
	{
	    use_mind = MIND_AESTHETE;
	    p = "skill";
	    skill = SK_AESTHETE;
    }
    else if (p_ptr->pclass == CLASS_EXPLORER)
	{
	    use_mind = MIND_EXPLORER;
	    p = "talent";
	    skill = SK_EXPLORER;
    }
    else if (p_ptr->pclass == CLASS_MEDIUM)
	{
	    use_mind = MIND_MEDIUM;
	    p = "mental power";
	    skill = SK_MEDIUM;
    }
    else if (p_ptr->pclass == CLASS_RECKONER)
	{
	    use_mind = MIND_RECKONER;
	    p = "reality alteration";
	    skill = SK_RECKONER;
    }
    else if (p_ptr->pclass == CLASS_TOURIST)
	{
	    use_mind = MIND_TOURIST;
	    p = "tourism dept.";
	    skill = SK_TOURIST;
    }
    else if (p_ptr->pclass == CLASS_DASHING_H)
	{
	    use_mind = MIND_HUSSAR;
	    p = "combat techniques";
	    skill = SK_HUSSAR;
    }
    else if (p_ptr->pclass == CLASS_NATURAL)
	{
	    use_mind = MIND_NATURE;
	    p = "natural empathy";
	    skill = SK_NATURE;
    }
    else if (p_ptr->pclass == CLASS_NINJA)
	{
	    use_mind = MIND_NINJA;
	    p = "ninjutsu";
	    skill = SK_NINJA;
    }
    else
    {
    	msg_print("You have no powers.");
    	return (FALSE);
    }

    mind_ptr = &mind_powers[use_mind];

	/* Assume cancelled */
	*sn = (-1);

#ifdef ALLOW_REPEAT /* TNB */

	/* Get the spell, if available */
	if (repeat_pull(sn))
	{
		/* Verify the spell */
		if (mind_ptr->info[*sn].min_lev <= p_ptr->skills[skill].skill_rank)
		{
			/* Success */
			return (TRUE);
		}
	}

#endif /* ALLOW_REPEAT -- TNB */

	/* Nothing chosen yet */
	flag = FALSE;

	/* No redraw yet */
	redraw = FALSE;

	for (i = 0; i < MAX_CLASS_POWERS; i++)
	{
		if (mind_ptr->info[i].min_lev <= p_ptr->skills[skill].skill_rank)
		{
			num++;
		}
	}

    /* Build a prompt (accept all spells) */
	(void)strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) Use which %s? ",
			p, I2A(0), I2A(num - 1), p);
	
	/* Get a spell from the user */
	while (!flag && get_com(out_val, &choice))
	{
		/* Request redraw */
		if ((choice == ' ') || (choice == '*') || (choice == '?'))
		{
			/* Show the list */
			if (!redraw)
			{
				char psi_desc[80];
				
				/* Show list */
				redraw = TRUE;

				/* Save the screen */
				screen_save();
				
				prt("", y, x);
				put_str("Name", y, x + 5);

				put_str(format("Rank  MP  Fail Info"), y, x + 35);
				

				
				
				/* Dump the spells */
				for (i = 0; i < MAX_CLASS_POWERS; i++)
				{
					/* Access the spell */
					spell = mind_ptr->info[i];
					if (spell.min_lev > p_ptr->skills[skill].skill_rank)   break;

					chance = spell.fail;

					/* Reduce failure rate by "effective" level adjustment */
					chance -= 3 * (p_ptr->skills[skill].skill_rank - spell.min_lev);

					/* Reduce failure rate by INT/WIS adjustment */
					chance -= 3 * (p_ptr->stat_use[cp_ptr->spell_stat] / 50);

					/* Not enough mana to cast */
					if (spell.mana_cost > p_ptr->csp)
					{
						chance += 5 * (spell.mana_cost - p_ptr->csp);
					}

					/* Extract the minimum failure rate */
					minfail = 15 - p_ptr->stat_use[cp_ptr->spell_stat] / 50;
					if (minfail < 0) minfail = 0;

					/* Minimum failure rate */
					if (chance < minfail) chance = minfail;

					/* Stunning makes spells harder */
					if (p_ptr->stun > 50) chance += 25;
					else if (p_ptr->stun) chance += 15;

					/* Always a 5 percent chance of working */
					if (chance > 95) chance = 95;

					/* Get info */
					mindcraft_info(comment, use_mind, i);
					
					sprintf(psi_desc, "  %c) ",I2A(i));
					
					 strcat(psi_desc,
					       format("%-30s%2d %4d %3d%%%s",
						      spell.name, spell.min_lev, spell.mana_cost,
						      chance, comment));
					prt(psi_desc, y + i + 1, x);
					 
				}	

				/* Clear the bottom line */
				prt("", y + i + 1, x);
			}

			/* Hide the list */
			else 
			{
				/* Hide list */
				redraw = FALSE;

				/* Restore the screen */
				screen_load();
			}

			/* Redo asking */
			continue;
		}

		/* Note verify */
		ask = isupper(choice);

		/* Lowercase */
		if (ask) choice = tolower(choice);

		/* Extract request */
		i = (islower(choice) ? A2I(choice) : -1);

		/* Totally Illegal */
		if ((i < 0) || (i >= num))
		{
			bell("Illegal class power choice!");
			continue;
		}

		/* Save the spell index */
		spell = mind_ptr->info[i];

		/* Verify it */
		if (ask)
		{
			char tmp_val[160];

			/* Prompt */
			(void)strnfmt(tmp_val, 78, "Use %s? ", spell.name);


			/* Belay that order */
			if (!get_check(tmp_val)) continue;
		}

		/* Stop the loop */
		flag = TRUE;
	}

	/* Restore the screen */
	if (redraw) screen_load();

	/* Show choices */
	/* Update */
	p_ptr->window |= (PW_OVERHEAD);

	/* Window stuff */
	window_stuff();
	
	/* Abort if needed */
	if (!flag) return (FALSE);

	/* Save the choice */
	(*sn) = i;

#ifdef ALLOW_REPEAT /* TNB */

	repeat_push(*sn);

#endif /* ALLOW_REPEAT -- TNB */

	/* Success */
	return (TRUE);
}

/*
 * do_cmd_mind calls this function if the player's class
 * is 'Officer'.
 */
static bool cast_officer_spell(int spell)
{
	/* this will vary based on the spells, and what they depend on */
	int				dir;
	int 			time = randint(20) + 20;
	
	/* spell code */
	switch (spell)
	{
    			case 0:  if (!get_aim_dir(&dir)) return FALSE;
							(void)fear_monster(dir, p_ptr->skills[SK_OFFICER].skill_rank);
						 break;
				case 1:  if (!get_aim_dir(&dir)) return FALSE;
							(void)fire_bolt(GF_STUN, dir,
							damroll(p_ptr->skills[SK_OFFICER].skill_rank, 5));
						 break;
				case 2:  if (!get_aim_dir(&dir)) return FALSE; 
						 	(void)fire_bolt(GF_CHARM, dir,
						 	damroll(p_ptr->skills[SK_PRESENCE].skill_rank, p_ptr->skills[SK_PRESENCE].skill_rank));
						 break;
				case 3:  (void)hp_player(damroll((p_ptr->skills[SK_OFFICER].skill_rank), (p_ptr->skills[SK_OFFICER].skill_rank * 4)));
						 if (p_ptr->skills[SK_OFFICER].skill_rank > 13)
						 {
						 (void)set_afraid(0);
						 (void)set_stun(0);
						 (void)set_cut(0);
						 }
						 if (p_ptr->skills[SK_OFFICER].skill_rank > 19)
						 {
						 (void)do_res_stat(A_MUS);
				 		 (void)do_res_stat(A_AGI);
				 		 (void)do_res_stat(A_VIG);
				 		 (void)do_res_stat(A_SCH);
				 		 (void)do_res_stat(A_EGO);
				 		 (void)do_res_stat(A_CHR);
						 }
						 break;
				case 4:  (void)detect_monsters_normal();
						 break;
				case 5:  (void)set_protevil(p_ptr->protevil + randint(25) + 6 * p_ptr->skills[SK_OFFICER].skill_rank);
						if (!p_ptr->fast)
						{
							(void)set_fast(randint(20) + p_ptr->skills[SK_OFFICER].skill_rank * 2);
						}
						else
						{
							(void)set_fast(p_ptr->fast + randint(5));
						}
						if (p_ptr->skills[SK_OFFICER].skill_rank > 18)
						{
						(void)set_oppose_acid(p_ptr->oppose_acid + time);
						(void)set_oppose_elec(p_ptr->oppose_elec + time);
						(void)set_oppose_fire(p_ptr->oppose_fire + time);
						(void)set_oppose_cold(p_ptr->oppose_cold + time);
						(void)set_oppose_pois(p_ptr->oppose_pois + time);
						}
						if (p_ptr->skills[SK_PRESENCE].skill_rank > 15)
						{
						(void)set_invuln(p_ptr->invuln + (2 + randint(2)));
						}
						break;
				case 6:  if (!get_aim_dir(&dir)) return FALSE;
							fire_ball(GF_CHARM, dir,
				        				  (p_ptr->skills[SK_PRESENCE].skill_rank * 10), (p_ptr->skills[SK_PRESENCE].skill_rank / 3));
						 break;
				case 7:  break;
				case 8:  break;
				case 9:  break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
				case 15: break;
				case 16: break;
			    case 17: break;
			    case 18: break;
			    case 19: break;
			    case 20: break;
	}

	return TRUE;
}

/*
 * do_cmd_mind calls this function if the player's class
 * is 'Aesthete'.
 */
static bool cast_aesthete_spell(int spell)
{
	/* this will vary based on the spells, and what they depend on */
	int py = p_ptr->py;
	int px = p_ptr->px;
	int radius;
	
	/* spell code */
	switch (spell)
	{
    			case 0:  
    			/* Arrogant Laugh (almost bad) */
    			if ((p_ptr->skills[SK_AESTHETE].skill_rank * 2) > (randint(36) + 4))
    			{
    				/* for staying in bounds of max 16 range */
    				radius = (p_ptr->skills[SK_AESTHETE].skill_rank * 3);
    				/* paranoia */
    				if (radius > 16)
    				{
    					radius = 16;
    				}
	    			msg_print("Creatures Cower in fear before you!");
					project(-1, radius, py, px, 
								(p_ptr->skills[SK_AESTHETE].skill_rank * 8), GF_TURN_ALL, PROJECT_KILL);

				}	
    			else
    			{
    			msg_print("Your Arrogant Laugh angers the monsters around you!");
    			aggravate_monsters(-1);
       			}	
    			break;
    			/* Treasure Sense */
				case 1: if (p_ptr->skills[SK_AESTHETE].skill_rank > 18)
						{
							detect_traps();
						}
						if (p_ptr->skills[SK_AESTHETE].skill_rank > 16)
						{
							detect_objects_magic();
						}
						if (p_ptr->skills[SK_AESTHETE].skill_rank > 14)
						{
							detect_objects_normal();
						}
						if (p_ptr->skills[SK_AESTHETE].skill_rank > 10)
						{
							detect_objects_gold();
						}
						detect_treasure();
						break;
				/* Enhance weapon */
				case 2: if (p_ptr->skills[SK_AESTHETE].skill_rank > 19) 
						{
							brand_weapon();
						}
						else
						{
							(void)enchant_spell(rand_int(p_ptr->skills[SK_AESTHETE].skill_rank) + 1, rand_int(p_ptr->skills[SK_AESTHETE].skill_rank) + 1, 0);
						}
						break;
				/* Enhance Armor */
				case 3: (void)enchant_spell(0, 0, rand_int(p_ptr->skills[SK_AESTHETE].skill_rank) + 1);
						break;
				/* Legendary lore */
				case 4: if (p_ptr->skills[SK_AESTHETE].skill_rank > 19)
						{
						identify_fully();
						} 
						else 
						{
						ident_spell();
						}
						 break;
				case 5:  teleport_player(p_ptr->skills[SK_AESTHETE].skill_rank * 2);  break;
				case 6:  recharge(p_ptr->skills[SK_AESTHETE].skill_rank * 3); break;
				case 7:  alchemy(); break;
				case 8:  set_recall(); break;
				case 9:  break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
			    case 15: break;
			    case 16: break;
			    case 17: break;
			    case 18: break;
			    case 19: break;
			    case 20: break;
	}

	return TRUE;
}

/*
 * do_cmd_mind calls this function if the player's class
 * is 'explorer'.
 */
static bool cast_explorer_spell(int spell)
{
	/* this will vary based on the spells, and what they depend on */
	int b = 0;
	int	px = p_ptr->px;
	int py = p_ptr->py;
	
	/* spell code */
	switch (spell)
	{
    			case 0:  lite_room(py, px);
    					 break;
				case 1:  if (p_ptr->skills[SK_EXPLORER].skill_rank > 19)
							wiz_lite();
						else if (p_ptr->skills[SK_EXPLORER].skill_rank > 7)
							map_area();
			
						if (p_ptr->skills[SK_EXPLORER].skill_rank < 10)
						{
							b = detect_monsters_normal();
							if (p_ptr->skills[SK_EXPLORER].skill_rank > 4)
								b |= detect_monsters_invis();
			
							if (p_ptr->skills[SK_EXPLORER].skill_rank > 1)
							{
								b |= detect_traps();
								b |= detect_doors();
							}
						}
						else
						{
							b = detect_all();
						}
			
						if (p_ptr->skills[SK_EXPLORER].skill_rank > 14)
						{
							(void)set_tim_esp(p_ptr->tim_esp + (p_ptr->skills[SK_EXPLORER].skill_rank * 3));
						}
			
						if (!b) msg_print("You feel safe.");
						 break;
				case 2:  teleport_player(p_ptr->skills[SK_EXPLORER].skill_rank * 3);
						 break;
				case 3:  (void)set_food(PY_FOOD_MAX - 1);
						 break;
				case 4:  ident_spell();
						 break;
				case 5:  set_recall();
						 break;
				case 6:  (void)recharge(30);
						 break;
				case 7:  alchemy();
						 break;
				case 8: {
						 int time = randint(p_ptr->skills[SK_EXPLORER].skill_rank * 2) + (p_ptr->skills[SK_EXPLORER].skill_rank * 2);
						(void)set_oppose_acid(p_ptr->oppose_acid + time);
						(void)set_oppose_elec(p_ptr->oppose_elec + time);
						(void)set_oppose_fire(p_ptr->oppose_fire + time);
						(void)set_oppose_cold(p_ptr->oppose_cold + time);
						(void)set_oppose_pois(p_ptr->oppose_pois + time); 
						 break;
						 }
				case 9:  break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
			    case 15: break;
			    case 16: break;
			    case 17: break;
			    case 18: break;
			    case 19: break;
			    case 20: break;
	}

	return TRUE;
}

/*
 * do_cmd_mind calls this function if the player's class
 * is 'Medium'.
 */
static bool cast_medium_spell(int spell)
{
	/* this will vary based on the spells, and what they depend on */
	int             b = 0;
	int 			dir;
	int             plev = p_ptr->lev;

	/* spell code */
	switch (spell)
	{
		case 0:
			/* Mindblast */
			if (!get_aim_dir(&dir)) return FALSE;

			if (randint(60) < p_ptr->skills[SK_MESMERIC_WILL].skill_rank * 3)
				(void)fire_bolt_or_beam(100, GF_PSI, dir,
								damroll(3 + (((p_ptr->skills[SK_MEDIUM].skill_rank) / 2) + 2), (3 + p_ptr->skills[SK_MEDIUM].skill_rank / 6)));
			else
				(void)fire_ball(GF_PSI, dir,
								damroll(3 + (((p_ptr->skills[SK_MEDIUM].skill_rank) / 2) + 2), (3 + p_ptr->skills[SK_MEDIUM].skill_rank / 6)),
								0);
			break;
		case 1:if (p_ptr->skills[SK_MEDIUM].skill_rank > 19)
							wiz_lite();
						else if (p_ptr->skills[SK_MEDIUM].skill_rank > 7)
							map_area();
			
						if (p_ptr->skills[SK_MEDIUM].skill_rank < 10)
						{
							b = detect_monsters_normal();
							if (p_ptr->skills[SK_MEDIUM].skill_rank > 4)
								b |= detect_monsters_invis();
			
							if (p_ptr->skills[SK_MEDIUM].skill_rank > 1)
							{
								b |= detect_traps();
								b |= detect_doors();
							}
						}
						else
						{
							b = detect_all();
						}
			
						if (p_ptr->skills[SK_MEDIUM].skill_rank > 14)
						{
							(void)set_tim_esp(p_ptr->tim_esp + (p_ptr->skills[SK_MEDIUM].skill_rank * 3));
						}
			
						if (!b) msg_print("You feel safe.");
			break;
		case 2:
			/* Minor displace */
			teleport_player(p_ptr->skills[SK_MEDIUM].skill_rank * 2);
			break;
		case 3:
			/* Major displace */
			teleport_player(p_ptr->skills[SK_MEDIUM].skill_rank * 10);
			break;
		case 4:
			/* Psychic Disturbance */
			msg_print("You disturb specters from beyond the veil!");
			(void)project(-1, 2 + p_ptr->skills[SK_MESMERIC_WILL].skill_rank / 3, p_ptr->py, p_ptr->px,
						  damroll((p_ptr->skills[SK_MEDIUM].skill_rank + p_ptr->skills[SK_MESMERIC_WILL].skill_rank), ((p_ptr->skills[SK_MEDIUM].skill_rank + p_ptr->skills[SK_MESMERIC_WILL].skill_rank) * 2)), GF_CONFUSION, PROJECT_KILL);
			break;
		case 5:
			/* spirit blast  ---  not 'true' TK */
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_SOUND, dir, damroll(p_ptr->skills[SK_MEDIUM].skill_rank, 8),
							(p_ptr->skills[SK_MESMERIC_WILL].skill_rank > 5 ? (p_ptr->skills[SK_MESMERIC_WILL].skill_rank) / 5 + 1 : 0));
			break;
		case 6:
			/* Character Armour */
			(void)set_shield(p_ptr->shield + (p_ptr->skills[SK_MEDIUM].skill_rank + p_ptr->skills[SK_MESMERIC_WILL].skill_rank * 2));
			if (plev > 14) (void)set_oppose_acid(p_ptr->oppose_acid + (p_ptr->skills[SK_MEDIUM].skill_rank + p_ptr->skills[SK_MESMERIC_WILL].skill_rank * 2));
			if (plev > 19) (void)set_oppose_fire(p_ptr->oppose_fire + (p_ptr->skills[SK_MEDIUM].skill_rank + p_ptr->skills[SK_MESMERIC_WILL].skill_rank * 2));
			if (plev > 24) (void)set_oppose_cold(p_ptr->oppose_cold + (p_ptr->skills[SK_MEDIUM].skill_rank + p_ptr->skills[SK_MESMERIC_WILL].skill_rank * 2));
			if (plev > 29) (void)set_oppose_elec(p_ptr->oppose_elec + (p_ptr->skills[SK_MEDIUM].skill_rank + p_ptr->skills[SK_MESMERIC_WILL].skill_rank * 2));
			if (plev > 34) (void)set_oppose_pois(p_ptr->oppose_pois + (p_ptr->skills[SK_MEDIUM].skill_rank + p_ptr->skills[SK_MESMERIC_WILL].skill_rank * 2));
			break;
		case 7:
			ident_spell();
			break;
		case 8:
			if (!get_aim_dir(&dir)) return FALSE; 
			(void)fire_bolt(GF_DOMINATION, dir,
					damroll((p_ptr->skills[SK_MEDIUM].skill_rank), (1 + p_ptr->skills[SK_MESMERIC_WILL].skill_rank)));
			break;
		case 9:
			/* Soul Purge - Need to change this to spirit. */
			msg_print("The anguish of the dead emanates from your brain!");
			
			(void)project(-1, 2 + (p_ptr->skills[SK_MESMERIC_WILL].skill_rank / 4), p_ptr->py, p_ptr->px,
						  damroll((p_ptr->skills[SK_MEDIUM].skill_rank + p_ptr->skills[SK_MESMERIC_WILL].skill_rank) * 2, 4), GF_PSI, PROJECT_KILL);
			break;
		case 10:
			/* Adrenaline */
			(void)set_afraid(0);
			(void)set_stun(0);

			/*
			 * Only heal when Adrenalin Channeling is not active. We check
			 * that by checking if the player isn't fast and 'heroed' atm.
			 */
			if (!p_ptr->fast || !(p_ptr->hero || p_ptr->shero))
			{
				(void)hp_player((p_ptr->skills[SK_MEDIUM].skill_rank + p_ptr->skills[SK_MESMERIC_WILL].skill_rank) * 2);
			}

			b = 10 + randint((p_ptr->skills[SK_MEDIUM].skill_rank + p_ptr->skills[SK_MESMERIC_WILL].skill_rank) * 2);
			if (p_ptr->skills[SK_MESMERIC_WILL].skill_rank < 15)
				(void)set_hero(p_ptr->hero + b);
			else
				(void)set_shero(p_ptr->shero + b);

			if (!p_ptr->fast)
			{
				/* Haste */
				(void)set_fast(b);
			}
			else
			{
				(void)set_fast(p_ptr->fast + b);
			}
			break;
		case 11:
			/* Psychic Drain Turned into MEGA-STUN*/
			if (!get_aim_dir(&dir)) return FALSE;

			b = damroll((p_ptr->skills[SK_MEDIUM].skill_rank + p_ptr->skills[SK_MESMERIC_WILL].skill_rank) * 2, 3);

			/* This is always a radius-0 ball now */
			if (fire_ball(GF_STUN, dir, b, 0))
			p_ptr->energy -= randint(150);
			break;
		case 12:
			/* Entropic Blast */
			msg_print
				("A wave of pure entropic force blasts out from your spirit!");
			(void)project(-1, 3 + p_ptr->skills[SK_MESMERIC_WILL].skill_rank / 4, p_ptr->py, p_ptr->px,
						  p_ptr->skills[SK_MEDIUM].skill_rank * (p_ptr->skills[SK_MESMERIC_WILL].skill_rank / 3), GF_FORCE,
						  PROJECT_KILL | PROJECT_ITEM | PROJECT_GRID);
			break;
		default:
			msg_print("Unknown Mindcrafter power!");
	}


	return TRUE;
}

/*
 * do_cmd_mind calls this function if the player's class
 * is 'Reckoner'.
 */
static bool cast_reckoner_spell(int spell)
{
	/* this will vary based on the spells, and what they depend on */
	int 	b;
	int		py = p_ptr->py;
	int		px = p_ptr->px;
	int 	dir;
	/* for the non-living bond destoryer */
	int     flg = PROJECT_STOP | PROJECT_KILL | PROJECT_GRID;
	
	/* spell code */
	switch (spell)
	{
    			case 0:	
    					if (p_ptr->skills[SK_RECKONER].skill_rank < 15) 
    					{
    						teleport_player(p_ptr->skills[SK_RECKONER].skill_rank * 2);
    					}  
    					else
    					{
    						teleport_player(p_ptr->skills[SK_RECKONER].skill_rank * 3);
    					}
    					break;
    					
				case 1: 
						b = (randint(50) + p_ptr->skills[SK_RECKONER].skill_rank * 3);
						if (b < 10)
						{
						unlite_area(randint(p_ptr->skills[SK_RECKONER].skill_rank *2), (randint(p_ptr->skills[SK_RECKONER].skill_rank / 5) + 2));
						}
						else if (b < 25)
						{
						unlite_room(py, px);
						}
						else if  (b < 50)
						{
						lite_room(py, px);
						}
						else 
						{
						lite_area(randint(p_ptr->skills[SK_RECKONER].skill_rank * 6), (randint(p_ptr->skills[SK_RECKONER].skill_rank / 2) + 3));
						}
						
						break;
				
				case 2: b = (randint(50) + p_ptr->skills[SK_RECKONER].skill_rank * 3);
						if (b < 20)
						{
						speed_monsters();
						}
						else if (b < 30)
						{
						slow_monsters();
						}
						else if (b < 55)
						{
						sleep_monsters();
						}
						else if (b < 75)
						{
							if (!p_ptr->fast)
							{
								(void)set_fast(randint(p_ptr->skills[SK_RECKONER].skill_rank * 2) + p_ptr->skills[SK_RECKONER].skill_rank * 2);
							}
							else
							{
								(void)set_fast(p_ptr->fast + randint(5));
							}
							break;
						}
						else
						{
						(void)set_fast(p_ptr->fast + b);
						slow_monsters();
						sleep_monsters();
						}
						 break;
						/* Maybe turn this into a random bolt */
				case 3:  if (!get_aim_dir(&dir)) return FALSE; 
						 (void)fire_bolt(GF_OLD_DRAIN, dir,
								damroll((p_ptr->skills[SK_RECKONER].skill_rank), (p_ptr->skills[SK_RECKONER].skill_rank)));
						 break;
				
				case 4:  if (!get_aim_dir(&dir)) return FALSE;
						 fire_ball(GF_KILL_WALL, dir, damroll((p_ptr->skills[SK_RECKONER].skill_rank), (p_ptr->skills[SK_RECKONER].skill_rank)), 0);
						 break;
						 
				case 5:  (void)hp_player(damroll(p_ptr->skills[SK_RECKONER].skill_rank, p_ptr->skills[SK_RECKONER].skill_rank * 2));
						 (void)set_cut(0);
						 break;
						 
				case 6:  set_recall(); break;
				
				case 7:  if (!get_aim_dir(&dir)) return FALSE;
						 (void)poly_monster(dir);
						 break;
				case 8:  if (p_ptr->skills[SK_RECKONER].skill_rank < 15)
						 {
						 map_area();
						 (void)set_tim_esp(p_ptr->tim_esp + 10);
						 }
						 else
						 {
						 wiz_lite();
						 (void)set_tim_esp(p_ptr->tim_esp + p_ptr->skills[SK_RECKONER].skill_rank);
						 }
						 break;
				case 9:  msg_print("The world changes!");
			
 						 /* Leaving */
						 p_ptr->leaving = TRUE;
						 			
						 break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
			    case 15: break;
			    case 16: break;
			    case 17: break;
			    case 18: break;
			    case 19: break;
			    case 20: break;
	}

	return TRUE;
}

/*
 * do_cmd_mind calls this function if the player's class
 * is 'tourist'.
 */
static bool cast_tourist_spell(int spell)
{
	/* this will vary based on the spells, and what they depend on */
	int             plev = p_ptr->lev;

	/* spell code */
	switch (spell)
	{
    			case 0: teleport_player((p_ptr->skills[SK_TOURIST].skill_rank / 4) + 1); break;
				case 1: teleport_player((p_ptr->skills[SK_TOURIST].skill_rank / 2) + 2); break;
				case 2: teleport_player(p_ptr->skills[SK_TOURIST].skill_rank * 2); break;
				case 3: teleport_player(p_ptr->skills[SK_TOURIST].skill_rank * 10); break;
				case 4: set_recall();break;
				case 5:  break;
				case 6:  break;
				case 7:  break;
				case 8:  break;
				case 9:  break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
			    case 15: break;
			    case 16: break;
			    case 17: break;
			    case 18: break;
			    case 19: break;
			    case 20: break;
	}

	return TRUE;
}

/*
 * do_cmd_mind calls this function if the player's class
 * is 'Hussar'.
 */
static bool cast_hussar_spell(int spell)
{
	/* this will vary based on the spells, and what they depend on */

	/* spell code */
	switch (spell)
	{
    			case 0:  break;
				case 1:  break;
				case 2:  break;
				case 3:  break;
				case 4:  break;
				case 5:  break;
				case 6:  break;
				case 7:  break;
				case 8:  break;
				case 9:  break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
			    case 15: break;
			    case 16: break;
			    case 17: break;
			    case 18: break;
			    case 19: break;
			    case 20: break;
	}

	return TRUE;
}

/*
 * do_cmd_mind calls this function if the player's class
 * is 'Naturalist'.
 */
static bool cast_nature_spell(int spell)
{
	/* this will vary based on the spells, and what they depend on */
	int 			dir;
	int 			px = p_ptr->px;
	int				py = p_ptr->py;
	
	/* spell code */
	switch (spell)
	{
	case 0: /* Detect Creatures */
		(void)detect_monsters_normal();
		break;
	case 1: /* First Aid */
		(void)hp_player(damroll(2, 8));
		(void)set_cut(p_ptr->cut - 15);
		break;
	case 2: /* Detect Doors + Traps */
		(void)detect_traps();
		(void)detect_doors();
		(void)detect_stairs();
		break;
	case 3: /* Produce Food */
		(void)set_food(PY_FOOD_MAX - 1);
		break;
	case 4: /* Daylight */
		(void)lite_area(damroll(2, (p_ptr->skills[SK_NATURE].skill_rank * 3)), (p_ptr->skills[SK_NATURE].skill_rank / 4) + 1);
		break;
	case 5: /* Animal Taming */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)charm_animal(dir, p_ptr->skills[SK_NATURE].skill_rank * 3);
		break;
	case 6: /* Resist Environment */
		(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
		(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
		(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
		break;
	case 7: /* Cure Wounds + Poison */
		(void)hp_player(damroll(p_ptr->skills[SK_NATURE].skill_rank * 3, 8));
		(void)set_cut(0);
		(void)set_poisoned(0);
		break;
	case 8: /* Stone to Mud */
		if (!get_aim_dir(&dir)) return FALSE;
		(void)wall_to_mud(dir);
		break;
	case 9: /* Lightning Bolt */
		if (!get_aim_dir(&dir)) return FALSE;
		fire_bolt_or_beam(100, GF_ELEC, dir,
			damroll(3 + (p_ptr->skills[SK_NATURE].skill_rank / 3), 8));
		break;
	case 10: /* Ray of Sunlight */
		if (!get_aim_dir(&dir)) return FALSE;
		msg_print("A line of sunlight appears.");
		(void)lite_line(dir);
		break;
	case 11: /* Entangle */
		slow_monsters();
		break;
	case 12: /* Stone Skin */
		(void)set_shield(p_ptr->shield + randint(20) + 30);
		break;
	case 13: /* Resistance True */
		(void)set_oppose_acid(p_ptr->oppose_acid + randint(20) + 20);
		(void)set_oppose_elec(p_ptr->oppose_elec + randint(20) + 20);
		(void)set_oppose_fire(p_ptr->oppose_fire + randint(20) + 20);
		(void)set_oppose_cold(p_ptr->oppose_cold + randint(20) + 20);
		(void)set_oppose_pois(p_ptr->oppose_pois + randint(20) + 20);
		break;
	case 14: /* Stone Tell */
		return identify_fully();
	case 15: /* Earthquake */
		earthquake(py, px, 10);
		break;
	case 16: /* Blizzard */
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_COLD, dir, damroll(70 + (p_ptr->skills[SK_NATURE].skill_rank * 3), (p_ptr->skills[SK_NATURE].skill_rank / 5) + 1), 4);
		break;
	case 17: /* Lightning Storm */
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_ELEC, dir, damroll(90 + (p_ptr->skills[SK_NATURE].skill_rank * 3), (p_ptr->skills[SK_NATURE].skill_rank / 5) + 1), 5);
		break;
	case 18: /* Whirlpool */
		if (!get_aim_dir(&dir)) return FALSE;
		fire_ball(GF_WATER, dir, damroll(100 + (p_ptr->skills[SK_NATURE].skill_rank * 3), (p_ptr->skills[SK_NATURE].skill_rank / 5) + 1), 6);
		break;
	case 19: /* Call Sunlight */
		fire_ball(GF_LITE, 0, 150, 8);
		wiz_lite();
		break;
	case 20: /* Nature's Wrath */
		(void)dispel_monsters(p_ptr->skills[SK_NATURE].skill_rank * 10);
		earthquake(py, px, 20 + (p_ptr->skills[SK_NATURE].skill_rank));
		project(0, 1 + p_ptr->skills[SK_NATURE].skill_rank / 5, py, px,
			100 + (p_ptr->skills[SK_NATURE].skill_rank * 2), GF_DISINTEGRATE, PROJECT_KILL | PROJECT_ITEM);
		break;
	default:
		msg_format("You cast an unknown Nature spell: %d.", spell);
		msg_print(NULL);
	}

	return TRUE;
}

/*
 * do_cmd_mind calls this function if the player's class
 * is 'Ninja'.
 */
static bool cast_ninja_spell(int spell)
{

	/* spell code */
	switch (spell)
	{
    			case 0:  break;
				case 1:  break;
				case 2:  break;
				case 3:  break;
				case 4:  break;
				case 5:  break;
				case 6:  break;
				case 7:  break;
				case 8:  break;
				case 9:  break;
				case 10: break;
				case 11: break;
				case 12: break;
				case 13: break;
				case 14: break;
			    case 15: break;
			    case 16: break;
			    case 17: break;
			    case 18: break;
			    case 19: break;
			    case 20: break;
	}
	return TRUE;
}

/*
 * do_cmd_cast calls this function based on player class
 */
 void do_cmd_mind(void)
{
	int             n = 0;
	int             chance;
	int             minfail = 0;
	int             skill;
	int             old_csp = p_ptr->csp;
	mind_type       spell;
	bool            cast = FALSE;
	int             use_mind;
	
	/* not if confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	/* get power */
	if (!get_class_power(&n)) return;

	
	if (p_ptr->pclass == CLASS_OFFICER)
    	{use_mind = MIND_OFFICER;
    	 skill = SK_OFFICER;}
    else if (p_ptr->pclass == CLASS_AESTHETE)
		{use_mind = MIND_AESTHETE;
    	 skill = SK_AESTHETE;}
    else if (p_ptr->pclass == CLASS_EXPLORER)
		{use_mind = MIND_EXPLORER;
    	 skill = SK_EXPLORER;}
    else if (p_ptr->pclass == CLASS_MEDIUM)
		{use_mind = MIND_MEDIUM;
    	 skill = SK_MEDIUM;}
    else if (p_ptr->pclass == CLASS_RECKONER)
		{use_mind = MIND_RECKONER;
    	 skill = SK_RECKONER;}
    else if (p_ptr->pclass == CLASS_TOURIST)
		{use_mind = MIND_TOURIST;
    	 skill = SK_TOURIST;}
    else if (p_ptr->pclass == CLASS_DASHING_H)
		{use_mind = MIND_HUSSAR;
    	 skill = SK_HUSSAR;}
    else if (p_ptr->pclass == CLASS_NATURAL)
		{use_mind = MIND_NATURE;
    	 skill = SK_NATURE;}
    else if (p_ptr->pclass == CLASS_NINJA)
		{use_mind = MIND_NINJA;
    	 skill = SK_NINJA;}
	else {
			msg_print("You have no powers!");
			return;
		 }
		 
	spell = mind_powers[use_mind].info[n];
	
	/* Verify "dangerous" spells */
	if (spell.mana_cost > p_ptr->csp)
	{
		/* Warning */
		msg_print("You do not have enough mana to use this power.");

		/* Verify */
		if (!get_check("Attempt it anyway? ")) return;
	}

	/* Spell failure chance */
	chance = spell.fail;

	
	/* Reduce failure rate by "effective" level adjustment */
	chance -= 3 * (p_ptr->skills[skill].skill_rank - spell.min_lev);
	
	/* Reduce failure rate by spell_stat adjustment */
	chance -= 3 * (p_ptr->stat_use[cp_ptr->spell_stat] / 50);

	/* Not enough mana to cast */
	if (spell.mana_cost > p_ptr->csp)
	{
		chance += 5 * (spell.mana_cost - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = 15 - p_ptr->stat_use[cp_ptr->spell_stat] / 50;
	if (minfail < 0) minfail = 0;

	/* Minimum failure rate */
	if (chance < minfail) chance = minfail;

	/* Stunning makes spells harder */
	if (p_ptr->stun > 50) chance += 25;
	else if (p_ptr->stun) chance += 15;

	/* Always a 5 percent chance of working */
	if (chance > 95) chance = 95;

	/* Failed spell */
	if (randint(100) < chance)
	{
		if (flush_failure) flush();
		msg_format("You failed to concentrate hard enough!");

		/* Take a turn */
		p_ptr->energy_use = 100;
		
		/* Sufficient mana */
		if (spell.mana_cost <= old_csp)
		{
			/* Use some mana */
			p_ptr->csp -= spell.mana_cost;
	
			/* Limit */
			if (p_ptr->csp < 0) p_ptr->csp = 0;
		}
	
		/* Over-exert the player */
		else
		{
			int oops = spell.mana_cost - old_csp;
	
			/* No mana left */
			p_ptr->csp = 0;
			p_ptr->csp_frac = 0;
			
			/* Message */
			msg_print("You faint from the effort!");
	
			/* Hack -- Bypass free action */
			(void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));
	
			/* Damage WIS (possibly permanently) */
			if (randint(100) < 50)
			{
				bool perm = (randint(100) < 25);
	
				/* Message */
				msg_print("You have damaged your mind!");
	
	
				/* Reduce constitution */
				(void)dec_stat(A_EGO, 15 + randint(10), perm);
			}
		}

		/* Redraw mana */
		p_ptr->redraw |= (PR_MANA);

		/* Window stuff */
		p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);

	}
	else
	{
		sound(SOUND_ZAP);

		switch(use_mind)
		{
		case MIND_OFFICER:
			/* Cast the spell */
			cast = cast_officer_spell(n);
			break;
		case MIND_AESTHETE:
			/* Cast the spell */
			cast = cast_aesthete_spell(n);
			break;
		case MIND_EXPLORER:
			/* Cast the spell */
			cast = cast_explorer_spell(n);
			break;
		case MIND_MEDIUM:
			/* Cast the spell */
			cast = cast_medium_spell(n);
			break;
		case MIND_RECKONER:
			/* Cast the spell */
			cast = cast_reckoner_spell(n);
			break;
		case MIND_TOURIST:
			/* Cast the spell */
			cast = cast_tourist_spell(n);
			break;
		case MIND_HUSSAR:
			/* Cast the spell */
			cast = cast_hussar_spell(n);
			break;
		case MIND_NATURE:
			/* Cast the spell */
			cast = cast_nature_spell(n);
			break;
		case MIND_NINJA:
			/* Cast the spell */
			cast = cast_ninja_spell(n);
			break;
		default:
			msg_format("Mystery power:%d, %d",use_mind, n);
			return;
		}
	}
	if (!cast) return;
	
	/* Take a turn */
	p_ptr->energy_use = 100;
	
	/* Sufficient mana */
	if (spell.mana_cost <= old_csp)
	{
		/* Use some mana */
		p_ptr->csp -= spell.mana_cost;

		/* Limit */
		if (p_ptr->csp < 0) p_ptr->csp = 0;
	}

	/* Over-exert the player */
	else
	{
		int oops = spell.mana_cost - old_csp;

		/* No mana left */
		p_ptr->csp = 0;
		p_ptr->csp_frac = 0;
		
		/* Message */
		msg_print("You faint from the effort!");

		/* Hack -- Bypass free action */
		(void)set_paralyzed(p_ptr->paralyzed + randint(5 * oops + 1));

		/* Damage WIS (possibly permanently) */
		if (randint(100) < 50)
		{
			bool perm = (randint(100) < 25);

			/* Message */
			msg_print("You have damaged your mind!");


			/* Reduce constitution */
			(void)dec_stat(A_EGO, 15 + randint(10), perm);
		}
	}

	/* Redraw mana */
	p_ptr->redraw |= (PR_MANA);

	/* Window stuff */
	p_ptr->window |= (PW_PLAYER_0 | PW_PLAYER_1);
}
/* add in do cmd browse here */
/* Going to finish adding in a browse command later 
 *
 *void do_cmd_mind_browse(void)
 *{
 *	int n = 0;
 *	int j, line;
 *	char temp[62*5];
 *	int use_mind = 0;
 *
 *	if ((cp_ptr->flags) & CF_OFFICER)
 *   	{use_mind = MIND_OFFICER;}
 *    else if ((cp_ptr->flags) & CF_AESTHETE)
 *		{use_mind = MIND_AESTHETE;}
 *    else if ((cp_ptr->flags) & CF_EXPLORER)
 *		{use_mind = MIND_EXPLORER;}
 *    else if ((cp_ptr->flags) & CF_MEDIUM)
 *		{use_mind = MIND_MEDIUM;}
 *    else if ((cp_ptr->flags) & CF_RECKONER)
 *		{use_mind = MIND_RECKONER;}
 *    else if ((cp_ptr->flags) & CF_TOURIST)
 *		{use_mind = MIND_TOURIST;}
 *    else if ((cp_ptr->flags) & CF_HUSSAR)
 *		{use_mind = MIND_HUSSAR;}
 *    else if ((cp_ptr->flags) & CF_NATURE)
 *		{use_mind = MIND_NATURE;}
 *    else if((cp_ptr->flags) & CF_NINJA)
 *		{use_mind = MIND_NINJA;}
 *	else{
 *		msg_print("Error, no powers!");	
 *		}
 *	screen_save();
 */
	/* Hmmmm, need to put some display code in here */
/*	screen_load();
 *}
 */
