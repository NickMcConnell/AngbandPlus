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
 * I come so far only because I stand on the shoulders of giants.
 */
 
 #include "angband.h"
 
 mind_power mind_powers[9] =
 {
  /* Officer Powers */
  {
    {
      /* Level gained,  cost,  %fail,  name */
      { 1,  99,  99, "not implimented 'til pets"},
      { 1,  99,  99, "but, hey, they make a pretty decent warrior"},
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
  
  /* Aesthete Powers */
  
  {
    {
      /* Level gained,  cost,  %fail,  name */
      { 1,   1,  10, "Arrogant Laugh"},
      { 3,   2,  10, "Treasure Sense"},
      { 7,  20,  95, "Enhance Weapon"},
      { 7,  20,  95, "Enhance Armor"},
      { 10, 10,  40, "Legendary Lore"},
      { 12,  5,  30, "Duck out the Back"},
      { 15, 30,  80, "Item Overhaul"},
      { 21, 40,  70, "Touch of Fortune"},
      { 25, 20,  80, "Merchant's Trail"},
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
      { 1,  1,   35, "Light area"},
      { 2,  3,   40, "Detection"},
      { 5,  5,   35, "Portal"},
      { 15, 10,  50, "Satisfy Hunger"},
      { 30, 20,  70, "Identify"},
      { 35, 25,  65, "Word of Recall"},
      { 40, 60,  95, "Recharge Item"},
      { 45, 30,  80, "Alchemy"},
      { 50, 25,  40, "Resistance"},
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
		{1, 1, 15, "Neural Blast"},
		{2, 1, 20, "Precognition"},
		{3, 2, 25, "Minor Displacement"},
		{7, 6, 35, "Major Displacement"},
		{9, 7, 50, "Psychic Disturbance"},
		{11, 7, 30, "Spirit Blast"},
		{13, 12, 50, "Character Armour"},
		{15, 12, 60, "Psychometry"},
		{18, 10, 45, "Soul Purge"},
		{23, 15, 50, "Drawing upon the Sprits"},
		{25, 10, 40, "Psychic Drain"},
		{28, 20, 45, "Entropic blast"},
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
  
  /* Reckoner Powers */
  /* Reality altering powers, . I'll have to work on this a bit */
  {
    {
      /* Level gained,  cost,  %fail,  name */
      { 1,  1,   25, "Spacial Distortion"},
      { 3,  2,   45, "Photoshift"},
      { 8,  5,   50, "Chronoshift"},
      { 10,  8,   65, "Living Bond Destoyer"},
      { 13,  12,  80, "Non-Living Bond Destoyer"},
      { 15,  20,  75, "Conservation of Vital Force"},
      { 20,  25,  70, "Code of Recall"},
      { 22,  30,  90, "Darwin's Logic"},
      { 30,  35,  95, "Mind of the Machine"},
      { 40,  80,  99, "Rewrite Reality"},
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
      { 99,  0,   0,  "Word of Recall"},
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
  /* ack! No idea!*/
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
  /* Ninjitsu powers I stole this ninja from heng */
  /* Sigh I need to fix these too. */
  {
    {
      /* Level gained,  cost,  %fail,  name */
      {  1,  1,  20, "Create Darkness"},
      {  2,  2,  25, "Detect Near"},
      {  3,  3,  25, "Hide in Leafs"},
      {  5,  3,  30, "Kawarimi"},
      {  7,  8,  35, "Absconding"},
      {  8, 10,  35, "Hit and Away"},
      { 10, 10,  40, "Bind Monster"},
      { 12, 12,  70, "Ancient Knowledge"},
      { 15, 10,  50, "Floating"},
      { 17, 12,  45, "Hide in Flame"},
      { 18, 20,  40, "Nyusin"},
      { 20,  5,  50, "Syuriken Spreading"},
      { 22, 15,  55, "Chain Hook"},
      { 25, 32,  60, "Smoke Ball"},
      { 28, 32,  60, "Swap Position"},
      { 30, 30,  70, "Glyph of Explosion"},
      { 32, 40,  40, "Hide in Mud"},
      { 34, 35,  50, "Hide in Mist"},
      { 38, 40,  60, "Rengoku-Kaen"},
      { 41, 50,  55, "Bunshin"},
      { 99,  0,   0, ""},
      
    }
  },
};


void mindcraft_info(char *p, int use_mind, int power)
{
	cptr s_dam = "dam ";
	cptr s_dur = "dur ";
	cptr s_range = "range ";

	int plev = p_ptr->lev;

	strcpy(p, "");

	switch (use_mind)
    {
		case MIND_OFFICER:		
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
    	case MIND_AESTHETE:				
    	switch (power)
	  		{
		  		case 0:  break;
				case 1:  break;
				case 2:  break;
				case 3:  break;
				case 4:   break;
				case 5:  sprintf(p, " range %d", plev * 2); break;
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
    	case MIND_EXPLORER:	
    	switch (power)
	    	{
		    	case 0:  break;
				case 1:  break;
				case 2:  break;
				case 3:  sprintf(p, " range %d", plev); break;
				case 4:  break;
				case 5:  break;
				case 6:  break;
				case 7:  break;
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
    			case 0:  sprintf(p, " dam %dd%d", 3 + ((plev - 1) / 4), 3 + plev / 15);  break;
				case 1:  break;
				case 2:  sprintf(p, " range %d", (plev)); break;
				case 3:  sprintf(p, " range %d", plev * 5); break;
				case 4:  break;
				case 5:  sprintf(p, " dam %dd8", 8 + ((plev - 5) / 4));break;
				case 6:  sprintf(p, " dur %d", plev);break;
				case 7:  break;
				case 8:  sprintf(p, " dam %dd4", plev); break;
				case 9:  sprintf(p, " dur 11-%d", ((plev * 3)/ 2) + 10); break;
				case 10: sprintf(p, " dam %dd3 Stun", plev * 2); break;
				case 11: sprintf(p, " dam %d", plev * (plev > 39 ? 4 : 3)); break;
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
		case MIND_RECKONER: 	
		switch (power)
	    	{
    			case 0:  sprintf(p, " range %d", (plev < 30) ? plev : (plev * 3)); break;
				case 1:  break;
				case 2:  break;
				case 3:  sprintf(p, " dam %dd%d", (plev / 2), (3 + plev / 3)); break;
				case 4:  break;
				case 5:  sprintf(p, " heal %dd%d", (plev / 3), plev); break;
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
		case MIND_TOURIST: 	
		switch (power)
	    	{
    			case 0:  sprintf(p, " range %d", ((plev / 10) + 1)); break;
				case 1:  sprintf(p, " range %d", ((plev / 5) + 2)); break;
				case 2:  sprintf(p, " range %d", (plev)); break;
				case 3:  sprintf(p, " range %d", ((plev * 3))); break;
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

  static int get_class_power(int *sn)
    {
      int 	i;
      int 	num = 0;
      int   y = 1;
      int   x = 10;
      int   minfail =0;
      int   plev = p_ptr->lev;
      int   chance;
      int   ask;
      char  choice;
      char  out_val[160];
      char  comment[80];
      cptr  p;

      mind_type  spell;
      mind_power *mind_ptr;
      bool       flag, redraw;
      int        use_mind;
      
    /* Insert if /if else statement here for classflag. */
    
    if ((cp_ptr->flags) & CF_OFFICER)
    {
    	use_mind = MIND_OFFICER;
	    p = "commanding presence";
    }
    else if ((cp_ptr->flags) & CF_AESTHETE)
	{
	    use_mind = MIND_AESTHETE;
	    p = "skill";
    }
    else if ((cp_ptr->flags) & CF_EXPLORER)
	{
	    use_mind = MIND_EXPLORER;
	    p = "talent";
    }
    else if ((cp_ptr->flags) & CF_MEDIUM)
	{
	    use_mind = MIND_MEDIUM;
	    p = "mental power";
    }
    else if ((cp_ptr->flags) & CF_RECKONER)
	{
	    use_mind = MIND_RECKONER;
	    p = "reality alteration";
    }
    else if ((cp_ptr->flags) & CF_TOURIST)
	{
	    use_mind = MIND_TOURIST;
	    p = "tourism dept.";
    }
    else if ((cp_ptr->flags) & CF_HUSSAR)
	{
	    use_mind = MIND_HUSSAR;
	    p = "combat techniques";
    }
    else if ((cp_ptr->flags) & CF_NATURE)
	{
	    use_mind = MIND_NATURE;
	    p = "natural empathy";
    }
    else if ((cp_ptr->flags) & CF_NINJA)
	{
	    use_mind = MIND_NINJA;
	    p = "ninjutsu";
    }
    else
    {
    	msg_print("You have no powers.");
    	
    }
    
    
      mind_ptr = &mind_powers[use_mind];

	/* Assume cancelled */
      *sn = (-1);

#ifdef ALLOW_REPEAT /* TNB */

      /* Get the spell, if available */
      if (repeat_pull(sn))
	{
	  /* Verify the spell */
	  if (mind_ptr->info[*sn].min_lev <= plev)
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
	  if (mind_ptr->info[i].min_lev <= plev)
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

				/* Display a list of spells */
				/* Commeneted out the z code, reinserting the h code.
				 * prt("", x, y);
				 * put_str("Name", (x + 5), y);
				 * put_str("Lv Mana Fail Info", (x + 35), y);
				 */
				
				prt("", y, x);
				put_str("Name", y, x + 5);

				put_str(format("Lv  MP  Fail Info"), y, x + 35);
				

				
				
				/* Dump the spells */
				for (i = 0; i < MAX_CLASS_POWERS; i++)
				{
					/* Access the spell */
					spell = mind_ptr->info[i];
					if (spell.min_lev > plev)   break;

					chance = spell.fail;

					/* Reduce failure rate by "effective" level adjustment */
					chance -= 3 * (plev - spell.min_lev);

					/* Reduce failure rate by INT/WIS adjustment */
					chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[cp_ptr->spell_stat]] - 1);

					/* Not enough mana to cast */
					if (spell.mana_cost > p_ptr->csp)
					{
						chance += 5 * (spell.mana_cost - p_ptr->csp);
					}

					/* Extract the minimum failure rate */
					minfail = adj_mag_fail[p_ptr->stat_ind[cp_ptr->spell_stat]];

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
					
					/* Dump the spell --(-- */
					/* Commenting out the z code and putting in the heng code
					 * sprintf(psi_desc, "  %c) %-30s%2d %4d %3d%%%s",
					 *	   I2A(i), spell.name, 
					 *	   spell.min_lev, mana_cost, chance, comment);
					 * prt(psi_desc, x, y + i + 1);
					 */
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
	int             b = 0;
	int             plev = p_ptr->lev;

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
 * is 'Aesthete'.
 */
static bool cast_aesthete_spell(int spell)
{
	/* this will vary based on the spells, and what they depend on */
	int b = 0;
	int plev = p_ptr->lev;
	int py = p_ptr->py;
	int px = p_ptr->px;
	int radius;
	
	/* spell code */
	switch (spell)
	{
    	   		case 0:  
    			/* Arrogant Laugh (almost bad) */
    			if ((plev * 2) > (randint(130) + 4))
    			{
    				/* for staying in bounds of max 16 range */
    				radius = (plev * 3);
    				/* paranoia */
    				if (radius > 16)
    				{
    					radius = 16;
    				}
	    			msg_print("Creatures Cower in fear before you!");
					project(-1, (plev * 3), py, px, 
											radius, GF_TURN_ALL, PROJECT_KILL);

				}	
    			else
    			{
    			msg_print("Your Arrogant Laugh angers the monsters around you!");
    			aggravate_monsters(-1);
       			}	
    			break;
    			/* Treasure Sense */
				case 1: if (plev > 45)
						{
						detect_traps();
						}
						if (plev > 35)
						{
						detect_objects_magic();
						}
						if (plev > 30)
						{
						detect_objects_normal();
						}
						if (plev > 20)
						{
						detect_objects_gold();
						}
						detect_treasure();
						break;
				/* Enhance weapon */
				case 2: if (plev > 45) 
						{
						brand_weapon();
						}
						else if (plev > 35)
						{
						(void)enchant_spell(rand_int(15) + 1, rand_int(15) + 1, 0);
						}
						else if (plev > 25)
						{
						(void)enchant_spell(rand_int(10) + 1, rand_int(10) + 1, 0);
						}
						else if (plev > 15)
						{
						(void)enchant_spell(rand_int(5) + 1, rand_int(5) + 1, 0);
						}
						else
						{
						(void)enchant_spell(rand_int(3) + 1, rand_int(3) + 1, 0);
						}
						break;
				/* Enhance Armor */
				case 3: 
						if (plev > 45) 
						{
						(void)enchant_spell(0, 0, rand_int(20) + 1);
						}
						else if (plev > 35)
						{
						(void)enchant_spell(0, 0, rand_int(15) + 1);
						}
						else if (plev > 25)
						{
						(void)enchant_spell(0, 0, rand_int(10) + 1);
						}
						else if (plev > 15)
						{
						(void)enchant_spell(0, 0, rand_int(5) + 1);
						}
						else
						{
						(void)enchant_spell(0, 0, rand_int(3) + 1);
						}
						break;
				/* Legendary lore */
				case 4: if (plev > 40)
						{
						identify_fully();
						} 
						else 
						{
						ident_spell();
						}
						 break;
				case 5:  teleport_player(plev * 2);  break;
				case 6:  recharge(plev * 3); break;
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
	int plev = p_ptr->lev;
	int	px = p_ptr->px;
	int py = p_ptr->py;
	
	/* spell code */
	switch (spell)
	{
    			case 0:  lite_room (py, px);
    					 break;
				case 1:  if (plev > 44)
							wiz_lite();
						else if (plev > 19)
							map_area();
			
						if (plev < 30)
						{
							b = detect_monsters_normal();
							if (plev > 14)
								b |= detect_monsters_invis();
			
							if (plev > 4)
							{
								b |= detect_traps();
								b |= detect_doors();
							}
						}
						else
						{
							b = detect_all();
						}
			
						if (plev > 24)
						{
							(void)set_tim_esp(p_ptr->tim_esp + plev);
						}
			
						if (!b) msg_print("You feel safe.");
						 break;
				case 2:  teleport_player(plev);
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
						 int time = randint(plev) + plev;
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

			if (randint(100) < plev * 2)
				(void)fire_beam(GF_PSI, dir,
								damroll(3 + ((plev - 1) / 4), (3 + plev / 15)));
			else
				(void)fire_ball(GF_PSI, dir,
								damroll(3 + ((plev - 1) / 4), (3 + plev / 15)),
								0);
			break;
		case 1:
			if (plev > 44)
				wiz_lite();
			else if (plev > 19)
				map_area();

			if (plev < 30)
			{
				b = detect_monsters_normal();
				if (plev > 14)
					b |= detect_monsters_invis();

				if (plev > 4)
				{
					b |= detect_traps();
					b |= detect_doors();
				}
			}
			else
			{
				b = detect_all();
			}

			if (plev > 24)
			{
				(void)set_tim_esp(p_ptr->tim_esp + plev);
			}

			if (!b) msg_print("You feel safe.");
			break;
		case 2:
			/* Minor displace */
			teleport_player(plev);
			break;
		case 3:
			/* Major displace */
			teleport_player(plev * 5);
			break;
		case 4:
			/* Psychic Disturbance */
			msg_print("You disturb specters from beyond the veil!");
			(void)project(-1, 2 + plev / 8, p_ptr->py, p_ptr->px,
						  damroll((plev / 2), plev), GF_CONFUSION, PROJECT_KILL);
			break;
		case 5:
			/* spirit blast  ---  not 'true' TK */
			if (!get_aim_dir(&dir)) return FALSE;
			(void)fire_ball(GF_SOUND, dir, damroll(8 + ((plev - 5) / 4), 8),
							(plev > 20 ? (plev - 20) / 8 + 1 : 0));
			break;
		case 6:
			/* Character Armour */
			(void)set_shield(p_ptr->shield + plev);
			if (plev > 14) (void)set_oppose_acid(p_ptr->oppose_acid + plev);
			if (plev > 19) (void)set_oppose_fire(p_ptr->oppose_fire + plev);
			if (plev > 24) (void)set_oppose_cold(p_ptr->oppose_cold + plev);
			if (plev > 29) (void)set_oppose_elec(p_ptr->oppose_elec + plev);
			if (plev > 34) (void)set_oppose_pois(p_ptr->oppose_pois + plev);
			break;
		case 7:
			return ident_spell();
		case 8:
			/* Soul Purge */
			msg_print("The anguish of the dead emanates from your brain!");
			
			(void)project(-1, 2 + plev / 10, p_ptr->py, p_ptr->px,
						  damroll(plev, 4), GF_PSI, PROJECT_KILL);
			break;
		case 9:
			/* Adrenaline */
			(void)set_afraid(0);
			(void)set_stun(0);

			/*
			 * Only heal when Adrenalin Channeling is not active. We check
			 * that by checking if the player isn't fast and 'heroed' atm.
			 */
			if (!p_ptr->fast || !(p_ptr->hero || p_ptr->shero))
			{
				(void)hp_player(plev);
			}

			b = 10 + randint((plev * 3) / 2);
			if (plev < 35)
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
		case 10:
			/* Psychic Drain Turned into MEGA-STUN*/
			if (!get_aim_dir(&dir)) return FALSE;

			b = damroll(plev * 2, 3);

			/* This is always a radius-0 ball now */
			if (fire_ball(GF_STUN, dir, b, 0))
			p_ptr->energy -= randint(150);
			break;
		case 11:
			/* Entropic Blast */
			msg_print
				("A wave of pure entropic force blasts out from your spirit!");
			(void)project(-1, 3 + plev / 10, p_ptr->py, p_ptr->px,
						  plev * (plev > 39 ? 4 : 3), GF_FORCE,
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
	int     plev = p_ptr->lev;
	int 	b;
	int		py = p_ptr->py;
	int		px = p_ptr->px;
	int 	dir;
	
	/* spell code */
	switch (spell)
	{
    			case 0:	
    					if (plev < 30) 
    					{
    					teleport_player(plev);
    					}  
    					else
    					{
    					teleport_player(plev * 3);
    					}
    					break;
    					
				case 1: 
						b = (randint(50) + plev);
						if (b < 10)
						{
						unlite_area(randint(plev *2), (randint(plev / 5) + 2));
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
						lite_area(randint(plev * 2), (randint(plev / 5) + 3));
						}
						
						break;
				
				case 2: b = (randint(50) + plev);
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
								(void)set_fast(randint(plev) + plev);
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
				case 3:  if (!get_aim_dir(&dir)) return FALSE; 
						 (void)fire_bolt(GF_NETHER, dir,
								damroll((plev / 2), (3 + plev / 3)));
						 break;
				
				case 4:  if (!get_aim_dir(&dir)) return FALSE;
						 (void)wall_to_mud(dir);
						 break;
						 
				case 5:  (void)hp_player(damroll((plev / 3), plev));
						 (void)set_cut(0);
						 break;
						 
				case 6:  set_recall(); break;
				
				case 7:  if (!get_aim_dir(&dir)) return FALSE;
						 (void)poly_monster(dir);
						 break;
				case 8:  if (plev < 40)
						 {
						 map_area();
						 (void)set_tim_esp(p_ptr->tim_esp + 10);
						 }
						 else
						 {
						 wiz_lite();
						 (void)set_tim_esp(p_ptr->tim_esp + plev);
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
    			case 0: teleport_player((plev / 10) + 1); break;
				case 1: teleport_player((plev / 5) + 2); break;
				case 2: teleport_player(plev); break;
				case 3: teleport_player(plev * 3); break;
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
	int             b = 0;
	int             plev = p_ptr->lev;

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
	int             b = 0;
	int             plev = p_ptr->lev;

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
	int				b = 0;
	int             chance;
	int             minfail = 0;
	int             plev = p_ptr->lev;
	int             old_csp = p_ptr->csp;
	mind_type       spell;
	bool            cast;
	int             use_mind;
	
	/* not if confused */
	if (p_ptr->confused)
	{
		msg_print("You are too confused!");
		return;
	}

	/* get power */
	if (!get_class_power(&n)) return;

	
	if ((cp_ptr->flags) & CF_OFFICER)
    	{use_mind = MIND_OFFICER;}
    else if ((cp_ptr->flags) & CF_AESTHETE)
		{use_mind = MIND_AESTHETE;}
    else if ((cp_ptr->flags) & CF_EXPLORER)
		{use_mind = MIND_EXPLORER;}
    else if ((cp_ptr->flags) & CF_MEDIUM)
		{use_mind = MIND_MEDIUM;}
    else if ((cp_ptr->flags) & CF_RECKONER)
		{use_mind = MIND_RECKONER;}
    else if ((cp_ptr->flags) & CF_TOURIST)
		{use_mind = MIND_TOURIST;}
    else if ((cp_ptr->flags) & CF_HUSSAR)
		{use_mind = MIND_HUSSAR;}
    else if ((cp_ptr->flags) & CF_NATURE)
		{use_mind = MIND_NATURE;}
    else if ((cp_ptr->flags) & CF_NINJA)
		{use_mind = MIND_NINJA;}
	else {
			msg_print("You have no powers!");
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
	chance -= 3 * (plev - spell.min_lev);
	
	/* Reduce failure rate by INT/WIS adjustment */
	chance -= 3 * (adj_mag_stat[p_ptr->stat_ind[cp_ptr->spell_stat]] - 1);

	/* Not enough mana to cast */
	if (spell.mana_cost > p_ptr->csp)
	{
		chance += 5 * (spell.mana_cost - p_ptr->csp);
	}

	/* Extract the minimum failure rate */
	minfail = adj_mag_fail[p_ptr->stat_ind[cp_ptr->spell_stat]];

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

		/* Backfire information would go here */

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
			(void)dec_stat(A_WIS, 15 + randint(10), perm);
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
