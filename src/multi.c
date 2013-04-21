/* File: multi.c */

/*
 * Copyright (c) 2001 Emily Fu
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

#include "angband.h"

/* 0 = allowed, 1 = disallowed (except to cheaters), 2 = disallowed to all */
bool not_allowed[MAX_CLASS][MAX_CLASS] =
{
     /* War Mag Pri Rog Ran Pal Ill Arc Dea Ber Mon Tri */
     {   2,  0,  0,  0,  1,  1,  0,  1,  0,  1,  1,  0 }, /* Warrior      6 */
     {   0,  2,  0,  2,  2,  1,  2,  0,  0,  2,  0,  2 }, /* Mage         5 */
     {   0,  0,  2,  1,  1,  2,  0,  0,  2,  0,  0,  0 }, /* Priest       5 */
     {   0,  2,  1,  2,  2,  1,  2,  0,  0,  2,  0,  2 }, /* Rogue        4 */
     {   1,  2,  1,  2,  2,  1,  2,  1,  1,  2,  1,  2 }, /* Ranger       0 */
     {   1,  1,  2,  1,  1,  2,  1,  1,  1,  1,  1,  1 }, /* Paladin      0 */
     {   0,  2,  0,  2,  2,  1,  2,  0,  0,  2,  0,  2 }, /* Illusionist  4 */
     {   1,  0,  0,  0,  1,  1,  0,  2,  0,  1,  0,  0 }, /* Archer       7 */
     {   0,  0,  2,  0,  1,  2,  0,  0,  2,  0,  0,  0 }, /* Death Priest 8 */
     {   1,  2,  0,  2,  2,  1,  2,  1,  0,  2,  1,  2 }, /* Berserker    2 */
     {   1,  0,  0,  0,  1,  1,  0,  0,  0,  1,  2,  0 }, /* Monk         7 */
     {   0,  2,  0,  2,  2,  1,  2,  0,  0,  2,  0,  2 }, /* Trickster    4 */
};

/* Create a new class */
void do_cmd_create_multi_class(void)
{
     int k, n, allowed = 0, list[MAX_CLASS];
     cptr str;
     char ch;
     char buf[80];
     bool not_enough_exp, disallowed, error;

     /* Clear screen */
     Term_clear();

     /* Show current classes */
     for (n = 0; n < p_ptr->available_classes; n++)
     {
	  int i;
	  player_class *temp;
      
	  /* Analyze */
	  i = p_ptr->pclass[n];

	  temp = &class_info[i];
	  str = temp->title;

	  /* Show classes */
	  Term_putstr(2, 10, -1, TERM_WHITE, "Current Classes"); 
	  sprintf(buf, "%c) %s", I2A(n), str);
	  put_str(buf, 11 + (n/3), 2 + 20 * (n%3));
     }

     /* If playing a non-recommended class, prevent multiclassing */
     if (!(rp_ptr->choice & (1L << p_ptr->pclass[0])))
     {
	  prt("As you are already playing a non-recommended combination", 
	      1, 0);
	  prt("of races and class, multiclassing is not allowed.", 2, 0);
	  inkey();
	  do_cmd_redraw();
	  return;             
     }

     /* Racial limitation on how many classes can be taken */
     if (p_ptr->available_classes == rp_ptr->max_multi)
     {
	  prt(format("%s can only take %d classes!", p_name + rp_ptr->name, 
		     rp_ptr->max_multi), 1, 0);
	  inkey();
	  do_cmd_redraw();
	  return;       
     }

     /* Rogues and Tricksters can only multiclass once */
     if (player_has_class(CLASS_ROGUE, 0) &&
	 p_ptr->available_classes > 1)
     {
	  prt("Rogues can only take one other class!", 1, 0); 
	  inkey();
	  do_cmd_redraw();
	  return;       	  
     }
     if (player_has_class(CLASS_TRICKSTER, 0) &&
	 p_ptr->available_classes > 1)
     {
	  prt("Tricksters can only take one other class!", 1, 0); 
	  inkey();
	  do_cmd_redraw();
	  return;       	  
     }


     not_enough_exp = FALSE; /* Default is OK */

     /* Player must be at least level 25 in all existing classes to gain a new 
	one */
     for (n = 0; n < p_ptr->available_classes; n++)
     {
	  if (p_ptr->lev[n] < 25) not_enough_exp = TRUE;
     }

     if (not_enough_exp)
     {    
	  prt("You are not experienced enough in your existing classes!", 
	      1, 0);
	  inkey();
	  do_cmd_redraw();
	  return; 
     }

     /* Dump classes */
     for (n = 0; n < MAX_CLASS; n++)
     {
	  /* Analyze */
	  cp_ptr[n] = &class_info[n];
	  str = cp_ptr[n]->title;

	  /* Show allowed classes */
	  if (rp_ptr->choice & (1L << n))
	  {
	       int i;

	       disallowed = FALSE; /* Default is OK */
	
	       /* Check for dis-allowed combinations */
	       for (i = 0; i < p_ptr->available_classes; i++)
	       {
		    /* Check table of disallowed combinations */
		    if (not_allowed[p_ptr->pclass[i]][n]) disallowed = TRUE; 
	       }
	
	       /* Can only choose rogue/trickster if only one other class is 
		  chosen */
	       if ((p_ptr->available_classes > 1) && 
		   (n == CLASS_ROGUE || n == CLASS_TRICKSTER))
		    disallowed = TRUE;

	       if (!disallowed)
	       {
		    /* Display */
		    sprintf(buf, "%c) %s", I2A(allowed), str);
		    put_str(buf, 3 + (allowed/3), 2 + 20 * (allowed%3));
		    list[allowed] = n;
		    allowed++;
	       }

	  }
      
     }

     /* No classes can be taken */
     if (allowed == 0)
     {    
	  prt("There are no more classes available to you!", 1, 0);
	  inkey();
	  do_cmd_redraw();
	  return; 
     }

     error = FALSE;

     /* Get a class */
     while (1)
     {
	  sprintf(buf, "Choose your next class (%c-%c): ",
		  I2A(0), I2A(allowed-1));
	  put_str(buf, 1, 2);
	  ch = inkey();
	  if (ch == 'Q') { error = TRUE; break; }
	  if (ch == 'S') { error = TRUE; break; }
	  k = (islower(ch) ? A2I(ch) : -1); /* Turn into integer */
	  if (ch == ESCAPE) { error = TRUE; break; }
	  if ((k >= 0) && (k < allowed)) break; /* Successful */
	  if (ch == '?') do_cmd_help();
	  else bell("Illegal class!");
     }

     /* A valid class was not chosen */
     if (error) 
     {
	  prt("No class chosen!", 1, 0);
	  inkey();
	  do_cmd_redraw();
	  return;
     }

     /* Add class to end of list */

     p_ptr->pclass[p_ptr->available_classes] = list[k];

     /* Level one */
     p_ptr->max_lev[p_ptr->available_classes] = 
	  p_ptr->lev[p_ptr->available_classes] = 1;

     /* Start with no experience */
     p_ptr->max_exp[p_ptr->available_classes] = 
	  p_ptr->exp[p_ptr->available_classes] = 0;

     /* Experience factor */
     {
       int temp = (rp_ptr->r_exp + cp_ptr[p_ptr->available_classes]->c_exp);
       p_ptr->expfact[p_ptr->available_classes] = temp;
     }

     /* Nearly last,  class to new class */
     p_ptr->current_class = p_ptr->available_classes;

     p_ptr->available_classes++; /* Lastly, one more class is now available */

     /* Success ... */
     prt("You have chosen another class!", 1, 0);
     inkey();

     msg_print(format("Switching to level %d %s", 
		      p_ptr->lev[p_ptr->current_class], 
		      cp_ptr[p_ptr->pclass[p_ptr->current_class]]->title));  
     message_flush();

     /* Refresh */
     do_cmd_redraw();
}

/* Wizard - create a new class */
void do_cmd_wiz_create_multi_class(void)
{
     int k, n, allowed = 0, list[MAX_CLASS];
     cptr str;
     char ch;
     char buf[80];
     bool disallowed, error;

     /* Clear screen */
     Term_clear();

     /* Show current classes */
     for (n = 0; n < p_ptr->available_classes; n++)
     {
	  int i;
	  player_class *temp;
      
	  /* Analyze */
	  i = p_ptr->pclass[n];

	  temp = &class_info[i];
	  str = temp->title;

	  /* Show classes */
	  Term_putstr(2, 10, -1, TERM_WHITE, "Current Classes"); 
	  sprintf(buf, "%c) %s", I2A(n), str);
	  put_str(buf, 11 + (n/3), 2 + 20 * (n%3));
     }

     /* Dump classes */
     for (n = 0; n < MAX_CLASS; n++)
     {
	  /* Analyze */
	  cp_ptr[n] = &class_info[n];
	  str = cp_ptr[n]->title;

	  /* Show allowed classes */
	  if (rp_ptr->choice & (1L << n))
	  {
	       int i;

	       disallowed = FALSE; /* Default is OK */
	
	       /* Check for dis-allowed combinations */
	       for (i = 0; i < p_ptr->available_classes; i++)
	       {
		    /* Check table of disallowed combinations, more are OK */
		    if (not_allowed[p_ptr->pclass[i]][n] == 2) 
			 disallowed = TRUE; 
	       }
	
	       if (!disallowed)
	       {
		    /* Display */
		    sprintf(buf, "%c) %s", I2A(allowed), str);
		    put_str(buf, 3 + (allowed/3), 2 + 20 * (allowed%3));
		    list[allowed] = n;
		    allowed++;
	       }

	  }
      
     }

     /* No classes can be taken */
     if (allowed == 0)
     {    
	  prt("There are no more classes available to you!", 1, 0);
	  inkey();
	  do_cmd_redraw();
	  return; 
     }

     error = FALSE;

     /* Get a class */
     while (1)
     {
	  sprintf(buf, "Choose your next class (%c-%c): ",
		  I2A(0), I2A(allowed-1));
	  put_str(buf, 1, 2);
	  ch = inkey();
	  if (ch == 'Q') { error = TRUE; break; }
	  if (ch == 'S') { error = TRUE; break; }
	  k = (islower(ch) ? A2I(ch) : -1); /* Turn into integer */
	  if (ch == ESCAPE) { error = TRUE; break; }
	  if ((k >= 0) && (k < allowed)) break; /* Successful */
	  if (ch == '?') do_cmd_help();
	  else bell("Illegal class!");
     }

     /* A valid class was not chosen */
     if (error) 
     {
	  prt("No class chosen!", 1, 0);
	  inkey();
	  do_cmd_redraw();
	  return;
     }

     /* Add class to end of list */

     p_ptr->pclass[p_ptr->available_classes] = list[k];

     /* Level one */
     p_ptr->max_lev[p_ptr->available_classes] = 
	  p_ptr->lev[p_ptr->available_classes] = 1;

     /* Start with no experience */
     p_ptr->max_exp[p_ptr->available_classes] = 
	  p_ptr->exp[p_ptr->available_classes] = 0;

     /* Experience factor */
     {
       int temp = (rp_ptr->r_exp + cp_ptr[p_ptr->available_classes]->c_exp);
       p_ptr->expfact[p_ptr->available_classes] = temp;
     }

     /* Set class to new class */
     p_ptr->current_class = p_ptr->available_classes;

     p_ptr->available_classes++; /* One more class is now available */

     /* Success ... */
     prt("You have chosen another class!", 1, 0);
     inkey();

     msg_print(format("Switching to level %d %s", 
		      p_ptr->lev[p_ptr->current_class], 
		      cp_ptr[p_ptr->pclass[p_ptr->current_class]]->title));  
     message_flush();

     /* Refresh */
     do_cmd_redraw();
}

/* Switch class (wrap around if neccesary) */
void do_cmd_switch_multi_class(int direction)
{
     if (p_ptr->available_classes == 1)
     {
	  msg_print("This is the only class you are a member of!");
	  return;
     }
 
     if (direction == 0) /* Forwards */
     {
	  /* End of list */
	  if (p_ptr->current_class + 1 == p_ptr->available_classes) 
               /* Switch to first class */
	       p_ptr->current_class = 0; 
	  /* Else move forwards one place */
	  else p_ptr->current_class ++; 
     }
     else /* Backwards */
     {
          /* Beginning of list */
	  if (p_ptr->current_class == 0) 
               /* Switch to last class */
	       p_ptr->current_class = p_ptr->available_classes - 1; 
	  /* Else move backwards one place */
	  else p_ptr->current_class --; 
     }

     msg_print(format("Switching to level %d %s", 
		      p_ptr->lev[p_ptr->current_class], 
		      cp_ptr[p_ptr->pclass[p_ptr->current_class]]->title));  
     message_flush();

     /* Refresh */
     do_cmd_redraw();
}

bool player_has_class(int class, int level)
{
     int i;
     bool check = FALSE; /* Assume not */

     /* Do not check level */
     if (level == 0) 
     {
	  /* Check each player class */
	  for (i = 0; i < p_ptr->available_classes; i++)
	  {
	       if (p_ptr->pclass[i] == class) check = TRUE; /* Found */
	  }
     }
     else
     {
	  /* Check each player class */
	  for (i = 0; i < p_ptr->available_classes; i++)
	  {
	       if (p_ptr->pclass[i] == class && p_ptr->lev[i] >= level) check = TRUE;
	  }
     }

     /* Return value */
     return check;
}

/* Find level of a specified class */
int level_of_class(int class)
{
     int i; 

     /* Player does not have this class */
     if (!player_has_class(class, 0)) return 0;

     /* Check each class */
     for (i = 0; i < p_ptr->available_classes; i++)
     {
	  /* Return level of class if found */
	  if (p_ptr->pclass[i] == class) return p_ptr->lev[i];
     }

     return 0;
}

/* Which class does the player have the highest level in ? */
int best_class()
{
     int i;
     int best_level = 1; /* Assume first level */
     int temp = 0; /* Assume first class */

     /* Get best level */
     for (i = 0; i < p_ptr->available_classes; i++)
     {
	  /* Any higher levels ? */
	  if (p_ptr->lev[i] > best_level)
	  {
	       /* Store higher level */
	       best_level = p_ptr->lev[i];

	       /* Store this class */
	       temp = i;
	  }
     }

     /* Return best class */
     return temp;
}

/* Get the index of a sought class, ie CLASS_WARRIOR */
int index_of_class(int class_sought)
{
  int class;

  /* Look in each available class */
  for (class = 0; class < p_ptr->available_classes; class++)
  {
    /* Found */
    if (p_ptr->pclass[class] == class_sought)
      return class;
  }

  /* None found */
  return -1;
}
