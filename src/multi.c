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

/* Get the index of character's spellcasting class */
int magery_class()
{
  if (player_has_class(CLASS_MAGE, 0)) return CLASS_MAGE;
  if (player_has_class(CLASS_ROGUE, 0)) return CLASS_ROGUE;
  if (player_has_class(CLASS_RANGER, 0)) return CLASS_RANGER;
  if (player_has_class(CLASS_ILLUSIONIST, 0)) return CLASS_ILLUSIONIST;
  if (player_has_class(CLASS_TRICKSTER, 0)) return CLASS_TRICKSTER;

  return -1;
}

/* Get index of player's praying class */
int priest_class()
{
  if (player_has_class(CLASS_PRIEST, 0)) return CLASS_PRIEST;
  if (player_has_class(CLASS_PALADIN, 0)) return CLASS_PALADIN;
  if (player_has_class(CLASS_DEATH_PRIEST, 0)) return CLASS_DEATH_PRIEST;

  return -1;
}
