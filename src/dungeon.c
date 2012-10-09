/* File: dungeon.c */

/* Purpose: Angband game engine */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Regenerate hit points                                -RAK-
 */
static void regenhp(s16b percent)
{
   s32b        new_chp, new_chp_frac;
   s16b                   old_chp;

   /* Save the old hitpoints */
   old_chp = p_ptr->chp;

   /* Extract the new hitpoints */
   new_chp = ((long)p_ptr->mhp) * percent + PY_REGEN_HPBASE;

   if (p_ptr->movement > 4) new_chp /= (p_ptr->movement - 3); 

   p_ptr->chp += new_chp >> 16;   /* div 65536 */

   /* check for overflow */
   if ((p_ptr->chp < 0) && (old_chp > 0)) p_ptr->chp = MAX_SHORT;
   new_chp_frac = (new_chp & 0xFFFF) + p_ptr->chp_frac;        /* mod 65536 */
   if (new_chp_frac >= 0x10000L)
   {
      p_ptr->chp_frac = new_chp_frac - 0x10000L;
      p_ptr->chp++;
   }
   else
   {
      p_ptr->chp_frac = new_chp_frac;
   }

   /* Fully healed */
   if (p_ptr->chp >= p_ptr->mhp)
   {
      p_ptr->chp = p_ptr->mhp;
      p_ptr->chp_frac = 0;
   }

   /* Notice changes */
   if (old_chp != p_ptr->chp) p_ptr->redraw1 |= (PR1_HP);
}

/*
 * Regenerate mana points                               -RAK-
 */
static void regenmana(s16b percent)
{
   s32b     new_mana, new_mana_frac;
   s16b     old_csp;

   old_csp = p_ptr->csp;
   new_mana = ((long)p_ptr->msp) * percent + PY_REGEN_MNBASE;
  
   if (p_ptr->movement > 4) new_mana /= (p_ptr->movement - 3); 

   p_ptr->csp += new_mana >> 16;    /* div 65536 */
   /* check for overflow */
   if ((p_ptr->csp < 0) && (old_csp > 0))
   {
      p_ptr->csp = MAX_SHORT;
   }
   new_mana_frac = (new_mana & 0xFFFF) + p_ptr->csp_frac;      /* mod 65536 */
   if (new_mana_frac >= 0x10000L)
   {
      p_ptr->csp_frac = new_mana_frac - 0x10000L;
      p_ptr->csp++;
   }
   else
   {
      p_ptr->csp_frac = new_mana_frac;
   }

   /* Must set frac to zero even if equal */
   if (p_ptr->csp >= p_ptr->msp)
   {
      p_ptr->csp = p_ptr->msp;
      p_ptr->csp_frac = 0;
   }

   /* Redraw mana */
   if (old_csp != p_ptr->csp) p_ptr->redraw1 |= (PR1_MANA);
}

/*
 * Regenerate the monsters (once per 100 game turns)
 *
 * XXX XXX XXX Should probably be done during monster turns.
 */
static void regen_monsters(void)
{
   s16b i, frac;

   /* Regenerate everyone */
   for (i = 1; i < mn_max; i++)
   {
      /* Check the i'th monster */
      monster_type *m_ptr = &mn_list[i];
      monster_race *r_ptr = &r_info[m_ptr->r_idx];

      /* Skip dead monsters */
      if (!m_ptr->r_idx) continue;

      if (m_ptr->attacked < MAX_ATTACK_HISTORY)
         m_ptr->attacked++;
      /* Allow regeneration (if needed) */
      if (m_ptr->hp < m_ptr->maxhp)
      {
         /* Hack -- Base regeneration */
         frac = m_ptr->maxhp / 100;

         /* Hack -- Minimal regeneration rate */
         if (!frac) frac = 1;

         /* Hack -- Some monsters regenerate quickly */
         if (r_ptr->flags2 & RF2_REGENERATE) frac *= 2;

         /* Hack -- Regenerate */
         m_ptr->hp += frac;

         /* Do not over-regenerate */
         if (m_ptr->hp > m_ptr->maxhp) m_ptr->hp = m_ptr->maxhp;

         /* Redraw (later) if needed */
         if (health_who == i) p_ptr->redraw1 |= (PR1_HEALTH);
      }
   }
}

/*
 * this routine updates one store at a time, but all in turn
 */
void do_check_update_stores(void)
{
   static s16b last_store = 0;

   /* Update the stores once a day (while in dungeon) */
   if ( !(turn % (STORE_TURNS/num_stores)) && ( num_stores>0) )
   {
      store_type *st_ptr;

      /* Message */
      if (cheat_xtra) msg_format("Updating Stores... (%d/%d)", last_store, num_stores);
      
      /* num_stores could have changed */
      if (last_store >= num_stores) last_store=0;

      /* Maintain */
      st_ptr = &store[last_store];
      if (st_ptr->store_type != DUNG_ENTR_HOME) store_maint(last_store);

      /* New owners */
      if (shuffle_owners && (rand_int(STORE_SHUFFLE) == 0))
      {
         if (cheat_xtra) msg_print("Shuffling a Store...");
         store_shuffle();
      }
      last_store++;
      if (last_store >= num_stores) last_store=0;

      /* Message */
      if (cheat_xtra) msg_print("Done.");
   }
}

/*
 * check if there is lava to cool down somewhere
 */
void do_check_lava(void)
{
   if ( (turn % LAVA_TURNS) == 0)
   {
      lava_cool_down();
   }
}

void do_check_load(void)
{
   /* Check time and load */
   if ((0 != check_time()) || (0 != check_load()))
   {
      /* Warning */
      if (closing_flag <= 2)
      {
         /* Disturb */
         disturb(0, 0);

         /* Count warnings */
         closing_flag++;

         /* Message */
         msg_print("The gates to ANGBAND are closing...");
         msg_print("Please finish up and/or save your game.");
      }

      /* Slam the gate */
      else
      {
         /* Message */
         msg_print("The gates to ANGBAND are now closed.");

         /* Stop playing */
         alive = FALSE;
      }
   }
}

void do_check_nightfall(void)
{
   s16b       x, y;
   cave_cell_type *c_ptr;

   /* Hack -- Daybreak/Nighfall in town */
   if (!(turn % ((10L * TOWN_DAWN) / 2)))
   {
      bool dawn;

      /* Check for dawn */
      dawn = (!(turn % (10L * TOWN_DAWN)));

      /* Day breaks */
      if (dawn)
      {
         /* Message */
         msg_print("The sun has risen.");

         /* Hack -- Scan the town */
         for (y = 0; y < cur_hgt; y++)
         {
            for (x = 0; x < cur_wid; x++)
            {
               /* Get the cave grid */
               c_ptr = &dungeon.level[sublevel][y][x];

               /* Assume lit */
               c_ptr->fdat |= CAVE_GLOW;

               /* Hack -- Memorize lit grids if allowed */
               if (view_perma_grids) c_ptr->fdat |= CAVE_MARK;

               /* Hack -- Notice spot */
               note_spot(x, y);
            }
         }
      }

      /* Night falls */
      else
      {
         /* Message */
         msg_print("The sun has fallen.");

         /* Hack -- Scan the town */
         for (y = 0; y < cur_hgt; y++)
         {
            for (x = 0; x < cur_wid; x++)
            {
               /* Get the cave grid */
               c_ptr = &dungeon.level[sublevel][y][x];

               /* Hack -- darken "boring" features */
               if ( (c_ptr->mtyp == DUNG_FLOOR) &&
                    ( (c_ptr->styp == DUNG_FLOOR_NORMAL) ||
                      (c_ptr->styp == DUNG_FLOOR_TRAP) ) )
               {
                  /* Forget the grid */
                  c_ptr->fdat &= ~(CAVE_GLOW | CAVE_MARK);

                  /* Hack -- Notice spot */
                  note_spot(x, y);
               }
            }
         }
      }

      /* Update the monsters */
      p_ptr->update |= (PU_UN_VIEW | PU_VIEW | PU_MONSTERS);

      /* Redraw map */
      p_ptr->redraw1 |= (PR1_MAP);

      /* Window stuff */
      p_ptr->window |= (PW_OVERHEAD);
   }
}

void handle_food_regenerate(void)
{
   s16b regen_amount;

   /* Digest normally */
   /* Every 100 game turns */
   if (!(turn % 100))
   {
      digest_food();
   }

   /* Starve to death (slowly) */
   if (p_ptr->food < PY_FOOD_STARVE)
   {
      s16b food_amount;

      /* Calculate damage */
      food_amount = (PY_FOOD_STARVE - p_ptr->food) / 10;


      /* Take damage */
      take_hit(food_amount, "starvation");
   }

   /* Default regeneration */
   regen_amount = PY_REGEN_NORMAL;

   /* Getting Weak */
   if (p_ptr->food < PY_FOOD_WEAK)
   {
      /* Lower regeneration */
      if (p_ptr->food < PY_FOOD_STARVE)
          regen_amount = 0;
      else if (p_ptr->food < PY_FOOD_FAINT)
          regen_amount = PY_REGEN_FAINT;
      else
          regen_amount = PY_REGEN_WEAK;

      /* Getting Faint */
      if (p_ptr->food < PY_FOOD_FAINT)
      {
         /* Faint occasionally */
         if (!p_ptr->paralyzed && (rand_int(100) < 10))
         {
            /* Message */
            msg_print("You faint from the lack of food.");
            disturb(1, 0);

            /* Hack -- faint (bypass free action) */
            (void)set_paralyzed(p_ptr->paralyzed + 1 + rand_int(5));
         }
      }
   }

   /* Regeneration ability */
   if (p_ptr->regenerate) regen_amount = regen_amount * 2;

   /* Searching or Resting */
   if (p_ptr->searching || p_ptr->resting) regen_amount = regen_amount * 2;

   /* High Priests regenerate quickly */
   if (p_ptr->pclass == CLASS_HIGHPRST) regen_amount = regen_amount * 2;

   /* Regenerate the mana */
   if (p_ptr->csp < p_ptr->msp) regenmana(regen_amount);

   /* Poisoned or cut yields no healing */
   if (p_ptr->poisoned) regen_amount = 0;
   if (p_ptr->cut) regen_amount = 0;

   /* Regenerate Hit Points if needed */
   if (p_ptr->chp < p_ptr->mhp) regenhp(regen_amount);
}

void handle_timeouts(void)
{
   s16b i;
   for (i=0; i < 6; i++)
   {
      if (p_ptr->stat_cnt[i]>0)
      {
         p_ptr->stat_cnt[i]--;
         if (p_ptr->stat_cnt[i]==0)
         {
            do_inc_stat(i, TRUE);
         }
      }
   }

   /* Hack -- Hallucinating */
   if (p_ptr->image) (void)set_image(p_ptr->image - 1);

   /* Blindness */
   if (p_ptr->blind) (void)set_blind(p_ptr->blind - 1);

   /* Times see-invisible */
   if (p_ptr->tim_invis) (void)set_tim_invis(p_ptr->tim_invis - 1);

   /* Timed infra-vision */
   if (p_ptr->tim_infra) (void)set_tim_infra(p_ptr->tim_infra - 1);

   /* Paralysis */
   if (p_ptr->paralyzed) (void)set_paralyzed(p_ptr->paralyzed - 1);

   /* Confusion */
   if (p_ptr->confused) (void)set_confused(p_ptr->confused - 1);

   /* Afraid */
   if (p_ptr->afraid) (void)set_afraid(p_ptr->afraid - 1);

   /* Fast */
   if (p_ptr->fast) (void)set_fast(p_ptr->fast - 1);

   /* Slow */
   if (p_ptr->slow) (void)set_slow(p_ptr->slow - 1);

   /* Protection from evil */
   if (p_ptr->protevil) (void)set_protevil(p_ptr->protevil - 1);

   /* Invulnerability */
   /* jk - it should only dissolve by hits, but not stay there when there */
   /* are no hits - so we dissolve it at 2% + 1 per move */
   if (p_ptr->invuln) (void)set_invuln(p_ptr->invuln - (p_ptr->invuln/50) - 1);

   /* Heroism */
   if (p_ptr->hero) (void)set_hero(p_ptr->hero - 1);

   /* Super Heroism */
   if (p_ptr->shero) (void)set_shero(p_ptr->shero - 1);

   /* Blessed */
   if (p_ptr->blessed) (void)set_blessed(p_ptr->blessed - 1);

   /* Shield */
   if (p_ptr->shield) (void)set_shield(p_ptr->shield - 1);

/* jk */
   if (p_ptr->sliding) (void)set_sliding(p_ptr->sliding - 1);

   if (p_ptr->lift) (void)set_lift(p_ptr->lift - 1);

   if (p_ptr->reflecting) (void)set_reflecting(-1);

   if (p_ptr->fire) (void)set_fire(p_ptr->fire - 1);

   if (p_ptr->cold) (void)set_cold(p_ptr->cold - 1);

   if (p_ptr->acid) (void)set_acid(p_ptr->acid - 1);

   if (p_ptr->elec) (void)set_elec(p_ptr->elec - 1);

   /* Oppose Acid */
   if (p_ptr->oppose_acid) (void)set_oppose_acid(p_ptr->oppose_acid - 1);

   /* Oppose Lightning */
   if (p_ptr->oppose_elec) (void)set_oppose_elec(p_ptr->oppose_elec - 1);

   /* Oppose Fire */
   if (p_ptr->oppose_fire) (void)set_oppose_fire(p_ptr->oppose_fire - 1);

   /* Oppose Cold */
   if (p_ptr->oppose_cold) (void)set_oppose_cold(p_ptr->oppose_cold - 1);

   /* Oppose Poison */
   if (p_ptr->oppose_pois) (void)set_oppose_pois(p_ptr->oppose_pois - 1);

   /*** Poison and Stun and Cut ***/

   /* Poison */
   if (p_ptr->poisoned)
   {
      s16b         adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + 1);
      object_type *i_ptr;

/* jk - prevent frequent 'you remain poisoned' messages with Melkor's Spear */
      i_ptr = &inventory[INVEN_WIELD];
      if (i_ptr->name1!=ART_MELKOR)
      {  /* Apply some healing */
         (void)set_poisoned(p_ptr->poisoned - adjust);
      }
   }

   /* Stun */
   if (p_ptr->stun)
   {
      s16b adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + 1);

      /* Apply some healing */
      (void)set_stun(p_ptr->stun - adjust);
   }

   if (p_ptr->reading)
   {
      p_ptr->reading--;
      delay(5);

      /* the arguments were saved in static variables inside read_spell */
      /* so signal we have read it all */
      if (p_ptr->reading==1) read_spell(NULL, 0);
      p_ptr->redraw2 |= (PR2_READ);
   }

   /* Cut */
   if (p_ptr->cut)
   {
      s16b adjust = (adj_con_fix[p_ptr->stat_ind[A_CON]] + 1);

      /* Hack -- Truly "mortal" wound */
      if (p_ptr->cut > 1000) adjust = 0;

      /* Apply some healing */
      (void)set_cut(p_ptr->cut - adjust);
   }
}

/*
 * Handle certain things once every 10 game turns
 */
static void process_world(void)
{
   s16b i;

   /* Every 10 game turns */
   if (turn % 10) return;

   /*** Check the Time and Load ***/

   if (!(turn % 1000))
   {
      do_check_load();
   }

   /*** Handle the "town" (stores and sunshine) ***/

   /* While in town */
   if ( (!p_ptr->mdepth) && (!p_ptr->sdepth) )
   {
      do_check_nightfall();
   }

   /* While in the dungeon */
   else
   {
      do_check_update_stores();
   }
   do_check_lava();

   /*** Process the monsters ***/

   /* Check for creature generation */
   if (rand_int(MAX_M_ALLOC_CHANCE) == 0)
   {
      /* Make a new monster */
      if (!p_ptr->mdepth)
      {
         (void)alloc_monster(MAX_SIGHT + 5, FALSE, 0);
      }
      else
      {
         (void)alloc_monster(MAX_SIGHT + 5, FALSE, 2);
      }
   }

   /* Hack -- Check for creature regeneration */
   if (!(turn % 100)) regen_monsters();

   /*** Damage over Time ***/

   /* Take damage from poison */
   if (p_ptr->poisoned)
   {
      /* Take damage */
      take_hit(1, "poison");
   }

   /* Take damage from cuts */
   if (p_ptr->cut)
   {
      /* Mortal wound or Deep Gash */
      if (p_ptr->cut > 200)
         i = 3;

      /* Severe cut */
      else if (p_ptr->cut > 100)
         i = 2;

      /* Other cuts */
      else
         i = 1;

      /* Take damage */
      take_hit(i, "a fatal wound");
   }

   /*** Check the Food, and Regenerate ***/
   handle_food_regenerate();

   /*** Timeout Various Things ***/
   handle_timeouts();

   /*** Process Light ***/

   burn_light();

   /*** Process Inventory ***/

   /* Handle experience draining */
   if (p_ptr->exp_drain)
   {
      if ((rand_int(100) < 10) && (p_ptr->exp > 0))
      {
         p_ptr->exp--;
         p_ptr->max_exp--;
         check_experience();
      }
   }

   recharge_inventory();

   /* Feel the inventory */
   sense_inventory();

   /*** cash registers, only on main levels ***/
   /* not too often, or memorizing a high-level spell is impossible */
   if (store_built && (randint(1000)==1) && (sublevel == 0))
   {
      msg_print("You hear the chime of a cash register.");
   }

   /*** Involuntary Movement ***/

   /* Delayed Word-of-Recall */
   if (p_ptr->word_recall)
   {
      /* Count down towards recall */
      p_ptr->word_recall--;

      /* Activate the recall */
      if (!p_ptr->word_recall)
      {
         /* Disturbing! */
         disturb(0,0);

         /* Determine the level */
         if (p_ptr->mdepth)
         {
            msg_print("You feel yourself yanked upwards!");
            if (in_maze(px, py))
            {
               msg_print("You bump your head on the rough ceiling.");
               take_hit(damroll(4,8), "a bruised head");
            }
            else
            {
               p_ptr->new_mdepth = 0;
               p_ptr->new_sdepth = 0;
               new_level_flag = TRUE;
            }
         }
         else
         {
            msg_print("You feel yourself yanked downwards!");
            if (in_maze(px, py))
            {
               msg_print("You stumble and nearly sprain your ankle.");
               take_hit(damroll(4,8), "a nearly sprained ankle");
            }
            else
            {
               if (!p_ptr->max_dlv)
               {
                  p_ptr->new_mdepth = 1;
                  p_ptr->new_sdepth = 0;
               }
               else
               {
                  p_ptr->new_mdepth = p_ptr->max_dlv;
                  p_ptr->new_sdepth = 0;
               }

               new_level_flag = TRUE;
            }
         }
      }
   }
}

#ifdef ALLOW_BORG

/*
 * Verify use of "borg" mode
 */
static bool verify_borg_mode(void)
{
   static int verify = 1;

   /* Ask first time */
   if (verify && ((noscore & 0x0010) != 0x0010) )
   {
      /* Mention effects */
      msg_print("You are about to use the dangerous, unsupported, borg commands!");
      msg_print("Your machine may crash, and your savefile may become corrupted!");
      msg_print(NULL);

      /* Verify request */
      if (!get_check("Are you sure you want to use the borg commands? "))
      {
         return (FALSE);
      }
   }

   /* Verified */
   verify = 0;

   /* Mark savefile */
   noscore |= 0x0010;

   /* Okay */
   return (TRUE);
}


/*
 * Hack -- Declare the Borg Routines
 */
extern void do_cmd_borg(void);

#endif



/*
 * Parse and execute the current command
 * Give "Warning" on illegal commands.
 */
static void process_command(void)
{
dlog(DEBUGKEYS,"dungeon.c: process_command: command_cmd %d ('%c')\n", (int)p_ptr->command_cmd, p_ptr->command_cmd);
   /* Parse the command */
   switch (p_ptr->command_cmd)
   {
      /* commands that are not always allowed: */
#ifdef ALLOW_WIZARD
      /* Special "debug" commands */
      case KTRL('A'): do_cmd_wizard(); break;
#endif
#ifdef ALLOW_BORG

      /* Special "borg" commands */
      case KTRL('B'):
      {
         if (verify_borg_mode()) do_cmd_borg();
         break;
      }

#endif


#ifndef VERIFY_SAVEFILE
      /* Hack -- Save and don't quit */
      case KTRL('S'):
      case ('$'):
         do_cmd_save_game(TRUE);
         break;

      case KTRL('L'):
      case KTRL('Q'): signals_handle_tstp(); quit(NULL); /* quit without saving */
#endif

      /* Ignore */
      case ESCAPE:
      case ' ':
      case '\r':
      case '\n':
      case '\t':   break;

      case '#': do_cmd_check();                      break; /* Check artifacts/uniques/whatever     */
      case '(': do_cmd_load_screen();                break; /* Load "screen dump"                   */
      case ')': do_cmd_save_screen();                break; /* Save "screen dump"                   */
      case '*': do_cmd_target();                     break; /* Target monster or location           */
      case '+': do_cmd_tunnel();                     break; /* Dig a tunnel                         */
      case ',': do_cmd_hold();                       break; /* Stay still (usually do not pick up)  */
      case '-': do_cmd_walk(!always_pickup);         break; /* Move (usually do not pick up)        */
      case '.': do_cmd_run();                        break; /* Begin Running -- Arg is Max Distance */
      case '/': do_cmd_query_symbol();               break; /* Identify symbol                      */
      case ':': do_cmd_note();                       break; /* Take notes                           */
      case ';': do_cmd_walk(always_pickup);          break; /* Move (usually pick up things)        */
      case '<': do_cmd_go_up();                      break; /* Go up staircase                      */
      case '>': do_cmd_go_down();                    break; /* Go down staircase                    */
      case '?': do_cmd_help("help.hlp");             break; /* Help                                 */
      case '@': do_cmd_system_command();             break; /* Interact with the options etc.       */
      case 'A': do_cmd_activate();                   break; /* Activate an artifact                 */
      case 'B': do_cmd_bash();                       break; /* Bash a door                          */
      case 'C': do_cmd_change_name();                break; /* Character description                */
      case 'D': do_cmd_disarm();                     break; /* Disarm a trap or chest               */
      case 'E': do_cmd_eat_food();                   break; /* Eat some food                        */
      case 'F': do_cmd_refill();                     break; /* Fuel your lantern/torch              */
      case 'I': do_cmd_observe();                    break; /* Identify an object                   */
      case 'L': do_cmd_locate();                     break; /* Locate player on map                 */
      case 'M': do_cmd_view_map(TRUE);               break; /* scalable dungeon map                 */
      case 'Q': do_cmd_suicide();                    break; /* Quit (commit suicide)                */
      case 'R': do_cmd_rest();                       break; /* Rest -- Arg is time                  */
      case 'S': do_cmd_toggle_search();              break; /* Toggle search mode                   */
      case 'T': do_cmd_tunnel();                     break; /* Dig a tunnel                         */
      case 'V': do_cmd_version();                    break; /* Version info                         */
      case '_': do_cmd_store();                      break; /* Enter store                          */
      case 'b': do_cmd_browse();                     break; /* Browse a book                        */
      case 'c': do_cmd_close();                      break; /* Close a door                         */
      case 'd': do_cmd_drop();                       break; /* Drop an item                         */
      case 'e': do_cmd_equip();                      break; /* Equipment list                       */
      case 'f': do_cmd_fire();                       break; /* Fire an item                         */
      case 'g': do_cmd_stay();                       break; /* Stay still (usually do not pick up)  */
      case 'i': do_cmd_inven();                      break; /* Inventory list                       */
      case 'j': do_cmd_spike();                      break; /* Jam a door with spikes               */
      case 'k': do_cmd_destroy();                    break; /* Destroy an item                      */
      case 'l': do_cmd_look();                       break; /* Look around                          */
      case 'm': do_cmd_cast_spell();                 break; /* use a spell                          */
      case 'o': do_cmd_open();                       break; /* Open a door or chest                 */
      case 'q': do_cmd_quaff_potion();               break; /* Quaff a potion                       */
      case 'r': do_cmd_read_scroll();                break; /* Read a scroll                        */
      case 's': do_cmd_search();                     break; /* Search for traps/doors               */
      case 't': do_cmd_takeoff();                    break; /* Take off equipment                   */
      case 'v': do_cmd_throw();                      break; /* Throw an item                        */
      case 'w': do_cmd_wield();                      break; /* Wear/wield equipment                 */
      case 'z': do_cmd_zap();                        break; /* Zap a rod                            */
      case '{': do_cmd_inscribe();                   break; /* Inscribe an object                   */
      case '}': do_cmd_uninscribe();                 break; /* Uninscribe an object                 */

      /* control key sequences: */

      case KTRL('F'): do_cmd_feeling();            break; /* Repeat level feeling */
      case KTRL('O'): do_cmd_message_one();        break; /* Show previous message */
      case KTRL('P'): do_cmd_messages();           break; /* Show previous messages */
      case KTRL('R'): do_cmd_redraw();             break; /* Redraw the screen */
      case KTRL('X'): alive = FALSE;               break; /* Save and quit */
      case KTRL('W'): do_cmd_toggle_wizard_mode(); break; /* Toggle Wizard Mode */

      default:
      {
         if (show_key_help && get_check("Unknown key! Display keyboard Usage Reminder? "))
         {
            show_file("quickkey.txt", "A Quick keyboard usage reminder");
         }
         else
         {
            prt("Press ? for help.", 0, MESSAGE_ROW); return;      /* Hack -- Unknown command */
         }
      }
   }
}

/*
 * Hack -- helper function for "process_player()"
 *
 * Check for changes in the "monster memory"
 */
static void process_player_aux(void)
{
   static int old_monster_race_idx = 0;

   static u64b old_r_flags1 = 0L;
   static u64b old_r_flags2 = 0L;
   static u64b old_r_flags3 = 0L;
   static u64b old_r_flags4 = 0L;
   static u64b old_r_flags5 = 0L;
   static u64b old_r_flags6 = 0L;

   static byte old_r_blows0 = 0;
   static byte old_r_blows1 = 0;
   static byte old_r_blows2 = 0;
   static byte old_r_blows3 = 0;

   static byte old_r_cast_inate = 0;
   static byte old_r_cast_spell = 0;

   /* Tracking a monster */
   if (p_ptr->monster_race_idx)
   {
      monster_race *r_ptr;

      /* Acquire monster race */
      r_ptr = &r_info[p_ptr->monster_race_idx];

      /* Check for change of any kind */
      if ((old_monster_race_idx != p_ptr->monster_race_idx) ||
          (old_r_flags1 != r_ptr->r_flags1) ||
          (old_r_flags2 != r_ptr->r_flags2) ||
          (old_r_flags3 != r_ptr->r_flags3) ||
          (old_r_flags4 != r_ptr->r_flags4) ||
          (old_r_flags5 != r_ptr->r_flags5) ||
          (old_r_flags6 != r_ptr->r_flags6) ||
          (old_r_blows0 != r_ptr->r_blows[0]) ||
          (old_r_blows1 != r_ptr->r_blows[1]) ||
          (old_r_blows2 != r_ptr->r_blows[2]) ||
          (old_r_blows3 != r_ptr->r_blows[3]) ||
          (old_r_cast_inate != r_ptr->r_cast_inate) ||
          (old_r_cast_spell != r_ptr->r_cast_spell))
      {
         /* Memorize old race */
         old_monster_race_idx = p_ptr->monster_race_idx;

         /* Memorize flags */
         old_r_flags1 = r_ptr->r_flags1;
         old_r_flags2 = r_ptr->r_flags2;
         old_r_flags3 = r_ptr->r_flags3;
         old_r_flags4 = r_ptr->r_flags4;
         old_r_flags5 = r_ptr->r_flags5;
         old_r_flags6 = r_ptr->r_flags6;

         /* Memorize blows */
         old_r_blows0 = r_ptr->r_blows[0];
         old_r_blows1 = r_ptr->r_blows[1];
         old_r_blows2 = r_ptr->r_blows[2];
         old_r_blows3 = r_ptr->r_blows[3];

         /* Memorize castings */
         old_r_cast_inate = r_ptr->r_cast_inate;
         old_r_cast_spell = r_ptr->r_cast_spell;

         /* Window stuff */
         p_ptr->window |= (PW_MONSTER);

         /* Window stuff */
         window_stuff();
      }
   }
}

/*
 * Process the player
 */
static void process_player()
{
   s16b i;

   /* Give the player some energy */
   p_ptr->energy += extract_energy[p_ptr->pspeed];

   /* No turn yet */
   if (p_ptr->energy < 100) return;

   /*** Handle Resting ***/

   /* Check "Resting" status */
   if (p_ptr->resting)
   {
      /* +n -> rest for n turns */
      if (p_ptr->resting > 0)
      {
         /* Reduce rest count */
         p_ptr->resting--;

         /* Redraw the state */
         p_ptr->redraw1 |= (PR1_STATE);
      }

      /* -1 -> rest until HP/mana restored */
      else if (p_ptr->resting == -1)
      {
         /* Stop p_ptr->resting */
         if ((p_ptr->chp == p_ptr->mhp) &&
             (p_ptr->csp == p_ptr->msp))
         {
            disturb(0, 0);
         }
         p_ptr->redraw1 |= (PR1_STATE);
      }

      /* -2 -> like -1, plus blind/conf/fear/stun/slow/stone/halluc/recall */
      /* Note: stop (via "disturb") as soon as blind or recall is done */
      else if (p_ptr->resting == -2)
      {
         bool stop = TRUE;

         if (p_ptr->chp != p_ptr->mhp) stop = FALSE;
         if (p_ptr->csp != p_ptr->msp) stop = FALSE;
         if (p_ptr->blind || p_ptr->confused || p_ptr->poisoned) stop = FALSE;
         if (p_ptr->afraid || p_ptr->stun || p_ptr->cut) stop = FALSE;
         if (p_ptr->slow || p_ptr->paralyzed || p_ptr->sliding) stop = FALSE;
         if (p_ptr->food>PY_FOOD_MAX) stop = FALSE;
         if (p_ptr->image || p_ptr->word_recall) stop = FALSE;
         for (i=0; i<6; i++)
         {
            if (p_ptr->stat_cnt[i]>0) stop = FALSE;
         }
         if (stop)
         {
            disturb(0, 0);
         }
         p_ptr->redraw1 |= (PR1_STATE);
      }
   }

   /*** Handle actual user input ***/

   /* Hack -- Check for "player interrupts" */
   if ((p_ptr->command_rep && !(p_ptr->command_rep & 0x07)) ||
       (p_ptr->running && !(p_ptr->running & 0x07)) ||
       (p_ptr->resting && !(p_ptr->resting & 0x07)) ||
       (p_ptr->reading))
   {
      /* Do not wait */
      inkey_scan = TRUE;

      /* Check for a key */
      if (inkey())
      {
         /* Flush input */
         flush();

         /* Disturb */
         disturb(0, 0);

         /* Hack -- Show a Message */
         msg_print("Cancelled.");
         if (p_ptr->reading)
         {
            p_ptr->reading=0;
            p_ptr->redraw2 |= PR2_READ;
            redraw_stuff();
            /* signal we interrupted reading */
            read_spell(NULL, 0);
         }
      }
   }

   /* Hack -- constant hallucination */
   if (p_ptr->image) p_ptr->redraw1 |= (PR1_MAP);

   /* Mega-Hack -- Random teleportation XXX XXX XXX */
   if ((p_ptr->teleport) && (rand_int(100) < 1))
   {
       /* Teleport player */
       teleport_player(40);
   }

   /* Repeat until out of energy */
   while (p_ptr->energy >= 100)
   {
      /* Notice stuff (if needed) */
      if (p_ptr->notice) notice_stuff();

      /* Update stuff (if needed) */
      if (p_ptr->update) update_stuff();

      /* Redraw stuff (if needed) */
      p_ptr->redraw1 |= PR1_POSITION;
      if (p_ptr->redraw1) redraw_stuff();

      /* Place the cursor on the player */
      move_cursor_relative(px, py);

      /* Refresh (optional) */
      if (fresh_before) Term_fresh();

      /* Hack -- Pack Overflow */
      if (inventory[INVEN_PACK].k_idx)
      {
         s16b item = INVEN_PACK;
         s16b amt;
         char i_name[80];
         object_type *i_ptr;

         i_ptr = &inventory[item];     /* Access the slot to be dropped */
         amt = i_ptr->number;                       /* Drop all of that item */

         disturb(0, 0);                             /* Disturbing */
         msg_print("Your pack overflows!");         /* Warning */
         object_desc(i_name, i_ptr, TRUE, 3);       /* Describe */

         /* Message */
         msg_format("You drop %s (%c).", i_name, index_to_label(item));

         (void)drop_near(i_ptr, 0, px, py, 0, FALSE, FALSE);
         /* Drop it (carefully) near the player */

         item_increase(item, -amt, px, py);      /* Decrease the item, optimize. */
         item_optimize(item, px, py);

         if (p_ptr->notice) notice_stuff();       /* Notice stuff (if needed) */
         if (p_ptr->update) update_stuff();       /* Update stuff (if needed) */
         if (p_ptr->redraw1) redraw_stuff();      /* Redraw stuff (if needed) */
      }

      /* Hack -- cancel "lurking browse mode" */
      if (!p_ptr->command_new) p_ptr->command_see = FALSE;

      /* Assume free turn */
      energy_use = 0;

dlog((DEBUGMOVES|DEBUGKEYS),"dungeon.c: process_player: p_ptr->running now %d\n", p_ptr->running);
      /* Hack -- Resting */
      if ((p_ptr->resting) ||
          (p_ptr->paralyzed) ||
          (p_ptr->stun >= 100) ||
          (p_ptr->reading))
      {
         /* Take a turn */
         energy_use = 100;
      }

      /* Hack -- Running */
      else if (p_ptr->running)
      {
         /* Take a step */
         run_step(0);
      }

      /* Repeated command */
      else if (p_ptr->command_rep)
      {
         /* Count this execution */
         p_ptr->command_rep--;
dlog(DEBUGKEYS,"dungeon.c: process_player: repeating, counter now %d\n", p_ptr->command_rep);
         /* Redraw the state */
         p_ptr->redraw1 |= (PR1_STATE);

         /* Redraw stuff */
         redraw_stuff();

         /* Hack -- Assume messages were seen */
         msg_flag = FALSE;

         /* Clear the top line */
         prt("", 0, MESSAGE_ROW);

         /* Process the command */
         process_command();
      }

      /* Normal command */
      else
      {
         /* Check monster recall */
         process_player_aux();

         /* Place the cursor on the player */
         move_cursor_relative(px, py);

         /* Get a command (new or old) */
         request_command();

         /* Process the command */
         process_command();
      }

      /* Notice stuff */
      if (p_ptr->notice) notice_stuff();

      /* Use some energy, if required */
      if (energy_use)
      {
         s16b i;
         p_ptr->energy -= energy_use;

/* this is copied from Zangband */
         for (i = 1; i < mn_max; i++)
         {
            monster_type *m_ptr;

            /* Access monster */
            m_ptr = &mn_list[i];

            /* Skip dead monsters */
            if (!m_ptr->r_idx) continue;

            /* Nice monsters get mean */
            if (m_ptr->mflag & (MFLAG_NICE)) m_ptr->mflag &= ~(MFLAG_NICE);
         }
      }
      /* Hack -- notice death or departure */
      if (!alive || death || new_level_flag) break;
   }
}

/*
 * Interact with the current dungeon level.
 *
 * This function will not exit until the level is completed,
 * the user dies, or the game is terminated.
 */
static void handle_dungeon(void)
{

   /* Reset various flags */
   new_level_flag = FALSE;

   /* Reset the "command" vars */
   p_ptr->command_cmd = 0;
   p_ptr->command_new = 0;
   p_ptr->command_rep = 0;
   p_ptr->command_arg = 0;
   p_ptr->command_dir = 0;

   /* Cancel the target */
   target_who = 0;

   /* Cancel the health bar */
   health_track(0);

   /* Disturb */
   disturb(1, 0);

   /* Remember deepest dungeon level visited */
   if (p_ptr->mdepth > (unsigned)(p_ptr->max_dlv))
   {
      p_ptr->max_dlv = p_ptr->mdepth;
   }

   /* Paranoia -- No stairs down from Quest */
   if (is_quest(p_ptr->mdepth)) create_down_stair = FALSE;

   /* Paranoia -- no stairs from town */
   if (!p_ptr->mdepth) create_down_stair = create_up_stair = FALSE;

   /* Make a stairway. */
   if (create_up_stair || create_down_stair)
   {
      /* Place a stairway */
      if (valid_grid(px, py))
      {
         cave_cell_type *c_ptr;

         /* Delete the old object - any */
         delete_object(px, py,-1);

         /* Access the cave grid */
         c_ptr = &dungeon.level[sublevel][py][px];

         /* Make stairs */
         if (create_down_stair)
         {
            place_main_down_stair(px, py,FALSE);
         }
         else
         {
            place_main_up_stair(px, py,FALSE);
         }
      }

      /* Cancel the stair request */
      create_down_stair = create_up_stair = FALSE;
   }

   /* Choose a panel row */
   p_ptr->wy = ((py - SCREEN_HGT / 4) / (SCREEN_HGT / 2));
   if (p_ptr->wy > panel_max_rows)
   {
      p_ptr->wy = panel_max_rows;
   }
   else
   {
      if (p_ptr->wy < 0) p_ptr->wy = 0;
   }

   /* Choose a panel col */
   p_ptr->wx = ((px - SCREEN_WID / 4) / (SCREEN_WID / 2));
   if (p_ptr->wx > panel_max_cols)
   {
      p_ptr->wx = panel_max_cols;
   }
   else
   {
      if (p_ptr->wx < 0) p_ptr->wx = 0;
   }

   /* Recalculate the boundaries */
   panel_bounds();

   /* Flush messages */
   msg_print(NULL);

   /* Enter "xtra" mode */
   character_xtra = TRUE;

   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

   /* Window stuff */
   p_ptr->window |= (PW_MONSTER);

   /* Window stuff */
   p_ptr->window |= (PW_OVERHEAD);

   /* Redraw dungeon */
   p_ptr->redraw1 |= (PR1_WIPE | PR1_BASIC | PR1_EXTRA);
   update_stuff();
dlog(DEBUGFLOW,"dungeon.c: handle_dungeon: step 1b\n");
   redraw_stuff();

   /* Redraw map */
   p_ptr->redraw1 |= (PR1_MAP);
   p_ptr->redraw2 |= PR2_EQUIPPY;

   /* Window stuff */
   p_ptr->window |= (PW_OVERHEAD | PW_MESSAGE);
   update_stuff();
   redraw_stuff();
   window_stuff();

   /* Update stuff */
   p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);
   update_stuff();
   redraw_stuff();

   /* Calculate torch radius */
   p_ptr->update |= (PU_TORCH);

   /* Update stuff */
   update_stuff();
   redraw_stuff();

   /* Update stuff */
   p_ptr->update |= (PU_UN_VIEW | PU_VIEW | PU_FLOW | PU_DISTANCE);

   /* Update stuff */
   update_stuff();
dlog(DEBUGFLOW,"dungeon.c: handle_dungeon: step 1c\n");

   /* Redraw stuff */
   redraw_stuff();

   /* Leave "xtra" mode */
   character_xtra = FALSE;

   /* Update stuff */
   p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);

   /* Combine / Reorder the pack */
   p_ptr->notice |= (PN_COMBINE | PN_REORDER);

   /* Notice stuff */
   notice_stuff();

   /* Update stuff */
   update_stuff();

   /* Redraw stuff */
   redraw_stuff();

   /* Refresh */
   Term_fresh();

   /* Announce (or repeat) the feeling */
   if (p_ptr->mdepth || p_ptr->sdepth) do_cmd_feeling();

   /* Hack -- notice death or departure */
   if (!alive || death || new_level_flag) return;

   /*** Process this dungeon level ***/

   /* Reset the monster generation level */
   monster_level = p_ptr->mdepth;

   /* Reset the object generation level */
   object_level = p_ptr->mdepth;

   /* Main loop */
   while (TRUE)
   {
      /* Hack -- Compact the object list occasionally */
      /* jk - don't touch the monster inventory objects */
      if (i_top + 16 > MAX_I_IDX) compact_objects(32,FALSE);

      /* Hack -- Compact the monster list occasionally */
      if (mn_top + 32 > MAX_M_IDX) compact_monsters(64, FALSE);
      /* if that doesn't help - also compact monsters with inven! */
      if (mn_top + 16 > MAX_M_IDX) compact_monsters(32, TRUE);

#if (debuglevel & DEBUGMONST)
{
   s16b i;
   dlog(DEBUGMONST,"dungeon.c: handle_dungeon: monster list around player @ %d,%d:\n", px, py);
   for (i=0; i<mn_max; i++)
   {
      monster_type *m_ptr = &mn_list[i];
      monster_race *r_ptr = &r_info[m_ptr->r_idx];

      if (mn_list[i].r_idx == 0) continue;
      if (distance(px, py, m_ptr->fx, m_ptr->fy) > 10) continue;
      dlog(DEBUGMONST,"dungeon.c: handle_dungeon: m_idx %d (%s) @ %d,%d hp %d ml %d los %d\n",
                      i, r_name + r_ptr->name, m_ptr->fx, m_ptr->fy,
                      m_ptr->hp, m_ptr->ml, m_ptr->los);
      if (m_ptr->has_drop)
      {
         s16b           is_idx = item_set_this_monster(i);
         item_set_type *is_ptr = &is_list[is_idx];
         s16b           number = items_in_set (is_idx);
         s16b j;

         dlog(DEBUGMONST,"dungeon.c: handle_dungeon: is_idx %d has %d items\n", is_idx, number);
         for (j = 0; j < number; j++)
         {
            object_type *i_ptr = &i_list[is_ptr->index[j]];

            if (i_ptr->k_idx)
            {
               dlog(DEBUGMONST,"dungeon.c: handle_dungeon: index %d k_idx %d @ %d,%d,%d (%s)\n",
                               is_ptr->index[j], i_ptr->k_idx,i_ptr->ix,i_ptr->iy,i_ptr->iz,
                               k_name + k_info[i_ptr->k_idx].name);
            }
            else
            {
               dlog(DEBUGMONST,"dungeon.c: handle_dungeon: index %d k_idx %d @ %d,%d,%d (<k_idx NULL>)\n",
                               is_ptr->index[j], i_ptr->k_idx,i_ptr->ix,i_ptr->iy,i_ptr->iz);
            }
         }
      }
   }
}
#endif

      process_player();                             /* Process the player */

      if (p_ptr->notice) notice_stuff();            /* Notice stuff */
      if (p_ptr->update) update_stuff();            /* Update stuff */
      if (p_ptr->redraw1 || p_ptr->redraw2)         /* Redraw stuff */
          redraw_stuff();
      if (p_ptr->window) window_stuff();            /* Redraw stuff */

      move_cursor_relative(px, py);                 /* Hack -- Hilite the player */
      if (fresh_after) Term_fresh();                /* Optional fresh */

      if (!alive || death || new_level_flag) break; /* Hack -- Notice death or departure */

      process_monsters();  /* Process all of the monsters */

      if (p_ptr->notice) notice_stuff();     /* Notice stuff */
      if (p_ptr->update) update_stuff();     /* Update stuff */
      if (p_ptr->redraw1 || p_ptr->redraw2) redraw_stuff(); /* Redraw stuff */

      move_cursor_relative(px, py);          /* Hack -- Hilite the player */
      if (fresh_after) Term_fresh();         /* Optional fresh */

      /* Hack -- Notice death or departure */
      if (!alive || death || new_level_flag) break;

      /* Process all of the objects */
      process_objects();

      /* Notice stuff */
      if (p_ptr->notice) notice_stuff();

      /* Update stuff */
      if (p_ptr->update) update_stuff();

      /* Redraw stuff */
      if (p_ptr->redraw1) redraw_stuff();

      /* Hack -- Hilite the player */
      move_cursor_relative(px, py);

      /* Optional fresh */
      if (fresh_after) Term_fresh();

      /* Hack -- Notice death or departure */
      if (!alive || death || new_level_flag) break;

      /* Process the world */
      process_world();

      /* Notice stuff */
      if (p_ptr->notice) notice_stuff();

      /* Update stuff */
      if (p_ptr->update) update_stuff();

      /* Redraw stuff */
      if (p_ptr->redraw1) redraw_stuff();

      /* Hack -- Hilite the player */
      move_cursor_relative(px, py);

      /* Optional fresh */
      if (fresh_after) Term_fresh();

      /* Hack -- Notice death or departure */
      if (!alive || death || new_level_flag) break;

      /* Count game turns */
      turn++;
   }

   /* Notice stuff */
   if (p_ptr->notice) notice_stuff();

   /* Update stuff */
   if (p_ptr->update) update_stuff();

   if (p_ptr->redraw1 || p_ptr->redraw2) redraw_stuff(); /* Redraw stuff */

   /* Cancel the target */
   target_who = 0;

   /* Cancel the health bar */
   health_track(0);

   /* Forget the old view */
   forget_view();
}

/*
 * Process some user pref files
 */
static void process_last_prf(void)
{
   char buf[128];
dlog(DEBUGPREF,"dungeon.c: process_last_prf: about to start\n");

/* this comes from main-x11.c. If it is executed there, the game will hang
 * and eventually overflow the stack. Problem is get_f_idx is called for
 * features in those .prf files, and when f_info is not loaded yet, this
 * will not succeed.
 */
#ifdef USE_X11
dlog(DEBUGPREF,"dungeon.c: process_last_prf: X11 compiled in\n");
   /* Attempt to use the "main-x11.c" support */
   if (streq(ANGBAND_SYS, "x11") ||
       streq(ANGBAND_SYS, "xaw") )
   {
dlog(DEBUGPREF,"dungeon.c: process_last_prf: X11 system found\n");
      /* Load colors */
      if (use_graphics)
      {
         /* Process "graf-x11.prf" XXX XXX XXX */
         (void)process_pref_file("graf-x11.prf");
      }
      else
      {
         /* Process "font-x11.prf" XXX XXX XXX */
         (void)process_pref_file("font-x11.prf");
      }
   }
#endif


dlog(DEBUGPREF,"dungeon.c: process_last_prf: opening user.prf\n");

   /* Process the "user.prf" file */
   (void)process_pref_file("user.prf");

   /* Process the "PLAYER.prf" file */
   sprintf(buf, "%s.prf", player_name);
dlog(DEBUGPREF,"dungeon.c: process_last_prf: opening %s\n", buf);

   /* Process the "PLAYER.prf" file */
   (void)process_pref_file(buf);
dlog(DEBUGPREF,"dungeon.c: process_last_prf: returning\n");
}

static bool cheat_death(void)
{
   bool result = FALSE;

/* jk - store old quick messages */
   bool old_quick_messages = quick_messages;

   quick_messages=FALSE;

   /* Mega-Hack -- Allow player to cheat death */
/* jk - this was !get_check("die?") - that is deadly with quick_messages on */
   if (wizard || ((cheat_live) && get_check("Live on? ")))
   {
      /* Mark savefile */
      noscore |= 0x0001;

      /* Message */
      msg_print("You invoke wizard mode and cheat death.");
      msg_print(NULL);

      /* Restore hit points */
      p_ptr->chp = p_ptr->mhp;
      p_ptr->chp_frac = 0;

      /* Restore spell points */
      p_ptr->csp = p_ptr->msp;
      p_ptr->csp_frac = 0;

      /* Hack -- Healing */
      (void)set_blind(0);
/* jk */
      (void)set_sliding(0);
      (void)set_fire(0);
      (void)set_cold(0);
      (void)set_acid(0);
      (void)set_elec(0);

      (void)set_confused(0);
      (void)set_poisoned(0);
      (void)set_afraid(0);
      (void)set_paralyzed(0);
      (void)set_image(0);
      (void)set_stun(0);
      (void)set_cut(0);

      /* Hack -- Prevent starvation */
      (void)set_food(PY_FOOD_MAX - 1);

      /* XXX XXX XXX */
      if (p_ptr->word_recall)
      {
         /* Message */
         msg_print("A tension leaves the air around you...");

         /* Hack -- Prevent recall */
         p_ptr->word_recall = 0;
      } /* recall */

      if (dungeon.level[sublevel][py][px].fdat & CAVE_AREN)
      {
         s16b nx, ny;
         kill_arena_monsters();
         arena_reward = -1;
         been_in_arena = FALSE;
         nx = SCREEN_WID;
         ny = SCREEN_HGT/2;
         (void)set_grid_type(nx, ny, DUNG_FLOOR, DUNG_FLOOR_NORMAL,
                             GRID_KEEP, CAVE_GLOW | CAVE_MARK | CAVE_AREN | CAVE_VIEW);
         note_spot(nx, ny);
         lite_spot(nx, ny);
      }

      /* Note cause of death XXX XXX XXX */
      (void)strcpy(died_from, "Cheating death");

/* jk - wizard mode doesn't zap you to town */
      if (!wizard)
      {
         /* Teleport to town */
         new_level_flag = TRUE;

         /* Go to town */
         p_ptr->new_mdepth = 0;
         p_ptr->new_mdepth = 0;
      } /* wizard */

      /* Do not die */
      death = FALSE;
      result = TRUE;

   } /* (wizard || ((cheat_live) && get_check("Live on? ")) */
   quick_messages=old_quick_messages;
   return (result);
}

/*
 * finds staircases on the new level which point to where we came from
 * leave number found in temp_n, coordinates in temp_x, temp_y
 */
static void find_staircases(s16b old_mlevel, s16b old_slevel, bool sublevel_too)
{
   s16b max_x = cur_wid;
   s16b max_y = cur_hgt;
   s16b x, y;

   temp_n=0;

   /* if we arrive in town, we arrive in town, not in the jungle */
   if (!p_ptr->mdepth && !p_ptr->sdepth)
   {
      max_x = TOWN_WID;
      max_y = TOWN_HGT;
   }
/* now where do we enter this level? - first we see if there are any stairs */
/* on the new level which lead to the old level */

   for (y = 0; y < max_y; y++)
   {
      for (x = 0; x < max_x; x++)
      {
         u16b tmp_main, tmp_sub;

         if (dungeon.level[sublevel][y][x].mtyp!=DUNG_STAIR) continue;
         /* try to find stairs which lead from the exact old level to exactly here, */
         /* or at least from the old main level to the current main level */
         get_depth(dungeon.level[sublevel][y][x].extra, &tmp_main, &tmp_sub);
         if ( (!sublevel_too && ((tmp_main == old_mlevel) && (tmp_sub == 0)) ) ||
              (sublevel_too && (tmp_main == old_mlevel) && (tmp_sub == old_slevel) ) )
         {
            temp_x[temp_n]=x;
            temp_y[temp_n++]=y;
         }
      }
   }
}

/*
 * this function handles a new level, including
 * the determination on what stair the player arrives and
 * shuffling of stores if we just got into town
 */
void handle_new_level(s16b old_mlevel, s16b old_slevel)
{
   s16b x, y;

   /* we are here, and the dungeon is this size: */
   cur_wid = dungeon.level_wid[p_ptr->sdepth];
   cur_hgt = dungeon.level_hgt[p_ptr->sdepth];

dlog(DEBUGFLOW,"dungeon.c: handle_new_level: starting, now %d,%d from %d,%d\n",
               p_ptr->mdepth, p_ptr->sdepth, old_mlevel, old_slevel);

   /* did we only move to a different sublevel? */
   /* note that we can move from level 1,0 to level 1,0 if a level couldn't be read from disk for example */
   if ((old_mlevel == p_ptr->mdepth) && (old_slevel != p_ptr->sdepth))
   {
dlog(DEBUGFLOW,"dungeon.c: handle_new_level: different sublevel\n");

      /* find out which staircases on the current level point back at */
      /* where we came from - including the sublevel                  */
      find_staircases(old_mlevel, old_slevel, TRUE);

dlog(DEBUGFLOW,"dungeon.c: handle_new_level: %d stairs found\n", temp_n);
      if (temp_n>0)
/* if there are such stairs, use one at random */
      {
         s16b n=rand_int(temp_n);
         px=temp_x[n];
         py=temp_y[n];
      }
/* player_new_spot will not work here, as it doesn't leave the player in a vault */
      else /* we shouldn't be here! */
      {
dlog(DEBUGALWAYS,"dungeon.c: handle_new_level: returning to %d,%d from %d,%d - no staircases found?\n",
                 p_ptr->mdepth, p_ptr->sdepth, old_mlevel, old_slevel);
         if ((!p_ptr->mdepth) && (!p_ptr->sdepth))
         {
            new_player_spot(TRUE);
         }
         else
         {
            new_player_spot(FALSE);
         }
      }
      return;
   }

   if (used_stairs)
   {
      find_staircases(old_mlevel, old_slevel, FALSE);
      if (temp_n>0)
/* if there are such stairs, use one at random */
      {
         s16b n=rand_int(temp_n);
         px=temp_x[n];
         py=temp_y[n];
      }
      else
      {
/* if not, we create them at a random location */
         x=rand_range(1,cur_wid);
         y=rand_range(1,cur_hgt);
         while (!naked_grid_bold(x,y))
         {
            x=rand_range(1,cur_wid);
            y=rand_range(1,cur_hgt);
         }
         if (old_mlevel<p_ptr->mdepth)
         {
            place_main_up_stair(x, y, TRUE);
            point_stair_to_level(x, y, old_mlevel, old_slevel);
            note_spot(x, y);
         }
         else
         {
            place_main_down_stair(x, y, TRUE);
            point_stair_to_level(x, y, old_mlevel, old_slevel);
            note_spot(x, y);
         }
/* and we end up there */
         px=x;
         py=y;
      } /* no stairs found */
   } /* used_stairs */
   else if (old_mlevel != -1)
   {
      if ((!p_ptr->mdepth) && (!p_ptr->sdepth))
      {
         new_player_spot(TRUE);
      }
      else
      {
         new_player_spot(FALSE);
      }
   }
   do_cmd_redraw();

   /* reshuffle the stores at level 0 after reading from disk */
   if ((p_ptr->mdepth==0) & (p_ptr->sdepth==0))
   {
      old_turn = level_info[p_ptr->mdepth].last_visit;
      if ((turn-old_turn) > STORE_TURNS)
      {
         /* Message */
         if (cheat_xtra) msg_print("Updating Stores after reading level...");
         while ((turn-old_turn) > STORE_TURNS)
         {
            s16b n;
dlog(DEBUGFLOW,"dungeon.c: handle_new_level: turn %ld old_turn %ld updating stores\n",
               turn, old_turn);
            old_turn += STORE_TURNS;

            /* Maintain each shop (except home) */
            for (n = 0; n <num_stores; n++)
            {
               /* Maintain */
               store_type *st_ptr = &store[n];
               if (st_ptr->store_type != DUNG_ENTR_HOME) store_maint(n);
            }

            /* New owners */
            if (shuffle_owners && (rand_int(STORE_SHUFFLE) == 0))
            {
               if (cheat_xtra) msg_print("Shuffling a Store...");
               store_shuffle();
            }
         }
      }
      /* Message */
      if (cheat_xtra) msg_print("Done.");
   }

   old_turn = 0;
   used_stairs = FALSE;
}

/*
 * try to load or generate a level new level at p_ptr->mdepth
 * if old_mlevel = -1, there is no old_mlevel.
 */
void get_new_level(s16b old_mlevel, s16b old_slevel)
{
   s16b error;

   /* there's really no need to start wiping things if we */
   /* just visit another sublevel */
   if (p_ptr->mdepth == old_mlevel)
   {
      /* make sure we end up on a staircase within the new borders */
      /* the new level may not be as large as this one, so px,py   */
      /* should change!                                            */
      handle_new_level(old_mlevel, old_slevel);
      return;
   }

   if ( (save_levels || (p_ptr->mdepth == 0)) && level_info[p_ptr->mdepth].saved)
   {
      /* this can safely be done, because inventory items are not */
      /* stored in i_list */
      wipe_old_level(p_ptr->mdepth);
      error = rd_level();
      if (error)
      {
         wipe_old_level(p_ptr->mdepth);
         /* so that we recreate this level later on */
         level_info[p_ptr->mdepth].saved = FALSE;
      }
      else
      {
         handle_new_level(old_mlevel, old_slevel);
      }
   }
   if ( !(save_levels || (p_ptr->mdepth == 0)) || !level_info[p_ptr->mdepth].saved)
   {
      /* Make a new level */
dlog(DEBUGFLOW,"dungeon.c: get_new_level: generating new level mdepth,sdepth %d,%d main %d sub %d\n",
               p_ptr->mdepth, p_ptr->sdepth,baselevel, sublevel);
      generate_cave();
dlog(DEBUGFLOW,"dungeon.c: get_new_level: new level (size %d x %d), px,py %d,%d\n",
               dungeon.level_wid[p_ptr->sdepth], dungeon.level_hgt[p_ptr->sdepth], px, py);
   }
   baselevel = p_ptr->mdepth;
   sublevel = p_ptr->sdepth;
}

/*
 * Actually play a game
 *
 * If the "new_game" parameter is true, then, after loading the
 * savefile, we will commit suicide, if necessary, to allow the
 * player to start a new game.
 */
void play_game(bool new_game)
{
   /* Init the RNG */
   /* this has to be here, since main-win.c and perhaps */
   /* others call play_game directly.                   */
   if (TRUE)
   {
      u32b seed;

      /* Basic seed */
      seed = (u32b)(time(NULL));

#ifdef SET_UID

      /* Mutate the seed on Unix machines */
      seed = ((seed >> 3) * ((u32b)getpid() << 1));

#endif

      /* Seed the "complex" RNG */
      rand_seed_init(seed);
   }
dlog(DEBUGFLOW,"dungeon.c: play_game step 0, remove_levelfiles %d\n", remove_levelfiles);

   /* Initialize the arrays */
   /* first the default material/flavor arrays */
   object_material_init();
   /* then possibly override them with user-defined settings */
   init_some_arrays();

   /* Hack -- Character is "icky" */
   character_icky = TRUE;

   /* Hack -- turn off the cursor */
   (void)Term_set_cursor(0);
dlog(DEBUGFLOW,"dungeon.c: play_game step 0a\n");

   /* for whatever reason this is necessary in pc Windows */
#ifdef WINDOWS
   process_player_name(TRUE);
#endif

dlog(DEBUGFLOW,"dungeon.c: play_game step 0b\n");

   /* Attempt to load */
   if (!new_game && !load_player())
   {
      /* Oops */
      msg_print("Error during reading of savefile. Starting new game!");
      character_loaded = FALSE;
   }
dlog(DEBUGFLOW,"dungeon.c: play_game step 0d\n");

   if (debuglevel != 0L)
   {
      msg_format("WARNING: the game is slow; debugging is on! (0x%04lx).", debuglevel);
   }

   /* Nothing loaded */
   if (!character_loaded)
   {
      /* Make new player */
      new_game = TRUE;

      /* Create a new dungeon */
      character_dungeon = FALSE;
   }
dlog(DEBUGFLOW,"dungeon.c: play_game step 0e\n");

   /* Process old character */
   if (!new_game)
   {
      /* Process the player name */
      process_player_name(FALSE);
      add_msg("Savefile loaded.");
   }
dlog(DEBUGFLOW,"dungeon.c: play_game step 0f\n");

   if (read_options)
   {
      read_text_options_file(ANGBAND_OPTION_FILE);
      read_options = FALSE;
   }
dlog(DEBUGFLOW,"dungeon.c: play_game step 1, new_game %d remove_levelfiles %d\n",
               new_game, remove_levelfiles);

   /* Roll new character */
   if (new_game)
   {
      /* jk - new players don't know traps. */
      s16b i;
      for (i=1;i<t_number;i++) t_info[i].known=0;

      /* Ignore the dungeon */
      character_dungeon = FALSE;

      /* Start in town */
      p_ptr->mdepth = 0;
      p_ptr->sdepth = 0;
      baselevel = 0;
      sublevel = 0;

      /* Roll up a new character */
dlog(DEBUGFLOW,"dungeon.c: play_game: about to call player_birth\n");
      player_birth();
      add_msg("A new player is born.");

      /* Hack -- enter the world */
      turn = 1;
   }
dlog(DEBUGFLOW,"dungeon.c: play_game step 2\n");
   /* we should now be able to write messages to disk, as */
   /* the name of the file is known */

   /* Flash a message */
   prt("Please wait...", 0, MESSAGE_ROW);

   /* Flush the message */
   Term_fresh();

   /* Hack -- Enter wizard mode */
   if (arg_wizard && enter_wizard_mode()) wizard = TRUE;

   /* Flavor the objects */
   flavor_init();

   /* Reset the visual mappings, by reading graf.prf */
   reset_visuals();

   /* Window stuff */
   p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER_0 | PW_PLAYER_1);

   /* Window stuff */
   p_ptr->window |= (PW_MONSTER);

   /* Window stuff */
   window_stuff();

   /* Process some user pref files */
   process_last_prf();

   /* special initialisations for races/classes */
   races_init();

dlog(DEBUGFLOW,"dungeon.c: play_game step 4, remove_levelfiles %d\n", remove_levelfiles);

   /* Set or clear "rogue_like_commands" if requested */
   if (arg_force_original) rogue_like_commands = FALSE;
   if (arg_force_roguelike) rogue_like_commands = TRUE;

   /* React to changes */
   Term_xtra(TERM_XTRA_REACT, 0);
   /* Make a level if necessary */
   if (!character_dungeon)
   {
      get_new_level(-1, 0);
   }
   /* Character is now "complete" */
   character_generated = TRUE;

   p_ptr->new_mdepth = p_ptr->mdepth; /* for the moment, we don't go to another level */
   p_ptr->new_sdepth = p_ptr->sdepth; /* for the moment, we don't go to another level */
   /* Hack -- Character is no longer "icky" */
   character_icky = FALSE;

   /* Start game */
   alive = TRUE;
   suicide = FALSE;

   /* Hack -- Enforce "delayed death" */
   if (p_ptr->chp < 0) death = TRUE;

   /* Loop till dead */
   while (TRUE)
   {
       s16b old_mlevel = 0, old_slevel = 0;
       bool cheated = FALSE;

       /* Process the level */
dlog(DEBUGFLOW,"dungeon.c: play_game: about to call handle_dungeon, p_ptr->mdepth, sdepth %d,%d baselevel,sublevel %d,%d\n",
               p_ptr->mdepth, p_ptr->sdepth,baselevel, sublevel);
       handle_dungeon();

dlog(DEBUGFLOW,"dungeon.c: play_game: handle_dungeon finished alive %d death %d\n",
               alive, death);
       /* Accidental Death */
       if (alive && death)
       {
          cheated = cheat_death();
       }
/* jk - write this level if we're not dead and not just resurrected */
/* if we're alive, we are just leaving this level */
       if (!death && !cheated)
       {
          level_info[p_ptr->mdepth].last_visit=turn;
          if ( (save_levels) || (p_ptr->mdepth == 0) )
          {
             wr_level();
dlog(DEBUGFLOW,"dungeon.c: play_game: saved old level %d\n", p_ptr->mdepth);
             level_info[p_ptr->mdepth].saved=TRUE;
          }

          old_mlevel = p_ptr->mdepth;
          old_slevel = p_ptr->sdepth;

          p_ptr->mdepth = p_ptr->new_mdepth;
          p_ptr->sdepth = p_ptr->new_sdepth;
          baselevel = p_ptr->mdepth;
          sublevel = p_ptr->sdepth;
          if ((level_info[p_ptr->mdepth].first_visit == 0) && (p_ptr->mdepth > 0))
          {
             level_info[p_ptr->mdepth].first_visit=turn;
          }
       }

       /* Handle "quit and save" */
       if (!alive && !death) break;

       /* XXX XXX XXX */
       msg_print(NULL);

       /* Handle "death" */
       if (death) break;

/* jk - is this an already saved level? */
/* if not, create it */
dlog(DEBUGFLOW,"dungeon.c: play_game: calling get_new_level, md %d sd %d basel %d subl %d\n",
               p_ptr->mdepth, p_ptr->sdepth,baselevel, sublevel);

       get_new_level(old_mlevel, old_slevel);
   }

   /* Close stuff */
   close_game(FALSE);

   /* Quit */
   quit(NULL);
}


