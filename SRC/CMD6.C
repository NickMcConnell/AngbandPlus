/* File: cmd6.c */

/* Purpose: process player commands */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"


/*
 * Hack -- make sure we have a good "ANSI" definition for "CTRL()"
 */
#undef CTRL
#define CTRL(C) ((C)&037)









/*
 * Go up one level                                      -RAK-   
 */
void do_cmd_go_up(void)
{
    cave_type *c_ptr;
    inven_type *i_ptr;

    c_ptr = &cave[py][px];
    i_ptr = &i_list[c_ptr->i_idx];

    /* Verify stairs */
    if (i_ptr->tval != TV_UP_STAIR) {
	msg_print("I see no up staircase here.");
	energy_use = 0;
	return;
    }

    /* Success */
    msg_print("You enter a maze of up staircases. ");

    /* Go up the stairs */
    dun_level--;
    new_level_flag = TRUE;

    /* Create a way back */
    create_down_stair = TRUE;
}

/* 
 *special powers, mostly dragon breath %%%%
 */
void do_cmd_powers(void)
{
    char c,buf[80];
    int i,j,dir,k=0;

    buf[0]='\0';

    if (p_ptr->prace==PIXIE) k=3;
    if (p_ptr->prace>=MIN_DRAGON && p_ptr->lev>4 && p_ptr->lev<25) k=1;

    if (p_ptr->prace>=MIN_DRAGON && p_ptr->lev>24) {
      sprintf(buf,"(1) bolt (2) ball");
      prt(buf,0,0);
      c=inkey();
      if (c!='1' && c!='2') {
	energy_use = 0;
	return(0);
      }
      k = c-'0';
    }

    switch (k) {
      case 0:
       if (p_ptr->prace<MIN_DRAGON) {
	 msg_print("You have no special powers."); 
       } else {
	 msg_print("You don't have a breath weapon yet.");
       }
       return(0);
       break;

      case 1:
       if (!get_dir(NULL,&dir)) { energy_use=0; return(0); }
       if (p_ptr->confused>0) {
	 msg_print("You are confused.");
	 do {dir = randint(9);} while (dir == 5);
       }
       switch(p_ptr->prace) {
	 case CRYSTALDRAG:
	   fire_bolt (GF_SHARDS, dir, py, px, p_ptr->chp/5+1);
	   break;
	 case COPPERDRAG:
	   line_spell (GF_DISENCHANT, dir, py, px, p_ptr->chp/8+1);
	   break;
	 case BRONZEDRAG:
	   fire_bolt (GF_CONFUSION, dir, py, px, p_ptr->chp/8+1);
	   break;
	 case GOLDDRAG:
	   fire_bolt (GF_SOUND, dir, py, px, p_ptr->chp/8+1);
	   break;
	 case PSEUDODRAG:
	   line_spell (GF_LITE, dir, py, px, p_ptr->chp/8+1);
	   break;
	 default:
	   msg_print ("Huh????");
	   break;
       }
       take_hit (p_ptr->mhp/15,"breathing.");
       break;

      case 2:
       if (!get_dir(NULL,&dir)) { energy_use=0; return(0); }
       if (p_ptr->confused>0) {
	 msg_print("You are confused.");
	 do {dir = randint(9);} while (dir == 5);
       }
       switch (p_ptr->prace) {
	 case CRYSTALDRAG:
	   fire_ball (GF_SHARDS, dir, py, px, p_ptr->chp/3+1, p_ptr->lev>40?3:2 );
	   break;
	 case COPPERDRAG:
	   fire_ball (GF_DISENCHANT, dir, py, px, p_ptr->chp/3+1, p_ptr->lev>40?3:2 );
	   break;
	 case BRONZEDRAG:
	   fire_ball (GF_CONFUSION, dir, py, px, p_ptr->chp/3+1, p_ptr->lev>40?3:2 );
	   break;
	 case GOLDDRAG:
	   fire_ball (GF_SOUND, dir, py, px, p_ptr->chp/3+1, p_ptr->lev>40?3:2 );
	   break;
	 case PSEUDODRAG:
	   fire_ball (GF_DARK, dir, py, px, p_ptr->chp/3+1, p_ptr->lev>40?3:2 );
	   break;
	 default:
	   msg_print ("Huh????");
	   break;
       }
       take_hit (p_ptr->mhp/6, "breathing.");
       break;

      case 3:
       if (!get_dir(NULL,&dir)) { energy_use=0; return(0); }
       if (p_ptr->confused>0) {
	 msg_print("You are confused.");
	 do {dir = randint(9);} while (dir == 5);
       }
       fire_bolt (GF_CONFUSION, dir, py, px, 1);
       p_ptr->cmana-=10;
       if (p_ptr->cmana<=0) {
	 p_ptr->cmana=0;
	 take_hit (10,"running out of pixie dust.");
       }
       break;
      default:
       msg_print ("Huh?");
       break;
    }
}



/*
 * Go down one level
 */
void do_cmd_go_down(void)
{
    cave_type *c_ptr;
    inven_type *i_ptr;

    c_ptr = &cave[py][px];
    i_ptr = &i_list[c_ptr->i_idx];

    if (i_ptr->tval != TV_DOWN_STAIR) {
	msg_print("I see no down staircase here.");
	energy_use = 0;
	return;
    }

    /* Success */
    msg_print("You enter a maze of down staircases. ");

    /* Go down */
    dun_level++;
    new_level_flag = TRUE;

    /* Create a way back */
    create_up_stair = TRUE;
}


/*
 * Hack -- commit suicide
 */
void do_cmd_suicide(void)
{
    /* Free turn */
    energy_use = 0;

    /* Flush input */
    flush();

    /* Verify */
    if (total_winner) {
	if (!get_check("Do you want to retire?")) return;
    }
    else {
	int i;
	if (!get_check("Do you really want to quit?")) return;
	flush();
	prt("Please verify QUITTING by typing the '@' sign: ", 0, 0);
	i = inkey();
	prt("", 0, 0);
	if (i != '@') return;
    }

    death = TRUE;
    
    (void)strcpy(died_from, "Quitting");
}


/*
 * Hack -- redraw the screen
 */
void do_cmd_redraw(void)
{
    /* Free command */
    energy_use = 0;

    /* Hack */
    if (p_ptr->image) {
	msg_print("You cannot be sure what is real and what is not!");
    }

    /* Update stuff */
    p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);
    
    /* Redraw everything */
    p_ptr->redraw |= (PR_CAVE);

    /* Hack -- Redraw the recall and choice windows */
    p_ptr->redraw |= (PR_RECALL | PR_CHOICE);
    
    /* Handle stuff */
    handle_stuff();
    
    /* Hack -- Redraw physically */
    Term_redraw();

    /* Hack -- Flush the output */
    Term_fresh();
}


/*
 * Hack -- change name
 */
void do_cmd_change_name(void)
{
    energy_use = 0;

    save_screen();
    change_name();
    restore_screen();
}


/*
 * Hack -- toggle search mode
 */
void do_cmd_toggle_search(void)
{
    energy_use = 0;

    if (p_ptr->searching) {
	search_off();
    }
    else {
	search_on();
    }
}



/*
 * Refill the players lamp (from the pack or floor)
 */
static void do_cmd_refill_lamp(void)
{
    int item;
    bool floor;
    inven_type *i_ptr;
    inven_type *j_ptr;


    /* Assume this will be free */
    energy_use = 0;

    /* Access the lantern */
    j_ptr = &inventory[INVEN_LITE];


    /* Access the item on the floor */
    i_ptr = &i_list[cave[py][px].i_idx];

    /* Restrict the choices to flasks */
    item_tester_tval = TV_FLASK;

    /* Check for use of the floor */
    floor = item_tester_okay(i_ptr);

    /* Get a flask to refuel with */
    if (!get_item(&item, "Refill with which flask? ", 0, inven_ctr - 1, floor)) {
	if (item == -2) msg_print("You have no flasks of oil.");
	return;
    }
    

    /* Access the item */
    if (item >= 0) i_ptr = &inventory[item];

    /* XXX XXX Indent me */
    if (TRUE) {
    
	/* Take a turn */       
	energy_use = 100;

	/* Refuel */
	j_ptr->pval += i_ptr->pval;

	/* Comment */
	if (j_ptr->pval < FUEL_LAMP / 2) {
	    msg_print("Your lamp is less than half full.");
	}
	else if (j_ptr->pval < FUEL_LAMP / 2 + FUEL_LAMP / 16) {
	    msg_print("Your lamp is about half full.");
	}
	else if (j_ptr->pval < FUEL_LAMP) {
	    msg_print("Your lamp is more than half full.");
	}
	else {
	    j_ptr->pval = FUEL_LAMP;
	    msg_print("Your lamp overflows, spilling oil on the ground.");
	    msg_print("Your lamp is full.");
	}

	/* Decrease the item (from the pack) */
	if (item >= 0) {
	    inven_item_increase(item, -1);
	    inven_item_optimize(item);
	}

	/* Decrease the item (from the floor) */
	else {
	    floor_item_increase(py, px, -1);
	    floor_item_optimize(py, px);
	}
    }
}




/*
 * Refuel the players torch (from the pack or floor)
 */
static void do_cmd_refill_torch(void)
{
    int item;
    bool floor;
    inven_type *i_ptr;
    inven_type *j_ptr;


    /* Assume this will be free */
    energy_use = 0;

    /* Access the primary torch */
    j_ptr = &inventory[INVEN_LITE];


    /* Access the item on the floor */
    i_ptr = &i_list[cave[py][px].i_idx];

    /* Restrict the choices */
    item_tester_tval = TV_LITE;
    item_tester_sval = SV_LITE_TORCH;

    /* Check for use of the floor */
    floor = item_tester_okay(i_ptr);

    /* Get a torch to refuel with */
    if (!get_item(&item, "Refuel with which torch? ", 0, inven_ctr - 1, floor)) {
	if (item == -2) msg_print("You have no extra torches.");
	return;
    }
    
    /* Cancel "auto-see" */
    command_see = FALSE;

    /* XXX XXX Indent me */
    if (TRUE) {
    
	/* Access the item */
	if (item >= 0) i_ptr = &inventory[item];

	/* Take a turn */       
	energy_use = 100;

	/* Refuel */
	j_ptr->pval += i_ptr->pval;

	/* Message */
	msg_print("You combine the torches.");

	/* Over-fuel message */
	if (j_ptr->pval > FUEL_TORCH) {
	    j_ptr->pval = FUEL_TORCH;
	    msg_print("Your torch fully fueled.");
	}

	/* Refuel message */
	else {
	    msg_print("Your torch glows more brightly.");
	}

	/* Decrease the item (from the pack) */
	if (item >= 0) {
	    inven_item_increase(item, -1);
	    inven_item_optimize(item);
	}

	/* Decrease the item (from the floor) */
	else {
	    floor_item_increase(py, px, -1);
	    floor_item_optimize(py, px);
	}
    }

    /* Cancel "auto-see" */
    command_see = FALSE;
}




/*
 * Refill the players lamp, or restock his torches
 */
void do_cmd_refill(void)
{
    inven_type *i_ptr;

    /* Assume a free turn */
    energy_use = 0;

    /* Get the light */
    i_ptr = &inventory[INVEN_LITE];

    /* It is nothing */
    if (i_ptr->tval != TV_LITE) {
	message("You are not wielding a light.", 0);
    }

    /* It's a lamp */
    else if (i_ptr->sval == SV_LITE_LANTERN) {
	do_cmd_refill_lamp();
    }

    /* It's a torch */
    else if (i_ptr->sval == SV_LITE_TORCH) {
	do_cmd_refill_torch();
    }

    /* No torch to refill */
    else {
	message("Your light cannot be refilled.", 0);
    }
}



/*
 * Support code for the "CTRL('P')" recall command
 *
 * If "command_arg" is set, recall that many messages.
 * Otherwise, recall until there are no more messages.
 *
 * The screen format uses line 0 for prompts, skips line 1,
 * uses line 2 and 23 for "[continued]" messages, and uses
 * lines 3 to 22 for the display of old messages.
 */
void do_cmd_messages(void)
{
    int i = 0, j = 0, n = 0, k = 0;

    cptr pmt1 = "[... older messages continued above, use ^P to scroll ...]";
    cptr pmt2 = "[... newer messages continued below, use ^N to scroll ...]";

    /* Free move */
    energy_use = 0;

    /* Assume "full recall" */
    n = message_num();

    /* Hack -- no messages */
    if (n <= 0) {
	prt("There are no messages to recall.", 0, 0);
	return;
    }

    /* Hack -- allow "quick recall" of a single message */
    if ((!command_arg) && (command_old != CTRL('P'))) n = 1;

    /* Hack -- quick recall of simple messages */
    if (n <= 1) {
	put_str(">", 0, 0);
	prt(message_str(0), 0, 1);
	return;
    }

    /* Save the screen */
    save_screen();

    /* Enter "icky" mode */
    character_icky = TRUE;
    
    /* Hack -- force "clear" */
    k = ' ';

    /* Process requests until done */
    while (1) {

	/* Only redraw if needed */
	if (k) {

	    /* Clear the screen */
	    clear_screen();

	    /* Dump the current batch of messages */
	    for (j = 0; j < 20 && (i+j < n); j++) {

		/* Dump the messages, bottom to top */
		put_str(message_str(i+j), 22-j, 0);
	    }

	    /* Indicate extra messages */
	    if (i + 20 < n) prt(pmt1, 2, 0);

	    /* Indicate extra messages */
	    if (i > 0) prt(pmt2, 23, 0);
	}

	/* Display a simple prompt */
	prt("Message Recall [press ESCAPE to exit] ", 0, 0);

	/* Get a command */
	k = inkey();

	/* Exit on Escape */
	if (k == ESCAPE) break;

	/* Save the old index */
	j = i;

	/* Recall more older messages */
	if ((k == 'p') || (k == CTRL('P'))) {
	    if (i+20 < n) i += 20;
	}

	/* Recall more newer messages */
	if ((k == 'n') || (k == CTRL('N'))) {
	    if (i >= 20) i -= 20;
	}

	/* Error of some kind, do not redraw */
	if (i == j) {
	    k = 0;
	    bell();
	}
    }

    /* Leave "icky" mode */
    character_icky = FALSE;
    
    /* Restore the screen */
    restore_screen();
}


/*
 * Target command
 */
void do_cmd_target(void)
{
    /* Free move */
    energy_use = 0;

#ifdef ALLOW_TARGET

    /* Be sure we can see */
    if (p_ptr->blind) {
	msg_print("You can't see anything to target!");
    }
    else if (!target_set()) {
	msg_print("Target Aborted");
    }
    else {
	msg_print("Target Selected");
    }

#else

    msg_print("Target code not compiled in this version.");

#endif

}




/*
 * A simple structure to hold some options
 */
typedef struct _opt_desc {
    cptr        o_prompt;
    bool        *o_var;
} opt_desc;


/*
 * General User-Interface Options
 */
static opt_desc options_interface[] = {

    { "Rogue-like commands",                            &rogue_like_commands },
    { "Quick messages",                                 &quick_messages },
    { "Prompt for various information",                 &other_query_flag },
    { "Prompt before picking things up",                &carry_query_flag },
    { "Use old target by default",                      &use_old_target },
    { "Pick things up by default",                      &always_pickup },
    { "Accept all throw commands",                      &always_throw },
    { "Repeat obvious commands",                        &always_repeat },

    { "Use new screen layout",                          &new_screen_layout },
    { "Display Equippy chars",                          &equippy_chars },
    { "Show dungeon level in feet",                     &depth_in_feet },
    { "Notice mineral seams",                           &notice_seams },

    { "Use color (slow)",                               &use_color },

    { "Hilite the player",                              &hilite_player },

    { "Draw Torch-Lite in yellow (slow)",               &view_yellow_lite },
    { "Draw Viewable Lite brightly (v.slow)",           &view_bright_lite },

    { "Ring bell on error",                             &ring_bell },

    { "Compress savefiles",                             &compress_savefile },

    { NULL,                                             NULL }
};


/*
 * Disturbance Options -- for running/resting
 */
static opt_desc options_disturb[] = {

    { "Cut known corners",                              &find_cut },
    { "Examine potential corners",                      &find_examine },
    { "Print self during run (slow)",                   &find_prself },
    { "Stop when map sector changes",                   &find_bound },
    { "Run through open doors",                         &find_ignore_doors },
    { "Run past stairs",                                &find_ignore_stairs },

    { "Monster moving nearby disturbs me",              &disturb_near },
    { "Monster moving anywhere disturbs me",            &disturb_move },
    { "Monster appearance disturbs me",                 &disturb_enter },
    { "Monster disappearance disturbs me",              &disturb_leave },

    { "Flush input before normal commands",             &flush_command },
    { "Flush input whenever disturbed",                 &flush_disturb },
    { "Flush input on various failures",                &flush_failure },

    { "Flush output before all commands",               &fresh_before },
    { "Flush output after all commands",                &fresh_after },
    { "Flush output while running (slow)",              &fresh_find },

    { NULL,                                             NULL }
};


/*
 * Inventory Options -- these slightly affect game-play
 */
static opt_desc options_inventory[] = {

    { "Use Recall window (if available)",               &use_recall_win },
    { "Use Choice window (if available)",               &use_choice_win },

    { "Recall monster descriptions",                    &recall_show_desc },
    { "Recall monster kill counts",                     &recall_show_kill },

    { "Show spell choices in choice window",            &choice_spells },
    { "Show inven-weights in choice window",            &choice_inven_wgt },
    { "Show equip-weights in choice window",            &choice_equip_wgt },
    { "Show equip-labels in choice window",             &choice_equip_xtra },

    { "Show weights in inventory list",                 &show_inven_weight },
    { "Show weights in equipment list",                 &show_equip_weight },
    { "Show weights in stores",                         &show_store_weight },
    { "Plain object descriptions",                      &plain_descriptions },

    { "Allow weapons and armor to stack",               &stack_allow_items },
    { "Allow wands/staffs/rods to stack",               &stack_allow_wands },
    { "Over-ride inscriptions when stacking",           &stack_force_notes },
    { "Over-ride discounts when stacking",              &stack_force_costs },

    { "Disable haggling in stores",                     &no_haggle_flag },
    { "Shuffle store owners",                           &shuffle_owners },

    { NULL,                                             NULL}
};


/*
 * Gameplay Options -- these actually affect game-play
 */
static opt_desc options_gameplay[] = {

    { "Map remembers all illuminated walls",            &view_wall_memory },
    { "Map remembers all important stuff",              &view_xtra_memory },
    { "Map remembers all perma-lit grids",              &view_perma_grids },
    { "Map remembers all torch-lit grids",              &view_torch_grids },

    { "Reduce view-radius when running",                &view_reduce_view },
    { "Reduce lite-radius when running",                &view_reduce_lite },

    { "Show extra spell info",                          &show_spell_info },
    { "Show monster health bar (slow)",                 &show_health_bar },

    { "Create characters in 'maximize' mode",           &begin_maximize },
    { "Create characters in 'preserve' mode",           &begin_preserve },

    { "Generate dungeons with aligned rooms",           &dungeon_align },
    { "Generate dungeons with connected stairs",        &dungeon_stair },

#ifdef MONSTER_FLOW
    { "Monsters chase current location (v.slow)",       &flow_by_sound },
    { "Monsters chase recent locations (v.slow)",       &flow_by_smell },
#endif

#ifdef WDT_TRACK_OPTIONS
    { "Monsters follow the player (beta)",              &track_follow },
    { "Monsters target the player (beta)",              &track_target },
#endif

#ifdef DRS_SMART_OPTIONS
    { "Monsters learn from their mistakes",             &smart_learn },
    { "Monsters exploit players weaknesses",            &smart_cheat },
#endif

    { NULL,                                             NULL}
};


/*
 * Cheating Options -- go ahead and cheat
 */
static opt_desc options_cheating[] = {

    { "Peek into object creation",              &cheat_peek },
    { "Peek into monster creation",             &cheat_hear },
    { "Peek into dungeon creation",             &cheat_room },
    { "Peek into something else",               &cheat_xtra },

    { "Know complete monster info",             &cheat_know },
    { "Allow player to avoid death",            &cheat_live },

    { NULL,                                     NULL}
};


/*
 * Set or unset various boolean options.
 */
static void do_cmd_options_aux(opt_desc *options, cptr info)
{
    int         i, max, ch;

    char        pmt[80];
    char        dat[80];


    /* Clear the screen */
    clear_screen();

    /* Prompt */
    sprintf(pmt, "%s (RET to advance, y/n to set, ESC to accept) ", info);
    prt(pmt, 0, 0);

    /* Prepare the screen, Count the options. */
    for (max = 0; options[max].o_prompt; max++) {
	sprintf(dat, "%-48s: %s ", options[max].o_prompt,
		(*options[max].o_var ? "yes" : "no "));
	prt(dat, max + 2, 0);
    }

    /* Start at the first option */
    i = 0;

    /* Interact with the player */
    for (;;) {
	move_cursor(i + 2, 50);
	ch = inkey();
	switch (ch) {
	  case ESCAPE:  
	    return;
	  case '-':
	  case '8':
	    i = (max + i - 1) % max;
	    break;
	  case ' ':
	  case '\n':
	  case '\r':
	  case '2':
	    i = (i + 1) % max;
	    break;
	  case 'y':
	  case 'Y':
	  case '6':
	    put_str("yes ", i + 2, 50);
	    *options[i].o_var = TRUE;
	    i = (i + 1) % max;
	    break;
	  case 'n':
	  case 'N':
	  case '4':
	    put_str("no  ", i + 2, 50);
	    *options[i].o_var = FALSE;
	    i = (i + 1) % max;
	    break;
	  default:
	    bell();
	    break;
	}
    }
}


/*
 * Set or unset various options.  Redraw screen when done.
 */
void do_cmd_options(void)
{
    int i;


    /* Save the screen */
    save_screen();

    /* Enter "icky" mode */
    character_icky = TRUE;
    

    /* Interact */
    while (1) {

	clear_screen();

	/* Give some choices */
	prt("(1) User Interface Options", 2, 5);
	prt("(2) Disturbance Options", 3, 5);
	prt("(3) Inventory Options", 4, 5);
	prt("(4) Game-Play Options", 5, 5);
	prt("(5) Base Delay Speed", 6, 5);
	prt("(6) Hitpoint Warning", 7, 5);

	/* Cheating */
	if (can_be_wizard) prt("(C) Cheating Options", 8, 5);

	/* Ask for a choice */
	prt("Angband Options (1-6 or ESC to exit) ", 0, 0);
	i = inkey();

	/* Exit */
	if (i == ESCAPE) break;

	/* General Options */
	if (i == '1') {

	    /* Process the general options */
	    do_cmd_options_aux(options_interface, "User Interface Options");
	}

	/* Disturbance Options */
	else if (i == '2') {

	    /* Process the running options */
	    do_cmd_options_aux(options_disturb, "Disturbance Options");
	}

	/* Inventory Options */
	else if (i == '3') {

	    /* Process the running options */
	    do_cmd_options_aux(options_inventory, "Inventory Options");
	}

	/* Gameplay Options */
	else if (i == '4') {

	    /* Process the game-play options */
	    do_cmd_options_aux(options_gameplay, "Game-Play Options");
	}

	/* Hack -- Delay Speed */
	else if (i == '5') {

	    clear_screen();

	    /* Get a new value */
	    while (1) {
		char buf[128];
		sprintf(buf, "Current delay speed: %d milliseconds", delay_spd);
		prt(buf, 5, 5);
		prt("Delay Speed (0-9 or ESC to accept) ", 0, 0);
		i = inkey();
		if (i == ESCAPE) break;
		if (isdigit(i)) delay_spd = (i - '0');
		else bell();
	    }
	}

	/* Hack -- hitpoint warning factor */
	else if (i == '6') {

	    clear_screen();

	    /* Get a new value */
	    while (1) {
		char buf[128];
		sprintf(buf, "Current hitpoint warning: %d0%%", hitpoint_warn);
		prt(buf, 5, 5);
		prt("Hitpoint Warning (0-9 or ESC to accept) ", 0, 0);
		i = inkey();
		if (i == ESCAPE) break;
		if (isdigit(i)) hitpoint_warn = (i - '0');
		else bell();
	    }
	}

	/* Cheating Options */
	else if ((i == 'C') && can_be_wizard) {

	    /* Process the cheating options */
	    do_cmd_options_aux(options_cheating, "Cheating Options");

	    /* Hack -- note use of "cheat" options */
	    if (cheat_peek) noscore |= 0x0100;
	    if (cheat_hear) noscore |= 0x0200;
	    if (cheat_room) noscore |= 0x0400;
	    if (cheat_xtra) noscore |= 0x0800;
	    if (cheat_know) noscore |= 0x1000;
	    if (cheat_live) noscore |= 0x2000;
	}

	/* Unknown option */
	else {
	    bell();
	}
    }


    /* Leave "icky" mode */
    character_icky = FALSE;
    
    /* Restore the screen */
    restore_screen();


    /* Verify the keymap */
    keymap_init();
    
    
    /* Mega-Hack -- React */
    Term_xtra(TERM_XTRA_REACT, 0);


    /* XXX XXX Mega-Hack (see "birth.c") */
    if (!character_generated) return;


    /* Free turn */
    energy_use = 0;

    /* Combine the pack */
    combine_pack();

    /* Update stuff */
    p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);
    
    /* Redraw the recall and choice windows */
    p_ptr->redraw |= (PR_RECALL | PR_CHOICE);
    
    /* Redraw stuff */
    p_ptr->redraw |= (PR_CAVE);
}



/*
 * Append monster attr/char definitions to the given file
 */
static void pref_dump_race(cptr fname)
{
    int i;
    FILE *fff;

#ifdef MACINTOSH
    _ftype = 'TEXT';
#endif

    /* Append to the file */
    fff = my_tfopen(fname, "a");

    /* Failure */
    if (!fff) return;

    /* Start dumping */
    fprintf(fff, "\n\n# Monster attr/char definitions\n\n");

    /* Dump them (including ghost) */
    for (i = 1; i < MAX_R_IDX; i++) {

	monster_lore *l_ptr = &l_list[i];

	/* Dump the monster attr/char info */
	fprintf(fff, "R:%d:%d/%d\n", i, l_ptr->l_attr, (byte)(l_ptr->l_char));
    }

    /* Start dumping */
    fprintf(fff, "\n\n\n\n");

    /* Close */
    fclose(fff);
}


/*
 * Append object attr/char definitions to the given file
 */
static void pref_dump_kind(cptr fname)
{
    int i;
    FILE *fff;

#ifdef MACINTOSH
    _ftype = 'TEXT';
#endif

    /* Append to the file */
    fff = my_tfopen(fname, "a");

    /* Failure */
    if (!fff) return;

    /* Start dumping */
    fprintf(fff, "\n\n# Object attr/char definitions\n\n");

    /* Dump them */
    for (i = 0; i < MAX_K_IDX; i++) {

	/* Dump the monster attr/char info */
	fprintf(fff, "K:%d:%d/%d\n", i, x_list[i].x_attr, (byte)(x_list[i].x_char));
    }

    /* Start dumping */
    fprintf(fff, "\n\n\n\n");

    /* Close */
    fclose(fff);
}


/*
 * Interact with the "pref" files
 */
void do_cmd_prefs(void)
{
    int i;

    char buf[1024];


    /* Drop priv's */
    safe_setuid_drop();

    /* Enter "icky" mode */
    character_icky = TRUE;
    
    /* Interact until done */
    while (1) {

	/* Clear the screen */
	clear_screen();

	/* Give some choices */
	prt("(0) Reset visuals", 2, 5);
	prt("(1) Load a 'pref' file", 3, 5);
	prt("(2) Append macros to a 'pref' file", 4, 5);
	prt("(3) Append something -- not ready yet", 5, 5);
	prt("(4) Append monster attr/chars to a 'pref' file", 6, 5);
	prt("(5) Append object attr/chars to a 'pref' file", 7, 5);
	prt("(6) Change monster attr/chars", 8, 5);
	prt("(7) Change object attr/chars", 9, 5);

#if 0
	prt("(8) Change tval_to_attr[] -- not ready yet", 10, 5);
	prt("(9) Change tval_to_char[] -- not ready yet", 11, 5);
#endif

	/* Ask for a choice */
	prt("Angband Preferences (0-7 or ESC to exit) ", 0, 0);
	i = inkey();
	if (i == ESCAPE) break;

	/* Reset visuals */
	if (i == '0') {
	
	    /* Reset */
	    reset_visuals();
	    
	    /* Message */
	    msg_print("Reset visual attr/char tables.");
	}
	
	/* Load a 'pref' file */
	else if (i == '1') {

	    /* Clear */
	    clear_screen();

	    /* Info */
	    prt("Load an existing 'pref' file.", 0, 0);

	    /* Get a filename, handle ESCAPE */
	    prt("File: ", 5, 0);
	    strcpy(buf, "pref.prf");
	    if (!askfor_aux(buf, 70)) continue;

	    /* Process the given filename */
	    if (process_pref_file(buf)) msg_print("Error...");
	}

	/* Dump macros */
	else if (i == '2') {

	    /* Clear */
	    clear_screen();

	    /* Info */
	    prt("Appends all current macros to a file.", 0, 0);

	    /* Get a filename, dump the macros to it */
	    prt("File: ", 5, 0);
	    strcpy(buf, "macro.prf");
	    if (!askfor_aux(buf, 70)) continue;

	    /* Dump the macros */
	    macro_dump(buf);
	}

	/* Dump monster attr/chars */
	else if (i == '4') {

	    /* Clear */
	    clear_screen();

	    /* Info */
	    prt("Append all monster attr/char data to a file.", 0, 0);

	    /* Get a filename, dump to it */
	    prt("File: ", 5, 0);
	    strcpy(buf, "race.prf");
	    if (!askfor_aux(buf, 70)) continue;

	    /* Hack -- open the file */
	    pref_dump_race(buf);
	}

	/* Dump object attr/chars */
	else if (i == '5') {

	    /* Clear */
	    clear_screen();

	    /* Info */
	    prt("Append all object attr/char data to a file.", 0, 0);

	    /* Get a filename, dump to it */
	    prt("File: ", 5, 0);
	    strcpy(buf, "kind.prf");
	    if (!askfor_aux(buf, 70)) continue;

	    /* Hack -- open the file */
	    pref_dump_kind(buf);
	}

	/* Hack -- monsters */
	else if (i == '6') {

	    /* Hack -- start on "nobody" */
	    static int r_idx = 0;

	    /* Clear the screen */
	    clear_screen();

	    /* Hack -- query until done */
	    while (1) {

		int r = r_idx;

		monster_race *r_ptr = &r_list[r];
		monster_lore *l_ptr = &l_list[r];

		int da = (byte)(r_ptr->r_attr);
		int dc = (byte)(r_ptr->r_char);
		int ca = (byte)(l_ptr->l_attr);
		int cc = (byte)(l_ptr->l_char);

		/* Label the object */
		Term_putstr(5, 10, -1, TERM_WHITE,
			    format("Monster = %d, Name = %-40.40s",
				   r, r_ptr->name));

		/* Label the Default values */
		Term_putstr(10, 12, -1, TERM_WHITE,
			    format("Default attr/char = %3u / %3u", da, dc));
		Term_putstr(40, 12, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 12, da, dc);

		/* Label the Current values */
		Term_putstr(10, 14, -1, TERM_WHITE,
			    format("Current attr/char = %3u / %3u", ca, cc));
		Term_putstr(40, 14, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 14, ca, cc);

		/* Directions */
		Term_putstr(5, 2, -1, TERM_WHITE, "n/N = change index");
		Term_putstr(5, 3, -1, TERM_WHITE, "a/A = change attr");
		Term_putstr(5, 4, -1, TERM_WHITE, "c/C = change char");

		/* Prompt */
		Term_putstr(0, 0, -1, TERM_WHITE,
			    "Object attr/char (n/N/a/A/c/C): ");

		/* Get a command */
		i = inkey();

		/* All done */
		if (i == ESCAPE) break;

		/* Analyze */
		if (i == 'n') r_idx = (r_idx + MAX_R_IDX + 1) % MAX_R_IDX;
		if (i == 'N') r_idx = (r_idx + MAX_R_IDX - 1) % MAX_R_IDX;
		if (i == 'a') l_ptr->l_attr = (byte)(ca + 1);
		if (i == 'A') l_ptr->l_attr = (byte)(ca - 1);
		if (i == 'c') l_ptr->l_char = (byte)(cc + 1);
		if (i == 'C') l_ptr->l_char = (byte)(cc - 1);
	    }
	}

	/* Hack -- Redefine object attr/chars */
	else if (i == '7') {

	    /* Hack -- start on "nothing" */
	    static int k_idx = 0;

	    /* Clear the screen */
	    clear_screen();

	    /* Hack -- query until done */
	    while (1) {

		int k = k_idx;

		int da = (byte)k_list[k].k_attr;
		int dc = (byte)k_list[k].k_char;
		int ca = (byte)x_list[k].x_attr;
		int cc = (byte)x_list[k].x_char;

		/* Label the object */
		Term_putstr(5, 10, -1, TERM_WHITE,
			    format("Object = %d, Name = %-40.40s",
				   k, k_list[k].name));

		/* Label the Default values */
		Term_putstr(10, 12, -1, TERM_WHITE,
			    format("Default attr/char = %3d / %3d", da, dc));
		Term_putstr(40, 12, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 12, da, dc);

		/* Label the Current values */
		Term_putstr(10, 14, -1, TERM_WHITE,
			    format("Current attr/char = %3d / %3d", ca, cc));
		Term_putstr(40, 14, -1, TERM_WHITE, "<< ? >>");
		Term_putch(43, 14, ca, cc);

		/* Directions */
		Term_putstr(5, 2, -1, TERM_WHITE, "n/N = change index");
		Term_putstr(5, 3, -1, TERM_WHITE, "a/A = change attr");
		Term_putstr(5, 4, -1, TERM_WHITE, "c/C = change char");

		/* Prompt */
		Term_putstr(0, 0, -1, TERM_WHITE,
			    "Object attr/char (n/N/a/A/c/C): ");

		/* Get a command */
		i = inkey();

		/* All done */
		if (i == ESCAPE) break;

		/* Analyze */
		if (i == 'n') k_idx = (k_idx + MAX_K_IDX + 1) % MAX_K_IDX;
		if (i == 'N') k_idx = (k_idx + MAX_K_IDX - 1) % MAX_K_IDX;
		if (i == 'a') x_list[k].x_attr = (byte)(ca + 1);
		if (i == 'A') x_list[k].x_attr = (byte)(ca - 1);
		if (i == 'c') x_list[k].x_char = (byte)(cc + 1);
		if (i == 'C') x_list[k].x_char = (byte)(cc - 1);
	    }
	}

	/* Unknown option */
	else {
	    bell();
	}
    }

    /* Leave "icky" mode */
    character_icky = FALSE;
    
    /* Grab priv's */
    safe_setuid_grab();

    /* Free turn */
    energy_use = 0;

    /* Update stuff */
    p_ptr->update |= (PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);
    
    /* Redraw stuff */
    p_ptr->redraw |= (PR_CAVE);
}


/*
 * Note something in the message recall
 */
void do_cmd_note(void)
{
    char buf[128];
    energy_use = 0;
    prt("Note: ", 0, 0);
    if (askfor(buf, 60)) msg_print(format("Note: %s", buf));
}



/*
 * Save the game
 */
void do_cmd_save_game(void)
{
    /* Hack -- free turn */
    energy_use = 0;
	    
    /* Disturb the player */
    disturb(1, 0);

    /* Clear messages */
    msg_print(NULL);
    
    /* Handle stuff */
    handle_stuff();

    /* Message */
    prt("Saving game... ", 0, 0);

    /* Refresh */
    Term_fresh();
    
    /* The player is not dead */
    (void)strcpy(died_from, "(saved)");

    /* Save the player, note the result */
    if (save_player()) prt("done.", 0, 15);
    else prt("Save failed!", 0, 15);

    /* Hack -- Forget that the player was saved */
    character_saved = FALSE;

    /* Note that the player is not dead */
    (void)strcpy(died_from, "(alive and well)");
}



/*
 * Destroy an item
 */
void do_cmd_destroy(void)
{
    int                 item_val;
    bool                floor;

    cave_type           *c_ptr = &cave[py][px];
    inven_type          *i_ptr = &i_list[c_ptr->i_idx];

    cptr                pmt = "Destroy which item? ";

    char                out_val[160];
    char                tmp_str[160];


    /* Assume this will be free */
    energy_use = 0;


    /* Verify the item on the ground */
    floor = (i_ptr->tval && (i_ptr->tval < TV_MAX_PICK_UP));

    /* Get an item (allow floor) or abort */
    if (!get_item(&item_val, pmt, 0, inven_ctr, floor)) {
	if (item_val == -2) msg_print("You have nothing to destroy.");
	return;
    }

    /* Get the item (if in inven/equip) */
    if (item_val >= 0) i_ptr = &inventory[item_val];


    /* Describe the object */
    objdes(tmp_str, i_ptr, 3);

    /* Make a verification */
    sprintf(out_val, "Really destroy %s?", tmp_str);
    
    /* XXX XXX Verify with the player */
    if (other_query_flag && !get_check(out_val)) return;


    /* Take a turn */
    energy_use = 100;

    /* Artifacts cannot be destroyed */
    if (artifact_p(i_ptr)) {

	/* Message */
	sprintf(out_val, "Cannot destroy %s.", tmp_str);
	msg_print(out_val);

	/* Done */
	return;
    }
    
    /* Message */
    sprintf(out_val, "Destroyed %s.", tmp_str);
    msg_print(out_val);
    
    /* Eliminate the item (from the pack) */
    if (item_val >= 0) {
	inven_item_increase(item_val, -999);
	inven_item_optimize(item_val);
    }

    /* Eliminate the item (from the floor) */
    else {
	floor_item_increase(py, px, -999);
	floor_item_optimize(py, px);
    }


    /* Update stuff */
    p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);

    /* Redraw the choice window */
    p_ptr->redraw |= (PR_CHOICE);
}


/*
 * Observe an item which has been *identify*-ed
 */
void do_cmd_observe(void)
{
    int                 item_val;
    bool                floor;

    cave_type           *c_ptr = &cave[py][px];
    inven_type          *i_ptr = &i_list[c_ptr->i_idx];

    cptr                pmt = "Examine which item? ";

    char                out_val[160];
    char                tmp_str[160];


    /* Assume free */
    energy_use = 0;
    
    
    /* Verify the item on the ground */
    floor = (i_ptr->tval && (i_ptr->tval < TV_MAX_PICK_UP));

    /* Get an item to identify (allow floor) or abort */
    if (!get_item(&item_val, pmt, 0, INVEN_TOTAL-1, floor)) {
	if (item_val == -2) msg_print("You have nothing to examine.");
	return;
    }

    /* Get the item (if in inven/equip) */
    if (item_val >= 0) i_ptr = &inventory[item_val];


    /* Require full knowledge */
    if (!(i_ptr->ident & ID_MENTAL)) {
	msg_print("You have no special knowledge about that item.");
	return;
    }
    
    
    /* Description */
    objdes(tmp_str, i_ptr, 3);

    /* Describe */
    sprintf(out_val, "Examining %s...", tmp_str);
    message(out_val, 0);

    /* Describe it fully */
    if (!identify_fully_aux(i_ptr)) msg_print("You see nothing special.");
}










/*
 * Define a new "keymap" entry
 */
void do_cmd_keymap(void)
{
    char i1, i2, i3;

    /* Free turn */
    energy_use = 0;

#ifdef ALLOW_KEYMAP

    /* Flush the input */
    flush();

    /* Get the trigger */
    if (get_com("Type the trigger keypress: ", &i1) &&
	get_com("Type the resulting keypress: ", &i2) &&
	get_com("Type a direction (or space): ", &i3)) {

	/* Acquire the array index */
	int k = (byte)(i1);
	int r = (byte)(i2);
	int d = (byte)(i3);

	/* Analyze the result key */
	keymap_cmds[k] = r;

	/* Hack -- Analyze the "direction" (always allow numbers) */
	keymap_dirs[k] = (isdigit(d) ? (d - '0') : keymap_dirs[d]);

	/* Success */
	prt(format("Keypress 0x%02x mapped to 0x%02x/%d",
		   k, keymap_cmds[k], keymap_dirs[k]), 0, 0);
    }

    /* Forget it */
    else {
	prt("Cancelled.", 0, 0);
    }

#else

    msg_print("You are not allowed to do that.");

#endif

}





/*
 * Parse and execute the current command
 * Give "Warning" on illegal commands.
 */
void process_command(void)
{
    /* Parse the command */
    switch (command_cmd) {

	/* (ESC) do nothing. */
	case ESCAPE:
	    energy_use = 0; break;

	/* (SPACE) do nothing */
	case ' ':
	    energy_use = 0; break;



	/*** Wizard Commands ***/

	/* Toggle Wizard Mode */
	case CTRL('W'):
	    energy_use = 0;
	    if (wizard) {
		wizard = FALSE;
		msg_print("Wizard mode off.");
	    }
	    else if (!can_be_wizard) {
		msg_print("You are not allowed to do that.");
	    }
	    else if (enter_wiz_mode()) {
		wizard = TRUE;
		msg_print("Wizard mode on.");
	    }
	    
	    /* Update monsters */
	    p_ptr->update |= (PU_MONSTERS);

	    /* Redraw stats */
	    p_ptr->redraw |= (PR_BLOCK);

	    break;

	/* Special Wizard Command */
	case CTRL('A'):
	    if (wizard) {
		do_wiz_command();
	    }
	    else {
		energy_use = 0;
		msg_print("You are not allowed to do that.");
	    }
	    break;

	/* Interact with the Borg */
	case CTRL('Z'):

	    energy_use = 0;

#ifdef ALLOW_BORG
	    borg_mode();
#endif

	    break;


	/*** Inventory Commands ***/

	/* Wear or wield something */
	case '[':
	    do_cmd_inven_w(); break;

	/* Take something off */
	case ']':
	    do_cmd_inven_t(); break;

	/* Drop something */
	case 'd':
	    do_cmd_inven_d(); break;

	/* Equipment list */
	case 'e':
	    do_cmd_inven_e(); break;

	/* Inventory */
	case 'i':
	    do_cmd_inven_i(); break;


	/*** Some temporary commands ***/
	
	/* Destory an inventory slot */
	case 'k':
	    do_cmd_destroy(); break;
	    
	/* Identify an object */
	case 'I':
	    do_cmd_observe(); break;
	    

	/*** Handle "choice window" ***/

	/* Hack -- toggle choice window */
	case CTRL('E'):

	    /* Free command */
	    energy_use = 0;

	    /* Hack -- flip the current status */
	    choice_default = !choice_default;

	    /* Redraw choice window */
	    p_ptr->redraw |= (PR_CHOICE);

	    break;


	/*** Standard "Movement" Commands ***/

	/* Dig a tunnel */
	case '+':
	    do_cmd_tunnel(); break;

	/* Move (usually pick up things) */
	case ';':
	    do_cmd_walk(always_pickup); break;

	/* Move (usually do not pick up) */
	case '-':
	    do_cmd_walk(!always_pickup); break;


	/*** Running, Resting, Searching, Staying */

	/* Begin Running -- Arg is Max Distance */
	case '.':
	    do_cmd_run(); break;

	/* Stay still (usually pick things up) */
	case ',':
	    do_cmd_stay(always_pickup); break;

	/* Stay still (usually do not pick up) */
	case 'g':
	    do_cmd_stay(!always_pickup); break;

	/* Begin Resting -- Arg is time */
	case 'R':
	    do_cmd_rest(); break;

	/* Search the adjoining grids */
	case 's':
	    do_cmd_search(); break;

	/* Toggle search status */
	case 'S':
	    do_cmd_toggle_search(); break;


	/*** Stairs and Doors and Chests and Traps ***/

	/* Go up staircases */
	case '<':
	    do_cmd_go_up(); break;

	/* Go down staircases */
	case '>':
	    do_cmd_go_down(); break;

	/* Open something */
	case 'o':
	    do_cmd_open(); break;

	/* Close something */
	case 'c':
	    do_cmd_close(); break;

	/* Spike a door */
	case 'j':
	    do_cmd_spike(); break;

	/* Force a door or Bash a monster. */
	case 'B':
	    do_cmd_bash(); break;

	/* Disarm a trap */
	case 'D':
	    do_cmd_disarm(); break;


	/*** Magic and Prayers ***/

	/* Gain some spells */
	case 'G':
	    do_cmd_study(); break;

	/* Peruse a Book */
	case 'b':
	    do_cmd_browse(); break;

	/* Cast a magic spell */
	case 'm':
	    do_cmd_cast(); break;

	/* Pray a prayer */
	case 'p':
	    do_cmd_pray(); break;

	/* use innate powers */
	case 'P':
	    do_cmd_powers(); break;

	/*** Use various objects ***/

	/* Inscribe an object */
	case '{':
	    do_cmd_inscribe(); break;

	/* Inscribe an object (in a different way) */
	case '}':
	    do_cmd_uninscribe(); break;

	/* Activate an artifact */
	case 'A':
	    do_cmd_activate(); break;

	/* Eat some food */
	case 'E':
	    do_cmd_eat_food(); break;

	/* Fill the lamp */
	case 'F':
	    do_cmd_refill(); break;

	/* Throw something */
	case 'f':
	    do_cmd_fire(); break;

	/* Zap a wand */
	case 'a':
	    do_cmd_aim_wand(); break;

	/* Activate a rod */
	case 'z':
	    do_cmd_zap_rod(); break;

	/* Quaff a potion */
	case 'q':
	    do_cmd_quaff_potion(); break;

	/* Read a scroll */
	case 'r':
	    do_cmd_read_scroll(); break;

	/* Zap a staff */
	case 'u':
	    do_cmd_use_staff(); break;


	/*** Looking at Things (nearby or on map) ***/

	/* Full screen Map */
	case 'M':
	    do_cmd_view_map(); break;

	/* Locate player on the map */  
	case 'L':
	    do_cmd_locate(); break;

	/* Examine surroundings */
	case 'l':
	    do_cmd_look(); break;

	/* Examine current target location */
	case 'x':
	    do_cmd_examine(); break;

	/* Attempt to select a new target, if compiled */
	case '*':
	    do_cmd_target(); break;



	/*** Help and Such ***/

	/* Help */
	case '?':
	    do_cmd_help("help.hlp"); break;

	/* Identify Symbol */
	case '/':
	    do_cmd_query_symbol(); break;

	/* Character Description */
	case 'C':
	    do_cmd_change_name(); break;


	/*** System Commands ***/

	case '@':
	    do_cmd_macro(FALSE); break;

	case '!':
	    do_cmd_macro(TRUE); break;

	case '&':
	    do_cmd_keymap(); break;

	/* Set options */
	case '=':
	    do_cmd_options(); break;

	/* Interact with preference files */
	case '%':
	    do_cmd_prefs(); break;


	/*** Misc Commands ***/
	
	case ':':
	    do_cmd_note(); break;       

	/* Hack -- Game Version */
	case 'V':
	    do_cmd_help("version.hlp"); break;

	/* Repeat Feeling */
	case CTRL('F'):
	    do_cmd_feeling(); break;

	/* Previous message(s). */
	case CTRL('P'):
	    do_cmd_messages(); break;

	/* Commit Suicide and Quit */
	case CTRL('K'):
	    do_cmd_suicide(); break;

	/* Save and Quit */
	case CTRL('X'):
	    exit_game(); break;

#ifndef VERIFY_SAVEFILE
	/* Hack -- Save (no quit) */
	case CTRL('S'):
	    do_cmd_save_game(); break;
#endif

	/* Redraw the screen */
	case CTRL('R'):
	    do_cmd_redraw(); break;

	/* Check artifacts */
	case '~':
	    do_cmd_check_artifacts(); break;

	/* Check uniques */
	case '|':
	    do_cmd_check_uniques(); break;


	/* Hack -- Unknown command */
	default:
	    energy_use = 0;
	    prt("Type '?' for help.", 0, 0);
	    return;
    }


    /* Save the command */
    command_old = command_cmd;


    /* Optional fresh */
    if (fresh_after) {

	/* Handle stuff */
	handle_stuff();

	/* Hack -- Hilite the player */
	move_cursor_relative(py, px);

	/* Refresh */            
	Term_fresh();
    }
}





/*
 * XXX An explanation of the "Angband Keypress" concept. XXX
 *
 * Inherently, many Angband commands consist not only of a basic action
 * (such as "tunnel"), but also a direction (such as "north-east"), plus
 * other information such as a "repeat" count or "extra argument".
 *
 * These components are thus explicitly represented, with globals.
 *
 * The "base command" (see below) is stored in "command_cmd"
 * The "desired direction" is stored in "command_dir".
 * The "repeat count" is stored in "command_rep"
 * The "numerical argument" is stored in "command_arg"
 *
 * When "command_dir" is set, it overrides all calls to "get*dir()"
 * So we always reset command_dir to -1 before getting a new command.
 * Hack -- a "command_dir" of "zero" means "the current target".
 *
 * Note that "command_arg" is sometimes used to select an argument
 * to a command (whereas "command_rep" actually "repeats" it).
 * Commands using this include "rest", "run", and "wizard" commands.
 *
 * Note that nothing cancels "command_rep", so it must be explicitly
 * canceled by the repeatable functions on "termination" conditions.
 * The simple way to do this is to force the respective do_cmd routines
 * to actually check explicitly for permission to NOT cancel it.
 * The only way to cancel "command_rep" is via the "disturb()" function.
 *
 * Note that, to correctly handle repeating commands, all commands that
 * wish to be repeatable AND to do so with a specific "direction" had
 * better set "command_dir" on their first call to the user's DESIRED
 * direction.  A local copy can then be used to check confusion, etc.
 * The easiest way to do this is to actually USE "command_dir" as the
 * argument holding the direction (see "do_cmd_walk").
 *
 * Note that, however, to correctly handle confusion + repeated commands,
 * it may be necessary to call "get_a_dir" as above, and then use a temp
 * dir to apply confusion, via "dir = command_dir; confuse_dir(&dir,mode);"
 * where mode is, for example, 0x02 for "partial" confusion.
 *
 * The last command successfully executed is stored in "command_old".
 *
 * Eventually, "command_esc" will allow one to track low level "Escapes".
 */



/*
 * Check whether this command can be "repeated".
 *
 * Note -- this routine applies ONLY to "Angband Commands".
 *
 * Repeated commands must be VERY careful to correctly turn off the
 * "repeat" (by calling "disturb()") if they induce an action of any
 * kind that should cancel the "repeat".
 */
static int command_takes_rep(char c)
{
    /* Examine the command */
    switch (c) {

	/* Take a direction */
	case '-': /* Jump */
	case ';': /* Walk */
	case '+': /* Tunnel */
	case 'D': /* Disarm */
	case 'B': /* Bash/Force */
	case 'o': /* Open */
	    return TRUE;

	/* Take no direction */
	case ',': /* Stay still */
	case 'g': /* Stay still */
	case 's': /* Search */
	    return TRUE;
    }

    /* Assume no count allowed */
    return (FALSE);
}



/*
 * Check whether this command will accept an argument.
 * An "argument" command is one which allows the use of
 * the "repeat" formalism, but does not actually repeat.
 *
 * These commands are supplied an "extra" argument in the global variable
 * "command_arg".  It is (currently) always an integer from 0 to 9999.
 *
 * Note -- this routine applies ONLY to "Angband Commands".
 */
static int command_takes_arg(char c)
{
    /* Examine the command */
    switch (c) {

	/* Normal commands */
	case '.': /* Run */
	case 'R': /* Rest */

	/* Hack -- All Wizard Commands */
	case CTRL('A'): /* Special Wizard Command */
	    return TRUE;
    }

    /* Assume no count allowed */
    return (FALSE);
}




/*
 * Request a command from the user.
 *
 * Sets command_cmd, command_dir, command_rep, command_arg.
 *
 * Note that "caret" ("^") is treated special, and is used to
 * allow manual input of control characters.  This can be used
 * on many machines to request repeated tunneling (Ctrl-H) and
 * on the Macintosh to request "Control-Caret".
 */
void request_command(void)
{
    int i = 0;
    char cmd;


    /* Hack -- Assume no abortion yet */
    command_esc = 0;


    /* Hack -- process "repeated" commands */
    if (command_rep) {

	/* Count this execution */
	command_rep--;

	/* Hack -- Dump repeat count */
	p_ptr->redraw |= (PR_STATE);

	/* Handle stuff */
	handle_stuff();

	/* Hack -- Illuminate the player */
	move_cursor_relative(py, px);

	/* Refresh */
	Term_fresh();
	
	/* Hack -- Assume messages were seen */
	msg_flag = FALSE;

	/* Keep the current command */
	return;
    }


    /* No command yet */
    command_cmd = 0;

    /* No "argument" yet */
    command_arg = 0;

    /* Hack -- no direction yet */
    command_dir = -1;


    /* Hack -- Optional flush */
    if (flush_command) flush();


    /* Hack -- auto-commands */
    if (command_new) {
	prt("", 0, 0);
	cmd = command_new;
	command_new = 0;
    }

    /* Get a keypress in "command" mode */
    else {
	msg_flag = FALSE;
	move_cursor_relative(py, px);
	inkey_flag = TRUE;
	cmd = inkey();
	inkey_flag = FALSE;
    }


    /* Special command -- Get a "count" for another command */
    if (cmd == '0') {

	/* Begin the input */
	prt("Repeat count:", 0, 0);

	/* Get a command count */
	while (1) {

	    /* Get a new keypress */
	    cmd = inkey();

	    /* Simple editing */
	    if (cmd == DELETE || cmd == CTRL('H')) {
		i = i / 10;
		prt(format("Repeat count: %d", i), 0, 0);
	    }

	    /* Actual numeric data */
	    else if (cmd >= '0' && cmd <= '9') {

		/* Allow counts up to 9999 */
		if (i > 999) {
		    bell();
		}

		/* Incorporate that digit */
		else {
		    i = i * 10 + cmd - '0';
		    prt(format("Repeat count: %d", i), 0, 0);
		}
	    }

	    /* Exit on "unusable" input */
	    else {
		break;
	    }
	}

	/* Let a "non-count" default to 99 repetitions */
	if (i == 0) {
	    i = 99;
	    prt(format("Repeat count: %d", i), 0, 0);
	}

	/* Hack -- white-space means "enter command now" */
	if ((cmd == ' ') || (cmd == '\n') || (cmd == '\r')) {
	    if (!get_com("Command:", &cmd)) cmd = ESCAPE;
	}
    }


    /* Bypass "keymap" */
    if (cmd == '\\') {

	/* Get a char to use without casting */
	if (!get_com("Command: ", &cmd)) cmd = ESCAPE;

	/* Hack -- allow "control chars" to be entered */
	if (cmd == '^') {

	    /* Get a char to "cast" into a control char */
	    if (!get_com("Command: Control-", &cmd)) cmd = ESCAPE;

	    /* Hack -- create a control char if legal */
	    else if (CTRL(cmd)) cmd = CTRL(cmd);
	}

	/* Use the key directly */
	command_cmd = cmd;
    }

    /* Utilize "keymap" */
    else {

	/* Hack -- allow "control chars" to be entered */
	if (cmd == '^') {

	    /* Get a char to "cast" into a control char */
	    if (!get_com("Control-", &cmd)) cmd = ESCAPE;

	    /* Hack -- create a control char if legal */
	    else if (CTRL(cmd)) cmd = CTRL(cmd);
	}

	/* Access the array info */
	command_cmd = keymap_cmds[(byte)(cmd)];
	command_dir = keymap_dirs[(byte)(cmd)];

	/* Hack -- notice "undefined" commands */
	if (!command_cmd) command_cmd = ESCAPE;

	/* Hack -- extract "non-directions" if needed */
	if ((command_dir < 1) || (command_dir > 9)) command_dir = -1;
    }


    /* Some commands can be "auto-repeated" by default */
    if (always_repeat && (i <= 0)) {

	/* Bash, Disarm, Open, Tunnel get 99 tries */
	if (strchr("BDo+", command_cmd)) i = 99;
    }

    /* Make sure a "Count" is legal for this command */
    if ((i > 0) && (command_cmd != ESCAPE)) {

	/* Commands that can be repeated */
	if (command_takes_rep(command_cmd)) {

	    /* Save the count (this time counts) */
	    command_rep = i - 1;

	    /* Hack -- dump the count */
	    p_ptr->redraw |= PR_STATE;

	    /* Handle stuff */
	    handle_stuff();
	}

	/* Commands that take arguments */
	else if (command_takes_arg(command_cmd)) {

	    /* Save the argument */
	    command_arg = i;
	}

	/* Invalid combination */
	else {

	    /* Abort gracefully */
	    msg_print("Invalid command with a count.");

	    /* Forget the command */
	    command_cmd = ESCAPE;
	}
    }

    /* Hack -- erase the message line. */
    prt("", 0, 0);

    /* Hack -- Re-Illuminate the player */
    move_cursor_relative(py, px);
}



