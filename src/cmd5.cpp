// File: cmd5.cpp



// Purpose: Spell/Prayer commands



/*

 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke

 *

 * This software may be copied and distributed for educational, research, and

 * not for profit purposes provided that this copyright and statement are

 * included in all such copies.

 */



#include "utumno.h"







/*

 * Returns spell chance of failure for spell            -RAK-

 */

static s16b spell_chance(int spell)

{

    int chance, minfail;



    magic_type *s_ptr;





    /* Paranoia -- must be literate */

    if (!mp_ptr->spell_book) return 100;



    /* Access the spell */

    s_ptr = &mp_ptr->info[spell];



    /* Extract the base spell failure rate */

    chance = s_ptr->sfail;



    /* Reduce failure rate by "effective" level adjustment */

    chance -= 3 * (p_ptr->GetLev() - s_ptr->slevel);



    /* Reduce failure rate by INT/WIS adjustment */

    chance -= 3 * (adj_mag_stat[p_ptr->GetStatInd(mp_ptr->spell_stat)] - 1);



    // Not enough mana to cast

    if (s_ptr->smana > p_ptr->GetCSP()) {

        chance += 5 * (s_ptr->smana - p_ptr->GetCSP());

    }



    /* Extract the minimum failure rate */

    minfail = adj_mag_fail[p_ptr->GetStatInd(mp_ptr->spell_stat)];



    /* Non mage/priest characters never get too good */

    if ((p_ptr->GetClass() != CLASS_MAGE) && (p_ptr->GetClass() != CLASS_PRIEST)) {

        if (minfail < 5) minfail = 5;

    }



    /* Hack -- Priest prayer penalty for "edged" weapons  -DGK */

    if ((p_ptr->GetClass() == CLASS_PRIEST) && p_ptr->get_icky_wield()) {

        chance += 25;

    }



    // Bad gloves increase failure rates

    if (p_ptr->get_cumber_glove()) chance += 20;



    // Minimum failure rate

    if (chance < minfail) chance = minfail;



    /* Stunning makes spells harder */

    if (p_ptr->GetStun() > 50) chance += 25;

    else if (p_ptr->GetStun()) chance += 15;



    /* Always a 5 percent chance of working */

    if (chance > 95) chance = 95;



    /* Return the chance */

    return chance;

}







/*

 * Determine if a spell is "okay" for the player to cast or study

 * The spell must be legible, not forgotten, and also, to cast,

 * it must be known, and to study, it must not be known.

 */

static bool spell_okay(int j, bool known)

{

    magic_type *s_ptr;



    // Access the spell

    s_ptr = &mp_ptr->info[j];



    // Spell is illegal

    if (s_ptr->slevel > p_ptr->GetLev()) return FALSE;



    /* Spell is forgotten */

    if (spell_forgotten[j]) return FALSE;



    /* Spell is learned */

    if (spell_learned[j]) {

        /* Okay to cast, not to study */

        return known;

    }



    /* Okay to study, not to cast */

    return !known;

}







/*

 * Extra information on a spell         -DRS-

 *

 * We can use up to 14 characters of the buffer 'p'

 *

 * The strings in this function were extracted from the code in the

 * functions "do_cmd_cast()" and "do_cmd_pray()" and may be dated.

 */

static void spell_info(char *p, int j)

{

    /* Default */

    strcpy(p, "");



    /* Mage spells */

    if (mp_ptr->spell_book == TV_MAGIC_BOOK) {

        int plev = p_ptr->GetLev();



        /* Analyze the spell */

        switch (j) {

            case 0: sprintf(p, "dam %dd4", 3+((plev-1)/5)); break;

            case 2: strcpy(p, "range 10"); break;

            case 5: strcpy(p, "heal 2d8"); break;

            case 8: sprintf(p, "dam %d", 10 + (plev / 2)); break;

            case 10: sprintf(p, "dam %dd8", (3+((plev-5)/4))); break;

            case 14: sprintf(p, "range %d", plev * 10); break;

            case 15: strcpy(p, "dam 6d8"); break;

            case 16: sprintf(p, "dam %dd8", (5+((plev-5)/4))); break;

            case 24: sprintf(p, "dam %dd8", (8+((plev-5)/4))); break;

            case 26: sprintf(p, "dam %d", 30 + plev); break;

            case 29: sprintf(p, "dur %d+d20", plev); break;

            case 30: sprintf(p, "dam %d", 55 + plev); break;

            case 38: sprintf(p, "dam %dd8", (6+((plev-5)/4))); break;

            case 39: sprintf(p, "dam %d", 40 + plev/2); break;

            case 40: sprintf(p, "dam %d", 40 + plev); break;

            case 41: sprintf(p, "dam %dd10", (10+((plev-5)/2))); break;

            case 42: sprintf(p, "dam %d", 65 + plev); break;

            case 43: sprintf(p, "dam %dd17", plev); break;

            case 49: strcpy(p, "dur 20+d20"); break;

            case 50: strcpy(p, "dur 20+d20"); break;

            case 51: strcpy(p, "dur 20+d20"); break;

            case 52: strcpy(p, "dur 20+d20"); break;

            case 53: strcpy(p, "dur 20+d20"); break;

            case 54: strcpy(p, "dur 25+d25"); break;

            case 55: strcpy(p, "dur 30+d20"); break;

            case 56: strcpy(p, "dur 25+d25"); break;

            case 57: sprintf(p, "dur %d+d25", 30+plev); break;

            case 58: strcpy(p, "dur 8+d8"); break;

        }

    }



    // Priest spells

    if (mp_ptr->spell_book == TV_PRAYER_BOOK) {

        int plev = p_ptr->GetLev();



        // See below

        int orb = (p_ptr->GetClass() == CLASS_PRIEST) ? (plev/4) : 0;



        // Analyze the spell

        switch (j) {

            case 1: strcpy(p, "heal 2d10"); break;

            case 2: strcpy(p, "dur 12+d12"); break;

            case 9: sprintf(p, "range %d", 3*plev); break;

            case 10: strcpy(p, "heal 4d10"); break;

            case 11: strcpy(p, "dur 24+d24"); break;

            case 15: strcpy(p, "dur 10+d10"); break;

            case 17: sprintf(p, "%d+2d6", plev + orb); break;

            case 18: strcpy(p, "heal 6d10"); break;

            case 19: strcpy(p, "dur 24+d24"); break;

            case 20: sprintf(p, "dur %d+d25", 3*plev); break;

            case 23: strcpy(p, "heal 8d10"); break;

            case 25: strcpy(p, "dur 48+d48"); break;

            case 26: sprintf(p, "dam d%d", 3*plev); break;

            case 27: strcpy(p, "heal 300"); break;

            case 28: sprintf(p, "dam d%d", 3*plev); break;

            case 30: strcpy(p, "heal 1000"); break;

            case 36: strcpy(p, "heal 4d10"); break;

            case 37: strcpy(p, "heal 8d10"); break;

            case 38: strcpy(p, "heal 2000"); break;

            case 41: sprintf(p, "dam d%d", 4*plev); break;

            case 42: sprintf(p, "dam d%d", 4*plev); break;

            case 45: strcpy(p, "dam 150"); break;

            case 52: strcpy(p, "range 10"); break;

            case 53: sprintf(p, "range %d", 8*plev); break;

        }

    }

}





/*

 * Print a list of spells (for browsing or casting)

 */

static void print_spells(byte *spell, int num)

{

    int i, j, col;

    magic_type *s_ptr;

    char comment[160];

    char info[160], out_val[160];

    byte attr;





    /* Print column */

    col = 30;



    // Title the list

    box(col*8, 16, 639, 31, COLOR_BLACK);

    put_text((col+5)*8, 16, "Name", COLOR_WHITE, FONT_BOLD);

    put_string((col+25)*8, 16, "Lv Mana Fail", COLOR_WHITE);



    // Dump the spells

    for (i = 0; i < num; i++) {

        // Access the spell

        j = spell[i];



        // Access the spell

        s_ptr = &mp_ptr->info[j];



        // Skip illegible spells

        if (s_ptr->slevel >= 99) {

            box(col*8, (2+i)*16, 639, (2+i)*16+15, COLOR_BLACK);

            sprintf(out_val, "%c)", I2A(i));

            put_string((col+2)*8, (2+i)*16, out_val, COLOR_GREY);

            sprintf(out_val, "%-30s", "(illegible)");

            put_text((col+5)*8, (2+i)*16, out_val, COLOR_GREY, FONT_BOLD);

            continue;

        }



        // XXX XXX Could label spells above the players level



        // Get extra info

        spell_info(info, j);



        // Use that info

        strcpy(comment , info);



        // Set the color

        attr = COLOR_WHITE;



        // Analyze the spell

        if (spell_forgotten[j]) {

 	/*SAW*/

            strcpy(comment , "forgotten");

            attr = COLOR_ORANGE;

        }

        else if (!spell_learned[j]) {

            strcpy(comment, "unknown");

            attr = COLOR_RED;

        }

        else if (!spell_worked[j]) {

            strcpy(comment,"untried");

            attr = COLOR_LT_GREY;

        }



        // Dump the spell --(--

        box(col*8, (2+i)*16, 639, (2+i)*16+15, COLOR_BLACK);

        sprintf(out_val, "%c)", I2A(i));

        put_string((col+2)*8, (2+i)*16, out_val, attr);

        sprintf(out_val, "%-30s",

            spell_names[mp_ptr->spell_type][j]);

        put_text((col+5)*8, (2+i)*16, out_val, attr, FONT_BOLD);

        sprintf(out_val, "%2d %4d %3d%% %s",

            s_ptr->slevel, s_ptr->smana, spell_chance(j), comment);

        put_string((col+25)*8, (2+i)*16, out_val, attr);

    }



    // Clear the bottom line

    box(col*8, (2+i)*16, 639, (2+i)*16+15, COLOR_BLACK);

}









/*

 * Allow user to choose a spell/prayer from the given book.

 *

 * If a valid spell is chosen, saves it in '*sn' and returns TRUE

 * If the user hits escape, returns FALSE, and set '*sn' to -1

 * If there are no legal choices, returns FALSE, and sets '*sn' to -2

 *

 * The "prompt" should be "cast", "recite", or "study"

 * The "known" should be TRUE for cast/pray, FALSE for study

 */

static int get_spell(int *sn, char *prompt, int sval, bool known)

{

    int i, j = -1;

    byte spell[64], num = 0, *screen = NULL;

    bool flag, redraw, okay;

    char choice;

    char out_val[160];

    char p[20];

    strcpy(p,(mp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" : "prayer");





    /* Extract spells */

    for (i = 0; i < 64; i++) {

        /* Check for this spell */

        if ((i < 32) ?

            (spell_flags[mp_ptr->spell_type][sval][0] & (1L << i)) :

            (spell_flags[mp_ptr->spell_type][sval][1] & (1L << (i - 32))))

        {

            /* Collect this spell */

            spell[num++] = i;

        }

    }





    /* Assume no usable spells */

    okay = FALSE;



    /* Assume no spells available */

    *sn = -2;



    /* Check for "okay" spells */

    for (i = 0; i < num; i++) {

        /* Look for "okay" spells */

        if (spell_okay(spell[i], known)) okay = TRUE;

    }



    /* No "okay" spells */

    if (!okay) return FALSE;





    /* Assume cancelled */

    *sn = -1;



    /* Nothing chosen yet */

    flag = FALSE;



    /* No redraw yet */

    redraw = FALSE;





    /* Build a prompt (accept all spells) */

    strnfmt(out_val, 78, "(%^ss %c-%c, *=List, ESC=exit) %^s which %s? ",

            p, I2A(0), I2A(num - 1), prompt, p);



    /* Get a spell from the user */

    while (!flag && get_com(out_val, &choice)) {

        // Request redraw

        if ((choice == ' ') || (choice == '*') || (choice == '?')) {

            /* Show the list */

            if (!redraw) {

                /* Show list */

                redraw = TRUE;



                /* Save the screen */

                if (screen) delete[] screen;

                screen = save_screen();



                /* Display a list of spells */

                print_spells(spell, num);

            }



            /* Hide the list */

            else {

                // Hide list

                redraw = FALSE;



                // Restore the screen

                restore_screen(screen);

            }



            // Ask again

            continue;

        }





        /* Lowercase */

        if (isupper(choice)) choice = tolower(choice);



        /* Extract request */

        i = (islower(choice) ? A2I(choice) : -1);



        /* Totally Illegal */

        if ((i < 0) || (i >= num)) {

            bell();

            continue;

        }



        /* Save the spell index */

        j = spell[i];



        // Require "okay" spells

        if (!spell_okay(j, known)) {

            bell();

            msg_format("You may not %s that %s.", prompt, p);

            continue;

        }



        /* Stop the loop */

        flag = TRUE;    

    }





    /* Restore the screen */

    if (redraw && screen) restore_screen(screen);

    if (screen) delete[] screen;





    /* Abort if needed */

    if (!flag)
       {
       reset_timer();
       return FALSE;
       }


    /* Save the choice */

    (*sn) = j;



    /* Success */
    reset_timer();
    return TRUE;

}









/*

 * Peruse the spells/prayers in a Book

 *

 * Note that *all* spells in the book are listed

 */

void do_cmd_browse(void)

{

    int i, item, sval;

    byte spell[64], num = 0, *screen;

    CItem *i_ptr;





    // Warriors are illiterate

    if (!mp_ptr->spell_book) {

        msg_print("You cannot read books!");

        return;

    }



    // No lite

    if (p_ptr->GetBlind() || p_ptr->no_lite()) {

        msg_print("You cannot see!");

        return;

    }



    // Confused

    if (p_ptr->GetConfused()) {

        msg_print("You are too confused!");

        return;

    }





    /* Restrict choices to "useful" books */

    item_tester_tval = mp_ptr->spell_book;



    /* Get an item (from inven or floor) */

    if (!get_item(&item, "Browse which book? ", GI_INVEN | GI_FLOOR)) {

        if (item == -2) msg_print("You have no books that you can read.");
        
        reset_timer();
        return;

    }



    /* Get the item (in the pack) */

    i_ptr = gi_i_ptr;



    /* Access the item's sval */

    sval = i_ptr->GetSval();





    /* Extract spells */

    for (i = 0; i < 64; i++) {

        /* Check for this spell */

        if ((i < 32) ?

            (spell_flags[mp_ptr->spell_type][sval][0] & (1L << i)) :

            (spell_flags[mp_ptr->spell_type][sval][1] & (1L << (i - 32))))

        {

            /* Collect this spell */

            spell[num++] = i;

        }

    }





    /* Save the screen */

    screen = save_screen();



    /* Display the spells */

    print_spells(spell, num);



    /* Clear the top line */

    box(0, 0, 639, 15, COLOR_BLACK);



    /* Prompt user */

    put_text_format(320, 0, "[Press any key to continue]", COLOR_WHITE, FONT_BOLD,

        JUST_CENTER);



    /* Wait for key */

    screen_refresh();

    wait_for_key();



    /* Restore the screen */

    restore_screen(screen);

    delete[] screen;

}









/*

 * Study a book to gain a new spell/prayer

 */

bool do_cmd_study(void)

{

    int i, item, sval;

    int j = -1;



    char p[20];

    strcpy(p,((mp_ptr->spell_book == TV_MAGIC_BOOK) ? "spell" : "prayer"));

    CItem *i_ptr;



    if (!mp_ptr->spell_book) {

        msg_print("You cannot read books!");

        return TRUE;

    }



    if (p_ptr->GetBlind() || p_ptr->no_lite()) {

        msg_print("You cannot see!");

        return TRUE;

    }



    if (p_ptr->GetConfused()) {

        msg_print("You are too confused!");

        return TRUE;

    }



    if (!p_ptr->GetNewSpells()) {

        msg_format("You cannot learn any new %ss!", p);

        return TRUE;

    }



    if (p_ptr->isBusy()) return FALSE;





    /* Restrict choices to "useful" books */

    item_tester_tval = mp_ptr->spell_book;



    /* Get an item (from inven or floor) */

    if (!get_item(&item, "Study which book? ", GI_INVEN | GI_FLOOR)) {

        if (item == -2) msg_print("You have no books that you can read.");
        
        reset_timer();
        return TRUE;

    }



    /* Get the item (in the pack) */

    i_ptr = gi_i_ptr;



    /* Access the item's sval */

    sval = i_ptr->GetSval();





    /* Mage -- Learn a selected spell */

    if (mp_ptr->spell_book == TV_MAGIC_BOOK) {

        /* Ask for a spell, allow cancel */

        if (!get_spell(&j, "study", sval, FALSE) && (j == -1)) 
            {
            reset_timer();
            
            return TRUE;
            }

    }



    /* Priest -- Learn a random prayer */

    if (mp_ptr->spell_book == TV_PRAYER_BOOK) {

        int k = 0;



        // Extract spells

        for (i = 0; i < 64; i++) {

            /* Check spells in the book */

            if ((i < 32) ?

                (spell_flags[mp_ptr->spell_type][sval][0] & (1L << i)) :

                (spell_flags[mp_ptr->spell_type][sval][1] & (1L << (i - 32))))

            {

                /* Skip non "okay" prayers */

                if (!spell_okay(i, FALSE)) continue;



                /* Hack -- Prepare the randomizer */

                k++;



                /* Hack -- Apply the randomizer */

                if (rand_int(k) == 0) j = i;

            }

        }

    }



    // Nothing to study

    if (j < 0) {

        /* Message */

        msg_format("You cannot learn any %ss in that book.", p);



        /* Abort */
        reset_timer();
        return TRUE;

    }





    /* Take a turn */

    p_ptr->DrainEnergy(100);



    /* Learn the spell */

    spell_learned[j] = TRUE;



    // Find the next open entry in "spell_order[]"

    for (i = 0; i < 64; i++) {

        // Stop at the first empty space

        if (spell_order[i] == 99) break;

    }



    // Add the spell to the known list

    spell_order[i++] = j;



    // Mention the result

    msg_format("You have learned the %s of %s.",

               p, spell_names[mp_ptr->spell_type][j]);



    // One less spell available

    p_ptr->SetNewSpells(p_ptr->GetNewSpells() - 1);



    // Report on remaining prayers

    if (p_ptr->GetNewSpells()) {

        msg_format("You can learn more %ss.", p);

    }



    // Save the new_spells value

    p_ptr->SetOldSpells(p_ptr->GetNewSpells());



    // Did something
    reset_timer();
    
    return TRUE;

}







/*

 * Cast a spell or pray a prayer

 *

 * CHEATING ALERT: invalid current spells! (lost book, forgotten, ...)

 */

void do_spell(int j, int tx, int ty)

{

    int plev = p_ptr->GetLev();

    magic_type *s_ptr = &mp_ptr->info[j];

    int chance;

    int book = mp_ptr->spell_book;

    char spell_type[20];

    char action[20];

    strcpy(action,(book == TV_MAGIC_BOOK) ? "cast" : "recite");

    strcpy(spell_type,(book == TV_MAGIC_BOOK) ? "spell" : "prayer");

    // Must not be warrior

    if (!book) {

        msg_print("You cannot use any magic.");

        return;

    }



    // Require the ability to see

    if (p_ptr->GetBlind() || p_ptr->no_lite()) {

        msg_print("You cannot see!");

        return;

    }



    // Not when confused

    if (p_ptr->GetConfused()) {

        msg_print("You are too confused!");

        return;

    }



    if (p_ptr->isBusy()) return;





    // Disallow spells/prayers costing too much mana

    if (s_ptr->smana > p_ptr->GetCSP()) {

        // Message

        msg_format("You do not have enough mana to %s that %s.",

            action, spell_type);

        return;

    }





    // Use energy

    p_ptr->DrainEnergy(100);



    // Use some mana

    p_ptr->SetCSP(p_ptr->GetCSP() - s_ptr->smana);





    // Get failure probability

    chance = spell_chance(j);

     

    // Check for failure

    if (percent(chance)) {

        if (book == TV_MAGIC_BOOK) {

            msg_print("You failed to get the spell off!");

        }

        else {

            msg_print("You failed to concentrate hard enough!");

        }



        return;

    }





    if (book == TV_MAGIC_BOOK) {

        int beam;



        // Hack -- chance of beam instead of bolt

        beam = (p_ptr->GetClass() == CLASS_MAGE) ? plev : (plev / 2);



        // Switch on the exact spell

        switch (j) {

            case 0:

                fire_bolt_or_beam(beam-10, GF_MISSILE, tx, ty,

                    damroll(3 + ((plev - 1) / 5), 4));

                break;



            case 1: detect_monsters(); break;



            case 2: teleport_player(10); break;



            case 3: lite_area(damroll(2, plev / 2), (plev / 10) + 1); break;



            case 4: detect_treasure(); break;



            case 5:

                p_ptr->heal_up(damroll(2, 8));

                p_ptr->mod_cut(p_ptr->GetCut() - 15);

                break;



            case 6: detect_object(); break;



            case 7: detect_sdoor(); detect_trap(); break;



            case 8: fire_ball(GF_POIS, tx, ty, 10 + plev/2, 2); break;



            case 9: confuse_monster(tx, ty, plev); break;



            case 10:

                fire_bolt_or_beam(beam-10, GF_ELEC, tx, ty,

                    damroll(3+((plev-5)/4), 8));

                break;



            case 11: destroy_doors_touch(); break;



            case 12: sleep_monster(tx, ty); break;



            case 13: p_ptr->mod_poisoned(0); break;



            case 14: teleport_player(plev * 5); break;



            case 15:

                msg_print("A line of blue shimmering light appears.");

                lite_line(tx, ty);

                break;



            case 16:

                fire_bolt_or_beam(beam-10, GF_COLD, tx, ty, damroll(5+((plev-5)/4), 8));

                break;



            case 17: wall_to_mud(tx, ty); break;



            case 18: p_ptr->mod_food(PY_FOOD_MAX - 1); break;



            case 19: recharge(5); break;



            case 20: sleep_monsters_touch(); break;



            case 21: poly_monster(tx, ty); break;



            case 22: ident_spell(); break;



            case 23: sleep_monsters(); break;



            case 24:

                fire_bolt_or_beam(beam, GF_FIRE, tx, ty,

                    damroll(8+((plev-5)/4), 8));

                break;



            case 25: slow_monster(tx, ty); break;



            case 26: fire_ball(GF_COLD, tx, ty, 30 + plev, 2); break;



            case 27: recharge(40); break;



            case 28: teleport_monster(tx, ty); break;



            case 29:

                if (!p_ptr->GetFast()) {

                    p_ptr->mod_fast(randint(20) + plev);

                }

                else {

                    p_ptr->mod_fast(p_ptr->GetFast() + randint(5));

                }

                break;



            case 30: fire_ball(GF_FIRE, tx, ty, 55 + (plev), 2); break;



            case 31: destroy_area(p_ptr->GetY(), p_ptr->GetX(), 15, TRUE); break;



            case 32: genocide(); break;



            case 33: door_creation(); break;



            case 34: stair_creation(); break;



            case 35: teleport_player_level(); break;



            case 36: earthquake(p_ptr->GetY(), p_ptr->GetX(), 10); break;



            case 37:

                if (!p_ptr->GetWordRecall()) {

                    p_ptr->SetWordRecall(rand_int(20) + 15);

                    msg_print("The air about you becomes charged...");

                }

                else {

                    p_ptr->SetWordRecall(0);

                    msg_print("A tension leaves the air around you...");

                }

                break;



            case 38:

                fire_bolt_or_beam(beam, GF_ACID, tx, ty,

                    damroll(6+((plev-5)/4), 8));

                break;



            case 39: fire_ball(GF_POIS, tx, ty, 20 + plev/2, 3); break;



            case 40: fire_ball(GF_ACID, tx, ty, 40 + plev, 2); break;



            case 41: fire_bolt(GF_MANA, tx, ty, damroll(10+((plev-5)/2), 10)); break;



            case 42: fire_ball(GF_METEOR, tx, ty, 65 + plev, 3); break;



            case 43: fire_ball(GF_MANA, tx, ty, damroll(plev, 17), 3); break;



            case 44: detect_evil(); break;



            case 45: detect_magic(); break;



            case 46: recharge(100); break;



            case 47: genocide(); break;



            case 48: mass_genocide(); break;



            case 49: p_ptr->mod_oppose_fire(p_ptr->GetOpposeFire() + randint(20) + 20); break;



            case 50: p_ptr->mod_oppose_cold(p_ptr->GetOpposeCold() + randint(20) + 20); break;



            case 51: p_ptr->mod_oppose_acid(p_ptr->GetOpposeAcid() + randint(20) + 20); break;



            case 52: p_ptr->mod_oppose_pois(p_ptr->GetOpposePois() + randint(20) + 20); break;



            case 53:

                p_ptr->mod_oppose_acid(p_ptr->GetOpposeAcid() + randint(20) + 20);

                p_ptr->mod_oppose_elec(p_ptr->GetOpposeElec() + randint(20) + 20);

                p_ptr->mod_oppose_fire(p_ptr->GetOpposeFire() + randint(20) + 20);

                p_ptr->mod_oppose_cold(p_ptr->GetOpposeCold() + randint(20) + 20);

                p_ptr->mod_oppose_pois(p_ptr->GetOpposePois() + randint(20) + 20);

                break;



            case 54:

                p_ptr->heal_up(10);

                p_ptr->mod_hero(p_ptr->GetHero() + randint(25) + 25);

                p_ptr->mod_afraid(0);

                break;



            case 55:

                p_ptr->mod_shield(p_ptr->GetShield() + randint(20) + 30);

                break;



            case 56:

                p_ptr->heal_up(30);

                p_ptr->mod_shero(p_ptr->GetSHero() + randint(25) + 25);

                p_ptr->mod_afraid(0);

                break;



            case 57:

                if (!p_ptr->GetFast()) {

                    p_ptr->mod_fast(randint(30) + 30 + plev);

                }

                else {

                    p_ptr->mod_fast(p_ptr->GetFast() + randint(10));

                }

                break;



            case 58: p_ptr->mod_shadowform(p_ptr->GetShadowform() + randint(8) + 8); break;



            default: quit("Buggy spell."); break;

        }

    }

    else {

        // Switch on the exact prayer

        switch (j) {

            case 0: detect_evil(); break;



            case 1:

                p_ptr->heal_up(damroll(2, 10));

                p_ptr->mod_cut(p_ptr->GetCut() - 10);

                break;



            case 2: p_ptr->mod_blessed(p_ptr->GetBlessed() + randint(12) + 12); break;



            case 3: p_ptr->mod_afraid(0); break;



            case 4: lite_area(damroll(2, plev / 2), (plev / 10) + 1); break;



            case 5: detect_trap(); break;



            case 6: detect_sdoor(); break;



            case 7: p_ptr->mod_poisoned(p_ptr->GetPoisoned() / 2); break;



            case 8: fear_monster(tx, ty, plev); break;



            case 9: teleport_player(plev * 3); break;



            case 10:

                p_ptr->heal_up(damroll(4, 10));

                p_ptr->mod_cut((p_ptr->GetCut() / 2) - 20);

                break;



            case 11: p_ptr->mod_blessed(p_ptr->GetBlessed() + randint(24) + 24); break;



            case 12: sleep_monsters_touch(); break;



            case 13: p_ptr->mod_food(PY_FOOD_MAX - 1); break;



            case 14: remove_curse(); break;



            case 15:

                p_ptr->mod_oppose_fire(p_ptr->GetOpposeFire() + randint(10) + 10);

                p_ptr->mod_oppose_cold(p_ptr->GetOpposeCold() + randint(10) + 10);

                break;



            case 16: p_ptr->mod_poisoned(0); break;



            case 17:

                fire_ball(GF_HOLY_ORB, tx, ty,

                    damroll(2,6) + plev +

                    (p_ptr->GetClass() == CLASS_PRIEST) ? (plev/4) : 0,

                    ((plev < 30) ? 2 : 3));

                break;



            case 18:

                p_ptr->heal_up(damroll(6, 10));

                p_ptr->mod_cut(0);

                break;



            case 19: p_ptr->mod_tim_invis(p_ptr->GetTimInvis() + randint(24) + 24); break;



            case 20:

                p_ptr->mod_protevil(p_ptr->GetProtevil() + randint(25) + 3*p_ptr->GetLev());

                break;



            case 21: earthquake(p_ptr->GetY(), p_ptr->GetX(), 10); break;



            case 22: map_area(); break;



            case 23:

                p_ptr->heal_up(damroll(8, 10));

                p_ptr->mod_stun(0);

                p_ptr->mod_cut(0);

                break;



            case 24: turn_undead(); break;



            case 25: p_ptr->mod_blessed(p_ptr->GetBlessed() + randint(48) + 48); break;



            case 26: dispel_undead(plev * 3); break;



            case 27:

                p_ptr->heal_up(300);

                p_ptr->mod_stun(0);

                p_ptr->mod_cut(0);

                p_ptr->mod_poisoned(0);

                break;



            case 28: dispel_evil(plev * 3); break;



            case 29: warding_glyph(); break;



            case 30:

                dispel_evil(plev * 4);

                p_ptr->heal_up(1000);

                p_ptr->mod_afraid(0);

                p_ptr->mod_poisoned(0);

                p_ptr->mod_stun(0);

                p_ptr->mod_cut(0);

                break;



            case 31: detect_monsters(); break;



            case 32: detection(); break;



            case 33: ident_spell(); break;



            case 34: probing(); break;



            case 35: wiz_lite(); break;



            case 36:

                p_ptr->heal_up(damroll(4, 10));

                p_ptr->mod_cut(0);

                break;



            case 37:

                p_ptr->heal_up(damroll(8, 10));

                p_ptr->mod_stun(0);

                p_ptr->mod_cut(0);

                break;



            case 38:

                p_ptr->heal_up(2000);

                p_ptr->mod_stun(0);

                p_ptr->mod_cut(0);

                p_ptr->mod_poisoned(0);

                break;



            case 39:

                do_res_stat(STAT_STR);

                do_res_stat(STAT_INT);

                do_res_stat(STAT_WIS);

                do_res_stat(STAT_DEX);

                do_res_stat(STAT_CON);

                do_res_stat(STAT_CHR);

                break;



            case 40: p_ptr->restore_level(); break;



            case 41: dispel_undead(plev * 4); break;



            case 42: dispel_evil(plev * 4); break;



            case 43:

                if (banish_evil(100)) {

                    msg_print("The power of your god banishes evil!");

                }

                break;



            case 44: destroy_area(p_ptr->GetY(), p_ptr->GetX(), 15, TRUE); break;



            case 45: fire_bolt(GF_HOLY_ORB, tx, ty, 150); break;



            case 46: destroy_doors_touch(); break;



            case 47: recharge(15); break;



            case 48: remove_all_curse(); break;



            case 49: enchant_spell(rand_int(4) + 1, rand_int(4) + 1, 0); break;



            case 50: enchant_spell(0, 0, rand_int(3) + 2); break;



            case 51: brand_weapon(); break;



            case 52: teleport_player(10); break;



            case 53: teleport_player(plev * 8); break;



            case 54: teleport_monster(tx, ty); break;



            case 55: teleport_player_level(); break;



            case 56:

                if (!p_ptr->GetWordRecall()) {

                    p_ptr->SetWordRecall(rand_int(20) + 15);

                    msg_print("The air about you becomes charged...");

                }

                else {

                    p_ptr->SetWordRecall(0);

                    msg_print("A tension leaves the air around you...");

                }

                break;



            case 57:

                msg_print("The world changes!");

                new_level_flag = TRUE;

                break;



            default: quit("Buggy prayer."); break;

        }

    }



    // A spell was cast

    if (!spell_worked[j]) {

        int e = s_ptr->sexp;



        // The spell worked

        spell_worked[j] = TRUE;



        // Gain experience

        p_ptr->gain_exp(e * s_ptr->slevel);

    }

}





/*

 * Choose and cast a spell by invoking either do_pray() or do_cast()

 */

void do_cmd_magic(void)

{

    int book = mp_ptr->spell_book;

    char spell_type[20] ;

    char action[20] ;

    int item, sval, j;



    strcpy(spell_type,(book == TV_MAGIC_BOOK) ? "spell" : "prayer");

    strcpy(action, (book == TV_MAGIC_BOOK) ? "cast" : "recite");

    CItem *i_ptr;



    // Must not be warrior

    if (!book) {

        msg_print("You cannot use any magic.");

        return;

    }





    // Restrict item choices to the proper kind of books

    item_tester_tval = book;



    // Get an item (from inven or floor)

    if (!get_item(&item, "Use which book? ", GI_INVEN | GI_FLOOR)) {

        if (item == -2) msg_format("You have no %s books!", spell_type);
        
        reset_timer();
        return;

    }



    // Get the item (in the pack)

    i_ptr = gi_i_ptr;



    // Access the item's sval

    sval = i_ptr->GetSval();





    // Choose a spell/prayer

    if (!get_spell(&j, action, sval, TRUE)) {

        if (j == -2) msg_format("You don't know any %ss in that book.",

            spell_type);
            
        reset_timer();  
        return;

    }





    // Set the spell but DON'T cast

    current_spell_type = 1;

    current_spell = j;
    reset_timer();
}

