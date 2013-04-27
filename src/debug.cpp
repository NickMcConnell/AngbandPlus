// File: debug.cpp

// Purpose: Debug commands



/*

 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke

 *

 * This software may be copied and distributed for educational, research, and

 * not for profit purposes provided that this copyright and statement are

 * included in all such copies.

 */



#include "utumno.h"







/*

 * Output a long int in binary format.

 */

static void prt_binary(u32b flags, int row, int col)

{

    int         i;

    u32b        bitmask;



    box(col*8, row*16, col*8+32*8-1, row*16+15, COLOR_BLACK);



    /* Scan the flags */

    for (i = bitmask = 1; i <= 32; i++, bitmask *= 2) {

        /* Dump set bits */

        if (flags & bitmask) {

            put_string((col++)*8, row*16, "*", COLOR_BLUE);

        }



        /* Dump unset bits */

        else {

            put_string((col++)*8, row*16, "-", COLOR_WHITE);

        }

    }

}





/*

 * Change various "permanent" player variables.

 */

void do_cmd_wiz_change(void)

{

    int i, tmp_int;

    long tmp_long;

    char tmp_val[160], ppp[80];



    // Query the stats

    for (i = 0; i < 6; i++) {

        // Prompt

        sprintf(ppp, "%s (3-118): ", stat_names[i]);



        // Default

        sprintf(tmp_val, "%d", p_ptr->GetStatMax(i));



        // Query

        if (!get_string(ppp, tmp_val, 3)) return;



        // Extract

        tmp_int = atoi(tmp_val);



        // Verify

        if (tmp_int > 18+100) tmp_int = 18+100;

        else if (tmp_int < 3) tmp_int = 3;



        // Save it

        p_ptr->SetStatCur(i, tmp_int);

        p_ptr->SetStatMax(i, tmp_int);

    }





    /* Default */

    sprintf(tmp_val, "%ld", (long)(p_ptr->GetMaxExp()));



    /* Query */

    if (!get_string("Experience: ", tmp_val, 9)) return;



    /* Extract */

    tmp_long = atol(tmp_val);



    /* Verify */

    if (tmp_long < 0) tmp_long = 0L;



    /* Save */

    p_ptr->SetMaxExp(tmp_long);



    /* Update */

    p_ptr->check_experience();

    p_ptr->set_update(p_ptr->get_update() | PU_BONUS);

}





/*

 * Wizard routines for creating objects         -RAK-   

 * And for manipulating them!                   -Bernd-

 *

 * This has been rewritten to make the whole procedure

 * of debugging objects much easier and more comfortable.

 *

 * The following functions are meant to play with objects:

 * Create, modify, and more.

 * The original functions were by RAK.

 * The function to show an item's debug information was written

 * by David Reeve Sward <sward+@CMU.EDU>.

 *                             Bernd (wiebelt@mathematik.hu-berlin.de)

 *

 * Here are the low-level functions

 * - wiz_display_item()

 *     display an item's debug-info

 * - wiz_create_itemtype()

 *     specify tval and sval (type and subtype of object)

 * - wiz_tweak_item()

 *     specify pval, +AC, +tohit, +todam

 *     Note that the wizard can leave this function anytime,

 *     thus accepting the default-values for the remaining values.

 *     pval comes first now, since it is most important.

 * - wiz_reroll_item()

 *     apply some magic to the item or turn it into an artifact.

 * - wiz_quantity_item()

 *     change the quantity of an item, but be sane about it.

 *

 * And now the high-level functions

 * - do_cmd_wiz_play()

 *     play with an existing object

 * - wiz_create_item()

 *     create a new object

 *

 * Note -- You do not have to specify "pval" and other item-properties

 * directly. Just apply magic until you are satisfied with the item.

 *

 * Note -- For some items (such as wands, staffs, some rings, etc), you

 * must apply magic, or you will get "broken" or "uncharged" objects.

 *

 * Note -- Redefining artifacts via "do_cmd_wiz_play()" may destroy

 * the artifact.  Be careful.

 *

 * Hack -- this function will allow you to create multiple artifacts.

 * This "feature" may induce crashes or other nasty effects.

 */



/*

 * Just display an item's properties (debug-info)

 * Originally by David Reeve Sward <sward+@CMU.EDU>

 * Verbose item flags by -Bernd-

 */

void wiz_display_item(CItem *i_ptr)

{

    int         i, j = 13;



    u32b        f1, f2, f3;



    char        buf[256];





    /* Extract the flags */

    i_ptr->GetFlags(&f1, &f2, &f3);



    /* Clear the screen */

    for (i = 1; i <= 23; i++) {

        box((j-2)*8, i*16, 639, i*16+15, COLOR_BLACK);

    }



    /* Describe fully */

    i_ptr->object_desc_store(buf, TRUE, 3);



    put_string(j*8, 2*16, buf, COLOR_WHITE);



    put_string(j*8, 4*16,

        format("kind = %-5d  level = %-4d  tval = %-5d  sval = %-5d",

        i_ptr->GetKIdx(), k_info[i_ptr->GetKIdx()].level,

        i_ptr->GetTval(), i_ptr->GetSval()), COLOR_WHITE);



    put_string(j*8, 5*16,

        format("number = %-3d  wgt = %-6d  ac = %-5d    damage = %dd%d",

        i_ptr->GetNumber(), i_ptr->GetWeight(),

        i_ptr->GetAC(), i_ptr->GetDD(), i_ptr->GetDS()), COLOR_WHITE);



    put_string(j*8, 6*16,

        format("pval = %-5d  toac = %-5d  tohit = %-4d  todam = %-4d",

        i_ptr->GetPval(), i_ptr->GetToA(), i_ptr->GetToH(),

        i_ptr->GetToD()), COLOR_WHITE);



    put_string(j*8, 7*16,

        format("name1 = %-4d  name2 = %-4d  cost = %d",

            i_ptr->GetName1(), i_ptr->GetName2(), i_ptr->GetValue()),

        COLOR_WHITE);



    put_string(j*8, 8*16,

        format("ident = %04x  timeout = %-d",

        i_ptr->GetIdent(), i_ptr->GetTimeout()), COLOR_WHITE);



    put_string(j*8, 10*16, "+------------FLAGS1------------+", COLOR_WHITE);

    put_string(j*8, 11*16, "AFFECT..........SLAY......BRAND.", COLOR_WHITE);

    put_string(j*8, 12*16, "                ae      x q aefc", COLOR_WHITE);

    put_string(j*8, 13*16, "siwdcc  ssidsa  nvudotgdd u clio", COLOR_WHITE);

    put_string(j*8, 14*16, "tnieoh  trnipt  iinmrrnrr a ierl", COLOR_WHITE);

    put_string(j*8, 15*16, "rtsxna..lcfgdk..mldncltgg.k.dced", COLOR_WHITE);

    prt_binary(f1, 16, j);



    put_string(j*8, 17*16, "+------------FLAGS2------------+", COLOR_WHITE);

    put_string(j*8, 18*16, "SUST....IMMUN.RESIST............", COLOR_WHITE);

    put_string(j*8, 19*16, "        aefcp psaefcp ldbc sn   ", COLOR_WHITE);

    put_string(j*8, 20*16, "siwdcc  clioo atclioo ialoshtncd", COLOR_WHITE);

    put_string(j*8, 21*16, "tnieoh  ierli raierli trnnnrhehi", COLOR_WHITE);

    put_string(j*8, 22*16, "rtsxna..dceds.atdceds.ekdfddrxss", COLOR_WHITE);

    prt_binary(f2, 23, j);



    put_string((j+32)*8, 10*16, "+------------FLAGS3------------+", COLOR_WHITE);

    put_string((j+32)*8, 11*16, "        ehsi  st    iiiiadta  hp", COLOR_WHITE);

    put_string((j+32)*8, 12*16, "        aihnf ee    ggggcregb vr", COLOR_WHITE);

    put_string((j+32)*8, 13*16, "        sdose eld   nnnntalrl ym", COLOR_WHITE);

    put_string((j+32)*8, 14*16, "        yewta ieirmsrrrriieaeccc", COLOR_WHITE);

    put_string((j+32)*8, 15*16, "        ktmatlnpgeihaefcvnpvsuuu", COLOR_WHITE);

    put_string((j+32)*8, 16*16, "        nyoahivaeggoclioaeoasrrr", COLOR_WHITE);

    put_string((j+32)*8, 17*16, "        opdretitsehtierltxrtesss", COLOR_WHITE);

    put_string((j+32)*8, 18*16, "        westreshtntsdcedeptedeee", COLOR_WHITE);

    prt_binary(f3, 19, j+32);

}





/*

 * A structure to hold a tval and its description

 */

typedef struct tval_desc {

    int tval;

    char *desc;

} tval_desc;



/*

 * A list of tvals and their textual names

 */

static tval_desc tvals[] = {

    { TV_SWORD,             "Sword"                },

    { TV_POLEARM,           "Polearm"              },

    { TV_HAFTED,            "Hafted Weapon"        },

    { TV_BOW,               "Bow"                  },

    { TV_ARROW,             "Arrows"               },

    { TV_BOLT,              "Bolts"                },

    { TV_SHOT,              "Shots"                },

    { TV_SHIELD,            "Shield"               },

    { TV_CROWN,             "Crown"                },

    { TV_HELM,              "Helm"                 },

    { TV_GLOVES,            "Gloves"               },

    { TV_BOOTS,             "Boots"                },

    { TV_CLOAK,             "Cloak"                },

    { TV_DRAG_ARMOR,        "Dragon Scale Mail"    },

    { TV_HARD_ARMOR,        "Hard Armor"           },

    { TV_SOFT_ARMOR,        "Soft Armor"           },

    { TV_RING,              "Ring"                 },

    { TV_AMULET,            "Amulet"               },

    { TV_LITE,              "Lite"                 },

    { TV_POTION,            "Potion"               },

    { TV_SCROLL,            "Scroll"               },

    { TV_WAND,              "Wand"                 },

    { TV_STAFF,             "Staff"                },

    { TV_ROD,               "Rod"                  },

    { TV_PRAYER_BOOK,       "Priest Book"          },

    { TV_MAGIC_BOOK,        "Magic Book"           },

    { TV_DIGGING,           "Digger"               },

    { TV_CHEST,             "Chest"                },

    { TV_FOOD,              "Food"                 },

    { TV_FLASK,             "Flask"                },

    { TV_NOTHING,           NULL                   }

};





/*

 * Strip an "object name" into a buffer

 */

static void strip_name(char *buf, int k_idx)

{

    char *t;



    CObjectKind *k_ptr = &k_info[k_idx];



    char *str = k_ptr->name;





    /* Skip past leading characters */

    while ((*str == ' ') || (*str == '&')) str++;



    /* Copy useful chars */

    for (t = buf; *str; str++) {

        if (*str != '~') *t++ = *str;

    }



    /* Terminate the new name */

    *t = '\0';

}





/*

 * Hack -- title for each column

 */

static char column_title[3] = { 'a', 'A', '0' };





/*

 * Specify tval and sval (type and subtype of object) originally

 * by RAK, heavily modified by -Bernd-

 *

 * This function returns the k_idx of an object type, or zero if failed

 *

 * List up to 50 choices in three columns

 */

static int wiz_create_itemtype(void)

{

    int i, num, max_num, col, row, tval, choice[60];

    char ch, buf[160], *tval_desc;





    /* Clear the screen */

    blank_screen(COLOR_BLACK);



    /* Print all tval's and their descriptions */

    for (num = 0; (num < 60) && tvals[num].tval; num++) {

        row = 2 + (num % 20);

        col = 30 * (num / 20);

        ch = column_title[num/20] + (num%20);

        put_string(col*8, row*16, format("[%c] %s", ch, tvals[num].desc),

            COLOR_WHITE);

    }



    /* Need to know the maximal possible tval_index */

    max_num = num;



    /* Choose! */

    if (!get_com("Get what type of object? ", &ch)) return (0);



    /* Analyze choice */

    num = -1;

    if ((ch >= column_title[0]) && (ch < column_title[0] + 20)) {

        num = ch - column_title[0];

    }

    if ((ch >= column_title[1]) && (ch < column_title[1] + 20)) num = ch - column_title[1] + 20;

    if ((ch >= column_title[2]) && (ch < column_title[2] + 10)) num = ch - column_title[2] + 40;



    /* Bail out if choice is illegal */

    if ((num < 0) || (num >= max_num)) return (0);



    /* Base object type chosen, fill in tval */

    tval = tvals[num].tval;

    tval_desc = tvals[num].desc;





    /*** And now we go for k_idx ***/



    /* Clear the screen */

    blank_screen(COLOR_BLACK);



    /* We have to search the whole itemlist. */

    for (num = 0, i = 1; (num < 60) && (i < MAX_K_IDX); i++) {

        CObjectKind *k_ptr = &k_info[i];



        /* Analyze matching items */

        if (k_ptr->tval == tval) {

            /* Hack -- Skip instant artifacts */

            if (k_ptr->flags3 & TR3_INSTA_ART) continue;



            /* Prepare it */

            row = 2 + (num % 20);

            col = 30 * (num / 20);

            ch = column_title[num/20] + (num%20);



            /* Acquire the "name" of object "i" */

            strip_name(buf, i);



            /* Print it */

            put_string(col*8, row*16, format("[%c] %s", ch, buf), COLOR_WHITE);



            /* Remember the object index */

            choice[num++] = i;

        }

    }



    /* Me need to know the maximal possible remembered object_index */

    max_num = num;



    /* Choose! */

    if (!get_com(format("What Kind of %s? ", tval_desc), &ch)) return (0);



    /* Analyze choice */

    num = -1;

    if ((ch >= column_title[0]) && (ch < column_title[0] + 20)) num = ch - column_title[0];

    if ((ch >= column_title[1]) && (ch < column_title[1] + 20)) num = ch - column_title[1] + 20;

    if ((ch >= column_title[2]) && (ch < column_title[2] + 10)) num = ch - column_title[2] + 40;



    /* Bail out if choice is "illegal" */

    if ((num < 0) || (num >= max_num)) return (0);



    /* And return successful */
    
    return (choice[num]);

}





/*

 * Tweak an item

 */

static void wiz_tweak_item(CItem *i_ptr)

{

    char *p, tmp_val[80];





    /* Hack -- leave artifacts alone */

    if (i_ptr->isArtifact()) return;



    p = "Enter new 'pval' setting: ";

    sprintf(tmp_val, "%d", i_ptr->GetPval());

    if (!get_string(p, tmp_val, 5)) return;

    i_ptr->SetPval(atoi(tmp_val));

    wiz_display_item(i_ptr);



    p = "Enter new 'to_a' setting: ";

    sprintf(tmp_val, "%d", i_ptr->GetToA());

    if (!get_string(p, tmp_val, 5)) return;

    i_ptr->SetToA(atoi(tmp_val));

    wiz_display_item(i_ptr);



    p = "Enter new 'to_h' setting: ";

    sprintf(tmp_val, "%d", i_ptr->GetToH());

    if (!get_string(p, tmp_val, 5)) return;

    i_ptr->SetToH(atoi(tmp_val));

    wiz_display_item(i_ptr);



    p = "Enter new 'to_d' setting: ";

    sprintf(tmp_val, "%d", i_ptr->GetToD());

    if (!get_string(p, tmp_val, 5)) return;

    i_ptr->SetToD(atoi(tmp_val));

    wiz_display_item(i_ptr);

}





/*

 * Apply magic to an item or turn it into an artifact. -Bernd-

 */

static void wiz_reroll_item(CItem *i_ptr)

{

    CItem mod_item;

    char ch;

    bool changed = FALSE;





    /* Hack -- leave artifacts alone */

    if (i_ptr->isArtifact()) return;





    /* Copy the item to be modified. */

    mod_item = *i_ptr;



    /* Main loop. Ask for magification and artifactification */

    while (TRUE) {

        /* Display full item debug information */

        wiz_display_item(&mod_item);



        /* Ask wizard what to do. */

        if (!get_com("[a]ccept, [n]ormal, [g]ood, [e]xcellent? ", &ch)) {

            changed = FALSE;

            break;

        }



        /* Create/change it! */

        if (ch == 'A' || ch == 'a') {

            changed = TRUE;

            break;

        }



        /* Apply normal magic, but first clear object */

        else if (ch == 'n' || ch == 'N') {

            mod_item.invcopy(i_ptr->GetKIdx());

            mod_item.apply_magic(dun_level, 0);

        }



        /* Apply good magic, but first clear object */

        else if (ch == 'g' || ch == 'g') {

            mod_item.invcopy(i_ptr->GetKIdx());

            mod_item.apply_magic(dun_level, AM_FORCE_GOOD);

        }



        /* Apply great magic, but first clear object */

        else if (ch == 'e' || ch == 'e') {

            mod_item.invcopy(i_ptr->GetKIdx());

            mod_item.apply_magic(dun_level, AM_FORCE_GOOD | AM_FORCE_GREAT);

        }

    }



    /* Notice change */

    if (changed) {

        /* Apply changes */

        *i_ptr = mod_item;



        /* Recalculate bonuses */

        p_ptr->set_update(p_ptr->get_update() | PU_BONUS);



        /* Combine / Reorder the pack (later) */

        p_ptr->set_notice(p_ptr->get_notice() | PN_COMBINE | PN_REORDER);

    }

}







/*

 * Change the quantity of a the item

 */

static void wiz_quantity_item(CItem *i_ptr)

{

    int tmp_int;

    char tmp_val[100];





    /* Never duplicate artifacts */

    if (i_ptr->isArtifact()) return;





    /* Default */

    sprintf(tmp_val, "%d", i_ptr->GetNumber());



    /* Query */

    if (get_string("Quantity: ", tmp_val, 2))

    {

        /* Extract */

        tmp_int = atoi(tmp_val);



        /* Paranoia */

        if (tmp_int < 1) tmp_int = 1;

        if (tmp_int > 99) tmp_int = 99;



        /* Accept modifications */

        i_ptr->SetNumber(tmp_int);

    }

}







/*

 * Play with an item. Options include:

 *   - Reroll item (via wiz_reroll_item)

 *   - Change properties (via wiz_tweak_item)

 *   - Change the number of items (via wiz_quantity_item)

 */

void do_cmd_wiz_play(void)

{

    int item;

    CItem *i_ptr, forge;

    char ch;

    bool changed;

    byte *screen;





    /* Get an item (from equip or inven) */

    if (!get_item(&item, "Play with which object? ", GI_EQUIP | GI_INVEN | GI_FLOOR)) {

        if (item == -2) msg_print("You have nothing to play with.");

        return;

    }



    /* Get the item (in the pack) */

    i_ptr = gi_i_ptr;

    



    /* The item was not changed */

    changed = FALSE;





    /* Icky */

    character_icky = TRUE;



    /* Save the screen */

    screen = save_screen();





    /* Get a copy of the item */

    forge = *i_ptr;



    /* The main loop */

    while (TRUE) {

        /* Display the item */

        wiz_display_item(&forge);



        /* Get choice */

        if (!get_com("[a]ccept [r]eroll [t]weak [q]uantity? ", &ch)) {

            changed = FALSE;

            break;

        }



        if (ch == 'a' || ch == 'A') {

            changed = TRUE;

            break;

        }



        if (ch == 'r' || ch == 'R') {

            wiz_reroll_item(&forge);

        }



        if (ch == 't' || ch == 'T') {

            wiz_tweak_item(&forge);

        }



        if (ch == 'q' || ch == 'Q') {

            wiz_quantity_item(&forge);

        }

    }





    /* Restore the screen */

    restore_screen(screen);

    delete[] screen;



    /* Not Icky */

    character_icky = FALSE;





    // Accept change

    if (changed) {

        /* Message */

        msg_print("Changes accepted.");



        /* Change */

        *i_ptr = forge;



        /* Recalculate bonuses */

        p_ptr->set_update(p_ptr->get_update() | PU_BONUS);



        /* Combine / Reorder the pack (later) */

        p_ptr->set_notice(p_ptr->get_notice() | PN_COMBINE | PN_REORDER);

    }



    // Ignore change

    else {

        msg_print("Changes ignored.");

    }

}





/*

 * Wizard routine for creating objects          -RAK-   

 * Heavily modified to allow magification and artifactification  -Bernd-

 *

 * Note that wizards cannot create objects on top of other objects.

 *

 * Hack -- this routine always makes a "dungeon object", and applies

 * magic to it, and attempts to decline cursed items.

 */

void wiz_create_item()

{

    CItem forge;

    int k_idx;

    byte *screen;





    // Icky

    character_icky = TRUE;



    // Save the screen

    screen = save_screen();



    // Get object base type

    k_idx = wiz_create_itemtype();



    // Restore the screen

    restore_screen(screen);

    delete[] screen;



    // Not Icky

    character_icky = FALSE;





    // Return if failed

    if (!k_idx) return;



    // Create the item

    forge.invcopy(k_idx);



    // Apply magic (no messages, no artifacts, no force good/great)

    wizard = FALSE;

    forge.apply_magic(dun_level, 0);

    wizard = TRUE;



    /* Drop the object from heaven */

    drop_near(&forge, -1, p_ptr->GetY(), p_ptr->GetX());

}





/*

 * Cure everything instantly

 */

void do_cmd_wiz_cure_all()

{

    // Remove curses

    remove_all_curse();



    // Restore stats

    res_stat(STAT_STR);

    res_stat(STAT_INT);

    res_stat(STAT_WIS);

    res_stat(STAT_CON);

    res_stat(STAT_DEX);

    res_stat(STAT_CHR);



    // Restore the level

    p_ptr->restore_level();



    // Heal the player

    p_ptr->SetCHP(p_ptr->GetMHP());

    p_ptr->SetCHPFrac(0);



    // Restore mana

    p_ptr->SetCSP(p_ptr->GetMSP());

    p_ptr->SetCSPFrac(0);



    // Cure stuff

    p_ptr->mod_blind(0);

    p_ptr->mod_confused(0);

    p_ptr->mod_poisoned(0);

    p_ptr->mod_afraid(0);

    p_ptr->mod_paralyzed(0);

    p_ptr->mod_stun(0);

    p_ptr->mod_cut(0);

    p_ptr->mod_slow(0);



    // No longer hungry or gorged

    p_ptr->mod_food(PY_FOOD_MAX - 1);

}





/*

 * Go to any level

 */

void do_cmd_wiz_jump(void)

{

    char ppp[80], tmp_val[160];

    int depth;



    /* Prompt */

    sprintf(ppp, "Jump to level (0-%d): ", MAX_DEPTH-1);



    /* Default */

    sprintf(tmp_val, "%d", dun_level);



    /* Ask for a level */

    if (!get_string(ppp, tmp_val, 10)) return;



    /* Extract request */

    depth = atoi(tmp_val);



    /* Paranoia */

    if (depth < 0) depth = 0;



    /* Paranoia */

    if (depth > MAX_DEPTH - 1) depth = MAX_DEPTH - 1;



    /* Accept request */

    msg_format("You jump to dungeon level %d.", depth);



    /* Change level */

    dun_level = depth;

    new_level_flag = TRUE;

}





/*

 * Become aware of a lot of objects

 */

void do_cmd_wiz_learn(void)

{

    int i;



    /* Scan every object */

    for (i = 1; i < MAX_K_IDX; i++) {

        /* Induce awareness */

        CItem inv;

        inv.invcopy(i);

        inv.object_aware();

    }

}





/*

 * Summon some creatures

 */

void do_cmd_wiz_summon(void)

{

    summon_specific(p_ptr->GetY(), p_ptr->GetX(), dun_level, 0);

}





/*

 * Summon a creature of the specified type

 *

 * XXX XXX XXX This function is rather dangerous

 */

void do_cmd_wiz_named(void)

{

    int i, x, y, r_idx;

    char ppp[80], tmp_val[160];



    /* Prompt */

    sprintf(ppp, "Monster to summon (1-%d): ", MAX_R_IDX-1);



    /* Default */

    sprintf(tmp_val, "1");



    /* Ask for a level */

    if (!get_string(ppp, tmp_val, 10)) return;



    /* Extract request */

    r_idx = atoi(tmp_val);



    /* Paranoia */

    if (!r_idx) return;



    /* Prevent illegal monsters */

    if (r_idx >= MAX_R_IDX) return;



    /* Try 10 times */

    for (i = 0; i < 10; i++) {

        int d = 1;



        /* Pick a location */

        scatter(&y, &x, p_ptr->GetY(), p_ptr->GetX(), d, 0);



        /* Require empty grids */

        if (!empty_grid_bold(y, x)) continue;



        /* Place it (allow groups) */

        if (place_monster_aux(y, x, r_idx, PM_ALLOW_GROUP | PM_FORCE_SLEEP)) break;

    }

}







/*

 * Hack -- Delete all nearby monsters

 */

void do_cmd_wiz_zap(void)

{

    CMonster *m_ptr;

    int x, y;



    // Go through every tile

    for (x = 0; x < cur_wid; x++) {

        for (y = 0; y < cur_hgt; y++) {

            m_ptr = cave[y][x].m_ptr;



            // If there is no monster, skip

            if (!m_ptr) continue;



                /* Delete nearby monsters */

            if (m_ptr->get_cdis() <= MAX_SIGHT) delete_monster(m_ptr);

        }

    }

}



