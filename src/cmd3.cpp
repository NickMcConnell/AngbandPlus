// File: cmd3.cpp

// Purpose: Inventory commands



/*

 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke

 *

 * This software may be copied and distributed for educational, research, and

 * not for profit purposes provided that this copyright and statement are

 * included in all such copies.

 */



#include "utumno.h"







/*

 * Move an item from equipment list to pack

 * Note that only one item at a time can be wielded per slot.

 * Note that taking off an item when "full" will cause that item

 * to fall to the ground.

 */

static void inven_takeoff(int item, int amt)

{

    int posn;

    CItem *i_ptr, tmp_obj;

    char *act, i_name[80];





    /* Get the item to take off */

    i_ptr = &inventory[item];



    /* Paranoia */

    if (amt <= 0) return;



    /* Verify */

    if (amt > i_ptr->GetNumber()) amt = i_ptr->GetNumber();



    /* Make a copy to carry */

    tmp_obj = *i_ptr;

    tmp_obj.SetNumber(amt);



    /* What are we "doing" with the object */

    if (amt < i_ptr->GetNumber()) {

        act = "Took off";

    }

    else if (item == INVEN_WIELD) {

        act = "Was wielding";

    }

    else if (item == INVEN_BOW) {

        act = "Was shooting with";

    }

    else if (item == INVEN_ARROW) {

        act = "Was shooting";

    }

    else if (item == INVEN_LITE) {

        act = "Light source was";

    }

    else {

        act = "Was wearing";

    }



    /* Carry the object, saving the slot it went in */

    posn = inven_carry(&tmp_obj);



    /* Describe the result */

    i_ptr->object_desc(i_name, TRUE, 3);



    /* Message */

    msg_format("%^s %s (%c).", act, i_name, index_to_label(posn));



    /* Delete (part of) it */

    inven_item_increase(item, -amt);

    inven_item_optimize(item);

}









/*

 * Drops (some of) an item from inventory to "near" the current location

 */

static void inven_drop(int item, int amt)

{

    CItem *i_ptr, tmp_obj;

    char *act, i_name[80];





    /* Access the slot to be dropped */

    i_ptr = &inventory[item];



    /* Error check */

    if (amt <= 0) return;



    /* Not too many */

    if (amt > i_ptr->GetNumber()) amt = i_ptr->GetNumber();



    /* Nothing done? */

    if (amt <= 0) return;



    /* Make a "fake" object */

    tmp_obj = *i_ptr;

    tmp_obj.SetNumber(amt);



    /* What are we "doing" with the object */

    if (amt < i_ptr->GetNumber()) {

        act = "Dropped";

    }

    else if (item == INVEN_WIELD) {

        act = "Was wielding";

    }

    else if (item == INVEN_BOW) {

        act = "Was shooting with";

    }

    else if (item == INVEN_ARROW) {

        act = "Was shooting";

    }

    else if (item == INVEN_LITE) {

        act = "Light source was";

    }

    else if (item >= INVEN_WIELD) {

        act = "Was wearing";

    }

    else {

        act = "Dropped";

    }



    /* Message */

    tmp_obj.object_desc(i_name, TRUE, 3);



    // Message 

    msg_format("%^s %s (%c).", act, i_name, index_to_label(item));



    /* Drop it (carefully) near the player */

    drop_near(&tmp_obj, 0, p_ptr->GetY(), p_ptr->GetX());



    /* Decrease the item, optimize. */

    inven_item_increase(item, -amt);

    inven_item_describe(item);

    inven_item_optimize(item);

}







/*

 * Draw inventory

 */

void draw_inven_stuff(void)

{

    char out_val[160];

    s16b weight = p_ptr->GetTotalWeight();



    /* Hack -- show empty slots */

    item_tester_full = TRUE;



    /* Display the inventory */

    show_inven();



    /* Hack -- hide empty slots */

    item_tester_full = FALSE;



    /* Build a header */

    sprintf(out_val, "Inventory (carrying %d.%d pounds).", weight / 10, weight % 10);

    box(0, 0, 639, 15, COLOR_BLACK);

    put_string(0, 0, out_val, COLOR_WHITE);

}







/*

 * Draw equipment

 */

void draw_equip_stuff(void)

{

    char out_val[160];

    s16b weight = p_ptr->GetTotalWeight();





    /* Hack -- show empty slots */

    item_tester_full = TRUE;



    // Display the equipment

    show_equip();



    // Hack -- undo the hack above

    item_tester_full = FALSE;



    // Build a header

    sprintf(out_val, "Equipment (carrying %d.%d pounds).", weight / 10, weight % 10);

    box(0, 0, 639, 15, COLOR_BLACK);

    put_string(0, 0, out_val, COLOR_WHITE);

}





/*

 * The "wearable" tester

 */

static bool item_tester_hook_wear(CItem *i_ptr)

{

    /* Check for a usable slot */

    if (i_ptr->WieldSlot() >= INVEN_WIELD) return TRUE;



    /* Assume not wearable */

    return FALSE;

}





/*

 * Wield or wear a single item from the pack or floor

 */

bool do_cmd_wield(void)

{

    int item, slot;

    CItem tmp_obj, *i_ptr;

    char *act, i_name[80];





    if (p_ptr->isBusy()) return FALSE;



    // Restrict the choices

    item_tester_hook = item_tester_hook_wear;



    // Get an item from inven or floor

    if (!get_item(&item, "Wear/Wield which item? ", GI_INVEN | GI_FLOOR)) {

        if (item == -2) msg_print("You have nothing you can wear or wield.");
        
        reset_timer();
        
        return TRUE;

    }



    // Get the item (in the pack)

    i_ptr = gi_i_ptr;





    // Check the slot

    slot = i_ptr->WieldSlot();



    // Prevent wielding into a cursed slot

    if (inventory[slot].isCursed()) {

        // Describe it

        inventory[slot].object_desc(i_name, FALSE, 0);



        // Message

        msg_format("The %s you are %s appears to be cursed.",

            i_name, describe_use(slot));



        // Cancel the command

        reset_timer();         
        return TRUE;

    }



    // Verify potential overflow

    if ((inven_cnt >= INVEN_PACK) && ((item < 0) || i_ptr->isPlural())) {

        // Verify with the player

        if (other_query_flag &&

            !get_check("Your pack may overflow. Continue?"))
            {
            reset_timer();
            return TRUE;
            }

    }





    // Take a turn

    p_ptr->DrainEnergy(100);



    // Get a copy of the object to wield

    tmp_obj = *i_ptr;

    tmp_obj.SetNumber(1);



    // If wield slot is the quiver slot, wield all the items of that type

    if (slot == INVEN_ARROW) tmp_obj.SetNumber(i_ptr->GetNumber());



    // Decrease the item (from the pack)

    if (item >= 0) {

        inven_item_increase(item, -tmp_obj.GetNumber());

        inven_item_optimize(item);

    }



    // Decrease the item (from the floor)

    else {

        floor_item_increase(i_ptr, -tmp_obj.GetNumber());

        floor_item_optimize(i_ptr);

    }



    // Access the wield slot

    i_ptr = &inventory[slot];



    // Take off the "entire" item if one is there

    if (inventory[slot].exists()) inven_takeoff(slot, 255);



    /*** Could make procedure "inven_wield()" ***/



    // Wear the new stuff

    *i_ptr = tmp_obj;



    // Increment the equip counter by hand

    equip_cnt++;



    // Where is the item now

    if (slot == INVEN_WIELD) {

        act = "You are wielding";

    }

    else if (slot == INVEN_BOW) {

        act = "You are shooting with";

    }

    else if (slot == INVEN_ARROW) {

        act = "You are shooting";

    }

    else if (slot == INVEN_LITE) {

        act = "Your light source is";

    }

    else {

        act = "You are wearing";

    }



    // Describe the result 

    i_ptr->object_desc(i_name, TRUE, 3);



    // Message

    msg_format("%^s %s (%c).", act, i_name, index_to_label(slot));



    // Cursed!

    if (i_ptr->isCursed()) {

        // Warn the player

        msg_print("It feels deathly cold!");



        /* Note the curse */

        i_ptr->SetIdentFlag(ID_SENSE);

    }



    // Recalculate bonuses, torch, mana

    p_ptr->set_update(p_ptr->get_update() | PU_BONUS | PU_TORCH | PU_MANA);



    // Did something
    
    reset_timer();        
    return TRUE;

}





/*

 * Take off an item

 */

bool do_cmd_takeoff(void)

{

    int item;

    CItem *i_ptr;



    if (p_ptr->isBusy()) return FALSE;



    // Verify potential overflow

    if (inven_cnt >= INVEN_PACK) {

        // Verify with the player

        if (other_query_flag &&

            !get_check("Your pack may overflow. Continue?"))
            {
            reset_timer();
            return TRUE;
            }

    }



    // Get an item (from equip)

    if (!get_item(&item, "Take off which item? ", GI_EQUIP)) {

        if (item == -2) msg_print("You are not wearing anything to take off.");
        
        reset_timer();
        
        return TRUE;

    }



    // Get the item

    i_ptr = gi_i_ptr;



    // Take a partial turn

    p_ptr->DrainEnergy(50);



    // Item is cursed

    if (i_ptr->isCursed()) {

        // Oops

        msg_print("Hmmm, it seems to be cursed.");



        // Nope
        
        reset_timer();
        return TRUE;

    }



    // Take off the item 

    inven_takeoff(item, 255);



    // Did something
    
    reset_timer();
    return TRUE;

}





/*

 * Drop an item

 */

bool do_cmd_drop(void)

{

    int item, amt = 1;

    CItem *i_ptr;



    if (p_ptr->isBusy()) return FALSE;



    /* Get an item (from equip or inven) */

    if (!get_item(&item, "Drop which item? ", GI_INVEN)) {

        if (item == -2) msg_print("You have nothing to drop.");

        reset_timer();
        
        return TRUE;

    }



    /* Get the item (in the pack) */

    i_ptr = gi_i_ptr;





    /* See how many items */

    if (i_ptr->isPlural()) {

        /* Get a quantity */

        amt = get_quantity(NULL, i_ptr->GetNumber());



        /* Allow user abort */

        if (amt <= 0)
           {
           reset_timer();
           return TRUE;
           }

    }





    /* Take a partial turn */

    p_ptr->DrainEnergy(50);



    /* Drop (some of) the item */

    inven_drop(item, amt);



    // Did something
    reset_timer();
    return TRUE;

}







/*

 * Destroy an item

 */

bool do_cmd_destroy(void)

{

    int item, amt = 1, old_number;

    CItem *i_ptr;

    char i_name[80], out_val[160];

    char feel[80];



    if (p_ptr->isBusy()) return FALSE;



    /* Get an item (from inven or floor) */

    if (!get_item(&item, "Destroy which item? ", GI_INVEN | GI_FLOOR)) {

        if (item == -2) msg_print("You have nothing to destroy.");
        
        reset_timer();
        
        return TRUE;

    }



    /* Get the item (in the pack) */

    i_ptr = gi_i_ptr;





    /* See how many items */

    if (i_ptr->isPlural()) {

        /* Get a quantity */

        amt = get_quantity(NULL, i_ptr->GetNumber());



        /* Allow user abort */

        if (amt <= 0)
           {
           reset_timer();
           return TRUE;
           }

    }





    /* Describe the object */

    old_number = i_ptr->GetNumber();

    i_ptr->SetNumber(amt);

    i_ptr->object_desc(i_name, TRUE, 3);

    i_ptr->SetNumber(old_number);



    /* Make a verification */

    sprintf(out_val, "Really destroy %s?", i_name);

    if (!get_check(out_val))
        {
        reset_timer();
        return TRUE;
        }




    /* Take a turn */

    p_ptr->DrainEnergy(100);



    /* Artifacts cannot be destroyed */

    if (i_ptr->isArtifact()) {

        strcpy(feel, "special");



        /* Message */

        msg_format("You cannot destroy %s.", i_name);



        /* Hack -- Handle icky artifacts */

        if (i_ptr->isCursed() || i_ptr->isBroken()) 

		strcpy(feel ,"terrible");



        /* Hack -- inscribe the artifact */

        i_ptr->SetNote(feel);



        /* We have "felt" it (again) */

        i_ptr->SetIdentFlag(ID_SENSE);



        /* Combine the pack */

        p_ptr->set_notice(p_ptr->get_notice() | PN_COMBINE);



        /* Done */
        reset_timer();
        return TRUE;

    }



    // Message

    msg_format("You destroy %s.", i_name);



    // Eliminate the item (from the pack)

    if (item >= 0) {

        inven_item_increase(item, -amt);

        inven_item_describe(item);

        inven_item_optimize(item);

    }



    // Eliminate the item (from the floor)

    else {

        floor_item_increase(i_ptr, -amt);

        floor_item_describe(i_ptr);

        floor_item_optimize(i_ptr);

    }



    // Did something
    reset_timer();
    return TRUE;

}





/*

 * Observe an item which has been *identify*-ed

 */

void do_cmd_observe(void)

{

    int item;

    CItem *i_ptr;

    char i_name[80];





    /* Get an item (from equip or inven or floor) */

    if (!get_item(&item, "Examine which item? ", GI_EQUIP | GI_INVEN | GI_FLOOR)) {

        if (item == -2) msg_print("You have nothing to examine.");

        return;

    }



    /* Get the item (in the pack) */

    i_ptr = gi_i_ptr;





    /* Require full knowledge */

    if (!i_ptr->TestIdentFlag(ID_MENTAL)) {

        msg_print("You have no special knowledge about that item.");
        
        return;

    }





    /* Description */

    i_ptr->object_desc(i_name, TRUE, 3);



    /* Describe it fully */

    if (!identify_fully_aux(i_ptr)) msg_print("You see nothing special.");
    
}







/*

 * The table of "symbol info" -- each entry is a string of the form

 * "X:desc" where "X" is the trigger, and "desc" is the "info".

 */

static char *ident_info[] = {

    ",:Mushroom patch",

    "A:Angel",

    "B:Bat/Bird",

    "C:Canine",

    "D:Ancient Dragon/Wyrm",

    "E:Elemental",

    "F:Dragon Fly",

    "G:Ghost",

    "H:Hybrid",

    "I:Insect",

    "J:Snake",

    "K:unused",

    "L:Lich",

    "M:Multi-Headed Reptile",

    "N:unused",

    "O:Ogre",

    "P:Giant Humanoid",

    "Q:Quylthulg (Pulsing Flesh Mound)",

    "R:Reptile/Amphibian",

    "S:Spider/Scorpion/Tick",

    "T:Troll",

    "U:Major Demon",

    "V:Vampire",

    "W:Wight/Wraith/etc",

    "X:Xorn/Xaren/etc",

    "Y:Yeti",

    "Z:Zephyr Hound",

    "a:Ant",

    "b:Beetle",

    "c:Centipede",

    "d:Dragon",

    "e:Floating Eye",

    "f:Feline",

    "g:Golem",

    "h:Hobbit/Elf/Dwarf",

    "i:Icky Thing",

    "j:Jelly",

    "k:Kobold",

    "l:unused",

    "m:Mold",

    "n:Naga",

    "o:Orc",

    "p:Person/Human",

    "q:Quadruped",

    "r:Rodent",

    "s:Skeleton",

    "t:Townsperson",

    "u:Minor Demon",

    "v:Vortex",

    "w:Worm/Worm-Mass",

    "x:unused",

    "y:Yeek",

    "z:Zombie/Mummy",

    NULL

};







/*

 * Sorting hook -- Comp function -- see below

 *

 * We use "u" to point to array of monster indexes,

 * and "v" to select the type of sorting to perform on "u".

 */

static bool ang_sort_comp_hook(vptr u, vptr v, int a, int b)

{

    u16b *who = (u16b*)(u);



    u16b *why = (u16b*)(v);



    int w1 = who[a];

    int w2 = who[b];



    int z1, z2;





    /* Sort by player kills */

    if (*why >= 4) {

        /* Extract player kills */

        z1 = r_info[w1].r_pkills;

        z2 = r_info[w2].r_pkills;



        /* Compare player kills */

        if (z1 < z2) return TRUE;

        if (z1 > z2) return FALSE;

    }





    /* Sort by total kills */

    if (*why >= 3) {

        /* Extract total kills */

        z1 = r_info[w1].r_tkills;

        z2 = r_info[w2].r_tkills;



        /* Compare total kills */

        if (z1 < z2) return TRUE;

        if (z1 > z2) return FALSE;

    }





    /* Sort by monster level */

    if (*why >= 2) {

        /* Extract levels */

        z1 = r_info[w1].level;

        z2 = r_info[w2].level;



        /* Compare levels */

        if (z1 < z2) return TRUE;

        if (z1 > z2) return FALSE;

    }





    /* Sort by monster experience */

    if (*why >= 1) {

        /* Extract experience */

        z1 = r_info[w1].mexp;

        z2 = r_info[w2].mexp;



        /* Compare experience */

        if (z1 < z2) return TRUE;

        if (z1 > z2) return FALSE;

    }





    /* Compare indexes */

    return (w1 <= w2);

}





/*

 * Sorting hook -- Swap function -- see below

 *

 * We use "u" to point to array of monster indexes,

 * and "v" to select the type of sorting to perform.

 */

static void ang_sort_swap_hook(vptr u, vptr v, int a, int b)

{

    u16b *who = (u16b*)(u);



    u16b holder;



    /* XXX XXX */

    v = v ? v : 0;



    /* Swap */

    holder = who[a];

    who[a] = who[b];

    who[b] = holder;

}







/*

 * Identify a character, allow recall of monsters

 *

 * Several "special" responses recall "mulitple" monsters:

 *   ^A (all monsters)

 *   ^U (all unique monsters)

 *   ^N (all non-unique monsters)

 *

 * The responses may be sorted in several ways, see below.

 */

void do_cmd_query_symbol(void)

{

    int i, n, r_idx;

    char sym, query, buf[128];

// -- MV: Commenting out unused variable

//	bool all = FALSE;

    bool uniq = FALSE, norm = FALSE;

    byte *screen;



    u16b why = 0;

    u16b who[MAX_R_IDX];





    /* Get a character, or abort */

    if (!get_com("Enter character to be identified: ", &sym)) return;



    /* Find that character info, and describe it */

    for (i = 0; ident_info[i]; ++i)

    {

        if (sym == ident_info[i][0]) break;

    }



    /* Describe */

    if (ident_info[i]) {

        sprintf(buf, "%c - %s.", sym, ident_info[i] + 2);

    }

    else {

        sprintf(buf, "%c - %s.", sym, "Unknown Symbol");

    }



    /* Display the result */

    box(0, 0, 639, 15, COLOR_BLACK);

    put_string(0, 0, buf, COLOR_WHITE);

    screen_refresh();





    /* Collect matching monsters */

    for (n = 0, i = 1; i < MAX_R_IDX; i++) {

        CMonsterRace *r_ptr = &r_info[i];



        /* Nothing to recall and not in wiz mode */

        if (!r_ptr->r_sights && !wizard) continue;



        /* Require non-unique monsters if needed */

        if (norm && (r_ptr->flags1 & RF1_UNIQUE)) continue;



        /* Require unique monsters if needed */

        if (uniq && !(r_ptr->flags1 & RF1_UNIQUE)) continue;



        /* Collect "appropriate" monsters */

        //: There are no longer any racial chars

        // if (all || (r_ptr->r_char == sym)) who[n++] = i;

    }



    /* Nothing to recall */

    if (!n) return;





    /* Prompt XXX XXX XXX */

    put_text(40*8, 0, "Recall details? (k/p/y/n): ", COLOR_WHITE, FONT_BOLD);



    /* Query */

    screen_refresh();

    query = scan_inkey();

    query = convert(query, get_shift(), get_capslock());



    /* Restore */

    box(0, 0, 639, 15, COLOR_BLACK);

    put_string(0, 0, buf, COLOR_WHITE);







    /* Sort by kills (and level) */

    if (query == 'k') {

        why = 4;

        query = 'y';

    }



    /* Sort by level */

    if (query == 'p') {

        why = 2;

        query = 'y';

    }



    /* Catch "escape" */

    if (query != 'y') return;





    /* Sort if needed */

    if (why) {

        /* Select the sort method */

        ang_sort_comp = ang_sort_comp_hook;

        ang_sort_swap = ang_sort_swap_hook;



        /* Sort the array */

        ang_sort(who, &why, n);

    }





    /* Start at the end */

    i = n - 1;



    /* Scan the monster memory. */

    while ((0 <= i) && (i <= n - 1)) {

        /* Validate index */

        for (i = i % n; i < 0; i += n) ;



        /* Extract a race */

        r_idx = who[i];



        // Hack -- update stuff

        update_stuff();



        /* Recall on screen */

        screen = save_screen();

        screen_roff(who[i]);



        /* Command */

        screen_refresh();

        restore_screen(screen);

        delete[] screen;

        query = scan_inkey();



        /* Stop scanning */

        if (query == KEY_ESCAPE) break;



        /* Move to the "next" or "prev" monster */

        i = i + ((query == KEY_MINUS) ? 1 : -1);

    }





    /* Re-display the identity */

    box(0, 0, 639, 15, COLOR_BLACK);

    put_string(0, 0, buf, COLOR_WHITE);

}





// Test monsters

void do_cmd_test_monsters(void)

{

    int r_idx = 1;

    CMonsterRace *r_ptr;

    char c;



    for (;;) {

        r_ptr = &r_info[r_idx];

        blank_screen(COLOR_BLACK);

        put_string(0, 0, "Monster Test Center", COLOR_WHITE);

        put_string(0, 32, format("Current race index: %d", r_idx), COLOR_WHITE);

        put_string(0, 48, format("Monster name: %s", r_name + r_ptr->name), COLOR_WHITE);

        put_string(0, 80, format("Dungeon level: %d", r_ptr->level), COLOR_WHITE);

        draw_tile(9*32, 8*16, "features/floor00", FALSE, "base", 0, 0);

        draw_tile(10*32, 9*16, "features/floor00", FALSE, "base", 0, 0);

        draw_tile(11*32, 10*16, "features/floor00", FALSE, "base", 0, 0);

        draw_tile(8*32, 9*16, "features/floor00", FALSE, "base", 0, 0);

        draw_tile(9*32, 10*16, "features/floor00", FALSE, "base", 0, 0);

        draw_tile(10*32, 11*16, "features/floor00", FALSE, "base", 0, 0);

        draw_tile(7*32, 10*16, "features/floor00", FALSE, "base", 0, 0);

        draw_tile(8*32, 11*16, "features/floor00", FALSE, "base", 0, 0);

        draw_tile(9*32, 12*16, "features/floor00", FALSE, "base", 0, 0);

        draw_tile(9*32, 10*16, "hilite", FALSE, "base", 0, 0);

        draw_tile_idx(9*32, 10*16, r_ptr->tile, FALSE, "base", 0, 0);

        screen_refresh();

        c = scan_inkey();

        switch (c) {

            case KEY_ESCAPE:

		reset_timer();

                return;

            case KEY_MINUS:

                if (r_idx > 1) r_idx--;

                break;

            case KEY_EQUAL:

                if (r_idx < MAX_R_IDX-1) r_idx++;

                break;

        }

    }

}







// Test objects

void do_cmd_test_objects(void)

{

    int k_idx = 1;

    CObjectKind *k_ptr;

    CItem lobj;

    char c, buf[120];



    for (;;) {

	while(k_info[k_idx].name==NULL) k_idx++;

        k_ptr = &k_info[k_idx];

        blank_screen(COLOR_BLACK);

        put_string(0, 0, "Object Test Center", COLOR_WHITE);

        put_string(0, 32, format("Current kind index: %d", k_idx), COLOR_WHITE);

        lobj.invcopy(k_idx);

        lobj.object_desc(buf, FALSE, 0);

        put_string(0, 48, format("Object name: %s", buf), COLOR_WHITE);

        if (!streq(buf, "(nothing)")) {

            draw_tile(9*32, 8*16, "features/floor00", FALSE, "base", 0, 0);

            draw_tile(10*32, 9*16, "features/floor00", FALSE, "base", 0, 0);

            draw_tile(11*32, 10*16, "features/floor00", FALSE, "base", 0, 0);

            draw_tile(8*32, 9*16, "features/floor00", FALSE, "base", 0, 0);

            draw_tile(9*32, 10*16, "features/floor00", FALSE, "base", 0, 0);

            draw_tile(10*32, 11*16, "features/floor00", FALSE, "base", 0, 0);

            draw_tile(7*32, 10*16, "features/floor00", FALSE, "base", 0, 0);

            draw_tile(8*32, 11*16, "features/floor00", FALSE, "base", 0, 0);

            draw_tile(9*32, 12*16, "features/floor00", FALSE, "base", 0, 0);

            draw_tile(9*32, 10*16, "hilite", FALSE, "base", 0, 0);

            draw_tile_idx(9*32, 10*16, k_ptr->tile, FALSE, "base", 0, 0);

        }

        screen_refresh();

        c = scan_inkey();

        switch (c) {

            case KEY_ESCAPE:

                reset_timer();

                return;

            case KEY_MINUS:

                if (k_idx > 1) k_idx--;

                break;

            case KEY_EQUAL:

                if (k_idx < MAX_K_IDX-1) k_idx++;

                break;

        }

    }

}

