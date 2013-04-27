// File: load.c
// Purpose: support for loading savefiles -BEN-

#include "utumno.h"


/*
 * This file is responsible for loading all savefiles
 *
 * We attempt to prevent corrupt savefiles from inducing memory errors.
 *
 * Note that Angband 2.7.9 encodes "terrain features" in the savefile
 * using the old 2.7.8 method.  Angband 2.8.0 will use the same method
 * to read pre-2.8.0 savefiles, but will use a new method to save them,
 * which will only affect "save.c".
 *
 * Note that Angband 2.8.0 will use a VERY different savefile method,
 * which will use "blocks" of information which can be ignored or parsed,
 * and which will not use a silly "protection" scheme on the savefiles,
 * but which may still use some form of "checksums" to prevent the use
 * of "corrupt" savefiles, which might cause nasty weirdness.
 *
 * Note that this file should not use the random number generator, the
 * object flavors, the visual attr/char mappings, or anything else which
 * is initialized *after* or *during* the "load character" function.
 */





// Local "savefile" pointer
static FILE     *fff;

extern char savefile[1024];

/*
 * Show information on the screen, one line at a time.
 * Start at line 2, and wrap, if needed, back to line 2.
 */
static void note(char *msg)
{
    static int y = 2;

    /* Draw the message */
    box(0, y*16, 639, y*16+15, COLOR_BLACK);
    put_string(0, y*16, msg, COLOR_WHITE);

    /* Advance one line (wrap if needed) */
    if (++y >= 24) y = 2;

    /* Flush it */
    screen_refresh();

    // To the console
    console_print(msg);
}


/*
 * Hack -- determine if an item is "wearable" (or a missile)
 */
static bool wearable_p(CItem *i_ptr)
{
    /* Valid "tval" codes */
    switch (i_ptr->GetTval()) {
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        case TV_BOW:
        case TV_DIGGING:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_HELM:
        case TV_CROWN:
        case TV_SHIELD:
        case TV_CLOAK:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_LITE:
        case TV_AMULET:
        case TV_RING:
            return (TRUE);
    }

    /* Nope */
    return (FALSE);
}


/*
 * The following functions are used to load the basic building blocks
 * of savefiles.
 */

static byte sf_get(void)
{
    byte v;

    // Get a character
    v = getc(fff) & 0xFF;
    // Return the value
 
    return v;
}

static void rd_byte(byte *ip)
{
    *ip = sf_get();
}

static void rd_u16b(u16b *ip)
{
    (*ip) = sf_get();
    (*ip) |= ((u16b)(sf_get()) << 8);
}

static void rd_s16b(s16b *ip)
{
    rd_u16b((u16b*)ip);
}

static void rd_u32b(u32b *ip)
{
    (*ip) = sf_get();
    (*ip) |= ((u32b)(sf_get()) << 8);
    (*ip) |= ((u32b)(sf_get()) << 16);
    (*ip) |= ((u32b)(sf_get()) << 24);
}

static void rd_s32b(s32b *ip)
{
    rd_u32b((u32b*)ip);
}

/*
 * Hack -- read a string
 */
static void rd_string(char *str, int max)
{
    int i;

    /* Read the string */
    for (i = 0; TRUE; i++) {
        byte tmp8u;

        /* Read a byte */
        rd_byte(&tmp8u);

        /* Collect string while legal */
        if (i < max) str[i] = tmp8u;

        /* End of string */
        if (!tmp8u) break;
    }

    /* Terminate */
    str[max-1] = '\0';
}


/*
 * Read an item (2.7.0 or later)
 *
 * Note that Angband 2.7.9 introduced a new method for object "flags"
 * in which the "flags" on an object are actually extracted when they
 * are needed from the object kind, artifact index, ego-item index,
 * and two special "xtra" fields which are used to encode any "extra"
 * power of certain ego-items.  This had the side effect that items
 * imported from pre-2.7.9 savefiles will lose any "extra" powers they
 * may have had, and also, all "uncursed" items will become "cursed"
 * again, including Calris, even if it is being worn at the time.  As
 * a complete hack, items which are inscribed with "uncursed" will be
 * "uncursed" when imported from pre-2.7.9 savefiles.
 */
static void rd_item(CItem *i_ptr)
{
    u32b f1, f2, f3;

    s16b tmp_s16b;
    byte tmp_byte;

    CObjectKind *k_ptr;

    char note[128];


    /* Hack -- wipe */
    i_ptr->wipe();

    /* Kind */
    rd_s16b(&tmp_s16b); i_ptr->SetKIdx(tmp_s16b);

    /* Location */
    rd_s16b(&tmp_s16b); i_ptr->SetX(tmp_s16b);
    rd_s16b(&tmp_s16b); i_ptr->SetY(tmp_s16b);

    /* Special pval */
    rd_s16b(&tmp_s16b); i_ptr->SetPval(tmp_s16b);

    /* New method */
    rd_byte(&tmp_byte); i_ptr->SetDiscount(tmp_byte);
    rd_byte(&tmp_byte); i_ptr->SetNumber(tmp_byte);

    rd_byte(&tmp_byte); i_ptr->SetName1(tmp_byte);
    rd_byte(&tmp_byte); i_ptr->SetName2(tmp_byte);
    rd_s16b(&tmp_s16b); i_ptr->SetTimeout(tmp_s16b);

    rd_s16b(&tmp_s16b); i_ptr->SetToH(tmp_s16b);
    rd_s16b(&tmp_s16b); i_ptr->SetToD(tmp_s16b);
    rd_s16b(&tmp_s16b); i_ptr->SetToA(tmp_s16b);
    rd_s16b(&tmp_s16b); i_ptr->SetAC(tmp_s16b);

    rd_byte(&tmp_byte); i_ptr->SetDD(tmp_byte);
    rd_byte(&tmp_byte); i_ptr->SetDS(tmp_byte);

    rd_byte(&tmp_byte); i_ptr->SetIdent(tmp_byte);
    rd_byte(&tmp_byte); i_ptr->SetMarked(tmp_byte);

    /* Special powers */
    rd_byte(&tmp_byte); i_ptr->SetXtra1(tmp_byte);
    rd_byte(&tmp_byte); i_ptr->SetXtra2(tmp_byte);

    // Inscription
    rd_string(note, 128);

    /* Save the inscription */
    if (note[0]) i_ptr->SetNote(note);

    i_ptr->next_i_ptr = NULL;


    /* Obtain the "kind" template */
    k_ptr = i_ptr->get_k_ptr();


    /* Hack -- notice "broken" items */
    if (k_ptr->cost <= 0) i_ptr->SetIdentFlag(ID_BROKEN);


    /* Repair non "wearable" items */
    if (!wearable_p(i_ptr)) {
        /* Acquire correct fields */
        i_ptr->SetToH(k_ptr->to_h);
        i_ptr->SetToD(k_ptr->to_d);
        i_ptr->SetToA(k_ptr->to_a);

        /* Acquire correct fields */
        i_ptr->SetAC(k_ptr->ac);

        /* Paranoia */
        i_ptr->SetName1(0);
        i_ptr->SetName2(0);

        /* All done */
        return;
    }


    // Extract the flags
    i_ptr->GetFlags(&f1, &f2, &f3);

    // Paranoia
    if (i_ptr->isArtifact()) {
        // Obtain the artifact info
        artifact_type *a_ptr = i_ptr->get_a_ptr();

        // Verify that artifact
        if (!a_ptr->name) i_ptr->SetName1(0);
    }

    // Paranoia
    if (i_ptr->isEgoItem()) {
        // Obtain the ego-item info
        ego_item_type *e_ptr = i_ptr->get_e_ptr();

        // Verify that ego-item
        if (!e_ptr->name) i_ptr->SetName2(0);
    }


    // Acquire standard fields
    i_ptr->SetAC(k_ptr->ac);

    // Hack -- extract the "broken" flag
    if (i_ptr->GetPval() < 0) i_ptr->SetIdentFlag(ID_BROKEN);


    // Artifacts
    if (i_ptr->isArtifact()) {
        // Obtain the artifact info
        artifact_type *a_ptr = i_ptr->get_a_ptr();

        // Acquire new artifact "pval"
        i_ptr->SetPval(a_ptr->pval);

        // Acquire new artifact fields
        i_ptr->SetAC(a_ptr->ac);

        /* Hack -- extract the "broken" flag */
        if (!a_ptr->cost) i_ptr->SetIdentFlag(ID_BROKEN);
    }

    /* Ego items */
    if (i_ptr->isEgoItem()) {
        /* Obtain the ego-item info */
        ego_item_type *e_ptr = i_ptr->get_e_ptr();

        /* Hack -- extract the "broken" flag */
        if (!e_ptr->cost) i_ptr->SetIdentFlag(ID_BROKEN);
    }
}


/*
 * Read a monster
 */
static CMonster *rd_monster(void)
{
    byte tmp8u;
    s16b tmp_s16b;
    byte tmp_byte;
    u16b tmp_u16b;
    CMonster *m_ptr;

    // Read the monster race
    rd_s16b(&tmp_s16b);

    // Create a new monster
    m_ptr = new CMonster(tmp_s16b);

    // Read the other information
    rd_s16b(&tmp_s16b); m_ptr->SetX(tmp_s16b);
    rd_s16b(&tmp_s16b); m_ptr->SetY(tmp_s16b);
    rd_s16b(&tmp_s16b); m_ptr->SetMHP(tmp_s16b);
    rd_s16b(&tmp_s16b); m_ptr->SetCHP(tmp_s16b);
    rd_u16b(&tmp_u16b); m_ptr->SetCHPFrac(tmp_u16b);
    rd_s16b(&tmp_s16b); m_ptr->set_csleep(tmp_s16b);
    rd_s16b(&tmp_s16b); m_ptr->set_busy(tmp_s16b);
    rd_s16b(&tmp_s16b); m_ptr->set_fast(tmp_s16b);
    rd_s16b(&tmp_s16b); m_ptr->set_slow(tmp_s16b);
    rd_s16b(&tmp_s16b); m_ptr->set_confused(tmp_s16b);
    rd_s16b(&tmp_s16b); m_ptr->set_afraid(tmp_s16b);
    rd_s16b(&tmp_s16b); m_ptr->set_stun(tmp_s16b);
    rd_byte(&tmp_byte); m_ptr->set_spawned(tmp_byte);
    rd_byte(&tmp_byte); m_ptr->detect = tmp_byte;
    m_ptr->i_ptr = NULL;
    rd_byte(&tmp8u);
    rd_s16b(&tmp_s16b); m_ptr->action = tmp_s16b;

    // Return the monster
    return m_ptr;
}


/*
 * Read the monster lore
 */
static void rd_lore(int r_idx)
{
    byte tmp8u;

    CMonsterRace *r_ptr = &r_info[r_idx];

    // Count sights/deaths/kills
    rd_s16b(&r_ptr->r_sights);
    rd_s16b(&r_ptr->r_deaths);
    rd_s16b(&r_ptr->r_pkills);
    rd_s16b(&r_ptr->r_tkills);

    // Count wakes and ignores
    rd_byte(&r_ptr->r_wake);
    rd_byte(&r_ptr->r_ignore);

    /* Extra stuff */
    rd_byte(&r_ptr->r_xtra1);
    rd_byte(&r_ptr->r_xtra2);

    /* Count drops */
    rd_byte(&r_ptr->r_drop_gold);
    rd_byte(&r_ptr->r_drop_item);

    /* Count spells */
    rd_byte(&r_ptr->r_cast_inate);
    rd_byte(&r_ptr->r_cast_spell);

    /* Count blows of each type */
    rd_byte(&r_ptr->r_blows[0]);
    rd_byte(&r_ptr->r_blows[1]);
    rd_byte(&r_ptr->r_blows[2]);
    rd_byte(&r_ptr->r_blows[3]);

    /* Memorize flags */
    rd_u32b(&r_ptr->r_flags1);
    rd_u32b(&r_ptr->r_flags2);
    rd_u32b(&r_ptr->r_flags3);
    rd_u32b(&r_ptr->r_flags4);
    rd_u32b(&r_ptr->r_flags5);
    rd_u32b(&r_ptr->r_flags6);


    /* Read the "Racial" monster limit per level */
    rd_byte(&r_ptr->max_num);

    /* Later (?) */
    rd_byte(&tmp8u);
    rd_byte(&tmp8u);
    rd_byte(&tmp8u);


    /* Repair the lore flags */
    r_ptr->r_flags1 &= r_ptr->flags1;
    r_ptr->r_flags2 &= r_ptr->flags2;
    r_ptr->r_flags3 &= r_ptr->flags3;
    r_ptr->r_flags4 &= r_ptr->flags4;
    r_ptr->r_flags5 &= r_ptr->flags5;
    r_ptr->r_flags6 &= r_ptr->flags6;
}


/*
 * Read a store
 */
static errr rd_store(int n)
{
    store_type *st_ptr = &store[n];
    int j;
    byte own, num;
    CItem *i_ptr;

    /* Read the basic info */
    rd_s32b(&st_ptr->store_open);
    rd_s16b(&st_ptr->insult_cur);
    rd_byte(&own);
    rd_byte(&num);
    rd_s16b(&st_ptr->good_buy);
    rd_s16b(&st_ptr->bad_buy);

    /* Extract the owner (see above) */
    st_ptr->owner = own;

    /* Read the items */
    for (j = 0; j < num; j++) {
        // Make an item
        i_ptr = new CItem;

        // Read the item
        rd_item(i_ptr);

        // Acquire valid items
        if (st_ptr->stock_num < STORE_INVEN_MAX) {
            // Acquire the item
            st_ptr->stock[st_ptr->stock_num++] = *i_ptr;
        }

        // Kill the item
        delete i_ptr;
    }

    // Success
    return (0);
}


/*
 * Read/Write the "extra" information
 */
static void rd_extra()
{
    int i;
    byte tmp8u, tmp_byte;
    s16b tmp_s16b;
    u16b tmp_u16b;
    s32b tmp_s32b;

    /* Class/Race/Gender/Experience factor */
    rd_byte(&tmp_byte); p_ptr->SetRace(tmp_byte);
    rd_byte(&tmp_byte); p_ptr->SetClass(tmp_byte);
    rd_byte(&tmp_byte); p_ptr->SetMale(tmp_byte);

    rd_string(player_name, 32);

    rd_string(died_from, 80);

    /* Read the stat info */
    for (i = 0; i < 6; i++) {
        rd_s16b(&tmp_s16b);
        p_ptr->SetStatMax(i, tmp_s16b);
    }
    for (i = 0; i < 6; i++) {
        rd_s16b(&tmp_s16b);
        p_ptr->SetStatCur(i, tmp_s16b);
    }

    rd_s32b(&tmp_s32b); p_ptr->SetGold(tmp_s32b);

    rd_s32b(&tmp_s32b); p_ptr->SetMaxExp(tmp_s32b);
    rd_s32b(&tmp_s32b); p_ptr->SetExp(tmp_s32b);
    rd_u16b(&tmp_u16b); p_ptr->SetExpFrac(tmp_u16b);

    rd_s16b(&tmp_s16b); p_ptr->SetLev(tmp_s16b);

    rd_s16b(&tmp_s16b); p_ptr->SetMHP(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetCHP(tmp_s16b);
    rd_u16b(&tmp_u16b); p_ptr->SetCHPFrac(tmp_u16b);

    rd_s16b(&tmp_s16b); p_ptr->SetMSP(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetCSP(tmp_s16b);
    rd_u16b(&tmp_u16b); p_ptr->SetCSPFrac(tmp_u16b);

    rd_s16b(&tmp_s16b); p_ptr->SetMaxPlv(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetMaxDlv(tmp_s16b);

    /* Read the flags */
    rd_s16b(&tmp_s16b); p_ptr->SetBlind(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetParalyzed(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetConfused(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetFood(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetBusy(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetFast(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetSlow(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetAfraid(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetCut(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetStun(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetPoisoned(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetProtevil(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetShadowform(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetHero(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetSHero(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetShield(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetBlessed(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetTimInvis(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetWordRecall(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->set_see_infra(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetTimInfra(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetOpposeFire(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetOpposeCold(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetOpposeAcid(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetOpposeElec(tmp_s16b);
    rd_s16b(&tmp_s16b); p_ptr->SetOpposePois(tmp_s16b);

    rd_byte(&tmp_byte); p_ptr->SetConfusing(tmp_byte);


    /* Hack -- the two "special seeds" */
    rd_u32b(&seed_flavor);
    rd_u32b(&seed_town);


    /* Special stuff */
    rd_u16b(&panic_save);
    rd_u16b(&total_winner);
    rd_byte(&tmp8u); p_ptr->SetNoScore(tmp8u);
    rd_s16b(&tmp_s16b); p_ptr->action = tmp_s16b;
    rd_s16b(&tmp_s16b); p_ptr->last_move = tmp_s16b;


    /* Read "death" */
    rd_byte(&tmp8u); death = tmp8u;

    /* Current turn */
    rd_s32b(&game_turn);
}


/*
 * Read the player inventory
 *
 * Note that the inventory changed in Angband 2.7.4.  Two extra
 * pack slots were added and the equipment was rearranged.  Note
 * that these two features combine when parsing old save-files, in
 * which items from the old "aux" slot are "carried", perhaps into
 * one of the two new "inventory" slots.
 *
 * Note that the inventory is "re-sorted" later by "dungeon()".
 */
static errr rd_inventory()
{
    int slot = 0;
    CItem forge;

    /* No items */
    inven_cnt = 0;
    equip_cnt = 0;

    /* Read until done */
    while (1) {
        u16b n;

        /* Get the next item index */
        rd_u16b(&n);

        /* Nope, we reached the end */
        if (n == 0xFFFF) break;

        /* Read the item */
        rd_item(&forge);

        /* Hack -- verify item */
        if (!forge.exists()) return 53;

        /* Wield equipment */
        if (n >= INVEN_WIELD) {
            /* Structure copy */
            inventory[n] = forge;

            /* One more item */
            equip_cnt++;
        }

        /* Warning -- backpack is full */
        else if (inven_cnt == INVEN_PACK) {
            /* Oops */
            note("Too many items in the inventory!");

            /* Fail */
            return (54);
        }

        /* Carry inventory */
        else {
            /* Get a slot */
            n = slot++;

            /* Structure copy */
            inventory[n] = forge;

            /* One more item */
            inven_cnt++;
        }
    }

    /* Success */
    return (0);
}


/*
 * Initialize an arrow from the savefile
 */
CArrow::CArrow(FILE *f)
{
    fread(&x, sizeof(x), 1, fff);
    fread(&y, sizeof(y), 1, fff);
    fread(&z, sizeof(z), 1, fff);
    fread(&vx, sizeof(vx), 1, fff);
    fread(&vy, sizeof(vy), 1, fff);
    fread(&vz, sizeof(vz), 1, fff);
    rd_s16b(&chance);
    rd_s16b(&damage);
    rd_byte(&purge);
    byte tmp_byte; rd_byte(&tmp_byte);
    if (!tmp_byte) who = p_ptr;
    else {
        s16b whox, whoy;
        rd_s16b(&whox);
        rd_s16b(&whoy);
        who = cave[whoy][whox].m_ptr;
    }
    i_ptr = new CItem;
    rd_item(i_ptr);
}


/*
 * Read the dungeon (new method)
 *
 * XXX XXX XXX Angband 2.8.0 will totally change the dungeon info
 *
 * XXX XXX XXX Try to be more flexible about "too many monsters"
 *
 * XXX XXX XXX Mega-Hack -- attempt to convert pre-2.8.0 savefile
 * format into 2.8.0 internal format, by extracting the new cave
 * grid terrain feature flags.  Note that we may have to move the
 * terrain feature extractors into the "rd_item()" function.
 */
static errr rd_dungeon()
{
    int y, x;
    s16b tmp_s16b;
    CGrid *g_ptr;
    CItem *last_i_ptr = NULL;
    CMonster *m_ptr;


    /* Header info */
    rd_s16b(&dun_level);
    rd_s16b(&num_repro);
    rd_s16b(&p_ptr->loc.x);
    rd_s16b(&p_ptr->loc.y);
    rd_s16b(&cur_hgt);
    rd_s16b(&cur_wid);

    /* Read cave */
    for (x = 0; x < cur_wid; x++) {
        for (y = 0; y < cur_hgt; y++) {
            /* Get the cave pointer */
            g_ptr = &cave[y][x];

            /* Read features/flags */
            rd_s16b(&tmp_s16b); g_ptr->set_feat(tmp_s16b);
            rd_u16b(&g_ptr->flags);
            rd_byte(&g_ptr->variant);
        }
    }

    /* Read the dungeon items */
    for (;;) {
        CItem *i_ptr;
        byte tmp_byte;

        // Another item?
        rd_byte(&tmp_byte);
        if (!tmp_byte) break;

        // Allocate an item
        i_ptr = new CItem;
        
        /* Read the item */
        rd_item(i_ptr);


        /* Access the item location */
        g_ptr = i_ptr->get_g_ptr();


        /* Skip dead objects */
        if (!i_ptr->exists()) continue;


        // Mark the location or put together the linked list
        if (tmp_byte == 1) {
            g_ptr->i_ptr = i_ptr;
        }
        else {
            last_i_ptr->next_i_ptr = i_ptr;
        }

        // Keep last object
        last_i_ptr = i_ptr;
    }



    /* Read the monsters */
    m_ptr = NULL;
    for (;;) {
        byte tmp_byte;

        // Another monster?
        rd_byte(&tmp_byte);
        if (!tmp_byte) break;

        if (tmp_byte == 1) {
            CMonsterRace *r_ptr;

            // Read the monster
            m_ptr = rd_monster();

            // Access grid
            g_ptr = m_ptr->get_g_ptr();

            // Access race
            r_ptr = m_ptr->get_r_ptr();


            // Mark the location
            g_ptr->m_ptr = m_ptr;

            // Count XXX XXX XXX
            r_ptr->cur_num++;
        }
        else {
            // Do we have a current monster?
            if (m_ptr) {
                // Allocate an item
                CItem *i_ptr = new CItem;

                // Read the item
                rd_item(i_ptr);

                // Hook it in
                i_ptr->next_i_ptr = m_ptr->i_ptr;
                m_ptr->i_ptr = i_ptr;
            }

            // Break on error
            else {
                quit("unlinked monster inventory object");
            }
        }
    }


    // Read the arrows
    for (int i = 0; i < MAX_PROJECTILES; i++) {
        byte tmp_byte;

        // Read the byte
        rd_byte(&tmp_byte);
        if (!tmp_byte) break;

        // Create the arrow
        add_projectile(new CArrow(fff));
    }


    /* Read the dungeon */
    character_dungeon = TRUE;


    /* Success */
    return (0);
}


/*
 * Actually read the savefile
 */
static errr rd_savefile_new_aux(void)
{
    int i;

    byte tmp8u, tmp_byte;
    u16b tmp16u;


    /* Mention the savefile version */
    if (sf_extra == 0) {
        note(format("Loading a v%d.%da%d savefile...", sf_major, sf_minor, sf_patch));
    }
    else if (sf_extra == 1) {
        note(format("Loading a v%d.%db%d savefile...", sf_major, sf_minor, sf_patch));
    }
    else if (sf_extra == 2) {
        note(format("Loading a v%d.%d savefile...", sf_major, sf_minor));
    }
    else {
        note(format("Loading an unusual savefile: %d.%d.%d (%d)", sf_major, sf_minor,
            sf_patch, sf_extra));
    }


    /* Strip the version bytes */
    for (i = 0; i < 4; i++) rd_byte(&tmp8u);


    /* Read the extra stuff */
    rd_extra();


    // Wizard mode
    rd_byte(&tmp_byte); wizard = tmp_byte;


    /* Monster Memory */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > MAX_R_IDX) {
        note(format("Too many (%u) monster races!", tmp16u));
        return 21;
    }

    /* Read the available records */
    for (i = 0; i < tmp16u; i++) {
        CMonsterRace *r_ptr;

        /* Read the lore */
        rd_lore(i);

        /* Access that monster */
        r_ptr = &r_info[i];
    }


    /* Item Memory */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > MAX_K_IDX) {
        note(format("Too many (%u) object kinds!", tmp16u));
        return (22);
    }

    /* Read the object memory */
    for (i = 0; i < tmp16u; i++) {
        byte tmp8u;

        CObjectKind *k_ptr = &k_info[i];

        rd_byte(&tmp8u);

        k_ptr->aware = (tmp8u & 0x01) ? TRUE : FALSE;
        k_ptr->tried = (tmp8u & 0x02) ? TRUE : FALSE;
    }


    /* Load the Quests */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > 8) {
        note(format("Too many (%u) quests!", tmp16u));
        return (23);
    }

    /* Load the Quests */
    for (i = 0; i < tmp16u; i++) {
        rd_byte(&tmp8u);
        q_list[i].level = tmp8u;
        rd_byte(&tmp8u);
        rd_byte(&tmp8u);
        rd_byte(&tmp8u);
    }


    /* Load the Artifacts */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > MAX_A_IDX) {
        note(format("Too many (%u) artifacts!", tmp16u));
        return (24);
    }

    /* Read the artifact flags */
    for (i = 0; i < tmp16u; i++)
    {
        rd_byte(&tmp8u);
        a_info[i].cur_num = tmp8u;
        rd_byte(&tmp8u);
        rd_byte(&tmp8u);
        rd_byte(&tmp8u);
    }


    /* Read the player_hp array */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > PY_MAX_LEVEL) {
        note(format("Too many (%u) hitpoint entries!", tmp16u));
        return 25;
    }

    /* Read the player_hp array */
    for (i = 0; i < tmp16u; i++) {
        rd_s16b(&p_ptr->player_hp[i]);
    }


    /* Important -- Initialize the race/class */
    rp_ptr = &race_info[p_ptr->GetRace()];
    cp_ptr = &class_info[p_ptr->GetClass()];

    /* Important -- Choose the magic info */
    mp_ptr = &magic_info[p_ptr->GetClass()];


    /* Read spell info */
    for (i = 0; i < 64; i++) {
        rd_byte(&spell_learned[i]);
        rd_byte(&spell_worked[i]);
        rd_byte(&spell_forgotten[i]);
    }

    for (i = 0; i < 64; i++) {
        rd_byte(&spell_order[i]);
    }


    // Read the inventory
    if (rd_inventory()) {
        note("Unable to read inventory");
        return (21);
    }


    // Read the stores
    rd_u16b(&tmp16u);
    for (i = 0; i < tmp16u; i++) {
        if (rd_store(i)) return (22);
    }


    // I'm not dead yet...
    if (!death) {
        /* Dead players have no dungeon */
        note("Restoring Dungeon...");
        if (rd_dungeon()) {
            note("Error reading dungeon data");
            return 34;
        }
    }


    /* Success */
    return (0);
}


/*
 * Actually read the savefile
 */
errr rd_savefile_new(void)
{
    errr err;

    /* The savefile is a binary file */
    fff = my_fopen(savefile, "rb");

    /* Paranoia */
    if (!fff) return (-1);

    /* Call the sub-function */
    err = rd_savefile_new_aux();

    /* Check for errors */
    if (ferror(fff)) err = -1;

    /* Close the file */
    fclose(fff);

    /* Result */
    return (err);
}


void get_player_summary(player_summary *ps)
{
    int i, j;
    char filename[80];
    byte tmp_byte;
    FILE *f = fopen("dat/save/save.dat", "rt");
    int slot_taken;

    for (i = 0; i < 10; i++) {
        fscanf(f, "%d", &slot_taken);
        if (!slot_taken) continue;
        sprintf(filename, "dat/save/save%d", i);
        fff = fopen(filename, "rb");
        for (j = 0; j < 4; j++) rd_byte(&tmp_byte);
        rd_byte(&ps[i].prace);
        rd_byte(&ps[i].pclass);
        rd_byte(&ps[i].male);
        rd_string(ps[i].name, 32);
        fclose(fff);
    }
    fclose(f);
}
