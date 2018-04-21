/* File: load.c */

/*
 * Copyright (c) 1997 Ben Harrison, and others
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: support for loading savefiles -BEN- */

#include "angband.h"

#include <assert.h>

/*
 * Hack -- Show information on the screen, one line at a time.
 *
 * Avoid the top two lines, to avoid interference with "msg_print()".
 */
static void note(cptr msg)
{
    static int y = 2;

    /* Draw the message */
    prt(msg, y, 0);

    /* Advance one line (wrap if needed) */
    if (++y >= 24) y = 2;

    /* Flush it */
    Term_fresh();
}

void rd_item(savefile_ptr file, object_type *o_ptr)
{
    object_kind *k_ptr;
    char         buf[128];

    object_wipe(o_ptr);

    o_ptr->k_idx = savefile_read_s16b(file);
    k_ptr = &k_info[o_ptr->k_idx];
    o_ptr->tval = k_ptr->tval;
    o_ptr->sval = k_ptr->sval;

    o_ptr->iy = savefile_read_byte(file);
    o_ptr->ix = savefile_read_byte(file);
    o_ptr->weight = savefile_read_s16b(file);

    o_ptr->number = 1;

    for (;;)
    {
        byte code = savefile_read_byte(file);
        if (code == SAVE_ITEM_DONE)
            break;

        switch (code)
        {
        case SAVE_ITEM_PVAL:
            o_ptr->pval = savefile_read_s16b(file);
            break;
        case SAVE_ITEM_DISCOUNT:
            o_ptr->discount = savefile_read_byte(file);
            break;
        case SAVE_ITEM_NUMBER:
            o_ptr->number = savefile_read_byte(file);
            break;
        case SAVE_ITEM_NAME1:
            o_ptr->name1 = savefile_read_s16b(file);
            break;
        case SAVE_ITEM_NAME2:
            o_ptr->name2 = savefile_read_s16b(file);
            break;
        case SAVE_ITEM_NAME3:
            o_ptr->name3 = savefile_read_s16b(file);
            break;
        case SAVE_ITEM_TIMEOUT:
            o_ptr->timeout = savefile_read_s16b(file);
            break;
        case SAVE_ITEM_COMBAT:
            o_ptr->to_h = savefile_read_s16b(file);
            o_ptr->to_d = savefile_read_s16b(file);
            break;
        case SAVE_ITEM_ARMOR:
            o_ptr->to_a = savefile_read_s16b(file);
            o_ptr->ac = savefile_read_s16b(file);
            break;
        case SAVE_ITEM_DAMAGE_DICE:
            o_ptr->dd = savefile_read_byte(file);
            o_ptr->ds = savefile_read_byte(file);
            break;
        case SAVE_ITEM_MULT:
            o_ptr->mult = savefile_read_s16b(file);
            break;
        case SAVE_ITEM_IDENT:
            o_ptr->ident = savefile_read_byte(file);
            break;
        case SAVE_ITEM_MARKED_BYTE:
            o_ptr->marked = savefile_read_byte(file);
            break;
        case SAVE_ITEM_MARKED:
            o_ptr->marked = savefile_read_u32b(file);
            break;
        case SAVE_ITEM_ART_FLAGS_0:
            o_ptr->flags[0] = savefile_read_u32b(file);
            break;
        case SAVE_ITEM_ART_FLAGS_1:
            o_ptr->flags[1] = savefile_read_u32b(file);
            break;
        case SAVE_ITEM_ART_FLAGS_2:
            o_ptr->flags[2] = savefile_read_u32b(file);
            break;
        case SAVE_ITEM_ART_FLAGS_3:
            o_ptr->flags[3] = savefile_read_u32b(file);
            break;
        case SAVE_ITEM_ART_FLAGS_4:
            o_ptr->flags[4] = savefile_read_u32b(file);
            break;
        case SAVE_ITEM_ART_FLAGS_5:
            o_ptr->flags[5] = savefile_read_u32b(file);
            break;
        case SAVE_ITEM_CURSE_FLAGS:
            o_ptr->curse_flags = savefile_read_u32b(file);
            break;
        case SAVE_ITEM_KNOWN_FLAGS_0:
            o_ptr->known_flags[0] = savefile_read_u32b(file);
            break;
        case SAVE_ITEM_KNOWN_FLAGS_1:
            o_ptr->known_flags[1] = savefile_read_u32b(file);
            break;
        case SAVE_ITEM_KNOWN_FLAGS_2:
            o_ptr->known_flags[2] = savefile_read_u32b(file);
            break;
        case SAVE_ITEM_KNOWN_FLAGS_3:
            o_ptr->known_flags[3] = savefile_read_u32b(file);
            break;
        case SAVE_ITEM_KNOWN_FLAGS_4:
            o_ptr->known_flags[4] = savefile_read_u32b(file);
            break;
        case SAVE_ITEM_KNOWN_FLAGS_5:
            o_ptr->known_flags[5] = savefile_read_u32b(file);
            break;
        case SAVE_ITEM_KNOWN_CURSE_FLAGS:
            o_ptr->known_curse_flags = savefile_read_u32b(file);
            break;
        case SAVE_ITEM_RUNE_FLAGS:
            o_ptr->rune = savefile_read_u32b(file);
            break;
        case SAVE_ITEM_HELD_M_IDX:
            o_ptr->held_m_idx = savefile_read_s16b(file);
            break;
        case SAVE_ITEM_XTRA1:
            o_ptr->xtra1 = savefile_read_byte(file);
            break;
        case SAVE_ITEM_XTRA2:
            o_ptr->xtra2 = savefile_read_byte(file);
            break;
        case SAVE_ITEM_XTRA3:
            o_ptr->xtra3 = savefile_read_byte(file);
            break;
        case SAVE_ITEM_XTRA4:
            o_ptr->xtra4 = savefile_read_s16b(file);
            break;
        case SAVE_ITEM_XTRA5_OLD:
            o_ptr->xtra5 = savefile_read_s16b(file);
            break;
        case SAVE_ITEM_XTRA5:
            o_ptr->xtra5 = savefile_read_s32b(file);
            break;
        case SAVE_ITEM_FEELING:
            o_ptr->feeling = savefile_read_byte(file);
            break;
        case SAVE_ITEM_INSCRIPTION:
            savefile_read_cptr(file, buf, sizeof(buf));
            o_ptr->inscription = quark_add(buf);
            break;
        case SAVE_ITEM_ART_NAME:
            savefile_read_cptr(file, buf, sizeof(buf));
            o_ptr->art_name = quark_add(buf);
            break;
        case SAVE_ITEM_ACTIVATION:
            o_ptr->activation.type = savefile_read_s16b(file);
            o_ptr->activation.power = savefile_read_byte(file);
            o_ptr->activation.difficulty = savefile_read_byte(file);
            o_ptr->activation.cost = savefile_read_s16b(file);
            o_ptr->activation.extra = savefile_read_s16b(file);
            break;
        /* default:
            TODO: Report an error back to the load routine!!*/
        }
    }
    if (object_is_device(o_ptr))
        add_flag(o_ptr->flags, OF_ACTIVATE);
}


static void rd_monster(savefile_ptr file, monster_type *m_ptr)
{
    char buf[128];
    int  which;

    WIPE(m_ptr, monster_type);

    m_ptr->r_idx = savefile_read_s16b(file);
    m_ptr->ap_r_idx = m_ptr->r_idx;
    m_ptr->fy = savefile_read_byte(file);
    m_ptr->fx = savefile_read_byte(file);
    m_ptr->hp = savefile_read_s16b(file);
    m_ptr->maxhp = savefile_read_s16b(file);
    m_ptr->max_maxhp = savefile_read_s16b(file);
    m_ptr->mspeed = savefile_read_byte(file);
    m_ptr->energy_need = savefile_read_s16b(file);

    for (;;)
    {
        byte code = savefile_read_byte(file);
        if (code == SAVE_MON_DONE)
            break;

        switch (code)
        {
        case SAVE_MON_AP_R_IDX:
            m_ptr->ap_r_idx = savefile_read_s16b(file);
            break;
        case SAVE_MON_SUB_ALIGN:
            m_ptr->sub_align = savefile_read_byte(file);
            break;
        case SAVE_MON_TIMER:
            which = savefile_read_byte(file);
            m_ptr->mtimed[which] = savefile_read_s16b(file);
            break;
        case SAVE_MON_TARGET_Y:
            m_ptr->target_y = savefile_read_s16b(file);
            break;
        case SAVE_MON_TARGET_X:
            m_ptr->target_x = savefile_read_s16b(file);
            break;
        case SAVE_MON_SMART:
            m_ptr->smart = savefile_read_u32b(file);
            break;
        case SAVE_MON_EXP:
            m_ptr->exp = savefile_read_u32b(file);
            break;
        case SAVE_MON_MFLAG2:
            m_ptr->mflag2 = savefile_read_u32b(file);
            break;
        case SAVE_MON_NICKNAME:
            savefile_read_cptr(file, buf, sizeof(buf));
            m_ptr->nickname = quark_add(buf);
            break;
        case SAVE_MON_PARENT:
            m_ptr->parent_m_idx = savefile_read_s16b(file);
            break;
        case SAVE_MON_PACK_IDX:
            m_ptr->pack_idx = savefile_read_s16b(file);
            break;
        case SAVE_MON_AC:
            m_ptr->ac_adj = savefile_read_s16b(file);
            break;
        case SAVE_MON_MELEE:
            m_ptr->melee_adj = savefile_read_s16b(file);
            break;
        case SAVE_MON_DROP_CT:
            m_ptr->drop_ct = savefile_read_byte(file);
            break;
        case SAVE_MON_STOLEN_CT:
            m_ptr->stolen_ct = savefile_read_byte(file);
            break;
        case SAVE_MON_SUMMON_CT:
            m_ptr->summon_ct = savefile_read_u16b(file);
            break;
        case SAVE_MON_EGO_WHIP:
            m_ptr->ego_whip_ct = savefile_read_byte(file);
            m_ptr->ego_whip_pow = savefile_read_byte(file);
            break;
        case SAVE_MON_ANTI_MAGIC:
            m_ptr->anti_magic_ct = savefile_read_byte(file);
            break;
        case SAVE_MON_PEXP:
            m_ptr->pexp = savefile_read_s32b(file);
            break;
        case SAVE_MON_PARALYZED:
            m_ptr->paralyzed = savefile_read_s16b(file);
            break;
        case SAVE_MON_ANGER_CT:
            m_ptr->anger_ct = savefile_read_byte(file);
            break;
        /* default:
            TODO: Report an error back to the load routine!!*/
        }
    }
}

static void rd_lore(savefile_ptr file, int r_idx)
{
    bool pact = FALSE;

    monster_race *r_ptr = &r_info[r_idx];

    r_ptr->r_sights = savefile_read_s16b(file);
    r_ptr->r_deaths = savefile_read_s16b(file);
    r_ptr->r_pkills = savefile_read_s16b(file);
    r_ptr->r_akills = savefile_read_s16b(file);
    r_ptr->r_skills = savefile_read_s16b(file);
    r_ptr->r_tkills = savefile_read_s16b(file);
    r_ptr->r_wake = savefile_read_byte(file);
    r_ptr->r_ignore = savefile_read_byte(file);
    r_ptr->r_xtra1 = savefile_read_byte(file);
    r_ptr->r_xtra2 = savefile_read_byte(file);
    r_ptr->r_drop_gold = savefile_read_byte(file);
    r_ptr->r_drop_item = savefile_read_byte(file);
    r_ptr->r_cast_spell = savefile_read_byte(file);
    r_ptr->r_blows[0] = savefile_read_byte(file);
    r_ptr->r_blows[1] = savefile_read_byte(file);
    r_ptr->r_blows[2] = savefile_read_byte(file);
    r_ptr->r_blows[3] = savefile_read_byte(file);
    r_ptr->r_flags1 = savefile_read_u32b(file);
    r_ptr->r_flags2 = savefile_read_u32b(file);
    r_ptr->r_flags3 = savefile_read_u32b(file);
    r_ptr->r_flags4 = savefile_read_u32b(file);
    r_ptr->r_flags5 = savefile_read_u32b(file);
    r_ptr->r_flags6 = savefile_read_u32b(file);
    r_ptr->r_flagsr = savefile_read_u32b(file);
    r_ptr->max_num = savefile_read_byte(file);
    r_ptr->floor_id = savefile_read_s16b(file);
    r_ptr->stolen_ct = savefile_read_byte(file);

    if (r_ptr->r_flagsr & (RFR_PACT_MONSTER)) pact = TRUE;

    /* Repair the lore flags */
    r_ptr->r_flags1 &= r_ptr->flags1;
    r_ptr->r_flags2 &= r_ptr->flags2;
    r_ptr->r_flags3 &= r_ptr->flags3;
    r_ptr->r_flags4 &= r_ptr->flags4;
    r_ptr->r_flags5 &= r_ptr->flags5;
    r_ptr->r_flags6 &= r_ptr->flags6;
    r_ptr->r_flagsr &= r_ptr->flagsr;

    if (pact)
        r_ptr->r_flagsr |= RFR_PACT_MONSTER;
}

/*
 * Add the item "o_ptr" to the inventory of the "Home"
 *
 * In all cases, return the slot (or -1) where the object was placed
 *
 * Note that this is a hacked up version of "inven_carry()".
 *
 * Also note that it may not correctly "adapt" to "knowledge" bacoming
 * known, the player may have to pick stuff up and drop it again.
 */
static void home_carry(store_type *st_ptr, object_type *o_ptr)
{
    int                 slot;
    s32b               value;
    int     i;
    object_type *j_ptr;


    /* Check each existing item (try to combine) */
    for (slot = 0; slot < st_ptr->stock_num; slot++)
    {
        /* Get the existing item */
        j_ptr = &st_ptr->stock[slot];

        /* The home acts just like the player */
        if (object_similar(j_ptr, o_ptr))
        {
            /* Save the new number of items */
            object_absorb(j_ptr, o_ptr);

            /* All done */
            return;
        }
    }

    /* No space? */
    if (st_ptr->stock_num >= STORE_INVEN_MAX * 10) {
        return;
    }

    /* Determine the "value" of the item */
    value = obj_value(o_ptr);

    /* Check existing slots to see if we must "slide" */
    for (slot = 0; slot < st_ptr->stock_num; slot++)
    {
        if (object_sort_comp(o_ptr, value, &st_ptr->stock[slot])) break;
    }

    /* Slide the others up */
    for (i = st_ptr->stock_num; i > slot; i--)
    {
        st_ptr->stock[i] = st_ptr->stock[i-1];
    }

    /* More stuff now */
    st_ptr->stock_num++;

    /* Insert the new item */
    st_ptr->stock[slot] = *o_ptr;

    virtue_add(VIRTUE_SACRIFICE, -1);

    /* Return the location */
    return;
}

static errr rd_store(savefile_ptr file, int town_number, int store_number)
{
    store_type *st_ptr;

    int j;

    byte own;
    s16b num;

    bool sort = FALSE;

    st_ptr = &town[town_number].store[store_number];

    st_ptr->store_open = savefile_read_s32b(file);
    st_ptr->insult_cur = savefile_read_s16b(file);
    own = savefile_read_byte(file);
    num = savefile_read_s16b(file);
    st_ptr->good_buy = savefile_read_s16b(file);
    st_ptr->bad_buy = savefile_read_s16b(file);
    st_ptr->last_visit = savefile_read_s32b(file);
    st_ptr->last_lev = savefile_read_s16b(file);
    st_ptr->last_exp = savefile_read_s32b(file);

    /* Extract the owner (see above) */
    st_ptr->owner = own;

    /* Read the items */
    for (j = 0; j < num; j++)
    {
        object_type forge;
        object_type *q_ptr;

        q_ptr = &forge;

        rd_item(file, q_ptr);

        /* Acquire valid items */
        if (st_ptr->stock_num < (store_number == STORE_HOME ? (STORE_INVEN_MAX) * 10 : (store_number == STORE_MUSEUM ? (STORE_INVEN_MAX) * 50 : STORE_INVEN_MAX)))
        {
            int k;
            if (sort)
            {
                home_carry(st_ptr, q_ptr);
            }
            else
            {
                k = st_ptr->stock_num++;
                object_copy(&st_ptr->stock[k], q_ptr);
            }
        }
    }

    return 0;
}

static void rd_randomizer(savefile_ptr file)
{
    int i;
    Rand_place = savefile_read_u16b(file);

    for (i = 0; i < RAND_DEG; i++)
        Rand_state[i] = savefile_read_u32b(file);

    Rand_quick = FALSE;
}


/*
 * Read options (ignore most pre-2.8.0 options)
 *
 * Note that the normal options are now stored as a set of 256 bit flags,
 * plus a set of 256 bit masks to indicate which bit flags were defined
 * at the time the savefile was created. This will allow new options
 * to be added, and old options to be removed, at any time, without
 * hurting old savefiles.
 *
 * The window options are stored in the same way, but note that each
 * window gets 32 options, and their order is fixed by certain defines.
 */
static void rd_options(savefile_ptr file)
{
    int i, n;
    u16b c;
    u32b flag[8];
    u32b mask[8];

    delay_factor = savefile_read_byte(file);
    hitpoint_warn = savefile_read_byte(file);
    mana_warn = savefile_read_byte(file);

    /*** Cheating options ***/
    c = savefile_read_u16b(file);
    if (c & 0x0002) p_ptr->wizard = TRUE;

    cheat_peek = (c & 0x0100) ? TRUE : FALSE;
    cheat_hear = (c & 0x0200) ? TRUE : FALSE;
    cheat_room = (c & 0x0400) ? TRUE : FALSE;
    cheat_xtra = (c & 0x0800) ? TRUE : FALSE;
    cheat_live = (c & 0x2000) ? TRUE : FALSE;
    cheat_save = (c & 0x4000) ? TRUE : FALSE;

    autosave_l = savefile_read_byte(file);
    autosave_t = savefile_read_byte(file);
    autosave_freq = savefile_read_s16b(file);


    /*** Normal Options ***/
    for (n = 0; n < 8; n++) flag[n] = savefile_read_u32b(file);
    for (n = 0; n < 8; n++) mask[n] = savefile_read_u32b(file);

    for (n = 0; n < 8; n++)
    {
        for (i = 0; i < 32; i++)
        {
            if (mask[n] & (1L << i))
            {
                if (option_mask[n] & (1L << i))
                {
                    if (flag[n] & (1L << i))
                        option_flag[n] |= (1L << i);
                    else
                        option_flag[n] &= ~(1L << i);
                }
            }
        }
    }

    /* Extract the options */
    extract_option_vars();

    /*** Window Options ***/
    for (n = 0; n < 8; n++) flag[n] = savefile_read_u32b(file);
    for (n = 0; n < 8; n++) mask[n] = savefile_read_u32b(file);

    for (n = 0; n < 8; n++)
    {
        for (i = 0; i < 32; i++)
        {
            if (mask[n] & (1L << i))
            {
                if (window_mask[n] & (1L << i))
                {
                    if (flag[n] & (1L << i))
                        window_flag[n] |= (1L << i);
                    else
                        window_flag[n] &= ~(1L << i);
                }
            }
        }
    }
}

static void rd_quick_start(savefile_ptr file)
{
    int i;

    previous_char.psex = savefile_read_byte(file);
    previous_char.prace = savefile_read_byte(file);
    previous_char.psubrace = savefile_read_byte(file);
    previous_char.pclass = savefile_read_byte(file);
    previous_char.psubclass = savefile_read_byte(file);
    previous_char.personality = savefile_read_byte(file);
    previous_char.realm1 = savefile_read_byte(file);
    previous_char.realm2 = savefile_read_byte(file);
    previous_char.dragon_realm = savefile_read_byte(file);
    previous_char.age = savefile_read_s16b(file);
    previous_char.au = savefile_read_s32b(file);

    for (i = 0; i < 6; i++) previous_char.stat_max[i] = savefile_read_s16b(file);
    for (i = 0; i < 6; i++) previous_char.stat_max_max[i] = savefile_read_s16b(file);
    for (i = 0; i < PY_MAX_LEVEL; i++) previous_char.player_hp[i] = savefile_read_s16b(file);

    previous_char.chaos_patron = savefile_read_s16b(file);
    previous_char.mutation = savefile_read_s32b(file);

    for (i = 0; i < 8; i++) previous_char.vir_types[i] = savefile_read_s16b(file);
    previous_char.quick_ok = savefile_read_byte(file);
}

static void rd_extra(savefile_ptr file)
{
    int i,j;
    char buf[1024];

    savefile_read_cptr(file, player_name, sizeof(player_name));
    savefile_read_cptr(file, p_ptr->died_from, sizeof(p_ptr->died_from));

    savefile_read_cptr(file, buf, sizeof buf);
    if (buf[0]) p_ptr->last_message = z_string_make(buf);

    rd_quick_start(file);

    game_mode = savefile_read_s32b(file);

    p_ptr->prace = savefile_read_byte(file);
    p_ptr->pclass = savefile_read_byte(file);
    p_ptr->personality = savefile_read_byte(file);
    p_ptr->psex = savefile_read_byte(file);
    p_ptr->realm1 = savefile_read_byte(file);
    p_ptr->realm2 = savefile_read_byte(file);
    p_ptr->dragon_realm = savefile_read_byte(file);
    p_ptr->psubclass = savefile_read_byte(file);
    p_ptr->psubrace = savefile_read_byte(file);
    p_ptr->current_r_idx = savefile_read_s16b(file);
    p_ptr->expfact = savefile_read_u16b(file);

    for (i = 0; i < 6; i++) p_ptr->stat_max[i] = savefile_read_s16b(file);
    for (i = 0; i < 6; i++) p_ptr->stat_max_max[i] = savefile_read_s16b(file);
    for (i = 0; i < 6; i++) p_ptr->stat_cur[i] = savefile_read_s16b(file);

    p_ptr->au = savefile_read_s32b(file);
    p_ptr->fame = savefile_read_s16b(file);
    p_ptr->max_exp = savefile_read_s32b(file);
    p_ptr->max_max_exp = savefile_read_s32b(file);
    p_ptr->exp = savefile_read_s32b(file);
    p_ptr->exp_frac = savefile_read_u32b(file);
    p_ptr->lev = savefile_read_s16b(file);

    for (i = 0; i < 64; i++) p_ptr->spell_exp[i] = savefile_read_s16b(file);
    for (i = 0; i < 64; i++) p_ptr->spell_turn[i] = savefile_read_s32b(file);
    for (i = 0; i < 5; i++) for (j = 0; j < 64; j++) p_ptr->weapon_exp[i][j] = savefile_read_s16b(file);
    for (i = 0; i < 10; i++) p_ptr->skill_exp[i] = savefile_read_s16b(file);
    for (i = 0; i < MAX_MAGIC_NUM; i++) p_ptr->magic_num1[i] = savefile_read_s32b(file);
    for (i = 0; i < MAX_MAGIC_NUM; i++) p_ptr->magic_num2[i] = savefile_read_byte(file);
    if (music_singing_any()) p_ptr->action = ACTION_SING;

    p_ptr->start_race = savefile_read_byte(file);
    p_ptr->old_race1 = savefile_read_s32b(file);
    p_ptr->old_race2 = savefile_read_s32b(file);
    p_ptr->old_realm = savefile_read_s16b(file);

    for (i = 0; i < MAX_MANE; i++)
    {
        p_ptr->mane_spell[i] = savefile_read_s16b(file);
        p_ptr->mane_dam[i] = savefile_read_s16b(file);
    }
    p_ptr->mane_num = savefile_read_s16b(file);

    for (i = 0; i < MAX_KUBI; i++)
        kubi_r_idx[i] = savefile_read_s16b(file);

    for (i = 0; i < 4; i++)
    {
        battle_mon[i] = savefile_read_s16b(file);
        mon_odds[i] = savefile_read_u32b(file);
    }

    p_ptr->town_num = savefile_read_s16b(file);

    p_ptr->arena_number = savefile_read_s16b(file);
    p_ptr->inside_arena = (bool)savefile_read_s16b(file);
    p_ptr->inside_quest = savefile_read_s16b(file);
    p_ptr->inside_battle = (bool)savefile_read_s16b(file);
    p_ptr->exit_bldg = savefile_read_byte(file);

    p_ptr->oldpx = savefile_read_s16b(file);
    p_ptr->oldpy = savefile_read_s16b(file);

    p_ptr->mhp = savefile_read_s32b(file);
    p_ptr->chp = savefile_read_s32b(file);
    p_ptr->chp_frac = savefile_read_u32b(file);
    p_ptr->msp = savefile_read_s32b(file);
    p_ptr->csp = savefile_read_s32b(file);
    p_ptr->csp_frac = savefile_read_u32b(file);
    p_ptr->max_plv = savefile_read_s16b(file);

    {
        byte max = (byte)max_d_idx;

        max = savefile_read_byte(file);
        for (i = 0; i < max; i++)
        {
            max_dlv[i] = savefile_read_s16b(file);
            if (max_dlv[i] > d_info[i].maxdepth) max_dlv[i] = d_info[i].maxdepth;
        }

        for (i = 0; i < max; i++)
            dungeon_flags[i] = savefile_read_u32b(file);
    }

    if (p_ptr->max_plv < p_ptr->lev) p_ptr->max_plv = p_ptr->lev;

    p_ptr->concent = savefile_read_s16b(file);
    p_ptr->blind = savefile_read_s16b(file);
    p_ptr->paralyzed = savefile_read_s16b(file);
    p_ptr->confused = savefile_read_s16b(file);
    p_ptr->food = savefile_read_s16b(file);
    p_ptr->energy_need = savefile_read_s16b(file);
    p_ptr->fast = savefile_read_s16b(file);
    p_ptr->slow = savefile_read_s16b(file);
    p_ptr->afraid = savefile_read_s16b(file);
    p_ptr->cut = savefile_read_s16b(file);
    p_ptr->stun = savefile_read_s16b(file);
    p_ptr->poisoned = savefile_read_s16b(file);
    p_ptr->image = savefile_read_s16b(file);
    p_ptr->protevil = savefile_read_s16b(file);
    p_ptr->invuln = savefile_read_s16b(file);
    p_ptr->ult_res = savefile_read_s16b(file);
    p_ptr->hero = savefile_read_s16b(file);
    p_ptr->shero = savefile_read_s16b(file);
    p_ptr->shield = savefile_read_s16b(file);
    p_ptr->blessed = savefile_read_s16b(file);
    p_ptr->tim_invis = savefile_read_s16b(file);
    p_ptr->word_recall = savefile_read_s16b(file);
    p_ptr->recall_dungeon = (byte)savefile_read_s16b(file);
    p_ptr->alter_reality = savefile_read_s16b(file);
    p_ptr->see_infra = savefile_read_s16b(file);
    p_ptr->tim_infra = savefile_read_s16b(file);
    p_ptr->oppose_fire = savefile_read_s16b(file);
    p_ptr->oppose_cold = savefile_read_s16b(file);
    p_ptr->oppose_acid = savefile_read_s16b(file);
    p_ptr->oppose_elec = savefile_read_s16b(file);
    p_ptr->oppose_pois = savefile_read_s16b(file);
    p_ptr->tsuyoshi = savefile_read_s16b(file);
    p_ptr->tim_esp = savefile_read_s16b(file);
    p_ptr->tim_esp_magical = savefile_read_s16b(file);
    p_ptr->wraith_form = savefile_read_s16b(file);
    p_ptr->resist_magic = savefile_read_s16b(file);
    p_ptr->tim_regen = savefile_read_s16b(file);
    p_ptr->kabenuke = savefile_read_s16b(file);
    p_ptr->tim_stealth = savefile_read_s16b(file);
    p_ptr->tim_levitation = savefile_read_s16b(file);
    p_ptr->tim_sh_touki = savefile_read_s16b(file);
    p_ptr->lightspeed = savefile_read_s16b(file);
    p_ptr->tsubureru = savefile_read_s16b(file);
    p_ptr->magicdef = savefile_read_s16b(file);
    p_ptr->tim_res_nether = savefile_read_s16b(file);
    p_ptr->tim_res_time = savefile_read_s16b(file);
    p_ptr->tim_res_disenchantment = savefile_read_s16b(file);
    p_ptr->mimic_form = savefile_read_s16b(file);
    p_ptr->tim_mimic = savefile_read_s16b(file);
    p_ptr->tim_sh_fire = savefile_read_s16b(file);
    p_ptr->tim_sh_elements = savefile_read_s16b(file);
    p_ptr->tim_sh_shards = savefile_read_s16b(file);
    p_ptr->tim_sh_domination = savefile_read_s16b(file);
    p_ptr->tim_weaponmastery = savefile_read_s16b(file);
    p_ptr->tim_sh_holy = savefile_read_s16b(file);
    p_ptr->tim_eyeeye = savefile_read_s16b(file);
    p_ptr->tim_spurt = savefile_read_s16b(file);
    p_ptr->tim_spec_corporeal = savefile_read_s16b(file);
    p_ptr->tim_no_spells = savefile_read_s16b(file);
    p_ptr->tim_no_device = savefile_read_s16b(file);
    p_ptr->tim_speed_essentia = savefile_read_s16b(file);
    p_ptr->tim_slow_digest = savefile_read_s16b(file);
    p_ptr->tim_crystal_skin = savefile_read_s16b(file);
    p_ptr->tim_chaotic_surge = savefile_read_s16b(file);
    p_ptr->tim_wild_pos = savefile_read_s16b(file);
    p_ptr->tim_wild_mind = savefile_read_s16b(file);
    p_ptr->tim_blood_shield = savefile_read_s16b(file);
    p_ptr->tim_blood_sight = savefile_read_s16b(file);
    p_ptr->tim_blood_feast = savefile_read_s16b(file);
    p_ptr->tim_blood_revenge = savefile_read_s16b(file);
    p_ptr->tim_blood_seek = savefile_read_s16b(file);
    p_ptr->tim_blood_rite = savefile_read_s16b(file);
    p_ptr->tim_genji = savefile_read_s16b(file);
    p_ptr->tim_force = savefile_read_s16b(file);
    p_ptr->tim_building_up = savefile_read_s16b(file);
    p_ptr->tim_vicious_strike = savefile_read_s16b(file);
    p_ptr->tim_enlarge_weapon = savefile_read_s16b(file);
    p_ptr->tim_spell_reaction = savefile_read_s16b(file);
    p_ptr->tim_resist_curses = savefile_read_s16b(file);
    p_ptr->tim_armor_of_fury = savefile_read_s16b(file);
    p_ptr->tim_spell_turning = savefile_read_s16b(file);
    p_ptr->tim_dark_stalker = savefile_read_s16b(file);
    p_ptr->tim_nimble_dodge = savefile_read_s16b(file);
    p_ptr->tim_stealthy_snipe = savefile_read_s16b(file);
    p_ptr->tim_killing_spree = savefile_read_s16b(file);
    p_ptr->tim_slay_sentient = savefile_read_s16b(file);
    p_ptr->tim_shrike = savefile_read_s16b(file);

    {
        int i;
        s16b ct = savefile_read_s16b(file);
        wild_reset_counters();
        for (i = 0; i < MAX_WILD_COUNTERS && i < ct; i++)
        {
            p_ptr->wild_counters[i].type = savefile_read_s16b(file);
            p_ptr->wild_counters[i].counter = savefile_read_s16b(file);
        }
    }

    {
        int i;
        monster_race *r_ptr = &r_info[MON_MONKEY_CLONE];
        r_ptr->cur_num = savefile_read_byte(file);
        if (r_ptr->cur_num)
        {
            r_ptr->hdice = savefile_read_byte(file);
            r_ptr->hside = savefile_read_byte(file);
            r_ptr->ac = savefile_read_s16b(file);
            r_ptr->speed = savefile_read_byte(file);
            for (i = 0; i < 4; i++)
            {
                r_ptr->blow[i].method = savefile_read_byte(file);
                r_ptr->blow[i].effect = savefile_read_byte(file);
                r_ptr->blow[i].d_dice = savefile_read_byte(file);
                r_ptr->blow[i].d_side = savefile_read_byte(file);
            }
            r_ptr->flags3 = savefile_read_u32b(file);
            r_ptr->flagsr = savefile_read_u32b(file);
            r_ptr->flags2 = savefile_read_u32b(file);
            r_ptr->flags7 = savefile_read_u32b(file);
        }
    }

    p_ptr->entrench_x = savefile_read_s16b(file);
    p_ptr->entrench_y = savefile_read_s16b(file);
    p_ptr->entrench_ct = savefile_read_s16b(file);
    p_ptr->sense_artifact = savefile_read_byte(file);
    p_ptr->duelist_target_idx = savefile_read_s16b(file);
    p_ptr->tim_reflect = savefile_read_s16b(file);
    p_ptr->multishadow = savefile_read_s16b(file);
    p_ptr->dustrobe = savefile_read_s16b(file);
    p_ptr->tim_superstealth = savefile_read_s16b(file);
    p_ptr->fasting = savefile_read_bool(file);
    p_ptr->tim_sustain_str = savefile_read_s16b(file);
    p_ptr->tim_sustain_int = savefile_read_s16b(file);
    p_ptr->tim_sustain_wis = savefile_read_s16b(file);
    p_ptr->tim_sustain_dex = savefile_read_s16b(file);
    p_ptr->tim_sustain_con = savefile_read_s16b(file);
    p_ptr->tim_sustain_chr = savefile_read_s16b(file);
    p_ptr->tim_hold_life = savefile_read_s16b(file);
    p_ptr->tim_transcendence = savefile_read_s16b(file);
    p_ptr->tim_quick_walk = savefile_read_s16b(file);
    p_ptr->tim_inven_prot = savefile_read_s16b(file);
    p_ptr->tim_device_power = savefile_read_s16b(file);
    p_ptr->tim_sh_time = savefile_read_s16b(file);
    p_ptr->free_turns = savefile_read_s16b(file);
    p_ptr->tim_foresight = savefile_read_s16b(file);
    p_ptr->chaos_patron= savefile_read_s16b(file);

    for (i = 0; i < MUT_FLAG_SIZE; ++i)
        p_ptr->muta[i] = savefile_read_u32b(file);

    for (i = 0; i < MUT_FLAG_SIZE; ++i)
        p_ptr->muta_lock[i] = savefile_read_u32b(file);
         
    for (i = 0; i < MAX_DEMIGOD_POWERS; ++i)
        p_ptr->demigod_power[i] = savefile_read_s16b(file);

    p_ptr->draconian_power = savefile_read_s16b(file);

    for (i = 0; i < 8; i++)
        p_ptr->virtues[i] = savefile_read_s16b(file);
    for (i = 0; i < 8; i++)
        p_ptr->vir_types[i] = savefile_read_s16b(file);

    mutant_regenerate_mod = mut_regenerate_mod();

    p_ptr->ele_attack = savefile_read_s16b(file);
    p_ptr->special_attack = savefile_read_u32b(file);

    if (p_ptr->special_attack & KAMAE_MASK) p_ptr->action = ACTION_KAMAE;
    else if (p_ptr->special_attack & KATA_MASK) p_ptr->action = ACTION_KATA;

    p_ptr->ele_immune = savefile_read_s16b(file);
    p_ptr->special_defense = savefile_read_u32b(file);
    p_ptr->knowledge = savefile_read_byte(file);

    p_ptr->autopick_autoregister = savefile_read_byte(file) ? TRUE: FALSE;
    p_ptr->action = savefile_read_byte(file);
    preserve_mode = savefile_read_byte(file);
    p_ptr->wait_report_score = savefile_read_byte(file);

    seed_flavor = savefile_read_u32b(file);
    seed_town = savefile_read_u32b(file);
    p_ptr->panic_save = savefile_read_u16b(file);
    p_ptr->total_winner = savefile_read_u16b(file);
    p_ptr->noscore = savefile_read_u16b(file);
    p_ptr->is_dead = savefile_read_byte(file) ? TRUE : FALSE;
    p_ptr->feeling = savefile_read_byte(file);

    switch (p_ptr->start_race)
    {
    case RACE_VAMPIRE:
    case RACE_MON_VAMPIRE:
    case RACE_SKELETON:
    case RACE_ZOMBIE:
    case RACE_SPECTRE:
        game_turn_limit = TURNS_PER_TICK * TOWN_DAWN * MAX_DAYS + TURNS_PER_TICK * TOWN_DAWN * 3 / 4;
        break;
    default:
        game_turn_limit = TURNS_PER_TICK * TOWN_DAWN * (MAX_DAYS - 1) + TURNS_PER_TICK * TOWN_DAWN * 3 / 4;
        break;
    }
    dungeon_turn_limit = TURNS_PER_TICK * TOWN_DAWN * (MAX_DAYS - 1) + TURNS_PER_TICK * TOWN_DAWN * 3 / 4;

    old_turn = savefile_read_s32b(file);
    p_ptr->feeling_turn = savefile_read_s32b(file);
    game_turn = savefile_read_s32b(file);
    player_turn = savefile_read_s32b(file);

    dungeon_turn = savefile_read_s32b(file);
    old_battle = savefile_read_s32b(file);
    today_mon = savefile_read_s16b(file);
    p_ptr->today_mon = savefile_read_s16b(file);
    p_ptr->riding = savefile_read_s16b(file);
    p_ptr->floor_id = savefile_read_s16b(file);

    playtime = savefile_read_u32b(file);
    p_ptr->visit = savefile_read_s32b(file);
    p_ptr->count = savefile_read_u32b(file);

    {
    race_t  *race_ptr = get_true_race();
    class_t *class_ptr = get_class();

        if (race_ptr->load_player)
            race_ptr->load_player(file);
        if (class_ptr->load_player)
            class_ptr->load_player(file);
    }
}

/*
 * Read the player inventory
 * Note that the inventory is "re-sorted" later by "dungeon()".
 */
static errr rd_inventory(savefile_ptr file)
{
    int           slot = 0;
    object_type   forge;

    p_ptr->total_weight = 0;
    inven_cnt = 0;

    /* Read until done */
    while (1)
    {
        u16b n = savefile_read_u16b(file);

        if (n == 0xFFFF) break;

        rd_item(file, &forge);

        if (!forge.k_idx) return (53);

        if (n >= EQUIP_BEGIN)
        {
            forge.marked |= OM_TOUCHED;
            object_copy(&inventory[n], &forge);
            p_ptr->total_weight += (forge.number * forge.weight);
        }
        else if (inven_cnt == INVEN_PACK)
        {
            note("Too many items in the inventory!");
            return (54);
        }
        else
        {
            n = slot++;
            forge.marked |= OM_TOUCHED;
            object_copy(&inventory[n], &forge);
            p_ptr->total_weight += (forge.number * forge.weight);
            inven_cnt++;
        }
    }
    return 0;
}

/*
 * Read the saved floor
 *
 * The monsters/objects must be loaded in the same order
 * that they were stored, since the actual indexes matter.
 */
static errr rd_saved_floor(savefile_ptr file, saved_floor_type *sf_ptr)
{
    int ymax, xmax;
    int i, y, x;
    byte count;
    byte tmp8u;
    s16b tmp16s;
    s32b tmp32s;
    u32b tmp32u;
    u16b limit;
    cave_template_type *template;

    clear_cave();

    if (!sf_ptr)
    {
        dun_level = savefile_read_s16b(file);
        base_level = dun_level;
    }
    else
    {
        tmp16s = savefile_read_s16b(file);
        if (tmp16s != sf_ptr->floor_id) return 171;

        tmp8u = savefile_read_byte(file);
        if (tmp8u != sf_ptr->savefile_id) return 171;

        tmp16s = savefile_read_s16b(file);
        if (tmp16s != sf_ptr->dun_level) return 171;
        dun_level = sf_ptr->dun_level;

        tmp32s = savefile_read_s32b(file);
        if (tmp32s != sf_ptr->last_visit) return 171;

        tmp32u = savefile_read_u32b(file);
        if (tmp32u != sf_ptr->visit_mark) return 171;

        tmp16s = savefile_read_s16b(file);
        if (tmp16s != sf_ptr->upper_floor_id) return 171;

        tmp16s = savefile_read_s16b(file);
        if (tmp16s != sf_ptr->lower_floor_id) return 171;
    }

    base_level = savefile_read_s16b(file);
    num_repro = savefile_read_s16b(file);
    num_repro_kill = savefile_read_s16b(file);
    py = savefile_read_u16b(file);
    px = savefile_read_u16b(file);
    cur_hgt = savefile_read_s16b(file);
    cur_wid = savefile_read_s16b(file);
    p_ptr->feeling = savefile_read_byte(file);

    limit = savefile_read_u16b(file);
    C_MAKE(template, limit, cave_template_type);

    for (i = 0; i < limit; i++)
    {
        cave_template_type *ct_ptr = &template[i];
        ct_ptr->info = savefile_read_u16b(file);
        ct_ptr->feat = savefile_read_s16b(file);
        ct_ptr->mimic = savefile_read_s16b(file);
        ct_ptr->special = savefile_read_s16b(file);
    }

    /* Maximal size */
    ymax = cur_hgt;
    xmax = cur_wid;


    /*** Run length decoding ***/
    /* Load the dungeon data */
    for (x = y = 0; y < ymax; )
    {
        u16b id;

        /* Grab RLE info */
        count = savefile_read_byte(file);

        id = 0;
        do 
        {
            tmp8u = savefile_read_byte(file);
            id += tmp8u;
        } while (tmp8u == MAX_UCHAR);

        /* Apply the RLE info */
        for (i = count; i > 0; i--)
        {
            /* Access the cave */
            cave_type *c_ptr = &cave[y][x];

            /* Extract cave data */
            c_ptr->info = template[id].info;
            c_ptr->feat = template[id].feat;
            c_ptr->mimic = template[id].mimic;
            c_ptr->special = template[id].special;

            /* Advance/Wrap */
            if (++x >= xmax)
            {
                /* Wrap */
                x = 0;

                /* Advance/Wrap */
                if (++y >= ymax) break;
            }
        }
    }

    /* Free the "template" array */
    C_FREE(template, limit, cave_template_type);


    /*** Objects ***/

    limit = savefile_read_u16b(file);
    if (limit > max_o_idx) return 151;
    for (i = 1; i < limit; i++)
    {
        int o_idx;
        object_type *o_ptr;

        o_idx = o_pop();
        if (i != o_idx) return 152;
        o_ptr = &o_list[o_idx];
        rd_item(file, o_ptr);
        if (o_ptr->held_m_idx)
        {
            monster_type *m_ptr = &m_list[o_ptr->held_m_idx];
            o_ptr->next_o_idx = m_ptr->hold_o_idx;
            m_ptr->hold_o_idx = o_idx;
        }
        else
        {
            cave_type *c_ptr = &cave[o_ptr->iy][o_ptr->ix];
            o_ptr->next_o_idx = c_ptr->o_idx;
            c_ptr->o_idx = o_idx;
        }
    }


    /*** Monsters ***/
    limit = savefile_read_u16b(file);
    if (limit > max_m_idx) return 161;
    for (i = 1; i < limit; i++)
    {
        cave_type *c_ptr;
        int m_idx;
        monster_type *m_ptr;

        m_idx = m_pop();
        if (i != m_idx) return 162;
        m_ptr = &m_list[m_idx];
        rd_monster(file, m_ptr);
        c_ptr = &cave[m_ptr->fy][m_ptr->fx];
        c_ptr->m_idx = m_idx;
        real_r_ptr(m_ptr)->cur_num++;
    }

    {
        s16b count;

        pack_info_wipe();
        count = savefile_read_s16b(file);
        for (i = 0; i < count; ++i)
        {
            s16b new_idx = pack_info_pop();
            s16b old_idx;
            pack_info_t *ptr = &pack_info_list[new_idx];
            int j;

            old_idx = savefile_read_s16b(file);
            ptr->leader_idx = savefile_read_s16b(file);
            ptr->count = savefile_read_s16b(file);
            ptr->ai = savefile_read_s16b(file);
            ptr->guard_idx = savefile_read_s16b(file);
            ptr->guard_x = savefile_read_s16b(file);
            ptr->guard_y = savefile_read_s16b(file);
            ptr->distance = savefile_read_s16b(file);

            /* I make no effort to keep the same pack_info index on a reload, so
               patch things up. I'm pretty sure, but not certain, that monster
               and object indices won't change after a save and reload. */
            for (j = 1; j < max_m_idx; ++j)
            {
                if (m_list[j].pack_idx == old_idx)
                    m_list[j].pack_idx = new_idx;
            }
        }
    }

    return 0;
}


/*
 * Read the dungeon (new method)
 *
 * The monsters/objects must be loaded in the same order
 * that they were stored, since the actual indexes matter.
 */
static errr rd_dungeon(savefile_ptr file)
{
    errr err = 0;
    byte num;
    int i;

    /* Initialize saved_floors array and temporal files */
    init_saved_floors(FALSE);

    /*** Meta info ***/

    max_floor_id = savefile_read_s16b(file);
    dungeon_type = savefile_read_byte(file);
    
    num = savefile_read_byte(file);

    /*** No saved floor (On the surface etc.) ***/
    if (!num)
    {
        /* Read the current floor data */
        err = rd_saved_floor(file, NULL);
    }

    /*** In the dungeon ***/
    else
    {

        /* Read the saved_floors array */
        for (i = 0; i < num; i++)
        {
            saved_floor_type *sf_ptr = &saved_floors[i];

            sf_ptr->floor_id = savefile_read_s16b(file);
            sf_ptr->savefile_id = savefile_read_byte(file);
            sf_ptr->dun_level = savefile_read_s16b(file);
            sf_ptr->last_visit = savefile_read_s32b(file);
            sf_ptr->visit_mark = savefile_read_u32b(file);
            sf_ptr->upper_floor_id = savefile_read_s16b(file);
            sf_ptr->lower_floor_id = savefile_read_s16b(file);
        }


        /* Move saved floors data to temporal files */
        for (i = 0; i < num; i++)
        {
            saved_floor_type *sf_ptr = &saved_floors[i];
            byte tmp8u;

            /* Unused element */
            if (!sf_ptr->floor_id) continue;

            /* Read the failure mark */
            tmp8u = savefile_read_byte(file);
            if (tmp8u) continue;

            /* Read from the save file */
            err = rd_saved_floor(file, sf_ptr);

            /* Error? */
            if (err) break;

            /* Re-save as temporal saved floor file */
            if (!save_floor(sf_ptr, SLF_SECOND)) err = 182;

            /* Error? */
            if (err) break;
        }

        /* Finally load current floor data from temporal file */
        if (!err)
        {
            if (!load_floor(get_sf_ptr(p_ptr->floor_id), SLF_SECOND)) err = 183;
        }
    }

    /*** Error messages ***/
    switch (err)
    {
    case 151:
        note("Too many object entries!");
        break;
    case 152:
        note("Object allocation error");
        break;
    case 161:
        note("Too many monster entries!");
        break;
    case 162:
        note("Monster allocation error");
        break;
    case 171:
        note("Dungeon data of saved floors are broken!");
        break;
    case 182:
        note("Failed to make temporal files!");
        break;
    case 183:
        note("Error 183");
        break;
    }

    /* The dungeon is ready */
    character_dungeon = TRUE;

    /* Success or Error */
    return err;
}

static errr rd_savefile_new_aux(savefile_ptr file)
{
    int i, j;
    int town_count;

    s32b wild_x_size;
    s32b wild_y_size;

    u16b tmp16u;

#ifdef VERIFY_CHECKSUMS
    u32b n_x_check, n_v_check;
    u32b o_x_check, o_v_check;
#endif

    /* Mention the savefile version */
    note(format(
             "Loading a %d.%d.%d savefile...",
             (z_major > 9) ? z_major - 10 : z_major, z_minor, z_patch));

    if (savefile_is_older_than(file, 5, 0, 0, 0))
    {
        note("Old savefiles are not supported!");
        return 1;
    }

    sf_system = savefile_read_u32b(file);
    sf_when = savefile_read_u32b(file);
    sf_lives = savefile_read_u16b(file);
    sf_saves = savefile_read_u16b(file);

    rd_randomizer(file);
    if (arg_fiddle) note("Loaded Randomizer Info");

    rd_options(file);
    if (arg_fiddle) note("Loaded Option Flags");

    msg_on_load(file);
    if (arg_fiddle) note("Loaded Messages");

    for (i = 0; i < max_r_idx; i++)
    {
        /* Access that monster */
        monster_race *r_ptr = &r_info[i];

        /* Hack -- Reset the death counter */
        r_ptr->max_num = 100;

        if (r_ptr->flags1 & RF1_UNIQUE) r_ptr->max_num = 1;

        /* Hack -- Non-unique Nazguls are semi-unique */
        else if (r_ptr->flags7 & RF7_NAZGUL) r_ptr->max_num = MAX_NAZGUL_NUM;
        else if (i == MON_CAMELOT_KNIGHT) 
            r_ptr->max_num = MAX_CAMELOT_KNIGHT_NUM;
    }

    /* Monster Memory */
    tmp16u = savefile_read_u16b(file);

    if (tmp16u > max_r_idx)
    {
        note(format("Too many (%u) monster races!", tmp16u));
        return (21);
    }
    for (i = 0; i < tmp16u; i++)
        rd_lore(file, i);

    if (arg_fiddle) note("Loaded Monster Memory");


    /* Object Memory */
    tmp16u = savefile_read_u16b(file);

    if (tmp16u > max_k_idx)
    {
        note(format("Too many (%u) object kinds!", tmp16u));
        return (22);
    }
    for (i = 0; i < tmp16u; i++)
    {
        byte tmp8u;
        object_kind *k_ptr = &k_info[i];

        tmp8u = savefile_read_byte(file);

        k_ptr->aware = (tmp8u & 0x01) ? TRUE: FALSE;
        k_ptr->tried = (tmp8u & 0x02) ? TRUE: FALSE;

        k_ptr->counts.generated = savefile_read_s32b(file);
        k_ptr->counts.found = savefile_read_s32b(file);
        k_ptr->counts.bought = savefile_read_s32b(file);
        k_ptr->counts.used = savefile_read_s32b(file);
        k_ptr->counts.destroyed = savefile_read_s32b(file);
    }

    {
        tmp16u = savefile_read_u16b(file);

        if (tmp16u > max_e_idx)
        {
            note(format("Too many (%u) ego kinds!", tmp16u));
            return (22);
        }
        for (i = 0; i < tmp16u; i++)
        {
            int       ct, j;
            ego_type *e_ptr = &e_info[i];

            ct = savefile_read_byte(file);
            if (ct > OF_ARRAY_SIZE)
            {
                note(format("Too many (%d) ego known flags!", ct));
                return (22);
            }
            for (j = 0; j < ct; j++)
                e_ptr->known_flags[j] = savefile_read_u32b(file);
            for (j = ct; j < OF_ARRAY_SIZE; j++)
                e_ptr->known_flags[j] = 0;

            ct = savefile_read_byte(file);
            if (ct > OF_ARRAY_SIZE)
            {
                note(format("Too many (%d) ego xtra flags!", ct));
                return (22);
            }
            for (j = 0; j < ct; j++)
                e_ptr->xtra_flags[j] = savefile_read_u32b(file);
            for (j = ct; j < OF_ARRAY_SIZE; j++)
                e_ptr->xtra_flags[j] = 0;

            e_ptr->counts.generated = savefile_read_s32b(file);
            e_ptr->counts.found = savefile_read_s32b(file);
            e_ptr->counts.bought = savefile_read_s32b(file);
            /*e_ptr->counts.used = savefile_read_s32b(file);*/
            e_ptr->counts.destroyed = savefile_read_s32b(file);
        }

        tmp16u = savefile_read_u16b(file);
        if (tmp16u > max_a_idx)
        {
            note(format("Too many (%u) artifacts!", tmp16u));
            return (22);
        }
        for (i = 0; i < tmp16u; i++)
        {
            int            ct, j;
            artifact_type *a_ptr = &a_info[i];

            ct = savefile_read_byte(file);
            if (ct > OF_ARRAY_SIZE)
            {
                note(format("Too many (%d) artifact known flags!", ct));
                return (22);
            }
            for (j = 0; j < ct; j++)
                a_ptr->known_flags[j] = savefile_read_u32b(file);
            for (j = ct; j < OF_ARRAY_SIZE; j++)
                a_ptr->known_flags[j] = 0;
        }
    }
    if (arg_fiddle) note("Loaded Object Memory");

    /* Init the wilderness seeds */
    for (i = 0; i < max_wild_x; i++)
    {
        for (j = 0; j < max_wild_y; j++)
        {
            wilderness[j][i].seed = randint0(0x10000000);
        }
    }

    {
        u16b max_towns_load;
        u16b max_quests_load;
        byte max_rquests_load;

        max_towns_load = savefile_read_u16b(file);
        if (max_towns_load > max_towns)
        {
            note(format("Too many (%u) towns!", max_towns_load));
            return (23);
        }

        max_quests_load = savefile_read_u16b(file);
        max_rquests_load = savefile_read_byte(file);
        num_random_quests = max_rquests_load;
        if (max_quests_load > max_quests)
        {
            note(format("Too many (%u) quests!", max_quests_load));
            return (23);
        }
        for (i = 0; i < max_quests_load; i++)
        {
            if (i < max_quests)
            {
                quest[i].status = savefile_read_s16b(file);
                quest[i].level = savefile_read_s16b(file);
                quest[i].complev = savefile_read_byte(file);

                if ((quest[i].status == QUEST_STATUS_TAKEN) ||
                    (quest[i].status == QUEST_STATUS_COMPLETED) ||
                    ((i >= MIN_RANDOM_QUEST) && (i < (MIN_RANDOM_QUEST + max_rquests_load))))
                {
                    quest[i].cur_num = savefile_read_s16b(file);
                    quest[i].max_num = savefile_read_s16b(file);
                    quest[i].type = savefile_read_s16b(file);
                    quest[i].r_idx = savefile_read_s16b(file);

                    if (quest[i].type == QUEST_TYPE_RANDOM)
                    {
                        if (!quest[i].r_idx || quest[i].r_idx == MON_NAZGUL)
                        {
                            quest_type      *q_ptr = &quest[i];
                            monster_race    *quest_r_ptr;
                            
                            determine_random_questor(q_ptr);
                            quest_r_ptr = &r_info[q_ptr->r_idx];
                            
                            if (quest_r_ptr->flags1 & RF1_UNIQUE)
                            {
                                quest_r_ptr->flags1 |= RF1_QUESTOR;
                                q_ptr->max_num = 1;
                            }
                            else
                            {
                                q_ptr->max_num = randint1(20) + 5;
                            }
                        }
                    }

                    quest[i].k_idx = savefile_read_s16b(file);
                    if (quest[i].k_idx)
                        a_info[quest[i].k_idx].gen_flags |= OFG_QUESTITEM;

                    quest[i].flags = savefile_read_byte(file);
                    quest[i].dungeon = savefile_read_byte(file);
                    /* Mark uniques */
                    if (quest[i].status == QUEST_STATUS_TAKEN || quest[i].status == QUEST_STATUS_UNTAKEN)
                        if (r_info[quest[i].r_idx].flags1 & RF1_UNIQUE)
                            r_info[quest[i].r_idx].flags1 |= RF1_QUESTOR;

                    quest[i].seed = savefile_read_u32b(file);
                }
            }
            /* Ignore the empty quests from old versions */
            else
            {
                savefile_read_skip(file, 4);
            }
        }

        p_ptr->wilderness_x = savefile_read_s32b(file);
        p_ptr->wilderness_y = savefile_read_s32b(file);
        p_ptr->wilderness_dx = savefile_read_s16b(file);
        p_ptr->wilderness_dy = savefile_read_s16b(file);
        p_ptr->wild_mode = savefile_read_byte(file);
        savefile_read_skip(file, 1);

        wild_x_size = savefile_read_s32b(file);
        wild_y_size = savefile_read_s32b(file);
        if ((wild_x_size > max_wild_x) || (wild_y_size > max_wild_y))
        {
            note(format("Wilderness is too big (%u/%u)!", wild_x_size, wild_y_size));
            return (23);
        }

        for (i = 0; i < wild_x_size; i++)
        {
            for (j = 0; j < wild_y_size; j++)
                wilderness[j][i].seed = savefile_read_u32b(file);
        }
    }

    if (arg_fiddle) note("Loaded Quests");

    /* Load the Artifacts */
    tmp16u = savefile_read_u16b(file);
    if (tmp16u > max_a_idx)
    {
        note(format("Too many (%u) artifacts!", tmp16u));
        return (24);
    }
    for (i = 0; i < tmp16u; i++)
    {
        artifact_type *a_ptr = &a_info[i];
        a_ptr->generated = savefile_read_byte(file);
        if (!savefile_is_older_than(file, 5, 0, 0, 1))
            a_ptr->found = savefile_read_byte(file);
        a_ptr->floor_id = savefile_read_s16b(file);
    }
    if (arg_fiddle) note("Loaded Artifacts");


    rd_extra(file);

    if (p_ptr->energy_need < -999) world_player = TRUE;

    if (arg_fiddle) note("Loaded extra information");

    /* Read the player_hp array */
    tmp16u = savefile_read_u16b(file);
    if (tmp16u > PY_MAX_LEVEL)
    {
        note(format("Too many (%u) hitpoint entries!", tmp16u));
        return (25);
    }
    for (i = 0; i < tmp16u; i++)
        p_ptr->player_hp[i] = savefile_read_s16b(file);

    /* Important -- Initialize stuff */
    sp_ptr = &sex_info[p_ptr->psex];
    mp_ptr = &m_info[p_ptr->pclass];

    /* Read spell info */
    p_ptr->spell_learned1 = savefile_read_u32b(file);
    p_ptr->spell_learned2 = savefile_read_u32b(file);
    p_ptr->spell_worked1 = savefile_read_u32b(file);
    p_ptr->spell_worked2 = savefile_read_u32b(file);
    p_ptr->spell_forgotten1 = savefile_read_u32b(file);
    p_ptr->spell_forgotten2 = savefile_read_u32b(file);
    p_ptr->learned_spells = savefile_read_s16b(file);
    p_ptr->add_spells = savefile_read_s16b(file);
    if (p_ptr->pclass == CLASS_MINDCRAFTER) p_ptr->add_spells = 0;

    for (i = 0; i < 64; i++)
        p_ptr->spell_order[i] = savefile_read_byte(file);


    /* Read the inventory */
    equip_on_init();
    if (rd_inventory(file))
    {
        note("Unable to read inventory");
        return (21);
    }

    town_count = savefile_read_u16b(file);
    tmp16u = savefile_read_u16b(file);
    for (i = 1; i < town_count; i++)
    {
        for (j = 0; j < tmp16u; j++)
        {
            if (rd_store(file, i, j)) return (22);
        }
    }

    p_ptr->pet_follow_distance = savefile_read_s16b(file);
    p_ptr->pet_extra_flags = savefile_read_s16b(file);

    if (1)
    {
        char buf[SCREEN_BUF_SIZE];
        savefile_read_cptr(file, buf, sizeof(buf));
        if (buf[0]) screen_dump = z_string_make(buf);
    }

    if (p_ptr->is_dead)
    {
        for (i = MIN_RANDOM_QUEST; i < MIN_RANDOM_QUEST + num_random_quests; i++)
        {
            r_info[quest[i].r_idx].flags1 &= ~(RF1_QUESTOR);
        }
    }

    spell_stats_on_load(file);
    skills_on_load(file);
    stats_on_load(file);

    /* I'm not dead yet... */
    if (!p_ptr->is_dead)
    {
        int tmp_ix = p_ptr->duelist_target_idx;
        note("Restoring Dungeon...");
        if (rd_dungeon(file))
        {
            note("Error reading dungeon data");
            return (34);
        }
        p_ptr->duelist_target_idx = tmp_ix;
    }

#ifdef VERIFY_CHECKSUMS

    n_v_check = file->v_check;
    o_v_check = savefile_read_u32b(file);
    if (o_v_check != n_v_check)
    {
        note("Invalid checksum");
        return (11);
    }
    n_x_check = file->x_check;
    o_x_check = savefile_read_u32b(file);
    if (o_x_check != n_x_check)
    {
        note("Invalid encoded checksum");
        return (11);
    }

#endif

    return 0;
}


/*
 * Actually read the savefile
 */
errr rd_savefile_new(void)
{
    errr err;
    savefile_ptr file = savefile_open_read(savefile);

    if (!file) return -1;

    err = rd_savefile_new_aux(file);
    if (ferror(file->file)) err = -1;

    savefile_close(file);
    return err;
}


/*
 * Actually load and verify a floor save data
 */
static bool load_floor_aux(savefile_ptr file, saved_floor_type *sf_ptr)
{
    u32b tmp32u;

#ifdef VERIFY_CHECKSUMS
    u32b n_x_check, n_v_check;
    u32b o_x_check, o_v_check;
#endif

    /* Verify the sign */
    tmp32u = savefile_read_u32b(file);
    if (saved_floor_file_sign != tmp32u) return FALSE;

    /* Read -- have error? */
    if (rd_saved_floor(file, sf_ptr)) return FALSE;


#ifdef VERIFY_CHECKSUMS
    n_v_check = file->v_check;
    o_v_check = savefile_read_u32b(file);
    if (o_v_check != n_v_check) return FALSE;
    n_x_check = file->x_check;
    o_x_check = savefile_read_u32b(file);
    if (o_x_check != n_x_check) return FALSE;
#endif

    return TRUE;
}


/*
 * Attempt to load the temporally saved-floor data
 */
bool load_floor(saved_floor_type *sf_ptr, u32b mode)
{
    bool ok = TRUE;
    char floor_savefile[1024];
    savefile_ptr file = NULL;

    sprintf(floor_savefile, "%s.F%02d", savefile, (int)sf_ptr->savefile_id);
    file = savefile_open_read(floor_savefile);
    if (!file) return FALSE;

    ok = load_floor_aux(file, sf_ptr);
    if (ferror(file->file)) ok = FALSE;

    savefile_close(file);

    if (!(mode & SLF_NO_KILL))
    {
        safe_setuid_grab();
        fd_kill(floor_savefile);
        safe_setuid_drop();
    }
    
    if (!ok)
    {
        msg_print("Software bug in load_floor: All is *not* OK!");
    }

    return TRUE;
}
