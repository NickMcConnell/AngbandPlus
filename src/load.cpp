
/* File: load.c */

/*
 * Copyright (c) 1997 Ben Harrison, Jeff Greene, Diego Gonzalez, and others
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"
#include "src/loadsave.h"
#include <QLabel>




/*
 * This file loads savefiles from Angband 2.9.X.
 *
 * We attempt to prevent corrupt savefiles from inducing memory errors.
 *
 * Note that this file should not use the random number generator, the
 * object flavors, the visual attr/char mappings, or anything else which
 * is initialized *after* or *during* the "load character" function.
 *
 * This file assumes that the monster/object records are initialized
 * to zero, and the race/kind tables have been loaded correctly.  The
 * order of object stacks is currently not saved in the savefiles, but
 * the "next" pointers are saved, so all necessary knowledge is present.
 *
 * We should implement simple "savefile extenders" using some form of
 * "sized" chunks of bytes, with a {size,type,data} format, so everyone
 * can know the size, interested people can know the type, and the actual
 * data is available to the parsing routines that acknowledge the type.
 *
 * Consider changing the "globe of invulnerability" code so that it
 * takes some form of "maximum damage to protect from" in addition to
 * the existing "number of turns to protect for", and where each hit
 * by a monster will reduce the shield by that amount.  XXX XXX XXX
 */

static QFile save_file;
static QDataStream in(&save_file);

static u16b new_artifacts;
static u16b art_norm_count;


byte sf_major;
byte sf_minor;
byte sf_patch;
byte sf_extra;
u32b sf_xtra;
u32b sf_when;
u16b sf_lives;
u16b sf_saves;
bool arg_fiddle;



/*
 * This function determines if the version of the savefile
 * currently being read is older than version "x.y.z".
 */
static bool older_than(int x, int y, int z)
{
    /* Much older, or much more recent */
    if (sf_major < x) return (TRUE);
    if (sf_major > x) return (FALSE);

    /* Distinctly older, or distinctly more recent */
    if (sf_minor < y) return (TRUE);
    if (sf_minor > y) return (FALSE);

    /* Barely older, or barely more recent */
    if (sf_patch < z) return (TRUE);
    if (sf_patch > z) return (FALSE);

    /* Identical versions */
    return (FALSE);
}


static void rd_byte(byte *ip)
{
    byte data_in;

    in >> data_in;

    *ip = data_in;
}

static void rd_u16b(u16b *ip)
{
    u16b data_in;

    in >> data_in;

    *ip = data_in;
}

static void rd_s16b(s16b *ip)
{
    s16b data_in;

    in >> data_in;

    *ip = data_in;
}

static void rd_u32b(u32b *ip)
{
    u32b data_in;

    in >> data_in;

    *ip = data_in;
}

static void rd_s32b(s32b *ip)
{
    s32b data_in;

    in >> data_in;

    *ip = data_in;
}


/*
 * Hack -- read a string
 */
static void rd_string(QString *str)
{
    QString data_in;

    in >> data_in;

    *str = data_in;
}


/*
 * Hack -- strip some bytes
 */
static void strip_bytes(int n)
{
    byte tmp8u;

    /* Strip the bytes */
    while (n--) rd_byte(&tmp8u);
}


/*
 * Read an object
 *
 * This function attempts to "repair" old savefiles, and to extract
 * the most up to date values for various object fields.
 */
static int rd_item(object_type *o_ptr)
{
    byte old_dd;
    byte old_ds;

    object_kind *k_ptr;

    QString buf;

    /* Kind */
    rd_s16b(&o_ptr->k_idx);

    /* Paranoia */
    if ((o_ptr->k_idx < 0) || (o_ptr->k_idx >= z_info->k_max))
    {
        return (-1);
    }

    /* Location */
    rd_byte(&o_ptr->iy);
    rd_byte(&o_ptr->ix);

    /* Type/Subtype */
    rd_byte(&o_ptr->tval);
    rd_byte(&o_ptr->sval);

    /* Special pval */
    rd_s16b(&o_ptr->pval);

    rd_byte(&o_ptr->discount);

    rd_byte(&o_ptr->number);
    rd_s16b(&o_ptr->weight);

    rd_byte(&o_ptr->art_num);
    rd_byte(&o_ptr->ego_num);

    rd_s16b(&o_ptr->timeout);

    rd_s16b(&o_ptr->to_h);
    rd_s16b(&o_ptr->to_d);
    rd_s16b(&o_ptr->to_a);

    rd_s16b(&o_ptr->ac);

    rd_byte(&old_dd);
    rd_byte(&old_ds);

    rd_u32b(&o_ptr->ident);

    rd_byte(&o_ptr->marked);

    rd_s16b(&o_ptr->mimic_r_idx);

    /* Old flags */
    strip_bytes(12);

    /* Monster holding object */
    rd_s16b(&o_ptr->held_m_idx);

    /* Special powers */
    rd_byte(&o_ptr->xtra1);
    rd_u32b(&o_ptr->xtra2);

    /* Inscription */
    rd_string(&o_ptr->inscription);

    // Read the object verify bool array
    for (int i = 0; i < VERIFY_MAX; i++)
    {
        byte tmp8u;
        rd_byte(&tmp8u);
        o_ptr->use_verify[i] = tmp8u;
    }

    /* Object history */
    rd_byte(&o_ptr->origin_nature);
    rd_s16b(&o_ptr->origin_dlvl);
    rd_s16b(&o_ptr->origin_r_idx);
    rd_string(&buf);
    if (buf.length()) o_ptr->origin_m_name = buf;

    /* Obtain the "kind" template */
    k_ptr = &k_info[o_ptr->k_idx];

    /* Obtain tval/sval from k_info */
    o_ptr->tval = k_ptr->tval;
    o_ptr->sval = k_ptr->sval;

    /* Hack -- notice "broken" items */
    if (k_ptr->cost <= 0) o_ptr->ident |= (IDENT_BROKEN);

    /* Ensure that rods and wands get the appropriate pvals,
     * and transfer rod charges to timeout.
     * this test should only be passed once, the first
     * time the file is open with ROD/WAND stacking code
     * It could change the timeout improperly if the PVAL (time a rod
     * takes to charge after use) is changed in object.txt.
     * But this is nothing a little resting won't solve.
     *
     * -JG-
     */
    if ((o_ptr->tval == TV_ROD) && (o_ptr->pval - (k_ptr->pval * o_ptr->number) != 0))
    {

        o_ptr->timeout = o_ptr->pval;
        o_ptr->pval = k_ptr->pval * o_ptr->number;

    }



    /* Repair non "wearable" items */
    if (!o_ptr->is_wearable())
    {
        /* Get the correct fields */
        o_ptr->to_h = k_ptr->to_h;
        o_ptr->to_d = k_ptr->to_d;
        o_ptr->to_a = k_ptr->to_a;

        /* Get the correct fields */
        o_ptr->ac = k_ptr->ac;
        o_ptr->dd = k_ptr->dd;
        o_ptr->ds = k_ptr->ds;

        /* Get the correct weight */
        o_ptr->weight = k_ptr->weight;

        if ((o_ptr->tval != TV_MAGIC_BOOK) &&
            (o_ptr->tval != TV_PRAYER_BOOK) &&
            (o_ptr->tval != TV_DRUID_BOOK))
        {

            /* Paranoia */
            o_ptr->art_num = o_ptr->ego_num = 0;

            /* All done */
            return (0);
        }

        /*spellbooks can now have an ego-item*/
        else o_ptr->art_num = 0;
    }

    /* Extract the flags */
    o_ptr->update_object_flags();

       /* Paranoia */
    if (o_ptr->art_num)
    {
        artifact_type *a_ptr;

        /*hack - adjust if new artifact*/
        if (o_ptr->art_num >= art_norm_count)
        {

            o_ptr->art_num += new_artifacts;
        }

        /* Paranoia */
        if (o_ptr->art_num >= z_info->art_max)
        {
            return (-1);
        }

        /* Obtain the artifact info */
        a_ptr = &a_info[o_ptr->art_num];

        /* Verify that artifact */
        if (a_ptr->tval + a_ptr->sval == 0)
        {
            o_ptr->art_num = 0;
        }
    }

    /* Paranoia */
    if (o_ptr->ego_num)
    {
        ego_item_type *e_ptr;

        /* Paranoia */
        if (o_ptr->ego_num >= z_info->e_max)
        {
            return (-1);
        }

        /* Obtain the ego-item info */
        e_ptr = &e_info[o_ptr->ego_num];

        /* Verify that ego-item */
        if (e_ptr->e_name.isEmpty()) o_ptr->ego_num = 0;
    }

    /* Get the standard fields */
    o_ptr->ac = k_ptr->ac;
    o_ptr->dd = k_ptr->dd;
    o_ptr->ds = k_ptr->ds;

    /* Get the standard weight */
    o_ptr->weight = k_ptr->weight;

    /* Hack -- extract the "broken" flag */
    if (o_ptr->pval < 0) o_ptr->ident |= (IDENT_BROKEN);

    /* Artifacts */
    if (o_ptr->art_num)
    {
        artifact_type *a_ptr;

        /* Obtain the artifact info */
        a_ptr = &a_info[o_ptr->art_num];

        /* Get the new artifact "pval" */
        o_ptr->pval = a_ptr->pval;

        /* Get the new artifact fields */
        o_ptr->ac = a_ptr->ac;
        o_ptr->dd = a_ptr->dd;
        o_ptr->ds = a_ptr->ds;

        /* Get the new artifact weight */
        o_ptr->weight = a_ptr->weight;

        /* Hack -- extract the "broken" flag */
        if (!a_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);
    }

    /* Ego items */
    if (o_ptr->ego_num)
    {
        ego_item_type *e_ptr;

        /* Obtain the ego-item info */
        e_ptr = &e_info[o_ptr->ego_num];

        /* Hack -- extract the "broken" flag */
        if (!e_ptr->cost) o_ptr->ident |= (IDENT_BROKEN);

        /* Hack -- enforce legal pval */
        if (e_ptr->e_flags1 & (TR1_PVAL_MASK))
        {
            /* Force a meaningful pval */
            if (!o_ptr->pval) o_ptr->pval = 1;
        }

        /* Mega-Hack - Enforce the special broken items */
        if ((o_ptr->ego_num == EGO_BLASTED) ||
            (o_ptr->ego_num == EGO_SHATTERED))
        {
            /* These were set to k_info values by preceding code */
            o_ptr->ac = 0;
            o_ptr->dd = 0;
            o_ptr->ds = 0;
        }
    }

    /* Hack -- keep boosted damage dice and sides */
    if (o_ptr->dd < old_dd) o_ptr->dd = old_dd;
    if (o_ptr->ds < old_ds) o_ptr->ds = old_ds;

    /* Hack -- *Identified* artifacts are known in future games */
    if ((o_ptr->ident & (IDENT_MENTAL)) && ARTIFACT_EASY_MENTAL(o_ptr))
    {
        /* Mark as *identified* */
        a_l_list[o_ptr->art_num].was_fully_identified = TRUE;
    }

    /* Success */
    return (0);
}


static bool rd_monster_races(void)
{
    u16b tmp16u;

    /* Monster Memory */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > z_info->r_max)
    {
        pop_up_message_box(QString("Too many (%1) monster races!") .arg(tmp16u));
        return (FALSE);
    }

    /* Read the available records */
    for (int i = 0; i < tmp16u; i++)
    {
        monster_race *r_ptr = &r_info[i];

        /* Read the "Racial" monster limit per level */
        rd_byte(&r_ptr->max_num);

        /* Hack - allow for new monsters from a modified monster list to appear in a current game. */

        /* In case of a monster entry that wasn't a unique is now made a unique.*/
        if (!(r_ptr->flags1 & (RF1_UNIQUE)))
        {
            r_ptr->max_num = 100;
        }
        /* Not a unique, but a new monster entry in the current game. */
        else if (r_ptr->max_num >1) r_ptr->max_num = 1;
    }

    return (TRUE);
}

/*
 * Read a monster
 */
static void rd_monster(monster_type *m_ptr)
{
    byte num;
    int i;

    /* Read the monster race */
    rd_s16b(&m_ptr->r_idx);

    /* Read the other information */
    rd_byte(&m_ptr->fy);
    rd_byte(&m_ptr->fx);
    rd_s16b(&m_ptr->hp);
    rd_s16b(&m_ptr->maxhp);
    rd_byte(&m_ptr->m_speed);
    rd_s16b(&m_ptr->m_energy);

    /* Find the number of monster timed effects */
    rd_byte(&num);

    if (num == MON_TMD_MAX)
    {
        /* Read all the effects */
        for (i = 0; i < num; i++) rd_s16b(&m_ptr->m_timed[i]);
    }
    else
    {
        s16b dummy;
        /* Probably in trouble anyway */
        for (i = 0; i < num; i++) rd_s16b(&dummy);

        message(QString("Discarding unsupported monster timed effects"));
    }

    rd_u32b(&m_ptr->mflag);
    rd_u32b(&m_ptr->smart);
    rd_byte(&m_ptr->target_y);
    rd_byte(&m_ptr->target_x);
    rd_byte(&m_ptr->mana);

    strip_bytes(1);
}


/*
 * Read an object
 *
 * This function attempts to "repair" old savefiles, and to extract
 * the most up to date values for various object fields.
 */
static int rd_effect(void)
{

    int x_idx;
    byte type;
    u16b f_idx;
    byte y;
    byte x;
    byte countdown;
    byte repeats;
    u16b power;
    s16b source;
    u16b flags;

    x_idx = x_pop();

    /*something is wrong*/
    if (!x_idx) return (-1);

    /*Read the effect*/
    rd_byte(&type);
    rd_u16b(&f_idx);


    rd_byte(&y);
    rd_byte(&x);

    rd_byte(&countdown);

    rd_byte(&repeats);

    rd_u16b(&power);

    rd_s16b(&source);

    rd_u16b(&flags);

    /*Write it, unless it is an empty effect*/
    if (type) effect_prep(x_idx, type, f_idx, y, x, countdown, repeats, power, source, flags);

    s16b r_idx;

    rd_s16b(&r_idx);

    x_list[x_idx].x_r_idx = (type ? r_idx: 0);

    /* Success */
    return (0);
}

static void rd_hotkey(int hotkey)
{
    single_hotkey this_hotkey;

    u16b num_steps;
    s16b tmp_s16b;
    byte tmp_byte;
    QString temp_string;

    rd_string(&this_hotkey.hotkey_name);
    rd_string(&this_hotkey.hotkey_button_name);
    rd_s16b(&tmp_s16b);
    this_hotkey.hotkey_button = tmp_s16b;
    rd_u16b(&num_steps);

    for (u16b i = 0; i < num_steps; i++)
    {
        hotkey_step this_step;
        rd_byte(&this_step.step_commmand);

        cmd_arg *args = &this_step.step_args;

        rd_string(&temp_string);
        args->string1 = temp_string;
        rd_string(&temp_string);
        args->string2 = temp_string;
        rd_s16b(&tmp_s16b);
        args->choice = tmp_s16b;
        rd_s16b(&tmp_s16b);
        args->item = tmp_s16b;
        rd_s16b(&tmp_s16b);
        args->number = tmp_s16b;
        rd_s16b(&tmp_s16b);
        args->direction = tmp_s16b;
        rd_s16b(&tmp_s16b);
        args->slot = tmp_s16b;
        rd_s16b(&tmp_s16b);
        args->repeats = tmp_s16b;
        rd_s16b(&tmp_s16b);
        args->k_idx = tmp_s16b;

        rd_byte (&tmp_byte);
        if (tmp_byte) args->verify = TRUE;
        else args->verify = FALSE;

        this_step.step_object.object_wipe();

        this_hotkey.hotkey_steps.append(this_step);
    }

    // Too many hotkeys.  Don't crash the game.
    if (hotkey >= NUM_HOTKEYS) return;

    player_hotkeys[hotkey].copy_hotkey(&this_hotkey);
}


/*
 * Read the monster lore
 */
static void rd_monster_lore(int r_idx)
{
    byte tmp8u;

    int i;

    monster_lore *l_ptr = &l_list[r_idx];
    monster_race *r_ptr = &r_info[r_idx];

    /* Count sights/deaths/kills */
    rd_s16b(&l_ptr->sights);
    rd_s16b(&l_ptr->deaths);
    rd_s16b(&l_ptr->pkills);
    rd_s16b(&l_ptr->tkills);

    /* Count wakes and ignores */
    rd_byte(&l_ptr->wake);
    rd_byte(&l_ptr->ignore);

    /* Extra stuff */
    rd_byte(&l_ptr->xtra1);
    rd_byte(&l_ptr->xtra2);

    /* Count drops */
    rd_byte(&l_ptr->drop_gold);
    rd_byte(&l_ptr->drop_item);

    rd_byte(&l_ptr->ranged);

    /* Count blows of each type */
    for (i = 0; i < MONSTER_BLOW_MAX; i++)
        rd_byte(&l_ptr->blows[i]);

    /* Memorize flags */
    rd_u32b(&l_ptr->r_l_flags1);
    rd_u32b(&l_ptr->r_l_flags2);
    rd_u32b(&l_ptr->r_l_flags3);
    rd_u32b(&l_ptr->r_l_flags4);
    rd_u32b(&l_ptr->r_l_flags5);
    rd_u32b(&l_ptr->r_l_flags6);
    rd_u32b(&l_ptr->r_l_flags7);
    rd_u32b(&l_ptr->r_l_native);

    /* Later (?) */
    rd_byte(&tmp8u);
    rd_byte(&tmp8u);
    rd_byte(&tmp8u);

    /* Repair the lore flags */
    l_ptr->r_l_flags1 &= r_ptr->flags1;
    l_ptr->r_l_flags2 &= r_ptr->flags2;
    l_ptr->r_l_flags3 &= r_ptr->flags3;
    l_ptr->r_l_flags4 &= r_ptr->flags4;
    l_ptr->r_l_flags5 &= r_ptr->flags5;
    l_ptr->r_l_flags6 &= r_ptr->flags6;
    l_ptr->r_l_flags7 &= r_ptr->flags7;
    l_ptr->r_l_native &= r_ptr->r_native;

}




/*
 * Read a store
 */
static int rd_store(int n)
{
    store_type *st_ptr = &store[n];

    int j;

    byte own, num;

    u32b extra32;
    s16b extra16;


    /* Read the basic info */
    rd_u32b(&extra32);
    rd_s16b(&extra16);
    rd_byte(&own);
    rd_byte(&num);
    rd_s16b(&extra16);
    rd_s16b(&extra16);

    /* Paranoia */
    if (own >= z_info->b_max)
    {

        pop_up_message_box("Illegal store owner!");
        return (-1);
    }

    st_ptr->owner = own;

    /* Read the items */
    for (j = 0; j < num; j++)
    {
        object_type *i_ptr;
        object_type object_type_body;

        /* Get local object */
        i_ptr = &object_type_body;

        /* Wipe the object */
        i_ptr->object_wipe();

        /* Read the item */
        if (rd_item(i_ptr))
        {
            pop_up_message_box("Error reading item");
            return (-1);
        }

        /* Accept any valid items */
        if (st_ptr->stock_num < STORE_INVEN_MAX)
        {
            int k = st_ptr->stock_num++;

            /* Accept the item */
            st_ptr->stock[k].object_copy(i_ptr);
        }
    }

    /* Success */
    return (0);
}


/*
 * Read artifact lore
 */
static int rd_artifact_lore(int a_idx)
{
    byte tmp8u;

    /* Read flags */
    rd_byte(&tmp8u);

    // Remember whether the artifact was fully identified.
    if (!tmp8u) a_l_list[a_idx].was_fully_identified = FALSE;
    else a_l_list[a_idx].was_fully_identified = TRUE;

    /* For future use */
    rd_byte(&tmp8u);
    rd_byte(&tmp8u);
    rd_byte(&tmp8u);

    return (0);
}


/*
 * Read terrain lore
 */
static int rd_feature_lore(int f_idx)
{
    /* Get the feature */
    feature_type *f_ptr = &f_info[f_idx];
    feature_lore *f_l_ptr = &f_l_list[f_idx];
    byte tmp8u;
    int i;

    rd_byte(&tmp8u);

    /* Activate the "everseen" flag, if needed */
    f_ptr->f_everseen = (tmp8u & 0x01);

    /* Write the terrain_lore memory*/
    rd_byte(&f_l_ptr->f_l_sights);

    /*Write the lore flags*/
    rd_u32b(&f_l_ptr->f_l_flags1);
    rd_u32b(&f_l_ptr->f_l_flags2);
    rd_u32b(&f_l_ptr->f_l_flags3);

    /* Repair the lore flags */
    f_l_ptr->f_l_flags1 &= f_ptr->f_flags1;
    f_l_ptr->f_l_flags2 &= f_ptr->f_flags2;
    f_l_ptr->f_l_flags3 &= f_ptr->f_flags3;

    rd_byte(&f_l_ptr->f_l_defaults);

    /*record the max amount of feat states*/
    rd_byte(&tmp8u);

    /*Failure, if the # of MAX FEAT STATES is desceased*/
    if (tmp8u > MAX_FEAT_STATES) return (-1);

    for (i = 0; i < tmp8u; i++)
    {
        rd_byte(&f_l_ptr->f_l_state[i]);
    }

    rd_byte(&f_l_ptr->f_l_power);

    rd_byte(&f_l_ptr->f_l_dam_non_native);
    rd_byte(&f_l_ptr->f_l_native_moves);
    rd_byte(&f_l_ptr->f_l_non_native_moves);
    rd_byte(&f_l_ptr->f_l_native_to_hit_adj);
    rd_byte(&f_l_ptr->f_l_non_native_to_hit_adj);
    rd_byte(&f_l_ptr->f_l_stealth_adj);


    /* Success */
    return (0);
}


/*
 * Read RNG state
 */
static void rd_randomizer(void)
{
    int i;

    u16b tmp16u;

    /* Tmp */
    rd_u16b(&tmp16u);

    /* Place */
    rd_u16b(&Rand_place);

    /* State */
    for (i = 0; i < RAND_DEG; i++)
    {
        rd_u32b(&Rand_state[i]);
    }

    /* Accept */
    Rand_quick = FALSE;
}



/*
 * Read options
 *
 * Note that the normal options are stored as a set of 256 bit flags,
 * plus a set of 256 bit masks to indicate which bit flags were defined
 * at the time the savefile was created.  This will allow new options
 * to be added, and old options to be removed, at any time, without
 * hurting old savefiles.
 *
 * The window options are stored in the same way, but note that each
 * window gets 32 options, and their order is fixed by certain defines.
 */
static void rd_options(void)
{
    int i, n;

    byte b;

    u32b flag[8];
    u32b mask[8];

    /*** Oops ***/

    /* Ignore old options */
    strip_bytes(16);


    /*** Special info */

    /* Read "animation_factor" */
    rd_byte(&b);
    // Hack - "Fix" animation delay factor
    if ((b < 25) || (b > 200))
    {
        b = 100;
    }
    op_ptr->delay_anim_factor = b;

    /* Read "hitpoint_warn" */
    rd_byte(&b);
    op_ptr->hitpoint_warn = b;

    /* Read "run delay" */
    rd_byte(&b);
    op_ptr->delay_run_factor = b;

    /* Old cheating options */
    strip_bytes(1);


    /*** Normal Options ***/

    /* Read the option flags */
    for (n = 0; n < 8; n++) rd_u32b(&flag[n]);

    /* Read the option masks */
    for (n = 0; n < 8; n++) rd_u32b(&mask[n]);

    /* Analyze the options */
    for (i = 0; i < OPT_MAX; i++)
    {
        int os = i / 32;
        int ob = i % 32;

        /* Process real entries */
        if (options[i].name.isEmpty()) continue;

        /* Process saved entries */
        if (mask[os] & (1L << ob))
        {
            /* Set flag */
            if (flag[os] & (1L << ob))
            {
                /* Set */
                op_ptr->opt[i] = TRUE;
            }

            /* Clear flag */
            else
            {
                /* Set */
                op_ptr->opt[i] = FALSE;

            }
        }
    }

    // Old ANGBAND_TERM_MAX window flags info
    strip_bytes(64);

}

static int rd_player_spells(void)
{
    int i;
    u16b tmp16u;

    /* Read the number of spells */
    rd_u16b(&tmp16u);
    if (tmp16u > PY_MAX_SPELLS)
    {
        pop_up_message_box(QString("Too many player spells (%1).") .arg(tmp16u));
        return (-1);
    }

    /* Read the spell flags */
    for (i = 0; i < tmp16u; i++)
    {
        rd_byte(&p_ptr->spell_flags[i]);
    }

    /* Read the spell order */
    for (i = 0; i < tmp16u; i++)
    {
        rd_byte(&p_ptr->spell_order[i]);
    }

    /* Success */
    return (0);
}


/*
 * Read the "extra" information
 */
static int rd_extra(void)
{
    int i;

    byte tmp8u;
    u16b tmp16u;
    u16b file_e_max;
    byte num;
    byte dummy_byte;


    rd_string(&op_ptr->full_name);
    rd_string(&p_ptr->died_from);

    rd_string(&p_ptr->history);

    /* Player race */
    rd_byte(&p_ptr->prace);

    /* Verify player race */
    if (p_ptr->prace >= z_info->p_max)
    {
        pop_up_message_box(QString("Invalid player race (%1).") .arg(p_ptr->prace));
        return (-1);
    }

    /* Player class */
    rd_byte(&p_ptr->pclass);

    /* Verify player class */
    if (p_ptr->pclass >= z_info->c_max)
    {
        pop_up_message_box(QString("Invalid player class (%1).")  .arg(p_ptr->pclass));
        return (-1);
    }

    /* Player gender */
    rd_byte(&p_ptr->psex);

    strip_bytes(1);

    /* Special Race/Class info */
    rd_byte(&p_ptr->hitdie);

    rd_byte(&p_ptr->expfact);

    /* Age/Height/Weight */
    rd_s16b(&p_ptr->age);
    rd_s16b(&p_ptr->ht);
    rd_s16b(&p_ptr->wt);

    /* Read the stat info */
    for (i = 0; i < A_MAX; i++) rd_s16b(&p_ptr->stat_base_max[i]);
    for (i = 0; i < A_MAX; i++) rd_s16b(&p_ptr->stat_base_cur[i]);
    for (i = 0; i < A_MAX; i++) rd_s16b(&p_ptr->stat_birth[i]);
    for (i = 0; i < A_MAX; ++i) rd_s16b(&p_ptr->stat_quest_add[i]);

    rd_s16b(&p_ptr->ht_birth);
    rd_s16b(&p_ptr->wt_birth);
    rd_s16b(&p_ptr->sc_birth);
    rd_s32b(&p_ptr->au_birth);

    strip_bytes(24);	/* oops */

    rd_u16b(&p_ptr->q_fame);
    rd_u16b(&p_ptr->deferred_rewards);

    rd_s32b(&p_ptr->au);

    rd_s32b(&p_ptr->max_exp);

    rd_s32b(&p_ptr->exp);

    rd_u16b(&p_ptr->exp_frac);

    rd_s16b(&p_ptr->lev);

    /* Verify player level */
    if ((p_ptr->lev < 1) || (p_ptr->lev > z_info->max_level))
    {
        pop_up_message_box(QString("Invalid player level (%1).")  .arg(p_ptr->lev));
        return (-1);
    }

    rd_s16b(&p_ptr->mhp);
    rd_s16b(&p_ptr->chp);
    rd_u16b(&p_ptr->chp_frac);

    rd_s16b(&p_ptr->msp);
    rd_s16b(&p_ptr->csp);
    rd_u16b(&p_ptr->csp_frac);

    rd_s16b(&p_ptr->max_lev);
    rd_s16b(&p_ptr->max_depth);

    rd_s16b(&p_ptr->recall_depth);

    /* Hack -- Repair maximum player level */
    if (p_ptr->max_lev < p_ptr->lev) p_ptr->max_lev = p_ptr->lev;

    /* Hack -- Repair maximum dungeon level */
    if (p_ptr->max_depth < 0) p_ptr->max_depth = 1;

    /* Hack -- Repair recall dungeon level */
    if (p_ptr->recall_depth < 0) p_ptr->recall_depth = 1;

    rd_s16b(&p_ptr->quest_depth);

    /* Hack -- Repair max quest level */
    if ((p_ptr->max_depth > 1) &&
        (p_ptr->max_depth > p_ptr->quest_depth))
    {
        p_ptr->quest_depth = p_ptr->max_depth;
    }

    /* More info */
    strip_bytes(6);
    rd_s16b(&p_ptr->sc);
    strip_bytes(2);

    /* Read the flags */
    strip_bytes(2);	/* Old "rest" */
    rd_s16b(&p_ptr->food);

    strip_bytes(4);	/* Old "food_digested" / "protection" */
    rd_s16b(&p_ptr->p_energy);
    rd_s16b(&p_ptr->word_recall);
    rd_s16b(&p_ptr->state.see_infra);

    rd_byte(&p_ptr->confusing);
    rd_byte(&tmp8u);
    rd_byte(&p_ptr->searching);
    rd_byte(&tmp8u);	/* oops */
    rd_byte(&tmp8u);	/* oops */
    rd_byte(&tmp8u);	/* oops */

    /* Find the number of timed effects */
    rd_byte(&num);

    if (num == TMD_MAX)
    {
        /* Read all the effects */
        for (i = 0; i < num; i++)  rd_s16b(&p_ptr->timed[i]);
    }
    else
    {
        s16b dummy2;

        /* Probably in trouble anyway */
        for (i = 0; i < TMD_MAX; i++) rd_s16b(&dummy2);

        /* Discard unused entries */
        pop_up_message_box("Discarding unsupported timed effects");
    }

    rd_s16b(&p_ptr->base_wakeup_chance);
    rd_s16b(&total_wakeup_chance);

    /* Read item-quality squelch sub-menu */
    for (i = 0; i < SQUELCH_BYTES; i++) rd_byte(&squelch_level[i]);

    /* Load the name of the current greater vault */
    rd_string(&g_vault_name);

    /* Read the number of saved ego-item types */
    rd_u16b(&file_e_max);

    /* Read ego-item squelch settings */
    for (i = 0; i < z_info->e_max; i++)
    {
        ego_item_type *e_ptr = &e_info[i];

        tmp8u = 0;

        if (i < file_e_max) rd_byte(&tmp8u);

        if (!tmp8u) e_ptr->squelch = FALSE;
        else e_ptr->squelch = TRUE;
    }

    /* Read possible extra elements */
    while (i < file_e_max)
    {
        rd_byte(&tmp8u);
        i++;
    }

    /*
     * The number of the bone file (if any) that player ghosts should use to
     * derive the ghost name, sex, class, race, and level.
     */
    rd_s16b(&player_ghost_num);

    /* Find out how many thefts have recently occurred. */
    strip_bytes(1);

    /* Read number of monster traps on level. */
    rd_byte(&num_trap_on_level);

    /* Future use */
    strip_bytes(13);

    /* Read the summon spells that have already failed on the level */
    rd_u32b(&dungeon_summon_mask_f7);

    /* Read the randart seed */
    rd_u32b(&seed_randart);

    /* Skip the flags */
    strip_bytes(12);

    /* Hack -- the three "special seeds" */
    rd_u32b(&seed_flavor);
    rd_u32b(&seed_town);
    rd_u32b(&seed_ghost);

    /* Special stuff */
    rd_u16b(&p_ptr->panic_save);
    rd_u16b(&p_ptr->total_winner);
    rd_byte(&dummy_byte);
    if (dummy_byte == TRUE) p_ptr->is_wizard = TRUE;
    else p_ptr->is_wizard = FALSE;

    /* Note "retirement */
    rd_byte(&tmp8u);
    p_ptr->terminated = tmp8u;

    /* Read "death" */
    rd_byte(&tmp8u);
    p_ptr->is_dead = tmp8u;

    /* Read "feeling" */
    rd_byte(&tmp8u);
    feeling = tmp8u;

    /*read the level feeling*/
    rd_byte(&tmp8u);
    do_feeling = tmp8u;

    /* Current turn */
    rd_s32b(&p_ptr->game_turn);

    /*Current Player Turn*/
    rd_s32b(&p_ptr->p_turn);

    /* Turn count for quest indicator */
    rd_u16b(&quest_indicator_timer);

    /* Check if the quest indicator must flash the victory sign */
    if (quest_indicator_timer & (QUEST_INDICATOR_COMPLETE_BIT))
    {
        /* We won the quest */
        quest_indicator_complete = TRUE;
        /* Clear the mark from the timer */
        quest_indicator_timer &= ~(QUEST_INDICATOR_COMPLETE_BIT);
    }

    /* Read the player_hp array */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > z_info->max_level)
    {
        pop_up_message_box(QString("Too many (%1) hitpoint entries!") .arg(tmp16u));
        return (-1);
    }

    /* Read the player_hp array */
    for (i = 0; i < tmp16u; i++)
    {
        rd_s16b(&p_ptr->player_hp[i]);
    }

    /* Read the player spells */
    if (rd_player_spells()) return (-1);

    return (0);
}

/*
 * Read the random artifacts
 */
static int rd_randarts(void)
{

    int i;
    byte tmp8u;
    s16b tmp16s;
    u16b tmp16u;
    u16b artifact_count, begin;
    s32b tmp32s;
    u32b tmp32u;

    /* Read the number of artifacts */
    rd_u16b(&begin);
    rd_u16b(&artifact_count);

    rd_u16b(&art_norm_count);

    /* Alive or cheating death */
    if (!p_ptr->is_dead || p_ptr->is_wizard)
    {
        /* Incompatible save files */
        if ((artifact_count > z_info->art_max) || (art_norm_count > z_info->art_norm_max))
        {
            pop_up_message_box(QString("Too many (%1) artifacts!") .arg(artifact_count));
            return (-1);
        }
        /*Mark any new added artifacts*/
        if (art_norm_count < z_info->art_norm_max)
        {
            new_artifacts = z_info->art_norm_max - art_norm_count;
        }
        else new_artifacts = 0;

        /* Mark the old artifacts as "empty" */
        for (i = begin; i < z_info->art_max; i++)
        {
            artifact_type *a_ptr = &a_info[i];

            /*hack - if a new "normal artifact has been added in mid-game, don't erase it*/
            if ((i >= art_norm_count) && (i < z_info->art_norm_max)) continue;

            a_ptr->tval = 0;
            a_ptr->sval = 0;
            a_ptr->a_name.clear();
        }

        /* Read the artifacts */
        for (i = begin; i < artifact_count; i++)
        {

            artifact_type *a_ptr = &a_info[i];

            /*hack - if a new "normal artifact has been added in mid-game, don't erase it*/
            if ((i >= art_norm_count) && (i < z_info->art_norm_max)) continue;

            rd_string (&a_ptr->a_name);

            rd_byte(&a_ptr->tval);
            rd_byte(&a_ptr->sval);
            rd_s16b(&a_ptr->pval);

            rd_s16b(&a_ptr->to_h);
            rd_s16b(&a_ptr->to_d);
            rd_s16b(&a_ptr->to_a);
            rd_s16b(&a_ptr->ac);
            rd_byte(&a_ptr->dd);
            rd_byte(&a_ptr->ds);
            rd_s16b(&a_ptr->weight);
            rd_s32b(&a_ptr->cost);
            rd_u32b(&a_ptr->a_flags1);
            rd_u32b(&a_ptr->a_flags2);
            rd_u32b(&a_ptr->a_flags3);
            rd_u32b(&a_ptr->a_native);
            rd_byte(&a_ptr->a_level);
            rd_byte(&a_ptr->a_rarity);
            rd_byte(&a_ptr->activation);
            rd_u16b(&a_ptr->time);
            rd_u16b(&a_ptr->randtime);
        }
    }
    else
    {
        /* Strip the the artifacts for a dead/new character*/
        for (i = begin; i < artifact_count; i++)
        {
            QString tmpstr;
            rd_string (&tmpstr); /*a_ptr->name*/
            rd_byte(&tmp8u); /* a_ptr->tval */
            rd_byte(&tmp8u); /* a_ptr->sval */
            rd_s16b(&tmp16s); /* a_ptr->pval */

            rd_s16b(&tmp16s); /* a_ptr->to_h */
            rd_s16b(&tmp16s); /* a_ptr->to_d */
            rd_s16b(&tmp16s); /* a_ptr->to_a */
            rd_s16b(&tmp16s); /* a_ptr->ac */

            rd_byte(&tmp8u); /* a_ptr->dd */
            rd_byte(&tmp8u); /* a_ptr->ds */

            rd_s16b(&tmp16s); /* a_ptr->weight */
            rd_s32b(&tmp32s); /* a_ptr->cost */

            rd_u32b(&tmp32u); /* a_ptr->flags1 */
            rd_u32b(&tmp32u); /* a_ptr->flags2 */
            rd_u32b(&tmp32u); /* a_ptr->flags3 */
            rd_u32b(&tmp32u); /* a_ptr->a_native */
            rd_byte(&tmp8u); /* a_ptr->level */
            rd_byte(&tmp8u); /* a_ptr->rarity */

            rd_byte(&tmp8u); /* a_ptr->activation */
            rd_u16b(&tmp16u); /* a_ptr->time */
            rd_u16b(&tmp16u); /* a_ptr->randtime */
        }
    }

    return (0);

}

/*
 * Read the notes. Every new savefile has at least NOTES_MARK.
 */
static bool rd_notes(void)
{
    int alive = (!p_ptr->is_dead || p_ptr->is_wizard);

    u16b num;

    rd_u16b(&num);

    /*
     * Either read the notes or strip them from the
     * savefile, depending on whether the character
     * is alive or dead.
     */
    for (int i = 0; i < num; i++)
    {
        QString tmpstr;
        byte tmp_level;
        s16b tmp_depth;
        s32b tmp_turn;
        notes_type notes_body;
        notes_type *notes_ptr = &notes_body;

        rd_byte(&tmp_level);
        rd_s16b(&tmp_depth);
        rd_s32b(&tmp_turn);
        rd_string(&tmpstr);

        notes_ptr->player_level = tmp_level;
        notes_ptr->dun_depth = tmp_depth;
        notes_ptr->game_turn = tmp_turn;
        notes_ptr->recorded_note = tmpstr;

        // Only keep notes for living character.
        if (alive) notes_log.append(notes_body);
    }

    return (FALSE);
}






/*
 * Read the player inventory
 *
 * Note that the inventory is "re-sorted" later by "dungeon()".
 */
static int rd_inventory(void)
{
    int slot = 0;

    object_type *i_ptr;
    object_type object_type_body;

    /* Read until done */
    while (1)
    {
        u16b n;

        /* Get the next item index */
        rd_u16b(&n);

        /* Nope, we reached the end */
        if (n == 0xFFFF) break;

        /* Get local object */
        i_ptr = &object_type_body;

        /* Wipe the object */
        i_ptr->object_wipe();

        /* Read the item */
        if (rd_item(i_ptr))
        {
            pop_up_message_box("Error reading item");
            return (-1);
        }

        /* Hack -- verify item */
        if (!i_ptr->k_idx)	return (-1);

        /* Verify slot */
        if (n >= ALL_INVEN_TOTAL) return (-1);

        /* Wield equipment */
        if (n >= INVEN_WIELD)
        {
            /* Copy object */
            inventory[n].object_copy(i_ptr);

            /* One more item */
            if (!IS_QUIVER_SLOT(n)) p_ptr->equip_cnt++;
        }

        /* Warning -- backpack is full */
        else if (p_ptr->inven_cnt == INVEN_PACK)
        {
            /* Oops */
            pop_up_message_box("Too many items in the inventory!");

            /* Fail */
            return (-1);
        }

        /* Carry inventory */
        else
        {
            /* Get a slot */
            n = slot++;

            /* Copy object */
            inventory[n].object_copy(i_ptr);

            /* One more item */
            p_ptr->inven_cnt++;
        }

        /* Update "p_ptr->pack_size_reduce" */
        save_quiver_size();
    }

    /* Success */
    return (0);
}



/*
 * Read the saved messages
 */
static void rd_messages(void)
{
    u16b num;

    rd_u16b(&num);

    /* Read the messages */
    for (int i = 0; i < num; i++)
    {
        QString buf;
        byte red, green, blue, append;
        u16b repeats;
        s32b msg_turn;
        message_type message_body;
        message_type *msg_ptr = &message_body;

        /* Read the message */
        rd_string(&buf);

        msg_ptr->message = buf;

        /* Read the color */
        rd_byte(&red);
        rd_byte(&green);
        rd_byte(&blue);

        msg_ptr->msg_color.setRgb(red, green, blue, 255);

        rd_u16b(&repeats);
        rd_s32b(&msg_turn);
        msg_ptr->repeats = repeats;
        msg_ptr->message_turn = msg_turn;
        msg_ptr->displayed = FALSE;

        rd_byte(&append);
        msg_ptr->append = append;

        /* Save the message, backward  */
        // Add the message at the beginning of the list
        message_list.append(message_body);
    }
}

static void rd_scores(void)
{
    u16b num;

    rd_u16b(&num);

    /* Read the scores */
    for (int i = 0; i < num; i++)
    {
        high_score this_score;
        high_score *score_ptr = &this_score;

        rd_string(&score_ptr->version);
        rd_u32b(&score_ptr->score);
        rd_s32b(&score_ptr->turns);
        rd_string(&score_ptr->date_time);
        rd_string(&score_ptr->p_name);
        rd_string(&score_ptr->p_sex);
        rd_string(&score_ptr->p_race);
        rd_string(&score_ptr->p_class);
        rd_s16b(&score_ptr->cur_level);
        rd_s16b(&score_ptr->cur_depth);
        rd_s32b(&score_ptr->cur_exp);
        rd_s16b(&score_ptr->max_level);
        rd_s16b(&score_ptr->max_depth);
        rd_s32b(&score_ptr->max_exp);
        rd_u16b(&score_ptr->fame);
        rd_string(&score_ptr->death_how);


        player_scores_list.append(this_score);
    }
}


/*
 * Read the dungeon
 *
 * The monsters/objects must be loaded in the same order
 * that they were stored, since the actual indexes matter.
 *
 * Note that the size of the dungeon is now hard-coded to
 * DUNGEON_HGT by DUNGEON_WID, and any dungeon with another
 * size will be silently discarded by this routine.
 *
 * Note that dungeon objects, including objects held by monsters, are
 * placed directly into the dungeon, using "object_copy()", which will
 * copy "iy", "ix", and "held_m_idx", leaving "next_o_idx" blank for
 * objects held by monsters, since it is not saved in the savefile.
 *
 * After loading the monsters, the objects being held by monsters are
 * linked directly into those monsters.
 */
static int rd_dungeon(void)
{
    int i, y, x;

    s16b depth;
    s16b py, px;

    byte count;
    byte tmp8u;

    u16b limit;


    /*** Basic info ***/

    /* Header info */
    rd_s16b(&depth);
    rd_u16b(&p_ptr->dungeon_type);
    /* Get dungeon capabilities */
    set_dungeon_type(p_ptr->dungeon_type);

    rd_s16b(&py);
    rd_s16b(&px);
    rd_byte(&p_ptr->cur_map_hgt);
    rd_byte(&p_ptr->cur_map_wid);
    rd_u16b(&altered_inventory_counter);
    /* Paranoia */
    allow_altered_inventory = FALSE;

    rd_s16b(&p_ptr->create_stair);

    /* Ignore illegal dungeons */
    if ((depth < 0) || (depth >= MAX_DEPTH))
    {
        pop_up_message_box(QString("Ignoring illegal dungeon depth (%1)") .arg(depth));
        return (-1);
    }

    /* Ignore illegal dungeons */
    if ((p_ptr->cur_map_hgt > MAX_DUNGEON_HGT) || (p_ptr->cur_map_wid > MAX_DUNGEON_WID))
    {
        /* XXX XXX XXX */
        pop_up_message_box(QString("Ignoring illegal dungeon size (%1,%2).") .arg(p_ptr->cur_map_hgt) .arg(p_ptr->cur_map_wid));
        return (-1);
    }

    /* Ignore illegal dungeons */
    if ((px < 0) || (px >= p_ptr->cur_map_wid) ||
        (py < 0) || (py >= p_ptr->cur_map_hgt))
    {
        pop_up_message_box(QString("Ignoring illegal player location (%1,%2).") .arg(py) .arg(px));
        return (-1);
    }


    /*** Run length decoding ***/

    /* Load the dungeon data */
    for (x = y = 0; y < p_ptr->cur_map_hgt; )
    {
        /* Grab RLE info */
        rd_byte(&count);
        rd_byte(&tmp8u);

        /* Apply the RLE info */
        for (i = count; i > 0; i--)
        {
            /* Extract "info" */
            dungeon_info[y][x].cave_info = tmp8u;

            /* Advance/Wrap */
            if (++x >= p_ptr->cur_map_wid)
            {
                /* Wrap */
                x = 0;

                /* Advance/Wrap */
                if (++y >= p_ptr->cur_map_hgt) break;
            }
        }
    }


    /*** Run length decoding ***/

    /* Load the dungeon data */
    for (x = y = 0; y < p_ptr->cur_map_hgt; )
    {
        /* Grab RLE info */
        rd_byte(&count);
        rd_byte(&tmp8u);

        /* Apply the RLE info */
        for (i = count; i > 0; i--)
        {
            feature_type *f_ptr;

            /* Extract "feat" */
            dungeon_info[y][x].feature_idx = tmp8u;

            update_los_proj_move(y, x);

            /* Get fast access to feature */
            f_ptr = &f_info[tmp8u];            

            /* Handle glowing grids */
            if (_feat_ff2_match(f_ptr, FF2_GLOW))
            {
                int d;

                /* Turn on super glow */
                dungeon_info[y][x].cave_info |= (CAVE_HALO);

                /* Spread super glow through adjacent grids */
                for (d = 0; d < 8; d++)
                {
                    /* Get coordinates */
                    int yy = y + ddy_ddd[d];
                    int xx = x + ddx_ddd[d];

                    /* Ignore annoying locations */
                    if (!in_bounds_fully(yy, xx))
                    {
                        continue;
                    }

                    /* Turn on super glow */
                    dungeon_info[yy][xx].cave_info |= (CAVE_HALO);
                }
            }

            /* Register dynamic features */
            if (_feat_ff3_match(f_ptr, FF3_DYNAMIC))
            {
                (void)add_dynamic_terrain(y, x);
            }

            /* Update the flags of the current level */
            if (_feat_ff3_match(f_ptr, TERRAIN_MASK))
            {
                level_flag |= get_level_flag((u16b)(f_ptr - f_info));
            }

            /* Advance/Wrap */
            if (++x >= p_ptr->cur_map_wid)
            {
                /* Wrap */
                x = 0;

                /* Advance/Wrap */
                if (++y >= p_ptr->cur_map_hgt) break;
            }
        }
    }


    /*** Player ***/

    /* Load depth */
    p_ptr->depth = depth;

    /* Place player in dungeon */
    if (!player_place(py, px))
    {        
        pop_up_message_box(QString("Cannot place player (%1,%2)!").arg(py) .arg(px));
        return (-1);
    }

    /*** Objects ***/

    /* Read the item count */
    rd_u16b(&limit);

    /* Verify maximum */
    if (limit > z_info->o_max)
    {
        pop_up_message_box(QString("Too many (%1) object entries!") .arg(limit));
        return (-1);
    }

    /* Read the dungeon items */
    for (i = 1; i < limit; i++)
    {
        object_type *i_ptr;
        object_type object_type_body;

        s16b o_idx;
        object_type *o_ptr;


        /* Get the object */
        i_ptr = &object_type_body;

        /* Wipe the object */
        i_ptr->object_wipe();

        /* Read the item */
        if (rd_item(i_ptr))
        {
            pop_up_message_box("Error reading item");
            return (-1);
        }

        /* Make an object */
        o_idx = o_pop();

        /* Paranoia */
        if (o_idx != i)
        {
            pop_up_message_box(QString("Cannot place object %1!") .arg(i));
            return (-1);
        }

        /* Get the object */
        o_ptr = &o_list[o_idx];

        /* Structure Copy */
        o_ptr->object_copy(i_ptr);

        /* Dungeon floor */
        if (!i_ptr->held_m_idx)
        {
            int x = i_ptr->ix;
            int y = i_ptr->iy;

            /* ToDo: Verify coordinates */

            /* Link the object to the pile */
            o_ptr->next_o_idx = dungeon_info[y][x].object_idx;

            /* Link the floor to the object */
            dungeon_info[y][x].object_idx = o_idx;

            /* Rearrange stack if needed */
            rearrange_stack(y, x);
        }
    }


    /*** Monsters ***/

    if (!rd_monster_races()) return (-1);

    /* Read the monster count */
    rd_u16b(&limit);

    /* Hack -- verify */
    if (limit > z_info->m_max)
    {
        pop_up_message_box(QString("Too many (%1) monster entries!") .arg(limit));
        return (-1);
    }

    /* Read the monsters */
    for (i = 1; i < limit; i++)
    {
        monster_type *n_ptr;
        monster_type monster_type_body;
        monster_race *r_ptr;

        int r_idx;

        /* Get local monster */
        n_ptr = &monster_type_body;

        /* Clear the monster */
        n_ptr->monster_wipe();

        /* Read the monster */
        rd_monster(n_ptr);

        /* Access the "r_idx" of the chosen monster */
        r_idx = n_ptr->r_idx;

        /* Access the actual race */
        r_ptr = &r_info[r_idx];

        /* If a player ghost, some special features need to be added. */
        if (r_ptr->flags2 & (RF2_PLAYER_GHOST))
        {
            //prepare ghost
            (void)prepare_ghost(n_ptr->r_idx);
        }

        /* Place monster in dungeon */
        if (monster_place(n_ptr->fy, n_ptr->fx, n_ptr) != i)
        {
            pop_up_message_box(QString("Cannot place monster %1") .arg(i));
            return (-1);
        }
    }


    /*** Holding ***/

    /* Reacquire objects */
    for (i = 1; i < o_max; ++i)
    {
        object_type *o_ptr;

        monster_type *m_ptr;

        /* Get the object */
        o_ptr = &o_list[i];

        /* Ignore dungeon objects */
        if (!o_ptr->held_m_idx) continue;

        /* Verify monster index */
        if (o_ptr->held_m_idx > z_info->m_max)
        {
            pop_up_message_box("Invalid monster index");
            return (-1);
        }

        /* Get the monster */
        m_ptr = &mon_list[o_ptr->held_m_idx];

        /* Link the object to the pile */
        o_ptr->next_o_idx = m_ptr->hold_o_idx;

        /* Link the monster to the object */
        m_ptr->hold_o_idx = i;
    }

    /*** Effects ***/
    /* Read the effect count */
    rd_u16b(&limit);

    /* Verify maximum */
    if (limit > z_info->x_max)
    {
        pop_up_message_box(QString("Too many (%1) effect entries!") .arg(limit));
        return (-1);
    }

    /* Read the dungeon items */
    for (i = 1; i < limit; i++)
    {

        /* Read the item */
        if (rd_effect())
        {
            pop_up_message_box("Error reading effect");
            return (-1);
        }
    }

    // Read the hotkeys
    for (i = 0; i < NUM_HOTKEYS; i++)
    {
        rd_hotkey(i);
    }


    /*** Success ***/

    /* The dungeon is ready */
    character_dungeon = TRUE;

    /* Success */
    return (0);
}

/*
 * Actually read the savefile
 */
static int rd_savefile(void)
{
    int i;

    byte tmp8u;
    u16b tmp16u;
    u32b tmp32u;

    QLabel status_update;
    status_update.setText (QString("Loading a %1.%2.%3 savefile...") .arg(sf_major) .arg(sf_minor) .arg(sf_patch));
    status_update.show();

    /* Operating system info */
    rd_u32b(&sf_xtra);

    /* Time of savefile creation */
    rd_u32b(&sf_when);

    /* Number of resurrections */
    rd_u16b(&sf_lives);

    /* Number of times played */
    rd_u16b(&sf_saves);

    /* Later use (always zero) */
    rd_u32b(&tmp32u);

    /* Later use (always zero) */
    rd_u32b(&tmp32u);

    /* Read RNG state */
    rd_randomizer();
    if (arg_fiddle) status_update.setText (QString(QObject::tr("Loaded Randomizer Info")));

    /* Then the options */
    rd_options();
    if (arg_fiddle) status_update.setText (QString(QObject::tr("Loaded Option Flags")));

    /* Then the "messages" */
    rd_messages();
    if (arg_fiddle) status_update.setText (QString(QObject::tr("Loaded Messages")));

    /* Object Memory */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > z_info->k_max)
    {
        pop_up_message_box(QString("Too many (%1) object kinds!") .arg(tmp16u));
        return (-1);
    }

    /* Read the object memory */
    for (i = 0; i < tmp16u; i++)
    {
        byte tmp8u;

        object_kind *k_ptr = &k_info[i];

        rd_byte(&tmp8u);

        k_ptr->aware = (tmp8u & 0x01) ? TRUE: FALSE;
        k_ptr->tried = (tmp8u & 0x02) ? TRUE: FALSE;

        rd_byte(&k_ptr->squelch);

        // Read the object kind verify bool array
        for (int i = 0; i < VERIFY_MAX; i++)
        {
            rd_byte(&tmp8u);
            k_ptr->use_verify[i] = tmp8u;
        }

        rd_string(&k_ptr->autoinscribe);
    }
    if (arg_fiddle) pop_up_message_box("Loaded Object Memory");

    /* Load the Quests */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > z_info->q_max)
    {
        pop_up_message_box(QString("Too many (%1) quests!") .arg(tmp16u));
        return (23);
    }

    /* Load the Quests */
    for (i = 0; i < tmp16u; i++)
    {
        quest_type *q_ptr = &q_info[i];

        rd_byte(&q_ptr->q_type);
        /* Only limited info for permanent quests.  The rest is detailed in quest.txt */
        if (q_ptr->q_type == QUEST_PERMANENT)
        {
            rd_byte(&q_ptr->q_flags);
            rd_s16b(&q_ptr->q_num_killed);
            continue;
        }

        rd_u16b(&q_ptr->q_reward);
        rd_u16b(&q_ptr->q_fame_inc);
        rd_byte(&q_ptr->base_level);
        rd_byte(&q_ptr->q_theme);
        rd_s16b(&q_ptr->mon_idx);
        rd_s32b(&q_ptr->turn_counter);
        rd_s16b(&q_ptr->q_num_killed);
        rd_s16b(&q_ptr->q_max_num);
        rd_byte(&q_ptr->q_flags);
    }

    if (arg_fiddle) pop_up_message_box("Loaded Quests");

    /* Load the Artifacts */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > z_info->art_max)
    {
        pop_up_message_box(QString("Too many (%1) artifacts!") .arg(tmp16u));
        return (-1);
    }

    /* Read the artifact flags */
    for (i = 0; i < tmp16u; i++)
    {
        rd_byte(&tmp8u);
        a_info[i].a_cur_num = tmp8u;
        rd_byte(&tmp8u);
        rd_byte(&tmp8u);
        rd_byte(&tmp8u);
    }
    if (arg_fiddle) pop_up_message_box("Loaded Artifacts");

    /* Read the extra stuff */
    if (rd_extra()) return (-1);    
    if (arg_fiddle) pop_up_message_box("Loaded extra information");

    if (rd_randarts()) return (-1);
    if (arg_fiddle) pop_up_message_box("Loaded Random Artifacts");

    if (rd_notes()) return (-1);
    if (arg_fiddle) pop_up_message_box("Loaded Notes");

    /* Important -- Initialize the sex */
    sp_ptr = &sex_info[p_ptr->psex];

    /* Important -- Initialize the race/class */
    rp_ptr = &p_info[p_ptr->prace];
    cp_ptr = &c_info[p_ptr->pclass];

    /* Important -- Initialize the magic */
    mp_ptr = &cp_ptr->spells;

    /* Hack - In NPP 050, we moved a spell out of ironman book.*/
    if (cp_ptr->spell_book == TV_MAGIC_BOOK)  p_ptr->spell_flags[SPELL_FLIGHT] &= ~(PY_SPELL_IRONMAN);

    /* Read the inventory */
    if (rd_inventory())
    {
        if (arg_fiddle) status_update.setText (QString(QObject::tr("Unable to read inventory")));
        return (-1);
    }

    /* Read the stores */
    rd_u16b(&tmp16u);
    for (i = 0; i < tmp16u; i++)
    {
        if (rd_store(i)) return (-1);
    }    

    /* I'm not dead yet... */
    if (!p_ptr->is_dead)
    {
        /* Dead players have no dungeon */
        if (arg_fiddle) status_update.setText (QString(QObject::tr("Restoring Dungeon...")));
        if (rd_dungeon())
        {
            pop_up_message_box("Error reading dungeon data");
            return (-1);
        }

    }

    /* Success */
    return (0);
}

/* Open the basic savefile, parses the contents into bytes,
 * verify correct savefile version #
 * returns false if anything fails
 */
static int read_savefile(void)
{
    /* Allow empty savefile name */
    if (current_savefile.isEmpty()) return (FALSE);

    save_file.setFileName(current_savefile);

    /* Open the savefile */
    if (!save_file.exists())
    {
        /* Give a message */
        pop_up_message_box("Savefile does not exist.");

        /* Allow this */
        return (FALSE);
    }

    /* Okay */
    if (!save_file.open(QIODevice::ReadOnly))
    {
        /* Message (below) */
        pop_up_message_box("Cannot open savefile");

        return (FALSE);
    }

    // Ensure the data is read and written consistently
    in.setVersion(QDataStream::Qt_5_2);

    /* Extract version */
    rd_byte(&sf_major);
    rd_byte(&sf_minor);
    rd_byte(&sf_patch);
    rd_byte(&sf_extra);

    if (older_than(OLD_VERSION_MAJOR, OLD_VERSION_MINOR, OLD_VERSION_PATCH))
    {
        pop_up_message_box("Savefile is too old.");
        return (FALSE);
    }
    if (!older_than(VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH + 1))
    {
        pop_up_message_box("Savefile is from the future.");
        return (FALSE);

    }

    // Success
    return (TRUE);
}

/* Open and read the player scores file,
 * verify correct savefile version #
 * returns false if anything fails
 */
static bool load_scores(void)
{
    byte this_game_mode;

    QString scores_filename = QString("scores.npp");

    if (game_mode == GAME_NPPANGBAND) scores_filename.prepend("nppangband_");
    else if (game_mode == GAME_NPPMORIA) scores_filename.prepend("nppmoria_");
    else return (FALSE);

    scores_filename.prepend(QString("%1/") .arg(npp_dir_bone.path()));

    save_file.setFileName(scores_filename);

    /* Okay */
    if (!save_file.open(QIODevice::ReadOnly))
    {
        return (FALSE);
    }

    // Ensure the data is read and written consistently
    in.setVersion(QDataStream::Qt_5_2);

    /* Extract version */
    rd_byte(&sf_major);
    rd_byte(&sf_minor);
    rd_byte(&sf_patch);
    rd_byte(&sf_extra);
    rd_byte(&this_game_mode);

    if (this_game_mode!= game_mode)
    {
        return (FALSE);
    }

    if (!older_than(VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH + 1))
    {
        return (FALSE);
    }

    rd_scores();

    save_file.close();

    // Success
    return (TRUE);
}

/* Open and read the player scores file,
 * verify correct savefile version #
 * returns false if anything fails
 */
static bool load_memory(void)
{
    byte this_game_mode;

    u16b tmp16u;
    byte tmp8u;
    int i;

    QString memory_filename = QString("memory.npp");

    if (game_mode == GAME_NPPANGBAND) memory_filename.prepend("nppangband_");
    else if (game_mode == GAME_NPPMORIA) memory_filename.prepend("nppmoria_");
    else return (FALSE);

    memory_filename.prepend(QString("%1/") .arg(npp_dir_bone.path()));

    save_file.setFileName(memory_filename);

    /* Okay */
    if (!save_file.open(QIODevice::ReadOnly))
    {
        return (FALSE);
    }

    // Ensure the data is read and written consistently
    in.setVersion(QDataStream::Qt_5_2);

    /* Extract version */
    rd_byte(&sf_major);
    rd_byte(&sf_minor);
    rd_byte(&sf_patch);
    rd_byte(&sf_extra);
    rd_byte(&this_game_mode);

    if (this_game_mode!= game_mode)
    {
        return (FALSE);
    }

    if (!older_than(VERSION_MAJOR, VERSION_MINOR, VERSION_PATCH + 1))
    {
        return (FALSE);
    }


    /* Monster Memory */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > z_info->r_max)
    {
        return (-1);
    }

    /* Read the available records */
    for (i = 0; i < tmp16u; i++)
    {
        /* Read the lore */
        rd_monster_lore(i);
    }

    /* Read the stored number of terrain features */
    rd_u16b(&tmp16u);

    /* Check bounds */
    if (tmp16u > z_info->f_max)
    {
        return (FALSE);
    }

    /* Read terrain lore */
    for (i = 0; i < tmp16u; i++)
    {
        if (rd_feature_lore(i)) return (FALSE);
    }

    /* Artifact lore */
    /* Read the stored number of artifacts (normal + special) */
    rd_u16b(&tmp16u);

    /* Check bounds */
    if (tmp16u > z_info->art_norm_max)
    {
        return (FALSE);
    }

    /* Read artifact lore */
    for (i = 0; i < tmp16u; i++)
    {
        if (rd_artifact_lore(i)) return (FALSE);
    }

     /* Object Memory */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > z_info->k_max)
    {
        return (FALSE);
    }

    /* Read object everseen */
    for (i = 0; i < tmp16u; i++)
    {
        object_kind *k_ptr = &k_info[i];

        rd_byte(&tmp8u);

        if (!tmp8u) k_ptr->everseen = FALSE;
        else k_ptr->everseen = TRUE;

        /* Hack - Repair the savefile */
        if (!k_ptr->everseen) k_ptr->squelch = SQUELCH_NEVER;
    }

    /* Read the number of saved ego-item types */
    rd_u16b(&tmp16u);

    /* Incompatible save files */
    if (tmp16u > z_info->e_max)
    {
        return (FALSE);
    }

    /* Read ego-item squelch settings */
    for (i = 0; i < tmp16u; i++)
    {
        ego_item_type *e_ptr = &e_info[i];

        tmp8u = 0;

        if (i < tmp16u) rd_byte(&tmp8u);

        if (!tmp8u) e_ptr->everseen = FALSE;
        else e_ptr->everseen = TRUE;

        /* Hack - Repair the savefile */
        if (!e_ptr->everseen) e_ptr->squelch = FALSE;
    }

    save_file.close();

    // Success
    return (TRUE);
}

/*
 * Attempt to Load a "savefile"
 *
 * We return "TRUE" if the savefile was usable, and we set the global
 * flag "character_loaded" if a real, living, character was loaded.
 *
 * Assumes the game_mode has already been read.
 */
bool load_player(void)
{
    byte savefile_game;

    /* We start empty */
    character_loaded = false;

    /* Paranoia */
    p_ptr->game_turn = 0;

    p_ptr->p_turn = 0;

    /* Paranoia */
    p_ptr->is_dead = FALSE;

    // load player ghost files
    load_player_ghost_file();

    /* Open the savefile */
    if (!read_savefile()) return (FALSE);

    // Load the game mode
    rd_byte(&savefile_game);

    if (game_mode != savefile_game)
    {
        if (game_mode == GAME_NPPMORIA)         pop_up_message_box("Not a NPPMoria savefile");
        else if (game_mode == GAME_NPPANGBAND)  pop_up_message_box("Not a NPPAngband savefile");
        else                                    pop_up_message_box("Unknown savefile type");

        // We are done with the file
        save_file.close();
        return (FALSE);
    }
    else if (rd_savefile())
    {
        /* Attempt to load */
        pop_up_message_box("Cannot parse savefile");

        // We are done with the file
        save_file.close();
        return (FALSE);
    }

    /* Give a conversion warning */
    if ((VERSION_MAJOR != sf_major) ||
        (VERSION_MINOR != sf_minor) ||
        (VERSION_PATCH != sf_patch))
    {
        /* Message */
        pop_up_message_box(QString("Converted a %1.%2.%3 savefile.")
                               .arg(sf_major) .arg(sf_minor) .arg(sf_patch));
    }

    /* Player is dead */
    if (p_ptr->is_dead)
    {
        /*note, add or_true to the arg wizard if statement to resurrect character*/
        /* Cheat death (unless the character retired) */
        if ((p_ptr->is_wizard) && (!p_ptr->terminated))
        {
            /*heal the player*/
            hp_player(2000);

            /* Forget death */
            p_ptr->is_dead = FALSE;

            /* A character was loaded */
            character_loaded = TRUE;

            // We are done with the file
            save_file.close();

            /* Done */
            return (TRUE);
        }

        /* Forget death */
        p_ptr->is_dead = FALSE;

        /* Count lives */
        sf_lives++;

        /* Forget turns */
        p_ptr->game_turn = 0;

        p_ptr->p_turn = 0;

        // We are done with the file
        save_file.close();

        /* Done */
        return (TRUE);
    }

    /* A character was loaded */
    character_loaded = TRUE;

    /* Still alive */
    if (p_ptr->chp >= 0)
    {
        /* Reset cause of death */
        p_ptr->died_from = "(alive and well)";
    }

    // We are done with the file
    save_file.close();

    /* Success */
    return (TRUE);
}

void load_memory_scores(void)
{
    (void)load_scores();
    (void)load_memory();
}

/*
 * Attempt to Load a gamemode
 *
 * returns false if an improper game_mode is read;
 */
bool load_gamemode(void)
{
    byte savefile_game;

    /* Open the savefile */
    if (!read_savefile()) return (FALSE);

    // Load the game mode
    rd_byte(&savefile_game);

    // We are done with the file
    save_file.close();

    if (savefile_game == GAME_NPPMORIA)
    {
        game_mode = GAME_NPPMORIA;
    }
    else if (savefile_game == GAME_NPPANGBAND)
    {
        game_mode = GAME_NPPANGBAND;
    }

    // Something is wrong
    else
    {
        pop_up_message_box("Unknown savefile type");
        return (FALSE);
    }

    // Success
    return (TRUE);
}

void do_hotkey_import(QString file_name)
{
    save_file.setFileName(file_name);

    /* Okay */
    if (!save_file.open(QIODevice::ReadOnly))
    {
        pop_up_message_box("Hotkey Import Failed.  Invalid filename.");
        return;
    }

    // Ensure the data is read and written consistently
    in.setVersion(QDataStream::Qt_5_2);

    byte this_game_mode;
    rd_byte(&this_game_mode);

    if (this_game_mode!= game_mode)
    {
        if (this_game_mode == GAME_NPPMORIA) pop_up_message_box("Hotkey Import Failed. Hotkey file is for NPPMoria.");
        else pop_up_message_box("Hotkey Import Failed. Hotkey file is for NPPAngband.");
        save_file.close();
        return;
    }

    for (int i = 0; i < NUM_HOTKEYS; i++)
    {
        rd_hotkey(i);
    }

    save_file.close();
}
