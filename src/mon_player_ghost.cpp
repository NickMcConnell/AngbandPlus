/* File: player_ghost.cpp */

/*
 * Copyright (c) 2010 Jeff Greene and DIego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include <src/init.h>
#include <src/npp.h>
#include <QTextStream>

/*
 * Add various player ghost attributes depending on race.
 */
static void process_ghost_race(int ghost_race, int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    byte n;
    QString racename;
    int i;

    int dun_level = r_ptr->level;

    int race_middle = 0;

    /*nonsense high amount which will be lowered later*/
    int race_min = 25000;
    int race_max = 0;

    /*adjust hit points for the ghost class*/
    r_ptr->hdice = (p_info[ghost_race].r_mhp * r_ptr->hdice)/10;

    /*increase searching range based on ghost race searching and infravision*/
    r_ptr->aaf += (p_info[ghost_race].r_srh + p_info[ghost_race].r_fos) / 2 + p_info[ghost_race].infra;

    /*Add in some intrinsic race abilities*/
    if (p_info[ghost_race].pr_flags2 & TR2_RES_LIGHT) r_ptr->flags3 &= ~(RF3_HURT_LIGHT);
    if (p_info[ghost_race].pr_flags3 & TR3_FREE_ACT) r_ptr->flags3 |= (RF3_NO_CHARM);
    if (p_info[ghost_race].pr_flags2 & TR2_RES_POIS) r_ptr->flags3 |= (RF3_IM_POIS);
    if (p_info[ghost_race].pr_flags3 & TR3_REGEN) r_ptr->flags2 |= (RF2_REGENERATE);

    /*Add native abilities*/
    r_ptr->r_native |= p_info[ghost_race].pr_native;

    /*extract the ghost name*/
    racename = p_info[ghost_race].pr_name;

    /*is it an orc name?*/
    if (racename.contains("orc", Qt::CaseInsensitive))
    {
        r_ptr->flags3 |= (RF3_ORC);
    }

    /*is it a troll name?*/
    if (racename.contains("troll", Qt::CaseInsensitive))
    {
        r_ptr->flags4 |= (RF4_BOULDER);
        r_ptr->flags3 |= (RF3_TROLL);

    }

    /*is it an elf name?*/
    if (racename.contains("elf", Qt::CaseInsensitive))
    {
        r_ptr->flags3 &= ~(RF3_HURT_LIGHT);
    }

    /*go through the races, get average, min, and max abilities for fighting*/
    for(i = 0; i < z_info->p_max; i++)
    {

        if (p_info[i].r_thn < race_min)
        {
            race_min = p_info[i].r_thn;
        }
        if (p_info[i].r_thn > race_max)
        {
            race_max = p_info[i].r_thn;
        }
        race_middle += p_info[i].r_thn;

    }

    /*get the average fighting ability*/
    race_middle =  (race_middle / (z_info->p_max - 1));

    /*
     * Get "quartiles" for race fighting ability.
     * This isn't quite statistically correct, but close enough
     */
    race_max =  (race_middle + race_max) / 2;
    race_min =  (race_middle + race_min) / 2;

    /*top quartile gets extra fighting ability*/
    if(p_info[ghost_race].r_thn > race_max)
    {

        for (n = 0; n < MONSTER_BLOW_MAX; n++)
        {
            /* Skip non-attacks */
            if (!r_ptr->blow[n].method) continue;

            if (one_in_(2)) r_ptr->blow[n].d_side += 1 + r_ptr->blow[n].d_side / 5;
            else r_ptr->blow[n].d_dice += 1 * r_ptr->blow[n].d_dice / 6;

            /* Sometimes make it extra tough */
            if (one_in_(4))
            {
                if (one_in_(2)) r_ptr->blow[n].d_side += 1+ r_ptr->blow[n].d_side / 5;
                else r_ptr->blow[n].d_dice += 1+ r_ptr->blow[n].d_dice / 6;
            }
        }

        r_ptr->ac += r_ptr->ac / 3;
        /* Sometimes make it extra tough */
        if (one_in_(4)) r_ptr->ac += r_ptr->ac / 3;

    }

    /*bottom quartile gets less fighting ability*/
    else if(p_info[ghost_race].r_thn < race_min)
    {
        for (n = 0; n < MONSTER_BLOW_MAX; n++)
        {
            /* Skip non-attacks */
            if (!r_ptr->blow[n].method) continue;

            r_ptr->blow[n].d_side = 5 * r_ptr->blow[n].d_side / 6;

            /* Sometimes make it extra weak */
            if (one_in_(4))
            {
                r_ptr->blow[n].d_side = 5 * r_ptr->blow[n].d_side / 6;
            }
        }

        r_ptr->ac -= r_ptr->ac / 3;

        /* Sometimes make it extra weak */
        if (one_in_(4)) r_ptr->ac -= r_ptr->ac / 3;

    }

    /*re-set for bows*/
    race_middle = 0;
    /*nonsense high amount which will be lowered later*/
    race_min = 25000;
    race_max = 0;

    /*go through the races, get average, min, and max abilities for bow ability*/
    for(i = 0; i < z_info->p_max; i++)
    {

        if (p_info[i].r_thb < race_min)
        {
            race_min = p_info[i].r_thb;
        }
        if (p_info[i].r_thb > race_max)
        {
            race_max = p_info[i].r_thb;
        }
        race_middle += p_info[i].r_thb;

    }

    /*get the average bow ability*/
    race_middle =  (race_middle / (z_info->p_max - 1));

    /*
     * Get "quartiles" for race bow ability.
     * This isn't quite statistically correct, but close enough
     */
    race_max =  (race_middle + race_max) / 2;
    race_min =  (race_middle + race_min) / 2;

    /*top quartile gets extra bow ability*/
    if(p_info[ghost_race].r_thb > race_max)
    {

        r_ptr->freq_ranged += ((100 - r_ptr->freq_ranged) / 6);

        /* Sometimes make it extra tough */
        if (one_in_(3))
        {
            r_ptr->freq_ranged += ((100 - r_ptr->freq_ranged) / 6);
        }

        if (dun_level > 54)
            r_ptr->flags4 |= (RF4_PMISSL);
        else if (dun_level > 44)
            r_ptr->flags4 |= (RF4_BOLT);
        else if (dun_level > 34)
            r_ptr->flags4 |= (RF4_MISSL);
        else if (dun_level > 24)
            r_ptr->flags4 |= (RF4_ARROW);
        else r_ptr->flags4 |= (RF4_SHOT);

    }

    /*bottom quartile gets no bow ability*/
    else if(p_info[ghost_race].r_thb < race_min)
    {
        r_ptr->flags4 &= ~(RF4_ARCHERY_MASK);
    }

}

/*
 * Add various player ghost attributes depending on class. -LM-
 */
static void process_ghost_class(int ghost_class, int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    int dun_level = r_ptr->level;
    byte i, n;
    int class_middle = 0;
    /*nonsense high amount which will be lowered later*/
    int class_min = 25000;
    int class_max = 0;

    /*adjust hit points for the ghost class*/
    r_ptr->hside += (c_info[ghost_class].c_mhp * r_ptr->hside)/25;

    /*Add native abilities*/
    r_ptr->r_native |= c_info[ghost_class].c_native;

    /*spellcasters get magic ability*/
    if (c_info[ghost_class].spell_book)
    {

        r_ptr->freq_ranged += (100 - r_ptr->freq_ranged) / 8;
        r_ptr->mana += r_ptr->mana / 5;

        /* Sometimes make it extra tough */
        if (one_in_(3))
        {
            r_ptr->freq_ranged += (100 - r_ptr->freq_ranged) / 8;
            r_ptr->mana += r_ptr->mana / 5;
        }
    }

    /*pure spellcasters get even more magic ability*/
    if (c_info[ghost_class].flags & CF_ZERO_FAIL)
    {
        r_ptr->freq_ranged += (100 - r_ptr->freq_ranged) / 8;
        r_ptr->mana += r_ptr->mana / 5;

        /* Sometimes make it extra tough */
        if (one_in_(4))
        {
            r_ptr->freq_ranged += (100 - r_ptr->freq_ranged) / 8;
            r_ptr->mana += r_ptr->mana / 5;
        }
    }

    /* adjust for bravery flag*/
    if ((c_info[ghost_class].flags & CF_BRAVERY_30) && (dun_level > 20))
    {
        r_ptr->flags3 |= (RF3_NO_FEAR);
    }

    /*gives ghost rogues stealing ability and other thief abilities*/
    if (c_info[ghost_class].flags & CF_ROGUE_COMBAT)
    {

        if (r_ptr->blow[0].effect == RBE_HURT) r_ptr->blow[0].effect = RBE_EAT_GOLD;
        if (r_ptr->blow[1].effect == RBE_HURT) r_ptr->blow[1].effect = RBE_EAT_ITEM;
        if ((dun_level > 50) && one_in_(2)) r_ptr->flags7 |= (RF7_S_THIEF);

        r_ptr->aaf += r_ptr->aaf / 3;
    }

    /*set trap setters set traps*/
    if (c_info[ghost_class].flags & CF_SET_TRAPS) r_ptr->flags6 |= (RF6_TRAPS);

    /*get extra frequency with arrows*/
    if (c_info[ghost_class].flags & CF_EXTRA_ARROW)
    {
        if (dun_level > 30) r_ptr->freq_ranged += ((100 - r_ptr->freq_ranged) / 3);
        r_ptr->flags4 |= (RF4_ARROW);
    }

    /*get extra frequency with shots*/
    if (c_info[ghost_class].flags & CF_EXTRA_SHOT)
    {
        if (dun_level > 30) r_ptr->freq_ranged += ((100 - r_ptr->freq_ranged) / 3);
        r_ptr->flags4 |= (RF4_SHOT);
    }

    /*add the mage_spells*/
    if (c_info[ghost_class].spell_book == TV_MAGIC_BOOK)
    {
        byte level_adj = MAX(dun_level - c_info[ghost_class].spell_first, 0);


        if ((level_adj > 25) && one_in_(2)) r_ptr->flags2 |= (RF2_LOW_MANA_RUN);
        if ((level_adj > 35) && one_in_(2)) r_ptr->flags3 |= (RF3_IM_ELEM);
        if ((level_adj > 11) && one_in_(2)) r_ptr->flags5 |= (RF5_BALL_POIS);
        if ((level_adj > 65) && one_in_(2)) r_ptr->flags6 |= (RF6_TELE_AWAY);
        if ((level_adj > 55) && one_in_(2)) r_ptr->flags6 |= (RF6_TELE_TO);
        if ((level_adj > 5) && one_in_(2)) r_ptr->flags6 |= (RF6_BLINK);
        if ((level_adj > 50) && one_in_(2)) r_ptr->flags6 |= (RF6_TELE_SELF_TO);
        if ((dun_level > 35) && one_in_(2)) r_ptr->flags6 |= (RF6_HASTE);


        if (c_info[ghost_class].flags & CF_ZERO_FAIL)
        {
            if (level_adj > 0) r_ptr->flags5 |= (RF5_BOLT_MANA);
            if ((level_adj > 25) && one_in_(2)) r_ptr->flags5 |= (RF5_BALL_FIRE);
            if ((level_adj > 35) && one_in_(2)) r_ptr->flags5 |= (RF5_BALL_COLD);
            if ((level_adj > 55) && one_in_(2)) r_ptr->flags5 |= (RF5_BALL_STORM);
            if ((level_adj > 65) && one_in_(2)) r_ptr->flags5 |= (RF5_BALL_MANA);
            if ((level_adj > 75) && one_in_(2)) r_ptr->flags6 |= (RF6_MIND_BLAST);
            if ((level_adj > 85) && one_in_(2)) r_ptr->flags6 |= (RF6_BRAIN_SMASH);
        }

    }

    if (c_info[ghost_class].spell_book == TV_PRAYER_BOOK)
    {
        byte level_adj = MAX(dun_level - c_info[ghost_class].spell_first, 0);

        if (level_adj > 25) r_ptr->flags2 |= (RF2_LOW_MANA_RUN);
        if (level_adj > 11) r_ptr->flags5 |= (RF5_HOLY_ORB);
        if (level_adj > 5)  r_ptr->flags6 |= (RF6_HEAL);
        if ((level_adj > 15) && one_in_(2)) r_ptr->flags6 |= (RF6_BLINK);
        if ((level_adj > 55) && one_in_(2)) r_ptr->flags6 |= (RF6_TELE_SELF_TO);
        if ((dun_level > 45) && one_in_(2)) r_ptr->flags6 |= (RF6_HASTE);
        if ((dun_level > 65) && one_in_(2)) r_ptr->flags5 |= (RF5_BEAM_NETHR);
        if ((dun_level > 10) && one_in_(2))
        {
            r_ptr->flags3 |= (RF3_IM_FIRE |	RF3_IM_COLD);
            r_ptr->flags3 &= ~(RF3_HURT_FIRE |	RF3_HURT_COLD);
        }

        if (c_info[ghost_class].flags & CF_ZERO_FAIL)
        {
            r_ptr->mana += r_ptr->mana / 4;

            if ((level_adj > 25) && one_in_(2)) r_ptr->flags5 |= (RF5_BALL_FIRE);
            if ((level_adj > 35) && one_in_(2)) r_ptr->flags5 |= (RF5_BALL_COLD);
            if ((level_adj > 55) && one_in_(2)) r_ptr->flags5 |= (RF5_BALL_STORM);
            if ((level_adj > 65) && one_in_(2)) r_ptr->flags5 |= (RF5_BALL_MANA);
            if ((level_adj > 45) && one_in_(2)) r_ptr->flags6 |= (RF6_WOUND);
            if ((level_adj > 65) && one_in_(2)) r_ptr->flags7 |= (RF7_S_UNDEAD);
            if ((level_adj > 80) && one_in_(2)) r_ptr->flags7 |= (RF7_S_HI_UNDEAD);
            if ((level_adj > 50) && one_in_(2)) r_ptr->flags7 |= (RF7_S_WRAITH);
        }

    }

    if (c_info[ghost_class].spell_book == TV_DRUID_BOOK)
    {
        byte level_adj = MAX(dun_level - c_info[ghost_class].spell_first, 0);


        if (level_adj > 3) r_ptr->flags5 |= (RF5_BOLT_ACID);
        if (level_adj > 5)
        {
            r_ptr->flags6 |= (RF6_HEAL);
            if (one_in_(2)) r_ptr->flags5 |= (RF5_BEAM_ELEC);

        }

        if ((level_adj > 9) && (one_in_(2)))  r_ptr->flags5 |= (RF5_BALL_POIS);
        if (dun_level > 10)
        {
            r_ptr->flags3 |= (RF3_IM_FIRE |	RF3_IM_COLD);
            r_ptr->flags3 &= ~(RF3_HURT_FIRE |	RF3_HURT_COLD);
        }

        if ((level_adj > 20) && one_in_(2)) r_ptr->flags5 |= (RF5_BALL_FIRE);
        if ((level_adj > 25) && one_in_(2)) r_ptr->flags5 |= (RF5_BALL_COLD);
        if ((level_adj > 27) && one_in_(2)) r_ptr->flags5 |= (RF5_BALL_ACID);
        if ((level_adj > 30) && one_in_(2)) r_ptr->flags5 |= (RF5_BALL_ELEC);
        if ((dun_level > 40) && one_in_(2)) r_ptr->flags6 |= (RF6_HASTE);
        if ((level_adj > 45) && one_in_(2)) r_ptr->flags6 |= (RF6_WOUND);


    }

    /*go through the classes, get average, min, and max abilities for fighting*/
    for(i = 0; i < z_info->c_max; i++)
    {
        int total = c_info[i].c_thn + ((c_info[i].x_thn * dun_level) / 20);

        if (total < class_min)
        {
            class_min = total;
        }
        if (total > class_max)
        {
            class_max = total;
        }
        class_middle += total;

    }

    /*get the average fighting ability*/
    class_middle =  (class_middle / (z_info->c_max - 1));

    /*
     * Get "quartiles" for class fighting ability.
     * This isn't quite statistically correct, but close enough
     */
    class_max =  (class_middle + class_max) / 2;
    class_min =  (class_middle + class_min) / 2;

    /*top quartile gets extra fighting ability*/
    if((c_info[ghost_class].c_thn + (c_info[ghost_class].x_thn * dun_level / 20)) > class_max)
    {

        for (n = 0; n < MONSTER_BLOW_MAX; n++)
        {
            /* Skip non-attacks */
            if (!r_ptr->blow[n].method) continue;

            if (one_in_(2)) r_ptr->blow[n].d_side += 1 + r_ptr->blow[n].d_side / 5;
            else r_ptr->blow[n].d_dice += 1 + r_ptr->blow[n].d_dice / 6;

            /* Sometimes make it extra tough */
            if (one_in_(4))
            {
                if (one_in_(2)) r_ptr->blow[n].d_side += 1 + r_ptr->blow[n].d_side / 5;
                else r_ptr->blow[n].d_dice += 1 + r_ptr->blow[n].d_dice / 6;
            }
        }

        r_ptr->ac += r_ptr->ac / 3;

        /* Sometimes make it extra tough */
        if (one_in_(4)) r_ptr->ac += r_ptr->ac / 3;
    }

    /*bottom quartile gets less fighting ability*/
    else if((c_info[ghost_class].c_thn + ((c_info[ghost_class].x_thn * dun_level) / 20)) < class_min)
    {

        for (n = 0; n < MONSTER_BLOW_MAX; n++)
        {
            /* Skip non-attacks */
            if (!r_ptr->blow[n].method) continue;

            r_ptr->blow[n].d_side = 4 * r_ptr->blow[n].d_side / 5;

            /* Sometimes make it extra weak */
            if (one_in_(4))
            {
                r_ptr->blow[n].d_side = 4 * r_ptr->blow[n].d_side / 5;
            }
        }

        r_ptr->ac -= r_ptr->ac / 3;

        /* Sometimes make it extra weak */
        if (one_in_(4)) r_ptr->ac -= r_ptr->ac / 4;

    }

    /*re-set for bows*/
    class_middle = 0;
    /*nonsense high amount which will be lowered later*/
    class_min = 25000;
    class_max = 0;

    /*go through the classes, get average, min, and max abilities for bow ability*/
    for(i = 0; i < z_info->c_max; i++)
    {
        int total = c_info[i].c_thb + ((c_info[i].x_thb * dun_level) / 20);

        if (total < class_min)
        {
            class_min = total;
        }
        if (total > class_max)
        {
            class_max = total;
        }
        class_middle += total;

    }

    /*get the average bow ability*/
    class_middle =  (class_middle / (z_info->c_max - 1));

    /*
     * Get "quartiles" for class bow ability.
     * This isn't quite statistically correct, but close enough
     */
    class_max =  (class_middle + class_max) / 2;
    class_min =  (class_middle + class_min) / 2;

    /*top quartile gets extra bow ability*/
    if((c_info[ghost_class].c_thb + (c_info[ghost_class].x_thb * dun_level / 20)) > class_max)
    {

        r_ptr->freq_ranged += ((100 - r_ptr->freq_ranged) / 6);

        /* Sometimes make it extra tough */
        if (one_in_(3))
        {
            r_ptr->freq_ranged += ((100 - r_ptr->freq_ranged) / 7);
        }

        if (dun_level > 54)
            r_ptr->flags4 |= (RF4_PMISSL);
        else if (dun_level > 44)
            r_ptr->flags4 |= (RF4_BOLT);
        else if (dun_level > 34)
            r_ptr->flags4 |= (RF4_MISSL);
        else if (dun_level > 24)
            r_ptr->flags4 |= (RF4_ARROW);
        else r_ptr->flags4 |= (RF4_SHOT);

    }

    /*bottom quartile gets no bow ability*/
    else if((c_info[ghost_class].c_thb + (c_info[ghost_class].x_thb * dun_level / 20)) < class_min)
    {
        r_ptr->flags4 &= ~(RF4_ARCHERY_MASK);
    }

}

void prepare_ghost_name(void)
{
    monster_race *r_ptr = &r_info[ghost_r_idx];

    /* Current entry */
    ghost_template *t_ptr;

    /* Paranoia */
    if ((player_ghost_num < 0) || (player_ghost_num >= z_info->ghost_template_max))
    {
        player_ghost_name.clear();
        return;
    }

    t_ptr = &t_info[player_ghost_num];

    player_ghost_name = (QString("%1, the %2") .arg(capitalize_first(t_ptr->t_name))
                         .arg(capitalize_first(r_ptr->r_name_full)));
}

/*
 * Once a monster with the flag "PLAYER_GHOST" is generated, it needs
 * to have a little color added, if it hasn't been prepared before.
 * This function prepares teh ghost and then flagss depending on the
 * race and class of the slain adventurer, or picks from one of the
 * maintainer templates.
 */
bool prepare_ghost(int r_idx)
{
    byte ghost_sex = 0, ghost_race = 0, ghost_class = 0;
    int	i;

    monster_race *r_ptr = &r_info[r_idx];
    monster_lore *l_ptr = &l_list[r_idx];
    ghost_template *t_ptr;

    bool found_template = FALSE;

    /* Paranoia. */
    if (!(r_ptr->flags2 & (RF2_PLAYER_GHOST))) return (FALSE);
    if (birth_no_player_ghosts) return (FALSE);
    if (!z_info->ghost_template_max) return (FALSE);

    /* No more than one player ghost at a time */
    if (ghost_r_idx) return (FALSE);

    /* The template is already selected */
    if ((player_ghost_num > -1) && (player_ghost_num < z_info->ghost_template_max))
    {
        t_ptr = &t_info[player_ghost_num];

        /* We have a valid template */
        if (!t_ptr->t_name.isEmpty())
        {
            found_template = TRUE;
            ghost_sex = t_ptr->t_gender;
            ghost_race = t_ptr->t_race;
            ghost_class = t_ptr->t_class;
        }
    }

    /* We need to select a template */
    if (!found_template)
    {
        /* There are dead player characters to check */
        if (z_info->ghost_maint_max < z_info->ghost_template_max)
        {
            for (i = 0; i < z_info->ghost_template_max; i++)
            {
                t_ptr = &t_info[i];

                /* Empty template */
                if (t_ptr->t_name.isEmpty()) continue;

                /* The current level is too far from the depth the player was killed */
                if (ABS(t_ptr->t_depth - p_ptr->depth) > 3) continue;

                /* Use this template */
                found_template = TRUE;
                player_ghost_num = i;
                ghost_sex = t_ptr->t_gender;
                ghost_race = t_ptr->t_race;
                ghost_class = t_ptr->t_class;
                break;
            }
        }

        /* No dead char templates to use, select a random maintainer template instead */
        if (!found_template)
        {
            player_ghost_num = randint0(z_info->ghost_maint_max);
            t_ptr = &t_info[player_ghost_num];
            ghost_sex = t_ptr->t_gender;
            ghost_race = t_ptr->t_race;
            ghost_class = t_ptr->t_class;
        }
    }

    /* Hack -- Use the "simple" RNG */
    Rand_quick = TRUE;

    /* Hack -- Induce consistant town layout */
    Rand_value = seed_ghost;

    /* Store the index of the base race. */
    ghost_r_idx = r_idx;

    /*** Process sex. ***/

    /* Sanity check. */
    if (ghost_sex >= MAX_SEXES) ghost_sex = rand_int(MAX_SEXES);

    /* And use that number to specify the male or the female flag (or remain gender neutral). */
    if (ghost_sex == 0) r_ptr->flags1 |= (RF1_FEMALE);
    if (ghost_sex == 1) r_ptr->flags1 |= (RF1_MALE);


    /*** Process race. ***/

    /* Sanity check. */
    if (ghost_race >= z_info->p_max)
    {
        ghost_race = rand_int(z_info->p_max);
    }

    /* And use the ghost race to gain some flags. */
    process_ghost_race(ghost_race, r_idx);


    /*** Process class. ***/

    /* Sanity check. */
    if (ghost_class >= z_info->c_max)
    {
        ghost_class = rand_int(z_info->c_max);
    }

    /* And use the ghost class to gain some flags. */
    process_ghost_class(ghost_class, r_idx);

    /*** Process the ghost name and store it in a global variable. ***/
    prepare_ghost_name();

    /* Hack -- increase the level feeling */
    rating += 10;

    /* A ghost makes the level special */
    good_item_flag = TRUE;

    /* Hack - Player ghosts are "seen" whenever generated, to conform with
     * previous practice.
     */
    l_ptr->sights = 1;

    /* Hack -- Use the "simple" RNG */
    Rand_quick = FALSE;

    /* Success */
    return (TRUE);
}

#define NUM_GHOST_CHALLENGES	26

/*
 * Array of feeling strings
 */
static QString do_cmd_challenge_text[NUM_GHOST_CHALLENGES] =
{
    "challenges you from beyond the grave!",
    "thunders 'Prove worthy of your traditions - or die ashamed!'.",
    "desires to test your mettle!",
    "has risen from the dead to test you!",
    "roars 'Fight, or know yourself for a coward!'.",
    "summons you to a duel of life and death!",
    "desires you to know that you face a mighty champion of yore!",
    "demands that you prove your worthiness in combat!",
    "calls you unworthy of your ancestors!",
    "challenges you to a deathmatch!",
    "walks Middle-Earth once more!",
    "challenges you to demonstrate your prowess!",
    "demands you prove yourself here and now!",
    "asks 'Can ye face the best of those who came before?'.",
    "challenges you to a fight to the death!",
    "wails 'These halls shall claim your life as well!'",
    "begs you 'Free me from this cursed form!'.",
    "whispers 'Those who perish here shall find no rest'.",
    "boasts 'You won't leave this level alive!",
    "wishes to claim your soul!",
    "declares 'You will join me in this tortured afterlife!'",
    "proclaims 'Prepare to fight your last battle'",
    "bellows 'Your adventures will end here!'",
    "dares you to proceed further!",
    "wants to collect your bones!",
    "yells 'Now you shall meet your undoing!'"
};


/*
 * Personalize, randomize, and announce the challenge of a player ghost.
 */
void ghost_challenge(bool color)
{
    size_t i;

    /* No active player ghost template */
    if (!ghost_r_idx) return;

    /*paranoia*/
    /* Check there is a name/ghost first */
    if (!player_ghost_name.isEmpty())
    {
        /*Make sure the name has been created*/
        prepare_ghost_name();
    }

    i = randint0(NUM_GHOST_CHALLENGES);

    QString challenge = (QString("%1 %2") .arg(capitalize_first(player_ghost_name)) .arg(do_cmd_challenge_text[i]));

    if (color) color_message(challenge, TERM_RED_LAVA);
    else message(challenge);
}



/* Remove the ghost. Make sure each one only shows up once per game */
void remove_player_ghost(void)
{
    monster_lore *l_ptr;
    monster_race *r_ptr;

    /* Paranoia */
    if ((player_ghost_num < 0) || (player_ghost_num >= z_info->ghost_template_max))
    {
        player_ghost_num = -1;
        return;
    }
    if (!ghost_r_idx) return;

    /* Remove the memory of seeing it */
    l_ptr = &l_list[ghost_r_idx];

    l_ptr->sights = 0;
    l_ptr->pkills = 1;
    l_ptr->tkills = 0;

    /* No current player ghosts, set up a new random entry */
    r_ptr = &r_info[ghost_r_idx];
    r_ptr->max_num = 0;
    player_ghost_num = -1;
    ghost_r_idx = 0;
    player_ghost_name.clear();
    seed_ghost = rand_int(0x10000000);
}

/*
 * Remove a player ghost entry if the player killed it.
  */
void delete_player_ghost_entry(void)
{
    /* Paranoia */
    if ((player_ghost_num < 0) || (player_ghost_num >= z_info->ghost_template_max))
    {
        player_ghost_num = -1;
        return;
    }

    /* Don't wipe the permanent maintainer templates */
    else if (player_ghost_num >= z_info->ghost_maint_max)
    {
        ghost_template *t_ptr = &t_info[player_ghost_num];

        /* Wipe the structure */
        t_ptr->ghost_template_wipe();
    }
}

/*
 * Add a player ghost entry if the player died.
 * but only if there is an unused slot.
 */
void add_player_ghost_entry(void)
{
    int i;

    for (i = z_info->ghost_maint_max; i < z_info->ghost_template_max; i++)
    {
        ghost_template *t_ptr = &t_info[i];

        /* This slot already used...keep looking */
        if (!t_ptr->t_name.isEmpty()) continue;

        /* Make the entry */
        if (!op_ptr->full_name.isEmpty())
        {
             t_ptr->t_name = op_ptr->full_name;
        }
        else t_ptr->t_name = "Anonymous";

        t_ptr->t_gender = p_ptr->psex;
        t_ptr->t_race = p_ptr->prace;
        t_ptr->t_class = p_ptr->pclass;
        t_ptr->t_depth = p_ptr->depth;

        break;
    }
}

/*
 * Populate the ghost_player_max portion of the player ghost file.
 */
void load_player_ghost_file(void)
{

    QString player_ghost_filename = "ghost_templates.txt";
    QFile player_ghost_file;

    if (game_mode == GAME_NPPANGBAND) player_ghost_filename.prepend("nppangband_");

    // Hack - currently NPPMoria doesn't do player ghosts
    else if (game_mode == GAME_NPPMORIA) return;

    /* Build the filename */
    player_ghost_file.setFileName(QString("%1/%2" ) .arg(npp_dir_bone.path()) .arg(player_ghost_filename));

    if (!player_ghost_file.exists()) return;
    if (!player_ghost_file.open(QIODevice::ReadOnly)) return;

    // Keep track of the last idx we have read so we can point to the correct entry.
    last_idx = -1;

    QTextStream reading(&player_ghost_file);

    /* Parse */
    while (!reading.atEnd())
    {
        QString file_line = reading.readLine();

        //Shouldn't need error handling, if the file is correctly generated by save_player_ghost_file
        parse_t_info(file_line);
    }

    // close the file
    player_ghost_file.close();
}

/*
 * Populate the ghost_player_max portion of the player ghost file.
 */
void save_player_ghost_file(void)
{
    QString player_ghost_filename = "ghost_templates.txt";
    QFile player_ghost_file;

    if (game_mode == GAME_NPPANGBAND) player_ghost_filename.prepend("nppangband_");

    // Hack - currently NPPMoria doesn't do player ghosts
    else if (game_mode == GAME_NPPMORIA) return;

    /* Build the filename */
    player_ghost_file.setFileName(QString("%1/%2" ) .arg(npp_dir_bone.path()) .arg(player_ghost_filename));

    if (!player_ghost_file.exists()) return;
    if (!player_ghost_file.open(QIODevice::WriteOnly)) return;

    QTextStream out(&player_ghost_file);

    out << "# This file contains dead player ghost templates.\n";
    out << "# do not edit this file unless you know exactly what you are doing! \n\n";
    out << "# === Understanding ghost_templates.txt === \n\n";
    out << "# N: serial number : dead player ghost name \n";
    out << "# I: gender : race : class : depth \n\n";

    for (int i = z_info->ghost_maint_max; i < z_info->ghost_template_max; i++)
    {
        ghost_template *t_ptr = &t_info[i];

        /* Unused slot */
        if (t_ptr->t_name.isEmpty()) continue;

        /*(
         * Write N line
         * n:entry num:ghost name
         */
        out << "N:" << i << ":" << t_ptr->t_name << "\n";

        /* Write I: line and double return at end*/
        out << "I:" << t_ptr->t_gender << ":" << t_ptr->t_class << ":" << t_ptr->t_depth << "\n\n";
    }

    /* Close and save the ghost_template file */
    player_ghost_file.close();
}

