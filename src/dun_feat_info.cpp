
/* File: was feature.c */

/*
 * Copyright (c) 2006 Jeff Greene, Diego Gonzalez
 *
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"
#include "src/help.h"

/*
 * Return a string that describes the type of a feature
 */
static QString get_feature_type(const feature_lore *f_l_ptr)
{
    if (f_l_ptr->f_l_flags1 & FF1_SHOP)		return (" shop entrance");
    else if (f_l_ptr->f_l_flags1 & FF1_DOOR)	return (" door");
    else if (f_l_ptr->f_l_flags1 & FF1_TRAP)	return (" trap");
    else if (f_l_ptr->f_l_flags1 & FF1_STAIRS)	return (" staircase");
    else if (f_l_ptr->f_l_flags1 & FF1_GLYPH)	return (" glyph");
    else if (f_l_ptr->f_l_flags1 & FF1_FLOOR)	return (" floor");
    else if (f_l_ptr->f_l_flags1 & FF1_WALL)	return (" wall");
    else if (f_l_ptr->f_l_flags3 & FF3_TREE)	return (" tree");
    else if (f_l_ptr->f_l_flags2 & FF2_EFFECT)	return (" effect");

    /*Default*/
    else return (" feature");
}


static QString describe_feature_basic(int f_idx, const feature_lore *f_l_ptr)
{
    const feature_type *f_ptr = &f_info[f_idx];
    QVector<QString> flags;
    flags.clear();
    QString type;
    int i = 0;

    QString output;
    output.clear();

    output.append("This is a");

    /* Collect some flags */
    if ((f_l_ptr->f_l_flags2 & FF2_GLOW) &&
         (!(f_l_ptr->f_l_flags1 & FF1_SHOP)))	flags.append(" glowing");

    if (f_l_ptr->f_l_flags3 & FF3_LAVA)			flags.append(" lava");
    if (f_l_ptr->f_l_flags3 & FF3_ICE)			flags.append(" icy");
    if (f_l_ptr->f_l_flags3 & FF3_OIL)			flags.append(" oil");
    if (f_l_ptr->f_l_flags3 & FF3_FIRE)			flags.append(" fire");
    if (f_l_ptr->f_l_flags3 & FF3_SAND)			flags.append(" sandy");
    if (f_l_ptr->f_l_flags3 & FF3_FOREST)		flags.append(" forest");
    if (f_l_ptr->f_l_flags3 & FF3_WATER)		flags.append(" water");
    if (f_l_ptr->f_l_flags3 & FF3_ACID)			flags.append(" acid");
    if (f_l_ptr->f_l_flags3 & FF3_MUD)			flags.append(" mud");

    /* Print the collected flags */
    for (i = 0; i < flags.size(); i++)
    {
        /* Append a 'n' to 'a' if the first flag begins with a vowel */
        if ((i == 0) && begins_with_vowel(flags[i])) output.append("n");

        output.append(color_string(flags[i], TERM_NAVY_BLUE));
    }

    /* Get the feature type name */
    type = get_feature_type(f_l_ptr);

    /* Append a 'n' to 'a' if the type name begins with a vowel */
    if (!flags.size() && begins_with_vowel(type)) output.append("n");

    /* Describe the feature type */
    output.append(type);

    /* Describe location */
    if (f_ptr->f_flags2 & (FF2_EFFECT))
    {
        /* Do nothing */
    }

    else if ((f_l_ptr->f_l_flags1 & FF1_SHOP) || (f_idx == FEAT_COBBLESTONE_FLOOR))
    {
        output.append(color_string(" that is found in the town", TERM_L_GREEN));
    }
    else if ((f_l_ptr->f_l_flags2 & FF2_TRAP_MON) ||
    (f_l_ptr->f_l_flags1 & FF1_GLYPH))
    {
        output.append(color_string(" that is set by the player", TERM_L_GREEN));
    }
    else if (f_l_ptr->f_l_sights > 10)
    {
        output.append(" that");

        if (f_l_ptr->f_l_sights > 20)
        {
            if (f_ptr->f_rarity >= 4) output.append(" rarely");
            else if (f_ptr->f_rarity >= 2) output.append(" occasionally");
            else output.append(" commonly");
        }

        if (f_ptr->f_level == 0)
        {
            output.append(color_string(" appears in both the town and dungeon", TERM_L_GREEN));
        }
        else if (f_ptr->f_level == 1)
        {
            output.append(color_string(" appears throughout the dungeon", TERM_L_GREEN));
        }
        else
        {
            output.append(color_string(" appears", TERM_L_GREEN));
            output.append(color_string(QString(" at depths of %1 feet and below") .arg(f_ptr->f_level * 50), TERM_L_GREEN));
        }

    }

    /* Little Information Yet */
    else output.append(color_string(" that is found in the dungeon", TERM_L_GREEN));

    /* End this sentence */
    output.append(".  ");

    /* More misc info */
    if (f_l_ptr->f_l_flags1 & FF1_DROP)
    {
        output.append("  This");
        /*Describe the feature type*/
        output.append(get_feature_type(f_l_ptr));
        output.append(" can hold objects.  ");
    }
    if (f_l_ptr->f_l_flags1 & FF1_HAS_GOLD)
    {
        output.append("  This");
        output.append(get_feature_type(f_l_ptr));
        output.append(" may be hiding treasure.  ");
    }

    return(output);
}


static QString describe_feature_move_see_cast(int f_idx, const feature_lore *f_l_ptr)
{

    QString output;
    output.clear();

    QVector<QString> vp;
    vp.clear();

    if (f_l_ptr->f_l_flags1 & FF1_MOVE)
    {
        if feat_ff1_match(f_idx, FF1_FLOOR)	vp.append("move over");
        else vp.append("move through");
    }
    if (f_l_ptr->f_l_flags1 & FF1_LOS) vp.append("see through");
    if (f_l_ptr->f_l_flags1 & FF1_RUN) vp.append("run past");
    if (f_l_ptr->f_l_flags1 & FF1_PROJECT)
    {
        vp.append("cast magic through");
        vp.append("fire projectiles through");
    }

    /* Describe special abilities. */
    if (vp.size())
    {
        /* Intro */
        output.append("  You ");

        /* Scan */
        for (int n = 0; n < vp.size(); n++)
        {
            /* Intro */
            if (n == 0) output.append("can ");
            else if (n < (vp.size()-1)) output.append(", ");
            else output.append(" and ");

            /* Dump */
            output.append(color_string(vp[n], TERM_GREEN));
        }

        /* End */
        output.append(" this");
        output.append(get_feature_type(f_l_ptr));
        output.append(".  ");
    }

    if (f_l_ptr->f_l_flags2 & FF2_CAN_FLY)
    {
        output.append("  Creatures who have the ability to do so can fly over this");
        output.append(get_feature_type(f_l_ptr));
        output.append(".  ");
    }
    if (f_l_ptr->f_l_flags2 & FF2_COVERED)
    {
        output.append("  Native creatures can hide in this");
        output.append(get_feature_type(f_l_ptr));
        output.append(".  ");
    }

    return(output);
}


static QString describe_feature_stairs(const feature_lore *f_l_ptr)
{
    QString output;
    output.clear();

    output.append("  This");

    if (f_l_ptr->f_l_flags2 & FF2_SHAFT)	output.append(color_string(" shaft", TERM_NAVY_BLUE));
    else output.append(color_string(" staircase", TERM_NAVY_BLUE));

    output.append(" will take you");

    if (f_l_ptr->f_l_flags1 & FF1_LESS)		output.append(color_string(" up", TERM_NAVY_BLUE));
    if (f_l_ptr->f_l_flags1 & FF1_MORE)		output.append(color_string(" down", TERM_NAVY_BLUE));

    if (f_l_ptr->f_l_flags2 & FF2_SHAFT)	output.append(color_string(" two levels.  ", TERM_NAVY_BLUE));
    else output.append(color_string(" one level.  ", TERM_NAVY_BLUE));

    return(output);
}


static QString describe_feature_trap(int f_idx, const feature_lore *f_l_ptr)
{
    QString output;
    output.clear();

    /*Describe passive traps the player can set off*/
    if (f_l_ptr->f_l_flags2 & FF2_TRAP_PASSIVE) return (hit_trap(f_idx, 0, 0, MODE_DESCRIBE));

    /*Describe passive traps the player can set off*/
    if (f_l_ptr->f_l_flags2 & FF2_TRAP_MON) return (apply_monster_trap(f_idx, 0, 0, MODE_DESCRIBE));

    /*Describe smart traps */

    if (f_l_ptr->f_l_flags2 & FF2_TRAP_SMART)
    {
        (void)fire_trap_smart(f_idx, 0, 0, MODE_DESCRIBE, &output);
        return (output);
    }

    return (output);
}


static QString describe_feature_interaction(int f_idx, const feature_lore *f_l_ptr)
{
    QString output;
    output.clear();

    const feature_type *f_ptr = &f_info[f_idx];

    QVector<QString> vp;
    vp.clear();

    u32b filtered_flag1 = f_l_ptr->f_l_flags1;

    /*
     * First get rid of redundant messages, if they are to be
     * described later in describe_feature_transitions.
     */
    for (int i = 0; i < MAX_FEAT_STATES; i++)
    {
        /* There isn't a recorded action here */
        if (f_ptr->state[i].fs_action == FS_FLAGS_END) continue;

        /* Have we seen it yet? */
        if (f_l_ptr->f_l_state[i] == 0) continue;

        switch (f_ptr->state[i].fs_action)
        {

            /* Make sure we don't list something redundantly and more specifically later*/
            case 	FS_OPEN:		{filtered_flag1 &= ~(FF1_CAN_OPEN); 	continue;}
            case	FS_CLOSE:		{filtered_flag1 &= ~(FF1_CAN_CLOSE); 	continue;}
            case	FS_BASH:		{filtered_flag1 &= ~(FF1_CAN_BASH); 	continue;}
            case	FS_SPIKE:		{filtered_flag1 &= ~(FF1_CAN_SPIKE); 	continue;}
            case	FS_TUNNEL:		{filtered_flag1 &= ~(FF1_CAN_TUNNEL); 	continue;}

            /*All others*/
            default: continue;
        }
    }

    if (filtered_flag1 & FF1_CAN_OPEN) vp.append("open");
    if (filtered_flag1 & FF1_CAN_CLOSE) vp.append("close");
    if (filtered_flag1 & FF1_CAN_BASH) vp.append("bash");
    if (filtered_flag1 & FF1_CAN_SPIKE) vp.append("spike");
    if (filtered_flag1 & FF1_CAN_DISARM) vp.append("disarm");
    if (filtered_flag1 & FF1_CAN_TUNNEL) vp.append("tunnel into");

    /* Describe special abilities. */
    if (vp.size())
    {
        /* Intro */
        output.append("  You ");

        /* Scan */
        for (int n = 0; n < vp.size(); n++)
        {
            /* Intro */
            if (n == 0) output.append("can ");
            else if (n < (vp.size()-1)) output.append(", ");
            else output.append(" and ");

            /* Dump */
            output.append(color_string(vp.at(n), TERM_GOLD));
        }

        /* End */
        output.append(" this");
        output.append(get_feature_type(f_l_ptr));
        output.append(".  ");
    }

    return(output);
}


static QString describe_feature_vulnerabilities(const feature_lore *f_l_ptr)
{
    QString output;
    output.clear();

    QVector<QString> vp;
    vp.clear();

    if (f_l_ptr->f_l_flags2 & FF2_HURT_ROCK) vp.append("stone-to-mud");
    if (f_l_ptr->f_l_flags2 & FF2_HURT_FIRE) vp.append("fire");
    if (f_l_ptr->f_l_flags2 & FF2_HURT_FIRE) vp.append("lava");
    if (f_l_ptr->f_l_flags2 & FF2_HURT_FIRE) vp.append("plasma");
    if (f_l_ptr->f_l_flags2 & FF2_HURT_COLD) vp.append("cold");
    if (f_l_ptr->f_l_flags2 & FF2_HURT_COLD) vp.append("ice");
    if (f_l_ptr->f_l_flags2 & FF2_HURT_ACID) vp.append("acid");
    if (f_l_ptr->f_l_flags2 & FF2_HURT_ELEC) vp.append("electricity");
    if (f_l_ptr->f_l_flags2 & FF2_HURT_WATER) vp.append("water");
    if (f_l_ptr->f_l_flags3 & FF3_HURT_BOIL_WATER) vp.append("boiling water");
    if (f_l_ptr->f_l_flags3 & FF3_HURT_BOIL_WATER) vp.append("steam");
    if (f_l_ptr->f_l_flags3 & FF3_HURT_POIS) vp.append("poison");

    /* Describe special abilities. */
    if (vp.size())
    {
        /* Intro */
        output.append("  This");

        output.append(get_feature_type(f_l_ptr));

        output.append(" is affected ");

        /* Scan */
        for (int n = 0; n < vp.size(); n++)
        {
            /* Intro */
            if (n == 0) output.append("by ");
            else if (n < vp.size()-1) output.append(", ");
            else output.append(" and ");

            /* Dump */
            output.append(color_string(vp.at(n), TERM_RED));
        }

        /* End */
        output.append(".  ");
    }

    return(output);
}


/*
 * Returns true if special language is used
 * returns false if the standard output should follow this.
 * This function assumes it is the start of the sentence.
 * Returning false follows this function by "causes this feature to change to"
 */

static QString describe_transition_action(int action, bool *skip_output)
{
    *skip_output = FALSE;

    switch (action)
    {

        case 	FS_SECRET:		{*skip_output = TRUE; return(color_string("  Once discovered, this feature is revealed as", TERM_GOLD));}
        case 	FS_OPEN:		{return(color_string("  Opening", TERM_GOLD));}
        case	FS_CLOSE:		{return(color_string("  Closing", TERM_GOLD));}
        case	FS_BASH:		{return(color_string("  Bashing", TERM_GOLD));}
        case	FS_SPIKE:		{return(color_string("  Spiking", TERM_GOLD));}
        case	FS_TUNNEL:		{return(color_string("  Tunneling", TERM_GOLD));}
        case	FS_TRAP:		{return(color_string("  Creating a trap", TERM_GOLD));}
        case	FS_GLYPH:		{*skip_output = TRUE; return(color_string("  This feature can change into a ", TERM_GOLD));}
        case	FS_FLOOR:		{return(color_string("  A plain floor", TERM_GOLD));}
        case	FS_BRIDGE:		{return(color_string("  Bridging", TERM_GOLD));}
        case	FS_HIT_TRAP:	{return(color_string("  De-activating this trap", TERM_GOLD));}
        case	FS_HURT_ROCK:	{return(color_string("  Stone-to-mud", TERM_GOLD));}
        case	FS_HURT_FIRE:	{return(color_string("  Fire, smoke, lava or plasma", TERM_GOLD));}
        case	FS_HURT_COLD:	{return(color_string("  Cold or ice", TERM_GOLD));}
        case	FS_HURT_ACID:	{return(color_string("  Acid", TERM_GOLD));}
        case	FS_HURT_ELEC:	{return(color_string("  Electricity", TERM_GOLD));}
        case	FS_HURT_WATER:	{return(color_string("  Water", TERM_GOLD));}
        case	FS_HURT_BWATER:	{return(color_string("  Boiling water", TERM_GOLD));}
        case	FS_HURT_POIS:	{return(color_string("  Poison", TERM_GOLD));}
        case	FS_TREE:		{return(color_string("  Forest creation", TERM_GOLD));}

        /*Paranoia*/
        default:				{return(color_string("ERROR - Unspecified Action", TERM_RED));}
    }
}


static QString describe_feature_transitions(int f_idx, const feature_lore *f_l_ptr)
{
    QString output;
    output.clear();

    feature_type *f_ptr = &f_info[f_idx];

    int i;

    bool other_trans = FALSE;

    /*Handle permanent features*/
    if (f_l_ptr->f_l_flags1 & FF1_PERMANENT)
    {
        output.append("  This is a permanent feature.");
    }

    /*Mention the mimic, if known*/
    if (f_ptr->f_mimic != f_idx)
    {
        /* Remember we have a transition we are reporting */
        other_trans = TRUE;

        /*Describe it*/
        output.append("  Until discovered, this feature appears as ");
        output.append(feature_desc(f_ptr->f_mimic, TRUE, FALSE));
        output.append(".  ");
    }

    /* Search the action */
    for (i = 0; i < MAX_FEAT_STATES; i++)
    {
        /* There isn't a recorded action here */
        if (f_ptr->state[i].fs_action == FS_FLAGS_END) continue;

        /* The feature isn't changing */
        if (f_ptr->state[i].fs_result == f_idx)
        {
            if (f_l_ptr->f_l_flags3 & (FF3_PICK_DOOR))
            {
                output.append(" Discovering this ");
                output.append(get_feature_type(f_l_ptr));
                output.append(" reveals a closed door.  ");
            }

            continue;
        }

        /* Have we seen it yet? */
        if (f_l_ptr->f_l_state[i] == 0) continue;

        /* Remember we have a transition we are reporting */
        other_trans = TRUE;

        bool skip_output;

        output.append(describe_transition_action(f_ptr->state[i].fs_action, &skip_output));

        /* Describe it, followed by standard output */
        if(!skip_output)
        {
            output.append(" changes this");
            output.append(get_feature_type(f_l_ptr));
            output.append(" to ");
        }

        output.append(feature_desc(f_ptr->state[i].fs_result, TRUE, FALSE));
        output.append(".  ");
    }

    /*Mention the default if it is different*/
    if ((f_l_ptr->f_l_defaults > 0) && (f_ptr->defaults != f_idx))
    {
        /*Describe this transition, handle differently if we have described a transition above*/
        if (other_trans)
        {
            output.append("  For all other effects, this");
        }
        else output.append("  This");
        output.append(get_feature_type(f_l_ptr));
        output.append(" changes to ");
        output.append(feature_desc(f_ptr->defaults, TRUE, FALSE));
        output.append(".  ");
    }

    return(output);
}


static QString describe_feature_damage(int f_idx, const feature_lore *f_l_ptr)
{
    QString output;
    output.clear();

    feature_type *f_ptr = &f_info[f_idx];

    QString action = "hurts";

    /* No damage, or no damage seen yet */
    if (f_ptr->dam_non_native == 0) return (output);
    if (!f_l_ptr->f_l_dam_non_native) return(output);

    /* Specify the damage type from certain features */
    if (_feat_ff3_match(f_ptr, FF3_ICE))
    {
        action = "freezes";
    }
    else if (_feat_ff3_match(f_ptr, FF3_LAVA | FF3_FIRE))
    {
        action = "burns";
    }

    /* Intro */
    output.append("  This");
    output.append(get_feature_type(f_l_ptr));
    output.append(QString(" %1 any non-native creature") .arg(action));

    /* Slightly more information when the player has seen it several times */
    if (f_l_ptr->f_l_dam_non_native > 10)
    {
        output.append(QString(" for %1 damage") .arg(f_ptr->dam_non_native));
    }

    output.append(" who stays on this feature for one turn at normal speed.  ");

    return(output);
}


static QString describe_feature_movement_effects(int f_idx, const feature_lore *f_l_ptr)
{
    QString output;
    output.clear();

    if (!feat_ff1_match(f_idx, FF1_MOVE)) return (output);

    feature_type *f_ptr = &f_info[f_idx];

    if (feat_ff2_match(f_idx, FF2_EFFECT)) return (output);

    /*Describe movement by native creatures*/
    if ((f_ptr->native_energy_move != BASE_ENERGY_MOVE) && (f_l_ptr->f_l_native_moves > 0))
    {
        int percent_movement = (ABS(BASE_ENERGY_MOVE - f_ptr->native_energy_move) * 100) / BASE_ENERGY_MOVE;

        output.append("  A creature native to this terrain uses");

        /*More information for who have observed movement more*/
        if (f_l_ptr->f_l_native_moves > 20)
        {
            if (f_ptr->native_energy_move > BASE_ENERGY_MOVE)
            {
                output.append(color_string(QString("   %1 percent more") .arg(percent_movement), TERM_BLUE));
            }
            else output.append(color_string(QString(" %1 percent less") .arg(percent_movement), TERM_BLUE));
        }
        else
        {
            if (percent_movement  > 15) output.append(color_string(" significantly", TERM_BLUE));

            if (f_ptr->native_energy_move > BASE_ENERGY_MOVE)  output.append(color_string(" more", TERM_BLUE));
            else output.append(color_string(" less", TERM_BLUE));

        }

        output.append(" energy moving into this terrain.  ");
    }

    /*Describe movement by non-native creatures*/
    if ((f_ptr->non_native_energy_move != BASE_ENERGY_MOVE) && (f_l_ptr->f_l_non_native_moves > 0))
    {
        int percent_movement = (ABS(BASE_ENERGY_MOVE - f_ptr->non_native_energy_move) * 100) / BASE_ENERGY_MOVE;

        output.append("  A creature who is not native to this terrain uses");

        /*More information for who have observed movement more*/
        if (f_l_ptr->f_l_non_native_moves > 20)
        {
            if (f_ptr->non_native_energy_move > BASE_ENERGY_MOVE)
            {
                output.append(color_string(QString("   %1 percent more") .arg(percent_movement), TERM_BLUE));
            }
            else output.append(color_string(QString(" %1 percent less") .arg(percent_movement), TERM_BLUE));
        }
        else
        {
            if (percent_movement  > 15) output.append(color_string(" significantly", TERM_BLUE));

            if (f_ptr->non_native_energy_move > BASE_ENERGY_MOVE)  output.append(color_string(" more", TERM_BLUE));
            else output.append(color_string(" less", TERM_BLUE));

        }

        output.append(" energy moving into this terrain.  ");
    }

    return(output);
}


static QString describe_feature_combat_effects(int f_idx, const feature_lore *f_l_ptr)
{
    QString output;
    output.clear();

    feature_type *f_ptr = &f_info[f_idx];

    if (feat_ff2_match(f_idx, FF2_EFFECT)) return(output);

    /* Describe movement by native creatures */
    if ((f_ptr->native_to_hit_adj != 100) && (f_l_ptr->f_l_native_to_hit_adj > 0))
    {
        output.append("  A native creature is");

        /* More information for who have observed movement more */
        if (f_l_ptr->f_l_native_to_hit_adj > 100)
        {
            if (f_ptr->native_to_hit_adj  > 100)
            {
                output.append(color_string(QString(" %1 percent more") .arg(f_ptr->native_to_hit_adj  - 100), TERM_BLUE));
            }
            else output.append(color_string(QString(" %1 percent less") .arg(100 - f_ptr->native_to_hit_adj), TERM_BLUE));

        }
        else
        {
            if (ABS(f_ptr->native_to_hit_adj  - 100) > 15) output.append(color_string(" significantly", TERM_BLUE));

            if (f_ptr->native_to_hit_adj  > 100)  output.append(color_string(" more", TERM_BLUE));
            else output.append(color_string(" less", TERM_BLUE));

        }

        output.append(" successful in attacking and defending while fighting in this terrain.  ");
    }

    /*Describe movement by non_native creatures*/
    if ((f_ptr->non_native_to_hit_adj != 100) && (f_l_ptr->f_l_non_native_to_hit_adj > 0))
    {
        output.append("  A non-native creature is");

        /*More information for who have observed movement more*/
        if (f_l_ptr->f_l_non_native_to_hit_adj > 100)
        {
            if (f_ptr->non_native_to_hit_adj > 100)
            {
                 output.append(color_string(QString(" %1 percent more") .arg(f_ptr->non_native_to_hit_adj - 100), TERM_BLUE));
            }
            else output.append(color_string(QString(" %1 percent less") .arg(100 - f_ptr->non_native_to_hit_adj), TERM_BLUE));

        }
        else
        {
            if (ABS(f_ptr->non_native_to_hit_adj - 100) > 15) output.append(color_string(" significantly", TERM_BLUE));

            if (f_ptr->non_native_to_hit_adj > BASE_ENERGY_MOVE)  output.append(color_string(" more", TERM_BLUE));
            else output.append(color_string(" less", TERM_BLUE));

        }

        output.append(" successful in attacking and defending while fighting in this terrain.  ");
    }

    return(output);
}


static QString describe_feature_stealth_effects(int f_idx, const feature_lore *f_l_ptr)
{
    QString output;
    output.clear();

    feature_type *f_ptr = &f_info[f_idx];

    if (feat_ff2_match(f_idx, FF2_EFFECT)) return(output);

    /*Describe stealth effects*/
    if ((f_ptr->f_stealth_adj != 0) && (f_l_ptr->f_l_stealth_adj > 0))
    {

        output.append("  Walking through this terrain");

        if (f_l_ptr->f_l_stealth_adj > 100)
        {

            if (ABS(f_ptr->f_stealth_adj) >= 3) output.append(" significantly");
            else if (ABS(f_ptr->f_stealth_adj) == 2) output.append(" considerably");
            else if (ABS(f_ptr->f_stealth_adj) == 1) output.append(" somewhat");
        }

        if (f_ptr->f_stealth_adj  > 0)  output.append(" improves");
        else output.append(" reduces");

        output.append(" the player's stealth.  ");
    }

    return(output);
}


/*
 * This section describes dynamic features.
 * The description for each feature needs to be described on a case-by-case basis.
 * This function should be consistent with process_dynamic_terrain_aux for its output.
 */
static QString describe_feature_dynamic(int f_idx, const feature_lore *f_l_ptr)
{
    QString output;
    output.clear();

    feature_type *f_ptr = &f_info[f_idx];

    /*
     * Hack - describe Ethereal WallTeleport player
     * TODO - figure out how to remember this in feature_lore.
     */
    if (f_idx == FEAT_ETHEREAL_WALL)
    {
        output.append("  This wall can teleport you across the dungeon, or explode in a burst of light that blinds the player, unless the player has resist blindness or resist light.");
    }

    /* TODO - figure out how to remember this in feature_lore.*/
    if (f_idx == FEAT_WALL_INSCRIPTION)
    {
        output.append("  This wall can cast a spell at the player,");
        output.append(" or reveal some useful information about the current dungeon level.  ");

        return (output);
    }

    /* Has not been observed */
    if (!(f_l_ptr->f_l_flags3 & (FF3_DYNAMIC))) return (output);

    /* Dynamic fire can spread smoke and fire */
    if (_feat_ff3_match(f_ptr, FF3_FIRE))
    {
        output.append("  This terrain can spread fire and smoke to adjacent terrains.  ");

        return (output);
    }

    if (f_idx == FEAT_WALL_WATER_BOILING_GEYSER)
    {
        output.append("  The geyser can explode in a burst of boiling water!  ");

        /* Done */
        return (output);
    }

    if ((f_idx == FEAT_FOREST_SOIL_DYNAMIC) || (f_idx == FEAT_GRASS_DYNAMIC))
    {
        output.append("  This feature can slowly spread across the dungeon.  ");

        /* Done */
        return (output);
    }

    /* Sniper flowers */
    if (f_idx == FEAT_PUTRID_FLOWER)
    {
        output.append("  This flower can fire spikes or spit poison at you.  ");

        /* Done */
        return (output);
    }

    /* Silent watchers */
    if (f_idx == FEAT_SILENT_WATCHER)
    {
        output.append("  The silent watcher can aggravate nearly monsters.  ");

        /* Done */
        return (output);
    }

    /* Dynamic lava can spread fire */
    if (_feat_ff3_match(f_ptr, TERRAIN_MASK) == (ELEMENT_LAVA))
    {
        output.append("  This terrain can spread fire to adjacent terrains.  ");

        return (output);
    }

    return(output);
}

static QString describe_store(int f_idx)
{

    feature_type *f_ptr = &f_info[f_idx];

    QString output = get_help_topic("store_info", f_ptr->f_name);

    return (output);


}

/*
 * Display feature information
 */
QString get_feature_description(int f_idx, bool spoilers, bool include_header)
{
    QString output;
    output.clear();
    QString latest_output;

    feature_lore lore;

    feature_lore save_mem;

    /* Get the race and lore */
    feature_type *f_ptr = &f_info[f_idx];
    feature_lore *f_l_ptr = &f_l_list[f_idx];
    QString feat_name = feature_desc(f_idx, TRUE, FALSE);

    /* Cheat -- know everything */
    if (cheat_know)
    {
        // mark savefile as a "cheater"
        p_ptr->is_wizard = TRUE;

        /* Hack -- save memory */
        COPY(&save_mem, f_l_ptr, feature_lore);
    }

    /* Hack -- create a copy of the monster-memory */
    COPY(&lore, f_l_ptr, feature_lore);

    /* Assume some "obvious" flags */
    lore.f_l_flags1 |= (f_ptr->f_flags1 & FF1_OBVIOUS_MASK);
    lore.f_l_flags2 |= (f_ptr->f_flags2 & FF2_OBVIOUS_MASK);
    lore.f_l_flags3 |= (f_ptr->f_flags3 & FF3_OBVIOUS_MASK);

    /* Cheat -- know everything*/
    if (cheat_know || spoilers)
    {
        if (cheat_know) p_ptr->is_wizard = TRUE;

        cheat_feature_lore(f_idx, &lore);
    }

    QString feat_symbol = color_string(f_ptr->d_char, f_ptr->d_color);

    /* Print, in colour */
    if (include_header) output.append(QString("<b><h1><span style='background-color: black;'>'%1'</span> - %2</h1></b><br><br>") .arg(feat_symbol) .arg(feat_name));

    /* Describe the movement and level of the monster */
    output.append(describe_feature_basic(f_idx, &lore));

    /* Describe the movement, LOS, and projection */
    output.append(describe_feature_move_see_cast(f_idx, &lore));

    /* Describe stairs */
    if (lore.f_l_flags1 & FF1_STAIRS) output.append(describe_feature_stairs(&lore));

    /* Describe trap */
    if (lore.f_l_flags1 & FF1_TRAP) output.append(describe_feature_trap(f_idx, &lore));

    output.append("<br>");

    latest_output = describe_feature_interaction(f_idx, &lore);

    latest_output.append(describe_feature_vulnerabilities(&lore));

    output.append(latest_output);

    if (latest_output.length()) output.append("<br>");
    latest_output.clear();

    latest_output = describe_feature_transitions(f_idx, &lore);

    output.append(latest_output);
    if (latest_output.length()) output.append("<br>");

    latest_output = describe_feature_damage(f_idx, &lore);

    latest_output.append(describe_feature_movement_effects(f_idx, &lore));

    latest_output.append(describe_feature_combat_effects(f_idx, &lore));

    latest_output.append(describe_feature_stealth_effects(f_idx, &lore));

    output.append(latest_output);
    if (latest_output.length()) output.append("<br>");

    output.append(describe_feature_dynamic(f_idx, &lore));

    if (f_ptr->is_store())
    {
        output.append("<br>");
        output.append(describe_store(f_idx));
    }

    return (output);
}

/*
 * Display feature information
 */
void describe_feature(int f_idx, bool spoilers)
{
    QString output = get_feature_description(f_idx, spoilers, TRUE);

    /* Finally, display it */
    display_info_window(DISPLAY_INFO_FEATURE, f_idx, output);
}

