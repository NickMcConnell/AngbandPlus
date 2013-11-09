/* File: object1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 */

/* Purpose: Object code, part 1 */

#include "angband.h"

#if defined(MACINTOSH) || defined(MACH_O_CARBON)
#ifdef verify
#undef verify
#endif
#endif
/*
 * Reset the "visual" lists
 *
 * This involves resetting various things to their "default" state.
 *
 * If the "prefs" flag is TRUE, then we will also load the appropriate
 * "user pref file" based on the current setting of the "use_graphics"
 * flag.  This is useful for switching "graphics" on/off.
 *
 * The features, objects, and monsters, should all be encoded in the
 * relevant "font.pref" and/or "graf.prf" files.  XXX XXX XXX
 *
 * The "prefs" parameter is no longer meaningful.  XXX XXX XXX
 */
void reset_visuals(void)
{
    int i, j;

    /* Extract some info about terrain features */
    for (i = 0; i < max_f_idx; i++)
    {
        feature_type *f_ptr = &f_info[i];

        /* Assume we will use the underlying values */
        for (j = 0; j < F_LIT_MAX; j++)
        {
            f_ptr->x_attr[j] = f_ptr->d_attr[j];
            f_ptr->x_char[j] = f_ptr->d_char[j];
        }
    }

    /* Extract default attr/char code for objects */
    for (i = 0; i < max_k_idx; i++)
    {
        object_kind *k_ptr = &k_info[i];

        /* Default attr/char */
        k_ptr->x_attr = k_ptr->d_attr;
        k_ptr->x_char = k_ptr->d_char;
    }

    /* Extract default attr/char code for monsters */
    for (i = 0; i < max_r_idx; i++)
    {
        monster_race *r_ptr = &r_info[i];

        /* Default attr/char */
        r_ptr->x_attr = r_ptr->d_attr;
        r_ptr->x_char = r_ptr->d_char;
    }

    if (use_graphics)
    {
        char buf[1024];

        /* Process "graf.prf" */
        process_pref_file("graf.prf");

        /* Access the "character" pref file */
        sprintf(buf, "graf-%s.prf", player_base);

        /* Process "graf-<playername>.prf" */
        process_pref_file(buf);
    }

    /* Normal symbols */
    else
    {
        char buf[1024];

        /* Process "font.prf" */
        process_pref_file("font.prf");

        /* Access the "character" pref file */
        sprintf(buf, "font-%s.prf", player_base);

        /* Process "font-<playername>.prf" */
        process_pref_file(buf);
    }
}


/*
 * Obtain the "flags" for an item
 */
void weapon_flags(int hand, u32b flgs[TR_FLAG_SIZE])
{
    object_type *o_ptr = equip_obj(p_ptr->weapon_info[hand].slot);
    if (o_ptr)
    {
        int i;
        object_flags(o_ptr, flgs);
        for (i = 0; i < TR_FLAG_SIZE; i++)
            flgs[i] |= p_ptr->weapon_info[hand].flags[i];
    }
}

void weapon_flags_known(int hand, u32b flgs[TR_FLAG_SIZE])
{
    object_type *o_ptr = equip_obj(p_ptr->weapon_info[hand].slot);
    if (o_ptr)
    {
        int i;
        object_flags_known(o_ptr, flgs);
        /* TODO: Some of the following flags might not be known ... */
        for (i = 0; i < TR_FLAG_SIZE; i++)
            flgs[i] |= p_ptr->weapon_info[hand].flags[i];
    }
}

void missile_flags(object_type *arrow, u32b flgs[TR_FLAG_SIZE])
{
    int i;
    int slot = equip_find_first(object_is_bow);

    object_flags(arrow, flgs);
    for (i = 0; i < TR_FLAG_SIZE; i++)
        flgs[i] |= p_ptr->shooter_info.flags[i];

    if (slot)
    {
        object_type *bow = equip_obj(slot);
        u32b         bow_flgs[TR_FLAG_SIZE];

        object_flags(bow, bow_flgs);
        for (i = 0; i < TR_FLAG_SIZE; i++)
            flgs[i] |= bow_flgs[i]; /* Mask? */
    }
}

void missile_flags_known(object_type *arrow, u32b flgs[TR_FLAG_SIZE])
{
    int i;
    int slot = equip_find_first(object_is_bow);

    object_flags_known(arrow, flgs);
    for (i = 0; i < TR_FLAG_SIZE; i++)
        flgs[i] |= p_ptr->shooter_info.flags[i];

    if (slot)
    {
        object_type *bow = equip_obj(slot);
        u32b         bow_flgs[TR_FLAG_SIZE];

        object_flags_known(bow, bow_flgs);
        for (i = 0; i < TR_FLAG_SIZE; i++)
            flgs[i] |= bow_flgs[i]; /* Mask? */
    }
}

void object_flags(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE])
{
    object_kind *k_ptr = &k_info[o_ptr->k_idx];
    int i;

    /* Base object */
    for (i = 0; i < TR_FLAG_SIZE; i++)
        flgs[i] = k_ptr->flags[i];

    /* Artifact */
    if (object_is_fixed_artifact(o_ptr))
    {
        artifact_type *a_ptr = &a_info[o_ptr->name1];

        for (i = 0; i < TR_FLAG_SIZE; i++)
            flgs[i] = a_ptr->flags[i];
    }

    /* Ego-item */
    if (object_is_ego(o_ptr))
    {
        ego_item_type *e_ptr = &e_info[o_ptr->name2];

        for (i = 0; i < TR_FLAG_SIZE; i++)
            flgs[i] |= e_ptr->flags[i];

        if ((o_ptr->name2 == EGO_LITE_AURA_FIRE) && !o_ptr->xtra4 && (o_ptr->sval <= SV_LITE_LANTERN))
        {
            remove_flag(flgs, TR_SH_FIRE);
        }
        else if ((o_ptr->name2 == EGO_LITE_INFRA) && !o_ptr->xtra4 && (o_ptr->sval <= SV_LITE_LANTERN))
        {
            remove_flag(flgs, TR_INFRA);
        }
        else if ((o_ptr->name2 == EGO_LITE_EYE) && !o_ptr->xtra4 && (o_ptr->sval <= SV_LITE_LANTERN))
        {
            remove_flag(flgs, TR_RES_BLIND);
            remove_flag(flgs, TR_SEE_INVIS);
        }
    }

    /* Random artifact ! */
    for (i = 0; i < TR_FLAG_SIZE; i++)
        flgs[i] |= o_ptr->art_flags[i];

    if (object_is_smith(o_ptr))
    {
        int add = o_ptr->xtra3 - 1;

        if (add < TR_FLAG_MAX)
        {
            add_flag(flgs, add);
        }
        else if (add == ESSENCE_TMP_RES_ACID)
        {
            add_flag(flgs, TR_RES_ACID);
            add_flag(flgs, TR_ACTIVATE);
        }
        else if (add == ESSENCE_TMP_RES_ELEC)
        {
            add_flag(flgs, TR_RES_ELEC);
            add_flag(flgs, TR_ACTIVATE);
        }
        else if (add == ESSENCE_TMP_RES_FIRE)
        {
            add_flag(flgs, TR_RES_FIRE);
            add_flag(flgs, TR_ACTIVATE);
        }
        else if (add == ESSENCE_TMP_RES_COLD)
        {
            add_flag(flgs, TR_RES_COLD);
            add_flag(flgs, TR_ACTIVATE);
        }
        else if (add == ESSENCE_SH_FIRE)
        {
            add_flag(flgs, TR_RES_FIRE);
            add_flag(flgs, TR_SH_FIRE);
        }
        else if (add == ESSENCE_SH_ELEC)
        {
            add_flag(flgs, TR_RES_ELEC);
            add_flag(flgs, TR_SH_ELEC);
        }
        else if (add == ESSENCE_SH_COLD)
        {
            add_flag(flgs, TR_RES_COLD);
            add_flag(flgs, TR_SH_COLD);
        }
        else if (add == ESSENCE_RESISTANCE)
        {
            add_flag(flgs, TR_RES_ACID);
            add_flag(flgs, TR_RES_ELEC);
            add_flag(flgs, TR_RES_FIRE);
            add_flag(flgs, TR_RES_COLD);
        }
        else if (add == TR_IMPACT)
        {
            add_flag(flgs, TR_ACTIVATE);
        }
    }
}



/*
 * Obtain the "flags" for an item which are known to the player
 */
void object_flags_known(object_type *o_ptr, u32b flgs[TR_FLAG_SIZE])
{
    bool spoil = FALSE;
    int i;

    object_kind *k_ptr = &k_info[o_ptr->k_idx];

    /* Clear */
    for (i = 0; i < TR_FLAG_SIZE; i++)
        flgs[i] = 0;

    if (!object_is_aware(o_ptr) && !(o_ptr->ident & IDENT_STORE)) return;

    /* Base object */
    for (i = 0; i < TR_FLAG_SIZE; i++)
        flgs[i] = k_ptr->flags[i];

    /* Must be identified */
    if (!object_is_known(o_ptr)) return;

    /* Ego-item (known basic flags) */
    if (object_is_ego(o_ptr))
    {
        ego_item_type *e_ptr = &e_info[o_ptr->name2];

        for (i = 0; i < TR_FLAG_SIZE; i++)
            flgs[i] |= e_ptr->flags[i];

        if ((o_ptr->name2 == EGO_LITE_AURA_FIRE) && !o_ptr->xtra4 && (o_ptr->sval <= SV_LITE_LANTERN))
        {
            remove_flag(flgs, TR_SH_FIRE);
        }
        else if ((o_ptr->name2 == EGO_LITE_INFRA) && !o_ptr->xtra4 && (o_ptr->sval <= SV_LITE_LANTERN))
        {
            remove_flag(flgs, TR_INFRA);
        }
        else if ((o_ptr->name2 == EGO_LITE_EYE) && !o_ptr->xtra4 && (o_ptr->sval <= SV_LITE_LANTERN))
        {
            remove_flag(flgs, TR_RES_BLIND);
            remove_flag(flgs, TR_SEE_INVIS);
        }
    }


#ifdef SPOIL_ARTIFACTS
    /* Full knowledge for some artifacts */
    if (object_is_artifact(o_ptr)) spoil = TRUE;
#endif /* SPOIL_ARTIFACTS */

#ifdef SPOIL_EGO_ITEMS
    /* Full knowledge for some ego-items */
    if (object_is_ego(o_ptr)) spoil = TRUE;
#endif /* SPOIL_EGO_ITEMS */

    /* Need full knowledge or spoilers */
    if (spoil || (o_ptr->ident & IDENT_MENTAL))
    {
        /* Artifact */
        if (object_is_fixed_artifact(o_ptr))
        {
            artifact_type *a_ptr = &a_info[o_ptr->name1];

            for (i = 0; i < TR_FLAG_SIZE; i++)
                flgs[i] = a_ptr->flags[i];
        }

        /* Random artifact ! */
        for (i = 0; i < TR_FLAG_SIZE; i++)
            flgs[i] |= o_ptr->art_flags[i];
    }

    if (object_is_smith(o_ptr))
    {
        int add = o_ptr->xtra3 - 1;

        if (add < TR_FLAG_MAX)
        {
            add_flag(flgs, add);
        }
        else if (add == ESSENCE_TMP_RES_ACID)
        {
            add_flag(flgs, TR_RES_ACID);
        }
        else if (add == ESSENCE_TMP_RES_ELEC)
        {
            add_flag(flgs, TR_RES_ELEC);
        }
        else if (add == ESSENCE_TMP_RES_FIRE)
        {
            add_flag(flgs, TR_RES_FIRE);
        }
        else if (add == ESSENCE_TMP_RES_COLD)
        {
            add_flag(flgs, TR_RES_COLD);
        }
        else if (add == ESSENCE_SH_FIRE)
        {
            add_flag(flgs, TR_RES_FIRE);
            add_flag(flgs, TR_SH_FIRE);
        }
        else if (add == ESSENCE_SH_ELEC)
        {
            add_flag(flgs, TR_RES_ELEC);
            add_flag(flgs, TR_SH_ELEC);
        }
        else if (add == ESSENCE_SH_COLD)
        {
            add_flag(flgs, TR_RES_COLD);
            add_flag(flgs, TR_SH_COLD);
        }
        else if (add == ESSENCE_RESISTANCE)
        {
            add_flag(flgs, TR_RES_ACID);
            add_flag(flgs, TR_RES_ELEC);
            add_flag(flgs, TR_RES_FIRE);
            add_flag(flgs, TR_RES_COLD);
        }
    }
}


/*
 * Determine the "Activation" (if any) for an artifact
 * Return a string, or NULL for "no activation"
 */
cptr item_activation(object_type *o_ptr)
{
    u32b flgs[TR_FLAG_SIZE];

    /* Extract the flags */
    object_flags(o_ptr, flgs);

    /* Require activation ability */
    if (!(have_flag(flgs, TR_ACTIVATE))) return ("nothing");



    /*
     * We need to deduce somehow that it is a random artifact -- one
     * problem: It could be a random artifact which has NOT YET received
     * a name. Thus we eliminate other possibilities instead of checking
     * for art_name
     */

    if (!object_is_fixed_artifact(o_ptr) &&
        !object_is_ego(o_ptr) &&
        !(o_ptr->xtra1) &&
        (o_ptr->xtra2))
    {
        switch (o_ptr->xtra2)
        {
            case ACT_SUNLIGHT:
            {
                return "beam of sunlight every 10 turns";

            }
            case ACT_BO_MISS_1:
            {
                return "magic missile (2d6) every 2 turns";

            }
            case ACT_BA_POIS_1:
            {
                return "stinking cloud (12), rad. 3, every 4+d4 turns";

            }
            case ACT_BO_ELEC_1:
            {
                return "lightning bolt (4d8) every 5+d5 turns";

            }
            case ACT_BO_ACID_1:
            {
                return "acid bolt (5d8) every 6+d6 turns";

            }
            case ACT_BO_COLD_1:
            {
                return "frost bolt (6d8) every 7+d7 turns";

            }
            case ACT_BO_FIRE_1:
            {
                return "fire bolt (9d8) every 8+d8 turns";

            }
            case ACT_BA_COLD_1:
            {
                return "ball of cold (48) every 400 turns";

            }
            case ACT_BA_FIRE_1:
            {
                return "ball of fire (72) every 400 turns";

            }
            case ACT_DRAIN_1:
            {
                return "drain life (100) every 100+d100 turns";

            }
            case ACT_BA_COLD_2:
            {
                return "ball of cold (100) every 300 turns";

            }
            case ACT_BA_ELEC_2:
            {
                return "ball of lightning (100) every 500 turns";

            }
            case ACT_DRAIN_2:
            {
                return "drain life (120) every 400 turns";

            }
            case ACT_VAMPIRE_1:
            {
                return "vampiric drain (3*50) every 400 turns";

            }
            case ACT_BO_MISS_2:
            {
                return "arrows (150) every 90+d90 turns";

            }
            case ACT_BA_FIRE_2:
            {
                return "fire ball (120) every 225+d225 turns";

            }
            case ACT_BA_COLD_3:
            {
                return "ball of cold (200) every 325+d325 turns";

            }
            case ACT_BA_ELEC_3:
            {
                return "ball of lightning (250) every 425+d425 turns";

            }
            case ACT_WHIRLWIND:
            {
                return "whirlwind attack every 250 turns";

            }
            case ACT_VAMPIRE_2:
            {
                return "vampiric drain (3*100) every 400 turns";

            }
            case ACT_CALL_CHAOS:
            {
                return "call chaos every 350 turns";

            }
            case ACT_ROCKET:
            {
                return "launch rocket (120+level) every 400 turns";

            }
            case ACT_DISP_EVIL:
            {
                return "dispel evil (level*5) every 300+d300 turns";

            }
            case ACT_BA_MISS_3:
            {
                return "elemental breath (300) every 500 turns";

            }
            case ACT_DISP_GOOD:
            {
                return "dispel good (level*5) every 300+d300 turns";

            }
            case ACT_CONFUSE:
            {
                return "confuse monster every 15 turns";

            }
            case ACT_SLEEP:
            {
                return "sleep nearby monsters every 55 turns";

            }
            case ACT_QUAKE:
            {
                return "earthquake (rad 10) every 50 turns";

            }
            case ACT_TERROR:
            {
                return "terror every 3 * (level+10) turns";

            }
            case ACT_TELE_AWAY:
            {
                return "teleport away every 200 turns";

            }
            case ACT_BANISH_EVIL:
            {
                return "banish evil every 250+d250 turns";

            }
            case ACT_GENOCIDE:
            {
                return "genocide every 500 turns";

            }
            case ACT_MASS_GENO:
            {
                return "mass genocide every 1000 turns";

            }
            case ACT_CHARM_ANIMAL:
            {
                return "charm animal every 300 turns";

            }
            case ACT_CHARM_UNDEAD:
            {
                return "enslave undead every 333 turns";

            }
            case ACT_CHARM_OTHER:
            {
                return "charm monster every 400 turns";

            }
            case ACT_CHARM_ANIMALS:
            {
                return "animal friendship every 500 turns";

            }
            case ACT_CHARM_OTHERS:
            {
                return "mass charm every 750 turns";

            }
            case ACT_SUMMON_ANIMAL:
            {
                return "summon animal every 200+d300 turns";

            }
            case ACT_SUMMON_PHANTOM:
            {
                return "summon phantasmal servant every 200+d200 turns";

            }
            case ACT_SUMMON_ELEMENTAL:
            {
                return "summon elemental every 750 turns";

            }
            case ACT_SUMMON_DEMON:
            {
                return "summon demon every 666+d333 turns";

            }
            case ACT_SUMMON_UNDEAD:
            {
                return "summon undead every 666+d333 turns";

            }
            case ACT_CURE_LW:
            {
                return "heal 30 hp every 10 turns";

            }
            case ACT_CURE_MW:
            {
                return "heal 4d8 & wounds every 3+d3 turns";

            }
            case ACT_CURE_POISON:
            {
                return "cure poison every 5 turns";

            }
            case ACT_REST_LIFE:
            {
                return "restore life levels every 450 turns";

            }
            case ACT_REST_ALL:
            {
                return "restore stats and life levels every 750 turns";

            }
            case ACT_CURE_700:
            {
                return "heal 700 hit points every 250 turns";

            }
            case ACT_CURE_1000:
            {
                return "heal 1000 hit points every 888 turns";

            }
            case ACT_ESP:
            {
                return "telepathy (dur 25+d30) every 200 turns";

            }
            case ACT_BERSERK:
            {
                return "heroism and blessed (dur 50+d50) every 100+d100 turns";

            }
            case ACT_PROT_EVIL:
            {
                return "protect evil (dur level*3 + d25) every 225+d225 turns";

            }
            case ACT_RESIST_ALL:
            {
                return "resist elements (dur 40+d40) every 200 turns";

            }
            case ACT_SPEED:
            {
                return "speed (dur 20+d20) every 250 turns";

            }
            case ACT_XTRA_SPEED:
            {
                return "speed (dur 75+d75) every 200+d200 turns";

            }
            case ACT_WRAITH:
            {
                return "wraith form (dur level/2 + d(level/2)) every 1000 turns";

            }
            case ACT_INVULN:
            {
                return "invulnerability (dur 8+d8) every 1000 turns";

            }
            case ACT_LIGHT:
            {
                return "light area (dam 2d15) every 10+d10 turns";

            }
            case ACT_WIZ_LITE:
            {
                return "enlightenment every 20+d20 turns";
            }
            case ACT_MAP_LIGHT:
            {
                return "light (dam 2d15) & map area every 50+d50 turns";

            }
            case ACT_DETECT_ALL:
            {
                return "detection every 55+d55 turns";

            }
            case ACT_DETECT_XTRA:
            {
                return "detection, probing and identify true every 1000 turns";

            }
            case ACT_ID_FULL:
            {
                return "identify true every 750 turns";

            }
            case ACT_ID_PLAIN:
            {
                return "identify spell every 10 turns";

            }
            case ACT_RUNE_EXPLO:
            {
                return "explosive rune every 200 turns";

            }
            case ACT_RUNE_PROT:
            {
                return "rune of protection every 400 turns";

            }
            case ACT_SATIATE:
            {
                return "satisfy hunger every 200 turns";

            }
            case ACT_DEST_DOOR:
            {
                return "destroy doors every 10 turns";

            }
            case ACT_STONE_MUD:
            {
                return "stone to mud every 5 turns";

            }
            case ACT_RECHARGE:
            {
                return "recharging every 70 turns";

            }
            case ACT_ALCHEMY:
            {
                return "alchemy every 500 turns";

            }
            case ACT_DIM_DOOR:
            {
                return "dimension door every 100 turns";

            }
            case ACT_TELEPORT:
            {
                return "teleport (range 100) every 45 turns";

            }
            case ACT_RECALL:
            {
                return "word of recall every 200 turns";

            }
            default:
            {
                return "something undefined";

            }
        }
    }

    /* Some artifacts can be activated */
    switch (o_ptr->name1)
    {
        case ART_HOLY_GRAIL:
            return "Heal 50hp and Resist Magic every 12 turns";
        case ART_ZEUS:
            return "Lightning Storm (555) every 5 turns";
        case ART_POSEIDON:
            return "Earthquake every turn";
        case ART_HADES:
            return "Restoration every 500 turns";
        case ART_ATHENA:
            return "Recharging every 15 turns";
        case ART_ARES:
            return "Berserk every 20 turns";
        case ART_HERMES:
            return "Haste and Dimension Door every 100 turns";
        case ART_APOLLO:
            return "Clairvoyance every 20 turns";
        case ART_ARTEMIS:
            return "Create Arrows every 999 turns";
        case ART_HEPHAESTUS:
            return "Enchantment every 200 turns";
        case ART_HERA:
            return "Restore Mana every 55 turns";
        case ART_DEMETER:
            return "Heal (500) and Satisfy Hunger every 100 turns";
        case ART_APHRODITE:
            return "Summon Pet every 25 turns";

        case ART_NARTHANC:
        {
            return "fire bolt (9d8) every 8+d8 turns";

        }
        case ART_GONG:
            return "a loud bong every 5+d5 turns";
        case ART_STOMPER:
            return "earthquake every 35 turns";
        case ART_RAILGUN:
            return "beam of light (300) every turn";
        case ART_KAMIKAZE_ROBE:
            return "Speed Essentia every 111 turns";
        case ART_BALLISTA:
            return "piercing shot every 100 turns";
        case ART_STONE_OF_NATURE:
            return "stone skin every 100 turns";
        case ART_STONE_OF_LIFE:
            return "restoring every 500 turns";
        case ART_STONE_OF_SORCERY:
            return "haste self every 100 turns";
        case ART_STONE_OF_CHAOS:
            return "polymorph self every 500 turns";
        case ART_STONE_OF_DEATH:
            return "animate dead every 666 turns";
        case ART_STONE_OF_TRUMP:
            return "teleport self every turn";
        case ART_STONE_OF_DAEMON:
            return "breathe fire every 666 turns";
        case ART_STONE_OF_CRUSADE:
            return "protection from evil every 555 turns";
        case ART_STONE_OF_CRAFT:
            return "resistance every 100 turns";
        case ART_STONE_OF_WAR:
            return "berserk rage every 100 turns";
        case ART_STONE_OF_ARMAGEDDON:
            return "destruction every 10 turns";
        case ART_OMARAX:
            return "clairvoyance every 20+d20 turns";
        case ART_LERNEAN:
            return "heal 700hp every 300 turns";
        case ART_NIMTHANC:
        {
            return "frost bolt (6d8) every 7+d7 turns";

        }
        case ART_DETHANC:
        {
            return "lightning bolt (4d8) every 6+d6 turns";

        }
        case ART_RILIA:
        {
            return "stinking cloud (12) every 4+d4 turns";

        }
        case ART_FIONA:
        {
            return "frost ball (48) every 5+d5 turns";

        }
        case ART_FLORA:
        {
            return "remove fear and cure poison every 5 turns";

        }
        case ART_RINGIL:
        {
            return "frost ball (100) every 200 turns";

        }
        case ART_DAWN:
        {
            return "summon the Legion of the Dawn every 500+d500 turns";

        }
        case ART_ANDURIL:
        {
            return "fire ball (72) every 400 turns";

        }
        case ART_FIRESTAR:
        {
            return "large fire ball (72) every 100 turns";

        }
        case ART_AEGIR:
        {
            return "polymorph colossus every 500 turns";
        }
        case ART_DEFENDER_OF_THE_CROWN:
        {
            return "stone skin every 300 turns";
        }
        case ART_MONKEY_KING:
        {
            return "enlarge weapon every 555 turns";
        }
        case ART_MAUL_OF_VICE:
            return "light speed every 888 turns";
        case ART_GOTHMOG:
        {
            return "large fire ball (120) every 15 turns";

        }
        case ART_FEANOR:
        {
            return "haste self (20+d20 turns) every 200 turns";

        }
        case ART_THEODEN:
        {
            return "drain life (120) every 400 turns";

        }
        case ART_TURMIL:
        {
            return "drain life (90) every 70 turns";

        }
        case ART_CASPANION:
        {
            return "door and trap destruction every 10 turns";

        }
        case ART_AVAVIR:
        case ART_MAGATAMA:
        case ART_HEAVENLY_MAIDEN:
        {
            return "word of recall every 200 turns";

        }
        case ART_TARATOL:
        {
            return "haste self (20+d20 turns) every 100+d100 turns";

        }
        case ART_ERIRIL:
        {
            return "identify every 10 turns";

        }
        case ART_GANDALF:
        {
            return "probing, detection and full id every 100 turns";

        }
        case ART_EONWE:
        {
            return "mass genocide every 1000 turns";

        }
        case ART_LOTHARANG:
        {
            return "cure wounds (4d8) every 3+d3 turns";

        }
        case ART_CRIMSON:
        {
            return "fire! every 15 turns";

        }
        case ART_KUSANAGI:
        case ART_WEREWINDLE:
        {
            return "a getaway every 35 turns";

        }
        case ART_KAMUI:
        {
            return "a teleport every 25 turns";

        }
        case ART_RUNESPEAR:
        {
            return "lightning ball (100) every 200 turns";

        }
        case ART_AEGLOS:
        {
            return "frost ball (100) every 200 turns";

        }
        case ART_DESTINY:
        {
            return "stone to mud every 5 turns";

        }
        case ART_NAIN:
        {
            return "stone to mud every 2 turns";

        }
        case ART_SOULKEEPER:
        {
            return "heal (1000) every 888 turns";

        }
        case ART_DUELIST: return "Strafing every 3 turns";
        case ART_LOHENGRIN:
        case ART_DAERON:
        {
            return ("heal (777), curing and heroism every 300 turns");

        }
        case ART_MAGLOR:
        {
            return ("restore mana every 777 turns");
        }
        case ART_JULIAN:
        {
            return "genocide every 500 turns";

        }
        case ART_LUTHIEN:
        {
            return "restore life levels every 450 turns";

        }
        case ART_ULMO:
        {
            return "teleport away every 150 turns";

        }
        case ART_COLLUIN:
        case ART_SARUMAN:
        case ART_SEIRYU:
        {
            return "resistance (20+d20 turns) every 111 turns";

        }
        case ART_BLOODRIP:
        {
            return "whirlwind attack every 66 turns";
        }
        case ART_HOLCOLLETH:
        {
            return "sleep monsters every 55 turns";

        }
        case ART_THINGOL:
        {
            return "recharge item every 70 turns";

        }
        case ART_COLANNON:
        {
            return "teleport every 45 turns";

        }
        case ART_TOTILA:
        {
            return "confuse monster every 15 turns";

        }
        case ART_CAMMITHRIM:
        {
            return "magic missile (2d6) every 2 turns";

        }
        case ART_FINGOLFIN:
        {
            return "a magical arrow (150) every 90+d90 turns";

        }
        case ART_HOLHENNETH:
        {
            return "detection every 55+d55 turns";

        }
        case ART_AMBER:
        {
            return "heal (700) every 250 turns";

        }
        case ART_RAZORBACK:
        {
            return "star ball (150) every 1000 turns";

        }
        case ART_BLADETURNER:
        {
            return "breathe elements (300), hero, bless, and resistance";

        }
        case ART_GALADRIEL:
        {
            return "illumination every 10+d10 turns";

        }
        case ART_ELENDIL:
        {
            return "magic mapping and light every 50+d50 turns";

        }
        case ART_EYE_OF_VECNA:
            return "the Vision of Vecna, every 20+d20 turns";
        case ART_JUDGE:
        {
            return "clairvoyance and recall, draining you every 20+d20 turns";

        }
        case ART_INGWE:
        case ART_YATA:
        {
            return "dispel evil (x5) every 200+d200 turns";

        }
        case ART_FUNDIN:
        {
            return "dispel evil (x5) every 100+d100 turns";

        }
        case ART_CARLAMMAS:
        case ART_HERMIT:
        {
            return "protection from evil every 225+d225 turns";

        }
        case ART_FRAKIR:
        {
            return "a strangling attack (100) every 100+d100 turns";

        }
        case ART_TULKAS:
        {
            return "haste self (75+d75 turns) every 150+d150 turns";

        }
        case ART_NARYA:
        {
            return "large fire ball (300) every 225+d225 turns";

        }
        case ART_NENYA:
        {
            return "large frost ball (400) every 325+d325 turns";

        }
        case ART_VILYA:
        case ART_GOURYU:
        {
            return "large lightning ball (500) every 425+d425 turns";

        }
        case ART_POWER:
        case ART_AHO:
        {
            return "bizarre things every 450+d450 turns";

        }
        case ART_DOR: case ART_TERROR: case ART_STONEMASK:
        {
            return "rays of fear in every direction every 3*(level+10) turns";

        }
        case ART_PALANTIR:
        {
            return "list of the uniques on the level every 200 turns";
        }
        case ART_FARAMIR:
        {
            return "dispel small life every 55+d55 turns";
        }
        case ART_BOROMIR:
        {
            return "frighten monsters every 40+d40 turns";
        }
        case ART_HIMRING:
        {
            return "protection from evil every 200 + d200 turns";
        }
        case ART_ICANUS:
        {
            return "a mana bolt (120) every 120+d120 turns";
        }
        case ART_HURIN:
        {
            return "hero and +10 to speed (50) every 100+200d turns";
        }
        case ART_GIL_GALAD:
        {
            return "blinding light every 250 turns";
        }
        case ART_YENDOR:
        {
            return "recharge item every 200 turns";
        }
        case ART_MURAMASA:
        {
            return "increase STR (destroyed 50%)";
        }
        case ART_FLY_STONE:
        {
            return "a mana storm every 250+d250 turns";
        }
        case ART_JONES:
        {
            return "a telekinesis (500 lb) every 25+d25 turns";
        }
        case ART_ARRYU:
        {
            return "summon hound every 300+d150 turns";
        }
        case ART_GAEBOLG:
        {
            return "large star ball (200) every 200+d200 turns";

        }
        case ART_INROU:
        {
            return "reveal your identity every 150+d150 turns";

        }
        case ART_HYOUSIGI:
        {
            return "beat wooden clappers every turn";

        }
        case ART_MATOI:
        case ART_AEGISFANG:
        {
            return "heroism every 30+d30 turns";

        }

        case ART_EARENDIL:
        {
            return "curing every 100 turns";

        }

        case ART_BOLISHOI:
        {
            return "charm animal every 200 turns";

        }
        case ART_ARUNRUTH:
        {
            return "frost bolt (12d8) every 50 turns";

        }
        case ART_BLOOD:
        {
            return "change zokusei every 3333 turns";

        }
        case ART_NUMAHOKO:
        {
            return "water ball (200) every 250 turns";

        }
        case ART_KESHO:
        {
            return "shiko every 100+d100 turns";

        }
        case ART_MOOK:
        {
            return "resist cold every 40+d40 turns";

        }
        case ART_JIZO:
        {
            return "summon octopus every 300+d150 turns";
        }
        case ART_NIGHT:
        case ART_HELL:
        {
            return "darkness storm (250) every 150+d150 turns";

        }
        case ART_SACRED_KNIGHTS:
        {
            return "dispel curse and probing every turn";

        }
        case ART_CHARMED:
        {
            return "restore mana every 777 turns";

        }
        case ART_AESCULAPIUS:
        {
            return "restore stats and life levels every 750 turns";
        }
    }


    if ((o_ptr->tval == TV_POLEARM) && (o_ptr->sval == SV_TSURIZAO))
    {
        return "fishing : every time";

    }

    if (object_is_smith(o_ptr))
    {
        switch (o_ptr->xtra3 - 1)
        {
        case ESSENCE_TMP_RES_ACID:
            return "resist acid every 50+d50 turns";

        case ESSENCE_TMP_RES_ELEC:
            return "resist elec every 50+d50 turns";

        case ESSENCE_TMP_RES_FIRE:
            return "resist fire every 50+d50 turns";

        case ESSENCE_TMP_RES_COLD:
            return "resist cold every 50+d50 turns";

        case TR_IMPACT:
            return "earthquake every 100+d100 turns";
        }
    }

    if (o_ptr->name2 == EGO_TRUMP)
    {
        return "teleport every 50+d50 turns";

    }

    if (o_ptr->name2 == EGO_LITE_ILLUMINATION)
    {
            return "illumination every 10+d10 turns";
    }

    else if (o_ptr->name2 == EGO_EARTHQUAKES)
    {
        return "earthquake every 100+d100 turns";

    }

    else if (o_ptr->name2 == EGO_JUMP)
    {
        return "blink every 10+d10 turns";

    }

    else if (o_ptr->name2 == EGO_DAEMON)
    {
        return "destruction every 100+d100 turns";
    }

    if (o_ptr->tval == TV_RING)
    {
        if (object_is_ego(o_ptr))
        {
            switch (o_ptr->name2)
            {
            case EGO_RING_HERO:
                return "heroism every 100+d100 turns";
            case EGO_RING_MAGIC_MIS:
            return "magic missile (2d6) every 2 turns";
            case EGO_RING_FIRE_BOLT:
            return "fire bolt (9d8) every 8+d8 turns";
            case EGO_RING_COLD_BOLT:
                return "frost bolt (6d8) every 7+d7 turns";
            case EGO_RING_ELEC_BOLT:
                return "lightning bolt (4d8) every 5+d5 turns";
            case EGO_RING_ACID_BOLT:
                return "acid bolt (5d8) every 6+d6 turns";
            case EGO_RING_MANA_BOLT:
            return "a mana bolt (120) every 120+d120 turns";
            case EGO_RING_FIRE_BALL:
                return "fire ball (100) every 80+d80 turns";
            case EGO_RING_COLD_BALL:
                return "cold ball (100) every 80+d80 turns";
            case EGO_RING_ELEC_BALL:
                return "elec ball (100) every 80+d80 turns";
            case EGO_RING_ACID_BALL:
                return "acid ball (100) every 80+d80 turns";
            case EGO_RING_MANA_BALL:
                return "mana storm (250) every 300 turns";
            case EGO_RING_DRAGON_F:
                if (o_ptr->sval == SV_RING_FLAMES)
                    return "breath of fire (200) and resist fire every 200 turns";
                else
                    return "fire breath (200) every 250 turns";
            case EGO_RING_DRAGON_C:
                if (o_ptr->sval == SV_RING_ICE)
                    return "breath of cold (200) and resist cold every 200 turns";
                else
                    return "cold breath (200) every 250 turns";
            case EGO_RING_M_DETECT:
                return "detect all monsters every 150 turns";
            case EGO_RING_D_SPEED:
                return "haste self (15+d30 turns) every 100 turns";
            case EGO_RING_BERSERKER:
                return "berserk (25+d25 turns) every 75+d75 turns";
            case EGO_RING_TELE_AWAY:
            return "teleport away every 150 turns";
            case EGO_RING_TRUE:
            return "hero, bless, and ultimate resistance every 777 turns";
            }
        }
        switch (o_ptr->sval)
        {
            case SV_RING_FLAMES:
                return "ball of fire (100) and resist fire every 50+d50 turns";

            case SV_RING_ICE:
                return "ball of cold (100) and resist cold every 50+d50 turns";

            case SV_RING_ACID:
                return "ball of acid (100) and resist acid every 50+d50 turns";

            case SV_RING_ELEC:
                return "ball of elec (100) and resist elec every 50+d50 turns";

            default:
                return NULL;
        }
    }

    if (o_ptr->tval == TV_AMULET)
    {
        if (object_is_ego(o_ptr))
        {
            switch (o_ptr->name2)
            {
            case EGO_AMU_IDENT:
                return "identify every 10 turns";
            case EGO_AMU_CHARM:
                return "charm monster every 200 turns";
            case EGO_AMU_JUMP:
                return "blink every 10+d10 turns";
            case EGO_AMU_RESISTANCE:
                return "resist elements every 75+d75 turns";

            case EGO_AMU_TELEPORT:
                return "teleport every 50+d50 turns";
            case EGO_AMU_D_DOOR:
                return "dimension door every 200 turns";
            case EGO_AMU_RES_FIRE_:
                return "resist fire every 50+d50 turns";
            case EGO_AMU_RES_COLD_:
                return "resist cold every 50+d50 turns";
            case EGO_AMU_RES_ELEC_:
                return "resist elec every 50+d50 turns";
            case EGO_AMU_RES_ACID_:
                return "resist acid every 50+d50 turns";
            case EGO_AMU_DETECTION:
                return "detect all floor every 55+d55 turns";
            }
        }
    }

    if (o_ptr->tval == TV_WHISTLE)
    {
        return "call pet every 100+d100 turns";
    }

    if (o_ptr->tval == TV_CAPTURE)
    {
        return "captures or releases a monster.";
    }

    /* Require dragon scale mail */
    if (o_ptr->tval != TV_DRAG_ARMOR) return ("a strange glow");


    /* Branch on the sub-type */
    switch (o_ptr->sval)
    {
        case SV_DRAGON_BLUE:
        {
            return "breathe lightning (100) every 15+d15 turns";

        }
        case SV_DRAGON_WHITE:
        {
            return "breathe frost (110) every 15+d15 turns";

        }
        case SV_DRAGON_BLACK:
        {
            return "breathe acid (130) every 15+d15 turns";

        }
        case SV_DRAGON_GREEN:
        {
            return "breathe poison gas (150) every 18+d18 turns";

        }
        case SV_DRAGON_RED:
        {
            return "breathe fire (200) every 20+d20 turns";

        }
        case SV_DRAGON_MULTIHUED:
        {
            return "breathe multi-hued (250) every 20+d20 turns";

        }
        case SV_DRAGON_BRONZE:
        {
            return "breathe confusion (120) every 18+d18 turns";

        }
        case SV_DRAGON_GOLD:
        {
            return "breathe sound (130) every 18+d18 turns";

        }
        case SV_DRAGON_CHAOS:
        {
            return "breathe chaos/disenchant (220) every 20+d20 turns";

        }
        case SV_DRAGON_LAW:
        {
            return "breathe sound/shards (230) every 20+d20 turns";

        }
        case SV_DRAGON_BALANCE:
        {
            return "breathe balance (250) every 20+d20 turns";

        }
        case SV_DRAGON_SHINING:
        {
            return "breathe light/darkness (200) every 20+d20 turns";

        }
        case SV_DRAGON_POWER:
        {
            return "breathe the elements (300) every 20+d20 turns";

        }
    }

    /* Oops */
    return "breathe air";

}


/*
 * Describe a "fully identified" item
 */
bool screen_object(object_type *o_ptr, u32b mode)
{
    int                     i = 0, j, k;

    u32b flgs[TR_FLAG_SIZE];

    char temp[70 * 20];
    cptr            info[128];
    char o_name[MAX_NLEN];
    char replacement_name[MAX_NLEN];
    int wid, hgt;

    int trivial_info = 0;

    /* Extract the flags */
    object_flags_known(o_ptr, flgs);

    /* Extract the description */
    if (object_is_device(o_ptr))
    {
        char temp2[70 * 20];
        cptr res = do_device(o_ptr->tval, o_ptr->sval, SPELL_DESC);
        strcpy(temp2, res);
        if (o_ptr->ident & IDENT_MENTAL)
        {
            res = do_device(o_ptr->tval, o_ptr->sval, SPELL_INFO);
            if (res && strlen(res))
            {   /* Here is a classic case where calling format() leads to bugs ... sigh */
                strcat(temp2, "\nInfo: ");
                strcat(temp2, res);
            }   /* But format() here is fine ... Obvious, huh? */
            if (o_ptr->tval != TV_POTION)
            {
                int fail = device_calc_fail_rate(o_ptr);
                strcat(temp2, format("\nFail: %d.%d%%", fail/10, fail%10));    
            }
        }
        roff_to_buf(temp2, 77-15, temp, sizeof(temp));
        for (j = 0; temp[j]; j += 1 + strlen(&temp[j]))
        { info[i] = &temp[j]; i++;}
    }
    else
    {
        roff_to_buf(o_ptr->name1 ? (a_text + a_info[o_ptr->name1].text) :
                (k_text + k_info[o_ptr->k_idx].text),
                77 - 15, temp, sizeof(temp));
        for (j = 0; temp[j]; j += 1 + strlen(&temp[j]))
        { info[i] = &temp[j]; i++;}
    }
    if ( p_ptr->prace == RACE_MON_POSSESSOR 
      && o_ptr->tval == TV_CORPSE 
      && o_ptr->sval == SV_CORPSE
      && (o_ptr->ident & IDENT_MENTAL) )
    {
        monster_race *r_ptr = &r_info[o_ptr->pval];
        char          temp2[70*20];

        sprintf(temp2, "Body: %s\n \nStr : %+3d       Disarm : %d+%d\nInt : %+3d       Device : %d+%d\nWis : %+3d       Save   : %d+%d\nDex : %+3d       Stealth: %d+%d\nCon : %+3d       Search : %d/%d\nChr : %+3d       Melee  : %d+%d\nLife: %3d%%      Ranged : %d+%d\n \n \n",
            b_name + b_info[r_ptr->body.body_idx].name,
            r_ptr->body.stats[A_STR], r_ptr->body.skills.dis, r_ptr->body.extra_skills.dis,
            r_ptr->body.stats[A_INT], r_ptr->body.skills.dev, r_ptr->body.extra_skills.dev,
            r_ptr->body.stats[A_WIS], r_ptr->body.skills.sav, r_ptr->body.extra_skills.sav,
            r_ptr->body.stats[A_DEX], r_ptr->body.skills.stl, r_ptr->body.extra_skills.stl,
            r_ptr->body.stats[A_CON], r_ptr->body.skills.srh, r_ptr->body.skills.fos,
            r_ptr->body.stats[A_CHR], r_ptr->body.skills.thn, r_ptr->body.extra_skills.thn,
            r_ptr->body.life, r_ptr->body.skills.thb, r_ptr->body.extra_skills.thb
        );
            
        roff_to_buf(temp2, 77-15, temp, sizeof(temp));
        for (j = 0; temp[j]; j += 1 + strlen(&temp[j]))
        { info[i] = &temp[j]; i++;}
    }

    if (object_is_equipment(o_ptr))
    {
        /* Descriptions of a basic equipment is just a flavor */
        trivial_info = i;
    }

    /* Mega-Hack -- describe activation */
    if (have_flag(flgs, TR_ACTIVATE))
    {
        info[i++] = "It can be activated for...";

        info[i++] = item_activation(o_ptr);
        info[i++] = "...if it is being worn.";

    }

    /* Figurines, a hack */
    if (o_ptr->tval == TV_FIGURINE)
    {
        info[i++] = "It will transform into a pet when thrown.";

    }

    if (o_ptr->name1 == ART_STONE_OF_NATURE)
        info[i++] = "It greatly enhances Nature magic.";
    if (o_ptr->name1 == ART_STONE_OF_LIFE)
        info[i++] = "It greatly enhances Life magic.";
    if (o_ptr->name1 == ART_STONE_OF_SORCERY)
        info[i++] = "It greatly enhances Sorcery magic.";
    if (o_ptr->name1 == ART_STONE_OF_CHAOS)
        info[i++] = "It greatly enhances Chaos magic.";
    if (o_ptr->name1 == ART_STONE_OF_DEATH)
        info[i++] = "It greatly enhances Death magic.";
    if (o_ptr->name1 == ART_STONE_OF_TRUMP)
        info[i++] = "It greatly enhances Trump magic.";
    if (o_ptr->name1 == ART_STONE_OF_DAEMON)
        info[i++] = "It greatly enhances Daemon magic.";
    if (o_ptr->name1 == ART_STONE_OF_CRUSADE)
        info[i++] = "It greatly enhances Crusade magic.";
    if (o_ptr->name1 == ART_STONE_OF_CRAFT)
        info[i++] = "It greatly enhances Craft magic.";
    if (o_ptr->name1 == ART_STONE_OF_ARMAGEDDON)
        info[i++] = "It greatly enhances Armageddon magic.";

    if (o_ptr->name1 == ART_STONEMASK)
    {
        info[i++] = "It makes you turn into a vampire permanently.";

    }

    if ((o_ptr->tval == TV_SWORD) && (o_ptr->sval == SV_DOKUBARI))
    {
        info[i++] = "It will attempt to kill a monster instantly.";

    }

    if ((o_ptr->tval == TV_POLEARM) && (o_ptr->sval == SV_DEATH_SCYTHE))
    {
        info[i++] = "It causes you to strike yourself sometimes.";

        info[i++] = "It always penetrates invulnerability barriers.";
    }

    if (o_ptr->name2 == EGO_GENJI || o_ptr->name1 == ART_MASTER_TONBERRY || o_ptr->name1 == ART_MEPHISTOPHELES)
        info[i++] = "It affects your ability to hit when you are wielding two weapons.";

    if (have_flag(flgs, TR_WEAPONMASTERY))
        info[i++] = "It increases the damage dice of your melee weapon.";

    if (o_ptr->name2 == EGO_SNIPER)
        info[i++] = "It increases your shooting speed.";

    if (have_flag(flgs, TR_EASY_SPELL))
    {
        info[i++] = "It affects your ability to cast spells.";
    }

    if (o_ptr->name2 == EGO_AMU_FOOL)
    {
        info[i++] = "It interferes with casting spells.";
    }

    if (o_ptr->name2 == EGO_RING_THROW)
    {
        info[i++] = "It provides great strength when you throw an item.";
    }

    if (o_ptr->name2 == EGO_AMU_NAIVETY)
    {
        info[i++] = "It decreases your magic resistance.";
    }

    if (o_ptr->tval == TV_STATUE)
    {
        monster_race *r_ptr = &r_info[o_ptr->pval];

        if (o_ptr->pval == MON_BULLGATES)
            info[i++] = "It is shameful.";
        else if ( r_ptr->flags2 & (RF2_ELDRITCH_HORROR))
            info[i++] = "It is fearful.";
        else
            info[i++] = "It is cheerful.";
    }
    
    /* Hack -- describe lite's */
    if (o_ptr->tval == TV_LITE)
    {
        if (o_ptr->name2 == EGO_LITE_DARKNESS || have_flag(o_ptr->art_flags, TR_DARKNESS))
        {
            info[i++] = "It provides no light.";

            if (o_ptr->sval == SV_LITE_LANTERN)
            {
                info[i++] = "It decreases radius of light source by 2.";
            }
            else if (o_ptr->sval == SV_LITE_TORCH)
            {
                info[i++] = "It decreases radius of light source by 1.";
            }
            else
            {
                info[i++] = "It decreases radius of light source by 3.";
            }
        }
        else if (o_ptr->name1 || o_ptr->art_name)
        {
            if (o_ptr->name1 == ART_EYE_OF_VECNA)
            {
                info[i++] = "It allows you to see in the dark.";
            }
            else
            {
                info[i++] = "It provides light (radius 3) forever.";
            }
        }
        else if (o_ptr->name2 == EGO_LITE_SHINE)
        {
            if (o_ptr->sval == SV_LITE_FEANOR)
            {
                info[i++] = "It provides light (radius 3) forever.";

            }
            else if (o_ptr->sval == SV_LITE_LANTERN)
            {
                info[i++] = "It provides light (radius 3) when fueled.";

            }
            else
            {
                info[i++] = "It provides light (radius 2) when fueled.";

            }
        }
        else
        {
            if (o_ptr->sval == SV_LITE_FEANOR)
            {
                info[i++] = "It provides light (radius 2) forever.";

            }
            else if (o_ptr->sval == SV_LITE_LANTERN)
            {
                info[i++] = "It provides light (radius 2) when fueled.";

            }
            else
            {
                info[i++] = "It provides light (radius 1) when fueled.";

            }
        }
        if (o_ptr->name2 == EGO_LITE_LONG)
        {
            info[i++] = "It provides light for much longer time.";
        }
    }


    /* And then describe it fully */

    if (have_flag(flgs, TR_RIDING))
    {
        if ((o_ptr->tval == TV_POLEARM) && ((o_ptr->sval == SV_LANCE) || (o_ptr->sval == SV_HEAVY_LANCE)))
            info[i++] = "It is made for use while riding.";
        else
        {
            info[i++] = "It is suitable for use while riding.";
            /* This information is not important enough */
            trivial_info++;
        }
    }
    if (have_flag(flgs, TR_STR))
    {
        info[i++] = "It affects your strength.";

    }
    if (have_flag(flgs, TR_INT))
    {
        info[i++] = "It affects your intelligence.";

    }
    if (have_flag(flgs, TR_WIS))
    {
        info[i++] = "It affects your wisdom.";

    }
    if (have_flag(flgs, TR_DEX))
    {
        info[i++] = "It affects your dexterity.";

    }
    if (have_flag(flgs, TR_CON))
    {
        info[i++] = "It affects your constitution.";

    }
    if (have_flag(flgs, TR_CHR))
    {
        info[i++] = "It affects your charisma.";

    }

    if (have_flag(flgs, TR_MAGIC_MASTERY))
    {
        info[i++] = "It affects your ability to use magic devices.";

    }
    if (have_flag(flgs, TR_STEALTH))
    {
        info[i++] = "It affects your stealth.";

    }
    if (have_flag(flgs, TR_SEARCH))
    {
        info[i++] = "It affects your searching.";

    }
    if (have_flag(flgs, TR_INFRA))
    {
        info[i++] = "It affects your infravision.";

    }
    if (have_flag(flgs, TR_TUNNEL))
    {
        info[i++] = "It affects your ability to tunnel.";

    }
    if (have_flag(flgs, TR_SPEED))
    {
        info[i++] = "It affects your speed.";

    }
    if (have_flag(flgs, TR_BLOWS))
    {
        info[i++] = "It affects your attack speed.";

    }

    if (have_flag(flgs, TR_BRAND_ACID))
    {
        info[i++] = "It does extra damage from acid.";

    }
    if (have_flag(flgs, TR_BRAND_ELEC))
    {
        info[i++] = "It does extra damage from electricity.";

    }
    if (have_flag(flgs, TR_BRAND_FIRE))
    {
        info[i++] = "It does extra damage from fire.";

    }
    if (have_flag(flgs, TR_BRAND_COLD))
    {
        info[i++] = "It does extra damage from frost.";

    }

    if (have_flag(flgs, TR_BRAND_POIS))
    {
        info[i++] = "It poisons your foes.";

    }

    if (have_flag(flgs, TR_CHAOTIC))
    {
        info[i++] = "It produces chaotic effects.";

    }

    if (have_flag(flgs, TR_VAMPIRIC))
    {
        info[i++] = "It drains life from your foes.";

    }

    if (have_flag(flgs, TR_IMPACT))
    {
        info[i++] = "It can cause earthquakes.";

    }

    if (have_flag(flgs, TR_VORPAL))
    {
        info[i++] = "It is very sharp and can cut your foes.";

    }

    if (have_flag(flgs, TR_ORDER))
        info[i++] = "It is a weapon of order.";

    if (have_flag(flgs, TR_WILD))
        info[i++] = "It is a wild weapon.";

    if (have_flag(flgs, TR_KILL_DRAGON))
    {
        info[i++] = "It is a great bane of dragons.";

    }
    else if (have_flag(flgs, TR_SLAY_DRAGON))
    {
        info[i++] = "It is especially deadly against dragons.";

    }

    if (have_flag(flgs, TR_KILL_ORC))
    {
        info[i++] = "It is a great bane of orcs.";

    }
    if (have_flag(flgs, TR_SLAY_ORC))
    {
        info[i++] = "It is especially deadly against orcs.";

    }

    if (have_flag(flgs, TR_KILL_TROLL))
    {
        info[i++] = "It is a great bane of trolls.";

    }
    if (have_flag(flgs, TR_SLAY_TROLL))
    {
        info[i++] = "It is especially deadly against trolls.";

    }

    if (have_flag(flgs, TR_KILL_GIANT))
    {
        info[i++] = "It is a great bane of giants.";
    }
    else if (have_flag(flgs, TR_SLAY_GIANT))
    {
        info[i++] = "It is especially deadly against giants.";

    }

    if (have_flag(flgs, TR_KILL_DEMON))
    {
        info[i++] = "It is a great bane of demons.";

    }
    if (have_flag(flgs, TR_SLAY_DEMON))
    {
        info[i++] = "It strikes at demons with holy wrath.";

    }

    if (have_flag(flgs, TR_KILL_UNDEAD))
    {
        info[i++] = "It is a great bane of undead.";

    }
    if (have_flag(flgs, TR_SLAY_UNDEAD))
    {
        info[i++] = "It strikes at undead with holy wrath.";

    }

    if (have_flag(flgs, TR_KILL_EVIL))
    {
        info[i++] = "It is a great bane of evil monsters.";

    }
    if (have_flag(flgs, TR_SLAY_EVIL))
    {
        info[i++] = "It fights against evil with holy fury.";

    }
    if (have_flag(flgs, TR_SLAY_GOOD))
    {
        info[i++] = "It fights against good with hellish glee.";

    }

    if (have_flag(flgs, TR_KILL_ANIMAL))
    {
        info[i++] = "It is a great bane of natural creatures.";

    }
    if (have_flag(flgs, TR_SLAY_ANIMAL))
    {
        info[i++] = "It is especially deadly against natural creatures.";

    }

    if (have_flag(flgs, TR_KILL_HUMAN))
    {
        info[i++] = "It is a great bane of humans.";

    }
    if (have_flag(flgs, TR_SLAY_HUMAN))
    {
        info[i++] = "It is especially deadly against humans.";

    }

    if (have_flag(flgs, TR_FORCE_WEAPON))
    {
        info[i++] = "It powerfully strikes at a monster using your mana.";

    }
    if (have_flag(flgs, TR_DEC_MANA))
    {
        info[i++] = "It decreases your mana consumption.";

    }
    if (have_flag(flgs, TR_SPELL_POWER))
    {
        info[i++] = "It increases your spell power.";
    }
    if (have_flag(flgs, TR_SPELL_CAP))
    {
        info[i++] = "It increases your spell capacity.";
    }
    if (have_flag(flgs, TR_LIFE))
        info[i++] = "It affects your hitpoints.";
    if (have_flag(flgs, TR_SUST_STR))
    {
        info[i++] = "It sustains your strength.";

    }
    if (have_flag(flgs, TR_SUST_INT))
    {
        info[i++] = "It sustains your intelligence.";

    }
    if (have_flag(flgs, TR_SUST_WIS))
    {
        info[i++] = "It sustains your wisdom.";

    }
    if (have_flag(flgs, TR_SUST_DEX))
    {
        info[i++] = "It sustains your dexterity.";

    }
    if (have_flag(flgs, TR_SUST_CON))
    {
        info[i++] = "It sustains your constitution.";

    }
    if (have_flag(flgs, TR_SUST_CHR))
    {
        info[i++] = "It sustains your charisma.";

    }

    if (have_flag(flgs, TR_IM_ACID))
    {
        info[i++] = "It provides immunity to acid.";

    }
    if (have_flag(flgs, TR_IM_ELEC))
    {
        info[i++] = "It provides immunity to electricity.";

    }
    if (have_flag(flgs, TR_IM_FIRE))
    {
        info[i++] = "It provides immunity to fire.";

    }
    if (have_flag(flgs, TR_IM_COLD))
    {
        info[i++] = "It provides immunity to cold.";

    }

    if (have_flag(flgs, TR_THROW))
    {
        info[i++] = "It is perfectly balanced for throwing.";
    }

    if (have_flag(flgs, TR_FREE_ACT))
    {
        info[i++] = "It provides immunity to paralysis.";

    }
    if (have_flag(flgs, TR_HOLD_LIFE))
    {
        info[i++] = "It provides resistance to life draining.";

    }
    if (have_flag(flgs, TR_RES_FEAR))
    {
        info[i++] = "It provides resistance to fear.";

    }
    if (have_flag(flgs, TR_RES_ACID))
    {
        info[i++] = "It provides resistance to acid.";

    }
    if (have_flag(flgs, TR_RES_ELEC))
    {
        info[i++] = "It provides resistance to electricity.";

    }
    if (have_flag(flgs, TR_RES_FIRE))
    {
        info[i++] = "It provides resistance to fire.";

    }
    if (have_flag(flgs, TR_RES_COLD))
    {
        info[i++] = "It provides resistance to cold.";

    }
    if (have_flag(flgs, TR_RES_POIS))
    {
        info[i++] = "It provides resistance to poison.";

    }

    if (have_flag(flgs, TR_RES_LITE))
    {
        info[i++] = "It provides resistance to light.";

    }
    if (have_flag(flgs, TR_RES_DARK))
    {
        info[i++] = "It provides resistance to dark.";

    }

    if (have_flag(flgs, TR_RES_BLIND))
    {
        info[i++] = "It provides resistance to blindness.";

    }
    if (have_flag(flgs, TR_RES_CONF))
    {
        info[i++] = "It provides resistance to confusion.";

    }
    if (have_flag(flgs, TR_RES_SOUND))
    {
        info[i++] = "It provides resistance to sound.";

    }
    if (have_flag(flgs, TR_RES_SHARDS))
    {
        info[i++] = "It provides resistance to shards.";

    }

    if (have_flag(flgs, TR_RES_NETHER))
    {
        info[i++] = "It provides resistance to nether.";

    }
    if (have_flag(flgs, TR_RES_TIME))
    {
        info[i++] = "It provides resistance to time.";

    }
    if (have_flag(flgs, TR_RES_NEXUS))
    {
        info[i++] = "It provides resistance to nexus.";

    }
    if (have_flag(flgs, TR_RES_CHAOS))
    {
        info[i++] = "It provides resistance to chaos.";

    }
    if (have_flag(flgs, TR_RES_DISEN))
    {
        info[i++] = "It provides resistance to disenchantment.";

    }

    if (have_flag(flgs, TR_LEVITATION))
    {
        info[i++] = "It allows you to levitate.";

    }
    if (have_flag(flgs, TR_LITE))
    {
        if ((o_ptr->name2 == EGO_DARK) || (o_ptr->name1 == ART_NIGHT))
            info[i++] = "It decreases radius of your light source by 1.";
        else
            info[i++] = "It provides permanent light. (radius +1)";

    }
    if (have_flag(flgs, TR_SEE_INVIS))
    {
        info[i++] = "It allows you to see invisible monsters.";

    }
    if (have_flag(flgs, TR_TELEPATHY))
    {
        info[i++] = "It gives telepathic powers.";

    }
    if (have_flag(flgs, TR_ESP_ANIMAL))
    {
        info[i++] = "It senses natural creatures.";

    }
    if (have_flag(flgs, TR_ESP_UNDEAD))
    {
        info[i++] = "It senses undead.";

    }
    if (have_flag(flgs, TR_ESP_DEMON))
    {
        info[i++] = "It senses demons.";

    }
    if (have_flag(flgs, TR_ESP_ORC))
    {
        info[i++] = "It senses orcs.";

    }
    if (have_flag(flgs, TR_ESP_TROLL))
    {
        info[i++] = "It senses trolls.";

    }
    if (have_flag(flgs, TR_ESP_GIANT))
    {
        info[i++] = "It senses giants.";

    }
    if (have_flag(flgs, TR_ESP_DRAGON))
    {
        info[i++] = "It senses dragons.";

    }
    if (have_flag(flgs, TR_ESP_HUMAN))
    {
        info[i++] = "It senses humans.";

    }
    if (have_flag(flgs, TR_ESP_EVIL))
    {
        info[i++] = "It senses evil creatures.";

    }
    if (have_flag(flgs, TR_ESP_GOOD))
    {
        info[i++] = "It senses good creatures.";

    }
    if (have_flag(flgs, TR_ESP_NONLIVING))
    {
        info[i++] = "It senses non-living creatures.";

    }
    if (have_flag(flgs, TR_ESP_UNIQUE))
    {
        info[i++] = "It senses unique monsters.";

    }
    if (have_flag(flgs, TR_SLOW_DIGEST))
    {
        info[i++] = "It slows your metabolism.";

    }
    if (have_flag(flgs, TR_REGEN))
    {
        info[i++] = "It speeds your regenerative powers.";

    }
    if (have_flag(flgs, TR_WARNING))
    {
        info[i++] = "It warns you of danger";

    }
    if (have_flag(flgs, TR_REFLECT))
    {
        info[i++] = "It reflects bolts and arrows.";

    }
    if (have_flag(flgs, TR_SH_FIRE))
    {
        info[i++] = "It produces a fiery sheath.";
    }
    if (have_flag(flgs, TR_SH_SHARDS))
    {
        info[i++] = "It produces a shard aura.";
    }

    if (have_flag(flgs, TR_SH_ELEC))
    {
        info[i++] = "It produces an electric sheath.";

    }
    if (have_flag(flgs, TR_SH_COLD))
    {
        info[i++] = "It produces a sheath of coldness.";

    }
    if (have_flag(flgs, TR_NO_MAGIC))
    {
        info[i++] = "It produces an anti-magic shell.";

    }
    if (have_flag(flgs, TR_NO_SUMMON))
        info[i++] = "It disrupts summoning spells.";
    if (have_flag(flgs, TR_NO_TELE))
    {
        info[i++] = "It prevents teleportation.";

    }
    if (have_flag(flgs, TR_XTRA_MIGHT))
    {
        info[i++] = "It fires missiles with extra might.";

    }
    if (have_flag(flgs, TR_XTRA_SHOTS))
    {
        info[i++] = "It fires missiles excessively fast.";

    }

    if (have_flag(flgs, TR_BLESSED))
    {
        info[i++] = "It has been blessed by the gods.";

    }

    if (object_is_cursed(o_ptr))
    {
        if (o_ptr->curse_flags & TRC_PERMA_CURSE)
        {
            info[i++] = "It is permanently cursed.";

        }
        else if (o_ptr->curse_flags & TRC_HEAVY_CURSE)
        {
            info[i++] = "It is heavily cursed.";

        }
        else
        {
            info[i++] = "It is cursed.";

            /*
             * It's a trivial infomation since there is
             * fake inscription {cursed}
             */
            trivial_info++;
        }
    }

    if ((have_flag(flgs, TR_TY_CURSE)) || (o_ptr->curse_flags & TRC_TY_CURSE))
    {
        info[i++] = "It carries an ancient foul curse.";

    }
    if ((have_flag(flgs, TR_AGGRAVATE)) || (o_ptr->curse_flags & TRC_AGGRAVATE))
    {
        info[i++] = "It aggravates nearby creatures.";

    }
    if ((have_flag(flgs, TR_DRAIN_EXP)) || (o_ptr->curse_flags & TRC_DRAIN_EXP))
    {
        info[i++] = "It drains experience.";

    }
    if (o_ptr->curse_flags & TRC_SLOW_REGEN)
    {
        info[i++] = "It slows your regenerative powers.";

    }
    if (o_ptr->curse_flags & TRC_ADD_L_CURSE)
    {
        info[i++] = "It adds weak curses.";

    }
    if (o_ptr->curse_flags & TRC_ADD_H_CURSE)
    {
        info[i++] = "It adds heavy curses.";

    }
    if (o_ptr->curse_flags & TRC_CALL_ANIMAL)
    {
        info[i++] = "It attracts animals.";

    }
    if (o_ptr->curse_flags & TRC_CALL_DEMON)
    {
        info[i++] = "It attracts demons.";

    }
    if (o_ptr->curse_flags & TRC_CALL_DRAGON)
    {
        info[i++] = "It attracts dragons.";

    }
    if (o_ptr->curse_flags & TRC_COWARDICE)
    {
        info[i++] = "It makes you subject to cowardice.";

    }
    if ((have_flag(flgs, TR_TELEPORT)) || (o_ptr->curse_flags & TRC_TELEPORT))
    {
        info[i++] = "It induces random teleportation.";

    }
    if (o_ptr->curse_flags & TRC_LOW_MELEE)
    {
        info[i++] = "It causes you to miss blows.";

    }
    if (o_ptr->curse_flags & TRC_LOW_AC)
    {
        info[i++] = "It helps your enemies' blows.";

    }
    if (o_ptr->curse_flags & TRC_LOW_MAGIC)
    {
        info[i++] = "It encumbers you while spellcasting.";

    }
    if (o_ptr->curse_flags & TRC_FAST_DIGEST)
    {
        info[i++] = "It speeds your metabolism.";

    }
    if (o_ptr->curse_flags & TRC_DRAIN_HP)
    {
        info[i++] = "It drains you.";

    }
    if (o_ptr->curse_flags & TRC_DRAIN_MANA)
    {
        info[i++] = "It drains your mana.";

    }

    /* Describe about this kind of object instead of THIS fake object */
    if (mode & SCROBJ_FAKE_OBJECT)
    {
        switch (o_ptr->tval)
        {
        case TV_RING:
            switch (o_ptr->sval)
            {
            case SV_RING_LORDLY:
                info[i++] = "It provides some random resistances.";
                break;
            case SV_RING_WARNING:
                info[i++] = "It may provide a low rank ESP.";
                break;
            }
            break;

        case TV_AMULET:
            switch (o_ptr->sval)
            {
            case SV_AMULET_RESISTANCE:
                info[i++] = "It may provides resistance to poison.";
                info[i++] = "It may provide a random resistances.";
                break;
            case SV_AMULET_THE_MAGI:
                info[i++] = "It provides up to three low rank ESPs.";
                break;
            }
            break;
        }
    }

    if (have_flag(flgs, TR_IGNORE_ACID) &&
        have_flag(flgs, TR_IGNORE_ELEC) &&
        have_flag(flgs, TR_IGNORE_FIRE) &&
        have_flag(flgs, TR_IGNORE_COLD))
    {
        info[i++] = "It cannot be harmed by the elements.";
    }
    else
    {
        if (have_flag(flgs, TR_IGNORE_ACID))
        {
            info[i++] = "It cannot be harmed by acid.";
        }
        if (have_flag(flgs, TR_IGNORE_ELEC))
        {
            info[i++] = "It cannot be harmed by electricity.";
        }
        if (have_flag(flgs, TR_IGNORE_FIRE))
        {
            info[i++] = "It cannot be harmed by fire.";
        }
        if (have_flag(flgs, TR_IGNORE_COLD))
        {
            info[i++] = "It cannot be harmed by cold.";
        }
    }

    if (o_ptr->name3)
    {
        sprintf(replacement_name, "It reminds you of the artifact %s.", a_name + a_info[o_ptr->name3].name);
        info[i++] = replacement_name;
    }

    if (!(o_ptr->ident & IDENT_MENTAL))
    {
        if (i) info[i++] = "";
        info[i++] = "This object may have additional powers.";
    }

    if (mode & SCROBJ_FORCE_DETAIL) trivial_info = 0;

    /* No relevant informations */
    if (i <= trivial_info) return (FALSE);

    /* Save the screen */
    screen_save();

    /* Get size */
    Term_get_size(&wid, &hgt);

    /* Display Item name */
    if (!(mode & SCROBJ_FAKE_OBJECT))
        object_desc(o_name, o_ptr, 0);
    else
        object_desc(o_name, o_ptr, (OD_NAME_ONLY | OD_STORE));

    prt(o_name, 0, 0);

    /* Erase the screen */
    for (k = 1; k < hgt; k++) prt("", k, 13);

    /* Label the information */
    if ((o_ptr->tval == TV_STATUE) && (o_ptr->sval == SV_PHOTO))
    {
        monster_race *r_ptr = &r_info[o_ptr->pval];
        int namelen = strlen(r_name + r_ptr->name);
        prt(format("%s: '", r_name + r_ptr->name), 1, 15);
        Term_queue_bigchar(18 + namelen, 1, r_ptr->x_attr, r_ptr->x_char, 0, 0);
        prt("'", 1, (use_bigtile ? 20 : 19) + namelen);
    }
    else
    prt("     Item Attributes:", 1, 15);

    /* We will print on top of the map (column 13) */
    for (k = 2, j = 0; j < i; j++)
    {
        /* Show the info */
        prt(info[j], k++, 15);

        /* Every 20 entries (lines 2 to 21), start over */
        if ((k == hgt - 2) && (j+1 < i))
        {
            prt("-- more --", k, 15);
            inkey();
            for (; k > 2; k--) prt("", k, 15);
        }
    }

    /* Wait for it */
    prt("[Press any key to continue]", k, 15);

    inkey();

    /* Restore the screen */
    screen_load();

    /* Gave knowledge */
    return (TRUE);
}



/*
 * Convert an inventory index into a one character label
 * Note that the label does NOT distinguish inven/equip.
 */
char index_to_label(int i)
{
    /* Indexes for "inven" are easy */
    if (i <= INVEN_PACK) return (I2A(i));

    /* Indexes for "equip" are offset */
    return (I2A(i - EQUIP_BEGIN));
}


/*
 * Convert a label into the index of an item in the "inven"
 * Return "-1" if the label does not indicate a real item
 */
s16b label_to_inven(int c)
{
    int i;

    /* Convert */
    i = (islower(c) ? A2I(c) : -1);

    /* Verify the index */
    if ((i < 0) || (i > INVEN_PACK)) return (-1);

    /* Empty slots can never be chosen */
    if (!inventory[i].k_idx) return (-1);

    /* Return the index */
    return (i);
}


/*
 * Convert a label into the index of a item in the "equip"
 * Return "-1" if the label does not indicate a real item
 */
s16b label_to_equip(int c)
{
    int i = (islower(c) ? A2I(c) : -1) + EQUIP_BEGIN;
    if (!equip_is_valid_slot(i)) return -1;
    if (!equip_obj(i)) return -1;
    return i;
}

/*
 * Return a string describing how a given item is being worn.
 * Currently, only used for items in the equipment, not inventory.
 */
cptr describe_use(int i)
{
    if (equip_is_valid_slot(i))
        return "wearing";
    return "carrying in your pack;";
}


/* Hack: Check if a spellbook is one of the realms we can use. -- TY */

bool check_book_realm(const byte book_tval, const byte book_sval)
{
    if (book_tval < TV_LIFE_BOOK) return FALSE;
    if (p_ptr->pclass == CLASS_SORCERER)
    {
        return is_magic(tval2realm(book_tval));
    }
    else if (p_ptr->pclass == CLASS_RED_MAGE)
    {
        if (is_magic(tval2realm(book_tval)))
            return ((book_tval == TV_ARCANE_BOOK) || (book_sval < 2));
    }
    return (REALM1_BOOK == book_tval || REALM2_BOOK == book_tval);
}


/*
 * Check an item against the item tester info
 */
bool item_tester_okay(object_type *o_ptr)
{
    /* Hack -- allow listing empty slots */
    if (item_tester_full) return (TRUE);

    if (!o_ptr) return FALSE;

    /* Require an item */
    if (!o_ptr->k_idx) return (FALSE);

    /* Hack -- ignore "gold" */
    if (o_ptr->tval == TV_GOLD)
    {
        /* See xtra2.c */
        extern bool show_gold_on_floor;

        if (!show_gold_on_floor) return (FALSE);
    }

    /* Check the tval */
    if (item_tester_tval)
    {
        /* Is it a spellbook? If so, we need a hack -- TY */
        if ((item_tester_tval <= TV_DEATH_BOOK) &&
            (item_tester_tval >= TV_LIFE_BOOK))
            return check_book_realm(o_ptr->tval, o_ptr->sval);
        else
            if (item_tester_tval != o_ptr->tval) return (FALSE);
    }

    /* Check the hook */
    if (item_tester_hook)
    {
        if (!(*item_tester_hook)(o_ptr)) return (FALSE);
    }

    /* Assume okay */
    return (TRUE);
}




/*
 * Choice window "shadow" of the "show_inven()" function
 */
void display_inven(void)
{
    register        int i, n, z = 0;
    object_type     *o_ptr;
    byte            attr = TERM_WHITE;
    char            tmp_val[80];
    char            o_name[MAX_NLEN];
    int             wid, hgt;

    /* Get size */
    Term_get_size(&wid, &hgt);

    /* Find the "final" slot */
    for (i = 0; i < INVEN_PACK; i++)
    {
        o_ptr = &inventory[i];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Track */
        z = i + 1;
    }

    /* Display the pack */
    for (i = 0; i < z; i++)
    {
        /* Examine the item */
        o_ptr = &inventory[i];

        /* Start with an empty "index" */
        tmp_val[0] = tmp_val[1] = tmp_val[2] = ' ';

        /* Is this item "acceptable"? */
        if (item_tester_okay(o_ptr))
        {
            /* Prepare an "index" */
            tmp_val[0] = index_to_label(i);

            /* Bracket the "index" --(-- */
            tmp_val[1] = ')';
        }

        /* Display the index (or blank space) */
        Term_putstr(0, i, 3, TERM_WHITE, tmp_val);

        /* Obtain an item description */
        object_desc(o_name, o_ptr, 0);

        /* Obtain the length of the description */
        n = strlen(o_name);

        /* Get a color */
        attr = tval_to_attr[o_ptr->tval % 128];

        /* Grey out charging items */
        if (o_ptr->timeout)
        {
            attr = TERM_L_DARK;
        }

        /* Display the entry itself */
        Term_putstr(3, i, n, attr, o_name);

        /* Erase the rest of the line */
        Term_erase(3+n, i, 255);

        /* Display the weight if needed */
        if (show_weights)
        {
            int wgt = o_ptr->weight * o_ptr->number;
            sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);

            prt(tmp_val, i, wid - 9);
        }
    }

    /* Erase the rest of the window */
    for (i = z; i < hgt; i++)
    {
        /* Erase the line */
        Term_erase(0, i, 255);
    }
}



/*
 * Choice window "shadow" of the "show_equip()" function
 */
void display_equip(void)
{
    int             i, n, r = 0;
    object_type    *o_ptr;
    byte            attr = TERM_WHITE;
    char            tmp_val[80];
    char            o_name[MAX_NLEN];
    int             wid, hgt;

    /* Get size */
    Term_get_size(&wid, &hgt);

    /* Display the equipment */
    for (i = EQUIP_BEGIN; i < EQUIP_BEGIN + equip_count(); i++, r++)
    {
        o_ptr = equip_obj(i);

        /* Start with an empty "index" */
        tmp_val[0] = tmp_val[1] = tmp_val[2] = ' ';

        /* Is this item "acceptable"? */
        if (item_tester_okay(o_ptr))
        {
            /* Prepare an "index" */
            tmp_val[0] = index_to_label(i);

            /* Bracket the "index" --(-- */
            tmp_val[1] = ')';
        }

        /* Display the index (or blank space) */
        Term_putstr(0, r, 3, TERM_WHITE, tmp_val);

        if (o_ptr)
        {
            object_desc(o_name, o_ptr, 0);
            attr = tval_to_attr[o_ptr->tval % 128];
            if (o_ptr->timeout)
                attr = TERM_L_DARK;
        }
        else
            sprintf(o_name, "");

        n = strlen(o_name);
        Term_putstr(3, r, n, attr, o_name);
        Term_erase(3+n, r, 255);

        if (show_weights && o_ptr)
        {
            int wgt = o_ptr->weight * o_ptr->number;
            sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
            prt(tmp_val, r, wid - (show_labels ? 28 : 9));
        }
        if (show_labels && o_ptr)
        {
            Term_putstr(wid - 20, r, -1, TERM_WHITE, " <-- ");
            prt(equip_describe_slot(i), r, wid - 15);
        }
    }

    /* Erase the rest of the window */
    for (; r < hgt; r++)
        Term_erase(0, r, 255);
}


/*
 * Find the "first" inventory object with the given "tag".
 *
 * A "tag" is a numeral "n" appearing as "@n" anywhere in the
 * inscription of an object.  Alphabetical characters don't work as a
 * tag in this form.
 *
 * Also, the tag "@xn" will work as well, where "n" is a any tag-char,
 * and "x" is the "current" command_cmd code.
 */
static bool get_tag(int *cp, char tag, int mode)
{
    int i, start, end;
    cptr s;

    /* Extract index from mode */
    switch (mode)
    {
    case USE_EQUIP:
        start = EQUIP_BEGIN;
        end = EQUIP_BEGIN + equip_count() - 1;
        break;

    case USE_INVEN:
        start = 0;
        end = INVEN_PACK - 1;
        break;

    default:
        return FALSE;
    }

    /**** Find a tag in the form of {@x#} (allow alphabet tag) ***/

    /* Check every inventory object */
    for (i = start; i <= end; i++)
    {
        object_type *o_ptr = &inventory[i];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Skip empty inscriptions */
        if (!o_ptr->inscription) continue;

        /* Skip non-choice */
        if (!item_tester_okay(o_ptr)) continue;

        /* Find a '@' */
        s = my_strchr(quark_str(o_ptr->inscription), '@');

        /* Process all tags */
        while (s)
        {
            /* Check the special tags */
            if ((s[1] == command_cmd) && (s[2] == tag))
            {
                /* Save the actual inventory ID */
                *cp = i;

                /* Success */
                return (TRUE);
            }

            /* Find another '@' */
            s = my_strchr(s + 1, '@');
        }
    }


    /**** Find a tag in the form of {@#} (allows only numerals)  ***/

    /* Don't allow {@#} with '#' being alphabet */
    if (tag < '0' || '9' < tag)
    {
        /* No such tag */
        return FALSE;
    }

    /* Check every object */
    for (i = start; i <= end; i++)
    {
        object_type *o_ptr = &inventory[i];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Skip empty inscriptions */
        if (!o_ptr->inscription) continue;

        /* Skip non-choice */
        if (!item_tester_okay(o_ptr)) continue;

        /* Find a '@' */
        s = my_strchr(quark_str(o_ptr->inscription), '@');

        /* Process all tags */
        while (s)
        {
            /* Check the normal tags */
            if (s[1] == tag)
            {
                /* Save the actual inventory ID */
                *cp = i;

                /* Success */
                return (TRUE);
            }

            /* Find another '@' */
            s = my_strchr(s + 1, '@');
        }
    }

    /* No such tag */
    return (FALSE);
}


/*
 * Find the "first" floor object with the given "tag".
 *
 * A "tag" is a numeral "n" appearing as "@n" anywhere in the
 * inscription of an object.  Alphabetical characters don't work as a
 * tag in this form.
 *
 * Also, the tag "@xn" will work as well, where "n" is a any tag-char,
 * and "x" is the "current" command_cmd code.
 */
static bool get_tag_floor(int *cp, char tag, int floor_list[], int floor_num)
{
    int i;
    cptr s;

    /**** Find a tag in the form of {@x#} (allow alphabet tag) ***/

    /* Check every object in the grid */
    for (i = 0; i < floor_num && i < 23; i++)
    {
        object_type *o_ptr = &o_list[floor_list[i]];

        /* Skip empty inscriptions */
        if (!o_ptr->inscription) continue;

        /* Find a '@' */
        s = my_strchr(quark_str(o_ptr->inscription), '@');

        /* Process all tags */
        while (s)
        {
            /* Check the special tags */
            if ((s[1] == command_cmd) && (s[2] == tag))
            {
                /* Save the actual floor object ID */
                *cp = i;

                /* Success */
                return (TRUE);
            }

            /* Find another '@' */
            s = my_strchr(s + 1, '@');
        }
    }


    /**** Find a tag in the form of {@#} (allows only numerals)  ***/

    /* Don't allow {@#} with '#' being alphabet */
    if (tag < '0' || '9' < tag)
    {
        /* No such tag */
        return FALSE;
    }

    /* Check every object in the grid */
    for (i = 0; i < floor_num && i < 23; i++)
    {
        object_type *o_ptr = &o_list[floor_list[i]];

        /* Skip empty inscriptions */
        if (!o_ptr->inscription) continue;

        /* Find a '@' */
        s = my_strchr(quark_str(o_ptr->inscription), '@');

        /* Process all tags */
        while (s)
        {
            /* Check the normal tags */
            if (s[1] == tag)
            {
                /* Save the floor object ID */
                *cp = i;

                /* Success */
                return (TRUE);
            }

            /* Find another '@' */
            s = my_strchr(s + 1, '@');
        }
    }

    /* No such tag */
    return (FALSE);
}


/*
 * Move around label characters with correspond tags
 */
static void prepare_label_string(char *label, int mode)
{
    cptr alphabet_chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    int  offset = (mode == USE_EQUIP) ? EQUIP_BEGIN : 0;
    int  i;

    /* Prepare normal labels */
    strcpy(label, alphabet_chars);

    /* Move each label */
    for (i = 0; i < 52; i++)
    {
        int index;
        char c = alphabet_chars[i];

        /* Find a tag with this label */
        if (get_tag(&index, c, mode))
        {
            /* Delete the overwritten label */
            if (label[i] == c) label[i] = ' ';

            /* Move the label to the place of corresponding tag */
            label[index - offset] = c;
        }
    }
}


/*
 * Move around label characters with correspond tags (floor version)
 */
static void prepare_label_string_floor(char *label, int floor_list[], int floor_num)
{
    cptr alphabet_chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
    int  i;

    /* Prepare normal labels */
    strcpy(label, alphabet_chars);

    /* Move each label */
    for (i = 0; i < 52; i++)
    {
        int index;
        char c = alphabet_chars[i];

        /* Find a tag with this label */
        if (get_tag_floor(&index, c, floor_list, floor_num))
        {
            /* Delete the overwritten label */
            if (label[i] == c) label[i] = ' ';

            /* Move the label to the place of corresponding tag */
            label[index] = c;
        }
    }
}


/*
 * Display the inventory.
 *
 * Hack -- do not display "trailing" empty slots
 */
int show_inven(int target_item, int mode)
{
    int             i, j, k, l, z = 0;
    int             col, cur_col, len;
    object_type     *o_ptr;
    char            o_name[MAX_NLEN];
    char            tmp_val[80];
    int             out_index[23];
    byte            out_color[23];
    char            out_desc[23][MAX_NLEN];
    int             target_item_label = 0;
    int             wid, hgt;
    char            inven_label[52 + 1];

    /* Starting column */
    col = command_gap;

    /* Get size */
    Term_get_size(&wid, &hgt);

    /* Default "max-length" */
    len = wid - col - 1;


    /* Find the "final" slot */
    for (i = 0; i < INVEN_PACK; i++)
    {
        o_ptr = &inventory[i];

        /* Skip non-objects */
        if (!o_ptr->k_idx) continue;

        /* Track */
        z = i + 1;
    }

    prepare_label_string(inven_label, USE_INVEN);

    /* Display the inventory */
    for (k = 0, i = 0; i < z; i++)
    {
        o_ptr = &inventory[i];

        /* Is this item acceptable? */
        if (!item_tester_okay(o_ptr)) continue;

        /* Describe the object */
        object_desc(o_name, o_ptr, 0);

        /* Save the object index, color, and description */
        out_index[k] = i;
        out_color[k] = tval_to_attr[o_ptr->tval % 128];

        /* Grey out charging items */
        if (o_ptr->timeout)
        {
            bool darken = TRUE;

            if (o_ptr->tval == TV_ROD && o_ptr->number > 1)
            {
                int power;
                object_kind *k_ptr = &k_info[o_ptr->k_idx];

                darken = FALSE;
                if (k_ptr->pval == 0) k_ptr->pval = 1;
                power = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;
                if (power >= o_ptr->number)
                    darken = TRUE;
            }

            if (darken)
                out_color[k] = TERM_L_DARK;
        }

        (void)strcpy(out_desc[k], o_name);

        /* Find the predicted "line length" */
        l = strlen(out_desc[k]) + 5;

        /* Be sure to account for the weight */
        if (mode & SHOW_FAIL_RATES) l += 12;
        else if (mode & SHOW_VALUE) l += 12;
        else if (show_weights) l += 9;

        /* Account for icon if displayed */
        if (show_item_graph)
        {
            l += 2;
            if (use_bigtile) l++;
        }

        /* Maintain the maximum length */
        if (l > len) len = l;

        /* Advance to next "line" */
        k++;
    }

    /* Find the column to start in */
    col = (len > wid - 4) ? 0 : (wid - len - 1);

    /* Output each entry */
    for (j = 0; j < k; j++)
    {
        /* Get the index */
        i = out_index[j];

        /* Get the item */
        o_ptr = &inventory[i];

        /* Clear the line */
        prt("", j + 1, col ? col - 2 : col);

        if (use_menu && target_item)
        {
            if (j == (target_item-1))
            {
                strcpy(tmp_val, "> ");
                target_item_label = i;
            }
            else strcpy(tmp_val, "  ");
        }
        else if (i <= INVEN_PACK)
        {
            /* Prepare an index --(-- */
            sprintf(tmp_val, "%c)", inven_label[i]);
        }
        else
        {
            /* Prepare an index --(-- */
            sprintf(tmp_val, "%c)", index_to_label(i));
        }

        /* Clear the line with the (possibly indented) index */
        put_str(tmp_val, j + 1, col);

        cur_col = col + 3;

        /* Display graphics for object, if desired */
        if (show_item_graph)
        {
            byte  a = object_attr(o_ptr);
            char c = object_char(o_ptr);

#ifdef AMIGA
            if (a & 0x80) a |= 0x40;
#endif

            Term_queue_bigchar(cur_col, j + 1, a, c, 0, 0);
            if (use_bigtile) cur_col++;

            cur_col += 2;
        }


        /* Display the entry itself */
        c_put_str(out_color[j], out_desc[j], j + 1, cur_col);

        /* Display the weight if needed */
        if (mode & SHOW_FAIL_RATES)
        {
            int fail = device_calc_fail_rate(o_ptr);
            sprintf(tmp_val, "Fail: %2d.%d%%", fail/10, fail%10);
            prt(tmp_val, j + 1, wid - 12);
        }
        else if (mode & SHOW_VALUE)
        {
            int value = object_value_real(o_ptr);
            sprintf(tmp_val, "Pow: %7d", value);
            prt(tmp_val, j + 1, wid - 12);
        }
        else if (show_weights)
        {
            int wgt = o_ptr->weight * o_ptr->number;
            (void)sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);
            prt(tmp_val, j + 1, wid - 9);
        }
    }

    /* Make a "shadow" below the list (only if needed) */
    if (j && (j < 23)) prt("", j + 1, col ? col - 2 : col);

    /* Save the new column */
    /*command_gap = col;*/

    return target_item_label;
}



/*
 * Display the equipment.
 */
int show_equip(int target_item, int mode)
{
    int             i, j, k, l;
    int             col, cur_col, len;
    object_type     *o_ptr;
    char            tmp_val[80];
    int             out_index[23];
    byte            out_color[23];
    char            out_desc[23][MAX_NLEN];
    int             target_item_label = 0;
    int             wid, hgt;
    char            equip_label[52 + 1];

    /* Starting column */
    col = command_gap;

    /* Get size */
    Term_get_size(&wid, &hgt);

    /* Maximal length */
    len = wid - col - 1;

    /* Scan the equipment list */
    for (k = 0, i = EQUIP_BEGIN; i < EQUIP_BEGIN + equip_count(); i++)
    {
        o_ptr = equip_obj(i);

        if (!item_tester_okay(o_ptr)) continue; /* NULL is OK ... */
        if (equip_is_empty_two_handed_slot(i)) continue;

        if (o_ptr)
        {
            object_desc(out_desc[k], o_ptr, 0);
            if (o_ptr->timeout)
                out_color[k] = TERM_L_DARK;
            else
                out_color[k] = tval_to_attr[o_ptr->tval % 128];
        }
        else
            sprintf(out_desc[k], "");

        out_index[k] = i;

        /* Extract the maximal length (see below) */
        l = strlen(out_desc[k]) + (2 + 3);
        if (show_labels) l += (10 + 2);
        if (mode & SHOW_FAIL_RATES) l += 12;
        else if (mode & SHOW_VALUE) l += 12;
        else if (show_weights) l += 9;
        if (show_item_graph) l += 2;
        if (l > len) len = l;

        k++;
    }

    /* Hack -- Find a column to start in */
    col = (len > wid - 4) ? 0 : (wid - len - 1);

    prepare_label_string(equip_label, USE_EQUIP);

    /* Output each entry */
    for (j = 0; j < k; j++)
    {
        i = out_index[j];
        o_ptr = equip_obj(i);

        prt("", j + 1, col ? col - 2 : col);

        if (use_menu && target_item)
        {
            if (j == (target_item-1))
            {
                strcpy(tmp_val, "> ");
                target_item_label = i;
            }
            else strcpy(tmp_val, "  ");
        }
        else
        {
            sprintf(tmp_val, "%c)", equip_label[i - EQUIP_BEGIN]);
        }

        /* Clear the line with the (possibly indented) index */
        put_str(tmp_val, j+1, col);

        cur_col = col + 3;

        /* Display graphics for object, if desired */
        if (show_item_graph)
        {
            if (o_ptr)
            {
                byte a = object_attr(o_ptr);
                char c = object_char(o_ptr);

                Term_queue_bigchar(cur_col, j + 1, a, c, 0, 0);
                if (use_bigtile) cur_col++;
            }
            else
                put_str(" ", j+1, cur_col);

            cur_col += 2;
        }

        if (show_labels)
        {
            (void)sprintf(tmp_val, "%-10.10s: ", equip_describe_slot(i));
            put_str(tmp_val, j+1, cur_col);
            c_put_str(out_color[j], out_desc[j], j+1, cur_col + 12);
        }
        else
        {
            c_put_str(out_color[j], out_desc[j], j+1, cur_col);
        }

        if (!o_ptr) continue;
        if (mode & SHOW_FAIL_RATES)
        {
            int fail = activation_fail_rate(o_ptr);
            sprintf(tmp_val, "Fail: %2d.%d%%", fail/10, fail%10);
            prt(tmp_val, j + 1, wid - 12);
        }
        else if (mode & SHOW_VALUE)
        {
            int value = object_value_real(o_ptr);
            sprintf(tmp_val, "Pow: %7d", value);
            prt(tmp_val, j + 1, wid - 12);
        }
        else if (show_weights)
        {
            int wgt = o_ptr->weight * o_ptr->number;
            (void)sprintf(tmp_val, "%3d.%d lb", wgt / 10, wgt % 10);
            prt(tmp_val, j + 1, wid - 9);
        }
    }

    /* Make a "shadow" below the list (only if needed) */
    if (j && (j < 23)) prt("", j + 1, col ? col - 2 : col);

    /* Save the new column */
    /*command_gap = col;*/

    return target_item_label;
}




/*
 * Flip "inven" and "equip" in any sub-windows
 */
void toggle_inven_equip(void)
{
    int j;

    /* Scan windows */
    for (j = 0; j < 8; j++)
    {
        /* Unused */
        if (!angband_term[j]) continue;

        /* Flip inven to equip */
        if (window_flag[j] & (PW_INVEN))
        {
            /* Flip flags */
            window_flag[j] &= ~(PW_INVEN);
            window_flag[j] |= (PW_EQUIP);

            /* Window stuff */
            p_ptr->window |= (PW_EQUIP);
        }

        /* Flip inven to equip */
        else if (window_flag[j] & (PW_EQUIP))
        {
            /* Flip flags */
            window_flag[j] &= ~(PW_EQUIP);
            window_flag[j] |= (PW_INVEN);

            /* Window stuff */
            p_ptr->window |= (PW_INVEN);
        }
    }
}



/*
 * Verify the choice of an item.
 *
 * The item can be negative to mean "item on floor".
 */
static bool verify(cptr prompt, int item)
{
    char        o_name[MAX_NLEN];
    char        out_val[MAX_NLEN+20];
    object_type *o_ptr;


    /* Inventory */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Floor */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* Describe */
    object_desc(o_name, o_ptr, 0);

    /* Prompt */
    (void)sprintf(out_val, "%s %s? ", prompt, o_name);


    /* Query */
    return (get_check(out_val));
}


/*
 * Hack -- allow user to "prevent" certain choices
 *
 * The item can be negative to mean "item on floor".
 */
static bool get_item_allow(int item)
{
    cptr s;

    object_type *o_ptr;

    if (!command_cmd) return TRUE; /* command_cmd is no longer effective */

    /* Inventory */
    if (item >= 0)
    {
        o_ptr = &inventory[item];
    }

    /* Floor */
    else
    {
        o_ptr = &o_list[0 - item];
    }

    /* No inscription */
    if (!o_ptr->inscription) return (TRUE);

    /* Find a '!' */
    s = my_strchr(quark_str(o_ptr->inscription), '!');

    /* Process preventions */
    while (s)
    {
        /* Check the "restriction" */
        if ((s[1] == command_cmd) || (s[1] == '*'))
        {
            /* Verify the choice */
            if (!verify("Really try", item)) return (FALSE);

        }

        /* Find another '!' */
        s = my_strchr(s + 1, '!');
    }

    /* Allow it */
    return (TRUE);
}



/*
 * Auxiliary function for "get_item()" -- test an index
 */
static bool get_item_okay(int i)
{
    /* Illegal items */
    if ((i < 0) || (i >= INVEN_TOTAL)) return (FALSE);

    /* Verify the item */
    if (!item_tester_okay(&inventory[i])) return (FALSE);

    /* Assume okay */
    return (TRUE);
}



/*
 * Determine whether get_item() can get some item or not
 * assuming mode = (USE_EQUIP | USE_INVEN | USE_FLOOR).
 */
bool can_get_item(void)
{
    int j, floor_list[23], floor_num = 0;

    for (j = 0; j < INVEN_TOTAL; j++)
        if (item_tester_okay(&inventory[j]))
            return TRUE;

    floor_num = scan_floor(floor_list, py, px, 0x03);
    if (floor_num)
        return TRUE;

    return FALSE;
}

/*
 * Let the user select an item, save its "index"
 *
 * Return TRUE only if an acceptable item was chosen by the user.
 *
 * The selected item must satisfy the "item_tester_hook()" function,
 * if that hook is set, and the "item_tester_tval", if that value is set.
 *
 * All "item_tester" restrictions are cleared before this function returns.
 *
 * The user is allowed to choose acceptable items from the equipment,
 * inventory, or floor, respectively, if the proper flag was given,
 * and there are any acceptable items in that location.
 *
 * The equipment or inventory are displayed (even if no acceptable
 * items are in that location) if the proper flag was given.
 *
 * If there are no acceptable items available anywhere, and "str" is
 * not NULL, then it will be used as the text of a warning message
 * before the function returns.
 *
 * Note that the user must press "-" to specify the item on the floor,
 * and there is no way to "examine" the item on the floor, while the
 * use of "capital" letters will "examine" an inventory/equipment item,
 * and prompt for its use.
 *
 * If a legal item is selected from the inventory, we save it in "cp"
 * directly (0 to 35), and return TRUE.
 *
 * If a legal item is selected from the floor, we save it in "cp" as
 * a negative (-1 to -511), and return TRUE.
 *
 * If no item is available, we do nothing to "cp", and we display a
 * warning message, using "str" if available, and return FALSE.
 *
 * If no item is selected, we do nothing to "cp", and return FALSE.
 *
 * Global "p_ptr->command_new" is used when viewing the inventory or equipment
 * to allow the user to enter a command while viewing those screens, and
 * also to induce "auto-enter" of stores, and other such stuff.
 *
 * Global "p_ptr->command_see" may be set before calling this function to start
 * out in "browse" mode.  It is cleared before this function returns.
 *
 * Global "p_ptr->command_wrk" is used to choose between equip/inven listings.
 * If it is TRUE then we are viewing inventory, else equipment.
 *
 * We always erase the prompt when we are done, leaving a blank line,
 * or a warning message, if appropriate, if no items are available.
 */
bool get_item(int *cp, cptr pmt, cptr str, int mode)
{
    s16b this_o_idx, next_o_idx = 0;

    char which = ' ';

    int j, k, i1, i2, e1, e2;

    bool done, item;

    bool oops = FALSE;

    bool equip = FALSE;
    bool inven = FALSE;
    bool floor = FALSE;
    bool quiver = FALSE;

    bool allow_floor = FALSE;

    bool toggle = FALSE;

    char tmp_val[160];
    char out_val[160];

    /* See cmd5.c */
    extern bool select_the_force;

    int menu_line = (use_menu ? 1 : 0);
    int max_inven = 0;
    int max_equip = 0;

#ifdef ALLOW_REPEAT

    static char prev_tag = '\0';
    char cur_tag = '\0';

#endif /* ALLOW_REPEAT */

#ifdef ALLOW_EASY_FLOOR /* TNB */

    if (easy_floor || use_menu) return get_item_floor(cp, pmt, str, mode);

#endif /* ALLOW_EASY_FLOOR -- TNB */

    /* Extract args */
    if (mode & USE_EQUIP) equip = TRUE;
    if (mode & USE_INVEN) inven = TRUE;
    if (mode & USE_FLOOR) floor = TRUE;
    if (mode & USE_QUIVER) quiver = TRUE;

#ifdef ALLOW_REPEAT

    /* Get the item index */
    if (repeat_pull(cp))
    {
        /* the_force */
        if (select_the_force && (*cp == INVEN_FORCE))
        {
            item_tester_tval = 0;
            item_tester_hook = NULL;
            command_cmd = 0; /* Hack -- command_cmd is no longer effective */
            return (TRUE);
        }

        else if (quiver && (*cp == INVEN_UNLIMITED_QUIVER))
        {
            item_tester_tval = 0;
            item_tester_hook = NULL;
            command_cmd = 0; /* Hack -- command_cmd is no longer effective */
            return (TRUE);            
        }

        /* Floor item? */
        else if (floor && (*cp < 0))
        {
            object_type *o_ptr;

            /* Special index */
            k = 0 - (*cp);

            /* Acquire object */
            o_ptr = &o_list[k];

            /* Validate the item */
            if (item_tester_okay(o_ptr))
            {
                /* Forget restrictions */
                item_tester_tval = 0;
                item_tester_hook = NULL;
                command_cmd = 0; /* Hack -- command_cmd is no longer effective */

                /* Success */
                return TRUE;
            }
        }

        else if ( (inven && (*cp >= 0) && (*cp < INVEN_PACK)) 
               || (equip && equip_is_valid_slot(*cp)) )
        {
            if (prev_tag && command_cmd)
            {
                /* Look up the tag and validate the item */
                if (!get_tag(&k, prev_tag, equip_is_valid_slot(*cp) ? USE_EQUIP : USE_INVEN)) /* Reject */;
                else if ((k < INVEN_PACK) ? !inven : !equip) /* Reject */;
                else if (!get_item_okay(k)) /* Reject */;
                else
                {
                    /* Accept that choice */
                    (*cp) = k;

                    /* Forget restrictions */
                    item_tester_tval = 0;
                    item_tester_hook = NULL;
                    command_cmd = 0; /* Hack -- command_cmd is no longer effective */

                    /* Success */
                    return TRUE;
                }

                prev_tag = '\0'; /* prev_tag is no longer effective */
            }

            /* Verify the item */
            else if (get_item_okay(*cp))
            {
                /* Forget restrictions */
                item_tester_tval = 0;
                item_tester_hook = NULL;
                command_cmd = 0; /* Hack -- command_cmd is no longer effective */

                /* Success */
                return TRUE;
            }
        }
    }

#endif /* ALLOW_REPEAT */


    /* Paranoia XXX XXX XXX */
    msg_print(NULL);


    /* Not done */
    done = FALSE;

    /* No item selected */
    item = FALSE;


    /* Full inventory */
    i1 = 0;
    i2 = INVEN_PACK - 1;

    /* Forbid inventory */
    if (!inven) i2 = -1;
    else if (use_menu)
    {
        for (j = 0; j < INVEN_PACK; j++)
            if (item_tester_okay(&inventory[j])) max_inven++;
    }

    /* Restrict inventory indexes */
    while ((i1 <= i2) && (!get_item_okay(i1))) i1++;
    while ((i1 <= i2) && (!get_item_okay(i2))) i2--;


    /* Full equipment */
    e1 = EQUIP_BEGIN;
    e2 = EQUIP_BEGIN + equip_count() - 1;

    /* Forbid equipment */
    if (!equip) e2 = -1;
    else if (use_menu)
    {
        for (j = EQUIP_BEGIN; j < EQUIP_BEGIN + equip_count(); j++)
            if (item_tester_okay(&inventory[j])) max_equip++;
    }

    /* Restrict equipment indexes */
    while ((e1 <= e2) && (!get_item_okay(e1))) e1++;
    while ((e1 <= e2) && (!get_item_okay(e2))) e2--;

    /* Restrict floor usage */
    if (floor)
    {
        /* Scan all objects in the grid */
        for (this_o_idx = cave[py][px].o_idx; this_o_idx; this_o_idx = next_o_idx)
        {
            object_type *o_ptr;

            /* Acquire object */
            o_ptr = &o_list[this_o_idx];

            /* Acquire next object */
            next_o_idx = o_ptr->next_o_idx;

            /* Accept the item on the floor if legal */
            if (item_tester_okay(o_ptr) && (o_ptr->marked & OM_FOUND)) allow_floor = TRUE;
        }
    }

    /* Require at least one legal choice */
    if (!allow_floor && (i1 > i2) && (e1 > e2))
    {
        /* Cancel p_ptr->command_see */
        command_see = FALSE;

        /* Oops */
        oops = TRUE;

        /* Done */
        done = TRUE;

        if (select_the_force) {
            *cp = INVEN_FORCE;
            item = TRUE;
        }
        if (quiver && p_ptr->unlimited_quiver) {
            *cp = INVEN_UNLIMITED_QUIVER;
            item = TRUE;
            oops = FALSE;
        }
    }

    /* Analyze choices */
    else
    {
        /* Hack -- Start on equipment if requested */
        if (command_see && command_wrk && equip)
        {
            command_wrk = TRUE;
        }

        /* Use inventory if allowed */
        else if (inven)
        {
            command_wrk = FALSE;
        }

        /* Use equipment if allowed */
        else if (equip)
        {
            command_wrk = TRUE;
        }

        /* Use inventory for floor */
        else
        {
            command_wrk = FALSE;
        }
    }


    /*
     * 追加オプション(always_show_list)が設定されている場合は常に一覧を表示する
     */
    if ((always_show_list == TRUE) || use_menu) command_see = TRUE;

    /* Hack -- start out in "display" mode */
    if (command_see)
    {
        /* Save screen */
        screen_save();
    }


    /* Repeat until done */
    while (!done)
    {
        int get_item_label = 0;

        /* Show choices */
        int ni = 0;
        int ne = 0;

        /* Scan windows */
        for (j = 0; j < 8; j++)
        {
            /* Unused */
            if (!angband_term[j]) continue;

            /* Count windows displaying inven */
            if (window_flag[j] & (PW_INVEN)) ni++;

            /* Count windows displaying equip */
            if (window_flag[j] & (PW_EQUIP)) ne++;
        }

        /* Toggle if needed */
        if ((command_wrk && ni && !ne) ||
            (!command_wrk && !ni && ne))
        {
            /* Toggle */
            toggle_inven_equip();

            /* Track toggles */
            toggle = !toggle;
        }

        /* Update */
        p_ptr->window |= (PW_INVEN | PW_EQUIP);

        /* Redraw windows */
        window_stuff();


        /* Inventory screen */
        if (!command_wrk)
        {
            /* Redraw if needed */
            if (command_see) get_item_label = show_inven(menu_line, mode);
        }

        /* Equipment screen */
        else
        {
            /* Redraw if needed */
            if (command_see) get_item_label = show_equip(menu_line, mode);
        }

        /* Viewing inventory */
        if (!command_wrk)
        {
            /* Begin the prompt */
            sprintf(out_val, "Inven:");

            /* Some legal items */
            if ((i1 <= i2) && !use_menu)
            {
                /* Build the prompt */
                sprintf(tmp_val, " %c-%c,'(',')',",
                    index_to_label(i1), index_to_label(i2));

                /* Append */
                strcat(out_val, tmp_val);
            }

            /* Indicate ability to "view" */
            if (!command_see && !use_menu) strcat(out_val, " * to see,");

            /* Append */
            if (equip) strcat(out_val, format(" %s for Equip,", use_menu ? "4 or 6" : "/"));
        }

        /* Viewing equipment */
        else
        {
            /* Begin the prompt */
            sprintf(out_val, "Equip:");

            /* Some legal items */
            if ((e1 <= e2) && !use_menu)
            {
                /* Build the prompt */
                sprintf(tmp_val, " %c-%c,'(',')',",
                    index_to_label(e1), index_to_label(e2));

                /* Append */
                strcat(out_val, tmp_val);
            }

            /* Indicate ability to "view" */
            if (!command_see) strcat(out_val, " * to see,");

            /* Append */
            if (inven) strcat(out_val, format(" %s for Inven,", use_menu ? "4 or 6" : "'/'"));
        }

        /* Indicate legality of the "floor" item */
        if (allow_floor) strcat(out_val, " - for floor,");
        if (select_the_force) strcat(out_val, " w for the Force,");
        if (quiver && p_ptr->unlimited_quiver) strcat(out_val, " z for unlimited quiver,");

        /* Finish the prompt */
        strcat(out_val, " ESC");

        /* Build the prompt */
        sprintf(tmp_val, "(%s) %s", out_val, pmt);

        /* Show the prompt */
        prt(tmp_val, 0, 0);

        /* Get a key */
        which = inkey();

        if (use_menu)
        {
        int max_line = (command_wrk ? max_equip : max_inven);
        switch (which)
        {
            case ESCAPE:
            case '0':
            {
                done = TRUE;
                break;
            }

            case '8':
            case 'k':
            case 'K':
            {
                menu_line += (max_line - 1);
                break;
            }

            case '2':
            case 'j':
            case 'J':
            {
                menu_line++;
                break;
            }

            case '4':
            case '6':
            case 'h':
            case 'H':
            case 'l':
            case 'L':
            {
                /* Verify legality */
                if (!inven || !equip)
                {
                    bell();
                    break;
                }

                /* Hack -- Fix screen */
                if (command_see)
                {
                    /* Load screen */
                    screen_load();

                    /* Save screen */
                    screen_save();
                }

                /* Switch inven/equip */
                command_wrk = !command_wrk;
                max_line = (command_wrk ? max_equip : max_inven);
                if (menu_line > max_line) menu_line = max_line;

                /* Need to redraw */
                break;
            }

            case 'x':
            case 'X':
            case '\r':
            case '\n':
            {
                if (command_wrk == USE_FLOOR)
                {
                    /* Special index */
                    (*cp) = -get_item_label;
                }
                else
                {
                    /* Validate the item */
                    if (!get_item_okay(get_item_label))
                    {
                        bell();
                        break;
                    }

                    /* Allow player to "refuse" certain actions */
                    if (!get_item_allow(get_item_label))
                    {
                        done = TRUE;
                        break;
                    }

                    /* Accept that choice */
                    (*cp) = get_item_label;
                }

                item = TRUE;
                done = TRUE;
                break;
            }
            case 'w':
            {
                if (select_the_force) {
                    *cp = INVEN_FORCE;
                    item = TRUE;
                    done = TRUE;
                }
                break;

            case 'z':
                if (quiver && p_ptr->unlimited_quiver) {
                    *cp = INVEN_UNLIMITED_QUIVER;
                    item = TRUE;
                    done = TRUE;
                }
                break;
            }
        }
        if (menu_line > max_line) menu_line -= max_line;
        }
        else
        {
        /* Parse it */
        switch (which)
        {
            case ESCAPE:
            {
                done = TRUE;
                break;
            }

            case '*':
            case '?':
            case ' ':
            {
                /* Hide the list */
                if (command_see)
                {
                    /* Flip flag */
                    command_see = FALSE;

                    /* Load screen */
                    screen_load();
                }

                /* Show the list */
                else
                {
                    /* Save screen */
                    screen_save();

                    /* Flip flag */
                    command_see = TRUE;
                }
                break;
            }

            case '/':
            {
                /* Verify legality */
                if (!inven || !equip)
                {
                    bell();
                    break;
                }

                /* Hack -- Fix screen */
                if (command_see)
                {
                    /* Load screen */
                    screen_load();

                    /* Save screen */
                    screen_save();
                }

                /* Switch inven/equip */
                command_wrk = !command_wrk;

                /* Need to redraw */
                break;
            }

            case '-':
            {
                /* Use floor item */
                if (allow_floor)
                {
                    /* Scan all objects in the grid */
                    for (this_o_idx = cave[py][px].o_idx; this_o_idx; this_o_idx = next_o_idx)
                    {
                        object_type *o_ptr;

                        /* Acquire object */
                        o_ptr = &o_list[this_o_idx];

                        /* Acquire next object */
                        next_o_idx = o_ptr->next_o_idx;

                        /* Validate the item */
                        if (!item_tester_okay(o_ptr)) continue;

                        /* Special index */
                        k = 0 - this_o_idx;

                        /* Verify the item (if required) */
                        if (other_query_flag && !verify("Try", k)) continue;


                        /* Allow player to "refuse" certain actions */
                        if (!get_item_allow(k)) continue;

                        /* Accept that choice */
                        (*cp) = k;
                        item = TRUE;
                        done = TRUE;
                        break;
                    }

                    /* Outer break */
                    if (done) break;
                }

                /* Oops */
                bell();
                break;
            }

            case '0':
            case '1': case '2': case '3':
            case '4': case '5': case '6':
            case '7': case '8': case '9':
            {
                /* Look up the tag */
                if (!get_tag(&k, which, command_wrk ? USE_EQUIP : USE_INVEN))
                {
                    bell();
                    break;
                }

                /* Hack -- Validate the item */
                if ((k < INVEN_PACK) ? !inven : !equip)
                {
                    bell();
                    break;
                }

                /* Validate the item */
                if (!get_item_okay(k))
                {
                    bell();
                    break;
                }

                /* Allow player to "refuse" certain actions */
                if (!get_item_allow(k))
                {
                    done = TRUE;
                    break;
                }

                /* Accept that choice */
                (*cp) = k;
                item = TRUE;
                done = TRUE;
#ifdef ALLOW_REPEAT
                cur_tag = which;
#endif /* ALLOW_REPEAT */
                break;
            }

#if 0
            case '\n':
            case '\r':
            {
                /* Choose "default" inventory item */
                if (!command_wrk)
                {
                    k = ((i1 == i2) ? i1 : -1);
                }

                /* Choose "default" equipment item */
                else
                {
                    k = ((e1 == e2) ? e1 : -1);
                }

                /* Validate the item */
                if (!get_item_okay(k))
                {
                    bell();
                    break;
                }

                /* Allow player to "refuse" certain actions */
                if (!get_item_allow(k))
                {
                    done = TRUE;
                    break;
                }

                /* Accept that choice */
                (*cp) = k;
                item = TRUE;
                done = TRUE;
                break;
            }
#endif

            case 'w':
            {
                if (select_the_force) {
                    *cp = INVEN_FORCE;
                    item = TRUE;
                    done = TRUE;
                    break;
                }

            case 'z':
                if (quiver && p_ptr->unlimited_quiver) {
                    *cp = INVEN_UNLIMITED_QUIVER;
                    item = TRUE;
                    done = TRUE;
                    break;
                }

                /* Fall through */
            }

            default:
            {
                int ver;
                bool not_found = FALSE;

                /* Look up the alphabetical tag */
                if (!get_tag(&k, which, command_wrk ? USE_EQUIP : USE_INVEN))
                {
                    not_found = TRUE;
                }

                /* Hack -- Validate the item */
                else if ((k < INVEN_PACK) ? !inven : !equip)
                {
                    not_found = TRUE;
                }

                /* Validate the item */
                else if (!get_item_okay(k))
                {
                    not_found = TRUE;
                }

                if (!not_found)
                {
                    /* Accept that choice */
                    (*cp) = k;
                    item = TRUE;
                    done = TRUE;
#ifdef ALLOW_REPEAT
                    cur_tag = which;
#endif /* ALLOW_REPEAT */
                    break;
                }

                /* Extract "query" setting */
                ver = isupper(which);
                which = tolower(which);

                /* Convert letter to inventory index */
                if (!command_wrk)
                {
                    if (which == '(') k = i1;
                    else if (which == ')') k = i2;
                    else k = label_to_inven(which);
                }

                /* Convert letter to equipment index */
                else
                {
                    if (which == '(') k = e1;
                    else if (which == ')') k = e2;
                    else k = label_to_equip(which);
                }

                /* Validate the item */
                if (!get_item_okay(k))
                {
                    bell();
                    break;
                }

                /* Verify the item */
                if (ver && !verify("Try", k))

                {
                    done = TRUE;
                    break;
                }

                /* Allow player to "refuse" certain actions */
                if (!get_item_allow(k))
                {
                    done = TRUE;
                    break;
                }

                /* Accept that choice */
                (*cp) = k;
                item = TRUE;
                done = TRUE;
                break;
            }
        }
        }
    }


    /* Fix the screen if necessary */
    if (command_see)
    {
        /* Load screen */
        screen_load();

        /* Hack -- Cancel "display" */
        command_see = FALSE;
    }


    /* Forget the item_tester_tval restriction */
    item_tester_tval = 0;

    item_tester_no_ryoute = FALSE;

    /* Forget the item_tester_hook restriction */
    item_tester_hook = NULL;


    /* Clean up  'show choices' */
    /* Toggle again if needed */
    if (toggle) toggle_inven_equip();

    /* Update */
    p_ptr->window |= (PW_INVEN | PW_EQUIP);

    /* Window stuff */
    window_stuff();


    /* Clear the prompt line */
    prt("", 0, 0);

    /* Warning if needed */
    if (oops && str) msg_print(str);

    if (item)
    {
#ifdef ALLOW_REPEAT
        repeat_push(*cp);
        if (command_cmd) prev_tag = cur_tag;
#endif /* ALLOW_REPEAT */

        command_cmd = 0; /* Hack -- command_cmd is no longer effective */
    }

    /* Result */
    return (item);
}


#ifdef ALLOW_EASY_FLOOR

/*
 * scan_floor --
 *
 * Return a list of o_list[] indexes of items at the given cave
 * location. Valid flags are:
 *
 *        mode & 0x01 -- Item tester
 *        mode & 0x02 -- Marked items only
 *        mode & 0x04 -- Stop after first
 */
int scan_floor(int *items, int y, int x, int mode)
{
    int this_o_idx, next_o_idx;

    int num = 0;

    /* Sanity */
    if (!in_bounds(y, x)) return 0;

    /* Scan all objects in the grid */
    for (this_o_idx = cave[y][x].o_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Acquire object */
        o_ptr = &o_list[this_o_idx];

        /* Acquire next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Item tester */
        if ((mode & 0x01) && !item_tester_okay(o_ptr)) continue;

        /* Marked */
        if ((mode & 0x02) && !(o_ptr->marked & OM_FOUND)) continue;

        /* Accept this item */
        /* XXX Hack -- Enforce limit */
        if (num < 23)
            items[num] = this_o_idx;

        num++;

        /* Only one */
        if (mode & 0x04) break;
    }

    /* Result */
    return num;
}


/*
 * Display a list of the items on the floor at the given location.
 */
int show_floor(int target_item, int y, int x, int *min_width)
{
    int i, j, k, l;
    int col, len;

    object_type *o_ptr;

    char o_name[MAX_NLEN];

    char tmp_val[80];

    int out_index[23];
    byte out_color[23];
    char out_desc[23][MAX_NLEN];
    int target_item_label = 0;

    int floor_list[23], floor_num;
    int wid, hgt;
    char floor_label[52 + 1];

    bool dont_need_to_show_weights = TRUE;

    /* Get size */
    Term_get_size(&wid, &hgt);

    /* Default length */
    len = MAX((*min_width), 20);


    /* Scan for objects in the grid, using item_tester_okay() */
    floor_num = scan_floor(floor_list, y, x, 0x03);

    /* Display the floor objects */
    for (k = 0, i = 0; i < floor_num && i < 23; i++)
    {
        o_ptr = &o_list[floor_list[i]];

        /* Describe the object */
        object_desc(o_name, o_ptr, 0);

        /* Save the index */
        out_index[k] = i;

        /* Acquire inventory color */
        out_color[k] = tval_to_attr[o_ptr->tval & 0x7F];

        /* Save the object description */
        strcpy(out_desc[k], o_name);

        /* Find the predicted "line length" */
        l = strlen(out_desc[k]) + 5;

        /* Be sure to account for the weight */
        if (show_weights) l += 9;

        if (o_ptr->tval != TV_GOLD) dont_need_to_show_weights = FALSE;

        /* Maintain the maximum length */
        if (l > len) len = l;

        /* Advance to next "line" */
        k++;
    }

    if (show_weights && dont_need_to_show_weights) len -= 9;

    /* Save width */
    *min_width = len;

    /* Find the column to start in */
    col = (len > wid - 4) ? 0 : (wid - len - 1);

    prepare_label_string_floor(floor_label, floor_list, floor_num);

    /* Output each entry */
    for (j = 0; j < k; j++)
    {
        /* Get the index */
        i = floor_list[out_index[j]];

        /* Get the item */
        o_ptr = &o_list[i];

        /* Clear the line */
        prt("", j + 1, col ? col - 2 : col);

        if (use_menu && target_item)
        {
            if (j == (target_item-1))
            {
                strcpy(tmp_val, "> ");
                target_item_label = i;
            }
            else strcpy(tmp_val, "   ");
        }
        else
        {
            /* Prepare an index --(-- */
            sprintf(tmp_val, "%c)", floor_label[j]);
        }

        /* Clear the line with the (possibly indented) index */
        put_str(tmp_val, j + 1, col);

        /* Display the entry itself */
        c_put_str(out_color[j], out_desc[j], j + 1, col + 3);

        /* Display the weight if needed */
        if (show_weights && (o_ptr->tval != TV_GOLD))
        {
            int wgt = o_ptr->weight * o_ptr->number;
            sprintf(tmp_val, "%3d.%1d lb", wgt / 10, wgt % 10);

            prt(tmp_val, j + 1, wid - 9);
        }
    }

    /* Make a "shadow" below the list (only if needed) */
    if (j && (j < 23)) prt("", j + 1, col ? col - 2 : col);

    return target_item_label;
}

/*
 * This version of get_item() is called by get_item() when
 * the easy_floor is on.
 */
bool get_item_floor(int *cp, cptr pmt, cptr str, int mode)
{
    char n1 = ' ', n2 = ' ', which = ' ';

    int j, k, i1, i2, e1, e2;

    bool done, item;

    bool oops = FALSE;

    /* Extract args */
    bool equip = (mode & USE_EQUIP) ? TRUE : FALSE;
    bool inven = (mode & USE_INVEN) ? TRUE : FALSE;
    bool floor = (mode & USE_FLOOR) ? TRUE : FALSE;

    bool allow_equip = FALSE;
    bool allow_inven = FALSE;
    bool allow_floor = FALSE;

    bool toggle = FALSE;

    char tmp_val[160];
    char out_val[160];

    int floor_num, floor_list[23], floor_top = 0;
    int min_width = 0;

    extern bool select_the_force;

    int menu_line = (use_menu ? 1 : 0);
    int max_inven = 0;
    int max_equip = 0;

#ifdef ALLOW_REPEAT

    static char prev_tag = '\0';
    char cur_tag = '\0';

    /* Get the item index */
    if (repeat_pull(cp))
    {
        /* the_force */
        if (select_the_force && (*cp == INVEN_FORCE))
        {
            item_tester_tval = 0;
            item_tester_hook = NULL;
            command_cmd = 0; /* Hack -- command_cmd is no longer effective */
            return (TRUE);
        }

        /* Floor item? */
        else if (floor && (*cp < 0))
        {
            if (prev_tag && command_cmd)
            {
                /* Scan all objects in the grid */
                floor_num = scan_floor(floor_list, py, px, 0x03);

                /* Look up the tag */
                if (get_tag_floor(&k, prev_tag, floor_list, floor_num))
                {
                    /* Accept that choice */
                    (*cp) = 0 - floor_list[k];

                    /* Forget restrictions */
                    item_tester_tval = 0;
                    item_tester_hook = NULL;
                    command_cmd = 0; /* Hack -- command_cmd is no longer effective */

                    /* Success */
                    return TRUE;
                }

                prev_tag = '\0'; /* prev_tag is no longer effective */
            }

            /* Validate the item */
            else if (item_tester_okay(&o_list[0 - (*cp)]))
            {
                /* Forget restrictions */
                item_tester_tval = 0;
                item_tester_hook = NULL;
                command_cmd = 0; /* Hack -- command_cmd is no longer effective */

                /* Success */
                return TRUE;
            }
        }

        else if ( (inven && (*cp >= 0) && (*cp < INVEN_PACK)) 
               || (equip && equip_is_valid_slot(*cp)) )
        {
            if (prev_tag && command_cmd)
            {
                /* Look up the tag and validate the item */
                if (!get_tag(&k, prev_tag, equip_is_valid_slot(*cp) ? USE_EQUIP : USE_INVEN)) /* Reject */;
                else if ((k < EQUIP_BEGIN) ? !inven : !equip) /* Reject */;
                else if (!get_item_okay(k)) /* Reject */;
                else
                {
                    /* Accept that choice */
                    (*cp) = k;

                    /* Forget restrictions */
                    item_tester_tval = 0;
                    item_tester_hook = NULL;
                    command_cmd = 0; /* Hack -- command_cmd is no longer effective */

                    /* Success */
                    return TRUE;
                }

                prev_tag = '\0'; /* prev_tag is no longer effective */
            }

            /* Verify the item */
            else if (get_item_okay(*cp))
            {
                /* Forget restrictions */
                item_tester_tval = 0;
                item_tester_hook = NULL;
                command_cmd = 0; /* Hack -- command_cmd is no longer effective */

                /* Success */
                return TRUE;
            }
        }
    }

#endif /* ALLOW_REPEAT */


    /* Paranoia XXX XXX XXX */
    msg_print(NULL);


    /* Not done */
    done = FALSE;

    /* No item selected */
    item = FALSE;


    /* Full inventory */
    i1 = 0;
    i2 = INVEN_PACK - 1;

    /* Forbid inventory */
    if (!inven) i2 = -1;
    else if (use_menu)
    {
        for (j = 0; j < INVEN_PACK; j++)
            if (item_tester_okay(&inventory[j])) max_inven++;
    }

    /* Restrict inventory indexes */
    while ((i1 <= i2) && (!get_item_okay(i1))) i1++;
    while ((i1 <= i2) && (!get_item_okay(i2))) i2--;


    /* Full equipment */
    e1 = EQUIP_BEGIN;
    e2 = EQUIP_BEGIN + equip_count() - 1;

    /* Forbid equipment */
    if (!equip) e2 = -1;
    else if (use_menu)
    {
        for (j = EQUIP_BEGIN; j < EQUIP_BEGIN + equip_count(); j++)
            if (item_tester_okay(&inventory[j])) max_equip++;
    }

    /* Restrict equipment indexes */
    while ((e1 <= e2) && (!get_item_okay(e1))) e1++;
    while ((e1 <= e2) && (!get_item_okay(e2))) e2--;

    /* Count "okay" floor items */
    floor_num = 0;

    /* Restrict floor usage */
    if (floor)
    {
        /* Scan all objects in the grid */
        floor_num = scan_floor(floor_list, py, px, 0x03);
    }

    /* Accept inventory */
    if (i1 <= i2) allow_inven = TRUE;

    /* Accept equipment */
    if (e1 <= e2) allow_equip = TRUE;

    /* Accept floor */
    if (floor_num) allow_floor = TRUE;

    /* Require at least one legal choice */
    if (!allow_inven && !allow_equip && !allow_floor)
    {
        /* Cancel p_ptr->command_see */
        command_see = FALSE;

        /* Oops */
        oops = TRUE;

        /* Done */
        done = TRUE;

        if (select_the_force) {
            *cp = INVEN_FORCE;
            item = TRUE;
        }
    }

    /* Analyze choices */
    else
    {
        /* Hack -- Start on equipment if requested */
        if (command_see && (command_wrk == (USE_EQUIP))
            && allow_equip)
        {
            command_wrk = (USE_EQUIP);
        }

        /* Use inventory if allowed */
        else if (allow_inven)
        {
            command_wrk = (USE_INVEN);
        }

        /* Use equipment if allowed */
        else if (allow_equip)
        {
            command_wrk = (USE_EQUIP);
        }

        /* Use floor if allowed */
        else if (allow_floor)
        {
            command_wrk = (USE_FLOOR);
        }
    }

    /*
     * 追加オプション(always_show_list)が設定されている場合は常に一覧を表示する
     */
    if ((always_show_list == TRUE) || use_menu) command_see = TRUE;

    /* Hack -- start out in "display" mode */
    if (command_see)
    {
        /* Save screen */
        screen_save();
    }

    /* Repeat until done */
    while (!done)
    {
        int get_item_label = 0;

        /* Show choices */
        int ni = 0;
        int ne = 0;

        /* Scan windows */
        for (j = 0; j < 8; j++)
        {
            /* Unused */
            if (!angband_term[j]) continue;

            /* Count windows displaying inven */
            if (window_flag[j] & (PW_INVEN)) ni++;

            /* Count windows displaying equip */
            if (window_flag[j] & (PW_EQUIP)) ne++;
        }

        /* Toggle if needed */
        if ((command_wrk == (USE_EQUIP) && ni && !ne) ||
            (command_wrk == (USE_INVEN) && !ni && ne))
        {
            /* Toggle */
            toggle_inven_equip();

            /* Track toggles */
            toggle = !toggle;
        }

        /* Update */
        p_ptr->window |= (PW_INVEN | PW_EQUIP);

        /* Redraw windows */
        window_stuff();

        /* Inventory screen */
        if (command_wrk == (USE_INVEN))
        {
            /* Extract the legal requests */
            n1 = I2A(i1);
            n2 = I2A(i2);

            /* Redraw if needed */
            if (command_see) get_item_label = show_inven(menu_line, mode);
        }

        /* Equipment screen */
        else if (command_wrk == (USE_EQUIP))
        {
            /* Extract the legal requests */
            n1 = I2A(e1 - EQUIP_BEGIN);
            n2 = I2A(e2 - EQUIP_BEGIN);

            /* Redraw if needed */
            if (command_see) get_item_label = show_equip(menu_line, mode);
        }

        /* Floor screen */
        else if (command_wrk == (USE_FLOOR))
        {
            j = floor_top;
            k = MIN(floor_top + 23, floor_num) - 1;

            /* Extract the legal requests */
            n1 = I2A(j - floor_top);
            n2 = I2A(k - floor_top);

            /* Redraw if needed */
            if (command_see) get_item_label = show_floor(menu_line, py, px, &min_width);
        }

        /* Viewing inventory */
        if (command_wrk == (USE_INVEN))
        {
            /* Begin the prompt */
            sprintf(out_val, "Inven:");

            if (!use_menu)
            {
                /* Build the prompt */
                sprintf(tmp_val, " %c-%c,'(',')',",
                    index_to_label(i1), index_to_label(i2));

                /* Append */
                strcat(out_val, tmp_val);
            }

            /* Indicate ability to "view" */
            if (!command_see && !use_menu) strcat(out_val, " * to see,");

            /* Append */
            if (allow_equip)
            {
                if (!use_menu)
                    strcat(out_val, " / for Equip,");
                else if (allow_floor)
                    strcat(out_val, " 6 for Equip,");
                else
                    strcat(out_val, " 4 or 6 for Equip,");
            }

            /* Append */
            if (allow_floor)
            {
                if (!use_menu)
                    strcat(out_val, " - for floor,");
                else if (allow_equip)
                    strcat(out_val, " 4 for floor,");
                else
                    strcat(out_val, " 4 or 6 for floor,");
            }
        }

        /* Viewing equipment */
        else if (command_wrk == (USE_EQUIP))
        {
            /* Begin the prompt */
            sprintf(out_val, "Equip:");

            if (!use_menu)
            {
                /* Build the prompt */
                sprintf(tmp_val, " %c-%c,'(',')',",
                    index_to_label(e1), index_to_label(e2));

                /* Append */
                strcat(out_val, tmp_val);
            }

            /* Indicate ability to "view" */
            if (!command_see && !use_menu) strcat(out_val, " * to see,");

            /* Append */
            if (allow_inven)
            {
                if (!use_menu)
                    strcat(out_val, " / for Inven,");
                else if (allow_floor)
                    strcat(out_val, " 4 for Inven,");
                else
                    strcat(out_val, " 4 or 6 for Inven,");
            }

            /* Append */
            if (allow_floor)
            {
                if (!use_menu)
                    strcat(out_val, " - for floor,");
                else if (allow_inven)
                    strcat(out_val, " 6 for floor,");
                else
                    strcat(out_val, " 4 or 6 for floor,");
            }
        }

        /* Viewing floor */
        else if (command_wrk == (USE_FLOOR))
        {
            /* Begin the prompt */
            sprintf(out_val, "Floor:");

            if (!use_menu)
            {
                /* Build the prompt */
                sprintf(tmp_val, " %c-%c,'(',')',", n1, n2);

                /* Append */
                strcat(out_val, tmp_val);
            }

            /* Indicate ability to "view" */
            if (!command_see && !use_menu) strcat(out_val, " * to see,");

            if (use_menu)
            {
                if (allow_inven && allow_equip)
                {
                    strcat(out_val, " 4 for Equip, 6 for Inven,");
                }
                else if (allow_inven)
                {
                    strcat(out_val, " 4 or 6 for Inven,");
                }
                else if (allow_equip)
                {
                    strcat(out_val, " 4 or 6 for Equip,");
                }
            }
            /* Append */
            else if (allow_inven)
            {
                strcat(out_val, " / for Inven,");
            }
            else if (allow_equip)
            {
                strcat(out_val, " / for Equip,");
            }

            /* Append */
            if (command_see && !use_menu)
            {
                strcat(out_val, " Enter for scroll down,");
            }
        }

        /* Append */
        if (select_the_force) strcat(out_val, " w for the Force,");

        /* Finish the prompt */
        strcat(out_val, " ESC");

        /* Build the prompt */
        sprintf(tmp_val, "(%s) %s", out_val, pmt);

        /* Show the prompt */
        prt(tmp_val, 0, 0);

        /* Get a key */
        which = inkey();

        if (use_menu)
        {
        int max_line = 1;
        if (command_wrk == USE_INVEN) max_line = max_inven;
        else if (command_wrk == USE_EQUIP) max_line = max_equip;
        else if (command_wrk == USE_FLOOR) max_line = MIN(23, floor_num);
        switch (which)
        {
            case ESCAPE:
            case 'z':
            case 'Z':
            case '0':
            {
                done = TRUE;
                break;
            }

            case '8':
            case 'k':
            case 'K':
            {
                menu_line += (max_line - 1);
                break;
            }

            case '2':
            case 'j':
            case 'J':
            {
                menu_line++;
                break;
            }

            case '4':
            case 'h':
            case 'H':
            {
                /* Verify legality */
                if (command_wrk == (USE_INVEN))
                {
                    if (allow_floor) command_wrk = USE_FLOOR;
                    else if (allow_equip) command_wrk = USE_EQUIP;
                    else
                    {
                        bell();
                        break;
                    }
                }
                else if (command_wrk == (USE_EQUIP))
                {
                    if (allow_inven) command_wrk = USE_INVEN;
                    else if (allow_floor) command_wrk = USE_FLOOR;
                    else
                    {
                        bell();
                        break;
                    }
                }
                else if (command_wrk == (USE_FLOOR))
                {
                    if (allow_equip) command_wrk = USE_EQUIP;
                    else if (allow_inven) command_wrk = USE_INVEN;
                    else
                    {
                        bell();
                        break;
                    }
                }
                else
                {
                    bell();
                    break;
                }

                /* Hack -- Fix screen */
                if (command_see)
                {
                    /* Load screen */
                    screen_load();

                    /* Save screen */
                    screen_save();
                }

                /* Switch inven/equip */
                if (command_wrk == USE_INVEN) max_line = max_inven;
                else if (command_wrk == USE_EQUIP) max_line = max_equip;
                else if (command_wrk == USE_FLOOR) max_line = MIN(23, floor_num);
                if (menu_line > max_line) menu_line = max_line;

                /* Need to redraw */
                break;
            }

            case '6':
            case 'l':
            case 'L':
            {
                /* Verify legality */
                if (command_wrk == (USE_INVEN))
                {
                    if (allow_equip) command_wrk = USE_EQUIP;
                    else if (allow_floor) command_wrk = USE_FLOOR;
                    else
                    {
                        bell();
                        break;
                    }
                }
                else if (command_wrk == (USE_EQUIP))
                {
                    if (allow_floor) command_wrk = USE_FLOOR;
                    else if (allow_inven) command_wrk = USE_INVEN;
                    else
                    {
                        bell();
                        break;
                    }
                }
                else if (command_wrk == (USE_FLOOR))
                {
                    if (allow_inven) command_wrk = USE_INVEN;
                    else if (allow_equip) command_wrk = USE_EQUIP;
                    else
                    {
                        bell();
                        break;
                    }
                }
                else
                {
                    bell();
                    break;
                }

                /* Hack -- Fix screen */
                if (command_see)
                {
                    /* Load screen */
                    screen_load();

                    /* Save screen */
                    screen_save();
                }

                /* Switch inven/equip */
                if (command_wrk == USE_INVEN) max_line = max_inven;
                else if (command_wrk == USE_EQUIP) max_line = max_equip;
                else if (command_wrk == USE_FLOOR) max_line = MIN(23, floor_num);
                if (menu_line > max_line) menu_line = max_line;

                /* Need to redraw */
                break;
            }

            case 'x':
            case 'X':
            case '\r':
            case '\n':
            {
                if (command_wrk == USE_FLOOR)
                {
                    /* Special index */
                    (*cp) = -get_item_label;
                }
                else
                {
                    /* Validate the item */
                    if (!get_item_okay(get_item_label))
                    {
                        bell();
                        break;
                    }

                    /* Allow player to "refuse" certain actions */
                    if (!get_item_allow(get_item_label))
                    {
                        done = TRUE;
                        break;
                    }

                    /* Accept that choice */
                    (*cp) = get_item_label;
                }

                item = TRUE;
                done = TRUE;
                break;
            }
            case 'w':
            {
                if (select_the_force) {
                    *cp = INVEN_FORCE;
                    item = TRUE;
                    done = TRUE;
                    break;
                }
            }
        }
        if (menu_line > max_line) menu_line -= max_line;
        }
        else
        {
        /* Parse it */
        switch (which)
        {
            case ESCAPE:
            {
                done = TRUE;
                break;
            }

            case '*':
            case '?':
            case ' ':
            {
                /* Hide the list */
                if (command_see)
                {
                    /* Flip flag */
                    command_see = FALSE;

                    /* Load screen */
                    screen_load();
                }

                /* Show the list */
                else
                {
                    /* Save screen */
                    screen_save();

                    /* Flip flag */
                    command_see = TRUE;
                }
                break;
            }

            case '\n':
            case '\r':
            case '+':
            {
                int i, o_idx;
                cave_type *c_ptr = &cave[py][px];

                if (command_wrk != (USE_FLOOR)) break;

                /* Get the object being moved. */
                o_idx = c_ptr->o_idx;

                /* Only rotate a pile of two or more objects. */
                if (!(o_idx && o_list[o_idx].next_o_idx)) break;

                /* Remove the first object from the list. */
                excise_object_idx(o_idx);

                /* Find end of the list. */
                i = c_ptr->o_idx;
                while (o_list[i].next_o_idx)
                    i = o_list[i].next_o_idx;

                /* Add after the last object. */
                o_list[i].next_o_idx = o_idx;

                /* Re-scan floor list */ 
                floor_num = scan_floor(floor_list, py, px, 0x03);

                /* Hack -- Fix screen */
                if (command_see)
                {
                    /* Load screen */
                    screen_load();

                    /* Save screen */
                    screen_save();
                }

                break;
            }

            case '/':
            {
                if (command_wrk == (USE_INVEN))
                {
                    if (!allow_equip)
                    {
                        bell();
                        break;
                    }
                    command_wrk = (USE_EQUIP);
                }
                else if (command_wrk == (USE_EQUIP))
                {
                    if (!allow_inven)
                    {
                        bell();
                        break;
                    }
                    command_wrk = (USE_INVEN);
                }
                else if (command_wrk == (USE_FLOOR))
                {
                    if (allow_inven)
                    {
                        command_wrk = (USE_INVEN);
                    }
                    else if (allow_equip)
                    {
                        command_wrk = (USE_EQUIP);
                    }
                    else
                    {
                        bell();
                        break;
                    }
                }

                /* Hack -- Fix screen */
                if (command_see)
                {
                    /* Load screen */
                    screen_load();

                    /* Save screen */
                    screen_save();
                }

                /* Need to redraw */
                break;
            }

            case '-':
            {
                if (!allow_floor)
                {
                    bell();
                    break;
                }

                /*
                 * If we are already examining the floor, and there
                 * is only one item, we will always select it.
                 * If we aren't examining the floor and there is only
                 * one item, we will select it if floor_query_flag
                 * is FALSE.
                 */
                if (floor_num == 1)
                {
                    if ((command_wrk == (USE_FLOOR)) || (!carry_query_flag))
                    {
                        /* Special index */
                        k = 0 - floor_list[0];

                        /* Allow player to "refuse" certain actions */
                        if (!get_item_allow(k))
                        {
                            done = TRUE;
                            break;
                        }

                        /* Accept that choice */
                        (*cp) = k;
                        item = TRUE;
                        done = TRUE;

                        break;
                    }
                }

                /* Hack -- Fix screen */
                if (command_see)
                {
                    /* Load screen */
                    screen_load();

                    /* Save screen */
                    screen_save();
                }

                command_wrk = (USE_FLOOR);

                break;
            }

            case '0':
            case '1': case '2': case '3':
            case '4': case '5': case '6':
            case '7': case '8': case '9':
            {
                if (command_wrk != USE_FLOOR)
                {
                    /* Look up the tag */
                    if (!get_tag(&k, which, command_wrk))
                    {
                        bell();
                        break;
                    }

                    /* Hack -- Validate the item */
                    if (equip_is_valid_slot(k) ? !equip : !inven)
                    {
                        bell();
                        break;
                    }

                    /* Validate the item */
                    if (!get_item_okay(k))
                    {
                        bell();
                        break;
                    }
                }
                else
                {
                    /* Look up the alphabetical tag */
                    if (get_tag_floor(&k, which, floor_list, floor_num))
                    {
                        /* Special index */
                        k = 0 - floor_list[k];
                    }
                    else
                    {
                        bell();
                        break;
                    }
                }

                /* Allow player to "refuse" certain actions */
                if (!get_item_allow(k))
                {
                    done = TRUE;
                    break;
                }

                /* Accept that choice */
                (*cp) = k;
                item = TRUE;
                done = TRUE;
#ifdef ALLOW_REPEAT
                cur_tag = which;
#endif /* ALLOW_REPEAT */
                break;
            }

#if 0
            case '\n':
            case '\r':
            {
                /* Choose "default" inventory item */
                if (command_wrk == (USE_INVEN))
                {
                    k = ((i1 == i2) ? i1 : -1);
                }

                /* Choose "default" equipment item */
                else if (command_wrk == (USE_EQUIP))
                {
                    k = ((e1 == e2) ? e1 : -1);
                }

                /* Choose "default" floor item */
                else if (command_wrk == (USE_FLOOR))
                {
                    if (floor_num == 1)
                    {
                        /* Special index */
                        k = 0 - floor_list[0];

                        /* Allow player to "refuse" certain actions */
                        if (!get_item_allow(k))
                        {
                            done = TRUE;
                            break;
                        }

                        /* Accept that choice */
                        (*cp) = k;
                        item = TRUE;
                        done = TRUE;
                    }
                    break;
                }

                /* Validate the item */
                if (!get_item_okay(k))
                {
                    bell();
                    break;
                }

                /* Allow player to "refuse" certain actions */
                if (!get_item_allow(k))
                {
                    done = TRUE;
                    break;
                }

                /* Accept that choice */
                (*cp) = k;
                item = TRUE;
                done = TRUE;
                break;
            }
#endif

            case 'w':
            {
                if (select_the_force) {
                    *cp = INVEN_FORCE;
                    item = TRUE;
                    done = TRUE;
                    break;
                }

                /* Fall through */
            }

            default:
            {
                int ver;

                if (command_wrk != USE_FLOOR)
                {
                    bool not_found = FALSE;

                    /* Look up the alphabetical tag */
                    if (!get_tag(&k, which, command_wrk))
                    {
                        not_found = TRUE;
                    }

                    /* Hack -- Validate the item */
                    else if ((k < INVEN_PACK) ? !inven : !equip)
                    {
                        not_found = TRUE;
                    }

                    /* Validate the item */
                    else if (!get_item_okay(k))
                    {
                        not_found = TRUE;
                    }

                    if (!not_found)
                    {
                        /* Accept that choice */
                        (*cp) = k;
                        item = TRUE;
                        done = TRUE;
#ifdef ALLOW_REPEAT
                        cur_tag = which;
#endif /* ALLOW_REPEAT */
                        break;
                    }
                }
                else
                {
                    /* Look up the alphabetical tag */
                    if (get_tag_floor(&k, which, floor_list, floor_num))
                    {
                        /* Special index */
                        k = 0 - floor_list[k];

                        /* Accept that choice */
                        (*cp) = k;
                        item = TRUE;
                        done = TRUE;
#ifdef ALLOW_REPEAT
                        cur_tag = which;
#endif /* ALLOW_REPEAT */
                        break;
                    }
                }

                /* Extract "query" setting */
                ver = isupper(which);
                which = tolower(which);

                /* Convert letter to inventory index */
                if (command_wrk == (USE_INVEN))
                {
                    if (which == '(') k = i1;
                    else if (which == ')') k = i2;
                    else k = label_to_inven(which);
                }

                /* Convert letter to equipment index */
                else if (command_wrk == (USE_EQUIP))
                {
                    if (which == '(') k = e1;
                    else if (which == ')') k = e2;
                    else k = label_to_equip(which);
                }

                /* Convert letter to floor index */
                else if (command_wrk == USE_FLOOR)
                {
                    if (which == '(') k = 0;
                    else if (which == ')') k = floor_num - 1;
                    else k = islower(which) ? A2I(which) : -1;
                    if (k < 0 || k >= floor_num || k >= 23)
                    {
                        bell();
                        break;
                    }

                    /* Special index */
                    k = 0 - floor_list[k];
                }

                /* Validate the item */
                if ((command_wrk != USE_FLOOR) && !get_item_okay(k))
                {
                    bell();
                    break;
                }

                /* Verify the item */
                if (ver && !verify("Try", k))

                {
                    done = TRUE;
                    break;
                }

                /* Allow player to "refuse" certain actions */
                if (!get_item_allow(k))
                {
                    done = TRUE;
                    break;
                }

                /* Accept that choice */
                (*cp) = k;
                item = TRUE;
                done = TRUE;
                break;
            }
        }
        }
    }

    /* Fix the screen if necessary */
    if (command_see)
    {
        /* Load screen */
        screen_load();

        /* Hack -- Cancel "display" */
        command_see = FALSE;
    }


    /* Forget the item_tester_tval restriction */
    item_tester_tval = 0;

    /* Forget the item_tester_hook restriction */
    item_tester_hook = NULL;


    /* Clean up  'show choices' */
    /* Toggle again if needed */
    if (toggle) toggle_inven_equip();

    /* Update */
    p_ptr->window |= (PW_INVEN | PW_EQUIP);

    /* Window stuff */
    window_stuff();


    /* Clear the prompt line */
    prt("", 0, 0);

    /* Warning if needed */
    if (oops && str) msg_print(str);

    if (item)
    {
#ifdef ALLOW_REPEAT
        repeat_push(*cp);
        if (command_cmd) prev_tag = cur_tag;
#endif /* ALLOW_REPEAT */

        command_cmd = 0; /* Hack -- command_cmd is no longer effective */
    }

    /* Result */
    return (item);
}


static bool py_pickup_floor_aux(void)
{
    s16b this_o_idx;

    cptr q, s;

    int item;

    /* Restrict the choices */
    item_tester_hook = inven_carry_okay;

    /* Get an object */
    q = "Get which item? ";
    s = "You no longer have any room for the objects on the floor.";

    if (get_item(&item, q, s, (USE_FLOOR)))
    {
        this_o_idx = 0 - item;
    }
    else
    {
        return (FALSE);
    }

    /* Pick up the object */
    py_pickup_aux(this_o_idx);

    return (TRUE);
}


/*
 * Make the player carry everything in a grid
 *
 * If "pickup" is FALSE then only gold will be picked up
 *
 * This is called by py_pickup() when easy_floor is TRUE.
 */
void py_pickup_floor(bool pickup)
{
    s16b this_o_idx, next_o_idx = 0;

    char o_name[MAX_NLEN];
    object_type *o_ptr;

    int floor_num = 0, floor_list[23], floor_o_idx = 0;

    int can_pickup = 0;

    /* Scan the pile of objects */
    for (this_o_idx = cave[py][px].o_idx; this_o_idx; this_o_idx = next_o_idx)
    {
        object_type *o_ptr;

        /* Access the object */
        o_ptr = &o_list[this_o_idx];

        /* Describe the object */
        object_desc(o_name, o_ptr, 0);

        /* Access the next object */
        next_o_idx = o_ptr->next_o_idx;

        /* Hack -- disturb */
        disturb(0, 0);

        /* Pick up gold */
        if (o_ptr->tval == TV_GOLD)
        {
            /* Message */
            msg_format("You have found %d gold pieces worth of %s.",
                (int)o_ptr->pval, o_name);

            /* Collect the gold */
            p_ptr->au += o_ptr->pval;

            /* Redraw gold */
            p_ptr->redraw |= (PR_GOLD);

            /* Window stuff */
            p_ptr->window |= (PW_PLAYER);

            if (prace_is_(RACE_MON_LEPRECHAUN))
                p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);

            /* Delete the gold */
            delete_object_idx(this_o_idx);

            /* Check the next object */
            continue;
        }
        else if (o_ptr->marked & OM_NOMSG)
        {
            /* If 0 or 1 non-NOMSG items are in the pile, the NOMSG ones are
             * ignored. Otherwise, they are included in the prompt. */
            o_ptr->marked &= ~(OM_NOMSG);
            continue;
        }

        /* Count non-gold objects that can be picked up. */
        if (inven_carry_okay(o_ptr))
        {
            can_pickup++;
        }

        /* Remember this object index */
        if (floor_num < 23)
            floor_list[floor_num] = this_o_idx;

        /* Count non-gold objects */
        floor_num++;

        /* Remember this index */
        floor_o_idx = this_o_idx;
    }

    /* There are no non-gold objects */
    if (!floor_num)
        return;

    /* Mention the number of objects */
    if (!pickup)
    {
        /* One object */
        if (floor_num == 1)
        {
            /* Access the object */
            o_ptr = &o_list[floor_o_idx];

#ifdef ALLOW_EASY_SENSE

            /* Option: Make object sensing easy */
            if (easy_sense)
            {
                /* Sense the object */
                (void) sense_object(o_ptr);
            }

#endif /* ALLOW_EASY_SENSE */

            /* Describe the object */
            object_desc(o_name, o_ptr, 0);

            /* Message */
            msg_format("You see %s.", o_name);

        }

        /* Multiple objects */
        else
        {
            /* Message */
            msg_format("You see a pile of %d items.", floor_num);

        }

        /* Done */
        return;
    }

    /* The player has no room for anything on the floor. */
    if (!can_pickup)
    {
        /* One object */
        if (floor_num == 1)
        {
            /* Access the object */
            o_ptr = &o_list[floor_o_idx];

#ifdef ALLOW_EASY_SENSE

            /* Option: Make object sensing easy */
            if (easy_sense)
            {
                /* Sense the object */
                (void) sense_object(o_ptr);
            }

#endif /* ALLOW_EASY_SENSE */

            /* Describe the object */
            object_desc(o_name, o_ptr, 0);

            /* Message */
            msg_format("You have no room for %s.", o_name);

        }

        /* Multiple objects */
        else
        {
            /* Message */
            msg_print("You have no room for any of the objects on the floor.");

        }

        /* Done */
        return;
    }

    /* One object */
    if (floor_num == 1)
    {
        /* Hack -- query every object */
        if (carry_query_flag)
        {
            char out_val[MAX_NLEN+20];

            /* Access the object */
            o_ptr = &o_list[floor_o_idx];

#ifdef ALLOW_EASY_SENSE

            /* Option: Make object sensing easy */
            if (easy_sense)
            {
                /* Sense the object */
                (void) sense_object(o_ptr);
            }

#endif /* ALLOW_EASY_SENSE */

            /* Describe the object */
            object_desc(o_name, o_ptr, 0);

            /* Build a prompt */
            (void) sprintf(out_val, "Pick up %s? ", o_name);


            /* Ask the user to confirm */
            if (!get_check(out_val))
            {
                /* Done */
                return;
            }
        }

        /* Access the object */
        o_ptr = &o_list[floor_o_idx];

#ifdef ALLOW_EASY_SENSE

        /* Option: Make object sensing easy */
        if (easy_sense)
        {
            /* Sense the object */
            (void) sense_object(o_ptr);
        }

#endif /* ALLOW_EASY_SENSE */

        /* Pick up the object */
        py_pickup_aux(floor_o_idx);
    }

    /* Allow the user to choose an object */
    else
    {
        while (can_pickup--)
        {
            if (!py_pickup_floor_aux()) break;
        }
    }
}

#endif /* ALLOW_EASY_FLOOR */
