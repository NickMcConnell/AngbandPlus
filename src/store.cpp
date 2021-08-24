/* File: store.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"
#include <src/cmds.h>
#include "src/store.h"
#include "src/storedialog.h"
#include "src/object_settings.h"


service_info services_info[] =
{
    //SERVICE_ENCHANT_ARMOR
    {STORE_ARMOR,   125,    do_service_enchant,     "Enchant armor [price varies]"},
    //SERVICE_ENCHANT_TO_HIT
    {STORE_WEAPON,  125,    do_service_enchant,     "Enchant weapon to-hit [price varies]"},
    //SERVICE_ENCHANT_TO_DAM
    {STORE_WEAPON,  125,    do_service_enchant,     "Enchant weapon to-dam [price varies]"},
    //SERVICE_ELEM_BRAND_WEAP
    {STORE_WEAPON,  35000,  do_service_brand,       "Elemental Brand a weapon [price varies]"},
    //SERVICE_ELEM_BRAND_AMMO
    {STORE_WEAPON,  17500,  do_service_brand,       "Elemental brand some ammunition[price varies]"},
    //SERVICE_RECHARGING
    {STORE_MAGIC,   175,    do_service_recharge,    "Recharge item [price varies]"},
    //SERVICE_IDENTIFY
    {STORE_MAGIC,   75,     do_service_identify,    "Identify item"},
    //SERVICE_IDENTIFY_FULLY
    {STORE_MAGIC,   4500,   do_service_identify,    "Fully identify item"},
    //SERVICE_CURE_CRITICAL
    {STORE_TEMPLE,  75,     do_service_cure_critical,"Cure Critical Wounds"},
    //SERVICE_RESTORE_LIFE_LEVELS
    {STORE_TEMPLE,  1000,   do_service_restore_life,"Restore Life Levels"},
    //SERVICE_REMOVE_CURSE
    {STORE_TEMPLE,  300,    do_service_remove_curse,"Remove curse"},
    //SERVICE_REMOVE_HEAVY_CURSE
    {STORE_TEMPLE,  15000,  do_service_remove_curse,"Remove heavy curse"},
    //SERVICE_RESTORE_STAT
    {STORE_ALCHEMY, 700,    do_service_stat,        "Restore stat"},
    //SERVICE_INCREASE_STAT
    {STORE_ALCHEMY, 37500,  do_service_stat,        "Increase stat"},
    //SERVICE_CREATE_RANDART
    {STORE_GUILD,   750000, do_service_make_randart,"Create Artifact[price varies]"},
    //SERVICE_PROBE_QUEST_MON
    {STORE_GUILD,   150,    do_service_probing,     "Probe a Quest Monster[price varies]"},
    //SERVICE_BUY_HEALING_POTION
    {STORE_TEMPLE,  20000,  do_service_buy_object,  "Purchase Potion of Healing"},
    //SERVICE_BUY_LIFE_POTION
    {STORE_TEMPLE,  125000, do_service_buy_object,  "Purchase Potion of Life"},
    //SERVICE_BUY_SCROLL_BANISHMENT
    {STORE_MAGIC,   125000, do_service_buy_object,  "Purchase Scroll of Mass Banishment"},
    //SERVICE_FIREPROOF_BOOK
    {STORE_BOOKSHOP, 100000,do_service_fireproof,   "Make Spell Book Fireproof[price varies]"},
    //SERVICE_QUEST_DEFER_REWARD
    {STORE_GUILD,   0,      do_service_defer_reward,"Defer Quest Reward"},
    //SERVICE_ABANDON_QUEST
    {STORE_GUILD,   0,      do_service_abandon_quest,"Abandon Your Quest"},
    //SERVICE_QUEST_REWARD_RANDART
    {STORE_GUILD,   0,      do_service_quest_art_reward,"Create Artifact Quest Reward"},
    //SERVICE_QUEST_REWARD_INC_HP
    {STORE_GUILD,   0,      do_service_hp_inc,      "Permanent Hit Point Increase Reward"},
    //SERVICE_QUEST_REWARD_INC_STAT
    {STORE_GUILD,   0,      do_service_stat,        "Permanent Stat Increase Reward"},
    //SERVICE_QUEST_REWARD_AUGMENTATION
    {STORE_GUILD,   0,      do_service_augment,     "Permanent Stats Augmentation Reward"},
};




/*** Utilities ***/



/* Randomly select one of the entries in an array */
#define ONE_OF(x)	x[randint0(N_ELEMENTS(x))]


/*
 * Shopkeeper welcome messages.
 *
 * The shopkeeper's is automatically added first first
 * The character's name is added if indicated by ZZZ.
 */
static QString comment_welcome[] =
{
    " glances only briefly in your direction.",
    " nods to you.",
    " says hello.",
    ": \"See anything you like, adventurer?\"",
    ": \"How may I help you, ZZZ?\"",
    ": \"Welcome back, ZZZ.\"",
    ": \"A pleasure to see you again, ZZZ.\"",
    ": \"How may I be of assistance, good ZZZ?\"",
    ": \"You do honour to my humble store, noble ZZZ.\"",
    ": \"I and my family are entirely at your service, glorious ZZZ.\""
};

/*
 * Messages for reacting to purchase prices.
 */
static QString comment_worthless[] =
{
    "Arrgghh!",
    "You bastard!",
    "You hear someone sobbing...",
    "The shopkeeper howls in agony!",
    "The shopkeeper wails in anguish!",
    "The shopkeeper mutters in disgust."
};

static QString comment_bad[] =
{
    "Damn!",
    "You fiend!",
    "The shopkeeper curses at you.",
    "The shopkeeper glares at you."
};

static QString comment_accept[] =
{
    "Okay.",
    "Fine.",
    "Accepted!",
    "Agreed!",
    "Done!",
    "Taken!"
};

static QString comment_good[] =
{
    "Cool!",
    "You've made my day!",
    "The shopkeeper sniggers.",
    "The shopkeeper giggles.",
    "The shopkeeper laughs loudly."
};

static QString comment_great[] =
{
    "Yipee!",
    "I think I'll retire!",
    "The shopkeeper jumps for joy.",
    "The shopkeeper smiles gleefully.",
    "Wow.  I'm going to name my new villa in your honour."
};


/*
 * The greeting a shopkeeper gives the character says a lot about his
 * general attitude.
 *
 * Taken and modified from Sangband 1.0.
 */
QString store_welcome(int store_idx)
{
    QString player_name;

    store_type *st_ptr = &store[store_idx];

    owner_type *ot_ptr = &b_info[(store_idx * z_info->b_max) + st_ptr->owner];

    QString owner_name = ot_ptr->owner_name;

    /* We go from level 1 - 50  */
    byte i = ((unsigned)p_ptr->lev - 1) / 5;

    /* Sanity check in case we increase the max level carelessly */
    i = MIN(i, N_ELEMENTS(comment_welcome) - 1);

    QString welcome = comment_welcome[i];

    /* Extract the first name of the store owner (stop before the first space) */
    QString short_name = owner_name;
    int j = short_name.indexOf(' ');
    if (j >=0) short_name.truncate(j);

    welcome.prepend(short_name);

    /* Get a title for the character */
    if ((i % 2) && randint0(2)) player_name = get_player_title();
    else if (randint0(2))       player_name = op_ptr->full_name;
    else                        player_name = (p_ptr->psex == SEX_MALE ? "sir" : "lady");

    welcome.replace("ZZZ", player_name);

    return (welcome);
}

/*
 * Let a shop-keeper React to a purchase
 *
 * We paid "price", it was worth "value", and we thought it was worth "guess"
 */
static void purchase_analyze(s32b price, s32b value, s32b guess)
{
    /* Item was worthless, but we bought it */
    if ((value <= 0) && (price > value))
        message(ONE_OF(comment_worthless));

    /* Item was cheaper than we thought, and we paid more than necessary */
    else if ((value < guess) && (price > value))
        message(ONE_OF(comment_bad));

    /* Item was a good bargain, and we got away with it */
    else if ((value > guess) && (value < (4 * guess)) && (price < value))
        message(ONE_OF(comment_good));

    /* Item was a great bargain, and we got away with it */
    else if ((value > guess) && (price < value))
        message(ONE_OF(comment_great));
}





/*return false if the player doesn't have enough gold*/
static bool check_gold(s32b price)
{
    if (price > p_ptr->au)
    {
        message(QString("It would cost you %1 gold.  You don't have enough.") .arg(price));

        return (FALSE);
    }

    return (TRUE);
}


bool do_service_enchant(byte choice, u32b price)
{
    s16b add_to;
    s16b counter = 1;
    int item;
    object_type *o_ptr;
    object_kind *k_ptr;
    QString o_name;

    /* Enchant armor if requested */
    if (choice == SERVICE_ENCHANT_ARMOR)
    {
        item_tester_hook = item_tester_hook_ided_armour;
    }
    else item_tester_hook = item_tester_hook_ided_weapon;

    /* Get an item */
    QString q = "Enchant which item? ";
    QString s = "You have nothing to enchant.";
    if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_QUIVER))) return (FALSE);

    /*Got the item*/
    o_ptr = &inventory[item];
    k_ptr = &k_info[o_ptr->k_idx];

    if (choice == SERVICE_ENCHANT_ARMOR) add_to = o_ptr->to_a;
    else if (choice == SERVICE_ENCHANT_TO_HIT) add_to = o_ptr->to_h;
    /* to-damage*/
    else add_to = o_ptr->to_d;

    /* Description, shorten it for artifacts */
    if (o_ptr->is_artifact()) o_name = object_desc(o_ptr, ODESC_BASE);
    else o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

    /*
     * We will eventually run into the u32 variable max number, so
     * cut off the + allowed
     */
    if (add_to >= 15)
    {
        message(QString("%1 %2 cannot be enchanted any further") .arg(((item >= 0) ? "Your" : "The")) .arg(o_name));

        return (FALSE);
    }

    /* Missiles are easier to enchant */
    if ((o_ptr->tval == TV_BOLT) ||
        (o_ptr->tval == TV_ARROW) ||
        (o_ptr->tval == TV_SHOT))
    {
        price = price / 20;
    }

    /* Greater to-hit and to-dam makes things more expensive*/
    while (add_to >= counter)
    {
        price += (price * 8) / 10;

        counter ++;
    }

    /*multiply for quantity*/
    price *= o_ptr->number;

    /*artifacts are double*/
    if (o_ptr->art_num) choice *= 2;

    /*Too expensive*/
    if (!check_gold(price)) return (FALSE);

    QString prompt = (QString("Spend %1 gold to enchant %2? ") .arg(price) .arg(o_name));
    if (!get_check(prompt)) return (FALSE);

    /*reduce the gold*/
    p_ptr->au -= price;

    /* Description */
    o_name = object_desc(o_ptr, ODESC_FULL);

    /* Describe */
    message(QString("%1 %2 glow%3 brightly!") .arg((item >= 0) ? "Your" : "The") .arg(o_name) .arg((o_ptr->number > 1) ? "" : "s"));

    if (choice == SERVICE_ENCHANT_ARMOR) o_ptr->to_a ++;
    else if (choice == SERVICE_ENCHANT_TO_HIT) o_ptr->to_h ++;
    /* to-damage*/
    else o_ptr->to_d++;

    /* Break curse */
    if (o_ptr->is_cursed() &&
        (!(k_ptr->k_flags3 & (TR3_PERMA_CURSE))) &&
         (add_to >= 0) && (rand_int(100) < 25))
    {
        message(QString("The curse is broken!"));

        /* Uncurse the object */
        o_ptr->uncurse();

    }

    return (TRUE);
}

bool do_service_brand(byte choice, u32b price)
{
    byte brand_type;
    object_type *o_ptr;
    QString o_name;
    int item;

    /* Enchant weapon if requested */
    if (choice == SERVICE_ELEM_BRAND_WEAP)
    {
        item_tester_hook = item_tester_hook_wieldable_ided_weapon;
    }
    /*Ammo*/
    else item_tester_hook = item_tester_hook_ided_ammo;

    /* Get an item */
    QString q = "Brand which item? ";
    QString s = "You have nothing to Brand.";
    if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN | USE_QUIVER))) return (FALSE);

    /*Got the item*/
    o_ptr = &inventory[item];

    /* Description, shorten it for artifacts */
    if (o_ptr->art_num) o_name = object_desc(o_ptr, ODESC_BASE);
    else o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

    /*If artifact, or ego item, don't bother*/
    if ((o_ptr->art_num) || (o_ptr->ego_num))
    {
        message(QString("%1 cannot be branded!") .arg(capitalize_first(o_name)));

        return (FALSE);
    }

    /* Missiles are easier to enchant */
    if ((o_ptr->tval == TV_BOLT) ||
        (o_ptr->tval == TV_ARROW) ||
        (o_ptr->tval == TV_SHOT))
    {
        price = price / 20;
    }

    /*multiply for quantity*/
    price *= o_ptr->number;

    /*Too expensive*/
    if (!check_gold(price)) return (FALSE);

    QString prompt = (QString("Spend %1 gold to brand %2? ") .arg(price) .arg(o_name));
    if (!get_check(prompt)) return (FALSE);

    if (choice == SERVICE_ELEM_BRAND_WEAP)
    {
        if (one_in_(2)) brand_type = BRAND_OFFSET_FLAME;
        else brand_type = BRAND_OFFSET_FROST;

    }
    /*ammo*/
    else
    {
        /* Select the brand */
        if (one_in_(3))
            brand_type = EGO_AMMO_FLAME;
        else if (one_in_(2))
            brand_type = EGO_AMMO_FROST;
        else brand_type = EGO_AMMO_VENOM;
    }

    /*Brand*/
    if (brand_object(o_ptr, brand_type, FALSE))
    {
        p_ptr->au -= price;
        return (TRUE);
    }
    message(QString("Branding failed."));
    return (FALSE);
}

bool do_service_recharge(byte choice, u32b price)
{
    (void)choice;
    object_type *o_ptr;
    QString o_name;
    int item;

    /* Only accept legal items, which are wands and staffs */
    item_tester_hook = item_tester_hook_recharge;

    /* Get an item */
    QString q = "Recharge which item? ";
    QString s = "You have nothing to recharge.";
    if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return (FALSE);

    /*Got the item*/
    o_ptr = &inventory[item];

    /* Description */
    o_name = object_desc(o_ptr, ODESC_FULL);

    /* Extract the object "level" */
    byte lev = k_info[o_ptr->k_idx].k_level;

    /*base price on level*/
    price += price * (lev / 2);

    /*get the price for rods*/
    if (o_ptr->tval == TV_ROD)
    {
        if (!o_ptr->timeout)
        {
            /* Describe */
            message(QString("The %1 %2 not require re-charging!") .arg(o_name) .arg((o_ptr->number > 1 ? "do" : "does")));

            return (FALSE);
        }
        else
        {
            price += (price * o_ptr->timeout) / 20;
        }
    }

    /*Wands, and Staffs*/
    else
    {
        price += o_ptr->pval * price;

        /*Bigger charage for a stack of staffs or wands*/
        if (o_ptr->number > 1) price += (o_ptr->number - 1) * price;
    }

    /*Too expensive*/
    if (!check_gold(price)) return(FALSE);

    QString prompt = (QString("Spend %1 gold to recharge %2?") .arg(price) .arg(o_name));

    if (!get_check(prompt)) return(FALSE);

    /*re-charge the rods*/
    if (o_ptr->tval == TV_ROD)
    {
        o_ptr->timeout = 0;
    }
    /*Wands and staffs*/
    else
    {
        recharge_staff_wand(o_ptr, 75);
    }

    /*We re-charged an item*/
    p_ptr->au -= price;

    return (TRUE);
}


bool do_service_identify(byte choice, u32b price)
{
   bool success;

    /*Too expensive*/
    if (!check_gold(price)) return (FALSE);

    /*We identified an item*/
    if (choice == SERVICE_IDENTIFY) success = ident_spell();
    // choice == SERVICE_IDENTIFY_FULLY
    else success = identify_fully();

    if (!success) return (FALSE);

    // We ID'ed an item
    p_ptr->au -= price;
    return (TRUE);
}

bool do_service_cure_critical(byte choice, u32b price)
{
    (void)choice;

    bool healed = FALSE;

    /*Too expensive*/
    if (!check_gold(price)) return (FALSE);

    QString prompt = (QString("Spend %1 gold to cure critical wounds? ") .arg(price));
    if (!get_check(prompt)) return (FALSE);

    /*Heal the player, note if they actually need healing*/
    if (hp_player(damroll(8, 10))) healed = TRUE;
    if (clear_timed(TMD_BLIND, TRUE)) healed = TRUE;
    if (clear_timed(TMD_CONFUSED, TRUE)) healed = TRUE;
    if (clear_timed(TMD_POISONED, TRUE)) healed = TRUE;
    if (set_stun(0)) healed = TRUE;
    if (set_cut(0)) healed = TRUE;

    /*We identified an item*/
    if (healed)
    {
        p_ptr->au -= price;
        return (TRUE);
    }
    pop_up_message_box(QString("You do not require any healing services."));

    return (FALSE);
}

bool do_service_restore_life(byte choice, u32b price)
{
    (void)choice;

    /*Too expensive*/
    if (!check_gold(price)) return (FALSE);

    QString prompt = (QString("Spend %1 gold to restore life levels? ") .arg(price));
    if (!get_check(prompt)) return (FALSE);

    /*We restored the player*/
    if (restore_level())
    {
        p_ptr->au -= price;
        return (TRUE);
    }
    /* Not needed*/
    pop_up_message_box(QString("Your life levels do not require restoring."));
    return (FALSE);
}

bool do_service_remove_curse(byte choice, u32b price)
{
    /*Too expensive*/
    if (!check_gold(price)) return (FALSE);

    /*We removed a curse an item, charge the player*/
    // FALSE is SERVICE_REMOVE_CURSE
    if (remove_curse(choice == SERVICE_REMOVE_HEAVY_CURSE ? TRUE : FALSE))
    {
        p_ptr->au -= price;
        return (TRUE);
    }

    else pop_up_message_box(QString("No items had a curse removed."));
    return (FALSE);
}

bool do_service_stat(byte choice, u32b price)
{

    int result;
    QString title = get_title();

    /*Too expensive*/
    if (choice != SERVICE_QUEST_REWARD_INC_STAT)
    {
        if (!check_gold(price)) return (FALSE);
    }
    else
    {
        /* Ask confirmation */
        if (!get_check(QString("Choose a stat to permanently increase, %1?") .arg(title))) return (FALSE);
    }

    result = launch_stat_dialog(choice);

    //Player cancelled
    if (result == A_MAX+1) return (FALSE);

    // No eligible stats
    if (result == A_MAX)
    {

        if (choice == SERVICE_RESTORE_STAT)
        {
            pop_up_message_box(QString("None of your stats need restoring."));
        }
        else if (choice == SERVICE_INCREASE_STAT)
        {
            pop_up_message_box(QString("Your stats cannot be increased any further."));
        }
        /* must be SERVICE_QUEST_REWARD_INC_STAT*/
        else
        {
            pop_up_message_box(QString("Your stats cannot be permanently increased any further."));
        }
        return (FALSE);
    }

    /*restore the stat*/
    if (choice == SERVICE_RESTORE_STAT)
    {
        /*charge it*/
        if (do_res_stat(result)) p_ptr->au -= price;
        else pop_up_message_box(QString("Your %1 does not need restoring.") .arg(stat_names_full[result]));

    }
    else if (choice == SERVICE_INCREASE_STAT)
    {
        if (do_inc_stat(result)) p_ptr->au -= price;
        else pop_up_message_box(QString("Your %1 cannot be increased any further.") .arg(stat_names_full[result]));
    }
    /* must be SERVICE_QUEST_REWARD_INC_STAT*/
    else
    {
        do_perm_stat_boost(result);
        guild_quest_wipe(TRUE);
    }
    return (TRUE);
}

bool do_service_make_randart(byte choice, u32b price)
{
    (void)choice;
    object_type *o_ptr;
    object_kind *k_ptr;
    QString o_name;
    int item;
    s32b o_value;
    QString title = get_title();

    if ((birth_no_artifacts) || (birth_no_xtra_artifacts))
    {
        message(QString("Nothing happens."));
        return (FALSE);
    }

    /* Only accept legal items */
    item_tester_hook = item_tester_hook_randart;

    /* Get an item */
    QString q = (QString("Choose an item to be made into an artifact, %1.") .arg(title));
    QString s = (QString("You have no eligible item, %1.") .arg(title));
    if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return (FALSE);

    /*Got the item*/
    o_ptr = &inventory[item];

    /*Got the object kind*/
    k_ptr = &k_info[o_ptr->k_idx];

    /* Description */
    o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Get the "value" of the item */
    o_value = k_ptr->cost * 50;

    /*Get the price for the item*/
    price = price + o_value;

    /*Too expensive*/
    if (!check_gold(price)) return (FALSE);

    QString prompt = (QString("Spend %1 gold to make %2 into an artifact? ") .arg(price) .arg(o_name));
    if (!get_check(prompt)) return (FALSE);

    /*extra power bonus for expensive items and high player fame*/
    s32b art_value = p_ptr->q_fame / 20 + MAX((k_ptr->cost / 2000), p_ptr->q_fame / 50);

    /*Hack - add in any to-hit and to-value, since they will be erased*/
    art_value += (o_ptr->to_h + o_ptr->to_d + o_ptr->to_a) / 2;

    /*actually create the Randart, or handle failure*/
    if (make_one_randart(o_ptr, art_value, TRUE))
    {
        p_ptr->au -= price;

        /* Identify it fully */
        o_ptr->mark_fully_known(TRUE);

        /* Mark the history */
        o_ptr->origin_nature = ORIGIN_ACQUIRE;
        o_ptr->origin_r_idx = 0;
        o_ptr->origin_dlvl = 0;

        /*Let the player know what they just got*/
        object_info_screen(o_ptr);

        return (TRUE);
    }

    message(QString("The attempt at making an artifact has failed"));
    return (FALSE);
}

bool do_service_probing(byte choice, u32b price)
{
    (void)choice;
    QString race_name;
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
    monster_race *r_ptr = &r_info[q_ptr->mon_idx];
    monster_lore *l_ptr = &l_list[q_ptr->mon_idx];

    if ((!quest_single_r_idx(q_ptr)) || (q_ptr->mon_idx == 0))
    {
        message(QString("You are not currently questing for a specific creature."));
        return (FALSE);
    }

    /* Not a vault quest, so get the monster race name (singular)*/
    race_name = monster_desc_race(q_ptr->mon_idx);

    /* Make it plural if necessary*/
    if (q_ptr->q_max_num > 1) race_name = plural_aux(race_name);

    price += r_ptr->level * 100;

    /*Too expensive*/
    if (!check_gold(price)) return (FALSE);

    /*confirm*/
    QString prompt = (QString("Spend %1 gold to probe %2? ") .arg(price) .arg(race_name));
    if (!get_check(prompt)) return (FALSE);

    /*charge the player*/
    p_ptr->au -= price;

    /*learn something about the monster*/
    lore_probe_monster_aux(q_ptr->mon_idx);

    /* Hack -- Increse the sightings, and ranged attacks around 50% of the time */
    l_ptr->sights = SHRT_MAX;
    l_ptr->ranged = UCHAR_MAX;

    /* Know "race" flags */
    l_ptr->r_l_flags3 |= (r_ptr->flags3 & RF3_RACE_MASK);
    /* Know "forced" flags */
    l_ptr->r_l_flags1 |= (r_ptr->flags1 & (RF1_FORCE_DEPTH | RF1_FORCE_MAXHP));

    /* Output to the screen */
    describe_monster(q_ptr->mon_idx, false, "");

    return (TRUE);
}

bool do_service_buy_object(byte choice, u32b price)
{
    QString o_name;
    int k_idx;
    object_type *i_ptr;
    object_type object_type_body;

    i_ptr = &object_type_body;

    QString obj_type = "Potion";

    /*Too expensive*/
    if (!check_gold(price)) return (FALSE);

    /*get the healing potion index*/
    if (choice == SERVICE_BUY_HEALING_POTION)
    {
        k_idx = lookup_kind(TV_POTION, SV_POTION_HEALING);
    }
    /*get the potion of life index*/
    else if (choice == SERVICE_BUY_LIFE_POTION)
    {
        k_idx = lookup_kind(TV_POTION, SV_POTION_LIFE);
    }

    /* SERVICE_BUY_SCROLL_BANISHMENT */
    else
    {
        k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_MASS_BANISHMENT);
        obj_type = "Scroll";
    }

    /*get the object kind*/
    object_kind *k_ptr = &k_info[k_idx];

    /*Too expensive*/
    if (!check_gold(price)) return (FALSE);

    QString prompt = (QString("Spend %1 gold to purchase a %3 of %2? ") .arg(price) .arg(k_ptr->k_name) .arg(obj_type));
    if (!get_check(prompt)) return (FALSE);

    /*charge the player*/
    p_ptr->au -= price;

    /* Make the potion */
    object_prep(i_ptr, k_idx);

    /* Identify it */
    i_ptr->mark_known(TRUE);

    /* Describe the result */
    o_name = object_desc(i_ptr, ODESC_FULL);

    /* Remember history */
    object_history(i_ptr, ORIGIN_STORE, 0);

    /* Note that the pack is too full */
    if (!inven_carry_okay(i_ptr))
    {
        message(QString("You have no room in your backpack."));

        /* Drop the object */
        drop_near(i_ptr, -1, p_ptr->py, p_ptr->px);

        /* Inform the player */
        message(QString("Your %1 is waiting outside!") .arg(o_name));

    }

    /* Give it to the player */
    else
    {
        int item_new;

        /* Give it to the player */
        item_new = inven_carry(i_ptr);

        /* Describe just the result */
        o_name = object_desc(&inventory[item_new], ODESC_PREFIX | ODESC_FULL);

        /* Message */
        message(QString("You have (%1) %2.") .arg(index_to_label(item_new)) .arg(o_name));
    }

    return (TRUE);
}

bool do_service_fireproof(byte choice, u32b price)
{
    int i;
    (void)choice;
    object_type *o_ptr;
    object_kind *k_ptr;
    QString o_name;
    int item;
    QString s, q;

    /*Too expensive*/
    if (!check_gold(price)) return (FALSE);

    /* Restrict choices to spell books */
    item_tester_tval = cp_ptr->spell_book;

    /* Only accept legal items, which are burnable books */
    item_tester_hook = item_tester_hook_flammable_book;

    /* Get an item */
    q = "Fireproof which book? ";
    if (cp_ptr->spell_book == TV_PRAYER_BOOK) s = "You have no flammable prayer books!";
    else s = "You have no flammable spell books!";
    if (!get_item(&item, q, s, (USE_INVEN | USE_FLOOR))) return (FALSE);

    /*Got the item*/
    o_ptr = &inventory[item];
    k_ptr = &k_info[o_ptr->k_idx];

    /*Adjust the price for the book and the number of books*/
    price += (k_ptr->cost * 50);
    price *= o_ptr->number;

    /*Too expensive*/
    if (!check_gold(price)) return (FALSE);

    /* Description */
    o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

    /*confirm*/
    QString prompt = (QString("Spend %1 gold to fireproof %2? ") .arg(price) .arg(o_name));
    if (!get_check(prompt)) return (FALSE);

    /*find the ego-item*/
    for (i = 0; i < z_info->e_max; i++)
    {
        ego_item_type *e_ptr = &e_info[i];

        if (e_ptr->e_name.contains("Fireproof"))
        {
            int j;
            bool right_type = FALSE;

            /*Confirm the right tval*/
            for (j = 0; j < EGO_TVALS_MAX; j++)
            {
                if (e_ptr->tval[j] == cp_ptr->spell_book) right_type = TRUE;
            }

            /*We found it*/
            if (right_type)
            {
                /*charge the player*/
                p_ptr->au -= price;

                o_ptr->ego_num = i;

                /* Description */
                o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

                /*Confirm it worked*/
                message(QString("You have %1") .arg(o_name));

                return (TRUE);
            }
        }
    }

    return (FALSE);
}

bool do_service_defer_reward(byte choice, u32b price)
{
    (void)choice;
    (void)price;
    QString title = get_title();
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    /* Check for current quest */
    if (!guild_quest_level())
    {
        message(QString("You don't have a current quest, %1.") .arg(title));
        return (FALSE);
    }

    /* Ask confirmation */
    if (!get_check(QString("Really defer your reward, %1?").arg(title))) return (FALSE);

    p_ptr->deferred_rewards += (q_ptr->q_fame_inc * 3) / 2;

    guild_quest_wipe(FALSE);

    return (TRUE);
}

bool do_service_abandon_quest(byte choice, u32b price)
{
    (void)choice;
    (void)price;

    QString title = get_title();

    /* Check for current quest */
    if (!guild_quest_level())
    {
        message(QString("You don't have a current quest, %1.") .arg(title));
        return (FALSE);
    }

    /* Ask confirmation */
    if (!get_check(QString("Abandon your quest, %1?") .arg(title))) return (FALSE);

    /* Remove the current quest */
    quest_fail();

    /*Get the new title, and give a message*/
    message(QString("The guild is disappointed in you, %1.") .arg(get_title()));

    return (TRUE);
}

bool do_service_quest_art_reward(byte choice, u32b price)
{
    (void)choice;
    (void)price;
    int rand_power;
    object_type *o_ptr;
    object_kind *k_ptr;
    QString o_name;
    int item;
    QString title = get_title();
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    /* Paranoia - should never happen */
    if ((birth_no_artifacts) || (birth_no_xtra_artifacts))
    {
        message(QString("Nothing happens."));
        return (FALSE);
    }

    /* Only accept legal items */
    item_tester_hook = item_tester_hook_randart;

    /* Get an item */
    QString q = (QString("Choose an item to be made into an artifact, %1. ") .arg(title));
    QString s = (QString("You have no eligible item, %1. ") .arg(title));
    if (!get_item(&item, q, s, (USE_EQUIP | USE_INVEN))) return (FALSE);

    /*Got the item*/
    o_ptr = &inventory[item];

    /*Got the object kind*/
    k_ptr = &k_info[o_ptr->k_idx];

    /* Description */
    o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

    QString prompt = (QString("Make %1 into an artifact? ") .arg(o_name));

    if (!get_check(prompt)) return (FALSE);

    /* extra power bonus for expensive items and high player fame*/
    rand_power = (p_ptr->q_fame + p_ptr->deferred_rewards) / 20 + MAX((k_ptr->cost / 2000), p_ptr->q_fame / 50);

    /*Hack - add in any to-hit and to-value, since they will be erased*/
    rand_power += (o_ptr->to_h + o_ptr->to_d + o_ptr->to_a) / 2;

    /*actually create the Randart, or handle failure*/
    if (make_one_randart(o_ptr, rand_power, TRUE))
    {
        /* Identify it fully */
        o_ptr->mark_fully_known(TRUE);

        /* Mark the history */
        o_ptr->origin_nature = ORIGIN_REWARD;
        o_ptr->origin_r_idx = 0;
        o_ptr->origin_dlvl = q_ptr->base_level;

        /*Let the player know what they just got*/
        object_info_screen(o_ptr);

        guild_quest_wipe(TRUE);

        return (TRUE);
    }
    message(QString("The attempt at making an artifact has failed"));
    return (FALSE);
}
bool do_service_hp_inc(byte choice, u32b price)
{
    (void)choice;
    (void)price;
    QString title = get_title();

    /* Check for current quest */
    if (!guild_quest_level())
    {
        message(QString("You don't have a current quest, %1.") .arg(title));
        return (FALSE);
    }

    /* Ask confirmation */
    if (!get_check(QString("Do you wish to permanently increase your hit points, %1?") .arg(title))) return (FALSE);

    grant_reward_hp();

    /* Inform the player */
    message(QString("You now have an increased vitality, %1!") .arg(title));

    guild_quest_wipe(TRUE);

    return (TRUE);
}
bool do_service_augment(byte choice, u32b price)
{
    int i;
    (void)choice;
    (void)price;
    QString title = get_title();

    /* Check for current quest */
    if (!guild_quest_level())
    {
        message(QString("You don't have a current quest, %1.") .arg(title));
        return (FALSE);
    }

    /* Ask confirmation */
    if (!get_check(QString("Do you wish to permanently increase your stats, %1?").arg(title))) return (FALSE);

    /* Boost all six stats */
    for (i = 0; i < A_MAX; i++) do_perm_stat_boost(i);

    /* The quest is over */
    guild_quest_wipe(TRUE);
    return (TRUE);
}


/* Percent decrease or increase in price of goods		 */
s16b moria_chr_adj(void)
{
    int charisma  = p_ptr->state.stat_loaded_cur[A_CHR];

    if (charisma > 117) 		return(90);
    else if (charisma > 107) 	return(92);
    else if (charisma > 87)		return(94);
    else if (charisma > 67)		return(96);
    else if (charisma > 18)		return(98);
    else switch(charisma)
    {
        case 18:	return(100);
        case 17:	return(101);
        case 16:	return(102);
        case 15:	return(103);
        case 14:	return(104);
        case 13:	return(106);
        case 12:	return(108);
        case 11:	return(110);
        case 10:	return(112);
        case 9:  return(114);
        case 8:  return(116);
        case 7:  return(118);
        case 6:  return(120);
        case 5:  return(122);
        case 4:  return(125);
        case 3:  return(130);
        default: return(100);
    }
}

/*
 * Determine the price of an object (qty one) in a store.
 *  store_buying == TRUE  means the shop is buying, player selling
 *               == FALSE means the shop is selling, player buying
 *
 * This function takes into account the player's charisma, but
 * never lets a shop-keeper lose money in a transaction.
 *
 * The "greed" value should exceed 100 when the player is "buying" the
 * object, and should be less than 100 when the player is "selling" it.
 *
 * Hack -- the black market always charges twice as much as it should.
 *
 */
s32b price_item(int this_store, object_type *o_ptr, bool store_buying)
{
    int adjust;
    s32b price;
    owner_type *ot_ptr;

    store_type *st_ptr = &store[this_store];

    ot_ptr = &b_info[(this_store * z_info->b_max) + st_ptr->owner];

    /* Get the value of one of the items */
    price = object_value(o_ptr);

    /* Worthless items */
    if (price <= 0) return (0L);

    /* Add in the charisma factor */
    if (this_store == STORE_B_MARKET) adjust = 175;
    else if (game_mode == GAME_NPPMORIA)
    {
        adjust = moria_chr_adj();
    }
    else adjust = adj_chr_gold[p_ptr->state.stat_index[A_CHR]];


    /* Shop is buying */
    if (store_buying)
    {
        /* Check for no_selling option */
        if (birth_no_selling) return (0L);

        /* Set the factor */
        adjust = 185 - adjust;

        /* Boundry control */
        if (adjust > 100) adjust = 100;

        /* Mega-Hack -- Black market sucks */
        if (this_store == STORE_B_MARKET) price = price / 2;
    }

    /* Shop is selling */
    else
    {
        adjust = 25 + adjust;

        /* Boundry control */
        if (adjust < 100) adjust = 100;

        /* Mega-Hack -- Black market sucks */
        if (this_store == STORE_B_MARKET) price = price * 2;
    }

    /* Compute the final price (with rounding) */
    price = (price * adjust + 50L) / 100L;

    /* Now limit the price to the purse limit */
    if (store_buying && (price > ot_ptr->max_cost))
        price = ot_ptr->max_cost;

    /* Note -- Never become "free" */
    if (price <= 0L) return (1L);

    /* Return the price */
    return (price);
}



/*
 * Certain "cheap" objects should be created in "piles".
 *
 * Some objects can be sold at a "discount" (in smaller piles).
 *
 * Standard percentage discounts include 10, 25, 50, 75, and 90.
 */
static void mass_produce(object_type *o_ptr)
{
    int size = 1;

    int discount = 0;

    s32b cost = object_value(o_ptr);

    /* Analyze the type */
    switch (o_ptr->tval)
    {
        /* Food, Flasks, and Lites */
        case TV_FOOD:
        case TV_FLASK:
        case TV_LIGHT:
        {
            if (cost <= 5L) size += damroll(2, 5);
            if (cost <= 20L) size += damroll(2, 5);
            break;
        }

        case TV_POTION:
        case TV_SCROLL:
        {
            if (cost <= 60L) size += damroll(3, 5);
            if (cost <= 240L) size += damroll(1, 5);
            break;
        }

        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:
        case TV_DRUID_BOOK:
        {
            if (cost <= 50L) size += damroll(2, 3);
            if (cost <= 500L) size += damroll(1, 3);
            break;
        }

        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_SHIELD:
        case TV_GLOVES:
        case TV_BOOTS:
        case TV_CLOAK:
        case TV_HELM:
        case TV_CROWN:
        case TV_SWORD:
        case TV_POLEARM:
        case TV_HAFTED:
        case TV_DIGGING:
        case TV_BOW:
        {
            if (o_ptr->ego_num) break;
            if (cost <= 10L) size += damroll(2, 2);
            if (cost <= 100L) size += damroll(2, 2);
            break;
        }
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        case TV_SPIKE:
        {
            if (o_ptr->ego_num) size += damroll(3, 5);
            else if ((o_ptr->to_h > 0) || (o_ptr->to_d > 0)) size += damroll(4, 6);
            else size += damroll(8, 7);
            break;
        }

    }

    /* Pick a discount */
    if (cost < 5L)
    {
        discount = 0;
    }
    else if ((one_in_(25)) && (cost > 10L))
    {
        discount = 10;
    }
    else if (one_in_(50))
    {
        discount = 25;
    }
    else if (one_in_(150))
    {
        discount = 50;
    }
    else if (one_in_(300))
    {
        discount = 75;
    }
    else if (one_in_(500))
    {
        discount = 90;
    }

    /* Save the discount */
    o_ptr->discount = discount;

    /* Save the total pile size */
    o_ptr->number = MAX(size - (size * discount / 100), 1);

    /* Hack -- rods need to increase PVAL if stacked */
    if (o_ptr->tval == TV_ROD)
    {
        o_ptr->pval = o_ptr->number * k_info[o_ptr->k_idx].pval;
    }
}



/*
 * Determine if a store object can "absorb" another object.
 *
 * See "object_similar()" for the same function for the "player".
 *
 * This function can ignore many of the checks done for the player,
 * since stores (but not the home) only get objects under certain
 * restricted circumstances.
 */
static bool store_object_similar(object_type *o_ptr, object_type *j_ptr)
{
    /* Extract the flags */
    o_ptr->update_object_flags();
    j_ptr->update_object_flags();

    /* Different objects cannot be stacked */
    if (o_ptr->k_idx != j_ptr->k_idx) return (0);

    /* Different charges (etc) cannot be stacked, except for staves, wands and rods. */
    if ((o_ptr->pval != j_ptr->pval) &&
        (o_ptr->tval != TV_WAND) &&
        (o_ptr->tval != TV_ROD) &&
        (o_ptr->tval != TV_STAFF)) return (0);

    /* Require many identical values */
    if (o_ptr->to_h != j_ptr->to_h) return (0);
    if (o_ptr->to_d != j_ptr->to_d) return (0);
    if (o_ptr->to_a != j_ptr->to_a) return (0);

    /* Require identical "artifact" names */
    if (o_ptr->art_num != j_ptr->art_num) return (0);

    /* Require identical "ego-item" names */
    if (o_ptr->ego_num != j_ptr->ego_num) return (0);

    /* Hack -- Never stack "powerful" items */
    if (o_ptr->xtra1 || j_ptr->xtra1) return (0);

    /* Mega-Hack -- Handle lites */
    if (o_ptr->is_fuelable_lite())
    {
        if (o_ptr->timeout != j_ptr->timeout) return 0;
    }

    /* Hack -- Never stack recharging items */
    else if (o_ptr->timeout || j_ptr->timeout) return (0);

    /* Require many identical values */
    if (o_ptr->ac != j_ptr->ac) return (0);
    if (o_ptr->dd != j_ptr->dd) return (0);
    if (o_ptr->ds != j_ptr->ds) return (0);

    /* Hack -- Never stack chests */
    if (o_ptr->tval == TV_CHEST) return (0);

    /* Require matching "discount" fields */
    if (o_ptr->discount != j_ptr->discount) return (0);

    /*Allow well balanced items to stack only with other
             *well balanced items*/
    if ((o_ptr->ident & IDENT_PERFECT_BALANCE) !=
        (o_ptr->ident & IDENT_PERFECT_BALANCE)) return (FALSE);

    /* Different flags */
    if ((o_ptr->obj_flags_1 != j_ptr->obj_flags_1) || (o_ptr->obj_flags_2 != j_ptr->obj_flags_2) || \
        (o_ptr->obj_flags_3 != j_ptr->obj_flags_3) || (o_ptr->obj_flags_native != j_ptr->obj_flags_native)) return(FALSE);

    /* They match, so they must be similar */
    return (TRUE);
}


/*
 * Allow a store object to absorb another object
 */
static void store_object_absorb(object_type *o_ptr, object_type *j_ptr)
{
    int total = o_ptr->number + j_ptr->number;

    /* Combine quantity, lose excess items */
    o_ptr->number = (total > STORE_MAX_ITEM) ? STORE_MAX_ITEM : total;

    /*
     *Hack -- if rods are stacking, add the pvals (maximum timeouts)
     * and any charging timeouts together.
     */
    if (o_ptr->tval == TV_ROD)
    {
        o_ptr->pval += j_ptr->pval;
        o_ptr->timeout += j_ptr->timeout;
    }

    /* Hack -- if wands/staves are stacking, combine the charges. */
    if ((o_ptr->tval == TV_WAND) || (o_ptr->tval == TV_STAFF))
    {
        o_ptr->pval += j_ptr->pval;
    }

    /* Combine the histories */
    stack_histories(o_ptr, j_ptr);
}


/*
 * Check to see if the shop will be carrying too many objects
 *
 * Note that the shop, just like a player, will not accept things
 * it cannot hold.  Before, one could "nuke" objects this way, by
 * adding them to a pile which was already full.
 */
static bool store_check_num(int st, object_type *o_ptr)
{
    int i;
    object_type *j_ptr;

    store_type *st_ptr = &store[st];

    if (st == STORE_GUILD) return (FALSE);

    /* Free space is always usable */
    if (st_ptr->stock_num < st_ptr->stock_size) return TRUE;

    /* The "home" acts like the player */
    if (st == STORE_HOME)
    {
        /* Check all the objects */
        for (i = 0; i < st_ptr->stock_num; i++)
        {
            /* Get the existing object */
            j_ptr = &st_ptr->stock[i];

            /* Can the new object be combined with the old one? */
            if (object_similar(j_ptr, o_ptr)) return (TRUE);
        }
    }

    /* Normal stores do special stuff */
    else
    {
        /* Check all the objects */
        for (i = 0; i < st_ptr->stock_num; i++)
        {
            /* Get the existing object */
            j_ptr = &st_ptr->stock[i];

            /* Can the new object be combined with the old one? */
            if (store_object_similar(j_ptr, o_ptr)) return (TRUE);
        }
    }

    /* But there was no room at the inn... */
    return (FALSE);
}


/*
 * Determine if the current store will purchase the given object
 *
 * Note that a shop-keeper must refuse to buy "worthless" objects
 */
bool store_will_buy(int store_num, object_type *o_ptr)
{
    /* Hack -- The Home and guild are simple */
    if (store_num == STORE_HOME) return (TRUE);
    if (store_num == STORE_GUILD) return (FALSE);

    /* Some results are slightly different for Moria */
    if (game_mode == GAME_NPPMORIA)
    {
        if ((store_num == STORE_TEMPLE) && (o_ptr->tval == TV_PRAYER_BOOK)) return (TRUE);
        if ((store_num == STORE_MAGIC) &&  (o_ptr->tval == TV_MAGIC_BOOK)) 	return (TRUE);
    }

    /* Switch on the store */
    switch (store_num)
    {
        /* General Store */
        case STORE_GENERAL:
        {
            /* Analyze the type */
            switch (o_ptr->tval)
            {
                case TV_FOOD:
                case TV_LIGHT:
                case TV_FLASK:
                case TV_SPIKE:
                case TV_SHOT:
                case TV_ARROW:
                case TV_BOLT:
                case TV_DIGGING:
                case TV_CLOAK:
                break;
                default:
                return (FALSE);
            }
            break;
        }

        /* Armoury */
        case STORE_ARMOR:
        {
            /* Analyze the type */
            switch (o_ptr->tval)
            {
                case TV_BOOTS:
                case TV_GLOVES:
                case TV_CROWN:
                case TV_HELM:
                case TV_SHIELD:
                case TV_CLOAK:
                case TV_SOFT_ARMOR:
                case TV_HARD_ARMOR:
                case TV_DRAG_ARMOR:
                case TV_DRAG_SHIELD:
                break;
                default:
                return (FALSE);
            }
            break;
        }

        /* Weapon Shop */
        case STORE_WEAPON:
        {
            /* Analyze the type */
            switch (o_ptr->tval)
            {
                case TV_SHOT:
                case TV_BOLT:
                case TV_ARROW:
                case TV_BOW:
                case TV_DIGGING:
                case TV_HAFTED:
                case TV_POLEARM:
                case TV_SWORD:
                break;
                default:
                return (FALSE);
            }
            break;
        }

        /* Temple */
        case STORE_TEMPLE:
        {
            /* Analyze the type */
            switch (o_ptr->tval)
            {

                case TV_SCROLL:
                case TV_POTION:
                case TV_HAFTED:
            break;
                case TV_POLEARM:
                case TV_SWORD:
                {
                    /* Known blessed blades are accepted too */
                    if (is_blessed(o_ptr) && o_ptr->is_known()) break;
                }
                default:
                return (FALSE);
            }
            break;
        }

        /* Alchemist */
        case STORE_ALCHEMY:
        {
            /* Analyze the type */
            switch (o_ptr->tval)
            {

                case TV_SCROLL:
                case TV_POTION:
                break;
                default:
                return (FALSE);
            }
            break;
        }

        /* Magic Shop */
        case STORE_MAGIC:
        {
            /* Analyze the type */
            switch (o_ptr->tval)
            {
                case TV_AMULET:
                case TV_RING:
                case TV_STAFF:
                case TV_WAND:
                case TV_ROD:
                break;
                default:
                return (FALSE);
            }
            break;
        }

        case STORE_BOOKSHOP:
        {
            /* Analyze the type */
            switch (o_ptr->tval)
            {
                case TV_MAGIC_BOOK:
                case TV_DRUID_BOOK:
                case TV_PRAYER_BOOK:
                break;
                default:
                return (FALSE);
            }
        }
    }

    /* Ignore "worthless" items XXX XXX XXX */
    if (object_value(o_ptr) <= 0) return (FALSE);

    /* Assume okay */
    return (TRUE);
}



/*
 * Add an object to the inventory of the Home.
 *
 * In all cases, return the slot (or -1) where the object was placed.
 *
 * Note that this is a hacked up version of "inven_carry()".
 *
 * Also note that it may not correctly "adapt" to "knowledge" becoming
 * known: the player may have to pick stuff up and drop it again.
 */
static int home_carry(object_type *o_ptr)
{
    int i, slot;
    u32b value, j_value;
    object_type *j_ptr;

    store_type *st_ptr = &store[STORE_HOME];

    /* Check each existing object (try to combine) */
    for (slot = 0; slot < st_ptr->stock_num; slot++)
    {
        /* Get the existing object */
        j_ptr = &st_ptr->stock[slot];

        if ((o_ptr->number + j_ptr->number) >= MAX_STACK_SIZE) continue;

        /* The home acts just like the player */
        if (object_similar(j_ptr, o_ptr))
        {
            /* Save the new number of items */
            object_absorb(j_ptr, o_ptr);

            /* All done */
            return (slot);
        }
    }

    /* No space? */
    if (st_ptr->stock_num >= st_ptr->stock_size) return (-1);


    /* Determine the "value" of the object */
    value = object_value(o_ptr);

    /* Check existing slots to see if we must "slide" */
    for (slot = 0; slot < st_ptr->stock_num; slot++)
    {
        /* Get that object */
        j_ptr = &st_ptr->stock[slot];

        /* Hack -- readable books always come first */
        if ((o_ptr->tval == cp_ptr->spell_book) &&
            (j_ptr->tval != cp_ptr->spell_book)) break;
        if ((j_ptr->tval == cp_ptr->spell_book) &&
            (o_ptr->tval != cp_ptr->spell_book)) continue;

        /* Objects sort by decreasing type */
        if (o_ptr->tval > j_ptr->tval) break;
        if (o_ptr->tval < j_ptr->tval) continue;

        /* Can happen in the home */
        if (!o_ptr->is_flavor_known()) continue;
        if (!j_ptr->is_flavor_known()) break;

        /* Objects sort by increasing sval */
        if (o_ptr->sval < j_ptr->sval) break;
        if (o_ptr->sval > j_ptr->sval) continue;

        /* Objects in the home can be unknown */
        if (!o_ptr->is_flavor_known()) continue;
        if (!j_ptr->is_flavor_known()) break;

        /* Objects sort by decreasing value */
        j_value = object_value(j_ptr);
        if (value > j_value) break;
        if (value < j_value) continue;
    }

    /* Slide the others up */
    for (i = st_ptr->stock_num; i > slot; i--)
    {
        /* Hack -- slide the objects */
        st_ptr->stock[i].object_copy(&st_ptr->stock[i-1]);
    }

    /* More stuff now */
    st_ptr->stock_num++;

    /* Hack -- Insert the new object */
    st_ptr->stock[slot].object_copy(o_ptr);

    /* Return the location */
    return (slot);
}


/*
 * Add an object to a real stores inventory.
 *
 * If the object is "worthless", it is thrown away (except in the home).
 *
 * If the object cannot be combined with an object already in the inventory,
 * make a new slot for it, and calculate its "per item" price.  Note that
 * this price will be negative, since the price will not be "fixed" yet.
 * Adding an object to a "fixed" price stack will not change the fixed price.
 *
 * In all cases, return the slot (or -1) where the object was placed
 */
static int store_carry(int st, object_type *o_ptr)
{
    int i, slot;
    u32b value, j_value;
    object_type *j_ptr;

    store_type *st_ptr = &store[st];

    /* Evaluate the object */
    value = object_value(o_ptr);

    /* Cursed/Worthless items "disappear" when sold */
    if (value <= 0) return (-1);

    /* Erase the inscription & pseudo-ID bit */
    o_ptr->inscription.clear();

    // Erase the object settings
    o_ptr->settings_erase();

    /* Some item types require maintenance */
    switch (o_ptr->tval)
    {
        /* Refuel lights to the standard amount */
        case TV_LIGHT:
        {
            if (o_ptr->sval == SV_LIGHT_TORCH)
                o_ptr->timeout = DEFAULT_TORCH;

            else if (o_ptr->sval == SV_LIGHT_LANTERN)
                    o_ptr->timeout = DEFAULT_LAMP;

            break;
        }

        /* Recharge rods */
        case TV_ROD:
        {
            o_ptr->timeout = 0;
            break;
        }

        /* recharge wands and staves */
        case TV_STAFF:
        case TV_WAND:
        {
            int charges = o_ptr->pval;

            o_ptr->pval = 0;

            /* Calculate the recharged number of charges */
            for (i = 0; i < o_ptr->number; i++) recharge_staff_wand(o_ptr, 60);

            /* Use recharged value only if greater */
            if (charges > o_ptr->pval) o_ptr->pval = charges;
        }
    }


    /* Remove special inscription, if any */
    if (o_ptr->discount >= INSCRIP_NULL) o_ptr->discount = 0;


    /* Check each existing object (try to combine) */
    for (slot = 0; slot < st_ptr->stock_num; slot++)
    {
        /* Get the existing object */
        j_ptr = &st_ptr->stock[slot];

        /* Can the existing items be incremented? */
        if (store_object_similar(j_ptr, o_ptr))
        {
            /* Absorb (some of) the object */
            store_object_absorb(j_ptr, o_ptr);

            /* All done */
            return (slot);
        }
    }

    /* No space? */
    if (st_ptr->stock_num >= st_ptr->stock_size) return (-1);


    /* Check existing slots to see if we must "slide" */
    for (slot = 0; slot < st_ptr->stock_num; slot++)
    {
        /* Get that object */
        j_ptr = &st_ptr->stock[slot];

        /* Hack -- readable books always come first */
        if ((o_ptr->tval == cp_ptr->spell_book) &&
            (j_ptr->tval != cp_ptr->spell_book)) break;
        if ((j_ptr->tval == cp_ptr->spell_book) &&
            (o_ptr->tval != cp_ptr->spell_book)) continue;

        /* Objects sort by decreasing type */
        if (o_ptr->tval > j_ptr->tval) break;
        if (o_ptr->tval < j_ptr->tval) continue;

        /* Objects sort by increasing sval */
        if (o_ptr->sval < j_ptr->sval) break;
        if (o_ptr->sval > j_ptr->sval) continue;

        /* Evaluate that slot */
        j_value = object_value(j_ptr);

        /* Objects sort by decreasing value */
        if (value > j_value) break;
        if (value < j_value) continue;
    }

    /* Slide the others up */
    for (i = st_ptr->stock_num; i > slot; i--)
    {
        /* Hack -- slide the objects */
        st_ptr->stock[i].object_copy(&st_ptr->stock[i-1]);
    }

    /* More stuff now */
    st_ptr->stock_num++;

    /* Hack -- Insert the new object */
    st_ptr->stock[slot].object_copy(o_ptr);

    /* Return the location */
    return (slot);
}

/*
 * Increase, by a 'num', the number of an item 'item' in store 'st'.
 * This can result in zero items.
 */
void store_item_increase(int st, int item, int num)
{
    int cnt;
    object_type *o_ptr;

    store_type *st_ptr = &store[st];

    /* Get the object */
    o_ptr = &st_ptr->stock[item];

    /* Verify the number */
    cnt = o_ptr->number + num;
    if (cnt > STORE_MAX_ITEM) cnt = STORE_MAX_ITEM;
    else if (cnt < 0) cnt = 0;
    num = cnt - o_ptr->number;

    /* Save the new number */
    o_ptr->number += num;
}


/*
 * Remove a slot if it is empty, in store 'st'.
 */
void store_item_optimize(int st, int item)
{
    int j;
    object_type *o_ptr;

    store_type *st_ptr = &store[st];

    /* Get the object */
    o_ptr = &st_ptr->stock[item];

    /* Must exist */
    if (!o_ptr->k_idx) return;

    /* Must have no items */
    if (o_ptr->number) return;

    /* One less object */
    st_ptr->stock_num--;

    /* Slide everyone */
    for (j = item; j < st_ptr->stock_num; j++)
    {
        st_ptr->stock[j] = st_ptr->stock[j + 1];
    }

    /* Nuke the final slot */
   st_ptr->stock[j].object_wipe();
}


/*
 * This makes sure that the black market doesn't stock any object that other
 * stores have, unless it is an ego-item or has various bonuses.
 *
 * Based on a suggestion by Lee Vogt <lvogt@cig.mcel.mot.com>.
 */
static bool black_market_ok(object_type *o_ptr)
{
    int i, j;

    /* Ego items are always fine */
    if (o_ptr->is_ego_item()) return (TRUE);

    /* Good items are normally fine */
    if (o_ptr->to_a > 2) return (TRUE);
    if (o_ptr->to_h > 1) return (TRUE);
    if (o_ptr->to_d > 2) return (TRUE);


    /* No cheap items */
    if (object_value(o_ptr) < 10) return (FALSE);

    /* Check the other stores */
    for (i = 0; i < MAX_STORES; i++)
    {
        /* Skip home, the guild, and the black market */
        if (i == STORE_B_MARKET || i == STORE_HOME || i == STORE_GUILD)
            continue;

        /* Check every object in the store */
        for (j = 0; j < store[i].stock_num; j++)
        {
            object_type *j_ptr = &store[i].stock[j];

            /* Compare object kinds */
            if (o_ptr->k_idx == j_ptr->k_idx)
                return (FALSE);
        }
    }

    /* Otherwise fine */
    return (TRUE);
}

/*
 * Keep certain objects (undiscounted only).
 *
 * Note if this list is greatly expanded, the store_maint function
 * could get caught in an eternal loop.  Be mindful of the fixed
 * variable STORE_MAX_KEEP and STORE_MIN_KEEP when making this list.
 */
static bool keep_in_stock(const object_type *o_ptr, int which)
{

    object_kind *k_ptr = &k_info[o_ptr->k_idx];

    if (game_mode == GAME_NPPMORIA) return (FALSE);

    /*Discounted items, ego items, or artifacts don't stay in stock*/
    if (o_ptr->discount) return (FALSE);
    if (o_ptr->art_num) return (FALSE);
    if (o_ptr->ego_num) return (FALSE);

    /* Never in the home */
    if (which == STORE_HOME) return (FALSE);

    /* Analyze the item type */
    switch (k_ptr->tval)
    {
        /* Certain kinds of food is sold there*/
        case TV_FOOD:
        {
            /*only keep in the general store*/
            if (which != STORE_GENERAL) return (FALSE);
            if (k_ptr->sval == SV_FOOD_RATION) return (TRUE);
            return (FALSE);
        }

        /* Non artifact Lite Sources should be kept */
        case TV_LIGHT:
        {
            /*only keep in the general store*/
            if (which != STORE_GENERAL) return (FALSE);
            if (k_ptr->sval == SV_LIGHT_TORCH &&
                o_ptr->timeout > 0) return (TRUE);
            return (FALSE);
        }
        case TV_SHOT:
        case TV_ARROW:
        case TV_BOLT:
        {
            /*only keep in the general store or weaponsmith*/
            if ((which != STORE_GENERAL) && (which != STORE_WEAPON)) return (FALSE);
            if (k_ptr->sval == SV_AMMO_NORMAL)
            {
                /*only normal ammo that isn't enchanted*/
                if ((o_ptr->to_h > 0) || (o_ptr->to_d > 0)) return FALSE;
                return (TRUE);
            }
            return (FALSE);
        }
        case TV_POTION:
        {
            /*only keep in the temple*/
            if (which != STORE_TEMPLE) return (FALSE);
            if (k_ptr->sval == SV_POTION_CURE_CRITICAL) return (TRUE);
            if (k_ptr->sval == SV_POTION_RESTORE_EXP) return (TRUE);
            return (FALSE);
        }
        case TV_SCROLL:
        {
            /*only keep in the alchemy shop*/
            if (which != STORE_ALCHEMY) return (FALSE);
            if (k_ptr->sval == SV_SCROLL_PHASE_DOOR) return (TRUE);
            if (k_ptr->sval == SV_SCROLL_SATISFY_HUNGER) return (TRUE);
            if (k_ptr->sval == SV_SCROLL_IDENTIFY) return (TRUE);
            if (k_ptr->sval == SV_SCROLL_WORD_OF_RECALL) return (TRUE);
            return (FALSE);

        }
        /* Flasks should be kept */
        case TV_FLASK:
        {
            if (which != STORE_GENERAL) return (FALSE);
            return (TRUE);
        }
        case TV_PRAYER_BOOK:
        case TV_MAGIC_BOOK:
        case TV_DRUID_BOOK:
        {
            if (which != STORE_BOOKSHOP) return (FALSE);
            if (k_ptr->sval < SV_BOOK_MIN_GOOD) return (TRUE);
            return (FALSE);
        }

    }
    return (FALSE);
}

/*
 * Return true if all items in store are standard stock items.
 * False if standard items.
 * Used to determine if the store should shuffle inventory or
 * if the shopkeeper should retire.
 */
static int count_nonstandard_inven(int which)
{
    store_type *st_ptr = &store[which];
    int i;
    int counter = 0;

    /* Discount all the items */
    for (i = 0; i < st_ptr->stock_num; i++)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = &st_ptr->stock[i];

        /* Don't count essential stock items*/
        if (keep_in_stock(o_ptr, which)) continue;

        /* We have some non-standard inventory */
        counter++;
    }

    return (counter);
}


/*
 * Delete an object from store 'st', or, if it is a stack, perhaps only
 * partially delete it.
 */
void store_delete_index(int st, int what)
{
    int num;
    object_type *o_ptr;

    store_type *st_ptr = &store[st];

    /* Paranoia */
    if (st_ptr->stock_num <= 0) return;

    /* keep certain items */

    /* Get the object */
    o_ptr = &st_ptr->stock[what];

    /* Determine how many objects are in the slot */
    num = o_ptr->number;

    /* Some stores keep large amounts of certain objects in stock objects*/
    if ((st != STORE_B_MARKET) && (keep_in_stock(o_ptr, st)))
    {
        if (o_ptr->number > 60) num = num / 2;
        return;
    }

    /* Deal with stacks */
    if (num > 1)
    {
        /* Special behaviour for arrows, bolts &tc. */
        switch (o_ptr->tval)
        {
            case TV_SPIKE:
            case TV_SHOT:
            case TV_ARROW:
            case TV_BOLT:
            {
                /* 50% of the time, destroy the entire stack */
                if (randint0(100) < 50 || num < 10)
                    num = o_ptr->number;

                /* 50% of the time, reduce the size to a multiple of 5 */
                else
                    num = randint1(num / 5) * 5 + (num % 5);

                break;
            }

            default:
            {
                /* 50% of the time, destroy a single object */
                if (randint0(100) < 50) num = 1;

                /* 25% of the time, destroy half the objects */
                else if (randint0(100) < 50) num = (num + 1) / 2;

                /* 25% of the time, destroy all objects */
                else num = o_ptr->number;

                /* Hack -- decrement the total charges of staves and wands. */
                if (o_ptr->tval == TV_STAFF || o_ptr->tval == TV_WAND)
                {
                    o_ptr->pval -= num * o_ptr->pval / o_ptr->number;
                }
            }
        }

    }

    /*Wipe the randart if necessary*/
    if (o_ptr->art_num) artifact_wipe(o_ptr->art_num, FALSE);

    /* Delete the item */
    store_item_increase(st, what, -num);
    store_item_optimize(st, what);
}


/*
 * Delete a random object from store 'st', or, if it is a stack, perhaps only
 * partially delete it.
 *
 * This function is used when store maintenance occurs, and is designed to
 * imitate non-PC purchasers making purchases from the store.
 */
static void store_delete_random(int st)
{
    int what;
    store_type *st_ptr = &store[st];
    object_type *o_ptr;

    /* Paranoia */
    if (st_ptr->stock_num <= 0) return;

    /* Pick a random slot */
    what = randint0(st_ptr->stock_num);

    /* Get the object */
    o_ptr = &st_ptr->stock[what];

    if (keep_in_stock(o_ptr, st)) return;

    store_delete_index(st, what);
}

/*
 * Creates a random object and gives it to a store
 * This algorithm needs to be rethought.  A lot.
 *
 */
static void store_create_random(int which)
{
    int tries, k_idx;

    object_type *i_ptr;
    object_type object_type_body;

    /* Activate that store */
    store_type *st_ptr = &store[which];

    /* Paranoia -- no room left */
    if (st_ptr->stock_num >= st_ptr->stock_size) return;

    /* Hack -- consider up to ten items */
    for (tries = 0; tries < 10; tries++)
    {

        s16b magic_level;

        /* Pick a level for object/creation */
        magic_level = 15 + (p_ptr->lev / 2) + rand_int(p_ptr->lev);

        /* Get local object */
        i_ptr = &object_type_body;

        /*wipe the object*/
        i_ptr->object_wipe();

        /*
         * Get the object level.  The object level of 100 is a hack
         * to ensure that all items that are part of the regular store
         * inventory are not out of depth.
         */
        if ((which == STORE_B_MARKET) || (allow_altered_inventory))
        {
            object_level = magic_level;
        }
        else object_level = 100;

        /* Pick a random object */
        k_idx = get_obj_num(object_level);

        /* Handle failure - but this should never happen*/
        if (!k_idx) continue;

        /* Prepare the object */
        object_prep(i_ptr, k_idx);

        /* Apply magic (dis-allow artifacts) */
        apply_magic(i_ptr, magic_level, FALSE, FALSE, FALSE, FALSE);

        /* The object is "fully known" */
        i_ptr->mark_fully_known(FALSE);

        /* Item belongs to a store */
        i_ptr->ident |= IDENT_STORE;

        /* Hack -- Charge lite's */
        if (i_ptr->tval == TV_LIGHT)
        {
            if (i_ptr->sval == SV_LIGHT_TORCH) i_ptr->timeout = FUEL_TORCH / 2;
            if (i_ptr->sval == SV_LIGHT_LANTERN) i_ptr->timeout = FUEL_LAMP / 2;
        }

        /* Remember history */
        object_history(i_ptr, ORIGIN_STORE, 0);

        /* Extract the flags */
        i_ptr->update_object_flags();

        /* Black markets have expensive tastes */
        if ((which == STORE_B_MARKET) && !black_market_ok(i_ptr))
            continue;

        /* No "worthless" items */
        if (object_value(i_ptr) <= 0) continue;

        /* Mass produce and/or Apply discount */
        mass_produce(i_ptr);

        /* Attempt to carry the (known) object */
        (void)store_carry(which, i_ptr);

        /* Definitely done */
        break;
    }

    /* Reset the object level */
    object_level = p_ptr->depth;

}



/*
 * Maintain the inventory at the stores.
 */
void store_maint(int which)
{
    int j;
    int alt_min = 0;

    int old_rating = rating;

    /* Ignore home and guild */
    if ((which == STORE_HOME) || (which == STORE_GUILD)) return;

    /* Activate that store */
    store_type *st_ptr = &store[which];

    /* XXX Prune the black market */
    if (which == STORE_B_MARKET)
    {
        /* Destroy crappy black market items */
        for (j = st_ptr->stock_num - 1; j >= 0; j--)
        {
            object_type *o_ptr = &st_ptr->stock[j];

            /* Destroy crappy items */
            if (!black_market_ok(o_ptr))
            {
                /*Wipe a randart if necessary*/
                if (o_ptr->art_num) artifact_wipe(o_ptr->art_num, FALSE);

                /* Destroy the object */
                store_item_increase(which, j, 0 - o_ptr->number);
                store_item_optimize(which, j);
            }
        }
    }

    /* Choose the number of slots to keep */
    j = st_ptr->stock_num;

    /* Sell a few items */
    if (game_mode == GAME_NPPMORIA)
    {
        /* Sell a few items */
        j = j - randint(STORE_TURNOVER_NPPMORIA);

        /* Never keep more than "STORE_MAX_KEEP" slots */
        if (j > STORE_MAX_KEEP_NPPMORIA) j = STORE_MAX_KEEP_NPPMORIA;

        /* Always "keep" at least "STORE_MIN_KEEP" items */
        if (j < STORE_MIN_KEEP_NPPMORIA) j = STORE_MIN_KEEP_NPPMORIA;
    }
    else
    {
        /* Sell a few items */
        j = j - randint(STORE_TURNOVER_NPPANGBAND);

        /* Never keep more than "STORE_MAX_KEEP" slots */
        if (j > STORE_MAX_KEEP_NPPANGBAND) j = STORE_MAX_KEEP_NPPANGBAND;

        /* Always "keep" at least "STORE_MIN_KEEP" items */
        if (j < STORE_MIN_KEEP_NPPANGBAND) j = STORE_MIN_KEEP_NPPANGBAND;
    }



    /* Count how many items must be kept*/
    alt_min = st_ptr->stock_num - count_nonstandard_inven(which);

    /* Paranoia */
    if (alt_min < 0) alt_min = 0;

    /*
     * Paranoia - the while loop below will lock up game if j
     * is less than the # of "must-keep" items in store
     */
    if (j < alt_min) j = alt_min;

    /* Destroy objects until only "j" slots are left */
    while (st_ptr->stock_num > j) store_delete_random(which);

    /* Choose the number of slots to fill */
    j = st_ptr->stock_num;

    /* Sell a few items */
    if (game_mode == GAME_NPPMORIA)
    {
        /* Buy some more items */
            j = j + randint(STORE_TURNOVER_NPPMORIA);

            /* Never keep more than "STORE_MAX_KEEP" slots */
            if (j > STORE_MAX_KEEP_NPPMORIA) j = STORE_MAX_KEEP_NPPMORIA;

            /* Always "keep" at least "STORE_MIN_KEEP" items */
            if (j < STORE_MIN_KEEP_NPPMORIA) j = STORE_MIN_KEEP_NPPMORIA;
    }
    else
    {
        /* Buy some more items */
            j = j + randint(STORE_TURNOVER_NPPANGBAND);

            // Give the black market a little more inventory
            if (which == STORE_B_MARKET) j += BLACK_MARKET_ADJUST;

            /* Never keep more than "STORE_MAX_KEEP" slots */
            if (j > STORE_MAX_KEEP_NPPANGBAND) j = STORE_MAX_KEEP_NPPANGBAND;

            /* Always "keep" at least "STORE_MIN_KEEP" items */
            if (j < STORE_MIN_KEEP_NPPANGBAND) j = STORE_MIN_KEEP_NPPANGBAND;
    }

    /*
     * Paranoia - should never happen unless the items in
     * the keep_in_stock function is greatly enlarged.
     */
    if (j < alt_min) j = alt_min;

    /* Hack -- prevent "overflow" */
    if (j >= st_ptr->stock_size) j = st_ptr->stock_size - 1;

    /* Calculate if the player gets altered inventory */
    if (randint1(500) < (p_ptr->lev + (p_ptr->max_depth / 2) + altered_inventory_counter))
    {
        allow_altered_inventory = TRUE;

        /* Reduce teh altered inventory counter */
        if (which == STORE_GENERAL)
        {
            if (altered_inventory_counter <= 10) altered_inventory_counter = 0;
            else altered_inventory_counter -= 10;
        }
        else
        {
            if (altered_inventory_counter <= 25) altered_inventory_counter = 0;
            else altered_inventory_counter -= 25;
        }

    }

    /*
     * Paranoia:
     * This should never be false unless a new store isn't set up properly.
     * Note this function sets the allocation table, which must be undone at the bottom.
     */
    if (prep_store_object(which))
    {
        /* Create some new items */
        while (st_ptr->stock_num < j) store_create_random(which);
    }

    /* Re-set all object generation settings and level rating. */
    get_obj_num_hook = NULL;
    get_obj_num_prep();
    object_generation_mode = OB_GEN_MODE_NORMAL;
    rating = old_rating;
    allow_altered_inventory = FALSE;

}



/*
 * Initialize the stores
 */
void store_init(int which)
{
    int k;

    /* Activate that store */
    store_type *st_ptr = &store[which];

    /* Pick an owner */
    st_ptr->owner = (byte)rand_int(z_info->b_max);

    /* Nothing in stock */
    st_ptr->stock_num = 0;

    /* Clear any old items */
    for (k = 0; k < st_ptr->stock_size; k++)
    {
        st_ptr->stock[k].object_wipe();
    }

}


/*
 * Shuffle one of the stores.
 */
void store_shuffle(int which)
{
    int i;

    /* Activate that store */
    store_type *st_ptr = &store[which];

    /* Ignore home & guild*/
    if ((which == STORE_HOME) || (which == STORE_GUILD)) return;

    /* Pick a new owner */
    i = st_ptr->owner;

    while (i == st_ptr->owner)
        i = randint0(z_info->b_max);

    st_ptr->owner = i;

    /* Discount all the items */
    for (i = 0; i < st_ptr->stock_num; i++)
    {
        object_type *o_ptr;

        /* Get the object */
        o_ptr = &st_ptr->stock[i];

        /*don't discount the essential stock items*/
        if (keep_in_stock(o_ptr, which)) continue;

        /* Discount non-discounted items by 40 percent */
        if (o_ptr->discount == 0) o_ptr->discount = 40;

    }
}



/*
 * Buy the item with the given index from the current store's inventory.
 */
void do_cmd_buy(int this_store, cmd_arg args)
{
    int item = args.item;
    int amt = args.number;

    object_type *o_ptr;
    object_type object_type_body;
    object_type *i_ptr = &object_type_body;

    QString o_name;
    int price, item_new;

    store_type *st_ptr;

    st_ptr = &store[this_store];

    /* Get the actual object */
    o_ptr = &st_ptr->stock[item];

    /* Get desired object */
    object_copy_amt(i_ptr, o_ptr, amt);

    /* Ensure we have room */
    if (!inven_carry_okay(i_ptr))
    {
        message(QString("You cannot carry that many items."));
        return;
    }

    /* Describe the object (fully) */
    o_name = object_desc(i_ptr, ODESC_PREFIX | ODESC_FULL);

    /* Extract the price for the entire stack */
    price = price_item(this_store, i_ptr, FALSE) * i_ptr->number;

    if (price > p_ptr->au)
    {
        message(QString("You cannot afford that purchase."));
        return;
    }

    /* Spend the money */
    p_ptr->au -= price;

    /* ID objects on buy */
    identify_object(i_ptr, TRUE);

    /* Combine / Reorder the pack (later) */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    /* Redraw stuff */
    p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT | PR_SIDEBAR_PL | PR_WIN_OBJLIST);

    /* The object no longer belongs to the store */
    i_ptr->ident &= ~(IDENT_STORE);

    /* Message */
    if (one_in_(3)) color_message(ONE_OF(comment_accept), TERM_WHITE);
    message(QString("You bought %1 for %2 gold.") .arg(o_name) .arg((long)price));

    /* Erase the inscription */
    i_ptr->inscription.clear();

    /* Give it to the player */
    item_new = inven_carry(i_ptr);

    /* Message */
    o_name = object_desc(&inventory[item_new], ODESC_PREFIX | ODESC_FULL);

    message(QString("You have %1 (%2).") .arg(o_name) .arg(index_to_label(item_new)));

    /* Hack - Reduce the number of charges in the original stack */
    if (o_ptr->tval == TV_WAND || o_ptr->tval == TV_STAFF)
    {
        o_ptr->pval -= i_ptr->pval;
    }

    /* Remove the bought objects from the store */
    store_item_increase(this_store, item, -amt);
    store_item_optimize(this_store, item);

    /* Store is empty */
    if ((st_ptr->stock_num == 0) || (count_nonstandard_inven(this_store) == 0))
    {
        int i;

        /* Shuffle */
        if (one_in_(STORE_SHUFFLE))
        {
            /* Message */
            message(QString("The shopkeeper retires."));

            /* Shuffle the store */
            store_shuffle(this_store);
            store_maint(this_store);
        }

        /* Maintain */
        else
        {
            /* Message */
            message(QString("The shopkeeper brings out some new stock."));
        }

        /* New inventory */
        for (i = 0; i < 10; ++i)
        {
            /* Maintain the store */
            store_maint(this_store);
        }
    }
}

/*
 * Handle a quest reward selection from the Guild inventory.
 */
void do_cmd_reward(int this_store, cmd_arg args)
{
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
    int item = args.item;
    int amt = args.number;
    QString title = get_title();
    object_type *o_ptr;
    object_kind *k_ptr;
    object_type picked_item;
    QString o_name;
    int item_new;

    store_type *st_ptr;

    /* Paranoia */
    if (this_store != STORE_GUILD)
    {
        message(QString("You are not currently in the guild, %1.") .arg(title));
        return;
    }
    if (!guild_quest_complete())
    {
        message(QString("You are not currently eligible for a quest reward, %1.") .arg(title));
        return;
    }

    st_ptr = &store[STORE_GUILD];

    /* Get the actual object */
    o_ptr = &st_ptr->stock[item];
    k_ptr = &k_info[o_ptr->k_idx];

    o_ptr->mark_fully_known(TRUE);

    /* Mark the history */
    o_ptr->origin_nature = ORIGIN_REWARD;
    o_ptr->origin_r_idx = 0;
    o_ptr->origin_dlvl = q_ptr->base_level;

    /* Get desired object */
    object_copy_amt(&picked_item, o_ptr, amt);

    /* Ensure we have room */
    if ((!inven_carry_okay(&picked_item)) && (o_ptr->tval != TV_GOLD))
    {
        message(QString("You cannot carry that many items, %1.") .arg(title));
        return;
    }

    /* Give it to the player, with gold handled differently than objects */
    if (o_ptr->tval == TV_GOLD)
    {
        o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

        p_ptr->au += o_ptr->pval;
        message(QString("You have been rewarded with %1, %2.") .arg(o_name) .arg(title));
    }
    else
    {
        item_new = inven_carry(&picked_item);

        /* Describe just the result */
        o_name = object_desc(&inventory[item_new], ODESC_PREFIX | ODESC_FULL);

        /* Message */
        message(QString("You have been rewarded with %1 (%2), %3.") .arg(o_name)  .arg(index_to_label(item_new)) .arg(title));
    }

    /*It's an ironman spellbook, so make the spells available. */
    if ((k_ptr->k_flags3 & (TR3_IRONMAN_ONLY)) && (cp_ptr->spell_book == k_ptr->tval))
    {
        byte j;

        /* Extract spells */
        for (j = 0; j < SPELLS_PER_BOOK; j++)
        {
            s16b spell = get_spell_from_list(k_ptr->sval, j);

            /*skip blank spell slots*/
            if (spell == -1) continue;

            /* Remove the Ironman Restriction. */
            p_ptr->spell_flags[spell] &= ~(PY_SPELL_IRONMAN);
        }

        /* Update the spells. */
        p_ptr->update |= PU_SPELLS;
    }

    /* Handle Artifacts */
    if (o_ptr->art_num)
    {
        /*
         * Artifact might not yet be marked as created (if it was chosen from tailored
         * rewards), so now it's the right time to mark it.
         */
        a_info[o_ptr->art_num].a_cur_num = 1;

        int artifact_depth;
        QString note;
        QString shorter_desc;

        /* Get a shorter description to fit the notes file */
        shorter_desc = object_desc(o_ptr, ODESC_BASE);

        /* Build note and write */
        note = (QString("Quest Reward: %1") .arg(shorter_desc));

        /*record the depth where the artifact was created */
        artifact_depth = o_ptr->xtra1;

        write_note(note, artifact_depth);

        /*mark item creation depth as 0, which will indicate the artifact
         *has been previously identified.  This prevents an artifact from showing
         *up on the notes list twice if it has been previously identified.  JG */
        o_ptr->xtra1 = 0;

        /* Process artifact lore */
        if (ARTIFACT_EASY_MENTAL(o_ptr))
        {
            /* Get the lore entry */
            artifact_lore *a_l_ptr = &a_l_list[o_ptr->art_num];

            /* Remember this artifact from now on */
            a_l_ptr->was_fully_identified = TRUE;
        }
    }

    /* Remove the item from the guild before we wipe everything */
    store_item_increase(STORE_GUILD, item, -amt);
    store_item_optimize(STORE_GUILD, item);

    /* The quest is over */
    guild_quest_wipe(TRUE);
    p_ptr->redraw |= (PR_SIDEBAR_PL);
}


/*
 * Retrieve the item with the given index from the home's inventory.
 */
void do_cmd_retrieve(int this_store, cmd_arg args)
{
    int item = args.item;
    int amt = args.number;

    object_type *o_ptr;
    object_type picked_item;
    QString o_name;
    int item_new;

    store_type *st_ptr;

    if (this_store != STORE_HOME)
    {
        message(QString("You are not currently at home."));
        return;
    }

    st_ptr = &store[STORE_HOME];

    /* Get the actual object */
    o_ptr = &st_ptr->stock[item];

    /* Get desired object */
    object_copy_amt(&picked_item, o_ptr, amt);

    /* Ensure we have room */
    if (!inven_carry_okay(&picked_item))
    {
        message(QString("You cannot carry that many items."));
        return;
    }

    /* Distribute charges of wands, staves, or rods */
    distribute_charges(o_ptr, &picked_item, amt);

    /* Give it to the player */
    item_new = inven_carry(&picked_item);

    if (item_new == -1)
    {
        message("Retrieval unsucessful");
        return;
    }

    /* Describe just the result */
    o_name = object_desc(&inventory[item_new], ODESC_PREFIX | ODESC_FULL);

    /* Message */
    message(QString("You have %1 (%2).") .arg(o_name) .arg(index_to_label(item_new)));

    /* Remove the items from the home */
    store_item_increase(STORE_HOME, item, -amt);
    store_item_optimize(STORE_HOME, item);
}



/*
 * Sell an item to the current store.
 */
void do_cmd_sell(int this_store, cmd_arg args)
{
    int item = args.item;
    int amt = args.number;
    object_type sold_item;
    int price, dummy, value;
    QString o_name;

    /* Get the item */
    object_type *o_ptr = object_from_item_idx(item);

    /* Cannot remove cursed objects */
    if ((item >= INVEN_WIELD) && o_ptr->is_cursed())
    {
        message(QString("Hmmm, it seems to be cursed."));
        return;
    }

    /* Check the store wants the items being sold */
    if (!store_will_buy(this_store, o_ptr))
    {
        message(QString("I do not wish to purchase this item."));
        return;
    }

    /* Get a copy of the object representing the number being sold */
    object_copy_amt(&sold_item, o_ptr, amt);


    /* Check if the store has space for the items */
    if (!store_check_num(this_store, &sold_item))
    {
        message(QString("I have not the room in my store to keep it."));
        return;
    }

    if (!get_item_allow(item, VERIFY_SELL)) return;

    price = price_item(this_store, &sold_item, TRUE) * amt;

    /* Get some money */
    p_ptr->au += price;

    /* Combine / Reorder the pack (later) */
    p_ptr->notice |= (PN_COMBINE | PN_REORDER | PN_SORT_QUIVER);

    p_ptr->update |= (PU_BONUS | PU_TORCH | PU_MANA | PU_PLAYER_SCORE);

    /* Redraw stuff */
    p_ptr->redraw |= (PR_WIN_INVENTORY | PR_WIN_EQUIPMENT | PR_SIDEBAR_PL | PR_WIN_OBJLIST);

    /* Get the "apparent" value */
    dummy = object_value(&sold_item) * amt;

    /* Identify original object */
    identify_object(o_ptr, TRUE);

    /* Take a new copy of the now known-about object. */
    object_copy_amt(&sold_item, o_ptr, amt);

    /* The item belongs to the store now */
    sold_item.ident |= IDENT_STORE;
    o_ptr->use_verify[AUTO_WIELD_QUIVER] = FALSE;

    /*
     * Hack -- Allocate charges between those wands, staves, or rods
     * sold and retained, unless all are being sold.
     */
    distribute_charges(o_ptr, &sold_item, amt);

    /* Get the "actual" value */
    value = object_value(&sold_item) * amt;

    /* Get the description all over again */
    o_name = object_desc(&sold_item, ODESC_PREFIX | ODESC_FULL);

    /* Describe the result (in message buffer) */
    message(QString("You sold %1 (%2) for %3 gold.") .arg(o_name) .arg(index_to_label(item)) .arg((long)price));

    /* Analyze the prices (and comment verbally) */
    purchase_analyze(price, value, dummy);

    /* Take the object from the player */
    inven_item_increase(item, -amt);
    inven_item_optimize(item);

    /* The store gets that (known) object */
    store_carry(this_store, &sold_item);
}

/*
 * Stash an item in the home.
 */
void do_cmd_stash(int this_store, cmd_arg args)
{
    int item = args.item;
    int amt = args.number;
    object_type dropped_item;
    object_type *o_ptr = object_from_item_idx(item);
    QString o_name;

    /* Check we are somewhere we can stash items. */
    if (this_store != STORE_HOME)
    {
        message(QString("You are not in your home."));
        return;
    }

    /* Cannot remove cursed objects */
    if ((item >= INVEN_WIELD) && o_ptr->is_cursed())
    {
        message(QString("Hmmm, it seems to be cursed."));
        return;
    }

    /* Get a copy of the object representing the number being sold */
    object_copy_amt(&dropped_item, o_ptr, amt);

    if (!store_check_num(STORE_HOME, &dropped_item))
    {
        message(QString("Your home is full."));
        return;
    }

    /* Distribute charges of wands/staves/rods */
    distribute_charges(o_ptr, &dropped_item, amt);

    /* Describe */
    o_name = object_desc(&dropped_item, ODESC_PREFIX | ODESC_FULL);

    /* Message */
    message(QString("You drop %1 (%2).") .arg(o_name) .arg(index_to_label(item)));

    /* Take it from the players inventory */
    inven_item_increase(item, -amt);
    inven_item_optimize(item);

    /* Let the home carry it */
    home_carry(&dropped_item);
}

/*
 * Flee the store when it overflows.
 */
bool store_overflow(int this_store)
{
    int item = INVEN_PACK;

    object_type *o_ptr = &inventory[item];

    p_ptr->redraw |= (PR_WIN_OBJLIST);

    /* Flee from the store */
    if (this_store != STORE_HOME)
    {
        /* Leave */
        message(QString("Your pack is so full that you flee the store..."));
        return TRUE;
    }

    /* Flee from the home */
    else if (!store_check_num(this_store, o_ptr))
    {
        /* Leave */
        message(QString("Your pack is so full that you flee your home..."));
        return TRUE;
    }

    /* Drop items into the home */
    else
    {
        object_type *i_ptr;
        object_type object_type_body;

        QString o_name;


        /* Give a message */
        message(QString("Your pack overflows!"));

        /* Get local object */
        i_ptr = &object_type_body;

        /* Grab a copy of the object */
        i_ptr->object_copy(o_ptr);

        /* Describe it */
        o_name = object_desc(o_ptr, ODESC_PREFIX | ODESC_FULL);

        /* Message */
        message(QString("You drop %1 (%2).") .arg(o_name) .arg(index_to_label(item)));

        /* Remove it from the players inventory */
        inven_item_increase(item, -255);
        inven_item_describe(item);
        inven_item_optimize(item);

        /* Let the home carry it */
        home_carry(i_ptr);
    }

    return FALSE;
}

