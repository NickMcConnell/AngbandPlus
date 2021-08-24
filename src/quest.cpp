
/* File: quest.c */

/*
 * Copyright (c) 1997 Eytan Zweig, Jeff Greene, Diego Gonzalez
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.  Other copyrights may also apply.
 *
 * This file contains functions used for handling quests by Eytan Zweig
 */

#include "src/npp.h"
#include "src/store.h"
#include "src/storedialog.h"

/*Monsters only appear so deep*/
#define MAX_MIN_DEPTH			76

QString quests_info[] =
{
    /* QUEST_MONSTER*/
    "Monster or Unique Quest",
    /* QUEST_GUARDIAN */
    "Guardian Quest",
    /* QUEST_PIT*/
    "Pit or Nest Quest",
    /* QUEST_WILDERNESS */
    "Wilderness Quest",
    /* QUEST_THEMED_LEVEL*/
    "Level Quest",
    /* QUEST_SLOT_VAULT */
    "Artifact Vault Quest",
    /* QUEST_ARENA_LEVEL */
    "Arena Quest",
    /* QUEST_LABYRINTH_LEVEL */
    "Labyrinth Quest",
    /* QUEST_SLOT_GREATER_VAULT */
    "Greater Vault Quest",
};


/*
 * Return the next guild quest level.
 * Note this function does not handle overflows such as
 * the max_depth of the dungeon.
 */
static int guild_quest_new_level(void)
{
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
    int depth = 0;

    if (guild_quest_level()) return (guild_quest_level());

    /*
     * If the player hasn't left town yet, first quest
     * is at 50 feet..
     */
    if (!p_ptr->quest_depth) return (2);

    depth = p_ptr->quest_depth + QUEST_LEVEL_BOOST;

    /* Consistently apply either 2 or three levels to the max depth */
    if (q_ptr->q_flags & (QFLAG_EXTRA_LEVEL)) depth ++;

    if (depth > MAX_DEPTH) depth = MAX_DEPTH;

    return (depth);
}

/*
 * Name plural names for monsters
 *
 * Originated from PernAngband via EY, re-written to use in NPP
 */
QString plural_aux(QString name)
{

    if (name.contains(" of "))
    {
        int slot = name.indexOf(" of ");
        // Paranoia - to avoid a crash
        if (slot == 0) return (name);
        if (name[slot-1]== 's') name.replace((slot-1), 2, "es");
        else name.insert((slot), "s");
    }
    else if (name.contains( "coins"))
    {
        int slot = name.indexOf("coins");
        name.replace(slot, 1, "Piles of c");
    }
    else if (name.contains("gems"))
    {
        int slot = name.indexOf("gems");
        name.replace(slot, 1, "Piles of g");
    }
    else if (name.contains("Great Wyrm"))
    {
        int slot = name.indexOf("Great Wyrm");
        name.replace(slot, QString("Great Wyrm").length(), "Great Wyrms");
    }
    else if (name.contains("Spawn of"))
    {
        int slot = name.indexOf("Spawn of");
        name.replace(slot, QString("Spawn of").length(), "Spawns of");
    }
    else if (name.contains("Descendant of"))
    {
        int slot = name.indexOf("Descendant of");
        name.replace(slot, QString("Descendant of").length(), "Descendants of ");
    }
    else if (name.contains("Manes")) return (name);
    else if (name.endsWith('u')) return (name);
    else if (name.contains("Yeti")) return (name);
    else if (name.endsWith("ua")) return (name);
    else if (name.endsWith("nee")) return (name);
    else if (name.endsWith("idhe")) return (name);
    else if (name.endsWith('y'))
    {
        name.replace(name.lastIndexOf("y"),1, "ies");
    }
    else if (name.endsWith("ouse"))
    {
        name.replace(name.lastIndexOf("ouse"),4, "ice");
    }
    else if (name.endsWith("lung"))
    {
        name.replace(name.lastIndexOf("lung"),4, "lungen");
    }
    else if (name.endsWith("sus"))
    {
        name.replace(name.lastIndexOf("sus"),3, "si");
    }
    else if (name.endsWith("star"))
    {
        name.replace(name.lastIndexOf("star"),4, "stari");
    }
    else if (name.endsWith("aia"))
    {
        name.replace(name.lastIndexOf("aia"),3, "aiar");
    }
    else if (name.endsWith("inu"))
    {
        name.replace(name.lastIndexOf("inu"),3, "inur");
    }
    else if (name.endsWith("culus"))
    {
        name.replace(name.lastIndexOf("culus"),5, "culi");
    }
    else if (name.endsWith("sman"))
    {
        name.replace(name.lastIndexOf("sman"),4, "smen");
    }
    else if (name.endsWith("lman"))
    {
        name.replace(name.lastIndexOf("lman"),4, "lmen");
    }
    else if (name.endsWith("ex"))
    {
        name.replace(name.lastIndexOf("ex"),2, "ices");
    }
    else if ((name.endsWith("f")) && (!name.endsWith("ff")))
    {
        name.replace(name.lastIndexOf("f"),1, "ves");
    }
    else if (((name.endsWith("ch")) || (name.endsWith("s"))) && (!name.endsWith("iarch")))
    {
        name.append("es");
    }
    else name.append("s");

    return (name);
}

int quest_collection_num(quest_type *q_ptr)
{
    if (q_ptr->q_type == QUEST_LABYRINTH) 		return (LABYRINTH_COLLECT);
    else if (q_ptr->q_type == QUEST_WILDERNESS) return (WILDERNESS_COLLECT);
    return (0);
}

/*
 * Provide a description of the quest
 */
QString describe_quest(s16b level)
{
    int q_idx = quest_num(level);
    QString buf;
    QString name;
    QString intro;
    QString targets;
    QString where;
    QString race_name;
    QString what;

    quest_type *q_ptr = &q_info[q_idx];
    monster_race *r_ptr = &r_info[q_ptr->mon_idx];

    /*Paranoia*/
    if (guild_quest_level() == 0)
    {
        return ("None");
    }

    /* Completed quest */
    if (guild_quest_complete())
    {
        return  ("Collect your reward!");
    }

    /* Vault quests */
    if (q_ptr->q_type == QUEST_VAULT)
    {
        object_type *i_ptr;
        object_type object_type_body;
        QString o_name;

        /* Get local object */
        i_ptr = &object_type_body;

        /*create a "mock" quest artifact*/
        create_quest_artifact(i_ptr);

        /* Get a shorter description to fit the notes file */
        o_name = object_desc(i_ptr, ODESC_BASE);

        intro = QString("Retrieve the %1 from a vault") .arg(o_name);

        /* The location of the quest */
        where = (QString("at a depth of %1 feet and return to the Guild.") .arg(level * 50));

        buf = (QString("%1 %2") .arg(intro) .arg(where));

        /*we are done*/
        return (buf);
    }

    /* Vault quests */
    else if (q_ptr->q_type == QUEST_GREATER_VAULT)
    {
        if (guild_quest_active())
        {
            intro = (QString("At your current speed, you have %1 turns remaining") .arg(quest_player_turns_remaining()));
            if (!g_vault_name.isEmpty())
            {
                intro.append(" to enter this greater vault.");
            }
            else intro.append(" on this greater vault level.");
        }

        else intro = (QString("Stay on a greater vault level for at least %1 game turns") .arg(GREATER_VAULT_INITIAL_TIME));

        /* The location of the quest */
        where = (QString("at a depth of %1 feet.") .arg(level * 50));

        buf = (QString("%1 %2") .arg(intro) .arg(where));

        /*we are done*/
        return (buf);
    }
    else if (quest_type_collection(q_ptr))
    {
        int remaining = quest_collection_num(q_ptr) - quest_item_count();

        /* Just needs to get the reward */
        if (remaining < 1 )
        {
            return("Collect your reward at the Guild!");
        }
        else
        {
            /*
             * Hack - get the description.  We need to make an object to make sure
             * the plural version works right.
             */
            int k_idx = lookup_kind(TV_PARCHMENT, SV_PARCHMENT_FRAGMENT);
            object_type *i_ptr;
            object_type object_type_body;
            QString o_name;
            i_ptr = &object_type_body;
            i_ptr->object_wipe();
            object_prep(i_ptr, k_idx);
            apply_magic(i_ptr, p_ptr->depth, TRUE, FALSE, FALSE, TRUE);
            i_ptr->number = remaining;
            o_name = object_desc(i_ptr, ODESC_BASE);

            intro = (QString("Retrieve %1 ") .arg(remaining));

            /* The player has some quest items in inventory */
            if ((remaining > 0) && (remaining < quest_collection_num(q_ptr))) intro.append("additional ");

            if (q_ptr->q_type == QUEST_WILDERNESS)
            {
                intro.append(QString("%1 from a wilderness level") .arg(o_name));
            }
            /* (q_ptr->q_type == QUEST_LABYRINTH_LEVEL) */
            else intro.append(QString("%1 from a labyrinth level") .arg(o_name));

            /* The location of the quest */
            where = (QString("at a depth of %1 feet and return to the Guild.") .arg(level * 50));
        }

        buf = (QString("%1 %2") .arg(intro) .arg(where));

        /*we are done*/
        return(buf);
    }

    /* Vault, pit or nest quests */
    if  (quest_multiple_r_idx(q_ptr))
    {
        /*print out a message about a themed level*/
        QString mon_theme;

        intro = "Clear out ";

        /*If the middle of a quest, give the number remaining*/
        if (q_ptr->q_flags & (QFLAG_STARTED))
        {
            /* Completed quest */
            if (guild_quest_complete() && q_ptr->q_reward)
            {
                return("Collect your reward!");
            }
            /* Still active */
            else
            {
                int remaining = q_ptr->q_max_num - q_ptr->q_num_killed;

                if (remaining > 1)
                {
                    intro = (QString("There are %1 remaining creatures from ") .arg(remaining));
                }
                else intro = ("There is 1 remaining creatures from ");
            }
        }

        if (q_ptr->q_type == QUEST_ARENA_LEVEL)
        {
            intro.append("an arena level");
        }
        else
        {
            mon_theme = (QString(feeling_themed_level[q_ptr->q_theme]));

            if (begins_with_vowel(mon_theme)) intro.append("an ");
            else intro.append("a ");

            if (q_ptr->q_type ==  QUEST_THEMED_LEVEL)
            {
                intro.append(QString("%1 stronghold") .arg(mon_theme));
            }
            else if (q_ptr->q_type == QUEST_PIT)
            {
                intro.append(QString("%1 pit") .arg(mon_theme));
            }
            else if (q_ptr->q_type == QUEST_NEST)
            {
                intro.append(QString("%1 nest") .arg(mon_theme));
            }
        }

        /* The location of the quest */
        where = (QString("at a depth of %1 feet.") .arg(level * 50));

        buf = (QString("%1 %2") .arg(intro) .arg(where));

        /*we are done*/
        return(buf);
    }

    /* Not a vault quest, so get the monster race name (singular)*/
    race_name = monster_desc_race(q_ptr->mon_idx);

    if (quest_fixed(q_ptr))
    {
        /* Monster quests */
        targets = race_name;
        intro = ("To fulfill your quest, ");
    }
    else
    {
        /* Monster quests */
        name = race_name;

        /* Multiple quest monsters */
        if ((q_ptr->q_max_num - q_ptr->q_num_killed) > 1)
        {
            name = plural_aux(name);
            targets = (QString("%1 %2") .arg(q_ptr->q_max_num - q_ptr->q_num_killed) .arg(name));
        }

        /* One quest monster */
        else if (r_ptr->flags1 & (RF1_UNIQUE))
        {
            targets = name;
        }
        else
        {
            if (begins_with_vowel(name)) targets = (QString("an %1") .arg(name));
            else targets = (QString("a %1") .arg(name));
        }
    }

    if (monster_nonliving(r_ptr)) what = "destroy";
    else what = "kill";

    /* The type of the quest */
    if (quest_fixed(q_ptr)) intro = "For eternal glory, ";
    else if ((q_ptr->q_type == QUEST_MONSTER) || (q_ptr->q_type == QUEST_GUARDIAN))
    {
        intro = "To complete your ";
        if (q_ptr->q_type == QUEST_GUARDIAN)
        {
            intro.append("guardian ");
        }
        intro.append("quest,");
    }

    /* The location of the quest */
    /* The location of the quest */
    where = (QString("at a depth of %1 feet.") .arg(level * 50));

    /* Output */
    buf = (QString("%1 %2 %3 %4") .arg(intro) .arg(what) .arg(targets) .arg(where));

    /*we are done*/
    return(buf);
}

/*
 * Display the current quest (if any)
 */
void do_cmd_quest_desc(void)
{
    QString quest_desc;

    /* Check if you're on a quest */
    if (guild_quest_level() > 0)
    {
        /* Completed quest */
        if (guild_quest_complete())
        {
            quest_desc = "Collect your reward at the guild!";
        }
        else quest_desc = describe_quest(guild_quest_level());
    }
    /* No quest at all */
    else quest_desc = "You are not currently undertaking a quest.";

    pop_up_message_box(quest_desc);
}


/*
 * A simplified version of object_similar, for guild quest rewards.
 * Just figure out if the objects are the same type.
 * The guild will keep the more valuable one.
 */
static bool guild_object_similar(const object_type *o_ptr, const object_type *j_ptr)
{
    /* Items are identical */
    if (o_ptr == j_ptr) return (TRUE);

    /* They are different objects */
    if (o_ptr->k_idx != j_ptr->k_idx) return (FALSE);

    /* It is possible for artifacts to be created twice in guild reward generation */
    if ((o_ptr->art_num) || (j_ptr->art_num))
    {
        if (o_ptr->art_num == j_ptr->art_num) return (TRUE);

        /* Or else artifacts are never similar*/
        else return (FALSE);
    }

    /* Require identical "ego-item" names */
    if (o_ptr->ego_num != j_ptr->ego_num) return (FALSE);

    /* They match, so they must be similar */
    return (TRUE);
}

/*
 * Helper function for guild carry.  See if we already have a similar item in stock.
 * If the item is better than the one it is replacing, return the replacement slot
 */
static int guild_redundant_item(const object_type *o_ptr)
{
    store_type *st_ptr = &store[STORE_GUILD];
    int slot;

    /* Check each existing object (look for similar items) */
    for (slot = 0; slot < st_ptr->stock_num; slot++)
    {
        /* Get the existing object */
        object_type *j_ptr = &st_ptr->stock[slot];

        /* Found a match */
        if (guild_object_similar(j_ptr, o_ptr)) return (slot);
    }

    /* No match found */
    return (-1);
}


/*
 * Add an potential quest reward to the guild inventory.
 *
 * If there is no room for the item, try to replace a similar, less valued item.
 * If all else fails, just wipe out the lest valuable item in the store and replace that.
 * Return FALSE if there is no place for the object.
 */
static bool guild_carry(object_type *o_ptr)
{
    int i, slot;
    store_type *st_ptr = &store[STORE_GUILD];
    object_type *j_ptr;
    u32b o_value = object_value(o_ptr);
    u32b j_value;

    o_ptr->ident |= IDENT_STORE;
    o_ptr->update_object_flags();

    /* First see if we already have one of these in stock */
    slot = guild_redundant_item(o_ptr);
    if (slot > -1)
    {
        j_ptr = &st_ptr->stock[slot];

        j_value = object_value(j_ptr);

        /* The one already in stock is better */
        if (j_value >= o_value) return (FALSE);

        /* replace the one already in stock */
        /* Not fair to lose an artifact this way */
        if (j_ptr->art_num) a_info[j_ptr->art_num].a_cur_num = 0;
        j_ptr->object_wipe();
        j_ptr->object_copy(o_ptr);

        /* Don't count this one */
        return (FALSE);
    }

    /* Is the guild full? Try to replace the cheapest item*/
    if (st_ptr->stock_num >= st_ptr->stock_size - 1)
    {
        u32b cheapest_object = 0xFFFFFFFFL;
        int cheapest_slot = 0;

        /* Check each existing object (look for similar items) */
        for (slot = 0; slot < st_ptr->stock_num; slot++)
        {
            /* Get the existing object */
            j_ptr = &st_ptr->stock[slot];

            j_value = object_value(j_ptr);

            /* Keep track of the cheapest object */
            if (j_value < cheapest_object)
            {
                cheapest_object = j_value;
                cheapest_slot = slot;
            }
        }

        /* Everything in stock is better than the one we are trying to replace */
        if (o_value <= cheapest_object)
        {
            /* Not fair to lose an artifact this way */
            if (o_ptr->art_num) a_info[o_ptr->art_num].a_cur_num = 0;
            return (TRUE);
        }

        /* Wipe the cheapest item, and put the new on in it's place */
        /* Not fair to lose an artifact this way */
        j_ptr = &st_ptr->stock[cheapest_slot];
        if (j_ptr->art_num) a_info[j_ptr->art_num].a_cur_num = 0;

        /* Delete the item */
        store_item_increase(STORE_GUILD, cheapest_slot, 0 - j_ptr->number);
        store_item_optimize(STORE_GUILD, cheapest_slot);
    }

    /* At this point, we know there is an we should always have an empty slot for the new item */

    /* Check existing slots to see if we must "slide" */
    for (slot = 0; slot < st_ptr->stock_num; slot++)
    {
        /* Get that object */
        j_ptr = &st_ptr->stock[slot];

        /* Objects sort by decreasing type */
        if (o_ptr->tval > j_ptr->tval) break;
        if (o_ptr->tval < j_ptr->tval) continue;

        /* Objects sort by increasing sval */
        if (o_ptr->sval < j_ptr->sval) break;
        if (o_ptr->sval > j_ptr->sval) continue;

        /* Evaluate that slot */
        j_value = object_value(j_ptr);

        /* Objects sort by decreasing value */
        if (o_value > j_value) break;
        if (o_value < j_value) continue;
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
 * Make some gold and place it in the
 * guild for a possible quest reward.
 */
void add_reward_gold(void)
{
    int i;
    int repeats;

    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    object_type *i_ptr;
    object_type object_type_body;

    /* Get local object */
    i_ptr = &object_type_body;
    i_ptr->object_wipe();

    /*ugly, but effective hack - make extra gold, prevent chests & allow artifacts*/
    object_generation_mode = OB_GEN_MODE_QUEST;

    /* Base the gold on the quest level */
    object_level = q_ptr->base_level;

    /* Make initial gold */
    while (!make_gold(i_ptr)) continue;

    /*base amount of gold on fame*/
    repeats = p_ptr->q_fame / 40;

    /*but put in a minimum*/
    if (repeats < 2) repeats = 2;

    /* Give a good gold type for the level */
    for (i = 0; i < repeats; i++)
    {
        object_type *j_ptr;
        object_type object_type_body2;

        /* Get local object */
        j_ptr = &object_type_body2;

        /* Wipe the object */
        j_ptr->object_wipe();

        /* Make more gold */
        while (!make_gold(j_ptr)) continue;

        /* Hack - max out the gold, make sure we don't go over the max for s16b; */
        if ((i_ptr->pval + j_ptr->pval) >= SHRT_MAX)
        {
            i_ptr->pval = SHRT_MAX;
            break;
        }

        /* Combine the gold */
        i_ptr->pval += j_ptr->pval;
    }

    /*undo special item creation*/
    object_generation_mode = OB_GEN_MODE_NORMAL;
    object_level = p_ptr->depth;

    (void)guild_carry(i_ptr);

    return;
}

/*
 * Choose a reward theme.  The lite slot is never returned.  The amulet slot
 * is never returned either, because that is the same theme as a ring.
 */


/*Get the player title, based on the current player fame*/
QString get_title(void)
{

    /* Player's title */
    if (p_ptr->q_fame > 1800)       return("oh glorious one");
    else if (p_ptr->q_fame > 1400) return("oh great one");
    else if (p_ptr->q_fame > 1000)
    {
        if (p_ptr->psex == SEX_MALE) return("my lord");
        else return("my lady");
    }
    else if (p_ptr->q_fame > 700)
    {
        if (p_ptr->psex == SEX_MALE) return("sir");
        else return("madam");
    }
    else if (p_ptr->q_fame > 300)
    {
        if (op_ptr->full_name.isEmpty())
        {
            return(cp_ptr->cl_name);
        }
        else return(op_ptr->full_name);
    }
    else if (p_ptr->q_fame > 50)
    {
        return(rp_ptr->pr_name);
    }
    else return("stranger");

}



QString get_rep_guild(void)
{
    byte attr;

    QString title;

    QString rep =  QString("Your guild reputation is ");

    /* Player's reputation */
    switch (p_ptr->q_fame / 100)
    {
        case 0:
        {
            attr = TERM_RED;
            title = "insubstantial";
            break;
        }
        case 1:
        case 2:
        {
            attr = TERM_RED;
            title = "poor";
            break;
        }

        case 3:
        case 4:
        case 5:
        {
            attr = TERM_ORANGE_PEEL;
            title = "fair";
            break;
        }

        case 6:
        case 7:
        case 8:
        {
            attr = TERM_EARTH_YELLOW;
            title = "good";
            break;
        }

        case 9:
        case 10:
        case 11:
        {
            attr = TERM_EARTH_YELLOW;
            title = "very good";
            break;
        }

        case 12:
        case 13:
        {
            attr = TERM_L_GREEN;
            title = "excellent";
            break;
        }

        case 14:
        case 15:
        {
            attr = TERM_GREEN;
            title = "superb";
            break;
        }

        case 16:
        case 17:
        {
            attr = TERM_BLUE;
            title = "heroic";
            break;
        }
        default:
        {
            attr = TERM_BLUE;
            title = "legendary";
            break;
        }
    }

    rep.append(color_string(title, attr));

    return (rep);
}

QString get_welcome_guild(void)
{
    /*Get the current title*/
    QString title = get_title();

    if (guild_quest_complete())
    {
        return(QString("Collect your reward, %1.") .arg(title));
    }

    /* Introduction */
    return(QString("Welcome to the Adventurer's Guild, %1.") .arg(title));

}

void grant_reward_hp(void)
{

    int max = p_ptr->player_hp[0];
    s16b new_hp[PY_MAX_LEVEL];

    int max_increase = MIN((max * 5 / 2), (z_info->max_level / 2));
    int i, this_increase;

    /* Get level 1 hitdice */
    new_hp[0] = max;

    /*
     * Get the max increases for each level from the player_hp seed.
     * It's easier to work with the data this way.  Also find the current smallest increase.
     */
    for (i = 1; i < z_info->max_level; i++)
    {
        new_hp[i] = (p_ptr->player_hp[i] - p_ptr->player_hp[i - 1]);
    }

    /*
     * Max out HP at the lowest possible levels until we are out.
     * The reward system does not allow this reward if the player
     * is anywhere close to max HP in all levels.
     * for i = 1 because we know 0 is already maxed.
     */
    for (i = 1; i < z_info->max_level; i++)
    {
        /* Already Maxed */
        if (new_hp[i] == max) continue;

        this_increase = max - new_hp[i];

        /*
         * Add HP, but only up until the max increase.
         * Then we are done.
         */
        if (this_increase < max_increase)
        {
            new_hp[i] = max;
            max_increase -= this_increase;
            continue;
        }
        /* Adding the final allotment of HP */
        else
        {
            new_hp[i] = new_hp[i] + max_increase;

            /* We are done. */
            break;
        }
    }

    /*
     * Re-calc the HP seed with the increases.
     */
    for (i = 1; i < z_info->max_level; i++)
    {
        p_ptr->player_hp[i] = p_ptr->player_hp[i-1] + new_hp[i];
    }

    /* Update the hitpoints. */
    p_ptr->update |= PU_HP;
}

/* Try to create a spellbook the player doesn't have as a quest reward */
static void create_reward_spellbook(void)
{
    int i, x;
    object_type *o_ptr;
    object_type *j_ptr;
    object_type object_type_body;
    byte which_realm;
    store_type *st_ptr = &store[STORE_HOME];

    /* Pure fighters don't need spellbooks */
    if (!cp_ptr->spell_book) return;

    /*
     * Greater chance of trying to generate a book if a "pure" spellcaster.
    */
    if (cp_ptr->flags & CF_ZERO_FAIL) i = 40;
    else i = 20;

    if (randint0(100) >= i) return;

    if (cp_ptr->spell_book == TV_MAGIC_BOOK) which_realm = DROP_TYPE_DUNGEON_MAGIC_BOOK;
    else if (cp_ptr->spell_book == TV_PRAYER_BOOK) which_realm = DROP_TYPE_DUNGEON_PRAYER_BOOK;
    else  /* TV_DRUID_BOOK */ which_realm = DROP_TYPE_DUNGEON_DRUID_BOOK;

    /*200 tries at a book*/
    for (i = 0; i < 200; i++)
    {
        bool already_own = FALSE;

        bool interesting = one_in_(2);

        /* Get local object */
        o_ptr = &object_type_body;

        /* Wipe the object */
        o_ptr->object_wipe();

        /*try to make a spellbook, frequently returns nothing*/
        if (!make_object(o_ptr, TRUE, TRUE, which_realm, interesting)) continue;

        /* Paranoia */
        if (!o_ptr->k_idx) continue;

        /* Was this already a reward (marked tried) */
        if (k_info[o_ptr->k_idx].tried) continue;

        /* Look for item in the pack */
        for (x = 0; x < INVEN_PACK; x++)
        {
            /* Get the item */
            j_ptr = &inventory[x];

            /* Nothing here */
            if (!j_ptr->k_idx) continue;

            /*Is it the same spellbook?*/
            if ((j_ptr->tval == o_ptr->tval) && (j_ptr->sval == o_ptr->sval))
            {
                already_own = TRUE;
            }
        }

        /* If necessary, Look for item in home */
        if ((st_ptr->stock_num) && (!already_own))
        {
            for (x = 0; x < st_ptr->stock_num; x++)
            {
                j_ptr = &st_ptr->stock[x];

                /* Nothing here */
                if (!j_ptr->k_idx) continue;

                if ((j_ptr->tval == o_ptr->tval) && (j_ptr->sval == o_ptr->sval))
                {
                    already_own = TRUE;
                }
            }
        }

        /* We already have this book */
        if (already_own)
        {
            /* Hack - Mark as tried */
            k_info[o_ptr->k_idx].tried = TRUE;
            continue;
        }

        /* Add the book to the guild, and we are done. */
        if (guild_carry(o_ptr)) return;
    }

    /* Maybe offer the ironman book as a reward? */
    if ((randint(1000) + 1000) < p_ptr->q_fame)
    {
        /* Search for a suitable ironman spellbook */
        for (i = 1; i < z_info->k_max; i++)
        {
            object_kind *k_ptr = &k_info[i];

            /*Not the right kind of spellbook*/
            if (cp_ptr->spell_book != k_ptr->tval) continue;

            /*Not an ironman book*/
            if (!(k_ptr->k_flags3 & (TR3_IRONMAN_ONLY))) continue;

            /* Book has already been "tried" */
            if (k_ptr->tried) continue;

            /* We now have a suitable spellbook. */

            /* Wipe the object. */
            o_ptr->object_wipe();

            /* Prepare the Spellbook. */
            object_prep(o_ptr, i);

            /* give it to the guild */
            (void)guild_carry(o_ptr);
            return;
        }
    }
}




/*
 * Give a reward to the player
 *
 * Hack - The entire "tailored item" routine makes quite a few assumptions
 * as to what's a good reward, plus it relies a lot of item price to make
 * deductions. It is basically a list of special cases.
 */
static void create_reward_objects(quest_type *q_ptr, byte reward_type)
{
    int x;

    int repeats;

    QString title;

    object_type *o_ptr;
    object_type object_type_body;

    /* Get local object */
    o_ptr = &object_type_body;

    /* Wipe the object */
    o_ptr->object_wipe();

    /* Paranoia */
    if (!reward_type) return;

    /*ugly, but effective hack - make extra gold, prevent chests & allow non-special artifacts*/
    object_generation_mode = OB_GEN_MODE_QUEST;

    title = get_title();

    /* Try a couple things with a tailored reward */
    if (reward_type == REWARD_TAILORED)
    {
        create_reward_spellbook();
    }

    /*
     * Base number for loops on fame and reward quest type,
     * but put in a minimum for each one.
     */
    switch (reward_type)
    {
        case REWARD_TAILORED:
        {
            repeats = (p_ptr->q_fame + p_ptr->deferred_rewards) / 75;
            if (repeats < 3) repeats = 3;
            break;
        }
        case REWARD_GREAT_ITEM:
        {
            repeats = (p_ptr->q_fame + p_ptr->deferred_rewards) / 90;
            if (repeats < 2) repeats = 2;
            break;
        }

        /*good reward*/
        default:
        {
            repeats = (p_ptr->q_fame + p_ptr->deferred_rewards) / 100;
            if (repeats < 5) repeats = 5;
            break;
        }
    }

    /*
     * X is only increased after certain conditions are met
     * Do pre-determined # of attempts at an item
      */
    for (x = 0; x <= repeats;)
    {
        bool do_great = FALSE;
        bool do_good = FALSE;

        /* Wipe the object */
        o_ptr->object_wipe();

        object_level = q_ptr->base_level;

        /*sometimes make good counter true to give 4 artifact rarity rolls*/
        if (reward_type == REWARD_TAILORED)
        {
            do_great = TRUE;
            if (one_in_(3)) do_good = TRUE;
            if (one_in_(5)) object_level += 5;
        }
        else if (reward_type == REWARD_GREAT_ITEM)
        {
            do_great = TRUE;
            if (one_in_(5)) do_good = TRUE;
        }
        /* (reward_type == REWARD_GOOD_ITEM) */
        else do_good = TRUE;

        /* Valid item exists?  If not, don't count it*/
        if (!make_object(o_ptr, do_good, do_great, 0, TRUE)) continue;

        /*
         * Catch_all if we never found anything.  Should never happen.
         */
        if (!o_ptr->k_idx) continue;

        /* If a dungeon spellbook was created, make sure it is the right type */
        if (o_ptr->is_spellbook())
        {
            object_kind *k_ptr = &k_info[o_ptr->k_idx];

            /*Not the right kind of spellbook*/
            if (cp_ptr->spell_book != k_ptr->tval) continue;
        }

        /* Make sure the weapon makes sense for the class */
        if (o_ptr->is_weapon())
        {
            /* Make sure weapon isn't too heavy */
            if (adj_str_hold[p_ptr->state.stat_index[A_STR]] < o_ptr->weight / 10)
            {
                continue;
            }

            /*blessed weapons only for priests*/
            if (cp_ptr->flags & (CF_BLESS_WEAPON))
            {
                if (((o_ptr->tval == TV_SWORD) || (o_ptr->tval == TV_POLEARM)) &&
                     (!is_blessed(o_ptr)))
                {
                    continue;
                }
            }
        }

        o_ptr->update_object_flags();

        /* Make sure gloves won't ruin spellcasting */
        if ((o_ptr->tval == TV_GLOVES) && (cp_ptr->flags & (CF_CUMBER_GLOVE)))
        {
            /* Limit to legal glove types */
            if (!((o_ptr->obj_flags_3 & (TR3_FREE_ACT)) || (o_ptr->obj_flags_1 & (TR1_DEX))))
            {
                continue;
            }
        }

        /* Help the good items a little bit */
        if ((!o_ptr->art_num) && (!o_ptr->ego_num))
        {
            if (o_ptr->to_a > 0)
            {
                o_ptr->to_a += 2 + randint0(3);
            }
            if (o_ptr->to_h > 0)
            {
                o_ptr->to_h += 2 + randint0(5);
            }
            if (o_ptr->to_d > 0)
            {
                o_ptr->to_d += 2 + randint0(4);
            }
        }

        /* Identify it fully */
        o_ptr->mark_fully_known(FALSE);

        /* Give it to the guild */
        if (!guild_carry(o_ptr)) continue;

        /* Success */
        x++;
    }

    /*undo special item level creation*/
    object_generation_mode = FALSE;

}

#define LEV_BOOST 3

/* Helper function to decide if the player should have a quest reward */
static void check_reward_extra_hp(quest_type *q_ptr, int chance)
{
    int cur_hp = p_ptr->player_hp[z_info->max_level-1];
    int max_hp = ((z_info->max_level * (p_ptr->hitdie - 1) * 5) / 8) + z_info->max_level + (p_ptr->hitdie * 3);
    int hp_left = max_hp - cur_hp;

    /* HP rewards are maxed out */
    if (hp_left < 1) return;

    if (randint(chance) > (p_ptr->q_fame + p_ptr->deferred_rewards)) return;

    if (!one_in_(3)) return;

    if (randint1(hp_left) < p_ptr->hitdie) return;

    q_ptr->q_reward |= (REWARD_INC_HP);
}

/* Helper function to decide if the player should have a quest reward */
static void check_reward_stat_increase(quest_type *q_ptr, int chance)
{
    int x, i;

    /*clear the counter*/
    x = 0;

    if (one_in_(3)) return;

    if (randint0(chance) > (p_ptr->q_fame + p_ptr->deferred_rewards)) return;

    /* Add up the total quest_stats given so far */
    for (i = 0; i < A_MAX; i++)
    {
        /*Record this stat*/
        x += p_ptr->stat_quest_add[i];
    }

    /* Small chance of augmentation as a reward */
    if ((randint1(8) > x ) && (one_in_(5))) q_ptr->q_reward |= (REWARD_AUGMENTATION);

    else if (randint1(10) > x ) q_ptr->q_reward |= (REWARD_INC_STAT);

    return;
}

/* Helper function to decide if the player should have a custom quest reward */
static void check_reward_custom_randart(quest_type *q_ptr, int chance, int dice)
{
    if (birth_no_artifacts) return;
    if (birth_no_xtra_artifacts) return;

    if (chance + damroll(dice,225) > (p_ptr->q_fame + p_ptr->deferred_rewards)) return;

    if (one_in_(3)) return;

    q_ptr->q_reward |= (REWARD_RANDART);
}


/*
 * Actually give the character a fixed or monster quest.
 * If bool fixed it true, a fixed quest is placed.
 */
static bool place_mon_quest(int lev, bool fixed)
{
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
    monster_lore *l_ptr;

    int i;

    monster_race *r_ptr;

    int r_idx;

    alloc_entry *table = alloc_race_table;

    long value;

    int total = 0;

    int max_depth, min_depth;
    u32b max_diff, min_diff, max_diff_unique, min_diff_unique;

    /* Monsters can be up to 3 levels out of depth */
    max_depth = LEV_BOOST + lev;

    /*assumes lev is always greater than 0*/
    min_depth = lev;

    /* Fixed quests use deeper monsters */
    if (fixed)
    {
        max_depth += 6;
        min_depth += 6;
    }

    /*don't make it too easy if the player isn't diving very fast*/
    if (lev < p_ptr->max_lev)
    {
        max_depth += ((p_ptr->max_lev - lev) / 2);
        min_depth += ((p_ptr->max_lev - lev) / 2);
    }

    /*boundry control*/
    if (max_depth > (MAX_DEPTH - 15)) max_depth = MAX_DEPTH - 15;
    if (min_depth < 1) min_depth = 1;
    if (min_depth > MAX_MIN_DEPTH) min_depth = MAX_MIN_DEPTH;

    /*get the average difficulty spanning 5 levele for monsters*/
    max_diff = min_diff = min_diff_unique = max_diff_unique = 0;

    /*first get the total of the 5 levels*/
    for (i = 0; i < 5; i++)
    {
        min_diff += mon_power_ave[i + min_depth][CREATURE_NON_UNIQUE];
        min_diff_unique += mon_power_ave[i + min_depth][CREATURE_UNIQUE];

        /*put some boundry control on the highest level*/
        max_diff += mon_power_ave[i + min_depth + LEV_BOOST][CREATURE_NON_UNIQUE];
        max_diff_unique += mon_power_ave[i + min_depth + LEV_BOOST][CREATURE_UNIQUE];
    }

    /*now get the average*/
    min_diff /= 5;
    max_diff /= 5;
    min_diff_unique /= 5;
    max_diff_unique /= 5;

    /*hack - make sure there is a suitable range*/
    while ((min_diff * 10 / 9) > max_diff)
    {
        min_diff = min_diff * 19 / 20;
        max_diff = max_diff * 21 / 20;
    }
    while ((min_diff_unique * 10 / 9) > max_diff_unique)
    {
        min_diff_unique = min_diff_unique * 19 / 20;
        max_diff_unique = max_diff_unique * 21 / 20;
    }

    /*
     * Process the probabilities
     */
    for (i = 0; i < alloc_race_size; i++)
    {
        /* Default */
        table[i].prob3 = 0;

        /* Get the "r_idx" of the chosen monster */
        r_idx = table[i].index;

        /* Get the actual race */
        r_ptr = &r_info[r_idx];

        /*A little bit of level control to keep out extreme monsters*/

        /*enforce a maximum depth*/
        if (r_ptr->level > max_depth + 5) continue;
        /*enforce a minimum depth*/
        if (r_ptr->level < min_depth - 5) continue;

        /*no player ghosts*/
        if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) continue;

        /*no mimics */
        if (r_ptr->flags1 & (RF1_CHAR_MIMIC)) continue;

        /* Uniques only for unique quests*/
        if (r_ptr->flags1 & (RF1_UNIQUE))
        {
            /*get the right difficulty*/
            if (r_ptr->mon_power < min_diff_unique) continue;
            if (r_ptr->mon_power > max_diff_unique) continue;

            /* no dead ones*/
            if (r_ptr->cur_num >= r_ptr->max_num) continue;

            /* Not for fixed quests */
            if (fixed) continue;
        }
        /*no uniques for monster quests*/
        else
        {
            /*get the right difficulty*/
            if (r_ptr->mon_power < min_diff) continue;
            if (r_ptr->mon_power > max_diff) continue;

            /* Check if an immobile monster can still hurt the player from a distance */
            if (r_ptr->flags1 & RF1_NEVER_MOVE)
            {
                bool okay = FALSE;

                /* Allow if the monster has at least one summon spell*/
                if (r_ptr->flags7)	okay = TRUE;

                /* Allow if the monster can attack */
                if ((r_ptr->flags4 & (RF4_ATTACK_MASK | RF4_BREATH_MASK)) ||
                    (r_ptr->flags5 & (RF5_ATTACK_MASK | RF5_BREATH_MASK)) ||
                    (r_ptr->flags5 & (RF6_ATTACK_MASK | RF6_BREATH_MASK)) ||
                    (r_ptr->flags6 & (RF7_ATTACK_MASK | RF7_BREATH_MASK)))
                    okay = TRUE;

                /* Allow if the monster can bring the player to itself */
                if  (r_ptr->flags6 & (RF6_TELE_TO))
                        okay = TRUE;

                if (!okay) continue;
            }

        }

        /* Depth Monsters never appear in quests*/
        if (r_ptr->flags1 & (RF1_FORCE_DEPTH)) continue;

        /* Accept */
        table[i].prob3 = table[i].prob2;

        /* Total */
        total += table[i].prob3;
    }

    /*no eligible creatures - should never happen*/
    if (!total)
    {
        /* No monsters - no quest */
        message(QString("There are no eligible creatures to quest for"));
        return (FALSE);

    }

    /* Select the monster from eligible monsters */

    /* Pick a monster */
    value = rand_int(total);

    /* Find the monster */
    for (i = 0; i < alloc_race_size; i++)
    {
        /* Found the entry */
        if (value < table[i].prob3) break;

        /* Decrement */
        value = value - table[i].prob3;
    }

    /* Set up the quest */
    q_ptr->mon_idx = table[i].index;
    q_ptr->q_num_killed = 0;
    q_ptr->base_level = lev;

    /* Get the actual race */
    r_ptr = &r_info[q_ptr->mon_idx];
    l_ptr = &l_list[q_ptr->mon_idx];

    /* Mark it as seen at least once */
    if (l_ptr->sights < 1 ) l_ptr->sights = 1;

    /*
     * This info slightly different depending on if
     * this is a unique quest or monster quest
     */
    if (r_ptr->flags1 & (RF1_UNIQUE))
    {
        q_ptr->q_max_num = 1;
        q_ptr->q_type = QUEST_MONSTER;
    }
    else
    {
        s16b num;

        /*the number of quest monsters is based around 10, depending on monster power*/
        num = ((min_diff + max_diff) * 5) / r_ptr->mon_power;

        /*higher number of monsters based on player fame*/
        num += p_ptr->q_fame / 100;

        /*boundry control*/
        if (num < 4) num = 4;
        if (num > 60) num = 60;

        /*just a touch of randomness*/
        num += randint(3);

        /* Fixed have more monsters */
        if (fixed) num += 3 + randint1(2) + randint1(2);

        /*assign the number*/
        q_ptr->q_max_num = num;

        /*assign the quest type*/
        if (fixed)
        {
            q_ptr->q_type = QUEST_GUARDIAN;
            q_ptr->q_fame_inc = 15;
        }
        else
        {
            q_ptr->q_type	= QUEST_MONSTER;
            q_ptr->q_fame_inc = 10;
        }

    }

    /* Add various possible rewards */
    check_reward_custom_randart(q_ptr, 200, 10);
    check_reward_extra_hp(q_ptr, 500);
    check_reward_stat_increase(q_ptr, 500);

    /* Try for a tailored award */
    if (25 + damroll(40,20) < (p_ptr->q_fame + p_ptr->deferred_rewards)) q_ptr->q_reward |= REWARD_TAILORED;

    else
    {
        /* Chance of good item reward */
        int chance = 95 - ((p_ptr->q_fame + p_ptr->deferred_rewards) / 5) - (lev *  2);

        /* Better rewards for unique or fixed quests */
        if (r_ptr->flags1 & (RF1_UNIQUE)) chance -= 10;
        if (fixed) chance -= 10;

        if (rand_int(100) > chance)
        {
            q_ptr->q_reward |= REWARD_GREAT_ITEM;
        }

        else if (one_in_(2)) q_ptr->q_reward |= REWARD_GOOD_ITEM;

        /* Offer gold as a last resort */
        else q_ptr->q_reward |= REWARD_GOLD;

    }

    /*success*/
    return (TRUE);
}

/*
 * Make sure a quest will be a challenge by making sure we aren't assigning a
 * monster theme where all the monsters are already in depth.
 */
static bool check_pit_nest_depth(int lev, byte theme)
{
    bool return_value = FALSE;

    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    int i, max_depth;

    alloc_entry *table = alloc_race_table;

    /*set the hook*/
    get_mon_hook(theme);

    /* Prepare allocation table */
    get_mon_num_prep();

    max_depth = lev + PIT_NEST_QUEST_BOOST;

    /*Monsters only go so high*/
    if (lev > MAX_MIN_DEPTH) lev = MAX_MIN_DEPTH;

    /* Go through each one */
    for (i = 0; i < alloc_race_size; i++)
    {
        monster_race *r_ptr;

        int r_idx;

        /* Hack -- No town monsters are acceptable */
        if (table[i].level <= 0) continue;

        /* Get the "r_idx" of the chosen monster */
        r_idx = table[i].index;

        /* Get the actual race */
        r_ptr = &r_info[r_idx];

        /*No player ghosts*/
        if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) continue;

        /*Pits or nests do not allow uniques*/
        if ((q_ptr->q_type == QUEST_NEST) || (q_ptr->q_type == QUEST_PIT))
        {
            if (r_ptr->flags1 & (RF1_UNIQUE)) continue;
        }

        /* Make sure there is at least one monster out of depth. */
        if ((table[i].level > lev) && (table[i].level < max_depth))
        {
            return_value = TRUE;

            /*No need to continue, we only need one*/
            break;
        }

    }

    /* Remove restriction */
    get_mon_num_hook = NULL;

    /* Prepare allocation table */
    get_mon_num_prep();

    return (return_value);

}



/*
 * Make sure a quest will be a challenge by making sure we aren't assigning a
 * monster theme where all the monsters are already in depth.
 */
static bool check_theme_depth(int lev, byte theme)
{
    bool out_of_depth_mon = FALSE;

    int potential_monsters = 0;
    int i;
    int max_depth, min_depth;
    u32b max_diff, max_diff_unique;

    alloc_entry *table = alloc_race_table;

    /*set the hook*/
    get_mon_hook(theme);

    /* Prepare allocation table */
    get_mon_num_prep();

    /* Factor in the boost */
    max_depth = lev + THEMED_LEVEL_QUEST_BOOST;
    min_depth = lev;

    /*don't make it too easy if the player isn't diving very fast*/
    if (p_ptr->depth < p_ptr->max_lev)
    {
        min_depth += ((p_ptr->max_lev - p_ptr->depth) / 2);
        max_depth += ((p_ptr->max_lev - p_ptr->depth) / 2);
    }

    /*boundry control*/
    if (max_depth > (MAX_DEPTH - 15)) max_depth = MAX_DEPTH - 15;
    if (min_depth > MAX_MIN_DEPTH) min_depth = MAX_MIN_DEPTH;

    /*get the average difficulty spanning 5 levels for monsters*/
    max_diff = max_diff_unique = 0;

    /*first get the total of the 5 levels*/
    for (i = 0; i < 5; i++)
    {
        /*put some boundry control on the highest level*/
        max_diff += mon_power_ave[max_depth + i][CREATURE_NON_UNIQUE];
        max_diff_unique += mon_power_ave[max_depth + i][CREATURE_UNIQUE];
    }

    /*now get the average*/
    max_diff /= 5;
    max_diff_unique /= 5;

    /* Go through each one */
    for (i = 0; i < alloc_race_size; i++)
    {
        monster_race *r_ptr;

        int r_idx;

        /* Hack -- No town monsters are acceptable */
        if (table[i].level <= 0) continue;

        /* Get the "r_idx" of the chosen monster */
        r_idx = table[i].index;

        /* Get the actual race */
        r_ptr = &r_info[r_idx];

        /*enforce a maximum depth*/
        if (r_ptr->level > max_depth) continue;

        /*No player ghosts*/
        if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) continue;

        /* Uniques only for unique quests*/
        if (r_ptr->flags1 & (RF1_UNIQUE))
        {
            /*get the right difficulty*/
            if (r_ptr->mon_power > max_diff_unique) continue;
            if (r_ptr->mon_power < mon_power_ave[min_depth][CREATURE_UNIQUE]) continue;

            /* no dead ones*/
            if (r_ptr->cur_num >= r_ptr->max_num) continue;

            /*Make sure it is reasonably hard*/

        }
        /*other monsters based on difficulty*/
        else
        {
            /*get the right difficulty*/
            if (r_ptr->mon_power > max_diff) continue;
            if (r_ptr->mon_power < mon_power_ave[min_depth][CREATURE_NON_UNIQUE]) continue;

        }

        /*Count the number of monsters on the level*/
        potential_monsters += max_themed_monsters(r_ptr, max_diff);

        /*There is at least one monster of the right depth*/
        out_of_depth_mon = TRUE;

    }

    /* Remove restriction */
    get_mon_num_hook = NULL;

    /* Prepare allocation table */
    get_mon_num_prep();

    if (!out_of_depth_mon) return(FALSE);
    if (potential_monsters < (QUEST_THEMED_LEVEL_NUM / 3)) return (FALSE);
    return (TRUE);

}


/*
 * Actually give the character a vault quest
 */
static bool place_pit_nest_quest(int lev)
{

    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
    int chance = ((p_ptr->q_fame + p_ptr->deferred_rewards) / 5) - (lev *  2);
    int tries = 0;
    int i;

    bool checked_theme_yet[LEV_THEME_TAIL];

    /*start with false, meaning we haven't checked it yet*/
    for (i = 0; i < LEV_THEME_TAIL; i++) checked_theme_yet[i] = FALSE;

    /*50% chance of a pit or nest*/
    if one_in_(2) q_ptr->q_type = QUEST_PIT;
    else q_ptr->q_type = QUEST_NEST;
    q_ptr->base_level = lev;
    q_ptr->q_fame_inc = 10;
    q_ptr->q_fame_inc += damroll(10,2);

    /* Decide what types of reward to offer */
    check_reward_custom_randart(q_ptr, 150, 10);
    check_reward_extra_hp(q_ptr, 400);
    check_reward_stat_increase(q_ptr, 400);

    if (30 + damroll(30,25) < (p_ptr->q_fame + p_ptr->deferred_rewards)) q_ptr->q_reward |= REWARD_TAILORED;

    else if (randint(100 < chance)) q_ptr->q_reward |= REWARD_GREAT_ITEM;
    else q_ptr->q_reward |= REWARD_GOOD_ITEM;

    while (TRUE)
    {
        /*
         * We get 5000 tries at this, but it should only fail
         * if somebody wasn't careful while editing around the monster list.
         */
        if ((tries++) > 5000) return (FALSE);

        /*Get the actual theme for either the pit or nest*/
        if (q_ptr->q_type == QUEST_PIT) q_ptr->q_theme = get_pit_theme(lev + 3, TRUE);
        else q_ptr->q_theme = get_nest_theme(lev + 3, TRUE);

        /*hack - never do jelly quests*/
        if (q_ptr->q_theme == LEV_THEME_JELLY) continue;

        /*We don't want to run through check_theme_depth a bunch of times*/
        if (checked_theme_yet[q_ptr->q_theme] == TRUE) continue;

        /*make sure there are hard enough monsters*/
        if (!check_pit_nest_depth(q_ptr->base_level, q_ptr->q_theme))
        {
            /*mark it as checked*/
            checked_theme_yet[q_ptr->q_theme] = TRUE;

            /*next try*/
            continue;
        }

        /*found a good theme*/
        break;
    }

    /*success*/
    return (TRUE);
}

/* Helper function to see if a level has a good level quest. */
static int check_level_quest(void)
{
    int i;

    for (i = 0; i < LEV_THEME_TAIL; i++)
    {

        /*hack - never do jelly quests, or the single element breathing dragons. */
        if (i == LEV_THEME_JELLY) 		continue;
        if (i == LEV_THEME_DRAGON_ACID) continue;
        if (i == LEV_THEME_DRAGON_FIRE) continue;
        if (i == LEV_THEME_DRAGON_ELEC) continue;
        if (i == LEV_THEME_DRAGON_COLD) continue;
        if (i == LEV_THEME_DRAGON_POIS) continue;

        /* There is at least one good theme. */
        if (check_theme_depth(guild_quest_new_level(), i)) return (TRUE);
    }

    return (FALSE);
}

/*
 * Actually give the character a themed level quest
 */
static bool place_level_quest(int lev)
{

    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    int i;
    int tries = 0;

    bool checked_theme_yet[LEV_THEME_TAIL];

    /*start with false, meaning we haven't checked it yet*/
    for (i = 0; i < LEV_THEME_TAIL; i++) checked_theme_yet[i] = FALSE;

    /* Actually write the quest */
    q_ptr->q_type = QUEST_THEMED_LEVEL;
    q_ptr->base_level = lev;
    q_ptr->q_fame_inc = 15;
    q_ptr->q_fame_inc += damroll(10,2);

    /* Decide on what type of reward to give the player */
    check_reward_custom_randart(q_ptr, 150, 8);
    check_reward_extra_hp(q_ptr, 350);
    check_reward_stat_increase(q_ptr, 350);
    if (25 + damroll(25,25) < (p_ptr->q_fame + p_ptr->deferred_rewards)) q_ptr->q_reward |= REWARD_TAILORED;
    else q_ptr->q_reward |= REWARD_GREAT_ITEM;

    while (TRUE)
    {
        /*
         * We get 5000 tries at this, but it should only fail
         * if somebody wasn't careful while editing around the monster list.
         */
        if ((tries++) > 5000) return (FALSE);

        /*Get the actual theme*/
        q_ptr->q_theme = get_level_theme(lev + THEMED_LEVEL_QUEST_BOOST / 2, TRUE);

        /*hack - never do jelly quests*/
        if (q_ptr->q_theme == LEV_THEME_JELLY) continue;

        /*We don't want to run through check_theme_depth a bunch of times*/
        if (checked_theme_yet[q_ptr->q_theme] == TRUE) continue;

        /*make sure there are hard enough monsters*/
        if (!check_theme_depth(lev, q_ptr->q_theme))
        {
            /*mark it as checked*/
            checked_theme_yet[q_ptr->q_theme] = TRUE;

            /*next try*/
            continue;
        }

        /*found a good theme*/
        break;

    }

    /*success*/
    return (TRUE);
}

/*
 * Actually give the character a wilderness quest
 */
static bool place_wilderness_quest(int lev)
{

    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    /* Actually write the quest */
    q_ptr->q_type = QUEST_WILDERNESS;
    q_ptr->base_level = lev;
    q_ptr->q_fame_inc = 20;
    q_ptr->q_fame_inc += damroll(10,2);

    /* Decide which rewards to offer the player */
    check_reward_custom_randart(q_ptr, 125, 8);
    check_reward_extra_hp(q_ptr, 300);
    check_reward_stat_increase(q_ptr, 300);
    if (25 + damroll(25,20) < (p_ptr->q_fame + p_ptr->deferred_rewards)) q_ptr->q_reward |= REWARD_TAILORED;
    else q_ptr->q_reward |= REWARD_GREAT_ITEM;

    /*success*/
    return (TRUE);
}

/*
 * Actually give the character a labyrinth quest
 */
static bool place_labyrinth_quest(int lev)
{

    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    /* Actually write the quest */
    q_ptr->q_type = QUEST_LABYRINTH;
    q_ptr->base_level = lev;
    q_ptr->q_fame_inc = 25;
    q_ptr->q_fame_inc += damroll(10,2);

    /* Decide which rewards to offer the player */
    check_reward_custom_randart(q_ptr, 150, 8);
    check_reward_extra_hp(q_ptr, 250);
    check_reward_stat_increase(q_ptr, 250);
    if (25 + damroll(20,20) < (p_ptr->q_fame + p_ptr->deferred_rewards)) q_ptr->q_reward |= REWARD_TAILORED;
    else q_ptr->q_reward |= REWARD_GREAT_ITEM;

    /*success*/
    return (TRUE);
}


/*
 * Actually give the character a wilderness quest
 */
static bool place_arena_quest(int lev)
{

    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    /* Actually write the quest */
    q_ptr->q_type = QUEST_ARENA_LEVEL;
    q_ptr->base_level = lev;
    q_ptr->q_max_num = ARENA_MAX_MON;
    q_ptr->q_fame_inc = 40;
    q_ptr->q_fame_inc += damroll(10,2);

    /* Decide on what types of rewards to offer the player */
    check_reward_custom_randart(q_ptr, 75, 6);
    check_reward_stat_increase(q_ptr, 225);
    check_reward_extra_hp(q_ptr, 225);

    if (25 + damroll(15,15) < (p_ptr->q_fame + p_ptr->deferred_rewards)) q_ptr->q_reward |= REWARD_TAILORED;
    else q_ptr->q_reward |= REWARD_GREAT_ITEM;

    /*success*/
    return (TRUE);
}

/*
 * Actually give the character a vault quest
 */
static bool place_vault_quest(int lev)
{

    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    /* Actually write the quest */
    q_ptr->q_type = QUEST_VAULT;
    q_ptr->base_level = lev;
    q_ptr->q_fame_inc = 50;

    /* Decide what types of rewards to give */
    check_reward_custom_randart(q_ptr, 60, 7);
    check_reward_extra_hp(q_ptr, 250);
    check_reward_stat_increase(q_ptr, 250);
    if (20 + damroll(25,20) < (p_ptr->q_fame + p_ptr->deferred_rewards)) q_ptr->q_reward |= REWARD_TAILORED;
    else q_ptr->q_reward |= (REWARD_GREAT_ITEM);

    /*generate a quest artifact*/
    make_quest_artifact(lev);

    /*success*/
    return (TRUE);
}

/*
 * Actually give the character a wilderness quest
 */
static bool place_greater_vault_quest(int lev)
{
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
    u16b i;
    u16b vault_choices = 0;
    u16b *vaults;
    u16b min_vault = p_ptr->q_fame;

    vaults = C_ZNEW(z_info->v_max, u16b);

    /* Boundry control */
    if (min_vault > 1750) min_vault = 1750;

    /* Actually write the quest */
    q_ptr->q_type = QUEST_GREATER_VAULT;
    q_ptr->base_level = lev;
    q_ptr->q_fame_inc = 10;

    /* Pick a greater vault, based on player fame */
    for (i = 0; i < z_info->v_max; i++)
    {
        /* Analyze each vault, see if it is worthy of the player */
        vault_type *v_ptr = &v_info[i];
        int ymax = v_ptr->hgt;
        int xmax = v_ptr->wid;
        int t = 0;
        int dx, dy;
        int rating = 0;

        if (v_ptr->typ != 8) continue;

        for (dy = 0; dy < ymax; dy++)
        {
            for (dx = 0; dx < xmax; dx++, t++)
            {
                /* We are only factoring in good and great vaults */
                if (v_ptr->vault_text[t] == '9') rating += 20;
                else if (v_ptr->vault_text[t] == '8') rating += 40;
            }
        }

        /* Make sure the vault is lucrative enough to be worth the player's time */
        if (rating <= min_vault) continue;

        /* Record this one */
        vaults[vault_choices] = i;
        vault_choices++;
    }

    /* Paranoia - should never happen unless somebody has messed up vaults.txt. */
    if (!vault_choices)
    {
        FREE_ARRAY(vaults);
        return (FALSE);
    }

    /* Select the vault */
    i = randint0(vault_choices);
    q_ptr->q_theme = vaults[i];

    FREE_ARRAY(vaults);

    /* For vault quests, the reward is found in the greater vault */
    q_ptr->q_reward |= REWARD_GOLD;

    /*success*/
    return (TRUE);
}


bool quest_allowed(byte j)
{
    /*not a valid quest type*/
    if (j >= QUEST_SLOT_MAX) return (FALSE);

    /*quest vaults not always offered*/
    if (j == QUEST_SLOT_VAULT)
    {
        if (p_ptr->q_fame < 100)return (FALSE);
        if (!(q_info[GUILD_QUEST_SLOT].q_flags & (QFLAG_VAULT_QUEST))) return (FALSE);
    }
    else if (j == QUEST_SLOT_PIT_NEST)
    {
        if (p_ptr->max_depth < 6) return (FALSE);
    }
    else if (j == QUEST_SLOT_LEVEL)
    {
        if (birth_classic_dungeons) return (FALSE);
        if (!allow_themed_levels) return (FALSE);
        if (p_ptr->max_depth < 14) return (FALSE);
        if (!check_level_quest()) return (FALSE);
    }
    else if (j == QUEST_SLOT_GUARDIAN)
    {
        if (p_ptr->max_depth < 3) return (FALSE);
        if (p_ptr->quest_depth > MAX_MIN_DEPTH) return (FALSE);
    }
    else if (j == QUEST_SLOT_WILDERNESS)
    {
        if (birth_classic_dungeons) return (FALSE);
        if (p_ptr->max_depth < 17) return (FALSE);
        if (!(q_info[GUILD_QUEST_SLOT].q_flags & (QFLAG_WILDERNESS_QUEST))) return (FALSE);
    }
    else if (j == QUEST_SLOT_LABYRINTH)
    {
        if (p_ptr->max_depth < 20) return (FALSE);
        if (!(q_info[GUILD_QUEST_SLOT].q_flags & (QFLAG_LABYRINTH_QUEST))) return (FALSE);
    }
    else if (j == QUEST_SLOT_ARENA)
    {
        if (p_ptr->max_depth < 20) return (FALSE);
        if (p_ptr->q_fame < 100) return (FALSE);
        if (!(q_info[GUILD_QUEST_SLOT].q_flags & (QFLAG_ARENA_QUEST))) return (FALSE);
    }
    else if (j == QUEST_SLOT_GREATER_VAULT)
    {
        if (p_ptr->max_depth < 30) return (FALSE);
        if (p_ptr->q_fame < 200) return (FALSE);
        if (!(q_info[GUILD_QUEST_SLOT].q_flags & (QFLAG_GREATER_VAULT_QUEST))) return (FALSE);
    }
    /* Allowable */
    return (TRUE);

}

/*Determine if we can do a quest, based on players current max depth */
bool can_quest_at_level(void)
{
    int i;

    /*Honor the no quests option*/
    if (birth_no_quests) return (FALSE);

    /* No more quests if they are at the bottom of the dungeon. */
    if (guild_quest_new_level() >= MAX_DEPTH) return (FALSE);

    /* Make sure there is no fixed quest on the same level of quests */
    for (i = 0; i < z_info->q_max; i++)
    {
        quest_type *q_ptr = &q_info[i];

        /* check fixed quests to see that they're not on the same level*/
        if (quest_fixed(q_ptr))
        {
            if (q_ptr->base_level == guild_quest_new_level())
            {
                return (FALSE);
            }
        }
    }

    /* Allowable */
    return (TRUE);
}

/*
 * Create magical stairs after finishing certain types of quests.
 */
static void build_quest_stairs(int y, int x)
{
    int ny, nx;

    /* Not on the bottom floor */
    if (p_ptr->depth == (MAX_DEPTH - 1)) return;

    /* Stagger around */
    while (!cave_clean_bold(y, x))
    {

        /* Pick a location */
        scatter(&ny, &nx, y, x, 5, 1);

        /* Stagger */
        y = ny; x = nx;
    }

    /* Destroy any objects */
    delete_object(y, x);

    /* Explain the staircase */
    message(QString("A magical staircase appears..."));

    /* Create stairs down */
    cave_set_feat(y, x, FEAT_STAIRS_DOWN);

    light_spot(y, x);

    /* Update the visuals */
    p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
}

/*
 * Get rid of inventory quest objects on a completed or failed quest.
 * Not intended for VAULT_QUESTS and artifacts
 */
static void remove_quest_objects(void)
{
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
    object_type *o_ptr;
    int j;

    if (!quest_type_collection(q_ptr)) return;

    /* Find quest items in the inventory*/
    for (j = 0; j < INVEN_PACK; j++)
    {
        o_ptr = &inventory[j];

        if (o_ptr->is_quest_object())
        {
            /* Destroy the quest items in the pack */
            inven_item_increase(j, -o_ptr->number);
            inven_item_describe(j);
            inven_item_optimize(j);
        }
    }
}



/*
 * Function to be called when a quest is finished.  Mark the quest complete,
 * Increase the fame.  The player will pick their quest reward later.
 */
void quest_finished(quest_type *q_ptr)
{
    int j;

    q_ptr->q_flags |= (QFLAG_COMPLETED);
    p_ptr->redraw |= (PR_SIDEBAR_PL);

    if (quest_no_down_stairs(q_ptr))
    {
        build_quest_stairs(p_ptr->py, p_ptr->px);
    }

    /* For a fixed quest, we are done */
    if (quest_fixed(q_ptr))
    {
        /* Handle and handle stuff */
        notice_stuff();
        handle_stuff();
        return;
    }

    p_ptr->q_fame += q_ptr->q_fame_inc;

    if (quest_single_r_idx(q_ptr))
    {
        altered_inventory_counter += 3;
    }

    else if (quest_multiple_r_idx(q_ptr))
    {
        altered_inventory_counter += 4;
    }
    else if (q_ptr->q_type == QUEST_GREATER_VAULT)
    {
        write_quest_note(TRUE);
        altered_inventory_counter += 15;
    }

    else if (q_ptr->q_type == QUEST_VAULT)
    {
        object_type *o_ptr;
        byte art_num;

        /* Find quest item in the inventory*/
        j = quest_item_slot();

        o_ptr = &inventory[j];

        altered_inventory_counter += 5;

        /* Remember the artifact */
        art_num = o_ptr->art_num;

        /*if using notes file, make a note*/
        write_quest_note(TRUE);

        /* Destroy the quest item in the pack */
        inven_item_increase(j, -255);
        inven_item_describe(j);
        inven_item_optimize(j);
        artifact_wipe(art_num, TRUE);
    }

    else if (quest_type_collection(q_ptr))
    {
        altered_inventory_counter += 5;

        remove_quest_objects();

        /*if using notes file, make a note*/
        write_quest_note(TRUE);
    }

    p_ptr->update |= (PU_PLAYER_SCORE);

    notice_stuff();
    handle_stuff();
}




/*
 * "Purchase" a quest from the guild
 */
bool guild_purchase(int choice)
{
    int i;
    int qlev = guild_quest_new_level();
    bool quest_placed = TRUE;

    /* Make sure there is no fixed quest on the same level of quests */
    for (i = 0; i < z_info->q_max; i++)
    {
        /* check fixed quests to see that they're not on the same level*/
        if (quest_slot_fixed(i))
        {
            if (q_info[i].base_level == qlev)
            {
                message(QString("A greater task lies before you!"));
                return (FALSE);
            }
        }
    }

    /*place a monster quest*/
    if (choice == QUEST_SLOT_MONSTER)
    {
        if (!place_mon_quest(qlev, FALSE))
        {
            guild_quest_wipe(FALSE);
            quest_placed = FALSE;
        }
    }
    /*Place a vault quest*/
    else if (choice == QUEST_SLOT_VAULT)
    {
        if (!place_vault_quest(qlev))
        {
            guild_quest_wipe(FALSE);
            quest_placed = FALSE;
        }
    }
    else if (choice == QUEST_SLOT_LEVEL)
    {
        if (!place_level_quest(qlev))
        {
            guild_quest_wipe(FALSE);
            quest_placed = FALSE;
        }
    }
    else if (choice == QUEST_SLOT_WILDERNESS)
    {
        if (!place_wilderness_quest(qlev))
        {
            guild_quest_wipe(FALSE);
            quest_placed = FALSE;
        }
    }
    else if (choice == QUEST_SLOT_ARENA)
    {
        if (!place_arena_quest(qlev))
        {
            guild_quest_wipe(FALSE);
            quest_placed = FALSE;
        }
    }
    else if (choice == QUEST_SLOT_LABYRINTH)
    {
        if (!place_labyrinth_quest(qlev))
        {
            guild_quest_wipe(FALSE);
            quest_placed = FALSE;
        }
    }
    /*Nest or Pit quests*/
    else if (choice == QUEST_SLOT_PIT_NEST)
    {
        if (!place_pit_nest_quest(qlev))
        {
            guild_quest_wipe(FALSE);
            quest_placed = FALSE;
        }
    }
    /*place a monster quest*/
    else if (choice == QUEST_SLOT_GUARDIAN)
    {
        if (!place_mon_quest(qlev, TRUE))
        {
            guild_quest_wipe(FALSE);
            quest_placed = FALSE;
        }
    }
    else if (choice == QUEST_SLOT_GREATER_VAULT)
    {
        if (!place_greater_vault_quest(qlev))
        {
            guild_quest_wipe(FALSE);
            quest_placed = FALSE;
        }
    }

    /* Set current quest */
    if (quest_placed)
    {
        quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

        p_ptr->quest_depth = qlev;
        q_ptr->q_flags &= ~(QFLAG_STARTED);

        /* Paranoia - eliminate weaker degrees of object rewards */
        if (q_ptr->q_reward & (REWARD_TAILORED))
        {
            create_reward_objects(q_ptr, REWARD_TAILORED);
        }
        else if (q_ptr->q_reward & (REWARD_GREAT_ITEM))
        {
            create_reward_objects(q_ptr, REWARD_GREAT_ITEM);
        }
        else if (q_ptr->q_reward & (REWARD_GOOD_ITEM))
        {
            create_reward_objects(q_ptr, REWARD_GOOD_ITEM);
        }
        else if (q_ptr->q_reward & (REWARD_GOLD))
        {
            add_reward_gold();
        }
    }

    /* No quest placed */
    else return (FALSE);

    /*
     * Have the next quest be either two or three levels above
     * this one.
     */
    if (one_in_(2) || (q_info[GUILD_QUEST_SLOT].base_level < p_ptr->max_lev))
    {
        q_info[GUILD_QUEST_SLOT].q_flags |= (QFLAG_EXTRA_LEVEL);
    }
    else q_info[GUILD_QUEST_SLOT].q_flags &= ~(QFLAG_EXTRA_LEVEL);

    /* Vault quest allowed 1/5 of the time */
    if (one_in_(5)) q_info[GUILD_QUEST_SLOT].q_flags |= (QFLAG_VAULT_QUEST);
    else q_info[GUILD_QUEST_SLOT].q_flags &= ~(QFLAG_VAULT_QUEST);

    /* Arena quest allowed 1/4 of the time */
    if (one_in_(4)) q_info[GUILD_QUEST_SLOT].q_flags |= (QFLAG_ARENA_QUEST);
    else q_info[GUILD_QUEST_SLOT].q_flags &= ~(QFLAG_ARENA_QUEST);

    /* Wilderness quests allowed 1/4 of the time */
    if (one_in_(4)) q_info[GUILD_QUEST_SLOT].q_flags |= (QFLAG_WILDERNESS_QUEST);
    else q_info[GUILD_QUEST_SLOT].q_flags &= ~(QFLAG_WILDERNESS_QUEST);

    /* Labyrinth quests allowed 1/4 of the time */
    if (one_in_(4)) q_info[GUILD_QUEST_SLOT].q_flags |= (QFLAG_LABYRINTH_QUEST);
    else q_info[GUILD_QUEST_SLOT].q_flags &= ~(QFLAG_LABYRINTH_QUEST);

    /* Greater vault quests allowed 1/5 of the time */
    if (one_in_(5)) q_info[GUILD_QUEST_SLOT].q_flags |= (QFLAG_GREATER_VAULT_QUEST);
    else q_info[GUILD_QUEST_SLOT].q_flags &= ~(QFLAG_GREATER_VAULT_QUEST);

    p_ptr->redraw |= PR_SIDEBAR_PL;

    return (TRUE);
}

/*
 * Hack -- Check if a level is a "quest" level - returns quest type
 */
byte quest_check(int lev)
{
    int i;

    /* Town is never a quest */
    if (!lev) return 0;

    /* No quests in Moria */
    if (game_mode == GAME_NPPMORIA) return (FALSE);

    /* Check quests */
    for (i = 0; i < z_info->q_max; i++)
    {
        /* Check for quest */
        if (q_info[i].base_level == lev)
        {
            if (!is_quest_complete(i)) return (q_info[i].q_type);
        }
    }

    p_ptr->redraw |= (PR_SIDEBAR_PL);

    /* Nope */
    return 0;
}

/*
 * Return the index of the quest for current level
 */
int quest_num(int lev)
{
    int i;

    /* Town is never a quest */
    if (!lev) return 0;

    /* Count incomplete quests */
    for (i = 0; i < z_info->q_max; i++)
    {
        /* Quest level? */
        if (q_info[i].base_level == lev)
        {
            if (!is_quest_complete(i)) return i;
        }
    }

    /* No quest */
    return 0;
}

/*
 * Return the slot of a quest item in the inventory
 */
int quest_item_slot(void)
{
    int i;
    object_type *o_ptr;

    for (i = 0; i < INVEN_PACK; i++)
    {
        o_ptr = &inventory[i];

        if (o_ptr->is_quest_object()) return (i);
    }

    /* No quest item */
    return (-1);
}

/*
 * Return the number of a quest items held in the inventory
 */
int quest_item_count(void)
{
    int i;
    object_type *o_ptr;
    int item_count = 0;

    for (i = 0; i < INVEN_PACK; i++)
    {
        o_ptr = &inventory[i];

        if (o_ptr->is_quest_object())
        {
            item_count += o_ptr->number;
        }
    }

    return (item_count);
}



/*
 * Calculate the time remaining on a greater vault quest.
 * Assumes the quest is currently active.
 */
s32b quest_time_remaining(void)
{
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
    s32b turns_lapsed = p_ptr->game_turn - q_ptr->turn_counter;
    s32b turns_allowed = GREATER_VAULT_INITIAL_TIME + (GREATER_VAULT_BONUS_TIME * q_ptr->q_num_killed);

    /* Paranoia */
    if (!quest_timed(q_ptr)) return 0;
    if (turns_lapsed >= turns_allowed) return 0;

    return (turns_allowed - turns_lapsed);
}

/*
 * Calculate the time remaining on a greater vault quest.
 * Assumes a timed quest is currently active.
 */
s32b quest_player_turns_remaining(void)
{
    s32b time_left = quest_time_remaining();

    s32b turns_left;

    /* Paranoia */
    if (time_left <1) return 0;

    turns_left = time_left * calc_energy_gain(p_ptr->state.p_speed) / 100;

    if (turns_left < 1) return 1;

    return (turns_left);
}



/*
 * Wipe the quest clean.
 */
void guild_quest_wipe(bool reset_defer)
{
    store_type *st_ptr = &store[STORE_GUILD];
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
    byte preserve_mask = (q_ptr->q_flags & (QFLAG_PRESERVE_MASK));
    int i;

    /* Wipe the structure */
    q_ptr->name.clear();
    q_ptr->q_type = 0;
    q_ptr->q_reward = 0;
    q_ptr->q_fame_inc = 0;
    q_ptr->q_theme = 0;
    q_ptr->base_level = 0;
    q_ptr->mon_idx = 0;
    q_ptr->turn_counter = 0;
    q_ptr->q_num_killed = 0;
    q_ptr->q_max_num = 0;

    /* Bring back the flags we didn't want to wipe */
    q_ptr->q_flags = preserve_mask;


    if (reset_defer) p_ptr->deferred_rewards = 0;

    /* Clean out the guild */
    for (i = st_ptr->stock_num; i >= 0; i--)
    {
        /* Get the existing object */
        object_type *o_ptr = &st_ptr->stock[i];

        /* Not fair to lose an artifact this way */
        if (o_ptr->art_num) a_info[o_ptr->art_num].a_cur_num = 0;

        store_delete_index(STORE_GUILD, i);
    }

    /* Paranoia - Wipe the guild inventory */
    st_ptr->stock_num = 0;
    for (i = 0; i < st_ptr->stock_size; i++)
    {
        st_ptr->stock[i].object_wipe();
    }
}

/*
 * Write a note to the file when the quest is over.
 * Success is true if the quest was completed.
 * Success is false if the quest was failed.
 */
void write_quest_note(bool success)
{
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    QString note;

    if (q_ptr->q_type == QUEST_VAULT)
    {
        object_type *i_ptr;
        object_type object_type_body;
        QString o_name;

        /* Get local object */
        i_ptr = &object_type_body;

        /*create a "mock" quest artifact*/
        create_quest_artifact(i_ptr);

        /* Get a shorter description to fit the notes file */
        o_name = object_desc(i_ptr, ODESC_BASE);

        /*create the note*/
        if (success) note = (QString("Quest: Returned %1 to the Adventurer's Guild.") .arg(o_name));
        else note = (QString("Quest: Failed to return %1 to the Guild.") .arg(o_name));

    }

    else if (q_ptr->q_type == QUEST_GREATER_VAULT)
    {
        vault_type *v_ptr = &v_info[q_ptr->q_theme];

        /*create the note*/
        if (success) note = (QString("Quest: Explored the %1.") .arg(v_ptr->vault_name));
        else note = (QString("Quest: Failed to adequately explore the %1.") .arg(v_ptr->vault_name));

        g_vault_name.clear();
    }

    else if (quest_type_collection(q_ptr))
    {
        /*
         * Hack - get the description.  We need to make an object to make sure
         * the plural version works right.
         */
        int k_idx = lookup_kind(TV_PARCHMENT, SV_PARCHMENT_FRAGMENT);
        object_type *i_ptr;
        object_type object_type_body;
        QString o_name;
        i_ptr = &object_type_body;
        i_ptr->object_wipe();
        object_prep(i_ptr, k_idx);
        apply_magic(i_ptr, p_ptr->depth, TRUE, FALSE, FALSE, TRUE);
        i_ptr->number = quest_collection_num(q_ptr);
        o_name = object_desc(i_ptr, ODESC_BASE);

        /*create the note*/
        if (q_ptr->q_type == QUEST_LABYRINTH)
        {
            if (success) note = (QString("Quest: Returned %1 %2 from a labyrinth level to the Adventurer's Guild.") .arg(LABYRINTH_COLLECT) .arg(o_name));
            else note = (QString("Quest: Failed to return %1 %2 from a labyrinth level to the Guild.") .arg(LABYRINTH_COLLECT) .arg(o_name));
        }
        else /* (q_ptr->q_type ==  QUEST_WILDERNESS) */
        {
            if (success) note = (QString("Quest: Returned %1 %2 from a burning wilderness to the Adventurer's Guild.") .arg(WILDERNESS_COLLECT) .arg(o_name));
            else note = (QString("Quest: Failed to return %1 %2 from a burning wilderness to the Guild.") .arg(WILDERNESS_COLLECT) .arg(o_name));;
        }
    }


    else if (quest_multiple_r_idx(q_ptr))
    {
        if (success) note = "Quest: Completed ";
        else note = "Quest: Failed to complete ";


        if (q_ptr->q_type ==  QUEST_ARENA_LEVEL) note.append("an arena quest.");
        else
        {
            QString mon_theme;

            /*Get the theme*/
            mon_theme = feeling_themed_level[q_ptr->q_theme];

            if (success) note = "Quest: Cleared out ";
            else note = "Quest: Failed to clear out ";

            /*make the grammar proper*/
            if (begins_with_vowel(mon_theme)) note.append("an ");
            else note.append("a ");

            /*dump the monster theme*/
            note.append(mon_theme);

            /*Finish off the line*/
            if  (q_ptr->q_type == QUEST_THEMED_LEVEL) 	note.append(" stronghold.");
            else if (q_ptr->q_type == QUEST_PIT)	note.append(" pit.");
            else if (q_ptr->q_type == QUEST_NEST)	note.append(" nest.");
        }

    }

    else
    {
        monster_race *r_ptr = &r_info[q_ptr->mon_idx];
        QString race_name;

        /* Get the monster race name (singular)*/
        race_name = monster_desc_race(q_ptr->mon_idx);

        /* Multiple quest monsters */
        if (q_ptr->q_max_num > 1)
        {
            race_name = plural_aux(race_name);
        }

        if (r_ptr->flags1 == RF1_UNIQUE)
        {
            if (success)
            {
                /*write note*/
                if (monster_nonliving(r_ptr))
                {
                    note = (QString("Quest: Destroyed %1") .arg(race_name));
                }
                else note = (QString("Quest: Killed %1") .arg(race_name));
            }
            else
            {
                /*write note*/
                if (monster_nonliving(r_ptr))
                {
                    note = (QString("Quest: Failed to destroy %1") .arg(race_name));
                }
                else note = (QString("Quest: Failed to kill %1") .arg(race_name));
            }
        }

        else
        {
            if (success)
            {
                /* Write note */
                if (monster_nonliving(r_ptr))
                {
                    note = (QString("Quest: Destroyed %1 %2") .arg(q_ptr->q_max_num) .arg(race_name));
                }
                else note = (QString("Quest: Killed %1 %2") .arg(q_ptr->q_max_num) .arg(race_name));
            }
            else
            {
                /* Write note */
                if (monster_nonliving(r_ptr))
                {
                    note = (QString("Quest: Failed to destroy %1 %2") .arg(q_ptr->q_max_num) .arg(race_name));
                }
                else note = (QString("Quest: Failed to kill %1 %2") .arg(q_ptr->q_max_num) .arg(race_name));
            }
        }
    }

    /*write the note*/
    write_note(note, q_ptr->base_level);
}

/*
 * Fail your quest
 */
void quest_fail(void)
{
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    /* Message */
    message(QString("You have failed in your quest!"));

    /* Show a special mark for some player turns. Redraw if necessary */
    quest_indicator_timer = 50;
    quest_indicator_complete = FALSE;
    p_ptr->redraw |= (PR_SIDEBAR_PL);

    /* Reputation penalty */
    if (p_ptr->q_fame)
    {
        /*Arena quests are only a small decrease, special quests cut the player fame in half*/
        if (q_ptr->q_type == QUEST_ARENA_LEVEL)	p_ptr->q_fame = (p_ptr->q_fame * 8 / 10);
        else if (quest_type_collection(q_ptr))	p_ptr->q_fame = (p_ptr->q_fame * 7 / 10);

        else if (q_ptr->q_type == QUEST_VAULT)	p_ptr->q_fame /= 2;

        /* Failing a pit, nest, wilderness, or themed level quests isn't as bad */
        else if (quest_multiple_r_idx(q_ptr)) p_ptr->q_fame = (p_ptr->q_fame * 65 / 100);

        /* Minimal penalty for greater vaults */
        else if (q_ptr->q_type == QUEST_GREATER_VAULT) p_ptr->q_fame = (p_ptr->q_fame * 9 / 10);

        else
        {
            /* Monster quests */
            p_ptr->q_fame = (p_ptr->q_fame * 45 / 100);
        }
    }

    if (quest_type_collection(q_ptr)) remove_quest_objects();

    /*make a note of the failed quest */
    write_quest_note(FALSE);

    /* No special quests next time */
    q_ptr->q_flags &= ~(QFLAG_PRESERVE_MASK);

    /*wipe the quest*/
    guild_quest_wipe(TRUE);
    p_ptr->update |= (PU_PLAYER_SCORE);

    /* Disturb */
    disturb(FALSE, FALSE);
}

/*
 *  Helper function for format_quest_indicator.
 *  Figure out if we should print the Goto message.
 *  Assumes there is an active quest.
 */

static bool quest_indicator_aux(quest_type *q_ptr)
{
    /* Quest hasn't been started yet */
    if (!guild_quest_started()) return (TRUE);

    /* We are on the current level */
    if (q_ptr->base_level == p_ptr->depth) return (FALSE);

    if (q_ptr->q_type == QUEST_VAULT)
    {
        if (guild_quest_started()) return (FALSE);
        if (quest_item_slot() > -1) return (FALSE);
        return (TRUE);
    }

    if (quest_type_collection(q_ptr))
    {
        if (guild_quest_started()) return (FALSE);
        if (quest_item_count() >= quest_collection_num(q_ptr)) return (FALSE);
        return (TRUE);
    }

    return (TRUE);

}

/*
 * Create the quest indicator string and put it on "dest"
 * Return TRUE if the quest indicator is active
 */
QString format_quest_indicator(byte *attr)
{
    QString dest;

    dest.clear();

    /* Get the current quest, if any */
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    /* No quest */
    if (!q_ptr->q_type)
    {
        //dest = "Qst: None";
    }

    /* Completed Quest! */
    else if (guild_quest_complete())
    {
        dest = "Q:Reward!";
        *attr = TERM_GREEN;
    }

    else if (quest_indicator_aux(q_ptr))
    {
        dest = (QString("Q:Goto %1'") .arg(q_ptr->base_level * 50));

        if (quest_might_fail_now())
        {
            *attr = TERM_RED;
        }
        else *attr = TERM_BLUE;
    }

    /* Special case. Vault quests */
    else if (q_ptr->q_type == QUEST_VAULT)
    {
        /* Get the artifact template */
        artifact_type *a_ptr = &a_info[QUEST_ART_SLOT];

        /* Default character */
        QChar chr = ']';

        if (quest_item_slot() > -1)
        {
            dest = "Q:GoTo Guild!";
            *attr = TERM_GREEN;
            return (dest);
        }

        /* Get the symbol of the artifact */
        if (a_ptr->a_name.length())
        {
            /* Get the object kind */
            s16b k_idx = lookup_kind(a_ptr->tval, a_ptr->sval);

            /* Get the default character (graphics mode not supported) */
            if (k_idx) chr = k_info[k_idx].d_char;
        }

        /* Format */
        dest = (QString("Qst:%1") .arg(chr));
        *attr = TERM_L_RED;
    }

    /* Special case. Labyrinth levels */
    else if (quest_type_collection(q_ptr))
    {
        int k_idx = lookup_kind(TV_PARCHMENT, SV_PARCHMENT_FRAGMENT);
        object_kind *k_ptr = &k_info[k_idx];
        int num = quest_collection_num(q_ptr);

        /* Default character */
        QChar chr = k_ptr->d_char;

        /* Player has the quest item */
        if (quest_item_count() >= num)
        {
            dest = "Q:GoTo Guild!";
            *attr = TERM_GREEN;
            return(dest);
        }

        /* Format */
        dest = (QString("Qst:%1 %2") .arg(chr) .arg(num - quest_item_count()));

        /* Color of the object */
        *attr = k_ptr->color_num;
    }

    /* Special case. Vault quests */
    else if (q_ptr->q_type == QUEST_GREATER_VAULT)
    {
        if (g_vault_name.length()) dest = "Q:Entervault";
        else dest = (QString("Q:%1 turns") .arg(quest_player_turns_remaining()));
    }

    /* Monster, pit/nest or themed level quests */
    else
    {
        int remaining = q_ptr->q_max_num - q_ptr->q_num_killed;
        /* Show the remaining number of monsters */
        dest = (QString("Qst:%1") .arg(remaining));
        if ((q_ptr->q_type == QUEST_MONSTER) || (q_ptr->q_type == QUEST_GUARDIAN))
        {
            QString name = r_info[q_ptr->mon_idx].r_name_short;
            if (remaining > 1) name = plural_aux(name);

            dest.append(QString(" %1") .arg(r_info[q_ptr->mon_idx].r_name_short));
        }
        *attr = TERM_L_RED;
    }

    /* Success */
    return(dest);
}

void quest_status_update(void)
{
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];
    QString race_name;
    QString note;

    int remaining = q_ptr->q_max_num - q_ptr->q_num_killed;

    /*nothing left, notification already printed in monster_death */
    if (guild_quest_complete()) return;

    /* No message */
    if (q_ptr->q_type == QUEST_VAULT) return;

    if (q_ptr->q_type == QUEST_GREATER_VAULT)
    {
        if (guild_quest_active())
        {
            note = (QString("At your current speed, you have %1 turns remaining") .arg(quest_player_turns_remaining()));
            if (g_vault_name.length())
            {
                note.append(" to enter this greater vault.");
            }
            else note.append(" on this greater vault level.");
        }

        /*dump the final note*/
        message(note);

    }

    else if (quest_type_collection(q_ptr))
    {
        remaining = quest_collection_num(q_ptr) - quest_item_count();

        if (remaining > 0)
        {
            /*
             * Hack - get the description.  We need to make an object to make sure
             * the plural version works right.
             */
            int k_idx = lookup_kind(TV_PARCHMENT, SV_PARCHMENT_FRAGMENT);
            object_type *i_ptr;
            object_type object_type_body;
            QString o_name;
            i_ptr = &object_type_body;
            i_ptr->object_wipe();
            object_prep(i_ptr, k_idx);
            apply_magic(i_ptr, p_ptr->depth, TRUE, FALSE, FALSE, TRUE);
            i_ptr->number = remaining;
            o_name = object_desc(i_ptr, ODESC_BASE);

            if (q_ptr->q_type == QUEST_WILDERNESS)
            {
                note = (QString("You have %1 remaining %2 to collect from the wilderness level.") .arg(remaining) .arg(o_name));
            }
            else /* (q_ptr->q_type == QUEST_LABYRINTH) */
            {
                note = (QString("You have %1 remaining %2 to collect from the labyrinth level.") .arg(remaining) .arg(o_name));
            }
        }
        else note = "Collect your reward at the Guild!";


        /*dump the final note*/
        message(note);
    }

    else if (quest_multiple_r_idx(q_ptr))
    {
        if (remaining > 1)
        {
            note = (QString("There are %1 creatures remaining ") .arg(remaining));
        }
        else note = ("There is one creature remaining ");
        if (q_ptr->q_type ==  QUEST_ARENA_LEVEL) note.append("to kill in the arena.");
        else
        {
            note.append(QString("from the %1") .arg(feeling_themed_level[q_ptr->q_theme]));

            if  (q_ptr->q_type ==  QUEST_THEMED_LEVEL) 	note.append(" stronghold.");
            else if (q_ptr->q_type == QUEST_PIT)	note.append(" pit.");
            else if (q_ptr->q_type == QUEST_NEST)	note.append(" nest.");
        }

        /*dump the final note*/
        message(note);
    }
    /* Monster and fixed monster quests */
    else
    {
        /* Get the monster race name (singular)*/
        race_name = monster_desc_race(q_ptr->mon_idx);

        /* Multiple quest monsters */
        if (remaining > 1)
        {
            race_name = plural_aux(race_name);
        }

        note = (QString("You have %1 %2 remaining to complete your ") .arg(remaining) .arg(race_name));

        if (q_ptr->q_type == QUEST_GUARDIAN)
        {
            note.append("guardian ");
        }

        note.append("quest.");

        /*dump the final note*/
        message(note);
    }
}

/* Verify if the quest if a fixed quest found in quest.txt */
bool quest_fixed(const quest_type *q_ptr)
{
    if (q_ptr->q_type == QUEST_PERMANENT) return (TRUE);
    return (FALSE);
}

/* Verify if the quest slot is a fixed quest found in quest.txt */
bool quest_slot_fixed(int quest_num)
{
    quest_type *q_ptr;

    /* Boundry control, then get the quest */
    if (quest_num >= z_info->q_max) return (FALSE);
    if (quest_num < 0) return (FALSE);
    q_ptr = &q_info[quest_num];

    return (quest_fixed(q_ptr));
}

/* Verify if the quest involves clearing multiple races on a single level */
bool quest_multiple_r_idx(const quest_type *q_ptr)
{
    if (q_ptr->q_type == QUEST_THEMED_LEVEL) return (TRUE);
    if (q_ptr->q_type == QUEST_PIT) return (TRUE);
    if (q_ptr->q_type == QUEST_NEST) return (TRUE);
    if (q_ptr->q_type == QUEST_ARENA_LEVEL) return (TRUE);

    return (FALSE);
}

/* Verify if the quest slot involves clearing multiple races on a single level */
bool quest_slot_multiple_r_idx(int quest_num)
{
    quest_type *q_ptr;

    /* Boundry control, then get the quest */
    if (quest_num >= z_info->q_max) return (FALSE);
    if (quest_num < 0) return (FALSE);
    q_ptr = &q_info[quest_num];

    return (quest_multiple_r_idx(q_ptr));
}

/* Verify if the quest involves clearing multiple races on a single level */
bool quest_single_r_idx(const quest_type *q_ptr)
{
    if (q_ptr->q_type == QUEST_MONSTER) return (TRUE);
    if (q_ptr->q_type == QUEST_GUARDIAN) return (TRUE);

    return (FALSE);
}

/* Verify if the quest involves clearing multiple races on a single level */
bool quest_slot_single_r_idx(int quest_num)
{
    quest_type *q_ptr;

    /* Boundry control, then get the quest */
    if (quest_num >= z_info->q_max) return (FALSE);
    if (quest_num < 0) return (FALSE);
    q_ptr = &q_info[quest_num];

    return (quest_single_r_idx(q_ptr));
}

/* Verify if the quest is for a monster theme */
bool quest_themed(const quest_type *q_ptr)
{
    if (q_ptr->q_type == QUEST_THEMED_LEVEL) return (TRUE);
    if (q_ptr->q_type == QUEST_PIT) return (TRUE);
    if (q_ptr->q_type == QUEST_NEST) return (TRUE);

    return (FALSE);
}

/* Verify if the quest is for a monster theme */
bool quest_slot_themed(int quest_num)
{
    quest_type *q_ptr;

    /* Boundry control, then get the quest */
    if (quest_num >= z_info->q_max) return (FALSE);
    if (quest_num < 0) return (FALSE);
    q_ptr = &q_info[quest_num];

    return (quest_themed(q_ptr));
}

/* Verify if the quest involves clearing multiple races on a single level */
bool quest_type_collection(const quest_type *q_ptr)
{
    if (q_ptr->q_type == QUEST_LABYRINTH) return (TRUE);
    if (q_ptr->q_type == QUEST_WILDERNESS) return (TRUE);

    return (FALSE);
}

/* Verify if the quest involves clearing multiple races on a single level */
bool quest_slot_collection(int quest_num)
{
    quest_type *q_ptr;

    /* Boundry control, then get the quest */
    if (quest_num >= z_info->q_max) return (FALSE);
    if (quest_num < 0) return (FALSE);
    q_ptr = &q_info[quest_num];

    return (quest_single_r_idx(q_ptr));
}

/* Verify if the quest has a time limit */
bool quest_timed(const quest_type *q_ptr)
{
    if (q_ptr->q_type == QUEST_GREATER_VAULT) return (TRUE);

    return (FALSE);
}

/* Verify if the quest is timed */
bool quest_slot_timed(int quest_num)
{
    quest_type *q_ptr;

    /* Boundry control, then get the quest */
    if (quest_num >= z_info->q_max) return (FALSE);
    if (quest_num < 0) return (FALSE);
    q_ptr = &q_info[quest_num];

    return (quest_timed(q_ptr));
}

/* Verify if no down stairs should be generated until the quest is completed */
bool quest_no_down_stairs(const quest_type *q_ptr)
{
    if (game_mode == GAME_NPPMORIA) return (FALSE);
    if (q_ptr->q_type == QUEST_PERMANENT) return (TRUE);
    if (q_ptr->q_type == QUEST_GUARDIAN) return (TRUE);
    if (q_ptr->q_type == QUEST_ARENA_LEVEL) return (TRUE);

    return (FALSE);
}

/* Verify if no down stairs should be generated on the current level due to quests */
bool no_down_stairs(s16b check_depth)
{
    int i;

    /* Always false in the town */
    if (!check_depth) return (FALSE);

    if (game_mode == GAME_NPPMORIA) return (FALSE);

    /* Count incomplete quests */
    for (i = 0; i < z_info->q_max; i++)
    {
        quest_type *q_ptr = &q_info[i];

        /* Not the right level */
        if (q_ptr->base_level != check_depth) continue;

        /* Dont count completed quests */
        if (q_ptr->q_flags & (QFLAG_COMPLETED)) continue;

        if (quest_no_down_stairs(q_ptr)) return (TRUE);
    }

    return (FALSE);
}

/*
 * Confirm that the player shall fail the quest
 * immediately if they leave the current level
 */
bool quest_shall_fail_if_leave_level(void)
{
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    /* Quest has not started, or quest is complete */
    if (!(q_ptr->q_flags & (QFLAG_STARTED))) return (FALSE);

    if (q_ptr->q_flags & (QFLAG_COMPLETED)) return (FALSE);

    /* Not the right level */
    if (p_ptr->depth != q_ptr->base_level) return (FALSE);

    if (q_ptr->q_type == QUEST_THEMED_LEVEL) return (TRUE);
    if (quest_type_collection(q_ptr))
    {
        if (quest_item_count() < quest_collection_num(q_ptr)) return (TRUE);
    }
    if (q_ptr->q_type == QUEST_PIT) return (TRUE);
    if (q_ptr->q_type == QUEST_NEST) return (TRUE);
    if (q_ptr->q_type == QUEST_ARENA_LEVEL) return (TRUE);
    if (q_ptr->q_type == QUEST_GREATER_VAULT) return (TRUE);
    if (q_ptr->q_type == QUEST_VAULT)
    {
        if (quest_item_slot() == -1) return (TRUE);
    }

    return (FALSE);
}

/* Confirm that the player might fail the quest if they leave the current level */
bool quest_might_fail_if_leave_level(void)
{
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    /* Not the right level */
    if (p_ptr->depth != q_ptr->base_level) return (FALSE);

    /* Quest has not started, or quest is complete */
    if (!(q_ptr->q_flags & (QFLAG_STARTED))) return (FALSE);
    if (q_ptr->q_flags & (QFLAG_COMPLETED)) return (FALSE);

    if (q_ptr->q_type == QUEST_MONSTER) return (TRUE);

    return (FALSE);
}

/* Confirm that the player should fail the quest immediately */
bool quest_fail_immediately(void)
{
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    /* Quest has not started, or quest is complete */
    if (!(q_ptr->q_flags & (QFLAG_STARTED))) return (FALSE);

    if (q_ptr->q_flags & (QFLAG_COMPLETED)) return (FALSE);

    /* Currently on the quest level */
    if (p_ptr->depth == q_ptr->base_level) return (FALSE);

    if (q_ptr->q_type == QUEST_THEMED_LEVEL) return (TRUE);
    if (quest_type_collection(q_ptr))
    {
        if (quest_item_count() < quest_collection_num(q_ptr)) return (TRUE);
    }
    if (q_ptr->q_type == QUEST_PIT) return (TRUE);
    if (q_ptr->q_type == QUEST_NEST) return (TRUE);
    if (q_ptr->q_type == QUEST_ARENA_LEVEL) return (TRUE);
    if (q_ptr->q_type == QUEST_GREATER_VAULT) return (TRUE);
    if (q_ptr->q_type == QUEST_VAULT)
    {
        if (quest_item_slot() == -1) return (TRUE);
    }

    return (FALSE);
}

/* Confirm that the player might fail the quest in their current situation */
bool quest_might_fail_now(void)
{
    quest_type *q_ptr = &q_info[GUILD_QUEST_SLOT];

    /* Currently on the quest level */
    if (p_ptr->depth == q_ptr->base_level) return (FALSE);

    /* Quest has not started, or quest is complete */
    if (!(q_ptr->q_flags & (QFLAG_STARTED))) return (FALSE);
    if (q_ptr->q_flags & (QFLAG_COMPLETED)) return (FALSE);

    if (q_ptr->q_type == QUEST_MONSTER) return (TRUE);

    return (FALSE);
}

