/* File: object_desc.cpp */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"
#include "src/utilities.h"

QString sign(int value)
{
    QString s;
    if (value >= 0) {
        s = "+";
    }
    return s.append(_num(value));
}



/*
 * Strip an "object name" into a buffer.
 */
QString strip_name(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    QString str = k_ptr->k_name;

    /* remove leading characters */
    while ((str[0] == ' ') || (str[0] == '&')) str.remove(0,1);

    //remove '~'
    str.remove(QChar('~'));

    return (str);
}

static QString obj_desc_get_modstr(const object_type *o_ptr)
{
    object_kind *k_ptr = &k_info[o_ptr->k_idx];

    switch (o_ptr->tval)
    {
        case TV_AMULET:
        case TV_RING:
        case TV_STAFF:
        case TV_WAND:
        case TV_ROD:
        case TV_POTION:
        case TV_FOOD:
            return (flavor_info[k_ptr->flavor].text);

        case TV_SCROLL:
        case TV_PARCHMENT:
            return scroll_adj[o_ptr->sval];

        case TV_MAGIC_BOOK:
        case TV_PRAYER_BOOK:
        case TV_DRUID_BOOK:
            return (k_ptr->k_name);
    }

    return "";
}



static QString obj_desc_get_basename(object_type *o_ptr, bool aware, bool plural)
{
    object_kind *k_ptr = &k_info[o_ptr->k_idx];

    bool show_flavor = k_ptr->flavor ? TRUE : FALSE;

    if (o_ptr->ident & IDENT_STORE) show_flavor = FALSE;
    if (aware && !show_flavors) show_flavor = FALSE;

    /* Known artifacts get special treatment */
    if (o_ptr->is_artifact() && aware)
    {
        /* An exception for unidentified special artifacts */
        if (!k_ptr->flavor || o_ptr->is_known()) return (k_ptr->k_name);
    }

    /* Analyze the object */
    switch (o_ptr->tval)
    {
        case TV_SKELETON:
        case TV_BOTTLE:
        case TV_JUNK:
        case TV_SPIKE:
        case TV_FLASK:
        case TV_CHEST:
        case TV_SHOT:
        case TV_BOLT:
        case TV_ARROW:
        case TV_BOW:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_DIGGING:
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_CLOAK:
        case TV_CROWN:
        case TV_HELM:
        case TV_SHIELD:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_DRAG_SHIELD:
        case TV_LIGHT:
        case TV_PARCHMENT:
            return (k_ptr->k_name);

        case TV_AMULET:
        {
            if (plural) return (show_flavor ? "& # Amulets" : "& Amulets");
            else     return (show_flavor ? "& # Amulet" : "& Amulet");
        }

        case TV_RING:
        {
            if (plural) return (show_flavor ? "& # Rings" : "& Rings");
            else     return (show_flavor ? "& # Ring" : "& Ring");
        }

        case TV_STAFF:
        {
            if (plural) return (show_flavor ? "& # Staves" : "& Staves");
            else     return (show_flavor ? "& # Staff" : "& Staff");
        }

        case TV_WAND:
        {
            if (plural) return (show_flavor ? "& # Wands" : "& Wands");
            else     return (show_flavor ? "& # Wand" : "& Wand");
        }

        case TV_ROD:
        {
            if (plural) return (show_flavor ? "& # Rods" : "& Rods");
            else     return (show_flavor ? "& # Rod" : "& Rod");
        }

        case TV_POTION:
        {
            if (plural) return (show_flavor ? "& # Potions" : "& Potions");
            else     return (show_flavor ? "& # Potion" : "& Potion");
        }

        case TV_SCROLL:
        {
            if (plural) return (show_flavor ? "& Scrolls titled \"#\"" : "& Scrolls");
            else     return (show_flavor ? "& Scroll titled \"#\"" : "& Scroll");
        }

        case TV_MAGIC_BOOK:
        {
            if (plural) return "& Books of Magic Spells #";
            else return "& Book of Magic Spells #";
        }

        case TV_PRAYER_BOOK:
        {
            if (plural) return "& Holy Books of Prayers #";
            else return "& Holy Book of Prayers #";
        }

        case TV_DRUID_BOOK:
        {
            if (plural) return "& Books of Druid Incantations #";
            else return "& Book of Druid Incantations #";
        }

        case TV_FOOD:
        {
            if (o_ptr->sval < SV_FOOD_MIN_FOOD)
            {
                if (plural) return (show_flavor ? "& # Mushrooms" : "& Mushrooms");
                else return (show_flavor ? "& # Mushroom" : "& Mushroom");
            }
            else return (k_ptr->k_name);
        }
        default: return ("(nothing)");
    }

    return "(nothing)";
}



/*
 * Copy 'src' into 'buf, replacing '#' with 'modstr' (if found), putting a plural
 * in the place indicated by '~' if required, or using alterate...
 */
static QString obj_desc_name(object_type *o_ptr, bool prefix, byte mode, bool spoil)
{
    QString buf;

    object_kind *k_ptr = &k_info[o_ptr->k_idx];

    bool known = o_ptr->is_known() || spoil;
    bool aware = o_ptr->is_flavor_known() || (o_ptr->ident & IDENT_STORE) || spoil;

    QString modstr = obj_desc_get_modstr(o_ptr);

    bool pluralise = (mode & ODESC_PLURAL) ? TRUE : FALSE;

    if (o_ptr->number > 1)
        pluralise = TRUE;
    if (mode & ODESC_SINGULAR)
        pluralise = FALSE;

    QString basename = obj_desc_get_basename(o_ptr, aware, pluralise);

    //Start with a clear name
    buf.clear();

    /* Add a pseudo-numerical prefix if desired */
    if (prefix)
    {
        if (o_ptr->number <= 0)
        {
            if (!pluralise)
            {
                /* Pluralise for grammatical correctness */
                pluralise = TRUE;

                // Update the name
                basename = obj_desc_get_basename(o_ptr, aware, pluralise);
            }

            buf.append("no more ");
        }
        else if (o_ptr->number > 1) buf.append(QString("%1 ") .arg(o_ptr->number));

        else if ((known) && o_ptr->is_artifact())  buf.append( "The ");

        else if (basename.contains('&'))
        {
            bool an = FALSE;
            int lookahead = 1;

            while (basename[lookahead] == ' ') lookahead++;

            if (basename.contains('#') && (o_ptr->tval != TV_SCROLL))
            {
                if (!modstr.isEmpty() && begins_with_vowel(modstr))
                    an = TRUE;
            }
            else if (is_a_vowel(basename[lookahead]))
            {
                an = TRUE;
            }

                 // If we are inserting "well balanced", always use "a"
            if ((known) && (o_ptr->ident & IDENT_PERFECT_BALANCE)) an = FALSE;

            if (an) buf.append("an ");
            else    buf.append("a ");

        }
    }

    /* Perfectly balanced throwing weapons are indicated. */
    if ((known) && (o_ptr->ident & IDENT_PERFECT_BALANCE))
    {
        buf.append("Well-balanced ");
    }



/*
 * Names have the following elements:
 *
 * '~' indicates where to place an 's' or an 'es'.  Other plural forms should
 * be handled with the syntax '|singular|plural|', e.g. "kni|fe|ves|".
 *
 * '#' indicates the position of the "modifier", e.g. the flavour or spellbook
 * name.
 */
    while (basename.contains('&') || basename.contains('~') || basename.contains('#'))
    {
        if (basename.contains('&'))
        {
            int location = basename.indexOf('&');
            if (basename[location+1] == ' ')
            {
                basename.remove(location, 2);
            }
            else basename.remove(location, 1);
            continue;
        }

        /* Pluralizer (regular English plurals) */
        if (basename.contains('~'))
        {
            //Don't make plural.
            if (!pluralise)
            {
                basename.remove('~');
                continue;
            }

            int location = basename.indexOf('~');

            // Paranoia - should never happen
            if (location < 1) break;

            QChar prev = basename[location - 1];

            /* e.g. cutlass-e-s, torch-e-s, box-e-s */
            if (prev == 's' || prev == 'h' || prev == 'x') basename.replace("~", "es");
            else    basename.replace("~", "s");
        }

        /* Insert the modifier */
        if (basename.contains('#'))
        {
            basename.replace("#", modstr);
        }
    }

    buf.append(basename);


    /** Append extra names of various kinds **/

    if ((known) && o_ptr->art_num)
        buf.append(QString(" %1") .arg(a_info[o_ptr->art_num].a_name));

    else if ((o_ptr->ego_num) && (known || spoil))
    {
        buf.append(QString(" %1") .arg(e_info[o_ptr->ego_num].e_name));
    }

    else if (aware && (!o_ptr->is_artifact()) && (k_ptr->flavor || k_ptr->tval == TV_SCROLL))
        buf.append(QString(" of %1") .arg(k_ptr->k_name));

    return buf;
}

/*
 * Is o_ptr a weapon?
 */
static bool obj_desc_show_weapon(object_type *o_ptr)
{
    if (o_ptr->obj_flags_3 & TR3_SHOW_MODS) return TRUE;
    if (o_ptr->to_h && o_ptr->to_d) return TRUE;

    /* You need to list both to_h and to_d for things like unaware rings of accuracy and damage e.g. to differentiate (+8) */
    if ((o_ptr->to_h || o_ptr->to_d) && !o_ptr->is_flavor_known()) return TRUE;

    switch (o_ptr->tval)
    {
        case TV_SHOT:
        case TV_BOLT:
        case TV_ARROW:
        case TV_BOW:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_DIGGING:
        {
            return TRUE;
        }
    }

    return FALSE;
}

/*
 * Is o_ptr armor?
 */
static bool obj_desc_show_armor(const object_type *o_ptr)
{
    if (o_ptr->ac) return TRUE;

    switch (o_ptr->tval)
    {
        case TV_BOOTS:
        case TV_GLOVES:
        case TV_CLOAK:
        case TV_CROWN:
        case TV_HELM:
        case TV_SHIELD:
        case TV_SOFT_ARMOR:
        case TV_HARD_ARMOR:
        case TV_DRAG_ARMOR:
        case TV_DRAG_SHIELD:
        {
            return TRUE;
        }
    }

    return FALSE;
}

static QString obj_desc_chest(object_type *o_ptr, QString buf)
{
    bool known = o_ptr->is_known();

    if (o_ptr->tval != TV_CHEST) return (buf);
    if (!known) return (buf);

    /* May be "empty" */
    else if (!o_ptr->pval) buf.append(" (empty)");

    /* May be "disarmed" */
    else if (o_ptr->pval < 0)
    {
        if (chest_traps[0 - o_ptr->pval]) buf.append(" (disarmed)");
        else buf.append(" (unlocked)");
    }

    /* Describe the traps, if any */
    else
    {
        /* Describe the traps */
        switch (chest_traps[o_ptr->pval])
        {
            case 0:
                buf.append(" (Locked)");
                break;

            case CHEST_LOSE_STR:
                buf.append(" (Poison Needle)");
                break;

            case CHEST_LOSE_CON:
                buf.append(" (Poison Needle)");
                break;

            case CHEST_POISON:
                buf.append(" (Gas Trap)");
                break;

            case CHEST_PARALYZE:
                buf.append(" (Gas Trap)");
                break;

            case CHEST_EXPLODE:
                buf.append(" (Explosion Device)");
                break;

            case CHEST_SUMMON:
                buf.append(" (Summoning Runes)");
                break;

            default:
                buf.append(" (Multiple Traps)");
                break;
        }
    }

    return (buf);
}

static QString obj_desc_combat(object_type *o_ptr,  QString buf, bool spoil)
{
    object_kind *k_ptr = &k_info[o_ptr->k_idx];

    /* Dump base weapon info */
    switch (o_ptr->tval)
    {
        /* Weapons */
        case TV_SHOT:
        case TV_BOLT:
        case TV_ARROW:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_SWORD:
        case TV_DIGGING:
        {
            /* Only display the real damage dice if the combat stats are known */
            if ((spoil) || o_ptr->is_known())
                buf.append(QString(" (%1d%2)") .arg(o_ptr->dd)  .arg(o_ptr->ds));
            else buf.append(QString(" (%1d%2)") .arg(k_ptr->dd)  .arg(k_ptr->ds));
            break;
        }

        /* Missile launchers */
        case TV_BOW:
        {
            /* Display shooting power as part of the multiplier */
            if ((o_ptr->obj_flags_1 & TR1_MIGHT) && (spoil || (o_ptr->known_obj_flags_1 & TR1_MIGHT)))
                buf.append(QString(" (x%1)") .arg((o_ptr->sval % 10) + o_ptr->pval));
            else
                buf.append(QString(" (x%1)") .arg(o_ptr->sval % 10));
            break;
        }
    }


    /* Show weapon bonuses */
    if (spoil || o_ptr->is_known())
    {
        if (obj_desc_show_weapon(o_ptr) || o_ptr->to_d || o_ptr->to_h)
        {
            /* Make an exception for body armor with only a to-hit penalty */
            if (o_ptr->to_h < 0 && o_ptr->to_d == 0 &&
                (o_ptr->tval == TV_SOFT_ARMOR ||
                 o_ptr->tval == TV_HARD_ARMOR ||
                 o_ptr->tval == TV_DRAG_ARMOR ||
                 o_ptr->tval == TV_DRAG_SHIELD))
                buf.append(QString(" (%1)").arg(sign(o_ptr->to_h)));

            /* Otherwise, always use the full tuple */
            else buf.append(QString(" (%1,%2)").arg(sign(o_ptr->to_h)).arg(sign(o_ptr->to_d)));
        }
    }


    /* Show armor bonuses */
    if (spoil || o_ptr->is_known())
    {
        if (obj_desc_show_armor(o_ptr))
            buf.append(QString(" [%1,%2]").arg(sign(o_ptr->ac)).arg(sign(o_ptr->to_a)));
        else if (o_ptr->to_a)
                    buf.append(QString(" [%1]").arg(sign(o_ptr->to_a)));
    }
    else if (obj_desc_show_armor(o_ptr))
    {
        buf.append(QString(" [%1]").arg(sign(o_ptr->ac)));
    }

    return (buf);
}

static QString obj_desc_light(object_type *o_ptr, QString buf)
{
    /* Fuelled light sources get number of remaining turns appended */
    if (o_ptr->is_fuelable_lite())
        buf.append(QString(" (%1 turns)") .arg(o_ptr->timeout));

    return (buf);
}

static QString obj_desc_pval(object_type *o_ptr, QString buf)
{
    if (!(o_ptr->obj_flags_1 & TR1_PVAL_MASK)) return (buf);

    buf.append(QString(" (%1").arg(sign(o_ptr->pval)));

    if (!(o_ptr->obj_flags_3 & TR3_HIDE_TYPE))
    {
        if (o_ptr->obj_flags_1 & TR1_STEALTH)
            buf.append(" stealth");
        else if (o_ptr->obj_flags_1 & TR1_SEARCH)
            buf.append(" searching");
        else if (o_ptr->obj_flags_1 & TR1_INFRA)
            buf.append(" infravision");
        else if (o_ptr->obj_flags_1 & TR1_SPEED)
            buf.append(" speed");
        else if (o_ptr->obj_flags_1 & TR1_BLOWS)
        {
            buf.append(QString(" attack%1") .arg(o_ptr->pval));
            if (o_ptr->pval > 1) buf.append("s");
        }
    }

    buf.append(")");

    return (buf);
}

static QString obj_desc_charges(object_type *o_ptr, QString buf)
{
    object_kind *k_ptr = &k_info[o_ptr->k_idx];

    bool aware = o_ptr->is_flavor_known() || (o_ptr->ident & IDENT_STORE);

    /* See if the object is "known" */
    bool known = o_ptr->is_known();

    /* Wands and Staffs have charges */
    if (aware && known && (o_ptr->tval == TV_STAFF || o_ptr->tval == TV_WAND))
    {
        buf.append(QString(" (%1 charge") .arg(o_ptr->pval));
        if (o_ptr->pval > 1) buf.append("s");
        buf += ")";
    }

    /* Charging things */
    else if (o_ptr->timeout > 0)
    {
        /* Hack -- Rods have a "charging" indicator */
        if (known && (o_ptr->tval == TV_ROD))
        {
            /* Hack -- Dump " (# charging)" if relevant */
            if (o_ptr->timeout >= 1)
            {

                /* Stacks of rods display an exact count of charging rods. */
                if (o_ptr->number > 1)
                {
                    int power;

                    /* Paranoia. */
                    if (k_ptr->pval == 0) k_ptr->pval = 1;

                    /* Find out how many rods are charging, by dividing
                     * current timeout by each rod's maximum timeout.
                     * Ensure that any remainder is rounded up.  Display
                     * very discharged stacks as merely fully discharged.
                     */
                    power = (o_ptr->timeout + (k_ptr->pval - 1)) / k_ptr->pval;

                    if (power > o_ptr->number) power = o_ptr->number;

                    /* Display prettily */
                    buf.append(QString(" (%1 charging)") .arg(power));
                }


                /* Display prettily */
                buf.append(" (charging)");
            }
        }


        /* Indicate "charging" objects, but not rods or lites */
        if (known && o_ptr->timeout && o_ptr->tval != TV_ROD
                && !o_ptr->is_fuelable_lite())
        {

            /* Hack -- Dump " (charging)" if relevant */
            buf.append(" (charging)");
        }
    }

    return (buf);
}

static QString obj_desc_inscrip(object_type *o_ptr, QString buf)
{
    QVector<QString> inscriptions;
    inscriptions.clear();

    /* See if the object is "known" */
    bool known = o_ptr->is_known();
    bool aware = o_ptr->is_aware();

    /* Get inscription, pdeudo-id, or store discount */
    if (!o_ptr->inscription.isEmpty()) inscriptions.append(o_ptr->inscription);
    if (o_ptr->discount >= INSCRIP_NULL)
    {
        inscriptions.append(inscrip_text[o_ptr->discount - INSCRIP_NULL]);
    }
    else if (o_ptr->is_cursed() && known)
    {
        inscriptions.append(QString("cursed"));
    }
    else if ((o_ptr->ident & IDENT_EMPTY) && (!known))
    {
        inscriptions.append(QString("empty"));
    }
    else if ((!aware) && o_ptr->is_tried())
    {
        inscriptions.append(QString("tried"));
    }
    else if (o_ptr->discount > 0)
    {
        inscriptions.append(QString("%1 pct off") .arg(o_ptr->discount));
    }

    /* Use the "unknown" inscription */
    else if (!known && o_ptr->can_be_pseudo_ided() &&
            (o_ptr->discount < INSCRIP_NULL))
    {
        inscriptions.append(QString("unknown"));
    }

    if (inscriptions.size())
    {
        for (int i = 0; i < inscriptions.size(); i++)
        {
            if (i == 0) buf.append(" {");
            buf.append(inscriptions.at(i));
            if (i < inscriptions.size()-1) buf.append(", ");
        }

        buf.append("}");
    }

    return (buf);
}



/*
 * Creates a description of the item "o_ptr", and stores it in "buf".
 *
 *
 * ODESC_PREFIX prepends a 'the', 'a' or number
 * ODESC_BASE results in a base description.
 * ODESC_COMBAT will add to-hit, to-dam and AC info.
 * ODESC_EXTRA will add pval/charge/inscription/squelch info.
 * ODESC_PLURAL will pluralise regardless of the number in the stack.
 * ODESC_STORE turns off squelch markers, for in-store display.
 * ODESC_SPOIL treats the object as fully identified.
 *
 * Setting 'prefix' to TRUE prepends a 'the', 'a' or the number in the stack,
 * respectively.
 *
 * \returns The number of bytes used of the buffer.
 */
QString object_desc(object_type *o_ptr, byte mode)
{
    bool prefix = mode & ODESC_PREFIX;
    bool spoil = ((mode & (ODESC_SPOIL)));
    QString buf;

    bool known;

    object_kind *k_ptr = &k_info[o_ptr->k_idx];

    /* Make sure the flags are up to date */
    o_ptr->update_object_flags();

    /* See if the object is "known" */
    known = o_ptr->is_known();

    /*** Some things get really simple descriptions ***/

    if (o_ptr->tval == TV_GOLD)
    {
        return (QString("%1 gold pieces worth of %2") .arg(o_ptr->pval) .arg(k_ptr->k_name));
    }
    else if (!o_ptr->tval) return ("(nothing)");

    /* Copy the base name to the buffer */
    buf = obj_desc_name(o_ptr, prefix, mode, spoil);

    if (mode & ODESC_COMBAT)
    {
        if (o_ptr->tval == TV_CHEST)
            buf = obj_desc_chest(o_ptr, buf);
        else if (o_ptr->tval == TV_LIGHT)
            buf = obj_desc_light(o_ptr, buf);
        buf = obj_desc_combat(o_ptr, buf, spoil);
    }

    if (mode & ODESC_EXTRA)
    {
        if (spoil || known)
            buf = obj_desc_pval(o_ptr, buf);

        buf = obj_desc_charges(o_ptr, buf);

        buf = obj_desc_inscrip(o_ptr, buf);
    }

    return (buf);

}

/*
 * Describe an item and pretend the item is fully known and has no flavor.
 */
QString object_desc_spoil(object_type *o_ptr)
{    
    object_type object_type_body;
    object_type *i_ptr = &object_type_body;

    /* Make a backup */
    i_ptr->object_copy(o_ptr);

    /* HACK - Pretend the object is in a store inventory */
    i_ptr->ident |= IDENT_STORE;

    /* Describe */
    return(object_desc(i_ptr, ODESC_FULL | ODESC_SPOIL));
}

QString object_desc(int k_idx, byte mode)
{
    object_type object_type_body;
    object_type *o_ptr = &object_type_body;

    /* Make fake object */
    o_ptr->object_wipe();
    make_object_fake(o_ptr, k_idx, 0, FALSE);

    object_level = p_ptr->depth;

    return (object_desc(o_ptr, mode));
}

