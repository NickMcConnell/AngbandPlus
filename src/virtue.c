/* Virtue System from Hengband, but re-written for clarity */

#include "angband.h"

static cptr _names[VIRTUE_MAX] =
{
    "Unknown",
    "Compassion",
    "Honour",
    "Justice",
    "Sacrifice",
    "Knowledge",
    "Faith",
    "Enlightenment",
    "Mysticism",
    "Chance",
    "Nature",
    "Harmony",
    "Vitality",
    "Unlife",
    "Patience",
    "Temperance",
    "Diligence",
    "Valour",
    "Individualism",
};

static cptr _good_msg[VIRTUE_MAX] =
{
    "Bug: Invalid Virtue",
    "That was very compassionate.",
    "That was rather honorable.",
    "Your sense of justice is impressive.",
    "What a noble sacrifice!",
    "You feel more knowledgeable.",
    "What an act of faith!",
    "That was quite enlightening.",
    "You are a true mystic!",
    "A bold risk indeed!",
    "You feel more in tune with the world around you.",
    "That was very harmonious.",
    "How vigorous!",
    "You commune with the undead.",
    "Your patience is astounding!",
    "Moderation in all things.",
    "That was very diligent.",
    "How heroic!",
    "You walk your own road in life.",
};

static cptr _bad_msg[VIRTUE_MAX] =
{
    "Bug: Invalid Virtue",
    "Stop, you brute!",
    "That was rather dishonorable.",
    "How unfair!",
    "You are straying from the path of sacrifice.",
    "You feel less knowledgeable.",
    "That shows a complete lack of faith on your part.",
    "That was not very enlightening.",
    "You are not very mystical.",
    "Perhaps you are too cautious?",
    "You feel out of touch with natural world.",
    "Your actions are disharmonious.",
    "That was rather lethargic.",
    "You are straying from the path of unlife.",
    "How impatient!",
    "Gluttony is very unbecoming.",
    "How lazy!",
    "You dastardly villain!",
    "You are straying from the path of individualism.",
};

static bool _is_valid_index(int idx)
{
    if (0 <= idx && idx < 8)
        return TRUE;
    return FALSE;
}

static bool _is_valid_virtue(int which)
{
    if (0 < which && which < VIRTUE_MAX)
        return TRUE;
    return FALSE;
}

static s16b _random_virtue(void)
{
    int result = VIRTUE_NONE;

    while (!_is_valid_virtue(result) || virtue_present(result))
    {
        switch (randint1(29))
        {
        case 1: case 2: case 3:
            result = VIRTUE_SACRIFICE;
            break;
        case 4: case 5: case 6:
            result = VIRTUE_COMPASSION;
            break;
        case 7: case 8: case 9: case 10: case 11: case 12:
            result = VIRTUE_VALOUR;
            break;
        case 13: case 14: case 15: case 16: case 17:
            result = VIRTUE_HONOUR;
            break;
        case 18: case 19: case 20: case 21:
            result = VIRTUE_JUSTICE;
            break;
        case 22: case 23:
            result = VIRTUE_TEMPERANCE;
            break;
        case 24: case 25:
            result = VIRTUE_HARMONY;
            break;
        case 26: case 27: case 28:
            result = VIRTUE_PATIENCE;
            break;
        default:
            result = VIRTUE_DILIGENCE;
            break;
        }
    }
    return result;
}

static s16b _realm_virtue(int realm)
{
    switch (realm)
    {
    case REALM_LIFE:
    case REALM_BLESS:
        if (virtue_present(VIRTUE_VITALITY)) return VIRTUE_TEMPERANCE;
        else return VIRTUE_VITALITY;
    case REALM_SORCERY:
        if (virtue_present(VIRTUE_KNOWLEDGE)) return VIRTUE_ENCHANTMENT;
        else return VIRTUE_KNOWLEDGE;
    case REALM_NATURE:
        if (virtue_present(VIRTUE_NATURE)) return VIRTUE_HARMONY;
        else return VIRTUE_NATURE;
    case REALM_CHAOS:
        if (virtue_present(VIRTUE_CHANCE)) return VIRTUE_INDIVIDUALISM;
        else return VIRTUE_CHANCE;
    case REALM_DEATH: case REALM_NECROMANCY:
        return VIRTUE_UNLIFE;
    case REALM_TRUMP:
        return VIRTUE_KNOWLEDGE;
    case REALM_ARCANE:
        return VIRTUE_NONE;
    case REALM_CRAFT:
        if (virtue_present(VIRTUE_ENCHANTMENT)) return VIRTUE_INDIVIDUALISM;
        else return VIRTUE_ENCHANTMENT;
    case REALM_DAEMON:
        if (virtue_present(VIRTUE_JUSTICE)) return VIRTUE_FAITH;
        else return VIRTUE_JUSTICE;
    case REALM_CRUSADE:
        if (virtue_present(VIRTUE_JUSTICE)) return VIRTUE_HONOUR;
        else return VIRTUE_JUSTICE;
    case REALM_HEX:
        if (virtue_present(VIRTUE_COMPASSION)) return VIRTUE_JUSTICE;
        else return VIRTUE_COMPASSION;
    }

    return VIRTUE_NONE;
}

cptr virtue_name(int which)
{
    if (which > 0 && which < VIRTUE_MAX)
        return _names[which];
    return _names[VIRTUE_NONE];
}

int virtue_find(int which)
{
    int i;
    for (i = 0; i < 8; i++)
        if (plr->vir_types[i] == which) return i;

    return -1;
}

bool virtue_present(int which)
{
    return _is_valid_index(virtue_find(which));
}

int virtue_current(int which)
{
    int idx = virtue_find(which);
    if (_is_valid_index(idx))
        return plr->virtues[idx];
    return 0;
}

void virtue_init(void)
{
    int i = 0;

    /* Reset */
    for (i = 0; i < 8; i++)
    {
        plr->virtues[i] = 0;
        plr->vir_types[i] = VIRTUE_NONE;
    }

    i = 0;

    /* Get pre-defined types */
    /* 1 or more virtues based on class */
    switch (plr->pclass)
    {
    case CLASS_WARRIOR:
    case CLASS_SAMURAI:
    case CLASS_MAULER:
        plr->vir_types[i++] = VIRTUE_VALOUR;
        plr->vir_types[i++] = VIRTUE_HONOUR;
        break;
    case CLASS_MAGE:
    case CLASS_NECROMANCER:
    case CLASS_YELLOW_MAGE:
    case CLASS_GRAY_MAGE:
    case CLASS_BLUE_MAGE:
        plr->vir_types[i++] = VIRTUE_KNOWLEDGE;
        plr->vir_types[i++] = VIRTUE_ENCHANTMENT;
        break;
    case CLASS_PRIEST:
    case CLASS_HIGH_PRIEST:
        plr->vir_types[i++] = VIRTUE_FAITH;
        plr->vir_types[i++] = VIRTUE_TEMPERANCE;
        break;
    case CLASS_ROGUE:
    case CLASS_SNIPER:
    case CLASS_SCOUT:
        plr->vir_types[i++] = VIRTUE_HONOUR;
        break;
    case CLASS_RANGER:
    case CLASS_ARCHER:
        plr->vir_types[i++] = VIRTUE_NATURE;
        plr->vir_types[i++] = VIRTUE_TEMPERANCE;
        break;
    case CLASS_PALADIN:
        plr->vir_types[i++] = VIRTUE_JUSTICE;
        plr->vir_types[i++] = VIRTUE_VALOUR;
        plr->vir_types[i++] = VIRTUE_HONOUR;
        plr->vir_types[i++] = VIRTUE_FAITH;
        break;
    case CLASS_WARRIOR_MAGE:
    case CLASS_RED_MAGE:
        plr->vir_types[i++] = VIRTUE_ENCHANTMENT;
        plr->vir_types[i++] = VIRTUE_VALOUR;
        break;
    case CLASS_CHAOS_WARRIOR:
        plr->vir_types[i++] = VIRTUE_CHANCE;
        plr->vir_types[i++] = VIRTUE_INDIVIDUALISM;
        break;
    case CLASS_MONK:
    case CLASS_FORCETRAINER:
        plr->vir_types[i++] = VIRTUE_FAITH;
        plr->vir_types[i++] = VIRTUE_HARMONY;
        plr->vir_types[i++] = VIRTUE_TEMPERANCE;
        plr->vir_types[i++] = VIRTUE_PATIENCE;
        break;
    case CLASS_MYSTIC:
        plr->vir_types[i++] = VIRTUE_HARMONY;
        plr->vir_types[i++] = VIRTUE_TEMPERANCE;
        plr->vir_types[i++] = VIRTUE_PATIENCE;
        break;
    case CLASS_MINDCRAFTER:
    case CLASS_MIRROR_MASTER:
    case CLASS_PSION:
        plr->vir_types[i++] = VIRTUE_HARMONY;
        plr->vir_types[i++] = VIRTUE_ENLIGHTENMENT;
        plr->vir_types[i++] = VIRTUE_PATIENCE;
        break;
    case CLASS_HIGH_MAGE:
    case CLASS_SORCERER:
        plr->vir_types[i++] = VIRTUE_ENLIGHTENMENT;
        plr->vir_types[i++] = VIRTUE_ENCHANTMENT;
        plr->vir_types[i++] = VIRTUE_KNOWLEDGE;
        break;
    case CLASS_WILD_TALENT:
        plr->vir_types[i++] = VIRTUE_CHANCE;
        break;
    case CLASS_BEASTMASTER:
        plr->vir_types[i++] = VIRTUE_NATURE;
        plr->vir_types[i++] = VIRTUE_CHANCE;
        plr->vir_types[i++] = VIRTUE_VITALITY;
        break;
    case CLASS_MAGIC_EATER:
        plr->vir_types[i++] = VIRTUE_ENCHANTMENT;
        plr->vir_types[i++] = VIRTUE_KNOWLEDGE;
        break;
    case CLASS_BARD:
        plr->vir_types[i++] = VIRTUE_HARMONY;
        plr->vir_types[i++] = VIRTUE_COMPASSION;
        break;
    case CLASS_CAVALRY:
        plr->vir_types[i++] = VIRTUE_VALOUR;
        plr->vir_types[i++] = VIRTUE_HARMONY;
        break;
    case CLASS_WEAPONSMITH:
        plr->vir_types[i++] = VIRTUE_HONOUR;
        plr->vir_types[i++] = VIRTUE_KNOWLEDGE;
        break;
    case CLASS_NINJA:
        plr->vir_types[i++] = VIRTUE_PATIENCE;
        plr->vir_types[i++] = VIRTUE_KNOWLEDGE;
        plr->vir_types[i++] = VIRTUE_FAITH;
        plr->vir_types[i++] = VIRTUE_UNLIFE;
        break;
    };

    /* Get one virtue based on race */
    switch (plr->prace)
    {
    case RACE_CENTAUR:
    case RACE_WOOD_ELF:
    case RACE_WATER_ELF:
        plr->vir_types[i++] = VIRTUE_NATURE;
        break;
    case RACE_MON_SWORD:
        plr->vir_types[i++] = VIRTUE_ENCHANTMENT;
        break;
    case RACE_MON_ANGEL:
        plr->vir_types[i++] = VIRTUE_FAITH;
        break;
    case RACE_MON_BEHOLDER:
        plr->vir_types[i++] = VIRTUE_KNOWLEDGE;
        break;
    case RACE_MON_DEMON:
    case RACE_MON_LICH:
        plr->vir_types[i++] = VIRTUE_UNLIFE;
        break;
    case RACE_MON_DRAGON:
        plr->vir_types[i++] = VIRTUE_INDIVIDUALISM;
        break;
    case RACE_MON_GIANT:
        plr->vir_types[i++] = VIRTUE_JUSTICE;
        break;
    case RACE_MON_HOUND:
        plr->vir_types[i++] = VIRTUE_NATURE;
        break;
    case RACE_MON_JELLY:
        plr->vir_types[i++] = VIRTUE_DILIGENCE;
        break;
    case RACE_MON_LEPRECHAUN:
        plr->vir_types[i++] = VIRTUE_CHANCE;
        break;
    case RACE_MON_SPIDER:
        plr->vir_types[i++] = VIRTUE_NATURE;
        break;
    case RACE_MON_TROLL:
        plr->vir_types[i++] = VIRTUE_VITALITY;
        break;
    case RACE_MON_XORN:
        plr->vir_types[i++] = VIRTUE_DILIGENCE;
        break;
    case RACE_HUMAN: case RACE_DEMIGOD: case RACE_DUNADAN:
        plr->vir_types[i++] = VIRTUE_INDIVIDUALISM;
        break;
    case RACE_SPRITE: case RACE_ENT:
        plr->vir_types[i++] = VIRTUE_NATURE;
        break;
    case RACE_HOBBIT: case RACE_HALF_OGRE:
        plr->vir_types[i++] = VIRTUE_TEMPERANCE;
        break;
    case RACE_DWARF: case RACE_KLACKON: case RACE_ANDROID:
        plr->vir_types[i++] = VIRTUE_DILIGENCE;
        break;
    case RACE_GNOME: case RACE_CYCLOPS:
        plr->vir_types[i++] = VIRTUE_KNOWLEDGE;
        break;
    case RACE_SNOTLING: case RACE_AMBERITE: case RACE_KOBOLD:
        plr->vir_types[i++] = VIRTUE_HONOUR;
        break;
    case RACE_HALF_TROLL: case RACE_BARBARIAN:
        plr->vir_types[i++] = VIRTUE_VALOUR;
        break;
    case RACE_HIGH_ELF: case RACE_KUTAR:
        plr->vir_types[i++] = VIRTUE_VITALITY;
        break;
    case RACE_HALF_GIANT: case RACE_GOLEM: case RACE_ARCHON: case RACE_BALROG:
        plr->vir_types[i++] = VIRTUE_JUSTICE;
        break;
    case RACE_HALF_TITAN:
        plr->vir_types[i++] = VIRTUE_HARMONY;
        break;
    case RACE_YEEK:
        plr->vir_types[i++] = VIRTUE_SACRIFICE;
        break;
    case RACE_MIND_FLAYER:
        plr->vir_types[i++] = VIRTUE_ENLIGHTENMENT;
        break;
    case RACE_DARK_ELF: case RACE_DRACONIAN: case RACE_SHADOW_FAIRY: case RACE_DRIDER:
        plr->vir_types[i++] = VIRTUE_ENCHANTMENT;
        break;
    case RACE_NIBELUNG:
        plr->vir_types[i++] = VIRTUE_PATIENCE;
        break;
    case RACE_IMP:
    case RACE_TENGU:
        plr->vir_types[i++] = VIRTUE_FAITH;
        break;
    case RACE_ZOMBIE: case RACE_SKELETON:
    case RACE_VAMPIRE: case RACE_SPECTRE:
    case RACE_MON_VAMPIRE:
        plr->vir_types[i++] = VIRTUE_UNLIFE;
        break;
    case RACE_BEASTMAN:
        plr->vir_types[i++] = VIRTUE_CHANCE;
        break;
    }

    /* Get a virtue for realms */
    if (plr->pclass == CLASS_GRAY_MAGE)
    {
        if (plr->psubclass == GRAY_MAGE_GOOD)
        {
            s16b v = _realm_virtue(REALM_LIFE);
            if (_is_valid_virtue(v))
                plr->vir_types[i++] = v;
            v = _realm_virtue(REALM_CRUSADE);
            if (_is_valid_virtue(v))
                plr->vir_types[i++] = v;
        }
        else if (plr->psubclass == GRAY_MAGE_NEUTRAL)
        {
            s16b v = _realm_virtue(REALM_NATURE);
            if (_is_valid_virtue(v))
                plr->vir_types[i++] = v;
            v = _realm_virtue(REALM_CHAOS);
            if (_is_valid_virtue(v))
                plr->vir_types[i++] = v;
        }
        else if (plr->psubclass == GRAY_MAGE_EVIL)
        {
            s16b v = _realm_virtue(REALM_DEATH);
            if (_is_valid_virtue(v))
                plr->vir_types[i++] = v;
            v = _realm_virtue(REALM_DAEMON);
            if (_is_valid_virtue(v))
                plr->vir_types[i++] = v;
        }
    }
    else
    {
        if (plr->realm1)
        {
            s16b v = _realm_virtue(plr->realm1);
            if (_is_valid_virtue(v))
                plr->vir_types[i++] = v;
        }
        if (plr->realm2)
        {
            s16b v = _realm_virtue(plr->realm2);
            if (_is_valid_virtue(v))
                plr->vir_types[i++] = v;
        }
    }

    /* Eliminate doubles */
    for (i = 0; i < 8; i++)
    {
        if (_is_valid_virtue(plr->vir_types[i]))
        {
            int j;
            for (j = i + 1; j < 8; j++)
            {
                if ( _is_valid_virtue(plr->vir_types[j])
                  && plr->vir_types[j] == plr->vir_types[i] )
                {
                    plr->vir_types[j] = 0;
                }
            }
        }
    }

    /* Fill in the blanks */
    for (i = 0; i < 8; i++)
    {
        if (!_is_valid_virtue(plr->vir_types[i]))
            plr->vir_types[i] = _random_virtue();
    }
}


void virtue_add(int which, int amount)
{
    int idx;

    idx = virtue_find(which);
    if (!_is_valid_index(idx))
        return;

    if (amount > 0)
    {
        plr->update |= PU_BONUS;
        if (disturb_minor)
            msg_print(_good_msg[which]);

        if (amount + plr->virtues[idx] > 50 && one_in_(2))
        {
            plr->virtues[idx] = MAX(plr->virtues[idx], 50);
            return;
        }
        if (amount + plr->virtues[idx] > 80 && one_in_(2))
        {
            plr->virtues[idx] = MAX(plr->virtues[idx], 80);
            return;
        }
        if (amount + plr->virtues[idx] > 100 && one_in_(2))
        {
            plr->virtues[idx] = MAX(plr->virtues[idx], 100);
            return;
        }
        if (amount + plr->virtues[idx] > 125)
            plr->virtues[idx] = 125;
        else
            plr->virtues[idx] = plr->virtues[idx] + amount;
    }
    else
    {
        if (disturb_minor)
            msg_print(_bad_msg[which]);

        if (amount + plr->virtues[idx] < -50 && one_in_(2))
        {
            plr->virtues[idx] = MIN(plr->virtues[idx], -50);
            return;
        }
        if (amount + plr->virtues[idx] < -80 && one_in_(2))
        {
            plr->virtues[idx] = MIN(plr->virtues[idx], -80);
            return;
        }
        if (amount + plr->virtues[idx] < -100 && one_in_(2))
        {
            plr->virtues[idx] = MIN(plr->virtues[idx], -100);
            return;
        }
        if (amount + plr->virtues[idx] < -125)
            plr->virtues[idx] = -125;
        else
            plr->virtues[idx] = plr->virtues[idx] + amount;
    }
}

static char _alignment_color(void)
{
    if (plr->align > 150) return 'g';
    else if (plr->align > 50) return 'G';
    else if (plr->align > 10) return 'B';
    else if (plr->align > -11) return 'w';
    else if (plr->align > -51) return 'o';
    else if (plr->align > -151) return 'r';
    else return 'v';
}

void virtue_display(doc_ptr doc, bool spoil)
{
    int idx = 0;

    doc_printf(doc, "<color:G>Your alignment:</color> <color:%c>%s</color>", _alignment_color(), your_alignment());
    if (spoil)
        doc_printf(doc, " <color:D>(%d)</color>", plr->align);
    doc_newline(doc);

    for (idx = 0; idx < 8; idx++)
    {
        if (!_is_valid_virtue(plr->vir_types[idx]))
        {
        }
        else
        {
            char name[255];
            int tester = plr->virtues[idx];

            strcpy(name, virtue_name(plr->vir_types[idx]));

            if (tester < -100)
                doc_printf(doc, "You are the <color:v>polar opposite</color> of %s", name);
            else if (tester < -80)
                doc_printf(doc, "You are an <color:r>arch-enemy</color> of %s", name);
            else if (tester < -60)
                doc_printf(doc, "You are a <color:R>bitter enemy</color> of %s", name);
            else if (tester < -40)
                doc_printf(doc, "You are an <color:o>enemy</color> of %s", name);
            else if (tester < -20)
                doc_printf(doc, "You have <color:y>sinned against</color> %s", name);
            else if (tester < 0)
                doc_printf(doc, "You have <color:U>strayed</color> from the path of %s", name);
            else if (tester == 0)
                doc_printf(doc, "You are neutral to %s", name);
            else if (tester < 20)
                doc_printf(doc, "You are <color:G>somewhat virtuous</color> in %s", name);
            else if (tester < 40)
                doc_printf(doc, "You are <color:G>virtuous</color> in %s", name);
            else if (tester < 60)
                doc_printf(doc, "You are <color:g>very virtuous</color> in %s", name);
            else if (tester < 80)
                doc_printf(doc, "You are a <color:g>champion</color> of %s", name);
            else if (tester < 100)
                doc_printf(doc, "You are a <color:B>great champion</color> of %s",  name);
            else
                doc_printf(doc, "You are the <color:B>living embodiment</color> of %s", name);

            if (spoil)
                doc_printf(doc, " (%d).\n", tester);
            else
                doc_insert(doc, ".\n");
        }
    }
}

/*************************************************************************
 * Spellcasting and the Virtues
 ************************************************************************/
void virtue_on_fail_spell(int realm, int fail)
{
    switch (realm)
    {
    case REALM_LIFE:
    case REALM_BLESS:
        if (randint1(100) < fail)
            virtue_add(VIRTUE_VITALITY, -1);
        break;
    case REALM_DEATH:
    case REALM_NECROMANCY:
        if (randint1(100) < fail)
            virtue_add(VIRTUE_UNLIFE, -1);
        break;
    case REALM_NATURE:
        if (randint1(100) < fail)
            virtue_add(VIRTUE_NATURE, -1);
        break;
    case REALM_DAEMON:
        if (randint1(100) < fail)
            virtue_add(VIRTUE_JUSTICE, 1);
        break;
    case REALM_CRUSADE:
        if (randint1(100) < fail)
            virtue_add(VIRTUE_JUSTICE, -1);
        break;
    case REALM_HEX:
        if (randint1(100) < fail)
            virtue_add(VIRTUE_COMPASSION, -1);
        break;
    default:
        if (randint1(100) < fail)
            virtue_add(VIRTUE_KNOWLEDGE, -1);
        break;
    }
    if (randint1(100) >= fail)
        virtue_add(VIRTUE_CHANCE,-1);
}

void virtue_on_first_cast_spell(int realm)
{
    switch (realm)
    {
    case REALM_LIFE:
        virtue_add(VIRTUE_TEMPERANCE, 1);
        virtue_add(VIRTUE_COMPASSION, 1);
        virtue_add(VIRTUE_VITALITY, 1);
        virtue_add(VIRTUE_DILIGENCE, 1);
        break;
    case REALM_BLESS:
        virtue_add(VIRTUE_TEMPERANCE, 1);
        virtue_add(VIRTUE_COMPASSION, 1);
        virtue_add(VIRTUE_VITALITY, 1);
        virtue_add(VIRTUE_HONOUR, 1);
        break;
    case REALM_DEATH:
    case REALM_NECROMANCY:
        virtue_add(VIRTUE_UNLIFE, 1);
        virtue_add(VIRTUE_JUSTICE, -1);
        virtue_add(VIRTUE_FAITH, -1);
        virtue_add(VIRTUE_VITALITY, -1);
        break;
    case REALM_DAEMON:
        virtue_add(VIRTUE_JUSTICE, -1);
        virtue_add(VIRTUE_FAITH, -1);
        virtue_add(VIRTUE_HONOUR, -1);
        virtue_add(VIRTUE_TEMPERANCE, -1);
        break;
    case REALM_CRUSADE:
        virtue_add(VIRTUE_FAITH, 1);
        virtue_add(VIRTUE_JUSTICE, 1);
        virtue_add(VIRTUE_SACRIFICE, 1);
        virtue_add(VIRTUE_HONOUR, 1);
        break;
    case REALM_NATURE:
        virtue_add(VIRTUE_NATURE, 1);
        virtue_add(VIRTUE_HARMONY, 1);
        break;
    case REALM_HEX:
        virtue_add(VIRTUE_JUSTICE, -1);
        virtue_add(VIRTUE_FAITH, -1);
        virtue_add(VIRTUE_HONOUR, -1);
        virtue_add(VIRTUE_COMPASSION, -1);
        break;
    default:
        virtue_add(VIRTUE_KNOWLEDGE, 1);
        break;
    }
}

void virtue_on_cast_spell(int realm, int cost, int fail)
{
    switch (realm)
    {
    case REALM_LIFE:
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_TEMPERANCE, 1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_COMPASSION, 1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_VITALITY, 1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_DILIGENCE, 1);
        break;
    case REALM_BLESS:
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_TEMPERANCE, 1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_COMPASSION, 1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_VITALITY, 1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_HONOUR, 1);
        break;
    case REALM_DEATH:
    case REALM_NECROMANCY:
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_UNLIFE, 1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_JUSTICE, -1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_FAITH, -1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_VITALITY, -1);
        break;
    case REALM_DAEMON:
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_JUSTICE, -1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_FAITH, -1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_HONOUR, -1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_TEMPERANCE, -1);
        break;
    case REALM_CRUSADE:
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_FAITH, 1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_JUSTICE, 1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_SACRIFICE, 1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_HONOUR, 1);
        break;
    case REALM_NATURE:
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_NATURE, 1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_HARMONY, 1);
        break;
    case REALM_HEX:
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_JUSTICE, -1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_FAITH, -1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_HONOUR, -1);
        if (randint1(100 + plr->lev) < cost) virtue_add(VIRTUE_COMPASSION, -1);
        break;
    }
    if (randint1(100) < fail)
        virtue_add(VIRTUE_CHANCE,1);
}

/* Here is a return to Hengband's harshness ... but rather than
 * making the large penalties all or nothing, we'll distribute
 * the penalty based upon the alignment's deviation */
int virtue_mod_spell_fail(int realm, int fail)
{
    caster_info *caster_ptr = get_caster_info();
    int          max = 5;

    if (caster_ptr && caster_ptr->which_stat == A_WIS)
        max = 10;

    if (realm == REALM_NATURE)
    {
        /* Nature is a realm of balance, and the player must
         * strive to maintain neutrality */
        int align = abs(plr->align);
        if (align > 50)
        {
            int base = 1;
            int xtra = max - base;
            int mod = MIN(max, base + (align - 51) * xtra / 150);
            fail += mod;
        }
    }
    else if (is_good_realm(realm))
    {
        if (plr->align < -20)
        {
            int align = abs(plr->align);
            int base = 1;
            int xtra = max - base;
            int mod = MIN(max, base + (align - 21) * xtra / 130);
            fail += mod;
        }
        else if (plr->align > 150)
            fail -= 1;
    }
    else if (is_evil_realm(realm))
    {
        if (plr->align > 20)
        {
            int base = 1;
            int xtra = max - base;
            int mod = MIN(max, base + (plr->align - 21) * xtra / 130);
            fail += mod;
        }
        else if (plr->align < -150)
            fail -= 1;
    }
    return fail;
}


