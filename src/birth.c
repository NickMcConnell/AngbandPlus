/* Purpose: create a player character */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

#include "angband.h"
#include "z-doc.h"

/*
 * Returns adjusted stat -JK-  Algorithm by -JWT-
 */
static int adjust_stat(int value, int amount)
{
    int i;

    /* Negative amounts */
    if (amount < 0)
    {
        /* Apply penalty */
        for (i = 0; i < (0 - amount); i++)
        {
            if (value >= 18+10)
            {
                value -= 10;
            }
            else if (value > 18)
            {
                value = 18;
            }
            else if (value > 3)
            {
                value--;
            }
        }
    }

    /* Positive amounts */
    else if (amount > 0)
    {
        /* Apply reward */
        for (i = 0; i < amount; i++)
        {
            if (value < 18)
            {
                value++;
            }
            else
            {
                value += 10;
            }
        }
    }

    /* Return the result */
    return (value);
}

cptr realm_jouhou[VALID_REALM] =
{
"Life magic is very good for healing; it relies mostly on healing, protection and detection spells. Also life magic have a few attack spells as well. It said that some high level spell of life magic can disintegrate Undead monsters into ash.",
"Sorcery is a `meta` realm, including enchantment and general spells. It provides superb protection spells, spells to enhance your odds in combat and, most importantly, a vast selection of spells for gathering information. However, Sorcery has one weakness: it has no spells to deal direct damage to your enemies.",
"Nature magic makes you master of elements; it provides protection, detection, curing and attack spells. Nature also has a spell of Herbal Healing, which is the only powerful healing spell outside the realm of Life magic.",
"There are few types of magic more unpredictable and difficult to control than Chaos magic. Chaos is the very element of unmaking, and the Chaos spells are the most terrible weapons of destruction imaginable. The caster can also call on the primal forces of Chaos to induce mutations in his/her opponents and even him/herself.",
"There is no fouler nor more evil category of spells than the necromantic spells of Death Magic. These spells are relatively hard to learn, but at higher levels the spells give the caster power over living and the (un)dead, but the most powerful spells need his / her own blood as the focus, often hurting the caster in the process of casting.",
"Trump magic has, indeed, an admirable selection of teleportation spells. Since the Trump gateways can also be used to summon other creatures, Trump magic has an equally impressive selection of summoning spells. However, not all monsters appreciate being drawn to another place by Trump user.",
"Arcane magic is a general purpose realm of magic. It attempts to encompass all 'useful' spells from all realms. This is the downside of Arcane magic: while Arcane does have all the necessary 'tool' spells for a dungeon delver, it has no ultra-powerful high level spells. As a consequence, all Arcane spellbooks can be bought in town. It should also be noted that the 'specialized' realms usually offer the same spell at a lower level and cost. ",
"Craft magic can strengthen the caster or the equipments. These spells can greatly improve the caster's fighting ability. Using them against opponents directly is not possible.",
"Demon is a very evil realm, same as Death. It provides various attack spells and devilish detection spells. at higher levels, Demon magic provides ability to dominate demons, and to polymorph yourself into a demon.",
"Crusade is a magic of 'Justice'. It includes damage spells, which are greatly effective against foul and evil monsters, but have poor effects against good monsters.",
"Necromancy allows communication with and ultimately control over the deceased. All direct damage afforded by this realm requires the caster to touch his or her opponent. Any weapons or gloves will obstruct this macabre contact.",
"Armageddon is the most deadly offensive realm. You won't be lacking for firepower here. "
    "However, every spell is an offensive spell, so this realm suffers from a lack of any "
    "utility spells.",
"Music magic shows various effects as sing song. There is two type of song; the one which shows effects instantly and the other one shows effect continuously until SP runs out. But the latter type has a limit; only one song can be sing at the same time.",
"The books of Kendo describe about various combat techniques. When learning new techniques, you are required to carry the books, but once you memorizes them, you don't have to carry them. When using a technique, wielding a weapon is required.",
"Hex is a very terrible realm. Spells gives continual effects when they are spelled continually like songs. Spells may obstract monsters' actions, may deal damages in sight, may revenge against enemies.",
"The books of Rage describe various techniques. To learn a new technique, you must perform a ritual of rage, destroying the book in the process."
    " Once learned, you may use techniques without requiring the corresponding Rage book, but you will need to find many copies of each book in order"
    " to learn all of the techniques.",
"Burglary is the preferred realm of rogues, allowing them to specialize in what they do best: Stealing! "
    "This realm offers good detection and escapes, offers talents for picking pockets and setting traps, "
    "and even allows for direct assassination of sleeping monsters. The books for this realm are only "
    "available in the Black Market (or in the dungeon).",
};
static char realm_subinfo[VALID_REALM][128] =
{
"Good at detection and healing.",
"Utility and protective spells.",
"Good at detection and defence.",
"Offensive and destructive.",
"Ruins living creatures.",
"Good at summoning, teleportation.",
"Very useful but poor a bit.",
"Support for melee fighting.",
"Good at both offence and defence.",
"Destroys evil creatures.",
"Control and Commune with the dead.",
"Most powerful offensive magic.",
"Song with magical effects.",
"Special attacks on melee.",
"Good at obstacle and revenge.",
"Good at destroying spellcasting foes.",
"Good at stealth, stealing and escapes.",
};

typedef struct {
    cptr name;
    cptr desc;
} _name_desc_t;

#define _BIRTH_RESTART -1
#define _BIRTH_ESCAPE  -2

static void birth_quit(void);
static void show_help(cptr helpfile);

static int _menu_choose(menu_ptr menu, int start_choice)
{
    int  k, i, cs, os;
    char keys[100];
    char buf[80], cur[80];
    int  cur_color = TERM_WHITE;
    char c;

    if (menu->count == 1) return 0;
    if (start_choice < 0 || start_choice > menu->count)
        start_choice = menu->count;

    clear_from(10);
    put_str(menu->heading, 23, 5);

    /* List */
    for (i = 0; i < menu->count; i++)
    {
        variant key, text, color;
        var_init(&key);
        var_init(&text);
        var_init(&color);

        menu->fn(MENU_KEY, i, menu->cookie, &key);
        if (var_is_null(&key))
            keys[i] = I2A(i);
        else
            keys[i] = (char)var_get_int(&key);

        if (menu->count <= 26)
            keys[i] = tolower(keys[i]);
        
        menu->fn(MENU_TEXT, i, menu->cookie, &text);
        if (var_is_null(&text))
            var_set_string(&text, "");

        menu->fn(MENU_COLOR, i, menu->cookie, &color);
        if (var_is_null(&color))
            var_set_int(&color, TERM_WHITE);

        sprintf(buf, "%c) %s", keys[i], var_get_string(&text));
        if (i == start_choice)
        {
            sprintf(cur, "%s", buf);
            cur_color = var_get_int(&color);
            c_put_str(TERM_YELLOW, buf, 12 + i/3, 1 + 20 * (i%3));
        }
        else
            c_put_str(var_get_int(&color), buf, 12 + i/3, 1 + 20 * (i%3));

        var_clear(&key);
        var_clear(&text);
        var_clear(&color);
    }
    if (start_choice == menu->count)
    {
        sprintf(cur, "*) Random");
        cur_color = TERM_WHITE;
        c_put_str(TERM_YELLOW, "*) Random", 12 + menu->count/3, 1 + 20 * (menu->count%3));
    }
    else
        c_put_str(TERM_WHITE, "*) Random", 12 + menu->count/3, 1 + 20 * (menu->count%3));

    /* Choose */
    k = -1;
    cs = start_choice;
    os = /*start_choice? ... but I want the initial MENU_ON_BROWSE to display, so:*/-1;
    for (;;)
    {
        if (cs != os)
        {
            c_put_str(cur_color, cur, 12 + (os/3), 1 + 20 * (os%3));
            put_str("                                      ", 3, 40);
            put_str("                                      ", 4, 40);
            put_str("                                      ", 5, 40);
            if(cs == menu->count)
            {
                sprintf(cur, "*) Random");
                cur_color = TERM_WHITE;
            }
            else
            {
                variant res;
                var_init(&res);

                menu->fn(MENU_TEXT, cs, menu->cookie, &res);
                sprintf(cur, "%c) %s", keys[cs], var_get_string(&res));

                var_clear(&res);
                menu->fn(MENU_COLOR, cs, menu->cookie, &res);
                if (var_is_null(&res))
                    var_set_int(&res, TERM_WHITE);
                cur_color = var_get_int(&res);

                menu->fn(MENU_ON_BROWSE, cs, menu->cookie, &res);
                var_clear(&res);

            }
            c_put_str(TERM_YELLOW, cur, 12 + (cs/3), 1 + 20 * (cs%3));
            os = cs;
        }

        if (k >= 0) break;

        sprintf(buf, "Choose a %s (%c-%c) ('=' for options): ", menu->choose_prompt, keys[0], keys[menu->count - 1]);
        put_str(buf, 10, 10);
        c = inkey();
        if (c == 'Q') birth_quit();
        if (c == 'S') return _BIRTH_RESTART;
        if (c == ' ' || c == '\r' || c == '\n')
        {
            if(cs == menu->count)
            {
                k = randint0(menu->count);
                cs = k;
                continue;
            }
            else
            {
                k = cs;
                break;
            }
        }
        if (c == '*')
        {
            k = randint0(menu->count);
            cs = k;
            continue;
        }
        if (c == '8')
        {
            if (cs >= 3) cs -= 3;
        }
        if (c == '4')
        {
            if (cs > 0) cs--;
        }
        if (c == '6')
        {
            if (cs < menu->count) cs++;
        }
        if (c == '2')
        {
            if ((cs + 3) <= menu->count) cs += 3;
        }
        k = -1;
        for (i = 0; i < menu->count; i++)
        {
            if (menu->count <= 26)
            {
                if (toupper(keys[i]) == toupper(c))
                {
                    k = i;
                    cs = i;
                    continue;
                }
            }
            else if (keys[i] == c)
            {
                k = i;
                cs = i;
                continue;
            }
        }
        if (c == '?')
        {
            /* Race and Class helpfiles should have appropriately named topics */
            if (cs < menu->count && !strchr(menu->browse_prompt, '#'))
            {
                char    helpfile[255];
                variant text;

                var_init(&text);
                menu->fn(MENU_TEXT, cs, menu->cookie, &text);

                strcpy(helpfile, menu->browse_prompt);
                if (!var_is_null(&text))
                    sprintf(helpfile + strlen(helpfile), "#%s", var_get_string(&text));

                show_help(helpfile);
                var_clear(&text);
            }
            else
                show_help(menu->browse_prompt);
        }
        else if (c == '=')
        {
            screen_save();
            do_cmd_options_aux(OPT_PAGE_BIRTH, "Birth Option((*)s effect score)");
            screen_load();
        }
        else if (c == ESCAPE) return _BIRTH_ESCAPE;
        else if (c !='2' && c !='4' && c !='6' && c !='8') bell();
    }

    return k;
}

static int _count_ids(int *ns)
{
    int ct = 0;
    while (*ns++ != -1)
        ct++;
    return ct;
}
static int _find_id(int *ns, int id)
{
    int i = 0;

    for (i = 0; ns[i] != -1; i++)
    {
        if (ns[i] == id) break;
    }
    return i;
}
static bool _confirm_choice(cptr desc, int ct)
{
    char temp[80*30];
    cptr t;
    int i;
    bool result = FALSE;

    clear_from(10);
    roff_to_buf(desc, 74, temp, sizeof(temp));
    t = temp;

    for (i = 0; i < 30; i++)
    {
        if(t[0] == 0)
            break; 
        else
        {
            prt(t, 12+i, 3);
            t += strlen(t) + 1;
        }
    }
    if (ct > 1)
        result = get_check_strict("Are you sure? ", CHECK_DEFAULT_Y);
    else
    {
        prt("Hit any key.", 0, 0);
        (void)inkey();
        prt("", 0, 0);
        result = TRUE;
    }
    clear_from(10);
    return result;
}

static void _personality_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    int  idx = ((int*)cookie)[which];
    char buf[100];
    personality_ptr pers_ptr = get_personality_aux(idx);

    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, pers_ptr->name);
        break;
    case MENU_ON_BROWSE:
        c_put_str(TERM_L_BLUE, pers_ptr->name, 3, 40);
        put_str(": Personality modification", 3, 40+strlen(pers_ptr->name));
        put_str("Str  Int  Wis  Dex  Con  Chr   EXP ", 4, 40);
        sprintf(buf, "%+3d  %+3d  %+3d  %+3d  %+3d  %+3d %4d%% ",
            pers_ptr->stats[A_STR], pers_ptr->stats[A_INT], pers_ptr->stats[A_WIS],
            pers_ptr->stats[A_DEX], pers_ptr->stats[A_CON], pers_ptr->stats[A_CHR],
            pers_ptr->exp);
        c_put_str(TERM_L_BLUE, buf, 5, 40);
        var_set_bool(res, TRUE);
        break;
    }
}

static bool _valid_personality(int which)
{
    if (which == PERS_SEXY && p_ptr->psex == SEX_MALE) return FALSE;
    if (which == PERS_LUCKY && p_ptr->psex == SEX_FEMALE) return FALSE;
    return TRUE;
}

static int _prompt_personality(void)
{
    if (game_mode == GAME_MODE_BEGINNER)
    {
        p_ptr->personality = PERS_ORDINARY;
        return p_ptr->personality;
    }

    for (;;)
    {
        char   tmp[80];
        int    idx, ct = 0, i;
        int    choices[MAX_PERSONALITIES];
        personality_ptr pers_ptr = NULL;
        menu_t menu = { "Personality", "Personalities.txt", "Note: Your personality determines various intrinsic abilities and bonuses.",
                            _personality_menu_fn, 
                            choices, 0};

        for (i = 0; i < MAX_PERSONALITIES; i++)
        {
            if (_valid_personality(i))
                choices[ct++] = i;
        }
        choices[ct] = -1;
        menu.count = ct;

        strcpy(tmp, player_name);
        c_put_str(TERM_L_BLUE, tmp, 1, 34);
        c_put_str(TERM_WHITE, "              ", 3, 14);
        idx = _menu_choose(&menu, _find_id(choices, p_ptr->personality));
        if (idx < 0) return idx;

        p_ptr->personality = choices[idx];
        pers_ptr = get_personality();
        
        c_put_str(TERM_L_BLUE, format("%-14s", pers_ptr->name), 3, 14);

        if (!_confirm_choice(pers_ptr->desc, menu.count)) continue;
        return p_ptr->personality;
    }
    /*return _BIRTH_ESCAPE;  unreachable */
}

static void _realm_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    int idx = ((int *)cookie)[which];
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, realm_names[idx]);
        break;
    case MENU_ON_BROWSE:
        c_put_str(TERM_L_BLUE, realm_names[idx], 3, 40);
        put_str(": Characteristic", 3, 40+strlen(realm_names[idx]));
        put_str(realm_subinfo[technic2magic(idx)-1], 4, 40);
        var_set_bool(res, TRUE);
        break;
    }
}

static int _prompt_realm2(void)
{
    c_put_str(TERM_L_BLUE, format("%-20s", realm_names[p_ptr->realm1]), 7, 14);
    if (!realm_choices2[p_ptr->pclass]) 
    {
        p_ptr->realm2 = 0;
        return _prompt_personality();
    }
    for (;;)
    {
        u32b   bits = realm_choices2[p_ptr->pclass];
        int    choices[MAX_REALM];
        int    ct = 0;
        int    idx, i;
        menu_t menu = { "Realm", "magic.txt#MagicRealms", "Note: The realm of magic will determine which spells you can learn.",
                            _realm_menu_fn, 
                            choices, 0};

        if (p_ptr->pclass == CLASS_PRIEST)
        {
            if (is_good_realm(p_ptr->realm1))
                bits &= ~(CH_DEATH | CH_DAEMON);
            else
                bits &= ~(CH_LIFE | CH_CRUSADE);
        }
        for (i = 0; i < 32; i++)
        {
            if (bits & (1L << i) && p_ptr->realm1 != i+1)
                choices[ct++] = i+1;
        }
        menu.count = ct;
        choices[ct] = -1;
        c_put_str(TERM_L_BLUE, format("%-20s", realm_names[p_ptr->realm1]), 7, 14);
        idx = _menu_choose(&menu, _find_id(choices, p_ptr->realm2));
        if (idx < 0) return idx;
        p_ptr->realm2 = choices[idx];
        c_put_str(TERM_L_BLUE, format("%s, %s", realm_names[p_ptr->realm1], realm_names[p_ptr->realm2]), 7, 14);
        if (!_confirm_choice(realm_jouhou[technic2magic(p_ptr->realm2)-1], menu.count)) continue;
        idx = _prompt_personality();
        if (idx == _BIRTH_ESCAPE)
        {
            if (menu.count == 1) return idx;
            continue;
        }
        return idx;
    }
    /* return _BIRTH_ESCAPE; unreachable */
}

static void _dragon_realm_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    int  idx = ((int*)cookie)[which];
    char buf[100];
    dragon_realm_ptr realm = dragon_get_realm(idx);

    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, realm->name);
        break;
    case MENU_ON_BROWSE:
        c_put_str(TERM_L_BLUE, realm->name, 3, 40);
        put_str(": Realm modification", 3, 40+strlen(realm->name));
        put_str("Str  Int  Wis  Dex  Con  Chr   Exp ", 4, 40);
        sprintf(buf, "%+3d  %+3d  %+3d  %+3d  %+3d  %+3d %4d%% ",           
            realm->stats[A_STR], realm->stats[A_INT], realm->stats[A_WIS], 
            realm->stats[A_DEX], realm->stats[A_CON], realm->stats[A_CHR], 
            realm->exp);
        c_put_str(TERM_L_BLUE, buf, 5, 40);
        var_set_bool(res, TRUE);
        break;
    case MENU_COLOR:
        if (!realm->desc || !strlen(realm->desc))
            var_set_int(res, TERM_L_DARK);
        else
            var_set_int(res, TERM_WHITE);
        break;
    }
}

static int _prompt_realm1(void)
{
    put_str("Realm      :", 7, 1);
    c_put_str(TERM_L_BLUE, format("%-20s", ""), 7, 14);

    if (p_ptr->prace == RACE_MON_DRAGON)
    {
        p_ptr->realm1 = p_ptr->realm2 = 0;
        if (p_ptr->psubrace == DRAGON_STEEL)
        {
            p_ptr->dragon_realm = DRAGON_REALM_NONE;
            return _prompt_personality();
        }
        else
        {
            for (;;)
            {
                int    idx, ct = 0;
                int    choices[DRAGON_REALM_MAX];
                menu_t menu = { "Dragon Realm", "DragonRealms.txt", "", _dragon_realm_menu_fn, choices, 0 };
                dragon_realm_ptr realm = NULL;
                
                /* TODO: Subracial restrictions? */
                choices[ct++] = DRAGON_REALM_LORE;
                choices[ct++] = DRAGON_REALM_BREATH;
                choices[ct++] = DRAGON_REALM_ATTACK;
                choices[ct++] = DRAGON_REALM_CRAFT;
                choices[ct++] = DRAGON_REALM_ARMOR;
                choices[ct++] = DRAGON_REALM_DOMINATION;
                if (p_ptr->psubrace == DRAGON_LAW || p_ptr->psubrace == DRAGON_GOLD)
                    choices[ct++] = DRAGON_REALM_CRUSADE;
                if (p_ptr->psubrace == DRAGON_NETHER || p_ptr->psubrace == DRAGON_CHAOS)
                    choices[ct++] = DRAGON_REALM_DEATH;

                menu.count = ct;
                choices[ct] = -1;

                c_put_str(TERM_WHITE, "                   ", 7, 14);
                idx = _menu_choose(&menu, _find_id(choices, p_ptr->dragon_realm));
                if (idx < 0) return idx;
                p_ptr->dragon_realm = choices[idx];
                realm = dragon_get_realm(p_ptr->dragon_realm);
                c_put_str(TERM_L_BLUE, realm->name, 7, 14);
                if (!_confirm_choice(realm->desc, menu.count)) continue;
                idx = _prompt_personality();
                if (idx == _BIRTH_ESCAPE) continue;
                return idx;
            }
        }
    }
    p_ptr->dragon_realm = 0;
    if (!realm_choices1[p_ptr->pclass]) 
    {
        p_ptr->realm1 = p_ptr->realm2 = 0;
        return _prompt_personality();
    }
    for (;;)
    {
        u32b   bits = realm_choices1[p_ptr->pclass];
        int    choices[MAX_REALM];
        int    ct = 0;
        int    idx, i;
        menu_t menu = { "Realm", "magic.txt#MagicRealms", "Note: The realm of magic will determine which spells you can learn.",
                            _realm_menu_fn, 
                            choices, 0};
        for (i = 0; i < 32; i++)
        {
            if (bits & (1L << i))
                choices[ct++] = i+1;
        }
        menu.count = ct;
        choices[ct] = -1;
        c_put_str(TERM_L_BLUE, format("%-20s", ""), 7, 14);
        idx = _menu_choose(&menu, _find_id(choices, p_ptr->realm1));
        if (idx < 0) return idx;
        p_ptr->realm1 = choices[idx];
        c_put_str(TERM_L_BLUE, realm_names[p_ptr->realm1], 7, 14);
        if (!_confirm_choice(realm_jouhou[technic2magic(p_ptr->realm1)-1], menu.count)) continue;
        idx = _prompt_realm2();
        if (idx == _BIRTH_ESCAPE)
        {
            if (menu.count == 1) 
            {
                c_put_str(TERM_L_BLUE, format("%-20s", ""), 7, 14);
                return idx;
            }
            continue;
        }
        return idx;
    }    
    /* return _BIRTH_ESCAPE; unreachable */
}

#define _MAX_CLASSES_PER_GROUP 20
#define _MAX_CLASS_GROUPS      11
typedef struct _class_group_s {
    cptr name;
    int ids[_MAX_CLASSES_PER_GROUP];
} _class_group_t;
static _class_group_t _class_groups[_MAX_CLASS_GROUPS] = {
    { "Melee", {CLASS_BERSERKER, CLASS_BLOOD_KNIGHT, CLASS_DUELIST, CLASS_MAULER, 
                    CLASS_RUNE_KNIGHT, CLASS_SAMURAI, CLASS_WARRIOR, CLASS_WEAPONMASTER, 
                    CLASS_WEAPONSMITH, -1} }, 
    { "Archery", {CLASS_ARCHER, CLASS_SNIPER, -1} },
    { "Martial Arts", {CLASS_FORCETRAINER, CLASS_MONK, CLASS_MYSTIC, -1} },
    { "Magic", {CLASS_BLOOD_MAGE, CLASS_BLUE_MAGE, CLASS_HIGH_MAGE, CLASS_MAGE, 
                    CLASS_NECROMANCER, CLASS_SORCERER, -1} },
    { "Devices", {CLASS_DEVICEMASTER, CLASS_MAGIC_EATER, -1} },
    { "Prayer", {CLASS_PRIEST, -1} },
    { "Stealth", {CLASS_NINJA, CLASS_ROGUE, CLASS_SCOUT, -1} },
    { "Hybrid", {CLASS_CHAOS_WARRIOR, CLASS_PALADIN, CLASS_RANGER, CLASS_RED_MAGE, 
                    CLASS_WARRIOR_MAGE, -1} },
    { "Riding", {CLASS_BEASTMASTER, CLASS_CAVALRY, -1} },
    { "Mind", {CLASS_MINDCRAFTER, CLASS_MIRROR_MASTER, CLASS_PSION, 
                    CLASS_TIME_LORD, CLASS_WARLOCK, -1} },
    { "Other", {CLASS_ARCHAEOLOGIST, CLASS_BARD, CLASS_IMITATOR, CLASS_RAGE_MAGE, 
                    CLASS_TOURIST, CLASS_WILD_TALENT, -1} },
};

static void _class_group_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, _class_groups[which].name);
        break;
    }
}

static void _class_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    int idx = ((int *)cookie)[which];
    char buf[100];
    class_t *class_ptr = get_class_aux(idx, 0);

    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, class_ptr->name);
        break;
    case MENU_ON_BROWSE:
        c_put_str(TERM_L_BLUE, class_ptr->name, 3, 40);
        put_str(": Class modification", 3, 40+strlen(class_ptr->name));
        put_str("Str  Int  Wis  Dex  Con  Chr   EXP ", 4, 40);
        sprintf(buf, "%+3d  %+3d  %+3d  %+3d  %+3d  %+3d %4d%% ",
            class_ptr->stats[A_STR], class_ptr->stats[A_INT], class_ptr->stats[A_WIS], 
            class_ptr->stats[A_DEX], class_ptr->stats[A_CON], class_ptr->stats[A_CHR], 
            class_ptr->exp);
        c_put_str(TERM_L_BLUE, buf, 5, 40);
        var_set_bool(res, TRUE);
        break;
    }
}

static void _warlock_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    class_t *class_ptr = get_class_aux(CLASS_WARLOCK, which);
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, class_ptr->subname);
        break;
    }
}

static void _weaponmaster_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, weaponmaster_speciality_name(which));
        break;
    }
}

static void _devicemaster_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, devicemaster_speciality_name(which));
        break;
    }
}

static bool _valid_class(int which)
{
    if (which == CLASS_BLOOD_KNIGHT || which == CLASS_BLOOD_MAGE)
    {
        if (get_race()->flags & RACE_IS_NONLIVING)
            return FALSE;
    }
    if (which == CLASS_BEASTMASTER || which == CLASS_CAVALRY)
    {
        if (p_ptr->prace == RACE_CENTAUR)
            return FALSE;
    }
    if (which == CLASS_DUELIST || which == CLASS_MAULER)
    {
        if (p_ptr->prace == RACE_TONBERRY)
            return FALSE;
    }
    return TRUE;
}

static int _beginner_classes[] = {
  CLASS_WARRIOR,
  CLASS_MAGE,
  CLASS_PRIEST,
  CLASS_ROGUE,
  CLASS_RANGER,
  CLASS_PALADIN,
  CLASS_NINJA,
  -1
};

static int _prompt_class_beginner(void)
{
    int idx;
    for (;;)
    {
    class_t *class_ptr = NULL;
    menu_t menu = { "Class", "Classes.txt", "Note: Your 'class' determines various intrinsic factors and bonuses.",
                        _class_menu_fn,
                        _beginner_classes, _count_ids(_beginner_classes)};


        c_put_str(TERM_WHITE, "              ", 6, 14);
        c_put_str(TERM_WHITE, "              ", 7, 14);
        idx = _menu_choose(&menu, _find_id(_beginner_classes, p_ptr->pclass));
        if (idx < 0) return idx;

        if (_beginner_classes[idx] != p_ptr->pclass)
        {
            p_ptr->pclass = _beginner_classes[idx];
            p_ptr->psubclass = 0;
            p_ptr->realm1 = 0;
            p_ptr->realm2 = 0;
            p_ptr->dragon_realm = 0;
        }
        mp_ptr = &m_info[p_ptr->pclass];
        class_ptr = get_class();

        c_put_str(TERM_L_BLUE, format("%-14s", class_ptr->name), 6, 14);
        if (!_confirm_choice(class_ptr->desc, menu.count)) continue;
        p_ptr->psubclass = 0;
        c_put_str(TERM_WHITE, "              ", 7, 14);
        idx = _prompt_realm1();
        if (idx == _BIRTH_ESCAPE) continue;
        return idx;
    }
    /*return _BIRTH_ESCAPE;  unreachable */
}

static int _prompt_class(void)
{
    if (get_race()->flags & RACE_IS_MONSTER)
    {
        p_ptr->pclass = CLASS_MONSTER;
        mp_ptr = &m_info[p_ptr->pclass];
        p_ptr->psubclass = 0;
        p_ptr->realm1 = p_ptr->realm2 = 0;
        p_ptr->dragon_realm = 0;
        c_put_str(TERM_L_BLUE, "Monster", 6, 14);

        if (p_ptr->prace == RACE_MON_DRAGON)
            return _prompt_realm1();
        else
            return _prompt_personality();
    }

    for (;;)
    {
        int idx;
        int group_id = 0;

        c_put_str(TERM_WHITE, "              ", 6, 14);
        for (;;)
        {
            menu_t menu1 = { "Class Type", "Classes.txt", "",
                                _class_group_menu_fn, 
                                NULL, _MAX_CLASS_GROUPS};
            idx = _menu_choose(&menu1, group_id);
            if (idx < 0) return idx;
            group_id = idx;
            for (;;)
            {
            int      choices[_MAX_CLASSES_PER_GROUP];
            int      ct = 0, i;
            class_t *class_ptr = NULL;
            menu_t   menu2 = { "Class", "Classes.txt", "Note: Your 'class' determines various intrinsic factors and bonuses.",
                                _class_menu_fn, 
                                choices, 0};

                for (i = 0; ; i++)
                {
                    int id = _class_groups[group_id].ids[i];
                    if (id == -1) break;
                    if (_valid_class(id))
                        choices[ct++] = id;
                }

                if (!ct) /* TODO: Give an error message! */
                    break;

                choices[ct] = -1;
                menu2.count = ct;
                c_put_str(TERM_WHITE, "              ", 6, 14);
                c_put_str(TERM_WHITE, "              ", 7, 14);
                idx = _menu_choose(&menu2, _find_id(choices, p_ptr->pclass));
                if (idx == _BIRTH_ESCAPE) break;
                if (idx < 0) return idx;

                if (choices[idx] != p_ptr->pclass)
                {
                    p_ptr->pclass = choices[idx];
                    p_ptr->psubclass = 0;
                    p_ptr->realm1 = 0;
                    p_ptr->realm2 = 0;
                    p_ptr->dragon_realm = 0;
                }
                mp_ptr = &m_info[p_ptr->pclass];
                class_ptr = get_class();

                c_put_str(TERM_L_BLUE, format("%-14s", class_ptr->name), 6, 14);
                if (!_confirm_choice(class_ptr->desc, menu2.count)) continue;
                if (p_ptr->pclass == CLASS_WARLOCK)
                {
                    for (;;)
                    {
                    menu_t menu3 = { "Pact", "Warlocks.txt", "Its time to make your pact.",
                                        _warlock_menu_fn, 
                                        NULL, WARLOCK_MAX};
                        c_put_str(TERM_WHITE, "              ", 7, 14);
                        idx = _menu_choose(&menu3, p_ptr->psubclass);
                        if (idx == _BIRTH_ESCAPE) break;
                        if (idx < 0) return idx;
                        p_ptr->psubclass = idx;
                        class_ptr = get_class();
                        c_put_str(TERM_L_BLUE, format("%-14s", class_ptr->subname), 7, 14);
                        if (!_confirm_choice(class_ptr->subdesc, menu3.count)) continue;
                        idx = _prompt_personality();
                        if (idx == _BIRTH_ESCAPE) continue;
                        return idx;
                    }
                }
                else if (p_ptr->pclass == CLASS_WEAPONMASTER)
                {
                    for (;;)
                    {
                    menu_t menu3 = { "Speciality", "Weaponmasters.txt", "It is time to specialize.",
                                        _weaponmaster_menu_fn, 
                                        NULL, WEAPONMASTER_MAX};
                        c_put_str(TERM_WHITE, "              ", 7, 14);
                        idx = _menu_choose(&menu3, p_ptr->psubclass);
                        if (idx == _BIRTH_ESCAPE) break;
                        if (idx < 0) return idx;
                        p_ptr->psubclass = idx;
                        class_ptr = get_class();
                        c_put_str(TERM_L_BLUE, format("%-14s", class_ptr->subname), 7, 14);
                        if (!_confirm_choice(class_ptr->subdesc, menu3.count)) continue;
                        idx = _prompt_personality();
                        if (idx == _BIRTH_ESCAPE) continue;
                        return idx;
                    }
                }
                else if (p_ptr->pclass == CLASS_DEVICEMASTER)
                {
                    for (;;)
                    {
                    menu_t menu3 = { "Speciality", "Devicemasters.txt", "It is time to specialize.",
                                        _devicemaster_menu_fn, 
                                        NULL, DEVICEMASTER_MAX};
                        c_put_str(TERM_WHITE, "              ", 7, 14);
                        idx = _menu_choose(&menu3, p_ptr->psubclass);
                        if (idx == _BIRTH_ESCAPE) break;
                        if (idx < 0) return idx;
                        p_ptr->psubclass = idx;
                        c_put_str(TERM_L_BLUE, format("%-14s", devicemaster_speciality_name(p_ptr->psubclass)), 7, 14);
                        if (!_confirm_choice(devicemaster_speciality_desc(p_ptr->psubclass), menu3.count)) continue;
                        idx = _prompt_personality();
                        if (idx == _BIRTH_ESCAPE) continue;
                        return idx;
                    }
                }
                else
                {
                    p_ptr->psubclass = 0;
                    c_put_str(TERM_WHITE, "              ", 7, 14);
                    idx = _prompt_realm1();
                    if (idx == _BIRTH_ESCAPE)
                    {
                        if (menu2.count == 1) break;
                        continue;
                    }
                    return idx;
                }
            }
        }
    }
    /*return _BIRTH_ESCAPE;  unreachable */
}

#define _MAX_RACES_PER_GROUP 23
#define _MAX_RACE_GROUPS      8
typedef struct _race_group_s {
    cptr name;
    cptr help;
    int ids[_MAX_RACES_PER_GROUP];
} _race_group_t;
static _race_group_t _race_groups[_MAX_RACE_GROUPS] = {
    { "Human", "Races.txt",
        {RACE_AMBERITE, RACE_BARBARIAN, RACE_DEMIGOD, RACE_DUNADAN, RACE_HUMAN, -1} },
    { "Elf", "Races.txt",
        {RACE_DARK_ELF, RACE_HIGH_ELF, RACE_WOOD_ELF, -1} },
    { "Hobbit/Dwarf", "Races.txt",
        {RACE_DWARF, RACE_GNOME, RACE_HOBBIT, RACE_NIBELUNG, -1} },
    { "Fairy", "Races.txt",
        {RACE_SHADOW_FAIRY, RACE_SPRITE, -1} },
    { "Angel/Demon",  "Races.txt",
        {RACE_ARCHON, RACE_BALROG, RACE_IMP, -1} },
    { "Orc/Troll/Giant", "Races.txt",
        {RACE_CYCLOPS, RACE_HALF_GIANT, RACE_HALF_OGRE,
         RACE_HALF_TITAN, RACE_HALF_TROLL, RACE_KOBOLD, RACE_SNOTLING, -1} },
    { "Undead", "Races.txt",
        {RACE_SKELETON, RACE_SPECTRE, RACE_VAMPIRE, RACE_ZOMBIE, -1} },
    { "Other", "Races.txt",
        {RACE_ANDROID, RACE_BEASTMAN, RACE_CENTAUR, RACE_DRACONIAN, RACE_DOPPELGANGER, RACE_ENT, 
         RACE_GOLEM, RACE_KLACKON, RACE_KUTAR, RACE_MIND_FLAYER, RACE_TONBERRY, RACE_YEEK,-1 } },
};

#define _MAX_MON_RACE_GROUPS      12
static _race_group_t _mon_race_groups[_MAX_MON_RACE_GROUPS] = {
    { "Animal", "MonsterRaces.txt",
        {/*RACE_MON_ANT, RACE_MON_BEETLE, RACE_MON_BIRD, RACE_MON_CAT,*/ RACE_MON_CENTIPEDE,
            RACE_MON_HOUND, /*RACE_MON_HORSE, */ RACE_MON_HYDRA, RACE_MON_SPIDER, -1} },
    { "Angel/Demon", "MonsterRaces.txt",
        {RACE_MON_ANGEL, RACE_MON_DEMON, -1} },
    { "Beholder",  "MonsterRaces.txt#Beholder",
        {RACE_MON_BEHOLDER, -1} },
    { "Dragon",  "MonsterRaces.txt#Dragon",
        {RACE_MON_DRAGON, -1} },
    { "Elemental",  "MonsterRaces.txt#Elemental",
        {RACE_MON_ELEMENTAL, -1} },
    { "Golem",  "MonsterRaces.txt#Golem",
        {RACE_MON_GOLEM, -1} },
    { "Jelly",  "MonsterRaces.txt",
        {RACE_MON_JELLY, /*RACE_MON_MOLD,*/ RACE_MON_QUYLTHULG, -1} },
    { "Leprechaun",  "MonsterRaces.txt#Leprechaun",
        {RACE_MON_LEPRECHAUN, -1} },
    { "Mimic/Possessor", "MonsterRaces.txt",
        {RACE_MON_SWORD, /*RACE_MON_ARMOR,*/ RACE_MON_MIMIC, RACE_MON_POSSESSOR, RACE_MON_RING, -1} },
    { "Orc/Troll/Giant", "MonsterRaces.txt",
        {RACE_MON_GIANT, /*RACE_MON_KOBOLD, RACE_MON_ORC,*/ RACE_MON_TROLL, -1} },
    { "Undead", "MonsterRaces.txt",
        {/*RACE_MON_GHOST,*/ RACE_MON_LICH, RACE_MON_VAMPIRE, /*RACE_MON_WRAITH, RACE_MON_ZOMBIE,*/ -1 } },
    { "Xorn",  "MonsterRaces.txt#Xorn",
        {RACE_MON_XORN, -1} },
};

static void _race_group_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, _race_groups[which].name);
        break;
    }
}

static void _mon_race_group_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, _mon_race_groups[which].name);
        break;
    }
}

static void _race_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    int idx = ((int *)cookie)[which];
    char buf[100];
    race_t *race_ptr = get_race_aux(idx, 0);

    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, race_ptr->name);
        break;
    case MENU_ON_BROWSE:
        c_put_str(TERM_L_BLUE, race_ptr->name, 3, 40);
        put_str(": Race modification", 3, 40+strlen(race_ptr->name));
        put_str("Str  Int  Wis  Dex  Con  Chr   EXP ", 4, 40);
        sprintf(buf, "%+3d  %+3d  %+3d  %+3d  %+3d  %+3d %4d%% ",
            race_ptr->stats[A_STR], race_ptr->stats[A_INT], race_ptr->stats[A_WIS], 
            race_ptr->stats[A_DEX], race_ptr->stats[A_CON], race_ptr->stats[A_CHR], 
            race_ptr->exp);
        c_put_str(TERM_L_BLUE, buf, 5, 40);
        var_set_bool(res, TRUE);
        break;
    }
}

static void _demigod_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    switch (cmd)
    {
    case MENU_TEXT:
    {
        race_t *race_ptr = get_race_aux(RACE_DEMIGOD, which);
        var_set_string(res, race_ptr->subname);
        break;
    }
    case MENU_ON_BROWSE:
    {
        char buf[100];
        race_t *race_ptr = get_race_aux(RACE_DEMIGOD, which);

        c_put_str(TERM_L_BLUE, race_ptr->subname, 3, 40);
        put_str(": Race modification", 3, 40+strlen(race_ptr->subname));
        put_str("Str  Int  Wis  Dex  Con  Chr   EXP ", 4, 40);
        sprintf(buf, "%+3d  %+3d  %+3d  %+3d  %+3d  %+3d %+4d%% ",
            race_ptr->stats[A_STR], race_ptr->stats[A_INT], race_ptr->stats[A_WIS], 
            race_ptr->stats[A_DEX], race_ptr->stats[A_CON], race_ptr->stats[A_CHR], 
            race_ptr->exp);
        c_put_str(TERM_L_BLUE, buf, 5, 40);

        var_set_bool(res, TRUE);
        break;
    }
    }
}

static _name_desc_t _troll_info[TROLL_MAX] = {
    { "Ettin", "Ettins are large, two-headed trolls. They lack much in the way of "
                "powers and abilities, but make up for this with the ability to "
                "wield an extra helmet." },
    { "Storm Troll", "Storm Trolls are fast trolls with elemental powers. They may call "
                        "forth elemental balls and bolts. Their weapons are wreathed in "
                        "electricity as their fury rains down on all they meet." },
    { "Spirit Troll", "Spirit trolls may pass through walls on their quest to demolish "
                        "all that oppose them." },
    { "Troll King", "Troll Kings are lords of their kind, fast and extremely deadly in "
                        "melee. They may blink themselves out of harms way." },
};
static void _troll_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, _troll_info[which].name);
        break;
    case MENU_ON_BROWSE:
    {
        char buf[100];
        race_t *race_ptr = get_race_aux(RACE_MON_TROLL, which);

        c_put_str(TERM_L_BLUE, _troll_info[which].name, 3, 40);
        put_str(": Race modification", 3, 40+strlen(_troll_info[which].name));
        put_str("Str  Int  Wis  Dex  Con  Chr   EXP ", 4, 40);
        sprintf(buf, "%+3d  %+3d  %+3d  %+3d  %+3d  %+3d %+4d%% ",
            race_ptr->stats[A_STR], race_ptr->stats[A_INT], race_ptr->stats[A_WIS], 
            race_ptr->stats[A_DEX], race_ptr->stats[A_CON], race_ptr->stats[A_CHR], 
            race_ptr->exp);
        c_put_str(TERM_L_BLUE, buf, 5, 40);

        var_set_bool(res, TRUE);
        break;
    }
    }
}

static _name_desc_t _giant_info[GIANT_MAX] = {
    { "Fire Giant", "Fire Giants are massive giants of flame. At high levels they become "
                        "wreathed in flames and even their weapons will burn their foes. Like "
                        "all giants, they may toss loose rubble at their foes. In addition, "
                        "they have a few fire based distance attacks up their sleeves." },
    { "Frost Giant", "Frost Giants are massive giants of ice. At high levels they become "
                        "wreathed in cold and even their weapons will freeze their foes. Like "
                        "all giants, they may toss loose rubble at their foes. In addition, "
                        "they have a few cold based distance attacks up their sleeves." },
    { "Storm Giant", "Storm Giants are massive giants of lightning. At high levels they become "
                        "wreathed in electricity and even their weapons will shock their foes. Like "
                        "all giants, they may toss loose rubble at their foes. In addition, "
                        "they have a few lightning based distance attacks up their sleeves." },
    { "Titan", "Titans are huge immortal beings of incredible strength and awesome power. "
                "Descended from Gaia and Uranus, they ruled during the legendary Golden Age, "
                "but were overthrown by the Olympians during the War of the Titans." },
    { "Hru", "Hrus are rock giants, made of stone. Their hides are tough and they are able "
                "to break through walls effortlessly. Hrus are incredibly strong, but lack "
                "much in the way of magical powers." },
};
static void _giant_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, _giant_info[which].name);
        break;
    case MENU_ON_BROWSE:
    {
        char buf[100];
        race_t *race_ptr = get_race_aux(RACE_MON_GIANT, which);

        c_put_str(TERM_L_BLUE, _giant_info[which].name, 3, 40);
        put_str(": Race modification", 3, 40+strlen(_giant_info[which].name));
        put_str("Str  Int  Wis  Dex  Con  Chr   EXP ", 4, 40);
        sprintf(buf, "%+3d  %+3d  %+3d  %+3d  %+3d  %+3d %+4d%% ",
            race_ptr->stats[A_STR], race_ptr->stats[A_INT], race_ptr->stats[A_WIS], 
            race_ptr->stats[A_DEX], race_ptr->stats[A_CON], race_ptr->stats[A_CHR], 
            race_ptr->exp);
        c_put_str(TERM_L_BLUE, buf, 5, 40);

        var_set_bool(res, TRUE);
        break;
    }
    }
}

static _name_desc_t _golem_info[GOLEM_MAX] = {
    { "Colossus", "The Colossus is the biggest of all golems, truly immense. "
                  "Unfortunately, they are also the slowest of all golems on "
                  "account of their great size." },
    { "Sky Golem", "The Sky Golem is the product of powerful enchantments, resistant "
                   "to the ravages of time. They may even breathe time!" },
    { "Spellwarp Automaton", "The Spellwarp Automaton is nearly indestructible, being "
                             "almost completely immune to magic. However, their great "
                             "power takes a seeming eternity to mature." },
};

static void _golem_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, _golem_info[which].name);
        break;
    case MENU_ON_BROWSE:
    {
        char buf[100];
        race_t *race_ptr = get_race_aux(RACE_MON_GOLEM, which);

        c_put_str(TERM_L_BLUE, _golem_info[which].name, 3, 40);
        put_str(": Race modification", 3, 40+strlen(_golem_info[which].name));
        put_str("Str  Int  Wis  Dex  Con  Chr   EXP ", 4, 40);
        sprintf(buf, "%+3d  %+3d  %+3d  %+3d  %+3d  %+3d %+4d%% ",
            race_ptr->stats[A_STR], race_ptr->stats[A_INT], race_ptr->stats[A_WIS], 
            race_ptr->stats[A_DEX], race_ptr->stats[A_CON], race_ptr->stats[A_CHR], 
            race_ptr->exp);
        c_put_str(TERM_L_BLUE, buf, 5, 40);

        var_set_bool(res, TRUE);
        break;
    }
    }
}

static _name_desc_t _spider_info[SPIDER_MAX] = {
    { "Phase Spider", "Phase Spiders have unsurpassed powers of teleportation and average offense." },
    { "Aranea", "Aranea are stronger in melee than Phase Spiders but lack special powers except for their Spider Web." },
};
static void _spider_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, _spider_info[which].name);
        break;
    }
}

static void _dragon_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, get_race_aux(RACE_MON_DRAGON, which)->subname);
        break;
    case MENU_ON_BROWSE:
    {
        char buf[100];
        race_t *race_ptr = get_race_aux(RACE_MON_DRAGON, which);

        c_put_str(TERM_L_BLUE, race_ptr->subname, 3, 40);
        put_str(": Race modification", 3, 40+strlen(race_ptr->subname));
        put_str("Str  Int  Wis  Dex  Con  Chr   EXP ", 4, 40);
        sprintf(buf, "%+3d  %+3d  %+3d  %+3d  %+3d  %+3d %+4d%% ",
            race_ptr->stats[A_STR], race_ptr->stats[A_INT], race_ptr->stats[A_WIS], 
            race_ptr->stats[A_DEX], race_ptr->stats[A_CON], race_ptr->stats[A_CHR], 
            race_ptr->exp);
        c_put_str(TERM_L_BLUE, buf, 5, 40);

        var_set_bool(res, TRUE);
        break;
    }
    }
}
static _name_desc_t _draconian_info[DRAGON_MAX] = {
    { "Red",
        "Red Draconians are resistant to fire and may scorch enemies with their fiery breath. "
        "They are strong and fight very well though their skill with magic is slightly diminished." },
    { "White",
        "White Draconians are resistant to cold and may freeze enemies with their icy breath. "
        "They are strong and fight very well though their skill with magic is slightly diminished." },
    { "Blue",
        "Blue Draconians are resistant to lightning and may shock enemies with their breath. "
        "They are strong and fight very well though their skill with magic is slightly diminished." },
    { "Black",
        "Black Draconians are resistant to acid and may dissolve enemies with their breath. "
        "They are strong and fight very well though their skill with magic is slightly diminished." },
    { "Green",
        "Green Draconians are resistant to poison which they may breathe at will." },
    { "Bronze",
        "Bronze Draconians are seldom confused, though the same may not be said of their enemies! "
        "They are the smartest of their kind and use magical devices very well indeed." },
    { "Crystal",
        "Crystal Draconians are resistant to shards, have tough hides, and eventually reflect bolt spells. "
        "They may breathe shards at will, are very hardy, and are excellent in melee. "
        "Unfortunately, they are neither smart nor nimble, and find magical devices a bit difficult." },
    { "Gold",
        "Gold Draconians are resistant to sound which they may also breathe at will. "
        "They are very wise and have enhanced magic resistance. They are also very good with magical devices." },
    { "Shadow",
        "Shadow Draconians are resistant to nether which they may also breathe at will. "
        "Cloaked in darkness, they are very stealthy and are the most nimble of their kind, "
        "though not particularly strong." },
};
static void _draconian_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, _draconian_info[which].name);
        break;
    case MENU_ON_BROWSE:
    {
        char buf[100];
        race_t *race_ptr = get_race_aux(RACE_DRACONIAN, which);

        c_put_str(TERM_L_BLUE, _draconian_info[which].name, 3, 40);
        put_str(": Race modification", 3, 40+strlen(_draconian_info[which].name));
        put_str("Str  Int  Wis  Dex  Con  Chr   EXP ", 4, 40);
        sprintf(buf, "%+3d  %+3d  %+3d  %+3d  %+3d  %+3d %+4d%% ",
            race_ptr->stats[A_STR], race_ptr->stats[A_INT], race_ptr->stats[A_WIS],
            race_ptr->stats[A_DEX], race_ptr->stats[A_CON], race_ptr->stats[A_CHR],
            race_ptr->exp);
        c_put_str(TERM_L_BLUE, buf, 5, 40);

        var_set_bool(res, TRUE);
        break;
    }
    }
}
static void _demon_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, get_race_aux(RACE_MON_DEMON, which)->subname);
        break;
    case MENU_ON_BROWSE:
    {
        char buf[100];
        race_t *race_ptr = get_race_aux(RACE_MON_DEMON, which);

        c_put_str(TERM_L_BLUE, race_ptr->subname, 3, 40);
        put_str(": Race modification", 3, 40+strlen(race_ptr->subname));
        put_str("Str  Int  Wis  Dex  Con  Chr   EXP ", 4, 40);
        sprintf(buf, "%+3d  %+3d  %+3d  %+3d  %+3d  %+3d %+4d%% ",
            race_ptr->stats[A_STR], race_ptr->stats[A_INT], race_ptr->stats[A_WIS], 
            race_ptr->stats[A_DEX], race_ptr->stats[A_CON], race_ptr->stats[A_CHR], 
            race_ptr->exp);
        c_put_str(TERM_L_BLUE, buf, 5, 40);

        var_set_bool(res, TRUE);
        break;
    }
    }
}

static _name_desc_t _elemental_info[ELEMENTAL_MAX] = {
    { "Earth Elemental", 
        "Earth Elementals are creatures of rock: Strong, tough and slow. "
        "They may move freely through the earth and are capable of conjuring "
        "sharp clods of earth to hurl at their foes. Their skin is very tough, "
        "and they can even turn their bodies to stone. However, being made of "
        "earth, their potions frequently turn to mud." },
    { "Air Elemental", 
        "Air Elementals are creatures of electricity. They are incredibly fast, "
        "blinking in and out of sight as they shower their enemies with confusing "
        "and shocking blows. Electricity crackles menacingly about their nimble frames, "
        "tending to destroy rings, amulets, wands and rods." },
    { "Water Elemental", 
        "Water Elementals are creatures of water, able to modify this ubiquitous "
        "liquid into a deadly and often corrosive weapon of destruction. Fear their "
        "rage! They cannot be stunned. Their corrosive nature erodes any armor that "
        "gets too close." },
    { "Fire Elemental", 
        "Fire Elementals are creatures of flame. They have a vast arsenal of "
        "flaming attacks with which to singe the fiercest of foes. However, they "
        "must beware of cold based attacks! Being wreathed in flames, scrolls and "
        "staves are quickly burned to ash." },
};
static void _elemental_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, _elemental_info[which].name);
        break;
    case MENU_ON_BROWSE:
    {
        char buf[100];
        race_t *race_ptr = get_race_aux(RACE_MON_ELEMENTAL, which);

        c_put_str(TERM_L_BLUE, _elemental_info[which].name, 3, 40);
        put_str(": Race modification", 3, 40+strlen(_elemental_info[which].name));
        put_str("Str  Int  Wis  Dex  Con  Chr   EXP ", 4, 40);
        sprintf(buf, "%+3d  %+3d  %+3d  %+3d  %+3d  %+3d %+4d%% ",
            race_ptr->stats[A_STR], race_ptr->stats[A_INT], race_ptr->stats[A_WIS], 
            race_ptr->stats[A_DEX], race_ptr->stats[A_CON], race_ptr->stats[A_CHR], 
            race_ptr->exp);
        c_put_str(TERM_L_BLUE, buf, 5, 40);

        var_set_bool(res, TRUE);
        break;
    }
    }
}

static int _beginner_races[] = {
  RACE_HUMAN,
  RACE_DWARF,
  RACE_HOBBIT,
  RACE_GNOME,
  RACE_DUNADAN,
  RACE_HIGH_ELF,
  RACE_HALF_TROLL,
  -1
};

static int _prompt_race_beginner(void)
{
    int idx;
    for (;;)
    {
    race_t *race_ptr = NULL;
    menu_t menu = { "Race", "Races.txt", "Note: Your 'race' determines various intrinsic factors and bonuses.",
                        _race_menu_fn,
                        _beginner_races, _count_ids(_beginner_races)};

        c_put_str(TERM_WHITE, "              ", 4, 14);
        c_put_str(TERM_WHITE, "                   ", 5, 14);
        idx = _menu_choose(&menu, _find_id(_beginner_races, p_ptr->prace));
        if (idx < 0) return idx;

        if (_beginner_races[idx] != p_ptr->prace)
        {
            p_ptr->prace = _beginner_races[idx];
            p_ptr->psubrace = 0;
        }
        race_ptr = get_race();

        c_put_str(TERM_L_BLUE, format("%-14s", race_ptr->name), 4, 14);
        if (!_confirm_choice(race_ptr->desc, menu.count)) continue;
        c_put_str(TERM_WHITE, "                   ", 5, 14);
        p_ptr->psubrace = 0;
        idx = _prompt_class_beginner();
        if (idx == _BIRTH_ESCAPE) continue;
        return idx;
    }
    /*return _BIRTH_ESCAPE;  unreachable */
}

static int _prompt_race(void)
{
    for (;;)
    {
        int idx;
        int group_id = 0;

        c_put_str(TERM_WHITE, "              ", 4, 14);
        c_put_str(TERM_WHITE, "                   ", 5, 14);
        for (;;)
        {
            menu_t menu1 = { "Race Type", "Races.txt", "",
                                _race_group_menu_fn, 
                                NULL, _MAX_RACE_GROUPS};
            idx = _menu_choose(&menu1, group_id);
            if (idx < 0) return idx;
            group_id = idx;
            for (;;)
            {
            race_t *race_ptr = NULL;
            menu_t menu2 = { "Race", _race_groups[group_id].help, "Note: Your 'race' determines various intrinsic factors and bonuses.",
                                _race_menu_fn, 
                                _race_groups[group_id].ids, _count_ids(_race_groups[group_id].ids)};

                c_put_str(TERM_WHITE, "              ", 4, 14);
                c_put_str(TERM_WHITE, "                   ", 5, 14);
                idx = _menu_choose(&menu2, _find_id(_race_groups[group_id].ids, p_ptr->prace));
                if (idx == _BIRTH_ESCAPE) break;
                if (idx < 0) return idx;

                if (_race_groups[group_id].ids[idx] != p_ptr->prace)
                {
                    p_ptr->prace = _race_groups[group_id].ids[idx];
                    p_ptr->psubrace = 0;
                }
                race_ptr = get_race();

                c_put_str(TERM_L_BLUE, format("%-14s", race_ptr->name), 4, 14);
                if (!_confirm_choice(race_ptr->desc, menu2.count)) continue;
                if (p_ptr->prace == RACE_DEMIGOD)
                {
                    for (;;)
                    {
                    menu_t menu3 = { "Parentage", "Demigods.txt", "Unlike in real life, you get to choose your parent.",
                                        _demigod_menu_fn, 
                                        NULL, MAX_DEMIGOD_TYPES};

                        c_put_str(TERM_WHITE, "                        ", 5, 14);
                        idx = _menu_choose(&menu3, p_ptr->psubrace);
                        if (idx == _BIRTH_ESCAPE) break;
                        if (idx < 0) return idx;
                        p_ptr->psubrace = idx;
                        race_ptr = get_race();
                        c_put_str(TERM_L_BLUE, format("%-19s", race_ptr->subname), 5, 14);
                        if (!_confirm_choice(race_ptr->subdesc, menu3.count)) continue;
                        idx = _prompt_class();
                        if (idx == _BIRTH_ESCAPE) continue;
                        return idx;
                    }
                }
                else if (p_ptr->prace == RACE_DRACONIAN)
                {
                    for (;;)
                    {
                    menu_t menu3 = { "Subrace", "Draconians.txt", "Choose your subrace",
                                        _draconian_menu_fn,
                                        NULL, DRACONIAN_MAX};

                        c_put_str(TERM_WHITE, "                        ", 5, 14);
                        idx = _menu_choose(&menu3, p_ptr->psubrace);
                        if (idx == _BIRTH_ESCAPE) break;
                        if (idx < 0) return idx;
                        p_ptr->psubrace = idx;
                        c_put_str(TERM_L_BLUE, format("%-19s", _draconian_info[p_ptr->psubrace].name), 5, 14);
                        if (!_confirm_choice(_draconian_info[p_ptr->psubrace].desc, menu3.count)) continue;
                        idx = _prompt_class();
                        if (idx == _BIRTH_ESCAPE) continue;
                        return idx;
                    }
                }
                else
                {
                    c_put_str(TERM_WHITE, "                   ", 5, 14);
                    p_ptr->psubrace = 0;
                    idx = _prompt_class();
                    if (idx == _BIRTH_ESCAPE) continue;
                    return idx;
                }
            }
        }
    }
    /*return _BIRTH_ESCAPE;  unreachable */
}

static int _prompt_mon_race(void)
{
    for (;;)
    {
        int idx = 0;
        int group_id = 0;

        c_put_str(TERM_WHITE, "              ", 4, 14);
        c_put_str(TERM_WHITE, "                   ", 5, 14);
        for (;;)
        {
            menu_t menu1 = { "Race Type", "MonsterRaces.txt", "",
                                _mon_race_group_menu_fn,
                                NULL, _MAX_MON_RACE_GROUPS};
            idx = _menu_choose(&menu1, group_id);
            if (idx < 0) return idx;
            group_id = idx;
            for (;;)
            {
            race_t *race_ptr = NULL;
            menu_t menu2 = { "Race", _mon_race_groups[group_id].help, "Note: Your 'race' determines various intrinsic factors and bonuses.",
                                _race_menu_fn,
                                _mon_race_groups[group_id].ids, _count_ids(_mon_race_groups[group_id].ids)};

                c_put_str(TERM_WHITE, "              ", 4, 14);
                c_put_str(TERM_WHITE, "                   ", 5, 14);
                if (idx == _BIRTH_ESCAPE && menu2.count == 1) break;
                idx = _menu_choose(&menu2, _find_id(_mon_race_groups[group_id].ids, p_ptr->prace));
                if (idx == _BIRTH_ESCAPE) break;
                if (idx < 0) return idx;

                if (_mon_race_groups[group_id].ids[idx] != p_ptr->prace)
                {
                    p_ptr->prace = _mon_race_groups[group_id].ids[idx];
                    p_ptr->psubrace = 0;
                }
                race_ptr = get_race();

                c_put_str(TERM_L_BLUE, format("%-14s", race_ptr->name), 4, 14);
                if (!_confirm_choice(race_ptr->desc, menu2.count)) continue;
                if (p_ptr->prace == RACE_MON_SPIDER)
                {
                    for (;;)
                    {
                        menu_t menu3 = { "Subrace", "MonsterRaces.txt#Spider", "",
                                            _spider_menu_fn,
                                            NULL, SPIDER_MAX};
                        c_put_str(TERM_WHITE, "                   ", 5, 14);
                        idx = _menu_choose(&menu3, p_ptr->psubrace);
                        if (idx == _BIRTH_ESCAPE) break;
                        if (idx < 0) return idx;
                        p_ptr->psubrace = idx;
                        c_put_str(TERM_L_BLUE, format("%-19s", _spider_info[p_ptr->psubrace].name), 5, 14);
                        if (!_confirm_choice(_spider_info[p_ptr->psubrace].desc, menu3.count)) continue;
                        idx = _prompt_class();
                        if (idx == _BIRTH_ESCAPE) continue;
                        return idx;
                    }
                }
                else if (p_ptr->prace == RACE_MON_GIANT)
                {
                    for (;;)
                    {
                        menu_t menu3 = { "Subrace", "MonsterRaces.txt#Giant", "",
                                            _giant_menu_fn,
                                            NULL, GIANT_MAX};
                        c_put_str(TERM_WHITE, "                   ", 5, 14);
                        idx = _menu_choose(&menu3, p_ptr->psubrace);
                        if (idx == _BIRTH_ESCAPE) break;
                        if (idx < 0) return idx;
                        p_ptr->psubrace = idx;
                        c_put_str(TERM_L_BLUE, format("%-19s", _giant_info[p_ptr->psubrace].name), 5, 14);
                        if (!_confirm_choice(_giant_info[p_ptr->psubrace].desc, menu3.count)) continue;
                        idx = _prompt_class();
                        if (idx == _BIRTH_ESCAPE) continue;
                        return idx;
                    }
                }
                else if (p_ptr->prace == RACE_MON_GOLEM)
                {
                    for (;;)
                    {
                        menu_t menu3 = { "Subrace", "MonsterRaces.txt#Golem", "",
                                            _golem_menu_fn,
                                            NULL, GOLEM_MAX};
                        c_put_str(TERM_WHITE, "                   ", 5, 14);
                        idx = _menu_choose(&menu3, p_ptr->psubrace);
                        if (idx == _BIRTH_ESCAPE) break;
                        if (idx < 0) return idx;
                        p_ptr->psubrace = idx;
                        c_put_str(TERM_L_BLUE, format("%-19s", _golem_info[p_ptr->psubrace].name), 5, 14);
                        if (!_confirm_choice(_golem_info[p_ptr->psubrace].desc, menu3.count)) continue;
                        idx = _prompt_class();
                        if (idx == _BIRTH_ESCAPE) continue;
                        return idx;
                    }
                }
                else if (p_ptr->prace == RACE_MON_TROLL)
                {
                    for (;;)
                    {
                        menu_t menu3 = { "Subrace", "MonsterRaces.txt#Troll", "",
                                            _troll_menu_fn,
                                            NULL, TROLL_MAX};
                        c_put_str(TERM_WHITE, "                   ", 5, 14);
                        idx = _menu_choose(&menu3, p_ptr->psubrace);
                        if (idx == _BIRTH_ESCAPE) break;
                        if (idx < 0) return idx;
                        p_ptr->psubrace = idx;
                        c_put_str(TERM_L_BLUE, format("%-19s", _troll_info[p_ptr->psubrace].name), 5, 14);
                        if (!_confirm_choice(_troll_info[p_ptr->psubrace].desc, menu3.count)) continue;
                        idx = _prompt_class();
                        if (idx == _BIRTH_ESCAPE) continue;
                        return idx;
                    }
                }
                else if (p_ptr->prace == RACE_MON_DRAGON)
                {
                    for (;;)
                    {
                        menu_t menu3 = { "Subrace", "Dragons.txt", "",
                                            _dragon_menu_fn,
                                            NULL, DRAGON_MAX};
                        c_put_str(TERM_WHITE, "                   ", 5, 14);
                        idx = _menu_choose(&menu3, p_ptr->psubrace);
                        if (idx == _BIRTH_ESCAPE) break;
                        if (idx < 0) return idx;
                        p_ptr->psubrace = idx;
                        race_ptr = get_race();
                        c_put_str(TERM_L_BLUE, format("%-19s", race_ptr->subname), 5, 14);
                        if (!_confirm_choice(race_ptr->subdesc, menu3.count)) continue;
                        idx = _prompt_class();
                        if (idx == _BIRTH_ESCAPE) continue;
                        return idx;
                    }
                }
                else if (p_ptr->prace == RACE_MON_DEMON)
                {
                    for (;;)
                    {
                        menu_t menu3 = { "Subrace", "Demons.txt", "",
                                            _demon_menu_fn,
                                            NULL, DEMON_MAX};
                        c_put_str(TERM_WHITE, "                   ", 5, 14);
                        idx = _menu_choose(&menu3, p_ptr->psubrace);
                        if (idx == _BIRTH_ESCAPE) break;
                        if (idx < 0) return idx;
                        p_ptr->psubrace = idx;
                        race_ptr = get_race();
                        c_put_str(TERM_L_BLUE, format("%-19s", race_ptr->subname), 5, 14);
                        if (!_confirm_choice(race_ptr->subdesc, menu3.count)) continue;
                        idx = _prompt_class();
                        if (idx == _BIRTH_ESCAPE) continue;
                        return idx;
                    }
                }
                else if (p_ptr->prace == RACE_MON_ELEMENTAL)
                {
                    for (;;)
                    {
                        menu_t menu3 = { "Subrace", "MonsterRaces.txt#Elemental", "",
                                            _elemental_menu_fn,
                                            NULL, ELEMENTAL_MAX};
                        c_put_str(TERM_WHITE, "                   ", 5, 14);
                        idx = _menu_choose(&menu3, p_ptr->psubrace);
                        if (idx == _BIRTH_ESCAPE) break;
                        if (idx < 0) return idx;
                        p_ptr->psubrace = idx;
                        c_put_str(TERM_L_BLUE, format("%-19s", _elemental_info[p_ptr->psubrace].name), 5, 14);
                        if (!_confirm_choice(_elemental_info[p_ptr->psubrace].desc, menu3.count)) continue;
                        idx = _prompt_class();
                        if (idx == _BIRTH_ESCAPE) continue;
                        return idx;
                    }
                }
                else
                {
                    c_put_str(TERM_WHITE, "                   ", 5, 14);
                    p_ptr->psubrace = 0;
                    idx = _prompt_class();
                    if (idx == _BIRTH_ESCAPE) continue;
                    return idx;
                }
            }
        }
    }
    /*return _BIRTH_ESCAPE;  unreachable */
}

static void _sex_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    switch (cmd)
    {
    case MENU_TEXT:
        if (which == SEX_MALE)
            var_set_string(res, "Male");
        else
            var_set_string(res, "Female");
        break;
    }
}

static int _prompt_sex(void)
{
    for (;;)
    {
        int idx;
        menu_t menu = { "Sex", "birth.txt", "Note: Your 'sex' does not have any significant gameplay effects.",
                            _sex_menu_fn, 
                            NULL, 2};

        c_put_str(TERM_WHITE, "              ", 2, 14);
        idx = _menu_choose(&menu, p_ptr->psex);
        if (idx < 0) return idx;

        p_ptr->psex = idx;
        sp_ptr = &sex_info[p_ptr->psex]; /* TODO: Remove this ... */
        c_put_str(TERM_L_BLUE, format("%-14s", sp_ptr->title), 2, 14);

        if (game_mode == GAME_MODE_MONSTER)
            idx = _prompt_mon_race();
        else if (game_mode == GAME_MODE_BEGINNER)
            idx = _prompt_race_beginner();
        else
            idx = _prompt_race();
        if (idx == _BIRTH_ESCAPE) continue;
        return idx;
    }
    /*return _BIRTH_ESCAPE;  unreachable */
}

static _name_desc_t _game_mode_info[GAME_MODE_MAX] = {
    {"Beginner",
        "This option restricts the number of races and classes available. Also, instead of playing "
        "with a normal game wilderness, you play with a special town." },
    {"Normal",
        "All races and classes are available and the normal wilderness is used by default." },
    {"Real Life",
        "Your race, sex and stats are rolled automatically and then you choose a class and "
        "personality to match. This is how things work in real life, right?" },
    {"Monster",
        "Play as a monster rather than a normal player!" },
};

static void _game_mode_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, _game_mode_info[which].name);
        break;
    case MENU_ON_BROWSE:
    {
        char buf[80 * 10];
        int  i, r = 0;

        Term_erase(5, 16, 255);
        Term_erase(5, 17, 255);
        Term_erase(5, 18, 255);

        roff_to_buf(_game_mode_info[which].desc, 70, buf, sizeof(buf));
        for (i = 0; buf[i]; i += 1 + strlen(&buf[i]))
        {
            put_str(buf + i, 16 + r, 5);
            i++;
            r++;
        }
        var_set_bool(res, TRUE);
        break;
    }
    }
}

static int _real_life_races[] = {
    RACE_HUMAN, RACE_TONBERRY, RACE_DEMIGOD, RACE_HOBBIT, RACE_GNOME, RACE_DWARF,
    RACE_SNOTLING, RACE_HALF_TROLL, RACE_AMBERITE, RACE_HIGH_ELF, RACE_BARBARIAN,
    RACE_HALF_OGRE, RACE_HALF_GIANT, RACE_HALF_TITAN, RACE_CYCLOPS, RACE_YEEK,
    RACE_KLACKON, RACE_KOBOLD, RACE_NIBELUNG, RACE_DARK_ELF, RACE_DRACONIAN,
    RACE_MIND_FLAYER, RACE_IMP, RACE_GOLEM, RACE_SKELETON, RACE_ZOMBIE, RACE_VAMPIRE,
    RACE_SPECTRE, RACE_SPRITE, RACE_BEASTMAN, RACE_ENT, RACE_ARCHON, RACE_BALROG,
    RACE_DUNADAN, RACE_SHADOW_FAIRY, RACE_KUTAR, RACE_ANDROID,RACE_DOPPELGANGER,
    RACE_CENTAUR, RACE_WOOD_ELF, -1
};

static bool _prompt_game_mode(void)
{
    for (;;)
    {
        int idx;
        menu_t menu = { "Mode", "tang.txt", "Choose which kind of game to play.",
                            _game_mode_menu_fn,
                            NULL, GAME_MODE_MAX};

        c_put_str(TERM_WHITE, "              ", 2, 14);
        idx = _menu_choose(&menu, game_mode);
        if (idx < 0) return FALSE;
        game_mode = idx;

        switch (game_mode)
        {
        case GAME_MODE_REAL_LIFE:
        {
            race_t *race_ptr = NULL;
            int i, col, row;

            c_put_str(TERM_WHITE, "              ", 2, 14);
            p_ptr->psex = one_in_(2) ? SEX_MALE : SEX_FEMALE;
            sp_ptr = &sex_info[p_ptr->psex]; /* TODO: Remove this ... */
            c_put_str(TERM_L_BLUE, format("%-14s", sp_ptr->title), 2, 14);

            c_put_str(TERM_WHITE, "              ", 4, 14);
            c_put_str(TERM_WHITE, "                   ", 5, 14);
            idx = randint0(_count_ids(_real_life_races));
            p_ptr->prace = _real_life_races[idx];
            p_ptr->psubrace = 0;
            race_ptr = get_race();

            c_put_str(TERM_L_BLUE, format("%-14s", race_ptr->name), 4, 14);

            if (p_ptr->prace == RACE_DEMIGOD)
            {
                p_ptr->psubrace = randint0(MAX_DEMIGOD_TYPES);
                race_ptr = get_race();
                c_put_str(TERM_L_BLUE, format("%-19s", race_ptr->subname), 5, 14);
            }
            else if (p_ptr->prace == RACE_DRACONIAN)
            {
                p_ptr->psubrace = randint0(DRACONIAN_MAX);
                c_put_str(TERM_L_BLUE, format("%-19s", _draconian_info[p_ptr->psubrace].name), 5, 14);
            }

            for (;;)
            {
                int spread = 0, i;
                for (i = 0; i < 6; i++)
                {
                    int n = randint1(9) - 5;
                    p_ptr->stat_cur[i] = 13 + n;
                    p_ptr->stat_max[i] = 13 + n;
                    spread += n;
                }
                if (spread > 2) break;
            }

            col = 42;
            row = 1;

            c_put_str(TERM_WHITE, "  Base  R  C  P Total", row, col + 8);
            for (i = 0; i < 6; i++)
            {
                int total;
                char buf[MAX_NLEN];

                c_put_str(TERM_WHITE, stat_names[i], row+i+1, col+1);

                cnv_stat(p_ptr->stat_max[i], buf);
                c_put_str(TERM_L_BLUE, buf, row + i+1, col + 8 + 6 - strlen(buf));

                sprintf(buf, "%3d", race_ptr->stats[i]);
                c_put_str(TERM_L_BLUE, buf, row + i+1, col + 8 + 9 - strlen(buf));

                total = adjust_stat(p_ptr->stat_cur[i], race_ptr->stats[i]);
                cnv_stat(total, buf);
                c_put_str(TERM_L_GREEN, buf, row + i+1, col + 8 + 21 - strlen(buf));
            }

            prt("Your are born thus. Hit any key.", 0, 0);
            inkey();
            prt("", 0, 0);

            Term_erase(col, row, 255);
            for (i = 0; i < 6; i++)
            {
                Term_erase(col, row + i + 1, 255);
            }

            idx = _prompt_class();
            break;
        }
        default:
            idx = _prompt_sex();
        }

        if (idx == _BIRTH_RESTART) return FALSE;
        if (idx == _BIRTH_ESCAPE) continue;
        return TRUE;
    }
    /*return FALSE;  unreachable */
}

static bool _prompt(void)
{
    /* Note: The call stack stores the users path thru the directed graph
       of birth options (up to personality). We now support escaping to 
       back up and rechoose options. */
    return _prompt_game_mode();
}

#define AUTOROLLER_STEP 50
#define AUTOROLLER_MAX 50 * 1000
#define AUTOROLLER_DELAY
#define MAX_TRIES 100
static s16b stat_limit[6];
static s32b stat_match[6];
static s32b auto_round;

static void birth_quit(void)
{
    remove_loc();
    quit(NULL);
}

static void show_help(cptr helpfile)
{
    screen_save();
    /*(void)show_file(TRUE, helpfile, NULL, 0, 0);*/
    doc_display_help(helpfile, NULL);
    screen_load();
}

/*
 * Save the current data for later
 */
static void save_prev_data(birther *birther_ptr)
{
    int i;

    /* Save the data */
    birther_ptr->psex = p_ptr->psex;
    birther_ptr->prace = p_ptr->prace;
    birther_ptr->psubrace = p_ptr->psubrace;
    birther_ptr->pclass = p_ptr->pclass;
    birther_ptr->psubclass = p_ptr->psubclass;
    birther_ptr->personality = p_ptr->personality;
    birther_ptr->realm1 = p_ptr->realm1;
    birther_ptr->realm2 = p_ptr->realm2;
    birther_ptr->dragon_realm = p_ptr->dragon_realm;
    birther_ptr->au = p_ptr->au;

    /* Save the stats */
    for (i = 0; i < 6; i++)
    {
        birther_ptr->stat_max[i] = p_ptr->stat_max[i];
        birther_ptr->stat_max_max[i] = p_ptr->stat_max_max[i];
    }

    /* Save the hp */
    for (i = 0; i < PY_MAX_LEVEL; i++)
    {
        birther_ptr->player_hp[i] = p_ptr->player_hp[i];
    }

    birther_ptr->chaos_patron = p_ptr->chaos_patron;
    birther_ptr->mutation = p_ptr->birth_mutation;

    /* Save the virtues */
    for (i = 0; i < 8; i++)
    {
        birther_ptr->vir_types[i] = p_ptr->vir_types[i];
    }
}


/*
 * Load the previous data
 */
static void load_prev_data(bool swap)
{
    int i;

    birther    temp;

    /*** Save the current data ***/
    if (swap) save_prev_data(&temp);


    /*** Load the previous data ***/

    /* Load the data */
    p_ptr->psex = previous_char.psex;
    p_ptr->prace = previous_char.prace;
    p_ptr->psubrace = previous_char.psubrace;
    p_ptr->current_r_idx = 0;
    p_ptr->pclass = previous_char.pclass;
    p_ptr->psubclass = previous_char.psubclass;
    p_ptr->personality = previous_char.personality;
    p_ptr->realm1 = previous_char.realm1;
    p_ptr->realm2 = previous_char.realm2;
    p_ptr->dragon_realm = previous_char.dragon_realm;
    p_ptr->au = previous_char.au;

    /* Load the stats */
    for (i = 0; i < 6; i++)
    {
        p_ptr->stat_cur[i] = p_ptr->stat_max[i] = previous_char.stat_max[i];
        p_ptr->stat_max_max[i] = previous_char.stat_max_max[i];
    }

    /* Load the hp */
    for (i = 0; i < PY_MAX_LEVEL; i++)
    {
        p_ptr->player_hp[i] = previous_char.player_hp[i];
    }

    p_ptr->mhp = 10;
    p_ptr->chp = 10;

    p_ptr->chaos_patron = previous_char.chaos_patron;
    p_ptr->birth_mutation = previous_char.mutation;

    for (i = 0; i < 8; i++)
    {
        p_ptr->vir_types[i] = previous_char.vir_types[i];
    }

    /*** Save the previous data ***/
    if (swap)
    {
        (void)COPY(&previous_char, &temp, birther);
    }
}

/*
 * Roll for a characters stats
 *
 * For efficiency, we include a chunk of "calc_bonuses()".
 */
static void get_stats(void)
{
    /* Roll and verify some stats */
    while (TRUE)
    {
        int i;
        int sum = 0;

        /* Roll some dice */
        for (i = 0; i < 2; i++)
        {
            s32b tmp = randint0(60*60*60);
            int val;

            /* Extract 5 + 1d3 + 1d4 + 1d5 */
            val = 5 + 3;
            val += tmp % 3; tmp /= 3;
            val += tmp % 4; tmp /= 4;
            val += tmp % 5; tmp /= 5;

            /* Save that value */
            sum += val;
            p_ptr->stat_cur[3*i] = p_ptr->stat_max[3*i] = val;

            /* Extract 5 + 1d3 + 1d4 + 1d5 */
            val = 5 + 3;
            val += tmp % 3; tmp /= 3;
            val += tmp % 4; tmp /= 4;
            val += tmp % 5; tmp /= 5;

            /* Save that value */
            sum += val;
            p_ptr->stat_cur[3*i+1] = p_ptr->stat_max[3*i+1] = val;

            /* Extract 5 + 1d3 + 1d4 + 1d5 */
            val = 5 + 3;
            val += tmp % 3; tmp /= 3;
            val += tmp % 4; tmp /= 4;
            val += tmp;

            /* Save that value */
            sum += val;
            p_ptr->stat_cur[3*i+2] = p_ptr->stat_max[3*i+2] = val;
        }

        /* Verify totals */
        if ((sum > 42+5*6) && (sum < 54+5*6)) break;
    }
}

void get_max_stats(void)
{
    int        i, j, roll;
    int        dice[6];

    /* Roll and verify some stats */
    while (TRUE)
    {
        /* Roll some dice */
        for (j = i = 0; i < 6; i++)
        {
            /* Roll the dice */
            roll = randint1(7);

            dice[i] = roll;

            /* Collect the maximum */
            j += dice[i];
        }

        /* Verify totals */
        if (j == 24) break;
    }

    /* Acquire the stats */
    for (i = 0; i < 6; i++)
    {
        j = 18 + 60 + dice[i]*10;

        /* Save that value */
        p_ptr->stat_max_max[i] = j;
        if (p_ptr->stat_max[i] > j)
            p_ptr->stat_max[i] = j;
        if (p_ptr->stat_cur[i] > j)
            p_ptr->stat_cur[i] = j;
    }
    p_ptr->knowledge &= ~(KNOW_STAT);

    /* Redisplay the stats later */
    p_ptr->redraw |= (PR_STATS);
}

int _race_exp_factor(void)
{
    if (p_ptr->prace == RACE_DOPPELGANGER)
        return get_race()->exp;
    return get_true_race()->exp;
}
int calc_exp_factor(void)
{
    int exp;
    int r_exp = _race_exp_factor();
    int c_exp = get_class()->exp;
    int a_exp = get_personality()->exp;

    if (p_ptr->prace == RACE_ANDROID) 
        return r_exp;

    exp = r_exp * c_exp / 100;
    exp = exp * a_exp / 100;

    if (p_ptr->prace == RACE_MON_DRAGON)
    {
        dragon_realm_ptr realm = dragon_get_realm(p_ptr->dragon_realm);
        exp = exp * realm->exp / 100;
    }

    return exp;
}

/*
 * Roll for some info that the auto-roller ignores
 */
static void get_extra(bool roll_hitdie)
{
    int i;

    p_ptr->expfact = calc_exp_factor();

    /* Reset record of race/realm changes */
    p_ptr->start_race = p_ptr->prace;
    p_ptr->old_race1 = 0L;
    p_ptr->old_race2 = 0L;
    p_ptr->old_realm = 0;

    for (i = 0; i < 64; i++)
    {
        if (p_ptr->pclass == CLASS_SORCERER) p_ptr->spell_exp[i] = SPELL_EXP_MASTER;
        else if (p_ptr->pclass == CLASS_RED_MAGE) p_ptr->spell_exp[i] = SPELL_EXP_SKILLED;
        else p_ptr->spell_exp[i] = SPELL_EXP_UNSKILLED;
    }

    /* Roll for hit point unless quick-start */
    if (roll_hitdie) do_cmd_rerate_aux();

    /* Initial hitpoints */
    p_ptr->mhp = 10;
}

/*
 * Get the player's starting money
 */
static void get_money(void)
{
    int i, gold;

    /* Social Class determines starting gold */
    gold = randint1(600) + randint1(100) + 300;
    if (p_ptr->pclass == CLASS_TOURIST)
      gold += 2000;

    /* Process the stats */
    for (i = 0; i < 6; i++)
    {
        /* Mega-Hack -- reduce gold for high stats */
        if (p_ptr->stat_max[i] >= 18 + 50) gold -= 300;
        else if (p_ptr->stat_max[i] >= 18 + 20) gold -= 200;
        else if (p_ptr->stat_max[i] > 18) gold -= 150;
        else gold -= (p_ptr->stat_max[i] - 8) * 10;
    }

    /* Minimum 100 gold */
    if (gold < 100) gold = 100;

    if (p_ptr->personality == PERS_LAZY)
        gold /= 2;
    else if (p_ptr->personality == PERS_MUNCHKIN)
        gold = 10000000;
    else if (prace_is_(RACE_MON_LEPRECHAUN))
        gold = 50000;
    if (p_ptr->prace == RACE_ANDROID) gold /= 5;

    /* Save the gold */
    p_ptr->au = gold;
}

/*
 * Display stat values, subset of "put_stats()"
 *
 * See 'display_player()' for screen layout constraints.
 */
static void birth_put_stats(void)
{
    int i, j, m, p;
    int col;
    byte attr;
    char buf[80];
    race_t  *race_ptr = get_race();
    class_t *class_ptr = get_class();
    personality_ptr pers_ptr = get_personality();

    if (autoroller)
    {
        col = 42;
        /* Put the stats (and percents) */
        for (i = 0; i < 6; i++)
        {
            /* Race/Class bonus */
            j = race_ptr->stats[i] + class_ptr->stats[i] + pers_ptr->stats[i];

            /* Obtain the current stat */
            m = adjust_stat(p_ptr->stat_max[i], j);

            /* Put the stat */
            cnv_stat(m, buf);
            c_put_str(TERM_L_GREEN, buf, 3+i, col+24);

            /* Put the percent */
            if (stat_match[i])
            {
                if (stat_match[i] > 1000000L)
                {
                    /* Prevent overflow */
                    p = stat_match[i] / (auto_round / 1000L);
                }
                else
                {
                    p = 1000L * stat_match[i] / auto_round;
                }
            
                attr = (p < 100) ? TERM_YELLOW : TERM_L_GREEN;
                sprintf(buf, "%3d.%d%%", p/10, p%10);
                c_put_str(attr, buf, 3+i, col+13);
            }

            /* Never happened */
            else
            {
                c_put_str(TERM_RED, "(NONE)", 3+i, col+13);
            }
        }
    }
}

static void e_info_reset(void)
{
    int i;

    /* Reset the "objects" */
    for (i = 1; i < max_e_idx; i++)
    {
        ego_item_type *e_ptr = &e_info[i];

        e_ptr->aware = FALSE;
    }
}

static void k_info_reset(void)
{
    int i;

    /* Reset the "objects" */
    for (i = 1; i < max_k_idx; i++)
    {
        object_kind *k_ptr = &k_info[i];

        k_ptr->tried = FALSE;
        k_ptr->aware = FALSE;
    }
}

/*
 * Clear all the global "character" data
 */
static void player_wipe(void)
{
    int i;

    /* Hack -- free the "last message" string */
    if (p_ptr->last_message) z_string_free(p_ptr->last_message);

    /* Hack -- zero the struct */
    (void)WIPE(p_ptr, player_type);
    p_ptr->mimic_form = MIMIC_NONE;

    /* Wipe the quests */
    for (i = 0; i < max_quests; i++)
    {
        quest[i].status = QUEST_STATUS_UNTAKEN;

        quest[i].cur_num = 0;
        quest[i].max_num = 0;
        quest[i].type = 0;
        quest[i].level = 0;
        quest[i].r_idx = 0;
        quest[i].complev = 0;
        quest[i].seed = 0;
    }

    /* No weight */
    p_ptr->total_weight = 0;

    /* No items */
    inven_cnt = 0;

    /* Clear the inventory */
    for (i = 0; i < INVEN_TOTAL; i++)
    {
        object_wipe(&inventory[i]);
    }


    /* Start with no artifacts made yet */
    for (i = 0; i < max_a_idx; i++)
    {
        artifact_type *a_ptr = &a_info[i];
        a_ptr->cur_num = 0;
    }

    /* Reset the objects */
    k_info_reset();
    e_info_reset();
    stats_reset();

    /* Reset the "monsters" */
    for (i = 1; i < max_r_idx; i++)
    {
        monster_race *r_ptr = &r_info[i];

        /* Hack -- Reset the counter */
        r_ptr->cur_num = 0;

        /* Hack -- Reset the max counter */
        r_ptr->max_num = 100;

        /* Hack -- Reset the max counter */
        if (r_ptr->flags1 & RF1_UNIQUE) r_ptr->max_num = 1;

        /* Hack -- Non-unique Nazguls are semi-unique */
        else if (r_ptr->flags7 & RF7_NAZGUL) r_ptr->max_num = MAX_NAZGUL_NUM;
        else if (i == MON_CAMELOT_KNIGHT) r_ptr->max_num = MAX_CAMELOT_KNIGHT_NUM;

        /* Clear visible kills in this life */
        r_ptr->r_pkills = 0;

        /* Clear all kills in this life */
        r_ptr->r_akills = 0;
        r_ptr->r_skills = 0;
        r_ptr->stolen_ct = 0;

        /* Wipe out pact alliances from previous character 
           Currently, flagsr is only set to make the memory field
           work, but perhaps it would be better to set this once
           and for all when a pact is made?  This would break
           my savefiles though ...*/
        r_ptr->flagsr &= ~(RFR_PACT_MONSTER);
        r_ptr->r_flagsr &= ~(RFR_PACT_MONSTER);
    }


    /* Hack -- Well fed player */
    p_ptr->food = PY_FOOD_FULL - 1;


    /* Wipe the spells */
    if (p_ptr->pclass == CLASS_SORCERER)
    {
        p_ptr->spell_learned1 = p_ptr->spell_learned2 = 0xffffffffL;
        p_ptr->spell_worked1 = p_ptr->spell_worked2 = 0xffffffffL;
    }
    else
    {
        p_ptr->spell_learned1 = p_ptr->spell_learned2 = 0L;
        p_ptr->spell_worked1 = p_ptr->spell_worked2 = 0L;
    }
    p_ptr->spell_forgotten1 = p_ptr->spell_forgotten2 = 0L;
    for (i = 0; i < 64; i++) p_ptr->spell_order[i] = 99;
    p_ptr->learned_spells = 0;
    p_ptr->add_spells = 0;
    p_ptr->knowledge = 0;

    /* Clean the mutation count */
    mutant_regenerate_mod = 100;

    /* Clear "cheat" options */
    cheat_peek = FALSE;
    cheat_hear = FALSE;
    cheat_room = FALSE;
    cheat_xtra = FALSE;
    cheat_know = FALSE;
    cheat_live = FALSE;
    cheat_save = FALSE;

    /* Assume no winning game */
    p_ptr->total_winner = FALSE;

    world_player = FALSE;

    /* Assume no panic save */
    p_ptr->panic_save = 0;

    /* Assume no cheating */
    p_ptr->noscore = 0;
    p_ptr->wizard = FALSE;

    /* Not waiting to report score */
    p_ptr->wait_report_score = FALSE;

    /* Default pet command settings */
    p_ptr->pet_follow_distance = PET_FOLLOW_DIST;
    p_ptr->pet_extra_flags = (PF_TELEPORT | PF_ATTACK_SPELL | PF_SUMMON_SPELL);

    /* Wipe the recall depths */
    for (i = 0; i < max_d_idx; i++)
    {
        max_dlv[i] = 0;
        dungeon_flags[i] = 0;
    }

    p_ptr->visit = 1;
    p_ptr->wild_mode = FALSE;

    for (i = 0; i < MAX_MAGIC_NUM; i++)
    {
        p_ptr->magic_num1[i] = 0;
        p_ptr->magic_num2[i] = 0;
    }

    /* Level one */
    p_ptr->max_plv = p_ptr->lev = 1;

    /* Initialize arena and rewards information -KMW- */
    p_ptr->arena_number = 0;
    p_ptr->inside_arena = FALSE;
    p_ptr->inside_quest = 0;
    for (i = 0; i < MAX_MANE; i++)
    {
        p_ptr->mane_spell[i] = -1;
        p_ptr->mane_dam[i] = 0;
    }
    p_ptr->mane_num = 0;
    p_ptr->exit_bldg = TRUE; /* only used for arena now -KMW- */

    /* Bounty */
    p_ptr->today_mon = 0;

    /* Reset monster arena */
    battle_monsters();

    /* Reset mutations */
    for (i = 0; i < MUT_FLAG_SIZE; ++i)
    {
        p_ptr->muta[i] = 0;
        p_ptr->muta_lock[i] = 0;
    }

    for (i = 0; i < MAX_DEMIGOD_POWERS; ++i)
        p_ptr->demigod_power[i] = -1;

    p_ptr->draconian_power = -1;

    p_ptr->duelist_target_idx = 0;

    /* Reset virtues*/
    for (i = 0; i < 8; i++) p_ptr->virtues[i]=0;

    /* Set the recall dungeon accordingly */
    if (no_wilderness)
    {
        dungeon_type = 0;
        p_ptr->recall_dungeon = DUNGEON_ANGBAND;
    }
    else
    {
        dungeon_type = 0;
        p_ptr->recall_dungeon = DUNGEON_STRONGHOLD;
    }
}


/*
 *  Hook function for quest monsters
 */
static bool mon_hook_quest(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    /* Random quests are in the dungeon */
    if (r_ptr->flags8 & RF8_WILD_ONLY) return FALSE;

    /* No random quests for aquatic monsters */
    if (r_ptr->flags7 & RF7_AQUATIC) return FALSE;

    /* No random quests for multiplying monsters */
    if (r_ptr->flags2 & RF2_MULTIPLY) return FALSE;

    /* No quests to kill friendly monsters */
    if (r_ptr->flags7 & RF7_FRIENDLY) return FALSE;

    return TRUE;
}

static bool mon_hook_quest_unique(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (!(r_ptr->flags1 & RF1_UNIQUE)) return FALSE;

    return TRUE;
}

static bool mon_hook_quest_nonunique(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (r_ptr->flags1 & RF1_UNIQUE) return FALSE;
    if (r_ptr->flags7 & RF7_UNIQUE2) return FALSE;
    if (r_ptr->flags7 & RF7_NAZGUL) return FALSE;

    return TRUE;
}


/*
 * Determine the random quest uniques
 */
void determine_random_questor(quest_type *q_ptr)
{
    int          r_idx;
    monster_race *r_ptr;
    int             attempt = 0;
    bool         force_unique = FALSE;
    bool         prevent_unique = FALSE;

    /* High Level quests are stacked with uniques. Everything else
       is stacked the other way. So lets make some attempt at balance.
       Of course, users can force all quests to be for uniques, in 
       true Hengband spirit. */
    if (quest_unique || one_in_(3))
    {
        get_mon_num_prep(mon_hook_quest, mon_hook_quest_unique);
        force_unique = TRUE;
    }
    else if (one_in_(2))
    {
        get_mon_num_prep(mon_hook_quest, mon_hook_quest_nonunique);
        prevent_unique = TRUE;
    }
    else
        get_mon_num_prep(mon_hook_quest, NULL);

    while (1)
    {
        int accept_lev = q_ptr->level + (q_ptr->level / 20);
        int mon_lev = q_ptr->level + 5 + randint1(q_ptr->level / 10);

        /* Hacks for high level quests */
        if (accept_lev > 88)
            accept_lev = 88;

        if (mon_lev > 88)
            mon_lev = 88;

        attempt++;

        /*
         * Random monster 5 - 10 levels out of depth
         * (depending on level)
         */
        unique_count = 0; /* Hack! */
        r_idx = get_mon_num(mon_lev);
        r_ptr = &r_info[r_idx];

        if (r_idx == MON_ROBIN_HOOD) continue;
        if (r_idx == MON_JACK_SHADOWS) continue;

        /* Try to enforce preferences, but its virtually impossible to prevent
           high level quests for uniques */
        if (attempt < 5000)
        {
            if (prevent_unique && (r_ptr->flags1 & RF1_UNIQUE)) continue;
            if (force_unique && !(r_ptr->flags1 & RF1_UNIQUE)) continue;
        }

        if (r_ptr->flags1 & RF1_QUESTOR) continue;

        if (r_ptr->rarity > 100) continue;

        if (r_ptr->flags7 & RF7_FRIENDLY) continue;

        if (r_ptr->flags7 & RF7_AQUATIC) continue;

        if (r_ptr->flags8 & RF8_WILD_ONLY) continue;

        if (no_questor_or_bounty_uniques(r_idx)) continue;

        if (r_ptr->level > q_ptr->level + 12) continue;

        if (r_ptr->level > accept_lev) break;
    }

    q_ptr->r_idx = r_idx;
}


/*
 *  Initialize random quests and final quests
 */
static void init_dungeon_quests(void)
{
    int i;

    num_random_quests = 10; /*get_quantity("How many quests (0 to 49)? ", 49);*/

    /* Init the random quests */
    init_flags = INIT_ASSIGN;
    p_ptr->inside_quest = MIN_RANDOM_QUEST;

    process_dungeon_file("q_info.txt", 0, 0, 0, 0);

    p_ptr->inside_quest = 0;

    /* Generate quests */
    for (i = MIN_RANDOM_QUEST + num_random_quests - 1; i >= MIN_RANDOM_QUEST; i--)
    {
        quest_type      *q_ptr = &quest[i];
        monster_race    *quest_r_ptr;

        q_ptr->status = QUEST_STATUS_TAKEN;
        determine_random_questor(q_ptr);

        /* Mark uniques */
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

    /* Init the two main quests (Oberon + Serpent) */
    init_flags = INIT_ASSIGN;
    p_ptr->inside_quest = QUEST_OBERON;

    process_dungeon_file("q_info.txt", 0, 0, 0, 0);

    quest[QUEST_OBERON].status = QUEST_STATUS_TAKEN;

    p_ptr->inside_quest = QUEST_SERPENT;

    process_dungeon_file("q_info.txt", 0, 0, 0, 0);

    quest[QUEST_SERPENT].status = QUEST_STATUS_TAKEN;
    p_ptr->inside_quest = 0;
}

/*
 * Reset turn
 */
static void init_turn(void)
{
    if ( p_ptr->prace == RACE_VAMPIRE
      || p_ptr->prace == RACE_MON_VAMPIRE
      || p_ptr->prace == RACE_SKELETON
      || p_ptr->prace == RACE_ZOMBIE
      || p_ptr->prace == RACE_SPECTRE )
    {
        /* Undead start just after midnight */
        game_turn = (TURNS_PER_TICK*3 * TOWN_DAWN) / 4 + 1;
        game_turn_limit = TURNS_PER_TICK * TOWN_DAWN * MAX_DAYS + TURNS_PER_TICK * TOWN_DAWN * 3 / 4;
    }
    else
    {
        game_turn = 1;
        game_turn_limit = TURNS_PER_TICK * TOWN_DAWN * (MAX_DAYS - 1) + TURNS_PER_TICK * TOWN_DAWN * 3 / 4;
    }

    dungeon_turn = 1;
    dungeon_turn_limit = TURNS_PER_TICK * TOWN_DAWN * (MAX_DAYS - 1) + TURNS_PER_TICK * TOWN_DAWN * 3 / 4;
}

/*
 * Each player starts out with a few items, given as tval/sval pairs.
 * In addition, he always has some food and a few torches.
 */
static int player_init[MAX_CLASS][3][2] =
{
    {
        /* Warrior */
        { TV_BOW, SV_LIGHT_XBOW },
        { TV_HARD_ARMOR, SV_CHAIN_MAIL },
        { TV_SWORD, SV_BROAD_SWORD }
    },

    {
        /* Mage */
        { TV_SORCERY_BOOK, 0 }, /* Hack: for realm1 book */
        { TV_DEATH_BOOK, 0 }, /* Hack: for realm2 book */
        { TV_SWORD, SV_DAGGER }
    },

    {
        /* Priest */
        { TV_SORCERY_BOOK, 0 }, /* Hack: for Life / Death book */
        { TV_DEATH_BOOK, 0 }, /* Hack: for realm2 book */
        { TV_HAFTED, SV_MACE }
    },

    {
        /* Rogue */
        { TV_SORCERY_BOOK, 0 }, /* Hack: for realm1 book */
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
        { TV_SWORD, SV_DAGGER }
    },

    {
        /* Ranger */
        { TV_NATURE_BOOK, 0 },
        { TV_DEATH_BOOK, 0 },        /* Hack: for realm2 book */
        { TV_SWORD, SV_DAGGER }
    },

    {
        /* Paladin */
        { TV_SORCERY_BOOK, 0 },
        { TV_HARD_ARMOR, SV_RING_MAIL },
        { TV_SWORD, SV_BROAD_SWORD }
    },

    {
        /* Warrior-Mage */
        { TV_SORCERY_BOOK, 0 }, /* Hack: for realm1 book */
        { TV_DEATH_BOOK, 0 }, /* Hack: for realm2 book */
        { TV_SWORD, SV_SHORT_SWORD }
    },

    {
        /* Chaos Warrior */
        { TV_SORCERY_BOOK, 0 }, /* Hack: For realm1 book */
        { TV_HARD_ARMOR, SV_METAL_SCALE_MAIL },
        { TV_SWORD, SV_BROAD_SWORD }
    },

    {
        /* Monk */
        { TV_SORCERY_BOOK, 0 },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
        { TV_POTION, SV_POTION_HEROISM }
    },

    {
        /* Mindcrafter */
        { TV_POTION, SV_POTION_SPEED },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
        { TV_SWORD, SV_SMALL_SWORD }
    },

    {
        /* High Mage */
        { TV_SORCERY_BOOK, 0 }, /* Hack: for realm1 book */
        { TV_SWORD, SV_DAGGER },
        { TV_POTION, SV_POTION_RESTORE_MANA }
    },

    {
        /* Tourist */
        { TV_FOOD, SV_FOOD_JERKY},
        { TV_SCROLL, SV_SCROLL_MAPPING },
        { TV_BOW, SV_SLING}
    },

    {
        /* Imitator */
        { TV_POTION, SV_POTION_SPEED },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
        { TV_SWORD, SV_SHORT_SWORD}
    },

    {
        /* Beastmaster */
        { TV_TRUMP_BOOK, 0 },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
        { TV_POLEARM, SV_SPEAR}
    },

    {
        /* Sorcerer */
        { TV_HAFTED, SV_WIZSTAFF },
        { TV_WAND, EFFECT_BOLT_MISSILE },
        { TV_POTION, SV_POTION_RESTORE_MANA }
    },

    {
        /* Archer */
        { TV_BOW, SV_SHORT_BOW },
        { TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL},
        { TV_SWORD, SV_SHORT_SWORD },
    },

    {
        /* Magic eater */
        { TV_WAND, EFFECT_BOLT_MISSILE },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR},
        { TV_SWORD, SV_SHORT_SWORD },
    },

    {
        /* Bard */
        { TV_MUSIC_BOOK, 0 },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR},
        { TV_SWORD, SV_SHORT_SWORD },
    },

    {
        /* Red Mage */
        { TV_ARCANE_BOOK, 0 },
        { TV_SOFT_ARMOR, SV_HARD_LEATHER_ARMOR},
        { TV_SWORD, SV_SHORT_SWORD },
    },

    {
        /* Samurai */
        { TV_HISSATSU_BOOK, 0 },
        { TV_HARD_ARMOR, SV_CHAIN_MAIL },
        { TV_SWORD, SV_BROAD_SWORD }
    },

    {
        /* ForceTrainer */
        { TV_SORCERY_BOOK, 0 },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
        { TV_POTION, SV_POTION_RESTORE_MANA }
    },

    {
        /* Blue Mage */
        { TV_SOFT_ARMOR, SV_ROBE },
        { TV_WAND, EFFECT_BOLT_MISSILE },
        { TV_SWORD, SV_DAGGER }
    },

    {
        /* Cavalry */
        { TV_BOW, SV_SHORT_BOW },
        { TV_SOFT_ARMOR, SV_LEATHER_SCALE_MAIL},
        { TV_POLEARM, SV_BROAD_SPEAR}
    },

    {
        /* Berserker */
        { TV_POTION, SV_POTION_HEALING },
        { TV_HARD_ARMOR, SV_AUGMENTED_CHAIN_MAIL },
        { TV_POLEARM, SV_BROAD_AXE }
    },

    {
        /* Weaponsmith */
        { TV_HARD_ARMOR, SV_CHAIN_MAIL },
        { TV_POLEARM, SV_BROAD_AXE },
        { TV_BOW, SV_LIGHT_XBOW }
    },
    {
        /* Mirror-Master */
        { TV_POTION, SV_POTION_SPEED },
        { TV_SWORD, SV_DAGGER },
        { TV_POTION, SV_POTION_RESTORE_MANA }
    },
    {
        /* Ninja */
        { TV_POTION, SV_POTION_SPEED },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
        { TV_SWORD, SV_DAGGER }
    },
    {
        /* Sniper */
        { TV_BOW, SV_LIGHT_XBOW },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
        { TV_SWORD, SV_DAGGER }
    },
    {
        /* Time Lord */
        { TV_SWORD, SV_SHORT_SWORD },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
        { TV_POTION, SV_POTION_SPEED }
    },
    {
        /* Blood Knight */
        { TV_POTION, SV_POTION_CURE_CRITICAL },
        { TV_HARD_ARMOR, SV_CHAIN_MAIL },
        { TV_SWORD, SV_BROAD_SWORD }
    },
    {
        /* Warlock */
        { TV_POTION, SV_POTION_SPEED },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR},
        { TV_SWORD, SV_SHORT_SWORD },
    },
    {
        /* Archaeologist */
        { TV_SCROLL, SV_SCROLL_MAPPING },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR},
        { TV_HAFTED, SV_WHIP},
    },
    {
        /* Duelist */
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
        { TV_SWORD, SV_RAPIER },
        { TV_POTION, SV_POTION_SPEED },
    },
    {
        /* Wild-Talent */
        { TV_POTION, SV_POTION_SPEED },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR},
        { TV_SWORD, SV_SMALL_SWORD },
    },
    {
        /* Rune-Knight */
        { TV_POTION, SV_POTION_SPEED },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR},
        { TV_SWORD, SV_BROAD_SWORD },
    },
    {
        /* Weaponmaster */
        { TV_POTION, SV_POTION_SPEED },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR},
        { TV_SCROLL, SV_SCROLL_PHASE_DOOR },
    },

    {
        /* Blood Mage */
        { TV_SORCERY_BOOK, 0 }, /* Hack: for realm1 book */
        { TV_DEATH_BOOK, 0 }, /* Hack: for realm2 book */
        { TV_SWORD, SV_DAGGER }
    },

    {
        /* Necromancer */
        { TV_NECROMANCY_BOOK, 0 },
        { TV_SCROLL, SV_SCROLL_PHASE_DOOR },
        { TV_WHISTLE, 0 },
    },

    {
        /* Psion */
        { TV_SWORD, SV_SMALL_SWORD },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
        { TV_POTION, SV_POTION_RESTORE_MANA }
    },

    {
        /* Rage-Mage */
        { TV_RAGE_BOOK, 0 },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR},
        { TV_SWORD, SV_BROAD_SWORD },
    },

    {
        /* Scout */
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR },
        { TV_SWORD, SV_DAGGER },
        { TV_BOW, SV_LIGHT_XBOW },
    },

    {
        /* Mauler */
        { TV_HARD_ARMOR, SV_CHAIN_MAIL },
        { TV_SWORD, SV_TWO_HANDED_SWORD },
        { TV_BOOTS, SV_PAIR_OF_METAL_SHOD_BOOTS },
    },

    {
        /* Monster TODO */
        { 0, 0 },
        { 0, 0 },
        { 0, 0 }
    },

    {
        /* Mystic TODO */
        { 0, 0 },
        { 0, 0 },
        { 0, 0 }
    },

    {
        /* Devicemaster */
        { TV_WAND, EFFECT_BOLT_MISSILE },
        { TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR},
        { TV_SWORD, SV_SHORT_SWORD },
    },
};


/*
 * Hook function for human corpses
 */
static bool monster_hook_human(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (r_ptr->flags1 & (RF1_UNIQUE)) return FALSE;

    if (my_strchr("pht", r_ptr->d_char)) return TRUE;

    return FALSE;
}

/*
 * Add an outfit object
 */
void add_outfit(object_type *o_ptr)
{
    int slot;

    if (spoiler_hack) return;

    object_aware(o_ptr);
    ego_aware(o_ptr);
    object_known(o_ptr);
    o_ptr->ident |= IDENT_FULL;

    slot = equip_first_empty_slot(o_ptr);
    if (slot && o_ptr->number == 1) /* Fix later for torches ... */
        equip_wield_aux(o_ptr, slot);
    else
        slot = inven_carry(o_ptr);

    autopick_alter_item(slot, FALSE);
}


/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
static void _birth_object(int tv, int sv, int qty)
{
    object_type forge;

    switch (tv)
    {
    case TV_WAND: case TV_ROD: case TV_STAFF:
    {
        int k_idx = lookup_kind(tv, SV_ANY);
        object_prep(&forge, k_idx);
        if (!device_init_fixed(&forge, sv))
            return;
        qty = 1;
        break;
    }
    default:
        object_prep(&forge, lookup_kind(tv, sv));
    }

    forge.number = qty;
    identify_item(&forge);
    forge.ident |= IDENT_FULL;
    add_outfit(&forge);
}
 
void player_outfit(void)
{
    int i, tv, sv, k_idx;

    object_type    forge;

    /* Give the player some food */
    switch (p_ptr->prace)
    {
    case RACE_VAMPIRE:
    case RACE_MON_VAMPIRE:
        /* Nothing! */
        /* Vampires can drain blood of creatures */
        break;

    case RACE_BALROG:
    case RACE_MON_DEMON:
        /* Demon can drain vitality from humanoid corpse */
        get_mon_num_prep(monster_hook_human, NULL);

        for (i = rand_range(3,4); i > 0; i--)
        {
            object_prep(&forge, lookup_kind(TV_CORPSE, SV_CORPSE));
            forge.pval = get_mon_num(2);
            forge.number = 1;
            add_outfit(&forge);
        }
        break;

    case RACE_SKELETON:
    case RACE_GOLEM:
    case RACE_ZOMBIE:
    case RACE_SPECTRE:
    case RACE_MON_GOLEM:
    case RACE_MON_SWORD:
    case RACE_MON_RING:
        _birth_object(TV_STAFF, EFFECT_NOTHING, 1);
        break;

    case RACE_ENT:
        _birth_object(TV_POTION, SV_POTION_WATER, rand_range(15, 23));
        break;

    case RACE_ANDROID:
    {
        int k_idx = lookup_kind(TV_FLASK, SV_ANY);
        object_prep(&forge, k_idx);

        /* Fuel with oil (move pval to xtra4) */
        apply_magic(&forge, 1, AM_NO_FIXED_ART);

        forge.number = (byte)rand_range(7, 12);
        add_outfit(&forge);

        break;
    }
    case RACE_MON_JELLY:
        break;
    case RACE_MON_ELEMENTAL:
        if (elemental_is_(ELEMENTAL_WATER))
            _birth_object(TV_POTION, SV_POTION_WATER, rand_range(15, 23));
        break;
    default:
        _birth_object(TV_FOOD, SV_FOOD_RATION, 2 + rand_range(3, 7));
    }

    if ((p_ptr->prace == RACE_VAMPIRE) && (p_ptr->pclass != CLASS_NINJA))
    {
        _birth_object(TV_SCROLL, SV_SCROLL_DARKNESS, rand_range(2, 5));
    }
    else if ( p_ptr->prace == RACE_MON_JELLY 
           || p_ptr->prace == RACE_MON_SPIDER 
           || p_ptr->prace == RACE_MON_VAMPIRE
           || p_ptr->prace == RACE_MON_SWORD
           || p_ptr->prace == RACE_MON_RING )
    {
    }
    else if (p_ptr->pclass != CLASS_NINJA)
    {
        object_prep(&forge, lookup_kind(TV_LITE, SV_LITE_TORCH));
        forge.number = (byte)rand_range(3, 7);
        forge.xtra4 = rand_range(3, 7) * 500;
        add_outfit(&forge);
    }

    if (p_ptr->prace == RACE_SNOTLING)
    {
        _birth_object(TV_FOOD, SV_FOOD_CURE_SERIOUS, randint1(3));
    }

    if ((p_ptr->pclass == CLASS_RANGER) || (p_ptr->pclass == CLASS_CAVALRY))
    {
        _birth_object(TV_ARROW, SV_AMMO_NORMAL, rand_range(15, 20));
    }

    if (demigod_is_(DEMIGOD_ARTEMIS))
    {
        _birth_object(TV_BOW, SV_SHORT_BOW, 1);
        _birth_object(TV_ARROW, SV_AMMO_NORMAL, rand_range(15, 20));
    }
    else if (p_ptr->pclass == CLASS_RANGER)
    {
        _birth_object(TV_BOW, SV_SHORT_BOW, 1);
    }
    else if (p_ptr->pclass == CLASS_ARCHER)
    {
        _birth_object(TV_ARROW, SV_AMMO_NORMAL, rand_range(15, 20));
    }
    else if (p_ptr->pclass == CLASS_HIGH_MAGE)
    {
        object_prep(&forge, lookup_kind(TV_WAND, SV_ANY));
        if (device_init_fixed(&forge, EFFECT_BOLT_MISSILE))
            add_outfit(&forge);
    }
    else if (p_ptr->pclass == CLASS_SORCERER)
    {
        for (i = TV_LIFE_BOOK; i <= TV_LIFE_BOOK+MAX_MAGIC-1; i++)
        {
            if (i == TV_NECROMANCY_BOOK) continue;
            _birth_object(i, 0, 1);
        }
    }
    else if (p_ptr->pclass == CLASS_TOURIST)
    {
        if (p_ptr->personality != PERS_SEXY)
        {
            _birth_object(TV_SHOT, SV_AMMO_LIGHT, rand_range(15, 20));
        }

        _birth_object(TV_FOOD, SV_FOOD_BISCUIT, rand_range(2, 4));
        _birth_object(TV_FOOD, SV_FOOD_WAYBREAD, rand_range(2, 4));
        _birth_object(TV_FOOD, SV_FOOD_JERKY, rand_range(1, 3));
        _birth_object(TV_FOOD, SV_FOOD_PINT_OF_ALE, rand_range(2, 4));
        _birth_object(TV_FOOD, SV_FOOD_PINT_OF_WINE, rand_range(2, 4));
        _birth_object(TV_FOOD, SV_FOOD_BISCUIT, rand_range(2, 4));
    }
    else if (p_ptr->pclass == CLASS_NINJA)
    {
        _birth_object(TV_SPIKE, 0, rand_range(15, 20));
    }
    else if (p_ptr->pclass == CLASS_SNIPER)
    {
        _birth_object(TV_BOLT, SV_AMMO_NORMAL, rand_range(15, 20));
    }
    else if (p_ptr->pclass == CLASS_SCOUT)
    {
        _birth_object(TV_BOLT, SV_AMMO_NORMAL, rand_range(15, 20));
    }

    /* Hack -- Give the player three useful objects */
    for (i = 0; i < 3; i++)
    {
        /* Look up standard equipment */
        tv = player_init[p_ptr->pclass][i][0];
        sv = player_init[p_ptr->pclass][i][1];
        
        if (!tv) continue;
        if (p_ptr->prace == RACE_DEMIGOD && p_ptr->psubrace == DEMIGOD_ARTEMIS && tv == TV_BOW && sv == SV_SHORT_BOW) continue;
        if ((p_ptr->prace == RACE_ANDROID) && ((tv == TV_SOFT_ARMOR) || (tv == TV_HARD_ARMOR))) continue;

        /* Hack to initialize spellbooks */
        if (tv == TV_SORCERY_BOOK) tv = TV_LIFE_BOOK + p_ptr->realm1 - 1;
        else if (tv == TV_DEATH_BOOK) tv = TV_LIFE_BOOK + p_ptr->realm2 - 1;

        switch (tv)
        {
        case TV_WAND: case TV_ROD: case TV_STAFF:
            k_idx = lookup_kind(tv, SV_ANY);
            object_prep(&forge, k_idx);
            if (device_init_fixed(&forge, sv))
                add_outfit(&forge);
            break;
        default:
            k_idx = lookup_kind(tv, sv);
            if (!k_idx) continue;
            object_prep(&forge, k_idx);
            /* Assassins begin the game with a poisoned dagger */
            if ((tv == TV_SWORD || tv == TV_HAFTED) && (p_ptr->pclass == CLASS_ROGUE &&
                p_ptr->realm1 == REALM_DEATH)) /* Only assassins get a poisoned weapon */
            {
                forge.name2 = EGO_WEAPON_VENOM;
            }

            /* Hack: Rune-Knights begin with an Absorption Rune on their broad sword (or whip if sexy) */
            if (p_ptr->pclass == CLASS_RUNE_KNIGHT && tv == TV_SWORD && sv == SV_BROAD_SWORD)
                rune_add(&forge, RUNE_ABSORPTION, FALSE);

            add_outfit(&forge);
        }
    }

    /* Hack -- make aware of the water */
    k_info[lookup_kind(TV_POTION, SV_POTION_WATER)].aware = TRUE;
}

static bool get_stat_limits(void)
{
    int i, j, m, cs, os;
    int cval[6];
    char c;
    char buf[80], cur[80];
    char inp[80];
    race_t *race_ptr = get_race();
    class_t *class_ptr = get_class();
    personality_ptr pers_ptr = get_personality();

    /* Clean up */
    clear_from(10);

    /* Extra infomation */
    put_str("Set minimum stats.", 10, 10);
    put_str("2/8 for Select, 4/6 for Change value, Enter for Goto next", 11, 10);
    put_str("           Base   Rac  Cla  Per      Total  Maximum", 13, 10);

    /* Output the maximum stats */
    for (i = 0; i < 6; i++)
    {
        /* Reset the "success" counter */
        stat_match[i] = 0;
        cval[i] = 3;

        /* Race/Class bonus */
        j = race_ptr->stats[i] + class_ptr->stats[i] + pers_ptr->stats[i];

        /* Obtain the "maximal" stat */
        m = adjust_stat(17, j);

        if (m > 18)
            sprintf(cur, "18/%02d", (m - 18));        
        else
            sprintf(cur, "%2d", m);

        /* Obtain the current stat */
        m = adjust_stat(cval[i], j);

        if (m > 18)
            sprintf(inp, "18/%02d", (m - 18));
        else
            sprintf(inp, "%2d", m);

        sprintf(buf, "%6s       %2d   %+3d  %+3d  %+3d  =  %6s  %6s",
            stat_names[i], cval[i], race_ptr->stats[i], class_ptr->stats[i],
            pers_ptr->stats[i], inp, cur);
        
        put_str(buf, 14 + i, 10);
    }
    
    /* Get a minimum stat */
    cs = 0;
    os = 6;
    while (TRUE)
    {
        /* Move Cursol */
        if (cs != os)
        {
            if(os == 6)
                c_put_str(TERM_WHITE, "Accept", 21, 35);
            else if(os < 6)
                c_put_str(TERM_WHITE, cur, 14 + os, 10);
            
            if(cs == 6)
                c_put_str(TERM_YELLOW, "Accept", 21, 35);
            else
            {
                /* Race/Class bonus */
                j = race_ptr->stats[cs] + class_ptr->stats[cs] + pers_ptr->stats[cs];

                /* Obtain the current stat */
                m = adjust_stat(cval[cs], j);
                
                if (m > 18)
                    sprintf(inp, "18/%02d", (m - 18));
                else
                    sprintf(inp, "%2d", m);
                
                sprintf(cur, "%6s       %2d   %+3d  %+3d  %+3d  =  %6s",
                    stat_names[cs], cval[cs], race_ptr->stats[cs],
                    class_ptr->stats[cs], pers_ptr->stats[cs], inp);
                c_put_str(TERM_YELLOW, cur, 14 + cs, 10);
            }
            os = cs;
        }
        
        /* Prompt for the minimum stats */
        c = inkey();
        switch ( c ){
        case 'Q':
            birth_quit();
        case 'S':
            return FALSE;
        case ESCAPE:
            break;
        case ' ':
        case '\r':
        case '\n':
            if(cs == 6) break;
            cs++;
            c = '2';
            break;
        case '8':
        case 'k':
            if (cs > 0) cs--;
            break;
        case '2':
        case 'j':
            if (cs < 6) cs++;
            break;
        case '4':
        case 'h':
            if (cs != 6)
            {
                if (cval[cs] == 3)
                {
                    cval[cs] = 17;
                    os = 7;
                }
                else if (cval[cs] > 3)
                {
                    cval[cs]--;
                    os = 7;
                }
                else return FALSE;
            }
            break;
        case '6':
        case 'l':
            if (cs != 6)
            {
                if (cval[cs] == 17)
                {
                    cval[cs] = 3;
                    os = 7;
                }
                else if (cval[cs] < 17)
                {
                    cval[cs]++;
                    os = 7;
                }
                else return FALSE;
            }
            break;
        case 'm':
            if(cs != 6)
            {
                cval[cs] = 17;
                os = 7;
            }
            break;
        case 'n':
            if(cs != 6)
            {
                cval[cs] = 3;
                os = 7;
            }
            break;
        case '?':
            show_help("birth.txt#AutoRoller");
            break;
        case '=':
            screen_save();
            do_cmd_options_aux(OPT_PAGE_BIRTH, "Birth Option((*)s effect score)");
            screen_load();
            break;
        default:
            bell();
            break;
        }
        if(c == ESCAPE || ((c == ' ' || c == '\r' || c == '\n') && cs == 6))break;
    }
    
    for (i = 0; i < 6; i++)
    {
        /* Save the minimum stat */
        stat_limit[i] = cval[i];
    }

    return TRUE;
}

cptr birth_get_realm_desc(int i)
{
    return realm_jouhou[i-1];
}

static bool player_birth_aux(void)
{
    int i;

    bool stop = FALSE;
    bool flag = FALSE;
    bool prev = FALSE;
    bool use_autoroller = autoroller;

    char c;

    char b1 = '[';
    char b2 = ']';

    char buf[80];


    /*** Intro ***/
    Term_clear();
    put_str("Name       :", 1, 1);
    put_str("Sex        :", 2, 1);
    put_str("Personality:", 3, 1);
    put_str("Race       :", 4, 1);
    put_str("Subrace    :", 5, 1);
    put_str("Class      :", 6, 1);
    put_str("Subclass   :", 7, 1);
    c_put_str(TERM_L_BLUE, player_name, 1, 14);

    put_str("Make your character. ('S' Restart, 'Q' Quit, 'ESC' Previous, '?' Help)", 9, 10);

    if (!_prompt()) return FALSE;
    equip_on_init();

    /* Clean up */
    put_str("Make your character. ('S' Restart, 'Q' Quit, '?' Help)                ", 9, 10);
    clear_from(10);
    put_str("                                     ", 3, 40);
    put_str("                                     ", 4, 40);
    put_str("                                     ", 5, 40);

    if (game_mode == GAME_MODE_BEGINNER)
    {
        no_wilderness = TRUE;
    }
    else
    {
        /*no_wilderness = FALSE;*/
        screen_save();
        do_cmd_options_aux(OPT_PAGE_BIRTH, "Birth Option((*)s effect score)");
        screen_load();
    }

    if (game_mode == GAME_MODE_REAL_LIFE)
    {
        use_autoroller = FALSE;
        stop = TRUE;
    }

    /*** Autoroll ***/
auto_roller_barf:
    if (use_autoroller)
    {
        /* Clear fields */
        auto_round = 0L;
    }

    /* Initialize */
    if (use_autoroller)
    {
        if (!get_stat_limits()) return FALSE;
    }

    /* Clear */
    clear_from(10);

    /* Reset turn; before auto-roll and after choosing race */
    init_turn();

    /*** Generate ***/

    /* Roll */
    while (TRUE)
    {
        int col;

        col = 42;

        if (use_autoroller)
        {
            Term_clear();
            put_str("Round:", 10, col+13);
            put_str("(Hit ESC to stop)", 12, col+13);
        }
        else if (game_mode != GAME_MODE_REAL_LIFE)
        {
            get_stats();
        }

        if (use_autoroller)
        {
            class_t *class_ptr = get_class();
            race_t  *race_ptr = get_race();
            personality_ptr pers_ptr = get_personality();

            put_str(" Limit", 2, col+5);
            put_str("  Freq", 2, col+13);
            put_str("  Roll", 2, col+24);

            /* Put the minimal stats */
            for (i = 0; i < 6; i++)
            {
                int j, m;

                /* Label stats */
                put_str(stat_names[i], 3+i, col);

                /* Race/Class bonus */
                j = race_ptr->stats[i] + class_ptr->stats[i] + pers_ptr->stats[i];

                /* Obtain the current stat */
                m = adjust_stat(stat_limit[i], j);

                /* Put the stat */
                cnv_stat(m, buf);
                c_put_str(TERM_L_BLUE, buf, 3+i, col+5);
            }
        }

        while (use_autoroller)
        {
            bool accept = TRUE;

            get_stats();
            auto_round++;

            /* Hack -- Prevent overflow */
            if (auto_round >= 1000000000L)
            {
                auto_round = 1;

                if (use_autoroller)
                {
                    for (i = 0; i < 6; i++)
                        stat_match[i] = 0;
                }
            }

            if (use_autoroller)
            {
                /* Check and count acceptable stats */
                for (i = 0; i < 6; i++)
                {
                    /* This stat is okay */
                    if (p_ptr->stat_max[i] >= stat_limit[i])
                    {
                        stat_match[i]++;
                    }

                    /* This stat is not okay */
                    else
                    {
                        accept = FALSE;
                    }
                }
            }

            /* Hack: Enforce a maximum number of rolls. If we never matched, 
               go back and re-prompt for stat limits. If we did match, force
               the last successful match as the starting character. */
            if (auto_round >= AUTOROLLER_MAX)
            {
                if (prev)
                {
                    load_prev_data(TRUE);
                    stop = TRUE;
                    break;
                }
                else
                    goto auto_roller_barf;
            }

            if (accept)
            {
                if (accept) break;
            }

            /* Update display occasionally */
            flag = (!(auto_round % AUTOROLLER_STEP));
            if (flag)
            {
                birth_put_stats();

                /* Dump round */
                put_str(format("%10d", auto_round), 10, col+20);

#ifdef AUTOROLLER_DELAY
                if (flag) Term_xtra(TERM_XTRA_DELAY, 10);
#endif

                /* Make sure they see everything */
                Term_fresh();

                /* Do not wait for a key */
                inkey_scan = TRUE;

                /* Check for a keypress */
                if (inkey())
                    break;
            }
        }

        if (use_autoroller) sound(SOUND_LEVEL);

        flush();


        /*** Display ***/

        /* Roll for base hitpoints */
        get_extra(TRUE);

        /* Roll for gold */
        get_money();

        /* Hack -- get a chaos patron even if you are not a chaos warrior */
        p_ptr->chaos_patron = (s16b)randint0(MAX_PATRON);

        if (p_ptr->prace == RACE_BEASTMAN)
            p_ptr->birth_mutation = mut_gain_random_aux(mut_good_pred);

        /* Input loop */
        while (TRUE)
        {
            /* Calculate the bonuses and hitpoints */
            p_ptr->update |= (PU_BONUS | PU_HP | PU_MANA);

            /* Update stuff */
            update_stuff();

            /* Fully healed */
            p_ptr->chp = p_ptr->mhp;

            /* Fully rested */
            p_ptr->csp = p_ptr->msp;

            /* Display the player */
            py_display_birth();

            if (p_ptr->prace == RACE_BEASTMAN)
            {
                char buf[100];
                put_str("Mutation :", 8, 1);
                mut_name(p_ptr->birth_mutation, buf);
                c_put_str(TERM_L_BLUE, buf, 8, 12);
            }

            /* Prepare a prompt (must squeeze everything in) */
            Term_gotoxy(2, 23);
            Term_addch(TERM_WHITE, b1);

            if (stop) Term_addstr(-1, TERM_WHITE, "You must accept");
            else Term_addstr(-1, TERM_WHITE, "'r'eroll");
            if (prev && !stop) Term_addstr(-1, TERM_WHITE, ", 'p'previous");
            Term_addstr(-1, TERM_WHITE, ", or Enter to accept");
            Term_addch(TERM_WHITE, b2);

            c = inkey();
            if (c == 'Q') birth_quit();
            if (c == 'S') return (FALSE);
            if (c == '\r' || c == '\n' || c == ESCAPE) break;
            if (!stop && (c == ' ' || c == 'r')) break;
            if (prev && !stop && (c == 'p'))
            {
                load_prev_data(TRUE);
                continue;
            }
            if (c == '?')
            {
                show_help("birth.txt#AutoRoller");
                continue;
            }
            else if (c == '=')
            {
                screen_save();
                do_cmd_options_aux(OPT_PAGE_BIRTH, "Birth Option((*)s effect score)");
                screen_load();
                continue;
            }
            bell();
        }

        /* Are we done? */
        if (c == '\r' || c == '\n' || c == ESCAPE) break;

        /* Save this for the "previous" character */
        save_prev_data(&previous_char);
        previous_char.quick_ok = FALSE;

        /* Note that a previous roll exists */
        prev = TRUE;
    }

    /* Clear prompt */
    clear_from(23);

    /* Get a name, recolor it, prepare savefile */
    py_get_name();

    /* Re-Draw the name (in light blue) */
    Term_erase(34, 1, 255);
    c_put_str(TERM_L_BLUE, player_name, 1, 14);

    /* Erase the prompt, etc */
    clear_from(22);

    /* Process the player name */
    process_player_name(creating_savefile);

    /*** Finish up ***/
    get_max_stats();
    virtue_init();

    /* Prompt for it */
    prt("['Q'uit, 'S'tart over, or Enter to continue]", 23, 10);

    /* Get a key */
    c = inkey();

    /* Quit */
    if (c == 'Q') birth_quit();

    /* Start over */
    if (c == 'S') return (FALSE);

    /* Initialize random quests */
    init_dungeon_quests();

    /* Save character data for quick start */
    save_prev_data(&previous_char);
    previous_char.quick_ok = TRUE;

    /* Accept */
    return (TRUE);
}


/*
 * Ask whether the player use Quick Start or not.
 */
static bool ask_quick_start(void)
{
    /* Doesn't have previous data */
    if (!previous_char.quick_ok) return FALSE;


    /* Clear screen */
    Term_clear();

    /* Extra info */
    put_str("Do you want to use the quick start function(same character as your last one).", 11, 2);

    /* Choose */
    while (1)
    {
        char c;

        put_str("Use quick start? [y/N]", 14, 10);
        c = inkey();

        if (c == 'Q') quit(NULL);
        else if (c == 'S') return (FALSE);
        else if (c == '?')
        {
            show_help("birth.txt#QuickStart");
        }
        else if ((c == 'y') || (c == 'Y'))
        {
            /* Yes */
            break;
        }
        else
        {
            /* No */
            return FALSE;
        }
    }

    load_prev_data(FALSE);
    equip_on_init();
    get_max_stats();
    do_cmd_rerate_aux();
    init_dungeon_quests();
    init_turn();

    sp_ptr = &sex_info[p_ptr->psex];
    mp_ptr = &m_info[p_ptr->pclass];

    /* Calc hitdie, but don't roll */
    get_extra(FALSE);

    /* Calculate the bonuses and hitpoints */
    p_ptr->update |= (PU_BONUS | PU_HP);

    /* Update stuff */
    update_stuff();

    /* Fully healed */
    p_ptr->chp = p_ptr->mhp;

    /* Fully rested */
    p_ptr->csp = p_ptr->msp;

    /* Process the player name */
    process_player_name(FALSE);

    return TRUE;
}


/*
 * Create a new character.
 *
 * Note that we may be called with "junk" leftover in the various
 * fields, so we must be sure to clear them first.
 */
bool birth_hack = FALSE;
void player_birth(void)
{
    int i, j;

    birth_hack = TRUE;
    playtime = 0;
    
    wipe_m_list();
    player_wipe();

    /* Create a new character */

    /* Quick start? */
    if (!ask_quick_start())
    {
        /* No, normal start */
        while (1)
        {
            if (player_birth_aux()) break;
            player_wipe();
        }
    }
    else
    {
        if (p_ptr->prace == RACE_BEASTMAN && !p_ptr->birth_mutation)
            p_ptr->birth_mutation = mut_gain_random_aux(mut_good_pred);
    }

    /* Init the shops */
    for (i = 1; i < max_towns; i++)
    {
        for (j = 0; j < MAX_STORES; j++)
            store_init(i, j);
    }

    /* Generate the random seeds for the wilderness */
    seed_wilderness();

    /* Give beastman a mutation at character birth */
    if (p_ptr->prace == RACE_BEASTMAN) 
        mut_gain(p_ptr->birth_mutation);

    /* Set the message window flag as default */
    if (!window_flag[1])
        window_flag[1] |= PW_MESSAGE;

    /* Set the inv/equip window flag as default */
    if (!window_flag[2])
        window_flag[2] |= PW_INVEN;

    birth_hack = FALSE;

    /* Hack: Gain CL1 */
    {
        class_t *class_ptr = get_class();
        if (class_ptr != NULL && class_ptr->gain_level != NULL)
            (class_ptr->gain_level)(p_ptr->lev);
    }
}

