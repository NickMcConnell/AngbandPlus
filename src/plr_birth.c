#include "angband.h"

#include <assert.h>

/************************************************************************
 * Give birth to a new player.
 *
 * For reference, there are 3 possible game modes:
 * [1] Beginner: Limited Races and Classes
 *               No Wilderness
 *               No Personality
 *               No Birth Options
 * [2] Normal:   Standard Game with All Races/Classes/etc.
 * [3] Monster:  Monster Races
 *               No Class (CLASS_MONSTER is a bunch of zeros!)
 *
 * *_ui() functions run menu loops according to the following flow graph:
 ***********************************************************************/
extern int plr_birth(void);
    static int _welcome_ui(void);
    static int _world_ui(void);
    static int _race_class_ui(void);
        static void _pers_ui(void);
        /* Normal Mode */
        static void _race_group_ui(void);
            static int _race_ui(int ids[]);
                static int _subrace_ui(void);
                    static int _demigod_ui(void);
                    static int _draconian_ui(void);
        static void _class_group_ui(void);
            static int _class_ui(int ids[]);
                static int _subclass_ui(void);
                    static int _warlock_ui(void);
                    static int _weaponmaster_ui(void);
                    static int _devicemaster_ui(void);
                    static int _gray_mage_ui(void);
        static int _realm1_ui(void);
            static int _realm2_ui(void);
        /* Monster Mode */
        static void _mon_race_group_ui(void);
            static int _mon_race_ui(int ids[]);
                static int _mon_subrace_ui(void);
                    static int _mon_demon_ui(void);
                    static int _mon_dragon_ui(void);
                        static int _dragon_realm_ui(void);
                    static int _mon_elemental_ui(void);
                    static int _mon_giant_ui(void);
                    static int _mon_golem_ui(void);
                    static int _mon_lich_ui(void);
                    static int _mon_spider_ui(void);
                    static int _mon_troll_ui(void);
    static int _stats_ui(void);

extern void plr_birth_obj(object_type *o_ptr);
extern void plr_birth_obj_aux(int tval, int sval, int qty);
extern void plr_birth_food(void);
extern void plr_birth_light(void);
extern void plr_birth_spellbooks(void);

/* I prefer to render menus to a document rather than directly to the terminal */
static doc_ptr _doc = NULL;

static void _birth_options(void);
static void _birth_finalize(void);
static int _inkey(void);
static void _sync_term(doc_ptr doc);
static int _count(int ids[]);

/************************************************************************
 * Public Entrypoints
 ***********************************************************************/ 
int plr_birth(void)
{
    int result = UI_OK;

    /* Windows is not setting a player name for new files */
    if (0 == strlen(player_name))
        strcpy(player_name, "PLAYER");

    assert(!_doc);
    _doc = doc_alloc(80);

    msg_line_clear();
    Term_clear();
    Term_save();
    result = _welcome_ui();
    Term_load();

    doc_free(_doc);
    _doc = NULL;

    return result;
}

void plr_birth_obj_aux(int tval, int sval, int qty)
{
    object_type forge;

    switch (tval)
    {
    case TV_WAND: case TV_ROD: case TV_STAFF:
    {
        int k_idx = lookup_kind(tval, SV_ANY);
        object_prep(&forge, k_idx);
        if (!device_init_fixed(&forge, sval))
            return;
        qty = 1;
        break;
    }
    case TV_QUIVER:
        object_prep(&forge, lookup_kind(tval, sval));
        apply_magic(&forge, 0, AM_AVERAGE);
        break;
    default:
        object_prep(&forge, lookup_kind(tval, sval));
    }

    forge.number = qty;
    plr_birth_obj(&forge);
}

void plr_birth_obj(object_type *o_ptr)
{
    int slot;

    if (spoiler_hack)
        return;

    /* Androids can hit CL9 or more with starting Chain Mail! */
    if (plr->prace == RACE_ANDROID && obj_is_body_armor(o_ptr))
        return;

    /* Weed out duplicate gear (e.g. Artemis Archer) but note
     * that centipedes start with duplicate boots (so allow
     * multiple objects provided they can also be equipped) */
    if ( obj_is_wearable(o_ptr)
      && o_ptr->number == 1
      && plr->prace != RACE_MON_RING /* Hack: Ring cannot wear rings, but can absorb them for powers */
      && equip_find_obj(o_ptr->tval, o_ptr->sval)
      && !equip_first_empty_slot(o_ptr) ) /* Hack: Centipede gets multiple boots */
    {
        return;
    }

    obj_identify_fully(o_ptr);

    /* Big hack for sexy players ... only wield the starting whip,
     * but carry the alternate weapon. Previously, sexy characters
     * would usually start off dual-wielding (ineffectual and confusing)*/
    if ( plr->personality == PERS_SEXY
      && plr->prace != RACE_MON_SWORD
      && obj_is_weapon(o_ptr)
      && !object_is_(o_ptr, TV_HAFTED, SV_WHIP) )
    {
        pack_carry(o_ptr);
        return;
    }

    slot = equip_first_empty_slot(o_ptr);
    if (slot && o_ptr->number == 1)
        equip_wield(o_ptr, slot);
    else
        pack_carry(o_ptr);
}

/* Standard Food and Light */
void plr_birth_food(void)
{
    plr_birth_obj_aux(TV_FOOD, SV_FOOD_RATION, 2 + rand_range(3, 7));
}

void plr_birth_light(void)
{
    if (plr->pclass != CLASS_NINJA && plr->pclass != CLASS_NECROMANCER)
    {
        object_type forge = {0};
        object_prep(&forge, lookup_kind(TV_LIGHT, SV_LIGHT_TORCH));
        forge.number = rand_range(3, 7);
        forge.xtra4 = rand_range(3, 7) * 500;
        plr_birth_obj(&forge);
    }
}

void plr_birth_spellbooks(void)
{
    if (plr->realm1)
        plr_birth_obj_aux(TV_LIFE_BOOK + plr->realm1 - 1, 0, 1);
    if (plr->realm2)
        plr_birth_obj_aux(TV_LIFE_BOOK + plr->realm2 - 1, 0, 1);
}

void plr_birth_pet(cptr which)
{
    point_t pos = dun_scatter_aux(cave, plr->pos, 4, dun_allow_mon_at);
    mon_race_ptr race = mon_race_parse(which);
    mon_ptr mon = place_monster_aux(who_create_null(), pos, race, PM_FORCE_PET | PM_NO_KAGE);

    if (mon)
    {
        mon_race_ptr race = mon->race;
        int hp = dice_avg_roll(race->hp);

        mon->mspeed = race->move.speed;
        mon->maxhp = hp;
        mon->max_maxhp = hp;
        mon->hp = hp;
        mon->energy_need = ENERGY_NEED() + ENERGY_NEED();
    }
}

/************************************************************************
 * Welcome to Poschengband!
 ***********************************************************************/ 
static void _set_mode(int mode);
static bool _stats_changed = FALSE;

static int _welcome_ui(void)
{
    for (;;)
    {
        int cmd;

        doc_clear(_doc);

        doc_printf(_doc,
            "Welcome to <color:keyword>%s</color>, a dungeon exploration role playing game. "
            "Your goal is to defeat <color:keyword>Morgoth, Lord of Darkness</color>, "
            "but before you can face him, you must battle many foes. "
            "Your first step is to create a character for this quest. "
            "The following screens will guide you through this process so that you "
            "may quickly begin playing. You can get general help at any time by "
            "pressing <color:keypress>?</color>.\n\n", VERSION_NAME);

        doc_insert(_doc,
            "First, you must decide what type of game to play. If you are new, "
            "it is recommended you play in <color:keyword>Beginner Mode</color>.\n\n"
        );

        doc_insert(_doc, "<color:G>Choose the Type of Game to Play</color>\n");
        doc_insert(_doc, "  <color:y>b</color>) Beginner\n");
        doc_insert(_doc, "  <color:y>n</color>) Normal\n");
        doc_insert(_doc, "  <color:y>m</color>) Monster\n");
        doc_newline(_doc);
        if (previous_char.quick_ok)
            doc_insert(_doc, "  <color:y>q</color>) Quick Start\n");
        if (game_mode != GAME_MODE_BEGINNER)
            doc_insert(_doc, "  <color:y>=</color>) Options\n");
        doc_insert(_doc, "  <color:y>?</color>) Help\n");
        doc_insert(_doc, "  <color:y>s</color>) View Scores\n");
        doc_insert(_doc, "<color:y>ESC</color>) <color:v>Quit</color>\n");


        doc_newline(_doc);
        doc_insert(_doc, "<color:G>Tip:</color> <indent>You can often get specific "
                         "help by entering the uppercase letter for a command. For "
                         "example, type <color:keypress>B</color> on this screen "
                         "to receive help on Beginner Mode.</indent>");
        _sync_term(_doc);
        cmd = _inkey();
        if (cmd == '?')
            doc_display_help("birth.txt", NULL /*"Welcome"*/);
        else if (cmd == ESCAPE)
            return UI_CANCEL;
        else if (cmd == '=')
            _birth_options();
        else if (cmd == 'q' && previous_char.quick_ok)
        {
            int i;
            plr->initial_world_id = previous_char.initial_world_id;
            if (!plr->initial_world_id)
                plr->initial_world_id = W_SMAUG; /* paranoia */
            game_mode = previous_char.game_mode;
            plr->psex = previous_char.psex;
            plr->prace = previous_char.prace;
            plr->psubrace = previous_char.psubrace;
            plr->pclass = previous_char.pclass;
            plr->psubclass = previous_char.psubclass;
            plr->personality = previous_char.personality;
            plr->realm1 = previous_char.realm1;
            plr->realm2 = previous_char.realm2;
            plr->dragon_realm = previous_char.dragon_realm;
            plr->au = previous_char.au;
            for (i = 0; i < MAX_STATS; i++)
            {
                plr->stat_cur[i] = previous_char.stat_max[i];
                plr->stat_max[i] = previous_char.stat_max[i];
            }
            _stats_changed = TRUE; /* block default stat allocation via _stats_init */
            if (_race_class_ui() == UI_OK) /* XXX skip _world_ui */
                return UI_OK;
        }
        else if (cmd == 'Q' && previous_char.quick_ok)
            doc_display_help("birth.txt", "QuickStart");
        else if (cmd == 'b')
        {
            _set_mode(GAME_MODE_BEGINNER);
            plr->initial_world_id = W_SMAUG;
            if (_race_class_ui() == UI_OK) /* XXX skip _world_ui */
                return UI_OK;
        }
        else if (cmd == 'B')
            doc_display_help("birth.txt", "BeginnerMode");
        else if (cmd == 'n')
        {
            _set_mode(GAME_MODE_NORMAL);
            if (_world_ui() == UI_OK)
                return UI_OK;
        }
        else if (cmd == 'N')
            doc_display_help("birth.txt", "NormalMode");
        else if (cmd == 'm')
        {
            _set_mode(GAME_MODE_MONSTER);
            if (_world_ui() == UI_OK)
                return UI_OK;
        }
        else if (cmd == 'M')
            doc_display_help("birth.txt", "MonsterMode");
        else if (cmd == 's')
        {
            vec_ptr scores = scores_load(NULL);
            scores_display(scores);
            vec_free(scores);
        }
    }
}

static void _stats_init(void);

static void _set_mode(int mode)
{
    static bool first = TRUE;
    if (mode == GAME_MODE_BEGINNER)
    {
        if (first || game_mode != mode)
        {
            plr->prace = RACE_HOBBIT;
            plr->psubrace = 0;
            plr->pclass = CLASS_ROGUE;
            plr->psubclass = 0;
            plr->realm1 = REALM_BURGLARY;
            plr->realm2 = REALM_NONE;
            plr->dragon_realm = DRAGON_REALM_NONE;
            plr->personality = PERS_ORDINARY;
            _stats_init();
        }
    }
    else if (mode == GAME_MODE_NORMAL)
    {
        if (first || game_mode == GAME_MODE_MONSTER)
        {
            plr->prace = RACE_HOBBIT;
            plr->psubrace = 0;
            plr->pclass = CLASS_ROGUE;
            plr->psubclass = 0;
            plr->realm1 = REALM_BURGLARY;
            plr->realm2 = REALM_NONE;
            plr->dragon_realm = DRAGON_REALM_NONE;
            plr->personality = PERS_ORDINARY;
            _stats_init();
        }
    }
    else if (mode == GAME_MODE_MONSTER)
    {
        if (first || game_mode != mode)
        {
            plr->prace = RACE_MON_TROLL;
            plr->psubrace = TROLL_ETTIN;
            plr->pclass = CLASS_MONSTER;
            plr->psubclass = 0;
            plr->realm1 = REALM_NONE;
            plr->realm2 = REALM_NONE;
            plr->dragon_realm = DRAGON_REALM_NONE;
            _stats_init();
        }
    }
    if ((first || game_mode != mode) && mode == previous_char.game_mode && previous_char.quick_ok)
    {
        plr->prace = previous_char.prace;
        plr->psubrace = previous_char.psubrace;
        plr->pclass = previous_char.pclass;
        plr->psubclass = previous_char.psubclass;
        plr->personality = previous_char.personality;
        plr->realm1 = previous_char.realm1;
        plr->realm2 = previous_char.realm2;
        plr->dragon_realm = previous_char.dragon_realm;
        _stats_init();
    }
    game_mode = mode;
    first = FALSE;
}

static int _world_ui(void)
{
    for (;;)
    {
        int cmd;

        doc_clear(_doc);

        doc_insert(_doc, "<color:G>Choose Which World to Play</color>\n");

        doc_printf(_doc, "  <color:y>a</color>) <indent><color:%c>The World of Middle Earth</color>\n"
            "You must travel to Mordor and slay both Sauron and Morgoth. This game consists "
            "of multiple worlds, each with its own wilderness map, set of dungeons, and "
            "final world guardian to slay.</indent>\n\n",
            plr->initial_world_id == W_SMAUG ? 'B' : 'U'
        );
        doc_printf(_doc, "  <color:y>b</color>) <indent><color:%c>The World of Amber</color>\n"
            "You must slay the Serpent of Chaos in order to restore balance to the world. This "
            "game consists of a single world with a randomly generated wilderness. In addition, "
            "some of the available dungeons are randomly chosen for each game.</indent>\n\n",
            plr->initial_world_id == W_AMBER ? 'B' : 'U'
        );
        doc_insert(_doc, "  <color:y>?</color>) Help\n");
        doc_insert(_doc, "<color:y>ESC</color>) Prev Screen\n");

        doc_newline(_doc);
        _sync_term(_doc);
        cmd = _inkey();
        if (cmd == '?')
            doc_display_help("birth.txt", NULL /*"World"*/);
        else if (cmd == ESCAPE)
            return UI_CANCEL;
        else if (cmd == '=')
            _birth_options();
        else if (cmd == 'a')
        {
            plr->initial_world_id = W_SMAUG;
            if (_race_class_ui() == UI_OK)
                return UI_OK;
        }
        else if (cmd == 'b')
        {
            plr->initial_world_id = W_AMBER;
            if (_race_class_ui() == UI_OK)
                return UI_OK;
        }
    }
}

/************************************************************************
 * 2) Race/Class/Personality
 *
 * Note: It assumed througout that the player starts with a valid
 * race, class and personality, and that throughout the birth process
 * these fields remain legal. Currently, player_wipe gives an Ordinary
 * Human Warrior to start.
 ***********************************************************************/ 
static void _race_class_top(doc_ptr doc);
static void _inc_rcp_state(void);
static void _change_name(void);

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

static int _race_class_ui(void)
{
    for (;;)
    {
        int cmd;
        doc_ptr cols[2];

        doc_clear(_doc);
        _race_class_top(_doc);

        cols[0] = doc_alloc(30);
        cols[1] = doc_alloc(46);

        doc_insert(cols[0], "  <color:y>n</color>) Change Name\n");
        doc_insert(cols[0], "  <color:y>s</color>) Change Sex\n");
        if (game_mode != GAME_MODE_BEGINNER)
            doc_insert(cols[0], "  <color:y>p</color>) Change Personality\n");
        doc_insert(cols[0], "  <color:y>r</color>) Change Race\n");
        if (game_mode == GAME_MODE_MONSTER)
        {
            if (plr->dragon_realm)
                doc_insert(cols[0], "  <color:y>m</color>) Change Magic\n");
        }
        else
        {
            doc_insert(cols[0], "  <color:y>c</color>) Change Class\n");
            if (plr->realm1)
                doc_insert(cols[0], "  <color:y>m</color>) Change Magic\n");
        }

        doc_insert(cols[1], "<color:y>  ?</color>) Help\n");
        if (game_mode != GAME_MODE_BEGINNER)
            doc_insert(cols[1], "<color:y>  =</color>) Options\n");
        doc_insert(cols[1], "<color:y>TAB</color>) More Info\n");
        doc_insert(cols[1], "<color:y>RET</color>) Next Screen\n");
        doc_insert(cols[1], "<color:y>ESC</color>) Prev Screen\n");

        doc_insert_cols(_doc, cols, 2, 1);
        doc_free(cols[0]);
        doc_free(cols[1]);

        doc_insert(_doc, "<color:G>Tip:</color> <indent>You can often get specific "
                         "help by entering the uppercase letter for a command. For "
                         "example, type <color:keypress>R</color> on this screen "
                         "to receive help on your currently selected race.</indent>");
        _sync_term(_doc);

        cmd = _inkey();
        switch (cmd)
        {
        case '\r':
            if (_stats_ui() == UI_OK)
                return UI_OK;
            break;
        case ESCAPE:
            return UI_CANCEL;
        case '=':
            _birth_options();
            break;
        case '\t':
            _inc_rcp_state();
            break;
        case '?':
            doc_display_help("birth.txt", "RaceClass");
            break;
        case 'n':
            _change_name();
            break;
        case 'N':
            doc_display_help("birth.txt", "CharacterName");
            break;
        case 'r':
            if (game_mode == GAME_MODE_BEGINNER)
                _race_ui(_beginner_races);
            else if (game_mode == GAME_MODE_MONSTER)
                _mon_race_group_ui();
            else
                _race_group_ui();
            break;
        case 'R':
        {
            race_t *race_ptr = get_race();
            if (plr->prace == RACE_DEMIGOD)
                doc_display_help("Demigods.txt", race_ptr->subname);
            else if (plr->prace == RACE_DRACONIAN)
                doc_display_help("Draconians.txt", race_ptr->subname);
            else if (plr->prace == RACE_MON_DEMON)
                doc_display_help("Demons.txt", race_ptr->subname);
            else if (plr->prace == RACE_MON_DRAGON)
                doc_display_help("Dragons.txt", race_ptr->subname);
            else if (game_mode == GAME_MODE_MONSTER)
                doc_display_help("MonsterRaces.txt", race_ptr->name);
            else
                doc_display_help("Races.txt", race_ptr->name);
            break;
        }
        case 'c':
            if (game_mode == GAME_MODE_BEGINNER)
                _class_ui(_beginner_classes);
            else if (game_mode != GAME_MODE_MONSTER)
                _class_group_ui();
            break;
        case 'C':
        {
            class_t *class_ptr = get_class();
            if (plr->pclass == CLASS_WARLOCK)
                doc_display_help("Warlocks.txt", class_ptr->subname);
            else if (game_mode != GAME_MODE_MONSTER)
                doc_display_help("Classes.txt", class_ptr->name);
            break;
        }
        case 'p':
            if (game_mode != GAME_MODE_BEGINNER)
                _pers_ui();
            break;
        case 'P':
        {
            if (game_mode != GAME_MODE_BEGINNER)
            {
                personality_ptr pers_ptr = get_personality();
                doc_display_help("Personalities.txt", pers_ptr->name);
            }
            break;
        }
        case 'm':
            if (plr->dragon_realm != DRAGON_REALM_NONE)
                _dragon_realm_ui();
            else if (plr->realm1)
                _realm1_ui();
            break;
        case 'M':
            if (plr->dragon_realm != DRAGON_REALM_NONE)
                doc_display_help("DragonRealms.txt", dragon_get_realm(plr->dragon_realm)->name);
            else if (plr->realm1)
                doc_display_help("magic.txt", realm_names[plr->realm1]);
            break;
        case 's':
            if (plr->psex == SEX_MALE)
                plr->psex = SEX_FEMALE;
            else
                plr->psex = SEX_MALE;
            break;
        }
    }
}

/************************************************************************
 * 2.1) Personality
 ***********************************************************************/ 
static vec_ptr _pers_choices(void);

static void _pers_ui(void)
{
    vec_ptr v = _pers_choices();
    for (;;)
    {
        int cmd, i, split = vec_length(v) + 1;
        doc_ptr cols[2];

        doc_clear(_doc);
        _race_class_top(_doc);

        cols[0] = doc_alloc(20);
        cols[1] = doc_alloc(20);

        if (split > 7)
            split = (split + 1)/2;
        for (i = 0; i < vec_length(v); i++)
        {
            personality_ptr pers_ptr = vec_get(v, i);
            doc_printf(
                cols[i < split ? 0 : 1],
                "  <color:y>%c</color>) <color:%c>%s</color>\n",
                I2A(i),
                pers_ptr->id == plr->personality ? 'B' : 'w',
                pers_ptr->name
            );
        }
        doc_insert(cols[vec_length(v) <= split ? 0 : 1], "  <color:y>*</color>) Random\n");

        doc_insert(_doc, "<color:G>Choose Your Personality</color>\n");
        doc_insert_cols(_doc, cols, 2, 1);
        doc_insert(_doc, "     Use SHIFT+choice to display help topic\n");

        doc_free(cols[0]);
        doc_free(cols[1]);

        _sync_term(_doc);

        cmd = _inkey();

        if (cmd == ESCAPE) break;
        else if (cmd == '\t') _inc_rcp_state();
        else if (cmd == '=') _birth_options();
        else if (cmd == '?') doc_display_help("Personalities.txt", NULL);
        else if (isupper(cmd))
        {
            i = A2I(tolower(cmd));
            if (0 <= i && i < vec_length(v))
            {
                personality_ptr pers_ptr = vec_get(v, i);
                doc_display_help("Personalities.txt", pers_ptr->name);
            }
        }
        else
        {
            if (cmd == '*') i = randint0(vec_length(v));
            else i = A2I(cmd);
            if (0 <= i && i < vec_length(v))
            {
                personality_ptr pers_ptr = vec_get(v, i);
                plr->personality = pers_ptr->id;
                break;
            }
        }
    }
    vec_free(v);
}

static int _pers_cmp(personality_ptr l, personality_ptr r)
{
    return strcmp(l->name, r->name);
}

static vec_ptr _pers_choices(void)
{
    vec_ptr v = vec_alloc(NULL);
    int i;
    for (i = 0; i < MAX_PERSONALITIES; i++)
    {
        personality_ptr pers_ptr = get_personality_aux(i);
        if (pers_ptr->flags & DEPRECATED) continue;
        vec_add(v, pers_ptr);
    }
    vec_sort(v, (vec_cmp_f)_pers_cmp);
    return v;
}

/************************************************************************
 * 2.2) Race
 ***********************************************************************/ 
static vec_ptr _get_races_aux(int ids[]);
static bool _is_valid_race_class(int race_id, int class_id);

#define _MAX_RACES_PER_GROUP 23
#define _MAX_RACE_GROUPS      8
typedef struct _race_group_s {
    cptr name;
    int ids[_MAX_RACES_PER_GROUP];
} _race_group_t, *_race_group_ptr;
static _race_group_t _race_groups[_MAX_RACE_GROUPS] = {
    { "Human",
        {RACE_AMBERITE, RACE_BARBARIAN, RACE_DEMIGOD, RACE_DUNADAN, RACE_HUMAN, -1} },
    { "Elf",
        {RACE_DARK_ELF, RACE_DRIDER, RACE_HIGH_ELF, RACE_WATER_ELF, RACE_WOOD_ELF, -1} },
    { "Hobbit/Dwarf",
        {RACE_DWARF, RACE_GNOME, RACE_HOBBIT, RACE_NIBELUNG, -1} },
    { "Fairy",
        {RACE_SHADOW_FAIRY, RACE_SPRITE, -1} },
    { "Angel/Demon",
        {RACE_ARCHON, RACE_BALROG, RACE_IMP, RACE_TENGU, -1} },
    { "Orc/Troll/Giant",
        {RACE_CYCLOPS, RACE_HALF_GIANT, RACE_HALF_OGRE,
         RACE_HALF_TITAN, RACE_HALF_TROLL, RACE_KOBOLD, RACE_SNOTLING, -1} },
    { "Undead",
        {RACE_SKELETON, RACE_SPECTRE, RACE_VAMPIRE, RACE_ZOMBIE, -1} },
    { "Other",
        {RACE_ANDROID, RACE_BEASTMAN, RACE_CENTAUR, RACE_DRACONIAN, RACE_DOPPELGANGER,
         RACE_ENT, RACE_GOLEM, RACE_KLACKON, RACE_KUTAR, RACE_MIND_FLAYER, RACE_YEEK,-1 } },
};

static void _race_group_ui(void)
{
    vec_ptr groups = vec_alloc(NULL);
    int     i;

    for (i = 0; i < _MAX_RACE_GROUPS; i++)
    {
        _race_group_ptr g_ptr = &_race_groups[i];
        vec_ptr         races = _get_races_aux(g_ptr->ids);

        if (vec_length(races))
            vec_add(groups, g_ptr);
        vec_free(races);
    }

    for (;;)
    {
        int cmd;

        doc_clear(_doc);
        _race_class_top(_doc);

        doc_insert(_doc, "<color:G>Choose a Type of Race to Play</color>\n");
        for (i = 0; i < vec_length(groups); i++)
        {
            _race_group_ptr g_ptr = vec_get(groups, i);
            doc_printf(_doc, "  <color:y>%c</color>) %s\n", I2A(i), g_ptr->name);
        }
        doc_insert(_doc, "  <color:y>*</color>) Random\n");
        _sync_term(_doc);

        cmd = _inkey();

        if (cmd == ESCAPE) break;
        else if (cmd == '\t') _inc_rcp_state();
        else if (cmd == '=') _birth_options();
        else if (cmd == '?') doc_display_help("Races.txt", NULL);
        else
        {
            if (cmd == '*') i = randint0(vec_length(groups));
            else i = A2I(cmd);
            if (0 <= i && i < vec_length(groups))
            {
                _race_group_ptr g_ptr = vec_get(groups, i);
                int old_id = plr->prace;
                int old_subid = plr->psubrace;
                if (_race_ui(g_ptr->ids) == UI_OK) break;
                plr->prace = old_id;
                plr->psubrace = old_subid;
            }
        }
    }
    vec_free(groups);
}

static int _race_ui(int ids[])
{
    vec_ptr v = _get_races_aux(ids);
    int     result = UI_NONE;

    while (result == UI_NONE)
    {
        int cmd, i, split = vec_length(v) + 1;
        doc_ptr cols[2];

        doc_clear(_doc);
        _race_class_top(_doc);

        cols[0] = doc_alloc(30);
        cols[1] = doc_alloc(30);

        if (split > 7)
            split = (split + 1)/2;
        for (i = 0; i < vec_length(v); i++)
        {
            race_t *race_ptr = vec_get(v, i);
            doc_printf(
                cols[i < split ? 0 : 1],
                "  <color:y>%c</color>) <color:%c>%s</color>\n",
                I2A(i),
                race_ptr->id == plr->prace ? 'B' : 'w',
                race_ptr->name
            );
        }
        doc_insert(cols[vec_length(v) < split ? 0 : 1], "  <color:y>*</color>) Random\n");

        doc_insert(_doc, "<color:G>Choose Your Race</color>\n");
        doc_insert_cols(_doc, cols, 2, 1);
        doc_insert(_doc, "     Use SHIFT+choice to display help topic\n");

        doc_free(cols[0]);
        doc_free(cols[1]);

        _sync_term(_doc);

        cmd = _inkey();

        if (cmd == ESCAPE) result = UI_CANCEL;
        else if (cmd == '\t') _inc_rcp_state();
        else if (cmd == '=') _birth_options();
        else if (cmd == '?') doc_display_help("Races.txt", NULL);
        else if (isupper(cmd))
        {
            i = A2I(tolower(cmd));
            if (0 <= i && i < vec_length(v))
            {
                race_t *race_ptr = vec_get(v, i);
                doc_display_help("Races.txt", race_ptr->name);
            }
        }
        else
        {
            if (cmd == '*') i = randint0(vec_length(v));
            else i = A2I(cmd);
            if (0 <= i && i < vec_length(v))
            {
                race_t *race_ptr = vec_get(v, i);
                int old_id = plr->prace;
                int old_subid = plr->psubrace;

                if (plr->prace != race_ptr->id)
                {
                    plr->prace = race_ptr->id;
                    plr->psubrace = 0;
                }
                result = _subrace_ui();
                if (result == UI_CANCEL)
                {
                    plr->prace = old_id;
                    plr->psubrace = old_subid;
                    result = UI_NONE;
                }
            }
        }
    }
    vec_free(v);
    return result;
}

static vec_ptr _get_races_aux(int ids[])
{
    vec_ptr v = vec_alloc(NULL);
    int     i;

    for (i = 0; ; i++)
    {
        int id = ids[i];
        if (id == -1) break;
        if (!_is_valid_race_class(id, plr->pclass)) continue;
        vec_add(v, get_race_aux(id, 0));
    }

    return v;
}

static bool _is_valid_race_class(int race_id, int class_id)
{
    if (class_id == CLASS_BLOOD_KNIGHT)
    {
        if (get_race_aux(race_id, 0)->flags & RACE_IS_NONLIVING)
            return FALSE;
    }
    if (class_id == CLASS_BEASTMASTER || class_id == CLASS_CAVALRY)
    {
        if (race_id == RACE_CENTAUR || race_id == RACE_DRIDER)
            return FALSE;
    }
    return TRUE;
}

/************************************************************************
 * 2.2.1) Subrace
 ***********************************************************************/ 
static int _subrace_ui(void)
{
    if (plr->prace == RACE_DEMIGOD)
        return _demigod_ui();
    else if (plr->prace == RACE_DRACONIAN)
        return _draconian_ui();
    else
    {
        plr->psubrace = 0;
        return UI_OK;
    }
}

static int _subrace_ui_aux(int ct, cptr desc, cptr help, cptr topic)
{
    for (;;)
    {
        int cmd, i, split = ct + 1;
        doc_ptr cols[2];

        cols[0] = doc_alloc(30);
        cols[1] = doc_alloc(30);

        doc_clear(_doc);
        _race_class_top(_doc);

        if (split > 7)
            split = (split + 1)/2;

        for (i = 0; i < ct; i++)
        {
            race_t *race_ptr = get_race_aux(plr->prace, i);
            doc_printf(
                i < split ? cols[0] : cols[1],
                "  <color:y>%c</color>) <color:%c>%s</color>\n",
                I2A(i),
                i == plr->psubrace ? 'B' : 'w',
                race_ptr->subname);
        }
        doc_insert(ct < split ? cols[0] : cols[1], "  <color:y>*</color>) Random\n");

        doc_printf(_doc, "<color:G>Choose %s</color>\n", desc);
        doc_insert_cols(_doc, cols, 2, 1);
        doc_insert(_doc, "     Use SHIFT+choice to display help topic\n");

        doc_free(cols[0]);
        doc_free(cols[1]);

        _sync_term(_doc);
        cmd = _inkey();
        if (cmd == ESCAPE) return UI_CANCEL;
        else if (cmd == '\t') _inc_rcp_state();
        else if (cmd == '=') _birth_options();
        else if (cmd == '?') doc_display_help(help, topic);
        else if (isupper(cmd) && !topic)
        {
            i = A2I(tolower(cmd));
            if (0 <= i && i < ct)
            {
                race_t *race_ptr = get_race_aux(plr->prace, i);
                doc_display_help(help, race_ptr->subname);
            }
        }
        else
        {
            if (cmd == '*') i = randint0(ct);
            else i = A2I(cmd);
            if (0 <= i && i < ct)
            {
                plr->psubrace = i;
                return UI_OK;
            }
        }
    }
}

static int _demigod_ui(void)
{
    assert(plr->prace == RACE_DEMIGOD);
    return _subrace_ui_aux(DEMIGOD_MAX, "Demigod Parentage", "Demigods.txt", NULL);
}

static int _draconian_ui(void)
{
    assert(plr->prace == RACE_DRACONIAN);
    return _subrace_ui_aux(DRACONIAN_MAX, "Draconian Subrace", "Draconians.txt", NULL);
}

/************************************************************************
 * 2.3) Class
 ***********************************************************************/ 
static vec_ptr _get_classes_aux(int ids[])
{
    vec_ptr v = vec_alloc(NULL);
    int     i;

    for (i = 0; ; i++)
    {
        int id = ids[i];
        if (id == -1) break;
        if (!_is_valid_race_class(plr->prace, id)) continue;
        vec_add(v, get_class_aux(id, 0));
    }

    return v;
}

#define _MAX_CLASSES_PER_GROUP 20
#define _MAX_CLASS_GROUPS      11
typedef struct _class_group_s {
    cptr name;
    int ids[_MAX_CLASSES_PER_GROUP];
} _class_group_t, *_class_group_ptr;
static _class_group_t _class_groups[_MAX_CLASS_GROUPS] = {
    { "Melee", {CLASS_BLOOD_KNIGHT, CLASS_DUELIST, CLASS_MAULER,
                    CLASS_RUNE_KNIGHT, CLASS_SAMURAI, CLASS_WARRIOR, CLASS_WEAPONMASTER,
                    CLASS_WEAPONSMITH, -1} },
    { "Archery", {CLASS_ARCHER, CLASS_SNIPER, -1} },
    { "Martial Arts", {CLASS_FORCETRAINER, CLASS_MONK, CLASS_MYSTIC, -1} },
    { "Magic", {CLASS_BLUE_MAGE, CLASS_GRAY_MAGE, CLASS_HIGH_MAGE, CLASS_MAGE,
                    CLASS_NECROMANCER, CLASS_SORCERER, CLASS_YELLOW_MAGE, -1} },
    { "Devices", {CLASS_DEVICEMASTER, CLASS_MAGIC_EATER, -1} },
    { "Prayer", {CLASS_PRIEST, CLASS_HIGH_PRIEST, -1} },
    { "Stealth", {CLASS_NINJA, CLASS_ROGUE, CLASS_SCOUT, -1} },
    { "Hybrid", {CLASS_CHAOS_WARRIOR, CLASS_PALADIN, CLASS_RANGER, CLASS_RED_MAGE,
                    CLASS_WARRIOR_MAGE, -1} },
    { "Riding", {CLASS_BEASTMASTER, CLASS_CAVALRY, -1} },
    { "Mind", {CLASS_MINDCRAFTER, CLASS_MIRROR_MASTER, CLASS_PSION,
                    CLASS_TIME_LORD, CLASS_WARLOCK, -1} },
    { "Other", {CLASS_ARCHAEOLOGIST, CLASS_BARD, CLASS_RAGE_MAGE,
                    CLASS_SKILLMASTER, CLASS_WILD_TALENT, -1} },
};

static void _class_group_ui(void)
{
    vec_ptr groups = vec_alloc(NULL);
    int     i;

    for (i = 0; i < _MAX_CLASS_GROUPS; i++)
    {
        _class_group_ptr g_ptr = &_class_groups[i];
        vec_ptr          classes = _get_classes_aux(g_ptr->ids);

        if (vec_length(classes))
            vec_add(groups, g_ptr);
        vec_free(classes);
    }

    for (;;)
    {
        int cmd, split = vec_length(groups) + 1;
        doc_ptr cols[2];

        cols[0] = doc_alloc(30);
        cols[1] = doc_alloc(30);

        doc_clear(_doc);
        _race_class_top(_doc);

        if (split > 7)
            split = (split + 1)/2;

        for (i = 0; i < vec_length(groups); i++)
        {
            _class_group_ptr g_ptr = vec_get(groups, i);
            doc_printf(
                i < split ? cols[0] : cols[1],
                "  <color:y>%c</color>) %s\n", I2A(i), g_ptr->name);
        }
        doc_insert(cols[vec_length(groups) <= split ? 0 : 1], "  <color:y>*</color>) Random\n");

        doc_insert(_doc, "<color:G>Choose a Type of Class to Play</color>\n");
        doc_insert_cols(_doc, cols, 2, 1);

        doc_free(cols[0]);
        doc_free(cols[1]);

        _sync_term(_doc);

        cmd = _inkey();

        if (cmd == ESCAPE) break;
        else if (cmd == '\t') _inc_rcp_state();
        else if (cmd == '=') _birth_options();
        else if (cmd == '?') doc_display_help("Classes.txt", NULL);
        else
        {
            if (cmd == '*') i = randint0(vec_length(groups));
            else i = A2I(cmd);
            if (0 <= i && i < vec_length(groups))
            {
                _class_group_ptr g_ptr = vec_get(groups, i);
                if (_class_ui(g_ptr->ids) == UI_OK)
                    break;
            }
        }
    }
    vec_free(groups);
}

static int _class_ui(int ids[])
{
    vec_ptr v = _get_classes_aux(ids);
    int     result = UI_NONE;

    while (result == UI_NONE)
    {
        int cmd, i, split = vec_length(v) + 1;
        doc_ptr cols[2];

        doc_clear(_doc);
        _race_class_top(_doc);

        cols[0] = doc_alloc(30);
        cols[1] = doc_alloc(30);

        if (split > 7)
            split = (split + 1)/2;
        for (i = 0; i < vec_length(v); i++)
        {
            class_t *class_ptr = vec_get(v, i);
            doc_printf(
                cols[i < split ? 0 : 1],
                "  <color:y>%c</color>) <color:%c>%s</color>\n",
                I2A(i),
                class_ptr->id == plr->pclass ? 'B' : 'w',
                class_ptr->name
            );
        }
        doc_insert(cols[vec_length(v) <= split ? 0 : 1], "  <color:y>*</color>) Random\n");

        doc_insert(_doc, "<color:G>Choose Your Class</color>\n");
        doc_insert_cols(_doc, cols, 2, 1);
        doc_insert(_doc, "     Use SHIFT+choice to display help topic\n");

        doc_free(cols[0]);
        doc_free(cols[1]);

        _sync_term(_doc);

        cmd = _inkey();

        if (cmd == ESCAPE) result = UI_CANCEL;
        else if (cmd == '\t') _inc_rcp_state();
        else if (cmd == '=') _birth_options();
        else if (cmd == '?') doc_display_help("Classes.txt", NULL);
        else if (isupper(cmd))
        {
            i = A2I(tolower(cmd));
            if (0 <= i && i < vec_length(v))
            {
                class_t *class_ptr = vec_get(v, i);
                doc_display_help("Classes.txt", class_ptr->name);
            }
        }
        else
        {
            if (cmd == '*') i = randint0(vec_length(v));
            else i = A2I(cmd);
            if (0 <= i && i < vec_length(v))
            {
                class_t *class_ptr = vec_get(v, i);
                int      old_id = plr->pclass;

                plr->pclass = class_ptr->id;
                result = _subclass_ui();
                if (result == UI_CANCEL)
                {
                    plr->pclass = old_id;
                    result = UI_NONE;
                }
            }
        }
    }
    vec_free(v);
    return result;
}

/************************************************************************
 * 2.3.1) Subclass
 ***********************************************************************/ 
static int _subclass_ui(void)
{
    for (;;)
    {
        int rc;
        bool has_subclass = TRUE;

        /* Prompt for a subclass */
        if (plr->pclass == CLASS_WARLOCK)
            rc = _warlock_ui();
        else if (plr->pclass == CLASS_WEAPONMASTER)
            rc = _weaponmaster_ui();
        else if (plr->pclass == CLASS_DEVICEMASTER)
            rc = _devicemaster_ui();
        else if (plr->pclass == CLASS_GRAY_MAGE)
            rc = _gray_mage_ui();
        else
        {
            plr->psubclass = 0;
            rc = UI_OK;
            has_subclass = FALSE;
        }
        /* Cancel subclass returns to _class_ui */
        if (rc == UI_CANCEL) return UI_CANCEL;

        /* Prompt for magic */
        rc =_realm1_ui();

        /* Cancel magic stays here if there is a sublass ui */
        if (rc == UI_OK || !has_subclass) return rc;
    }
}

static int _warlock_ui(void)
{
    assert(plr->pclass == CLASS_WARLOCK);
    for (;;)
    {
        int cmd, i;

        doc_clear(_doc);
        _race_class_top(_doc);

        doc_insert(_doc, "<color:G>Choose Warlock Pact</color>\n");
        for (i = 0; i < WARLOCK_MAX; i++)
        {
            class_t *class_ptr = get_class_aux(plr->pclass, i);
            doc_printf(_doc, "  <color:y>%c</color>) <color:%c>%s</color>\n",
                I2A(i),
                plr->psubclass == i ? 'B' : 'w',
                class_ptr->subname
            );
        }
        doc_insert(_doc, "  <color:y>*</color>) Random\n");
        doc_insert(_doc, "     Use SHIFT+choice to display help topic\n");

        _sync_term(_doc);
        cmd = _inkey();
        if (cmd == ESCAPE) return UI_CANCEL;
        else if (cmd == '\t') _inc_rcp_state();
        else if (cmd == '=') _birth_options();
        else if (cmd == '?') doc_display_help("Warlocks.txt", NULL);
        else if (isupper(cmd))
        {
            i = A2I(tolower(cmd));
            if (0 <= i && i < WARLOCK_MAX)
            {
                class_t *class_ptr = get_class_aux(plr->pclass, i);
                doc_display_help("Warlocks.txt", class_ptr->subname);
            }
        }
        else
        {
            if (cmd == '*') i = randint0(WARLOCK_MAX);
            else i = A2I(cmd);
            if (0 <= i && i < WARLOCK_MAX)
            {
                plr->psubclass = i;
                return UI_OK;
            }
        }
    }
}

static int _weaponmaster_ui(void)
{
    assert(plr->pclass == CLASS_WEAPONMASTER);
    for (;;)
    {
        int cmd, i;

        doc_clear(_doc);
        _race_class_top(_doc);

        doc_insert(_doc, "<color:G>Choose Speciality</color>\n");
        for (i = 0; i < WEAPONMASTER_MAX; i++)
        {
            class_t *class_ptr = get_class_aux(plr->pclass, i);
            doc_printf(_doc, "  <color:y>%c</color>) <color:%c>%s</color>\n",
                I2A(i),
                plr->psubclass == i ? 'B' : 'w',
                class_ptr->subname
            );
        }
        doc_insert(_doc, "  <color:y>*</color>) Random\n");
        doc_insert(_doc, "     Use SHIFT+choice to display help topic\n");

        _sync_term(_doc);
        cmd = _inkey();
        if (cmd == ESCAPE) return UI_CANCEL;
        else if (cmd == '\t') _inc_rcp_state();
        else if (cmd == '=') _birth_options();
        else if (cmd == '?') doc_display_help("Weaponmasters.txt", NULL);
        else if (isupper(cmd))
        {
            i = A2I(tolower(cmd));
            if (0 <= i && i < WEAPONMASTER_MAX)
            {
                class_t *class_ptr = get_class_aux(plr->pclass, i);
                doc_display_help("Weaponmasters.txt", class_ptr->subname);
            }
        }
        else
        {
            if (cmd == '*') i = randint0(WEAPONMASTER_MAX);
            else i = A2I(cmd);
            if (0 <= i && i < WEAPONMASTER_MAX)
            {
                plr->psubclass = i;
                return UI_OK;
            }
        }
    }
}

static int _devicemaster_ui(void)
{
    assert(plr->pclass == CLASS_DEVICEMASTER);
    for (;;)
    {
        int cmd, i;

        doc_clear(_doc);
        _race_class_top(_doc);

        doc_insert(_doc, "<color:G>Choose Speciality</color>\n");
        for (i = 0; i < DEVICEMASTER_MAX; i++)
        {
            class_t *class_ptr = get_class_aux(plr->pclass, i);
            doc_printf(_doc, "  <color:y>%c</color>) <color:%c>%s</color>\n",
                I2A(i),
                plr->psubclass == i ? 'B' : 'w',
                class_ptr->subname
            );
        }
        doc_insert(_doc, "  <color:y>*</color>) Random\n");

        _sync_term(_doc);
        cmd = _inkey();
        if (cmd == ESCAPE) return UI_CANCEL;
        else if (cmd == '\t') _inc_rcp_state();
        else if (cmd == '=') _birth_options();
        else if (cmd == '?') doc_display_help("Classes.txt", "Devicemaster");
        else
        {
            if (cmd == '*') i = randint0(DEVICEMASTER_MAX);
            else i = A2I(cmd);
            if (0 <= i && i < DEVICEMASTER_MAX)
            {
                plr->psubclass = i;
                return UI_OK;
            }
        }
    }
}

static int _gray_mage_ui(void)
{
    assert(plr->pclass == CLASS_GRAY_MAGE);
    for (;;)
    {
        int cmd, i;

        doc_clear(_doc);
        _race_class_top(_doc);

        doc_insert(_doc, "<color:G>Choose Bias</color>\n");
        for (i = 0; i < GRAY_MAGE_MAX; i++)
        {
            class_t *class_ptr = get_class_aux(plr->pclass, i);
            doc_printf(_doc, "  <color:y>%c</color>) <color:%c>%s</color>\n",
                I2A(i),
                plr->psubclass == i ? 'B' : 'w',
                class_ptr->subname
            );
        }
        doc_insert(_doc, "  <color:y>*</color>) Random\n");

        _sync_term(_doc);
        cmd = _inkey();
        if (cmd == ESCAPE) return UI_CANCEL;
        else if (cmd == '\t') _inc_rcp_state();
        else if (cmd == '=') _birth_options();
        else if (cmd == '?') doc_display_help("Classes.txt", "Gray-Mage");
        else
        {
            if (cmd == '*') i = randint0(GRAY_MAGE_MAX);
            else i = A2I(cmd);
            if (0 <= i && i < GRAY_MAGE_MAX)
            {
                plr->psubclass = i;
                return UI_OK;
            }
        }
    }
}

/************************************************************************
 * 2.3.2) Magic
 ***********************************************************************/ 
static caster_info *_caster_info(void)
{
    class_t *class_ptr = get_class_aux(plr->pclass, plr->psubclass);
    if (class_ptr->hooks.caster_info)
        return class_ptr->hooks.caster_info();
    return NULL;
}
static u32b _realm1_bits(void)
{
    caster_info *info = _caster_info();
    if (!info) return 0;
    return info->realm1_choices;
}
static u32b _realm2_bits(void)
{
    caster_info *info = _caster_info();
    if (!info) return 0;
    return info->realm2_choices;
}
static int _realm1_ui(void)
{
    u32b bits = _realm1_bits();
    int  choices[MAX_REALM];
    int  ct = 0, i;

    if (!bits)
    {
        plr->realm1 = 0;
        plr->realm2 = 0;
        return UI_OK;
    }

    for (i = 0; i < 32; i++)
    {
        if (bits & (1L << i))
            choices[ct++] = i+1;
    }
    choices[ct] = -1;

    for (;;)
    {
        int cmd;

        doc_clear(_doc);
        _race_class_top(_doc);

        doc_insert(_doc, "<color:G>Choose Your Primary Magic Realm</color>\n");
        for (i = 0; i < ct; i++)
        {
            int id = choices[i];
            doc_printf(_doc, "  <color:y>%c</color>) <color:%c>%s</color>\n",
                I2A(i),
                id == plr->realm1 ? 'B' : 'w',
                realm_names[id]
            );
        }
        doc_insert(_doc, "  <color:y>*</color>) Random\n");
        doc_insert(_doc, "\n     Use SHIFT+choice to display help topic\n");

        _sync_term(_doc);
        cmd = _inkey();
        if (cmd == ESCAPE)
            return UI_CANCEL;
        else if (cmd == '=')
            _birth_options();
        else if (cmd == '?')
            doc_display_help("magic.txt", NULL);
        else if (isupper(cmd))
        {
            i = A2I(tolower(cmd));
            if (0 <= i && i < ct)
            {
                int id = choices[i];
                doc_display_help("magic.txt", realm_names[id]);
            }
        }
        else
        {
            if (cmd == '*') i = randint0(ct);
            else i = A2I(cmd);
            if (0 <= i && i < ct)
            {
                int id = choices[i];
                int old_id = plr->realm1;
                int rc;

                plr->realm1 = id;
                rc = _realm2_ui();
                if (rc == UI_CANCEL)
                    plr->realm1 = old_id;
                else
                    return UI_OK;
            }
        }
    }
}

static int _realm2_ui(void)
{
    u32b bits = _realm2_bits();
    int  choices[MAX_REALM];
    int  ct = 0, i;

    if (!bits)
    {
        plr->realm2 = 0;
        return UI_OK;
    }

    if (plr->pclass == CLASS_PRIEST)
    {
        if (plr->realm1 == REALM_NATURE)
            bits &= ~(CH_LIFE | CH_CRUSADE | CH_DEATH | CH_DAEMON);
        else if (is_good_realm(plr->realm1))
            bits &= ~(CH_DEATH | CH_DAEMON);
        else
            bits &= ~(CH_LIFE | CH_CRUSADE);
    }

    for (i = 0; i < 32; i++)
    {
        int id = i + 1;
        if (bits & (1L << i) && plr->realm1 != id)
            choices[ct++] = id;
    }
    choices[ct] = -1;

    for (;;)
    {
        int cmd;

        doc_clear(_doc);
        _race_class_top(_doc);

        doc_insert(_doc, "<color:G>Choose Your Secondary Magic Realm</color>\n");
        for (i = 0; i < ct; i++)
        {
            int id = choices[i];
            doc_printf(_doc, "  <color:y>%c</color>) <color:%c>%s</color>\n",
                I2A(i),
                id == plr->realm2 ? 'B' : 'w',
                realm_names[id]
            );
        }
        doc_insert(_doc, "  <color:y>*</color>) Random\n");
        doc_insert(_doc, "\n     Use SHIFT+choice to display help topic\n");

        _sync_term(_doc);
        cmd = _inkey();
        if (cmd == ESCAPE)
            return UI_CANCEL;
        else if (cmd == '=')
            _birth_options();
        else if (cmd == '?')
            doc_display_help("magic.txt", NULL);
        else if (isupper(cmd))
        {
            i = A2I(tolower(cmd));
            if (0 <= i && i < ct)
            {
                int id = choices[i];
                doc_display_help("magic.txt", realm_names[id]);
            }
        }
        else
        {
            if (cmd == '*') i = randint0(ct);
            else i = A2I(cmd);
            if (0 <= i && i < ct)
            {
                int id = choices[i];
                plr->realm2 = id;
                return UI_OK;
            }
        }
    }
}

/************************************************************************
 * 2.2') Monster Race (Replaces 2.2.* path when in monster mode)
 ***********************************************************************/ 

#define _MAX_MON_RACE_GROUPS      12
static _race_group_t _mon_race_groups[_MAX_MON_RACE_GROUPS] = {
    { "Animal",
        {/*RACE_MON_ANT, RACE_MON_BEETLE, RACE_MON_BIRD, RACE_MON_CAT,*/ RACE_MON_CENTIPEDE,
            RACE_MON_HOUND, /*RACE_MON_HORSE, */ RACE_MON_HYDRA, RACE_MON_SPIDER, -1} },
    { "Angel/Demon",
        {RACE_MON_ANGEL, RACE_MON_DEMON, -1} },
    { "Beholder",
        {RACE_MON_BEHOLDER, -1} },
    { "Dragon",
        {RACE_MON_DRAGON, -1} },
    { "Elemental/Vortex",
        {RACE_MON_ELEMENTAL, RACE_MON_VORTEX, -1} },
    { "Golem",
        {RACE_MON_GOLEM, -1} },
    { "Jelly",
        {RACE_MON_JELLY, /*RACE_MON_MOLD,*/ RACE_MON_QUYLTHULG, -1} },
    { "Leprechaun",
        {RACE_MON_LEPRECHAUN, -1} },
    { "Mimic/Possessor",
        {RACE_MON_SWORD, /*RACE_MON_ARMOR,*/ RACE_MON_MIMIC, RACE_MON_POSSESSOR, RACE_MON_RING, -1} },
    { "Orc/Troll/Giant",
        {RACE_MON_GIANT, /*RACE_MON_KOBOLD, RACE_MON_ORC,*/ RACE_MON_TROLL, -1} },
    { "Undead",
        {/*RACE_MON_GHOST,*/ RACE_MON_LICH, RACE_MON_VAMPIRE, /*RACE_MON_WRAITH, RACE_MON_ZOMBIE,*/ -1 } },
    { "Xorn",
        {RACE_MON_XORN, -1} },
};

static void _mon_race_group_ui(void)
{
    for (;;)
    {
        int cmd, i;

        doc_clear(_doc);
        _race_class_top(_doc);

        doc_insert(_doc, "<color:G>Choose a Type of Monster to Play</color>\n");
        for (i = 0; i < _MAX_MON_RACE_GROUPS; i++)
        {
            _race_group_ptr g_ptr = &_mon_race_groups[i];
            doc_printf( _doc, "  <color:y>%c</color>) %s\n", I2A(i), g_ptr->name);
        }
        doc_insert(_doc, "  <color:y>*</color>) Random\n");
        _sync_term(_doc);

        cmd = _inkey();

        if (cmd == ESCAPE) break;
        else if (cmd == '\t') _inc_rcp_state();
        else if (cmd == '=') _birth_options();
        else if (cmd == '?') doc_display_help("MonsterRaces.txt", NULL);
        else if (isupper(cmd))
        {
            i = A2I(cmd);
            if (0 <= i && i < _MAX_MON_RACE_GROUPS)
            {
                _race_group_ptr g_ptr = &_mon_race_groups[i];
                if (_count(g_ptr->ids) == 1)
                {
                    race_t *race_ptr = get_race_aux(plr->prace, 0);
                    doc_display_help("MonsterRaces.txt", race_ptr->name);
                }
                else
                    doc_display_help("MonsterRaces.txt", NULL);
            }
        }
        else
        {
            if (cmd == '*') i = randint0(_MAX_MON_RACE_GROUPS);
            else i = A2I(cmd);
            if (0 <= i && i < _MAX_MON_RACE_GROUPS)
            {
                _race_group_ptr g_ptr = &_mon_race_groups[i];
                int old_id = plr->prace;
                int old_subid = plr->psubrace;
                int old_realm = plr->dragon_realm;

                if (_count(g_ptr->ids) == 1)
                {
                    plr->prace = g_ptr->ids[0];
                    if (_mon_subrace_ui() == UI_OK)
                        break;
                }
                else if (_mon_race_ui(g_ptr->ids) == UI_OK)
                    break;

                plr->prace = old_id;
                plr->psubrace = old_subid;
                plr->dragon_realm = old_realm;
            }
        }
    }
    assert(!(plr->prace != RACE_MON_DRAGON && plr->dragon_realm != 0));
}

static int _mon_race_ui(int ids[])
{
    for (;;)
    {
        int cmd, i, ct = 0;

        doc_clear(_doc);
        _race_class_top(_doc);

        doc_insert(_doc, "<color:G>Choose Your Monster Race</color>\n");
        for (i = 0; ; i++)
        {
            int id = ids[i];
            race_t *race_ptr;

            if (id == -1) break;
            ct++;

            race_ptr = get_race_aux(id, 0);
            doc_printf(
                _doc,
                "  <color:y>%c</color>) <color:%c>%s</color>\n",
                I2A(i),
                race_ptr->id == plr->prace ? 'B' : 'w',
                race_ptr->name
            );
        }
        doc_insert(_doc, "  <color:y>*</color>) Random\n");
        doc_insert(_doc, "     Use SHIFT+choice to display help topic\n");

        _sync_term(_doc);

        cmd = _inkey();

        if (cmd == ESCAPE) return UI_CANCEL;
        else if (cmd == '\t') _inc_rcp_state();
        else if (cmd == '=') _birth_options();
        else if (cmd == '?') doc_display_help("MonsterRaces.txt", NULL);
        else if (isupper(cmd))
        {
            i = A2I(tolower(cmd));
            if (0 <= i && i < ct)
            {
                int     id = ids[i];
                race_t *race_ptr = get_race_aux(id, 0);
                doc_display_help("MonsterRaces.txt", race_ptr->name);
            }
        }
        else
        {
            if (cmd == '*') i = randint0(ct);
            else i = A2I(cmd);
            if (0 <= i && i < ct)
            {
                int id = ids[i];
                int old_id = plr->prace;
                int old_subid = plr->psubrace;
                int old_realm = plr->dragon_realm;

                if (plr->prace != id)
                {
                    plr->prace = id;
                    plr->psubrace = 0;
                    plr->dragon_realm = 0;
                }
                if (_mon_subrace_ui() == UI_OK) return UI_OK;
                plr->prace = old_id;
                plr->psubrace = old_subid;
                plr->dragon_realm = old_realm;
            }
        }
    }
}

static int _mon_subrace_ui(void)
{
    if (plr->prace != RACE_MON_DRAGON)
        plr->dragon_realm = 0;

    if (plr->prace == RACE_MON_DEMON)
        return _mon_demon_ui();
    else if (plr->prace == RACE_MON_DRAGON)
        return _mon_dragon_ui();
    else if (plr->prace == RACE_MON_ELEMENTAL)
        return _mon_elemental_ui();
    else if (plr->prace == RACE_MON_GIANT)
        return _mon_giant_ui();
    else if (plr->prace == RACE_MON_GOLEM)
        return _mon_golem_ui();
    else if (plr->prace == RACE_MON_LICH)
        return _mon_lich_ui();
    else if (plr->prace == RACE_MON_SPIDER)
        return _mon_spider_ui();
    else if (plr->prace == RACE_MON_TROLL)
        return _mon_troll_ui();
    else
    {
        plr->psubrace = 0;
        return UI_OK;
    }
}

static int _mon_demon_ui(void)
{
    assert(plr->prace == RACE_MON_DEMON);
    return _subrace_ui_aux(DEMON_MAX, "Demon Subrace", "Demons.txt", NULL);
}

static int _mon_lich_ui(void)
{
    assert(plr->prace == RACE_MON_LICH);
    return _subrace_ui_aux(LICH_MAX, "Lich Subrace", "MonsterRaces.txt", "Lich");
}

static int _mon_dragon_ui(void)
{
    int rc;
    assert(plr->prace == RACE_MON_DRAGON);
    rc = _subrace_ui_aux(DRAGON_MAX, "Dragon Subrace", "Dragons.txt", NULL);
    if (rc == UI_OK)
        rc = _dragon_realm_ui();
    return rc;
}

static int _dragon_realm_ui(void)
{
    vec_ptr v;
    int     i, rc = UI_NONE;

    if (plr->psubrace == DRAGON_STEEL)
    {
        plr->dragon_realm = DRAGON_REALM_NONE;
        return UI_OK;
    }

    v = vec_alloc(NULL);
    for (i = 1; i < DRAGON_REALM_MAX; i++)
    {
        dragon_realm_ptr realm = dragon_get_realm(i);
        if (i == DRAGON_REALM_CRUSADE && plr->psubrace != DRAGON_LAW && plr->psubrace != DRAGON_GOLD)
            continue;
        if (i == DRAGON_REALM_DEATH && plr->psubrace != DRAGON_NETHER && plr->psubrace != DRAGON_CHAOS)
            continue;
        vec_add(v, realm);
    }
    assert(vec_length(v));

    while (rc == UI_NONE)
    {
        int cmd, i;

        doc_clear(_doc);
        _race_class_top(_doc);

        doc_insert(_doc, "<color:G>Choose Your Realm</color>\n");
        for (i = 0; i < vec_length(v); i++)
        {
            dragon_realm_ptr realm = vec_get(v, i);
            doc_printf(
                _doc,
                "  <color:y>%c</color>) <color:%c>%s</color>\n",
                I2A(i),
                realm->id == plr->dragon_realm ? 'B' : 'w',
                realm->name
            );
        }
        doc_insert(_doc, "  <color:y>*</color>) Random\n");
        doc_insert(_doc, "     Use SHIFT+choice to display help topic\n");

        _sync_term(_doc);

        cmd = _inkey();

        if (cmd == ESCAPE) rc = UI_CANCEL;
        else if (cmd == '\t') _inc_rcp_state();
        else if (cmd == '=') _birth_options();
        else if (cmd == '?') doc_display_help("DragonRealms.txt", NULL);
        else if (isupper(cmd))
        {
            i = A2I(tolower(cmd));
            if (0 <= i && i < vec_length(v))
            {
                dragon_realm_ptr realm = vec_get(v, i);
                doc_display_help("DragonRealms.txt", realm->name);
            }
        }
        else
        {
            if (cmd == '*') i = randint0(vec_length(v));
            else i = A2I(cmd);
            if (0 <= i && i < vec_length(v))
            {
                dragon_realm_ptr realm = vec_get(v, i);
                plr->dragon_realm = realm->id;
                rc = UI_OK;
            }
        }
    }

    vec_free(v);
    return rc;
}

static int _mon_elemental_ui(void)
{
    assert(plr->prace == RACE_MON_ELEMENTAL);
    return _subrace_ui_aux(ELEMENTAL_MAX, "Elemental Subrace", "MonsterRaces.txt", "Elemental");
}

static int _mon_giant_ui(void)
{
    assert(plr->prace == RACE_MON_GIANT);
    return _subrace_ui_aux(GIANT_MAX, "Giant Subrace", "MonsterRaces.txt", "Giant");
}

static int _mon_golem_ui(void)
{
    assert(plr->prace == RACE_MON_GOLEM);
    return _subrace_ui_aux(GOLEM_MAX, "Golem Subrace", "MonsterRaces.txt", "Golem");
}

static int _mon_spider_ui(void)
{
    assert(plr->prace == RACE_MON_SPIDER);
    return _subrace_ui_aux(SPIDER_MAX, "Spider Subrace", "MonsterRaces.txt", "Spider");
}

static int _mon_troll_ui(void)
{
    assert(plr->prace == RACE_MON_TROLL);
    return _subrace_ui_aux(TROLL_MAX, "Troll Subrace", "MonsterRaces.txt", "Troll");
}


/************************************************************************
 * 3) Stats
 ***********************************************************************/ 

static int _stat_points[18] =
{ 0, 0, 0,
 -8,-7,-6,   /*  3  4  5 */
 -5,-4,-3,   /*  6  7  8 */
 -2,-1, 0,   /*  9 10 11 */
  1, 2, 3,   /* 12 13 14 */
  6,10,16 }; /* 15 16 17 */

#define _MAX_SCORE 30

static int _stats_score(void);
static void _stat_dec(int which);
static void _stat_inc(int which);
static cptr _stat_desc(int stat);

static cptr _stat_names[MAX_STATS] = { "STR", "INT", "WIS", "DEX", "CON", "CHR" };
static char _stat_to_char(int which);
static int _char_to_stat(char which);
static void _stats_add(s16b stats[MAX_STATS], s16b to_add[MAX_STATS]);

static int _stats_ui(void)
{
    race_t         *race_ptr = get_race();
    class_t        *class_ptr = get_class();
    personality_ptr pers_ptr = get_personality();
    dragon_realm_ptr realm_ptr = dragon_get_realm(plr->dragon_realm); /* 0 is OK */
    s16b stats[MAX_STATS] = {0};

    _stats_add(stats, race_ptr->stats);
    _stats_add(stats, class_ptr->stats);
    _stats_add(stats, pers_ptr->stats);
    if (plr->dragon_realm)
        _stats_add(stats, realm_ptr->stats);

    /* Initialize stats with reasonable defaults.
       If the user backs up and changes their class,
       pick new defaults (unless they manually made
       changes. */
    if (!_stats_changed)
        _stats_init();

    for (;;)
    {
        int cmd, score, i;
        doc_ptr cols[2];

        cols[0] = doc_alloc(32);
        cols[1] = doc_alloc(30);

        doc_clear(_doc);
        _race_class_top(_doc);

        score = _stats_score();
        doc_insert(cols[0], "<color:G>Enter Your Starting Stats</color>\n");
        doc_insert(cols[0], "<tab:9>Base  Pts  Mod  Total\n");
        for (i = 0; i < MAX_STATS; i++)
        {
            int stat = plr->stat_cur[i];
            int bonus = stats[i];
            doc_printf(cols[0], "<color:y>%c</color>/<color:y>%c</color>) %-3.3s ",
                _stat_to_char(i),
                toupper(_stat_to_char(i)),
                _stat_names[i]
            );
            doc_printf(cols[0], "%4d  <color:w>%3d</color>  <color:R>%+3d</color> %6.6s\n",
                stat,
                _stat_points[stat],
                bonus,
                _stat_desc(stat + bonus)
            );
        }
        doc_printf(cols[0], "<tab:15><color:%c>%3d</color>\n",
            score <= _MAX_SCORE ? 'G' : 'r', score);

        doc_newline(cols[1]);
        doc_newline(cols[1]);
        doc_insert(cols[1], "<color:y>  n</color>) Change Name\n");
        doc_insert(cols[1], "<color:y>  ?</color>) Help\n");
        if (game_mode != GAME_MODE_BEGINNER)
            doc_insert(cols[1], "<color:y>  =</color>) Options\n");
        doc_insert(cols[1], "<color:y>TAB</color>) More Info\n");
        doc_printf(cols[1], "<color:%c>RET</color>) <color:%c>Begin Play</color>\n",
            score <= _MAX_SCORE ? 'y' : 'D',
            score <= _MAX_SCORE ? 'v' : 'D');
        doc_insert(cols[1], "<color:y>ESC</color>) Prev Screen\n");

        doc_insert_cols(_doc, cols, 2, 1);
        doc_free(cols[0]);
        doc_free(cols[1]);

        doc_insert(_doc,
            "<color:G>Note: </color><indent>"
            "You may adjust each starting stat using the indicated "
            "keys. The lower case key decrements while the upper case increments "
            "the starting value. Values may range from 8 to 17 and each value that "
            "you enter is charged the indicated number of points. You only have 30 "
            "points to spend so be sure to adjust those stats that matter most."
            "</indent>");

        _sync_term(_doc);
        cmd = _inkey();
        if (cmd == '\r' && score <= _MAX_SCORE)
        {
            _birth_finalize();
            return UI_OK;
        }
        else if (cmd == ESCAPE)
            return UI_CANCEL;
        else if (cmd == '?')
            doc_display_help("birth.txt", "Stats");
        else if (cmd == '\t')
            _inc_rcp_state();
        else if (cmd == '=')
            _birth_options();
        else if (cmd == 'n')
            _change_name();
        else if (cmd == 'N')
            doc_display_help("birth.txt", "CharacterName");
        else if (isupper(cmd))
        {
            i = _char_to_stat(tolower(cmd));
            if (i != A_NONE)
                _stat_inc(i);
        }
        else
        {
            i = _char_to_stat(cmd);
            if (i != A_NONE)
                _stat_dec(i);
        }
    }
}

static char _stat_cmd_map[MAX_STATS] = { 's', 'i', 'w', 'd', 'c', 'r' };
static char _stat_to_char(int which)
{
    return _stat_cmd_map[which];
}

static int _char_to_stat(char which)
{
    int i;
    for (i = 0; i < MAX_STATS; i++)
    {
        char c = _stat_cmd_map[i];
        if (c == which) return i;
    }
    return A_NONE;
}

static cptr _stat_desc(int stat)
{
    static char buf[10];
    if (stat < 3) stat = 3;
    if (stat > 40) stat = 40;
    if (stat < 19)
        sprintf(buf, "%2d", stat);
    else
        sprintf(buf, "18/%d", 10*(stat - 18));
    return buf;
}

static void _stats_init_aux(int stats[MAX_STATS])
{
    int i;
    for (i = 0; i < MAX_STATS; i++)
    {
        plr->stat_cur[i] = stats[i];
        plr->stat_max[i] = stats[i];
    }
}

static void _stats_init(void)
{
    if (plr->pclass == CLASS_MONSTER)
    {
        switch (plr->prace)
        {
        case RACE_MON_JELLY:
        case RACE_MON_XORN:
        case RACE_MON_HOUND:
        case RACE_MON_GIANT:
        case RACE_MON_VORTEX:
        case RACE_MON_CENTIPEDE:
        case RACE_MON_TROLL:
        case RACE_MON_HYDRA:
        case RACE_MON_SPIDER:
        case RACE_MON_LEPRECHAUN:
        case RACE_MON_ELEMENTAL:
        case RACE_MON_SWORD:
        case RACE_MON_GOLEM:
        {
            int stats[6] = { 17, 8, 8, 17, 15, 9 };
            _stats_init_aux(stats);
            break;
        }
        case RACE_MON_ANGEL:
        {
            int stats[6] = { 16, 8, 17, 16, 11, 8 };
            _stats_init_aux(stats);
            break;
        }
        case RACE_MON_LICH:
            if (plr->psubrace == LICH_MONASTIC)
            {
                int stats[6] = { 16, 17, 8, 16, 11, 8 };
                _stats_init_aux(stats);
                break;
            }
            /* vvvvv Fall Thru vvvv */
        case RACE_MON_BEHOLDER:
        case RACE_MON_RING:
        {
            int stats[6] = { 16, 17, 9, 9, 16, 9 };
            _stats_init_aux(stats);
            break;
        }
        case RACE_MON_VAMPIRE:
        {
            int stats[6] = { 16, 8, 8, 17, 11, 16 };
            _stats_init_aux(stats);
            break;
        }
        case RACE_MON_MIMIC:
        case RACE_MON_POSSESSOR:
        {
            int stats[6] = { 15, 15, 11, 15, 15, 15 };
            _stats_init_aux(stats);
            break;
        }
        case RACE_MON_QUYLTHULG:
        {
            int stats[6] = { 16, 8, 8, 11, 16, 17 };
            _stats_init_aux(stats);
            break;
        }
        case RACE_MON_DRAGON:
            if (plr->dragon_realm == DRAGON_REALM_LORE)
            {
                int stats[6] = { 16, 16, 9, 16, 14, 10 };
                _stats_init_aux(stats);
            }
            else if (plr->dragon_realm == DRAGON_REALM_BREATH)
            {
                int stats[6] = { 16, 8, 8, 16, 17, 11 };
                _stats_init_aux(stats);
            }
            else if (plr->dragon_realm == DRAGON_REALM_CRAFT)
            {
                int stats[6] = { 16, 9, 16, 16, 14, 10 };
                _stats_init_aux(stats);
            }
            else if ( plr->dragon_realm == DRAGON_REALM_DOMINATION
                   || plr->dragon_realm == DRAGON_REALM_CRUSADE )
            {
                int stats[6] = { 16, 8, 8, 16, 15, 16 };
                _stats_init_aux(stats);
            }
            else
            {
                int stats[6] = { 17, 8, 8, 17, 15, 9 };
                _stats_init_aux(stats);
            }
            break;
        case RACE_MON_DEMON:
            if (plr->psubrace == DEMON_BALROG || plr->psubrace == DEMON_MARILITH)
            {
                int stats[6] = { 16, 16, 9, 16, 14, 10 };
                _stats_init_aux(stats);
            }
            else if (plr->psubrace == DEMON_CYBERDEMON)
            {
                int stats[6] = { 17, 8, 8, 15, 17, 9 };
                _stats_init_aux(stats);
            }
            else
            {
                int stats[6] = { 17, 8, 8, 17, 15, 9 };
                _stats_init_aux(stats);
            }
            break;
        }
    }
    else
    {
        switch (plr->pclass)
        {
        case CLASS_WARRIOR:
        case CLASS_CAVALRY:
        case CLASS_MAULER:
        case CLASS_ARCHER:
        case CLASS_WEAPONSMITH:
        case CLASS_WEAPONMASTER:
        case CLASS_NINJA:
        case CLASS_SNIPER:
        case CLASS_DUELIST:
        case CLASS_RAGE_MAGE:
        {
            int stats[6] = { 17, 8, 8, 17, 15, 9 };
            _stats_init_aux(stats);
            break;
        }
        case CLASS_BLOOD_KNIGHT:
        {
            int stats[6] = { 17, 8, 8, 15, 17, 9 };
            _stats_init_aux(stats);
            break;
        }
        case CLASS_SAMURAI:
        {
            int stats[6] = { 16, 9, 16, 16, 14, 10 };
            _stats_init_aux(stats);
            break;
        }
        case CLASS_BLUE_MAGE:
        {
            int stats[6] = { 16, 16, 9, 16, 14, 10 };
            _stats_init_aux(stats);
            break;
        }
        case CLASS_MAGE:
        case CLASS_HIGH_MAGE:
        case CLASS_GRAY_MAGE:
        case CLASS_YELLOW_MAGE:
        case CLASS_MIRROR_MASTER:
        case CLASS_NECROMANCER:
        {
            int stats[6] = { 16, 17, 9, 9, 16, 9 };
            _stats_init_aux(stats);
            break;
        }
        case CLASS_SORCERER:
        {
            int stats[6] = { 16, 9, 9, 9, 16, 17 };
            _stats_init_aux(stats);
            break;
        }
        case CLASS_PRIEST:
        case CLASS_HIGH_PRIEST:
        case CLASS_PALADIN:
        case CLASS_MINDCRAFTER:
        case CLASS_FORCETRAINER:
        case CLASS_TIME_LORD:
        case CLASS_ARCHAEOLOGIST:
        case CLASS_SCOUT:
        {
            int stats[6] = { 16, 8, 17, 16, 11, 8 };
            _stats_init_aux(stats);
            break;
        }
        case CLASS_ROGUE:
        case CLASS_RANGER:
        case CLASS_WARRIOR_MAGE:
        case CLASS_CHAOS_WARRIOR:
        case CLASS_MAGIC_EATER:
        case CLASS_RED_MAGE:
        case CLASS_RUNE_KNIGHT:
        case CLASS_DEVICEMASTER:
        {
            int stats[6] = { 16, 16, 9, 16, 14, 10 };
            _stats_init_aux(stats);
            break;
        }
        case CLASS_MONK:
        {
            int stats[6] = { 16, 8, 16, 17, 11, 8 };
            _stats_init_aux(stats);
            break;
        }
        case CLASS_MYSTIC:
        {
            int stats[6] = { 16, 8, 8, 17, 11, 16 };
            _stats_init_aux(stats);
            break;
        }
        case CLASS_BEASTMASTER:
        case CLASS_BARD:
        case CLASS_WARLOCK:
        {
            int stats[6] = { 16, 8, 8, 16, 11, 17 };
            _stats_init_aux(stats);
            break;
        }
        /* TODO */
        case CLASS_WILD_TALENT:
        case CLASS_PSION:
        default:
        {
            int stats[6] = { 15, 15, 15, 15, 15, 11 };
            _stats_init_aux(stats);
        }
        }
    }
    _stats_changed = FALSE;
}

static void _stat_dec(int which)
{
    int val = plr->stat_cur[which];
    if (val > 8)
        val--;
    else
        val = 17;
    plr->stat_cur[which] = val;
    plr->stat_max[which] = val;
    _stats_changed = TRUE;
}

static void _stat_inc(int which)
{
    int val = plr->stat_cur[which];
    if (val < 17)
        val++;
    else
        val = 8;
    plr->stat_cur[which] = val;
    plr->stat_max[which] = val;
    _stats_changed = TRUE;
}

static int _stats_score(void)
{
    int i, score = 0;

    for (i = 0; i < MAX_STATS; i++)
        score += _stat_points[plr->stat_cur[i]];

    return score;
}

/************************************************************************
 * Low level utilities
 ***********************************************************************/ 
/* The UI is organized as follows:
    <player fields> | <info panel>
   -----------------------------------
              <menu choices>
   -----------------------------------
              <tips and help>
   ----------------------------------- */

static void _race_class_header(doc_ptr doc);
static void _race_class_info(doc_ptr doc);

static void _race_class_top(doc_ptr doc)
{
    doc_ptr cols[2];

    cols[0] = doc_alloc(27);
    cols[1] = doc_alloc(53);

    _race_class_header(cols[0]);
    _race_class_info(cols[1]);
    doc_insert_cols(doc, cols, 2, 1);

    doc_free(cols[0]);
    doc_free(cols[1]);
}

/* Player Fields */
static void _name_line(doc_ptr doc);
static void _sex_line(doc_ptr doc);
static void _pers_line(doc_ptr doc);
static void _race_line(doc_ptr doc);
static void _class_line(doc_ptr doc);
static void _magic_line(doc_ptr doc);

static void _race_class_header(doc_ptr doc)
{
    _name_line(doc);
    _sex_line(doc);
    if (game_mode != GAME_MODE_BEGINNER)
        _pers_line(doc);
    _race_line(doc);
    if (game_mode != GAME_MODE_MONSTER)
        _class_line(doc);
    _magic_line(doc);
}

static void _name_line(doc_ptr doc)
{
    doc_printf(doc, "Name : <color:B>%s</color>\n", player_name);
}

static void _sex_line(doc_ptr doc)
{
    doc_printf(doc, "Sex  : <color:B>%s</color>\n", sex_info[plr->psex].title);
}

static void _pers_line(doc_ptr doc)
{
    personality_ptr pers_ptr = get_personality();
    doc_printf(doc, "Pers : <color:B>%s</color>\n", pers_ptr->name);
}

static void _race_line(doc_ptr doc)
{
    race_t *race_ptr = get_race();
    if (game_mode == GAME_MODE_MONSTER)
    {
        if ( plr->prace == RACE_MON_DRAGON
          || plr->prace == RACE_MON_GIANT
          || plr->prace == RACE_MON_ELEMENTAL
          || plr->prace == RACE_MON_LICH
          || plr->prace == RACE_MON_TROLL )
        {
            doc_printf(doc, "Race : <color:B>%s</color>\n", race_ptr->subname);
        }
        else
        {
            doc_printf(doc, "Race : <color:B>%s</color>\n", race_ptr->name);
            if (race_ptr->subname)
                doc_printf(doc, "       <color:B>%s</color>\n", race_ptr->subname);
        }
    }
    else
    {
        doc_insert(doc, "Race : <indent><color:B>");
        if (race_ptr->subname)
            doc_printf(doc, "%s ", race_ptr->subname);
        doc_printf(doc, "%s</indent></color>\n", race_ptr->name);
    }
}

static void _class_line(doc_ptr doc)
{
    class_t *class_ptr = get_class();
    doc_printf(doc, "Class: <color:B>%s</color>\n", class_ptr->name);
    if (class_ptr->subname)
    {
        if (plr->pclass == CLASS_WARLOCK)
            doc_printf(doc, "Pact : <color:B>%s</color>\n", class_ptr->subname);
        else if (plr->pclass == CLASS_WEAPONMASTER || plr->pclass == CLASS_DEVICEMASTER)
            doc_printf(doc, "Spec : <color:B>%s</color>\n", class_ptr->subname);
        else if (plr->pclass == CLASS_GRAY_MAGE)
            doc_printf(doc, "Bias : <color:B>%s</color>\n", class_ptr->subname);
        else
            doc_printf(doc, "       <color:B>%s</color>\n", class_ptr->subname);
    }
}

static void _magic_line(doc_ptr doc)
{
    if (plr->dragon_realm)
    {
        dragon_realm_ptr realm = dragon_get_realm(plr->dragon_realm);
        doc_printf(doc, "Magic: <color:B>%s</color>\n", realm->name);
    }
    else if (plr->realm1)
    {
        doc_printf(doc, "Magic: <color:B>%s</color>\n", realm_names[plr->realm1]);
        if (plr->realm2)
            doc_printf(doc, "       <color:B>%s</color>\n", realm_names[plr->realm2]);
    }
}

/* The Info Panel displays Stats and Skills as
   the player builds the character. Since this
   won't all fit in 80x27, we let the user <TAB>
   through the following sections */
enum {
    _RCP_STATS,
    _RCP_SKILLS1,
    _RCP_SKILLS2,
    _RCP_COUNT
};
static int _rcp_state = _RCP_STATS;

/* <TAB> rotates the state of the Info Panel */
static void _inc_rcp_state(void)
{
    ++_rcp_state;
    if (_rcp_state == _RCP_COUNT)
        _rcp_state = _RCP_STATS;
}

static void _stats_line(doc_ptr doc, s16b stats[MAX_STATS], int spell_stat, char color)
{
    int i;
    for (i = 0; i < MAX_STATS; i++)
    {
        doc_printf(doc, "<color:%c>%+3d</color>  ",
            i == spell_stat ? 'v' : color,
            stats[i]);
    }
}

static void _stats_add(s16b stats[MAX_STATS], s16b to_add[MAX_STATS])
{
    int i;
    for (i = 0; i < MAX_STATS; i++)
        stats[i] += to_add[i];
}

static void _race_class_info(doc_ptr doc)
{
    race_t *race_ptr = get_race();
    class_t *class_ptr = get_class();
    personality_ptr pers_ptr = get_personality();
    dragon_realm_ptr realm_ptr = dragon_get_realm(plr->dragon_realm); /* 0 is OK */
    int spell_stat = get_spell_stat();
    
    doc_newline(doc);
    if (_rcp_state == _RCP_STATS)
    {
        s16b stats[MAX_STATS] = {0};

        _stats_add(stats, race_ptr->stats);
        _stats_add(stats, class_ptr->stats);
        _stats_add(stats, pers_ptr->stats);
        if (plr->dragon_realm)
            _stats_add(stats, realm_ptr->stats);

        doc_insert(doc, "<style:heading><color:w>STR  INT  WIS  DEX  CON  CHR  Life  BHP  Exp</color>\n");
        if (game_mode != GAME_MODE_BEGINNER)
        {
            _stats_line(doc, pers_ptr->stats, spell_stat, 'G');
            doc_printf(doc, "%3d%%       %3d%%\n", pers_ptr->life, pers_ptr->exp);
        }
        _stats_line(doc, race_ptr->stats, spell_stat, 'G');
        doc_printf(doc, "%3d%%  %+3d  %3d%%\n", race_ptr->life, race_ptr->base_hp, race_ptr->exp);
        if (game_mode != GAME_MODE_MONSTER)
        {
            _stats_line(doc, class_ptr->stats, spell_stat, 'G');
            doc_printf(doc, "%3d%%  %+3d  %3d%%\n", class_ptr->life, class_ptr->base_hp, class_ptr->exp);
        }
        if (plr->dragon_realm)
        {
            _stats_line(doc, realm_ptr->stats, spell_stat, 'G');
            doc_printf(doc, "%3d%%       %3d%%\n", realm_ptr->life, realm_ptr->exp);
        }

        _stats_line(doc, stats, spell_stat, 'R');
        doc_printf(doc, "<color:R>%3d%%  %+3d  %3d%%</color>\n",
            race_ptr->life * class_ptr->life * pers_ptr->life * realm_ptr->life / 1000000,
            race_ptr->base_hp + class_ptr->base_hp,
            race_ptr->exp * class_ptr->exp * pers_ptr->exp * realm_ptr->exp / 1000000
        );
        doc_insert(doc, "</style>");
    }
    else if (_rcp_state == _RCP_SKILLS1 || _rcp_state == _RCP_SKILLS2)
    {
        skills_desc_t r_desc, c_desc, p_desc, dr_desc, tot_desc;
        skills_t      base, xtra;

        if (game_mode == GAME_MODE_MONSTER)
        {
            base = race_ptr->skills;
            skills_add(&base, &pers_ptr->skills);
            if (plr->dragon_realm)
                skills_add(&base, &realm_ptr->skills);

            xtra = pers_ptr->skills;
            skills_scale(&xtra, 1, 5);
            if (plr->dragon_realm)
            {
                skills_t tmp = realm_ptr->skills;
                skills_scale(&tmp, 1, 5);
                skills_add(&xtra, &tmp);
            }
            skills_add(&xtra, &race_ptr->extra_skills);

            skills_desc_mon_race(race_ptr, &r_desc);
            skills_desc_pers(pers_ptr, &p_desc);
            if (plr->dragon_realm)
                skills_desc_realm(realm_ptr, &dr_desc);
            skills_desc_aux(&base, &xtra, &tot_desc);
        }
        else
        {
            base = class_ptr->skills;
            skills_add(&base, &race_ptr->skills);
            skills_add(&base, &pers_ptr->skills);

            xtra = pers_ptr->skills;
            skills_scale(&xtra, 1, 5);
            skills_add(&xtra, &class_ptr->extra_skills);

            skills_desc_race(race_ptr, &r_desc);
            skills_desc_class(class_ptr, &c_desc);
            skills_desc_pers(pers_ptr, &p_desc);
            skills_desc_aux(&base, &xtra, &tot_desc);
        }

        if (_rcp_state == _RCP_SKILLS1)
        {
            doc_printf(doc, "<color:w>%-10.10s %-10.10s %-10.10s %-10.10s</color>\n",
                "Disarming", "Device", "Save", "Stealth");
            if (game_mode != GAME_MODE_BEGINNER)
            {
                doc_printf(doc, "%s %s %s %s\n",
                    p_desc.dis, p_desc.dev, p_desc.sav, p_desc.stl);
            }
            doc_printf(doc, "%s %s %s %s\n", /* Note: descriptions contain formatting tags, so %-10.10s won't work ... cf skills.c */
                r_desc.dis, r_desc.dev, r_desc.sav, r_desc.stl);
            if (game_mode != GAME_MODE_MONSTER)
            {
                doc_printf(doc, "%s %s %s %s\n",
                    c_desc.dis, c_desc.dev, c_desc.sav, c_desc.stl);
            }
            if (plr->dragon_realm)
            {
                doc_printf(doc, "%s %s %s %s\n",
                    dr_desc.dis, dr_desc.dev, dr_desc.sav, dr_desc.stl);
            }
            doc_printf(doc, "%s %s %s %s\n",
                tot_desc.dis, tot_desc.dev, tot_desc.sav, tot_desc.stl);
        }
        else
        {
            doc_printf(doc, "<color:w>%-10.10s %-10.10s %-10.10s %-10.10s</color>\n",
                "Searching", "Perception", "Melee", "Bows");
            if (game_mode != GAME_MODE_BEGINNER)
            {
                doc_printf(doc, "%s %s %s %s\n",
                    p_desc.srh, p_desc.fos, p_desc.thn, p_desc.thb);
            }
            doc_printf(doc, "%s %s %s %s\n",
                r_desc.srh, r_desc.fos, r_desc.thn, r_desc.thb);
            if (game_mode != GAME_MODE_MONSTER)
            {
                doc_printf(doc, "%s %s %s %s\n",
                    c_desc.srh, c_desc.fos, c_desc.thn, c_desc.thb);
            }
            if (plr->dragon_realm)
            {
                doc_printf(doc, "%s %s %s %s\n",
                    dr_desc.srh, dr_desc.fos, dr_desc.thn, dr_desc.thb);
            }
            doc_printf(doc, "%s %s %s %s\n",
                tot_desc.srh, tot_desc.fos, tot_desc.thn, tot_desc.thb);
        }
    }
}

static void _change_name(void)
{
    if (!arg_lock_name)
    {
        char tmp[64];
        strcpy(tmp, player_name);
        Term_gotoxy(7, 0); /* Hack */
        if (askfor(tmp, 15))
            strcpy(player_name, tmp);
        if (0 == strlen(player_name))
            strcpy(player_name, "PLAYER");
    }
}

static int _inkey(void)
{
    return inkey_special(TRUE);
}

static void _sync_term(doc_ptr doc)
{
    rect_t r = ui_screen_rect();
    Term_load();
    doc_sync_term(doc, doc_range_top_lines(doc, r.cy), doc_pos_create(r.x, r.y));
}

static int _count(int ids[])
{
    int ct = 0, i;
    for (i = 0; ; i++)
    {
        if (ids[i] == -1) break;
        ct++;
    }
    return ct;
}

static void _birth_options(void)
{
    if (game_mode != GAME_MODE_BEGINNER)
    {
        Term_load();
        do_cmd_options_aux(OPT_PAGE_BIRTH, "Birth Option((*)s effect score)");
    }
}

static int _compare_rlvl(mon_race_ptr left, mon_race_ptr right)
{
    if (left->alloc.lvl < right->alloc.lvl)
        return -1;
    if (left->alloc.lvl > right->alloc.lvl)
        return 1;
    if (left->id < right->id)
        return -1;
    if (left->id > right->id)
        return 1;
    return 0;
}

static void _bounty_uniques(void)
{
    vec_ptr v = vec_alloc(NULL);
    int     i;

    mon_alloc_push_filter(mon_race_is_unique);
    for (i = 0; i < MAX_KUBI; i++)
    {
        while (1)
        {
            mon_race_ptr race = mon_alloc_choose_aux2(mon_alloc_tbl, MAX_DEPTH, 0, GMN_QUESTOR);

            if (race->alloc.flags & RFA_NO_QUEST) continue;
            if (race->flagsx & RFX_GUARDIAN) continue;
            if (race->flagsx & RFX_WANTED) continue;
            if (!(race->body.flags & (RF_DROP_CORPSE | RF_DROP_SKELETON))) continue;
            if (race->alloc.rarity > 100) continue;

            race->flagsx |= RFX_WANTED;
            vec_add(v, race);
            break;
        }
    }
    mon_alloc_pop_filter();

    assert(vec_length(v) == MAX_KUBI);
    vec_sort(v, (vec_cmp_f)_compare_rlvl);
    for (i = 0; i < MAX_KUBI; i++)
    {
        mon_race_ptr race = vec_get(v, i);
        kubi_r_idx[i] = race->id;
    }
    vec_free(v);
}

static void _init_turn(void)
{
    if ( plr->prace == RACE_VAMPIRE
      || plr->prace == RACE_MON_VAMPIRE
      || plr->prace == RACE_SKELETON
      || plr->prace == RACE_ZOMBIE
      || plr->prace == RACE_SPECTRE )
    {
        /* Undead start just after midnight */
        dun_mgr()->turn = (TURNS_PER_TICK*3 * TOWN_DAWN) / 4 + 1;
    }
    else dun_mgr()->turn = 1;
}
static void _suppress(mon_race_ptr r)
{
    if (r->attributes & RF_DEPRECATED)
        r->flagsx |= RFX_SUPPRESS;
}
static void _birth_finalize(void)
{
    int i;

    plr->id = scores_next_id();
    plr->pflag |= PFLAG_BIRTH;

    /* Quick Start */
    assert(plr->initial_world_id);
    previous_char.quick_ok = TRUE;
    previous_char.initial_world_id = plr->initial_world_id;
    previous_char.game_mode = game_mode;
    previous_char.psex = plr->psex;
    previous_char.prace = plr->prace;
    previous_char.psubrace = plr->psubrace;
    previous_char.pclass = plr->pclass;
    previous_char.psubclass = plr->psubclass;
    previous_char.personality = plr->personality;
    previous_char.realm1 = plr->realm1;
    previous_char.realm2 = plr->realm2;
    previous_char.dragon_realm = plr->dragon_realm;
    previous_char.au = plr->au;

    for (i = 0; i < MAX_STATS; i++)
        previous_char.stat_max[i] = plr->stat_max[i];

    /* Other Initialization */
    equip_init();
    pack_init();
    quiver_init();
    home_init();
    virtue_init();

    /* Suppress any deprecated monsters for this new game. Upgrading an existing
     * game should continue to generate deprecated monsters, since they may be wanted
     * or questors. We do this before choosing questors and bounty uniques, of course. */
    mon_race_iter(_suppress);

    /* Create initial level ... XXX quests_on_birth needs cave to be set */
    _init_turn();  /* XXX dun_worlds_birth will generate and glow D_SURFACE */
    dun_worlds_birth();
    quests_on_birth();

    _bounty_uniques();

    plr->au = randint1(600) + randint1(100) + 100;

    /* Everybody gets a chaos patron. The chaos warrior is obvious,
     * but anybody else can acquire MUT_CHAOS_GIFT during the game */
    plr->chaos_patron = randint0(MAX_PATRON);

    get_max_stats();
    do_cmd_rerate_aux();

    plr->start_race = plr->prace;
    plr->expfact = calc_exp_factor();

    mp_ptr = &m_info[plr->pclass];
    /* Hack ... external files always make easy stuff hard ... Burglary is natural for rogues!!!*/
    if (plr->pclass == CLASS_ROGUE)
    {
        if (plr->realm1 == REALM_BURGLARY)
            mp_ptr->spell_first = 1;
        else
            mp_ptr->spell_first = 5;
    }

    /* Rest Up to Max HP and SP */
    plr->update |= PU_BONUS | PU_HP | PU_MANA;
    update_stuff();
    plr->chp = plr->mhp;
    plr->csp = plr->msp;
    process_player_name(FALSE);
}

