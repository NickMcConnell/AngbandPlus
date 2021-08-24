#include "angband.h"

#include <assert.h>

bool store_hack = FALSE;
static bool leave_bldg = FALSE;

/************************************************************************
 * Data Types
 ***********************************************************************/
typedef bool (*_k_idx_p)(int k_idx);

typedef struct _owner_s _owner_t, *_owner_ptr;
struct _owner_s
{
    int  id;
    cptr name;
    int  purse;
    int  greed;
    int  race_id;
    bool active;
};

typedef struct _shop_type_s _shop_type_t, *_shop_type_ptr;
struct _shop_type_s
{
    int           id;
    cptr          name;
    obj_p         buy_p;
    obj_create_f  create_f;
    _owner_ptr    owners;
};

typedef struct _bldg_type_s _bldg_type_t, *_bldg_type_ptr;
struct _bldg_type_s
{
    int        id;
    cptr       name;
    void     (*display_f)(bldg_ptr bldg, doc_ptr doc);
    bool     (*command_f)(bldg_ptr bldg, int cmd);
    bool     (*guild_p)(void);
    _owner_ptr owners;
};

typedef struct _last_restock_s _last_restock_t;
struct _last_restock_s
{
    int turn;
    int level;
    int exp;
};

struct shop_s
{
    _shop_type_ptr  type;
    _owner_ptr      owner;
    inv_ptr         inv;
    town_ptr        town;
    _last_restock_t last_restock;
};

struct bldg_s
{
    _bldg_type_ptr type;
    town_ptr       town;
    _owner_ptr     owner;
};

/************************************************************************
 * Shop Types and Their Owners (names originally from CthAngband?)
 ***********************************************************************/

static bool _general_will_buy(obj_ptr obj);
static bool _general_create(obj_ptr obj, u32b mode);
static bool _armory_will_buy(obj_ptr obj);
static bool _armory_create(obj_ptr obj, u32b mode);
static bool _weapon_will_buy(obj_ptr obj);
static bool _weapon_create(obj_ptr obj, u32b mode);
static bool _temple_will_buy(obj_ptr obj);
static bool _temple_create(obj_ptr obj, u32b mode);
static bool _alchemist_will_buy(obj_ptr obj);
static bool _alchemist_create(obj_ptr obj, u32b mode);
static bool _magic_will_buy(obj_ptr obj);
static bool _magic_create(obj_ptr obj, u32b mode);
static bool _black_market_will_buy(obj_ptr obj);
static bool _black_market_create(obj_ptr obj, u32b mode);
static bool _book_will_buy(obj_ptr obj);
static bool _book_create(obj_ptr obj, u32b mode);
static bool _jeweler_will_buy(obj_ptr obj);
static bool _jeweler_create(obj_ptr obj, u32b mode);

static _owner_t _general_owners[] = {
    {  1, "Bilbo the Friendly",         200, 108, RACE_HOBBIT },
    {  2, "Elwyn, Lady of the Forest", 2000, 108, RACE_HIGH_ELF },
    {  3, "Sultan the Midget",          300, 107, RACE_GNOME },
    {  4, "Lyar-el the Comely",         300, 107, RACE_WOOD_ELF },
    {  5, "Falilmawen the Friendly",    250, 108, RACE_HOBBIT },
    {  6, "Voirin the Cowardly",        500, 108, RACE_HUMAN },
    {  7, "Erashnak the Midget",        750, 107, RACE_BEASTMAN },
    {  8, "Grug the Comely",           1000, 107, RACE_HALF_TITAN },
    {  9, "Forovir the Cheap",          250, 108, RACE_HUMAN },
    { 10, "Ellis the Fool",             500, 108, RACE_HUMAN },
    { 11, "Filbert the Hungry",         750, 107, RACE_VAMPIRE },
    { 12, "Fthnargl Psathiggua",       1000, 107, RACE_MIND_FLAYER },
    { 13, "Eloise Long-Dead",           250, 108, RACE_SPECTRE },
    { 14, "Fundi the Slow",             500, 108, RACE_ZOMBIE },
    { 15, "Granthus",                   750, 107, RACE_SKELETON },
    { 16, "Lorax the Suave",           1000, 107, RACE_VAMPIRE },
    { 17, "Butch",                      250, 108, RACE_SNOTLING },
    { 18, "Elbereth the Beautiful",     500, 108, RACE_HIGH_ELF },
    { 19, "Sarleth the Sneaky",         750, 107, RACE_GNOME },
    { 20, "Narlock",                   1000, 107, RACE_DWARF },
    { 21, "Haneka the Small",           250, 108, RACE_GNOME },
    { 22, "Loirin the Mad",             500, 108, RACE_HALF_GIANT },
    { 23, "Wuto Poisonbreath",          750, 107, RACE_DRACONIAN },
    { 24, "Araaka the Rotund",         1000, 107, RACE_DRACONIAN },
    { 25, "Poogor the Dumb",            250, 108, RACE_BEASTMAN },
    { 26, "Felorfiliand",               500, 108, RACE_DEMIGOD },
    { 27, "Maroka the Aged",            750, 107, RACE_GNOME },
    { 28, "Sasin the Bold",            1000, 107, RACE_HALF_GIANT },
    { 29, "Abiemar the Peasant",        250, 108, RACE_HUMAN },
    { 30, "Hurk the Poor",              500, 108, RACE_SNOTLING },
    { 31, "Soalin the Wretched",        750, 107, RACE_ZOMBIE },
    { 32, "Merulla the Humble",        1000, 107, RACE_DEMIGOD },
    { 0 }
};
static _owner_t _armory_owners[] = {
    {  1, "Kon-Dar the Ugly",          5000, 115, RACE_SNOTLING },
    {  2, "Darg-Low the Grim",        10000, 111, RACE_HUMAN },
    {  3, "Decado the Handsome",      25000, 112, RACE_DUNADAN },
    {  4, "Wieland the Smith",        40000, 112, RACE_DWARF },
    {  5, "Valirel",                  30000, 115, RACE_WOOD_ELF },
    {  6, "Darg-Low the Grim",        15000, 111, RACE_HUMAN },
    {  7, "Decado the Handsome",      25000, 112, RACE_AMBERITE },
    {  8, "Elo Dragonscale",          35000, 112, RACE_DEMIGOD },
    {  9, "Delicatus",                10000, 115, RACE_SPRITE },
    { 10, "Gruce the Huge",           15000, 111, RACE_HALF_GIANT },
    { 11, "Animus",                   25000, 112, RACE_GOLEM },
    { 12, "Malvus",                   30000, 112, RACE_HALF_TITAN },
    { 13, "Selaxis",                  10000, 115, RACE_ZOMBIE },
    { 14, "Deathchill",                5000, 111, RACE_SPECTRE },
    { 15, "Drios the Faint",          25000, 112, RACE_SPECTRE },
    { 16, "Bathric the Cold",         30000, 112, RACE_VAMPIRE },
    { 17, "Vengella the Cruel",       10000, 115, RACE_HALF_TROLL },
    { 18, "Wyrana the Mighty",        15000, 111, RACE_HUMAN },
    { 19, "Yojo II",                  25000, 112, RACE_DWARF },
    { 20, "Ranalar the Sweet",        30000, 112, RACE_AMBERITE },
    { 21, "Horbag the Unclean",        5000, 115, RACE_SNOTLING },
    { 22, "Elelen the Telepath",      30000, 111, RACE_HIGH_ELF },
    { 23, "Isedrelias",               25000, 112, RACE_SPRITE },
    { 24, "Vegnar One-eye",            5000, 112, RACE_CYCLOPS },
    { 25, "Rodish the Chaotic",       10000, 115, RACE_BEASTMAN },
    { 26, "Hesin Swordmaster",        15000, 111, RACE_NIBELUNG },
    { 27, "Elvererith the Cheat",     10000, 112, RACE_DARK_ELF },
    { 28, "Zzathath the Imp",         30000, 112, RACE_IMP },
    { 0 }
};
static _owner_t _weapon_owners[] = {
    {  1, "Arnold the Beastly",        5000, 115, RACE_BARBARIAN },
    {  2, "Arndal Beast-Slayer",      10000, 110, RACE_HUMAN },
    {  3, "Eddie Beast-Master",       25000, 115, RACE_SNOTLING },
    {  4, "Oglign Dragon-Slayer",     50000, 112, RACE_DWARF },
    {  5, "Drew the Skilled",         10000, 115, RACE_HUMAN },
    {  6, "Orrax Dragonson",          15000, 110, RACE_DRACONIAN },
    {  7, "Anthrax Disease-Carrier",  25000, 115, RACE_BEASTMAN },
    {  8, "Arkhoth the Stout",        35000, 112, RACE_DWARF },
    {  9, "Sarlyas the Rotten",        5000, 115, RACE_ZOMBIE },
    { 10, "Tuethic Bare-Bones",       15000, 110, RACE_SKELETON },
    { 11, "Bilious",                  25000, 115, RACE_BEASTMAN },
    { 12, "Fasgul",                   30000, 112, RACE_ZOMBIE },
    { 13, "Ellefris the Paladin",     10000, 115, RACE_BARBARIAN },
    { 14, "K'trrik'k",                15000, 110, RACE_KLACKON },
    { 15, "Drocus Spiderfriend",      25000, 115, RACE_DARK_ELF },
    { 16, "Fungus Giant-Slayer",      40000, 112, RACE_DWARF },
    { 17, "Delantha",                 10000, 115, RACE_WATER_ELF },
    { 18, "Solvistani the Ranger",    15000, 110, RACE_WOOD_ELF },
    { 19, "Xoril the Slow",           25000, 115, RACE_GOLEM },
    { 20, "Aeon Flux",                20000, 112, RACE_HUMAN },
    { 21, "Nadoc the Strong",         10000, 115, RACE_HOBBIT },
    { 22, "Eramog the Weak",          15000, 110, RACE_KOBOLD },
    { 23, "Eowilith the Fair",        25000, 115, RACE_VAMPIRE },
    { 24, "Huimog Balrog-Slayer",     30000, 112, RACE_SNOTLING },
    { 25, "Peadus the Cruel",          5000, 115, RACE_HUMAN },
    { 26, "Vamog Slayer",             15000, 110, RACE_HALF_OGRE },
    { 27, "Hooshnak the Vicious",     25000, 115, RACE_BEASTMAN },
    { 28, "Balenn War-Dancer",        30000, 112, RACE_BARBARIAN },
    { 0 }
};
static _owner_t _temple_owners[] = {
    {  1, "Ludwig the Humble",         5000, 109, RACE_DWARF },
    {  2, "Gunnar the Paladin",       10000, 110, RACE_HALF_TROLL },
    {  3, "Torin the Chosen",         25000, 107, RACE_HIGH_ELF },
    {  4, "Sarastro the Wise",        30000, 109, RACE_HUMAN },
    {  5, "Parsival the Pure",        25000, 107, RACE_HIGH_ELF },
    {  6, "Asenath the Holy",         30000, 109, RACE_HUMAN },
    {  7, "McKinnon",                 10000, 109, RACE_HUMAN },
    {  8, "Mistress Chastity",        15000, 110, RACE_HIGH_ELF },
    {  9, "Hashnik the Druid",        25000, 107, RACE_HOBBIT },
    { 10, "Finak",                    30000, 109, RACE_YEEK },
    { 11, "Krikkik",                  10000, 109, RACE_KLACKON },
    { 12, "Morival the Wild",         15000, 110, RACE_DEMIGOD },
    { 13, "Hoshak the Dark",          25000, 107, RACE_IMP },
    { 14, "Atal the Wise",            30000, 109, RACE_HUMAN },
    { 15, "Ibenidd the Chaste",       10000, 109, RACE_HUMAN },
    { 16, "Eridish",                  15000, 110, RACE_HALF_TROLL },
    { 17, "Vrudush the Shaman",       25000, 107, RACE_HALF_OGRE },
    { 18, "Haob the Berserker",       30000, 109, RACE_BARBARIAN },
    { 19, "Proogdish the Youthfull",  10000, 109, RACE_HALF_OGRE },
    { 20, "Lumwise the Mad",          15000, 110, RACE_YEEK },
    { 21, "Muirt the Virtuous",       25000, 107, RACE_KOBOLD },
    { 22, "Dardobard the Weak",       30000, 109, RACE_SPECTRE },
    { 0 }
};
static _owner_t _alchemist_owners[] = {
    {  1, "Mauser the Chemist",       10000, 111, RACE_HUMAN },
    {  2, "Wizzle the Chaotic",       10000, 110, RACE_HOBBIT },
    {  3, "Midas the Greedy",         15000, 116, RACE_GNOME },
    {  4, "Ja-Far the Alchemist",     15000, 111, RACE_DEMIGOD },
    {  5, "Kakalrakakal",             15000, 116, RACE_KLACKON },
    {  6, "Jal-Eth the Alchemist",    15000, 111, RACE_DEMIGOD },
    {  7, "Fanelath the Cautious",    10000, 111, RACE_DWARF },
    {  8, "Runcie the Insane",        10000, 110, RACE_HUMAN },
    {  9, "Grumbleworth",             15000, 116, RACE_GNOME },
    { 10, "Flitter",                  15000, 111, RACE_SPRITE },
    { 11, "Xarillus",                 10000, 111, RACE_HUMAN },
    { 12, "Egbert the Old",           10000, 110, RACE_DWARF },
    { 13, "Valindra the Proud",       15000, 116, RACE_HIGH_ELF },
    { 14, "Taen the Alchemist",       15000, 111, RACE_HUMAN },
    { 15, "Cayd the Sweet",           10000, 111, RACE_VAMPIRE },
    { 16, "Fulir the Dark",           10000, 110, RACE_NIBELUNG },
    { 17, "Domli the Humble",         15000, 116, RACE_DWARF },
    { 18, "Yaarjukka Demonspawn",     15000, 111, RACE_IMP },
    { 19, "Gelaraldor the Herbmaster",10000, 111, RACE_HIGH_ELF },
    { 20, "Olelaldan the Wise",       10000, 110, RACE_BARBARIAN },
    { 21, "Fthoglo the Demonicist",   15000, 116, RACE_IMP },
    { 22, "Dridash the Alchemist",    15000, 111, RACE_SNOTLING },
    { 23, "Nelir the Strong",         10000, 111, RACE_CYCLOPS },
    { 24, "Lignus the Pungent",       10000, 110, RACE_SNOTLING },
    { 25, "Tilba",                    15000, 116, RACE_HOBBIT },
    { 26, "Myrildric the Wealthy",    15000, 111, RACE_HUMAN },
    { 27, "The Mad Chemist",          25000, 118, RACE_HUMAN },
    { 0 }
};
static _owner_t _magic_owners[] = {
    {  1, "Lo Pan the Sorcerer",      20000, 110, RACE_HUMAN },
    {  2, "Buggerby the Great",       20000, 113, RACE_GNOME },
    {  3, "The Wizard of Yendor",     30000, 110, RACE_HUMAN },
    {  4, "Rjak the Necromancer",     30000, 110, RACE_DARK_ELF },
    {  5, "Skidney the Sorcerer",     15000, 110, RACE_HUMAN },
    {  6, "Kyria the Illusionist",    30000, 110, RACE_HUMAN },
    {  7, "Nikki the Necromancer",    30000, 110, RACE_DARK_ELF },
    {  8, "Solostoran",               15000, 110, RACE_SPRITE },
    {  9, "Achshe the Tentacled",     20000, 113, RACE_MIND_FLAYER },
    { 10, "Kaza the Noble",           30000, 110, RACE_HIGH_ELF },
    { 11, "Fazzil the Dark",          30000, 110, RACE_DARK_ELF },
    { 12, "Keldorn the Grand",        15000, 110, RACE_DWARF },
    { 13, "Philanthropus",            20000, 113, RACE_HOBBIT },
    { 14, "Agnar the Enchantress",    30000, 110, RACE_HUMAN },
    { 15, "Buliance the Necromancer", 30000, 110, RACE_BEASTMAN },
    { 16, "Vuirak the High-Mage",     15000, 110, RACE_BEASTMAN },
    { 17, "Madish the Smart",         20000, 113, RACE_BEASTMAN },
    { 18, "Falebrimbor",              30000, 110, RACE_HIGH_ELF },
    { 19, "Felil-Gand the Subtle",    30000, 110, RACE_DARK_ELF },
    { 20, "Thalegord the Shaman",     15000, 110, RACE_BARBARIAN },
    { 21, "Cthoaloth the Mystic",     20000, 113, RACE_MIND_FLAYER },
    { 22, "Ibeli the Illusionist",    30000, 110, RACE_SKELETON },
    { 23, "Heto the Necromancer",     30000, 110, RACE_YEEK },
    { 0 }
};
static _owner_t _black_market_owners[] = {
    {  1, "Gary Gygaz",               20000, 150, RACE_HALF_TROLL },
    {  2, "Histor the Goblin",        20000, 150, RACE_SNOTLING },
    {  3, "Quark the Ferengi",        30000, 150, RACE_DWARF },
    {  4, "Topi the Fair(?)",         30000, 150, RACE_HUMAN },
    {  5, "Vhassa the Dead",          20000, 150, RACE_ZOMBIE },
    {  6, "Kyn the Treacherous",      20000, 150, RACE_VAMPIRE },
    {  7, "Bubonicus",                30000, 150, RACE_BEASTMAN },
    {  8, "Corpselight",              30000, 150, RACE_SPECTRE },
    {  9, "Parrish the Bloodthirsty", 20000, 150, RACE_VAMPIRE },
    { 10, "Vile",                     20000, 150, RACE_SKELETON },
    { 11, "Prentice the Trusted",     30000, 150, RACE_SKELETON },
    { 12, "Griella Humanslayer",      30000, 150, RACE_IMP },
    { 13, "Angel",                    20000, 150, RACE_VAMPIRE },
    { 14, "Flotsam the Bloated",      20000, 150, RACE_ZOMBIE },
    { 15, "Nieval",                   30000, 150, RACE_VAMPIRE },
    { 16, "Anastasia the Luminous",   30000, 150, RACE_SPECTRE },
    { 17, "Charity the Necromancer",  20000, 150, RACE_DARK_ELF },
    { 18, "Pugnacious the Pugilist",  20000, 150, RACE_SNOTLING },
    { 19, "Footsore the Lucky",       30000, 150, RACE_BEASTMAN },
    { 20, "Sidria Lighfingered",      30000, 150, RACE_HUMAN },
    { 21, "Riatho the Juggler",       20000, 150, RACE_HOBBIT },
    { 22, "Janaaka the Shifty",       20000, 150, RACE_GNOME },
    { 23, "Cina the Rogue",           30000, 150, RACE_GNOME },
    { 24, "Arunikki Greatclaw",       30000, 150, RACE_DRACONIAN },
    { 25, "Chaeand the Poor",         20000, 150, RACE_HUMAN },
    { 26, "Afardorf the Brigand",     20000, 150, RACE_BARBARIAN },
    { 27, "Lathaxl the Greedy",       30000, 150, RACE_MIND_FLAYER },
    { 28, "Falarewyn",                30000, 150, RACE_WOOD_ELF },
    { 29, "Vosur the Wrinkled",       20000, 150, RACE_NIBELUNG },
    { 30, "Araord the Handsome",      20000, 150, RACE_AMBERITE },
    { 31, "Theradfrid the Loser",     30000, 150, RACE_HUMAN },
    { 32, "One-Legged Eroolo",        30000, 150, RACE_HALF_OGRE },
    { 0 }
};
static _owner_t _book_owners[] = {
    {  1, "Dolaf the Greedy",         10000, 108, RACE_HUMAN },
    {  2, "Odnar the Sage",           15000, 105, RACE_HIGH_ELF },
    {  3, "Gandar the Neutral",       25000, 110, RACE_DARK_ELF },
    {  4, "Ro-sha the Patient",       30000, 105, RACE_DEMIGOD },
    {  5, "Randolph Carter",          15000, 108, RACE_HUMAN },
    {  6, "Sarai the Swift",          15000, 108, RACE_HUMAN },
    {  7, "Bodril the Seer",          20000, 105, RACE_HIGH_ELF },
    {  8, "Veloin the Quiet",         25000, 110, RACE_ZOMBIE },
    {  9, "Vanthylas the Learned",    30000, 105, RACE_MIND_FLAYER },
    { 10, "Ossein the Literate",      15000, 108, RACE_SKELETON },
    { 11, "Olvar Bookworm",           20000, 105, RACE_VAMPIRE },
    { 12, "Shallowgrave",             25000, 110, RACE_ZOMBIE },
    { 13, "Death Mask",               30000, 105, RACE_ZOMBIE },
    { 14, "Asuunu the Learned",       15000, 108, RACE_MIND_FLAYER },
    { 15, "Prirand the Dead",         20000, 105, RACE_ZOMBIE },
    { 16, "Ronar the Iron",           25000, 110, RACE_GOLEM },
    { 17, "Galil-Gamir",              35000, 105, RACE_HIGH_ELF },
    { 18, "Rorbag Book-Eater",         5000, 108, RACE_KOBOLD },
    { 19, "Kiriarikirk",              20000, 105, RACE_KLACKON },
    { 20, "Rilin the Quiet",          25000, 110, RACE_DWARF },
    { 21, "Isung the Lord",           30000, 105, RACE_HIGH_ELF },
    { 22, "Grundin Inkbeard",         35000, 112, RACE_DWARF },
    { 0 }
};
static _owner_t _jeweler_owners[] = {
    {  1, "Dalanna the Sweet",        20000, 108, RACE_HUMAN },
    {  2, "Mesistrond",               15000, 105, RACE_DARK_ELF },
    {  3, "Mr. Biggles",              50000, 110, RACE_GNOME },
    {  4, "Snivelsby",                10000, 108, RACE_SNOTLING },
    {  5, "Grug",                     10000, 110, RACE_HALF_TROLL },
    {  6, "Raphaella",                35000, 105, RACE_ARCHON },
    {  7, "Sylphrana Lightfoot",      25000, 105, RACE_SPRITE },
    {  8, "Helen the Beautiful",      20000, 110, RACE_DEMIGOD },
    {  9, "Rattles Neverborn",        10000, 112, RACE_SKELETON },
    { 10, "Trinkles the Stinky",      30000, 110, RACE_GNOME },
    { 11, "Gaudella",                 15000, 105, RACE_HUMAN },
    { 12, "Argwynna of the Wood",     40000, 105, RACE_WOOD_ELF },
    { 13, "Mugbasha",                  5000, 120, RACE_KOBOLD },
    { 0 }
};
static _shop_type_t _shop_types[] = 
{
    { SHOP_GENERAL, "General Store", _general_will_buy, _general_create, _general_owners },
    { SHOP_ARMORY, "Armory", _armory_will_buy, _armory_create, _armory_owners },
    { SHOP_WEAPON, "Weapon Smiths", _weapon_will_buy, _weapon_create, _weapon_owners },
    { SHOP_TEMPLE, "Temple", _temple_will_buy, _temple_create, _temple_owners },
    { SHOP_ALCHEMIST, "Alchemy Shop", _alchemist_will_buy, _alchemist_create, _alchemist_owners },
    { SHOP_MAGIC, "Magic Shop", _magic_will_buy, _magic_create, _magic_owners },
    { SHOP_BLACK_MARKET, "Black Market", _black_market_will_buy, _black_market_create, _black_market_owners },
    { SHOP_BOOK, "Bookstore", _book_will_buy, _book_create, _book_owners },
    { SHOP_JEWELER, "Jewelry Shop", _jeweler_will_buy, _jeweler_create, _jeweler_owners },
    { SHOP_NONE }
};

static _shop_type_ptr _get_shop_type(int which)
{
    int i;
    for (i = 0;; i++)
    {
        _shop_type_ptr type = &_shop_types[i];
        if (type->id == SHOP_NONE) return NULL;
        if (type->id == which) return type;
    }
}

static bool _shop_is_basic(shop_ptr shop)
{
    switch (shop->type->id)
    {
    case SHOP_BLACK_MARKET:
    case SHOP_JEWELER:
        return FALSE;
    }
    return TRUE;
}

static _owner_ptr _get_owner(_owner_ptr owners, int which)
{
    int i;
    for (i = 0; ; i++)
    {
        _owner_ptr owner = &owners[i];
        if (!owner->name) break;
        if (owner->id == which) return owner;
    }
    return NULL;
}

static bool _will_buy(obj_ptr obj)
{
    if (obj_value(obj) <= 0) return FALSE;
    return TRUE;
}

static bool _stock_p(int k_idx)
{
    if (k_info[k_idx].gen_flags & OFG_INSTA_ART)
        return FALSE;

    if (plr_dun()->type->id == D_SURFACE)
    {
        town_ptr town = towns_current_town();
        assert(town);
        if (!town || !(town->flags & TF_SECRET))
        {
            if (!(k_info[k_idx].gen_flags & OFG_TOWN))
                return FALSE;
        }
    }
    return TRUE;
}

static int _get_k_idx(_k_idx_p p, int lvl)
{
    int k_idx;
    if (p)
    {
        get_obj_num_hook = p;
        get_obj_num_prep();
    }
    k_idx = get_obj_num(lvl);
    if (p)
    {
        get_obj_num_hook = NULL;
        get_obj_num_prep();
    }
    return k_idx;
}

static int _mod_lvl(int lvl)
{
    int d = plr_dun()->difficulty;
    if (d > lvl)
        return (d - lvl)/3 + lvl;
    return lvl;
}

static void _discount(obj_ptr obj)
{
    int discount = 0;
    int cost = obj_value(obj);

    if (cost < 5)          discount = 0;
    else if (one_in_(25))  discount = 25;
    else if (one_in_(150)) discount = 50;
    else if (one_in_(300)) discount = 75;
    else if (one_in_(500)) discount = 90;

    if (obj_is_art(obj))
        discount = 0;

    obj->discount = discount;
}

static bool _create(obj_ptr obj, int k_idx, int lvl, u32b mode)
{
    if (!k_idx) return FALSE;

    object_prep(obj, k_idx);
    apply_magic(obj, lvl, mode);
    if (obj->tval == TV_LIGHT)
    {
        if (obj->sval == SV_LIGHT_TORCH) obj->xtra4 = FUEL_TORCH / 2;
        if (obj->sval == SV_LIGHT_LANTERN) obj->xtra4 = FUEL_LAMP / 2;
    }

    if (obj_is_cursed(obj)) return FALSE;

    obj->ident |= IDENT_STORE;
    if (obj_value(obj) <= 0) return FALSE; /* Note: requires IDENT_STORE to work!!! */

    _discount(obj);
    obj_make_pile(obj);
    return TRUE;
}

/************************************************************************
 * The General Store
 ***********************************************************************/
static bool _general_will_buy(obj_ptr obj)
{
    switch (obj->tval)
    {
    case TV_POTION:
        if (obj->sval != SV_POTION_WATER) return FALSE;
    case TV_WHISTLE:
    case TV_FOOD:
    case TV_LIGHT:
    case TV_FLASK:
    case TV_SPIKE:
    case TV_SHOT:
    case TV_ARROW:
    case TV_BOLT:
    case TV_DIGGING:
    case TV_CLOAK:
    case TV_BOTTLE: /* 'Green', recycling Angband */
    case TV_FIGURINE:
    case TV_STATUE:
    case TV_CAPTURE:
    case TV_CARD:
        break;
    default:
        return FALSE;
    }
    return _will_buy(obj);
}

static bool _general_stock_p(int k_idx)
{
    if (!_stock_p(k_idx))
        return FALSE;

    switch (k_info[k_idx].tval)
    {
    case TV_FLASK:
    case TV_SPIKE:
    case TV_SHOT:
    case TV_ARROW:
    case TV_BOLT:
    case TV_CAPTURE:
    case TV_FIGURINE:
    case TV_CLOAK:
    case TV_LIGHT:
    case TV_FOOD:
    case TV_DIGGING:
        return TRUE;
    }
    return FALSE;
}

static bool _stock_ammo_p(int k_idx)
{
    if (!_stock_p(k_idx))
        return FALSE;
    switch (k_info[k_idx].tval)
    {
    case TV_SHOT:
    case TV_ARROW:
    case TV_BOLT:
        return TRUE;
    }
    return FALSE;
}

static bool _general_create(obj_ptr obj, u32b mode)
{
    int k_idx;
    if (one_in_(50))
        k_idx = lookup_kind(TV_CAPTURE, 0);
    else if (one_in_(3))
        k_idx = lookup_kind(TV_FOOD, SV_FOOD_RATION);
    else if (one_in_(3))
        k_idx = lookup_kind(TV_POTION, SV_POTION_WATER);
    else if (one_in_(3))
        k_idx = lookup_kind(TV_FLASK, SV_FLASK_OIL);
    else if (one_in_(3))
        k_idx = lookup_kind(TV_LIGHT, one_in_(2) ? SV_LIGHT_LANTERN : SV_LIGHT_TORCH);
    else if (one_in_(3))
        k_idx = _get_k_idx(_stock_ammo_p, _mod_lvl(10));
    else if (one_in_(3))
        k_idx = lookup_kind(TV_SPIKE, SV_ANY);
    else if (one_in_(3))
        k_idx = lookup_kind(TV_DIGGING, SV_SHOVEL);
    else if (one_in_(5))
        k_idx = lookup_kind(TV_DIGGING, SV_PICK);
    else
        k_idx = _get_k_idx(_general_stock_p, _mod_lvl(20));
    return _create(obj, k_idx, _mod_lvl(rand_range(1, 5)), mode);
}

/************************************************************************
 * The Armory
 ***********************************************************************/
static bool _armory_will_buy(obj_ptr obj)
{
    return obj_is_armor(obj) && _will_buy(obj);
}

static bool _armory_stock_p(int k_idx)
{
    if (!_stock_p(k_idx))
        return FALSE;

    switch (k_info[k_idx].tval)
    {
    case TV_HARD_ARMOR:
    case TV_SOFT_ARMOR:
    case TV_DRAG_ARMOR:
    case TV_GLOVES:
    case TV_HELM:
    case TV_BOOTS:
    case TV_SHIELD:
        return TRUE;
    }
    return FALSE;
}

static bool _armory_create(obj_ptr obj, u32b mode)
{
    int k_idx = _get_k_idx(_armory_stock_p, _mod_lvl(20));
    return _create(obj, k_idx, _mod_lvl(rand_range(1, 5)), mode);
}

/************************************************************************
 * The Weapon Smiths
 ***********************************************************************/
static bool _weapon_will_buy(obj_ptr obj)
{
    switch (obj->tval)
    {
    case TV_SHOT:
    case TV_BOLT:
    case TV_ARROW:
    case TV_BOW:
    case TV_QUIVER:
    case TV_DIGGING:
    case TV_POLEARM:
    case TV_SWORD:
    case TV_HISSATSU_BOOK:
    case TV_RAGE_BOOK:
        break;
    case TV_HAFTED:
        if(obj->sval == SV_WIZSTAFF) return FALSE;
        break;
    default:
        return FALSE;
    }
    return _will_buy(obj);
}

static bool _weapon_stock_p(int k_idx)
{
    if (!_stock_p(k_idx))
        return FALSE;

    switch (k_info[k_idx].tval)
    {
    case TV_POLEARM:
    case TV_SWORD:
        return TRUE;
    }
    return FALSE;
}
static bool _weapon_book_p(int k_idx)
{
    if (!_stock_p(k_idx))
        return FALSE;

    switch (k_info[k_idx].tval)
    {
    case TV_HISSATSU_BOOK:
    case TV_RAGE_BOOK:
        return TRUE;
    }
    return FALSE;
}
static bool _weapon_stock_shooter_p(int k_idx)
{
    if (!_stock_p(k_idx))
        return FALSE;
    switch (k_info[k_idx].tval)
    {
    case TV_BOW:
        return TRUE;
    }
    return FALSE;
}
static bool _weapon_create(obj_ptr obj, u32b mode)
{
    int k_idx;
    int l1 = _mod_lvl(20);
    int l2 = _mod_lvl(rand_range(1, 5));
    if (one_in_(3))
        k_idx = _get_k_idx(_weapon_book_p, l1);
    else if (one_in_(4))
        k_idx = _get_k_idx(_weapon_stock_shooter_p, l1);
    else if (one_in_(3))
        k_idx = _get_k_idx(_stock_ammo_p, l1);
    else if (one_in_(9))
        k_idx = lookup_kind(TV_QUIVER, 0);
    else
        k_idx = _get_k_idx(_weapon_stock_p, l1);
    return _create(obj, k_idx, l2, mode);
}

/************************************************************************
 * The Temple
 ***********************************************************************/
static bool _temple_will_buy(obj_ptr obj)
{
    switch (obj->tval)
    {
    case TV_LIFE_BOOK:
    case TV_CRUSADE_BOOK:
    case TV_BLESS_BOOK:
    case TV_HEX_BOOK:
    case TV_SCROLL:
    case TV_POTION:
    case TV_HAFTED:
        break;
    case TV_FIGURINE:
    case TV_STATUE: {
        monster_race *r_ptr = mon_race_lookup(obj->race_id);

        if (!mon_race_is_evil(r_ptr))
        {
            if (mon_race_is_good(r_ptr)) break;
            if (mon_race_is_animal(r_ptr)) break;
            if (mon_race_is_char_ex(r_ptr, "?!")) break; /* mimics?? */
        }
        return FALSE; }
    case TV_POLEARM:
    case TV_SWORD:
        if (obj_is_blessed(obj)) break;
        return FALSE;
    default:
        return FALSE;
    }
    return _will_buy(obj);
}

static bool _temple_stock_p(int k_idx)
{
    if (!_stock_p(k_idx))
        return FALSE;

    switch (k_info[k_idx].tval)
    {
    case TV_LIFE_BOOK:
    case TV_CRUSADE_BOOK:
    case TV_BLESS_BOOK:
        return TRUE;

    case TV_HAFTED:
        return TRUE;

    /* Scrolls and Potions are also stocked by the Alchemist */
    case TV_SCROLL:
        switch (k_info[k_idx].sval)
        {
        case SV_SCROLL_REMOVE_CURSE:
        case SV_SCROLL_BLESSING:
        case SV_SCROLL_HOLY_CHANT:
        case SV_SCROLL_WORD_OF_RECALL:
        case SV_SCROLL_STAR_REMOVE_CURSE:
        case SV_SCROLL_RUNE_OF_PROTECTION:
            return TRUE;
        }
        return FALSE;

    case TV_POTION:
        switch (k_info[k_idx].sval)
        {
        case SV_POTION_RESIST_HEAT:
        case SV_POTION_RESIST_COLD:
        case SV_POTION_RESTORE_EXP:
        case SV_POTION_CURE_CRITICAL:
        case SV_POTION_CURE_SERIOUS:
        case SV_POTION_CURE_LIGHT:
        case SV_POTION_BOLDNESS:
        case SV_POTION_HEROISM:
        case SV_POTION_HEALING:
        case SV_POTION_STAR_HEALING:
        case SV_POTION_LIFE:
        case SV_POTION_CURING:
            return TRUE;
        }
        return FALSE;
    }
    return FALSE;
}

static bool _temple_create(obj_ptr obj, u32b mode)
{
    int k_idx;
    /*if (one_in_(3))
        k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_WORD_OF_RECALL);
    else*/ if (one_in_(7))
        k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_REMOVE_CURSE);
    else if (one_in_(20))
        k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_STAR_REMOVE_CURSE);
    else
        k_idx = _get_k_idx(_temple_stock_p, _mod_lvl(20));
    return _create(obj, k_idx, _mod_lvl(rand_range(1, 5)), mode);
}

/************************************************************************
 * The Alchemist
 ***********************************************************************/
static bool _alchemist_will_buy(obj_ptr obj)
{
    switch (obj->tval)
    {
    case TV_POTION:
    case TV_SCROLL:
        break;
    default:
        return FALSE;
    }
    return _will_buy(obj);
}

static bool _alchemist_stock_p(int k_idx)
{
    if (!_stock_p(k_idx))
        return FALSE;

    switch (k_info[k_idx].tval)
    {
    /* Scrolls and Potions are also stocked by the Temple. */
    case TV_SCROLL:
    case TV_POTION:
        if (!_temple_stock_p(k_idx))
            return TRUE;
        break;
    }
    return FALSE;
}

static bool _alchemist_create(obj_ptr obj, u32b mode)
{
    int k_idx;
    /*if (one_in_(3))
        k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_WORD_OF_RECALL);
    else*/ if (one_in_(5))
        k_idx = lookup_kind(TV_POTION, SV_POTION_RES_STR + randint0(6));
    else if (one_in_(7))
        k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_IDENTIFY);
    else if (one_in_(10))
        k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_TELEPORT);
    else if (one_in_(20))
        k_idx = lookup_kind(TV_SCROLL, SV_SCROLL_STAR_IDENTIFY);
    else
        k_idx = _get_k_idx(_alchemist_stock_p, _mod_lvl(20));
    return _create(obj, k_idx, _mod_lvl(rand_range(1, 5)), mode);
}

/************************************************************************
 * The Magic Shop
 ***********************************************************************/
static bool _magic_will_buy(obj_ptr obj)
{
    switch (obj->tval)
    {
    case TV_SORCERY_BOOK:
    case TV_NATURE_BOOK:
    case TV_CHAOS_BOOK:
    case TV_ARMAGEDDON_BOOK:
    case TV_ILLUSION_BOOK:
    case TV_DEATH_BOOK:
    case TV_TRUMP_BOOK:
    case TV_ARCANE_BOOK:
    case TV_CRAFT_BOOK:
    case TV_DAEMON_BOOK:
    case TV_MUSIC_BOOK:
    case TV_AMULET:
    case TV_RING:
    case TV_STAFF:
    case TV_WAND:
    case TV_ROD:
    case TV_SCROLL:
    case TV_POTION:
    case TV_FIGURINE:
        break;
    case TV_HAFTED:
        if(obj->sval == SV_WIZSTAFF) break;
        else return FALSE;
    case TV_LIGHT:
        return obj_is_specified_art(obj, "~.Sorcery");
    default:
        return FALSE;
    }
    return _will_buy(obj);
}

static bool _magic_stock_p(int k_idx)
{
    if (!_stock_p(k_idx))
        return FALSE;
    switch (k_info[k_idx].tval)
    {
    case TV_WAND:
    case TV_STAFF:
    case TV_FIGURINE:
    case TV_ARCANE_BOOK:
    case TV_SORCERY_BOOK:
        return TRUE;
    }
    return FALSE;
}

static bool _magic_create(obj_ptr obj, u32b mode)
{
    int k_idx;
    if (one_in_(20))
    {
        /* Hack: Early resists are hard to find, and Archviles are so damn nasty!
           BTW, since we are cheating and not using normal ego generation code, we'll
           need to manually add to ego_type.xtra_flags. This will improve the
           player's lore experience should they purchase or examine this item
           of stock. */
        if (one_in_(5))
        {
            object_prep(obj, lookup_kind(TV_AMULET, 0));
            obj->name2 = EGO_JEWELRY_ELEMENTAL;
        }
        else
        {
            object_prep(obj, lookup_kind(TV_RING, 0));
            obj->name2 = EGO_JEWELRY_ELEMENTAL;
        }
        switch (randint1(5))
        {
        case 1: case 2:
            add_flag(obj->flags, OF_RES_(GF_COLD));
            add_flag(e_info[EGO_JEWELRY_ELEMENTAL].xtra_flags, OF_RES_(GF_COLD));
            break;
        case 3: case 4:
            add_flag(obj->flags, OF_RES_(GF_FIRE));
            add_flag(e_info[EGO_JEWELRY_ELEMENTAL].xtra_flags, OF_RES_(GF_FIRE));
            break;
        case 5:
            add_flag(obj->flags, OF_RES_(GF_ACID));
            add_flag(e_info[EGO_JEWELRY_ELEMENTAL].xtra_flags, OF_RES_(GF_ACID));
            break;
        }
        obj->ident |= IDENT_STORE;
        return TRUE;
    }
    else if (one_in_(20))
    {
        /* For "The Desolation of Smaug", an early source of Levitation is required */
        if (prace_is_(RACE_MON_HYDRA))
        {
            object_prep(obj, lookup_kind(TV_AMULET, 0));
            obj->name2 = EGO_AMULET_MAGI;
            add_flag(e_info[EGO_AMULET_MAGI].xtra_flags, OF_LEVITATION);
        }
        else
        {
            object_prep(obj, lookup_kind(TV_RING, 0));
            obj->name2 = EGO_RING_WIZARDRY;
            add_flag(e_info[EGO_RING_WIZARDRY].xtra_flags, OF_LEVITATION);
        }
        add_flag(obj->flags, OF_LEVITATION);
        obj->ident |= IDENT_STORE;
        return TRUE;
    }
    else
        k_idx = _get_k_idx(_magic_stock_p, _mod_lvl(20));
    return _create(obj, k_idx, _mod_lvl(15), mode);
}

/************************************************************************
 * The Black Market
 ***********************************************************************/
static bool _black_market_will_buy(obj_ptr obj)
{
    return _will_buy(obj);
}

static bool _black_market_stock_p(int k_idx)
{
    object_kind *k_ptr = &k_info[k_idx];

    if (k_ptr->gen_flags & OFG_INSTA_ART) return FALSE;

    switch (k_ptr->tval)
    {
    case TV_CHEST: return FALSE;
    case TV_WAND:
    case TV_ROD:
    case TV_STAFF: return one_in_(3);
    }

    return !(k_ptr->gen_flags & OFG_TOWN);
}

static bool _black_market_create(obj_ptr obj, u32b mode)
{
    int spread = plr_prorata_level(50);
    int l1 = 20 + spread/4 + randint0(3*spread/4);
    int l2 = 20 + spread/4 + randint0(3*spread/4);
    int k_idx;

    if (one_in_(9))
    {
        k_idx = lookup_kind(TV_BURGLARY_BOOK, randint0(2));
    }
    else
    {
        for (;;)
        {
            choose_obj_kind(0);
            k_idx = _get_k_idx(get_obj_num_hook, _mod_lvl(l1));
            if (_black_market_stock_p(k_idx)) break;
        }
    }
    if (!_create(obj, k_idx, _mod_lvl(l2), mode)) return FALSE;
    if (obj_value(obj) < 10) return FALSE;
    if (object_is_nameless(obj) && !object_is_rare(obj) && obj_is_wearable(obj))
    {
        if (obj->to_a <= 0 && obj->to_h <= 0 && obj->to_d <= 0)
            return FALSE;
    }
    return TRUE;
}

/************************************************************************
 * The Bookstore
 ***********************************************************************/
static bool _book_will_buy(obj_ptr obj)
{
    switch (obj->tval)
    {
    case TV_SORCERY_BOOK:
    case TV_NATURE_BOOK:
    case TV_CHAOS_BOOK:
    case TV_DEATH_BOOK:
    case TV_LIFE_BOOK:
    case TV_TRUMP_BOOK:
    case TV_ARCANE_BOOK:
    case TV_CRAFT_BOOK:
    case TV_DAEMON_BOOK:
    case TV_CRUSADE_BOOK:
    case TV_NECROMANCY_BOOK:
    case TV_ARMAGEDDON_BOOK:
    case TV_ILLUSION_BOOK:
    case TV_MUSIC_BOOK:
    case TV_HEX_BOOK:
    case TV_BLESS_BOOK:
        break;
    default:
        return FALSE;
    }
    return _will_buy(obj);
}

static bool _book_stock_p(int k_idx)
{
    if (!_stock_p(k_idx))
        return FALSE;
    switch (k_info[k_idx].tval)
    {
    case TV_ARCANE_BOOK:
    case TV_SORCERY_BOOK:
    case TV_NATURE_BOOK:
    case TV_CHAOS_BOOK:
    case TV_DEATH_BOOK:
    case TV_TRUMP_BOOK:
    case TV_CRAFT_BOOK:
    case TV_DAEMON_BOOK:
    case TV_MUSIC_BOOK:
    case TV_HEX_BOOK:
    case TV_NECROMANCY_BOOK:
    case TV_ARMAGEDDON_BOOK:
    case TV_ILLUSION_BOOK:
        return TRUE;
    }
    return FALSE;
}

static bool _book_create(obj_ptr obj, u32b mode)
{
    int k_idx = _get_k_idx(_book_stock_p, _mod_lvl(20));
    return _create(obj, k_idx, _mod_lvl(rand_range(1, 5)), mode);
}

/************************************************************************
 * The Jeweler
 ***********************************************************************/
static bool _jeweler_will_buy(obj_ptr obj)
{
    if (obj->tval != TV_RING && obj->tval != TV_AMULET) return FALSE;
    return _will_buy(obj);
}

static bool _jeweler_stock_p(int k_idx)
{
    switch (k_info[k_idx].tval)
    {
    case TV_RING:
    case TV_AMULET:
        return TRUE;
    }
    return FALSE;
}

static bool _jeweler_create(obj_ptr obj, u32b mode)
{
    int spread = plr_prorata_level(50);
    int l1 = 20 + spread/4 + randint0(3*spread/4);
    int l2 = 20 + spread/4 + randint0(3*spread/4);
    int k_idx = _get_k_idx(_jeweler_stock_p, _mod_lvl(l1));
    return _create(obj, k_idx, _mod_lvl(l2), mode);
}

/************************************************************************
 * Shops
 ***********************************************************************/
static _owner_ptr _shop_choose_owner(shop_ptr shop, bool filter)
{
    int i, tot = 0, pick;
    for (i = 0; ; i++)
    {
        _owner_ptr owner = &shop->type->owners[i];
        if (!owner->name) break;
        if (filter && shop->town->owner_race_p && !shop->town->owner_race_p(owner->race_id)) continue;
        if (owner == shop->owner) continue;
        if (owner->active) continue;
        tot++;
    }
    if (!tot) return NULL;
    pick = randint0(tot);
    for (i = 0; ; i++)
    {
        _owner_ptr owner = &shop->type->owners[i];
        if (!owner->name) break;
        if (filter && shop->town->owner_race_p && !shop->town->owner_race_p(owner->race_id)) continue;
        if (owner == shop->owner) continue;
        if (owner->active) continue;
        if (--pick < 0) return owner;
    }
    return NULL;
}
static void _shop_change_owner(shop_ptr shop, bool msg)
{
    _owner_ptr owner = _shop_choose_owner(shop, TRUE);
    if (!owner)
        owner = _shop_choose_owner(shop, FALSE);

    if (shop->owner)
    {
        if (msg)
            msg_format("<color:U>%s</color> retires.", shop->owner->name);
        shop->owner->active = FALSE;
    }
    shop->owner = owner;
    shop->owner->active = TRUE;
}
static shop_ptr shop_alloc(town_ptr town, int shop_id)
{
    shop_ptr shop = malloc(sizeof(shop_t));
    shop->type = _get_shop_type(shop_id);
    shop->owner = NULL;
    shop->inv = inv_alloc(shop->type->name, INV_SHOP, 0);
    shop->town = town;
    _shop_change_owner(shop, FALSE);
    shop_reset(shop);
    return shop;
}
shop_ptr shop_load(savefile_ptr file)
{
    shop_ptr shop = malloc(sizeof(shop_t));
    int      tmp;
    u32b     guard;

    tmp = savefile_read_s16b(file);
    shop->type = _get_shop_type(tmp);
    assert(shop->type);

    tmp = savefile_read_s16b(file);
    shop->owner = _get_owner(shop->type->owners, tmp);
    assert(shop->owner);
    shop->owner->active = TRUE;

    shop->inv = inv_alloc(shop->type->name, INV_SHOP, 0);
    inv_load(shop->inv, file);

    shop->last_restock.turn = savefile_read_s32b(file);
    shop->last_restock.level = savefile_read_s16b(file);
    shop->last_restock.exp = savefile_read_s32b(file);

    guard = savefile_read_u32b(file);
    assert(guard == 0xFEEDFEED);
    if (guard != 0xFEEDFEED) quit("Corrupted savefile in shop_load");

    return shop;
}
void shop_free(shop_ptr shop)
{
    if (shop)
    {
        inv_free(shop->inv);
        shop->inv = NULL;
        shop->type = NULL;
        if (shop->owner)
            shop->owner->active = FALSE;
        shop->owner = NULL;
        free(shop);
    }
}
void shop_reset(shop_ptr shop)
{
    inv_clear(shop->inv);
    shop->last_restock.turn = 0;
    shop->last_restock.level = 0;
    shop->last_restock.exp = 0;
}
void shop_save(shop_ptr shop, savefile_ptr file)
{
    savefile_write_s16b(file, shop->type->id);
    savefile_write_s16b(file, shop->owner->id);
    inv_save(shop->inv, file);
    savefile_write_s32b(file, shop->last_restock.turn);
    savefile_write_s16b(file, shop->last_restock.level);
    savefile_write_s32b(file, shop->last_restock.exp);
    savefile_write_u32b(file, 0xFEEDFEED);
}

/************************************************************************
 * Pricing
 * Note: All functions take the point of view of the *shop*, not 
 *       the player. So _buy is the shop buying or the player
 *       selling. This is appropriate for a shop module!
 ***********************************************************************/
static int _price_factor_aux(int greed)
{
    int factor;

    factor = get_race()->shop_adjust;
    if (factor == 0)
        factor = 110;

    factor = (factor * adj_gold[plr->stat_ind[A_CHR]] + 50) / 100;
    factor = (factor * (135 - MIN(200, plr->fame)/4) + 50) / 100;
    factor = (factor * greed + 50) / 100;

    return factor;
}

static int _price_factor(_owner_ptr owner)
{
    int factor = _price_factor_aux(owner->greed);

    if (prace_is_(owner->race_id))
        factor = factor * 90 / 100;

    return factor;
}

static int _sell_price_aux(int price, int factor)
{
    if (factor < 100)
        factor = 100;

    if (price > 1000*1000)
        price = (price / 100) * factor;
    else
        price = (price * factor + 50) / 100;

    if (price > 1000)
        price = big_num_round(price, 3);

    return price;
}

static int _sell_price(shop_ptr shop, int price)
{
    int factor = _price_factor(shop->owner);

    price = _sell_price_aux(price, factor);
    if (shop->type->id == SHOP_BLACK_MARKET)
    {
        if (plr->realm1 != REALM_BURGLARY && !mut_present(MUT_BLACK_MARKETEER))
            price = price * 2;

        price = price * (625 + virtue_current(VIRTUE_JUSTICE)) / 625;
    }
    else if (shop->type->id == SHOP_JEWELER)
        price = price * 2;

    return price;
}

static int _bldg_price(bldg_ptr bldg, int price)
{
    int factor = _price_factor(bldg->owner);
    price = _sell_price_aux(price, factor);
    if (bldg->type->guild_p())
        price = (price + 1) / 2;
    return price;
}
static int _bldg_price2(bldg_ptr bldg, int guild_price, int outsider_price)
{
    int factor = _price_factor(bldg->owner);
    int price = bldg->type->guild_p() ? guild_price : outsider_price;
    return _sell_price_aux(price, factor);
}

static int _buy_price_aux(int price, int factor)
{
    if (factor < 105)
        factor = 105;

    if (price > 1000*1000)
        price = (price / factor) * 100;
    else
        price = (price * 100) / factor;

    if (price > 1000)
        price = big_num_round(price, 3);

    return price;
}

static int _buy_price(shop_ptr shop, int price)
{
    int factor = _price_factor(shop->owner);

    price = _buy_price_aux(price, factor);
    if (shop->type->id == SHOP_BLACK_MARKET)
    {
        if (plr->realm1 != REALM_BURGLARY && !mut_present(MUT_BLACK_MARKETEER))
            price = price / 2;

        price = price * (625 - virtue_current(VIRTUE_JUSTICE)) / 625;
    }
    else if (shop->type->id == SHOP_JEWELER)
        price = price / 2;

    if (price > shop->owner->purse)
        price = shop->owner->purse;

    return MAX(1, price);
}

/************************************************************************
 * User Interface
 ***********************************************************************/
struct _ui_context_s
{
    shop_ptr shop;
    slot_t   top;
    int      page_size;
    doc_ptr  doc;
};
typedef struct _ui_context_s _ui_context_t, *_ui_context_ptr;

static void _shop_display(_ui_context_ptr context);
static void _buy(_ui_context_ptr context);
static void _examine(_ui_context_ptr context);
static void _sell(_ui_context_ptr context);
static void _sellout(shop_ptr shop);
static void _reserve(_ui_context_ptr context);
static void _shop_ui(_ui_context_ptr context);

static void _maintain(shop_ptr shop);
static int  _cull(shop_ptr shop, int target);
static int  _restock(shop_ptr shop, int target);
static void _wizard_stock(shop_ptr shop);
static void _shuffle_stock(shop_ptr shop);
static int  _stock_base(shop_ptr shop);

void shop_ui(shop_ptr shop)
{
    _ui_context_t context = {0};

    store_hack = TRUE;
    _maintain(shop);

    context.shop = shop;
    context.top = 1;
    _shop_ui(&context);
    store_hack = FALSE;
}

static void _shop_ui(_ui_context_ptr context)
{
    character_icky = TRUE;

    msg_line_clear();
    msg_line_init(ui_shop_msg_rect());

    Term_clear();
    context->doc = doc_alloc(MIN(80, ui_shop_rect().cx));
    for (;;)
    {
        int    max = inv_last(context->shop->inv, obj_exists);
        rect_t r = ui_shop_rect(); /* recalculate in case resize */
        int    cmd, ct;

        context->page_size = MIN(26, r.cy - 3 - 6);
        if ((context->top - 1) % context->page_size != 0) /* resize?? */
            context->top = 1;

        _shop_display(context);

        cmd = inkey_special(TRUE);
        msg_line_clear();
        msg_boundary(); /* turn_count is unchanging while in home/museum */
        if (cmd == ESCAPE || cmd == 'q' || cmd == 'Q') break;
        pack_lock();
        if (!shop_common_cmd_handler(cmd))
        {
            switch (cmd) /* cmd is from the player's perspective */
            {
            case 'g': case 'b': case 'p': _sell(context); break;
            case 'B': _sellout(context->shop); break;
            case 'd': case 's': _buy(context); break;
            case 'x': _examine(context); break;
            case 'S': _shuffle_stock(context->shop); break;
            case 'R': _reserve(context); break;
            case KTRL('A'):
                if (plr->wizard) _wizard_stock(context->shop);
            case KTRL('S'):
                if (plr->wizard) inv_sort(context->shop->inv);
                break;
            case KTRL('O'):
                if (plr->wizard) _shop_change_owner(context->shop, TRUE);
                break;
            case '?':
                doc_display_help("context_shop.txt", NULL);
                Term_clear_rect(ui_shop_msg_rect());
                break;
            case SKEY_PGDOWN: case '3': case ' ':
                if (context->top + context->page_size - 1 < max)
                    context->top += context->page_size;
                break;
            case SKEY_PGUP: case '9': case '-':
                if (context->top > context->page_size)
                    context->top -= context->page_size;
                break;
            default:
                if (cmd < 256 && isprint(cmd))
                {
                    msg_format("Unrecognized command: <color:R>%c</color>. "
                               "Press <color:keypress>?</color> for help.", cmd);
                }
                else if (KTRL('A') <= cmd && cmd <= KTRL('Z'))
                {
                    cmd |= 0x40;
                    msg_format("Unrecognized command: <color:R>^%c</color>. "
                               "Press <color:keypress>?</color> for help.", cmd);
                }
            }
            ct = inv_count_slots(context->shop->inv, obj_exists);
            if (!ct)
            {
                _restock(context->shop, _stock_base(context->shop));
                context->top = 1;
                if (one_in_(20)) _shop_change_owner(context->shop, TRUE);
                msg_format("<color:U>%s</color> brings out some new stock.", context->shop->owner->name);
            }
            else
            {
                max = inv_last(context->shop->inv, obj_exists);
                while (context->top > max)
                    context->top -= context->page_size;
                if (context->top < 1) context->top = 1;
            }
        }
        pack_unlock();
        notice_stuff(); /* PW_INVEN and PW_PACK ... */
        handle_stuff(); /* Plus 'C' to view character sheet */
        if (pack_overflow_count())
        {
            msg_print("<color:v>Your pack is overflowing!</color> It's time for you to leave!");
            msg_print(NULL);
            break;
        }
    }
    character_icky = FALSE;
    energy_use = 100;
    msg_line_clear();
    msg_line_init(ui_msg_rect());

    Term_clear();
    do_cmd_redraw();

    doc_free(context->doc);
}

static void _display_inv(doc_ptr doc, shop_ptr shop, slot_t top, int page_size);
static void _shop_display(_ui_context_ptr context)
{
    rect_t   r = ui_shop_rect();
    doc_ptr  doc = context->doc;
    shop_ptr shop = context->shop;
    int      ct = strlen(shop->type->name) + 10; /* " (20000gp)" */
    char     buf[10];

    doc_clear(doc);
    doc_insert(doc, "<style:table>");
    doc_printf(doc, "    <color:U>%s (%s)</color>",
        shop->owner->name, plr_race_aux(shop->owner->race_id, 0)->name);
    doc_printf(doc, "<tab:%d><color:G>%s</color> (<color:r>%d</color>)\n\n",
        doc_width(doc) - ct, shop->type->name, shop->owner->purse);

    _display_inv(doc, shop, context->top, context->page_size);
    
    {
        slot_t max = inv_last(shop->inv, obj_exists);
        slot_t bottom = context->top + context->page_size - 1;

        if (context->top > 1 || bottom < max)
        {
            int page_count = (max - 1) / context->page_size + 1;
            int page_current = (context->top - 1) / context->page_size + 1;

            doc_printf(doc, "<color:B>(Page %d of %d)</color>\n", page_current, page_count);
        }
    }

    big_num_display(plr->au, buf);
    doc_printf(doc, "Gold Remaining: <color:y>%s</color>\n\n", buf);
    doc_insert(doc, "<color:keypress>b</color> to buy. ");
    doc_insert(doc, "<color:keypress>s</color> to sell. ");
    doc_insert(doc, 
        "<color:keypress>x</color> to begin examining items.\n"
        "<color:keypress>B</color> to buyout inventory. ");

    if (mut_present(MUT_MERCHANTS_FRIEND))
    {
        doc_insert(doc,
            "<color:keypress>S</color> to shuffle stock. "
            "<color:keypress>R</color> to reserve an item.");
    }
    doc_newline(doc);

    doc_insert(doc,
        "<color:keypress>Esc</color> to exit. "
        "<color:keypress>?</color> for help.");
    doc_insert(doc, "</style>");

    Term_clear_rect(r);
    doc_sync_term(doc,
        doc_range_top_lines(doc, r.cy),
        doc_pos_create(r.x, r.y));
}

static int _add_obj(shop_ptr shop, obj_ptr obj);
static bool _buy_aux(shop_ptr shop, obj_ptr obj)
{
    char       name[MAX_NLEN];
    str_ptr s = str_alloc();
    char       c;
    int        price = obj_value(obj);

    if (!price)
    {
        msg_print("I have no interest in your junk!");
        return FALSE;
    }
    price = _buy_price(shop, price);
    price *= obj->number;

    object_desc(name, obj, OD_COLOR_CODED);
    str_printf(s, "Really sell %s for <color:R>%d</color> gp? <color:y>[y/n]</color>", name, price);
    c = msg_prompt(str_buffer(s), "ny", PROMPT_YES_NO);
    str_free(s);
    if (c == 'n') return FALSE;

    plr->au += price;
    stats_on_gold_selling(price);

    plr->redraw |= PR_GOLD;
    if (prace_is_(RACE_MON_LEPRECHAUN))
        plr->update |= (PU_BONUS | PU_HP | PU_MANA);

    obj->inscription = 0;
    obj->feeling = FEEL_NONE;
    obj->marked &= ~OM_WORN;
    obj->timeout = 0;

    obj_identify_fully(obj);
    stats_on_purchase(obj);

    /* This message may seem like spam, but it is not. Selling an
     * un-identified potion of augmentation, for example. */
    object_desc(name, obj, OD_COLOR_CODED);
    msg_format("You sold %s for <color:R>%d</color> gold.", name, price);

    if (shop->type->id == SHOP_BLACK_MARKET)
        virtue_add(VIRTUE_JUSTICE, -1);

    if (obj->tval == TV_BOTTLE)
        virtue_add(VIRTUE_NATURE, 1);

    if (object_is_(obj, TV_POTION, SV_POTION_BLOOD))
    {
        msg_print("The potion goes sour.");
        obj->sval = SV_POTION_SALT_WATER;
        obj->k_idx = lookup_kind(TV_POTION, SV_POTION_SALT_WATER);
    }

    price = obj_value(obj); /* correctly handle unidentified items */
    if (price > 0 && _add_obj(shop, obj))
        inv_sort(shop->inv);
    else
        obj->number = 0;
    return TRUE;
}

static void _buy(_ui_context_ptr context)
{
    obj_prompt_t prompt = {0};
    int          amt = 1;

    prompt.prompt = "Sell which item?";
    prompt.error = "You have nothing to sell.";
    prompt.filter = context->shop->type->buy_p;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_QUIVER;

    command_cmd = 's'; /* Hack for !s inscriptions */
    obj_prompt(&prompt);
    command_cmd = '\0';
    if (!prompt.obj) return;

    if (prompt.obj->number > 1)
    {
        amt = prompt.obj->number;
        if (!msg_input_num("Quantity", &amt, 1, prompt.obj->number)) return;
    }

    if (amt < prompt.obj->number)
    {
        obj_t copy = *prompt.obj;
        copy.number = amt;
        if (_buy_aux(context->shop, &copy))
        {
            obj_identify_fully(prompt.obj);
            prompt.obj->number -= amt;
            prompt.obj->marked |= OM_DELAYED_MSG;
            plr->notice |= PN_CARRY;
            if (prompt.obj->loc.where == INV_QUIVER)
                plr->notice |= PN_OPTIMIZE_QUIVER;
            else if (prompt.obj->loc.where == INV_PACK)
                plr->notice |= PN_OPTIMIZE_PACK;
        }
    }
    else
        _buy_aux(context->shop, prompt.obj);

    obj_release(prompt.obj, OBJ_RELEASE_QUIET);
}

static void _examine(_ui_context_ptr context)
{
    for (;;)
    {
        char    cmd;
        slot_t  slot;
        obj_ptr obj;

        if (!msg_command("<color:y>Examine which item <color:w>(<color:keypress>Esc</color> when done)</color>?</color>", &cmd)) break;
        if (cmd < 'a' || cmd > 'z') continue;
        slot = label_slot(cmd);
        slot = slot + context->top - 1;
        obj = inv_obj(context->shop->inv, slot);
        if (!obj) continue;

        obj_learn_store(obj);
        obj_display(obj);
    }
}

static void _reserve_aux(shop_ptr shop, obj_ptr obj)
{
    int        cost = _sell_price(shop, 10000);
    str_ptr s;
    char       c;
    char       name[MAX_NLEN];

    object_desc(name, obj, OD_COLOR_CODED);
    s = str_alloc_format("Reserve %s for <color:R>%d</color> gp? <color:y>[y/n]</color>", name, cost);
    c = msg_prompt(str_buffer(s), "ny", PROMPT_YES_NO);
    str_free(s);
    if (c == 'n') return;
    if (cost > plr->au)
    {
        msg_print("You don't have enough gold.");
        return;
    }
    plr->au -= cost;
    stats_on_gold_services(cost);

    plr->redraw |= PR_GOLD;
    if (prace_is_(RACE_MON_LEPRECHAUN))
        plr->update |= (PU_BONUS | PU_HP | PU_MANA);

    obj->marked |= OM_RESERVED;
    msg_format("Done! I'll hold on to %s for you. You may come back at any time to purchase it.", name);
}

static void _reserve(_ui_context_ptr context)
{
    if (plr->wizard || mut_present(MUT_MERCHANTS_FRIEND))
    {
        for (;;)
        {
            char    cmd;
            slot_t  slot;
            obj_ptr obj;

            if (!msg_command("<color:y>Reserve which item <color:w>(<color:keypress>Esc</color> when done)</color>?</color>", &cmd)) break;
            if (cmd < 'a' || cmd > 'z') continue;
            slot = label_slot(cmd);
            slot = slot + context->top - 1;
            obj = inv_obj(context->shop->inv, slot);
            if (!obj) continue;

            if (obj->marked & OM_RESERVED)
            {
                msg_print("You have already reserved that item. Choose another.");
                continue;
            }
            _reserve_aux(context->shop, obj);
            break;
        }
    }
    else
        msg_print("I will only reserve items in my stock for wizards or true friends of the merchant's guild.");
}

static bool _sell_aux(shop_ptr shop, obj_ptr obj)
{
    char       name[MAX_NLEN];
    str_ptr s = str_alloc();
    char       c;
    int        price = obj_value(obj);

    price = _sell_price(shop, price);
    price *= obj->number;

    object_desc(name, obj, OD_COLOR_CODED);
    str_printf(s, "Really buy %s for <color:R>%d</color> gp? <color:y>[y/n]</color>", name, price);
    c = msg_prompt(str_buffer(s), "ny", PROMPT_YES_NO);
    str_free(s);
    if (c == 'n') return FALSE;

    if (price > plr->au)
    {
        msg_print("You do not have enough gold.");
        return FALSE;
    }
    plr->au -= price;
    stats_on_gold_buying(price);

    plr->redraw |= PR_GOLD;
    if (prace_is_(RACE_MON_LEPRECHAUN))
        plr->update |= (PU_BONUS | PU_HP | PU_MANA);

    obj->ident &= ~IDENT_STORE;
    obj->inscription = 0;
    obj->feeling = FEEL_NONE;
    obj->marked &= ~OM_RESERVED;

    obj_identify_fully(obj);
    stats_on_purchase(obj);

    if (shop->type->id == SHOP_BLACK_MARKET)
        virtue_add(VIRTUE_JUSTICE, -1);

    if (obj->tval == TV_BOTTLE)
        virtue_add(VIRTUE_NATURE, -1);

    pack_carry(obj);
    /*msg_format("You have %s.", name);*/
    return TRUE;
}

static void _sell(_ui_context_ptr context)
{
    for (;;)
    {
        char    cmd;
        slot_t  slot;
        obj_ptr obj;
        int     amt = 1;

        if (!msg_command("<color:y>Buy which item <color:w>(<color:keypress>Esc</color> "
                         "to cancel)</color>?</color>", &cmd)) break;
        if (cmd < 'a' || cmd > 'z') continue;
        slot = label_slot(cmd);
        slot = slot + context->top - 1;
        obj = inv_obj(context->shop->inv, slot);
        if (!obj) continue;

        if (obj->number > 1)
        {
            if (!msg_input_num("Quantity", &amt, 1, obj->number)) continue;
        }

        if (amt < obj->number)
        {
            obj_t copy = *obj;
            copy.number = amt;
            if (_sell_aux(context->shop, &copy))
                obj->number -= amt;
        }
        else
        {
            _sell_aux(context->shop, obj);
            if (!obj->number)
            {
                inv_remove(context->shop->inv, slot);
                inv_sort(context->shop->inv);
            }
        }
        break;
    }
}

static void _sellout(shop_ptr shop)
{
    slot_t slot, max = inv_last(shop->inv, obj_exists);
    int    price, total_price = 0;

    if (1 && !get_check("Are you sure you want to buy the entire inventory of this store? "))
        return;

    for (slot = 1; slot <= max; slot++)
    {
        obj_ptr obj = inv_obj(shop->inv, slot);
        if (!obj) continue;
        price = _sell_price(shop, obj_value(obj));
        price *= obj->number;
        if (price <= plr->au)
        {
            bool destroy = FALSE;
            int auto_pick_idx = is_autopick(obj);

            if (auto_pick_idx >= 0 && autopick_list[auto_pick_idx].action & DO_AUTODESTROY)
                destroy = TRUE;

            obj->ident &= ~IDENT_STORE;
            obj->inscription = 0;
            obj->feeling = FEEL_NONE;
            obj->marked &= ~OM_RESERVED;

            obj_identify_fully(obj);

            if (!destroy)
            {
                pack_carry(obj);
                stats_on_purchase(obj);
            }
            if (shop->type->id == SHOP_BLACK_MARKET)
                virtue_add(VIRTUE_JUSTICE, -1);

            if (obj->tval == TV_BOTTLE)
                virtue_add(VIRTUE_NATURE, -1);

            obj->number = 0;
            inv_remove(shop->inv, slot);

            plr->au -= price;
            total_price += price;
            stats_on_gold_buying(price);

            plr->redraw |= PR_GOLD;
        }
        else
        {
            msg_print("You have run out of gold.");
            break;
        }
    }

    msg_format("You spent <color:R>%d</color> gp.", total_price);
    if (prace_is_(RACE_MON_LEPRECHAUN))
        plr->update |= (PU_BONUS | PU_HP | PU_MANA);

    inv_sort(shop->inv);
}

/************************************************************************
 * Stocking
 ***********************************************************************/
#define _STOCK_LO   6
#define _STOCK_BASE 15
#define _STOCK_HI   24

static int _stock_base(shop_ptr shop)
{
    if (shop->type->id == SHOP_GENERAL)
        return 10 - 2 + randint1(4);
    return _STOCK_BASE - 4 + randint1(8);
}

static void _maintain(shop_ptr shop)
{
    int  num;
    int  i;
    bool allow_restock = TRUE;

    /* Always initialize an empty shop */
    if (!inv_count_slots(shop->inv, obj_exists))
    {
        _restock(shop, _stock_base(shop));
        return;
    }

    /* Shops maintain once per day */
    num = MIN(10, (dun_mgr()->turn - shop->last_restock.turn) / TOWN_DAWN);
    if (!num) return;

    /* Limit shop scumming (ie resting in town or on DL1 for BM wares) */
    if (!_shop_is_basic(shop))
    {
        if (shop->last_restock.turn)
        {
            int xp = shop->last_restock.exp;
            xp += MIN(MAX(xp / 20, 1000), 100000);
            if ( plr->max_plv <= shop->last_restock.level
              && plr->max_exp <= xp
              && plr->prace != RACE_ANDROID )
            {
                allow_restock = FALSE;
            }
        }
    }

    /* Maintain the shop for each day since last visit */
    for (i = 0; i < num; i++)
    {
        int ct = inv_count_slots(shop->inv, obj_exists);
        if (ct < _STOCK_LO) _restock(shop, _stock_base(shop));
        else if (ct > _STOCK_HI) _cull(shop, _stock_base(shop));
        else
        {
            ct = _cull(shop, MAX(_STOCK_LO, ct - randint1(9)));
            if (allow_restock)
                ct = _restock(shop, MIN(_STOCK_HI, ct + randint1(9)));
        }
    }
}

static bool _can_cull(obj_ptr obj)
{
    if (obj && !(obj->marked & OM_RESERVED)) return TRUE;
    return FALSE;
}
static int _cull(shop_ptr shop, int target)
{
    int ct = inv_count_slots(shop->inv, obj_exists);
    int attempt;

    assert(ct >= target);
    for (attempt = 1; ct > target && attempt < 100; attempt++)
    {
        slot_t  slot = inv_random_slot(shop->inv, _can_cull);
        obj_ptr obj;

        if (!slot) break; /* nothing but 'Reserved' objects remain */

        obj = inv_obj(shop->inv, slot);
        assert(obj->number > 0);
        assert (!(obj->marked & OM_RESERVED));

        if (one_in_(2))
            obj->number = (obj->number + 1)/2;
        else if (one_in_(2))
            obj->number--;
        else
            obj->number = 0;

        if (!obj->number)
        {
            inv_remove(shop->inv, slot);
            ct--;
        }
    }
    inv_sort(shop->inv);
    assert(ct == inv_count_slots(shop->inv, obj_exists));
    return ct;
}

static int _add_obj(shop_ptr shop, obj_ptr obj) /* return number of new slots used (0 or 1) */
{
    slot_t slot, max = inv_last(shop->inv, obj_exists);
    for (slot = 1; slot <= max; slot++)
    {
        obj_ptr dest = inv_obj(shop->inv, slot);
        if (!dest) continue;
        if (obj_can_combine(dest, obj, INV_SHOP))
        {
            obj_combine(dest, obj, INV_SHOP);
            obj->number = 0; /* forget spillover */
            return 0;
        }
    }
    inv_add(shop->inv, obj);
    return 1;
}

static void _wizard_stock(shop_ptr shop)
{
    u32b mode = AM_NO_FIXED_ART;

    if (plr_dun()->type->id == D_SURFACE && _shop_is_basic(shop))
        mode |=  AM_STOCK_TOWN;

    if (shop->type->id == SHOP_BLACK_MARKET)
        mode |= AM_STOCK_BM;

    wiz_create_objects(shop->type->create_f, mode);
    Term_clear_rect(ui_shop_msg_rect());
}

static int _restock(shop_ptr shop, int target)
{
    int ct = inv_count_slots(shop->inv, obj_exists);
    int attempt = 0;
    u32b mode = AM_NO_FIXED_ART;

    if (plr_dun()->type->id == D_SURFACE && _shop_is_basic(shop))
        mode |=  AM_STOCK_TOWN;

    if (shop->type->id == SHOP_BLACK_MARKET)
        mode |= AM_STOCK_BM;

    assert(ct <= target);
    for (attempt = 1; ct < target && attempt < 100; attempt++)
    {
        obj_t forge = {0};
        if (shop->type->create_f(&forge, mode))
        {
            assert(obj_value(&forge) > 0);
            ct += _add_obj(shop, &forge);
        }
    }
    inv_sort(shop->inv);
    assert(ct == inv_count_slots(shop->inv, obj_exists));
    shop->last_restock.turn = dun_mgr()->turn;
    shop->last_restock.level = plr->max_plv;
    shop->last_restock.exp = plr->max_exp;
    return ct;
}

static void _shuffle_stock(shop_ptr shop)
{
    if (plr->wizard || mut_present(MUT_MERCHANTS_FRIEND))
    {
        if (!plr->wizard)
        {
            int        cost = _sell_price(shop, 5000);
            str_ptr s;
            char       c;
            s = str_alloc_format("Shuffle stock for <color:R>%d</color> gp? <color:y>[y/n]</color>", cost);
            c = msg_prompt(str_buffer(s), "ny", PROMPT_YES_NO);
            str_free(s);
            if (c == 'n') return;
            if (cost > plr->au)
            {
                msg_print("You don't have enough gold.");
                return;
            }
            plr->au -= cost;
            stats_on_gold_services(cost);

            plr->redraw |= PR_GOLD;
            if (prace_is_(RACE_MON_LEPRECHAUN))
                plr->update |= (PU_BONUS | PU_HP | PU_MANA);
        }
        _cull(shop, 0);
        _restock(shop, _stock_base(shop));
    }
    else
        msg_print("I will only shuffle my stock for wizards or true friends of the merchant's guild.");
}

/************************************************************************
 * User Interface Helpers
 ***********************************************************************/
bool shop_common_cmd_handler(int cmd)
{
    switch (cmd)
    {
    case '\r':
        return TRUE;
    case 'w':
        equip_wield_ui();
        return TRUE;
    case 't': case 'T':
        equip_takeoff_ui();
        return TRUE;
    case 'k': case KTRL('D'):
        obj_destroy_ui();
        return TRUE;
    case 'e':
        equip_ui();
        return TRUE;
    case 'i':
        pack_ui();
        return TRUE;
    case 'I':
        obj_inspect_ui();
        return TRUE;
    case KTRL('I'):
        toggle_inven_equip();
        return TRUE;
    case '{':
        obj_inscribe_ui();
        return TRUE;
    case '}':
        obj_uninscribe_ui();
        return TRUE;
    case 'C':
        plr_display();
        return TRUE;
    case KTRL('W'):
        show_weights = !show_weights;
        return TRUE;
    case KTRL('G'):
        show_item_graph = !show_item_graph;
        return TRUE;
    }
    return FALSE;
}

static void _display_inv(doc_ptr doc, shop_ptr shop, slot_t top, int page_size)
{
    slot_t  slot;
    int     xtra = 0;
    char    name[MAX_NLEN];
    inv_ptr inv = shop->inv;
    bool    show_prices = inv_loc(inv) == INV_SHOP;
    bool    show_values = inv_loc(inv) != INV_SHOP || plr-> wizard;

    if (show_weights)
        xtra += 10;  /* " 123.0 lbs" */
    if (show_prices)
        xtra += 7;
    if (show_values)
        xtra += 7;

    doc_insert(doc, "    Item Description");
    if (xtra)
    {
        doc_printf(doc, "<tab:%d>", doc_width(doc) - xtra);
        if (show_weights)
            doc_printf(doc, " %9.9s", "Weight");
        if (show_prices)
            doc_printf(doc, " %6.6s", "Price");
        if (show_values)
            doc_printf(doc, " %6.6s", "Score");
    }
    doc_newline(doc);

    for (slot = top; slot < top + page_size; slot++)
    {
        obj_ptr obj = inv_obj(inv, slot);
        if (obj)
        {
            obj_ptr     next = inv_obj(inv, slot + 1);
            doc_style_t style = *doc_current_style(doc);
            char        label_color = 'w';

            if (next && obj_cmp(obj, next) > 0)
                label_color = 'r';

            object_desc(name, obj, OD_COLOR_CODED);

            doc_printf(doc, " <color:%c>%c</color>) ", label_color, slot_label(slot - top + 1));
            if (show_item_graph)
            {
                doc_insert_char(doc, object_attr(obj), object_char(obj));
                doc_insert(doc, " ");
            }
            if (xtra)
            {
                style.right = doc_width(doc) - xtra;
                doc_push_style(doc, &style);
            }
            doc_printf(doc, "%s", name);
            if (xtra)
            {
                doc_pop_style(doc);
                doc_printf(doc, "<tab:%d>", doc_width(doc) - xtra);

                if (show_weights)
                {
                    int wgt = obj->weight; /* single object only for home/shops */
                    doc_printf(doc, " %3d.%d lbs", wgt/10, wgt%10);
                }
                if (show_prices || show_values)
                {
                    int value = obj_value(obj);

                    if (show_prices)
                    {
                        int price = _sell_price(shop, value);
                        doc_printf(doc, " <color:%c>%6d</color>", price <= plr->au ? 'w' : 'D', price);
                    }
                    if (show_values)
                        doc_printf(doc, " %6d", value);
                }
            }
            doc_newline(doc);
        }
        else
            doc_newline(doc);
    }
}
void shop_display_inv(doc_ptr doc, inv_ptr inv, slot_t top, int page_size)
{
    shop_t hack = {0};
    hack.inv = inv;
    _display_inv(doc, &hack, top, page_size);
}

/************************************************************************
 * Building Types and Owners
 ***********************************************************************/
static bool _false(void) { return FALSE; }
static void _inn_display(bldg_ptr bldg, doc_ptr doc);
static bool _inn_command(bldg_ptr bldg, int cmd);
static void _fighter_display(bldg_ptr bldg, doc_ptr doc);
static bool _fighter_command(bldg_ptr bldg, int cmd);
static bool _fighter_guild(void);
static void _archer_display(bldg_ptr bldg, doc_ptr doc);
static bool _archer_command(bldg_ptr bldg, int cmd);
static bool _archer_guild(void);
static void _thief_display(bldg_ptr bldg, doc_ptr doc);
static bool _thief_command(bldg_ptr bldg, int cmd);
static bool _thief_guild(void);
static void _wizard_display(bldg_ptr bldg, doc_ptr doc);
static bool _wizard_command(bldg_ptr bldg, int cmd);
static bool _wizard_guild(void);
static void _priest_display(bldg_ptr bldg, doc_ptr doc);
static bool _priest_command(bldg_ptr bldg, int cmd);
static bool _priest_guild(void);
static void _hunter_display(bldg_ptr bldg, doc_ptr doc);
static bool _hunter_command(bldg_ptr bldg, int cmd);

static _owner_t _inn_owners[] = {
    {  1, "Mellon the Friendly",        200, 108, RACE_HIGH_ELF },
    {  2, "Bartleby the Rotund",        200, 110, RACE_HUMAN },
    {  3, "Dwalir Greasebeard",         200, 117, RACE_DWARF },
    {  4, "Butterbumps",                200, 109, RACE_GNOME },
    {  5, "The Jolly Giant",            200, 116, RACE_HALF_GIANT },
    {  6, "Balaak Everdrunk",           200, 111, RACE_HUMAN },
    {  7, "Snivels the Wretch",         200, 119, RACE_SNOTLING },
    {  8, "Weevils the Vile",           200, 117, RACE_HALF_TROLL },
    {  9, "Mungo Baggins",              200, 106, RACE_HOBBIT },
    { 10, "Turien the Fair",            200, 105, RACE_WOOD_ELF },
    { 11, "Elthien Greeneyes",          200, 106, RACE_WATER_ELF },
    { 12, "Celimos the Wayfarer",       200, 105, RACE_HIGH_ELF },
    { 13, "Drodo Elf-friend",           200, 107, RACE_HOBBIT },
    { 14, "Grimm the Hero",             200, 112, RACE_HUMAN },
    { 15, "Haldir Lightfoot",           200, 104, RACE_WOOD_ELF },
    {0}
};
static _owner_t _fighter_owners[] = {
    {  1, "Hektor the Amorous",         200, 109, RACE_DEMIGOD },
    {  2, "Krull the Conqueror",        200, 107, RACE_BARBARIAN },
    {  3, "Dal Dror, Demonsbane",       200, 119, RACE_DWARF },
    {  4, "Eomer",                      200, 111, RACE_HUMAN },
    {  5, "Grynwel Kinslayer",          200, 107, RACE_HUMAN },
    {  6, "Grendel's Mom",              200, 125, RACE_HALF_OGRE },
    {  7, "Hephaestus the Lame",        200, 106, RACE_DEMIGOD },
    {  8, "Borin Stoutbeard",           200, 120, RACE_DWARF },
    {  9, "Fengel the Greedy",          200, 117, RACE_HUMAN },
    { 10, "Elros the Swift",            200, 108, RACE_WOOD_ELF },
    { 11, "Myra Stillwater",            200, 106, RACE_WATER_ELF },
    { 12, "Magnus Maximus",             200, 112, RACE_HALF_TITAN },
    { 13, "Shalrok Orcsbane",           200, 107, RACE_DRACONIAN },
    { 14, "Torog the Tuff",             200, 118, RACE_HALF_TROLL },
    { 15, "Celimryl the Pure",          200, 104, RACE_HIGH_ELF },
    { 16, "Grond the Mighty",           200, 116, RACE_CYCLOPS },
    {0}
};
static _owner_t _archer_owners[] = {
    {  1, "Bard the Dragonslayer",      200, 109, RACE_HUMAN },
    {  2, "Legolas Longshot",           200, 107, RACE_WOOD_ELF },
    {  3, "Snagsby",                    200, 112, RACE_HUMAN },
    {  4, "Fenwyn Elfbow",              200, 106, RACE_HIGH_ELF },
    {  5, "Deadeye Grook",              200, 118, RACE_ZOMBIE },
    {  6, "Thendor the Unbeliever",     200, 108, RACE_BARBARIAN },
    {  7, "Dorin the Hunter",           200, 106, RACE_DUNADAN },
    {  8, "Birgita Silverbow",          200, 102, RACE_HUMAN },
    {  9, "Meliok Blackarrow",          200, 111, RACE_DARK_ELF },
    { 10, "Syrilla Stareyes",           200, 109, RACE_SPRITE },
    { 11, "Thedessa Nightshade",        200, 110, RACE_SHADOW_FAIRY },
    {0}
};
static _owner_t _thief_owners[] = {
    {  1, "Simion Lightfingers",        200, 118, RACE_HUMAN },
    {  2, "Belrok the Cutpurse",        200, 112, RACE_HUMAN },
    {  3, "Kryl the Handsome",          200, 117, RACE_KOBOLD },
    {  4, "Thendros of the Night",      200, 116, RACE_DARK_ELF },
    {  5, "Vadimir the Restorer",       200, 114, RACE_VAMPIRE },
    {  6, "Shadrok the Lame",           200, 115, RACE_BARBARIAN },
    {  7, "Nedrel the Nefarious",       200, 122, RACE_GNOME },
    {  8, "Nibrel the Honest",          200, 121, RACE_NIBELUNG },
    {  9, "Thestion the Guildmaster",   200, 118, RACE_HUMAN },
    { 10, "Celryl the Forsaken",        200, 120, RACE_WOOD_ELF },
    { 11, "Valtar the Cunning",         200, 113, RACE_HUMAN },
    {0}
};
static _owner_t _wizard_owners[] = {
    {  1, "Mentok the Dominator",       200, 117, RACE_MIND_FLAYER },
    {  2, "Kelios the Apprentice",      200, 112, RACE_HUMAN },
    {  3, "Etheryl the Immortal",       200, 106, RACE_HIGH_ELF },
    {  4, "Grastios Stargazer",         200, 109, RACE_HUMAN },
    {  5, "Delios the Ancient",         200, 111, RACE_MIND_FLAYER },
    {  6, "Kelrond, Master of Chaos",   200, 107, RACE_DUNADAN },
    {  7, "Yestor the Small",           200, 106, RACE_YEEK },
    {  8, "Thalrok the Undying",        200, 119, RACE_SKELETON },
    {  9, "Xortar, Master of Fire",     200, 118, RACE_IMP },
    { 10, "Malfarious Shadowrought",    200, 113, RACE_DARK_ELF },
    { 11, "Thenamir, Betrayer of Hope", 200, 117, RACE_HUMAN },
    { 12, "Alantin the Wise",           200, 112, RACE_HUMAN },
    { 13, "Althanos the Thaumaturge",   200, 109, RACE_HUMAN },
    { 14, "Vartilla the Worldsinger",   200, 107, RACE_HUMAN },
    { 15, "Malizondra the Voidbringer", 200, 125, RACE_DARK_ELF },
    { 16, "Silfranna Mirthwood",        200, 104, RACE_SPRITE },
    {0}
};
static _owner_t _priest_owners[] = {
    {  1, "Chiron",                     200, 108, RACE_CENTAUR },
    {  2, "Thesira the Gentle",         200, 109, RACE_HUMAN },
    {  3, "Dailwyn the Yellow",         200, 107, RACE_HIGH_ELF },
    {  4, "Arldin Goatsbeard",          200, 124, RACE_DWARF },
    {  5, "Bones",                      200, 114, RACE_SKELETON },
    {  6, "Zartok Braineater",          200, 121, RACE_ZOMBIE },
    {  7, "Danethor the Experimenter",  200, 109, RACE_DUNADAN },
    {  8, "Bailwyn the Golden",         200, 105, RACE_WOOD_ELF },
    {  9, "Celeste Nightflier",         200, 108, RACE_SHADOW_FAIRY },
    { 10, "Haldor Leachfriend",         200, 111, RACE_HUMAN },
    { 11, "Yenessa the Whisperer",      200, 107, RACE_YEEK },
    {0}
};
static _owner_t _hunter_owners[] = {
    {  1, "Yardin Deerslayer",          200, 109, RACE_HUMAN },
    {  2, "Vantak the Impaler",         200, 119, RACE_VAMPIRE },
    {  3, "Krenivar the Stalker",       200, 108, RACE_BARBARIAN },
    {  4, "Bordok the Savage",          200, 109, RACE_BARBARIAN },
    {  5, "Alemwyn Boarspear",          200, 107, RACE_WOOD_ELF },
    {  7, "Dundin Orcslayer",           200, 123, RACE_DWARF },
    {0}
};
static _bldg_type_t _bldg_types[] = {
    { BLDG_INN, "The Inn", _inn_display, _inn_command, _false, _inn_owners },
    { BLDG_FIGHTERS_GUILD, "Fighters Hall", _fighter_display, _fighter_command, _fighter_guild, _fighter_owners },
    { BLDG_ARCHERS_GUILD, "Archers Guild", _archer_display, _archer_command, _archer_guild, _archer_owners },
    { BLDG_THIEVES_GUILD, "Thieves Guild", _thief_display, _thief_command, _thief_guild, _thief_owners },
    { BLDG_WIZARDS_GUILD, "Wizards Tower", _wizard_display, _wizard_command, _wizard_guild, _wizard_owners },
    { BLDG_PRIESTS_GUILD, "Inner Temple", _priest_display, _priest_command, _priest_guild, _priest_owners },
    { BLDG_HUNTERS_OFFICE, "Bounty Hunter", _hunter_display, _hunter_command, _false, _hunter_owners },
    { BLDG_NONE }
};

static _bldg_type_ptr _get_bldg_type(int which)
{
    int i;
    for (i = 0;; i++)
    {
        _bldg_type_ptr type = &_bldg_types[i];
        if (type->id == BLDG_NONE) return NULL;
        if (type->id == which) return type;
    }
}

static _owner_ptr _bldg_choose_owner(bldg_ptr bldg, bool filter)
{
    int i, tot = 0, pick;
    for (i = 0; ; i++)
    {
        _owner_ptr owner = &bldg->type->owners[i];
        if (!owner->name) break;
        if (filter && bldg->town->owner_race_p && !bldg->town->owner_race_p(owner->race_id)) continue;
        if (owner == bldg->owner) continue;
        if (owner->active) continue;
        tot++;
    }
    if (!tot) return NULL;
    pick = randint0(tot);
    for (i = 0; ; i++)
    {
        _owner_ptr owner = &bldg->type->owners[i];
        if (!owner->name) break;
        if (filter && bldg->town->owner_race_p && !bldg->town->owner_race_p(owner->race_id)) continue;
        if (owner == bldg->owner) continue;
        if (owner->active) continue;
        if (--pick < 0) return owner;
    }
    return NULL;
}

static void _bldg_change_owner(bldg_ptr bldg, bool msg)
{
    _owner_ptr owner = _bldg_choose_owner(bldg, TRUE);
    if (!owner)
        owner = _bldg_choose_owner(bldg, FALSE);

    if (bldg->owner)
    {
        if (msg)
            msg_format("<color:U>%s</color> retires.", bldg->owner->name);
        bldg->owner->active = FALSE;
    }
    bldg->owner = owner;
    bldg->owner->active = TRUE;
}

static void _bldg_display_aux(char cmd, cptr desc, int price, doc_ptr doc)
{
    doc_printf(doc, " <color:%c>%c</color>) %s", price > plr->au ? 'D' : 'y', cmd, desc);
    if (price)
    {
        doc_printf(doc, "<tab:%d><color:%c>%6d</color>",
            doc_width(doc) - 6, price > plr->au ? 'D' : 'w', price);
    }
    doc_newline(doc);
}

static void _plr_pay(int price)
{
    plr->au -= price;
    stats_on_gold_services(price);
    if (prace_is_(RACE_MON_LEPRECHAUN))
        plr->update |= (PU_BONUS | PU_HP | PU_MANA);
}

/************************************************************************
 * The Inn
 ***********************************************************************/
#define _INN_REST_AU  20
#define _INN_FOOD_AU   2
#define _INN_TOWN_AU 500
#define _INN_FAME_AU   1
#define _INN_RUMOR_AU 10
static void _rest(int price) /* shared with BLDG_THIEVES_GUILD */
{
    int prev_day, prev_hour, prev_min;
    slot_t slot;

    if (plr_tim_find(T_POISON) || plr_tim_find(T_CUT))
    {
        msg_print("You need a healer, not a room. Sorry, but I don't want anyone dying in here.");
        return;
    }
    if (price > plr->au)
    {
        msg_print("You don't have enough gold!");
        return;
    }

    extract_day_hour_min(&prev_day, &prev_hour, &prev_min);
    dun_mgr()->turn = (dun_mgr()->turn / (TURNS_PER_TICK*TOWN_DAWN/2) + 1) * (TURNS_PER_TICK*TOWN_DAWN/2);

    plr->chp = plr->mhp;
    plr_tim_remove(T_BLIND);
    plr_tim_remove(T_CONFUSED);
    plr_tim_remove(T_STUN);
    plr->chp = plr->mhp;
    if (plr->pclass != CLASS_RUNE_KNIGHT)
        plr->csp = plr->msp;

    if (plr->pclass == CLASS_MAGIC_EATER)
        magic_eater_restore_all();

    for (slot = 1; slot <= pack_max(); slot++)
    {
        obj_ptr obj = pack_obj(slot);
        if (obj && obj_is_device(obj))
            device_regen_sp_aux(obj, 1000);
    }
    for (slot = 1; slot <= quiver_max(); slot++)
    {
        obj_ptr obj = quiver_obj(slot);
        if (obj && obj_is_device(obj))
            device_regen_sp_aux(obj, 1000);
    }
    for (slot = 1; slot <= equip_max(); slot++)
    {
        obj_ptr obj = equip_obj(slot);
        if (obj && obj->timeout > 0)
            obj->timeout = 0;
        if (obj_is_(obj, TV_HAFTED, SV_WIZSTAFF))
            device_regen_sp_aux(obj, 1000);
    }

    if (prev_hour >= 6 && prev_hour <= 17)
        msg_print("You awake refreshed for the evening.");
    else
        msg_print("You awake refreshed for the new day.");

    if (price > 0)
        _plr_pay(price);
}
static void _inn_rest(bldg_ptr bldg)
{
    _rest(_bldg_price(bldg, _INN_REST_AU));
}
static void _inn_food(bldg_ptr bldg)
{
    int price = _bldg_price(bldg, _INN_FOOD_AU);
    if (((plr_race()->flags & RACE_IS_NONLIVING) && plr->prace != RACE_MON_POSSESSOR) || prace_is_(RACE_ENT))
    {
        msg_print("The food of mortals is poor sustenance for one such as yourself!");
        return;
    }
    if (price > plr->au)
    {
        msg_print("You don't have enough gold!");
        return;
    }
    msg_print("The barkeep gives you some gruel and a beer.");
    set_food(PY_FOOD_MAX - 1);
    _plr_pay(price);
}
static void _inn_tele_town(bldg_ptr bldg)
{
    int price = _bldg_price(bldg, _INN_TOWN_AU);
    u32b flags = plr->wizard ? TF_SECRET : TF_VISITED;
    if (price > plr->au)
    {
        msg_print("You don't have enough gold!");
        return;
    }
    if (dun_mgr_teleport_town(flags))
    {
        _plr_pay(price);
        leave_bldg = TRUE;
    }
}
static void _inn_fame(bldg_ptr bldg)
{
    int price = _bldg_price(bldg, _INN_FAME_AU);
    if (price > plr->au)
    {
        msg_print("You don't have enough gold!");
        return;
    }
    if (plr->fame <= 0)
        cmsg_print(TERM_WHITE, "Who the hell are you?");
    else if (plr->fame < 20)
        cmsg_print(TERM_WHITE, "I've never even heard of you!");
    else if (plr->fame < 40)
        cmsg_print(TERM_L_UMBER, "Hmmm ... You've done a few minor notable deeds, but hardly anything worth bragging about!");
    else if (plr->fame < 60)
        cmsg_print(TERM_YELLOW, "Yes, I've heard of you. The townfolk are talking!");
    else if (plr->fame < 80)
        cmsg_print(TERM_ORANGE, "Ah, good sir. 'Tis an honor to see you again!");
    else if (plr->fame < 100)
        cmsg_print(TERM_L_RED, "You are a true hero!");
    else if (plr->fame < 150)
        cmsg_print(TERM_RED, "You are the stuff of legends!");
    else
        cmsg_print(TERM_VIOLET, "The bards doth sing of ye: Heroic ballads both far 'n wide!");
    _plr_pay(price);
}
static void _inn_rumor(bldg_ptr bldg)
{
    int  price = _bldg_price(bldg, _INN_RUMOR_AU);
    char rumor[1024];
    if (price > plr->au)
    {
        msg_print("You don't have enough gold!");
        return;
    }
    if (!get_rnd_line("rumors.txt", 0, rumor))
    {
        msg_print(rumor);
        _plr_pay(price);
    }
}
static vec_ptr _towns_filter(u32b flags);
static void _inn_display(bldg_ptr bldg, doc_ptr doc)
{
    u32b    flags = plr->wizard ? TF_SECRET : TF_VISITED;
    vec_ptr towns = _towns_filter(flags);
    _bldg_display_aux('r', "Rest for the Night", _bldg_price(bldg, _INN_REST_AU), doc);
    _bldg_display_aux('f', "Buy Food and Drink", _bldg_price(bldg, _INN_FOOD_AU), doc);
    if (vec_length(towns) > 1)
        _bldg_display_aux('t', "Travel to Another Town", _bldg_price(bldg, _INN_TOWN_AU), doc);
    _bldg_display_aux('a', "Ask about your Reputation", _bldg_price(bldg, _INN_FAME_AU), doc);
    _bldg_display_aux('l', "Listen for Rumors", _bldg_price(bldg, _INN_RUMOR_AU), doc);
    vec_free(towns);
}
static bool _inn_command(bldg_ptr bldg, int cmd)
{
    switch (cmd)
    {
    case 'r': _inn_rest(bldg); break;
    case 'f': _inn_food(bldg); break;
    case 't': _inn_tele_town(bldg); break;
    case 'a': _inn_fame(bldg); break;
    case 'l': _inn_rumor(bldg); break;
    default: return FALSE;
    }
    return TRUE;
}
/************************************************************************
 * Helper for Enchanting Items
 ***********************************************************************/
typedef struct _enchant_choice_s { int amt; int cost; } _enchant_choice_t;
static void _enchant_menu_fn(int cmd, int which, vptr cookie, var_ptr res)
{
    _enchant_choice_t *ptr = (_enchant_choice_t *)cookie;
    ptr += which;
    switch (cmd)
    {
    case MENU_TEXT:
    {
        char tmp[255];
        if (ptr->cost >= 10000)
            big_num_display(ptr->cost, tmp);
        else
            sprintf(tmp, "%d", ptr->cost);

        var_set_string(res, format("+%2d %9.9s", ptr->amt, tmp));
        break;
    }
    case MENU_COLOR:
        if (ptr->cost > plr->au)
        {
            var_set_int(res, TERM_L_DARK);
            break;
        }
    default:
        default_menu(cmd, which, cookie, res);
    }
}
static bool enchant_item(bldg_ptr bldg, obj_p filter, int cost, int to_hit, int to_dam, int to_ac)
{
    obj_prompt_t prompt = {0};
    int          i;
    bool         okay = FALSE;
    int          maxenchant;
    char         tmp_str[MAX_NLEN];
    bool         is_guild = bldg->type->guild_p();

    if (cost == 0)
        cost = _bldg_price(bldg, 1500);

    if (plr->prace == RACE_MON_SWORD)
    {
        msg_print("Go enchant yourself!");
        return FALSE;
    }

    prompt.prompt = "Improve which item?";
    prompt.error = "You have nothing to improve.";
    prompt.filter = filter;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_EQUIP;
    prompt.where[2] = INV_QUIVER;
    obj_prompt(&prompt);
    if (!prompt.obj) return FALSE;

    if (is_guild)
        maxenchant = 5 + plr->lev/5;
    else
        maxenchant = 2 + plr->lev/5;

    {
        int idx = -1;
        int old_cost;
        int unit_cost_sum = 0;
        _enchant_choice_t choices[25];
        object_type copy = {0};
        menu_t menu = { "Enchant How Much?", NULL, 
                        "Amt      Cost", _enchant_menu_fn,
                        choices, 25 };

        object_copy(&copy, prompt.obj);
        copy.curse_flags = 0;
        remove_flag(copy.flags, OF_AGGRAVATE);
        remove_flag(copy.flags, OF_NO_TELE);
        remove_flag(copy.flags, OF_NO_MAGIC);
        remove_flag(copy.flags, OF_DRAIN_EXP);
        remove_flag(copy.flags, OF_TY_CURSE);
        old_cost = new_object_cost(&copy, COST_REAL);
                
        for (i = 0; i < 25; i++) /* TODO: Option for max. But +25 a pop is enough perhaps? */
        {
            bool ok = FALSE;
            int  v = 0, m = 5;
            
            choices[i].amt = i+1;

            if (to_hit && copy.to_h < maxenchant) {copy.to_h++; ok = TRUE; v = MAX(v, copy.to_h);}
            if (to_dam && copy.to_d < maxenchant) {copy.to_d++; ok = TRUE; v = MAX(v, copy.to_d);}
            if (to_ac && copy.to_a < maxenchant) {copy.to_a++; ok = TRUE; v = MAX(v, copy.to_a);}

            if (v > 10)
            {
                if (to_ac)
                {
                    int j;
                    for (j = 10; j < v; j++)
                        m = m * 5 / 3;
                }
                else
                    m += v - 10;
            }

            if (obj_is_art(prompt.obj))
                m *= 3;

            if (!ok) break;

            if (prompt.obj->tval == TV_ARROW || prompt.obj->tval == TV_BOLT || prompt.obj->tval == TV_SHOT)
            {
                choices[i].cost = (i+1)*cost*prompt.obj->number;
            }
            else
            {
                int new_cost = new_object_cost(&copy, COST_REAL);
                int unit_cost_add = new_cost - old_cost;
                int min_cost = (i+1)*cost;
                int unit_cost;
                old_cost = new_cost;

                unit_cost_add *= m;
                unit_cost_sum += unit_cost_add;
                unit_cost = unit_cost_sum;

                unit_cost = _bldg_price(bldg, unit_cost);

                if (unit_cost < min_cost)
                    unit_cost = min_cost;

                if (unit_cost >= 10000)
                    choices[i].cost = big_num_round(unit_cost, 3);
                else
                    choices[i].cost = unit_cost;
            }
        }
        if (!i)
        {
            object_desc(tmp_str, prompt.obj, OD_COLOR_CODED);
            msg_format("%^s can not be further improved.", tmp_str);
            return FALSE;
        }

        menu.count = i;
        idx = menu_choose(&menu);
        if (idx < 0) return FALSE;
        
        if (to_hit) to_hit = choices[idx].amt;
        if (to_dam) to_dam = choices[idx].amt;
        if (to_ac) to_ac = choices[idx].amt;
        cost = choices[idx].cost;
    }

    /* Check if the player has enough money */
    if (plr->au < cost)
    {
        object_desc(tmp_str, prompt.obj, OD_NAME_ONLY | OD_COLOR_CODED);
        msg_format("You do not have the gold to improve %s!", tmp_str);
        return FALSE;
    }

    /* Enchant to hit */
    for (i = 0; i < to_hit; i++)
    {
        if (prompt.obj->to_h < maxenchant)
        {
            if (enchant(prompt.obj, 1, (ENCH_TOHIT | ENCH_FORCE)))
                okay = TRUE;
        }
    }

    /* Enchant to damage */
    for (i = 0; i < to_dam; i++)
    {
        if (prompt.obj->to_d < maxenchant)
        {
            if (enchant(prompt.obj, 1, (ENCH_TODAM | ENCH_FORCE)))
                okay = TRUE;
        }
    }

    /* Enchant to AC */
    for (i = 0; i < to_ac; i++)
    {
        if (prompt.obj->to_a < maxenchant)
        {
            if (enchant(prompt.obj, 1, (ENCH_TOAC | ENCH_FORCE)))
                okay = TRUE;
        }
    }

    if (!okay)
    {
        if (flush_failure) flush();
        msg_print("The improvement failed.");
        return FALSE;
    }
    else
    {
        object_desc(tmp_str, prompt.obj, OD_NAME_AND_ENCHANT | OD_COLOR_CODED);
        msg_format("Improved %s for %d gold.", tmp_str, cost);

        _plr_pay(cost);
        obj_release(prompt.obj, OBJ_RELEASE_ENCHANT);
        return TRUE;
    }
}
/************************************************************************
 * The Fighters' Guild
 ***********************************************************************/
static void _fighter_weapon(bldg_ptr bldg)
{
    enchant_item(bldg, obj_is_weapon, 0, 1, 1, 0);
}
static void _fighter_armor(bldg_ptr bldg)
{
    enchant_item(bldg, obj_is_armor, 0, 0, 0, 1);
}
static void _fighter_display(bldg_ptr bldg, doc_ptr doc)
{
    _bldg_display_aux('w', "Enchant Weapon", 0, doc);
    _bldg_display_aux('a', "Enchant Armor", 0, doc);
}
static bool _fighter_command(bldg_ptr bldg, int cmd)
{
    switch (cmd)
    {
    case 'w': _fighter_weapon(bldg); break;
    case 'a': _fighter_armor(bldg); break;
    default: return FALSE;
    }
    return TRUE;
}
static bool _fighter_guild(void)
{
    switch (plr->pclass)
    {
    case CLASS_WARRIOR:
    case CLASS_SAMURAI:
    case CLASS_CAVALRY:
    case CLASS_MAULER: return TRUE;
    }
    return FALSE;
}
/************************************************************************
 * The Archers' Guild
 ***********************************************************************/
#define _ARCHER_AMMO_AU  40
static void _archer_ammo(bldg_ptr bldg)
{
    int price = _bldg_price(bldg, _ARCHER_AMMO_AU);
    enchant_item(bldg, obj_is_ammo, price, 1, 1, 0);
}
static void _archer_bow(bldg_ptr bldg)
{
    enchant_item(bldg, obj_is_bow, 0, 1, 1, 0);
}
static void _archer_display(bldg_ptr bldg, doc_ptr doc)
{
    _bldg_display_aux('a', "Enchant Ammo", _bldg_price(bldg, _ARCHER_AMMO_AU), doc);
    _bldg_display_aux('b', "Enchant Bow", 0, doc);
}
static bool _archer_command(bldg_ptr bldg, int cmd)
{
    switch (cmd)
    {
    case 'a': _archer_ammo(bldg); break;
    case 'b': _archer_bow(bldg); break;
    default: return FALSE;
    }
    return TRUE;
}
static bool _archer_guild(void)
{
    switch (plr->pclass)
    {
    case CLASS_ARCHER:
    case CLASS_SNIPER:
    case CLASS_RANGER: return TRUE;
    }
    if (weaponmaster_is_(WEAPONMASTER_BOWS)) return TRUE;
    if (weaponmaster_is_(WEAPONMASTER_SLINGS)) return TRUE;
    if (weaponmaster_is_(WEAPONMASTER_CROSSBOWS)) return TRUE;
    return FALSE;
}
/************************************************************************
 * The Thieves' Guild
 ***********************************************************************/
static void _thief_rest(bldg_ptr bldg)
{
    _rest(_bldg_price2(bldg, 0, 50));
}
static void _thief_identify(bldg_ptr bldg)
{
    int price = _bldg_price2(bldg, 100, 600);
    if (price > plr->au)
    {
        msg_print("You don't have enough gold!");
        return;
    }
    identify_pack();
    _plr_pay(price);
}
static void _thief_display(bldg_ptr bldg, doc_ptr doc)
{
    _bldg_display_aux('r', "Rest for the Night", _bldg_price2(bldg, 0, 50), doc);
    _bldg_display_aux('i', "Identify Possessions", _bldg_price2(bldg, 100, 600), doc);
}
static bool _thief_command(bldg_ptr bldg, int cmd)
{
    switch (cmd)
    {
    case 'r': _thief_rest(bldg); break;
    case 'i': _thief_identify(bldg); break;
    default: return FALSE;
    }
    return TRUE;
}
static bool _thief_guild(void) { return plr->pclass == CLASS_ROGUE; }
/************************************************************************
 * The Wizards' Tower
 ***********************************************************************/
static void _wizard_identify(bldg_ptr bldg)
{
    int price = _bldg_price(bldg, 50);
    if (price > plr->au)
    {
        msg_print("You don't have enough gold!");
        return;
    }
    if (ident_spell(NULL))
        _plr_pay(price);
}
static void _wizard_identify_full(bldg_ptr bldg)
{
    int price = _bldg_price(bldg, 1300);
    if (price > plr->au)
    {
        msg_print("You don't have enough gold!");
        return;
    }
    if (identify_fully(NULL))
        _plr_pay(price);
}
static void _wizard_self_knowledge(bldg_ptr bldg)
{
    int price = _bldg_price(bldg, 50000);
    if (price > plr->au)
    {
        msg_print("You don't have enough gold!");
        return;
    }
    self_knowledge();
    _plr_pay(price);
}
static void _wizard_recall(bldg_ptr bldg)
{
    int price = _bldg_price(bldg, 200);
    if (price > plr->au)
    {
        msg_print("You don't have enough gold!");
        return;
    }
    if (dun_mgr_recall_plr())
    {
        _plr_pay(price);
        leave_bldg = TRUE;
    }
}
static void _wizard_locate(bldg_ptr bldg)
{
    dun_ptr world = dun_mgr()->world;
    int price = _bldg_price(bldg, 10000), i, tot = 0;
    vec_ptr v;
    if (price > plr->au)
    {
        msg_print("You don't have enough gold!");
        return;
    }
    v = world_dun_types();
    for (i = 0; i < vec_length(v); i++)
    {
        dun_type_ptr dt = vec_get(v, i);
        if (dt->flags.plr & DF_PLR_ENTERED) continue;
        if (dt->flags.plr & DF_PLR_SECRET) continue;
        if (!dun_pos_interior(world, dt->world_pos)) continue; /* paranoia */
        if (dun_grid_at(world, dt->world_pos)->flags & CELL_MAP) continue;
        tot++;
    }
    if (!tot)
        msg_print("You have located all of the dungeons in this world.");
    else
    {
        int n = randint0(tot);
        for (i = 0; i < vec_length(v); i++)
        {
            dun_type_ptr dt = vec_get(v, i);
            if (dt->flags.plr & DF_PLR_ENTERED) continue;
            if (dt->flags.plr & DF_PLR_SECRET) continue;
            if (!dun_pos_interior(world, dt->world_pos)) continue; /* paranoia */
            if (dun_grid_at(world, dt->world_pos)->flags & CELL_MAP) continue;
            if (--n < 0)
            {
                dun_grid_ptr grid = dun_grid_at(world, dt->world_pos);
                grid->flags |= CELL_MAP;
                _plr_pay(price);
                msg_format("I have located <color:o>%s</color> for you. Check your wilderness map once you leave my premises (MM).",
                    dt->name);
                break;
            }
        }
    }
    vec_free(v);
}
static void _wizard_display(bldg_ptr bldg, doc_ptr doc)
{
    _bldg_display_aux('a', "Research Item", _bldg_price(bldg, 1300), doc);
    _bldg_display_aux('i', "Identify Item", _bldg_price(bldg, 50), doc);
    _bldg_display_aux('r', "Recall to Dungeon", _bldg_price(bldg, 200), doc);
    _bldg_display_aux('l', "Locate Dungeon", _bldg_price(bldg, 10000), doc);
    if (bldg->town->level > 45)
        _bldg_display_aux('k', "Self Knowledge", _bldg_price(bldg, 50000), doc);
}
static bool _wizard_command(bldg_ptr bldg, int cmd)
{
    switch (cmd)
    {
    case 'a': _wizard_identify_full(bldg); break;
    case 'i': _wizard_identify(bldg); break;
    case 'r': _wizard_recall(bldg); break;
    case 'l': _wizard_locate(bldg); break;
    case 'k': 
        if (bldg->town->level > 45)
        {
            _wizard_self_knowledge(bldg);
            break;
        }
    default: return FALSE;
    }
    return TRUE;
}
static bool _wizard_guild(void) { return plr_mage_bonus(); }
/************************************************************************
 * Inner Temple
 ***********************************************************************/
static void _priest_heal(bldg_ptr bldg)
{
    mon_ptr mount = plr_riding_mon();
    int     price = _bldg_price2(bldg, 0, 100);

    if (price > plr->au)
    {
        msg_print("You don't have enough gold!");
        return;
    }

    hp_player(200);
    plr_tim_remove(T_POISON);
    plr_tim_remove(T_BLIND);
    plr_tim_remove(T_CONFUSED);
    plr_tim_remove(T_CUT);
    plr_tim_remove(T_STUN);

    if (mount)
    {
        if (mount->hp < 30000) mount->hp += 500;
        if (mount->hp > mount->maxhp) mount->hp = mount->maxhp;
        plr->redraw |= PR_HEALTH_BARS;
    }
    _plr_pay(price);
}
static void _priest_restore(bldg_ptr bldg)
{
    int price = _bldg_price2(bldg, 300, 1000);
    if (price > plr->au)
    {
        msg_print("You don't have enough gold!");
        return;
    }
    do_res_stat(A_STR);
    do_res_stat(A_INT);
    do_res_stat(A_WIS);
    do_res_stat(A_DEX);
    do_res_stat(A_CON);
    do_res_stat(A_CHR);
    _plr_pay(price);
}
static void _priest_cure_mutation(bldg_ptr bldg)
{
    int price = _bldg_price2(bldg, 10000, 250000);
    if (price > plr->au)
    {
        msg_print("You don't have enough gold!");
        return;
    }
    if (!mut_lose_random(NULL))
        msg_print("You have no mutation that I can cure.");
    else
        _plr_pay(price);
}
static void _priest_display(bldg_ptr bldg, doc_ptr doc)
{
    _bldg_display_aux('h', "Healing Prayer", _bldg_price2(bldg, 0, 100), doc);
    _bldg_display_aux('r', "Restoration", _bldg_price2(bldg, 300, 1000), doc);
    if (bldg->town->level > 45)
        _bldg_display_aux('m', "Cure Mutation", _bldg_price2(bldg, 10000, 250000), doc);
}
static bool _priest_command(bldg_ptr bldg, int cmd)
{
    switch (cmd)
    {
    case 'h': _priest_heal(bldg); break;
    case 'r': _priest_restore(bldg); break;
    case 'm':
        if (bldg->town->level > 45)
        {
            _priest_cure_mutation(bldg);
            break;
        }
    default: return FALSE;
    }
    return TRUE;
}
static bool _priest_guild(void)
{
    if (plr->realm1 == REALM_LIFE) return TRUE;
    if (plr->realm1 == REALM_BLESS) return TRUE;
    if (prace_is_(RACE_MON_ANGEL)) return TRUE;
    return FALSE;
}
/************************************************************************
 * Hunters Office
 ***********************************************************************/
static int _prize_count = 0;
static struct {
    s16b tval;
    s16b sval;
} prize_list[MAX_KUBI] = 
{
    {TV_POTION, SV_POTION_CURING},
    {TV_POTION, SV_POTION_SPEED},
    {TV_POTION, SV_POTION_SPEED},
    {TV_POTION, SV_POTION_RESISTANCE},
    {TV_POTION, SV_POTION_ENLIGHTENMENT},

    {TV_POTION, SV_POTION_HEALING},
    {TV_SCROLL, SV_SCROLL_STAR_DESTRUCTION},
    {TV_POTION, SV_POTION_STAR_ENLIGHTENMENT},
    {TV_SCROLL, SV_SCROLL_GENOCIDE},
    {TV_POTION, SV_POTION_AUGMENTATION},

    {TV_POTION, SV_POTION_NEW_LIFE},
    {TV_SCROLL, SV_SCROLL_CRAFTING},
    {TV_POTION, SV_POTION_STAR_HEALING},
    {TV_POTION, SV_POTION_STAR_HEALING},
    {TV_SCROLL, SV_SCROLL_MASS_GENOCIDE},

    {TV_POTION, SV_POTION_LIFE},
    {TV_POTION, SV_POTION_LIFE},
    {TV_POTION, SV_POTION_RESTORE_MANA},
    {TV_POTION, SV_POTION_INVULNERABILITY},
    {TV_SCROLL, SV_SCROLL_ARTIFACT},
};

static bool _is_captured_tsuchinoko(obj_ptr obj)
{
    if (obj->tval == TV_CAPTURE && sym_equals(obj->race_id, "J.tsuchinoko"))
        return TRUE;
    return FALSE;
}
static bool _is_corpse_tsuchinoko(obj_ptr obj)
{
    if (obj->tval == TV_CORPSE && sym_equals(obj->race_id, "J.tsuchinoko"))
        return TRUE;
    return FALSE;
}
static int _tsuchinoko_amt(obj_ptr obj)
{
    if (obj->tval == TV_CAPTURE)
        return 1000000 * obj->number;
    if (obj->tval == TV_CORPSE && obj->sval == SV_CORPSE)
        return 200000 * obj->number;
    if (obj->tval == TV_CORPSE && obj->sval == SV_SKELETON)
        return 100000 * obj->number;
    return 0;
}
static void _obj_reward(obj_ptr obj, int amt)
{
    msg_format("You get %dgp.", amt);
    plr->au += amt;
    stats_on_gold_winnings(amt);
    obj->number = 0;
    obj_release(obj, 0);
    plr->redraw |= PR_GOLD;
    plr->notice |= PN_OPTIMIZE_PACK;
}
static void _process_tsuchinoko(obj_ptr obj)
{
    char name[MAX_NLEN];
    char buf[MAX_NLEN+30];
    object_desc(name, obj, OD_COLOR_CODED);
    sprintf(buf, "Convert %s into money? ", name);
    if (get_check(buf))
        _obj_reward(obj, _tsuchinoko_amt(obj));
    ++_prize_count;
}
static bool _is_todays_prize(obj_ptr obj)
{
    if (obj->tval == TV_CORPSE)
        return obj->race_id == today_mon;
    return FALSE;
}
static void _process_todays_prize(obj_ptr obj)
{
    char name[MAX_NLEN];
    char buf[MAX_NLEN+30];
    object_desc(name, obj, OD_COLOR_CODED);
    sprintf(buf, "Convert %s into money? ", name);
    if (get_check(buf))
    {
        int mult = obj->sval == SV_CORPSE ? 50 : 30;
        int amt = (mon_race_lookup(today_mon)->alloc.lvl * mult + 100) * obj->number;
        _obj_reward(obj, amt);
    }
    ++_prize_count;
}
static int _wanted_monster_idx(int r_idx)
{
    int i;
    for (i = 0; i < MAX_KUBI; i++)
    {
        if (kubi_r_idx[i] == r_idx)
            return i;
    }
    return -1;
}
static bool _is_wanted_monster(int r_idx)
{
    int idx = _wanted_monster_idx(r_idx);
    if (idx >= 0) return TRUE;
    return FALSE;
}
static void _forge_wanted_monster_prize(obj_ptr obj, int r_idx)
{
    int idx = _wanted_monster_idx(r_idx);
    int tval = prize_list[idx].tval;
    int sval = prize_list[idx].sval;

    object_prep(obj, lookup_kind(tval, sval));
    apply_magic(obj, cave->difficulty, AM_NO_FIXED_ART);
    obj_make_pile(obj);
    obj_identify_fully(obj);
}
static bool _is_wanted_corpse(obj_ptr obj)
{
    if (obj->tval == TV_CORPSE && _is_wanted_monster(obj->race_id))
        return TRUE;
    return FALSE;
}
static void _process_wanted_corpse(obj_ptr obj)
{
    char  name[MAX_NLEN];
    char  buf[MAX_NLEN+30];
    obj_t prize;
    int   r_idx = obj->race_id;
    int   num, k;

    ++_prize_count;

    object_desc(name, obj, OD_COLOR_CODED);
    sprintf(buf, "Hand %s over? ", name);
    if (!get_check(buf)) return;

    _forge_wanted_monster_prize(&prize, r_idx);
    obj->number = 0;
    obj_release(obj, 0);

    virtue_add(VIRTUE_JUSTICE, 5);
    plr->fame++;
    mon_race_lookup(r_idx)->flagsx |= RFX_BOUNTY;

    /* Count number of unique corpses already handed */
    for (num = 0, k = 0; k < MAX_KUBI; k++)
    {
        mon_race_ptr race = mon_race_lookup(kubi_r_idx[k]);
        if (!race) continue;
        if (race->flagsx & RFX_BOUNTY) num++;
    }

    msg_format("You earned %d point%s total.", num, num > 1 ? "s" : "");

    object_desc(name, &prize, OD_COLOR_CODED);
    /*msg_format("You get %s.", name);*/
    pack_carry(&prize);
}
static bool _bounty(void)
{
    _prize_count = 0;
    equip_for_each_that(_process_tsuchinoko, _is_captured_tsuchinoko);
    pack_for_each_that(_process_tsuchinoko, _is_captured_tsuchinoko);
    pack_for_each_that(_process_tsuchinoko, _is_corpse_tsuchinoko);
    pack_for_each_that(_process_todays_prize, _is_todays_prize);
    pack_for_each_that(_process_wanted_corpse, _is_wanted_corpse);
    if (!_prize_count)
    {
        msg_print("You have nothing that I want.");
        return FALSE;
    }
    return TRUE;
}
static void _today_mon(void)
{
    mon_race_ptr race = mon_race_lookup(today_mon);

    msg_format("<color:R>Today's Wanted Monster:</color> %s\n", race->name);
    msg_format("<color:U>Corpse:</color> %dgp.\n", race->alloc.lvl * 50 + 100);
    msg_format("<color:U>Bones :</color> %dgp.\n", race->alloc.lvl * 30 + 60);
    plr->today_mon = today_mon;
}
static void _hunter_display(bldg_ptr bldg, doc_ptr doc)
{
    _bldg_display_aux('t', "Today's Wanter Monster", 0, doc);
    _bldg_display_aux('u', "Wanted Unique Monsters", 0, doc);
    _bldg_display_aux('b', "Receive Bounty", 0, doc);
}
void display_wanted_uniques(void)
{
    doc_ptr doc = doc_alloc(80);
    int i;

    doc_insert(doc, "<color:U>Most Wanted Uniques</color>\n");
    for (i = 0; i < MAX_KUBI; i++)
    {
        int  id = kubi_r_idx[i];
        bool done = FALSE;
        mon_race_ptr race;

        if (!id) continue; /* SUPPRESSED by reduce_uniques options */
        race = mon_race_lookup(id);
        if (!race) continue; /* paranoia */

        if (race->flagsx & RFX_BOUNTY) done = TRUE;

        doc_printf(doc, "<color:%c>%s%s</color>\n", done ? 'R' : 'w',
            race->name, done ? " (done)" : "");
    }
    doc_insert(doc, "\n\nWanted for crimes against the peoples of Middle Earth. "
                    "Be warned: These uniques are armed and considered extremely dangerous. "
                    "Approach with utmost caution!\n");

    screen_save();
    doc_display(doc, "Wanted Uniques", 0);
    screen_load();
    doc_free(doc);
}
static bool _hunter_command(bldg_ptr bldg, int cmd)
{
    switch (cmd)
    {
    case 't': _today_mon(); break;
    case 'b': _bounty(); break;
    case 'u': display_wanted_uniques(); break;
    default: return FALSE;
    }
    return TRUE;
}
/************************************************************************
 * Buildings
 ***********************************************************************/
static bldg_ptr bldg_alloc(town_ptr town, int bldg_id)
{
    bldg_ptr bldg = malloc(sizeof(bldg_t));
    bldg->type = _get_bldg_type(bldg_id);
    bldg->owner = NULL;
    bldg->town = town;
    _bldg_change_owner(bldg, FALSE);
    return bldg;
}
bldg_ptr bldg_load(savefile_ptr file)
{
    bldg_ptr bldg = malloc(sizeof(bldg_t));
    int      tmp;

    tmp = savefile_read_s16b(file);
    bldg->type = _get_bldg_type(tmp);
    assert(bldg->type);

    tmp = savefile_read_s16b(file);
    bldg->owner = _get_owner(bldg->type->owners, tmp);
    assert(bldg->owner);
    bldg->owner->active = TRUE;

    return bldg;
}
void bldg_save(bldg_ptr bldg, savefile_ptr file)
{
    savefile_write_s16b(file, bldg->type->id);
    savefile_write_s16b(file, bldg->owner->id);
}
void bldg_free(bldg_ptr bldg)
{
    if (!bldg) return;
    free(bldg);
}
static void _bldg_display(bldg_ptr bldg, doc_ptr doc)
{
    rect_t   r = ui_shop_rect();
    int      ct = strlen(bldg->type->name);
    char     buf[10];

    doc_clear(doc);
    doc_insert(doc, "<style:table>");
    doc_printf(doc, "    <color:U>%s (%s)</color>",
        bldg->owner->name, plr_race_aux(bldg->owner->race_id, 0)->name);
    doc_printf(doc, "<tab:%d><color:G>%s</color>\n\n", doc_width(doc) - ct, bldg->type->name);
    doc_insert(doc, "    Service");
    doc_printf(doc, "<tab:%d>", doc_width(doc) - 7);
    doc_printf(doc, " %6.6s", "Price");
    doc_newline(doc);

    bldg->type->display_f(bldg, doc);

    big_num_display(plr->au, buf);
    doc_printf(doc, "\n\nGold Remaining: <color:y>%s</color>\n\n", buf);
    doc_insert(doc,
        "<color:keypress>Esc</color> to exit. "
        "<color:keypress>?</color> for help.");
    doc_insert(doc, "</style>");

    Term_clear_rect(r);
    doc_sync_term(doc,
        doc_range_top_lines(doc, r.cy),
        doc_pos_create(r.x, r.y));
}
void bldg_ui(bldg_ptr bldg)
{
    doc_ptr doc;
    character_icky = TRUE;

    msg_line_clear();
    msg_line_init(ui_shop_msg_rect());

    Term_clear();
    doc = doc_alloc(MIN(80, ui_shop_rect().cx));
    leave_bldg = FALSE;
    while (!leave_bldg)
    {
        int cmd;

        _bldg_display(bldg, doc);

        cmd = inkey_special(TRUE);
        msg_line_clear();
        msg_boundary(); /* turn_count is unchanging while in home/museum */
        if (cmd == ESCAPE || cmd == 'q' || cmd == 'Q') break;
        if (!bldg->type->command_f(bldg, cmd))
        {
            if (cmd == KTRL('O') && plr->wizard)
            {
                _bldg_change_owner(bldg, TRUE);
            }
            else if (cmd < 256 && isprint(cmd))
            {
                msg_format("Unrecognized command: <color:R>%c</color>. "
                           "Press <color:keypress>?</color> for help.", cmd);
            }
            else if (KTRL('A') <= cmd && cmd <= KTRL('Z'))
            {
                cmd |= 0x40;
                msg_format("Unrecognized command: <color:R>^%c</color>. "
                           "Press <color:keypress>?</color> for help.", cmd);
            }
        }
    }
    character_icky = FALSE;
    energy_use = 100;
    msg_print(NULL); /* recall service might warn about guardian */
    msg_line_clear();
    msg_line_init(ui_msg_rect());

    Term_clear();
    do_cmd_redraw();

    doc_free(doc);
}
/************************************************************************
 * Town
 ***********************************************************************/
static char *_strcpy(cptr s)
{
    char *r = malloc(strlen(s)+1);
    strcpy(r, s);
    return r;
}
town_ptr town_alloc(int which, cptr name)
{
    town_ptr town = malloc(sizeof(town_t));
    memset(town, 0, sizeof(town_t));
    town->id = which;
    town->name = _strcpy(name);
    town->shops = int_map_alloc((int_map_free_f)shop_free);
    town->bldgs = int_map_alloc((int_map_free_f)bldg_free);
    return town;
}
void town_free(town_ptr town)
{
    if (town)
    {
        free(town->name);
        int_map_free(town->shops);
        int_map_free(town->bldgs);
        town->shops = NULL;
        town->bldgs = NULL;
        free(town);
    }
}
void town_load(town_ptr town, savefile_ptr file)
{
    int      ct, i;

    town->flags = savefile_read_u32b(file);
    town->world_pos.x = savefile_read_s16b(file);
    town->world_pos.y = savefile_read_s16b(file);

    if (town->shops) int_map_free(town->shops);
    town->shops = int_map_alloc((int_map_free_f)shop_free);

    ct = savefile_read_s16b(file);
    for (i = 0; i < ct; i++)
    {
        shop_ptr shop = shop_load(file);
        shop->town = town;
        int_map_add(town->shops, shop->type->id, shop);
    }

    ct = savefile_read_s16b(file);
    for (i = 0; i < ct; i++)
    {
        bldg_ptr bldg = bldg_load(file);
        bldg->town = town;
        int_map_add(town->bldgs, bldg->type->id, bldg);
    }
}
void town_save(town_ptr town, savefile_ptr file)
{
    int_map_iter_ptr iter;

    savefile_write_u32b(file, town->flags);
    savefile_write_s16b(file, town->world_pos.x);
    savefile_write_s16b(file, town->world_pos.y);

    savefile_write_s16b(file, int_map_count(town->shops));
    for (iter = int_map_iter_alloc(town->shops);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        shop_ptr shop = int_map_iter_current(iter);
        shop_save(shop, file);
    }
    int_map_iter_free(iter);

    savefile_write_s16b(file, int_map_count(town->bldgs));
    for (iter = int_map_iter_alloc(town->bldgs);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        bldg_ptr bldg = int_map_iter_current(iter);
        bldg_save(bldg, file);
    }
    int_map_iter_free(iter);
}
void town_reset(town_ptr town)
{
    int_map_iter_ptr iter;
    for ( iter = int_map_iter_alloc(town->shops);
          int_map_iter_is_valid(iter);
          int_map_iter_next(iter) )
    {
        shop_ptr shop = int_map_iter_current(iter);
        shop_reset(shop);
        _shop_change_owner(shop, FALSE);
    }
    int_map_iter_free(iter);
    for ( iter = int_map_iter_alloc(town->bldgs);
          int_map_iter_is_valid(iter);
          int_map_iter_next(iter) )
    {
        bldg_ptr bldg = int_map_iter_current(iter);
        _bldg_change_owner(bldg, FALSE);
    }
    int_map_iter_free(iter);
}
shop_ptr town_get_shop(town_ptr town, int which)
{
    shop_ptr shop = int_map_find(town->shops, which);
    if (!shop)
    {
        shop = shop_alloc(town, which);
        int_map_add(town->shops, which, shop);
    }
    assert(shop);
    return shop;
}
bldg_ptr town_get_bldg(town_ptr town, int which)
{
    bldg_ptr bldg = int_map_find(town->bldgs, which);
    if (!bldg)
    {
        bldg = bldg_alloc(town, which);
        int_map_add(town->bldgs, which, bldg);
    }
    assert(bldg);
    return bldg;
}
void town_on_visit(int which)
{
    towns_lookup(which)->flags |= TF_VISITED;
}
cptr town_name(int which)
{
    return towns_lookup(which)->name;
}

/************************************************************************
 * Towns
 ***********************************************************************/
static town_ptr _rivendell(void);
static town_ptr _beorn(void);
static town_ptr _laketown(void);
static town_ptr _edoras(void);
static town_ptr _morannon(void);
static town_ptr _osgiliath(void);
static town_ptr _minas_tirith(void);
static town_ptr _outpost(void);
static town_ptr _angwil(void);
static town_ptr _telmora(void);
static town_ptr _zul(void);

typedef struct {
    int        id;
    cptr       parse;
    town_ptr (*create_f)(void);
} _town_entry_t, *_town_entry_ptr;
static _town_entry_t _town_tbl[] = {
    { TOWN_RIVENDELL, "Rivendell", _rivendell },
    { TOWN_BEORN, "Beorn", _beorn },
    { TOWN_LAKETOWN, "Laketown", _laketown },
    { TOWN_EDORAS, "Edoras", _edoras },
    { TOWN_MORANNON, "Morannon", _morannon },
    { TOWN_OSGILIATH, "Osgiliath", _osgiliath },
    { TOWN_MINAS_TIRITH, "Minas Tirith", _minas_tirith },
    { TOWN_OUTPOST, "Outpost", _outpost },
    { TOWN_ANGWIL, "Angwil", _angwil },
    { TOWN_TELMORA, "Telmora", _telmora },
    { TOWN_ZUL, "Zul", _zul },
    { 0 }
};

static int_map_ptr _towns(void)
{
    static int_map_ptr map = NULL;
    if (!map)
        map = int_map_alloc((int_map_free_f) town_free);
    return map;
}
int towns_parse(cptr name)
{
    int i;
    for (i = 0;; i++)
    {
        _town_entry_ptr e = &_town_tbl[i];
        if (e->id == TOWN_NONE) break;
        if (strcmp(e->parse, name) == 0) return e->id;
    }
    return TOWN_NONE;
}
town_ptr towns_lookup(int id)
{
    town_ptr town = int_map_find(_towns(), id);
    if (!town)
    {
        int i;
        for (i = 0; !town; i++)
        {
            _town_entry_ptr e = &_town_tbl[i];
            if (e->id == TOWN_NONE) break;
            if (e->id != id) continue;
            assert(e->create_f);
            if (!e->create_f) return NULL;
            town = e->create_f();
            assert(town->id == id);
            int_map_add(_towns(), id, town);
        }
    }
    return town;
}
void towns_reset_world(void)
{
    int_map_clear(_towns());
}
town_ptr towns_current_town(void)
{
    if (cave->type->id == D_SURFACE)
    {
        int id = dun_world_town_id();
        if (id) return towns_lookup(dun_world_town_id());
    }
    if (cave->flags & DF_SHOP)
        return cave->town;
    return NULL;
}
static vec_ptr _towns_filter(u32b flags)
{
    vec_ptr v = vec_alloc(NULL);
    dun_ptr world = dun_mgr()->world;
    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(_towns());
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        town_ptr town = int_map_iter_current(iter);
        if (!dun_pos_interior(world, town->world_pos)) continue;
        /* TF_SECRET allows inclusion of "secret" town (Zul) */
        if (!(flags & TF_SECRET) && (town->flags & TF_SECRET)) continue;
        /* TF_VISITED restricts choices to visited towns only (wizard mode bypasses this) */
        if ((flags & TF_VISITED) && !(town->flags & TF_VISITED)) continue;
        vec_add(v, town);
    }
    int_map_iter_free(iter);
    return v;
}
static town_ptr _towns_choose(vec_ptr towns)
{
    int i, ct = 0, skip = dun_world_town_id(), cmd;
    doc_ptr doc;
    town_ptr result = NULL;
    if (!vec_length(towns))
    {
        msg_print("You haven't visited any towns yet.");
        return NULL;
    }
    for (i = 0; i < vec_length(towns); i++)
    {
        town_ptr town = vec_get(towns, i);
        if  (town->id == skip) continue;
        ct++;
    }
    if (!ct)
    {
        assert(skip);
        msg_format("You haven't visited any towns other than <color:R>%s</color> yet.", towns_lookup(skip)->name);
        return NULL;
    }
    Term_save();
    doc = doc_alloc(80);
    doc_insert(doc, "<color:U>Choose a Town:</color>\n");
    for (i = 0; i < vec_length(towns); i++)
    {
        town_ptr town = vec_get(towns, i);
        doc_printf(doc, " <color:%c>%c)</color> %s\n",
            town->id == skip ? 'D' : 'y', I2A(i), town->name);
    }
    while (!result)
    {
        doc_sync_menu(doc);
        cmd = inkey_special(TRUE);
        if (cmd == ESCAPE) break;
        i = A2I(cmd);
        if (0 <= i && i < vec_length(towns))
        {
            town_ptr town = vec_get(towns, i);
            if (town->id == skip) continue;
            result = town;
        }
    }
    doc_free(doc);
    Term_load();
    return result;
}
town_ptr towns_choose(u32b flags)
{
    vec_ptr towns = _towns_filter(flags);
    town_ptr town = _towns_choose(towns);
    vec_free(towns);
    return town;
}
void towns_save(savefile_ptr file)
{
    int_map_iter_ptr iter;

    savefile_write_s16b(file, int_map_count(_towns()));

    for (iter = int_map_iter_alloc(_towns());
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        town_ptr town = int_map_iter_current(iter);
        savefile_write_s16b(file, town->id);
        town_save(town, file);
    }
    int_map_iter_free(iter);
}
void towns_load(savefile_ptr file)
{
    int i, ct;

    int_map_clear(_towns());
    ct = savefile_read_s16b(file);
    for (i = 0; i < ct; i++)
    {
        s16b id = savefile_read_s16b(file);
        town_ptr town = towns_lookup(id);
        town_load(town, file);
    }
}

/************************************************************************
 * Towns
 ***********************************************************************/
static int _rivendell_mon_alloc(mon_race_ptr race, int prob)
{
    if (mon_race_is_char(race, 'h')) return 10 * prob;
    if (mon_race_is_char(race, 't')) return 0;
    if (race->align < 0) return 0;
    return prob;
}
static bool _rivendell_owner_race(int id)
{
    switch (id)
    {
    case RACE_HIGH_ELF:
    case RACE_WATER_ELF:
    case RACE_WOOD_ELF:
        return TRUE;
    }
    return FALSE;
}
static void _rivendell_kill_mon(mon_ptr mon)
{
    int_map_iter_ptr iter;
    if (!mon_is_friendly(mon)) return;
    for (iter = int_map_iter_alloc(plr_dun()->mon);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_ptr m = int_map_iter_current(iter);
        if (m != mon && m->cdis < MAX_SIGHT && mon_is_friendly(m))
        {
            set_hostile(m);
            mon_tim_delete(m, MT_SLEEP);
        }
    }
    int_map_iter_free(iter);
}
static town_ptr _rivendell(void)
{
    town_ptr me = town_alloc(TOWN_RIVENDELL, "Rivendell");
    me->level = 5;
    me->file = "t_rivendell.txt";
    me->mon_alloc_f = _rivendell_mon_alloc;
    me->owner_race_p = _rivendell_owner_race;
    me->kill_mon_f = _rivendell_kill_mon;
    me->flags = TF_FRIENDLY;
    return me;
}
static town_ptr _beorn(void)
{
    town_ptr me = town_alloc(TOWN_BEORN, "Beorn");
    me->level = 15;
    me->file = "t_beorn.txt";
    return me;
}
static town_ptr _laketown(void)
{
    town_ptr me = town_alloc(TOWN_LAKETOWN, "Laketown");
    me->level = 25;
    me->file = "t_laketown.txt";
    return me;
}
static bool _edoras_owner_race(int id)
{
    switch (id)
    {
    case RACE_HUMAN:
    case RACE_DUNADAN:
    case RACE_DEMIGOD:
    case RACE_BARBARIAN:
        return TRUE;
    }
    return FALSE;
}
static int _edoras_mon_alloc(mon_race_ptr race, int prob)
{
    if (mon_race_is_char(race, 'p')) return 10 * prob;
    if (mon_race_is_char(race, 't')) return 0;
    return prob;
}
static town_ptr _edoras(void)
{
    town_ptr me = town_alloc(TOWN_EDORAS, "Edoras");
    me->level = 40;
    me->file = "t_edoras.txt";
    me->owner_race_p = _edoras_owner_race;
    me->mon_alloc_f = _edoras_mon_alloc;
    return me;
}
static int _morannon_mon_alloc(mon_race_ptr race, int prob)
{
    if (!mon_race_is_evil(race)) return 0;
    if (mon_race_is_undead(race)) return 0;
    if (race->alloc.lvl < 10) return 0;
    if (mon_race_is_troll(race)) return 10*prob;
    if (mon_race_is_orc(race)) return 10*prob;
    if (mon_race_is_char(race, 'P') && mon_race_is_giant(race)) return 3*prob;
    if (mon_race_is_human(race)) return prob;
    return (prob + 4)/5;
}
static void _morannon_populate(dun_ptr dun, rect_t rect)
{
    point_t c = rect_center(rect);
    rect_t  r = rect_create_centered(c, 35, 8);
    int     i, j, ct = damroll(12, 10);

    mon_alloc_push_weight(_morannon_mon_alloc);
    for (i = 0; i < ct; i++)
    {
        point_t      pos = {0};
        mon_race_ptr race = NULL;

        for (j = 0; j < 1000; j++)
        {
            point_t p = rect_random_point(r);
            if (!dun_allow_mon_at(dun, p)) continue;
            pos = p;
            break;
        }
        if (!rect_contains_point(r, pos)) continue;
        race = mon_alloc_choose_aux2(mon_alloc_tbl, 50, 0, GMN_NO_UNIQUES | GMN_IGNORE_MAX_LEVEL);
        if (!race) continue;
        place_monster_aux(who_create_null(), pos, race, PM_ALLOW_GROUP);
    }
    mon_alloc_pop_weight();
}
static town_ptr _morannon(void) /* The Black Gates of Mordor */
{
    town_ptr me = town_alloc(TOWN_MORANNON, "Morannon");
    me->level = 80;
    me->file = "t_morannon.txt";
    me->populate_f = _morannon_populate;
    return me;
}
static town_ptr _osgiliath(void)
{
    town_ptr me = town_alloc(TOWN_OSGILIATH, "Osgiliath");
    me->level = 50;
    me->file = "t_osgiliath.txt";
    return me;
}
static town_ptr _minas_tirith(void)
{
    town_ptr me = town_alloc(TOWN_MINAS_TIRITH, "Minas Tirith");
    me->level = 50;
    me->file = "t_minas_tirith.txt";
    return me;
}
static town_ptr _outpost(void)
{
    town_ptr me = town_alloc(TOWN_OUTPOST, "Outpost");
    me->level = 5;
    me->file = "t_outpost.txt";
    return me;
}
static int _angwil_mon_alloc(mon_race_ptr race, int prob)
{
    if (mon_race_is_char(race, 'h')) return 10 * prob;
    if (mon_race_is_char(race, 't')) return 0;
    if (race->align < 0) return 0;
    return prob;
}
static bool _angwil_owner_race(int id)
{
    switch (id)
    {
    case RACE_HIGH_ELF:
    case RACE_WATER_ELF:
    case RACE_WOOD_ELF:
        return TRUE;
    }
    return FALSE;
}
static town_ptr _angwil(void)
{
    town_ptr me = town_alloc(TOWN_ANGWIL, "Angwil");
    me->level = 15;
    me->file = "t_angwil.txt";
    me->mon_alloc_f = _angwil_mon_alloc;
    me->owner_race_p = _angwil_owner_race;
    return me;
}
static town_ptr _telmora(void)
{
    town_ptr me = town_alloc(TOWN_TELMORA, "Telmora");
    me->level = 10;
    me->file = "t_telmora.txt";
    return me;
}
static town_ptr _zul(void)
{
    town_ptr me = town_alloc(TOWN_ZUL, "Zul");
    me->level = 50;
    me->file = "t_zul.txt";
    me->flags = TF_SECRET;
    return me;
}
/************************************************************************
 * Towns: Parsing
 ***********************************************************************/
static room_ptr _temp_room;
static errr _parse_town(char *line, int options)
{
    if (line[0] == 'B' && line[1] == ':')
        return 0;
    if (_temp_room)
        return parse_room_line(_temp_room, line, options);
    return 0;
}
room_ptr towns_get_map(int town_id)
{
    room_ptr room = room_alloc("NotSureYet");
    town_ptr town = towns_lookup(town_id);

    assert(town->file);

    _temp_room = room;
    if (parse_edit_file(town->file, _parse_town, 0) != ERROR_SUCCESS)
    {
        room_free(room);
        room = NULL;
    }
    _temp_room = NULL;
    return room;
}

