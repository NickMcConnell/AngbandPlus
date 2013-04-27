// File: types.h
// Purpose: global type declarations

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */


/*
 * Note that "char" may or may not be signed, and that "signed char"
 * may or may not work on all machines.  So always use "s16b" or "s32b"
 * for signed values.  Also, note that unsigned values cause math problems
 * in many cases, so try to only use "u16b" and "u32b" for "bit flags",
 * unless you really need the extra bit of information, or you really
 * need to restrict yourself to a single byte for storage reasons.
 *
 * Also, if possible, attempt to restrict yourself to sub-fields of
 * known size (use "s16b" or "s32b" instead of "int", and "byte" instead
 * of "bool"), and attempt to align all fields along four-byte words, to
 * optimize storage issues on 32-bit machines.  Also, avoid "bit flags"
 * since these increase the code size and slow down execution.  When
 * you need to store bit flags, use one byte per flag, or, where space
 * is an issue, use a "byte" or "u16b" or "u32b", and add special code
 * to access the various bit flags.
 *
 * Many of these structures were developed to reduce the number of global
 * variables, facilitate structured program design, allow the use of ascii
 * template files, simplify access to indexed data, or facilitate efficient
 * clearing of many variables at once.
 */





/*
 * Simple class to hold a map location
 */
class CCoord {
public:
    s16b x, y;

    CCoord() { x = y = 0; }
    CCoord(CCoord &c) { x = c.x; y = c.y; }

    int GetX(void) { return x; }
    int GetY(void) { return y; }
    void GetLocation(int *xp, int *yp) { *xp = x; *yp = y; }
    
    void SetX(int i) { x = i; }
    void SetY(int i) { y = i; }
    void SetLocation(int xx, int yy) { x = xx; y = yy; }
};



/*
 * Template file header information (see "init.c").  16 bytes.
 *
 * Note that the sizes of many of the "arrays" are between 32768 and
 * 65535, and so we must use "unsigned" values to hold the "sizes" of
 * these arrays below.  Normally, I try to avoid using unsigned values,
 * since they can cause all sorts of bizarre problems, but I have no
 * choice here, at least, until the "race" array is split into "normal"
 * and "unique" monsters, which may or may not actually help.
 *
 * Note that, on some machines, for example, the Macintosh, the standard
 * "read()" and "write()" functions cannot handle more than 32767 bytes
 * at one time, so we need replacement functions, see "util.c" for details.
 *
 * In general, these problems occur only on machines (such as most personal
 * computers) which use 2 byte "int" values, and which use "int" for the
 * arguments to the relevent functions.
 */
struct header {
    u16b name_size;             // Size of the "name" array in bytes
    u16b text_size;             // Size of the "text" array in bytes
};



/*
 * Information about terrain "features"
 */
struct feature_type {
    u16b name;                  // Name (offset)
    u16b text;                  // Text (offset)
};


/*
 * Some generic information about a player
 */
struct player_summary {
    char name[32];
    byte prace;
    byte pclass;
    bool male;
};


/*
 * Information about object "kinds", including player knowledge.
 *
 * Only "aware" and "tried" are saved in the savefile
 */
class CObjectKind {
public:
    char *name;                 // Name
    s16b tile;                  // Name of tile

    byte tval;                  // Item type
    byte sval;                  // Item sub type

    s16b pval;                  // Item extra info

    s16b to_h;                  // Bonus to hit
    s16b to_d;                  // Bonus to damage
    s16b to_a;                  // Bonus to armor

    s16b ac;                    // Base armor

    byte dd, ds;                // Damage dice/sides

    s16b weight;                // Weight

    s32b cost;                  // Item "base cost"

    u32b flags1;                // Flags, set 1
    u32b flags2;                // Flags, set 2
    u32b flags3;                // Flags, set 3

    byte locale[4];             // Allocation level(s)
    byte chance[4];             // Allocation chance(s)

    byte level;                 // Level
    byte extra;                 // Something


    bool has_flavor;            // This item has a flavor
    bool easy_know;             // This item is always known (if aware)

    bool aware;                 // The player is "aware" of the item's effects
    bool tried;                 // The player has "tried" one of the items
};



/*
 * Information about "artifacts".
 *
 * Note that the save-file only writes "cur_num" to the savefile.
 *
 * Note that "max_num" is always "1" (if that artifact "exists")
 */
struct artifact_type {
    u16b name;                  // Name (offset)
    u16b text;                  // Text (offset)

    byte tval;                  // Artifact type
    byte sval;                  // Artifact sub type

    s16b pval;                  // Artifact extra info

    s16b to_h;                  // Bonus to hit
    s16b to_d;                  // Bonus to damage
    s16b to_a;                  // Bonus to armor

    s16b ac;                    // Base armor

    byte dd, ds;                // Damage when hits

    s16b weight;                // Weight

    s32b cost;                  // Artifact "cost"

    u32b flags1;                // Artifact Flags, set 1
    u32b flags2;                // Artifact Flags, set 2
    u32b flags3;                // Artifact Flags, set 3

    byte level;                 // Artifact level
    byte rarity;                // Artifact rarity

    byte cur_num;               // Number created (0 or 1)
    byte max_num;               // Unused (should be "1")
};


/*
 * Information about "ego-items".
 */
struct ego_item_type {
    u16b name;                  // Name (offset)
    u16b text;                  // Text (offset)

    byte slot;                  // Standard slot value
    byte rating;                // Rating boost

    byte level;                 // Minimum level
    byte rarity;                // Item rarity

    byte max_to_h;              // Maximum to-hit bonus
    byte max_to_d;              // Maximum to-dam bonus
    byte max_to_a;              // Maximum to-ac bonus

    byte max_pval;              // Maximum pval

    s32b cost;                  // Ego-item "cost"

    u32b flags1;                // Ego-Item Flags, set 1
    u32b flags2;                // Ego-Item Flags, set 2
    u32b flags3;                // Ego-Item Flags, set 3
};




/*
 * Monster blow structure
 */
struct monster_blow {
    byte method;                // RBM_*
    byte effect;                // RBE_*
    byte d_dice;                // Damage dice
    byte d_side;                // Damage sides
};



/*
 * Monster "race" information, including racial memories
 *
 * Note that "cur_num" (and "max_num") represent the number of monsters
 * of the given race currently on (and allowed on) the current level.
 * This information yields the "dead" flag for Unique monsters.
 *
 * Note that "max_num" is reset when a new player is created.
 * Note that "cur_num" is reset when a new level is created.
 *
 * Note that several of these fields, related to "recall", can be
 * scrapped if space becomes an issue, resulting in less "complete"
 * monster recall (no knowledge of spells, etc).  All of the "recall"
 * fields have a special prefix to aid in searching for them.
 */
class CMonsterRace {
public:
    u16b name;                  // Name (offset)
    u16b text;                  // Text (offset)
    s16b tile;                  // Name of tile

    byte hdice;                 // Creatures hit dice count
    byte hside;                 // Creatures hit dice sides

    s16b ac;                    // Armor Class

    s16b sleep;                 // Inactive counter (base)
    byte aaf;                   // Area affect radius (1-100)
    byte speed;                 // Speed (normally 110)

    s32b mexp;                  // Exp value for kill

    byte type;                  // Type
    byte extra;                 // Unused (for now)

    byte freq_inate;            // Inate spell frequency
    byte freq_spell;            // Other spell frequency

    u32b flags1;                // Flags 1 (general)
    u32b flags2;                // Flags 2 (abilities)
    u32b flags3;                // Flags 3 (race/resist)
    u32b flags4;                // Flags 4 (inate/breath)
    u32b flags5;                // Flags 5 (normal spells)
    u32b flags6;                // Flags 6 (special spells)

    monster_blow blow[4];       // Up to four blows per round


    byte level;                 // Level of creature
    byte rarity;                // Rarity of creature


    byte max_num;               // Maximum population allowed per level
    byte cur_num;               // Monster population on current level


    s16b r_sights;              // Count sightings of this monster
    s16b r_deaths;              // Count deaths from this monster

    s16b r_pkills;              // Count monsters killed in this life
    s16b r_tkills;              // Count monsters killed in all lives

    byte r_wake;                // Number of times woken up (?)
    byte r_ignore;              // Number of times ignored (?)

    byte r_xtra1;               // Something (unused)
    byte r_xtra2;               // Something (unused)

    byte r_drop_gold;           // Max number of gold dropped at once
    byte r_drop_item;           // Max number of item dropped at once

    byte r_cast_inate;          // Max number of inate spells seen
    byte r_cast_spell;          // Max number of other spells seen

    byte r_blows[4];            // Number of times each blow type was seen

    u32b r_flags1;              // Observed racial flags
    u32b r_flags2;              // Observed racial flags
    u32b r_flags3;              // Observed racial flags
    u32b r_flags4;              // Observed racial flags
    u32b r_flags5;              // Observed racial flags
    u32b r_flags6;              // Observed racial flags

    bool isNonLiving(void) {
        if (flags3 & RF3_DEMON) return TRUE;
        if (flags3 & RF3_UNDEAD) return TRUE;
        if (type == TYPE_ELEMENT_SPIRIT) return TRUE;
        if (type == TYPE_ELEMENT_VORTEX) return TRUE;
        if (type == TYPE_GOLEM) return TRUE;
        return FALSE;
    }
};



/*
 * Information about "vault generation"
 */

struct vault_type {
    u16b name;                  // Name (offset)
    u16b text;                  // Text (offset)

    byte typ;                   // Vault type

    byte rat;                   // Vault rating

    byte hgt;                   // Vault height
    byte wid;                   // Vault width
};





// Declare this a bit early
class CGrid;


/*
 * A globally unique identifier
 */
typedef u32b guid_type;

/*
 * Any game object, from which everything is derived.
 */
class CGameObject {
private:
    guid_type guid;
public:
    CGameObject();
    guid_type GetGUID(void) { return guid; }
    virtual int GetTypeID(void) = 0;
};


/*
 * An entity, from which both objects and livings are derived.
 */
class CEntity: public CGameObject {
public:
    CCoord loc;                 // Location on map

    int GetX(void) { return loc.x; }
    int GetY(void) { return loc.y; }
    void GetLocation(int *xp, int *yp) { *xp = loc.x; *yp = loc.y; }

    void SetX(int i) { loc.x = i; }
    void SetY(int i) { loc.y = i; }
    void SetLocation(int nx, int ny) { loc.x = nx; loc.y = ny; }

    bool is_at(int xx, int yy) { return ((loc.x == xx) && (loc.y == yy)); }

    CGrid *get_g_ptr(void);
};


/*
 * Structure for an object.
 *
 * Note that a "discount" on an item is permanent and never goes away.
 *
 * Note that "object flags" must now be derived from the object kind,
 * the artifact and ego-item indexes, and the two "xtra" fields.
 */
class CItem: public CEntity {
private:
    s16b k_idx;                 // Kind index (zero if "dead")

    s16b pval;                  // Item extra-parameter
    byte discount;              // Discount (if any)
    byte number;                // Number of items

    byte name1;                 // Artifact type, if any
    byte name2;                 // Ego-Item type, if any

    byte xtra1;                 // Extra info type
    byte xtra2;                 // Extra info index

    s16b to_h;                  // Plusses to hit
    s16b to_d;                  // Plusses to damage
    s16b to_a;                  // Plusses to AC
    s16b ac;                    // Normal AC

    byte dd, ds;                // Damage dice/sides

    s16b timeout;               // Timeout Counter

    byte ident;                 // Special flags
    byte marked;                // Item is marked

    char *note;                 // Inscription index

public:
    CItem *next_i_ptr;          // Next object in pile

    void wipe(void);
    void invcopy(int k_idx);
    CItem() { wipe(); }
    CItem(int k_idx) { wipe(); invcopy(k_idx); }
    CItem(int tv, int sv);
    virtual ~CItem();

    s16b GetKIdx(void) { return k_idx; }
    byte GetTval(void) { return get_k_ptr()->tval; }
    byte GetSval(void) { return get_k_ptr()->sval; }
    s16b GetPval(void) { return pval; }
    byte GetDiscount(void) { return discount; }
    byte GetNumber(void) { return number; }
    s16b GetWeight(void);
    byte GetName1(void) { return name1; }
    byte GetName2(void) { return name2; }
    byte GetXtra1(void) { return xtra1; }
    byte GetXtra2(void) { return xtra2; }
    s16b GetToH(void) { return to_h; }
    s16b GetToD(void) { return to_d; }
    s16b GetToA(void) { return to_a; }
    s16b GetAC(void) { return ac; }
    byte GetDD(void) { return dd; }
    byte GetDS(void) { return ds; }
    s16b GetTimeout(void) { return timeout; }
    byte GetIdent(void) { return ident; }
    byte GetMarked(void) { return marked; }
    char *GetNote(void) { return note; }

    void SetKIdx(s16b n) { k_idx = n; }
    void SetPval(s16b n) { pval = n; }
    void SetDiscount(byte n) { discount = n; }
    void SetNumber(byte n) { number = n; }
    void SetName1(byte n) { name1 = n; }
    void SetName2(byte n) { name2 = n; }
    void SetXtra1(byte n) { xtra1 = n; }
    void SetXtra2(byte n) { xtra2 = n; }
    void SetToH(s16b n) { to_h = n; }
    void SetToD(s16b n) { to_d = n; }
    void SetToA(s16b n) { to_a = n; }
    void SetAC(s16b n) { ac = n; }
    void SetDD(byte n) { dd = n; }
    void SetDS(byte n) { ds = n; }
    void SetTimeout(s16b n) { timeout = n; }
    void SetIdent(byte n) { ident = n; }
    void SetMarked(byte n) { marked = n; }
    void SetNote(char *n);

    bool TestIdentFlag(byte flag) { return (ident & flag) ? TRUE : FALSE; }
    void SetIdentFlag(byte flags) { ident |= flags; }
    void ClearIdentFlag(byte flags) { ident &= ~flags; }

    bool exists(void) { return k_idx ? TRUE : FALSE; }
    bool isPlural(void) { return (number > 1); }
    bool isArtifact(void) { return name1 ? TRUE : FALSE; }
    bool isEgoItem(void) { return name2 ? TRUE : FALSE; }
    bool isCursed(void) { return TestIdentFlag(ID_CURSED); }
    bool isBroken(void) { return TestIdentFlag(ID_BROKEN); }
    bool isAware(void) { return get_k_ptr()->aware; }
    bool isTried(void) { return get_k_ptr()->tried; }
    bool isKnown(void);
    bool hatesAcid(void);
    bool hatesElec(void);
    bool hatesFire(void);
    bool hatesCold(void);

    CObjectKind *get_k_ptr(void);
    artifact_type *get_a_ptr(void);
    ego_item_type *get_e_ptr(void);

    s16b WieldSlot(void);
    s16b GetDamRoll(void);
    byte GetObjLevel(void) { return get_k_ptr()->level; }

    s32b GetValueBase(void);
    s32b GetValueReal(void);
    s32b GetValue(void);

    bool make_artifact_special(void);
    bool make_artifact(void);
    void charge_up(void);
    void a_m_aux_1(int level, int power);
    void a_m_aux_2(int level, int power);
    void a_m_aux_3(int level, int power);
    void a_m_aux_4(int level, int power);
    void apply_magic(int lev, byte flags);

    void MakeKnown(void);
    void object_tried(void);
    void object_aware(void);

    void GetFlags(u32b *f1, u32b *f2, u32b *f3);
    void object_desc(char *buf, int pref, int mode);
    void object_desc_store(char *buf, int pref, int mode);

    int GetTypeID(void) { return TYPEID_CITEM; }

    byte get_attr(void);

    void Process(void);
};



/* Living object */
class CLiving: public CEntity {
private:
    s16b mhp;                   // Max hit pts
    s16b chp;                   // Cur hit pts
    u16b chp_frac;              // Cur hit frac (times 2^16)

public:
    CLiving() { mhp = chp = chp_frac = 0; }

    s16b GetMHP(void) { return mhp; }
    s16b GetCHP(void) { return chp; }
    u16b GetCHPFrac(void) { return chp_frac; }

    void SetMHP(s16b n) { mhp = n; }
    void SetCHP(s16b n) { chp = n; }
    void SetCHPFrac(u16b n) { chp_frac = n; }

    void correct_hp_overflows(void);

    virtual void get_desc(char *desc, int mode) = 0;
};



/*
 * Monster information, for a specific monster.
 */
class CMonster: public CLiving {
private:
    s16b r_idx;                 // Monster race index

    s16b csleep;                // Inactive counter

    s16b busy;                  // Monster busy time (in energy units)

    s16b fast;                  // Monster is temporarily fast
    s16b slow;                  // Monster is temporarily slow
    s16b confused;              // Monster is confused
    s16b afraid;                // Monster is afraid
    s16b stun;                  // Monster is stunned

    byte cdis;                  // Current dis from player

    bool los;                   // Monster is in sight
    bool visible;               // Monster is visible
    bool spawned;               // Monster was spawned

public:
    CItem *i_ptr;               // Pointer to first object in inventory
    byte detect;                // Monster is detected
    s16b action;                // Current action

    void setup(void);

    CMonster(int race);

    s16b get_r_idx(void) { return r_idx; }
    s16b get_csleep(void) { return csleep; }
    byte get_speed(void);
    s16b get_busy(void) { return busy; }
    s16b get_fast(void) { return fast; }
    s16b get_slow(void) { return slow; }
    s16b get_confused(void) { return confused; }
    s16b get_afraid(void) { return afraid; }
    s16b get_stun(void) { return stun; }
    byte get_cdis(void) { return cdis; }
    bool get_los(void) { return los; }
    bool is_visible(void) { return visible; }
    bool get_spawned(void) { return spawned; }

    void set_r_idx(s16b n) { r_idx = n; }
    void set_csleep(s16b n) { csleep = n; }
    void set_busy(s16b n) { busy = n; if (busy < 0) busy = 0; }
    void set_fast(s16b n) { fast = n; }
    void set_slow(s16b n) { slow = n; }
    void set_confused(s16b n) { confused = n; }
    void set_afraid(s16b n) { afraid = n; }
    void set_stun(s16b n) { stun = n; }
    void set_cdis(byte n) { cdis = n; }
    void set_los(bool n) { los = n; }
    void set_visible(bool n) { visible = n; }
    void set_spawned(bool n) { spawned = n; }

    CMonsterRace *get_r_ptr(void);

    void update(void);

    void get_desc(char *desc, int mode);

    void monster_death(void);
    bool mon_take_hit(int dam, bool *fear, char *note);
    void message_pain(int dam);

    bool make_attack_normal(void);
    bool make_attack_spell(CLiving *target);

    void bolt(int typ, int dam_hp);
    void breath(int typ, int dam_hp);

    void lore_do_probe(void);
    void lore_treasure(int num_item, int num_gold);
    
    int mon_will_run(void);
    void get_moves(int *mm);

    int GetTypeID(void) { return TYPEID_CMONSTER; }
};




// An entry for the object kind allocator function
struct kind_entry {
    u16b k_idx;         // Item kind index
    byte locale;        // Base dungeon level
    byte chance;        // Rarity of occurance
};


// An entry for the monster race allocator function
struct race_entry {
    u16b r_idx;         // Monster race index
    byte locale;        // Base dungeon level
    byte chance;        // Rarity of occurance
};



/*
 * Available "options"
 *
 *      - Address of actual option variable (or NULL)
 *
 *      - Whether it should show up on the options screen in a checkbox
 *
 *      - Textual name (or NULL)
 *      - Textual description
 */
struct option_type {
    byte *o_var;
    byte o_show;
    char *o_text;
    char *o_desc;
};



/*
 * Structure for the "quests"
 *
 * Hack -- currently, only the "level" parameter is set, with the
 * semantics that "one (QUEST) monster of that level" must be killed,
 * and then the "level" is reset to zero, meaning "all done".  Later,
 * we should allow quests like "kill 100 fire hounds", and note that
 * the "quest level" is then the level past which progress is forbidden
 * until the quest is complete.  Note that the "QUESTOR" flag then could
 * become a more general "never out of depth" flag for monsters.
 *
 * Actually, in Angband 2.8.0 it will probably prove easier to restrict
 * the concept of quest monsters to specific unique monsters, and to
 * actually scan the dead unique list to see what quests are left.
 */
struct quest {
    int level;                  // Dungeon level
    int r_idx;                  // Monster race

    int cur_num;                // Number killed (unused)
    int max_num;                // Number required (unused)
};




/*
 * A store owner
 */
struct owner_type {
    char *owner_name;           // Name

    s16b max_cost;              // Purse limit

    byte max_inflate;           // Inflation (max)
    byte min_inflate;           // Inflation (min)

    byte haggle_per;            // Haggle unit

    byte insult_max;            // Insult limit

    byte owner_race;            // Owner race

    byte unused;                // Unused
};




/*
 * A store, with an owner, various state flags, a current stock
 * of items, and a table of items that are often purchased.
 */
struct store_type {
    byte owner;                 // Owner index
    byte extra;                 // Unused for now

    s16b insult_cur;            // Insult counter

    s16b good_buy;              // Number of "good" buys
    s16b bad_buy;               // Number of "bad" buys

    s32b store_open;            // Closed until this turn

    s32b store_wrap;            // Unused for now

    s16b table_num;             // Table -- Number of entries
    s16b table_size;            // Table -- Total Size of Array
    s16b *table;                // Table -- Legal item kinds

    s16b stock_num;             // Stock -- Number of entries
    s16b stock_size;            // Stock -- Total Size of Array
    CItem *stock;               // Stock -- Actual stock items
};





/*
 * The "name" of spell 'N' is stored as spell_names[X][N],
 * where X is 0 for mage-spells and 1 for priest-spells.
 */
struct magic_type {
    byte slevel;                // Required level (to learn)
    byte smana;                 // Required mana (to cast)
    byte sfail;                 // Minimum chance of failure
    byte sexp;                  // Encoded experience bonus
};


/*
 * Information about the player's "magic"
 *
 * Note that a player with a "spell_book" of "zero" is illiterate.
 */
struct player_magic {
    s16b spell_book;            // Tval of spell books (if any)
    s16b spell_xtra;            // Something for later

    s16b spell_stat;            // Stat for spells (if any)
    s16b spell_type;            // Spell type (mage/priest)

    s16b spell_first;           // Level of first spell
    s16b spell_weight;          // Weight that hurts spells

    magic_type info[64];        // The available spells
};



/*
 * Player racial info
 */
struct player_race {
    char *title;                // Name of race

    s16b r_adj[6];              // Racial stat bonuses

    s16b r_dis;                 // disarming
    s16b r_dev;                 // magic devices
    s16b r_sav;                 // saving throw
    s16b r_stl;                 // stealth
    s16b r_srh;                 // search ability
    s16b r_fos;                 // search frequency
    s16b r_thn;                 // combat (normal)
    s16b r_thb;                 // combat (shooting)

    byte r_mhp;                 // Race hit-dice modifier
    byte r_exp;                 // Race experience factor

    byte b_age;                 // base age
    byte m_age;                 // mod age

    byte m_b_ht;                // base height (males)
    byte m_m_ht;                // mod height (males)
    byte m_b_wt;                // base weight (males)
    byte m_m_wt;                // mod weight (males)

    byte f_b_ht;                // base height (females)
    byte f_m_ht;                // mod height (females)
    byte f_b_wt;                // base weight (females)
    byte f_m_wt;                // mod weight (females)

    byte infra;                 // Infra-vision range

    byte choice;                // Legal class choices
};


/*
 * Player class info
 */
struct player_class {
    char *title;                // Type of class

    s16b c_adj[6];              // Class stat modifier

    s16b c_dis;                 // class disarming
    s16b c_dev;                 // class magic devices
    s16b c_sav;                 // class saving throws
    s16b c_stl;                 // class stealth
    s16b c_srh;                 // class searching ability
    s16b c_fos;                 // class searching frequency
    s16b c_thn;                 // class to hit (normal)
    s16b c_thb;                 // class to hit (bows)

    s16b x_dis;                 // extra disarming
    s16b x_dev;                 // extra magic devices
    s16b x_sav;                 // extra saving throws
    s16b x_stl;                 // extra stealth
    s16b x_srh;                 // extra searching ability
    s16b x_fos;                 // extra searching frequency
    s16b x_thn;                 // extra to hit (normal)
    s16b x_thb;                 // extra to hit (bows)

    s16b c_mhp;                 // Class hit-dice adjustment
    s16b c_exp;                 // Class experience factor
};




/*
 * Most of the "player" information goes here.
 *
 * This stucture gives us a large collection of player variables.
 *
 * This structure contains several "blocks" of information.
 *   (1) the "permanent" info
 *   (2) the "variable" info
 *   (3) the "transient" info
 *
 * All of the "permanent" info, and most of the "variable" info,
 * is saved in the savefile.  The "transient" info is recomputed
 * whenever anything important changes.
 */
class CPlayer: public CLiving {
private:
    byte prace;                 // Race index
    byte pclass;                // Class index
    byte male;                  // Sex of character


    s32b gold;                  // Current Gold

    s32b max_exp;               // Max experience
    s32b exp;                   // Cur experience
    u16b exp_frac;              // Cur exp frac (times 2^16)

    s16b lev;                   // Level

    s16b msp;                   // Max mana pts
    s16b csp;                   // Cur mana pts
    u16b csp_frac;              // Cur mana frac (times 2^16)

    s16b max_plv;               // Max Player Level
    s16b max_dlv;               // Max level explored

    s16b stat_max[6];           // Current "maximal" stat values
    s16b stat_cur[6];           // Current "natural" stat values

    s16b fast;                  // Timed -- Fast
    s16b slow;                  // Timed -- Slow
    s16b blind;                 // Timed -- Blindness
    s16b paralyzed;             // Timed -- Paralysis
    s16b confused;              // Timed -- Confusion
    s16b afraid;                // Timed -- Fear
    s16b poisoned;              // Timed -- Poisoned
    s16b cut;                   // Timed -- Cut
    s16b stun;                  // Timed -- Stun

    s16b protevil;              // Timed -- Protection
    s16b shadowform;            // Timed -- Shadowform
    s16b hero;                  // Timed -- Heroism
    s16b shero;                 // Timed -- Super Heroism
    s16b shield;                // Timed -- Shield Spell
    s16b blessed;               // Timed -- Blessed
    s16b tim_invis;             // Timed -- See Invisible
    s16b tim_infra;             // Timed -- Infra Vision

    s16b oppose_acid;           // Timed -- oppose acid
    s16b oppose_elec;           // Timed -- oppose lightning
    s16b oppose_fire;           // Timed -- oppose heat
    s16b oppose_cold;           // Timed -- oppose cold
    s16b oppose_pois;           // Timed -- oppose poison

    s16b word_recall;           // Word of recall counter

    s16b busy;                  // Current busy time (in energy units)

    s16b food;                  // Current nutrition

    byte confusing;             // Glowing hands

    s16b new_spells;            // Number of spells available
    s16b old_spells;            // Former number of spells available

    bool no_score;              // Cheats used so far


    bool cumber_armor;          // Mana draining armor
    bool cumber_glove;          // Mana draining gloves
    bool heavy_wield;           // Heavy weapon
    bool heavy_shoot;           // Heavy shooter
    bool icky_wield;            // Icky weapon

    s16b cur_lite;              // Radius of lite (if any)

    u32b notice;                // Special Updates (bit flags)

    u32b update;                // Pending Updates (bit flags)

    s16b stat_use[6];           // Current modified stats
    s16b stat_top[6];           // Maximal modified stats

    s16b stat_add[6];           // Modifiers to stat values
    s16b stat_ind[6];           // Indexes into stat tables

    bool immunes[MAX_IMMUNE];   // Immunities
    bool resists[MAX_RESIST];   // Resistances
    bool sustains[6];           // Sustains

    bool aggravate;             // Aggravate monsters
    bool teleport;              // Random teleporting

    bool exp_drain;             // Experience draining

    bool ffall;                 // No damage falling
    bool free_act;              // Never paralyzed
    byte lite;                  // Amount of permanent light
    bool see_inv;               // Can see invisible
    bool regenerate;            // Regenerate hit pts
    bool hold_life;             // Resist life draining
    bool telepathy;             // Telepathy
    bool slow_digest;           // Slower digestion
    bool bless_blade;           // Blessed blade
    bool xtra_might;            // Extra might bow
    bool impact;                // Earthquake blows

    s16b dis_to_h;              // Known bonus to hit
    s16b dis_to_d;              // Known bonus to dam
    s16b dis_to_a;              // Known bonus to ac

    s16b dis_ac;                // Known base ac

    s16b to_h;                  // Bonus to hit
    s16b to_d;                  // Bonus to dam
    s16b to_a;                  // Bonus to ac

    s16b ac;                    // Base ac

    s16b see_infra;             // Infravision range

    s16b skills[MAX_SKILL];     // The various skills

    s16b num_blow;              // Number of blows
    s16b num_fire;              // Number of shots

    byte tval_xtra;             // Correct xtra tval

    byte tval_ammo;             // Correct ammo tval

    s16b speed;                 // Current speed

    s16b dest_x, dest_y;        // Destination planned, if a cave grid
    guid_type dest_mon;         // Destination planned, if a monster

public:
    s16b player_hp[PY_MAX_LEVEL];
    s16b action;
    s16b last_move;

    CPlayer();

    byte GetRace(void) { return prace; }
    byte GetClass(void) { return pclass; }
    byte GetMale(void) { return male; }
    byte GetExpFact(void);

    void SetRace(byte i) { prace = i; }
    void SetClass(byte i) { pclass = i; }
    void SetMale(byte i) { male = i; }

    s32b GetGold(void) { return gold; }
    s32b GetMaxExp(void) { return max_exp; }
    s32b GetExp(void) { return exp; }
    u16b GetExpFrac(void) { return exp_frac; }
    s16b GetLev(void) { return lev; }
    s16b GetMSP(void) { return msp; }
    s16b GetCSP(void) { return csp; }
    u16b GetCSPFrac(void) { return csp_frac; }
    s16b GetMaxPlv(void) { return max_plv; }
    s16b GetMaxDlv(void) { return max_dlv; }
    s16b GetStatMax(int i) { return stat_max[i]; }
    s16b GetStatCur(int i) { return stat_cur[i]; }
    s16b GetFast(void) { return fast; }
    s16b GetSlow(void) { return slow; }
    s16b GetBlind(void) { return blind; }
    s16b GetParalyzed(void) { return paralyzed; }
    s16b GetConfused(void) { return confused; }
    s16b GetAfraid(void) { return afraid; }
    s16b GetPoisoned(void) { return poisoned; }
    s16b GetCut(void) { return cut; }
    s16b GetStun(void) { return stun; }
    s16b GetProtevil(void) { return protevil; }
    s16b GetShadowform(void) { return shadowform; }
    s16b GetHero(void) { return hero; }
    s16b GetSHero(void) { return shero; }
    s16b GetShield(void) { return shield; }
    s16b GetBlessed(void) { return blessed; }
    s16b GetTimInvis(void) { return tim_invis; }
    s16b GetTimInfra(void) { return tim_infra; }
    s16b GetOpposeAcid(void) { return oppose_acid; }
    s16b GetOpposeElec(void) { return oppose_elec; }
    s16b GetOpposeFire(void) { return oppose_fire; }
    s16b GetOpposeCold(void) { return oppose_cold; }
    s16b GetOpposePois(void) { return oppose_pois; }
    s16b GetWordRecall(void) { return word_recall; }
    s16b GetBusy(void) { return busy; }
    s16b GetFood(void) { return food; }
    byte GetConfusing(void) { return confusing; }
    s16b GetNewSpells(void) { return new_spells; }
    s16b GetOldSpells(void) { return old_spells; }
    bool GetNoScore(void) { return no_score; }

    void SetGold(s32b i) { gold = i; }
    void SetMaxExp(s32b i) { max_exp = i; }
    void SetExp(s32b i) { exp = i; }
    void SetExpFrac(u16b i) { exp_frac = i; }
    void SetLev(s16b i) { lev = i; }
    void SetMSP(s16b i) { msp = i; }
    void SetCSP(s16b i) { csp = i; }
    void SetCSPFrac(u16b i) { csp_frac = i; }
    void SetMaxPlv(s16b i) { max_plv = i; }
    void SetMaxDlv(s16b i) { max_dlv = i; }
    void SetStatMax(int s, s16b i) { stat_max[s] = i; }
    void SetStatCur(int s, s16b i) { stat_cur[s] = i; }
    void SetFast(s16b i) { fast = i; }
    void SetSlow(s16b i) { slow = i; }
    void SetBlind(s16b i) { blind = i; }
    void SetParalyzed(s16b i) { paralyzed = i; }
    void SetConfused(s16b i) { confused = i; }
    void SetAfraid(s16b i) { afraid = i; }
    void SetPoisoned(s16b i) { poisoned = i; }
    void SetCut(s16b i) { cut = i; }
    void SetStun(s16b i) { stun = i; }
    void SetProtevil(s16b i) { protevil = i; }
    void SetShadowform(s16b i) { shadowform = i; }
    void SetHero(s16b i) { hero = i; }
    void SetSHero(s16b i) { shero = i; }
    void SetShield(s16b i) { shield = i; }
    void SetBlessed(s16b i) { blessed = i; }
    void SetTimInvis(s16b i) { tim_invis = i; }
    void SetTimInfra(s16b i) { tim_infra = i; }
    void SetOpposeAcid(s16b i) { oppose_acid = i; }
    void SetOpposeElec(s16b i) { oppose_elec = i; }
    void SetOpposeFire(s16b i) { oppose_fire = i; }
    void SetOpposeCold(s16b i) { oppose_cold = i; }
    void SetOpposePois(s16b i) { oppose_pois = i; }
    void SetWordRecall(s16b i) { word_recall = i; }
    void SetBusy(s16b i) { busy = i; }
    void SetFood(s16b i) { food = i; }
    void SetConfusing(byte i) { confusing = i; }
    void SetNewSpells(s16b i) { new_spells = i; }
    void SetOldSpells(s16b i) { old_spells = i; }
    void SetNoScore(bool i) { no_score = i; }

    bool get_cumber_armor(void) { return cumber_armor; }
    bool get_cumber_glove(void) { return cumber_glove; }
    bool get_heavy_wield(void) { return heavy_wield; }
    bool get_heavy_shoot(void) { return heavy_shoot; }
    bool get_icky_wield(void) { return icky_wield; }
    s16b get_cur_lite(void) { return cur_lite; }
    u32b get_notice(void) { return notice; }
    u32b get_update(void) { return update; }
    s16b GetStatUse(int i) { return stat_use[i]; }
    s16b GetStatTop(int i) { return stat_top[i]; }
    s16b GetStatAdd(int i) { return stat_add[i]; }
    s16b GetStatInd(int i) { return stat_ind[i]; }
    bool get_immunes(int i) { return immunes[i]; }
    bool get_resists(int i) { return resists[i]; }
    bool get_sustains(int i) { return sustains[i]; }
    bool get_aggravate(void) { return aggravate; }
    bool get_teleport(void) { return teleport; }
    bool get_exp_drain(void) { return exp_drain; }
    bool get_ffall(void) { return ffall; }
    bool get_free_act(void) { return free_act; }
    byte get_lite(void) { return lite; }
    bool get_see_inv(void) { return see_inv; }
    bool get_regenerate(void) { return regenerate; }
    bool get_hold_life(void) { return hold_life; }
    bool get_telepathy(void) { return telepathy; }
    bool get_slow_digest(void) { return slow_digest; }
    bool get_bless_blade(void) { return bless_blade; }
    bool get_xtra_might(void) { return xtra_might; }
    bool get_impact(void) { return impact; }
    s16b get_dis_to_h(void) { return dis_to_h; }
    s16b get_dis_to_d(void) { return dis_to_d; }
    s16b get_dis_to_a(void) { return dis_to_a; }
    s16b get_dis_ac(void) { return dis_ac; }
    s16b get_to_h(void) { return to_h; }
    s16b get_to_d(void) { return to_d; }
    s16b get_to_a(void) { return to_a; }
    s16b get_ac(void) { return ac; }
    s16b GetTotalDisAC(void) { return dis_ac + dis_to_a; }
    s16b GetTotalAC(void) { return ac + to_a; }
    s16b get_see_infra(void) { return see_infra; }
    s16b GetSkill(int i) { return skills[i]; }
    s16b get_num_blow(void) { return num_blow; }
    s16b get_num_fire(void) { return num_fire; }
    byte get_tval_xtra(void) { return tval_xtra; }
    byte get_tval_ammo(void) { return tval_ammo; }
    s16b get_speed(void) { return speed; }
    s16b get_dest_x(void) { return dest_x; }
    s16b get_dest_y(void) { return dest_y; }
    guid_type get_dest_mon(void) { return dest_mon; }

    void set_cumber_armor(bool i) { cumber_armor = i; }
    void set_cumber_glove(bool i) { cumber_glove = i; }
    void set_heavy_wield(bool i) { heavy_wield = i; }
    void set_heavy_shoot(bool i) { heavy_shoot = i; }
    void set_icky_wield(bool i) { icky_wield = i; }
    void set_cur_lite(s16b i) { cur_lite = i; }
    void set_notice(u32b i) { notice = i; }
    void set_update(u32b i) { update = i; }
    void set_stat_use(int s, s16b i) { stat_use[s] = i; }
    void set_stat_top(int s, s16b i) { stat_top[s] = i; }
    void set_stat_add(int s, s16b i) { stat_add[s] = i; }
    void set_stat_ind(int s, s16b i) { stat_ind[s] = i; }
    void set_immunes(int ind, bool i) { immunes[ind] = i; }
    void set_resists(int ind, bool i) { resists[ind] = i; }
    void set_sustains(int ind, bool i) { sustains[ind] = i; }
    void set_aggravate(bool i) { aggravate = i; }
    void set_teleport(bool i) { teleport = i; }
    void set_exp_drain(bool i) { exp_drain = i; }
    void set_ffall(bool i) { ffall = i; }
    void set_free_act(bool i) { free_act = i; }
    void set_lite(byte i) { lite = i; }
    void set_see_inv(bool i) { see_inv = i; }
    void set_regenerate(bool i) { regenerate = i; }
    void set_hold_life(bool i) { hold_life = i; }
    void set_telepathy(bool i) { telepathy = i; }
    void set_slow_digest(bool i) { slow_digest = i; }
    void set_bless_blade(bool i) { bless_blade = i; }
    void set_xtra_might(bool i) { xtra_might = i; }
    void set_impact(bool i) { impact = i; }
    void set_dis_to_h(s16b i) { dis_to_h = i; }
    void set_dis_to_d(s16b i) { dis_to_d = i; }
    void set_dis_to_a(s16b i) { dis_to_a = i; }
    void set_dis_ac(s16b i) { dis_ac = i; }
    void set_to_h(s16b i) { to_h = i; }
    void set_to_d(s16b i) { to_d = i; }
    void set_to_a(s16b i) { to_a = i; }
    void set_ac(s16b i) { ac = i; }
    void set_see_infra(s16b i) { see_infra = i; }
    void SetSkill(int ind, s16b i) { skills[ind] = i; }
    void AddSkill(int ind, s16b i) { skills[ind] += i; }
    void set_num_blow(s16b i) { num_blow = i; }
    void set_num_fire(s16b i) { num_fire = i; }
    void set_tval_xtra(byte i) { tval_xtra = i; }
    void set_tval_ammo(byte i) { tval_ammo = i; }
    void set_speed(s16b i) { speed = i; }
    void set_dest_x(s16b i) { dest_x = i; }
    void set_dest_y(s16b i) { dest_y = i; }
    void set_dest_mon(guid_type i) { dest_mon = i; }
    void kill_destination(void) { dest_x = -1; dest_mon = 0; }

    bool mod_blind(int v);
    bool mod_confused(int v);
    bool mod_poisoned(int v);
    bool mod_afraid(int v);
    bool mod_paralyzed(int v);
    bool mod_image(int v);
    bool mod_fast(int v);
    bool mod_slow(int v);
    bool mod_shield(int v);
    bool mod_blessed(int v);
    bool mod_hero(int v);
    bool mod_shero(int v);
    bool mod_protevil(int v);
    bool mod_shadowform(int v);
    bool mod_tim_invis(int v);
    bool mod_tim_infra(int v);
    bool mod_oppose_acid(int v);
    bool mod_oppose_elec(int v);
    bool mod_oppose_fire(int v);
    bool mod_oppose_cold(int v);
    bool mod_oppose_pois(int v);
    bool mod_stun(int v);
    bool mod_cut(int v);
    bool mod_food(int v);

    // Get race-based info
    char *GetRaceTitle(void);

    // Get class-based info
    char *GetClassTitle(void);


    bool can_see_bold(int y, int x);
    bool no_lite(void);

    bool restore_level();
    void check_experience(void);
    void gain_exp(s32b amount);
    void lose_exp(s32b amount);

    void take_hit(int damage, char *hit_from);
    void take_hit_internal(int damage, char *hit_from);

    bool heal_up(int num);

    int GetTypeID(void) { return TYPEID_CPLAYER; }

    void get_desc(char *desc, int mode);

    void combine_pack(void);
    void reorder_pack(void);
    void SenseInventory(void);
    void RegenHP(int percent);
    void RegenSP(int percent);
    void Process(void);

    bool isBusy(void) { return (busy > 0); }
    void DrainEnergy(int quantity) { busy += quantity; }

    s16b GetTotalWeight(void);

    void attack(CMonster *m_ptr);

    void GetSubtilePosition(int *x, int *y);
    void Draw(int mcx, int mcy);
};



/*
 * A single "grid" in the map
 *
 * Note that several aspects of the code restrict the actual cave
 * to a max size of 256 by 256.  In partcular, locations are often
 * saved as bytes, limiting each coordinate to the 0-255 range.
 *
 * The "i_idx" and "m_idx" fields are very interesting.  There are
 * many places in the code where we need quick access to the actual
 * monster or object(s) in a given cave grid.  The easiest way to
 * do this is to simply keep the index of the monster and object
 * (if any) with the grid, but takes a lot of memory.  Several other
 * methods come to mind, but they all seem rather complicated.
 */
class CGrid: public CGameObject {
private:
    s16b feat;                  // Feature type

public:
    u16b flags;                 // Grid flags
    CItem *i_ptr;               // Item pointer or NULL
    CMonster *m_ptr;            // Monster pointer or NULL
    byte variant;               // Variant

    void wipe(void) { feat = flags = variant = 0; i_ptr = NULL; m_ptr = NULL; }
    CGrid() { wipe(); }

    s16b get_feat(void) { return feat; }
    void set_feat(s16b i) { feat = i; }

    bool is_permawall(void);
    bool is_wall(void);
    bool is_locked_door(void);
    bool is_door(void);
    bool is_store_door(void);
    bool is_visible_trap(void);
    bool is_glowing(void) { return ((flags & MAP_LITE) || (flags & MAP_GLOW)); }

    int get_door_lock_strength(void);
    int get_store_type(void);

    int GetTypeID(void) { return TYPEID_CGRID; }
};


/*
 * An arrow-style projectile
 */
class CArrow: public CGameObject {
private:
    double x, y, z;
    double vx, vy, vz;
    CItem *i_ptr;
    s16b chance, damage;
    bool purge;
    CLiving *who;

    void HitEdge(void) { purge = TRUE; }
    void HitWall(int old_x, int old_y);
    void HitFloor(void);
    void HitPlayer(void);
    void HitMonster(void);

public:
    CArrow(double xx, double yy, double zz, double vxx, double vyy, double vzz, CLiving *w,
        CItem *arrow, int ch, int td);
    CArrow(FILE *f);
    virtual ~CArrow() { if (i_ptr) delete i_ptr; }

    int GetTypeID(void) { return TYPEID_CPROJECTILE; }

    void GetIntLoc(int *xx, int *yy);
    void GetRealLoc(double *xx, double *yy) { *xx = x; *yy = y; }
    double GetHeight(void) { return z; }
    bool ToBePurged(void) { return purge; }

    void Process();

    void Write(void);
};
