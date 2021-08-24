#include "angband.h"

#include "mon.h"
#include <assert.h>

/************************************************************************
 * Monster Allocation Table
 *
 * Monsters are chosen from an allocation table (vec<mon_race_ptr>) by
 * applying a small number of user defined filters.
 * The table to use is chosen as follows:
 *   [1] Pass a custom table
 *   [2] Use table for current level if any (cave->mon_alloc_tbl)
 *   [3] Use table for current dungeon type (dun_type()->mon_alloc_tbl)
 *   [4] Use table for current world (dun_mgr()->world->mon_alloc_tbl)
 *   [5] Use global allocation table (mon_alloc_tbl defined here).
 * This allows for themed levels (set cave->mon_alloc_tbl) as well
 * as themed dungeons, or worlds. Use vec_filter to filter the global
 * table as needed. The global table is sorted by depth, and vec_filter
 * will maintain the sort order, but if you do something crazy, be sure
 * to sort your table before using it.
 ************************************************************************/
vec_ptr mon_alloc_tbl;
#define _MAX_FILTERS 10
static mon_race_p _filters[_MAX_FILTERS];
static int _filter_ct = 0;

#define _MAX_WEIGHTS 5
static mon_alloc_weight_f _weights[_MAX_WEIGHTS];
static int _weight_ct = 0;

static int _mon_alloc_cmp(mon_race_ptr left, mon_race_ptr right)
{
    if (left->level < right->level) return -1;
    if (left->level > right->level) return 1;
    return 0;
}
vec_ptr mon_alloc_current_tbl(void)
{
    if (cave->mon_alloc_tbl) return cave->mon_alloc_tbl;
    if (dun_type()->mon_alloc_tbl) return dun_type()->mon_alloc_tbl;
    if (dun_mgr()->world->mon_alloc_tbl) return dun_mgr()->world->mon_alloc_tbl;
    return mon_alloc_tbl;
}
void mon_alloc_init(void)
{
    int i;
    if (mon_alloc_tbl) vec_free(mon_alloc_tbl); /* allow re-initialization */
    mon_alloc_tbl = vec_alloc(NULL);
    for (i = 1; i < max_r_idx; i++)
    {
        mon_race_ptr race = &r_info[i];
        if (race->rarity == 0 || race->rarity > 100) continue;
        vec_add(mon_alloc_tbl, race);
    }
    vec_sort(mon_alloc_tbl, (vec_cmp_f)_mon_alloc_cmp);
}
void mon_alloc_clear_filters(void)
{
    int i;
    for (i = 0; i < _MAX_FILTERS; i++)
        _filters[i] = NULL;
    _filter_ct = 0;
}
void mon_alloc_push_filter(mon_race_p filter)
{
    assert(_filter_ct < _MAX_FILTERS);
    assert(filter);
    if (_filter_ct < _MAX_FILTERS)
        _filters[_filter_ct++] = filter;
}
void mon_alloc_pop_filter(void)
{
    assert(_filter_ct > 0);
    if (_filter_ct > 0) _filter_ct--;
}
void mon_alloc_push_weight(mon_alloc_weight_f weight)
{
    assert(_weight_ct < _MAX_WEIGHTS);
    assert(weight);
    if (_weight_ct < _MAX_WEIGHTS)
        _weights[_weight_ct++] = weight;
}
void mon_alloc_pop_weight(void)
{
    assert(_weight_ct > 0);
    if (_weight_ct > 0) _weight_ct--;
}
mon_race_ptr mon_alloc_choose(int level)
{
    return mon_alloc_choose_aux(level, GMN_DEFAULT);
}
mon_race_ptr mon_alloc_choose_aux(int level, u32b options)
{
    return mon_alloc_choose_aux2(mon_alloc_current_tbl(), level, 0, options);
}
static int _mon_alloc_prob(mon_race_ptr race, int level, int min_level, u32b options)
{
    int i, prob;

    if (race->max_level && !(options & GMN_IGNORE_MAX_LEVEL) && race->max_level < level) return 0;
    if (race->level < min_level) return 0;
    if (quests_get_current() && (race->flags1 & RF1_NO_QUEST)) return 0;
    if (summon_specific_who != SUMMON_WHO_NOBODY && (race->flags1 & RF1_NO_SUMMON)) return 0;
    if (race->dun_type_id)
    {
        #if 0
        if ((options & GMN_QUESTOR) && race->dun_type_id == D_AMBER) { /* XXX */ }
        else if (race->dun_type_id != cave->dun_type_id) return 0;
        #else
        if (race->dun_type_id != cave->dun_type_id) return 0;
        #endif
    }
    if (!(options & GMN_ALLOW_DEAD_UNIQUES)) /* Chameleon Lord */
    {
        if (race->cur_num >= race->max_num) return 0;
        if ((race->flags7 & RF7_UNIQUE2) && race->cur_num) return 0; 
    }
    if ((race->flags1 & RF1_UNIQUE) && (options & GMN_NO_UNIQUES)) return 0;
    if (race->id == MON_BANORLUPART)
    {
        if (mon_race_lookup(MON_BANOR)->cur_num) return 0;
        if (mon_race_lookup(MON_LUPART)->cur_num) return 0;
    }
    if (race->rarity == 0 || race->rarity > 100) return 0;
    if (!chameleon_change_mon && summon_specific_type != SUMMON_GUARDIAN)
    {
        if (race->flagsx & (RFX_QUESTOR | RFX_SUPPRESS | RFX_GUARDIAN)) return 0;
        if (race->flags7 & RF7_GUARDIAN) return 0;
        if ((race->flags1 & RF1_FORCE_DEPTH) && race->level > cave->dun_lvl) return 0;
    }

    for (i = 0; i < _filter_ct; i++)
        if (!_filters[i](race)) return 0;

    prob = 100/race->rarity;

    /* Hack: Undersized monsters become more rare ... but only for max_depth restricted monsters.
       The goal is that these monsters gradually become less and less common, rather than suddenly
       disappearing. About 50% of monsters currently have depth restrictions. */
    if ( race->max_level /* <=== Remove this, and the end game becomes too difficult */
      && level > race->level + 9
      && !(race->flags1 & RF1_UNIQUE) ) /* Redundant. Uniques never have depth restrictions. */
    {
        int delta = level - race->level;
        prob = prob >> (delta/10);
        if (!prob) prob = 1;
    }

    for (i = 0; i < _weight_ct; i++)
        prob = _weights[i](race, prob);

   {dun_type_ptr type = dun_type();
    if (type->mon_alloc_f)
        prob = type->mon_alloc_f(type, race, prob);}

    return prob;
}
mon_race_ptr mon_alloc_choose_aux2(vec_ptr tbl, int level, int min_level, u32b options)
{
    int i, roll, total = 0;
    int rolls = (options & GMN_POWER_BOOST) ? 3 : 1;
    mon_race_ptr best = NULL;

    /* Boost level if allowed */
    if ((options & GMN_ALLOW_OOD) && level > 0)
    {
        for (i = 0; i < 2; i++)
        {
            if (one_in_(NASTY_MON))
                level += MIN(5, level/10);
        }
    }
    if (level > MAX_DEPTH - 1) level = MAX_DEPTH - 1;

    /* Pass 1: Count up total probability */
    for (i = 0; i < vec_length(tbl); i++)
    {
        mon_race_ptr race = vec_get(tbl, i);
        if (race->level > level) break;  /* assume tbl is sorted */
        race->scratch = _mon_alloc_prob(race, level, min_level, options);
        total += race->scratch;
    }
    if (options & GMN_DEBUG)
    {
        doc_ptr doc = doc_alloc(80);
        doc_printf(doc, "<color:G>Monster Allocation Table for <color:R>L%d</color></color>\n", level);
        for (i = 0; i < vec_length(tbl); i++)
        {
            mon_race_ptr race = vec_get(tbl, i);
            if (race->level > level) break;  /* assume tbl is sorted */
            if (!race->scratch) continue;
            doc_printf(doc, "%5d %3d.%d%% ML%2d %s %d of %d\n", race->scratch,
                race->scratch*100/total, (race->scratch*1000/total)%10,
                race->level, r_name + race->name, race->cur_num, race->max_num);
        }
        doc_display(doc, "mon_alloc", 0);
        doc_free(doc);
    }
    if (total <= 0) return NULL;
    /* Pass 2: Pick best of N */
    if (summon_specific_who == SUMMON_WHO_NOBODY && !(options & GMN_QUESTOR))
    {
        while (rolls < 5 && randint0(20000) < level*level)
            rolls++;
    }
    assert(rolls > 0);
    for (roll = 0; roll < rolls; roll++)
    {
        mon_race_ptr race = NULL;
        int pick = randint0(total);
        for (i = 0; i < vec_length(tbl); i++)
        {
            race = vec_get(tbl, i);
            assert(race->level <= level);
            if (race->level > level) break;  /* assume tbl is sorted */
            pick -= race->scratch;
            if (pick < 0) break;
        }
        assert(race != NULL);
        if (!best || race->level > best->level) best = race;
    }
    assert(best);
    return best;
}

/* D_SURFACE allocation */
bool mon_alloc_town(mon_race_ptr race)
    { return BOOL(race->flags8 & (RF8_WILD_TOWN)); }
bool mon_alloc_ocean(mon_race_ptr race)
    { return BOOL(race->flags8 & RF8_WILD_OCEAN); }
bool mon_alloc_shore(mon_race_ptr race)
    { return BOOL(race->flags8 & RF8_WILD_SHORE); }
bool mon_alloc_waste(mon_race_ptr race)
    { return BOOL(race->flags8 & (RF8_WILD_WASTE | RF8_WILD_ALL)); }
bool mon_alloc_woods(mon_race_ptr race)
    { return BOOL(race->flags8 & (RF8_WILD_WOOD | RF8_WILD_ALL)); }
bool mon_alloc_mountain(mon_race_ptr race)
    { return BOOL(race->flags8 & RF8_WILD_MOUNTAIN); }
bool mon_alloc_grass(mon_race_ptr race)
    { return BOOL(race->flags8 & (RF8_WILD_GRASS | RF8_WILD_ALL)); }
bool mon_alloc_volcano(mon_race_ptr race)
    { return BOOL(race->flags8 & RF8_WILD_VOLCANO); }
bool mon_alloc_surface(mon_race_ptr race)
    { return BOOL(race->flags8); }

/* dungeon allocation */
bool mon_alloc_dungeon(mon_race_ptr race)
{ 
    if (cave->dun_type_id == D_SURFACE) return TRUE; /* XXX ignore this filter if on surface */

    /* Allow Gwaihir, Thorondor and Meneldor to appear in "The Mountain" */
    if (cave->dun_type_id == D_MOUNTAIN && race->d_char == 'B' && (race->flags8 & RF8_WILD_MOUNTAIN))
        return TRUE;
    if (cave->dun_type_id == D_RANDOM_MOUNTAIN && race->d_char == 'B' && (race->flags8 & RF8_WILD_MOUNTAIN))
        return TRUE;
    if (cave->dun_type_id == D_RANDOM_SEA && (race->flags8 & RF8_WILD_OCEAN))
        return TRUE;
    if (cave->dun_type_id == D_RANDOM_VOLCANO && (race->flags8 & RF8_WILD_VOLCANO))
        return TRUE;
    return !(race->flags8 & RF8_WILD_ONLY); 
}
bool mon_alloc_deep_water(mon_race_ptr race)
{ 
    if (!mon_alloc_dungeon(race)) return FALSE;
    return BOOL(race->flags7 & RF7_AQUATIC);
}
bool mon_alloc_shallow_water(mon_race_ptr race)
{
    if (!mon_alloc_dungeon(race)) return FALSE;
    return !mon_auras_find(race, GF_FIRE);
}
bool mon_alloc_floor(mon_race_ptr race)
{ 
    if (!mon_alloc_dungeon(race)) return FALSE;
    return !(race->flags7 & RF7_AQUATIC) || (race->flags7 & RF7_CAN_FLY);
}
bool mon_alloc_lava(mon_race_ptr race)
{
    if (!mon_alloc_dungeon(race)) return FALSE;
    if (race->flagsr & RFR_EFF_IM_FIRE_MASK) return TRUE;
    return (race->flags7 & RF7_CAN_FLY) && !mon_auras_find(race, GF_COLD);
}

mon_race_p mon_alloc_feat_p(int feat_id)
{
    feat_ptr feat = &f_info[feat_id];
    if (have_flag(feat->flags, FF_WATER))
    {
        if (have_flag(feat->flags, FF_DEEP)) return mon_alloc_deep_water;
        else return mon_alloc_shallow_water;
    }
    else if (have_flag(feat->flags, FF_LAVA))
        return mon_alloc_lava;
    return mon_alloc_floor;
}

/************************************************************************
 * Monsters
 ************************************************************************/
mon_ptr mon_alloc(void)
{
    mon_ptr mon = malloc(sizeof(mon_t));
    memset(mon, 0, sizeof(mon_t));
    return mon;
}

void mon_free(mon_ptr mon)
{
    if (!mon) return;
    mon_tim_clear(mon);
    free(mon);
}

dun_ptr mon_dun(mon_ptr mon)
{
    return dun_mgr_dun(mon->dun_id);
}

int mon_ac(mon_ptr mon)
{
    mon_race_ptr race = mon_race(mon);
    int          ac = race->ac;

    ac += mon->ac_adj;
    if (ac < 0) ac = 0;
    /* XXX timed buffs */
    return ac;
}

/* Should we show a message to the player for this monster?
 * When monsters battle each other, especially off screen, this
 * can generate a lot of message spam. cf ignore_unview.
 * Messages about monster status should generally be guarded:
 *   if (mon_show_msg(mon))
 *      msg_format("%s is upset!", m_name);
 * However, if the player is the cause, you probably just want:
 *   if (mon->ml)
 *      msg_format("%s resists!", m_name);
 * The reason is that player_can_see_bold() requires illumination,
 * not just telepathy. I guess this distinction is debatable, but
 * the gameplay effects for ranged spells are confusing wrt resistance
 * when mon_show_msg is used. (formerly, this code was is_seen()).
 * XXX Comment written before MFLAG2_FUZZY. Review? */
bool mon_show_msg(mon_ptr mon)
{
    assert(mon);
    if (mon->dun_id != p_ptr->dun_id) return FALSE;
    if (!mon->ml) return FALSE;
    if (plr_tim_find(T_BLIND)) return FALSE;
    if (!ignore_unview) return TRUE;
    return plr_can_see(mon->pos) && plr_project(mon->pos);
}

mon_race_ptr mon_race_lookup(int id)
{
    return &r_info[id];
}

/* mon->r_idx vs mon->ap_r_idx is a bit confusing (to me, anyway)
 * [1] Generally, these are the same.
 * [2] r_idx is what the monster is.
 * [3] ap_r_idx is what the monster appears to be to the player.
 * [4] Tanuki: r_idx = MON_TANUKI. ap_r_idx = current mask. As far as the game
 *     is concerned (monster flags, speed, etc), the monster is a Tanuki. But
 *     the player sees something else (e.g. a Death mold that is moving around!).
 * [5] Chameleon: r_idx = ap_r_idx = current form. Uses MFLAG2_CHAMELEON
 *     to remember that it really is a chameleon. This monster is a shapeshifter
 *     and truly does become different monster forms. You never kill or encounter
 *     a monster with r_idx == MON_CHAMELEON.
 * [6] Shadower: r_idx = true form. ap_r_idx = MON_KAGE (shadower). The shadower's
 *     true nature is hidden from the player until it dies, or is probed.
 *
 * Use mon_race() for gameplay stuff, such as resistances, movement, etc.
 *
 * Use mon_apparent_race() for display stuff, such as monster_desc, the monster
 * list, or monster recall.
 *
 * Use mon_true_race() to recover the original race. This is usually the same
 * as mon_race(), except for chameleons (cf [5] above). Generally, this is done
 * when the monster dies. (real_r_ptr atm).
 *
 * Guard monster lore with is_original_ap (r_idx == ap_r_idx).
 */
mon_race_ptr mon_race(mon_ptr mon)
{
    assert(mon);
    return mon_race_lookup(mon->r_idx);
}

mon_race_ptr mon_apparent_race(mon_ptr mon)
{
    assert(mon);
    return mon_race_lookup(mon->ap_r_idx);
}

mon_race_ptr mon_true_race(mon_ptr mon)
{
    mon_race_ptr race;
    assert(mon);
    race = mon_race_lookup(mon->r_idx);
    if (mon->mflag2 & MFLAG2_CHAMELEON)
    {
        if (race->flags1 & RF1_UNIQUE) /* Chameleon lord only uses unique forms */
            return mon_race_lookup(MON_CHAMELEON_K);
        else                           /* other chameleons never use unique forms */
            return mon_race_lookup(MON_CHAMELEON);
    }
    return race;
}

/* Monster Anger: Attacking a monster from a distance should make it more
 * likely to respond with a distance attack (spell or breath). */
void mon_anger(mon_ptr mon)
{
    mon->anger = MIN(100, mon->anger + 10 + mon->anger/2); 
}

void mon_anger_spell(mon_ptr mon, int dam)
{
    int inc = 10 + mon->anger/2;

    if (dam < 450)
        inc = MAX(1, inc*(dam + 50)/500);

    mon->anger = MIN(100, mon->anger + inc);
    #if 0
    msg_format("<color:D>mon_anger_spell:%d</color>", mon->anger);
    #endif
}

void mon_anger_shoot(mon_ptr mon, int dam)
{
    int inc = 5 + mon->anger/4;

    if (dam < 175)
        inc = MAX(1, inc*(dam + 25)/200);

    mon->anger = MIN(100, mon->anger + inc);
    #if 0
    msg_format("<color:D>mon_anger_shoot:%d</color>", mon->anger);
    #endif
}

/* Monster Stunning: The amount of stunning varies with the damage of
 * the attack. If desired, monsters may get a saving throw vs the damage
 * amount. Clients should check RF3_NO_STUN since sometimes, this flag
 * is ignored (eg, Warlock's Stunning Blast). Other times, RFR_RES_SOUN
 * protects from stuns. */
int mon_stun_amount(int dam)
{
    static point_t tbl[4] = { {1, 1}, {10, 10}, {100, 25}, {500, 50} };
    return interpolate(dam, tbl, 4);
}
bool mon_stun(mon_ptr mon, int amt)
{
    int cur_stun;
    if (amt <= 0) return FALSE;
    cur_stun = mon_tim_amount(mon, T_STUN);
    if (cur_stun)
    {
        int div = 1 + cur_stun / 20;
        amt = MAX(1, amt/div);
    }
    mon_tim_add(mon, T_STUN, amt);
    return cur_stun == 0;
}
int mon_save_r_level(int r_idx)
{
    monster_race *r_ptr = mon_race_lookup(r_idx);
    int           ml = r_ptr->level;

    if (r_ptr->flags1 & RF1_UNIQUE)
        ml += ml/5;

    if (ml < 1)
        ml = 1;

    return ml;
}

/* Some general saving throws vs the damage of the attack. Higher damage
 * attacks should be more likely to stun, confuse, slow or whatever. This
 * makes more sense than rolling against the player's level, and also allows
 * for devices or mon v. mon scenarios.
 * XXX gf_affect_m takes both high damage spells and low to med damage melee
 * attacks and auras. Pass the GF_AFFECT_* flags here so we can choose an
 * appropriate saving throw. (IN PROGRESS) XXX */
static int _ds(int l, int d) { return 10 + l + l*l/d; }
bool mon_save_stun(int rlev, int dam)
{
    return randint1(_ds(rlev, 12)) > dam;
}
bool mon_save_time(int r_idx, int dam, int flags)
{
    int  ml = mon_save_r_level(r_idx);
    if (flags & GF_AFFECT_SPELL)
    {
        int ds = _ds(ml, 12);
        #if 0
        int odds = 0;
        if (ds > dam) odds = (ds - dam)*1000/ds;
        msg_format("<color:D>mon_save_time(%d, %d) = %d.%d%%</color>", ml, dam, odds/10, odds%10);
        #endif
        return randint1(ds) > dam;
    }
    return randint1(dam) <= randint1(ml);
}
bool mon_save_poly(int rlev, int dam)
{
    return randint1(_ds(rlev, 6)) > dam;
}
bool mon_save_slow(int rlev, int dam)
{
    return randint1(_ds(rlev, 6)) > dam;
}
bool mon_save_disenchant(int r_idx, int dam, int flags)
{
    int  ml = mon_save_r_level(r_idx);
    if (flags & GF_AFFECT_SPELL)
        return randint1(_ds(ml, 6)) > dam;
    return randint1(dam) <= randint1(ml);
}
bool mon_save_smash(int rlev, int dam)
{
    int ds = _ds(rlev, 17);
    #if 0
    int odds = 0;
    if (ds > dam) odds = (ds - dam)*1000/ds;
    msg_format("<color:D>mon_save_smash(%d, %d) = %d.%d%%</color>", rlev, dam, odds/10, odds%10);
    #endif
    return randint1(ds) > dam;
}
bool mon_save_psi(int rlev, int dam)
{
    return randint1(_ds(rlev, 17)) > dam;
}

/* Monster saving throws versus player techniques
   We use competing dice for the save, so it is important
   that the scales match. ML runs up to 100 (or 120 for the Serpent)
   and PL typically runs up to 100 (for stat based saves) or
   just 50 (if A_NONE is used for a weak save). Call mon_save_aux
   directly will give you access to the save logic, but make sure
   the power makes sense. This happens a bit with project() using
   the damage parameter to pass in a player power.

   For damage effects, like GF_SOUND stunning monsters, use
   mon_save_stun and the like instead. Damage numbers can run
   very high (400 or more) which makes the competing dice approach
   not work well (Monsters have no reasonable chance to save).

   For example, the Serpent has 57.6% save from mon_save_stun(400),
   but only 12.6% from mon_save_aux(400).

   As always, build a crosstab in a spreadsheet for analysis.
*/

bool mon_save_aux(int r_idx, int power)
{
    int  ml = mon_save_r_level(r_idx);
    bool result = FALSE;

    if (power < 1)
        power = 1;

    #if 0
  { int odds;
    if (power<=ml)
        odds=(2*ml-power+1)*1000/(2*ml);
    else
        odds=(ml+1)*1000/(2*power);
    msg_format("<color:D>mon_save_aux(%d, %d) = %d.%d%%</color>", ml, power, odds/10, odds%10); }
    #endif
    if (randint1(power) <= randint1(ml))
        result = TRUE;

    return result;
}

bool mon_save_p(int r_idx, int stat)
{
    int pl = p_ptr->lev;

    if (stat >= 0 && stat < 6)
        pl += adj_stat_save[p_ptr->stat_ind[stat]];

    return mon_save_aux(r_idx, pl);
}

bool mon_save_m(int r_idx, int src_r_idx)
{
    return mon_save_aux(r_idx, mon_save_r_level(src_r_idx));
}

bool mon_can_attack(mon_ptr mon)
{
    mon_race_ptr race = mon_race(mon);
    if (race->flags1 & RF1_NEVER_BLOW) return FALSE;
    return vec_length(race->blows) > 0;
}

cptr mon_race_describe_singular(char c)
{
    switch (c)
    {
    case 'a': return "Ant";
    case 'A': return "Angel";
    case 'b': return "Bat";
    case 'B': return "Bird";
    case 'c': return "Centipede";
    case 'C': return "Canine";
    case 'd': return "Young Dragon";
    case 'D': return "Dragon";
    case 'e': return "Floating Eye";
    case 'E': return "Elemental";
    case 'f': return "Feline";
    case 'F': return "Dragon Fly";
    case 'g': return "Golem";
    case 'G': return "Ghost";
    case 'h': return "Hobbit/Dwarf/Elf";
    case 'H': return "Hybrid Monster";
    case 'i': return "Icky Thing";
    case 'I': return "Insect";
    case 'j': return "Jelly";
    case 'J': return "Serpent";
    case 'k': return "Kobold";
    case 'K': return "Beetle";
    case 'l': return "Aquatic Monster";
    case 'L': return "Lich";
    case 'm': return "Mold";
    case 'M': return "Multi-Headed Reptile";
    case 'n': return "Naga";
    case 'N': return "Mystery Monster";
    case 'o': return "Orc";
    case 'O': return "Ogre";
    case 'p': return "Human";
    case 'P': return "Giant";
    case 'q': return "Quadruped";
    case 'Q': return "Quylthulg";
    case 'r': return "Rodent";
    case 'R': return "Reptile";
    case 's': return "Skeleton";
    case 'S': return "Spider";
    case 't': return "Townsperson";
    case 'T': return "Troll";
    case 'u': return "Minor Demon";
    case 'U': return "Demon";
    case 'v': return "Vortex";
    case 'V': return "Vampire";
    case 'w': return "Worm";
    case 'W': return "Wraith";
    case 'X': return "Xorn";
    case 'y': return "Yeek";
    case 'Y': return "Yeti";
    case 'z': return "Zombie";
    case 'Z': return "Hound";
    case ',': return "Mushroom";
    }
    return "Monster";
}

cptr mon_race_describe_plural(char c)
{
    switch (c)
    {
    case 'a': return "Ants";
    case 'A': return "Angels";
    case 'b': return "Bats";
    case 'B': return "Birds";
    case 'c': return "Centipedes";
    case 'C': return "Canines";
    case 'd': return "Young Dragons";
    case 'D': return "Dragons";
    case 'e': return "Floating Eyes";
    case 'E': return "Elementals";
    case 'f': return "Felines";
    case 'F': return "Dragon Flies";
    case 'g': return "Golems";
    case 'G': return "Ghosts";
    case 'h': return "Hobbits/Dwarves/Elves";
    case 'H': return "Hybrid Monsters";
    case 'i': return "Icky Things";
    case 'I': return "Insects";
    case 'j': return "Jellies";
    case 'J': return "Serpents";
    case 'k': return "Kobolds";
    case 'K': return "Beetles";
    case 'l': return "Aquatic Monsters";
    case 'L': return "Liches";
    case 'm': return "Molds";
    case 'M': return "Multi-Headed Reptiles";
    case 'n': return "Nagas";
    case 'N': return "Mystery Monsters";
    case 'o': return "Orcs";
    case 'O': return "Ogres";
    case 'p': return "Humans";
    case 'P': return "Giants";
    case 'q': return "Quadrupeds";
    case 'Q': return "Quylthulgs";
    case 'r': return "Rodents";
    case 'R': return "Reptiles";
    case 's': return "Skeletons";
    case 'S': return "Spiders";
    case 't': return "Townspeople";
    case 'T': return "Trolls";
    case 'u': return "Minor Demons";
    case 'U': return "Demons";
    case 'v': return "Vortices";
    case 'V': return "Vampires";
    case 'w': return "Worms";
    case 'W': return "Wraiths";
    case 'X': return "Xorns";
    case 'y': return "Yeeks";
    case 'Y': return "Yeties";
    case 'z': return "Zombies";
    case 'Z': return "Hounds";
    case ',': return "Mushrooms";
    }
    return "Monsters";
}

/*************************************************************************
 * Monster Lore
 *************************************************************************/
bool mon_is_dead(mon_ptr mon) { return mon->hp < 0; }
bool unique_is_dead(int id) { return !mon_race_lookup(id)->max_num; }
bool mon_is_deleted(mon_ptr mon) { return mon->dun_id == 0; }

bool mon_is_smart(mon_ptr mon, int sm) { return BOOL(mon->smart & (1 << sm)); }
bool mon_is_pet(mon_ptr mon) { return mon_is_smart(mon, SM_PET); }
bool mon_is_friendly(mon_ptr mon) { return mon_is_smart(mon, SM_FRIENDLY); }
bool mon_is_cloned(mon_ptr mon) { return mon_is_smart(mon, SM_CLONED); }
bool mon_is_guardian(mon_ptr mon) { return mon_is_smart(mon, SM_GUARDIAN); }

bool mon_is_animal(mon_ptr mon) { return mon_race_is_animal(mon_race(mon)); }
bool mon_race_is_animal(mon_race_ptr race) { return BOOL(race->flags3 & RF3_ANIMAL); }
void mon_lore_animal(mon_ptr mon) { mon_lore_3(mon, RF3_ANIMAL); }

bool mon_is_demon(mon_ptr mon) { return mon_race_is_demon(mon_race(mon)); }
bool mon_race_is_demon(mon_race_ptr race) { return BOOL(race->flags3 & RF3_DEMON); }
void mon_lore_demon(mon_ptr mon) { mon_lore_3(mon, RF3_DEMON); }

bool mon_is_dragon(mon_ptr mon) { return mon_race_is_dragon(mon_race(mon)); }
bool mon_race_is_dragon(mon_race_ptr race) { return BOOL(race->flags3 & RF3_DRAGON); }
void mon_lore_dragon(mon_ptr mon) { mon_lore_3(mon, RF3_DRAGON); }

bool mon_is_evil(mon_ptr mon) { return mon_race_is_evil(mon_race(mon)); }
bool mon_race_is_evil(mon_race_ptr race) { return BOOL(race->flags3 & RF3_EVIL); }
void mon_lore_evil(mon_ptr mon) { mon_lore_3(mon, RF3_EVIL); }

bool mon_is_good(mon_ptr mon) { return mon_race_is_good(mon_race(mon)); }
bool mon_race_is_good(mon_race_ptr race) { return BOOL(race->flags3 & RF3_GOOD); }
void mon_lore_good(mon_ptr mon) { mon_lore_3(mon, RF3_GOOD); }

bool mon_is_giant(mon_ptr mon) { return mon_race_is_giant(mon_race(mon)); }
bool mon_race_is_giant(mon_race_ptr race) { return BOOL(race->flags3 & RF3_GIANT); }
void mon_lore_giant(mon_ptr mon) { mon_lore_3(mon, RF3_GIANT); }

bool mon_is_human(mon_ptr mon) { return mon_race_is_human(mon_race(mon)); }
bool mon_race_is_human(mon_race_ptr race) { return BOOL(race->flags2 & RF2_HUMAN); }
void mon_lore_human(mon_ptr mon) { mon_lore_2(mon, RF2_HUMAN); }

bool mon_is_living(mon_ptr mon) { return mon_race_is_living(mon_race(mon)); }
bool mon_race_is_living(mon_race_ptr race) { return !BOOL(race->flags3 & (RF3_DEMON | RF3_UNDEAD | RF3_NONLIVING)); }
void mon_lore_living(mon_ptr mon) { mon_lore_3(mon, RF3_DEMON | RF3_UNDEAD | RF3_NONLIVING); }

bool mon_is_orc(mon_ptr mon) { return mon_race_is_orc(mon_race(mon)); }
bool mon_race_is_orc(mon_race_ptr race) { return BOOL(race->flags3 & RF3_ORC); }
void mon_lore_orc(mon_ptr mon) { mon_lore_3(mon, RF3_ORC); }

bool mon_is_troll(mon_ptr mon) { return mon_race_is_troll(mon_race(mon)); }
bool mon_race_is_troll(mon_race_ptr race) { return BOOL(race->flags3 & RF3_TROLL); }
void mon_lore_troll(mon_ptr mon) { mon_lore_3(mon, RF3_TROLL); }

bool mon_is_undead(mon_ptr mon) { return mon_race_is_undead(mon_race(mon)); }
bool mon_race_is_undead(mon_race_ptr race) { return BOOL(race->flags3 & RF3_UNDEAD); }
void mon_lore_undead(mon_ptr mon) { mon_lore_3(mon, RF3_UNDEAD); }

bool mon_is_unique(mon_ptr mon) { return mon_race_is_unique(mon_race(mon)); }
bool mon_race_is_unique(mon_race_ptr race) { return BOOL((race->flags1 & RF1_UNIQUE) || (race->flags7 & RF7_UNIQUE2)); }

/* acid */
bool mon_not_res_acid(mon_ptr mon) { return !mon_res_acid(mon); }
bool mon_res_acid(mon_ptr mon) { return mon_race_res_acid(mon_race(mon)); }
bool mon_race_res_acid(mon_race_ptr race) { return BOOL(race->flagsr & RFR_EFF_RES_ACID_MASK); }
void mon_lore_res_acid(mon_ptr mon) { mon_lore_r(mon, RFR_EFF_RES_ACID_MASK); }

bool mon_im_acid(mon_ptr mon) { return mon_race_im_acid(mon_race(mon)); }
bool mon_race_im_acid(mon_race_ptr race) { return BOOL(race->flagsr & RFR_EFF_IM_ACID_MASK); }
void mon_lore_im_acid(mon_ptr mon) { mon_lore_r(mon, RFR_EFF_IM_ACID_MASK); }

/* elec */
bool mon_not_res_elec(mon_ptr mon) { return !mon_res_elec(mon); }
bool mon_res_elec(mon_ptr mon) { return mon_race_res_elec(mon_race(mon)); }
bool mon_race_res_elec(mon_race_ptr race) { return BOOL(race->flagsr & RFR_EFF_RES_ELEC_MASK); }
void mon_lore_res_elec(mon_ptr mon) { mon_lore_r(mon, RFR_EFF_RES_ELEC_MASK); }

bool mon_im_elec(mon_ptr mon) { return mon_race_im_elec(mon_race(mon)); }
bool mon_race_im_elec(mon_race_ptr race) { return BOOL(race->flagsr & RFR_EFF_IM_ELEC_MASK); }
void mon_lore_im_elec(mon_ptr mon) { mon_lore_r(mon, RFR_EFF_IM_ELEC_MASK); }

/* fire */
bool mon_vuln_fire(mon_ptr mon) { return mon_race_vuln_fire(mon_race(mon)); }
bool mon_race_vuln_fire(mon_race_ptr race) { return BOOL(race->flags3 & RF3_HURT_FIRE); }
void mon_lore_vuln_fire(mon_ptr mon) { mon_lore_3(mon, RF3_HURT_FIRE); }

bool mon_not_res_fire(mon_ptr mon) { return !mon_res_fire(mon); }
bool mon_res_fire(mon_ptr mon) { return mon_race_res_fire(mon_race(mon)); }
bool mon_race_res_fire(mon_race_ptr race) { return BOOL(race->flagsr & RFR_EFF_RES_FIRE_MASK); }
void mon_lore_res_fire(mon_ptr mon) { mon_lore_r(mon, RFR_EFF_RES_FIRE_MASK); }

bool mon_im_fire(mon_ptr mon) { return mon_race_im_fire(mon_race(mon)); }
bool mon_race_im_fire(mon_race_ptr race) { return BOOL(race->flagsr & RFR_EFF_IM_FIRE_MASK); }
void mon_lore_im_fire(mon_ptr mon) { mon_lore_r(mon, RFR_EFF_IM_FIRE_MASK); }

/* cold */
bool mon_vuln_cold(mon_ptr mon) { return mon_race_vuln_cold(mon_race(mon)); }
bool mon_race_vuln_cold(mon_race_ptr race) { return BOOL(race->flags3 & RF3_HURT_COLD); }
void mon_lore_vuln_cold(mon_ptr mon) { mon_lore_3(mon, RF3_HURT_COLD); }

bool mon_not_res_cold(mon_ptr mon) { return !mon_res_cold(mon); }
bool mon_res_cold(mon_ptr mon) { return mon_race_res_cold(mon_race(mon)); }
bool mon_race_res_cold(mon_race_ptr race) { return BOOL(race->flagsr & RFR_EFF_RES_COLD_MASK); }
void mon_lore_res_cold(mon_ptr mon) { mon_lore_r(mon, RFR_EFF_RES_COLD_MASK); }

bool mon_im_cold(mon_ptr mon) { return mon_race_im_cold(mon_race(mon)); }
bool mon_race_im_cold(mon_race_ptr race) { return BOOL(race->flagsr & RFR_EFF_IM_COLD_MASK); }
void mon_lore_im_cold(mon_ptr mon) { mon_lore_r(mon, RFR_EFF_IM_COLD_MASK); }

/* poison */
bool mon_not_res_pois(mon_ptr mon) { return !mon_res_pois(mon); }
bool mon_res_pois(mon_ptr mon) { return mon_race_res_pois(mon_race(mon)); }
bool mon_race_res_pois(mon_race_ptr race) { return BOOL(race->flagsr & RFR_EFF_RES_POIS_MASK); }
void mon_lore_res_pois(mon_ptr mon) { mon_lore_r(mon, RFR_EFF_RES_POIS_MASK); }

bool mon_im_pois(mon_ptr mon) { return mon_race_im_pois(mon_race(mon)); }
bool mon_race_im_pois(mon_race_ptr race) { return BOOL(race->flagsr & RFR_EFF_IM_POIS_MASK); }
void mon_lore_im_pois(mon_ptr mon) { mon_lore_r(mon, RFR_EFF_IM_POIS_MASK); }

/* lite */
bool mon_vuln_lite(mon_ptr mon) { return mon_race_vuln_lite(mon_race(mon)); }
bool mon_race_vuln_lite(mon_race_ptr race) { return BOOL(race->flags3 & RF3_HURT_LITE); }
void mon_lore_vuln_lite(mon_ptr mon) { mon_lore_3(mon, RF3_HURT_LITE); }

bool mon_not_res_lite(mon_ptr mon) { return !mon_res_lite(mon); }
bool mon_res_lite(mon_ptr mon) { return mon_race_res_lite(mon_race(mon)); }
bool mon_race_res_lite(mon_race_ptr race) { return BOOL(race->flagsr & RFR_EFF_RES_LITE_MASK); }
void mon_lore_res_lite(mon_ptr mon) { mon_lore_r(mon, RFR_EFF_RES_LITE_MASK); }

/* dark */
bool mon_not_res_dark(mon_ptr mon) { return !mon_res_dark(mon); }
bool mon_res_dark(mon_ptr mon) { return mon_race_res_dark(mon_race(mon)); }
bool mon_race_res_dark(mon_race_ptr race) { return BOOL(race->flagsr & RFR_EFF_RES_DARK_MASK); }
void mon_lore_res_dark(mon_ptr mon) { mon_lore_r(mon, RFR_EFF_RES_DARK_MASK); }

/* plasma */
bool mon_not_res_plasma(mon_ptr mon) { return !mon_res_plasma(mon); }
bool mon_res_plasma(mon_ptr mon) { return mon_race_res_plasma(mon_race(mon)); }
bool mon_race_res_plasma(mon_race_ptr race) { return BOOL(race->flagsr & RFR_EFF_RES_PLAS_MASK); }
void mon_lore_res_plasma(mon_ptr mon) { mon_lore_r(mon, RFR_EFF_RES_PLAS_MASK); }

/*************************************************************************
 * Monster Drops
 *************************************************************************/
static obj_ptr _make(mon_ptr mon, mon_race_ptr race, mon_drop_ptr drop)
{
    int mode = 0, lvl = cave->difficulty;
    obj_ptr obj = NULL;

    /* Uniques get better object creation */
    if (race->flags1 & RF1_UNIQUE)
        mode |= (AM_UNIQUE | AM_GOOD);

    /* Caculate the Object Level, being generous to the player */
    if (race->level >= lvl)
        lvl = race->level;
    else
    {
        if (mode & AM_GREAT)
            lvl = (race->level + 3*lvl) / 4;
        else if (mode & AM_GOOD)
            lvl = (race->level + 2*lvl) / 3;
        else
            lvl = (race->level + lvl) / 2;
    }

    /* Try for Thematic and Tailored Drops */
    obj_drop_theme = 0;
    if (drop->theme && one_in_(2))
    {
        assert(drop->drop.flags & OBJ_DROP_RANDOM); /* double check parser */
        obj_drop_theme = drop->theme;
    }
    else /* Don't try to tailor themed drops since they could easily fail ... */
    {
        if ( (mon->mflag2 & MFLAG2_QUESTOR)
          || (race->flags7 & RF7_GUARDIAN)
          || (race->flagsx & RFX_GUARDIAN) )
        {
            if (one_in_(5))
                mode |= AM_TAILORED;
        }
        if (race->flags1 & RF1_UNIQUE)
        {
            if (one_in_(10))
                mode |= AM_TAILORED;
        }
        else if (mode & (AM_GOOD | AM_GREAT))
        {
            if (one_in_(30))
                mode |= AM_TAILORED;
        }
    }

    obj = obj_drop_make(&drop->drop, lvl, mode);
    obj_drop_theme = 0;
    return obj;
}

static int _roll_drop_ct(mon_drop_ptr drop)
{
    int ct = 0;
    if (!drop->pct || randint0(100) < drop->pct)
    {
        ct = damroll(drop->dd, drop->ds) + drop->base;
        if (!ct) ct = 1; /* e.g. "O:5%:ART(amber)" should not force user to enter dice */
    }
    return ct;
}

mon_drop_ptr mon_drop_alloc(void)
{
    mon_drop_ptr drop = malloc(sizeof(mon_drop_t));
    memset(drop, 0, sizeof(mon_drop_t));
    return drop;
}

void mon_drop_free(mon_drop_ptr drop)
{
    mon_drop_ptr next;
    while (drop)
    {
        next = drop->next;
        free(drop);
        drop = next;
    }
}

vec_ptr mon_drop_make(mon_ptr mon)
{
    vec_ptr drops = vec_alloc((vec_free_f)obj_free);
    mon_race_ptr race = mon_race(mon);
    mon_drop_ptr drop;

    if (!race->drops) return drops;
    if (is_pet(mon)) return drops;

    for (drop = race->drops; drop; drop = drop->next)
    {
        int ct = 0, j;

        if (drop == race->drops) /* we pre-roll the default rule so rogues can pick pockets */
            ct = mon->drop_ct - mon->stolen_ct;
        else
            ct = _roll_drop_ct(drop);

        /* Check for boss artifact drops */
        if (!ct && (drop->drop.flags & OBJ_DROP_STD_ART))
        {
            race_t *race = get_race();
            if (race->boss_r_idx == mon->r_idx)
                ct = 1;
        }
        for (j = 0; j < ct; j++)
        {
            obj_ptr obj = _make(mon, race, drop);
            if (obj)
                vec_add(drops, obj);
        }
    }
    return drops;
}

void mon_drop_init(mon_ptr mon)
{
    mon_race_ptr race = mon_race(mon);

    mon->drop_ct = 0;
    mon->stolen_ct = 0;
    if (race->flags1 & RF1_UNIQUE)
        mon->stolen_ct = race->stolen_ct;

    /* pre-roll the first rule so rogues can pick pockets */
    if (is_pet(mon)) return;
    if (!race->drops) return;

    mon->drop_ct = _roll_drop_ct(race->drops);
}

obj_ptr mon_pick_pocket(mon_ptr mon)
{
    obj_ptr loot = NULL;
    mon_race_ptr race;

    /* XXX quick check to steal a carried object */

    /* We always steal from the first rule only ... this is for simplicity
     * of implementation for a feature I thought I would enjoy more than I do!
     * In practice, most monsters only have a single rule, and those that don't
     * will use non-initial rules for dropping fixed artifacts, egos or gold. */
    race = mon_race(mon);
    if (strchr("!?=$|/\\([", race->d_char)) return NULL;  /* don't steal from mimics */
    if (!race->drops) return NULL;
    if (mon->stolen_ct >= mon->drop_ct) return NULL;

    loot = _make(mon, race, race->drops);
    if (loot)
    {
        mon->stolen_ct++;
        if (race->flags1 & RF1_UNIQUE)
            race->stolen_ct++;
    }
    return loot;
}

static cptr r_drop_themes[R_DROP_MAX] =
{
    "NONE",

    "DROP_WARRIOR",
    "DROP_WARRIOR_SHOOT",
    "DROP_ARCHER",
    "DROP_MAGE",
    "DROP_PRIEST",
    "DROP_PRIEST_EVIL",
    "DROP_PALADIN",
    "DROP_PALADIN_EVIL",
    "DROP_SAMURAI",
    "DROP_NINJA",
    "DROP_ROGUE",

    "DROP_HOBBIT",
    "DROP_DWARF",

    "DROP_JUNK",
};

errr mon_drop_parse(char *buf, mon_race_ptr race, int options)
{
    errr  rc = 0;
    char *tokens[10];
    int   token_ct = z_string_split(buf, tokens, 10, ":");
    int   i;
    mon_drop_ptr drop = mon_drop_alloc();

    drop->drop.flags |= OBJ_DROP_MON;

    for (i = 0; i < token_ct; i++)
    {
        char *token = tokens[i];
        char arg[100], sentinel = '~', check;
        int  dd, ds, base, pct;

        if (!strlen(token)) continue;
        sprintf(arg, "%s%c", token, sentinel);

        if (2 == sscanf(arg, "%d%%%c", &pct, &check) && check == sentinel)
        {
            drop->pct = MAX(0, MIN(100, pct));
        }
        else if (4 == sscanf(arg, "%dd%d+%d%c", &dd, &ds, &base, &check) && check == sentinel)
        {
            drop->dd = MAX(0, dd);
            drop->ds = MAX(0, ds);
            drop->base = base;
        }
        else if (3 == sscanf(arg, "%dd%d%c", &dd, &ds, &check) && check == sentinel)
        {
            drop->dd = MAX(0, dd);
            drop->ds = MAX(0, ds);
            drop->base = 0;
        }
        else if (2 == sscanf(arg, "%d%c", &base, &check) && check == sentinel)
        {
            drop->dd = 0;
            drop->ds = 0;
            drop->base = base;
        }
        /* O:25%:1d4:DROP_WARRIOR 
         * O:1d2:OBJ(*, GOOD):DROP_WARRIOR ... OBJ directives here should always use '*'
         * (I'm trying to remove DROP_GOOD from racial flags ...) */
        else if (strstr(token, "DROP_") == token)
        {
            int j;
            for (j = 0; j < R_DROP_MAX; j++)
            {
                if (streq(token, r_drop_themes[j]))
                {
                    drop->theme = j;
                    drop->drop.flags |= OBJ_DROP_RANDOM;
                    break;
                }
            }
            if (!drop->theme)
                rc = PARSE_ERROR_INVALID_FLAG; /* Not a valid theme */
            else if (i < token_ct - 1)
                rc = PARSE_ERROR_TOO_FEW_ARGUMENTS; /* too many, actually. DROP_FOO should be last and only drop directive */
            break;
        }
        /* O:5%:ART(amber)
         * O:2d3+1:OBJ(*)
         * O:5%:OBJ(blade of chaos) */
        else
        {
            rc = obj_drop_parse_cmd(token, &drop->drop, options);
            if (rc) break;
        }
    }
    if (rc)
        free(drop);
    /* add new drop rule to tail. rogue pick-pockets relies on ordering of rules */
    else if (!race->drops)
        race->drops = drop;
    else
    {
        mon_drop_ptr tail = race->drops;
        while (tail->next) tail = tail->next;
        tail->next = drop;
    }
    return rc;
}

/*************************************************************************
 * Monster Auras
 *************************************************************************/
mon_effect_ptr mon_auras_find(mon_race_ptr race, int effect)
{
    int i;
    for (i = 0; i < MAX_MON_AURAS; i++)
    {
        mon_effect_ptr aura = &race->auras[i];
        if (aura->effect == effect) return aura;
    }
    return NULL;
}

/*************************************************************************
 * Monster Blows
 *************************************************************************/
mon_blow_ptr mon_blows_find(vec_ptr blows, int method)
{
    int i;
    for (i = 0; i < vec_length(blows); i++)
    {
        mon_blow_ptr blow = vec_get(blows, i);
        if (blow->method == method) return blow;
    }
    return NULL;
}

mon_blow_ptr mon_blow_alloc(int method)
{
    mon_blow_ptr blow = (mon_blow_ptr)malloc(sizeof(mon_blow_t));
    memset(blow, 0, sizeof(mon_blow_t));
    blow->method = method;
    blow->flags = mon_blow_info_lookup(method)->flags;
    blow->blows = 100;
    return blow;
}
void mon_blow_free(mon_blow_ptr blow)
{
    if (!blow) return;
    if (blow->effects)
    {
        free(blow->effects);
        blow->effects = NULL;
        blow->effect_ct = 0;
        blow->allocated = 0;
    }
    free(blow);
}

mon_blow_ptr mon_blow_copy(mon_blow_ptr blow)
{
    mon_blow_ptr copy = (mon_blow_ptr)malloc(sizeof(mon_blow_t));
    *copy = *blow; /* shallow ... now copy->effects and allocated are wrong */
    if (blow->effect_ct)
    {
        int cb = sizeof(mon_effect_t)*blow->effect_ct;
        copy->effects = malloc(cb);
        copy->allocated = blow->effect_ct;
        memcpy(copy->effects, blow->effects, cb);
    }
    else
    {
        copy->effects = NULL;
        copy->allocated = 0;
    }
    return copy;
}

static void _blow_effects_grow(mon_blow_ptr blow)
{
    if (!blow->allocated)
    {
        blow->allocated = 1;
        blow->effects = malloc(sizeof(mon_effect_t)*blow->allocated);
    }
    else
    {
        mon_effect_ptr old = blow->effects;
        blow->allocated *= 2;
        blow->effects = malloc(sizeof(mon_effect_t)*blow->allocated);
        memcpy(blow->effects, old, sizeof(mon_effect_t)*blow->effect_ct);
        free(old);
    }
}
mon_effect_ptr mon_blow_push_effect(mon_blow_ptr blow, int effect, dice_t dice)
{
    mon_effect_t e = {0};
    e.effect = effect;
    e.dice = dice;
    return mon_blow_push_effect_aux(blow, &e);
}
mon_effect_ptr mon_blow_push_effect_aux(mon_blow_ptr blow, mon_effect_ptr effect)
{
    mon_effect_ptr e;
    assert(blow);
    if (blow->effect_ct == blow->allocated)
        _blow_effects_grow(blow);
    assert(blow->effect_ct < blow->allocated);
    e = &blow->effects[blow->effect_ct];
    *e = *effect;
    blow->effect_ct++;
    return e;
}
void mon_blow_pop_effect(mon_blow_ptr blow)
{
    assert(blow->effect_ct); /* underflow */
    blow->effect_ct--;
}
bool mon_blow_allow_crit(mon_blow_ptr blow)
{
    int effect = mon_blow_base_effect(blow);
    if (!(blow->flags & MBF_ALLOW_CRIT)) return FALSE;
    if (effect != RBE_HURT && effect != RBE_SHATTER && effect != RBE_VAMP) /* XXX on RBE_VAMP */
        return FALSE;
    return TRUE;
}
dice_t mon_blow_base_dice(mon_blow_ptr blow)
{
    dice_t dice = {0};
    if (blow->effect_ct)
        dice = blow->effects[0].dice;
    return dice;
}
int mon_blow_base_effect(mon_blow_ptr blow)
{
    int effect = GF_NONE;
    if (blow->effect_ct)
        effect = blow->effects[0].effect;
    return effect;
}
static mon_blow_info_t _mon_blow_info[RBM_COUNT] = {
    {RBM_NONE, "None", "%^s ignores.", "You ignore.", "NONE", 0},
    {RBM_HIT, "Hit", "%^s hits.", "You hit.", "HIT", MBF_TOUCH | MBF_ALLOW_CRIT | MBF_MASK_HAND},
    {RBM_TOUCH, "Touch", "%^s touches.", "You touch.", "TOUCH", MBF_TOUCH | MBF_MASK_HAND},
    {RBM_PUNCH, "Punch", "%^s punches.", "You punch.", "PUNCH", MBF_MONK_PUNCH},
    {RBM_KICK, "Kick", "%^s kicks.", "You kick.", "KICK", MBF_TOUCH | MBF_ALLOW_CRIT | MBF_MONK},
    {RBM_CLAW, "Claw", "%^s claws.", "You claw.", "CLAW", MBF_TOUCH | MBF_ALLOW_CRIT | MBF_MASK_HAND},
    {RBM_BITE, "Bite", "%^s bites.", "You bite.", "BITE", MBF_TOUCH | MBF_ALLOW_CRIT},
    {RBM_STING, "Sting", "%^s stings.", "You sting.", "STING", MBF_TOUCH},
    {RBM_SLASH, "Slash", "%^s slashes.", "You slash.", "SLASH", MBF_TOUCH | MBF_ALLOW_CRIT | MBF_MASK_HAND},
    {RBM_BUTT, "Butt", "%^s butts.", "You butt.", "BUTT", MBF_TOUCH | MBF_ALLOW_CRIT},
    {RBM_CRUSH, "Crush", "%^s crushes.", "You crush.", "CRUSH", MBF_TOUCH | MBF_ALLOW_CRIT},
    {RBM_ENGULF, "Engulf", "%^s engulfs.", "You engulf.", "ENGULF", MBF_TOUCH},
    {RBM_CHARGE, "Charge", "%^s charges.", "You charge.", "CHARGE", MBF_TOUCH},
    {RBM_CRAWL, "Crawl", "%^s crawls.", "You crawl.", "CRAWL", MBF_TOUCH},
    {RBM_DROOL, "Drool", "%^s drools.", "You drool.", "DROOL", 0},
    {RBM_SPIT, "Spit", "%^s spits.", "You spit.", "SPIT", 0},
    {RBM_EXPLODE, "Explode", "%^s explodes.", "You explode.", "EXPLODE", MBF_TOUCH},
    {RBM_GAZE, "Gaze", "%^s gazes.", "You gaze.", "GAZE", MBF_MASK_BLIND},
    {RBM_WAIL, "Wail", "%^s wails.", "You wail.", "WAIL", 0},
    {RBM_SPORE, "Spore", "%^s releases spores.", "You release spores.", "SPORE", 0},
    {RBM_PECK, "Peck", "%^s pecks.", "You peck.", "PECK", MBF_TOUCH | MBF_ALLOW_CRIT},
    {RBM_BEG, "Beg", "%^s begs.", "You beg.", "BEG", MBF_MASK_HAND}, /* on your knees with hands folded in supplication! */
    {RBM_INSULT, "Insult", "%^s insults.", "You insult.", "INSULT", 0},
    {RBM_MOAN, "Moan", "%^s moans.", "You moan.", "MOAN", 0},
    {RBM_SHOW, "Sing", "%^s sings.", "You sing.", "SHOW", 0},
    {RBM_MONK, "Monk", "%^s monkifies %s.", "You monkify %s.", "MONK", 0}, /* redirect */
    {RBM_STRIKE, "Strike", "%^s strikes.", "You strike.", "STRIKE", MBF_MONK_PUNCH},
    {RBM_KNEE, "Knee", "%^s knees.", "You knee.", "KNEE", MBF_TOUCH | MBF_ALLOW_CRIT | MBF_MONK},
    {RBM_ELBOW, "Elbow", "%^s elbows.", "You elbow.", "ELBOW", MBF_TOUCH | MBF_ALLOW_CRIT | MBF_MONK},
    {RBM_UPPERCUT, "Uppercut", "%^s uppercuts.", "You uppercut.", "UPPERCUT", MBF_MONK_PUNCH},
    {RBM_DOUBLE_KICK, "Double Kick", "%^s double-kicks.", "You double-kick.", "DOUBLE_KICK", MBF_MONK_KICK},
    {RBM_CATS_CLAW, "Cat's Claw", "%^s lands a Cat's Claw.", "You land a Cat's Claw.", "CATS_CLAW", MBF_MONK_PUNCH},
    {RBM_JUMP_KICK, "Jump Kick", "%^s jump kicks.", "You jump kick.", "JUMP_KICK", MBF_MONK_KICK},
    {RBM_EAGLES_CLAW, "Eagle's Claw", "%^s lands an Eagle's Claw.", "You land an Eagle's Claw.", "EAGLES_CLAW", MBF_MONK_PUNCH},
    {RBM_CIRCLE_KICK, "Circle Kick", "%^s circle kicks.", "You circle kick.", "CIRCLE_KICK", MBF_MONK_KICK},
    {RBM_IRON_FIST, "Iron Fist", "%^s lands an Iron Fist.", "You land an Iron Fist.", "IRON_FIST", MBF_MONK_PUNCH},
    {RBM_FLYING_KICK, "Flying Kick", "%^s lands a flying kick.", "You land a flying kick.", "FLYING_KICK", MBF_MONK_KICK},
    {RBM_DRAGON_FIST, "Dragon Fist", "%^s lands a <color:r>Dragon Fist</color>.",
        "You land a <color:r>Dragon Fist</color>.", "DRAGON_FIST", MBF_MONK_PUNCH},
    {RBM_CRUSHING_BLOW, "Crushing Blow", "%^s lands a <color:v>Crushing Blow</color>.",
        "You land a <color:v>Crushing Blow</color>.", "CRUSHING_BLOW", MBF_MONK_PUNCH},
    {RBM_ZOMBIE_CLAW, "Zombie Claw", "%^s lands a Zombie Claw.", "You land a Zombie Claw.", "ZOMBIE_CLAW", MBF_MONK_PUNCH},
    {RBM_GHOUL_TOUCH, "Ghoul Touch", "%^s lands a ghastly touch.", "You land a ghastly touch.", "GHOUL_TOUCH", MBF_MONK_PUNCH},
    {RBM_LICH_FIST, "Lich Fist", "%^s lands an undeadly strike.", "You land an undeadly strike.", "LICH_FIST", MBF_MONK_PUNCH},
    {RBM_REAVER_FIST, "Reaver Fist", "%^s lands the <color:r>Fist of the Reaver</color>.",
        "You land the <color:r>Fist of the Reaver</color>.", "REAVER_FIST", MBF_MONK_PUNCH},
    {RBM_HAND_OF_VECNA, "Hand of Vecna", "%^s lands the <color:v>Hand of Vecna</color>.",
        "You land the <color:v>Hand of Vecna</color>.", "HAND_OF_VECNA", MBF_MONK_PUNCH},
    {RBM_IMP_CLAW, "Imp Claw", "%^s lands an Imp's Claw.", "You land an Imp's Claw.", "IMP_CLAW", MBF_MONK_PUNCH},
    {RBM_DEVIL_CLAW, "Devil Claw", "%^s lands a devilish strike.", "You land a devilish strike.", "DEVIL_CLAW", MBF_MONK_PUNCH}, 
    {RBM_HELL_HAMMER, "Hell Hammer", "%^s lands Hell's Hammer!", "You land Hell's Hammer!", "HELL_HAMMER", MBF_MONK_PUNCH},
    {RBM_SATANS_CLAW, "Satan's Claw", "%^s lands Satan's Claw!", "You land Satan's Claw!", "SATANS_CLAW", MBF_MONK_PUNCH},
    {RBM_CHAOS_FIST, "Chaos Fist", "%^s lands the <color:v>Fist of Change</color>!",
        "You land the <color:v>Fist of Change</color>!", "CHAOS_FIST", MBF_MONK_PUNCH},
    {RBM_HELL_CLAW, "Hell Claw", "%^s lands the <color:r>Claws of Hell</color>!",
        "You land the <color:r>Claws of Hell</color>!", "HELL_CLAW", MBF_MONK_PUNCH},
    {RBM_VAMP_FIST, "Vampire Fist", "%^s lands a <color:D>Vampire Fist</color>.",
        "You land a <color:D>Vampire Fist</color>.", "VAMP_FIST", MBF_MONK_PUNCH},
};

mon_blow_info_ptr mon_blow_info_lookup(int method)
{
    mon_blow_info_ptr info;
    assert(0 <= method && method < RBM_COUNT);
    info = &_mon_blow_info[method];
    assert(info->id == method);
    return info;
}

mon_blow_info_ptr mon_blow_info_parse(cptr name)
{
    int i;
    for (i = 0; i < RBM_COUNT; i++)
    {
        mon_blow_info_ptr info = &_mon_blow_info[i];
        if (strcmp(name, info->parse) == 0) return info;
    }
    return NULL;
}

/*************************************************************************
 * Savefiles
 *************************************************************************/
enum {
    SAVE_MON_DONE = 0,
    SAVE_MON_AP_R_IDX,
    SAVE_MON_SUB_ALIGN,
    SAVE_MON_TIMER,
    SAVE_MON_TARGET,
    SAVE_MON_SMART,
    SAVE_MON_EXP,
    SAVE_MON_MFLAG2,
    SAVE_MON_NICKNAME,
    SAVE_MON_PARENT,
    SAVE_MON_PACK_IDX,
    SAVE_MON_AC,
    SAVE_MON_POWER,
    SAVE_MON_EGO_WHIP,
    SAVE_MON_ANTI_MAGIC,
    SAVE_MON_FORGOT_4,
    SAVE_MON_FORGOT_5,
    SAVE_MON_FORGOT_6,
    SAVE_MON_SUMMON_CT,
    SAVE_MON_DROP_CT,
    SAVE_MON_STOLEN_CT,
    SAVE_MON_PEXP,
    SAVE_MON_ANGER,
    SAVE_MON_MANA,
    SAVE_MON_TIMERS,
    SAVE_MON_TURNS,
};

void mon_load(mon_ptr mon, savefile_ptr file)
{
    char buf[128];

    mon->id = savefile_read_u16b(file);
    mon->mpower = 1000;
    mon->r_idx = savefile_read_s16b(file);
    mon->ap_r_idx = mon->r_idx;
    mon->dun_id = savefile_read_u16b(file);
    mon->pos.x = savefile_read_s16b(file);
    mon->pos.y = savefile_read_s16b(file);
    mon->hp = savefile_read_s16b(file);
    mon->maxhp = savefile_read_s16b(file);
    mon->max_maxhp = savefile_read_s16b(file);
    mon->mspeed = savefile_read_byte(file);
    mon->cdis = savefile_read_s16b(file);
    mon->energy_need = savefile_read_s16b(file);

    for (;;)
    {
        byte code = savefile_read_byte(file);
        if (code == SAVE_MON_DONE)
            break;

        switch (code)
        {
        case SAVE_MON_AP_R_IDX:
            mon->ap_r_idx = savefile_read_s16b(file);
            break;
        case SAVE_MON_SUB_ALIGN:
            mon->sub_align = savefile_read_byte(file);
            break;
        case SAVE_MON_TIMER:
            savefile_read_byte(file);
            savefile_read_s16b(file);
            break;
        case SAVE_MON_TIMERS:
            mon_tim_load(mon, file);
            break;
        case SAVE_MON_TARGET:
            mon->target.x = savefile_read_s16b(file);
            mon->target.y = savefile_read_s16b(file);
            break;
        case SAVE_MON_SMART:
            mon->smart = savefile_read_u32b(file);
            break;
        case SAVE_MON_EXP:
            mon->exp = savefile_read_u32b(file);
            break;
        case SAVE_MON_MFLAG2:
            mon->mflag2 = savefile_read_u32b(file);
            break;
        case SAVE_MON_NICKNAME:
            savefile_read_cptr(file, buf, sizeof(buf));
            mon->nickname = quark_add(buf);
            break;
        case SAVE_MON_PARENT:
            mon->parent_m_idx = savefile_read_s16b(file);
            break;
        case SAVE_MON_PACK_IDX:
            mon->pack_idx = savefile_read_s16b(file);
            break;
        case SAVE_MON_AC:
            mon->ac_adj = savefile_read_s16b(file);
            break;
        case SAVE_MON_POWER:
            mon->mpower = savefile_read_s16b(file);
            break;
        case SAVE_MON_DROP_CT:
            mon->drop_ct = savefile_read_byte(file);
            break;
        case SAVE_MON_STOLEN_CT:
            mon->stolen_ct = savefile_read_byte(file);
            break;
        case SAVE_MON_SUMMON_CT:
            savefile_read_u16b(file);
            break;
        case SAVE_MON_EGO_WHIP:
            savefile_read_byte(file);
            savefile_read_byte(file);
            break;
        case SAVE_MON_ANTI_MAGIC:
            mon->anti_magic_ct = savefile_read_byte(file);
            break;
        case SAVE_MON_PEXP:
            mon->pexp = savefile_read_s32b(file);
            break;
        case SAVE_MON_ANGER:
            mon->anger = savefile_read_byte(file);
            break;
        case SAVE_MON_MANA:
            mon->mana = savefile_read_s16b(file);
            break;
        case SAVE_MON_TURNS:
            mon->turns = savefile_read_u16b(file);
            break;
        /* default:
            TODO: Report an error back to the load routine!!*/
        }
    }
}
void mon_save(mon_ptr mon, savefile_ptr file)
{
    savefile_write_u16b(file, mon->id);
    savefile_write_s16b(file, mon->r_idx);
    savefile_write_u16b(file, mon->dun_id);
    savefile_write_s16b(file, mon->pos.x);
    savefile_write_s16b(file, mon->pos.y);
    savefile_write_s16b(file, mon->hp);
    savefile_write_s16b(file, mon->maxhp);
    savefile_write_s16b(file, mon->max_maxhp);
    savefile_write_byte(file, mon->mspeed);
    savefile_write_s16b(file, mon->cdis);
    savefile_write_s16b(file, mon->energy_need);

    if (!is_original_ap(mon))
    {
        savefile_write_byte(file, SAVE_MON_AP_R_IDX);
        savefile_write_s16b(file, mon->ap_r_idx);
    }
    if (mon->sub_align)
    {
        savefile_write_byte(file, SAVE_MON_SUB_ALIGN);
        savefile_write_byte(file, mon->sub_align);
    }
    if (mon_tim_count(mon))
    {
        savefile_write_byte(file, SAVE_MON_TIMERS);
        mon_tim_save(mon, file);
    }
    if (mon->target.x || mon->target.y)
    {
        savefile_write_byte(file, SAVE_MON_TARGET);
        savefile_write_s16b(file, mon->target.x);
        savefile_write_s16b(file, mon->target.y);
    }
    if (mon->smart)
    {
        savefile_write_byte(file, SAVE_MON_SMART);
        savefile_write_u32b(file, mon->smart);
    }
    if (mon->exp)
    {
        savefile_write_byte(file, SAVE_MON_EXP);
        savefile_write_u32b(file, mon->exp);
    }
    if (mon->mflag2)
    {
        savefile_write_byte(file, SAVE_MON_MFLAG2);
        savefile_write_u32b(file, mon->mflag2);
    }
    if (mon->nickname)
    {
        savefile_write_byte(file, SAVE_MON_NICKNAME);
        savefile_write_cptr(file, quark_str(mon->nickname));
    }
    if (mon->parent_m_idx)
    {
        savefile_write_byte(file, SAVE_MON_PARENT);
        savefile_write_s16b(file, mon->parent_m_idx);
    }
    if (mon->pack_idx)
    {
        savefile_write_byte(file, SAVE_MON_PACK_IDX);
        savefile_write_s16b(file, mon->pack_idx);
    }
    if (mon->ac_adj)
    {
        savefile_write_byte(file, SAVE_MON_AC);
        savefile_write_s16b(file, mon->ac_adj);
    }
    if (mon->mpower != 1000)
    {
        savefile_write_byte(file, SAVE_MON_POWER);
        savefile_write_s16b(file, mon->mpower);
    }
    if (mon->drop_ct)
    {
        savefile_write_byte(file, SAVE_MON_DROP_CT);
        savefile_write_byte(file, mon->drop_ct);
    }
    if (mon->stolen_ct)
    {
        savefile_write_byte(file, SAVE_MON_STOLEN_CT);
        savefile_write_byte(file, mon->stolen_ct);
    }
    if (mon->anti_magic_ct)
    {
        savefile_write_byte(file, SAVE_MON_ANTI_MAGIC);
        savefile_write_byte(file, mon->anti_magic_ct);
    }
    if (mon->pexp)
    {
        savefile_write_byte(file, SAVE_MON_PEXP);
        savefile_write_s32b(file, mon->pexp);
    }
    if (mon->anger)
    {
        savefile_write_byte(file, SAVE_MON_ANGER);
        savefile_write_byte(file, mon->anger);
    }
    if (mon->mana)
    {
        savefile_write_byte(file, SAVE_MON_MANA);
        savefile_write_s16b(file, mon->mana);
    }
    if (mon->turns)
    {
        savefile_write_byte(file, SAVE_MON_TURNS);
        savefile_write_u16b(file, mon->turns);
    }

    savefile_write_byte(file, SAVE_MON_DONE);
}

