#include "angband.h"

#include <assert.h>

void res_add(int which)
{
    assert(0 <= which && which < RES_MAX);
    p_ptr->resist[which]++;
}

void res_add_amt(int which, int amt)
{
    p_ptr->resist[which] += amt;
}

void res_add_all(void)
{
    int i;
    for (i = 0; i < RES_MAX; i++)
        res_add(i);
}

void res_add_immune(int which)
{
    p_ptr->resist[which] += 100;
}

void res_add_vuln(int which)
{
    p_ptr->resist[which]--;
}

bool res_is_high(int which)
{
    if (res_is_low(which))
        return FALSE;
    return TRUE;
}

bool res_is_low(int which)
{
    switch (which)
    {
    case RES_ACID:
    case RES_ELEC:
    case RES_FIRE:
    case RES_COLD:
    case RES_POIS:
        return TRUE;
    }
    return FALSE;
}

typedef struct {
    cptr name;
    byte color;
    int flg;
    int vuln_flg;
    int im_flg;
} _res_info_t;

static _res_info_t _resist_map[RES_MAX] = {
    { "Acid",           TERM_GREEN,   TR_RES_ACID,    TR_VULN_ACID,   TR_IM_ACID },
    { "Electricity",    TERM_BLUE,    TR_RES_ELEC,    TR_VULN_ELEC,   TR_IM_ELEC },
    { "Fire",           TERM_RED,     TR_RES_FIRE,    TR_VULN_FIRE,   TR_IM_FIRE },
    { "Cold",           TERM_L_WHITE, TR_RES_COLD,    TR_VULN_COLD,   TR_IM_COLD },
    { "Poison",         TERM_L_GREEN, TR_RES_POIS,    TR_VULN_POIS,   TR_IM_POIS },
    { "Light",          TERM_YELLOW,  TR_RES_LITE,    TR_VULN_LITE,   TR_IM_LITE },
    { "Dark",           TERM_L_DARK,  TR_RES_DARK,    TR_VULN_DARK,   TR_IM_DARK },
    { "Confusion",      TERM_L_RED,   TR_RES_CONF,    TR_VULN_CONF,   TR_INVALID },
    { "Nether",         TERM_L_DARK,  TR_RES_NETHER,  TR_VULN_NETHER, TR_IM_NETHER },
    { "Nexus",          TERM_VIOLET,  TR_RES_NEXUS,   TR_VULN_NEXUS,  TR_INVALID },
    { "Sound",          TERM_ORANGE,  TR_RES_SOUND,   TR_VULN_SOUND,  TR_INVALID },
    { "Shards",         TERM_L_UMBER, TR_RES_SHARDS,  TR_VULN_SHARDS, TR_INVALID },
    { "Chaos",          TERM_VIOLET,  TR_RES_CHAOS,   TR_VULN_CHAOS,  TR_INVALID },
    { "Disenchantment", TERM_VIOLET,  TR_RES_DISEN,   TR_VULN_DISEN,  TR_INVALID },
    { "Time",           TERM_L_BLUE,  TR_RES_TIME,    TR_INVALID,     TR_INVALID },
    { "Blindness",      TERM_L_DARK,  TR_RES_BLIND,   TR_VULN_BLIND,  TR_IM_BLIND },
    { "Fear",           TERM_L_RED,   TR_RES_FEAR,    TR_VULN_FEAR,   TR_IM_FEAR },
    { "Teleportation",  TERM_ORANGE,  TR_NO_TELE,     TR_INVALID,     TR_INVALID }
};

void res_calc_bonuses(u32b flgs[TR_FLAG_SIZE])
{
    int i;
    for (i = RES_BEGIN; i < RES_END; i++)
    {
        _res_info_t m = _resist_map[i];
        if (m.flg != TR_INVALID && have_flag(flgs, m.flg))
            res_add(i);
        if (m.vuln_flg != TR_INVALID && have_flag(flgs, m.vuln_flg))
            res_add_vuln(i);
        if (m.im_flg != TR_INVALID && have_flag(flgs, m.im_flg))
            res_add_immune(i);
    }
}

bool res_has_bonus(u32b flgs[TR_FLAG_SIZE])
{
    int i;
    for (i = RES_BEGIN; i < RES_END; i++)
    {
        _res_info_t m = _resist_map[i];
        int    net = 0;
        if (m.im_flg != TR_INVALID && have_flag(flgs, m.im_flg))
            return TRUE;
        if (m.flg != TR_INVALID && have_flag(flgs, m.flg))
            net++;
        if (m.vuln_flg != TR_INVALID && have_flag(flgs, m.vuln_flg))
            net--;
        if (net)
            return TRUE;
    }
    return FALSE;
}

int  res_get_object_flag(int which)
{
    return _resist_map[which].flg;
}

int  res_get_object_vuln_flag(int which)
{
    return _resist_map[which].vuln_flg;
}

int  res_get_object_immune_flag(int which)
{
    return _resist_map[which].im_flg;
}

static int _randomize(int pct)
{
    if (pct != 100)
    {
        if (pct > 0)
            pct = randnor(pct, pct/10);
        else if (pct < 0)
            pct = -randnor(-pct, -pct/10);
    }
    return pct;
}
int res_calc_dam(int which, int dam)
{
    int pct1 = res_pct(which);
    int pct2 = _randomize(pct1);
    int result = dam;

    result -= pct2 * dam / 100;
    if (result < 0)
        result = 0;

    return result;
}

void res_clear(void)
{
    int i;
    for (i = 0; i < RES_MAX; i++)
        p_ptr->resist[i] = 0;
}

cptr res_name(int which)
{
    return _resist_map[which].name;
}

byte res_color(int which)
{
    return _resist_map[which].color;
}


#define _MAX_PCTS 29
static int _lo_pcts[_MAX_PCTS] = {
   0, 50, 65, 72, 75,
  77, 78, 79, 80, 81,
  82, 83, 84, 85, 86,
  87, 88, 89, 90, 91,
  92, 93, 94, 95, 96,
  97, 98, 99, 100
};

static int _hi_pcts[_MAX_PCTS] = {
   0, 30, 40, 45, 47,
  48, 49, 50, 51, 52,
  53, 54, 55, 56, 57,
  58, 59, 60, 61, 62,
  63, 64, 65, 66, 67,
  68, 69, 70, 100
};
/* Note: I've decided to move back to a two-tiered resistance system.
 * While I like the simplicity of a single system, the fact is that it
 * forced the player to cover all resists, especially rather early on when
 * it is practically impossible to do so. This redesign is closer to the
 * original Hengband system. Also, you can no longer "shut-down" end game
 * high damage with multiple resists like before:
 * Attack  Hengband(1x) Old(3x) New(3x) New(1x)
 * ======= ============ ======= ======= =======
 * ROCKET           400     224     330     420
 * BR_CHAO          439     196     330     420
 * BR_NETH          356     182     302     385

 * These are the Serpent's big three attacks. As you can see, he could be
 * made quite tame with enough resists. But no longer ;)
 */


int  res_pct_aux(int which, int count)
{
    int result = 0;
    int idx = count;

    if (count < 0)
        idx *= -1;

    if (idx >= _MAX_PCTS)
        idx = _MAX_PCTS-1;

    if (res_is_low(which))
        result = _lo_pcts[idx];
    else
        result = _hi_pcts[idx];

    if (count < 0)
        result *= -1;
    else if (result < 100)
    {
        if (which == RES_CONF)
        {
            if (prace_is_(RACE_TONBERRY))
                result = (result + 1) / 2;
        }

        if (which == RES_LITE)
        {
            if (prace_is_(RACE_VAMPIRE) || prace_is_(RACE_MON_VAMPIRE) || prace_is_(MIMIC_VAMPIRE))
                result = (result + 1) / 2;
        }

        if (which == RES_FIRE)
        {
            if (prace_is_(RACE_ENT))
                result = result * 7 / 10;
        }

        if (which == RES_ELEC)
        {
            if (prace_is_(RACE_ANDROID))
                result = result * 7 / 10;
        }
    }
    return result;
}

int res_pct(int which)
{
    int ct = p_ptr->resist[which];
    return res_pct_aux(which, ct);
}

int res_ct_known(int which)
{
    int ct = p_ptr->resist[which];
    int hidden = 0;
    int flg = res_get_object_flag(which);
    int i;

    /* Life is a bit hard at the moment since "player flags"
       may account for multiple resistances. Really, the entire
       flag based approach to resistance is just wrong, but I'm
       too lazy to fix ...
    */
    for (i = 0; i < equip_count(); i++)
    {
        int          slot = EQUIP_BEGIN + i;
        object_type *o_ptr = equip_obj(slot);
        u32b         flgs[TR_FLAG_SIZE];
        u32b         flgs_known[TR_FLAG_SIZE];

        if (!o_ptr) continue;
        object_flags(o_ptr, flgs);
        object_flags_known(o_ptr, flgs_known);

        if (have_flag(flgs, flg) && !have_flag(flgs_known, flg))
            hidden++;
    }

    ct -= hidden;
    return ct;
}

int res_pct_known(int which)
{
    return res_pct_aux(which, res_ct_known(which));
}

bool res_save(int which, int power)
{
    int pct = res_pct(which);
    int roll = randint0(power);
    if (roll < pct)
        return TRUE;
    return FALSE;
}

bool res_save_default(int which)
{
    int power = res_is_low(which) ? 55 : 33;
    return res_save(which, power);
}
bool res_can_ignore(int which)
{
    int power = res_is_low(which) ? 55 : 33;
    if (res_pct(which) >= power)
        return TRUE;
    return FALSE;
}

bool res_save_inventory(int which)
{
    int power = res_is_low(which) ? 66 : 41;

    /* Mercy for racial vulnerabilities:
        ResCt  Old% New%
        =====  ==== ====
          1    24.2 35.2
          2     1.5 16.7
          3     0.0  7.4
          4          3.7
          5          1.9
          6          0.0
    */
    if (prace_is_(RACE_ENT) && which == RES_FIRE)
        power = 54;
    if (prace_is_(RACE_ANDROID) && which == RES_ELEC)
        power = 54;

    return res_save(which, power);
}
