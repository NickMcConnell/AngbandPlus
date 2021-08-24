#include "angband.h"

#include <assert.h>

void res_add(int gf)
{
    assert(GF_RES_MIN <= gf && gf <= GF_RES_MAX);
    plr->resist[gf]++;
}

void res_add_amt(int gf, int amt)
{
    assert(GF_RES_MIN <= gf && gf <= GF_RES_MAX);
    plr->resist[gf] += amt;
}

void res_add_ultimate(void)
{
    res_add(GF_ACID);
    res_add(GF_ELEC);
    res_add(GF_FIRE);
    res_add(GF_COLD);
    res_add(GF_POIS);
    res_add(GF_LIGHT);
    res_add(GF_DARK);
    res_add(GF_CONFUSION);
    res_add(GF_NETHER);
    res_add(GF_NEXUS);
    res_add(GF_SOUND);
    res_add(GF_SHARDS);
    res_add(GF_CHAOS);
    res_add(GF_DISENCHANT);
    res_add(GF_FEAR);
}

void res_add_immune(int gf)
{
    assert(GF_RES_MIN <= gf && gf <= GF_RES_MAX);
    plr->resist[gf] += 100;
}

void res_add_vuln(int gf)
{
    assert(GF_RES_MIN <= gf && gf <= GF_RES_MAX);
    plr->resist[gf]--;
}

bool res_is_high(int gf)
{
    gf_info_ptr gfi = gf_lookup(gf);
    return BOOL(gfi->flags & GFF_RESIST_HI);
}

bool res_is_low(int gf)
{
    switch (gf)
    {
    case GF_ACID:
    case GF_ELEC:
    case GF_FIRE:
    case GF_COLD:
    case GF_POIS: return TRUE;
    }
    return FALSE;
}

void res_calc_bonuses(u32b flgs[OF_ARRAY_SIZE])
{
    int i;
    for (i = GF_RES_MIN; i <= GF_RES_MAX; i++)
    {
        if (have_flag(flgs, OF_IM_(i)))
            res_add_immune(i);
        else if (have_flag(flgs, OF_VULN_(i)))
            res_add_vuln(i);
        else if (have_flag(flgs, OF_RES_(i)))
            res_add(i);
    }
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
int res_calc_dam(int gf, int dam)
{
    int pct1 = res_pct(gf);
    int pct2 = _randomize(pct1);
    int result = dam;

    result -= pct2 * dam / 100;
    if (result < 0)
        result = 0;

    if (result < dam)
    {
        int flag = result ? OF_RES_(gf) : OF_IM_(gf);
        equip_learn_resist(flag);
    }
    else if (result > dam)
    {
        equip_learn_vuln(OF_VULN_(gf));
    }

    return result;
}

void res_clear(void)
{
    int i;
    for (i = GF_RES_MIN; i <= GF_RES_MAX; i++)
        plr->resist[i] = 0;
}

cptr res_name(int gf)
{
    return gf_lookup(gf)->name;
}

byte res_color(int gf)
{
    return gf_lookup(gf)->color;
}


#define _MAX_PCTS 29
static int _pcts[_MAX_PCTS] = {
   0, 50, 65, 72, 75,
  77, 78, 79, 80, 81,
  82, 83, 84, 85, 86,
  87, 88, 89, 90, 91,
  92, 93, 94, 95, 96,
  97, 98, 99, 100
};

/* each resistance scales the default percentage table in a
 * customizable manner */
static int _gf_pct(int gf)
{
    /* high resists */
    switch (gf)
    {
    case GF_LIGHT:
    case GF_DARK:
    case GF_TIME:
        return 85;

    case GF_SOUND:
    case GF_CONFUSION:
        return 70;

    case GF_NETHER:
    case GF_CHAOS:
    case GF_SHARDS:
    case GF_DISENCHANT:
    case GF_NEXUS:
        return 60;
    }

    /* low resists */
    return 100;
}
static int _gf_scale(int gf, int amount)
{
    int pct = _gf_pct(gf);
    int x = (100 * amount + 50) * pct / 100;

    return x / 100;
}
int res_pct_mon(int gf)
{
    return _gf_scale(gf, _pcts[2]); /* cf mon_res_pct */
}
int res_pct_aux(int gf, int count)
{
    int result = 0;
    int idx = count;

    if (count < 0)
        idx *= -1;

    if (idx >= _MAX_PCTS)
        idx = _MAX_PCTS-1;

    result = _pcts[idx];
    if (result < 100)
        result = _gf_scale(gf, result);

    if (count < 0)
        result *= -1;
    else if (result < 100)
    {
        if (gf == GF_CONFUSION)
        {
            if (demon_is_(DEMON_CYBERDEMON))
                result = (result + 1) / 2;
        }

        if (gf == GF_LIGHT)
        {
            if (prace_is_(RACE_VAMPIRE) || prace_is_(RACE_MON_VAMPIRE) || prace_is_(MIMIC_VAMPIRE))
                result = (result + 1) / 2;
        }

        if (gf == GF_FIRE)
        {
            if (prace_is_(RACE_ENT))
                result = result * 7 / 10;
        }

        if (gf == GF_ELEC)
        {
            if (prace_is_(RACE_ANDROID))
                result = result * 7 / 10;
        }
    }
    return result;
}

int res_pct(int gf)
{
    int ct = plr->resist[gf];
    return res_pct_aux(gf, ct);
}

int res_ct_known(int gf)
{
    int ct = plr->resist[gf];
    int hidden = 0;
    int flg = OF_RES_(gf);
    int vuln_flg = OF_VULN_(gf);
    int i;

    /* Life is a bit hard at the moment since "player flags"
       may account for multiple resistances. Really, the entire
       flag based approach to resistance is just wrong, but I'm
       too lazy to fix ...
    */
    for (i = 1; i <= equip_max(); i++)
    {
        object_type *o_ptr = equip_obj(i);
        u32b         flgs[OF_ARRAY_SIZE];
        u32b         flgs_known[OF_ARRAY_SIZE];

        if (!o_ptr) continue;
        obj_flags(o_ptr, flgs);
        obj_flags_known(o_ptr, flgs_known);

        if (have_flag(flgs, flg) && !have_flag(flgs_known, flg))
            hidden++;
        if (have_flag(flgs, vuln_flg) && !have_flag(flgs_known, vuln_flg))
            hidden--;
    }

    ct -= hidden;
    return ct;
}

int res_pct_known(int gf)
{
    return res_pct_aux(gf, res_ct_known(gf));
}

bool res_save(int gf, int power)
{
    int pct = res_pct(gf);
    int roll = randint0(power);
    if (roll < pct)
    {
        equip_learn_resist(OF_RES_(gf));
        equip_learn_resist(OF_IM_(gf));
        return TRUE;
    }
    return FALSE;
}

bool res_save_default(int gf)
{
    int power = _gf_scale(gf, 55);
    return res_save(gf, power);
}
bool res_can_ignore(int gf)
{
    int power = _gf_scale(gf, 55);
    if (res_pct(gf) >= power)
        return TRUE;
    return FALSE;
}

bool res_save_inventory(int gf)
{
    int power = _gf_scale(gf, 66);

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
    if (prace_is_(RACE_ENT) && gf == GF_FIRE)
        power = 54;
    if (prace_is_(RACE_ANDROID) && gf == GF_ELEC)
        power = 54;

    return res_save(gf, power);
}
