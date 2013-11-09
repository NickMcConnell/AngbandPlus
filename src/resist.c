#include "angband.h"

void res_add(int which)
{
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

typedef struct {
    int flg;
    int res;
} _map_t;

static _map_t _resist_map[] = {
    { TR_RES_ACID, RES_ACID },
    { TR_RES_ELEC, RES_ELEC },
    { TR_RES_FIRE, RES_FIRE },
    { TR_RES_COLD, RES_COLD },
    { TR_RES_POIS, RES_POIS },
    { TR_RES_FEAR, RES_FEAR },
    { TR_RES_CONF, RES_CONF },
    { TR_RES_SOUND, RES_SOUND },
    { TR_RES_LITE, RES_LITE },
    { TR_RES_DARK, RES_DARK },
    { TR_RES_CHAOS, RES_CHAOS },
    { TR_RES_DISEN, RES_DISEN }, 
    { TR_RES_SHARDS, RES_SHARDS },
    { TR_RES_NEXUS, RES_NEXUS },
    { TR_RES_BLIND, RES_BLIND },
    { TR_RES_NETHER, RES_NETHER },
    { TR_RES_TIME, RES_TIME },
    { -1, -1 }
};

static _map_t _immunity_map[] = {
    { TR_IM_ACID, RES_ACID },
    { TR_IM_ELEC, RES_ELEC },
    { TR_IM_FIRE, RES_FIRE },
    { TR_IM_COLD, RES_COLD },
    { -1, -1 }
};

static _map_t _vulnerability_map[] = {
    { TR_VULN_ACID, RES_ACID },
    { TR_VULN_ELEC, RES_ELEC },
    { TR_VULN_FIRE, RES_FIRE },
    { TR_VULN_COLD, RES_COLD },
    { TR_VULN_POIS, RES_POIS },
    { TR_VULN_FEAR, RES_FEAR },
    { TR_VULN_CONF, RES_CONF },
    { TR_VULN_SOUND, RES_SOUND },
    { TR_VULN_LITE, RES_LITE },
    { TR_VULN_DARK, RES_DARK },
    { TR_VULN_CHAOS, RES_CHAOS },
    { TR_VULN_DISEN, RES_DISEN }, 
    { TR_VULN_SHARDS, RES_SHARDS },
    { TR_VULN_NEXUS, RES_NEXUS },
    { TR_VULN_BLIND, RES_BLIND },
    { TR_VULN_NETHER, RES_NETHER },
    { -1, -1 }
};

void res_calc_bonuses(u32b flgs[TR_FLAG_SIZE])
{
    int i;
    for (i = 0; ; i++)
    {
        _map_t m = _resist_map[i];
        if (m.flg < 0) break;
        if (have_flag(flgs, m.flg))
            res_add(m.res);
    }
    for (i = 0; ; i++)
    {
        _map_t m = _immunity_map[i];
        if (m.flg < 0) break;
        if (have_flag(flgs, m.flg))
            res_add_immune(m.res);
    }
    for (i = 0; ; i++)
    {
        _map_t m = _vulnerability_map[i];
        if (m.flg < 0) break;
        if (have_flag(flgs, m.flg))
            res_add_vuln(m.res);
    }
}

int  res_get_object_flag(int which)
{
    int i;
    for (i = 0; ; i++)
    {
        _map_t m = _resist_map[i];
        if (m.flg < 0) break;
        if (m.res == which) return m.flg;
    }
    return -1;
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

static cptr _names[RES_MAX] = {
    "acid",
    "lightning",
    "fire",
    "cold",
    "poison",
    "light",
    "dark",
    "confusion",
    "nether",
    "nexus",
    "sound",
    "shards",
    "chaos",
    "disenchantment",
    "time",
    "blindness",
    "fear",
    "teleportation",
};

cptr res_name(int which)
{
    if (which >= 0 && which < RES_MAX)
        return _names[which];
    return "";
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

int  res_pct_aux(int count)
{
    int result = 0;
    int idx = count;

    if (count < 0)
        idx *= -1;

    if (idx >= _MAX_PCTS)
        idx = _MAX_PCTS-1;

    result = _pcts[idx];
    if (count < 0)
        result *= -1;

    return result;
}

int res_pct(int which)
{
    int ct = p_ptr->resist[which];
    return res_pct_aux(ct);
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
    return res_save(which, 55);
}
bool res_can_ignore(int which)
{
    if (res_pct(which) >= 55)
        return TRUE;
    return FALSE;
}

bool res_save_inventory(int which)
{
    return res_save(which, 66);
}
