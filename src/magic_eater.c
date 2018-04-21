#include "angband.h"

/* TODO: Stop using p_ptr->magicnum*. Rewrite load/save code and keep a local
   map (tv,sv)->{ ... } instead. */

#define _EATER_CHARGE 0x10000L
#define _EATER_ROD_CHARGE 0x10L

/* Magic-Eaters are a hack. They use eaten devices as if they were spells. */
bool magic_eater_hack = FALSE;
static cptr _do_device(int tval, int sval, int mode)
{
    cptr res;
    magic_eater_hack = TRUE;
    device_known = TRUE;
    res = do_device(tval, sval, mode);
    magic_eater_hack = FALSE;
    return res;
}

/* Kinds of Objects */
typedef struct {
    int tval;
    int sval;
    int k_idx;   /* memoized */
} _kind_t, *_kind_ptr;

static int _lookup_kind(_kind_ptr kind)
{
    if (!kind->k_idx)
        kind->k_idx = lookup_kind(kind->tval, kind->sval);
    return kind->k_idx;
}

static cptr _kind_name(_kind_ptr kind)
{
    int k_idx = _lookup_kind(kind);
    return k_name + k_info[k_idx].name;
}

static cptr _kind_desc(_kind_ptr kind)
{
    static char buf[1024];
    cptr res = _do_device(kind->tval, kind->sval, SPELL_DESC);
    strcpy(buf, res);
    res =  _do_device(kind->tval, kind->sval, SPELL_INFO);
    if (res && strlen(res))
    {
        strcat(buf, " (");
        strcat(buf, res);
        strcat(buf, ")");
    }
    return buf;
}

/* Allowed objects for Eating */
typedef struct {
    int     idx;
    _kind_t kind;
    int     options;
} _spell_t, *_spell_ptr;
typedef void (*_spell_fn)(_spell_ptr spell);

static int _calc_fail_rate(_spell_ptr spell)
{
    int result = 0;
    int k_idx = _lookup_kind(&spell->kind);
    int lvl = k_info[k_idx].level;

    if (spell->kind.tval == TV_ROD)
        lvl = lvl * 5/6 - 5;
    
    result = lvl * 4 / 5 + 20;
    result -= 3 * (adj_mag_stat[p_ptr->stat_ind[A_INT]] - 1);
    lvl /= 2;
    if (p_ptr->lev > lvl)
        result -= 3 * (p_ptr->lev - lvl);

    result = mod_spell_chance_1(result, REALM_NONE);
    result = MAX(result, adj_mag_fail[p_ptr->stat_ind[A_INT]]);

    if (p_ptr->stun > 50) result += 25;
    else if (p_ptr->stun) result += 15;
    if (result> 95) result = 95;

    result = mod_spell_chance_2(result, REALM_NONE);
    return result;
}

static int _calc_charges_total(_spell_ptr spell)
{
    return p_ptr->magic_num2[spell->idx];
}

static int _calc_charges(_spell_ptr spell)
{
    int result = 0;
    int magic = p_ptr->magic_num1[spell->idx];
    if (spell->kind.tval == TV_ROD)
    {
        result = p_ptr->magic_num2[spell->idx];
        if (magic)
        {
            int k_idx = _lookup_kind(&spell->kind);
            result -= (magic - 1) / (_EATER_ROD_CHARGE * k_info[k_idx].pval) + 1;
        }
    }
    else
        result = magic/_EATER_CHARGE;

    return result;
}

static void _use_charge(_spell_ptr spell)
{
    if (spell->kind.tval == TV_ROD)
    {
        int k_idx = _lookup_kind(&spell->kind);
        p_ptr->magic_num1[spell->idx] += k_info[k_idx].pval * _EATER_ROD_CHARGE;
    }
    else 
        p_ptr->magic_num1[spell->idx] -= _EATER_CHARGE;
}

/*
static bool _suppress(_spell_ptr spell)
{
    object_type o = {0};
    int         k_idx = _lookup_kind(spell->kind);
    int         ap_idx;

    object_prep(&o, k_idx);
    identify_item(&o);
    o.ident |= IDENT_MENTAL;
    ap_idx = is_autopick(&o);
    if (ap_idx >= 0 && (autopick_list[ap_idx].action & DO_AUTODESTROY))
        return TRUE; 
    return FALSE;
} */

/* Grouped for Cleaner Menu Display */
#define _MAX_PER_GROUP 20
typedef struct {
    cptr     name;
    _spell_t spells[_MAX_PER_GROUP];
} _group_t, *_group_ptr;

static _group_t _staves[] = {
    { "Detection", 
      { { 12, {TV_STAFF, SV_STAFF_DETECT_TRAP, 0}, 0},
        { 13, {TV_STAFF, SV_STAFF_DETECT_DOOR, 0}, 0},
        { 10, {TV_STAFF, SV_STAFF_DETECT_GOLD, 0}, 0},
        { 11, {TV_STAFF, SV_STAFF_DETECT_ITEM, 0}, 0},
        { 14, {TV_STAFF, SV_STAFF_DETECT_INVIS, 0}, 0},
        { 15, {TV_STAFF, SV_STAFF_DETECT_EVIL, 0}, 0},
        { 9, {TV_STAFF, SV_STAFF_MAPPING, 0}, 0},
        { -1, {-1, -1, -1}, -1} }},
    { "Healing", 
      { { 16, {TV_STAFF, SV_STAFF_CURE_LIGHT, 0}, 0},
        { 17, {TV_STAFF, SV_STAFF_CURING, 0}, 0},
        { 18, {TV_STAFF, SV_STAFF_HEALING, 0}, 0},
        { -1, {-1, -1, -1}, -1} }},
    { "Offense", 
      { { 7, {TV_STAFF, SV_STAFF_STARLITE, 0}, 0},
        { 24, {TV_STAFF, SV_STAFF_DISPEL_EVIL, 0}, 0},
        { 26, {TV_STAFF, SV_STAFF_HOLINESS, 0}, 0},
        { 25, {TV_STAFF, SV_STAFF_POWER, 0}, 0},
        { 27, {TV_STAFF, SV_STAFF_GENOCIDE, 0}, 0},
        { 31, {TV_STAFF, SV_STAFF_MSTORM, 0}, 0},
        { -1, {-1, -1, -1}, -1} }},
    { "Utility", 
      { { 8, {TV_STAFF, SV_STAFF_LITE, 0}, 0},
        { 5, {TV_STAFF, SV_STAFF_IDENTIFY, 0}, 0},    
        { 20, {TV_STAFF, SV_STAFF_SLEEP_MONSTERS, 0}, 0},
        { 21, {TV_STAFF, SV_STAFF_SLOW_MONSTERS, 0}, 0},
        { 6, {TV_STAFF, SV_STAFF_REMOVE_CURSE, 0}, 0},
        { 4, {TV_STAFF, SV_STAFF_TELEPORTATION, 0}, 0},
        { 23, {TV_STAFF, SV_STAFF_PROBING, 0}, 0},
        { 28, {TV_STAFF, SV_STAFF_EARTHQUAKES, 0}, 0},
        { 30, {TV_STAFF, SV_STAFF_ANIMATE_DEAD, 0}, 0},
        { 22, {TV_STAFF, SV_STAFF_SPEED, 0}, 0},
        { 29, {TV_STAFF, SV_STAFF_DESTRUCTION, 0}, 0},
        { -1, {-1, -1, -1}, -1} }},
    { "Junk", 
      { { 0, {TV_STAFF, SV_STAFF_DARKNESS, 0}, 0},
        { 1, {TV_STAFF, SV_STAFF_SLOWNESS, 0}, 0},
        { 2, {TV_STAFF, SV_STAFF_HASTE_MONSTERS, 0}, 0},
        { 3, {TV_STAFF, SV_STAFF_SUMMONING, 0}, 0},
        { -1, {-1, -1, -1}, -1} }},
    { NULL, {{ -1, {-1, -1, -1}, -1} }}
};
static _group_t _wands[] = {
    { "Offense", 
      { { 36+15, {TV_WAND, SV_WAND_MAGIC_MISSILE, 0}, 0},
        { 36+14, {TV_WAND, SV_WAND_STINKING_CLOUD, 0}, 0},
        { 36+24, {TV_WAND, SV_WAND_WONDER, 0}, 0},
        { 36+19, {TV_WAND, SV_WAND_COLD_BOLT, 0}, 0},
        { 36+16, {TV_WAND, SV_WAND_ACID_BOLT, 0}, 0},
        { 36+18, {TV_WAND, SV_WAND_FIRE_BOLT, 0}, 0},
        { 36+21, {TV_WAND, SV_WAND_ELEC_BALL, 0}, 0},
        { 36+23, {TV_WAND, SV_WAND_COLD_BALL, 0}, 0},
        { 36+20, {TV_WAND, SV_WAND_ACID_BALL, 0}, 0},
        { 36+22, {TV_WAND, SV_WAND_FIRE_BALL, 0}, 0},
        { 36+12, {TV_WAND, SV_WAND_DRAIN_LIFE, 0}, 0},
        { 36+26, {TV_WAND, SV_WAND_DRAGON_FIRE, 0}, 0},
        { 36+27, {TV_WAND, SV_WAND_DRAGON_COLD, 0}, 0},
        { 36+28, {TV_WAND, SV_WAND_DRAGON_BREATH, 0}, 0},
        { 36+31, {TV_WAND, SV_WAND_GENOCIDE, 0}, 0},
        { 36+30, {TV_WAND, SV_WAND_STRIKING, 0}, 0},
        { 36+25, {TV_WAND, SV_WAND_DISINTEGRATE, 0}, 0},
        { 36+29, {TV_WAND, SV_WAND_ROCKETS, 0}, 0},
        { -1, {-1, -1, -1}, -1} }},
    { "Utility", 
      { { 36+7, {TV_WAND, SV_WAND_LITE, 0}, 0},
        { 36+5, {TV_WAND, SV_WAND_TRAP_DOOR_DEST, 0}, 0},
        { 36+10, {TV_WAND, SV_WAND_CONFUSE_MONSTER, 0}, 0},
        { 36+11, {TV_WAND, SV_WAND_FEAR_MONSTER, 0}, 0},
        { 36+8, {TV_WAND, SV_WAND_SLEEP_MONSTER, 0}, 0},
        { 36+9, {TV_WAND, SV_WAND_SLOW_MONSTER, 0}, 0},
        { 36+13, {TV_WAND, SV_WAND_POLYMORPH, 0}, 0},
        { 36+17, {TV_WAND, SV_WAND_CHARM_MONSTER, 0}, 0},
        { 36+4, {TV_WAND, SV_WAND_DISARMING, 0}, 0},
        { 36+6, {TV_WAND, SV_WAND_STONE_TO_MUD, 0}, 0},
        { 36+3, {TV_WAND, SV_WAND_TELEPORT_AWAY, 0}, 0},
        { -1, {-1, -1, -1}, -1} }},
    { "Junk", 
      { { 36+0, {TV_WAND, SV_WAND_HEAL_MONSTER, 0}, 0},
        { 36+1, {TV_WAND, SV_WAND_HASTE_MONSTER, 0}, 0},
        { 36+2, {TV_WAND, SV_WAND_CLONE_MONSTER, 0}, 0},
        { -1, {-1, -1, -1}, -1} }},
    { NULL, {{ -1, {-1, -1, -1}, -1} }}
};
static _group_t _rods[] = {
    { "Detection", 
      {    { 72+0, {TV_ROD, SV_ROD_DETECT_TRAP, 0}, 0}, 
        { 72+1, {TV_ROD, SV_ROD_DETECT_DOOR, 0}, 0}, 
        { 72+31, {TV_ROD, SV_ROD_DETECT_MONSTERS, 0}, 0}, 
        { 72+5, {TV_ROD, SV_ROD_MAPPING, 0}, 0}, 
        { 72+6, {TV_ROD, SV_ROD_DETECTION, 0}, 0}, 
        { -1, {-1, -1, -1}, -1} }},
    { "Healing", 
      {    { 72+8, {TV_ROD, SV_ROD_CURING, 0}, 0}, 
        { 72+9, {TV_ROD, SV_ROD_HEALING, 0}, 0}, 
        { 72+10, {TV_ROD, SV_ROD_RESTORATION, 0}, 0}, 
        { -1, {-1, -1, -1}, -1} }},
    { "Offense", 
      {    { 72+15, {TV_ROD, SV_ROD_LITE, 0}, 0}, 
        { 72+12, {TV_ROD, SV_ROD_PESTICIDE, 0}, 0},
        { 72+21, {TV_ROD, SV_ROD_ELEC_BOLT, 0}, 0},
        { 72+23, {TV_ROD, SV_ROD_COLD_BOLT, 0}, 0},
        { 72+20, {TV_ROD, SV_ROD_ACID_BOLT, 0}, 0},
        { 72+22, {TV_ROD, SV_ROD_FIRE_BOLT, 0}, 0},
        { 72+25, {TV_ROD, SV_ROD_ELEC_BALL, 0}, 0},
        { 72+27, {TV_ROD, SV_ROD_COLD_BALL, 0}, 0},
        { 72+24, {TV_ROD, SV_ROD_ACID_BALL, 0}, 0},
        { 72+26, {TV_ROD, SV_ROD_FIRE_BALL, 0}, 0},
        { 72+18, {TV_ROD, SV_ROD_DRAIN_LIFE, 0}, 0},
        { 72+34, {TV_ROD, SV_ROD_MANA_BOLT, 0}, 0}, 
        { 72+33, {TV_ROD, SV_ROD_MANA_BALL, 0}, 0}, 
        { 72+28, {TV_ROD, SV_ROD_HAVOC, 0}, 0},
        { -1, {-1, -1, -1}, -1} }},
    { "Utility", 
      {    { 72+30, {TV_ROD, SV_ROD_AGGRAVATE, 0}, 0},
        { 72+4, {TV_ROD, SV_ROD_ILLUMINATION, 0}, 0}, 
        { 72+16, {TV_ROD, SV_ROD_SLEEP_MONSTER, 0}, 0}, 
        { 72+17, {TV_ROD, SV_ROD_SLOW_MONSTER, 0}, 0}, 
        { 72+19, {TV_ROD, SV_ROD_POLYMORPH, 0}, 0}, 
        { 72+14, {TV_ROD, SV_ROD_DISARMING, 0}, 0}, 
        { 72+32, {TV_ROD, SV_ROD_ESCAPING, 0}, 0}, 
        { 72+13, {TV_ROD, SV_ROD_TELEPORT_AWAY, 0}, 0}, 
        { 72+29, {TV_ROD, SV_ROD_STONE_TO_MUD, 0}, 0}, 
        { 72+7, {TV_ROD, SV_ROD_PROBING, 0}, 0}, 
        { 72+3, {TV_ROD, SV_ROD_RECALL, 0}, 0}, 
        { 72+2, {TV_ROD, SV_ROD_IDENTIFY, 0}, 0}, 
        { 72+11, {TV_ROD, SV_ROD_SPEED, 0}, 0}, 
        { -1, {-1, -1, -1}, -1} }},
    { NULL, {{ -1, {-1, -1, -1}, -1} }}
};

static _group_ptr _which_groups(int tval)
{
    switch (tval)
    {
    case TV_STAFF: return _staves;
    case TV_WAND: return _wands;
    case TV_ROD: return _rods;
    }
    return NULL;
}

static void _for_each(int tval, _spell_fn f)
{
    _group_ptr gs = _which_groups(tval);
    int i, j;
    for (i = 0; ; i++)
    {
        _group_ptr g = gs + i;
        if (!g->name) break;
        for (j = 0; ; j++)
        {
            _spell_ptr s = g->spells + j;
            if (s->idx < 0) break;
            f(s);
        }
    }
}

static int _groups_count(_group_t *groups)
{
    int result = 0;
    int i;
    for (i = 0; ; i++)
    {
        if (!groups[i].name) break;
        result++;
    }
    return result;
}
static int _spells_count(_spell_t *spells)
{
    int result = 0;
    int i;
    for (i = 0; ; i++)
    {
        if (spells[i].idx < 0) break;
        result++;
    }
    return result;
}
static int _spells_count_allowed(_spell_t *spells)
{
    int result = 0;
    int i;
    for (i = 0; ; i++)
    {
        if (spells[i].idx < 0) break;
        if (_calc_charges_total(&spells[i]))
            result++;
    }
    return result;
}

/* Menu Code 1: Choose which type of magic to use (tval) */
typedef struct {
    int tval;
    cptr name;
} _tval_menu_t;
static cptr _tval_choice = NULL;
static _tval_menu_t _tval_choices[3] = {
    { TV_STAFF, "Staff" },
    { TV_WAND, "Wand" },
    { TV_ROD, "Rod" },
};

static void _tval_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    switch (cmd)
    {
    case MENU_KEY:
        var_set_int(res, _tval_choices[which].name[0]);
        break;
    case MENU_TEXT:
        var_set_string(res, format("%s", _tval_choices[which].name));
        break;
    default:
        default_menu(cmd, which, cookie, res);
    }
}

static int _prompt_tval(int tval)
{
    int idx = -1;
    menu_t menu = { "Use which type of device?", NULL, NULL,
                        _tval_menu_fn, 
                        NULL, 3};

    if (tval)
    {
        int i;
        for (i = 0; i < 3; i++)
        {
            if (_tval_choices[i].tval == tval)
            {
                _tval_choice = _tval_choices[i].name;
                return tval;
            }
        }
    }

    idx = menu_choose(&menu);
    if (idx < 0) return 0;
    _tval_choice = _tval_choices[idx].name;
    return _tval_choices[idx].tval;
}

/* Menu Code 2: Choose which group of magic to use */
static cptr _group_choice = NULL;

static void _group_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    _group_t *groups = (_group_t*)cookie;
    switch (cmd)
    {
    case MENU_KEY:
        var_set_int(res, groups[which].name[0]);
        break;
    case MENU_TEXT:
        var_set_string(res, format("%s", groups[which].name));
        break;
    default:
        default_menu(cmd, which, cookie, res);
    }
}

static _group_t *_prompt_group(int tval)
{
    _group_t *result = NULL;
    _group_t *groups = _which_groups(tval);
    if (groups)
    {
        int idx = -1;
        char prompt[255];
        menu_t menu = { prompt, NULL, NULL,
                        _group_menu_fn,
                        groups, _groups_count(groups)};

        sprintf(prompt, "Use which type of %s?", _tval_choice);
        idx = menu_choose(&menu);
        if (idx < 0) return NULL;
        _group_choice = groups[idx].name;
        return &groups[idx];
    }
    return result;
}

/* Menu Code 3: Choose which spell to use */
typedef struct {
    _spell_t *spell;
    int       charges;
    int       total_charges;
    int       fail;
} _spell_menu_t;

static void _spell_menu_fn(int cmd, int which, vptr cookie, variant *res)
{
    _spell_menu_t *ss = (_spell_menu_t*)cookie;
    _spell_menu_t *s = ss + which;

    switch (cmd)
    {
    case MENU_TEXT:
        if (s->total_charges)
        {
            char    buf[1024];
            _kind_t k = s->spell->kind;
            cptr    info = _do_device(s->spell->kind.tval, s->spell->kind.sval, SPELL_INFO);

            sprintf(buf, "%-22.22s %3d %3d %3d%% ", _kind_name(&k), s->charges, s->total_charges, s->fail);
            if (info)
                strcat(buf, info);
            var_set_string(res, buf);
        }
        else
            var_clear(res);
        break;            
    case MENU_HELP:
        var_set_string(res, _kind_desc(&s->spell->kind));
        break;
    case MENU_COLOR:
        if (!s->total_charges)
            var_set_int(res, TERM_DARK);
        else if (!s->charges)
            var_set_int(res, TERM_RED);
        else
            var_set_int(res, TERM_WHITE);
        break;
    default:
        default_menu(cmd, which, cookie, res);
    }
}

static _spell_t *_prompt_spell(_spell_t *spells)
{
    _spell_menu_t choices[_MAX_PER_GROUP];
    int           ct_total = _spells_count(spells);
    int           ct_avail = 0;
    int           i;

    for (i = 0; i < ct_total; i++)
    {
        _spell_ptr spell = &spells[i];
        
        /*if (_calc_charges_total(spell) && !_suppress(spell))*/
        {
            _spell_menu_t *choice = &choices[ct_avail];
            
            choice->spell = spell;
            choice->charges = _calc_charges(spell);
            choice->total_charges = _calc_charges_total(spell);
            choice->fail = _calc_fail_rate(spell);

            ct_avail++;
        }
    }

    if (!ct_avail)
    {
        msg_print("You haven't absorbed any of these items yet.");
    }
    else
    {
        int    idx = -1;
        char   heading[255], prompt1[255], prompt2[255];
        menu_t menu = { prompt1, prompt2, heading,
                        _spell_menu_fn, choices, ct_avail};

        sprintf(prompt1, "Use which type of %s (%s)?", _group_choice, _tval_choice);
        sprintf(prompt2, "Browse which type of %s (%s)?", _group_choice, _tval_choice);
        sprintf(heading, "%-22.22s Chg Tot Fail Info", "");
        idx = menu_choose(&menu);
        if (idx >= 0)
            return choices[idx].spell;
    }
    return NULL;
}

/* Menu Code: Putting it all together
   Hack: To address complaints about adding a single (!) extra keystroke,
   I've added code to bypass the tval selection. Simply use normal zap,
   aim or use commands and you end up here, provided that your inventory
   does not have a qualifying object. It is unlikely that a magic-eater
   would not eat a given object, but they can just use the normal 'm' 
   command if necessary.
*/
static _spell_t *_prompt(int tval)
{
    int tval2;
    _group_ptr group;
    _spell_ptr spell;

    for (;;)
    {
        tval2 = _prompt_tval(tval); /* _tval_choice needs to be set for submenus! */
        if (tval2 <= 0) break;
        for (;;)
        {
            group = _prompt_group(tval2);
            if (!group)
            {
                if (tval) return NULL;
                else return NULL/*break*/;
            }
            spell = _prompt_spell(group->spells);
            if (spell)
                return spell;
        }
    }
    return NULL;
}

static void _browse(void)
{
    int tval;
    _group_ptr group;
    _spell_ptr spell;
    int i, ct, line;
    char tmp[62*10];

    for (;;)
    {
        tval = _prompt_tval(0);
        if (tval <= 0) break;
        for (;;)
        {
            group = _prompt_group(tval);
            if (!group) break;
            ct = _spells_count_allowed(group->spells);
            screen_save();
            for (;;)
            {
                spell = _prompt_spell(group->spells);
                if (!spell) break;
                for (i = 0; i < 7; i++)
                    Term_erase(13, ct + i + 2, 255);

                roff_to_buf(_kind_desc(&spell->kind), 62, tmp, sizeof(tmp));

                for(i = 0, line = ct + 3; tmp[i]; i += 1+strlen(&tmp[i]))
                {
                    prt(&tmp[i], line, 15);
                    line++;
                }
            }
            screen_load();            
        }
    }
}

/* Map (tval, sval) -> Index for "Magic Numbers"
   The only time we need to map in this direction
   is during "absorption". */
static int _find_idx_spells(_spell_t *spells, int sval)
{
    int result = -1;
    int i;
    for (i = 0; ; i++)
    {
        if (spells[i].idx == -1) break;
        if (spells[i].kind.sval == sval)
        {
            result = spells[i].idx;
            break;
        }
    }
    return result;
}

static int _find_idx_groups(_group_t *groups, int sval)
{
    int result = -1;
    int i;
    for (i = 0; ; i++)
    {
        if (!groups[i].name) break;
        result = _find_idx_spells(groups[i].spells, sval);
        if (result >= 0) break;
    }
    return result;
}

static int _find_idx(int tval, int sval)
{
    _group_t *groups = _which_groups(tval);
    if (groups)
        return _find_idx_groups(groups, sval);
    return -1;
}

/* Use Magic */
void magic_eater_browse(void)
{
    _browse();
}

void magic_eater_cast(int tval)
{
    int chance;
    _spell_t *spell;

    /* Duplicate anti-magic checks since "device" commands might re-route here (as "magic" commands)
       For example, do_cmd_use_staff() will allow magic-eaters to invoke staff based spells. */
    if (dun_level && (d_info[dungeon_type].flags1 & DF1_NO_MAGIC))
    {
        msg_print("The dungeon absorbs all attempted magic!");
        return;
    }
    else if (p_ptr->tim_no_spells)
    {
        msg_print("Your spells are blocked!");
        return;
    }
    else if (p_ptr->anti_magic)
    {
        msg_print("An anti-magic shell disrupts your magic!");
        return;
    }
    else if (IS_SHERO())
    {
        msg_print("You cannot think clearly!");
        return;
    }

    if (p_ptr->confused)
    {
        msg_print("You are too confused!");
        return;
    }

    spell = _prompt(tval);
    if (!spell)
        return;
    if (!_calc_charges(spell))
    {
        msg_print("You are out of charges!");
        return;
    }

    energy_use = 100;
    chance = _calc_fail_rate(spell);
    if (randint0(100) < chance)
    {
        if (flush_failure) flush();
        msg_format("You failed to get the magic off!");
        sound(SOUND_FAIL);
        if (randint1(100) >= chance)
            virtue_add(VIRTUE_CHANCE,-1);
        return;
    }
    else
    {
        if (_do_device(spell->kind.tval, spell->kind.sval, SPELL_CAST)) 
            _use_charge(spell);
        else
            energy_use = 0;
    }
}

/* Absorb Magic */
static bool gain_magic(void)
{
    int item;
    int pval;
    object_type *o_ptr;
    int idx;
    char o_name[MAX_NLEN];

    item_tester_hook = item_tester_hook_recharge;
    if (!get_item(&item, "Gain power of which item? ", "You have nothing to gain power from.", (USE_INVEN | USE_FLOOR))) 
        return (FALSE);

    if (item >= 0)
        o_ptr = &inventory[item];
    else
        o_ptr = &o_list[0 - item];

    if (!object_is_known(o_ptr))
    {
        msg_print("You need to identify before absorbing.");
        return FALSE;
    }
    if (o_ptr->timeout)
    {
        msg_print("This item is still charging.");
        return FALSE;
    }

    idx = _find_idx(o_ptr->tval, o_ptr->sval);
    if (idx < 0) /* Bug? Tables need to be updated for every new device ... */
    {
        msg_print("You can't eat that!");
        return FALSE;
    }

    pval = o_ptr->pval;
    if (o_ptr->tval == TV_ROD)
    {
        p_ptr->magic_num2[idx] += o_ptr->number;
        if (p_ptr->magic_num2[idx] > 99) p_ptr->magic_num2[idx] = 99;
    }
    else
    {
        int num;
        for (num = o_ptr->number; num; num--)
        {
            int gain_num = pval;

            if (o_ptr->tval == TV_WAND) 
                gain_num = (pval + num - 1) / num;
            
            if (p_ptr->magic_num2[idx])
                gain_num = (gain_num + randint0(2))/2;

            p_ptr->magic_num2[idx] += gain_num;
            if (p_ptr->magic_num2[idx] > 99) p_ptr->magic_num2[idx] = 99;
            
            p_ptr->magic_num1[idx] += pval * _EATER_CHARGE;
            if (p_ptr->magic_num1[idx] > 99 * _EATER_CHARGE) 
                p_ptr->magic_num1[idx] = 99 * _EATER_CHARGE;
            if (p_ptr->magic_num1[idx] > p_ptr->magic_num2[idx] * _EATER_CHARGE) 
                p_ptr->magic_num1[idx] = p_ptr->magic_num2[idx] * _EATER_CHARGE;

            if (o_ptr->tval == TV_WAND) 
                pval -= (pval + num - 1) / num;
        }
    }

    object_desc(o_name, o_ptr, 0);
    msg_format("You absorb magic of %s.", o_name);

    /* Eliminate the item (from the pack) */
    if (item >= 0)
    {
        inven_item_increase(item, -999);
        inven_item_describe(item);
        inven_item_optimize(item);
    }
    /* Eliminate the item (from the floor) */
    else
    {
        floor_item_increase(0 - item, -999);
        floor_item_describe(0 - item);
        floor_item_optimize(0 - item);
    }
    return TRUE;
}

static void _absorb_magic_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Absorb Magic");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_CAST:
        var_set_bool(res, gain_magic());
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void magic_eater_gain(void) 
{ 
    if (cast_spell(_absorb_magic_spell))
        energy_use = 100;
}

/* Regeneration */
static int _regen_pct = 0;
static void _do_regen(_spell_ptr s)
{
    s32b amt;
    int  k_idx, lvl;

    if (!p_ptr->magic_num2[s->idx]) return;
    if (p_ptr->magic_num1[s->idx] == ((long)p_ptr->magic_num2[s->idx] *_EATER_CHARGE)) return;
    
    amt = ((long)p_ptr->magic_num2[s->idx]+adj_mag_mana[p_ptr->stat_ind[A_INT]]+13) * _regen_pct / 8;

    /* Hack: Low level devices recharge more quickly */
    k_idx = _lookup_kind(&s->kind);
    lvl = k_info[k_idx].level;
    if (lvl < 50)
        amt = amt * (100 - lvl) / 50;
    
    p_ptr->magic_num1[s->idx] += amt;
    if (p_ptr->magic_num1[s->idx] >= ((long)p_ptr->magic_num2[s->idx] *_EATER_CHARGE))
    {
        if (disturb_minor)
            msg_format("Regenerated %s (%d rnds/chg)", _kind_name(&s->kind), _EATER_CHARGE/amt);
        p_ptr->magic_num1[s->idx] = ((long)p_ptr->magic_num2[s->idx] *_EATER_CHARGE);
    }
}
static void _do_regen_rod(_spell_ptr s)
{
    int amt;
    if (!p_ptr->magic_num1[s->idx]) return;
    if (!p_ptr->magic_num2[s->idx]) return;
    
    amt = (long)(p_ptr->magic_num2[s->idx] * (adj_mag_mana[p_ptr->stat_ind[A_INT]] + 10)) * _EATER_ROD_CHARGE/16;
    p_ptr->magic_num1[s->idx] -= amt;
    if (p_ptr->magic_num1[s->idx] <= 0) 
    {
        if (disturb_minor)
        {
            int k_idx = _lookup_kind(&s->kind);
            int tot = k_info[k_idx].pval * _EATER_ROD_CHARGE;
            msg_format("Regenerated %s (%d rnds/chg)", _kind_name(&s->kind), tot/amt);
        }
        p_ptr->magic_num1[s->idx] = 0;
    }
}
bool magic_eater_regen(int pct)
{
    if (p_ptr->pclass != CLASS_MAGIC_EATER) return FALSE;
    _regen_pct = pct;
    _for_each(TV_STAFF, _do_regen);
    _for_each(TV_WAND, _do_regen);
    _for_each(TV_ROD, _do_regen_rod);
    return TRUE;
}

static void _do_restore(_spell_ptr s)
{
    p_ptr->magic_num1[s->idx] += 
        (p_ptr->magic_num2[s->idx] < 10) ? 
            _EATER_CHARGE * 3 : 
            p_ptr->magic_num2[s->idx]*_EATER_CHARGE/3;
    if (p_ptr->magic_num1[s->idx] > p_ptr->magic_num2[s->idx]*_EATER_CHARGE) 
        p_ptr->magic_num1[s->idx] = p_ptr->magic_num2[s->idx]*_EATER_CHARGE;
}
static void _do_restore_rod(_spell_ptr s)
{
    if (p_ptr->magic_num2[s->idx])
    {
        int k_idx = _lookup_kind(&s->kind);
        int amt;

        if (p_ptr->magic_num2[s->idx] < 10)
            amt = _EATER_ROD_CHARGE*3;
        else
            amt = p_ptr->magic_num2[s->idx]*_EATER_ROD_CHARGE/3;

        amt *= k_info[k_idx].pval; 
        p_ptr->magic_num1[s->idx] -= amt;
        if (p_ptr->magic_num1[s->idx] < 0) 
            p_ptr->magic_num1[s->idx] = 0;
    }
}
void magic_eater_restore(void)
{
    if (p_ptr->pclass == CLASS_MAGIC_EATER)
    {
        _for_each(TV_STAFF, _do_restore);
        _for_each(TV_WAND, _do_restore);
        _for_each(TV_ROD, _do_restore_rod);
    }
    p_ptr->window |= PW_PLAYER;
}

static void _do_restore_all(_spell_ptr s)
{
    p_ptr->magic_num1[s->idx] = p_ptr->magic_num2[s->idx]*_EATER_CHARGE;
}
static void _do_restore_all_rod(_spell_ptr s)
{
    p_ptr->magic_num1[s->idx] = 0;
}
void magic_eater_restore_all(void)
{
    if (p_ptr->pclass == CLASS_MAGIC_EATER)
    {
        _for_each(TV_STAFF, _do_restore_all);
        _for_each(TV_WAND, _do_restore_all);
        _for_each(TV_ROD, _do_restore_all_rod);
    }
}

/* Old annoyance of Magic Eaters: No automatic resting to regenerate charges! 
   See dungeon.c:process_player() */
static bool _can_regen_hack = FALSE;
static void _can_regen(_spell_ptr s) 
{
    if (!_can_regen_hack)
    {
        int tot = _calc_charges_total(s);
        if (tot && _calc_charges(s) < tot)
            _can_regen_hack = TRUE;
    }
}
bool magic_eater_can_regen(void)
{
    if (p_ptr->pclass != CLASS_MAGIC_EATER) return FALSE;
    _can_regen_hack = FALSE;
    _for_each(TV_STAFF, _can_regen);
    if (!_can_regen_hack)
        _for_each(TV_WAND, _can_regen);
    if (!_can_regen_hack)
        _for_each(TV_ROD, _can_regen);
    return _can_regen_hack;
}

/* Character Dump */
static void _dump(FILE* fff, int tval, cptr title)
{
    _group_ptr gs = _which_groups(tval);
    int i, j;
    fprintf(fff, "\n\n  [%s]\n", title);
    for (i = 0; ; i++)
    {
        _group_ptr g = gs + i;
        int ct = 0;
        if (!g->name) break;
        for (j = 0; ;j++)
        {
            _spell_ptr s = g->spells + j;
            if (s->idx < 0) break;
            if (_calc_charges_total(s) /*&& !_suppress(s)*/)
            {
                if (!ct)
                {
                    fprintf(fff, "\n%-22.22s Chg Tot Fail Info\n", g->name);
                    fprintf(fff, "---------------------- --- --- ---- ----------------\n");
                }
                fprintf(fff, "%-22.22s %3d %3d %3d%% %s\n", 
                    _kind_name(&s->kind), 
                    _calc_charges(s), 
                    _calc_charges_total(s), 
                    _calc_fail_rate(s),
                    _do_device(s->kind.tval, s->kind.sval, SPELL_INFO)
                );
                ct++;
            }
        }
    }
}

static void _character_dump(FILE* fff)
{
    fprintf(fff, "\n\n==================================== Magic ====================================\n");
    _dump(fff, TV_STAFF, "Staves");
    _dump(fff, TV_WAND, "Wands");
    _dump(fff, TV_ROD, "Rods");
}

/* Class Info */
static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 1;
    spell->cost = 0;
    spell->fail = 0;
    spell->fn = _absorb_magic_spell;

    return ct;
}

class_t *magic_eater_get_class_t(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    /* static info never changes */
    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  42,  36,   2,  20,  16,  48,  35 };
    skills_t xs = {  7,  16,  10,   0,   0,   0,  13,  11 };

        me.name = "Magic-Eater";
        me.desc = "Magic-Eaters can absorb magic devices, and use these magics as "
                    "their spells.  They are middling-poor at fighting.  A "
                    "Magic-Eater's prime statistic is intelligence.\n \n"
                    "Magic-Eaters can absorb the energy of wands, staffs, and rods, and "
                    "can then use these magics as if they were carrying all of these "
                    "absorbed devices.  Mana and changes of absorbed devices are "
                    "regenerated naturally by a Magic-Eater's power, and speed of "
                    "regeneration is influenced by their intelligence.  They have a "
                    "class power - 'Absorb Magic' - which is used to absorb magic "
                    "devices.\n \n"
                    "Note: Magic-Eaters may now use the resting commands (R* or R&) "
                    "to automatically regenerate all charges the same way that other "
                    "spellcasters can use these commands to regenerate all mana.";
    
        me.stats[A_STR] = -1;
        me.stats[A_INT] =  2;
        me.stats[A_WIS] =  1;
        me.stats[A_DEX] =  2;
        me.stats[A_CON] = -2;
        me.stats[A_CHR] = -2;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 103;
        me.base_hp = 6;
        me.exp = 130;
        me.pets = 30;

        me.get_powers = _get_powers;
        me.character_dump = _character_dump;
        init = TRUE;
    }

    return &me;
}
