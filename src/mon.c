#include "angband.h"

#include "mon.h"
#include <assert.h>

/************************************************************************
 * Source of a projection, effect, damage or summons
 ************************************************************************/
who_t who_create_null(void)
{
    who_t who = {0};
    who.tag = WHO_NULL;
    return who;
}
who_t who_create_plr(void)
{
    who_t who = {0};
    who.tag = WHO_PLR;
    return who;
}
who_t who_create_mon(mon_ptr mon)
{
    who_t who = {0};
    who.tag = WHO_MON;
    who.v.mon = mon;
    return who;
}
who_t who_create_trap(point_t pos)
{
    who_t who = {0};
    who.tag = WHO_TRAP;
    who.v.pos = pos;
    return who;
}
who_t who_create_mirror(point_t pos)
{
    who_t who = {0};
    who.tag = WHO_MIRROR;
    who.v.pos = pos;
    return who;
}
who_t who_create_unctrl_power(void)
{
    who_t who = {0};
    who.tag = WHO_UNCTRL_POWER;
    return who;
}
who_t who_create_pos(point_t pos)
{
    who_t who = {0};
    who.tag = WHO_POS;
    who.v.pos = pos;
    return who;
}

bool who_is_null(who_t who) { return who.tag == WHO_NULL; }
bool who_is_plr(who_t who) { return who.tag == WHO_PLR; }
bool who_is_mon(who_t who) { return who.tag == WHO_MON; }
bool who_is_trap(who_t who) { return who.tag == WHO_TRAP; }
bool who_is_mirror(who_t who) { return who.tag == WHO_MIRROR; }
bool who_is_unctrl_power(who_t who) { return who.tag == WHO_UNCTRL_POWER; }
bool who_is_pos(who_t who) { return who.tag == WHO_POS; }

mon_ptr who_mon(who_t who)
{
    if (!who_is_mon(who)) return NULL;
    return who.v.mon;
}
bool who_is_mon_id(who_t who, u32b id)
{
    if (!who_is_mon(who)) return FALSE;
    return who.v.mon->id == id;
}
bool who_is_pet(who_t who)
{
    mon_ptr mon = who_mon(who);
    if (!mon) return FALSE;
    return mon_is_pet(mon);
}
point_t who_pos(who_t who)
{
    switch (who.tag)
    {
    case WHO_PLR: return plr->pos;
    case WHO_MON: return who.v.mon->pos;
    case WHO_TRAP: return who.v.pos;
    case WHO_MIRROR: return who.v.pos;
    case WHO_POS: return who.v.pos;
    }
    return point_create(-1, -1);
}
bool who_equals(who_t who, who_t what)
{
    if (who.tag != what.tag) return FALSE;
    switch (who.tag)
    {
    case WHO_MON: return who.v.mon == what.v.mon;
    case WHO_TRAP:
    case WHO_MIRROR:
    case WHO_POS: return point_equals(who.v.pos, what.v.pos);
    }
    return TRUE;
}

/************************************************************************
 * MON() rules for quests, rooms and friends
 ************************************************************************/
mon_rule_ptr mon_rule_alloc(void)
{
    mon_rule_ptr rule = malloc(sizeof(mon_rule_t));
    memset(rule, 0, sizeof(mon_rule_t));
    return rule;
}
void mon_rule_free(mon_rule_ptr rule)
{
    mon_rule_ptr r = rule, n;
    while (r)
    {
        n = r->next;
        free(r);
        r = n;
    }
}
/* Parse an M: line in r_info:
 *   v--- buf
 * M:50%:1d4+1:MON(...) */
errr mon_rule_parse(mon_rule_ptr rule, char *buf)
{
    char *tokens[10];
    int   token_ct = z_string_split(buf, tokens, 10, ":");
    int   i;
    bool  did_mon = FALSE;

    for (i = 0; i < token_ct; i++)
    {
        char *token = tokens[i];

        /* the last token should be a MON() directive */
        if (prefix(token, "MON("))
        {
            char *name;
            char *args[10];
            int   arg_ct = parse_args(token, &name, args, 10);
            errr  rc;


            if (arg_ct < 0)
            {
                msg_format("Error: Malformed argument %s. Missing )?", name);
                return PARSE_ERROR_GENERIC;
            }

            assert(strcmp(name, "MON") == 0);
            if (i != token_ct - 1) return PARSE_ERROR_TOO_FEW_ARGUMENTS;

            rc = mon_rule_parse_aux(rule, args, arg_ct);
            if (rc) return rc;
            did_mon = TRUE; /* MON() directive is required */
        }
        /* the initial tokens are both optional, and can be specified in 
         * any order. we are looking for a pct and some dice. */
        else
        {
            char arg[100], sentinel = '~', check;
            int  dd, ds, base, pct;

            sprintf(arg, "%s%c", token, sentinel);

            if (2 == sscanf(arg, "%d%%%c", &pct, &check) && check == sentinel)
            {
                rule->pct = MAX(0, MIN(100, pct));
            }
            else if (4 == sscanf(arg, "%dd%d+%d%c", &dd, &ds, &base, &check) && check == sentinel)
            {
                rule->amt.dd = MAX(0, dd);
                rule->amt.ds = MAX(0, ds);
                rule->amt.base = base;
            }
            else if (3 == sscanf(arg, "%dd%d%c", &dd, &ds, &check) && check == sentinel)
            {
                rule->amt.dd = MAX(0, dd);
                rule->amt.ds = MAX(0, ds);
                rule->amt.base = 0;
            }
            else if (2 == sscanf(arg, "%d%c", &base, &check) && check == sentinel)
            {
                rule->amt.dd = 0;
                rule->amt.ds = 0;
                rule->amt.base = base;
            }
            else return PARSE_ERROR_UNDEFINED_DIRECTIVE;
        }
    }
    if (!did_mon)
    {
        msg_print("Missing MON() directive.");
        return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    }
    return ERROR_SUCCESS;
}
/* Parse the arguments of a single MON() directive:
 * e.g. args = {"DRAGON", "DEPTH+20 | HASTE"} */
static bool _is_d_char(const char *token)
{
    if (strlen(token) != 1) return FALSE;
    return mon_race_is_d_char(token[0]);
}
errr mon_rule_parse_aux(mon_rule_ptr rule, char **args, int arg_ct)
{
    if (arg_ct < 1 || arg_ct > 2)
    {
        msg_print("Invalid MON() directive: Syntax: MON(<which> [,<options>]).");
        return PARSE_ERROR_TOO_FEW_ARGUMENTS;
    }

    rule->flags = 0;
    rule->which = 0;
    rule->lvl_boost = 0;

    /* Which monster? Can be random, by index, by display character, or by summoning type.*/
    if (streq(args[0], "*"))
    {
        rule->flags |= MON_RULE_RANDOM;
    }
    else if (is_numeric(args[0]))
    {
        msg_print("Numeric monster index is not supported!");
        return PARSE_ERROR_GENERIC;
    }
    else if (_is_d_char(args[0]))
    {
        rule->flags |= MON_RULE_CHAR;
        rule->which = args[0][0];
    }
    else
    {
        parse_tbl_ptr p = summon_type_parse(args[0]);
        if (p)
        {
            rule->flags |= MON_RULE_TYPE;
            rule->which = p->id;
        }
        else
        {
            rule->which = sym_add(args[0]); /* _verify_mon_race */
        }
    }

    /* Options */
    if (arg_ct >= 2)
    {
        char *flags[10];
        int   flag_ct = z_string_split(args[1], flags, 10, "|");
        int   i, n;

        for (i = 0; i < flag_ct; i++)
        {
            char* flag = flags[i];
            if (streq(flag, "NO_GROUP"))
                rule->flags |= MON_RULE_NO_GROUP;
            else if (streq(flag, "NO_SLEEP"))
                rule->flags |= MON_RULE_NO_SLEEP;
            else if (streq(flag, "UNIQUE"))
                rule->flags |= MON_RULE_UNIQUE;
            else if (streq(flag, "FRIENDLY"))
                rule->flags |= MON_RULE_FRIENDLY;
            else if (streq(flag, "HASTE"))
                rule->flags |= MON_RULE_HASTE;
            else if (streq(flag, "STOP"))
                rule->flags |= MON_RULE_STOP;
            else if (streq(flag, "SAME"))
                rule->flags |= MON_RULE_SAME;
            else if (streq(flag, "ANCESTOR"))
            {
                if (rule->flags & (MON_RULE_CHAR | MON_RULE_TYPE | MON_RULE_RANDOM))
                {
                    msg_format("Ancestors require a specific mon_race->id");
                    return PARSE_ERROR_GENERIC;
                }
                rule->flags |= MON_RULE_ANCESTOR;
            }
            else if (sscanf(flag, "DEPTH+%d", &n) == 1)
                rule->lvl_boost = n;
            else if (sscanf(flag, "%d%%", &n) == 1)
                rule->pct = n;
            else
            {
                msg_format("Error: Invalid monster option %s.", flag);
                return PARSE_ERROR_GENERIC;
            }
        }
    }
    return ERROR_SUCCESS;
}

static mon_rule_ptr _rule;
static bool _rule_p(mon_race_ptr race)
{
    assert(_rule);
    return mon_rule_filter(_rule, race);
}
bool mon_rule_filter(mon_rule_ptr rule, mon_race_ptr race)
{
    if (rule->flags & MON_RULE_GOOD)
    {
        if (race->align < 0)
            return FALSE;
    }
    if (rule->flags & MON_RULE_EVIL)
    {
        if (race->align > 0)
            return FALSE;
    }
    if (!(rule->flags & MON_RULE_UNIQUE))
    {
        if (mon_race_is_unique(race))
            return FALSE;
    }

    if (rule->flags & MON_RULE_TYPE)
    {
        if (!mon_is_type(race, rule->which))
            return FALSE;
    }
    else if (rule->flags & MON_RULE_CHAR)
    {
        if (!mon_race_is_char(race, rule->which))
            return FALSE;
    }
    else if (rule->flags & MON_RULE_RANDOM)
    {
    }
    else if (rule->flags & MON_RULE_ANCESTOR)
    {
        mon_race_ptr me = mon_race_lookup(rule->which);
        if (!mon_race_can_evolve(race, me))
            return FALSE;
    }
    return TRUE;
}

int mon_rule_amt(mon_rule_ptr rule)
{
    int ct = 0;

    if (rule->pct && _1d(100) > rule->pct)
        return ct;

    ct = dice_roll(rule->amt);
    if (!ct) /* no dice specified => 1 monster */
        ct = 1;
    return ct;
}
u32b mon_rule_mode(mon_rule_ptr rule)
{
    u32b mode = 0;
    if (!(rule->flags & MON_RULE_NO_SLEEP))
        mode |= PM_ALLOW_SLEEP;
    if (!(rule->flags & MON_RULE_NO_GROUP))
        mode |= PM_ALLOW_GROUP;
    if (rule->flags & MON_RULE_UNIQUE)
        mode |= PM_ALLOW_UNIQUE;
    if (rule->flags & MON_RULE_FRIENDLY)
        mode |= PM_FORCE_FRIENDLY;
    if (rule->flags & MON_RULE_HASTE)
        mode |= PM_HASTE;
    return mode;
}
static bool _is_char(int which)
{
    if (which < 32) return FALSE;
    if (which > 128) return FALSE;
    return which;
}
/* helper to see if room_grid_s.monster has been specified */
bool mon_rule_is_valid(mon_rule_ptr rule)
{
    if (rule->flags & (MON_RULE_TYPE | MON_RULE_RANDOM))
        return TRUE;
    if (rule->flags & MON_RULE_CHAR)
        return _is_char(rule->which) && mon_race_is_d_char(rule->which);
    return rule->which != 0;
}
mon_race_ptr mon_rule_race(mon_rule_ptr rule)
{
    mon_race_ptr race = NULL;
    if (rule->flags & (MON_RULE_TYPE | MON_RULE_RANDOM | MON_RULE_CHAR | MON_RULE_ANCESTOR))
    {
        u32b options = 0;
        int lvl = cave->difficulty + rule->lvl_boost;

        if (!(rule->flags & MON_RULE_UNIQUE))
            options |= GMN_NO_UNIQUES;

        if (rule->flags & MON_RULE_VAULT)
            options |= GMN_POWER_BOOST;

        _rule = rule;
        mon_alloc_push_filter(_rule_p);
        race = mon_alloc_choose_aux2(mon_alloc_current_tbl(), lvl, rule->min_lvl, options);
        mon_alloc_pop_filter();
        _rule = NULL;
    }
    else if (rule->which)
    {
        race = mon_race_lookup(rule->which);
    }
    return race;
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
    assert(!mon->pack); /* cf _mon_free */
    mon_tim_clear(mon);
    if (mon->flow) dun_flow_free(mon->flow);
    free(mon);
}

mon_ptr mon_parent(mon_ptr mon)
{
    if (mon->parent_id)
        return dun_mon(mon->dun, mon->parent_id); /* XXX parent off level? cf dun_mon_ex XXX */
    return NULL;
}

int mon_lvl(mon_ptr mon) { return mon_race_lvl(mon->race); }
int mon_race_lvl(mon_race_ptr race) { return race->alloc.lvl; }
int mon_ac(mon_ptr mon)
{
    int ac = mon->race->ac;

    ac += mon->ac_adj;
    if (ac < 0) ac = 0;
    /* XXX timed buffs */
    return ac;
}

int mon_skill_thn(mon_ptr mon)
{
    /* XXX we could scale for T_BERSERK and T_STUN here, but these
     * scalings apply to *both* accuracy and damage. cf _scale in mon_attack.c
     * where the logic is not duplicated ... */
    return mon_race_skill_thn(mon->race);
}

int mon_race_skill_thn(mon_race_ptr race)
{
    /* XXX Historically: return 3 * mon_race_lvl(race); */
    return race->body.skills.thn
         + mon_race_lvl(race) * race->body.extra_skills.thn / 50;
}

int mon_dis(mon_ptr mon)
{
    return mon->cdis + mon->dun->plr_dis;
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
    if (!mon_is_valid(mon)) return FALSE; /* dead or deleted */
    if (mon->dun->id != plr->dun_id) return FALSE;
    if (!mon->ml) return FALSE;
    if (plr_tim_find(T_BLIND)) return FALSE;
    if (!ignore_unview) return TRUE;
    return plr_can_see(mon->pos);
}

mon_race_ptr mon_true_race(mon_ptr mon)
{
    assert(mon);
    if (mon->mflag2 & MFLAG2_CHAMELEON)
    {
        if (mon_is_unique(mon)) /* Chameleon lord only uses unique forms */
            return mon_race_parse("R.Chameleon");
        else                           /* other chameleons never use unique forms */
            return mon_race_parse("R.chameleon");
    }
    return mon->race;
}

void mon_set_hunted(mon_ptr mon)
{
    #if 0
    if (mon->id == plr->riding) /* XXX Hunt the plr instead (via dun->flow) */
    {
        mon->mflag2 &= ~MFLAG2_HUNTED;
        if (mon->flow)
        {
            dun_flow_free(mon->flow);
            mon->flow = NULL;
        }
        return;
    }
    #endif
    mon->mflag2 |= MFLAG2_HUNTED;
    if (!mon->flow) mon->flow = dun_flow_calc(mon->dun, mon->pos, MON_HUNT_RAD, NULL);
    else dun_flow_recalc(mon->flow, mon->pos);
}
void mon_drop_carried_obj(mon_ptr mon)
{
    dun_mon_drop_carried_obj(mon->dun, mon);
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

/************************************************************************
 * Lore
 ************************************************************************/
static void u16b_inc(u16b_ptr ct)
{
    if (*ct < USHRT_MAX)
        (*ct)++; /* parentheses required! */
}
bool mon_lore_allow(mon_ptr mon)
{
    if (!mon->ml) return FALSE;
    if ((mon->mflag2 & MFLAG2_FUZZY) && !(mon->mflag2 & MFLAG2_LORE)) return FALSE;
    if (mon->race != mon->apparent_race) return FALSE;
    if (plr_tim_find(T_HALLUCINATE)) return FALSE;
    return TRUE;
}
void mon_lore_move(mon_ptr mon, u32b mask)
{
    if (mon_lore_allow(mon))
        mon_race_lore_move(mon->race, mask);
}
void mon_race_lore_move(mon_race_ptr race, u32b mask)
{
    u16b old = race->lore.move;

    race->lore.move |= (race->move.flags & mask);
    if (race->lore.move != old && race->id == plr->monster_race_idx)
        plr->window |= PW_MONSTER;
}
void mon_lore_kind(mon_ptr mon, u32b mask)
{
    if (mon_lore_allow(mon))
        mon_race_lore_kind(mon->race, mask);
}
void mon_race_lore_kind(mon_race_ptr race, u32b mask)
{
    u32b old = race->lore.kind;
    race->lore.kind |= (race->kind & mask);
    if (race->lore.kind != old && race->id == plr->monster_race_idx)
        plr->window |= PW_MONSTER;
}
void mon_lore_abilities(mon_ptr mon, u32b mask)
{
    if (mon_lore_allow(mon))
        mon_race_lore_abilities(mon->race, mask);
}
void mon_race_lore_abilities(mon_race_ptr race, u32b mask)
{
    u32b old = race->lore.abilities;
    race->lore.abilities |= (race->abilities & mask);
    if (race->lore.abilities != old && race->id == plr->monster_race_idx)
        plr->window |= PW_MONSTER;
}
void mon_lore_attributes(mon_ptr mon, u32b mask)
{
    if (mon_lore_allow(mon))
        mon_race_lore_attributes(mon->race, mask);
}
void mon_race_lore_attributes(mon_race_ptr race, u32b mask)
{
    u32b old = race->lore.attributes;
    race->lore.attributes |= (race->attributes & mask);
    if (race->lore.attributes != old && race->id == plr->monster_race_idx)
        plr->window |= PW_MONSTER;
}
void mon_lore_resist(mon_ptr mon, int gf)
{
    if (mon_lore_allow(mon))
        mon_race_lore_resist(mon->race, gf);
}
void mon_race_lore_resist(mon_race_ptr race, int gf)
{
    u32b mask;
    assert(GF_RES_MIN <= gf && gf <= GF_RES_MAX);
    assert(GF_RES_MAX < 32);
    mask = (1U << gf);
    if (race->lore.resist & mask) return;
    if ((race->resist & mask) || (race->immune & mask) || (race->vuln & mask))
    {
        race->lore.resist |= mask;
        if (race->id == plr->monster_race_idx)
            plr->window |= PW_MONSTER;
    }
}
void mon_lore_align(mon_ptr mon)
{
    if (mon_lore_allow(mon))
        mon_race_lore_align(mon->race);
}
void mon_race_lore_align(mon_race_ptr race)
{
    if (race->lore.flags & RFL_ALIGN) return;
    race->lore.flags |= RFL_ALIGN;
    if (race->id == plr->monster_race_idx)
        plr->window |= PW_MONSTER;
}
void mon_lore_sighting(mon_ptr mon)
{
    if (mon_lore_allow(mon))
        mon_race_lore_sighting(mon->race);
}
void mon_race_lore_sighting(mon_race_ptr race)
{
    if (!race->lore.sightings) /* initial sighting */
    {
        mon_race_lore_attributes(race, RF_MALE|RF_FEMALE|RF_RIDING);
        mon_race_lore_move(race, RFM_FLY); /* XXX */
        mon_race_lore_kind(race, RFK_ORC|RFK_TROLL|RFK_GIANT|RFK_DRAGON|RFK_HUMAN|RFK_AQUATIC);
    }
    u16b_inc(&race->lore.sightings);
}
void mon_lore_death(mon_ptr mon)
{
    /* XXX N.shadower no longer lores */
    if (mon_lore_allow(mon) || mon_is_fixed_unique(mon) || statistics_hack)
    {
        if (!mon->race->lore.kills.total) /* XXX this is historical */
        {
            mon_race_lore_kind(mon->race, RFK_UNDEAD|RFK_DEMON|RFK_ANIMAL|RFK_AMBERITE|RFK_OLYMPIAN|RFK_NAZGUL);
        }
        u16b_inc(&mon->race->lore.kills.total);
        u16b_inc(&mon->race->lore.kills.current);
    }
}

static void mon_race_lore_turn(mon_race_ptr race)
{
    if (race->lore.turns.total < UINT_MAX)
    {
        race->lore.turns.total++;
        if (race->id == plr->monster_race_idx)
            plr->window |= PW_MONSTER;
    }
}

static void mon_race_lore_spell_turn(mon_race_ptr race)
{
    if (race->lore.turns.spell < UINT_MAX)
    {
        race->lore.turns.spell++;
        if (race->id == plr->monster_race_idx)
            plr->window |= PW_MONSTER;
    }
    mon_race_lore_turn(race);
}

void mon_lore_turn(mon_ptr mon)
{
    if (mon_lore_allow(mon))
        mon_race_lore_turn(mon->race);
}

void mon_lore_spell(mon_ptr mon, mon_spell_ptr spell)
{
    if (mon_lore_allow(mon))
        mon_race_lore_spell(mon->race, spell);
}

void mon_race_lore_spell(mon_race_ptr race, mon_spell_ptr spell)
{
    if (spell->lore < MAX_SHORT)
    {
        spell->lore++;
        if (race->id == plr->monster_race_idx)
            plr->window |= PW_MONSTER;
    }
    mon_race_lore_spell_turn(race);
}

void mon_lore_spell_failure(mon_ptr mon)
{
    if (mon_lore_allow(mon))
        mon_race_lore_spell_turn(mon->race);
}

void mon_lore_wake(mon_ptr mon)
{
    if (mon_lore_allow(mon) && mon->race->lore.turns.wake < USHRT_MAX)
    {
        mon->race->lore.turns.wake++;
        if (mon->race->id == plr->monster_race_idx)
            plr->window |= PW_MONSTER;
    }
}

void mon_lore_sleep(mon_ptr mon)
{
    if (mon_lore_allow(mon) && mon->race->lore.turns.sleep < USHRT_MAX)
    {
        mon->race->lore.turns.sleep++;
        if (mon->race->id == plr->monster_race_idx)
            plr->window |= PW_MONSTER;
    }
}

void mon_lore_aura(mon_ptr mon, mon_aura_ptr aura)
{
    if (mon_lore_allow(mon))
        mon_race_lore_aura(mon->race, aura);
}
void mon_race_lore_aura(mon_race_ptr race, mon_aura_ptr aura)
{
    if (aura->lore < USHRT_MAX)
    {
        aura->lore++;
        if (race->id == plr->monster_race_idx)
            plr->window |= PW_MONSTER;
    }
}

/************************************************************************
 * Resistance
 ************************************************************************/
bool mon_resist(mon_ptr mon, int gf) { return mon_race_resist(mon->race, gf); }
bool mon_race_resist(mon_race_ptr race, int gf)
{
    assert(GF_RES_MIN <= gf && gf <= GF_RES_MAX);
    assert(GF_RES_MAX < 32);
    return BOOL(race->resist & (1U << gf));
}
bool mon_immune(mon_ptr mon, int gf) { return mon_race_immune(mon->race, gf); }
bool mon_race_immune(mon_race_ptr race, int gf)
{
    assert(GF_RES_MIN <= gf && gf <= GF_RES_MAX);
    assert(GF_RES_MAX < 32);
    return BOOL(race->immune & (1U << gf));
}
bool mon_vuln(mon_ptr mon, int gf) { return mon_race_vuln(mon->race, gf); }
bool mon_race_vuln(mon_race_ptr race, int gf)
{
    assert(GF_RES_MIN <= gf && gf <= GF_RES_MAX);
    assert(GF_RES_MAX < 32);
    return BOOL(race->vuln & (1U << gf));
}
int mon_res_pct(mon_ptr mon, int gf) { return mon_race_res_pct(mon->race, gf); }
int mon_race_res_pct(mon_race_ptr race, int gf)
{
    gf_info_ptr gfi;
    assert(GF_RES_MIN <= gf && gf <= GF_RES_MAX);
    assert(GF_RES_MAX < 32);
    gfi = gf_lookup(gf);
    assert(gfi);
    if (!gfi) return 0;

    /* at most 1 of the following should apply ... but don't insist. For example,
     * see e.undead -> e.beholder. The undead beholder inherits GF_POIS but adds
     * IM_POIS. Forcing RESIST(-POISON) would be un-kind ... */
    if (race->immune & (1U << gf)) return 100;
    if (race->vuln & (1U << gf))
    {
        if (gfi->flags & GFF_RESIST_HI) return -50;
        return -100;
    }
    if (race->resist & (1U << gf))
        return res_pct_mon(gf); /* XXX keep plr and monster systems in sync */
    return 0;
}
int mon_res_calc_dam(mon_ptr mon, int gf, int dam)
{
    int pct = mon_res_pct(mon, gf);
    if (!pct) return dam;
    mon_lore_resist(mon, gf); /* XXX simplify lore tracking XXX */
    dam -= dam * pct / 100;
    if (dam < 0) dam = 0;
    return dam;
}

/************************************************************************
 * Motion
 ************************************************************************/
int mon_move_range(mon_ptr mon)
{
    int r = mon_race_move_range(mon->race);
    if (mon_is_pet(mon) && r > MAX_SIGHT)
        r = MAX_SIGHT;
    return r;
}
int mon_race_move_range(mon_race_ptr race)
{
    int r = race->move.range;
    if (plr_on_surface())
        r *= 5;
    return r;
}

bool mon_never_move(mon_ptr mon) { return mon_race_never_move(mon->race); }
bool mon_race_never_move(mon_race_ptr race) { return BOOL(race->move.flags & RFM_NEVER); }
void mon_lore_never_move(mon_ptr mon) { mon_lore_move(mon, RFM_NEVER); }

bool mon_can_open_door(mon_ptr mon)
{
    if (!mon_race_can_open_door(mon->race)) return FALSE;
    if (mon->mflag2 & MFLAG2_ILLUSION) return FALSE; /* illusions are non-physical */
    if (mon_is_pet(mon) && !(plr->pet_extra_flags & PF_OPEN_DOORS)) return FALSE;
    return TRUE;
}
bool mon_race_can_open_door(mon_race_ptr race) { return BOOL(race->move.flags & RFM_OPEN); }
void mon_lore_open_door(mon_ptr mon) { mon_lore_move(mon, RFM_OPEN); }

bool mon_can_bash_door(mon_ptr mon)
{
    if (!mon_race_can_bash_door(mon->race)) return FALSE;
    if (mon->mflag2 & MFLAG2_ILLUSION) return FALSE; /* illusions are non-physical */
    if (mon_is_pet(mon) && !(plr->pet_extra_flags & PF_OPEN_DOORS)) return FALSE;
    return TRUE;
}
bool mon_race_can_bash_door(mon_race_ptr race) { return BOOL(race->move.flags & RFM_BASH); }
void mon_lore_bash_door(mon_ptr mon) { mon_lore_move(mon, RFM_BASH); }

bool mon_can_push_mon(mon_ptr mon) { return mon_race_can_push_mon(mon->race); }
bool mon_race_can_push_mon(mon_race_ptr race) { return BOOL(race->move.flags & RFM_PUSH); }
void mon_lore_push_mon(mon_ptr mon) { mon_lore_move(mon, RFM_PUSH); }

bool mon_can_trample_mon(mon_ptr mon) { return mon_race_can_trample_mon(mon->race); }
bool mon_race_can_trample_mon(mon_race_ptr race) { return BOOL(race->move.flags & RFM_TRAMPLE); }
void mon_lore_trample_mon(mon_ptr mon) { mon_lore_move(mon, RFM_TRAMPLE); }

bool mon_can_pickup_obj(mon_ptr mon)
{
    if (!mon_race_can_pickup_obj(mon->race)) return FALSE;
    if (mon->mflag2 & MFLAG2_ILLUSION) return FALSE; /* illusions are non-physical */
    if (!mon_is_pet(mon)) return TRUE;
    return BOOL(plr->pet_extra_flags & PF_PICKUP_ITEMS);
}
bool mon_race_can_pickup_obj(mon_race_ptr race) { return BOOL(race->move.flags & RFM_PICKUP); }
void mon_lore_pickup_obj(mon_ptr mon) { mon_lore_move(mon, RFM_PICKUP); }

bool mon_can_destroy_obj(mon_ptr mon)
{
    if (!mon_race_can_destroy_obj(mon->race)) return FALSE;
    if (mon->mflag2 & MFLAG2_ILLUSION) return FALSE; /* illusions are non-physical */
    if (!mon_is_pet(mon)) return TRUE;
    return FALSE;
}
bool mon_race_can_destroy_obj(mon_race_ptr race) { return BOOL(race->move.flags & RFM_DESTROY); }
void mon_lore_destroy_obj(mon_ptr mon) { mon_lore_move(mon, RFM_DESTROY); }

bool mon_can_tunnel(mon_ptr mon)
{
    bool riding = mon->id == plr->riding;

    if (mon->mflag2 & MFLAG2_ILLUSION) return FALSE; /* illusions are non-physical */
    if (mon_race_can_tunnel(mon->race) && !riding) /* XXX disagree with riding check XXX */
        return TRUE;
    return FALSE;
}
bool mon_race_can_tunnel(mon_race_ptr race) { return BOOL(race->move.flags & RFM_TUNNEL); }
void mon_lore_tunnel(mon_ptr mon) { mon_lore_move(mon, RFM_TUNNEL); }

bool mon_can_passwall(mon_ptr mon)
{
    bool riding = mon->id == plr->riding;

    if (mon->mflag2 & MFLAG2_ILLUSION) return FALSE;
    if (mon_race_can_passwall(mon->race) && (!riding || plr->pass_wall))
        return TRUE;
    return FALSE;
}
bool mon_race_can_passwall(mon_race_ptr race) { return BOOL(race->move.flags & RFM_PASSWALL); }
void mon_lore_passwall(mon_ptr mon) { mon_lore_move(mon, RFM_PASSWALL); }

extern bool mon_can_passweb(mon_ptr mon) { return mon_race_can_passweb(mon->race); }
extern bool mon_race_can_passweb(mon_race_ptr race)
    { return mon_race_can_passwall(race) || (race->move.flags & RFM_PASSWEB); }
extern void mon_lore_passweb(mon_ptr mon) { mon_lore_move(mon, RFM_PASSWEB); }

extern bool mon_can_clearweb(mon_ptr mon) { return mon_race_can_clearweb(mon->race); }
extern bool mon_race_can_clearweb(mon_race_ptr race) { return BOOL(race->move.flags & RFM_CLEARWEB); }
extern void mon_lore_clearweb(mon_ptr mon) { mon_lore_move(mon, RFM_CLEARWEB); }

bool mon_can_swim(mon_ptr mon) { return mon_race_can_swim(mon->race); }
bool mon_race_can_swim(mon_race_ptr race) { return BOOL(race->move.flags & RFM_SWIM); }
void mon_lore_swim(mon_ptr mon) { mon_lore_move(mon, RFM_SWIM); }

bool mon_can_fly(mon_ptr mon) {
    if (plr->prace == RACE_MON_RING && plr->riding == mon->id && plr->levitation)
        return TRUE;
    return mon_race_can_fly(mon->race);
}
bool mon_race_can_fly(mon_race_ptr race) { return BOOL(race->move.flags & RFM_FLY); }
void mon_lore_fly(mon_ptr mon) { mon_lore_move(mon, RFM_FLY); }

bool mon_can_climb(mon_ptr mon) { return mon_race_can_climb(mon->race); }
bool mon_race_can_climb(mon_race_ptr race) { return BOOL(race->move.flags & RFM_CLIMB); }
void mon_lore_climb(mon_ptr mon) { mon_lore_move(mon, RFM_CLIMB); }

bool mon_is_trump(mon_ptr mon) { return mon_race_is_trump(mon->race); }
bool mon_race_is_trump(mon_race_ptr race) { return BOOL(race->move.flags & RFM_TRUMP); }
void mon_lore_trump(mon_ptr mon) { mon_lore_move(mon, RFM_TRUMP); }

bool mon_move_quick(mon_ptr mon) { return mon_race_move_quick(mon->race); }
bool mon_race_move_quick(mon_race_ptr race) { return BOOL(race->move.flags & RFM_QUICK); }
void mon_lore_move_quick(mon_ptr mon) { mon_lore_move(mon, RFM_QUICK); }

/************************************************************************
 * Kind
 ************************************************************************/
bool mon_is_animal(mon_ptr mon) { return mon_race_is_animal(mon->race); }
bool mon_race_is_animal(mon_race_ptr race) { return BOOL(race->kind & RFK_ANIMAL); }
void mon_lore_animal(mon_ptr mon) { mon_lore_kind(mon, RFK_ANIMAL); }

bool mon_is_demon(mon_ptr mon) { return mon_race_is_demon(mon->race); }
bool mon_race_is_demon(mon_race_ptr race) { return BOOL(race->kind & RFK_DEMON); }
void mon_lore_demon(mon_ptr mon) { mon_lore_kind(mon, RFK_DEMON); }

bool mon_is_dragon(mon_ptr mon) { return mon_race_is_dragon(mon->race); }
bool mon_race_is_dragon(mon_race_ptr race) { return BOOL(race->kind & RFK_DRAGON); }
void mon_lore_dragon(mon_ptr mon) { mon_lore_kind(mon, RFK_DRAGON); }

bool mon_is_giant(mon_ptr mon) { return mon_race_is_giant(mon->race); }
bool mon_race_is_giant(mon_race_ptr race) { return BOOL(race->kind & RFK_GIANT); }
void mon_lore_giant(mon_ptr mon) { mon_lore_kind(mon, RFK_GIANT); }

bool mon_is_human(mon_ptr mon) { return mon_race_is_human(mon->race); }
bool mon_race_is_human(mon_race_ptr race) { return BOOL(race->kind & RFK_HUMAN); }
void mon_lore_human(mon_ptr mon) { mon_lore_kind(mon, RFK_HUMAN); }

/* Note: KIND(NONLIVING) is no longer assumed for DEMON|UNDEAD. It should be explicitly
 * specified in r_info and, if ommitted, is silently added by _verify_mon_race. Thus,
 * for OF_ESP_NONLIVING, one gets mon_lore_nonliving() which only reveals RFK_NONLIVING,
 * not RFK_DEMON | RFK_UNDEAD as used to happen. OF_SLAY_LIVING tries to mon_lore_living()
 * but, really, that is just too broad a category. I guess you should learn that a monster
 * is *not* UNDEAD|DEMON|NONLIVING, but our lore system only handles positive knowledge. */
bool mon_is_living(mon_ptr mon) { return mon_race_is_living(mon->race); }
bool mon_race_is_living(mon_race_ptr race) { return !BOOL(race->kind & RFK_NONLIVING); }
void mon_lore_living(mon_ptr mon) { /* no-op */ }

extern bool mon_is_nonliving(mon_ptr mon) { return mon_race_is_nonliving(mon->race); }
extern bool mon_race_is_nonliving(mon_race_ptr race) { return BOOL(race->kind & RFK_NONLIVING); }
extern void mon_lore_nonliving(mon_ptr mon) { mon_lore_kind(mon, RFK_NONLIVING); }

bool mon_is_orc(mon_ptr mon) { return mon_race_is_orc(mon->race); }
bool mon_race_is_orc(mon_race_ptr race) { return BOOL(race->kind & RFK_ORC); }
void mon_lore_orc(mon_ptr mon) { mon_lore_kind(mon, RFK_ORC); }

bool mon_is_troll(mon_ptr mon) { return mon_race_is_troll(mon->race); }
bool mon_race_is_troll(mon_race_ptr race) { return BOOL(race->kind & RFK_TROLL); }
void mon_lore_troll(mon_ptr mon) { mon_lore_kind(mon, RFK_TROLL); }

bool mon_is_undead(mon_ptr mon) { return mon_race_is_undead(mon->race); }
bool mon_race_is_undead(mon_race_ptr race) { return BOOL(race->kind & RFK_UNDEAD); }
void mon_lore_undead(mon_ptr mon) { mon_lore_kind(mon, RFK_UNDEAD); }

bool mon_is_elf(mon_ptr mon) { return mon_race_is_elf(mon->race); }
bool mon_race_is_elf(mon_race_ptr race) { return BOOL(race->kind & RFK_ELF); }
void mon_lore_elf(mon_ptr mon) { mon_lore_kind(mon, RFK_ELF); }

bool mon_is_dark_elf(mon_ptr mon) { return mon_race_is_dark_elf(mon->race); }
bool mon_race_is_dark_elf(mon_race_ptr race) { return BOOL(race->kind & RFK_DARK_ELF); }
void mon_lore_dark_elf(mon_ptr mon) { mon_lore_kind(mon, RFK_DARK_ELF); }

bool mon_is_hobbit(mon_ptr mon) { return mon_race_is_hobbit(mon->race); }
bool mon_race_is_hobbit(mon_race_ptr race) { return BOOL(race->kind & RFK_HOBBIT); }
void mon_lore_hobbit(mon_ptr mon) { mon_lore_kind(mon, RFK_HOBBIT); }

bool mon_is_dwarf(mon_ptr mon) { return mon_race_is_dwarf(mon->race); }
bool mon_race_is_dwarf(mon_race_ptr race) { return BOOL(race->kind & RFK_DWARF); }
void mon_lore_dwarf(mon_ptr mon) { mon_lore_kind(mon, RFK_DWARF); }

bool mon_is_amberite(mon_ptr mon) { return mon_race_is_amberite(mon->race); }
bool mon_race_is_amberite(mon_race_ptr race) { return BOOL(race->kind & RFK_AMBERITE); }
void mon_lore_amberite(mon_ptr mon) { mon_lore_kind(mon, RFK_AMBERITE); }

bool mon_is_thief(mon_ptr mon) { return mon_race_is_thief(mon->race); }
bool mon_race_is_thief(mon_race_ptr race) { return BOOL(race->kind & RFK_THIEF); }
void mon_lore_thief(mon_ptr mon) { mon_lore_kind(mon, RFK_THIEF); }

bool mon_is_knight(mon_ptr mon) { return mon_race_is_knight(mon->race); }
bool mon_race_is_knight(mon_race_ptr race) { return BOOL(race->kind & RFK_KNIGHT); }
void mon_lore_knight(mon_ptr mon) { mon_lore_kind(mon, RFK_KNIGHT); }

bool mon_is_olympian(mon_ptr mon) { return mon_race_is_olympian(mon->race); }
bool mon_race_is_olympian(mon_race_ptr race) { return BOOL(race->kind & RFK_OLYMPIAN); }
void mon_lore_olympian(mon_ptr mon) { mon_lore_kind(mon, RFK_OLYMPIAN); }

bool mon_is_aquatic(mon_ptr mon) { return mon_race_is_aquatic(mon->race); }
bool mon_race_is_aquatic(mon_race_ptr race) { return BOOL(race->kind & RFK_AQUATIC); }
void mon_lore_aquatic(mon_ptr mon) { mon_lore_kind(mon, RFK_AQUATIC); }

bool mon_is_nazgul(mon_ptr mon) { return mon_race_is_nazgul(mon->race); }
bool mon_race_is_nazgul(mon_race_ptr race) { return BOOL(race->kind & RFK_NAZGUL); }
void mon_lore_nazgul(mon_ptr mon) { mon_lore_kind(mon, RFK_NAZGUL); }

bool mon_is_horror(mon_ptr mon) { return mon_race_is_horror(mon->apparent_race); } /* XXX N.B. XXX */
bool mon_race_is_horror(mon_race_ptr race) { return BOOL(race->kind & RFK_HORROR); }
void mon_lore_horror(mon_ptr mon) { mon_lore_kind(mon, RFK_HORROR); } /* cf mon_lore_allow */

/* extra 'kinds' */
bool mon_race_is_chameleon(mon_race_ptr race)
    { return mon_race_is_(race, "R.chameleon") || mon_race_is_(race, "R.Chameleon"); }

bool mon_is_hound(mon_ptr mon) { return mon_race_is_hound(mon->race); }
bool mon_race_is_hound(mon_race_ptr race) { return race->display.c == 'Z'; }

bool mon_is_mimic(mon_ptr mon) { return mon_race_is_mimic(mon->race); }
bool mon_race_is_mimic(mon_race_ptr race) { return mon_race_is_char_ex(race, "!?=$|/\\(["); }

/* uniques */
bool mon_is_unique(mon_ptr mon) { return mon_race_is_unique(mon->race); }
bool mon_race_is_unique(mon_race_ptr race)
    { return BOOL(race->alloc.flags & (RFA_UNIQUE | RFA_UNIQUE2)); }
bool mon_is_fixed_unique(mon_ptr mon) { return mon_race_is_fixed_unique(mon->race); }
bool mon_race_is_fixed_unique(mon_race_ptr race) { return BOOL(race->alloc.flags & RFA_UNIQUE); }

bool mon_is_living_unique(mon_ptr mon) { return mon_race_is_living_unique(mon->race); }
bool mon_race_is_living_unique(mon_race_ptr race)
    { return mon_race_is_fixed_unique(race) &&  race->alloc.max_num > 0; }

bool mon_is_dead_unique(mon_ptr mon) { return mon_race_is_dead_unique(mon->race); }
bool mon_race_is_dead_unique(mon_race_ptr race)
    { return mon_race_is_fixed_unique(race) && race->alloc.max_num == 0; }

/************************************************************************
 * Abilities
 ************************************************************************/
bool mon_can_speak(mon_ptr mon) { return mon_race_can_speak(mon->apparent_race); } /* XXX N.B. XXX */
bool mon_race_can_speak(mon_race_ptr race) { return BOOL(race->abilities & RF_SPEAK); }
void mon_lore_can_speak(mon_ptr mon) { mon_lore_abilities(mon, RF_SPEAK); }

bool mon_can_reflect(mon_ptr mon) { return mon_race_can_reflect(mon->race); }
bool mon_race_can_reflect(mon_race_ptr race) { return BOOL(race->abilities & RF_REFLECT); }
void mon_lore_can_reflect(mon_ptr mon) { mon_lore_abilities(mon, RF_REFLECT); }

bool mon_is_invisible(mon_ptr mon) { return mon_race_is_invisible(mon->race); }
bool mon_race_is_invisible(mon_race_ptr race) { return BOOL(race->abilities & RF_INVIS); }
void mon_lore_is_invisible(mon_ptr mon) { mon_lore_abilities(mon, RF_INVIS); }

bool mon_can_multiply(mon_ptr mon) { return mon_race_can_multiply(mon->race); }
bool mon_race_can_multiply(mon_race_ptr race) { return BOOL(race->abilities & RF_MULTIPLY); }
void mon_lore_can_multiply(mon_ptr mon) { mon_lore_abilities(mon, RF_MULTIPLY); }

bool mon_can_regen(mon_ptr mon) { return mon_race_can_regen(mon->race); }
bool mon_race_can_regen(mon_race_ptr race) { return BOOL(race->abilities & RF_REGEN); }
void mon_lore_can_regen(mon_ptr mon) { mon_lore_abilities(mon, RF_REGEN); }

bool mon_can_retaliate(mon_ptr mon) { return mon_race_can_retaliate(mon->race); }
bool mon_race_can_retaliate(mon_race_ptr race) { return BOOL(race->abilities & RF_REVENGE); }
void mon_lore_can_retaliate(mon_ptr mon) { mon_lore_abilities(mon, RF_REVENGE); }

bool mon_projects_fear(mon_ptr mon) { return mon_race_projects_fear(mon->apparent_race); } /* XXX N.B. XXX */
bool mon_race_projects_fear(mon_race_ptr race) { return BOOL(race->abilities & RF_FEAR); }
void mon_lore_projects_fear(mon_ptr mon) { mon_lore_abilities(mon, RF_FEAR); }

/************************************************************************
 * Attributes
 ************************************************************************/
bool mon_is_male(mon_ptr mon) { return mon_race_is_male(mon->race); }
bool mon_race_is_male(mon_race_ptr race) { return BOOL(race->attributes & RF_MALE); }
void mon_lore_male(mon_ptr mon) { mon_lore_attributes(mon, RF_MALE); }

bool mon_is_female(mon_ptr mon) { return mon_race_is_female(mon->race); }
bool mon_race_is_female(mon_race_ptr race) { return BOOL(race->attributes & RF_FEMALE); }
void mon_lore_female(mon_ptr mon) { mon_lore_attributes(mon, RF_FEMALE); }

bool mon_is_smart(mon_ptr mon) { return mon_race_is_smart(mon->race); }
bool mon_race_is_smart(mon_race_ptr race) { return BOOL(race->attributes & RF_SMART); }
void mon_lore_smart(mon_ptr mon) { mon_lore_attributes(mon, RF_SMART); }

bool mon_is_stupid(mon_ptr mon) { return mon_race_is_stupid(mon->race); }
bool mon_race_is_stupid(mon_race_ptr race) { return BOOL(race->attributes & RF_STUPID); }
void mon_lore_stupid(mon_ptr mon) { mon_lore_attributes(mon, RF_STUPID); }

bool mon_has_weird_mind(mon_ptr mon) { return mon_race_has_weird_mind(mon->race); }
bool mon_race_has_weird_mind(mon_race_ptr race) { return BOOL(race->attributes & RF_WEIRD_MIND); }
void mon_lore_weird_mind(mon_ptr mon) { mon_lore_attributes(mon, RF_WEIRD_MIND); }

bool mon_has_empty_mind(mon_ptr mon) { return mon_race_has_empty_mind(mon->race); }
bool mon_race_has_empty_mind(mon_race_ptr race) { return BOOL(race->attributes & RF_EMPTY_MIND); }
void mon_lore_empty_mind(mon_ptr mon) { mon_lore_attributes(mon, RF_EMPTY_MIND); }

bool mon_is_cold_blooded(mon_ptr mon) { return mon_race_is_cold_blooded(mon->race); }
bool mon_race_is_cold_blooded(mon_race_ptr race) { return BOOL(race->attributes & RF_COLD_BLOOD); }
void mon_lore_cold_blooded(mon_ptr mon) { mon_lore_attributes(mon, RF_COLD_BLOOD); }

bool mon_is_friendly(mon_ptr mon) { return mon_has_smart_flag(mon, SM_FRIENDLY); } /* XXX N.B. Abuse!  XXX */
bool mon_is_temp_friendly(mon_ptr mon) { return mon_is_friendly(mon) && mon_has_smart_flag(mon, SM_TEMP_FRIENDLY); }
bool mon_race_is_friendly(mon_race_ptr race) { return BOOL(race->attributes & RF_FRIENDLY); }
void mon_lore_friendly(mon_ptr mon) { mon_lore_attributes(mon, RF_FRIENDLY); }

bool mon_is_ridable(mon_ptr mon) { return mon_race_is_ridable(mon->race); }
bool mon_race_is_ridable(mon_race_ptr race) { return BOOL(race->attributes & RF_RIDING); }
void mon_lore_ridable(mon_ptr mon) { mon_lore_attributes(mon, RF_RIDING); }

bool mon_kill_exp(mon_ptr mon) { return mon_race_kill_exp(mon->race); }
bool mon_race_kill_exp(mon_race_ptr race) { return BOOL(race->attributes & RF_KILL_EXP); }
void mon_lore_kill_exp(mon_ptr mon) { mon_lore_attributes(mon, RF_KILL_EXP); }

bool mon_immune_illusion(mon_ptr mon) { return mon_race_immune_illusion(mon->race); }
bool mon_race_immune_illusion(mon_race_ptr race) { return BOOL(race->attributes & RF_IM_ILLUSION); }
void mon_lore_immune_illusion(mon_ptr mon) { mon_lore_attributes(mon, RF_IM_ILLUSION); }

bool mon_race_is_template(mon_race_ptr race) { return BOOL(race->attributes & RF_TEMPLATE); }
bool mon_race_is_deprecated(mon_race_ptr race) { return BOOL(race->attributes & RF_DEPRECATED); }

/************************************************************************
 * Saving Throws
 ************************************************************************/
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
    int           ml = r_ptr->alloc.lvl;

    if (mon_race_is_unique(r_ptr))
        ml += ml/5;

    if (ml < 1)
        ml = 1;

    return ml;
}
int  mon_save_level(mon_ptr mon)
{
    int ml = MAX(1, mon->race->alloc.lvl);
    if (mon_is_unique(mon))
        ml += 5 + ml/5;
    /* XXX timed effects could affect saving throws (e.g. stun, berserk) */
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
    #if DEVELOPER
    if (0 || plr->wizard)
    {
        int odds = 0;
        if (ds > dam) odds = (ds - dam)*1000/ds;
        msg_format("<color:D>mon_save_smash(%d, %d) = %d.%d%%</color>", rlev, dam, odds/10, odds%10);
    }
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
   the power makes sense. This happens a bit with project using
   the damage parameter to pass in a player power.

   For damage effects, like GF_SOUND stunning monsters, use
   mon_save_stun and the like instead. Damage numbers can run
   very high (400 or more) which makes the competing dice approach
   not work well (Monsters have no reasonable chance to save).

   For example, the Serpent has 57.6% save from mon_save_stun(400),
   but only 12.6% from mon_save_aux(400).

   As always, build a crosstab in a spreadsheet for analysis.
*/

bool mon_save_aux(mon_ptr mon, int power)
{
    int  ml = mon_save_level(mon);
    bool result = FALSE;

    if (power < 1)
        power = 1;

    #if DEVELOPER
    if (0 || plr->wizard)
    {
        int odds;
        if (power<=ml)
            odds=(2*ml-power+1)*1000/(2*ml);
        else
            odds=(ml+1)*1000/(2*power);
        msg_format("<color:D>mon_save_aux(%d, %d) = %d.%d%%</color>", ml, power, odds/10, odds%10);
    }
    #endif
    if (_1d(power) <= _1d(ml))
        result = TRUE;

    return result;
}

bool mon_save_p(mon_ptr mon, int stat)
{
    int pl = plr->lev;

    if (stat >= 0 && stat < 6)
        pl += adj_stat_save[plr->stat_ind[stat]];

    return mon_save_aux(mon, pl);
}

/************************************************************************
 * XXX
 ************************************************************************/
bool mon_can_attack(mon_ptr mon)
{
    return !mon_never_blow(mon);
}

cptr mon_race_describe_singular(mon_race_ptr race)
{
    switch (mon_race_char(race))
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

cptr mon_race_describe_plural(mon_race_ptr race)
{
    switch (mon_race_char(race))
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
 * XXX
 *************************************************************************/
bool mon_is_dead(mon_ptr mon) { return mon->hp < 0; }
bool mon_is_deleted(mon_ptr mon) { return !mon->dun; }
bool mon_is_valid(mon_ptr mon) { return mon && !mon_is_dead(mon) && !mon_is_deleted(mon); }

bool mon_has_smart_flag(mon_ptr mon, int sm) { return have_flag(mon->smart, sm); }
bool mon_is_pet(mon_ptr mon) { return mon_has_smart_flag(mon, SM_PET); } /* XXX abuse! */
bool mon_is_temp_pet(mon_ptr mon) { return mon_is_pet(mon) && mon_has_smart_flag(mon, SM_TEMP_PET); }
bool mon_is_cloned(mon_ptr mon) { return mon_has_smart_flag(mon, SM_CLONED); }
bool mon_is_hostile(mon_ptr mon)
{
    if (plr->innocence) return FALSE; /* innocence trumps discord! */
    if (mon_tim_find(mon, MT_DISCORD)) return TRUE; /* temporarily override SM_FRIENDLY *and* SM_PET ... mon is not in his right mind! */
    if (mon_is_pet(mon)) return FALSE;
    if (mon_is_friendly(mon)) return FALSE;
    return TRUE;
}

bool mon_ignore_walls(mon_ptr mon)
{
    bool riding = mon->id == plr->riding;

    if (mon->mflag2 & MFLAG2_ILLUSION) return FALSE;
    if (mon_can_passwall(mon) && (!riding || plr->pass_wall))
        return TRUE;

    if (mon_can_tunnel(mon) && !riding)
        return TRUE;

    return FALSE;
}
bool mon_ignore_webs(mon_ptr mon) { return mon_race_ignore_webs(mon->race); }
bool mon_race_ignore_webs(mon_race_ptr race)
{
    if (mon_race_can_passweb(race)) return TRUE;
    if (mon_race_can_clearweb(race)) return TRUE;
    if (mon_auras_find(race, GF_FIRE)) return TRUE;
    if (mon_race_can_passwall(race)) return TRUE;
    if (mon_race_can_tunnel(race)) return TRUE;
    if (mon_race_is_stupid(race)) return TRUE;
    return FALSE;
}
static bool _in_mountain(mon_ptr mon)
{
    if (mon->dun->type->id == D_MOUNTAIN) return TRUE;
    if (mon->dun->type->id == D_RANDOM_MOUNTAIN) return TRUE;
    return FALSE;
}
bool mon_will_flow(mon_ptr mon)
{
    if (mon_ignore_walls(mon) && !_in_mountain(mon)) return FALSE;
    return TRUE;
}
bool mon_never_blow(mon_ptr mon) { return mon_race_never_blow(mon->race); }
bool mon_race_never_blow(mon_race_ptr race) { return vec_length(race->blows) == 0; }

bool mon_can_enter(mon_ptr mon, point_t pos)
{
    if (!dun_pos_interior(mon->dun, pos)) return FALSE;
    if (dun_plr_at(mon->dun, pos)) return FALSE;
    if (dun_mon_at(mon->dun, pos)) return FALSE;
    return mon_can_cross(mon, pos);
}
bool mon_race_can_enter(mon_race_ptr race, point_t pos)
{
    if (!dun_pos_interior(cave, pos)) return FALSE;
    if (dun_plr_at(cave, pos)) return FALSE;
    if (dun_mon_at(cave, pos)) return FALSE;
    return cell_allow_mon_race(dun_grid_at(cave, pos), race);
}
bool mon_can_cross(mon_ptr mon, point_t pos)
{
    if (!dun_pos_interior(mon->dun, pos)) return FALSE;
    return cell_allow_mon(dun_grid_at(mon->dun, pos), mon);
}
bool mon_can_cross_illusion(mon_ptr mon, point_t pos)
{
    if (!dun_pos_interior(mon->dun, pos)) return FALSE;
    return illusion_allow_mon(dun_grid_at(mon->dun, pos), mon);
}
bool mon_can_follow_teleport(mon_ptr mon)
{
    int stun;
    if (!mon_race_can_teleport(mon->race)) return FALSE;
    if (mon_res_pct(mon, GF_TELEPORT) > 0) return FALSE; /* mercy! */
    if (mon->mflag2 & MFLAG2_VAULT) return FALSE;
    if (mon_tim_find(mon, MT_SLEEP)) return FALSE;
    if (mon_tim_find(mon, T_BLIND)) return FALSE;
    if (mon_tim_find(mon, T_CONFUSED)) return FALSE;
    if (mon_tim_find(mon, T_PARALYZED)) return FALSE;
    stun = mon_tim_amount(mon, T_STUN);
    if (stun && randint0(100) < stun) return FALSE;
    return TRUE;
}

term_char_t mon_visual(mon_ptr mon)
{
    if (mon->mflag2 & MFLAG2_FUZZY) /* && !mon->apparent_race->mimic_id ... fuzzy mimics should look real */
        return mon_race_visual_fuzzy(mon->apparent_race);
    return mon_race_visual(mon->apparent_race);
}
term_char_t mon_race_visual(mon_race_ptr race)
{
    /* XXX race->mimic_id? For example, #.granite should use GRANITE with lighting! XXX */
    return visual_get_aux(race->id, 0);
}
term_char_t mon_race_visual_fuzzy(mon_race_ptr race)
{
    char buf[50];
    sprintf(buf, "%c.fuzzy", race->display.c);
    return visual_get(buf, 0);
}
term_char_t mon_visual_ascii(mon_ptr mon) { return mon_race_visual_ascii(mon->apparent_race); }
term_char_t mon_race_visual_ascii(mon_race_ptr race) { return visual_get_ascii_aux(race->id); }
char mon_char(mon_ptr mon) { return mon_race_char(mon->race); }
char mon_race_char(mon_race_ptr race) { return race->display.c; }
bool mon_is_char(mon_ptr mon, char c) { return mon_race_is_char(mon->race, c); }
bool mon_race_is_char(mon_race_ptr race, char c) { return race->display.c == c; }
bool mon_is_char_ex(mon_ptr mon, cptr s) { return mon_race_is_char_ex(mon->race, s); }
bool mon_race_is_char_ex(mon_race_ptr race, cptr s) { return strchr(s, race->display.c) != NULL; }

static int _align_dam_pct(int align)
{
    static point_t tbl[7] = { {-150, 200}, {-50, 150}, {-10, 125}, {0, 100},
                              {10, 80}, {50, 66}, {150, 50} };

    return interpolate(align, tbl, 7);
}
/* Note: the following preserve historical semantics of Hellfire and Holy Orb,
 * except that 'hit hard' is no longer automatically double, but scaled by align. */
int holy_align_dam_pct(int align)
{
    if (align >= ALIGN_GOOD) return 0; /* good monsters immune */
    if (align < 0) return  _align_dam_pct(align); /* evil monsters hit hard */
    return 50; /* neutral monster resist */
}
int hell_align_dam_pct(int align)
{
    if (align > 0) return _align_dam_pct(-align); /* good monsters hit hard */
    return 100; /* everyone else takes full damage */
}
cptr align_desc(int align)
{
    if (align > 150) return "<color:o>Lawful</color>";
    else if (align > 50) return "<color:y>Good</color>";
    else if (align > 10) return "<color:U>Neutral Good</color>";
    else if (align > -11) return "Neutral";
    else if (align > -51) return "<color:R>Neutral Evil</color>";
    else if (align > -151) return "<color:r>Evil</color>";
    return "<color:v>Chaotic</color>";
}
int align_hostile(int a1, int a2) { return align_hostile_aux(a1, a2, 10); }
int align_hostile_aux(int a1, int a2, int threshold)
{
    int h = 0; /* measure of hostility */

    /* slight alignment deviations count as 'neutral' */
    if (ABS(a1) <= threshold) a1 = 0;
    if (ABS(a2) <= threshold) a2 = 0;

    /* Good vs Evil */
    if (SGN(a1) * SGN(a2) == -1)
    {
        h = ABS(a1 - a2);
        /* XXX Use as pct chance of fighting: Neutral Good vs Neutral Evil is 50% (cf ALIGN_NEUTRAL_GOOD) */ 
        /* XXX Use as rank to pick target enemy. Solar picks Chaotic demon before Evil rogue */
    }
    /* XXX Currently, neutrality is the safest choice, but this is not historically correct!
     * Perhaps very evil monsters should not respect neutrality? XXX */
    return h;
}

bool mon_is_evil(mon_ptr mon) { return mon_race_is_evil(mon->race); }
bool mon_race_is_evil(mon_race_ptr race) { return race->align < ALIGN_NEUTRAL_EVIL; }
void mon_lore_evil(mon_ptr mon) { mon_lore_align(mon); }

bool mon_is_good(mon_ptr mon) { return mon_race_is_good(mon->race); }
bool mon_race_is_good(mon_race_ptr race) { return race->align > ALIGN_NEUTRAL_GOOD; }
void mon_lore_good(mon_ptr mon) { mon_lore_align(mon); }

bool mon_is_neutral(mon_ptr mon) { return mon_race_is_neutral(mon->race); }
bool mon_race_is_neutral(mon_race_ptr race) { return strcmp(mon_race_align_desc(race), "Neutral") == 0; }

cptr mon_align_desc(mon_ptr mon) { return mon_race_align_desc(mon->race); } /* XXX sub-align? */
cptr mon_race_align_desc(mon_race_ptr race) { return align_desc(race->align); }

bool mon_race_can_evolve(mon_race_ptr race, mon_race_ptr target)
{
    mon_race_ptr r;
    if (!race->evolution.id) return FALSE;
    r = mon_race_lookup(race->evolution.id);
    while (r)
    {
        if (r == target) return TRUE;
        if (!r->evolution.id) break;
        r = mon_race_lookup(r->evolution.id);
    }
    return FALSE;
}

/*************************************************************************
 * Monster Drops
 *************************************************************************/
static obj_ptr _make(mon_ptr mon, mon_race_ptr race, mon_drop_ptr drop)
{
    int mode = 0, lvl = cave->difficulty;
    obj_ptr obj = NULL;

    /* Uniques get better object creation */
    if (mon_race_is_unique(race))
        mode |= (AM_UNIQUE | AM_GOOD);

    /* Caculate the Object Level, being generous to the player */
    if (race->alloc.lvl >= lvl)
        lvl = race->alloc.lvl;
    else
    {
        if (mode & AM_GREAT)
            lvl = (race->alloc.lvl + 3*lvl) / 4;
        else if (mode & AM_GOOD)
            lvl = (race->alloc.lvl + 2*lvl) / 3;
        else
            lvl = (race->alloc.lvl + lvl) / 2;
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
          || (race->alloc.flags & RFA_GUARDIAN)
          || (race->flagsx & RFX_GUARDIAN) )
        {
            if (one_in_(5))
                mode |= AM_TAILORED;
        }
        if (mon_race_is_unique(race))
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
    mon_race_ptr race = mon->race;
    mon_drop_ptr drop;

    if (!race->drops) return drops;
    if (mon_is_pet(mon)) return drops;

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
            if (plr_race()->boss_r_idx == mon->race->id)
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
    mon_race_ptr race = mon->race;

    mon->drop_ct = 0;
    mon->stolen_ct = 0;
    if (mon_race_is_unique(race))
        mon->stolen_ct = race->stolen_ct;

    /* pre-roll the first rule so rogues can pick pockets */
    if (mon_is_pet(mon)) return;
    if (!race->drops) return;

    mon->drop_ct = _roll_drop_ct(race->drops);
}

obj_ptr mon_pick_pocket(mon_ptr mon)
{
    obj_ptr loot = NULL;

    /* XXX quick check to steal a carried object */

    /* We always steal from the first rule only ... this is for simplicity
     * of implementation for a feature I thought I would enjoy more than I do!
     * In practice, most monsters only have a single rule, and those that don't
     * will use non-initial rules for dropping fixed artifacts, egos or gold. */
    if (mon_is_mimic(mon)) return NULL;  /* don't steal from mimics */
    if (!mon->race->drops) return NULL;
    if (mon->stolen_ct >= mon->drop_ct) return NULL;

    loot = _make(mon, mon->race, mon->race->drops);
    if (loot)
    {
        mon->stolen_ct++;
        if (mon_is_unique(mon))
            mon->race->stolen_ct++;
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
mon_aura_ptr mon_auras_find(mon_race_ptr race, int gf)
{
    mon_aura_ptr aura;
    for (aura = race->auras; aura; aura = aura->next)
    {
        if (aura->gf == gf) return aura;
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
mon_effect_ptr mon_blow_push_effect(mon_blow_ptr blow, int type, dice_t dice)
{
    mon_effect_t e = {0};
    e.type = type;
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
bool mon_blow_allow_slay(mon_blow_ptr blow)
{
    int effect = mon_blow_base_effect(blow);
    switch (effect)
    {
    case RBE_HURT:
    case RBE_SHATTER:
    case RBE_VAMP:
    case GF_MISSILE:
        return TRUE;
    }
    return FALSE;
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
        effect = blow->effects[0].type;
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
 * Monster Packs
 *************************************************************************/
mon_pack_ptr mon_pack_alloc(void)
{
    mon_pack_ptr pack = malloc(sizeof(mon_pack_t));
    memset(pack, 0, sizeof(mon_pack_t));
    pack->members = vec_alloc(NULL);
    return pack;
}
void mon_pack_free(mon_pack_ptr pack)
{
    if (!pack) return;
    vec_free(pack->members);
    if (pack->flow) dun_flow_free(pack->flow);
    free(pack);
}
void mon_pack_save(mon_pack_ptr pack, savefile_ptr file)
{
    savefile_write_u16b(file, pack->id);
    savefile_write_u32b(file, pack->leader_id);
    savefile_write_s16b(file, pack->ai);
    savefile_write_u32b(file, pack->guard_id);
    savefile_write_s16b(file, pack->pos.x);
    savefile_write_s16b(file, pack->pos.y);
    savefile_write_s16b(file, pack->distance);
    savefile_write_u32b(file, pack->prey_id);
}
void mon_pack_load(mon_pack_ptr pack, savefile_ptr file)
{
    pack->id = savefile_read_u16b(file);
    pack->leader_id = savefile_read_u32b(file);
    pack->ai = savefile_read_s16b(file);
    pack->guard_id = savefile_read_u32b(file);
    pack->pos.x = savefile_read_s16b(file);
    pack->pos.y = savefile_read_s16b(file);
    pack->distance = savefile_read_s16b(file);
    pack->prey_id = savefile_read_u32b(file);
}
int mon_pack_count(mon_pack_ptr pack)
{
    return vec_length(pack->members);
}
str_ptr mon_pack_desc(mon_pack_ptr pack)
{
    str_ptr s = str_alloc();
    assert(pack);
    switch (pack->ai)
    {
    case AI_SEEK: str_append_s(s, "<color:R>Seek</color>"); break;
    case AI_LURE: str_append_s(s, "<color:D>Lure</color>"); break;
    case AI_GUARD_MON: {
        mon_ptr mon = mon_pack_mon(pack, pack->guard_id);
        str_printf(s, "<color:B>Guarding <color:U>%s</color></color>", mon->race->name); /* monster_desc calls here, so don't do that */
        break; }
    case AI_GUARD_POS:
        str_printf(s, "<color:B>Guarding <color:U>Location (%d,%d)</color></color>",
            pack->pos.x, pack->pos.y);
        break;
    case AI_FEAR: str_append_s(s, "<color:v>Fear</color>"); break;
    case AI_SHOOT: str_append_s(s, "<color:r>Shoot</color>"); break;
    case AI_MAINTAIN_DISTANCE:
        str_printf(s, "<color:G>Keep Distance: %d</color>", pack->distance);
        break;
    case AI_WANDER: {
        if (pack->flow)
        {
            mon_ptr mon = mon_pack_representative(pack);
            dun_grid_ptr g = dun_grid_at(mon->dun, pack->flow->pos);
            if (cell_is_stairs(g))
                str_printf(s, "<color:B>Wandering to <color:U>Stairs at (%d,%d)</color></color>", pack->flow->pos.x, pack->flow->pos.y);
            else
                str_printf(s, "<color:B>Wandering to <color:U>(%d,%d)</color></color>", pack->flow->pos.x, pack->flow->pos.y);
        }
        else str_append_s(s, "<color:B>Wandering</color>");
        break; }
    case AI_HUNT: {
        mon_ptr mon = mon_pack_representative(pack);
        mon_ptr prey = dun_mon(mon->dun, pack->prey_id);
        if (prey)
        {
            str_printf(s, "<color:R>Hunting <color:U>%s (%d,%d)</color></color>",
                prey->race->name,
                prey->pos.x, prey->pos.y);
        }
        else
            str_append_s(s, "<color:R>Hunting (No Current Prey)</color>");
        break; }
    case AI_PATROL: str_append_s(s, "Patrol"); break;
    case AI_PETS: str_append_s(s, "Pets"); break;
    default: str_append_s(s, "Doing Something Weird"); break;
    }
    if (pack->leader_id)
    {
        mon_ptr mon = mon_pack_leader(pack);
        if (mon)
            str_printf(s, " <color:U>(Boss: <color:w>%s %d</color>)</color>", mon->race->name, mon->id);
        else
            str_printf(s, " <color:r>(Boss: Missing %d)</color>", pack->leader_id);
    }
    return s;
}
void mon_pack_doc(mon_pack_ptr pack, doc_ptr doc)
{
    str_ptr s = mon_pack_desc(pack);
    doc_insert(doc, str_buffer(s));
    str_free(s);
}
mon_ptr mon_pack_mon(mon_pack_ptr pack, u32b id)
{
    int i;
    for (i = 0; i < vec_length(pack->members); i++)
    {
        mon_ptr test = vec_get(pack->members, i);
        if (test->id == id)
            return test;
    }
    return NULL;
}
mon_ptr mon_pack_representative(mon_pack_ptr pack)
{
    mon_ptr mon = mon_pack_leader(pack);
    if (!mon)
    {
        int ct = vec_length(pack->members);
        if (ct) mon = vec_get(pack->members, 0);
    }
    return mon;
}
mon_ptr mon_pack_leader(mon_pack_ptr pack)
{
    if (!pack->leader_id) return NULL;
    return mon_pack_mon(pack, pack->leader_id);
}
int mon_pack_align(mon_pack_ptr pack)
{
    int i, align = 0, weight = 0;
    for (i = 0; i < vec_length(pack->members); i++)
    {
        mon_ptr mon = vec_get(pack->members, i);
        int     w = mon_lvl(mon);

        if (!mon_is_valid(mon)) continue; /* dead member not removed yet */
        if (mon_has_smart_flag(mon, SM_TEMP_PET)) continue;

        if (mon->id == pack->leader_id)
            w *= 10;

        align += w*mon->align;
        weight += w;
    }
    if (weight)
        return align / weight;
    return 0;
}
#define INVALID_INDEX -1
static int _mon_pack_find(mon_pack_ptr pack, mon_ptr mon)
{
    int i;
    for (i = 0; i < vec_length(pack->members); i++)
    {
        mon_ptr test = vec_get(pack->members, i);
        if (mon == test) return i;
    }
    return INVALID_INDEX;
}
/* scatter is used for inital mon_pack placement: cf place_monster_aux */
point_t mon_pack_scatter(mon_pack_ptr pack, int radius)
{
    int attempts = 20;
    assert(vec_length(pack->members)); /* don't 'scatter' the pack leader/representative */
    while (attempts--)
    {
        mon_ptr mon = vec_random(pack->members);
        point_t p;

        if (!mon_is_valid(mon)) continue;
        if (mon->dun != cave) continue;

        p = scatter(mon->pos, radius);
        if (dun_allow_mon_at(cave, p)) return p;
    }
    return point_create(-1, -1);
}
void mon_pack_add(mon_pack_ptr pack, mon_ptr mon)
{
    assert(!mon->pack);
    assert(_mon_pack_find(pack, mon) == INVALID_INDEX);
    if (pack->ai == AI_PETS)
    {
        assert(mon_is_pet(mon));
    }
    /* Enforce pack hostility to plr ... The leader chooses this
     * in place_monster_one (pack_id = 0). Followers should align
     * with the leader in place_monster_one(pack_id != 0)->mon_pack_add */
    else if (vec_length(pack->members))
    {
        mon_ptr test = vec_get(pack->members, 0);
        if (mon_is_friendly(test))
            add_flag(mon->smart, SM_FRIENDLY);
        else if (!mon_is_pet(test)) /* pets are implicitly "friendly", but don't use SM_FRIENDLY */
            remove_flag(mon->smart, SM_FRIENDLY);
    }
    mon->pack = pack;
    vec_push(pack->members, mon);
}
void mon_pack_remove(mon_pack_ptr pack, mon_ptr mon)
{
    int i = _mon_pack_find(pack, mon);
    mon->pack = NULL;
    if (i == INVALID_INDEX) return;
    vec_delete(pack->members, i);
    if (!vec_length(pack->members))
    {
        if (pack->ai != AI_PETS)
            dun_mgr_free_pack(pack);
    }
    else
    {
        if (pack->leader_id == mon->id) /* dun_mgr_gc might partially depopulate a pack */
        {
            pack->ai = AI_SEEK;
            pack->leader_id = 0;
        }
        if (pack->guard_id == mon->id)
        {
            pack->ai = AI_SEEK;
            pack->guard_id = 0;
        }
    }
}
void mon_pack_iter(mon_pack_ptr pack, mon_f f)
{
    int i;
    assert(f);
    for (i = 0; i < vec_length(pack->members); i++)
    {
        mon_ptr mon = vec_get(pack->members, i);
        f(mon);
    }
}
vec_ptr mon_pack_filter(mon_pack_ptr pack, mon_p p)
{
    return vec_filter(pack->members, (vec_item_p)p);
}
void mon_pack_anger(mon_pack_ptr pack)
{
    mon_pack_iter(pack, anger_monster);
}
void mon_pack_set_target(mon_pack_ptr pack, mon_ptr victim, mon_ptr culprit)
{
    /* culprit shamelessly attacked victim, a member of this pack. other
     * members of the pack may notice this offense and seek retribution! */
    int i, d;
    if (!mon_is_valid(victim)) return;
    if (!mon_is_valid(culprit)) return;
    for (i = 0; i < vec_length(pack->members); i++)
    {
        mon_ptr mon = vec_get(pack->members, i);
        if (mon == victim) continue;
        if (mon_tim_find(mon, MT_SLEEP)) continue;
        if (mon_tim_find(mon, T_CONFUSED)) continue;
        d = point_fast_distance(victim->pos, mon->pos); 
        if (d > MAX_SIGHT/2) continue;   /* too far away */
        if (mon->target_id) continue; /* currently fighting somebody else */
        if (dun_pos_interior(victim->dun, mon->target_pos)) continue; /* distracted by a lure */
        if (!point_project(mon->pos, victim->pos)) continue; /* need to witness the offense ... */
        if (!point_project(mon->pos, culprit->pos)) continue; /* ... as well as who dunnit */
        mon->target_id = culprit->id;
    }
}
static void _mon_pack_unflow(mon_pack_ptr pack)
{
    if (pack->flow)
    {
        dun_flow_free(pack->flow);
        pack->flow = NULL;
    }
}
static bool _mon_pack_wander_aux(mon_pack_ptr pack, mon_ptr mon, point_t pos)
{
    if (!pack->flow)
        pack->flow = dun_flow_calc(mon->dun, pos, MON_WANDER_RAD, NULL);
    else if (pack->flow->dun != mon->dun) /* took stairs to new lvl */
    {
        dun_flow_free(pack->flow);
        pack->flow = dun_flow_calc(mon->dun, pos, MON_WANDER_RAD, NULL);
    }
    else /* new goal on current lvl */
        dun_flow_recalc(pack->flow, pos);

    if (dun_flow_at(pack->flow, mon->pos) != DUN_FLOW_NULL)
    {
        pack->pos = pos;
        return TRUE;
    }
    return FALSE;
}
void mon_pack_wander(mon_pack_ptr pack)
{
    mon_ptr mon = mon_pack_representative(pack);
    mon_race_ptr race;
    dun_ptr dun;
    rect_t  rect;
    int     attempt = 1000;

    if (pack->ai != AI_WANDER) return;
    if (!mon) return;
    race = mon->race;
    dun = mon->dun;
    rect = rect_interior(dun->rect);
    /* Wander off level (cf _try_stairs in mon_ai.c). Note we cannot
     * handle large packs wandering off level, since that would require
     * one goal (flow) for each level containing pack members. This
     * is for friendly uniques. 
     * XXX Actually, we could handle, but we'd need to detect mon->dun_id
     * different from flow->dun->id and, also, we'd then need to know
     * which staircase the pack is flowing across, which might entail
     * another level of pathfinding. Probably not worth the effort ... XXX */
    if (mon_pack_count(pack) == 1 && one_in_(20))
    {
        point_t goal = {0};
        if (plr->dun_id != dun->id) /* XXX wander to plr_dun() */
            goal = dun->flow_pos;
        else /* leave plr_dun() */
        {
            dun_stairs_ptr stairs = dun_random_stairs(dun);
            if (stairs) goal = stairs->pos_here;
        }
        if (dun_pos_interior(dun, goal) && _mon_pack_wander_aux(pack, mon, goal))
            return;
    }
    /* look for a new room to path towards */
    while (--attempt)
    {
        point_t p = rect_random_point(rect);
        int d = point_fast_distance(mon->pos, p);
        dun_grid_ptr g;

        if (d < 15 || d > 80) continue;
        g = dun_grid_at(dun, p);
        if (!(g->flags & CELL_ROOM)) continue;
        if (!cell_allow_mon_race(g, race)) continue;

        if (_mon_pack_wander_aux(pack, mon, p)) break;
    }
    if (!attempt)
    {
        pack->ai = AI_SEEK;
        _mon_pack_unflow(pack);
    }
}
static mon_ptr _pack_mon = NULL;
static bool _is_valid_prey(mon_ptr mon) { return are_enemies(_pack_mon, mon); }
static int _cmp_prey(mon_ptr p1, mon_ptr p2)
{
    int d1, d2;
    assert(_pack_mon);
    d1 = point_fast_distance(_pack_mon->pos, p1->pos);
    d2 = point_fast_distance(_pack_mon->pos, p2->pos);
    if (d1 < d2) return -1;
    if (d1 > d2) return 1;
    return 0;
}
mon_ptr mon_pack_hunt(mon_pack_ptr pack)
{
    mon_ptr mon = mon_pack_representative(pack);
    dun_ptr dun;
    vec_ptr v;
    mon_ptr prey = NULL;
    int     i;

    if (!mon) return NULL;
    if (pack->ai == AI_WANDER) /* took stairs to new lvl (_try_stairs) */
    {
        _mon_pack_unflow(pack);
        mon->mflag2 &= ~MFLAG2_HUNTER;
        pack->ai = AI_HUNT;
    }
    if (pack->ai != AI_HUNT) return NULL;
    /* XXX No hunting on surface as 100 depth flow calcs are too slow. For example, 
     * if Nami spawns on the surface, she begins hunting the wilderness with noticeable
     * slowdown ... at least in DEBUG builds. XXX */
    if (mon->dun->type->id == D_SURFACE)
    {
        pack->ai = AI_SEEK;
        return NULL;
    }

    /* find a (reachable) prey */
    dun = mon->dun;
    _pack_mon = mon;
    v = dun_filter_mon(dun, _is_valid_prey);
    vec_sort(v, (vec_cmp_f)_cmp_prey);

    for (i = 0; i < vec_length(v); i++)
    {
        prey = vec_get(v, i);
        if (prey->mflag2 & MFLAG2_HUNTED)
        {
            /* XXX already being hunted by somebody else */
            if (dun_flow_at(prey->flow, mon->pos) != DUN_FLOW_NULL) break;
        }
        else
        {
            mon_set_hunted(prey);
            assert(prey->flow);
            if (dun_flow_at(prey->flow, mon->pos) != DUN_FLOW_NULL) break;
            prey->mflag2 &= ~MFLAG2_HUNTED;
            dun_flow_free(prey->flow);
            prey->flow = NULL;
        }
        prey = NULL; /* XXX in case no reachable prey with non-empty v (rubble) */
    }
    vec_free(v);

    if (prey)
    {
        mon_set_hunted(prey);
        pack->prey_id = prey->id;
        return prey;
    }
    /* wander to next level if no more prey (solitary packs only for now) */
    else if (mon_pack_count(pack) == 1)
    {
        point_t goal = {0};

        mon->mflag2 |= MFLAG2_HUNTER; /* so we can recover AI_HUNT on next lvl (_try_stairs) */
        pack->prey_id = 0;
        pack->ai = AI_WANDER;

        if (plr->dun_id != dun->id) /* wander to plr_dun() */
            goal = dun->flow_pos;
        else /* leave plr_dun() */
        {
            dun_stairs_ptr stairs = dun_random_stairs(dun);
            if (stairs) goal = stairs->pos_here;
        }
        if (!dun_pos_interior(dun, goal) || !_mon_pack_wander_aux(pack, mon, goal))
        {
            mon->mflag2 &= ~MFLAG2_HUNTER;
            mon_pack_wander(pack); /* wander current level instead */
        }
    }
    else
    {
        pack->prey_id = 0;
        pack->ai = AI_SEEK; /* give up and seek plr */
    }
    return NULL;
}
void mon_pack_choose_ai(mon_pack_ptr pack)
{
    mon_ptr mon = mon_pack_representative(pack);
    mon_race_ptr race;

    if (!mon) return;
    race = mon->race;

    pack->ai = AI_SEEK; /* paranoia ... make sure something gets chosen! */
    if (mon_pack_count(pack) == 1 && mon_is_unique(mon))
    {   /*      v--- Artemis has no attacks ... probably a bug I should fix later */
        int t = mon_can_attack(mon) ? 100 : 25;
        int p = randint0(t);

        if (mon_is_friendly(mon) && mon->race->alloc.lvl > 0) /* XXX friendly uniques used to be drunkards ... */
        {
            /* XXX Occasionally hunt. Otherwise AI_WANDER */
            pack->ai = AI_HUNT;
            mon_pack_hunt(pack);
        }
        else if (mon_is_pet(mon))
            pack->ai = AI_SEEK;
        else if (p < 5 && mon_has_worthy_attack_spell(mon))
            pack->ai = AI_SHOOT;
        else if (p < 15 && mon_has_summon_spell(mon))
        {
            /* Lure the player into an open room in order to surround
                with evil summons! */
            pack->ai = AI_LURE;
        }
        else if ((p < 25 && mon_has_worthy_attack_spell(mon)) || !mon_can_attack(mon))
        {
            /* Hang back and pelt the player from a distance */
            pack->ai = AI_MAINTAIN_DISTANCE;
            pack->distance = 5;
        }
        else
            pack->ai = AI_SEEK;
    }
    else if (mon_pack_count(pack) == 1)
    {
        /* mon_race->friends but none of the rules succeeded. often, monsters
         * have rules with low percentages so that packs are optional. in this
         * case, simply AI_SEEK */
        assert(!mon_is_unique(mon)); /* handled above */
        pack->ai = AI_SEEK;
    }
    else if (mon_race_is_animal(race))
    {
        switch(randint1(10))
        {
        case 1: case 2: case 3:
            pack->ai = AI_SEEK;
            break;
        case 4:
            if (mon_race_has_worthy_attack_spell(race))
                pack->ai = AI_SHOOT;
            else
                pack->ai = AI_LURE;
            break;
        default:
            pack->ai = AI_LURE;
            break;
        }
    }
    else
    {
        switch(randint1(10))
        {
        case 1: case 2: case 3: case 4: case 5: case 6:
            pack->ai = AI_SEEK;
            break;
        case 7: case 8:
            pack->ai = AI_LURE;
            break;
        case 9:
            if (pack->leader_id)
            {
                pack->ai = AI_GUARD_MON;
                pack->guard_id = pack->leader_id;
            }
            else if (mon_race_has_attack_spell(race))
            {
                pack->ai = AI_GUARD_POS;
                pack->pos = mon->pos;
            }
            else
                pack->ai = AI_LURE;
            break;
        case 10:
            if (mon_race_has_worthy_attack_spell(race))
                pack->ai = AI_SHOOT;
            else
                pack->ai = AI_SEEK;
            break;
        }
    }
}


void mon_packs_on_damage(mon_ptr mon)
{
    if (!mon->pack) return;
    if ( mon->pack->leader_id == mon->id
      && mon->pack->ai != AI_SEEK
      && one_in_(3) )
    {
        mon->pack->ai = AI_SEEK;
    }
}

void mon_packs_on_death(mon_ptr mon)
{
    if (!mon->pack) return;
    /* mon_pack_remove will happen in _mon_free (dun.c) */
    if (mon->pack->leader_id == mon->id)
    {
        mon->pack->ai = AI_FEAR;
        mon->pack->leader_id = 0;
    }
    else if (mon->pack->ai != AI_FEAR)
    {
        int ct = vec_length(mon->pack->members) - 1; /* not removed yet */
        if (one_in_(ct * (mon->pack->leader_id ? 2 : 1)))
            mon->pack->ai = AI_FEAR;
        else if (mon->pack->ai == AI_LURE && one_in_(2))
            mon->pack->ai = AI_SEEK;
        else if (mon->pack->ai == AI_SHOOT && one_in_(3))
            mon->pack->ai = AI_SEEK;
    }
    if (mon->pack->guard_id == mon->id)
    {
        if (mon->pack->ai == AI_GUARD_MON) mon->pack->ai = AI_SEEK;
        mon->pack->guard_id = 0;
    }
}

/*************************************************************************
 * Debugging
 *************************************************************************/
void mon_wizard(mon_ptr mon)
{
    doc_ptr doc = doc_alloc(80);
    mon_wizard_doc(mon, doc);
    doc_display(doc, "Wizard Probe", 0);
    doc_free(doc);
    do_cmd_redraw();
}
static void _obj_wizard(mon_ptr mon, doc_ptr doc)
{
    /* XXX */
}
static void _pack_wizard(mon_ptr mon, doc_ptr doc)
{
    doc_insert(doc, "Pack    : <indent><style:indent>");
    mon_pack_doc(mon->pack, doc);
    /* XXX */
    doc_insert(doc, "</style></indent>\n");
}
static void _target_wizard(mon_ptr mon, doc_ptr doc)
{
    dun_ptr dun = mon->dun;
    doc_insert(doc, "Target  : <indent><style:indent>");
    if (mon->target_id)
    {
        mon_ptr target = dun_mon(dun, mon->target_id);
        if (target)
        {
            doc_printf(doc, "%s at (%d,%d)",
                target->race->name,
                target->pos.x,
                target->pos.y);
        }
        else
        {
            doc_insert(doc, "<color:v>Invalid (Dead?)</color>");
        }
    }
    else
    {
        doc_printf(doc, "Location (%d,%d)", mon->target_pos.x, mon->target_pos.y);
    }
    doc_insert(doc, "</style></indent>\n");
}
void mon_wizard_doc(mon_ptr mon, doc_ptr doc)
{
    mon_race_ptr race = mon->race;
    term_char_t ac = mon_race_visual_ascii(race);
    term_char_t gc = mon_race_visual(race);
    doc_printf(doc, "Name    : <indent><style:indent><color:B>%s</color></style></indent>\n", race->name);
    doc_printf(doc, "Display : <color:%c>%c</color>", attr_to_attr_char(ac.a), ac.c);
    if (use_graphics && (gc.c != ac.c || gc.a != ac.a))
    {
        doc_insert_char(doc, TERM_WHITE, ' ');
        doc_insert_term_char(doc, gc);
    }
    doc_newline(doc);
    if (mon->align != mon->race->align)
        doc_printf(doc, "Align   : %s\n", align_desc(mon->align));
    if (mon_has_valid_target(mon)) _target_wizard(mon, doc);
    if (mon->mpower != 1000)
    {
        doc_printf(doc, "Power   : <color:%c>%d.%d%%</color>\n",
            mon->mpower > 1000 ? 'r' : 'u', mon->mpower/10, mon->mpower%10);
    }
    if (mon->parent_id)
    {
        mon_ptr parent = dun_mon(mon->dun, mon->parent_id);
        if (!parent)
            doc_insert(doc, "Parent  : <color:R>Missing</color>\n");
        else 
        {
            doc_printf(doc, "Parent  : %s at (%d,%d)\n",
                parent->race->name,
                parent->pos.x,
                parent->pos.y);
        }
    }
    if (mon->timers)
    {
        doc_insert(doc, "Timers  : <indent>");
        mon_tim_probe(mon, doc);
        doc_insert(doc, "</indent>\n");
    }
    if (mon->pack) _pack_wizard(mon, doc);
    if (race->spells)
    {
        doc_insert(doc, "\n\n");
        if (plr->pclass == CLASS_BLUE_MAGE)
        {
            doc_insert(doc, "Blue Mage Spell Table:\n");
            blue_mage_wizard_probe(race, doc);
        }
        else
            mon_spell_wizard(mon, NULL, doc);
        doc_newline(doc);
    }
    if (mon->obj) _obj_wizard(mon, doc);
    /*mon_display_doc(mon->race, doc);*/
}

/*************************************************************************
 * Savefiles
 *************************************************************************/
enum {
    SAVE_MON_DONE = 0,
    SAVE_MON_AP_R_IDX,
    SAVE_MON_ALIGN,
    SAVE_MON_TIMER,
    SAVE_MON_TARGET,
    SAVE_MON_SMART_0,
    SAVE_MON_SMART_1,
    SAVE_MON_EXP,
    SAVE_MON_MFLAG2,
    SAVE_MON_NICKNAME,
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
    SAVE_MON_TARGET_ID,
    SAVE_MON_PARENT,
    SAVE_MON_PACK_ID,
    SAVE_MON_LAST_ENEMY,
};

void mon_load(mon_ptr mon, savefile_ptr file)
{
    char buf[128];

    mon->id = savefile_read_u32b(file);
    mon->mpower = 1000;
    mon->race = mon_race_lookup(savefile_read_sym(file));
    mon->align = mon->race->align;
    mon->apparent_race = mon->race;
    mon->pos.x = savefile_read_s16b(file);
    mon->pos.y = savefile_read_s16b(file);
    mon->hp = savefile_read_s16b(file);
    mon->maxhp = savefile_read_s16b(file);
    mon->max_maxhp = savefile_read_s16b(file);
    mon->mspeed = savefile_read_s16b(file);
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
            mon->apparent_race = mon_race_lookup(savefile_read_sym(file));
            break;
        case SAVE_MON_ALIGN:
            mon->align = savefile_read_s16b(file);
            break;
        case SAVE_MON_TIMER:
            savefile_read_byte(file);
            savefile_read_s16b(file);
            break;
        case SAVE_MON_TIMERS:
            mon_tim_load(mon, file);
            break;
        case SAVE_MON_TARGET:
            mon->target_pos.x = savefile_read_s16b(file);
            mon->target_pos.y = savefile_read_s16b(file);
            break;
        case SAVE_MON_TARGET_ID:
            mon->target_id = savefile_read_u32b(file);
            break;
        case SAVE_MON_LAST_ENEMY:
            mon->last_enemy_pos.x = savefile_read_s16b(file);
            mon->last_enemy_pos.y = savefile_read_s16b(file);
            break;
        case SAVE_MON_SMART_0:
            mon->smart[0] = savefile_read_u32b(file);
            break;
        case SAVE_MON_SMART_1:
            mon->smart[1] = savefile_read_u32b(file);
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
            mon->parent_id = savefile_read_u32b(file);
            break;
        case SAVE_MON_PACK_ID:
            mon->pack = dun_mgr_pack(savefile_read_u16b(file));
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
            savefile_read_s32b(file);
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
    savefile_write_u32b(file, mon->id);
    savefile_write_sym(file, mon->race->id);
    savefile_write_s16b(file, mon->pos.x);
    savefile_write_s16b(file, mon->pos.y);
    savefile_write_s16b(file, mon->hp);
    savefile_write_s16b(file, mon->maxhp);
    savefile_write_s16b(file, mon->max_maxhp);
    savefile_write_s16b(file, mon->mspeed);
    savefile_write_s16b(file, mon->cdis);
    savefile_write_s16b(file, mon->energy_need);

    if (!is_original_ap(mon))
    {
        savefile_write_byte(file, SAVE_MON_AP_R_IDX);
        savefile_write_sym(file, mon->apparent_race->id);
    }
    if (mon->align != mon->race->align)
    {
        savefile_write_byte(file, SAVE_MON_ALIGN);
        savefile_write_s16b(file, mon->align);
    }
    if (mon_tim_count(mon))
    {
        savefile_write_byte(file, SAVE_MON_TIMERS);
        mon_tim_save(mon, file);
    }
    if (mon->target_pos.x || mon->target_pos.y)
    {
        savefile_write_byte(file, SAVE_MON_TARGET);
        savefile_write_s16b(file, mon->target_pos.x);
        savefile_write_s16b(file, mon->target_pos.y);
    }
    if (mon->target_id)
    {
        savefile_write_byte(file, SAVE_MON_TARGET_ID);
        savefile_write_u32b(file, mon->target_id);
    }
    if (mon->last_enemy_pos.x || mon->last_enemy_pos.y)
    {
        savefile_write_byte(file, SAVE_MON_LAST_ENEMY);
        savefile_write_s16b(file, mon->last_enemy_pos.x);
        savefile_write_s16b(file, mon->last_enemy_pos.y);
    }
    if (mon->smart[0])
    {
        savefile_write_byte(file, SAVE_MON_SMART_0);
        savefile_write_u32b(file, mon->smart[0]);
    }
    if (mon->smart[1])
    {
        savefile_write_byte(file, SAVE_MON_SMART_1);
        savefile_write_u32b(file, mon->smart[1]);
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
    if (mon->parent_id)
    {
        savefile_write_byte(file, SAVE_MON_PARENT);
        savefile_write_u32b(file, mon->parent_id);
    }
    if (mon->pack)
    {
        savefile_write_byte(file, SAVE_MON_PACK_ID);
        savefile_write_u16b(file, mon->pack->id);
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

