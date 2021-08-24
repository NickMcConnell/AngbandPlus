#include "angband.h"

#include <assert.h>
#include "str_map.h"

skill_table *s_info;

static int _class_idx(void)
{
    int result = plr->pclass;
    if (result == CLASS_MONSTER)
    {
        race_t *race_ptr = get_race();
        if (race_ptr->pseudo_class_id != CLASS_NONE)
            result = race_ptr->pseudo_class_id;
    }
    return result;
}

void skills_add(skills_t *dest, skills_t *src)
{
    dest->dis += src->dis;
    dest->dev += src->dev;
    dest->sav += src->sav;
    dest->stl += src->stl;
    dest->srh += src->srh;
    dest->fos += src->fos;
    dest->thn += src->thn;
    dest->thb += src->thb;
}

void skills_scale(skills_t *dest, int num, int denom)
{
    dest->dis = dest->dis * num / denom;
    dest->dev = dest->dev * num / denom;
    dest->sav = dest->sav * num / denom;
    dest->stl = dest->stl * num / denom;
    dest->srh = dest->srh * num / denom;
    dest->fos = dest->fos * num / denom;
    dest->thn = dest->thn * num / denom;
    dest->thb = dest->thb * num / denom;
}

void skills_wipe(skills_t *dest)
{
    memset(dest, 0, sizeof(skills_t));
}

skill_desc_t skills_describe(int amt, int div)
{
    skill_desc_t result = {0};

    if (div <= 0) div = 1;
    if (amt < 0)
    {
        result.desc = "Very Bad";
        result.color = TERM_L_DARK;
    }
    else
    {
        int n = amt / div;
        switch (n)
        {
        case 0:
        case 1:
            result.desc = "Bad";
            result.color = TERM_RED;
            break;
        case 2:
            result.desc = "Poor";
            result.color = TERM_L_RED;
            break;
        case 3:
        case 4:
            result.desc = "Fair";
            result.color = TERM_ORANGE;
            break;
        case 5:
            result.desc = "Good";
            result.color = TERM_YELLOW;
            break;
        case 6:
            result.desc = "Very Good";
            result.color = TERM_YELLOW;
            break;
        case 7:
        case 8:
            result.desc = "Excellent";
            result.color = TERM_L_GREEN;
            break;
        case 9:
        case 10:
        case 11:
        case 12:
        case 13:
            result.desc = "Superb";
            result.color = TERM_GREEN;
            break;
        case 14:
        case 15:
        case 16:
        case 17:
            result.desc = "Heroic";
            result.color = TERM_BLUE;
            break;
        default:
        {
            int k = (n - 17) * 5 / 2;
            result.desc = format("Amber[%d]", k); /*Legendary is too long for tables */
            result.color = TERM_VIOLET;
            break;
        }
        }
    }
    return result;
}

int skills_bow_current(int sval)
{
    if (plr->pclass == CLASS_SKILLMASTER)
        return skillmaster_bow_prof();
    else
    {
        int max = skills_bow_max(sval);
        int cur = plr->weapon_exp[0][sval];

        if (cur > max)
            cur = max;

        return cur;
    }
}

int skills_bow_max(int sval)
{
    if (mut_present(MUT_WEAPON_SKILLS))
        return WEAPON_EXP_MASTER;

    if (demigod_is_(DEMIGOD_ARTEMIS))
        return WEAPON_EXP_MASTER;

    if (plr->pclass == CLASS_SKILLMASTER)
        return skillmaster_bow_prof();

    return s_info[_class_idx()].w_max[0][sval];
}

static int _bow_calc_bonus_aux(int sval, int skill)
{
    int bonus;

    /* XXX Historically, crossbows have never had proficiency penalties, while
     * bows and slings have behaved just like melee weapons. I'm not sure why this
     * has always been so ... */
    if (sval == SV_LIGHT_XBOW || sval == SV_HEAVY_XBOW)
        bonus = skill / 400; /* +0 to +20 */
    else              /* v~~~~~~ WEAPON_EXP_BEGINNER */
        bonus = (skill - WEAPON_EXP_MASTER/2) / 200; /* -20 to +20 */

    return bonus;
}

int skills_bow_calc_bonus(int sval)
{
    int current = skills_bow_current(sval);
    return _bow_calc_bonus_aux(sval, current);
}

/* XXX Use same logic as melee weapons for now ... */
static int _weapon_min_rlvl(int plvl);
static int _weapon_max_skill(int rlvl);
static int _weapon_gain_amt(int skill);
void skills_bow_gain(int sval, int rlvl)
{
    int max = skills_bow_max(sval);
    int cur = plr->weapon_exp[0][sval];

    if (plr->pclass == CLASS_SKILLMASTER) return;
    if (cur < max)
    {
        int step;
        int add;

        if (rlvl < _weapon_min_rlvl(plr->lev))
        {
            if (plr->wizard)
                msg_format("<color:B>You must shoot level <color:R>%d</color> monsters to gain bow proficiency.</color>", _weapon_min_rlvl(plr->lev));
            return;
        }
        if (cur >= _weapon_max_skill(rlvl))
        {
            if (plr->wizard)
            {
                msg_format("<color:B>Against level <color:R>%d</color> foes, you can only train bow "
                    "proficiency to <color:R>%d</color> (Current Skill: <color:R>%d</color>).</color>",
                    rlvl, _weapon_max_skill(rlvl), cur);
            }
            return;
        }

        step = _weapon_gain_amt(cur);
        add = step / 10;
        if (step%10 && randint0(10) < step%10) add++;

        if (add > 0)
        {
            int old_bonus = _bow_calc_bonus_aux(sval, cur);
            int new_bonus;
            cur += add;
            if (cur > max)
                cur = max;
            new_bonus = _bow_calc_bonus_aux(sval, cur);
            plr->weapon_exp[0][sval] = cur;
            if (old_bonus != new_bonus)
            {
                int k_idx = lookup_kind(TV_BOW, sval);
                char buf[MAX_NLEN];
                strip_name(buf, k_idx);
                msg_format("<color:B>Your <color:R>%s</color> skills are improving.</color>", buf);
                plr->update |= PU_BONUS;
            }
        }
    }
}

int skills_weapon_current(int tval, int sval)
{
    int max;
    int cur;

    if (tval == TV_BOW)
        return skills_bow_current(sval);

    assert(tv_is_weapon(tval));

    if (plr->pclass == CLASS_SKILLMASTER)
        return skillmaster_weapon_prof(tval);

    max = skills_weapon_max(tval, sval);
    cur = plr->weapon_exp[tval-TV_WEAPON_BEGIN][sval];

    if (cur > max)
        cur = max;

    return cur;
}

void skills_weapon_init(int tval, int sval, int skill)
{
    int max, cur;

    assert(tv_is_weapon(tval) || tval == TV_BOW);

    max = skills_weapon_max(tval, sval);
    cur = MIN(skill, max);

    plr->weapon_exp[tval-TV_WEAPON_BEGIN][sval] = cur;
}

int skills_weapon_max(int tval, int sval)
{
    if (tval == TV_BOW)
        return skills_bow_max(sval);

    assert(tv_is_weapon(tval));

    if (plr->pclass == CLASS_SKILLMASTER)
        return skillmaster_weapon_prof(tval);

    if (mut_present(MUT_WEAPON_SKILLS))
        return WEAPON_EXP_MASTER;

    /* TODO: Add a class_t callback to override the skill tables ... */

    /* Dragon Warlocks are Dragon Riders! */
    if ( warlock_is_(WARLOCK_DRAGONS)
      && tval == TV_POLEARM
      && (sval == SV_LANCE || sval == SV_HEAVY_LANCE) )
    {
        return WEAPON_EXP_MASTER;
    }

    if (warlock_is_(WARLOCK_GIANTS) && tval != TV_BOW)
    {
        int k_idx = lookup_kind(tval, sval);
        if (k_info[k_idx].weight >= 200)
            return WEAPON_EXP_MASTER;
        if (k_info[k_idx].weight <= 100)
            return WEAPON_EXP_BEGINNER;
    }

    /* Edged weapons for priests. I never understood the restriction for evil priests! */
    if ( (plr->pclass == CLASS_PRIEST || plr->pclass == CLASS_HIGH_PRIEST)
      && (tval == TV_SWORD || tval == TV_POLEARM))
    {
        if (priest_is_evil())
            return WEAPON_EXP_SKILLED;
        else /* priests of life and nature abhor bloodshed */
            return WEAPON_EXP_BEGINNER;
    }

    if (plr->personality == PERS_SEXY && tval == TV_HAFTED && sval == SV_WHIP)
        return WEAPON_EXP_MASTER;

    if (demigod_is_(DEMIGOD_POSEIDON) && tval == TV_POLEARM && sval == SV_TRIDENT)
        return WEAPON_EXP_MASTER;

    return s_info[_class_idx()].w_max[tval-TV_WEAPON_BEGIN][sval];
}

/* Weapons: Gaining Proficiency
 * [1] r_level must exceed a min_r_level(plvl)
 * [2] current skill must not exceed a max_skill(rlvl)
 * [3] amount gained decays exponentially with skill
 * [4] effects of skill are linear */
static int _weapon_calc_bonus_aux(int skill)
{
    int bonus = (skill - WEAPON_EXP_BEGINNER) / 200; /* -20 to +20 */
    return bonus;
}

int skills_weapon_calc_bonus(int tval, int sval)
{
    int current = skills_weapon_current(tval, sval);
    return _weapon_calc_bonus_aux(current);
}

int skills_martial_arts_calc_bonus(void)
{
    int current = skills_martial_arts_current();
    return _weapon_calc_bonus_aux(current);
}

static int _weapon_gain_amt(int skill)
{
    static point_t tbl[9] = {
        {0, 1280}, {1000, 640}, {2000, 320}, {3000, 160}, {4000, 80},
        {5000, 40}, {6000, 20}, {7000, 10}, {8000, 1} };
    return interpolate(skill, tbl, 9);
}
static int _weapon_max_skill(int rlvl)
{
    static point_t tbl[5] = { {1, 2000}, {20, 5000}, {30, 6000}, {60, 7500}, {80, 8000} };

    if (rlvl <= 0) return 0;
    return interpolate(rlvl, tbl, 5);
}
static int _weapon_min_rlvl(int plvl)
{
    static point_t tbl[6] = { {20, 1}, {30, 10}, {35, 15}, {40, 25}, {45, 30}, {50, 35} };
    return interpolate(plvl, tbl, 6);
}

void skills_weapon_gain(int tval, int sval, int rlvl)
{
    int max;
    int cur;

    if (plr->pclass == CLASS_SKILLMASTER) return;
    if (tval == TV_SHIELD)
    {
        /*assert(weaponmaster_get_toggle() == TOGGLE_SHIELD_BASH);*/
        skills_shield_gain(sval, rlvl);
        return;
    }

    assert(tv_is_weapon(tval));

    max = skills_weapon_max(tval, sval);
    cur = plr->weapon_exp[tval-TV_WEAPON_BEGIN][sval];

    if (cur < max)
    {
        int step;
        int add;

        if (rlvl < _weapon_min_rlvl(plr->lev))
        {
            if (plr->wizard)
                msg_format("<color:B>You must fight level <color:R>%d</color> monsters to gain weapon proficiency.</color>", _weapon_min_rlvl(plr->lev));
            return;
        }
        if (cur >= _weapon_max_skill(rlvl))
        {
            if (plr->wizard)
            {
                msg_format("<color:B>Against level <color:R>%d</color> foes, you can only train weapon "
                    "proficiency to <color:R>%d</color> (Current Skill: <color:R>%d</color>).</color>",
                    rlvl, _weapon_max_skill(rlvl), cur);
            }
            return;
        }

        step = _weapon_gain_amt(cur);
        add = step / 10;
        if (step%10 && randint0(10) < step%10) add++;

        if (add > 0)
        {
            int old_bonus = _weapon_calc_bonus_aux(cur);
            int new_bonus;
            cur += add;
            if (cur > max)
                cur = max;
            new_bonus = _weapon_calc_bonus_aux(cur);
            plr->weapon_exp[tval-TV_WEAPON_BEGIN][sval] = cur;
            if (old_bonus != new_bonus)
            {
                int k_idx = lookup_kind(tval, sval);
                char buf[MAX_NLEN];
                strip_name(buf, k_idx);
                msg_format("<color:B>Your <color:R>%s</color> skills are improving.</color>", buf);
                plr->update |= PU_BONUS;
            }
        }
    }
}

bool skills_weapon_is_icky(int tval, int sval)
{
    bool result = FALSE;

    assert(tv_is_weapon(tval) || tval == TV_BOW);

    /* Some classes use weapon skill tables to determine allowable weapons.
       But if the character gains the Weapon Versatility ability, all weapons
       will be masterable, even "icky" ones ... */
    switch (plr_pseudo_class_id())
    {
    case CLASS_MONK:
    case CLASS_MYSTIC:
    case CLASS_FORCETRAINER:
        if (s_info[plr->pclass].w_max[tval-TV_WEAPON_BEGIN][sval] == WEAPON_EXP_UNSKILLED)
            result = TRUE;
        break;

    case CLASS_NINJA:
    case CLASS_DUELIST:
        if (s_info[plr->pclass].w_max[tval-TV_WEAPON_BEGIN][sval] <= WEAPON_EXP_BEGINNER)
            result = TRUE;
        break;
    case CLASS_SKILLMASTER:
        return skillmaster_weapon_is_icky(tval);
    }
    return result;
}

static cptr _weapon_describe_aux(int skill, int max)
{
    cptr        desc;
    static char buf[MAX_NLEN];

    buf[0] = '\0';

    if (skill < WEAPON_EXP_BEGINNER)
        desc = "Unskilled";
    else if (skill < WEAPON_EXP_SKILLED)
        desc = "Beginner";
    else if (skill < WEAPON_EXP_EXPERT)
        desc = "Skilled";
    else if (skill < WEAPON_EXP_MASTER)
        desc = "Expert";
    else
        desc = "Master";

    if (skill == max)
    {
        sprintf(buf, "!%s", desc);
        return buf;
    }
    return desc;
}

cptr skills_weapon_describe_current(int tval, int sval)
{
    return _weapon_describe_aux(
        skills_weapon_current(tval, sval),
        skills_weapon_max(tval, sval));
}

cptr skills_martial_arts_describe_current(void)
{
    return _weapon_describe_aux(
        skills_martial_arts_current(), 
        skills_martial_arts_max());
}

cptr skills_bow_describe_current(int sval)
{
    return _weapon_describe_aux(
        skills_bow_current(sval),
        skills_bow_max(sval));
}

/* Shieldmasters 'Shield Bash' technique */
static cptr skills_shield_calc_name(int sval)
{
    static char buf[MAX_NLEN];
    sprintf(buf, "Shield.%d", sval);
    return buf;
}

int skills_shield_current(int sval)
{
    return skills_innate_current(
        skills_shield_calc_name(sval));
}

void skills_shield_init(int sval, int current, int max)
{
    skills_innate_init(
        skills_shield_calc_name(sval), current, max);
}

int skills_shield_max(int sval)
{
    return skills_innate_max(
        skills_shield_calc_name(sval));
}

void skills_shield_gain(int sval, int rlvl)
{
    skills_innate_gain(
        skills_shield_calc_name(sval), rlvl);
}

int skills_shield_calc_bonus(int sval)
{
    return _weapon_calc_bonus_aux(
        skills_shield_current(sval));
}

cptr skills_shield_describe_current(int sval)
{
    return _weapon_describe_aux(
        skills_shield_current(sval),
        skills_shield_max(sval));
}

void skills_martial_arts_gain(void)
{
    int current, max;

    if (plr->pclass == CLASS_SKILLMASTER) return;

    current = plr->skill_exp[SKILL_MARTIAL_ARTS];
    max = s_info[_class_idx()].s_max[SKILL_MARTIAL_ARTS];

    if (current < max)
    {
        if (current < WEAPON_EXP_BEGINNER)
            current += 40;
        else if (current < WEAPON_EXP_SKILLED)
            current += 5;
        else if (current < WEAPON_EXP_EXPERT && plr->lev > 19)
            current += 1;
        else if (plr->lev > 34 && one_in_(3))
            current += 1;

        plr->skill_exp[SKILL_MARTIAL_ARTS] = MIN(current, max);
        plr->update |= PU_BONUS;
    }
}

int skills_martial_arts_current(void)
{
    if (plr->pclass == CLASS_SKILLMASTER)
        return skillmaster_martial_arts_prof();
    else
    {
        int current = plr->skill_exp[SKILL_MARTIAL_ARTS];
        int max = s_info[_class_idx()].s_max[SKILL_MARTIAL_ARTS];
        return MIN(current, max);
    }
}

int skills_martial_arts_max(void)
{
    if (plr->pclass == CLASS_SKILLMASTER)
        return skillmaster_martial_arts_prof();
    return s_info[_class_idx()].s_max[SKILL_MARTIAL_ARTS];
}

void skills_dual_wielding_gain(monster_race *r_ptr)
{
    int current, max;

    if (plr->pclass == CLASS_SKILLMASTER) return;

    current = plr->skill_exp[SKILL_DUAL_WIELDING];
    max = s_info[_class_idx()].s_max[SKILL_DUAL_WIELDING];

    if (current < max && (current - 1000) / 200 < r_ptr->alloc.lvl)
    {
        if (current < WEAPON_EXP_BEGINNER)
            current += 80;
        else if (current < WEAPON_EXP_SKILLED)
            current += 4;
        else if (current < WEAPON_EXP_EXPERT)
            current += 1;
        else if (current < WEAPON_EXP_MASTER && one_in_(3))
            current += 1;

        plr->skill_exp[SKILL_DUAL_WIELDING] = MIN(current, max);
        plr->update |= PU_BONUS;
    }
}

int skills_dual_wielding_current(void)
{
    if (plr->pclass == CLASS_SKILLMASTER)
        return skillmaster_dual_wielding_prof();
    else
    {
        int current = plr->skill_exp[SKILL_DUAL_WIELDING];
        int max = s_info[_class_idx()].s_max[SKILL_DUAL_WIELDING];
        return MIN(current, max);
    }
}

int skills_dual_wielding_max(void)
{
    if (plr->pclass == CLASS_SKILLMASTER)
        return skillmaster_dual_wielding_prof();
    return s_info[_class_idx()].s_max[SKILL_DUAL_WIELDING];
}

static void _skills_riding_gain(int inc)
{
    int current, max, update;

    if (plr->pclass == CLASS_SKILLMASTER) return;

    current = plr->skill_exp[SKILL_RIDING];
    max = skills_riding_max();
    update = MIN(max, current + inc);

    if (update > current)
    {
        plr->skill_exp[SKILL_RIDING] = update;
        plr->update |= PU_BONUS;

        /* Give some feedback every 100 points */
        if (update/100 > current/100)
        {
            if (update <= 500)
                cmsg_print(TERM_L_BLUE, "You are starting to get the hang of riding. Keep at it!");
            else if (update <= 1000)
                cmsg_print(TERM_L_BLUE, "You feel more comfortable while riding.");
            else if (update <= 2000)
                cmsg_print(TERM_L_BLUE, "You feel your riding improve.");
            else if (update <= 5000)
                cmsg_print(TERM_L_BLUE, "You are getting quite good at riding.");
            else
                cmsg_print(TERM_L_BLUE, "Soon you will be a riding master!");
        }
    }
}

void skills_riding_gain_melee(monster_race *r_ptr)
{
    int current, max;

    if (plr->pclass == CLASS_SKILLMASTER) return;

    current = plr->skill_exp[SKILL_RIDING];
    max = skills_riding_max();

    assert(plr->riding);

    if (current < max)
    {
        int ridinglevel = plr_riding_lvl();
        int inc = 0;

        if (current/200 - 5 < r_ptr->alloc.lvl)
            inc += 1;

        if (current/100 < ridinglevel)
        {
            if (current/100 + 15 < ridinglevel)
                inc += 1 + ridinglevel - (current/100 + 15);
            else
                inc += 1;
        }

        if (inc)
            _skills_riding_gain(inc);
    }
}

void skills_riding_gain_rakuba(int dam)
{
    int current, max;

    if (plr->pclass == CLASS_SKILLMASTER) return;

    current = plr->skill_exp[SKILL_RIDING];
    max = skills_riding_max();

    assert(plr->riding);

    if (current < max && max > 1000)
    {
        int ridinglevel = plr_riding_lvl();
        int inc = 0;

        if (dam/2 + ridinglevel > current/30 + 10)
        {
            if (ridinglevel > current/100 + 15)
                inc += 1 + ridinglevel - current/100 - 15;
            else
                inc += 1;

            _skills_riding_gain(inc);
        }
    }
}

void skills_riding_gain_archery(monster_race *r_ptr)
{
    int current, max;

    if (plr->pclass == CLASS_SKILLMASTER) return;

    current = plr->skill_exp[SKILL_RIDING];
    max = skills_riding_max();

    assert(plr->riding);

    if (current < max)
    {
        int ridinglevel = plr_riding_lvl();

        if ((current - RIDING_EXP_BEGINNER*2) / 200 < ridinglevel && one_in_(2))
        {
            _skills_riding_gain(1);
        }
    }
}

int skills_riding_current(void)
{
    if (plr->pclass == CLASS_SKILLMASTER)
        return skillmaster_riding_prof();
    else
    {
        int current = plr->skill_exp[SKILL_RIDING];
        int max = skills_riding_max();
        if (plr->prace == RACE_MON_RING)
            return RIDING_EXP_MASTER;
        return MIN(current, max);
    }
}

int skills_riding_max(void)
{
    if (warlock_is_(WARLOCK_DRAGONS))
        return RIDING_EXP_MASTER;
    if (plr->pclass == CLASS_SKILLMASTER)
        return skillmaster_riding_prof();
    return s_info[_class_idx()].s_max[SKILL_RIDING];
}

/* Innate Attacks
 * Note: The Possessor and The Mimic may need to learn a large number of
 * innate attacks, and I'm thinking of making the proficiency depend on the
 * body type as well as the attack type (So a Dragon's Bite requires different
 * skill than a Tiger's Bite). For other classes, the number of possible forms
 * to learn is small, and and this implementation may seem like so much overkill.
 *
 * Note: You need not call init to use innate skills. But failing to do so
 * means you begin as unskilled. My thoughts are that normal guys like dragons
 * will init their skills on birth, beginning life as a Beginner. This makes sense,
 * since they have had time to grow up and are used to their body. But possessors
 * and mimics will not init their skills as they change bodies, and this means
 * that they always need to spend some time learning the new form. Again, this
 * makes sense.
 */
struct _skill_info_s
{
    int current;
    int max;
};

typedef struct _skill_info_s  _skill_info_t;
typedef struct _skill_info_s *_skill_info_ptr;

static str_map_ptr _innate_map(void)
{
    static str_map_ptr _map = NULL;
    if (!_map)
        _map = str_map_alloc(free);
    return _map;
}

static _skill_info_ptr _innate_info(cptr name)
{
    _skill_info_ptr result = (_skill_info_ptr)str_map_find(_innate_map(), name);
    return result;
}

void skills_innate_init(cptr name, int current, int max)
{
    _skill_info_ptr info = _innate_info(name);
    if (!info)
    {
        info = malloc(sizeof(_skill_info_t));
        str_map_add(_innate_map(), name, info);
    }
    info->current = current;
    info->max = max;
}

int skills_innate_max(cptr name)
{
    _skill_info_ptr info = _innate_info(name);
    int             result = WEAPON_EXP_MASTER;

    if (info)
        result = info->max;

    return result;
}

int skills_innate_current(cptr name)
{
    _skill_info_ptr info = _innate_info(name);
    int             result = WEAPON_EXP_UNSKILLED;

    if (info)
        result = info->current;

    return result;
}

static int _innate_calc_bonus_aux(int skill)
{
    int bonus = (skill - WEAPON_EXP_BEGINNER) / 200; /* -20 to +20 */
    return bonus;
}

int skills_innate_calc_bonus(cptr name)
{
    int current = skills_innate_current(name);
    return _innate_calc_bonus_aux(current);
}

void skills_innate_gain(cptr name, int rlvl)
{
    _skill_info_ptr info = _innate_info(name);

    /* Double Check initialization */
    if (!info)
    {
        info = malloc(sizeof(_skill_info_t));
        info->current = WEAPON_EXP_UNSKILLED;
        info->max = WEAPON_EXP_MASTER;
        str_map_add(_innate_map(), name, info);
    }

    if (info->current < info->max)
    {
        int step;
        int add;

        if (rlvl < _weapon_min_rlvl(plr->lev))
        {
            if (plr->wizard)
                msg_format("<color:B>You must fight level <color:R>%d</color> monsters to gain weapon proficiency.</color>", _weapon_min_rlvl(plr->lev));
            return;
        }

        if (info->current >= _weapon_max_skill(rlvl))
        {
            if (plr->wizard)
            {
                msg_format("<color:B>Against level <color:R>%d</color> foes, you can only train weapon "
                    "proficiency to <color:R>%d</color> (Current Skill: <color:R>%d</color>).</color>",
                    rlvl, _weapon_max_skill(rlvl), info->current);
            }
            return;
        }

        step = _weapon_gain_amt(info->current);
        add = step / 10;
        if (step%10 && randint0(10) < step%10) add++;

        if (add > 0)
        {
            int old_bonus = _innate_calc_bonus_aux(info->current);
            int new_bonus;
            info->current += add;
            if (info->current > info->max)
                info->current = info->max;
            new_bonus = _innate_calc_bonus_aux(info->current);
            if (old_bonus != new_bonus)
            {
                msg_format("<color:B>Your <color:R>%s</color> skills are improving.</color>", name);
                /*plr->update |= PU_BONUS;*/
            }
        }
    }
}

cptr skills_innate_calc_name(mon_blow_ptr blow)
{
    static char buf[MAX_NLEN];
    cptr name = blow->name;
    buf[0] = '\0';
    if (!name)
        name = mon_blow_info_lookup(blow->method)->name;
    if (blow->flags & MBF_POSSESSOR)
    {
        mon_race_ptr race = plr_mon_race();
        if (mon_race_is_unique(race))
            sprintf(buf, "%s.%s", sym_str(race->id), name); /* XXX race->id can change */
        else
            sprintf(buf, "%c.%s", mon_race_char(race), name);
    }
    else
        sprintf(buf, "%s", name);
    return buf;
}

cptr skills_innate_describe_current(cptr name)
{
    int         current = skills_innate_current(name);
    cptr        desc;
    static char buf[MAX_NLEN];

    buf[0] = '\0';

    if (current < WEAPON_EXP_BEGINNER)
        desc = "Unskilled";
    else if (current < WEAPON_EXP_SKILLED)
        desc = "Beginner";
    else if (current < WEAPON_EXP_EXPERT)
        desc = "Skilled";
    else if (current < WEAPON_EXP_MASTER)
        desc = "Expert";
    else
        desc = "Master";

    if (current == skills_innate_max(name))
    {
        sprintf(buf, "!%s", desc);
        return buf;
    }
    return desc;
}

void skills_on_load(savefile_ptr file)
{
    str_map_ptr map = _innate_map();
    int         ct, i;

    str_map_clear(map);
    ct = savefile_read_s32b(file);

    for (i = 0; i < ct; i++)
    {
        char            name[255];
        _skill_info_ptr info = malloc(sizeof(_skill_info_t));

        savefile_read_cptr(file, name, sizeof(name));
        info->current = savefile_read_s32b(file);
        info->max = savefile_read_s32b(file);

        str_map_add(map, name, info);
    }

    /* TODO: Spell Skills for Bookless Casters */
    ct = savefile_read_s32b(file);
}

void skills_on_save(savefile_ptr file)
{
    str_map_ptr         map = _innate_map();
    str_map_iter_ptr    iter;

    savefile_write_s32b(file, str_map_count(map));

    for (iter = str_map_iter_alloc(map);
            str_map_iter_is_valid(iter);
            str_map_iter_next(iter))
    {
        cptr            name = str_map_iter_current_key(iter);
        _skill_info_ptr info = (_skill_info_ptr)str_map_iter_current(iter);

        savefile_write_cptr(file, name);
        savefile_write_s32b(file, info->current);
        savefile_write_s32b(file, info->max);
    }
    str_map_iter_free(iter);

    /* TODO: Spell Skills for Bookless Casters */
    savefile_write_s32b(file, 0);
}

void skills_on_birth(void)
{
    int i, j;
    int class_idx = _class_idx();
    for (i = 0; i < 5; i++)
    {
        for (j = 0; j < 64; j++)
        {
            if (i == TV_BOW-TV_WEAPON_BEGIN && demigod_is_(DEMIGOD_ARTEMIS))
                plr->weapon_exp[i][j] = WEAPON_EXP_BEGINNER;
            else if (demigod_is_(DEMIGOD_ARES))
                plr->weapon_exp[i][j] = WEAPON_EXP_BEGINNER;
            else
                plr->weapon_exp[i][j] = s_info[class_idx].w_start[i][j];
        }
    }
    if (plr->personality == PERS_SEXY)
        plr->weapon_exp[TV_HAFTED-TV_WEAPON_BEGIN][SV_WHIP] = MAX(WEAPON_EXP_BEGINNER, plr->weapon_exp[TV_HAFTED-TV_WEAPON_BEGIN][SV_WHIP]);
    for (i = 0; i < 10; i++)
        plr->skill_exp[i] = s_info[class_idx].s_start[i];

    if (warlock_is_(WARLOCK_DRAGONS))
        plr->skill_exp[SKILL_RIDING] = RIDING_EXP_BEGINNER;

    str_map_clear(_innate_map());
}

/*************************************************************************
 * Human Readable Skill Descriptions (Used for helpfiles and such)
 ************************************************************************/
static cptr _skill_desc(int amt, int div)
{
    static char buf[255];
    skill_desc_t desc = skills_describe(amt, div);
    sprintf(buf, "<color:%c>%-10.10s</color>", attr_to_attr_char(desc.color), desc.desc);
    return buf;
}

/* Disarming */
static cptr _dis_skill_desc(int base, int xtra) { return _skill_desc(base + xtra - 40, 8); }
static cptr _class_dis_skill_desc(class_t *class_ptr) { return _dis_skill_desc(class_ptr->skills.dis, class_ptr->extra_skills.dis); }
static cptr _mon_race_dis_skill_desc(race_t *race_ptr) { return _dis_skill_desc(race_ptr->skills.dis, race_ptr->extra_skills.dis); }

static cptr _dis_skill_desc2(int base) { return _skill_desc(base + 5, 2); }
static cptr _race_dis_skill_desc(race_t *race_ptr) { return _dis_skill_desc2(race_ptr->skills.dis); }
static cptr _pers_dis_skill_desc(personality_ptr pers_ptr) { return _dis_skill_desc2(pers_ptr->skills.dis*2); }
static cptr _realm_dis_skill_desc(dragon_realm_ptr realm_ptr) { return _skill_desc(realm_ptr->skills.dis + 10, 2); }

/* Devices */
static cptr _dev_skill_desc(int base, int xtra) { return _skill_desc(base + xtra - 50, 6); }
static cptr _class_dev_skill_desc(class_t *class_ptr) { return _dev_skill_desc(class_ptr->skills.dev, class_ptr->extra_skills.dev); }
static cptr _mon_race_dev_skill_desc(race_t *race_ptr) { return _dev_skill_desc(race_ptr->skills.dev, race_ptr->extra_skills.dev); }

static cptr _dev_skill_desc2(int base) { return _skill_desc(base + 5, 2); }
static cptr _race_dev_skill_desc(race_t *race_ptr) { return _dev_skill_desc2(race_ptr->skills.dev); }
static cptr _pers_dev_skill_desc(personality_ptr pers_ptr) { return _dev_skill_desc2(pers_ptr->skills.dev*2); }
static cptr _realm_dev_skill_desc(dragon_realm_ptr realm_ptr) { return _skill_desc(realm_ptr->skills.dev + 5, 1); }

/* Saving Throws */
static cptr _sav_skill_desc(int base, int xtra) { return _skill_desc(base + xtra - 65, 5); }
static cptr _class_sav_skill_desc(class_t *class_ptr) { return _sav_skill_desc(class_ptr->skills.sav, class_ptr->extra_skills.sav); }
static cptr _mon_race_sav_skill_desc(race_t *race_ptr) { return _sav_skill_desc(race_ptr->skills.sav, race_ptr->extra_skills.sav); }

static cptr _sav_skill_desc2(int base) { return _skill_desc(base + 5, 2); }
static cptr _race_sav_skill_desc(race_t *race_ptr) { return _sav_skill_desc2(race_ptr->skills.sav); }
static cptr _pers_sav_skill_desc(personality_ptr pers_ptr) { return _sav_skill_desc2(pers_ptr->skills.sav*2); }
static cptr _realm_sav_skill_desc(dragon_realm_ptr realm_ptr) { return _skill_desc(realm_ptr->skills.sav + 5, 1); }

/* Melee */
static cptr _thn_skill_desc(int base, int xtra) { return _skill_desc(base + xtra - 70, 12); }
static cptr _class_thn_skill_desc(class_t *class_ptr) { return _thn_skill_desc(class_ptr->skills.thn, class_ptr->extra_skills.thn); }
static cptr _mon_race_thn_skill_desc(race_t *race_ptr) { return _thn_skill_desc(race_ptr->skills.thn, race_ptr->extra_skills.thn); }

static cptr _thn_skill_desc2(int base) { return _skill_desc(base + 5, 2); }
static cptr _race_thn_skill_desc(race_t *race_ptr) { return _thn_skill_desc2(race_ptr->skills.thn); }
static cptr _pers_thn_skill_desc(personality_ptr pers_ptr) { return _thn_skill_desc2(pers_ptr->skills.thn*2); }
static cptr _realm_thn_skill_desc(dragon_realm_ptr realm_ptr) { return _skill_desc(realm_ptr->skills.thn + 10, 2); }

/* Bows */
static cptr _thb_skill_desc(int base, int xtra) { return _skill_desc(base + xtra - 60, 12); }
static cptr _class_thb_skill_desc(class_t *class_ptr) { return _thb_skill_desc(class_ptr->skills.thb, class_ptr->extra_skills.thb); }
static cptr _mon_race_thb_skill_desc(race_t *race_ptr) { return _thb_skill_desc(race_ptr->skills.thb, race_ptr->extra_skills.thb); }

static cptr _thb_skill_desc2(int base) { return _skill_desc(base + 5, 2); }
static cptr _race_thb_skill_desc(race_t *race_ptr) { return _thb_skill_desc2(race_ptr->skills.thb); }
static cptr _pers_thb_skill_desc(personality_ptr pers_ptr) { return _thb_skill_desc2(pers_ptr->skills.thb*2); }
static cptr _realm_thb_skill_desc(dragon_realm_ptr realm_ptr) { return _skill_desc(realm_ptr->skills.thb + 10, 2); }

/* Stealth */
static cptr _stl_skill_desc(int base, int xtra) { return _skill_desc(base + xtra + 2, 1); }
static cptr _class_stl_skill_desc(class_t *class_ptr) { return _stl_skill_desc(class_ptr->skills.stl, class_ptr->extra_skills.stl); }
static cptr _mon_race_stl_skill_desc(race_t *race_ptr) { return _stl_skill_desc(race_ptr->skills.stl, race_ptr->extra_skills.stl); }
static cptr _race_stl_skill_desc(race_t *race_ptr) { return _stl_skill_desc(race_ptr->skills.stl, 0); }
static cptr _pers_stl_skill_desc(personality_ptr pers_ptr) { return _stl_skill_desc(pers_ptr->skills.stl, 0); }
static cptr _realm_stl_skill_desc(dragon_realm_ptr realm_ptr) { return _skill_desc(realm_ptr->skills.stl + 4, 1); }

/* Searching */
static cptr _srh_skill_desc(int base, int xtra) { return _skill_desc(base + xtra, 6); }
static cptr _class_srh_skill_desc(class_t *class_ptr) { return _srh_skill_desc(class_ptr->skills.srh, class_ptr->extra_skills.srh); }
static cptr _mon_race_srh_skill_desc(race_t *race_ptr) { return _srh_skill_desc(race_ptr->skills.srh, race_ptr->extra_skills.srh); }

static cptr _srh_skill_desc2(int base) { return _skill_desc(base, 1); }
static cptr _race_srh_skill_desc(race_t *race_ptr) { return _srh_skill_desc2(race_ptr->skills.srh); }
static cptr _pers_srh_skill_desc(personality_ptr pers_ptr) { return _srh_skill_desc2(pers_ptr->skills.srh + 3); }
static cptr _realm_srh_skill_desc(dragon_realm_ptr realm_ptr) { return _skill_desc(realm_ptr->skills.srh, 1); }

/* Perception */
static cptr _fos_skill_desc(int base, int xtra) { return _skill_desc(base + xtra, 6); }
static cptr _class_fos_skill_desc(class_t *class_ptr) { return _fos_skill_desc(class_ptr->skills.fos, class_ptr->extra_skills.fos); }
static cptr _mon_race_fos_skill_desc(race_t *race_ptr) { return _fos_skill_desc(race_ptr->skills.fos, race_ptr->extra_skills.fos); }

static cptr _fos_skill_desc2(int base) { return _skill_desc(base, 1); }
static cptr _race_fos_skill_desc(race_t *race_ptr) { return _fos_skill_desc2(race_ptr->skills.fos); }
static cptr _pers_fos_skill_desc(personality_ptr pers_ptr) { return _fos_skill_desc2(pers_ptr->skills.fos + 3); }
static cptr _realm_fos_skill_desc(dragon_realm_ptr realm_ptr) { return _skill_desc(realm_ptr->skills.fos, 1); }


void skills_desc_class(class_t *class_ptr, skills_desc_t *skills)
{
    strcpy(skills->dis, _class_dis_skill_desc(class_ptr));
    strcpy(skills->dev, _class_dev_skill_desc(class_ptr));
    strcpy(skills->sav, _class_sav_skill_desc(class_ptr));
    strcpy(skills->stl, _class_stl_skill_desc(class_ptr));
    strcpy(skills->srh, _class_srh_skill_desc(class_ptr));
    strcpy(skills->fos, _class_fos_skill_desc(class_ptr));
    strcpy(skills->thn, _class_thn_skill_desc(class_ptr));
    strcpy(skills->thb, _class_thb_skill_desc(class_ptr));
}

void skills_desc_mon_race(race_t *race_ptr, skills_desc_t *skills)
{
    strcpy(skills->dis, _mon_race_dis_skill_desc(race_ptr));
    strcpy(skills->dev, _mon_race_dev_skill_desc(race_ptr));
    strcpy(skills->sav, _mon_race_sav_skill_desc(race_ptr));
    strcpy(skills->stl, _mon_race_stl_skill_desc(race_ptr));
    strcpy(skills->srh, _mon_race_srh_skill_desc(race_ptr));
    strcpy(skills->fos, _mon_race_fos_skill_desc(race_ptr));
    strcpy(skills->thn, _mon_race_thn_skill_desc(race_ptr));
    strcpy(skills->thb, _mon_race_thb_skill_desc(race_ptr));
}

void skills_desc_race(race_t *race_ptr, skills_desc_t *skills)
{
    strcpy(skills->dis, _race_dis_skill_desc(race_ptr));
    strcpy(skills->dev, _race_dev_skill_desc(race_ptr));
    strcpy(skills->sav, _race_sav_skill_desc(race_ptr));
    strcpy(skills->stl, _race_stl_skill_desc(race_ptr));
    strcpy(skills->srh, _race_srh_skill_desc(race_ptr));
    strcpy(skills->fos, _race_fos_skill_desc(race_ptr));
    strcpy(skills->thn, _race_thn_skill_desc(race_ptr));
    strcpy(skills->thb, _race_thb_skill_desc(race_ptr));
}

void skills_desc_pers(personality_t *pers_ptr, skills_desc_t *skills)
{
    strcpy(skills->dis, _pers_dis_skill_desc(pers_ptr));
    strcpy(skills->dev, _pers_dev_skill_desc(pers_ptr));
    strcpy(skills->sav, _pers_sav_skill_desc(pers_ptr));
    strcpy(skills->stl, _pers_stl_skill_desc(pers_ptr));
    strcpy(skills->srh, _pers_srh_skill_desc(pers_ptr));
    strcpy(skills->fos, _pers_fos_skill_desc(pers_ptr));
    strcpy(skills->thn, _pers_thn_skill_desc(pers_ptr));
    strcpy(skills->thb, _pers_thb_skill_desc(pers_ptr));
}

void skills_desc_realm(dragon_realm_ptr realm_ptr, skills_desc_t *skills)
{
    strcpy(skills->dis, _realm_dis_skill_desc(realm_ptr));
    strcpy(skills->dev, _realm_dev_skill_desc(realm_ptr));
    strcpy(skills->sav, _realm_sav_skill_desc(realm_ptr));
    strcpy(skills->stl, _realm_stl_skill_desc(realm_ptr));
    strcpy(skills->srh, _realm_srh_skill_desc(realm_ptr));
    strcpy(skills->fos, _realm_fos_skill_desc(realm_ptr));
    strcpy(skills->thn, _realm_thn_skill_desc(realm_ptr));
    strcpy(skills->thb, _realm_thb_skill_desc(realm_ptr));
}

void skills_desc_aux(skills_t *base, skills_t *xtra, skills_desc_t *skills)
{
    strcpy(skills->dis, _dis_skill_desc(base->dis, xtra->dis));
    strcpy(skills->dev, _dev_skill_desc(base->dev, xtra->dev));
    strcpy(skills->sav, _sav_skill_desc(base->sav, xtra->sav));
    strcpy(skills->stl, _stl_skill_desc(base->stl, xtra->stl));
    strcpy(skills->srh, _srh_skill_desc(base->srh, xtra->srh));
    strcpy(skills->fos, _fos_skill_desc(base->fos, xtra->fos));
    strcpy(skills->thn, _thn_skill_desc(base->thn, xtra->thn));
    strcpy(skills->thb, _thb_skill_desc(base->thb, xtra->thb));
}

