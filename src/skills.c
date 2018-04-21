#include "angband.h"

#include <assert.h>
#include "str-map.h"

skill_table *s_info;

static int _class_idx(void)
{
    int result = p_ptr->pclass;
    if (result == CLASS_MONSTER)
    {
        race_t *race_ptr = get_race();
        result = race_ptr->pseudo_class_idx;
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

void skills_init(skills_t *dest)
{
    dest->dis = 0;
    dest->dev = 0;
    dest->sav = 0;
    dest->stl = 0;
    dest->srh = 0;
    dest->fos = 0;
    dest->thn = 0;
    dest->thb = 0;
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
            result.desc = format("Legendary[%d]", k);
            result.color = TERM_VIOLET;
            break;
        }
        }
    }
    return result;
}

int skills_bow_current(int sval)
{
    int max = skills_bow_max(sval);
    int cur = p_ptr->weapon_exp[0][sval];

    if (cur > max)
        cur = max;

    return cur;
}

int skills_bow_max(int sval)
{
    if (mut_present(MUT_WEAPON_SKILLS))
        return WEAPON_EXP_MASTER;

    if (demigod_is_(DEMIGOD_ARTEMIS))
        return WEAPON_EXP_MASTER;

    return s_info[_class_idx()].w_max[0][sval];
}

void skills_bow_gain(int sval)
{
    int max = skills_bow_max(sval);
    int cur = p_ptr->weapon_exp[0][sval];

    if (cur < max)
    {
        int add = 0;
        
        if (cur < WEAPON_EXP_BEGINNER) add = 80;
        else if (cur < WEAPON_EXP_SKILLED) add = 25;
        else if (cur < WEAPON_EXP_EXPERT && p_ptr->lev > 19) add = 10;
        else if (p_ptr->lev > 34) add = 2;

        if (add > 0)
        {
            cur += add;
            if (cur > max)
                cur = max;
            p_ptr->weapon_exp[0][sval] += add;
            p_ptr->update |= (PU_BONUS);
        }
    }
}

int skills_weapon_current(int tval, int sval)
{
    int max;
    int cur;

    assert(TV_WEAPON_BEGIN <= tval && tval <= TV_WEAPON_END);

    if (tval == TV_BOW)
        return skills_bow_current(sval);

    max = skills_weapon_max(tval, sval);
    cur = p_ptr->weapon_exp[tval-TV_WEAPON_BEGIN][sval];

    if (cur > max)
        cur = max;

    return cur;
}

int skills_weapon_max(int tval, int sval)
{
    assert(TV_WEAPON_BEGIN <= tval && tval <= TV_WEAPON_END);

    if (tval == TV_BOW)
        return skills_bow_max(sval);

    if (mut_present(MUT_WEAPON_SKILLS))
        return WEAPON_EXP_MASTER;

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

    return s_info[_class_idx()].w_max[tval-TV_WEAPON_BEGIN][sval];
}

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

void skills_weapon_gain(int tval, int sval)
{
    int max;
    int cur;

    assert(TV_WEAPON_BEGIN <= tval && tval <= TV_WEAPON_END);
    assert(tval != TV_BOW);

    max = skills_weapon_max(tval, sval);
    cur = p_ptr->weapon_exp[tval-TV_WEAPON_BEGIN][sval];

    if (cur < max)
    {
        int add = 0;
        
        if (cur < WEAPON_EXP_BEGINNER) add = 80;
        else if (cur < WEAPON_EXP_SKILLED) add = 10;
        else if (cur < WEAPON_EXP_EXPERT && p_ptr->lev > 19) add = 1;
        else if (p_ptr->lev > 34 && one_in_(2)) add = 1;

        if (add > 0)
        {
            int old_bonus = _weapon_calc_bonus_aux(cur);
            int new_bonus;
            cur += add;
            if (cur > max)
                cur = max;
            new_bonus = _weapon_calc_bonus_aux(cur);
            p_ptr->weapon_exp[tval-TV_WEAPON_BEGIN][sval] += add;
            if (old_bonus != new_bonus)
                p_ptr->update |= PU_BONUS;
        }
    }
}

bool skills_weapon_is_icky(int tval, int sval)
{
    bool result = FALSE;

    assert(TV_WEAPON_BEGIN <= tval && tval <= TV_WEAPON_END);

    /* Some classes use weapon skill tables to determine allowable weapons.
       But if the character gains the Weapon Versatility ability, all weapons
       will be masterable, even "icky" ones ... */
    switch (p_ptr->pclass)
    {
    case CLASS_MONK:
    case CLASS_MYSTIC:
    case CLASS_FORCETRAINER:
        if (s_info[p_ptr->pclass].w_max[tval-TV_WEAPON_BEGIN][sval] == WEAPON_EXP_UNSKILLED)
            result = TRUE;
        break;

    case CLASS_NINJA:
    case CLASS_DUELIST:
        if (s_info[p_ptr->pclass].w_max[tval-TV_WEAPON_BEGIN][sval] <= WEAPON_EXP_BEGINNER)
            result = TRUE;
        break;
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

int skills_shield_max(int sval)
{
    return skills_innate_max(
        skills_shield_calc_name(sval));
}

void skills_shield_gain(int sval)
{
    skills_innate_gain(
        skills_shield_calc_name(sval));
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
    int current = p_ptr->skill_exp[SKILL_MARTIAL_ARTS];
    int max = s_info[_class_idx()].s_max[SKILL_MARTIAL_ARTS];
    
    if (current < max)
    {
        if (current < WEAPON_EXP_BEGINNER)
            current += 40;
        else if (current < WEAPON_EXP_SKILLED)
            current += 5;
        else if (current < WEAPON_EXP_EXPERT && p_ptr->lev > 19)
            current += 1;
        else if (p_ptr->lev > 34 && one_in_(3)) 
            current += 1;

        p_ptr->skill_exp[SKILL_MARTIAL_ARTS] = MIN(current, max);
        p_ptr->update |= PU_BONUS;
    }
}

int skills_martial_arts_current(void)
{
    int current = p_ptr->skill_exp[SKILL_MARTIAL_ARTS];
    int max = s_info[_class_idx()].s_max[SKILL_MARTIAL_ARTS];
    return MIN(current, max);
}

int skills_martial_arts_max(void)
{
    return s_info[_class_idx()].s_max[SKILL_MARTIAL_ARTS];
}

void skills_dual_wielding_gain(monster_race *r_ptr)
{
    int current = p_ptr->skill_exp[SKILL_DUAL_WIELDING];
    int max = s_info[_class_idx()].s_max[SKILL_DUAL_WIELDING];
    
    if (current < max && (current - 1000) / 200 < r_ptr->level)
    {
        if (current < WEAPON_EXP_BEGINNER)
            current += 80;
        else if (current < WEAPON_EXP_SKILLED)
            current += 4;
        else if (current < WEAPON_EXP_EXPERT)
            current += 1;
        else if (current < WEAPON_EXP_MASTER && one_in_(3)) 
            current += 1;

        p_ptr->skill_exp[SKILL_DUAL_WIELDING] = MIN(current, max);
        p_ptr->update |= PU_BONUS;
    }
}

int skills_dual_wielding_current(void)
{
    int current = p_ptr->skill_exp[SKILL_DUAL_WIELDING];
    int max = s_info[_class_idx()].s_max[SKILL_DUAL_WIELDING];
    return MIN(current, max);
}

int skills_dual_wielding_max(void)
{
    return s_info[_class_idx()].s_max[SKILL_DUAL_WIELDING];
}

void skills_riding_gain_melee(monster_race *r_ptr)
{
    int current = p_ptr->skill_exp[SKILL_RIDING];
    int max = s_info[_class_idx()].s_max[SKILL_RIDING];

    assert(p_ptr->riding);

    if (current < max)
    {
        int ridinglevel = r_info[m_list[p_ptr->riding].r_idx].level;
        int inc = 0;

        if (current/200 - 5 < r_ptr->level)
            inc += 1;

        if (current/100 < ridinglevel)
        {
            if (current/100 + 15 < ridinglevel)
                inc += 1 + ridinglevel - (current/100 + 15);
            else
                inc += 1;
        }

        if (inc)
        {
            p_ptr->skill_exp[SKILL_RIDING] = MIN(max, current + inc);
            p_ptr->update |= PU_BONUS;
        }
    }
}

void skills_riding_gain_archery(monster_race *r_ptr)
{
    int current = p_ptr->skill_exp[SKILL_RIDING];
    int max = s_info[_class_idx()].s_max[SKILL_RIDING];

    assert(p_ptr->riding);

    if (current < max)
    {
        int ridinglevel = r_info[m_list[p_ptr->riding].r_idx].level;
        
        if ((current - RIDING_EXP_BEGINNER*2) / 200 < ridinglevel && one_in_(2))
        {
            p_ptr->skill_exp[SKILL_RIDING] += 1;
            p_ptr->update |= PU_BONUS;
        }
    }
}

int skills_riding_current(void)
{
    int current = p_ptr->skill_exp[SKILL_RIDING];
    int max = skills_riding_max();
    if (p_ptr->prace == RACE_MON_RING)
        return RIDING_EXP_MASTER;
    return MIN(current, max);
}

int skills_riding_max(void)
{
    /* Dragon Warlocks are Dragon Riders! */
    if (warlock_is_(WARLOCK_DRAGONS))
        return RIDING_EXP_MASTER;
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

void skills_innate_gain(cptr name)
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
        int add = 0;

        if (info->current < WEAPON_EXP_BEGINNER) add = 80;
        else if (info->current < WEAPON_EXP_SKILLED) add = 10;
        else if (info->current < WEAPON_EXP_EXPERT && p_ptr->lev > 19) add = 1;
        else if (p_ptr->lev > 34 && one_in_(2)) add = 1;

        if (add > 0)
        {
            int old_bonus = _innate_calc_bonus_aux(info->current);
            int new_bonus;
            info->current += add;
            if (info->current > info->max)
                info->current = info->max;
            new_bonus = _innate_calc_bonus_aux(info->current);
            if (old_bonus != new_bonus)
                p_ptr->update |= PU_BONUS;
        }
    }
}

cptr skills_innate_calc_name(innate_attack_ptr attack)
{
    static char buf[MAX_NLEN];
    buf[0] = '\0';
    if ((p_ptr->prace == RACE_MON_POSSESSOR || p_ptr->prace == RACE_MON_MIMIC) && p_ptr->current_r_idx)
    {
        monster_race *r_ptr = &r_info[p_ptr->current_r_idx];
        if (r_ptr->flags1 & RF1_UNIQUE)
            sprintf(buf, "%d.%s", p_ptr->current_r_idx, attack->name);
        else
            sprintf(buf, "%c.%s", r_ptr->d_char, attack->name);
    }
    else
        sprintf(buf, "%s", attack->name);
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
                p_ptr->weapon_exp[i][j] = WEAPON_EXP_BEGINNER;
            else if (demigod_is_(DEMIGOD_ARES))
                p_ptr->weapon_exp[i][j] = WEAPON_EXP_BEGINNER;
            else
                p_ptr->weapon_exp[i][j] = s_info[class_idx].w_start[i][j];
        }
    }
    if (p_ptr->personality == PERS_SEXY)
        p_ptr->weapon_exp[TV_HAFTED-TV_WEAPON_BEGIN][SV_WHIP] = MAX(WEAPON_EXP_BEGINNER, p_ptr->weapon_exp[TV_HAFTED-TV_WEAPON_BEGIN][SV_WHIP]);
    for (i = 0; i < 10; i++)
        p_ptr->skill_exp[i] = s_info[class_idx].s_start[i];

    str_map_clear(_innate_map());
}

