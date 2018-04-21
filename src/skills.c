#include "angband.h"

#include <assert.h>

skill_table *s_info;

static int _class_idx(void)
{
    int result = p_ptr->pclass;
    if (result == CLASS_MONSTER)
    {
        race_t *race_ptr = get_race_t();
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
    int max = skills_weapon_max(tval, sval);
    int cur = p_ptr->weapon_exp[tval-TV_WEAPON_BEGIN][sval];

    if (cur > max)
        cur = max;

    return cur;
}

int skills_weapon_max(int tval, int sval)
{
    if (mut_present(MUT_WEAPON_SKILLS))
        return WEAPON_EXP_MASTER;

    /* Hack: In case somebody calls this instead of skills_bow_max() */
    if (tval == TV_BOW && demigod_is_(DEMIGOD_ARTEMIS))
        return WEAPON_EXP_MASTER;

    return s_info[_class_idx()].w_max[tval-TV_WEAPON_BEGIN][sval];
}

void skills_weapon_gain(int tval, int sval)
{
    int max = skills_weapon_max(tval, sval);
    int cur = p_ptr->weapon_exp[tval-TV_WEAPON_BEGIN][sval];

    if (cur < max)
    {
        int add = 0;
        
        if (cur < WEAPON_EXP_BEGINNER) add = 80;
        else if (cur < WEAPON_EXP_SKILLED) add = 10;
        else if (cur < WEAPON_EXP_EXPERT && p_ptr->lev > 19) add = 1;
        else if (p_ptr->lev > 34 && one_in_(2)) add = 1;

        if (add > 0)
        {
            cur += add;
            if (cur > max)
                cur = max;
            p_ptr->weapon_exp[tval-TV_WEAPON_BEGIN][sval] += add;
            p_ptr->update |= (PU_BONUS);
        }
    }
}

bool skills_weapon_is_icky(int tval, int sval)
{
    bool result = FALSE;

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
    int max = s_info[_class_idx()].s_max[SKILL_RIDING];
    if (p_ptr->prace == RACE_MON_RING)
        return RIDING_EXP_MASTER;
    return MIN(current, max);
}

int skills_riding_max(void)
{
    return s_info[_class_idx()].s_max[SKILL_RIDING];
}
