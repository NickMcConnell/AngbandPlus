#include "angband.h"

skill_table *s_info;

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
    int max = skills_weapon_max(TV_BOW, sval);
    int cur = p_ptr->weapon_exp[0][sval];

    if (cur > max)
        cur = max;

    return cur;
}

int skills_bow_max(int sval)
{
    if (mut_present(MUT_WEAPON_SKILLS))
        return WEAPON_EXP_MASTER;

    if (p_ptr->prace == RACE_DEMIGOD && p_ptr->psubrace == DEMIGOD_ARTEMIS)
        return WEAPON_EXP_MASTER;

    return s_info[p_ptr->pclass].w_max[0][sval];
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
    /* Hack: In case somebody calls this instead of skills_bow_max() */
    if (tval == TV_BOW && p_ptr->prace == RACE_DEMIGOD && p_ptr->psubrace == DEMIGOD_ARTEMIS)
        return WEAPON_EXP_MASTER;

    if (mut_present(MUT_WEAPON_SKILLS))
        return WEAPON_EXP_MASTER;

    return s_info[p_ptr->pclass].w_max[tval-TV_WEAPON_BEGIN][sval];
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
        if (s_info[p_ptr->pclass].w_max[tval-TV_WEAPON_BEGIN][sval] <= WEAPON_EXP_BEGINNER)
            result = TRUE;
        break;
    }
    return result;
}