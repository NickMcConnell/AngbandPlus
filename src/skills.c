#include "angband.h"

#include <assert.h>

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
        case 3:
            result.desc = "Poor";
            result.color = TERM_L_RED;
            break;
        case 4:
        case 5:
            result.desc = "Fair";
            result.color = TERM_ORANGE;
            break;
        case 6:
        case 7:
            result.desc = "Good";
            result.color = TERM_YELLOW;
            break;
        case 8:
            result.desc = "Very Good";
            result.color = TERM_YELLOW;
            break;
        case 9:
        case 10:
            result.desc = "Excellent";
            result.color = TERM_L_GREEN;
            break;
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
            if ((p_ptr->wizard) || (display_skill_num)) result.desc = "Amber";
            else result.desc = format("Amber[%d]", k); /*Legendary is too long for tables */
            result.color = TERM_VIOLET;
            break;
        }
        }
    }
    return result;
}

int skills_bow_current(int sval)
{
    if (p_ptr->pclass == CLASS_SKILLMASTER)
        return skillmaster_bow_prof();
    else
    {
        int prof = tsvals_to_proficiency(TV_BOW, sval);

        int max = p_ptr->proficiency_cap[prof];
        int cur = p_ptr->proficiency[prof];

        if (cur > max)
            cur = max;

        return cur;
    }
}

int skills_bow_max(int sval)
{
    if (mut_present(MUT_WEAPON_SKILLS))
        return WEAPON_EXP_MASTER;

    if (p_ptr->pclass == CLASS_SKILLMASTER)
        return skillmaster_bow_prof();

    int prof = tsvals_to_proficiency(TV_BOW, sval);
    return p_ptr->proficiency_cap[prof];
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

int skills_weapon_current(int prof)
{
    int max;
    int cur;

    if (p_ptr->prace == RACE_MON_ARMOR) return WEAPON_EXP_BEGINNER; /* Skills? What skills? */

    if (p_ptr->pclass == CLASS_SKILLMASTER)
        return skillmaster_weapon_prof(prof);

    cur = p_ptr->proficiency[prof];
    max = p_ptr->proficiency_cap[prof];

    if (cur > max)
        cur = max;

    return cur;
}

int skills_weapon_max(int prof)
{
    int skill = p_ptr->proficiency_cap[prof];

    if (mut_present(MUT_WEAPON_SKILLS))
        return WEAPON_EXP_MASTER;

    if (p_ptr->pclass == CLASS_SKILLMASTER)
        return skillmaster_weapon_prof(prof);

    if (mut_present(MUT_WEAPON_SKILLS))
        return WEAPON_EXP_MASTER;

    return skill;
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

int skills_weapon_calc_bonus(int prof)
{
    int current = p_ptr->proficiency[prof];
    return _weapon_calc_bonus_aux(current);
}

static int _weapon_gain_amt(int skill)
{
    static point_t tbl2[9] =
    { {0, 4480}, {1000, 2880}, {2000, 1600}, {3000, 800}, {4000, 480},
        {5000, 280}, {6000, 180}, {7000, 120}, {8000, 12} }, tbl[9] =
    { {0, 1280}, {1000, 640}, {2000, 320}, {3000, 160}, {4000, 80},
        {5000, 40}, {6000, 20}, {7000, 10}, {8000, 1} };
    return interpolate(skill, coffee_break ? tbl2 : tbl, 9);
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

void skills_weapon_gain(int proficiency, int rlvl)
{
    int max;
    int cur;

    if (p_ptr->prace == RACE_MON_ARMOR) return; /* No skill gain for you */

    if (p_ptr->pclass == CLASS_SKILLMASTER) return;

    max = p_ptr->proficiency_cap[proficiency];
    cur = p_ptr->proficiency[proficiency];

    if (cur < max)
    {
        int step;
        int add;

        if (rlvl < _weapon_min_rlvl(p_ptr->lev))
        {
            if (p_ptr->wizard)
                msg_format("<color:B>You must fight level <color:R>%d</color> monsters to gain weapon proficiency.</color>", _weapon_min_rlvl(p_ptr->lev));
            return;
        }
        if (cur >= _weapon_max_skill(rlvl))
        {
            if (p_ptr->wizard)
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
            p_ptr->proficiency[proficiency] = cur;
            if (old_bonus != new_bonus)
            {
                msg_format("<color:B>Your <color:R>%s</color> skills are improving.</color>", PROFICIENCIES[proficiency]);
                p_ptr->update |= PU_BONUS;
            }
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
        if (p_ptr->proficiency_cap[tsvals_to_proficiency(tval, sval)] == WEAPON_EXP_UNSKILLED) 
            result = TRUE;
        break;

    case CLASS_NINJA:
    case CLASS_NINJA_LAWYER:
    case CLASS_DUELIST:
        if (p_ptr->proficiency_cap[tsvals_to_proficiency(tval, sval)] == WEAPON_EXP_BEGINNER)
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

cptr skills_weapon_describe_current(int prof)
{
    return _weapon_describe_aux(
        skills_weapon_current(prof),
        skills_weapon_max(prof));
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
    int k_idx = lookup_kind(TV_SHIELD, sval);
    strip_name(buf, k_idx);
    return buf;
}

int skills_shield_calc_bonus(int sval)
{
    return _weapon_calc_bonus_aux(p_ptr->proficiency[PROF_INNATE_ATTACKS]);
}

cptr skills_shield_describe_current(int sval)
{
    return _weapon_describe_aux(
        skills_weapon_current(PROF_INNATE_ATTACKS),
        skills_weapon_max(PROF_INNATE_ATTACKS));
}

void skills_martial_arts_gain(void)
{
    int current, max;
    int mult = coffee_break ? 4 : 1;

    if (p_ptr->pclass == CLASS_SKILLMASTER) return;

    current = p_ptr->proficiency[PROF_MARTIAL_ARTS];
    max = p_ptr->proficiency_cap[PROF_MARTIAL_ARTS];

    if (current < max)
    {
        if (current < WEAPON_EXP_BEGINNER)
            current += (40 * mult);
        else if (current < WEAPON_EXP_SKILLED)
            current += (5 * mult);
        else if (current < WEAPON_EXP_EXPERT && p_ptr->lev > 19)
            current += mult;
        else if ((p_ptr->lev > 34) && (one_in_(3) || coffee_break))
            current += 1;

        p_ptr->proficiency[PROF_MARTIAL_ARTS] = MIN(current, max);
        p_ptr->update |= PU_BONUS;
    }
}

int skills_martial_arts_current(void)
{
    if (p_ptr->pclass == CLASS_SKILLMASTER)
        return skillmaster_martial_arts_prof();
    else
    {
        int current = p_ptr->proficiency[PROF_MARTIAL_ARTS];
        int max = p_ptr->proficiency_cap[PROF_MARTIAL_ARTS];
        return MIN(current, max);
    }
}

int skills_martial_arts_max(void)
{
    if (p_ptr->pclass == CLASS_SKILLMASTER)
        return skillmaster_martial_arts_prof();

    return p_ptr->proficiency_cap[PROF_MARTIAL_ARTS];
}

void skills_dual_wielding_gain(monster_race *r_ptr)
{
    int current, max;
    int mult = coffee_break ? 4 : 1;

    if (p_ptr->pclass == CLASS_SKILLMASTER) return;

    current = p_ptr->proficiency[PROF_DUAL_WIELDING];
    max = skills_dual_wielding_max();

    if (current < max && (current - 1000) / 200 < r_ptr->level)
    {
        if (current < WEAPON_EXP_BEGINNER)
            current += 80 * mult;
        else if (current < WEAPON_EXP_SKILLED)
            current += 4 * mult;
        else if (current < WEAPON_EXP_EXPERT)
            current += mult;
        else if ((current < WEAPON_EXP_MASTER) && (one_in_(3) || coffee_break))
            current += 1;

        p_ptr->proficiency[PROF_DUAL_WIELDING] = MIN(current, max);
        p_ptr->update |= PU_BONUS;
    }
}

int skills_dual_wielding_current(void)
{
    if (p_ptr->pclass == CLASS_SKILLMASTER)
        return skillmaster_dual_wielding_prof();
    else
    {
        int current = p_ptr->proficiency[PROF_DUAL_WIELDING];
        int max = skills_dual_wielding_max();
        return MIN(current, max);
    }
}

int skills_dual_wielding_max(void)
{
    if (p_ptr->pclass == CLASS_SKILLMASTER)
        return skillmaster_dual_wielding_prof();

    return p_ptr->proficiency_cap[PROF_DUAL_WIELDING];
}

static void _skills_riding_gain(int inc)
{
    int current, max, update;

    if (p_ptr->pclass == CLASS_SKILLMASTER) return;

    if (coffee_break) inc *= 3;

    current = p_ptr->proficiency[PROF_RIDING];
    max = skills_riding_max();
    update = MIN(max, current + inc);

    if (update > current)
    {
        p_ptr->proficiency[PROF_RIDING] = update;
        p_ptr->update |= PU_BONUS;

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

    if (p_ptr->pclass == CLASS_SKILLMASTER) return;

    current = p_ptr->proficiency[PROF_RIDING];
    max = skills_riding_max();

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
            _skills_riding_gain(inc);
    }
}

void skills_riding_gain_rakuba(int dam)
{
    int current, max;

    if (p_ptr->pclass == CLASS_SKILLMASTER) return;

    current = p_ptr->proficiency[PROF_RIDING];
    max = skills_riding_max();

    assert(p_ptr->riding);

    if (current < max && max > 1000)
    {
        int ridinglevel = r_info[m_list[p_ptr->riding].r_idx].level;
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

    if (p_ptr->pclass == CLASS_SKILLMASTER) return;

    current = p_ptr->proficiency[PROF_RIDING];
    max = skills_riding_max();

    assert(p_ptr->riding);

    if (current < max)
    {
        int ridinglevel = r_info[m_list[p_ptr->riding].r_idx].level;

        if ((current - RIDING_EXP_BEGINNER*2) / 200 < ridinglevel && one_in_(2))
        {
            _skills_riding_gain(1);
        }
    }
}

int skills_riding_current(void)
{
    if (p_ptr->pclass == CLASS_SKILLMASTER)
        return skillmaster_riding_prof();
    else
    {
        int current = p_ptr->proficiency[PROF_RIDING];
        int max = skills_riding_max();
        if (p_ptr->prace == RACE_MON_RING)
            return RIDING_EXP_MASTER;
        return MIN(current, max);
    }
}

int skills_riding_max(void)
{
    if (p_ptr->pclass == CLASS_SKILLMASTER)
        return skillmaster_riding_prof();
    return p_ptr->proficiency_cap[PROF_RIDING];
}

static str_map_ptr _innate_map(void)
{
    static str_map_ptr _map = NULL;
    if (!_map)
        _map = str_map_alloc(free);
    return _map;
}

void skills_innate_init(cptr name, int current, int max)
{
    p_ptr->proficiency[PROF_INNATE_ATTACKS] = current;
    p_ptr->proficiency_cap[PROF_INNATE_ATTACKS] = max;
}

int skills_innate_max(cptr name)
{
    
    return p_ptr->proficiency_cap[PROF_INNATE_ATTACKS];
}

int skills_innate_current(cptr name)
{
    return p_ptr->proficiency[PROF_INNATE_ATTACKS];
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
    int current = p_ptr->proficiency[PROF_INNATE_ATTACKS];
    int max = p_ptr->proficiency_cap[PROF_INNATE_ATTACKS];
    if (mut_present(MUT_WEAPON_SKILLS))
        max = WEAPON_EXP_MASTER;

    if (current < max)
    {
        int step;
        int add;

        if (rlvl < _weapon_min_rlvl(p_ptr->lev))
        {
            if (p_ptr->wizard)
                msg_format("<color:B>You must fight level <color:R>%d</color> monsters to gain weapon proficiency.</color>", _weapon_min_rlvl(p_ptr->lev));
            return;
        }

        if (current >= _weapon_max_skill(rlvl))
        {
            if (p_ptr->wizard)
            {
                msg_format("<color:B>Against level <color:R>%d</color> foes, you can only train weapon "
                    "proficiency to <color:R>%d</color> (Current Skill: <color:R>%d</color>).</color>",
                    rlvl, _weapon_max_skill(rlvl), current);
            }
            return;
        }

        step = _weapon_gain_amt(current);
        add = step / 10;
        if (step%10 && randint0(10) < step%10) add++;

        if (add > 0)
        {
            int old_bonus = _innate_calc_bonus_aux(current);
            int new_bonus;
            current += add;
            if (current > max)
                current = max;
            new_bonus = _innate_calc_bonus_aux(current);
            if (old_bonus != new_bonus)
            {
                msg_format("<color:B>Your <color:R>%s</color> skills are improving.</color>", name);
                p_ptr->update |= PU_BONUS;
            }
        }
    }
}

cptr skills_innate_calc_name(innate_attack_ptr attack)
{
    static char buf[MAX_NLEN];
    buf[0] = '\0';
#if 0
    if ((p_ptr->prace == RACE_MON_POSSESSOR || p_ptr->prace == RACE_MON_MIMIC) && p_ptr->current_r_idx)
    {
        monster_race *r_ptr = &r_info[p_ptr->current_r_idx];
        if (r_ptr->flags1 & RF1_UNIQUE)
            sprintf(buf, "%d.%s", p_ptr->current_r_idx, attack->name);
        else
            sprintf(buf, "%c.%s", r_ptr->d_char, attack->name);
    }
    else
#endif
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


void skills_on_birth(void)
{
    /* Start with weapon proficiency halfway to beginner */
    for (int i = 0; i < MAX_PROFICIENCIES - 2; i++)
    {
        p_ptr->proficiency[i] = WEAPON_EXP_BEGINNER / 2;
    }
    p_ptr->proficiency[PROF_RIDING] = RIDING_EXP_UNSKILLED;

    /* Everyone starts unskilled with innate attacks, but can become an expert since they are innate. */
    p_ptr->proficiency[PROF_INNATE_ATTACKS] = WEAPON_EXP_BEGINNER / 2;
    p_ptr->proficiency_cap[PROF_INNATE_ATTACKS] = WEAPON_EXP_EXPERT;
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
static cptr _dis_skill_desc(int base, int xtra) { return _skill_desc(base + 5*xtra - 40, 8); }
static cptr _class_dis_skill_desc(class_t *class_ptr) { return _dis_skill_desc(class_ptr->base_skills.dis, class_ptr->extra_skills.dis); }
static cptr _mon_race_dis_skill_desc(race_t *race_ptr) { return _dis_skill_desc(race_ptr->skills.dis, race_ptr->extra_skills.dis); }

static cptr _dis_skill_desc2(int base) { return _skill_desc(base + 5, 2); }
static cptr _race_dis_skill_desc(race_t *race_ptr) { return _dis_skill_desc2(race_ptr->skills.dis); }
static cptr _pers_dis_skill_desc(personality_ptr pers_ptr) { return _dis_skill_desc2(pers_ptr->skills.dis*2); }
static cptr _realm_dis_skill_desc(dragon_realm_ptr realm_ptr) { return _skill_desc(realm_ptr->skills.dis + 10, 2); }

/* Devices */
static cptr _dev_skill_desc(int base, int xtra) { return _skill_desc(base + 5*xtra - 50, 6); }
static cptr _class_dev_skill_desc(class_t *class_ptr) { return _dev_skill_desc(class_ptr->base_skills.dev, class_ptr->extra_skills.dev); }
static cptr _mon_race_dev_skill_desc(race_t *race_ptr) { return _dev_skill_desc(race_ptr->skills.dev, race_ptr->extra_skills.dev); }

static cptr _dev_skill_desc2(int base) { return _skill_desc(base + 5, 2); }
static cptr _race_dev_skill_desc(race_t *race_ptr) { return _dev_skill_desc2(race_ptr->skills.dev); }
static cptr _pers_dev_skill_desc(personality_ptr pers_ptr) { return _dev_skill_desc2(pers_ptr->skills.dev*2); }
static cptr _realm_dev_skill_desc(dragon_realm_ptr realm_ptr) { return _skill_desc(realm_ptr->skills.dev + 5, 1); }

/* Saving Throws */
static cptr _sav_skill_desc(int base, int xtra) { return _skill_desc(base + 5*xtra - 65, 5); }
static cptr _class_sav_skill_desc(class_t *class_ptr) { return _sav_skill_desc(class_ptr->base_skills.sav, class_ptr->extra_skills.sav); }
static cptr _mon_race_sav_skill_desc(race_t *race_ptr) { return _sav_skill_desc(race_ptr->skills.sav, race_ptr->extra_skills.sav); }

static cptr _sav_skill_desc2(int base) { return _skill_desc(base + 5, 2); }
static cptr _race_sav_skill_desc(race_t *race_ptr) { return _sav_skill_desc2(race_ptr->skills.sav); }
static cptr _pers_sav_skill_desc(personality_ptr pers_ptr) { return _sav_skill_desc2(pers_ptr->skills.sav*2); }
static cptr _realm_sav_skill_desc(dragon_realm_ptr realm_ptr) { return _skill_desc(realm_ptr->skills.sav + 5, 1); }

/* Melee */
static cptr _thn_skill_desc(int base, int xtra) { return _skill_desc(base + 5*xtra - 70, 12); }
static cptr _class_thn_skill_desc(class_t *class_ptr) { return _thn_skill_desc(class_ptr->base_skills.thn, class_ptr->extra_skills.thn); }
static cptr _mon_race_thn_skill_desc(race_t *race_ptr) { return _thn_skill_desc(race_ptr->skills.thn, race_ptr->extra_skills.thn); }

static cptr _thn_skill_desc2(int base) { return _skill_desc(base + 5, 2); }
static cptr _race_thn_skill_desc(race_t *race_ptr) { return _thn_skill_desc2(race_ptr->skills.thn); }
static cptr _pers_thn_skill_desc(personality_ptr pers_ptr) { return _thn_skill_desc2(pers_ptr->skills.thn*2); }
static cptr _realm_thn_skill_desc(dragon_realm_ptr realm_ptr) { return _skill_desc(realm_ptr->skills.thn + 10, 2); }

/* Bows */
static cptr _thb_skill_desc(int base, int xtra) { return _skill_desc(base + 5*xtra - 60, 12); }
static cptr _class_thb_skill_desc(class_t *class_ptr) { return _thb_skill_desc(class_ptr->base_skills.thb, class_ptr->extra_skills.thb); }
static cptr _mon_race_thb_skill_desc(race_t *race_ptr) { return _thb_skill_desc(race_ptr->skills.thb, race_ptr->extra_skills.thb); }

static cptr _thb_skill_desc2(int base) { return _skill_desc(base + 5, 2); }
static cptr _race_thb_skill_desc(race_t *race_ptr) { return _thb_skill_desc2(race_ptr->skills.thb); }
static cptr _pers_thb_skill_desc(personality_ptr pers_ptr) { return _thb_skill_desc2(pers_ptr->skills.thb*2); }
static cptr _realm_thb_skill_desc(dragon_realm_ptr realm_ptr) { return _skill_desc(realm_ptr->skills.thb + 10, 2); }

/* Stealth */
static cptr _stl_skill_desc(int base, int xtra) { return _skill_desc(base + 5*xtra + 2, 1); }
static cptr _class_stl_skill_desc(class_t *class_ptr) { return _stl_skill_desc(class_ptr->base_skills.stl, class_ptr->extra_skills.stl); }
static cptr _mon_race_stl_skill_desc(race_t *race_ptr) { return _stl_skill_desc(race_ptr->skills.stl, race_ptr->extra_skills.stl); }
static cptr _race_stl_skill_desc(race_t *race_ptr) { return _stl_skill_desc(race_ptr->skills.stl, 0); }
static cptr _pers_stl_skill_desc(personality_ptr pers_ptr) { return _stl_skill_desc(pers_ptr->skills.stl, 0); }
static cptr _realm_stl_skill_desc(dragon_realm_ptr realm_ptr) { return _skill_desc(realm_ptr->skills.stl + 4, 1); }

/* Searching */
static cptr _srh_skill_desc(int base, int xtra) { return _skill_desc(base + 5*xtra, 6); }
static cptr _class_srh_skill_desc(class_t *class_ptr) { return _srh_skill_desc(class_ptr->base_skills.srh, class_ptr->extra_skills.srh); }
static cptr _mon_race_srh_skill_desc(race_t *race_ptr) { return _srh_skill_desc(race_ptr->skills.srh, race_ptr->extra_skills.srh); }

static cptr _srh_skill_desc2(int base) { return _skill_desc(base, 1); }
static cptr _race_srh_skill_desc(race_t *race_ptr) { return _srh_skill_desc2(race_ptr->skills.srh); }
static cptr _pers_srh_skill_desc(personality_ptr pers_ptr) { return _srh_skill_desc2(pers_ptr->skills.srh + 3); }
static cptr _realm_srh_skill_desc(dragon_realm_ptr realm_ptr) { return _skill_desc(realm_ptr->skills.srh, 1); }

/* Perception */
static cptr _fos_skill_desc(int base, int xtra) { return _skill_desc(base + 5*xtra, 6); }
static cptr _class_fos_skill_desc(class_t *class_ptr) { return _fos_skill_desc(class_ptr->base_skills.fos, class_ptr->extra_skills.fos); }
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

/* Default monster weapon / skill proficiencies */
void monster_proficiencies(void)
{
    p_ptr->proficiency[PROF_DIGGER] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency[PROF_BLUNT] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency[PROF_POLEARM] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency[PROF_SWORD] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency[PROF_DAGGER] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency[PROF_BOW] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency[PROF_CROSSBOW] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency[PROF_SLING] = WEAPON_EXP_BEGINNER;
    p_ptr->proficiency[PROF_INNATE_ATTACKS] = WEAPON_EXP_BEGINNER;

    p_ptr->proficiency_cap[PROF_DIGGER] = WEAPON_EXP_MASTER;
    p_ptr->proficiency_cap[PROF_BLUNT] = WEAPON_EXP_MASTER;
    p_ptr->proficiency_cap[PROF_POLEARM] = WEAPON_EXP_MASTER;
    p_ptr->proficiency_cap[PROF_SWORD] = WEAPON_EXP_MASTER;
    p_ptr->proficiency_cap[PROF_STAVE] = WEAPON_EXP_MASTER;
    p_ptr->proficiency_cap[PROF_AXE] = WEAPON_EXP_MASTER;
    p_ptr->proficiency_cap[PROF_DAGGER] = WEAPON_EXP_MASTER;
    p_ptr->proficiency_cap[PROF_BOW] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_CROSSBOW] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_SLING] = WEAPON_EXP_EXPERT;
    p_ptr->proficiency_cap[PROF_MARTIAL_ARTS] = WEAPON_EXP_UNSKILLED;
    p_ptr->proficiency_cap[PROF_DUAL_WIELDING] = WEAPON_EXP_MASTER;
    p_ptr->proficiency_cap[PROF_RIDING] = RIDING_EXP_UNSKILLED;
    p_ptr->proficiency_cap[PROF_INNATE_ATTACKS] = WEAPON_EXP_MASTER;
}

int tsvals_to_proficiency(int tval, int sval)
{
    int prof = PROF_INNATE_ATTACKS;
    switch (tval)
    {
    case TV_BOW:
        prof = PROF_CROSSBOW;
        switch (sval)
        {
        case SV_SHORT_BOW:
        case SV_LONG_BOW:
        case SV_NAMAKE_BOW:
            prof = PROF_BOW;
            break;
        case SV_SLING:
            prof = PROF_SLING;
        }
        break;

    case TV_HAFTED:
        prof = PROF_BLUNT;
        break;
    case TV_POLEARM:
        prof = PROF_POLEARM;
        break;
    case TV_SWORD:
        prof = PROF_SWORD;
        break;
    case TV_STAVES:
        prof = PROF_STAVE;
        break;
    case TV_AXE:
        prof = PROF_AXE;
        break;
    case TV_DAGGER:
        prof = PROF_DAGGER;
        break;
    }

    return prof;
}