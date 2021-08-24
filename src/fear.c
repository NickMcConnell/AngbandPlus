#include "angband.h"

int _get_level(int amount)
{
    if (amount >= FEAR_PETRIFIED)
        return FEAR_PETRIFIED;
    if (amount >= FEAR_TERRIFIED)
        return FEAR_TERRIFIED;
    if (amount >= FEAR_SCARED)
        return FEAR_SCARED;
    if (amount >= FEAR_NERVOUS)
        return FEAR_NERVOUS;
    if (amount >= FEAR_UNEASY)
        return FEAR_UNEASY;

    return FEAR_BOLD;
}

int _decrement_level(int lvl)
{
    switch (lvl)
    {
    case FEAR_UNEASY: return FEAR_BOLD;
    case FEAR_NERVOUS: return FEAR_UNEASY;
    case FEAR_SCARED: return FEAR_NERVOUS;
    case FEAR_TERRIFIED: return FEAR_SCARED;
    case FEAR_PETRIFIED: return FEAR_TERRIFIED;
    }
    return FEAR_BOLD;
}

static void _decrease_p(int lvls)
{
    int lvl = fear_level_p();

    for (;lvls; lvls--)
        lvl = _decrement_level(lvl);

    fear_set_p(lvl);
}

int  fear_level_p(void)
{
    return _get_level(plr->afraid);
}

void fear_clear_p(void)
{
    fear_set_p(0);
}

bool fear_add_p(int amount)
{
    return fear_set_p(plr->afraid + amount);
}

static char _get_level_color(int v)
{
    int lvl = _get_level(v);
    switch (lvl)
    {
    case FEAR_UNEASY: return 'U';
    case FEAR_NERVOUS: return 'y';
    case FEAR_SCARED: return 'R';
    case FEAR_TERRIFIED: return 'r';
    case FEAR_PETRIFIED: return 'v';
    }

    return 'W';
}

static cptr _get_level_name(int v)
{
    int lvl = _get_level(v);
    switch (lvl)
    {
    case FEAR_UNEASY: return "uneasy";
    case FEAR_NERVOUS: return "nervous";
    case FEAR_SCARED: return "scared";
    case FEAR_TERRIFIED: return "terrified";
    case FEAR_PETRIFIED: return "petrified";
    }

    return "bold";
}

bool fear_set_p(int v)
{
    int old_lvl, new_lvl;
    bool notice = FALSE;

    /* Hack -- Force good values */
    v = (v > 10000) ? 10000 : (v < 0) ? 0 : v;

    if (plr->is_dead) return FALSE;

    old_lvl = _get_level(plr->afraid);
    new_lvl = _get_level(v);

    if (new_lvl > old_lvl)
    {
        msg_format("You feel <color:%c>%s</color>.", _get_level_color(new_lvl), _get_level_name(new_lvl));
        if (old_lvl <= FEAR_SCARED && randint0(30) < cave->difficulty && one_in_(6) && !fear_save_p(v/5))
            do_dec_stat(A_CHR);
        if (plr->special_defense & KATA_MASK)
        {
            msg_print("Your posture gets loose.");
            plr->special_defense &= ~KATA_MASK;
            plr->update |= PU_BONUS;
            plr->update |= PU_MONSTERS;
            plr->redraw |= PR_STATE;
            plr->redraw |= PR_STATUS;
            plr->action = ACTION_NONE;
        }
        notice = TRUE;
        plr->counter = FALSE;
        virtue_add(VIRTUE_VALOUR, -1);
    }
    else if (new_lvl < old_lvl)
    {
        if (new_lvl == FEAR_BOLD)
            msg_print("Your fears finally subside.");
        else
        {
            msg_format("You are no longer %s, but you still feel %s.",
                        _get_level_name(old_lvl), _get_level_name(new_lvl));
        }
        notice = TRUE;
    }

    plr->afraid = v;
    plr->redraw |= PR_EFFECTS;
    if (!notice) return FALSE;
    if (disturb_state) disturb(0, 0);
    return TRUE;
}

/* Effective monster level for fear calculations */
static int _r_level(monster_race *r_ptr)
{
    int ml = r_ptr->alloc.lvl;
    if (mon_race_is_unique(r_ptr))
        ml += 3;
    return ml;
}

/* Effective threat level for player recovery */
int fear_threat_level(void)
{
    int dl = MIN(cave->dun_lvl + 2, 127);
    int ml = 0;

    int_map_iter_ptr iter;
    for (iter = int_map_iter_alloc(cave->mon);
            int_map_iter_is_valid(iter);
            int_map_iter_next(iter))
    {
        mon_ptr mon = int_map_iter_current(iter);
        mon_race_ptr race = mon->race;
        if (!mon->ml) continue;
        if (!mon_projects_fear(mon)) continue;
        if (!plr_can_see(mon->pos)) continue; /* don't fear what you can't see, right? */

        ml = MAX(ml, _r_level(race)/MAX(1, mon->cdis - 2));
    }
    int_map_iter_free(iter);

    return MAX(dl, ml);
}

/* Permissable Player Actions */
bool fear_allow_device(void)
{
    if (plr->afraid && !fear_save_p(plr->afraid)) return FALSE;
    return TRUE;
}

bool fear_allow_magic(void)
{
    if (plr->afraid && !fear_save_p(plr->afraid)) return FALSE;
    return TRUE;
}

bool fear_allow_melee(mon_ptr mon)
{
    if (plr->afraid)
    {
        if ( plr->pclass == CLASS_DUELIST
            && plr->lev >= 5
            && mon == who_mon(plr->duelist_target) )
        {
            /* Duelist: Fearless Duel */
            if (!fear_save_p(plr->afraid)) return FALSE;
        }
        else if (!fear_save_p(3*plr->afraid))
            return FALSE;
    }
    return TRUE;
}

bool fear_allow_shoot(void)
{
    if (plr->afraid && !fear_save_p(3*plr->afraid)) return FALSE;
    return TRUE;
}

static int _plev(void)
{
    int l = plr->lev;
    if (plr->personality == PERS_CRAVEN)
        l = MAX(1, l - 5);
    if (l <= 40)
        return 5 + l;

    return 45 + (l - 40)*2;
}

/* Fear Saving Throws
   Odds that 1dM <= 1dN. There are 2 cases:
   [1] M<=N: p = (2N-M+1)/2N
   [2] M>N : p = (N+1)/2M
 */
bool fear_save_p(int ml)
{
    int pl;
    int rolls;
    int i;

    if (!hack_mind) return TRUE;
    if (ml <= 1) return TRUE;

    /* Immunity to Fear?*/
    if (res_pct(GF_FEAR) >= 100) return TRUE;

    pl = _plev() + adj_stat_save_fear[plr->stat_ind[A_CHR]];
    if (pl < 1) pl = 1;

    rolls = 1 + plr->resist[GF_FEAR];

    /* Vulnerability to Fear? At least give the player a chance! */
    if (rolls < 1)
    {
        rolls = 1;
        pl = (pl + 1)/2;
    }

    for (i = 0; i < rolls; i++)
    {
        #if DEVELOPER
        if (0 || plr->wizard)
        {
            int odds;
            if (ml<=pl)
                odds=(2*pl-ml+1)*1000/(2*pl);
            else
                odds=(pl+1)*1000/(2*ml);
            msg_format("<color:D>fear_save_p(%d, %d) = %d.%d%%</color>", pl, ml, odds/10, odds%10);
        }
        #endif
        if (randint1(ml) <= randint1(pl))
        {
            equip_learn_resist(OF_RES_(GF_FEAR));
            return TRUE;
        }
    }
    return FALSE;
}

bool life_save_p(int ml)
{
    int pl;
    int rolls;
    int i;

    pl = _plev() + adj_stat_save[plr->stat_ind[A_CHR]];
    if (pl < 1) pl = 1;

    rolls = plr->hold_life;

    if (plr->wizard) msg_format("<color:D>life_save_p: 1d%d <= 1d%d</color>", ml, pl);
    for (i = 0; i < rolls; i++)
    {
        if (randint1(ml) <= randint1(pl))
        {
            equip_learn_resist(OF_HOLD_LIFE);
            return TRUE;
        }
    }
    return FALSE;
}

bool fear_save_m(monster_type *m_ptr)
{
    int           pl = _plev();
    monster_race *r_ptr = m_ptr->race;
    int           ml = _r_level(r_ptr);
    bool          result = FALSE;

    if (mon_tim_find(m_ptr, T_BERSERK)) return TRUE;

    /* Player may not exert their force of will out of sight! */
    if (plr_view(m_ptr->pos))
        pl += adj_stat_save[plr->stat_ind[A_CHR]];

    if (pl <= 1) return TRUE;

    if (randint1(pl) <= randint1(ml)) result = TRUE;

    return result;
}

/* Recovery from Fear */
void fear_recover_p(void)
{
    if (plr->afraid)
    {
        int threat = fear_threat_level();
        if (fear_save_p(threat + plr->afraid / 10))
        {
            int lvls = 1;

            while (lvls < 3 && fear_save_p(threat + plr->afraid))
                lvls++;

            _decrease_p(lvls);
        }
        else if (plr->afraid >= FEAR_SCARED && !fear_save_p((threat + plr->afraid)/6))
        {
            if (plr->afraid >= FEAR_PETRIFIED)
            {
                plr->energy_need += 100 * TURNS_PER_TICK / 10;
                msg_print("You are scared stiff!");
                disturb(1, 0);
            }
            else if (plr->afraid >= FEAR_TERRIFIED)
            {
                plr->energy_need += 60 * TURNS_PER_TICK / 10;
                msg_print("You shudder uncontrollably!");
                disturb(1, 0);
            }
            else
            {
                plr->energy_need += 30 * TURNS_PER_TICK / 10;
                msg_print("You tremble in terror!");
                disturb(1, 0);
            }
        }
    }
}

/* Handle the Terrifying Aura of Fear! */
static void _fear_process_p_aux(int id, mon_ptr mon)
{
    if (mon->ml)
    {
        int r_level;
        mon_race_ptr race = mon->apparent_race;

        if (!mon_projects_fear(mon)) return;
        if (mon_is_pet(mon) || mon_is_friendly(mon)) return;
        if (!plr_can_see(mon->pos)) return;

        r_level = _r_level(race);
        if (!fear_save_p(r_level/MAX(1, mon->cdis-2)))
        {
            char m_name[80];
            monster_desc(m_name, mon, 0);
            msg_format("You behold the terrifying visage of %s!", m_name);
            mon_lore_projects_fear(mon);
            fear_add_p(r_level/MAX(1, mon->cdis-2));
        }
    }
}
void fear_process_p(void)
{
    dun_iter_mon(cave, _fear_process_p_aux);
}

void fear_update_m(monster_type *m_ptr)
{
    monster_race *r_ptr = m_ptr->apparent_race;
    if (mon_projects_fear(m_ptr) && m_ptr->ml && !mon_is_pet(m_ptr) && !mon_is_friendly(m_ptr))
    {
        int r_level = _r_level(r_ptr);
        if (!fear_save_p(r_level))
        {
            char m_name[80];
            monster_desc(m_name, m_ptr, 0);
            msg_format("You behold the terrifying visage of %s!", m_name);
            mon_lore_projects_fear(m_ptr);
            fear_add_p(r_level);
        }
    }
}

void fear_p_touch_m(monster_type *m_ptr)
{
    monster_race *r_ptr = m_ptr->apparent_race;

    if (mon_projects_fear(m_ptr))
    {
        int r_level = _r_level(r_ptr);
        if (!fear_save_p(r_level))
        {
            mon_lore_projects_fear(m_ptr);
            fear_add_p(r_level);
        }
    }
}

bool fear_p_hurt_m(mon_ptr mon, int dam)
{
    bool result = FALSE;

    /* Apply Aura of Fear to the Player for non-melee damage */
    if (!plr_attack_current() && mon_projects_fear(mon))
    {
        int r_level = _r_level(mon->apparent_race);
        if (!fear_save_p(r_level))
        {
            mon_lore_projects_fear(mon);
            fear_add_p(r_level/MAX(1, mon->cdis - 2));
        }
    }

    if ( !mon_tim_find(mon, T_FEAR)
      && !mon_tim_find(mon, T_BERSERK)
      && _1d(100) > mon_res_pct(mon, GF_FEAR) ) /* XXX check for IMMUNE(FEAR) */
    {
        int percentage = (100 * mon->hp) / mon->maxhp;
        int n = 10;

        n = n * adj_fear_m[plr->stat_ind[A_CHR]] / 100;

        if ((n >= percentage) || (dam >= mon->hp && randint0(100) < 80))
        {
            if (!fear_save_m(mon))
            {
                result = TRUE;
                mon_tim_add(mon, T_FEAR, randint1(10) + 20);
            }
        }
    }
    return result;
}

void fear_terrify_p(monster_type *m_ptr)
{
    monster_race *r_ptr = m_ptr->apparent_race;
    int           r_level = _r_level(r_ptr);

    if (fear_save_p(r_level))
    {
        if (disturb_minor)
            msg_print("You stand your ground!");
    }
    else
        fear_add_p(r_level);
}

void fear_scare_p(monster_type *m_ptr)
{
    monster_race *r_ptr = m_ptr->apparent_race;
    int           r_level = _r_level(r_ptr);

    if (fear_save_p(r_level))
        msg_print("You refuse to be frightened.");
    else
        fear_add_p(r_level);
}

/* Monster Fear */
bool fear_process_m(mon_ptr mon)
{
    if (!mon_is_valid(mon)) return FALSE; /* paranoia */

    /* XXX Pain cancels fear hack */
    if (mon->pain)
        mon_tim_subtract(mon, T_FEAR, _1d(mon->pain));

    if (mon_tim_find(mon, T_FEAR) && !mon_tim_find(mon, MT_SLEEP))
    {
        if (fear_save_m(mon))
        {
            if (fear_save_m(mon))
                mon_tim_remove(mon, T_FEAR);
            else
            {
                monster_race *r_ptr = mon->apparent_race;
                mon_tim_subtract(mon, T_FEAR, _1d(r_ptr->alloc.lvl/20 + 1));
            }

        }
        else if (one_in_(3) && !fear_save_m(mon))
        {
            if (mon_show_msg(mon))
            {
                char m_name[80];
                monster_desc(m_name, mon, 0);
                msg_format("%^s is scared stiff!", m_name);
            }
            return FALSE; /* cf _process_monster */
        }
    }
    return TRUE;
}

#define HURT_0   0
#define HURT_25 25
#define HURT_50 50
#define HURT_65 65
#define HURT_80 80
#define HURT_90 90
#define HURT_95 95

static int _get_hurt_level(int chp)
{
    int pct = (plr->mhp - MAX(chp, 0)) * 100 / plr->mhp;

    if (plr->mhp < 100)
    {
        if (pct >= HURT_65)
            return HURT_65;
    }
    else if (plr->mhp < 250)
    {
        if (pct >= HURT_80)
            return HURT_80;
        if (pct >= HURT_50)
            return HURT_50;
    }
    else if (plr->mhp < 500)
    {
        if (pct >= HURT_90)
            return HURT_90;
        if (pct >= HURT_65)
            return HURT_65;
        if (pct >= HURT_50)
            return HURT_50;
    }
    else
    {
        if (pct >= HURT_95)
            return HURT_95;
        if (pct >= HURT_90)
            return HURT_90;
        if (pct >= HURT_80)
            return HURT_80;
        if (pct >= HURT_65)
            return HURT_65;
        if (pct >= HURT_50)
            return HURT_50;
        if (pct >= HURT_25)
            return HURT_25;
    }
    return HURT_0;
}

void fear_heal_p(int old_hp, int new_hp)
{
    if (plr->pclass != CLASS_BLOOD_KNIGHT && plr->afraid)
    {
        int old_hurt = _get_hurt_level(old_hp);
        int new_hurt = _get_hurt_level(new_hp);

        if (new_hurt < old_hurt && fear_save_p(fear_threat_level()))
            _decrease_p(1);
    }
}

void fear_hurt_p(int old_hp, int new_hp)
{
    if (plr->pclass != CLASS_BLOOD_KNIGHT)
    {
        int old_hurt = _get_hurt_level(old_hp);
        int new_hurt = _get_hurt_level(new_hp);
        if (new_hurt > old_hurt)
        {
            if ( !fear_save_p(fear_threat_level())
              || (new_hurt > HURT_50 && !fear_save_p(fear_threat_level())) )
            {
                fear_add_p(new_hurt);
            }
            else
                msg_format("You stand your ground!");
        }
    }
}

