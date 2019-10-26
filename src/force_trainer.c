#include "angband.h"

static int _force_boost(void) { return p_ptr->magic_num1[0]; }

/************************************************************************
 * Timers
 ************************************************************************/
enum { _SH_FORCE = T_CUSTOM };
static bool _sh_force_on(plr_tim_ptr timer)
{
    msg_print("You are enveloped by an aura of the Force!");
    return TRUE;
}
static void _sh_force_off(plr_tim_ptr timer)
{
    msg_print("The aura of the Force disappeared.");
}
static status_display_t _sh_force_display(plr_tim_ptr timer)
{
    return status_display_create("Force", "Fo", TERM_WHITE);
}
static plr_tim_info_ptr _sh_force(void)
{
    plr_tim_info_ptr info = plr_tim_info_alloc(_SH_FORCE, "Aura of Force");
    info->desc = "You are shielded by an aura of the force.";
    info->on_f = _sh_force_on;
    info->off_f = _sh_force_off;
    info->status_display_f = _sh_force_display;
    return info;
}
static void _register_timers(void) { plr_tim_register(_sh_force()); }
/************************************************************************
 * Attack
 ************************************************************************/
static void _after_hit(mon_attack_ptr context)
{
    if (plr_tim_find(_SH_FORCE)) /* paranoia */
    {
        if (!(context->race->flagsr & RFR_RES_ALL))
        {
            int dam = 2 + damroll(1 + (p_ptr->lev / 10), 2 + (p_ptr->lev / 10));

            dam = mon_damage_mod(context->mon, dam, FALSE);
            msg_format("%^s is injured by the <color:B>Force</color>.", context->mon_name);

            if (mon_take_hit(context->mon->id, dam, &context->fear, " is destroyed."))
                context->stop = STOP_MON_DEAD;
        }
        else
            mon_lore_r(context->mon, RFR_RES_ALL);
    }
}
static void _mon_attack_init(mon_attack_ptr context)
{
    if (plr_tim_find(_SH_FORCE))
        context->after_hit_f = _after_hit;
}
/************************************************************************
 * Spells
 ************************************************************************/
static void _small_force_ball_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Small Force Ball");
        break;
    case SPELL_DESC:
    {        
        var_set_string(res, "Fires a very small energy ball.");
        break;
    }
    case SPELL_INFO:
    {
        int dice = 3 + ((p_ptr->lev - 1) / 5) + _force_boost()/ 12;
        int sides = 4;
        var_set_string(res, info_damage(spell_power(dice), sides, spell_power(p_ptr->to_d_spell)));
        break;
    }
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, FALSE);
        if (get_fire_dir(&dir))
        {
            int dice = 3 + ((p_ptr->lev - 1) / 5) + _force_boost()/ 12;
            int sides = 4;
            fire_ball(
                GF_MISSILE,
                dir,
                spell_power(damroll(dice, sides) + p_ptr->to_d_spell),
                0
            );
            var_set_bool(res, TRUE);
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _flying_technique_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Flying Technique");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives levitation a while.");
        break;
    case SPELL_CAST:
    {
        plr_tim_add(T_LEVITATION, spell_power(randint1(30) + 30 + _force_boost() / 5));
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _kamehameha_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Kamehameha");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a short energy beam.");
        break;
    case SPELL_INFO:
    {
        int dice = 5 + ((p_ptr->lev - 1) / 5) + _force_boost() / 10;
        int sides = 5;
        var_set_string(res, info_damage(spell_power(dice), sides, spell_power(p_ptr->to_d_spell)));
        break;
    }
    case SPELL_CAST:
    {
        int dir = 0;
        project_length = p_ptr->lev / 8 + 3;
        var_set_bool(res, FALSE);
        if (get_fire_dir(&dir))
        {
            int dice = 5 + ((p_ptr->lev - 1) / 5) + _force_boost() / 10;
            int sides = 5;
            fire_beam(
                GF_MISSILE,
                dir,
                spell_power(damroll(dice, sides) + p_ptr->to_d_spell)
            );
            var_set_bool(res, TRUE);
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _magic_resistance_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Magic Resistance");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives magic resistance for a while.");
        break;
    case SPELL_CAST:
    {
        int dur = randint1(20) + 20 + _force_boost() / 5;
        plr_tim_add(T_RES_MAGIC, spell_power(dur));
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _improve_force_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Improve Force");
        break;
    case SPELL_DESC:
        var_set_string(res, "Improves spirit energy power temporarily. Improved spirit energy will be more and more powerful or have longer duration. Too many improving results in uncontrollable explosion of spirit energy.");
        break;
    case SPELL_CAST:
    {
        int oops = 4*p_ptr->lev + 120;
        msg_print("You improved the Force.");
        p_ptr->magic_num1[0] += (70 + p_ptr->lev);
        p_ptr->update |= PU_BONUS;
        p_ptr->redraw |= PR_EFFECTS;
        if (randint1(p_ptr->magic_num1[0]) > oops)
        {
            int prob = (p_ptr->magic_num1[0] - oops) * 100 / p_ptr->magic_num1[0];
            /* XXX shouldn't show the odds ... right? */
            msg_format("<color:r>The Force exploded!</color> <color:B>(%d%%)</color>", prob);
            fire_ball(GF_MANA, 0, p_ptr->magic_num1[0] / 2, 10);
            take_hit(DAMAGE_LOSELIFE, p_ptr->magic_num1[0] / 2, "Explosion of the Force");
            p_ptr->magic_num1[0] = 0;
            p_ptr->update |= PU_BONUS;
            p_ptr->redraw |= PR_EFFECTS;
            var_set_bool(res, FALSE); /* no energy consumed?? */
        }
        else var_set_bool(res, TRUE);
        break;
    }
    case SPELL_COST_EXTRA:
    {
        int n = 0;
        int j;
        for (j = 0; j < p_ptr->magic_num1[0] / 50; j++)
            n += (j+1) * 3 / 2;

        var_set_int(res, n);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _aura_of_force_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Aura of Force");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives aura which damages all monsters which attacked you for a while.");
        break;
    case SPELL_CAST:
    {
        plr_tim_add(_SH_FORCE, spell_power(randint1(p_ptr->lev / 2) + 15 + _force_boost() / 7));
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _shock_power_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shock Power");
        break;
    case SPELL_DESC:
        var_set_string(res, "Damages an adjacent monster, and blow it away.");
        break;
    case SPELL_INFO:
    {
        int dice = 8 + ((p_ptr->lev - 5) / 4) + _force_boost() / 12;
        int sides = 8;
        var_set_string(res, info_damage(spell_power(dice), sides, spell_power(p_ptr->to_d_spell)));
        break;
    }
    case SPELL_CAST:
    {
        int y, x, dam, dir;
        mon_ptr mon;
        project_length = 1;
        /*if (!get_fire_dir(&dir)) XXX blow away won't work if a target is chosen ... dir == 5 */
        if (!get_rep_dir2(&dir))
        { 
            var_set_bool(res, FALSE);
            return;
        }

        y = p_ptr->pos.y + ddy[dir];
        x = p_ptr->pos.x + ddx[dir];
        dam = spell_power(damroll(8 + ((p_ptr->lev - 5) / 4) + _force_boost() / 12, 8) + p_ptr->to_d_spell);
        fire_beam(GF_MISSILE, dir, dam);
        mon = mon_at_xy(x, y);
        if (mon)
        {
            int i;
            int ty = y, tx = x;
            int oy = y, ox = x;
            mon_race_ptr race = mon_race(mon);
            char m_name[80];

            monster_desc(m_name, mon, 0);
            if (randint1(race->level * 3 / 2) > randint0(dam / 2) + dam/2)
            {
                msg_format("%^s was not blown away.", m_name);
            }
            else
            {
                for (i = 0; i < 5; i++)
                {
                    y += ddy[dir];
                    x += ddx[dir];
                    if (cave_empty_bold(y, x))
                    {
                        ty = y;
                        tx = x;
                    }
                    else break;
                }
                if (ty != oy || tx != ox)
                {
                    msg_format("You blow %s away!", m_name);

                    dun_move_mon(cave, mon, point_create(tx, ty));

                    if (race->flags7 & (RF7_LITE_MASK | RF7_DARK_MASK))
                        p_ptr->update |= PU_MON_LITE;
                }
            }
        }
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _large_force_ball_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Large Force Ball");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a large energy ball.");
        break;
    case SPELL_INFO:
    {
        int dice = spell_power(10);
        int sides = 6;
        int base = spell_power(p_ptr->lev * 3 / 2 + _force_boost() * 3 / 5 + p_ptr->to_d_spell);
        var_set_string(res, info_damage(dice, sides, base));
        break;
    }
    case SPELL_CAST:
    {
        int dir;
        var_set_bool(res, FALSE);
        if (get_fire_dir(&dir))
        {
            int dice = 10;
            int sides = 6;
            int base = p_ptr->lev * 3 / 2 + _force_boost() * 3 / 5;
            int radius = spell_power((p_ptr->lev < 30) ? 2 : 3);
            int dam = spell_power(damroll(dice, sides) + base + p_ptr->to_d_spell);
            fire_ball(GF_MISSILE, dir, dam, radius);
            var_set_bool(res, TRUE);
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _summon_ghost_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Ghost");
        break;
    case SPELL_DESC:
        var_set_string(res, "Summons ghosts.");
        break;
    case SPELL_CAST:
    {
        int i;
        bool success = FALSE;

        for (i = 0; i < 1 + _force_boost()/100; i++)
            if (summon_specific(-1, p_ptr->pos, p_ptr->lev, SUMMON_PHANTOM, PM_FORCE_PET))
                success = TRUE;
        if (success)
            msg_print("'Your wish, master?'");
        else
            msg_print("Nothing happen.");
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _exploding_flame_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Exploding Flame");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generates a huge ball of flame centered on you.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, spell_power(100 + p_ptr->lev + _force_boost() + p_ptr->to_d_spell)));
        break;
    case SPELL_CAST:
    {
        fire_ball(
            GF_FIRE,
            0,
            spell_power((100 +  p_ptr->lev + _force_boost() + p_ptr->to_d_spell) * 2),
            10
        );
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _super_kamehameha_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Super Kamehameha");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a long, powerful energy beam.");
        break;
    case SPELL_INFO:
    {
        int dice = spell_power(10 + p_ptr->lev/2 + _force_boost()*3/10);
        int sides = 15;
        var_set_string(res, info_damage(dice, sides, spell_power(p_ptr->to_d_spell)));
        break;
    }
    case SPELL_CAST:
    {
        int dir;
        int dice = 10 + p_ptr->lev/2 + _force_boost()*3/10;
        int sides = 15;
        
        var_set_bool(res, FALSE);
        if (!get_fire_dir(&dir)) return;

        fire_beam(
            GF_MANA,
            dir,
            spell_power(damroll(dice, sides) + p_ptr->to_d_spell)
        );
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _light_speed_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Light Speed");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives extremely fast speed.");
        break;
    case SPELL_CAST:
    {
        plr_tim_add(T_LIGHT_SPEED, spell_power(randint1(16) + 16 + _force_boost() / 20));
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

/****************************************************************
 * Spell Table and Exports
 ****************************************************************/

#define MAX_FORCETRAINER_SPELLS    14

static spell_info _spells[MAX_FORCETRAINER_SPELLS] = 
{
    /*lvl cst fail spell */
    { 1,   1,  15, _small_force_ball_spell},
    { 3,   3,  30, light_area_spell},
    { 5,   6,  35, _flying_technique_spell},
    { 8,   5,  40, _kamehameha_spell},
    { 10,  7,  45, _magic_resistance_spell},
    { 13,  5,  60, _improve_force_spell},
    { 17, 17,  50, _aura_of_force_spell},
    { 20, 20,  50, _shock_power_spell},
    { 23, 18,  55, _large_force_ball_spell},
    { 25, 30,  70, dispel_magic_spell},
    { 28, 26,  50, _summon_ghost_spell},
    { 32, 35,  65, _exploding_flame_spell},
    { 38, 42,  75, _super_kamehameha_spell},
    { 44, 50,  80, _light_speed_spell},
};

static int _get_spells(spell_info* spells, int max)
{
    int i, hand;
    int ct = 0;
    int stat_idx = p_ptr->stat_ind[A_WIS];
    int penalty1 = 0;
    int penalty2 = 0;

    /* These penalties should only apply to Force spells ... at the moment, choice
       of a conventional spellbook and realm is handled elsewhere, but should some
       day be moved here. */
    if (heavy_armor()) 
    {
        penalty1 += 20;
        penalty2 += 5;
    }
    for (hand = 0; hand < MAX_HANDS; hand++)
    {
        if (have_flag(p_ptr->attack_info[hand].paf_flags, PAF_ICKY))
        {
            penalty1 += 20;
            penalty2 += 5;
        }
        else if (p_ptr->attack_info[hand].type == PAT_WEAPON)
        {
            penalty1 += 10;
        }
    }
    for (i = 0; i < MAX_FORCETRAINER_SPELLS; i++)
    {
        spell_info *base = &_spells[i];
        if (ct >= max) break;
        if (base->level <= p_ptr->lev)
        {
            spell_info* current = &spells[ct];
            current->fn = base->fn;
            current->level = base->level;
            current->cost = base->cost;

            /* The first penalty can be completely overcome by high level, etc. */
            current->fail = calculate_fail_rate(base->level, base->fail + penalty1, stat_idx);            

            /* But the second penalty effectively boosts the minimum fail rate */
            current->fail += penalty2;
            if (current->fail > 95) current->fail = 95;

            ct++;
        }
    }
    return ct;
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 15;
    spell->cost = 0;
    spell->fail = calculate_fail_rate(spell->level, 30, p_ptr->stat_ind[A_WIS]);
    spell->fn = clear_mind_spell;

    return ct;
}

static void _calc_bonuses(void)
{
    if (p_ptr->lev <= 30)
        p_ptr->monk_lvl = p_ptr->lev;
    else
    {
        int l = p_ptr->lev - 30;
        p_ptr->monk_lvl = 30 + l * 17 / 20;
    }
        
    if (p_ptr->lev >= 15) 
        p_ptr->clear_mind = TRUE;

    if (!(heavy_armor()))
    {
        p_ptr->pspeed += (p_ptr->lev) / 10;
        p_ptr->sh_retaliation = TRUE;
        if  (p_ptr->lev >= 25)
            p_ptr->free_act++;

    }
    monk_ac_bonus();
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (!heavy_armor())
    {
        add_flag(flgs, OF_AURA_REVENGE);
        if (p_ptr->lev >= 10)
            add_flag(flgs, OF_SPEED);
        if (p_ptr->lev >= 25)
            add_flag(flgs, OF_FREE_ACT);
    }
}

static void _on_fail(const spell_info *spell)
{
    /* reset force counter for all spells *except* Improve Force */
    if (spell->fn != _improve_force_spell && p_ptr->magic_num1[0])
    {
        msg_print("Your improved Force has gone away...");
        p_ptr->magic_num1[0] = 0;
        p_ptr->update |= PU_BONUS;
        p_ptr->redraw |= PR_EFFECTS;
    }
}

static void _on_cast(const spell_info *spell)
{
    /* reset force counter for all spells *except* Improve Force */
    if (spell->fn != _improve_force_spell && p_ptr->magic_num1[0])
    {
        p_ptr->magic_num1[0] = 0;
        p_ptr->update |= PU_BONUS;
        p_ptr->redraw |= PR_EFFECTS;
    }
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "force";
        me.which_stat = A_WIS;
        me.encumbrance.max_wgt = 350;
        me.encumbrance.weapon_pct = 100;
        me.encumbrance.enc_wgt = 800;
        me.on_fail = _on_fail;
        me.on_cast = _on_cast;
        me.realm1_choices = CH_LIFE | CH_NATURE | CH_DEATH | CH_ENCHANT | CH_CRUSADE;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    plr_birth_obj_aux(TV_POTION, SV_POTION_CLARITY, rand_range(5, 10));
    plr_birth_spellbooks();
}

static void _character_dump(doc_ptr doc)
{
    spell_info spells[MAX_SPELLS];
    int        ct = _get_spells(spells, MAX_SPELLS);

    spellbook_character_dump(doc);
    plr_display_spells_aux(doc, spells, ct, "<color:B>Force</color>");
}

static void _prt_effects(doc_ptr doc)
{
    if (p_ptr->magic_num1[0])
    {
        int color;
        int force = p_ptr->magic_num1[0];

        if (force > 400) color = TERM_VIOLET;
        else if (force > 300) color = TERM_RED;
        else if (force > 200) color = TERM_L_RED;
        else if (force > 100) color = TERM_YELLOW;
        else color = TERM_L_UMBER;

        doc_printf(doc, "<color:B>Force:</color><color:%c>%d</color>\n", attr_to_attr_char(color), force);
    }
}

plr_class_ptr force_trainer_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  34,  38,   4,  32,  24,  50,  40 };
    skills_t xs = { 10,  11,  11,   0,   0,   0,  14,  15 };

        me = plr_class_alloc(CLASS_FORCETRAINER);
        me->name = "Force-Trainer";
        me->desc = "A ForceTrainer is a master of the spiritual Force. They prefer "
                    "fighting with neither weapon nor armor. They are not as good "
                    "fighters as are Monks, but they can use both magic and the "
                    "spiritual Force. Wielding weapons or wearing heavy armor disturbs "
                    "use of the Force. Wisdom is a ForceTrainer's primary stat.\n \n"
                    "ForceTrainers use both spellbook magic and the special spiritual "
                    "power called the Force. They can select a realm from Life, "
                    "Nature, Craft, Death, and Crusade. The most important spell of "
                    "the Force is 'Improve Force'; each time a ForceTrainer activates "
                    "it, their Force power becomes more powerful, and their attack "
                    "power in bare-handed melee fighting is increased temporarily. The "
                    "strengthened Force can be released at one stroke when a "
                    "ForceTrainer activates some other Force spell, typically an attack "
                    "spell. They have a class power - 'Clear Mind' - which allows them "
                    "to rapidly regenerate their mana.";
        
        me->stats[A_STR] =  0;
        me->stats[A_INT] = -1;
        me->stats[A_WIS] =  3;
        me->stats[A_DEX] =  2;
        me->stats[A_CON] =  1;
        me->stats[A_CHR] =  0;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 100;
        me->base_hp = 4;
        me->exp = 135;
        me->pets = 40;
        me->flags = CLASS_SENSE1_MED | CLASS_SENSE1_WEAK |
                   CLASS_SENSE2_MED | CLASS_SENSE2_STRONG | CLASS_MARTIAL_ARTS;

        me->hooks.birth = _birth;
        me->hooks.register_timers = _register_timers;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_flags = _get_flags;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_spells = _get_spells;
        me->hooks.get_powers = _get_powers;
        me->hooks.character_dump = _character_dump;
        me->hooks.mon_attack_init = _mon_attack_init;
        me->hooks.prt_effects = _prt_effects;
    }

    return me;
}
