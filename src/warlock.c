/****************************************************************
 * The Warlock
 ****************************************************************/

#include "angband.h"

bool warlock_is_pact_monster(monster_race *r_ptr)
{
    bool is_pact = FALSE;
    /* First, we list symbols for the alliance */
    char* pc = my_strchr(pact_info[p_ptr->psubclass].alliance, r_ptr->d_char);
    if (pc != NULL)
    {
        is_pact = TRUE;
    }
    else
    {
        /* If that fails, we check flags ... I'd prefer to only check flags
            but I'm not sure how accurate the beastiary is ... */
        switch (p_ptr->psubclass)
        {
        case PACT_UNDEAD:
            if (r_ptr->flags3 & RF3_UNDEAD)
                is_pact = TRUE;
            break;

        case PACT_DRAGON:
            if (r_ptr->flags3 & RF3_DRAGON)
                is_pact = TRUE;
            break;

        case PACT_ANGEL:
            /* Angel pact is now all good monsters!!! */
            if (r_ptr->flags3 & RF3_GOOD)
                is_pact = TRUE;
            break;
                
        case PACT_DEMON:
            if (r_ptr->flags3 & RF3_DEMON)
                is_pact = TRUE;
            break;
                
        case PACT_ABERRATION:
            if (r_ptr->flags2 & RF2_HUMAN)
                is_pact = TRUE;                
            break;
        }
    }

    return is_pact;
}

/****************************************************************
 * Private Helpers
 ****************************************************************/

static int _warlock_range(void)
{
    int rng = 5;

    if (p_ptr->lev > 47)
        rng = 8;
    else if (p_ptr->lev > 31)
        rng = 7;
    else if (p_ptr->lev > 15)
        rng = 6;

    return rng; 
}

static int _warlock_dice(void)
{
    return 1 + (p_ptr->lev/5) + (p_ptr->lev * p_ptr->lev * 3/500);
}

static int _warlock_sides(void)
{
    return warlock_damage_sides[p_ptr->stat_ind[A_CHR]];
}

/****************************************************************
 * Private Spells
 ****************************************************************/

static void _basic_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Basic");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires your basic Eldritch Blast.");
        break;
    case SPELL_INFO:
        var_set_string(res, 
            format("dam %dd%d (rng %d)", 
                   _warlock_dice(), 
                   spell_power(_warlock_sides()), 
                    _warlock_range()));
        break;
    case SPELL_CAST:
    {
        int dir = 0;

        var_set_bool(res, FALSE);

        project_length = _warlock_range();
        if (!get_aim_dir(&dir)) return;
        
        fire_ball(GF_ELDRITCH, 
                  dir, 
                  spell_power(damroll(_warlock_dice(), _warlock_sides())), 
                  0);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _extended_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Extended");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires an Eldritch Blast with increased range.");
        break;
    case SPELL_INFO:
        var_set_string(res, 
            format("dam %dd%d (rng %d)", 
                   _warlock_dice(), 
                   spell_power(_warlock_sides()), 
                    _warlock_range() + 10 * p_ptr->lev/50));
        break;
    case SPELL_CAST:
    {
        int dir = 0;

        var_set_bool(res, FALSE);

        project_length = _warlock_range() + 10 * p_ptr->lev/50;
        if (!get_aim_dir(&dir)) return;
        
        fire_ball(GF_ELDRITCH, 
                  dir, 
                  spell_power(damroll(_warlock_dice(), _warlock_sides())), 
                  0);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _spear_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Spear");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires an Eldritch Beam.");
        break;
    case SPELL_INFO:
        var_set_string(res, 
            format("dam %dd%d (rng %d)", 
                   _warlock_dice(), 
                   spell_power(_warlock_sides()), 
                    _warlock_range()));
        break;
    case SPELL_CAST:
    {
        int dir = 0;

        var_set_bool(res, FALSE);

        project_length = _warlock_range();
        if (!get_aim_dir(&dir)) return;
        
        fire_beam(GF_ELDRITCH, 
                  dir, 
                  spell_power(damroll(_warlock_dice(), _warlock_sides())));

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _burst_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Burst");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires an Eldritch Blast with increased radius.");
        break;
    case SPELL_INFO:
        var_set_string(res, 
            format("dam %dd%d (rng %d)", 
                   _warlock_dice(), 
                   spell_power(_warlock_sides()), 
                    _warlock_range()));
        break;
    case SPELL_CAST:
    {
        int dir = 0;

        var_set_bool(res, FALSE);

        project_length = _warlock_range();
        if (!get_aim_dir(&dir)) return;

        fire_ball_aux(
            GF_ELDRITCH, 
            dir, 
            spell_power(damroll(_warlock_dice(), _warlock_sides())), 
            2,
            PROJECT_FULL_DAM
        );
        
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _stunning_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Stunning");
        break;
    case SPELL_DESC:
        var_set_string(res, "Augments your Eldritch Blast with stunning effects.");
        break;
    case SPELL_INFO:
        var_set_string(res, 
            format("dam %dd%d (rng %d)", 
                   _warlock_dice(), 
                   spell_power(_warlock_sides()), 
                    _warlock_range()));
        break;
    case SPELL_CAST:
    {
        int dir = 0;

        var_set_bool(res, FALSE);

        project_length = _warlock_range();
        if (!get_aim_dir(&dir)) return;
        
        fire_ball(GF_ELDRITCH_STUN, 
                  dir, 
                  spell_power(damroll(_warlock_dice(), _warlock_sides())), 
                  0);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _empowered_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Empowered");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a very powerful Eldritch Blast, but you can't use your powers again for a bit.");
        break;
    case SPELL_INFO:
        var_set_string(res, 
            format("dam %dd%d*1.75 (rng %d)", 
                   _warlock_dice(), 
                   spell_power(_warlock_sides()), 
                    _warlock_range()));
        break;
    case SPELL_CAST:
    {
        int dir = 0;

        var_set_bool(res, FALSE);

        project_length = _warlock_range();
        if (!get_aim_dir(&dir)) return;
        
        fire_ball(GF_ELDRITCH, 
                  dir, 
                  spell_power(damroll(_warlock_dice(), _warlock_sides())*7/4), 
                  0);
        set_tim_no_spells(p_ptr->tim_no_spells + 1 + 1, FALSE);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _draining_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Draining");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires an Eldritch Blast which also does Drain Life.");
        break;
    case SPELL_INFO:
        var_set_string(res, 
            format("dam %dd%d (rng %d)", 
                   _warlock_dice(), 
                   spell_power(_warlock_sides()), 
                    _warlock_range()));
        break;
    case SPELL_CAST:
    {
        int dir = 0;

        var_set_bool(res, FALSE);

        project_length = _warlock_range();
        if (!get_aim_dir(&dir)) return;
        
        fire_ball(GF_ELDRITCH_DRAIN, 
                  dir, 
                  spell_power(damroll(_warlock_dice(), _warlock_sides())), 
                  0);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _prismatic_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Prismatic");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires multiple blasts, one each of fire, frost, acid, lightning and poison.");
        break;
    case SPELL_INFO:
        var_set_string(res, 
            format("dam %dd%d*5 (rng %d)", 
                   _warlock_dice(), 
                   spell_power(_warlock_sides()/2), 
                    _warlock_range()));
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        int dice = _warlock_dice();
        int sides = _warlock_sides();

        var_set_bool(res, FALSE);

        project_length = _warlock_range();
        if (!get_aim_dir(&dir)) return;
        
        fire_ball(GF_FIRE, dir, spell_power(damroll(dice, sides)/2), 0);
        fire_ball(GF_COLD, dir, spell_power(damroll(dice, sides)/2), 0);
        fire_ball(GF_ACID, dir, spell_power(damroll(dice, sides)/2), 0);
        fire_ball(GF_ELEC, dir, spell_power(damroll(dice, sides)/2), 0);
        fire_ball(GF_POIS, dir, spell_power(damroll(dice, sides)/2), 0);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _dispelling_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dispelling");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires an Eldritch Blast which also does Dispel Magic.");
        break;
    case SPELL_INFO:
        var_set_string(res, 
            format("dam %dd%d (rng %d)", 
                   _warlock_dice(), 
                   spell_power(_warlock_sides()), 
                    _warlock_range()));
        break;
    case SPELL_CAST:
    {
        int dir = 0;

        var_set_bool(res, FALSE);

        project_length = _warlock_range();
        if (!get_aim_dir(&dir)) return;
        
        fire_ball(GF_ELDRITCH_DISPEL, 
                  dir, 
                  spell_power(damroll(_warlock_dice(), _warlock_sides())), 
                  0);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _vengeful_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Vengeful");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires an extremely deadly Eldritch Blast, but you also take damage.");
        break;
    case SPELL_INFO:
        var_set_string(res, 
            format("dam %dd%d*2 (rng %d)", 
                   _warlock_dice(), 
                   spell_power(_warlock_sides()), 
                    _warlock_range()));
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        int dam = damroll(_warlock_dice(), _warlock_sides());
        dam *= 2;
        dam = spell_power(dam);

        var_set_bool(res, FALSE);

        project_length = _warlock_range();
        if (!get_aim_dir(&dir)) return;

        fire_ball(GF_ELDRITCH, dir, dam, 0);
        take_hit(DAMAGE_USELIFE, 100, "vengeful blast", -1);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _confusing_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Confusing");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires an Eldritch Blast that also confuses your opponent.");
        break;
    case SPELL_INFO:
        var_set_string(res, 
            format("dam %dd%d (rng %d)", 
                   _warlock_dice(), 
                   spell_power(_warlock_sides()), 
                    _warlock_range()));
        break;
    case SPELL_CAST:
    {
        int dir = 0;

        var_set_bool(res, FALSE);

        project_length = _warlock_range();
        if (!get_aim_dir(&dir)) return;
        
        fire_ball(GF_ELDRITCH_CONFUSE, 
                  dir, 
                  spell_power(damroll(_warlock_dice(), _warlock_sides())), 
                  0);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

/****************************************************************
 * Private Powers
 ****************************************************************/

void _dragon_breath_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dragon Breath");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes a powerful elemental breath.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("dam %d", spell_power(400)));
        break;
    case SPELL_CAST:
    {
        int  type = 0, dir = 0;
        cptr desc = NULL;

        var_set_bool(res, FALSE);
        if (!get_aim_dir(&dir)) return;

        switch (randint0(5))
        {
        case 0: type = GF_FIRE; desc = "fire"; break;
        case 1: type = GF_COLD; desc = "cold"; break;
        case 2: type = GF_ELEC; desc = "lightning"; break;
        case 3: type = GF_ACID; desc = "acid"; break;
        case 4: type = GF_POIS; desc = "poison"; break;
        }


        msg_format("You breathe %s.", desc);
        fire_ball(type, dir, spell_power(400), -(p_ptr->lev / 15) - 1);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void _invulnerability_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Invulnerability");
        break;
    case SPELL_DESC:
        var_set_string(res, "Invulnerability!! Nothing can touch you!  Nothing can hurt you!  You become *Invincible*!!!");
        break;
    case SPELL_CAST:
        set_invuln(2 + randint1(2), FALSE);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void _wraithform_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Wraithform");
        break;
    case SPELL_DESC:
        var_set_string(res, "Leave the world of the living and travel the shadows of the underwold.  You gain passwall and great resistance to damage.");
        break;
    case SPELL_CAST:
        set_wraith_form(2 + randint1(2), FALSE);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

/****************************************************************
 * Spell Table and Exports
 ****************************************************************/

#define MAX_WARLOCK_SPELLS    7

static spell_info _spells[MAX_WARLOCK_SPELLS] = 
{
    /*lvl cst fail spell */
    {  1,  0,  20, _basic_spell},
    { 10,  0,  40, _extended_spell},
    { 18,  0,  45, _spear_spell},
    { 26,  0,  60, _burst_spell},
    { 33,  0,  60, _stunning_spell},
    { 40,  0,  70, NULL},
    { 45,  0,  75, _empowered_spell},
};

static ang_spell _pact_spells[MAX_PACTS] = 
{
    _draining_spell,
    _prismatic_spell,
    _dispelling_spell,
    _vengeful_spell,
    _confusing_spell,
};

static int _get_spells(spell_info* spells, int max)
{
    int i;
    int ct = 0;
    int stat_idx = p_ptr->stat_ind[A_CHR];

    for (i = 0; i < MAX_WARLOCK_SPELLS; i++)
    {
        spell_info *base = &_spells[i];
        if (ct >= max) break;
        if (base->level <= p_ptr->lev)
        {
            spell_info* current = &spells[ct];
            current->fn = base->fn;
            current->level = base->level;
            current->cost = base->cost;
            current->fail = calculate_fail_rate(base->level, base->fail, stat_idx);            

            /* Hack for the pact variable slot */
            if (current->fn == NULL)
                current->fn = _pact_spells[p_ptr->psubclass];

            ct++;
        }
    }
    return ct;
}

int _undead_get_powers(spell_info* spells, int max)
{
    int ct = 0;
    
    spell_info *spell = &spells[ct++];
    spell->level = 5;
    spell->cost = 5;
    spell->fail = calculate_fail_rate(5, 60, p_ptr->stat_ind[A_CHR]);
    spell->fn = satisfy_hunger_spell;

    spell = &spells[ct++];
    spell->level = 20;
    spell->cost = 20;
    spell->fail = calculate_fail_rate(20, 80, p_ptr->stat_ind[A_CHR]);
    spell->fn = restore_life_spell;

    spell = &spells[ct++];
    spell->level = 50;
    spell->cost = 100;
    spell->fail = calculate_fail_rate(50, 80, p_ptr->stat_ind[A_CHR]);
    spell->fn = _wraithform_spell;

    return ct;
}

int _dragon_get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info *spell = &spells[ct++];
    spell->level = 5;
    spell->cost = 5;
    spell->fail = calculate_fail_rate(5, 60, p_ptr->stat_ind[A_CHR]);
    spell->fn = detect_objects_spell;

    spell = &spells[ct++];
    spell->level = 15;
    spell->cost = 10;
    spell->fail = calculate_fail_rate(15, 60, p_ptr->stat_ind[A_CHR]);
    spell->fn = heroism_spell;

    spell = &spells[ct++];
    spell->level = 20;
    spell->cost = 20;
    spell->fail = calculate_fail_rate(20, 70, p_ptr->stat_ind[A_CHR]);
    spell->fn = identify_spell;

    spell = &spells[ct++];
    spell->level = 35;
    spell->cost = 40;
    spell->fail = calculate_fail_rate(35, 70, p_ptr->stat_ind[A_CHR]);
    spell->fn = stone_skin_spell;

    spell = &spells[ct++];
    spell->level = 50;
    spell->cost = 15;
    spell->fail = calculate_fail_rate(50, 60, p_ptr->stat_ind[A_CHR]);
    spell->fn = massacre_spell;

    return ct;
}

int _angel_get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info *spell = &spells[ct++];
    spell->level = 5;
    spell->cost = 5;
    spell->fail = calculate_fail_rate(5, 40, p_ptr->stat_ind[A_CHR]);
    spell->fn = light_area_spell;

    spell = &spells[ct++];
    spell->level = 20;
    spell->cost = 20;
    spell->fail = calculate_fail_rate(20, 60, p_ptr->stat_ind[A_CHR]);
    if (p_ptr->lev >= 45)
        spell->fn = remove_curse_II_spell;
    else
        spell->fn = remove_curse_I_spell;

    spell = &spells[ct++];
    spell->level = 30;
    spell->cost = 10;
    spell->fail = calculate_fail_rate(30, 60, p_ptr->stat_ind[A_CHR]);
    spell->fn = earthquake_spell;

    spell = &spells[ct++];
    spell->level = 35;
    spell->cost = 40;
    spell->fail = calculate_fail_rate(35, 60, p_ptr->stat_ind[A_CHR]);
    spell->fn = protection_from_evil_spell;

    spell = &spells[ct++];
    spell->level = 35;
    spell->cost = 70;
    spell->fail = calculate_fail_rate(35, 60, p_ptr->stat_ind[A_CHR]);
    spell->fn = destruction_spell;

    spell = &spells[ct++];
    spell->level = 50;
    spell->cost = 100;
    spell->fail = calculate_fail_rate(50, 90, p_ptr->stat_ind[A_CHR]);
    spell->fn = _invulnerability_spell;

    return ct;
}

int _demon_get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info *spell = &spells[ct++];
    spell->level = 5;
    spell->cost = 5;
    spell->fail = calculate_fail_rate(5, 40, p_ptr->stat_ind[A_CHR]);
    spell->fn = phase_door_spell;

    spell = &spells[ct++];
    spell->level = 20;
    spell->cost = 10;
    spell->fail = calculate_fail_rate(20, 50, p_ptr->stat_ind[A_CHR]);
    spell->fn = teleport_spell;

    spell = &spells[ct++];
    spell->level = 35;
    spell->cost = 40;
    spell->fail = calculate_fail_rate(35, 70, p_ptr->stat_ind[A_CHR]);
    spell->fn = recharging_spell;

    return ct;
}

int _aberration_get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info *spell = &spells[ct++];
    spell->level = 5;
    spell->cost = 5;
    spell->fail = calculate_fail_rate(5, 40, p_ptr->stat_ind[A_CHR]);
    spell->fn = detect_monsters_spell;

    spell = &spells[ct++];
    spell->level = 15;
    spell->cost = 10;
    spell->fail = calculate_fail_rate(15, 40, p_ptr->stat_ind[A_CHR]);
    spell->fn = detect_doors_stairs_traps_spell;

    spell = &spells[ct++];
    spell->level = 20;
    spell->cost = 20;
    spell->fail = calculate_fail_rate(20, 50, p_ptr->stat_ind[A_CHR]);
    spell->fn = magic_mapping_spell;

    spell = &spells[ct++];
    spell->level = 30;
    spell->cost = 30;
    spell->fail = calculate_fail_rate(30, 50, p_ptr->stat_ind[A_CHR]);
    spell->fn = polymorph_self_spell;

    spell = &spells[ct++];
    spell->level = 45;
    spell->cost = 0;
    spell->fail = 0;
    spell->fn = create_ammo_spell;

    spell = &spells[ct++];
    spell->level = 50;
    spell->cost = 20;
    spell->fail = calculate_fail_rate(50, 65, p_ptr->stat_ind[A_CHR]);
    spell->fn = dimension_door_spell;

    return ct;
}

static void _undead_calc_bonuses(void)
{
    res_add(RES_COLD);
    p_ptr->skills.stl += 7 * p_ptr->lev/50;
    if (p_ptr->lev > 14) res_add(RES_POIS);
    p_ptr->stat_add[A_CON] += 5 * p_ptr->lev/50;
    if (p_ptr->lev > 29) 
    {
        res_add(RES_NETHER);
        p_ptr->hold_life = TRUE;
    }
    if (p_ptr->lev > 34) 
    {
        res_add(RES_DARK);
        res_add(RES_BLIND);
    }
    if (p_ptr->lev > 44) res_add(RES_SHARDS);
}

static void _dragon_calc_bonuses(void)
{
    res_add(RES_FEAR);
    p_ptr->skills.thn += 100 * p_ptr->lev / 50;
    /*if (p_ptr->lev > 14) p_ptr->levitation = TRUE; */
    p_ptr->stat_add[A_STR] += 5 * p_ptr->lev / 50;
    p_ptr->weapon_info[0].to_h += 10 * p_ptr->lev / 50;
    p_ptr->weapon_info[0].dis_to_h +=  10 * p_ptr->lev / 50;
    p_ptr->weapon_info[0].to_d += 10 * p_ptr->lev / 50;
    p_ptr->weapon_info[0].dis_to_d += 10 * p_ptr->lev / 50;
    p_ptr->weapon_info[1].to_h += 10 * p_ptr->lev / 50;
    p_ptr->weapon_info[1].dis_to_h +=  10 * p_ptr->lev / 50;
    p_ptr->weapon_info[1].to_d += 10 * p_ptr->lev / 50;
    p_ptr->weapon_info[1].dis_to_d += 10 * p_ptr->lev / 50;
    if (p_ptr->lev > 29) p_ptr->sustain_con = TRUE;
    if (p_ptr->lev > 29)
    {
        /* only give it if they don't already have it */
        if (!mut_present(MUT_RESIST))
        {
            mut_gain(MUT_RESIST);
            mut_lock(MUT_RESIST);
        }
    }
    /* only remove it if they got it from us ... hey, they could have used !Poly */
    else if (mut_present(MUT_RESIST) && mut_locked(MUT_RESIST))
    {
        mut_unlock(MUT_RESIST);
        mut_lose(MUT_RESIST);
    }
    if (p_ptr->lev > 44)
    {
        /* only give it if they don't already have it */
        if (!mut_present(MUT_BERSERK))
        {
            mut_gain(MUT_BERSERK);
            mut_lock(MUT_BERSERK);
        }
    }
    /* only remove it if they got it from us ... hey, they could have used !Poly */
    else if (mut_present(MUT_BERSERK) && mut_locked(MUT_BERSERK))
    {
        mut_unlock(MUT_BERSERK);
        mut_lose(MUT_BERSERK);
    }
}

static void _angel_calc_bonuses(void)
{
    p_ptr->levitation = TRUE;
    p_ptr->skills.sav += 30 * p_ptr->lev/50;
    if (p_ptr->lev > 14) p_ptr->see_inv = TRUE;
    p_ptr->stat_add[A_WIS] += 5 * p_ptr->lev/50;
    if (p_ptr->lev > 34) p_ptr->reflect = TRUE;
}

static void _demon_calc_bonuses(void)
{
    res_add(RES_FIRE);
    p_ptr->skills.dev += 50 * p_ptr->lev/50;
    p_ptr->device_power += 5 * p_ptr->lev/50;
    if (p_ptr->lev > 14) p_ptr->hold_life = TRUE;
    p_ptr->stat_add[A_INT] += 5 * p_ptr->lev/50;
    if (p_ptr->lev >= 30)
        p_ptr->no_eldritch = TRUE;
    if (p_ptr->lev >= 40)
        p_ptr->no_charge_drain = TRUE;
    if (p_ptr->lev > 44)
        p_ptr->kill_wall = TRUE;
    if (p_ptr->lev > 49)
        res_add_immune(RES_FIRE);
}

static void _aberration_calc_bonuses(void)
{
    res_add(RES_CHAOS);
    p_ptr->skills.thb += 100 * p_ptr->lev/50;
    p_ptr->stat_add[A_DEX] += 5 * p_ptr->lev/50;
    if (p_ptr->lev > 34) p_ptr->telepathy = TRUE;    /* Easier then granting MUT3_ESP :) */
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "blast";
        me.which_stat = A_CHR;
        me.options = CASTER_NO_SPELL_COST;
        me.weight = 500;
        init = TRUE;
    }
    return &me;
}

class_t *warlock_get_class_t(int psubclass)
{
    static class_t me = {0};
    static bool init = FALSE;
    static int pact_init = -1;

    /* static info never changes */
    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 20,  24,  34,   1,  16,  20,  34,  20};
    skills_t xs = {  8,  10,  11,   0,   0,   0,  10,   8};

        me.name = "Warlock";
        me.desc = "A Warlock, unlike typical mages, derives his powers from pacts with "
                  "arcane creatures, rather than through careful study. Warlocks possess "
                  "the unique offensive power of the Eldritch Blast, and they use this "
                  "without paying any sort of mana or hitpoint costs (though they still have "
                  "fail rates). The Warlock's primary spell stat is Charisma, and this "
                  "directly influences the damage of their Eldritch Blast.\n \n"
                  "Each Warlock makes a pact with a single type of creature: "
                  "Undead, Dragons, Angels, Demons or Aberrations. This pact confers both "
                  "advantages and disadvantages. For one thing, Warlocks are very ineffective "
                  "when fighting monsters with whom they have formed an alliance. Every form "
                  "of damage is actually reduced by a substantial amount, and this makes killing "
                  "pact monsters extremely difficult. Probably, the Warlock should avoid fighting "
                  "these monsters altogether. Indeed, at high levels pact monsters are often friendly to the "
                  "warlock, but some fights cannot be avoided it seems. That's the bad news. "
                  "The good news is that each pact confers unique abilities and powers to the warlock. "
                  "These benefits reflect the nature of the pact monster. For example, a pact with "
                  "Undead confers resistance to Poison and Nether as well as Hold Life, among other "
                  "things. A pact with Dragons confers the power to resist the elements as well as "
                  "strong melee (Warlocks are generally poor fighters otherwise). In addition, each "
                  "pact gives a unique high level offensive power when using their Eldritch Blast.";


        me.stats[A_STR] = -2;
        me.stats[A_INT] =  1;
        me.stats[A_WIS] =  2;
        me.stats[A_DEX] = -1;
        me.stats[A_CON] = -2;
        me.stats[A_CHR] =  4;

        me.base_skills = bs;
        me.extra_skills = xs;

        me.life = 100;
        me.base_hp = 4;
        me.exp = 125;
        me.pets = 25;

        me.caster_info = _caster_info;
        me.get_spells = _get_spells;
        init = TRUE;
    }

    if (pact_init != psubclass)
    {
        switch (psubclass)
        {
        case PACT_UNDEAD:
            me.calc_bonuses = _undead_calc_bonuses;
            me.get_powers = _undead_get_powers;
            break;
        case PACT_DRAGON:
            me.calc_bonuses = _dragon_calc_bonuses;
            me.get_powers = _dragon_get_powers;
            break;
        case PACT_ANGEL:
            me.calc_bonuses = _angel_calc_bonuses;
            me.get_powers = _angel_get_powers;
            break;
        case PACT_DEMON:
            me.calc_bonuses = _demon_calc_bonuses;
            me.get_powers = _demon_get_powers;
            break;
        case PACT_ABERRATION:
            me.calc_bonuses = _aberration_calc_bonuses;
            me.get_powers = _aberration_get_powers;
            break;
        }
        pact_init = psubclass;
    }

    return &me;
}

