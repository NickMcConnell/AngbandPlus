#include "angband.h"

void _precognition_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Precognition");
        break;
    case SPELL_DESC:
    {
        if (plr->lev < 5)
            var_set_string(res, "Detects visible monsters in your vicinity.");
        else if (plr->lev < 15)
            var_set_string(res, "Detects visible monsters, traps, and doors in your vicinity.");
        else if (plr->lev < 20)
            var_set_string(res, "Detects monsters, traps, and doors in your vicinity.");
        else if (plr->lev < 25)
            var_set_string(res, "Detects monsters, traps, and doors in your vicinity and maps nearby area.");
        else if (plr->lev < 30)
            var_set_string(res, "Detects monsters, traps, and doors in your vicinity and maps nearby area. Grants temporary ESP.");
        else if (plr->lev < 40)
            var_set_string(res, "Detects monsters, traps, doors, stairs and objects in your vicinity and maps nearby area. Grants temporary ESP.");
        else if (plr->lev < 45)
            var_set_string(res, "Detects monsters, traps, doors, stairs and objects in your vicinity and maps nearby area.");
        else
            var_set_string(res, "Detects monsters, traps, doors, stairs and objects in your vicinity and maps the entire level.");
        break;
    }
    case SPELL_CAST:
    {
        int b = 0;
        if (plr->lev > 44)
        {
            virtue_add(VIRTUE_KNOWLEDGE, 1);
            virtue_add(VIRTUE_ENLIGHTENMENT, 1);
            wiz_lite();
        }
        else if (plr->lev > 19)
            map_area(DETECT_RAD_MAP);

        if (plr->lev < 30)
        {
            b = detect_monsters_normal(DETECT_RAD_DEFAULT);
            if (plr->lev > 14) b |= detect_monsters_invis(DETECT_RAD_DEFAULT);
            if (plr->lev > 4)  {
                b |= detect_traps(DETECT_RAD_DEFAULT, TRUE);
                b |= detect_doors(DETECT_RAD_DEFAULT);
            }
        }
        else
        {
            b = detect_all(DETECT_RAD_DEFAULT);
        }

        if ((plr->lev > 24) && (plr->lev < 40))
            plr_tim_add(T_TELEPATHY, plr->lev + randint1(plr->lev));

        if (!b) msg_print("You feel safe.");

        var_set_bool(res, TRUE);
        break;
    }
    case SPELL_COST_EXTRA:
    {
        int n = 0;

        if (plr->lev >= 45)
            n += 9;
        else if (plr->lev >= 30)
            n += 4;
        else if (plr->lev >= 25)
            n += 3;
        else if (plr->lev >= 20)
            n += 1;
        else if (plr->lev >= 15)
            n += 0;
        else if (plr->lev >= 5)
            n += 0;

        var_set_int(res, n);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void _neural_blast_spell(int cmd, var_ptr res)
{
    int dd = 3 + (plr->lev - 1)/4;
    int ds = 3 + plr->lev/15;
    dice_t dice = spell_dam_dice(dd, ds, 0);
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Neural Blast");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a beam or ball which inflicts psionic damage.");
        break;
    case SPELL_INFO:
        var_printf(res, "dam ~%d", dice_avg_roll(dice));
        break;
    case SPELL_CAST:
        if (randint1(100) < 2*plr->lev)
            var_set_bool(res, plr_cast_beam(GF_PSI, dice));
        else
            var_set_bool(res, plr_cast_bolt(GF_PSI, dice));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void _minor_displacement_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Minor Displacement");
        break;
    case SPELL_DESC:
        var_set_string(res, "Teleport short distance.");
        break;
    case SPELL_CAST:
        teleport_player(10, 0L);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void _major_displacement_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Major Displacement");
        break;
    case SPELL_DESC:
        var_set_string(res, "Teleport long distance.");
        break;
    case SPELL_CAST:
    {
        teleport_player(plr->lev * 5, 0L);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static int _domination_power(void) /* cf _gaze_power in race_vampire.c */
{
    int power = plr->lev;
    if (plr->lev > 40)
        power += plr->lev - 40;
    power += adj_con_fix[plr->stat_ind[A_CHR]] - 1;
    return power;
}
void _domination_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Domination");
        break;
    case SPELL_DESC:
        var_set_string(res, "Stuns, confuses or scares a monster. Or attempts to charm all monsters in sight at level 30.");
        break;
    default:
        if (plr->lev < 30)
            direct_spell(cmd, res, GF_DOMINATION, _domination_power());
        else
            los_spell(cmd, res, GF_DOMINATION, _domination_power());
    }
}

static int _pulverise_rad(void) {
    if (plr->lev > 20)
        return 1 + (plr->lev - 20)/8;
    return 0;
}
static dice_t _pulverise_dice(void) {
    return spell_dam_dice(8 + (plr->lev - 5)/4, 8, 0);
}
void _pulverise_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Pulverise");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a ball which hurts monsters with telekinesis.");
        break;
    default:
        ball_spell_aux(cmd, res, _pulverise_rad(), GF_TELEKINESIS, _pulverise_dice());
    }
}

void _character_armor_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Character Armour");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives stone skin and some resistance to elements for a while. The level "
                              "increased, the more number of resistances given.");
        break;
    case SPELL_CAST: {
        dice_t dice = spell_dice(1, plr->lev, plr->lev);
        plr_tim_add(T_STONE_SKIN, dice_roll(dice));
        if (plr->lev > 14) plr_tim_add(T_RES_ACID, dice_roll(dice));
        if (plr->lev > 19) plr_tim_add(T_RES_FIRE, dice_roll(dice));
        if (plr->lev > 24) plr_tim_add(T_RES_COLD, dice_roll(dice));
        if (plr->lev > 29) plr_tim_add(T_RES_ELEC, dice_roll(dice));
        if (plr->lev > 34) plr_tim_add(T_RES_POIS, dice_roll(dice));
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

void _psychometry_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Psychometry");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives feeling of an item. Or identify an item at level 25.");
        break;
    case SPELL_CAST:
        if (plr->lev < 25)
            var_set_bool(res, psychometry());
        else
            var_set_bool(res, ident_spell(NULL));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void _mind_wave_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mind Wave");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generate a ball centered on you which inflict monster with PSI damage. "
                              "Or inflict all monsters with PSI damage at level 25.");
        break;
    default:
        if (plr->lev < 25)
            burst_spell(cmd, res, 2 + plr->lev/10, GF_PSI, 3*plr->lev/2);
        else {
            int ds = plr->lev*(1 + (plr->lev - 5)/10);
            los_spell_aux(cmd, res, GF_PSI, spell_dam_dice(1, ds, 0));
        }
    }
}

void _adrenaline_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Adrenaline Channeling");
        break;
    case SPELL_DESC:
        var_set_string(res, "Removes fear and stun. Gives heroism and speed. Heals HP a little unless "
                              "you already have heroism and temporary speed boost.");
        break;
    case SPELL_CAST: {
        dice_t dice = spell_dice(1, 3*plr->lev/2, 15);
        bool heal = !plr_tim_find(T_FAST);

        plr_tim_remove(T_STUN);

        plr_tim_add(T_HERO, dice_roll(dice));
        plr_tim_add(T_FAST, dice_roll(dice));

        if (heal) /* Heal after granting Heroism to fill the +10 mhp */
            hp_player(plr->lev);

        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
    }
}

void _telekinesis_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Telekinesis");
        break;
    case SPELL_DESC:
        var_set_string(res, "Pulls a distant item close to you.");        
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, FALSE);
        if (!get_aim_dir(&dir)) return;

        fetch(dir, plr->lev * 15, FALSE);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static dice_t _drain_dice(void) { return spell_dam_dice(plr->lev/2, 6, 0); }
void _psychic_drain_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Psychic Drain");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a ball which damages monsters and absorbs monsters' mind power. "
                              "Absorbing takes from 0 to 1.5 more turns.");
        break;
    case SPELL_INFO:
        var_printf(res, "dam ~%d", dice_avg_roll(_drain_dice()));
        break;
    case SPELL_CAST: {
        point_t pos = plr_get_ball_target(GF_PSI_DRAIN);
        dice_t  dice = _drain_dice();

        var_set_bool(res, FALSE);
        if (!dun_pos_interior(cave, pos)) return;

        /* Only charge extra energy if the drain succeeded */
        if (plr_ball(0, pos, GF_PSI_DRAIN, dice_roll(dice)))
            plr->energy_need += _1d(150);

        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
    }
}

static dice_t _spear_dice(void) { return spell_dam_dice(1, 3*plr->lev, 3*plr->lev); }
void psycho_spear_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Psycho-Spear");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a beam of pure energy which penetrate the invulnerability barrier.");
        break;
    default:
        beam_spell_aux(cmd, res, GF_PSY_SPEAR, _spear_dice());
    }
}

static dice_t _storm_dice(void) { return spell_dam_dice(10, 10, 5*plr->lev); }
void _psycho_storm_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Psycho-Storm");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a large ball of pure mental energy.");
        break;
    default:
        ball_spell_aux(cmd, res, 4, GF_PSI_STORM, _storm_dice());
    }
}

/****************************************************************
 * Spell Table and Exports
 ****************************************************************/
static spell_info _spells[] = 
{
    /*lvl cst fail spell */
    { 1,   1,  15, _neural_blast_spell},
    { 2,   1,  20, _precognition_spell},
    { 3,   2,  25, _minor_displacement_spell},
    { 7,   6,  35, _major_displacement_spell},
    { 9,   7,  50, _domination_spell},
    { 11,  7,  30, _pulverise_spell},
    { 13, 12,  50, _psychometry_spell},
    { 15, 12,  60, _character_armor_spell},
    { 18, 10,  45, _mind_wave_spell},
    { 23, 15,  50, _adrenaline_spell},
    { 26, 28,  60, _telekinesis_spell},
    { 28, 10,  40, _psychic_drain_spell},
    { 35, 35,  75, psycho_spear_spell},
    { 45, 50,  80, _psycho_storm_spell},
    { -1, -1,  -1, NULL}
};

static power_info _powers[] =
{
    { A_WIS, {15, 0, 30, clear_mind_spell}}, 
    { -1, {-1, -1, -1, NULL}}
};

static int _get_spells(spell_info* spells, int max)
{
    return get_spells_aux(spells, max, _spells);
}

static int _get_powers(spell_info* spells, int max)
{
    return get_powers_aux(spells, max, _powers);
}

static void _calc_bonuses(void)
{
    if (equip_find_art("~.Mind"))
    {
        plr->dec_mana++;
        plr->easy_spell++;
    }

    if (plr->lev >= 10) res_add(GF_FEAR);
    if (plr->lev >= 15) plr->clear_mind = TRUE;
    if (plr->lev >= 20) plr->sustain_wis = TRUE;
    if (plr->lev >= 25) plr->auto_id_sp = 12;
    if (plr->lev >= 30) res_add(GF_CONF);
    if (plr->lev >= 40)
    {
        plr->telepathy = TRUE;
        plr->wizard_sight = TRUE;
    }
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (plr->lev >= 10)
        add_flag(flgs, OF_RES_(GF_FEAR));
    if (plr->lev >= 20)
        add_flag(flgs, OF_SUST_WIS);
    if (plr->lev >= 30)
        add_flag(flgs, OF_RES_(GF_CONF));
    if (plr->lev >= 40)
        add_flag(flgs, OF_TELEPATHY);
}

static void _on_fail(const spell_info *spell)
{
    if (randint1(100) < (spell->fail / 2))
    {
        int b = randint1(100);

        if (b < 5)
        {
            msg_print("Oh, no! Your mind has gone blank!");
            lose_all_info();
        }
        else if (b < 15 && !mut_present(MUT_WEIRD_MIND))
        {
            msg_print("Weird visions seem to dance before your eyes...");
            plr_tim_add(T_HALLUCINATE, 5 + randint1(10));
        }
        else if (b < 45)
        {
            msg_print("Your brain is addled!");
            plr_tim_add(T_CONFUSED, randint1(8));
        }
        else if (b < 90)
        {
            if (!res_save(GF_STUN, 100)) plr_tim_add(T_STUN, randint1(8));
        }
        else
        {
            msg_print("Your mind unleashes its power in an uncontrollable storm!");
            dun_burst(cave, who_create_unctrl_power(), 2 + plr->lev/10, plr->pos, GF_MANA, 2*plr->lev);
            plr->csp = MAX(0, plr->csp - plr->lev * MAX(1, plr->lev / 10));
        }
    }
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "mindcraft";
        me.which_stat = A_WIS;
        me.encumbrance.max_wgt = 400;
        me.encumbrance.weapon_pct = 50;
        me.encumbrance.enc_wgt = 800;
        me.on_fail = _on_fail;
        me.options = CASTER_GAIN_SKILL;
        init = TRUE;
    }
    return &me;
}

static void _character_dump(doc_ptr doc)
{
    spell_info spells[MAX_SPELLS];
    int        ct = _get_spells(spells, MAX_SPELLS);

    plr_display_spells(doc, spells, ct);
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_HAFTED, SV_WAR_HAMMER, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    plr_birth_obj_aux(TV_POTION, SV_POTION_SPEED, rand_range(2, 5));
}

plr_class_ptr mindcrafter_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 30,  33,  38,   3,  22,  16,  50,  35 };
    skills_t xs = { 50,  55,  50,   0,   0,   0,  70,  55 };

        me = plr_class_alloc(CLASS_MINDCRAFTER);
        me->name = "Mindcrafter";
        me->desc = "The Mindcrafter is a unique class that uses the powers of the mind "
                    "instead of magic. These powers are unique to Mindcrafters, and "
                    "vary from simple extrasensory powers to mental domination of "
                    "others. Since these powers are developed by the practice of "
                    "certain disciplines, a Mindcrafter requires no spellbooks to use "
                    "them. The available powers are simply determined by the "
                    "character's level. Wisdom determines a Mindcrafter's ability to "
                    "use mind powers.\n \n"
                    "Mindcrafters gain new mindcrafting powers and their existing ones "
                    "become stronger as they gain levels. They can use their power "
                    "even when blinded. They have a class power - 'Clear Mind' - which "
                    "allows them to rapidly regenerate their mana.";
        me->stats[A_STR] = -1;
        me->stats[A_INT] =  0;
        me->stats[A_WIS] =  3;
        me->stats[A_DEX] = -1;
        me->stats[A_CON] = -1;
        me->stats[A_CHR] =  2;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 100;
        me->base_hp = 4;
        me->exp = 125;
        me->pets = 35;
        me->flags = CLASS_SENSE1_SLOW | CLASS_SENSE1_WEAK |
                    CLASS_SENSE2_MED | CLASS_SENSE2_STRONG;

        me->hooks.birth = _birth;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_flags = _get_flags;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_spells = _get_spells;
        me->hooks.get_powers = _get_powers;
        me->hooks.character_dump = _character_dump;
    }

    return me;
}
