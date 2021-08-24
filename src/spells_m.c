#include "angband.h"

void magic_mapping_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Magic Mapping");
        break;
    case SPELL_DESC:
        var_set_string(res, "Maps the dungeon in your vicinity.");
        break;
    case SPELL_CAST:
        map_area(DETECT_RAD_MAP);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_magic_mapping(void) { return cast_spell(magic_mapping_spell); }

void magic_missile_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Magic Missile");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a weak bolt of unresistable magic.");
        break;
    default:
        bolt_spell(cmd, res, GF_MISSILE, 3 + (plr->lev - 1)/5, 4);
    }
}
bool cast_magic_missile(void) { return cast_spell(magic_missile_spell); }

static dice_t _malediction_dice(void) {
    return spell_dam_dice(3 + (plr->lev - 1)/5, 4, 0);
}
static bool _malediction(void) {
    dice_t dice = _malediction_dice();
    point_t p = get_fire_pos_aux(TARGET_KILL | TARGET_BALL);
    if (!dun_pos_interior(plr_dun(), p)) return FALSE;
    plr_ball(0, p, GF_HELL_FIRE, dice_roll(dice));
    if (one_in_(5))
    {
        int effect = randint1(1000);
        if (effect == 666)
            plr_ball_hide(0, p, GF_DEATH_RAY, 200*plr->lev);
        else if (effect < 500)
            plr_ball_hide(0, p, GF_FEAR, plr->lev);
        else if (effect < 800)
            plr_ball_hide(0, p, GF_OLD_CONF, dice_roll(dice));
        else
            plr_ball_hide(0, p, GF_STUN, dice_roll(dice));
    }
    return TRUE;
}
void malediction_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Malediction");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a tiny ball of evil power which hurts good monsters greatly.");
        break;
    case SPELL_INFO:
        var_printf(res, "dam ~%d", dice_avg_roll(_malediction_dice()));
        break;
    case SPELL_CAST:
        var_set_bool(res, _malediction());
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void mana_branding_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mana Branding");
        break;
    case SPELL_DESC:
        var_set_string(res, "Makes current weapon some elemental branded. You must wield weapons.");
        break;
    case SPELL_CAST:
        var_set_bool(res, choose_ele_attack());
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void mana_bolt_I_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mana Bolt");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a bolt of pure mana.");
        break;
    default:
        bolt_spell_aux(cmd, res, GF_MANA, spell_dam_dice(1, 7*plr->lev/2, plr->lev));
    }
}

void mana_bolt_II_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mana Bolt");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a powerful bolt of pure mana.");
        break;
    default:
        bolt_spell_aux(cmd, res, GF_MANA, spell_dam_dice(1, 7*plr->lev, 2*plr->lev));
    }
}

void mana_storm_I_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mana Storm");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a large ball of pure mana.");
        break;
    default:
        ball_spell_aux(cmd, res, 4, GF_MANA, spell_dam_dice(10, 10, 5*plr->lev));
    }
}

void mana_storm_II_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mana Storm");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a large ball of pure mana.");
        break;
    default:
        ball_spell_aux(cmd, res, 4, GF_MANA, spell_dam_dice(10, 10, 8*plr->lev));
    }
}

void massacre_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Massacre");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack all adjacent monsters in a fit of wild, uncontrollable fury.");
        break;
    case SPELL_CAST: {
        int i;
        for (i = 0; i < 8; i++)
        {
            point_t p = point_step(plr->pos, ddd[i]);
            mon_ptr mon = dun_mon_at(cave, p);
            if (mon && (mon->ml || dun_allow_project_at(cave, p)))
                plr_attack_normal(p);
        }
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

void mind_blast_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Mind Blast");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to blast your opponent with psionic energy.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You gain the power of Mind Blast.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You lose the power of Mind Blast.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can Mind Blast your enemies.");
        break;
    default:
        bolt_spell(cmd, res, GF_PSI, 3 + (plr->lev - 1)/5, 3);
    }
}
bool cast_mind_blast(void) { return cast_spell(mind_blast_spell); }

void nature_awareness_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Nature Awareness");
        break;
    case SPELL_DESC:
        var_set_string(res, "Maps nearby area. Detects all monsters, traps, doors and stairs.");
        break;
    case SPELL_CAST:
        map_area(DETECT_RAD_MAP);
        detect_traps(DETECT_RAD_DEFAULT, TRUE);
        detect_doors(DETECT_RAD_DEFAULT);
        detect_stairs(DETECT_RAD_DEFAULT);
        detect_recall(DETECT_RAD_DEFAULT);
        detect_monsters_normal(DETECT_RAD_DEFAULT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void nether_ball_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Nether Ball");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a huge ball of nether.");
        break;
    default:
        ball_spell(cmd, res, 2 + plr->lev/20, GF_NETHER, 50 + plr_prorata_level(150));
    }
}

void nether_bolt_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Nether Bolt");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a bolt or beam of nether.");
        break;
    default:
        bolt_or_beam_spell(cmd, res, GF_NETHER, 5 + (plr->lev - 5)/4, 8);
    }
}

void orb_of_entropy_spell(int cmd, var_ptr res)
{
    int base;

    if (plr->pclass == CLASS_MAGE || plr->pclass == CLASS_HIGH_MAGE || plr->pclass == CLASS_SORCERER || plr->pclass == CLASS_YELLOW_MAGE || plr->pclass == CLASS_GRAY_MAGE)
        base = plr->lev + plr->lev / 2;
    else
        base = plr->lev + plr->lev / 4;

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Orb of Entropy");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a ball which damages living monsters.");
        break;
    default:
        ball_spell_aux(cmd, res, 2 + plr->lev/30, GF_OLD_DRAIN, spell_dam_dice(3, 6, base));
    }
}

void panic_hit_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Panic Hit");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent monster and attempt a getaway.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You suddenly understand how thieves feel.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You no longer feel jumpy.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can run for your life after hitting something.");
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_attack_special(PLR_HIT_TELEPORT, 0));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_panic_hit(void) { return cast_spell(panic_hit_spell); }

void paralyze_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Paralyze");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to freeze a monster.");
        break;
    default:
        direct_spell(cmd, res, GF_STASIS, plr_prorata_level(100));
    }
}

void pattern_mindwalk_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Pattern Mindwalking");
        break;
    case SPELL_DESC:
        var_set_string(res, "Walk the pattern in your mind. Restores life and stats.");
        break;
    case SPELL_CAST:
        msg_print("You picture the Pattern in your mind and walk it...");

        plr_tim_remove(T_POISON);
        plr_tim_remove(T_HALLUCINATE);
        plr_tim_remove(T_STUN);
        plr_tim_remove(T_CUT);
        plr_tim_remove(T_BLIND);
        fear_clear_p();
        do_res_stat(A_STR);
        do_res_stat(A_INT);
        do_res_stat(A_WIS);
        do_res_stat(A_DEX);
        do_res_stat(A_CON);
        do_res_stat(A_CHR);
        restore_level();
        plr_restore_life(1000);

        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void perception_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Perception");
        break;
    default:
        identify_spell(cmd, res);
        break;
    }
}

void phase_door_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Phase Door");
        break;
    case SPELL_DESC:
        var_set_string(res, "A short range teleport.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You gain the power of minor teleportation.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You lose the power of minor teleportation.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can teleport yourself short distances.");
        break;
    case SPELL_CAST:
        teleport_player(10, 0);
        var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        if (mut_present(MUT_ASTRAL_GUIDE))
        {
            var_set_int(res, 30);
            break;
        }
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_phase_door(void) { return cast_spell(phase_door_spell); }

void plasma_ball_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Plasma Ball");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a ball of plasma.");
        break;
    default:
        ball_spell(cmd, res, 2 + plr->lev/40, GF_PLASMA, 80 + 3*plr->lev/2);
    }
}

void plasma_bolt_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Plasma Bolt");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a bolt or beam of plasma.");
        break;
    default:
        bolt_or_beam_spell(cmd, res, GF_PLASMA, 11 + plr->lev/4, 8);
    }
}

void poison_dart_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Poison Dart");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a poison dart at a single foe.");
        break;
    default:
        bolt_spell_aux(cmd, res, GF_POIS, innate_dice(0, 0, plr->lev));
    }
}

void polish_shield_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Polish Shield");
        break;
    case SPELL_DESC:
        var_set_string(res, "Makes your shield reflect missiles and bolt spells.");
        break;
    case SPELL_CAST:
        var_set_bool(res, polish_shield());
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_polish_shield(void) {    return cast_spell(polish_shield_spell); }

void polymorph_colossus_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Polymorph Colossus");
        break;
    case SPELL_DESC:
        var_set_string(res, "Mimic a Colossus for a while. Loses abilities of original race and gets great abilities as a colossus.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(spell_power(15), spell_power(15)));
        break;
    case SPELL_CAST:
    {
        int base = spell_power(15);
        set_mimic(base + randint1(base), MIMIC_COLOSSUS, FALSE);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
void polymorph_mithril_golem_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Polymorph Mithril Golem");
        break;
    case SPELL_DESC:
        var_set_string(res, "Mimic a Colossus for a while. Loses abilities of original race and gets great abilities as a mithril golem.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(spell_power(15), spell_power(15)));
        break;
    case SPELL_CAST:
    {
        int base = spell_power(15);
        set_mimic(base + randint1(base), MIMIC_MITHRIL_GOLEM, FALSE);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}


void polymorph_demon_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Polymorph Demon");
        break;
    case SPELL_DESC:
        var_set_string(res, "Mimic a demon for a while. Loses abilities of original race and gets abilities as a demon.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(spell_power(15), spell_power(15)));
        break;
    case SPELL_CAST:
    {
        int base = spell_power(10 + plr->lev / 2);
        set_mimic(base + randint1(base), MIMIC_DEMON, FALSE);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void polymorph_demonlord_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Polymorph Demonlord");
        break;
    case SPELL_DESC:
        var_set_string(res, "Mimic a demon lord for a while. Loses abilities of original race and gets great abilities as a demon lord. Even hard walls can't stop your walking.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(spell_power(15), spell_power(15)));
        break;
    case SPELL_CAST:
    {
        int base = spell_power(15);
        set_mimic(base + randint1(base), MIMIC_DEMON_LORD, FALSE);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void polymorph_self_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Polymorph");
        break;
    case SPELL_DESC:
        var_set_string(res, "Mutates yourself. This can be dangerous!");
        break;
    case SPELL_GAIN_MUT:
        msg_print("Your body seems mutable.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("Your body seems stable.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can polymorph yourself at will.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (get_check("You will polymorph yourself. Are you sure? "))
        {
            do_poly_self();
            var_set_bool(res, TRUE);
        }
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_polymorph_self(void) { return cast_spell(polymorph_self_spell); }

void polymorph_vampire_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Polymorph Vampire");
        break;
    case SPELL_DESC:
        var_set_string(res, "Mimic a powerful vampire for a while. Loses abilities of original race and gets abilities as a vampire.");
        break;
    case SPELL_INFO:
    {
        int base = spell_power(10 + plr->lev / 2);
        var_set_string(res, info_duration(base, base));
        break;
    }
    case SPELL_CAST:
    {
        int base = spell_power(10 + plr->lev / 2);
        set_mimic(base + randint1(base), MIMIC_VAMPIRE, FALSE);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void power_throw_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Throw Object");
        break;
    case SPELL_DESC:
        var_set_string(res, "Hurl an object with great force.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("Your throwing arm feels much stronger.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("Your throwing arm feels much weaker.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can hurl objects with great force.");
        break;
    case SPELL_CALC_BONUS:
        plr->mighty_throw = TRUE;
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_power_throw(void) { return cast_spell(power_throw_spell); }

void probing_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Probe Monster");
        break;
    case SPELL_DESC:
        var_set_string(res, "Determines the abilities, strengths and weaknesses of nearby monsters.");
        break;
    case SPELL_CAST:
        probing();
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_probing(void) { return cast_spell(probing_spell); }

void protection_from_evil_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Protection from Evil");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to prevent evil monsters from attacking you. When a weak evil monster melees you, it may be repelled by the forces of good.");
        break;
    case SPELL_CAST:
        plr_tim_add(T_PROT_EVIL, randint1(3 * plr->lev) + 25);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_protection_from_evil(void) { return cast_spell(protection_from_evil_spell); }

void punishment_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Punishment");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a bolt or beam of lightning.");
        break;
    default:
        bolt_spell(cmd, res, GF_ELEC, 3 + (plr->lev - 1)/5, 4);
    }
}

void radiation_ball_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Radiation Ball");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a ball of radiation.");
        break;
    default:
        ball_spell_aux(cmd, res, 2, GF_NUKE, innate_dice(10, 6, 2*plr->lev));
    }
}

void radiation_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Emit Radiation");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generates a huge ball of radiation centered on you.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You start emitting hard radiation.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You stop emitting hard radiation.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can emit hard radiation at will.");
        break;
    case SPELL_INFO:
        var_printf(res, "dam %d", plr->lev);
        break;
    case SPELL_CAST:
        msg_print("Radiation flows from your body!");
        plr_burst(3 + plr->lev/30, GF_NUKE, plr->lev);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
    }
}
bool cast_radiation(void) { return cast_spell(radiation_spell); }

void ray_of_sunlight_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Ray of Sunlight");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a beam of light which damages to light-sensitive monsters.");
        break;
    default:
        beam_spell(cmd, res, GF_LIGHT_WEAK, 6, 8);
    }
}

void recall_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Recall");
        break;
    case SPELL_DESC:
        var_set_string(res, "Travel back and forth between the town and the dungeon.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You feel briefly homesick, but it passes.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You feel briefly homesick.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can travel between town and the depths.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (dun_mgr_recall_plr())
            var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_recall(void) { return cast_spell(recall_spell); }

void recharging_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Recharging");
        break;
    case SPELL_DESC:
        if (plr->prace == RACE_MON_LEPRECHAUN)
            var_set_string(res, "It attempts to recharge a device using your gold for power.");
        else if (!plr->msp)
            var_set_string(res, "It attempts to recharge a device using another device for power.");
        else
            var_set_string(res, "It attempts to recharge a device using your mana for power.");
        break;
    case SPELL_CAST:
        if (plr->prace == RACE_MON_LEPRECHAUN)
            var_set_bool(res, recharge_from_player(2 * plr->lev));
        else if (!plr->msp)
            var_set_bool(res, recharge_from_device(3 * plr->lev));
        else
            var_set_bool(res, recharge_from_player(3 * plr->lev));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_recharging(void) { return cast_spell(recharging_spell); }

void remove_curse_I_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Remove Curse");
        break;
    case SPELL_DESC:
        var_set_string(res, "Uncurses an item so that you may remove it.");
        break;
    case SPELL_CAST:
        if (remove_curse())
            msg_print("You feel as if someone is watching over you.");
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_remove_curse_I(void) { return cast_spell(remove_curse_I_spell); }

void remove_curse_II_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "*Remove Curse*");
        break;
    case SPELL_DESC:
        var_set_string(res, "Uncurses an item so that you may remove it. Even heavily cursed items can be removed.");
        break;
    case SPELL_CAST:
        if (remove_all_curse())
            msg_print("You feel as if someone is watching over you.");
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_remove_curse_II(void) { return cast_spell(remove_curse_II_spell); }

void remove_fear_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Remove Fear");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_CAST:
        fear_clear_p();
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_remove_fear(void) { return cast_spell(remove_fear_spell); }

void resistance_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Resistance");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives resistance to fire, cold, electricity, acid and poison for a while.");
        break;
    case SPELL_CAST:
    {
        int base = spell_power(20);

        plr_tim_add(T_RES_ACID, randint1(base) + base);
        plr_tim_add(T_RES_ELEC, randint1(base) + base);
        plr_tim_add(T_RES_FIRE, randint1(base) + base);
        plr_tim_add(T_RES_COLD, randint1(base) + base);
        plr_tim_add(T_RES_POIS, randint1(base) + base);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void resist_elements_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Resist Elements");
        break;
    case SPELL_DESC:
        var_set_string(res, "Protect yourself from the ravages of the elements.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You feel like you can protect yourself.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You feel like you might be vulnerable.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can harden yourself to the ravages of the elements.");
        break;
    case SPELL_COST_EXTRA:
    {
        int n = 0;
        if (plr->lev >= 20)
            n += 5;
        if (plr->lev >= 30)
            n += 5;
        if (plr->lev >= 40)
            n += 5;
        if (plr->lev >= 50)
            n += 5;
        var_set_int(res, n);
        break;
    }
    case SPELL_CAST:
    {
        int num = plr->lev / 10;
        int dur = randint1(20) + 20;

        if (randint0(5) < num)
        {
            plr_tim_add(T_RES_ACID, dur);
            num--;
        }
        if (randint0(4) < num)
        {
            plr_tim_add(T_RES_ELEC, dur);
            num--;
        }
        if (randint0(3) < num)
        {
            plr_tim_add(T_RES_FIRE, dur);
            num--;
        }
        if (randint0(2) < num)
        {
            plr_tim_add(T_RES_COLD, dur);
            num--;
        }
        if (num)
        {
            plr_tim_add(T_RES_POIS, dur);
            num--;
        }
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_resist_elements(void) { return cast_spell(resist_elements_spell); }

void resist_environment_spell(int cmd, var_ptr res)
{
    int base = spell_power(20);
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Resist Environment");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives resistance to fire, cold and electricity for a while.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(base, base));
        break;
    case SPELL_CAST:
        plr_tim_add(T_RES_COLD, randint1(base) + base);
        plr_tim_add(T_RES_FIRE, randint1(base) + base);
        plr_tim_add(T_RES_ELEC, randint1(base) + base);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void resist_fire_spell(int cmd, var_ptr res)
{
    int base = spell_power(20);
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Resist Fire");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives extra resistance to fire for a bit.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(base, base));
        break;
    case SPELL_CAST:
        plr_tim_add(T_RES_FIRE, randint1(base) + base);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void resist_heat_cold_spell(int cmd, var_ptr res)
{
    int base = spell_power(20);
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Resist Heat and Cold");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives resistance to fire and cold.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(base, base));
        break;
    case SPELL_CAST:
        plr_tim_add(T_RES_COLD, randint1(base) + base);
        plr_tim_add(T_RES_FIRE, randint1(base) + base);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void resist_poison_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Resist Poison");
        break;
    case SPELL_DESC:
        var_set_string(res, "Provides temporary resistance to poison.");
        break;
    case SPELL_CAST:
        plr_tim_add(T_RES_POIS, randint1(20) + 20);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void restoration_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Restoration");
        break;
    case SPELL_DESC:
        var_set_string(res, "Restores all stats and experience.");
        break;
    case SPELL_CAST:
        do_res_stat(A_STR);
        do_res_stat(A_INT);
        do_res_stat(A_WIS);
        do_res_stat(A_DEX);
        do_res_stat(A_CON);
        do_res_stat(A_CHR);
        restore_level();
        plr_restore_life(1000);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void restore_life_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Restore Life");
        break;
    case SPELL_DESC:
        var_set_string(res, "Regain all lost experience.");
        break;
    case SPELL_CAST:
        restore_level();
        plr_restore_life(150);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_restore_life(void) { return cast_spell(restore_life_spell); }

void rocket_I_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Magic Rocket I");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a magic rocket.");
        break;
    default:
        rocket_spell(cmd, res, 120 + 2*plr->lev);
    }
}

void rocket_II_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Magic Rocket II");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a magic rocket of unsurpassable fire power.");
        break;
    default:
        rocket_spell(cmd, res, 500);
    }
}

void rush_attack_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Rush Attack");
        break;
    case SPELL_DESC:
        var_set_string(res, "Charge a nearby monster and attack with your weapons.");
        break;
    case SPELL_CAST:
        var_set_bool(res, rush_attack(5, NULL));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
