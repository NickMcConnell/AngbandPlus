#include "angband.h"

/************************************************************************
 * Helpers
 ************************************************************************/
void _dice_info_dam(var_ptr res, dice_t dice)
{
    int dam = dice_avg_roll(dice);
    if (dice.dd && dice.ds)
        var_printf(res, "dam ~%d", dam); /* approximate */
    else
        var_printf(res, "dam %d", dam); /* exact */
}
void _dice_info_power(var_ptr res, dice_t dice)
{
    int dam = dice_avg_roll(dice);
    if (dice.dd && dice.ds)
        var_printf(res, "power ~%d", dam); /* approximate */
    else
        var_printf(res, "power %d", dam); /* exact */
}
void _dice_info_range(var_ptr res, dice_t dice)
{
    int dam = dice_avg_roll(dice);
    if (dice.dd && dice.ds)
        var_printf(res, "range ~%d", dam); /* approximate */
    else
        var_printf(res, "range %d", dam); /* exact */
}
void _gf_spell(int cmd, var_ptr res, int gf, dice_t dice)
{
    switch (cmd)
    {
    case SPELL_INFO: {
        gf_info_ptr gfi = gf_lookup(gf);
        if (gfi->flags & GFF_DAMAGE)
            _dice_info_dam(res, dice);
        else if (gfi->flags & GFF_TELEPORT)
            _dice_info_range(res, dice);
        else
            _dice_info_power(res, dice);
        break; }
    default:
        default_spell(cmd, res);
    }
}
void ball_spell(int cmd, var_ptr res, int rad, int gf, int base)
{
    ball_spell_aux(cmd, res, rad, gf, spell_dam_dice(0, 0, base));
}
void ball_spell_aux(int cmd, var_ptr res, int rad, int gf, dice_t dice)
{
    switch (cmd)
    {
    case SPELL_CAST:
        var_set_bool(res, plr_cast_ball(rad, gf, dice));
        break;
    default:
        _gf_spell(cmd, res, gf, dice);
    }
}
void burst_spell(int cmd, var_ptr res, int rad, int gf, int base)
{
    burst_spell_aux(cmd, res, rad, gf, spell_dam_dice(0, 0, base));
}
void burst_spell_aux(int cmd, var_ptr res, int rad, int gf, dice_t dice)
{
    switch (cmd)
    {
    case SPELL_CAST:
        plr_burst(rad, gf, dice_roll(dice));
        var_set_bool(res, TRUE);
        break;
    default:
        _gf_spell(cmd, res, gf, dice);
    }
}
void bolt_spell(int cmd, var_ptr res, int gf, int dd, int ds)
{
    bolt_spell_aux(cmd, res, gf, spell_dam_dice(dd, ds, 0));
}
void bolt_spell_aux(int cmd, var_ptr res, int gf, dice_t dice)
{
    switch (cmd)
    {
    case SPELL_CAST:
        var_set_bool(res, plr_cast_bolt(gf, dice));
        break;
    default:
        _gf_spell(cmd, res, gf, dice);
    }
}
void beam_spell(int cmd, var_ptr res, int gf, int dd, int ds)
{
    beam_spell_aux(cmd, res, gf, spell_dam_dice(dd, ds, 0));
}
void beam_spell_aux(int cmd, var_ptr res, int gf, dice_t dice)
{
    switch (cmd)
    {
    case SPELL_CAST:
        var_set_bool(res, plr_cast_beam(gf, dice));
        break;
    default:
        _gf_spell(cmd, res, gf, dice);
    }
}
void bolt_or_beam_spell(int cmd, var_ptr res, int gf, int dd, int ds)
{
    bolt_or_beam_spell_aux(cmd, res, gf, spell_dam_dice(dd, ds, 0));
}
void bolt_or_beam_spell_aux(int cmd, var_ptr res, int gf, dice_t dice)
{
    switch (cmd)
    {
    case SPELL_CAST:
        var_set_bool(res, plr_cast_bolt_or_beam(gf, dice, beam_chance()));
        break;
    default:
        _gf_spell(cmd, res, gf, dice);
    }
}
void breath_spell(int cmd, var_ptr res, int rad, int gf, int base)
{
    breath_spell_aux(cmd, res, rad, gf, spell_dam_dice(0, 0, base));
}
void breath_spell_innate(int cmd, var_ptr res, int rad, int gf, int base)
{
    breath_spell_aux(cmd, res, rad, gf, innate_dice(0, 0, base));
}
void breath_spell_aux(int cmd, var_ptr res, int rad, int gf, dice_t dice)
{
    switch (cmd)
    {
    case SPELL_CAST:
        var_set_bool(res, plr_cast_breath(rad, gf, dice));
        break;
    default:
        _gf_spell(cmd, res, gf, dice);
    }
}
void curse_spell(int cmd, var_ptr res, int gf, int dd, int ds)
{
    dice_t dice = spell_dam_dice(dd, ds, 0);
    switch (cmd)
    {
    case SPELL_CAST:
        var_set_bool(res, plr_cast_ball(0, gf, dice));
        break;
    default:
        _gf_spell(cmd, res, gf, dice);
    }
}
void direct_spell(int cmd, var_ptr res, int gf, int power)
{
    dice_t dice = spell_dice(0, 0, power);
    switch (cmd)
    {
    case SPELL_CAST:
        var_set_bool(res, plr_cast_direct(gf, dice));
        break;
    default:
        _gf_spell(cmd, res, gf, dice);
    }
}
void los_spell(int cmd, var_ptr res, int gf, int power) /* e.g. GF_OLD_CONF */
{
    los_spell_aux(cmd, res, gf, spell_dice(0, 0, power));
}
void los_dam_spell(int cmd, var_ptr res, int gf, int power) /* e.g. GF_DISP_EVIL */
{
    los_spell_aux(cmd, res, gf, spell_dam_dice(0, 0, power));
}
void los_spell_aux(int cmd, var_ptr res, int gf, dice_t dice)
{
    switch (cmd)
    {
    case SPELL_CAST:
        plr_project_los(gf, dice_roll(dice));
        var_set_bool(res, TRUE);
        break;
    default:
        _gf_spell(cmd, res, gf, dice);
    }
}
void rocket_spell(int cmd, var_ptr res, int dam)
{
    rocket_spell_aux(cmd, res, spell_dam_dice(0, 0, dam));
}
void rocket_spell_aux(int cmd, var_ptr res, dice_t dice)
{
    switch (cmd)
    {
    case SPELL_INFO:
        _dice_info_dam(res, dice);
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_cast_rocket(2, dice));
        break;
    default:
        default_spell(cmd, res);
    }
}

/************************************************************************
 * Spells
 ************************************************************************/
void acid_ball_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Acid Ball");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generate an Acid Ball on chosen target.");
        break;
    default:
        ball_spell(cmd, res, 2, GF_ACID, 35 + 3*plr->lev/2);
    }
}

void acid_bolt_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Acid Bolt");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a bolt or beam of acid.");
        break;
    default:
        bolt_or_beam_spell(cmd, res, GF_ACID, 5 + plr->lev/4, 8);
    }
}

void alchemy_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Alchemy");
        break;
    case SPELL_DESC:
        var_set_string(res, "Turns valuable items into gold.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You gain the Midas touch.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You lose the Midas touch.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can turn ordinary items to gold.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (alchemy())
            var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_alchemy(void) { return cast_spell(alchemy_spell); }

void alter_reality_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Alter Reality");
        break;
    case SPELL_DESC:
        var_set_string(res, "Recreates current dungeon level.");
        break;
    case SPELL_CAST:
        alter_reality();
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void amnesia_spell(int cmd, var_ptr res)
{
    int power;
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Amnesia");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to make target monster forget something.");
        break;
    default:
        power = plr->lev;
        if (plr->lev > 40)
            power += (plr->lev - 40) * 2;
        direct_spell(cmd, res, GF_AMNESIA, power);
    }
}

void android_ray_gun_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Ray Gun");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires unresistable damage at chosen foe.");
        break;
    default:
        bolt_spell_aux(cmd, res, GF_MISSILE, innate_dice(0, 0, 5 + plr->lev/2));
    }
}

void android_blaster_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Blaster");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    default:
        bolt_spell_aux(cmd, res, GF_MISSILE, innate_dice(0, 0, 5 + plr->lev));
    }
}

void android_bazooka_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Bazooka");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires your bazooka at a nearby monster.");
        break;
    default:
        ball_spell_aux(cmd, res, 2, GF_MISSILE, innate_dice(0, 0, 25 + 2*plr->lev));
    }
}

void android_beam_cannon_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Beam Cannon");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    default:
        beam_spell_aux(cmd, res, GF_MISSILE, innate_dice(0, 0, 25 + 3*plr->lev));
    }
}

void android_rocket_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Rocket Launcher");
        break;
    case SPELL_DESC:
        var_set_string(res, "Launches a powerful rocket at your opponent.");
        break;
    default:
        rocket_spell_aux(cmd, res, innate_dice(0, 0, 7*plr->lev));
    }
}

void animate_dead_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Animate Dead");
        break;
    case SPELL_DESC:
        var_set_string(res, "Resurrects nearby corpse and skeletons. And makes these your pets.");
        break;
    case SPELL_CAST:
        plr_animate_dead();
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _awesome_blow(plr_attack_ptr ctx)
{
    int dir = point_step_dir(plr->pos, ctx->mon_pos);
    if (dir != 5)
    {
        int ct = 0;
        int max = 3;
        point_t last = ctx->mon_pos;

        if (plr->pclass == CLASS_RAGE_MAGE)
        {
            if (plr_tim_find(T_BERSERK)) max = 6;
        }
        else if (plr->pclass == CLASS_MAULER && ctx->obj)
        {
            int w = ctx->obj->weight;
            max = MIN(plr->lev/5, w/40);
        }

        for (ct = 0; ct < max; ct++)
        {
            point_t pos = point_step(last, dir);
            
            if (!dun_allow_mon_at(cave, pos))
            {
                dun_cell_ptr cell = dun_cell_at(cave, pos);
                int dam = 50;

                if ( dun_mon_at(cave, pos)
                  || cell_is_tree(cell)
                  || wall_is_rubble(cell)
                  || cell_is_chasm(cell) )
                {
                    dam = 25;
                }
                msg_format("%^s is wounded.", ctx->mon_name);
                dam = dam * (max - ct);
                ctx->dam_total += dam;
                if (mon_take_hit(ctx->mon, dam, &ctx->fear, NULL))
                    ctx->stop = STOP_MON_DEAD;
                break;
            }
            else
            {
                dun_move_mon(cave, ctx->mon, pos);
                last = pos;

                if (ctx->race->light || ctx->race->lantern)
                    plr->update |= PU_MON_LIGHT;

                Term_fresh();
                Term_xtra(TERM_XTRA_DELAY, delay_animation);
                ctx->stop = STOP_MON_MOVED;
            }
        }
    }
}
void awesome_blow_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Awesome Blow");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack a monster with a single melee blow. If blow hits, does normal melee damage and propels the monster backwards.");
        break;
    case SPELL_CAST: {
        plr_attack_t ctx = {0};
        ctx.flags = PAC_NO_INNATE | PAC_ONE_BLOW;
        ctx.hooks.after_hit_f = _awesome_blow;
        var_set_bool(res, plr_attack_special_aux(&ctx, 1));
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

void banish_evil_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Banish Evil");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to remove a single evil opponent.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You feel a holy wrath fill you.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You no longer feel a holy wrath.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can send evil creatures directly to Hell.");
        break;
    case SPELL_CAST:
    {
        mon_ptr mon = plr_target_mon();
        mon_race_ptr race;

        var_set_bool(res, FALSE);
        if (!mon)
        {
            msg_print("You sense no evil there!");
            break;
        }
        var_set_bool(res, TRUE);

        race = mon->race;
        if (mon_is_evil(mon) &&
            !(mon->mflag2 & MFLAG2_QUESTOR) &&
            !mon_race_is_unique(race) &&
            !quests_get_current() &&
            (race->alloc.lvl < randint1(plr->lev+50)) &&
            !(mon->mflag2 & MFLAG2_NOGENO))
        {
            /* Delete the monster, rather than killing it. */
            delete_monster(mon);
            msg_print("The evil creature vanishes in a puff of sulfurous smoke!");
        }
        else
        {
            msg_print("Your invocation is ineffectual!");
            if (one_in_(13)) mon->mflag2 |= MFLAG2_NOGENO;
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_banish_evil(void) { return cast_spell(banish_evil_spell); }

void battle_frenzy_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Battle Frenzy");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives another bonus to hit and HP, resistance to fear for a while. Hastes you. But decreases AC.");
        break;
    case SPELL_CAST:
    {
        int b_base = spell_power(25);
        int sp_base = spell_power(plr->lev / 2);
        int sp_sides = 20 + plr->lev / 2;

        plr_tim_add(T_BERSERK, randint1(b_base) + b_base);
        plr_tim_add(T_FAST, randint1(sp_sides) + sp_base);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void berserk_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Berserk");
        break;
    case SPELL_DESC:
        var_set_string(res, "Enter a berserk frenzy, gaining great combat bonuses, but losing the ability to think clearly.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You feel a controlled rage.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You no longer feel a controlled rage.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can drive yourself into a berserk frenzy.");
        break;
    case SPELL_CAST:
    {
        msg_print("Raaagh! You feel like hitting something.");
        plr_tim_add(T_BERSERK, 10 + randint1(plr->lev));
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_berserk(void) { return cast_spell(berserk_spell); }

void bless_spell(int cmd, var_ptr res)
{
    int base = spell_power(12);
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Bless");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives bonus to hit and AC for a few turns.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(base, base));
        break;
    case SPELL_CAST:
        plr_tim_add(T_BLESSED, randint1(base) + base);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void bless_weapon_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Bless Weapon");
        break;
    case SPELL_DESC:
        var_set_string(res, "Blesses your current weapon.");
        break;
    case SPELL_CAST:
        var_set_bool(res, bless_weapon());
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static dice_t _brain_smash_dice(void)
{
    int dd = 2 + plr->lev/5;
    return spell_dam_dice(dd, dd, MAX(plr->lev - 20, 0));
}
void brain_smash_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Brain Smash");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gaze intently at a single foe, causing damage, confusion and stunning");
        break;
    case SPELL_INFO:
        var_set_string(res, dice_info_dam(_brain_smash_dice()));
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_cast_ball(0, GF_BRAIN_SMASH, _brain_smash_dice()));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void breathe_disintegration_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Disintegration");
        break;
    case SPELL_DESC:
        var_set_string(res, "A disintegration breath. Not even the dungeon walls can withstand its power!");
        break;
    default:
        breath_spell_innate(cmd, res, 2 + plr->lev/40, GF_DISINTEGRATE, MIN(plr->chp/6, 150));
    }
}

void breathe_fire_I_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Fire");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes Fire at your opponent.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You gain the ability to breathe fire.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You lose the ability to breathe fire.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can breathe fire.");
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, (plr->lev+1)/2);
        break;
    default:
        breath_spell_innate(cmd, res, 1 + plr->lev/20, GF_FIRE, 2*plr->lev);
    }
}
bool cast_breathe_fire_I(void) { return cast_spell(breathe_fire_I_spell); }

void breathe_fire_II_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Fire");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathes Fire at your opponent.");
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, plr->lev);
        break;
    default:
        breath_spell_innate(cmd, res, 1 + plr->lev/20, GF_FIRE, plr->chp*2/5);
    }
}

void building_up_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Building Up");
        break;
    case SPELL_DESC:
        var_set_string(res, "Increases your physical prowess");
        break;
    case SPELL_CAST:
        plr_tim_add(T_GIANT_STRENGTH, 20 + randint1(20));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
