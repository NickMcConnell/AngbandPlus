#include "angband.h"

void heroism_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Heroism");
        break;
    case SPELL_DESC:
        var_set_string(res, "Temporarily grants increased combat prowess and great bravery.");
        break;
    case SPELL_CAST:
        plr_tim_add(T_HERO, randint1(25) + 25);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_heroism(void) { return cast_spell(heroism_spell); }

void hide_in_mud_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Hide in Mud");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gain the ability to pass into walls temporarily, as well as extra resistance to acid.");
        break;
    case SPELL_CAST:
        plr_tim_add(T_PASSWALL, randint1(plr->lev/2) + plr->lev/2);
        plr_tim_add(T_RES_ACID, plr->lev);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void ice_bolt_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Ice Bolt");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a bolt of ice.");
        break;
    default:
        bolt_spell(cmd, res, GF_ICE, 5 + plr->lev/4, 15);
    }
}

void identify_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Identify");
        break;
    case SPELL_DESC:
        var_set_string(res, "Identify a single object.");
        break;
    case SPELL_CAST:
        var_set_bool(res, ident_spell(NULL));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_identify(void) { return cast_spell(identify_spell); }

void identify_fully_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Identify True");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_CAST:
        var_set_bool(res, identify_fully(NULL));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_identify_fully(void) { return cast_spell(identify_fully_spell); }

void hand_of_doom_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Hand of Doom");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to mortally wound a target monster, draining a large proportion of their remaining health.");
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_cast_ball(0, GF_HAND_DOOM, spell_dice(0, 0, 3*plr->lev)));
        break;
    default:
        default_spell(cmd, res);
    }
}

void haste_self_spell(int cmd, var_ptr res)
{
    int base = spell_power(plr->lev);
    int sides = spell_power(20 + plr->lev);
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Haste Self");
        break;
    case SPELL_DESC:
        var_set_string(res, "Hastes you for a while.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(base, sides));
        break;
    case SPELL_CAST:
        plr_tim_add(T_FAST, base + randint1(sides));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void healing_I_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Healing");
        break;
    case SPELL_DESC:
        var_set_string(res, "Powerful healing magic:  heals hitpoints, cuts and stun.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("Heals %d", spell_power(300)));
        break;
    case SPELL_CAST:
        hp_player(spell_power(300));
        plr_tim_remove(T_STUN);
        plr_tim_remove(T_CUT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void healing_II_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Healing");
        break;
    case SPELL_DESC:
        var_set_string(res, "Powerful healing magic:  heals hitpoints, cuts and stun.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("Heals %d", spell_power(500)));
        break;
    case SPELL_CAST:
        hp_player(spell_power(500));
        plr_tim_remove(T_STUN);
        plr_tim_remove(T_CUT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void hellfire_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Hellfire");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a powerful ball of evil power directly from the bowels of hell. Good monsters are especially susceptible.");
        break;
    default:
        ball_spell(cmd, res, 3, GF_HELL_FIRE, 666);
        if (cmd == SPELL_CAST && !demon_is_(DEMON_BALROG) && var_get_bool(res))
            take_hit(DAMAGE_USELIFE, 20 + randint1(30), "the strain of casting Hellfire");
    }
}

void hell_lance_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Hell Lance");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a beam of pure hellfire.");
        break;
    default:
        beam_spell_aux(cmd, res, GF_HELL_FIRE, spell_dam_dice(0, 0, 3*plr->lev));
    }
}
bool cast_hell_lance(void) { return cast_spell(hell_lance_spell); }

void holy_lance_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Holy Lance");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a beam of pure holiness.");
        break;
    default:
        beam_spell_aux(cmd, res, GF_HOLY_FIRE, spell_dam_dice(0, 0, 3*plr->lev));
    }
}
bool cast_holy_lance(void) { return cast_spell(holy_lance_spell); }

void hp_to_sp_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Convert HP to SP");
        break;
    case SPELL_DESC:
        var_set_string(res, "Converts HP into SP");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You are subject to fits of painful clarity.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You are no longer subject to fits of painful clarity.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "Your blood sometimes rushes to your head.");
        break;
    case SPELL_PROCESS:
        if (!plr->anti_magic && one_in_(4000))
        {
            int wounds = plr->msp - plr->csp;

            if (wounds > 0 && plr->pclass != CLASS_RUNE_KNIGHT)
            {
                int healing = plr->chp;

                if (healing > wounds)
                    healing = wounds;

                plr->csp += healing;

                plr->redraw |= (PR_MANA);
                take_hit(DAMAGE_LOSELIFE, healing, "blood rushing to the head");
            }
        }
        break;

    case SPELL_CAST:
    {
        int gain_sp = take_hit(DAMAGE_USELIFE, plr->lev, "thoughtless convertion from HP to SP") / 5;
        if (gain_sp && plr->pclass != CLASS_RUNE_KNIGHT)
        {
            plr->csp += gain_sp;
            if (plr->csp > plr->msp)
            {
                plr->csp = plr->msp;
                plr->csp_frac = 0;
            }

            plr->redraw |= PR_MANA;
        }
        else
            msg_print("You failed to convert.");

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void hypnotic_gaze_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Hypnotic Gaze");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to charm a monster.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("Your eyes look mesmerizing...");
        break;
    case SPELL_LOSE_MUT:
        msg_print("Your eyes look uninteresting.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "Your gaze is hypnotic.");
        break;
    default:
       {int m = prace_is_(RACE_MON_VAMPIRE) ? 2 : 1;
        bolt_spell_aux(cmd, res, GF_CHARM, spell_dice(0, 0, m*plr->lev));}
    }
}
bool cast_hypnotic_gaze(void) { return cast_spell(hypnotic_gaze_spell); }

void imp_fire_spell(int cmd, var_ptr res)
{
    const int ball_lev = 30;
    switch (cmd)
    {
    case SPELL_NAME:
        if (plr->lev >= ball_lev)
            var_set_string(res, "Fire Ball");
        else
            var_set_string(res, "Fire Bolt");
        break;
    case SPELL_DESC:
        if (plr->lev >= ball_lev)
            var_set_string(res, "Generate a Fire Ball on chosen target.");
        else
            var_set_string(res, "Hurls a fiery missile at chosen target.");
        break;
    case SPELL_COST_EXTRA:
        if (plr->lev >= ball_lev)
            var_set_int(res, 7);
        else
            var_set_int(res, 0);
        break;
    default:
        if (plr->lev >= ball_lev)
            ball_spell_aux(cmd, res, 2, GF_FIRE, innate_dice(0, 0, 2*plr->lev));
        else
            bolt_spell_aux(cmd, res, GF_FIRE, innate_dice(0, 0, plr->lev));
    }
}

void invoke_logrus_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Invoke Logrus");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a huge ball of chaos.");
        break;
    default:
        ball_spell_aux(cmd, res, 4, GF_CHAOS, spell_dam_dice(10, 10, 4*plr->lev));
    }
}

void invulnerability_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Globe of Invulnerability");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generates barrier which completely protect you from almost all damages. Takes a few your turns when the barrier breaks or duration time is exceeded.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(7, 7));
        break;
    case SPELL_CAST:
        msg_print("You cast a Globe of Invulnerability.");
        plr_tim_add(T_INVULN, spell_power(500 + _1d(1000)));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void kiss_of_succubus_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Kiss of Succubus");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a ball of nexus.");
        break;
    default:
        ball_spell(cmd, res, 4, GF_NEXUS, 100 + 2*plr->lev);
    }
}

void laser_eye_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Laser Eye");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a laser beam.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("Your eyes burn for a moment.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("Your eyes burn for a moment, then feel soothed.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "Your eyes can fire laser beams.");
        break;
    default:
        beam_spell_aux(cmd, res, GF_LIGHT, innate_dice(0, 0, 2*plr->lev));
    }
}
bool cast_laser_eye(void) { return cast_spell(laser_eye_spell); }

void light_area_spell(int cmd, var_ptr res)
{
    int dice = 2;
    int sides = plr->lev / 2;
    int rad = spell_power(plr->lev / 10 + 1);

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Light Area");
        break;
    case SPELL_DESC:
        var_set_string(res, "Lights up nearby area and the inside of a room permanently.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You can light up rooms with your presence.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You can no longer light up rooms with your presence.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can emit bright light.");
        break;
    case SPELL_CAST:
        lite_area(spell_power(damroll(dice, sides)), rad);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_light_area(void) { return cast_spell(light_area_spell); }

void lightning_ball_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Lightning Ball");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a ball of electricity.");
        break;
    default:
        ball_spell(cmd, res, 2, GF_ELEC, 20 + 3*plr->lev/2);
    }
}

void lightning_bolt_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Lightning Bolt");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a bolt or beam of electricity.");
        break;
    default:
        bolt_or_beam_spell(cmd, res, GF_ELEC, 3 + plr->lev/4, 8);
    }
}

void living_trump_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Living Trump");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives mutation which makes you teleport randomly or makes you able to teleport at will.");
        break;
    case SPELL_CAST:
    {
        int mutation = one_in_(7) ? MUT_TELEPORT : MUT_TELEPORT_RND;

        if (mut_gain(mutation))
            msg_print("You have turned into a Living Trump.");
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

