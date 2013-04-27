// File: melee2.cpp
// Purpose: Monster spells and movement

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"



/*
 * Cast a bolt at the player
 * Stop if we hit a monster
 * Affect monsters and the player
 */
void CMonster::bolt(int typ, int dam_hp)
{
    int flg = PROJECT_STOP | PROJECT_KILL;

    /* Target the player with a bolt attack */
    project(this, 0, p_ptr->GetY(), p_ptr->GetX(), dam_hp, typ, flg);
}


/*
 * Cast a breath (or ball) attack at the player
 * Pass over any monsters that may be in the way
 * Affect grids, objects, monsters, and the player
 */
void CMonster::breath(int typ, int dam_hp)
{
    int rad;
    int flg = PROJECT_GRID | PROJECT_ITEM | PROJECT_KILL;
    CMonsterRace *r_ptr = get_r_ptr();

    /* Determine the radius of the blast */
    rad = (r_ptr->flags2 & RF2_POWERFUL) ? 3 : 2;

    /* Target the player with a ball attack */
    project(this, rad, p_ptr->GetY(), p_ptr->GetX(), dam_hp, typ, flg);
}



/*
 * Creatures can cast spells, shoot missiles, and breathe.
 *
 * Returns "TRUE" if a spell (or whatever) was (successfully) cast.
 *
 * XXX XXX XXX This function could use some work, but remember to
 * keep it as optimized as possible, while retaining generic code.
 *
 * Verify the various "blind-ness" checks in the code.
 *
 * XXX XXX XXX Note that several effects should really not be "seen"
 * if the player is blind.  See also "effects.c" for other "mistakes".
 *
 * Perhaps monsters should breathe at locations *near* the player,
 * since this would allow them to inflict "partial" damage.
 *
 * Perhaps smart monsters should decline to use "bolt" spells if
 * there is a monster in the way, unless they wish to kill it.
 *
 * Certain non-optimal things are done in the code below,
 * including explicit checks against the "direct" variable, which is
 * currently always true by the time it is checked, but which should
 * really be set according to an explicit "projectable()" test, and
 * the use of generic "x,y" locations instead of the player location,
 * with those values being initialized with the player location.
 *
 * It will not be possible to "correctly" handle the case in which a
 * monster attempts to attack a location which is thought to contain
 * the player, but which in fact is nowhere near the player, since this
 * might induce all sorts of messages about the attack itself, and about
 * the effects of the attack, which the player might or might not be in
 * a position to observe.  Thus, for simplicity, it is probably best to
 * only allow "faulty" attacks by a monster if one of the important grids
 * (probably the initial or final grid) is in fact in view of the player.
 * It may be necessary to actually prevent spell attacks except when the
 * monster actually has line of sight to the player.  Note that a monster
 * could be left in a bizarre situation after the player ducked behind a
 * pillar and then teleported away, for example.
 *
 * Note that certain spell attacks do not use the "project()" function
 * but "simulate" it via the "direct" variable, which is always at least
 * as restrictive as the "project()" function.  This is necessary to
 * prevent "blindness" attacks and such from bending around walls, etc,
 * and to allow the use of the "track_target" option in the future.
 *
 * Note that this function attempts to optimize the use of spells for the
 * cases in which the monster has no spells, or has spells but cannot use
 * them, or has spells but they will have no "useful" effect.  Note that
 * this function has been an efficiency bottleneck in the past.
 */
bool CMonster::make_attack_spell(CLiving *target)
{
    int k, chance, thrown_spell, rlev;
    byte spell[96], num = 0;
    u32b f4, f5, f6;
    CMonsterRace *r_ptr = get_r_ptr();
    char m_name[80], m_poss[80], ddesc[80];

    /* Target location */
    int x = target->GetX(), y = target->GetY();

    /* Summon count */
    int count = 0;


    /* Extract the blind-ness */
    bool blind = (p_ptr->GetBlind() ? TRUE : FALSE);

    /* Extract the "see-able-ness" */
    bool seen = (!blind && is_visible());


    /* Assume "normal" target */
    bool normal = TRUE;

    /* Assume "projectable" */
    bool direct = TRUE;


    /* Hack -- Extract the spell probability */
    chance = (r_ptr->freq_inate + r_ptr->freq_spell) / 2;

    /* Not allowed to cast spells */
    if (!chance) return (FALSE);

    /* Cannot cast spells when confused */
    if (get_confused()) return FALSE;

    /* Only do spells occasionally */
    if (!percent(chance)) return FALSE;


    /* XXX XXX XXX Handle "track_target" option (?) */


    /* Hack -- require projectable player */
    if (normal) {
        /* Check range */
        if (get_cdis() > MAX_RANGE) return (FALSE);

        /* Check path */
        if (!projectable(GetY(), GetX(), target->GetY(), target->GetX())) {
            return (FALSE);
        }
    }


    /* Extract the monster level */
    rlev = ((r_ptr->level >= 1) ? r_ptr->level : 1);


    /* Extract the racial spell flags */
    f4 = r_ptr->flags4;
    f5 = r_ptr->flags5;
    f6 = r_ptr->flags6;


    /* Hack -- allow "desperate" spells */
    if ((r_ptr->flags2 & RF2_SMART) &&
        (GetCHP() < GetMHP() / 10) &&
        percent(50))
    {
        /* Require intelligent spells */
        f4 &= RF4_INT_MASK;
        f5 &= RF5_INT_MASK;
        f6 &= RF6_INT_MASK;

        /* No spells left */
        if (!f4 && !f5 && !f6) return (FALSE);
    }


    /* Extract the "inate" spells */
    for (k = 0; k < 32; k++) {
        if (f4 & (1L << k)) spell[num++] = k + 32 * 3;
    }

    /* Extract the "normal" spells */
    for (k = 0; k < 32; k++) {
        if (f5 & (1L << k)) spell[num++] = k + 32 * 4;
    }

    /* Extract the "bizarre" spells */
    for (k = 0; k < 32; k++) {
        if (f6 & (1L << k)) spell[num++] = k + 32 * 5;
    }

    /* No spells left */
    if (!num) return (FALSE);


    /* Stop if player is dead or gone */
    if (!alive || death || new_level_flag) return FALSE;


    /* XXX The spell can fail if monster is stunned */
    if (get_stun() && percent(25)) {
        // XXX this message isn't great for unchanting monsters
        msg_format("%^s begins chanting but the spell fizzles!", m_name);
        return TRUE;
    }


    /* Get the monster name (or "it") */
    get_desc(m_name, 0x00);

    /* Get the monster possessive ("his"/"her"/"its") */
    get_desc(m_poss, 0x22);

    /* Hack -- Get the "died from" name */
    get_desc(ddesc, 0x88);


    /* Choose a spell to cast */
    thrown_spell = spell[rand_int(num)];


    /* Cast the spell. */
    switch (thrown_spell) {
        case 96+0:    /* RF4_SHRIEK */
            if (!direct) break;
            msg_format("%^s makes a high pitched shriek.", m_name);
            aggravate_monsters(this);
            break;

        case 96+1:    /* RF4_XXX2X4 */
            break;

        case 96+2:    /* RF4_XXX3X4 */
            break;

        case 96+3:    /* RF4_XXX4X4 */
            break;

        case 96+4:    /* RF4_ARROW_1 */
            if (blind) msg_format("%^s makes a strange noise.", m_name);
            else msg_format("%^s fires an arrow.", m_name);
            bolt(GF_ARROW, damroll(1, 6));
            break;

        case 96+5:    /* RF4_ARROW_2 */
            if (blind) msg_format("%^s makes a strange noise.", m_name);
            else msg_format("%^s fires an arrow!", m_name);
            bolt(GF_ARROW, damroll(3, 6));
            break;

        case 96+6:    /* RF4_ARROW_3 */
            if (blind) msg_format("%^s makes a strange noise.", m_name);
            else msg_format("%^s fires a missile.", m_name);
            bolt(GF_ARROW, damroll(5, 6));
            break;

        case 96+7:    /* RF4_ARROW_4 */
            if (blind) msg_format("%^s makes a strange noise.", m_name);
            else msg_format("%^s fires a missile!", m_name);
            bolt(GF_ARROW, damroll(7, 6));
            break;

        case 96+8:    /* RF4_BR_ACID */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes acid.", m_name);
            breath(GF_ACID, ((GetCHP() / 3) > 1600 ? 1600 : (GetCHP() / 3)));
            break;

        case 96+9:    /* RF4_BR_ELEC */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes lightning.", m_name);
            breath(GF_ELEC, ((GetCHP() / 3) > 1600 ? 1600 : (GetCHP() / 3)));
            break;

        case 96+10:    /* RF4_BR_FIRE */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes fire.", m_name);
            breath(GF_FIRE, ((GetCHP() / 3) > 1600 ? 1600 : (GetCHP() / 3)));
            break;

        case 96+11:    /* RF4_BR_COLD */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes frost.", m_name);
            breath(GF_COLD, ((GetCHP() / 3) > 1600 ? 1600 : (GetCHP() / 3)));
            break;

        case 96+12:    /* RF4_BR_POIS */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes gas.", m_name);
            breath(GF_POIS, ((GetCHP() / 3) > 800 ? 800 : (GetCHP() / 3)));
            break;

        case 96+13:    /* RF4_BR_NETH */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes nether.", m_name);
            breath(GF_NETHER, ((GetCHP() / 6) > 550 ? 550 : (GetCHP() / 6)));
            break;

        case 96+14:    /* RF4_BR_LITE */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes light.", m_name);
            breath(GF_LITE, ((GetCHP() / 6) > 400 ? 400 : (GetCHP() / 6)));
            break;

        case 96+15:    /* RF4_BR_DARK */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes darkness.", m_name);
            breath(GF_DARK, ((GetCHP() / 6) > 400 ? 400 : (GetCHP() / 6)));
            break;

        case 96+16:    /* RF4_BR_CONF */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes confusion.", m_name);
            breath(GF_CONFUSION, ((GetCHP() / 6) > 400 ? 400 : (GetCHP() / 6)));
            break;

        case 96+17:    /* RF4_BR_SOUN */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes sound.", m_name);
            breath(GF_SOUND, ((GetCHP() / 6) > 400 ? 400 : (GetCHP() / 6)));
            break;

        case 96+18:    /* RF4_BR_CHAO */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes chaos.", m_name);
            breath(GF_CHAOS, ((GetCHP() / 6) > 600 ? 600 : (GetCHP() / 6)));
            break;

        case 96+19:    /* RF4_BR_DISE */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes disenchantment.", m_name);
            breath(GF_DISENCHANT, ((GetCHP() / 6) > 500 ? 500 : (GetCHP() / 6)));
            break;

        case 96+20:    /* RF4_BR_NEXU */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes nexus.", m_name);
            breath(GF_NEXUS, ((GetCHP() / 3) > 250 ? 250 : (GetCHP() / 3)));
            break;

        case 96+21:    /* RF4_BR_TIME */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes time.", m_name);
            breath(GF_TIME, ((GetCHP() / 3) > 150 ? 150 : (GetCHP() / 3)));
            break;

        case 96+22:    /* RF4_BR_INER */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes inertia.", m_name);
            breath(GF_INERTIA, ((GetCHP() / 6) > 200 ? 200 : (GetCHP() / 6)));
            break;

        case 96+23:    /* RF4_BR_GRAV */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes gravity.", m_name);
            breath(GF_GRAVITY, ((GetCHP() / 3) > 200 ? 200 : (GetCHP() / 3)));
            break;

        case 96+24:    /* RF4_BR_SHAR */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes shards.", m_name);
            breath(GF_SHARDS, ((GetCHP() / 6) > 400 ? 400 : (GetCHP() / 6)));
            break;

        case 96+25:    /* RF4_BR_PLAS */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes plasma.", m_name);
            breath(GF_PLASMA, ((GetCHP() / 6) > 150 ? 150 : (GetCHP() / 6)));
            break;

        case 96+26:    /* RF4_BR_WALL */
            if (blind) msg_format("%^s breathes.", m_name);
            else msg_format("%^s breathes force.", m_name);
            breath(GF_FORCE, ((GetCHP() / 6) > 200 ? 200 : (GetCHP() / 6)));
            break;

        case 96+27:    /* RF4_BR_MANA */
            /* XXX XXX XXX */
            break;

        case 96+28:    /* RF4_XXX5X4 */
            break;

        case 96+29:    /* RF4_XXX6X4 */
            break;

        case 96+30:    /* RF4_XXX7X4 */
            break;

        case 96+31:    /* RF4_XXX8X4 */
            break;



        case 128+0:    /* RF5_BA_ACID */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s casts an acid ball.", m_name);
            breath(GF_ACID, randint(rlev * 3) + 15);
            break;

        case 128+1:    /* RF5_BA_ELEC */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s casts a lightning ball.", m_name);
            breath(GF_ELEC, randint(rlev * 3 / 2) + 8);
            break;

        case 128+2:    /* RF5_BA_FIRE */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s casts a fire ball.", m_name);
            breath(GF_FIRE, randint(rlev * 7 / 2) + 10);
            break;

        case 128+3:    /* RF5_BA_COLD */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s casts a frost ball.", m_name);
            breath(GF_COLD, randint(rlev * 3 / 2) + 10);
            break;

        case 128+4:    /* RF5_BA_POIS */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s casts a stinking cloud.", m_name);
            breath(GF_POIS, damroll(12, 2));
            break;

        case 128+5:    /* RF5_BA_NETH */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s casts a nether ball.", m_name);
            breath(GF_NETHER, (50 + damroll(10, 10) + rlev));
            break;

        case 128+6:    /* RF5_BA_WATE */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s gestures fluidly.", m_name);
            msg_print("You are engulfed in a whirlpool.");
            breath(GF_WATER, randint(rlev * 5 / 2) + 50);
            break;

        case 128+7:    /* RF5_BA_MANA */
            if (blind) msg_format("%^s mumbles powerfully.", m_name);
            else msg_format("%^s invokes a mana storm.", m_name);
            breath(GF_MANA, (rlev * 5) + damroll(10, 10));
            break;

        case 128+8:    /* RF5_BA_DARK */
            if (blind) msg_format("%^s mumbles powerfully.", m_name);
            else msg_format("%^s invokes a darkness storm.", m_name);
            breath(GF_DARK, (rlev * 5) + damroll(10, 10));
            break;

        case 128+9:    /* RF5_DRAIN_MANA */
            if (!direct) break;
            if (p_ptr->GetCSP()) {
                int r1;

                /* Basic message */
                msg_format("%^s draws psychic energy from you!", m_name);

                /* Attack power */
                r1 = (randint(rlev) / 2) + 1;

                /* Full drain */
                if (r1 >= p_ptr->GetCSP()) {
                    r1 = p_ptr->GetCSP();
                    p_ptr->SetCSP(0);
                    p_ptr->SetCSPFrac(0);
                }

                /* Partial drain */
                else {
                    p_ptr->SetCSP(p_ptr->GetCSP() - r1);
                }

                /* Heal the monster */
                if (GetCHP() < GetMHP()) {
                    /* Heal */
                    SetCHP(GetCHP() + 6 * r1);
                    correct_hp_overflows();

                    /* Special message */
                    if (seen) {
                        msg_format("%^s appears healthier.", m_name);
                    }
                }
            }
            break;

        case 128+10:    /* RF5_MIND_BLAST */
            if (!direct) break;
            if (!seen) {
                msg_print("You feel something focusing on your mind.");
            }
            else {
                msg_format("%^s gazes deep into your eyes.", m_name);
            }

            if (percent(p_ptr->GetSkill(SKILL_SAV))) {
                msg_print("You resist the effects!");
            }
            else {
                msg_print("Your mind is blasted by psionic energy.");
                if (!p_ptr->get_resists(RESIST_CONF)) {
                    p_ptr->mod_confused(p_ptr->GetConfused() + rand_int(4) + 4);
                }
                p_ptr->take_hit(damroll(8, 8), ddesc);
            }
            break;

        case 128+11:    /* RF5_BRAIN_SMASH */
            if (!direct) break;
            if (!seen) {
                msg_print("You feel something focusing on your mind.");
            }
            else {
                msg_format("%^s looks deep into your eyes.", m_name);
            }
            if (percent(p_ptr->GetSkill(SKILL_SAV))) {
                msg_print("You resist the effects!");
            }
            else {
                msg_print("Your mind is blasted by psionic energy.");
                p_ptr->take_hit(damroll(12, 15), ddesc);
                if (!p_ptr->get_resists(RESIST_BLIND)) {
                    p_ptr->mod_blind(p_ptr->GetBlind() + 8 + rand_int(8));
                }
                if (!p_ptr->get_resists(RESIST_CONF)) {
                    p_ptr->mod_confused(p_ptr->GetConfused() + rand_int(4) + 4);
                }
                if (!p_ptr->get_free_act()) {
                    p_ptr->mod_paralyzed(p_ptr->GetParalyzed() + rand_int(4) + 4);
                }
                p_ptr->mod_slow(p_ptr->GetSlow() + rand_int(4) + 4);
            }
            break;

        case 128+12:    /* RF5_CAUSE_1 */
            if (!direct) break;
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s points at you and curses.", m_name);
            if (percent(p_ptr->GetSkill(SKILL_SAV))) {
                msg_print("You resist the effects!");
            }
            else {
                p_ptr->take_hit(damroll(3, 8), ddesc);
            }
            break;

        case 128+13:    /* RF5_CAUSE_2 */
            if (!direct) break;
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s points at you and curses horribly.", m_name);
            if (percent(p_ptr->GetSkill(SKILL_SAV))) {
                msg_print("You resist the effects!");
            }
            else {
                p_ptr->take_hit(damroll(8, 8), ddesc);
            }
            break;

        case 128+14:    /* RF5_CAUSE_3 */
            if (!direct) break;
            if (blind) msg_format("%^s mumbles loudly.", m_name);
            else msg_format("%^s points at you, incanting terribly!", m_name);
            if (percent(p_ptr->GetSkill(SKILL_SAV))) {
                msg_print("You resist the effects!");
            }
            else {
                p_ptr->take_hit(damroll(10, 15), ddesc);
            }
            break;

        case 128+15:    /* RF5_CAUSE_4 */
            if (!direct) break;
            if (blind) msg_format("%^s screams the word 'DIE!'", m_name);
            else msg_format("%^s points at you, screaming the word DIE!", m_name);
            if (percent(p_ptr->GetSkill(SKILL_SAV))) {
                msg_print("You resist the effects!");
            }
            else {
                p_ptr->take_hit(damroll(15, 15), ddesc);
                p_ptr->mod_cut(p_ptr->GetCut() + damroll(10, 10));
            }
            break;

        case 128+16:    /* RF5_BO_ACID */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s casts a acid bolt.", m_name);
            bolt(GF_ACID, damroll(7, 8) + (rlev / 3));
            break;

        case 128+17:    /* RF5_BO_ELEC */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s casts a lightning bolt.", m_name);
            bolt(GF_ELEC, damroll(4, 8) + (rlev / 3));
            break;

        case 128+18:    /* RF5_BO_FIRE */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s casts a fire bolt.", m_name);
            bolt(GF_FIRE, damroll(9, 8) + (rlev / 3));
            break;

        case 128+19:    /* RF5_BO_COLD */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s casts a frost bolt.", m_name);
            bolt(GF_COLD, damroll(6, 8) + (rlev / 3));
            break;

        case 128+20:    /* RF5_BO_POIS */
            /* XXX XXX XXX */
            break;

        case 128+21:    /* RF5_BO_NETH */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s casts a nether bolt.", m_name);
            bolt(GF_NETHER, 30 + damroll(5, 5) + (rlev * 3) / 2);
            break;

        case 128+22:    /* RF5_BO_WATE */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s casts a water bolt.", m_name);
            bolt(GF_WATER, damroll(10, 10) + (rlev));
            break;

        case 128+23:    /* RF5_BO_MANA */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s casts a mana bolt.", m_name);
            bolt(GF_MANA, randint(rlev * 7 / 2) + 50);
            break;

        case 128+24:    /* RF5_BO_PLAS */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s casts a plasma bolt.", m_name);
            bolt(GF_PLASMA, 10 + damroll(8, 7) + (rlev));
            break;

        case 128+25:    /* RF5_BO_ICEE */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s casts an ice bolt.", m_name);
            bolt(GF_ICE, damroll(6, 6) + (rlev));
            break;

        case 128+26:    /* RF5_MISSILE */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s casts a magic missile.", m_name);
            bolt(GF_MISSILE, damroll(2, 6) + (rlev / 3));
            break;

        case 128+27:    /* RF5_SCARE */
            if (!direct) break;
            if (blind) msg_format("%^s mumbles, and you hear scary noises.", m_name);
            else msg_format("%^s casts a fearful illusion.", m_name);
            if (p_ptr->get_resists(RESIST_FEAR)) {
                msg_print("You are not frightened.");
            }
            else if (percent(p_ptr->GetSkill(SKILL_SAV))) {
                msg_print("You are not frightened.");
            }
            else {
                p_ptr->mod_afraid(p_ptr->GetAfraid() + rand_int(4) + 4);
            }
            break;

        case 128+28:    /* RF5_BLIND */
            if (!direct) break;
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s casts a spell, burning your eyes!", m_name);
            if (p_ptr->get_resists(RESIST_BLIND)) {
                msg_print("You are unaffected!");
            }
            else if (percent(p_ptr->GetSkill(SKILL_SAV))) {
                msg_print("You resist the effects!");
            }
            else {
                p_ptr->mod_blind(p_ptr->GetBlind() + 12 + rand_int(4));
            }
            break;

        case 128+29:    /* RF5_CONF */
            if (!direct) break;
            if (blind) msg_format("%^s mumbles, and you hear puzzling noises.", m_name);
            else msg_format("%^s creates a mesmerising illusion.", m_name);
            if (p_ptr->get_resists(RESIST_CONF)) {
                msg_print("You disbelieve the feeble spell.");
            }
            else if (percent(p_ptr->GetSkill(SKILL_SAV))) {
                msg_print("You disbelieve the feeble spell.");
            }
            else {
                p_ptr->mod_confused(p_ptr->GetConfused() + rand_int(4) + 4);
            }
            break;

        case 128+30:    /* RF5_SLOW */
            if (!direct) break;
            msg_format("%^s drains power from your muscles!", m_name);
            if (p_ptr->get_free_act()) {
                msg_print("You are unaffected!");
            }
            else if (percent(p_ptr->GetSkill(SKILL_SAV))) {
                msg_print("You resist the effects!");
            }
            else {
                p_ptr->mod_slow(p_ptr->GetSlow() + rand_int(4) + 4);
            }
            break;

        case 128+31:    /* RF5_HOLD */
            if (!direct) break;
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s stares deep into your eyes!", m_name);
            if (p_ptr->get_free_act()) {
                msg_print("You are unaffected!");
            }
            else if (percent(p_ptr->GetSkill(SKILL_SAV))) {
                msg_print("You resist the effects!");
            }
            else {
                p_ptr->mod_paralyzed(p_ptr->GetParalyzed() + rand_int(4) + 4);
            }
            break;



        case 160+0:    /* RF6_HASTE */
            if (blind) {
                msg_format("%^s mumbles.", m_name);
            }
            else {
                msg_format("%^s concentrates on %s body.", m_name, m_poss);
            }

            /* Already hasted */
            if (get_fast()) {
                set_fast(get_fast() + 5);
            }

            /* Not already hasted */
            else {
                msg_format("%^s starts moving faster.", m_name);
                set_fast(10 + randint(10));
            }

            break;

        case 160+1:    /* RF6_XXX1X6 */
            break;

        case 160+2:    /* RF6_HEAL */
            /* Message */
            if (blind) {
                msg_format("%^s mumbles.", m_name);
            }
            else {
                msg_format("%^s concentrates on %s wounds.", m_name, m_poss);
            }

            /* Heal some */
            SetCHP(GetCHP() + rlev * 6);

            /* Fully healed */
            correct_hp_overflows();

            /* Cancel fear */
            if (get_afraid()) {
                /* Cancel fear */
                set_afraid(0);

                /* Message */
                msg_format("%^s recovers %s courage.", m_name, m_poss);
            }

            break;

        case 160+3:    /* RF6_XXX2X6 */
            break;

        case 160+4:    /* RF6_BLINK */
            msg_format("%^s blinks away.", m_name);
            teleport_away(this, 10);
            break;

        case 160+5:    /* RF6_TPORT */
            msg_format("%^s teleports away.", m_name);
            teleport_away(this, MAX_SIGHT * 2 + 5);
            break;

        case 160+6:    /* RF6_XXX3X6 */
            break;

        case 160+7:    /* RF6_XXX4X6 */
            break;

        case 160+8:    /* RF6_TELE_TO */
            if (!direct) break;
            msg_format("%^s commands you to return.", m_name);
            teleport_player_to(GetY(), GetX());
            break;

        case 160+9:    /* RF6_TELE_AWAY */
            if (!direct) break;
            msg_format("%^s teleports you away.", m_name);
            teleport_player(100);
            break;

        case 160+10:    /* RF6_TELE_LEVEL */
            if (!direct) break;
            if (blind) msg_format("%^s mumbles strangely.", m_name);
            else msg_format("%^s gestures at your feet.", m_name);
            if (p_ptr->get_resists(RESIST_NEXUS)) {
                msg_print("You are unaffected!");
            }
            else if (percent(p_ptr->GetSkill(SKILL_SAV))) {
                msg_print("You resist the effects!");
            }
            else {
                teleport_player_level();
            }
            break;

        case 160+11:    /* RF6_XXX5 */
            break;

        case 160+12:    /* RF6_DARKNESS */
            if (!direct) break;
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s gestures in shadow.", m_name);
            unlite_area(0, 3);
            break;

        case 160+13:    /* RF6_TRAPS */
            if (!direct) break;
            if (blind) msg_format("%^s mumbles, and then cackles evilly.", m_name);
            else msg_format("%^s casts a spell and cackles evilly.", m_name);
            trap_creation();
            break;

        case 160+14:    /* RF6_FORGET */
            if (!direct) break;
            msg_format("%^s tries to blank your mind.", m_name);

            if (percent(p_ptr->GetSkill(SKILL_SAV))) {
                msg_print("You resist the effects!");
            }
            else if (lose_all_info()) {
                msg_print("Your memories fade away.");
            }
            break;

        case 160+15:    /* RF6_XXX6X6 */
            break;

        case 160+16:    /* RF6_XXX7X6 */
            break;

        case 160+17:    /* RF6_XXX8X6 */
            break;

        case 160+18:    /* RF6_S_MONSTER */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s magically summons help!", m_name);
            for (k = 0; k < 1; k++) {
                count += summon_specific(y, x, rlev, 0);
            }
            if (blind && count) msg_print("You hear something appear nearby.");
            break;

        case 160+19:    /* RF6_S_MONSTERS */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s magically summons monsters!", m_name);
            for (k = 0; k < 8; k++) {
                count += summon_specific(y, x, rlev, 0);
            }
            if (blind && count) msg_print("You hear many things appear nearby.");
            break;

        case 160+20:    /* RF6_S_ANT */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s magically summons ants.", m_name);
            for (k = 0; k < 6; k++) {
                count += summon_specific(y, x, rlev, SUMMON_ANT);
            }
            if (blind && count) msg_print("You hear many things appear nearby.");
            break;

        case 160+21:    /* RF6_S_SPIDER */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s magically summons spiders.", m_name);
            for (k = 0; k < 6; k++) {
                count += summon_specific(y, x, rlev, SUMMON_SPIDER);
            }
            if (blind && count) msg_print("You hear many things appear nearby.");
            break;

        case 160+22:    /* RF6_S_HOUND */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s magically summons hounds.", m_name);
            for (k = 0; k < 6; k++) {
                count += summon_specific(y, x, rlev, SUMMON_HOUND);
            }
            if (blind && count) msg_print("You hear many things appear nearby.");
            break;

        case 160+23:    /* RF6_S_HYDRA */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s magically summons hydras.", m_name);
            for (k = 0; k < 6; k++) {
                count += summon_specific(y, x, rlev, SUMMON_HYDRA);
            }
            if (blind && count) msg_print("You hear many things appear nearby.");
            break;

        case 160+24:    /* RF6_S_ANGEL */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s magically summons an angel!", m_name);
            for (k = 0; k < 1; k++) {
                count += summon_specific(y, x, rlev, SUMMON_ANGEL);
            }
            if (blind && count) msg_print("You hear something appear nearby.");
            break;

        case 160+25:    /* RF6_S_DEMON */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s magically summons a hellish adversary!", m_name);
            for (k = 0; k < 1; k++) {
                count += summon_specific(y, x, rlev, SUMMON_DEMON);
            }
            if (blind && count) msg_print("You hear something appear nearby.");
            break;

        case 160+26:    /* RF6_S_UNDEAD */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s magically summons an undead adversary!", m_name);
            for (k = 0; k < 1; k++) {
                count += summon_specific(y, x, rlev, SUMMON_UNDEAD);
            }
            if (blind && count) msg_print("You hear something appear nearby.");
            break;

        case 160+27:    /* RF6_S_DRAGON */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s magically summons a dragon!", m_name);
            for (k = 0; k < 1; k++) {
                count += summon_specific(y, x, rlev, SUMMON_DRAGON);
            }
            if (blind && count) msg_print("You hear something appear nearby.");
            break;

        case 160+28:    /* RF6_S_HI_UNDEAD */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s magically summons greater undead!", m_name);
            for (k = 0; k < 8; k++) {
                count += summon_specific(y, x, rlev, SUMMON_HI_UNDEAD);
            }
            if (blind && count) msg_print("You hear many creepy things appear nearby.");
            break;

        case 160+29:    /* RF6_S_HI_DRAGON */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s magically summons ancient dragons!", m_name);
            for (k = 0; k < 8; k++) {
                count += summon_specific(y, x, rlev, SUMMON_HI_DRAGON);
            }
            if (blind && count) msg_print("You hear many powerful things appear nearby.");
            break;

        case 160+30:    /* RF6_S_WRAITH */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s magically summons mighty undead opponents!", m_name);
            for (k = 0; k < 8; k++) {
                count += summon_specific(y, x, rlev, SUMMON_WRAITH);
            }
            for (k = 0; k < 8; k++) {
                count += summon_specific(y, x, rlev, SUMMON_HI_UNDEAD);
            }
            if (blind && count) msg_print("You hear many creepy things appear nearby.");
            break;

        case 160+31:    /* RF6_S_UNIQUE */
            if (blind) msg_format("%^s mumbles.", m_name);
            else msg_format("%^s magically summons special opponents!", m_name);
            for (k = 0; k < 8; k++) {
                count += summon_specific(y, x, rlev, SUMMON_UNIQUE);
            }
            for (k = 0; k < 8; k++) {
                count += summon_specific(y, x, rlev, SUMMON_HI_UNDEAD);
            }
            if (blind && count) msg_print("You hear many powerful things appear nearby.");
            break;


        default:
            quit(format("%^s cast a buggy spell.", m_name));
    }


    /* Remember what the monster did to us */
    if (seen)
    {
        /* Inate spell */
        if (thrown_spell < 32*4) {
            r_ptr->r_flags4 |= (1L << (thrown_spell - 32*3));
            if (r_ptr->r_cast_inate < MAX_UCHAR) r_ptr->r_cast_inate++;
        }

        /* Bolt or Ball */
        else if (thrown_spell < 32*5) {
            r_ptr->r_flags5 |= (1L << (thrown_spell - 32*4));
            if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
        }

        /* Special spell */
        else if (thrown_spell < 32*6) {
            r_ptr->r_flags6 |= (1L << (thrown_spell - 32*5));
            if (r_ptr->r_cast_spell < MAX_UCHAR) r_ptr->r_cast_spell++;
        }
    }


    /* Always take note of monsters that kill you */
    if (death && (r_ptr->r_deaths < MAX_SHORT)) r_ptr->r_deaths++;


    /* A spell was cast */
    return TRUE;
}


/*
 * Returns whether a given monster will try to run from the player.
 *
 * Monsters will attempt to avoid very powerful players.  See below.
 *
 * Because this function is called so often, little details are important
 * for efficiency.  Like not using "mod" or "div" when possible.  And
 * attempting to check the conditions in an optimal order.  Note that
 * "(x << 2) == (x * 4)" if "x" has enough bits to hold the result.
 *
 * Note that this function is responsible for about one to five percent
 * of the processor use in normal conditions...
 */
int CMonster::mon_will_run(void)
{
    CMonsterRace *r_ptr = get_r_ptr();

    u16b p_lev, m_lev;
    u16b p_chp, p_mhp;
    u16b m_chp, m_mhp;
    u32b p_val, m_val;

    /* Keep monsters from running too far away */
    if (get_cdis() > MAX_SIGHT + 5) return FALSE;

    /* All "afraid" monsters will run away */
    if (get_afraid()) return TRUE;

    /* Nearby monsters will not become terrified */
    if (get_cdis() <= 5) return FALSE;

    /* Examine player power (level) */
    p_lev = p_ptr->GetLev();

    /* Examine monster power (level plus morale) */
    m_lev = r_ptr->level + rand_int(8) + 25;

    /* Optimize extreme cases below */
    if (m_lev > p_lev + 4) return FALSE;
    if (m_lev + 4 <= p_lev) return (TRUE);

    /* Examine player health */
    p_chp = p_ptr->GetCHP();
    p_mhp = p_ptr->GetMHP();

    /* Examine monster health */
    m_chp = GetCHP();
    m_mhp = GetMHP();

    /* Prepare to optimize the calculation */
    p_val = (p_lev * p_mhp) + (p_chp << 2);     /* div p_mhp */
    m_val = (m_lev * m_mhp) + (m_chp << 2);     /* div m_mhp */

    /* Strong players scare strong monsters */
    if (p_val * m_mhp > m_val * p_mhp) return TRUE;

    /* Assume no terror */
    return FALSE;
}




/*
 * Choose "logical" directions for monster movement
 */
void CMonster::get_moves(int *mm)
{
    int y, ay, x, ax;

    int move_val = 0;

    int y2 = p_ptr->GetY();
    int x2 = p_ptr->GetX();


    /* Extract the "pseudo-direction" */
    x = GetX() - x2;
    y = GetY() - y2;


    /* Apply fear if possible and necessary */
    if (mon_will_run()) {
        /* XXX XXX Not very "smart" */
        x = -x;
        y = -y;
    }


    /* Extract the "absolute distances" */
    ax = ABS(x);
    ay = ABS(y);

    // Do something weird
    if (y < 0) move_val += 8;
    if (x > 0) move_val += 4;

    // Prevent the diamond maneuvre
    if (ay > (ax << 1)) {
        move_val++;
        move_val++;
    }
    else if (ax > (ay << 1)) {
        move_val++;
    }

    /* Extract some directions */
    switch (move_val) {
        case 0:
            mm[0] = 9;
            if (ay > ax) {
                mm[1] = 8;
                mm[2] = 6;
                mm[3] = 7;
                mm[4] = 3;
            }
            else {
                mm[1] = 6;
                mm[2] = 8;
                mm[3] = 3;
                mm[4] = 7;
            }
            break;
        case 1:
        case 9:
            mm[0] = 6;
            if (y < 0) {
                mm[1] = 3;
                mm[2] = 9;
                mm[3] = 2;
                mm[4] = 8;
            }
            else {
                mm[1] = 9;
                mm[2] = 3;
                mm[3] = 8;
                mm[4] = 2;
            }
            break;
        case 2:
        case 6:
            mm[0] = 8;
            if (x < 0) {
                mm[1] = 9;
                mm[2] = 7;
                mm[3] = 6;
                mm[4] = 4;
            }
            else {
                mm[1] = 7;
                mm[2] = 9;
                mm[3] = 4;
                mm[4] = 6;
            }
            break;
        case 4:
            mm[0] = 7;
            if (ay > ax) {
                mm[1] = 8;
                mm[2] = 4;
                mm[3] = 9;
                mm[4] = 1;
            }
            else {
                mm[1] = 4;
                mm[2] = 8;
                mm[3] = 1;
                mm[4] = 9;
            }
            break;
        case 5:
        case 13:
            mm[0] = 4;
            if (y < 0) {
                mm[1] = 1;
                mm[2] = 7;
                mm[3] = 2;
                mm[4] = 8;
            }
            else {
                mm[1] = 7;
                mm[2] = 1;
                mm[3] = 8;
                mm[4] = 2;
            }
            break;
        case 8:
            mm[0] = 3;
            if (ay > ax) {
                mm[1] = 2;
                mm[2] = 6;
                mm[3] = 1;
                mm[4] = 9;
            }
            else {
                mm[1] = 6;
                mm[2] = 2;
                mm[3] = 9;
                mm[4] = 1;
            }
            break;
        case 10:
        case 14:
            mm[0] = 2;
            if (x < 0) {
                mm[1] = 3;
                mm[2] = 1;
                mm[3] = 6;
                mm[4] = 4;
            }
            else {
                mm[1] = 1;
                mm[2] = 3;
                mm[3] = 4;
                mm[4] = 6;
            }
            break;
        case 12:
            mm[0] = 1;
            if (ay > ax) {
                mm[1] = 2;
                mm[2] = 4;
                mm[3] = 3;
                mm[4] = 7;
            }
            else {
                mm[1] = 4;
                mm[2] = 2;
                mm[3] = 7;
                mm[4] = 3;
            }
            break;
    }
}



/*
 * Hack -- local "player stealth" value (see below)
 */
static u32b noise = 0L;


/*
 * Process a monster
 *
 * The monster is known to be within 100 grids of the player
 *
 * In several cases, we directly update the monster lore
 *
 * Note that a monster is only allowed to "reproduce" if there
 * are a limited number of "reproducing" monsters on the current
 * level.  This should prevent the level from being "swamped" by
 * reproducing monsters.  It also allows a large mass of mice to
 * prevent a louse from multiplying, but this is a small price to
 * pay for a simple multiplication method.
 *
 * XXX Monster fear is slightly odd, in particular, monsters will
 * fixate on opening a door even if they cannot open it.  Actually,
 * the same thing happens to normal monsters when they hit a door
 *
 * XXX XXX XXX In addition, monsters which *cannot* open or bash
 * down a door will still stand there trying to open it...
 *
 * XXX Technically, need to check for monster in the way
 * combined with that monster being in a wall (or door?)
 *
 * A "direction" of "5" means "pick a random direction".
 */
static void process_monster(CMonster *m_ptr)
{
    CMonsterRace *r_ptr = m_ptr->get_r_ptr();

    int i, d, ox, oy, nx, ny;

    int mm[8];

    CGrid *g_ptr;
    CItem *i_ptr;
    CMonster *y_ptr;

    bool do_turn;
    bool do_move;
    bool do_view;

    bool did_open_door;
    bool did_bash_door;
    bool did_take_item;
    bool did_kill_item;
    bool did_move_body;
    bool did_kill_body;
    bool did_pass_wall;
    bool did_kill_wall;


    /* Handle "sleep" */
    if (m_ptr->get_csleep()) {
        u32b notice = 0;

        /* Hack -- handle non-aggravation */
        if (!p_ptr->get_aggravate()) notice = rand_int(1024);

        /* Hack -- See if monster "notices" player */
        if ((notice * notice * notice) <= noise) {
            /* Hack -- amount of "waking" */
            int d = 1;

            /* Wake up faster near the player */
            if (m_ptr->get_cdis() < 50) d = 100 / m_ptr->get_cdis();

            /* Hack -- handle aggravation */
            if (p_ptr->get_aggravate()) d = m_ptr->get_csleep();

            /* Still asleep */
            if (m_ptr->get_csleep() > d) {
                /* Monster wakes up "a little bit" */
                m_ptr->set_csleep(m_ptr->get_csleep() - d);

                /* Notice the "not waking up" */
                if (m_ptr->is_visible()) {
                    /* Hack -- Count the ignores */
                    if (r_ptr->r_ignore < MAX_UCHAR) r_ptr->r_ignore++;
                }
            }

            /* Just woke up */
            else {
                /* Reset sleep counter */
                m_ptr->set_csleep(0);

                /* Notice the "waking up" */
                if (m_ptr->is_visible()) {
                    char m_name[80];

                    /* Acquire the monster name */
                    m_ptr->get_desc(m_name, 0);

                    /* Dump a message */
                    msg_format("%^s wakes up.", m_name);

                    /* Hack -- Count the wakings */
                    if (r_ptr->r_wake < MAX_UCHAR) r_ptr->r_wake++;
                }
            }
        }

        /* Still sleeping */
        if (m_ptr->get_csleep()) return;
    }


    /* Handle "stun" */
    if (m_ptr->get_stun()) {
        int d = 1;

        /* Make a "saving throw" against stun */
        if (rand_int(5000) <= r_ptr->level * r_ptr->level) {
            /* Recover fully */
            d = m_ptr->get_stun();
        }

        /* Hack -- Recover from stun */
        if (m_ptr->get_stun() > d) {
            /* Recover somewhat */
            m_ptr->set_stun(m_ptr->get_stun() - d);
        }

        /* Fully recover */
        else {
            /* Recover fully */
            m_ptr->set_stun(0);

            /* Message if visible */
            if (m_ptr->is_visible()) {
                char m_name[80];

                /* Acquire the monster name */
                m_ptr->get_desc(m_name, 0);

                /* Dump a message */
                msg_format("%^s is no longer stunned.", m_name);
            }
        }
    }


    /* Handle confusion */
    if (m_ptr->get_confused()) {
        /* Amount of "boldness" */
        int d = randint(r_ptr->level / 10 + 1);

        /* Still confused */
        if (m_ptr->get_confused() > d) {
            /* Reduce the confusion */
            m_ptr->set_confused(m_ptr->get_confused() - d);
        }

        /* Recovered */
        else {
            /* No longer confused */
            m_ptr->set_confused(0);

            /* Message if visible */
            if (m_ptr->is_visible()) {
                char m_name[80];

                /* Acquire the monster name */
                m_ptr->get_desc(m_name, 0);

                /* Dump a message */
                msg_format("%^s is no longer confused.", m_name);
            }
        }
    }


    /* Handle "fear" */
    if (m_ptr->get_afraid()) {
        /* Amount of "boldness" */
        int d = randint(r_ptr->level / 10 + 1);

        /* Still afraid */
        if (m_ptr->get_afraid() > d) {
            /* Reduce the fear */
            m_ptr->set_afraid(m_ptr->get_afraid() - d);
        }

        /* Recover from fear, take note if seen */
        else {
            /* No longer afraid */
            m_ptr->set_afraid(0);

            /* Visual note */
            if (m_ptr->is_visible()) {
                char m_name[80];
                char m_poss[80];

                /* Acquire the monster name/poss */
                m_ptr->get_desc(m_name, 0);
                m_ptr->get_desc(m_poss, 0x22);

                /* Dump a message */
                msg_format("%^s recovers %s courage.", m_name, m_poss);
            }
        }
    }


    /* Get the origin */
    ox = m_ptr->GetX();
    oy = m_ptr->GetY();


    /* Attempt to "mutiply" if able and allowed */
    if ((r_ptr->flags2 & RF2_MULTIPLY) && (num_repro < MAX_REPRO)) {
        int k, y, x;

        /* Count the adjacent monsters */
        for (k = 0, y = oy - 1; y <= oy + 1; y++) {
            for (x = ox - 1; x <= ox + 1; x++) {
                if (cave[y][x].m_ptr) k++;
            }
        }

        /* Hack -- multiply slower in crowded areas */
        if ((k < 4) && (!k || !rand_int(k * MON_MULT_ADJ))) {
            /* Try to multiply */
            if (multiply_monster(m_ptr)) {
                /* Take note if visible */
                if (m_ptr->is_visible()) r_ptr->r_flags2 |= RF2_MULTIPLY;

                /* Multiplying takes energy */
                return;
            }
        }
    }


    /* Attempt to cast a spell */
    if (m_ptr->make_attack_spell(p_ptr)) return;


    /* Hack -- Assume no movement */
    mm[0] = mm[1] = mm[2] = mm[3] = 0;
    mm[4] = mm[5] = mm[6] = mm[7] = 0;


    /* Confused -- 100% random */
    if (m_ptr->get_confused()) {
        /* Try four "random" directions */
        mm[0] = mm[1] = mm[2] = mm[3] = 5;
    }

    /* 75% random movement */
    else if ((r_ptr->flags1 & RF1_RAND_50) && (r_ptr->flags1 & RF1_RAND_25)) {
        // Move randomly?
        if (percent(75)) {
            /* Memorize flags */
            if (m_ptr->is_visible()) r_ptr->r_flags1 |= RF1_RAND_50;
            if (m_ptr->is_visible()) r_ptr->r_flags1 |= RF1_RAND_25;

            /* Try four "random" directions */
            mm[0] = mm[1] = mm[2] = mm[3] = 5;
        }
        else m_ptr->get_moves(mm);
    }

    /* 50% random movement */
    else if (r_ptr->flags1 & RF1_RAND_50) {
        // Move randomly?
        if (percent(50)) {
            /* Memorize flags */
            if (m_ptr->is_visible()) r_ptr->r_flags1 |= RF1_RAND_50;

            /* Try four "random" directions */
            mm[0] = mm[1] = mm[2] = mm[3] = 5;
        }
        else m_ptr->get_moves(mm);
    }

    /* 25% random movement */
    else if (r_ptr->flags1 & RF1_RAND_25) {
        // Move randomly?
        if (percent(25)) {
            /* Memorize flags */
            if (m_ptr->is_visible()) r_ptr->r_flags1 |= RF1_RAND_25;

            /* Try four "random" directions */
            mm[0] = mm[1] = mm[2] = mm[3] = 5;
        }
        else m_ptr->get_moves(mm);
    }

    /* Normal movement */
    else {
        /* Logical moves */
        m_ptr->get_moves(mm);
    }


    /* Assume nothing */
    do_turn = FALSE;
    do_move = FALSE;
    do_view = FALSE;

    /* Assume nothing */
    did_open_door = FALSE;
    did_bash_door = FALSE;
    did_take_item = FALSE;
    did_kill_item = FALSE;
    did_move_body = FALSE;
    did_kill_body = FALSE;
    did_pass_wall = FALSE;
    did_kill_wall = FALSE;


    /* Take a zero-terminated array of "directions" */
    for (i = 0; mm[i]; i++) {
        /* Get the direction */
        d = mm[i];

        /* Hack -- allow "randomized" motion */
        if (d == 5) d = ddd[rand_int(8)];

        /* Get the destination */
        ny = oy + ddy[d];
        nx = ox + ddx[d];

        /* Access that cave grid */
        g_ptr = &cave[ny][nx];

        /* Access that cave grid's contents */
        i_ptr = g_ptr->i_ptr;

        /* Access that cave grid's contents */
        y_ptr = g_ptr->m_ptr;


        /* Floor is open? */
        if (floor_grid_bold(ny, nx)) {
            /* Go ahead and move */
            do_move = TRUE;
        }

        /* Permanent wall */
        else if (g_ptr->is_permawall()) {
            /* Nothing */
        }

        /* Monster moves through walls (and doors) */
        else if (r_ptr->flags2 & RF2_PASS_WALL) {
            /* Pass through walls/doors/rubble */
            do_move = TRUE;

            /* Monster went through a wall */
            did_pass_wall = TRUE;
        }

        /* Monster destroys walls (and doors) */
        else if (r_ptr->flags2 & RF2_KILL_WALL) {
            /* Eat through walls/doors/rubble */
            do_move = TRUE;

            /* Monster destroyed a wall */
            did_kill_wall = TRUE;

            /* Clear the wall code, if any */
            g_ptr->set_feat(CF_FLOOR);

            /* Forget the "field mark", if any */
            g_ptr->flags &= ~MAP_KNOW;

            /* Notice */
            note_spot(ny, nx);

            /* Note changes to viewable region */
            if (player_has_los_bold(ny, nx)) do_view = TRUE;
        }

        // Handle doors and secret doors (note: open/broken doors already handled)
        else if (g_ptr->is_door()) {
            bool may_bash = TRUE;

            /* Take a turn */
            do_turn = TRUE;

            /* Creature can open doors. */
            if (r_ptr->flags2 & RF2_OPEN_DOOR) {
                /* Closed doors and secret doors */
                if ((g_ptr->get_feat() == CF_DOOR_CLOSED) ||
                    (g_ptr->get_feat() == CF_DOOR_SECRET))
                {
                    /* The door is open */
                    did_open_door = TRUE;

                    /* Do not bash the door */
                    may_bash = FALSE;
                }

                //: Locked doors
                else {
                    /* Try to unlock it XXX XXX XXX */
                    if (rand_int(m_ptr->GetCHP() / 10) > g_ptr->get_door_lock_strength()) {
                        /* Unlock the door */
                        g_ptr->set_feat(CF_DOOR_CLOSED);

                        /* Do not bash the door */
                        may_bash = FALSE;
                    }
                }
            }

            // Attempt to bash if allowed
            if (may_bash && (r_ptr->flags2 & RF2_BASH_DOOR)) {
                /* Attempt to Bash XXX XXX XXX */
                if (rand_int(m_ptr->GetCHP() / 10) > g_ptr->get_door_lock_strength()) {
                    /* Message */
                    msg_print("You hear a door burst open!");

                    /* The door was bashed open */
                    did_bash_door = TRUE;

                    /* Hack -- fall into doorway */
                    do_move = TRUE;
                }
            }


            /* Deal with doors in the way */
            if (did_open_door || did_bash_door) {
                /* Break down the door */
                if (did_bash_door && percent(50)) {
                    g_ptr->set_feat(CF_DOOR_BROKEN);
                }

                /* Open the door */
                else {
                    g_ptr->set_feat(CF_DOOR_OPEN);
                }

                /* Notice */
                note_spot(ny, nx);

                /* Handle viewable doors */
                if (player_has_los_bold(ny, nx)) do_view = TRUE;
            }
        }


        /* Hack -- check for Glyph of Warding */
        if (do_move && (g_ptr->get_feat() == CF_GLYPH)) {
            /* Assume no move allowed */
            do_move = FALSE;

            /* Break the ward */
            if (randint(BREAK_GLYPH) < r_ptr->level) {
                /* Describe observable breakage */
                if (g_ptr->flags & MAP_KNOW) {
                    msg_print("The rune of protection is broken!");
                }

                /* Break the rune */
                g_ptr->set_feat(CF_FLOOR);

                /* Allow movement */
                do_move = TRUE;
            }
        }

        /* The player is in the way.  Attack him. */
        if (do_move && p_ptr->is_at(nx, ny)) {
            // Some monsters never attack
            if (r_ptr->flags1 & RF1_NEVER_BLOW) {
                /* Hack -- memorize lack of attacks */
                /* if (m_ptr->ml) r_ptr->r_flags1 |= RF1_NEVER_BLOW; */

                /* Do not move */
                do_move = FALSE;
            }

            // Attack
            else {
                /* Do the attack */
                m_ptr->make_attack_normal();

                /* Do not move */
                do_move = FALSE;

                /* Took a turn */
                do_turn = TRUE;
            }
        }


        /* Some monsters never move */
        if (do_move && (r_ptr->flags1 & RF1_NEVER_MOVE)) {
            /* Hack -- memorize lack of attacks */
            /* if (m_ptr->ml) r_ptr->r_flags1 |= RF1_NEVER_MOVE; */

            /* Do not move */
            do_move = FALSE;
        }


        /* A monster is in the way */
        if (do_move && g_ptr->m_ptr) {
            CMonsterRace *z_ptr = y_ptr->get_r_ptr();

            /* Assume no movement */
            do_move = FALSE;

            /* Kill weaker monsters */
            if ((r_ptr->flags2 & RF2_KILL_BODY) &&
                (r_ptr->mexp > z_ptr->mexp))
            {
                /* Allow movement */
                do_move = TRUE;

                /* Monster ate another monster */
                did_kill_body = TRUE;

                /* XXX XXX XXX Message */

                /* Kill the monster */
                delete_monster(ny, nx);

                /* Hack -- get the empty monster */
                y_ptr = g_ptr->m_ptr;
            }

            /* Push past weaker monsters (unless leaving a wall) */
            if ((r_ptr->flags2 & RF2_MOVE_BODY) &&
                (r_ptr->mexp > z_ptr->mexp) &&
                (floor_grid_bold(m_ptr->GetY(), m_ptr->GetX())))
            {
                /* Allow movement */
                do_move = TRUE;

                /* Monster pushed past another monster */
                did_move_body = TRUE;

                /* XXX XXX XXX Message */
            }
        }


        /* Creature has been allowed move */
        if (do_move) {
            /* Take a turn */
            do_turn = TRUE;

            /* Hack -- Update the old location */
            cave[oy][ox].m_ptr = g_ptr->m_ptr;

            /* Mega-Hack -- move the old monster, if any */
            if (g_ptr->m_ptr) {
                // Move the old monster
                y_ptr->SetLocation(ox, oy);

                /* Update the old monster */
                y_ptr->update();
            }

            /* Hack -- Update the new location */
            g_ptr->m_ptr = m_ptr;

            // Move the monster
            m_ptr->SetLocation(nx, ny);

            // Set the action
            m_ptr->action = d;

            /* Update the monster */
            m_ptr->update();

            /* Take or Kill objects (not "gold") on the floor */
            if (i_ptr &&
                ((r_ptr->flags2 & RF2_TAKE_ITEM) ||
                (r_ptr->flags2 & RF2_KILL_ITEM)))
            {
                u32b f1, f2, f3;

                u32b flg3 = 0L;

                char m_name[80];
                char i_name[80];

                CItem *next_i_ptr;

                /* Loop through pile */
                while (i_ptr) {
                    // Get next object
                    next_i_ptr = i_ptr->next_i_ptr;

                    /* Check for gold */
                    if (i_ptr->GetTval() == TV_GOLD) {
                        i_ptr = next_i_ptr;
                        continue;
                    }

                    /* Extract some flags */
                    i_ptr->GetFlags(&f1, &f2, &f3);

                    /* Acquire the object name */
                    i_ptr->object_desc(i_name, TRUE, 3);

                    /* Acquire the monster name */
                    m_ptr->get_desc(m_name, 0x04);

                    /* React to objects that hurt the monster */
                    if (f1 & TR1_KILL_DRAGON) flg3 |= RF3_DRAGON;
                    if (f1 & TR1_SLAY_DRAGON) flg3 |= RF3_DRAGON;
                    if (f1 & TR1_SLAY_TROLL) flg3 |= RF3_TROLL;
                    if (f1 & TR1_SLAY_GIANT) flg3 |= RF3_GIANT;
                    if (f1 & TR1_SLAY_ORC) flg3 |= RF3_ORC;
                    if (f1 & TR1_SLAY_DEMON) flg3 |= RF3_DEMON;
                    if (f1 & TR1_SLAY_UNDEAD) flg3 |= RF3_UNDEAD;
                    if (f1 & TR1_SLAY_ANIMAL) flg3 |= RF3_ANIMAL;
                    if (f1 & TR1_SLAY_EVIL) flg3 |= RF3_EVIL;

                    /* The object cannot be picked up by the monster */
                    if (i_ptr->isArtifact() || (r_ptr->flags3 & flg3)) {
                        /* Only give a message for "take_item" */
                        if (r_ptr->flags2 & RF2_TAKE_ITEM) {
                            /* Take note */
                            did_take_item = TRUE;

                            /* Describe observable situations */
                            if (m_ptr->is_visible() && player_has_los_bold(ny, nx)) {
                                /* Dump a message */
                                msg_format("%^s tries to pick up %s but fails.",
                                    m_name, i_name);
                            }
                        }
                    }

                    /* Pick up the item */
                    else if (r_ptr->flags2 & RF2_TAKE_ITEM) {
                        /* Take note */
                        did_take_item = TRUE;

                        /* Describe observable situations */
                        if (player_has_los_bold(ny, nx)) {
                            /* Dump a message */
                            msg_format("%^s picks up %s.", m_name, i_name);
                        }

                        // Make an extra copy of the object
                        // XXX this code is not remotely elegant
                        CItem temp = *i_ptr;

                        // Delete the original
                        delete_object(i_ptr);

                        // Get a new object
                        CItem *j_ptr = new CItem;

                        // Copy into that new place
                        *j_ptr = temp;

                        // Hook it in
                        j_ptr->next_i_ptr = m_ptr->i_ptr;
                        m_ptr->i_ptr = j_ptr;
                    }

                    // Destroy the item
                    else {
                        // Take note
                        did_kill_item = TRUE;

                        // Describe observable situations
                        if (player_has_los_bold(ny, nx)) {
                            // Dump a message
                            msg_format("%^s crushes %s.", m_name, i_name);
                        }

                        // Delete the object
                        delete_object(i_ptr);
                    }

                    // Get next object
                    i_ptr = next_i_ptr;
                }
            }
        }

        // Stop when done
        if (do_turn) break;
    }


    // Notice changes in view
    if (do_view) {
        // Update some things
        p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | PU_FLOW | PU_MONSTERS);
    }


    // Learn things from observable monster
    if (m_ptr->is_visible()) {
        // Monster opened a door
        if (did_open_door) r_ptr->r_flags2 |= RF2_OPEN_DOOR;

        // Monster bashed a door
        if (did_bash_door) r_ptr->r_flags2 |= RF2_BASH_DOOR;

        // Monster tried to pick something up
        if (did_take_item) r_ptr->r_flags2 |= RF2_TAKE_ITEM;

        // Monster tried to crush something
        if (did_kill_item) r_ptr->r_flags2 |= RF2_KILL_ITEM;

        // Monster pushed past another monster
        if (did_move_body) r_ptr->r_flags2 |= RF2_MOVE_BODY;

        // Monster ate another monster
        if (did_kill_body) r_ptr->r_flags2 |= RF2_KILL_BODY;

        // Monster passed through a wall
        if (did_pass_wall) r_ptr->r_flags2 |= RF2_PASS_WALL;

        // Monster destroyed a wall
        if (did_kill_wall) r_ptr->r_flags2 |= RF2_KILL_WALL;
    }


    /* Hack -- get "bold" if out of options */
    if (!do_turn && !do_move && m_ptr->get_afraid()) {
        /* No longer afraid */
        m_ptr->set_afraid(0);

        /* Message if seen */
        if (m_ptr->is_visible()) {
            char m_name[80];

            /* Acquire the monster name */
            m_ptr->get_desc(m_name, 0);

            /* Dump a message */
            msg_format("%^s turns to fight!", m_name);
        }

        /* XXX XXX XXX Actually do something now (?) */
    }
}




/*
 * Process all the "live" monsters, once per game turn.
 *
 * During each game turn, we scan through the list of all the "live" monsters,
 * (backwards, so we can excise any "freshly dead" monsters), energizing each
 * monster, and allowing fully energized monsters to move, attack, pass, etc.
 *
 * Note that monsters can never move in the monster array (except when the
 * "compact_monsters()" function is called by "dungeon()" or "save_player()").
 *
 * This function is responsible for at least half of the processor time
 * on a normal system with a "normal" amount of monsters and a player doing
 * normal things.
 *
 * When the player is resting, virtually 90% of the processor time is spent
 * in this function, and its children, "process_monster()" and "make_move()".
 *
 * Most of the rest of the time is spent in "update_view()" and "prt_map()",
 * especially when the player is running.
 */
void process_monsters(void)
{
    int e, fx, fy, x, y;
    bool test;
    byte speed;
    CMonsterRace *r_ptr;


    // Hack -- calculate the "player noise"
    noise = (1L << (30 - p_ptr->GetSkill(SKILL_STL)));



    // Go through every tile
    for (x = 0; x < cur_wid; x++) {
        for (y = 0; y < cur_hgt; y++) {
            CMonster *m_ptr = cave[y][x].m_ptr;

            // If there is no monster, skip
            if (!m_ptr) continue;

            // Get race
            r_ptr = m_ptr->get_r_ptr();


            // Make this monster detected for a little less time
            if (m_ptr->detect) m_ptr->detect--;


            // Obtain the monster's speed
            speed = m_ptr->get_speed();

            // Obtain the energy boost
            e = extract_energy[speed];

            // Give this monster some energy
            m_ptr->set_busy(m_ptr->get_busy() - e);


            // Still busy?
            if (m_ptr->get_busy() > 0) continue;

            // Be busy for a while, have no current action
            m_ptr->set_busy(m_ptr->get_busy() + 100);
            m_ptr->action = 0;


            // Hack -- Require proximity */
            if (m_ptr->get_cdis() >= 100) continue;


            // Access the location */
            fx = m_ptr->GetX();
            fy = m_ptr->GetY();


            // Assume no move
            test = FALSE;

            // Handle "sensing radius"
            if (m_ptr->get_cdis() <= r_ptr->aaf) {
                // We can "sense" the player
                test = TRUE;
            }

            // Handle "sight" and "aggravation"
            else if ((m_ptr->get_cdis() <= MAX_SIGHT) &&
                     (player_has_los_bold(fy, fx) ||
                      p_ptr->get_aggravate()))
            {
                // We can "see" or "feel" the player
                test = TRUE;
            }

            // Do nothing
            if (!test) continue;


            // Process the monster
            process_monster(m_ptr);

            // Hack -- notice death or departure
            if (!alive || death || new_level_flag) return;
        }
    }
}