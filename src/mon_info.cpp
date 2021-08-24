/* File: mon_desc.cpp */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 * 						Jeff Greene, Diego Gonzalez
 *
 * Please see copyright.txt for complete copyright and licensing restrictions.
 *
 */

#include "src/npp.h"

/*
 * Pronoun arrays, by gender.
 */
static QString wd_he[3] =
{ "it", "he", "she" };
static QString wd_his[3] =
{ "its", "his", "her" };

/*
 * Determine if the "armor" is known
 * The higher the level, the fewer kills needed.
 */
static bool know_armour(int r_idx, s32b kills)
{
    const monster_race *r_ptr = &r_info[r_idx];

    s32b level = r_ptr->level;

    if (p_ptr->is_wizard) return (TRUE);

    /* Normal monsters */
    if (kills > 304 / (4 + level)) return (TRUE);

    /* Skip non-uniques */
    if (!(r_ptr->flags1 & (RF1_UNIQUE))) return (FALSE);

    /* Unique monsters */
    if (kills > 304 / (38 + (5*level) / 4)) return (TRUE);

    /* Assume false */
    return (FALSE);
}


/*
 * Determine if the "mana" is known
 * The higher the level, the fewer kills needed.
 */
static bool know_mana_or_spells(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    s32b level = r_ptr->level;

    s32b kills = l_list[r_idx].tkills;

    if (p_ptr->is_wizard) return (TRUE);

    /*Hack - always know about ghosts*/
    if (r_ptr->flags2 & (RF2_PLAYER_GHOST)) return (TRUE);

    /* Mages learn quickly. */
    if (cp_ptr->spell_book == TV_MAGIC_BOOK) kills *= 2;

    /* Normal monsters */
    if (kills > 304 / (4 + level)) return (TRUE);

    /* Skip non-uniques */
    if (!(r_ptr->flags1 & RF1_UNIQUE)) return (FALSE);

    /* Unique monsters */
    if (kills > 304 / (38 + (5 * level) / 4)) return (TRUE);

    /* Assume false */
    return (FALSE);
}


/*
 * Determine if the "damage" of the given attack is known
 * the higher the level of the monster, the fewer the attacks you need,
 * the more damage an attack does, the more attacks you need
 */
static bool know_damage(int r_idx, const monster_lore *l_ptr, int i)
{
    const monster_race *r_ptr = &r_info[r_idx];

    if (p_ptr->is_wizard) return (TRUE);

    s32b level = r_ptr->level;

    s32b a = l_ptr->blows[i];

    s32b d1 = r_ptr->blow[i].d_dice;
    s32b d2 = r_ptr->blow[i].d_side;

    s32b d = d1 * d2;

    /* Hack - keep the target number reasonable */
    if (d > 100) d = 100;

    /* Normal monsters */
    if ((4 + level) * a > 80 * d) return (TRUE);

    /* Skip non-uniques */
    if (!(r_ptr->flags1 & RF1_UNIQUE)) return (FALSE);

    /* Unique monsters */
    if ((4 + level) * (2 * a) > 80 * d) return (TRUE);

    /* Assume false */
    return (FALSE);
}


static QString describe_monster_desc(int r_idx)
{
    const monster_race *r_ptr = &r_info[r_idx];
    QString output;

    /* Simple method */
    output =  r_ptr->r_text;

    /* Dump it */
    output.append("<br><br>");
    return (output);
}


QString describe_monster_spells(int r_idx, const monster_lore *l_ptr)
{
    QString output;
    const monster_race *r_ptr = &r_info[r_idx];
    int m, n;
    int msex = 0;
    int spower;
    bool breath = FALSE;
    bool magic = FALSE;
    QVector<QString> vp;
    vp.clear();

    output.clear();

    /* Extract a gender (if applicable) */
    if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
    else if (r_ptr->flags1 & RF1_MALE) msex = 1;

    /* Get spell power */
    spower = r_ptr->spell_power;

    if (l_ptr->r_l_flags4 & (RF4_SHRIEK))		vp.append("shriek for help");

    if (l_ptr->r_l_flags4 & (RF4_LASH))
    {
        if ((l_ptr->r_l_flags3 & (RF3_ANIMAL)) || (r_ptr->blow[0].effect == RBE_ACID))
            vp.append("spit at you from a distance");
        else
            vp.append("lash you if nearby");
    }

    if (l_ptr->r_l_flags4 & (RF4_BOULDER))
    {
        if (spower < 8) vp.append("throw rocks");
        else vp.append("throw boulders");
    }

    if (l_ptr->r_l_flags4 & (RF4_SHOT))
    {
        if (spower < 4) vp.append("sling pebbles");
        else if (spower < 10) vp.append("sling leaden pellets");
        else vp.append("sling seeker shots");
    }

    if (l_ptr->r_l_flags4 & (RF4_ARROW))
    {
        if (spower < 4) vp.append("shoot little arrows");
        else if (spower < 10) vp.append("shoot arrows");
        else vp.append("shoot seeker arrows");
    }

    if (l_ptr->r_l_flags4 & (RF4_BOLT))
    {
        if (spower < 4) vp.append("fire bolts");
        else if (spower < 10) vp.append("fire crossbow quarrels");
        else vp.append("fire seeker bolts");
    }

    if (l_ptr->r_l_flags4 & (RF4_MISSL))
    {
        if (spower < 4) vp.append("fire little missiles");
        else if (spower < 10) vp.append("fire missiles");
        else vp.append("fire heavy missiles");
    }

    if (l_ptr->r_l_flags4 & (RF4_PMISSL)) vp.append("whip poisoned darts");

    /* Describe innate attacks */
    if (vp.size())
    {
        /* Intro */
        output.append(QString("%1") .arg(capitalize_first(wd_he[msex])));

        /* Scan */
        for (n = 0; n < vp.size(); n++)
        {
            /* Intro */
            if (n == 0)
            {
                output.append(" may ");
            }

            else if (n < vp.size()-1) output.append(", ");
            else if (n == 1) output.append(" or ");
            else output.append(", or ");

            /* Dump */
            output.append(color_string(capitalize_first(vp[n]), TERM_RED));
        }

        /* End */
        output.append(".  ");
    }

    /* Collect breaths */
    vp.clear();

    if (l_ptr->r_l_flags4 & (RF4_BRTH_ACID))       vp.append("acid");
    if (l_ptr->r_l_flags4 & (RF4_BRTH_ELEC))       vp.append("lightning");
    if (l_ptr->r_l_flags4 & (RF4_BRTH_FIRE))       vp.append("fire");
    if (l_ptr->r_l_flags4 & (RF4_BRTH_COLD))       vp.append("frost");
    if (l_ptr->r_l_flags4 & (RF4_BRTH_POIS))       vp.append("poison");
    if (l_ptr->r_l_flags4 & (RF4_BRTH_PLAS))       vp.append("plasma");

    if (l_ptr->r_l_flags4 & (RF4_BRTH_LIGHT))       vp.append("light");
    if (l_ptr->r_l_flags4 & (RF4_BRTH_DARK))	   vp.append("darkness");
    if (l_ptr->r_l_flags4 & (RF4_BRTH_CONFU))      vp.append("confusion");
    if (l_ptr->r_l_flags4 & (RF4_BRTH_SOUND))      vp.append("sound");
    if (l_ptr->r_l_flags4 & (RF4_BRTH_SHARD))      vp.append("shards");
    if (l_ptr->r_l_flags4 & (RF4_BRTH_INER))       vp.append("inertia");
    if (l_ptr->r_l_flags4 & (RF4_BRTH_GRAV))       vp.append("gravity");
    if (l_ptr->r_l_flags4 & (RF4_BRTH_FORCE))      vp.append("force");

    if (l_ptr->r_l_flags4 & (RF4_BRTH_NEXUS))      vp.append("nexus");
    if (l_ptr->r_l_flags4 & (RF4_BRTH_NETHR))      vp.append("nether");
    if (l_ptr->r_l_flags4 & (RF4_BRTH_CHAOS))      vp.append("chaos");
    if (l_ptr->r_l_flags4 & (RF4_BRTH_DISEN))      vp.append("disenchantment");
    if (l_ptr->r_l_flags4 & (RF4_BRTH_TIME))       vp.append("time");
    if (l_ptr->r_l_flags4 & (RF4_BRTH_MANA))       vp.append("mana");

    if (l_ptr->r_l_flags4 & (RF4_RF4XXX1))         vp.append("something");
    if (l_ptr->r_l_flags4 & (RF4_RF4XXX2))            vp.append("something");
    if (l_ptr->r_l_flags4 & (RF4_RF4XXX3))            vp.append("something");

    /* Describe breaths */
    if (vp.size())
    {
        /* Note breath */
        breath = TRUE;

        /* Intro */
        output.append(QString("%1") .arg(capitalize_first(wd_he[msex])));

        /* Scan */
        for (n = 0; n < vp.size(); n++)
        {
            /* Intro */
            if (n == 0) output.append(" may breathe ");
            else if (n < vp.size()-1) output.append(", ");
            else if (n == 1) output.append(" or ");
            else output.append(", or ");

            /* Dump */
            output.append(color_string(vp[n], TERM_RED));
        }

        /*note powerful*/
        if (l_ptr->r_l_flags2 & (RF2_POWERFUL)) output.append(" powerfully");
    }


    /* Collect spells */
    vp.clear();

    if (l_ptr->r_l_flags5 & (RF5_BALL_ACID))
    {
        if (r_ptr->flags4 & (RF4_BRTH_ACID))
        {
            if (spower < 40)	vp.append("breathe acid balls");
            else 				vp.append("breathe enormous acid balls");
        }
        else if (spower < 10) vp.append("produce small acid balls");
        else if (spower < 40) vp.append("produce acid balls");
        else vp.append("produce acid storms");
    }

    if (l_ptr->r_l_flags5 & (RF5_BALL_ELEC))
    {
        if (r_ptr->flags4 & (RF4_BRTH_ELEC))
        {
            if (spower < 40)	vp.append("breathe lightning balls");
            else 				vp.append("breathe enormous lightning balls");
        }
        else if (spower < 10) vp.append("produce small lightning balls");
        else if (spower < 40) vp.append("produce lightning balls");
        else vp.append("produce lightning storms");
    }

    if (l_ptr->r_l_flags5 & (RF5_BALL_FIRE))
    {
        if (r_ptr->flags4 & (RF4_BRTH_FIRE))
        {
            if (spower < 40)	vp.append("breathe balls of flames");
            else 				vp.append("breathe enormous balls of flames");
        }
        else if (spower < 10) vp.append("produce small fire balls");
        else if (spower < 40) vp.append("produce fire balls");
        else vp.append("produce fire storms");
    }

    if (l_ptr->r_l_flags5 & (RF5_BALL_COLD))
    {
        if (r_ptr->flags4 & (RF4_BRTH_COLD))
        {
            if (spower < 40)	vp.append("breathe balls of frost");
            else 				vp.append("breathe enormous balls of frost");
        }
        else if (spower < 10) vp.append("produce small frost balls");
        else if (spower < 40) vp.append("produce frost balls");
        else vp.append("produce frost storms");
    }

    if (l_ptr->r_l_flags5 & (RF5_BALL_POIS))
    {
        if (r_ptr->flags4 & (RF4_BRTH_POIS))
        {
            if (spower < 40)	vp.append("breathe balls of poison");
            else 				vp.append("breathe enormous balls of poison");
        }
        else if (spower < 10) vp.append("produce stinking clouds");
        else if (spower < 40) vp.append("produce poison balls");
        else vp.append("produce storms of poison");
    }

    if (l_ptr->r_l_flags5 & (RF5_BALL_LIGHT))
    {
        if (r_ptr->flags4 & (RF4_BRTH_LIGHT))
        {
            if (spower < 40)	vp.append("breathe balls of light");
            else 				vp.append("breathe brilliant balls of light");
        }
        else if (spower < 10) vp.append("produce spheres of light");
        else if (spower < 40) vp.append("produce explosions of light");
        else vp.append("produce powerful explosions of light");
    }

    if (l_ptr->r_l_flags5 & (RF5_BALL_DARK))
    {
        if (r_ptr->flags4 & (RF4_BRTH_DARK))
        {
            if (spower < 40)	vp.append("breathe balls of darkness");
            else 				vp.append("breathe enormous balls of darkness");
        }
        else if (spower < 20) vp.append("produce balls of darkness");
        else if (spower < 70) vp.append("produce storms of darkness");
        else vp.append("produce powerful storms of darkness");
    }

    if (l_ptr->r_l_flags5 & (RF5_BALL_CONFU))
    {
        if (r_ptr->flags4 & (RF4_BRTH_CONFU))
        {
            if (spower < 40)	vp.append("breathe balls of confusion");
            else 				vp.append("breathe massive balls of confusion");
        }
        else if (spower < 10) vp.append("produce balls of confusion");
        else if (spower < 40) vp.append("produce storms of confusion");
        else vp.append("produce powerful storms of confusion");
    }

    if (l_ptr->r_l_flags5 & (RF5_BALL_SOUND))
    {
        if (r_ptr->flags4 & (RF4_BRTH_SOUND))
        {
            if (spower < 40)	vp.append("breathe balls of noise");
            else 				vp.append("breathe ear-splitting balls of noise");
        }
        else if (spower < 10) vp.append("produce blasts of sound");
        else if (spower < 40) vp.append("produce thunderclaps");
        else vp.append("unleash storms of sound");
    }

    if (l_ptr->r_l_flags5 & (RF5_BALL_SHARD))
    {
        if (r_ptr->flags4 & (RF4_BRTH_SHARD))
        {
            if (spower < 40)	vp.append("breathe balls of shards");
            else 				vp.append("breathe enormous balls of shards");
        }
        else if (spower < 10) vp.append("produce blasts of shards");
        else if (spower < 50) vp.append("produce whirlwinds of shards");
        else vp.append("call up storms of knives");
    }
    if (l_ptr->r_l_flags5 & (RF5_BALL_METEOR))
    {
        if (spower < 10) vp.append("produce meteor showers");
        else if (spower < 50) vp.append("produce meteor storms");
        else vp.append("produce violent meteor storms");
    }

    if (l_ptr->r_l_flags5 & (RF5_BALL_STORM))
    {
        if (spower < 22) vp.append("produce little storms");
        else if (spower < 40) vp.append("produce whirlpools");
        else vp.append("call up raging storms");
    }

    if (l_ptr->r_l_flags5 & (RF5_BALL_NETHR))
    {
        if (r_ptr->flags4 & (RF4_BRTH_NETHR))
        {
            if (spower < 40)	vp.append("breathe nether balls");
            else 				vp.append("breathe enormous nether balls");
        }
        else if (spower < 22) vp.append("produce nether orbs");
        else if (spower < 40) vp.append("produce nether balls");
        else vp.append("invoke nether storms");
    }

    if (l_ptr->r_l_flags5 & (RF5_BALL_CHAOS))
    {
        if (r_ptr->flags4 & (RF4_BRTH_CHAOS))
        {
            if (spower < 40)	vp.append("breathe balls of chaos");
            else 				vp.append("breathe enormous balls of chaos");
        }
        else if (spower < 13) vp.append("produce spheres of chaos");
        else if (spower < 40) vp.append("produce explosions of chaos");
        else vp.append("call up maelstroms of raw chaos");
    }

    if (l_ptr->r_l_flags5 & (RF5_BALL_MANA))
    {
        if (spower < 25) vp.append("produce manabursts");
        else if (spower < 50) vp.append("produce balls of mana");
        else vp.append("invoke mana storms");
    }

    if (l_ptr->r_l_flags5 & (RF5_BALL_WATER))
    {
        if (spower < 16) vp.append("produce water balls");
        else if (spower < 40) vp.append("produce water balls");
        else vp.append("produce storms of water balls");
    }

    if (l_ptr->r_l_flags5 & (RF5_HOLY_ORB))
    {
        if (spower < 25) vp.append("produce orbs of draining");
        else if (spower < 50) vp.append("produce powerful orbs of draining");
        else vp.append("produce large orbs of holy might");
    }

    if (l_ptr->r_l_flags5 & (RF5_BOLT_ACID))		vp.append("produce acid bolts");
    if (l_ptr->r_l_flags5 & (RF5_BOLT_ELEC))		vp.append("produce lightning bolts");
    if (l_ptr->r_l_flags5 & (RF5_BOLT_FIRE))		vp.append("produce fire bolts");
    if (l_ptr->r_l_flags5 & (RF5_BOLT_COLD))		vp.append("produce frost bolts");
    if (l_ptr->r_l_flags5 & (RF5_BOLT_POIS))		vp.append("produce poison bolts");
    if (l_ptr->r_l_flags5 & (RF5_BOLT_PLAS))		vp.append("produce plasma bolts");
    if (l_ptr->r_l_flags5 & (RF5_BOLT_ICE))		vp.append("produce ice bolts");
    if (l_ptr->r_l_flags5 & (RF5_BOLT_WATER))	vp.append("produce water bolts");
    if (l_ptr->r_l_flags5 & (RF5_BOLT_NETHR))
    {
        if (spower < 40) vp.append("produces a nether bolt");
        else vp.append("hurls black bolts of nether");
    }

    if (l_ptr->r_l_flags5 & (RF5_BOLT_MANA))
    {
        if (spower < 5) vp.append("fire magic missiles");
        else vp.append("fire mana bolts");
    }
    if (l_ptr->r_l_flags5 & (RF5_BOLT_GRAV))
    {
        if (spower < 5) vp.append("fires gravity bolts");
        else vp.append("shoots powerful bolts of gravity");
    }

    if (l_ptr->r_l_flags5 & (RF5_BEAM_ELEC))
    {
        if (r_ptr->flags4 & (RF4_BRTH_ELEC))
        {
            vp.append("breathe lightning bolts");
        }
        else vp.append("shoot sparks of lightning");
    }
    if (l_ptr->r_l_flags5 & (RF5_BEAM_ICE))
    {
        if (r_ptr->flags4 & (RF4_BRTH_ELEC))
        {
            vp.append("breathe spears of ice");
        }
        else 	vp.append("shoot lances of ice");
    }

    if (l_ptr->r_l_flags5 & (RF5_BEAM_NETHR))
    {
        if (r_ptr->flags4 & (RF4_BRTH_NETHR))
        {
            vp.append("breathe beams of nether");
        }
        else if (spower < 25) vp.append("shoot beams of nether");
        else if (spower < 50) vp.append("hurl lances of nether");
        else vp.append("shoot rays of death");
    }
    if (l_ptr->r_l_flags5 & (RF5_BEAM_LAVA))
    {
        /* SLightly different message for breathers */
        if (r_ptr->flags4 & (RF4_BRTH_ALL))
        {
            vp.append("breathe streams of fiery lava");
        }
        else if (spower < 25) vp.append("shoots beams of molten magma");
        else if (spower < 50) vp.append("shoots jets of lava");
        else vp.append("shoots searing jets of lava");
    }

    if (l_ptr->r_l_flags6 & RF6_HASTE)       vp.append("haste-self");
    if (l_ptr->r_l_flags6 & (RF6_ADD_MANA))		vp.append("restore mana");
    if (l_ptr->r_l_flags6 & RF6_HEAL)        vp.append("heal-self");
    if (l_ptr->r_l_flags6 & (RF6_CURE))		vp.append("cure what ails it");
    if (l_ptr->r_l_flags6 & RF6_BLINK)       vp.append("blink-self");
    if (l_ptr->r_l_flags6 & RF6_TPORT)       vp.append("teleport-self");
    if (l_ptr->r_l_flags6 & (RF6_TELE_SELF_TO))	vp.append("teleport toward you");
    if (l_ptr->r_l_flags6 & RF6_TELE_TO)     vp.append("teleport to");
    if (l_ptr->r_l_flags6 & RF6_TELE_AWAY)   vp.append("teleport away");
    if (l_ptr->r_l_flags6 & RF6_TELE_LEVEL)  vp.append("teleport level");
    if (l_ptr->r_l_flags6 & RF6_DARKNESS)    vp.append("create darkness");
    if (l_ptr->r_l_flags6 & RF6_TRAPS)       vp.append("create traps");

    if (l_ptr->r_l_flags6 & (RF6_DRAIN_MANA))	vp.append("drain mana");
    if (l_ptr->r_l_flags6 & (RF6_MIND_BLAST))	vp.append("cause mind blasting");
    if (l_ptr->r_l_flags6 & (RF6_BRAIN_SMASH))	vp.append("cause brain smashing");
    if (l_ptr->r_l_flags6 & (RF6_WOUND))
    {
        if (spower < 4) vp.append("cause light wounds");
        else if (spower < 10) vp.append("cause medium wounds");
        else if (spower < 20) vp.append("cause serious wounds");
        else if (spower < 35) vp.append("cause critical wounds");
        else vp.append("cause mortal wounds");
    }
    if (l_ptr->r_l_flags6 & (RF6_HUNGER))		vp.append("cause hunger");
    if (l_ptr->r_l_flags6 & (RF6_SCARE))		vp.append("terrify");
    if (l_ptr->r_l_flags6 & (RF6_BLIND))		vp.append("blind");
    if (l_ptr->r_l_flags6 & (RF6_CONF))		vp.append("confuse");
    if (l_ptr->r_l_flags6 & (RF6_SLOW))		vp.append("slow");
    if (l_ptr->r_l_flags6 & (RF6_HOLD))		vp.append("paralyze");

    m = vp.size();

    /* Summons are described somewhat differently. */
    if (l_ptr->r_l_flags7)
    {

        /* Summons */
        if (l_ptr->r_l_flags7 & (RF7_S_KIN))
        {
            if (r_ptr->flags1 & (RF1_UNIQUE))
            {
                if (r_ptr->flags1 & (RF1_FEMALE)) vp.append("her minions");
                else if (r_ptr->flags1 & (RF1_MALE)) vp.append("his minions");
                else vp.append("its minions");
            }
            else
                vp.append("similar monsters");
        }
        if (l_ptr->r_l_flags7 & (RF7_S_MONSTER))		vp.append("a monster");
        if (l_ptr->r_l_flags7 & (RF7_S_MONSTERS))	vp.append("monsters");
        if (l_ptr->r_l_flags7 & (RF7_S_ANT))		vp.append("ants");
        if (l_ptr->r_l_flags7 & (RF7_S_SPIDER))		vp.append("spiders");
        if (l_ptr->r_l_flags7 & (RF7_S_HOUND))		vp.append("hounds");
        if (l_ptr->r_l_flags7 & (RF7_S_ANIMAL))		vp.append("natural creatures");
        if (l_ptr->r_l_flags7 & (RF7_S_HYDRA))		vp.append("hydras");
        if (l_ptr->r_l_flags7 & (RF7_S_THIEF))		vp.append("thieves");
        if (l_ptr->r_l_flags7 & (RF7_S_BERTBILLTOM))	vp.append("his friends");
        if (l_ptr->r_l_flags7 & (RF7_S_DRAGON))		vp.append("a dragon");
        if (l_ptr->r_l_flags7 & (RF7_S_HI_DRAGON))	vp.append("Ancient Dragons");
        if (l_ptr->r_l_flags7 & (RF7_S_AINU))		vp.append("a maia");
        if (l_ptr->r_l_flags7 & (RF7_S_DEMON))		vp.append("a demon");
        if (l_ptr->r_l_flags7 & (RF7_S_HI_DEMON))	vp.append("Greater Demons");
        if (l_ptr->r_l_flags7 & (RF7_S_UNIQUE))		vp.append("Unique Monsters");
        if (l_ptr->r_l_flags7 & (RF7_S_HI_UNIQUE))	vp.append("Greater Unique Monsters");
        if (l_ptr->r_l_flags7 & (RF7_S_UNDEAD))		vp.append("an undead");
        if (l_ptr->r_l_flags7 & (RF7_S_HI_UNDEAD))	vp.append("Greater Undead");
        if (l_ptr->r_l_flags7 & (RF7_S_WRAITH))		vp.append("the Ringwraiths");

    }

    /* Describe spells */
    if (vp.size())
    {
        /* Note magic */
        magic = TRUE;

        /* Intro */
        if (breath)
        {
            output.append(", and is also");
        }
        else
        {
            output.append(QString("%1 is") .arg(capitalize_first(wd_he[msex])));
        }

        /* Verb Phrase */
        output.append(" magical, casting spells");

        /* Adverb */
        if (l_ptr->r_l_flags2 & RF2_SMART) output.append(color_string(" intelligently", TERM_PURPLE));

        /* Normal spells */
        for (n = 0; n < m; n++)
        {
            if (n == 0)       output.append(" which ");
            else if (n < m-1) output.append(", ");
            else if (n != 1)  output.append(", or ");
            else              output.append(" or ");

            /* Dump */
            output.append(color_string(vp[n], TERM_RED));
        }

        /* Summons */
        for (n = m; n < vp.size(); n++)
        {
            if (n == 0) output.append(" which summon ");
            else if (n == m) output.append(", or summon ");
            else if (n < vp.size()-1) output.append(", ");
            else if (n == m+1) output.append(" or ");
            else output.append(", or ");

            /* Dump */
            output.append(color_string(vp[n], TERM_L_RED));
        }
    }


    /* End the sentence about innate/other spells */
    if (breath || magic)
    {
        /* Total casting */
        m = l_ptr->ranged;

        /* Average frequency */
        n = (r_ptr->freq_ranged);

        /*players don't hone in on spell frequency right away*/
        if (m < 75)
        {
            /*sometimes minus, sometimes plus*/
            if (n % 2) n -= ((100 - m) / 10);
            else n += ((100 - m) / 10);

            /*boundry control*/
            if (n > 100) n = 100;
            if (n < 1) n = 1;

        }

        /* Describe the spell frequency */
        if (m > 30)
        {
            output.append(QString(" about %1 percent of the time") .arg(n));
        }

        /* Describe monster mana and spellpower*/
        if (((r_ptr->mana) || (r_ptr->spell_power)) && know_mana_or_spells(r_idx))
        {
            output.append(" with");

            /* Mana */
            if (r_ptr->mana)
            {
                output.append(QString(" a mana rating of %1") .arg(r_ptr->mana));

                if (r_ptr->spell_power) output.append(" and");
            }

            /* spell power */
            if (r_ptr->spell_power)
            {
                output.append(QString(" a spell power of %1") .arg(r_ptr->spell_power));
            }
        }

        /* End this sentence */
        output.append(".  ");
    }

    return (output);
}


static QString describe_monster_drop(int r_idx, const monster_lore *l_ptr)
{
    const monster_race *r_ptr = &r_info[r_idx];

    bool sin = FALSE;

    int n;

    QString output, p;

    int msex = 0;

    output.clear();
    p.clear();

    /* Extract a gender (if applicable) */
    if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
    else if (r_ptr->flags1 & RF1_MALE) msex = 1;

    /* No Drops gold and/or items */
    if (!l_ptr->drop_gold && !l_ptr->drop_item) return (output);

    /* Intro */
    output.append(QString("%1 may carry") .arg(capitalize_first(wd_he[msex])));

    /* Count maximum drop */
    n = MAX(l_ptr->drop_gold, l_ptr->drop_item);

    /* One drop (may need an "n") */
    if (n == 1)
    {
        output.append(" a");
        sin = TRUE;
    }

    /* Two drops */
    else if (n == 2)
    {
        output.append(" one or two");
    }

    /* Many drops */
    else
    {
        output.append(QString(" up to %1") .arg(n));
    }


    /* Chests are not noted as good or great
     * (no "n" needed)
     */
    if (l_ptr->r_l_flags1 & RF1_DROP_CHEST)
    {
        sin = FALSE;
    }

    /* Great */
    else if (l_ptr->r_l_flags1 & RF1_DROP_GREAT)
    {
        p.append(" exceptional");
    }

    /* Good (no "n" needed) */
    else if (l_ptr->r_l_flags1 & RF1_DROP_GOOD)
    {
        p.append(" good");
        sin = FALSE;
    }

    /* Objects */
    if (l_ptr->drop_item)
    {
        /* Handle singular "an" */
        if (sin) output.append("n");
        sin = FALSE;

        /* Dump "object(s)" */
        if (!p.isEmpty()) output.append(p);

        /*specify chests where needed*/
        if (l_ptr->r_l_flags1 & RF1_DROP_CHEST) output.append(" chest");
        else output.append(" object");
        if (n != 1) output.append("s");

        /* Conjunction replaces variety, if needed for "gold" below */
        p = " or";
    }

    /* Treasures */
    if (l_ptr->drop_gold)
    {
        /* Cancel prefix */
        if (p.isEmpty()) sin = FALSE;

        /* Handle singular "an" */
        if (sin) output.append("n");

        /* Dump "treasure(s)" */
        if (!p.isEmpty()) output.append(p);
        output.append(" treasure");
        if (n != 1) output.append("s");
    }
    /* End this sentence */
    output.append(".  ");
    return (output);

}


static QString describe_monster_attack(int r_idx, const monster_lore *l_ptr)
{
    const monster_race *r_ptr = &r_info[r_idx];
    int m, r, n;
    QString p, q;
    QString output;
    output.clear();
    p.clear();
    q.clear();

    int msex = 0;

    /* Extract a gender (if applicable) */
    if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
    else if (r_ptr->flags1 & RF1_MALE) msex = 1;

    /* Count the number of "known" attacks */
    for (n = 0, m = 0; m < MONSTER_BLOW_MAX; m++)
    {
        /* Skip non-attacks */
        if (!r_ptr->blow[m].method) continue;

        /* Count known attacks */
        if ((l_ptr->blows[m]) || (l_ptr->sights == SHRT_MAX) ||
                                  (l_ptr->ranged == UCHAR_MAX)) n++;
    }

    /* Examine (and count) the actual attacks */
    for (r = 0, m = 0; m < MONSTER_BLOW_MAX; m++)
    {
        int method, effect, d1, d2;

        /* Skip non-attacks */
        if (!r_ptr->blow[m].method) continue;

        /* Skip unknown attacks */
        if (!l_ptr->blows[m]) continue;

        /* Extract the attack info */
        method = r_ptr->blow[m].method;
        effect = r_ptr->blow[m].effect;
        d1 = r_ptr->blow[m].d_dice;
        d2 = r_ptr->blow[m].d_side;

        /* Get the method */
        switch (method)
        {
            case RBM_HIT:           p = "hit"; break;
            case RBM_TOUCH:         p = "touch"; break;
            case RBM_PUNCH:         p = "punch"; break;
            case RBM_KICK:          p = "kick"; break;
            case RBM_CLAW:          p = "claw"; break;
            case RBM_BITE:          p = "bite"; break;
            case RBM_PECK:          p = "peck"; break;
            case RBM_STING:         p = "sting"; break;
            case RBM_BREATHE:       p = "breathe";  break;
            case RBM_BUTT:          p = "butt"; break;
            case RBM_CRUSH:         p = "crush"; break;
            case RBM_ENGULF:        p = "engulf"; break;
            case RBM_CRAWL:         p = "crawl on you"; break;
            case RBM_DROOL:         p = "drool on you"; break;
            case RBM_SPIT:          p = "spit"; break;
            case RBM_SLIME:         p = "slime"; break;
            case RBM_GAZE:          p = "gaze"; break;
            case RBM_WAIL:          p = "wail"; break;
            case RBM_SPORE:         p = "release spores"; break;
            case RBM_TRAMPLE:       p = "tramples you"; break;break;
            case RBM_BEG:           p = "beg"; break;
            case RBM_INSULT:        p = "insult"; break;
            case RBM_XXX5:          break;
            case RBM_XXX6:			break;
        }

        /* Get the effect */
        switch (effect)
        {
            case RBE_HURT:          q = "attack"; break;
            case RBE_WOUND:         q = "wound"; break;
            case RBE_BATTER:        q = "stun"; break;
            case RBE_SHATTER:       q = "shatter"; break;

            case RBE_UN_BONUS:      q = "disenchant"; break;
            case RBE_UN_POWER:      q = "drain charges"; break;
            case RBE_LOSE_MANA:     q = "drain mana"; break;
            case RBE_EAT_GOLD:      q = "steal gold"; break;
            case RBE_EAT_ITEM:      q = "steal items"; break;
            case RBE_EAT_FOOD:      q = "eat your food"; break;
            case RBE_EAT_LIGHT:      q = "absorb light"; break;
            case RBE_HUNGER:        q = "cause hunger"; break;

            case RBE_POISON:        q = "poison"; break;
            case RBE_ACID:          q = "shoot acid"; break;
            case RBE_ELEC:          q = "electrocute"; break;
            case RBE_FIRE:          q = "burn"; break;
            case RBE_COLD:          q = "freeze"; break;

            case RBE_BLIND:         q = "blind"; break;
            case RBE_CONFUSE:       q = "confuse"; break;
            case RBE_TERRIFY:       q = "terrify"; break;
            case RBE_PARALYZE:      q = "paralyze"; break;
            case RBE_HALLU:         q = "induce hallucinations"; break;
            case RBE_DISEASE:       q = "cause disease"; break;

            case RBE_LOSE_STR:      q = "reduce strength"; break;
            case RBE_LOSE_INT:      q = "reduce intelligence"; break;
            case RBE_LOSE_WIS:      q = "reduce wisdom"; break;
            case RBE_LOSE_DEX:      q = "reduce dexterity"; break;
            case RBE_LOSE_CON:      q = "reduce constitution"; break;
            case RBE_LOSE_CHR:      q = "reduce charisma"; break;
            case RBE_LOSE_ALL:      q = "reduce all stats"; break;

            case RBE_EXP_10:        q = "lower experience (by 10d6+)"; break;
            case RBE_EXP_20:        q = "lower experience (by 20d6+)"; break;
            case RBE_EXP_40:        q = "lower experience (by 40d6+)"; break;
            case RBE_EXP_80:        q = "lower experience (by 80d6+)"; break;
        }

        /* Introduce the attack description */
        if (!r)
        {
            output.append(QString("%1 can ") .arg(capitalize_first(wd_he[msex])));
        }
        else if (r < n-1)
        {
            output.append(", ");
        }
        else
        {
            output.append(", and ");
        }


        /* Hack -- force a method */
        if (p.isEmpty()) p = "do something weird";

        /* Describe the method */
        output.append(p);

        /* Describe the effect (if any) */
        if (!q.isEmpty())
        {
            /* Describe the attack type */
            output.append(" to ");
            output.append(color_string(q, TERM_RED));

            /* Describe damage (if known) */
            if (d1 && d2 && ((know_damage(r_idx, l_ptr, m)) || (l_ptr->sights == SHRT_MAX) ||
                                  (l_ptr->ranged == UCHAR_MAX)))
            {
                /* Display the damage */
                output.append(" with damage");
                output.append(QString(" %1d%2") .arg(d1) .arg(d2));
            }
        }


        /* Count the attacks as printed */
        r++;
    }

    /* Finish sentence above */
    if (r)
    {
        output.append(".  ");
    }

    /* Notice lack of attacks */
    else if (l_ptr->r_l_flags1 & RF1_NEVER_BLOW)
    {
        output.append(QString("%1 has no physical attacks.  ") .arg(capitalize_first(wd_he[msex])));
    }

    /* Or describe the lack of knowledge */
    else
    {
        output.append(QString("Nothing is known about %1 attack.  ") .arg(wd_his[msex]));
    }
    return (output);
}


static QString describe_monster_abilities(int r_idx, const monster_lore *l_ptr)
{
    const monster_race *r_ptr = &r_info[r_idx];

    int n;

    QVector<QString> vp;
    QString output;
    int msex = 0;
    vp.clear();

    output.clear();

    /* Extract a gender (if applicable) */
    if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
    else if (r_ptr->flags1 & RF1_MALE) msex = 1;

    /* Collect special abilities. */
    if (l_ptr->r_l_flags2 & RF2_HAS_LIGHT)
    {
        QString tester = "hkoOTtPp";

        /*humaniods carry torches, others glow*/
        if (!tester.contains(r_ptr->d_char)) vp.append("radiate natural light");
        else vp.append("use a light source");
    }
    if (l_ptr->r_l_flags2 & RF2_EVASIVE) vp.append("dodge attacks");
    if (l_ptr->r_l_flags2 & RF2_OPEN_DOOR) vp.append("open doors");
    if (l_ptr->r_l_flags2 & RF2_BASH_DOOR) vp.append("bash down doors");
    if (l_ptr->r_l_flags2 & RF2_PASS_WALL) vp.append("pass through walls");
    if (l_ptr->r_l_flags2 & RF2_KILL_WALL) vp.append("bore through walls");
    if (l_ptr->r_l_flags2 & RF2_KILL_BODY) vp.append("destroy weaker monsters");
    if (l_ptr->r_l_flags2 & RF2_TAKE_ITEM) vp.append("pick up objects");
    if (l_ptr->r_l_flags2 & RF2_KILL_ITEM) vp.append("destroy objects");

    /* Describe special abilities. */
    if (vp.size())
    {
        /* Intro */
        output.append(QString("%1") .arg(capitalize_first(wd_he[msex])));

        /* Scan */
        for (n = 0; n < vp.size(); n++)
        {
            /* Intro */
            if (n == 0) output.append(" can ");
            else if (n < vp.size()-1) output.append(", ");
            else output.append(" and ");

            /* Dump */
            output.append(vp[n]);
        }
        output.append(".  ");
    }

    /*note if this is an unused ghost template*/
    if ((r_ptr->flags2 & (RF2_PLAYER_GHOST)) && (r_ptr->cur_num == 0))
    {
        output.append(QString("%1 is a player ghost template.  ") .arg(capitalize_first(wd_he[msex])));
    }

    /* Describe special abilities. */
    if (l_ptr->r_l_flags2 & RF2_INVISIBLE)
    {
        output.append(QString("%1 is invisible.  ") .arg(capitalize_first(wd_he[msex])));
    }
    if (l_ptr->r_l_flags2 & RF2_COLD_BLOOD)
    {
        output.append(QString("%1 is cold blooded.  ") .arg(capitalize_first(wd_he[msex])));
    }
    if (l_ptr->r_l_flags2 & RF2_STAY_NATIVE)
    {
        output.append(QString("%1 does not leave %2 native terrain.  ") .arg(capitalize_first(wd_he[msex])) .arg(wd_his[msex]));
    }
    if (l_ptr->r_l_flags2 & RF2_EMPTY_MIND)
    {
        output.append(QString("%1 is not detected by telepathy.  ") .arg(capitalize_first(wd_he[msex])));
    }
    if (l_ptr->r_l_flags2 & RF2_WEIRD_MIND)
    {
        output.append(QString("%1 is rarely detected by telepathy.  ") .arg(capitalize_first(wd_he[msex])));
    }
    if (l_ptr->r_l_flags2 & RF2_MULTIPLY)
    {
        output.append(QString("%1 breeds explosively.  ") .arg(capitalize_first(wd_he[msex])));
    }
    if (l_ptr->r_l_flags2 & RF2_REGENERATE)
    {
        output.append(QString("%1 regenerates quickly.  ") .arg(capitalize_first(wd_he[msex])));
    }

    if (l_ptr->r_l_flags2 & (RF2_CLOUD_SURROUND))
    {
        int typ = 0, dam = 0, rad = 0;

        /* Get type of cloud */
        cloud_surround(r_idx, &typ, &dam, &rad);

        /*hack - alter type for char-attr monster*/

        if ((r_ptr->flags1 & (RF1_ATTR_MULTI)) &&
            (r_ptr->flags4 & (RF4_BRTH_FIRE)) &&
            (r_ptr->flags4 & (RF4_BRTH_POIS)) &&
            (r_ptr->flags4 & (RF4_BRTH_ACID)) &&
            (r_ptr->flags4 & (RF4_BRTH_ELEC)) &&
            (r_ptr->flags4 & (RF4_BRTH_COLD)))
            {
                output.append(QString("%1 is surrounded by an ever-changing cloud of elements.  ") .arg(capitalize_first(wd_he[msex])));
            }


        /* We emit something */
        else if (typ)
        {
            output.append(QString("%1 is surrounded by ") .arg(capitalize_first(wd_he[msex])));

            /* Describe cloud */
            if (typ == GF_SPORE)     output.append("spores");
            else if (typ == GF_DARK)      output.append("darkness");
            else if (typ == GF_DARK_WEAK) output.append("darkness");
            else                          output.append("powerful forces");
            output.append(".<br><br>");
        }
    }

    /* Collect susceptibilities */
    vp.clear();
    if (l_ptr->r_l_flags3 & RF3_HURT_POIS) vp.append("poison");
    if (l_ptr->r_l_flags3 & RF3_HURT_ACID) vp.append("acid");
    if (l_ptr->r_l_flags3 & RF3_HURT_ROCK) vp.append("rock remover");
    if (l_ptr->r_l_flags3 & RF3_HURT_LIGHT) vp.append("bright light");
    if (l_ptr->r_l_flags3 & RF3_HURT_FIRE) vp.append("fire");
    if (l_ptr->r_l_flags3 & RF3_HURT_COLD) vp.append("cold");

    /* Describe susceptibilities */
    if (vp.size())
    {
        /* Intro */
        output.append(QString("%1") .arg(capitalize_first(wd_he[msex])));

        /* Scan */
        for (n = 0; n < vp.size(); n++)
        {
            /* Intro */
            if (n == 0) output.append(" is hurt by ");
            else if (n < vp.size()-1) output.append(", ");
            else output.append(" and ");

            /* Dump */
            output.append(color_string(vp[n], TERM_COPPER));
        }

        /* End */
        output.append(".  ");
    }


    /* Collect immunities */
    vp.clear();
    if (l_ptr->r_l_flags3 & RF3_IM_ACID) vp.append("acid");
    if (l_ptr->r_l_flags3 & RF3_IM_ELEC) vp.append("lightning");
    if (l_ptr->r_l_flags3 & RF3_IM_FIRE) vp.append("fire");
    if (l_ptr->r_l_flags3 & RF3_IM_COLD) vp.append("cold");
    if (l_ptr->r_l_flags3 & RF3_IM_POIS) vp.append("poison");
    if (l_ptr->r_l_flags3 & RF3_RES_CHAOS) vp.append("chaos");
    if (l_ptr->r_l_flags3 & RF3_RES_NETHR) vp.append("nether");
    if (l_ptr->r_l_flags3 & RF3_RES_WATER) vp.append("water");
    if (l_ptr->r_l_flags3 & RF3_RES_PLAS) vp.append("plasma");
    if (l_ptr->r_l_flags3 & RF3_RES_NEXUS) vp.append("nexus");
    if (l_ptr->r_l_flags3 & RF3_RES_DISEN) vp.append("disenchantment");

    /* Describe immunities */
    if (vp.size())
    {
        /* Intro */
        output.append(QString("%1") .arg(capitalize_first(wd_he[msex])));

        /* Scan */
        for (n = 0; n < vp.size(); n++)
        {
            /* Intro */
            if (n == 0) output.append(" resists ");
            else if (n < vp.size()-1) output.append(", ");
            else output.append(" and ");

            /* Dump */
            output.append(color_string(vp[n], TERM_GREEN));
        }

        /* End */
        output.append(".  ");
    }

    /* Collect non-effects */
    vp.clear();
    if (l_ptr->r_l_flags3 & RF3_NO_SLOW) vp.append("slowed");
    if (l_ptr->r_l_flags3 & RF3_NO_STUN) vp.append("stunned");
    if (l_ptr->r_l_flags3 & RF3_NO_FEAR) vp.append("frightened");
    if (l_ptr->r_l_flags3 & RF3_NO_CONF) vp.append("confused");
    if (l_ptr->r_l_flags3 & RF3_NO_SLEEP) vp.append("slept");

    /* Describe non-effects */
    if (vp.size())
    {
        /* Intro */
        output.append(QString("%1") .arg(capitalize_first(wd_he[msex])));

        /* Scan */
        for (n = 0; n < vp.size(); n++)
        {
            /* Intro */
            if (n == 0) output.append(" is highly resistant to being ");
            else if (n < vp.size()-1) output.append(", ");
            else output.append(" or ");

            /* Dump */
            output.append(color_string(vp[n], TERM_ORANGE_PEEL));
        }
        /* End */
        output.append(".  ");
    }

    return (output);
}


static QString describe_monster_kills(int r_idx, const monster_lore *l_ptr)
{
    const monster_race *r_ptr = &r_info[r_idx];
    QString output;
    int msex = 0;

    output.clear();

    /* Extract a gender (if applicable) */
    if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
    else if (r_ptr->flags1 & RF1_MALE) msex = 1;

    /* Treat uniques differently */
    if (l_ptr->r_l_flags1 & RF1_UNIQUE)
    {
        /* Hack -- Determine if the unique is "dead" */
        bool dead = (r_ptr->max_num == 0) ? TRUE : FALSE;

        /* We've been killed... */
        if (l_ptr->deaths)
        {
            /* Killed ancestors */
            output.append(QString("%1 has slain %2 of your ancestors")
                        .arg(capitalize_first(wd_he[msex])) .arg(l_ptr->deaths));

            /* But we've also killed it */
            if (dead)
            {
                output.append(", but you have taken revenge!  ");
            }

            /* Unavenged (ever) */
            else
            {
                QString remains;
                if (l_ptr->deaths > 1) remains = "remain";
                else remains = "remains";

                output.append(QString(", who %1 unavenged.  ") .arg(remains));
            }
        }

        /* Dead unique who never hurt us */
        else if (dead)
        {
            output.append("You have slain this foe.  ");
        }
    }

    /* Not unique, but killed us */
    else if (l_ptr->deaths)
    {
        QString remains;
        if (l_ptr->deaths > 1) remains = "remain";
        else remains = "remains";

        /* Dead ancestors */
        output.append(QString("%1 of your ancestors %2 been killed by this creature, ")
                    .arg(l_ptr->deaths) .arg(remains));

        /* Some kills this life */
        if (l_ptr->pkills)
        {
            output.append(QString("and you have exterminated at least %1 of the creatures.  ") .arg(l_ptr->pkills));
        }

        /* Some kills past lives */
        else if (l_ptr->tkills)
        {
            output.append(QString("and your ancestors have exterminated at least %1 of the creatures.  ") .arg(l_ptr->tkills));
        }

        /* No kills */
        else
        {
            output.append(color_string(QString("and %1 is not ever known to have been defeated.  ") .arg(capitalize_first(wd_he[msex])), TERM_RED));
        }
    }

    /* Normal monsters */
    else
    {
        /* Killed some this life */
        if (l_ptr->pkills)
        {
            output.append(QString("You have killed at least %1 of these creatures.  ") .arg(l_ptr->pkills));
        }

        /* Killed some last life */
        else if (l_ptr->tkills)
        {
            output.append(QString("Your ancestors have killed at least %1 of these creatures.  ") .arg(l_ptr->tkills));
        }

        /* Killed none */
        else
        {
            output.append("No battles to the death are recalled.  ");
        }
    }

    return (output);
}


static QString describe_monster_toughness(int r_idx, const monster_lore *l_ptr)
{
    const monster_race *r_ptr = &r_info[r_idx];
    QString output;
    int msex = 0;
    output.clear();

    /* Extract a gender (if applicable) */
    if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
    else if (r_ptr->flags1 & RF1_MALE) msex = 1;

    /* Describe monster "toughness" */
    if ((know_armour(r_idx, l_ptr->tkills)) || (l_ptr->sights == SHRT_MAX) ||
             (l_ptr->ranged == UCHAR_MAX))
    {
        /* Armor */
        output.append(QString("%1 has an armor rating of %2") .arg(capitalize_first(wd_he[msex])) .arg(r_ptr->ac));

        /* Maximized hitpoints */
        if (l_ptr->r_l_flags1 & (RF1_FORCE_MAXHP))
        {
            output.append(QString(" and a life rating of %1.  ") .arg(r_ptr->hdice * r_ptr->hside));
        }

        /* Variable hitpoints */
        else
        {
            output.append(QString(" and a life rating of %1d%2.  ") .arg(r_ptr->hdice) .arg(r_ptr->hside));
        }
    }

    return (output);
}


static QString describe_monster_exp(int r_idx, const monster_lore *l_ptr)
{
    const monster_race *r_ptr = &r_info[r_idx];
    QString output;
    QString p, q;

    long i, j;

    output.clear();

    /* Describe experience if known */
    if (l_ptr->tkills)
    {
        /* Introduction */
        if (l_ptr->r_l_flags1 & RF1_UNIQUE)
            output.append("Killing");
        else
            output.append("A kill of");

        output.append(" this creature");

        /* calculate the integer exp part */
        i = (long)r_ptr->mexp * r_ptr->level / p_ptr->lev;

        /* calculate the fractional exp part scaled by 100, */
        /* must use long arithmetic to avoid overflow */
        j = ((((long)r_ptr->mexp * r_ptr->level % p_ptr->lev) *
              (long)1000 / p_ptr->lev + 5) / 10);

        /* Mention the experience */
        output.append(QString(" is worth %1.%2 point") .arg(number_to_formatted_string(i)) .arg(j));
        if ((i != 1) || (j != 0)) output.append("s");

        /* Take account of annoying English */
        p = "th";
        i = p_ptr->lev % 10;
        if ((p_ptr->lev / 10) == 1) /* nothing */;
        else if (i == 1) p = "st";
        else if (i == 2) p = "nd";
        else if (i == 3) p = "rd";

        /* Take account of "leading vowels" in numbers */
        q = "";
        i = p_ptr->lev;
        if ((i == 8) || (i == 11) || (i == 18)) q = "n";

        /* Mention the dependance on the player's level */
        output.append(QString(" for a%1 %2%3 level character.  ") .arg(q) .arg(i) .arg(p));
    }

    return (output);
}


static QString describe_monster_movement(int r_idx, const monster_lore *l_ptr)
{
    const monster_race *r_ptr = &r_info[r_idx];
    byte energy_gain = calc_energy_gain(r_ptr->r_speed);
    QString output;
    bool old = FALSE;
    output.clear();

    int msex = 0;

    /* Extract a gender (if applicable) */
    if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
    else if (r_ptr->flags1 & RF1_MALE) msex = 1;

    output.append("This");

    if (l_ptr->r_l_flags3 & RF3_FLYING) output.append(color_string(" flying", TERM_NAVY_BLUE));
    if (l_ptr->r_l_flags3 & RF3_ANIMAL) output.append(color_string(" natural", TERM_NAVY_BLUE));
    if (l_ptr->r_l_flags3 & RF3_EVIL) output.append(color_string(" evil", TERM_NAVY_BLUE));
    if (l_ptr->r_l_flags3 & RF3_UNDEAD) output.append(color_string(" undead", TERM_NAVY_BLUE));

    if (l_ptr->r_l_flags3 & RF3_DRAGON) output.append(color_string(" dragon", TERM_NAVY_BLUE));
    else if (l_ptr->r_l_flags3 & RF3_DEMON) output.append(color_string(" demon", TERM_NAVY_BLUE));
    else if (l_ptr->r_l_flags3 & RF3_GIANT) output.append(color_string(" giant", TERM_NAVY_BLUE));
    else if (l_ptr->r_l_flags3 & RF3_TROLL) output.append(color_string(" troll", TERM_NAVY_BLUE));
    else if (l_ptr->r_l_flags3 & RF3_ORC) output.append(color_string(" orc", TERM_NAVY_BLUE));
    else output.append(" creature");

    /* Describe location */
    if (r_ptr->level == 0)
    {
        output.append(color_string(" lives in the town", TERM_SLATE));
        old = TRUE;
    }
    else if ((l_ptr->tkills)  || (l_ptr->sights == SHRT_MAX) ||
             (l_ptr->ranged == UCHAR_MAX))
    {
        if (l_ptr->r_l_flags1 & RF1_FORCE_DEPTH) output.append(color_string(" is found", TERM_SLATE));
        else output.append(color_string(" is normally found", TERM_SLATE));

        output.append(color_string(QString(" at depths of %1 feet") .arg(r_ptr->level * 50), TERM_SLATE));

        old = TRUE;
    }

    if (old) output.append(", and");

    output.append(" moves");

    /* Random-ness */
    if ((l_ptr->r_l_flags1 & RF1_RAND_50) || (l_ptr->r_l_flags1 & RF1_RAND_25))
    {
        /* Adverb */
        if ((l_ptr->r_l_flags1 & RF1_RAND_50) && (l_ptr->r_l_flags1 & RF1_RAND_25))
        {
            output.append(" extremely");
        }
        else if (l_ptr->r_l_flags1 & RF1_RAND_50)
        {
            output.append(" somewhat");
        }
        else if (l_ptr->r_l_flags1 & RF1_RAND_25)
        {
            output.append(" a bit");
        }

        /* Adjective */
        output.append(" erratically");

        /* Hack -- Occasional conjunction */
        if (energy_gain != STANDARD_ENERGY_GAIN) output.append(", and");
    }

    /* Speed */
    if (energy_gain > STANDARD_ENERGY_GAIN)
    {

        if (energy_gain > extract_energy_nppangband[139]) output.append(color_string(" incredibly", TERM_GREEN));
        else if (energy_gain > extract_energy_nppangband[134]) output.append(color_string(" extremely", TERM_GREEN));
        else if (energy_gain > extract_energy_nppangband[129]) output.append(color_string(" very", TERM_GREEN));
        else if (energy_gain > extract_energy_nppangband[124]) output.append(color_string(" exceedingly", TERM_GREEN));
        else if (energy_gain < extract_energy_nppangband[120]) output.append(color_string(" somewhat", TERM_GREEN));
        output.append(color_string(" quickly", TERM_GREEN));

    }
    else if (energy_gain < STANDARD_ENERGY_GAIN)
    {
        if (energy_gain < extract_energy_nppangband[90]) output.append(color_string(" incredibly", TERM_GREEN));
        else if (energy_gain < extract_energy_nppangband[100]) output.append(color_string(" very", TERM_GREEN));
        output.append(color_string(" slowly", TERM_GREEN));
    }
    else
    {
        output.append(color_string(" at normal speed", TERM_GREEN));
    }

    /* The code above includes "attack speed" */
    if (l_ptr->r_l_flags1 & RF1_NEVER_MOVE)
    {
        output.append(", but does not deign to chase intruders");
    }

    output.append(".  ");

    /* Do we know how aware it is? */
    if ((((int)l_ptr->wake * (int)l_ptr->wake) > r_ptr->sleep) ||
        (l_ptr->ignore == UCHAR_MAX) ||
        ((r_ptr->sleep == 0) && (l_ptr->tkills >= 10)))
    {
        QString act;

        if (r_ptr->sleep > 200)
        {
            act = "prefers to ignore";
        }
        else if (r_ptr->sleep > 95)
        {
            act = "pays very little attention to";
        }
        else if (r_ptr->sleep > 75)
        {
            act = "pays little attention to";
        }
        else if (r_ptr->sleep > 45)
        {
            act = "tends to overlook";
        }
        else if (r_ptr->sleep > 25)
        {
            act = "takes quite a while to see";
        }
        else if (r_ptr->sleep > 10)
        {
            act = "takes a while to see";
        }
        else if (r_ptr->sleep > 5)
        {
            act = "is fairly observant of";
        }
        else if (r_ptr->sleep > 3)
        {
            act = "is observant of";
        }
        else if (r_ptr->sleep > 1)
        {
            act = "is very observant of";
        }
        else if (r_ptr->sleep > 0)
        {
            act = "is vigilant for";
        }
        else
        {
            act = "is ever vigilant for";
        }

        output.append(QString("%1 %2 intruders, which %3 may notice from %4 feet.  ")
                    .arg(capitalize_first(wd_he[msex])) .arg(act) .arg(wd_he[msex]) .arg(10 * r_ptr->aaf));
    }

    /* Describe escorts */
    if ((l_ptr->r_l_flags1 & RF1_ESCORT) || (l_ptr->r_l_flags1 & RF1_ESCORTS))
    {
        output.append(QString("%1 usually appears with escorts.  ")
                     .arg(capitalize_first(wd_he[msex])));
    }

    /* Describe friends */
    else if ((l_ptr->r_l_flags1 & RF1_FRIEND) || (l_ptr->r_l_flags1 & RF1_FRIENDS))
    {
        output.append(QString("%1 usually appears in groups.  ")
                     .arg(capitalize_first(wd_he[msex])));
    }

    /*Print out the known native terrains*/
    if (l_ptr->r_l_native)
    {
        int n;
        QVector<QString> vp;

        /* Intro */
        output.append(QString("%1 is native to ") .arg(capitalize_first(wd_he[msex])));

        if (l_ptr->r_l_native & (RN1_N_LAVA)) vp.append("lava");
        if (l_ptr->r_l_native & (RN1_N_ICE)) vp.append("ice");
        if (l_ptr->r_l_native & (RN1_N_OIL)) vp.append("oil");
        if (l_ptr->r_l_native & (RN1_N_FIRE)) vp.append("fire");
        if (l_ptr->r_l_native & (RN1_N_SAND)) vp.append("sand");
        if (l_ptr->r_l_native & (RN1_N_FOREST)) vp.append("forests");
        if (l_ptr->r_l_native & (RN1_N_WATER)) vp.append("water");
        if (l_ptr->r_l_native & (RN1_N_ACID)) vp.append("acid");
        if (l_ptr->r_l_native & (RN1_N_MUD)) vp.append("mud");

        /* Scan */
        for (n = 0; n < vp.size(); n++)
        {

            /* Dump */
            output.append(color_string(vp[n], TERM_BLUE));

            if (vp.size() == n + 1) break;
            else if (vp.size() == n + 2) output.append(" and ");
            else output.append(", ");
        }

        output.append(".  ");
    }

    return (output);
}



/*
 * Learn everything about a monster (by cheating)
 */
static void cheat_monster_lore(int r_idx, monster_lore *l_ptr)
{
    const monster_race *r_ptr = &r_info[r_idx];

    /* Know all flags */
    l_ptr->r_l_flags1 = r_ptr->flags1;
    l_ptr->r_l_flags2 = r_ptr->flags2;
    l_ptr->r_l_flags3 = r_ptr->flags3;
    l_ptr->r_l_flags4 = r_ptr->flags4;
    l_ptr->r_l_flags5 = r_ptr->flags5;
    l_ptr->r_l_flags6 = r_ptr->flags6;
    l_ptr->r_l_flags7 = r_ptr->flags7;
    l_ptr->r_l_native = r_ptr->r_native;

    /* Know max sightings, sleeping habits, spellcasting, and combat blows. */
    l_ptr->sights = SHRT_MAX;
    l_ptr->ranged = UCHAR_MAX;
    for (int i = 0; i < MONSTER_BLOW_MAX; i++)
    {
        l_ptr->blows[i] = UCHAR_MAX;
    }
    l_ptr->wake = l_ptr->ignore = UCHAR_MAX;

    /* know the treasure drops*/
    l_ptr->drop_gold = l_ptr->drop_item =
    (((r_ptr->flags1 & RF1_DROP_4D2) ? 8 : 0) +
     ((r_ptr->flags1 & RF1_DROP_3D2) ? 6 : 0) +
     ((r_ptr->flags1 & RF1_DROP_2D2) ? 4 : 0) +
     ((r_ptr->flags1 & RF1_DROP_1D2) ? 2 : 0) +
     ((r_ptr->flags1 & RF1_DROP_90)  ? 1 : 0) +
     ((r_ptr->flags1 & RF1_DROP_60)  ? 1 : 0));

    /* But only "valid" treasure drops */
    if (r_ptr->flags1 & RF1_ONLY_GOLD) l_ptr->drop_item = 0;
    if (r_ptr->flags1 & RF1_ONLY_ITEM) l_ptr->drop_gold = 0;
}


/*
 */
QString get_monster_description(int r_idx, bool spoilers, QString extra_message, bool include_header)
{
    monster_lore lore;
    QString output, last_output;
    monster_lore save_mem;
    output.clear();

    /* Get the race and lore */
    const monster_race *r_ptr = &r_info[r_idx];
    monster_lore *l_ptr = &l_list[r_idx];

    QString mon_name = monster_desc_race(r_idx);

    /* Cheat -- know everything */
    if ((cheat_know) || (r_ptr->flags2 & (RF2_PLAYER_GHOST)))
    {
        if (cheat_know) p_ptr->is_wizard = TRUE;

        /* Hack -- save memory */
        COPY(&save_mem, l_ptr, monster_lore);
    }

    /* Hack -- create a copy of the monster-memory */
    COPY(&lore, l_ptr, monster_lore);

    /* Assume some "obvious" flags */
    lore.r_l_flags1 |= (r_ptr->flags1 & RF1_OBVIOUS_MASK);

    /* Killing a monster reveals some properties */
    if (lore.tkills)
    {
        /* Know "race" flags */
        lore.r_l_flags3 |= (r_ptr->flags3 & RF3_RACE_MASK);

        /* Know "forced" flags */
        lore.r_l_flags1 |= (r_ptr->flags1 & (RF1_FORCE_DEPTH | RF1_FORCE_MAXHP));
    }

    /* Cheat -- know everything */
    if (cheat_know || spoilers || (r_ptr->flags2 & (RF2_PLAYER_GHOST)))
    {
        if (cheat_know) p_ptr->is_wizard = TRUE;

        cheat_monster_lore(r_idx, &lore);
    }

    QString mon_symbol = color_string(r_ptr->d_char, r_ptr->d_color);

    /* Print, in colour */
    if (include_header) output.append(QString("<b><h1><span style='background-color: black;'>'%1'</span> - %2</h1></b><br><br>") .arg(mon_symbol) .arg(mon_name));

    /* Show kills of monster vs. player(s) */
    if (!spoilers)last_output = describe_monster_kills(r_idx, &lore);
    output.append(last_output);
    if (last_output.length()) output.append("<br><br>");

    /* Monster description */
    last_output = describe_monster_desc(r_idx);

    /* Describe the movement and level of the monster */
    last_output.append(describe_monster_movement(r_idx, &lore));

    /* Describe experience */
    if (!spoilers) last_output.append(describe_monster_exp(r_idx, &lore));

    /* Describe monster "toughness" */
    last_output.append(describe_monster_toughness(r_idx, &lore));

    output.append(last_output);
    if (last_output.length()) output.append("<br><br>");

    /* Describe spells and innate attacks */
    last_output = describe_monster_spells(r_idx, &lore);

    /* Describe the abilities of the monster */
    output.append(describe_monster_abilities(r_idx, &lore));

    /* Describe the known attacks */
    output.append(describe_monster_attack(r_idx, &lore));

    output.append(last_output);
    if (last_output.length()) output.append("<br><br>");

    /* Describe the monster drop */
    last_output = describe_monster_drop(r_idx, &lore);

    output.append(last_output);
    if (last_output.length()) output.append("<br><br>");

    /* Notice "Quest" monsters */
    if (lore.r_l_flags1 & RF1_QUESTOR)
    {
        output.append("<big>You feel an intense desire to kill this monster...  </big><br><br>");
    }

    /* Cheat -- know everything */
    if ((cheat_know) || (r_ptr->flags2 & (RF2_PLAYER_GHOST)))
    {
        if (cheat_know) p_ptr->is_wizard = TRUE;

        /* Hack -- restore memory */
        COPY(l_ptr, &save_mem, monster_lore);
    }

    if (!extra_message.isEmpty())
    {
        output.append(QString("%1  <br><br>") .arg(extra_message));
    }

    return (output);
}


void describe_monster(int r_idx, bool spoilers, QString extra_message)
{
    QString output = get_monster_description(r_idx, spoilers, extra_message, TRUE);

    /* Finally, display it */
    display_info_window(DISPLAY_INFO_MONSTER, r_idx, output);
}
