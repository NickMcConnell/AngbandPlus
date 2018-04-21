/* File: monster1.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: describe monsters (using monster memory) */

#include "angband.h"


/*
 * Pronoun arrays, by gender.
 */
static cptr wd_he[3] =
{ "it", "he", "she" };

static cptr wd_his[3] =
{ "its", "his", "her" };



/*
 * Pluralizer. Args(count, singular, plural)
 */
#define plural(c,s,p) \
    (((c) == 1) ? (s) : (p))

/* 
Monster saving throws versus player attacks.

I changed this to competing dice rolls. Evaluating this
change is best done in a spread sheet, but here is a sample
assuming end game max player power (CL50 + max save stat):

ML MSave	Old MSave
10	6.1%	0%
20	11.7%	0%
30	17.2%	0%
40	22.8%	0%
50	28.3%	10%
60	33.9%	20%
70	39.4%	30%
80	45.0%	40%
90	50.6%	50%
100	55.5%	60%
110	59.5%	70%
120	62.9%	80%
130	65.8%	90%
140	68.2%	100%
150	70.3%	100%

The player is now slightly less overwhelming vs. weaker monsters
while end game uniques keep their nearly 60% fail (Serpent goes from
power 100 to 127 with this change so his save goes from 60% to 65%).
 */

static int _r_level(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];
    int           ml = r_ptr->level;

    if (r_ptr->flags1 & RF1_UNIQUE)
        ml += ml/5;
    
    if (r_ptr->flags2 & RF2_POWERFUL)
        ml += 7;

    if (ml < 1)
        ml = 1;

    return ml;
}

bool mon_save_p(int r_idx, int stat)
{
    int  pl = p_ptr->lev;
    int  ml = _r_level(r_idx);
    bool result = FALSE;
    
    if (stat >= 0 && stat < 6) 
        pl += adj_stat_save[p_ptr->stat_ind[stat]];

    if (pl < 1)
        pl = 1;

    if (randint1(pl) <= randint1(ml)) 
        result = TRUE;
    
    return result;
}

/*
 * Determine if the "armor" is known
 * The higher the level, the fewer kills needed.
 */
static bool know_armour(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    s32b level = r_ptr->level;

    s32b kills = r_ptr->r_tkills;

    if (cheat_know) return (TRUE);

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
static bool know_damage(int r_idx, int i)
{
    monster_race *r_ptr = &r_info[r_idx];

    s32b level = r_ptr->level;

    s32b a = r_ptr->r_blows[i];

    s32b d1 = r_ptr->blow[i].d_dice;
    s32b d2 = r_ptr->blow[i].d_side;

    s32b d = d1 * d2;

    if (d >= ((4+level)*MAX_UCHAR)/80) d = ((4+level)*MAX_UCHAR-1)/80;

    /* Normal monsters */
    if ((4 + level) * a > 80 * d) return (TRUE);

    /* Skip non-uniques */
    if (!(r_ptr->flags1 & RF1_UNIQUE)) return (FALSE);

    /* Unique monsters */
    if ((4 + level) * (2 * a) > 80 * d) return (TRUE);

    /* Assume false */
    return (FALSE);
}


/*
 * Prepare hook for c_roff(). It will be changed for spoiler generation in wizard1.c.
 */
void (*hook_c_roff)(byte attr, cptr str) = c_roff;

static void hooked_roff(cptr str)
{
    /* Spawn */
    hook_c_roff(TERM_WHITE, str);
}


/*
 * Hack -- display monster information using "hooked_roff()"
 *
 * This function should only be called with the cursor placed at the
 * left edge of the screen, on a cleared line, in which the recall is
 * to take place. One extra blank line is left after the recall.
 */
static void roff_aux(int r_idx, int mode)
{
    monster_race    *r_ptr = &r_info[r_idx];

    bool            old = FALSE;

    int             m, n, r;

    cptr            p, q;

    bool            sin = FALSE;
    int             msex = 0;

    bool nightmare = ironman_nightmare && !(mode & 0x02);
    int speed = nightmare ? r_ptr->speed + 5 : r_ptr->speed;

    bool            breath = FALSE;
    bool            magic = FALSE;

    u32b        flags1;
    u32b        flags2;
    u32b        flags3;
    u32b        flags4;
    u32b        flags5;
    u32b        flags6;
    u32b        flags7;
    u32b        flagsr;

    byte drop_gold, drop_item;

    int        vn = 0;
    byte        color[96];
    cptr        vp[96];

    bool know_everything = FALSE;

    /* Obtain a copy of the "known" number of drops */
    drop_gold = r_ptr->r_drop_gold;
    drop_item = r_ptr->r_drop_item;

    /* Obtain a copy of the "known" flags */
    flags1 = (r_ptr->flags1 & r_ptr->r_flags1);
    flags2 = (r_ptr->flags2 & r_ptr->r_flags2);
    flags3 = (r_ptr->flags3 & r_ptr->r_flags3);
    flags4 = (r_ptr->flags4 & r_ptr->r_flags4);
    flags5 = (r_ptr->flags5 & r_ptr->r_flags5);
    flags6 = (r_ptr->flags6 & r_ptr->r_flags6);
    flags7 = (r_ptr->flags7 & r_ptr->flags7);
    flagsr = (r_ptr->flagsr & r_ptr->r_flagsr);

    /* cheat_know or research_mon() */
    if (cheat_know || (mode & 0x01))
        know_everything = TRUE;

    if (r_idx == MON_MONKEY_CLONE)
        know_everything = TRUE;

    if (p_ptr->wizard)
        know_everything = TRUE;

    if (r_ptr->r_xtra1 & MR1_LORE)
        know_everything = TRUE;

    /* Cheat -- Know everything */
    if (know_everything)
    {
        /* Hack -- maximal drops */
        drop_gold = drop_item =
        (((r_ptr->flags1 & RF1_DROP_4D2) ? 8 : 0) +
         ((r_ptr->flags1 & RF1_DROP_3D2) ? 6 : 0) +
         ((r_ptr->flags1 & RF1_DROP_2D2) ? 4 : 0) +
         ((r_ptr->flags1 & RF1_DROP_1D2) ? 2 : 0) +
         ((r_ptr->flags1 & RF1_DROP_90)  ? 1 : 0) +
         ((r_ptr->flags1 & RF1_DROP_60)  ? 1 : 0));

        /* Hack -- but only "valid" drops */
        if (r_ptr->flags1 & RF1_ONLY_GOLD) drop_item = 0;
        if (r_ptr->flags1 & RF1_ONLY_ITEM) drop_gold = 0;

        /* Hack -- know all the flags */
        flags1 = r_ptr->flags1;
        flags2 = r_ptr->flags2;
        flags3 = r_ptr->flags3;
        flags4 = r_ptr->flags4;
        flags5 = r_ptr->flags5;
        flags6 = r_ptr->flags6;
        flagsr = r_ptr->flagsr;
    }

    /* Extract a gender (if applicable) */
    if (r_ptr->flags1 & RF1_FEMALE) msex = 2;
    else if (r_ptr->flags1 & RF1_MALE) msex = 1;

    /* Assume some "obvious" flags */
    if (r_ptr->flags1 & RF1_UNIQUE)  flags1 |= (RF1_UNIQUE);
    if (r_ptr->flags1 & RF1_QUESTOR) flags1 |= (RF1_QUESTOR);
    if (r_ptr->flags1 & RF1_MALE)    flags1 |= (RF1_MALE);
    if (r_ptr->flags1 & RF1_FEMALE)  flags1 |= (RF1_FEMALE);

    /* Assume some "creation" flags */
    if (r_ptr->flags1 & RF1_FRIENDS) flags1 |= (RF1_FRIENDS);
    if (r_ptr->flags1 & RF1_ESCORT)  flags1 |= (RF1_ESCORT);
    if (r_ptr->flags1 & RF1_ESCORTS) flags1 |= (RF1_ESCORTS);

    /* Killing a monster reveals some properties */
    if (r_ptr->r_tkills || know_everything)
    {
        /* Know "race" flags */
        if (r_ptr->flags3 & RF3_ORC)      flags3 |= (RF3_ORC);
        if (r_ptr->flags3 & RF3_TROLL)    flags3 |= (RF3_TROLL);
        if (r_ptr->flags3 & RF3_GIANT)    flags3 |= (RF3_GIANT);
        if (r_ptr->flags3 & RF3_DRAGON)   flags3 |= (RF3_DRAGON);
        if (r_ptr->flags3 & RF3_DEMON)    flags3 |= (RF3_DEMON);
        if (r_ptr->flags3 & RF3_UNDEAD)   flags3 |= (RF3_UNDEAD);
        if (r_ptr->flags3 & RF3_EVIL)     flags3 |= (RF3_EVIL);
        if (r_ptr->flags3 & RF3_GOOD)     flags3 |= (RF3_GOOD);
        if (r_ptr->flags3 & RF3_ANIMAL)   flags3 |= (RF3_ANIMAL);
        if (r_ptr->flags3 & RF3_AMBERITE) flags3 |= (RF3_AMBERITE);
        if (r_ptr->flags2 & RF2_HUMAN)    flags2 |= (RF2_HUMAN);

        /* Know 'quantum' flag */
        if (r_ptr->flags2 & RF2_QUANTUM)  flags2 |= (RF2_QUANTUM);

        /* Know "forced" flags */
        if (r_ptr->flags1 & RF1_FORCE_DEPTH) flags1 |= (RF1_FORCE_DEPTH);
        if (r_ptr->flags1 & RF1_FORCE_MAXHP) flags1 |= (RF1_FORCE_MAXHP);
    }

    /* For output_monster_spoiler() */
    if (mode & 0x02)
    {
        /* Nothing to do */
    }
    else

    /* Treat uniques differently */
    if (flags1 & RF1_UNIQUE)
    {
        /* Hack -- Determine if the unique is "dead" */
        bool dead = (r_ptr->max_num == 0) ? TRUE : FALSE;

        /* We've been killed... */
        if (r_ptr->r_deaths)
        {
            /* Killed ancestors */
            hooked_roff(format("%^s has slain %d of your ancestors",
                       wd_he[msex], r_ptr->r_deaths));


            /* But we've also killed it */
            if (dead)
            {
                hooked_roff(format(", but you have avenged %s!  ",
                        plural(r_ptr->r_deaths, "him", "them")));

            }

            /* Unavenged (ever) */
            else
            {
                hooked_roff(format(", who %s unavenged. ",
                        plural(r_ptr->r_deaths, "remains", "remain")));

            }

            /* Start a new line */
            hooked_roff("\n");
        }

        /* Dead unique who never hurt us */
        else if (dead)
        {
            hooked_roff("You have slain this foe. ");

            /* Start a new line */
            hooked_roff("\n");
        }
    }

    /* Not unique, but killed us */
    else if (r_ptr->r_deaths)
    {
        /* Dead ancestors */
        hooked_roff(format("%d of your ancestors %s been killed by this creature, ",
                r_ptr->r_deaths, plural(r_ptr->r_deaths, "has", "have")));


        /* Some kills this life */
        if (r_ptr->r_pkills)
        {
            hooked_roff(format("and you have exterminated at least %d of the creatures. ", r_ptr->r_pkills));

        }

        /* Some kills past lives */
        else if (r_ptr->r_tkills)
        {
            hooked_roff(format("and %s have exterminated at least %d of the creatures. ",
                    "your ancestors", r_ptr->r_tkills));

        }

        /* No kills */
        else
        {
            hooked_roff(format("and %s is not ever known to have been defeated. ", wd_he[msex]));

        }

        /* Start a new line */
        hooked_roff("\n");
    }

    /* Normal monsters */
    else
    {
        /* Killed some this life */
        if (r_ptr->r_pkills)
        {
            hooked_roff(format("You have killed at least %d of these creatures. ", r_ptr->r_pkills));

        }

        /* Killed some last life */
        else if (r_ptr->r_tkills)
        {
            hooked_roff(format("Your ancestors have killed at least %d of these creatures. ", r_ptr->r_tkills));

        }

        /* Killed none */
        else
        {
            hooked_roff("No battles to the death are recalled. ");
        }

        /* Start a new line */
        hooked_roff("\n");
    }

    /* Descriptions */
    {
        cptr tmp = r_text + r_ptr->text;

        if (tmp[0])
        {
            /* Dump it */
            hooked_roff(tmp);

            /* Start a new line */
            hooked_roff("\n");
        }
    }

    if (r_idx == MON_KAGE)
    {
        /* All done */
        hooked_roff("\n");

        return;
    }

    /* Nothing yet */
    old = FALSE;

    /* Describe location */
    if (r_ptr->level == 0)
    {
        hooked_roff(format("%^s lives in the town", wd_he[msex]));

        old = TRUE;
    }
    else if (r_ptr->r_tkills || know_everything)
    {
        if (depth_in_feet)
        {
            hooked_roff(format("%^s is normally found at depths of %d feet",

                    wd_he[msex], r_ptr->level * 50));
        }
        else
        {
            hooked_roff(format("%^s is normally found on dungeon level %d",

                    wd_he[msex], r_ptr->level));
        }
        old = TRUE;

        if (r_ptr->max_level < 999)
        {
            if (depth_in_feet)
                hooked_roff(format(", won't be found deeper than depths of %d feet", r_ptr->max_level * 50));
            else
                hooked_roff(format(", won't be found deeper than dungeon level %d", r_ptr->max_level));
        }
    }

    /* Describe movement */
    if (r_idx == MON_CHAMELEON)
    {
        hooked_roff("and can take the shape of other monster.");
        return;
    }
    else
    {
        /* Introduction */
        if (old)
        {
            hooked_roff(", and ");

        }
        else
        {
            hooked_roff(format("%^s ", wd_he[msex]));

            old = TRUE;
        }
        hooked_roff("moves");

        /* Random-ness */
        if ((flags1 & RF1_RAND_50) || (flags1 & RF1_RAND_25))
        {
            /* Adverb */
            if ((flags1 & RF1_RAND_50) && (flags1 & RF1_RAND_25))
            {
                hooked_roff(" extremely");

            }
            else if (flags1 & RF1_RAND_50)
            {
                hooked_roff(" somewhat");

            }
            else if (flags1 & RF1_RAND_25)
            {
                hooked_roff(" a bit");

            }

            /* Adjective */
            hooked_roff(" erratically");


            /* Hack -- Occasional conjunction */
            if (speed != 110) hooked_roff(", and");

        }

        /* Speed */
        if (speed > 110)
        {
            if (speed > 139) hook_c_roff(TERM_RED, " incredibly");
            else if (speed > 134) hook_c_roff(TERM_ORANGE, " extremely");
            else if (speed > 129) hook_c_roff(TERM_ORANGE, " very");
            else if (speed > 124) hook_c_roff(TERM_UMBER, " fairly");
            else if (speed < 120) hook_c_roff(TERM_L_UMBER, " somewhat");
            hook_c_roff(TERM_L_RED, " quickly");

        }
        else if (speed < 110)
        {
            if (speed < 90) hook_c_roff(TERM_L_GREEN, " incredibly");
            else if (speed < 95) hook_c_roff(TERM_BLUE, " very");
            else if (speed < 100) hook_c_roff(TERM_BLUE, " fairly");
            else if (speed > 104) hook_c_roff(TERM_GREEN, " somewhat");
            hook_c_roff(TERM_L_BLUE, " slowly");

        }
        else
        {
            hooked_roff(" at normal speed");

        }
    }

    /* The code above includes "attack speed" */
    if (flags1 & RF1_NEVER_MOVE)
    {
        /* Introduce */
        if (old)
        {
            hooked_roff(", but ");

        }
        else
        {
            hooked_roff(format("%^s ", wd_he[msex]));

            old = TRUE;
        }

        /* Describe */
        hooked_roff("does not deign to chase intruders");

    }

    /* End this sentence */
    if (old)
    {
        hooked_roff(". ");

        old = FALSE;
    }


    /* Describe experience if known */
    if (r_ptr->r_tkills || know_everything)
    {
        /* Introduction */
        if (flags1 & RF1_UNIQUE)
        {
            hooked_roff("Killing this");
        }
        else
        {
            hooked_roff("A kill of this");
        }


        /* Describe the "quality" */
        if (flags2 & RF2_ELDRITCH_HORROR) hook_c_roff(TERM_VIOLET, " sanity-blasting");

        if (flags3 & RF3_ANIMAL)          hook_c_roff(TERM_L_GREEN, " natural");

        if (flags3 & RF3_EVIL)            hook_c_roff(TERM_L_DARK, " evil");

        if (flags3 & RF3_GOOD)            hook_c_roff(TERM_YELLOW, " good");

        if (flags3 & RF3_UNDEAD)          hook_c_roff(TERM_VIOLET, " undead");
        if (flags3 & RF3_AMBERITE)        hook_c_roff(TERM_VIOLET, " Amberite");


    if ((flags3 & (RF3_DRAGON | RF3_DEMON | RF3_GIANT | RF3_TROLL | RF3_ORC)) || (flags2 & (RF2_QUANTUM | RF2_HUMAN | RF2_THIEF)))
    {
    /* Describe the "race" */
             if (flags3 & RF3_DRAGON)   hook_c_roff(TERM_ORANGE, " dragon");

        if (flags3 & RF3_DEMON)    hook_c_roff(TERM_VIOLET, " demon");

        if (flags3 & RF3_GIANT)    hook_c_roff(TERM_L_UMBER, " giant");

        if (flags3 & RF3_TROLL)    hook_c_roff(TERM_BLUE, " troll");

        if (flags3 & RF3_ORC)      hook_c_roff(TERM_UMBER, " orc");

        if (flags2 & RF2_HUMAN) hook_c_roff(TERM_L_WHITE, " human");

        if (flags2 & RF2_THIEF) hook_c_roff(TERM_L_DARK, " thief");

        if (flags2 & RF2_QUANTUM)  hook_c_roff(TERM_VIOLET, " quantum creature");

    }
        else                            hooked_roff(" creature");


        /* Group some variables */
        {
            int i, j;

            /* calculate the integer exp part */
            i = r_ptr->mexp * r_ptr->level / (p_ptr->max_plv+2);

            /* calculate the fractional exp part scaled by 100, */
            /* must use long arithmetic to avoid overflow  */
            j = (((r_ptr->mexp * r_ptr->level % (p_ptr->max_plv+2)) *
                   1000 / (p_ptr->max_plv+2) + 5) / 10);

            /* Mention the experience */
            hooked_roff(format(" is worth about %d.%02d point%s",
                    i, j,
                    (((i == 1) && (j == 0)) ? "" : "s")));

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
            hooked_roff(format(" for a%s %u%s level character. ",
                    q, i, p));
        }
    }

    if (flags2 & RF2_AURA_REVENGE)
        hook_c_roff(TERM_VIOLET, format("%^s retaliates when struck. ", wd_he[msex]));
    if (flags2 & RF2_AURA_FEAR)
        hook_c_roff(TERM_VIOLET, format("%^s inspires fear. ", wd_he[msex]));

    if ((flags2 & RF2_AURA_FIRE) && (flags2 & RF2_AURA_ELEC) && (flags3 & RF3_AURA_COLD))
    {
        hook_c_roff(TERM_VIOLET, format("%^s is surrounded by flames, ice and electricity. ", wd_he[msex]));
    }
    else if ((flags2 & RF2_AURA_FIRE) && (flags2 & RF2_AURA_ELEC))
    {
        hook_c_roff(TERM_L_RED, format("%^s is surrounded by flames and electricity. ", wd_he[msex]));
    }
    else if ((flags2 & RF2_AURA_FIRE) && (flags3 & RF3_AURA_COLD))
    {
        hook_c_roff(TERM_BLUE, format("%^s is surrounded by flames and ice. ", wd_he[msex]));
    }
    else if ((flags3 & RF3_AURA_COLD) && (flags2 & RF2_AURA_ELEC))
    {
        hook_c_roff(TERM_L_GREEN, format("%^s is surrounded by ice and electricity. ", wd_he[msex]));
    }
    else if (flags2 & RF2_AURA_FIRE)
    {
        hook_c_roff(TERM_RED, format("%^s is surrounded by flames. ", wd_he[msex]));
    }
    else if (flags3 & RF3_AURA_COLD)
    {
        hook_c_roff(TERM_BLUE, format("%^s is surrounded by ice. ", wd_he[msex]));
    }
    else if (flags2 & RF2_AURA_ELEC)
    {
        hook_c_roff(TERM_L_BLUE, format("%^s is surrounded by electricity. ", wd_he[msex]));
    }

    if (flags2 & RF2_REFLECTING)
    {
        hooked_roff(format("%^s reflects bolt spells. ", wd_he[msex]));

    }

    /* Describe escorts */
    if ((flags1 & RF1_ESCORT) || (flags1 & RF1_ESCORTS))
    {
        hooked_roff(format("%^s usually appears with escorts. ",

                wd_he[msex]));
    }

    /* Describe friends */
    else if (flags1 & RF1_FRIENDS)
    {
        hooked_roff(format("%^s usually appears in groups. ",

                wd_he[msex]));
    }


    /* Collect inate attacks */
    vn = 0;
    if (flags4 & RF4_SHRIEK)  {vp[vn] = "shriek for help";color[vn++] = TERM_L_WHITE;}

    if (flags4 & RF4_ROCKET)  {vp[vn] = "shoot a rocket";color[vn++] = TERM_UMBER;}

    if (flags4 & RF4_SHOOT) {vp[vn] = "fire an arrow";color[vn++] = TERM_UMBER;}

    if (flags6 & (RF6_SPECIAL)) {vp[vn] = "do something";color[vn++] = TERM_VIOLET;}

    /* Describe inate attacks */
    if (vn)
    {
        /* Intro */
        hooked_roff(format("%^s", wd_he[msex]));


        /* Scan */
        for (n = 0; n < vn; n++)
        {
            /* Intro */
            if (n == 0) hooked_roff(" may ");
            else if (n < vn - 1) hooked_roff(", ");
            else hooked_roff(" or ");

            /* Dump */
            hook_c_roff(color[n], vp[n]);

        }

        /* End */
        hooked_roff(". ");

    }


    /* Collect breaths */
    vn = 0;
    if (flags4 & (RF4_BR_ACID))        {vp[vn] = "acid";color[vn++] = TERM_GREEN;}

    if (flags4 & (RF4_BR_ELEC))        {vp[vn] = "lightning";color[vn++] = TERM_BLUE;}

    if (flags4 & (RF4_BR_FIRE))        {vp[vn] = "fire";color[vn++] = TERM_RED;}

    if (flags4 & (RF4_BR_COLD))        {vp[vn] = "frost";color[vn++] = TERM_L_WHITE;}

    if (flags4 & (RF4_BR_POIS))        {vp[vn] = "poison";color[vn++] = TERM_L_GREEN;}

    if (flags4 & (RF4_BR_NETH))        {vp[vn] = "nether";color[vn++] = TERM_L_DARK;}

    if (flags4 & (RF4_BR_LITE))        {vp[vn] = "light";color[vn++] = TERM_YELLOW;}

    if (flags4 & (RF4_BR_DARK))        {vp[vn] = "darkness";color[vn++] = TERM_L_DARK;}

    if (flags4 & (RF4_BR_CONF))        {vp[vn] = "confusion";color[vn++] = TERM_L_UMBER;}

    if (flags4 & (RF4_BR_SOUN))        {vp[vn] = "sound";color[vn++] = TERM_ORANGE;}

    if (flags4 & (RF4_BR_CHAO))        {vp[vn] = "chaos";color[vn++] = TERM_VIOLET;}

    if (flags4 & (RF4_BR_DISE))        {vp[vn] = "disenchantment";color[vn++] = TERM_VIOLET;}

    if (flags4 & (RF4_BR_NEXU))        {vp[vn] = "nexus";color[vn++] = TERM_VIOLET;}

    if (flags4 & (RF4_BR_TIME))        {vp[vn] = "time";color[vn++] = TERM_L_BLUE;}

    if (flags4 & (RF4_BR_INER))        {vp[vn] = "inertia";color[vn++] = TERM_SLATE;}

    if (flags4 & (RF4_BR_GRAV))        {vp[vn] = "gravity";color[vn++] = TERM_SLATE;}

    if (flags4 & (RF4_BR_SHAR))        {vp[vn] = "shards";color[vn++] = TERM_L_UMBER;}

    if (flags4 & (RF4_BR_PLAS))        {vp[vn] = "plasma";color[vn++] = TERM_L_RED;}

    if (flags4 & (RF4_BR_WALL))        {vp[vn] = "force";color[vn++] = TERM_UMBER;}

    if (flags4 & (RF4_BR_MANA))        {vp[vn] = "mana";color[vn++] = TERM_L_BLUE;}

    if (flags4 & (RF4_BR_NUKE))        {vp[vn] = "toxic waste";color[vn++] = TERM_L_GREEN;}

    if (flags4 & (RF4_BR_DISI))        {vp[vn] = "disintegration";color[vn++] = TERM_SLATE;}

    if (flags4 & (RF4_BR_STORM))       {vp[vn] = "storm";color[vn++] = TERM_BLUE;}

    /* Describe breaths */
    if (vn)
    {
        /* Note breath */
        breath = TRUE;

        /* Intro */
        hooked_roff(format("%^s", wd_he[msex]));


        /* Scan */
        for (n = 0; n < vn; n++)
        {
            /* Intro */
            if (n == 0) hooked_roff(" may breathe ");
            else if (n < vn-1) hooked_roff(", ");
            else hooked_roff(" or ");


            /* Dump */
            hook_c_roff(color[n], vp[n]);
        }
    }


    /* Collect spells */
    vn = 0;
    if (flags5 & (RF5_BA_ACID))         {vp[vn] = "produce acid balls";color[vn++] = TERM_GREEN;}

    if (flags5 & (RF5_BA_ELEC))         {vp[vn] = "produce lightning balls";color[vn++] = TERM_BLUE;}

    if (flags5 & (RF5_BA_FIRE))         {vp[vn] = "produce fire balls";color[vn++] = TERM_RED;}

    if (flags5 & (RF5_BA_COLD))         {vp[vn] = "produce frost balls";color[vn++] = TERM_L_WHITE;}

    if (flags5 & (RF5_BA_POIS))         {vp[vn] = "produce poison balls";color[vn++] = TERM_L_GREEN;}

    if (flags5 & (RF5_BA_NETH))         {vp[vn] = "produce nether balls";color[vn++] = TERM_L_DARK;}

    if (flags5 & (RF5_BA_WATE))         {vp[vn] = "produce water balls";color[vn++] = TERM_BLUE;}

    if (flags4 & (RF4_BA_NUKE))         {vp[vn] = "produce balls of radiation";color[vn++] = TERM_L_GREEN;}

    if (flags4 & RF4_THROW) {vp[vn] = "throw a large boulder";color[vn++] = TERM_SLATE;}

    if (flags5 & (RF5_BA_MANA))         {vp[vn] = "invoke mana storms";color[vn++] = TERM_L_BLUE;}

    if (flags5 & (RF5_BA_DARK))         {vp[vn] = "invoke darkness storms";color[vn++] = TERM_L_DARK;}

    if (flags5 & (RF5_BA_LITE))         {vp[vn] = "invoke starburst";color[vn++] = TERM_YELLOW;}

    if (flags4 & (RF4_BA_CHAO))         {vp[vn] = "invoke raw Logrus";color[vn++] = TERM_VIOLET;}

    if (flags6 & (RF6_HAND_DOOM))       {vp[vn] = "invoke the Hand of Doom";color[vn++] = TERM_VIOLET;}

    if (flags6 & (RF6_PSY_SPEAR))            {vp[vn] = "psycho-spear";color[vn++] = TERM_YELLOW;}

    if (flags5 & (RF5_DRAIN_MANA))      {vp[vn] = "drain mana";color[vn++] = TERM_SLATE;}

    if (flags5 & (RF5_MIND_BLAST))      {vp[vn] = "cause mind blasting";color[vn++] = TERM_L_RED;}

    if (flags5 & (RF5_BRAIN_SMASH))     {vp[vn] = "cause brain smashing";color[vn++] = TERM_RED;}

    if (flags5 & (RF5_CAUSE_1))         {vp[vn] = "cause light wounds and cursing";color[vn++] = TERM_L_WHITE;}

    if (flags5 & (RF5_CAUSE_2))         {vp[vn] = "cause serious wounds and cursing";color[vn++] = TERM_L_WHITE;}

    if (flags5 & (RF5_CAUSE_3))         {vp[vn] = "cause critical wounds and cursing";color[vn++] = TERM_L_WHITE;}

    if (flags5 & (RF5_CAUSE_4))         {vp[vn] = "cause mortal wounds";color[vn++] = TERM_L_WHITE;}

    if (flags5 & (RF5_BO_ACID))         {vp[vn] = "produce acid bolts";color[vn++] = TERM_GREEN;}

    if (flags5 & (RF5_BO_ELEC))         {vp[vn] = "produce lightning bolts";color[vn++] = TERM_BLUE;}

    if (flags5 & (RF5_BO_FIRE))         {vp[vn] = "produce fire bolts";color[vn++] = TERM_RED;}

    if (flags5 & (RF5_BO_COLD))         {vp[vn] = "produce frost bolts";color[vn++] = TERM_L_WHITE;}

    if (flags5 & (RF5_BO_NETH))         {vp[vn] = "produce nether bolts";color[vn++] = TERM_L_DARK;}

    if (flags5 & (RF5_BO_WATE))         {vp[vn] = "produce water bolts";color[vn++] = TERM_BLUE;}

    if (flags5 & (RF5_BO_MANA))         {vp[vn] = "produce mana bolts";color[vn++] = TERM_L_BLUE;}

    if (flags5 & (RF5_BO_PLAS))         {vp[vn] = "produce plasma bolts";color[vn++] = TERM_L_RED;}

    if (flags5 & (RF5_BO_ICEE))         {vp[vn] = "produce ice bolts";color[vn++] = TERM_WHITE;}

    if (flags5 & (RF5_MISSILE))         {vp[vn] = "produce magic missiles";color[vn++] = TERM_SLATE;}

    if (flags5 & (RF5_SCARE))           {vp[vn] = "terrify";color[vn++] = TERM_SLATE;}

    if (flags5 & (RF5_BLIND))           {vp[vn] = "blind";color[vn++] = TERM_L_DARK;}

    if (flags5 & (RF5_CONF))            {vp[vn] = "confuse";color[vn++] = TERM_L_UMBER;}

    if (flags5 & (RF5_SLOW))            {vp[vn] = "slow";color[vn++] = TERM_UMBER;}

    if (flags5 & (RF5_HOLD))            {vp[vn] = "paralyze";color[vn++] = TERM_RED;}

    if (flags6 & (RF6_HASTE))           {vp[vn] = "haste-self";color[vn++] = TERM_L_GREEN;}

    if (flags6 & (RF6_HEAL))            {vp[vn] = "heal-self";color[vn++] = TERM_WHITE;}

    if (flags6 & (RF6_INVULNER))        {vp[vn] = "make invulnerable";color[vn++] = TERM_WHITE;}

    if (flags4 & RF4_DISPEL)    {vp[vn] = "dispel-magic";color[vn++] = TERM_L_WHITE;}

    if (flags4 & RF4_ANTI_MAGIC) {vp[vn] = "anti-magic";color[vn++] = TERM_L_WHITE;}
    if (flags4 & RF4_ANTI_MAGIC) {vp[vn] = "polymorph";color[vn++] = TERM_L_RED;}

    if (flags6 & (RF6_BLINK))           {vp[vn] = "blink-self";color[vn++] = TERM_UMBER;}

    if (flags6 & (RF6_TPORT))           {vp[vn] = "teleport-self";color[vn++] = TERM_ORANGE;}

    if (flags6 & (RF6_WORLD))            {vp[vn] = "stop the time";color[vn++] = TERM_L_BLUE;}

    if (flags6 & (RF6_TELE_TO))         {vp[vn] = "teleport to";color[vn++] = TERM_L_UMBER;}

    if (flags6 & (RF6_TELE_AWAY))       {vp[vn] = "teleport away";color[vn++] = TERM_UMBER;}

    if (flags6 & (RF6_TELE_LEVEL))      {vp[vn] = "teleport level";color[vn++] = TERM_ORANGE;}

    if (flags6 & (RF6_DARKNESS))
    {
        if ((p_ptr->pclass != CLASS_NINJA) || (r_ptr->flags3 & (RF3_UNDEAD | RF3_HURT_LITE)) || (r_ptr->flags7 & RF7_DARK_MASK))
        {
            vp[vn] = "create darkness"; color[vn++] = TERM_L_DARK;
        }
        else
        {
            vp[vn] = "create light"; color[vn++] = TERM_YELLOW;
        }
    }

    if (flags6 & (RF6_TRAPS))           {vp[vn] = "create traps";color[vn++] = TERM_BLUE;}

    if (flags6 & (RF6_FORGET))          {vp[vn] = "cause amnesia";color[vn++] = TERM_BLUE;}

    if (flags6 & (RF6_RAISE_DEAD))      {vp[vn] = "raise dead";color[vn++] = TERM_RED;}

    if (flags6 & (RF6_S_MONSTER))       {vp[vn] = "summon a monster";color[vn++] = TERM_SLATE;}

    if (flags6 & (RF6_S_MONSTERS))      {vp[vn] = "summon monsters";color[vn++] = TERM_L_WHITE;}

    if (flags6 & (RF6_S_KIN))           {vp[vn] = "summon aid";color[vn++] = TERM_ORANGE;}

    if (flags6 & (RF6_S_ANT))           {vp[vn] = "summon ants";color[vn++] = TERM_RED;}

    if (flags6 & (RF6_S_SPIDER))        {vp[vn] = "summon spiders";color[vn++] = TERM_L_DARK;}

    if (flags6 & (RF6_S_HOUND))         {vp[vn] = "summon hounds";color[vn++] = TERM_L_UMBER;}

    if (flags6 & (RF6_S_HYDRA))         {vp[vn] = "summon hydras";color[vn++] = TERM_L_GREEN;}

    if (flags6 & (RF6_S_ANGEL))         {vp[vn] = "summon an angel";color[vn++] = TERM_YELLOW;}

    if (flags6 & (RF6_S_DEMON))         {vp[vn] = "summon a demon";color[vn++] = TERM_L_RED;}

    if (flags6 & (RF6_S_UNDEAD))        {vp[vn] = "summon an undead";color[vn++] = TERM_L_DARK;}

    if (flags6 & (RF6_S_DRAGON))        {vp[vn] = "summon a dragon";color[vn++] = TERM_ORANGE;}

    if (flags6 & (RF6_S_HI_UNDEAD))     {vp[vn] = "summon Greater Undead";color[vn++] = TERM_L_DARK;}

    if (flags6 & (RF6_S_HI_DRAGON))     {vp[vn] = "summon Ancient Dragons";color[vn++] = TERM_ORANGE;}

    if (flags6 & (RF6_S_CYBER))         {vp[vn] = "summon Cyberdemons";color[vn++] = TERM_UMBER;}

    if (flags6 & (RF6_S_AMBERITES))     {vp[vn] = "summon Lords of Amber";color[vn++] = TERM_VIOLET;}

    if (flags6 & (RF6_S_UNIQUE))        {vp[vn] = "summon Unique Monsters";color[vn++] = TERM_VIOLET;}


    /* Describe spells */
    if (vn)
    {
        /* Note magic */
        magic = TRUE;

        /* Intro */
        if (breath)
        {
            hooked_roff(", and is also");

        }
        else
        {
            hooked_roff(format("%^s is", wd_he[msex]));

        }

        /* Verb Phrase */
        hooked_roff(" magical, casting spells");

        /* Adverb */
        if (flags2 & RF2_SMART) hook_c_roff(TERM_YELLOW, " intelligently");


        /* Scan */
        for (n = 0; n < vn; n++)
        {
            /* Intro */
            if (n == 0) hooked_roff(" which ");
            else if (n < vn-1) hooked_roff(", ");
            else hooked_roff(" or ");


            /* Dump */
            hook_c_roff(color[n], vp[n]);
        }
    }


    /* End the sentence about inate/other spells */
    if (breath || magic)
    {
        /* Total casting */
        m = r_ptr->r_cast_spell;

        /* Average frequency */
        n = r_ptr->freq_spell;

        /* Describe the spell frequency */
        if (m > 100 || know_everything)
        {
            /*hooked_roff(format("; 1 time in %d", 100 / n));*/
            hooked_roff(format("; %d%% of the time", n));
        }

        /* Guess at the frequency */
        else if (m)
        {
            n = ((n + 9) / 10) * 10;
            /*hooked_roff(format("; about 1 time in %d", 100 / n));*/
            hooked_roff(format("; about %d%% of the time", n));
        }

        /* End this sentence */
        hooked_roff(". ");

    }

    /* Describe monster "toughness" */
    if (know_armour(r_idx) || know_everything)
    {
        /* Armor */
        hooked_roff(format("%^s has an armor rating of %d",

                wd_he[msex], r_ptr->ac));

        /* Maximized hitpoints */
        if ((flags1 & RF1_FORCE_MAXHP) || (r_ptr->hside == 1))
        {
            u32b hp = r_ptr->hdice * (nightmare ? 2 : 1) * r_ptr->hside;
            hooked_roff(format(" and a life rating of %d. ",
                    (s16b)MIN(30000, hp)));
        }

        /* Variable hitpoints */
        else
        {
            hooked_roff(format(" and a life rating of %dd%d. ",
                    r_ptr->hdice * (nightmare ? 2 : 1), r_ptr->hside));
        }
    }



    /* Collect special abilities. */
    vn = 0;
    if (flags7 & (RF7_HAS_LITE_1 | RF7_HAS_LITE_2)) { vp[vn] = "illuminate the dungeon"; color[vn++] = TERM_WHITE; }

    if (flags7 & (RF7_HAS_DARK_1 | RF7_HAS_DARK_2)) { vp[vn] = "darken the dungeon";     color[vn++] = TERM_L_DARK; }

    if (flags2 & RF2_OPEN_DOOR) { vp[vn] = "open doors"; color[vn++] = TERM_WHITE; }

    if (flags2 & RF2_BASH_DOOR) { vp[vn] = "bash down doors"; color[vn++] = TERM_WHITE; }

    if (flags2 & RF2_PASS_WALL) { vp[vn] = "pass through walls"; color[vn++] = TERM_WHITE; }

    if (flags2 & RF2_KILL_WALL) { vp[vn] = "bore through walls"; color[vn++] = TERM_WHITE; }

    if (flags2 & RF2_MOVE_BODY) { vp[vn] = "push past weaker monsters"; color[vn++] = TERM_WHITE; }

    if (flags2 & RF2_KILL_BODY) { vp[vn] = "destroy weaker monsters"; color[vn++] = TERM_WHITE; }

    if (flags2 & RF2_TAKE_ITEM) { vp[vn] = "pick up objects"; color[vn++] = TERM_WHITE; }

    if (flags2 & RF2_KILL_ITEM) { vp[vn] = "destroy objects"; color[vn++] = TERM_WHITE; }


    /* Describe special abilities. */
    if (vn)
    {
        /* Intro */
        hooked_roff(format("%^s", wd_he[msex]));


        /* Scan */
        for (n = 0; n < vn; n++)
        {
            /* Intro */
            if (n == 0) hooked_roff(" can ");
            else if (n < vn - 1) hooked_roff(", ");
            else hooked_roff(" and ");

            /* Dump */
            hook_c_roff(color[n], vp[n]);

        }

        /* End */
        hooked_roff(". ");

    }


    /* Describe special abilities. */
    if (flags7 & (RF7_SELF_LITE_1 | RF7_SELF_LITE_2))
    {
        hooked_roff(format("%^s is shining. ", wd_he[msex]));

    }
    if (flags7 & (RF7_SELF_DARK_1 | RF7_SELF_DARK_2))
    {
        hook_c_roff(TERM_L_DARK, format("%^s is surrounded by darkness. ", wd_he[msex]));

    }
    if (flags2 & RF2_INVISIBLE)
    {
        hooked_roff(format("%^s is invisible. ", wd_he[msex]));

    }
    if (flags2 & RF2_COLD_BLOOD)
    {
        hooked_roff(format("%^s is cold blooded. ", wd_he[msex]));

    }
    if (flags2 & RF2_EMPTY_MIND)
    {
        hooked_roff(format("%^s is not detected by telepathy. ", wd_he[msex]));

    }
    else if (flags2 & RF2_WEIRD_MIND)
    {
        hooked_roff(format("%^s is rarely detected by telepathy. ", wd_he[msex]));

    }
    if (flags2 & RF2_MULTIPLY)
    {
        hook_c_roff(TERM_L_UMBER, format("%^s breeds explosively. ", wd_he[msex]));

    }
    if (flags2 & RF2_REGENERATE)
    {
        hook_c_roff(TERM_L_WHITE, format("%^s regenerates quickly. ", wd_he[msex]));

    }
    if (flags7 & RF7_RIDING)
    {
        hook_c_roff(TERM_SLATE, format("%^s is suitable for riding. ", wd_he[msex]));

    }


    /* Collect susceptibilities */
    vn = 0;
    if (flags3 & RF3_HURT_ROCK) {vp[vn] = "rock remover";color[vn++] = TERM_UMBER;}

    if (flags3 & RF3_HURT_LITE) {vp[vn] = "bright light";color[vn++] = TERM_YELLOW;}

    if (flags3 & RF3_HURT_FIRE) {vp[vn] = "fire";color[vn++] = TERM_RED;}

    if (flags3 & RF3_HURT_COLD) {vp[vn] = "cold";color[vn++] = TERM_L_WHITE;}


    /* Describe susceptibilities */
    if (vn)
    {
        /* Intro */
        hooked_roff(format("%^s", wd_he[msex]));


        /* Scan */
        for (n = 0; n < vn; n++)
        {
            /* Intro */
            if (n == 0) hooked_roff(" is hurt by ");
            else if (n < vn-1) hooked_roff(", ");
            else hooked_roff(" and ");


            /* Dump */
            hook_c_roff(color[n], vp[n]);
        }

        /* End */
        hooked_roff(". ");

    }


    /* Collect immunities */
    vn = 0;
    if (flagsr & RFR_IM_ACID) {vp[vn] = "acid";color[vn++] = TERM_GREEN;}

    if (flagsr & RFR_IM_ELEC) {vp[vn] = "lightning";color[vn++] = TERM_BLUE;}

    if (flagsr & RFR_IM_FIRE) {vp[vn] = "fire";color[vn++] = TERM_RED;}

    if (flagsr & RFR_IM_COLD) {vp[vn] = "cold";color[vn++] = TERM_L_WHITE;}

    if (flagsr & RFR_IM_POIS) {vp[vn] = "poison";color[vn++] = TERM_L_GREEN;}

    /* Describe resistances */
    if (vn)
    {
        /* Intro */
        hooked_roff(format("%^s", wd_he[msex]));


        /* Scan */
        for (n = 0; n < vn; n++)
        {
            /* Intro */
            if (n == 0) hooked_roff(" is immune to ");
            else if (n < vn-1) hooked_roff(", ");
            else hooked_roff(" and ");


            /* Dump */
            hook_c_roff(color[n], vp[n]);
        }

        /* End */
        hooked_roff(". ");

    }



    /* Collect resistances */
    vn = 0;
    if (flagsr & RFR_RES_ACID) {vp[vn] = "acid";color[vn++] = TERM_GREEN;}

    if (flagsr & RFR_RES_ELEC) {vp[vn] = "lightning";color[vn++] = TERM_BLUE;}

    if (flagsr & RFR_RES_FIRE) {vp[vn] = "fire";color[vn++] = TERM_RED;}

    if (flagsr & RFR_RES_COLD) {vp[vn] = "cold";color[vn++] = TERM_L_WHITE;}

    if (flagsr & RFR_RES_POIS) {vp[vn] = "poison";color[vn++] = TERM_L_GREEN;}

    if (flagsr & RFR_RES_LITE) {vp[vn] = "light";color[vn++] = TERM_YELLOW;}

    if (flagsr & RFR_RES_DARK) {vp[vn] = "dark";color[vn++] = TERM_L_DARK;}

    if (flagsr & RFR_RES_NETH) {vp[vn] = "nether";color[vn++] = TERM_L_DARK;}

    if (flagsr & RFR_RES_WATE) {vp[vn] = "water";color[vn++] = TERM_BLUE;}

    if (flagsr & RFR_RES_PLAS) {vp[vn] = "plasma";color[vn++] = TERM_L_RED;}

    if (flagsr & RFR_RES_SHAR) {vp[vn] = "shards";color[vn++] = TERM_L_UMBER;}

    if (flagsr & RFR_RES_SOUN) {vp[vn] = "sound";color[vn++] = TERM_ORANGE;}

    if (flagsr & RFR_RES_CHAO) {vp[vn] = "chaos";color[vn++] = TERM_VIOLET;}

    if (flagsr & RFR_RES_NEXU) {vp[vn] = "nexus";color[vn++] = TERM_VIOLET;}

    if (flagsr & RFR_RES_DISE) {vp[vn] = "disenchantment";color[vn++] = TERM_VIOLET;}

    if (flagsr & RFR_RES_WALL) {vp[vn] = "force";color[vn++] = TERM_UMBER;}

    if (flagsr & RFR_RES_INER) {vp[vn] = "inertia";color[vn++] = TERM_SLATE;}

    if (flagsr & RFR_RES_TIME) {vp[vn] = "time";color[vn++] = TERM_L_BLUE;}

    if (flagsr & RFR_RES_GRAV) {vp[vn] = "gravity";color[vn++] = TERM_SLATE;}

    if (flagsr & RFR_RES_ALL) {vp[vn] = "all";color[vn++] = TERM_YELLOW;}

    if ((flagsr & RFR_RES_TELE) && !(r_ptr->flags1 & RF1_UNIQUE)) {vp[vn] = "teleportation";color[vn++] = TERM_ORANGE;}


    /* Describe resistances */
    if (vn)
    {
        /* Intro */
        hooked_roff(format("%^s", wd_he[msex]));


        /* Scan */
        for (n = 0; n < vn; n++)
        {
            /* Intro */
            if (n == 0) hooked_roff(" resists ");
            else if (n < vn-1) hooked_roff(", ");
            else hooked_roff(" and ");


            /* Dump */
            hook_c_roff(color[n], vp[n]);
        }

        /* End */
        hooked_roff(". ");

    }


    if ((r_ptr->r_xtra1 & MR1_SINKA) || know_everything)
    {
        if (r_ptr->next_r_idx)
        {
            hooked_roff(format("%^s will evolve into ", wd_he[msex]));
            hook_c_roff(TERM_YELLOW, format("%s", r_name+r_info[r_ptr->next_r_idx].name));
            hooked_roff(format(" when %s gets enough experience. ", wd_he[msex]));
        }
        else if (!(r_ptr->flags1 & RF1_UNIQUE))
        {
            hooked_roff(format("%^s won't evolve. ", wd_he[msex]));
        }
    }

    /* Collect non-effects */
    vn = 0;
    if (flags3 & RF3_NO_STUN)  {vp[vn] = "stunned";color[vn++] = TERM_ORANGE;}

    if (flags3 & RF3_NO_FEAR)  {vp[vn] = "frightened";color[vn++] = TERM_SLATE;}

    if (flags3 & RF3_NO_CONF)  {vp[vn] = "confused";color[vn++] = TERM_L_UMBER;}

    if (flags3 & RF3_NO_SLEEP) {vp[vn] = "slept";color[vn++] = TERM_BLUE;}

    if ((flagsr & RFR_RES_TELE) && (r_ptr->flags1 & RF1_UNIQUE)) {vp[vn] = "teleported";color[vn++] = TERM_ORANGE;}

    /* Describe non-effects */
    if (vn)
    {
        /* Intro */
        hooked_roff(format("%^s", wd_he[msex]));


        /* Scan */
        for (n = 0; n < vn; n++)
        {
            /* Intro */
            if (n == 0) hooked_roff(" cannot be ");
            else if (n < vn - 1) hooked_roff(", ");
            else hooked_roff(" or ");


            /* Dump */
            hook_c_roff(color[n], vp[n]);
        }

        /* End */
        hooked_roff(". ");

    }


    /* Do we know how aware it is? */
    if ((((int)r_ptr->r_wake * (int)r_ptr->r_wake) > r_ptr->sleep) ||
          (r_ptr->r_ignore == MAX_UCHAR) ||
        (r_ptr->sleep == 0 && r_ptr->r_tkills >= 10) || know_everything)
    {
        cptr act;

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

        hooked_roff(format("%^s %s intruders, which %s may notice from %d feet. ",
                wd_he[msex], act, wd_he[msex], 10 * r_ptr->aaf));

    }


    /* Drops gold and/or items */
    if (drop_gold || drop_item)
    {
        /* Intro */
        hooked_roff(format("%^s may carry", wd_he[msex]));

        /* No "n" needed */
        sin = FALSE;


        /* Count maximum drop */
        n = MAX(drop_gold, drop_item);

        /* One drop (may need an "n") */
        if (n == 1)
        {
            hooked_roff(" a");
            sin = TRUE;
        }

        /* Two drops */
        else if (n == 2)
        {
            hooked_roff(" one or two");

        }

        /* Many drops */
        else
        {
            hooked_roff(format(" up to %d", n));

        }


        /* Great */
        if (flags1 & RF1_DROP_GREAT)
        {
            p = " exceptional";

        }

        /* Good (no "n" needed) */
        else if (flags1 & RF1_DROP_GOOD)
        {
            p = " good";
            sin = FALSE;
        }

        /* Okay */
        else
        {
            p = NULL;
        }


        /* Objects */
        if (drop_item)
        {
            /* Handle singular "an" */
            if (sin) hooked_roff("n");
            sin = FALSE;

            /* Dump "object(s)" */
            if (p) hooked_roff(p);
            hooked_roff(" object");
            if (n != 1) hooked_roff("s");


            /* Conjunction replaces variety, if needed for "gold" below */
            p = " or";

        }

        /* Treasures */
        if (drop_gold)
        {
            /* Cancel prefix */
            if (!p) sin = FALSE;

            /* Handle singular "an" */
            if (sin) hooked_roff("n");
            sin = FALSE;

            /* Dump "treasure(s)" */
            if (p) hooked_roff(p);
            hooked_roff(" treasure");
            if (n != 1) hooked_roff("s");

        }

        /* End this sentence */
        hooked_roff(". ");

    }


    /* Count the number of "known" attacks */
    for (n = 0, m = 0; m < 4; m++)
    {
        /* Skip non-attacks */
        if (!r_ptr->blow[m].method) continue;
        if (r_ptr->blow[m].method == RBM_SHOOT) continue;

        /* Count known attacks */
        if (r_ptr->r_blows[m] || know_everything) n++;
    }

    /* Examine (and count) the actual attacks */
    for (r = 0, m = 0; m < 4; m++)
    {
        int method, effect, d1, d2;

        /* Skip non-attacks */
        if (!r_ptr->blow[m].method) continue;
        if (r_ptr->blow[m].method == RBM_SHOOT) continue;

        /* Skip unknown attacks */
        if (!r_ptr->r_blows[m] && !know_everything) continue;

        /* Extract the attack info */
        method = r_ptr->blow[m].method;
        effect = r_ptr->blow[m].effect;
        d1 = r_ptr->blow[m].d_dice;
        d2 = r_ptr->blow[m].d_side;

        /* No method yet */
        p = NULL;

        /* Acquire the method */
        switch (method)
        {
            case RBM_HIT:        p = "hit"; break;

            case RBM_TOUCH:        p = "touch"; break;

            case RBM_PUNCH:        p = "punch"; break;

            case RBM_KICK:        p = "kick"; break;

            case RBM_CLAW:        p = "claw"; break;

            case RBM_BITE:        p = "bite"; break;

            case RBM_STING:        p = "sting"; break;

            case RBM_SLASH:        p = "slash"; break;

            case RBM_BUTT:        p = "butt"; break;

            case RBM_CRUSH:        p = "crush"; break;

            case RBM_ENGULF:    p = "engulf"; break;

            case RBM_CHARGE:     p = "charge";   break;

            case RBM_CRAWL:        p = "crawl on you"; break;

            case RBM_DROOL:        p = "drool on you"; break;

            case RBM_SPIT:        p = "spit"; break;

            case RBM_EXPLODE:    p = "explode"; break;

            case RBM_GAZE:        p = "gaze"; break;

            case RBM_WAIL:        p = "wail"; break;

            case RBM_SPORE:        p = "release spores"; break;

            case RBM_XXX4:        break;
            case RBM_BEG:        p = "beg"; break;

            case RBM_INSULT:    p = "insult"; break;

            case RBM_MOAN:        p = "moan"; break;

            case RBM_SHOW:      p = "sing"; break;

        }


        /* Default effect */
        q = NULL;

        /* Acquire the effect */
        switch (effect)
        {
            case RBE_SUPERHURT:
            case RBE_HURT:        q = "attack"; break;

            case RBE_POISON:      q = "poison"; break;

            case RBE_UN_BONUS:    q = "disenchant"; break;

            case RBE_UN_POWER:    q = "drain charges"; break;

            case RBE_EAT_GOLD:    q = "steal gold"; break;

            case RBE_EAT_ITEM:    q = "steal items"; break;

            case RBE_EAT_FOOD:    q = "eat your food"; break;

            case RBE_EAT_LITE:    q = "absorb light"; break;

            case RBE_ACID:        q = "shoot acid"; break;

            case RBE_ELEC:        q = "electrocute"; break;

            case RBE_FIRE:        q = "burn"; break;

            case RBE_COLD:        q = "freeze"; break;

            case RBE_BLIND:       q = "blind"; break;

            case RBE_CONFUSE:     q = "confuse"; break;

            case RBE_TERRIFY:     q = "terrify"; break;

            case RBE_PARALYZE:    q = "paralyze"; break;

            case RBE_LOSE_STR:    q = "reduce strength"; break;

            case RBE_LOSE_INT:    q = "reduce intelligence"; break;

            case RBE_LOSE_WIS:    q = "reduce wisdom"; break;

            case RBE_LOSE_DEX:    q = "reduce dexterity"; break;

            case RBE_LOSE_CON:    q = "reduce constitution"; break;

            case RBE_LOSE_CHR:    q = "reduce charisma"; break;

            case RBE_LOSE_ALL:    q = "reduce all stats"; break;

            case RBE_SHATTER:    q = "shatter"; break;

            case RBE_EXP_10:    q = "lower experience (by 10d6+)"; break;

            case RBE_EXP_20:    q = "lower experience (by 20d6+)"; break;

            case RBE_EXP_40:    q = "lower experience (by 40d6+)"; break;

            case RBE_EXP_80:    q = "lower experience (by 80d6+)"; break;

            case RBE_DISEASE:    q = "disease"; break;

            case RBE_TIME:      q = "time"; break;

            case RBE_EXP_VAMP:  q = "drain life force"; break;

            case RBE_DR_MANA:  q = "drain mana force"; break;

        }


        /* Introduce the attack description */
        if (!r)
        {
            hooked_roff(format("%^s can ", wd_he[msex]));
        }
        else if (r < n-1)
        {
            hooked_roff(", ");
        }
        else
        {
            hooked_roff(", and ");
        }


        /* Hack -- force a method */
        if (!p) p = "do something weird";

        /* Describe the method */
        hooked_roff(p);


        /* Describe the effect (if any) */
        if (q)
        {
            /* Describe the attack type */
            hooked_roff(" to ");
            hooked_roff(q);

            /* Describe damage (if known) */
            if (d1 && d2 && (know_everything || know_damage(r_idx, m)))
            {
                /* Display the damage */
                hooked_roff(" with damage");
                hooked_roff(format(" %dd%d", d1, d2));
            }
        }



        /* Count the attacks as printed */
        r++;
    }

    /* Finish sentence above */
    if (r)
    {
        hooked_roff(". ");

    }

    /* Notice lack of attacks */
    else if (flags1 & RF1_NEVER_BLOW)
    {
        hooked_roff(format("%^s has no physical attacks. ", wd_he[msex]));

    }

    /* Or describe the lack of knowledge */
    else
    {
        hooked_roff(format("Nothing is known about %s attack. ", wd_his[msex]));

    }


    /*
     * Notice "Quest" monsters, but only if you
     * already encountered the monster.
     */
    if ((flags1 & RF1_QUESTOR) && ((r_ptr->r_sights) && (r_ptr->max_num) && ((r_idx == MON_OBERON) || (r_idx == MON_SERPENT))))
    {
        hook_c_roff(TERM_VIOLET, "You feel an intense desire to kill this monster... ");

    }

    else if ((flags7 & RF7_GUARDIAN) && !no_wilderness)
    {
        hook_c_roff(TERM_L_RED, "This monster is the master of a dungeon.");

    }


    /* All done */
    hooked_roff("\n");

}



/*
 * Hack -- Display the "name" and "attr/chars" of a monster race
 */
void roff_top(int r_idx)
{
    monster_race    *r_ptr = &r_info[r_idx];

    byte        a1, a2;
    char        c1, c2;


    /* Access the chars */
    c1 = r_ptr->d_char;
    c2 = r_ptr->x_char;

    /* Access the attrs */
    a1 = r_ptr->d_attr;
    a2 = r_ptr->x_attr;


    /* Clear the top line */
    Term_erase(0, 0, 255);

    /* Reset the cursor */
    Term_gotoxy(0, 0);

    /* A title (use "The" for non-uniques) */
    if (!(r_ptr->flags1 & RF1_UNIQUE))
    {
        Term_addstr(-1, TERM_WHITE, "The ");
    }

    /* Dump the name */
    Term_addstr(-1, TERM_WHITE, (r_name + r_ptr->name));

    /* Append the "standard" attr/char info */
    Term_addstr(-1, TERM_WHITE, " ('");
    Term_add_bigch(a1, c1);
    Term_addstr(-1, TERM_WHITE, "')");

    /* Append the "optional" attr/char info */
    Term_addstr(-1, TERM_WHITE, "/('");
    Term_add_bigch(a2, c2);
    Term_addstr(-1, TERM_WHITE, "'):");

    /* Wizards get extra info */
    if (p_ptr->wizard)
    {
        char buf[6];

        sprintf(buf, "%d", r_idx);

        Term_addstr(-1, TERM_WHITE, " (");
        Term_addstr(-1, TERM_L_BLUE, buf);
        Term_addch(TERM_WHITE, ')');
    }
}



/*
 * Hack -- describe the given monster race at the top of the screen
 */
void screen_roff(int r_idx, int mode)
{
    /* Flush messages */
    msg_print(NULL);

    /* Begin recall */
    Term_erase(0, 1, 255);

    hook_c_roff = c_roff;

    /* Recall monster */
    roff_aux(r_idx, mode);

    /* Describe monster */
    roff_top(r_idx);
}




/*
 * Hack -- describe the given monster race in the current "term" window
 */
void display_roff(int r_idx)
{
    int y;

    /* Erase the window */
    for (y = 0; y < Term->hgt; y++)
    {
        /* Erase the line */
        Term_erase(0, y, 255);
    }

    /* Begin recall */
    Term_gotoxy(0, 1);

    hook_c_roff = c_roff;

    /* Recall monster */
    roff_aux(r_idx, 0);

    /* Describe monster */
    roff_top(r_idx);
}



/*
 * Hack -- output description of the given monster race
 */
void output_monster_spoiler(int r_idx, void (*roff_func)(byte attr, cptr str))
{
    hook_c_roff = roff_func;

    /* Recall monster */
    roff_aux(r_idx, 0x03);
}


bool mon_hook_dungeon(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (!(r_ptr->flags8 & RF8_WILD_ONLY))
        return TRUE;
    else
    {
        dungeon_info_type *d_ptr = &d_info[dungeon_type];
        if ((d_ptr->mflags8 & RF8_WILD_MOUNTAIN) &&
            (r_ptr->flags8 & RF8_WILD_MOUNTAIN)) return TRUE;
        return FALSE;
    }
}

static bool _mon_hook_wild_daytime_check(int r_idx)
{
    bool result = TRUE;
    if (is_daytime())
    {
        monster_race *r_ptr = &r_info[r_idx];
        if (r_ptr->flags3 & RF3_HURT_LITE)
            return FALSE;
    }
    return result;
}

static bool mon_hook_ocean(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (r_ptr->flags8 & RF8_WILD_OCEAN)
        return _mon_hook_wild_daytime_check(r_idx);
    else
        return FALSE;
}


static bool mon_hook_shore(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (r_ptr->flags8 & RF8_WILD_SHORE)
        return _mon_hook_wild_daytime_check(r_idx);
    else
        return FALSE;
}


static bool mon_hook_waste(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (r_ptr->flags8 & (RF8_WILD_WASTE | RF8_WILD_ALL))
        return _mon_hook_wild_daytime_check(r_idx);
    else
        return FALSE;
}


static bool mon_hook_town(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (r_ptr->flags8 & (RF8_WILD_TOWN | RF8_WILD_ALL))
        return _mon_hook_wild_daytime_check(r_idx);
    else
        return FALSE;
}


static bool mon_hook_wood(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (r_ptr->flags8 & (RF8_WILD_WOOD | RF8_WILD_ALL))
        return _mon_hook_wild_daytime_check(r_idx);
    else
        return FALSE;
}


static bool mon_hook_volcano(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (r_ptr->flags8 & RF8_WILD_VOLCANO)
        return _mon_hook_wild_daytime_check(r_idx);
    else
        return FALSE;
}


static bool mon_hook_mountain(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (r_ptr->flags8 & RF8_WILD_MOUNTAIN)
        return _mon_hook_wild_daytime_check(r_idx);
    else
        return FALSE;
}


static bool mon_hook_grass(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (r_ptr->flags8 & (RF8_WILD_GRASS | RF8_WILD_ALL))
        return _mon_hook_wild_daytime_check(r_idx);
    else
        return FALSE;
}


static bool mon_hook_deep_water(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (!mon_hook_dungeon(r_idx)) return FALSE;

    if (r_ptr->flags7 & RF7_AQUATIC)
        return _mon_hook_wild_daytime_check(r_idx);
    else
        return FALSE;
}


static bool mon_hook_shallow_water(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (!mon_hook_dungeon(r_idx)) return FALSE;

    if (r_ptr->flags2 & RF2_AURA_FIRE)
        return FALSE;
    else
        return _mon_hook_wild_daytime_check(r_idx);
}


static bool mon_hook_lava(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (!mon_hook_dungeon(r_idx)) return FALSE;

    if (((r_ptr->flagsr & RFR_EFF_IM_FIRE_MASK) ||
         (r_ptr->flags7 & RF7_CAN_FLY)) &&
        !(r_ptr->flags3 & RF3_AURA_COLD))
        return _mon_hook_wild_daytime_check(r_idx);
    else
        return FALSE;
}


static bool mon_hook_floor(int r_idx)
{
    monster_race *r_ptr = &r_info[r_idx];

    if (!(r_ptr->flags7 & RF7_AQUATIC) ||
        (r_ptr->flags7 & RF7_CAN_FLY))
        return TRUE;
    else
        return FALSE;
}

monster_hook_type get_wilderness_monster_hook(int x, int y)
{
    if (wilderness[y][x].town)
        return mon_hook_town;

    switch (wilderness[y][x].terrain)
    {
    case TERRAIN_TOWN: /* Probably no longer used ... ? */
        return mon_hook_town;
    case TERRAIN_DEEP_WATER:
        return mon_hook_ocean;
    case TERRAIN_SHALLOW_WATER:
    case TERRAIN_SWAMP:
        return mon_hook_shore;
    case TERRAIN_DIRT:
    case TERRAIN_DESERT:
        return mon_hook_waste;
    case TERRAIN_GRASS:
        return mon_hook_grass;
    case TERRAIN_TREES:
        return mon_hook_wood;
    case TERRAIN_SHALLOW_LAVA:
    case TERRAIN_DEEP_LAVA:
        return mon_hook_volcano;
    case TERRAIN_MOUNTAIN:
        return mon_hook_mountain;
    default:
        return mon_hook_dungeon;
    }
}

monster_hook_type get_monster_hook(void)
{
    if (!dun_level && !p_ptr->inside_quest)
        return get_wilderness_monster_hook(p_ptr->wilderness_x, p_ptr->wilderness_y);
    else
        return (monster_hook_type)mon_hook_dungeon;
}


monster_hook_type get_monster_hook2(int y, int x)
{
    feature_type *f_ptr = &f_info[cave[y][x].feat];

    /* Set the monster list */

    /* Water */
    if (have_flag(f_ptr->flags, FF_WATER))
    {
        /* Deep water */
        if (have_flag(f_ptr->flags, FF_DEEP))
        {
            return (monster_hook_type)mon_hook_deep_water;
        }

        /* Shallow water */
        else
        {
            return (monster_hook_type)mon_hook_shallow_water;
        }
    }

    /* Lava */
    else if (have_flag(f_ptr->flags, FF_LAVA))
    {
        return (monster_hook_type)mon_hook_lava;
    }

    else return (monster_hook_type)mon_hook_floor;
}


void set_friendly(monster_type *m_ptr)
{
    m_ptr->smart |= SM_FRIENDLY;
}

void set_pet(monster_type *m_ptr)
{
    if (!is_pet(m_ptr)) check_pets_num_and_align(m_ptr, TRUE);

    /* Check for quest completion */
    check_quest_completion(m_ptr);

    m_ptr->smart |= SM_PET;
    if (!(r_info[m_ptr->r_idx].flags3 & (RF3_EVIL | RF3_GOOD)))
        m_ptr->sub_align = SUB_ALIGN_NEUTRAL;
}

/*
 * Makes the monster hostile towards the player
 */
void set_hostile(monster_type *m_ptr)
{
    if (p_ptr->inside_battle) return;

    if (is_pet(m_ptr)) check_pets_num_and_align(m_ptr, FALSE);

    m_ptr->smart &= ~SM_PET;
    m_ptr->smart &= ~SM_FRIENDLY;
}


/*
 * Anger the monster
 */
void anger_monster(monster_type *m_ptr)
{
    if (p_ptr->inside_battle) return;
    if (is_friendly(m_ptr))
    {
        char m_name[80];

        monster_desc(m_name, m_ptr, 0);
        msg_format("%^s gets angry!", m_name);

        set_hostile(m_ptr);

        virtue_add(VIRTUE_INDIVIDUALISM, 1);
        virtue_add(VIRTUE_HONOUR, -1);
        virtue_add(VIRTUE_JUSTICE, -1);
        virtue_add(VIRTUE_COMPASSION, -1);
    }
}


/*
 * Check if monster can cross terrain
 */
bool monster_can_cross_terrain(s16b feat, monster_race *r_ptr, u16b mode)
{
    feature_type *f_ptr = &f_info[feat];
    bool          ring_lev = FALSE;

    if ((mode & CEM_RIDING) && p_ptr->prace == RACE_MON_RING && p_ptr->levitation)
        ring_lev = TRUE;

    /* Pattern */
    if (have_flag(f_ptr->flags, FF_PATTERN))
    {
        if (!(mode & CEM_RIDING))
        {
            if (!(r_ptr->flags7 & RF7_CAN_FLY)) return FALSE;
        }
        else
        {
            if (!(mode & CEM_P_CAN_ENTER_PATTERN)) return FALSE;
        }
    }

    /* "CAN" flags */
    if (have_flag(f_ptr->flags, FF_CAN_FLY) && ((r_ptr->flags7 & RF7_CAN_FLY) || ring_lev)) return TRUE;
    if (have_flag(f_ptr->flags, FF_CAN_SWIM) && ((r_ptr->flags7 & RF7_CAN_SWIM) || ring_lev)) return TRUE;
    if (have_flag(f_ptr->flags, FF_CAN_PASS))
    {
        if ((r_ptr->flags2 & RF2_PASS_WALL) && (!(mode & CEM_RIDING) || p_ptr->pass_wall)) return TRUE;
    }

    if (!have_flag(f_ptr->flags, FF_MOVE)) return FALSE;

    /* Some monsters can walk on mountains */
    if (have_flag(f_ptr->flags, FF_MOUNTAIN) && (r_ptr->flags8 & RF8_WILD_MOUNTAIN)) return TRUE;

    /* Water */
    if (have_flag(f_ptr->flags, FF_WATER))
    {
        if (!(r_ptr->flags7 & RF7_AQUATIC))
        {
            /* Deep water */
            if (have_flag(f_ptr->flags, FF_DEEP)) return FALSE;

            /* Shallow water */
            else if (r_ptr->flags2 & RF2_AURA_FIRE) return FALSE;
        }
    }

    /* Aquatic monster into non-water? */
    else if (r_ptr->flags7 & RF7_AQUATIC) return FALSE;

    /* Lava */
    if (have_flag(f_ptr->flags, FF_LAVA))
    {
        if (!(r_ptr->flagsr & RFR_EFF_IM_FIRE_MASK)) return FALSE;
    }

    return TRUE;
}


/*
 * Strictly check if monster can enter the grid
 */
bool monster_can_enter(int y, int x, monster_race *r_ptr, u16b mode)
{
    cave_type *c_ptr = &cave[y][x];

    /* Player or other monster */
    if (player_bold(y, x)) return FALSE;
    if (c_ptr->m_idx) return FALSE;

    return monster_can_cross_terrain(c_ptr->feat, r_ptr, mode);
}


/*
 * Check if this monster has "hostile" alignment (aux)
 */
static bool check_hostile_align(byte sub_align1, byte sub_align2)
{
    if (sub_align1 != sub_align2)
    {
        if (((sub_align1 & SUB_ALIGN_EVIL) && (sub_align2 & SUB_ALIGN_GOOD)) ||
            ((sub_align1 & SUB_ALIGN_GOOD) && (sub_align2 & SUB_ALIGN_EVIL)))
            return TRUE;
    }

    /* Non-hostile alignment */
    return FALSE;
}


/*
 * Check if two monsters are enemies
 */
bool are_enemies(monster_type *m_ptr, monster_type *n_ptr)
{
    monster_race *r_ptr = &r_info[m_ptr->r_idx];
    monster_race *s_ptr = &r_info[n_ptr->r_idx];

    if (p_ptr->inside_battle)
    {
        if (is_pet(m_ptr) || is_pet(n_ptr)) return FALSE;
        return TRUE;
    }

    if ((r_ptr->flags8 & (RF8_WILD_TOWN | RF8_WILD_ALL))
        && (s_ptr->flags8 & (RF8_WILD_TOWN | RF8_WILD_ALL)))
    {
        if (!is_pet(m_ptr) && !is_pet(n_ptr)) return FALSE;
    }

    /* Friendly vs. opposite aligned normal or pet */
    if (check_hostile_align(m_ptr->sub_align, n_ptr->sub_align))
    {
        /* No monster fighting (option) except involving pets */
        if (!allow_hostile_monster && !is_pet(m_ptr) && !is_pet(n_ptr)) return FALSE;

        if (!(m_ptr->mflag2 & MFLAG2_CHAMELEON) || !(n_ptr->mflag2 & MFLAG2_CHAMELEON)) return TRUE;
    }

    /* Hostile vs. non-hostile */
    if (is_hostile(m_ptr) != is_hostile(n_ptr))
    {
        return TRUE;
    }

    /* Default */
    return FALSE;
}


/*
 * Check if this monster race has "hostile" alignment
 * If user is player, m_ptr == NULL.
 */
bool monster_has_hostile_align(monster_type *m_ptr, int pa_good, int pa_evil, monster_race *r_ptr)
{
    byte sub_align1 = SUB_ALIGN_NEUTRAL;
    byte sub_align2 = SUB_ALIGN_NEUTRAL;

    if (m_ptr) /* For a monster */
    {
        sub_align1 = m_ptr->sub_align;
    }
    else /* For player */
    {
        if (p_ptr->align >= pa_good) sub_align1 |= SUB_ALIGN_GOOD;
        if (p_ptr->align <= pa_evil) sub_align1 |= SUB_ALIGN_EVIL;
    }

    /* Racial alignment flags */
    if (r_ptr->flags3 & RF3_EVIL) sub_align2 |= SUB_ALIGN_EVIL;
    if (r_ptr->flags3 & RF3_GOOD) sub_align2 |= SUB_ALIGN_GOOD;

    if (check_hostile_align(sub_align1, sub_align2)) return TRUE;

    /* Non-hostile alignment */
    return FALSE;
}


/*
 * Is the monster "alive"?
 *
 * Used to determine the message to print for a killed monster.
 * ("dies", "destroyed")
 */
bool monster_living(monster_race *r_ptr)
{
    /* Non-living, undead, or demon */
    if (r_ptr->flags3 & (RF3_DEMON | RF3_UNDEAD | RF3_NONLIVING))
        return FALSE;
    else
        return TRUE;
}

bool monster_magical(monster_race *r_ptr)
{
    if (r_ptr->freq_spell >= 16)
        return TRUE;
    else
        return FALSE;
}

/*
 * Is this monster declined to be questor or bounty?
 */
bool no_questor_or_bounty_uniques(int r_idx)
{
    switch (r_idx)
    {
    /*
     * Decline them to be questor or bounty because they use
     * special motion "split and combine"
     */
    case MON_BANORLUPART:
    case MON_BANOR:
    case MON_LUPART:
        return TRUE;
    default:
        return FALSE;
    }
}
