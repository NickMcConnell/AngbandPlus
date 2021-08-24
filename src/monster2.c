/* File: monster2.c */

/*
 * Copyright (c) 1997 Ben Harrison, James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies. Other copyrights may also apply.
 */

/* Purpose: misc code for monsters */

#include "angband.h"
#include "dun.h"
#include "rooms.h"

#include <assert.h>

#define HORDE_NOGOOD 0x01
#define HORDE_NOEVIL 0x02

cptr horror_desc[MAX_SAN_HORROR] =
{
    "abominable",
    "abysmal",
    "appalling",
    "baleful",
    "blasphemous",

    "disgusting",
    "dreadful",
    "filthy",
    "grisly",
    "hideous",

    "hellish",
    "horrible",
    "infernal",
    "loathsome",
    "nightmarish",

    "repulsive",
    "sacrilegious",
    "terrible",
    "unclean",
    "unspeakable",

};

cptr funny_desc[MAX_SAN_FUNNY] =
{
    "silly",
    "hilarious",
    "absurd",
    "insipid",
    "ridiculous",

    "laughable",
    "ludicrous",
    "far-out",
    "groovy",
    "postmodern",

    "fantastic",
    "dadaistic",
    "cubistic",
    "cosmic",
    "awesome",

    "incomprehensible",
    "fabulous",
    "amazing",
    "incredible",
    "chaotic",

    "wild",
    "preposterous",

};

cptr funny_comments[MAX_SAN_COMMENT] =
{
    "Wow, cosmic, man!",
    "Rad!",
    "Groovy!",
    "Cool!",
    "Far out!"

};

/* cf design/see_invisible.ods. One can greatly improve the
 * odds of perceiving invisible foes with a something like an
 * elven cloak +4 and more than one source of SI. Pay attention
 * to searching skills when choosing race/class/personality as 
 * this skill does not increase with level or stats. */
bool plr_see_invis(int rlev)
{
    int i;
    int skill = plr->skills.srh;
    for (i = 0; i < plr->see_inv; i++)
    {
        if (randint0(65 + rlev/2) < 15 + skill)
            return TRUE;
    }
    return FALSE;
}

monster_type *mon_get_parent(monster_type *m_ptr)
{
    monster_type *result = NULL;
    if (m_ptr->parent_id)
    {
        result = dun_mon(cave, m_ptr->parent_id);
        if (!mon_is_valid(result))
            result = NULL;
    }
    return result;
}

void mon_set_parent(monster_type *m_ptr, int pm_idx)
{
    m_ptr->parent_id = pm_idx;
}

/*
 *  Extract monster race pointer of a monster's true form
 */
monster_race *real_r_ptr(monster_type *m_ptr)
{
    return mon_true_race(m_ptr);
}

int real_r_idx(monster_type *m_ptr)
{
    return mon_true_race(m_ptr)->id;
}


/*
 * Delete a monster by index.
 *
 * When a monster is deleted, all of its objects are deleted.
 */
void delete_monster(mon_ptr mon)
{
    dun_ptr dun = mon->dun; /* XXX do_cmd_pet_dismiss can dismiss off level pets (!= cave) */
    point_t pos = mon->pos;
    mon_race_ptr race = mon->race;

    assert(!mon_is_deleted(mon));
    if (mon_is_deleted(mon)) return; /* paranoia: delete_monster already called! */

    /* Hack -- count the number of "reproducers" */
    if (mon_can_multiply(mon)) dun->breed_ct--;

    mon_set_parent(mon, 0);

    if (mon->id == plr->health_who) health_track(NULL);

    if (mon == who_mon(plr->target))
        plr->target = who_create_null();
    if (mon == who_mon(plr->pet_target))
        plr->pet_target = who_create_null();
    if (mon == who_mon(plr->riding_target))
        plr->riding_target = who_create_null();

    if (plr->riding == mon->id) plr->riding = 0;

    if (mon == who_mon(plr->duelist_target))
    {
        plr->duelist_target = who_create_null();
        plr->redraw |= PR_STATUS;
    }
    if (mon_is_pet(mon)) check_pets_num_and_align();

    if (mon->dun->id == plr->dun_id)
    {
        if (race->light || race->lantern)
            plr->update |= PU_LIGHT;

        plr->window |= PW_MONSTER_LIST;
    }
    dun_delete_mon(dun, mon->id);
    dun_draw_pos(dun, pos); /* it feels icky to use mon->pos here ... */
}


/*
 * Delete the monster, if any, at a given location
 */
void delete_monster_at(point_t pos)
{
    mon_ptr mon;
    if (!dun_pos_interior(cave, pos)) return;
    mon = dun_mon_at(cave, pos);
    if (mon) delete_monster(mon);
}

/*
 * Hack -- the "type" of the current "summon specific"
 */
int summon_specific_type = 0;
static bool summon_specific_okay(mon_race_ptr r_ptr);


/*
 * Hack -- the index of the summoning monster
 */
who_t summon_specific_who;


static bool summon_unique_okay = FALSE;
static bool summon_cloned_okay = FALSE;
static bool summon_wall_scummer = FALSE;
static bool summon_ring_bearer = FALSE;


static bool summon_specific_aux(mon_race_ptr r_ptr)
{
    return mon_is_type(r_ptr, summon_specific_type);
}

bool mon_is_type(mon_race_ptr r_ptr, int type)
{
    switch (type)
    {
    case SUMMON_CHAPEL_GOOD: return vault_aux_chapel_g(r_ptr);
    case SUMMON_CHAPEL_EVIL: return vault_aux_chapel_e(r_ptr);

    case SUMMON_ULTIMATE: {
        static cptr _which[] = {"p.ultimate magus", "D.power",
            "D.sky", "Q.master", "L.reaver", "U.star-spawn", "U.cyber", NULL};
        if (mon_race_is_one_(r_ptr, _which))
        {
            return TRUE;
        }
        break; }
    case SUMMON_BALROG:
        if (mon_race_is_(r_ptr, "U.balrog") || mon_race_is_(r_ptr, "U.balrog.lesser"))
            return TRUE;
        break;
    case SUMMON_CLUBBER_DEMON: return mon_race_is_(r_ptr, "U.clubber");
    case SUMMON_DEMON_SUMMONER:
        if (!mon_race_is_unique(r_ptr) && mon_race_can_summon(r_ptr, SUMMON_DEMON))
            return TRUE;
        break;
    case SUMMON_MATURE_DRAGON:
        /* Hack -- all non-unique 'd's with 'ature' or 'rake' in name */
        if ( mon_race_is_char(r_ptr, 'd')
          && !mon_race_is_unique(r_ptr)
          && (strstr(r_ptr->name, "ature") || strstr(r_ptr->name, "rake")) )
        {
            return TRUE;
        }
        break;
    case SUMMON_DRAGON_SUMMONER:
        if ( !mon_race_is_unique(r_ptr)
          && (mon_race_can_summon(r_ptr, SUMMON_DRAGON) || mon_race_can_summon(r_ptr, SUMMON_HI_DRAGON)) )
        {
            return TRUE;
        }
        break;
    case SUMMON_UNDEAD_SUMMONER:
        if ( !mon_race_is_unique(r_ptr)
          && (mon_race_can_summon(r_ptr, SUMMON_UNDEAD) || mon_race_can_summon(r_ptr, SUMMON_HI_UNDEAD)) )
        {
            return TRUE;
        }
        break;
    case SUMMON_DARK_ELF: return mon_race_is_dark_elf(r_ptr);
    case SUMMON_GIANT: return mon_race_is_char_ex(r_ptr, "OP");
    case SUMMON_ORC: return mon_race_is_char(r_ptr, 'o');
    case SUMMON_TROLL: return mon_race_is_char(r_ptr, 'T');
    case SUMMON_YEEK: return mon_race_is_char(r_ptr, 'y');
    case SUMMON_ANT: return mon_race_is_char(r_ptr, 'a');
    case SUMMON_SPIDER: return mon_race_is_char(r_ptr, 'S') && (r_ptr->move.flags & RFM_PASSWEB);
    case SUMMON_HOUND: return mon_race_is_char_ex(r_ptr, "CZ");
    case SUMMON_HYDRA: return mon_race_is_char(r_ptr, 'M');
    case SUMMON_ENT: return mon_race_is_(r_ptr, "#.ent");
    case SUMMON_ANGEL:
        if (mon_race_is_char(r_ptr, 'A') && r_ptr->align) /* exlude A.silver: and gold? */
            return TRUE;
        break;
    case SUMMON_DEMON: return mon_race_is_demon(r_ptr);
    case SUMMON_UNDEAD: return mon_race_is_undead(r_ptr);
    case SUMMON_DRAGON: return mon_race_is_dragon(r_ptr);
    case SUMMON_HI_UNDEAD: return mon_race_is_char_ex(r_ptr, "LVW"); 
    case SUMMON_HI_DRAGON: return mon_race_is_char(r_ptr, 'D');
    case SUMMON_HI_DEMON:
        if (mon_race_is_char_ex(r_ptr, "UHB") && mon_race_is_demon(r_ptr)) return TRUE;
        break;
    case SUMMON_AMBERITE: return mon_race_is_amberite(r_ptr);
    case SUMMON_OLYMPIAN: return mon_race_is_olympian(r_ptr);
    case SUMMON_HUMAN: return mon_race_is_human(r_ptr);
    case SUMMON_HORSE:
        if (mon_race_is_char(r_ptr, 'q') && mon_race_is_ridable(r_ptr)) return TRUE;
        break;
    case SUMMON_CAMELOT:
        if ((r_ptr->alloc.dun_type_id == D_CAMELOT) && mon_race_is_knight(r_ptr)) return TRUE;
        break;
    case SUMMON_NIGHTMARE: return mon_race_is_(r_ptr, "q.night mare");
    case SUMMON_RAT: return mon_race_is_char(r_ptr, 'r');
    case SUMMON_BAT: return mon_race_is_char(r_ptr, 'b');
    case SUMMON_WOLF: return mon_race_is_char(r_ptr, 'C');
    case SUMMON_DREAD: return mon_race_is_(r_ptr, "G.dread");
    case SUMMON_ZOMBIE: return mon_race_is_char(r_ptr, 'z');
    case SUMMON_SKELETON:
        if (mon_race_is_char(r_ptr, 's') || mon_race_is_(r_ptr, "D.bone")) return TRUE;
        break;
    case SUMMON_GHOST:
        if (mon_race_is_(r_ptr, "G.shadow demon")) return FALSE;
        if (mon_race_is_char(r_ptr, 'G') || mon_race_is_(r_ptr, "D.spectral")) return TRUE;
        break;
    case SUMMON_KRAKEN:
        if ( mon_race_is_(r_ptr, "l.greater kraken") 
          || mon_race_is_(r_ptr, "l.lesser kraken") ) return TRUE;
        break;
    case SUMMON_VAMPIRE: return mon_race_is_char(r_ptr, 'V');
    case SUMMON_WIGHT: return mon_race_is_char(r_ptr, 'W');
    case SUMMON_LICH:
        return (mon_race_is_char(r_ptr, 'L') || mon_race_is_(r_ptr, "D.dracolich"));
    case SUMMON_THIEF: return mon_race_is_thief(r_ptr);
    case SUMMON_UNIQUE: return mon_race_is_unique(r_ptr);
    case SUMMON_BIZARRE1: return mon_race_is_char(r_ptr, 'm');
    case SUMMON_BIZARRE2: return mon_race_is_char(r_ptr, 'b');
    case SUMMON_BIZARRE3: return mon_race_is_char(r_ptr, 'Q');
    case SUMMON_BIZARRE4: return mon_race_is_char(r_ptr, 'v');
    case SUMMON_BIZARRE5: return mon_race_is_char(r_ptr, '$');
    case SUMMON_BIZARRE6: return mon_race_is_char_ex(r_ptr, "!?=$|");
    case SUMMON_GOLEM: return mon_race_is_char(r_ptr, 'g');
    case SUMMON_CYBER: return mon_race_is_(r_ptr, "U.cyber");
    case SUMMON_KIN: return mon_race_is_char(r_ptr, summon_kin_type);
    case SUMMON_DAWN: return mon_race_is_(r_ptr, "p.dawn");
    case SUMMON_ANIMAL: return mon_race_is_animal(r_ptr);
    case SUMMON_ANIMAL_RANGER:
        if ( mon_race_is_animal(r_ptr)
          && mon_race_is_char_ex(r_ptr, "abcflqrwBCHIJKMRS")
          && !mon_race_is_dragon(r_ptr)
          && !mon_race_is_evil(r_ptr)
          && !mon_race_is_undead(r_ptr)
          && !mon_race_is_demon(r_ptr)
          && !mon_race_can_multiply(r_ptr)
          && !r_ptr->spells )
        {
            return TRUE;
        }
        break;
    case SUMMON_HI_DRAGON_LIVING:
        if (mon_race_is_char(r_ptr, 'D') && mon_race_is_living(r_ptr)) return TRUE;
        break;
    case SUMMON_LIVING: return mon_race_is_living(r_ptr);
    case SUMMON_PHANTOM:
        if (mon_race_is_(r_ptr, "G.phantom") || mon_race_is_(r_ptr, "G.phantom beast"))
            return TRUE;
        break;
    case SUMMON_BLUE_HORROR: return mon_race_is_(r_ptr, "u.horror.blue");
    case SUMMON_ELEMENTAL: return mon_race_is_char(r_ptr, 'E');
    case SUMMON_VORTEX: return mon_race_is_char(r_ptr, 'v');
    case SUMMON_HYBRID: return mon_race_is_char(r_ptr, 'H');
    case SUMMON_BIRD: return mon_race_is_char(r_ptr, 'B');
    case SUMMON_ARCHER:
        if ( mon_race_is_char(r_ptr, 'p')
          && r_ptr->spells
          && mon_spells_find(r_ptr->spells, mon_spell_id(MST_BOLT, GF_ARROW)) )
        {
            return TRUE;
        }
        break;
    case SUMMON_KAMIKAZE:
        if (mon_blows_find(r_ptr->blows, RBM_EXPLODE)) return TRUE;
        break;
    case SUMMON_KAMIKAZE_LIVING:
        if (mon_race_is_living(r_ptr) && mon_blows_find(r_ptr->blows, RBM_EXPLODE)) return TRUE;
        break;
    case SUMMON_MANES: return mon_race_is_(r_ptr, "u.manes");
    case SUMMON_LOUSE: return mon_race_is_(r_ptr, "I.louse");
    case SUMMON_SOFTWARE_BUG: return mon_race_is_(r_ptr, "I.software bug");
    case SUMMON_GUARDIAN:
        if (r_ptr->alloc.flags & RFA_GUARDIAN) return TRUE;
        if (r_ptr->flagsx & RFX_GUARDIAN) return TRUE;
        break;
    case SUMMON_KNIGHT: {
        static cptr _which[] = { "p.novice paladin", "p.paladin", "p.white knight",
            "p.ultra-elite paladin", "p.knight templar", NULL };
        if (mon_race_is_one_(r_ptr, _which))
            return TRUE;
        break; }
    case SUMMON_EAGLE:
        if (mon_race_is_char(r_ptr, 'B') && (r_ptr->alloc.flags & RFA_WILD_MOUNTAIN) && (r_ptr->alloc.flags & RFA_WILD_ONLY))
            return TRUE;
        break;
    case SUMMON_PIRANHA:
        if (mon_race_is_(r_ptr, "l.piranha") || mon_race_is_(r_ptr, "l.giant piranha"))
            return TRUE;
        break;
    case SUMMON_ARMAGE_GOOD:
        if (mon_race_is_char(r_ptr, 'A') && r_ptr->align > 0) return TRUE;
        break;
    case SUMMON_ARMAGE_EVIL:
        if ( mon_race_is_demon(r_ptr)
          || (mon_race_is_char(r_ptr, 'A') && r_ptr->align < 0) )
        {
            return TRUE;
        }
        break;
    case SUMMON_MAGICAL: return monster_magical(r_ptr);
    case SUMMON_RING_BEARER:
        if (mon_race_is_char_ex(r_ptr, "pthAy"))
            return TRUE;
        else if (mon_race_is_nazgul(r_ptr))
            return TRUE;
        else if ( mon_race_is_(r_ptr, "W.King") || mon_race_is_(r_ptr, "W.Hoarmurath")
               || mon_race_is_(r_ptr, "W.Dwar") || mon_race_is_(r_ptr, "W.Khamul") )
             return TRUE;
        else if (mon_race_is_char(r_ptr, 'V'))
        {
            return !mon_race_is_(r_ptr, "V.oriental") && !mon_race_is_(r_ptr, "V.star")
                && !mon_race_is_(r_ptr, "V.fire");
        }
        else if (mon_race_is_char(r_ptr, 'L'))
            return !mon_race_is_(r_ptr, "L.iron");
        else if (mon_race_is_(r_ptr, "z.hand") || mon_race_is_(r_ptr, "s.druj.hand"))
            return TRUE;
        break;
    case SUMMON_MONK: {
        static cptr _which[] = { "p.jade monk", "p.ivory monk", "L.monk", "p.ebony monk",
            "p.topaz monk", "p.mystic", "p.master mystic", "p.grand master mystic", NULL};
        if (mon_race_is_one_(r_ptr, _which))
            return TRUE;
        break; }
    case SUMMON_MAGE:
        if (mon_race_is_char_ex(r_ptr, "hp"))
        {
            if ( r_ptr->body.class_id == CLASS_MAGE
              || r_ptr->body.class_id == CLASS_SORCERER )
            {
                return TRUE;
            }
        }
        break;
    case SUMMON_ELDRITCH_HORROR: return mon_race_is_horror(r_ptr);
    }
    return FALSE;
}




int mysqrt(int n)
{
    int tmp = n>>1;
    int tasu = 10;
    int kaeriti = 1;

    if (!tmp)
    {
        if (n) return 1;
        else return 0;
    }

    while(tmp)
    {
        if ((n/tmp) < tmp)
        {
            tmp >>= 1;
        }
        else break;
    }
    kaeriti = tmp;
    while(tasu)
    {
        if ((n/tmp) < tmp)
        {
            tasu--;
            tmp = kaeriti;
        }
        else
        {
            kaeriti = tmp;
            tmp += tasu;
        }
    }
    return kaeriti;
}

/*
 * Build a string describing a monster in some way.
 *
 * We can correctly describe monsters based on their visibility.
 * We can force all monsters to be treated as visible or invisible.
 * We can build nominatives, objectives, possessives, or reflexives.
 * We can selectively pronominalize hidden, visible, or all monsters.
 * We can use definite or indefinite descriptions for hidden monsters.
 * We can use definite or indefinite descriptions for visible monsters.
 *
 * Pronominalization involves the gender whenever possible and allowed,
 * so that by cleverly requesting pronominalization / visibility, you
 * can get messages like "You hit someone. She screams in agony!".
 *
 * Reflexives are acquired by requesting Objective plus Possessive.
 *
 * If no m_ptr arg is given (?), the monster is assumed to be hidden,
 * unless the "Assume Visible" mode is requested.
 *
 * If no r_ptr arg is given, it is extracted from m_ptr and r_info
 * If neither m_ptr nor r_ptr is given, the monster is assumed to
 * be neuter, singular, and hidden (unless "Assume Visible" is set),
 * in which case you may be in trouble... :-)
 *
 * I am assuming that no monster name is more than 70 characters long,
 * so that "char desc[80];" is sufficiently large for any result.
 *
 * Mode Flags:
 *  MD_OBJECTIVE      --> Objective (or Reflexive)
 *  MD_POSSESSIVE     --> Possessive (or Reflexive)
 *  MD_INDEF_HIDDEN   --> Use indefinites for hidden monsters ("something")
 *  MD_INDEF_VISIBLE  --> Use indefinites for visible monsters ("a kobold")
 *  MD_PRON_HIDDEN    --> Pronominalize hidden monsters
 *  MD_PRON_VISIBLE   --> Pronominalize visible monsters
 *  MD_ASSUME_HIDDEN  --> Assume the monster is hidden
 *  MD_ASSUME_VISIBLE --> Assume the monster is visible
 *  MD_TRUE_NAME      --> Chameleon's true name
 *  MD_IGNORE_HALLU   --> Ignore hallucination, and penetrate shape change
 *
 * Useful Modes:
 *  0x00 --> Full nominative name ("the kobold") or "it"
 *  MD_INDEF_HIDDEN --> Full nominative name ("the kobold") or "something"
 *  MD_ASSUME_VISIBLE --> Genocide resistance name ("the kobold")
 *  MD_ASSUME_VISIBLE | MD_INDEF_VISIBLE --> Killing name ("a kobold")
 *  MD_PRON_VISIBLE | MD_POSSESSIVE
 *    --> Possessive, genderized if visable ("his") or "its"
 *  MD_PRON_VISIBLE | MD_POSSESSIVE | MD_OBJECTIVE
 *    --> Reflexive, genderized if visable ("himself") or "itself"
 */
void monster_desc(char *desc, monster_type *m_ptr, int mode)
{
    cptr            res;
    monster_race    *r_ptr;

    cptr            name;
    char            buf[128];
    char            silly_name[1024];
    bool            seen, pron;
    bool            fuzzy = (m_ptr->mflag2 & MFLAG2_FUZZY) && !(mode & MD_IGNORE_FUZZY);
    bool            named = FALSE;

    /* Hack: Chengband requested to just show the pet's name (e.g. 'Stinky' vs
     * 'Your super-duper multi-hued behemoth called Stinky') */
    if (m_ptr->nickname && !(mode & MD_NO_PET_ABBREV) && !(mode & MD_PRON_VISIBLE))
    {
        sprintf(desc, "%s", quark_str(m_ptr->nickname));
        return;
    }

    r_ptr = m_ptr->apparent_race;

    if (fuzzy) name = "Monster";
    /* Mode of MD_TRUE_NAME will reveal Chameleon's true name */
    else if (mode & MD_TRUE_NAME) name = real_r_ptr(m_ptr)->name;
    else name = r_ptr->name;

    /* Are we hallucinating? (Idea from Nethack...) */
    if (plr_tim_find(T_HALLUCINATE) && !(mode & MD_IGNORE_HALLU))
    {
        if (one_in_(2))
        {
            if (!get_rnd_line("silly.txt", sym_str(m_ptr->race->id), silly_name))

                named = TRUE;
        }

        if (!named)
        {
            monster_race *hallu_race;

            do
            {
                hallu_race = vec_random(mon_alloc_tbl);
            }
            while (mon_race_is_unique(hallu_race));

            strcpy(silly_name, hallu_race->name);
        }

        /* Better not strcpy it, or we could corrupt r_info... */
        name = silly_name;
    }

    /* Can we "see" it (exists + forced, or visible + not unforced) */
    seen = (m_ptr && ((mode & MD_ASSUME_VISIBLE) || (!(mode & MD_ASSUME_HIDDEN) && m_ptr->ml)));

    /* Sexed Pronouns (seen and allowed, or unseen and allowed) */
    pron = (m_ptr && ((seen && (mode & MD_PRON_VISIBLE)) || (!seen && (mode & MD_PRON_HIDDEN))));


    /* First, try using pronouns, or describing hidden monsters */
    if (!seen || pron)
    {
        /* an encoding of the monster "sex" */
        int kind = 0x00;

        /* Extract the gender (if applicable) */
        if (mon_race_is_female(r_ptr)) kind = 0x20;
        else if (mon_race_is_male(r_ptr)) kind = 0x10;

        /* Ignore the gender (if desired) */
        if (!m_ptr || !pron) kind = 0x00;


        /* Assume simple result */
        res = "it";


        /* Brute force: split on the possibilities */
        switch (kind + (mode & (MD_INDEF_HIDDEN | MD_POSSESSIVE | MD_OBJECTIVE)))
        {
            /* Neuter, or unknown */
            case 0x00:                                                    res = "it"; break;
            case 0x00 + (MD_OBJECTIVE):                                   res = "it"; break;
            case 0x00 + (MD_POSSESSIVE):                                  res = "its"; break;
            case 0x00 + (MD_POSSESSIVE | MD_OBJECTIVE):                   res = "itself"; break;
            case 0x00 + (MD_INDEF_HIDDEN):                                res = "something"; break;
            case 0x00 + (MD_INDEF_HIDDEN | MD_OBJECTIVE):                 res = "something"; break;
            case 0x00 + (MD_INDEF_HIDDEN | MD_POSSESSIVE):                res = "something's"; break;
            case 0x00 + (MD_INDEF_HIDDEN | MD_POSSESSIVE | MD_OBJECTIVE): res = "itself"; break;


            /* Male (assume human if vague) */
            case 0x10:                                                    res = "he"; break;
            case 0x10 + (MD_OBJECTIVE):                                   res = "him"; break;
            case 0x10 + (MD_POSSESSIVE):                                  res = "his"; break;
            case 0x10 + (MD_POSSESSIVE | MD_OBJECTIVE):                   res = "himself"; break;
            case 0x10 + (MD_INDEF_HIDDEN):                                res = "someone"; break;
            case 0x10 + (MD_INDEF_HIDDEN | MD_OBJECTIVE):                 res = "someone"; break;
            case 0x10 + (MD_INDEF_HIDDEN | MD_POSSESSIVE):                res = "someone's"; break;
            case 0x10 + (MD_INDEF_HIDDEN | MD_POSSESSIVE | MD_OBJECTIVE): res = "himself"; break;


            /* Female (assume human if vague) */
            case 0x20:                                                    res = "she"; break;
            case 0x20 + (MD_OBJECTIVE):                                   res = "her"; break;
            case 0x20 + (MD_POSSESSIVE):                                  res = "her"; break;
            case 0x20 + (MD_POSSESSIVE | MD_OBJECTIVE):                   res = "herself"; break;
            case 0x20 + (MD_INDEF_HIDDEN):                                res = "someone"; break;
            case 0x20 + (MD_INDEF_HIDDEN | MD_OBJECTIVE):                 res = "someone"; break;
            case 0x20 + (MD_INDEF_HIDDEN | MD_POSSESSIVE):                res = "someone's"; break;
            case 0x20 + (MD_INDEF_HIDDEN | MD_POSSESSIVE | MD_OBJECTIVE): res = "herself"; break;
        }

        /* Copy the result */
        (void)strcpy(desc, res);
    }


    /* Handle visible monsters, "reflexive" request */
    else if ((mode & (MD_POSSESSIVE | MD_OBJECTIVE)) == (MD_POSSESSIVE | MD_OBJECTIVE))
    {
        /* The monster is visible, so use its gender */
        if (mon_race_is_female(r_ptr)) strcpy(desc, "herself");
        else if (mon_race_is_male(r_ptr)) strcpy(desc, "himself");
        else strcpy(desc, "itself");
    }


    /* Handle all other visible monster requests */
    else
    {
        /* Tanuki? */
        if (mon_is_pet(m_ptr) && !is_original_ap(m_ptr))
        {
            (void)sprintf(desc, "%s?", name);
        }
        else

        /* It could be a Unique */
        if ( mon_race_is_unique(r_ptr)
          && !(plr_tim_find(T_HALLUCINATE) && !(mode & MD_IGNORE_HALLU))
          && !fuzzy )
        {
            /* Start with the name (thus nominative and objective) */
            if ((m_ptr->mflag2 & MFLAG2_CHAMELEON) && !(mode & MD_TRUE_NAME))
            {
                (void)sprintf(desc, "%s?", name);
            }
            else
            {
                (void)strcpy(desc, name);
            }
        }

        /* It could be an indefinite monster */
        else if (mode & MD_INDEF_VISIBLE)
        {
            /* XXX Check plurality for "some" */

            /* Indefinite monsters need an indefinite article */
            (void)strcpy(desc, is_a_vowel(name[0]) ? "an " : "a ");

            (void)strcat(desc, name);
        }
        /* It could be a normal, definite, monster */
        else
        {
            /* Definite monsters need a definite article */
            if (mon_is_pet(m_ptr))
                (void)strcpy(desc, "your ");

            else
                (void)strcpy(desc, "the ");

            if (m_ptr->mflag2 & MFLAG2_ILLUSION)
                strcat(desc, "illusory ");

            (void)strcat(desc, name);
        }

        if (m_ptr->nickname)
        {
            sprintf(buf," called %s",quark_str(m_ptr->nickname));
            strcat(desc,buf);
        }

        if (plr->riding && (dun_mon(cave, plr->riding) == m_ptr))
        {
            if (plr->prace == RACE_MON_RING)
                strcat(desc," (controlling)");
            else
                strcat(desc," (riding)");
        }

        if ((mode & MD_IGNORE_HALLU) && (m_ptr->mflag2 & MFLAG2_CHAMELEON))
        {
            if (mon_race_is_unique(r_ptr))
                strcat(desc," (Chameleon Lord)");
            else
                strcat(desc," (Chameleon)");
        }

        if ((mode & MD_IGNORE_HALLU) && !is_original_ap(m_ptr))
        {
            strcat(desc, format("(%s)", m_ptr->race->name));
        }

        /* Handle the Possessive as a special afterthought */
        if (mode & MD_POSSESSIVE)
        {
            /* XXX Check for trailing "s" */

            /* Simply append "apostrophe" and "s" */
            (void)strcat(desc, "'s");
        }
    }

    /* Hack for the Duelist */
    #if 0
    if ( plr->pclass == CLASS_DUELIST
      && !(mode & MD_PRON_VISIBLE)
      && m_ptr == who_mon(plr->duelist_target) )
    {
        strcat(desc, " (Foe)");
    }
    #endif

    if ( plr->painted_target
      && plr->painted_target_idx
      && m_ptr == dun_mon(cave, plr->painted_target_idx)
      && plr->painted_target_ct >= 3 )
    {
        strcat(desc, " (Painted)");
    }

    #if 0
    if (plr->wizard && m_ptr->mpower != 1000)
        strcat(desc, format(" (%d.%d%%)", m_ptr->mpower/10, m_ptr->mpower%10));
    if (plr->wizard && m_ptr->turns)
        strcat(desc, format(" (%d)", m_ptr->turns));
    #endif
}




/*
 * Learn about a monster (by "probing" it)
 */
void lore_do_probe(int r_idx)
{
    monster_race *r_ptr = mon_race_lookup(r_idx);

    r_ptr->lore.move = r_ptr->move.flags;
    r_ptr->lore.kind = r_ptr->kind;
    r_ptr->lore.abilities = r_ptr->abilities;
    r_ptr->lore.attributes = r_ptr->attributes;
    r_ptr->lore.resist = r_ptr->resist | r_ptr->immune | r_ptr->vuln;

    r_ptr->lore.flags |= RFL_EVOLUTION; /* including knowledge that monsters don't evolve */
    r_ptr->lore.flags |= RFL_POSSESSOR;
    r_ptr->lore.flags |= RFL_PROBE;
    r_ptr->lore.flags |= RFL_ALIGN;

    if (plr->monster_race_idx == r_idx)
        plr->window |= (PW_MONSTER);
}


/*
 * Take note that the given monster just dropped some treasure
 *
 * Note that learning the "GOOD"/"GREAT" flags gives information
 * about the treasure (even when the monster is killed for the first
 * time, such as uniques, and the treasure has not been examined yet).
 *
 * This "indirect" method is used to prevent the player from learning
 * exactly how much treasure a monster can drop from observing only
 * a single example of a drop. This method actually observes how much
 * gold and items are dropped, and remembers that information to be
 * described later by the monster recall code.
 */
void lore_treasure(int m_idx, int num_item, int num_gold)
{
    monster_type *m_ptr = dun_mon(cave, m_idx);
    monster_race *r_ptr = m_ptr->race;

    /* If the monster doesn't have original appearance, don't note */
    if (!mon_lore_allow(m_ptr)) return;

    /* Note the number of things dropped */
    if (num_item > r_ptr->lore.drops.obj) r_ptr->lore.drops.obj = num_item;
    if (num_gold > r_ptr->lore.drops.gold) r_ptr->lore.drops.gold = num_gold;

    /* Update monster recall window */
    if (plr->monster_race_idx == m_ptr->race->id)
    {
        /* Window stuff */
        plr->window |= (PW_MONSTER);
    }
}



void sanity_blast(monster_type *m_ptr, bool necro)
{
    bool happened = FALSE;
    int power = 100;

    if (!(cave->flags & DF_GENERATED)) return;
    if (plr->no_eldritch) return;

    if (!necro)
    {
        char            m_name[80];
        monster_race    *r_ptr = m_ptr->apparent_race;

        power = r_ptr->alloc.lvl / 2;

        monster_desc(m_name, m_ptr, 0);

        if (!mon_race_is_unique(r_ptr))
        {
            if (r_ptr->friends)
                power /= 2;
        }
        else power *= 2;

        if (!hack_mind)
            return; /* No effect yet, just loaded... */

        if (!m_ptr->ml)
            return; /* Cannot see it for some reason */

        if (!mon_is_horror(m_ptr))
            return; /* oops */



        if (mon_is_pet(m_ptr))
            return; /* Pet eldritch horrors are safe most of the time */

        if (randint1(100) > power) return;

        if (saving_throw(plr->skills.sav - power))
        {
            return; /* Save, no adverse effects */
        }

        if (plr_tim_find(T_HALLUCINATE))
        {
            /* Something silly happens... */
            msg_format("You behold the %s visage of %s!",
                funny_desc[randint0(MAX_SAN_FUNNY)], m_name);


            if (one_in_(3))
            {
                msg_print(funny_comments[randint0(MAX_SAN_COMMENT)]);
                plr_tim_add(T_HALLUCINATE, randint1(r_ptr->alloc.lvl));
            }

            return; /* Never mind; we can't see it clearly enough */
        }

        /* Something frightening happens... */
        msg_format("You behold the %s visage of %s!",
            horror_desc[randint0(MAX_SAN_HORROR)], m_name);

        mon_lore_horror(m_ptr);

        /* Demon characters are unaffected */
        if (get_race()->flags & RACE_IS_DEMON) return;

        if (plr->wizard) return;

        /* Undead characters are 50% likely to be unaffected */
        if (get_race()->flags & RACE_IS_UNDEAD)
        {
            if (saving_throw(25 + plr->lev)) return;
        }
    }
    else
    {
        msg_print("Your sanity is shaken by reading the Necronomicon!");

    }

    if (!saving_throw(plr->skills.sav - power)) /* Mind blast */
    {
        if (!res_save_default(GF_CONF))
            plr_tim_add(T_CONFUSED, randint0(4) + 4);
        if (!res_save_default(GF_CHAOS) && !mut_present(MUT_WEIRD_MIND) && one_in_(3))
            plr_tim_add(T_HALLUCINATE, randint0(25) + 15);
        return;
    }

    if (!saving_throw(plr->skills.sav - power))
    {
        do_dec_stat(A_INT);
        do_dec_stat(A_WIS);
        do_dec_stat(A_CHR);
        return;
    }

    if (!saving_throw(plr->skills.sav - power)) /* Brain smash */
    {
        if (!res_save_default(GF_CONF))
            plr_tim_add(T_CONFUSED, randint0(4) + 4);
        if (!free_act_save_p(power))
            plr_tim_add(T_PARALYZED, randint1(4));
        else 
            equip_learn_flag(OF_FREE_ACT);
        while (randint0(100) > plr->skills.sav)
            do_dec_stat(A_INT);
        while (randint0(100) > plr->skills.sav)
            do_dec_stat(A_WIS);
        if (!res_save_default(GF_CHAOS) && !mut_present(MUT_WEIRD_MIND))
            plr_tim_add(T_HALLUCINATE, randint0(25) + 15);
        return;
    }

    if (!saving_throw(plr->skills.sav - power)) /* Amnesia */
    {

        if (lose_all_info())
            msg_print("You forget everything in your utmost terror!");

        return;
    }

    if (saving_throw(plr->skills.sav - power))
    {
        return;
    }

    /* Else gain permanent insanity */
    if (mut_present(MUT_MORONIC) && /*(plr->muta2 & MUT2_BERS_RAGE) &&*/
        (mut_present(MUT_COWARDICE) || res_save_default(GF_FEAR)) &&
        (mut_present(MUT_HALLUCINATION) || res_save_default(GF_CHAOS)))
    {
        /* The poor bastard already has all possible insanities! */
        return;
    }

    while (!happened)
    {
        switch (randint1(21))
        {
            case 1:
                if (!mut_present(MUT_MORONIC))
                {
                    if (one_in_(5))
                    {
                        mut_gain(MUT_MORONIC);
                        happened = TRUE;
                    }
                    else
                    {
                        /* Tone down the Moron. For characters who resist fear and chaos, it
                           becomes a guaranteed mutation if we get this far (which requires FMMMMF on
                           saving throws, btw). I think the FMMMMF sequence penalizes characters with
                           relatively high saves, as they are more likely to make 4 in a row than
                           characters with poor saves. Perhaps, that would be a better level of redress.
                           So, one initial save to avoid noticing the eldritch horror, and then a random
                           dice roll to pick the effect?  As it used to stand, the moron mutation was a gimme
                           unless you had a very high save (like with a patient sorceror), and would generally
                           occur every 5 to 10 minutes of play. So you could fight it, running to Zul after
                           every trip down to the dungeon, or you could just try to find more Int/Wis to
                           compensate. A guaranteed 12% fail rate penalty on spells is a bit too harsh ...
                           */
                        msg_print("You feel like a moron for a moment, but it passes.");
                        happened = TRUE;
                    }
                }
                break;
            case 2:
            case 3:
            case 4:
            case 5:
            case 6:
            case 7:
            case 8:
            case 9:
            case 10:
            case 11:
                if (!mut_present(MUT_COWARDICE) && !res_save_default(GF_FEAR))
                {
                    mut_gain(MUT_COWARDICE);
                    happened = TRUE;
                }
                break;
            case 12:
            case 13:
            case 14:
            case 15:
            case 16:
            case 17:
            case 18:
            case 19:
            case 20:
            case 21:
                if (!mut_present(MUT_HALLUCINATION) && !res_save_default(GF_CHAOS))
                {
                    mut_gain(MUT_HALLUCINATION);
                    happened = TRUE;
                }
                break;
            default:
                if (!mut_present(MUT_BERS_RAGE))
                {
                    mut_gain(MUT_BERS_RAGE);
                    happened = TRUE;
                }
                break;
        }
    }

    plr->update |= PU_BONUS;
    handle_stuff();
}


/*
 * This function updates the monster record of the given monster
 *
 * This involves extracting the distance to the player (if requested),
 * and then checking for visibility (natural, infravision, see-invis,
 * telepathy), updating the monster visibility flag, redrawing (or
 * erasing) the monster when its visibility changes, and taking note
 * of any interesting monster flags (cold-blooded, invisible, etc).
 *
 * Note the new "mflag" field which encodes several monster state flags,
 * including "view" for when the monster is currently in line of sight,
 * and "mark" for when the monster is currently visible via detection.
 *
 * The only monster fields that are changed here are "cdis" (the
 * distance from the player), "ml" (visible to the player), and
 * "mflag" (to maintain the "MFLAG_VIEW" flag).
 *
 * Note the special "update_monsters()" function which can be used to
 * call this function once for every monster.
 *
 * Note the "full" flag which requests that the "cdis" field be updated,
 * this is only needed when the monster (or the player) has moved.
 *
 * Every time a monster moves, we must call this function for that
 * monster, and update the distance, and the visibility. Every time
 * the player moves, we must call this function for every monster, and
 * update the distance, and the visibility. Whenever the player "state"
 * changes in certain ways ("blindness", "infravision", "telepathy",
 * and "see invisible"), we must call this function for every monster,
 * and update the visibility.
 *
 * Routines that change the "illumination" of a grid must also call this
 * function for any monster in that grid, since the "visibility" of some
 * monsters may be based on the illumination of their grid.
 *
 * Note that this function is called once per monster every time the
 * player moves. When the player is running, this function is one
 * of the primary bottlenecks, along with "update_view()" and the
 * "process_monsters()" code, so efficiency is important.
 *
 * Note the optimized "inline" version of the "distance()" function.
 *
 * A monster is "visible" to the player if (1) it has been detected
 * by the player, (2) it is close to the player and the player has
 * telepathy, or (3) it is close to the player, and in line of sight
 * of the player, and it is "illuminated" by some combination of
 * infravision, torch light, or permanent light (invisible monsters
 * are only affected by "light" if the player can see invisible).
 *
 * Monsters which are not on the current panel may be "visible" to
 * the player, and their descriptions will include an "offscreen"
 * reference. Currently, offscreen monsters cannot be targetted
 * or viewed directly, but old targets will remain set. XXX XXX
 *
 * The player can choose to be disturbed by several things, including
 * "disturb_move" (monster which is viewable moves in some way), and
 * "disturb_near" (monster which is "easily" viewable moves in some
 * way). Note that "moves" includes "appears" and "disappears".
 *
 */
void update_mon(mon_ptr mon, bool full)
{
    bool do_disturb = disturb_move;

    int d;

    /* Seen at all */
    bool flag = FALSE;
    bool fuzzy = FALSE;
    bool old_fuzzy = BOOL(mon->mflag2 & MFLAG2_FUZZY);
    mon_f lore_f[20] = {0}; /* Awkward: cf mon_lore_allow which requires ml and FUZZY be up to date */
    int  lore_ct = 0;       /* Historically, we duplicated checks here, but did so incorrectly */

    /* Seen by vision */
    bool easy = FALSE;

    /* Do disturb? */
    if (disturb_high)
    {
        if (mon->apparent_race->lore.kills.total && mon->apparent_race->alloc.lvl >= plr->lev)
            do_disturb = TRUE;
    }

    /* Compute distance */
    if (full)
    {
        d = point_fast_distance(cave->flow_pos, mon->pos);
        mon->cdis = d;

        if (mon->cdis <= 2 && plr_view(mon->pos))
            do_disturb = TRUE;
    }

    /* Extract distance */
    else
    {
        /* Extract the distance */
        d = mon->cdis;
    }

    /* done if player is not present */
    if (plr->dun_id != cave->id)
    {
        mon->mflag2 &= ~MFLAG2_FUZZY;
        mon->ml = FALSE;
        return;
    }

    /* Detected */
    if (mon->mflag2 & (MFLAG2_MARK))
        flag = TRUE;

    /* Nearby */
    if (d <= MAX_SIGHT)
    {
        if (plr->special_defense & KATA_MUSOU)
        {
            /* Detectable */
            flag = TRUE;
        }

        /* Basic Telepathy */
        else if (plr->telepathy)
        {
            /* Empty mind, no telepathy */
            if (mon_has_empty_mind(mon))
                lore_f[lore_ct++] = mon_lore_empty_mind;

            /* Weird mind, occasional telepathy */
            else if (mon_has_weird_mind(mon))
            {
                if (redraw_hack)
                    flag = mon->ml;
                else if (one_in_(10))
                {
                    flag = TRUE;
                    lore_f[lore_ct++] = mon_lore_weird_mind;
                    equip_learn_flag(OF_TELEPATHY);
                }
            }

            /* Normal mind, allow telepathy */
            else
            {
                flag = TRUE;
                equip_learn_flag(OF_TELEPATHY);
            }
            lore_f[lore_ct++] = mon_lore_smart;
            lore_f[lore_ct++] = mon_lore_stupid;
        }

        if (plr->esp_living && mon_is_living(mon))
        {
            flag = TRUE;
            lore_f[lore_ct++] = mon_lore_living;
            /*equip_learn_flag(OF_ESP_LIVING);*/
        }
        if (plr->esp_animal && mon_is_animal(mon))
        {
            flag = TRUE;
            lore_f[lore_ct++] = mon_lore_animal;
            equip_learn_flag(OF_ESP_ANIMAL);
        }
        if (plr->esp_undead && mon_is_undead(mon))
        {
            flag = TRUE;
            lore_f[lore_ct++] = mon_lore_undead;
            equip_learn_flag(OF_ESP_UNDEAD);
        }
        if (plr->esp_demon && mon_is_demon(mon))
        {
            flag = TRUE;
            lore_f[lore_ct++] = mon_lore_demon;
            equip_learn_flag(OF_ESP_DEMON);
        }
        if (plr->esp_orc && mon_is_orc(mon))
        {
            flag = TRUE;
            lore_f[lore_ct++] = mon_lore_orc;
            equip_learn_flag(OF_ESP_ORC);
        }
        if (plr->esp_troll && mon_is_troll(mon))
        {
            flag = TRUE;
            lore_f[lore_ct++] = mon_lore_troll;
            equip_learn_flag(OF_ESP_TROLL);
        }
        if (plr->esp_giant && mon_is_giant(mon))
        {
            flag = TRUE;
            lore_f[lore_ct++] = mon_lore_giant;
            equip_learn_flag(OF_ESP_GIANT);
        }
        if (plr->esp_dragon && mon_is_dragon(mon))
        {
            flag = TRUE;
            lore_f[lore_ct++] = mon_lore_dragon;
            equip_learn_flag(OF_ESP_DRAGON);
        }
        if (plr->esp_human && mon_is_human(mon))
        {
            flag = TRUE;
            lore_f[lore_ct++] = mon_lore_human;
            equip_learn_flag(OF_ESP_HUMAN);
        }
        if (plr->esp_evil && mon_is_evil(mon))
        {
            flag = TRUE;
            lore_f[lore_ct++] = mon_lore_evil;
            equip_learn_flag(OF_ESP_EVIL);
        }
        if (plr->esp_good && mon_is_good(mon))
        {
            flag = TRUE;
            lore_f[lore_ct++] = mon_lore_good;
            equip_learn_flag(OF_ESP_GOOD);
        }
        if (plr->esp_nonliving && mon_is_nonliving(mon))
        {
            flag = TRUE;
            lore_f[lore_ct++] = mon_lore_nonliving;
            equip_learn_flag(OF_ESP_NONLIVING);
        }
        if (plr->esp_unique && mon_is_fixed_unique(mon))
        {
            flag = TRUE;
            /*lore_f[lore_ct++] = mon_lore_fixed_unique;*/
            equip_learn_flag(OF_ESP_UNIQUE);
        }

        /* Warlock Pacts: Warlocks sense pact monsters at CL25 */
        if ( plr->pclass == CLASS_WARLOCK
          && plr->lev >= 25
          && warlock_is_pact_monster(mon->race) )
        {
            flag = easy = TRUE;
        }
        if ( plr->pclass == CLASS_DUELIST
          && plr->lev >= 25
          && mon == who_mon(plr->duelist_target) )
        {
            flag = easy = TRUE;
        }

        if (plr->esp_magical && mon_race_is_magical(mon->race))
            flag = TRUE;

        /* Normal line of sight, and not blind */
        if (!plr_tim_find(T_BLIND) && plr_view(mon->pos))
        {
            bool do_invisible = FALSE;
            bool do_cold_blood = FALSE;


            /* Snipers can see targets in darkness when they concentrate deeper */
            if (plr->concent >= CONCENT_RADAR_THRESHOLD)
            {
                /* Easy to see */
                easy = flag = TRUE;
            }

            /* Use "infravision" */
            if (d <= plr->see_infra)
            {
                /* Handle "cold blooded" monsters */
                if (mon_is_cold_blooded(mon) && !mon_auras_find(mon->race, GF_FIRE))
                {
                    /* Take note */
                    do_cold_blood = TRUE;
                }

                /* Handle "warm blooded" monsters */
                else
                {
                    /* XXX infravision is no longer "easy" */
                    flag = TRUE;
                }
            }

            /* Use "illumination" */
            if (plr_can_see(mon->pos))
            {
                /* Handle "invisible" monsters */
                if (mon_is_invisible(mon))
                {
                    /* Take note */
                    do_invisible = TRUE;

                    /* See invisible */
                    if (redraw_hack)
                    {
                        easy = !old_fuzzy;
                        flag = mon->ml;
                    }
                    /* Once spotted, and invisible sleeping monster remains spotted. Invisible
                     * monsters that never move likewise remain spotted. However, mobile monsters
                     * can hide, even if they didn't actually move this turn. */
                    else if ( mon->ml
                           && !old_fuzzy
                           && plr->see_inv /* rule out single round ?DetectInvisible */
                           && (mon_tim_find(mon, MT_SLEEP) || mon_never_move(mon)) )
                    {
                        easy = flag = TRUE;
                    }
                    else if (plr_see_invis(mon->race->alloc.lvl))
                    {
                        /* Easy to see */
                        easy = flag = TRUE;
                        equip_learn_flag(OF_SEE_INVIS);
                    }
                }

                /* Handle "normal" monsters */
                else
                {
                    /* Easy to see */
                    easy = flag = TRUE;
                }
            }

            /* Visible */
            if (flag)
            {
                if (is_original_ap(mon) && !plr_tim_find(T_HALLUCINATE))
                {
                    /* Memorize flags */
                    if (do_invisible) mon_lore_is_invisible(mon); /* XXX before mon disappears! */
                    if (do_cold_blood) lore_f[lore_ct++] = mon_lore_cold_blooded;
                }
            }
        }
    }

    if (plr->wizard)
        flag = TRUE;

    mon->mflag2 &= ~MFLAG2_FUZZY;

    /* The monster is now visible */
    if (flag)
    {
        bool old_ml = mon->ml;

        /* "easy" denotes direct illumination. Detection and telepathy are
         * generally "fuzzy" except for pets (you know their minds since you
         * control their minds) and certain privileged classes (wizards). */
        if (!easy && !plr->wizard_sight && !mon_is_pet(mon) && !plr->wizard)
        {
            fuzzy = TRUE;
            mon->mflag2 |= MFLAG2_FUZZY;
        }

        /* It was previously unseen, or previously fuzzy */
        if (!mon->ml || fuzzy != old_fuzzy)
        {
            int i;
            /* Mark as visible */
            mon->ml = TRUE;

            /* mon is now up to date for lore */
            for (i = 0; i < lore_ct; i++)
                lore_f[i](mon);

            /* Draw the monster */
            draw_pos(mon->pos);

            /* Update health bar as needed */
            check_mon_health_redraw(mon);

            mon_lore_sighting(mon); /* XXX used to track shadowers ... */

            if (!fuzzy && mon_is_horror(mon))
                sanity_blast(mon, FALSE);

            if (!fuzzy)
            {
                fear_update_m(mon);
                mon->mflag2 |= MFLAG2_LORE;
            }

            /* Disturb on appearance ... but not on change of "fuzziness" */
            if (disturb_near && !old_ml && plr_view(mon->pos))
            {
                if (town_no_disturb && plr_in_town() && mon->race->alloc.lvl == 0)
                {
                }
                else if (disturb_pets || mon_is_hostile(mon))
                    disturb(1, 0);
            }

            plr->window |= PW_MONSTER_LIST;
        }
    }

    /* The monster is not visible */
    else
    {
        /* It was previously seen */
        if (mon->ml)
        {
            /* Mark as not visible */
            mon->ml = FALSE;

            /* Erase the monster */
            draw_pos(mon->pos);

            /* Update health bar as needed */
            check_mon_health_redraw(mon);

            /* Disturb on disappearance */
            if (do_disturb)
            {
                if (disturb_pets || mon_is_hostile(mon))
                    disturb(1, 0);
            }
            plr->window |= PW_MONSTER_LIST;
        }
    }


    /* The monster is now easily visible */
    if (easy)
    {
        /* Change */
        if (!(mon->mflag & (MFLAG_VIEW)))
        {
            /* Mark as easily visible */
            mon->mflag |= (MFLAG_VIEW);

            /* Disturb on appearance */
            if (do_disturb)
            {
                if (disturb_pets || mon_is_hostile(mon))
                    disturb(1, 0);
            }
            plr->window |= PW_MONSTER_LIST;
        }
    }

    /* The monster is not easily visible */
    else
    {
        /* Change */
        if (mon->mflag & (MFLAG_VIEW))
        {
            /* Mark as not easily visible */
            mon->mflag &= ~(MFLAG_VIEW);

            /* Disturb on disappearance */
            if (do_disturb)
            {
                if (disturb_pets || mon_is_hostile(mon))
                    disturb(1, 0);
            }
            plr->window |= PW_MONSTER_LIST;
        }
    }
}


/*
 * This function simply updates all the (non-dead) monsters (see above).
 */
static void _update_mon_full(int id, mon_ptr mon) { update_mon(mon, TRUE); }
static void _update_mon(int id, mon_ptr mon) { update_mon(mon, FALSE); }
void update_monsters(bool full)
{
    if (full) dun_iter_mon(cave, _update_mon_full);
    else dun_iter_mon(cave, _update_mon);
}

/*
 * Hack -- the index of the summoning monster
 */
mon_ptr chameleon_change_mon = NULL;
static bool monster_hook_chameleon_lord(mon_race_ptr r_ptr)
{
    monster_type *m_ptr = chameleon_change_mon;
    monster_race *old_r_ptr = m_ptr->race;
    dun_cell_ptr  cell;

    if (!mon_race_is_unique(r_ptr)) return FALSE;
    if (mon_race_is_friendly(r_ptr)) return FALSE;
    if (mon_race_is_chameleon(r_ptr)) return FALSE;

    if (ABS(r_ptr->alloc.lvl - mon_race_parse("R.Chameleon")->alloc.lvl) > 5) return FALSE;

    if (mon_blows_find(r_ptr->blows, RBM_EXPLODE))
        return FALSE;

    cell = dun_cell_at(cave, m_ptr->pos);
    if (!cell_allow_mon_race(cell, r_ptr)) return FALSE;

    if (!mon_race_is_chameleon(old_r_ptr))
    {
        if (align_hostile(m_ptr->align, r_ptr->align)) return FALSE;
    }
    else if (who_is_mon(summon_specific_who))
    {
        mon_ptr master = who_mon(summon_specific_who);
        if (master && align_hostile(master->align, r_ptr->align)) return FALSE;
    }

    return TRUE;
}

static bool monster_hook_chameleon(mon_race_ptr r_ptr)
{
    monster_type *m_ptr = chameleon_change_mon;
    monster_race *old_r_ptr = m_ptr->race;
    dun_cell_ptr  cell;

    if (mon_race_is_unique(r_ptr)) return FALSE;
    if (mon_race_can_multiply(r_ptr)) return FALSE;
    if (mon_race_is_friendly(r_ptr)) return FALSE;
    if (mon_race_is_chameleon(r_ptr)) return FALSE;

    if (mon_blows_find(r_ptr->blows, RBM_EXPLODE))
        return FALSE;

    cell = dun_cell_at(cave, m_ptr->pos);
    if (!cell_allow_mon_race(cell, r_ptr)) return FALSE;

    if (!mon_race_is_chameleon(old_r_ptr))
    {
        if (align_hostile(old_r_ptr->align, r_ptr->align)) return FALSE;
    }
    else if (who_is_mon(summon_specific_who))
    {
        mon_ptr master = who_mon(summon_specific_who);
        if (master && align_hostile(master->align, r_ptr->align)) return FALSE;
    }

    assert(cave->type->id != D_SURFACE);
    return mon_alloc_dungeon(r_ptr);
}

void choose_new_monster(mon_ptr mon, bool born, mon_race_ptr race)
{
    int oldmaxhp;
    mon_race_ptr old_r_ptr = mon->race;
    char old_m_name[80];
    bool old_unique = FALSE;

    /* XXX monster_hook_chameleon won't work on the surface */
    if (mon->dun->type->id == D_SURFACE) return;

    if (mon_race_is_unique(old_r_ptr))
    {
        old_unique = TRUE;
        if (race && mon_race_is_(race, "R.chameleon"))
            race = mon_race_parse("R.Chameleon");
    }

    monster_desc(old_m_name, mon, 0);

    if (!race)
    {
        int level;

        chameleon_change_mon = mon;
        if (old_unique)
            mon_alloc_push_filter(monster_hook_chameleon_lord);
        else
            mon_alloc_push_filter(monster_hook_chameleon);

        if (old_unique) level = mon_race_parse("R.Chameleon")->alloc.lvl;
        else level = cave->difficulty;

        race = mon_alloc_choose(level);
        mon_alloc_pop_filter();

        chameleon_change_mon = NULL;

        /* Paranoia */
        if (!race) return;
    }

    mon->race = race; /* XXX mon_set_race */
    mon->align = mon->race->align; /* XXX mon_set_race */
    mon->apparent_race = mon->race; /* XXX mon_set_race */

    update_mon(mon, FALSE);
    draw_pos(mon->pos);

    if (old_r_ptr->light || old_r_ptr->lantern || race->light || race->lantern)
        plr->update |= PU_MON_LIGHT;

    if (mon_is_pet(mon)) check_pets_num_and_align();

    if (born) /* "being born", not "already born" (very poorly named) */
    {
        return;
    }

    if (mon->id == plr->riding)
    {
        char m_name[80];
        monster_desc(m_name, mon, 0);
        msg_format("Suddenly, %s transforms!", old_m_name);
        if (!mon_race_is_ridable(race))
            if (rakuba(0, TRUE)) msg_format("You have fallen from %s.", m_name);
    }

    /* Extract the monster base speed */
    mon->mspeed = get_mspeed(race);

    oldmaxhp = mon->max_maxhp;
    mon->max_maxhp = dice_roll(race->hp);
    mon->maxhp = (int)mon->maxhp * mon->max_maxhp / oldmaxhp;
    if (mon->maxhp < 1) mon->maxhp = 1;
    mon->hp = (int)mon->hp * mon->max_maxhp / oldmaxhp;
}


/*
 *  Hook for Tanuki
 */
static bool monster_hook_tanuki(mon_race_ptr r_ptr)
{

    if (mon_race_is_unique(r_ptr)) return FALSE;
    if (mon_race_can_multiply(r_ptr)) return FALSE;
    if (mon_race_is_friendly(r_ptr)) return FALSE;
    if (mon_race_is_chameleon(r_ptr)) return FALSE;
    if (mon_race_is_aquatic(r_ptr)) return FALSE;

    if (mon_blows_find(r_ptr->blows, RBM_EXPLODE))
        return FALSE;

    return mon_alloc_dungeon(r_ptr);
}


/*
 *  Set initial racial appearance of a monster for the Tanuki.
 */
static mon_race_ptr initial_r_appearance(mon_race_ptr race)
{
    int attempts = 1000;
    int min = MIN(cave->difficulty-5, 50);
    mon_race_ptr ap_race;

    if (!mon_race_is_(race, "q.tanuki"))
        return race;

    mon_alloc_push_filter(monster_hook_tanuki);
    while (--attempts)
    {
        ap_race = mon_alloc_choose(cave->difficulty + 10);
        if (!ap_race) break; /* bad filter ... */
        if (ap_race->alloc.lvl < min) continue;
        break;
    }
    mon_alloc_pop_filter();
    if (!ap_race) return race;
    return ap_race;
}


/*
 * Get initial monster speed
 */
s16b get_mspeed(monster_race *r_ptr)
{
    /* Extract the monster base speed */
    s16b mspeed = r_ptr->move.speed;

    /* Hack -- small racial variety */
    if (!mon_race_is_unique(r_ptr))
    {
        /* Allow some small variation per monster */
        int i = speed_to_energy(r_ptr->move.speed) / (one_in_(4) ? 3 : 10);
        if (i) mspeed += rand_spread(0, i);
    }

    return mspeed;
}

/*
 * Attempt to place a monster of the given race at the given location.
 *
 * To give the player a sporting chance, any monster that appears in
 * line-of-sight and is extremely dangerous can be marked as
 * "FORCE_SLEEP", which will cause them to be placed with low energy,
 * which often (but not always) lets the player move before they do.
 *
 * This routine refuses to place out-of-depth "FORCE_DEPTH" monsters.
 *
 * XXX XXX XXX Use special "here" and "dead" flags for unique monsters,
 * remove old "cur_num" and "max_num" fields.
 *
 * XXX XXX XXX Actually, do something similar for artifacts, to simplify
 * the "preserve" mode, and to make the "what artifacts" flag more useful.
 *
 * This is the only function which may place a monster in the dungeon,
 * except for the savefile loading code.
 */
mon_ptr place_monster_one(who_t who, point_t pos, mon_race_ptr race, mon_pack_ptr pack, u32b mode)
{
    dun_cell_ptr cell;
    mon_ptr mon = NULL, parent = who_mon(who);
    bool cloned = FALSE;

    /* Verify location */
    if (!dun_pos_interior(cave, pos)) return 0;
    cell = dun_cell_at(cave, pos);

    /* Paranoia */
    if (!race) return 0;

    if (!(mode & PM_IGNORE_TERRAIN))
    {
        if (cell->type == FEAT_PATTERN) return 0;
        if (!mon_race_can_enter(race, pos)) return 0;
    }

    /* Hack -- "unique" monsters must be "unique" */
    if (race->alloc.max_max_num && race->alloc.cur_num >= race->alloc.max_num)
    {
        /* Cannot create */
        if (mode & PM_ALLOW_CLONED)
            cloned = TRUE;
        else
            return 0;
    }

    if ( (race->alloc.flags & RFA_UNIQUE2)
      && race->alloc.cur_num >= 1 )
    {
        return 0;
    }

    /* Depth monsters may NOT be created out of depth */
    if ( (race->alloc.flags & RFA_FORCE_DEPTH)
      && cave->dun_lvl < race->alloc.lvl
      && (race->flagsx & RFX_QUESTOR) )
    {
        /* Cannot create */
        return 0;
    }

    if (floor_has_glyph_of_warding(cell))
    {
        if (randint1(BREAK_GLYPH) < (race->alloc.lvl+20))
        {
            if (cell->flags & CELL_MAP)
                msg_print("The rune of protection is broken!");

            cell->flags &= ~CELL_MAP;
            floor_remove_glyph(cell);
            note_pos(pos);
        }
        else return 0;
    }

    /* restrict shadowers */
    if (mon_race_is_unique(race) || mon_race_is_nazgul(race) || race->alloc.lvl < 10)
        mode &= ~PM_KAGE;

    /* Make a new monster */
    mon = dun_alloc_mon(cave);

    /* race */
    mon->race = race;
    mon->apparent_race = initial_r_appearance(mon->race);
    mon->align = mon->race->align; /* XXX s/b set at same time as race (XXX formalize this: mon_set_race) */
    if (parent) /* XXX move to mon_set_parent? */
    {
        /* a neutral minion aligns with master */
        if (!mon->align) mon->align = parent->align;
        /* a neutral master aligns with minion to prevent mis-aligned future minions (NEW) */
        else if (!parent->align) parent->align = mon->align;
    }


    /* No flags */
    mon->mflag = 0;
    mon->mflag2 = 0;
    if (mode & PM_ILLUSION)
        mon->mflag2 |= MFLAG2_ILLUSION;

    /* Hack -- Appearance transfer */
    if ((mode & PM_MULTIPLY) && parent && !is_original_ap(parent))
    {
        mon->apparent_race = parent->apparent_race;

        /* Hack -- Shadower spawns Shadower */
        if (parent->mflag2 & MFLAG2_KAGE)
            mon->mflag2 |= MFLAG2_KAGE;
    }
        
    if (cloned)
        add_flag(mon->smart, SM_CLONED);
    if (parent)
        add_flag(mon->smart, SM_SUMMONED);

    mon->cdis = point_fast_distance(pos, cave->flow_pos);

    mon->nickname = 0;

    mon->exp = 0;

    if (parent)
    {
        /* Your pet summons its pet. */
        if (mon_is_pet(parent))
            mode |= PM_FORCE_PET;
        mon_set_parent(mon, parent->id);
    }
    else
    {
        mon_set_parent(mon, 0);
    }

    if (mon_race_is_chameleon(race))
    {
        mon->pos = pos; /* XXX Hack: mon not born yet, so not placed yet. The hook needs
                           the position to pick a realistic race. */
        choose_new_monster(mon, TRUE, NULL);
        race = mon->race; /* keep race and mon->race in sync */
        mon->mflag2 |= MFLAG2_CHAMELEON;

        #if 0
        /* Hack - Set sub_align to neutral when the Chameleon Lord is generated as "GUARDIAN" */
        if (mon_race_is_unique(race) && !parent)
            mon->align = 0;
        #endif
    }
    else if ((mode & PM_KAGE) && !(mode & PM_FORCE_PET)) /* shadower */
    {
        mon->apparent_race = mon_race_parse("N.shadower");
        mon->mflag2 |= MFLAG2_KAGE;
    }

    if (mode & PM_NO_PET) mon->mflag2 |= MFLAG2_NOPET;

    /* Not visible */
    mon->ml = FALSE;

    /* Pet? */
    if (mode & PM_FORCE_PET)
    {
        assert(!pack); /* pets join a special plr_pack() ... cf set_pet_aux */
        set_pet(mon);
    }
    /* Friendly? */
    else if ( ( mon_race_is_friendly(race)
             || (mode & PM_FORCE_FRIENDLY)
             || (parent && mon_is_friendly(parent)) )
           && !(mode & PM_NO_FRIEND) )
    {
        if (allow_friendly_monster && !align_hostile(plr->align, mon->align))
        {
            if (parent && mon_is_temp_friendly(parent))
                set_temp_friendly(mon);
            else
                set_friendly(mon);
        }
    }
    else if (pack)
    {
        /* XXX packs should be all or nothing ... mon_pack_add will handle this, forcing
         * all monsters to align with the first monster added to the pack. */
    }
    else if ( mon_race_is_animal(race)
           && randint0(1000) < virtue_current(VIRTUE_NATURE) )
    {
        if (allow_friendly_monster && !align_hostile(plr->align, race->align))
            set_friendly(mon);
    }
    else if ( mon_race_is_knight(race)
           && randint0(1000) < virtue_current(VIRTUE_HONOUR) )
    {
        if (allow_friendly_monster && !align_hostile(plr->align, race->align))
            set_friendly(mon);
    }
    else if ( mon_race_is_char(race, 'A')
           && randint0(1000) < virtue_current(VIRTUE_FAITH) )
    {
        if (allow_friendly_monster && !align_hostile(plr->align, race->align))
            set_friendly(mon);
    }
    else if ( mon_race_is_demon(race)
           && randint0(1000) < -virtue_current(VIRTUE_FAITH) )
    {
        if (allow_friendly_monster && !align_hostile(plr->align, race->align))
            set_friendly(mon);
    }
    else if ( mon_race_is_undead(race)
           && randint0(1000) < virtue_current(VIRTUE_UNLIFE) )
    {
        if (allow_friendly_monster && !align_hostile(plr->align, race->align))
            set_friendly(mon);
    }

    /* Enforce sleeping if needed */
    if ((mode & PM_ALLOW_SLEEP) && race->move.sleep)
    {
        int x = race->move.sleep;
        mon_tim_add(mon, MT_SLEEP, 2*x + _1d(10*x));
    }

    /* roll hp */
    if ((mode & PM_ILLUSION) && mon_race_is_(race, "@.player"))
        mon->max_maxhp = plr->mhp;
    else
        mon->max_maxhp = dice_roll(race->hp);

    mon->maxhp = mon->max_maxhp;

    /* And start out fully healthy */
    if (mon_race_is_(race, "q.Wounded Bear"))
        mon->hp = mon->maxhp / 2;
    else mon->hp = mon->maxhp;


    /* Extract the monster base speed */
    mon->mspeed = get_mspeed(race);
    mon->ac_adj = 0;
    mon->mpower = 1000;
    if ((mode & PM_ILLUSION) && mon_race_is_(race, "@.player"))
    {
        mon->mspeed = plr->pspeed;
        mon->ac_adj = plr->ac;
    }

    if (mode & PM_HASTE) mon_tim_add(mon, T_FAST, 100);
    if (mode & PM_ILLUSION) mon_tim_add(mon, MT_ILLUSION, 50 + _1d(50));

    /* Give a random starting energy */
    mon->energy_need = ENERGY_NEED() - (s16b)randint0(100);

    if (race->light)
        plr->update |= PU_MON_LIGHT;
    else if (race->lantern && !mon_tim_find(mon, MT_SLEEP))
        plr->update |= PU_MON_LIGHT;

    /* Pre-roll the number of monsters drops so that Rogues may pick pockets.
       What the drops actually are will be determined later.  */
    mon_drop_init(mon);

    dun_place_mon(cave, mon, pos); /* XXX *after* birth is completed */
    if (pack)
    {
        assert(!mon_is_pet(mon));
        mon_pack_add(pack, mon);
    }

    /* Count the monsters on the level */
    real_r_ptr(mon)->alloc.cur_num++;

    /* Hack -- Count the number of "reproducers" */
    if (mon_race_can_multiply(race)) cave->breed_ct++;

    /* Hack -- Notice new multi-hued monsters */
    if (mon->apparent_race->display.flags & (RFD_ATTR_MULTI | RFD_SHAPECHANGER))
        shimmer_monsters = TRUE;

    if (mon->race->alloc.flags & RFA_WEB)
    {
        int pct = 25 + mon_lvl(mon)/5;
        int i;
        dun_place_web(cave, mon->pos);
        for (i = 0; i < 8; i++)
        {
            point_t pos = point_step(mon->pos, ddd[i]);
            if (_1d(100) <= pct)
                dun_place_web(cave, pos);
        }
    }

    if (plr->warning && (cave->flags & DF_GENERATED))
    {
        if (mon_race_is_unique(race))
        {
            cptr color;
            object_type *o_ptr;
            char o_name[MAX_NLEN];

            if (race->alloc.lvl > plr->lev + 30)
                color = "black";
            else if (race->alloc.lvl > plr->lev + 15)
                color = "purple";
            else if (race->alloc.lvl > plr->lev + 5)
                color = "deep red";
            else if (race->alloc.lvl > plr->lev - 5)
                color = "red";
            else if (race->alloc.lvl > plr->lev - 15)
                color = "pink";
            else
                color = "white";

            o_ptr = choose_warning_item();
            if (o_ptr)
            {
                object_desc(o_name, o_ptr, (OD_OMIT_PREFIX | OD_NAME_ONLY));
                msg_format("%s glows %s.", o_name, color);
                obj_learn_flag(o_ptr, OF_WARNING);
            }
            else
            {
                msg_format("An %s image forms in your mind.");
            }
        }
    }

    cell_accept_mon(cave, mon->pos, cell, mon); /* XXX */

    return mon;
}


/*
 *  improved version of scatter() for place monster
 */

#define MON_SCAT_MAXD 10

static point_t mon_scatter(mon_race_ptr race, point_t pos, int rad)
{
    point_t place[MON_SCAT_MAXD];
    int num[MON_SCAT_MAXD] = {0};
    int i;
    point_t p;

    if (rad >= MON_SCAT_MAXD)
        return point_create(-1, -1);

    for (p.x = pos.x - rad; p.x <= pos.x + rad; p.x++)
    {
        for (p.y = pos.y - rad; p.y <= pos.y + rad; p.y++)
        {
            int d;
            if (!dun_pos_interior(cave, p)) continue;
            if (!point_project(pos, p)) continue;
            if (!dun_allow_mon_at(cave, p)) continue;
            if (race)
            {
                dun_cell_ptr cell = dun_cell_at(cave, p);
                if (!cell_allow_mon_race(cell, race)) continue;
            }

            d = point_fast_distance(pos, p);
            if (d > rad) continue;

            num[d]++;
            if (one_in_(num[d]))
                place[d] = p;
        }
    }

    i = 0;
    while (i < MON_SCAT_MAXD && 0 == num[i])
        i++;
    if (i >= MON_SCAT_MAXD)
        return point_create(-1, -1);

    return place[i];
}


/*
 * Hack -- help pick an escort type
 */
static mon_ptr _leader;
static bool _friend_p(mon_race_ptr race)
{
    assert(_leader);
    if (_leader->race->alloc.flags & RFA_ESCORT)
    {
        if (race == _leader->race)
            return FALSE;
    }
    if (!mon_is_unique(_leader))
    {
        if (race->alloc.flags & RFA_ESCORT)
            return FALSE;
        if (mon_race_char(race) == mon_char(_leader))
        {
            if (race->alloc.lvl > _leader->race->alloc.lvl)
                return FALSE;
        }
    }
    if (mon_race_is_unique(race))
        return FALSE;
    if (align_hostile(_leader->align, race->align))
        return FALSE;
    if (mon_is_friendly(_leader))
    {
        if (align_hostile(plr->align, race->align)) /* XXX former threshold=1? */
            return FALSE;
    }

    if (mon_race_is_chameleon(_leader->race) && !mon_race_is_chameleon(race))
        return FALSE;
    return TRUE;
}

/*
 * Attempt to place a monster of the given race at the given location
 *
 * XXX We now return a pointer to the allocated monster. For packs, this
 * will always be the pack leader, or the first in a group of "friends".
 */
mon_ptr place_monster_aux(who_t who, point_t pos, mon_race_ptr race, u32b mode)
{
    mon_ptr       result = NULL;
    mon_pack_ptr  pack = NULL;

    if (!(mode & PM_NO_KAGE) && one_in_(333))
        mode |= PM_KAGE;

    if ((mode & PM_FORCE_PET) && (!allow_pets || plr->pclass == CLASS_PSION))
        mode &= (~PM_FORCE_PET);

    /* Place one monster, or fail */
    result = place_monster_one(who, pos, race, 0, mode);
    if (!result) return NULL;

    /* Friends and Escorts */
    if ((mode & PM_ALLOW_GROUP) && race->friends)
    {
        mon_rule_ptr rule;

        if (!mon_is_pet(result))
        {
            pack = dun_mgr_alloc_pack();

            mon_pack_add(pack, result);
            if (race->alloc.flags & RFA_ESCORT)
                pack->leader_id = result->id;
        }

        _leader = result; /* for _friend_p */
        mon_alloc_push_filter(_friend_p);
        for (rule = race->friends; rule; rule = rule->next)
        {
            int ct = mon_rule_amt(rule), i, n = 0;
            mon_race_ptr friend = NULL;
            for (i = 0; i < ct; i++)
            {
                if (!friend || !(rule->flags & MON_RULE_SAME))
                    friend = mon_rule_race(rule);
                if (friend)
                {
                    point_t mp;
                    if (pack) mp = mon_pack_scatter(pack, 4);
                    else mp = scatter(pos, 4);
                    if (place_monster_one(who_create_mon(result), mp, friend, pack, mode & mon_rule_mode(rule)))
                        n++;
                }
            }
            if (n && (rule->flags & MON_RULE_STOP))
                break;
        }
        mon_alloc_pop_filter();
        _leader = NULL;
        if (pack && mon_pack_count(pack) > 1)
            mon_pack_choose_ai(pack);
    }

    /* Give uniques variable AI strategies. We do this as a hack, using the
       existing pack code, by creating a "pack of 1".
                                     v---- Mercy!*/
    if (mon_race_is_unique(race) && !(race->flagsx & RFX_QUESTOR) && !(race->alloc.flags & RFA_GUARDIAN) && !(race->flagsx & RFX_GUARDIAN))
    {
        if (!pack && !mon_is_pet(result))
        {
            pack = dun_mgr_alloc_pack();
            mon_pack_add(pack, result);
            mon_pack_choose_ai(pack);
        }
    }

    return result;
}

static bool mon_alloc_summon(mon_race_ptr race)
    { return summon_specific_okay(race); }

/*
 * Hack -- attempt to place a monster at the given location
 *
 * Attempt to find a monster appropriate to the "level"
 */
mon_ptr place_monster(point_t pos, int level, u32b mode)
{
    mon_race_ptr race;

    if (mode & PM_RING_BEARER)
    {
        summon_specific_type = SUMMON_RING_BEARER;
        mon_alloc_push_filter(mon_alloc_summon);
    }
    else
    {
        dun_cell_ptr cell = dun_cell_at(cave, pos);
        mon_race_p filter = mon_alloc_cell_p(cell);
        mon_alloc_push_filter(filter);
    }

    race = mon_alloc_choose(level);
    mon_alloc_pop_filter();

    if (!race) return FALSE;

    /* Hack: Warlock pact monsters are occasionally friendly */
    if (plr->pclass == CLASS_WARLOCK)
    {
        if ( warlock_is_pact_monster(race)
          && !(mode & PM_QUESTOR) /* RFX_QUESTOR is *not* set for non-unique quest monsters */
          && one_in_(12 - plr->lev/5) )
        {
            mode |= PM_FORCE_FRIENDLY;
        }
    }

    return place_monster_aux(who_create_null(), pos, race, mode);
}


#ifdef MONSTER_HORDES

bool alloc_horde(point_t pos, int level)
{
    point_t last = pos;
    mon_race_ptr race = NULL;
    mon_ptr mon = NULL;
    int attempts;
    dun_cell_ptr cell = dun_cell_at(cave, pos);
    mon_race_p filter = mon_alloc_cell_p(cell);

    mon_alloc_push_filter(filter);
    race = mon_alloc_choose_aux(level, GMN_NO_UNIQUES);
    mon_alloc_pop_filter();
    if (!race) return FALSE;

    mon = place_monster_aux(who_create_null(), pos, race, 0);
    if (!mon) return FALSE;

    if (mon->mflag2 & MFLAG2_CHAMELEON) race = mon->race;
    summon_kin_type = mon_race_char(race);

    for (attempts = _3d(3); attempts; attempts--)
    {
        point_t next = scatter(last, 5);
        summon_specific(who_create_mon(mon), next, cave->dun_lvl, SUMMON_KIN, 0);
        last = next;
    }

    return TRUE;
}

#endif /* MONSTER_HORDES */

/*
 * Attempt to allocate a random monster in the dungeon.
 *
 * Place the monster at least "dis" distance from the player.
 *
 * Use "slp" to choose the initial "sleep" status
 *
 * Use "cave->difficulty" for the monster level
 */
bool alloc_monster(int dis, u32b mode)
{
    rect_t rect = rect_interior(cave->rect);
    point_t pos;
    int     attempts_left = 10000;

    /* Find a legal, distant, unoccupied, space */
    while (attempts_left--)
    {
        pos = rect_random_point(rect);

        if (!dun_allow_mon_at(cave, pos)) continue;
        if (plr_distance(pos) > dis) break;
    }

    if (!attempts_left)
        return FALSE;


#ifdef MONSTER_HORDES
    /* XXX This mechanic is very fragile. On balance it is good, but occasionally it
     * finds some accidental oddity in r_info. Examples include hordes of Flying Polyps (l)
     * on DL30, hordes of Lesser Hell Beasts (U) in the Labyrinth, and hordes of deep
     * dragons (D) in the early DL30's. A pack of wide awake Dracoliches, AMHDs, Great Crystal
     * Drakes and Ethereal Dragons is not welcome the first time a player stretches towards
     * stat gain depths. I would suggest that XXXget_mon_num() have an option to disallow
     * OoD monsters so that the inital choice for the horde is reasonable. Also, the kin
     * monsters should either disable the +5 level boost, the random OoD stuff in XXXget_mon_num(),
     * or both. Pending these, I'm disabling this mechanic below DL40. XXX */
    if ( cave->dun_lvl >= 40
      && _1d(5000) <= cave->dun_lvl )
    {
        if (alloc_horde(pos, cave->difficulty))
            return TRUE;
    }
    else
    {
#endif /* MONSTER_HORDES */

        /* Attempt to place the monster, allow groups */
        if (place_monster(pos, cave->difficulty, mode | PM_ALLOW_GROUP)) return TRUE;

#ifdef MONSTER_HORDES
    }
#endif /* MONSTER_HORDES */

    /* Nope */
    return (FALSE);
}




/*
 * Hack -- help decide if a monster race is "okay" to summon
 */
static bool summon_specific_okay(mon_race_ptr r_ptr)
{

    /* Hack - Only summon dungeon monsters */
    if (!mon_hook_dungeon(r_ptr->id)) return (FALSE);

    if (summon_ring_bearer)
    {
        if (!mon_is_type(r_ptr, SUMMON_RING_BEARER))
            return FALSE;
    }

    /* Hack -- identify the summoning monster */
    if (who_is_mon(summon_specific_who))
    {
        mon_ptr master = who_mon(summon_specific_who);
        if (align_hostile(master->align, r_ptr->align)) return FALSE;
    }
    /* Use the player's alignment */
    else if (who_is_plr(summon_specific_who))
    {
        if (align_hostile(plr->align, r_ptr->align))
        {
            if (!one_in_(ABS(plr->align) / 2 + 1)) return FALSE;
        }
    }

    /* Hack -- no specific type specified */
    if (!summon_specific_type) return (TRUE);

    if (!summon_unique_okay && (mon_race_is_unique(r_ptr) || mon_race_is_nazgul(r_ptr))) return FALSE;

    if (who_is_plr(summon_specific_who) &&
        (mon_race_is_unique(r_ptr) || mon_race_is_nazgul(r_ptr)) &&
        align_hostile(plr->align, r_ptr->align))
        return FALSE;

    return (summon_specific_aux(r_ptr));
}


/*
 * Place a monster (of the specified "type") near the given
 * location. Return TRUE if a monster was actually summoned.
 *
 * We will attempt to place the monster up to 10 times before giving up.
 *
 * Note: SUMMON_UNIQUE and SUMMON_AMBERITES will summon Unique's
 * Note: SUMMON_HI_UNDEAD and SUMMON_HI_DRAGON may summon Unique's
 * Note: None of the other summon codes will ever summon Unique's.
 *
 * This function has been changed. We now take the "monster level"
 * of the summoning monster as a parameter, and use that, along with
 * the current dungeon level, to help determine the level of the
 * desired monster. Note that this is an upper bound, and also
 * tends to "prefer" monsters of that level. Currently, we use
 * the average of the dungeon and monster levels, and then add
 * five to allow slight increases in monster power.
 *
 * Note that we use the new "monster allocation table" creation code
 * to restrict the "XXXget_mon_num()" function to the set of "legal"
 * monsters, making this function much faster and more reliable.
 *
 * Note that this function may not succeed, though this is very rare.
 */
bool summon_specific(who_t who, point_t tgt_pos, int lev, int type, u32b mode)
{
    point_t pos = tgt_pos;
    mon_race_ptr race = NULL;
    bool mon_summoned = FALSE;
    bool boss_summoned = FALSE;
    int options = GMN_DEFAULT;

    if (who_is_mon(who) && mon_spell_current() && mon_spell_current()->spell->id.type == MST_SUMMON)
    {
        mon_summoned = TRUE;
        if (who_mon(who)->race->alloc.flags & RFA_GUARDIAN)
            boss_summoned = TRUE;
    }

    if (who_is_mon(who) && plr_block_summon(who_mon(who)))
    {
        msg_format("The summoning is blocked!");
        return FALSE;
    }

    pos = mon_scatter(NULL, tgt_pos, 2);
    if (!dun_pos_interior(cave, pos)) return FALSE;

    /* Save the summoner */
    summon_specific_who = who;

    /* Save the "summon" type */
    summon_specific_type = type;

    if (who_is_plr(who) && (type == SUMMON_BIZARRE1 || type == SUMMON_THIEF))
        options |= GMN_IGNORE_MAX_LEVEL;

    summon_unique_okay = (mode & PM_ALLOW_UNIQUE) ? TRUE : FALSE;
    summon_cloned_okay = (mode & PM_ALLOW_CLONED) ? TRUE : FALSE;
    summon_wall_scummer = (mode & PM_WALL_SCUMMER) && one_in_(2) ? TRUE : FALSE;
    summon_ring_bearer = (mode & PM_RING_BEARER) ? TRUE : FALSE;

    /* XXX Experimental: Nerf summons a bit by removing "max_lvl" restrictions
     * for monster and player summoning (mon->race->alloc.max_lvl) ... but not
     * for Dungeon Masters (esp Morgoth and The Serpent). XXX */
    if (!who_is_null(summon_specific_who) && !boss_summoned)
        options |= GMN_IGNORE_MAX_LEVEL;

    lev = (cave->difficulty + lev)/2;
    if (mon_summoned)
        lev += 5; /* XXX Historical */

    mon_alloc_push_filter(mon_alloc_summon);
   {dun_cell_ptr cell = dun_cell_at(cave, pos);
    mon_race_p filter = mon_alloc_cell_p(cell);
    mon_alloc_push_filter(filter);}
    race = mon_alloc_choose_aux(lev, options);
    if (!race && summon_wall_scummer)
    {
        summon_wall_scummer = FALSE;
        race = mon_alloc_choose_aux(lev, options);
    }
    if (!race && summon_ring_bearer)
    {
        summon_ring_bearer = FALSE;
        race = mon_alloc_choose_aux(lev, options);
    }
    mon_alloc_pop_filter();
    mon_alloc_pop_filter();

    summon_cloned_okay = FALSE; /* This is a hack for RF6_S_UNIQUE ... XXXget_mon_num() is much more widely used, however! */
                                /* place_monster_aux() will now handle setting the unique as "cloned" if appropriate */
    /* Handle failure */
    if (!race)
    {
        summon_specific_type = 0;
        summon_specific_who = who_create_null();
        return FALSE;
    }

    if ((type == SUMMON_BLUE_HORROR) || (type == SUMMON_DAWN)) mode |= PM_NO_KAGE;

    if (mon_summoned && plr->cult_of_personality)
    {
        int pl = plr->lev + adj_stat_save[plr->stat_ind[A_CHR]];
        if (plr_view(pos) && one_in_(2) && _1d(pl) >= _1d(race->alloc.lvl))
        {
            mode |= PM_FORCE_FRIENDLY;
            if (_1d(pl) >= _1d(race->alloc.lvl))
                mode |= PM_FORCE_PET;
        }
    }

    /* Attempt to place the monster (awake, allow groups) */
    if (!place_monster_aux(who, pos, race, mode))
    {
        summon_specific_type = 0;
        summon_specific_who = who_create_null();
        return FALSE;
    }

    summon_specific_type = 0;
    summon_specific_who = who_create_null();
    /* Success */
    return TRUE;
}

/* A "dangerous" function, creates a pet of the specified type */
static bool _allow_summon_named(mon_race_ptr race)
{
    if (mon_race_is_(race, "@.player")) return TRUE;
    if ((race->alloc.flags & RFA_GUARDIAN) && !plr->wizard) return FALSE;
    if ((race->flagsx & RFX_GUARDIAN) && !plr->wizard) return FALSE;
    if (race->alloc.max_max_num && race->alloc.cur_num >= race->alloc.max_num) return FALSE;
    return TRUE;
}
mon_ptr summon_named_creature(who_t who, point_t tgt_pos, mon_race_ptr race, u32b mode)
{
    mon_ptr result = NULL;
    point_t pos;

    if (!race) return FALSE;
    if (who_is_mon(who) && plr_block_summon(who_mon(who)))
    {
        msg_format("The summoning is blocked!");
        return FALSE;
    }
    pos = mon_scatter(NULL, tgt_pos, 2);
    if (!dun_pos_interior(cave, pos)) return FALSE;

    if (_allow_summon_named(race))
        result = place_monster_aux(who, pos, race, mode | PM_NO_KAGE);

    return result;
}


/*
 * Let the given monster attempt to reproduce.
 *
 * Note that "reproduction" REQUIRES empty space.
 */
mon_ptr multiply_monster(mon_ptr parent, bool clone, u32b mode)
{
    mon_ptr child;

    point_t pos = mon_scatter(parent->race, parent->pos, 2);
    if (!dun_pos_interior(cave, pos)) return NULL;

    if (parent->mflag2 & MFLAG2_NOPET) mode |= PM_NO_PET;

    /* Create a new monster (awake, no groups) */
    child = place_monster_aux(who_create_mon(parent), pos, parent->race, (mode | PM_NO_KAGE | PM_MULTIPLY));
    if (!child)
        return NULL;

    /* Hack -- Transfer "clone" flag */
    if (clone || have_flag(parent->smart, SM_CLONED))
    {
        add_flag(child->smart, SM_CLONED);
        child->mflag2 |= MFLAG2_NOPET;
    }

    return child;
}





/*
 * Dump a message describing a monster's reaction to damage
 *
 * Technically should attempt to treat "Beholder"'s as jelly's
 */
void message_pain(int m_idx, int dam)
{
    long oldhp, newhp, tmp;
    int percentage;

    monster_type *m_ptr = dun_mon(cave, m_idx);
    monster_race *r_ptr = m_ptr->race;

    char m_name[80];


    /* Get the monster name */
    monster_desc(m_name, m_ptr, 0);

    /* Notice non-damage */
    if (dam == 0)
    {
        msg_format("%^s is unharmed.", m_name);

        return;
    }

    /* Note -- subtle fix -CFT */
    newhp = (long)(m_ptr->hp);
    oldhp = newhp + (long)(dam);
    tmp = (newhp * 100L) / oldhp;
    percentage = (int)(tmp);


    /* Mushrooms, Eyes, Jellies, Molds, Vortices, Worms, Quylthulgs */
    if (mon_race_is_char_ex(r_ptr, ",ejmvwQ"))
    {
        if (percentage > 95)
            msg_format("%^s barely notices.", m_name);
        else if (percentage > 75)
            msg_format("%^s flinches.", m_name);
        else if (percentage > 50)
            msg_format("%^s squelches.", m_name);
        else if (percentage > 35)
            msg_format("%^s quivers in pain.", m_name);
        else if (percentage > 20)
            msg_format("%^s writhes about.", m_name);
        else if (percentage > 10)
            msg_format("%^s writhes in agony.", m_name);
        else
            msg_format("%^s jerks limply.", m_name);

    }


    /* Fish */
    else if (mon_race_is_char_ex(r_ptr, "l"))
    {
        if (percentage > 95)
            msg_format("%^s barely notices.", m_name);
        else if (percentage > 75)
            msg_format("%^s flinches.", m_name);
        else if (percentage > 50)
            msg_format("%^s hesitates.", m_name);
        else if (percentage > 35)
            msg_format("%^s quivers in pain.", m_name);
        else if (percentage > 20)
            msg_format("%^s writhes about.", m_name);
        else if (percentage > 10)
            msg_format("%^s writhes in agony.", m_name);
        else
            msg_format("%^s jerks limply.", m_name);
    }


    /* Golems, Walls, Doors, Stairs */
    else if (mon_race_is_char_ex(r_ptr, "g#+<>"))
    {
        if (percentage > 95)
            msg_format("%^s ignores the attack.", m_name);
        else if (percentage > 75)
            msg_format("%^s shrugs off the attack.", m_name);
        else if (percentage > 50)
            msg_format("%^s roars thunderously.", m_name);
        else if (percentage > 35)
            msg_format("%^s rumbles.", m_name);
        else if (percentage > 20)
            msg_format("%^s grunts.", m_name);
        else if (percentage > 10)
            msg_format("%^s hesitates.", m_name);
        else
            msg_format("%^s crumples.", m_name);
    }


    /* Snakes, Hydrae, Reptiles, Mimics */
    else if (mon_race_is_char_ex(r_ptr, "JMR") || !isalpha(mon_race_char(r_ptr)))
    {
        if (percentage > 95)
            msg_format("%^s barely notices.", m_name);
        else if (percentage > 75)
            msg_format("%^s hisses.", m_name);
        else if (percentage > 50)
            msg_format("%^s rears up in anger.", m_name);
        else if (percentage > 35)
            msg_format("%^s hisses furiously.", m_name);
        else if (percentage > 20)
            msg_format("%^s writhes about.", m_name);
        else if (percentage > 10)
            msg_format("%^s writhes in agony.", m_name);
        else
            msg_format("%^s jerks limply.", m_name);
    }


    /* Felines */
    else if (mon_race_is_char_ex(r_ptr, "f"))
    {
        if (percentage > 95)
            msg_format("%^s shrugs off the attack.", m_name);
        else if (percentage > 75)
            msg_format("%^s roars.", m_name);
        else if (percentage > 50)
            msg_format("%^s growls angrily.", m_name);
        else if (percentage > 35)
            msg_format("%^s hisses with pain.", m_name);
        else if (percentage > 20)
            msg_format("%^s mewls in pain.", m_name);
        else if (percentage > 10)
            msg_format("%^s hisses in agony.", m_name);
        else
            msg_format("%^s mewls pitifully.", m_name);
    }


    /* Ants, Centipedes, Flies, Insects, Beetles, Spiders */
    else if (mon_race_is_char_ex(r_ptr, "acFIKS"))
    {
        if (percentage > 95)
            msg_format("%^s ignores the attack.", m_name);
        else if (percentage > 75)
            msg_format("%^s chitters.", m_name);

        else if (percentage > 50)
            msg_format("%^s scuttles about.", m_name);

        else if (percentage > 35)
            msg_format("%^s twitters.", m_name);

        else if (percentage > 20)
            msg_format("%^s jerks in pain.", m_name);

        else if (percentage > 10)
            msg_format("%^s jerks in agony.", m_name);

        else
            msg_format("%^s twitches.", m_name);

    }


    /* Birds */
    else if (mon_race_is_char_ex(r_ptr, "B"))
    {
        if (percentage > 95)
            msg_format("%^s chirps.", m_name);

        else if (percentage > 75)
            msg_format("%^s twitters.", m_name);

        else if (percentage > 50)
            msg_format("%^s squawks.", m_name);

        else if (percentage > 35)
            msg_format("%^s chatters.", m_name);

        else if (percentage > 20)
            msg_format("%^s jeers.", m_name);

        else if (percentage > 10)
            msg_format("%^s flutters about.", m_name);

        else
            msg_format("%^s squeaks.", m_name);

    }


    /* Dragons, Demons, High Undead */
    else if (mon_race_is_char_ex(r_ptr, "duDLUW"))
    {
        if (percentage > 95)
            msg_format("%^s ignores the attack.", m_name);

        else if (percentage > 75)
            msg_format("%^s flinches.", m_name);

        else if (percentage > 50)
            msg_format("%^s hisses in pain.", m_name);

        else if (percentage > 35)
            msg_format("%^s snarls with pain.", m_name);

        else if (percentage > 20)
            msg_format("%^s roars with pain.", m_name);

        else if (percentage > 10)
            msg_format("%^s gasps.", m_name);

        else
            msg_format("%^s snarls feebly.", m_name);

    }


    /* Skeletons */
    else if (mon_race_is_char_ex(r_ptr, "s"))
    {
        if (percentage > 95)
            msg_format("%^s ignores the attack.", m_name);

        else if (percentage > 75)
            msg_format("%^s shrugs off the attack.", m_name);

        else if (percentage > 50)
            msg_format("%^s rattles.", m_name);

        else if (percentage > 35)
            msg_format("%^s stumbles.", m_name);

        else if (percentage > 20)
            msg_format("%^s rattles.", m_name);

        else if (percentage > 10)
            msg_format("%^s staggers.", m_name);

        else
            msg_format("%^s clatters.", m_name);

    }


    /* Zombies */
    else if (mon_race_is_char_ex(r_ptr, "z"))
    {
        if (percentage > 95)
            msg_format("%^s ignores the attack.", m_name);

        else if (percentage > 75)
            msg_format("%^s shrugs off the attack.", m_name);

        else if (percentage > 50)
            msg_format("%^s groans.", m_name);

        else if (percentage > 35)
            msg_format("%^s moans.", m_name);

        else if (percentage > 20)
            msg_format("%^s hesitates.", m_name);

        else if (percentage > 10)
            msg_format("%^s grunts.", m_name);

        else
            msg_format("%^s staggers.", m_name);

    }


    /* Ghosts */
    else if (mon_race_is_char_ex(r_ptr, "G"))
    {
        if (percentage > 95)
            msg_format("%^s ignores the attack.", m_name);

        else if (percentage > 75)
            msg_format("%^s shrugs off the attack.", m_name);

        else if (percentage > 50)
            msg_format("%^s moans.", m_name);

        else if (percentage > 35)
            msg_format("%^s wails.", m_name);

        else if (percentage > 20)
            msg_format("%^s howls.", m_name);

        else if (percentage > 10)
            msg_format("%^s moans softly.", m_name);

        else
            msg_format("%^s sighs.", m_name);

    }


    /* Dogs and Hounds */
    else if (mon_race_is_char_ex(r_ptr, "CZ"))
    {
        if (percentage > 95)
            msg_format("%^s shrugs off the attack.", m_name);
        else if (percentage > 75)
            msg_format("%^s snarls with pain.", m_name);
        else if (percentage > 50)
            msg_format("%^s yelps in pain.", m_name);
        else if (percentage > 35)
            msg_format("%^s howls in pain.", m_name);
        else if (percentage > 20)
            msg_format("%^s howls in agony.", m_name);
        else if (percentage > 10)
            msg_format("%^s writhes in agony.", m_name);
        else
            msg_format("%^s yelps feebly.", m_name);

    }

    /* One type of monsters (ignore,squeal,shriek) */
    else if (mon_race_is_char_ex(r_ptr, "Xbilqrt"))
    {
        if (percentage > 95)
            msg_format("%^s ignores the attack.", m_name);
        else if (percentage > 75)
            msg_format("%^s grunts with pain.", m_name);
        else if (percentage > 50)
            msg_format("%^s squeals in pain.", m_name);
        else if (percentage > 35)
            msg_format("%^s shrieks in pain.", m_name);
        else if (percentage > 20)
            msg_format("%^s shrieks in agony.", m_name);
        else if (percentage > 10)
            msg_format("%^s writhes in agony.", m_name);
        else
            msg_format("%^s cries out feebly.", m_name);

    }

    /* Another type of monsters (shrug,cry,scream) */
    else
    {
        if (percentage > 95)
            msg_format("%^s shrugs off the attack.", m_name);
        else if (percentage > 75)
            msg_format("%^s grunts with pain.", m_name);
        else if (percentage > 50)
            msg_format("%^s cries out in pain.", m_name);
        else if (percentage > 35)
            msg_format("%^s screams in pain.", m_name);
        else if (percentage > 20)
            msg_format("%^s screams in agony.", m_name);
        else if (percentage > 10)
            msg_format("%^s writhes in agony.", m_name);
        else
            msg_format("%^s cries out feebly.", m_name);

    }
}


/*
 * Learn about an "observed" resistance.
 */
void mon_smart_learn(mon_ptr mon, int what)
{
    /* Not allowed to learn */
    if (!smart_learn) return;

    /* Too stupid to learn anything */
    if (mon_is_stupid(mon)) return;

    /* Not intelligent, only learn sometimes */
    if (!mon_is_smart(mon) && one_in_(2)) return;

    /* Analyze the knowledge */
    add_flag(mon->smart, what);
}
