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
    int skill = p_ptr->skills.srh;
    for (i = 0; i < p_ptr->see_inv; i++)
    {
        if (randint0(65 + rlev/2) < 15 + skill)
            return TRUE;
    }
    return FALSE;
}

monster_type *mon_get_parent(monster_type *m_ptr)
{
    monster_type *result = NULL;
    if (m_ptr->parent_m_idx)
    {
        result = dun_mon(cave, m_ptr->parent_m_idx);
        if (mon_is_dead(result))
            result = NULL;
    }
    return result;
}

void mon_set_parent(monster_type *m_ptr, int pm_idx)
{
    m_ptr->parent_m_idx = pm_idx;
}

/*
 * Set the target of counter attack
 */
void set_target(mon_ptr mon, point_t pos)
{
    mon->target = pos;
}


/*
 * Reset the target of counter attack
 */
void reset_target(mon_ptr mon)
{
    mon->target.x = 0;
    mon->target.y = 0;
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
    point_t pos = mon->pos;
    mon_race_ptr race = mon_race(mon);

    /* Hack -- count the number of "reproducers" */
    if (race->flags2 & RF2_MULTIPLY) cave->breed_ct--;

    mon_set_parent(mon, 0);

    if (mon->id == target_who) target_who = 0;
    if (mon->id == p_ptr->health_who) health_track(0);
    if (pet_t_m_idx == mon->id) pet_t_m_idx = 0;
    if (riding_t_m_idx == mon->id) riding_t_m_idx = 0;
    if (p_ptr->riding == mon->id) p_ptr->riding = 0;
    if (p_ptr->pclass == CLASS_DUELIST && p_ptr->duelist_target_idx == mon->id)
    {
        p_ptr->duelist_target_idx = 0;
        p_ptr->redraw |= PR_STATUS;
    }
    if (mon_is_pet(mon)) check_pets_num_and_align(mon, FALSE);

    if (race->flags7 & (RF7_LITE_MASK | RF7_DARK_MASK))
        p_ptr->update |= (PU_MON_LITE);

    p_ptr->window |= PW_MONSTER_LIST;

    dun_delete_mon(cave, mon->id);
    lite_pos(pos); /* it feels icky to use mon->pos here ... */
}


/*
 * Delete the monster, if any, at a given location
 */
void delete_monster_at(point_t pos)
{
    mon_ptr mon;
    if (!dun_pos_interior(cave, pos)) return;
    mon = mon_at(pos);
    if (mon) delete_monster(mon);
}

void pack_choose_ai(int m_idx)
{
    mon_ptr mon = dun_mon(cave, m_idx);
    mon_race_ptr race = mon_race(mon);
    mon_pack_ptr pack;

    if (!mon->pack_idx) return;
    pack = pack_info_ptr(m_idx);

    pack->ai = AI_SEEK; /* paranoia ... make sure something gets chosen! */
    if (pack->count == 1) /* Uniques can now come in packs of 1 for variable AI */
    {   /*      v--- Artemis has no attacks ... probably a bug I should fix later */
        int t = mon_can_attack(mon) ? 100 : 25;
        int p = randint0(t);

        if (p < 5 && mon_has_worthy_attack_spell(mon))
            pack->ai = AI_SHOOT;
        else if (p < 15 && mon_has_summon_spell(mon))
        {
            /* Lure the player into an open room in order to surround
                with evil summons! */
            pack->ai = AI_LURE;
        }
        else if ((p < 25 && mon_has_worthy_attack_spell(mon)) || !mon_can_attack(mon))
        {
            /* Hang back and pelt the player from a distance */
            pack->ai = AI_MAINTAIN_DISTANCE;
            pack->distance = 5;
        }
        else
            pack->ai = AI_SEEK;
    }
    else if (race->flags3 & RF3_ANIMAL)
    {
        switch(randint1(10))
        {
        case 1: case 2: case 3:
            pack->ai = AI_SEEK;
            break;
        case 4:
            if (mon_race_has_worthy_attack_spell(race))
                pack->ai = AI_SHOOT;
            else
                pack->ai = AI_LURE;
            break;
        default:
            pack->ai = AI_LURE;
            break;
        }
    }
    else
    {
        switch(randint1(10))
        {
        case 1: case 2: case 3: case 4: case 5: case 6:
            pack->ai = AI_SEEK;
            break;
        case 7: case 8:
            pack->ai = AI_LURE;
            break;
        case 9:
            if (pack->leader_idx)
            {
                pack->ai = AI_GUARD_MON;
                pack->guard_idx = pack->leader_idx;
            }
            else if (mon_race_has_attack_spell(race))
            {
                pack->ai = AI_GUARD_POS;
                pack->guard_x = mon->pos.x;
                pack->guard_y = mon->pos.y;
            }
            else
                pack->ai = AI_LURE;
            break;
        case 10:
            if (mon_race_has_worthy_attack_spell(race))
                pack->ai = AI_SHOOT;
            else
                pack->ai = AI_SEEK;
            break;
        }
    }
}

void pack_on_damage_monster(int m_idx)
{
    mon_ptr mon = dun_mon(cave, m_idx);
    mon_pack_ptr pack;

    if (!mon->pack_idx) return;
    pack = dun_mgr_pack(mon->pack_idx);
    if ( pack->leader_idx == m_idx
      && pack->ai != AI_SEEK
      && one_in_(3) )
    {
        pack->ai = AI_SEEK;
    }
}

void pack_on_slay_monster(int m_idx)
{
    mon_ptr mon = dun_mon(cave, m_idx);
    mon_pack_ptr pack;

    if (!mon->pack_idx) return;
    pack = dun_mgr_pack(mon->pack_idx);
    /* pack->count-- will happen in _mon_free (dun.c) */
    assert(pack);
    if (!pack) /* paranoia */
    {
        mon->pack_idx = 0;
        return;
    }
    if (pack->ai != AI_FEAR)
    {
        if (pack->leader_idx == m_idx)
        {
            pack->ai = AI_FEAR;
            pack->leader_idx = 0;
        }
        else if (one_in_((pack->count - 1) * (pack->leader_idx ? 2 : 1)))
            pack->ai = AI_FEAR;
        else if (pack->ai == AI_LURE && one_in_(2))
            pack->ai = AI_SEEK;
        else if (pack->ai == AI_SHOOT && one_in_(3))
            pack->ai = AI_SEEK;
    }
}

pack_info_t *pack_info_ptr(int m_idx)
{
    mon_ptr mon;
    if (!m_idx) return NULL;
    mon = dun_mon(cave, m_idx);
    if (!mon->pack_idx) return NULL;
    return dun_mgr_pack(mon->pack_idx);
}


/*
 * Hack -- the "type" of the current "summon specific"
 */
int summon_specific_type = 0;
static bool summon_specific_okay(int r_idx);


/*
 * Hack -- the index of the summoning monster
 */
int summon_specific_who = 0;


static bool summon_unique_okay = FALSE;
static bool summon_cloned_okay = FALSE;
static bool summon_wall_scummer = FALSE;
static bool summon_ring_bearer = FALSE;


static bool summon_specific_aux(int r_idx)
{
    return mon_is_type(r_idx, summon_specific_type);
}

bool mon_is_type(int r_idx, int type)
{
    monster_race *r_ptr = mon_race_lookup(r_idx);

    switch (type)
    {
    case SUMMON_CHAPEL_GOOD:
        return vault_aux_chapel_g(r_ptr);

    case SUMMON_CHAPEL_EVIL:
        return vault_aux_chapel_e(r_ptr);

    case SUMMON_ULTIMATE:
        if ( r_idx == 1083 || r_idx == 1087 || r_idx == 1088 || r_idx == 1085 || r_idx == 1084
            || r_idx == 847 || r_idx == 793 || r_idx == 800 || r_idx == 798 || r_idx == 836
            || r_idx == 816 )
        {
            return TRUE;
        }
        break;
    case SUMMON_BALROG:
        if (r_idx == 720 || r_idx == 940) return TRUE;
        break;
    case SUMMON_CLUBBER_DEMON:
        if (r_idx == 648) return TRUE;
        break;
    case SUMMON_DEMON_SUMMONER:
        if (!(r_ptr->flags1 & RF1_UNIQUE) && mon_race_can_summon(r_ptr, SUMMON_DEMON))
            return TRUE;
        break;
    case SUMMON_MATURE_DRAGON:
        /* Hack -- all non-unique 'd's with 'ature' or 'rake' in name */
        if ( r_ptr->d_char == 'd'
          && !(r_ptr->flags1 & RF1_UNIQUE)
          && (strstr(r_name + r_ptr->name, "ature") || strstr(r_name + r_ptr->name, "rake")) )
        {
            return TRUE;
        }
        break;
    case SUMMON_DRAGON_SUMMONER:
        if ( !(r_ptr->flags1 & RF1_UNIQUE)
          && (mon_race_can_summon(r_ptr, SUMMON_DRAGON) || mon_race_can_summon(r_ptr, SUMMON_HI_DRAGON)) )
        {
            return TRUE;
        }
        break;
    case SUMMON_UNDEAD_SUMMONER:
        if ( !(r_ptr->flags1 & RF1_UNIQUE)
          && (mon_race_can_summon(r_ptr, SUMMON_UNDEAD) || mon_race_can_summon(r_ptr, SUMMON_HI_UNDEAD)) )
        {
            return TRUE;
        }
        break;
    case SUMMON_DARK_ELF:
        if ( r_idx == 122 || r_idx == 178 || r_idx == 182 || r_idx == 226 || r_idx == 348
            || r_idx == 375 || r_idx == 400 || r_idx == 564 || r_idx == 657 || r_idx == 886)
        {
            return TRUE;
        }
        break;
    case SUMMON_GIANT:
        if (r_ptr->d_char == 'O' || r_ptr->d_char == 'P') return TRUE;
        break;
    case SUMMON_ORC:
        if (r_ptr->d_char == 'o') return TRUE;
        break;
    case SUMMON_TROLL:
        if (r_ptr->d_char == 'T') return TRUE;
        break;
    case SUMMON_YEEK:
        if (r_ptr->d_char == 'y') return TRUE;
        break;
    case SUMMON_ANT:
        if (r_ptr->d_char == 'a') return TRUE;
        break;
    case SUMMON_SPIDER:
        if (r_ptr->d_char == 'S') return TRUE;
        break;
    case SUMMON_HOUND:
        if (r_ptr->d_char == 'C' || r_ptr->d_char == 'Z') return TRUE;
        break;
    case SUMMON_HYDRA:
        if (r_ptr->d_char == 'M') return TRUE;
        break;
    case SUMMON_ENT:
        if (r_idx == MON_ENT) return TRUE;
        break;
    case SUMMON_ANGEL:
        if (r_ptr->d_char == 'A' && (r_ptr->flags3 & (RF3_EVIL|RF3_GOOD)))
            return TRUE;
        break;
    case SUMMON_DEMON:
        if (r_ptr->flags3 & RF3_DEMON) return TRUE;
        break;
    case SUMMON_UNDEAD:
        if (r_ptr->flags3 & RF3_UNDEAD) return TRUE;
        break;
    case SUMMON_DRAGON:
        if (r_ptr->flags3 & RF3_DRAGON) return TRUE;
        break;
    case SUMMON_HI_UNDEAD:
        if (strchr("LVW", r_ptr->d_char)) return TRUE;
        break;
    case SUMMON_HI_DRAGON:
        if (r_ptr->d_char == 'D') return TRUE;
        break;
    case SUMMON_HI_DEMON:
        if (strchr("UHB", r_ptr->d_char) && (r_ptr->flags3 & RF3_DEMON)) return TRUE;
        break;
    case SUMMON_AMBERITE:
        if (r_ptr->flags3 & (RF3_AMBERITE)) return TRUE;
        break;
    case SUMMON_OLYMPIAN:
        if (r_ptr->flags3 & (RF3_OLYMPIAN)) return TRUE;
        break;
    case SUMMON_HUMAN:
        if (r_ptr->flags2 & RF2_HUMAN) return TRUE;
        break;
    case SUMMON_HORSE:
        if (r_ptr->d_char == 'q' && (r_ptr->flags7 & RF7_RIDING)) return TRUE;
        break;
    case SUMMON_CAMELOT:
        if ((r_ptr->dun_type_id == D_CAMELOT) && (r_ptr->flags2 & RF2_KNIGHT)) return TRUE;
        break;
    case SUMMON_NIGHTMARE:
        if (r_idx == MON_NIGHTMARE) return TRUE;
        break;
    case SUMMON_RAT:
        if (r_ptr->d_char == 'r') return TRUE;
        break;
    case SUMMON_BAT:
        if (r_ptr->d_char == 'b') return TRUE;
        break;
    case SUMMON_WOLF:
        if (r_ptr->d_char == 'C') return TRUE;
        break;
    case SUMMON_DREAD:
        if (r_idx == MON_DREAD) return TRUE;
        break;
    case SUMMON_ZOMBIE:
        if (r_ptr->d_char == 'z') return TRUE;
        break;
    case SUMMON_SKELETON:
        if (r_ptr->d_char == 's' || r_idx == MON_BONE_DRAGON) return TRUE;
        break;
    case SUMMON_GHOST:
        if (r_idx == MON_SHADOW_DEMON) return FALSE;
        if (r_ptr->d_char == 'G' || r_idx == MON_SPECT_WYRM) return TRUE;
        break;
    case SUMMON_KRAKEN:
        if (r_idx == MON_GREATER_KRAKEN || r_idx == MON_LESSER_KRAKEN) return TRUE;
        break;
    case SUMMON_VAMPIRE:
        if (r_ptr->d_char == 'V') return TRUE;
        break;
    case SUMMON_WIGHT:
        if (r_ptr->d_char == 'W') return TRUE;
        break;
    case SUMMON_LICH:
        if (r_ptr->d_char == 'L' || r_idx == MON_DRACOLICH) return TRUE;
        break;
    case SUMMON_THIEF:
        if (r_ptr->flags2 & RF2_THIEF) return TRUE;
        break;
    case SUMMON_UNIQUE:
        if (r_ptr->flags1 & RF1_UNIQUE) return TRUE;
        break;
    case SUMMON_BIZARRE1:
        if (r_ptr->d_char == 'm') return TRUE;
        break;
    case SUMMON_BIZARRE2:
        if (r_ptr->d_char == 'b') return TRUE;
        break;
    case SUMMON_BIZARRE3:
        if (r_ptr->d_char == 'Q') return TRUE;
        break;
    case SUMMON_BIZARRE4:
        if (r_ptr->d_char == 'v') return TRUE;
        break;
    case SUMMON_BIZARRE5:
        if (r_ptr->d_char == '$') return TRUE;
        break;
    case SUMMON_BIZARRE6:
        if (strchr("!?=$|", r_ptr->d_char)) return TRUE;
        break;
    case SUMMON_GOLEM:
        if (r_ptr->d_char == 'g') return TRUE;
        break;
    case SUMMON_CYBER:
        if (r_idx == MON_CYBER) return TRUE;
        break;
    case SUMMON_KIN:
        if (r_ptr->d_char == summon_kin_type) return TRUE;
        break;
    case SUMMON_DAWN:
        if (r_idx == MON_DAWN) return TRUE;
        break;
    case SUMMON_ANIMAL:
        if (r_ptr->flags3 & RF3_ANIMAL) return TRUE;
        break;
    case SUMMON_ANIMAL_RANGER:
        if ( (r_ptr->flags3 & RF3_ANIMAL)
          && strchr("abcflqrwBCHIJKMRS", r_ptr->d_char)
          && !(r_ptr->flags3 & RF3_DRAGON)
          && !(r_ptr->flags3 & RF3_EVIL)
          && !(r_ptr->flags3 & RF3_UNDEAD)
          && !(r_ptr->flags3 & RF3_DEMON)
          && !(r_ptr->flags2 & RF2_MULTIPLY)
          && !r_ptr->spells )
        {
            return TRUE;
        }
        break;
    case SUMMON_HI_DRAGON_LIVING:
        if (r_ptr->d_char == 'D' && monster_living(r_ptr)) return TRUE;
        break;
    case SUMMON_LIVING:
        if (monster_living(r_ptr)) return TRUE;
        break;
    case SUMMON_PHANTOM:
        if (r_idx == MON_PHANTOM_B || r_idx == MON_PHANTOM_W) return TRUE;
        break;
    case SUMMON_BLUE_HORROR:
        if (r_idx == MON_BLUE_HORROR) return TRUE;
        break;
    case SUMMON_ELEMENTAL:
        if (r_ptr->d_char == 'E') return TRUE;
        break;
    case SUMMON_VORTEX:
        if (r_ptr->d_char == 'v') return TRUE;
        break;
    case SUMMON_HYBRID:
        if (r_ptr->d_char == 'H') return TRUE;
        break;
    case SUMMON_BIRD:
        if (r_ptr->d_char == 'B') return TRUE;
        break;
    case SUMMON_ARCHER:
        if ( r_ptr->d_char == 'p'
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
        if (monster_living(r_ptr) && mon_blows_find(r_ptr->blows, RBM_EXPLODE)) return TRUE;
        break;
    case SUMMON_MANES:
        if (r_idx == MON_MANES) return TRUE;
        break;
    case SUMMON_LOUSE:
        if (r_idx == MON_LOUSE) return TRUE;
        break;
    case SUMMON_SOFTWARE_BUG:
        if (r_idx == MON_SOFTWARE_BUG) return TRUE;
        break;
    case SUMMON_GUARDIAN:
        if (r_ptr->flags7 & RF7_GUARDIAN) return TRUE;
        break;
    case SUMMON_KNIGHT:
        if ( r_idx == MON_NOV_PALADIN || r_idx == MON_PALADIN
          || r_idx == MON_W_KNIGHT || r_idx == MON_ULTRA_PALADIN || r_idx == MON_KNI_TEMPLAR )
        {
            return TRUE;
        }
        break;
    case SUMMON_EAGLE:
        if (r_ptr->d_char == 'B' && (r_ptr->flags8 & RF8_WILD_MOUNTAIN) && (r_ptr->flags8 & RF8_WILD_ONLY))
            return TRUE;
        break;
    case SUMMON_PIRANHA:
        if (r_idx == MON_PIRANHA || r_idx == MON_GIANT_PIRANHA) return TRUE;
        break;
    case SUMMON_ARMAGE_GOOD:
        if (r_ptr->d_char == 'A' && (r_ptr->flags3 & RF3_GOOD)) return TRUE;
        break;
    case SUMMON_ARMAGE_EVIL:
        if ( (r_ptr->flags3 & RF3_DEMON)
            || (r_ptr->d_char == 'A' && (r_ptr->flags3 & RF3_EVIL)) )
        {
            return TRUE;
        }
        break;
    case SUMMON_MAGICAL:
        return monster_magical(r_ptr);
    case SUMMON_RING_BEARER:
        if (strchr("pthAy", r_ptr->d_char))
            return TRUE;
        else if (r_ptr->flags7 & RF7_NAZGUL)
            return TRUE;
        else if (r_idx == MON_ANGMAR || r_idx == MON_HOARMURATH || r_idx == MON_DWAR || r_idx == MON_KHAMUL)
             return TRUE;
        else if (r_ptr->d_char == 'V' && r_idx != 521 && r_idx != 536 && r_idx != 613)
            return TRUE;
        else if (r_ptr->d_char == 'L' && r_idx != 666)
            return TRUE;
        else if (r_idx == 112 || r_idx == 748)
            return TRUE;
        break;
    case SUMMON_MONK:
        if ( r_idx == MON_JADE_MONK || r_idx == MON_IVORY_MONK || r_idx == MON_MONASTIC_LICH
          || r_idx == MON_EBONY_MONK || r_idx == MON_MYSTIC || r_idx == MON_MASTER_MYSTIC
          || r_idx == MON_GRAND_MASTER_MYSTIC || r_idx == MON_TOPAZ_MONK )
        {
            return TRUE;
        }
        break;
    case SUMMON_MAGE:
        if (r_ptr->d_char == 'p' || r_ptr->d_char == 'h')
        {
            if ( r_ptr->body.class_idx == CLASS_MAGE
              || r_ptr->body.class_idx == CLASS_SORCERER )
            {
                return TRUE;
            }
        }
        break;
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
    bool            named = FALSE;

    /* Hack: Chengband requested to just show the pet's name (e.g. 'Stinky' vs
     * 'Your super-duper multi-hued behemoth called Stinky') */
    if (m_ptr->nickname && !(mode & MD_NO_PET_ABBREV) && !(mode & MD_PRON_VISIBLE))
    {
        sprintf(desc, "%s", quark_str(m_ptr->nickname));
        return;
    }

    r_ptr = mon_apparent_race(m_ptr);

    if ((m_ptr->mflag2 & MFLAG2_FUZZY) && !(mode & MD_IGNORE_FUZZY)) name = "Monster";
    /* Mode of MD_TRUE_NAME will reveal Chameleon's true name */
    else if (mode & MD_TRUE_NAME) name = (r_name + real_r_ptr(m_ptr)->name);
    else name = (r_name + r_ptr->name);

    /* Are we hallucinating? (Idea from Nethack...) */
    if (plr_tim_find(T_HALLUCINATE) && !(mode & MD_IGNORE_HALLU))
    {
        if (one_in_(2))
        {
            if (!get_rnd_line("silly.txt", m_ptr->r_idx, silly_name))

                named = TRUE;
        }

        if (!named)
        {
            monster_race *hallu_race;

            do
            {
                hallu_race = &r_info[randint1(max_r_idx - 1)];
            }
            while (!hallu_race->name || (hallu_race->flags1 & RF1_UNIQUE));

            strcpy(silly_name, (r_name + hallu_race->name));
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
        if (r_ptr->flags1 & (RF1_FEMALE)) kind = 0x20;
        else if (r_ptr->flags1 & (RF1_MALE)) kind = 0x10;

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
        if (r_ptr->flags1 & RF1_FEMALE) strcpy(desc, "herself");
        else if (r_ptr->flags1 & RF1_MALE) strcpy(desc, "himself");
        else strcpy(desc, "itself");
    }


    /* Handle all other visible monster requests */
    else
    {
        /* Tanuki? */
        if (is_pet(m_ptr) && !is_original_ap(m_ptr))
        {
            (void)sprintf(desc, "%s?", name);
        }
        else

        /* It could be a Unique */
        if ( (r_ptr->flags1 & RF1_UNIQUE)
          && !(plr_tim_find(T_HALLUCINATE) && !(mode & MD_IGNORE_HALLU))
          && !(m_ptr->mflag2 & MFLAG2_FUZZY) )
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
            if (is_pet(m_ptr))
                (void)strcpy(desc, "your ");

            else
                (void)strcpy(desc, "the ");

            (void)strcat(desc, name);
        }

        if (m_ptr->nickname)
        {
            sprintf(buf," called %s",quark_str(m_ptr->nickname));
            strcat(desc,buf);
        }

        if (p_ptr->riding && (dun_mon(cave, p_ptr->riding) == m_ptr))
        {
            if (p_ptr->prace == RACE_MON_RING)
                strcat(desc," (controlling)");
            else
                strcat(desc," (riding)");
        }

        if ((mode & MD_IGNORE_HALLU) && (m_ptr->mflag2 & MFLAG2_CHAMELEON))
        {
            if (r_ptr->flags1 & RF1_UNIQUE)
                strcat(desc," (Chameleon Lord)");
            else
                strcat(desc," (Chameleon)");
        }

        if ((mode & MD_IGNORE_HALLU) && !is_original_ap(m_ptr))
        {
            strcat(desc, format("(%s)", r_name + mon_race(m_ptr)->name));
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
    if ( p_ptr->pclass == CLASS_DUELIST
      && p_ptr->duelist_target_idx
      && !(mode & MD_PRON_VISIBLE)
      && m_ptr == dun_mon(cave, p_ptr->duelist_target_idx) )
    {
        strcat(desc, " (Foe)");
    }

    if ( p_ptr->painted_target
      && p_ptr->painted_target_idx
      && m_ptr == dun_mon(cave, p_ptr->painted_target_idx)
      && p_ptr->painted_target_ct >= 3 )
    {
        strcat(desc, " (Painted)");
    }

    if (p_ptr->wizard && m_ptr->mpower != 1000)
    {
        strcat(desc, format(" (%d.%d%%)", m_ptr->mpower/10, m_ptr->mpower%10));
    }
}




/*
 * Learn about a monster (by "probing" it)
 *
 * Return the number of new flags learnt. -Mogami-
 */
int lore_do_probe(int r_idx)
{
    monster_race *r_ptr = mon_race_lookup(r_idx);
    int i, n = 0;

    /* Maximal info about awareness */
    if (r_ptr->r_wake != MAX_UCHAR) n++;
    if (r_ptr->r_ignore != MAX_UCHAR) n++;
    r_ptr->r_wake = r_ptr->r_ignore = MAX_UCHAR;

    /* Count unknown flags */
    for (i = 0; i < 32; i++)
    {
        if (!(r_ptr->r_flags1 & (1L << i)) &&
            (r_ptr->flags1 & (1L << i))) n++;
        if (!(r_ptr->r_flags2 & (1L << i)) &&
            (r_ptr->flags2 & (1L << i))) n++;
        if (!(r_ptr->r_flags3 & (1L << i)) &&
            (r_ptr->flags3 & (1L << i))) n++;
        if (!(r_ptr->r_flagsr & (1L << i)) &&
            (r_ptr->flagsr & (1L << i))) n++;

        /* r_flags7 is actually unused */
#if 0
        if (!(r_ptr->r_flags7 & (1L << i)) &&
            (r_ptr->flags7 & (1L << i))) n++;
#endif
    }

    /* Know all the flags */
    r_ptr->r_flags1 = r_ptr->flags1;
    r_ptr->r_flags2 = r_ptr->flags2;
    r_ptr->r_flags3 = r_ptr->flags3;
    r_ptr->r_flagsr = r_ptr->flagsr;

    /* r_flags7 is actually unused */
    /* r_ptr->r_flags7 = r_ptr->flags7; */

    /* Know about evolution */
    if (!(r_ptr->r_xtra1 & MR1_SINKA)) n++;
    r_ptr->r_xtra1 |= MR1_SINKA;

    if (!(r_ptr->r_xtra1 & MR1_POSSESSOR)) n++;
    r_ptr->r_xtra1 |= MR1_POSSESSOR;

    if (!(r_ptr->r_xtra1 & MR1_LORE)) n++;
    r_ptr->r_xtra1 |= MR1_LORE;

    /* Update monster recall window */
    if (p_ptr->monster_race_idx == r_idx)
    {
        /* Window stuff */
        p_ptr->window |= (PW_MONSTER);
    }

    /* Return the number of new flags learnt */
    return n;
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
    monster_race *r_ptr = mon_race(m_ptr);

    /* If the monster doesn't have original appearance, don't note */
    if (!is_original_ap(m_ptr)) return;

    /* Note the number of things dropped */
    if (num_item > r_ptr->r_drop_item) r_ptr->r_drop_item = num_item;
    if (num_gold > r_ptr->r_drop_gold) r_ptr->r_drop_gold = num_gold;

    /* Update monster recall window */
    if (p_ptr->monster_race_idx == m_ptr->r_idx)
    {
        /* Window stuff */
        p_ptr->window |= (PW_MONSTER);
    }
}



void sanity_blast(monster_type *m_ptr, bool necro)
{
    bool happened = FALSE;
    int power = 100;

    if (!(cave->flags & DF_GENERATED)) return;
    if (p_ptr->no_eldritch) return;

    if (!necro)
    {
        char            m_name[80];
        monster_race    *r_ptr = mon_apparent_race(m_ptr);

        power = r_ptr->level / 2;

        monster_desc(m_name, m_ptr, 0);

        if (!(r_ptr->flags1 & RF1_UNIQUE))
        {
            if (r_ptr->flags1 & RF1_FRIENDS)
            power /= 2;
        }
        else power *= 2;

        if (!hack_mind)
            return; /* No effect yet, just loaded... */

        if (!m_ptr->ml)
            return; /* Cannot see it for some reason */

        if (!(r_ptr->flags2 & RF2_ELDRITCH_HORROR))
            return; /* oops */



        if (is_pet(m_ptr))
            return; /* Pet eldritch horrors are safe most of the time */

        if (randint1(100) > power) return;

        if (saving_throw(p_ptr->skills.sav - power))
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
                plr_tim_add(T_HALLUCINATE, randint1(r_ptr->level));
            }

            return; /* Never mind; we can't see it clearly enough */
        }

        /* Something frightening happens... */
        msg_format("You behold the %s visage of %s!",
            horror_desc[randint0(MAX_SAN_HORROR)], m_name);

        mon_lore_aux_2(r_ptr, RF2_ELDRITCH_HORROR);

        /* Demon characters are unaffected */
        if (get_race()->flags & RACE_IS_DEMON) return;

        if (p_ptr->wizard) return;

        /* Undead characters are 50% likely to be unaffected */
        if (get_race()->flags & RACE_IS_UNDEAD)
        {
            if (saving_throw(25 + p_ptr->lev)) return;
        }
    }
    else
    {
        msg_print("Your sanity is shaken by reading the Necronomicon!");

    }

    if (!saving_throw(p_ptr->skills.sav - power)) /* Mind blast */
    {
        if (!res_save_default(RES_CONF))
            plr_tim_add(T_CONFUSED, randint0(4) + 4);
        if (!res_save_default(RES_CHAOS) && !mut_present(MUT_WEIRD_MIND) && one_in_(3))
            plr_tim_add(T_HALLUCINATE, randint0(25) + 15);
        return;
    }

    if (!saving_throw(p_ptr->skills.sav - power))
    {
        do_dec_stat(A_INT);
        do_dec_stat(A_WIS);
        do_dec_stat(A_CHR);
        return;
    }

    if (!saving_throw(p_ptr->skills.sav - power)) /* Brain smash */
    {
        if (!res_save_default(RES_CONF))
            plr_tim_add(T_CONFUSED, randint0(4) + 4);
        if (!free_act_save_p(power))
            plr_tim_add(T_PARALYZED, randint1(4));
        else 
            equip_learn_flag(OF_FREE_ACT);
        while (randint0(100) > p_ptr->skills.sav)
            do_dec_stat(A_INT);
        while (randint0(100) > p_ptr->skills.sav)
            do_dec_stat(A_WIS);
        if (!res_save_default(RES_CHAOS) && !mut_present(MUT_WEIRD_MIND))
            plr_tim_add(T_HALLUCINATE, randint0(25) + 15);
        return;
    }

    if (!saving_throw(p_ptr->skills.sav - power)) /* Amnesia */
    {

        if (lose_all_info())
            msg_print("You forget everything in your utmost terror!");

        return;
    }

    if (saving_throw(p_ptr->skills.sav - power))
    {
        return;
    }

    /* Else gain permanent insanity */
    if (mut_present(MUT_MORONIC) && /*(p_ptr->muta2 & MUT2_BERS_RAGE) &&*/
        (mut_present(MUT_COWARDICE) || res_save_default(RES_FEAR)) &&
        (mut_present(MUT_HALLUCINATION) || res_save_default(RES_CHAOS)))
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
                if (!mut_present(MUT_COWARDICE) && !res_save_default(RES_FEAR))
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
                if (!mut_present(MUT_HALLUCINATION) && !res_save_default(RES_CHAOS))
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

    p_ptr->update |= PU_BONUS;
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
    mon_race_ptr race = mon_race(mon);
    mon_race_ptr ap_race = mon_apparent_race(mon);

    bool do_disturb = disturb_move;

    int d;

    /* Seen at all */
    bool flag = FALSE;
    bool fuzzy = FALSE;
    bool old_fuzzy = BOOL(mon->mflag2 & MFLAG2_FUZZY);

    /* Seen by vision */
    bool easy = FALSE;

    /* Do disturb? */
    if (disturb_high)
    {
        monster_race *ap_race = mon_apparent_race(mon);

        if (ap_race->r_tkills && ap_race->level >= p_ptr->lev)
            do_disturb = TRUE;
    }

    /* Compute distance */
    if (full)
    {
        d = point_fast_distance(cave->flow_pos, mon->pos);
        mon->cdis = d;

        if (mon->cdis <= 2 && plr_project_mon(mon))
            do_disturb = TRUE;
    }

    /* Extract distance */
    else
    {
        /* Extract the distance */
        d = mon->cdis;
    }

    /* done if player is not present */
    if (p_ptr->dun_id != cave->dun_id)
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
        if (p_ptr->special_defense & KATA_MUSOU)
        {
            /* Detectable */
            flag = TRUE;

            mon_lore_aux_2(race, RF2_SMART | RF2_STUPID);
        }

        /* Basic telepathy */
        /* Snipers get telepathy when they concentrate deeper */
        else if (p_ptr->telepathy)
        {
            /* Empty mind, no telepathy */
            if (race->flags2 & (RF2_EMPTY_MIND))
            {
                /* Memorize flags */
                mon_lore_aux_2(race, RF2_EMPTY_MIND);
            }

            /* Weird mind, occasional telepathy */
            else if (race->flags2 & (RF2_WEIRD_MIND))
            {
                if (redraw_hack)
                    flag = mon->ml;
                else if (one_in_(10))
                {
                    flag = TRUE;
                    mon_lore_aux_2(race, RF2_WEIRD_MIND | RF2_SMART | RF2_STUPID);
                    equip_learn_flag(OF_TELEPATHY);
                }
            }

            /* Normal mind, allow telepathy */
            else
            {
                /* Detectable */
                flag = TRUE;
                mon_lore_aux_2(race, RF2_SMART | RF2_STUPID);
                equip_learn_flag(OF_TELEPATHY);
            }
        }

        /* Magical sensing */
        if (p_ptr->esp_living && monster_living(race))
        {
            flag = TRUE;
            /* There is no RF3_LIVING flag, so you won't gain any monster memory here ... */
        }

        /* Warlock Pacts: Warlocks sense pact monsters at CL25 */
        if ( p_ptr->pclass == CLASS_WARLOCK
          && p_ptr->lev >= 25
          && warlock_is_pact_monster(race) )
        {
            flag = TRUE;
            /* Pacts are too broad to augment monster memory ... */
        }

        /* Magical sensing */
        if ((p_ptr->esp_animal) && (race->flags3 & (RF3_ANIMAL)))
        {
            flag = TRUE;
            if (is_original_ap(mon) && !plr_tim_find(T_HALLUCINATE)) mon_lore_aux_3(race, RF3_ANIMAL);
            equip_learn_flag(OF_ESP_ANIMAL);
        }

        /* Magical sensing */
        if ((p_ptr->esp_undead) && (race->flags3 & (RF3_UNDEAD)))
        {
            flag = TRUE;
            if (is_original_ap(mon) && !plr_tim_find(T_HALLUCINATE)) mon_lore_aux_3(race, RF3_UNDEAD);
            equip_learn_flag(OF_ESP_UNDEAD);
        }

        /* Magical sensing */
        if ((p_ptr->esp_demon) && (race->flags3 & (RF3_DEMON)))
        {
            flag = TRUE;
            if (is_original_ap(mon) && !plr_tim_find(T_HALLUCINATE)) mon_lore_aux_3(race, RF3_DEMON);
            equip_learn_flag(OF_ESP_DEMON);
        }

        /* Magical sensing */
        if ((p_ptr->esp_orc) && (race->flags3 & (RF3_ORC)))
        {
            flag = TRUE;
            if (is_original_ap(mon) && !plr_tim_find(T_HALLUCINATE)) mon_lore_aux_3(race, RF3_ORC);
            equip_learn_flag(OF_ESP_ORC);
        }

        /* Magical sensing */
        if ((p_ptr->esp_troll) && (race->flags3 & (RF3_TROLL)))
        {
            flag = TRUE;
            if (is_original_ap(mon) && !plr_tim_find(T_HALLUCINATE)) mon_lore_aux_3(race, RF3_TROLL);
            equip_learn_flag(OF_ESP_TROLL);
        }

        /* Magical sensing */
        if ((p_ptr->esp_giant) && (race->flags3 & (RF3_GIANT)))
        {
            flag = TRUE;
            if (is_original_ap(mon) && !plr_tim_find(T_HALLUCINATE)) mon_lore_aux_3(race, RF3_GIANT);
            equip_learn_flag(OF_ESP_GIANT);
        }

        /* Magical sensing */
        if ((p_ptr->esp_dragon) && (race->flags3 & (RF3_DRAGON)))
        {
            flag = TRUE;
            if (is_original_ap(mon) && !plr_tim_find(T_HALLUCINATE)) mon_lore_aux_3(race, RF3_DRAGON);
            equip_learn_flag(OF_ESP_DRAGON);
        }

        /* Magical sensing */
        if ((p_ptr->esp_human) && (race->flags2 & (RF2_HUMAN)))
        {
            flag = TRUE;
            if (is_original_ap(mon) && !plr_tim_find(T_HALLUCINATE)) mon_lore_aux_2(race, RF2_HUMAN);
            equip_learn_flag(OF_ESP_HUMAN);
        }

        /* Magical sensing */
        if ((p_ptr->esp_evil) && (race->flags3 & (RF3_EVIL)))
        {
            flag = TRUE;
            if (is_original_ap(mon) && !plr_tim_find(T_HALLUCINATE)) mon_lore_aux_3(race, RF3_EVIL);
            equip_learn_flag(OF_ESP_EVIL);
        }

        /* Magical sensing */
        if ((p_ptr->esp_good) && (race->flags3 & (RF3_GOOD)))
        {
            flag = TRUE;
            if (is_original_ap(mon) && !plr_tim_find(T_HALLUCINATE)) mon_lore_aux_3(race, RF3_GOOD);
            equip_learn_flag(OF_ESP_GOOD);
        }

        /* Magical sensing */
        if ((p_ptr->esp_nonliving) &&
            ((race->flags3 & (RF3_DEMON | RF3_UNDEAD | RF3_NONLIVING)) == RF3_NONLIVING))
        {
            flag = TRUE;
            if (is_original_ap(mon) && !plr_tim_find(T_HALLUCINATE)) mon_lore_aux_3(race, RF3_NONLIVING);
            equip_learn_flag(OF_ESP_NONLIVING);
        }

        /* Magical sensing */
        if ((p_ptr->esp_unique) && (race->flags1 & (RF1_UNIQUE)))
        {
            flag = TRUE;
            if (is_original_ap(mon) && !plr_tim_find(T_HALLUCINATE)) mon_lore_aux_1(race, RF1_UNIQUE);
            equip_learn_flag(OF_ESP_UNIQUE);
        }

        if (p_ptr->esp_magical && mon_race_is_magical(race))
            flag = TRUE;

        /* Normal line of sight, and not blind */
        if (!plr_tim_find(T_BLIND) && (plr_los(mon->pos)/* || plr_project_mon(mon)*/))
        {
            bool do_invisible = FALSE;
            bool do_cold_blood = FALSE;


            /* Snipers can see targets in darkness when they concentrate deeper */
            if (p_ptr->concent >= CONCENT_RADAR_THRESHOLD)
            {
                /* Easy to see */
                easy = flag = TRUE;
            }

            /* Use "infravision" */
            if (d <= p_ptr->see_infra)
            {
                /* Handle "cold blooded" monsters */
                if ((race->flags2 & RF2_COLD_BLOOD) && !mon_auras_find(race, GF_FIRE))
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
                if (race->flags2 & (RF2_INVISIBLE))
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
                           && p_ptr->see_inv /* rule out single round ?DetectInvisible */
                           && (mon_tim_find(mon, MT_SLEEP) || (race->flags1 & RF1_NEVER_MOVE)))
                    {
                        easy = flag = TRUE;
                    }
                    else if (plr_see_invis(race->level))
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
                    if (do_invisible) mon_lore_aux_2(race, RF2_INVISIBLE);
                    if (do_cold_blood) mon_lore_aux_2(race, RF2_COLD_BLOOD);
                }
            }
        }
    }

    if (p_ptr->wizard)
        flag = TRUE;

    mon->mflag2 &= ~MFLAG2_FUZZY;

    /* The monster is now visible */
    if (flag)
    {
        /* "easy" denotes direct illumination. Detection and telepathy are
         * generally "fuzzy" except for pets (you know their minds since you
         * control their minds) and certain privileged classes (wizards). */
        if (!easy && !p_ptr->wizard_sight && !is_pet(mon) && !p_ptr->wizard)
        {
            fuzzy = TRUE;
            mon->mflag2 |= MFLAG2_FUZZY;
        }

        /* It was previously unseen, or previously fuzzy */
        if (!mon->ml || fuzzy != old_fuzzy)
        {
            /* Mark as visible */
            mon->ml = TRUE;

            /* Draw the monster */
            lite_pos(mon->pos);

            /* Update health bar as needed */
            check_mon_health_redraw(mon->id);

            /* Hack -- Count "fresh" sightings
             * XXX Don't leak monster identity for fuzzy awareness */
            if (!plr_tim_find(T_HALLUCINATE) && !fuzzy)
            {
                if (mon->ap_r_idx == MON_KAGE && ap_race->r_sights < MAX_SHORT)
                    ap_race->r_sights++;
                else if (is_original_ap(mon) && (race->r_sights < MAX_SHORT))
                    race->r_sights++;
            }

            /* Eldritch Horror */
            if (!fuzzy && (ap_race->flags2 & RF2_ELDRITCH_HORROR))
            {
                sanity_blast(mon, FALSE);
            }

            if (!fuzzy)
                fear_update_m(mon);

            /* Disturb on appearance */
            if (disturb_near && plr_project(mon->pos))
            {
                if (town_no_disturb && plr_in_town() && race->level == 0)
                {
                }
                else if (disturb_pets || is_hostile(mon))
                    disturb(1, 0);
            }

            p_ptr->window |= PW_MONSTER_LIST;
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
            lite_pos(mon->pos);

            /* Update health bar as needed */
            check_mon_health_redraw(mon->id);

            /* Disturb on disappearance */
            if (do_disturb)
            {
                if (disturb_pets || is_hostile(mon))
                    disturb(1, 0);
            }
            p_ptr->window |= PW_MONSTER_LIST;
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
                if (disturb_pets || is_hostile(mon))
                    disturb(1, 0);
            }
            p_ptr->window |= PW_MONSTER_LIST;
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
                if (disturb_pets || is_hostile(mon))
                    disturb(1, 0);
            }
            p_ptr->window |= PW_MONSTER_LIST;
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
    monster_race *old_r_ptr = mon_race(m_ptr);

    if (!(r_ptr->flags1 & (RF1_UNIQUE))) return FALSE;
    if (r_ptr->flags7 & (RF7_FRIENDLY | RF7_CHAMELEON)) return FALSE;

    if (ABS(r_ptr->level - mon_race_lookup(MON_CHAMELEON_K)->level) > 5) return FALSE;

    if (mon_blows_find(r_ptr->blows, RBM_EXPLODE))
        return FALSE;

    if (!monster_can_cross_terrain(cave_at(m_ptr->pos)->feat, r_ptr, 0)) return FALSE;

    /* Not born */
    if (!(old_r_ptr->flags7 & RF7_CHAMELEON))
    {
        if (monster_has_hostile_align(m_ptr, 0, 0, r_ptr)) return FALSE;
    }

    /* Born now */
    else if (summon_specific_who > 0)
    {
        if (monster_has_hostile_align(dun_mon(cave, summon_specific_who), 0, 0, r_ptr)) return FALSE;
    }

    return TRUE;
}

static bool monster_hook_chameleon(mon_race_ptr r_ptr)
{
    monster_type *m_ptr = chameleon_change_mon;
    monster_race *old_r_ptr = mon_race(m_ptr);

    if (r_ptr->flags1 & (RF1_UNIQUE)) return FALSE;
    if (r_ptr->flags2 & RF2_MULTIPLY) return FALSE;
    if (r_ptr->flags7 & (RF7_FRIENDLY | RF7_CHAMELEON)) return FALSE;

    if (mon_blows_find(r_ptr->blows, RBM_EXPLODE))
        return FALSE;

    if (!monster_can_cross_terrain(cave_at(m_ptr->pos)->feat, r_ptr, 0)) return FALSE;

    /* Not born */
    if (!(old_r_ptr->flags7 & RF7_CHAMELEON))
    {
        if ((old_r_ptr->flags3 & RF3_GOOD) && !(r_ptr->flags3 & RF3_GOOD)) return FALSE;
        if ((old_r_ptr->flags3 & RF3_EVIL) && !(r_ptr->flags3 & RF3_EVIL)) return FALSE;
        if (!(old_r_ptr->flags3 & (RF3_GOOD | RF3_EVIL)) && (r_ptr->flags3 & (RF3_GOOD | RF3_EVIL))) return FALSE;
    }

    /* Born now */
    else if (summon_specific_who > 0)
    {
        if (monster_has_hostile_align(dun_mon(cave, summon_specific_who), 0, 0, r_ptr)) return FALSE;
    }

    assert(cave->dun_type_id != D_SURFACE);
    return mon_alloc_dungeon(r_ptr);
}

void choose_new_monster(mon_ptr mon, bool born, int r_idx)
{
    int oldmaxhp;
    monster_race *r_ptr;
    mon_race_ptr old_r_ptr = mon_race(mon);
    char old_m_name[80];
    bool old_unique = FALSE;

    if (old_r_ptr->flags1 & RF1_UNIQUE)
    {
        old_unique = TRUE;
        if (r_idx == MON_CHAMELEON)
            r_idx = MON_CHAMELEON_K;
    }
    r_ptr = mon_race_lookup(r_idx);

    monster_desc(old_m_name, mon, 0);

    if (!r_idx)
    {
        int level;

        chameleon_change_mon = mon;
        if (old_unique)
            mon_alloc_push_filter(monster_hook_chameleon_lord);
        else
            mon_alloc_push_filter(monster_hook_chameleon);

        if (old_unique) level = mon_race_lookup(MON_CHAMELEON_K)->level;
        else level = cave->difficulty;

        r_ptr = mon_alloc_choose(level);
        if (r_ptr) r_idx = r_ptr->id;
        mon_alloc_pop_filter();

        chameleon_change_mon = NULL;

        /* Paranoia */
        if (!r_idx) return;
    }

    if (is_pet(mon)) check_pets_num_and_align(mon, FALSE);

    mon->r_idx = r_idx;
    mon->ap_r_idx = r_idx;
    update_mon(mon, FALSE);
    lite_pos(mon->pos);

    if ((old_r_ptr->flags7 & (RF7_LITE_MASK | RF7_DARK_MASK)) ||
        (r_ptr->flags7 & (RF7_LITE_MASK | RF7_DARK_MASK)))
        p_ptr->update |= (PU_MON_LITE);

    if (is_pet(mon)) check_pets_num_and_align(mon, TRUE);

    if (born)
    {
        /* Sub-alignment of a chameleon */
        if (r_ptr->flags3 & (RF3_EVIL | RF3_GOOD))
        {
            mon->sub_align = SUB_ALIGN_NEUTRAL;
            if (r_ptr->flags3 & RF3_EVIL) mon->sub_align |= SUB_ALIGN_EVIL;
            if (r_ptr->flags3 & RF3_GOOD) mon->sub_align |= SUB_ALIGN_GOOD;
        }
        return;
    }

    if (mon->id == p_ptr->riding)
    {
        char m_name[80];
        monster_desc(m_name, mon, 0);
        msg_format("Suddenly, %s transforms!", old_m_name);
        if (!(r_ptr->flags7 & RF7_RIDING))
            if (rakuba(0, TRUE)) msg_format("You have fallen from %s.", m_name);
    }

    /* Extract the monster base speed */
    mon->mspeed = get_mspeed(r_ptr);

    oldmaxhp = mon->max_maxhp;
    /* Assign maximal hitpoints */
    if (r_ptr->flags1 & RF1_FORCE_MAXHP)
    {
        mon->max_maxhp = maxroll(r_ptr->hdice, r_ptr->hside);
    }
    else
    {
        mon->max_maxhp = damroll(r_ptr->hdice, r_ptr->hside);
    }

    mon->maxhp = (int)mon->maxhp * mon->max_maxhp / oldmaxhp;
    if (mon->maxhp < 1) mon->maxhp = 1;
    mon->hp = (int)mon->hp * mon->max_maxhp / oldmaxhp;
}


/*
 *  Hook for Tanuki
 */
static bool monster_hook_tanuki(mon_race_ptr r_ptr)
{

    if (r_ptr->flags1 & (RF1_UNIQUE)) return FALSE;
    if (r_ptr->flags2 & RF2_MULTIPLY) return FALSE;
    if (r_ptr->flags7 & (RF7_FRIENDLY | RF7_CHAMELEON)) return FALSE;
    if (r_ptr->flags7 & RF7_AQUATIC) return FALSE;

    if (mon_blows_find(r_ptr->blows, RBM_EXPLODE))
        return FALSE;

    return mon_alloc_dungeon(r_ptr);
}


/*
 *  Set initial racial appearance of a monster
 */
static int initial_r_appearance(int r_idx)
{
    int attempts = 1000;
    int min = MIN(cave->difficulty-5, 50);

    if (!(mon_race_lookup(r_idx)->flags7 & RF7_TANUKI))
        return r_idx;

    mon_alloc_push_filter(monster_hook_tanuki);
    while (--attempts)
    {
        mon_race_ptr race = mon_alloc_choose(cave->difficulty + 10);
        if (race->level < min) continue;
        r_idx = race->id;
        break;
    }
    mon_alloc_pop_filter();
    return r_idx;
}


/*
 * Get initial monster speed
 */
byte get_mspeed(monster_race *r_ptr)
{
    /* Extract the monster base speed */
    int mspeed = r_ptr->speed;

    /* Hack -- small racial variety */
    if (!(r_ptr->flags1 & RF1_UNIQUE))
    {
        /* Allow some small variation per monster */
        int i = SPEED_TO_ENERGY(r_ptr->speed) / (one_in_(4) ? 3 : 10);
        if (i) mspeed += rand_spread(0, i);
    }

    if (mspeed > 199) mspeed = 199;

    return (byte)mspeed;
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
int place_monster_one(int who, point_t pos, int r_idx, int pack_idx, u32b mode)
{
    cave_type        *c_ptr = cave_at(pos);
    monster_type    *m_ptr;
    monster_race    *r_ptr = mon_race_lookup(r_idx);
    bool cloned = FALSE;

    /* Verify location */
    if (!dun_pos_interior(cave, pos)) return 0;

    /* Paranoia */
    if (!r_idx) return 0;

    /* Sanity */
    if (pack_idx && dun_mgr_pack(pack_idx)->count > 30) return 0;

    /* Paranoia */
    if (!r_ptr->name) return 0;

    if (!(mode & PM_IGNORE_TERRAIN))
    {
        /* Not on the Pattern */
        if (pattern_tile(pos.y, pos.x)) return 0;

        /* Require empty space (if not ghostly) */
        if (!monster_can_enter(pos.y, pos.x, r_ptr, 0)) return 0;
    }

    /* Hack -- "unique" monsters must be "unique" */
    if (((r_ptr->flags1 & (RF1_UNIQUE)) ||
         (r_ptr->flags7 & (RF7_NAZGUL))) &&
        (r_ptr->cur_num >= r_ptr->max_num))
    {
        /* Cannot create */
        if (mode & PM_ALLOW_CLONED)
            cloned = TRUE;
        else
            return 0;
    }

    if ((r_ptr->flags7 & (RF7_UNIQUE2)) &&
        (r_ptr->cur_num >= 1))
    {
        return 0;
    }

    #if 0
    /* XXX cf _weird in mon_spell.c. This check will prevent Banor and Rupart from
     * combining into one.  cf _mon_alloc_prob in mon.c. Banor=Rupart should not
     * be chosen for monster allocation when the conditions of this check apply.
     * In other words, I don't think this check is needed, and it is broken in any
     * case. cur_num won't be correct after a monster deletion until the next game
     * turn (cf dun_t.graveyard and _mon_free in dun.c). */
    if (r_idx == MON_BANORLUPART)
    {
        if (mon_race_lookup(MON_BANOR)->cur_num > 0) return 0;
        if (mon_race_lookup(MON_LUPART)->cur_num > 0) return 0;
    }
    #endif

    /* Depth monsters may NOT be created out of depth */
    if ( (r_ptr->flags1 & RF1_FORCE_DEPTH)
      && cave->dun_lvl < r_ptr->level
      && (r_ptr->flagsx & RFX_QUESTOR) )
    {
        /* Cannot create */
        return 0;
    }
    /* XXX Arena and quest accidents. RF1_FIXED_UNIQUE *should* have been set ...
     * if (r_ptr->flagsx & RFX_SUPPRESS) return 0;*/

    if (is_glyph_grid(c_ptr))
    {
        if (randint1(BREAK_GLYPH) < (r_ptr->level+20))
        {
            /* Describe observable breakage */
            if (c_ptr->info & CAVE_MARK)
            {
                msg_print("The rune of protection is broken!");

            }

            /* Forget the rune */
            c_ptr->info &= ~(CAVE_MARK);

            /* Break the rune */
            c_ptr->info &= ~(CAVE_OBJECT);
            c_ptr->mimic = 0;

            /* Notice */
            note_pos(pos);
        }
        else return 0;
    }

    if ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags7 & RF7_NAZGUL) || (r_ptr->level < 10)) mode &= ~PM_KAGE;

    /* Make a new monster */
    m_ptr = dun_alloc_mon(cave);
    hack_m_idx_ii = m_ptr->id;

    /* Get a new monster record */
    m_ptr->pack_idx = pack_idx;
    if (pack_idx)
        dun_mgr_pack(pack_idx)->count++;

    /* Save the race */
    m_ptr->r_idx = r_idx;
    m_ptr->ap_r_idx = initial_r_appearance(r_idx);

    /* No flags */
    m_ptr->mflag = 0;
    m_ptr->mflag2 = 0;

    /* Hack -- Appearance transfer */
    if ((mode & PM_MULTIPLY) && (who > 0) && !is_original_ap(dun_mon(cave, who)))
    {
        m_ptr->ap_r_idx = dun_mon(cave, who)->ap_r_idx;

        /* Hack -- Shadower spawns Shadower */
        if (dun_mon(cave, who)->mflag2 & MFLAG2_KAGE) m_ptr->mflag2 |= MFLAG2_KAGE;
    }

    /* Sub-alignment of a monster */
    if ((who > 0) && !(r_ptr->flags3 & (RF3_EVIL | RF3_GOOD)))
        m_ptr->sub_align = dun_mon(cave, who)->sub_align;
    else
    {
        m_ptr->sub_align = SUB_ALIGN_NEUTRAL;
        if (r_ptr->flags3 & RF3_EVIL) m_ptr->sub_align |= SUB_ALIGN_EVIL;
        if (r_ptr->flags3 & RF3_GOOD) m_ptr->sub_align |= SUB_ALIGN_GOOD;
    }

    if (cloned)
        m_ptr->smart |= (1U << SM_CLONED);
    if (who > 0)
        m_ptr->smart |= (1U << SM_SUMMONED);

    m_ptr->cdis = point_fast_distance(pos, cave->flow_pos);

    reset_target(m_ptr);

    m_ptr->nickname = 0;

    m_ptr->exp = 0;

    if (who > 0)
    {
        /* Your pet summons its pet. */
        if (is_pet(dun_mon(cave, who)))
            mode |= PM_FORCE_PET;
        mon_set_parent(m_ptr, who);
    }
    else
    {
        mon_set_parent(m_ptr, 0);
    }

    if (r_ptr->flags7 & RF7_CHAMELEON)
    {
        m_ptr->pos = pos; /* XXX Hack: m_ptr not born yet, so not placed yet. The hook needs
                             the position to pick a realistic race. */
        choose_new_monster(m_ptr, TRUE, 0);
        r_ptr = mon_race(m_ptr);
        m_ptr->mflag2 |= MFLAG2_CHAMELEON;

        /* Hack - Set sub_align to neutral when the Chameleon Lord is generated as "GUARDIAN" */
        if ((r_ptr->flags1 & RF1_UNIQUE) && (who <= 0))
            m_ptr->sub_align = SUB_ALIGN_NEUTRAL;
    }
    else if ((mode & PM_KAGE) && !(mode & PM_FORCE_PET))
    {
        m_ptr->ap_r_idx = MON_KAGE;
        m_ptr->mflag2 |= MFLAG2_KAGE;
    }

    if (mode & PM_NO_PET) m_ptr->mflag2 |= MFLAG2_NOPET;

    /* Not visible */
    m_ptr->ml = FALSE;

    /* Pet? */
    if (mode & PM_FORCE_PET)
    {
        set_pet(m_ptr);
    }
    /* Friendly? */
    else if ((r_ptr->flags7 & RF7_FRIENDLY) ||
         (mode & PM_FORCE_FRIENDLY) || is_friendly_idx(who))
    {
        if (allow_friendly_monster && !monster_has_hostile_align(NULL, 11, -11, r_ptr)) set_friendly(m_ptr);
    }
    else if ( (r_ptr->flags3 & RF3_ANIMAL)
           && randint0(1000) < virtue_current(VIRTUE_NATURE) )
    {
        if (allow_friendly_monster && !monster_has_hostile_align(NULL, 11, -11, r_ptr))
            set_friendly(m_ptr);
    }
    else if ( (r_ptr->flags2 & RF2_KNIGHT)
           && randint0(1000) < virtue_current(VIRTUE_HONOUR) )
    {
        if (allow_friendly_monster && !monster_has_hostile_align(NULL, 11, -11, r_ptr))
            set_friendly(m_ptr);
    }
    else if ( r_ptr->d_char == 'A'
           && randint0(1000) < virtue_current(VIRTUE_FAITH) )
    {
        if (allow_friendly_monster && !monster_has_hostile_align(NULL, 11, -11, r_ptr))
            set_friendly(m_ptr);
    }
    else if ( (r_ptr->flags3 & RF3_DEMON)
           && randint0(1000) < -virtue_current(VIRTUE_FAITH) )
    {
        if (allow_friendly_monster && !monster_has_hostile_align(NULL, 11, -11, r_ptr))
            set_friendly(m_ptr);
    }
    else if ( (r_ptr->flags3 & RF3_UNDEAD)
           && randint0(1000) < virtue_current(VIRTUE_UNLIFE) )
    {
        if (allow_friendly_monster && !monster_has_hostile_align(NULL, 11, -11, r_ptr))
            set_friendly(m_ptr);
    }

    /* Enforce sleeping if needed */
    if ((mode & PM_ALLOW_SLEEP) && r_ptr->sleep)
    {
        int x = r_ptr->sleep;
        mon_tim_add(m_ptr, MT_SLEEP, 2*x + randint1(10*x));
    }

    /* Assign maximal hitpoints */
    if (r_ptr->flags1 & RF1_FORCE_MAXHP)
    {
        m_ptr->max_maxhp = maxroll(r_ptr->hdice, r_ptr->hside);
    }
    else
    {
        m_ptr->max_maxhp = damroll(r_ptr->hdice, r_ptr->hside);
    }

    m_ptr->maxhp = m_ptr->max_maxhp;

    /* And start out fully healthy */
    if (m_ptr->r_idx == MON_WOUNDED_BEAR)
        m_ptr->hp = m_ptr->maxhp / 2;
    else m_ptr->hp = m_ptr->maxhp;


    /* Extract the monster base speed */
    m_ptr->mspeed = get_mspeed(r_ptr);
    m_ptr->ac_adj = 0;
    m_ptr->mpower = 1000;

    if (mode & PM_HASTE) mon_tim_add(m_ptr, T_FAST, 100);

    /* Give a random starting energy */
    m_ptr->energy_need = ENERGY_NEED() - (s16b)randint0(100);

    if (r_ptr->flags7 & RF7_SELF_LD_MASK)
        p_ptr->update |= (PU_MON_LITE);
    else if ((r_ptr->flags7 & RF7_HAS_LD_MASK) && !mon_tim_find(m_ptr, MT_SLEEP))
        p_ptr->update |= (PU_MON_LITE);

    /* Pre-roll the number of monsters drops so that Rogues may pick pockets.
       What the drops actually are will be determined later.
     */
    mon_drop_init(m_ptr);

    dun_place_mon(cave, m_ptr, pos); /* XXX *after* birth is completed */

    /* Count the monsters on the level */
    real_r_ptr(m_ptr)->cur_num++;

    /* Hack -- Count the number of "reproducers" */
    if (r_ptr->flags2 & RF2_MULTIPLY) cave->breed_ct++;

    /* Hack -- Notice new multi-hued monsters */
    {
        monster_race *ap_r_ptr = mon_apparent_race(m_ptr);
        if (ap_r_ptr->flags1 & (RF1_ATTR_MULTI | RF1_SHAPECHANGER))
            shimmer_monsters = TRUE;
    }

    if (p_ptr->warning && (cave->flags & DF_GENERATED))
    {
        if (r_ptr->flags1 & RF1_UNIQUE)
        {
            cptr color;
            object_type *o_ptr;
            char o_name[MAX_NLEN];

            if (r_ptr->level > p_ptr->lev + 30)
                color = "black";
            else if (r_ptr->level > p_ptr->lev + 15)
                color = "purple";
            else if (r_ptr->level > p_ptr->lev + 5)
                color = "deep red";
            else if (r_ptr->level > p_ptr->lev - 5)
                color = "red";
            else if (r_ptr->level > p_ptr->lev - 15)
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

    if (is_mon_trap_grid(c_ptr))
        hit_mon_trap(pos.y, pos.x, m_ptr->id);

    /* Success */
    return m_ptr->id;
}


/*
 *  improved version of scatter() for place monster
 */

#define MON_SCAT_MAXD 10

static bool mon_scatter(int r_idx, int *yp, int *xp, int y, int x, int max_dist)
{
    int place_x[MON_SCAT_MAXD];
    int place_y[MON_SCAT_MAXD];
    int num[MON_SCAT_MAXD];
    int i;
    int nx, ny;

    if (max_dist >= MON_SCAT_MAXD)
        return FALSE;

    for (i = 0; i < MON_SCAT_MAXD; i++)
        num[i] = 0;

    for (nx = x - max_dist; nx <= x + max_dist; nx++)
    {
        for (ny = y - max_dist; ny <= y + max_dist; ny++)
        {
            /* Ignore annoying locations */
            if (!in_bounds(ny, nx)) continue;

            /* Require "line of projection" */
            if (!projectable(y, x, ny, nx)) continue;

            if (r_idx > 0)
            {
                monster_race *r_ptr = mon_race_lookup(r_idx);

                /* Require empty space (if not ghostly) */
                if (!monster_can_enter(ny, nx, r_ptr, 0))
                    continue;
            }
            else
            {
                /* Walls and Monsters block flow */
                if (!cave_empty_bold2(ny, nx)) continue;

                /* ... nor on the Pattern */
                if (pattern_tile(ny, nx)) continue;
            }

            i = distance(y, x, ny, nx);

            if (i > max_dist)
                continue;

            num[i]++;

            /* random swap */
            if (one_in_(num[i]))
            {
                place_x[i] = nx;
                place_y[i] = ny;
            }
        }
    }

    i = 0;
    while (i < MON_SCAT_MAXD && 0 == num[i])
        i++;
    if (i >= MON_SCAT_MAXD)
        return FALSE;

    *xp = place_x[i];
    *yp = place_y[i];

    return TRUE;
}


/*
 * Maximum size of a group of monsters
 */
#define GROUP_MAX    32


/*
 * Attempt to place a "group" of monsters around the given location
 */
static bool place_monster_group(int who, point_t pos, int r_idx, int pack_idx, u32b mode)
{
    monster_race *r_ptr = mon_race_lookup(r_idx);

    int n, i;
    int total = 0;

    int hack_n = 0;
    point_t hack_p[GROUP_MAX];

    if (r_ptr->pack_dice)
    {
        total = damroll(r_ptr->pack_dice, r_ptr->pack_sides);
    }
    else
    {
        int extra = 0;
        int lvl = cave->difficulty;

        /* Pick a group size */
        total = randint1(10);

        if (lvl)
        {
            /* Hard monsters, small groups */
            if (r_ptr->level > lvl)
            {
                extra = r_ptr->level - lvl;
                extra = 0 - randint1(extra);
            }

            /* Easy monsters, large groups */
            else if (r_ptr->level < lvl)
            {
                extra = lvl - r_ptr->level;
                extra = randint1(extra);
            }
        }

        /* Hack -- limit group reduction */
        if (extra > 9) extra = 9;

        /* Modify the group size */
        total += extra;
    }
    total = total * (625 - virtue_current(VIRTUE_INDIVIDUALISM)) / 625;

    /* Minimum size */
    if (total < 1) total = 1;

    /* Maximum size */
    if (total > GROUP_MAX) total = GROUP_MAX;

    /* Start on the monster */
    hack_p[hack_n++] = pos;

    /* Puddle monsters, breadth first, up to total */
    for (n = 0; (n < hack_n) && (hack_n < total); n++)
    {
        /* Grab the location */
        point_t hp = hack_p[n];

        /* Check each direction, up to total */
        for (i = 0; (i < 8) && (hack_n < total); i++)
        {
            point_t mp = scatter(hp, 4);

            /* Walls and Monsters block flow */
            if (!cave_empty_at(mp)) continue;

            /* Attempt to place another monster */
            if (place_monster_one(who, mp, r_idx, pack_idx, mode))
            {
                /* Add it to the "hack" set */
                hack_p[hack_n++] = mp;
            }
        }
    }

    /* Success */
    return (TRUE);
}


/*
 * Hack -- help pick an escort type
 */
static int place_monster_idx = 0;
static int place_monster_m_idx = 0;
static bool place_monster_okay(mon_race_ptr race)
{
    monster_race *p_r_ptr = mon_race_lookup(place_monster_idx);
    monster_type *p_m_ptr = dun_mon(cave, place_monster_m_idx);

    /* Require similar "race" */
    if (race->d_char != p_r_ptr->d_char) return FALSE;

    /* Skip more advanced monsters */
    if (race->level > p_r_ptr->level) return FALSE;

    /* Skip unique monsters */
    if (race->flags1 & RF1_UNIQUE) return FALSE;

    /* Paranoia -- Skip identical monsters */
    if (place_monster_idx == race->id) return FALSE;

    /* Skip different alignment */
    if (monster_has_hostile_align(p_m_ptr, 0, 0, race)) return FALSE;

    if (p_r_ptr->flags7 & RF7_FRIENDLY)
    {
        if (monster_has_hostile_align(NULL, 1, -1, race)) return FALSE;
    }

    if ((p_r_ptr->flags7 & RF7_CHAMELEON) && !(race->flags7 & RF7_CHAMELEON))
        return FALSE;

    return TRUE;
}


/*
 * Attempt to place a monster of the given race at the given location
 *
 * Note that certain monsters are now marked as requiring "friends".
 * These monsters, if successfully placed, and if the "grp" parameter
 * is TRUE, will be surrounded by a "group" of identical monsters.
 *
 * Note that certain monsters are now marked as requiring an "escort",
 * which is a collection of monsters with similar "race" but lower level.
 *
 * Some monsters induce a fake "group" flag on their escorts.
 *
 * Note the "bizarre" use of non-recursion to prevent annoying output
 * when running a code profiler.
 *
 * XXX We now return a pointer to the allocated monster. For packs, this
 * will always be the pack leader, or the first in a group of "friends".
 * This fixes bugs with code relying on hack_m_idx_ii, which gets clobbered
 * when filling in friends and escorts.
 */
mon_ptr place_monster_aux(int who, point_t pos, int r_idx, u32b mode)
{
    mon_ptr       result = NULL;
    int           i;
    int           m_idx = 0;
    monster_race *r_ptr = mon_race_lookup(r_idx);
    pack_info_t  *pack_ptr = NULL;

    if (!(mode & PM_NO_KAGE) && one_in_(333))
        mode |= PM_KAGE;

    if ((mode & PM_FORCE_PET) && (!allow_pets || p_ptr->pclass == CLASS_PSION))
        mode &= (~PM_FORCE_PET);

    /* Place one monster, or fail */
    m_idx = place_monster_one(who, pos, r_idx, 0, mode);
    if (!m_idx) return NULL;
    result = dun_mon(cave, m_idx);

    /* Friends and Escorts */
    if (mode & PM_ALLOW_GROUP)
    {
        assert(hack_m_idx_ii == m_idx);
        place_monster_m_idx = m_idx;

        if (r_ptr->flags1 & RF1_FRIENDS)
        {
            if (0 < r_ptr->pack_pct && randint1(100) > r_ptr->pack_pct)
            {
            }
            else
            {
                pack_ptr = dun_mgr_alloc_pack();
                result->pack_idx = pack_ptr->pack_idx;
                pack_ptr->count++;
                place_monster_group(who, pos, r_idx, pack_ptr->pack_idx, mode);
                pack_choose_ai(m_idx);
            }
        }
        else if (r_ptr->flags1 & RF1_ESCORT)
        {
            pack_ptr = dun_mgr_alloc_pack();

            result->pack_idx = pack_ptr->pack_idx;
            pack_ptr->leader_idx = m_idx;
            pack_ptr->count++;

            /* Set the escort index */
            place_monster_idx = r_idx;

            /* Try to place several "escorts" */
            for (i = 0; i < 32; i++)
            {
                mon_race_ptr escort;
                point_t mp = scatter(pos, 3);

                if (!cave_empty_at(mp)) continue;

                mon_alloc_push_filter(place_monster_okay);
                mon_alloc_push_filter(mon_alloc_feat_p(cave_at(mp)->feat));
                escort = mon_alloc_choose(r_ptr->level);
                mon_alloc_pop_filter();
                mon_alloc_pop_filter();
                if (!escort) break;
                place_monster_one(place_monster_m_idx, mp, escort->id, pack_ptr->pack_idx, mode);
            }
            pack_choose_ai(m_idx);
        }
    }

    /* Give uniques variable AI strategies. We do this as a hack, using the
       existing pack code, by creating a "pack of 1".
                                        v---- Mercy!*/
    if ((r_ptr->flags1 & RF1_UNIQUE) && !(r_ptr->flagsx & RFX_QUESTOR) && !(r_ptr->flags7 & RF7_GUARDIAN))
    {
        if (!pack_ptr)
        {
            pack_ptr = dun_mgr_alloc_pack();
            result->pack_idx = pack_ptr->pack_idx;
            pack_ptr->count++;
            pack_choose_ai(m_idx);
        }
    }

    /* Success */
    return result;
}

static bool mon_alloc_summon(mon_race_ptr race)
    { return summon_specific_okay(race->id); }

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
        mon_alloc_push_filter(mon_alloc_feat_p(cave_at(pos)->feat));

    race = mon_alloc_choose(level);
    mon_alloc_pop_filter();

    if (!race) return FALSE;

    /* Hack: Warlock pact monsters are occasionally friendly */
    if (p_ptr->pclass == CLASS_WARLOCK)
    {
        if ( warlock_is_pact_monster(race)
          && !(mode & PM_QUESTOR) /* RFX_QUESTOR is *not* set for non-unique quest monsters */
          && one_in_(12 - p_ptr->lev/5) )
        {
            mode |= PM_FORCE_FRIENDLY;
        }
    }

    return place_monster_aux(0, pos, race->id, mode);
}


#ifdef MONSTER_HORDES

bool alloc_horde(point_t pos, int level)
{
    point_t last = pos;
    mon_race_ptr race = NULL;
    mon_ptr mon = NULL;
    int attempts = 1000;

    mon_alloc_push_filter(mon_alloc_feat_p(cave_at(pos)->feat));
    while (--attempts)
    {
        race = mon_alloc_choose_aux(level, GMN_NO_UNIQUES);
        if (!race) break;
        break;
    }
    mon_alloc_pop_filter();
    if (!race) return FALSE;
    if (attempts < 1) return FALSE;

    attempts = 1000;

    while (--attempts && !mon)
        mon = place_monster_aux(0, pos, race->id, 0);

    if (!mon) return FALSE;

    if (mon->mflag2 & MFLAG2_CHAMELEON) race = mon_race(mon);
    summon_kin_type = race->d_char;

    for (attempts = damroll(3, 3); attempts; attempts--)
    {
        point_t next = scatter(last, 5);
        summon_specific(mon->id, next, cave->dun_lvl, SUMMON_KIN, 0);
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

        if (!cave_empty_at(pos)) continue;
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
      && randint1(5000) <= cave->dun_lvl )
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
static bool summon_specific_okay(int r_idx)
{
    monster_race *r_ptr = mon_race_lookup(r_idx);

    /* Hack - Only summon dungeon monsters */
    if (!mon_hook_dungeon(r_idx)) return (FALSE);

    #if 0
    if (summon_wall_scummer)
    {
        bool ok = FALSE;
        if (r_ptr->flags2 & RF2_KILL_WALL)
            ok = TRUE;
        if (r_ptr->flags2 & RF2_PASS_WALL)
            ok = TRUE;
        if (r_ptr->flags4 & RF4_BR_DISI)
            ok = TRUE;
        if ((r_ptr->flags6 & RF6_TELE_TO) && r_ptr->level >= 40)
            ok = TRUE;
        if (!ok)
            return FALSE;
    }
    #endif
    if (summon_ring_bearer)
    {
        if (!mon_is_type(r_idx, SUMMON_RING_BEARER))
            return FALSE;
    }

    /* Hack -- identify the summoning monster */
    if (summon_specific_who > 0)
    {
        monster_type *m_ptr = dun_mon(cave, summon_specific_who);

        /* Do not summon enemies */

        /* Friendly vs. opposite aligned normal or pet */
        if (monster_has_hostile_align(m_ptr, 0, 0, r_ptr)) return FALSE;
    }
    /* Use the player's alignment */
    else if (summon_specific_who < 0)
    {
        /* Do not summon enemies of the pets */
        if (monster_has_hostile_align(NULL, 10, -10, r_ptr))
        {
            if (!one_in_(ABS(p_ptr->align) / 2 + 1)) return FALSE;
        }
    }

    /* Hack -- no specific type specified */
    if (!summon_specific_type) return (TRUE);

    if (!summon_unique_okay && ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags7 & RF7_NAZGUL))) return FALSE;

    if ((summon_specific_who < 0) &&
        ((r_ptr->flags1 & RF1_UNIQUE) || (r_ptr->flags7 & RF7_NAZGUL)) &&
        monster_has_hostile_align(NULL, 10, -10, r_ptr))
        return FALSE;

    return (summon_specific_aux(r_idx));
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
bool summon_specific(int who, point_t tgt_pos, int lev, int type, u32b mode)
{
    point_t pos = tgt_pos;
    mon_race_ptr race = NULL;
    bool mon_summoned = FALSE;
    int options = GMN_DEFAULT;

    if (who > 0 && mon_spell_current() && mon_spell_current()->id.type == MST_SUMMON)
        mon_summoned = TRUE;

    /* Note: summon_specific does not imply summoning at all, despite the name ...
       Let's try to guess whether or not this is really a summon spell ...
    */
    if (mon_summoned || who == SUMMON_WHO_PLAYER)
    {
        if (p_ptr->anti_summon && !one_in_(3))
        {
            msg_format("The summoning is blocked!");
            return FALSE;
        }
    }
    if (!mon_scatter(0, &pos.y, &pos.x, tgt_pos.y, tgt_pos.x, 2)) return FALSE;

    /* Save the summoner */
    summon_specific_who = who;

    /* Save the "summon" type */
    summon_specific_type = type;
    if ((type == SUMMON_BIZARRE1 || type == SUMMON_THIEF) && who == -1)
        options |= GMN_IGNORE_MAX_LEVEL;

    summon_unique_okay = (mode & PM_ALLOW_UNIQUE) ? TRUE : FALSE;
    summon_cloned_okay = (mode & PM_ALLOW_CLONED) ? TRUE : FALSE;
    summon_wall_scummer = (mode & PM_WALL_SCUMMER) && one_in_(2) ? TRUE : FALSE;
    summon_ring_bearer = (mode & PM_RING_BEARER) ? TRUE : FALSE;

    if (summon_specific_who != SUMMON_WHO_NOBODY)
        options |= GMN_IGNORE_MAX_LEVEL;

    mon_alloc_push_filter(mon_alloc_summon);
    mon_alloc_push_filter(mon_alloc_feat_p(cave_at(pos)->feat));
    race = mon_alloc_choose_aux((cave->difficulty + lev) / 2 + 5, options);
    if (!race && summon_wall_scummer)
    {
        summon_wall_scummer = FALSE;
        race = mon_alloc_choose_aux((cave->difficulty + lev) / 2 + 5, options);
    }
    if (!race && summon_ring_bearer)
    {
        summon_ring_bearer = FALSE;
        race = mon_alloc_choose_aux((cave->difficulty + lev) / 2 + 5, options);
    }
    mon_alloc_pop_filter();
    mon_alloc_pop_filter();

    summon_cloned_okay = FALSE; /* This is a hack for RF6_S_UNIQUE ... XXXget_mon_num() is much more widely used, however! */
                                /* place_monster_aux() will now handle setting the unique as "cloned" if appropriate */
    /* Handle failure */
    if (!race)
    {
        summon_specific_type = 0;
        summon_specific_who = 0;
        return FALSE;
    }

    if ((type == SUMMON_BLUE_HORROR) || (type == SUMMON_DAWN)) mode |= PM_NO_KAGE;

    if (mon_summoned && p_ptr->cult_of_personality)
    {
        if (one_in_(2) && !mon_save_p(race->id, A_CHR))
        {
            mode |= PM_FORCE_FRIENDLY;
            if (!mon_save_p(race->id, A_CHR))
                mode |= PM_FORCE_PET;
        }
    }

    /* Attempt to place the monster (awake, allow groups) */
    if (!place_monster_aux(who, pos, race->id, mode))
    {
        summon_specific_type = 0;
        summon_specific_who = 0;
        return FALSE;
    }

    summon_specific_type = 0;
    summon_specific_who = 0;
    /* Success */
    return TRUE;
}

/* A "dangerous" function, creates a pet of the specified type */
mon_ptr summon_named_creature(int who, point_t tgt_pos, int r_idx, u32b mode)
{
    mon_ptr result = NULL;
    point_t pos;
    monster_race *r_ptr;

    if (r_idx >= max_r_idx) return FALSE;
    if (p_ptr->anti_summon && !one_in_(3))
    {
        msg_format("The summoning is blocked!");
        return FALSE;
    }
    if (!mon_scatter(r_idx, &pos.y, &pos.x, tgt_pos.y, tgt_pos.x, 2)) return FALSE;

    r_ptr = mon_race_lookup(r_idx);

    if ((!(r_ptr->flags7 & RF7_GUARDIAN) || p_ptr->wizard) && r_ptr->cur_num < r_ptr->max_num)
        result = place_monster_aux(who, pos, r_idx, mode | PM_NO_KAGE);

    return result;
}


/*
 * Let the given monster attempt to reproduce.
 *
 * Note that "reproduction" REQUIRES empty space.
 */
bool multiply_monster(int m_idx, bool clone, u32b mode)
{
    mon_ptr parent = dun_mon(cave, m_idx);
    mon_ptr child;

    int y, x;

    if (!mon_scatter(parent->r_idx, &y, &x, parent->pos.y, parent->pos.x, 1))
        return FALSE;

    if (parent->mflag2 & MFLAG2_NOPET) mode |= PM_NO_PET;

    /* Create a new monster (awake, no groups) */
    child = place_monster_aux(m_idx, point_create(x, y), parent->r_idx, (mode | PM_NO_KAGE | PM_MULTIPLY));
    if (!child)
        return FALSE;

    /* Hack -- Transfer "clone" flag */
    if (clone || (parent->smart & (1U << SM_CLONED)))
    {
        child->smart |= (1U << SM_CLONED);
        child->mflag2 |= MFLAG2_NOPET;
    }

    return TRUE;
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
    monster_race *r_ptr = mon_race(m_ptr);

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
    if (my_strchr(",ejmvwQ", r_ptr->d_char))
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
    else if (my_strchr("l", r_ptr->d_char))
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
    else if (my_strchr("g#+<>", r_ptr->d_char))
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
    else if (my_strchr("JMR", r_ptr->d_char) || !isalpha(r_ptr->d_char))
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
    else if (my_strchr("f", r_ptr->d_char))
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
    else if (my_strchr("acFIKS", r_ptr->d_char))
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
    else if (my_strchr("B", r_ptr->d_char))
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
    else if (my_strchr("duDLUW", r_ptr->d_char))
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
    else if (my_strchr("s", r_ptr->d_char))
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
    else if (my_strchr("z", r_ptr->d_char))
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
    else if (my_strchr("G", r_ptr->d_char))

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
    else if (my_strchr("CZ", r_ptr->d_char))
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
    else if (my_strchr("Xbilqrt", r_ptr->d_char))
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
void update_smart_learn(int m_idx, int what)
{
    monster_type *m_ptr;
    monster_race *r_ptr;

    if (m_idx <= 0) return; /* paranoia */

    /* Not allowed to learn */
    if (!smart_learn) return;

    m_ptr = dun_mon(cave, m_idx);
    if (!m_ptr) return;
    r_ptr = mon_race(m_ptr);

    /* Too stupid to learn anything */
    if (r_ptr->flags2 & (RF2_STUPID)) return;

    /* Not intelligent, only learn sometimes */
    if (!(r_ptr->flags2 & (RF2_SMART)) && (randint0(100) < 50)) return;

    /* Analyze the knowledge */
    assert(0 <= what && what < 32);
    m_ptr->smart |= (1U << what);
}
