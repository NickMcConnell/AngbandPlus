/* File: monster3.c */

/* Purpose: Monsters AI */

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "angband.h"

/*
 * Is the mon,ster in friendly state(pet, friend, ..)
 * -1 = enemy, 0 = neutral, 1 = friend
 */
int is_friend(monster_type *m_ptr)
{
        switch (m_ptr->status)
        {                
                /* pets/friends/companion attacks monsters */
                case MSTATUS_NEUTRAL_P:
                case MSTATUS_FRIEND:
                case MSTATUS_PET:
                case MSTATUS_COMPANION:
                        return 1;
                        break;
                case MSTATUS_NEUTRAL_M:
                case MSTATUS_ENEMY:
                        return -1;
                        break;
                case MSTATUS_NEUTRAL:
                        return 0;
                        break;
        }

        /* OUPS */
        return (0);
}

/* Should they attack each others */
bool is_enemy(monster_type *m_ptr, monster_type *t_ptr)
{
        monster_race *r_ptr = &r_info[m_ptr->r_idx], *rt_ptr = &r_info[t_ptr->r_idx];
        int s1 = is_friend(m_ptr), s2 = is_friend(t_ptr);
#if 0
        /* Stupid monsters attacks just about everything */
        if ((r_ptr->flags2 & RF2_STUPID) && (r_ptr->d_char != rt_ptr->d_char)) return TRUE;
#endif
        /* Monsters hates breeders */
        if ((m_ptr->status != MSTATUS_NEUTRAL) && (rt_ptr->flags4 & RF4_MULTIPLY) && (num_repro > MAX_REPRO * 2 / 3) && (r_ptr->d_char != rt_ptr->d_char)) return TRUE;
        if ((t_ptr->status != MSTATUS_NEUTRAL) && (r_ptr->flags4 & RF4_MULTIPLY) && (num_repro > MAX_REPRO * 2 / 3) && (r_ptr->d_char != rt_ptr->d_char)) return TRUE;

        /* No special conditions, lets test normal flags */
        if (s1 && s2 && (s1 == -s2)) return TRUE;

        /* Not ennemy */
        return (FALSE);
}

bool change_side(monster_type *m_ptr)
{
        /* neutrals and companions  */
        switch (m_ptr->status)
        {
#if 0
                case MSTATUS_ENEMY:
                        m_ptr->status = MSTATUS_FRIEND;
                        break;
#endif
                case MSTATUS_FRIEND:
                        m_ptr->status = MSTATUS_ENEMY;
                        break;
                case MSTATUS_NEUTRAL_P:
                        m_ptr->status = MSTATUS_NEUTRAL_M;
                        break;
                case MSTATUS_NEUTRAL_M:
                        m_ptr->status = MSTATUS_NEUTRAL_P;
                        break;
                case MSTATUS_PET:
                        m_ptr->status = MSTATUS_ENEMY;
                        break;
                case MSTATUS_COMPANION:
                        return FALSE;
                        break;
        }
        /* changed */
        return TRUE;
}

/* Multiply !! */
bool ai_multiply(int m_idx)
{
        monster_type *m_ptr = &m_list[m_idx];
        monster_race *r_ptr = &r_info[m_ptr->r_idx];
        int k, y, x, oy = m_ptr->fy, ox = m_ptr->fx;
        bool is_frien = (is_friend(m_ptr) > 0);

        /* Count the adjacent monsters */
        for (k = 0, y = oy - 1; y <= oy + 1; y++)
        {
                for (x = ox - 1; x <= ox + 1; x++)
                {
                        if (cave[y][x].m_idx) k++;
                }
        }
		
        if (is_friend(m_ptr) > 0)
        {
                is_frien = TRUE;
        }
        else
        {
                is_frien = FALSE;
        }		
		
        /* Hack -- multiply slower in crowded areas */
        if ((k < 4) && (!k || !rand_int(k * MON_MULT_ADJ)))
        {
                /* Try to multiply */
                if (multiply_monster(m_idx, (is_frien), FALSE))
                {
                        /* Take note if visible */
                        if (m_ptr->ml)
                        {
                                r_ptr->r_flags4 |= (RF4_MULTIPLY);
                        }
				
                        /* Multiplying takes energy */
                        return TRUE;
                }
        }
        return FALSE;
}

/* Possessor incarnates */
bool ai_possessor(int m_idx, int o_idx)
{
        object_type *o_ptr = &o_list[o_idx];
        monster_type *m_ptr = &m_list[m_idx];
        int r_idx = m_ptr->r_idx, r2_idx = o_ptr->pval2;
        int i;
        monster_race *r_ptr = &r_info[r2_idx];
        char m_name[80], m_name2[80];

        monster_desc(m_name, m_ptr, 0x00);
        monster_race_desc(m_name2, r2_idx, 0);

        if (m_ptr->ml) msg_format("%^s incarnates into a %s!", m_name, m_name2);

        /* Remove the old one */
        delete_object_idx(o_idx);

        m_ptr->r_idx = r2_idx;
        m_ptr->ego = 0;

	/* No "damage" yet */
	m_ptr->stunned = 0;
	m_ptr->confused = 0;
	m_ptr->monfear = 0;

        /* No target yet */
        m_ptr->target = -1;

	/* Assume no sleeping */
	m_ptr->csleep = 0;

	/* Assign maximal hitpoints */
	if (r_ptr->flags1 & (RF1_FORCE_MAXHP))
	{
		m_ptr->maxhp = maxroll(r_ptr->hdice, r_ptr->hside);
	}
	else
	{
		m_ptr->maxhp = damroll(r_ptr->hdice, r_ptr->hside);
	}

	/* And start out fully healthy */
	m_ptr->hp = m_ptr->maxhp;

        /* Some basic info */
        for (i = 0; i < 4; i++)
        {
                m_ptr->blow[i].method = r_ptr->blow[i].method;
                m_ptr->blow[i].effect = r_ptr->blow[i].effect;
                m_ptr->blow[i].d_dice = r_ptr->blow[i].d_dice;
                m_ptr->blow[i].d_side = r_ptr->blow[i].d_side;
        }
        m_ptr->ac = r_ptr->ac;
        m_ptr->level = r_ptr->level;
        m_ptr->speed = r_ptr->speed;
        m_ptr->exp = MONSTER_EXP(m_ptr->level);

	/* Extract the monster base speed */
        m_ptr->mspeed = m_ptr->speed;

        m_ptr->energy = 0;

	/* Hack -- Count the number of "reproducers" */
        if (r_ptr->flags4 & (RF4_MULTIPLY)) num_repro++;

	/* Hack -- Notice new multi-hued monsters */
	if (r_ptr->flags1 & (RF1_ATTR_MULTI)) shimmer_monsters = TRUE;

	/* Hack -- Count the monsters on the level */
	r_ptr->cur_num++;
        r_info[r_idx].cur_num--;
        
        m_ptr->possessor = r_idx;

	/* Update the monster */
        update_mon(m_idx, TRUE);

        return TRUE;
}

void ai_deincarnate(int m_idx)
{
        monster_type *m_ptr = &m_list[m_idx];
        int r2_idx = m_ptr->possessor, r_idx = m_ptr->r_idx;
        monster_race *r_ptr = &r_info[r2_idx];
        int i;
        char m_name[80];

        monster_desc(m_name, m_ptr, 0x04);

        if (m_ptr->ml) msg_format("The soul of %s deincarnates!", m_name);

        m_ptr->r_idx = r2_idx;
        m_ptr->ego = 0;

	/* No "damage" yet */
	m_ptr->stunned = 0;
	m_ptr->confused = 0;
	m_ptr->monfear = 0;

        /* No target yet */
        m_ptr->target = -1;

	/* Assume no sleeping */
	m_ptr->csleep = 0;

	/* Assign maximal hitpoints */
	if (r_ptr->flags1 & (RF1_FORCE_MAXHP))
	{
		m_ptr->maxhp = maxroll(r_ptr->hdice, r_ptr->hside);
	}
	else
	{
		m_ptr->maxhp = damroll(r_ptr->hdice, r_ptr->hside);
	}

	/* And start out fully healthy */
	m_ptr->hp = m_ptr->maxhp;

        /* Some basic info */
        for (i = 0; i < 4; i++)
        {
                m_ptr->blow[i].method = r_ptr->blow[i].method;
                m_ptr->blow[i].effect = r_ptr->blow[i].effect;
                m_ptr->blow[i].d_dice = r_ptr->blow[i].d_dice;
                m_ptr->blow[i].d_side = r_ptr->blow[i].d_side;
        }
        m_ptr->ac = r_ptr->ac;
        m_ptr->level = r_ptr->level;
        m_ptr->speed = r_ptr->speed;
        m_ptr->exp = MONSTER_EXP(m_ptr->level);

	/* Extract the monster base speed */
        m_ptr->mspeed = m_ptr->speed;

        m_ptr->energy = 0;

	/* Hack -- Count the number of "reproducers" */
        if (r_ptr->flags4 & (RF4_MULTIPLY)) num_repro++;

	/* Hack -- Notice new multi-hued monsters */
	if (r_ptr->flags1 & (RF1_ATTR_MULTI)) shimmer_monsters = TRUE;

	/* Hack -- Count the monsters on the level */
	r_ptr->cur_num++;
        r_info[r_idx].cur_num--;
        
        m_ptr->possessor = 0;

	/* Update the monster */
        update_mon(m_idx, TRUE);
}

/* Returns if a new companion is allowed */
bool can_create_companion()
{
        int i, mcnt = 0;

	for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
                monster_type *m_ptr = &m_list[i];

		/* Ignore "dead" monsters */
		if (!m_ptr->r_idx) continue;

                if (m_ptr->status == MSTATUS_COMPANION) mcnt++;
        }

        switch (p_ptr->pclass)
        {
                case CLASS_HARPER:
                        if (mcnt < 5) return (TRUE);
                        else return (FALSE);
                case CLASS_BEASTMASTER:
                        if (mcnt < 4 + (p_ptr->lev / 10)) return (TRUE);
                        else return (FALSE);
                default:
                        if (mcnt < 2) return (TRUE);
                        else return (FALSE);
        }
}
