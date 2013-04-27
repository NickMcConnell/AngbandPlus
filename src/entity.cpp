// File: entity.cpp
// Purpose: Functions for entities and projectiles

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"


// Get the cave pointer
CGrid *CEntity::get_g_ptr(void)
{
    return &cave[loc.y][loc.x];
}


/*
 * Initialize an arrow
 */
CArrow::CArrow(double xx, double yy, double zz, double vxx, double vyy, double vzz, CLiving *w,
    CItem *arrow, int ch, int td)
{
    x = xx; y = yy; z = zz;
    vx = vxx; vy = vyy; vz = vzz;
    purge = FALSE;
    who = w;
    i_ptr = new CItem;
    *i_ptr = *arrow;
    chance = ch;
    damage = td;
}


/*
 * Determines the odds of an object breaking when thrown
 * Note that "impact" is true if the object hit a monster
 * Artifacts never break, see the "drop_near()" function.
 * Assume the object has NOT hit a wall or monster
 * Hitting a monster doubles the breakage chance
 */
static int breakage_chance(CItem *i_ptr, bool hit_body)
{
    int x;

    // Examine the item type
    switch (i_ptr->GetTval()) {
        // Burning flasks
        case TV_FLASK:
            x = 100;
            break;

        // Very breakable objects
        case TV_POTION:
        case TV_BOTTLE:
        case TV_FOOD:
            x = 50;
            break;

        // Somewhat breakable objects
        case TV_LITE:
        case TV_SCROLL:
        case TV_ARROW:
        case TV_SKELETON:
            x = 30;
            break;

        // Slightly breakable objects
        case TV_WAND:
        case TV_SHOT:
        case TV_BOLT:
            x = 20;
            break;

        // Normal objects
        default:
            x = 10;
            break;
    }

    // Double if it hit a monster
    if (hit_body) x *= 2;

    // Prevent excessive chance
    if (x > 100) x = 100;

    // Return x
    return x;
}


/*
 * Process a projectile (done every turn)
 */
void CArrow::Process(void)
{
    int old_x, old_y;
    int map_x, map_y;
    CGrid *g_ptr;

    // If it needs to be purged, ignore it
    if (purge) return;

    // Get old location
    GetIntLoc(&old_x, &old_y);

    // Move it
    x += vx; y += vy; z += vz;
    vz -= 0.001;

    // Get new location
    GetIntLoc(&map_x, &map_y);

    // If we're off the map, purge
    if (!in_bounds(map_y, map_x)) {
        HitEdge();
        return;
    }

    // Can we pass through it?
    if (!floor_grid_bold(map_y, map_x)) {
        HitWall(old_x, old_y);
        return;
    }

    // Have we hit the floor?
    if (z <= 0) {
        HitFloor();
        return;
    }

    // Have we hit the player?
    if (p_ptr->is_at(map_x, map_y) && (who != p_ptr)) {
        HitPlayer();
        return;
    }

    // Get grid
    g_ptr = &cave[map_y][map_x];

    // Have we hit a monster?
    if (g_ptr->m_ptr && (who != g_ptr->m_ptr)) {
        HitMonster();
        return;
    }
}


/*
 * Get integer coordinates
 */
void CArrow::GetIntLoc(int *xx, int *yy)
{
    *xx = (int) floor(x+0.5);
    *yy = (int) floor(y+0.5);
}

void CArrow::HitWall(int old_x, int old_y)
{
    // Drop it at that place
    drop_near(i_ptr, breakage_chance(i_ptr, FALSE), old_y, old_x);

    // Purge it
    purge = TRUE;
}

void CArrow::HitFloor(void)
{
    int x, y;

    // Get location
    GetIntLoc(&x, &y);

    // Drop it at that place
    drop_near(i_ptr, breakage_chance(i_ptr, FALSE), y, x);

    // Purge it
    purge = TRUE;
}

void CArrow::HitPlayer(void)
{
    int x, y;

    // Get location
    GetIntLoc(&x, &y);

    // Drop it at that place
    drop_near(i_ptr, breakage_chance(i_ptr, TRUE), y, x);

    // Purge it
    purge = TRUE;
}


/*
 * An arrow hits a monster
 */
void CArrow::HitMonster(void)
{
    int map_x, map_y;
    char i_name[80];
    int tdam = damage;

    // Describe the object
    i_ptr->object_desc(i_name, FALSE, 3);

    // Get the location
    GetIntLoc(&map_x, &map_y);

    // Get the grid
    CGrid *g_ptr = &cave[map_y][map_x];

    // Get the monster
    CMonster *m_ptr = g_ptr->m_ptr;
    if (!m_ptr) return;

    // Get the race
    CMonsterRace *r_ptr = m_ptr->get_r_ptr();

    // Check the visibility
    bool visible = m_ptr->is_visible();

    // Did we hit it (penalize range)
    //: Penalize chance to chance-cur_dis
    if (test_hit(chance, r_ptr->ac, visible)) {
        bool fear = FALSE;

        // Assume a default death
        char *note_dies = " dies.";

        // Some monsters get "destroyed"
        if (r_ptr->isNonLiving() || (r_ptr->flags2 & RF2_STUPID)) {
            // Special note at death
            note_dies = " is destroyed.";
        }


        // Handle unseen monster
        if (!visible) {
            // Invisible monster
            msg_format("The %s finds a mark.", i_name);
        }

        // Handle visible monster
        else {
            char m_name[80];

            // Get "the monster" or "it"
            m_ptr->get_desc(m_name, 0);

            // Message
            msg_format("The %s hits %s.", i_name, m_name);
        }

        // Apply special damage XXX XXX XXX
        tdam = tot_dam_aux(i_ptr, tdam, m_ptr);
        tdam = critical_shot(i_ptr->GetWeight(), i_ptr->GetToH(), tdam);

        // No negative damage
        if (tdam < 0) tdam = 0;

        // Hit the monster, check for death
        if (m_ptr->mon_take_hit(tdam, &fear, note_dies)) {
            // Dead monster
        }

        // No death
        else {
            // Message
            m_ptr->message_pain(tdam);

            // Take note
            if (fear && m_ptr->is_visible()) {
                char m_name[80];

                //# Flee

                // Get the monster name (or "it")
                m_ptr->get_desc(m_name, 0);

                // Message
                msg_format("%^s flees in terror!", m_name);
            }
        }

        // Drop it at that place
        drop_near(i_ptr, breakage_chance(i_ptr, TRUE), map_y, map_x);

        // Purge
        purge = TRUE;
    }
}


/*
 * Process all the projectiles
 */
void process_projectiles(void)
{
    for (int i = 0; i < MAX_PROJECTILES; i++) {
        if (!arrows[i]) continue;
        if (arrows[i]->ToBePurged()) {
            delete arrows[i];
            arrows[i] = NULL;
            continue;
        }
        arrows[i]->Process();
    }
}


/*
 * Add a new projectile
 */
void add_projectile(CArrow *proj)
{
    for (int i = 0; i < MAX_PROJECTILES; i++) {
        if (arrows[i]) continue;
        arrows[i] = proj;
        return;
    }
    msg_print("Projectile overflow!");
    delete proj;
}


/*
 * Initialize a game object
 */
static guid_type cur_guid = 1;
CGameObject::CGameObject()
{
    guid = cur_guid++;
}
