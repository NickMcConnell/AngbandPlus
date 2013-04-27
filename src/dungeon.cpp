// File: dungeon.cpp
// Purpose: Utumno game engine

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"

static int MAP_START_X, MAP_START_Y, MAP_END_X, MAP_END_Y;
int MAP_CENTER_X, MAP_CENTER_Y;


/*
 * Get the isometric tile which the mouse is on.
 */
static void calc_mouse_map_xy(int mx, int my, bool *mom, int *mmx, int *mmy)
{
    int sx, sy;
    int offset_x, offset_y;
    int px, py;

    // If not on the map, break out here
    if ((mx < MAP_START_X) || (mx > MAP_END_X) || (my < MAP_START_Y) || (my > MAP_END_Y)) {
        *mom = FALSE;
        return;
    }

    // Mouse is on map
    *mom = TRUE;

    // Translate by map's center
    mx -= MAP_CENTER_X;
    my -= MAP_CENTER_Y;

    // Translate based on player's location
    p_ptr->GetSubtilePosition(&offset_x, &offset_y);
    subtile_to_pixel(offset_x, offset_y, &px, &py);
    mx += px;
    my += py;

    // Fix for negative numbers
    mx += ISO_ACROSS*500;
    my += ISO_DOWN*500;

    // Calculate sector
    sx = mx / ISO_ACROSS;
    sy = my / ISO_DOWN;

    // Get index inside sector
    mx = mx % ISO_ACROSS;
    my = my % ISO_DOWN;

    // Divide x in half
    mx /= 2;

    // Get base location
    *mmx = sx/2 + sy/2 - 500;
    *mmy = sy/2 - sx/2;

    // Find exact tile by splitting on the type of sector
    if (sx % 2) {
        if (sy % 2) {
            // sx, sy odd
            if (mx+my > 14) *mmx += 1;
        }
        else {
            // sx odd, sy even
            if (mx > my) *mmy -= 1;
        }
    }
    else {
        if (sy % 2) {
            // sx even, sy odd
            if (mx <= my) *mmy += 1;
        }
        else {
            // sx, sy even
            if (mx+my <= 14) *mmx -= 1;
        }
    }

    // Is it off the map in terms of map size?
    if ((*mmx < 0) || (*mmx >= cur_wid)) *mom = 0;
    if ((*mmy < 0) || (*mmy >= cur_hgt)) *mom = 0;
}


/*
 * Prints stuff at bottom of screen
 */
static void prt_bottom(void)
{
    char tmp[32];
    int chp, mhp, csp, msp, pixels, busy, i;

    put_text_format(50, 356, "Hit Points", COLOR_BLACK, FONT_BOLD, JUST_CENTER);
    put_text_format(50, 381, "Spell Points", COLOR_BLACK, FONT_BOLD, JUST_CENTER);
    put_text_format(50, 406, "Energy", COLOR_BLACK, FONT_BOLD, JUST_CENTER);
    put_text_format(50, 431, "Speed", COLOR_BLACK, FONT_BOLD, JUST_CENTER);
    draw_pane(100, 355, 200, 371, COLOR_BLACK);
    draw_pane(100, 380, 200, 396, COLOR_BLACK);
    draw_pane(100, 405, 200, 421, COLOR_BLACK);
    draw_pane(100, 430, 200, 446, COLOR_BLACK);

    chp = p_ptr->GetCHP(); mhp = p_ptr->GetMHP();
    csp = p_ptr->GetCSP(); msp = p_ptr->GetMSP();
    busy = p_ptr->GetBusy();

    if (mhp > 0) pixels = 97*chp/mhp;
    else pixels = 0;
    box(102, 357, 101+pixels, 369, COLOR_RED);

    if (msp > 0) pixels = 97*csp/msp;
    else pixels = 0;
    box(102, 382, 101+pixels, 394, 200);

    pixels = (100-busy)*97/100;
    box(102, 407, 101+pixels, 419, COLOR_GREEN);

    sprintf(tmp, "%d/%d", chp, mhp);
    put_text_format(150, 356, tmp, COLOR_WHITE, FONT_BOLD, JUST_CENTER);
    sprintf(tmp, "%d/%d", csp, msp);
    put_text_format(150, 381, tmp, COLOR_WHITE, FONT_BOLD, JUST_CENTER);
    sprintf(tmp, "%d/100", 100-busy);
    put_text_format(150, 406, tmp, COLOR_WHITE, FONT_BOLD, JUST_CENTER);

    // Speed
    i = p_ptr->get_speed();

    // Fast
    if (i > 110) {
        sprintf(tmp, "Fast (+%d)", i-110);
    }

    // Slow 
    else if (i < 110) {
        sprintf(tmp, "Slow (-%d)", 110-i);
    }

    // Normal
    else {
        strcpy(tmp, "Normal");
    }

    // Display
    put_text_format(150, 431, tmp, COLOR_WHITE, FONT_BOLD, JUST_CENTER);
}


static void prt_cut(int *y)
{
    int val = p_ptr->GetCut();
    byte attr;
    char *s;

    if (val > 1000) {
        attr = COLOR_LT_RED;
        s = "Mortally wounded";
    }
    else if (val > 200) {
        attr = COLOR_RED;
        s = "Severely cut";
    }
    else if (val > 100) {
        attr = COLOR_ORANGE;
        s = "Nastily cut";
    }
    else if (val > 50) {
        attr = COLOR_ORANGE;
        s = "Badly cut";
    }
    else if (val > 10) {
        attr = COLOR_YELLOW;
        s = "Lightly cut";
    }
    else if (val) {
        attr = COLOR_YELLOW;
        s = "Grazed";
    }
    else return;

    // Display
    put_text(MAP_START_X, *y, s, COLOR_ORANGE, FONT_BOLD);
    *y -= 16;
}


static void prt_stun(int *y)
{
    int val = p_ptr->GetStun();
    byte attr;
    char *s;

    if (val > 100) {
        attr = COLOR_RED;
        s = "Knocked out";
    }
    else if (val > 50) {
        attr = COLOR_ORANGE;
        s = "Heavy stunned";
    }
    else if (val) {
        attr = COLOR_ORANGE;
        s = "Stunned";
    }
    else return;

    // Display
    put_text(MAP_START_X, *y, s, COLOR_ORANGE, FONT_BOLD);
    *y -= 16;
}


/*
 * Prints status of hunger
 */
static void prt_hunger(int *y)
{
    byte attr;
    char *s;

    // Fainting / Starving
    if (p_ptr->GetFood() < PY_FOOD_FAINT) {
        attr = COLOR_RED;
        s = "Starving";
    }

    // Weak
    else if (p_ptr->GetFood() < PY_FOOD_WEAK) {
        attr = COLOR_ORANGE;
        s = "Weak";
    }

    // Hungry
    else if (p_ptr->GetFood() < PY_FOOD_ALERT) {
        attr = COLOR_YELLOW;
        s = "Hungry";
    }

    // Normal
    else if (p_ptr->GetFood() < PY_FOOD_FULL) return;

    // Full
    else if (p_ptr->GetFood() < PY_FOOD_MAX) {
        attr = COLOR_LT_GREEN;
        s = "Full";
    }

    // Gorged
    else {
        attr = COLOR_GREEN;
        s = "Gorged";
    }

    // Display
    put_text(MAP_START_X, *y, s, COLOR_ORANGE, FONT_BOLD);
    *y -= 16;
}


/*
 * Prints statuses: blindness, confusion, fear, poison
 */
static void prt_status(void)
{
    int y = MAP_END_Y-15;

    // Study
    if (p_ptr->GetNewSpells()) {
        put_text(MAP_START_X, y, "Can learn spells", COLOR_ORANGE, FONT_BOLD);
        y -= 16;
    }

    // Cut
    prt_cut(&y);

    // Stun
    prt_stun(&y);

    // Paralysis
    if (p_ptr->GetParalyzed()) {
        put_text(MAP_START_X, y, "Paralyzed", COLOR_ORANGE, FONT_BOLD);
        y -= 16;
    }

    // Confusion
    if (p_ptr->GetConfused()) {
        put_text(MAP_START_X, y, "Confused", COLOR_ORANGE, FONT_BOLD);
        y -= 16;
    }

    // Poison
    if (p_ptr->GetPoisoned()) {
        put_text(MAP_START_X, y, "Poisoned", COLOR_ORANGE, FONT_BOLD);
        y -= 16;
    }

    // Fear
    if (p_ptr->GetAfraid()) {
        put_text(MAP_START_X, y, "Afraid", COLOR_ORANGE, FONT_BOLD);
        y -= 16;
    }

    // Blindness
    if (p_ptr->GetBlind()) {
        put_text(MAP_START_X, y, "Blind", COLOR_ORANGE, FONT_BOLD);
        y -= 16;
    }

    // Resting
    if (resting) {
        put_text(MAP_START_X, y, "Resting", COLOR_ORANGE, FONT_BOLD);
        y -= 16;
    }

    // Hunger
    prt_hunger(&y);
}


/*
 * Prints the map of the dungeon, with depth
 */
static void prt_map(int mx, int my)
{
    int x, y, px, py, i, j, center_x, center_y;
    char depths[32];
    bool hilited, mouse_on_map;
    int mouse_map_x, mouse_map_y;
    int player_x, player_y;
    int player_px, player_py;

    // Get mouse location
    calc_mouse_map_xy(mx, my, &mouse_on_map, &mouse_map_x, &mouse_map_y);

    // Get player location
    p_ptr->GetLocation(&center_x, &center_y);

    // Calculate player's px, py
    p_ptr->GetSubtilePosition(&player_x, &player_y);
    subtile_to_pixel(player_x, player_y, &player_px, &player_py);

    // Draw layer 1 (floor)
    for (i = -6; i < 8; i++) {
        for (j = -6; j < 8; j++) {
            x = i+j-1+center_x;
            y = i-j+center_y;
            px = x*32-y*32-player_px+MAP_CENTER_X;
            py = x*16+y*16-player_py+MAP_CENTER_Y;
            hilited = (mouse_on_map && (x == mouse_map_x) && (y == mouse_map_y));
            draw_layer_1(x, y, px, py, hilited);
        }
        for (j = -6; j < 7; j++) {
            x = i+j+center_x;
            y = i-j+center_y;
            px = x*32-y*32-player_px+MAP_CENTER_X;
            py = x*16+y*16-player_py+MAP_CENTER_Y;
            hilited = (mouse_on_map && (x == mouse_map_x) && (y == mouse_map_y));
            draw_layer_1(x, y, px, py, hilited);
        }
    }

    // Start layer 2
    start_layer_2();

    // Add tile-based parts of layer 2
    for (i = -6; i < 8; i++) {
        for (j = -6; j < 8; j++) {
            x = i+j-1+center_x;
            y = i-j+center_y;
            px = x*32-y*32-player_px+MAP_CENTER_X;
            py = x*16+y*16-player_py+MAP_CENTER_Y;
            draw_layer_2(x, y, px, py, player_x, player_y);
        }
        for (j = -6; j < 7; j++) {
            x = i+j+center_x;
            y = i-j+center_y;
            px = x*32-y*32-player_px+MAP_CENTER_X;
            py = x*16+y*16-player_py+MAP_CENTER_Y;
            draw_layer_2(x, y, px, py, player_x, player_y);
        }
    }

    // Add player
    p_ptr->Draw(MAP_CENTER_X, MAP_CENTER_Y);


    // Projectiles
    for (i = 0; i < MAX_PROJECTILES; i++) {
        double x, y;

        // Skip non-arrows
        if (!arrows[i]) continue;

        // Get exact location
        arrows[i]->GetRealLoc(&x, &y);

        // Get subtile x, y
        int subtile_x = (int) (x*1000.0);
        int subtile_y = (int) (y*1000.0);

        // Get screen x, y, depth
        s32b depth = subtile_x + subtile_y;
        subtile_x -= player_x;
        subtile_y -= player_y;
        int sx, sy;
        subtile_to_pixel(subtile_x, subtile_y, &sx, &sy);
        sx += MAP_CENTER_X;
        sy += MAP_CENTER_Y;
        sy -= (int) (arrows[i]->GetHeight()*10);

        // Add sprite
        add_sprite(depth, sx, sy, "items/ammo/arrow", FALSE, "base", 0, 0);
    }

    // Draw layer 2
    end_layer_2();

    // Display depth
    if (!dun_level) {
        strcpy(depths, "Town");
    }
    else {
        sprintf(depths, "%d ft", dun_level*50);
    }
    put_text_format(MAP_END_X, MAP_END_Y-15, depths, COLOR_ORANGE, FONT_BOLD, JUST_RIGHT);
}


/*
 * Minimap
 */
static void prt_minimap(void)
{
    int p_x = p_ptr->GetX(), p_y = p_ptr->GetY();

    start_pixel_draw();
    for (int x = 1; x < cur_wid-1; x++) {
        int rx = x-p_x;
        for (int y = 1; y < cur_hgt-1; y++) {
            int ry = y-p_y;
            int px = MAP_CENTER_X+34+(rx-ry)*4;
            int py = MAP_CENTER_Y+1+(rx+ry)*2;

            if (p_ptr->is_at(x, y)) {
                draw_pixel(px, py, COLOR_WHITE);
            }
            if (!cave[y][x].is_wall() && (cave[y][x].flags & MAP_KNOW)) {
                if (cave[y][x-1].is_wall() && (cave[y][x-1].flags & MAP_KNOW)) {
                    draw_pixel(px-4, py, COLOR_ORANGE);
                    draw_pixel(px-3, py, COLOR_ORANGE);
                    draw_pixel(px-2, py-1, COLOR_ORANGE);
                    draw_pixel(px-1, py-1, COLOR_ORANGE);
                    draw_pixel(px, py-2, COLOR_ORANGE);
                    draw_pixel(px+1, py-2, COLOR_ORANGE);
                }
                if (cave[y][x+1].is_wall() && (cave[y][x+1].flags & MAP_KNOW)) {
                    draw_pixel(px, py+2, COLOR_ORANGE);
                    draw_pixel(px+1, py+2, COLOR_ORANGE);
                    draw_pixel(px+2, py+1, COLOR_ORANGE);
                    draw_pixel(px+3, py+1, COLOR_ORANGE);
                    draw_pixel(px+4, py, COLOR_ORANGE);
                    draw_pixel(px+5, py, COLOR_ORANGE);
                }
                if (cave[y-1][x].is_wall() && (cave[y-1][x].flags & MAP_KNOW)) {
                    draw_pixel(px, py-2, COLOR_ORANGE);
                    draw_pixel(px+1, py-2, COLOR_ORANGE);
                    draw_pixel(px+2, py-1, COLOR_ORANGE);
                    draw_pixel(px+3, py-1, COLOR_ORANGE);
                    draw_pixel(px+4, py, COLOR_ORANGE);
                    draw_pixel(px+5, py, COLOR_ORANGE);
                }
                if (cave[y+1][x].is_wall() && (cave[y+1][x].flags & MAP_KNOW)) {
                    draw_pixel(px-4, py, COLOR_ORANGE);
                    draw_pixel(px-3, py, COLOR_ORANGE);
                    draw_pixel(px-2, py+1, COLOR_ORANGE);
                    draw_pixel(px-1, py+1, COLOR_ORANGE);
                    draw_pixel(px, py+2, COLOR_ORANGE);
                    draw_pixel(px+1, py+2, COLOR_ORANGE);
                }
            }
        }
    }
    end_pixel_draw();
}


/*
 * Prints out some information relating to what is under the mouse cursor.
 *
 * Information is in format:
 *
 * Monster/player name
 * Health bar
 * Item name
 * Cave feature
 *
 * Rewritten by MJC from the old monster health bar code
 */
static void prt_mouse_info(int mx, int my)
{
    CGrid *g_ptr;
    CMonster *m_ptr;
    CItem *i_ptr;
    char info[80];
    int ninfos = 0;
    bool mouse_on_map;
    int mouse_map_x, mouse_map_y;

    // Draw the pane
    draw_pane(320-12*8, 368-16, 320+12*8, 447, COLOR_BLACK);

    // Get mouse location
    calc_mouse_map_xy(mx, my, &mouse_on_map, &mouse_map_x, &mouse_map_y);

    // If the mouse is not on the map, display nothing and be done
    if (!mouse_on_map) return;

    // Get the cave pointer for the tile the mouse is on
    g_ptr = &cave[mouse_map_y][mouse_map_x];

    // Get object and monster pointers
    m_ptr = g_ptr->m_ptr;
    i_ptr = g_ptr->i_ptr;

    // If there is a monster, show its information
    if (m_ptr) {
        // Is it alive and visible?
        if (m_ptr->is_visible() && (m_ptr->GetCHP() >= 0)) {
            int pct, pixels;

            // Give its name
            m_ptr->get_desc(info, 0x8);
            info[24] = 0;

            // Display the name
            put_string(320-4*strlen(info), 368 + ninfos*16, info, COLOR_WHITE);
            ninfos++;

            // Draw the rectangle
            rectangle(268, 368 + ninfos*16, 371, 368 + ninfos*16 + 15, COLOR_WHITE);

            // Default to almost dead
            byte attr = COLOR_RED;

            // Extract the "percent" of health
            pct = 100L * m_ptr->GetCHP() / m_ptr->GetMHP();
            pixels = 102L * m_ptr->GetCHP() / m_ptr->GetMHP();

            // Badly wounded
            if (pct >= 10) attr = COLOR_LT_RED;

            // Wounded
            if (pct >= 25) attr = COLOR_ORANGE;

            // Somewhat Wounded
            if (pct >= 60) attr = COLOR_YELLOW;

            // Healthy
            if (pct >= 100) attr = COLOR_LT_GREEN;

            // Afraid
            if (m_ptr->get_afraid()) attr = COLOR_PURPLE;

            // Asleep
            if (m_ptr->get_csleep()) attr = COLOR_BLUE;

            // Draw the bar
            if (pixels) box(269, 368 + ninfos*16 + 1, 268+pixels,
                368 + ninfos*16 + 14, attr);

            ninfos++;
        }
    }

    // If there is an object, show its information
    if (i_ptr && i_ptr->GetMarked()) {
        // Get the heaviest object
        CItem *j_ptr = i_ptr;
        bool in_stack = FALSE;
        while (j_ptr->next_i_ptr) {
            in_stack = TRUE;
            j_ptr = j_ptr->next_i_ptr;
            if (j_ptr->GetWeight() > i_ptr->GetWeight()) i_ptr = j_ptr;
        }

        // Get its description
        i_ptr->object_desc(info, TRUE, 0);
        info[24] = 0;

        // Display the name
        put_string(320-4*strlen(info), 368 + ninfos*16, info, COLOR_WHITE);
        ninfos++;

        // If it is part of a stack, note that
        if (in_stack) {
            strcpy(info, "(and other objects)");
            put_string(320-4*strlen(info), 368 + ninfos*16, info, COLOR_WHITE);
            ninfos++;
        }
    }

    // Interesting cave features (if known)
    if (g_ptr->flags & MAP_KNOW) {
        // Nothing for floors, invisible traps, secret doors, and walls
        if ((g_ptr->get_feat() != CF_FLOOR) && (g_ptr->get_feat() != CF_TRAP_INVIS) &&
            !g_ptr->is_wall() && (g_ptr->get_feat() != CF_DOOR_SECRET)) {
            char *name = f_name + f_info[g_ptr->get_feat()].name;

            // Add "a" or "an" to the beginning
            if (is_a_vowel(name[0])) {
                strcpy(info, "an ");
                strcat(info, name);
            }
            else {
                strcpy(info, "a ");
                strcat(info, name);
            }
            info[24] = 0;

            // Display the name
            put_string(320-4*strlen(info), 368 + ninfos*16, info, COLOR_WHITE);
            ninfos++;
        }
    }
}


/*
 * Draw messages
 */
static void prt_messages(void)
{
    int y = MAP_START_Y;

    // Draw the latest few
    for (int i = 0; i < MESSAGE_LIMIT; i++) {
        char *msg = get_old_message(i);
        if (msg) {
            put_string(MAP_START_X, y, msg, COLOR_WHITE);
            y += 16;
        }
    }
}


/*
 * Returns a "rating" of x depending on y
 */
static char *likert(int x, int y, byte *color)
{
    // Paranoia
    if (y <= 0) y = 1;

    // Negative value
    if (x < 0) {
        *color = COLOR_RED;
        return "Very Bad";
    }

    // Analyze the value
    switch (x / y) {
        case 0:
        case 1:
            *color = COLOR_RED;
            return "Bad";
        case 2:
            *color = COLOR_ORANGE;
            return "Poor";
        case 3:
        case 4:
            *color = COLOR_YELLOW;
            return "Fair";
        case 5:
            *color = COLOR_YELLOW;
            return "Good";
        case 6:
            *color = COLOR_YELLOW;
            return "Very Good";
        case 7:
        case 8:
            *color = COLOR_LT_GREEN;
            return "Excellent";
        case 9:
        case 10:
        case 11:
        case 12:
        case 13:
            *color = COLOR_LT_GREEN;
            return "Superb";
        case 14:
        case 15:
        case 16:
        case 17:
            *color = COLOR_LT_GREEN;
            return "Heroic";
        default:
            *color = COLOR_LT_GREEN;
            return "Legendary";
    }
}


/*
 * Output a window with some characer info.
 */
static void prt_character(void)
{
    char buf[160], *desc;
    int i, bonus, tmp, xthn, xthb, xfos, xsrh, xdis, xdev, xsav, xstl;
    byte attr, likert_color;
    int show_tohit = p_ptr->get_dis_to_h();
    int show_todam = p_ptr->get_dis_to_d();
    CItem *i_ptr = &inventory[INVEN_WIELD];

    draw_window_border(0, 0, 319, MAP_END_Y, FALSE);
    box(2, 2, 317, MAP_END_Y-2, COLOR_GREY);
    
    sprintf(buf, "%s the %s %s %s", player_name, p_ptr->GetMale() ? "male" : "female",
        p_ptr->GetRaceTitle(), p_ptr->GetClassTitle());
    put_text_format(160, 5, buf, COLOR_BLACK, FONT_BOLD, JUST_CENTER);

    put_text(7, 31, "Level", COLOR_BLACK, FONT_BOLD);
    put_text(167, 31, "Gold", COLOR_BLACK, FONT_BOLD);
    put_text(7, 49, "Experience", COLOR_BLACK, FONT_BOLD);
    put_text(167, 49, "Advance", COLOR_BLACK, FONT_BOLD);

    draw_pane(80, 30, 156, 46, COLOR_BLACK);
    draw_pane(235, 30, 311, 46, COLOR_BLACK);
    draw_pane(80, 48, 156, 64, COLOR_BLACK);
    draw_pane(235, 48, 311, 64, COLOR_BLACK);

    // Print level
    sprintf(buf, "%d", p_ptr->GetLev());
    put_text_format(118, 31, buf, (p_ptr->GetLev() < p_ptr->GetMaxPlv()) ?
        COLOR_RED : COLOR_WHITE, FONT_BOLD, JUST_CENTER);

    // Print gold
    sprintf(buf, "%d", p_ptr->GetGold());
    put_text_format(273, 31, buf, COLOR_WHITE, FONT_BOLD, JUST_CENTER);

    // Print experience
    sprintf(buf, "%d", p_ptr->GetExp());
    attr = (p_ptr->GetExp() < p_ptr->GetMaxExp()) ? COLOR_RED : COLOR_WHITE;
    put_text_format(118, 49, buf, attr, FONT_BOLD, JUST_CENTER);

    // Print experience to advance
    if (p_ptr->GetLev() == PY_MAX_LEVEL) strcpy(buf, "--");
    else sprintf(buf, "%d",
        player_exp[p_ptr->GetLev() - 1] * p_ptr->GetExpFact() / 100);
    put_text_format(273, 49, buf, (p_ptr->GetLev() < p_ptr->GetMaxPlv()) ?
        COLOR_RED : COLOR_WHITE, FONT_BOLD, JUST_CENTER);

    put_text(7, 75, "Stat", COLOR_BLACK, FONT_BOLD);
    put_text_format(120, 75, "Base", COLOR_BLACK, FONT_BOLD, JUST_CENTER);
    put_text_format(195, 75, "Bonuses", COLOR_BLACK, FONT_BOLD, JUST_CENTER);
    put_text_format(270, 75, "Total", COLOR_BLACK, FONT_BOLD, JUST_CENTER);
    for (i = 0; i < 6; i++) {
        put_text(7, 93+i*18, stat_names_full[i], COLOR_BLACK, FONT_BOLD);
       
        draw_pane(95, 92+i*18, 145, 108+i*18, COLOR_BLACK);
        draw_pane(170, 92+i*18, 220, 108+i*18, COLOR_BLACK);
        draw_pane(245, 92+i*18, 295, 108+i*18, COLOR_BLACK);

        // Get the eq bonus to the stat and display it
        bonus = p_ptr->GetStatAdd(i);
        sprintf(buf, "%s%d", (bonus > 0) ? "+" : "", bonus);
        put_text_format(195, 93+i*18, buf, COLOR_WHITE, FONT_BOLD, JUST_CENTER);

        // Display "injured" stat 
        if (p_ptr->GetStatCur(i) < p_ptr->GetStatMax(i)) {
            cnv_stat_left(p_ptr->GetStatCur(i), buf);
            put_text_format(120, 93+i*18, buf, COLOR_RED, FONT_BOLD, JUST_CENTER);

            cnv_stat_left(p_ptr->GetStatUse(i), buf);
            put_text_format(270, 93+i*18, buf, COLOR_RED, FONT_BOLD, JUST_CENTER);
        }

        // Display "healthy" stat
        else {
            cnv_stat_left(p_ptr->GetStatCur(i), buf);
            put_text_format(120, 93+i*18, buf, COLOR_WHITE, FONT_BOLD, JUST_CENTER);

            cnv_stat_left(p_ptr->GetStatUse(i), buf);
            put_text_format(270, 93+i*18, buf, COLOR_WHITE, FONT_BOLD, JUST_CENTER);
        }
    }

    // Hack -- add in weapon info if known
    if (i_ptr->isKnown()) show_tohit += i_ptr->GetToH();
    if (i_ptr->isKnown()) show_todam += i_ptr->GetToD();

    // Dump the bonuses to hit/dam and the armor class
    put_text(7, 209, "Hit Bonus", COLOR_BLACK, FONT_BOLD);
    put_text(7, 227, "Total Damage", COLOR_BLACK, FONT_BOLD);
    put_text(7, 245, "Armor Class", COLOR_BLACK, FONT_BOLD);
    draw_pane(95, 208, 145, 224, COLOR_BLACK);
    draw_pane(95, 226, 145, 242, COLOR_BLACK);
    draw_pane(95, 244, 145, 260, COLOR_BLACK);
    sprintf(buf, "%s%d", (show_tohit > 0) ? "+" : "", show_tohit);
    put_text_format(120, 209, buf, COLOR_WHITE, FONT_BOLD, JUST_CENTER);
    if (i_ptr->GetKIdx()) sprintf(buf, "%dd%d%s%d", i_ptr->GetDD(), i_ptr->GetDS(),
        (show_todam >= 0) ? "+" : "", show_todam);
    else sprintf(buf, "1d1%s%d", (show_todam >= 0) ? "+" : "", show_todam);
    put_text_format(120, 227, buf, COLOR_WHITE, FONT_BOLD, JUST_CENTER);
    sprintf(buf, "%d", p_ptr->GetTotalDisAC());
    put_text_format(120, 245, buf, COLOR_WHITE, FONT_BOLD, JUST_CENTER);

    // Blows, shows, infravision
    put_text(157, 209, "Blows/Round", COLOR_BLACK, FONT_BOLD);
    put_text(157, 227, "Shots/Round", COLOR_BLACK, FONT_BOLD);
    put_text(157, 245, "Infra-Vision", COLOR_BLACK, FONT_BOLD);
    draw_pane(245, 208, 295, 224, COLOR_BLACK);
    draw_pane(245, 226, 295, 242, COLOR_BLACK);
    draw_pane(245, 244, 295, 260, COLOR_BLACK);
    put_text_format(270, 209, format("%d", p_ptr->get_num_blow()), COLOR_WHITE, FONT_BOLD,
        JUST_CENTER);
    put_text_format(270, 227, format("%d", p_ptr->get_num_fire()), COLOR_WHITE, FONT_BOLD,
        JUST_CENTER);
    put_text_format(270, 245, format("%d ft", p_ptr->get_see_infra() * 10), COLOR_WHITE,
        FONT_BOLD, JUST_CENTER);

    // Fighting Skill (with current weapon)
    i_ptr = &inventory[INVEN_WIELD];
    tmp = p_ptr->get_to_h() + i_ptr->GetToH();
    xthn = p_ptr->GetSkill(SKILL_THN) + tmp*BTH_PLUS_ADJ;

    // Shooting Skill (with current bow and normal missile)
    i_ptr = &inventory[INVEN_BOW];
    tmp = p_ptr->get_to_h() + i_ptr->GetToH();
    xthb = p_ptr->GetSkill(SKILL_THB) + tmp*BTH_PLUS_ADJ;

    // Basic abilities
    xdis = p_ptr->GetSkill(SKILL_DIS);
    xdev = p_ptr->GetSkill(SKILL_DEV);
    xsav = p_ptr->GetSkill(SKILL_SAV);
    xstl = p_ptr->GetSkill(SKILL_STL);
    xsrh = p_ptr->GetSkill(SKILL_SRH);
    xfos = p_ptr->GetSkill(SKILL_FOS);


    put_text(7, 271, "Fighting", COLOR_BLACK, FONT_BOLD);
    put_text(7, 289, "Bows", COLOR_BLACK, FONT_BOLD);
    put_text(7, 307, "Disarming", COLOR_BLACK, FONT_BOLD);
    put_text(7, 325, "Stealth", COLOR_BLACK, FONT_BOLD);

    draw_pane(75, 270, 145, 286, COLOR_BLACK);
    draw_pane(75, 288, 145, 304, COLOR_BLACK);
    draw_pane(75, 306, 145, 322, COLOR_BLACK);
    draw_pane(75, 324, 145, 340, COLOR_BLACK);

    desc = likert(xthn, 12, &likert_color);
    put_text_format(110, 271, desc, likert_color, FONT_BOLD, JUST_CENTER);
    desc = likert(xthb, 12, &likert_color);
    put_text_format(110, 289, desc, likert_color, FONT_BOLD, JUST_CENTER);
    desc = likert(xdis, 8, &likert_color);
    put_text_format(110, 307, desc, likert_color, FONT_BOLD, JUST_CENTER);
    desc = likert(xstl, 1, &likert_color);
    put_text_format(110, 325, desc, likert_color, FONT_BOLD, JUST_CENTER);

    put_text(157, 271, "Perception", COLOR_BLACK, FONT_BOLD);
    put_text(157, 289, "Searching", COLOR_BLACK, FONT_BOLD);
    put_text(157, 307, "Saving Throw", COLOR_BLACK, FONT_BOLD);
    put_text(157, 325, "Magic Device", COLOR_BLACK, FONT_BOLD);

    draw_pane(245, 270, 315, 286, COLOR_BLACK);
    draw_pane(245, 288, 315, 304, COLOR_BLACK);
    draw_pane(245, 306, 315, 322, COLOR_BLACK);
    draw_pane(245, 324, 315, 340, COLOR_BLACK);

    desc = likert(xfos, 6, &likert_color);
    put_text_format(280, 271, desc, likert_color, FONT_BOLD, JUST_CENTER);
    desc = likert(xsrh, 6, &likert_color);
    put_text_format(280, 289, desc, likert_color, FONT_BOLD, JUST_CENTER);
    desc = likert(xsav, 6, &likert_color);
    put_text_format(280, 307, desc, likert_color, FONT_BOLD, JUST_CENTER);
    desc = likert(xdev, 6, &likert_color);
    put_text_format(280, 325, desc, likert_color, FONT_BOLD, JUST_CENTER);
}


/*
 * Redraw lots of stuff
 */
static void redraw_stuff(int mx, int my)
{
    // Draw the frame
    draw_window_border(0, MAP_END_Y+1, 639, 479, FALSE);
    box(2, MAP_END_Y+3, 637, 477, COLOR_GREY);

    /* Level/Experience/Gold/HP/SP/AC */
    prt_bottom();

    // Map/Minimap
    set_clip_rect(MAP_START_X, MAP_START_Y, MAP_END_X, MAP_END_Y);
    prt_map(mx, my);
    if (show_minimap) prt_minimap();
    clear_clip_rect();

    // Various statuses
    prt_status();

    // Mouse info
    prt_mouse_info(mx, my);

    // Extra things being shown
    switch (show_stuff) {
        case SHOW_JUST_MAP: prt_messages(); break;
        case SHOW_CONSOLE: draw_console(); break;
        case SHOW_CHARACTER: prt_character(); prt_messages(); break;
        case SHOW_INVEN: prt_messages(); draw_inven_stuff(); break;
        case SHOW_EQUIP: prt_messages(); draw_equip_stuff(); break;
    }

    // Refresh it, handling the mouse
    virt_draw_mouse(mx, my);
    screen_refresh();
    virt_kill_mouse(mx, my);
}


/*
 * Regenerate the monsters (once per 1000 game turns)
 *
 * XXX XXX XXX Should probably be done during monster turns.
 */
static void regen_monsters(void)
{
    CMonster *m_ptr;
    CMonsterRace *r_ptr;
    int frac, x, y;

    // Go through every tile
    for (x = 0; x < cur_wid; x++) {
        for (y = 0; y < cur_hgt; y++) {
            m_ptr = cave[y][x].m_ptr;

            // If there is no monster, skip
            if (!m_ptr) continue;

            // Get race
            r_ptr = m_ptr->get_r_ptr();

            // Allow regeneration (if needed)
            if (m_ptr->GetCHP() < m_ptr->GetMHP()) {
                // Hack -- Base regeneration
                frac = m_ptr->GetMHP() / 100;

                // Hack -- Minimal regeneration rate
                if (!frac) frac = 1;

                // Hack -- Some monsters regenerate quickly
                if (r_ptr->flags2 & RF2_REGENERATE) frac *= 2;

                // Hack -- Regenerate
                m_ptr->SetCHP(m_ptr->GetCHP() + frac);

                // Do not over-regenerate
                m_ptr->correct_hp_overflows();
            }
        }
    }
}



/*
 * Handle certain things once every 10 game turns
 */
static void process_world(void)
{
    int x, y;
    CGrid *g_ptr;


    /* Every 10 game turns */
    if (game_turn % 10) return;

    /*** Handle the "town" (stores and sunshine) ***/

    // While in town
    if (!dun_level) {
        /* Hack -- Daybreak/Nighfall in town */
        if (!(game_turn % ((10L * TOWN_DAWN) / 2))) {
            bool dawn;

            // Check for dawn
            dawn = (!(game_turn % (10L * TOWN_DAWN)));

            // Day breaks
            if (dawn) {
                // Message
                msg_print("The sun has risen.");

                // Hack -- Scan the town
                for (y = 0; y < cur_hgt; y++) {
                    for (x = 0; x < cur_wid; x++) {
                        // Get the cave grid
                        g_ptr = &cave[y][x];

                        // Assume lit and known
                        g_ptr->flags |= MAP_GLOW | MAP_KNOW;

                        // Hack -- Notice spot
                        note_spot(y, x);
                    }
                }
            }

            // Night falls
            else {
                // Message
                msg_print("The sun has fallen.");

                // Hack -- Scan the town
                for (y = 0; y < cur_hgt; y++) {
                    for (x = 0; x < cur_wid; x++) {
                        // Get the cave grid
                        g_ptr = &cave[y][x];

                        // Make it no longer glowing
                        g_ptr->flags &= ~MAP_GLOW;
                    }
                }
            }

            // Update the monsters 
            p_ptr->set_update(p_ptr->get_update() | PU_MONSTERS);
        }
    }


    // While in the dungeon
    else {
        /*** Update the Stores ***/

        // Update the stores once a day (while in dungeon)
        if (!(game_turn % (10L * STORE_TURNS))) {
            /* New inventory */
            store_maint();

            /* New owners */
            if (one_in(STORE_SHUFFLE)) {
                store_shuffle();
            }
        }
    }


    /*** Process the monsters ***/

    // Check for creature generation
    if (one_in(MAX_M_ALLOC_CHANCE)) {
        /* Make a new monster */
        alloc_monster(MAX_SIGHT + 5, FALSE, dun_level);
    }

    // Hack -- Check for creature regeneration
    if (!(game_turn % 100)) regen_monsters();
}



// Move towards a square
static bool make_move_towards(int x, int y)
{
    int d1, d2, d3, tx, ty, d2_min, d3_min, temp;
    int px = p_ptr->GetX(), py = p_ptr->GetY();
    CGrid *g_ptr;

    // Is the target a valid one?
    g_ptr = &cave[y][x];
    if ((g_ptr->flags & MAP_KNOW) && !floor_grid_bold(y, x)) {
        return FALSE;
    }

    // Get the valid directions to the target
    if (px < x) {
        if (py < y) {
            d1 = 2;
            d2 = 3;
            d3 = 1;
        }
        else if (py > y) {
            d1 = 6;
            d2 = 9;
            d3 = 3;
        }
        else {
            d1 = 3;
            d2 = 6;
            d3 = 2;
        }
    }
    else if (px > x) {
        if (py < y) {
            d1 = 4;
            d2 = 1;
            d3 = 7;
        }
        else if (py > y) {
            d1 = 8;
            d2 = 7;
            d3 = 9;
        }
        else {
            d1 = 7;
            d2 = 4;
            d3 = 8;
        }
    }
    else {
        if (py < y) {
            d1 = 1;
            d2 = 2;
            d3 = 4;
        }
        else if (py > y) {
            d1 = 9;
            d2 = 8;
            d3 = 6;
        }
        else return FALSE;
    }


    // Occasionally, swap d2 and d3
    tx = px+dx[d2]; ty = py+dy[d2];
    tx = abs(x-tx); ty = abs(y-ty);
    d2_min = (tx < ty) ? tx : ty;
    tx = px+dx[d3]; ty = py+dy[d3];
    tx = abs(x-tx); ty = abs(y-ty);
    d3_min = (tx < ty) ? tx : ty;
    if (d3_min < d2_min) {
        temp = d2;
        d2 = d3;
        d3 = temp;
    }


    // Try d1
    tx = px+dx[d1]; ty = py+dy[d1];
    g_ptr = &cave[ty][tx];
    if (!(g_ptr->flags & MAP_KNOW) || floor_grid_bold(ty, tx)) {
        move_player(d1);
        return TRUE;
    }

    // Try d2
    tx = px+dx[d2]; ty = py+dy[d2];
    g_ptr = &cave[ty][tx];
    if (!(g_ptr->flags & MAP_KNOW) || floor_grid_bold(ty, tx)) {
        move_player(d2);
        return TRUE;
    }

    // Try d3
    tx = px+dx[d3]; ty = py+dy[d3];
    g_ptr = &cave[ty][tx];
    if (!(g_ptr->flags & MAP_KNOW) || floor_grid_bold(ty, tx)) {
        move_player(d3);
        return TRUE;
    }

    // Give up
    return FALSE;
}




/*
 * Process the player's input
 */
static void process_input(void)
{
    int i, rx, ry, rmx, rmy;
    bool rom;
    CItem *i_ptr;


    /*** Handle actual user input ***/

    // Get shift status
    bool shift = get_shift();

    // Check for left button release
    if (get_last_left_button_release(&rx, &ry)) {
        CGrid *g_ptr;

        // Get the location on map
        calc_mouse_map_xy(rx, ry, &rom, &rmx, &rmy);

        // If the release was on the map...
        if (rom) {
            // If it's a shift-click, treat it as "alter" if adjacent
            if (shift) {
                bool handled = FALSE;

                // Get the cave pointer
                g_ptr = &cave[rmy][rmx];

                // On the player: pick something up
                if (!handled) {
                    if ((rmx == p_ptr->GetX()) && (rmy == p_ptr->GetY())) {
                        handled = TRUE;
                        carry();
                    }
                }

                // Try adjacent to the player
                if (!handled) {
                    for (int dir = 1; dir <= 9; dir++) {
                        if (dir == 5) continue;
                        if ((rmx == p_ptr->GetX() + dx[dir]) &&
                            (rmy == p_ptr->GetY() + dy[dir])) {
                            // Alter in this direction
                            do_alter(dir);

                            // Handled
                            handled = TRUE;

                            // Don't bother checking other directions
                            break;
                        }
                    }
                }
            }

            // No shift-click, so it's a move/attack command
            else {
                // Get the cave pointer
                g_ptr = &cave[rmy][rmx];

                // If there is a monster, set the monster destination
                if (g_ptr->m_ptr) {
                    p_ptr->kill_destination();
                    p_ptr->set_dest_mon(g_ptr->m_ptr->GetGUID());
                }

                // Otherwise, set the xy destination to that square
                else {
                    p_ptr->kill_destination();
                    p_ptr->set_dest_x(rmx);
                    p_ptr->set_dest_y(rmy);
                }
            }
        }
    }

    // Try keyboard commands
    for (;;) {
        // Get a key
        char c = scan_inkey_scan();

        // Break out if no keys left to get
        if (!c) break;

        // Console?
        if (show_stuff == SHOW_CONSOLE) {
            if (c == KEY_ESCAPE) {
                show_stuff = SHOW_JUST_MAP;
                strcpy(cur_console_line, "");
            }
            else if (c == KEY_ENTER) {
                console_format(">%s", cur_console_line);
                buffer_console(cur_console_line);
                strcpy(cur_console_line, "");
            }
            else if (c == KEY_BACKSPACE) {
                int len = strlen(cur_console_line);
                if (len > 0) cur_console_line[len-1] = 0;
            }
            else {
                char ascii = convert(c, get_shift(), get_capslock());
                if (isprint(ascii)) {
                    int len = strlen(cur_console_line);
                    if (len < 78) {
                        cur_console_line[len] = ascii;
                        cur_console_line[len+1] = 0;
                    }
                }
            }
        }

        // No console
        else {
            char buf[80];

            // Kill all pending destination-moves
            p_ptr->kill_destination();

            // Get rid of inventory/equipment
            if ((show_stuff == SHOW_INVEN) || (show_stuff == SHOW_EQUIP)) {
                show_stuff = SHOW_JUST_MAP;
            }

            // Get the console string for the character
            get_command(c, buf);

            // Interpret the string
            buffer_console(buf);
        }
    }


    // Check for right button release
    if (get_last_right_button_release(&rx, &ry)) {
        // Get the location on map
        calc_mouse_map_xy(rx, ry, &rom, &rmx, &rmy);

        // Is it on the map?
        if (rom) {
            // Check for active spell
            if (current_spell_type == 1) {
                // Cast
                do_spell(current_spell, rmx, rmy);
            }

            // Check for active missile weapon
            else if (current_spell_type == 2) {
                do_fire(rmx, rmy);
            }

            // Check for selected artifact to zap
            else if (current_spell_type == 3) {
                do_activate(current_spell, rmx, rmy);
            }

            // Zap a rod
            else if (current_spell_type == 4) {
                do_zap(TV_ROD, current_spell, rmx, rmy);
            }

            // Zap a staff
            else if (current_spell_type == 5) {
                do_zap(TV_STAFF, current_spell, rmx, rmy);
            }
            
            // Zap a wand
            else if (current_spell_type == 6) {
                do_zap(TV_WAND, current_spell, rmx, rmy);
            }
        }
    }


    // Moving towards a tile and have energy
    if ((p_ptr->get_dest_x() != -1) && !p_ptr->isBusy()) {
        // Make the move
        if (!make_move_towards(p_ptr->get_dest_x(), p_ptr->get_dest_y())) {
            // Could't move
            p_ptr->kill_destination();
        }
    }

    // Moving towards a monster and have energy
    if (p_ptr->get_dest_mon() && !p_ptr->isBusy()) {
        CMonster *m_ptr = NULL;

        // Seek out monster
        for (int x = 0; x < cur_wid; x++) {
            for (int y = 0; y < cur_hgt; y++) {
                // No monster?
                if (!cave[y][x].m_ptr) continue;

                // Wrong GUID?
                if (cave[y][x].m_ptr->GetGUID() != p_ptr->get_dest_mon()) continue;

                // Found
                m_ptr = cave[y][x].m_ptr;
                break;
            }

            // Found?
            if (m_ptr) break;
        }

        // Is there a monster?
        if (m_ptr) {
            // Is the monster dead or out of view?
            if ((m_ptr->GetCHP() < 0) || !m_ptr->is_visible()) {
                p_ptr->kill_destination();
            }

            // Otherwise, try moving towards it
            else {
                // Make the move
                if (!make_move_towards(m_ptr->GetX(), m_ptr->GetY())) {
                    // Couldn't move
                    p_ptr->kill_destination();
                }
            }
        }
    }


    /* Notice stuff */
    if (p_ptr->get_notice()) notice_stuff();

    /* XXX XXX XXX Pack Overflow */
    if (inventory[INVEN_PACK].exists()) {
        int amt;
        char i_name[80];


        /* Choose an item to spill */
        i = INVEN_PACK;

        /* Access the slot to be dropped */
        i_ptr = &inventory[i];

        /* Drop all of that item */
        amt = i_ptr->GetNumber();

        /* Warning */
        msg_print("Your pack overflows!");

        /* Describe */
        i_ptr->object_desc(i_name, TRUE, 3);

        /* Message */
        msg_format("You drop %s.", i_name);

        /* Drop it (carefully) near the player */
        drop_near(i_ptr, 0, p_ptr->GetY(), p_ptr->GetX());

        /* Decrease the item, optimize. */
        inven_item_increase(i, -amt);
        inven_item_optimize(i);
    }
}


/*
 * Calculate map boundaries
 */
static void calc_map_bounds(void)
{
    switch (show_stuff) {
        case SHOW_CHARACTER:
            MAP_START_X = 320;
            MAP_START_Y = 0;
            MAP_END_X = 639;
            MAP_END_Y = 345;
            MAP_CENTER_X = 160;
            MAP_CENTER_Y = 0;
            break;
        default:
            MAP_START_X = 0;
            MAP_START_Y = 0;
            MAP_END_X = 639;
            MAP_END_Y = 345;
            MAP_CENTER_X = 0;
            MAP_CENTER_Y = 0;
            break;
    }
    MAP_CENTER_X += 9*32;
    MAP_CENTER_Y += 156;
}


/*
 * Interact with the current dungeon level.
 *
 * This function will not exit until the level is completed,
 * the user dies, or the game is terminated.
 */
static void dungeon(void)
{
    unsigned long excess_time;
    int mx = 0, my = 0;
    bool left;

    // Reset various flags
    new_level_flag = FALSE;


    // No current spell
    current_spell_type = 0;


    /* Remember deepest dungeon level visited */
    if (dun_level > p_ptr->GetMaxDlv()) {
        p_ptr->SetMaxDlv(dun_level);
    }


    /* Enter "xtra" mode */
    character_xtra = TRUE;

    // Update stuff
    p_ptr->set_update(p_ptr->get_update() | PU_BONUS | PU_HP | PU_MANA | PU_SPELLS | PU_TORCH);
    p_ptr->set_update(p_ptr->get_update() | PU_VIEW | PU_LITE | PU_FLOW | PU_DISTANCE);
    update_stuff();

    // Leave "xtra" mode
    character_xtra = FALSE;

    // Update stuff
    p_ptr->set_update(p_ptr->get_update() | PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);
    update_stuff();

    // Combine / Reorder the pack
    p_ptr->set_notice(p_ptr->get_notice() | PN_COMBINE | PN_REORDER);
    notice_stuff();


    /* Hack -- notice death or departure */
    if (!alive || death || new_level_flag) return;


    /*** Process this dungeon level ***/

    // Show only the map
    show_stuff = SHOW_JUST_MAP;

    // Set the ticker to 0
    reset_timer();

    // No excess time
    excess_time = 0;

    // No destinations
    p_ptr->kill_destination();

    // Main loop
    while (TRUE) {
        // Update everything
        notice_stuff();
        update_stuff();

        // Calculate the map boundaries
        calc_map_bounds();

        // Access the mouse
        get_mouse_status(&mx, &my, &left);

        // Redraw everything
        blank_screen(COLOR_BLACK);
        redraw_stuff(mx, my);

        // FPS stuff
        frames++;
        fps_ticker += get_timer_value();

        // Add in extra time
        excess_time += get_timer_value();

        // Reset ticker to 0
        reset_timer();

        // Make sure game speed is sane
        if (game_speed < 1) game_speed = 5;

        // Run game turns
        while (excess_time >= game_speed) {
            // Remove some time
            excess_time -= game_speed;

            // Update everything
            notice_stuff();
            update_stuff();

            // Process the game
            p_ptr->Process();
            process_monsters();
            process_objects();
            process_world();
            for (int counter = 0; counter < 10; counter++) process_projectiles();

            // Increment turn
            game_turn++;

            // If dead, break out
            if (!alive || death || new_level_flag) break;
        }

        // Process input and flush commands
        process_input();
        flush_console_buffer();

        // Enter stores?
        if (force_enter_store) {
            force_enter_store = FALSE;
            //: Evil to to use do_cmd_ outside of console
            do_cmd_store();
        }

        // If dead, break out
        if (!alive || death || new_level_flag) break;
    }



    // Notice stuff
    notice_stuff();

    // Update stuff
    update_stuff();


    /* Forget the old lite */
    forget_lite();

    /* Forget the old view */
    forget_view();
}



/*
 * Actually play a game
 */
void play_game(void)
{
    char c, buf[80];
    int i, slot_taken[10];
    FILE *f;
    player_summary ps[10];
    int slot_sum;
    char last_error[80];

    // Display the splash screen
    show_splash();

    // Initialize the arrays
    init_some_arrays();

    // Wait for response
    put_text_format(320, 23*16, "Press any key to continue", COLOR_WHITE, FONT_BOLD,
        JUST_CENTER);
    screen_refresh();
    wait_for_key();

    // Reset the palette (safely!)
    blank_screen(COLOR_BLACK);
    screen_refresh();
    set_default_palette();

    // Init the RNG
    if (TRUE) {
        u32b seed;

        // Basic seed
        seed = time(NULL);

        // Use the complex RNG
        Rand_quick = FALSE;

        // Seed the complex RNG
        Rand_state_init(seed);
    }

    
    // Hack -- Character is "icky"
    character_icky = TRUE;


    // Which slots are open?
    f = fopen("dat/save/save.dat", "rt");
    slot_sum = 0;
    for (i = 0; i < 10; i++) {
        fscanf(f, "%d", &slot_taken[i]);
        slot_sum += slot_taken[i];
    }
    fclose(f);

    strcpy(last_error, "");

game_start_over:
    // Query: play a new game or load a character?
    blank_screen(COLOR_BLACK);
    put_string(6*8, 32, "What do you want to do?", COLOR_WHITE);
    put_string(8*8, 48, "(n) Create a new character", COLOR_WHITE);
    put_string(8*8, 64, "(l) Load an existing character", COLOR_WHITE);
    put_string(8*8, 96, "(Esc) Quit", COLOR_WHITE);
    put_string(8*8, 128, last_error, COLOR_WHITE);
    screen_refresh();
    for (;;) {
        c = scan_inkey();
        if (c == KEY_N) break;
        if (c == KEY_L) break;
        if (c == KEY_ESCAPE) return;
    }

    // Deal with input
    if (c == KEY_N) {
        // Set savefile name
        for (i = 0; i < 10; i++) {
            if (!slot_taken[i]) break;
        }

        // No slots?
        if (i == 10) {
            strcpy(last_error, "No open savefile slots remain.");
            goto game_start_over;
        }

        // Set character index
        char_idx = i;

        // Ignore the dungeon
        character_dungeon = FALSE;

        // Start in town
        dun_level = 0;

        // Hack -- seed for flavors
        seed_flavor = rand_int(0x10000000);

        // Hack -- seed for town layout
        seed_town = rand_int(0x10000000);

        // Roll up a new character, and if cancelled, try again from the start
        if (!player_birth()) goto game_start_over;

        // Set savefile to existing
        set_savefile_exist(char_idx, 1);
    }
    else {
        // Any saved chars?
        if (slot_sum == 0) {
            strcpy(last_error, "You have no saved characters.");
            goto game_start_over;
        }

        // Get a character summary
        get_player_summary(ps);

        // Get the first one so only valid chars can be loaded
        for (i = 0; i < 10; i++) {
            if (slot_taken[i]) break;
        }
        char_idx = i;

        // Load existing character
        CWindow *window = new CWindow(170, 100, 300, 230, "Load Character");
        CButton *ok = new CButton(240, 303, 60, 20, "OK");
        CButton *cancel = new CButton(340, 303, 60, 20, "Cancel");
        window->Attach(ok); window->Attach(cancel);
        for (i = 0; i < 10; i++) {
            CComponent *x;
            x = new CRadioButton(180, 133+i*16, &char_idx, i);
            if (!slot_taken[i]) x->Disable();
            window->Attach(x);
            if (slot_taken[i]) {
                sprintf(buf, "%s the %s %s %s", ps[i].name,
                    ps[i].male ? "male" : "female",
                    race_info[ps[i].prace].title,
                    class_info[ps[i].pclass].title);
            }
            else {
                strcpy(buf, "(no character saved here)");
            }
            x = new CText(200, 133+i*16, buf, FONT_BOLD, JUST_LEFT);
            if (!slot_taken[i]) x->Disable();
            window->Attach(x);
        }

        for (;;) {
            int mx, my;

            gui_draw(window);
            c = scan_inkey_scan();
            if (c == KEY_ESCAPE) {
                delete window;
                goto game_start_over;
            }
            if (c == KEY_ENTER) break;
            if (c == KEY_UP) {
                for (;;) {
                    char_idx--;
                    if (char_idx < 0) char_idx += 10;
                    if (slot_taken[char_idx]) break;
                }
            }
            if (c == KEY_DOWN) {
                for (;;) {
                    char_idx++;
                    if (char_idx >= 10) char_idx -= 10;
                    if (slot_taken[char_idx]) break;
                }
            }
            if (get_last_left_button_release(&mx, &my)) {
                window->Click(mx, my);
                if (ok->inClientArea(mx, my)) {
                    break;
                }
                if (cancel->inClientArea(mx, my)) {
                    delete window;
                    goto game_start_over;
                }
            }
        }
        delete window;

        // Attempt to load
        if (!load_player()) quit("broken savefile");
    }


    // Flavor the objects
    flavor_init();

    // Set up the bindings
    init_bind();


    // Make a level if necessary
    if (!character_dungeon) generate_cave();


    // Character is now "complete"
    character_generated = TRUE;


    /* Hack -- Character is no longer "icky" */
    character_icky = FALSE;


    /* Start game */
    alive = TRUE;

    /* Hack -- Enforce "delayed death" */
    if (p_ptr->GetCHP() < 0) death = TRUE;

    /* Loop till dead */
    while (TRUE) {
        /* Process the level */
        dungeon();

        /* Handle "quit and save" */
        if (!alive && !death) break;

        /* Erase the old cave */
        wipe_im_lists();

        /* Handle "death" */
        if (death) break;

        /* Make a new level */
        generate_cave();
    }

    // Close stuff
    close_game();
}