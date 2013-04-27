// File: player.cpp
// Purpose: code for CPlayer

/*
 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke
 *
 * This software may be copied and distributed for educational, research, and
 * not for profit purposes provided that this copyright and statement are
 * included in all such copies.
 */

#include "utumno.h"



/*
 * Regenerate hit points -RAK-   
 */
void CPlayer::RegenHP(int percent)
{
    s32b new_chp, new_chp_frac;
    int old_chp;

    // Poisoned or cut yields no healing
    if (GetPoisoned()) return;
    if (GetCut()) return;

    // Save the old hitpoints
    old_chp = GetCHP();

    // Extract the new hitpoints
    new_chp = ((long)GetMHP()) * percent + PY_REGEN_HPBASE;
    SetCHP(GetCHP() + (new_chp >> 16));

    // check for overflow
    if ((GetCHP() < 0) && (old_chp > 0)) SetCHP(MAX_SHORT);
    new_chp_frac = (new_chp & 0xFFFF) + GetCHPFrac();
    if (new_chp_frac >= 0x10000L) {
        SetCHPFrac(new_chp_frac - 0x10000);
        SetCHP(GetCHP() + 1);
    }
    else {
        SetCHPFrac(new_chp_frac);
    }

    // Fully healed
    correct_hp_overflows();
}


/*
 * Regenerate spell points -RAK-
 */
void CPlayer::RegenSP(int percent)
{
    s32b new_csp, new_csp_frac;
    int old_csp;

    old_csp = GetCSP();
    new_csp = ((long)GetMSP()) * percent + PY_REGEN_MNBASE;
    SetCSP(GetCSP() + (new_csp >> 16));

    /* check for overflow */
    if ((GetCSP() < 0) && (old_csp > 0)) {
        SetCSP(MAX_SHORT);
    }
    new_csp_frac = (new_csp & 0xFFFF) + GetCSPFrac();
    if (new_csp_frac >= 0x10000L) {
        SetCSPFrac(new_csp_frac - 0x10000);
        SetCSP(GetCSP() + 1);
    }
    else {
        SetCSPFrac(new_csp_frac);
    }

    // Must set frac to zero even if equal
    if (GetCSP() >= GetMSP()) {
        SetCSP(GetMSP());
        SetCSPFrac(0);
    }
}



/*
 * Return a "feeling" (or NULL) about an item.  Method 1 (Heavy).
 */
static char *value_check_aux1(CItem *i_ptr)
{
    // Artifacts
    if (i_ptr->isArtifact()) {
        // Cursed/Broken
        if (i_ptr->isCursed() || i_ptr->isBroken()) return "terrible";

        // Normal
        return "special";
    }

    // Ego-Items
    if (i_ptr->isEgoItem()) {
        // Cursed/Broken
        if (i_ptr->isCursed() || i_ptr->isBroken()) return "worthless";

        // Normal
        return "excellent";
    }

    // Cursed items
    if (i_ptr->isCursed()) return "cursed";

    // Broken items
    if (i_ptr->isBroken()) return "broken";

    // Good "armor" bonus
    if (i_ptr->GetToA() > 0) return "good";

    // Good "weapon" bonus
    if (i_ptr->GetToH() + i_ptr->GetToD() > 0) return "good";

    // Default to "average"
    return "average";
}


/*
 * Return a "feeling" (or NULL) about an item.  Method 2 (Light).
 */
static char *value_check_aux2(CItem *i_ptr)
{
    // Cursed items (all of them)
    if (i_ptr->isCursed()) return "cursed";

    // Broken items (all of them)
    if (i_ptr->isBroken()) return "broken";

    // Artifacts -- except cursed/broken ones
    if (i_ptr->isArtifact()) return "good";

    // Ego-Items -- except cursed/broken ones
    if (i_ptr->isEgoItem()) return "good";

    // Good armor bonus
    if (i_ptr->GetToA() > 0) return "good";

    // Good weapon bonuses
    if (i_ptr->GetToH() + i_ptr->GetToD() > 0) return "good";

    // No feeling
    return NULL;
}




/*
 * Sense the inventory
 *
 *   Class 0 = Warrior --> fast and heavy
 *   Class 1 = Mage    --> very slow and light
 *   Class 2 = Priest  --> fast but light
 *   Class 3 = Rogue   --> medium and heavy
 *   Class 4 = Ranger  --> very slow and light
 *   Class 5 = Paladin --> slow but heavy
 */
void CPlayer::SenseInventory(void)
{
    int i, plev = GetLev();
    bool heavy = FALSE;
    CItem *i_ptr;
    char i_name[80];
    char feel[80];

    /*** Check for "sensing" ***/

    // No sensing when confused
    if (GetConfused()) return;

    // Analyze the class
    switch (GetClass()) {
        // Warriors -- fast and heavy
        case CLASS_WARRIOR:
            if (!one_in(9000L / (plev * plev + 40))) return;
            heavy = TRUE;
            break;

        // Mages -- very slow and light
        case CLASS_MAGE:
            if (!one_in(240000L / (plev + 5))) return;
            break;

        // Priests -- fast but light
        case CLASS_PRIEST:
            if (!one_in(10000L / (plev * plev + 40))) return;
            break;

        // Rogues -- medium and heavy
        case CLASS_ROGUE:
            if (!one_in(20000L / (plev * plev + 40))) return;
            heavy = TRUE;
            break;

        // Rangers -- very slow and light
        case CLASS_RANGER:
            if (!one_in(120000L / (plev + 5))) return;
            break;

        // Paladins -- slow but heavy
        case CLASS_PALADIN:
            if (!one_in(80000L / (plev * plev + 40))) return;
            heavy = TRUE;
            break;
    }


    /*** Sense everything ***/

    // Check everything
    for (i = 0; i < INVEN_TOTAL; i++) {
        bool okay = FALSE;

        i_ptr = &inventory[i];

        // Skip empty slots
        if (!i_ptr->exists()) continue;

        // Valid "tval" codes
        switch (i_ptr->GetTval()) {
            case TV_SHOT:
            case TV_ARROW:
            case TV_BOLT:
            case TV_BOW:
            case TV_DIGGING:
            case TV_HAFTED:
            case TV_POLEARM:
            case TV_SWORD:
            case TV_BOOTS:
            case TV_GLOVES:
            case TV_HELM:
            case TV_CROWN:
            case TV_SHIELD:
            case TV_CLOAK:
            case TV_SOFT_ARMOR:
            case TV_HARD_ARMOR:
            case TV_DRAG_ARMOR:
                okay = TRUE;
                break;
        }

        // Skip non-sense machines
        if (!okay) continue;

        // We know about it already, do not tell us again
        if (i_ptr->TestIdentFlag(ID_SENSE)) continue;

        // It is fully known, no information needed
        if (i_ptr->isKnown()) continue;

        // Frequent failure on inventory items
        if ((i < INVEN_WIELD) && percent(80)) continue;

        // Check for a feeling
        strcpy(feel ,(heavy ? value_check_aux1(i_ptr) : value_check_aux2(i_ptr)));

        // Skip non-feelings
        if (!feel) continue;

        // Get an object description
        i_ptr->object_desc(i_name, FALSE, 0);

        // Message (equipment)
        if (i >= INVEN_WIELD) {
            msg_format("You feel the %s (%c) you are %s %s %s...",
                i_name, index_to_label(i), describe_use(i),
                ((i_ptr->GetNumber() == 1) ? "is" : "are"), feel);
        }

        // Message (inventory)
        else {
            msg_format("You feel the %s (%c) in your pack %s %s...",
                i_name, index_to_label(i),
                ((i_ptr->GetNumber() == 1) ? "is" : "are"), feel);
        }

        // We have "felt" it
        i_ptr->SetIdentFlag(ID_SENSE);

        // Inscribe it textually
        if (!i_ptr->GetNote()) i_ptr->SetNote(feel);

        // Combine / Reorder the pack (later)
        set_notice(get_notice() | PN_COMBINE | PN_REORDER);
    }
}


void CPlayer::get_desc(char *desc, int mode)
{
    //: This needs to be fleshed out
    strcpy(desc, player_name);
}


CPlayer::CPlayer()
{
    int i;

    prace = 0;
    pclass = 0;
    male = 0;
    gold = 0;
    max_exp = 0;
    exp = 0;
    exp_frac = 0;
    lev = 0;
    msp = 0;
    csp = 0;
    csp_frac = 0;
    max_plv = 0;
    max_dlv = 0;
    for (i = 0; i < 6; i++) {
        stat_max[i] = 0;
        stat_cur[i] = 0;
    }
    fast = 0;
    slow = 0;
    blind = 0;
    paralyzed = 0;
    confused = 0;
    afraid = 0;
    poisoned = 0;
    cut = 0;
    stun = 0;
    protevil = 0;
    shadowform = 0;
    hero = 0;
    shero = 0;
    shield = 0;
    blessed = 0;
    tim_invis = 0;
    tim_infra = 0;
    oppose_acid = 0;
    oppose_elec = 0;
    oppose_fire = 0;
    oppose_cold = 0;
    oppose_pois = 0;
    word_recall = 0;
    busy = 0;
    food = 0;
    confusing = 0;
    new_spells = 0;
    old_spells = 0;
    no_score = 0;
    cumber_armor = 0;
    cumber_glove = 0;
    heavy_wield = 0;
    heavy_shoot = 0;
    icky_wield = 0;
    cur_lite = 0;
    notice = 0;
    update = 0;
    for (i = 0; i < 6; i++) {
        stat_use[i] = 0;
        stat_top[i] = 0;
        stat_add[i] = 0;
        stat_ind[i] = 0;
    }
    for (i = 0; i < MAX_IMMUNE; i++) immunes[i] = 0;
    for (i = 0; i < MAX_RESIST; i++) resists[i] = 0;
    for (i = 0; i < 6; i++) sustains[i] = 0;
    aggravate = 0;
    teleport = 0;
    exp_drain = 0;
    ffall = 0;
    free_act = 0;
    lite = 0;
    see_inv = 0;
    regenerate = 0;
    hold_life = 0;
    telepathy = 0;
    slow_digest = 0;
    bless_blade = 0;
    xtra_might = 0;
    impact = 0;
    dis_to_h = 0;
    dis_to_d = 0;
    dis_to_a = 0;
    dis_ac = 0;
    to_h = 0;
    to_d = 0;
    to_a = 0;
    ac = 0;
    see_infra = 0;
    for (i = 0; i < MAX_SKILL; i++) skills[i] = 0;
    num_blow = 0;
    num_fire = 0;
    tval_xtra = 0;
    tval_ammo = 0;
    speed = 0;
    kill_destination();
    for (i = 0; i < PY_MAX_LEVEL; i++) player_hp[i] = 0;
    action = 0;
    last_move = 0;
}


s16b CPlayer::GetTotalWeight(void)
{
    s16b w = 0;
    int i;
    for (i = 0; i < INVEN_TOTAL; i++) {
        if (!inventory[i].exists()) continue;
        w += inventory[i].GetNumber() * inventory[i].GetWeight();
    }
    return w;
}

char *CPlayer::GetRaceTitle(void)
{
    return race_info[GetRace()].title;
}

char *CPlayer::GetClassTitle(void)
{
    return class_info[GetClass()].title;
}

byte CPlayer::GetExpFact(void)
{
    return rp_ptr->r_exp + cp_ptr->c_exp;
}


/*
 * Process the player
 */
void CPlayer::Process(void)
{
    CItem *i_ptr;
    int i, j, regen_amount;

    // Mega-Hack -- Random teleportation XXX XXX XXX
    if (get_teleport() && percent(1)) {
        /* Teleport player */
        teleport_player(40);
    }

    // Give the player some energy if busy, not paralyzed, not KO'ed, not resting
    if (isBusy() && !GetParalyzed() && (GetStun() < 100) && !resting) {
        SetBusy(GetBusy() - extract_energy[get_speed()]);
        if (busy <= 0) {
            busy = 0;
            action = 0;
        }
    }

    // Resting players always quite busy
    if (resting) SetBusy(100);

    // Various things every 10 turns
    if (game_turn % 10) return;

    // Spontaneous searching
    if ((GetSkill(SKILL_FOS) >= 50) || one_in(50 - GetSkill(SKILL_FOS))) {
        search();
    }


    /*** Damage over Time ***/

    /* Take damage from poison */
    if (GetPoisoned()) {
        /* Take damage */
        take_hit_internal(1, "poison");
    }

    /* Take damage from cuts */
    if (GetCut()) {
        /* Mortal wound or severe cut */
        if (GetCut() > 200) {
            i = 3;
        }

        /* Nasty cut */
        else if (GetCut() > 100) {
            i = 2;
        }

        /* Other cuts */
        else {
            i = 1;
        }

        // Take damage
        take_hit_internal(i, "a fatal wound");
    }


    /*** Check the Food, and Regenerate ***/

    // Digest normally
    if (GetFood() < PY_FOOD_MAX) {
        // Every 100 game turns
        if (!(game_turn % 100)) {
            // Basic digestion rate based on speed
            i = extract_energy[get_speed()] * 2;

            // Regeneration takes more food
            if (get_regenerate()) i += 30;

            // Slow digestion takes less food
            if (get_slow_digest()) i /= 2;

            // Paranoia -- always use at least 1 food
            if (i < 1) i = 1;

            /* Digest some food */
            mod_food(GetFood() - i);
        }
    }

    // Digest quickly when gorged
    else {
        // Digest a lot of food
        mod_food(GetFood() - 100);
    }

    // Starve to death (slowly)
    if (GetFood() < PY_FOOD_STARVE) {
        // Calculate damage 
        i = (PY_FOOD_STARVE - GetFood()) / 10;

        // Take damage
        take_hit_internal(i, "starvation");
    }

    // Default regeneration
    regen_amount = PY_REGEN_NORMAL;

    // Getting Weak
    if (GetFood() < PY_FOOD_WEAK) {
        /* Lower regeneration */
        if (GetFood() < PY_FOOD_STARVE) {
            regen_amount = 0;
        }
        else if (GetFood() < PY_FOOD_FAINT) {
            regen_amount = PY_REGEN_FAINT;
        }
        else {
            regen_amount = PY_REGEN_WEAK;
        }

        /* Getting Faint */
        if (GetFood() < PY_FOOD_FAINT) {
            /* Faint occasionally */
            if (!GetParalyzed() && percent(10)) {
                /* Message */
                msg_print("You faint from the lack of food.");

                // Hack -- faint (bypass free action)
                mod_paralyzed(GetParalyzed() + 1 + rand_int(5));
            }
        }
    }

    // Regeneration ability
    if (get_regenerate()) {
        regen_amount = regen_amount * 2;
    }

    // Resting
    if (resting) {
        regen_amount = regen_amount * 3;
    }

    // Regenerate spell points
    if (GetCSP() < GetMSP()) {
        RegenSP(regen_amount);
    }

    // Regenerate hit points
    if (GetCHP() < GetMHP()) {
        RegenHP(regen_amount);
    }


    /*** Timeout Various Things ***/

    /* Blindness */
    if (GetBlind()) {
        mod_blind(GetBlind() - 1);
    }

    /* Times see-invisible */
    if (GetTimInvis()) {
        mod_tim_invis(GetTimInvis() - 1);
    }

    // Timed infra-vision
    if (GetTimInfra()) {
        mod_tim_infra(GetTimInfra() - 1);
    }

    // Paralysis
    if (GetParalyzed()) {
        mod_paralyzed(GetParalyzed() - 1);
    }

    /* Confusion */
    if (GetConfused()) {
        mod_confused(GetConfused() - 1);
    }

    /* Afraid */
    if (GetAfraid()) {
        mod_afraid(GetAfraid() - 1);
    }

    /* Fast */
    if (GetFast()) {
        mod_fast(GetFast() - 1);
    }

    /* Slow */
    if (GetSlow()) {
        mod_slow(GetSlow() - 1);
    }

    /* Protection from evil */
    if (GetProtevil()) {
        mod_protevil(GetProtevil() - 1);
    }

    /* Invulnerability */
    if (GetShadowform()) {
        mod_shadowform(GetShadowform() - 1);
    }

    /* Heroism */
    if (GetHero()) {
        mod_hero(GetHero() - 1);
    }

    /* Super Heroism */
    if (GetSHero()) {
        mod_shero(GetSHero() - 1);
    }

    /* Blessed */
    if (GetBlessed()) {
        mod_blessed(GetBlessed() - 1);
    }

    /* Shield */
    if (GetShield()) {
        mod_shield(GetShield() - 1);
    }

    /* Oppose Acid */
    if (GetOpposeAcid()) {
        mod_oppose_acid(GetOpposeAcid() - 1);
    }

    /* Oppose Lightning */
    if (GetOpposeElec()) {
        mod_oppose_elec(GetOpposeElec() - 1);
    }

    /* Oppose Fire */
    if (GetOpposeFire()) {
        mod_oppose_fire(GetOpposeFire() - 1);
    }

    /* Oppose Cold */
    if (GetOpposeCold()) {
        mod_oppose_cold(GetOpposeCold() - 1);
    }

    /* Oppose Poison */
    if (GetOpposePois()) {
        mod_oppose_pois(GetOpposePois() - 1);
    }


    /*** Poison and Stun and Cut ***/

    /* Poison */
    if (GetPoisoned()) {
        int adjust = (adj_con_fix[GetStatInd(STAT_CON)] + 1);

        /* Apply some healing */
        mod_poisoned(GetPoisoned() - adjust);
    }

    /* Stun */
    if (GetStun()) {
        int adjust = adj_con_fix[GetStatInd(STAT_CON)] + 1;

        /* Apply some healing */
        mod_stun(GetStun() - adjust);
    }

    /* Cut */
    if (GetCut()) {
        int adjust = adj_con_fix[GetStatInd(STAT_CON)] + 1;

        /* Hack -- Truly "mortal" wound */
        if (GetCut() > 1000) adjust = 0;

        /* Apply some healing */
        mod_cut(GetCut() - adjust);
    }



    /*** Process Light ***/

    /* Check for light being wielded */
    i_ptr = &inventory[INVEN_LITE];

    /* Burn some fuel in the current lite */
    if (i_ptr->GetTval() == TV_LITE) {
        /* Hack -- Use some fuel (except on artifacts) (save light if blind */
        if (!i_ptr->isArtifact() && (i_ptr->GetPval() > 0) && !GetBlind()) {
            /* Decrease life-span */
            i_ptr->SetPval(i_ptr->GetPval() - 1);

            /* The light is now out */
            if (i_ptr->GetPval() == 0) {
                msg_print("Your light has gone out!");
            }

            /* The light is getting dim */
            else if ((i_ptr->GetPval() < 100) && (!(i_ptr->GetPval() % 10))) {
                msg_print("Your light is growing faint.");
            }
        }
    }

    // Calculate torch radius
    set_update(get_update() | PU_TORCH);


    /*** Process Inventory ***/

    /* Handle experience draining */
    if (get_exp_drain()) {
        if (percent(10) && (GetExp() > 0)) {
            SetExp(GetExp() - 1);
            SetMaxExp(GetMaxExp() - 1);
            check_experience();
        }
    }

    // Note changes
    j = 0;

    // Process equipment
    for (i = INVEN_WIELD; i < INVEN_TOTAL; i++) {
        /* Get the object */
        i_ptr = &inventory[i];

        /* Skip non-objects */
        if (!i_ptr->exists()) continue;

        /* Recharge activatable objects */
        if (i_ptr->GetTimeout() > 0) {
            /* Recharge */
            i_ptr->SetTimeout(i_ptr->GetTimeout() - 1);

            /* Notice changes */
            /* if (!(i_ptr->timeout)) j++; */
        }
    }

    /* Recharge rods */
    for (i = 0; i < INVEN_PACK; i++) {
        i_ptr = &inventory[i];

        /* Examine all charging rods */
        if ((i_ptr->GetTval() == TV_ROD) && i_ptr->GetPval()) {
            /* Charge it */
            i_ptr->SetPval(i_ptr->GetPval() - 1);

            /* Notice changes */
            if (!(i_ptr->GetPval())) j++;
        }
    }

    /* Notice changes */
    if (j) {
        /* Combine pack */
        set_notice(get_notice() | PN_COMBINE);
    }

    /* Feel the inventory */
    SenseInventory();


    /*** Involuntary Movement ***/

    /* Delayed Word-of-Recall */
    if (GetWordRecall()) {
        /* Count down towards recall */
        SetWordRecall(GetWordRecall() - 1);

        // Activate the recall
        if (!GetWordRecall()) {
            // Determine the level
            if (dun_level) {
                msg_print("You feel yourself yanked upwards!");
                dun_level = 0;
                new_level_flag = TRUE;
            }
            else {
                msg_print("You feel yourself yanked downwards!");
                dun_level = GetMaxDlv();
                if (!dun_level) dun_level = 1;
                new_level_flag = TRUE;
            }
        }
    }
}


void CPlayer::GetSubtilePosition(int *x, int *y)
{
    // Set based on regular position
    *x = GetX()*1000;
    *y = GetY()*1000;

    // If moving, offset more
    if ((action >= 1) && (action <= 9)) {
        *x -= dx[action]*busy*10;
        *y -= dy[action]*busy*10;
    }
}


void CPlayer::Draw(int mcx, int mcy)
{
    int view, frame;
    char *scene_name;
    const int dx_to_move[10] = {0, 1, 2, 3, 0, 0, 4, 7, 6, 5};
    int player_x, player_y;
    int player_px, player_py;
    int offset_x = 0, offset_y = 0, offset = 0;
    CGrid *g_ptr = get_g_ptr();
    bool glowing;

    // Calculate player's px, py
    GetSubtilePosition(&player_x, &player_y);
    subtile_to_pixel(player_x, player_y, &player_px, &player_py);

    // Get view
    view = dx_to_move[last_move];

    // Get glowing
    glowing = g_ptr->is_glowing();

    // Add the player to layer 2
    if ((p_ptr->action >= 1) && (p_ptr->action <= 9)) {
        scene_name = "walk";
        frame = (100 - GetBusy())*10/84;
    }
    else if (p_ptr->action == 10) {
        scene_name = "fight";
        frame = (100/p_ptr->get_num_blow() - GetBusy())*p_ptr->get_num_blow()*10/72;

        if (frame == 0) offset = 20;
        else if (frame == 1) offset = 25;
        else if (frame == 2) offset = 30;
        else if (frame == 3) offset = 35;
        else if (frame == 4) offset = 40;
        else if (frame == 5) offset = 45;
        else if (frame == 6) offset = 50;
        else if (frame == 7) offset = 45;
        else if (frame == 8) offset = 40;
        else if (frame == 9) offset = 35;
        else if (frame == 10) offset = 30;
        else if (frame == 11) offset = 25;
        else if (frame == 12) offset = 20;
        else if (frame == 13) offset = 20;
    }
    else {
        scene_name = "base";
        frame = 0;
    }

    switch (view) {
        case 0:
            offset_x = -offset*32/70;
            break;
        case 1:
            offset_x = -offset*32/99;
            offset_y = offset*16/99;
            break;
        case 2:
            offset_y = offset*16/70;
            break;
        case 3:
            offset_x = offset*32/99;
            offset_y = offset*16/99;
            break;
        case 4: offset_x = offset*32/70;
            break;
        case 5:
            offset_x = offset*32/99;
            offset_y = -offset*16/99;
            break;
        case 6:
            offset_y = -offset*16/70;
            break;
        case 7:
            offset_x = -offset*32/99;
            offset_y = -offset*16/99;
            break;
    }

    add_sprite(player_x+player_y, mcx+offset_x, mcy+offset_y, "player",
        !glowing, scene_name, view, frame);
}
