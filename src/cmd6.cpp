// File: cmd6.c

// Purpose: Item commands



/*

 * Copyright (c) 1989 James E. Wilson, Robert A. Koeneke

 *

 * This software may be copied and distributed for educational, research, and

 * not for profit purposes provided that this copyright and statement are

 * included in all such copies.

 */



#include "utumno.h"







/*

 * This file includes code for eating food, drinking potions,

 * reading scrolls, aiming wands, using staffs, zapping rods,

 * and activating artifacts.

 *

 * In all cases, if the player becomes "aware" of the item's use

 * by testing it, mark it as "aware" and reward some experience

 * based on the object's level, always rounding up.  If the player

 * remains "unaware", mark that object "kind" as "tried".

 *

 * This code now correctly handles the unstacking of wands, staffs,

 * and rods.  Note the overly paranoid warning about potential pack

 * overflow, which allows the player to use and drop a stacked item.

 *

 * In all "unstacking" scenarios, the "used" object is "carried" as if

 * the player had just picked it up.  In particular, this means that if

 * the use of an item induces pack overflow, that item will be dropped.

 *

 * For simplicity, these routines induce a full "pack reorganization"

 * which not only combines similar items, but also reorganizes various

 * items to obey the current "sorting" method.  This may require about

 * 400 item comparisons, but only occasionally.

 *

 * There may be a BIG problem with any "effect" that can cause "changes"

 * to the inventory.  For example, a "scroll of recharging" can cause

 * a wand/staff to "disappear", moving the inventory up.  Luckily, the

 * scrolls all appear BEFORE the staffs/wands, so this is not a problem.

 * But, for example, a "staff of recharging" could cause MAJOR problems.

 * In such a case, it will be best to either (1) "postpone" the effect

 * until the end of the function, or (2) "change" the effect, say, into

 * giving a staff "negative" charges, or "turning a staff into a stick".

 * It seems as though a "rod of recharging" might in fact cause problems.

 * The basic problem is that the act of recharging (and destroying) an

 * item causes the inducer of that action to "move", causing "i_ptr" to

 * no longer point at the correct item, with horrifying results.

 */





/*

 * Eat some food (from the pack or floor)

 */

static void use_food(int sval, bool *id)

{

    bool ident = FALSE;



    // Analyze the food

    switch (sval) {

        case SV_FOOD_POISON:

            if (!p_ptr->get_resists(RESIST_POIS) && !p_ptr->GetOpposePois()) {

                if (p_ptr->mod_poisoned(p_ptr->GetPoisoned() + rand_int(10) + 10)) {

                    ident = TRUE;

                }

            }

            break;



        case SV_FOOD_BLINDNESS:

            if (!p_ptr->get_resists(RESIST_BLIND)) {

                if (p_ptr->mod_blind(p_ptr->GetBlind() + rand_int(200) + 200)) {

                    ident = TRUE;

                }

            }

            break;



        case SV_FOOD_PARANOIA:

            if (!p_ptr->get_resists(RESIST_FEAR)) {

                if (p_ptr->mod_afraid(p_ptr->GetAfraid() + rand_int(10) + 10)) {

                    ident = TRUE;

                }

            }

            break;



        case SV_FOOD_CONFUSION:

            if (!p_ptr->get_resists(RESIST_CONF)) {

                if (p_ptr->mod_confused(p_ptr->GetConfused() + rand_int(10) + 10)) {

                    ident = TRUE;

                }

            }

            break;



        case SV_FOOD_PARALYSIS:

            if (!p_ptr->get_free_act()) {

                if (p_ptr->mod_paralyzed(p_ptr->GetParalyzed() + rand_int(10) + 10)) {

                    ident = TRUE;

                }

            }

            break;



        case SV_FOOD_WEAKNESS:

            p_ptr->take_hit(damroll(6, 6), "poisonous food.");

            do_dec_stat(STAT_STR);

            ident = TRUE;

            break;



        case SV_FOOD_SICKNESS:

            p_ptr->take_hit(damroll(6, 6), "poisonous food.");

            do_dec_stat(STAT_CON);

            ident = TRUE;

            break;



        case SV_FOOD_STUPIDITY:

            p_ptr->take_hit(damroll(8, 8), "poisonous food.");

            do_dec_stat(STAT_INT);

            ident = TRUE;

            break;



        case SV_FOOD_NAIVETY:

            p_ptr->take_hit(damroll(8, 8), "poisonous food.");

            do_dec_stat(STAT_WIS);

            ident = TRUE;

            break;



        case SV_FOOD_UNHEALTH:

            p_ptr->take_hit(damroll(10, 10), "poisonous food.");

            do_dec_stat(STAT_CON);

            ident = TRUE;

            break;



        case SV_FOOD_DISEASE:

            p_ptr->take_hit(damroll(10, 10), "poisonous food.");

            do_dec_stat(STAT_STR);

            ident = TRUE;

            break;



        case SV_FOOD_CURE_POISON:

            if (p_ptr->mod_poisoned(0)) ident = TRUE;

            break;



        case SV_FOOD_CURE_BLINDNESS:

            if (p_ptr->mod_blind(0)) ident = TRUE;

            break;



        case SV_FOOD_CURE_PARANOIA:

            if (p_ptr->mod_afraid(0)) ident = TRUE;

            break;



        case SV_FOOD_CURE_CONFUSION:

            if (p_ptr->mod_confused(0)) ident = TRUE;

            break;



        case SV_FOOD_CURE_SERIOUS:

            if (p_ptr->heal_up(damroll(4, 8))) ident = TRUE;

            break;



        case SV_FOOD_RESTORE_STR:

            if (do_res_stat(STAT_STR)) ident = TRUE;

            break;



        case SV_FOOD_RESTORE_CON:

            if (do_res_stat(STAT_CON)) ident = TRUE;

            break;



        case SV_FOOD_RESTORING:

            if (do_res_stat(STAT_STR)) ident = TRUE;

            if (do_res_stat(STAT_INT)) ident = TRUE;

            if (do_res_stat(STAT_WIS)) ident = TRUE;

            if (do_res_stat(STAT_DEX)) ident = TRUE;

            if (do_res_stat(STAT_CON)) ident = TRUE;

            if (do_res_stat(STAT_CHR)) ident = TRUE;

            break;





        case SV_FOOD_RATION:

        case SV_FOOD_BISCUIT:

        case SV_FOOD_JERKY:

        case SV_FOOD_SLIME_MOLD:

            ident = TRUE;

            break;



        case SV_FOOD_WAYBREAD:

            p_ptr->mod_poisoned(0);

            p_ptr->heal_up(damroll(4, 8));

            ident = TRUE;

            break;



        case SV_FOOD_PINT_OF_ALE:

        case SV_FOOD_PINT_OF_WINE:

            ident = TRUE;

            break;



        default:

            quit("Oops.  Undefined food effect.");

            break;

    }



    *id = ident;

}









/*

 * Quaff a potion (from the pack or the floor)

 */

static void use_potion(int sval, bool *id)

{

    bool ident = FALSE;



    // Analyze the potion

    switch (sval) {

        case SV_POTION_WATER:

        case SV_POTION_APPLE_JUICE:

        case SV_POTION_SLIME_MOLD:

            msg_print("You feel less thirsty.");

            ident = TRUE;

            break;



        case SV_POTION_SLOWNESS:

            if (p_ptr->mod_slow(p_ptr->GetSlow() + randint(25) + 15)) ident = TRUE;

            break;



        case SV_POTION_SALT_WATER:

            msg_print("The potion makes you vomit!");

            p_ptr->mod_food(PY_FOOD_STARVE - 1);

            p_ptr->mod_poisoned(0);

            p_ptr->mod_paralyzed(p_ptr->GetParalyzed() + 4);

            ident = TRUE;

            break;



        case SV_POTION_POISON:

            if (!p_ptr->get_resists(RESIST_POIS) && !p_ptr->GetOpposePois()) {

                if (p_ptr->mod_poisoned(p_ptr->GetPoisoned() + rand_int(15) + 10)) {

                    ident = TRUE;

                }

            }

            break;



        case SV_POTION_BLINDNESS:

            if (!p_ptr->get_resists(RESIST_BLIND)) {

                if (p_ptr->mod_blind(p_ptr->GetBlind() + rand_int(100) + 100)) {

                    ident = TRUE;

                }

            }

            break;



        case SV_POTION_CONFUSION:

            if (!p_ptr->get_resists(RESIST_CONF)) {

                if (p_ptr->mod_confused(p_ptr->GetConfused() + rand_int(20) + 15)) {

                    ident = TRUE;

                }

            }

            break;



        case SV_POTION_SLEEP:

            if (!p_ptr->get_free_act()) {

                if (p_ptr->mod_paralyzed(p_ptr->GetParalyzed() + rand_int(4) + 4)) {

                    ident = TRUE;

                }

            }

            break;



        case SV_POTION_LOSE_MEMORIES:

            if (!p_ptr->get_hold_life() && (p_ptr->GetExp() > 0)) {

                msg_print("You feel your memories fade.");

                p_ptr->lose_exp(p_ptr->GetExp() / 4);

                ident = TRUE;

            }

            break;



        case SV_POTION_RUINATION:

            msg_print("Your nerves and muscles feel weak and lifeless!");

            p_ptr->take_hit(damroll(10, 10), "a potion of Ruination");

            dec_stat(STAT_DEX, 25, TRUE);

            dec_stat(STAT_WIS, 25, TRUE);

            dec_stat(STAT_CON, 25, TRUE);

            dec_stat(STAT_STR, 25, TRUE);

            dec_stat(STAT_CHR, 25, TRUE);

            dec_stat(STAT_INT, 25, TRUE);

            ident = TRUE;



        case SV_POTION_DEC_STR:

            if (do_dec_stat(STAT_STR)) ident = TRUE;

            break;



        case SV_POTION_DEC_INT:

            if (do_dec_stat(STAT_INT)) ident = TRUE;

            break;



        case SV_POTION_DEC_WIS:

            if (do_dec_stat(STAT_WIS)) ident = TRUE;

            break;



        case SV_POTION_DEC_DEX:

            if (do_dec_stat(STAT_DEX)) ident = TRUE;

            break;



        case SV_POTION_DEC_CON:

            if (do_dec_stat(STAT_CON)) ident = TRUE;

            break;



        case SV_POTION_DEC_CHR:

            if (do_dec_stat(STAT_CHR)) ident = TRUE;

            break;



        case SV_POTION_DETONATIONS:

            msg_print("Massive explosions rupture your body!");

            p_ptr->take_hit_internal(damroll(50, 20), "a potion of Detonation");

            p_ptr->mod_stun(p_ptr->GetStun() + 75);

            p_ptr->mod_cut(p_ptr->GetCut() + 5000);

            ident = TRUE;

            break;



        case SV_POTION_DEATH:

            msg_print("A feeling of Death flows through your body.");

            p_ptr->take_hit_internal(5000, "a potion of Death");

            ident = TRUE;

            break;



        case SV_POTION_INFRAVISION:

            if (p_ptr->mod_tim_infra(p_ptr->GetTimInfra() + 100 + randint(100))) {

                ident = TRUE;

            }

            break;



        case SV_POTION_DETECT_INVIS:

            if (p_ptr->mod_tim_invis(p_ptr->GetTimInvis() + 12 + randint(12))) {

                ident = TRUE;

            }

            break;



        case SV_POTION_SLOW_POISON:

            if (p_ptr->mod_poisoned(p_ptr->GetPoisoned() / 2)) ident = TRUE;

            break;



        case SV_POTION_CURE_POISON:

            if (p_ptr->mod_poisoned(0)) ident = TRUE;

            break;



        case SV_POTION_BOLDNESS:

            if (p_ptr->mod_afraid(0)) ident = TRUE;

            break;



        case SV_POTION_SPEED:

            if (!p_ptr->GetFast()) {

                if (p_ptr->mod_fast(randint(25) + 15)) ident = TRUE;

            }

            else {

                p_ptr->mod_fast(p_ptr->GetFast() + 5);

            }

            break;



        case SV_POTION_RESIST_HEAT:

            if (p_ptr->mod_oppose_fire(p_ptr->GetOpposeFire() + randint(10) + 10)) {

                ident = TRUE;

            }

            break;



        case SV_POTION_RESIST_COLD:

            if (p_ptr->mod_oppose_cold(p_ptr->GetOpposeCold() + randint(10) + 10)) {

                ident = TRUE;

            }

            break;



        case SV_POTION_HEROISM:

            if (p_ptr->heal_up(10)) ident = TRUE;

            if (p_ptr->mod_afraid(0)) ident = TRUE;

            if (p_ptr->mod_hero(p_ptr->GetHero() + randint(25) + 25)) ident = TRUE;

            break;



        case SV_POTION_BESERK_STRENGTH:

            if (p_ptr->heal_up(30)) ident = TRUE;

            if (p_ptr->mod_afraid(0)) ident = TRUE;

            if (p_ptr->mod_shero(p_ptr->GetSHero() + randint(25) + 25)) ident = TRUE;

            break;



        case SV_POTION_CURE_LIGHT:

            if (p_ptr->heal_up(damroll(2, 8))) ident = TRUE;

            if (p_ptr->mod_blind(0)) ident = TRUE;

            if (p_ptr->mod_cut(p_ptr->GetCut() - 10)) ident = TRUE;

            break;



        case SV_POTION_CURE_SERIOUS:

            if (p_ptr->heal_up(damroll(4, 8))) ident = TRUE;

            if (p_ptr->mod_blind(0)) ident = TRUE;

            if (p_ptr->mod_confused(0)) ident = TRUE;

            if (p_ptr->mod_cut((p_ptr->GetCut() / 2) - 50)) ident = TRUE;

            break;



        case SV_POTION_CURE_CRITICAL:

            if (p_ptr->heal_up(damroll(6, 8))) ident = TRUE;

            if (p_ptr->mod_blind(0)) ident = TRUE;

            if (p_ptr->mod_confused(0)) ident = TRUE;

            if (p_ptr->mod_poisoned(0)) ident = TRUE;

            if (p_ptr->mod_stun(0)) ident = TRUE;

            if (p_ptr->mod_cut(0)) ident = TRUE;

            break;



        case SV_POTION_HEALING:

            if (p_ptr->heal_up(300)) ident = TRUE;

            if (p_ptr->mod_blind(0)) ident = TRUE;

            if (p_ptr->mod_confused(0)) ident = TRUE;

            if (p_ptr->mod_poisoned(0)) ident = TRUE;

            if (p_ptr->mod_stun(0)) ident = TRUE;

            if (p_ptr->mod_cut(0)) ident = TRUE;

            break;



        case SV_POTION_STAR_HEALING:

            if (p_ptr->heal_up(1200)) ident = TRUE;

            if (p_ptr->mod_blind(0)) ident = TRUE;

            if (p_ptr->mod_confused(0)) ident = TRUE;

            if (p_ptr->mod_poisoned(0)) ident = TRUE;

            if (p_ptr->mod_stun(0)) ident = TRUE;

            if (p_ptr->mod_cut(0)) ident = TRUE;

            break;



        case SV_POTION_LIFE:

            msg_print("You feel life flow through your body!");

            p_ptr->restore_level();

            p_ptr->heal_up(5000);

            p_ptr->mod_poisoned(0);

            p_ptr->mod_blind(0);

            p_ptr->mod_confused(0);

            p_ptr->mod_stun(0);

            p_ptr->mod_cut(0);

            do_res_stat(STAT_STR);

            do_res_stat(STAT_CON);

            do_res_stat(STAT_DEX);

            do_res_stat(STAT_WIS);

            do_res_stat(STAT_INT);

            do_res_stat(STAT_CHR);

            ident = TRUE;

            break;



        case SV_POTION_RESTORE_MANA:

            if (p_ptr->GetCSP() < p_ptr->GetMSP()) {

                p_ptr->SetCSP(p_ptr->GetMSP());

                p_ptr->SetCSPFrac(0);

                msg_print("Your feel your head clear.");

                ident = TRUE;

            }

            break;



        case SV_POTION_RESTORE_EXP:

            if (p_ptr->restore_level()) ident = TRUE;

            break;



        case SV_POTION_RES_STR:

            if (do_res_stat(STAT_STR)) ident = TRUE;

            break;



        case SV_POTION_RES_INT:

            if (do_res_stat(STAT_INT)) ident = TRUE;

            break;



        case SV_POTION_RES_WIS:

            if (do_res_stat(STAT_WIS)) ident = TRUE;

            break;



        case SV_POTION_RES_DEX:

            if (do_res_stat(STAT_DEX)) ident = TRUE;

            break;



        case SV_POTION_RES_CON:

            if (do_res_stat(STAT_CON)) ident = TRUE;

            break;



        case SV_POTION_RES_CHR:

            if (do_res_stat(STAT_CHR)) ident = TRUE;

            break;



        case SV_POTION_INC_STR:

            if (do_inc_stat(STAT_STR)) ident = TRUE;

            break;



        case SV_POTION_INC_INT:

            if (do_inc_stat(STAT_INT)) ident = TRUE;

            break;



        case SV_POTION_INC_WIS:

            if (do_inc_stat(STAT_WIS)) ident = TRUE;

            break;



        case SV_POTION_INC_DEX:

            if (do_inc_stat(STAT_DEX)) ident = TRUE;

            break;



        case SV_POTION_INC_CON:

            if (do_inc_stat(STAT_CON)) ident = TRUE;

            break;



        case SV_POTION_INC_CHR:

            if (do_inc_stat(STAT_CHR)) ident = TRUE;

            break;



        case SV_POTION_AUGMENTATION:

            if (do_inc_stat(STAT_STR)) ident = TRUE;

            if (do_inc_stat(STAT_INT)) ident = TRUE;

            if (do_inc_stat(STAT_WIS)) ident = TRUE;

            if (do_inc_stat(STAT_DEX)) ident = TRUE;

            if (do_inc_stat(STAT_CON)) ident = TRUE;

            if (do_inc_stat(STAT_CHR)) ident = TRUE;

            break;



        case SV_POTION_ENLIGHTENMENT:

            msg_print("An image of your surroundings forms in your mind...");

            wiz_lite();

            ident = TRUE;

            break;



        case SV_POTION_STAR_ENLIGHTENMENT:

            mini_message_box("Note", "You begin to feel more enlightened...");

            wiz_lite();

            do_inc_stat(STAT_INT);

            do_inc_stat(STAT_WIS);

            detect_treasure();

            detect_object();

            detect_sdoor();

            detect_trap();

            identify_pack();

            self_knowledge();

            ident = TRUE;

            break;



        case SV_POTION_SELF_KNOWLEDGE:

            mini_message_box("Note", "You begin to know yourself a little better...");

            self_knowledge();

            ident = TRUE;

            break;



        case SV_POTION_EXPERIENCE:

            if (p_ptr->GetExp() < PY_MAX_EXP) {

                s32b ee = (p_ptr->GetExp() / 2) + 10;

                if (ee > 100000L) ee = 100000L;

                msg_print("You feel more experienced.");

                p_ptr->gain_exp(ee);

                ident = TRUE;

            }

            break;



        default:

            quit("Oops.  Undefined potion.");

            break;

    }



    *id = ident;

}





/*

 * Read a scroll (from the pack or floor).

 *

 * Certain scrolls can be "aborted" without losing the scroll.  These

 * include scrolls with no effects but recharge or identify, which are

 * cancelled before use.  XXX Reading them still takes a turn, though.

 */

static void use_scroll(int sval, bool *id, bool *use_up)

{

    int k;

    bool used_up = TRUE;

    bool ident = FALSE;





    // Analyze the scroll

    switch (sval) {

        case SV_SCROLL_DARKNESS:

            if (unlite_area(10, 3)) ident = TRUE;

            if (!p_ptr->get_resists(RESIST_BLIND)) {

                p_ptr->mod_blind(p_ptr->GetBlind() + 3 + randint(5));

            }

            break;



        case SV_SCROLL_AGGRAVATE_MONSTER:

            msg_print("There is a high pitched humming noise.");

            aggravate_monsters(NULL);

            ident = TRUE;

            break;



        case SV_SCROLL_CURSE_ARMOR:

            if (curse_armor()) ident = TRUE;

            break;



        case SV_SCROLL_CURSE_WEAPON:

            if (curse_weapon()) ident = TRUE;

            break;



        case SV_SCROLL_SUMMON_MONSTER:

            for (k = 0; k < randint(3); k++) {

                if (summon_specific(p_ptr->GetY(), p_ptr->GetX(), dun_level, 0)) {

                    ident = TRUE;

                }

            }

            break;



        case SV_SCROLL_SUMMON_UNDEAD:

            for (k = 0; k < randint(3); k++) {

                if (summon_specific(p_ptr->GetY(), p_ptr->GetX(), dun_level, SUMMON_UNDEAD)) {

                    ident = TRUE;

                }

            }

            break;



        case SV_SCROLL_TRAP_CREATION:

            if (trap_creation()) ident = TRUE;

            break;



        case SV_SCROLL_PHASE_DOOR:

            teleport_player(10);

            ident = TRUE;

            break;



        case SV_SCROLL_TELEPORT:

            teleport_player(100);

            ident = TRUE;

            break;



        case SV_SCROLL_TELEPORT_LEVEL:

            (void)teleport_player_level();

            ident = TRUE;

            break;



        case SV_SCROLL_WORD_OF_RECALL:

            if (!p_ptr->GetWordRecall()) {

                p_ptr->SetWordRecall(randint(20) + 15);

                msg_print("The air about you becomes charged...");

            }

            else {

                p_ptr->SetWordRecall(0);

                msg_print("A tension leaves the air around you...");

            }

            ident = TRUE;

            break;



        case SV_SCROLL_IDENTIFY:

            ident = TRUE;

            if (!ident_spell()) used_up = FALSE;

            break;



        case SV_SCROLL_STAR_IDENTIFY:

            ident = TRUE;

            if (!identify_fully()) used_up = FALSE;

            break;



        case SV_SCROLL_REMOVE_CURSE:

            if (remove_curse()) {

                msg_print("You feel as if someone is watching over you.");

                ident = TRUE;

            }

            break;



        case SV_SCROLL_STAR_REMOVE_CURSE:

            remove_all_curse();

            ident = TRUE;

            break;



        case SV_SCROLL_ENCHANT_ARMOR:

            ident = TRUE;

            if (!enchant_spell(0, 0, 1)) used_up = FALSE;

            break;



        case SV_SCROLL_ENCHANT_WEAPON_TO_HIT:

            if (!enchant_spell(1, 0, 0)) used_up = FALSE;

            ident = TRUE;

            break;



        case SV_SCROLL_ENCHANT_WEAPON_TO_DAM:

            if (!enchant_spell(0, 1, 0)) used_up = FALSE;

            ident = TRUE;

            break;



        case SV_SCROLL_STAR_ENCHANT_ARMOR:

            if (!enchant_spell(0, 0, randint(3) + 2)) used_up = FALSE;

            ident = TRUE;

            break;



        case SV_SCROLL_STAR_ENCHANT_WEAPON:

            if (!enchant_spell(randint(3), randint(3), 0)) used_up = FALSE;

            ident = TRUE;

            break;



        case SV_SCROLL_RECHARGING:

            if (!recharge(60)) used_up = FALSE;

            ident = TRUE;

            break;



        case SV_SCROLL_LIGHT:

            if (lite_area(damroll(2, 8), 2)) ident = TRUE;

            break;



        case SV_SCROLL_MAPPING:

            map_area();

            ident = TRUE;

            break;



        case SV_SCROLL_DETECT_GOLD:

            if (detect_treasure()) ident = TRUE;

            break;



        case SV_SCROLL_DETECT_ITEM:

            if (detect_object()) ident = TRUE;

            break;



        case SV_SCROLL_DETECT_TRAP:

            if (detect_trap()) ident = TRUE;

            break;



        case SV_SCROLL_DETECT_DOOR:

            if (detect_sdoor()) ident = TRUE;

            break;



        case SV_SCROLL_DETECT_INVIS:

            if (detect_invisible()) ident = TRUE;

            break;



        case SV_SCROLL_SATISFY_HUNGER:

            if (p_ptr->mod_food(PY_FOOD_MAX - 1)) ident = TRUE;

            break;



        case SV_SCROLL_BLESSING:

            if (p_ptr->mod_blessed(p_ptr->GetBlessed() + randint(12) + 6)) ident = TRUE;

            break;



        case SV_SCROLL_HOLY_CHANT:

            if (p_ptr->mod_blessed(p_ptr->GetBlessed() + randint(24) + 12)) ident = TRUE;

            break;



        case SV_SCROLL_HOLY_PRAYER:

            if (p_ptr->mod_blessed(p_ptr->GetBlessed() + randint(48) + 24)) ident = TRUE;

            break;



        case SV_SCROLL_MONSTER_CONFUSION:

            if (!p_ptr->GetConfusing()) {

                msg_print("Your hands begin to glow.");

                p_ptr->SetConfusing(TRUE);

                ident = TRUE;

            }

            break;



        case SV_SCROLL_PROTECTION_FROM_EVIL:

            k = 3 * p_ptr->GetLev();

            if (p_ptr->mod_protevil(p_ptr->GetProtevil() + randint(25) + k)) ident = TRUE;

            break;



        case SV_SCROLL_RUNE_OF_PROTECTION:

            warding_glyph();

            ident = TRUE;

            break;



        case SV_SCROLL_TRAP_DOOR_DESTRUCTION:

            if (destroy_doors_touch()) ident = TRUE;

            break;



        case SV_SCROLL_STAR_DESTRUCTION:

            destroy_area(p_ptr->GetY(), p_ptr->GetX(), 15, TRUE);

            ident = TRUE;

            break;



        case SV_SCROLL_DISPEL_UNDEAD:

            if (dispel_undead(60)) ident = TRUE;

            break;



        case SV_SCROLL_GENOCIDE:

            genocide();

            ident = TRUE;

            break;



        case SV_SCROLL_MASS_GENOCIDE:

            mass_genocide();

            ident = TRUE;

            break;



        case SV_SCROLL_ACQUIREMENT:

            acquirement(p_ptr->GetY(), p_ptr->GetX(), 1, TRUE);

            ident = TRUE;

            break;



        case SV_SCROLL_STAR_ACQUIREMENT:

            acquirement(p_ptr->GetY(), p_ptr->GetX(), randint(2) + 1, TRUE);

            ident = TRUE;

            break;



        default:

            quit("Oops.  Undefined scroll.");

            break;

    }



    *use_up = used_up;

    *id = ident;

}







/*

 * Hook to determine if an object can be used

 */

static bool item_tester_hook_use(CItem *i_ptr)

{

    // Flask?

    if (i_ptr->GetTval() == TV_FLASK) {

        CItem *j_ptr = &inventory[INVEN_LITE];



        // No light

        if (j_ptr->GetTval() != TV_LITE) return FALSE;



        // Non-lantern light

        if (j_ptr->GetSval() != SV_LITE_LANTERN) return FALSE;



        // OK

        return TRUE;

    }



    // Potion?

    if (i_ptr->GetTval() == TV_POTION) return TRUE;



    // Food?

    if (i_ptr->GetTval() == TV_FOOD) return TRUE;



    // Scroll?

    if (i_ptr->GetTval() == TV_SCROLL) {

        // Mustn't be blind

        if (p_ptr->GetBlind()) return FALSE;



        // Must have light

        if (p_ptr->no_lite()) return FALSE;



        // Mustn't be confused

        if (p_ptr->GetConfused()) return FALSE;



        // OK

        return TRUE;

    }





    // Cannot be used

    return FALSE;

}







/*

 * Use an item (food, scroll, potion, oil)

 */

bool do_cmd_use(void)

{

    int item;

    CItem *i_ptr;

    bool use_up = TRUE;



    if (p_ptr->isBusy()) return FALSE;



    // Set up the hook

    item_tester_hook = item_tester_hook_use;



    // Get an item (from inven or floor)

    if (!get_item(&item, "Use which item? ", GI_INVEN | GI_FLOOR)) {

        if (item == -2) msg_print("You have nothing that you can use.");

        reset_timer(); //pause game while in screen

        return TRUE;

    }



    // Get the item (in the pack)

    i_ptr = gi_i_ptr;





    // Use a turn

    p_ptr->DrainEnergy(100);





    // Flasks are rather different than the others

    if (i_ptr->GetTval() == TV_FLASK) {

        CItem *j_ptr = &inventory[INVEN_LITE];



        // Refuel

        j_ptr->SetPval(j_ptr->GetPval() + i_ptr->GetPval());



        // Message

        msg_print("You fuel your lamp.");



        // Comment

        if (j_ptr->GetPval() >= FUEL_LAMP) {

            j_ptr->SetPval(FUEL_LAMP);

            msg_print("Your lamp is full.");

        }



        // Recalculate torch

        p_ptr->set_update(p_ptr->get_update() | PU_TORCH);

    }



    // Everything else shares properties

    else {

        bool ident;

        int lev = i_ptr->GetObjLevel();



        if (i_ptr->GetTval() == TV_SCROLL) {

            use_scroll(i_ptr->GetSval(), &ident, &use_up);

        }

        else if (i_ptr->GetTval() == TV_POTION) {

            use_potion(i_ptr->GetSval(), &ident);



            // Potions can feed the player

            p_ptr->mod_food(p_ptr->GetFood() + i_ptr->GetPval());

        }

        else if (i_ptr->GetTval() == TV_FOOD) {

            use_food(i_ptr->GetSval(), &ident);



            // Food can feed the player

            p_ptr->mod_food(p_ptr->GetFood() + i_ptr->GetPval());

        }

        else {

            quit("Invalid item tval in do_cmd_use()!");

        }



        // Combine / Reorder the pack (later)

        p_ptr->set_notice(p_ptr->get_notice() | PN_COMBINE | PN_REORDER);



        // We have tried it

        i_ptr->object_tried();



        // The player is now aware of the object

        if (ident && !i_ptr->isAware()) {

            i_ptr->object_aware();

            p_ptr->gain_exp((lev + (p_ptr->GetLev() >> 1)) / p_ptr->GetLev());

        }

    }



    // Use it up?

    if (use_up) {

        // Decrease the item (from the pack)

        if (item >= 0) {

            inven_item_increase(item, -1);

            inven_item_describe(item);

            inven_item_optimize(item);

        }



        // Decrease the item (from the floor)

        else {

            floor_item_increase(i_ptr, -1);

            floor_item_describe(i_ptr);

            floor_item_optimize(i_ptr);

        }

    }

    reset_timer(); //pause game while in screen

    // Did something

    return TRUE;

}







/*

 * Determine whether the player is successful at using a magical object

 * (wand, staff, rod, artifact).

 *

 * Culled from several functions by MJC.

 */

static bool use_magic(int lev)

{

    int chance = p_ptr->GetSkill(SKILL_DEV);



    // Confusion hurts skill

    if (p_ptr->GetConfused()) chance = chance / 2;



    // High level objects are harder

    chance = chance - ((lev > 50) ? 50 : lev);



    // Give everyone a (slight) chance

    if ((chance < USE_DEVICE) && one_in(USE_DEVICE - chance + 1)) {

        chance = USE_DEVICE;

    }



    // Roll for usage

    if ((chance < USE_DEVICE) || (randint(chance) < USE_DEVICE)) return FALSE;



    // Otherwise, it worked

    return TRUE;

}















/*

 * Carry out the effects of zapping a staff

 */

static void do_zap_staff(int sval, bool *id, bool *use_c)

{

    bool ident = FALSE;

    bool use_charge = TRUE;

    int k;



    // Analyze the staff

    switch (sval) {

        case SV_STAFF_DARKNESS:

            if (unlite_area(10, 3)) ident = TRUE;

            if (!p_ptr->get_resists(RESIST_BLIND)) {

                if (p_ptr->mod_blind(p_ptr->GetBlind() + 3 + randint(5))) ident = TRUE;

            }

            break;



        case SV_STAFF_SLOWNESS:

            if (p_ptr->mod_slow(p_ptr->GetSlow() + randint(30) + 15)) ident = TRUE;

            break;



        case SV_STAFF_HASTE_MONSTERS:

            if (speed_monsters()) ident = TRUE;

            break;



        case SV_STAFF_SUMMONING:

            for (k = 0; k < randint(4); k++) {

                if (summon_specific(p_ptr->GetY(), p_ptr->GetX(), dun_level, 0)) {

                    ident = TRUE;

                }

            }

            break;



        case SV_STAFF_TELEPORTATION:

            teleport_player(100);

            ident = TRUE;

            break;



        case SV_STAFF_IDENTIFY:

            if (!ident_spell()) use_charge = FALSE;

            ident = TRUE;

            break;



        case SV_STAFF_REMOVE_CURSE:

            if (remove_curse()) {

                if (!p_ptr->GetBlind()) {

                    msg_print("The staff glows blue for a moment...");

                }

                ident = TRUE;

            }

            break;



        case SV_STAFF_STARLITE:

            if (!p_ptr->GetBlind()) {

                msg_print("The end of the staff glows brightly...");

            }

            for (k = 0; k < 8; k++) {

                lite_line(p_ptr->GetX()+ddx[ddd[k]],

                          p_ptr->GetY()+ddy[ddd[k]]);

            }

            ident = TRUE;

            break;



        case SV_STAFF_LITE:

            if (lite_area(damroll(2, 8), 2)) ident = TRUE;

            break;



        case SV_STAFF_MAPPING:

            map_area();

            ident = TRUE;

            break;



        case SV_STAFF_DETECT_GOLD:

            if (detect_treasure()) ident = TRUE;

            break;



        case SV_STAFF_DETECT_ITEM:

            if (detect_object()) ident = TRUE;

            break;



        case SV_STAFF_DETECT_TRAP:

            if (detect_trap()) ident = TRUE;

            break;



        case SV_STAFF_DETECT_DOOR:

            if (detect_sdoor()) ident = TRUE;

            break;



        case SV_STAFF_DETECT_INVIS:

            if (detect_invisible()) ident = TRUE;

            break;



        case SV_STAFF_DETECT_EVIL:

            if (detect_evil()) ident = TRUE;

            break;



        case SV_STAFF_CURE_LIGHT:

            if (p_ptr->heal_up(randint(8))) ident = TRUE;

            break;



        case SV_STAFF_CURING:

            if (p_ptr->mod_blind(0)) ident = TRUE;

            if (p_ptr->mod_poisoned(0)) ident = TRUE;

            if (p_ptr->mod_confused(0)) ident = TRUE;

            if (p_ptr->mod_stun(0)) ident = TRUE;

            if (p_ptr->mod_cut(0)) ident = TRUE;

            break;



        case SV_STAFF_HEALING:

            if (p_ptr->heal_up(300)) ident = TRUE;

            if (p_ptr->mod_stun(0)) ident = TRUE;

            if (p_ptr->mod_cut(0)) ident = TRUE;

            break;



        case SV_STAFF_THE_MAGI:

            if (do_res_stat(STAT_INT)) ident = TRUE;

            if (p_ptr->GetCSP() < p_ptr->GetMSP()) {

                p_ptr->SetCSP(p_ptr->GetMSP());

                p_ptr->SetCSPFrac(0);

                ident = TRUE;

                msg_print("Your feel your head clear.");

            }

            break;



        case SV_STAFF_SLEEP_MONSTERS:

            if (sleep_monsters()) ident = TRUE;

            break;



        case SV_STAFF_SLOW_MONSTERS:

            if (slow_monsters()) ident = TRUE;

            break;



        case SV_STAFF_SPEED:

            if (!p_ptr->GetFast()) {

                if (p_ptr->mod_fast(randint(30) + 15)) ident = TRUE;

            }

            else {

                p_ptr->mod_fast(p_ptr->GetFast() + 5);

            }

            break;



        case SV_STAFF_PROBING:

            probing();

            ident = TRUE;

            break;



        case SV_STAFF_DISPEL_EVIL:

            if (dispel_evil(60)) ident = TRUE;

            break;



        case SV_STAFF_POWER:

            if (dispel_monsters(120)) ident = TRUE;

            break;



        case SV_STAFF_HOLINESS:

            if (dispel_evil(120)) ident = TRUE;

            if (p_ptr->mod_protevil(p_ptr->GetProtevil() + randint(25) + 3*p_ptr->GetLev())) {

                ident = TRUE;

            }

            if (p_ptr->mod_poisoned(0)) ident = TRUE;

            if (p_ptr->mod_afraid(0)) ident = TRUE;

            if (p_ptr->heal_up(50)) ident = TRUE;

            if (p_ptr->mod_stun(0)) ident = TRUE;

            if (p_ptr->mod_cut(0)) ident = TRUE;

            break;



        case SV_STAFF_GENOCIDE:

            genocide();

            ident = TRUE;

            break;



        case SV_STAFF_EARTHQUAKES:

            earthquake(p_ptr->GetY(), p_ptr->GetX(), 10);

            ident = TRUE;

            break;



        case SV_STAFF_DESTRUCTION:

            destroy_area(p_ptr->GetY(), p_ptr->GetX(), 15, TRUE);

            ident = TRUE;

            break;





        default:

            quit("Oops.  Undefined staff.");

            break;

    }



    // Copy results into the pointers

    *id = ident;

    *use_c = use_charge;

}





/*

 * Carry out the effects of zapping a wand

 */

static void do_zap_wand(int sval, int tx, int ty, bool *id, bool *use_c)

{

    bool ident = FALSE;

    bool use_charge = TRUE;



    // XXX Hack -- Wand of wonder can do anything before it

    if (sval == SV_WAND_WONDER) sval = rand_int(SV_WAND_WONDER);



    // Analyze the wand

    switch (sval) {

        case SV_WAND_HEAL_MONSTER:

            if (heal_monster(tx, ty)) ident = TRUE;

            break;



        case SV_WAND_HASTE_MONSTER:

            if (speed_monster(tx, ty)) ident = TRUE;

            break;



        case SV_WAND_CLONE_MONSTER:

            if (clone_monster(tx, ty)) ident = TRUE;

            break;



        case SV_WAND_TELEPORT_AWAY:

            if (teleport_monster(tx, ty)) ident = TRUE;

            break;



        case SV_WAND_DISARMING:

            if (disarm_trap(tx, ty)) ident = TRUE;

            break;



        case SV_WAND_TRAP_DOOR_DEST:

            if (destroy_door(tx, ty)) ident = TRUE;

            break;



        case SV_WAND_STONE_TO_MUD:

            if (wall_to_mud(tx, ty)) ident = TRUE;

            break;



        case SV_WAND_LITE:

            msg_print("A line of blue shimmering light appears.");

            lite_line(tx, ty);

            ident = TRUE;

            break;



        case SV_WAND_SLEEP_MONSTER:

            if (sleep_monster(tx, ty)) ident = TRUE;

            break;



        case SV_WAND_SLOW_MONSTER:

            if (slow_monster(tx, ty)) ident = TRUE;

            break;



        case SV_WAND_CONFUSE_MONSTER:

            if (confuse_monster(tx, ty, 10)) ident = TRUE;

            break;



        case SV_WAND_FEAR_MONSTER:

            if (fear_monster(tx, ty, 10)) ident = TRUE;

            break;



        case SV_WAND_DRAIN_LIFE:

            if (drain_life(tx, ty, 75)) ident = TRUE;

            break;



        case SV_WAND_POLYMORPH:

            if (poly_monster(tx, ty)) ident = TRUE;

            break;



        case SV_WAND_STINKING_CLOUD:

            fire_ball(GF_POIS, tx, ty, 12, 2);

            ident = TRUE;

            break;



        case SV_WAND_MAGIC_MISSILE:

            fire_bolt_or_beam(20, GF_MISSILE, tx, ty, damroll(2,6));

            ident = TRUE;

            break;



        case SV_WAND_ACID_BOLT:

            fire_bolt_or_beam(20, GF_ACID, tx, ty, damroll(5,8));

            ident = TRUE;

            break;



        case SV_WAND_ELEC_BOLT:

            fire_bolt_or_beam(20, GF_ELEC, tx, ty, damroll(3,8));

            ident = TRUE;

            break;



        case SV_WAND_FIRE_BOLT:

            fire_bolt_or_beam(20, GF_FIRE, tx, ty, damroll(6,8));

            ident = TRUE;

            break;



        case SV_WAND_COLD_BOLT:

            fire_bolt_or_beam(20, GF_COLD, tx, ty, damroll(3,8));

            ident = TRUE;

            break;



        case SV_WAND_ACID_BALL:

            fire_ball(GF_ACID, tx, ty, 60, 2);

            ident = TRUE;

            break;



        case SV_WAND_ELEC_BALL:

            fire_ball(GF_ELEC, tx, ty, 32, 2);

            ident = TRUE;

            break;



        case SV_WAND_FIRE_BALL:

            fire_ball(GF_FIRE, tx, ty, 72, 2);

            ident = TRUE;

            break;



        case SV_WAND_COLD_BALL:

            fire_ball(GF_COLD, tx, ty, 48, 2);

            ident = TRUE;

            break;



        case SV_WAND_WONDER:

            quit("Oops.  Wand of wonder activated.");

            break;



        case SV_WAND_DRAGON_FIRE:

            fire_ball(GF_FIRE, tx, ty, 100, 3);

            ident = TRUE;

            break;



        case SV_WAND_DRAGON_COLD:

            fire_ball(GF_COLD, tx, ty, 80, 3);

            ident = TRUE;

            break;



        case SV_WAND_DRAGON_BREATH:

            switch (randint(5)) {

                case 1:

                    fire_ball(GF_ACID, tx, ty, 100, 3);

                    break;

                case 2:

                    fire_ball(GF_ELEC, tx, ty, 80, 3);

                    break;

                case 3:

                    fire_ball(GF_FIRE, tx, ty, 100, 3);

                    break;

                case 4:

                    fire_ball(GF_COLD, tx, ty, 80, 3);

                    break;

                default:

                    fire_ball(GF_POIS, tx, ty, 60, 3);

                    break;

            }

            ident = TRUE;

            break;



        case SV_WAND_ANNIHILATION:

            if (drain_life(tx, ty, 125)) ident = TRUE;

            break;



        default:

            quit("Oops.  Undefined wand.");

            break;

    }



    // Copy results into the pointers

    *id = ident;

    *use_c = use_charge;

}











/*

 * Carry out the effects of zapping a rod

 */

static int do_zap_rod(int sval, int tx, int ty, bool *id, bool *use_c)

{

    bool ident = FALSE;

    bool use_charge = TRUE;

    int new_pval = 0;



    // Analyze the rod

    switch (sval) {

        case SV_ROD_DETECT_TRAP:

            if (detect_trap()) ident = TRUE;

            new_pval = 20;

            break;



        case SV_ROD_DETECT_DOOR:

            if (detect_sdoor()) ident = TRUE;

            new_pval = 40;

            break;



        case SV_ROD_IDENTIFY:

            ident = TRUE;

            if (!ident_spell()) use_charge = FALSE;

            new_pval = 10;

            break;



        case SV_ROD_RECALL:

            if (!p_ptr->GetWordRecall()) {

                msg_print("The air about you becomes charged...");

                p_ptr->SetWordRecall(15 + randint(20));

            }

            else {

                msg_print("A tension leaves the air around you...");

                p_ptr->SetWordRecall(0);

            }

            ident = TRUE;

            new_pval = 60;

            break;



        case SV_ROD_ILLUMINATION:

            if (lite_area(damroll(2, 8), 2)) ident = TRUE;

            new_pval = 30;

            break;



        case SV_ROD_MAPPING:

            map_area();

            ident = TRUE;

            new_pval = 99;

            break;



        case SV_ROD_DETECTION:

            detection();

            ident = TRUE;

            new_pval = 99;

            break;



        case SV_ROD_PROBING:

            probing();

            ident = TRUE;

            new_pval = 50;

            break;



        case SV_ROD_CURING:

            if (p_ptr->mod_blind(0)) ident = TRUE;

            if (p_ptr->mod_poisoned(0)) ident = TRUE;

            if (p_ptr->mod_confused(0)) ident = TRUE;

            if (p_ptr->mod_stun(0)) ident = TRUE;

            if (p_ptr->mod_cut(0)) ident = TRUE;

            new_pval = 999;

            break;



        case SV_ROD_HEALING:

            if (p_ptr->heal_up(500)) ident = TRUE;

            if (p_ptr->mod_stun(0)) ident = TRUE;

            if (p_ptr->mod_cut(0)) ident = TRUE;

            new_pval = 999;

            break;



        case SV_ROD_RESTORATION:

            if (p_ptr->restore_level()) ident = TRUE;

            if (do_res_stat(STAT_STR)) ident = TRUE;

            if (do_res_stat(STAT_INT)) ident = TRUE;

            if (do_res_stat(STAT_WIS)) ident = TRUE;

            if (do_res_stat(STAT_DEX)) ident = TRUE;

            if (do_res_stat(STAT_CON)) ident = TRUE;

            if (do_res_stat(STAT_CHR)) ident = TRUE;

            new_pval = 999;

            break;



        case SV_ROD_SPEED:

            if (!p_ptr->GetFast()) {

                if (p_ptr->mod_fast(randint(30) + 15)) ident = TRUE;

            }

            else {

                p_ptr->mod_fast(p_ptr->GetFast() + 5);

            }

            new_pval = 99;

            break;



        case SV_ROD_TELEPORT_AWAY:

            if (teleport_monster(tx, ty)) ident = TRUE;

            new_pval = 25;

            break;



        case SV_ROD_DISARMING:

            if (disarm_trap(tx, ty)) ident = TRUE;

            new_pval = 30;

            break;



        case SV_ROD_LITE:

            msg_print("A line of blue shimmering light appears.");

            lite_line(tx, ty);

            ident = TRUE;

            new_pval = 9;

            break;



        case SV_ROD_SLEEP_MONSTER:

            if (sleep_monster(tx, ty)) ident = TRUE;

            new_pval = 18;

            break;



        case SV_ROD_SLOW_MONSTER:

            if (slow_monster(tx, ty)) ident = TRUE;

            new_pval = 20;

            break;



        case SV_ROD_DRAIN_LIFE:

            if (drain_life(tx, ty, 75)) ident = TRUE;

            new_pval = 23;

            break;



        case SV_ROD_POLYMORPH:

            if (poly_monster(tx, ty)) ident = TRUE;

            new_pval = 25;

            break;



        case SV_ROD_ACID_BOLT:

            fire_bolt_or_beam(10, GF_ACID, tx, ty, damroll(6,8));

            ident = TRUE;

            new_pval = 12;

            break;



        case SV_ROD_ELEC_BOLT:

            fire_bolt_or_beam(10, GF_ELEC, tx, ty, damroll(3,8));

            ident = TRUE;

            new_pval = 11;

            break;



        case SV_ROD_FIRE_BOLT:

            fire_bolt_or_beam(10, GF_FIRE, tx, ty, damroll(8,8));

            ident = TRUE;

            new_pval = 15;

            break;



        case SV_ROD_COLD_BOLT:

            fire_bolt_or_beam(10, GF_COLD, tx, ty, damroll(5,8));

            ident = TRUE;

            new_pval = 13;

            break;



        case SV_ROD_ACID_BALL:

            fire_ball(GF_ACID, tx, ty, 60, 2);

            ident = TRUE;

            new_pval = 27;

            break;



        case SV_ROD_ELEC_BALL:

            fire_ball(GF_ELEC, tx, ty, 32, 2);

            ident = TRUE;

            new_pval = 23;

            break;



        case SV_ROD_FIRE_BALL:

            fire_ball(GF_FIRE, tx, ty, 72, 2);

            ident = TRUE;

            new_pval = 30;

            break;



        case SV_ROD_COLD_BALL:

            fire_ball(GF_COLD, tx, ty, 48, 2);

            ident = TRUE;

            new_pval = 25;

            break;



        default:

            quit("Oops.  Undefined rod.");

            break;

    }



    // If a charge is not being used, the new pval should be 0

    if (!use_charge) new_pval = 0;



    // Copy results into the pointers

    *id = ident;

    *use_c = use_charge;



    // Return the new pval

    return new_pval;

}







/*

 * Hook to determine if an object can be zapped

 */

static bool item_tester_hook_zap(CItem *i_ptr)

{

    // Test the three tvals

    if (i_ptr->GetTval() == TV_ROD) return TRUE;

    if (i_ptr->GetTval() == TV_WAND) return TRUE;

    if (i_ptr->GetTval() == TV_STAFF) return TRUE;



    // Otherwise, cannot be zapped

    return FALSE;

}







/*

 * Zap a rod, staff, or wand.

 *

 * Combined from three commands in earlier versions.

 *

 * If desired, a zapping may be cancelled and no charge may be used.

 * All rods and wands can be cancelled at the "Direction?" prompt.

 *

 * Unstacking is handled in a logical fashion.

 *

 * Zapped wands and staves lose one charge.  Zapped rods get their

 * recharge time set.

 *

 * There are no wands which can "destroy" themselves, in the inventory

 * or on the ground, so we can ignore this possibility.  Note that this

 * required giving "wand of wonder" the ability to ignore destruction

 * by electric balls.

 *

 * Note that the basic "bolt" wands do slightly less damage than the

 * basic "bolt" rods, but the basic "ball" wands do the same damage

 * as the basic "ball" rods.

 */

void do_zap(int tval, int sval, int tx, int ty)

{

    int item, lev, pval;

    CItem *i_ptr;

    bool found = FALSE, ident, use_charge;

    char *item_type;



    if (p_ptr->isBusy()) return;



    // Get a string for the item's type

    if (tval == TV_ROD) item_type = "rod";

    else if (tval == TV_STAFF) item_type = "staff";

    else if (tval == TV_WAND) item_type = "wand";

    else {

        quit("Error: incorrect tval in do_zap()");

        return;

    }



    // Find an item of that tval/sval

    for (item = 0; item < INVEN_PACK; item++) {

        // Get item pointer

        i_ptr = &inventory[item];



        // Right tval/sval?

        if ((i_ptr->GetTval() == tval) && (i_ptr->GetSval() == sval)) {

            // Rods must have 0 pval

            if ((tval == TV_ROD) && (i_ptr->GetPval() == 0)) {

                found = TRUE;

                break;

            }



            // Other: must be positive

            else if (i_ptr->GetPval() > 0) {

                found = TRUE;

                break;

            }

        }

    }



    // Didn't find anything?

    if (!found) {

        msg_print("You don't have anything like that you can zap now!");

        return;

    }



    // Deal with annoying warnings

    i_ptr = &inventory[item];





    // Acquire pval and level

    pval = i_ptr->GetPval();

    lev = i_ptr->GetObjLevel();



    // Take a turn

    p_ptr->DrainEnergy(100);



    // Roll for usage

    if (!use_magic(lev)) {

        msg_format("You failed to use the %s properly.", item_type);

        return;

    }





    // Rods have different charge properties

    if (tval == TV_ROD) {

        // Still charging

        if (pval) {

            msg_print("The rod is still charging.");

            return;

        }

    }



    // Wands and staves have charges

    else {

        // Notice empty wands and staves

        if (pval <= 0) {

            msg_format("The %s has no charges left.", item_type);

            i_ptr->SetIdentFlag(ID_EMPTY);

            return;

        }

    }





    if (tval == TV_ROD) {

        int new_pval = do_zap_rod(sval, tx, ty, &ident, &use_charge);

        if (use_charge) i_ptr->SetPval(new_pval);

    }

    else if (tval == TV_WAND) {

        do_zap_wand(sval, tx, ty, &ident, &use_charge);

    }

    else if (tval == TV_STAFF) {

        do_zap_staff(sval, &ident, &use_charge);

    }

    else {

        quit("Error: incorrect tval in do_zap()");

        return;

    }





    // Combine / Reorder the pack (later)

    p_ptr->set_notice(p_ptr->get_notice() | PN_COMBINE | PN_REORDER);



    // Tried the object

    i_ptr->object_tried();



    // Successfully determined the object function

    if (ident && !i_ptr->isAware()) {

        i_ptr->object_aware();

        p_ptr->gain_exp((lev + (p_ptr->GetLev() >> 1)) / p_ptr->GetLev());

    }





    // Deal with cancelled zap

    if (!use_charge) return;





    // For non-rods, use a single charge

    if (tval != TV_ROD) {

        i_ptr->SetPval(pval-1);

    }





    // Hack -- unstack if necessary

    if ((item >= 0) && i_ptr->isPlural()) {

        // Make a fake item

        CItem tmp_obj;

        tmp_obj = *i_ptr;

        tmp_obj.SetNumber(1);



        // Restore charge value

        i_ptr->SetPval(pval);



        // Unstack the used item

        i_ptr->SetNumber(i_ptr->GetNumber() - 1);

        item = inven_carry(&tmp_obj);



        // Message

        msg_format("You unstack your %s.", item_type);

    }





    // Describe charges left for non-rods

    if (tval != TV_ROD) {

        // Describe the charges in the pack

        if (item >= 0) {

            inven_item_charges(item);

        }



        // Describe the charges on the floor

        else {

            floor_item_charges(i_ptr);

        }

    }

}



/*

 * Command hook for zapping.

 */

void do_cmd_zap(void)

{

    int item, sval, tval;

    CItem *i_ptr;



    // Select only items that can be zapped

    item_tester_hook = item_tester_hook_zap;



    // Get an item from the inventory

    if (!get_item(&item, "Zap which item? ", GI_INVEN)) {

        if (item == -2) msg_print("You have nothing to zap.");

        return;

    }



    // Get the item (in the pack)

    i_ptr = gi_i_ptr;



    // Extract the item's tval, sval, pval, and object level

    tval = i_ptr->GetTval();

    sval = i_ptr->GetSval();



    // Set current spell type

    if (tval == TV_ROD) current_spell_type = 4;

    else if (tval == TV_STAFF) current_spell_type = 5;

    else if (tval == TV_WAND) current_spell_type = 6;

    else {

        msg_print("Error: incorrect tval in do_cmd_zap()");

        current_spell_type = 0;

        return;

    }



    // Set current spell

    current_spell = sval;

}









/*

 * Hook to determine if an object is activatable

 */

static bool item_tester_hook_activate(CItem *i_ptr)

{

    u32b f1, f2, f3;



    // Not known

    if (!i_ptr->isKnown()) return FALSE;



    // Extract the flags

    i_ptr->GetFlags(&f1, &f2, &f3);



    // Check activation flag

    if (f3 & TR3_ACTIVATE) return TRUE;



    // Assume not

    return FALSE;

}







/*

 * Hack -- activate the ring of power

 */

static void ring_of_power(int tx, int ty)

{

    // Pick a random effect

    switch (randint(10)) {

        case 1:

        case 2:

            // Message

            msg_print("You are surrounded by a malignant aura.");



            // Decrease all stats (permanently)

            dec_stat(STAT_STR, 50, TRUE);

            dec_stat(STAT_INT, 50, TRUE);

            dec_stat(STAT_WIS, 50, TRUE);

            dec_stat(STAT_DEX, 50, TRUE);

            dec_stat(STAT_CON, 50, TRUE);

            dec_stat(STAT_CHR, 50, TRUE);



            // Lose some experience (permanently)

            p_ptr->SetExp(p_ptr->GetExp() - p_ptr->GetExp()/4);

            p_ptr->SetMaxExp(p_ptr->GetMaxExp() - p_ptr->GetMaxExp()/4);

            p_ptr->check_experience();



            break;



        // Dispel monsters

        case 3:

            msg_print("You are surrounded by a powerful aura.");

            dispel_monsters(1000);

            break;



        // Mana Ball

        case 4:

        case 5:

        case 6:

            fire_ball(GF_MANA, tx, ty, 300, 3);

            break;



        // Mana Bolt

        default:

            fire_bolt(GF_MANA, tx, ty, 250);

            break;

     }

}





/*

 * Activate a wielded object.  Wielded objects never stack.

 * And even if they did, activatable objects never stack.

 *

 * Currently, only (some) artifacts, and Dragon Scale Mail, can be activated.

 * But one could, for example, easily make an activatable "Ring of Plasma".

 *

 * Note that it always takes a turn to activate an artifact, even if

 * the user hits "escape" at the "direction" prompt.

 */

void do_activate(int item, int tx, int ty)

{

    int i, k, a, lev, chance;

    CItem *i_ptr;



    if (p_ptr->isBusy()) return;



    // Get the item

    i_ptr = &inventory[item];



    // Does it still exist?

    if (!i_ptr->exists()) {

        msg_print("You cannot activate something you are no longer using!");

        return;

    }



    // Can the current item be activated?

    if (!item_tester_hook_activate(i_ptr)) {

        msg_print("You are no longer using something that can be activated!");

        return;

    }





    // Take a turn

    p_ptr->DrainEnergy(100);



    // Extract the item level

    lev = i_ptr->GetObjLevel();



    // Hack -- use artifact level instead

    if (i_ptr->isArtifact()) lev = (i_ptr->get_a_ptr())->level;



    /* Roll for usage */

    if (!use_magic(lev)) {

        msg_print("You failed to activate it properly.");

        return;

    }



    /* Check the recharge */

    if (i_ptr->GetTimeout()) {

        msg_print("It whines, glows and fades...");

        return;

    }





    // Wonder Twin Powers... Activate! 

    msg_print("You activate it...");





    // Artifacts activate by name

    if (i_ptr->isArtifact()) {

        // This needs to be changed

        switch (i_ptr->GetName1()) {

            case ART_NARTHANC:

                msg_print("Your dagger is covered in fire...");

                fire_bolt(GF_FIRE, tx, ty, damroll(9, 8));

                i_ptr->SetTimeout(rand_int(8) + 8);

                break;



            case ART_NIMTHANC:

                msg_print("Your dagger is covered in frost...");

                fire_bolt(GF_COLD, tx, ty, damroll(6, 8));

                i_ptr->SetTimeout(rand_int(7) + 7);

                break;



            case ART_DETHANC:

                msg_print("Your dagger is covered in sparks...");

                fire_bolt(GF_ELEC, tx, ty, damroll(4, 8));

                i_ptr->SetTimeout(rand_int(6) + 6);

                break;



            case ART_RILIA:

                msg_print("Your dagger throbs deep green...");

                fire_ball(GF_POIS, tx, ty, 12, 3);

                i_ptr->SetTimeout(rand_int(4) + 4);

                break;



            case ART_BELANGIL:

                msg_print("Your dagger is covered in frost...");

                fire_ball(GF_COLD, tx, ty, 48, 2);

                i_ptr->SetTimeout(rand_int(5) + 5);

                break;



            case ART_DAL:

                msg_print("You feel energy flow through your feet...");

                p_ptr->mod_afraid(0);

                p_ptr->mod_poisoned(0);

                i_ptr->SetTimeout(5);

                break;



            case ART_RINGIL:

                msg_print("Your sword glows an intense blue...");

                fire_ball(GF_COLD, tx, ty, 100, 2);

                i_ptr->SetTimeout(300);

                break;



            case ART_ANDURIL:

                msg_print("Your sword glows an intense red...");

                fire_ball(GF_FIRE, tx, ty, 72, 2);

                i_ptr->SetTimeout(400);

                break;



            case ART_FIRESTAR:

                msg_print("Your morningstar rages in fire...");

                fire_ball(GF_FIRE, tx, ty, 72, 3);

                i_ptr->SetTimeout(100);

                break;



            case ART_FEANOR:

                if (!p_ptr->GetFast()) {

                    p_ptr->mod_fast(randint(20) + 20);

                }

                else {

                    p_ptr->mod_fast(p_ptr->GetFast() + 5);

                }

                i_ptr->SetTimeout(200);

                break;



            case ART_THEODEN:

                msg_print("The blade of your axe glows black...");

                drain_life(tx, ty, 120);

                i_ptr->SetTimeout(400);

                break;



            case ART_TURMIL:

                msg_print("The head of your hammer glows white...");

                drain_life(tx, ty, 90);

                i_ptr->SetTimeout(70);

                break;



            case ART_CASPANION:

                msg_print("Your armor glows bright red...");

                destroy_doors_touch();

                i_ptr->SetTimeout(10);

                break;



            case ART_AVAVIR:

                if (!p_ptr->GetWordRecall()) {

                    p_ptr->SetWordRecall(randint(20) + 15);

                    msg_print("The air about you becomes charged...");

                }

                else {

                    p_ptr->SetWordRecall(0);

                    msg_print("A tension leaves the air around you...");

                }

                i_ptr->SetTimeout(200);

                break;



            case ART_TARATOL:

                if (!p_ptr->GetFast()) {

                    p_ptr->mod_fast(randint(20) + 20);

                }

                else {

                    p_ptr->mod_fast(p_ptr->GetFast() + 5);

                }

                i_ptr->SetTimeout(rand_int(100) + 100);

                break;



            case ART_ERIRIL:

                /* Identify and combine pack */

                ident_spell();

                /* XXX Note that the artifact is always de-charged */

                i_ptr->SetTimeout(10);

                break;



            case ART_OLORIN:

                probing();

                i_ptr->SetTimeout(20);

                break;



            case ART_EONWE:

                msg_print("Your axe lets out a long, shrill note...");

                mass_genocide();

                i_ptr->SetTimeout(1000);

                break;



            case ART_LOTHARANG:

                msg_print("Your battle axe radiates deep purple...");

                p_ptr->heal_up(damroll(4, 8));

                p_ptr->mod_cut(p_ptr->GetCut()/2 - 50);

                i_ptr->SetTimeout(rand_int(3) + 3);

                break;



            case ART_CUBRAGOL:

                /* Use the first (XXX) acceptable bolts */

                for (a = 0; a < INVEN_PACK; a++) {

                    CItem *j_ptr = &inventory[a];

                    if ((j_ptr->GetTval() == TV_BOLT) &&

                        !j_ptr->isArtifact() && !j_ptr->isEgoItem() &&

                        !j_ptr->isCursed() && !j_ptr->isBroken()) break;

                }



                /* Enchant the bolts (or fail) */

                msg_print("Your bolts are covered in a fiery aura!");

                if ((a < INVEN_PACK) && percent(25)) {

                    CItem *j_ptr = &inventory[a];

                    j_ptr->SetName2(EGO_FLAME);

                    enchant(j_ptr, rand_int(3) + 4, ENCH_TOHIT | ENCH_TODAM);

                }

                else {

                    msg_print("The enchantment failed.");

                }



                i_ptr->SetTimeout(999);

                break;



            case ART_ARANRUTH:

                msg_print("Your sword glows a pale blue...");

                fire_bolt(GF_COLD, tx, ty, damroll(12, 8));

                i_ptr->SetTimeout(500);

                break;



            case ART_AEGLOS:

                msg_print("Your spear glows a bright white...");

                fire_ball(GF_COLD, tx, ty, 100, 2);

                i_ptr->SetTimeout(500);

                break;



            case ART_OROME:

                msg_print("Your spear pulsates...");

                wall_to_mud(tx, ty);

                i_ptr->SetTimeout(5);

                break;



            case ART_SOULKEEPER:

                msg_print("Your armor glows a bright white...");

                msg_print("You feel much better...");

                p_ptr->heal_up(1000);

                p_ptr->mod_cut(0);

                i_ptr->SetTimeout(888);

                break;



            case ART_BELEGENNON:

                teleport_player(10);

                i_ptr->SetTimeout(2);

                break;



            case ART_CELEBORN:

                genocide();

                i_ptr->SetTimeout(500);

                break;



            case ART_LUTHIEN:

                p_ptr->restore_level();

                i_ptr->SetTimeout(450);

                break;



            case ART_ULMO:

                msg_print("Your trident glows deep red...");

                teleport_monster(tx, ty);

                i_ptr->SetTimeout(150);

                break;



            case ART_COLLUIN:

                msg_print("Your cloak glows many colors...");

                p_ptr->mod_oppose_acid(p_ptr->GetOpposeAcid() + randint(20) + 20);

                p_ptr->mod_oppose_elec(p_ptr->GetOpposeElec() + randint(20) + 20);

                p_ptr->mod_oppose_fire(p_ptr->GetOpposeFire() + randint(20) + 20);

                p_ptr->mod_oppose_cold(p_ptr->GetOpposeCold() + randint(20) + 20);

                p_ptr->mod_oppose_pois(p_ptr->GetOpposePois() + randint(20) + 20);

                i_ptr->SetTimeout(111);

                break;



            case ART_HOLCOLLETH:

                msg_print("Your cloak glows deep blue...");

                sleep_monsters_touch();

                i_ptr->SetTimeout(55);

                break;



            case ART_THINGOL:

                msg_print("You hear a low humming noise...");

                recharge(60);

                i_ptr->SetTimeout(70);

                break;



            case ART_COLANNON:

                teleport_player(100);

                i_ptr->SetTimeout(45);

                break;



            case ART_TOTILA:

                msg_print("Your flail glows in scintillating colours...");

                confuse_monster(tx, ty, 20);

                i_ptr->SetTimeout(15);

                break;



            case ART_CAMMITHRIM:

                msg_print("Your gloves glow extremely brightly...");

                fire_bolt(GF_MISSILE, tx, ty, damroll(3, 4));

                i_ptr->SetTimeout(2);

                break;



            case ART_PAURHACH:

                msg_print("Your gauntlets are covered in fire...");

                fire_bolt(GF_FIRE, tx, ty, damroll(9,8));

                i_ptr->SetTimeout(rand_int(8) + 8);

                break;



            case ART_PAURNIMMEN:

                msg_print("Your gauntlets are covered in frost...");

                fire_bolt(GF_COLD, tx, ty, damroll(6, 8));

                i_ptr->SetTimeout(rand_int(7) + 7);

                break;



            case ART_PAURAEGEN:

                msg_print("Your gauntlets are covered in sparks...");

                fire_bolt(GF_ELEC, tx, ty, damroll(4, 8));

                i_ptr->SetTimeout(rand_int(6) + 6);

                break;



            case ART_PAURNEN:

                msg_print("Your gauntlets look very acidic...");

                fire_bolt(GF_ACID, tx, ty, damroll(5, 8));

                i_ptr->SetTimeout(rand_int(5) + 5);

                break;



            case ART_FINGOLFIN:

                msg_print("Magical spikes appear on your cesti...");

                fire_bolt(GF_ARROW, tx, ty, 150);

                i_ptr->SetTimeout(rand_int(90) + 90);

                break;



            case ART_HOLHENNETH:

                msg_print("An image forms in your mind...");

                detection();

                i_ptr->SetTimeout(rand_int(55) + 55);

                break;



            case ART_GONDOR:

                msg_print("You feel a warm tingling inside...");

                p_ptr->heal_up(500);

                p_ptr->mod_cut(0);

                i_ptr->SetTimeout(500);

                break;



            case ART_RAZORBACK:

                msg_print("You are surrounded by lightning!");

                for (i = 0; i < 8; i++) {

                    fire_ball(GF_ELEC,

                        p_ptr->GetX()+ddx[ddd[i]],

                        p_ptr->GetY()+ddy[ddd[i]], 150, 3);

                }

                i_ptr->SetTimeout(1000);

                break;



            case ART_BLADETURNER:

                msg_print("Your armor glows many colours...");

                p_ptr->heal_up(30);

                p_ptr->mod_afraid(0);

                p_ptr->mod_shero(p_ptr->GetSHero() + randint(50) + 50);

                p_ptr->mod_blessed(p_ptr->GetBlessed() + randint(50) + 50);

                p_ptr->mod_oppose_acid(p_ptr->GetOpposeAcid() + randint(50) + 50);

                p_ptr->mod_oppose_elec(p_ptr->GetOpposeElec() + randint(50) + 50);

                p_ptr->mod_oppose_fire(p_ptr->GetOpposeFire() + randint(50) + 50);

                p_ptr->mod_oppose_cold(p_ptr->GetOpposeCold() + randint(50) + 50);

                p_ptr->mod_oppose_pois(p_ptr->GetOpposePois() + randint(50) + 50);

                i_ptr->SetTimeout(400);

                break;





            case ART_GALADRIEL:

                msg_print("The phial wells with clear light...");

                lite_area(damroll(2, 15), 3);

                i_ptr->SetTimeout(rand_int(10) + 10);

                break;



            case ART_ELENDIL:

                msg_print("The star shines brightly...");

                map_area();

                i_ptr->SetTimeout(rand_int(50) + 50);

                break;



            case ART_THRAIN:

                msg_print("The stone glows a deep green...");

                wiz_lite();

                detect_sdoor();

                detect_trap();

                i_ptr->SetTimeout(rand_int(100) + 100);

                break;





            case ART_INGWE:

                msg_print("An aura of good floods the area...");

                dispel_evil(p_ptr->GetLev()*5);

                i_ptr->SetTimeout(rand_int(300) + 300);

                break;



            case ART_CARLAMMAS:

                msg_print("The amulet lets out a shrill wail...");

                k = 3 * p_ptr->GetLev();

                p_ptr->mod_protevil(p_ptr->GetProtevil() + randint(25) + k);

                i_ptr->SetTimeout(rand_int(225) + 225);

                break;





            case ART_TULKAS:

                msg_print("The ring glows brightly...");

                if (!p_ptr->GetFast()) {

                    p_ptr->mod_fast(randint(75) + 75);

                }

                else {

                    p_ptr->mod_fast(p_ptr->GetFast() + 5);

                }

                i_ptr->SetTimeout(rand_int(150) + 150);

                break;



            case ART_NARYA:

                msg_print("The ring glows deep red...");

                fire_ball(GF_FIRE, tx, ty, 120, 3);

                i_ptr->SetTimeout(rand_int(225) + 225);

                break;



            case ART_NENYA:

                msg_print("The ring glows bright white...");

                fire_ball(GF_COLD, tx, ty, 200, 3);

                i_ptr->SetTimeout(rand_int(325) + 325);

                break;



            case ART_VILYA:

                msg_print("The ring glows deep blue...");

                fire_ball(GF_ELEC, tx, ty, 250, 3);

                i_ptr->SetTimeout(rand_int(425) + 425);

                break;



            case ART_POWER:

                msg_print("The ring glows intensely black...");

                ring_of_power(tx, ty);

                i_ptr->SetTimeout(rand_int(450) + 450);

                break;





            default:

                quit("Oops.  Non-Activatable Artifact.");

        }



        /* Done */

        return;

    }





    /* Hack -- Dragon Scale Mail can be activated as well */

    if (i_ptr->GetTval() == TV_DRAG_ARMOR) {

        // Branch on the sub-type

        switch (i_ptr->GetSval()) {

            case SV_DRAGON_BLUE:

                msg_print("You breathe lightning.");

                fire_ball(GF_ELEC, tx, ty, 100, 2);

                i_ptr->SetTimeout(rand_int(450) + 450);

                break;



            case SV_DRAGON_WHITE:

                msg_print("You breathe frost.");

                fire_ball(GF_COLD, tx, ty, 110, 2);

                i_ptr->SetTimeout(rand_int(450) + 450);

                break;



            case SV_DRAGON_BLACK:

                msg_print("You breathe acid.");

                fire_ball(GF_ACID, tx, ty, 130, 2);

                i_ptr->SetTimeout(rand_int(450) + 450);

                break;



            case SV_DRAGON_GREEN:

                msg_print("You breathe poison gas.");

                fire_ball(GF_POIS, tx, ty, 150, 2);

                i_ptr->SetTimeout(rand_int(450) + 450);

                break;



            case SV_DRAGON_RED:

                msg_print("You breathe fire.");

                fire_ball(GF_FIRE, tx, ty, 200, 2);

                i_ptr->SetTimeout(rand_int(450) + 450);

                break;



            case SV_DRAGON_MULTIHUED:

                chance = rand_int(5);

                msg_format("You breathe %s.",

                           ((chance == 1) ? "lightning" :

                            ((chance == 2) ? "frost" :

                             ((chance == 3) ? "acid" :

                              ((chance == 4) ? "poison gas" : "fire")))));

                fire_ball(((chance == 1) ? GF_ELEC :

                           ((chance == 2) ? GF_COLD :

                            ((chance == 3) ? GF_ACID :

                             ((chance == 4) ? GF_POIS : GF_FIRE)))),

                          tx, ty, 250, 2);

                i_ptr->SetTimeout(rand_int(225) + 225);

                break;



            case SV_DRAGON_BRONZE:

                msg_print("You breathe confusion.");

                fire_ball(GF_CONFUSION, tx, ty, 120, 2);

                i_ptr->SetTimeout(rand_int(450) + 450);

                break;



            case SV_DRAGON_GOLD:

                msg_print("You breathe sound.");

                fire_ball(GF_SOUND, tx, ty, 130, 2);

                i_ptr->SetTimeout(rand_int(450) + 450);

                break;



            case SV_DRAGON_CHAOS:

                chance = rand_int(2);

                msg_format("You breathe %s.",

                           ((chance == 1 ? "chaos" : "disenchantment")));

                fire_ball((chance == 1 ? GF_CHAOS : GF_DISENCHANT),

                          tx, ty, 220, 2);

                i_ptr->SetTimeout(rand_int(300) + 300);

                break;



            case SV_DRAGON_LAW:

                chance = rand_int(2);

                msg_format("You breathe %s.",

                           ((chance == 1 ? "sound" : "shards")));

                fire_ball((chance == 1 ? GF_SOUND : GF_SHARDS),

                          tx, ty, 230, 2);

                i_ptr->SetTimeout(rand_int(300) + 300);

                break;



            case SV_DRAGON_BALANCE:

                chance = rand_int(4);

                msg_format("You breathe %s.",

                           ((chance == 1) ? "chaos" :

                            ((chance == 2) ? "disenchantment" :

                             ((chance == 3) ? "sound" : "shards"))));

                fire_ball(((chance == 1) ? GF_CHAOS :

                           ((chance == 2) ? GF_DISENCHANT :

                            ((chance == 3) ? GF_SOUND : GF_SHARDS))),

                          tx, ty, 250, 2);

                i_ptr->SetTimeout(rand_int(300) + 300);

                break;



            case SV_DRAGON_SHINING:

                chance = rand_int(2);

                msg_format("You breathe %s.",

                           ((chance == 0 ? "light" : "darkness")));

                fire_ball((chance == 0 ? GF_LITE : GF_DARK), tx, ty, 200, 2);

                i_ptr->SetTimeout(rand_int(300) + 300);

                break;



            case SV_DRAGON_POWER:

                msg_print("You breathe the elements.");

                fire_ball(GF_MISSILE, tx, ty, 300, 2);

                i_ptr->SetTimeout(rand_int(300) + 300);

                break;



            default:

                quit("Oops.  You have bad breath.");

        }



        /* Success */

        return;

    }





    /* Mistake */

    quit("Oops.  That object cannot be activated.");

}



void do_cmd_activate(void)

{

    int item;



    // Prepare the hook 

    item_tester_hook = item_tester_hook_activate;



    // Get an item (from equip)

    if (!get_item(&item, "Activate which item? ", GI_EQUIP)) {

        if (item == -2) msg_print("You have nothing to activate.");
        
        reset_timer(); //pause game while in screen
        return;

    }



    // Set the spell type and number

    current_spell_type = 3;

    current_spell = item;

    reset_timer(); //pause game while in screen
}



