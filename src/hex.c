#include "angband.h"

/* Flag list */
/*
plr-magic_num1
0: Flag bits of spelling spells
1: Flag bits of despelled spells
2: Revange damage
plr->magic_num2
0: Number of spelling spells
1: Type of revenge
2: Turn count for revenge
*/

#define MAX_KEEP 4

/* Hex */
static bool item_tester_hook_weapon_except_bow(object_type *o_ptr)
{
    switch (o_ptr->tval)
    {
        case TV_SWORD:
        case TV_HAFTED:
        case TV_POLEARM:
        case TV_DIGGING:
        {
            return (TRUE);
        }
    }

    return (FALSE);
}

cptr do_hex_spell(int spell, int mode)
{
    bool name = (mode == SPELL_NAME) ? TRUE : FALSE;
    bool desc = (mode == SPELL_DESC) ? TRUE : FALSE;
    bool info = (mode == SPELL_INFO) ? TRUE : FALSE;
    bool cast = (mode == SPELL_CAST) ? TRUE : FALSE;
    bool cont = (mode == SPELL_CONT) ? TRUE : FALSE;
    bool stop = (mode == SPELL_STOP) ? TRUE : FALSE;

    bool add = TRUE;

    int plev = plr->lev;
    int power;

    switch (spell)
    {
    /*** 1st book (0-7) ***/
    case 0:
        if (name) return "Evily blessing";
        if (desc) return "Attempts to increase +to_hit of a weapon and AC";
        if (cast) plr_tim_lock(T_BLESSED);
        if (stop) plr_tim_unlock(T_BLESSED);
        break;

    case 1:
        if (name) return "Cure light wounds";
        if (desc) return "Heals cut and HP a little.";
        if (info) return info_heal(1, 10, 0);
        if (cast)
        {
            msg_print("You feel better and better.");
        }
        if (cast || cont)
        {
            hp_player(damroll(1, 10));
            plr_tim_subtract(T_CUT, 10);
        }
        break;

    case 2:
        if (name) return "Demonic aura";
        if (desc) return "Gives fire aura and regeneration.";
        if (cast)
        {
            msg_print("You have enveloped by fiery aura!");
        }
        if (stop)
        {
            msg_print("Fiery aura disappeared.");
        }
        break;

    case 3:
        if (name) return "Stinking mist";
        if (desc) return "Deals few damages of poison to all monsters in your sight.";
        power = plev / 2 + 5 + plr->to_d_spell;
        if (info) return info_damage(1, power, 0);
        if (cast || cont)
        {
            plr_project_los(GF_POIS, randint1(power));
        }
        break;

    case 4:
        if (name) return "Extra might";
        if (desc) return "Attempts to increase your strength.";
        if (cast)
        {
            msg_print("You feel you get stronger.");
        }
        break;

    case 5:
        if (name) return "Curse weapon";
        if (desc) return "Curses your weapon.";
        if (cast)
        {
            obj_prompt_t prompt = {0};
            char o_name[MAX_NLEN];

            prompt.prompt = "Which weapon do you curse?";
            prompt.error = "You wield no weapons.";
            prompt.filter = item_tester_hook_weapon_except_bow;
            prompt.where[0] = INV_EQUIP;

            obj_prompt(&prompt);
            if (!prompt.obj) return FALSE;

            object_desc(o_name, prompt.obj, OD_NAME_ONLY);

            if (!get_check(format("Do you curse %s, really?", o_name))) return FALSE;

            if (!one_in_(3) &&
                (obj_is_art(prompt.obj) || obj_has_flag(prompt.obj, OF_BLESSED)))
            {
                msg_format("%s resists the effect.", o_name);
                if (one_in_(3))
                {
                    if (prompt.obj->to_d > 0)
                    {
                        prompt.obj->to_d -= randint1(3) % 2;
                        if (prompt.obj->to_d < 0) prompt.obj->to_d = 0;
                    }
                    if (prompt.obj->to_h > 0)
                    {
                        prompt.obj->to_h -= randint1(3) % 2;
                        if (prompt.obj->to_h < 0) prompt.obj->to_h = 0;
                    }
                    if (prompt.obj->to_a > 0)
                    {
                        prompt.obj->to_a -= randint1(3) % 2;
                        if (prompt.obj->to_a < 0) prompt.obj->to_a = 0;
                    }
                    msg_format("Your %s was disenchanted!", o_name);
                }
            }
            else
            {
                int power = 0;
                msg_format("A terrible black aura blasts your %s!", o_name);
                prompt.obj->curse_flags |= (OFC_CURSED);

                if (obj_is_art(prompt.obj) || obj_is_ego(prompt.obj))
                {

                    if (one_in_(3)) prompt.obj->curse_flags |= (OFC_HEAVY_CURSE);
                    if (one_in_(666))
                    {
                        prompt.obj->curse_flags |= (OFC_TY_CURSE);
                        if (one_in_(666)) prompt.obj->curse_flags |= (OFC_PERMA_CURSE);

                        add_flag(prompt.obj->flags, OF_AGGRAVATE);
                        add_flag(prompt.obj->flags, OF_VORPAL);
                        add_flag(prompt.obj->flags, OF_BRAND_VAMP);
                        msg_print("Blood, Blood, Blood!");
                        power = 2;
                    }
                }

                prompt.obj->curse_flags |= get_curse(power, prompt.obj);
            }

            plr->update |= (PU_BONUS);
            add = FALSE;
        }
        break;

    case 6:
        if (name) return "Evil detection";
        if (desc) return "Detects evil monsters.";
        if (info) return info_range(MAX_SIGHT);
        if (cast)
        {
            msg_print("You attend to the presence of evil creatures.");
        }
        break;

    case 7:
        if (name) return "Patience";
        if (desc) return "Bursts hell fire strongly after patients any damage while few turns.";
        power = MIN(100, plr->magic_num1[2] + plr->to_d_spell);
        if (info) return info_damage(0, 0, power);
        if (cast)
        {
            int a = 3 - (plr->pspeed + 10) / 10;  /* XXX */
            int r = 3 + randint1(3) + MAX(0, MIN(3, a));

            if (plr->magic_num2[2] > 0)
            {
                msg_print("You are already patienting.");
                return NULL;
            }

            plr->magic_num2[1] = 1;
            plr->magic_num2[2] = r;
            plr->magic_num1[2] = 0;
            msg_print("You decide to patient all damages.");
            add = FALSE;
        }
        if (cont)
        {
            int rad = 2 + (power / 50);

            plr->magic_num2[2]--;

            if ((plr->magic_num2[2] <= 0) || (power >= 200))
            {
                msg_print("Time for end of patience!");
                if (power)
                    plr_burst(rad, GF_HELL_FIRE, power);
                if (plr->wizard)
                    msg_format("You return %d damages.", power);

                /* Reset */
                plr->magic_num2[1] = 0;
                plr->magic_num2[2] = 0;
                plr->magic_num1[2] = 0;
            }
        }
        break;

    /*** 2nd book (8-15) ***/
    case 8:
        if (name) return "Ice armor";
        if (desc) return "Gives cold aura and bonus to AC.";
        if (cast)
        {
            msg_print("You have enveloped by ice armor!");
        }
        if (stop)
        {
            msg_print("Ice armor disappeared.");
        }
        break;

    case 9:
        if (name) return "Cure serious wounds";
        if (desc) return "Heals cut and HP more.";
        if (info) return info_heal(2, 10, 0);
        if (cast)
        {
            msg_print("You feel better and better.");
        }
        if (cast || cont)
        {
            hp_player(damroll(2, 10));
            plr_tim_recover(T_CUT, 50, 0);
            plr_tim_subtract(T_CUT, 10);
        }
        break;

    case 10:
        if (name) return "Inhail potion";
        if (desc) return "Quaffs a potion without canceling of casting a spell.";
        if (cast)
        {
            plr->magic_num1[0] |= (1L << HEX_INHAIL);
            do_cmd_quaff_potion();
            plr->magic_num1[0] &= ~(1L << HEX_INHAIL);
            add = FALSE;
        }
        break;

    case 11:
        if (name) return "Vampiric mist";
        if (desc) return "Deals few damages of drain life to all monsters in your sight.";
        power = (plev / 2) + 5 + plr->to_d_spell;
        if (info) return info_damage(1, power, 0);
        if (cast || cont)
        {
            plr_project_los(GF_OLD_DRAIN, randint1(power));
        }
        break;

    case 12:
        if (name) return "Swords to runeswords";
        if (desc) return "Gives vorpal ability to your weapon. Increases damages by your weapon according to curse of your weapon.";
        if (cast)
        {
            if (plr->weapon_ct > 1)
                msg_print("Your weapons glow bright black.");
            else
                msg_print("Your weapon glows bright black.");
        }
        if (stop)
            msg_format("Brightness of weapon%s disappeared.", (plr->weapon_ct <= 1) ? "" : "s");
        break;

    case 13:
        if (name) return "Touch of confusion";
        if (desc) return "Confuses a monster when you attack.";
        if (cast)
        {
            msg_print("Your hands glow bright red.");
        }
        if (stop)
        {
            msg_print("Brightness on your hands disappeard.");
        }
        break;

    case 14:
        if (name) return "Building up";
        if (desc) return "Attempts to increases your strength, dexterity and constitution.";
        if (cast)
        {
            msg_print("You feel your body is developed more now.");
        }
        break;

    case 15:
        if (name) return "Anti teleport barrier";
        if (desc) return "Obstructs all teleportations by monsters in your sight.";
        power = plev * 3 / 2;
        if (info) return info_power(power);
        if (cast)
        {
            msg_print("You feel anyone can not teleport except you.");
        }
        break;

    /*** 3rd book (16-23) ***/
    case 16:
        if (name) return "Cloak of shock";
        if (desc) return "Gives lightning aura and a bonus to speed.";
        if (cast)
        {
            msg_print("You have enveloped by electrical aura!");
        }
        if (stop)
        {
            msg_print("Electrical aura disappeared.");
        }
        break;

    case 17:
        if (name) return "Cure critical wounds";
        if (desc) return "Heals cut and HP greatly.";
        if (info) return info_heal(4, 10, 0);
        if (cast)
        {
            msg_print("You feel better and better.");
        }
        if (cast || cont)
        {
            hp_player(damroll(4, 10));
            plr_tim_remove(T_STUN);
            plr_tim_remove(T_CUT);
        }
        break;

    case 18:
        if (name) return "Recharging";
        if (desc)
        {
            return "It attempts to recharge a device using your mana for power.";
        }

        power = plev * 2;
        if (info) return info_power(power);
        if (cast)
        {
            if (!recharge_from_player(power)) return NULL;
            add = FALSE;
        }
        break;

    case 19:
        if (name) return "Animate Dead";
        if (desc) return "Raises corpses and skeletons from dead.";
        if (cast)
        {
            msg_print("You start to call deads.!");
        }
        if (cast || cont) plr_animate_dead();
        break;

    case 20:
        if (name) return "Curse armor";
        if (desc) return "Curse a piece of armour that you wielding.";
        if (cast)
        {
            obj_prompt_t prompt = {0};
            char o_name[MAX_NLEN];

            prompt.prompt = "Which piece of armour do you curse?";
            prompt.error = "You wield no piece of armours.";
            prompt.filter = obj_is_armor;
            prompt.where[0] = INV_EQUIP;

            obj_prompt(&prompt);
            if (!prompt.obj) return FALSE;

            object_desc(o_name, prompt.obj, OD_NAME_ONLY);

            if (!get_check(format("Do you curse %s, really?", o_name))) return FALSE;

            if (!one_in_(3) &&
                (obj_is_art(prompt.obj) || obj_has_flag(prompt.obj, OF_BLESSED)))
            {
                msg_format("%s resists the effect.", o_name);
                if (one_in_(3))
                {
                    if (prompt.obj->to_d > 0)
                    {
                        prompt.obj->to_d -= randint1(3) % 2;
                        if (prompt.obj->to_d < 0) prompt.obj->to_d = 0;
                    }
                    if (prompt.obj->to_h > 0)
                    {
                        prompt.obj->to_h -= randint1(3) % 2;
                        if (prompt.obj->to_h < 0) prompt.obj->to_h = 0;
                    }
                    if (prompt.obj->to_a > 0)
                    {
                        prompt.obj->to_a -= randint1(3) % 2;
                        if (prompt.obj->to_a < 0) prompt.obj->to_a = 0;
                    }
                    msg_format("Your %s was disenchanted!", o_name);
                }
            }
            else
            {
                int power = 0;
                msg_format("A terrible black aura blasts your %s!", o_name);
                prompt.obj->curse_flags |= (OFC_CURSED);

                if (obj_is_art(prompt.obj) || obj_is_ego(prompt.obj))
                {

                    if (one_in_(3)) prompt.obj->curse_flags |= (OFC_HEAVY_CURSE);
                    if (one_in_(666))
                    {
                        prompt.obj->curse_flags |= (OFC_TY_CURSE);
                        if (one_in_(666)) prompt.obj->curse_flags |= (OFC_PERMA_CURSE);

                        add_flag(prompt.obj->flags, OF_AGGRAVATE);
                        add_flag(prompt.obj->flags, OF_RES_(GF_POIS));
                        add_flag(prompt.obj->flags, OF_RES_(GF_DARK));
                        add_flag(prompt.obj->flags, OF_RES_(GF_NETHER));
                        msg_print("Blood, Blood, Blood!");
                        power = 2;
                    }
                }

                prompt.obj->curse_flags |= get_curse(power, prompt.obj);
            }

            plr->update |= (PU_BONUS);
            add = FALSE;
        }
        break;

    case 21:
        if (name) return "Cloak of shadow";
        if (desc) return "Gives aura of shadow.";
        if (cast)
        {
            int slot = equip_find_first(obj_is_cloak);
            object_type *o_ptr = NULL;

            if (!slot)
            {
                msg_print("You are not wearing a cloak.");
                return NULL;
            }
            o_ptr = equip_obj(slot);
            if (!obj_is_cursed(o_ptr))
            {
                msg_print("Your cloak is not cursed.");
                return NULL;
            }
            else
            {
                msg_print("You have enveloped by shadow aura!");
            }
        }
        if (cont)
        {
            int slot = equip_find_first(obj_is_cloak);
            if (!slot || !obj_is_cursed(equip_obj(slot)))
            {
                do_spell(REALM_HEX, spell, SPELL_STOP);
                plr->magic_num1[0] &= ~(1L << spell);
                plr->magic_num2[0]--;
                if (!plr->magic_num2[0]) set_action(ACTION_NONE);
            }
        }
        if (stop)
        {
            msg_print("Shadow aura disappeared.");
        }
        break;

    case 22:
        if (name) return "Pains to mana";
        if (desc) return "Deals psychic damages to all monsters in sight, and drains some mana.";
        power = plev * 3 / 2 + plr->to_d_spell;
        if (info) return info_damage(1, power, 0);
        if (cast || cont)
        {
            plr_project_los(GF_PSI_DRAIN, randint1(power));
        }
        break;

    case 23:
        if (name) return "Eye for an eye";
        if (desc) return "Returns same damage which you got to the monster which damaged you.";
        if (cast)
        {
            msg_print("You wish strongly you want to revenge anything.");
            plr_tim_lock(T_REVENGE);
        }
        if (stop) plr_tim_unlock(T_REVENGE);
        break;

    /*** 4th book (24-31) ***/
    case 24:
        if (name) return "Anti multiply barrier";
        if (desc) return "Obstructs all multiplying by monsters in entire floor.";
        if (cast)
        {
            msg_print("You feel anyone can not already multiply.");
        }
        break;

    case 25:
        if (name) return "Restore life";
        if (desc) return "Restores life energy and status.";
        if (cast)
        {
            msg_print("You feel your life energy starting to return.");
        }
        if (cast || cont)
        {
            bool flag = FALSE;
            int d = (plr->max_exp - plr->exp);
            int r = (plr->exp / 20);
            int i;

            if (d > 0)
            {
                if (d < r)
                    plr->exp = plr->max_exp;
                else
                    plr->exp += r;

                /* Check the experience */
                check_experience();

                flag = TRUE;
            }
            for (i = A_STR; i < 6; i ++)
            {
                if (plr->stat_cur[i] < plr->stat_max[i])
                {
                    if (plr->stat_cur[i] < 18)
                        plr->stat_cur[i]++;
                    else
                        plr->stat_cur[i] += 10;

                    if (plr->stat_cur[i] > plr->stat_max[i])
                        plr->stat_cur[i] = plr->stat_max[i];

                    /* Recalculate bonuses */
                    plr->update |= (PU_BONUS);

                    flag = TRUE;
                }
            }

            if (!flag)
            {
                msg_format("Finish casting '%^s'.", do_spell(REALM_HEX, HEX_RESTORE, SPELL_NAME));
                plr->magic_num1[0] &= ~(1L << HEX_RESTORE);
                if (cont) plr->magic_num2[0]--;
                if (!plr->magic_num2[0]) set_action(ACTION_NONE);

                /* Redraw status */
                plr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);
                plr->redraw |= (PR_EXTRA);

                return "";
            }
        }
        break;

    case 26:
        if (name) return "Drain curse power";
        if (desc) return "Drains curse on your weapon and heals SP a little.";
        if (cast)
        {
            obj_prompt_t prompt = {0};

            prompt.prompt = "Which cursed equipment do you drain mana from?";
            prompt.error = "You have no cursed equipment.";
            prompt.filter = obj_is_cursed;
            prompt.where[0] = INV_EQUIP;

            obj_prompt(&prompt);
            if (!prompt.obj) return FALSE;

            plr->csp += (plev / 5) + randint1(plev / 5);
            if (obj_has_flag(prompt.obj, OF_TY_CURSE) || (prompt.obj->curse_flags & OFC_TY_CURSE)) plr->csp += randint1(5);
            if (plr->csp > plr->msp) plr->csp = plr->msp;

            if (prompt.obj->curse_flags & OFC_PERMA_CURSE)
            {
                /* Nothing */
            }
            else if (prompt.obj->curse_flags & OFC_HEAVY_CURSE)
            {
                if (one_in_(7))
                {
                    msg_print("Heavy curse vanished away.");
                    prompt.obj->curse_flags = 0L;
                }
            }
            else if ((prompt.obj->curse_flags & (OFC_CURSED)) && one_in_(3))
            {
                msg_print("Curse vanished away.");
                prompt.obj->curse_flags = 0L;
            }

            add = FALSE;
        }
        break;

    case 27:
        if (name) return "Swords to vampires";
        if (desc) return "Gives vampiric ability to your weapon.";
        if (cast)
        {
            if (plr->weapon_ct > 1)
                msg_print("Your weapons want more blood now.");
            else
                msg_print("Your weapon wants more blood now.");
        }
        if (stop)
            msg_format("Thirsty of weapon%s disappeared.", (plr->weapon_ct <= 1) ? "" : "s");
        break;

    case 28:
        if (name) return "Word of stun";
        if (desc) return "Stuns all monsters in your sight.";
        if (cast || cont)
        {
            plr_project_los(GF_STUN, 5 + plev/5);
        }
        break;

    case 29:
        if (name) return "Moving into shadow";
        if (desc) return "Teleports you close to a monster.";
        if (cast)
        {
            int i, dir;
            bool flag;
            point_t pos;

            for (i = 0; i < 3; i++)
            {
                int rng = plr->lev + 2;
                dun_cell_ptr cell;

                pos = target_pos(rng);
                if (!dun_pos_interior(cave, pos)) return FALSE;
                cell = dun_cell_at(cave, pos);

                flag = FALSE;

                for (dir = 0; dir < 8; dir++)
                {
                    point_t p = point_step(pos, ddd[dir]);
                    if (dir == 5) continue;
                    if(dun_mon_at(cave, p)) flag = TRUE;
                }

                if ( dun_mon_at(cave, pos)
                  || dun_plr_at(cave, pos)
                  || (cell->flags & CELL_VAULT)
                  || !cell_allow_plr(cell)
                  || point_fast_distance(plr->pos, pos) > rng )
                {
                    msg_print("Can not teleport to there.");
                    continue;
                }
                break;
            }

            if (flag && randint0(plev * plev / 2))
            {
                teleport_player_to(pos, 0L);
            }
            else
            {
                msg_print("Oops!");
                if (mut_present(MUT_ASTRAL_GUIDE))
                    energy_use /= 3;
                teleport_player(30, 0L);
            }

            add = FALSE;
        }
        break;

    case 30:
        if (name) return "Anti magic barrier";
        if (desc) return "Obstructs all magic spell of monsters in your sight.";
        power = plev * 3 / 2;
        if (info) return info_power(power);
        if (cast)
        {
            msg_print("You feel anyone can not cast spells except you.");
        }
        break;

    case 31:
        if (name) return "Revenge sentence";
        if (desc) return "Fires  a ball of hell fire to try revenging after few turns.";
        power = plr->magic_num1[2] + plr->to_d_spell;
        if (info) return info_damage(0, 0, power);
        if (cast)
        {
            int r;
            int a = 3 - (plr->pspeed + 10) / 10; /* XXX */
            r = 1 + randint1(2) + MAX(0, MIN(3, a));

            if (plr->magic_num2[2] > 0)
            {
                msg_print("You already pronounced your revenge.");
                return NULL;
            }

            plr->magic_num2[1] = 2;
            plr->magic_num2[2] = r;
            msg_format("You pronounce your revenge. %d turns left.", r);
            add = FALSE;
        }
        if (cont)
        {
            plr->magic_num2[2]--;

            if (plr->magic_num2[2] <= 0)
            {
                if (power)
                {
                    point_t pos;
                    for (;;)
                    {
                        msg_print("Time to revenge!");
                        pos = plr_get_ball_target(GF_HELL_FIRE);
                        if (dun_pos_interior(cave, pos)) break;
                    }
                    plr_ball(1, pos, GF_HELL_FIRE, power);
                    if (plr->wizard)
                        msg_format("You return %d damages.", power);
                }
                else
                {
                    msg_print("You are not in a mood for revenge.");
                }
                plr->magic_num1[2] = 0;
            }
        }
        break;
    }

    /* start casting */
    if ((cast) && (add))
    {
        /* add spell */
        plr->magic_num1[0] |= 1L << (spell);
        plr->magic_num2[0]++;

        if (plr->action != ACTION_SPELL) set_action(ACTION_SPELL);
    }

    /* Redraw status */
    if (!info)
    {
        plr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);
        plr->redraw |= (PR_EXTRA | PR_HP | PR_MANA);
    }

    return "";
}

bool stop_hex_spell_all(void)
{
    int i;

    for (i = 0; i < 32; i++)
    {
        if (hex_spelling(i)) do_spell(REALM_HEX, i, SPELL_STOP);
    }

    plr->magic_num1[0] = 0;
    plr->magic_num2[0] = 0;

    /* Print message */
    if (plr->action == ACTION_SPELL) set_action(ACTION_NONE);

    /* Redraw status */
    plr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);
    plr->redraw |= (PR_EXTRA | PR_HP | PR_MANA);

    return TRUE;
}


bool stop_hex_spell(void)
{
    int spell;
    char choice;
    char out_val[160];
    bool flag = FALSE;
    int y = 1;
    int x = 20;
    int sp[MAX_KEEP];

    if (!hex_spelling_any())
    {
        msg_print("You are casting no spell.");
        return FALSE;
    }

    /* Stop all spells */
    else if ((plr->magic_num2[0] == 1) || (plr->lev < 35))
    {
        return stop_hex_spell_all();
    }
    else
    {
        strnfmt(out_val, 78, "Which spell do you stop casting? (Spell %c-%c, 'l' to all, ESC)",
            I2A(0), I2A(plr->magic_num2[0] - 1));

        screen_save();

        while (!flag)
        {
            int n = 0;
            Term_erase(x, y, 255);
            prt("       ", y, x + 5);
            for (spell = 0; spell < 32; spell++)
            {
                if (hex_spelling(spell))
                {
                    Term_erase(x, y + n + 1, 255);
                    put_str(format("%c)  %s", I2A(n), do_spell(REALM_HEX, spell, SPELL_NAME)), y + n + 1, x + 2);
                    sp[n++] = spell;
                }
            }

            if (!get_com(out_val, &choice, TRUE)) break;
            if (isupper(choice)) choice = tolower(choice);

            if (choice == 'l')    /* All */
            {
                screen_load();
                return stop_hex_spell_all();
            }
            if ((choice < I2A(0)) || (choice > I2A(plr->magic_num2[0] - 1))) continue;
            flag = TRUE;
        }
    }

    screen_load();

    if (flag)
    {
        int n = sp[A2I(choice)];

        do_spell(REALM_HEX, n, SPELL_STOP);
        plr->magic_num1[0] &= ~(1L << n);
        plr->magic_num2[0]--;
    }

    /* Redraw status */
    plr->update |= (PU_BONUS | PU_HP | PU_MANA | PU_SPELLS);
    plr->redraw |= (PR_EXTRA | PR_HP | PR_MANA);

    return flag;
}


/* Upkeeping hex spells
   Called from dungeon.c */
void check_hex(void)
{
    magic_type *s_ptr = 0;
    int spell;
    s32b need_mana;
    u32b need_mana_frac;
    bool res = FALSE;

    /* Spells spelled by player */
    if (plr->realm1 != REALM_HEX) return;
    if (!plr->magic_num1[0] && !plr->magic_num1[1]) return;

    if (plr->magic_num1[1])
    {
        plr->magic_num1[0] = plr->magic_num1[1];
        plr->magic_num1[1] = 0;
        res = TRUE;
    }

    /* Stop all spells when anti-magic ability is given */
    if (plr->anti_magic)
    {
        stop_hex_spell_all();
        return;
    }

    need_mana = 0;
    for (spell = 0; spell < 32; spell++)
    {
        if (hex_spelling(spell))
        {
            s_ptr = &technic_info[REALM_HEX - MIN_TECHNIC][spell];
            need_mana += mod_need_mana(s_ptr->smana, spell, REALM_HEX);
        }
    }


    /* Culcurates final mana cost */
    need_mana_frac = 0;
    s64b_div(&need_mana, &need_mana_frac, 0, 3); /* Divide by 3 */
    need_mana += (plr->magic_num2[0] - 1);


    /* Not enough mana */
    if (s64b_cmp(plr->csp, plr->csp_frac, need_mana, need_mana_frac) < 0)
    {
        stop_hex_spell_all();
        return;
    }

    /* Enough mana */
    else
    {
        s64b_sub(&(plr->csp), &(plr->csp_frac), need_mana, need_mana_frac);

        plr->redraw |= PR_MANA;
        if (res)
        {
            msg_print("You restart spelling.");
            plr->action = ACTION_SPELL;

            /* Recalculate bonuses */
            plr->update |= (PU_BONUS | PU_HP);

            /* Redraw map and status bar */
            plr->redraw |= (PR_MAP | PR_STATUS | PR_STATE);

            /* Update monsters */
            plr->update |= (PU_MONSTERS);

            /* Window stuff */
            plr->window |= (PW_OVERHEAD | PW_DUNGEON);
        }
    }

    /* Gain experiences of spelling spells */
    for (spell = 0; spell < 32; spell++)
    {
        if (!hex_spelling(spell)) continue;

        if (plr->spell_exp[spell] < SPELL_EXP_BEGINNER)
            plr->spell_exp[spell] += 5;
        else if(plr->spell_exp[spell] < SPELL_EXP_SKILLED)
        { if (one_in_(2) && (cave->dun_lvl > 4) && ((cave->dun_lvl + 10) > plr->lev)) plr->spell_exp[spell] += 1; }
        else if(plr->spell_exp[spell] < SPELL_EXP_EXPERT)
        { if (one_in_(5) && ((cave->dun_lvl + 5) > plr->lev) && ((cave->dun_lvl + 5) > s_ptr->slevel)) plr->spell_exp[spell] += 1; }
        else if(plr->spell_exp[spell] < SPELL_EXP_MASTER)
        { if (one_in_(5) && ((cave->dun_lvl + 5) > plr->lev) && (cave->dun_lvl > s_ptr->slevel)) plr->spell_exp[spell] += 1; }
    }

    /* Do any effects of continual spells */
    for (spell = 0; spell < 32; spell++)
    {
        if (hex_spelling(spell))
        {
            do_spell(REALM_HEX, spell, SPELL_CONT);
        }
    }
}


bool hex_spell_fully(void)
{
    int k_max = 0;

    k_max = (plr->lev / 15) + 1;

    /* Paranoia */
    k_max = MIN(k_max, MAX_KEEP);

    if (plr->magic_num2[0] < k_max) return FALSE;

    return TRUE;
}

void revenge_spell(void)
{
    if (plr->realm1 != REALM_HEX) return;
    if (plr->magic_num2[2] <= 0) return;

    switch(plr->magic_num2[1])
    {
    case 1: do_spell(REALM_HEX, HEX_PATIENCE, SPELL_CONT); break;
    case 2: do_spell(REALM_HEX, HEX_REVENGE, SPELL_CONT); break;
    }
}

void revenge_store(int dam)
{
    if (plr->realm1 != REALM_HEX) return;
    if (plr->magic_num2[2] <= 0) return;

    plr->magic_num1[2] += dam;
}


bool teleport_barrier(int m_idx)
{
    monster_type *m_ptr = dun_mon(cave, m_idx);
    monster_race *r_ptr = m_ptr->race;

    if (!hex_spelling(HEX_ANTI_TELE)) return FALSE;
    if ((plr->lev * 3 / 2) < randint1(r_ptr->alloc.lvl)) return FALSE;

    return TRUE;
}


bool magic_barrier(int m_idx)
{
    monster_type *m_ptr = dun_mon(cave, m_idx);
    return magic_barrier_aux(m_ptr);
}

bool magic_barrier_aux(mon_ptr m_ptr)
{
    monster_race *r_ptr = m_ptr->race;

    if (!hex_spelling(HEX_ANTI_MAGIC)) return FALSE;
    if ((plr->lev * 3 / 2) < randint1(r_ptr->alloc.lvl)) return FALSE;

    return TRUE;
}


bool multiply_barrier(int m_idx)
{
    monster_type *m_ptr;
    monster_race *r_ptr;

    if (!hex_spelling(HEX_ANTI_MULTI)) return FALSE;
    m_ptr = dun_mon(cave, m_idx);
    r_ptr = m_ptr->race;
    if ((plr->lev * 3 / 2) < randint1(r_ptr->alloc.lvl)) return FALSE;

    return TRUE;
}

void hex_stop_spelling_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Stop Spelling");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_ENERGY:
        var_set_int(res, 10);
        break;
    case SPELL_CAST:
        var_set_bool(res, stop_hex_spell());
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _hex_ac(obj_ptr obj)
{
    if (obj_is_armor(obj) && obj_is_cursed(obj))
    {
        int ac = 5;
        if (obj->curse_flags & OFC_HEAVY_CURSE) ac += 7;
        if (obj->curse_flags & OFC_PERMA_CURSE) ac += 13;
        plr->to_a += ac;
        plr->dis_to_a += ac;
    }
}
void hex_calc_bonuses(void)
{
    if (plr->realm1 != REALM_HEX) return;
    if (hex_spelling_any()) plr->skills.stl -= (1 + plr->magic_num2[0]);
    if (hex_spelling(HEX_DETECT_EVIL)) plr->esp_evil = TRUE;
    if (hex_spelling(HEX_DEMON_AURA))
    {
        plr->sh_fire = TRUE;
        plr->regen += 100;
    }
    if (hex_spelling(HEX_ICE_ARMOR))
    {
        plr->sh_cold = TRUE;
        plr->to_a += 30;
        plr->dis_to_a += 30;
    }
    if (hex_spelling(HEX_SHOCK_CLOAK))
    {
        plr->sh_elec = TRUE;
        plr->pspeed += 3;
    }
    equip_for_each(_hex_ac);
    if (hex_spelling(HEX_RUNESWORD))
        plr->vorpal = TRUE;
}
void hex_calc_stats(s16b stats[MAX_STATS])
{
    if (hex_spelling(HEX_XTRA_MIGHT)) stats[A_STR] += 4;
    if (hex_spelling(HEX_BUILDING))
    {
        stats[A_STR] += 4;
        stats[A_DEX] += 4;
        stats[A_CON] += 4;
    }
}
void hex_calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    if (hex_spelling(HEX_XTRA_MIGHT) || hex_spelling(HEX_BUILDING))
    {
        info->blows_calc.wgt /= 2;
        info->blows_calc.mul += 20;
    }
    if (obj_is_cursed(obj))
    {
        if (obj->curse_flags & OFC_CURSED) { info->to_h += 5; info->dis_to_h += 5; }
        if (obj->curse_flags & OFC_HEAVY_CURSE) { info->to_h += 7; info->dis_to_h += 7; }
        if (obj->curse_flags & OFC_PERMA_CURSE) { info->to_h += 13; info->dis_to_h += 13; }
        if (obj->curse_flags & OFC_TY_CURSE) { info->to_h += 5; info->dis_to_h += 5; }
        if (hex_spelling(HEX_RUNESWORD))
        {
            if (obj->curse_flags & OFC_CURSED) { info->to_d += 5; info->dis_to_d += 5; }
            if (obj->curse_flags & OFC_HEAVY_CURSE) { info->to_d += 7; info->dis_to_d += 7; }
            if (obj->curse_flags & OFC_PERMA_CURSE) { info->to_d += 13; info->dis_to_d += 13; }
        }
    }
    if (hex_spelling(HEX_VAMP_BLADE))
    {
        add_flag(info->obj_flags, OF_BRAND_VAMP);
        add_flag(info->obj_known_flags, OF_BRAND_VAMP);
    }
    if (hex_spelling(HEX_RUNESWORD))
    {
        add_flag(info->obj_flags, OF_SLAY_GOOD);
        add_flag(info->obj_known_flags, OF_SLAY_GOOD);
    }
}
