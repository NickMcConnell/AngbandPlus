#include "angband.h"

#include <assert.h>

void satisfy_hunger_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Satisfy Hunger");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fills your belly with pure yumminess.");
        break;
    case SPELL_CAST:
        set_food(PY_FOOD_MAX - 1);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_satisfy_hunger(void) { return cast_spell(satisfy_hunger_spell); }

void scare_monster_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Scare Monster");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    default: /* hydra, spectre, yeek ... innate */
        bolt_spell_aux(cmd, res, GF_FEAR, innate_dice(0, 0, plr->lev));
    }
}

void scare_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Terrify");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to scare one or more monsters.");
        break;
    default: /* angel, demon, dragon, warlock ... spell */
        if (plr->lev < 30)
            bolt_spell_aux(cmd, res, GF_FEAR, spell_dice(0, 0, plr->lev));
        else
            los_spell(cmd, res, GF_FEAR, plr->lev);
    }
}

void self_knowledge_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Self Knowledge");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives you useful info regarding your current resistances, the powers of your weapon and maximum limits of your stats.");
        break;
    case SPELL_CAST:
        self_knowledge();
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void sense_surroundings_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Sense Surroundings");
        break;
    default:
        magic_mapping_spell(cmd, res);
        break;
    }
}

void shadow_shifting_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shadow Shifting");
        break;
    case SPELL_DESC:
        var_set_string(res, "Recreates the current dungeon level after a short delay.");
        break;
    case SPELL_CAST:
        msg_print("You start walking around.");
        alter_reality();
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static dice_t _shoot_dice(void) {
    int slot = equip_find_first(obj_is_weapon);
    if (slot)
    {
        obj_ptr obj = equip_obj(slot);
        return innate_dice(obj->dd, obj->ds, obj->to_d);
    }
    return innate_dice(3, 6, 0);
}
void shoot_arrow_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shoot Arrow");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires an arrow.");
        break;
    default:
        bolt_spell_aux(cmd, res, GF_ARROW, _shoot_dice());
    }
}

void shriek_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Shriek");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generates a large sound ball centered on you.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("Your vocal cords get much tougher.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("Your vocal cords get much weaker.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can emit a horrible shriek.");
        break;
    case SPELL_INFO:
        var_printf(res, "dam %d", plr->lev);
        break;
    case SPELL_CAST:
        stop_mouth();
        plr_burst(8, GF_SOUND, plr->lev);
        aggravate_monsters(who_create_plr());
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_shriek(void) { return cast_spell(shriek_spell); }

void sleeping_dust_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Sleeping Dust");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_CAST:
        msg_print("You throw some magic dust...");
        if (plr->lev < 25) plr_burst(1, GF_SLEEP, plr->lev);
        else plr_project_los(GF_SLEEP, plr->lev);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void sleep_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Sleep");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to sleep one or more monsters.");
        break;
    default:
        if (plr->lev < 30)
            bolt_spell_aux(cmd, res, GF_SLEEP, spell_dice(0, 0, 5 + plr_prorata_level(75)));
        else
            los_spell(cmd, res, GF_SLEEP, 5 + plr_prorata_level(75));
    }
}

void slow_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Slow");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to slow one or more monsters.");
        break;
    default:
        if (plr->lev < 30)
            bolt_spell_aux(cmd, res, GF_SLOW, spell_dice(0, 0, 5 + plr_prorata_level(75)));
        else
            los_spell(cmd, res, GF_SLOW, 5 + plr_prorata_level(75));
    }
}

void smell_metal_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Smell Metal");
        break;
    case SPELL_DESC:
        var_set_string(res, "Smells nearby metallic odors.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You smell a metallic odor.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You no longer smell a metallic odor.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can smell nearby precious metal.");
        break;
    case SPELL_CAST:
        stop_mouth();
        detect_treasure(DETECT_RAD_DEFAULT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void smell_monsters_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Smell Monsters");
        break;
    case SPELL_DESC:
        var_set_string(res, "Detects nearby monsters.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You smell filthy monsters.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You no longer smell filthy monsters.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can smell nearby monsters.");
        break;
    case SPELL_CAST:
        stop_mouth();
        detect_monsters_normal(DETECT_RAD_DEFAULT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void sp_to_hp_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Convert SP to HP");
        break;
    case SPELL_DESC:
        var_set_string(res, "Converts SP into HP");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You are subject to fits of magical healing.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You are no longer subject to fits of magical healing.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "Your blood sometimes rushes to your muscles.");
        break;
    case SPELL_PROCESS:
        if (one_in_(2000))
        {
            int wounds = plr->mhp - plr->chp;

            if (wounds > 0)
            {
                int healing = plr->csp;

                if (healing > wounds)
                    healing = wounds;

                hp_player(healing);
                plr->csp -= healing;

                plr->redraw |= (PR_MANA);
            }
        }
        break;
    case SPELL_CAST:
        if (plr->csp >= plr->lev / 5)
        {
            plr->csp -= plr->lev / 5;
            plr->redraw |= PR_MANA;
            hp_player(plr->lev);
        }
        else
            msg_print("You failed to convert.");

        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void spit_acid_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Spit Acid");
        break;
    case SPELL_DESC:
        if (plr->lev < 25)
            var_set_string(res, "Spits a bolt of acid.");
        else
            var_set_string(res, "Spits a ball of acid.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You gain the ability to spit acid.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You lose the ability to spit acid.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can spit acid.");
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, plr->lev/5);
        break;
    default:
        if (plr->lev < 25)
            bolt_spell_aux(cmd, res, GF_ACID, innate_dice(0, 0, 2*plr->lev));
        else
            ball_spell(cmd, res, 2, GF_ACID, 2*plr->lev);
    }
}
bool cast_spit_acid(void) { return cast_spell(spit_acid_spell); }

static int _starburst_I_dam(void) {
    if (plr->pclass == CLASS_WILD_TALENT) /* Wild-Talents gain both I and II versions ... */
        return 100 + plr_prorata_level_aux(100, 1, 1, 0);
    return 100 + plr_prorata_level_aux(200, 1, 1, 2);
}
void starburst_I_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Star Burst");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a huge ball of powerful light.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, spell_power(_starburst_I_dam() + plr->to_d_spell)));
        break;
    default:
        ball_spell(cmd, res, 4, GF_LIGHT, _starburst_I_dam());
    }
}

static int _starburst_II_dam(void) {
    return plr_prorata_level_aux(450, 1, 0, 2);
}
void starburst_II_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Star Burst");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a huge ball of powerful light.");
        break;
    default:
        ball_spell(cmd, res, 4, GF_LIGHT, _starburst_II_dam());
    }
}

void sterility_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Sterility");
        break;
    case SPELL_DESC:
        var_set_string(res, "Stops breeding monsters from ... umm ... doing the nasty.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You can give everything around you a headache.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You hear a massed sigh of relief.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can cause mass impotence.");
        break;
    case SPELL_CAST:
        msg_print("You suddenly have a headache!");
        take_hit(DAMAGE_LOSELIFE, randint1(17) + 17, "the strain of forcing abstinence");

        /* Fake a population explosion. */
        cave->breed_ct += MAX_REPRO;
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_sterility(void) { return cast_spell(sterility_spell); }

void stinking_cloud_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Stinking Cloud");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a ball of poison.");
        break;
    default:
        ball_spell(cmd, res, 2, GF_POIS, 10 + plr->lev/2);
    }
}

void stone_skin_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Stone Skin");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_CAST:
        plr_tim_add(T_STONE_SKIN, randint1(30) + 20);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_stone_skin(void) { return cast_spell(stone_skin_spell); }

void stone_to_mud_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Stone to Mud");
        break;
    case SPELL_DESC:
        var_set_string(res, "Turns one rock square to mud.");
        break;
    default:
        beam_spell_aux(cmd, res, GF_KILL_WALL, spell_dice(1, 30, 20));
    }
}
bool cast_stone_to_mud(void) { return cast_spell(stone_to_mud_spell); }

void stop_time_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "The World");
        break;
    case SPELL_DESC:
        var_set_string(res, "Spend all of your spell points to stop time. You gain a number of free moves depending on the amount of spell points spent.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("%d acts.", MIN((plr->csp + 100-plr->energy_need - 50)/100, 5)));
        break;
    case SPELL_CAST:
    {
        var_set_bool(res, FALSE);
        if (world_player)
        {
            msg_print("Time is already stopped.");
            return;
        }

        world_player = TRUE;
        msg_print("You yell 'Time!'");
        msg_print(NULL);

        /* Note: We pay the casting cost up front these days. So, add back the 150
           to figure the starting sp, and then bash sp down to 0. We can't use the 
           SPELL_COST_EXTRA mechanism here ... */
        plr->energy_need -= 1000 + (100 + (plr->csp + 150) - 50)*TURNS_PER_TICK/10;
        plr->energy_need = MAX(-1550, plr->energy_need);

        plr->csp = 0;
        plr->csp_frac = 0;

        plr->redraw |= (PR_MAP | PR_STATUS);
        plr->update |= (PU_MONSTERS);
        plr->window |= (PW_OVERHEAD | PW_DUNGEON);
        handle_stuff();

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_amberites_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Amberites");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to summon Amberites for assistance.");
        break;
    case SPELL_CAST:
    {
        int l = spell_power(plr_prorata_level(85));

        msg_print("You summon a Lord of Amber!");
        if (!summon_specific(who_create_plr(), plr->pos, l, SUMMON_AMBERITE, PM_FORCE_PET | PM_ALLOW_UNIQUE))
            msg_print("No Amberites arrives.");

        var_set_bool(res, TRUE);
        break;
    } 
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_angel_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Angel");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to summon a single angel for assistance.");
        break;
    case SPELL_CAST:
    {
        int ct = 0;
        int l = spell_power(plr_prorata_level(75));

        ct += summon_specific(who_create_plr(), plr->pos, l, SUMMON_ANGEL, PM_FORCE_PET);
        if (!ct)
            msg_print("No angel arrives.");

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_ants_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Ants");
        break;
    case SPELL_DESC:
        var_set_string(res, "Summon ants for assistance.");
        break;
    case SPELL_CAST:
    {
        int num = randint1(plr->lev/10);
        int ct = 0, i;
        int l = spell_power(plr_prorata_level(75));

        for (i = 0; i < num; i++)
        {
            ct += summon_specific(who_create_plr(), plr->pos, l, SUMMON_ANT, PM_FORCE_PET | PM_ALLOW_GROUP);
        }
        if (!ct)
            msg_print("No ants arrive.");
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_cyberdemon_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Cyberdemon");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to summon a single cyberdemon for assistance.");
        break;
    case SPELL_CAST:
    {
        int ct = 0;
        int l = spell_power(plr_prorata_level(100)); /* CL45 to get DL77 */

        ct += summon_specific(who_create_plr(), plr->pos, l, SUMMON_CYBER, PM_FORCE_PET);
        if (!ct)
            msg_print("No cyberdemon arrives.");

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_demon_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Demon");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to summon a single demon for assistance.");
        break;
    case SPELL_CAST:
    {
        bool pet = !one_in_(3);
        who_t who = pet ? who_create_plr() : who_create_null();
        u32b mode = 0L;
        int  l = spell_power(plr_prorata_level(85));

        if (pet) mode |= PM_FORCE_PET;
        else mode |= PM_NO_PET;
        if (!(pet && (plr->lev < 50))) mode |= PM_ALLOW_GROUP;

        if (summon_specific(who, plr->pos, l, SUMMON_DEMON, mode))
        {
            msg_print("The area fills with a stench of sulphur and brimstone.");
            if (pet)
                msg_print("'What is thy bidding... Master?'");
            else
                msg_print("'NON SERVIAM! Wretch! I shall feast on thy mortal soul!'");
        }
        else
            msg_print("No demons arrive.");

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_demon_II_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Demon");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to summon a single demon for assistance.");
        break;
    case SPELL_CAST:
    {
        int ct = 0;
        int l = spell_power(plr_prorata_level(85));

        ct += summon_specific(who_create_plr(), plr->pos, l, SUMMON_DEMON, PM_FORCE_PET);
        if (ct)
            msg_print("The area fills with a stench of sulphur and brimstone.");
        else
            msg_print("No demon arrives.");

        var_set_bool(res, TRUE);
        break;
    } 
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_dragon_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Dragon");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to summon a single dragon for assistance.");
        break;
    case SPELL_CAST:
    {
        int ct = 0;
        int l = spell_power(plr_prorata_level(75));

        ct += summon_specific(who_create_plr(), plr->pos, l, SUMMON_DRAGON, PM_FORCE_PET);
        if (!ct)
            msg_print("No dragon arrives.");

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_greater_demon_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Greater Demon");
        break;
    case SPELL_DESC:
        var_set_string(res, "Summons greater demon. You need to sacrifice a corpse of a human ('p','h' or 't') and the more powerful the corpse, the more powerful the demon you will conjure.");
        break;
    case SPELL_CAST:
        var_set_bool(res, cast_summon_greater_demon());
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_hi_dragon_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Ancient Dragons");
        break;
    case SPELL_DESC:
        var_set_string(res, "Summon one or more ancient dragons for assistance.");
        break;
    case SPELL_CAST:
    {
        int num = randint1(plr->lev/10);
        int ct = 0, i;

        if (plr->dragon_realm == DRAGON_REALM_DOMINATION)
            num = 2 + randint1(3);

        for (i = 0; i < num; i++)
        {
            int l = spell_power(plr_prorata_level(85));
            ct += summon_specific(who_create_plr(), plr->pos, l, SUMMON_HI_DRAGON, PM_FORCE_PET);
        }
        if (!ct)
            msg_print("No dragons arrive.");
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_hi_undead_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Greater Undead");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to summon greater undead for assistance.");
        break;
    case SPELL_CAST:
    {
        int num = randint1(plr->lev/10);
        int ct = 0, i;
        int l = spell_power(plr_prorata_level(85));

        for (i = 0; i < num; i++)
        {
            ct += summon_specific(who_create_plr(), plr->pos, l, SUMMON_HI_UNDEAD, PM_FORCE_PET);
        }
        if (!ct)
            msg_print("No undead arrive.");
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_hounds_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Hounds");
        break;
    case SPELL_DESC:
        var_set_string(res, "Summon hounds for assistance.");
        break;
    case SPELL_CAST:
    {
        int num = randint1(plr->lev/10);
        int ct = 0, i;
        int l = spell_power(plr_prorata_level(75));

        for (i = 0; i < num; i++)
        {
            ct += summon_specific(who_create_plr(), plr->pos, l, SUMMON_HOUND, PM_FORCE_PET | PM_ALLOW_GROUP);
        }
        if (!ct)
            msg_print("No hounds arrive.");
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_hydras_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Hydras");
        break;
    case SPELL_DESC:
        var_set_string(res, "Summon hydras for assistance.");
        break;
    case SPELL_CAST:
    {
        int num = randint1(plr->lev/10);
        int ct = 0, i;
        int l = spell_power(plr_prorata_level(75));

        for (i = 0; i < num; i++)
        {
            ct += summon_specific(who_create_plr(), plr->pos, l, SUMMON_HYDRA, PM_FORCE_PET | PM_ALLOW_GROUP);
        }
        if (!ct)
            msg_print("No hydras arrive.");
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_kin_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Kin");
        break;
    case SPELL_DESC:
        var_set_string(res, "Summon related monsters for assistance.");
        break;
    case SPELL_CAST:
        if (!summon_kin_player(plr->lev, plr->pos, PM_FORCE_PET | PM_ALLOW_GROUP))
            msg_print("No help arrives.");
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_manes_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Manes");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to summon some demonic friends.");
        break;
    case SPELL_CAST:
        if (!summon_specific(who_create_plr(), plr->pos, (plr->lev * 3) / 2, SUMMON_MANES, (PM_ALLOW_GROUP | PM_FORCE_PET)))
            msg_print("No Manes arrive.");

        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_monster_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Monster");
        break;
    case SPELL_DESC:
        var_set_string(res, "Summon a monster for assistance.");
        break;
    case SPELL_CAST:
    {
        int l = spell_power(plr_prorata_level(75));

        if (!summon_specific(who_create_plr(), plr->pos, l, 0, PM_FORCE_PET | PM_ALLOW_GROUP))
            msg_print("No monsters arrive.");
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_monsters_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Monsters");
        break;
    case SPELL_DESC:
        var_set_string(res, "Summon monsters for assistance.");
        break;
    case SPELL_CAST:
    {
        int num = randint1(plr->lev/10);
        int ct = 0, i;
        int l = spell_power(plr_prorata_level(75));

        for (i = 0; i < num; i++)
        {
            ct += summon_specific(who_create_plr(), plr->pos, l, 0, PM_FORCE_PET | PM_ALLOW_GROUP);
        }
        if (!ct)
            msg_print("No monsters arrive.");
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_spiders_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Spiders");
        break;
    case SPELL_DESC:
        var_set_string(res, "Summon spiders for assistance.");
        break;
    case SPELL_CAST:
    {
        int num = randint1(plr->lev/10);
        int ct = 0, i;
        int l = spell_power(plr_prorata_level(75));

        for (i = 0; i < num; i++)
        {
            ct += summon_specific(who_create_plr(), plr->pos, l, SUMMON_SPIDER, PM_FORCE_PET | PM_ALLOW_GROUP);
        }
        if (!ct)
            msg_print("No spiders arrive.");
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_tree_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        if (plr->lev >= 45)
            var_set_string(res, "Summon Trees");
        else
            var_set_string(res, "Summon Tree");
        break;
    case SPELL_DESC:
        if (plr->lev >= 45)
            var_set_string(res, "Attempts to summon many trees");
        else
            var_set_string(res, "Attempts to summon a tree.");
        break;
    case SPELL_CAST:
        if (plr->lev >= 45)
        {
            tree_creation();
            var_set_bool(res, TRUE);
        }    
        else
        {
            int attempts = 0;
            int dir;
            point_t pos;
            dun_cell_ptr cell;

            var_set_bool(res, TRUE);
            for (;;)
            {
                if (attempts > 4)
                {
                    msg_print("No trees arrive.");
                    break;
                }

                dir = randint0(9);
                if (dir == 5) continue;

                attempts++;
                pos = point_step(plr->pos, dir);

                if (!dun_pos_interior(cave, pos)) continue;
                if (dun_mon_at(cave, pos)) continue;
                if (dun_obj_at(cave, pos)) break;
                cell = dun_cell_at(cave, pos);
                if (!floor_is_clean(cell)) continue;
                assert(!dun_plr_at(cave, pos));

                dun_place_tree(cave, pos);
                break;
            }
        }
        break;
    case SPELL_COST_EXTRA:
    {
        int n = 0;
        if (plr->lev >= 45)
            n += 30;

        var_set_int(res, n);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_summon_tree(void) { return cast_spell(summon_tree_spell); }

void summon_undead_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Undead");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to summon undead for assistance.");
        break;
    case SPELL_CAST:
    {
        int num = randint1(plr->lev/10);
        int ct = 0, i;
        int l = spell_power(plr_prorata_level(75));

        for (i = 0; i < num; i++)
        {
            ct += summon_specific(who_create_plr(), plr->pos, l, SUMMON_UNDEAD, PM_FORCE_PET);
        }
        if (!ct)
            msg_print("No undead arrive.");
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void summon_uniques_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Summon Uniques");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to summon unique monsters for assistance.");
        break;
    case SPELL_CAST:
    {
        int l = spell_power(plr_prorata_level(85));

        msg_print("You summon a special opponent!");
        if (!summon_specific(who_create_plr(), plr->pos, l, SUMMON_UNIQUE, PM_FORCE_PET | PM_ALLOW_UNIQUE))
            msg_print("Nobody arrives.");

        var_set_bool(res, TRUE);
        break;
    } 
    default:
        default_spell(cmd, res);
        break;
    }
}

void super_stealth_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Hide in Darkness");
        break;
    case SPELL_DESC:
        var_set_string(res, "Grants the stealth of the Ninja!  You may hide in shadows and see in the dark. Your light radius is decreased by 3.");
        break;
    case SPELL_CAST:
        plr_tim_add(T_SUPERSTEALTH, spell_power(randint1(plr->lev/2) + plr->lev/2));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void swap_pos_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Swap Position");
        break;
    case SPELL_DESC:
        var_set_string(res, "Swap locations with a given monster.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You feel like walking a mile in someone else's shoes.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You feel like staying in your own shoes.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can switch locations with another being.");
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, FALSE);

        project_length = -1;
        if (get_fire_dir(&dir))
        {
            teleport_swap(dir);
            var_set_bool(res, TRUE);
        }
        project_length = 0;
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_swap_pos(void) { return cast_spell(swap_pos_spell); }

void sword_dance_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Sword Dancing");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attacks adjacent monsters randomly.");
        break;
    case SPELL_CAST: {
        int i;
        for (i = 0; i < 6; i++)
        {
            int     dir = ddd[randint0(8)];
            point_t p = point_step(plr->pos, dir);
            mon_ptr mon = dun_mon_at(cave, p);

            if (mon) plr_attack_normal(p);
            else msg_print("You attack the empty air.");
        }
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

void telekinesis_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Telekinesis");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to fetch a distant object.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You gain the ability to move objects telekinetically.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You lose the ability to move objects telekinetically.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You are telekinetic.");
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        var_set_bool(res, FALSE);
        if (get_aim_dir(&dir))
        {
            fetch(dir, plr->lev * 10, TRUE);
            var_set_bool(res, TRUE);
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_telekinesis(void) { return cast_spell(telekinesis_spell); }

void telepathy_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Telepathy");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives telepathy for a while.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(25, 30));
        break;
    case SPELL_CAST:
        plr_tim_add(T_TELEPATHY, randint1(25) + 30);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void teleport_other_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Teleport Other");
        break;
    case SPELL_DESC:
        var_set_string(res, "Teleports all monsters on the line away unless resisted.");
        break;
    default:
        beam_spell_aux(cmd, res, GF_TELEPORT, spell_dice(0, 0, 2*plr->lev));
    }
}

void teleport_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Teleport");
        break;
    case SPELL_DESC:
        var_set_string(res, "Escape to a distant location.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You gain the power of teleportation at will.");
        mut_lose(MUT_TELEPORT_RND);
        break;
    case SPELL_LOSE_MUT:
        msg_print("You lose the power of teleportation at will.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can teleport at will.");
        break;
    case SPELL_CAST:
        teleport_player(10 + 4 * plr->lev, 0);
        var_set_bool(res, TRUE);
        break;
    case SPELL_ENERGY:
        if (mut_present(MUT_ASTRAL_GUIDE))
        {
            var_set_int(res, 30);
            break;
        }
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_teleport(void) { return cast_spell(teleport_spell); }

void teleport_level_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Teleport Level");
        break;
    case SPELL_DESC:
        var_set_string(res, "Escape to another level.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (!get_check("Are you sure? (Teleport Level)")) return;
        dun_teleport_level_plr(cave);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_teleport_level(void) { return cast_spell(teleport_level_spell); }

void teleport_to_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Teleport To");
        break;
    case SPELL_DESC:
        var_set_string(res, "Teleport a visible monster next to you.");
        break;
    case SPELL_CAST: {
        mon_ptr mon;
        point_t pos;
        char m_name[80];

        if (!target_set(TARGET_KILL)) break;
        pos = who_pos(plr->target);
        if (!dun_mon_at(cave, pos)) break;
        if (!plr_view(pos)) break;
        if (!plr_project(pos)) break;

        var_set_bool(res, TRUE);

        mon = dun_mon_at(cave, pos);
        monster_desc(m_name, mon, 0);
        if (_1d(100) <= mon_res_pct(mon, GF_TELEPORT))
        {
            mon_lore_resist(mon, GF_TELEPORT);
            msg_format("%s resists!", m_name);
        }
        else
        {
            msg_format("You command %s to return.", m_name);
            teleport_monster_to(mon, plr->pos, 100, TELEPORT_PASSIVE);
        }
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static int _boulder_dam_aux(int max) {
    return plr_prorata_level_aux(max, 2, 1, 2);
}
static int _boulder_dam(void) {
    if (prace_is_(RACE_CYCLOPS))
        return _boulder_dam_aux(150);
    return _boulder_dam_aux(250);
}
void throw_boulder_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Throw Boulder");
        break;
    case SPELL_DESC:
        var_set_string(res, "Hurls a huge boulder at chosen target.");
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, (_boulder_dam() + 6)/7);
        break;
    default:
        bolt_spell_aux(cmd, res, GF_ROCK, innate_dice(0, 0, _boulder_dam()));
    }
}

void touch_of_confusion_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Touch of Confusion");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to confuse the next monster that you hit.");
        break;
    case SPELL_CAST:
        if (!(plr->special_attack & ATTACK_CONFUSE))
        {
            msg_print("Your hands start glowing.");
            plr->special_attack |= ATTACK_CONFUSE;
            plr->redraw |= (PR_STATUS);
        }
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void turn_undead_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Turn Undead");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to scare undead monsters in sight.");
        break;
    default:
        los_spell(cmd, res, GF_TURN_UNDEAD, plr->lev);
    }
}

void vampirism_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Vampiric Drain");
        break;
    case SPELL_DESC:
        var_set_string(res, "Suck blood from an adjacent monster, gaining hp in the process.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, spell_power(plr->lev * 2)));
        break;
    case SPELL_GAIN_MUT:
        msg_print("You become vampiric.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You are no longer vampiric.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can drain life from a foe like a vampire.");
        break;
    case SPELL_CAST: {
        int dam = spell_power(plr->lev*2);
        mon_ptr mon = plr_target_adjacent_mon();

        var_set_bool(res, FALSE);
        if (!mon) break;
        var_set_bool(res, TRUE);

        msg_print("You grin and bare your fangs...");
        stop_mouth(); /* no singing with your mouth full! */

        if (plr_touch_mon(mon, GF_OLD_DRAIN, dam))
        {
            if (plr->food < PY_FOOD_FULL)
                vamp_player(dam);
            else
                msg_print("You were not hungry.");

            if (plr->food < PY_FOOD_MAX)
            {
                int food = plr->food + MIN(5000, 100*dam);
                food = MIN(PY_FOOD_MAX - 1, food);
                set_food(food);
            }
        }
        else if (!mon_is_living(mon))
            msg_print("Yechh. That tastes foul.");
        break; }
    case SPELL_COST_EXTRA:
        var_set_int(res, plr->lev / 3);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_vampirism(void) { return cast_spell(vampirism_spell); }

void water_ball_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Water Ball");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a ball of water.");
        break;
    default:
        ball_spell(cmd, res, 2, GF_WATER, 50 + 4*plr->lev);
    }
}

void water_bolt_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Water Bolt");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a bolt of water.");
        break;
    default:
        bolt_spell(cmd, res, GF_WATER, 7 + plr->lev/4, 15);
    }
}

void weigh_magic_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Weigh Magic");
        break;
    case SPELL_DESC:
        var_set_string(res, "Determine the strength of magics affecting you.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You feel you can better understand the magic around you.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You no longer sense magic.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can feel the strength of the magics affecting you.");
        break;
    case SPELL_CAST:
        report_magics();
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_weigh_magic(void) { return cast_spell(weigh_magic_spell); }

void wonder_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Wonder");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires something with random effects.");
        break;
    case SPELL_CAST:
        var_set_bool(res, cast_wonder());
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void wraithform_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Wraithform");
        break;
    case SPELL_DESC:
        var_set_string(res, "Leave the world of the living and travel the shadows of the underwold. You gain passwall and great resistance to damage.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_duration(spell_power(plr->lev/2), spell_power(plr->lev/2)));
        break;
    case SPELL_CAST:
    {
        int base = spell_power(plr->lev / 2);
        plr_tim_add(T_WRAITH, randint1(base) + base);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
