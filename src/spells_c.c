#include "angband.h"

void cause_wounds_I_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Cause Light Wounds");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to damage a single foe.");
        break;
    default:
        curse_spell(cmd, res, GF_CAUSE_1, 3, 8);
    } 
}

void cause_wounds_II_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Cause Medium Wounds");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to damage a single foe.");
        break;
    default:
        curse_spell(cmd, res, GF_CAUSE_2, 8, 8);
    }
}

void cause_wounds_III_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Cause Critical Wounds");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to damage a single foe.");
        break;
    default:
        curse_spell(cmd, res, GF_CAUSE_3, 10, 15);
    }
}

void cause_wounds_IV_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Cause Mortal Wounds");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to damage a single foe.");
        break;
    default:
        curse_spell(cmd, res, GF_CAUSE_4, 15, 15);
    }
}

void clairvoyance_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Clairvoyance");
        break;
    case SPELL_DESC:
        var_set_string(res, "Maps and lights whole dungeon level and gives telepathy for a while.");
        break;
    case SPELL_CAST:
        virtue_add(VIRTUE_KNOWLEDGE, 1);
        virtue_add(VIRTUE_ENLIGHTENMENT, 1);

        wiz_lite();
        plr_tim_add(T_TELEPATHY, randint1(30) + 25);

        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void clear_mind_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Clear Mind");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_SPOIL_DESC:
        var_set_string(res, "Player regains 3 + L/20 sp. This won't work if the player has any pets.");
        break;
    case SPELL_CAST:
    {
        int amt;

        var_set_bool(res, FALSE);
        if (plr_pet_count())
        {
            msg_print("You need to concentrate on your pets now.");
            return;
        }
        if (plr->pclass == CLASS_RUNE_KNIGHT)
        {
            msg_print("Your mind remains cloudy.");
            return;
        }

        msg_print("You feel your head clear a little.");

        if (plr->pclass == CLASS_PSION) /* Testing ... */
            amt = 3 + plr->lev/10;
        else
            amt = 3 + plr->lev/20;

        sp_player(amt);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_clear_mind(void) { return cast_spell(clear_mind_spell); }

void confuse_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Confuse");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempt to confuse one or more monsters.");
        break;
    default:
        if (plr->lev < 40)
            bolt_spell_aux(cmd, res, GF_OLD_CONF, spell_dice(0, 0, 2*plr->lev));
        else
            los_spell(cmd, res, GF_OLD_CONF, 2*plr->lev);
    }
}

void cold_touch_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Cold Touch");
        break;
    case SPELL_DESC:
        var_set_string(res, "Freeze things with your icy fingers!");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, spell_power(2 * plr->lev)));
        break;
    case SPELL_GAIN_MUT:
        msg_print("Your hands get very cold.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("Your hands warm up.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can freeze things with a touch.");
        break;
    case SPELL_CAST: {
        mon_ptr mon = plr_target_adjacent_mon();
        var_set_bool(res, FALSE);
        if (!mon) break;
        plr_touch_mon(mon, GF_COLD, spell_power(2*plr->lev));
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_cold_touch(void) { return cast_spell(cold_touch_spell); }

void confusing_lights_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Confusing Lights");
        break;
    case SPELL_DESC:
        var_set_string(res, "Emits confusing lights, slowing, stunning, confusing, scaring and freezing nearby monsters.");
        break;
    case SPELL_CAST:
        msg_print("You glare nearby monsters with a dazzling array of confusing lights!");
        confusing_lights(plr_prorata_level(100));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void crafting_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Crafting");
        break;
    case SPELL_DESC:
        var_set_string(res, "Makes chosen weapon, armor or ammo an ego item.");
        break;
    case SPELL_CAST:
    {
        obj_prompt_t prompt = {0};
        bool         okay = FALSE;
        char         o_name[MAX_NLEN];

        var_set_bool(res, FALSE);

        prompt.prompt = "Enchant which item?";
        prompt.error = "You have nothing to enchant.";
        prompt.filter = object_is_weapon_armour_ammo;
        prompt.where[0] = INV_PACK;
        prompt.where[1] = INV_EQUIP;
        prompt.where[2] = INV_QUIVER;
        prompt.where[3] = INV_FLOOR;

        obj_prompt(&prompt);
        if (!prompt.obj) return;

        object_desc(o_name, prompt.obj, (OD_OMIT_PREFIX | OD_NAME_ONLY));

        if (object_is_nameless(prompt.obj))
        {
            if (obj_is_ammo(prompt.obj) && randint1(30) > (prompt.obj->number - 30))
            {
                if (brand_weapon_aux(prompt.obj))
                {
                    if (!object_is_rare(prompt.obj))
                        prompt.obj->discount = 99;
                    okay = TRUE;
                }
            }
            else if (obj_is_weapon(prompt.obj) && prompt.obj->number == 1)
            {
                if (brand_weapon_aux(prompt.obj))
                {
                    if (!object_is_rare(prompt.obj))
                        prompt.obj->discount = 99;
                    okay = TRUE;
                }
            }
            else if (obj_is_bow(prompt.obj) && prompt.obj->number == 1)
            {
                if (brand_weapon_aux(prompt.obj))
                {
                    if (!object_is_rare(prompt.obj))
                        prompt.obj->discount = 99;
                    okay = TRUE;
                }
            }
            else if (obj_is_armor(prompt.obj) && prompt.obj->number == 1)
            {
                if (brand_armour_aux(prompt.obj))
                {
                    if (!object_is_rare(prompt.obj))
                        prompt.obj->discount = 99;
                    okay = TRUE;
                }
            }
        }

        msg_format("The %s glow%s brightly!", o_name,
                ((prompt.obj->number > 1) ? "" : "s"));

        if (!okay)
        {
            if (flush_failure) flush();
            msg_print("The enchantment failed.");
            if (one_in_(3)) virtue_add(VIRTUE_ENCHANTMENT, -1);
        }
        else
        {
            virtue_add(VIRTUE_ENCHANTMENT, 1);
            obj_identify_fully(prompt.obj);
            obj_display(prompt.obj);
            obj_release(prompt.obj, OBJ_RELEASE_ENCHANT);
        }
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_crafting(void) { return cast_spell(crafting_spell); }

void create_darkness_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Create Darkness");
        break;
    case SPELL_DESC:
        var_set_string(res, "Darken nearby area and inside of a room.");
        break;
    case SPELL_CAST:
        unlite_area(0, 3);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void create_food_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Create Food");
        break;
    case SPELL_DESC:
        if (plr->prace == RACE_HOBBIT)
            var_set_string(res, "It's time for second breakfast!  Cook up a tasty meal.");
        else
            var_set_string(res, "Create a ration of tasty food.");
        break;
    case SPELL_CAST:
    {
        object_type forge;

        object_prep(&forge, lookup_kind(TV_FOOD, SV_FOOD_RATION));
        drop_near(&forge, plr->pos, -1);

        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_create_food(void) { return cast_spell(create_food_spell); }

void create_major_trap_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Create Major Trap");
        break;
    case SPELL_DESC:
        var_set_string(res, "Sets a trap under you. This trap will have various effects on a passing monster.");
        break;
    case SPELL_CAST:
        dun_place_plr_trap_major(cave, plr->pos);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void create_minor_trap_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Create Minor Trap");
        break;
    case SPELL_DESC:
        var_set_string(res, "Sets a weak trap under you. This trap will have various weak effects on a passing monster.");
        break;
    case SPELL_CAST:
        dun_place_plr_trap_minor(cave, plr->pos);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void create_ultimate_trap_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Create Ultimate Trap");
        break;
    case SPELL_DESC:
        var_set_string(res, "Sets an extremely powerful trap under you. This trap will have various strong effects on a passing monster.");
        break;
    case SPELL_CAST:
        dun_place_plr_trap_ultimate(cave, plr->pos);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void crusade_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Crusade");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to charm all good monsters in sight, and scare all non-charmed monsters, and summons great number of knights, and gives heroism, bless, speed and protection from evil.");
        break;
    case SPELL_CAST: {
        int base = 25;
        int sp_sides = 20 + plr->lev;
        int sp_base = plr->lev;
        int i;

        plr_project_los(GF_CRUSADE, plr->lev*4);
        for (i = 0; i < 12; i++)
        {
            int attempt = 10;
            point_t pos;

            while (attempt--)
            {
                pos = scatter(plr->pos, 4);
                if (dun_allow_mon_at(cave, pos)) break;
            }
            if (attempt < 0) continue;
            summon_specific(who_create_plr(), pos, plr->lev, SUMMON_KNIGHT, (PM_ALLOW_GROUP | PM_FORCE_PET | PM_HASTE));
        }
        plr_tim_add(T_HERO, randint1(base) + base);
        plr_tim_add(T_BLESSED, randint1(base) + base);
        plr_tim_add(T_FAST, randint1(sp_sides) + sp_base);
        plr_tim_add(T_PROT_EVIL, randint1(base) + base);
        fear_clear_p();

        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_crusade(void) { return cast_spell(crusade_spell); }

void cure_poison_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Cure Poison");
        break;
    case SPELL_DESC:
        var_set_string(res, "Cure poison status.");
        break;
    case SPELL_CAST:
        plr_tim_recover(T_POISON, 80, 100);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void cure_wounds_I_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Cure Light Wounds");
        break;
    case SPELL_DESC:
        var_set_string(res, "Heals cut and HP a little.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(2, spell_power(10), 0));
        break;
    case SPELL_CAST:
        hp_player(spell_power(damroll(2, 10)));
        plr_tim_subtract(T_CUT, 10);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void cure_wounds_II_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Cure Medium Wounds");
        break;
    case SPELL_DESC:
        var_set_string(res, "Heals cut and HP more.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(4, spell_power(10), 0));
        break;
    case SPELL_CAST:
        hp_player(spell_power(damroll(4, 10)));
        plr_tim_recover(T_CUT, 50, 0);
        plr_tim_subtract(T_CUT, 20);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void cure_wounds_III_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Cure Critical Wounds");
        break;
    case SPELL_DESC:
        var_set_string(res, "Heals cut, stun and HP greatly.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_heal(8, spell_power(10), 0));
        break;
    case SPELL_CAST:
        hp_player(spell_power(damroll(8, 10)));
        plr_tim_remove(T_STUN);
        plr_tim_remove(T_CUT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void curing_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Curing");
        break;
    case SPELL_DESC:
        var_set_string(res, "It heals you a bit and cures blindness, poison, confusion, stunning, cuts and hallucination.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_heal(0, 0, spell_power(50)));
        break;
    case SPELL_CAST:
        hp_player(spell_power(50));
        plr_tim_remove(T_BLIND);
        plr_tim_recover(T_POISON, 65, 150);
        plr_tim_remove(T_CONFUSED);
        plr_tim_remove(T_STUN);
        plr_tim_remove(T_CUT);
        plr_tim_remove(T_HALLUCINATE);
        plr_tim_remove(T_BERSERK);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static int _darkness_storm_I_dam(void) {
    if (plr->pclass == CLASS_WILD_TALENT) /* Wild-Talents gain both I and II versions ... */
        return 100 + plr_prorata_level_aux(100, 1, 1, 0);
    return 100 + plr_prorata_level_aux(200, 1, 1, 2);
}
void darkness_storm_I_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Darkness Storm");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a huge ball of darkness.");
        break;
    default:
        ball_spell(cmd, res, 4, GF_DARK, _darkness_storm_I_dam());
    }
}

static int _darkness_storm_II_dam(void) {
    return plr_prorata_level_aux(450, 1, 0, 2);
}
void darkness_storm_II_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Darkness Storm");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a huge ball of darkness of unmatched power");
        break;
    default:
        ball_spell(cmd, res, 4, GF_DARK, _darkness_storm_II_dam());
    }
}

void day_of_the_dove_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Day of the Dove");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to charm all monsters in sight.");
        break;
    default:
        los_spell(cmd, res, GF_CHARM, 2*plr->lev);
    }
}

void dazzle_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dazzle");
        break;
    case SPELL_DESC:
        var_set_string(res, "Emits dazzling lights, stunning, confusing and scaring nearby monsters.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You gain the ability to emit dazzling lights.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You lose the ability to emit dazzling lights.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can emit confusing, blinding radiation.");
        break;
    case SPELL_CAST:
        plr_project_los(GF_STUN, 5 + plr->lev/5);
        plr_project_los(GF_OLD_CONF, plr->lev * 4);
        plr_project_los(GF_FEAR, plr->lev * 4);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_dazzle(void) { return cast_spell(dazzle_spell); }

void detect_life_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Life");
        break;
    case SPELL_DESC:
        var_set_string(res, "Locate nearby living monsters.");
        break;
    case SPELL_CAST:
        detect_monsters_living(DETECT_RAD_DEFAULT, "You sense the presence of life around you.");
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void detect_unlife_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Unlife");
        break;
    case SPELL_DESC:
        var_set_string(res, "Detects all nonliving monsters in your vicinity.");
        break;
    case SPELL_CAST:
        detect_monsters_nonliving(DETECT_RAD_DEFAULT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void demon_breath_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Breathe Fire/Nether");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breathe a powerful blast of either fire or nether at your opponent.");
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, plr->lev/3);
        break;
    default:
       {int gf = one_in_(2) ? GF_NETHER : GF_FIRE;
        breath_spell_innate(cmd, res, 1 + plr->lev/15, gf, 3*plr->lev);}
    }
}

void destruction_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Word of Destruction");
        break;
    case SPELL_DESC:
        var_set_string(res, "Destroys everything in your nearby vicinity ... except you, of course.");
        break;
    case SPELL_CAST:
        destroy_area(plr->pos, 12 + randint1(4), spell_power(4 * plr->lev));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_destruction(void) { return cast_spell(destruction_spell); }

static void _detect_curses(obj_ptr obj)
{
    if (obj_is_cursed(obj))
        obj->feeling = FEEL_CURSED;
}

void detect_curses_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Curses");
        break;
    case SPELL_DESC:
        var_set_string(res, "Detected cursed items in your inventory.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You can feel evil magics.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You can no longer feel evil magics.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can feel the danger of evil magic.");
        break;
    case SPELL_CAST:
    {
        pack_for_each(_detect_curses);
        equip_for_each(_detect_curses);
        quiver_for_each(_detect_curses);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_detect_curses(void) { return cast_spell(detect_curses_spell); }

void detect_doors_stairs_traps_spell(int cmd, var_ptr res)
{
    int rad = DETECT_RAD_DEFAULT;

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Doors & Traps");
        break;
    case SPELL_DESC:
        var_set_string(res, "Detects doors, stairs, and traps in your vicinity.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_radius(rad));
        break;
    case SPELL_CAST:
        detect_traps(rad, TRUE);
        detect_doors(rad);
        detect_stairs(rad);
        detect_recall(rad);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_detect_doors_stairs_traps(void) { return cast_spell(detect_doors_stairs_traps_spell); }

void detect_evil_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Evil");
        break;
    case SPELL_DESC:
        var_set_string(res, "Detects nearby evil monsters.");
        break;
    case SPELL_CAST:
        detect_monsters_evil(DETECT_RAD_DEFAULT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void detect_menace_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Ferocity");
        break;
    case SPELL_DESC:
        var_set_string(res, "Detects nearby menacing monsters. Only intelligent monsters are detected.");
        break;
    case SPELL_CAST:
        detect_monsters_mind(DETECT_RAD_DEFAULT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void detect_monsters_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Monsters");
        break;
    case SPELL_DESC:
        var_set_string(res, "Detects nearby monsters.");
        break;
    case SPELL_CAST:
        detect_monsters_normal(DETECT_RAD_DEFAULT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_detect_monsters(void) { return cast_spell(detect_monsters_spell); }

void detect_objects_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Objects");
        break;
    case SPELL_DESC:
        var_set_string(res, "Detects nearby objects.");
        break;
    case SPELL_CAST:
        detect_objects_normal(DETECT_RAD_DEFAULT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_detect_objects(void) { return cast_spell(detect_objects_spell); }

void detect_traps_spell(int cmd, var_ptr res)
{
    int rad = DETECT_RAD_DEFAULT;

    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Traps");
        break;
    case SPELL_DESC:
        var_set_string(res, "Detects traps in your vicinity.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_radius(rad));
        break;
    case SPELL_CAST:
        detect_traps(rad, TRUE);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_detect_traps(void) { return cast_spell(detect_traps_spell); }

void detect_treasure_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Treasure");
        break;
    case SPELL_DESC:
        var_set_string(res, "Detects nearby treasure.");
        break;
    case SPELL_CAST:
        detect_treasure(DETECT_RAD_DEFAULT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_detect_treasure(void) { return cast_spell(detect_treasure_spell); }

void detection_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detection");
        break;
    case SPELL_DESC:
        var_set_string(res, "Detects all monsters, traps, doors, stairs, treasures and items in your vicinity.");
        break;
    case SPELL_CAST:
        detect_all(DETECT_RAD_DEFAULT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void dimension_door_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dimension Door");
        break;
    case SPELL_DESC:
        var_set_string(res, "Open a portal to another dimension and step to a nearby location with great precision.");
        break;
    case SPELL_CAST:
        var_set_bool(res, dimension_door(plr->lev / 2 + 10));
        break;
    case SPELL_ENERGY:
        if (mut_present(MUT_ASTRAL_GUIDE))
            var_set_int(res, 30);
        else
            default_spell(cmd, res);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_dimension_door(void) { return cast_spell(dimension_door_spell); }

void disintegrate_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Disintegrate");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a huge ball of disintegration.");
        break;
    default:
        ball_spell(cmd, res, 3 + plr->lev/40, GF_DISINTEGRATE, 70 + plr->lev);
    }
}

void dispel_evil_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dispel Evil");
        break;
    case SPELL_DESC:
        var_set_string(res, "Damages all evil monsters in sight.");
        break;
    default:
        los_dam_spell(cmd, res, GF_DISP_EVIL, 5*plr->lev/2);
    }
}

void dispel_life_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dispel Life");
        break;
    case SPELL_DESC:
        var_set_string(res, "Damages all living monsters in sight.");
        break;
    default:
        los_dam_spell(cmd, res, GF_DISP_LIVING, 5*plr->lev/2);
    }
}

void dispel_magic_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dispel Magic");
        break;
    case SPELL_DESC:
        var_set_string(res, "Dispels all magics which is effecting a monster.");
        break;
    case SPELL_CAST: {
        mon_ptr mon = plr_target_mon();
        var_set_bool(res, FALSE);
        if (!mon) return;
        mon_tim_dispel(mon);
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

void dispel_undead_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dispel Undead");
        break;
    case SPELL_DESC:
        var_set_string(res, "Damages all undead monsters in sight.");
        break;
    default:
        los_dam_spell(cmd, res, GF_DISP_UNDEAD, 3*plr->lev);
    }
}

void dominate_living_I_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dominate a Living Thing");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, (plr->lev+3)/4);
        break;
    default:
        ball_spell(cmd, res, 0, GF_CONTROL_LIVING, plr->lev);
    }
}

void dominate_living_II_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dominate Living Things");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, (plr->lev+20)/2);
        break;
    default:
        los_spell(cmd, res, GF_CONTROL_LIVING, plr->lev);
    }
}

static dice_t _drain_mana_dice(void) {
    return spell_dice(1, 3*plr->lev, plr->lev);
}
void drain_mana_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Drain Mana");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to drain mana from chosen monster.");
        break;
    case SPELL_INFO:
        var_set_string(res, dice_info_dam(_drain_mana_dice()));
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_cast_ball(0, GF_DRAIN_MANA, _drain_mana_dice()));
        break;
    default:
        default_spell(cmd, res);
    }
}

void earthquake_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Earthquake");
        break;
    case SPELL_DESC:
        var_set_string(res, "The walls will tremble and the ground will shake.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You gain the ability to wreck the dungeon.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You lose the ability to wreck the dungeon.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can bring down the dungeon around your ears.");
        break;
    case SPELL_CAST:
        earthquake(plr->pos, 10);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_earthquake(void) { return cast_spell(earthquake_spell); }

void eat_magic_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Eat Magic");
        break;
    case SPELL_DESC:
        var_set_string(res, "Consumes magical devices to regain spell points.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("Your magic items look delicious.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("Your magic items no longer look delicious.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can consume magic energy for your own use.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (eat_magic(20 + plr->lev * 8 / 5)) /* skillmasters can do this on CL1 ... */
            var_set_bool(res, TRUE);
        break;
    case SPELL_FAIL_MIN:
        var_set_int(res, 11);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_eat_magic(void) { return cast_spell(eat_magic_spell); }

void eat_rock_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Eat Rock");
        break;
    case SPELL_DESC:
        var_set_string(res, "Consumes nearby rock.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("The walls look delicious.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("The walls look unappetizing.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can consume solid rock.");
        break;
    case SPELL_CAST:
    {
        int dir = 0;
        point_t pos;
        dun_cell_ptr cell;
        mon_ptr mon;

        var_set_bool(res, FALSE);

        if (!get_rep_dir2(&dir)) break;

        pos = point_step(plr->pos, dir);
        cell = dun_cell_at(cave, pos);
        stop_mouth();

        if (!cell_is_wall(cell)) /* XXX no longer support doors or trees */
        {
            msg_print("You cannot eat this feature.");
            break;
        }
        else if (cell->flags & CELL_PERM)
        {
            msg_format("Ouch!  This %s is harder than your teeth!",
                cell_desc(cell));
            break;
        }
        mon = dun_mon_at(cave, pos);
        if (mon)
        {
            msg_print("There's something in the way!");
            if (!mon->ml || !mon_is_pet(mon)) plr_attack_normal(pos);
            break;
        }
        else if (wall_is_rubble(cell))
        {
            if (elemental_is_(ELEMENTAL_EARTH))
                set_food(MIN(plr->food + 500, PY_FOOD_MAX - 1));
            else
                set_food(plr->food + 3000);
        }
        else if (wall_is_granite(cell))
        {
            if (elemental_is_(ELEMENTAL_EARTH))
                set_food(MIN(plr->food + 2000, PY_FOOD_MAX - 1));
            else
            {
                msg_format("This %s is very filling!",
                    cell_desc(cell));
                set_food(plr->food + 10000);
            }
        }
        else
        {
            if (elemental_is_(ELEMENTAL_EARTH))
                set_food(MIN(plr->food + 1000, PY_FOOD_MAX - 1));
            else
                set_food(plr->food + 5000);
        }

        if (dun_tunnel(cave, pos, ACTION_FORCE | ACTION_QUIET) == ACTION_SUCCESS)
            move_player_effect(pos, MPE_DONT_PICKUP);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_eat_rock(void) { return cast_spell(eat_rock_spell); }

void evil_bless_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Evil Bless");
        break;
    default:
        bless_spell(cmd, res);
        break;
    }
}

void evocation_spell(int cmd, var_ptr res)
{
    int dam = spell_power(plr->lev * 4 + plr->to_d_spell);
    int power = spell_power(plr->lev * 4);
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Evocation");
        break;
    case SPELL_DESC:
        var_set_string(res, "Dispels, scares and banishes all monsters in view.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, dam));
        break;
    case SPELL_CAST:
        plr_project_los(GF_DISP_ALL, dam);
        plr_project_los(GF_FEAR, power);
        plr_project_los(GF_TELEPORT, power);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void minor_enchantment_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Minor Enchantment");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to enchant a weapon, ammo or armor.");
        break;
    case SPELL_CAST:
        var_set_bool(res, craft_enchant(2 + plr->lev/5, 1));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void enchantment_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Enchantment");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to enchant a weapon, ammo or armor.");
        break;
    case SPELL_CAST:
        var_set_bool(res, craft_enchant(15, 3));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_enchantment(void) { return cast_spell(enchantment_spell); }

static dice_t _enslave_undead_dice(void) { 
    int m = plr->pclass == CLASS_NECROMANCER ? 3 : 1;
    return spell_dice(0, 0, m*plr->lev);
}
void enslave_undead_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Enslave Undead");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to enslave an undead monster.");
        break;
    case SPELL_INFO:
        var_set_string(res, dice_info_power(_enslave_undead_dice()));
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_cast_bolt(GF_CONTROL_UNDEAD, _enslave_undead_dice()));
        break;
    default:
        default_spell(cmd, res);
    }
}

void explosive_rune_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Explosive Rune");
        break;
    case SPELL_DESC:
        var_set_string(res, "Sets a rune which will explode on a passing monster.");
        break;
    case SPELL_CAST:
        msg_print("You carefully set an explosive rune...");
        explosive_rune();
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void eye_for_an_eye_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "An Eye for an Eye");
        break;
    case SPELL_DESC:
        var_set_string(res, "Gives special aura for a while. When you are attacked by a monster, the monster are injured with same amount of damage as you take.");
        break;
    case SPELL_CAST:
        plr_tim_add(T_REVENGE, spell_power(randint1(10) + 10));
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void fire_ball_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Fire Ball");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generate a Fire Ball on chosen target.");
        break;
    default:
        ball_spell(cmd, res, 2, GF_FIRE, 30 + 3*plr->lev/2);
    }
}

void fire_bolt_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Fire Bolt");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a bolt or beam of fire.");
        break;
    default:
        bolt_or_beam_spell(cmd, res, GF_FIRE, 5 + plr->lev/4, 8);
    }
}

void flow_of_lava_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "The Flow of Lava");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generates a ball of fire centered on you which transforms floors to magma.");
        break;
    case SPELL_INFO:
    case SPELL_CAST: {
        dice_t dice = spell_dam_dice(0, 0, 55 + plr->lev);
        if (cmd == SPELL_INFO)
            var_set_string(res, dice_info_dam(dice));
        else {
            plr_burst(3, GF_FIRE, dice_roll(dice));
            plr_ball(3, plr->pos, GF_LAVA_FLOW, 2 + _1d(2));
            var_set_bool(res, TRUE);
        }
        break; }
    default:
        default_spell(cmd, res);
    }
}

void force_branding_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Force Branding");
        break;
    case SPELL_DESC:
        var_set_string(res, "Temporarily brands your weapon with force.");
        break;
    case SPELL_CAST:
    {
        int base = spell_power(plr->lev / 4);
        plr_tim_add(T_BRAND_MANA, base + randint1(base));
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void frost_ball_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Frost Ball");
        break;
    case SPELL_DESC:
        var_set_string(res, "Generate a Frost Ball on chosen target.");
        break;
    default:
        ball_spell(cmd, res, 2, GF_COLD, 25 + 3*plr->lev/2);
    }
}

void frost_bolt_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Frost Bolt");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a bolt or beam of frost.");
        break;
    default:
        bolt_or_beam_spell(cmd, res, GF_COLD, 4 + plr->lev/4, 8);
    }
}

void genocide_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Genocide");
        break;
    case SPELL_DESC:
        var_set_string(res, "Eliminates an entire class of monster, exhausting you. Powerful or unique monsters may resist.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_power(spell_power(plr->lev*3)));
        break;
    case SPELL_CAST:
    {
        int power = spell_power(plr->lev*3);
        var_set_bool(res, symbol_genocide(power, TRUE));
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

void glyph_of_warding_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Glyph of Warding");
        break;
    case SPELL_DESC:
        var_set_string(res, "Sets a glyph on the floor beneath you. Monsters cannot attack you if you are on a glyph, but can try to break glyph.");
        break;
    case SPELL_CAST:
        warding_glyph();
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void grow_mold_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Grow Mold");
        break;
    case SPELL_DESC:
        var_set_string(res, "Surrounds yourself with moldy things.");
        break;
    case SPELL_GAIN_MUT:
        msg_print("You feel a sudden affinity for mold.");
        break;
    case SPELL_LOSE_MUT:
        msg_print("You feel a sudden dislike for mold.");
        break;
    case SPELL_MUT_DESC:
        var_set_string(res, "You can cause mold to grow near you.");
        break;
    case SPELL_CAST:
    {
        int i;
        for (i = 0; i < 8; i++)
        {
            summon_specific(who_create_plr(), plr->pos, plr->lev, SUMMON_BIZARRE1, PM_FORCE_PET);
        }
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
bool cast_grow_mold(void) { return cast_spell(grow_mold_spell); }

