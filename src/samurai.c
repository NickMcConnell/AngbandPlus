#include "angband.h"

#include <assert.h>

/***********************************************************************
 * Samurai Combat
 ***********************************************************************/
enum {
    _HISSATSU_CUSTOM = PLR_HIT_CUSTOM,
    _HISSATSU_SEKIRYUKA,
    _HISSATSU_MINEUCHI,
    _HISSATSU_ZANMA,
    _HISSATSU_UNDEAD,
    _HISSATSU_HAGAN,
    _HISSATSU_SUTEMI,
    _HISSATSU_3DAN,
    _HISSATSU_TWIN_SLASH,
    /* samurai elemental attacks boost existing brands while PLR_HIT_* codes do not */
    _HISSATSU_FIRE,
    _HISSATSU_COLD,
    _HISSATSU_POIS,
    _HISSATSU_ELEC,
    _HISSATSU_ISSEN,
};
static void _mod_damage(plr_attack_ptr context)
{
    if (!context->obj) return; /* don't affect innate attacks or martial arts */
    switch (context->mode)
    {
    case _HISSATSU_ZANMA:
        if (mon_is_living(context->mon) || !mon_is_evil(context->mon))
            context->dam = 0;
        break;
    case _HISSATSU_SEKIRYUKA:
        if (!mon_is_living(context->mon))
            context->dam = 0;
        else if (!plr_tim_find(T_CUT))
            context->dam /= 2;
        break;
    case _HISSATSU_MINEUCHI: {
        int tmp = 10 + randint1(15) + plr->lev / 2;

        context->dam = 0; /* no damage, but monsters don't get a saving throw either */
        anger_monster(context->mon);

        if (_1d(100) > mon_res_pct(context->mon, GF_STUN)) /* XXX don't bypass IMMUNE(STUN) */
            mon_stun(context->mon, tmp);
        else
            msg_format("%s is not effected.", context->mon_name);
        break; }
    case _HISSATSU_SUTEMI:
    case _HISSATSU_3DAN:
        context->dam *= 2;
        break;
    }
    context->dam_drain = context->dam;
}

static slay_t _calc_slay(plr_attack_ptr context, slay_ptr best_slay)
{
    slay_t slay = {0};
    int    mul = MAX(100, best_slay->mul);
    bool   display = BOOL(context->flags & PAC_DISPLAY);

    if (!context->obj) return slay; /* don't affect innate attacks or martial arts */
    switch (context->mode)
    {
    case _HISSATSU_ZANMA:
        if (display || (!mon_is_living(context->mon) && mon_is_evil(context->mon)))
        {
            slay.id = _HISSATSU_ZANMA;
            slay.name = "Zammaken";
            if (mul < 150) slay.mul = 250;
            else mul = MIN(500, mul + 200);
        }
        break;
    case _HISSATSU_UNDEAD:
        if (display)
        {
            slay.id = best_slay->id;
            slay.name = best_slay->name;
            if (best_slay->id == OF_SLAY_UNDEAD || best_slay->id == OF_KILL_UNDEAD)
                slay.mul = MIN(1400, mul + 600);
            else if (best_slay->id)
                slay.mul = MIN(600, mul + 300);
            else
            {
                slay.id = _HISSATSU_UNDEAD;
                slay.name = "Keiun-Kininken";
                slay.mul = 400;
            }
        }
        else
        {
            slay.id = _HISSATSU_UNDEAD;
            if (mon_is_undead(context->mon))
            {
                mon_lore_undead(context->mon);
                slay.mul = MIN(1400, mul + 600);
            }
            else
                slay.mul = MIN(600, mul + 300);
        }
        break;
    case _HISSATSU_SEKIRYUKA:
        if (plr_tim_find(T_CUT) && (display || mon_is_living(context->mon)))
        {
            slay.id = _HISSATSU_SEKIRYUKA;
            slay.name = "Bloody Maelstrom";
            slay.mul = MIN(1000, MAX(100, plr_tim_amount(T_CUT))); /* cf CUT_MORTAL_WOUND */
        }
        break;
    case _HISSATSU_HAGAN:
        if (display || mon_vuln(context->mon, GF_DISINTEGRATE))
        {
            slay.id = _HISSATSU_HAGAN;
            slay.name = "Rock Smash";
            if (!display) mon_lore_resist(context->mon, GF_DISINTEGRATE);
            if (mul == 100) slay.mul = 400;
            else slay.mul = 600;
        }
        break;
    }
    return slay;
}
static slay_t _calc_brand(plr_attack_ptr context, slay_ptr best_brand)
{
    slay_t brand = {0};
    bool   display = BOOL(context->flags & PAC_DISPLAY);
    int    res_pct = 0;
    if (!context->obj) return brand; /* don't affect innate attacks or martial arts */
    switch (context->mode)
    {
    case _HISSATSU_FIRE:
        if (!display) res_pct = mon_res_pct(context->mon, GF_FIRE);
        if (res_pct) mon_lore_resist(context->mon, GF_FIRE);
        if (res_pct <= 0)
        {
            brand.id = OF_BRAND_FIRE;
            brand.name = "Fire";
            brand.mul = 250;
            if (have_flag(context->obj_flags, OF_BRAND_FIRE)) brand.mul = 350;
            if (res_pct < 0) slay_scale(&brand, 100 - res_pct);
        }
        break;
    case _HISSATSU_COLD:
        if (!display) res_pct = mon_res_pct(context->mon, GF_COLD);
        if (res_pct) mon_lore_resist(context->mon, GF_COLD);
        if (res_pct <= 0)
        {
            brand.id = OF_BRAND_COLD;
            brand.name = "Cold";
            brand.mul = 250;
            if (have_flag(context->obj_flags, OF_BRAND_COLD)) brand.mul = 350;
            if (res_pct < 0) slay_scale(&brand, 100 - res_pct);
        }
        break;
    case _HISSATSU_POIS:
        if (!display) res_pct = mon_res_pct(context->mon, GF_POIS);
        if (res_pct) mon_lore_resist(context->mon, GF_POIS);
        if (res_pct <= 0)
        {
            brand.id = OF_BRAND_POIS;
            brand.name = "Poison";
            if (have_flag(context->obj_flags, OF_BRAND_POIS)) brand.mul = 350;
            else brand.mul = 250;
            if (res_pct < 0) slay_scale(&brand, 100 - res_pct);
        }
        break;
    case _HISSATSU_ELEC:
        if (!display) res_pct = mon_res_pct(context->mon, GF_ELEC);
        if (res_pct) mon_lore_resist(context->mon, GF_ELEC);
        if (res_pct <= 0)
        {
            brand.id = OF_BRAND_ELEC;
            brand.name = "Elec";
            if (have_flag(context->obj_flags, OF_BRAND_ELEC)) brand.mul = 700;
            else brand.mul = 500;
            if (res_pct < 0) slay_scale(&brand, 100 - res_pct);
        }
        break;
    }
    return brand;
}

static void _mod_blows(plr_attack_ptr context)
{
    if (!context->obj) return; /* don't affect innate attacks or martial arts */
    switch (context->mode)
    {
    case _HISSATSU_MINEUCHI:
    case _HISSATSU_3DAN:
        context->blow_ct = 1;
        break;
    }
}

static void _begin(plr_attack_ptr context)
{
    switch (context->mode)
    {
    case _HISSATSU_3DAN:
        context->info.crit.freq_add += 1000;
        context->info.crit.qual_add += 650;
        break;
    case PLR_HIT_CRIT:
        context->info.crit.qual_add += 650;
        break;
    case _HISSATSU_SUTEMI:
        context->to_h += 20; /* XXX */
        break;
    }
    if (plr->special_defense & KATA_KOUKIJIN) context->to_h += 50;
}
    
bool _begin_weapon(plr_attack_ptr context)
{
    context->info.info_attr = TERM_L_BLUE;
    switch (context->mode)
    {
    case _HISSATSU_FIRE:
        context->info.info = "Attacks a monster with more damage unless it has resistance to fire.";
        break;
    case _HISSATSU_POIS:
        context->info.info = "Attacks a monster with more damage unless it has resistance to poison.";
        break;
    case _HISSATSU_ZANMA:
        context->info.info = "Attacks an evil unliving monster with great damage. No effect to other monsters.";
        break;
    case _HISSATSU_HAGAN:
        context->info.info = "Breaks rock. Or greatly damage a monster made by rocks.";
        break;
    case _HISSATSU_COLD:
        context->info.xtra_blow += 200;
        context->info.info = "Attacks a monster with increased number of attacks and more damage unless it has resistance to cold.";
        break;
    case PLR_HIT_CRIT:
        context->skill = context->skill * 65 / 100;
        context->info.info = "Attempts to attack with critical hits. But this attack is easy to evade for a monster.";
        break;
    case _HISSATSU_ELEC:
        context->info.info = "Attacks a monster with more damage unless it has resistance to electricity.";
        break;
    case _HISSATSU_SEKIRYUKA:
        context->info.info = "Attacks all adjacent monsters with power corresponding to your cut status. Then increases your cut status. No effect to unliving monsters.";
        break;
    case _HISSATSU_TWIN_SLASH:
        context->info.info = "Attacks with double the number of blows.";
        context->info.base_blow *= 2;
        context->info.xtra_blow *= 2;
        break;
    case _HISSATSU_UNDEAD:
        context->info.info = "Attacks a monster with extremely powerful damage, hurting yourself in the process. Undead monsters are especially hurt by this technique.";
        break;
    }
    return TRUE;
}

static void _end(plr_attack_ptr context)
{
    if (!(context->flags & PAC_DISPLAY) && (plr->special_defense & KATA_IAI))
    {
        if (context->mode != HISSATSU_IAI || context->stop == STOP_MON_DEAD)
            set_action(ACTION_NONE);
    }
}

static void _attack_init(plr_attack_ptr context)
{
    context->hooks.begin_f = _begin;
    context->hooks.begin_weapon_f = _begin_weapon;
    context->hooks.calc_slay_f = _calc_slay;
    context->hooks.calc_brand_f = _calc_brand;
    context->hooks.mod_damage_f = _mod_damage;
    context->hooks.mod_blows_f = _mod_blows;
    context->hooks.end_f = _end;
}
static bool _begin_bow(plr_shoot_ptr context)
{
    if (!(context->flags & PSC_DISPLAY))
    {
        if (plr->special_defense & KATA_MUSOU)
            set_action(ACTION_NONE);
    }
    return TRUE;
}
static void _shoot_init(plr_shoot_ptr context)
{
    context->hooks.begin_bow_f = _begin_bow;
}

/***********************************************************************
 * Samurai Spells
 ***********************************************************************/
static void _tobi_izuna_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Tobi-Izuna");
        break;
    case SPELL_DESC:
        var_set_string(res, "Reaches out to attack a distant monster (Range 2).");
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_attack_ranged(PLR_HIT_NORMAL, PAC_NO_INNATE, 2));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static bool _3_way_attack(point_t pos)
{
    plr_attack_t context = {0};
    context.flags = PAC_ANIMATE;
    return plr_attack(&context, pos);
}
static void _3_way_attack_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "3-Way Attack");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attacks in 3 directions in a single move. Attacks in the entered direction as well as to the left and right of this direction.");
        break;
    case SPELL_CAST: {
        int cdir, dir;
        point_t pos;

        var_set_bool(res, FALSE);
        if (!get_rep_dir2(&dir)) return;
        if (dir == 5) return;

        for (cdir = 0;cdir < 8; cdir++)
        {
            if (cdd[cdir] == dir) break;
        }

        if (cdir == 8) return;

        pos = point_step(plr->pos, cdd[cdir]);
        if (dun_mon_at(cave, pos))
            _3_way_attack(pos);
        else
            msg_print("You attack the empty air.");

        pos = point_step(plr->pos, cdd[(cdir + 7)%8]);
        if (dun_mon_at(cave, pos))
            _3_way_attack(pos);
        else
            msg_print("You attack the empty air.");

        pos = point_step(plr->pos, cdd[(cdir + 1)%8]);
        if (dun_mon_at(cave, pos))
            _3_way_attack(pos);
        else
            msg_print("You attack the empty air.");
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _boomerang_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Boomerang");
        break;
    case SPELL_DESC:
        var_set_string(res, "Throws current weapon. And it'll return to your hand unless failed.");
        break;
    case SPELL_ON_BROWSE:
    case SPELL_CAST: {
        plr_throw_t context = {0};
        context.type = THROW_BOOMERANG;
        context.back_chance = 24 + randint1(5);
        if (cmd == SPELL_CAST)
            var_set_bool(res, plr_throw(&context));
        else
        {
            context.type |= THROW_DISPLAY;
            plr_throw_display(&context);
            var_set_bool(res, TRUE);
        }
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _burning_strike_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Burning Strike");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attacks a monster with more damage unless it has resistance to fire.");
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_attack_special(_HISSATSU_FIRE, 0));
        break;
    case SPELL_ON_BROWSE:
        plr_attack_display_special(_HISSATSU_FIRE, 0);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _detect_ferocity_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Ferocity");
        break;
    case SPELL_DESC:
        var_set_string(res, "Detects all non-mindless monsters in your vicinity.");
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

static void _strike_to_stun_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Strike to Stun");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attacks an adjacent monster with a single blow that stuns, but does no damage.");
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_attack_special(_HISSATSU_MINEUCHI, PAC_NO_INNATE));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _counter_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Counter");
        break;
    case SPELL_DESC:
        var_set_string(res, "For a single turn, you will counter-attack all attacking monsters. This costs SP for each counter-attack.");
        break;
    case SPELL_CAST:
        if (plr->riding)
        {
            msg_print("You cannot do it when riding.");
            var_set_bool(res, FALSE);
        }
        else
        {
            msg_print("You prepare to counter blow.");
            plr->counter = TRUE;
            var_set_bool(res, TRUE);
        }
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _harainuke_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Harainuke");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attacks monster with your weapons normally, then move through counter side of the monster.");
        break;
    case SPELL_CAST: {
        point_t pos;
        dun_cell_ptr cell;
        int dir;

        var_set_bool(res, FALSE);
        if (plr->riding)
        {
            msg_print("You cannot do it when riding.");
            return;
        }

        if (!get_rep_dir2(&dir)) return;

        if (dir == 5) return;
        pos = point_step(plr->pos, dir);

        if (!dun_mon_at(cave, pos))
        {
            msg_print("There is no monster.");
            return;
        }

        plr_attack_normal(pos);

        cell = dun_cell_at(cave, pos);
        if (cell_allow_plr(cell) && !floor_has_known_trap(cell))
        {
            pos = point_step(pos, dir);
            cell = dun_cell_at(cave, pos);
            if (cell_allow_plr(cell) && !floor_has_known_trap(cell) && !dun_mon_at(cave, pos))
            {
                msg_print(NULL);
                move_player_effect(pos, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
            }
        }
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _serpents_tongue_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Serpent's Tongue");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attacks a monster with more damage unless it has resistance to poison.");
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_attack_special(_HISSATSU_POIS, 0));
        break;
    case SPELL_ON_BROWSE:
        plr_attack_display_special(_HISSATSU_POIS, 0);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _zammaken_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Zammaken");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attacks an evil unliving monster with great damage. No effect to other monsters.");
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_attack_special(_HISSATSU_ZANMA, 0));
        break;
    case SPELL_ON_BROWSE:
        plr_attack_display_special(_HISSATSU_ZANMA, 0);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _wind_blast_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Wind Blast");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack an adjacent monster and then blow it away.");
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_attack_special(PLR_HIT_KNOCKBACK, 0));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _judge_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Judge");
        break;
    case SPELL_DESC:
        var_set_string(res, "Identifies a weapon or armor. Or *identifies* these at level 45.");
        break;
    case SPELL_CAST:
        if (plr->lev > 44)
            var_set_bool(res, identify_fully(object_is_weapon_armour_ammo));
        else
            var_set_bool(res, ident_spell(object_is_weapon_armour_ammo));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _rock_smash_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Rock Smash");
        break;
    case SPELL_DESC:
        var_set_string(res, "Breaks rock. Or greatly damage a monster made by rocks.");
        break;
    case SPELL_CAST: {
        int dir;
        point_t pos;

        var_set_bool(res, FALSE);
        if (!get_rep_dir2(&dir)) return;
        if (dir == 5) return;

        pos = point_step(plr->pos, dir);
        if (dun_mon_at(cave, pos))
        {
            plr_attack_t ctx = {0};
            ctx.mode = _HISSATSU_HAGAN;
            plr_attack(&ctx, pos);
        }
        dun_tunnel(cave, pos, ACTION_FORCE | ACTION_QUIET);
        var_set_bool(res, TRUE);
        break; }
    case SPELL_ON_BROWSE:
        plr_attack_display_special(_HISSATSU_HAGAN, 0);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _midare_setsugekka_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Midare-Setsugekka");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attacks a monster with increased number of attacks and more damage unless it has resistance to cold.");
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_attack_special(_HISSATSU_COLD, 0));
        break;
    case SPELL_ON_BROWSE:
        plr_attack_display_special(_HISSATSU_COLD, 0);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _spot_aiming_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Spot Aiming");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to kill a monster instantly. If failed cause only 1HP of damage.");
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_attack_special(PLR_HIT_KILL, PAC_NO_INNATE));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _majingiri_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Majingiri");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to attack with critical hit. But this attack is easy to evade for a monster.");
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_attack_special(PLR_HIT_CRIT, 0));
        break;
    case SPELL_ON_BROWSE:
        plr_attack_display_special(PLR_HIT_CRIT, 0);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _desperate_attack_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Desperate Attack");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attacks with all of your power doing double damage. But all damages you take will be doubled for one turn.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (plr_attack_special(_HISSATSU_SUTEMI, 0))
        {
            plr->sutemi = TRUE;
            var_set_bool(res, TRUE);
        }
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _lightning_eagle_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Lightning Eagle");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attacks a monster with more damage unless it has resistance to electricity.");
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_attack_special(_HISSATSU_ELEC, 0));
        break;
    case SPELL_ON_BROWSE:
        plr_attack_display_special(_HISSATSU_ELEC, 0);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _rush_attack_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Rush Attack");
        break;
    case SPELL_DESC:
        var_set_string(res, "Steps close to a monster and attacks at a time.");
        break;
    case SPELL_CAST:
        var_set_bool(res, rush_attack(5, NULL));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _bloody_maelstrom_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Bloody Maelstrom");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attacks all adjacent monsters with power corresponding to your cut status. Then increases your cut status. No effect to unliving monsters.");
        break;
    case SPELL_CAST: {
        int dir;
        if (!plr->no_cut)
        {
            if (plr_tim_amount(T_CUT) < 300)
                plr_tim_add(T_CUT, 300);
            else
                plr_tim_add(T_CUT, plr_tim_amount(T_CUT));
        } 
        for (dir = 0; dir < 8; dir++)
        {
            point_t      p = point_step(plr->pos, ddd[dir]);
            dun_cell_ptr cell = dun_cell_at(cave, p);
            mon_ptr      mon = dun_mon_at(cave, p);
            
            if (mon && (mon->ml || cell_project(cell)))
            {
                if (!mon_is_living(mon))
                {
                    char m_name[80];
                    monster_desc(m_name, mon, 0);
                    msg_format("%s is unharmed!", m_name);
                }
                else
                {
                    plr_attack_t ctx = {0};
                    ctx.mode = _HISSATSU_SEKIRYUKA;
                    plr_attack(&ctx, p);
                }
            }
        }
        var_set_bool(res, TRUE);
        break; }
    case SPELL_ON_BROWSE:
        plr_attack_display_special(_HISSATSU_SEKIRYUKA, 0);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _earthquake_blow_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Earthquake Blow");
        break;
    case SPELL_DESC:
        var_set_string(res, "Shakes dungeon structure, and results in random swapping of floors and walls.");
        break;
    case SPELL_CAST:
        /* XXX You now need to actually hit an enemy to get the quake ... */
        var_set_bool(res, plr_attack_special(PLR_HIT_QUAKE, 0));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static int _crack_dam(void)
{
    int total_dam = 0, hand;
    for (hand = 0; hand < MAX_HANDS; hand++)
    {
        int dam, basedam;
        u32b flgs[OF_ARRAY_SIZE];
        object_type *o_ptr = NULL;

        if (plr->attack_info[hand].type != PAT_WEAPON) continue;
        o_ptr = equip_obj(plr->attack_info[hand].slot);
        if (!o_ptr) continue; /* paranoia */

        basedam = (o_ptr->dd * (o_ptr->ds + 1)) * 50;
        dam = o_ptr->to_d * 100;
        
        obj_flags(o_ptr, flgs);
        if (have_flag(flgs, OF_VORPAL2))
        {
            basedam *= 5;
            basedam /= 3;
        }
        else if (have_flag(flgs, OF_VORPAL))
        {
            basedam *= 11;
            basedam /= 9;
        }
        dam += basedam;
        dam *= NUM_BLOWS(hand)/100;
        total_dam += dam / 200;
    }
    return total_dam;
}
static dice_t _crack_dice(void) {
    return dice_create(0, 0, _crack_dam());
}
static void _crack_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Crack");
        break;
    case SPELL_DESC:
        var_set_string(res, "Fires a beam of shock wave.");
        break;
    default:
        beam_spell_aux(cmd, res, GF_FORCE, _crack_dice());
    }
}

static void _war_cry_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "War Cry");
        break;
    case SPELL_DESC:
        var_set_string(res, "Damages all monsters in sight with sound. Aggravate nearby monsters.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(1, plr->lev * 3, 0));
        break;
    case SPELL_CAST:
        msg_print("You roar out!");
        plr_project_los(GF_SOUND, randint1(plr->lev * 3));
        aggravate_monsters(who_create_plr());
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _musou_sandan_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Musou-Sandan");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attacks with 3 powerful strikes.");
        break;
    case SPELL_CAST: {
        int i, dir;

        var_set_bool(res, FALSE);
        if (!get_rep_dir2(&dir)) return;
        if (dir == 5) return;

        for (i = 0; i < 3; i++)
        {
            point_t pos = point_step(plr->pos, dir), next_pos;
            plr_attack_t ctx = {0}; /* paranoia: new context for each attack */

            if (!dun_mon_at(cave, pos))
            {
                msg_print("There is no monster.");
                return;
            }

            ctx.mode = _HISSATSU_3DAN;
            ctx.flags = PAC_NO_INNATE;
            if (!plr_attack(&ctx, pos)) return;
            var_set_bool(res, TRUE); /* ok, now the spell counts as being cast */
            if (ctx.stop) return; /* for any reason (e.g. dead, teleported, player fear, etc) */

            next_pos = point_step(pos, dir);

            /* move the monster from pos to next_pos */
            if (!mon_can_enter(ctx.mon, next_pos))
            {
                if (i < 2) msg_print(NULL); /* -more- */
                continue; /* keep attacking! */
            }
            dun_move_mon(cave, ctx.mon, next_pos);

            /* move the player to pos */
            if (!cell_allow_plr(dun_cell_at(cave, pos))) return;
            if (!move_player_effect(pos, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP)) return;
            if (i < 2) msg_print(NULL); /* -more- */
        }
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _vampires_fang_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Vampire's Fang");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attacks with vampiric strikes which absorbs HP from a monster and gives them to you. No effect to unliving monsters.");
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_attack_special(PLR_HIT_VAMP, 0));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _moon_dazzling_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Moon Dazzling");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to stun, confuse and sleep all waking monsters.");
        break;
    case SPELL_CAST:
        msg_print("You irregularly wave your weapon...");
        plr_project_los(GF_ENGETSU, plr->lev * 4);
        plr_project_los(GF_ENGETSU, plr->lev * 4);
        plr_project_los(GF_ENGETSU, plr->lev * 4);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _hundred_slaughter_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Hundred Slaughter");
        break;
    case SPELL_DESC:
        var_set_string(res, "Performs a series of rush attacks. The series continues while killing each monster in a time and SP remains.");
        break;
    case SPELL_CAST: {
        const int mana_cost_per_monster = 8;
        bool mdeath;
        var_set_bool(res, FALSE);
        do
        {
            if (!rush_attack(5, &mdeath)) break;
            var_set_bool(res, TRUE);
            plr->csp -= mana_cost_per_monster;

            if (!mdeath) break;
            command_dir = 0;
            plr->redraw |= PR_MANA;
            handle_stuff();
            /*msg_print(NULL);*/
        }
        while (plr->csp > mana_cost_per_monster);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _dragonic_flash_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Dragonic Flash");
        break;
    case SPELL_DESC:
        var_set_string(res, "Runs toward given location while attacking all monsters on the path.");
        break;
    case SPELL_CAST: {
        point_t pos;

        var_set_bool(res, FALSE);
        pos = target_pos(MAX_SIGHT/2);
        if (!dun_pos_interior(cave, pos)) return;

        if ( !cave_player_teleportable_bold(pos, 0L)
          || point_fast_distance(plr->pos, pos) > MAX_SIGHT/2
          || !point_project(plr->pos, pos) )
        {
            msg_print("You cannot move to that place!");
            return;
        }
        if (plr->anti_tele)
        {
            msg_print("A mysterious force prevents you from teleporting!");
            equip_learn_flag(OF_NO_TELE);
        }
        else
        {
            plr_beam(pos, GF_ATTACK, _HISSATSU_ISSEN);
            teleport_player_to(pos, 0);
        }
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _twin_slash_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Twin Slash");
        break;
    case SPELL_DESC:
        var_set_string(res, "Double attacks at a time.");
        break;
    case SPELL_CAST:
        var_set_bool(res, plr_attack_special(_HISSATSU_TWIN_SLASH, 0));
        break;
    case SPELL_ON_BROWSE:
        plr_attack_display_special(_HISSATSU_TWIN_SLASH, 0);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static int _kofuku_dam(void)
{
    return _crack_dam() * 3 / 2;
}
static void _kofuku_zettousei_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Kofuku-Zettousei");
        break;
    case SPELL_DESC:
        var_set_string(res, "Performs a powerful attack which even effect nearby monsters.");
        break;
    case SPELL_INFO:
        var_set_string(res, info_damage(0, 0, _kofuku_dam()));
        break;
    case SPELL_CAST: {
        int dir, rad = 0;
        point_t pos;

        var_set_bool(res, FALSE);
        if (!get_rep_dir2(&dir)) return;
        if (dir == 5) return;
        var_set_bool(res, TRUE);

        if (cave->flags & DF_NO_MELEE)
        {
            msg_print("Something prevent you from attacking.");
            return;
        }
        pos = point_step(plr->pos, dir);
        if (dun_allow_project_at(cave, pos)) rad = 5;
        msg_print("You swing your weapon downward.");
        plr_ball_direct(rad, pos, GF_METEOR, _kofuku_dam());
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _keiun_kininken_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Keiun-Kininken");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attacks a monster with extremely powerful damage. But you also takes some damages. Hurts a undead monster greatly.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (!plr_attack_special(_HISSATSU_UNDEAD, 0)) return;
        take_hit(DAMAGE_NOESCAPE, 100 + randint1(100), "exhaustion on using Keiun-Kininken");
        var_set_bool(res, TRUE);
        break;
    case SPELL_ON_BROWSE:
        plr_attack_display_special(_HISSATSU_UNDEAD, 0);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _harakiri_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Harakiri");
        break;
    case SPELL_DESC:
        var_set_string(res, "'Busido is found in death'");
        break;
    case SPELL_CAST: {
        int i;
        var_set_bool(res, FALSE);
        if (!get_check("Do you really want to commit suicide? ")) return;
            /* Special Verification for suicide */
        prt("Please verify SUICIDE by typing the '@' sign: ", 0, 0);

        flush();
        i = inkey();
        prt("", 0, 0);
        if (i != '@') return;
        if (plr->total_winner)
        {
            take_hit(DAMAGE_FORCE, 9999, "Seppuku");
            plr->total_winner = TRUE;
        }
        else
        {
            msg_print("Meaning of Bushi-do is found in the death.");
            take_hit(DAMAGE_FORCE, 9999, "Seppuku");
        }
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

/***********************************************************************
 * Samurai Books
 ***********************************************************************/
#define _SPELLS_PER_BOOK 8

typedef struct {
    cptr name;
    spell_info spells[_SPELLS_PER_BOOK];
} book_t;

static book_t _books[4] = {
    { "Bugei Shofu",
        {{ 1, 15,  0, _tobi_izuna_spell},
         { 3, 10,  0, _3_way_attack_spell},
         { 6, 15,  0, _boomerang_spell},
         { 9,  8,  0, _burning_strike_spell},
         {10, 12,  0, _detect_ferocity_spell},
         {12, 25,  0, _strike_to_stun_spell},
         {14,  7,  0, _counter_spell},
         {17, 20,  0, _harainuke_spell}}
    },
    { "Yagyuu Bugeichou",
        {{19, 10,  0, _serpents_tongue_spell},
         {22, 20,  0, _zammaken_spell},
         {24, 30,  0, _wind_blast_spell},
         {25, 10,  0, _judge_spell},
         {27, 15,  0, _rock_smash_spell},
         {29, 45,  0, _midare_setsugekka_spell},
         {32, 70,  0, _spot_aiming_spell},
         {35, 50,  0, _majingiri_spell}}
    },
    { "Gorinnosho",
        {{18, 40,  0, _desperate_attack_spell},
         {22, 22,  0, _lightning_eagle_spell},
         {24, 30,  0, _rush_attack_spell},
         {26, 35,  0, _bloody_maelstrom_spell},
         {30, 30,  0, _earthquake_blow_spell},
         {32, 60,  0, _crack_spell},
         {36, 40,  0, _war_cry_spell},
         {39, 80,  0, _musou_sandan_spell}}
    },
    { "Hokusin Ittouryuu Kaiden",
        {{26, 20,  0, _vampires_fang_spell},
         {29, 40,  0, _moon_dazzling_spell},
         {31, 35,  0, _hundred_slaughter_spell},
         {36, 80,  0, _dragonic_flash_spell},
         {39,100,  0, _twin_slash_spell},
         {42,110,  0, _kofuku_zettousei_spell},
         {45,130,  0, _keiun_kininken_spell},
         {50,255,  0, _harakiri_spell}}
    },
};

/***********************************************************************
 * Gain Spell
 ***********************************************************************/
static int _spell_index(int book, int spell)
{
    return book * _SPELLS_PER_BOOK + spell;
}

static bool _is_spell_known(int book, int spell)
{
    int idx = _spell_index(book, spell);
    if (plr->spell_learned1 & (1L << idx)) return TRUE;
    return FALSE;
}

static void _learn_spell(int book, int spell)
{
    int idx = _spell_index(book, spell);
    int i;

    plr->spell_learned1 |= (1L << idx);

    /* Find the next open entry in "plr->spell_order[]" */
    for (i = 0; i < 64; i++)
    {
        /* Stop at the first empty space */
        if (plr->spell_order[i] == 99) break;
    }

    /* Add the spell to the known list */
    plr->spell_order[i++] = spell;
    plr->learned_spells++;
    plr->update |= PU_SPELLS;
    plr->redraw |= PR_EFFECTS;

    msg_format("You have learned the technique of <color:B>%s</color>.", get_spell_name(_books[book].spells[spell].fn));
}

static bool _gain_spell(int book)
{
    int ct = 0, i;

    /* automatically learn all relevant techniques */
    for (i = 0; i < _SPELLS_PER_BOOK; i++)
    {
        spell_info *src = &_books[book].spells[i];

        if (!_is_spell_known(book, i) && src->level <= plr->lev)
        {
            _learn_spell(book, i);
            ct++;
        }
    }

    if (ct == 0)
    {
        msg_print("You may not learn any techniques in that book.");
        return FALSE;
    }

    return TRUE;
}

static bool _is_samurai_book(obj_ptr obj) { return obj->tval == TV_HISSATSU_BOOK; }
void samurai_gain_spell(void)
{
    obj_prompt_t prompt = {0};

    if (plr->special_defense & (KATA_MUSOU | KATA_KOUKIJIN))
        set_action(ACTION_NONE);

    if (plr_tim_find(T_BLIND) || no_light())
    {
        msg_print("You cannot see!");
        return;
    }
    if (plr_tim_find(T_CONFUSED))
    {
        msg_print("You are too confused!");
        return;
    }
    if (!plr->new_spells)
    {
        msg_print("You cannot learn any new techniques!");
        return;
    }

    prompt.prompt = "Study which book?";
    prompt.error = "You have no books that you can read.";
    prompt.filter = _is_samurai_book;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return;

    if (_gain_spell(prompt.obj->sval))
        energy_use = 100;
}

#define _BROWSE 0x01
static int _get_spells_imp(spell_info* spells, int max, int book, int options)
{
    int ct = 0, i;
    for (i = 0; i < _SPELLS_PER_BOOK; i++)
    {
        spell_info *src, *dest;

        if (ct >= max) break;
        src = &_books[book].spells[i];

        if ((options & _BROWSE) || _is_spell_known(book, i))
        {
            dest = &spells[ct++];
            dest->level = src->level;
            dest->cost = src->cost;
            dest->fail = calculate_fail_rate(
                src->level,
                src->fail,
                plr->stat_ind[A_STR]
            );
            dest->fn = src->fn;
        }
    }
    return ct;
}

void samurai_browse_spell(void)
{
    obj_prompt_t prompt = {0};
    spell_info spells[MAX_SPELLS];
    int ct;

    if (plr_tim_find(T_BLIND) || no_light())
    {
        msg_print("You cannot see!");
        return;
    }
    if (plr_tim_find(T_CONFUSED))
    {
        msg_print("You are too confused!");
        return;
    }

    prompt.prompt = "Browse which book?";
    prompt.error = "You have no books that you can read.";
    prompt.filter = _is_samurai_book;
    prompt.where[0] = INV_PACK;
    prompt.where[1] = INV_FLOOR;

    obj_prompt(&prompt);
    if (!prompt.obj) return;

    ct = _get_spells_imp(spells, MAX_SPELLS, prompt.obj->sval, _BROWSE);
    browse_spells(spells, ct, "technique");
}

static void _book_menu_fn(int cmd, int which, vptr cookie, var_ptr res)
{
    switch (cmd)
    {
    case MENU_TEXT:
        var_set_string(res, _books[which].name);
        break;
    default:
        default_menu(cmd, which, cookie, res);
    }
}

static int _get_spells(spell_info* spells, int max)
{
    int idx = -1;
    int ct = 0;
    menu_t menu = { "Use which book?", NULL, NULL,
                    _book_menu_fn, _books, 4 };

    if (!equip_find_first(obj_is_weapon))
    {
        if (flush_failure) flush();
        msg_print("You need to wield a weapon!");
        return 0;
    }
    if (equip_find_obj(TV_SWORD, SV_POISON_NEEDLE))
    {
        if (flush_failure) flush();
        msg_print("Your weapon is dishonorable!");
        return 0;
    }
    if (!plr->spell_learned1)
    {
        msg_print("You don't know any special attacks.");
        return 0;
    }

    idx = menu_choose(&menu);
    if (idx < 0) return 0;

    ct = _get_spells_imp(spells, max, idx, 0);
    if (ct == 0)
        msg_print("You don't know any of those techniques yet!");
    return ct;
}

static void _character_dump(doc_ptr doc)
{
    int i;

    doc_printf(doc, "<topic:Techniques>================================= <color:keypress>T</color>echniques ==================================\n\n");
    for (i = 0; i < 4; i++)
    {
        spell_info spells[MAX_SPELLS];
        int        ct = _get_spells_imp(spells, MAX_SPELLS, i, 0);
        char       header[100];
        if (ct)
        {
            sprintf(header, "<color:u>%s</color>", _books[i].name);
            plr_display_spells_aux(doc, spells, ct, header);
        }
    }
}


/***********************************************************************
 * Samurai Postures (Kata)
 ***********************************************************************/
void samurai_posture_calc_bonuses(void)
{
    if (plr->special_defense & KATA_FUUJIN)
    {
        /* see project_p for special handling ... review?
        if (!plr->blind)
            plr->reflect = TRUE; */
    }
    if (plr->special_defense & KATA_KOUKIJIN)
    {
        plr->to_a -= 50;
        plr->dis_to_a -= 50;
        res_add_vuln(GF_ACID);
        res_add_vuln(GF_ELEC);
        res_add_vuln(GF_FIRE);
        res_add_vuln(GF_COLD);
    }

    if (plr->special_defense & KATA_MUSOU)
    {
        plr->see_inv++;
        plr->free_act++;
        plr->slow_digest = TRUE;
        plr->regen += 100;
        plr->levitation = TRUE;
        plr->hold_life++;
        plr->sustain_str = TRUE;
        plr->sustain_int = TRUE;
        plr->sustain_wis = TRUE;
        plr->sustain_con = TRUE;
        plr->sustain_dex = TRUE;
        plr->sustain_chr = TRUE;
        plr->telepathy = TRUE;
        plr->weak_lite = TRUE;
        res_add_ultimate();
        plr->reflect = TRUE;
        plr->sh_fire = TRUE;
        plr->sh_elec = TRUE;
        plr->sh_cold = TRUE;
        plr_bonus_ac(100);
    }
}

void samurai_posture_calc_stats(s16b stats[MAX_STATS])
{
    if (plr->special_defense & KATA_KOUKIJIN)
    {
        int i;
        for (i = 0; i < MAX_STATS; i++)
            stats[i] += 5;
    }
}

void samurai_posture_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (plr->special_defense & KATA_FUUJIN)
        add_flag(flgs, OF_REFLECT);

    if (plr->special_defense & KATA_MUSOU)
    {
        add_flag(flgs, OF_RES_(GF_ACID));
        add_flag(flgs, OF_RES_(GF_ELEC));
        add_flag(flgs, OF_RES_(GF_FIRE));
        add_flag(flgs, OF_RES_(GF_COLD));
        add_flag(flgs, OF_RES_(GF_POIS));
        add_flag(flgs, OF_RES_(GF_LIGHT));
        add_flag(flgs, OF_RES_(GF_DARK));
        add_flag(flgs, OF_RES_(GF_CONF));
        add_flag(flgs, OF_RES_(GF_NETHER));
        add_flag(flgs, OF_RES_(GF_NEXUS));
        add_flag(flgs, OF_RES_(GF_SOUND));
        add_flag(flgs, OF_RES_(GF_SHARDS));
        add_flag(flgs, OF_RES_(GF_CHAOS));
        add_flag(flgs, OF_RES_(GF_DISEN));
        add_flag(flgs, OF_RES_(GF_FEAR));
        add_flag(flgs, OF_REFLECT);
        add_flag(flgs, OF_HOLD_LIFE);
        add_flag(flgs, OF_FREE_ACT);
        add_flag(flgs, OF_AURA_FIRE);
        add_flag(flgs, OF_AURA_ELEC);
        add_flag(flgs, OF_AURA_COLD);
        add_flag(flgs, OF_LEVITATION);
        add_flag(flgs, OF_LIGHT);
        add_flag(flgs, OF_SEE_INVIS);
        add_flag(flgs, OF_TELEPATHY);
        add_flag(flgs, OF_SLOW_DIGEST);
        add_flag(flgs, OF_REGEN);
        add_flag(flgs, OF_SUST_STR);
        add_flag(flgs, OF_SUST_INT);
        add_flag(flgs, OF_SUST_WIS);
        add_flag(flgs, OF_SUST_DEX);
        add_flag(flgs, OF_SUST_CON);
        add_flag(flgs, OF_SUST_CHR);
    }

    if (plr->special_defense & KATA_KOUKIJIN)
    {
        add_flag(flgs, OF_VULN_(GF_ACID));
        add_flag(flgs, OF_VULN_(GF_ELEC));
        add_flag(flgs, OF_VULN_(GF_FIRE));
        add_flag(flgs, OF_VULN_(GF_COLD));
    }
}

static bool _choose_kata(void)
{
    char choice;
    int new_kata = 0;
    int i;
    char buf[80];

    if (plr_tim_find(T_CONFUSED))
    {
        msg_print("You are too confused.");
        return FALSE;
    }
    if (plr_tim_find(T_STUN))
    {
        msg_print("You are not clear headed");
        return FALSE;
    }
    if (plr->afraid)
    {
        msg_print("You are trembling with fear!");
        return FALSE;
    }

    screen_save();

    prt(" a) No Form", 2, 20);

    for (i = 0; i < MAX_KATA; i++)
    {
        if (plr->lev >= kata_shurui[i].min_level)
        {
            sprintf(buf," %c) Form of %-12s  %s",I2A(i+1), kata_shurui[i].desc, kata_shurui[i].info);
            prt(buf, 3+i, 20);
        }
    }

    prt("", 1, 0);
    prt("        Choose Form: ", 1, 14);

    for(;;)
    {
        choice = inkey();

        if (choice == ESCAPE)
        {
            screen_load();
            return FALSE;
        }
        else if ((choice == 'a') || (choice == 'A'))
        {
            if (plr->action == ACTION_KATA)
                set_action(ACTION_NONE);
            else
                msg_print("You are not assuming posture.");

            screen_load();
            return TRUE;
        }
        else if ((choice == 'b') || (choice == 'B'))
        {
            new_kata = 0;
            break;
        }
        else if (((choice == 'c') || (choice == 'C')) && (plr->lev > 29))
        {
            new_kata = 1;
            break;
        }
        else if (((choice == 'd') || (choice == 'D')) && (plr->lev > 34))
        {
            new_kata = 2;
            break;
        }
        else if (((choice == 'e') || (choice == 'E')) && (plr->lev > 39))
        {
            new_kata = 3;
            break;
        }
    }
    set_action(ACTION_KATA);

    if (plr->special_defense & (KATA_IAI << new_kata))
    {
        msg_print("You reassume a posture.");
    }
    else
    {
        plr->special_defense &= ~(KATA_MASK);
        plr->update |= (PU_BONUS);
        plr->update |= (PU_MONSTERS);
        msg_format("You assume a posture of %s form.",kata_shurui[new_kata].desc);
        plr->special_defense |= (KATA_IAI << new_kata);
    }
    plr->redraw |= (PR_STATE);
    plr->redraw |= (PR_STATUS);
    screen_load();
    return TRUE;
}

static int _max_sp(void)
{
    return MAX(plr->msp*4, plr->lev*5+5);
}

void cast_concentration(void)
{
    int max_csp = _max_sp();
    if (plr_pet_count())
        return;
    if (plr->special_defense & KATA_MASK)
        return;
        
    msg_print("You concentrate to charge your power.");

    plr->csp += plr->msp / 2;
    if (plr->csp >= max_csp)
    {
        plr->csp = max_csp;
        plr->csp_frac = 0;
    }

    plr->redraw |= (PR_MANA);
 }

void samurai_concentration_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Concentration");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_CAST:
    {
        var_set_bool(res, FALSE);
        if (plr_pet_count())
        {
            msg_print("You need concentration on the pets now.");
            return;
        }
        if (plr->special_defense & KATA_MASK)
        {
            msg_print("You need concentration on your form.");
            return;
        }

        cast_concentration();
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _posture_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Assume a Guard Position");
        break;
    case SPELL_DESC:
        var_set_string(res, "");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (!equip_find_first(obj_is_weapon))
        {
            msg_print("You need to wield a weapon.");
            return;
        }
        if (!_choose_kata()) return;

        plr->update |= (PU_BONUS);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _calc_bonuses(void)
{
    samurai_posture_calc_bonuses();
    if (plr->lev >= 30)
        res_add(GF_FEAR);
}

static void _calc_stats(s16b stats[MAX_STATS])
{
    samurai_posture_calc_stats(stats);
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    samurai_posture_get_flags(flgs);
    if (plr->lev >= 30)
        add_flag(flgs, OF_RES_(GF_FEAR));
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 1;
    spell->cost = 0;
    spell->fail = 0;
    spell->fn = samurai_concentration_spell;

    spell = &spells[ct++];
    spell->level = 25;
    spell->cost = 0;
    spell->fail = 0;
    spell->fn = _posture_spell;

    return ct;
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "technique";
        me.which_stat = A_WIS;
        me.encumbrance.max_wgt = 3000;
        me.encumbrance.weapon_pct = 0;
        me.encumbrance.enc_wgt = 1200;
        me.options = CASTER_SUPERCHARGE_MANA;
        me.realm1_choices = CH_HISSATSU;
        init = TRUE;
    }
    return &me;
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_SWORD, SV_KATANA, 1);
    plr_birth_obj_aux(TV_HARD_ARMOR, SV_CHAIN_MAIL, 1);
    plr_birth_spellbooks();
}

static void _timer_on(plr_tim_ptr timer)
{
    switch (timer->id)
    {
    case T_HALLUCINATE:
    case T_PARALYZED:
        plr->counter = FALSE;
        break;
    case T_CONFUSED:
    case T_STUN:
        plr->counter = FALSE;
        if (plr->action == ACTION_KATA)
        {
            msg_print("Your posture gets loose.");
            plr->special_defense &= ~(KATA_MASK);
            plr->update |= PU_BONUS;
            plr->update |= PU_MONSTERS;
            plr->redraw |= PR_STATE;
            plr->redraw |= PR_STATUS;
            plr->action = ACTION_NONE;
        }
        break;
    }
}

plr_class_ptr samurai_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  18,  32,   2,  16,   6,  70,  40};
    skills_t xs = { 60,  35,  50,   0,   0,   0, 115,  90};


        me = plr_class_alloc(CLASS_SAMURAI);
        me->name = "Samurai";
        me->desc = "Samurai, masters of the art of the blade, are the next strongest "
                    "fighters after Warriors, and can use various special combat "
                    "techniques. Samurai are not good at most other skills, and many "
                    "magical devices may be too difficult for them to use. Wisdom "
                    "determines a Samurai's ability to use the special combat "
                    "techniques available to him.\n \n"
                    "Samurai use the art of the blade called Kendo (or Bugei). Books "
                    "of Kendo are similar to spellbooks, but Samurai don't need to "
                    "carry them around; the books are needed only when they study new "
                    "combat techniques. Samurai need a weapon wielded to use the "
                    "techniques of Kendo, and most techniques will add powerful special "
                    "properties to their blows; such as flaming, poisoning, vampiric, "
                    "etc... Their maximum spellpoints don't depend on their level but "
                    "solely on wisdom, and they can use the class power 'Concentration' "
                    "to temporarily increase SP beyond its usual maximum value. They "
                    "have one more class power - 'Assume a Posture'. They can choose "
                    "different forms of posture in different situations.";

        me->stats[A_STR] =  3;
        me->stats[A_INT] = -2;
        me->stats[A_WIS] =  1;
        me->stats[A_DEX] =  2;
        me->stats[A_CON] =  1;
        me->stats[A_CHR] =  1;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 111;
        me->base_hp = 12;
        me->exp = 130;
        me->pets = 40;
        me->flags = CLASS_SENSE1_FAST | CLASS_SENSE1_STRONG;

        me->hooks.birth = _birth;
        me->hooks.get_spells = _get_spells;
        me->hooks.caster_info = _caster_info;
        me->hooks.attack_init = _attack_init;
        me->hooks.shoot_init = _shoot_init;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.calc_stats = _calc_stats;
        me->hooks.get_flags = _get_flags;
        me->hooks.get_powers = _get_powers;
        me->hooks.character_dump = _character_dump;
        me->hooks.timer_on = _timer_on;
    }

    return me;
}
