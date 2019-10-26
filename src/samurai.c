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
        if (monster_living(context->race) || !(context->race->flags3 & RF3_EVIL))
            context->dam = 0;
        break;
    case _HISSATSU_SEKIRYUKA:
        if (!monster_living(context->race))
            context->dam = 0;
        else if (!plr_tim_find(T_CUT))
            context->dam /= 2;
        break;
    case _HISSATSU_MINEUCHI: {
        int tmp = 10 + randint1(15) + p_ptr->lev / 5;

        context->dam = 0; /* no damage, but monsters don't get a saving throw either */
        anger_monster(context->mon);

        if (!(context->race->flags3 & RF3_NO_STUN))
        {
            mon_stun(context->mon, tmp);
        }
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
                mon_lore_3(context->mon, RF3_UNDEAD);
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
            slay.mul = MIN(1000, MAX(100, plr_tim_amount(T_CUT)));
        }
        break;
    case _HISSATSU_HAGAN:
        if (display || (context->race->flags3 & RF3_HURT_ROCK))
        {
            slay.id = _HISSATSU_HAGAN;
            slay.name = "Rock Smash";
            if (!display) mon_lore_3(context->mon, RF3_HURT_ROCK);
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
    if (!context->obj) return brand; /* don't affect innate attacks or martial arts */
    switch (context->mode)
    {
    case _HISSATSU_FIRE:
        if (!display && mon_res_fire(context->mon))
            mon_lore_res_fire(context->mon);
        else
        {
            brand.id = OF_BRAND_FIRE;
            brand.name = "Fire";
            brand.mul = 250;
            if (have_flag(context->obj_flags, OF_BRAND_FIRE)) brand.mul = 350;
            if (!display && mon_vuln_fire(context->mon))
            {
                mon_lore_vuln_fire(context->mon);
                brand.mul *= 2;
            }
        }
        break;
    case _HISSATSU_COLD:
        if (!display && mon_res_cold(context->mon))
            mon_lore_res_cold(context->mon);
        else
        {
            brand.id = OF_BRAND_COLD;
            brand.name = "Cold";
            brand.mul = 250;
            if (have_flag(context->obj_flags, OF_BRAND_COLD)) brand.mul = 350;
            if (!display && mon_vuln_cold(context->mon))
            {
                mon_lore_vuln_cold(context->mon);
                brand.mul *= 2;
            }
        }
        break;
    case _HISSATSU_POIS:
        if (!display && mon_res_pois(context->mon))
            mon_lore_res_pois(context->mon);
        else
        {
            brand.id = OF_BRAND_POIS;
            brand.name = "Poison";
            if (have_flag(context->obj_flags, OF_BRAND_POIS)) brand.mul = 350;
            else brand.mul = 250;
        }
        break;
    case _HISSATSU_ELEC:
        if (!display && mon_res_elec(context->mon))
            mon_lore_res_elec(context->mon);
        else
        {
            brand.id = OF_BRAND_ELEC;
            brand.name = "Elec";
            if (have_flag(context->obj_flags, OF_BRAND_ELEC)) brand.mul = 700;
            else brand.mul = 500;
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
    case _HISSATSU_COLD:
        context->blow_ct += 2;
        break;
    case _HISSATSU_MINEUCHI:
    case _HISSATSU_3DAN:
        context->blow_ct = 1;
        break;
    case _HISSATSU_TWIN_SLASH:
        context->blow_ct *= 2;
        break;
    }
}
static bool _check_hit(plr_attack_ptr context)
{
    bool hit = plr_check_hit(context);
    if (context->mode == PLR_HIT_CRIT && one_in_(3))
        hit = FALSE;
    return hit;
}

static void _begin(plr_attack_ptr context)
{
    switch (context->mode)
    {
    case _HISSATSU_3DAN:
        p_ptr->crit_freq_add += 1000;
        p_ptr->crit_qual_add += 650;
        break;
    case PLR_HIT_CRIT:
        p_ptr->crit_qual_add += 650;
        break;
    case _HISSATSU_SUTEMI:
        context->to_h += 20; /* XXX */
        break;
    }
    if (p_ptr->special_defense & KATA_KOUKIJIN) context->to_h += 50;
}

static void _end(plr_attack_ptr context)
{
    switch (context->mode)
    {
    case _HISSATSU_3DAN:
        p_ptr->crit_freq_add -= 1000;
        p_ptr->crit_qual_add -= 650;
        break;
    case PLR_HIT_CRIT:
        p_ptr->crit_qual_add -= 650;
        break;
    }

    if (p_ptr->special_defense & KATA_IAI)
    {
        if (context->mode != HISSATSU_IAI || context->stop == STOP_MON_DEAD)
            set_action(ACTION_NONE);
    }
}

static void _attack_init(plr_attack_ptr context)
{
    context->hooks.begin_f = _begin;
    context->hooks.calc_slay_f = _calc_slay;
    context->hooks.calc_brand_f = _calc_brand;
    context->hooks.mod_damage_f = _mod_damage;
    context->hooks.mod_blows_f = _mod_blows;
    context->hooks.check_hit_f = _check_hit;
    context->hooks.end_f = _end;
}

/***********************************************************************
 * Samurai Spells (Hissatsu)
 ***********************************************************************/
cptr do_hissatsu_spell(int spell, int mode)
{
    bool name = BOOL(mode == SPELL_NAME);
    bool desc = BOOL(mode == SPELL_DESC);
    bool cast = BOOL(mode == SPELL_CAST);
    bool browse = BOOL(mode == SPELL_ON_BROWSE);

    int dir;
    int plev = p_ptr->lev;

    switch (spell)
    {
    case 0:
        if (name) return "Tobi-Izuna";
        if (desc) return "Attacks a two squares distant monster.";
        if (browse) return NULL;
        if (cast && !plr_attack_ranged(PLR_HIT_NORMAL, PAC_NO_INNATE, 2)) return NULL;
        break;

    case 1:
        if (name) return "3-Way Attack";
        if (desc) return "Attacks in 3 directions in one time. Attacks in entered direction as well as to the left and right of this direction.";
        if (browse) return NULL;
        if (cast)
        {
            int cdir, dir;
            point_t pos;

            if (!get_rep_dir2(&dir)) return NULL;
            if (dir == 5) return NULL;

            for (cdir = 0;cdir < 8; cdir++)
            {
                if (cdd[cdir] == dir) return NULL;
            }

            if (cdir == 8) return NULL;

            pos = point_step(p_ptr->pos, cdd[cdir]);
            if (mon_at(pos))
                plr_attack_normal(pos);
            else
                msg_print("You attack the empty air.");

            pos = point_step(p_ptr->pos, cdd[(cdir + 7)%8]);
            if (mon_at(pos))
                plr_attack_normal(pos);
            else
                msg_print("You attack the empty air.");

            pos = point_step(p_ptr->pos, cdd[(cdir + 1)%8]);
            if (mon_at(pos))
                plr_attack_normal(pos);
            else
                msg_print("You attack the empty air.");
        }
        break;

    case 2:
        if (name) return "Boomerang";
        if (desc) return "Throws current weapon. And it'll return to your hand unless failed.";
        if (browse) return NULL;
        if (cast)
        {
            plr_throw_t context = {0};
            context.type = THROW_BOOMERANG;
            context.back_chance = 24 + randint1(5);
            if (!plr_throw(&context)) return NULL;
        }
        break;

    case 3:
        if (name) return "Burning Strike";
        if (desc) return "Attacks a monster with more damage unless it has resistance to fire.";
        if (browse) plr_attack_display_special(_HISSATSU_FIRE, 0);
        if (cast && !plr_attack_special(_HISSATSU_FIRE, 0)) return NULL;
        break;

    case 4:
        if (name) return "Detect Ferocity";
        if (desc) return "Detects all monsters except mindless in your vicinity.";
        if (browse) return NULL;
        if (cast) detect_monsters_mind(DETECT_RAD_DEFAULT);
        break;

    case 5:
        if (name) return "Strike to Stun";
        if (desc) return "Attempts to stun a monster in the adjacent.";
        if (browse) return NULL;
        if (cast && !plr_attack_special(_HISSATSU_MINEUCHI, PAC_NO_INNATE)) return NULL;
        break;

    case 6:
        if (name) return "Counter";
        if (desc) return "Prepares to counterattack. When attack by a monster, strikes back using SP each time.";
        if (browse) return NULL;
        if (cast)
        {
            if (p_ptr->riding)
            {
                msg_print("You cannot do it when riding.");
                return NULL;
            }
            msg_print("You prepare to counter blow.");
            p_ptr->counter = TRUE;
        }
        break;

    case 7:
        if (name) return "Harainuke";
        if (desc) return "Attacks monster with your weapons normally, then move through counter side of the monster.";
        if (browse) return NULL;
        if (cast)
        {
            point_t pos;
            cave_ptr grid;
            if (p_ptr->riding)
            {
                msg_print("You cannot do it when riding.");
                return NULL;
            }
    
            if (!get_rep_dir2(&dir)) return NULL;
    
            if (dir == 5) return NULL;
            pos = point_step(p_ptr->pos, dir);
    
            if (!mon_at(pos))
            {
                msg_print("There is no monster.");
                return NULL;
            }
    
            plr_attack_normal(pos);

            grid = cave_at(pos);
            if (!player_can_enter(grid->feat, 0) || grid->feat)
                break;
    
            pos = point_step(pos, dir);
            grid = cave_at(pos);
            if (player_can_enter(grid->feat, 0) && !is_trap(grid->feat) && !mon_at(pos))
            {
                msg_print(NULL);
                move_player_effect(pos, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP);
            }
        }
        break;

    case 8:
        if (name) return "Serpent's Tongue";
        if (desc) return "Attacks a monster with more damage unless it has resistance to poison.";
        if (browse) plr_attack_display_special(_HISSATSU_POIS, 0);
        if (cast && !plr_attack_special(_HISSATSU_POIS, 0)) return NULL;
        break;

    case 9:
        if (name) return "Zammaken";
        if (desc) return "Attacks an evil unliving monster with great damage. No effect to other monsters.";
        if (browse) plr_attack_display_special(_HISSATSU_ZANMA, 0);
        if (cast && !plr_attack_special(_HISSATSU_ZANMA, 0)) return NULL;
        break;

    case 10:
        if (name) return "Wind Blast";
        if (desc) return "Attacks an adjacent monster, and blow it away.";
        if (browse) return NULL;
        if (cast && !plr_attack_special(PLR_HIT_KNOCKBACK, 0)) return NULL;
        break;

    case 11:
        if (name) return "Judge";
        if (desc) return "Identifies a weapon or armor. Or *identifies* these at level 45.";
        if (browse) return NULL;
        if (cast)
        {
            if (plev > 44)
            {
                if (!identify_fully(object_is_weapon_armour_ammo)) return NULL;
            }
            else
            {
                if (!ident_spell(object_is_weapon_armour_ammo)) return NULL;
            }
        }
        break;

    case 12:
        if (name) return "Rock Smash";
        if (desc) return "Breaks rock. Or greatly damage a monster made by rocks.";
        if (browse) return NULL;
        if (cast)
        {
            point_t pos;

            if (!get_rep_dir2(&dir)) return NULL;
            if (dir == 5) return NULL;

            pos = point_step(p_ptr->pos, dir);

            if (mon_at(pos))
            {
                plr_attack_t ctx = {0};
                ctx.mode = _HISSATSU_HAGAN;
                plr_attack(&ctx, pos);
            }
    
            if (!cave_have_flag_at(pos, FF_HURT_ROCK)) break;

            cave_alter_feat(pos.y, pos.x, FF_HURT_ROCK);
            p_ptr->update |= PU_FLOW;
        }
        break;

    case 13:
        if (name) return "Midare-Setsugekka";
        if (desc) return "Attacks a monster with increased number of attacks and more damage unless it has resistance to cold.";
        if (browse) plr_attack_display_special(_HISSATSU_COLD, 0);
        if (cast && !plr_attack_special(_HISSATSU_COLD, 0)) return NULL;
        break;

    case 14:
        if (name) return "Spot Aiming";
        if (desc) return "Attempts to kill a monster instantly. If failed cause only 1HP of damage.";
        if (browse) return NULL;
        if (cast && !plr_attack_special(PLR_HIT_KILL, PAC_NO_INNATE)) return NULL;
        break;

    case 15:
        if (name) return "Majingiri";
        if (desc) return "Attempts to attack with critical hit. But this attack is easy to evade for a monster.";
        if (browse) plr_attack_display_special(PLR_HIT_CRIT, 0);
        if (cast && !plr_attack_special(PLR_HIT_CRIT, 0)) return NULL;
        break;

    case 16:
        if (name) return "Desperate Attack";
        if (desc) return "Attacks with all of your power. But all damages you take will be doubled for one turn.";
        if (browse) return NULL;
        if (cast)
        {
            if (plr_attack_special(_HISSATSU_SUTEMI, 0))
                p_ptr->sutemi = TRUE;
        }
        break;

    case 17:
        if (name) return "Lightning Eagle";
        if (desc) return "Attacks a monster with more damage unless it has resistance to electricity.";
        if (cast && !plr_attack_special(_HISSATSU_ELEC, 0)) return NULL;
        if (browse) plr_attack_display_special(_HISSATSU_ELEC, 0);
        break;

    case 18:
        if (name) return "Rush Attack";
        if (desc) return "Steps close to a monster and attacks at a time.";
        if (browse) return NULL;
        if (cast)
        {
            if (!rush_attack(5, NULL)) return NULL;
        }
        break;

    case 19:
        if (name) return "Bloody Maelstrom";
        if (desc) return "Attacks all adjacent monsters with power corresponding to your cut status. Then increases your cut status. No effect to unliving monsters.";
        if (browse) plr_attack_display_special(_HISSATSU_SEKIRYUKA, 0);
    
        if (cast)
        {
            if (!p_ptr->no_cut)
            {
                if (plr_tim_amount(T_CUT) < 300)
                    plr_tim_add(T_CUT, 300);
                else
                    plr_tim_add(T_CUT, plr_tim_amount(T_CUT));
            } 
            for (dir = 0; dir < 8; dir++)
            {
                point_t p = point_step(p_ptr->pos, ddd[dir]);
                mon_ptr mon = mon_at(p);
                
                if (mon && (mon->ml || cave_have_flag_at(p, FF_PROJECT)))
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
        }
        break;

    case 20:
        if (name) return "Earthquake Blow";
        if (desc) return "Shakes dungeon structure, and results in random swapping of floors and walls.";
        if (browse) return NULL;
        /* XXX You now need to actually hit an enemy to get the quake ... */
        if (cast && !plr_attack_special(PLR_HIT_QUAKE, 0)) return NULL;
        break;

    case 21:
        if (name) return "Crack";
        if (desc) return "Fires a beam of shock wave.";
        if (browse) return NULL;
        if (cast)
        {
            int total_damage = 0, hand;
            if (!get_fire_dir(&dir)) return NULL;
            msg_print("You swing your weapon downward.");

            for (hand = 0; hand < MAX_HANDS; hand++)
            {
                int damage, basedam;
                u32b flgs[OF_ARRAY_SIZE];
                object_type *o_ptr = NULL;

                if (p_ptr->attack_info[hand].type != PAT_WEAPON) continue;
                o_ptr = equip_obj(p_ptr->attack_info[hand].slot);
                if (!o_ptr) continue; /* paranoia */

                basedam = (o_ptr->dd * (o_ptr->ds + 1)) * 50;
                damage = o_ptr->to_d * 100;
                
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
                damage += basedam;
                damage *= NUM_BLOWS(hand)/100;
                total_damage += damage / 200;
            }
            fire_beam(GF_FORCE, dir, total_damage);
        }
        break;

    case 22:
        if (name) return "War Cry";
        if (desc) return "Damages all monsters in sight with sound. Aggravate nearby monsters.";
        if (browse) return NULL;
    
        if (cast)
        {
            msg_print("You roar out!");
            project_los(GF_SOUND, randint1(plev * 3));
            aggravate_monsters(0);
        }
        break;

    case 23:
        if (name) return "Musou-Sandan";
        if (desc) return "Attacks with powerful 3 strikes.";
        if (browse) return NULL;
    
        if (cast)
        {
            int i;
            cptr result = NULL; /* spell counts as cancelled */

            if (!get_rep_dir2(&dir)) return result;
            if (dir == 5) return result;

            for (i = 0; i < 3; i++)
            {
                point_t pos = point_step(p_ptr->pos, dir), next_pos;
                cave_type *c_ptr;
                plr_attack_t ctx = {0}; /* paranoia: new context for each attack */
    
                c_ptr = cave_at(pos);
    
                if (!mon_at(pos))
                {
                    msg_print("There is no monster.");
                    return result;
                }

                ctx.mode = _HISSATSU_3DAN;
                ctx.flags = PAC_NO_INNATE;
                if (!plr_attack(&ctx, pos)) return result;
                result = ""; /* ok, now the spell counts as being cast */
                if (ctx.stop) return result; /* for any reason (e.g. dead, teleported, player fear, etc) */
    
                next_pos = point_step(pos, dir);
    
                /* move the monster from pos to next_pos */
                if (!monster_can_enter(next_pos.y, next_pos.x, ctx.race, 0))
                {
                    if (i < 2) msg_print(NULL); /* -more- */
                    continue; /* keep attacking! */
                }
                dun_move_mon(cave, ctx.mon, next_pos);
    
                /* move the player to pos */
                if (!player_can_enter(c_ptr->feat, 0)) return result;
                if (!move_player_effect(pos, MPE_FORGET_FLOW | MPE_HANDLE_STUFF | MPE_DONT_PICKUP)) return result;
                if (i < 2) msg_print(NULL); /* -more- */
            }
        }
        break;

    case 24:
        if (name) return "Vampire's Fang";
        if (desc) return "Attacks with vampiric strikes which absorbs HP from a monster and gives them to you. No effect to unliving monsters.";
        if (browse) return NULL;
        if (cast && !plr_attack_special(PLR_HIT_VAMP, 0)) return NULL;
        break;

    case 25:
        if (name) return "Moon Dazzling";
        if (desc) return "Attempts to stun, confuse and sleep all waking monsters.";
        if (browse) return NULL;
    
        if (cast)
        {
            msg_print("You irregularly wave your weapon...");
            project_los(GF_ENGETSU, plev * 4);
            project_los(GF_ENGETSU, plev * 4);
            project_los(GF_ENGETSU, plev * 4);
        }
        break;

    case 26:
        if (name) return "Hundred Slaughter";
        if (desc) return "Performs a series of rush attacks. The series continues while killing each monster in a time and SP remains.";
        if (browse) return NULL;
    
        if (cast)
        {
            const int mana_cost_per_monster = 8;
            bool new = TRUE;
            bool mdeath;

            do
            {
                if (!rush_attack(5, &mdeath)) break;
                if (new)
                {
                    /* Reserve needed mana point */
                    p_ptr->csp -= technic_info[REALM_HISSATSU - MIN_TECHNIC][26].smana;
                    new = FALSE;
                }
                else
                    p_ptr->csp -= mana_cost_per_monster;

                if (!mdeath) break;
                command_dir = 0;

                p_ptr->redraw |= PR_MANA;
                handle_stuff();
            }
            while (p_ptr->csp > mana_cost_per_monster);

            if (new) return NULL;
    
            /* Restore reserved mana */
            p_ptr->csp += technic_info[REALM_HISSATSU - MIN_TECHNIC][26].smana;
        }
        break;

    case 27:
        if (name) return "Dragonic Flash";
        if (desc) return "Runs toward given location while attacking all monsters on the path.";
        if (browse) return NULL;
    
        if (cast)
        {
            int y, x;

            if (!tgt_pt(&x, &y, MAX_SIGHT / 2)) return NULL;

            if (!cave_player_teleportable_bold(y, x, 0L) ||
                (distance(y, x, p_ptr->pos.y, p_ptr->pos.x) > MAX_SIGHT / 2) ||
                !projectable(p_ptr->pos.y, p_ptr->pos.x, y, x))
            {
                msg_print("You cannot move to that place!");
                break;
            }
            if (p_ptr->anti_tele)
            {
                msg_print("A mysterious force prevents you from teleporting!");
                equip_learn_flag(OF_NO_TELE);
                break;
            }
            project(0, 0, y, x, _HISSATSU_ISSEN, GF_ATTACK, PROJECT_BEAM | PROJECT_KILL);
            teleport_player_to(y, x, 0L);
        }
        break;

    case 28:
        if (name) return "Twin Slash";
        if (desc) return "double attacks at a time.";
        if (browse) return NULL;
        if (cast && !plr_attack_special(_HISSATSU_TWIN_SLASH, 0)) return NULL;
        break;

    case 29:
        if (name) return "Kofuku-Zettousei";
        if (desc) return "Performs a powerful attack which even effect nearby monsters.";
        if (browse) return NULL;
        if (cast)
        {
            int total_damage = 0, hand;
            int y, x;
    
            if (!get_rep_dir2(&dir)) return NULL;
            if (dir == 5) return NULL;

            y = p_ptr->pos.y + ddy[dir];
            x = p_ptr->pos.x + ddx[dir];

            if (cave->flags & DF_NO_MELEE)
            {
                msg_print("Something prevent you from attacking.");
                return "";
            }
            msg_print("You swing your weapon downward.");
            for (hand = 0; hand < MAX_HANDS; hand++)
            {
                int damage, basedam;
                u32b flgs[OF_ARRAY_SIZE];
                object_type *o_ptr = NULL;

                if (p_ptr->attack_info[hand].type != PAT_WEAPON) continue;
                o_ptr = equip_obj(p_ptr->attack_info[hand].slot);
                if (!o_ptr) continue; /* paranoia */

                basedam = (o_ptr->dd * (o_ptr->ds + 1)) * 50;
                damage = o_ptr->to_d * 100;
                
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
                damage += basedam;
                damage += p_ptr->attack_info[hand].to_d * 100;
                damage *= NUM_BLOWS(hand)/100;
                total_damage += damage / 200;
            }
            project(0, (cave_have_flag_bold(y, x, FF_PROJECT) ? 5 : 0), y, x, total_damage * 3 / 2, GF_METEOR, PROJECT_KILL | PROJECT_JUMP | PROJECT_ITEM);
        }
        break;

    case 30:
        if (name) return "Keiun-Kininken";
        if (desc) return "Attacks a monster with extremely powerful damage. But you also takes some damages. Hurts a undead monster greatly.";
        if (browse) plr_attack_display_special(_HISSATSU_UNDEAD, 0);
    
        if (cast)
        {
            if (!plr_attack_special(_HISSATSU_UNDEAD, 0)) return NULL;
            take_hit(DAMAGE_NOESCAPE, 100 + randint1(100), "exhaustion on using Keiun-Kininken");
        }
        break;

    case 31:
        if (name) return "Harakiri";
        if (desc) return "'Busido is found in death'";
        if (browse) return NULL;

        if (cast)
        {
            int i;
            if (!get_check("Do you really want to commit suicide? ")) return NULL;
                /* Special Verification for suicide */
            prt("Please verify SUICIDE by typing the '@' sign: ", 0, 0);
    
            flush();
            i = inkey();
            prt("", 0, 0);
            if (i != '@') return NULL;
            if (p_ptr->total_winner)
            {
                take_hit(DAMAGE_FORCE, 9999, "Seppuku");
                p_ptr->total_winner = TRUE;
            }
            else
            {
                msg_print("Meaning of Bushi-do is found in the death.");
                take_hit(DAMAGE_FORCE, 9999, "Seppuku");
            }
        }
        break;
    }

    return "";
}

/***********************************************************************
 * Samurai Postures (Kata)
 ***********************************************************************/
void samurai_posture_calc_bonuses(void)
{
    if (p_ptr->special_defense & KATA_FUUJIN)
    {
        /* see project_p for special handling ... review?
        if (!p_ptr->blind)
            p_ptr->reflect = TRUE; */
    }
    if (p_ptr->special_defense & KATA_KOUKIJIN)
    {
        p_ptr->to_a -= 50;
        p_ptr->dis_to_a -= 50;
        res_add_vuln(RES_ACID);
        res_add_vuln(RES_ELEC);
        res_add_vuln(RES_FIRE);
        res_add_vuln(RES_COLD);
    }

    if (p_ptr->special_defense & KATA_MUSOU)
    {
        p_ptr->see_inv++;
        p_ptr->free_act++;
        p_ptr->slow_digest = TRUE;
        p_ptr->regen += 100;
        p_ptr->levitation = TRUE;
        p_ptr->hold_life++;
        p_ptr->sustain_str = TRUE;
        p_ptr->sustain_int = TRUE;
        p_ptr->sustain_wis = TRUE;
        p_ptr->sustain_con = TRUE;
        p_ptr->sustain_dex = TRUE;
        p_ptr->sustain_chr = TRUE;
        p_ptr->telepathy = TRUE;
        p_ptr->lite = TRUE;
        res_add_ultimate();
        p_ptr->reflect = TRUE;
        p_ptr->sh_fire = TRUE;
        p_ptr->sh_elec = TRUE;
        p_ptr->sh_cold = TRUE;
        plr_bonus_ac(100);
    }
}

void samurai_posture_calc_stats(s16b stats[MAX_STATS])
{
    if (p_ptr->special_defense & KATA_KOUKIJIN)
    {
        int i;
        for (i = 0; i < MAX_STATS; i++)
            stats[i] += 5;
    }
}

void samurai_posture_get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (p_ptr->special_defense & KATA_FUUJIN)
        add_flag(flgs, OF_REFLECT);

    if (p_ptr->special_defense & KATA_MUSOU)
    {
        add_flag(flgs, OF_RES_FEAR);
        add_flag(flgs, OF_RES_LITE);
        add_flag(flgs, OF_RES_DARK);
        add_flag(flgs, OF_RES_BLIND);
        add_flag(flgs, OF_RES_CONF);
        add_flag(flgs, OF_RES_SOUND);
        add_flag(flgs, OF_RES_SHARDS);
        add_flag(flgs, OF_RES_NETHER);
        add_flag(flgs, OF_RES_NEXUS);
        add_flag(flgs, OF_RES_CHAOS);
        add_flag(flgs, OF_RES_DISEN);
        add_flag(flgs, OF_RES_TIME);
        add_flag(flgs, OF_REFLECT);
        add_flag(flgs, OF_HOLD_LIFE);
        add_flag(flgs, OF_FREE_ACT);
        add_flag(flgs, OF_AURA_FIRE);
        add_flag(flgs, OF_AURA_ELEC);
        add_flag(flgs, OF_AURA_COLD);
        add_flag(flgs, OF_LEVITATION);
        add_flag(flgs, OF_LITE);
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

    if (p_ptr->special_defense & KATA_KOUKIJIN)
    {
        add_flag(flgs, OF_VULN_ACID);
        add_flag(flgs, OF_VULN_ELEC);
        add_flag(flgs, OF_VULN_FIRE);
        add_flag(flgs, OF_VULN_COLD);
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
    if (p_ptr->afraid)
    {
        msg_print("You are trembling with fear!");
        return FALSE;
    }

    screen_save();

    prt(" a) No Form", 2, 20);

    for (i = 0; i < MAX_KATA; i++)
    {
        if (p_ptr->lev >= kata_shurui[i].min_level)
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
            if (p_ptr->action == ACTION_KATA)
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
        else if (((choice == 'c') || (choice == 'C')) && (p_ptr->lev > 29))
        {
            new_kata = 1;
            break;
        }
        else if (((choice == 'd') || (choice == 'D')) && (p_ptr->lev > 34))
        {
            new_kata = 2;
            break;
        }
        else if (((choice == 'e') || (choice == 'E')) && (p_ptr->lev > 39))
        {
            new_kata = 3;
            break;
        }
    }
    set_action(ACTION_KATA);

    if (p_ptr->special_defense & (KATA_IAI << new_kata))
    {
        msg_print("You reassume a posture.");
    }
    else
    {
        p_ptr->special_defense &= ~(KATA_MASK);
        p_ptr->update |= (PU_BONUS);
        p_ptr->update |= (PU_MONSTERS);
        msg_format("You assume a posture of %s form.",kata_shurui[new_kata].desc);
        p_ptr->special_defense |= (KATA_IAI << new_kata);
    }
    p_ptr->redraw |= (PR_STATE);
    p_ptr->redraw |= (PR_STATUS);
    screen_load();
    return TRUE;
}

static int _max_sp(void)
{
    return MAX(p_ptr->msp*4, p_ptr->lev*5+5);
}

void cast_concentration(void)
{
    int max_csp = _max_sp();
    if (total_friends)
        return;
    if (p_ptr->special_defense & KATA_MASK)
        return;
        
    msg_print("You concentrate to charge your power.");

    p_ptr->csp += p_ptr->msp / 2;
    if (p_ptr->csp >= max_csp)
    {
        p_ptr->csp = max_csp;
        p_ptr->csp_frac = 0;
    }

    p_ptr->redraw |= (PR_MANA);
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
        if (total_friends)
        {
            msg_print("You need concentration on the pets now.");
            return;
        }
        if (p_ptr->special_defense & KATA_MASK)
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

        p_ptr->update |= (PU_BONUS);
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
    if (p_ptr->lev >= 30)
        res_add(RES_FEAR);
}

static void _calc_stats(s16b stats[MAX_STATS])
{
    samurai_posture_calc_stats(stats);
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    samurai_posture_get_flags(flgs);
    if (p_ptr->lev >= 30)
        add_flag(flgs, OF_RES_FEAR);
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
        p_ptr->counter = FALSE;
        break;
    case T_CONFUSED:
    case T_STUN:
        p_ptr->counter = FALSE;
        if (p_ptr->action == ACTION_KATA)
        {
            msg_print("Your posture gets loose.");
            p_ptr->special_defense &= ~(KATA_MASK);
            p_ptr->update |= PU_BONUS;
            p_ptr->update |= PU_MONSTERS;
            p_ptr->redraw |= PR_STATE;
            p_ptr->redraw |= PR_STATUS;
            p_ptr->action = ACTION_NONE;
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
    skills_t xs = { 12,   7,  10,   0,   0,   0,  23,  18};


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
        me->hooks.caster_info = _caster_info;
        me->hooks.attack_init = _attack_init;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.calc_stats = _calc_stats;
        me->hooks.get_flags = _get_flags;
        me->hooks.get_powers = _get_powers;
        me->hooks.character_dump = spellbook_character_dump;
        me->hooks.timer_on = _timer_on;
    }

    return me;
}
