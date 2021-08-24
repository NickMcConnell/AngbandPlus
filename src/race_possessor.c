#include "angband.h"

#include <assert.h>

/***********************************************************************
 Possession History: Track the most recently assumed forms for the Possessor
 and Mimic races. Report these on the character dump.

 TODO: Free memory on quit().
 ***********************************************************************/
#define _HISTORY_MAX 1000

/* Special Codes for dungeon_type */
#define DUNGEON_WILD   0
#define DUNGEON_TOWN  -1   /* lvl gives which town */
#define DUNGEON_QUEST -2   /* lvl gives which quest */

typedef struct _history_s _history_t;
typedef struct _history_s *_history_ptr;

struct _history_s
{
    sym_t r_idx;
    int d_idx;
    int d_lvl;
    int p_lvl;
    int turn;

    _history_ptr next;
};

static _history_ptr _history = NULL;

static int _history_count(void)
{
    int          ct = 0;
    _history_ptr p = _history;
    
    while (p)
    {
        ct++;
        p = p->next;
    }
    
    return ct;
}

static void _history_clear(void)
{
    _history_ptr p = _history;
    while (p)
    {
        _history = p->next;
        free(p);
        p = _history;
    }
}

static void _history_on_birth(void)
{
    _history_clear();
}

static void _history_on_possess(int r_idx)
{
    _history_ptr p = malloc(sizeof(_history_t));
    if (p)
    {
        p->r_idx = r_idx;

        if (!plr_in_dungeon())
        {
            if (dun_world_town_id())
            {
                p->d_idx = DUNGEON_TOWN;
                p->d_lvl = dun_world_town_id();
            }
            else if (plr_on_surface())
            {
                p->d_idx = DUNGEON_WILD;
                p->d_lvl = cave->difficulty;
            }
            else if (quests_get_current())
            {
                p->d_idx = DUNGEON_QUEST;
                p->d_lvl = quests_get_current()->id;
            }
            else /* ??? */
            {
                p->d_idx = DUNGEON_WILD;
                p->d_lvl = cave->difficulty;
            }
        }
        else
        {
            p->d_idx = cave->type->id;
            p->d_lvl = cave->dun_lvl;
        }

        p->p_lvl = plr->lev;
        p->turn = dun_mgr()->turn;

        p->next = _history;
        _history = p;
    }
}

static void _history_on_load(savefile_ptr file)
{
    int          ct = savefile_read_s32b(file);
    int          i;
    _history_ptr c, t, lst = NULL;

    _history_clear();
    for (i = 0; i < ct; i++)
    {
        c = malloc(sizeof(_history_t));
        assert(c);

        if (savefile_is_older_than(file, 7, 3, 3, 1))
            c->r_idx = savefile_read_s32b(file);
        else
            c->r_idx = savefile_read_sym(file);
        c->d_idx = savefile_read_s32b(file);
        c->d_lvl = savefile_read_s32b(file);
        c->p_lvl = savefile_read_s32b(file);
        c->turn  = savefile_read_s32b(file);

        c->next = lst;
        lst = c;
    }

    /* Reverse List */
    while (lst)
    {
        t = lst;
        lst = lst->next;
        t->next = _history;
        _history = t;
    }
}

static void _history_on_save(savefile_ptr file)
{
    _history_ptr p = _history;
    int          ct = MIN(_HISTORY_MAX, _history_count());
    int          i = 0;

    savefile_write_s32b(file, ct);
    while (i < ct)
    {
        assert(p);
        savefile_write_sym(file, p->r_idx);
        savefile_write_s32b(file, p->d_idx);
        savefile_write_s32b(file, p->d_lvl);
        savefile_write_s32b(file, p->p_lvl);
        savefile_write_s32b(file, p->turn);
        p = p->next;
        i++;
    }
}

/***********************************************************************
 ...
 ***********************************************************************/
static int _calc_level(int l)
{
    return l + l*l*l/2500;
}

void possessor_on_birth(void)
{
    _history_on_birth();
}

static void _birth(void) 
{ 
    object_type forge;

    possessor_on_birth();

    plr_mon_race_set("@.soul");

    object_prep(&forge, lookup_kind(TV_WAND, SV_ANY));
    if (device_init_fixed(&forge, EFFECT_BOLT_COLD))
        plr_birth_obj(&forge);

    object_prep(&forge, lookup_kind(TV_RING, 0));
    forge.name2 = EGO_RING_COMBAT;
    forge.to_d = 3;
    add_flag(forge.flags, OF_MELEE);
    plr_birth_obj(&forge);

    plr_birth_food();
    plr_birth_light();
}

static int _get_toggle(void)
{
    return plr->magic_num1[0];
}

int possessor_get_toggle(void)
{
    int result = TOGGLE_NONE;
    if (plr->prace == RACE_MON_POSSESSOR || plr->prace == RACE_MON_MIMIC)
        result = _get_toggle();
    return result;
}

static void _player_action(void)
{
    if (_get_toggle() == LEPRECHAUN_TOGGLE_BLINK)
        teleport_player(10, TELEPORT_LINE_OF_SIGHT);
}

int possessor_max_plr_lvl(int r_idx)
{
    monster_race *r_ptr = mon_race_lookup(r_idx);
    point_t       tbl[5] = { {10, 15}, {20, 30}, {30, 37}, {40, 45}, {50, 50} };
    return        interpolate(r_ptr->alloc.lvl, tbl, 5);
}

static int _max_lvl(void)
{
    if (sym_equals(plr->current_r_idx, "@.mimic")) return 50;
    return possessor_max_plr_lvl(plr->current_r_idx);
}

/**********************************************************************
 * Attacks
 **********************************************************************/
static int _dam_boost(int method, int rlev)
{
    /* Most early monsters with innate attacks aren't worth possessing as
       their damage is just too low ... Heck, a Mean Looking Mercenary with
       a good longsword is usually a much better option! */
    switch (method)
    {
    case RBM_HIT:
    case RBM_PUNCH:
    case RBM_KICK:
    case RBM_CLAW:
    case RBM_BITE:
    case RBM_STING:
    case RBM_SLASH:
    case RBM_BUTT:
    case RBM_CRUSH:
        return  2 + (rlev + 4) / 5;
    }
    return 0;
}
void possessor_calc_innate_attacks(void)
{
    mon_race_ptr race = plr_mon_race();
    int          i;

    for (i = 0; i < vec_length(race->blows); i++)
    {
        mon_blow_ptr blow = vec_get(race->blows, i);
        int          xtra;
        /* Monk: The 'Monk' body type disallows both weapons and shields, and should
         * always allow martial arts. The 'Standard' body type should mask martial arts
         * if a weapon is used, or if the player is dual wielding shields (Monastic Lich) */
        if ( blow->method == RBM_MONK
          && equip_has_slot_type(EQUIP_SLOT_WEAPON_SHIELD) )
        {
            /* no martial arts with weapons */
            if (plr->weapon_ct) continue;
            /* martial arts requires a free hand (e.g. duel wielding shields) */
            if (!equip_is_empty_hand(0) && !equip_is_empty_hand(1)) continue;
        }
        blow = mon_blow_copy(blow);
        xtra = _dam_boost(blow->method, race->alloc.lvl);
        if (xtra && blow->effect_ct)
            blow->effects[0].dice.base += xtra;
        blow->flags |= MBF_POSSESSOR; /* for skills_innate_calc_name() */
        vec_add(plr->innate_blows, blow);
    }
}

/**********************************************************************
 * Spells
 **********************************************************************/
void possessor_cast(void)
{
    mon_race_ptr race = plr_mon_race();
    if (!race->spells)
    {
        msg_print("Your current body has no spells.");
        return;
    }
    if (plr_tim_find(T_CONFUSED))
    {
        msg_print("You are too confused.");
        return;
    }
    if (mon_spell_cast_possessor(race))
        energy_use = 100;
}

int possessor_antimagic_prob(void)
{
    mon_race_ptr race = plr_mon_race();
    if (!mon_race_has_noninnate_spell(race))
        return 0;
    /* XXX Look for spellcaster forms? */
    return 100;
}

/**********************************************************************
 * Possession
 **********************************************************************/
static bool _obj_can_possess(object_type *o_ptr)
{
    return o_ptr->tval == TV_CORPSE && o_ptr->sval == SV_CORPSE;
}
static void _possess_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Possess");
        break;
    case SPELL_DESC:
        var_set_string(res, "Enter the corpse of a new body, gaining the powers and abilities of that form.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("Lvl %d", _calc_level(plr->max_plv) + 5));
        break;
    case SPELL_CAST:
    {
        obj_prompt_t  prompt = {0};
        monster_race *r_ptr;
        char          name[MAX_NLEN];

        var_set_bool(res, FALSE);

        if (!plr_mon_race_is_("@.soul"))
        {
            msg_print("You must leave your current body first. Be careful!");
            return;
        }

        /*if ( plr->current_r_idx != MON_POSSESSOR_SOUL 
          && !get_check("Your current body may be destroyed. Are you sure? ") )
        {
            return;
        }*/

        prompt.prompt = "Possess which corpse?";
        prompt.error = "You have nothing to possess.";
        prompt.filter = _obj_can_possess;
        prompt.where[0] = INV_PACK;
        prompt.where[1] = INV_FLOOR;

        obj_prompt(&prompt);
        if (!prompt.obj) return;
        r_ptr = mon_race_lookup(prompt.obj->race_id);

        object_desc(name, prompt.obj, OD_NAME_ONLY | OD_SINGULAR);
        if (r_ptr->alloc.lvl > _calc_level(plr->max_plv) + 5)
        {
            msg_format("You are not powerful enough to possess %s (Lvl %d).",
                name, r_ptr->alloc.lvl);
            return;
        }

        msg_format("You possess %s.", name);
        if (!plr_mon_race_is_("@.soul"))
        {
            if (plr->lev <= 10 || one_in_(3))
            {
                object_type forge;
                object_prep(&forge, lookup_kind(TV_CORPSE, SV_CORPSE));
                forge.race_id = plr->current_r_idx;
                forge.weight = MIN(500*10, MAX(40, plr_mon_race()->weight * 10));
                drop_near(&forge, plr->pos, -1);
            }
            else
                msg_print("Your previous body quickly decays!");
        }

        possessor_set_current_r_idx(prompt.obj->race_id);
        stats_on_use(prompt.obj, 1);
        prompt.obj->number--;
        obj_release(prompt.obj, 0);
        var_set_bool(res, TRUE);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}
static void _unpossess_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Unpossess");
        break;
    case SPELL_DESC:
        var_set_string(res, "Leave your current body, returning to your native form. Your current body may be destroyed in the process.");
        break;
    case SPELL_CAST:
    {
        var_set_bool(res, FALSE);
        if (plr_mon_race_is_("@.soul")) return; /* paranoia */

        if (get_check("Your current body may be destroyed. Are you sure? "))
        {
            int old_r_idx = plr->current_r_idx;
            monster_race *old_r_ptr = mon_race_lookup(old_r_idx);

            msg_print("You leave your current body!");
            if (plr->lev <= 10 || one_in_(3))
            {
                object_type forge;
                object_prep(&forge, lookup_kind(TV_CORPSE, SV_CORPSE));
                forge.race_id = old_r_idx;
                forge.weight = MIN(500*10, MAX(40, old_r_ptr->weight * 10));
                drop_near(&forge, plr->pos, -1);
            }
            else
                msg_print("Your previous body quickly decays!");

            possessor_set_current_r_idx(mon_race_parse("@.soul")->id);
            var_set_bool(res, TRUE);
        }
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _add_power(spell_info* spell, int lvl, int cost, int fail, ang_spell fn, int stat_idx)
{
    int l = MIN(_max_lvl(), lvl); /* It's frustrating when a corpse can *never* use a given power ... */
    spell->level = l;
    spell->cost = cost;
    spell->fail = calculate_fail_rate(l, fail, stat_idx); 
    spell->fn = fn;
}

int possessor_get_powers(spell_info* spells, int max)
{
    mon_race_ptr race = plr_mon_race();
    int          ct = 0;
    if (ct < max && mon_race_is_trump(race))
        _add_power(&spells[ct++], 1, 0, 0, blink_toggle_spell, plr->stat_ind[A_DEX]);
    if (ct < max && (race->body.class_id == CLASS_MAGE || race->body.class_id == CLASS_HIGH_MAGE || race->body.class_id == CLASS_SORCERER))
        _add_power(&spells[ct++], 25, 1, 90, eat_magic_spell, plr->stat_ind[A_INT]);
    return ct;
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    if (/*plr->current_r_idx == MON_POSSESSOR_SOUL &&*/ ct < max)
        _add_power(&spells[ct++], 1, 0, 0, _possess_spell, plr->stat_ind[A_DEX]);
    if (!plr_mon_race_is_("@.soul") && ct < max)
        _add_power(&spells[ct++], 1, 0, 0, _unpossess_spell, plr->stat_ind[A_DEX]);

    ct += possessor_get_powers(spells + ct, max - ct);
    return ct;
}

caster_info *possessor_caster_info(void)
{
    static caster_info info = {0};
    mon_race_ptr race = plr_mon_race();
    int stat = race->body.spell_stat;

    /* This is a hack since the mimic's default class
       normally lacks mana. But if we do this, then every time the
       mimic assumes a magical form, they will start with 0sp! */
    if (sym_equals(plr->current_r_idx, "@.mimic"))
    {
        info.which_stat = stat;
        info.magic_desc = "power";
        info.options = 0;
        info.encumbrance.max_wgt = 450;
        info.encumbrance.weapon_pct = 0;
        info.encumbrance.enc_wgt = 800;
        return &info;
    }

    /* Try to use the pseudo-class caster info */
    if (race->body.class_id && race->spells)
    {
        class_t *class_ptr = get_class_aux(race->body.class_id, 0);
        if (class_ptr && class_ptr->hooks.caster_info)
        {
            info = *class_ptr->hooks.caster_info();
            info.which_stat = stat; /* r_info can now override the default spell stat */
            /*XXX Why? I suppose that many giant forms have HEALING
             * and cast as 'Maulers'. Giving a HP based healing spell
             * would probably be broken! */
            if (info.options & CASTER_USE_HP)
            {
                info.options &= ~CASTER_USE_HP;
                /* Most CASTER_USE_HP info ignores encumbrance */
                info.encumbrance.max_wgt = 3000;
                info.encumbrance.weapon_pct = 0;
                info.encumbrance.enc_wgt = 1200;
            }
            if (info.options & CASTER_SUPERCHARGE_MANA) /* many forms cast as 'Mystics', but cannot 'Concentrate'! */
                info.options &= ~CASTER_SUPERCHARGE_MANA;
            return &info;
        }
    }

    /* If that fails, we'll still give the plr mana in some cases. Currently,
     * only 'innate' spells can be cast using hp. */
    if ( mon_race_needs_mana(race)
      && (stat == A_INT || stat == A_WIS || stat == A_CHR) )
    {
        info.which_stat = stat;
        info.magic_desc = "power";
        info.options = 0;
        info.encumbrance.max_wgt = 450;
        info.encumbrance.weapon_pct = 0;
        info.encumbrance.enc_wgt = 800;
        return &info;
    }
    return NULL;
}

/**********************************************************************
 * Bonuses
 * TODO: Someday, we should have a unified flag system ... sigh.
 **********************************************************************/
static int ac_percent;

static void _ac_bonus_imp(int slot)
{
    object_type *o_ptr = equip_obj(slot);
    if (o_ptr)
    {
        switch (equip_slot_type(slot))
        {
        case EQUIP_SLOT_BODY_ARMOR:
            ac_percent -= 25;
            break;
        case EQUIP_SLOT_CLOAK:
            ac_percent -= 5;
            break;
        case EQUIP_SLOT_WEAPON_SHIELD:
            if (obj_is_shield(o_ptr))
                ac_percent -= 15;
            break;
        case EQUIP_SLOT_HELMET:
            ac_percent -= 7;
            break;
        case EQUIP_SLOT_GLOVES:
            ac_percent -= 5;
            break;
        case EQUIP_SLOT_BOOTS:
            ac_percent -= 5;
            break;
        }
    }
}

int possessor_r_speed(int r_idx)
{
    monster_race *r_ptr = mon_race_lookup(r_idx);
    int           sp;
    int           r_lvl = MAX(1, r_ptr->alloc.lvl);
    int           p_lvl = _calc_level(plr->lev);

    if (heavy_armor()) return 0;

    if (r_ptr->body.speed)
        sp = r_ptr->body.speed;
    else
    {
        sp = r_ptr->move.speed;
        if (sp > 0)
        {
            int i;
            equip_template_ptr body = equip_template_lookup(r_ptr->body.body_id);
            bool humanoid = FALSE;

            for (i = 1; i <= body->max; i++)
            {
                if (body->slots[i].type == EQUIP_SLOT_WEAPON_SHIELD)
                {
                    humanoid = TRUE;
                    break;
                }
            }

            if (humanoid)
            {
                int factor = 35;
                int tsp = sp * 10;
                sp = 0;
                while (tsp > 0)
                {
                    if (tsp >= 100)
                        sp += factor;
                    else
                        sp += tsp * factor / 100;
 
                    factor /= 2;
                    tsp -= 100;
                }
                sp = sp/10;
            }
        }
    }
    sp = sp * MIN(p_lvl, r_lvl) / r_lvl;
    return sp;
}

int possessor_r_ac(int r_idx)
{
    monster_race *r_ptr = mon_race_lookup(r_idx);
    int           ac = 0;
    int           r_lvl = MAX(1, r_ptr->alloc.lvl);
    int           p_lvl = _calc_level(plr->lev);

    if ((r_ptr->body.flags & RF_POS_GAIN_AC) && !heavy_armor())
    {
        ac = r_ptr->ac * MIN(p_lvl, r_lvl) / r_lvl;

        /* Reduce AC bonus a bit depending on what armor slots are available.
           For example, Wahha-man has AC200 yet can also wear a full complement of armor! */        
        ac_percent = 100;
        equip_for_each_slot(_ac_bonus_imp);
        ac = ac * ac_percent / 100;
    }
    return MAX(0, ac);
}

void possessor_calc_weapon_bonuses(obj_ptr obj, plr_attack_info_ptr info)
{
    monster_race *r_ptr = plr_mon_race();
    if (r_ptr->body.blows_calc.max)
        info->blows_calc.max = r_ptr->body.blows_calc.max;
    if (r_ptr->body.blows_calc.wgt)
        info->blows_calc.wgt = r_ptr->body.blows_calc.wgt;
    if (r_ptr->body.blows_calc.mul)
        info->blows_calc.mul = r_ptr->body.blows_calc.mul;
}

void possessor_calc_shooter_bonuses(object_type *o_ptr, plr_shoot_info_ptr info_ptr)
{
    if (plr->current_r_idx && !plr->shooter_info.heavy_shoot)
    {
        monster_race *r_ptr = plr_mon_race();

        if ( r_ptr->body.class_id == CLASS_RANGER
          && plr->shooter_info.tval_ammo != TV_ARROW )
        {
            plr->shooter_info.base_shot = 100;
        }
        if ( r_ptr->body.class_id == CLASS_ROGUE
          && plr->shooter_info.tval_ammo != TV_SHOT )
        {
            plr->shooter_info.base_shot = 100;
        }
    }
}

void possessor_calc_bonuses(void) 
{
    monster_race *r_ptr = plr_mon_race();
    int i;

    if (!plr->current_r_idx) /* Birth hack ... we haven't been "born" yet! */
        return;

    if (mon_race_is_female(r_ptr) && plr->psex != SEX_FEMALE)
        plr->psex = SEX_FEMALE;

    if (mon_race_is_male(r_ptr) && plr->psex != SEX_MALE)
        plr->psex = SEX_MALE;

    if (!equip_can_wield_kind(TV_LIGHT, SV_LIGHT_FEANOR))
        plr->see_nocto = DUN_VIEW_MAX;

    {
        int to_a = possessor_r_ac(plr->current_r_idx);
        plr->to_a += to_a;
        plr->dis_to_a += to_a;
    }

    plr->pspeed += possessor_r_speed(plr->current_r_idx);
    if (r_ptr->move.flags & RFM_QUICK)
        plr->quick_walk = TRUE;

    plr->align += r_ptr->align;

    if (r_ptr->body.flags & RF_POS_HOLD_LIFE)
        plr->hold_life++;
    if (r_ptr->body.flags & RF_POS_TELEPATHY)
    {
        plr->telepathy = TRUE;
        if (mon_race_is_(r_ptr, "p.grand master mindcrafter")) /* an elite form! */
        {
            assert(r_ptr->body.flags & RF_POS_TELEPATHY);
            plr->wizard_sight = TRUE;
            plr->clear_mind = TRUE;
        }
    }
    if (r_ptr->body.flags & RF_POS_SEE_INVIS)
        plr->see_inv++;
    if (mon_race_is_invisible(r_ptr))
        plr->see_inv++;
    if (r_ptr->body.flags & RF_POS_BACKSTAB)
    {
        plr->ambush = 300;
        plr->backstab = 150;
    }

    if (r_ptr->body.flags & RF_POS_SUST_STR)
        plr->sustain_str = TRUE;
    if (r_ptr->body.flags & RF_POS_SUST_INT)
        plr->sustain_int = TRUE;
    if (r_ptr->body.flags & RF_POS_SUST_WIS)
        plr->sustain_wis = TRUE;
    if (r_ptr->body.flags & RF_POS_SUST_DEX)
        plr->sustain_dex = TRUE;
    if (r_ptr->body.flags & RF_POS_SUST_CON)
        plr->sustain_con = TRUE;
    if (r_ptr->body.flags & RF_POS_SUST_CHR)
        plr->sustain_chr = TRUE;

    if (mon_race_can_reflect(r_ptr))
        plr->reflect = TRUE;
    if (mon_race_can_regen(r_ptr))
        plr->regen += 100;
    if (mon_race_is_horror(r_ptr) || mon_race_is_char_ex(r_ptr, "GLUVW"))
        plr->no_eldritch = TRUE;
    if (mon_race_can_passwall(r_ptr))
    {
        plr->pass_wall = TRUE;
        plr->no_passwall_dam = TRUE;
    }
    if (mon_race_can_tunnel(r_ptr))
        plr->kill_wall = TRUE;
    if (mon_race_can_passweb(r_ptr))
        plr->pass_web = TRUE;
    if (mon_race_can_clearweb(r_ptr))
        plr->clear_web = TRUE;
    if (mon_race_can_retaliate(r_ptr) && !heavy_armor())
        plr->sh_retaliation = TRUE;
    if (mon_race_projects_fear(r_ptr))
        plr->sh_fear = TRUE;

    /* resists */
    for (i = GF_RES_MIN; i <= GF_RES_MAX; i++)
    {
        if (mon_race_immune(r_ptr, i))
            res_add_immune(i);
        else if (mon_race_vuln(r_ptr, i))
            res_add_vuln(i);
        else if (mon_race_resist(r_ptr, i))
            res_add(i);
    }
    if (mon_race_immune(r_ptr, GF_SLEEP))
        plr->free_act++;

    if (mon_race_can_fly(r_ptr))
        plr->levitation = TRUE;

    plr->self_lite += r_ptr->light;

    if (mon_race_is_char_ex(r_ptr, "sGLVWz"))
        plr->no_cut = TRUE;

    if (mon_blows_find(r_ptr->blows, RBM_MONK))
        plr->monk_lvl = MAX(1, MIN(50, r_ptr->alloc.lvl));
}

void possessor_get_flags(u32b flgs[OF_ARRAY_SIZE]) 
{
    monster_race *r_ptr = plr_mon_race();
    int i;

    if (r_ptr->move.speed > 0)
        add_flag(flgs, OF_SPEED);
    if (r_ptr->move.speed < 0)
        add_flag(flgs, OF_DEC_SPEED);

    if (r_ptr->body.flags & RF_POS_HOLD_LIFE)
        add_flag(flgs, OF_HOLD_LIFE);
    if (r_ptr->body.flags & RF_POS_TELEPATHY)
        add_flag(flgs, OF_TELEPATHY);
    if (r_ptr->body.flags & RF_POS_SEE_INVIS)
        add_flag(flgs, OF_SEE_INVIS);
    if (r_ptr->body.flags & RF_POS_SUST_STR)
        add_flag(flgs, OF_SUST_STR);
    if (r_ptr->body.flags & RF_POS_SUST_INT)
        add_flag(flgs, OF_SUST_INT);
    if (r_ptr->body.flags & RF_POS_SUST_WIS)
        add_flag(flgs, OF_SUST_WIS);
    if (r_ptr->body.flags & RF_POS_SUST_DEX)
        add_flag(flgs, OF_SUST_DEX);
    if (r_ptr->body.flags & RF_POS_SUST_CON)
        add_flag(flgs, OF_SUST_CON);
    if (r_ptr->body.flags & RF_POS_SUST_CHR)
        add_flag(flgs, OF_SUST_CHR);

    if (mon_race_can_reflect(r_ptr))
        add_flag(flgs, OF_REFLECT);
    if (mon_race_can_regen(r_ptr))
        add_flag(flgs, OF_REGEN);

    if (mon_race_can_retaliate(r_ptr))
        add_flag(flgs, OF_AURA_REVENGE);

    if (mon_race_can_fly(r_ptr))
        add_flag(flgs, OF_LEVITATION);
    if (r_ptr->light > 0)
        add_flag(flgs, OF_LIGHT);
    if (r_ptr->light < 0)
        add_flag(flgs, OF_DARKNESS);

    /* resists */
    for (i = GF_RES_MIN; i <= GF_RES_MAX; i++)
    {
        if (mon_race_immune(r_ptr, i))
            add_flag(flgs, OF_IM_(i));
        else if (mon_race_vuln(r_ptr, i))
            add_flag(flgs, OF_VULN_(i));
        else if (mon_race_resist(r_ptr, i))
            add_flag(flgs, OF_RES_(i));
    }
    if (mon_race_immune(r_ptr, GF_SLEEP))
        add_flag(flgs, OF_FREE_ACT);
}

/**********************************************************************
 * Public
 **********************************************************************/
void possessor_init_race_t(race_t *race_ptr, int default_r_idx)
{
    static int last_r_idx = -1;
    int        r_idx = plr->current_r_idx, i;

    if (!r_idx) /* Birthing menus. plr->prace not chosen yet. _birth() not called yet. */
        r_idx = default_r_idx; 

    if (r_idx != last_r_idx)
    {
        monster_race *r_ptr;
    
        if (plr->current_r_idx == r_idx) /* Birthing menus. current_r_idx = 0 but r_idx = default_r_idx. */
            last_r_idx = r_idx;            /* BTW, the game really needs a "current state" concept ... */

        r_ptr = mon_race_lookup(r_idx);

        race_ptr->base_hp = 15;

        race_ptr->hooks.caster_info = NULL;
        if (r_ptr->body.spell_stat != A_NONE)
            race_ptr->hooks.caster_info = possessor_caster_info;

        race_ptr->infra = r_ptr->body.infra;

        race_ptr->life = r_ptr->body.life;
        if (!race_ptr->life)
            race_ptr->life = 100;
    
        race_ptr->equip_template = plr_equip_template();

        for (i = 0; i < MAX_STATS; i++)
        {
            race_ptr->stats[i] = r_ptr->body.stats[i];
            race_ptr->stats[i] += r_ptr->body.extra_stats[i] * plr->lev/50;
        }

        race_ptr->skills = r_ptr->body.skills;
        race_ptr->extra_skills = r_ptr->body.extra_skills;

        race_ptr->pseudo_class_id = r_ptr->body.class_id;

        race_ptr->subname = mon_name(r_idx);

        race_ptr->flags = RACE_IS_MONSTER;
        if (mon_race_is_undead(r_ptr))
            race_ptr->flags |= RACE_IS_UNDEAD;
        if (mon_race_is_nonliving(r_ptr))
            race_ptr->flags |= RACE_IS_NONLIVING;
        if (mon_race_is_demon(r_ptr))
            race_ptr->flags |= RACE_IS_DEMON;
        if (mon_blows_find(r_ptr->blows, RBM_MONK))
            race_ptr->flags |= RACE_MARTIAL_ARTS;
    }
    if (birth_hack || spoiler_hack)
    {
        race_ptr->subname = NULL;
        race_ptr->subdesc = NULL;
    }
}
plr_race_ptr mon_possessor_get_race(void)
{
    static plr_race_ptr me = NULL;

    if (!me)
    {
        me = plr_race_alloc(RACE_MON_POSSESSOR);
        me->name = "Possessor";
        me->desc = "The Possessor is an odd creature, completely harmless in its natural form. However, they "
                    "are capable of possessing the corpses of monsters they have slain, and gain powers and "
                    "abilities based on their current body. As such, they can become quite powerful indeed! "
                    "Unfortunately, not every type of monster will drop a corpse, and getting suitable corspes "
                    "to inhabit can be difficult. If the possessor ever leaves their current body then all of "
                    "their equipment will be removed (except a "
                    "light source) and they will temporarily be in their native, vulnerable state. Finally, "
                    "leaving their current body will destroy that corpse most of the time, so the possessor "
                    "should only do so if they have a better corpse on hand (and also only if there are no "
                    "monsters nearby!).\n \n"
                    "Possessors are monsters and do not choose a normal class. Their stats, skills, resistances "
                    "and spells are completely determined by the body they inhabit. Their current body also "
                    "determines their spell stat (e.g. a novice priest uses wisdom, a novice mage uses intelligence). "
                    "Their current body may offer innate powers (e.g. breath weapons or rockets) in addition to or in lieu "
                    "of magical powers (e.g. mana storms and frost bolts). Be sure to check both the racial power "
                    "command ('U') and the magic command ('m') after possessing a new body.";

        me->exp = 250;
        me->shop_adjust = 110; /* Really should depend on current form */

        me->hooks.birth = _birth;
        me->hooks.get_powers = _get_powers;
        me->hooks.calc_innate_attacks = possessor_calc_innate_attacks;
        me->hooks.calc_bonuses = possessor_calc_bonuses;
        me->hooks.calc_shooter_bonuses = possessor_calc_shooter_bonuses;
        me->hooks.calc_weapon_bonuses = possessor_calc_weapon_bonuses;
        me->hooks.get_flags = possessor_get_flags;
        me->hooks.player_action = _player_action;
        me->hooks.save_player = possessor_on_save;
        me->hooks.load_player = possessor_on_load;
        me->hooks.character_dump = possessor_character_dump;
    }

    possessor_init_race_t(me, mon_race_parse("@.soul")->id);
    return me;
}

bool possessor_can_gain_exp(void)
{
    int max = _max_lvl();
    if (max < PY_MAX_LEVEL && plr->lev >= max)
        return FALSE;
    return TRUE;
}

s32b possessor_max_exp(void)
{
    int max = _max_lvl();
    if (max < PY_MAX_LEVEL)
        return exp_requirement(max) - 1;
    else
        return 99999999;
}

void possessor_on_take_hit(void)
{
    /* Getting too wounded may eject the possessor! */
    if ( plr->chp < plr->mhp/4
      && !plr_mon_race_is_("@.soul") )
    {
        if (one_in_(66))
        {
            int old_r_idx = plr->current_r_idx;
            monster_race *old_r_ptr = mon_race_lookup(old_r_idx);

            msg_print("You can no longer maintain your current body!");
            if (one_in_(3))
            {
                object_type forge;
                object_prep(&forge, lookup_kind(TV_CORPSE, SV_CORPSE));
                forge.race_id = old_r_idx;
                forge.weight = MIN(500*10, MAX(40, old_r_ptr->weight * 10));
                drop_near(&forge, plr->pos, -1);
            }
            else
                msg_print("Your previous body quickly decays!");

            possessor_set_current_r_idx(mon_race_parse("@.soul")->id);
            plr->chp = plr->mhp; /* Be kind. This effect is nasty! */
            plr->chp_frac = 0;
        }
        else
        {
            msg_print("You struggle to maintain possession of your current body!");
        }
    }
}

void possessor_set_current_r_idx(int r_idx)
{
    if (r_idx != plr->current_r_idx)
    {
        int mana_ratio = plr->csp * 100 / MAX(1, plr->msp);

        plr->magic_num1[0] = 0; /* Blinking Death ... */
        plr->current_r_idx = r_idx;
        lore_do_probe(r_idx);

        if (plr->exp > possessor_max_exp())
        {
            plr->exp = possessor_max_exp();
            check_experience();
        }
        else
            restore_level();

        plr->update |= PU_BONUS | PU_INNATE | PU_HP | PU_MANA | PU_TORCH;
        plr->redraw |= PR_MAP | PR_BASIC | PR_MANA | PR_EXP | PR_EQUIPPY;

        /* Apply the new body type to our equipment */
        equip_on_change_race();

        /* Mimic's shift alot. Try to preserve the old mana ratio if possible. */
        if (plr->prace == RACE_MON_MIMIC)
        {
            handle_stuff();

            plr->csp = plr->msp * mana_ratio / 100;
            plr->csp_frac = 0;
            plr->redraw |= PR_MANA;
            plr->window |= PW_SPELL;

            if (!sym_equals(plr->current_r_idx, "@.mimic"))
                _history_on_possess(r_idx);
        }
        else if (!plr_mon_race_is_("@.soul"))
            _history_on_possess(r_idx);    
    }
}

void possessor_do_auras(mon_ptr mon)
{
    mon_race_ptr race;
    mon_aura_ptr aura;

    if (plr->prace != RACE_MON_POSSESSOR && plr->prace != RACE_MON_MIMIC) return;

    race = plr_mon_race();
    for (aura = race->auras; aura; aura = aura->next)
    {
        int dam;
        if (aura->pct && randint1(100) > aura->pct) continue;
        dam = dice_roll(aura->dam);
        if (!dam) continue;
        gf_affect_m(who_create_plr(), mon, aura->gf, dam, GF_AFFECT_AURA);
    }
}

void possessor_explode(int dam)
{
    if (plr->prace == RACE_MON_POSSESSOR || plr->prace == RACE_MON_MIMIC)
    {
        monster_race *r_ptr = plr_mon_race();
        mon_blow_ptr  blow = mon_blows_find(r_ptr->blows, RBM_EXPLODE);

        if (blow && blow->effect_ct)
        {
            int typ = blow->effects[0].type;
            plr_burst(3, typ, dam);
        }

        if (plr->prace == RACE_MON_MIMIC)
            possessor_set_current_r_idx(mon_race_parse("@.mimic")->id);
        else
            possessor_set_current_r_idx(mon_race_parse("@.soul")->id);

        take_hit(DAMAGE_NOESCAPE, dam, "Exploding");
        plr_tim_add(T_STUN, 10); /* XXX bypass resistance */
    }
}

void possessor_character_dump(doc_ptr doc)
{
    _history_ptr p = _history;
    int          ct = 0;
    char         lvl[80];
    char         loc[255];
    
    if ( !plr_mon_race_is_("@.mimic")
      && !plr_mon_race_is_("@.soul") )
    {
        mon_race_ptr race = plr_mon_race();
        bool old_use_graphics = use_graphics;
        use_graphics = FALSE;
        doc_printf(doc, "<topic:CurrentForm>================================ <color:keypress>C</color>urrent Form =================================\n\n");
        mon_display_possessor(race, doc);
        doc_newline(doc);
        use_graphics = old_use_graphics;
    }
    doc_printf(doc, "<topic:RecentForms>================================ <color:keypress>R</color>ecent Forms =================================\n\n");
    doc_insert(doc, "<style:table>");
    doc_printf(doc, "<color:G>%-33.33s CL Day  Time  DL %-28.28s</color>\n", "Most Recent Forms", "Location");

    while (p && ct < 100)
    {
        int day, hour, min;
        extract_day_hour_min_imp(p->turn, &day, &hour, &min);

        switch (p->d_idx)
        {
        case DUNGEON_QUEST:
            sprintf(loc, "%s", quests_get(p->d_lvl)->name);
            sprintf(lvl, "%3d", quests_get(p->d_lvl)->level);
            break;
        case DUNGEON_TOWN:
            sprintf(loc, "%s", town_name(p->d_lvl));
            sprintf(lvl, "%s", "   ");
            break;
        case DUNGEON_WILD:
            strcpy(loc, "Surface");
            sprintf(lvl, "%3d", p->d_lvl);
            break;
        default: {
            dun_type_ptr type = dun_types_lookup(p->d_idx);
            sprintf(loc, "%s", type->name);
            if (p->d_lvl)
                sprintf(lvl, "%3d", p->d_lvl);
            else
                sprintf(lvl, "%s", "   ");
            break; }
        }
        doc_printf(doc, "%-33.33s %2d %3d %2d:%02d %s %-25.25s\n",
            mon_name(p->r_idx), 
            p->p_lvl, 
            day, hour, min,
            lvl, loc
        );
        p = p->next;
        ct++;
    }
    doc_insert(doc, "</style>");
    doc_newline(doc);
}

void possessor_on_save(savefile_ptr file)
{
    _history_on_save(file);
}

void possessor_on_load(savefile_ptr file)
{
    _history_on_load(file);
}

