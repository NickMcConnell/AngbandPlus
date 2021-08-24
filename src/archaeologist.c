/****************************************************************
 * The Archaeologist
 ****************************************************************/

#include "angband.h"
#include "equip.h"

/* Confirm 1 or 2 whip weapons for whip techniques. Fail if no whip
   is worn, or if a non-whip weapon is warn. Do not fail if shields
   or capture balls are equipped. Handle empty inventory slots. */
static bool _whip_check(void)
{
    bool result = FALSE;
    int i;

    for (i = 0; i < MAX_HANDS; i++)
    {
        if (plr->attack_info[i].type == PAT_WEAPON)
        {
            obj_ptr obj = equip_obj(plr->attack_info[i].slot);
            if (obj_is_(obj, TV_HAFTED, SV_WHIP))
                result = TRUE; /* Found a whip weapon ... keep looking */
            else
                return FALSE; /* Found a non-whip weapon */
        }
    }
    return result;
}

/* A special fetch(), that places item in player's inventory */
static bool _whip_fetch(int dir, int rng)
{
    point_t tgt;
    obj_ptr obj;
    char    o_name[MAX_NLEN];

    /* Use a target */
    if (dir == 5 && target_okay())
    {
        tgt = who_pos(plr->target);

        if (point_distance(plr->pos, tgt) > MAX_RANGE)
        {
            msg_print("You can't fetch something that far away!");
            return FALSE;
        }
        if (!dun_obj_at(cave, tgt))
        {
            msg_print("There is no object at this place.");
            return TRUE;  /* didn't work, but charge the player energy anyway */
        }

        /* Fetching from a vault is OK */

        if (!plr_view(tgt))
        {
            msg_print("You have no direct line of sight to that location.");
            return FALSE;
        }
        else if (!point_project(plr->pos, tgt))
        {
            msg_print("You have no direct line of sight to that location.");
            return FALSE;
        }
    }
    else
    {
        tgt = plr->pos;
        do
        {
            tgt = point_step(tgt, dir);
            if (!dun_pos_interior(cave, tgt)) return TRUE;
            if ( point_distance(plr->pos, tgt) > MAX_RANGE
              || !cell_project(dun_grid_at(cave, tgt)) )
            {
                return TRUE;
            }
        }
        while (!dun_obj_at(cave, tgt));
    }

    obj = dun_obj_at(cave, tgt);
    if (obj->weight > plr->lev * 15)
    {
        msg_print("The object is too heavy.");
        return TRUE; /* didn't work, but charge the player energy anyway */
    }

    object_desc(o_name, obj, OD_NAME_ONLY);

    /* Get the object */
    msg_format("You skillfully crack your whip and fetch %^s.", o_name);
    pack_carry(obj);
    obj_release(obj, OBJ_RELEASE_QUIET);

    return TRUE;
}

static bool _detect = FALSE;
static bool _detect_range;
static void _sense_aux(point_t pos, obj_ptr pile)
{
    obj_ptr obj;
    if (point_distance(plr->pos, pos) > _detect_range) return;
    for (obj = pile; obj; obj = obj->next)
    {
        /* Only alert to great discoveries */
        if (!obj_is_art(obj)) continue;

        /* Only alert to new discoveries */
        if (obj_is_known(obj)) continue;

        obj->marked |= OM_FOUND;
        plr->window |= PW_OBJECT_LIST;
        draw_pos(pos);
        _detect = TRUE;
    }
}
static bool _sense_great_discovery(int range)
{
    _detect = FALSE;
    _detect_range = range;
    dun_iter_floor_obj(cave, _sense_aux);
    return _detect;
}

/****************************************************************
 * Private Spells
 ****************************************************************/
static void _ancient_protection_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Ancient Protection");
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

static void _double_crack_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Double Crack");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attack a monster normally with your whip, and then randomly attack an adjacent monster.");
        break;
    case SPELL_CAST: {
        mon_ptr mon;
        int i;

        var_set_bool(res, FALSE);
        if (!_whip_check())
        {
            msg_print("Whip techniques can only be used if you are fighting with whips.");
            break;
        }
        mon = plr_target_adjacent_mon();
        if (!mon) break;

        /* normal attack plus 3 tries at a random adjacent monster */
        plr_attack_normal(mon->pos);
        for (i = 0; i < 3; i++)
        {
            point_t p = point_random_step(plr->pos);
            mon = dun_mon_at(cave, p);
            if (mon)
            {
                plr_attack_normal(mon->pos);
                break;
            }
            if (i == 2) msg_print("Your whip cracks in empty air!");
        }
        var_set_bool(res, TRUE);
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _evacuation_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Escape Rope");
        break;
    case SPELL_DESC:
        var_set_string(res, "Danger! Abandon this expedition and escape to a new level.");
        break;
    case SPELL_CAST:
        var_set_bool(res, FALSE);
        if (msg_prompt("You are about to flee the current level. Are you sure? <color:y>[y/n]</color>", "ny", PROMPT_DEFAULT) == 'y')
        {
            dun_teleport_level_plr(cave);
            var_set_bool(res, TRUE);
        }
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _excavation_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Excavation");
        break;
    case SPELL_DESC:
        var_set_string(res, "You break walls on your quest for treasure!  This takes a bit more time, though.");
        break;
    case SPELL_ENERGY: {
        int n = 200;
        if (equip_find_obj(TV_DIGGING, SV_ANY))
            n -= 120 * plr->lev / 50;
        else
            n -= 80 * plr->lev / 50;
        var_set_int(res, n);
        break; }
    case SPELL_CAST: {
        int dir = 5;
        var_set_bool(res, FALSE);
        if (get_rep_dir2(&dir) && dir != 5)
        {
            point_t pos = point_step(plr->pos, dir);

            if (!dun_pos_interior(cave, pos))
            {
                msg_print("You may excavate no further.");
                break;
            }

            if (dun_tunnel(cave, pos, ACTION_FORCE | ACTION_QUIET) != ACTION_SUCCESS)
            {
                msg_print("There is nothing to excavate.");
                break;
            }

            msg_print("You dig your way to treasure!");
            move_player_effect(pos, 0);
            var_set_bool(res, TRUE);
        }
        break; }
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _extended_whip_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Extended Crack");
        break;
    case SPELL_DESC:
        var_set_string(res, "This spell extends the range of your whip based melee attack.");
        break;
    case SPELL_CAST:
        if (_whip_check())
            var_set_bool(res, plr_attack_ranged(PLR_HIT_NORMAL, PAC_NO_INNATE, 2));
        else
        {
            msg_print("Whip techniques can only be used if you are fighting with whips.");
            var_set_bool(res, FALSE);
        }
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _fetch_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Fetch");
        break;
    case SPELL_DESC:
        var_set_string(res, "Use your whip to fetch a nearby item.");
        break;
    case SPELL_CAST:
        if (_whip_check())
        {
            int dir = 5;
            bool b = FALSE;
            int rng = 3 + plr->lev/25;

            project_length = rng;
            if (get_aim_dir(&dir))
            {
                b = _whip_fetch(dir, rng);
            }
            var_set_bool(res, b);
        }
        else
        {
            msg_print("Whip techniques can only be used if you are fighting with whips.");
            var_set_bool(res, FALSE);
        }
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _first_aid_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "First Aid");
        break;
    case SPELL_DESC:
        if (plr->lev < 8)
            var_set_string(res, "Heals HP and Stun.");
        else if (plr->lev < 12)
            var_set_string(res, "Heals HP and Stun. Cures cuts.");
        else if (plr->lev < 16)
            var_set_string(res, "Heals HP and Stun. Cures cuts and slows poison.");
        else if (plr->lev < 20)
            var_set_string(res, "Heals HP and Stun. Cures cuts and poison.");
        else if (plr->lev < 30)
            var_set_string(res, "Heals HP and Stun. Cures cuts, poison and blindness.");
        else if (plr->lev < 40)
            var_set_string(res, "Heals HP and Stun. Cures cuts, poison and blindness. Restores Con.");
        else if (plr->lev < 45)
            var_set_string(res, "Heals HP and Stun. Cures cuts, poison and blindness. Restores Con and Chr.");
        else
            var_set_string(res, "Heals HP and Stun. Cures cuts, poison and blindness. Restores Con, Chr and Str.");
        break;
    case SPELL_SPOIL_DESC:
        var_set_string(res, "Heals HP and Stun. Slows Poison (L12). Cures cuts (L8), poison (L16) and blindness (L20). Restores Con (L30), Chr (L40) and Str (L45).");
        break;
    case SPELL_INFO:
        var_set_string(res, info_heal(0, 0, spell_power(plr->lev)));
        break;
    case SPELL_CAST:
        hp_player(spell_power(plr->lev));
        plr_tim_remove(T_STUN);

        if (plr->lev >= 8)
            plr_tim_remove(T_CUT);
        if (plr->lev >= 12 && plr->lev < 16)
            plr_tim_recover(T_POISON, 90, 25);
        if (plr->lev >= 16)
            plr_tim_recover(T_POISON, 80, 50);
        if (plr->lev >= 20)
            plr_tim_remove(T_BLIND);
        if (plr->lev >= 30)
            do_res_stat(A_CON);
        if (plr->lev >= 40)
            do_res_stat(A_CHR);
        if (plr->lev >= 45)
            do_res_stat(A_STR);

        var_set_bool(res, TRUE);
        break;
    case SPELL_COST_EXTRA:
        var_set_int(res, plr->lev / 5);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _identify_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Identify");
        break;
    case SPELL_DESC:
        if (plr->lev < 25)
            var_set_string(res, "New Treasure!  You examine your new discovery.");
        else
            var_set_string(res, "New Treasure!  You examine your new discovery and learn its deepest truths.");
        break;
    case SPELL_SPOIL_DESC:
        var_set_string(res, "Identifies an object. At L25, fully identifies an object.");
        break;
    case SPELL_CAST:
        {
            bool b = TRUE;
            if (plr->lev < 25)
                b = ident_spell(NULL);
            else
                b = identify_fully(NULL);
            var_set_bool(res, b);
        }
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _magic_blueprint_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Magic Blueprint");
        break;
    case SPELL_DESC:
        if (plr->lev < 20)
            var_set_string(res, "A map to treasure!  Maps the surrounding area.");
        else if (plr->lev < 25)
            var_set_string(res, "A map to treasure!  Maps the surrounding area and detects traps and doors.");
        else if (plr->lev < 30)
            var_set_string(res, "A map to treasure!  Maps the surrounding area and detects traps, doors and objects.");
        else if (plr->lev < 35)
            var_set_string(res, "A map to treasure!  Maps the entire level and detects traps, doors and objects.");
        else
            var_set_string(res, "A map to treasure!  Maps and lights the entire level and detects traps, doors and objects.");
        break;
    case SPELL_SPOIL_DESC:
        var_set_string(res, "Maps nearby area or the entire level (L35). Detects treasure, traps (L20), doors (L20) and objects (25).");
        break;
    case SPELL_CAST:
        {
            int rad = DETECT_RAD_DEFAULT;

            if (plr->lev >= 30)
                rad = DETECT_RAD_ALL;

            map_area(rad);
            detect_treasure(rad);
            detect_objects_gold(rad);
            if (plr->lev >= 20)
            {
                detect_traps(rad, TRUE);
                detect_doors(rad);
            }
            if (plr->lev >= 25)
                detect_objects_normal(rad);

            if (plr->lev >= 35)
                wiz_lite();    /* somewhat redundant, but I want level wide trap detection! */

            var_set_bool(res, TRUE);
        }
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _pharaohs_curse_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Pharaoh's Curse");
        break;
    case SPELL_DESC:
        var_set_string(res, "Curses all nearby monsters, doing great damage and various effects.");
        break;
    case SPELL_CAST:
        {
            int power = spell_power(plr->lev * 4);
            plr_project_los(GF_PHARAOHS_CURSE, plr->lev + _1d(plr->lev));
            if (plr->lev >= 46) plr_project_los(GF_OLD_CONF, power);
            if (plr->lev >= 47) plr_project_los(GF_SLOW, power);
            if (plr->lev >= 48) plr_project_los(GF_FEAR, power);
            if (plr->lev >= 49) plr_project_los(GF_STUN, 5 + 2*plr->lev/5);
            if (one_in_(5))
            {
                int mode = 0;
                if (one_in_(2))
                    mode = PM_FORCE_PET;
                if (summon_named_creature(who_create_null(), plr->pos, mon_race_parse("z.mummy"), mode))
                {
                    msg_print("You have disturbed the rest of an ancient pharaoh!");
                }
            }
            take_hit(DAMAGE_USELIFE, plr->lev + _1d(plr->lev), "the Pharaoh's Curse");
            var_set_bool(res, TRUE);
        }
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _remove_curse_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Remove Curse");
        break;
    case SPELL_DESC:
        if (plr->lev < 40)
            var_set_string(res, "Cursed Treasure!  Removes any weak curses from your equipment.");
        else
            var_set_string(res, "Cursed Treasure!  Removes any curses from your equipment.");
        break;
    case SPELL_SPOIL_DESC:
        var_set_string(res, "Removes weak curses. At L40, also removes heavy curses.");
        break;
    case SPELL_CAST:
        if (plr->lev < 40)
        {
            if (remove_curse()) msg_print("You feel the curse has lifted.");
            else msg_print("Hmmm ... nothing happens.");
        }
        else
        {
            if (remove_all_curse()) msg_print("You feel the curse has lifted.");
            else msg_print("Hmmm ... nothing happens.");
        }
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _remove_obstacles_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Remove Obstacles");
        break;
    case SPELL_DESC:
        var_set_string(res, "Clears a path to treasure!  Traps, doors and trees will be removed.");
        break;
    case SPELL_CAST: {
        dice_t dice = {0};
        var_set_bool(res, FALSE);
        if (plr_cast_beam(GF_REMOVE_OBSTACLE, dice))
        {
            plr_burst(1, GF_REMOVE_OBSTACLE, 0);
            var_set_bool(res, TRUE);
        }
        break; }
    default:
        default_spell(cmd, res);
    }
}



/****************************************************************
 * Spell Table and Exports
 ****************************************************************/

static spell_info _spells[] =
{
    /*lvl cst fail spell */
    {  1,   3, 10, _extended_whip_spell },
    {  2,   3, 20, detect_traps_spell },
    {  3,   5, 20, light_area_spell },
    {  5,   5, 30, _first_aid_spell },
    { 10,  10, 40, _identify_spell },
    { 12,  10, 30, _remove_obstacles_spell },
    { 13,  20, 30, _double_crack_spell },
    { 15,  15, 30, _magic_blueprint_spell },
    { 18,  10, 30, _excavation_spell },
    { 22,  20, 30, _fetch_spell },
    { 25,  20, 50, _remove_curse_spell },
    { 32,  30, 70, recharging_spell },
    { 35,  80, 70, _ancient_protection_spell },
    { 40, 150, 80, polish_shield_spell },
    { 42,  30, 50, _evacuation_spell },
    { 45,  50, 75, _pharaohs_curse_spell },
    { -1,  -1, -1, NULL }
};


static int _get_spells(spell_info* spells, int max)
{
    return get_spells_aux(spells, max, _spells);
}

static bool _is_favored_weapon(object_type *o_ptr)
{
    if (o_ptr->tval == TV_DIGGING)
        return TRUE;

    if (o_ptr->tval == TV_HAFTED && o_ptr->sval == SV_WHIP)
        return TRUE;

    return FALSE;
}

bool archaeologist_is_favored_weapon(object_type *o_ptr)
{
    return _is_favored_weapon(o_ptr);
}

static void _process_player(void)
{
    bool sense = _sense_great_discovery(3 + plr->lev/10);
    if (sense && !plr->sense_artifact)
    {
        msg_print("You feel close to a great discovery!");
        plr->sense_artifact = TRUE;
        plr->redraw |= PR_STATUS;
    }
    else if (!sense && plr->sense_artifact)
    {
        msg_print("You feel you are leaving something special behind...");
        plr->sense_artifact = FALSE;
        plr->redraw |= PR_STATUS;
    }
}

static void _calc_bonuses(void)
{
    plr->see_infra += plr->lev/10;
    plr->skill_dig += 2*plr->lev;
    if (plr->lev >= 20)
        plr->see_inv++;
    if (plr->lev >= 38)
        res_add(GF_DARK);

    if (plr->lev >= 20) /* L10 spell, but the fail rate is significant */
        plr->auto_id_sp = 10;
}

static void _get_flags(u32b flgs[OF_ARRAY_SIZE])
{
    if (plr->lev >= 20)
        add_flag(flgs, OF_SEE_INVIS);
    if (plr->lev >= 38)
        add_flag(flgs, OF_RES_(GF_DARK));
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "spell";
        me.which_stat = A_WIS;
        me.encumbrance.max_wgt = 400;
        me.encumbrance.weapon_pct = 33;
        me.encumbrance.enc_wgt = 800;
        me.options = CASTER_GAIN_SKILL;
        init = TRUE;
    }
    return &me;
}

static void _character_dump(doc_ptr doc)
{
    spell_info spells[MAX_SPELLS];
    int        ct = _get_spells(spells, MAX_SPELLS);

    plr_display_spells(doc, spells, ct);
}

static void _birth(void)
{
    plr_birth_obj_aux(TV_HAFTED, SV_WHIP, 1);
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    plr_birth_obj_aux(TV_SCROLL, SV_SCROLL_MAPPING, rand_range(5, 10));
}

void _get_object(obj_ptr obj)
{
    if (obj_is_art(obj) && !obj_is_known(obj))
    {
        /* Suppress you are leaving something special behind message ... */
        if (plr->sense_artifact)
        {
            plr->sense_artifact = FALSE;    /* There may be more than one? */
            plr->redraw |= PR_STATUS;
        }

        if (!(obj->ident & IDENT_SENSE))
        {
            char name[MAX_NLEN];

            object_desc(name, obj, OD_COLOR_CODED);
            cmsg_format(TERM_L_BLUE, "You feel that the %s is %s...", name, game_inscriptions[FEEL_SPECIAL]);

            obj->ident |= IDENT_SENSE;
            obj->feeling = FEEL_SPECIAL;
        }
    }
}
static bool _auto_detect(void)
{
    if (plr->lev >= 10 && plr->csp >= 3)
    {
        detect_traps(DETECT_RAD_DEFAULT, TRUE);
        sp_player(-3);
        return TRUE;
    }
    return FALSE;
}
static status_display_t _status_display(void)
{
    status_display_t d = {0};

    if (plr->sense_artifact)
    {
        d.color = TERM_L_BLUE;
        d.name = "Special";
        d.abbrev = "Art";
    }
    return d;
}
plr_class_ptr archaeologist_get_class(void)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 45,  40,  36,   4,  50,  32,  56,  35};
    skills_t xs = { 75,  60,  50,   0,   0,   0,  90,  55};

        me = plr_class_alloc(CLASS_ARCHAEOLOGIST);
        me->name = "Archaeologist";
        me->desc = "The Archaeologist is an erudite treasure hunter, seeking out the most valuable "
                  "prizes that the dungeon has to offer. At home in subterranean caverns and vaults, "
                  "he is rarely lost or snared in traps. His powers of perception and detection are "
                  "very great, as is his skill with arcane devices. At high levels he can use the "
                  "dark magic of the entombed Pharaohs. The powers of the Archaeologist are enhanced "
                  "by Wisdom.";

        me->stats[A_STR] = -1;
        me->stats[A_INT] =  1;
        me->stats[A_WIS] =  2;
        me->stats[A_DEX] =  1;
        me->stats[A_CON] = -1;
        me->stats[A_CHR] =  0;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 106;
        me->base_hp = 8;
        me->exp = 120;
        me->pets = 40;
        me->flags = CLASS_SENSE1_FAST | CLASS_SENSE1_STRONG |
                   CLASS_SENSE2_FAST | CLASS_SENSE2_STRONG;

        me->hooks.birth = _birth;
        me->hooks.calc_bonuses = _calc_bonuses;
        me->hooks.get_flags = _get_flags;
        me->hooks.process_player = _process_player;
        me->hooks.caster_info = _caster_info;
        me->hooks.get_spells = _get_spells;
        me->hooks.character_dump = _character_dump;
        me->hooks.get_object = _get_object;
        me->hooks.auto_detect = _auto_detect;
        me->hooks.status_display = _status_display;
    }

    return me;
}
