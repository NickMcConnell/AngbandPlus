#include "angband.h"

bool devicemaster_desperation = FALSE;

static int _speciality_tval(int psubclass)
{
    switch (psubclass)
    {
    case DEVICEMASTER_RODS: return TV_ROD;
    case DEVICEMASTER_STAVES: return TV_STAFF;
    case DEVICEMASTER_WANDS: return TV_WAND;
    case DEVICEMASTER_POTIONS: return TV_POTION;
    case DEVICEMASTER_SCROLLS: return TV_SCROLL;
    }
    return TV_WAND;
}

void _desperation_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Desperation");
        break;
    case SPELL_DESC:
        var_set_string(res, "Use multiple charges in chosen device for extra power. The chosen device may explode, however.");
        break;
    case SPELL_CAST:
        devicemaster_desperation = TRUE;
        switch (p_ptr->psubclass)
        {
        case DEVICEMASTER_RODS: do_cmd_zap_rod(); break;
        case DEVICEMASTER_WANDS: do_cmd_aim_wand(); break;
        case DEVICEMASTER_STAVES: do_cmd_use_staff(); break;
        case DEVICEMASTER_POTIONS: do_cmd_quaff_potion(); break;
        case DEVICEMASTER_SCROLLS: do_cmd_read_scroll(); break;
        }
        devicemaster_desperation = FALSE;
        var_set_bool(res, energy_use != 0);
        break;
    case SPELL_ENERGY:
        var_set_int(res, energy_use);     /* already set correctly by do_cmd_*() */
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

bool _detect_devices(int range)
{
    bool result = FALSE;
    int i, y, x;

    if (d_info[dungeon_type].flags1 & DF1_DARKNESS) range /= 3;

    for (i = 1; i < o_max; i++)
    {
        object_type *o_ptr = &o_list[i];

        if (!o_ptr->k_idx) continue;
        if (o_ptr->held_m_idx) continue;

        y = o_ptr->iy;
        x = o_ptr->ix;

        if (distance(py, px, y, x) > range) continue;

        switch (o_ptr->tval)
        {
        case TV_ROD:
        case TV_WAND:
        case TV_STAFF:
        case TV_SCROLL:
        case TV_POTION:
            o_ptr->marked |= OM_FOUND;
            lite_spot(y, x);
            result = TRUE;
        }
    }

    if (result)
        msg_print("You sense the presence of magical devices!");

    return result;
}

void _detect_devices_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Detect Devices");
        break;
    case SPELL_DESC:
        var_set_string(res, "Detects nearby magical devices.");
        break;
    case SPELL_CAST:
        _detect_devices(DETECT_RAD_DEFAULT);
        var_set_bool(res, TRUE);
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static bool _is_device(object_type *o_ptr)
{
    switch (o_ptr->tval)
    {
    case TV_ROD:
    case TV_WAND:
    case TV_STAFF:
    case TV_SCROLL:
    case TV_POTION:
        return TRUE;
    }
    return FALSE;
}

void _identify_device_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Identify Device");
        break;
    case SPELL_DESC:
        var_set_string(res, "Identify a single magical device.");
        break;
    case SPELL_CAST:
        var_set_bool(res, ident_spell(_is_device));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void _recharging_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Recharging");
        break;
    case SPELL_DESC:
        var_set_string(res, "Attempts to recharge staffs, wands or rods.  Items may be destroyed on failure.");
        break;
    case SPELL_INFO:
        var_set_string(res, format("Power %d", 50 + 3*p_ptr->lev));
        break;
    case SPELL_CAST:
        var_set_bool(res, recharge(50 + 3*p_ptr->lev));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static object_type *_transfer_src_obj = NULL;
static bool _transfer_obj_p(object_type *o_ptr)
{
    if ( o_ptr->tval == _speciality_tval(p_ptr->psubclass)
      && o_ptr != _transfer_src_obj )
    {
        if (p_ptr->psubclass == DEVICEMASTER_POTIONS || p_ptr->psubclass == DEVICEMASTER_SCROLLS)
        {
            /* One may not use worthless high level items as source objects (e.g. Curse Armor could make Genocide!!) */
            if (!_transfer_src_obj && k_info[o_ptr->k_idx].cost <= 0)
                return FALSE;
            /* Potions and scrolls must transfer to weaker destination objects */
            if (_transfer_src_obj && k_info[_transfer_src_obj->k_idx].level < k_info[o_ptr->k_idx].level)
                return FALSE;
        }
        return TRUE;
    }
    return FALSE;
}

void _transfer_charges_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        if (p_ptr->psubclass != DEVICEMASTER_POTIONS && p_ptr->psubclass != DEVICEMASTER_SCROLLS)
            var_set_string(res, "Transfer Charges");
        else
            var_set_string(res, "Transfer Essence");
        break;
    case SPELL_DESC:
        if (p_ptr->psubclass == DEVICEMASTER_POTIONS)
            var_set_string(res, "Transfer essence from one potion to another.");
        else if (p_ptr->psubclass == DEVICEMASTER_SCROLLS)
            var_set_string(res, "Transfer essence from one scroll to another.");
        else
            var_set_string(res, "Transfer charges from one device to another.");
        break;
    case SPELL_CAST:
    {
        int tval = _speciality_tval(p_ptr->psubclass);
        int src_idx = 0, dest_idx = 0;
        object_type *src_obj = NULL, *dest_obj = NULL;
        object_kind *src_kind = NULL, *dest_kind = NULL;
        int src_charges = 0, dest_charges = 0, max_charges = 0, power = 0;

        var_set_bool(res, FALSE);

        /* Choose the objects */
        _transfer_src_obj = NULL;
        item_tester_hook = _transfer_obj_p;
        if (!get_item(&src_idx, "Transfer from which item? ", "You have no items to use.", USE_INVEN)) return;
        src_obj = &inventory[src_idx];
        src_kind = &k_info[src_obj->k_idx];

        _transfer_src_obj = src_obj;
        item_tester_hook = _transfer_obj_p;
        if (!get_item(&dest_idx, "Transfer to which item? ", "You have no items to use.", USE_INVEN)) return;
        dest_obj = &inventory[dest_idx];
        dest_kind = &k_info[dest_obj->k_idx];

        if (dest_obj == src_obj)
        {
            msg_print("Failed! Please pick distinct objects for the source and destination.");
            return;
        }

        if (tval == TV_SCROLL || tval == TV_POTION)
        {
            if (dest_kind->level > src_kind->level) /* Double Check ... should already be excluded! */
            {
                msg_print("Failed! You may only transfer to objects of greater or equal power.");
                return;
            }
        }

        /*if (dest_obj->tval == TV_SCROLL)
        {
            switch (dest_obj->sval)
            {
            case SV_SCROLL_ARTIFACT:
            case SV_SCROLL_ACQUIREMENT:
            case SV_SCROLL_STAR_ACQUIREMENT:
                msg_print("Failed! You may not transfer to *that* type of scroll!");
                return;
            }
        }
        if (dest_obj->tval == TV_POTION)
        {
            switch (dest_obj->sval)
            {
            case SV_POTION_AUGMENTATION:
            case SV_POTION_INC_STR:
            case SV_POTION_INC_INT:
            case SV_POTION_INC_WIS:
            case SV_POTION_INC_DEX:
            case SV_POTION_INC_CON:
            case SV_POTION_INC_CHR:
            case SV_POTION_EXPERIENCE:
            case SV_POTION_STAR_ENLIGHTENMENT:
            case SV_POTION_INVULNERABILITY:
                msg_print("Failed! You may not transfer to *that* type of potion!");
                return;
            }
        }*/

        var_set_bool(res, TRUE);

        /* Calculate the number of charges */
        switch (tval)
        {
        case TV_ROD:
            src_charges = src_obj->number - (src_obj->timeout + src_kind->pval - 1)  / src_kind->pval;
            break;
        case TV_STAFF:
            src_charges = src_obj->pval * src_obj->number;
            break;
        case TV_WAND:
            src_charges = src_obj->pval;
            break;
        case TV_POTION:
        case TV_SCROLL:
            src_charges = src_obj->number;
            break;
        }
        if (src_charges <= 0) 
        {
            msg_print("There is nothing to transfer.");
            return;
        }

        switch (tval)
        {
        case TV_ROD:
            max_charges = dest_obj->number; /* Don't worry about charging destination rods ... */
            break;
        case TV_STAFF:
            max_charges = dest_kind->pval * dest_obj->number - dest_obj->pval * dest_obj->number;
            break;
        case TV_WAND:
            max_charges = dest_kind->pval * dest_obj->number - dest_obj->pval;
            break;
        case TV_POTION:
        case TV_SCROLL:
            max_charges = 99 - dest_obj->number;
            break;
        }

        if (max_charges <= 0)
        {
            msg_print("Failed! The destination object is already fully charged.");
            return;
        }

        power = src_charges * src_kind->level;
        switch (tval)
        {
        case TV_WAND:
        case TV_STAFF:
        case TV_SCROLL:
            power = power * 3 / 4;
            break;
        case TV_POTION:
            power = power / 2;
            break;
        }
        dest_charges = power / MAX(dest_kind->level, 10);

        if (dest_charges > max_charges)
            dest_charges = max_charges;

        if (tval == TV_STAFF)
            dest_charges /= dest_obj->number;

        if (dest_charges <= 0)
        {
            msg_print("Failed! The source object is not powerful enough to transfer even a single charge.");
            return;
        }

        /* Perform the transfer */
        switch (tval)
        {
        case TV_ROD:
            src_obj->timeout += src_charges * src_kind->pval;
            dest_obj->timeout -= dest_charges * dest_kind->pval;
            if (dest_obj->timeout < 0)
                dest_obj->timeout = 0;
            break;
        case TV_STAFF:
            src_obj->pval -= src_charges / src_obj->number;
            dest_obj->pval += dest_charges;
            break;
        case TV_WAND:
            src_obj->pval -= src_charges;
            dest_obj->pval += dest_charges;
            break;
        case TV_POTION:
        case TV_SCROLL: /* Must add to dest first!! */
            inven_item_increase(dest_idx, dest_charges);
            inven_item_describe(dest_idx);
            inven_item_optimize(dest_idx);

            inven_item_increase(src_idx, -src_charges);
            inven_item_describe(src_idx);
            inven_item_optimize(src_idx);
            break;
        }
        p_ptr->notice |= (PN_COMBINE | PN_REORDER);
        p_ptr->window |= (PW_INVEN | PW_EQUIP | PW_PLAYER);
        break;
    }
    default:
        default_spell(cmd, res);
        break;
    }
}


static spell_info _spells[] = 
{
    /*lvl cst fail spell */
    {  1,  1, 30, _detect_devices_spell},
    {  5,  5, 30, _identify_device_spell},
    { 10, 15, 60, _recharging_spell},
    { 15, 25, 60, _transfer_charges_spell},
    { 25,  0,  0, _desperation_spell},
    { -1, -1, -1, NULL}
};

static int _get_spells(spell_info* spells, int max)
{    
    return get_spells_aux(spells, max, _spells);
}

cptr devicemaster_speciality_name(int psubclass)
{
    switch (psubclass)
    {
    case DEVICEMASTER_RODS: return "Rods";
    case DEVICEMASTER_STAVES: return "Staves";
    case DEVICEMASTER_WANDS: return "Wands";
    case DEVICEMASTER_POTIONS: return "Potions";
    case DEVICEMASTER_SCROLLS: return "Scrolls";
    }
    return "";
}

cptr devicemaster_speciality_desc(int psubclass)
{
    switch (psubclass)
    {
    case DEVICEMASTER_RODS: return "You specialize in the use of rods.";
    case DEVICEMASTER_STAVES: return "You specialize in the use of staves.";
    case DEVICEMASTER_WANDS: return "You specialize in the use of wands.";
    case DEVICEMASTER_POTIONS: return "You specialize in the use of potions.";
    case DEVICEMASTER_SCROLLS: return "You specialize in the use of scrolls.";
    }
    return "";
}

static void _birth(void) 
{ 
    object_type    forge;

    switch (p_ptr->psubclass)
    {
    case DEVICEMASTER_RODS:
        object_prep(&forge, lookup_kind(TV_ROD, SV_ROD_SLEEP_MONSTER));
        add_outfit(&forge);
        break;
    case DEVICEMASTER_STAVES:
        object_prep(&forge, lookup_kind(TV_STAFF, SV_STAFF_SLEEP_MONSTERS));
        forge.pval = k_info[forge.k_idx].pval;
        add_outfit(&forge);
        break;
    case DEVICEMASTER_WANDS:
        object_prep(&forge, lookup_kind(TV_WAND, SV_WAND_SLEEP_MONSTER));
        forge.pval = k_info[forge.k_idx].pval;
        add_outfit(&forge);
        break;
    case DEVICEMASTER_POTIONS:
        object_prep(&forge, lookup_kind(TV_POTION, SV_POTION_SPEED));
        forge.number = 6;
        add_outfit(&forge);
        break;
    case DEVICEMASTER_SCROLLS:
        object_prep(&forge, lookup_kind(TV_SCROLL, SV_SCROLL_TELEPORT));
        forge.number = 6;
        add_outfit(&forge);
        break;
    }
}

static void _character_dump(FILE* file)
{
    cptr desc = devicemaster_speciality_name(p_ptr->psubclass);

    fprintf(file, "\n\n================================== Abilities ==================================\n\n");
    {
        int pow = p_ptr->lev / 10;
        if (devicemaster_is_(DEVICEMASTER_RODS))
            pow = p_ptr->lev / 5;
        if (pow)
            fprintf(file, " * You gain +%d%% power when using %s.\n", device_power_aux(100, pow) - 100, desc);
    }
    fprintf(file, " * You use %s more quickly.\n", desc);    
    if (p_ptr->psubclass != DEVICEMASTER_POTIONS && p_ptr->psubclass != DEVICEMASTER_SCROLLS)
        fprintf(file, " * You have a chance of not consuming a charge when using %s.\n", desc);
    else
        fprintf(file, " * You have a chance of not consuming an item when using %s.\n", desc);
    if (p_ptr->psubclass != DEVICEMASTER_POTIONS && p_ptr->psubclass != DEVICEMASTER_SCROLLS)
        fprintf(file, " * You may use %s even when frightened.\n", desc);
    fprintf(file, " * You are resistant to charge draining (Power=%d).\n\n", p_ptr->lev);    

    {
        spell_info spells[MAX_SPELLS];
        int        ct = _get_spells(spells, MAX_SPELLS);

        dump_spells_aux(file, spells, ct);
    }
}

static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.which_stat = A_INT;
        me.magic_desc = "talent";
        me.options = CASTER_USE_HP;
        me.weight = 3000;
        init = TRUE;
    }
    return &me;
}

class_t *devicemaster_get_class_t(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    /* static info never changes */
    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  50,  36,   2,  20,  16,  48,  35 };
    skills_t xs = {  7,  20,  10,   0,   0,   0,  13,  11 };

        me.name = "Devicemaster";
        me.desc = 
            "Devicemasters are excellent with magical devices, but poor in most other skills. "
            "They may shoot or use melee in a pinch, but this will never be their forte. Instead, "
            "they must rely on their arsenal of magical devices in order to survive.\n \n"
            "The Devicemaster chooses to specialize in a particular class of devices and they "
            "gain extra bonuses when using devices from their speciality. These bonuses include "
            "increased damage, increased speed of activation, extra resistance to charge draining, "
            "and even the ability to occasionally power these devices without consuming charges. "
            "Each of these abilities becomes greater with experience.\n \n"
            "Devicemasters have a few magical abilities to enhance their utility with devices. As expected, "
            "they gain a powerful talent of Recharging very early on. Also, they may detect magical devices "
            "from a distance. At higher levels, they gain the powerful ability to move charges from "
            "one device to another without the risk of destroying the target device. As a final ability, "
            "the Devicemaster may use multiple charges at once from a given device in an act of "
            "desperation. This greatly increases the power of the effect, but may destroy the device "
            "in the process.";
    
        me.stats[A_STR] = -1;
        me.stats[A_INT] =  2;
        me.stats[A_WIS] =  1;
        me.stats[A_DEX] =  2;
        me.stats[A_CON] = -2;
        me.stats[A_CHR] = -2;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 101;
        me.base_hp = 6;
        me.exp = 130;
        me.pets = 30;

        me.birth = _birth;
        me.get_spells = _get_spells;
        me.character_dump = _character_dump;
        me.caster_info = _caster_info;
        init = TRUE;
    }

    me.subname = devicemaster_speciality_name(p_ptr->psubclass);
    return &me;
}
