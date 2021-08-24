#include "angband.h"

#include <assert.h>

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

void _desperation_spell(int cmd, var_ptr res)
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
        switch (plr->psubclass)
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

static bool _obj_is_device(obj_ptr obj)
{
    switch (obj->tval)
    {
    case TV_ROD:
    case TV_WAND:
    case TV_STAFF:
    case TV_SCROLL:
    case TV_POTION: return TRUE;
    }
    return FALSE;
}
static void _detect_devices(int range)
{
    if (detect_obj_aux(_obj_is_device, range))
        msg_print("You sense the presence of magical devices!");
}
void _detect_devices_spell(int cmd, var_ptr res)
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

void _identify_device_spell(int cmd, var_ptr res)
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
        if (plr->lev >= 25)
            var_set_bool(res, identify_fully(_is_device));
        else
            var_set_bool(res, ident_spell(_is_device));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

void _recharging_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Recharging");
        break;
    case SPELL_DESC:
        var_set_string(res, "It attempts to recharge a device using another device for power.");
        break;
    case SPELL_INFO:
        var_printf(res, "Power %d", 50 + 2*plr->lev);
        break;
    case SPELL_CAST:
        /* Devicemasters have no mana */
        var_set_bool(res, recharge_from_player(50 + 2*plr->lev));
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static object_type *_transfer_src_obj = NULL;
static bool _transfer_obj_p(object_type *o_ptr)
{
    if ( o_ptr->tval == _speciality_tval(plr->psubclass)
      && o_ptr != _transfer_src_obj )
    {
        if (plr->psubclass == DEVICEMASTER_POTIONS || plr->psubclass == DEVICEMASTER_SCROLLS)
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

static obj_ptr _get_src_obj(void)
{
    obj_prompt_t prompt = {0};

    _transfer_src_obj = NULL;

    prompt.prompt = "Transfer from which item?";
    prompt.error = "You have no source items to use.";
    prompt.filter = _transfer_obj_p;
    prompt.where[0] = INV_PACK;

    obj_prompt(&prompt);
    return prompt.obj;
}

static obj_ptr _get_dest_obj(obj_ptr src_obj)
{
    obj_prompt_t prompt = {0};

    _transfer_src_obj = src_obj;

    prompt.prompt = "Transfer to which item?";
    prompt.error = "You have no destination items to use.";
    prompt.filter = _transfer_obj_p;
    prompt.where[0] = INV_PACK;

    obj_prompt(&prompt);
    return prompt.obj;
}

static bool _transfer_effect(void)
{
    obj_ptr src_obj, dest_obj;

    /* Choose the objects */
    src_obj = _get_src_obj();
    if (!src_obj) return FALSE;
    if (obj_is_art(src_obj))
    {
        msg_print("Failed! You cannot transfer from artifacts.");
        return FALSE;
    }

    dest_obj = _get_dest_obj(src_obj);
    if (!dest_obj) return FALSE;
    if (dest_obj == src_obj)
    {
        msg_print("Failed! Please pick distinct objects for the source and destination.");
        return FALSE;
    }
    if (obj_is_art(dest_obj))
    {
        msg_print("Failed! You cannot transfer to artifacts.");
        return FALSE;
    }
    if (device_level(dest_obj) < src_obj->activation.difficulty)
    {
        msg_print("Failed! The destination device is not powerful enough to receive the source effect.");
        return FALSE;
    }

    /* Move the effect and re-calc the casting cost to reflect changes in power level */
    dest_obj->activation.type = src_obj->activation.type;
    dest_obj->activation.difficulty = src_obj->activation.difficulty;
    dest_obj->activation.extra = src_obj->activation.extra;
    device_init_cost(dest_obj);

    /* Destroy the source */
    assert(src_obj->number == 1); /* Wands/Rods/Staves no longer stack */
    src_obj->number = 0;
    obj_release(src_obj, 0);

    return TRUE;
}

static bool _transfer_essence(void)
{
    int tval = _speciality_tval(plr->psubclass);
    object_type *src_obj = NULL, *dest_obj = NULL;
    object_kind *src_kind = NULL, *dest_kind = NULL;
    int src_charges = 0, dest_charges = 0, max_charges = 0, power = 0;

    /* Choose the objects */
    src_obj = _get_src_obj();
    if (!src_obj) return FALSE;
    src_kind = &k_info[src_obj->k_idx];

    dest_obj = _get_dest_obj(src_obj);
    if (!dest_obj) return FALSE;
    dest_kind = &k_info[dest_obj->k_idx];

    if (dest_obj == src_obj)
    {
        msg_print("Failed! Please pick distinct objects for the source and destination.");
        return FALSE;
    }
    if (tval == TV_SCROLL || tval == TV_POTION)
    {
        if (dest_kind->level > src_kind->level) /* Double Check ... should already be excluded! */
        {
            msg_print("Failed! You may only transfer to objects of greater or equal power.");
            return FALSE;
        }
    }

    src_charges = src_obj->number;
    max_charges = 99 - dest_obj->number;

    if (max_charges <= 0)
    {
        msg_print("Failed! The destination object is already fully charged.");
        return FALSE;
    }

    power = src_charges * src_kind->level;
    power = power / 2;
    dest_charges = power / MAX(dest_kind->level, 10);

    if (dest_charges > max_charges)
        dest_charges = max_charges;

    if (dest_charges <= 0)
    {
        msg_print("Failed! The source object is not powerful enough to transfer even a single charge.");
        return FALSE;
    }

    /* Perform the transfer */
    dest_obj->number += dest_charges;
    src_obj->number -= src_charges;

    obj_release(dest_obj, 0);
    obj_release(src_obj, 0);
    return TRUE;
}
void _transfer_charges_spell(int cmd, var_ptr res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        if (plr->psubclass != DEVICEMASTER_POTIONS && plr->psubclass != DEVICEMASTER_SCROLLS)
            var_set_string(res, "Transfer Effect");
        else
            var_set_string(res, "Transfer Essence");
        break;
    case SPELL_DESC:
        if (plr->psubclass == DEVICEMASTER_POTIONS)
            var_set_string(res, "Transfer essence from one potion to another.");
        else if (plr->psubclass == DEVICEMASTER_SCROLLS)
            var_set_string(res, "Transfer essence from one scroll to another.");
        else if (plr->psubclass == DEVICEMASTER_WANDS)
            var_set_string(res, "Transfer effect from one wand to another, destroying the source wand.");
        else if (plr->psubclass == DEVICEMASTER_RODS)
            var_set_string(res, "Transfer effect from one rod to another, destroying the source rod.");
        else if (plr->psubclass == DEVICEMASTER_STAVES)
            var_set_string(res, "Transfer effect from one staff to another, destroying the source staff.");
        break;
    case SPELL_CAST:
        if (plr->psubclass != DEVICEMASTER_POTIONS && plr->psubclass != DEVICEMASTER_SCROLLS)
            var_set_bool(res, _transfer_effect());
        else
            var_set_bool(res, _transfer_essence());
        break;
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

bool devicemaster_is_speciality(object_type *o_ptr)
{
    if (plr->pclass == CLASS_DEVICEMASTER)
    {
        if (_speciality_tval(plr->psubclass) == o_ptr->tval)
            return TRUE;
    }
    return FALSE;
}


static void _birth(void) 
{ 
    object_type    forge;

    switch (plr->psubclass)
    {
    case DEVICEMASTER_RODS:
        object_prep(&forge, lookup_kind(TV_ROD, SV_ANY));
        if (device_init_fixed(&forge, EFFECT_DETECT_MONSTERS))
            plr_birth_obj(&forge);
        break;
    case DEVICEMASTER_STAVES:
        object_prep(&forge, lookup_kind(TV_STAFF, SV_ANY));
        if (device_init_fixed(&forge, EFFECT_SLEEP_MONSTERS))
            plr_birth_obj(&forge);
        break;
    case DEVICEMASTER_WANDS:
        object_prep(&forge, lookup_kind(TV_WAND, SV_ANY));
        if (device_init_fixed(&forge, EFFECT_SLEEP_MONSTER))
            plr_birth_obj(&forge);
        break;
    case DEVICEMASTER_POTIONS:
        object_prep(&forge, lookup_kind(TV_POTION, SV_POTION_SPEED));
        forge.number = 6;
        plr_birth_obj(&forge);
        break;
    case DEVICEMASTER_SCROLLS:
        object_prep(&forge, lookup_kind(TV_SCROLL, SV_SCROLL_TELEPORT));
        forge.number = 6;
        plr_birth_obj(&forge);
        break;
    }
    plr_birth_obj_aux(TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 1);
    plr_birth_obj_aux(TV_SWORD, SV_SHORT_SWORD, 1);
    plr_birth_obj_aux(TV_WAND, EFFECT_BOLT_MISSILE, 1);
}

static void _character_dump(doc_ptr doc)
{
    cptr desc = devicemaster_speciality_name(plr->psubclass);

    doc_printf(doc, "<topic:Abilities>================================== <color:keypress>A</color>bilities ==================================\n\n");

    {
        int pow = plr->lev / 10;
        if (pow)
            doc_printf(doc, " * You gain +%d%% power when using %s.\n", device_power_aux(100, pow) - 100, desc);
    }
    doc_printf(doc, " * You use %s more quickly.\n", desc);
    if (plr->psubclass != DEVICEMASTER_POTIONS && plr->psubclass != DEVICEMASTER_SCROLLS)
        doc_printf(doc, " * You have a chance of not consuming a charge when using %s.\n", desc);
    else
        doc_printf(doc, " * You have a chance of not consuming an item when using %s.\n", desc);
    if (plr->psubclass != DEVICEMASTER_POTIONS && plr->psubclass != DEVICEMASTER_SCROLLS)
        doc_printf(doc, " * You may use %s even when frightened.\n", desc);
    doc_printf(doc, " * You are resistant to charge draining (Power=%d).\n\n", plr->lev);

    {
        spell_info spells[MAX_SPELLS];
        int        ct = _get_spells(spells, MAX_SPELLS);

        plr_display_spells(doc, spells, ct);
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
        me.encumbrance.max_wgt = 1000;
        me.encumbrance.weapon_pct = 0;
        me.encumbrance.enc_wgt = 1200;
        init = TRUE;
    }
    return &me;
}

plr_class_ptr devicemaster_get_class(int psubclass)
{
    static plr_class_ptr me = NULL;

    if (!me)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 25,  40,  36,   2,  20,  16,  48,  35 };
    skills_t xs = { 35,  75,  50,   0,   0,   0,  65,  55 };

        me = plr_class_alloc(CLASS_DEVICEMASTER);
        me->name = "Devicemaster";
        me->desc = 
            "Devicemasters are excellent with magical devices, but poor in most other skills. "
            "They may shoot or use melee in a pinch, but this will never be their forte. Instead, "
            "they must rely on their arsenal of magical devices in order to survive.\n \n"
            "The Devicemaster chooses to specialize in a particular class of devices and they "
            "gain extra bonuses when using devices from their speciality. These bonuses include "
            "increased damage, increased speed of activation, extra resistance to charge draining, "
            "and even the ability to occasionally power these devices without consuming charges. "
            "Each of these abilities becomes greater with experience. In addition, they gain enhanced "
            "device skill with speciality devices.\n \n"
            "Devicemasters have a few magical abilities to enhance their utility with devices. As expected, "
            "they gain a powerful talent of Recharging very early on. Also, they may detect magical devices "
            "from a distance. At higher levels, they gain the powerful ability to move effects from "
            "one device to another (wand/rod/staff) or to move essence from one potion/scroll to a "
            "lower level potion/scroll. As a final ability, "
            "the Devicemaster may use multiple charges at once from a given device in an act of "
            "desperation. This greatly increases the power of the effect, but may destroy the device "
            "in the process.";
    
        me->stats[A_STR] = -1;
        me->stats[A_INT] =  2;
        me->stats[A_WIS] =  1;
        me->stats[A_DEX] =  2;
        me->stats[A_CON] = -2;
        me->stats[A_CHR] = -2;
        me->skills = bs;
        me->extra_skills = xs;
        me->life = 101;
        me->base_hp = 6;
        me->exp = 130;
        me->pets = 30;
        me->flags = CLASS_SENSE1_MED | CLASS_SENSE1_WEAK |
                   CLASS_SENSE2_FAST | CLASS_SENSE2_STRONG;

        me->hooks.birth = _birth;
        me->hooks.get_spells = _get_spells;
        me->hooks.character_dump = _character_dump;
        me->hooks.caster_info = _caster_info;
    }

    me->subid = psubclass;
    me->subname = devicemaster_speciality_name(psubclass);
    me->subdesc = devicemaster_speciality_desc(psubclass);
    return me;
}
