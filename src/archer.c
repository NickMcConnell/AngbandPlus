#include "angband.h"

static bool _create_ammo_p(object_type *o_ptr)
{
    if (o_ptr->tval == TV_JUNK || o_ptr->tval == TV_SKELETON) return TRUE;
    if (object_is_(o_ptr, TV_CORPSE, SV_SKELETON)) return TRUE;
    return FALSE;
}

static bool _create_arrows(void)
{
    int             item, slot;
    object_type  forge;

    item_tester_hook = _create_ammo_p;
    if (!get_item(&item, "Convert which item? ", "You have no item to convert.", USE_INVEN | USE_FLOOR)) 
        return FALSE;

    object_prep(&forge, lookup_kind(TV_ARROW, m_bonus(1, p_ptr->lev)+ 1));
    forge.number = (byte)rand_range(5, 10);
    apply_magic(&forge, p_ptr->lev, AM_NO_FIXED_ART);
    obj_identify(&forge);
    forge.discount = 99;

    msg_print("You make some ammo.");
    if (item >= 0)
    {
        inven_item_increase(item, -1);
        inven_item_describe(item);
        inven_item_optimize(item);
    }
    else
    {
        floor_item_increase(0 - item, -1);
        floor_item_describe(0 - item);
        floor_item_optimize(0 - item);
    }

    slot = inven_carry(&forge);
    if (slot >= 0) 
        autopick_alter_item(slot, FALSE);
    return TRUE;
}

static bool _create_bolts(void)
{
    int             item, slot;
    object_type  forge;

    item_tester_hook = _create_ammo_p;
    if (!get_item(&item, "Convert which item? ", "You have no item to convert.", USE_INVEN | USE_FLOOR)) 
        return FALSE;

    /* Note: You won't ever get Steel Bolts this way since I:18:1 is shared with 
       by Bolts and Steel Bolts and lookup_kind() picks the first match in k_info.txt.
       However, getting Steel from Bones/Junk would be a bit odd anyway ... */
    object_prep(&forge, lookup_kind(TV_BOLT, m_bonus(1, p_ptr->lev)+ 1));
    forge.number = (byte)rand_range(4, 8);
    apply_magic(&forge, p_ptr->lev, AM_NO_FIXED_ART);
    obj_identify(&forge);
    forge.discount = 99;

    msg_print("You make some ammo.");
    if (item >= 0)
    {
        inven_item_increase(item, -1);
        inven_item_describe(item);
        inven_item_optimize(item);
    }
    else
    {
        floor_item_increase(0 - item, -1);
        floor_item_describe(0 - item);
        floor_item_optimize(0 - item);
    }

    slot = inven_carry(&forge);
    if (slot >= 0) 
        autopick_alter_item(slot, FALSE);
    return TRUE;
}

static bool _create_shots(void)
{
    int         x, y, dir, slot;
    cave_type  *c_ptr;
    object_type forge;

    if (!get_rep_dir(&dir, FALSE)) 
        return FALSE;

    y = py + ddy[dir];
    x = px + ddx[dir];
    c_ptr = &cave[y][x];

    if (!have_flag(f_info[get_feat_mimic(c_ptr)].flags, FF_CAN_DIG))
    {
        msg_print("You need pile of rubble.");
        return FALSE;
    }

    if (!cave_have_flag_grid(c_ptr, FF_CAN_DIG) || !cave_have_flag_grid(c_ptr, FF_HURT_ROCK))
    {
        msg_print("You failed to make ammo.");
        return FALSE;
    }

    object_prep(&forge, lookup_kind(TV_SHOT, m_bonus(1, p_ptr->lev) + 1));
    forge.number = (byte)rand_range(15,30);
    apply_magic(&forge, p_ptr->lev, AM_NO_FIXED_ART);
    obj_identify(&forge);
    forge.discount = 99;

    msg_print("You make some ammo.");

    slot = inven_carry(&forge);
    if (slot >= 0) 
        autopick_alter_item(slot, FALSE);

    cave_alter_feat(y, x, FF_HURT_ROCK);
    p_ptr->update |= PU_FLOW;
    return TRUE;
}

static bool _create_ammo(void)
{
    char com[256];

    if (p_ptr->confused)
    {
        msg_print("You are too confused!");
        return FALSE;
    }
    if (p_ptr->blind)
    {
        msg_print("You can't see!");
        return FALSE;
    }

    if (p_ptr->lev >= 20)
        sprintf(com, "Create [S]hots, Create [A]rrow or Create [B]olt ?");
    else if (p_ptr->lev >= 10)
        sprintf(com, "Create [S]hots or Create [A]rrow ?");
    else
        sprintf(com, "Create [S]hots ?");

    for(;;)
    {
        char ch;
        if (!get_com(com, &ch, TRUE))
            return FALSE;
        if (ch == 'S' || ch == 's')
            return _create_shots();
        if ((ch == 'A' || ch == 'a') && p_ptr->lev >= 10)
            return _create_arrows();
        else if ((ch == 'B' || ch == 'b') && p_ptr->lev >= 20)
            return _create_bolts();
    }
}

void create_ammo_spell(int cmd, variant *res)
{
    switch (cmd)
    {
    case SPELL_NAME:
        var_set_string(res, "Create Ammo");
        break;
    case SPELL_DESC:
        var_set_string(res, "Create arrows, bolts or shots.");
        break;
    case SPELL_CAST:
        var_set_bool(res, _create_ammo());
        break;
    default:
        default_spell(cmd, res);
        break;
    }
}

static void _calc_shooter_bonuses(object_type *o_ptr, shooter_info_t *info_ptr)
{
    if ( !p_ptr->shooter_info.heavy_shoot
      && p_ptr->shooter_info.tval_ammo <= TV_BOLT
      && p_ptr->shooter_info.tval_ammo >= TV_SHOT )
    {
        p_ptr->shooter_info.num_fire += p_ptr->lev * 200 / 50;
    }
}

static int _get_powers(spell_info* spells, int max)
{
    int ct = 0;

    spell_info* spell = &spells[ct++];
    spell->level = 1;
    spell->cost = 0;
    spell->fail = 0;
    spell->fn = create_ammo_spell;

    return ct;
}

class_t *archer_get_class(void)
{
    static class_t me = {0};
    static bool init = FALSE;
    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 38,  24,  35,   4,  24,  16,  56,  82};
    skills_t xs = { 12,  10,  10,   0,   0,   0,  18,  36};

        me.name = "Archer";
        me.desc = "Archers are to bows what warriors are to melee. They are the best "
                    "class around with any bow, crossbow, or sling. They need a lot of "
                    "ammunition, but will learn how to make it from junk found in the "
                    "dungeon. An archer is better than a warrior at stealth, "
                    "perception, searching and magical devices.\n \n"
                    "Archers have a class power - 'Create Ammo' - which creates stones "
                    "or shots from pile of rubble, and arrows and crossbow bolts from "
                    "bones.";

        me.stats[A_STR] =  2;
        me.stats[A_INT] = -1;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] =  2;
        me.stats[A_CON] =  1;
        me.stats[A_CHR] =  0;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 111;
        me.base_hp = 12;
        me.exp = 110;
        me.pets = 40;
        
        me.calc_shooter_bonuses = _calc_shooter_bonuses;
        me.get_powers = _get_powers;
        init = TRUE;
    }
    return &me;
}
