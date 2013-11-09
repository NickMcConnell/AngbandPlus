#include "angband.h"

static void _calc_shooter_bonuses(object_type *o_ptr, shooter_info_t *info_ptr)
{
    if (info_ptr->tval_ammo == TV_SHOT)
        info_ptr->num_fire += (p_ptr->lev * 4);
}


static caster_info * _caster_info(void)
{
    static caster_info me = {0};
    static bool init = FALSE;
    if (!init)
    {
        me.magic_desc = "spell";
        me.which_stat = A_INT;
        me.weight = 400;
        me.min_level = 5;
        me.min_fail = 5;
        me.options = CASTER_ALLOW_DEC_MANA | CASTER_GLOVE_ENCUMBRANCE;
        init = TRUE;
    }
    return &me;
}

class_t *rogue_get_class_t(void)
{
    static class_t me = {0};
    static bool init = FALSE;

    if (!init)
    {           /* dis, dev, sav, stl, srh, fos, thn, thb */
    skills_t bs = { 45,  37,  36,   5,  32,  24,  60,  66};
    skills_t xs = { 15,  12,  10,   0,   0,   0,  21,  18};

        me.name = "Rogue";
        me.desc = "A Rogue is a character that prefers to live by his cunning, but is "
                    "capable of fighting his way out of a tight spot. Rogues are good "
                    "at locating hidden traps and doors and are masters of "
                    "disarming traps and picking locks. A rogue has a high stealth "
                    "allowing him to sneak around many creatures without having to "
                    "fight, or to get in a telling first blow. A rogue may also "
                    "backstab a fleeing monster. Rogues also gain shooting bonuses "
                    "when using a sling. Intelligence determines a rogue's "
                    "spell casting ability.\n \n"
                    "Rogues can select one realm from Sorcery, Death, Trump, Arcane, Craft, "
                    "or Burglary. Except for this last realm, rogues have certain limitations " 
                    "on which spells they can learn, and they do not learn new spells "
                    "very quickly. The Burglary Realm however is unique to the rogue and "
                    "offers spells for setting traps, picking pockets, negotiating with "
                    "other thieves, and escaping from a tight spot. Burglary rogues are "
                    "agents of the Black Market and receive favorable pricing from "
                    "that shop.";

        me.stats[A_STR] =  2;
        me.stats[A_INT] =  1;
        me.stats[A_WIS] = -1;
        me.stats[A_DEX] =  3;
        me.stats[A_CON] =  1;
        me.stats[A_CHR] =  1;
        me.base_skills = bs;
        me.extra_skills = xs;
        me.life = 110;
        me.base_hp = 12;
        me.exp = 125;
        me.pets = 40;
        
        me.caster_info = _caster_info;
        me.calc_shooter_bonuses = _calc_shooter_bonuses;
        /* TODO: This class uses spell books, so we are SOL
        me.get_spells = _get_spells;*/
        me.character_dump = spellbook_character_dump;
        init = TRUE;
    }

    return &me;
}
