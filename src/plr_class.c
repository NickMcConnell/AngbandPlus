#include "angband.h"

#include <assert.h>
#include "int_map.h"
#include "str_map.h"

/************************************************************************
 * Class
 ************************************************************************/
typedef struct {
    int             id;
    cptr            name;
    plr_class_ptr (*class_f)(void);
    plr_class_ptr (*subclass_f)(int subclass);
} _class_info_t, *_class_info_ptr;

static _class_info_t _class_tbl[] = {
    {CLASS_WARRIOR, "Warrior", warrior_get_class, NULL},
    {CLASS_MAGE, "Mage", mage_get_class, NULL},
    {CLASS_PRIEST, "Priest", priest_get_class, NULL},
    {CLASS_ROGUE, "Rogue", rogue_get_class, NULL},
    {CLASS_RANGER, "Ranger", ranger_get_class, NULL},
    {CLASS_PALADIN, "Paladin", paladin_get_class, NULL},
    {CLASS_WARRIOR_MAGE, "Warrior-Mage", warrior_mage_get_class, NULL},
    {CLASS_CHAOS_WARRIOR, "Chaos-Warrior", chaos_warrior_get_class, NULL},
    {CLASS_MONK, "Monk", monk_get_class, NULL},
    {CLASS_MINDCRAFTER, "Mindcrafter", mindcrafter_get_class, NULL},
    {CLASS_HIGH_MAGE, "High-Mage", high_mage_get_class, NULL},
    {CLASS_BEASTMASTER, "Beastmaster", beastmaster_get_class, NULL},
    {CLASS_SORCERER, "Sorcerer", sorcerer_get_class, NULL},
    {CLASS_ARCHER, "Archer", archer_get_class, NULL},
    {CLASS_MAGIC_EATER, "Magic-Eater", magic_eater_get_class, NULL},
    {CLASS_BARD, "Bard", bard_get_class, NULL},
    {CLASS_RED_MAGE, "Red-Mage", red_mage_get_class, NULL},
    {CLASS_SAMURAI, "Samurai", samurai_get_class, NULL},
    {CLASS_FORCETRAINER, "Force-Trainer", force_trainer_get_class, NULL},
    {CLASS_CAVALRY, "Cavalry", cavalry_get_class, NULL},
    {CLASS_WEAPONSMITH, "Weaponsmith", weaponsmith_get_class, NULL},
    {CLASS_MIRROR_MASTER, "Mirror-Master", mirror_master_get_class, NULL},
    {CLASS_NINJA, "Ninja", ninja_get_class, NULL},
    {CLASS_SNIPER, "Sniper", sniper_get_class, NULL},
    {CLASS_TIME_LORD, "Time-Lord", time_lord_get_class, NULL},
    {CLASS_BLOOD_KNIGHT, "Blood-Knight", blood_knight_get_class, NULL},
    {CLASS_WARLOCK, "Warlock", NULL, warlock_get_class},
    {CLASS_ARCHAEOLOGIST, "Archaeologist", archaeologist_get_class, NULL},
    {CLASS_DUELIST, "Duelist", duelist_get_class, NULL},
    {CLASS_WILD_TALENT, "Wild-Talent", wild_talent_get_class, NULL},
    {CLASS_RUNE_KNIGHT, "Rune-Knight", rune_knight_get_class, NULL},
    {CLASS_WEAPONMASTER, "Weaponmaster", NULL, weaponmaster_get_class},
    {CLASS_NECROMANCER, "Necromancer", necromancer_get_class, NULL},
    {CLASS_PSION, "Psion", psion_get_class, NULL},
    {CLASS_RAGE_MAGE, "Rage-Mage", rage_mage_get_class, NULL},
    {CLASS_SCOUT, "Scout", scout_get_class, NULL},
    {CLASS_MAULER, "Mauler", mauler_get_class, NULL},
    {CLASS_MONSTER, "Monster", monster_get_class, NULL},
    {CLASS_MYSTIC, "Mystic", mystic_get_class, NULL},
    {CLASS_DEVICEMASTER, "Devicemaster", NULL, devicemaster_get_class},
    {CLASS_YELLOW_MAGE, "Yellow-Mage", yellow_mage_get_class, NULL},
    {CLASS_GRAY_MAGE, "Gray-Mage", NULL, gray_mage_get_class},
    {CLASS_SKILLMASTER, "Skillmaster", skillmaster_get_class, NULL},
    {CLASS_BLUE_MAGE, "Blue-Mage", blue_mage_get_class, NULL},
    {CLASS_HIGH_PRIEST, "High-Priest", high_priest_get_class, NULL},
    {0}
};
plr_class_ptr plr_class_alloc(int class_id) { return plr_class_alloc_aux(class_id, 0); }
plr_class_ptr plr_class_alloc_aux(int class_id, int subclass_id)
{
    plr_class_ptr c = malloc(sizeof(plr_class_t));
    memset(c, 0, sizeof(plr_class_t));
    c->id = class_id;
    c->subid = subclass_id;
    return c;
}
void plr_class_free(plr_class_ptr cls)
{
    if (cls)
        free(cls);
}
int plr_class_parse(cptr name)
{
    static str_map_ptr map = NULL;
    _class_info_ptr info;
    int result = CLASS_NONE;
    if (!map)
    {
        int i;
        map = str_map_alloc(NULL);
        for (i = 0; ; i++)
        {
            _class_info_ptr info = &_class_tbl[i];
            if (!info->name) break;
            str_map_add(map, info->name, info);
        }
    }
    info = str_map_find(map, name);
    if (info) result = info->id;
    return result;
}
plr_class_ptr plr_class_aux(int class_id, int subclass_id)
{
    static int_map_ptr map = NULL;
    _class_info_ptr info;
    class_ptr result;
    if (!map)
    {
        int i;
        map = int_map_alloc(NULL);
        for (i = 0; ; i++)
        {
            _class_info_ptr info = &_class_tbl[i];
            if (!info->name) break;
            int_map_add(map, info->id, info);
        }
    }
    info = int_map_find(map, class_id);
    assert(info);
    if (info->subclass_f) result = info->subclass_f(subclass_id);
    else result = info->class_f();
    assert(result);
    result->id = class_id;
    result->subid = subclass_id;
    return result;
}
plr_class_ptr plr_class(void) { return plr_class_aux(plr->pclass, plr->psubclass); }


