#include "angband.h"

#include <assert.h>
#include "int_map.h"
#include "str_map.h"

/************************************************************************
 * Race
 ************************************************************************/
#define _RACE_IS_MONSTER 0x0001
#define _RACE_NO_POLY    0x0002
#define _RACE_IS_MIMIC   0x0004
typedef struct {
    int            id;
    cptr           name;
    plr_race_ptr (*race_f)(void);
    plr_race_ptr (*subrace_f)(int subrace);
    int            flags;
} _race_info_t, *_race_info_ptr;

static _race_info_t _race_tbl[] = {
    {RACE_HUMAN, "Human", human_get_race, NULL, _RACE_NO_POLY},
    {RACE_WATER_ELF, "Water-Elf", water_elf_get_race, NULL, 0},
    {RACE_DEMIGOD, "Demigod", NULL, demigod_get_race, _RACE_NO_POLY},
    {RACE_HOBBIT, "Hobbit", hobbit_get_race, NULL, 0},
    {RACE_GNOME, "Gnome", gnome_get_race, NULL, 0},
    {RACE_DWARF, "Dwarf", dwarf_get_race, NULL, 0},
    {RACE_SNOTLING, "Snotling", snotling_get_race, NULL, 0},
    {RACE_HALF_TROLL, "Half-Troll", half_troll_get_race, NULL, 0},
    {RACE_AMBERITE, "Amberite", amberite_get_race, NULL, 0},
    {RACE_HIGH_ELF, "High-Elf", high_elf_get_race, NULL, 0},
    {RACE_BARBARIAN, "Barbarian", barbarian_get_race, NULL, 0},
    {RACE_HALF_OGRE, "Half-Ogre", half_ogre_get_race, NULL, 0},
    {RACE_HALF_GIANT, "Half-Giant", half_giant_get_race, NULL, 0},
    {RACE_HALF_TITAN, "Half-Titan", half_titan_get_race, NULL, 0},
    {RACE_CYCLOPS, "Cyclops", cyclops_get_race, NULL, 0},
    {RACE_YEEK, "Yeek", yeek_get_race, NULL, 0},
    {RACE_KLACKON, "Klackon", klackon_get_race, NULL, 0},
    {RACE_KOBOLD, "Kobold", kobold_get_race, NULL, 0},
    {RACE_NIBELUNG, "Nibelung", nibelung_get_race, NULL, 0},
    {RACE_DARK_ELF, "Dark-Elf", dark_elf_get_race, NULL, 0},
    {RACE_DRACONIAN, "Draconian", NULL, draconian_get_race, _RACE_NO_POLY},
    {RACE_MIND_FLAYER, "Mindflayer", mindflayer_get_race, NULL, 0},
    {RACE_IMP, "Imp", imp_get_race, NULL, 0},
    {RACE_GOLEM, "Golem", golem_get_race, NULL, 0},
    {RACE_SKELETON, "Skeleton", skeleton_get_race, NULL, 0},
    {RACE_ZOMBIE, "Zombie", zombie_get_race, NULL, 0},
    {RACE_VAMPIRE, "Vampire", vampire_get_race, NULL, 0},
    {RACE_SPECTRE, "Spectre", spectre_get_race, NULL, 0},
    {RACE_SPRITE, "Sprite", sprite_get_race, NULL, 0},
    {RACE_BEASTMAN, "Beastman", beastman_get_race, NULL, 0},
    {RACE_ENT, "Ent", ent_get_race, NULL, 0},
    {RACE_ARCHON, "Archon", archon_get_race, NULL, 0},
    {RACE_BALROG, "Balrog", balrog_get_race, NULL, 0},
    {RACE_DUNADAN, "Dunadan", dunadan_get_race, NULL, 0},
    {RACE_SHADOW_FAIRY, "Shadow-Fairy", shadow_fairy_get_race, NULL, 0},
    {RACE_KUTAR, "Kutar", kutar_get_race, NULL, 0},
    {RACE_ANDROID, "Android", android_get_race, NULL, _RACE_NO_POLY},
    {RACE_CENTAUR, "Centaur", centaur_get_race, NULL, 0},
    {RACE_WOOD_ELF, "Wood-Elf", wood_elf_get_race, NULL, 0},
    {RACE_DOPPELGANGER, "Doppelganger", doppelganger_get_race, NULL, _RACE_NO_POLY},
    {RACE_DRIDER, "Drider", drider_get_race, NULL, 0},
    {RACE_TENGU, "Tengu", tengu_get_race, NULL, 0},

    {RACE_MON_JELLY, "Jelly", mon_jelly_get_race, NULL, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_SPIDER, "Spider", NULL, mon_spider_get_race, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_DRAGON, "Dragon", NULL, mon_dragon_get_race, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_LICH, "Lich", NULL, mon_lich_get_race, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_XORN, "Xorn", mon_xorn_get_race, NULL, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_ANGEL, "Angel", mon_angel_get_race, NULL, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_HOUND, "Hound", mon_hound_get_race, NULL, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_GIANT, "Giant", NULL, mon_giant_get_race, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_BEHOLDER, "Beholder", mon_beholder_get_race, NULL, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_DEMON, "Demon", NULL, mon_demon_get_race, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_HYDRA, "Hydra", mon_hydra_get_race, NULL, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_LEPRECHAUN, "Leprechaun", mon_leprechaun_get_race, NULL, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_TROLL, "Troll", NULL, mon_troll_get_race, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_ELEMENTAL, "Elemental", NULL, mon_elemental_get_race, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_SWORD, "Sword", mon_sword_get_race, NULL, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_GOLEM, "Golem", NULL, mon_golem_get_race, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_QUYLTHULG, "Quylthulg", mon_quylthulg_get_race, NULL, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_POSSESSOR, "Possessor", mon_possessor_get_race, NULL, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_VAMPIRE, "Vampire", mon_vampire_get_race, NULL, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_RING, "Ring", mon_ring_get_race, NULL, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_MIMIC, "Mimic", mon_mimic_get_race, NULL, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_CENTIPEDE, "Centipede", mon_centipede_get_race, NULL, _RACE_IS_MONSTER | _RACE_NO_POLY},
    {RACE_MON_VORTEX, "Vortex", mon_vortex_get_race, NULL, _RACE_IS_MONSTER | _RACE_NO_POLY},

    {MIMIC_DEMON, "Demon", demon_get_race, NULL, _RACE_IS_MIMIC | _RACE_NO_POLY},
    {MIMIC_DEMON_LORD, "Demon-Lord", demon_lord_get_race, NULL, _RACE_IS_MIMIC | _RACE_NO_POLY},
    {MIMIC_VAMPIRE, "Vampire-Lord", vampire_lord_get_race, NULL, _RACE_IS_MIMIC | _RACE_NO_POLY},
    {MIMIC_CLAY_GOLEM, "Clay-Golem", clay_golem_get_race, NULL, _RACE_IS_MIMIC | _RACE_NO_POLY},
    {MIMIC_IRON_GOLEM, "Iron-Golem", iron_golem_get_race, NULL, _RACE_IS_MIMIC | _RACE_NO_POLY},
    {MIMIC_MITHRIL_GOLEM, "Mithril-Golem", mithril_golem_get_race, NULL, _RACE_IS_MIMIC | _RACE_NO_POLY},
    {MIMIC_COLOSSUS, "Colossus", colossus_get_race, NULL, _RACE_IS_MIMIC | _RACE_NO_POLY},
    {MIMIC_SMALL_KOBOLD, "Small-Kobold", small_kobold_get_race, NULL, _RACE_IS_MIMIC | _RACE_NO_POLY},
    {MIMIC_MANGY_LEPER, "Mangy-Leper", mangy_leper_get_race, NULL, _RACE_IS_MIMIC | _RACE_NO_POLY},
    {MIMIC_BAT, "Bat", bat_get_race, NULL, _RACE_IS_MIMIC | _RACE_NO_POLY},
    {MIMIC_MIST, "Mist", mist_get_race, NULL, _RACE_IS_MIMIC | _RACE_NO_POLY},
    {MIMIC_WOLF, "Wolf", wolf_get_race, NULL, _RACE_IS_MIMIC | _RACE_NO_POLY},
    {0}
};

plr_race_ptr plr_race_alloc(int race_id) { return plr_race_alloc_aux(race_id, 0); }
plr_race_ptr plr_race_alloc_aux(int race_id, int subrace_id)
{
    plr_race_ptr r = malloc(sizeof(plr_race_t));
    memset(r, 0, sizeof(plr_race_t));
    r->id = race_id;
    r->subid = subrace_id;
    return r;
}
void plr_race_free(plr_race_ptr race)
{
    if (race)
        free(race); 
}
int plr_race_parse(cptr name)
{
    static str_map_ptr map = NULL;
    _race_info_ptr info;
    int result = RACE_NONE;
    if (!map)
    {
        int i;
        map = str_map_alloc(NULL);
        for (i = 0; ; i++)
        {
            _race_info_ptr info = &_race_tbl[i];
            if (!info->name) break;
            if (info->flags & _RACE_IS_MONSTER) continue; /* XXX this will break r_info parsing */
            str_map_add(map, info->name, info);
        }
    }
    info = str_map_find(map, name);
    if (info) result = info->id;
    return result;
}
plr_race_ptr plr_race_aux(int race_id, int subrace_id)
{
    static int_map_ptr map = NULL;
    _race_info_ptr info;
    race_ptr result;
    if (!map)
    {
        int i;
        map = int_map_alloc(NULL);
        for (i = 0; ; i++)
        {
            _race_info_ptr info = &_race_tbl[i];
            if (!info->name) break;
            int_map_add(map, info->id, info);
        }
    }
    info = int_map_find(map, race_id);
    assert(info);
    if (info->subrace_f) result = info->subrace_f(subrace_id);
    else result = info->race_f();
    assert(result);
    result->id = race_id;
    result->subid = subrace_id;
    return result;
}
plr_race_ptr plr_race(void)
{
    plr_race_ptr result;
    if (plr->mimic_form != MIMIC_NONE)
    {
        result = plr_race_aux(plr->mimic_form, 0);
        result->mimic = TRUE;
    }
    else
    {
        result = plr_race_aux(plr->prace, plr->psubrace);
        result->mimic = FALSE;
    }
    return result;
}
plr_race_ptr plr_true_race(void)
{
    return plr_race_aux(plr->prace, plr->psubrace);
}
int plr_race_polymorph(void)
{
    int i, tot = 0, n;
    for (i = 0; ; i++)
    {
        _race_info_ptr info = &_race_tbl[i];
        if (!info->name) break;
        if (info->flags & _RACE_NO_POLY) continue;
        if (info->id == plr->prace) continue;
        tot++;
    }
    if (!tot) return plr->prace;
    n = randint0(tot);
    for (i = 0; ; i++)
    {
        _race_info_ptr info = &_race_tbl[i];
        if (!info->name) break;
        if (info->flags & _RACE_NO_POLY) continue;
        if (info->id == plr->prace) continue;
        n--;
        if (n < 0) return info->id;
    }
    return plr->prace;
}

