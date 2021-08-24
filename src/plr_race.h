#ifndef INCLUDED_PLR_RACE_H
#define INCLUDED_PLR_RACE_H

/************************************************************************
 * Race
 ************************************************************************/
enum {
    RACE_NONE = -1,

    /* normal player races */
    RACE_HUMAN = 0,
    RACE_WATER_ELF,
    RACE_DEMIGOD,
    RACE_HOBBIT,
    RACE_GNOME,
    RACE_DWARF,
    RACE_SNOTLING,
    RACE_HALF_TROLL,
    RACE_AMBERITE,
    RACE_HIGH_ELF,
    RACE_BARBARIAN,
    RACE_HALF_OGRE,
    RACE_HALF_GIANT,
    RACE_HALF_TITAN,
    RACE_CYCLOPS,
    RACE_YEEK,
    RACE_KLACKON,
    RACE_KOBOLD,
    RACE_NIBELUNG,
    RACE_DARK_ELF,
    RACE_DRACONIAN,
    RACE_MIND_FLAYER,
    RACE_IMP,
    RACE_GOLEM,
    RACE_SKELETON,
    RACE_ZOMBIE,
    RACE_VAMPIRE,
    RACE_SPECTRE,
    RACE_SPRITE,
    RACE_BEASTMAN,
    RACE_ENT,
    RACE_ARCHON,
    RACE_BALROG,
    RACE_DUNADAN,
    RACE_SHADOW_FAIRY,
    RACE_KUTAR,
    RACE_ANDROID,
    RACE_CENTAUR,
    RACE_WOOD_ELF,
    RACE_DOPPELGANGER,
    RACE_DRIDER,
    RACE_TENGU,
    MAX_RACES,  /* XXX try to stop using this! cf building_type and spoilers */

    /* player monster races for monster mode */
    RACE_MON_JELLY = 500,
    RACE_MON_SPIDER,
    RACE_MON_DRAGON,
    RACE_MON_LICH,
    RACE_MON_XORN,
    RACE_MON_ANGEL,
    RACE_MON_HOUND,
    RACE_MON_GIANT,
    RACE_MON_BEHOLDER,
    RACE_MON_DEMON,
    RACE_MON_HYDRA,
    RACE_MON_LEPRECHAUN,
    RACE_MON_TROLL,
    RACE_MON_ELEMENTAL,
    RACE_MON_SWORD,
    RACE_MON_GOLEM,
    RACE_MON_QUYLTHULG,
    RACE_MON_POSSESSOR,
    RACE_MON_VAMPIRE,
    RACE_MON_RING,
    RACE_MON_MIMIC,
    RACE_MON_CENTIPEDE,
    RACE_MON_VORTEX,

    /* player mimic races for spellcasting (tim_mimic and mimic_form) */
    MIMIC_DEMON = 1000,
    MIMIC_DEMON_LORD,
    MIMIC_VAMPIRE,
    MIMIC_CLAY_GOLEM,
    MIMIC_IRON_GOLEM,
    MIMIC_MITHRIL_GOLEM,
    MIMIC_COLOSSUS,
    MIMIC_SMALL_KOBOLD,
    MIMIC_MANGY_LEPER,
    MIMIC_BAT,
    MIMIC_MIST,
    MIMIC_WOLF,
};
struct race_s; /* XXX define this here and rename */
extern plr_race_ptr plr_race_alloc(int race_id);
extern plr_race_ptr plr_race_alloc_aux(int race_id, int subrace_id);
extern void         plr_race_free(plr_race_ptr race);

extern int          plr_race_parse(cptr name);
extern plr_race_ptr plr_race_aux(int race_id, int subrace_id);
extern plr_race_ptr plr_race(void);
extern plr_race_ptr plr_true_race(void);
extern int          plr_race_polymorph(void);


#endif
