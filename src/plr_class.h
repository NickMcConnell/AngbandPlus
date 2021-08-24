#ifndef INCLUDED_PLR_CLASS_H
#define INCLUDED_PLR_CLASS_H

/************************************************************************
 * Class
 ************************************************************************/
enum {
    CLASS_NONE = -1,
    CLASS_WARRIOR = 0,
    CLASS_MAGE,
    CLASS_PRIEST,
    CLASS_ROGUE,
    CLASS_RANGER,
    CLASS_PALADIN,
    CLASS_WARRIOR_MAGE,
    CLASS_CHAOS_WARRIOR,
    CLASS_MONK,
    CLASS_MINDCRAFTER,
    CLASS_HIGH_MAGE,
    CLASS_BEASTMASTER,
    CLASS_SORCERER,
    CLASS_ARCHER,
    CLASS_MAGIC_EATER,
    CLASS_BARD,
    CLASS_RED_MAGE,
    CLASS_SAMURAI,
    CLASS_FORCETRAINER,
    CLASS_CAVALRY,
    CLASS_WEAPONSMITH,
    CLASS_MIRROR_MASTER,
    CLASS_NINJA,
    CLASS_SNIPER,
    CLASS_TIME_LORD,
    CLASS_BLOOD_KNIGHT,
    CLASS_WARLOCK,
    CLASS_ARCHAEOLOGIST,
    CLASS_DUELIST,
    CLASS_WILD_TALENT,
    CLASS_RUNE_KNIGHT,
    CLASS_WEAPONMASTER,
    CLASS_NECROMANCER,
    CLASS_PSION,
    CLASS_RAGE_MAGE,
    CLASS_SCOUT,
    CLASS_MAULER,
    CLASS_MONSTER,
    CLASS_MYSTIC,
    CLASS_DEVICEMASTER,
    CLASS_YELLOW_MAGE,
    CLASS_GRAY_MAGE,
    CLASS_SKILLMASTER,
    CLASS_BLUE_MAGE,
    CLASS_HIGH_PRIEST,
    MAX_CLASS,  /* XXX try to stop using this! */
};
struct class_s; /* XXX define this here and rename */
extern plr_class_ptr plr_class_alloc(int class_id);
extern plr_class_ptr plr_class_alloc_aux(int class_id, int subclass_id);
extern void          plr_class_free(plr_class_ptr cls);

extern int           plr_class_parse(cptr name);
extern plr_class_ptr plr_class_aux(int class_id, int subclass_id);
extern plr_class_ptr plr_class(void);


#endif
