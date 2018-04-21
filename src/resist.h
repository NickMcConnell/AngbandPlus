#ifndef RESIST_H
#define RESIST_H

enum {
    RES_ACID,
    RES_ELEC,
    RES_FIRE,
    RES_COLD,
    RES_POIS,
    RES_LITE,
    RES_DARK,
    RES_CONF,
    RES_NETHER,
    RES_NEXUS,
    RES_SOUND,
    RES_SHARDS,
    RES_CHAOS,
    RES_DISEN,
    RES_TIME,
    RES_BLIND,
    RES_FEAR,
    RES_TELEPORT,
    RES_MAX
};

extern void res_add(int which);
extern void res_add_amt(int which, int amt);
extern void res_add_all(void);
extern void res_add_immune(int which);
extern void res_add_vuln(int which);
extern void res_calc_bonuses(u32b flgs[TR_FLAG_SIZE]);
extern int  res_calc_dam(int which, int dam);
extern bool res_can_ignore(int which);
extern void res_clear(void);
extern cptr res_name(int which);
extern int  res_pct(int which);
extern int  res_pct_aux(int which, int count);
extern bool res_save(int which, int power);
extern bool res_save_default(int which);
extern bool res_save_inventory(int which);
extern int  res_get_object_flag(int which); /* e.g., RES_FIRE -> TR_RES_FIRE */

#endif
