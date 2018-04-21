#ifndef RESIST_H
#define RESIST_H

enum {
    RES_INVALID = -1,
    RES_BEGIN = 0,
    RES_ACID = 0,
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
    RES_MAX,
    RES_END = RES_TELEPORT,
};

extern void res_add(int which);
extern void res_add_amt(int which, int amt);
extern void res_add_all(void);
extern void res_add_immune(int which);
extern void res_add_vuln(int which);
extern void res_calc_bonuses(u32b flgs[OF_ARRAY_SIZE]);
extern bool res_has_bonus(u32b flgs[OF_ARRAY_SIZE]);
extern int  res_calc_dam(int which, int dam);
extern bool res_can_ignore(int which);
extern void res_clear(void);
extern bool res_is_high(int which);
extern bool res_is_low(int which);
extern cptr res_name(int which);
extern byte res_color(int which);
extern int  res_pct(int which);
extern int  res_pct_known(int which);
extern int  res_ct_known(int which);
extern int  res_pct_aux(int which, int count);
extern bool res_save(int which, int power);
extern bool res_save_default(int which);
extern bool res_save_inventory(int which);
extern int  res_get_object_flag(int which); /* e.g., RES_FOO -> TR_RES_FOO */
extern int  res_get_object_vuln_flag(int which); /* e.g., RES_FOO -> TR_VULN_FOO | -1 */
extern int  res_get_object_immune_flag(int which); /* e.g., RES_FOO -> TR_IM_FOO | -1 */

#endif
