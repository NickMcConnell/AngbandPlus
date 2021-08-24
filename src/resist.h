#ifndef RESIST_H
#define RESIST_H

extern void res_add(int gf);
extern void res_add_amt(int gf, int amt);
extern void res_add_ultimate(void);
extern void res_add_immune(int gf);
extern void res_add_vuln(int gf);
extern void res_calc_bonuses(u32b flgs[OF_ARRAY_SIZE]);
extern int  res_calc_dam(int gf, int dam);
extern bool res_can_ignore(int gf);
extern void res_clear(void);
extern bool res_is_high(int gf);
extern bool res_is_low(int gf);
extern cptr res_name(int gf);
extern byte res_color(int gf);
extern int  res_pct(int gf);
extern int  res_pct_known(int gf);
extern int  res_ct_known(int gf);
extern int  res_pct_aux(int gf, int count);
extern int  res_pct_mon(int gf);
extern bool res_save(int gf, int power);
extern bool res_save_default(int gf);
extern bool res_save_inventory(int gf);

#endif
