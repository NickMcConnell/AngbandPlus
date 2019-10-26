#ifndef INCLUDED_MONK_ATTACK_H
#define INCLUDED_MONK_ATTACK_H

extern bool monk_attack_init(void);
extern void monk_attack_shutdown(void);
extern cptr monk_verify_table(cptr name);

extern mon_blow_ptr monk_choose_attack_plr(cptr tbl_name);
extern mon_blow_ptr monk_choose_attack_mon(cptr tbl_name, mon_ptr mon);

#endif
