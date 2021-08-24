#ifndef INCLUDE_FEAR_H
#define INCLUDE_FEAR_H

#define FEAR_BOLD           0
#define FEAR_UNEASY         1
#define FEAR_NERVOUS       30
#define FEAR_SCARED        50
#define FEAR_TERRIFIED    100
#define FEAR_PETRIFIED    250

extern int  fear_level_p(void);
extern void fear_clear_p(void);
extern bool fear_add_p(int amount);
extern bool fear_set_p(int amount);

/* Saving Throws */
extern int  fear_threat_level(void);
extern bool fear_save_p(int ml);
extern bool fear_save_m(monster_type *m_ptr);
extern bool life_save_p(int ml);

/* Fear restricts player options */
extern bool fear_allow_device(void);
extern bool fear_allow_magic(void);
extern bool fear_allow_melee(mon_ptr mon);
extern bool fear_allow_shoot(void);

/* Some monsters inspire fear (RF2_AURA_FEAR) */
extern void fear_p_touch_m(monster_type *m_ptr);
extern void fear_process_p(void);
extern void fear_update_m(monster_type *m_ptr);

/* Others have fear based attacks (RBE_TERRIFY and RF5_SCARE) */
extern void fear_terrify_p(monster_type *m_ptr);
extern void fear_scare_p(monster_type *m_ptr);

/* Shaking off the effects of fear */
extern void fear_recover_p(void);

/* Taking damage can be very frightening */
extern void fear_heal_p(int old_hp, int new_hp);
extern void fear_hurt_p(int old_hp, int new_hp);

/* Monster Fear */
extern bool fear_p_hurt_m(mon_ptr mon, int dam);
extern bool fear_process_m(mon_ptr mon);

#endif
