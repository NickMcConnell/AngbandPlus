#ifndef INCLUDED_MON_ATTACK_H
#define INCLUDED_MON_ATTACK_H

typedef struct mon_attack_s mon_attack_t, *mon_attack_ptr;
typedef void (*mon_attack_init_f)(mon_attack_ptr context);
struct mon_attack_s
{
/* State: Info about monster who is fighting */
int             stop;       /* non-zero ends the carnage and gives the reason why (e.g. STOP_MON_DEAD) */
mon_ptr         mon;        /* attacking monster */
mon_race_ptr    race;       /* XXX duplicate of mon->race, but needed to detect polymorph race change */
int             to_h;
char            mon_full_name[MAX_NLEN];
char            mon_name[MAX_NLEN];
char            mon_name_obj[MAX_NLEN];
point_t         mon_pos;    /* remember starting pos to detect monster relocation. distance attacks ok */
point_t         tgt_pos;    /* remember starting pos to detect target relocation (e.g. Nexus attacks) */

mon_ptr         mon2;       /* Dest monster ... NULL => attacking player */
mon_race_ptr    race2;
char            mon2_full_name[MAX_NLEN];
char            mon2_name[MAX_NLEN];
char            mon2_name_obj[MAX_NLEN];

/* State: Current "Weapon" */
int             which;
mon_blow_ptr    blow;       /* race->blows[which] */
int             blow_lore;

int             hits;
int             misses;
int             dam_total;  /* running total */
int             dam;        /* current blow */
int             ac;         /* player or monster ... recomputed for each strike */
bool            fear;       /* status of attacking mon (from mon_take_hit) */
int             to_dd;      /* boost damage dice of mon */

/* Delayed effects */
bool            do_blink;

/* Hooks for Customization */
void          (*begin_f)(mon_attack_ptr context);     /* e.g. boost your mount's prowess; samurai posture to cancel attack */
void          (*after_hit_f)(mon_attack_ptr context); /* e.g. apply custom auras */

u32b            flags;      /* Internal flags for retaliation, unview, etc */
};


extern bool mon_attack(mon_ptr mon, point_t pos);   /* full round of attacks vs plr or mon (depending on pos) */
extern bool mon_retaliate_plr(mon_ptr mon);         /* retaliate vs player using random blow */
extern bool mon_retaliate_mon(mon_ptr mon, mon_ptr mon2);

extern bool mon_attack_begin(mon_attack_ptr context, mon_ptr mon, point_t pos);
extern void mon_attack_end(mon_attack_ptr context);
extern mon_attack_ptr mon_attack_current(void);

extern bool mon_check_hit(mon_attack_ptr context);

extern bool mon_hit_plr(mon_attack_ptr context);    /* single strike with blow against plr */
extern bool mon_hit_mon(mon_attack_ptr context);

extern void mon_on_hit_plr(mon_attack_ptr context);  /* apply player auras to context->mon */
extern void mon_on_hit_mon(mon_attack_ptr context);  /* apply context->mon2 auras to context->mon */

#endif
