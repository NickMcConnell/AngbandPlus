#ifndef INCLUDED_PY_THROW_H
#define INCLUDED_PY_THROW_H

#include "obj.h"

enum
{
    THROW_BOOMERANG = 0x01, /* samurai; weapon and skill masters */
    THROW_DISPLAY   = 0x02, /* for the character sheet */
};
#define DIR_CANCEL   0
#define DIR_TARGET   5
#define DIR_RANDOM  10

typedef struct monster_type mon_t, *mon_ptr;
typedef struct py_throw_s py_throw_t, *py_throw_ptr;

struct py_throw_s
{
/* Input */
int     type;
obj_ptr obj;      /* NULL=>prompt */
int     range;    /* 0=>calculate range based on obj->wgt and str */
int     skill;    /* non-zero for extra skill (code will add p_ptr->skill_tht and obj->to_h). */
int     mult;     /* scaled by 100. default 100 */
int     to_d;     /* extra damage (e.g. ninja shuriken) */
int     to_dd;    /* extra damage dice (e.g. Flying Dagger stance) */
int     dir;      /* 0=>prompt; 5=>target; 10=>random; else is a keypad direction */
int     back_chance; /* BOOMERANG: code will add adj_dex_th[DEX] + 1d30 ... you set the base. */
int     energy;   /* 0=>normal energy use (100) */

/* State */
char    obj_name[MAX_NLEN];
u32b    flags[OF_ARRAY_SIZE];
u16b    path[MAX_SIGHT];
int     path_ct;
int     path_pos;
int     break_chance;
bool    come_back; /* BOOMERANG: if (back_chance > 30) ... */
bool    fail_catch;/* BOOMERANG: if (back_chance <= 37) ... */

/* Hooks for Customization */
int   (*mod_damage_f)(py_throw_ptr context, int dam);
void  (*after_hit_f)(py_throw_ptr context, int m_idx); /* e.g. confuse or stun effects */
};

extern bool py_throw(py_throw_ptr context);
extern void py_throw_doc(py_throw_ptr context, doc_ptr doc);

#endif
