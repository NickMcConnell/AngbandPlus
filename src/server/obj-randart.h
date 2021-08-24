/*
 * File: obj-randart.h
 * Purpose: Random artifact generation
 */

#ifndef OBJECT_RANDART_H
#define OBJECT_RANDART_H

#define MAX_TRIES 200

#define MIN_RNAME_LEN 5
#define MAX_RNAME_LEN 9

/*
 * Inhibiting factors for large bonus values
 * "HIGH" values use INHIBIT_WEAK
 * "VERYHIGH" values use INHIBIT_STRONG
 */
#define INHIBIT_STRONG  one_in_(6)
#define INHIBIT_WEAK    one_in_(2)

/*
 * Power rating below which uncursed randarts cannot aggravate
 * (so that aggravate is found only on endgame-quality items or
 * cursed items)
 */
#define AGGR_POWER 300

/*
 * Numerical index values for the different learned probabilities
 * These are to make the code more readable.
 */
enum
{
    #define ART_IDX(a, b) ART_IDX_##a,
    #include "list-randart-properties.h"
    #undef ART_IDX

    ART_IDX_TOTAL
};

struct artifact_set_data
{
    /* Mean start and increment values for to_hit, to_dam and AC */
    int hit_increment;
    int dam_increment;
    int hit_startval;
    int dam_startval;
    int ac_startval;
    int ac_increment;

    /* Data structures for learned probabilities */
    int art_probs[ART_IDX_TOTAL];
    int bow_total;
    int melee_total;
    int boot_total;
    int glove_total;
    int headgear_total;
    int shield_total;
    int cloak_total;
    int armor_total;
    int mstaff_total;
    int missile_total;
    int other_total;
    int total;
    int neg_power_total;

    /* Artifact power ratings */
    int max_power;
    int min_power;
    int avg_power;
    int var_power;
    int avg_tv_power[TV_MAX];
    int min_tv_power[TV_MAX];
    int max_tv_power[TV_MAX];
};

extern int get_new_esp(bitflag flags[OF_SIZE]);
extern struct artifact* do_randart(s32b randart_seed, struct artifact *art);
extern void do_randart_name(s32b randart_seed, char *buffer, int len);
extern void init_randart_generator(void);
extern int get_artifact_level(const struct object *obj);
extern void free_artifact(struct artifact *art);

#endif /* OBJECT_RANDART_H */
