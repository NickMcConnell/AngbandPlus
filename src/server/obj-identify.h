/*
 * File: obj-identify.h
 * Purpose: Object identification and knowledge routines
 */

#ifndef OBJECT_IDENTIFY_H
#define OBJECT_IDENTIFY_H

/*
 * Pseudo-ID markers.
 */
typedef enum
{
    INSCRIP_NULL = 0,       /* No pseudo-ID status */
    INSCRIP_AVERAGE,        /* Item with no interesting features */
    INSCRIP_MAGICAL_BAD,    /* Item with bad combat bonuses */
    INSCRIP_STRANGE,        /* Item with mixed combat bonuses */
    INSCRIP_MAGICAL_GOOD,   /* Item with good combat bonuses */
    INSCRIP_SPLENDID,       /* Obviously good item */
    INSCRIP_EXCELLENT,      /* Ego-item */
    INSCRIP_SPECIAL,        /* Artifact */

    INSCRIP_MAX             /* Maximum number of pseudo-ID markers */
} obj_pseudo_t;

extern bool easy_know(const struct object *obj, bool aware);
extern bool object_is_known(struct player *p, const struct object *obj);
extern bool object_is_known_artifact(const struct object *obj);
extern bool object_is_known_not_artifact(const struct object *obj);
extern bool object_was_worn(const struct object *obj);
extern bool object_flavor_is_aware(struct player *p, const struct object *obj);
extern bool object_flavor_was_tried(struct player *p, const struct object *obj);
extern bool object_effect_is_known(const struct object *obj, bool aware);
extern bool object_name_is_visible(const struct object *obj);
extern bool object_ego_is_visible(const struct object *obj);
extern bool object_attack_plusses_are_visible(const struct object *obj);
extern bool object_defence_plusses_are_visible(const struct object *obj);
extern bool object_flag_is_known(struct player *p, const struct object *obj, int flag);
extern bool object_element_is_known(const struct object *obj, int element, bool aware);
extern bool object_this_mod_is_visible(const struct object *obj, int mod);
extern void object_set_base_known(struct player *p, struct object *obj);
extern bool object_check_for_ident(struct player *p, struct object *obj);
extern void object_flavor_aware(struct player *p, struct object *obj);
extern void object_flavor_tried(struct player *p, struct object *obj);
extern void object_know_brands_and_slays(struct object *obj);
extern void object_notice_everything_aux(struct player *p, struct object *obj,
    bool bypass_aware, bool send);
extern void object_notice_everything(struct player *p, struct object *obj);
extern void object_notice_ego(struct player *p, struct object *obj);
extern void object_notice_effect(struct player *p, struct object *obj);
extern void object_notice_attack_plusses(struct player *p, struct object *obj);
extern bool object_notice_element(struct player *p, struct object *obj, int element);
extern bool object_notice_flag(struct player *p, struct object *obj, int flag);
extern bool object_notice_flags(struct player *p, struct object *obj, bitflag flags[OF_SIZE]);
extern bool object_notice_curses(struct player *p, struct object *obj);
extern void object_notice_on_wield(struct player *p, struct object *obj);
extern void object_notice_on_use(struct player *p, struct object *obj);
extern void equip_notice_on_defend(struct player *p);
extern void equip_notice_flag(struct player *p, int flag);
extern void equip_notice_element(struct player *p, int element);
extern void equip_notice_to_hit_on_attack(struct player *p);
extern void equip_notice_on_attack(struct player *p);
extern bool object_high_resist_is_possible(const struct object *obj);
extern bool object_was_sensed(const struct object *obj);
extern void object_notice_sensing(struct player *p, struct object *obj);
extern void object_sense_artifact(struct player *p, struct object *obj);
extern obj_pseudo_t object_pseudo(struct player *p, const struct object *obj, bool aware,
    bool known);
extern void do_ident_item(struct player *p, struct object *obj);
extern void sense_inventory(struct player *p);

#endif /* OBJECT_IDENTIFY_H */
