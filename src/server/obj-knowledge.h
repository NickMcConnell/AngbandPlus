/*
 * File: obj-knowledge.h
 * Purpose: Object knowledge
 */

#ifndef OBJECT_KNOWLEDGE_H
#define OBJECT_KNOWLEDGE_H

enum rune_variety
{
    RUNE_VAR_COMBAT,
    RUNE_VAR_MOD,
    RUNE_VAR_RESIST,
    RUNE_VAR_BRAND,
    RUNE_VAR_SLAY,
    RUNE_VAR_CURSE,
    RUNE_VAR_FLAG
};

enum combat_runes
{
    COMBAT_RUNE_TO_A = 0,
    COMBAT_RUNE_TO_H,
    COMBAT_RUNE_TO_D,
    COMBAT_RUNE_MAX
};

struct rune
{
    enum rune_variety variety;
    int index;
    const char *name;
};

extern int max_runes(void);
extern enum rune_variety rune_variety(size_t i);
extern bool player_knows_rune(struct player *p, size_t i);
extern char *rune_name(size_t i);
extern char *rune_desc(size_t i);
extern bool player_knows_brand(struct player *p, int i);
extern bool player_knows_slay(struct player *p, int i);
extern bool player_knows_curse(struct player *p, int index);
extern bool player_knows_ego(struct player *p, struct ego_item *ego);
extern bool object_effect_is_known(const struct object *obj, bool aware);
extern bool object_is_known_artifact(const struct object *obj);
extern bool object_has_standard_to_h(const struct object *obj);
extern bool object_runes_known(const struct object *obj);
extern bool object_fully_known(struct player *p, const struct object *obj);
extern bool object_flag_is_known(struct player *p, const struct object *obj, int flag);
extern bool object_element_is_known(const struct object *obj, int element, bool aware);
extern void object_set_base_known(struct player *p, struct object *obj);
extern void object_sense(struct player *p, struct object *obj);
extern void player_know_object(struct player *p, struct object *obj);
extern void update_player_object_knowledge(struct player *p);
extern void player_learn_flag(struct player *p, int flag);
extern void player_learn_innate(struct player *p);
extern void player_learn_everything(struct player *p);
extern void object_learn_unknown_rune(struct player *p, struct object *obj);
extern void object_learn_on_wield(struct player *p, struct object *obj);
extern void object_learn_on_use(struct player *p, struct object *obj);
extern void object_learn_slay(struct player *p, int index);
extern void object_learn_brand(struct player *p, int index);
extern void missile_learn_on_ranged_attack(struct player *p, struct object *obj);
extern void equip_learn_on_defend(struct player *p);
extern void equip_learn_on_ranged_attack(struct player *p);
extern void equip_learn_on_melee_attack(struct player *p);
extern bool equip_learn_flag(struct player *p, int flag);
extern void equip_learn_element(struct player *p, int element);
extern void equip_learn_after_time(struct player *p);
extern bool easy_know(const struct object *obj, bool aware);
extern bool object_flavor_is_aware(struct player *p, const struct object *obj);
extern bool object_flavor_was_tried(struct player *p, const struct object *obj);
extern void object_flavor_aware(struct player *p, struct object *obj);
extern void object_flavor_tried(struct player *p, struct object *obj);

/* PWMAngband: original object knowledge */

extern bool object_all_but_flavor_is_known(const struct object *obj, bool aware);
extern bool object_non_curse_runes_known(const struct object *obj, bool aware);
extern bool object_is_known(struct player *p, const struct object *obj);
extern bool object_was_worn(const struct object *obj);
extern bool object_check_for_ident(struct player *p, struct object *obj);
extern void object_know_all_flags(struct object *obj);
extern void object_know_all_elements(struct object *obj);
extern void object_know_brands_and_slays(struct object *obj);
extern void object_know_curses(struct object *obj);
extern void object_notice_everything_aux(struct player *p, struct object *obj, bool bypass_aware,
    bool send);
extern void object_notice_everything(struct player *p, struct object *obj);
extern void object_notice_ego(struct player *p, struct object *obj);
extern void object_notice_effect(struct player *p, struct object *obj);
extern void object_notice_defence_plusses(struct player *p, struct object *obj);
extern void object_notice_attack_plusses(struct player *p, struct object *obj);
extern bool object_notice_element(struct player *p, struct object *obj, int element);
extern bool object_notice_flag(struct player *p, struct object *obj, int flag);
extern bool object_was_sensed(const struct object *obj);
extern void object_notice_sensing(struct player *p, struct object *obj);
extern void object_know_everything(struct player *p, struct object *obj);
extern void object_id_set_aware(struct object *obj);

#endif /* OBJECT_KNOWLEDGE_H */
