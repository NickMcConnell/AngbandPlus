/*
 * File: player-spell.h
 * Purpose: Spell and prayer casting/praying
 */

#ifndef PLAYER_SPELL_H
#define PLAYER_SPELL_H

struct beam_info
{
    int beam;
    int spell_power;
    int elem_power;
};

extern const int adj_mag_stat[];

extern void player_spells_init(struct player *p);
extern void player_spells_free(struct player *p);
extern const struct class_book *object_kind_to_book(const struct object_kind *kind);
extern const struct class_book *player_object_to_book(struct player *p, const struct object *obj);
extern int object_to_book_index(struct player *p, const struct object *obj);
extern const struct class_spell *spell_by_index(const struct class_magic *magic, int index);
extern s16b spell_chance(struct player *p, int spell_index);
extern bool spell_is_identify(struct player *p, int spell_index);
extern void get_spell_info(struct player *p, int spell_index, char *buf, size_t len);
extern expression_base_value_f spell_value_base_by_name(const char *name);

extern void cast_spell_end(struct player *p);
extern void show_ghost_spells(struct player *p);
extern int antimagic_field(const struct object *obj, bitflag flags[OF_SIZE]);
extern bool check_antimagic(struct player *p, struct chunk *c, struct monster *who);
extern bool check_antisummon(struct player *p, struct monster *mon);
extern void show_mimic_spells(struct player *p);
extern bool cast_spell_proj(struct player *p, int cidx, int spell_index, bool silent);
extern void fill_beam_info(struct player *p, int spell_index, struct beam_info *beam);

#endif /* PLAYER_SPELL_H */
