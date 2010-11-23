/* spells.c */
extern void browse_spells(spell_info* spells, int ct, caster_info *caster);
extern int calculate_fail_rate(const spell_info *spell, int stat_idx);
extern bool cast_spell(ang_spell spell);
extern int  choose_spell(spell_info* spells, int ct, caster_info *caster);
extern void default_spell(int cmd, variant *res);
extern void do_cmd_power(void);
extern void do_cmd_spell(void);
extern void do_cmd_spell_browse(void);

/* Public Spells:  I'm using the following system for placing code.
   This makes it easier to split a too large file, and easier to locate
   the correct file for a spell.

   Here is an example:
   spells_a.c - Contains spells beginning with 'a' up to next file.
   spells_k.c - Contains spells beginning with 'k' up to next file, etc
*/
extern bool cast_alchemy(void);
extern bool cast_banish_evil(void);
extern bool cast_berserk(void);
extern bool cast_breathe_fire(void);
extern bool cast_cold_touch(void);
extern bool cast_dazzle(void);
extern bool cast_detect_curses(void);
extern bool cast_detect_monsters(void);
extern bool cast_detect_traps(void);
extern bool cast_detect_treasure(void);
extern bool cast_earthquake(void);
extern bool cast_eat_magic(void);
extern bool cast_eat_rock(void);
extern bool cast_grow_mold(void);
extern bool cast_hypnotic_gaze(void);
extern bool cast_laser_eye(void);
extern bool cast_light_area(void);
extern bool cast_mind_blast(void);
extern bool cast_panic_hit(void);
extern bool cast_phase_door(void);
extern bool cast_polish_shield(void);
extern bool cast_polymorph_self(void);
extern bool cast_power_throw(void);
extern bool cast_radiation(void);
extern bool cast_recall(void);
extern bool cast_recharging(void);
extern bool cast_resist_elements(void);
extern bool cast_shriek(void);
extern bool cast_spit_acid(void);
extern bool cast_sterility(void);
extern bool cast_summon_tree(void);
extern bool cast_swap_pos(void);
extern bool cast_telekinesis(void);
extern bool cast_teleport(void);
extern bool cast_vampirism(void);
extern bool cast_weigh_magic(void);

extern void alchemy_spell(int cmd, variant *res);
extern void banish_evil_spell(int cmd, variant *res);
extern void berserk_spell(int cmd, variant *res);
extern void breathe_fire_spell(int cmd, variant *res);
extern void cold_touch_spell(int cmd, variant *res);
extern void dazzle_spell(int cmd, variant *res);
extern void detect_curses_spell(int cmd, variant *res);
extern void detect_monsters_spell(int cmd, variant *res);
extern void detect_traps_spell(int cmd, variant *res);
extern void detect_treasure_spell(int cmd, variant *res);
extern void earthquake_spell(int cmd, variant *res);
extern void eat_magic_spell(int cmd, variant *res);
extern void eat_rock_spell(int cmd, variant *res);
extern void grow_mold_spell(int cmd, variant *res);
extern void hypnotic_gaze_spell(int cmd, variant *res);
extern void laser_eye_spell(int cmd, variant *res);
extern void light_area_spell(int cmd, variant *res);
extern void mind_blast_spell(int cmd, variant *res);
extern void panic_hit_spell(int cmd, variant *res);
extern void phase_door_spell(int cmd, variant *res);
extern void polish_shield_spell(int cmd, variant *res);
extern void polymorph_self_spell(int cmd, variant *res);
extern void power_throw_spell(int cmd, variant *res);
extern void radiation_spell(int cmd, variant *res);
extern void recall_spell(int cmd, variant *res);
extern void recharging_spell(int cmd, variant *res);
extern void resist_elements_spell(int cmd, variant *res);
extern void shriek_spell(int cmd, variant *res);
extern void smell_metal_spell(int cmd, variant *res);
extern void smell_monsters_spell(int cmd, variant *res);
extern void spit_acid_spell(int cmd, variant *res);
extern void sterility_spell(int cmd, variant *res);
extern void summon_tree_spell(int cmd, variant *res);
extern void swap_pos_spell(int cmd, variant *res);
extern void telekinesis_spell(int cmd, variant *res);
extern void teleport_spell(int cmd, variant *res);
extern void vampirism_spell(int cmd, variant *res);
extern void weigh_magic_spell(int cmd, variant *res);