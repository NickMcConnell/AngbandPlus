/* spells.c */
extern void browse_spells(spell_info* spells, int ct, caster_info *caster);
extern int calculate_fail_rate(const spell_info *spell, int stat_idx);
extern bool cast_spell(ang_spell spell);
extern int  choose_spell(spell_info* spells, int ct, caster_info *caster);
extern void default_spell(int cmd, variant *res);
extern void do_cmd_spell(void);
extern void do_cmd_spell_browse(void);

/* Public Spells:  I'm using the following system for placing code.
   This makes it easier to split a too large file, and easier to locate
   the correct file for a spell.

   Here is an example:
   spells_a.c - Contains spells beginning with 'a' up to next file.
   spells_k.c - Contains spells beginning with 'k' up to next file, etc
*/
extern bool cast_berserk(void);
extern bool cast_breathe_fire(void);
extern bool cast_detect_monsters(void);
extern bool cast_detect_traps(void);
extern bool cast_detect_treasure(void);
extern bool cast_hypnotic_gaze(void);
extern bool cast_light_area(void);
extern bool cast_mind_blast(void);
extern bool cast_phase_door(void);
extern bool cast_polish_shield(void);
extern bool cast_radiation(void);
extern bool cast_recharging(void);
extern bool cast_spit_acid(void);
extern bool cast_summon_tree(void);
extern bool cast_telekinesis(void);
extern bool cast_teleport(void);
extern bool cast_vampirism(void);

extern void breathe_fire_spell(int cmd, variant *res);
extern void berserk_spell(int cmd, variant *res);
extern void detect_monsters_spell(int cmd, variant *res);
extern void detect_traps_spell(int cmd, variant *res);
extern void detect_treasure_spell(int cmd, variant *res);
extern void hypnotic_gaze_spell(int cmd, variant *res);
extern void light_area_spell(int cmd, variant *res);
extern void mind_blast_spell(int cmd, variant *res);
extern void phase_door_spell(int cmd, variant *res);
extern void polish_shield_spell(int cmd, variant *res);
extern void radiation_spell(int cmd, variant *res);
extern void recharging_spell(int cmd, variant *res);
extern void spit_acid_spell(int cmd, variant *res);
extern void summon_tree_spell(int cmd, variant *res);
extern void telekinesis_spell(int cmd, variant *res);
extern void teleport_spell(int cmd, variant *res);
extern void vampirism_spell(int cmd, variant *res);
