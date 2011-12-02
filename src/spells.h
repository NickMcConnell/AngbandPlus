/* spells.c */
extern void browse_spells(spell_info* spells, int ct, cptr desc);
extern int calculate_cost(int cost);
extern int calculate_fail_rate(int level, int base_fail, int stat_idx);
extern bool cast_spell(ang_spell spell);
extern int  choose_spell(spell_info* spells, int ct, cptr desc, int max_cost);
extern void default_spell(int cmd, variant *res);
extern void do_cmd_power(void);
extern void do_cmd_spell(void);
extern void do_cmd_spell_browse(void);
extern int get_spell_energy(ang_spell spell);
extern int get_spell_cost_extra(ang_spell spell);
extern int get_spell_fail_min(ang_spell spell);

/* New Magic System ... In progress! */
enum spell_options {
	spell_allow_scroll = 0x01,
	spell_allow_potion = 0x02,
	spell_allow_wand = 0x04,
	spell_allow_staff = 0x08,
	spell_allow_rod = 0x10,
	spell_allow_shop = 0x20,
	spell_allow_read = 0x40, /* Restrict reading of certain scrolls to classes that know the realm */
};

enum spell_realms {
	realm_life = 0x0001,
	realm_sorcery = 0x0002,
	realm_nature = 0x0004,
	realm_chaos = 0x0008,
	realm_death = 0x0010,
	realm_trump = 0x0020,
	realm_arcane = 0x0040,
	realm_craft = 0x0080,
	realm_daemon = 0x0100,
	realm_crusade = 0x0200,
	realm_wild = 0x0400,
	realm_order = 0x0800,
};

typedef struct {
	u32b realm;
	ang_spell spell;
	s16b level;
	s16b cost;
	s16b fail;
	u32b options;
	byte rarity;
} spell_t;

typedef void (*spell_action)(const spell_info *spell);

typedef struct {
	cptr name;
	cptr desc;
	spell_action on_cast;
	spell_action on_fail;
} realm_t;

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
extern bool cast_breathe_fire_I(void);
extern bool cast_cold_touch(void);
extern bool cast_create_food(void);
extern bool cast_dazzle(void);
extern bool cast_destruction(void);
extern bool cast_detect_curses(void);
extern bool cast_detect_doors_stairs_traps(void);
extern bool cast_detect_monsters(void);
extern bool cast_detect_objects(void);
extern bool cast_detect_traps(void);
extern bool cast_detect_treasure(void);
extern bool cast_dimension_door(void);
extern bool cast_earthquake(void);
extern bool cast_eat_magic(void);
extern bool cast_eat_rock(void);
extern bool cast_enchantment(void);
extern bool cast_grow_mold(void);
extern bool cast_heroism(void);
extern bool cast_hypnotic_gaze(void);
extern bool cast_identify(void);
extern bool cast_identify_fully(void);
extern bool cast_laser_eye(void);
extern bool cast_light_area(void);
extern bool cast_magic_mapping(void);
extern bool cast_magic_missile(void);
extern bool cast_mind_blast(void);
extern bool cast_panic_hit(void);
extern bool cast_phase_door(void);
extern bool cast_polish_shield(void);
extern bool cast_polymorph_self(void);
extern bool cast_power_throw(void);
extern bool cast_probing(void);
extern bool cast_protection_from_evil(void);
extern bool cast_radiation(void);
extern bool cast_recall(void);
extern bool cast_recharging(void);
extern bool cast_remove_curse_I(void);
extern bool cast_remove_curse_II(void);
extern bool cast_remove_fear(void);
extern bool cast_resist_elements(void);
extern bool cast_restore_life(void);
extern bool cast_satisfy_hunger(void);
extern bool cast_shriek(void);
extern bool cast_spit_acid(void);
extern bool cast_sterility(void);
extern bool cast_stone_skin(void);
extern bool cast_stone_to_mud(void);
extern bool cast_summon_tree(void);
extern bool cast_swap_pos(void);
extern bool cast_telekinesis(void);
extern bool cast_teleport(void);
extern bool cast_teleport_level(void);
extern bool cast_vampirism(void);
extern bool cast_weigh_magic(void);

/* The Spells ... alot of racial powers that could be
   private are actually public.  Coming up is the "Wild Thing"
   class that gains random racial powers!
*/
extern void alchemy_spell(int cmd, variant *res);
extern void android_ray_gun_spell(int cmd, variant *res);
extern void android_blaster_spell(int cmd, variant *res);
extern void android_bazooka_spell(int cmd, variant *res);
extern void android_beam_cannon_spell(int cmd, variant *res);
extern void android_rocket_spell(int cmd, variant *res);
extern void banish_evil_spell(int cmd, variant *res);
extern void battle_frenzy_spell(int cmd, variant *res);
extern void berserk_spell(int cmd, variant *res);
extern void bless_weapon_spell(int cmd, variant *res);
extern void brain_smash_spell(int cmd, variant *res);
extern void breathe_disintegration_spell(int cmd, variant *res);
extern void breathe_fire_I_spell(int cmd, variant *res);
extern void breathe_fire_II_spell(int cmd, variant *res);
extern void cause_wounds_I_spell(int cmd, variant *res);
extern void cause_wounds_II_spell(int cmd, variant *res);
extern void cause_wounds_III_spell(int cmd, variant *res);
extern void cause_wounds_IV_spell(int cmd, variant *res);
extern void clairvoyance_spell(int cmd, variant *res);
extern void clear_mind_spell(int cmd, variant *res);
extern void cold_touch_spell(int cmd, variant *res);
extern void confusing_lights_spell(int cmd, variant *res);
extern void create_ammo_spell(int cmd, variant *res);
extern void create_food_spell(int cmd, variant *res);
extern void cure_wounds_I_spell(int cmd, variant *res);
extern void darkness_storm_I_spell(int cmd, variant *res);
extern void darkness_storm_II_spell(int cmd, variant *res);
extern void dazzle_spell(int cmd, variant *res);
extern void demon_breath_spell(int cmd, variant *res);
extern void destruction_spell(int cmd, variant *res);
extern void detect_menace_spell(int cmd, variant *res);
extern void detect_curses_spell(int cmd, variant *res);
extern void detect_doors_stairs_traps_spell(int cmd, variant *res);
extern void detect_objects_spell(int cmd, variant *res);
extern void detect_monsters_spell(int cmd, variant *res);
extern void detect_traps_spell(int cmd, variant *res);
extern void detect_treasure_spell(int cmd, variant *res);
extern void dimension_door_spell(int cmd, variant *res);
extern void disintegrate_spell(int cmd, variant *res);
extern void dominate_living_I_spell(int cmd, variant *res);
extern void dominate_living_II_spell(int cmd, variant *res);
extern void double_magic_spell(int cmd, variant *res);
extern void draconian_breath_spell(int cmd, variant *res);
extern void earthquake_spell(int cmd, variant *res);
extern void eat_magic_spell(int cmd, variant *res);
extern void eat_rock_spell(int cmd, variant *res);
extern void enchantment_spell(int cmd, variant *res);
extern void evocation_spell(int cmd, variant *res);
extern void explosive_rune_spell(int cmd, variant *res);
extern void eye_for_an_eye_spell(int cmd, variant *res);
extern void glyph_of_warding_spell(int cmd, variant *res);
extern void grow_mold_spell(int cmd, variant *res);
extern void healing_I_spell(int cmd, variant *res);
extern void healing_II_spell(int cmd, variant *res);
extern void hell_lance_spell(int cmd, variant *res);
extern void heroism_spell(int cmd, variant *res);
extern void holy_lance_spell(int cmd, variant *res);
extern void hp_to_sp_spell(int cmd, variant *res);
extern void hypnotic_gaze_spell(int cmd, variant *res);
extern void identify_spell(int cmd, variant *res);
extern void identify_fully_spell(int cmd, variant *res);
extern void imp_fire_spell(int cmd, variant *res);
extern void kutar_expand_spell(int cmd, variant *res);
extern void laser_eye_spell(int cmd, variant *res);
extern void light_area_spell(int cmd, variant *res);
extern void living_trump_spell(int cmd, variant *res);
extern void magic_missile_spell(int cmd, variant *res);
extern void magic_mapping_spell(int cmd, variant *res);
extern void mana_branding_spell(int cmd, variant *res);
extern void mana_bolt_I_spell(int cmd, variant *res);
extern void mana_bolt_II_spell(int cmd, variant *res);
extern void mana_storm_I_spell(int cmd, variant *res);
extern void mana_storm_II_spell(int cmd, variant *res);
extern void massacre_spell(int cmd, variant *res);
extern void mind_blast_spell(int cmd, variant *res);
extern void orb_of_entropy_spell(int cmd, variant *res);
extern void panic_hit_spell(int cmd, variant *res);
extern void pattern_mindwalk_spell(int cmd, variant *res);
extern void phase_door_spell(int cmd, variant *res);
extern void poison_dart_spell(int cmd, variant *res);
extern void polish_shield_spell(int cmd, variant *res);
extern void polymorph_demonlord_spell(int cmd, variant *res);
extern void polymorph_self_spell(int cmd, variant *res);
extern void power_throw_spell(int cmd, variant *res);
extern void probing_spell(int cmd, variant *res);
extern void protection_from_evil_spell(int cmd, variant *res);
extern void radiation_spell(int cmd, variant *res);
extern void ray_of_sunlight_spell(int cmd, variant *res);
extern void recall_spell(int cmd, variant *res);
extern void recharging_spell(int cmd, variant *res);
extern void remove_curse_I_spell(int cmd, variant *res);
extern void remove_curse_II_spell(int cmd, variant *res);
extern void remove_fear_spell(int cmd, variant *res);
extern void resistance_spell(int cmd, variant *res);
extern void resist_elements_spell(int cmd, variant *res);
extern void resist_poison_spell(int cmd, variant *res);
extern void restore_life_spell(int cmd, variant *res);
extern void rocket_I_spell(int cmd, variant *res);
extern void rocket_II_spell(int cmd, variant *res);
extern void rush_attack_spell(int cmd, variant *res);
extern void satisfy_hunger_spell(int cmd, variant *res);
extern void scare_monster_spell(int cmd, variant *res);
extern void shadow_shifting_spell(int cmd, variant *res);
extern void shoot_arrow_spell(int cmd, variant *res);
extern void shriek_spell(int cmd, variant *res);
extern void sleeping_dust_spell(int cmd, variant *res);
extern void smell_metal_spell(int cmd, variant *res);
extern void smell_monsters_spell(int cmd, variant *res);
extern void sp_to_hp_spell(int cmd, variant *res);
extern void spit_acid_spell(int cmd, variant *res);
extern void starburst_I_spell(int cmd, variant *res);
extern void starburst_II_spell(int cmd, variant *res);
extern void sterility_spell(int cmd, variant *res);
extern void stone_skin_spell(int cmd, variant *res);
extern void stone_to_mud_spell(int cmd, variant *res);
extern void summon_manes_spell(int cmd, variant *res);
extern void summon_tree_spell(int cmd, variant *res);
extern void super_stealth_spell(int cmd, variant *res);
extern void swap_pos_spell(int cmd, variant *res);
extern void sword_dance_spell(int cmd, variant *res);
extern void take_photo_spell(int cmd, variant *res);
extern void telekinesis_spell(int cmd, variant *res);
extern void teleport_spell(int cmd, variant *res);
extern void teleport_level_spell(int cmd, variant *res);
extern void teleport_other_spell(int cmd, variant *res);
extern void throw_boulder_spell(int cmd, variant *res);
extern void vampirism_spell(int cmd, variant *res);
extern void weigh_magic_spell(int cmd, variant *res);
extern void wonder_spell(int cmd, variant *res);
extern void wraithform_spell(int cmd, variant *res);

