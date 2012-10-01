/* 
 * This file contains stuff I used to be able to compile NewAngband 1.1.0 in Windows.
 * Eytan Zweig
 */

/* birth.c */
extern void no_more_items();
extern void no_more_kills();
extern void change_back_to_apprentice();
extern void update_and_handle();
extern void make_sword_devastation(void);
extern void do_cmd_create_dagger(void);
extern void do_cmd_create_sword(void);
extern void do_cmd_create_2hsword(void);
extern void do_cmd_create_flaming(void);
extern void get_hellqueen_history();
extern void apply_valkyrie_weapon_magic();
extern void apply_skatter_quiver_magic();
extern void valkyrie_create_spear(void);
extern void create_mage_staff(void);
extern void do_cmd_create_cold(void);
extern void do_cmd_create_elec(void);
extern void do_cmd_create_super_defender(void);
extern void do_cmd_change_class(void);

/* cmd1.c */
extern void do_cmd_damage_weapon();

/* cmd2.c */
extern void change_age();
extern void do_cmd_skatt_jump();
extern void do_cmd_dark_aura_info(void);
extern void turn_in_crystal(monster_type *m_ptr);

/* cmd3.c */
extern void do_cmd_auto_wield(object_type *o_ptr);

/* cmd5.c */
extern void hellqueen_mana_blast();
extern void do_cmd_bless_weapon(void);
extern void do_cmd_lite_charm();
extern void do_cmd_divine_bolt();
extern void wave_kick();
extern bool energy_spin();
extern void skatter_enchant_bow();
extern void skatter_create_arrows(void);
extern void firelord_fireball();
extern bool firelord_fireaura();
extern void firelord_firestorm();
extern void repair_weapon();
extern void get_history(void);
extern void do_cmd_staff_balance();
extern void skatter_exploding_arrow();
extern bool circle_of_force();
extern void devastation_beam();
extern void do_cmd_flooding();
extern void special_weapon_charge();

/* cmd6.c */
extern void do_cmd_asmodis(object_type *o_ptr);

/* cmd7.c */
extern void do_cmd_check_flooding();
extern bool fear_monsters(void);

/* Files.c */
extern void show_itemmake_prices(void);

/* learn.c */
extern bool ability(int abil);
extern void create_ability(int svalnum);
extern bool ring(int rsval);
extern int learn_ability();
extern int use_monster_power(bool great, bool only_number);
extern void pet_skills(int m_idx);

/* melee1.c */
extern void damage_armor();

/* monster2.c */
extern void apply_monster_level_hp(monster_type *m_ptr);

/* object1.c */
extern void do_cmd_make_item(object_type *o_ptr);
extern void do_cmd_breed();
extern void do_cmd_more_pval();

/* spells2.c */
extern bool dark_lord_aura(int dam, int rad);
extern bool valkyrie_aura(int dam, int rad);
extern bool fire_ball_pets(monster_type *m_ptr, int typ, int dam, int rad);
extern bool project_hook_pets(monster_type *m_ptr, int typ, int dir, int dam, int flg);

/* store.c */
extern void give_silly_love_message();