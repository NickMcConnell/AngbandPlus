#ifndef DUN_GENERATE_H
#define DUN_GENERATE_H

#include "src/dun_gen_structures.h"



extern bool allow_uniques;

/*
 * Dungeon generation data -- see "dun_gen_capabilities.cpp"
 */
extern dun_data *dun;




extern fractal_template fractal_repository[SIZE_FRACTAL_REPOSITORY];



// Dungeon level tables
extern flags_themed_levels themed_level_flags[CUR_NUM_THEME_LEVEL_FLAGS];
extern byte dungeon_size_tab[7][7];
extern const room_data room[ROOM_MAX];
extern level_feature_themes themed_level_features[CUR_NUM_THEME_LEVEL_FEATURES];
extern fractal_dim_struct fractal_dim[MAX_FRACTAL_TYPES];
extern elemental_transformation_info elemental_transformations[CUR_NUM_ELEMENTAL_TRANSFORMATIONS];
extern room_data lake_data[MAX_LAKE_DATA];


// dun_generate functions
extern int next_to_walls(int y, int x);
extern bool new_player_spot_old(void);
extern void basic_granite(void);
extern void set_perm_boundry(void);
extern bool scramble_and_connect_rooms_stairs(void);
extern bool place_traps_rubble_player(void);
extern void build_tunnel(int row1, int col1, int row2, int col2);
extern bool alloc_stairs(u16b feat, int num);
extern bool place_monsters_objects(void);
extern void alloc_object(int set, int typ, int num);


// dun_gen_rooms functions
extern void mark_g_vault(int y0, int x0, int hgt, int wid);
extern void build_vault(int y0, int x0, const vault_type *v_ptr);
extern bool room_build(int by0, int bx0, int typ);
extern void generate_fill(int y1, int x1, int y2, int x2, u16b feat);

// dun_gen_features functions
extern bool generate_starburst_room(int y1, int x1, int y2, int x2,
                                           u16b feat, u16b edge, u32b flag);

extern bool build_type_fractal(int y0, int x0, byte type);
extern void build_type_starburst(int y0, int x0, bool giant_room);
extern void build_nature(void);
extern void build_misc_features(void);
extern void build_formation(int y0, int x0, u16b feat, byte fractal_type, int chance, u16b feat2, byte mode);

extern void fractal1_init_func(fractal_map map, fractal_template *t_ptr);
extern void fractal2_init_func(fractal_map map, fractal_template *t_ptr);
extern void fractal3_init_func(fractal_map map, fractal_template *t_ptr);
extern void fractal4_init_func(fractal_map map, fractal_template *t_ptr);
extern void fractal5_init_func(fractal_map map, fractal_template *t_ptr);

//dun_gen_quest_levels.cpp
extern bool build_wilderness_level(void);
extern bool monster_wilderness_labrynth_okay(int r_idx);
extern bool build_themed_level(void);
extern bool build_arena_level(void);
extern bool build_labyrinth_level(void);
extern bool build_greater_vault_level(void);



#endif // DUN_GENERATE_H
