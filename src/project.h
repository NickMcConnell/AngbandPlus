#ifndef PROJECT_H
#define PROJECT_H

#include <QVector>
#include <src/structures.h>

extern QVector<dungeon_coordinates> temp_project_coords;


// project.cpp
extern bool teleport_away(int m_idx, int dis);
extern bool teleport_player(int dis, bool native);
extern void teleport_player_to(int ny, int nx);
extern void teleport_towards(int oy, int ox, int ny, int nx);
extern bool teleport_player_level(int who);
extern int drain_charges(object_type *o_ptr, u32b heal);
extern void disease(int *damage);
extern bool apply_disenchant(int mode);
extern byte gf_color(int type);
extern void take_terrain_hit(int dam, int feat, QString kb_str);
extern void take_hit(int dam, QString kb_str);
extern bool object_hates_feature(int feat, object_type *o_ptr);
extern bool object_hates_location(int y, int x, object_type *o_ptr);
extern void acid_dam(int dam, QString kb_str);
extern void elec_dam(int dam, QString kb_str);
extern void fire_dam(int dam, QString kb_str);
extern void cold_dam(int dam, QString kb_str);
extern void disease(int *damage);
extern bool apply_disenchant(int mode);
extern bool project_m(int who, int y, int x, int dam, int typ, u32b flg);
extern bool project_p(int who, int y, int x, int dam, int typ, QString msg);
extern bool project(int who, int rad, int y0, int x0, int y1, int x1, int dam, int typ,
             u32b flg, int degrees, byte source_diameter);

// project_util.cpp
extern bool project_bolt(int who, int rad, int y0, int x0, int y1, int x1, int dam,
                  int typ, u32b flg);
extern bool project_beam(int who, int rad, int y0, int x0, int y1, int x1, int dam,
                  int typ, u32b flg);
extern bool project_ball(int who, int rad, int y0, int x0, int y1, int x1, int dam,
                  int typ, u32b flg, int source_diameter);
extern bool explosion(int who, int rad, int y0, int x0, int dam, int typ, u32b flg);
extern bool mon_explode(int who, int rad, int y0, int x0, int dam, int typ);
extern bool project_arc(int who, int rad, int y0, int x0, int y1, int x1,
    int dam, int typ, u32b flg, int degrees);
extern bool project_star(int who, int rad, int y0, int x0, int dam, int typ,
    u32b flg);
extern bool project_los(int y0, int x0, int dam, int typ);
extern void clear_project_temp_array(int y, int x, bool room);
extern void spread_cave_temp(int y1, int x1, int range, bool room, bool pass_walls);
extern bool fire_bolt_beam_special(int typ, int dir, int dam, int rad, u32b flg);
extern bool fire_effect_orb(int typ, int dir, int dam, int rad);
extern bool fire_ball(int typ, int dir, int dam, int rad);
extern bool fire_orb(int typ, int dir, int dam, int rad);
extern bool fire_ball_special(int typ, int dir, int dam, int rad, u32b flg, int source_diameter);
extern bool fire_arc_special(int typ, int dir, int dam, int rad, int degrees, u32b flg);
extern bool fire_arc(int typ, int dir, int dam, int rad, int degrees);
extern bool fire_star(int typ, int dam, int rad, u32b flg);
extern bool beam_burst(int y, int x, int typ, int num, int dam);
extern bool fire_swarm(int num, int typ, int dir, int dam, int rad);
extern bool fire_bolt(int typ, int dir, int dam);
extern bool fire_beam(int typ, int dir, int dam, u32b flg);
extern bool fire_bolt_or_beam(int prob, int typ, int dir, int dam);
extern bool beam_chain(int gf_type, int dam, int max_hits, int decrement);

#endif // PROJECT_H
