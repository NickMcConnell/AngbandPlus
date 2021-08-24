#ifndef INCLUDED_DUN_PROJECT_H
#define INCLUDED_DUN_PROJECT_H

typedef struct dun_path_s dun_path_t, *dun_path_ptr;
typedef struct dun_blast_s dun_blast_t, *dun_blast_ptr;
typedef struct dun_blast_point_s dun_blast_point_t, *dun_blast_point_ptr;
typedef struct dun_line_gen_s dun_line_gen_t, *dun_line_gen_ptr;
typedef struct n_choose_k_s n_choose_k_t, *n_choose_k_ptr;

#define DUN_PATH_MAX 18
#define DUN_VIEW_MAX 20
#define DUN_BLAST_MAX 10  /* cf MAX_PRECOMPUTE_DISTANCE */

/*************************************************************************
 * Discrete line generation. Unlike line_gen_t, this only works on
 * lines of max length DUN_VIEW_MAX. Generation is discrete and symmetric
 * when possible. When not possible, different discretizations can easily
 * be tried via dun_line_gen_next_strategy. See ../lib/edit/n_choose_k.txt
 *************************************************************************/
struct dun_line_gen_s
{
    line_t         line;

    /* information about the line: valid for each strategy */
    bool           horizontal;
    point_t        step;
    int            major_length;
    int            minor_length;
    int            segment_count;       /* N */
    int            base_segment_length;
    int            segment_remainder;   /* k */
    n_choose_k_ptr strategies;          /* allocate segment_remainder to N_choose_k of the segments */

    /* current iteration using "current_strategy" */
    int            current_strategy;
    u32b           current_mask;

    int            current_segment;
    int            current_segment_length;
    int            segment_step;
    int            major_count;
    int            minor_count;
    point_t        current_point;
};
extern dun_line_gen_ptr dun_line_gen_alloc(line_t line);
extern void dun_line_gen_create(dun_line_gen_ptr gen, line_t line);
extern void dun_line_gen_destroy(dun_line_gen_ptr gen);
extern void dun_line_gen_free(dun_line_gen_ptr gen);
extern bool dun_line_gen_next_strategy(dun_line_gen_ptr gen);
extern point_t dun_line_gen_first(dun_line_gen_ptr gen);
extern point_t dun_line_gen_next(dun_line_gen_ptr gen);
extern int  dun_line_gen_distance(dun_line_gen_ptr gen);

extern point_vec_ptr dun_line_points(line_t l);

extern errr parse_n_choose_k(void);

/*************************************************************************
 * dun_los and _project
 *
 * Line-of-sight: aLb <==> bLa <==> dun_los(a,b)
 * Projectable  : aPb <==> bPa <==> dun_project(a,b)
 * View         : V(a) = {b|aLb} U {some wall grids} ... almost ;)
 *************************************************************************/
extern bool dun_los(dun_ptr dun, point_t p1, point_t p2);
extern bool dun_in_disintegration_range(dun_ptr dun, point_t p1, point_t p2);

extern bool dun_project(dun_ptr dun, point_t p1, point_t p2);
extern bool dun_project_aux(dun_ptr dun, point_t p1, point_t p2, u32b flags, int range);
extern bool dun_stop_project(dun_ptr dun, point_t pos, point_t stop, u32b flags);

extern void dun_update_view(dun_ptr dun);
extern void dun_forget_view(dun_ptr dun);
extern void dun_wizard_view(dun_ptr dun);
extern bool plr_view(point_t pos);
extern void plr_view_iter(void (*f)(dun_ptr dun, point_t pos));

extern void dun_update_light(dun_ptr dun);
extern void dun_update_mon_light(dun_ptr dun);
extern void dun_forget_light(dun_ptr dun);
extern int  plr_light(point_t pos);

extern void delayed_visual_update(dun_ptr dun);


/*************************************************************************
 * dun_path
 *************************************************************************/
struct dun_path_s
{
    dun_ptr dun;
    point_t start, stop;
    point_t points[DUN_VIEW_MAX];
    int count;
    u32b flags;
};
extern dun_path_ptr dun_path_alloc(dun_ptr dun, point_t start, point_t stop, u32b flags);
extern dun_path_ptr dun_path_alloc_aux(dun_ptr dun, point_t start, point_t stop, u32b flags, int range);
extern void dun_path_free(dun_path_ptr path);

void dun_path_fix(dun_path_ptr path, int gf); /* fix a "ball" path to explode before hitting walls */

/*************************************************************************
 * dun_blast
 *************************************************************************/
struct dun_blast_point_s
{
    point_t pos;
    int distance;
};
struct dun_blast_s
{
    who_t who;
    int dam;
    u32b flags;

    /* read-only */
    dun_ptr dun;
    int gf;
    int count;
    int allocated;
    dun_blast_point_ptr points;
    point_t center;

    int offsets[DUN_PATH_MAX + DUN_BLAST_MAX];
    int radius;
    /* offsets encodes "distance" information for animation purposes:
     * The range [offsets[r], offsets[r+1]) gives indices into points
     * for "distance" r along the animation. Use point.distance for
     * various gf_affect_* calls. */

    bool notice;
    bool turned; /* rage-mage */
};
extern dun_blast_ptr dun_blast_ball(dun_ptr dun, point_t src, int rad, int gf);
extern dun_blast_ptr dun_blast_burst(dun_ptr dun, point_t src, int rad, int gf);
extern dun_blast_ptr dun_blast_beam(dun_path_ptr path, int gf);
extern dun_blast_ptr dun_blast_breath(dun_path_ptr path, int rad, int gf);
extern dun_blast_ptr dun_blast_triangle(dun_ptr dun, triangle_t t, int gf);
extern void dun_blast_free(dun_blast_ptr blast);

typedef bool (*dun_blast_f)(dun_blast_ptr blast, dun_blast_point_t pos);
extern bool dun_blast_iter(dun_blast_ptr blast, dun_blast_f f);

/*************************************************************************
 * Spell Projections
 *
 * Bolt:   A reflectable missile from src to target; hits first monster on path.
 * Beam:   Path from src thru target to range, affecting every grid along path.
 * Ball:   Path from src to target; explode radius ball at target (or before if in wall).
 * Breath: Special blast from src to target that "fans out" affecting all grids along way.
 * Burst:  Explode radius ball around src. Adjacent grids take full damage.
 *         This is useful for exploding monsters and staff effects centered on plr.
 * Rocket: Non-reflectable bolt from src to target; explode radius ball at target.
 * LOS:    Affect monsters and plr with LOS to src.
 *
 * Every spell projection is a separate "event". Monsters and the plr can
 * only be affected once per event.
 *************************************************************************/
/* plr spells */
extern bool plr_bolt(point_t target, int gf, int dam);
extern bool plr_bolt_aux(point_t target, int gf, int dam, int range);
extern bool plr_beam(point_t target, int gf, int dam);
extern bool plr_beam_aux(point_t target, int gf, int dam, int range);
extern bool plr_bolt_or_beam(point_t target, int gf, int dam, int beam_chance);
extern bool plr_ball(int rad, point_t target, int gf, int dam);
extern bool plr_ball_direct(int rad, point_t target, int gf, int dam);
extern bool plr_ball_hide(int rad, point_t target, int gf, int dam);
extern bool plr_ball_aux(int rad, point_t target, int gf, int dam, int range);
extern bool plr_breath(int rad, point_t target, int gf, int dam);
extern bool plr_breath_aux(int rad, point_t target, int gf, int dam, int range);
extern bool plr_burst(int rad, int gf, int dam);  /* damage is scaled so that radius 1 receives full "dam" */
extern bool plr_burst_hide(int rad, int gf, int dam);
extern bool plr_rocket(int rad, point_t target, int gf, int dam);
extern bool plr_project_los(int gf, int dam);

/* plr devices */
extern bool device_bolt(point_t target, int gf, int dam);
extern bool device_bolt_aux(point_t target, int gf, int dam, int range);
extern bool device_beam(point_t target, int gf, int dam);
extern bool device_beam_aux(point_t target, int gf, int dam, int range);
extern bool device_ball(int rad, point_t target, int gf, int dam);
extern bool device_ball_aux(int rad, point_t target, int gf, int dam, int range);
extern bool device_breath(int rad, point_t target, int gf, int dam);
extern bool device_breath_aux(int rad, point_t target, int gf, int dam, int range);
extern bool device_burst(int rad, int gf, int dam);
extern bool device_rocket(int rad, point_t target, int gf, int dam);
extern bool device_project_los(int gf, int dam);

/* mon spells */
extern bool mon_bolt(mon_ptr mon, point_t target, int gf, int dam);
extern bool mon_bolt_aux(mon_ptr mon, point_t target, int gf, int dam, int range);
extern bool mon_beam(mon_ptr mon, point_t target, int gf, int dam);
extern bool mon_beam_aux(mon_ptr mon, point_t target, int gf, int dam, int range);
extern bool mon_ball(mon_ptr mon, int rad, point_t target, int gf, int dam);
extern bool mon_ball_aux(mon_ptr mon, int rad, point_t target, int gf, int dam, int range);
extern bool mon_breath(mon_ptr mon, int rad, point_t target, int gf, int dam);
extern bool mon_breath_aux(mon_ptr mon, int rad, point_t target, int gf, int dam, int range);
extern bool mon_burst(mon_ptr mon, int rad, int gf, int dam);
extern bool mon_rocket(mon_ptr mon, int rad, point_t target, int gf, int dam);
extern bool mon_project_los(mon_ptr mon, int gf, int dam);

/* mon ai checks */
extern bool mon_clean_bolt(mon_ptr mon, point_t target);
extern bool mon_clean_bolt_aux(mon_ptr mon, point_t target, int range);
extern bool mon_clean_beam(mon_ptr mon, point_t target);
extern bool mon_clean_beam_aux(mon_ptr mon, point_t target, int gf, int range);
extern bool mon_clean_breath(mon_ptr mon, point_t target, int rad, int gf);

/* lower level api for traps, et. al. */
extern bool dun_burst(dun_ptr dun, who_t who, int rad, point_t pos, int gf, int dam);

/*************************************************************************
 * Specialty Spell Projections
 *
 * These often trigger multiple "events" per call. For example, a meteor
 * shower drops mutliple "bursts" at random locations around the src.
 *************************************************************************/
/* mirror master */
extern bool plr_seeker_ray(point_t target, int dam);
extern bool plr_super_ray(point_t target, int dam);
extern bool plr_binding_field(int dam);

/* plr specialty spells: these are generalizations of indicated spells */
extern bool plr_meteor_shower(int count, int rad, int gf, dice_t dice); /* GF_METEOR */
extern bool plr_wrath_of_god(point_t target, int gf, dice_t dice); /* GF_DISINTEGRATE */
extern bool mon_wrath_of_god(mon_ptr mon, point_t target, int gf, dice_t dice); /* GF_DISINTEGRATE */
extern bool plr_star_dust(int count, int spread, point_t target, int gf, dice_t dice); /* GF_LIGHT */
extern bool plr_star_ball(int count, int gf, dice_t dice);
extern bool plr_star_light(int count, int gf, dice_t dice);

#ifdef DEVELOPER
extern int_stat_t wrath_of_god_stats(dun_ptr dun, point_t target);
#endif

#endif
