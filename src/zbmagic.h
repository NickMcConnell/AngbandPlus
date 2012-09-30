/* zbmagic1.c */
extern bool borg_desperate;
extern int borg_goto_dir(int x1, int y1, int x2, int y2);
extern bool borg_eat_food_any(void);
extern bool borg_surrounded(void);
extern int borg_freedom(int x, int y);
extern bool borg_happy_grid_bold(int x, int y);
extern void borg_target(int x, int y);
extern void borg_near_monster_type(int dist);
extern void borg_press_faint_accept(void);
extern bool borg_escape(int b_q);
extern bool borg_heal(int danger);

/* zbmagic2.c */
extern bool borg_simulate;	/* Simulation flag */
extern int borg_launch_beam(int dam, int typ, int max);
extern void borg_temp_fill(void);
