/* Skeleton class
 * Copy from this when creating a new class.
 * It's not intended to be compiled as is!
 */

#include "player.h"
#include "savefile.h"
#include "z-util.h"

/* Persistent state for the x class */
struct x_state {
	
};

/* Save or load state, according to the global saving flag */
static void x_loadsave(bool complete) {
	if (player->class->state == NULL)
		player->class->state = mem_zalloc(sizeof(struct x_state));

	struct x_state *state = (struct x_state *)player->class->state;
	/* rdwr_xxx */
}

/* Start a new character as a X */
static void x_init(void)
{
	/* Initialise saved state */
	player->class->state = mem_zalloc(sizeof(struct x_state));
}

/* Gain a new level (or levels) for the first time */
static void x_levelup(int from, int to)
{
	struct x_state *state = (struct x_state *)player->class->state;
}

/* Enter or exit a town building */
static void x_building(int store, bool entering, bool *do_default)
{
	struct x_state *state = (struct x_state *)player->class->state;
}

/* Install hooks */
void install_class_X(void)
{
	struct player_class *c = get_class_by_name("x");
	c->init = x_init;
	c->levelup = x_levelup;
	c->building = x_building;
	c->loadsave = x_loadsave;
}
