#include "player.h"
#include "savefile.h"
#include "z-util.h"

/* Persistent state for the X race */
struct x_state {
	
};

static void x_loadsave(void) {
	if (player->race->state == NULL)
		player->race->state = mem_zalloc(sizeof(struct x_state));

	struct x_state *state = (struct x_state *)player->race->state;
}

/* Start a new character as an x */
static void x_init(void)
{
	/* Initialise saved state */
	player->race->state = mem_zalloc(sizeof(struct x_state));
}

static void x_levelup(int from, int to)
{
}

/* Install hooks */
void install_race_X(void)
{
	struct player_race *r = get_race_by_name("Xx");
	r->init = x_init;
	r->levelup = x_levelup;
	r->loadsave = x_loadsave;
}
