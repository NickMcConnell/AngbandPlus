/* Android race
 * 
 * + to AC?
 * 
 * Androids can't eat conventional food, use batteries?
 * This includes mushrooms (and high level functional foods) and so is a
 * substantial nerf. As Androids make good fighters, this is reasonable
 * enough.
 * 
 * What about pills? 
 */

#include "player.h"
#include "savefile.h"
#include "z-util.h"

/* Persistent state for the android race */
struct android_state {
	
};

static void android_loadsave(bool complete) {
	if (!complete) {
		if (player->race->state == NULL)
			player->race->state = mem_zalloc(sizeof(struct android_state));

		struct android_state *state = (struct android_state *)player->race->state;
	}
}

/* Start a new character as an Android */
static void android_init(void)
{
	/* Initialise saved state */
	player->race->state = mem_zalloc(sizeof(struct android_state));
}

static void android_levelup(int from, int to)
{
}

/* Install hooks */
void install_race_ANDROID(void)
{
	struct player_race *r = get_race_by_name("Android");
	r->init = android_init;
	r->levelup = android_levelup;
	r->loadsave = android_loadsave;
}
