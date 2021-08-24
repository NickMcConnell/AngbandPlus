/* Android race
 * 
 * + to AC?
 * 
 * Androids can't eat conventional food, they use batteries.
 * This includes mushrooms (and high level functional foods) and so is a
 * substantial nerf. As Androids make good fighters, this is reasonable
 * enough.
 * 
 * What about pills? 
 */

#include "player.h"
#include "savefile.h"
#include "z-util.h"

static void android_loadsave(bool complete) {
	;
}

/* Start a new character as an Android */
static void android_init(void)
{
	;
}

static void android_levelup(int from, int to)
{
	;
}

/* Install hooks */
void install_race_ANDROID(void)
{
	struct player_race *r = get_race_by_name("Android");
	r->init = android_init;
	r->levelup = android_levelup;
	r->loadsave = android_loadsave;
}
