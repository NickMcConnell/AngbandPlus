/* Tourist class
 * This is a challenge class. You don't start with anything very useful (with
 * the exception of the mapping device) and few skills - although you do
 * have good Exp% and ability learning (as this is kind of a 'level 0' class)
 * and OK level-50 skills.
 * You start with some touristy items (e.g. cash, sun hat, loud shirt, flip flops, 
 * snacks, and a mapping device)
 * And occasionally a firecracker (or other item: rumor cards?)
 */

#include "angband.h"
#include "obj-util.h"
#include "player.h"
#include "player-birth.h"
#include "savefile.h"
#include "z-util.h"

/* Persistent state for the Tourist class */
struct tourist_state {
	
};

/* Save or load state, according to the global saving flag */
static void tourist_loadsave(bool complete) {

}

/* Start a new character as a Tourist */
static void tourist_init(void)
{
	/* Some extra spending money (generously, because the expensive mapping
	 * device means that au will probably be 0 at this point. The MAX is in
	 * case of an ability giving cash or the no-start-kit option)
	 **/
	player->au = MAX(damroll(10, 200), player->au + damroll(10, 80));
	player->au_birth = player->au;

	/* Possible random item */
	struct start_item item = { 0, 0, 1, 1, NULL };
	bool items = false;
	switch(randint0(10)) {
		case 0:
			item.tval = TV_LIGHT;
			item.sval = lookup_sval(TV_LIGHT, "firecracker");
			items = true;
			break;
		case 1:
		case 2:
			item.tval = TV_CARD;
			item.sval = lookup_sval(TV_LIGHT, "rumor");
			item.max = randint1(6);
			items = true;
			break;
		default:
			player->au += 30;
	}
	player->au_birth = player->au;
	if (items)
		add_start_items(player, &item, (!OPT(player, birth_start_kit)), false, ORIGIN_BIRTH);
}

/* Gain a new level (or levels) for the first time */
static void tourist_levelup(int from, int to)
{

}

/* Enter or exit a town building */
static void tourist_building(int store, bool entering, bool *do_default)
{

}

/* Install hooks */
void install_class_TOURIST(void)
{
	struct player_class *c = get_class_by_name("Tourist");
	c->init = tourist_init;
	c->levelup = tourist_levelup;
	c->building = tourist_building;
	c->loadsave = tourist_loadsave;
}
