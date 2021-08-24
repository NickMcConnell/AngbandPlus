#include "game-event.h"
#include "obj-gear.h"
#include "obj-knowledge.h"
#include "obj-make.h"
#include "obj-pile.h"
#include "obj-power.h"
#include "player.h"
#include "player-ability.h"
#include "player-calcs.h"
#include "savefile.h"
#include "z-util.h"

static void cyborg_loadsave(bool s) {
}

/* Start a new character as a cyborg */
static void cyborg_init(void)
{
	struct player *p = player;

	/* Initialise saved state */
	player->race->state = NULL;

	/* Add the Net Connection talent */
	gain_ability(PF_NET_CONNECTION, true);

	/* Generate initial equipment.
	 * This has a minimum and maximum price - so
	 * Generate pieces until over the minimum.
	 * Start over if now over the maximum.
	 * If incompatible (different tval), it will just be replaced.
	 */
	int min_price = 25000;
	int max_price = 30000;
	int reps = 0;
	const int implant_tval[] = {
		TV_LEGS, TV_ARMS
	};
	const int n_tvals = 2;
	struct object *obj[2] = { 0 };
	int value = 0;
	int level = 1;
	do {
		/* Generate a random implant tval */
		int i_obj = randint0(n_tvals);
		int tval = implant_tval[i_obj];

		/* From it, get a random kind */
		struct object_kind *k;
		do {
			k = get_obj_num(level, false, tval);
			if (!k) {
				level++;
				if (level == 127) {
					msg("Unable to find implant of tval %d", tval);
					return;
				}
			}
		} while (!k);

		/* Get rid of anything already in that slot */
		if (obj[i_obj]) {
			object_delete(&obj[i_obj]);
			obj[i_obj] = NULL;
		}

		/* And generate an object */
		obj[i_obj] = object_new();
		object_prep(obj[i_obj], k, 0, RANDOMISE);

		/* Now find the sum of values */
		/* If it's too high, remove at random */
		do {
			value = 0;
			for(int i=0;i<n_tvals;i++) {
				if (obj[i])
					value += object_value_real(obj[i], 1);
			}
			if (value > max_price) {
				int i_obj = randint0(n_tvals);
				if (obj[i_obj]) {
					object_delete(&obj[i_obj]);
					obj[i_obj] = NULL;
				}
			}
		} while (value > max_price);

		/* Try to stop this failing if there is no suitable combination */
		if (++reps > 100) {
			max_price += 50;
			if ((reps > 200) && (min_price > 20)) {
				min_price -= 20;
			}
		}

		/* If it's too low, go back and add another */
	} while (value < min_price);

	/* Idenitfy, carry and wield the items */
	for(int i=0;i<n_tvals;i++) {
		if (obj[i]) {
			/* ID */
			struct object *known_obj = object_new();
			obj[i]->known = known_obj;
			object_set_base_known(obj[i]);
			object_flavor_aware(obj[i]);
			obj[i]->known->pval = obj[i]->pval;
			obj[i]->known->effect = obj[i]->effect;
			obj[i]->known->notice |= OBJ_NOTICE_ASSESSED;
			obj[i]->kind->everseen = true;

			/* Carry and wield */
			inven_carry(p, obj[i], true, false);
			handle_stuff(player);
			event_signal(EVENT_INVENTORY);
			event_signal(EVENT_EQUIPMENT);
			do_inven_wield(obj[i], wield_slot(obj[i]), true, false);
		}
	}
}

static void cyborg_levelup(int from, int to)
{
}

/* Install hooks */
void install_race_CYBORG(void)
{
	struct player_race *r = get_race_by_name("Cyborg");
	r->init = cyborg_init;
	r->levelup = cyborg_levelup;
	r->loadsave = cyborg_loadsave;
}
