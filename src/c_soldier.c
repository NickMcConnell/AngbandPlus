/* The Soldier class.
 * 
 * This class's main schtick is support from a town building (Field HQ/Spacecraft) and/or possible teleported
 * in stuff on levelup or even when in danger, possibly backed up by usable powers. (Possibly after a quest,
 * should also collect $ bonus on promotion although it's mostly just for looks). They are generalist fighters
 * mostly, but prefering long guns, especially firearms (rather than energy weapons). Not good with home-made
 * stuff but can throw. May forbid Mob/Trade Connection, store ownership etc?
 * (They are risking being OP and too similar to Marksman, so the "long guns" might get nerfed)
 * Require quests to advance? If so, do store EXP (block levelling up, rather than accumulation of experience)
 * or it would be too annoying.
 */

#include "player.h"
#include "message.h"
#include "obj-gear.h"
#include "obj-knowledge.h"
#include "obj-make.h"
#include "obj-pile.h"
#include "randname.h"
#include "savefile.h"
#include "store.h"
#include "ui-display.h"
#include "ui-store.h"
#include "z-rand.h"
#include "z-util.h"

/* Persistent state for the soldier class */
struct soldier_state {
	s32b gift_waiting;
	s32b gift_given;
	char *storename;
};

/* Save or load state, according to the global saving flag and the "complete" flag.
 * This is called twice - once with complete false during load/save, and again
 * with complete true after load/save has completed
 **/
static void soldier_loadsave(bool complete) {
	struct soldier_state *state;

	if (complete) {
		struct store *store = get_store_by_name("Field HQ");
		if (store->owner->name)
			mem_free(store->owner->name);
		state = (struct soldier_state *)player->class->state;
		store->owner->name = string_make(state->storename);
	} else {
		if (player->class->state == NULL)
			player->class->state = mem_zalloc(sizeof(struct soldier_state));
		state = (struct soldier_state *)player->class->state;
		rdwr_s32b(&state->gift_waiting);
		rdwr_s32b(&state->gift_given);
		/* This only happens when saving at quit - the state has already gone away.
		 * There's no need to keep it, though - so store an empty string.
		 */
		if (saving && !state->storename) {
			char *empty = "";
			rdwr_string(&empty);
		} else {
			rdwr_string(&state->storename);
		}
	}
}

/* Start a new character as as Soldier - after talents setup (so not for selection of subclass) */
static void soldier_init(void)
{
	/* Initialise saved state */
	player->class->state = mem_zalloc(sizeof(struct soldier_state));

	/* Allow access to HQ */
	get_store_by_name("Field HQ")->open = true;

	/* Set a random owner */
	char buf[256];
	char name[256];
	const char *races[] = {
		"Human",
		"Bogon",
		"Kzin",
		"Time-Lord",
		"Thargoid",
		"Wookiee",
		"Android",
	};
	const char *race = races[randint0(sizeof(races) / sizeof(*races))];
	randname_make(RANDNAME_TOLKIEN, 5, 9, name, sizeof(name), name_sections); // FIXME
	name[0] = toupper(name[0]);
	strnfmt(buf, sizeof(buf), "General %s (%s)", name, race);
	get_store_by_name("Field HQ")->owner->name = string_make(buf);
	struct soldier_state *state = (struct soldier_state *)player->class->state;
	state->storename = string_make(buf);
}

/* FIXME: called-from clears hooks, etc. A final is called if there one but if it's just a single block, the wrapper can do it */

/* Gain a new level (or levels) for the first time */
static void soldier_levelup(int from, int to)
{
	struct soldier_state *state = (struct soldier_state *)player->class->state;
	if (!OPT(player, birth_no_recall)) {
		if (title_idx(from) != title_idx(to)) {
			const char *promo = "Congratulations on your promotion";
			if (title_idx(from) == 0)
				promo = "Congratulations on your induction into the Space Marines";
			else if ((my_stristr(player->class->title[title_idx(to)], "Lieutenant")) && (!my_stristr(player->class->title[title_idx(from)], "Lieutenant")))
				promo = "Congratulations on your commissioning as an officer of the Space Marines";
			else if ((my_stristr(player->class->title[title_idx(to)], "General")) && (!my_stristr(player->class->title[title_idx(from)], "General")))
				promo = "Welcome to Space Marine Command, General";
			msg("%s! Please return to HQ for your reward.", promo);
			state->gift_waiting = title_idx(to);
		}
	}
}

/* Enter or exit a town building (return false by AND of the optional parameter on entering to replace the default behaviour. This is ignored as default behavious is always wanted) */
static void soldier_building(int store, bool entering, bool *do_default)
{
	struct soldier_state *state = (struct soldier_state *)player->class->state;
	if (!streq(get_store_by_idx(store)->name, "Field HQ"))
		return;
	/* HQ: is a promotion gift available? Only give one, for the messages to make sense */
	if (entering) {
		if (state->gift_waiting > state->gift_given) {
			int lev = 0;
			bool good = true;
			bool great = false;
			bool extra = false;
			int tval = 0;
			int num = 0;
			const char *name = NULL;
			struct object *obj = NULL;
			s32b value;
			int bonus;

			const char *message = "Here's your new equipment.";

			/* Select a gift */
			switch(state->gift_given) {
				case 0:
					lev = 3; name = "6mm rifle";									/* Private */
					bonus = 50;
					break;
				case 1:
					lev = 6; name ="6mm bullet";
					bonus = 100;
					break;
				case 2:
					lev = 11; name = "combat boots";								/* Lance-Corporal */
					bonus = 250;
					break;
				case 3:
					lev = 18; tval = TV_GLOVES;
					bonus = 500;
					break;
				case 4:
					lev = 99; tval = TV_PILL; num = 2; name = "strength";			/* Sgt */
					message = "This should build you up!";
					bonus = 1000;
					break;
				case 5:
					lev = 25; tval = TV_SOFT_ARMOR;
					bonus = 2500;
					break;
				case 6:
					lev = 33; great = true; name = "12mm handgun";					/* Lt 2nd */
					bonus = 4000;
					break;
				case 7:
					lev = 99; tval = TV_PILL; num = 2; name = "augmentation";
					message = "You are going to like this one!";
					bonus = 6000;
					break;
				case 8:
					lev = 42; great = true; name ="12mm bullet";					/* Captain */
					bonus = 8000;
					break;
				case 9:
					lev = 99; tval = TV_PILL; num = 3; name = "speed nano-";
					message = "This will get you moving!";
					bonus = 10000;
					break;
				case 10:
					lev = 57; great = true; tval = TV_BELT;							/* Lt Colonel */
					bonus = 12500;
					break;
				case 11:
					lev = 65; great = true;	tval = TV_SWORD;
					bonus = 15000;
					break;
				case 12:
					lev = 72; great = true;	extra = true; tval = TV_HARD_ARMOR;		/* Maj-Gen */
					bonus = 20000;
					break;
				case 13:
					lev = 85; great = true;	extra = true; tval = TV_HELM;
					bonus = 25000;
					break;
				case 14:
					lev = 98; great = true;	extra = true; tval = TV_HARD_ARMOR;		/* General */
					message = "The base is now under your command, General.";
					store_your_name(get_store_by_idx(store));
					bonus = 30000;
					break;
			}

			msg("You receive a promotion bonus of $%d.", bonus);

			/* Build it */
			obj = make_object_named(cave, lev, good, great, extra, &value, tval, name);
			if (!obj) {
				message = "Seems we can't find your item.";
				obj = make_object_named(cave, lev, good, great, extra, &value, 0, NULL);
			}

			/* Message */
			msg(message);

			/* Initialize it and carry it */
			if (obj) {
				obj->origin = ORIGIN_PROMOTION;
				if (num > 0)
					obj->number = num;
				obj->known = object_new();
				object_set_base_known(obj);
				object_flavor_aware(obj);
				obj->known->pval = obj->pval;
				obj->known->effect = obj->effect;
				obj->known->notice |= OBJ_NOTICE_ASSESSED;
				inven_carry(player, obj, true, false);
				int icon;
				do {
					icon = object_find_unknown_icon(player, obj);
					if (icon >= 0)
						player_learn_icon(player, icon, false);
				} while (icon >= 0);
				update_player_object_knowledge(player);
				obj->kind->everseen = true;
			}

			state->gift_given++;
		}
		return;
	} else {
		if (state->gift_waiting > state->gift_given)
			msg("You've got more waiting for you when you return.");
		return;
	}
	return;
}

/* Install hooks */
void install_class_SOLDIER(void)
{
	struct player_class *c = get_class_by_name("Soldier");
	c->init = soldier_init;
	c->levelup = soldier_levelup;
	c->building = soldier_building;
	c->loadsave = soldier_loadsave;
}
