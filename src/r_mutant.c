#include "player.h"
#include "player-ability.h"
#include "player-calcs.h"
#include "savefile.h"
#include "z-util.h"

/* Persistent state for the mutant extension */
struct mutant_state {
	bool mutate[PY_MAX_LEVEL];
};

static void mutant_loadsave(bool complete) {
	struct mutant_state *state;

	if (!complete) {
		if (player->extension->state == NULL)
			player->extension->state = mem_zalloc(sizeof(struct mutant_state));
		state = (struct mutant_state *)player->extension->state;
		for(size_t i=0;i<sizeof(state->mutate); i++) {
			rdwr_bool(&state->mutate[i]);
		}
	}
}

/* Start a new character as an mutant */
static void mutant_init(void)
{
	/* Initialise saved state */
	struct mutant_state *state = player->extension->state = mem_zalloc(sizeof(struct mutant_state));

	/* Mutate.
	 * You get a total of 3 to 5 mutations,
	 * of which 1 to 3 are immediate and
	 * 1 to 4 are postponed.
	 **/
	int mutations = 1;
	int later = 1 + randint1(3);
	if (one_in_(2)) {
		mutations++;
		later--;
	}
	if (one_in_(10)) { 
		mutations++;
		later--;
	}
	if (later < 1)
		later = 1;

	/* Add a mutation immediately */
	for(int i=0;i<mutations;i++)
		get_mutation(AF_MUTATION | AF_BIRTH, false);

	/* Mutate later */
	for(int i=0;i<later;i++) {
		int level;
		do {
			level = rand_range(6, 45);
		} while (state->mutate[level] == true);
		state->mutate[level] = true;
	}
}

/* Resist radiation at high level */
static void mutant_calc(struct player_state *state)
{
	struct player *p = player;
	int res = 0;
	if (player->lev >= 35)
		res = 1;
	if (player->lev >= 45)
		res = 2;
	if (player->lev >= 50)
		res = 3;
	p->extension->el_info[ELEM_RADIATION].res_level = res;
}

static void mutant_levelup(int from, int to)
{
	struct mutant_state *state = (struct mutant_state *)player->extension->state;

	/* Mutate if one is scheduled for this level.
	 * If possible, these are the not-entirely-nasty ones allowed at birth,
	 * but it is possible that none of these are left. Also try to avoid
	 * losing mutations.
	 **/
	for(int i=from; i<=to; i++) {
		if (state->mutate[i]) {
			state->mutate[i] = false;
			if (!get_mutation(AF_MUTATION | AF_BIRTH, false)) {
				if (!get_mutation(AF_MUTATION, false)) {
					get_mutation(AF_MUTATION, true);
				}
			}
		}
	}
}

/* Install hooks */
void install_race_MUTANT(void)
{
	struct player_race *r = get_race_by_name("Mutant");
	r->init = mutant_init;
	r->levelup = mutant_levelup;
	r->loadsave = mutant_loadsave;
	r->calc = mutant_calc;
}
