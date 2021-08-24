/* monster/monster 
 *
 * Tests for monster/monster?.c
 *
 * Created by: myshkin
 *             26 Apr 2011
 */

#include "unit-test.h"
#include "unit-test-data.h"
#include "test-utils.h"
#include "mon-util.h"

int setup_tests(void **state) {
	read_edit_files();
	*state = 0;
	return 0;
}

int teardown_tests(void *state) {
	mem_free(state);
	return 0;
}

/* Originally a regression test for #1409.
 * Modified to use the new monster list and so probably
 * not relevant to that bug any more.
 **/
int test_match_monster_bases(void *state) {
	struct monster_base *base;

	/* Green lemming */
	base = (lookup_monster("green lemming"))->base;
	require(match_monster_bases(base, "lemming", NULL));
	require(!match_monster_bases(base, "person", NULL));
	require(!match_monster_bases(base, "hologran", NULL));

	/* Pinky */
	base = (lookup_monster("Pinky"))->base;
	require(match_monster_bases(base, "hologram", NULL));
	require(!match_monster_bases(base, "lemming", NULL));
	require(!match_monster_bases(base, "person", NULL));

	ok;
}

const char *suite_name = "monster/monster";
struct test tests[] = {
	{ "match_monster_bases", test_match_monster_bases },
	{ NULL, NULL }
};
