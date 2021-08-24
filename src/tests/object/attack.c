/* object/attack */

#include "unit-test.h"
#include "unit-test-data.h"

#include "object.h"
#include "obj-make.h"
#include "player-attack.h"

extern struct init_module obj_make_module;


int setup_tests(void **state) {
	struct alloc_test_state *st;

	player = &test_player;

	z_info = mem_zalloc(sizeof(*z_info));
	z_info->k_max = 6;
	/* Won't set up any egos for testing. */
	z_info->e_max = 0;
	z_info->max_obj_depth = 2;
	z_info->great_obj = 20;

	return 0;
}


int teardown_tests(void *state) {

	(*obj_make_module.cleanup)();
	mem_free(z_info);
	return 0;
}



int test_breakage_chance(void *state) {
	struct object obj;
	int c;

	object_prep(&obj, &test_longsword, 1, AVERAGE);
	c = breakage_chance(&obj, true);
	eq(c, 50);
	c = breakage_chance(&obj, false);
	eq(c, 25);
	obj.artifact = &test_artifact_sword;
	c = breakage_chance(&obj, true);
	eq(c, 0);
	c = breakage_chance(&obj, false);
	eq(c, 0);
	ok;
}

const char *suite_name = "object/attack";
struct test tests[] = {
	{ "breakage-chance", test_breakage_chance },
	{ NULL, NULL }
};
