/* File: bookless.h
 * Purpose: headers for bookless spellcasting classes
 */

struct spellholder {
	char *name;
	int level;
	int cost;
	int fail;
	char *desc;
};

extern void do_cmd_bookless();
extern void do_cmd_pyro();
extern void do_cmd_avatar();
extern void do_cmd_sapper();
extern void do_cmd_reaper();
extern void do_cmd_assassin();
