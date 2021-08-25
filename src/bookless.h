/* File: bookless.h
 * Purpose: headers for bookless spellcasting classes
 */

struct spellholder {
	char *name;
	int level;
	int cost;
	int fail;
	char *desc;
	void (*spell)(int);
	char needs_dir;
};

struct last_bookless {
	int dir;
	struct spellholder *spell;
};

extern void do_cmd_bookless();
extern void do_cmd_repeat_bookless();
