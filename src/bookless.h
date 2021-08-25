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

extern int last_bookless_dir;
extern struct spellholder last_bookless_spell;

extern void do_cmd_bookless();
extern void do_cmd_repeat_bookless();
