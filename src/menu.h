/*
 * Simple module for prompting the user for a choice.
 * I've gotten into trouble trying to smart wrap lists of choices before,
 * so this module won't even try this.  Keep the lists short enough to show
 * up on the minimal terminal size in a single column.
 */

typedef void *menu_choices;
typedef cptr (*menu_text_fn)(menu_choices choices, int which);
typedef int (*menu_attr_fn)(menu_choices choices, int which);
	
typedef struct {
	cptr          choose_prompt;
	cptr          browse_prompt;
	cptr          heading;
	menu_text_fn  text_fn;  /* Required: Format the current choice.  Accept NULL to emit the header */
	menu_text_fn  help_fn;  /* Optional: Allow Users to browse descriptive info */
	menu_attr_fn  color_fn; /* Optional: Color various choices */
	menu_choices  choices;
	int           count;
} menu_list_t;

extern int menu_choose(menu_list_t *menu_list);
