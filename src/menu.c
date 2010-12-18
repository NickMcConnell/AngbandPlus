#include "angband.h"

static void _describe(menu_list_t *menu_list, int which)
{
	char tmp[62*5];
	cptr help;
	int i, line;

	if (!menu_list->help_fn) return;
	if (which < 0 || which >= menu_list->count) return;

	/* 2 lines for prompt and heading.  5 lines for browse info. */
	for (i = 0; i < 7; i++)
		Term_erase(12, menu_list->count + i + 2, 255);

	/* Get the description, and line break it (max 5 lines)
	   Note that help may well point to a global buffer, so we
	   must copy it immediately some place else! */
	help = (menu_list->help_fn)(menu_list->choices, which);
	roff_to_buf(help, 62, tmp, sizeof(tmp));

	for(i = 0, line = menu_list->count + 3; tmp[i]; i += 1+strlen(&tmp[i]))
	{
		prt(&tmp[i], line, 15);
		line++;
	}
}

static void _list(menu_list_t *menu_list)
{
	char temp[255];
	int  i;
	int  y = 1;
	int  x = 10;

	Term_erase(x, y, 255);

	if (menu_list->heading)
		put_str(format("     %s", menu_list->heading), y++, x);

	/* List */
	for (i = 0; i < menu_list->count; i++)
	{
		char letter = '\0';
		byte attr = TERM_WHITE;

		if (menu_list->color_fn)
			attr = (byte)(menu_list->color_fn)(menu_list->choices, i);

		if (i < 26)
			letter = I2A(i);
		else if (i < 52)
			letter = 'A' + i - 26;
		else
			letter = '0' + i - 52;

		sprintf(temp, "  %c) %s", letter, (menu_list->text_fn)(menu_list->choices, i));
		c_prt(attr, temp, y + i, x);
	}
}

static int _choose(menu_list_t *menu_list)
{
	int	 choice = -1;
	bool describe = FALSE;
	bool allow_browse = FALSE;
	char choose_prompt[100];
	char browse_prompt[100];
	
	if (menu_list->browse_prompt && menu_list->help_fn)
	{
		allow_browse = TRUE;
		sprintf(choose_prompt, "%s (Type '?' to browse)", menu_list->choose_prompt);
		sprintf(browse_prompt, "%s (Type '?' to choose)", menu_list->browse_prompt);
	}
	else
	{
		sprintf(choose_prompt, "%s", menu_list->choose_prompt);
		sprintf(browse_prompt, "%s", "");
	}
	
	_list(menu_list);

	for (;;)
	{
		char ch = '\0';

		choice = -1;
		if (!get_com(describe ? browse_prompt : choose_prompt, &ch, FALSE)) break;

		if (ch == '?' && allow_browse)
		{
			describe = !describe;
			continue;
		}

		if (isupper(ch))
			choice = ch - 'A' + 26;
		else if (islower(ch))
			choice = ch - 'a';
		else if (ch >= '0' && ch <= '9')
			choice = ch - '0' + 52;

		if (choice < 0 || choice >= menu_list->count)
		{
			bell();
			continue;
		}

		if (describe)
		{
			_describe(menu_list, choice);
			continue;
		}

		break;
	}
	return choice;
}

int menu_choose(menu_list_t *menu_list)
{
	int choice = -1;

	if (REPEAT_PULL(&choice))
	{
		if (choice >= 0 && choice < menu_list->count)
			return choice;
	}

	screen_save();

	choice = _choose(menu_list);
	REPEAT_PUSH(choice);

	screen_load();

	return choice;
}
