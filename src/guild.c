#include "angband.h"


static bool leave_guild = FALSE;

static void display_guild(void);

static int guild = 0;


/*
 * Each player is given a few items when they join a guild.
 * These are given as tval/sval pairs.
 */

#define PLAYER_ITEMS_COUNT 44
static byte player_start_items[ PLAYER_ITEMS_COUNT ][ 3 ] = {
	/* These MUST be ordered by class */
	/* The third number identifies groups of items */
	/* The player will only be given one item from each group */
	/* Note: the code identifies the start of each class' section */
	/*   by a reduction in the "group number" */

	/* Warrior */
	{ TV_SWORD, SV_BROAD_SWORD, 1 },
	{ TV_SWORD, SV_LONG_SWORD, 1 },
	{ TV_HARD_ARMOR, SV_CHAIN_MAIL, 2 },
	{ TV_HARD_ARMOR, SV_METAL_SCALE_MAIL, 2 },
	{ TV_HELM, SV_METAL_CAP, 3 },
	{ TV_HELM, SV_HARD_LEATHER_CAP, 3 },
	{ TV_POTION, SV_POTION_BESERK_STRENGTH, 4 },
	{ TV_POTION, SV_POTION_HEROISM, 4 },

	/* Mage */
	{ TV_MAGIC_BOOK, 0, 1 },
	{ TV_SWORD, SV_DAGGER, 2 },
	{ TV_SCROLL, SV_SCROLL_IDENTIFY, 3 },
	{ TV_MAGIC_BOOK, 1, 4 },
	{ TV_SOFT_ARMOR, SV_ROBE, 4 },

	/* Priest */
	{ TV_PRAYER_BOOK, 0, 1 },
	{ TV_HAFTED, SV_MACE, 2 },
	{ TV_HAFTED, SV_QUARTERSTAFF, 2 },
	{ TV_POTION, SV_POTION_HEALING, 3 },
	{ TV_PRAYER_BOOK, 1, 4 },
	{ TV_SOFT_ARMOR, SV_ROBE, 4 },

	/* Rogue */
	{ TV_MAGIC_BOOK, 0, 1 },
	{ TV_SWORD, SV_SMALL_SWORD, 2 },
	{ TV_SWORD, SV_RAPIER, 2 },
	{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 3 },
	{ TV_SOFT_ARMOR, SV_SOFT_STUDDED_LEATHER, 3 },
	{ TV_BOOTS, SV_PAIR_OF_SOFT_LEATHER_BOOTS, 4 },
	{ TV_BOOTS, SV_PAIR_OF_HARD_LEATHER_BOOTS, 4 },

	/* Ranger */
	{ TV_MAGIC_BOOK, 0, 1 },
	{ TV_SWORD, SV_BROAD_SWORD, 2 },
	{ TV_BOW, SV_LONG_BOW, 3 },
	{ TV_SOFT_ARMOR, SV_SOFT_LEATHER_ARMOR, 4 },
	{ TV_SOFT_ARMOR, SV_SOFT_STUDDED_LEATHER, 4 },
	{ TV_ARROW, SV_AMMO_NORMAL, 5 },

	/* Paladin */
	{ TV_PRAYER_BOOK, 0, 1 },
	{ TV_SWORD, SV_BROAD_SWORD, 2 },
	{ TV_HAFTED, SV_FLAIL, 2 },
	{ TV_SCROLL, SV_SCROLL_PROTECTION_FROM_EVIL, 3 },
	{ TV_SOFT_ARMOR, SV_SOFT_STUDDED_LEATHER, 4 },
	{ TV_PRAYER_BOOK, 1, 4 },

	/* Druid */
	{ TV_ELE_BOOK, 0, 1 },
	{ TV_HAFTED, SV_QUARTERSTAFF, 2 },
	{ TV_SOFT_ARMOR, SV_ROBE, 3 },
	{ TV_BOOTS, SV_PAIR_OF_SOFT_LEATHER_BOOTS, 4 },
	{ TV_SHIELD, SV_SMALL_LEATHER_SHIELD, 4 },
	{ TV_SHIELD, SV_LARGE_LEATHER_SHIELD, 4 }
};



/*
 * Init players with some belongings
 *
 * Having an item makes the player "aware" of its purpose.
 */
static void player_outfit(void)
{
	int i, tv, sv, item_count, ii;
	byte class_counter, prev_val, root_idx;

	object_type *i_ptr;
	object_type object_type_body;


	/* Random item selection... */

	/* Find the index of the first item for the class */
	i = 0;
	for ( prev_val = 0, class_counter = p_ptr->pclass; class_counter > 0; class_counter-- ) {
		while ( player_start_items[i][2] >= prev_val ) {
			prev_val = player_start_items[i][2];
			i++;
		}
		prev_val = 0;
	}


	/* Now to give the player some items */
	while ( i < PLAYER_ITEMS_COUNT ) {
		root_idx = i;

		/* Find the number of items we can choose from */
		for ( prev_val = player_start_items[i][2]; prev_val==player_start_items[i][2]; i++ )
			;

		/* Choose an item in the given range */
		root_idx += randint( i - root_idx ) - 1;


		/* Give the player that item */
		/* Note that for arrows, we give a quantity (10d2) of that item */

		tv = player_start_items[root_idx][0];
		sv = player_start_items[root_idx][1];

		if ( tv == TV_ARROW )
			item_count = damroll( 10, 2 );
		else
			item_count = 1;

		for ( ii = 0; ii < item_count; ii++ ) {
			/* Get local object */
			i_ptr = &object_type_body;

			/* Give the player an object */
			object_prep(i_ptr, lookup_kind(tv, sv));
			object_aware(i_ptr);
			object_known(i_ptr);
			(void)inven_carry(i_ptr);
		}


		/* Have we made all the choices for this class? */
		if ( prev_val > player_start_items[i][2] ) break;
	}
}

/*
 * Process a command in a guild
 */
static void guild_process_command(void)
{
	/* Parse the command */
	switch (p_ptr->command_cmd)
	{
		/* Leave */
		case ESCAPE:
		{
			leave_guild = TRUE;
			break;
		}

		/* Redraw */
		case KTRL('R'):
		{
			do_cmd_redraw();
			display_guild();
			break;
		}

		/* Ignore return */
		case '\r':
		{
			break;
		}

		/*** Guild Commands ***/
#if 0
		/* Next guild */
		case 'n': {
			guild++;
			if ( guild == MAX_CLASS-1 ) guild = 0;
			do_cmd_redraw();
			display_guild();
			break;
		}

		/* Previous guild */
		case 'p': {
			if ( guild == 0 ) guild = MAX_CLASS-2;
			else guild--;
			do_cmd_redraw();
			display_guild();
			break;
		}
#endif
		/* Join guild */
		case 'j': if (p_ptr->pclass == MAX_CLASS-1)
		{
			p_ptr->pclass = guild;
			cp_ptr = &class_info[ guild ];
			mp_ptr = &magic_info[ guild ];

			/* give the player some equipment */
			player_outfit();

			get_extra();

			p_ptr->update |= ( PU_BONUS | PU_HP | PU_MANA | PU_SPELLS );
			p_ptr->redraw |= ( PR_LEV | PR_TITLE | PW_PLAYER_0 | PW_PLAYER_1 );
			p_ptr->window |= ( PW_PLAYER_0 | PW_PLAYER_1 );

			leave_guild = TRUE;
			break;
		} else {
			msg_print("You are already a member of a guild!");
			break;
		}



		/*** Inventory Commands ***/

		/* Wear/wield equipment */
		case 'w':
		{
			do_cmd_wield();
			break;
		}

			/* Take off equipment */
		case 't':
		{
			do_cmd_takeoff();
			break;
		}

		/* Destroy an item */
		case 'k':
		{
			do_cmd_destroy();
			break;
		}

		/* Equipment list */
		case 'e':
		{
			do_cmd_equip();
			break;
		}

		/* Inventory list */
		case 'i':
		{
			do_cmd_inven();
			break;
		}


		/*** Various commands ***/

		/* Identify an object */
		case 'I':
		{
			do_cmd_observe();
			break;
		}

		/* Hack -- toggle windows */
		case KTRL('E'):
		{
			toggle_inven_equip();
			break;
		}



		/*** Use various objects ***/

		/* Browse a book */
		case 'b':
		{
			do_cmd_browse();
			break;
		}

		/* Inscribe an object */
		case '{':
		{
			do_cmd_inscribe();
			break;
		}

		/* Uninscribe an object */
		case '}':
		{
			do_cmd_uninscribe();
			break;
		}



		/*** Help and Such ***/

		/* Help */
		case '?':
		{
			do_cmd_help();
			break;
		}

		/* Identify symbol */
		case '/':
		{
			do_cmd_query_symbol();
			break;
		}

		/* Character description */
		case 'C':
		{
			do_cmd_change_name();
			break;
		}


		/*** System Commands ***/

		/* Hack -- User interface */
		case '!':
		{
			(void)Term_user(0);
			break;
		}

		/* Single line from a pref file */
		case '"':
		{
			do_cmd_pref();
			break;
		}

		/* Interact with macros */
		case '@':
		{
			do_cmd_macros();
			break;
		}

		/* Interact with visuals */
		case '%':
		{
			do_cmd_visuals();
			break;
		}

		/* Interact with colors */
		case '&':
		{
			do_cmd_colors();
			break;
		}

		/* Interact with options */
		case '=':
		{
			do_cmd_options();
			character_icky = TRUE;
			do_cmd_redraw();
			display_guild();
			break;
		}


		/*** Misc Commands ***/

		/* Take notes */
		case ':':
		{
			do_cmd_note();
			break;
		}

		/* Version info */
		case 'V':
		{
			do_cmd_version();
			break;
		}

		/* Repeat level feeling */
		case KTRL('F'):
		{
			do_cmd_feeling();
			break;
		}

		/* Show previous message */
		case KTRL('O'):
		{
			do_cmd_message_one();
			break;
		}

		/* Show previous messages */
		case KTRL('P'):
		{
			do_cmd_messages();
			break;
		}

		/* Check knowledge */
		case '~':
		case '|':
		{
			do_cmd_knowledge();
			break;
		}

		/* Load "screen dump" */
		case '(':
		{
			do_cmd_load_screen();
			break;
		}

		/* Save "screen dump" */
		case ')':
		{
			do_cmd_save_screen();
			break;
		}


		/* Unknown command */
		default:
		{
			msg_print("That command does not work in guilds.");
			break;
		}
	}
}


/*
 * Display guild (after clearing screen)
 */
static void display_guild(void)
{
	int fd = -1;
	FILE *fp;
	char buf[1024];
	char b2[80];


	Term_clear();
	put_str("The Guild", 3, 35);
	put_str( class_info[ guild ].title, 4, 
		( 80 - strlen( class_info[ guild ].title )) / 2 );

	sprintf(b2, "guild%d.txt", guild+1);
	path_build(buf, 1024, ANGBAND_DIR_FILE, b2);

	fd = fd_open(buf, O_RDONLY);
	if (fd<0) {
		strncpy(b2, buf, 80);
		b2[79] = 0;

		sprintf(buf, "Couldn't open file \"%s\" for reading.", b2);
		put_str(buf, 6, 1);
	} else {
		fd_close(fd);
		fp = my_fopen(buf, "r");
		if (fp) {
			int i = 6;
			while (0 == my_fgets(fp, buf, 1024)) {
				put_str(buf, i++, 0);
			}
			my_fclose(fp);
		} else {
			put_str("A strange error has occurred.", 6, 1);
		}
	}
}

void do_cmd_guild(int guildnum)
{
	guild = guildnum;

	forget_view();
	character_icky = TRUE;
	p_ptr->command_arg = 0;
	p_ptr->command_rep = 0;
	p_ptr->command_new = 0;

	/* Display the guild */
	display_guild();

	/* Do not leave */
	leave_guild = FALSE;

	/* Interact with player */
	while (!leave_guild)
	{
		prt("", 1, 0);
		clear_from(21);

		/* Basic commands */
		prt(" ESC) Exit from Building.", 22, 0);

		/* Guild commands */
		if ( p_ptr->pclass == MAX_CLASS-1 ) {
			prt("   j) Join guild.", 23, 0);
/*			prt(" n) Look at next guild.", 22, 40); */
/*			prt(" p) Look at previous guild.", 23, 40); */
		}

		/* Prompt */
		prt("You may: ", 21, 0);

		/* Get a command */
		request_command();

		/* Process the command */
		guild_process_command();

		character_icky = TRUE;
	}


	p_ptr->energy_use = 0;
	character_icky = FALSE;
	p_ptr->command_new = 0;
	p_ptr->command_see = FALSE;
	msg_print(NULL);
	Term_clear();


	/* Update everything */
	p_ptr->update |= (PU_UPDATE_VIEW | PU_MONSTERS);
	p_ptr->redraw |= (PR_BASIC | PR_EXTRA | PR_MAP);
	p_ptr->window |= (PW_OVERHEAD);
}
