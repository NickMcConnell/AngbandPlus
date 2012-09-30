/* File status.c */

/* Purpose: Status information */

/* Written by Pat Gunn <qc@apk.net> for PernAngband
 * This file is released into the public domain
 */ 

/* Throughout this file, I make use of 2-d arrays called flag_arr.
 * I fill them out with esp as the 0th element because then the second
 * index is equal to the f-number that they are in the attributes, and
 * that makes it more intuitive to use. I do need to fill them out in an
 * odd order, but that's all abstracted nicely away. The 6th element is used
 * to mark if the rest of the array is filled, that is, if there's an object
 * there. 
 */

#include "angband.h"

static void row_trival(char*, s16b, u32b, s16b, u32b, int, u32b[INVEN_TOTAL - INVEN_WIELD+2][7]);
static void row_bival(char*, s16b, u32b, int, u32b[INVEN_TOTAL - INVEN_WIELD+2][7]);
static void row_npval(char*, s16b, u32b, int, u32b[INVEN_TOTAL - INVEN_WIELD+2][7]);
static void row_hd_bon(int, int, u32b[INVEN_TOTAL - INVEN_WIELD+2][7]);

static void status_trival(s32b, s32b, byte, byte);
static void status_bival(s32b, byte, byte);
static void status_numeric(s32b, byte, byte);
static void status_curses(void);
static void status_companion(void);
void status_attr(void);
void status_combat(void);
void status_main(void);
void status_move(void);
void status_item(void);
void az_line(u32b[INVEN_TOTAL - INVEN_WIELD +2][7]);
#define STATNM_LENGTH 9

void status_attr(void)
{
char statstr[8];
char statnm[4];
char c;
int i; /* Iterator */
int stat = 0; /* Stat designator */
clear_from(0);
c_put_str(TERM_L_BLUE, "Statistics", 0,1);
for(i=0;i<6;i++)
	{
	switch(i)
		{
		case 0:
			stat = A_STR;
			strcpy(statnm, "Str");
			break;
		case 1:
			stat = A_INT;
			strcpy(statnm, "Int");
			break;
		case 2:
			stat = A_WIS;
			strcpy(statnm, "Wis");
			break;
		case 3:
			stat = A_CON;
			strcpy(statnm, "Con");
			break;
		case 4:
			stat = A_DEX;
			strcpy(statnm, "Dex");
			break;
		case 5:
			stat = A_CHR;
			strcpy(statnm, "Chr");
			break;
		}
	c_put_str(TERM_L_RED, statnm, i+1, 0);
	cnv_stat(p_ptr->stat_use[stat], statstr);
	c_put_str(TERM_GREEN, statstr, i+1, 4);
	}
c_put_str(TERM_WHITE, "Press ESC to continue", 23, 0);
Term_fresh();
while(1)
	{
	c = inkey();
	if(c == ESCAPE) break;
	}
}

void status_move(void)
{
u32b flag_arr[INVEN_TOTAL - INVEN_WIELD + 2][7];
int yo=3;
clear_from(0);
c_put_str(TERM_L_BLUE, "Movement", 0, 1);
az_line(flag_arr);

row_trival("Fly/Lev", 4, TR4_FLY, 3, TR3_FEATHER, yo++, flag_arr);
row_bival("Climb", 4, TR4_CLIMB, yo++, flag_arr);
row_npval("Dig", 1, TR1_TUNNEL, yo++, flag_arr);
row_npval("Speed", 1, TR1_SPEED, yo++, flag_arr);
yo++;
row_npval("Stealth", 1, TR1_STEALTH, yo++, flag_arr);
row_bival("Tele", 3, TR3_TELEPORT, yo++, flag_arr);

c_put_str(TERM_WHITE, "Press ESC to continue", 23, 0);
Term_fresh();
while(1)
	{
	bool loop_exit=FALSE;
	char c;
	c = inkey();
	switch(c)
		{
		case ESCAPE:
			{
			loop_exit = TRUE;
			}
		}
	if(loop_exit) {break;}
	}
}

void status_item(void)
{
u32b flag_arr[INVEN_TOTAL - INVEN_WIELD +2][7];
int yo=3;
clear_from(0);
c_put_str(TERM_L_BLUE, "Item", 0,1);
az_line(flag_arr);

c_put_str(TERM_WHITE, "Press ESC to continue", 23, 0);
Term_fresh();
while(1)
	{
	bool loop_exit=FALSE;
	char c;
	c = inkey();
	switch(c)
		{
		case ESCAPE:
			loop_exit = TRUE;
		}
	if(loop_exit) {break;}
	}
}

void status_combat(void)
{
u32b flag_arr[INVEN_TOTAL - INVEN_WIELD +2][7];
int yo=3;
clear_from(0);
c_put_str(TERM_L_BLUE, "Combat", 0, 1);
az_line(flag_arr);

row_npval("+Blows", 1, TR1_BLOWS, yo++, flag_arr);
row_npval("Ammo_Mgt", 3, TR3_XTRA_MIGHT, yo++, flag_arr);
row_npval("Ammo_Sht", 3, TR3_XTRA_SHOTS, yo++, flag_arr);
row_bival("Vorpal", 1, TR1_VORPAL, yo++, flag_arr);
row_bival("Quake", 1, TR1_IMPACT, yo++, flag_arr);
row_bival("Chaotic", 1, TR1_CHAOTIC, yo++, flag_arr);
row_bival("Vampiric", 1, TR1_VAMPIRIC, yo++, flag_arr);
row_bival("Poison", 1, TR1_BRAND_POIS, yo++, flag_arr);
row_bival("Acidic", 1, TR1_BRAND_ACID, yo++, flag_arr);
row_bival("Shocks", 1, TR1_BRAND_ELEC, yo++, flag_arr);
row_bival("Burns", 1, TR1_BRAND_FIRE, yo++, flag_arr);
row_bival("Chills", 1, TR1_BRAND_COLD, yo++, flag_arr);
row_trival("S/K Undd", 1, TR1_SLAY_UNDEAD, 5, TR5_KILL_UNDEAD, yo++, flag_arr);
row_trival("S/K Dmn", 1, TR1_SLAY_DEMON, 5, TR5_KILL_DEMON, yo++, flag_arr);
row_trival("S/K Drag", 1, TR1_SLAY_DRAGON, 1, TR1_KILL_DRAGON, yo++, flag_arr);
row_bival("Sl.Orc", 1, TR1_SLAY_ORC, yo++, flag_arr);
row_bival("Sl.Troll", 1, TR1_SLAY_TROLL, yo++, flag_arr);
row_bival("Sl.Giant", 1, TR1_SLAY_GIANT, yo++, flag_arr);
row_hd_bon(0, yo++, flag_arr);
row_hd_bon(1, yo++, flag_arr);

c_put_str(TERM_WHITE, "Press ESC to continue", 23, 0);
Term_fresh();
while(1)
	{
	bool loop_exit=FALSE;
	char c;
	c = inkey();
	switch(c)
		{
		case ESCAPE:
			loop_exit = TRUE;
		}
	if(loop_exit){break;}
	}
}

void status_curses(void)
{
u32b flag_arr[INVEN_TOTAL - INVEN_WIELD +2][7];
int yo = 3;

clear_from(0);
c_put_str(TERM_L_BLUE, "Curses", 0, 1);
az_line(flag_arr);

row_trival("Hvy/Nrm", 3, TR3_HEAVY_CURSE, 3, TR3_CURSED, yo++, flag_arr);
row_bival("Perma", 3, TR3_PERMA_CURSE, yo++, flag_arr);
row_trival("DG/Ty", 4, TR4_DG_CURSE, 3, TR3_TY_CURSE, yo++, flag_arr);
row_trival("Prm/Auto", 3, TR3_PERMA_CURSE, 3, TR3_AUTO_CURSE, yo++, flag_arr);
row_bival("NoDrop", 4, TR4_CURSE_NO_DROP, yo++, flag_arr);
yo++;
row_bival("B.Breath", 4, TR4_BLACK_BREATH, yo++, flag_arr);
row_bival("Dr.Exp", 3, TR3_DRAIN_EXP, yo++, flag_arr);
row_bival("Dr.Mana", 5, TR5_DRAIN_MANA, yo++, flag_arr);
row_bival("Dr.HP", 5, TR5_DRAIN_HP, yo++, flag_arr);
row_bival("No Hit", 4, TR4_NEVER_BLOW, yo++, flag_arr);
row_bival("NoTele", 3, TR3_NO_TELE, yo++, flag_arr);
row_bival("Aggrav", 3, TR3_AGGRAVATE, yo++, flag_arr);

c_put_str(TERM_WHITE, "Press ESC to continue", 23, 0);
Term_fresh();
while(1)
	{
	bool loop_exit=FALSE;
	char c;

	c = inkey();
	switch(c)
		{
		case ESCAPE:
			{
			loop_exit=TRUE;
			}
		}
	if(loop_exit==TRUE) {break;}
	}
}

void status_main(void)
{
int do_quit = 0;
char c;

character_icky = TRUE;
Term_save();
while(1)
	{
	clear_from(0);
	c_put_str(TERM_WHITE, "Status screen", 0, 10);
	c_put_str(TERM_WHITE, "1) Statistics", 2, 5);
	c_put_str(TERM_WHITE, "2) Movement", 3, 5);
	c_put_str(TERM_WHITE, "3) Combat", 4, 5);
	c_put_str(TERM_WHITE, "4) Item", 5, 5);
	c_put_str(TERM_WHITE, "5) Curses", 6, 5);
        c_put_str(TERM_WHITE, "6) Companions", 7, 5);
	c_put_str(TERM_RED, "Press 'q' to Quit", 23, 5);
	c = inkey();
	switch(c)
		{
		case '1':
			status_attr();
			break;
		case '2':
			status_move();
			break;
		case '3':
			status_combat();
			break;
		case '4':
			status_item();
			break;
		case '5':
			status_curses();
			break;
                case '6':
                        status_companion();
			break;
		case 'q':
		case ESCAPE:
			do_quit = 1; /* Schedule leaving the outer loop */
			break;
		}
	Term_fresh();
	if(do_quit) break;
	}
Term_load();
character_icky = FALSE;
p_ptr->redraw |= (PR_WIPE|PR_BASIC|PR_EXTRA|PR_MAP);
handle_stuff();
}

void az_line(u32b flag_arr[INVEN_TOTAL - INVEN_WIELD +2][7])
{
int index = 9; /* Leave room for description */
int i;
for(i = INVEN_WIELD; i < INVEN_TOTAL; i++)
	{
	if(inventory[i].k_idx) /* Wearing anything here? */
		{
		char cstrng[2];
		cstrng[0] = (i - INVEN_WIELD) + 'a'; /* Assumes ASCII */
		cstrng[1] = '\0'; /* terminate it */
		c_put_str(TERM_WHITE, cstrng, 2, index++); /* Assumes ASCII */

#if 0 // DGDGDGDG
                object_flags_known(&inventory[i],
#else
		object_flags(&inventory[i], 	/* Help me debug */
#endif
					&flag_arr[i-INVEN_WIELD][1], /* f1 */
					&flag_arr[i-INVEN_WIELD][2], /* f2 */
					&flag_arr[i-INVEN_WIELD][3], /* f3 */
					&flag_arr[i-INVEN_WIELD][4], /* f4 */
					&flag_arr[i-INVEN_WIELD][5], /* f5 */
					&flag_arr[i-INVEN_WIELD][0]);  /* esp */
		flag_arr[i-INVEN_WIELD][6] = 1; /* And mark it to display */
		}
	else flag_arr[i - INVEN_WIELD][6] = 0; /* Otherwise don't display it */
	}
c_put_str(TERM_WHITE, "@", 2, index++);
player_flags(
		&flag_arr[INVEN_TOTAL - INVEN_WIELD +1][1], /* f1 */
		&flag_arr[INVEN_TOTAL - INVEN_WIELD +1][2], /* f2 */
		&flag_arr[INVEN_TOTAL - INVEN_WIELD +1][3], /* f3 */
		&flag_arr[INVEN_TOTAL - INVEN_WIELD +1][4], /* f4 */
		&flag_arr[INVEN_TOTAL - INVEN_WIELD +1][5], /* f5 */
		&flag_arr[INVEN_TOTAL - INVEN_WIELD +1][0] /* esp */
		);
flag_arr[INVEN_TOTAL - INVEN_WIELD +1][6] = 1;
}

static void status_trival(s32b val1, s32b val2, byte ypos, byte xpos)
{
if(val1 != 0)
	c_put_str(TERM_L_BLUE, "*", ypos, xpos);
else if(val2 != 0)
	c_put_str(TERM_L_BLUE, "+", ypos, xpos);
else
	c_put_str(TERM_WHITE, ".", ypos, xpos);
}

static void status_bival(s32b val, byte ypos, byte xpos)
{
if(val != 0)
	c_put_str(TERM_L_BLUE, "+", ypos, xpos);
else
	c_put_str(TERM_WHITE, ".", ypos, xpos);
}

static void status_numeric(s32b val, byte ypos, byte xpos)
{
if(val == 0)
	c_put_str(TERM_WHITE, ".", ypos, xpos);
else if (val < 0)
	{
	val *= -1;
	if(val > 9)
		c_put_str(TERM_RED, "*", ypos, xpos);
	else
		{
		char strnum[2];
		sprintf(strnum, "%lu", val);
		c_put_str(TERM_RED, strnum, ypos, xpos);
		}
	}
else if (val > 0)
	{
	if(val > 9)
		c_put_str(TERM_GREEN, "*", ypos, xpos);
	else
		{
		char strnum[2];
		sprintf(strnum, "%lu", val);
		c_put_str(TERM_GREEN, strnum, ypos, xpos);
		}
	}
}

static void row_trival(char* statname, s16b row, u32b flag, s16b row2, u32b flag2, int yo, u32b flag_arr[INVEN_TOTAL - INVEN_WIELD+2][7])
{
int i;
int x=0;
c_put_str(TERM_L_GREEN, statname, yo, 0);
for(i=0; i < (INVEN_TOTAL - INVEN_WIELD +2);i++)
	{
	if(flag_arr[i][6] == 1)
		{
		status_trival(
			(flag_arr[i][row] & flag),
			(flag_arr[i][row2] & flag2),
			yo, x+STATNM_LENGTH);
		x++;
		}
	}
}

static void row_bival(char* statname, s16b row, u32b flag, int yo, u32b flag_arr[INVEN_TOTAL - INVEN_WIELD+2][7])
{
int i;
int x=0;
c_put_str(TERM_L_GREEN, statname, yo, 0);
for(i=0; i < (INVEN_TOTAL - INVEN_WIELD +2);i++)
	{
	if(flag_arr[i][6] == 1)
		{
		status_bival((flag_arr[i][row] & flag), yo, x+STATNM_LENGTH);
		x++;
		}
	}
}

static void row_npval(char* statname, s16b row, u32b flag, int yo, u32b flag_arr[INVEN_TOTAL - INVEN_WIELD+2][7])
	/* Displays nicely a pval-based status row */
{
int i;
int x=0;
c_put_str(TERM_L_GREEN, statname, yo, 0);
for(i=0; i < (INVEN_TOTAL - INVEN_WIELD +2);i++)
	{
	if(flag_arr[i][6] == 1)
		{
		if(i == (INVEN_TOTAL - INVEN_WIELD +1) )
				/* Special case, player_flags */
				/* Players lack a pval, no way to calc value */
			{
			if(flag_arr[i][row] & flag)
				c_put_str(TERM_YELLOW, "*", yo, x+STATNM_LENGTH);
			else c_put_str(TERM_WHITE, ".", yo, x+STATNM_LENGTH);
			x++;
			continue;
			}
		if(flag_arr[i][row] & flag)
			{
			status_numeric(inventory[i+INVEN_WIELD].pval, yo, x+STATNM_LENGTH);
			}
		else
			{
			c_put_str(TERM_WHITE, ".", yo, x+STATNM_LENGTH);
			}
		x++;
		}
	}
}

static void row_hd_bon(int which, int yo, u32b flag_arr[INVEN_TOTAL - INVEN_WIELD+2][7])
		/* To-hit/dmg modifiers, selected by 1st argument */
{
int i;
int x=0;
if((which != 0) && (which != 1)) return;
c_put_str(TERM_L_GREEN, ((which == 0) ? "To-Hit" : "To-Dmg"), yo, 0);
for(i=0;i< (INVEN_TOTAL - INVEN_WIELD +2); i++)
	{
	if(flag_arr[i][6] == 1)
		{
		if(i == (INVEN_TOTAL - INVEN_WIELD +1) ) /* Player? */
			{
			c_put_str(TERM_WHITE, ".", yo, x+STATNM_LENGTH);
			x++;
			continue;
			}
		if( (which == 0) && (inventory[INVEN_WIELD+i].to_h != 0))
			{
			status_numeric(inventory[INVEN_WIELD+i].to_h, yo, x+STATNM_LENGTH);
			x++;
			continue;
			}
		if( (which == 1) && (inventory[INVEN_WIELD+i].to_d != 0))
			{
			status_numeric(inventory[INVEN_WIELD+i].to_d, yo, x+STATNM_LENGTH);
			x++;
			continue;
			}
		c_put_str(TERM_WHITE, ".", yo, x+STATNM_LENGTH);
		x++;
		}
	}
}

static void status_companion(void)
{
        int i, max = 0;

	FILE *fff;

	char file_name[1024];

        Term_clear();

	/* Temporary file */
	if (path_temp(file_name, 1024)) return;

	/* Open a new file */
	fff = my_fopen(file_name, "w");

        /* Calculate companions */
	/* Process the monsters (backwards) */
        for (i = m_max - 1; i >= 1; i--)
	{
		/* Access the monster */
                monster_type *m_ptr = &m_list[i];

                if (m_ptr->status == MSTATUS_COMPANION)
                {
                        char m_name[80];
                        int b, y = 0;

			/* Extract monster name */
                        monster_desc(m_name, m_ptr, 0x80);

                        fprintf(fff, "#####BCompanion: %s\n", m_name);

                        fprintf(fff, "  Lev/Exp : [[[[[G%d / %ld]\n", m_ptr->level, m_ptr->exp);
                        if (m_ptr->level < MONSTER_LEVEL_MAX) fprintf(fff, "  Next lvl: [[[[[G%ld]\n", MONSTER_EXP((s32b)m_ptr->level + 1));
                        else fprintf(fff, "  Next lvl: [[[[[G****]\n");

                        fprintf(fff, "  HP      : [[[[[G%d / %d]\n", m_ptr->hp, m_ptr->maxhp);
                        fprintf(fff, "  AC      : [[[[[G%d]\n", m_ptr->ac);
                        fprintf(fff, "  Speed   : [[[[[G%d]\n", m_ptr->mspeed - 110);

                        for (b = 0; b < 4; b++)
                        {
                                if (!m_ptr->blow[b].d_dice) continue;
                                if (!m_ptr->blow[b].d_side) continue;

                                fprintf(fff, "  Blow %1d  : [[[[[G%dd%d]\n", y + 1, m_ptr->blow[b].d_dice, m_ptr->blow[b].d_side);
                                y++;
                        }

                        fprintf(fff, "\n");
                }
	}

	/* Close the file */
	my_fclose(fff);

	/* Display the file contents */
        show_file(file_name, "Companion List", 0, 0);

	/* Remove the file */
	fd_kill(file_name);
}
