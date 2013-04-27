/* File: main-vcs.c */

/*
 * copyright 2001 Alexander Malmberg <alexander@malmberg.org>
 */

/*
 * Copyright (c) 1997 Ben Harrison
 *
 * This software may be copied and distributed for educational, research,
 * and not for profit purposes provided that this copyright and statement
 * are included in all such copies.
 */

/*
 * Here is the post to rec.games.roguelike.angband that linked to this
 * file. -SF-
 */
 
/*
 *
 *
 * Hi all,
 * 
 * I've written (mostly for my own use) a /dev/vcsa* based display driver
 * for Linux and thought it could be nice to announce it here. The
 * main-vcs.c file is available at
 * http://w1.423.telia.com/~u42303319/main-vcs.c and a patch that adds it
 * to main.c and changes the .prf files to use the gcu versions for this
 * display driver is at http://w1.423.telia.com/~u42303319/vcsa.patch . To
 * get it to compile, you'll have to add main-vcs.o to the object list in
 * your makefile, and add '-D"USE_VCS"' to CFLAGS. Note that the patch adds
 * vcs last in the list, so you'll probably need to run 'angband -mvcs' to
 * use it.
 *
 * Features:
 * - It's really fast, and there's no flickering.
 * 
 * - Palette changing works properly.
 *
 * - Easily customizable windows through command line options. The syntax
 * is "x0,y0,x1,y1", eg. "angband -mvcs -- 0,0,79,23 0,25,79,49" for an
 * 80x50 console. By default, a 0,0,79,23 window is added (and a bunch of
 * other windows if your console is 132x60). Make sure the first window is
 * at least 80x24 large, since it will be the main window. By default, a
 * frame (well, sortof) will be added around the windows (I like it that
 * way). If you don't want the frame, add --noframe somewhere on the
 * command line.
 *
 * - The escape key works without delay.
 *
 * Drawbacks:
 * - Only works on a local console. OTOH, if it can't initialize, it'll
 * signal failure and main.c should fall back to some other display driver.
 *
 * - Needs read/write access to the /dev/vcsa* file for your console. My
 * login handles this  automatically, YMMV.
 *
 * - It seems that if you switch away from the console running Angband
 * while it's updating the screen part of the update might end up on the
 * new console. Shouldn't be a problem in practice.
 *
 * Comments, suggestions, etc. are welcome.
 *
 * - Alexander Malmberg
 *
 */


/*
 * This module uses /dev/vcsa* to write characters and attributes
 * directly to console memory.  /dev/vcsa*'s first 4 bytes are the
 * number of lines and columns on the screen, and X and Y position
 * of the cursor with (0,0) as top left.  Then follows lines*columns
 * pairs of (char, attr) with the contents of the screen.
 */

#include "angband.h"


#ifdef USE_VCS


cptr help_vcs[] =
{
	"To use /dev/vcsa*",
	"x0,y0,x1,y1  Create new term",
	"--noframe    No window frames",
	NULL
};

/* Change the palette. If this is un-define'd, the standard palette will
be used, and it'll try to map colors to an equivalent color. */
#define SET_COLORS


#include <sys/ioctl.h>
#include <termios.h>

#define VCSA_CURSOR	2	/* Seek offset of cursor position */
#define VCSA_SCREEN	4	/* Seek offset of screen contents */


static int fd_vcsa;
static unsigned char *screen,*row_clear;

static byte s_width,s_height;
static byte cursor[2];


typedef struct term_data
{
	term t;

	int x0,y0,x1,y1,sx,sy;
	unsigned char *base;
} term_data;

#define MAX_VCS_TERM 1

static term_data data[MAX_VCS_TERM];
static int num_term;


/* This is (basically) the same color mapping as in main-gcu */
static unsigned char attr_for_color[16]=
{0x00,0x07,0x02,0x03,0x04,0x05,0x06,0x01,
 0x08,0x09,0x0a,0x0b,0x0c,0x0d,0x0e,0x0f
};

#define COLOR_BLANK 0x07 /* attr_for_color[TERM_WHITE] */


/* For some reason, the kernel moves the color indeces around, so we need
to reverse that here. */
static int reverse_linux_color_table[16]=
/*0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 */
{ 0, 7, 2, 6, 1, 5, 3, 4, 8,12,10,14, 9,13,11,15};

static void set_colors(void)
{
	int i;
	for (i=0;i<16;i++)
	{
		printf("\033]P%c%02x%02x%02x",
			hexsym[reverse_linux_color_table[i]],
			angband_color_table[i][1],
			angband_color_table[i][2],
			angband_color_table[i][3]);
	}
	fflush(stdout);
}





static struct termios norm_termios;
static struct termios game_termios;

static void reset_terminal(void)
{
	/* Turn off non-blocking on stdin */
	fcntl(0,F_SETFL,(~O_NONBLOCK)&fcntl(0,F_GETFL));
	
	/* Reset terminal parameters */
	tcsetattr(0, TCSAFLUSH, &norm_termios);
	
	/* Reset the palette */
	printf("\033]R");
	fflush(stdout);
}

static void setup_terminal(void)
{
	/* Make stdin non-blocking */
	fcntl(0,F_SETFL,O_NONBLOCK|fcntl(0,F_GETFL));
	
	/* Set terminal parameters */
	tcsetattr(0, TCSAFLUSH, &game_termios);
	
	/* Set up palette */
	set_colors();
}

/*
 * Reset terminal and move to last line
 */
static void leave_vcs(void)
{
	int i;
	unsigned char *c;
	
	/* Blank out line in memory */
	for (i = 0, c = screen; i < s_width; i++)
	{
		*c++ = ' ';
		*c++ = COLOR_BLANK;
	}

	/* Write blanked-out line to last line in console (vcsa) file */
	lseek(fd_vcsa, VCSA_SCREEN + 2 * s_width * (s_height - 1), SEEK_SET);
	write(fd_vcsa, screen, s_width * 2);
	
	/* Set cursor position to last line and write it to console */
	lseek(fd_vcsa, VCSA_CURSOR, SEEK_SET);
	cursor[0] = 0;
	cursor[1] = s_height - 1;
	write(fd_vcsa, cursor, sizeof(cursor));
	
	/* Reset terminal */
	reset_terminal();
}


/*
 * Handle a "special request"
 */
static errr Term_xtra_vcs(int n, int v)
{
	switch (n)
	{
		case TERM_XTRA_EVENT:
		{
			int lch;
			unsigned char ch;
			fd_set s;
			if (v)
			{
				/* Wait for input */
				FD_ZERO(&s);
				FD_SET(0,&s);
				select(1,&s,NULL,NULL,NULL);
			}
			lch=-1;
			
			/* Loop while reading a character */
			while (read(0,&ch,1)==1)
			{
			
				/* Consume previous character, if any */
				if (lch!=-1) Term_keypress(lch);
				
				lch=ch;
			}
			
			/* Consume last character */
			if (lch!=-1)
			{
				/* A bit of a hack. If we get a lone escape (no pending input
				after it) we assume it's the escape key and not part of an escape
				sequence and change it to a `. This shouldn't cause any problems
				as long as we're running locally, and if we're using the
				/dev/vcsa* devices, we are.
				*/
				if (lch== ESCAPE)
				{
					Term_keypress('`');
				}
				else
				{
					Term_keypress(lch);
				}
			}

			return (0);
		}

		case TERM_XTRA_FLUSH:
		{
			/* Flush input */
			unsigned char ch;
			while (read(0,&ch,1)==1) ;
			return (0);
		}

		case TERM_XTRA_FRESH:
		{
			/* Write screen memory to console (vcsa) file */
			lseek(fd_vcsa,VCSA_SCREEN,SEEK_SET);
			write(fd_vcsa,screen,2 * s_width * s_height);
			return (0);
		}

		case TERM_XTRA_NOISE:
		{
			/* Bell */
			write(1,"\007",1);
			return (0);
		}

		case TERM_XTRA_ALIVE:
		{
			if (!v)
				leave_vcs();
			else
				setup_terminal();
			return (0);
		}

		case TERM_XTRA_DELAY:
		{
			/* Delay for some milliseconds */
			if (v > 0) usleep(v*1000);
			return (0);
		}

		case TERM_XTRA_REACT:
		{
			/* Set up colours */
			set_colors();
			return 0;
		}

		case TERM_XTRA_SHAPE:
		{
			printf("\033[?25%c",v?'h':'l');
			fflush(stdout);
			return 0;
		}
	}

	/* Unknown or Unhandled action */
	return (1);
}

/*
 * Actually move the hardware cursor
 */
static errr Term_curs_vcs(int x, int y)
{
	term_data *td = (term_data*)(Term->data);
	
	/* Set the cursor position */
	cursor[0] = x + td->x0;
	cursor[1] = y + td->y0;

	/* Write cursor position to console (vcsa) file */
	lseek(fd_vcsa,VCSA_CURSOR,SEEK_SET);
	write(fd_vcsa,cursor,sizeof(cursor));

	return 0;
}

/*
 * Erase a grid of space
 */
static errr Term_wipe_vcs(int x, int y, int n)
{
	term_data *td = (term_data*)(Term->data);
	unsigned char *c;
	
	c = &td->base[2 * (s_width * y + x)];

	/* Blank out n characters in memory */
	for (; n; n--)
	{
		*c++ = ' ';
		*c++ = COLOR_BLANK;
	}

	return 0;
}

/*
 * Place some text on the screen using an attribute
 */
static errr Term_text_vcs(int x, int y, int n, byte a, const char *cp)
{
	term_data *td = (term_data*)(Term->data);
	unsigned char *c,col;

	col=attr_for_color[a&0xf];
	c=&td->base[2*(s_width*y+x)];
	
	/* Copy n characters to screen memory */
	for (;n;n--)
	{
		*c++=*cp++;
		*c++=col;
	}

	return 0;
}


static int active;

/*
 * Init the /dev/vcsa* system
 */
static void Term_init_vcs(term *t)
{
	/* Ignore t */
	(void) t;
	
	if (active++) return;
	setup_terminal();
}

/*
 * Nuke the /dev/vcsa* system
 */
static void Term_nuke_vcs(term *t)
{
	/* Ignore t */
	(void) t;
	
	if (--active) return;

	leave_vcs();
	close(fd_vcsa);
}

/*
 * Set up a new console terminal
 */
static void term_data_link(int x0,int y0,int x1,int y1)
{
	term_data *td = &data[num_term];
	term *t = &td->t;

	if (x0<0 || y0<0 || x0>=s_width || y0>=s_height || x1<=x0 || y1<=y0 ||
	    x1>=s_width || y1>=s_height || num_term==MAX_VCS_TERM)
	{
		fprintf(stderr,"ignoring invalid window (%i %i)-(%i %i) num %i/%i\n",
			x0,y0,x1,y1,num_term,MAX_VCS_TERM);
		return;
	}

	td->x0=x0;
	td->y0=y0;
	td->x1=x1;
	td->y1=y1;
	td->sx=x1-x0+1;
	td->sy=y1-y0+1;
	td->base=&screen[2*(x0+y0*s_width)];

	/* Initialize the term */
	term_init(t, td->sx, td->sy, num_term?0:256);

	t->soft_cursor = FALSE;
	t->icky_corner = FALSE;
	t->always_text = TRUE;
	t->never_bored = TRUE;
	t->never_frosh = TRUE;

	t->attr_blank = TERM_DARK;
	t->char_blank = ' ';

	/* Prepare the init/nuke hooks */
	t->init_hook = Term_init_vcs;
	t->nuke_hook = Term_nuke_vcs;

	/* Prepare the template hooks */
	t->xtra_hook = Term_xtra_vcs;
	t->curs_hook = Term_curs_vcs;
	t->wipe_hook = Term_wipe_vcs;
	t->text_hook = Term_text_vcs;

	/* Remember where we came from */
	t->data = (vptr)(td);

	/* Activate it */
	if (!num_term) Term_activate(t);

	/* Global pointer */
	angband_term[num_term] = t;

	num_term++;
}

/*
 * Prepare /dev/vcsa* for use by the terms package
 */
errr init_vcs(int argc,char **argv)
{
	int i;
	char *c;
	byte *cc;
	int frame=1,add_std_win=1;

	/* Find and open console (vcsa) file */
	char buf[256];
	c=ttyname(0);
	if (!c || sscanf(c,"/dev/tty%i",&i)!=1)
	{
		fprintf(stderr,"can't find my tty\n");
		return 1;
	}
	strnfmt(buf, 256, "/dev/vcsa%i",i);
	fd_vcsa=open(buf,O_RDWR);
	if (fd_vcsa==-1)
	{
		perror(buf);
		return 1;
	}
	
	/* Read screen lines and columns from console (vcsa) file */
	s_width=s_height=0;
	read(fd_vcsa,&s_height,1);
	read(fd_vcsa,&s_width,1);

	/* we allocate an extra row that's always cleared so we can use memcpy
	to clear other parts of the screen */
	screen=malloc((s_height+1)*s_width*2);
	if (!screen)
	{
		fprintf(stderr,"vcsa: out of memory\n");
		close(fd_vcsa);
		return 1;
	}

	/* clear screen and last row */
	for (cc=screen,i=0;i<s_width*(s_height+1);i++)
	{
		*cc++=' ';
		*cc++=COLOR_BLANK;
	}
	row_clear=&screen[s_height*s_width*2];

	/* Acquire the current mapping */
	tcgetattr(0, &norm_termios);
	tcgetattr(0, &game_termios);

	/*
	 * Turn off canonical mode, echo of input characters, and sending of
	 * SIGTTOU to proc.group of background process writing to
	 * controlling terminal.
	 */
	game_termios.c_lflag&=~(ICANON|ECHO|TOSTOP);

	/* Link in new terminals if required */
	if (argc>1)
	{
		int i;

		for (i=1;i<argc;i++)
		{
			if (streq(argv[i],"--noframe"))
				frame=0;
			else
			{
				int x0,y0,x1,y1;
				if (sscanf(argv[i],"%i,%i,%i,%i",&x0,&y0,&x1,&y1)!=4)
				{
					fprintf(stderr,"ignoring invalid argument %i '%s'\n",
						i,argv[i]);
					continue;
				}
				term_data_link(x0,y0,x1,y1);
				add_std_win=0;
			}
		}
	}

	if (add_std_win)
	{
		term_data_link( 0, 0,79,23);

		if (s_width==132 && s_height==60)
		{
/*

            0           80      132
          0 +-----------+-------+  0
            |           |       |
    main -> |   80x24   | 52x24 | <- equipment
            |           |       |
         24 +---------+-+-------+ 24
 object  -> |  65x13  |         |
 recall  38 +---------+  66x24  | <- inventory
            |         |         |
 monster -> |  65x21  +---------+ 49
 recall     |         |  66x10  | <- messages
         60 +---------+---------+ 60
                      65
*/

			term_data_link(  0,25, 64,37);
			term_data_link( 81, 0,131,23);
			term_data_link( 66,25,131,48);
			term_data_link(  0,39, 64,59);
			term_data_link( 66,50,131,59);
		}
	}

	if (!num_term)
	{
		fprintf(stderr,"no windows created!\n");
		return 1;
	}

	if (frame)
	{
		int y;
		for (cc=screen,i=0;i<s_width*s_height;i++)
		{
			*cc++=' ';
			*cc++=attr_for_color[1]+(attr_for_color[7]<<4);
		}
		for (i=0;i<num_term;i++)
		{
			for (cc=data[i].base,y=data[i].sy;y;y--,cc+=2*s_width)
				memcpy(cc,row_clear,data[i].sx*2);
		}
	}

	/* Success */
	return 0;
}

#endif /* USE_VCS */

