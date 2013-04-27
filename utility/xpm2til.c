/*  xpm2til.c 
 *
 *  compile like this
 *
 *  gcc -o xpm2til xpm2til.c
 *  
 *  written by Marcello Sega & Marco Vecchiocattivi (29.07.2002)
 *
 *  modified by Marco Vecchiocattivi (31.07.2002) to transform an image
 *  with arbitrary colors, reducing its palette to the standard one for
 *  utumno tile format.
 *  
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.     
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.     
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *  
 */


/*
 *	WARNING: CONVERT ONLY 1 XPM TO BASE SCENE, 1 FRAME, 1 VIEW !!
 *
 *	SEE FILE tile_format AS REFERENCE FOR UTUMNO BINARY FORMAT
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include "palette.h"
#include <string.h>
#include <math.h>
FILE *cid1, *cid2;
void
WriteHeader(int revision)
{
	fputc ('T', cid1);
 	fputc ('I', cid1);
	fputc ('L', cid1);         
	fputc (revision, cid1);
}
int 
distance(int rgb, int index, char *type )
{
	int r,g,b;
	
	r = rgb >> 16;
        g = (rgb >> 8) % 256;
	b = rgb % 256;
	r -= palette[3*index];
	g -= palette[3*index+1];
	b -= palette[3*index+2];
	
	if(!strcmp(type,"L1"))
		return (abs(r)+ abs(g)+ abs(b) );
	 if(!strcmp(type,"L2"))
		return ( r*r+g*g+b*b );
	 if(!strcmp(type,"L3"))
		return ( abs(r)*r*r+abs(g)*g*g+abs(b)*b*b);
	 if(!strcmp(type,"L4"))
		return ( r*r*r*r+g*g*g*g+b*b*b*b);
	 exit(printf("\nwrong conversion type: use L1 | L2\n"));
}
void
FileHandling(int argc,char **argv)
{
	char * name;
	 if (argc != 3)
            exit (printf ("Usage: xpm2til conversion_type input_file.xpm\n"));
         cid2 = fopen (argv[2], "r");
         if (!cid2)
            exit (printf ("File %s doesn't exist\n", argv[2]));
	 name = (char *) malloc (sizeof (char) * strlen (argv[2]) + 1);
         strcpy (name, argv[2]);
         sprintf (name + strlen (name) - 3, "til");
         cid1 = fopen (name, "w");
}

int
main (int argc, char **argv)
{
    char buf[256], scene_name[128], *ptr, color_buf[256], none[16], *buf2;
    struct my_palette_entry
    {
	char color[16];
	int palette;
    };
    typedef struct my_palette_entry palette_entry;
   
    palette_entry *my_palette;
    int rgb = 0, nchar;
    int nvw, nsc, nfr, hgt, ndt, wdt, i, l, tmp, m, k, kk;
    fpos_t position;

    FileHandling(argc,argv); 
    WriteHeader(2);
    
    nvw = 1;
    nsc = 1;
    fputc (nvw, cid1);		// number of views
    fputc (nsc, cid1);		// number of scenes
    for (i = 0; i < nsc; i++)
    {
	nfr = 1;		// number of frames
	fputc (nfr, cid1);
	printf ("scene name ? ");
	fgets (scene_name, 32, stdin);
	ptr = scene_name;
	do			// put scene name
	{
	    fputc (*ptr, cid1);
	    ptr++;
	}
	while (*(ptr - 1) != 0);
	fseek (cid1, -2, SEEK_CUR);	// remove '\n'
	fputc (0, cid1);	// terminate string

	do
	    tmp = fgetc (cid2);	// skip some stuff in xpm file
	while (tmp != '"');

	fscanf (cid2, "%d %d %d %d\",\n", &wdt, &hgt, &kk, &nchar);	// xpm size & color data
	printf ("width = %d  height = %d  colors = %d\n", wdt, hgt, kk);

/*
 * 
 *  Scan the palette entry in order to assign the neares color from the utumno palette
 *
 */

	my_palette = (palette_entry *) malloc (sizeof (palette_entry) * kk);

	sprintf (buf, "\"%%%ic\tc #%%x\",\n", nchar);

	tmp = 0;
	for (k = 0; k < kk; k++)
	{
	    int old_dist;
	    fgets (color_buf, 255, cid2);
	    if (strstr (color_buf, "None"))
	    {
		i = 0;
		while (color_buf[i] != '"')
		    i++;
		strncpy (none, &color_buf[i + 1], nchar);
		none[nchar] = '\0';
//                   k--;
	    }
	    else
	    {
		sscanf (color_buf, buf, my_palette[k].color, &rgb);
		old_dist = distance(rgb,0,argv[1]);
		my_palette[k].palette = 0;

		for (l = 1; l < 256; l++)
		{
		    int dist;
		    dist = distance(rgb,l,argv[1]);
		    if (dist < old_dist)
		    {
			old_dist = dist;
			my_palette[k].palette = l;
		    }
		}

	    }
	}

/*
 *
 *  Convert the xpm file into utumno tile format using previous palette
 *
 */ 	
	for (l = 0; l < nfr * nvw; l++)
	{
	    fputc (hgt, cid1);	// height
	    fgetpos (cid1, &position);
	    fputc (0, cid1);	// number of data (1) - will be changed later
	    fputc (0, cid1);	// (2)
	    ndt = 0;

	    buf2 = (char *) malloc (sizeof (char) * nchar * wdt + 10);

	    for (; hgt > 0; hgt--)
	    {

		do;
		while (fgetc (cid2) != '"');

		fgets (buf2, wdt * nchar + 2, cid2);	// get data plus "

		if (buf2[wdt * nchar] != '"')
		    exit (printf ("error: wrong xpm format\n"));
		m = 0;
		while (m < wdt)
		{
		    if (!strncmp (&buf2[m * nchar], none, nchar))	// a transparent pixel
		    {
			tmp = 0;
			fputc (1, cid1);
			ndt++;
			while (m < wdt
			       && !strncmp (&buf2[m * nchar], none, nchar))
			{
			    m++;
			    tmp++;	//count transparent pixels
			}
			fputc (tmp, cid1);
			ndt++;
		    }
		    if (m == wdt)
		    {
			fputc (2, cid1);	// end of line
			ndt++;
			break;
		    }
		    if (strncmp (&buf2[m * nchar], none, nchar))	// colored pixel
		    {
			tmp = 0;
			while (m < wdt
			       && strncmp (&buf2[m * nchar], none, nchar))
			{
			    m++;
			    tmp++;
			}	//count pixels
			fputc (0, cid1);
			fputc (tmp, cid1);
			ndt += 2;
			for (; tmp > 0; tmp--)
			{
			    for (i = 0; i < kk; i++)
			    {
				if (!strncmp
				    (&buf2[(m - tmp) * nchar],
				     my_palette[i].color, nchar))
				    break;
			    }
			    fputc (my_palette[i].palette, cid1);
			    ndt++;
			}
		    }
		    if (m > wdt)
			exit (printf ("!! %d\n", m));

		    if (m == wdt)
		    {
			fputc (2, cid1);	// end of line
			ndt++;
			break;
		    }
		}
		if (hgt == 1)
		{
		    fputc (3, cid1);	//end of frame
		    ndt++;
		    fsetpos (cid1, &position);	//return to ndata position
		    printf ("%d\n", ndt);
		    fputc (ndt % 256, cid1);
		    fputc (ndt / 256, cid1);
		}
	    }
	}
    }
    fclose (cid1);
    fclose (cid2);
    return 0;
}
